/*
 * Copyright (c) 2022 Lyn
 * Skew is licensed under Mulan PubL v2.
 * You can use this software according to the terms and conditions of the Mulan PubL v2.
 * You may obtain a copy of Mulan PubL v2 at:
 *         http://license.coscl.org.cn/MulanPubL-2.0
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PubL v2 for more details.
 */
package matrix.ifu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import matrix.common._
import matrix.vmm._

class KillReq(implicit p: Parameters) extends MatrixBundle {
  val valid = Bool()
  val rob_id = UInt(robIdWidth.W)
}

class KillWithAddrReq(implicit p: Parameters) extends KillReq {
  val addr = UInt(vaddrWidth.W)
}

class FetchUnitResp(implicit p: Parameters) extends MatrixBundle {
  val valid     = Bool()
  val pred_info = new PredictorResp
  val data      = UInt(icacheParams.blockBits.W)
  val pc        = UInt(vaddrWidth.W)
  val cause     = UInt(EXLEN.W)
}

class FetchUnitIO(implicit p: Parameters) extends MatrixBundle {
  val flush = Input(new KillWithAddrReq)
  val kill  = Input(new KillWithAddrReq)
  val stall = Input(Bool())
  val sfence = Flipped(Decoupled(new SFenceReq))
  val resp = Output(new FetchUnitResp)
  val update = Flipped(Valid(new PredictorUpdate))
  val retire = Flipped(Valid(Vec(retireWidth, new PredictorRetire)))
  val itlb_ptw = new TLBPTWIO
  val icache_ptw = new CachePTWIO(icacheParams.dCache, icacheParams.lineBits)
}


class FetchUnit(implicit p: Parameters) extends MatrixModule with MemoryOpConstants {
  val io = IO(new FetchUnitIO)
  //============================== Modules =============================//
  val bpu = Module(new Predictor)
  val itlb = Module(new ITLB(itlbParams))
  val icache = Module(new ICache(icacheParams))

  val s0_val = Wire(Bool())
  val s1_val = RegInit(Bool(), false.B)
  val s1_pc = Reg(UInt(vaddrWidth.W))
  val s2_val = RegInit(Bool(), false.B)
  val s2_pc = Reg(UInt(vaddrWidth.W))
  val stall = WireInit(false.B)

  //=========================== Prediction ===============================//
  val btb_taken = bpu.io.resp.btb.meta.jmp |
    bpu.io.resp.btb.meta.call |
    bpu.io.resp.btb.meta.ret |
    bpu.io.resp.btb.meta.ret_then_call
  val ltage_taken = bpu.io.resp.btb.meta.condi & Mux(bpu.io.resp.use_loop,
    bpu.io.resp.loop_taken, bpu.io.resp.tage_taken)
  val bpu_taken = btb_taken | ltage_taken
  val bpu_kill = s2_val && bpu_taken
  val kill = io.flush.valid | bpu_kill | io.kill.valid

  bpu.io.kill := kill
  bpu.io.update := io.update
  bpu.io.retire := io.retire
  bpu.io.stall  := stall
  itlb.io.ptw <> io.itlb_ptw
  itlb.io.kill := kill
  itlb.io.sfence <> io.sfence
  icache.io.ptw <> io.icache_ptw
  icache.io.kill := io.kill
  icache.io.stall := stall
  //============================ Stage 0 ===============================//
  val icache_hit = icache.io.resp.hit
  val itlb_hit = itlb.io.resp.hit

  s0_val := !stall
  val pc = RegInit(UInt(vaddrWidth.W), bootAddress.U)
  when(s0_val) {
    pc := Mux(io.flush.valid, io.flush.addr,
      Mux(io.kill.valid, io.kill.addr,
        Mux(bpu_kill, bpu.io.resp.tg_addr,
          pc + 16.U)))
  }
  //
  val s_ready :: s_itlb_miss :: s_itlb_wait :: s_icache_miss :: s_icache_wait :: s_icache_write :: Nil = Enum(6)
  val state = RegInit(s_ready)
  val next_state = WireInit(state)

  when(!io.stall) {
    switch(state) {
      is(s_ready) {
        when(s1_val && !itlb_hit) {
          next_state := s_itlb_miss
        }.otherwise {
          next_state := s_ready
        }
      }
      is(s_itlb_miss) {
        when(io.itlb_ptw.req.ready) {
          next_state := s_itlb_wait
        }.otherwise {
          next_state := s_itlb_miss
        }
      }
      is(s_itlb_wait) {
        when(io.itlb_ptw.resp.valid) {
          when(!icache_hit) {
            next_state := s_icache_miss
          }.otherwise {
            next_state := s_ready
          }
        }.otherwise {
          next_state := s_itlb_wait
        }
      }
      is(s_icache_miss) {
        when(io.icache_ptw.req.ready) {
          next_state := s_icache_wait
        }.otherwise {
          next_state := s_icache_miss
        }
      }
      is(s_icache_wait) {
        when(io.icache_ptw.resp.valid) {
          next_state := s_icache_write
        }.otherwise {
          next_state := s_icache_wait
        }
      }
      is(s_icache_write) {
        next_state := s_ready
      }
    }
  }

  when(kill) {
    state := s_ready
  }.otherwise {
    state := next_state
  }
  stall := io.stall | next_state =/= s_ready | bpu.io.empty
  //=========================== Stage 1 ===============================//
  bpu.io.req.valid := s0_val
  bpu.io.req.bits.pc := pc
  itlb.io.req.valid := s0_val
  itlb.io.req.bits.pc := pc
  icache.io.req.valid := s0_val
  icache.io.req.bits.pc := pc
  icache.io.req.bits.ppn := Mux(state === s_itlb_wait, io.itlb_ptw.resp.bits.pte.ppn, itlb.io.resp.ppn)
  icache.io.req.bits.cmd := M_XRD
  icache.io.invalidate := io.sfence.valid


  when(kill) {
    s1_val := false.B
    s2_val := false.B
  }.elsewhen(!stall) {
    s1_val := s0_val
    s1_pc := pc
  }
  //=========================== Stage 2 ===============================//
  val data_slices = VecInit(Seq.tabulate(icacheParams.numOfBanks) { i =>
    io.icache_ptw.resp.bits.data(icacheParams.blockBits * (i + 1) - 1, icacheParams.blockBits * i)
  })
  val use_resp = RegInit(Bool(), false.B)
  val resp_cause = Reg(UInt(EXLEN.W))
  val resp_data = Reg(UInt(icacheParams.blockBits.W))
  when(kill) {
    s2_val := false.B
  }.elsewhen(io.stall) {
    s2_val := s2_val
    s2_pc := s2_pc
    use_resp := use_resp
    resp_cause := resp_cause
    resp_data := resp_data
  }.elsewhen(!stall) {
    when(io.icache_ptw.resp.valid) {
      s2_val := s2_val
      s2_pc := s2_pc
      use_resp := true.B
      resp_cause := io.icache_ptw.resp.bits.cause
      resp_data := data_slices(s2_pc(5, 4))
    }.otherwise {
      s2_val := s1_val
      s2_pc := s1_pc
      resp_cause := Mux(use_resp, resp_cause, 0.U)
      resp_data := Mux(use_resp, resp_data, icache.io.resp.data)
      use_resp := false.B
    }
  }

  io.resp.valid       := s2_val
  io.resp.pred_info   := bpu.io.resp
  io.resp.cause       := resp_cause
  io.resp.data        := resp_data
  io.resp.pc          := s2_pc
  io.icache_ptw.req.valid       := state === s_icache_miss
  io.icache_ptw.req.bits.addr   := s1_pc
  io.icache_ptw.req.bits.cmd    := M_XRD
} 