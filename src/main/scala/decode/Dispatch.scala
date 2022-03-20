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
package matrix.decode

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import matrix.common._
import matrix.commit._
import matrix.ifu._

class DispatchResp(implicit p: Parameters) extends MatrixBundle
 with ScalarOpConstants {
  val valid       = Bool()
  val rob_id      = UInt(robIdWidth.W)
  val micro_op    = new MicroOp
  val csr_addr    = UInt(CSR_ADDR_SZ.W)
  val imm_sel     = UInt(IS_SZ.W)
  val short_imm   = UInt(LONGEST_IMM_SZ.W)
  val pc          = UInt(vaddrWidth.W)
  val pred_info   = new PredictorResp
  val cause       = UInt(EXLEN.W)
}

class DispatchIO(implicit p: Parameters) extends MatrixBundle {
  val kill = Input(Bool())
  val stall = Input(Bool())
  val req = Flipped(Valid(Vec(decodeWidth, new DecodeResp)))
  val resp = Valid(Vec(decodeWidth, new DispatchResp))
  val disp = Valid(Vec(decodeWidth, new RobDispatchReq))
  val release = Input(Bool())
  val order = Output(Bool())
  val empty = Output(Bool())
  val roll_back = Flipped(Valid(new AllocatorResp))
  val retire = Flipped(Valid(Vec(retireWidth, new AllocatorRetire)))
  val st_write_done = Input(Bool())
  val rob_head = Output(UInt(robIdWidth.W))
  val rob_tail = Output(UInt(robIdWidth.W))
  val ld_head = Output(UInt(ldqIdWidth.W))
  val ld_tail = Output(UInt(ldqIdWidth.W))
  val st_head = Output(UInt(stqIdWidth.W))
  val st_tail = Output(UInt(stqIdWidth.W))
  val st_ret = Output(UInt(stqIdWidth.W))
}

class Dispatch(implicit p: Parameters) extends MatrixModule 
 with ScalarOpConstants {
  val io = IO(new DispatchIO)

  val stall = io.stall | io.empty
  val kill = WireInit(io.kill)
  
  val s_ready::s_stall::s_redisp::Nil = Enum(3)
  val state = RegInit(s_ready)
  def is_ready = state === s_ready
  def is_redisp = state === s_redisp
  val need_order = io.req.bits(0).valid && io.req.bits(0).order
  //  Control
  switch (state) {
    is (s_ready) {
      when (io.req.valid && need_order && !stall) {
        state := s_stall
      }
    }
    is (s_stall) {
      when (io.release) {
        state := s_redisp
      }
    }
    is (s_redisp) {
      when (!stall) {
        state := s_ready
      }
    }
  }
  when (kill) {
    state := s_ready
  }
  io.order := need_order
  val allocator = Module(new Allocator)
  allocator.io.stall := io.stall
  allocator.io.kill := kill
  allocator.io.req.valid := io.req.valid && (is_redisp | is_ready)
  for (w <- 0 until decodeWidth) {
    allocator.io.req.bits(w).valid := io.req.bits(w).valid
    allocator.io.req.bits(w).is_load := io.req.bits(w).micro_op.uop === UOP_LD
    allocator.io.req.bits(w).is_store := io.req.bits(w).micro_op.uop === UOP_STORE
  }
  allocator.io.roll_back := io.roll_back
  allocator.io.retire := io.retire
  allocator.io.st_write_done := io.st_write_done
  io.empty := allocator.io.empty
  //  Dispatch
  val disp = Reg(Vec(decodeWidth, new RobDispatchReq))
  for (w <- 0 until decodeWidth) {
    when (kill) {
      disp(w).valid := false.B
    } .elsewhen (io.req.valid && (is_ready | is_redisp) && !stall) {
      disp(w).valid         := io.req.bits(w).valid
      disp(w).cause         := io.req.bits(w).cause
      disp(w).pc            := io.req.bits(w).pc
      disp(w).rob_id        := allocator.io.resp(w).rob_id
      disp(w).meta.uop      := io.req.bits(w).micro_op.uop
      disp(w).meta.is_load  := io.req.bits(w).micro_op.uop === UOP_LD
      disp(w).meta.ld_id    := allocator.io.resp(w).ld_id
      disp(w).meta.is_store := io.req.bits(w).micro_op.uop === UOP_STORE
      disp(w).meta.st_id    := allocator.io.resp(w).st_id
      disp(w).meta.rs1_val  := io.req.bits(w).micro_op.rs1_val
      disp(w).meta.rs2_val  := io.req.bits(w).micro_op.rs2_val
      disp(w).meta.rs3_val  := io.req.bits(w).micro_op.rs3_val
      disp(w).meta.rd_val   := io.req.bits(w).micro_op.rd_val
      disp(w).meta.rd_type  := io.req.bits(w).micro_op.rd_type
      disp(w).meta.rd       := io.req.bits(w).micro_op.rd
      disp(w).meta.order    := io.req.bits(w).order
      when (is_redisp) {
        disp(w).meta.order := false.B
      } .otherwise {
        disp(w).meta.order := io.req.bits(w).order
      }
    }
  }
  io.disp.valid := RegEnable(((is_redisp | is_ready) && io.req.valid) && !kill, !stall | kill)
  io.disp.bits := disp
  //  Response
  val resp = Reg(Vec(decodeWidth, new DispatchResp))
  for (w <- 0 until decodeWidth) {
    when (kill) {
      resp(w).valid := false.B
    } .elsewhen (io.req.valid && ((is_ready && !need_order) | is_redisp) && !stall) {
      resp(w).valid       := io.req.bits(w).valid
      resp(w).micro_op    := io.req.bits(w).micro_op
      resp(w).pc          := io.req.bits(w).pc
      resp(w).pred_info   := io.req.bits(w).pred_info
      resp(w).cause       := io.req.bits(w).cause
      resp(w).rob_id      := allocator.io.resp(w).rob_id
    }
  }
  io.resp.valid := RegEnable(((is_ready && !need_order) | is_redisp) && io.req.valid && !kill, !stall | kill)
  io.resp.bits := resp
  //
  io.rob_head := allocator.io.rob_head
  io.rob_tail := allocator.io.rob_tail
  io.ld_head  := allocator.io.ld_head
  io.ld_tail  := allocator.io.ld_tail
  io.st_head  := allocator.io.st_head
  io.st_tail  := allocator.io.st_tail
  io.st_ret   := allocator.io.st_ret
}