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
import matrix.utils._

case class BPDParams (
                       btb: BTBParams = BTBParams(),
                       ras: RASParams = RASParams(),
                       loop: LoopParams = LoopParams(),
                       tage: TAGEParams = TAGEParams()
                     ) {
  def numOfSets: Int = 64 // For predinfo cache
  def bufferIdWidth: Int = log2Ceil(numOfSets)
}

class PredictorReq(implicit p: Parameters) extends MatrixBundle {
  val pc    = UInt(vaddrWidth.W)
}

class PredictorBTBResp(implicit p: Parameters) extends MatrixBundle {
  val valid = Bool()
  val idx   = UInt(bpdParams.btb.btbIdWidth.W)
  val meta  = new BTBMeta
}

class PredictorResp(implicit p: Parameters) extends MatrixBundle {
  val tg_addr       = UInt(vaddrWidth.W)
  val buffer_idx    = UInt(bpdParams.bufferIdWidth.W)
  val btb           = new PredictorBTBResp
  val use_loop      = Bool()
  val loop_taken    = Bool()
  val tage_taken    = Bool()
}

class PredictorBTBUpdate(implicit p: Parameters) extends MatrixBundle {
  val valid   = Bool()
  val alloc   = Bool()
  val idx     = UInt(bpdParams.btb.btbIdWidth.W)
  val meta    = new BTBMeta
}

class PredictorUpdate(implicit p: Parameters) extends MatrixBundle {
  val pc            = UInt(vaddrWidth.W)
  val tg_addr       = UInt(vaddrWidth.W)
  val taken         = Bool()
  val buffer_idx    = UInt(bpdParams.bufferIdWidth.W)
  val btb           = new PredictorBTBUpdate
}

class PredictorRetire(implicit p: Parameters) extends MatrixBundle {
  val valid      = Bool()
  val buffer_idx = UInt(bpdParams.bufferIdWidth.W)
}

class PredictorIO(implicit p: Parameters) extends MatrixBundle {
  val kill = Input(Bool())
  val stall = Input(Bool())
  val req = Flipped(Valid(new PredictorReq))
  val resp = Output(new PredictorResp)
  val update = Flipped(Valid(new PredictorUpdate))
  val retire = Flipped(Valid(Vec(retireWidth, new PredictorRetire)))
  val empty = Output(Bool())
}

class Predictor(implicit p: Parameters) extends MatrixModule {
  val io = IO(new PredictorIO)

  val btb = Module(new BTB)
  val ras = Module(new RAS)
  val ltage = Module(new LTage)

  //  Stage 1
  //  Allocator
  val buffer_freelist = RegInit(UInt(bpdParams.numOfSets.W), ~0.U(bpdParams.numOfSets.W))
  val predinfo_buffer = Reg(Vec(bpdParams.numOfSets, new LTageResp))
  val buffer_selected_oh = selectFirstN(buffer_freelist)
  val buffer_selected = OHToUInt(buffer_selected_oh)
  val buffer_empty = !buffer_freelist.orR
  val s1_buffer_idx = RegEnable(buffer_selected, io.req.valid)

  val buffer_free = io.retire.bits.map { i =>
    Fill(bpdParams.numOfSets, io.retire.valid && i.valid) & UIntToOH(i.buffer_idx)
  }

  when (io.kill) {
    buffer_freelist := 0.U
  } .otherwise {
    buffer_freelist := buffer_freelist & ~buffer_selected_oh | Reverse(Cat(buffer_free))
  }
  io.empty := buffer_empty

  //
  val s0_valid = io.req.valid && !io.stall
  btb.io.req.valid := s0_valid
  btb.io.req.bits.pc := io.req.bits.pc
  ltage.io.req.valid := s0_valid
  ltage.io.req.bits.pc := io.req.bits.pc
  val s1_val = RegEnable(io.req.valid, s0_valid)
  val s1_pc = RegEnable(io.req.bits.pc, s0_valid)

  //  Stage 2
  val next_pc = s1_pc + btb.io.resp.meta.offset + Mux(btb.io.resp.meta.len, 2.U, 4.U)
  ras.io.req        := s1_val
  ras.io.push       := btb.io.resp.meta.call | btb.io.resp.meta.ret_then_call
  ras.io.push_addr  := next_pc
  ras.io.pop        := btb.io.resp.meta.ret | btb.io.resp.meta.ret_then_call

  val resp = Wire(new PredictorResp)
  val btb_val = btb.io.resp.valid && (btb.io.resp.meta.jmp | btb.io.resp.meta.condi)
  resp.tg_addr    := Mux(btb_val, btb.io.resp.tg_addr, ras.io.pop_addr)
  resp.buffer_idx := buffer_selected
  resp.btb.valid  := btb.io.resp.valid
  resp.btb.idx    := btb.io.resp.btb_idx
  resp.btb.meta   := btb.io.resp.meta
  resp.use_loop   := ltage.io.resp.loop.use_loop
  resp.loop_taken := ltage.io.resp.loop.loop_taken
  resp.tage_taken := ltage.io.resp.tage.prime_taken
  io.resp  := RegEnable(resp, s1_val)
  //
  val predinfo = Wire(new LTageResp)
  predinfo.loop.use_loop    := ltage.io.resp.loop.use_loop
  predinfo.loop.loop_taken  := ltage.io.resp.loop.loop_taken
  predinfo.tage.prime_taken := ltage.io.resp.tage.prime_taken
  predinfo.tage.alt_taken   := ltage.io.resp.tage.alt_taken
  predinfo.tage.prime_bank  := ltage.io.resp.tage.prime_bank
  predinfo.tage.alt_bank    := ltage.io.resp.tage.alt_bank
  predinfo.tage.bim_cnt     := ltage.io.resp.tage.bim_cnt
  predinfo.tage.meta        := ltage.io.resp.tage.meta
  predinfo.tage.tags        := ltage.io.resp.tage.tags
  predinfo.tage.banks       := ltage.io.resp.tage.banks

  when (s1_val && !io.empty && !io.stall) {
    predinfo_buffer(s1_buffer_idx) := predinfo
  }
  //  Update
  //  Update BTB
  btb.io.update.valid         := io.update.bits.btb.valid
  btb.io.update.bits.pc       := io.update.bits.pc
  btb.io.update.bits.alloc    := io.update.bits.btb.alloc
  btb.io.update.bits.tg_addr  := io.update.bits.tg_addr
  btb.io.update.bits.btb_idx  := io.update.bits.btb.idx
  btb.io.update.bits.meta     := io.update.bits.btb.meta

  //  Update LTage
  val predinfo_slot = predinfo_buffer(io.update.bits.buffer_idx)
  ltage.io.update.valid             := io.update.valid
  ltage.io.update.bits.taken        := io.update.bits.taken
  ltage.io.update.bits.use_loop     := predinfo_slot.loop.use_loop
  ltage.io.update.bits.loop_taken   := predinfo_slot.loop.loop_taken
  ltage.io.update.bits.prime_taken  := predinfo_slot.tage.prime_taken
  ltage.io.update.bits.alt_taken    := predinfo_slot.tage.alt_taken
  ltage.io.update.bits.prime_bank   := predinfo_slot.tage.prime_bank
  ltage.io.update.bits.alt_bank     := predinfo_slot.tage.alt_bank
  ltage.io.update.bits.bim_cnt      := predinfo_slot.tage.bim_cnt
  ltage.io.update.bits.meta         := predinfo_slot.tage.meta
  ltage.io.update.bits.tags         := predinfo_slot.tage.tags
  ltage.io.update.bits.banks        := predinfo_slot.tage.banks
  ltage.io.update.bits.pc           := io.update.bits.pc
}