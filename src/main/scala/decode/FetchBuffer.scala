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
import firrtl.PrimOps.Pad
import freechips.rocketchip.config.Parameters
import matrix.common._
import matrix.utils._
import matrix.ifu._

class InstBufferResp(implicit p: Parameters) extends PreDecodeResp {
  val pred_info   = new PredictorResp
  val cause       = UInt(EXLEN.W)
}

class InstBufferIO(implicit p: Parameters) extends MatrixBundle {
  val kill = Input(Bool())
  val stall = Input(Bool())
  val req = Flipped(Valid(Vec(decodeWidth*2, new PreDecodeResp)))
  val pred_info = Input(new PredictorResp)
  val cause = Input(UInt(EXLEN.W))
  val resp = Flipped(Valid(Vec(decodeWidth, new InstBufferResp)))
  val full = Output(Bool())
}

class InstBuffer(implicit p: Parameters) extends MatrixModule
  with ScalarOpConstants {
  val io = IO(new InstBufferIO)
  
  def bufferIdWidth = log2Ceil(numOfFBEntries) + 1
  
  val buffer = Reg(Vec(numOfFBEntries, new InstBufferResp))
  val head   = RegInit(UInt(bufferIdWidth.W), 0.U)
  val tail   = RegInit(UInt(bufferIdWidth.W), 0.U)
  val kill = WireInit(io.kill)
  val stall = WireInit(io.stall)
  val full = WireInit(io.full)
  //  Write
  val total_insts = PopCount(Cat(io.req.bits.map(_.valid)))
  val write_slots = for (w <- 0 until decodeWidth*2) yield {tail + w.U}
  val valid = io.req.valid
  val new_entries = Wire(Vec(decodeWidth*2, new InstBufferResp))
  for (w <- 0 until decodeWidth*2) {
    new_entries(w).valid     := io.req.bits(w).valid
    new_entries(w).inst      := io.req.bits(w).inst
    new_entries(w).len       := io.req.bits(w).len
    new_entries(w).pc        := io.req.bits(w).pc
    new_entries(w).order     := io.req.bits(w).order
    new_entries(w).pred_info  := io.pred_info
    new_entries(w).cause     := io.cause
    when (valid && !kill && !full) {
      buffer(hashIndex(write_slots(w))) := new_entries(w)
    }
  }
  val store_insts = tail-head
  io.full := total_insts > (numOfFBEntries.U - store_insts)
  //  Read
  val read_slots = for (w <- 0 until decodeWidth) yield { head + w.U }
  val read_entries = read_slots map { e => buffer(hashIndex(e)) }
  val order_vec = Reverse(Cat(read_entries.map(_.order)))
  val resp = Wire(Vec(decodeWidth, new InstBufferResp))
  val resp_valid = store_insts > 0.U && !stall
  resp(0) := read_entries(0)
  for (w <- 1 until decodeWidth) {
    resp(w).valid   := read_entries(w).valid && !order_vec(w-1, 0).orR
    resp(w).inst    := read_entries(w).inst
    resp(w).len     := read_entries(w).len
    resp(w).pc      := read_entries(w).pc
    resp(w).order   := read_entries(w).order
    resp(w).pred_info:= read_entries(w).pred_info
    resp(w).cause   := read_entries(w).cause
  }
  io.resp.valid := RegEnable(resp_valid && !kill, !stall | kill)
  io.resp.bits  := RegEnable(resp, !stall)
  //  Update
  when (kill) {
    head := 0.U
  } .elsewhen (stall) {
    head := head
  } .elsewhen (store_insts > 0.U) {
    head := head + PopCount(Cat(resp.map(_.valid)))
  }
  when (kill) {
    tail := 0.U
  } .elsewhen (valid) {
    tail := tail + total_insts
  } .otherwise {
    tail := tail
  }
}