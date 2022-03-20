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
import matrix.ifu._
import matrix.common._
import matrix.utils._

class PreDecodeReq(implicit p: Parameters) extends MatrixBundle {
  val insts       = UInt(icacheParams.blockBits.W)       
  val pc          = UInt(vaddrWidth.W)                 
  val cause       = UInt(EXLEN.W)                        
  val pred_info   = new PredictorResp                  
}

class PreDecodeResp(implicit p: Parameters) extends MatrixBundle {
  val valid       = Bool()                        
  val inst        = UInt(ILEN.W)                       
  val len         = Bool()                            
  val pc          = UInt(vaddrWidth.W)                  
  val order       = Bool()                              
}

class PreDecodeIO(implicit p: Parameters) extends MatrixBundle {
  val kill = Input(Bool())
  val bpu_kill = Input(Bool())
  val stall = Input(Bool())
  val req = Flipped(Valid(new PreDecodeReq))
  val resp = Valid(Vec(decodeWidth*2, new PreDecodeResp))
  val pred_info = Output(new PredictorResp)
  val cause = Output(UInt(EXLEN.W))
}


class PreDecode(implicit p: Parameters) extends MatrixModule {
  val io = IO(new PreDecodeIO)
  
  val stall = WireInit(io.stall)
  val inst_buffer = Reg(UInt(16.W))
  val inst_buffer_val = RegInit(Bool(), false.B)
  val cross_boundary = WireInit(false.B)
  when (io.kill || io.bpu_kill) {
    inst_buffer_val := false.B
  } .elsewhen (!stall && io.req.valid) {
    inst_buffer_val := cross_boundary
    inst_buffer := io.req.bits.insts(icacheParams.blockBits-1, icacheParams.blockBits-16)
  }
  def windowSz = 2*decodeWidth
  def check(inst: UInt, cause: UInt) = {
    val opcode = inst(6,0)
    val isFence = opcode === BitPat("b1110011")
    val isCsr = opcode === BitPat("b0001111")
    !cause.orR & (isCsr | isFence)
  }
  def is2Bytes(opcode: UInt) = opcode(1,0) =/= 3.U
  val base = Cat(io.req.bits.pc(3,0), 0.U(4.W))-2.U
  val mask = Cat(Mux(inst_buffer_val, Fill(8,true.B), ~(UIntToOH(io.req.bits.pc(3,1))-1.U)), inst_buffer_val)
  val block = Cat(io.req.bits.insts, inst_buffer & Fill(16, inst_buffer_val))
  val word_array = VecInit(Seq.tabulate(windowSz + 1) { b => block(16*(b+1)-1,b*16) })
  val valid_vec = Wire(Vec(windowSz + 1, Bool()))
  valid_vec(0) := mask(0)
  for (w <- 1 until windowSz + 1) {
    valid_vec(w) := mask(w) && (valid_vec(w-1) && is2Bytes(word_array(w-1)) || !valid_vec(w-1))
  }
  val sels = selectFirstN(Reverse(Cat(valid_vec)), windowSz)
  val insts = Wire(Vec(windowSz, new PreDecodeResp))
  for (w <- 0 until windowSz) {
    val selOH = sels(w)
    val valid = selOH.orR
    val sel = OHToUInt(selOH)
    insts(w).valid := valid
    insts(w).len   := is2Bytes(word_array(sel))
    insts(w).pc    := base + sel << 1.U
    insts(w).inst  := Mux(insts(w).len, Cat(0.U, word_array(sel)), Cat(word_array(sel+1.U), word_array(sel)))
    insts(w).order := check(insts(w).inst, io.req.bits.cause)
  }
  cross_boundary := insts(windowSz-1).valid && !insts(windowSz-1).len && insts(windowSz-1).pc(3,0) === 14.U
  val valid = io.req.valid && (!stall && !io.kill)
  val resp = Reg(Vec(windowSz, new PreDecodeResp))
  when (io.kill) {
    resp.foreach(_ := 0.U.asTypeOf(new PreDecodeResp))
  } .elsewhen (valid) {
    resp := insts
  }
  io.resp.valid := RegEnable(io.req.valid, valid)
  io.resp.bits := resp
  io.cause := RegEnable(io.req.bits.cause, valid)
  io.pred_info := RegEnable(io.req.bits.pred_info, valid)
}
  
