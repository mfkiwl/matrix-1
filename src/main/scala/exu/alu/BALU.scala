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
package matrix.exu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import matrix.ifu._

class BALU(implicit p: Parameters) extends BaseExecuteUnit (
  hasBAlu = true
) {
  val cur_uop = meta.uop

  def isEQ  = cur_uop === UOP_BEQ
  def isNEQ = cur_uop === UOP_BNE
  def isLT  = cur_uop === UOP_BLT
  def isGE  = cur_uop === UOP_BGE
  def isJAL = meta.is_jmp

  def isSUB = rs2(XLEN-1)
  val rs2_inv = Mux(isSUB, !rs2, rs2)
  val tg_addr = rs1 + rs2_inv + isSUB

  val rs1_extend = Cat(!meta.usign && rs1(XLEN-1), rs1).asSInt
  val rs2_extend = Cat(!meta.usign && rs2(XLEN-1), rs2).asSInt
  val taken = Mux(isEQ, rs1_extend === rs2_extend,
    Mux(isNEQ, rs1_extend =/= rs2_extend,
      Mux(isLT, rs1_extend < rs2_extend, rs1_extend >= rs2_extend)))
  val pred = Mux(meta.is_jmp, true.B,
    !meta.is_jmp && Mux(io.pred_info.use_loop, io.pred_info.loop_taken, io.pred_info.tage_taken))
  val direct_wrong = taken =/= pred
  val addr_wrong = tg_addr =/= io.pred_info.tg_addr && io.pred_info.btb.valid

  //  TODO: Implement kill signal.

  //
  val need_update_btb = Wire(Bool())
  val need_update_ltage = Wire(Bool())
  val update_predictor = Wire(new PredictorUpdate)
  update_predictor.pc                      := io.pc
  update_predictor.tg_addr                 := tg_addr
  update_predictor.btb.valid               := need_update_btb
  update_predictor.btb.alloc               := !io.pred_info.btb.valid
  update_predictor.btb.meta.offset         := io.pc(3, 0)
  update_predictor.btb.meta.len            := meta.len
  update_predictor.btb.meta.jmp            := meta.is_jmp
  update_predictor.btb.meta.ret            := meta.is_ret
  update_predictor.btb.meta.call           := meta.is_call
  update_predictor.btb.meta.ret_then_call  := meta.is_ret_then_call
  update_predictor.btb.meta.condi          := !isJAL
  update_predictor.btb.idx                 := io.pred_info.btb.idx
  update_predictor.taken                   := taken

  need_update_btb := (!io.pred_info.btb.valid | (
    io.pc(3, 0) =/= io.pred_info.btb.meta.offset |
      meta.len =/= io.pred_info.btb.meta.len |
      meta.is_jmp =/= io.pred_info.btb.meta.jmp |
      meta.is_ret =/= io.pred_info.btb.meta.ret |
      meta.is_call =/= io.pred_info.btb.meta.call |
      meta.is_ret_then_call =/= io.pred_info.btb.meta.ret_then_call |
      (isJAL && io.pred_info.btb.meta.condi) |
      (!isJAL && (tg_addr =/= io.pred_info.tg_addr))
    )) && io.req.valid
  need_update_ltage := io.req.valid
  io.update := RegNext(update_predictor)

  when (flush | kill) {
    resp.valid := false.B
  } .otherwise {
    resp.valid  := io.req.valid
    resp.rob_id := io.req.meta.rob_id
    resp.data   := io.pc + Mux(meta.len, 4.U, 2.U)
    resp.cause  := 0.U
    rd_val      := io.req.meta.rd_val
    rd_type     := io.req.meta.rd_type
    rd          := io.req.meta.rd
  }

  io.resp                 := resp
  io.bypass.valid         := resp.valid && rd_val
  io.bypass.bits.rd_type  := rd_type
  io.bypass.bits.rd       := rd
  io.bypass.bits.data     := resp.data
}