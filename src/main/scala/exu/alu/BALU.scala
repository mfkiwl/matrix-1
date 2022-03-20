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
  def isJAL = cur_uop === UOP_JAL

  def isSUB = rs2(XLEN-1)
  val rs2_inv = Mux(isSUB, !rs2, rs2)
  val tg_addr = rs1 + rs2_inv + isSUB

  val rs1_extend = Cat(!meta.usign && rs1(XLEN-1), rs1).asSInt
  val rs2_extend = Cat(!meta.usign && rs2(XLEN-1), rs2).asSInt
  val taken = Mux(isEQ, rs1_extend === rs2_extend,
    Mux(isNEQ, rs1_extend =/= rs2_extend,
      Mux(isLT, rs1_extend < rs2_extend, rs1_extend >= rs2_extend)))
  val pred = Mux(io.pred_info.ltage.loop.use_loop, io.pred_info.ltage.loop.taken,
    io.pred_info.ltage.tage.prime_taken)
  val direct_wrong = taken =/= pred
  val addr_wrong = tg_addr =/= io.pred_info.tg_addr && io.pred_info.btb.valid

  //
  val update_predictor = Wire(new PredictorUpdate)
  update_predictor.pc := io.pc
  update_predictor.tg_addr := tg_addr
  val need_update_btb = Wire(Bool())
  val need_update_ltage = Wire(Bool())

  update_predictor.btb_update.valid               := need_update_btb
  update_predictor.btb_update.alloc               := !io.pred_info.btb.valid
  update_predictor.btb_update.meta.offset         := io.pc(3, 0)
  update_predictor.btb_update.meta.len            := meta.len
  update_predictor.btb_update.meta.jmp            := meta.is_jmp
  update_predictor.btb_update.meta.ret            := meta.is_ret
  update_predictor.btb_update.meta.call           := meta.is_call
  update_predictor.btb_update.meta.ret_then_call  := meta.is_ret_then_call
  update_predictor.btb_update.meta.condi          := !isJAL
  update_predictor.ltage_update.valid             := need_update_ltage
  update_predictor.ltage_update.use_loop          := io.pred_info.ltage.loop.use_loop
  update_predictor.ltage_update.loop_taken        := io.pred_info.ltage.loop.taken
  update_predictor.ltage_update.taken             := taken
  update_predictor.ltage_update.prime_taken       := io.pred_info.ltage.tage.prime_taken
  update_predictor.ltage_update.alt_taken         := io.pred_info.ltage.tage.alt_taken
  update_predictor.ltage_update.prime_bank        := io.pred_info.ltage.tage.prime_bank
  update_predictor.ltage_update.alt_bank          := io.pred_info.ltage.tage.alt_bank
  update_predictor.ltage_update.bim_cnt           := io.pred_info.ltage.tage.bim_cnt
  update_predictor.ltage_update.meta              := io.pred_info.ltage.tage.meta
  update_predictor.ltage_update.tags              := io.pred_info.ltage.tage.tags
  update_predictor.ltage_update.banks             := io.pred_info.ltage.tage.banks

  when (flush | kill) {
    resp.valid := false.B
  } .otherwise {
    resp.valid := io.req.valid
    resp.rob_id := io.req.meta.rob_id
    resp.data := io.pc + Mux(meta.len, 4.U, 2.U)
    resp.cause := 0.U
    rd_val := io.req.meta.rd_val
    rd_type := io.req.meta.rd_type
    rd := io.req.meta.rd
  }

  io.resp := resp
  io.bypass.valid := resp.valid && rd_val
  io.bypass.bits.rd_type := rd_type
  io.bypass.bits.rd := rd
  io.bypass.bits.data := resp.data
}