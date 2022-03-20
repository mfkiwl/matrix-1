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

class ALU(implicit p: Parameters) extends BaseExecuteUnit(
  hasAlu = true
) {
  val curUop = meta.uop
  def isLUI   = curUop === UOP_LUI
  def isADD   = curUop === UOP_ADD
  def isSUB   = curUop === UOP_SUB
  def isSLT   = curUop === UOP_SLT
  def isXOR   = curUop === UOP_XOR
  def isOR    = curUop === UOP_OR
  def isAND   = curUop === UOP_AND
  def isSLL   = curUop === UOP_SLL
  def isSRL   = curUop === UOP_SRL
  def isSRA   = curUop === UOP_SRA
  def isSET   = curUop === UOP_SET
  def isCLR   = curUop === UOP_CLR
  def isXCHG  = curUop === UOP_XCHG

  val rs2_inv = Mux(isSUB, ~rs2, rs2)
  val addsub_res = rs1 + rs2_inv + isSUB
  val luiResult = Cat(rs2(XLEN-1, 12), 0.U(12.W))
  val sltResult = Cat(Fill(XLEN-1, 0.U),
    Mux(meta.usign, rs1.asUInt < rs2.asUInt, rs1.asSInt < rs2.asSInt))
  val shamt = Cat(rs2(5) & meta.bw === BW_D, rs2(4, 0))
  val sll_res = rs1 << shamt
  val srl_res = rs1 >> shamt
  val sra_res = (rs1.asSInt >> shamt).asUInt
  val and_res = rs1 & rs2
  val or_res = rs1 | rs2
  val clr_res = ~rs1 & rs2
  val xor_res = rs1 ^ rs2
  val xchg_res = rs1

  val selectors = Seq(isLUI, isADD | isSUB, isSLT, isXOR,
    isOR | isSET, isAND, isSLL, isSRL, isSRA, isCLR, isXCHG)
  val results = Seq(luiResult, addsub_res, sltResult, xor_res,
    or_res, and_res, sll_res, srl_res, sra_res, clr_res, xchg_res)
  val alu_res = Mux1H(selectors, results)
  val normal_alu_res = Mux(meta.bw === BW_D, alu_res,
    Cat(Fill(XLEN/2, alu_res(XLEN/2-1)), alu_res(XLEN/2-1,0)))

  when (flush | kill) {
    resp.valid := false.B
  } .otherwise {
    resp.valid    := io.req.valid
    resp.rob_id   := io.req.meta.rob_id
    resp.data     := normal_alu_res
    resp.cause    := 0.U
    rd_val        := io.req.meta.rd_val
    rd_type       := io.req.meta.rd_type
    rd            := io.req.meta.rd
  }
  
  io.resp                 := resp
  io.bypass.valid         := resp.valid && rd_val
  io.bypass.bits.rd_type  := rd_type
  io.bypass.bits.rd       := rd
  io.bypass.bits.data     := resp.data
}
