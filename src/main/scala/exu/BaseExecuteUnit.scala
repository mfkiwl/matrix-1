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
import matrix.common._
import matrix.issue._
import matrix.ifu._
import matrix.utils._

class ExecuteUnitResp(hasFpu: Boolean = false)(implicit p: Parameters) extends MatrixBundle {
  val valid   = Bool()
  val rob_id  = UInt(rsvIdWidth.W)
  val flags   = UInt(5.W)
  val data    = UInt(XLEN.W)
  val cause   = UInt(EXLEN.W)
}


abstract class BaseExecuteUnit(
                                hasFpu: Boolean   = false,
                                hasLsu: Boolean   = false,
                                hasCsr: Boolean   = false,
                                hasAlu: Boolean   = false,
                                hasBAlu: Boolean  = false,
                                hasMul: Boolean   = false,
                                hasDiv: Boolean   = false
                              )(implicit p: Parameters) extends MatrixModule
  with ScalarOpConstants {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val kill = if (!hasBAlu) Input(new KillReq) else null
    val req = Input(new IssueCIQResp)
    val pred_info = if (hasBAlu) Input(new PredictorResp) else null
    val pc = if (hasBAlu || hasAlu) Input(UInt(vaddrWidth.W)) else null
    val resp = Output(new ExecuteUnitResp(hasFpu))

    val grant = if (hasLsu) Output(Bool()) else null
    val replay = if (hasLsu) Output(Bool()) else null
    val bypass = Valid(new IssueCIQBypass)
    val fflags = if (hasFpu) Input(UInt(5.W)) else null
    val fpu_rm = if (hasFpu) Input(UInt(3.W)) else null

    val div_stall = if (hasDiv) Output(Bool()) else null
    val fpu_stall = if (hasFpu) Output(Bool()) else null
    val fpu_b2b_stall = if (hasFpu) Output(Bool()) else null
  })

  val flush = WireInit(io.flush)
  val kill = io.kill.valid && checkOld(io.kill.rob_id, io.req.meta.rob_id)

  val rs1 = io.req.src1
  val rs2 = io.req.src2
  val rs3 = io.req.src3

  val meta = WireInit(io.req.meta)
  val resp = Reg(new ExecuteUnitResp(hasFpu))
  val rd_val = Reg(Bool())
  val rd_type = Reg(UInt(RT_SZ.W))
  val rd = Reg(UInt(XLEN.W))
}
