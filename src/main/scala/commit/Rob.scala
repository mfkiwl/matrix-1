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
package matrix.commit

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import matrix.common._
import matrix.utils._

class RobCtrlMeta(implicit p: Parameters) extends MatrixBundle {
  val valid = Bool()
  val robId = UInt(robIdWidth.W)
  val cause = UInt(EXLEN.W)
  val fflags= UInt(5.W)
  val ready = Bool()
}

class RobDataMeta(implicit p: Parameters) extends MatrixBundle
 with ScalarOpConstants {
  val uop       = UInt(UOP_SZ.W)
  val is_load   = Bool()
  val ld_id     = UInt(ldqIdWidth.W)
  val is_store  = Bool()
  val st_id     = UInt(stqIdWidth.W)
  val rs1_val   = Bool()
  val rs2_val   = Bool()
  val rs3_val   = Bool()
  val rd_val    = Bool()
  val rd_type   = UInt(RT_SZ.W)
  val rd        = UInt(lregSz.W)
  val order     = Bool()
}

class RobDispatchReq(implicit p: Parameters) extends MatrixBundle
 with ScalarOpConstants {
  val valid   = Bool()
  val rob_id  = UInt(robIdWidth.W)
  val meta    = new RobDataMeta
  val cause   = UInt(EXLEN.W)
  val pc      = UInt(vaddrWidth.W)
}