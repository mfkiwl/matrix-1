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
package matrix.common

import chisel3._
import freechips.rocketchip.config.Parameters

class MicroOp(implicit p: Parameters) extends MatrixBundle with ScalarOpConstants with MemoryOpConstants {
  val len       = Bool()
  val port      = UInt(PORT_SZ.W)
  val uop       = UInt(UOP_SZ.W)
  val fp_val    = Bool()
  val usign     = Bool()
  val bw        = UInt(BW_SZ.W)
  val rs1_val   = Bool()
  val rs1_type  = UInt(RT_SZ.W)
  val rs1       = UInt(lregSz.W)
  val rs2_val   = Bool()
  val rs2_type  = UInt(RT_SZ.W)
  val rs2       = UInt(lregSz.W)
  val rs3_val   = Bool()
  val rs3_type  = UInt(RT_SZ.W)
  val rs3       = UInt(lregSz.W)
  val rd_val    = Bool()
  val rd_type   = UInt(RT_SZ.W)
  val rd        = UInt(lregSz.W)
  val wakeup    = UInt(WAKEUP_SZ.W)
  val mem_cmd   = UInt(M_SZ.W)
}