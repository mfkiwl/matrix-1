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
package matrix.vmm

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import matrix.common._

class PTWReq(dataOn: Boolean = false)(implicit p: Parameters) extends MatrixBundle with MemoryOpConstants {
  val cmd  = UInt(M_SZ.W)
  val addr = UInt(vaddrWidth.W)
  val data = if (dataOn) UInt() else null
}

class PTWResp(implicit p: Parameters) extends MatrixBundle {
  val pte     = new PTE
  val level   = UInt(log2Ceil(pgLevels).W)
  val cause   = UInt(EXLEN.W)
}