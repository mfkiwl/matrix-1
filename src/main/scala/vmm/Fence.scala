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
import freechips.rocketchip.config.Parameters
import matrix.common._

class SFenceReq(implicit p: Parameters) extends MatrixBundle {
  val rs1   = Bool()
  val rs2   = Bool()
  val vpn   = UInt(ppnWidth.W) // rs2
  val asid  = UInt(ASIDLEN.W)  // rs1
}