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

class PTE(implicit p: Parameters) extends MatrixBundle {
  //  For Sv48
  val n       = Bool()
  val pbmt    = UInt(2.W)
  val zeros   = UInt(7.W)
  val ppn     = UInt(ppnWidth.W)
  val rsw     = UInt(2.W)
  val d       = Bool()
  val a       = Bool()
  val g       = Bool()
  val u       = Bool()
  val x       = Bool()
  val w       = Bool()
  val r       = Bool()
  val v       = Bool()

  def pointer = v && !r && !w && !x
  def leaf    = v && (r || (x && !w)) && a
  def sr      = leaf && r
  def sw      = leaf && w
  def sx      = leaf && x
  def ur      = sr && u
  def uw      = sw && w
  def ux      = sx && x
}