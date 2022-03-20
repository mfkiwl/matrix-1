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
package matrix.bus.axi4

import chisel3._
import chisel3.util._

abstract class AXI4BundleA(params: AXI4Params) extends Bundle {
  val id      = UInt(params.ID_WIDTH.W)
  val addr    = UInt(params.ADDR_WIDTH.W)
  val len     = UInt(params.LEN_WIDTH.W)
  val size    = UInt(params.SIZE_WIDTH.W)
  val burst   = UInt(params.BURST_WIDTH.W)
  val cache   = UInt(params.CACHE_WIDTH.W)
  val prot    = UInt(params.PROT_WIDTH.W)
  val qos     = UInt(params.QoS_WIDTH.W)
  val region  = UInt(params.REGION_WIDTH.W)
  val user    = UInt(params.USER_WIDTH.W)
}

abstract class AXI4BundleBR(params: AXI4Params) extends Bundle {
  val id      = UInt(params.ID_WIDTH.W)
  val resp    = UInt(params.RESP_WIDTH.W)
  val user    = UInt(params.USER_WIDTH.W)
}

//========================== Write channel =========================//
class AXI4BundleAW(params: AXI4Params) extends AXI4BundleA(params)
class AXI4BundleB(params: AXI4Params) extends AXI4BundleBR(params)
class AXI4BundleW(params: AXI4Params) extends Bundle {
  val data = UInt(params.DATA_WIDTH.W)
  val strb = UInt(params.STRB_WIDTH.W)
  val last = Bool()
  val user = UInt(params.USER_WIDTH.W)
}

//========================== Read channel ==========================//
class AXI4BundleAR(params: AXI4Params) extends AXI4BundleA(params)
class AXI4BundleR(params: AXI4Params) extends AXI4BundleBR(params) {
  val data = UInt(params.DATA_WIDTH.W)
  val last = Bool()
}

class AXI4Bundle(params: AXI4Params) extends Bundle {
  val aw = Decoupled(new AXI4BundleAW(params))
  val w  = Decoupled(new AXI4BundleW(params))
  val b  = Flipped(Decoupled(new AXI4BundleB(params)))
  val ar = Decoupled(new AXI4BundleAR(params))
  val r  = Flipped(Decoupled(new AXI4BundleR(params)))
}

object AXI4Bundle {
  def apply(params: AXI4Params) = new AXI4Bundle(params)
}

