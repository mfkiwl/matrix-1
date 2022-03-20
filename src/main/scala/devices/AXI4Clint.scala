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
package matrix.device

import chisel3._
import chisel3.util._
import matrix.bus.axi4._
import scala.collection.mutable.LinkedHashMap


object ClintConstants {
  def timeOffset = 0xbff8
  def msipBytes  = 4
  def timecmpBytes = 8
  def size = 0x10000
  def timeWidth = 64
  def ipiWidth = 32
  def ints = 2

  val msip = 0x02000000
  val timecmp_lo = 0x02004000
  val timecmp_hi = 0x02004004
  val time_lo = 0x0200bff8
  val time_hi = 0x0200bffc

  def msipOffset(hart: Int) = hart * msipBytes
  def timecmpOffset(hart: Int) = 0x4000 + hart * timecmpBytes
}


class AXI4ClintIO extends Bundle {
  val rtc_tick = Input(Bool())
  val timer_int = Output(Bool())
  val sft_int = Output(Bool())
}

class AXI4Clint(params: AXI4Params) extends AXI4SlaveNode(params, new AXI4ClintIO) {
  val extra = io.extra.getOrElse(new AXI4ClintIO)

  val time = RegInit(UInt(ClintConstants.timeWidth.W), 0.U)
  val timecmp_lo = Reg(UInt(32.W))
  val timecmp_hi = Reg(UInt(32.W))
  val msip = RegInit(UInt(ClintConstants.ipiWidth.W), 0.U)

  when (extra.rtc_tick) { time := time + 1.U }
  extra.sft_int := msip(0)
  extra.timer_int := time >= Cat(timecmp_hi, timecmp_lo)

  val read_mapping = LinkedHashMap[Int, Bits] (
    0x02000000 -> msip,
    0x02004000 -> timecmp_lo,
    0x02004004 -> timecmp_hi,
    0x0200bff8 -> time(31, 0),
    0x0200bffc -> time(ClintConstants.timeWidth - 1, 32)
  )

  //  Read
  val r_lookup_addr = read_mapping map { case (k, v) => k -> (read_addr === k.U) }
  r_data := RegEnable(Mux1H(for ((k, v) <- read_mapping) yield r_lookup_addr(k) -> v), rden)
  r_resp := RegEnable(AXI4.RESP_OKAY.U, rden)

  //  Write
  b_resp := RegEnable(AXI4.RESP_OKAY.U, wren)
  val w_lookup_addr = read_mapping map { case (k, v) => k -> (write_addr === k.U) }
  when (wren) {
    when (w_lookup_addr(ClintConstants.msip)) {
      msip := Cat(Fill(32, 0.U), io.axi.w.bits.data(0))
    }
    when (w_lookup_addr(ClintConstants.timecmp_lo)) {
      timecmp_lo := io.axi.w.bits.data
    }
    when (w_lookup_addr(ClintConstants.timecmp_hi)) {
      timecmp_hi := io.axi.w.bits.data
    }
  }
}


