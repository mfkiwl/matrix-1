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

class AXI4RamExtra extends Bundle {
  val kill = Input(Bool())
}

class AXI4Ram(depth: Int)(params: AXI4Params = AXI4Params(), e: AXI4RamExtra = new AXI4RamExtra) extends AXI4SlaveNode(params, e) {
  val extra = io.extra.getOrElse(new AXI4RamExtra)

  val kill = WireInit(extra.kill)

  when (kill) {
    //  Write reset
    wr_state := s_ready
    aw_ready := true.B
    w_ready  := false.B
    b_valid  := false.B
    write_counter := 0.U
    wren     := false.B

    //  Read reset
    rd_state := s_ready
    ar_ready := true.B
    r_valid  := false.B
    read_counter := 0.U
    rden     := false.B
  }

  def memBytes = params.DATA_WIDTH / 8
  def offsetBits = log2Ceil(memBytes)
  def offsetMask = (1 << offsetBits) - 1
  def compute_idx(addr: UInt) = (addr & offsetMask.U) >> log2Ceil(memBytes)

  val mem = SyncReadMem(depth, Vec(memBytes, UInt(8.W)))
  //  Read
  val ridx = compute_idx(read_addr)
  r_resp := RegEnable(AXI4.RESP_OKAY.U, rden)
  r_data := Cat(mem.read(ridx.asUInt, rden).reverse)

  //  Write
  val widx = compute_idx(write_addr)
  val wdata = Seq.tabulate(memBytes) { i => io.axi.w.bits.data(8 * i + 7, 8 * i) }
  when (wren) {
    mem.write(widx.asUInt, VecInit(wdata), io.axi.w.bits.strb.asBools)
  }
  b_resp := RegEnable(AXI4.RESP_OKAY.U, wren)
}