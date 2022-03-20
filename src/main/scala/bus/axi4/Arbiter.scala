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

class AXI4Arbiter(
                   numChannels: Int = 2
                 ) extends Module {
  val io = IO(new Bundle() {
    val req = Input(Vec(numChannels, Bool()))
    val ack = Input(Vec(numChannels, Bool()))

    val grant = Output(Vec(numChannels, Bool()))
    val grant_valid = Output(Bool())
    val port = Output(UInt(log2Ceil(numChannels + 1).W))
  })

  val valid_mask = RegInit(UInt(numChannels.W), ~0.U(numChannels.W))
  val request = Reverse(Cat(io.req))

  val req_mask = PriorityEncoderOH(request & valid_mask)

  //  Set default
  val has_valid = req_mask.orR
  val which_port = OHToUInt(req_mask)
  val release = io.grant zip io.ack map { case (g, a) => g & a } reduce(_|_)

  io.grant := req_mask.asBools
  io.grant_valid := has_valid
  io.port := which_port
  when (has_valid) {
    when (io.grant_valid & !release) {
      //  Block state
      valid_mask := valid_mask
    } .otherwise {
      valid_mask  := Fill(numChannels, 1.U) << (which_port + 1.U)
    }
  }
}