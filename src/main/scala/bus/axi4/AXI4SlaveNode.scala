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

abstract class AXI4SlaveNode[E<: Data](params: AXI4Params, e: E = None) extends Module {
  val io = IO(new Bundle() {
    val axi = Flipped(new AXI4Bundle(params))
    val extra = Some(e)
  })

  val s_ready::s_data::s_resp::Nil = Enum(3)
  //============================= Write channel ==========================//
  val wr_state = RegInit(s_ready)

  val aw_hsk          = io.axi.aw.fire
  val aw_ready        = RegInit(Bool(), true.B)
  val aw_id           = Reg(UInt(params.ID_WIDTH.W))
  val aw_size         = Reg(UInt(params.SIZE_WIDTH.W))
  val aw_burst        = Reg(UInt(params.BURST_WIDTH.W))
  val aw_user         = Reg(UInt(params.USER_WIDTH.W))
  val write_counter   = Reg(UInt(params.LEN_WIDTH.W))
  val write_addr      = Reg(UInt(params.ADDR_WIDTH.W))

  val b_valid         = RegInit(Bool(), false.B)
  val b_id            = Reg(UInt(params.ID_WIDTH.W))
  val b_user          = Reg(UInt(params.USER_WIDTH.W))
  val b_resp          = Wire(UInt(params.RESP_WIDTH.W))

  val w_ready         = RegInit(Bool(), false.B)
  val wren            = io.axi.w.fire

  //  Default values
  aw_ready    := false.B
  w_ready     := false.B
  b_valid     := b_valid & !io.axi.b.ready

  switch (wr_state) {
    is (s_ready) {
      aw_ready                := !aw_hsk
      when (aw_hsk) {
        aw_id               := io.axi.aw.bits.id
        write_addr          := io.axi.aw.bits.addr
        write_counter       := io.axi.aw.bits.len
        aw_size             := io.axi.aw.bits.size
        aw_burst            := io.axi.aw.bits.burst
        w_ready             := true.B
        wr_state            := s_data
      }
    }
    is (s_data) {
      w_ready := true.B
      when (io.axi.w.fire) {
        when (aw_burst =/= AXI4.BURST_FIXED.U) {
          write_addr      := write_addr + (1.U << aw_size)
        }
        when (write_counter === 0.U) {
          w_ready         := false.B
          when (io.axi.b.ready || !b_valid) {
            b_valid     := true.B
            aw_ready    := true.B
            b_id        := aw_id
            b_user      := aw_user
            wr_state    := s_ready
          } .otherwise {
            wr_state    := s_resp
          }
        }
        write_counter       := write_counter - 1.U
      }
    }
    is (s_resp) {
      when (io.axi.b.ready || !b_valid) {
        b_valid     := true.B
        aw_ready    := true.B
        b_id        := aw_id
        b_user      := aw_user
        wr_state    := s_ready
      } .otherwise {
        wr_state    := s_resp
      }
    }
  }
  io.axi.aw.ready     := aw_ready
  io.axi.w.ready      := w_ready
  io.axi.b.valid      := b_valid
  io.axi.b.bits.id    := b_id
  io.axi.b.bits.user  := b_user
  io.axi.b.bits.resp  := b_resp

  //============================= Read channel ===========================//
  val rd_state = RegInit(s_ready)

  //  Read command channel.
  val ar_hsk      = io.axi.ar.fire
  val ar_id       = Reg(UInt(params.ID_WIDTH.W))
  val ar_user     = Reg(UInt(params.USER_WIDTH.W))
  val ar_size     = Reg(UInt(params.SIZE_WIDTH.W))
  val ar_burst    = Reg(UInt(params.BURST_WIDTH.W))
  val ar_ready    = RegInit(Bool(), true.B)

  //  Read response channel.
  val r_hsk           = io.axi.r.fire
  val r_valid         = RegInit(Bool(), false.B)
  val r_data          = Wire(UInt(params.DATA_WIDTH.W))
  val r_resp          = Wire(UInt(params.RESP_WIDTH.W))
  val r_last          = RegInit(Bool(), false.B)
  val read_counter    = Reg(UInt(params.LEN_WIDTH.W))
  val read_addr       = Reg(UInt(params.ADDR_WIDTH.W))
  val rden            = Wire(Bool())

  //  Default values
  ar_ready    := false.B
  r_valid     := r_valid && !io.axi.r.ready
  rden        := io.axi.r.ready || !r_valid

  switch (rd_state) {
    is (s_ready) {
      ar_ready    := !ar_hsk
      when (ar_hsk) {
        ar_id           := io.axi.ar.bits.id
        ar_size         := io.axi.ar.bits.size
        ar_burst        := io.axi.ar.bits.burst
        ar_user         := io.axi.ar.bits.user
        read_counter    := io.axi.ar.bits.len
        read_addr       := io.axi.ar.bits.addr
        r_last          := false.B
        ar_ready        := false.B
      }
    }
    is (s_data) {
      when (io.axi.r.ready || !r_valid) {
        r_valid := true.B
        when (ar_burst =/= AXI4.BURST_FIXED.U) {
          read_addr   := read_addr + (1.U << ar_size)
        }
        when (read_counter === 0.U) {
          ar_ready    := true.B
          rd_state    := s_ready
          r_last      := true.B
        } .otherwise {
          r_last      := false.B
        }
        read_counter    := read_counter - 1.U
      }
    }
  }

  io.axi.ar.ready     := ar_ready
  io.axi.r.valid      := r_valid
  io.axi.r.bits.id    := ar_id
  io.axi.r.bits.user  := ar_user
  io.axi.r.bits.data  := r_data
  io.axi.r.bits.resp  := r_resp
  io.axi.r.bits.last  := r_last
}