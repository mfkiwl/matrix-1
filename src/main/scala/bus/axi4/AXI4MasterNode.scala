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

abstract class AXI4MasterNode[E<: Data](params: AXI4Params, e: E = None) extends Module {
  val io = IO(new Bundle() {
    val axi = new AXI4Bundle(params)
    val extra = Some(e)
  })

  val s_ready::s_data::s_resp::Nil = Enum(3)
  //============================= Write channel ==========================//
  val wr_state = RegInit(s_ready)

  //  Write command channel.
  val aw_valid        = WireInit(false.B)
  val aw_valid_reg    = RegInit(Bool(), false.B)
  val aw_id_reg       = Reg(UInt(params.ID_WIDTH.W))
  val aw_addr_reg     = Reg(UInt(params.ADDR_WIDTH.W))
  val aw_len_reg      = Reg(UInt(params.LEN_WIDTH.W))
  val aw_size_reg     = Reg(UInt(params.SIZE_WIDTH.W))
  val aw_burst_reg    = Reg(UInt(params.BURST_WIDTH.W))
  val aw_lock_reg     = Reg(UInt(params.LOCK_WIDTH.W))
  val aw_cache_reg    = Reg(UInt(params.CACHE_WIDTH.W))
  val aw_prot_reg     = Reg(UInt(params.CACHE_WIDTH.W))
  val aw_qos_reg      = Reg(UInt(params.QoS_WIDTH.W))
  val aw_region_reg   = Reg(UInt(params.REGION_WIDTH.W))
  val aw_user_reg     = Reg(UInt(params.USER_WIDTH.W))

  val aw_id           = Wire(UInt(params.ID_WIDTH.W))
  val aw_addr         = Wire(UInt(params.ADDR_WIDTH.W))
  val aw_len          = Wire(UInt(params.LEN_WIDTH.W))
  val aw_size         = Wire(UInt(params.SIZE_WIDTH.W))
  val aw_burst        = Wire(UInt(params.BURST_WIDTH.W))
  val aw_lock         = Wire(UInt(params.LOCK_WIDTH.W))
  val aw_cache        = Wire(UInt(params.CACHE_WIDTH.W))
  val aw_prot         = Wire(UInt(params.CACHE_WIDTH.W))
  val aw_qos          = Wire(UInt(params.QoS_WIDTH.W))
  val aw_region       = Wire(UInt(params.REGION_WIDTH.W))
  val aw_user         = Wire(UInt(params.USER_WIDTH.W))

  //  Write data channel.
  val w_valid     = RegInit(Bool(), false.B)
  val w_data_reg  = Reg(UInt(params.DATA_WIDTH.W))
  val w_strb_reg  = Reg(UInt(params.STRB_WIDTH.W))
  val w_last_reg  = Reg(Bool())
  val w_user_reg  = Reg(UInt(params.USER_WIDTH.W))


  val w_data = Wire(UInt(params.DATA_WIDTH.W))
  val w_strb = Wire(UInt(params.STRB_WIDTH.W))
  val w_last = Wire(Bool())
  val w_user = Wire(UInt(params.USER_WIDTH.W))

  //  Write response channel.
  val b_ready     = RegInit(Bool(), false.B)
  val b_id_reg    = Reg(UInt(params.ID_WIDTH.W))
  val b_resp_reg  = Reg(UInt(params.RESP_WIDTH.W))
  val b_user_reg  = Reg(UInt(params.USER_WIDTH.W))

  aw_valid_reg := false.B
  switch (wr_state) {
    is (s_ready) {
      when (aw_valid) {
        aw_valid_reg := true.B

        aw_addr_reg     := aw_addr
        aw_len_reg      := aw_len
        aw_size_reg     := aw_size
        aw_burst_reg    := aw_burst
        aw_lock_reg     := aw_lock
        aw_cache_reg    := aw_cache
        aw_prot_reg     := aw_prot
        aw_qos_reg      := aw_qos
        aw_region_reg   := aw_region
        aw_user_reg     := aw_user
      }
      when (io.axi.aw.fire) {
        aw_valid_reg    := false.B
        w_valid         := true.B

        w_data_reg := w_data
        w_strb_reg := w_strb
        w_last_reg := w_last
        w_user_reg := w_user

        wr_state := s_data
      }
    }
    is (s_data) {
      when (io.axi.w.fire) {
        when (w_last_reg) {
          w_valid     := false.B
          b_ready     := true.B
          wr_state    := s_resp
        } .otherwise {
          w_data_reg  := w_data
          w_strb_reg  := w_strb
          w_last_reg  := w_last
          w_user_reg  := w_user
          wr_state    := s_data
        }
      }
    }
    is (s_resp) {
      when (io.axi.b.fire) {
        b_ready     := false.B
        b_id_reg    := io.axi.b.bits.id
        b_user_reg  := io.axi.b.bits.user
        b_resp_reg  := io.axi.b.bits.resp
        wr_state    := s_ready
      }
    }
  }

  //  Write command channel.
  io.axi.aw.valid         := aw_valid_reg
  io.axi.aw.bits.id       := aw_id_reg
  io.axi.aw.bits.addr     := aw_addr_reg
  io.axi.aw.bits.len      := aw_len_reg
  io.axi.aw.bits.size     := aw_size_reg
  io.axi.aw.bits.burst    := aw_burst_reg
  // io.axi.aw.bits.lock     := aw_lock_reg
  io.axi.aw.bits.cache    := aw_cache_reg
  io.axi.aw.bits.prot     := aw_prot_reg
  io.axi.aw.bits.qos      := aw_qos_reg
  io.axi.aw.bits.region   := aw_region_reg
  io.axi.aw.bits.user     := aw_user_reg

  //  Write data channel.
  io.axi.w.valid          := w_valid
  io.axi.w.bits.data      := w_data_reg
  io.axi.w.bits.strb      := w_strb_reg
  io.axi.w.bits.last      := w_last_reg
  io.axi.w.bits.user      := w_user_reg

  //  Write response channel.
  io.axi.b.ready          := b_ready


  //============================= Read channel ===========================//
  val rd_state = RegInit(s_ready)

  //  Read command channel.
  val ar_valid        = WireInit(false.B)
  val ar_valid_reg    = RegInit(Bool(), false.B)
  val ar_id_reg       = Reg(UInt(params.ID_WIDTH.W))
  val ar_addr_reg     = Reg(UInt(params.ADDR_WIDTH.W))
  val ar_len_reg      = Reg(UInt(params.LEN_WIDTH.W))
  val ar_size_reg     = Reg(UInt(params.SIZE_WIDTH.W))
  val ar_burst_reg    = Reg(UInt(params.BURST_WIDTH.W))
  val ar_lock_reg     = Reg(UInt(params.LOCK_WIDTH.W))
  val ar_cache_reg    = Reg(UInt(params.CACHE_WIDTH.W))
  val ar_prot_reg     = Reg(UInt(params.CACHE_WIDTH.W))
  val ar_qos_reg      = Reg(UInt(params.QoS_WIDTH.W))
  val ar_region_reg   = Reg(UInt(params.REGION_WIDTH.W))
  val ar_user_reg     = Reg(UInt(params.USER_WIDTH.W))

  val ar_id       = Wire(UInt(params.ID_WIDTH.W))
  val ar_addr     = Wire(UInt(params.ADDR_WIDTH.W))
  val ar_len      = Wire(UInt(params.LEN_WIDTH.W))
  val ar_size     = Wire(UInt(params.SIZE_WIDTH.W))
  val ar_burst    = Wire(UInt(params.BURST_WIDTH.W))
  val ar_lock     = Wire(UInt(params.LOCK_WIDTH.W))
  val ar_cache    = Wire(UInt(params.CACHE_WIDTH.W))
  val ar_prot     = Wire(UInt(params.CACHE_WIDTH.W))
  val ar_qos      = Wire(UInt(params.QoS_WIDTH.W))
  val ar_region   = Wire(UInt(params.REGION_WIDTH.W))
  val ar_user     = Wire(UInt(params.USER_WIDTH.W))

  //  Read response channel.
  val r_ready     = RegInit(Bool(), false.B)
  val r_data_reg  = Reg(UInt(params.DATA_WIDTH.W))
  val r_resp_reg  = Reg(UInt(params.RESP_WIDTH.W))
  val r_last_reg  = Reg(Bool())
  val r_user_reg  = Reg(UInt(params.USER_WIDTH.W))

  ar_valid_reg := false.B
  r_ready := false.B
  switch (rd_state) {
    is (s_ready) {
      when (ar_valid) {
        ar_valid_reg    := true.B
        ar_addr_reg     := ar_addr
        ar_len_reg      := ar_len
        ar_size_reg     := ar_size
        ar_burst_reg    := ar_burst
        ar_lock_reg     := ar_lock
        ar_cache_reg    := ar_cache
        ar_prot_reg     := ar_prot
        ar_qos_reg      := ar_qos
        ar_region_reg   := ar_region
        ar_user_reg     := ar_user
        r_ready         := true.B
      }
      when (io.axi.ar.fire) {
        ar_valid_reg    := false.B
        r_ready         := true.B
        rd_state        := s_resp
      }
    }
    is (s_resp) {
      when (io.axi.r.fire) {
        r_ready     := false.B
        r_data_reg  := io.axi.r.bits.data
        r_resp_reg  := io.axi.r.bits.resp
        r_last_reg  := io.axi.r.bits.last
        r_user_reg  := io.axi.r.bits.user
        rd_state    := s_ready
      }
    }
  }

  io.axi.ar.valid         := ar_valid_reg
  io.axi.ar.bits.id       := ar_id_reg
  io.axi.ar.bits.addr     := ar_addr_reg
  io.axi.ar.bits.len      := ar_len_reg
  io.axi.ar.bits.size     := ar_size_reg
  io.axi.ar.bits.burst    := ar_burst_reg
  // io.axi.ar.bits.lock     := ar_lock_reg
  io.axi.ar.bits.cache    := ar_cache_reg
  io.axi.ar.bits.prot     := ar_prot_reg
  io.axi.ar.bits.qos      := ar_qos_reg
  io.axi.ar.bits.region   := ar_region_reg
  io.axi.ar.bits.user     := ar_user_reg
  //  Data
  io.axi.r.ready          := r_ready
}