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
package matrix.ifu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import scala.math.min
import matrix.common._
import matrix.utils._

case class TAGEParams (
                        ghistLength: Int = 128,
                        tableInfo: Seq[Tuple3[Int, Int, Int]] = Seq((  128,       2,     7),
                          (  128,       4,     7),
                          (  256,       8,     8),
                          (  256,      16,     8),
                          (  128,      32,     9),
                          (  128,      64,     9)),
                        period: Int = 2048,
                        numOfBimEntries: Int = 2048
                      )

class TageTableResp(implicit p: Parameters) extends MatrixBundle {
  val hit     = Bool()
  val ctr     = UInt(3.W)
  val usb     = UInt(2.W)
  val tag     = UInt(9.W)
  val bank    = UInt(8.W)
}

class TageTableUpdate(implicit p: Parameters) extends MatrixBundle {
  val taken               = Bool()
  val old_cnt             = UInt(3.W)
  val old_usb             = UInt(2.W)
  val do_alloc            = Bool()
  val do_update_cnt       = Bool()
  val do_inc_cnt          = Bool()
  val do_flip_cnt         = Bool()
  val do_flip_cnt_lo      = Bool()
  val do_update_usb       = Bool()
  val do_inc_usb          = Bool()
  val tag                 = UInt(9.W)
  val bank                = UInt(8.W)
}

class TageTable(numOfSets: Int, tagSize: Int, histLength: Int)(implicit p: Parameters) extends MatrixModule {
  val io = IO(new Bundle() {
    val req  = Input(Bool())
    val pc   = Input(UInt(vaddrWidth.W))
    val hist  = Input(UInt(bpdParams.tage.ghistLength.W))
    val resp = Output(new TageTableResp)
    val update = Input(Valid(new TageTableUpdate))
  })

  def foldedHist(hist: UInt, len: Int) = {
    val nChunks = (histLength + len - 1) / len
    val histChunks = (0 until nChunks) map {
      i => hist(min((i + 1) * len, histLength) - 1, i * len)
    }
    histChunks.reduce(_^_)
  }

  def computeTagAndIndex(unhashedIndex: UInt, hist: UInt) = {
    val hashedIndex = unhashedIndex >> log2Ceil(fetchBytes)
    val indexHistory = foldedHist(hist, log2Ceil(numOfSets))
    val index = (hashedIndex.asUInt ^ indexHistory)(log2Ceil(numOfSets)-1,0)
    val tagHistory = foldedHist(hist, tagSize)
    val tag = ((unhashedIndex >> log2Ceil(numOfSets)).asUInt() ^ tagHistory)(tagSize-1,0)
    (index, tag)
  }

  class Meta extends Bundle {
    val tag = UInt(tagSize.W)
    val ctr = UInt(3.W)
    val usb = UInt(2.W)
  }

  val tagMetaSz = 2 + tagSize + 3

  //=============================================
  //  Name: Meta array.
  //  Desc: TAGE meta array
  //  Size: numOfSets
  //=============================================
  val meta_array = SyncReadMem(numOfSets, new Meta)

  //  Read
  val (ridx, rtag) = computeTagAndIndex(io.pc, io.hist)
  val rmeta = meta_array.read(ridx, io.req)
  io.resp.ctr     := rmeta.ctr
  io.resp.usb     := rmeta.usb
  io.resp.hit     := rmeta.tag === RegEnable(rtag, io.req)
  io.resp.tag     := RegEnable(rtag, io.req)
  io.resp.bank    := RegEnable(ridx, io.req)
  //  Update
  val (update_idx, update_tag) = (io.update.bits.bank, io.update.bits.tag(tagSize - 1, 0))

  val wentry = Wire(new Meta())
  val update = io.update.bits
  val wentry_ctr = Mux(update.do_alloc, Mux(update.taken, 4.U, 3.U),
    Mux(update.do_update_cnt, updateCounter(update.old_cnt, update.do_inc_cnt, 3), update.old_cnt))
  wentry.ctr := Mux(update.do_flip_cnt, Mux(update.do_flip_cnt_lo, wentry_ctr & 1.U, wentry_ctr & 2.U), wentry_ctr)
  wentry.usb := Mux(update.do_alloc, 0.U,
    Mux(update.do_update_usb, updateCounter(update.old_usb, update.do_inc_usb, 2), update.old_usb))
  wentry.tag := update_tag

  when (io.update.valid) {
    meta_array.write(update_idx, wentry)
  }
}

class TageTableMeta extends Bundle {
  val ctr = UInt(3.W)
  val usb = UInt(2.W)
}

class TageReq(implicit p: Parameters) extends MatrixBundle {
  val pc    = UInt(vaddrWidth.W)
}

class TageResp(implicit p: Parameters) extends MatrixBundle {
  val prime_taken = Bool()
  val prime_bank  = UInt(bpdParams.tage.tableInfo.size.W)
  val alt_taken   = Bool()
  val alt_bank    = UInt(bpdParams.tage.tableInfo.size.W)
  val bim_cnt     = UInt(2.W)
  val meta        = Vec(bpdParams.tage.tableInfo.size, new TageTableMeta)
  val tags        = Vec(bpdParams.tage.tableInfo.size, UInt())
  val banks       = Vec(bpdParams.tage.tableInfo.size, UInt())
}

class TageUpdate(implicit p: Parameters) extends TageResp {
  val taken = Bool()
}

class TageIO(implicit p: Parameters) extends MatrixBundle {
  val req = Flipped(Valid(new TageReq))
  val resp = Output(new TageResp)
  val update = Flipped(Valid(new TageUpdate))
  val update_pc = Input(UInt(vaddrWidth.W))
}

class Tage(implicit p: Parameters) extends MatrixModule {
  val io = IO(new TageIO)

  //============================================
  //  Name: Global history
  //  Desc: tage predictor history
  //  Size: bpdParams.tage.ghistLength
  //============================================
  val ghist = Reg(UInt(bpdParams.tage.ghistLength.W))
  val latest_ghist = Wire(UInt(bpdParams.tage.ghistLength.W))
  //============================================
  //  Name: Components
  //  Desc: tage predictor components
  //  Size:
  //============================================
  val tage_pred_fail = WireInit(false.B)
  val tables = (bpdParams.tage.tableInfo map {
    case (n, l, s) => {
      val t = Module(new TageTable(n, s, l))
      t.io.req := io.req.valid
      t.io.pc := io.req.bits.pc
      t.io.hist := latest_ghist
      t
    }
  })

  def numOfTables = bpdParams.tage.tableInfo.size
  //============================================
  //  Name: Alternative better counter
  //  Desc:
  //  Size: 4
  //============================================
  val alt_better_cnt = Reg(UInt(4.W))

  //============================================
  //  Name: Useful bit flip
  //  Desc: 0 flip LSB, 1 flip MSB
  //  Size: 1
  //============================================
  val flip = Reg(Bool())
  //============================================
  //  Name: Instruction counter
  //  Desc:
  //  Size: bpdParams.tage.period + 1
  //============================================
  val total_insts = Reg(UInt((log2Ceil(bpdParams.tage.period) + 1).W))

  //============================================
  //  Name: Bim table
  //  Desc: tage predictor bim table
  //  Size: bpdParams.tage.bimEntries
  //============================================
  val bim_table = SyncReadMem(bpdParams.tage.numOfBimEntries, UInt(2.W))
  //
  def BIM_CTR_WIDTH: Int = 2
  def BIM_NOT_TAKEN: UInt = 1.U(BIM_CTR_WIDTH.W)
  def computeBimIndex(pc: UInt, len: Int) = {
    val nChunks = (vaddrWidth + len - 1) / len
    val indexChunks = (0 until nChunks) map {
      i => pc(min((i + 1) * len, vaddrWidth) - 1, i * len)
    }
    indexChunks.reduce(_ ^ _)
  }


  //  Get prediction
  //  Get base prediction
  val base_idx = computeBimIndex(io.req.bits.pc, log2Ceil(bpdParams.tage.numOfBimEntries))
  val base_cnt = bim_table.read(base_idx, io.req.valid)
  val base_pred = base_cnt > BIM_NOT_TAKEN

  //  Get tage prediction
  val table_resp = VecInit(tables.map(_.io.resp))
  val match_vec = table_resp.map(_.hit)
  val bank_selected_oh = selectFirstN(Reverse(Cat(match_vec)), 2)
  val (prime_bank_oh, alt_bank_oh) = (bank_selected_oh(0), bank_selected_oh(1))
  val (prime_bank, alt_bank) = (OHToUInt(prime_bank_oh), OHToUInt(alt_bank_oh))
  val tage_pred = WireInit(base_pred)
  val prime_taken = WireInit(false.B)
  val alt_taken = WireInit(false.B)

  val has_valid_prime_bank = !prime_bank_oh.orR
  val has_valid_alt_bank = !alt_bank_oh.orR
  when (has_valid_prime_bank) {
    when (!has_valid_alt_bank) {
      alt_taken := base_pred
    } .otherwise {
      alt_taken := table_resp(alt_bank).ctr >= 3.U
    }
    when (table_resp(prime_bank).ctr =/= 3.U ||
    table_resp(prime_bank).ctr =/= 4.U ||
    table_resp(prime_bank).usb =/= 0.U ||
    alt_better_cnt < 8.U) {
      tage_pred := table_resp(prime_bank).ctr >= 3.U
    } .otherwise {
      tage_pred := alt_taken
    }
  } .otherwise {
    alt_taken := base_pred
  }

  val resp = Wire(new TageResp)
  resp.prime_taken := tage_pred
  resp.alt_taken := alt_taken
  resp.prime_bank := prime_bank_oh
  resp.alt_bank := alt_bank_oh
  for (t <- 0 until numOfTables) {
    resp.meta(t).usb := table_resp(t).usb
    resp.meta(t).ctr := table_resp(t).ctr
    resp.tags(t)     := table_resp(t).tag
    resp.banks(t)    := table_resp(t).bank
  }
  resp.bim_cnt := base_cnt
  io.resp := RegEnable(resp, io.req.valid)

  //============================= Update ================================//
  val update = WireInit(io.update.bits)
  val do_alloc = WireInit(false.B)
  val do_update_cnt = WireInit(false.B)
  val do_inc_cnt = WireInit(false.B)
  val do_flip_cnt = WireInit(false.B)
  val do_flip_cnt_lo = WireInit(false.B)
  val do_update_usb = WireInit(false.B)
  val do_inc_usb = WireInit(false.B)
  val do_update_bim = WireInit(false.B)
  val do_inc_bim = WireInit(false.B)

  val update_prime_bank = OHToUInt(update.prime_bank)
  val update_alt_bank  = OHToUInt(update.alt_bank)
  when (!update.prime_bank.orR) {
    when (update.prime_taken =/= update.alt_bank) {
      do_update_usb := true.B
      do_inc_usb := update.prime_taken === update.taken
    }
    do_update_cnt := true.B
    do_inc_cnt := update.taken
  } .otherwise {
    do_update_bim := true.B
    do_inc_bim := update.taken
  }
  val need_alloc = WireInit(false.B)
  when (!update.prime_bank.orR) {
    when (table_resp(update_prime_bank).usb === 0.U &&
      (table_resp(update_prime_bank).ctr === 3.U ||
        table_resp(update_prime_bank).ctr === 4.U)) {
      need_alloc := true.B
      when (update.prime_taken =/= update.alt_taken) {
        alt_better_cnt := updateCounter(alt_better_cnt, update.alt_taken === update.taken, 4)
      }
    }
  }

  //  Allocate a new entry
  //  Find useful bits == 0
  val alt_usb_bank_selected_oh = selectFirstN(Reverse(Cat(resp.meta.map(_.usb === 0.U))))
  when ((!need_alloc) || (need_alloc && (update.prime_taken =/= update.taken))) {
    when (update.prime_taken =/= update.taken) {
      when (!alt_usb_bank_selected_oh.orR) {
        do_alloc := false.B
        do_update_usb := true.B
      } .otherwise {
        do_alloc := true.B
        do_update_usb := false.B
      }
    }
  }

  //  Update the reset logic
  when (io.update.valid) {
    when (total_insts =/= bpdParams.tage.period.U) {
      total_insts := 0.U
      do_flip_cnt := true.B
      do_flip_cnt_lo := flip
      flip := !flip
    } .otherwise {
      total_insts := total_insts + 1.U
    }
  }

  //  Update the global history
  //  If update global history
  when (io.update.valid) {
    tage_pred_fail := update.taken =/= update.prime_taken
    latest_ghist := Cat(ghist(bpdParams.tage.ghistLength - 2, 0), !tage_pred_fail)
    ghist := latest_ghist
  } .otherwise {
    latest_ghist := ghist
  }

  //  Update the tage table
  for (t <- 0 until numOfTables) {
    val do_dec_usb = !do_alloc && (t.U < OHToUInt(update.prime_bank))
    tables(t).io.update.valid               := io.update.valid
    tables(t).io.update.bits.taken          := update.taken
    tables(t).io.update.bits.old_cnt        := update.meta(t).ctr
    tables(t).io.update.bits.old_usb        := update.meta(t).usb
    tables(t).io.update.bits.do_alloc       := do_alloc && (t.U === OHToUInt(alt_usb_bank_selected_oh))
    tables(t).io.update.bits.do_update_cnt  := do_update_cnt
    tables(t).io.update.bits.do_inc_cnt     := do_inc_cnt
    tables(t).io.update.bits.do_flip_cnt    := do_flip_cnt
    tables(t).io.update.bits.do_flip_cnt_lo := do_flip_cnt_lo
    tables(t).io.update.bits.do_update_usb  := do_update_usb
    tables(t).io.update.bits.do_inc_usb     := do_inc_usb && !do_dec_usb
    tables(t).io.update.bits.tag            := update.tags(t)
    tables(t).io.update.bits.bank           := update.banks(t)
  }
}