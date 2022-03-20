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
package matrix.issue

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import matrix.common._
import matrix.ifu._
import matrix.utils._

class IssueSrcMeta(implicit p: Parameters) extends MatrixBundle
  with ScalarOpConstants {
  val ready     = Bool()
  val rtype     = UInt(RT_SZ.W)
  val src       = UInt(XLEN.W)
  val wakeup    = UInt(WAKEUP_SZ.W)
  val ltrack    = UInt(WAKEUP_SZ.W)
}

class IssueCtrlMeta(implicit p: Parameters) extends MatrixBundle
  with ScalarOpConstants {
  val valid   = Bool()
  val issued  = Bool()
  val rob_id  = UInt(robIdWidth.W)
  val port    = UInt(PORT_SZ.W)
}

class IssueDataMeta(implicit p: Parameters) extends MatrixBundle
  with ScalarOpConstants
  with MemoryOpConstants {
  val is_load     = Bool()
  val ld_id       = UInt(ldqIdWidth.W)
  val is_store    = Bool()
  val st_id       = UInt(stqIdWidth.W)
  val uop         = UInt(UOP_SZ.W)
  val len         = Bool()
  val usign       = Bool()
  val bw          = UInt(BW_SZ.W)
  val mem_cmd     = UInt(M_SZ.W)
  val wakeup      = UInt(WAKEUP_SZ.W)
  val rd_val      = Bool()
  val rd_type     = UInt(RT_SZ.W)
  val rd          = UInt(lregSz.W)
  val pred_info   = new PredictorResp
  val pc          = UInt(vaddrWidth.W)

  //  For RAS
  val is_jmp      = Bool()
  val is_call     = Bool()
  val is_ret      = Bool()
  val is_ret_then_call = Bool()
}

class IssueSlot(implicit p: Parameters) extends MatrixBundle
  with ScalarOpConstants {
  val ctrl = new IssueCtrlMeta
  val src1 = new IssueSrcMeta
  val src2 = new IssueSrcMeta
  val src3 = new IssueSrcMeta
  val data = new IssueDataMeta

  def valid     = ctrl.valid
  def rob_id    = ctrl.rob_id
  def port      = ctrl.port
  def issued    = ctrl.issued
  def src_are_ready = src1.ready && src2.ready && src3.ready
  val can_be_issued = valid && !issued && src_are_ready
  def load_relative = src1.ltrack.orR | src2.ltrack.orR | src3.ltrack.orR
  def create(ctrl: IssueCtrlMeta, rs1: IssueSrcMeta, rs2: IssueSrcMeta, rs3: IssueSrcMeta, data: IssueDataMeta) = {
    this.ctrl := ctrl
    this.src1 := rs1
    this.src2 := rs2
    this.src3 := rs3
    this.data := data
  }
  def shiftWakeup = {
    when (valid && !issued) {
      //  SRC1
      val src1_next_wakeup = src1.wakeup >> 1.U
      when (!src1.ready) {
        src1.ready := src1_next_wakeup(0)
      }
      src1.wakeup := src1_next_wakeup
      //  SRC2
      val src2_next_wakeup = src2.wakeup >> 1.U
      when (!src2.ready) {
        src2.ready := src2_next_wakeup(0)
      }
      src2.wakeup := src2_next_wakeup
      //  SRC3
      val src3_next_wakeup = src3.wakeup >> 1.U
      when (!src3.ready) {
        src3.ready := src3_next_wakeup(0)
      }
      src3.wakeup := src3_next_wakeup
    }
  }
  def replay = {
    when (src1.ltrack(0)) {
      ctrl.issued := false.B
    } .elsewhen (src2.ltrack(0)) {
      ctrl.issued := false.B
    } .elsewhen (src3.ltrack(0)) {
      ctrl.issued := false.B
    }
    src1.ltrack(0) | src2.ltrack(0) | src3.ltrack(0)
  }
  def grant = {
    when (src1.ltrack(0)) {
      ctrl.issued := false.B
    } .elsewhen (src2.ltrack(0)) {
      ctrl.issued := false.B
    } .elsewhen (src3.ltrack(0)) {
      ctrl.issued := false.B
    }
    valid := false.B
    src1.ltrack(0) | src2.ltrack(0) | src3.ltrack(0)
  }
  def bypass(rd: UInt, rtype: UInt, data: UInt) = {
    //  SRC1
    val src1_hit = !src1.ready && (rd === src1.src(lregSz-1, 0)) && (rtype === src1.rtype)
    when (src1_hit) {
      src1.src := data
    }
    //  SRC2
    val src2_hit = !src2.ready && (rd === src2.src(lregSz-1, 0)) && (rtype === src2.rtype)
    when (src2_hit) {
      src2.src := data
    }
    //  SRC3
    val src3_hit = !src3.ready && (rd === src3.src(lregSz-1, 0)) && (rtype === src3.rtype)
    when (src3_hit) {
      src3.src := data
    }
  }
  def shiftTrack = {
    src1.ltrack := src1.ltrack >> 1.U
    src2.ltrack := src2.ltrack >> 1.U
    src3.ltrack := src3.ltrack >> 1.U
  }
  def kill = {
    ctrl.valid := false.B
    ctrl.issued := false.B
  }
  def kill(rob_id: UInt) = {
    when (checkOld(rob_id, ctrl.rob_id)) {
      ctrl.valid := false.B
      ctrl.issued := false.B
    }
  }
  def issue = {
    issued := true.B
    when (!load_relative) {
      valid := false.B
    }
  }
  def wakeup(sel: Bool, rd_val: Bool, rtype: UInt, rd: UInt, wakeup: UInt, is_load: Bool) = {
    when (sel) {
      //  SRC1
      val src1_hit = !src1.ready && rd_val && (rtype === src1.rtype) && (rd === src1.src(lregSz - 1, 0))
      when(src1_hit) {
        src1.wakeup := wakeup
        val src1_next_ltrack = src1.ltrack >> 1.U
        when (is_load) {
          src1.ltrack := src1_next_ltrack | 4.U
        }
      }
      //  SRC2
      val src2_hit = !src1.ready && rd_val && (rtype === src2.rtype) && (rd === src1.src(lregSz-1, 0))
      when (src2_hit) {
        src2.wakeup := wakeup
        val src2_next_ltrack = src2.ltrack >> 1.U
        when (is_load) {
          src1.ltrack := src2_next_ltrack | 4.U
        }
      }
      //  SRC3
      val src3_hit = !src3.ready && rd_val && (rtype === src1.rtype) && (rd === src3.src(lregSz-1, 0))
      when (src3_hit) {
        src3.wakeup := wakeup
        val src3_next_ltrack = src3.ltrack >> 1.U
        when (is_load) {
          src3.ltrack := src3_next_ltrack | 4.U
        }
      }
    }
  }
}