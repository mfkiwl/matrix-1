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
import matrix.exu._
import matrix.utils._

class IssueSrcReq(implicit p: Parameters) extends MatrixBundle with ScalarOpConstants {
  val valid = Bool()
  val busy  = Bool()
  val rtype = UInt(RT_SZ.W)
  val src   = UInt(XLEN.W)
}

class IssueCIQReq(implicit p: Parameters) extends MatrixBundle
  with ScalarOpConstants {
  val valid       = Bool()
  val rob_id      = UInt(robIdWidth.W)
  val rsv_id      = UInt(rsvIdWidth.W)
  val port        = UInt(PORT_SZ.W)
  val src1_meta   = new IssueSrcReq
  val src2_meta   = new IssueSrcReq
  val src3_meta   = new IssueSrcReq
  val data_meta   = new IssueDataMeta
}

class IssueCIQMeta(implicit p: Parameters) extends MatrixBundle
  with ScalarOpConstants
  with MemoryOpConstants {
  val rob_id      = UInt(robIdWidth.W)
  val ld_id       = UInt(ldqIdWidth.W)
  val st_id       = UInt(stqIdWidth.W)
  val uop         = UInt(UOP_SZ.W)
  val len         = Bool()
  val usign       = Bool()
  val bw          = UInt(BW_SZ.W)
  val mem_cmd     = UInt(M_SZ.W)
  val rd_val      = Bool()
  val rd_type     = UInt(RT_SZ.W)
  val rd          = UInt(lregSz.W)

  //  For only RAS
  val is_jmp      = Bool()
  val is_call     = Bool()
  val is_ret      = Bool()
  val is_ret_then_call = Bool()
}

class IssueCIQResp(implicit p: Parameters) extends MatrixBundle
  with ScalarOpConstants {
  val valid   = Bool()
  val meta    = new IssueCIQMeta
  val src1    = UInt(XLEN.W)
  val src2    = UInt(XLEN.W)
  val src3    = UInt(XLEN.W)
}

class IssueCIQBypass(implicit p: Parameters) extends MatrixBundle
  with ScalarOpConstants {
  val valid   = Bool()
  val rd_type  = UInt(RT_SZ.W)
  val rd      = UInt(lregSz.W)
  val data    = UInt(XLEN.W)
}

class IssueCIQGrant(implicit p: Parameters) extends MatrixBundle {
  val valid = Bool()
  val rsv_id = UInt(rsvIdWidth.W)
}

class IssueCIQIO(numOfIssuePorts: Int)(implicit p: Parameters) extends MatrixBundle
  with ScalarOpConstants {
  val flush = Input(Bool())
  val kill = Input(new KillReq)
  val req = Flipped(Valid(Vec(decodeWidth, new IssueCIQReq)))
  val resp = Output(Vec(numOfIssuePorts, new IssueCIQResp))
  val replay = Input(Bool())
  val grant = Input(Bool())
  val bypass = Input(Vec(numOfIssuePorts, new IssueCIQBypass))

  val div_stall = Input(Bool())
  val fpu_stall = Input(Bool())
  val fpu_b2b_stall = Input(Bool())
}

class IssueCIQ(numOfIssuePorts: Int)(implicit p: Parameters) extends MatrixModule
  with ScalarOpConstants {
  val io = IO(new IssueCIQIO(numOfIssuePorts))
  
  val flush = WireInit(io.flush)
  val kill = WireInit(io.kill.valid)
  
  val issue_ciq = Reg(Vec(numOfRsvEntries, new IssueSlot))
  val issue_age_matrix = Seq.fill(numOfIssuePorts) { Module(new IssueAgeMatrix) }
  //============================ Age matrix ===================================//
  val age_matrix_valid_vec = Wire(Vec(numOfIssuePorts, UInt(numOfRsvEntries.W)))
  for ((m, i) <- issue_age_matrix zipWithIndex) {
    m.io.flush := io.flush
    m.io.kill.valid := io.kill.valid
    val killVec = issue_ciq map { s => s.valid && checkOld(io.kill.rob_id, s.rob_id) && io.kill.valid }
    m.io.kill.bits := Reverse(Cat(killVec))
    m.io.req.valid := io.req.valid
    for (w <- 0 until decodeWidth) {
      m.io.req.bits(w).valid := io.req.bits(w).valid && io.req.bits(w).port === i.U
      m.io.req.bits(w).rsv_id := io.req.bits(w).rsv_id
    }
    m.io.valids_vec := age_matrix_valid_vec(i)
  }
  //========================== Issue Queue ===================================//
  val ctrl_meta = Wire(Vec(decodeWidth, new IssueCtrlMeta))
  val src1_meta = Wire(Vec(decodeWidth, new IssueSrcMeta))
  val src2_meta = Wire(Vec(decodeWidth, new IssueSrcMeta))
  val src3_meta = Wire(Vec(decodeWidth, new IssueSrcMeta))
  val data_meta = Wire(Vec(decodeWidth, new IssueDataMeta))

  for (w <- 0 until decodeWidth) {
    //  SRC1
    val src1BypassHits = io.bypass.map(i => {
      i.valid &&
        io.req.bits(w).src1_meta.valid &&
        io.req.bits(w).src1_meta.busy &&
        i.rd_type === io.req.bits(w).src1_meta.rtype &&
        i.rd === io.req.bits(w).src1_meta.src(lregSz-1, 0)
    })
    val src1BypassHit = src1BypassHits.reduce(_|_)
    val src1 = Mux(src1BypassHit, Mux1H(src1BypassHits, io.bypass.map(_.data)),
      io.req.bits(w).src1_meta.src)
    src1_meta(w).ready := !io.req.bits(w).src1_meta.valid | src1BypassHit | !io.req.bits(w).src1_meta.busy
    src1_meta(w).rtype := io.req.bits(w).src1_meta.rtype
    src1_meta(w).src   := src1
    src1_meta(w).wakeup:= 0.U
    src1_meta(w).ltrack := 0.U
    //  SRC2
    val src2BypassHits = io.bypass.map(i => {
      i.valid &&
        io.req.bits(w).src2_meta.valid &&
        io.req.bits(w).src2_meta.busy &&
        i.rd_type === io.req.bits(w).src2_meta.rtype &&
        i.rd === io.req.bits(w).src2_meta.src(lregSz-1, 0)
    })
    val src2BypassHit = src2BypassHits.reduce(_|_)
    val src2 = Mux(src2BypassHit, Mux1H(src2BypassHits, io.bypass.map(_.data)),
      io.req.bits(w).src2_meta.src)
    src2_meta(w).ready := !io.req.bits(w).src2_meta.valid | src2BypassHit | !io.req.bits(w).src2_meta.busy
    src2_meta(w).rtype := io.req.bits(w).src2_meta.rtype
    src2_meta(w).src   := src2
    src2_meta(w).wakeup:= 0.U
    src2_meta(w).ltrack := 0.U
    //  SRC3
    val src3BypassHits = io.bypass.map(i => {
      i.valid &&
        io.req.bits(w).src3_meta.valid &&
        io.req.bits(w).src3_meta.busy &&
        i.rd_type === io.req.bits(w).src3_meta.rtype &&
        i.rd === io.req.bits(w).src3_meta.src(lregSz-1, 0)
    })
    val src3BypassHit = src3BypassHits.reduce(_|_)
    val src3 = Mux(src3BypassHit, Mux1H(src3BypassHits, io.bypass.map(_.data)),
      io.req.bits(w).src3_meta.src)
    src3_meta(w).ready := !io.req.bits(w).src3_meta.valid | src3BypassHit | !io.req.bits(w).src3_meta.busy
    src3_meta(w).rtype := io.req.bits(w).src3_meta.rtype
    src3_meta(w).src   := src3
    src3_meta(w).wakeup:= 0.U
    src3_meta(w).ltrack := 0.U

    //  Contrl meta
    ctrl_meta(w).valid   := io.req.bits(w).valid
    ctrl_meta(w).issued  := false.B
    ctrl_meta(w).rob_id   := io.req.bits(w).rob_id
    ctrl_meta(w).port    := io.req.bits(w).port

    //  Data meta
    data_meta(w) := io.req.bits(w).data_meta
    when (io.req.valid && io.req.bits(w).valid && !flush && !kill) {
      issue_ciq(io.req.bits(w).rsv_id).create(ctrl_meta(w), src1_meta(w), src2_meta(w), src3_meta(w), data_meta(w))
    }
  }
  //  Shift wakeup
  issue_ciq.foreach(_.shiftWakeup)
  //  Grant and replay
  val grant_entries = Wire(UInt(numOfRsvEntries.W))
  val replay_entries = Wire(UInt(numOfRsvEntries.W))
  val need_grant = io.grant
  val need_replay = io.replay
  when (need_grant) {
    grant_entries := Reverse(Cat(issue_ciq.map(_.grant)))
  } .otherwise {
    grant_entries := 0.U
  }
  when (need_replay) {
    replay_entries := Reverse(Cat(issue_ciq.map(_.replay)))
  } .otherwise {
    replay_entries := 0.U
  }
  //  Bypass
  for (w <- 0 until numOfIssuePorts) {
    when (io.bypass(w).valid) {
      issue_ciq.foreach(_.bypass(io.bypass(w).rd,
        io.bypass(w).rd_type, io.bypass(w).data))
    }
  }
  //  Kill
  for (s <- issue_ciq) {
    when (flush) {
      s.kill
    } .elsewhen (kill) {
      s.kill(io.kill.rob_id)
    }
  }
  //============================== Select logic ==============================//
  val oldest = issue_age_matrix map { m => m.io.oldest }
  val can_be_issued = Wire(Vec(numOfIssuePorts, UInt(numOfRsvEntries.W)))
  for (w <- 0 until numOfIssuePorts) {
    can_be_issued(w) := Reverse(Cat(issue_ciq.map(s => s.can_be_issued && s.port === w.U)))
  }
  val valids_vec = oldest zip can_be_issued map { case (o, v) => o & v }
  val has_valids = valids_vec.map(_.orR)
  val valid_slots = valids_vec.map(OHToUInt(_))
  val slots_selected = valid_slots map { s => issue_ciq(s) }
  val issue = Wire(Vec(numOfIssuePorts, new IssueCIQResp))
  for (w <- 0 until numOfIssuePorts) {
    when (!flush && (!kill | (kill && checkOld(slots_selected(w).rob_id, io.kill.rob_id)))) {
      issue(w).valid          := has_valids(w)
      issue(w).meta.rob_id    := slots_selected(w).rob_id
      issue(w).meta.ld_id     := slots_selected(w).data.ld_id
      issue(w).meta.st_id     := slots_selected(w).data.st_id
      issue(w).meta.uop       := slots_selected(w).data.uop
      issue(w).meta.len       := slots_selected(w).data.len
      issue(w).meta.usign     := slots_selected(w).data.usign
      issue(w).meta.bw        := slots_selected(w).data.bw
      issue(w).meta.mem_cmd   := slots_selected(w).data.mem_cmd
      issue(w).meta.rd_val    := slots_selected(w).data.rd
      issue(w).meta.rd_type   := slots_selected(w).data.rd_type
      issue(w).meta.rd        := slots_selected(w).data.rd
      issue(w).src1           := slots_selected(w).src1.src
      issue(w).src2           := slots_selected(w).src2.src
      issue(w).src3           := slots_selected(w).src3.src
      issue(w).meta.is_jmp    := slots_selected(w).data.is_jmp
      issue(w).meta.is_ret    := slots_selected(w).data.is_ret
      issue(w).meta.is_call   := slots_selected(w).data.is_call
      issue(w).meta.is_ret_then_call := slots_selected(w).data.is_ret_then_call
      when (has_valids(w)) {
        issue_ciq(valid_slots(w)).issue
      }
    } .otherwise {
      issue(w) := 0.U.asTypeOf(new IssueCIQResp)
    }
  }
  io.resp := RegNext(issue)
  //============================= Wakeup logic ================================//
  issue_ciq.foreach(_.shiftTrack)
  for (w <- 0 until numOfIssuePorts) {
    for ((s, i) <- issue_ciq zipWithIndex) {
      s.wakeup(has_valids(w) && valid_slots(w) =/= i.U,
        slots_selected(w).data.rd_val,
        slots_selected(w).data.rd_type,
        slots_selected(w).data.rd,
        slots_selected(w).data.wakeup,
        slots_selected(w).data.is_load)
    }
  }
  val entries_valid_vec = Wire(Vec(numOfIssuePorts, UInt(numOfRsvEntries.W)))
  val div_valid_vec = Wire(UInt(numOfRsvEntries.W))
  val fpu_valid_vec = Wire(UInt(numOfRsvEntries.W))
  val b2b_valid_vec = Wire(UInt(numOfRsvEntries.W))
  val fpu_uop = Seq(UOP_SQRT, UOP_DIV, UOP_MUL)

  for (w <- 0 until numOfIssuePorts) {
    entries_valid_vec(w) := Reverse(Cat(issue_ciq.map(s => {
      s.valid && !s.issued && w.U === s.port && Mux(w.U === PORT_DIV, !io.div_stall,
        Mux(w.U === PORT_FPU,
          (!io.fpu_stall && isOneOf(s.data.uop, fpu_uop)) | (!io.fpu_b2b_stall && !isOneOf(s.data.uop, fpu_uop)),
          true.B))
    })))
    age_matrix_valid_vec(w) := (entries_valid_vec(w) & ~grant_entries) | replay_entries
  }
}