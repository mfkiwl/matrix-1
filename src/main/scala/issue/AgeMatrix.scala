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
import matrix.utils._

class IssueAgeMatrixReq(implicit p: Parameters) extends MatrixBundle {
  val valid = Bool()
  val rsv_id = UInt(rsvIdWidth.W)
}

class IssueAgeMatrixIO(implicit p: Parameters) extends MatrixBundle {
  val flush = Input(Bool())
  val kill  = Flipped(Valid(UInt(numOfRsvEntries.W)))
  val req = Flipped(Valid(Vec(decodeWidth, new IssueAgeMatrixReq)))
  val valids_vec = Input(UInt(numOfRsvEntries.W))
  val oldest = Output(UInt(numOfRsvEntries.W))
}

class IssueAgeMatrix(implicit p: Parameters) extends MatrixModule {
  val io = IO(new IssueAgeMatrixIO)
  
  val flush = WireInit(io.flush)
  val kill = WireInit(io.kill)
  val kill_rows = selectFirstN(io.kill.bits)
  //
  val age_matrix = Reg(Vec(numOfRsvEntries, UInt(numOfRsvEntries.W)))

  //  Update
  when (flush) {
    for (e <- 0 until numOfRsvEntries) {
      age_matrix(e) := 0.U
    }
  } .elsewhen (kill.valid) {
    for (e <- 0 until numOfRsvEntries) {
      when (kill.bits(e)) {
        age_matrix(e) := 0.U
      }
    }
  }  .elsewhen (io.req.valid) {
    for (w <- 0 until decodeWidth) {
      var mask = io.valids_vec
      for (i <- 0 until w) {
        mask = mask | (UIntToOH(io.req.bits(i).rsv_id) & Fill(numOfRsvEntries, io.req.bits(i).valid))
      }
      when (io.req.bits(w).valid) {
        age_matrix(io.req.bits(w).rsv_id) := mask
      }
    }
  }
  def foldVec(vec: UInt, mask: UInt, pos: UInt) = {
    val pos_mask = ~UIntToOH(pos, width = numOfRsvEntries)
    val vec_res = vec & pos_mask & mask
    vec_res.orR
  }
  //  Calculate
  val fold = Wire(Vec(numOfRsvEntries, Bool()))
  for (e <- 0 until numOfRsvEntries) {
    fold(e) := io.valids_vec(e) & !foldVec(age_matrix(e), io.valids_vec, e.U)
  }
  io.oldest := Reverse(Cat(fold))
}