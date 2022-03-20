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
import matrix.ifu._
import matrix.rename._
import matrix.common._

class IssueReadResp(implicit p: Parameters) extends MatrixBundle {
  val valid       = Bool()
  val rob_id      = UInt(robIdWidth.W)
  val rsv_id      = UInt(rsvIdWidth.W)
  val micro_op    = new MicroOp
  val pred_info   = new PredictorResp
  val pc          = UInt(vaddrWidth.W)
  val rs1_busy    = Bool()
  val rs1         = UInt(XLEN.W)
  val rs2_busy    = Bool()
  val rs2         = UInt(XLEN.W)
  val rs3_busy    = Bool()
  val rs3         = UInt(XLEN.W)
}

class IssueReadPort(implicit p: Parameters) extends MatrixBundle {
  val pointer = Output(UInt(robIdWidth.W))
  val data    = Input(UInt(XLEN.W))
}

class IssueReadIO(implicit p: Parameters) extends MatrixBundle {
  val kill = Input(Bool())
  val stall = Input(Bool())
  val req = Flipped(Valid(Vec(decodeWidth, new RenameResp)))
  val resp = Valid(Vec(decodeWidth, new IssueReadResp))
  val int_read_from_reg = Flipped(Vec(decodeWidth*3, new RegFilesReadPortIO))
  val fp_read_from_reg = Flipped(Vec(decodeWidth*3, new RegFilesReadPortIO))
  val read_from_rob = Vec(decodeWidth*3, new IssueReadPort)
}

class IssueRead(implicit p: Parameters) extends MatrixModule
  with ScalarOpConstants {
  val io = IO(new IssueReadIO)
  
  val kill = WireInit(io.kill)
  val stall = WireInit(io.stall)
  //  Read from regfiles
  val rs1_read_reg = io.req.bits.map(_.micro_op.rs1)
  val rs2_read_reg = io.req.bits.map(_.micro_op.rs2)
  val rs3_read_reg = io.req.bits.map(_.micro_op.rs3)
  val read_from_reg = Seq(rs1_read_reg, rs2_read_reg, rs3_read_reg).flatten
  io.int_read_from_reg zip read_from_reg map { case (port, r) => port.addr := r }
  io.fp_read_from_reg zip read_from_reg map { case (port, r) => port.addr := r }
  //  Read from rob
  val rs1_read_from_rob = io.req.bits.map(_.rs1_map.pointer)
  val rs2_read_from_rob = io.req.bits.map(_.rs2_map.pointer)
  val rs3_read_from_rob = io.req.bits.map(_.rs3_map.pointer)
  val read_from_rob = Seq(rs1_read_from_rob, rs2_read_from_rob, rs3_read_from_rob).flatten
  io.read_from_rob zip read_from_rob map { case (port, r) => port.pointer := r }
  //
  val rs1_in_rob = Wire(Vec(decodeWidth, Bool()))
  val rs2_in_rob = Wire(Vec(decodeWidth, Bool()))
  val rs3_in_rob = Wire(Vec(decodeWidth, Bool()))
  rs1_in_rob(0) := io.req.bits(0).rs1_map.busy
  rs2_in_rob(0) := io.req.bits(0).rs2_map.busy
  rs3_in_rob(0) := io.req.bits(0).rs3_map.busy
  for (w <- 1 until decodeWidth) {
    var rs1_hit = false.B
    for (i <- 0 until w) {
      rs1_hit = rs1_hit | ((io.req.bits(i).micro_op.rd === io.req.bits(w).micro_op.rs1)
        && io.req.bits(i).micro_op.rd_type === io.req.bits(w).micro_op.rs1_type)
    }
    rs1_in_rob(w) := Mux(rs1_hit, true.B, io.req.bits(w).rs1_map.busy)
    var rs2_hit = false.B
    for (i <- 0 until w) {
      rs2_hit = rs2_hit | ((io.req.bits(i).micro_op.rd === io.req.bits(w).micro_op.rs2)
        && io.req.bits(i).micro_op.rd_type === io.req.bits(w).micro_op.rs2_type)
    }
    rs2_in_rob(w) := Mux(rs2_hit, true.B, io.req.bits(w).rs2_map.busy)
    var rs3_hit = false.B
    for (i <- 0 until w) {
      rs3_hit = rs3_hit | ((io.req.bits(i).micro_op.rd === io.req.bits(w).micro_op.rs3)
        && io.req.bits(i).micro_op.rd_type === io.req.bits(w).micro_op.rs3_type)
    }
    rs3_in_rob(w) := Mux(rs3_hit, true.B, io.req.bits(w).rs3_map.busy)
  }
  val resp = Reg(Valid(Vec(decodeWidth, new IssueReadResp)))
  val rs1_type = io.req.bits.map(_.micro_op.rs1_type)
  val rs2_type = io.req.bits.map(_.micro_op.rs2_type)
  val rs3_type = io.req.bits.map(_.micro_op.rs3_type)
  when (kill) {
    resp.valid := false.B
    resp.bits.foreach(_.valid := false.B)
  } .elsewhen (io.req.valid && !stall) {
    resp.valid := true.B
    for (w <- 0 until decodeWidth) {
      resp.bits(w).valid      := io.req.bits(w).valid
      resp.bits(w).rob_id     := io.req.bits(w).rob_id
      resp.bits(w).rsv_id     := io.req.bits(w).rsv_id
      resp.bits(w).micro_op   := io.req.bits(w).micro_op
      resp.bits(w).pred_info  := io.req.bits(w).pc
      resp.bits(w).pc         := io.req.bits(w).pc
    }
    val rs_select = Seq(rs1_in_rob, rs2_in_rob, rs3_in_rob).flatten
    val rs_type = Seq(rs1_type, rs2_type, rs3_type).flatten
    val rs_resp = ((io.read_from_rob zip io.int_read_from_reg) zip io.fp_read_from_reg zipWithIndex) map { case (((rob, ireg), freg), w) =>
      Mux(rs_select(w), rob.data, Mux(rs_type(w) === RT_INT, ireg.data, freg.data))
    }
    val rs_resp_group = rs_resp.grouped(decodeWidth).toSeq.flatten
    resp.bits.zipWithIndex map { case (p, w) => {
      p.rs1_busy  := rs_select(w)
      p.rs1       := rs_resp_group(w)
      p.rs2_busy  := rs_select(w + decodeWidth)
      p.rs2       := rs_resp_group(w + decodeWidth)
      p.rs3_busy  := rs_select(w + 2 * decodeWidth)
      p.rs3       := rs_resp_group(w + 2 * decodeWidth)
    }}
  }
  io.resp := resp
}
