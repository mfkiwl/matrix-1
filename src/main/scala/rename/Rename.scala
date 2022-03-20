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
package matrix.rename

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import matrix.decode._
import matrix.common._
import matrix.ifu._
import matrix.decode._
import matrix.issue._
import matrix.utils._

class RenameResp(implicit p: Parameters) extends MatrixBundle {
  val valid     = Bool()
  val rob_id    = UInt(robIdWidth.W)
  val rsv_id    = UInt(rsvIdWidth.W)
  val micro_op  = new MicroOp
  val pc        = UInt(vaddrWidth.W)
  val pred_info = new PredictorResp 
  val rs1_map   = new MapTableMeta
  val rs2_map   = new MapTableMeta
  val rs3_map   = new MapTableMeta
}

class RenameRetireMeta(implicit p: Parameters) extends MatrixBundle 
  with ScalarOpConstants {
  val valid = Bool()
  val rtype = UInt(RT_SZ.W)
  val rd    = UInt(lregSz.W)
  val map   = new MapTableMeta 
}

class RenameRetire(implicit p: Parameters) extends MatrixBundle {
  val retire  = Bool()
  val meta    = Vec(retireWidth, new RenameRetireMeta)
}

class RenameIO(implicit p: Parameters) extends MatrixBundle {
  val kill  = Input(Bool())
  val stall = Input(Bool())
  val req = Flipped(Valid(Vec(decodeWidth, new DispatchResp)))
  val resp = Valid(Vec(decodeWidth, new RenameResp))
  val retire = Flipped(Valid(new RenameRetire))
  val rsv_fire = Flipped(Valid(Vec(issueWidth, new IssueCIQGrant)))
  val empty = Output(Bool())
}

class Rename(implicit p: Parameters) extends MatrixModule 
 with ScalarOpConstants {
  val io = IO(new RenameIO)

  val stall = io.stall | io.empty
  val kill = WireInit(io.kill)
  val roll_back = io.retire.valid && !io.retire.bits.retire
  val retire = io.retire.valid && io.retire.bits.retire

  //  Allocator
  val allocate_vec = Reverse(Cat(io.req.bits.map(_.valid)))
  val rsv_freelist = RegInit(UInt(numOfRsvEntries.W), ~0.U(numOfRsvEntries.W))
  val rsv_ld_selected_oh = selectFirstN(rsv_freelist, decodeWidth)
  val select_mask = io.req.bits zip rsv_ld_selected_oh map { case (r,s) =>
    Fill(numOfRsvEntries, r.valid && io.req.valid && !stall) & s
  } reduce(_|_)
  val fire_mask = io.rsv_fire.bits.map(r => {
    Fill(numOfRsvEntries, r.valid && io.rsv_fire.valid) & UIntToOH(r.rsv_id)
  }) reduce(_|_)
  val rsv_empty = PopCount(allocate_vec) > (numOfRsvEntries.U - PopCount(rsv_freelist))
  when (kill) {
    rsv_freelist := ~0.U(numOfRsvEntries.W)
  } .otherwise {
    rsv_freelist := rsv_freelist & ~select_mask  | fire_mask
  }
  //  Maptable
  val int_maptable = Module(new MapTable)
  val fp_maptable = Module(new MapTable)
  val mapTables = Seq(int_maptable, fp_maptable)
  for ((t, i) <- mapTables zipWithIndex) {
    t.io.stall := stall
    t.io.kill := kill
    t.io.req.valid := io.req.valid
    for (w <- 0 until decodeWidth) {
      val fpOp = Mux(i.U === 0.U, io.req.bits(w).micro_op.rd_type === RT_INT, io.req.bits(w).micro_op.rd_type === RT_FPU)
      t.io.req.bits(w).valid := io.req.bits(w).valid
      t.io.req.bits(w).rs1   := io.req.bits(w).micro_op.rs1
      t.io.req.bits(w).rs2   := io.req.bits(w).micro_op.rs2
      t.io.req.bits(w).rs3   := io.req.bits(w).micro_op.rs3
      t.io.req.bits(w).rd_val := io.req.bits(w).micro_op.rd_val && fpOp
      t.io.req.bits(w).rd     := io.req.bits(w).micro_op.rd
      t.io.req.bits(w).pointer:= io.req.bits(w).rob_id
    }
    //  Retire && Rollback
    t.io.retire.valid := io.retire.valid
    t.io.retire.bits.retire := io.retire.bits.retire
    for (w <- 0 until decodeWidth) {
      val fpOp = Mux(i.U === 0.U, io.retire.bits.meta(w).rtype === RT_INT, io.retire.bits.meta(w).rtype === RT_FPU)
      t.io.retire.bits.meta(w).valid := io.retire.bits.meta(w).valid && fpOp
      t.io.retire.bits.meta(w).busy  := io.retire.bits.meta(w).map.busy
      t.io.retire.bits.meta(w).pointer := io.retire.bits.meta(w).map.pointer

    }
  }

  val resp = Reg(Vec(decodeWidth, new RenameResp))
  for (w <- 0 until decodeWidth) {
    when (kill) {
      resp(w).valid := false.B
    } .elsewhen (!stall) {
      resp(w).valid   := io.req.valid
      resp(w).rob_id   := io.req.bits(w).rob_id
      resp(w).rsv_id   := OHToUInt(rsv_ld_selected_oh(w))
      resp(w).micro_op := io.req.bits(w).micro_op
      resp(w).pc      := io.req.bits(w).pc
      resp(w).pred_info:= io.req.bits(w).pred_info
      resp(w).rs1_map  := Mux(io.req.bits(w).micro_op.rs1_type === RT_INT,
        mapTables(0).io.resp(w).rs1_map, mapTables(1).io.resp(w).rs1_map)
      resp(w).rs2_map  := Mux(io.req.bits(w).micro_op.rs2_type === RT_INT,
        mapTables(0).io.resp(w).rs2_map, mapTables(1).io.resp(w).rs2_map)
      resp(w).rs3_map  := Mux(io.req.bits(w).micro_op.rs3_type === RT_INT,
        mapTables(0).io.resp(w).rs3_map, mapTables(1).io.resp(w).rs3_map)
    }
  }
  io.resp.valid := RegEnable(io.req.valid && !kill, !stall | kill)
  io.resp.bits  := resp
}