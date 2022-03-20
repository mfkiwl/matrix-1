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
package matrix.vmm

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import matrix.common._
import matrix.utils._
import matrix.ifu._

class MSHRHeaderSlot(implicit p: Parameters) extends MatrixBundle {
  def tagSz = vaddrWidth - log2Ceil(dcacheParams.lineBytes)

  val valid   = Bool()
  val rob_id  = UInt(robIdWidth.W)
  val tag     = UInt(tagSz.W)
  val issued  = Bool()

  def create(new_tag: UInt, rob_id: UInt) = {
    valid := true.B
    tag   := new_tag
    this.rob_id := rob_id
    issued := false.B
  }
  def delete(dummy: Boolean = false) = {
    valid := false.B
  }
  def issue(dummy: Boolean = false) = {
    issued := true.B
  }
  def lookup(tag: UInt) = valid && tag === this.tag
  def kill(rob_id: UInt) = {
    when (checkOld(rob_id, this.rob_id)) {
      valid := false.B
    }
  }
}

class MSHRTableSlot(numOfEntires: Int, ageSize: Int = 3)(implicit p: Parameters) extends MatrixBundle
  with ScalarOpConstants
  with MemoryOpConstants {
  val valid     = Bool()
  val entry     = UInt(log2Ceil(numOfEntires).W)
  val rob_id    = UInt(robIdWidth.W)
  val ld_id     = UInt(ldqIdWidth.W)
  val age       = UInt(ageSize.W)

  def create(entry: UInt, rob_id: UInt, ld_id: UInt, age: UInt) = {
    valid := true.B
    this.entry := entry
    this.rob_id := rob_id
    this.ld_id := ld_id
    this.age := age
  }
  def delete(entry: UInt) = {
    when (entry === this.entry) {
      valid := false.B
    }
  }
  def kill(rob_id: UInt) = {
    when (checkOld(rob_id, this.rob_id)) {
      valid := false.B
    }
  }
}

class MSHRReq(implicit p: Parameters) extends MatrixBundle
  with ScalarOpConstants
  with MemoryOpConstants {
  val rob_id = UInt(robIdWidth.W)
  val ld_id  = UInt(ldqIdWidth.W)
  val addr   = UInt((paddrWidth - log2Ceil(dcacheParams.lineBytes)).W)
}

class MSHRPTWReq(implicit p: Parameters) extends PTWReq
class MSHRPTWResp(implicit p: Parameters) extends MatrixBundle {
  val done = Input(Bool())
}

class MSHRPTW(implicit p: Parameters) extends MatrixBundle {
  val req = Decoupled(new MSHRPTWReq)
  val resp = new MSHRPTWResp
}

class MSHRIO(implicit p: Parameters) extends MatrixBundle {
  val kill = Input(new KillReq)
  val req = Flipped(Valid(new MSHRReq))
  val ld_id = Output(UInt(ldqIdWidth.W))
  val ptw = new MSHRPTW
  val full = Output(Bool())
}

class MSHR(numOfHeaderEntries: Int = 8, numOfTableEntries: Int = 16)(implicit p: Parameters) extends MatrixModule
  with MemoryOpConstants {
  val io = IO(new MSHRIO)

  def ageSz = log2Ceil(numOfHeaderEntries) + 1
  val header = Reg(Vec(numOfHeaderEntries, new MSHRHeaderSlot))
  val table = Reg(Vec(numOfTableEntries, new MSHRTableSlot(numOfHeaderEntries, ageSz)))

  //============================== Enter =================================//
  val header_selected_oh = selectFirstN(Reverse(Cat(header.map(!_.valid))))
  val header_selected = OHToUInt(header_selected_oh)
  val table_selected_oh = selectFirstN(Reverse(Cat(table.map(!_.valid))))
  val table_selected = OHToUInt(table_selected_oh)

  val tag = io.req.bits.addr(vaddrWidth - 1, log2Ceil(dcacheParams.lineBytes))
  val offset = io.req.bits.addr(log2Ceil(dcacheParams.lineBytes)-1, 0)
  val age = RegInit(UInt(ageSz.W), 0.U)

  val kill = io.kill.valid && checkOld(io.kill.rob_id, io.req.bits.rob_id)
  when (io.req.valid && !kill) {
    val hit = header.map(_.lookup(tag)).reduce(_|_)
    when (!hit) {
      header(header_selected).create(tag, io.req.bits.rob_id)
    }
    table(table_selected).create(header_selected,
      io.req.bits.rob_id,
      io.req.bits.ld_id,
      age)
    age := age + 1.U
  }
  //============================== Issue =================================//
  val issue_slot = table.foldLeft(table.head)((prev, cur) => {
    val prev_val = prev.valid && !header(prev.entry).issued
    val cur_val = cur.valid && !header(cur.entry).issued
    Mux(prev_val && cur_val,
      Mux(checkOld(prev.age, cur.age), prev, cur),
      Mux(prev_val, prev, cur))
  })
  val header_issue_selected = header(issue_slot.entry)
  val which_slot = Reg(UInt(log2Ceil(numOfHeaderEntries).W))
  when (io.ptw.req.fire()) {
    which_slot := issue_slot.entry
    header_issue_selected.issue()
  }
  io.ptw.req.valid      := header_issue_selected.valid && !header_issue_selected.issued
  io.ptw.req.bits.addr  := header_issue_selected.tag
  io.ptw.req.bits.cmd   := M_XRD
  io.ld_id             := issue_slot.ld_id

  when (io.ptw.resp.done) {
    header(which_slot).delete()
    table.map(_.delete(which_slot))
  }

  io.full := header.map(_.valid).reduce(_&_) | table.map(_.valid).reduce(_&_)
}

