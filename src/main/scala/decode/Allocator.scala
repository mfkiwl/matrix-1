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
package matrix.decode

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import matrix.common._
import matrix.utils._

class AllocatorReq(implicit p: Parameters) extends MatrixBundle {
  val valid   = Bool()
  val is_load  = Bool()
  val is_store = Bool()
}

class AllocatorResp(implicit p: Parameters) extends MatrixBundle {
  val rob_id  = UInt(robIdWidth.W)
  val ld_id   = UInt(ldqIdWidth.W)
  val st_id   = UInt(stqIdWidth.W)
}

class AllocatorRetire(implicit p: Parameters) extends AllocatorReq
class AllocatorIO(implicit p: Parameters) extends MatrixBundle {
  val kill = Input(Bool())
  val stall = Input(Bool())
  val req = Flipped(Valid(Vec(decodeWidth, new AllocatorReq)))
  val resp = Output(Vec(decodeWidth, new AllocatorResp))
  val roll_back = Flipped(Valid(new AllocatorResp))
  val retire = Flipped(Valid(Vec(retireWidth, new AllocatorRetire)))
  val st_write_done = Input(Bool())

  val rob_head = Output(UInt(robIdWidth.W))
  val rob_tail = Output(UInt(robIdWidth.W))
  val ld_head = Output(UInt(ldqIdWidth.W))
  val ld_tail = Output(UInt(ldqIdWidth.W))
  val st_head = Output(UInt(stqIdWidth.W))
  val st_tail = Output(UInt(stqIdWidth.W))
  val st_ret  = Output(UInt(stqIdWidth.W))
  val empty   = Output(Bool())
}

class Allocator(implicit p: Parameters) extends MatrixModule {
  val io = IO(new AllocatorIO)
  
  val allocateVec = Reverse(Cat(io.req.bits.map(_.valid)))
  val kill = WireInit(io.kill)
  val stall = WireInit(io.stall)
  val roll_back = WireInit(io.roll_back.valid)
  val retire = WireInit(io.retire.valid)
  
  //  Allocate rob id
  val rob_head = Reg(UInt(robIdWidth.W))
  val rob_tail = Reg(UInt(robIdWidth.W))
  io.rob_head := rob_head
  io.rob_tail := rob_tail
  val rob_id = Wire(Vec(decodeWidth, UInt(robIdWidth.W)))
  rob_id(0) := rob_tail
  for (w <- 1 until decodeWidth) {
    rob_id(w) := rob_tail + PopCount(allocateVec(w,0))
  }
  val robEmpty = PopCount(allocateVec) > (numOfRobEntries.U - (rob_tail - rob_head))
  when (kill) {
    rob_head := 0.U
  } .elsewhen (retire) {
    rob_head := rob_head + PopCount(Cat(io.retire.bits.map(_.valid)))
  }
  when (kill) {
    rob_tail := 0.U
  } .elsewhen (roll_back) {
    rob_tail := io.roll_back.bits.rob_id
  } .elsewhen (io.req.valid && !io.empty && !stall) {
    rob_tail := rob_tail + PopCount(allocateVec)
  }
  //  Allocate load id
  val ld_head = Reg(UInt(ldqIdWidth.W))
  val ld_tail = Reg(UInt(ldqIdWidth.W))
  io.ld_head := ld_head
  io.ld_tail := ld_tail
  val ldAllocateVec = Reverse(Cat(io.req.bits map { v => v.valid && v.is_load }))
  val ld_id = Wire(Vec(decodeWidth, UInt(ldqIdWidth.W)))
  ld_id(0) := ld_tail
  for (w <- 1 until decodeWidth) {
    ld_id(w) := ld_tail + PopCount(ldAllocateVec(w, 0))
  }
  val ldEmpty = PopCount(ldAllocateVec) > (numOfLdqEntries.U - (ld_tail - ld_head))
  when (kill) {
    ld_head := 0.U
  } .elsewhen (retire) {
    ld_head := ld_head + PopCount(io.retire.bits.map(r => r.valid && r.is_load))
  }
  when (kill) {
    ld_tail := 0.U
  } .elsewhen (roll_back) {
    ld_tail := io.roll_back.bits.ld_id
  } .elsewhen (io.req.valid && !io.empty && !stall && ldAllocateVec.orR) {
    ld_tail := ld_tail + PopCount(ldAllocateVec)
  }
  //  Allocate store id
  val st_head = Reg(UInt(stqIdWidth.W))
  val st_tail = Reg(UInt(stqIdWidth.W))
  val st_ret  = Reg(UInt(stqIdWidth.W))
  io.st_head := st_head
  io.st_tail := st_tail
  io.st_ret  := st_ret
  val stAllocateVec = Reverse(Cat(io.req.bits map { v => v.valid && v.is_store }))
  val st_id = Wire(Vec(decodeWidth, UInt(stqIdWidth.W)))
  st_id(0) := st_tail
  for (w <- 1 until decodeWidth) {
    st_id(w) := st_tail + PopCount(stAllocateVec(w,0))
  }
  val stqEmpty = PopCount(stAllocateVec) > (numOfStqEntries.U - (st_tail - st_head))
  when (kill) {
    st_head := 0.U
  } .elsewhen (retire) {
    st_head := st_head + PopCount(io.retire.bits.map(r => r.valid && r.is_store))
  }
  when (kill) {
    st_tail := 0.U
  } .elsewhen (roll_back) {
    st_tail := io.roll_back.bits.st_id
  } .elsewhen (io.req.valid && !io.empty && !stall && stAllocateVec.orR) {
    st_tail := st_tail + PopCount(stAllocateVec)
  }
  when (kill) {
    st_ret := 0.U
  } .elsewhen (io.st_write_done) {
    st_ret := st_ret + 1.U
  }
  //
  io.empty := robEmpty | ldEmpty | stqEmpty
  val resp = Wire(Vec(decodeWidth, new AllocatorResp))
  for (w <- 0 until decodeWidth) {
    resp(w).rob_id := rob_id(w)
    resp(w).ld_id := ld_id(w)
    resp(w).st_id := st_id(w)
  }
  io.resp := resp
}
