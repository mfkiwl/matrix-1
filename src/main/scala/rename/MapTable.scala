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
import matrix.common._

class MapTableMeta(implicit p: Parameters) extends MatrixBundle {
  val busy = Bool()
  val pointer = UInt(robIdWidth.W)
}

class MapTableReq(implicit p: Parameters) extends MatrixBundle {
  val valid   = Bool()
  val rs1     = UInt(lregSz.W)
  val rs2     = UInt(lregSz.W)
  val rs3     = UInt(lregSz.W)
  val rd_val  = Bool()
  val rd      = UInt(lregSz.W)
  val pointer = UInt(robIdWidth.W)
}

class MapTableResp(implicit p: Parameters) extends MatrixBundle {
  val rs1_map = new MapTableMeta
  val rs2_map = new MapTableMeta
  val rs3_map = new MapTableMeta
}

class MapTableRetireMeta(implicit p: Parameters) extends MatrixBundle
  with ScalarOpConstants {
  val valid   = Bool()
  val rtype   = UInt(RT_SZ.W)
  val rd      = UInt(lregSz.W)
  val busy    = Bool()
  val pointer = UInt(robIdWidth.W)
}

class MapTableRetire(implicit p: Parameters) extends MatrixBundle {
  val retire = Bool()
  val meta   = Vec(retireWidth, new MapTableRetireMeta)
}

class MapTableIO(implicit p: Parameters) extends MatrixBundle {
  val stall = Input(Bool())
  val kill  = Input(Bool())
  val req   = Flipped(Valid(Vec(decodeWidth, new MapTableReq)))
  val resp  = Output(Vec(decodeWidth, new MapTableResp))
  val retire = Flipped(Valid(new MapTableRetire))
}


class MapTable(implicit p: Parameters) extends MatrixModule {
  val io = IO(new MapTableIO)

  val stall = WireInit(io.stall)
  val retire = io.retire.valid && io.retire.bits.retire
  val roll_back = io.retire.valid && !io.retire.bits.retire
  val kill = WireInit(io.kill)

  val maptable = Reg(Vec(numOfLRegs, new MapTableMeta))

  def lookupMap(rs: UInt) = {
    val reg = maptable(rs)
    val hit = io.retire.bits.meta.map(r => reg.busy && reg.pointer === r.pointer).reduce(_|_)
    val res = Wire(new MapTableMeta)
    when (retire && hit) {
      res := 0.U.asTypeOf(new MapTableMeta)
    } .otherwise {
      res := reg
    }
    res
  }
  val resp = Wire(Vec(decodeWidth, new MapTableResp))
  for (w <- 0 until decodeWidth) {
    resp(w).rs1_map := lookupMap(io.req.bits(w).rs1)
    resp(w).rs2_map := lookupMap(io.req.bits(w).rs2)
    resp(w).rs3_map := lookupMap(io.req.bits(w).rs3)
  }
  io.resp := resp
  //  Update
  when (kill) {
    maptable.foreach(_.busy := false.B)
  } .elsewhen (roll_back) {
    for (w <- 0 until retireWidth) {
      val reg = maptable(io.retire.bits.meta(w).rd)
      when (io.retire.bits.meta(w).valid) {
        val hit = reg.busy && reg.pointer === io.retire.bits.meta(w).pointer
        when (hit) {
          maptable(io.retire.bits.meta(w).rd).busy := io.retire.bits.meta(w).busy
        }
      }
    }
  } .elsewhen (retire) {
    for (w <- 0 until retireWidth) {
      val reg = maptable(io.retire.bits.meta(w).rd)
      when (io.retire.bits.meta(w).valid) {
        val hit = reg.busy && reg.pointer === io.retire.bits.meta(w).pointer
        when (hit) {
          maptable(io.retire.bits.meta(w).rd).busy := false.B
        }
      }
    }
  } .elsewhen (io.req.valid && !stall) {
    for (w <- 0 until decodeWidth) {
      when (io.req.bits(w).valid && io.req.bits(w).rd_val) {
        maptable(io.req.bits(w).rd).busy := true.B
        maptable(io.req.bits(w).rd).pointer := io.req.bits(w).pointer
      }
    }
  }
}