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
import matrix.common._
import matrix.utils._

case class RASParams (
                       numOfEntries: Int = 32
                     )

class RASMeta(implicit p: Parameters) extends MatrixBundle {
  val valid = Bool()
  val addr  = UInt(vaddrWidth.W)
}

class RASIO(implicit p: Parameters) extends MatrixBundle {
  val req = Input(Bool())
  val push = Input(Bool())
  val push_addr = Input(UInt(vaddrWidth.W))
  val pop = Input(Bool())
  val pop_addr = Output(UInt(vaddrWidth.W))
}

class RAS(implicit p: Parameters) extends MatrixModule {
  val io = IO(new RASIO)

  //=============================================
  //  Name: RAS stack
  //  Desc:
  //  Size: bpdParams.ras.numEntries
  //=============================================
  val ras = Reg(Vec(bpdParams.ras.numOfEntries, UInt(vaddrWidth.W)))

  val top = Reg(UInt((log2Ceil(bpdParams.ras.numOfEntries) + 1).W))

  val pop_then_push = io.pop && io.push
  val where = hashIndex(top)
  when (io.req) {
    when (pop_then_push) {
      top := top
    } .elsewhen (io.pop) {
      top := top - 1.U
    } .elsewhen (io.push) {
      top := top + 1.U
    }
    when (io.push) {
      ras(where) := io.push_addr
    }
  } .otherwise {
    top := top
  }

  val peek = hashIndex(top - 1.U)
  io.pop_addr := ras(peek)
}