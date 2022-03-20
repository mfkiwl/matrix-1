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
package matrix.csr

import chisel3._
import freechips.rocketchip.config.Parameters
import matrix.common._

object PMP {
  def lgAlign = 10
}

class PMPConfig(implicit p: Parameters) extends MatrixBundle {
  val l = Bool()
  val zero = UInt(2.W)
  val a = UInt(2.W)
  val x = Bool()
  val w = Bool()
  val r = Bool()

  def off = a === 0.U

  def tor = a === 1.U

  def na4 = a === 2.U

  def napot = a === 3.U

  def locked = l
}

class PMPAddr(implicit p: Parameters) extends MatrixBundle {
  val addr = UInt((vaddrWidth - PMP.lgAlign).W)
}

class PMPReg(implicit p: Parameters) extends MatrixBundle {
  val pmpcfg = Vec(numOfPMPConfigs, new PMPConfig)
  val pmpaddr = Vec(numOfPMPConfigs, new PMPAddr)
}

class PMPChecker(implicit p: Parameters) extends MatrixModule {
  val io = IO(new Bundle() {
    val pmp = Input(Vec(numOfPMPs, new PMPReg))
    val prv = Input(UInt(PRV.SZ.W))
    val addr = Input(UInt(vaddrWidth.W))
    val size = Input(UInt())

    val r = Output(Bool())
    val w = Output(Bool())
    val x = Output(Bool())
  })

  val matchConfig = Wire(new PMPConfig)
  val pmpValid = io.prv > PRV.S

  //

  io.r := matchConfig.r
  io.w := matchConfig.w
  io.x := matchConfig.x
}

