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
package matrix.exu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import matrix.common._
import matrix.utils._

object FPToFP {
  def apply(singleToDouble: Bool, fp: FP) = {
  }
}

class FPToFP(
              inExpWidth: Int,
              inMantWidth: Int,
              outExpWidth: Int,
              outMantWidth: Int
            )(implicit p: Parameters) extends MatrixModule {

}

object FPToInt {
  def apply(source: UInt) = {
    val singleToDouble = Input(Bool())

  }
  def apply(source: SInt) = {

  }
}

object IntToFP {
  def apply(source: UInt) = {

  }
  def apply(source: SInt) = {

  }
}

class IntToFP (
                inWidth: Int,
                expWidth: Int,
                mantWith: Int
              )(implicit p: Parameters) extends MatrixModule {
  val io = IO(new Bundle() {
    val in = Input(UInt(inWidth.W))
    val roundingMode = Input(UInt(3.W))
    val out = Output(new FP(expWidth, mantWith))
  })

}