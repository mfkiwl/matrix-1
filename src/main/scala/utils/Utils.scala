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
package matrix.utils

import chisel3._
import chisel3.util._
import matrix.common._

object selectFirstN {
  def apply(in: UInt, n: Int): Vec[UInt] = {
    val sels = Wire(Vec(n, UInt(in.getWidth.W)))
    var mask = in

    for (i <- 0 until n) {
      sels(i) := PriorityEncoderOH(mask)
      mask = mask & (~sels(i))
    }
    sels
  }
  def apply(in: UInt): UInt = apply(in, 1)(0)
}

object updateCounter {
  def apply(cnt: UInt, mode: Bool, nBits: Int): UInt = {
    Mux(mode,
      Mux(cnt < ((1 << (nBits - 1)) - 1).U, cnt + 1.U, cnt),
      Mux(cnt =/= 0.U, cnt - 1.U, cnt))
  }
  def apply(cnt: SInt, mode: Bool, nBits: Int): SInt = {
    Mux(mode,
      Mux(cnt < ((1 << (nBits - 1)) - 1).S, cnt + 1.S, cnt),
      Mux(cnt > ~(1 << (nBits - 1)).S, cnt - 1.S, cnt))
  }
}

object checkOld {
  def apply(a: UInt, b: UInt) = {
    assert(a.getWidth == b.getWidth)
    val n = a.getWidth
    Mux(a(n - 1) ^ b(n - 1), a(n - 2, 0) >= b(n - 2, 0), a(n - 2) < b(n - 2, 0))
  }
}

object isOneOf {
  def apply(x: UInt, sel: Seq[UInt]): Bool = {
    sel.map(_ === x).reduce(_|_)
  }
}

object genImm extends ScalarOpConstants {
  def apply(ip: UInt, isel: UInt): SInt = {
    val sign = ip(LONGEST_IMM_SZ-1).asSInt
    val i30_20 = Mux(isel === IS_U, ip(18,8).asSInt, sign)
    val i19_12 = Mux(isel === IS_U || isel === IS_J, ip(7,0).asSInt, sign)
    val i11    = Mux(isel === IS_U, 0.S,
      Mux(isel === IS_J || isel === IS_B, ip(8).asSInt, sign))
    val i10_5  = Mux(isel === IS_U, 0.S, ip(18,14).asSInt)
    val i4_1   = Mux(isel === IS_U, 0.S, ip(13,9).asSInt)
    val i0     = Mux(isel === IS_S || isel === IS_I, ip(8).asSInt, 0.S)

    Cat(sign, i30_20, i19_12, i11, i10_5, i4_1, i0).asSInt
  }
}

object hashIndex {
  def apply(id: UInt): UInt = {
    val n = id.getWidth
    id(n - 2, 0)
  }
}

object signedExtend {
  def apply(x: UInt, n: Int): UInt = {
    val w = x.getWidth
    assert(n > w)
    Cat(Fill(n - w, x(w - 1)), x)
  }
}

object zerosExtend {
  def apply(x: UInt, n: Int): UInt = {
    val w = x.getWidth
    assert(n > w)
    Cat(Fill(n-w, 0.U), x)
  }
}