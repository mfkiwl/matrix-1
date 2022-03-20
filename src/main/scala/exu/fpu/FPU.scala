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
import chisel3.internal.firrtl.Width
import freechips.rocketchip.config.Parameters
import matrix.common._
import matrix.utils._

object FPUConstants {
  def RM_SZ  = 3
  def RM_RNE = 0.U(RM_SZ.W)
  def RM_RTZ = 1.U(RM_SZ.W)
  def RM_RDN = 2.U(RM_SZ.W)
  def RM_RUP = 3.U(RM_SZ.W)
  def RM_RMM = 4.U(RM_SZ.W)
  def RM_DYN = 7.U(RM_SZ.W)

  def FMT_SZ = 2
  def FMT_S  = 0.U(FMT_SZ.W)
  def FMT_D  = 1.U(FMT_SZ.W)
  def FMT_H  = 2.U(FMT_SZ.W)
  def FMT_Q  = 3.U(FMT_SZ.W)

  def CMD_SZ      = 6
  def CMD_MAX     = 1.U(CMD_SZ.W)
  def CMD_MIN     = 2.U(CMD_SZ.W)
  def CMD_EQ      = 3.U(CMD_SZ.W)
  def CMD_LT      = 4.U(CMD_SZ.W)
  def CMD_LE      = 5.U(CMD_SZ.W)
  def CMD_CLASS   = 6.U(CMD_SZ.W)
  def CMD_ADD     = 7.U(CMD_SZ.W)
  def CMD_SUB     = 8.U(CMD_SZ.W)
  def CMD_SGNJ    = 9.U(CMD_SZ.W)
  def CMD_SGNJN   = 10.U(CMD_SZ.W)
  def CMD_SGNJX   = 11.U(CMD_SZ.W)
}

sealed class FP(val expWidth: Int, val mantWidth: Int) extends Bundle {
  val sign        = Bool()
  val exp         = UInt(expWidth.W)
  val mantissa    = UInt(mantWidth.W)
  val isZero      = Bool()
  val isSubnormal = Bool()
  val isInfinite  = Bool()
  val isNaN       = Bool()
  val isOrdinary  = Bool()
  val isOverflow  = Bool()
  val isUnderflow = Bool()

  def BIAS                  = (1 << (expWidth - 1)) - 1
  def EXP_MAX               = (1 << expWidth) - 1
  def MIN_REAL_EXP          = BIAS - 1
  def MAX_REAL_EXP          = BIAS + 1
  def NAN                   = Cat(0.U(1.W), Fill(expWidth, 1.U(1.W)), 1.U << mantWidth)
  def INFINITE              = Cat(sign, Fill(expWidth, 1.U(1.W)), 1.U << mantWidth)
  def ZERO                  = Cat(sign, 0.U(expWidth.W), 0.U(mantWidth.W))
  def box                   = Cat(sign, exp, mantissa)
}

object FP {
  private def unbox(x: UInt, expWidth: Int, mantWidth: Int) = {
    val sign = x(expWidth + mantWidth)
    val exp  = x(expWidth + mantWidth - 1, mantWidth)
    val mantissa = x(mantWidth - 1, 0)

    val fp = Wire(new FP(expWidth, mantWidth))
    fp.sign := sign
    fp.exp := exp
    fp.mantissa := mantissa

    val expIsZero = exp === 0.U
    val expIsNotZero = !expIsZero
    val mantissaIsZero = mantissa === 0.U
    val mantissaIsNotZero = !mantissaIsZero
    //
    fp.isZero := expIsZero && mantissaIsZero
    fp.isSubnormal := expIsNotZero && mantissaIsNotZero
    fp.isInfinite := exp === fp.EXP_MAX.U && mantissaIsZero
    fp.isNaN := exp === fp.EXP_MAX.U && mantissaIsNotZero
    fp.isOrdinary := exp >= 1.U && exp <= fp.EXP_MAX.U

    val expExtend = signedExtend(exp, expWidth+1)
    val expReal = expExtend.asSInt - fp.BIAS.S
    fp.isOverflow := expReal > fp.MAX_REAL_EXP.S
    fp.isUnderflow := expReal < -fp.MIN_REAL_EXP.S
    fp
  }

  def apply(x: UInt, expWidth: Int, mantWidth: Int) = unbox(x, expWidth, mantWidth)
}


object FPPreNormal {
  def apply(fp1: FP, fp2: FP) = {
    val normalFp1Sign = fp1.sign
    val normalFp2Sign = fp2.sign

    val extendFp1Mantissa = Cat(1.U(1.W), fp1.mantissa)
    val extendFp2Mantissa = Cat(1.U(1.W), fp2.mantissa)
    val tempFp1Mantissa = Wire(UInt(extendFp1Mantissa.getWidth.W))
    val tempFp2Mantissa = Wire(UInt(extendFp2Mantissa.getWidth.W))
    val normalFpExp = Wire(UInt())
    val shamt = Wire(UInt())
    when (fp1.exp > fp2.exp) {
      shamt := fp1.exp - fp2.exp
      normalFpExp := fp1.exp
      tempFp1Mantissa := extendFp1Mantissa
      tempFp2Mantissa := extendFp2Mantissa >> shamt
    } .otherwise {
      shamt := fp2.exp - fp1.exp
      normalFpExp := fp2.exp
      tempFp2Mantissa := extendFp2Mantissa
      tempFp1Mantissa := extendFp1Mantissa >> shamt
    }
    val normalFp1Mantissa = tempFp1Mantissa
    val normalFp2Mantissa = tempFp2Mantissa
    val normalFp1 = Cat(normalFp1Sign, normalFpExp, normalFp1Mantissa)
    val normalFp2 = Cat(normalFp2Sign, normalFpExp, normalFp2Mantissa)
    (FP(normalFp1, fp1.expWidth, fp1.mantWidth + 1),
      FP(normalFp2, fp2.expWidth, fp2.mantWidth + 1))
  }
}

object FPClassify {
  def apply(fp: FP) = {
    val positive = !fp.sign
    val isSNaN = !fp.mantissa(fp.mantWidth - 1)
    val classify = Cat(
      fp.isNaN && !isSNaN,
      fp.isNaN && isSNaN,
      positive && fp.isInfinite,
      positive && fp.isOrdinary,
      positive && fp.isSubnormal,
      positive && fp.isZero,
      !positive && fp.isZero,
      !positive && fp.isSubnormal,
      !positive && fp.isOrdinary,
      !positive && fp.isInfinite
    )
    classify
  }
}

object FPEqual {
  def apply(fp1: FP, fp2: FP) = {
    val res = Mux(fp1.isNaN || fp2.isNaN, false.B,
      Mux(fp1.isZero && fp2.isZero, false.B, fp1.box === fp2.box))
    res
  }
}

object FPLessThan {
  def apply(fp1: FP, fp2: FP) = {
    val res = Mux(fp1.isNaN || fp2.isNaN, false.B,
      Mux(fp1.mantissa < fp2.mantissa, !fp2.sign, fp1.sign && !fp2.sign))
    res
  }
}

object FPLessEqual {
  def apply(fp1: FP, fp2: FP) = FPLessThan(fp1, fp2) && FPEqual(fp1, fp2)
}

object FPMax {
  def apply(fp1: FP, fp2: FP) = Mux(FPLessThan(fp1, fp2), fp2, fp1)
}

object FPMin {
  def apply(fp1: FP, fp2: FP) = Mux(FPLessThan(fp1, fp2), fp1, fp2)
}

object FPSgnj {
  def apply(fp1: FP, fp2: FP, neg: Bool, xor: Bool) = {
    val fp = Cat(
      Mux(neg, !fp1.sign,
        Mux(xor, fp1.sign ^ fp2.sign, fp2.sign)),
      fp1.exp,
      fp1.mantissa
    )
    FP(fp, fp1.expWidth, fp2.mantWidth)
  }
}

class FPArith(expWidth: Int, mantWidth: Int)(implicit p: Parameters) extends MatrixModule {
  import FPUConstants._
  val io = IO(new Bundle() {
    val cmd = Input(UInt(CMD_SZ.W))
    val lhs = Input(new FP(expWidth, mantWidth))
    val rhs = Input(new FP(expWidth, mantWidth))
    val result = Output(UInt(XLEN.W))
  })

  val fp1 = io.lhs
  val fp2 = io.rhs
  val fp1Box = fp1.box
  val fp2Box = fp2.box

  val classifyResult = FPClassify(fp1)
  val eqResult = FPEqual(fp1, fp2)
  val ltResult = FPLessThan(fp1, fp2)
  val leResult = eqResult || ltResult
  val maxResult = Wire(UInt())
  val minResult = Wire(UInt())

  when (ltResult) {
    maxResult := fp2Box
    minResult := fp1Box
  } .otherwise {
    maxResult := fp1Box
    minResult := fp2Box
  }
  val sgnjResult = Wire(UInt())
  val sgnjNeg = io.cmd === CMD_SGNJN
  val sgnjXor = io.cmd === CMD_SGNJX
  sgnjResult := FPSgnj(fp1, fp2, sgnjNeg, sgnjXor).box

  val arithResult = Seq(zerosExtend(classifyResult, XLEN),
    zerosExtend(eqResult, XLEN),
    zerosExtend(ltResult, XLEN),
    zerosExtend(leResult, XLEN),
    maxResult,
    minResult,
    sgnjResult,
    sgnjResult)
  val cmd = Seq(CMD_CLASS,
    CMD_EQ,
    CMD_LT,
    CMD_LE,
    CMD_MAX,
    CMD_MIN,
    CMD_SGNJ,
    CMD_SGNJN,
    CMD_SGNJX
  )
  val mapping = for (i <- 0 until cmd.size) yield { (cmd(i) === io.cmd) -> arithResult(i) }
  io.result := Mux1H(mapping)
}

object FPNormal {
  def apply(fp: FP) = {
    val expWidth  = fp.expWidth
    val mantWidth = fp.mantWidth
    val select = fp.mantissa(mantWidth - 1, mantWidth - 2)
    val greatEqualOneAndLessFour = select < 4.U && select >= 1.U
    val normalExp = Wire(UInt())
    val normalMant = Wire(UInt(mantWidth.W))
    when (greatEqualOneAndLessFour) {
      normalExp := (fp.mantissa >> 1.U)(mantWidth - 2, 0)
      normalExp := fp.exp + 1.U
    } .otherwise {
      val shamt = selectFirstN(Reverse(fp.mantissa(mantWidth-2,0)))
      normalMant := (fp.mantissa << shamt)(mantWidth-2,0)
      normalExp  := fp.exp - shamt
    }
    FP(Cat(fp.sign, normalExp, normalMant), expWidth, mantWidth)
  }
}

object FPRounding {
  def apply(fp: FP, remainder: UInt, roundMode: UInt) = {
    import FPUConstants._
    val expWidth = fp.expWidth
    val mantWidth = fp.mantWidth
    val grs = Cat(
      fp.mantissa(mantWidth -1, mantWidth - 2),
      fp.mantissa(mantWidth - 3, 0).orR
    )
    val flags = WireInit(0.U(5.W))
    val mantRounded = WireInit(false.B)

    val odd = fp.mantissa(0) | grs(1,0).orR | remainder === 1.U
    flags(0) := remainder =/= 0.U | grs.orR
    mantRounded :=  roundMode === RM_RNE && (grs(2) && odd) |
      roundMode === RM_RDN && (fp.sign && flags(0)) |
      roundMode === RM_RUP && (!fp.sign && flags(0)) |
      roundMode === RM_RMM && flags(0)
    val roundedMantissa = fp.mantissa + mantRounded
    val expRounded = WireInit(false.B)
    val roundedExp = WireInit(fp.exp)
    when (fp.mantissa(mantWidth-1)) {
      expRounded := true.B
    } .elsewhen (roundedMantissa(expRounded-2.U)) {
      when (fp.exp === 0.U) {
        roundedExp := 1.U
        flags(1) := !grs(1)
      }
    }
    val normalExp = roundedExp + expRounded
    val normalMantissa = roundedMantissa >> expRounded
    val result = WireInit(UInt())
    when (fp.isNaN) {
      result := fp.NAN
    } .elsewhen (fp.isInfinite) {
      result := fp.INFINITE
    } .elsewhen (fp.isZero) {
      result := fp.ZERO
    } .elsewhen (normalExp >=  (fp.EXP_MAX + 1).U) {
      result := Cat(fp.sign, Fill(expWidth, 1.U(1.W)), Fill(mantWidth, 0.U(1.W)))
    } .otherwise {
      result := Cat(fp.sign, normalExp, normalMantissa(mantWidth-3, 0))
    }
    FP(result, expWidth, mantWidth-2)
  }
}
