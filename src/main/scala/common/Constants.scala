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
package matrix.common

import chisel3._
import chisel3.util._

trait ScalarOpConstants {
  def X = BitPat("b?")
  def Y = BitPat("b1")
  def N = BitPat("b0")

  def LONGEST_IMM_SZ = 20

  //  Uop coding
  def UOP_SZ          = 6
  def UOP_X           = BitPat.dontCare(UOP_SZ)
  def UOP_UND         = 0.U(UOP_SZ.W)
  def UOP_NOP         = 1.U(UOP_SZ.W)
  def UOP_EXCEP       = 2.U(UOP_SZ.W)
  def UOP_LUI         = 3.U(UOP_SZ.W)
  def UOP_AUIPC       = 4.U(UOP_SZ.W)
  def UOP_JAL         = 5.U(UOP_SZ.W)
  def UOP_BEQ         = 7.U(UOP_SZ.W)
  def UOP_BNE         = 8.U(UOP_SZ.W)
  def UOP_BLT         = 9.U(UOP_SZ.W)
  def UOP_BGE         = 10.U(UOP_SZ.W)

  def UOP_ADD         = 12.U(UOP_SZ.W)
  def UOP_SUB         = 13.U(UOP_SZ.W)
  def UOP_SLT         = 14.U(UOP_SZ.W)
  def UOP_XOR         = 16.U(UOP_SZ.W)
  def UOP_OR          = 17.U(UOP_SZ.W)
  def UOP_AND         = 18.U(UOP_SZ.W)
  def UOP_SLL         = 19.U(UOP_SZ.W)
  def UOP_SRL         = 20.U(UOP_SZ.W)
  def UOP_SRA         = 21.U(UOP_SZ.W)

  def UOP_FENCE       = 22.U(UOP_SZ.W)
  def UOP_FENCEI      = 23.U(UOP_SZ.W)
  def UOP_PAUSE       = 24.U(UOP_SZ.W)
  def UOP_ECALL       = 25.U(UOP_SZ.W)
  def UOP_EBREAK      = 26.U(UOP_SZ.W)
  def UOP_SRET        = 27.U(UOP_SZ.W)
  def UOP_URET        = 28.U(UOP_SZ.W)
  def UOP_MRET        = 29.U(UOP_SZ.W)
  def UOP_WFI         = 30.U(UOP_SZ.W)
  def UOP_SFENCE      = 31.U(UOP_SZ.W)

  def UOP_LD          = 32.U(UOP_SZ.W)
  def UOP_STORE       = 33.U(UOP_SZ.W)

  def UOP_MUL         = 34.U(UOP_SZ.W)
  def UOP_MULSU       = 35.U(UOP_SZ.W)
  def UOP_DIV         = 36.U(UOP_SZ.W)
  def UOP_REM         = 37.U(UOP_SZ.W)

  def UOP_AMO         = 38.U(UOP_SZ.W)

  def UOP_MADD        = 39.U(UOP_SZ.W)
  def UOP_MSUB        = 40.U(UOP_SZ.W)
  def UOP_NMADD       = 41.U(UOP_SZ.W)
  def UOP_NMSUB       = 42.U(UOP_SZ.W)

  def UOP_XCHG        = 43.U(UOP_SZ.W)
  def UOP_SET         = 44.U(UOP_SZ.W)
  def UOP_CLR         = 45.U(UOP_SZ.W)
  def UOP_SQRT        = 46.U(UOP_SZ.W)
  def UOP_SGNJ        = 47.U(UOP_SZ.W)
  def UOP_SGNJN       = 48.U(UOP_SZ.W)
  def UOP_SGNJX       = 49.U(UOP_SZ.W)
  def UOP_MIN         = 50.U(UOP_SZ.W)
  def UOP_MAX         = 51.U(UOP_SZ.W)
  def UOP_CVT         = 52.U(UOP_SZ.W)
  def UOP_MV          = 53.U(UOP_SZ.W)
  def UOP_EQ          = 54.U(UOP_SZ.W)
  def UOP_LT          = 55.U(UOP_SZ.W)
  def UOP_LE          = 56.U(UOP_SZ.W)
  def UOP_CLASS       = 57.U(UOP_SZ.W)
  def UOP_DRET        = 58.U(UOP_SZ.W)

  //  Instruction type
  def IS_SZ = 3
  def IS_X = 0.U(IS_SZ.W)
  def IS_R = 1.U(IS_SZ.W)
  def IS_I = 2.U(IS_SZ.W)
  def IS_S = 3.U(IS_SZ.W)
  def IS_B = 4.U(IS_SZ.W)
  def IS_U = 5.U(IS_SZ.W)
  def IS_J = 6.U(IS_SZ.W)
  def IS_R4= 7.U(IS_SZ.W)

  //  Issue port
  def PORT_SZ     = 3
  def PORT_ALU    = 0.U(PORT_SZ.W) // Alu
  def PORT_MUL    = 1.U(PORT_SZ.W) // Mul
  def PORT_BRANCH = 2.U(PORT_SZ.W) // Branch
  def PORT_DIV    = 3.U(PORT_SZ.W) // Div
  def PORT_FDIV   = 4.U(PORT_SZ.W) // FDIV
  def PORT_FPU    = 5.U(PORT_SZ.W) // FPU
  def PORT_LSU    = 6.U(PORT_SZ.W) // LSU

  def RD_MSB  = 11
  def RD_LSB  = 7
  def RS1_MSB = 19
  def RS1_LSB = 15
  def RS2_MSB = 24
  def RS2_LSB = 20
  def RS3_MSB = 31
  def RS3_LSB = 27

  def RT_SZ  = 3
  def RT_NON = 0.U(RT_SZ.W)
  def RT_INT = 1.U(RT_SZ.W)
  def RT_FPU = 2.U(RT_SZ.W)
  def RT_IMM = 3.U(RT_SZ.W)
  def RT_PC  = 4.U(RT_SZ.W)
  def RT_CSR = 5.U(RT_SZ.W)

  //  Bit width
  def BW_SZ   = 2
  def BW_B    = 0.U(BW_SZ.W)
  def BW_H    = 1.U(BW_SZ.W)
  def BW_W    = 2.U(BW_SZ.W)
  def BW_D    = 3.U(BW_SZ.W)

  def WAKEUP_SZ  = 6
  def WAKEUP_X   = BitPat.dontCare(WAKEUP_SZ)
}

trait MemoryOpConstants {
  def M_SZ        = 4
  def M_X         = BitPat.dontCare(M_SZ)
  def M_UND       = 0.U(M_SZ.W)
  def M_XRD       = 1.U(M_SZ.W)
  def M_XWR       = 2.U(M_SZ.W)
  def M_PWR       = 4.U(M_SZ.W)
  def M_XA_SWAP   = 5.U(M_SZ.W)
  def M_XLR       = 6.U(M_SZ.W)
  def M_XSC       = 7.U(M_SZ.W)
  def M_XA_ADD    = 8.U(M_SZ.W)
  def M_XA_XOR    = 9.U(M_SZ.W)
  def M_XA_OR     = 10.U(M_SZ.W)
  def M_XA_AND    = 11.U(M_SZ.W)
  def M_XA_MIN    = 12.U(M_SZ.W)
  def M_XA_MAX    = 13.U(M_SZ.W)
  def M_SFENCE    = 14.U(M_SZ.W)
  def M_FENCE     = 15.U(M_SZ.W)
}
