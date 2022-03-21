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
import chisel3.util.BitPat._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.util.uintToBitPat
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket.Causes
import matrix.common._
import matrix.ifu._
import matrix.utils._

abstract trait DecodeConstants extends ScalarOpConstants with MemoryOpConstants {
  def default_table: List[BitPat] = List(
    N,          //  valid
    PORT_ALU,   //  port
    UOP_X,      //  uop
    N,          //  fp_val
    N,          //  usign
    BW_D,       //  bit width
    N,          //  rs1 valid
    RT_NON,     //  rs1 type
    N,          //  rs2 valid
    RT_NON,     //  rs2 type
    N,          //  rs3 valid
    RT_NON,     //  rs3 type
    N,          //  rd valid
    RT_NON,     //  rd type
    IS_R,       //  format
    WAKEUP_X,   //  wakeup
    M_X)        //  mem cmd
}

object IDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array (
    LUI         -> List(Y,PORT_ALU,UOP_LUI,N,N,BW_D,     N,RT_NON,N,RT_IMM,N,RT_NON,Y,RT_INT,    IS_U, 0.U, M_UND),
    AUIPC       -> List(Y,PORT_ALU,UOP_ADD,N,N,BW_D,     N,RT_PC ,N,RT_IMM,N,RT_NON,Y,RT_INT,    IS_U, 0.U, M_UND),
    JAL         -> List(Y,PORT_BRANCH,UOP_JAL,N,N,BW_D,  N,RT_PC ,N,RT_IMM,N,RT_NON,Y,RT_INT,    IS_J, 0.U, M_UND),
    JALR        -> List(Y,PORT_BRANCH,UOP_JAL,N,N,BW_D,  Y,RT_INT,N,RT_IMM,N,RT_NON,Y,RT_INT,    IS_I, 0.U, M_UND),
    BEQ         -> List(Y,PORT_BRANCH,UOP_BEQ,N,N,BW_D,  Y,RT_INT,Y,RT_INT,N,RT_NON,N,RT_NON,    IS_B, 0.U, M_UND),
    BNE         -> List(Y,PORT_BRANCH,UOP_BNE,N,N,BW_D,  Y,RT_INT,Y,RT_INT,N,RT_NON,N,RT_NON,    IS_B, 0.U, M_UND),
    BLT         -> List(Y,PORT_BRANCH,UOP_BLT,N,N,BW_D,  Y,RT_INT,Y,RT_INT,N,RT_NON,N,RT_NON,    IS_B, 0.U, M_UND),
    BGE         -> List(Y,PORT_BRANCH,UOP_BGE,N,N,BW_D,  Y,RT_INT,Y,RT_INT,N,RT_NON,N,RT_NON,    IS_B, 0.U, M_UND),
    BLTU        -> List(Y,PORT_BRANCH,UOP_BLT,N,Y,BW_D,  Y,RT_INT,Y,RT_INT,N,RT_NON,N,RT_NON,    IS_B, 0.U, M_UND),
    BGEU        -> List(Y,PORT_BRANCH,UOP_BGE,N,Y,BW_D,  Y,RT_INT,Y,RT_INT,N,RT_NON,N,RT_NON,    IS_B, 0.U, M_UND),
    LB          -> List(Y,PORT_LSU,UOP_LD,N,N,BW_B,      Y,RT_INT,N,RT_NON,N,RT_NON,Y,RT_INT,    IS_I, 2.U, M_XRD),
    LH          -> List(Y,PORT_LSU,UOP_LD,N,N,BW_H,      Y,RT_INT,N,RT_NON,N,RT_NON,Y,RT_INT,    IS_I, 2.U, M_XRD),
    LW          -> List(Y,PORT_LSU,UOP_LD,N,N,BW_W,      Y,RT_INT,N,RT_NON,N,RT_NON,Y,RT_INT,    IS_I, 2.U, M_XRD),
    LD          -> List(Y,PORT_LSU,UOP_LD,N,N,BW_D,      Y,RT_INT,N,RT_NON,N,RT_NON,Y,RT_INT,    IS_I, 2.U, M_XRD),
    LBU         -> List(Y,PORT_LSU,UOP_LD,N,Y,BW_B,      Y,RT_INT,N,RT_NON,N,RT_NON,Y,RT_INT,    IS_I, 2.U, M_XRD),
    LHU         -> List(Y,PORT_LSU,UOP_LD,N,Y,BW_H,      Y,RT_INT,N,RT_NON,N,RT_NON,Y,RT_INT,    IS_I, 2.U, M_XRD),
    LWU         -> List(Y,PORT_LSU,UOP_LD,N,Y,BW_W,      Y,RT_INT,N,RT_NON,N,RT_NON,Y,RT_INT,    IS_I, 2.U, M_XRD),
    SB          -> List(Y,PORT_LSU,UOP_STORE,N,N,BW_B,   Y,RT_INT,N,RT_IMM,Y,RT_INT,N,RT_NON,    IS_S, 1.U, M_XWR),
    SH          -> List(Y,PORT_LSU,UOP_STORE,N,N,BW_H,   Y,RT_INT,N,RT_IMM,Y,RT_INT,N,RT_NON,    IS_S, 1.U, M_XWR),
    SW          -> List(Y,PORT_LSU,UOP_STORE,N,N,BW_W,   Y,RT_INT,N,RT_IMM,Y,RT_INT,N,RT_NON,    IS_S, 1.U, M_XWR),
    SD          -> List(Y,PORT_LSU,UOP_STORE,N,N,BW_D,   Y,RT_INT,N,RT_IMM,Y,RT_INT,N,RT_NON,    IS_S, 1.U, M_XWR),
    ADDI        -> List(Y,PORT_ALU,UOP_ADD,N,N,BW_D,     Y,RT_INT,N,RT_IMM,N,RT_NON,Y,RT_INT,    IS_I, 0.U, M_UND),
    ADDIW       -> List(Y,PORT_ALU,UOP_ADD,N,N,BW_W,     Y,RT_INT,N,RT_IMM,N,RT_NON,Y,RT_INT,    IS_I, 0.U, M_UND),
    SLTI        -> List(Y,PORT_ALU,UOP_SLT,N,N,BW_D,     Y,RT_INT,N,RT_IMM,N,RT_NON,Y,RT_INT,    IS_I, 0.U, M_UND),
    SLTIU       -> List(Y,PORT_ALU,UOP_SLT,N,Y,BW_D,     Y,RT_INT,N,RT_IMM,N,RT_NON,Y,RT_INT,    IS_I, 0.U, M_UND),
    XORI        -> List(Y,PORT_ALU,UOP_XOR,N,N,BW_D,     Y,RT_INT,N,RT_IMM,N,RT_NON,Y,RT_INT,    IS_I, 0.U, M_UND),
    ORI         -> List(Y,PORT_ALU,UOP_OR,N,N,BW_D,      Y,RT_INT,N,RT_IMM,N,RT_NON,Y,RT_INT,    IS_I, 0.U, M_UND),
    ANDI        -> List(Y,PORT_ALU,UOP_AND,N,N,BW_D,     Y,RT_INT,N,RT_IMM,N,RT_NON,Y,RT_INT,    IS_I, 0.U, M_UND),
    SLLI        -> List(Y,PORT_ALU,UOP_SLL,N,N,BW_D,     Y,RT_INT,N,RT_IMM,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    SLLIW       -> List(Y,PORT_ALU,UOP_SLL,N,N,BW_D,     Y,RT_INT,N,RT_IMM,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    SRLI        -> List(Y,PORT_ALU,UOP_SRL,N,N,BW_D,     Y,RT_INT,N,RT_IMM,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    SRLIW       -> List(Y,PORT_ALU,UOP_SRL,N,N,BW_W,     Y,RT_INT,N,RT_IMM,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    SRAI        -> List(Y,PORT_ALU,UOP_SRA,N,N,BW_D,     Y,RT_INT,N,RT_IMM,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    SRAIW       -> List(Y,PORT_ALU,UOP_SRA,N,N,BW_W,     Y,RT_INT,N,RT_IMM,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    ADD         -> List(Y,PORT_ALU,UOP_ADD,N,N,BW_D,     Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    ADDW        -> List(Y,PORT_ALU,UOP_ADD,N,N,BW_W,     Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    SUB         -> List(Y,PORT_ALU,UOP_SUB,N,N,BW_D,     Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    SUBW        -> List(Y,PORT_ALU,UOP_SUB,N,N,BW_W,     Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    SLL         -> List(Y,PORT_ALU,UOP_SLL,N,N,BW_D,     Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    SLLW        -> List(Y,PORT_ALU,UOP_SLL,N,N,BW_W,     Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    SLT         -> List(Y,PORT_ALU,UOP_SLT,N,N,BW_D,     Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    SLTU        -> List(Y,PORT_ALU,UOP_SLT,N,Y,BW_W,     Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    XOR         -> List(Y,PORT_ALU,UOP_XOR,N,N,BW_D,     Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    SRL         -> List(Y,PORT_ALU,UOP_SRL,N,N,BW_D,     Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    SRLW        -> List(Y,PORT_ALU,UOP_SRL,N,N,BW_W,     Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    SRA         -> List(Y,PORT_ALU,UOP_SRA,N,N,BW_D,     Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    SRAW        -> List(Y,PORT_ALU,UOP_SRA,N,N,BW_W,     Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    OR          -> List(Y,PORT_ALU,UOP_OR,N,N,BW_D,      Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    AND         -> List(Y,PORT_ALU,UOP_AND,N,N,BW_D,     Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    FENCE       -> List(Y,PORT_ALU,UOP_FENCE,N,N,BW_D,   Y,RT_INT,N,RT_NON,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    FENCE_I     -> List(Y,PORT_ALU,UOP_FENCEI,N,N,BW_D,  Y,RT_INT,N,RT_IMM,N,RT_NON,Y,RT_INT,    IS_I, 0.U, M_UND),
  )
}


object MDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array (
    MUL     -> List(Y,PORT_MUL,UOP_MUL,N,N,BW_D,    Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    MULW    -> List(Y,PORT_MUL,UOP_MUL,N,N,BW_W,    Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    MULH    -> List(Y,PORT_MUL,UOP_MUL,N,N,BW_H,    Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    MULHSU  -> List(Y,PORT_MUL,UOP_MULSU,N,N,BW_H,  Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    MULHU   -> List(Y,PORT_MUL,UOP_MUL,N,Y,BW_H,    Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    DIV     -> List(Y,PORT_MUL,UOP_DIV,N,N,BW_D,    Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    DIVU    -> List(Y,PORT_MUL,UOP_DIV,N,Y,BW_D,    Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    DIVW    -> List(Y,PORT_MUL,UOP_DIV,N,N,BW_W,    Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    DIVUW   -> List(Y,PORT_MUL,UOP_DIV,N,Y,BW_W,    Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    REM     -> List(Y,PORT_MUL,UOP_REM,N,N,BW_D,    Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    REMU    -> List(Y,PORT_MUL,UOP_REM,N,Y,BW_D,    Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    REMW    -> List(Y,PORT_MUL,UOP_REM,N,N,BW_W,    Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND),
    REMUW   -> List(Y,PORT_MUL,UOP_REM,N,Y,BW_W,    Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_UND)


  )
}

object SystemDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array (
    SRET        -> List(Y,PORT_ALU,UOP_SRET,N,N,BW_D,   N,RT_INT,N,RT_INT,N,RT_NON,N,RT_INT,    IS_R, 0.U, M_UND),
    MRET        -> List(Y,PORT_ALU,UOP_MRET,N,N,BW_D,   N,RT_INT,N,RT_INT,N,RT_NON,N,RT_INT,    IS_R, 0.U, M_UND),
    SFENCE_VMA  -> List(Y,PORT_ALU,UOP_SFENCE,N,N,BW_D, Y,RT_INT,Y,RT_INT,N,RT_NON,N,RT_INT,    IS_R, 0.U, M_UND),
    WFI         -> List(Y,PORT_ALU,UOP_WFI,N,N,BW_D,    N,RT_INT,N,RT_INT,N,RT_NON,N,RT_INT,    IS_R, 0.U, M_UND),
    ECALL       -> List(Y,PORT_ALU,UOP_ECALL,N,N,BW_D,  N,RT_INT,N,RT_INT,N,RT_NON,N,RT_INT,    IS_I, 0.U, M_UND),
    EBREAK      -> List(Y,PORT_ALU,UOP_EBREAK,N,N,BW_D, N,RT_INT,N,RT_INT,N,RT_NON,N,RT_INT,    IS_I, 0.U, M_UND),
  )
}

object ADecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array (
    LR_W        -> List(Y,PORT_LSU,UOP_AMO,N,N,BW_W,     Y,RT_INT,N,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 2.U, M_XLR),
    LR_D        -> List(Y,PORT_LSU,UOP_AMO,N,N,BW_D,     Y,RT_INT,N,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 2.U, M_XLR),
    SC_W        -> List(Y,PORT_LSU,UOP_AMO,N,N,BW_W,     Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 1.U, M_XSC),
    SC_D        -> List(Y,PORT_LSU,UOP_AMO,N,N,BW_D,     Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 1.U, M_XSC),
    AMOSWAP_W   -> List(Y,PORT_LSU,UOP_AMO,N,N,BW_W,     Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_XA_SWAP),
    AMOSWAP_D   -> List(Y,PORT_LSU,UOP_AMO,N,N,BW_D,     Y,RT_INT,Y,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_XA_SWAP),
    AMOADD_W    -> List(Y,PORT_LSU,UOP_AMO,N,N,BW_W,     Y,RT_INT,N,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_XA_ADD),
    AMOADD_D    -> List(Y,PORT_LSU,UOP_AMO,N,N,BW_D,     Y,RT_INT,N,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_XA_ADD),
    AMOXOR_W    -> List(Y,PORT_LSU,UOP_AMO,N,N,BW_W,     Y,RT_INT,N,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_XA_XOR),
    AMOXOR_D    -> List(Y,PORT_LSU,UOP_AMO,N,N,BW_D,     Y,RT_INT,N,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_XA_XOR),
    AMOAND_W    -> List(Y,PORT_LSU,UOP_AMO,N,N,BW_W,     Y,RT_INT,N,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_XA_AND),
    AMOAND_D    -> List(Y,PORT_LSU,UOP_AMO,N,N,BW_D,     Y,RT_INT,N,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_XA_AND),
    AMOOR_W     -> List(Y,PORT_LSU,UOP_AMO,N,N,BW_W,     Y,RT_INT,N,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_XA_OR),
    AMOOR_D     -> List(Y,PORT_LSU,UOP_AMO,N,N,BW_D,     Y,RT_INT,N,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_XA_OR),
    AMOMIN_W    -> List(Y,PORT_LSU,UOP_AMO,N,N,BW_W,     Y,RT_INT,N,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_XA_MIN),
    AMOMIN_D    -> List(Y,PORT_LSU,UOP_AMO,N,N,BW_D,     Y,RT_INT,N,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_XA_MIN),
    AMOMAX_W    -> List(Y,PORT_LSU,UOP_AMO,N,N,BW_W,     Y,RT_INT,N,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_XA_MAX),
    AMOMAX_D    -> List(Y,PORT_LSU,UOP_AMO,N,N,BW_D,     Y,RT_INT,N,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_XA_MAX),
    AMOMINU_W   -> List(Y,PORT_LSU,UOP_AMO,N,Y,BW_W,     Y,RT_INT,N,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_XA_MIN),
    AMOMINU_D   -> List(Y,PORT_LSU,UOP_AMO,N,Y,BW_D,     Y,RT_INT,N,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_XA_MIN),
    AMOMAXU_W   -> List(Y,PORT_LSU,UOP_AMO,N,Y,BW_W,     Y,RT_INT,N,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_XA_MAX),
    AMOMAXU_D   -> List(Y,PORT_LSU,UOP_AMO,N,Y,BW_D,     Y,RT_INT,N,RT_INT,N,RT_NON,Y,RT_INT,    IS_R, 0.U, M_XA_MAX)
  )
}

object ZicsrDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array (
    CSRRW   -> List(Y,PORT_ALU,UOP_XCHG,N,N,BW_D,   Y,RT_INT,N,RT_CSR,N,RT_NON,Y,RT_CSR,    IS_I, 0.U, M_UND),
    CSRRS   -> List(Y,PORT_ALU,UOP_SET,N,N,BW_D,    Y,RT_INT,N,RT_CSR,N,RT_NON,Y,RT_CSR,    IS_I, 0.U, M_UND),
    CSRRC   -> List(Y,PORT_ALU,UOP_CLR,N,N,BW_D,    Y,RT_INT,N,RT_CSR,N,RT_NON,Y,RT_CSR,    IS_I, 0.U, M_UND),
    CSRRWI  -> List(Y,PORT_ALU,UOP_XCHG,N,Y,BW_D,   N,RT_IMM,N,RT_CSR,N,RT_NON,Y,RT_CSR,    IS_I, 0.U, M_UND),
    CSRRSI  -> List(Y,PORT_ALU,UOP_SET,N,Y,BW_D,    N,RT_IMM,N,RT_CSR,N,RT_NON,Y,RT_CSR,    IS_I, 0.U, M_UND),
    CSRRCI  -> List(Y,PORT_ALU,UOP_CLR,N,Y,BW_D,    N,RT_IMM,N,RT_CSR,N,RT_NON,Y,RT_CSR,    IS_I, 0.U, M_UND),
  )
}

object FDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array (
    FLW         -> List(Y,PORT_LSU,UOP_LD,Y,N,BW_W,         Y,RT_INT,N,RT_IMM,N,RT_NON,Y,RT_FPU,    IS_I,  2.U, M_XRD),
    FLD         -> List(Y,PORT_LSU,UOP_LD,Y,N,BW_D,         Y,RT_INT,N,RT_IMM,N,RT_NON,Y,RT_FPU,    IS_I,  2.U, M_XRD),
    FSW         -> List(Y,PORT_LSU,UOP_STORE,Y,N,BW_W,      Y,RT_INT,N,RT_IMM,Y,RT_INT,Y,RT_FPU,    IS_I,  1.U, M_XWR),
    FSD         -> List(Y,PORT_LSU,UOP_STORE,Y,N,BW_D,      Y,RT_INT,N,RT_IMM,Y,RT_INT,Y,RT_FPU,    IS_I,  1.U, M_XWR),
    FMADD_S     -> List(Y,PORT_FPU,UOP_MADD,Y,N,BW_W,       Y,RT_FPU,Y,RT_FPU,Y,RT_FPU,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FMADD_D     -> List(Y,PORT_FPU,UOP_MADD,Y,N,BW_D,       Y,RT_FPU,Y,RT_FPU,Y,RT_FPU,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FMSUB_S     -> List(Y,PORT_FPU,UOP_MSUB,Y,N,BW_W,       Y,RT_FPU,Y,RT_FPU,Y,RT_FPU,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FMSUB_D     -> List(Y,PORT_FPU,UOP_MSUB,Y,N,BW_D,       Y,RT_FPU,Y,RT_FPU,Y,RT_FPU,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FNMSUB_S    -> List(Y,PORT_FPU,UOP_NMSUB,Y,N,BW_W,      Y,RT_FPU,Y,RT_FPU,Y,RT_FPU,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FNMSUB_D    -> List(Y,PORT_FPU,UOP_NMSUB,Y,N,BW_D,      Y,RT_FPU,Y,RT_FPU,Y,RT_FPU,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FNMADD_S    -> List(Y,PORT_FPU,UOP_NMADD,Y,N,BW_W,      Y,RT_FPU,Y,RT_FPU,Y,RT_FPU,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FNMADD_D    -> List(Y,PORT_FPU,UOP_NMADD,Y,N,BW_W,      Y,RT_FPU,Y,RT_FPU,Y,RT_FPU,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FADD_S      -> List(Y,PORT_FPU,UOP_ADD,Y,N,BW_W,        Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FADD_D      -> List(Y,PORT_FPU,UOP_ADD,Y,N,BW_D,        Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FSUB_S      -> List(Y,PORT_FPU,UOP_SUB,Y,N,BW_W,        Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FSUB_D      -> List(Y,PORT_FPU,UOP_SUB,Y,N,BW_D,        Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FMUL_S      -> List(Y,PORT_FPU,UOP_MUL,Y,N,BW_W,        Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FMUL_D      -> List(Y,PORT_FPU,UOP_MUL,Y,N,BW_D,        Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FDIV_S      -> List(Y,PORT_FPU,UOP_DIV,Y,N,BW_W,        Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FDIV_D      -> List(Y,PORT_FPU,UOP_DIV,Y,N,BW_D,        Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FSQRT_S     -> List(Y,PORT_FPU,UOP_SQRT,Y,N,BW_W,       Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FSQRT_D     -> List(Y,PORT_FPU,UOP_SQRT,Y,N,BW_D,       Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FSGNJ_S     -> List(Y,PORT_FPU,UOP_SGNJ,Y,N,BW_W,       Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FSGNJ_D     -> List(Y,PORT_FPU,UOP_SGNJ,Y,N,BW_D,       Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FSGNJN_S    -> List(Y,PORT_FPU,UOP_SGNJN,Y,N,BW_W,      Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FSGNJN_D    -> List(Y,PORT_FPU,UOP_SGNJN,Y,N,BW_D,      Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FSGNJX_S    -> List(Y,PORT_FPU,UOP_SGNJX,Y,N,BW_W,      Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FSGNJX_D    -> List(Y,PORT_FPU,UOP_SGNJX,Y,N,BW_D,      Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FMIN_S      -> List(Y,PORT_FPU,UOP_MIN,Y,N,BW_W,        Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FMIN_D      -> List(Y,PORT_FPU,UOP_MIN,Y,N,BW_D,        Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FCVT_W_S    -> List(Y,PORT_FPU,UOP_CVT,Y,N,BW_W,        Y,RT_FPU,N,RT_NON,N,RT_NON,Y,RT_INT,    IS_R4, 0.U, M_UND),
    FCVT_W_D    -> List(Y,PORT_FPU,UOP_CVT,Y,N,BW_D,        Y,RT_FPU,N,RT_NON,N,RT_NON,Y,RT_INT,    IS_R4, 0.U, M_UND),
    FCVT_WU_S   -> List(Y,PORT_FPU,UOP_CVT,Y,Y,BW_W,        Y,RT_FPU,N,RT_NON,N,RT_NON,Y,RT_INT,    IS_R4, 0.U, M_UND),
    FCVT_WU_D   -> List(Y,PORT_FPU,UOP_CVT,Y,Y,BW_D,        Y,RT_FPU,N,RT_NON,N,RT_NON,Y,RT_INT,    IS_R4, 0.U, M_UND),
    FMV_X_W     -> List(Y,PORT_FPU,UOP_MV,Y,N,BW_W,         Y,RT_FPU,N,RT_NON,N,RT_NON,Y,RT_INT,    IS_R4, 0.U, M_UND),
    FEQ_S       -> List(Y,PORT_FPU,UOP_EQ,Y,N,BW_W,         Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_INT,    IS_R4, 0.U, M_UND),
    FEQ_D       -> List(Y,PORT_FPU,UOP_EQ,Y,N,BW_D,         Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_INT,    IS_R4, 0.U, M_UND),
    FLT_S       -> List(Y,PORT_FPU,UOP_LT,Y,N,BW_W,         Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_INT,    IS_R4, 0.U, M_UND),
    FLT_D       -> List(Y,PORT_FPU,UOP_LT,Y,N,BW_D,         Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_INT,    IS_R4, 0.U, M_UND),
    FLE_S       -> List(Y,PORT_FPU,UOP_LE,Y,N,BW_W,         Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_INT,    IS_R4, 0.U, M_UND),
    FLE_D       -> List(Y,PORT_FPU,UOP_LE,Y,N,BW_D,         Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_INT,    IS_R4, 0.U, M_UND),
    FCLASS_S    -> List(Y,PORT_FPU,UOP_CLASS,Y,N,BW_W,      Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_INT,    IS_R4, 0.U, M_UND),
    FCLASS_D    -> List(Y,PORT_FPU,UOP_CLASS,Y,N,BW_D,      Y,RT_FPU,Y,RT_FPU,N,RT_NON,Y,RT_INT,    IS_R4, 0.U, M_UND),
    FCVT_S_W    -> List(Y,PORT_FPU,UOP_CVT,Y,N,BW_W,        Y,RT_INT,N,RT_NON,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FCVT_S_WU   -> List(Y,PORT_FPU,UOP_CVT,Y,Y,BW_W,        Y,RT_INT,N,RT_NON,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FMV_W_X     -> List(Y,PORT_FPU,UOP_MV,Y,N,BW_W,         Y,RT_INT,N,RT_NON,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FCVT_L_S    -> List(Y,PORT_FPU,UOP_CVT,Y,N,BW_D,        Y,RT_FPU,N,RT_NON,N,RT_NON,Y,RT_INT,    IS_R4, 0.U, M_UND),
    FCVT_LU_S   -> List(Y,PORT_FPU,UOP_CVT,Y,Y,BW_D,        Y,RT_FPU,N,RT_NON,N,RT_NON,Y,RT_INT,    IS_R4, 0.U, M_UND),
    FCVT_S_L    -> List(Y,PORT_FPU,UOP_CVT,Y,N,BW_W,        Y,RT_INT,N,RT_NON,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FCVT_S_LU   -> List(Y,PORT_FPU,UOP_CVT,Y,Y,BW_W,        Y,RT_INT,N,RT_NON,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FCVT_S_D    -> List(Y,PORT_FPU,UOP_CVT,Y,N,BW_W,        Y,RT_FPU,N,RT_NON,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FCVT_D_S    -> List(Y,PORT_FPU,UOP_CVT,Y,N,BW_D,        Y,RT_FPU,N,RT_NON,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FCVT_L_D    -> List(Y,PORT_FPU,UOP_CVT,Y,N,BW_D,        Y,RT_FPU,N,RT_NON,N,RT_NON,Y,RT_INT,    IS_R4, 0.U, M_UND),
    FCVT_LU_D   -> List(Y,PORT_FPU,UOP_CVT,Y,Y,BW_D,        Y,RT_FPU,N,RT_NON,N,RT_NON,Y,RT_INT,    IS_R4, 0.U, M_UND),
    FMV_X_D     -> List(Y,PORT_FPU,UOP_MV,Y,N,BW_D,         Y,RT_FPU,N,RT_NON,N,RT_NON,Y,RT_INT,    IS_R4, 0.U, M_UND),
    FCVT_D_L    -> List(Y,PORT_FPU,UOP_CVT,Y,N,BW_D,        Y,RT_INT,N,RT_NON,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FCVT_D_LU   -> List(Y,PORT_FPU,UOP_CVT,Y,Y,BW_D,        Y,RT_INT,N,RT_NON,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
    FMV_D_X     -> List(Y,PORT_FPU,UOP_MV,Y,N,BW_D,         Y,RT_INT,N,RT_NON,N,RT_NON,Y,RT_FPU,    IS_R4, 0.U, M_UND),
  )
}

object DebugDecode extends DecodeConstants  {
  val table: Array[(BitPat, List[BitPat])] = Array (
    DRET    -> List(Y,PORT_ALU,UOP_DRET,N,N,BW_D,   N,RT_INT,N,RT_INT,N,RT_NON,N,RT_INT,    IS_R, 0.U, M_UND),
  )
}


class CompressedDecode(x: UInt)(implicit p: Parameters) extends MatrixBundle {
  def rs1p = Cat(1.U(2.W), x(9,7))
  def rs2p = Cat(1.U(2.W), x(4,2))
  def rs2 = x(6,2)
  def rd = x(11,7)
  def addi4spnImm = Cat(x(10,7), x(12,11), x(5), x(6), 0.U(2.W))
  def lwImm = Cat(x(5), x(12,10), x(6), 0.U(2.W))
  def ldImm = Cat(x(6,5), x(12,10), 0.U(3.W))
  def lwspImm = Cat(x(3,2), x(12), x(6,4), 0.U(2.W))
  def ldspImm = Cat(x(4,2), x(12), x(6,5), 0.U(3.W))
  def swspImm = Cat(x(8,7), x(12,9), 0.U(2.W))
  def sdspImm = Cat(x(9,7), x(12,10), 0.U(3.W))
  def luiImm = Cat(Fill(15, x(12)), x(6,2), 0.U(12.W))
  def addi16spImm = Cat(Fill(3, x(12)), x(4,3), x(5), x(2), x(6), 0.U(4.W))
  def addiImm = Cat(Fill(7, x(12)), x(6,2))
  def jImm = Cat(Fill(10, x(12)), x(8), x(10,9), x(6), x(7), x(2), x(11), x(5,3), 0.U(1.W))
  def bImm = Cat(Fill(5, x(12)), x(6,5), x(2), x(11,10), x(4,3), 0.U(1.W))
  def shamt = Cat(x(12), x(6,2))
  def x0 = 0.U(5.W)
  def ra = 1.U(5.W)
  def sp = 2.U(5.W)

  def q0 = {
    def addi4spn = {
      val opc = Mux(x(12,5).orR, 0x13.U(7.W), 0x1F.U(7.W))
      Cat(addi4spnImm, sp, 0.U(3.W), rs2p, opc)
    }
    def ld = Cat(ldImm, rs1p, 3.U(3.W), rs2p, 0x03.U(7.W))
    def lw = Cat(lwImm, rs1p, 2.U(3.W), rs2p, 0x03.U(7.W))
    def fld = Cat(ldImm, rs1p, 3.U(3.W), rs2p, 0x07.U(7.W))
    def flw = {
      if (XLEN == 32) Cat(lwImm, rs1p, 2.U(3.W), rs2p, 0x07.U(7.W))
      else ld
    }
    def unimp = Cat(lwImm >> 5, rs2p, rs1p, 2.U(3.W), lwImm(4,0), 0x3F.U(7.W))
    def sd = Cat(ldImm >> 5, rs2p, rs1p, 3.U(3.W), ldImm(4,0), 0x23.U(7.W))
    def sw = Cat(lwImm >> 5, rs2p, rs1p, 2.U(3.W), lwImm(4,0), 0x23.U(7.W))
    def fsd = Cat(ldImm >> 5, rs2p, rs1p, 3.U(3.W), ldImm(4,0), 0x27.U(7.W))
    def fsw = {
      if (XLEN == 32) Cat(lwImm >> 5, rs2p, rs1p, 2.U(3.W), lwImm(4,0), 0x27.U(7.W))
      else sd
    }
    Seq(addi4spn, fld, lw, flw, unimp, fsd, sw, fsw)
  }

  def q1 = {
    def addi = Cat(addiImm, rd, 0.U(3.W), rd, 0x13.U(7.W))
    def addiw = {
      val opc = Mux(rd.orR, 0x1B.U(7.W), 0x1F.U(7.W))
      Cat(addiImm, rd, 0.U(3.W), rd, opc)
    }
    def jal = {
      if (XLEN == 32) Cat(jImm(20), jImm(10,1), jImm(11), jImm(19,12), ra, 0x6F.U(7.W))
      else addiw
    }
    def li = Cat(addiImm, x0, 0.U(3.W), rd, 0x13.U(7.W))
    def addi16sp = {
      val opc = Mux(addiImm.orR, 0x13.U(7.W), 0x1F.U(7.W))
      Cat(addi16spImm, rd, 0.U(3.W), rd, opc)
    }
    def lui = {
      val opc = Mux(addiImm.orR, 0x37.U(7.W), 0x3F.U(7.W))
      val me = Cat(luiImm(31,12), rd, opc)
      Mux(rd === x0 || rd === sp, addi16sp, me)
    }
    def j = Cat(jImm(20), jImm(10,1), jImm(11), jImm(19,12), x0, 0x6F.U(7.W))
    def beqz = Cat(bImm(12), bImm(10,5), x0, rs1p, 0.U(3.W), bImm(4,1), bImm(11), 0x63.U(7.W))
    def bnez = Cat(bImm(12), bImm(10,5), x0, rs1p, 1.U(3.W), bImm(4,1), bImm(11), 0x63.U(7.W))
    def arith = {
      def srli = Cat(shamt, rs1p, 5.U(3.W), rs1p, 0x13.U(7.W))
      def srai = srli | (1 << 30).U
      def andi = Cat(addiImm, rs1p, 7.U(3.W), rs1p, 0x13.U(7.W))
      def rtype = {
        val funct = VecInit(Seq(0.U, 4.U, 6.U, 7.U, 0.U, 0.U, 2.U, 3.U))(Cat(x(12), x(6,5)))
        val sub = Mux(x(6,5) === 0.U, (1 << 30).U, 0.U)
        val opc = Mux(x(12), 0x3B.U(7.W), 0x33.U(7.W))
        Cat(rs2p, rs1p, funct, rs1p, opc) | sub
      }
      VecInit(Seq(srli, srai, andi, rtype))(x(11, 10))
    }
    Seq(addi, jal, li, lui, arith, j, beqz, bnez)
  }

  def q2 = {
    val load_opc = Mux(rd.orR, 0x03.U(7.W), 0x1F.U(7.W))
    def slli = Cat(shamt, rd, 1.U(3.W), rd, 0x13.U(7.W))
    def ldsp = Cat(ldspImm, sp, 3.U(3.W), rd, load_opc)
    def lwsp = Cat(lwspImm, sp, 2.U(3.W), rd, load_opc)
    def fldsp = Cat(ldspImm, sp, 3.U(3.W), rd, 0x07.U(7.W))
    def flwsp = {
      if (XLEN == 32) Cat(lwspImm, sp, 2.U(3.W), rd, 0x07.U(7.W))
      else ldsp
    }
    def sdsp = Cat(sdspImm >> 5, rs2, sp, 3.U(3.W), sdspImm(4,0), 0x23.U(7.W))
    def swsp = Cat(swspImm >> 5, rs2, sp, 2.U(3.W), swspImm(4,0), 0x23.U(7.W))
    def fsdsp = Cat(sdspImm >> 5, rs2, sp, 3.U(3.W), sdspImm(4,0), 0x27.U(7.W))
    def fswsp = {
      if (XLEN == 32) Cat(swspImm >> 5, rs2, sp, 2.U(3.W), swspImm(4,0), 0x27.U(7.W))
      else sdsp
    }
    def jalr = {
      val mv = Cat(rs2, x0, 0.U(3.W), rd, 0x33.U(7.W))
      val add = Cat(rs2, rd, 0.U(3.W), rd, 0x33.U(7.W))
      val jr = Cat(rs2, rd, 0.U(3.W), x0, 0x67.U(7.W))
      val reserved = Cat(jr >> 7, 0x1F.U(7.W))
      val jr_reserved = Mux(rd.orR, jr, reserved)
      val jr_mv = Mux(rs2.orR, mv, jr_reserved)
      val jalr = Cat(rs2, rd, 0.U(3.W), ra, 0x67.U(7.W))
      val ebreak = Cat(jr >> 7, 0x73.U(7.W)) | (1 << 20).U
      val jalr_ebreak = Mux(rd.orR, jalr, ebreak)
      val jalr_add = Mux(rs2.orR, add, jalr_ebreak)
      Mux(x(12), jalr_add, jr_mv)
    }
    Seq(slli, fldsp, lwsp, flwsp, jalr, fsdsp, swsp, fswsp)
  }

  def decode = {
    val s = VecInit(q0 ++ q1 ++ q2)
    s(Cat(x(1,0), x(15,13)))
  }
}


class CtrlSigs extends Bundle
  with ScalarOpConstants
  with MemoryOpConstants {
  val valid       = Bool()
  val port        = UInt(PORT_SZ.W)
  val uop         = UInt(UOP_SZ.W)
  val fp_val      = Bool()
  val usign       = Bool()
  val bw          = UInt(BW_SZ.W)
  val rs1_val     = Bool()
  val rs1_type    = UInt(RT_SZ.W)
  val rs2_val     = Bool()
  val rs2_type    = UInt(RT_SZ.W)
  val rs3_val     = Bool()
  val rs3_type    = UInt(RT_SZ.W)
  val rd_val      = Bool()
  val rd_type     = UInt(RT_SZ.W)
  val imm_sel     = UInt(IS_SZ.W)
  val wakeup      = UInt(WAKEUP_SZ.W)
  val mem_cmd     = UInt(M_SZ.W)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = freechips.rocketchip.rocket.DecodeLogic(inst, IDecode.default_table, table)
    val sigs = Seq(
      valid,
      port,
      uop,
      fp_val,
      usign,
      bw,

      rs1_val,
      rs1_type,
      rs2_val,
      rs2_type,
      rs3_val,
      rs3_type,
      rd_val,
      rd_type,

      imm_sel,
      wakeup,
      mem_cmd
    )
    sigs zip decoder map { case (s, w) => s := w }
    this
  }
}

class DecoderIO(implicit p: Parameters) extends MatrixBundle 
  with ScalarOpConstants {
  val excp        = Input(Bool())
  val inst        = Input(UInt(ILEN.W))
  val len         = Input(Bool())
  val micro_uop   = Output(new MicroOp)
  val csr_addr    = Output(UInt(CSR_ADDR_SZ.W))
  val imm_sel     = Output(UInt(IS_SZ.W))
  val short_imm   = Output(UInt(LONGEST_IMM_SZ.W))
  val cause       = Output(UInt(EXLEN.W))
}

class Decoder(implicit p: Parameters) extends MatrixModule 
  with ScalarOpConstants {
  val io = IO(new DecoderIO)

  val decode_table = IDecode.table ++ MDecode.table ++ SystemDecode.table ++
    ZicsrDecode.table ++ ADecode.table ++ FDecode.table
  val expand_inst = new CompressedDecode(io.inst).decode
  val inst = Mux(io.len, expand_inst, io.inst)

  val dsigs = Wire(new CtrlSigs).decode(inst, decode_table)
  val rs1 = inst(RS1_MSB, RS1_LSB)
  val rs2 = inst(RS2_MSB, RS2_LSB)
  val rs3 = inst(RS3_MSB, RS3_LSB)
  val rd  = inst(RD_MSB, RD_LSB)

  val rs1_is_not_x0 = rs1.orR
  val rs2_is_not_x0 = rs2.orR
  val rs3_is_not_x0 = rs3.orR
  val rd_is_not_x0  = rd.orR

  val inst_valid = dsigs.valid || !io.excp

  val uop = Wire(new MicroOp)
  uop.len             := io.len
  uop.uop             := Mux(inst_valid, dsigs.uop, UOP_EXCEP)
  uop.port            := Mux(inst_valid, dsigs.port, PORT_ALU)
  uop.bw              := dsigs.bw
  uop.fp_val          := dsigs.fp_val
  uop.usign           := dsigs.usign
  uop.rs1_val         := dsigs.rs1_val && rs1_is_not_x0 && inst_valid
  uop.rs1_type        := dsigs.rs2_type
  uop.rs1             := rs1
  uop.rs2_val         := dsigs.rs2_val && rs2_is_not_x0 && inst_valid
  uop.rs2_type        := dsigs.rs2_type
  uop.rs2             := rs2
  uop.rs3_val         := dsigs.rs3_val && rs3_is_not_x0 && inst_valid
  uop.rs3_type        := dsigs.rs3_type
  uop.rs3             := rs3
  uop.rd_val          := dsigs.rd_val && rd_is_not_x0 && inst_valid
  uop.rd_type         := dsigs.rd_type
  uop.rd              := rd
  uop.wakeup          := dsigs.wakeup
  uop.mem_cmd         := dsigs.mem_cmd

  io.micro_uop := uop
  io.csr_addr := io.inst(ILEN - 1, 20)
  io.imm_sel := dsigs.imm_sel
  io.short_imm := Cat(inst(31, 25),
    Mux(dsigs.imm_sel === IS_B || dsigs.imm_sel === IS_S, inst(11, 7), inst(24, 20)),
    inst(19, 12))
  io.cause := Mux(dsigs.valid, 0.U, Causes.illegal_instruction.U)
}

class DecodeResp(implicit p: Parameters) extends MatrixBundle
  with ScalarOpConstants {
  val valid       = Bool()
  val micro_op    = new MicroOp
  val pc          = UInt(vaddrWidth.W)
  val pred_info   = new PredictorResp
  val csr_addr    = UInt(CSR_ADDR_SZ.W)
  val imm_sel     = UInt(IS_SZ.W)
  val short_imm   = UInt(LONGEST_IMM_SZ.W)
  val cause       = UInt(EXLEN.W)
  val order       = Bool()
}

class DecodeIO(implicit p: Parameters) extends MatrixBundle {
  val kill = Input(Bool())
  val stall = Input(Bool())
  val req = Flipped(Valid(Vec(decodeWidth, new InstBufferResp)))
  val resp = Valid(Vec(decodeWidth, new DecodeResp))
}

class Decode(implicit p: Parameters) extends MatrixModule {
  val io = IO(new DecodeIO)

  val kill = WireInit(io.kill)
  val stall = WireInit(io.stall)

  val decoder_module = Seq.fill(decodeWidth) {
    Module(new Decoder)
  }
  val decoders = VecInit(decoder_module.map(_.io))
  for (w <- 0 until decodeWidth) {
    decoders(w).inst := io.req.bits(w).inst
    decoders(w).len := io.req.bits(w).len
    decoders(w).excp := io.req.bits(w).cause.orR
  }

  val valid = Wire(Bool())
  when(kill) {
    valid := false.B
  }.otherwise {
    valid := io.req.valid
  }

  io.resp.valid := RegEnable(valid, !stall | kill)
  val resp = Reg(Vec(decodeWidth, new DecodeResp))
  for (w <- 0 until decodeWidth) {
    when(kill) {
      resp(w).valid := false.B
    }.elsewhen(!stall && io.req.valid) {
      resp(w).valid := io.req.bits(w).valid
      resp(w).micro_op := decoders(w).micro_uop
      resp(w).csr_addr := decoders(w).csr_addr
      resp(w).imm_sel := decoders(w).imm_sel
      resp(w).short_imm := decoders(w).short_imm
      resp(w).pc := io.req.bits(w).pc
      resp(w).pred_info := io.req.bits(w).pred_info
      resp(w).cause := Mux(io.req.bits(w).cause.orR,
        io.req.bits(w).cause, decoders(w).cause)
      resp(w).order := io.req.bits(w).order
    }
  }

  io.resp := resp
}