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
import chisel3.util._
import freechips.rocketchip.config.Parameters
import matrix.common._

class MStatus(implicit p: Parameters) extends MatrixBundle {
  val sd      = Bool()
  val zeros0  = UInt(25.W)
  val mbe     = Bool()
  val sbe     = Bool()
  val sxl     = UInt(2.W)
  val uxl     = UInt(2.W)
  val zeros1  = UInt(9.W)
  val tsr     = Bool()
  val tw      = Bool()
  val tvm     = Bool()
  val mxr     = Bool()
  val sum     = Bool()
  val mprv    = Bool()
  val xs      = UInt(2.W)
  val fs      = UInt(2.W)
  val mpp     = UInt(2.W)
  val vs      = UInt(2.W)
  val spp     = Bool()
  val mpie    = Bool()
  val ube     = Bool()
  val spie    = Bool()
  val zeros2  = Bool()
  val mie     = Bool()
  val zeros3  = Bool()
  val sie     = Bool()
  val zeros4  = Bool()
}

class MTvec(implicit p: Parameters) extends MatrixBundle {
  val base = UInt((XLEN - 2).W)
  val mode = UInt(2.W)
  def direct = mode === 0.U
  def vectored = mode === 1.U
  def illegal = !(direct | vectored)
}

class Mip(implicit p: Parameters) extends MatrixBundle {
  val zeros0 = UInt((XLEN - 16).W)
  val zeros1 = UInt(3.W)
  val sgeip  = Bool()
  val meip   = Bool()
  val vseip  = Bool()
  val seip   = Bool()
  val zeros2 = Bool()
  val mtip   = Bool()
  val vstip  = Bool()
  val stip   = Bool()
  val zeros3 = Bool()
  val msip   = Bool()
  val vssip  = Bool()
  val ssip   = Bool()
  val zeros4 = Bool()
}

class Mie(implicit p: Parameters) extends MatrixBundle {
  val zeros0 = UInt((XLEN - 16).W)
  val zeros1 = UInt(3.W)
  val sgeie  = Bool()
  val meie   = Bool()
  val vseie  = Bool()
  val seie   = Bool()
  val zeros2 = Bool()
  val mtie   = Bool()
  val vstie  = Bool()
  val stie   = Bool()
  val zeros3 = Bool()
  val msie   = Bool()
  val vssie  = Bool()
  val ssie   = Bool()
  val zeros4 = Bool()
}

class Menvcfg(implicit p: Parameters) extends MatrixBundle {
  val stce    = Bool()
  val pbmte   = Bool()
  val zeros0  = UInt(54.W)
  val cbze    = Bool()
  val cbcfe   = Bool()
  val cbie    = Bool()
  val zeros1  = UInt(3.W)
  val fiom    = Bool()
}

class Mseccfg(implicit p: Parameters) extends MatrixBundle {
  val zeros0  = UInt((XLEN - 10).W)
  val sseed   = Bool()
  val useed   = Bool()
  val zeros1  = UInt(5.W)
  val rlb     = Bool()
  val mmwp    = Bool()
  val mml     = Bool()
}

class Sstatus(implicit p: Parameters) extends MatrixBundle {
  val sd      = Bool()
  val zeros0  = UInt(29.W)
  val uxl     = UInt(2.W)
  val zeros1  = UInt(12.W)
  val mxr     = Bool()
  val sum     = Bool()
  val zeros2  = Bool()
  val xs      = UInt(2.W)
  val fs      = UInt(2.W)
  val zeros3  = UInt(2.W)
  val vs      = UInt(2.W)
  val spp     = Bool()
  val zeros4  = Bool()
  val ube     = Bool()
  val spie    = Bool()
  val zeros5  = UInt(3.W)
  val sie     = Bool()
  val zeros6  = Bool()
}

class Stvec(implicit p: Parameters) extends MatrixBundle {
  val base = UInt((XLEN - 2).W)
  val mode = UInt(2.W)
  def direct = mode === 0.U
  def vectored = mode === 1.U
  def illegal = !(direct | vectored)
}

class Sip(implicit p: Parameters) extends MatrixBundle {
  val zeros0  = UInt((XLEN - 10).W)
  val seip    = Bool()
  val zeros1  = UInt(3.W)
  val stip    = Bool()
  val zeros2  = UInt(3.W)
  val ssip    = Bool()
  val zeros3  = Bool()
}

class Sie(implicit p: Parameters) extends MatrixBundle {
  val zeros0  = UInt((XLEN - 10).W)
  val seie    = Bool()
  val zeros1  = UInt(3.W)
  val stie    = Bool()
  val zeros2  = UInt(3.W)
  val ssie    = Bool()
  val zeros3  = Bool()
}

class Senvcfg(implicit p: Parameters) extends MatrixBundle {
  val zeros0  = UInt((XLEN - 10).W)
  val cbze    = Bool()
  val cbcfe   = Bool()
  val cbie    = Bool()
  val zeros1  = UInt(3.W)
  val fiom    = Bool()
}

class Satp(implicit p: Parameters) extends MatrixBundle {
  val mode = UInt(4.W)
  val asid = UInt(ASIDLEN.W)
  val ppn  = UInt(44.W)

  def bare = mode === 0.U
  def sv39 = mode === 8.U
  def sv48 = mode === 9.U
  def sv57 = mode === 10.U
  def sv64 = mode === 11.U
}

class DCSR extends Bundle {
  val debugver    = UInt(4.W)
  val zeros0      = UInt(10.W)
  val ebreakvs    = Bool()
  val ebreakvu    = Bool()
  val ebreakm     = Bool()
  val zeros1      = Bool()
  val ebreaks     = Bool()
  val ebreaku     = Bool()
  val stepie      = Bool()
  val stopcount   = Bool()
  val stoptime    = Bool()
  val cause       = UInt(3.W)
  val v           = Bool()
  val mprven      = Bool()
  val nmip        = Bool()
  val step        = Bool()
  val prv         = UInt(2.W)
}

class FFlags extends Bundle {
  val nv = Bool()
  val dz = Bool()
  val of = Bool()
  val uf = Bool()
  val nx = Bool()
}

class TData(implicit p: Parameters) extends MatrixBundle {
  val t       = UInt(4.W)
  val dmode   = Bool()
  val data    = UInt((XLEN - 5).W)
}


object CSRs {
  val fflags = 0x1
  val frm = 0x2
  val fcsr = 0x3
  val ustatus = 0x0
  val uie = 0x4
  val utvec = 0x5
  val vstart = 0x8
  val vxsat = 0x9
  val vxrm = 0xa
  val uscratch = 0x40
  val uepc = 0x41
  val ucause = 0x42
  val utval = 0x43
  val uip = 0x44
  val cycle = 0xc00
  val time = 0xc01
  val instret = 0xc02
  val hpmcounter3 = 0xc03
  val hpmcounter4 = 0xc04
  val hpmcounter5 = 0xc05
  val hpmcounter6 = 0xc06
  val hpmcounter7 = 0xc07
  val hpmcounter8 = 0xc08
  val hpmcounter9 = 0xc09
  val hpmcounter10 = 0xc0a
  val hpmcounter11 = 0xc0b
  val hpmcounter12 = 0xc0c
  val hpmcounter13 = 0xc0d
  val hpmcounter14 = 0xc0e
  val hpmcounter15 = 0xc0f
  val hpmcounter16 = 0xc10
  val hpmcounter17 = 0xc11
  val hpmcounter18 = 0xc12
  val hpmcounter19 = 0xc13
  val hpmcounter20 = 0xc14
  val hpmcounter21 = 0xc15
  val hpmcounter22 = 0xc16
  val hpmcounter23 = 0xc17
  val hpmcounter24 = 0xc18
  val hpmcounter25 = 0xc19
  val hpmcounter26 = 0xc1a
  val hpmcounter27 = 0xc1b
  val hpmcounter28 = 0xc1c
  val hpmcounter29 = 0xc1d
  val hpmcounter30 = 0xc1e
  val hpmcounter31 = 0xc1f
  val vl = 0xc20
  val vtype = 0xc21
  val vlenb = 0xc22
  val sstatus = 0x100
  val sie = 0x104
  val stvec = 0x105
  val scounteren = 0x106
  val senvcfg = 0x10a
  val sscratch = 0x140
  val sepc = 0x141
  val scause = 0x142
  val stval = 0x143
  val sip = 0x144
  val satp = 0x180
  val vsstatus = 0x200
  val vsie = 0x204
  val vstvec = 0x205
  val vsscratch = 0x240
  val vsepc = 0x241
  val vscause = 0x242
  val vstval = 0x243
  val vsip = 0x244
  val vsatp = 0x280
  val hstatus = 0x600
  val hedeleg = 0x602
  val hideleg = 0x603
  val hcounteren = 0x606
  val hgatp = 0x680
  val utvt = 0x7
  val unxti = 0x45
  val uintstatus = 0x46
  val uscratchcsw = 0x48
  val uscratchcswl = 0x49
  val stvt = 0x107
  val snxti = 0x145
  val sintstatus = 0x146
  val sscratchcsw = 0x148
  val sscratchcswl = 0x149
  val mtvt = 0x307
  val menvcfg = 0x30a
  val mnxti = 0x345
  val mintstatus = 0x346
  val mscratchcsw = 0x348
  val mscratchcswl = 0x349
  val mstatus = 0x300
  val misa = 0x301
  val medeleg = 0x302
  val mideleg = 0x303
  val mie = 0x304
  val mtvec = 0x305
  val mcounteren = 0x306
  val mscratch = 0x340
  val mepc = 0x341
  val mcause = 0x342
  val mtval = 0x343
  val mip = 0x344
  val pmpcfg0 = 0x3a0
  val pmpcfg1 = 0x3a1
  val pmpcfg2 = 0x3a2
  val pmpcfg3 = 0x3a3
  val pmpcfg4 = 0x3a4
  val pmpcfg5 = 0x3a5
  val pmpcfg6 = 0x3a6
  val pmpcfg7 = 0x3a7
  val pmpcfg8 = 0x3a8
  val pmpcfg9 = 0x3a9
  val pmpcfg10 = 0x3aa
  val pmpcfg11 = 0x3ab
  val pmpcfg12 = 0x3ac
  val pmpcfg13 = 0x3ad
  val pmpcfg14 = 0x3ae
  val pmpcfg15 = 0x3af
  val pmpaddr0 = 0x3b0
  val pmpaddr1 = 0x3b1
  val pmpaddr2 = 0x3b2
  val pmpaddr3 = 0x3b3
  val pmpaddr4 = 0x3b4
  val pmpaddr5 = 0x3b5
  val pmpaddr6 = 0x3b6
  val pmpaddr7 = 0x3b7
  val pmpaddr8 = 0x3b8
  val pmpaddr9 = 0x3b9
  val pmpaddr10 = 0x3ba
  val pmpaddr11 = 0x3bb
  val pmpaddr12 = 0x3bc
  val pmpaddr13 = 0x3bd
  val pmpaddr14 = 0x3be
  val pmpaddr15 = 0x3bf
  val pmpaddr16 = 0x3c0
  val pmpaddr17 = 0x3c1
  val pmpaddr18 = 0x3c2
  val pmpaddr19 = 0x3c3
  val pmpaddr20 = 0x3c4
  val pmpaddr21 = 0x3c5
  val pmpaddr22 = 0x3c6
  val pmpaddr23 = 0x3c7
  val pmpaddr24 = 0x3c8
  val pmpaddr25 = 0x3c9
  val pmpaddr26 = 0x3ca
  val pmpaddr27 = 0x3cb
  val pmpaddr28 = 0x3cc
  val pmpaddr29 = 0x3cd
  val pmpaddr30 = 0x3ce
  val pmpaddr31 = 0x3cf
  val pmpaddr32 = 0x3d0
  val pmpaddr33 = 0x3d1
  val pmpaddr34 = 0x3d2
  val pmpaddr35 = 0x3d3
  val pmpaddr36 = 0x3d4
  val pmpaddr37 = 0x3d5
  val pmpaddr38 = 0x3d6
  val pmpaddr39 = 0x3d7
  val pmpaddr40 = 0x3d8
  val pmpaddr41 = 0x3d9
  val pmpaddr42 = 0x3da
  val pmpaddr43 = 0x3db
  val pmpaddr44 = 0x3dc
  val pmpaddr45 = 0x3dd
  val pmpaddr46 = 0x3de
  val pmpaddr47 = 0x3df
  val pmpaddr48 = 0x3e0
  val pmpaddr49 = 0x3e1
  val pmpaddr50 = 0x3e2
  val pmpaddr51 = 0x3e3
  val pmpaddr52 = 0x3e4
  val pmpaddr53 = 0x3e5
  val pmpaddr54 = 0x3e6
  val pmpaddr55 = 0x3e7
  val pmpaddr56 = 0x3e8
  val pmpaddr57 = 0x3e9
  val pmpaddr58 = 0x3ea
  val pmpaddr59 = 0x3eb
  val pmpaddr60 = 0x3ec
  val pmpaddr61 = 0x3ed
  val pmpaddr62 = 0x3ee
  val pmpaddr63 = 0x3ef
  val tselect = 0x7a0
  val tdata1 = 0x7a1
  val tdata2 = 0x7a2
  val tdata3 = 0x7a3
  val dcsr = 0x7b0
  val dpc = 0x7b1
  val dscratch0 = 0x7b2
  val dscratch1 = 0x7b3
  val mseccfg = 0x747
  val mcycle = 0xb00
  val minstret = 0xb02
  val mhpmcounter3 = 0xb03
  val mhpmcounter4 = 0xb04
  val mhpmcounter5 = 0xb05
  val mhpmcounter6 = 0xb06
  val mhpmcounter7 = 0xb07
  val mhpmcounter8 = 0xb08
  val mhpmcounter9 = 0xb09
  val mhpmcounter10 = 0xb0a
  val mhpmcounter11 = 0xb0b
  val mhpmcounter12 = 0xb0c
  val mhpmcounter13 = 0xb0d
  val mhpmcounter14 = 0xb0e
  val mhpmcounter15 = 0xb0f
  val mhpmcounter16 = 0xb10
  val mhpmcounter17 = 0xb11
  val mhpmcounter18 = 0xb12
  val mhpmcounter19 = 0xb13
  val mhpmcounter20 = 0xb14
  val mhpmcounter21 = 0xb15
  val mhpmcounter22 = 0xb16
  val mhpmcounter23 = 0xb17
  val mhpmcounter24 = 0xb18
  val mhpmcounter25 = 0xb19
  val mhpmcounter26 = 0xb1a
  val mhpmcounter27 = 0xb1b
  val mhpmcounter28 = 0xb1c
  val mhpmcounter29 = 0xb1d
  val mhpmcounter30 = 0xb1e
  val mhpmcounter31 = 0xb1f
  val mcountinhibit = 0x320
  val mhpmevent3 = 0x323
  val mhpmevent4 = 0x324
  val mhpmevent5 = 0x325
  val mhpmevent6 = 0x326
  val mhpmevent7 = 0x327
  val mhpmevent8 = 0x328
  val mhpmevent9 = 0x329
  val mhpmevent10 = 0x32a
  val mhpmevent11 = 0x32b
  val mhpmevent12 = 0x32c
  val mhpmevent13 = 0x32d
  val mhpmevent14 = 0x32e
  val mhpmevent15 = 0x32f
  val mhpmevent16 = 0x330
  val mhpmevent17 = 0x331
  val mhpmevent18 = 0x332
  val mhpmevent19 = 0x333
  val mhpmevent20 = 0x334
  val mhpmevent21 = 0x335
  val mhpmevent22 = 0x336
  val mhpmevent23 = 0x337
  val mhpmevent24 = 0x338
  val mhpmevent25 = 0x339
  val mhpmevent26 = 0x33a
  val mhpmevent27 = 0x33b
  val mhpmevent28 = 0x33c
  val mhpmevent29 = 0x33d
  val mhpmevent30 = 0x33e
  val mhpmevent31 = 0x33f
  val mvendorid = 0xf11
  val marchid = 0xf12
  val mimpid = 0xf13
  val mhartid = 0xf14
  val mconfigptr = 0xf15
  val cycleh = 0xc80
  val timeh = 0xc81
  val instreth = 0xc82
  val hpmcounter3h = 0xc83
  val hpmcounter4h = 0xc84
  val hpmcounter5h = 0xc85
  val hpmcounter6h = 0xc86
  val hpmcounter7h = 0xc87
  val hpmcounter8h = 0xc88
  val hpmcounter9h = 0xc89
  val hpmcounter10h = 0xc8a
  val hpmcounter11h = 0xc8b
  val hpmcounter12h = 0xc8c
  val hpmcounter13h = 0xc8d
  val hpmcounter14h = 0xc8e
  val hpmcounter15h = 0xc8f
  val hpmcounter16h = 0xc90
  val hpmcounter17h = 0xc91
  val hpmcounter18h = 0xc92
  val hpmcounter19h = 0xc93
  val hpmcounter20h = 0xc94
  val hpmcounter21h = 0xc95
  val hpmcounter22h = 0xc96
  val hpmcounter23h = 0xc97
  val hpmcounter24h = 0xc98
  val hpmcounter25h = 0xc99
  val hpmcounter26h = 0xc9a
  val hpmcounter27h = 0xc9b
  val hpmcounter28h = 0xc9c
  val hpmcounter29h = 0xc9d
  val hpmcounter30h = 0xc9e
  val hpmcounter31h = 0xc9f
  val mcycleh = 0xb80
  val minstreth = 0xb82
  val mhpmcounter3h = 0xb83
  val mhpmcounter4h = 0xb84
  val mhpmcounter5h = 0xb85
  val mhpmcounter6h = 0xb86
  val mhpmcounter7h = 0xb87
  val mhpmcounter8h = 0xb88
  val mhpmcounter9h = 0xb89
  val mhpmcounter10h = 0xb8a
  val mhpmcounter11h = 0xb8b
  val mhpmcounter12h = 0xb8c
  val mhpmcounter13h = 0xb8d
  val mhpmcounter14h = 0xb8e
  val mhpmcounter15h = 0xb8f
  val mhpmcounter16h = 0xb90
  val mhpmcounter17h = 0xb91
  val mhpmcounter18h = 0xb92
  val mhpmcounter19h = 0xb93
  val mhpmcounter20h = 0xb94
  val mhpmcounter21h = 0xb95
  val mhpmcounter22h = 0xb96
  val mhpmcounter23h = 0xb97
  val mhpmcounter24h = 0xb98
  val mhpmcounter25h = 0xb99
  val mhpmcounter26h = 0xb9a
  val mhpmcounter27h = 0xb9b
  val mhpmcounter28h = 0xb9c
  val mhpmcounter29h = 0xb9d
  val mhpmcounter30h = 0xb9e
  val mhpmcounter31h = 0xb9f
}