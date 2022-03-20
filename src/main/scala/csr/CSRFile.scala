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
import freechips.rocketchip.rocket.{CSR, Causes}
import scala.collection.mutable.LinkedHashMap
import matrix.common._
import matrix.utils._


class Interrupts extends Bundle {
  val valid = Bool()
  val debug = Bool()
  val mtip  = Bool()
  val msip  = Bool()
  val meip  = Bool()
  val seip  = Bool()
}

class CsrFileReadWriteIO(implicit p: Parameters) extends MatrixBundle {
  val read  = Input(Bool())
  val write = Input(Bool())
  val addr  = Input(UInt(CSR_ADDR_SZ.W))
  val rdata = Output(UInt(MXLEN.W))
  val exc   = Output(UInt(EXLEN.W))
  val wdata = Input(UInt(MXLEN.W))
}

class CsrFileException(implicit p: Parameters) extends MatrixBundle {
  val ecall   = Bool()
  val ebrk    = Bool()
  val mret    = Bool()
  val sret    = Bool()
  val dret    = Bool()
  val wfi     = Bool()
  val sfence  = Bool()

  val interrupts  = new Interrupts
  val exception   = Bool()
  val cause       = UInt(EXLEN.W)
  val pc          = UInt(XLEN.W)
  val tval        = UInt(XLEN.W)
}

class CsrFilesIO(implicit p: Parameters) extends MatrixBundle  {
  val rw = new CsrFileReadWriteIO
  val exception = Input(new CsrFileException)

  val trap_kill = Output(Bool())
  val trap_addr = Output(UInt(vaddrWidth.W))

  val prv = Output(UInt(PRV.SZ.W))
  val satp = Output(new Satp)
  val pmps = Output(Vec(numOfPMPs, new PMPReg))
  val stall = Output(Bool())

  val status = Output(new MStatus)

  val retire = Flipped(Valid(Vec(retireWidth, Bool())))

  val set_fs_dirty = Input(Bool())
  val fcsr_fflags = Flipped(Valid(UInt(5.W)))
  val fcsr_rm = Output(UInt(3.W))
}

class CsrFiles(implicit p: Parameters) extends MatrixModule {
  val io = IO(new CsrFilesIO)

  //===========================================
  //  Field: Machine level
  //===========================================
  def isaStringToMask(s: String) = s.map(c => 1 << (c - 'A')).foldLeft(0)(_|_)
  val support_isa     = "U" + "S" + "P" + "M" + "I" + "F" + "D" + "C" + "A"
  val misa_reset      = isaStringToMask(support_isa)
  val reg_misa        = RegInit(UInt(MXLEN.W), Cat(2.U(2.W), 0.U((XLEN - 28).W), misa_reset.U(26.W)))

  val read_mvendorid   = WireInit(0.U(MXLEN.W))
  val read_marchid     = WireInit(0.U(MXLEN.W))
  val read_mimpid      = WireInit(0.U(MXLEN.W))
  val read_mhartid     = WireInit(0.U(MXLEN.W))

  val mstatus_reset   = WireInit(0.U.asTypeOf(new MStatus))
  mstatus_reset.mpp   := PRV.M
  mstatus_reset.sxl   := 2.U
  mstatus_reset.uxl   := 2.U

  val reg_mstatus     = RegInit(new MStatus, mstatus_reset)

  val reg_mtvec = Reg(new MTvec)

  val reg_medeleg = Reg(UInt(MXLEN.W))
  val read_medeleg = Wire(UInt(MXLEN.W))
  val reg_mideleg = Reg(UInt(MXLEN.W))
  val read_mideleg = Wire(UInt(MXLEN.W))

  val mip_reset = WireInit(0.U.asTypeOf(new Mip))
  val reg_mip = RegInit(new Mip, mip_reset)
  val mip = WireInit(reg_mip)
  val read_mip = Wire(UInt(MXLEN.W))
  val mie_reset = WireInit(0.U.asTypeOf(new Mie))
  val reg_mie = RegInit(new Mie, mie_reset)

  val reg_mcycle          = RegInit(UInt(MXLEN.W), 0.U)
  val reg_minstret        = RegInit(UInt(MXLEN.W), 0.U)
  val reg_mhpmcounters    = Reg(Vec(numOfHPMs, UInt(MXLEN.W)))
  val reg_mhpmevents      = Reg(Vec(numOfHPMs, UInt(MXLEN.W)))
  val reg_mcounteren      = RegInit(UInt(MXLEN.W), 0.U)
  val read_mcounteren     = Wire(UInt(MXLEN.W))
  val reg_mcountinhibit   = RegInit(UInt(MXLEN.W), 0.U)

  val reg_mscratch = RegInit(UInt(MXLEN.W), 0.U)

  val reg_mepc = RegInit(UInt(MXLEN.W), 0.U)
  val reg_mcause = RegInit(UInt(MXLEN.W), 0.U)
  val reg_mtval = RegInit(UInt(MXLEN.W), 0.U)

  val reg_mconfigptr = RegInit(UInt(MXLEN.W), 0.U)

  val menvcfg_reset = WireInit(0.U.asTypeOf(new Menvcfg))
  val reg_menvcfg = RegInit(new Menvcfg, menvcfg_reset)

  val mseccfg_reset = WireInit(0.U.asTypeOf(new Mseccfg))
  val reg_mseccfg = RegInit(new Mseccfg, mseccfg_reset)

  val reg_mtime = RegInit(UInt(MXLEN.W), 0.U)
  val reg_mtimecmp = RegInit(UInt(MXLEN.W), 0.U)

  val reg_pmps = Reg(Vec(numOfPMPs, new PMPReg))

  val reg_v = RegInit(Bool(), false.B)
  val reg_priv = RegInit(UInt(PRV.SZ.W), PRV.M)
  val reg_wfi = RegInit(Bool(), false.B)

  val mreg_list = LinkedHashMap[Int, UInt](
    CSRs.misa           -> reg_misa,
    CSRs.mvendorid      -> read_mvendorid,
    CSRs.marchid        -> read_marchid,
    CSRs.mimpid         -> read_mimpid,
    CSRs.mhartid        -> read_mhartid,
    CSRs.mstatus        -> reg_mstatus.asUInt,
    CSRs.mtvec          -> reg_mtvec.asUInt,
    CSRs.medeleg        -> read_medeleg,
    CSRs.mideleg        -> read_mideleg,
    CSRs.mip            -> read_mip,
    CSRs.mie            -> reg_mie.asUInt,
    CSRs.mcycle         -> reg_mcycle,
    CSRs.minstret       -> reg_minstret,
    CSRs.mhpmevent5     -> reg_mhpmevents(0),
    CSRs.mcounteren     -> read_mcounteren,
    CSRs.mcountinhibit  -> reg_mcountinhibit,
    CSRs.mscratch       -> reg_mscratch,
    CSRs.mepc           -> reg_mepc,
    CSRs.mcause         -> reg_mcause,
    CSRs.mtval          -> reg_mtval,
    CSRs.mconfigptr     -> reg_mconfigptr,
    CSRs.menvcfg        -> reg_menvcfg.asUInt,
    CSRs.mseccfg        -> reg_mseccfg.asUInt,
  )
  for (i <- 0 until numOfPMPs) {
    val pmpcfgs = Cat(reg_pmps(i).pmpcfg.reverse.map(_.asUInt))
    mreg_list += (CSRs.pmpcfg0 + (XLEN/32) * i) -> pmpcfgs
  }
  val pmpaddrs = reg_pmps.map(_.pmpaddr).flatten
  for (i <- 0 until numOfPMPs * numOfPMPConfigs) {
    mreg_list += (CSRs.pmpaddr0 + i) -> pmpaddrs(i).asUInt
  }
  for (i <- 0 until numOfPerfCounters) {
    mreg_list += (CSRs.mhpmcounter3 + i) -> reg_mhpmcounters(i)
    mreg_list += (CSRs.mhpmevent3 + i) -> reg_mhpmevents(i)
  }

  //===========================================
  //  Field: Supervisor level
  //===========================================
  val stvec_reset = WireInit(0.U.asTypeOf(new Stvec))
  val reg_stvec = RegInit(new Stvec, stvec_reset)

  val reg_scounteren = RegInit(UInt(SXLEN.W), 0.U)
  val read_scounteren = WireInit(UInt(SXLEN.W), 0.U)
  val reg_sscratch = RegInit(UInt(SXLEN.W), 0.U)

  val read_sip = read_mip & read_mideleg
  val reg_sepc = RegInit(UInt(SXLEN.W), 0.U)
  val reg_scause = RegInit(UInt(SXLEN.W), 0.U)
  val reg_stval = RegInit(UInt(SXLEN.W), 0.U)

  val senvcfg_reset = WireInit(0.U.asTypeOf(new Senvcfg))
  val reg_senvcfg = RegInit(new Senvcfg, senvcfg_reset)

  val satp_reset = WireInit(0.U.asTypeOf(new Satp))
  val reg_satp = RegInit(new Satp, satp_reset)
  val read_sstatus = WireInit(0.U.asTypeOf(new MStatus))
  read_sstatus.sd     := reg_mstatus.sd
  read_sstatus.uxl    := reg_mstatus.uxl
  read_sstatus.mxr    := reg_mstatus.mxr
  read_sstatus.sum    := reg_mstatus.sum
  read_sstatus.xs     := reg_mstatus.xs
  read_sstatus.vs     := reg_mstatus.vs
  read_sstatus.spp    := reg_mstatus.spp
  read_sstatus.ube    := reg_mstatus.ube
  read_sstatus.spie   := reg_mstatus.spie
  read_sstatus.sie    := reg_mstatus.sie

  val sreg_list = LinkedHashMap[Int, UInt] (
    CSRs.sstatus    -> read_sstatus.asUInt,
    CSRs.stvec      -> reg_stvec.asUInt,
    CSRs.scounteren -> read_scounteren,
    CSRs.sscratch   -> reg_sscratch,
    CSRs.stval      -> reg_stval,
    CSRs.senvcfg    -> reg_senvcfg.asUInt,
    CSRs.satp       -> reg_satp.asUInt,
    CSRs.sip        -> read_sip,
    CSRs.sepc       -> reg_sepc,
    CSRs.scause     -> reg_scause,
  )

  //===========================================
  //  Field: Debug
  //===========================================
  val reg_debug = Reg(Bool())
  val dcsr_reset = WireInit(0.U.asTypeOf(new DCSR))
  dcsr_reset.debugver := 1.U
  dcsr_reset.prv := PRV.M
  val reg_dcsr = RegInit(new DCSR, dcsr_reset)
  val reg_dscratch0 = RegInit(UInt(XLEN.W), 0.U)
  val reg_dscratch1 = RegInit(UInt(XLEN.W), 0.U)
  val reg_dpc = RegInit(UInt(vaddrWidth.W), 0.U)
  //  XXX:  Not sure that really need these registers
  //  val reg_tselect = RegInit(UInt(XLEN.W), 0.U)
  //  val reg_tdata1 = RegInit(UInt(XLEN.W), 0.U)
  //  val reg_tdata2 = RegInit(UInt(XLEN.W), 0.U)
  //  val reg_bps = Reg(Vec(nBreakpoints))

  val dreg_list = LinkedHashMap[Int, UInt] (
    CSRs.dcsr       -> reg_dcsr.asUInt,
    CSRs.dscratch0  -> reg_dscratch0,
    CSRs.dscratch1  -> reg_dscratch1,
    CSRs.dpc        -> reg_dpc,
    //  CSRs.tselect    -> reg_tselect,
    //  CSRs.tdata1     -> reg_tdata1,
    //  CSRs.tdata2     -> reg_tdata2
  )


  //===========================================
  //  Field: Fpu
  //===========================================
  val reg_fflags = RegInit(new FFlags, 0.U.asTypeOf(new FFlags))
  val reg_frm = RegInit(UInt(3.W), 0.U(3.W))
  val set_fs_dirty = WireInit(io.set_fs_dirty)

  val freg_list = LinkedHashMap[Int, UInt](
    CSRs.fflags     -> reg_fflags.asUInt,
    CSRs.frm        -> reg_frm,
    CSRs.fcsr       -> Cat(reg_frm, reg_fflags.asUInt)
  )

  //===========================================//
  //  Field: User
  //===========================================//
  val ureg_list = LinkedHashMap[Int, UInt] (
    CSRs.cycle      -> reg_mcycle,
    CSRs.instret    -> reg_minstret,
    CSRs.time       -> 0.U
  )

  //========================================================//

  when (set_fs_dirty) {
    reg_mstatus.fs := 3.U
  }
  when (io.fcsr_fflags.valid) {
    reg_fflags := (reg_fflags.asUInt | io.fcsr_fflags.bits).asTypeOf(new FFlags)
  }
  io.fcsr_rm := reg_frm

  //================== Interrupts ==========================//
  val support_mip = WireInit(0.U.asTypeOf(new Mip))
  support_mip.ssip := true.B
  support_mip.stip := true.B
  support_mip.seip := true.B
  val support_ints = support_mip.asUInt
  val delegate_mip = WireInit(support_mip)
  delegate_mip.msip := false.B
  delegate_mip.mtip := false.B
  delegate_mip.meip := false.B
  val delegate_ints = delegate_mip.asUInt

  val delegate_excp = Seq(
    Causes.misaligned_fetch,
    Causes.fetch_page_fault,
    Causes.breakpoint,
    Causes.load_page_fault,
    Causes.store_page_fault,
    Causes.misaligned_load,
    Causes.misaligned_store,
    Causes.illegal_instruction,
    Causes.user_ecall
  ).map(e => UIntToOH(e.U, MXLEN)).reduce(_|_)

  //========================================================//
  when (!io.stall && reg_mcountinhibit(0)) {
    reg_mcycle := reg_mcycle + 1.U
  }
  when (io.retire.valid && reg_mcountinhibit(2)) {
    reg_minstret := reg_minstret + PopCount(io.retire.bits)
  }

  val modify_kill = WireInit(false.B)

  //================= Read & Write =========================//
  val read_mapping = mreg_list ++ sreg_list ++ dreg_list ++ freg_list ++ ureg_list
  def lookup_addr = read_mapping map { case (k, v) => k -> (io.rw.addr === k.U) }
  io.rw.rdata := Mux1H(read_mapping map { case (k, v) => lookup_addr(k) -> v})

  read_medeleg := reg_medeleg & delegate_excp
  read_mideleg := reg_mideleg & delegate_ints
  read_mcounteren := ((1 << (numOfPerfCounters + 3)) - 1).U & reg_mcounteren
  read_scounteren := ((1 << (numOfPerfCounters + 3)) - 1).U & reg_scounteren
  read_mip := mip.asUInt & support_ints

  val pending_ints = read_mip & reg_mie.asUInt

  when (io.rw.write) {
    when (lookup_addr(CSRs.mstatus)) {
      val new_status = WireInit(io.rw.wdata).asTypeOf(new MStatus)
      reg_mstatus.mie     := new_status.mie
      reg_mstatus.mpie    := new_status.mpie
      reg_mstatus.mprv    := new_status.mprv
      reg_mstatus.mpp     := new_status.mpp
      reg_mstatus.mxr     := new_status.mxr
      reg_mstatus.sum     := new_status.sum
      reg_mstatus.spp     := new_status.spp
      reg_mstatus.spie    := new_status.spie
      reg_mstatus.sie     := new_status.sie
      reg_mstatus.tw      := new_status.tw
      reg_mstatus.tvm     := new_status.tvm
      reg_mstatus.tsr     := new_status.tsr

      reg_mstatus.fs      := new_status.fs
      reg_mstatus.vs      := 0.U
      reg_mstatus.sd      := new_status.fs.andR || new_status.xs.andR || new_status.vs.andR

      modify_kill := reg_mstatus.asUInt =/= new_status.asUInt
    }
    when (lookup_addr(CSRs.mip)) {
      val new_mip = io.rw.wdata.asTypeOf(new Mip)
      reg_mip.ssip := new_mip.ssip
      reg_mip.stip := new_mip.stip
      reg_mip.seip := new_mip.seip
    }
    when (lookup_addr(CSRs.mie))        { reg_mie := (io.rw.wdata & support_ints).asTypeOf(new Mie) }
    when (lookup_addr(CSRs.mepc))       {
      val new_mepc = io.rw.wdata & ~1.U
      reg_mepc := new_mepc
      modify_kill := reg_mepc =/= new_mepc
    }
    when (lookup_addr(CSRs.mscratch))   { reg_mscratch := io.rw.wdata }
    when (lookup_addr(CSRs.mtvec))      {
      val new_mtvec = io.rw.wdata.asTypeOf(new MTvec)
      reg_mtvec := new_mtvec
      modify_kill := reg_mtvec.asUInt =/= new_mtvec.asUInt
    }
    when (lookup_addr(CSRs.mcause))     { reg_mcause := io.rw.wdata }
    when (lookup_addr(CSRs.mtval))      { reg_mtval := io.rw.wdata }
    when (lookup_addr(CSRs.mcycle))     { reg_mcycle := io.rw.wdata }
    when (lookup_addr(CSRs.minstret))   { reg_minstret := io.rw.wdata }
    when (lookup_addr(CSRs.fflags) | lookup_addr(CSRs.fcsr)) {
      set_fs_dirty := true.B
      reg_fflags := io.rw.wdata(4, 0).asTypeOf(new FFlags)
    }
    when (lookup_addr(CSRs.frm) | lookup_addr(CSRs.fcsr)) {
      set_fs_dirty := true.B
      reg_frm := io.rw.wdata(7, 5)
    }
    when (lookup_addr(CSRs.dcsr)) {
      val new_dcsr = WireInit(io.rw.wdata).asTypeOf(new DCSR())
      reg_dcsr.step       := new_dcsr.step
      reg_dcsr.ebreakm    := new_dcsr.ebreakm
      reg_dcsr.ebreaks    := new_dcsr.ebreaks
      reg_dcsr.ebreaku    := new_dcsr.ebreaku
      reg_dcsr.ebreakvu   := false.B
      reg_dcsr.ebreakvs   := false.B
      reg_dcsr.prv        := new_dcsr.prv
    }
    when (lookup_addr(CSRs.dpc))        { reg_dpc := io.rw.wdata & ~1.U }
    when (lookup_addr(CSRs.dscratch0))  { reg_dscratch0 := io.rw.wdata }
    when (lookup_addr(CSRs.dscratch1))  { reg_dscratch1 := io.rw.wdata }
    when (lookup_addr(CSRs.sstatus))    {
      val new_sstatus = WireInit(io.rw.wdata).asTypeOf(new MStatus())
      reg_mstatus.sie     := new_sstatus.sie
      reg_mstatus.spie    := new_sstatus.spie
      reg_mstatus.spp     := new_sstatus.spp
      reg_mstatus.mxr     := new_sstatus.mxr
      reg_mstatus.sum     := new_sstatus.sum
      reg_mstatus.fs      := new_sstatus.fs
      reg_mstatus.vs      := 0.U
      modify_kill := new_sstatus.asUInt =/= read_sstatus.asUInt
    }
    when (lookup_addr(CSRs.sip)) {
      val new_sip = (WireInit(io.rw.wdata) & reg_mideleg | (read_mip  & ~read_mideleg)).asTypeOf(new Mip)
      reg_mip.ssip := new_sip.ssip
    }
    when (lookup_addr(CSRs.satp)) {
      val new_satp = WireInit(io.rw.wdata).asTypeOf(new Satp)
      when (isOneOf(new_satp.mode, Seq(0.U/* Bare */, 8.U /* Sv32 */ , 9.U/* Sv48 */, 10.U /* Sv57 */))) {
        reg_satp.mode := new_satp.mode
        reg_satp.ppn := new_satp.ppn
        reg_satp.asid := new_satp.asid
        modify_kill := new_satp.asUInt =/= reg_satp.asUInt
      } .otherwise {
        modify_kill := false.B
      }
    }
    when (lookup_addr(CSRs.sscratch))   { reg_sscratch := io.rw.wdata }
    when (lookup_addr(CSRs.sepc))       {
      val new_sepc = io.rw.wdata & ~1.U
      reg_sepc := new_sepc
      modify_kill := reg_sepc =/= new_sepc
    }
    when (lookup_addr(CSRs.stvec))      {
      val new_stvec = io.rw.wdata.asTypeOf(new Stvec)
      reg_stvec := new_stvec
      modify_kill := new_stvec.asUInt =/= reg_stvec.asUInt
    }
    when (lookup_addr(CSRs.scause))     { reg_scause := io.rw.wdata }
    when (lookup_addr(CSRs.stval))      { reg_stval := io.rw.wdata }
    when (lookup_addr(CSRs.mideleg))    { reg_mideleg := io.rw.wdata }
    when (lookup_addr(CSRs.medeleg))    { reg_medeleg := io.rw.wdata }
    when (lookup_addr(CSRs.scounteren)) { reg_scounteren := io.rw.wdata }
    when (lookup_addr(CSRs.mcounteren)) { reg_mcounteren := io.rw.wdata }
    // when (lookup_addr(CSRs.tselect))    { reg_tselect := io.rw.wdata }

    val pmp_cfgs = reg_pmps.map(_.pmpcfg).flatten :+ reg_pmps(0).pmpcfg(0)
    val pmp_addrs = reg_pmps.map(_.pmpaddr).flatten :+ reg_pmps(0).pmpaddr(0)
    val wdata = Seq.tabulate(numOfPMPConfigs) { i => io.rw.wdata(i * 8 + 7, i * 8) }
    for (i <- 0 until numOfPMPConfigs * numOfPMPs) {
      when (lookup_addr(CSRs.pmpcfg0 + (XLEN/32)*(i/8))) {
        when (!pmp_cfgs(i).locked) {
          val new_cfg = wdata(i % 8).asTypeOf(new PMPConfig)
          pmp_cfgs(i)     := new_cfg
          pmp_cfgs(i).w   := new_cfg.w && new_cfg.r
          pmp_cfgs(i).a   := Cat(new_cfg.a(1), new_cfg.a.orR)
        }
      }
      when (lookup_addr(CSRs.pmpaddr0 + i)) {
        when (!(pmp_cfgs(i).locked || (pmp_cfgs(i + 1).locked && pmp_cfgs(i + 1).tor))) {
          pmp_addrs(i) := io.rw.wdata.asTypeOf(new PMPAddr)
        }
      }
    }
    for (i <- 0 until numOfPerfCounters) {
      when (lookup_addr(CSRs.mhpmcounter3 + i)) {
        reg_mhpmcounters(i) := io.rw.wdata
      }
      when (lookup_addr(CSRs.mhpmevent3 + i)) {
        reg_mhpmevents(i) := io.rw.wdata
      }
    }
    when (lookup_addr(CSRs.mcountinhibit)) { reg_mcountinhibit := io.rw.wdata & ~2.U }
    // when (lookup_addr(CSRs.tselect)) { reg_tselect := io.rw.wdata }
  }
  //=======================================================//
  //  Set mip
  mip.meip := io.exception.interrupts.meip
  mip.msip := io.exception.interrupts.msip
  mip.mtip := io.exception.interrupts.mtip
  mip.seip := io.exception.interrupts.seip | reg_mip.seip

  io.satp := reg_satp
  io.prv  := reg_priv
  io.pmps := reg_pmps

  //====================== Exception ========================//
  val ecall = io.exception.ecall
  val ebrk = io.exception.ebrk
  val common_excp = io.exception.exception
  val exception = ecall | ebrk | common_excp
  val cause = Mux(ecall, reg_priv + Causes.user_ecall.U, Mux(ebrk, Causes.breakpoint.U, io.exception.cause))

  val debug_int = cause(XLEN - 1) && cause(XLEN - 2, 0) === CSR.debugIntCause.U
  val debug_trigger = cause(XLEN - 1) && cause(XLEN - 2, 0) === CSR.debugTriggerCause.U
  val debug_brk = Cat(reg_dcsr.ebreakvs, reg_dcsr.ebreakvu, reg_dcsr.ebreakm, reg_dcsr.ebreaks, reg_dcsr.ebreaku)(Cat(reg_v, reg_priv))
  val debug_breakpoint = !cause(XLEN - 1) && ebrk && debug_brk
  val debug_signlestep = reg_dcsr.step
  val gnerated_by_debug = debug_int | debug_trigger | debug_breakpoint | reg_debug

  val epc = io.exception.pc & ~1.U
  val delegate = reg_priv <= PRV.S && Mux(cause(XLEN - 1), reg_mideleg(EXLEN - 1, 0), reg_medeleg(EXLEN - 1, 0)).orR

  val debug_tvec = Mux(reg_debug, Mux(ebrk, 0x800.U, 0x808.U), 0x800.U)
  val common_tvec = {
    val tvec = Mux(delegate, reg_stvec.asUInt, reg_mtvec.asUInt).asTypeOf(new MTvec)
    Cat(tvec.base, 0.U(4.W)) + Mux(tvec.direct, 0.U, cause(EXLEN - 1, 0) << 2.U)
  }
  val tvec = Mux(gnerated_by_debug, debug_tvec, common_tvec)

  when (exception) {
    when (gnerated_by_debug) {
      when (!reg_debug) {
        reg_debug       := true.B
        reg_dcsr.cause  := Mux(debug_signlestep, 4.U, Mux(debug_int, 3.U, Mux(debug_trigger, 2.U, 1.U)))
        reg_dcsr.prv    := reg_priv
        reg_priv        := PRV.M
        reg_dpc         := epc
      } .otherwise {
        //  Already in debug mode
      }
    } .elsewhen (delegate) {
      reg_sepc            := epc
      reg_scause          := cause
      reg_stval           := io.exception.tval
      reg_mstatus.spie    := reg_mstatus.sie
      reg_mstatus.spp     := reg_priv
      reg_mstatus.sie     := false.B
      reg_priv            := PRV.S
    } .otherwise {
      reg_mepc            := epc
      reg_mcause          := cause
      reg_mtval           := io.exception.pc
      reg_mstatus.mpie    := reg_mstatus.mie
      reg_mstatus.mpp     := reg_priv
      reg_mstatus.mie     := false.B
      reg_priv            := PRV.M
    }
  }

  when (io.exception.wfi && !reg_dcsr.step && !reg_debug) {
    reg_wfi := true.B
  } .elsewhen (exception || pending_ints.orR || false.B) {
    reg_wfi := false.B
  }

  //====================== Return ========================//
  val return_from_m = io.exception.mret
  val return_from_s = io.exception.sret
  val return_from_debug = io.exception.dret

  val return_pc = WireInit(0.U(vaddrWidth.W))

  when (return_from_m) {
    reg_mstatus.mie     := reg_mstatus.mpie
    reg_mstatus.mpie    := true.B
    reg_mstatus.mpp     := PRV.U
    reg_priv            := reg_mstatus.mpp
    return_pc           := reg_mepc
  } .elsewhen (return_from_s) {
    reg_mstatus.sie     := reg_mstatus.spie
    reg_mstatus.spie    := true.B
    reg_mstatus.spp     := PRV.U
    reg_priv            := reg_mstatus.spp
    return_pc           := reg_sepc
  } .elsewhen (return_from_debug) {
    reg_priv            := reg_dcsr.prv
    reg_debug           := false.B
    return_pc           := reg_dpc
  }

  val modify_kill_addr = io.exception.pc + 4.U
  val ret = return_from_s | return_from_m | return_from_debug
  io.trap_kill := exception || modify_kill || ret
  io.trap_addr := Mux(exception, tvec, Mux(ret, return_pc, modify_kill_addr))(vaddrWidth - 1, 0)

  //========================== CSR exception =========================//
  val mcounteren_excp = ((lookup_addr(CSRs.cycle) & !reg_mcounteren(0)) |
    (lookup_addr(CSRs.time) & !reg_mcounteren(1)) |
    (lookup_addr(CSRs.instret) & !reg_mcounteren(2)) |
    {
      val map = for (i <- 0 until numOfPerfCounters) yield { lookup_addr(CSRs.mhpmcounter3 + i) -> !reg_mcounteren(i) }
      Mux1H(map)
    }) && (reg_priv <= PRV.S)
  io.rw.exc := Mux(mcounteren_excp, Causes.illegal_instruction.U, 0.U)
  //
  io.stall := reg_wfi
  io.status := reg_mstatus
}
