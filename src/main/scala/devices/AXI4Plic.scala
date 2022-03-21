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
package matrix.device

import chisel3._
import chisel3.util._
import matrix.bus.axi4._
import matrix.utils._

import scala.collection.mutable

object PLICConstants {
  val dataWidth = 32
  val ctrWidth = 16

  val maxDevices  = 1023
  val nDevices    = 51
  val maxHarts    = 15872
  val nHarts      = 1
  val idWidth     = log2Ceil(nDevices)
  val nPriority   = nDevices
  val prioWidth   = log2Ceil(nPriority + 1)


  //  Freedom E310 SoC mapping
  val plicBase            = 0x0c000000
  val priorityBase        = 0x4
  val pendingBase         = 0x1000
  val enableBase          = 0x2000
  val thresholdBase       = 0x200000
  val claimCompleteBase   = 0x200004

  def priorityOffset(source: Int):    Int = 0x4 * source
  def priorityAddr(source: Int):      Int = plicBase + priorityBase + priorityOffset(source)
  def enableOffset(hart: Int):        Int = hart * 0x80
  def enableAddr(hart: Int, w: Int):  Int = plicBase + enableBase + enableOffset(hart) + 4 * w
  def thresholdOffset(hart: Int):     Int = hart * 0x1000
  def thresholdAddr(hart: Int):       Int = plicBase + thresholdBase + thresholdOffset(hart)
  def pendingOffset(i: Int):          Int = i * 0x4
  def pendingAddr(i: Int):            Int = plicBase + pendingBase + pendingOffset(i)
  def claimCompleteOffset(hart: Int): Int = hart * 0x1000
  def claimCompleteAddr(hart: Int):   Int = plicBase + claimCompleteBase + claimCompleteOffset(hart)
}

class GateWayIO extends Bundle {
  val interrupt   = Input(Bool())
  val edge_level  = Input(Bool())

  val pending     = Output(Bool())
  val claim       = Input(Bool())
  val complete    = Input(Bool())
}

class GateWay extends Module {
  import PLICConstants._

  val io = IO(new GateWayIO)

  val int_req = RegInit(Bool(), false.B)
  val int_edge = RegInit(Bool(), false.B)

  int_req := io.interrupt
  int_edge := io.interrupt && !int_req

  val s_wait_resp::s_wait_claim::s_wait_complete::Nil = Enum(3)
  val state = RegInit(s_wait_resp)

  val pending_ctr = RegInit(UInt(ctrWidth.W), 0.U)
  val nxt_pending_ctr = Wire(UInt(ctrWidth.W))
  val pending_decr = RegInit(Bool(), false.B)

  pending_decr := false.B
  switch (state) {
    is (s_wait_resp) {
      when ((!io.edge_level && nxt_pending_ctr.orR) || (io.edge_level && io.interrupt)) {
        pending_decr := true.B
        state := s_wait_claim
      }
    }
    is (s_wait_claim) {
      when (io.claim) { state := s_wait_complete }
    }
    is (s_wait_complete) {
      when (io.complete) { state := s_wait_resp }
    }
  }
  io.pending := state === s_wait_claim

  //
  val pending_inc = !pending_decr && int_edge
  val pending_dec = pending_decr && !int_edge

  nxt_pending_ctr := updateCounter(pending_ctr, pending_inc & !pending_dec, ctrWidth)
  when (io.edge_level) {
    pending_ctr := 0.U
  } .elsewhen (pending_dec | pending_inc) {
    pending_ctr := nxt_pending_ctr
  }
}

class AXI4PlicCoreIO(nHarts: Int = PLICConstants.nHarts) extends Bundle {
  val interrupt   = Input(Bool())
  val pending     = Output(Bool())
  val enable      = Input(Vec(nHarts, Bool()))
  val priority    = Input(UInt(PLICConstants.prioWidth.W))
  val prioritys   = Output(Vec(nHarts, UInt(PLICConstants.prioWidth.W)))
  val id          = Output(Vec(nHarts, UInt(PLICConstants.idWidth.W)))
  val claim       = Input(UInt(nHarts.W))// Input(Vec(nHarts, Bool()))
  val complete    = Input(UInt(nHarts.W))// Input(Vec(nHarts, Bool()))
}

class AXI4PlicCore(id: Int) extends Module {
  val io = IO(new AXI4PlicCoreIO)

  import PLICConstants._

  val core_id = Reg(Vec(nHarts, UInt(PLICConstants.idWidth.W)))
  val prio = Reg(Vec(nHarts, UInt(PLICConstants.prioWidth.W)))

  val id_claimed = Reg(Vec(nHarts, UInt(PLICConstants.idWidth.W)))
  val claims = (0 until nHarts) map { h => {
    (io.id(h) === (h + 1).U) && io.claim(h)
  }}
  val completes = (0 until nHarts) map { h => {
    (id_claimed(h) === (h+1).U) && io.complete(h)
  }}

  id_claimed.zipWithIndex map { case (c, i) => c := Mux(io.claim(i), io.id(i), 0.U)}

  val gateway = Module(new GateWay)
  gateway.io.interrupt := io.interrupt
  gateway.io.edge_level := true.B //  Always be level trigger.
  gateway.io.claim := claims.reduce(_|_)
  gateway.io.complete := completes.reduce(_|_)

  for (h <- 0 until nHarts) {
    when (gateway.io.pending && io.enable(h)) {
      core_id(h) := id.U
      prio(h) := io.priority
    } .otherwise {
      core_id(h) := 0.U
      prio(h) := 0.U
    }
  }

  io.pending      := gateway.io.pending
  io.prioritys    := prio
  io.id           := core_id
}

class AXI4PlicMemoryMapRegionIO extends Bundle {
  import PLICConstants._
  val pending     = Input(Vec(nDevices, Bool()))
  val id          = Input(Vec(nHarts, UInt(idWidth.W)))
  val enable      = Output(Vec(nHarts, UInt(nDevices.W)))
  val priority    = Output(Vec(nDevices, UInt(prioWidth.W)))
  val threshold   = Output(Vec(nHarts, UInt(prioWidth.W)))
  val claim       = Output(UInt(nHarts.W))
  val complete    = Output(UInt(nHarts.W))
}

class AXI4PlicMemoryMapRegion(params: AXI4Params, e: AXI4PlicMemoryMapRegionIO = new AXI4PlicMemoryMapRegionIO) extends AXI4SlaveNode(params, e) {
  import PLICConstants._
  val io_extra = io.extra.getOrElse(new AXI4PlicMemoryMapRegionIO)
  //============================ Reg field ===========================//

  val first_enable = dataWidth
  val full_enables = if ((nDevices - first_enable) < 0) 0 else (nDevices - first_enable) / dataWidth
  val tail_enable = nDevices - dataWidth * full_enables - dataWidth
  val total_enables = 1 + full_enables + (if (tail_enable < 0) 0 else 1)
  val enableReg = Seq.fill(total_enables) { Reg(UInt(dataWidth.W)) }

  val enable = Seq.fill(nHarts) { enableReg }
  val priority = Reg(Vec(nDevices, UInt(prioWidth.W)))
  val threshold = Reg(Vec(nHarts, UInt(prioWidth.W)))

  val first_pending = dataWidth
  val full_pendings = if ((nDevices - first_pending) < 0) 0 else (nDevices - first_pending) / dataWidth
  val tail_pending = nDevices - dataWidth * full_pendings - dataWidth
  val total_pendings = 1 + full_pendings + (if (tail_pending < 0) 0 else 1)
  val pending = Wire(Vec(total_pendings, UInt(dataWidth.W)))
  val pending_group = io_extra.pending.grouped(dataWidth)
  for ((pend, i) <- pending_group.zipWithIndex) {
    val p = Reverse(Cat(pend))
    if (pend.size < dataWidth) {
      pending(i) := Cat(Fill(dataWidth - pend.size, 0.U), p)
    } else {
      pending(i) := p
    }
  }

  val hart_mapping = Seq.tabulate(nHarts) { hart => {
    val mapping = mutable.LinkedHashMap[Int, UInt] (
      thresholdAddr(hart) -> Cat(Fill(dataWidth - prioWidth, 0.U), threshold(hart))
    )
    val enable_mapping = mutable.LinkedHashMap[Int, UInt] ()
    for (e <- 0 until total_enables) {
      enable_mapping += (enableAddr(hart, e) -> enable(hart)(e))
    }

    enable_mapping ++ mapping
  }}

  val priority_mapping = mutable.LinkedHashMap[Int, UInt]()
  for (d <- 0 until nDevices)  {
    priority_mapping += priorityAddr(d) -> priority(d)
  }
  val pending_mapping = mutable.LinkedHashMap[Int, UInt]()
  for (d <- 0 until nDevices) {
    pending_mapping += pendingAddr(d/dataWidth) -> pending(d/dataWidth)
  }
  val claimCompleta_mapping = mutable.LinkedHashMap[Int, UInt] ()
  for (h <- 0 until nHarts) {
    claimCompleta_mapping += claimCompleteAddr(h) -> io_extra.id(h) //  Claim/Complete.
  }

  val mapping = hart_mapping.flatten.toMap ++ priority_mapping ++ pending_mapping ++ claimCompleta_mapping

  //============================= Write ==============================//
  def lookup_addr(addr: UInt) = mapping map { case (k, v) => k -> (addr === k.U) }
  def write_lookup_addr = lookup_addr(write_addr)
  def read_lookup_addr = lookup_addr(read_addr)

  val write_complete = WireInit(false.B)
  val complete = Reg(UInt(nHarts.W))

  when (wren) {
    for (h <- 0 until nHarts) {
      for (e <- 0 until total_enables) {
        when (write_lookup_addr(enableAddr(h, e))) {
          enable(h)(e) := ~Mux(e.U === 0.U, 1.U, 0.U) & io.axi.w.bits.data
        }
      }
      when (write_lookup_addr(thresholdAddr(h))) {
        threshold(h) := io.axi.w.bits.data(prioWidth - 1, 0)
      }
    }
    for (d <- 0 until nDevices) {
      when (write_lookup_addr(priorityAddr(d))) {
        priority(d) := io.axi.w.bits.data(prioWidth - 1, 0)
      }
    }
    val write_complete_valid = (0 until nHarts) map { h => {
      write_lookup_addr(claimCompleteAddr(h))
    }}
    write_complete := write_complete_valid.reduce(_|_)
    val which_complete = selectFirstN(Reverse(Cat(write_complete_valid)))
    when (write_complete) {
      complete := 1.U << which_complete
    } .otherwise {
      complete := 0.U
    }
  }
  b_resp := RegEnable(AXI4.RESP_OKAY.U, wren)
  //============================== Read ===============================//
  r_data := RegEnable(Mux1H(mapping map { case (k, v) => read_lookup_addr(k) -> v}), rden)
  val read_claim = WireInit(false.B)
  val read_claim_valid = (0 until nHarts) map { h => {
    read_lookup_addr(claimCompleteAddr(h))
  }}
  read_claim := read_claim_valid.reduce(_|_)
  val which_claim = selectFirstN(Reverse(Cat(read_claim_valid)))
  val claim = Reg(UInt(nHarts.W))
  claim := Mux(rden && read_claim, which_claim, 0.U)

  r_resp := RegEnable(AXI4.RESP_OKAY.U, rden)

  io_extra.threshold  := threshold
  io_extra.enable     := enable.map(h => {
    val devices = Cat(h.reverse)
    devices(nDevices, 1)
  })
  io_extra.priority   := priority
  io_extra.claim      := claim
  io_extra.complete   := complete
}


class AXI4PlicIO(params: AXI4Params) extends Bundle {
  val axi         = Flipped(new AXI4Bundle(params))
  val interrupts  = Input(Vec(PLICConstants.nDevices, Bool()))
  val int_reqs    = Output(Vec(PLICConstants.nHarts, Bool()))
}

class AXI4Plic(params: AXI4Params) extends Module {
  val io = IO(new AXI4PlicIO(params))
  import PLICConstants._

  val cores = Seq.tabulate(nDevices) { id => Module(new AXI4PlicCore(id + 1)) }
  val region = Module(new AXI4PlicMemoryMapRegion(params))

  val region_extra = region.io.extra.getOrElse(new AXI4PlicMemoryMapRegionIO)
  //  Find target id
  val int_req = Wire(Vec(nHarts, Bool()))
  val int_req_id = Reg(Vec(nHarts, UInt(idWidth.W)))

  for (h <- 0 until nHarts) {
    val hart_prio = cores.map(_.io.prioritys(h))
    val ids = cores.map(_.io.id(h))
    val sel = (hart_prio zip ids).foldLeft((0.U, 0.U))((prev, cur) => {
      val prio = Mux(prev._1 > cur._1, prev._1, cur._1)
      val idx = Mux(prev._1 > cur._1, prev._2, cur._2)
      (prio, idx)
    })._2
    int_req_id(h) := sel
    int_req(h) := region_extra.threshold(h) < VecInit(hart_prio)(sel)
  }
  region_extra.id := int_req_id
  region_extra.pending := cores.map(_.io.pending)

  for (d <- 0 until nDevices) {
    cores(d).io.enable      := region_extra.enable.map(_(d))
    cores(d).io.priority    := region_extra.priority(d)
    cores(d).io.claim       := region_extra.claim
    cores(d).io.complete    := region_extra.complete
  }
  cores.zipWithIndex.foreach(c => {
    val core = c._1
    core.io.interrupt := io.interrupts(c._2)
  })

  region.io.axi <> io.axi
  io.int_reqs := RegNext(int_req)
}