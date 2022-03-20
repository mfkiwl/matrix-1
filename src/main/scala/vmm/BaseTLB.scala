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
package matrix.vmm

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import matrix.common._
import matrix.csr._
import matrix.utils._

case class TLBParams (
                       numOfSets: Int = 256,
                       numOfWays: Int = 4
                     ) {
  def replacementPolicy = new PseudoLRUPolicyImpl(numOfWays)
}

class TLBException extends Bundle {
  val fetch = Bool()
  val amo   = Bool()
  val ld    = Bool()
  val st    = Bool()
}

class TLBMeta(implicit p: Parameters) extends MatrixBundle {
  val d       = Bool()
  val a       = Bool()
  val g       = Bool()
  val u       = Bool()
  val x       = Bool()
  val w       = Bool()
  val r       = Bool()
  val level   = UInt(log2Ceil(pgLevels).W)

  def superpage = level > 0.U
  def leaf    = (r || (x && !w)) && a
  def sr      = leaf && r
  def sw      = leaf && w && d
  def sx      = leaf && x
  def ur      = sr && u
  def uw      = sw && u
  def ux      = sx && u
}

class TLBReq(implicit p: Parameters) extends MatrixBundle {
  val pc      = UInt(vaddrWidth.W)
}

class TLBResp(implicit p: Parameters) extends MatrixBundle {
  val hit   = Bool()
  val ppn   = UInt(ppnWidth.W)
  val cause = UInt(EXLEN.W)
}

class TLBPTWIO(dataOn: Boolean = false)(implicit p: Parameters) extends MatrixBundle {
  val req   = Decoupled(new PTWReq(dataOn))
  val resp  = Flipped(Valid(new PTWResp))
  val satp  = Input(new Satp)
  val prv   = Input(UInt(PRV.SZ.W))
}

class TLBIO(implicit p: Parameters) extends MatrixBundle {
  val kill = Input(Bool())
  val sfence = Flipped(Decoupled(new SFenceReq))
  val req = Flipped(Valid(new TLBReq))
  val resp = Output(new TLBResp)
  val ptw = new TLBPTWIO
}
abstract class BaseTLB(params: TLBParams)(implicit p: Parameters) extends MatrixModule
  with MemoryOpConstants {
  val io = IO(new TLBIO)

  def numOfSets = params.numOfSets
  def numOfWays = params.numOfWays
  //============================================
  //  Name: Valid array
  //  Desc: ITLB valid array
  //  Size: 1
  //============================================
  val valid_array = Reg(Vec(numOfSets, Vec(numOfWays, Bool())))
  //============================================
  //  Name: Tag array
  //  Desc: ITLB tag array
  //  Size: Tag
  //============================================
  class TLBTag extends Bundle {
    val asid = UInt(ASIDLEN.W)
    val vpn  = UInt(vpnWidth.W)
  }
  val tag_array = Reg(Vec(numOfSets, Vec(numOfWays, new TLBTag)))
  //============================================
  //  Name: Meta array
  //  Desc: ITLB meta array
  //  Size: Meta
  //============================================
  val meta_array = SyncReadMem(numOfSets, Vec(numOfWays, new TLBMeta))
  //============================================
  //  Name: Data array
  //  Desc: ITLB data array
  //  Size: PPN bits
  //============================================
  val data_array = SyncReadMem(numOfSets, Vec(numOfWays, UInt(ppnWidth.W)))

  //  FSM
  val s_ready::s_request::s_wait::s_wait_invalidate::Nil = Enum(4)
  val state = RegInit(s_ready)
  val next_state = WireInit(state)

  def is_ready            = state === s_ready
  def is_request          = state === s_request
  def is_wait             = state === s_wait
  def is_wait_invalidate  = state === s_wait_invalidate

  //================================ Lookup ==================================//
  def computeIndexAndTag(pc: UInt, satp: Satp) = {
    val index = pc(11 + log2Ceil(numOfSets), 12)
    val tag = Wire(new TLBTag)
    tag.asid := satp.asid
    tag.vpn := pc(vaddrWidth - 1, 12)
    (index, tag)
  }
  def tlbHit(valid: Bool, asid: UInt, vpn: UInt, tag: TLBTag, levels: UInt, superpage: Bool) = {
    val readTag = Wire(new TLBTag)
    readTag.asid := asid
    readTag.vpn := vpn
    val res = Wire(Bool())
    res := false.B
    when (superpage) {
      var tagHit = valid
      for (j <- 0 until pgLevels) {
        val base = vpnWidth - (j + 1) * pgLevelWidth
        tagHit = tagHit & (levels < j.U || tag.vpn(base + pgLevelWidth - 1, base) === vpn(base + pgLevelWidth - 1, base))
      }
      res := tagHit && readTag.asid === tag.asid
    } .otherwise {
      res := valid && readTag.asid === tag.asid && readTag.vpn === tag.vpn
    }
    res
  }

  val kill = WireInit(io.kill)
  val (ridx, rtag) = computeIndexAndTag(io.req.bits.pc, io.ptw.satp)
  val s0_val = io.req.valid && is_ready && !kill
  val valids = valid_array(ridx)
  val tags = tag_array(ridx)
  val metas = meta_array.read(ridx, s0_val)
  val datas = data_array.read(ridx, s0_val)
  val s1_pc = RegEnable(io.req.bits.pc, s0_val)
  val (s1_idx, s1_tag) = computeIndexAndTag(s1_pc, io.ptw.satp)
  val vpn = s1_pc(vaddrWidth - 1, pgOffsetWidth)
  val asid = io.ptw.satp.asid
  val hit_vec = VecInit(valids zip tags zip metas map {
    case ((v, t), m) => tlbHit(v, asid, vpn, t, m.level, m.superpage)
  })
  val s1_hit_vec = RegEnable(hit_vec, s0_val)
  val hit_meta = Mux1H(hit_vec, metas)
  val hit_way = Mux1H(hit_vec, datas)
  val hit = s1_hit_vec.reduce(_|_)
  val miss = !hit
  io.resp.hit := hit | io.ptw.resp.valid
  io.resp.ppn := Mux(io.ptw.resp.valid, io.ptw.resp.bits.pte.ppn, hit_way)
  io.resp.cause := Mux(io.ptw.resp.valid, io.ptw.resp.bits.cause, 0.U)

  //================================ Refill ==================================//
  val refill_idx = s1_idx
  val refill_tag = s1_tag
  val pte = WireInit(io.ptw.resp.bits.pte)
  val has_exception = io.ptw.resp.bits.cause =/= 0.U
  val is_superpage = io.ptw.resp.bits.level > 0.U
  val do_refill = io.ptw.resp.valid && !has_exception && pte.v
  val sfence_refill = is_wait_invalidate
  val new_meta = WireInit(0.U.asTypeOf(new TLBMeta))
  val new_data = WireInit(0.U(ppnWidth.W))
  val replace_policy = params.replacementPolicy
  val invalid_way_oh = selectFirstN(Reverse(~Cat(s1_hit_vec)))
  val replace_way = Mux(invalid_way_oh.orR, OHToUInt(invalid_way_oh), replace_policy.getVictim)
  val refill_now = do_refill && !is_wait_invalidate
  when (refill_now) {
    new_meta.d     := pte.d
    new_meta.r     := pte.r
    new_meta.w     := pte.w
    new_meta.x     := pte.x
    new_meta.u     := pte.u
    new_meta.g     := pte.g
    new_meta.a     := pte.a
    new_meta.level := io.ptw.resp.bits.level
    new_data       := pte.ppn

    when (!has_exception) {
      valid_array(refill_idx)(replace_way) := true.B
      tag_array(refill_idx)(replace_way) := refill_tag
      meta_array.write(refill_idx, VecInit(Seq.fill(params.numOfWays){new_meta}), UIntToOH(replace_way).asBools)
      data_array.write(refill_idx, VecInit(Seq.fill(params.numOfWays){new_data}), UIntToOH(replace_way).asBools)
    }
  }

  //========================== Update replacement policy =========================//
  when (s0_val) {
    val touchWay = selectFirstN(Reverse(Cat(hit_vec)))
    replace_policy.touch(touchWay)
  } .elsewhen (do_refill) {
    replace_policy.touch(replace_way)
  }

  //================================ Invalidate ==================================//
  val sfence = WireInit(io.sfence.valid)
  val sfence_done = RegInit(Bool(), false.B)
  val kill_tag = Cat(io.sfence.bits.vpn, io.sfence.bits.asid)
  when (sfence) {
    for (s <- 0 until params.numOfSets) {
      /*
       * More details: The RISC-V Instruction Set Manual Volume II: Privileged Architecture
       * Page 76 (Document Version 20211203)
       * Scheme 0: rs1 == 0, rs2 == 0
       *  * Invalidate all entries.
       * Scheme 1: rs1 == 0, rs2 != 0
       *  * Invalidate entries with rs2 domain, except for entries containing global mapping.
       * Scheme 2: rs1 != 0, rs2 == 0
       *  * Invalidate all entries with rs1 domain
       * Scheme 3: rs1 != 0, rs2 != 0
       *  * Invalidate entries with rs1 and rs2 domain, except for entries containing global mapping.
       */
      when (io.sfence.bits.rs1 && io.sfence.bits.rs2) {
        //  Scheme 3
        for (((v, t), m) <- (valid_array(s) zip tag_array(s)) zip meta_array(s)) {
          when (kill_tag === Cat(t.vpn, t.asid) && !m.g) {
            v := false.B
          }
        }
      } .elsewhen (io.sfence.bits.rs1 && !io.sfence.bits.rs2) {
        //  Scheme 2
        for ((v, t) <- valid_array(s) zip tag_array(s)) {
          when (io.sfence.bits.vpn === t.vpn) {
            v := false.B
          }
        }
      } .elsewhen (!io.sfence.bits.rs1 && io.sfence.bits.rs2) {
        //  Scheme 1
        for (((v, t), m) <- (valid_array(s) zip tag_array(s)) zip meta_array(s)) {
          when (t.asid === io.sfence.bits.asid && !m.g) {
            v := false.B
          }
        }
      } .otherwise {
        //  Scheme 0
        for (v <- valid_array) {
          v.foreach(_ := false.B)
        }
      }
    }
    sfence_done := true.B
  } .otherwise {
    sfence_done := false.B
  }
  io.sfence.ready := sfence_done
  io.ptw.req.valid := is_request
  io.ptw.req.bits.addr := s1_pc
  //  io.ptw.req.bits.data := 0.U(XLEN.W)
  io.ptw.req.bits.cmd := M_XRD

  //================================= Control =================================//
  switch (state) {
    is (s_ready) {
      when (io.req.valid && miss) {
        //  TLB miss
        next_state := s_request
      }
    }
    is (s_request) {
      //  Request a new tlb entry
      when (sfence) {
        next_state := s_ready
      }
      when(io.ptw.req.ready) {
        //  Arbiter select ITLB channel
        next_state := Mux(sfence, s_wait_invalidate, s_wait)
      }
    }
    is (s_wait) {
      when (sfence) {
        next_state := s_wait_invalidate
      }
      when (io.ptw.resp.valid) {
        //  Response
        next_state := s_ready
      }
    }
    is (s_wait_invalidate) {
      when (io.ptw.resp.valid) {
        //  Response
        next_state := s_ready
      }
    }
  }
  when (kill) {
    next_state := s_ready
  } .otherwise {
    next_state := state
  }
  state := next_state
}