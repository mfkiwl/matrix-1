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
import matrix.utils._
import matrix.ifu._

case class CacheParams(
                        numOfSets: Int = 64,
                        numOfWays: Int = 16,
                        numOfBanks: Int = 4,
                        numOfMSHRHeaders: Int = 8,
                        numOfMSHRTableEntries: Int = 16,
                        lineBytes: Int = 64,
                        dCache: Boolean = false,
                        mshrOn: Boolean = true, // ICache must be false
                        dataWidth: Int = 128,
                        bufferSize: Int = 8
                      ) {
  def blockBytes = lineBytes / numOfBanks
  def lineBits = lineBytes * 8
  def blockBits = blockBytes * 8
  def replacementPolicy = new PseudoLRUPolicyImpl(numOfWays)
}


class CacheReq(dCache: Boolean = false, mshrOn: Boolean = false)(implicit p: Parameters) extends MatrixBundle
  with ScalarOpConstants
  with MemoryOpConstants {
  val rob_id    = if (dCache && mshrOn) UInt(robIdWidth.W) else null
  val ld_id     = if (dCache && mshrOn) UInt(ldqIdWidth.W) else null
  val pc        = UInt(vaddrWidth.W)           
  val data      = if (dCache) UInt() else null    //  For write
  val mask      = if (dCache) UInt() else null    //  For write
  val cmd       = UInt(M_SZ.W)                  
  val ppn       = UInt(ppnWidth.W)              
}

class CacheResp(implicit p: Parameters) extends MatrixBundle {
  val hit   = Bool()      
  val data  = UInt()      
}

class CachePTWReq(dataOn: Boolean = false)(implicit p: Parameters) extends PTWReq(dataOn = dataOn)
class CachePTWResp(dataWidth: Int)(implicit p: Parameters) extends MatrixBundle {
  val data  = UInt(dataWidth.W)         
  val cause = UInt(EXLEN.W)             
}

class CachePTWIO(dCache: Boolean = false, dataWidth: Int)(implicit p: Parameters) extends MatrixBundle {
  val req   = Decoupled(new CachePTWReq)                      
  val resp  = Flipped(Valid(new CachePTWResp(dataWidth)))       
}

class CacheIO(dCache: Boolean = false, mshrOn: Boolean = true, dataWidth: Int)(implicit p: Parameters) extends MatrixBundle {
  val kill          = Input(new KillReq)
  val req           = Flipped(Valid(new CacheReq(dCache)))                 
  val resp          = Output(new CacheResp)                           
  val ptw           = new CachePTWIO(dCache, dataWidth)             
  val stall         = Input(Bool())                                  
  val write_miss    = Output(Bool())
  val no_space      = Output(Bool())
  val invalidate    = Input(Bool())                                  
  val ld_id         = if (dCache && mshrOn) Output(UInt(ldqIdWidth.W)) else null
}

abstract class BaseCache(params: CacheParams)(implicit p: Parameters) extends MatrixModule
  with MemoryOpConstants {
  val io = IO(new CacheIO(params.dCache, params.mshrOn, params.lineBits))
  
  def BANK_LSB  = log2Ceil(params.blockBytes)
  def BANK_MSB  = BANK_LSB + log2Ceil(params.numOfBanks) - 1
  def INDEX_LSB = BANK_MSB + 1
  def INDEX_MSB = INDEX_LSB + log2Ceil(params.numOfSets) - 1
  def mshrOn    = params.dCache && params.mshrOn

  //==============================================//
  //  Name: Valid array                           //
  //  Desc: Cacche valid array                    //
  //  Size: 1                                     //
  //==============================================//
  val valid_array = Reg(Vec(params.numOfSets, Vec(params.numOfWays, Bool())))
  //==============================================//
  //  Name: Tag array                             //
  //  Desc: ICache tag.                           //
  //  Size: paddrBits - pgOffsetBits              //
  //==============================================//
  val tagSz = paddrWidth - pgOffsetWidth
  val tag_array = Reg(Vec(params.numOfSets, Vec(params.numOfWays, UInt(tagSz.W))))
  //==============================================//
  //  Name: Data array                            //
  //  Desc: 4 banks each way                      //
  //  Size: 64 bytes                              //
  //==============================================//
  val data_array = SyncReadMem(params.numOfSets
    , Vec(params.numOfWays, UInt(params.lineBits.W)))
  //  MSHR
  val mshr = if (mshrOn) {
    Module(new MSHR(params.numOfMSHRHeaders, params.numOfMSHRTableEntries))
  } else {
    null
  }


  class CacheMeta extends Bundle {
    val valid = Bool()
    val addr = UInt((vaddrWidth - log2Ceil(params.lineBytes)).W)
    val data = UInt(params.lineBits.W)
  }

  val write_queue = if (params.dCache) Reg(Vec(params.bufferSize, new CacheMeta)) else null
  val write_head = if (params.dCache) RegInit(UInt(log2Ceil(params.bufferSize).W), 0.U) else null
  val write_tail = if (params.dCache) RegInit(UInt(log2Ceil(params.bufferSize).W), 0.U) else null

  //  Replacement policy
  val replacePolicy = params.replacementPolicy
  val invalidate = WireInit(io.invalidate)
  val enable = WireInit(!io.stall)

  
  //  Lookup
  def computeIndexAndTag(pc: UInt) = {
    val index = pc(INDEX_MSB, INDEX_LSB)
    val tag = io.req.bits.ppn
    (index, tag)
  }

  def computeBankIndex(pc: UInt) = {
    val bankIndex = if (BANK_MSB > BANK_LSB) {
      UIntToOH(pc(BANK_MSB, BANK_LSB))
    } else {
      true.B
    }
    bankIndex
  }

  //======================= Stage 1 ==============================//
  val (s1_idx, s1_tag) = computeIndexAndTag(io.req.bits.pc)
  val s1_val = io.req.valid
  val write_enable = RegEnable(s1_val && io.req.bits.cmd === M_XWR, enable)
  val read_enable = RegEnable(s1_val && io.req.bits.cmd === M_XRD, enable)
  val s2_val = RegEnable(s1_val, enable)
  val s2_rob_id = if (mshrOn) RegEnable(io.req.bits.rob_id, enable) else null
  val s2_ld_id = if (mshrOn) RegEnable(io.req.bits.ld_id, enable) else null
  val s2_pc = RegEnable(io.req.bits.pc, enable)
  val s2_bank_idx = computeBankIndex(s2_pc)
  val (s2_idx, s2_tag) = computeIndexAndTag(s2_pc)
  val s2_offset = s2_pc(BANK_LSB - 1, 0)
  val s2_mask = if (params.dCache) RegEnable(io.req.bits.mask, enable) else null
  val s2_data = if (params.dCache) RegEnable(io.req.bits.data, enable) else null
  val valids = RegEnable(valid_array(s1_idx), s1_val && enable)
  val tags = RegEnable(tag_array(s1_idx), s1_val && enable)
  val datas = data_array.read(s1_idx, s1_val && enable)
  val data_slices = datas.map(d =>
    Seq.tabulate(params.numOfBanks) {i =>
      d(params.blockBits*(i+1)-1, params.blockBits*i)
    })
  val data_banks = data_slices.map(s => Mux1H(s2_bank_idx, s))
  val hit_vec = valids zip tags map { case (v, t) => v && t === s2_tag }
  val data = Mux1H(hit_vec, data_banks)
  val queue_valids = if (params.dCache) write_queue.map(_.valid) else null
  val queue_tags = if (params.dCache) write_queue.map(s => Cat(s.addr, s2_pc(pgOffsetWidth - 1, 0))(paddrWidth - 1, log2Ceil(params.lineBytes))) else null
  val queue_hit_vec = if (params.dCache) queue_valids zip queue_tags map { case (v, t) => v && s2_tag === t } else null
  val queue_hit_data = if (params.dCache) Mux1H(queue_hit_vec, write_queue.map(_.data)) else null
  val queue_data_slices = if (params.dCache) Seq.tabulate(params.numOfBanks) { b =>
    queue_hit_data(params.blockBits*(b+1)-1, params.blockBits*b)
  }  else null
  val queue_data = if (params.dCache) Mux1H(s2_bank_idx, queue_data_slices) else null
  val data_selected = if (params.dCache) Mux(hit_vec.reduce(_|_), data, queue_data) else data
  io.resp.hit := RegEnable(hit_vec.reduce(_|_) | (if (params.dCache) queue_hit_vec.reduce(_|_) else false.B), enable)
  io.resp.data := RegEnable(data_selected, enable)

  //============================ Write =================================//
  val write_miss = write_enable && !hit_vec.reduce(_|_)
  val queue_has_no_space = if (params.dCache) (params.bufferSize.U - (write_tail - write_head)) >= params.bufferSize.U else false.B
  //  Request
  val s_ready::s_write_miss::s_mshr::s_write::s_wait::Nil = Enum(5)
  val state = RegInit(s_ready)
  def is_ready     = state === s_ready
  def is_write_miss = state === s_write_miss
  def is_mshr      = state === s_mshr
  def is_write     = state === s_write

  val mshr_req = if (mshrOn) mshr.io.ptw.req.valid else false.B
  val wirte_queue_req = if (params.dCache) write_queue(write_head).valid else false.B
  io.ptw.req.valid := is_ready && (write_miss | mshr_req| wirte_queue_req)
  io.ptw.req.bits.addr := Mux(write_miss, Cat(s2_tag, s2_pc(pgOffsetWidth - 1, 0))(paddrWidth - 1, log2Ceil(params.lineBytes)),
    Mux(mshr_req, (if (mshrOn) mshr.io.ptw.req.bits.addr else 0.U), (if (params.dCache) write_queue(write_head).addr else 0.U)))
  io.ptw.req.bits.data := (if (params.dCache) write_queue(write_head).data else 0.U)
  io.ptw.req.bits.cmd  := Mux(write_miss | mshr_req, M_XRD, M_XWR)
  if (mshrOn) {
    mshr.io.ptw.req.ready := is_mshr && io.ptw.req.ready
  }

  switch (s_ready) {
    is (s_ready) {
      when (io.ptw.req.valid) {
        when (write_miss) {
          state := s_write_miss
        } .elsewhen (mshr_req) {
          state := s_mshr
        } .elsewhen (wirte_queue_req) {
          state := s_write
        }
      }
    }
    is (s_write_miss) {
      when (io.ptw.req.ready) {
        state := s_wait
      }
    }
    is (s_mshr) {
      when (io.ptw.req.ready) {
        state := s_wait
      }
    }
    is (s_write) {
      when (io.ptw.req.ready) {
        state := s_wait
      }
    }
    is (s_wait) {
      when (io.ptw.resp.valid) {
        state := s_ready
      }
    }
  }

  if (params.dCache) {
    val enq = WireInit(false.B)
    val need_write_to_memory = WireInit(false.B)
    val data_selected = datas(OHToUInt(hit_vec))
    val write_data_slices = Wire(Vec(params.lineBytes, UInt(8.W)))
    for (b <- 0 until params.lineBytes) {
      write_data_slices(b) := Mux(s2_offset <= b.U && s2_mask(b),
        s2_data(8*(b+1)-1, 8*b), data_selected(8*(b+1)-1, 8*b))
    }
    val write_data = Cat(write_data_slices.reverse)
    when (enable) {
      when(write_enable) {
        enq := write_enable && need_write_to_memory
        val new_entry = Wire(new CacheMeta)
        new_entry.valid  := true.B
        new_entry.addr   := Cat(s2_tag, s2_pc(pgOffsetWidth - 1, 0))(paddrWidth - 1, log2Ceil(params.lineBytes))
        new_entry.data   := write_data
        when (enq) {
          write_queue(write_tail) := new_entry
          write_tail := write_tail + 1.U
        }
        when (io.ptw.resp.valid && is_write) {
          write_queue(write_head).valid := false.B
          write_head := write_head + 1.U
        }
        when(!write_miss) {
          data_array.write(s2_idx, VecInit(Seq.fill(params.numOfWays) {
            write_data
          }), hit_vec)
          need_write_to_memory := true.B
        }
      }
    }
  }

  //  MSHR
  if (mshrOn) {
    mshr.io.kill                := io.kill
    mshr.io.req.valid           := read_enable && enable && !io.resp.hit
    mshr.io.req.bits.rob_id     := s2_rob_id
    mshr.io.req.bits.ld_id      := s2_ld_id
    mshr.io.req.bits.addr       := Cat(s2_tag, s2_pc(pgOffsetWidth - 1, 0))(paddrWidth - 1, log2Ceil(params.lineBytes))
    mshr.io.ptw.resp.done       := io.ptw.resp.valid && is_mshr
  }

  //  Refill
  val do_refill = WireInit(false.B)
  val refill_idx = s2_idx
  val refill_tag = s2_tag
  val invalid_way = selectFirstN(Reverse(~Cat(valids)))
  val has_invalid_way = !valids.reduce(_ & _)
  val has_exception = io.ptw.resp.bits.cause =/= 0.U
  val replace_way = Mux(has_invalid_way, invalid_way, UIntToOH(replacePolicy.getVictim))
  //
  when(write_enable) {
    when (io.ptw.resp.valid && is_write_miss && !has_exception) {
      do_refill := false.B
    } .elsewhen(!hit_vec.reduce(_|_)) {
      do_refill := true.B
    }
  }
  when(write_enable && io.ptw.resp.valid && is_write_miss) {
    val write_data_slices = Wire(Vec(params.lineBytes, UInt(8.W)))
    for (b <- 0 until params.lineBytes) {
      if (params.dCache) {
        write_data_slices(b) := Mux(s2_offset <= b.U && s2_mask(b),
          s2_data(8*(b+1)-1, 8*b), io.ptw.resp.bits.data(8*(b+1)-1, 8*b))
      } else {
        write_data_slices(b) := io.ptw.resp.bits.data(8*(b+1)-1, 8*b)
      }
    }
    val write_data = Cat(write_data_slices.reverse)
    data_array.write(refill_idx, VecInit(Seq.fill(params.numOfWays) {
      write_data
    }), replace_way.asBools)
  } .elsewhen (io.ptw.resp.valid && is_mshr && !has_exception) {
    data_array.write(refill_idx, VecInit(Seq.fill(params.numOfWays) {io.ptw.resp.bits.data}), replace_way.asBools)
  }
  when(invalidate) {
    valid_array.foreach(_.foreach(_ := false.B))
  }.elsewhen(io.ptw.resp.valid && (is_mshr | is_write_miss) && !has_exception) {
    for (w <- 0 until params.numOfWays) {
      when(w.U === replace_way) {
        valid_array(refill_idx)(w) := true.B
        tag_array(refill_idx)(w) := refill_tag
      }
    }
  }
  //
  io.write_miss := do_refill
  io.no_space := (if (mshrOn) mshr.io.full else false.B) | queue_has_no_space

  if (mshrOn) {
    io.ld_id := mshr.io.ld_id
  }
}
