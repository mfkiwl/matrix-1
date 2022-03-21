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
package matrix.ifu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import matrix.common._
import matrix.utils._

case class BTBParams (
                       numOfsets: Int = 64
                     ) {
  def btbIdWidth: Int = log2Ceil(numOfsets)
}

class BTBMeta(implicit p: Parameters) extends MatrixBundle {
  val offset        = UInt(4.W)
  val len           = Bool()
  val jmp           = Bool()
  val ret           = Bool()
  val call          = Bool()
  val ret_then_call = Bool()
  val condi         = Bool()
}

class BTBReq(implicit p: Parameters) extends MatrixBundle {
  val pc  = UInt(vaddrWidth.W)
}

class BTBResp(implicit p: Parameters) extends MatrixBundle {
  val valid     = Bool()
  val tg_addr   = UInt(vaddrWidth.W)
  val btb_idx   = UInt(bpdParams.btb.btbIdWidth.W)
  val meta      = new BTBMeta
}


class BTBUpdate(implicit p: Parameters) extends MatrixBundle {
  val pc      = UInt(vaddrWidth.W)
  val tg_addr = UInt(vaddrWidth.W)
  val alloc   = Bool()
  val btb_idx = UInt(bpdParams.btb.btbIdWidth.W)
  val meta    = new BTBMeta
}

class BTBIO(implicit p: Parameters) extends MatrixBundle {
  val req = Flipped(Valid(new BTBReq))
  val resp = Output(new BTBResp)
  val update = Flipped(Valid(new BTBUpdate))
}

class BTB(implicit p: Parameters) extends MatrixModule {
  val io = IO(new BTBIO)

  val numOfSets = bpdParams.btb.numOfsets
  def SET_INDEX_LSB = 4
  def SET_INDEX_MSB = SET_INDEX_LSB + log2Ceil(numOfSets) - 1
  def computeTag(pc: UInt) = pc(vaddrWidth - 1, SET_INDEX_MSB + 1)
  //  Name: Valid array.
  //  Desc: BTB valid array
  //  Size: bpdParams.btb.numOfSets
  //=============================================
  val valid_array = Reg(Vec(numOfSets, Bool()))
  //=============================================
  //  Name: Tag array.
  //  Desc: BTB tag array
  //  Size: bpdParams.btb.numOfSets
  //=============================================
  val tagSz = vaddrWidth - SET_INDEX_MSB - 1
  val tag_array = Reg(Vec(numOfSets, UInt(tagSz.W)))
  //=============================================
  //  Name: Data array
  //  Desc: BTB data array
  //  Size: bpdParams.btb.numOfSets
  //=============================================
  val data_array = SyncReadMem(numOfSets, UInt(vaddrWidth.W))

  //=============================================
  //  Name: Meta array
  //  Desc: BTB meta array
  //  Size: bpdParams.btb.numOfSets
  //=============================================
  val meta_array = SyncReadMem(numOfSets, new BTBMeta)

  ///=============================================
  //  Replacement policyL Pseudo-LRU
  //=============================================
  val replace_policy = new PseudoLRUPolicyImpl(numOfSets)

  //  Update BTB
  val update_tag = computeTag(io.update.bits.pc)
  val update_set_idx = io.update.bits.btb_idx
  val update_meta = WireInit(io.update.bits.meta)

  val valid_entry = Reverse(Cat(valid_array(update_set_idx)))
  val has_invalid_entry = valid_entry.andR
  val invalid_entryOH = selectFirstN(valid_entry)
  val invalid_entry = OHToUInt(invalid_entryOH)
  val replace_way = Mux(has_invalid_entry, invalid_entry, replace_policy.getVictim)

  val update_data = io.update.bits.tg_addr
  when (io.update.valid) {
    when (io.update.bits.alloc) {
      valid_array(update_set_idx) := valid_array(update_set_idx) | invalid_entryOH
    }
    tag_array(update_set_idx) := update_tag
    data_array.write(update_set_idx, update_data)
    meta_array.write(update_set_idx, update_meta)
  }

  //  Lookup BTB
  val tag = computeTag(io.req.bits.pc)
  //  FIXME: Hit when update ?
  val do_read = !io.update.valid && io.req.valid
  val hit_vec = valid_array zip tag_array map { case (v, t) => v && tag === t }
  val which_slot = RegEnable(OHToUInt(selectFirstN(Reverse(Cat(hit_vec)))), do_read)
  val tag_hit = RegEnable(hit_vec.reduce(_|_), do_read)
  val data_selected = data_array.read(which_slot, do_read)
  val meta_selected = meta_array.read(which_slot, do_read)

  val resp = Wire(new BTBResp)
  resp.valid    := tag_hit
  resp.tg_addr  := data_selected
  resp.meta     := meta_selected
  resp.btb_idx  := which_slot
  io.resp       := resp

  when (io.resp.valid && tag_hit) {
    replace_policy.touch(which_slot)
  }
}