package matrix.ifu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import matrix.common._
import matrix.utils._

case class BTBParams (
                       numOfSets: Int = 32,
                       numOfWays: Int = 8
                     )


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
  val meta      = new BTBMeta
}

class BTBUpdate(implicit p: Parameters) extends MatrixBundle {
  val pc      = UInt(vaddrWidth.W)
  val tg_addr = UInt(vaddrWidth.W)
  val alloc   = Bool()
  val meta    = new BTBMeta
}

class BTBIO(implicit p: Parameters) extends MatrixBundle {
  val req = Flipped(Valid(new BTBReq))
  val resp = Output(new BTBResp)
  val update = Flipped(Valid(new BTBUpdate))
}

class BTB(implicit p: Parameters) extends MatrixModule {
  val io = IO(new BTBIO)

  val numOfSets = bpdParams.btb.numOfSets
  val numOfWays = bpdParams.btb.numOfWays
  def SET_INDEX_LSB = 4
  def SET_INDEX_MSB = SET_INDEX_LSB + log2Ceil(numOfSets) - 1
  def computeTag(pc: UInt) = pc(vaddrWidth - 1, SET_INDEX_MSB + 1)
  def computeSetIndex(pc: UInt) = pc(SET_INDEX_MSB, SET_INDEX_LSB)

  //=============================================
  //  Name: Valid array.
  //  Desc: BTB valid array
  //  Size: bpdParams.btb.numOfSets
  //=============================================
  val valid_array = Reg(Vec(numOfSets, UInt(numOfWays.W)))
  //=============================================
  //  Name: Tag array.
  //  Desc: BTB tag array
  //  Size: bpdParams.btb.numOfSets
  //=============================================
  val tagSz = vaddrWidth - SET_INDEX_MSB - 1
  val tag_array = SyncReadMem(numOfSets, Vec(numOfWays, UInt(tagSz.W)))
  //=============================================
  //  Name: Data array
  //  Desc: BTB data array
  //  Size: bpdParams.btb.numOfSets
  //=============================================
  val data_array = SyncReadMem(numOfSets, Vec(numOfWays, UInt(vaddrWidth.W)))

  //=============================================
  //  Name: Meta array
  //  Desc: BTB meta array
  //  Size: bpdParams.btb.numOfSets
  //=============================================
  val meta_array = SyncReadMem(numOfSets, Vec(numOfWays, new BTBMeta))

  ///=============================================
  //  Replacement policyL Pseudo-LRU
  //=============================================
  val replace_policy = new PseudoLRUPolicyImpl(numOfWays)


  //  Update BTB
  val update_tag = computeTag(io.update.bits.pc)
  val update_set_idx = computeSetIndex(io.update.bits.pc)
  val update_meta = WireInit(io.update.bits.meta)

  val valid_entry = Reverse(Cat(valid_array(update_set_idx)))
  val has_invalid_entry = valid_entry.andR
  val invalid_entryOH = selectFirstN(valid_entry)
  val invalid_entry = OHToUInt(invalid_entryOH)
  val replace_way = Mux(has_invalid_entry, invalid_entry, replace_policy.getVictim)

  val update_data = io.update.bits.tg_addr
  val update_mask = Seq.tabulate(numOfWays) { replace_way === _.U }
  when (io.update.valid) {
    when (io.update.bits.alloc) {
      valid_array(update_set_idx) := valid_array(update_set_idx) | invalid_entryOH
    }
    tag_array.write(update_set_idx, VecInit(Seq.fill(numOfWays){update_tag}), update_mask)
    data_array.write(update_set_idx, VecInit(Seq.fill(numOfWays){update_data}), update_mask)
    meta_array.write(update_set_idx, VecInit(Seq.fill(numOfWays){update_meta}), update_mask)
  }

  //  Lookup BTB
  val tag = computeTag(io.req.bits.pc)
  val set_idx = computeSetIndex(io.req.bits.pc)

  //  FIXME: Hit when update ?
  val resp = Wire(new BTBResp)

  val do_read = !io.update.valid && io.req.valid
  val read_valids = RegEnable(valid_array(set_idx), do_read)
  val read_tags = tag_array.read(set_idx, do_read)
  val read_data = data_array.read(set_idx, do_read)
  val read_meta = meta_array.read(set_idx, do_read)

  val tag_hit = read_tags zip read_valids.asBools map { case (t,v) =>
    tag === t && v
  }
  val valid_selected = Mux1H(tag_hit, read_valids.asBools)
  val data_selected = Mux1H(tag_hit, read_data)
  val meta_selected = Mux1H(tag_hit, read_meta)

  resp.valid := valid_selected
  resp.tg_addr := data_selected
  resp.meta := meta_selected

  io.resp := resp

  when (io.resp.valid && tag_hit.reduce(_|_)) {
    replace_policy.touch(set_idx)
  }
}