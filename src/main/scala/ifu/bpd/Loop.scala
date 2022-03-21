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

case class LoopParams (
                        ageSize: Int = 3,
                        confSize: Int = 3,
                        cntSize: Int = 10,
                        numOfSets: Int = 128
                      ) {
  def AGE_MAX: Int = (1 << ageSize) - 1
  def CNT_MAX: Int = (1 << cntSize) - 1
  def CONF_MAX: Int = (1 << confSize) - 1
}

class LoopMeta(implicit p: Parameters) extends MatrixBundle {
  def tagSize = vaddrWidth - log2Ceil(bpdParams.loop.numOfSets) - 1

  val tag             = UInt(tagSize.W)                 
  val age             = UInt(bpdParams.loop.ageSize.W)    
  val conf            = UInt(bpdParams.loop.confSize.W)   
  val c_cnt           = UInt(bpdParams.loop.cntSize.W)    
  val p_cnt           = UInt(bpdParams.loop.cntSize.W)    
}

class LoopReq(implicit p: Parameters) extends MatrixBundle {   
  val pc      = UInt(vaddrWidth.W)           
}

class LoopResp(implicit p: Parameters) extends MatrixBundle {
  val loop_taken   = Bool()
  val use_loop     = Bool()
}

class LoopUpdate(implicit p: Parameters) extends MatrixBundle {
  val taken       = Bool()                
  val loop_taken   = Bool()                
  val pc          = UInt(vaddrWidth.W)   
}

class LoopPredictorIO(implicit p: Parameters) extends MatrixBundle {
  val req = Flipped(Valid(new LoopReq))
  val resp = Output(new LoopResp)
  val update = Flipped(Valid(new LoopUpdate))
}

class LoopPredictor(implicit p: Parameters) extends MatrixModule {
  val io = IO(new LoopPredictorIO)

  def computeIndexAndTag(pc: UInt) = {
    val idx = pc(log2Ceil(bpdParams.loop.numOfSets), 1)
    val tag = pc(vaddrWidth - 1, 1 + log2Ceil(bpdParams.loop.numOfSets))
    (idx, tag)
  }

  //============================================
  //  Name: Meta array
  //  Desc: Loop predictor meta array
  //  Size: bpdParam.loop.nSets
  //============================================
  val meta_array = Reg(Vec(bpdParams.loop.numOfSets, new LoopMeta))

  //  Update prediction
  val (update_idx, update_tag) = computeIndexAndTag(io.update.bits.pc)
  val update_info = WireInit(io.update.bits)

  val entry = meta_array(update_idx)
  val wentry = WireInit(entry)
  val tagHit = entry.tag === update_tag

  when (!tagHit && entry.age > 0.U) {
    wentry.age := entry.age - 1.U
  } .otherwise {
    when (entry.age === 0.U) {
      wentry.tag        := update_tag
      wentry.age        := bpdParams.loop.AGE_MAX.U
      wentry.c_cnt := bpdParams.loop.CNT_MAX.U
      wentry.conf       := 0.U
    } .otherwise {
      when (update_info.loop_taken === update_info.taken) {
        when (entry.c_cnt =/= entry.p_cnt) {
          wentry.c_cnt := entry.c_cnt + 1.U
        } .otherwise {
          wentry.c_cnt := 0.U
          when (entry.conf < bpdParams.loop.CONF_MAX.U) {
            wentry.conf   := entry.conf + 1.U
          }
        }
      } .otherwise {
        when (entry.age === bpdParams.loop.AGE_MAX.U) {
          wentry.p_cnt := entry.c_cnt
          wentry.c_cnt     := 0.U
          wentry.conf           := 1.U
        } .otherwise {
          wentry := 0.U.asTypeOf(new LoopMeta)
        }
      }
    }
  }

  when (io.update.valid) {
    meta_array(update_idx) := wentry
  }

  //  Get prediction
  val (read_idx, read_tag) = computeIndexAndTag(io.req.bits.pc)
  //  FIXME: need hit when update?
  val pred_entry = meta_array(read_idx)

  val loop_resp = Wire(new LoopResp)
  loop_resp.loop_taken := false.B
  loop_resp.use_loop := false.B

  val loop_hit = read_tag === pred_entry.tag
  when (loop_hit) {
    loop_resp.loop_taken := pred_entry.c_cnt < pred_entry.p_cnt
    loop_resp.use_loop := pred_entry.conf === bpdParams.loop.CONF_MAX.U
  }
  io.resp := RegEnable(loop_resp, io.req.valid && !io.update.valid)
}