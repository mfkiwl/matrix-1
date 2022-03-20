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

case class BPDParams (
                       btb: BTBParams = BTBParams(),
                       ras: RASParams = RASParams(),
                       loop: LoopParams = LoopParams(),
                       tage: TAGEParams = TAGEParams()
                     )

class PredictorReq(implicit p: Parameters) extends MatrixBundle {
  val pc    = UInt(vaddrWidth.W)     
}

class PredictorBTBResp(implicit p: Parameters) extends MatrixBundle {
  val meta  = new BTBMeta             
}

class PredictorLTageResp(implicit p: Parameters) extends MatrixBundle {
  val loop = new LoopResp            
  val tage = new TageData            
}

class PredictorResp(implicit p: Parameters) extends MatrixBundle {
  val tg_addr     = UInt(vaddrWidth.W)     
  val btb         = new PredictorBTBResp      
  val ltage       = new PredictorLTageResp    
}

class PredictorBTBUpdate(implicit p: Parameters) extends MatrixBundle {   
  val valid   = Bool()
  val alloc   = Bool()                  
  val meta    = new BTBMeta              
}

class PredictorLTageUpdate(implicit p: Parameters) extends LTageUpdate {
  val valid = Bool()
}

class PredictorUpdate(implicit p: Parameters) extends MatrixBundle {
  val pc            = UInt(vaddrWidth.W)       
  val tg_addr       = UInt(vaddrWidth.W)        
  val btb_update    = new PredictorBTBUpdate    
  val ltage_update  = new PredictorLTageUpdate  
}

class PredictorIO(implicit p: Parameters) extends MatrixBundle {
  val req = Flipped(Valid(new PredictorReq))
  val resp = Valid(new PredictorResp)
  val update = Input(new PredictorUpdate)
}

class Predictor(implicit p: Parameters) extends MatrixModule {
  val io = IO(new PredictorIO)
  
  val btb = Module(new BTB)
  val ras = Module(new RAS)
  val ltage = Module(new LTage)

  //  Stage 1
  btb.io.req := io.req
  ltage.io.req := io.req
  val s1_val = RegNext(io.req.valid)
  val s1_pc = RegEnable(io.req.bits.pc, io.req.valid)

  //  Stage 2
  val next_pc = s1_pc + btb.io.resp.bits.meta.offset + Mux(btb.io.resp.bits.meta.len, 2.U, 4.U)
  ras.io.req := s1_val
  ras.io.push := btb.io.resp.bits.meta.call | btb.io.resp.bits.meta.ret_then_call
  ras.io.push_addr := next_pc
  ras.io.pop := btb.io.resp.bits.meta.ret | btb.io.resp.bits.meta.ret_then_call

  val resp = Wire(new PredictorResp)
  val btbValid = btb.io.resp.valid && (btb.io.resp.bits.meta.jmp | btb.io.resp.bits.meta.condi)
  resp.tg_addr := Mux(btbValid, btb.io.resp.bits.tg_addr, ras.io.pop_addr)
  resp.btb.meta := btb.io.resp.bits.meta
  resp.ltage.loop := ltage.io.resp.loop
  resp.ltage.tage := ltage.io.resp.tage

  io.resp.valid := btb.io.resp.valid
  io.resp.bits := RegEnable(resp, s1_val)

  //  Update
  //  Update BTB
  btb.io.update.valid := io.update.btb_update.valid
  btb.io.update.bits.pc := io.update.pc
  btb.io.update.bits.alloc := io.update.btb_update.alloc
  btb.io.update.bits.tg_addr := io.update.tg_addr
  btb.io.update.bits.meta := io.update.btb_update.meta

  //  Update LTage
  ltage.io.update.valid             := io.update.ltage_update.valid
  ltage.io.update.bits.taken        := io.update.ltage_update.taken
  ltage.io.update.bits.use_loop     := io.update.ltage_update.use_loop
  ltage.io.update.bits.loop_taken   := io.update.ltage_update.loop_taken
  ltage.io.update.bits.prime_taken  := io.update.ltage_update.prime_taken
  ltage.io.update.bits.alt_taken    := io.update.ltage_update.alt_taken
  ltage.io.update.bits.prime_bank   := io.update.ltage_update.prime_bank
  ltage.io.update.bits.alt_bank     := io.update.ltage_update.alt_bank
  ltage.io.update.bits.bim_cnt      := io.update.ltage_update.bim_cnt
  ltage.io.update.bits.meta         := io.update.ltage_update.meta
  ltage.io.update.bits.tags         := io.update.ltage_update.tags
  ltage.io.update.bits.banks        := io.update.ltage_update.banks
  ltage.io.update.bits.pc           := io.update.pc
}