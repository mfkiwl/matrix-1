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

class LTageReq(implicit p: Parameters) extends MatrixBundle {
  val pc      = UInt(vaddrWidth.W)
}

class LTageResp(implicit p: Parameters) extends MatrixBundle {
  val loop  = new LoopResp
  val tage  = new TageData
}

class LTageUpdate(implicit p: Parameters) extends TageUpdate {
  val use_loop   = Bool()
  val loop_taken = Bool()
  val pc        = UInt(vaddrWidth.W)
}

class LTageIO(implicit p: Parameters) extends MatrixBundle {
  val req = Flipped(Valid(new LTageReq))
  val resp = Output(new LTageResp)
  val update = Flipped(Valid(new LTageUpdate))
}

class LTage(implicit p: Parameters) extends MatrixModule {
  val io = IO(new LTageIO)

  val loop = Module(new LoopPredictor)
  val tage = Module(new Tage)

  loop.io.req := io.req
  tage.io.req := io.req

  val loop_update = Wire(new LoopUpdate)
  loop_update.taken        := io.update.bits.taken
  loop_update.loop_taken   := io.update.bits.loop_taken
  loop_update.pc           := io.update.bits.pc
  loop.io.update.valid    := io.update.valid
  loop.io.update.bits     := loop_update

  val tage_update = Wire(new TageUpdate)
  tage_update.taken       := io.update.bits.taken
  tage_update.prime_bank  := io.update.bits.prime_bank
  tage_update.alt_bank    := io.update.bits.alt_bank
  tage_update.prime_bank  := io.update.bits.prime_bank
  tage_update.alt_bank    := io.update.bits.alt_bank
  tage_update.bim_cnt     := io.update.bits.bim_cnt
  tage_update.meta        := io.update.bits.meta
  tage_update.tags        := io.update.bits.tags
  tage_update.banks       := io.update.bits.banks

  tage.io.update.valid  := io.update.valid && !io.update.bits.use_loop
  tage.io.update.bits   := tage_update
  tage.io.update_pc     := io.update.bits.pc

  val ltage_resp = Wire(new LTageResp)
  ltage_resp.loop := loop.io.resp
  ltage_resp.tage := tage.io.resp
  io.resp := ltage_resp
}