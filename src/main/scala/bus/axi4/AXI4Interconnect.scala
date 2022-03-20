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
package matrix.bus.axi4

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters

class AXI4Interconnect(
                        numMasters: Int = 1,
                        numSlaves: Int = 1,
                        params: AXI4Params
                      ) extends Module {
  /**
   *  Add a "Null device". Plugin in region 0.
   */
  def totalSlaves = numSlaves + 1 // Null device
  val io = IO(new Bundle() {
    val masters = Flipped(Vec(numMasters, AXI4Bundle(params)))
    val slaves = Vec(totalSlaves, AXI4Bundle(params))
  })

  def legal_region(source: UInt) = {
    Mux(source < numSlaves.U, source, 0.U)
  }

  val arbiters = Seq.fill(totalSlaves) { Module(new AXI4Arbiter(totalSlaves * 2)) }
  val valids = io.masters.map(m => m.aw.valid) ++ io.masters.map(m => m.ar.valid)
  val regions = io.masters.map(m => m.aw.bits.region) ++ io.masters.map(m => m.ar.bits.region)
  val requests = valids zip regions map { case (v, r) => (v, legal_region(r)) }

  for (s <- 0 until totalSlaves) {
    val reqs = requests.map(r => {
      r._1 && r._2 === s.U
    })
    arbiters(s).io.req := reqs
  }
  for (s <- 0 until totalSlaves) {
    val ack = Wire(Vec(numMasters * 2, Bool()))
    for (n <- 0 until numMasters) {
      ack(n) := arbiters(s).io.grant(n) && io.masters(n).b.fire
      ack(numMasters+n) := arbiters(s).io.grant(numMasters+n) && io.masters(n).r.fire && io.masters(n).r.bits.last
    }
    arbiters(s).io.ack := ack
  }

  val select_ports = arbiters.map(_.io.port >> 1.U)
  val chan_mappings = select_ports.map(port => {
    for (c <- 0 until numMasters)
      yield
      {
        (c.U === port) -> io.masters(c)
      }
  })
  val select_chans = chan_mappings.map(Mux1H(_))
  for (s <- 0 until totalSlaves) {
    io.slaves(s) <> select_chans(s)
  }
}