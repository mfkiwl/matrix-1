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
package matrix.issue

import chisel3._
import freechips.rocketchip.config.Parameters
import matrix.common._


class RegFilesReadPortIO(implicit p: Parameters) extends MatrixBundle {
  val addr = Input(UInt(lregSz.W))
  val data = Output(UInt(XLEN.W))
}

class RegFilesWritePortIO(implicit p: Parameters) extends MatrixBundle {
  val valid   = Input(Bool())
  val addr    = Input(UInt(lregSz.W))
  val data    = Input(UInt(XLEN.W))
}

class RegisterFiles(numReadPorts: Int, numWritePorts: Int)(implicit p: Parameters) extends MatrixModule {
  val io = IO(new Bundle() {
    val read_ports = Vec(numReadPorts, new RegFilesReadPortIO)
    val write_ports = Vec(numWritePorts, new RegFilesWritePortIO)
  })
  val regfiles = Reg(Vec(numOfLRegs, UInt(XLEN.W)))
  //  Read
  val read_data = Wire(Vec(numReadPorts, UInt(XLEN.W)))
  for (i <- 0 until numReadPorts) {
    read_data(i) := Mux(io.write_ports(i).valid && io.write_ports(i).addr === io.read_ports(i).addr, io.write_ports(i).data, regfiles(io.read_ports(i).addr))
  }
  io.read_ports zip read_data map { case (f, s) => f.data := s }
  //  Write
  for (i <- 0 until  numWritePorts) {
    when (io.write_ports(i).valid) {
      regfiles(io.write_ports(i).addr) := io.write_ports(i).data
    }
  }
}