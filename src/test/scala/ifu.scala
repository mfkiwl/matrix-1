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
package matrix
import chisel3._
import chisel3.util._
import chisel3.iotesters._
import freechips.rocketchip.config.Parameters
import matrix.common._
import matrix.ifu._
import org.scalatest._

class ifu_tester(dut: FetchUnit) extends PeekPokeTester(dut) {
  step(10)
  step(1)
}

class ifu_spec extends FlatSpec with Matchers {
  val matrixParams: Parameters = MatrixTestUnit.getMatrixParameters("MatrixConfig")
  implicit val p: Parameters = matrixParams.alterPartial {
    case MatrixTileKey => matrixParams(MatrixTileKey)
  }
  "ifu_top" should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"),() => new FetchUnit()) {
      c => new ifu_tester(c)
    } should be (true)
  }
}
