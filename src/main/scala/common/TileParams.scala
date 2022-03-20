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
package matrix.common

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._

trait TileParams {
  val core: MatrixCoreParams
  val hartId: Int = 0
}
case class MatrixTileParams (
                              core: MatrixCoreParams,
                              hartId: Int
                            )

case object MatrixTileKey extends Field[MatrixTileParams]

trait HasTileParams {
  implicit val p: Parameters

  def tileParams: MatrixTileParams = p(MatrixTileKey)
  def hartId: Int = tileParams.hartId
}