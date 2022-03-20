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
package matrix.config

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import matrix.common._

class DefaultConfig(n: Int = 4) extends Config (
  new Config((site, here, up) => {
    case MatrixTileKey => {
      MatrixTileParams (
        core = MatrixCoreParams(),
        hartId = 0
      )
    }
    case MatrixXLEN => 64
    case MatrixFLEN => 64
    case MatrixILEN => 32
    case MatrixPgLevels => 4
    case MatrixASID => 16
  })
)