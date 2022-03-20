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

case class AXI4Params (
                        ID_WIDTH: Int = 4,
                        LEN_WIDTH: Int = 8,
                        SIZE_WIDTH: Int = 3,
                        BURST_WIDTH: Int = 2,
                        ADDR_WIDTH: Int = 64,
                        DATA_WIDTH: Int = 128,
                        LOCK_WIDTH: Int = 1,
                        CACHE_WIDTH: Int = 4,
                        PROT_WIDTH: Int = 3,
                        QoS_WIDTH: Int = 4,
                        REGION_WIDTH: Int = 4,
                        USER_WIDTH: Int = 32,
                        RESP_WIDTH: Int = 2
                      ) {
  def STRB_WIDTH = DATA_WIDTH / 8
}

object AXI4 {
  val BURST_FIXED         = 0x0
  val BURST_INCR          = 0x1
  val BURST_WRAP          = 0x2

  val RESP_OKAY           = 0x0
  val RESP_EXOKAY         = 0x1
  val RESP_SLVERR         = 0x2
  val RESP_DECERR         = 0x3

  val CACHE_ALLOCATE      = 0x8
  val CACHE_OTHER         = 0x4
  val CACHE_MODIFIABLE    = 0x2
  val CACHE_BUFFERABLE    = 0x1

  val PROT_UNPRIV         = 0x0
  val PROT_PRIV           = 0x1
  val PROT_SECTURE        = 0x0
  val PROT_NONSECURE      = 0x2
  val PROT_DATA           = 0x0
  val PROT_INST           = 0x4

  val SIZE_1Bytes         = 0x0
  val SIZE_2Bytes         = 0x1
  val SIZE_4Bytes         = 0x2
  val SIZE_8Bytes         = 0x3
  val SIZE_16Bytes        = 0x4
  val SIZE_32Bytes        = 0x5
  val SIZE_64Bytes        = 0x6
  val SIZE_128Bytes       = 0x7
}

