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
package matrix.utils

import chisel3._
import chisel3.util._

abstract class BaseReplacementPolicy {
  def reset(): Unit
  def touch(way: UInt): Unit
  def getVictim(): UInt
}

class PseudoLRUPolicyImpl(entries: Int) extends BaseReplacementPolicy {
  private val state = RegInit(0.U((entries - 1).W))

  def getNextState(state: UInt, way: UInt) = {
    var nextState = state << 1
    var idx = 1.U(1.W)
    for (i <- log2Up(entries) - 1 to 0 by -1) {
      val bit = way(i)
      nextState = nextState.asUInt().bitSet(idx, !bit)
      idx = Cat(idx, bit)
    }
    nextState(entries - 1, 1)
  }

  def getReplaceWay(state: UInt) = {
    val shiftedState = state << 1
    var idx = 1.U(1.W)
    for (i <- log2Up(entries) - 1 to 0 by -1) {
      val in_bounds = Cat(idx, 1.U << i)(log2Up(entries) - 1, 0) < entries.U
      idx = Cat(idx, in_bounds && shiftedState(idx))
    }
    idx(log2Up(entries) - 1, 0)
  }

  override def reset(): Unit = {
    state := 0.U
  }

  override def touch(way: UInt): Unit = {
    state := getNextState(state, way)
  }

  override def getVictim = getReplaceWay(state)
}