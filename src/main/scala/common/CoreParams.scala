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
import freechips.rocketchip.config.Field
import matrix.ifu._
import matrix.vmm._

case object MatrixILEN extends Field[Int]
case object MatrixXLEN extends Field[Int]
case object MatrixFLEN extends Field[Int]
case object MatrixPgLevels extends Field[Int]
case object MatrixASID extends Field[Int]

case class MatrixCoreParams (
                              fetchWidth: Int         = 4,
                              decodeWidth: Int        = 4,
                              issueWidth: Int         = 6,
                              retireWidth: Int        = 4,

                              useVM: Boolean          = true,
                              useUser: Boolean        = true,
                              useDebug: Boolean       = true,
                              useAtomics: Boolean     = true,
                              useCompressed: Boolean  = true,
                              useVector: Boolean      = false,
                              useRVE: Boolean         = false,
                              useFPU: Boolean         = true,
                              useSuperpage: Boolean   = true,
                              useMSHR: Boolean        = true,
                              nMSHRs: Int             = 32,
                              useClockGating: Boolean = true,
                              usePMP: Boolean         = true,
                              numOfPMPs: Int              = 8,
                              numOfHPMs: Int              = 29,
                              numOfPerfCounters: Int      = 29,
                              numOfBreakpoints: Int       = 8,
                              misaWritable: Boolean   = true,
                              useDMA: Boolean         = true,
                              useSv32: Boolean        = false,
                              useSv39: Boolean        = false,
                              useSv48: Boolean        = true,

                              numOfRobEntries: Int      = 128,
                              numOfStqEntries: Int      = 32,
                              numOfLdqEntries: Int      = 32,
                              numOfFBEntries: Int       = 32,
                              numOfLRegs: Int           = 32,
                              numOfRsvEntries: Int      = 80,
                              bpdParams: BPDParams = BPDParams(),
                              icacheParams: CacheParams = CacheParams(
                                dCache = false,
                                mshrOn = false,
                                dataWidth = 512
                              ),
                              dcacheParams: CacheParams = CacheParams(
                                dCache = true,
                                mshrOn = true,
                                dataWidth = 512
                              ),
                              itlbParams: TLBParams = TLBParams(
                                numOfSets = 256,
                                numOfWays = 4
                              ),
                              dtlbParams: TLBParams = TLBParams(
                                numOfSets = 256,
                                numOfWays = 4
                              )
                            )


trait HasMatrixParams extends HasTileParams {
  //  General features
  def usingVM: Boolean            = tileParams.core.useVM
  def usingUser: Boolean          = tileParams.core.useUser || usingVM
  def usingDebug: Boolean         = tileParams.core.useDebug
  def usingAtomics: Boolean       = tileParams.core.useAtomics
  def usingCompressed: Boolean    = tileParams.core.useCompressed
  def usingVector: Boolean        = tileParams.core.useVector
  def usingSuperpage: Boolean     = tileParams.core.useSuperpage
  def usingRVE: Boolean           = tileParams.core.useRVE
  def usingFPU: Boolean           = tileParams.core.useFPU
  def usingMSHR: Boolean          = tileParams.core.useMSHR
  def nMSHRs: Int                 = tileParams.core.nMSHRs
  def usingClockGating: Boolean   = tileParams.core.useClockGating
  def usingPMP: Boolean           = tileParams.core.usePMP
  def numOfPMPs: Int              = tileParams.core.numOfPMPs
  def numOfHPMs: Int              = tileParams.core.numOfHPMs
  def misaWritable: Boolean       = tileParams.core.misaWritable
  def usingDMA: Boolean           = tileParams.core.useDMA

  def usingSv32: Boolean          = tileParams.core.useSv32
  def usingSv39: Boolean          = tileParams.core.useSv39
  def usingSv48: Boolean          = tileParams.core.useSv48

  //======================================================
  //  Core
  //======================================================
  def bootAddress     = "hfffffffffffffff0"
  def fetchWidth      = tileParams.core.fetchWidth
  def decodeWidth     = tileParams.core.decodeWidth
  def issueWidth      = tileParams.core.issueWidth
  def retireWidth     = tileParams.core.retireWidth

  def ILEN: Int         = p(MatrixILEN)
  def XLEN: Int         = p(MatrixXLEN)
  def FLEN: Int         = p(MatrixFLEN)
  def pgOffsetWidth: Int= 12
  def pgLevels: Int     = p(MatrixPgLevels)

  def vaddrWidth: Int   = {
    val res = if (usingSv32) {
      32
    } else if (usingSv39) {
      39
    } else if (usingSv48) {
      48
    } else {
      57
    }
    res
  }
  def paddrWidth: Int   =  {
    val res = usingSv32 match {
      case true => 34;
      case false => 56;
    }
    res
  }
  val vpnWidth: Int = vaddrWidth - pgOffsetWidth
  val ppnWidth: Int = paddrWidth - pgOffsetWidth
  val pgLevelWidth: Int = vpnWidth / pgLevels

  val ASIDLEN: Int = p(MatrixASID)
  val ASIDMAX: Int = {
    val res = usingSv32 match {
      case true => 9;
      case false => 16;
    }
    res
  }

  val MXLEN: Int = XLEN
  val SXLEN: Int = MXLEN
  val EXLEN: Int = 7

  def CSR_ADDR_MSB = 31
  def CSR_ADDR_LSB = 20
  def CSR_ADDR_SZ = 12

  def instBytes: Int      = ILEN / 8
  def fetchBytes: Int     = fetchWidth * instBytes
  def immWidth: Int       = XLEN
  def CSRADDRLEN: Int     = 12

  def numOfPMPConfigs: Int    = 8
  def numOfPerfCounters: Int  = tileParams.core.numOfPerfCounters
  def numOfBreakpoints: Int   = tileParams.core.numOfBreakpoints

  val numOfRobEntries: Int  = tileParams.core.numOfRobEntries
  val numOfFBEntries: Int   = tileParams.core.numOfFBEntries
  val numOfStqEntries: Int  = tileParams.core.numOfStqEntries
  val numOfLdqEntries: Int  = tileParams.core.numOfLdqEntries
  val numOfRsvEntries: Int  = tileParams.core.numOfRsvEntries
  val numOfLRegs: Int       = tileParams.core.numOfLRegs

  val lregSz: Int = log2Ceil(numOfLRegs)

  val lregWidth: Int  = log2Ceil(numOfLRegs)
  val robIdWidth: Int = log2Ceil(numOfRobEntries) + 1
  val stqIdWidth: Int = log2Ceil(numOfStqEntries) + 1
  val ldqIdWidth: Int = log2Ceil(numOfLdqEntries) + 1
  val fbIdWidth: Int  = log2Ceil(numOfFBEntries) + 1
  val rsvIdWidth: Int = log2Ceil(numOfRsvEntries)

  //=====================================================
  //  Functions parameters
  //=====================================================
  val bpdParams = tileParams.core.bpdParams
  val icacheParams = tileParams.core.icacheParams
  val dcacheParams = tileParams.core.dcacheParams
  val itlbParams = tileParams.core.itlbParams
  val dtlbParams = tileParams.core.dtlbParams
}
