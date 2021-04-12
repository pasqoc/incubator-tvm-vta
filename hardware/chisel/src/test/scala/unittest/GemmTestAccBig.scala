/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

// Modified by contributors from Intel Labs

package unittest

import scala.util.Random
import scala.math._

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import vta.core._
import vta.util.config._
import ISA._

import unittest.util._



class TestTensorGemmAccBig(c: TensorGemm, commonSize: Int, inpSize: Int, wgtSize: Int, gemmCommonSize: Int, gemmInpSize: Int, gemmWgtSize: Int) extends GemmTestGEMMAlgorithms(c, gemmCommonSize, gemmInpSize, gemmWgtSize) {

  // Run computation
  
  // Matrices to multiply
  //Define matrix size
  
  // ----------------         ----------------         -----------------------
  // |  commonSize  |         |  commonSize  |         |         wgtSize     |
  // |              |         |              |         |                     |
  // |            i |         |            w |         |                   i |
  // |            n |         |            g |         |                   n |
  // |            p |         |            t |         |                   p |
  // |    inp     S |         |    wgt     S |         |           tnzrAcc     S |
  // |            i |         |            i |         |                   i |
  // |            z |         |            z |         |                   z |
  // |            e |         |            e |         |                   e |
  // |              |         |              |         -----------------------
  // ----------------         |              |        
  //                          |              |        
  //                          |              |        
  //                          ----------------         
  //Inner index goes along the commonSize side
  
  // Divide inp and wgt into bins
  // each bin is transferred from external memory, tnzrAcc bin is stored to external memory
  // multiply bin by bin and accumulate
  
  val bitSize = 8 // data bitwidth, just for smaller numbers to print

  //Define DRAM arrays
  val r = new Random
  val inpGen = new RandomArray(commonSize, bitSize, r)
  val wgtGen = new RandomArray(commonSize, bitSize, r)
  val memA   = Array.fill(inpSize) { wgtGen.positive }
  val memB   = Array.fill(wgtSize) { wgtGen.positive }
  val memAcc = Array.fill(inpSize) {  Array.fill(wgtSize) {0} }


  //--- Define bin sizes ---
  
  // define bins over input matrices
  val commonBSize = ceil(commonSize / gemmCommonSize.toFloat).toInt
  val inpBSize    = ceil(inpSize / gemmInpSize.toFloat).toInt
  val wgtBSize    = ceil(wgtSize / gemmWgtSize.toFloat).toInt
  println(s"-I- Binning: WxH inp ${commonBSize}x${inpBSize} inp ${commonBSize}x${wgtBSize} tnzrAcc ${wgtBSize}x${inpBSize}.")
  
  // define gemm scratchpads
  val tnzrA = Array.fill(gemmInpSize) { Array.fill(gemmCommonSize) {0} }
  val tnzrB = Array.fill(gemmWgtSize) { Array.fill(gemmCommonSize) {0} }
  val tnzrAcc  = Array.fill(gemmInpSize) { Array.fill(gemmWgtSize) {0} }

  
  
  var accBegin = 0 //accumulator idx in tnzrAcc memory
  var inpBegin = 0 //inp idx in inp memory
  var wgtBegin = 0 //weight idx in wgt memory
  var uopBegin = 0 //uop_begin index in uop memory
  var printInfo = true 
  
  // iterate over bins
  var numGemms = 0
  var numInpReads = 0
  var numWgtReads = 0
  var numAccReads = 0
  var numAccWrites = 0
  for (wgtBRowIdx  <- 0 until wgtBSize) {
    for (inpBRowIdx  <- 0 until inpBSize) {
      memManager.readSubArray(memAcc, tnzrAcc, inpBRowIdx * gemmInpSize, 
                   wgtBRowIdx * gemmWgtSize, gemmWgtSize, gemmInpSize)
      numAccReads += 1
      for (commonBColIdx  <- 0 until commonBSize) {
        memManager.readSubArray(memB, tnzrB, wgtBRowIdx * gemmWgtSize, 
                     commonBColIdx * gemmCommonSize, gemmCommonSize, gemmWgtSize)
        memManager.readSubArray(memA, tnzrA, inpBRowIdx * gemmInpSize, 
                     commonBColIdx * gemmCommonSize, gemmCommonSize, gemmInpSize)
        numInpReads += 1
        numWgtReads += 1
        runGEMM_Acc()
        numGemms += 1
        //set scratchpad offset for the next iteration
        accBegin = (accBegin + (1 << log2Ceil(c.p(CoreKey).accMemDepth))/2) % (1 << log2Ceil(c.p(CoreKey).accMemDepth))
        inpBegin = (inpBegin + (1 << log2Ceil(c.p(CoreKey).inpMemDepth))/2) % (1 << log2Ceil(c.p(CoreKey).inpMemDepth))
        wgtBegin = (wgtBegin + (1 << log2Ceil(c.p(CoreKey).wgtMemDepth))/2) % (1 << log2Ceil(c.p(CoreKey).wgtMemDepth))
        uopBegin = (uopBegin + (1 << log2Ceil(c.p(CoreKey).uopMemDepth))/2) % (1 << log2Ceil(c.p(CoreKey).uopMemDepth)) // TODO: fix it
        printInfo = false

      }
      memManager.writeSubArray(tnzrAcc, memAcc, inpBRowIdx * gemmInpSize, wgtBRowIdx * gemmWgtSize)
      numAccWrites +=1
    }
  }
  println(s"-I- Binning: WxH inp ${commonBSize}x${inpBSize} wgt ${commonBSize}x${wgtBSize} acc ${wgtBSize}x${inpBSize}. number of GEMM calls=${numGemms} ")
  println(s"-I- Acc mode. Memory transfer Nb:Total Bytes: Inp: ${numInpReads}:${numInpReads*gemmCommonSize*gemmInpSize} Wgt: ${numWgtReads}:${numWgtReads*gemmCommonSize*gemmWgtSize} Acc: read=${numAccReads}:${numAccReads*gemmInpSize*gemmWgtSize*4} write=${numAccWrites}:${numAccWrites*gemmInpSize*gemmWgtSize*4}")
  val res = gemmRef(memA, memB, Array.fill(inpSize) {  Array.fill(wgtSize) {0} })
  for (idx1 <- 0 until res.length) {
    val row = res(idx1)
    for (idx2 <- 0 until row.length) {
      require(memAcc(idx1)(idx2) == res(idx1)(idx2), s"-F- GEMM failed golden($idx1,$idx2)!=memAcc($idx1,$idx2) ${res(idx1)(idx2)} != ${memAcc(idx1)(idx2)}")
    }
  }
  




}
class TensorGemmAccTestBig32x32x32        extends GenericTest( "TensorGemm_Acc_Big_32", (p:Parameters) => new TensorGemm()(p), (c:TensorGemm) => new TestTensorGemmAccBig(c, 32, 32, 32, 16, 16, 16))
//class TensorGemmAccTestBig1024x1024x1024 extends GenericTest( "TensorGemm_Acc_Big_1024", (p:Parameters) => new TensorGemm()(p), (c:TensorGemm) => new TestTensorGemmAccBig(c, 1024, 1024, 1024, 32, 32, 32))
//class TensorGemmAccTestBig192x12544x64        extends GenericTest( "TensorGemm_Acc_Big_32", (p:Parameters) => new TensorGemm()(p), (c:TensorGemm) => new TestTensorGemmAccBig(c, 192, 12544, 64, 64, 64, 64))




