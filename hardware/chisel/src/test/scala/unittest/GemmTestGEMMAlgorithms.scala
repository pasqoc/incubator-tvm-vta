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


abstract class GemmTestGEMMAlgorithms(c: TensorGemm, gemmCommonSize: Int, gemmInpSize: Int, gemmWgtSize: Int
                              ) extends TestTensorGemmMemManager(c) {
  
  var UOPs = Array.fill(0){new TestUOP(0,0,0)}
  
  def runGEMM_Wgt () = {
    // basic memory offsets in tensors memory. 
    val commonTSize = ceil(gemmCommonSize / c.p(CoreKey).blockIn.toFloat).toInt
    val inpTSize    = ceil(gemmInpSize / c.p(CoreKey).batch.toFloat).toInt
    val wgtTSize    = ceil(gemmWgtSize / c.p(CoreKey).blockOut.toFloat).toInt

    // hardware tensor size limitation
    require(inpTSize * commonTSize  + inpBegin <= c.p(CoreKey).inpMemDepth, "-F- Inp tensor buff index is out of range")
    require(wgtTSize * commonTSize  + wgtBegin <= c.p(CoreKey).wgtMemDepth, "-F- Wgt tensor buff index is out of range")
    require(inpTSize * wgtTSize     + accBegin <= c.p(CoreKey).accMemDepth, "-F- tnzrAcc tensor buff index is out of range")

    if (printInfo) println(s"-I- Scratchpad usage inp ${gemmInpSize*gemmCommonSize} of ${c.p(CoreKey).inpMemDepth*c.p(CoreKey).blockIn*c.p(CoreKey).batch}")
    if (printInfo) println(s"-I- Scratchpad usage wgt ${gemmWgtSize*gemmCommonSize} of ${c.p(CoreKey).wgtMemDepth*c.p(CoreKey).blockIn*c.p(CoreKey).blockOut}")
    if (printInfo) println(s"-I- Scratchpad usage tnzrAcc ${gemmInpSize*gemmWgtSize} of ${c.p(CoreKey).accMemDepth*c.p(CoreKey).blockOut*c.p(CoreKey).batch}")

    //--- Start multiplication --- 


    //define uops
    // uops iterate over inp height keep wgt tensor and build partial sums 
    // of a given wgt tensor with different inp tensors
    UOPs = Array.tabulate(inpTSize){idx => new TestUOP(accBegin + idx * wgtTSize, inpBegin + idx * commonTSize, wgtBegin)} //base adresses for tnzrAcc,inp,wgt memory regions

    if (printInfo) println(s"-I- Num UOPs=${UOPs.length}; tnzrAcc iterNb = $commonTSize; wgt iterNb = $wgtTSize.")

    //define insruction
    //Instruction defines iteration over common index in _1 completing a partial sum
    // and over wgt height in _0 fields

    defineInst( lp_1  = commonTSize, // The number of steps to iterate over common part
                lp_0  = wgtTSize,// The number of steps iterate over wgt height
                wgt_1 = 1, // wgt increment on lp_1 increment
                wgt_0 = commonTSize,// wgt increment on lp_0 increment
                inp_1 = 1, // inp increment on lp_1 increment
                inp_0 = 0,// inp increment on lp_0 increment
                acc_1 = 0,  // tnzrAcc increment on lp_2 increment
                acc_0 = 1// tnzrAcc increment on lp_0 increment
               )

    runTest(UOPs)
  }
 
  def runGEMM_Acc () = {
  // basic memory offsets in tensors memory. 
    val commonTSize = ceil(gemmCommonSize / c.p(CoreKey).blockIn.toFloat).toInt
    val inpTSize    = ceil(gemmInpSize / c.p(CoreKey).batch.toFloat).toInt
    val wgtTSize    = ceil(gemmWgtSize / c.p(CoreKey).blockOut.toFloat).toInt
  
  
  
    // hardware tensor size limitation
    require(inpTSize * commonTSize  + inpBegin <= pow(2, log2Ceil(c.p(CoreKey).inpMemDepth)), "-F- Inp tensor buff index is out of range")
    require(wgtTSize * commonTSize  + wgtBegin <= pow(2, log2Ceil(c.p(CoreKey).wgtMemDepth)), "-F- Wgt tensor buff index is out of range")
    require(inpTSize * wgtTSize     + accBegin <= pow(2, log2Ceil(c.p(CoreKey).accMemDepth)), "-F- tnzrAcc tensor buff index is out of range")


    if (printInfo) println(s"-I- Scratchpad usage inp ${gemmInpSize*gemmCommonSize} of ${c.p(CoreKey).inpMemDepth*c.p(CoreKey).blockIn*c.p(CoreKey).batch}")
    if (printInfo) println(s"-I- Scratchpad usage wgt ${gemmWgtSize*gemmCommonSize} of ${c.p(CoreKey).wgtMemDepth*c.p(CoreKey).blockIn*c.p(CoreKey).blockOut}")
    if (printInfo) println(s"-I- Scratchpad usage tnzrAcc ${gemmInpSize*gemmWgtSize} of ${c.p(CoreKey).accMemDepth*c.p(CoreKey).blockOut*c.p(CoreKey).batch}")
    //--- Start multiplication --- 
  
  
    //define uops
    //This sequence of uops builds the final value of tnzrAcc tensor by iterating over common direction of inp and wgt
    UOPs = Array.tabulate(commonTSize){idx => new TestUOP(accBegin,inpBegin + idx, wgtBegin + idx)} //base adresses for tnzrAcc,inp,wgt memory regions

  
  
  
    if (printInfo) println(s"-I- Num UOPs=${UOPs.length}; inp iterNb = $inpTSize; wgt iterNb = $wgtTSize. Expected cycles=${UOPs.length*gemmInpSize*wgtTSize}*CyclesPerProduct(4)")
  
  
    //define insruction
    //Instruction defines iteration over inp matrix height in _1 fields
    // and over wgt height in _0 fields starting construction of different tnzrAcc tensors
  
    defineInst( lp_1  = inpTSize, // The number of steps to iterate over inp height
              lp_0  = wgtTSize,// The number of steps iterate over wgt height
              wgt_1 = 0, // wgt increment on lp_1 increment
              wgt_0 = commonTSize,// wgt increment on lp_0 increment
              inp_1 = commonTSize, // inp increment on lp_1 increment
              inp_0 = 0,// inp increment on lp_0 increment
              acc_1 = wgtTSize,  // tnzrAcc increment on lp_2 increment
              acc_0 = 1// tnzrAcc increment on lp_0 increment
             )

  
    runTest(UOPs)
  }

}
