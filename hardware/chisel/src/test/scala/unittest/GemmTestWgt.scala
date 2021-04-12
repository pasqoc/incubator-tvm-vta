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
import scala.math.pow

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import vta.core._
import vta.util.config._
import ISA._

import unittest.util._



class TestTensorGemmWgt(c: TensorGemm, gemmCommonSize: Int, gemmInpSize: Int, gemmWgtSize: Int) extends GemmTestGEMMAlgorithms(c, gemmCommonSize, gemmInpSize, gemmWgtSize) {

  

  // Run computation
  
  // Matrices to multiply
  //Define matrix size
  
  // ----------------         ----------------         -----------------------
  // |  commonSize  |         |  commonSize  |         |         wgtSize     |
  // |              |         |              |         |                     |
  // |            i |         |            w |         |                   i |
  // |            n |         |            g |         |                   n |
  // |            p |         |            t |         |                   p |
  // |    inp     S |         |    wgt     S |         |           acc     S |
  // |            i |         |            i |         |                   i |
  // |            z |         |            z |         |                   z |
  // |            e |         |            e |         |                   e |
  // |              |         |              |         -----------------------
  // ----------------         |              |        
  //                          |              |        
  //                          |              |        
  //                          ----------------         
  //Inner index goes along the commonSize side

  val bitSize = 8 // data bitwidth, just for smaller numbers to print

  //Fill with data
  val r = new Random
  val inpGen = new RandomArray(gemmCommonSize, bitSize, r)
  val wgtGen = new RandomArray(gemmCommonSize, bitSize, r)
  val tnzrA = Array.fill(gemmInpSize) { wgtGen.positive }
  val tnzrB = Array.fill(gemmWgtSize) { wgtGen.positive }
  val tnzrAcc  = Array.fill(gemmInpSize) {  Array.fill(gemmWgtSize) {0} }

  var accBegin = 0 //accumulator idx in tnzrAcc memory
  var inpBegin = 0 //inp idx in inp memory
  var wgtBegin = 0 //weight idx in wgt memory
  var uopBegin = 0 //uop_begin index in uop memory
  var printInfo = true 

  runGEMM_Wgt()
 

}
//class TensorGemmWgtTest32x32x32 extends GenericTest( "TensorGemm_Wgt", (p:Parameters) => new TensorGemm()(p), (c:TensorGemm) => new TestTensorGemmWgt(c, 512, 64, 1024, 256, 32, 512))
class TensorGemmWgtTest32x32x32 extends GenericTest( "TensorGemm_Wgt", (p:Parameters) => new TensorGemm()(p), (c:TensorGemm) => new TestTensorGemmWgt(c, 32, 32, 32))




