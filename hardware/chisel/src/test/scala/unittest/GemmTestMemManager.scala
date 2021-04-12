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
import vta.shell._
import vta.util.config._
import ISA._

import unittest.util._



abstract class TestTensorGemmMemManager(c: TensorGemm
                              ) extends PeekPokeTester(c) {

  implicit val p = c.p 
  
  var accBegin: Int
  var inpBegin: Int
  var wgtBegin: Int
  var uopBegin: Int
  val tnzrA: Array[Array[Int]] 
  val tnzrB: Array[Array[Int]]
  val tnzrAcc: Array[Array[Int]]
  var UOPs: Array[TestUOP]
  var printInfo: Boolean
  
  // GOLDEN reference GEMM
  def gemmRef(inp: Array[Array[Int]], wgt: Array[Array[Int]], accum: Array[Array[Int]]) : Array[Array[Int]] = {
    val sizeAH = inp.length
    val sizeAW = inp(0).length
    val sizeBTH = wgt.length
    val sizeBTW = wgt(0).length

    val sizeAccH = accum.length
    val sizeAccW = accum(0).length
    
    require(sizeAccW == sizeBTH, s"$sizeAccW != $sizeBTH")
    require(sizeAccH == sizeAH, s"$sizeAccH != $sizeAH")
    require(sizeAW == sizeBTW, s"$sizeAW != $sizeBTW")

   for (i <- 0 until sizeAH) {
      //println(s"a:${inp(i).mkString(" ")}")
    }
    for (i <- 0 until sizeBTH) {
      //println(s"b:${wgt(i).mkString(" ")}")
    }
    val res = Array.fill(sizeAH) {Array.fill(sizeBTH){0}}
    for (i <- 0 until sizeAH) {
      for (j <- 0 until sizeBTH) {
        var dot = 0
        for (k <- 0 until sizeAW) {
          dot += wgt(j)(k) * inp(i)(k)
        }
        //println(s"$i $j \na:${inp(i).mkString(" ")}\nb:${wgt(j).mkString(" ")}= $dot")
        res(i)(j) = dot + accum(i)(j)
      }
    }
    for (i <- 0 until sizeAH) {
      //println(s"c:${res(i).mkString(" ")}")
    }
    return res
  }
  
  require(c.io.inp.splitWidth == 1, "-F- split tensor access is not supported")
  require(c.io.wgt.splitWidth == 1, "-F- split tensor access is not supported")
  require(c.io.acc.splitWidth == 1, "-F- split tensor access is not supported")
  require(c.io.inp.splitLength == 1, "-F- split tensor access is not supported")
  require(c.io.wgt.splitLength == 1, "-F- split tensor access is not supported")
  require(c.io.acc.splitLength == 1, "-F- split tensor access is not supported")

  // Define managers for TensorGemm events
  

  //tnzrAcc write event management
  var accWrites = 0
  
  // this function responds to DUT data/uop read/write requests
  val uopStateData = new UOPReadStateData (alwaysRead = false,
                                          idxBegin = () => uopBegin,
                                          validIdxSig = c.io.uop.idx.valid,
                                          dataIdxSig  = c.io.uop.idx.bits,
                                          dataU0Sig   = c.io.uop.data.bits.u0,
                                          dataU1Sig   = c.io.uop.data.bits.u1,
                                          dataU2Sig   = c.io.uop.data.bits.u2,
                                          dataValidSig= c.io.uop.data.valid
                                        )
  val inpStateData = new SRAMReadStateData(tensorOuterSize = c.p(CoreKey).batch, 
                                    tensorInnerSize = c.p(CoreKey).blockIn, 
                                    alwaysRead = false,
                                    name = "inp", idxBegin = () => inpBegin,
                                    validDataSig = c.io.inp.rd(0).data.valid, validIdxSig = c.io.inp.rd(0).idx.valid,
                                    dataSig = c.io.inp.rd(0).data.bits, idxSig = c.io.inp.rd(0).idx.bits
                                   )
  val wgtStateData = new SRAMReadStateData(tensorOuterSize = c.p(CoreKey).blockOut, 
                                    tensorInnerSize = c.p(CoreKey).blockIn, 
                                    alwaysRead = false,
                                    name = "wgt", idxBegin = () => wgtBegin,
                                    validDataSig = c.io.wgt.rd(0).data.valid, validIdxSig = c.io.wgt.rd(0).idx.valid,
                                    dataSig = c.io.wgt.rd(0).data.bits, idxSig = c.io.wgt.rd(0).idx.bits
                                   )
  val accStateData = new SRAMReadStateData(tensorOuterSize = c.p(CoreKey).batch, 
                                    tensorInnerSize = c.p(CoreKey).blockOut, 
                                    alwaysRead = false,
                                    name = "tnzrAcc", idxBegin = () => accBegin,
                                    validDataSig = c.io.acc.rd(0).data.valid, validIdxSig = c.io.acc.rd(0).idx.valid,
                                    dataSig = c.io.acc.rd(0).data.bits, idxSig = c.io.acc.rd(0).idx.bits
                                   )
  val memManager = new GeneralMemRead (tester = this)
  def runEvents () = {
    memManager.readUOP(UOPs, uopStateData)
    memManager.readSram(tnzrA, inpStateData)
    memManager.readSram(tnzrB, wgtStateData)
    memManager.readSram(tnzrAcc, accStateData)
    accWrites += memManager.writeSram(tnzrAcc, c.p(CoreKey).batch, c.p(CoreKey).blockOut, 
                         accBegin, c.io.acc.wr(0).valid, c.io.acc.wr(0).bits.data, c.io.acc.wr(0).bits.idx)
    
  }
  
  def defineInst (lp_1:Int, lp_0:Int, wgt_1:Int, wgt_0:Int, 
                  inp_1:Int, inp_0:Int, acc_1:Int, acc_0:Int) = {

    poke(c.io.dec.lp_1, lp_1)  
    poke(c.io.dec.lp_0, lp_0)
    poke(c.io.dec.wgt_1, wgt_1) 
    poke(c.io.dec.wgt_0, wgt_0) 
    poke(c.io.dec.inp_1, inp_1) 
    poke(c.io.dec.inp_0, inp_0)
    poke(c.io.dec.acc_1, acc_1)
    poke(c.io.dec.acc_0, acc_0) 
  
    val numUops = UOPs.length
    poke(c.io.dec.uop_begin, uopBegin)
    poke(c.io.dec.uop_end, uopBegin + numUops)
    poke(c.io.dec.reset, 0)
  }

  def runTest (UOPs: Array[TestUOP]) = {

    val inAcc = tnzrAcc.map(_.clone)
    //start
    poke(c.io.start, 1)
    step(1)
    poke(c.io.start, 0)

    var idx = 1
    while (peek(c.io.done).toInt == 0) {
      //println(s"----- step $idx -----")
      runEvents()
      step(1)
    
      if (idx > 500000) {
        require(false, "-F- Too many iterations")
      }
      idx += 1
    }
    runEvents()
    step(1)
    //--- Finished multiplication --- 

    //Golden result
    val res = gemmRef(tnzrA, tnzrB, inAcc)

    // Check result
    for (idx1 <- 0 until res.length) {
      val row = res(idx1)
      for (idx2 <- 0 until row.length) {
        require(tnzrAcc(idx1)(idx2) == res(idx1)(idx2), s"-F- GEMM failed golden($idx1,$idx2)!=tnzrAcc($idx1,$idx2) ${res(idx1)(idx2)} != ${tnzrAcc(idx1)(idx2)}")
      }
    }
    if (printInfo) println(s"-I- uop reads=${uopStateData.reads} inp reads=${inpStateData.reads} wgt reads=${wgtStateData.reads} acc reads=${accStateData.reads} writes=${accWrites}")
    if (printInfo) println(s"-----GEMM unit test passed in $idx cycles -----")
  }
}
