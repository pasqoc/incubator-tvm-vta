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

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import scala.util.Random
import unittest.util._
import vta.core._
import vta.util.config._

import scala.io._
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper

class MACTester(c: MAC) extends PeekPokeTester(c) {

    poke(c.io.a, -1)
    poke(c.io.b,  7)
    poke(c.io.c, 10)
    step(1)
    expect(c.io.y, 3)
    poke(c.io.a, -2)
    poke(c.io.b,  7)
    poke(c.io.c, 11)
    step(1)
    expect(c.io.y, -3)
}

class MACTest extends GenericTest( "MACTest", (p:Parameters) => new MAC(), (c:MAC) => new MACTester(c))

class PipeAdderTester(c: PipeAdder) extends PeekPokeTester(c) {

    poke(c.io.a, -1)
    poke(c.io.b,  7)
    step(1)
    expect(c.io.y, 6)
    poke(c.io.a, -2)
    poke(c.io.b,  7)
    step(1)
    expect(c.io.y, 5)
}

class PipeAdderTest extends GenericTest( "PipeAdderTest", (p:Parameters) => new PipeAdder(), (c:PipeAdder) => new PipeAdderTester(c))

class AdderTester(c: Adder) extends PeekPokeTester(c) {

    poke(c.io.a, -1)
    poke(c.io.b,  7)
    expect(c.io.y, 6)
    step(1)

    poke(c.io.a, -2)
    poke(c.io.b,  7)
    expect(c.io.y, 5)
    step(1)

}

class AdderTest extends GenericTest( "AdderTest", (p:Parameters) => new Adder(), (c:Adder) => new AdderTester(c))

class DotProductTester(c: DotProduct) extends PeekPokeTester(c) {

    for { i<- 0 until 16} {
        poke(c.io.a(i), if (i %2 == 0) 1 else -1)
        poke(c.io.b(i), i)
    }
    step(1)
    for { i<- 0 until 16} {
        poke(c.io.a(i), if (i %2 == 1) 1 else -1)
        poke(c.io.b(i), i)
    }
    step(1)
    expect(c.io.y, -8)
    step(1)
    expect(c.io.y,  8)
}

class DotProductTest extends GenericTest( "DotProductTest", (p:Parameters) => new DotProduct(), (c:DotProduct) => new DotProductTester(c))

class TensorGemmTester(c: TensorGemmOrig) extends PeekPokeTester(c) {

  poke( c.io.start, 0)

  poke( c.io.dec.reset, 0)
  poke( c.io.dec.uop_begin, 0)
  poke( c.io.dec.uop_end, 1)
  poke( c.io.dec.lp_0, 1)
  poke( c.io.dec.lp_1, 1)
  poke( c.io.dec.acc_0, 1)
  poke( c.io.dec.acc_1, 1)
  poke( c.io.dec.inp_0, 1)
  poke( c.io.dec.inp_1, 1)
  poke( c.io.dec.wgt_0, 1)
  poke( c.io.dec.wgt_1, 1)
  // Don't need empty_0,{push,pop}_{next,prev},op

  poke( c.io.uop.data.bits.u0, 0)
  poke( c.io.uop.data.bits.u1, 0)
  poke( c.io.uop.data.bits.u2, 0)

  val inp = IndexedSeq.fill(c.io.inp.rd(0).data.bits(0).size){ BigInt(1) }
  for { lhs <- c.io.inp.rd(0).data.bits} {
    poke( lhs, inp.reverse)
  }

  val wgt = IndexedSeq.fill(c.io.wgt.rd(0).data.bits(0).size){ BigInt(1) }
  for { lhs <- c.io.wgt.rd(0).data.bits} {
    poke( lhs, wgt.reverse)
  }

  val acc = IndexedSeq.fill(c.io.acc.rd(0).data.bits(0).size){ BigInt(1) }
  for { lhs <- c.io.acc.rd(0).data.bits} {
    poke( lhs, acc.reverse)
  }

  class TensorMasterMock( tm: TensorMaster) {
    poke( tm.rd(0).data.valid, 0)
    var valid = peek(tm.rd(0).idx.valid)
    def logical_step( v: BigInt) {
      poke( tm.rd(0).data.valid, valid)
      valid = peek( tm.rd(0).idx.valid)
      expect( tm.rd(0).idx.valid, v)
    }
  }

  class UopMasterMock( um: UopMaster) {
    poke( um.data.valid, 0)
    var valid = peek( um.idx.valid)
    def logical_step( v: BigInt) {
      poke( um.data.valid, valid)
      valid = peek( um.idx.valid)
      expect( um.idx.valid, v)
    }
  }

  class Mocks {
    val uop_mock = new UopMasterMock( c.io.uop)
    val inp_mock = new TensorMasterMock( c.io.inp)
    val wgt_mock = new TensorMasterMock( c.io.wgt)
    val acc_mock = new TensorMasterMock( c.io.acc)

    def logical_step( sram_valid: BigInt, uop_valid: BigInt) {
      step(1)
      uop_mock.logical_step( uop_valid)
      inp_mock.logical_step( sram_valid)
      wgt_mock.logical_step( sram_valid)
      acc_mock.logical_step( sram_valid)
    }
  }

  val mocks = new Mocks

  poke( c.io.start, 0)

//  reset( 1)
  step( 1)

  expect( c.io.state, c.sIdle)

  poke( c.io.start, 1)
  mocks.logical_step(0, 1)
  expect( c.io.state, c.sReadUop)

  expect( c.io.out.wr(0).valid, 0)
  expect( c.io.acc.wr(0).valid, 0)

  poke( c.io.start, 0)

  mocks.logical_step(0, 0)
  expect( c.io.state, c.sComputeIdx)
  expect( c.io.out.wr(0).valid, 0)
  expect( c.io.acc.wr(0).valid, 0)

  mocks.logical_step(1, 0)
  expect( c.io.state, c.sReadTensor)
  expect( c.io.out.wr(0).valid, 0)
  expect( c.io.acc.wr(0).valid, 0)

  mocks.logical_step(0, 0)
  expect( c.io.state, c.sExe)
  expect( c.io.out.wr(0).valid, 0)
  expect( c.io.acc.wr(0).valid, 0)
  expect( c.io.done, 0)

  mocks.logical_step( 0, 0 /*1*/)
  expect( c.io.state, c.sWait)
  expect( c.io.inflight, 1)

  expect( c.io.out.wr(0).valid, 0)
  expect( c.io.acc.wr(0).valid, 0)

  mocks.logical_step( 0, 0)
  expect( c.io.state, c.sWait)
  expect( c.io.inflight, 1)

  expect( c.io.out.wr(0).valid, 1)
  expect( c.io.acc.wr(0).valid, 1)

  mocks.logical_step( 0, 0)
  expect( c.io.state, c.sWait)
  expect( c.io.inflight, 0)

  expect( c.io.out.wr(0).valid, 0)
  expect( c.io.acc.wr(0).valid, 0)

  mocks.logical_step( 0, 0)
  expect( c.io.state, c.sIdle)
  expect( c.io.inflight, 0)

  expect( c.io.out.wr(0).valid, 0)
  expect( c.io.acc.wr(0).valid, 0)

}

class TensorGemmTest extends GenericTest( "TensorGemm", (p:Parameters) => new TensorGemmOrig()(p), (c:TensorGemmOrig) => new TensorGemmTester(c))

class TensorGemmIdxTester(c: TensorGemmOrig) extends PeekPokeTester(c) {

  poke( c.io.start, 0)

  val uop_begin = 0
  val uop_end = 2
  assert( uop_begin < uop_end)
  val lp_0 = 2
  val lp_1 = 3
  val acc_0 = 1*lp_1
  val inp_0 = 2*lp_1
  val wgt_0 = 4*lp_1
  val acc_1 = 1
  val inp_1 = 2
  val wgt_1 = 4
  val u0 = BigInt( "000", 16)
  val u1 = BigInt( "100", 16)
  val u2 = BigInt( "200", 16)

  poke( c.io.dec.reset, 0)
  poke( c.io.dec.uop_begin, uop_begin)
  poke( c.io.dec.uop_end, uop_end)
  poke( c.io.dec.lp_0, lp_0)
  poke( c.io.dec.lp_1, lp_1)
  poke( c.io.dec.acc_0, acc_0)
  poke( c.io.dec.acc_1, acc_1)
  poke( c.io.dec.inp_0, inp_0)
  poke( c.io.dec.inp_1, inp_1)
  poke( c.io.dec.wgt_0, wgt_0)
  poke( c.io.dec.wgt_1, wgt_1)
  // Don't need empty_0,{push,pop}_{next,prev},op

  poke( c.io.uop.data.bits.u0, u0)
  poke( c.io.uop.data.bits.u1, u1)
  poke( c.io.uop.data.bits.u2, u2)

  val inp = IndexedSeq.fill(c.io.inp.rd(0).data.bits(0).size){ BigInt(1) }
  for { lhs <- c.io.inp.rd(0).data.bits} {
    poke( lhs, inp.reverse)
  }

  val wgt = IndexedSeq.fill(c.io.wgt.rd(0).data.bits(0).size){ BigInt(1) }
  for { lhs <- c.io.wgt.rd(0).data.bits} {
    poke( lhs, wgt.reverse)
  }

  val acc = IndexedSeq.fill(c.io.acc.rd(0).data.bits(0).size){ BigInt(1) }
  for { lhs <- c.io.acc.rd(0).data.bits} {
    poke( lhs, acc.reverse)
  }

  class TensorMasterMock( tm: TensorMaster) {
    poke( tm.rd(0).data.valid, 0)
    var valid = peek(tm.rd(0).idx.valid)
    def logical_step( v: BigInt) {
      poke( tm.rd(0).data.valid, valid)
      valid = peek( tm.rd(0).idx.valid)
      expect( tm.rd(0).idx.valid, v)
    }
  }

  class UopMasterMock( um: UopMaster) {
    poke( um.data.valid, 0)
    var valid = peek( um.idx.valid)
    def logical_step( v: BigInt) {
      poke( um.data.valid, valid)
      valid = peek( um.idx.valid)
      expect( um.idx.valid, v)
    }
  }

  class Mocks {
    val uop_mock = new UopMasterMock( c.io.uop)
    val inp_mock = new TensorMasterMock( c.io.inp)
    val wgt_mock = new TensorMasterMock( c.io.wgt)
    val acc_mock = new TensorMasterMock( c.io.acc)

    val uop_indices = new scala.collection.mutable.Queue[BigInt]
    val acc_indices = new scala.collection.mutable.Queue[BigInt]
    val inp_indices = new scala.collection.mutable.Queue[BigInt]
    val wgt_indices = new scala.collection.mutable.Queue[BigInt]
    val accout_indices = new scala.collection.mutable.Queue[BigInt]
    val out_indices = new scala.collection.mutable.Queue[BigInt]

    def logical_step( sram_valid: BigInt, uop_valid: BigInt) {
      step(1)
      uop_mock.logical_step( uop_valid)
      inp_mock.logical_step( sram_valid)
      wgt_mock.logical_step( sram_valid)
      acc_mock.logical_step( sram_valid)
      if ( peek( c.io.uop.idx.valid) == 1) {
        expect( c.io.uop.idx.bits, uop_indices.dequeue())
      }
      if ( peek( c.io.acc.rd(0).idx.valid) == 1) {
        expect( c.io.acc.rd(0).idx.bits, acc_indices.dequeue())
      }
      if ( peek( c.io.inp.rd(0).idx.valid) == 1) {
        expect( c.io.inp.rd(0).idx.bits, inp_indices.dequeue())
      }
      if ( peek( c.io.wgt.rd(0).idx.valid) == 1) {
        expect( c.io.wgt.rd(0).idx.bits, wgt_indices.dequeue())
      }
      if ( peek( c.io.acc.wr(0).valid) == 1) {
        expect( c.io.acc.wr(0).bits.idx, accout_indices.dequeue())
      }
      if ( peek( c.io.out.wr(0).valid) == 1) {
        expect( c.io.out.wr(0).bits.idx, out_indices.dequeue())
      }
    }

    def test_if_done() {
      assert( uop_indices.isEmpty)
      assert( acc_indices.isEmpty)
      assert( inp_indices.isEmpty)
      assert( wgt_indices.isEmpty)
//      assert( accout_indices.isEmpty)
    }
  }

  val mocks = new Mocks
  for { cnt_o <- 0 until lp_0
        cnt_i <- 0 until lp_1
        uop_idx <- uop_begin until uop_end} {
    mocks.uop_indices.enqueue( uop_idx)
    mocks.acc_indices.enqueue( u0 + acc_0*cnt_o + acc_1*cnt_i)
    mocks.inp_indices.enqueue( u1 + inp_0*cnt_o + inp_1*cnt_i)
    mocks.wgt_indices.enqueue( u2 + wgt_0*cnt_o + wgt_1*cnt_i)
    mocks.accout_indices.enqueue( u0 + acc_0*cnt_o + acc_1*cnt_i)
    mocks.out_indices.enqueue( u0 + acc_0*cnt_o + acc_1*cnt_i)
  }

  poke( c.io.start, 0)

  step( 1)

  expect( c.io.state, c.sIdle)

  poke( c.io.start, 1)


  for { q <- 0 until (uop_end-uop_begin)*lp_0*lp_1} {
    mocks.logical_step(0, 1)
//    expect( c.io.state, c.sReadUop)
    expect( c.io.out.wr(0).valid, 0)
    expect( c.io.acc.wr(0).valid, 0)

    poke( c.io.start, 0)

    mocks.logical_step(0, 0)
//    expect( c.io.state, c.sComputeIdx)
    expect( c.io.out.wr(0).valid, if (q > 0) 1 else 0)
    expect( c.io.acc.wr(0).valid, if (q > 0) 1 else 0)

    mocks.logical_step(1, 0)
//    expect( c.io.state, c.sReadTensor)
    expect( c.io.out.wr(0).valid, 0)
    expect( c.io.acc.wr(0).valid, 0)

    mocks.logical_step(0, 0)
//    expect( c.io.state, c.sExe)
    expect( c.io.out.wr(0).valid, 0)
    expect( c.io.acc.wr(0).valid, 0)
    expect( c.io.done, 0)
  }

  mocks.logical_step( 0, 0)
//  expect( c.io.state, c.sWait)
  expect( c.io.inflight, 1)

  expect( c.io.out.wr(0).valid, 0)
  expect( c.io.acc.wr(0).valid, 0)

  mocks.logical_step( 0, 0)
//  expect( c.io.state, c.sWait)
  expect( c.io.inflight, 1)

  expect( c.io.out.wr(0).valid, 1)
  expect( c.io.acc.wr(0).valid, 1)

  mocks.logical_step( 0, 0)
//  expect( c.io.state, c.sWait)
  expect( c.io.inflight, 0)

  expect( c.io.out.wr(0).valid, 0)
  expect( c.io.acc.wr(0).valid, 0)

  mocks.logical_step( 0, 0)
//  expect( c.io.state, c.sIdle)
  expect( c.io.inflight, 0)

  expect( c.io.out.wr(0).valid, 0)
  expect( c.io.acc.wr(0).valid, 0)

  mocks.test_if_done()

}

class TensorGemmIdxTest extends GenericTest( "TensorGemmIdx", (p:Parameters) => new TensorGemmOrig()(p), (c:TensorGemmOrig) => new TensorGemmIdxTester(c))

class TensorGemmIndexGeneratorTester(c: TensorGemmIndexGenerator) extends PeekPokeTester(c) {


  val uop_begin = 0
  val uop_end = 2
  assert( uop_begin < uop_end)
  val lp_0 = 2
  val lp_1 = 3
  val acc_0 = 1*lp_1
  val inp_0 = 2*lp_1
  val wgt_0 = 4*lp_1
  val acc_1 = 1
  val inp_1 = 2
  val wgt_1 = 4

  poke( c.io.dec.reset, 0)
  poke( c.io.dec.uop_begin, uop_begin)
  poke( c.io.dec.uop_end, uop_end)
  poke( c.io.dec.lp_0, lp_0)
  poke( c.io.dec.lp_1, lp_1)
  poke( c.io.dec.acc_0, acc_0)
  poke( c.io.dec.acc_1, acc_1)
  poke( c.io.dec.inp_0, inp_0)
  poke( c.io.dec.inp_1, inp_1)
  poke( c.io.dec.wgt_0, wgt_0)
  poke( c.io.dec.wgt_1, wgt_1)
  // Don't need empty_0,{push,pop}_{next,prev},op


  class Mocks {
    val uop_indices = new scala.collection.mutable.Queue[BigInt]
    val acc_indices = new scala.collection.mutable.Queue[BigInt]
    val inp_indices = new scala.collection.mutable.Queue[BigInt]
    val wgt_indices = new scala.collection.mutable.Queue[BigInt]

    def logical_step() {
      step(1)
      if ( peek( c.io.valid) == 1) {
        expect( c.io.uop_idx, uop_indices.dequeue())
        expect( c.io.acc_i, acc_indices.dequeue())
        expect( c.io.inp_i, inp_indices.dequeue())
        expect( c.io.wgt_i, wgt_indices.dequeue())
      }
    }

    def test_if_done() {
      println( s"uop_indices remaining: ${uop_indices.size}")
      println( s"acc_indices remaining: ${acc_indices.size}")
      println( s"inp_indices remaining: ${inp_indices.size}")
      println( s"wgt_indices remaining: ${wgt_indices.size}")
      assert( uop_indices.isEmpty)
      assert( acc_indices.isEmpty)
      assert( inp_indices.isEmpty)
      assert( wgt_indices.isEmpty)
    }
  }

  val mocks = new Mocks
  for { cnt_o <- 0 until lp_0
        cnt_i <- 0 until lp_1
        uop_idx <- uop_begin until uop_end} {
    mocks.uop_indices.enqueue( uop_idx)
    mocks.acc_indices.enqueue( acc_0*cnt_o + acc_1*cnt_i)
    mocks.inp_indices.enqueue( inp_0*cnt_o + inp_1*cnt_i)
    mocks.wgt_indices.enqueue( wgt_0*cnt_o + wgt_1*cnt_i)
  }

  poke( c.io.start, 1)
  mocks.logical_step()
  poke( c.io.start, 0)

  val end = (uop_end-uop_begin)*lp_0*lp_1
  var count = 0
  while( peek( c.io.last) == 0 && count < 10*end + 100) { 
    mocks.logical_step()
    count += 1
  }
  mocks.test_if_done()
}

class TensorGemmIndexGeneratorTest extends GenericTest( "TensorGemmIndexGenerator", (p:Parameters) => new TensorGemmIndexGenerator()(p), (c:TensorGemmIndexGenerator) => new TensorGemmIndexGeneratorTester(c))

class TensorGemmPipelinedTester(c: TensorGemmPipelinedSplit) extends PeekPokeTester(c) {


  poke( c.io.start, 0)

  val uop_begin = 0
  val uop_end = 2
  assert( uop_begin < uop_end)
  val lp_0 = 2
  val lp_1 = 3
  val acc_0 = 1*lp_1
  val inp_0 = 2*lp_1
  val wgt_0 = 4*lp_1
  val acc_1 = 1
  val inp_1 = 2
  val wgt_1 = 4
  val u0 = BigInt( "000", 16)
  val u1 = BigInt( "100", 16)
  val u2 = BigInt( "200", 16)

  poke( c.io.dec.reset, 0)
  poke( c.io.dec.uop_begin, uop_begin)
  poke( c.io.dec.uop_end, uop_end)
  poke( c.io.dec.lp_0, lp_0)
  poke( c.io.dec.lp_1, lp_1)
  poke( c.io.dec.acc_0, acc_0)
  poke( c.io.dec.acc_1, acc_1)
  poke( c.io.dec.inp_0, inp_0)
  poke( c.io.dec.inp_1, inp_1)
  poke( c.io.dec.wgt_0, wgt_0)
  poke( c.io.dec.wgt_1, wgt_1)
  // Don't need empty_0,{push,pop}_{next,prev},op

  poke( c.io.uop.data.bits.u0, u0)
  poke( c.io.uop.data.bits.u1, u1)
  poke( c.io.uop.data.bits.u2, u2)

  val inp = IndexedSeq.fill(c.io.inp.rd(0).data.bits(0).size){ BigInt(1) }
  for { lhs <- c.io.inp.rd(0).data.bits} {
    poke( lhs, inp.reverse)
  }

  val wgt = IndexedSeq.fill(c.io.wgt.rd(0).data.bits(0).size){ BigInt(1) }
  for { lhs <- c.io.wgt.rd(0).data.bits} {
    poke( lhs, wgt.reverse)
  }

  val acc = IndexedSeq.fill(c.io.acc.rd(0).data.bits(0).size){ BigInt(1) }
  for { lhs <- c.io.acc.rd(0).data.bits} {
    poke( lhs, acc.reverse)
  }

  class TensorMasterMock( tm: TensorMaster) {
    poke( tm.rd(0).data.valid, 0)
    var valid = peek(tm.rd(0).idx.valid)
    def logical_step( v: Option[BigInt]) {
      poke( tm.rd(0).data.valid, valid)
      valid = peek( tm.rd(0).idx.valid)
      for { x <- v} expect( tm.rd(0).idx.valid, x)
    }
  }

  class UopMasterMock( um: UopMaster) {
    poke( um.data.valid, 0)
    var valid = peek( um.idx.valid)
    def logical_step( v: Option[BigInt]) {
      poke( um.data.valid, valid)
      valid = peek( um.idx.valid)
      for { x <- v} expect( um.idx.valid, x)
    }
  }

  class Mocks {
    val uop_mock = new UopMasterMock( c.io.uop)
    val inp_mock = new TensorMasterMock( c.io.inp)
    val wgt_mock = new TensorMasterMock( c.io.wgt)
    val acc_mock = new TensorMasterMock( c.io.acc)

    val uop_indices = new scala.collection.mutable.Queue[BigInt]
    val acc_indices = new scala.collection.mutable.Queue[BigInt]
    val inp_indices = new scala.collection.mutable.Queue[BigInt]
    val wgt_indices = new scala.collection.mutable.Queue[BigInt]
    val accout_indices = new scala.collection.mutable.Queue[BigInt]
    val out_indices = new scala.collection.mutable.Queue[BigInt]

    def logical_step() {
      step(1)
      uop_mock.logical_step( None)
      inp_mock.logical_step( None)
      wgt_mock.logical_step( None)
      acc_mock.logical_step( None)
      if ( peek( c.io.uop.idx.valid) == 1) {
        expect( c.io.uop.idx.bits, uop_indices.dequeue())
      }
      if ( peek( c.io.acc.rd(0).idx.valid) == 1) {
        expect( c.io.acc.rd(0).idx.bits, acc_indices.dequeue())
      }
      if ( peek( c.io.inp.rd(0).idx.valid) == 1) {
        expect( c.io.inp.rd(0).idx.bits, inp_indices.dequeue())
      }
      if ( peek( c.io.wgt.rd(0).idx.valid) == 1) {
        expect( c.io.wgt.rd(0).idx.bits, wgt_indices.dequeue())
      }
      if ( peek( c.io.acc.wr(0).valid) == 1) {
        expect( c.io.acc.wr(0).bits.idx, accout_indices.dequeue())
      }
      if ( peek( c.io.out.wr(0).valid) == 1) {
        expect( c.io.out.wr(0).bits.idx, out_indices.dequeue())
      }
    }

    def test_if_done() {
      println( s"uop_indices remaining: ${uop_indices.size}")
      println( s"acc_indices remaining: ${acc_indices.size}")
      println( s"inp_indices remaining: ${inp_indices.size}")
      println( s"wgt_indices remaining: ${wgt_indices.size}")
      println( s"accout_indices remaining: ${accout_indices.size}")
      println( s"out_indices remaining: ${out_indices.size}")
      //assert( uop_indices.isEmpty)
      //assert( acc_indices.isEmpty)
      //assert( inp_indices.isEmpty)
      //assert( wgt_indices.isEmpty)
      //assert( accout_indices.isEmpty)
      //assert( out_indices.isEmpty)
    }
  }

  val mocks = new Mocks
  for { cnt_o <- 0 until lp_0
        cnt_i <- 0 until lp_1
        uop_idx <- uop_begin until uop_end} {
    mocks.uop_indices.enqueue( uop_idx)
    mocks.acc_indices.enqueue( u0 + acc_0*cnt_o + acc_1*cnt_i)
    mocks.inp_indices.enqueue( u1 + inp_0*cnt_o + inp_1*cnt_i)
    mocks.wgt_indices.enqueue( u2 + wgt_0*cnt_o + wgt_1*cnt_i)
    mocks.accout_indices.enqueue( u0 + acc_0*cnt_o + acc_1*cnt_i)
    mocks.out_indices.enqueue( u0 + acc_0*cnt_o + acc_1*cnt_i)
  }

  poke( c.io.start, 0)

  step( 1)

  expect( c.io.state, c.sIdle)

  poke( c.io.start, 1)

  var count = 0
  val end = (uop_end-uop_begin)*lp_0*lp_1

  while ( peek( c.io.done) == 0 && count < 10*end + 100) {
    mocks.logical_step()
    poke( c.io.start, 0)
  }

  expect( c.io.done, 1)

  mocks.test_if_done()
}

class TensorGemmPipelinedTest extends GenericTest( "TensorGemmPipelined", (p:Parameters) => new TensorGemmPipelinedSplit()(p), (c:TensorGemmPipelinedSplit) => new TensorGemmPipelinedTester(c))

class TensorGemmResetTester(c: TensorGemm) extends PeekPokeTester(c) {


  poke( c.io.start, 0)

  val uop_begin = 0
  val uop_end = 2
  assert( uop_begin < uop_end)
  val lp_0 = 2
  val lp_1 = 3
  val acc_0 = 1*lp_1
  val inp_0 = 2*lp_1
  val wgt_0 = 4*lp_1
  val acc_1 = 1
  val inp_1 = 2
  val wgt_1 = 4
  val u0 = BigInt( "000", 16)
  val u1 = BigInt( "100", 16)
  val u2 = BigInt( "200", 16)
  val dec_reset = 1

  poke( c.io.dec.reset, dec_reset)
  poke( c.io.dec.uop_begin, uop_begin)
  poke( c.io.dec.uop_end, uop_end)
  poke( c.io.dec.lp_0, lp_0)
  poke( c.io.dec.lp_1, lp_1)
  poke( c.io.dec.acc_0, acc_0)
  poke( c.io.dec.acc_1, acc_1)
  poke( c.io.dec.inp_0, inp_0)
  poke( c.io.dec.inp_1, inp_1)
  poke( c.io.dec.wgt_0, wgt_0)
  poke( c.io.dec.wgt_1, wgt_1)
  // Don't need empty_0,{push,pop}_{next,prev},op

  poke( c.io.uop.data.bits.u0, u0)
  poke( c.io.uop.data.bits.u1, u1)
  poke( c.io.uop.data.bits.u2, u2)

  val inp = IndexedSeq.fill(c.io.inp.rd(0).data.bits(0).size){ BigInt(1) }
  for { lhs <- c.io.inp.rd(0).data.bits} {
    poke( lhs, inp.reverse)
  }

  val wgt = IndexedSeq.fill(c.io.wgt.rd(0).data.bits(0).size){ BigInt(1) }
  for { lhs <- c.io.wgt.rd(0).data.bits} {
    poke( lhs, wgt.reverse)
  }

  val acc = IndexedSeq.fill(c.io.acc.rd(0).data.bits(0).size){ BigInt(1) }
  for { lhs <- c.io.acc.rd(0).data.bits} {
    poke( lhs, acc.reverse)
  }

  class TensorMasterMock( tm: TensorMaster) {
    poke( tm.rd(0).data.valid, 0)
    var valid = peek(tm.rd(0).idx.valid)
    def logical_step( v: Option[BigInt]) {
      poke( tm.rd(0).data.valid, valid)
      valid = peek( tm.rd(0).idx.valid)
      for { x <- v} expect( tm.rd(0).idx.valid, x)
    }
  }

  class UopMasterMock( um: UopMaster) {
    poke( um.data.valid, 0)
    var valid = peek( um.idx.valid)
    def logical_step( v: Option[BigInt]) {
      poke( um.data.valid, valid)
      valid = peek( um.idx.valid)
      for { x <- v} expect( um.idx.valid, x)
    }
  }

  class Mocks {
    val uop_mock = new UopMasterMock( c.io.uop)
    val inp_mock = new TensorMasterMock( c.io.inp)
    val wgt_mock = new TensorMasterMock( c.io.wgt)
    val acc_mock = new TensorMasterMock( c.io.acc)

    val uop_indices = new scala.collection.mutable.Queue[BigInt]
    val acc_indices = new scala.collection.mutable.Queue[BigInt]
    val inp_indices = new scala.collection.mutable.Queue[BigInt]
    val wgt_indices = new scala.collection.mutable.Queue[BigInt]
    val accout_indices = new scala.collection.mutable.Queue[BigInt]
    val out_indices = new scala.collection.mutable.Queue[BigInt]

    def logical_step( sram_valid: BigInt, uop_valid: BigInt) {
      step(1)
      uop_mock.logical_step( None)
      inp_mock.logical_step( None)
      wgt_mock.logical_step( None)
      acc_mock.logical_step( None)
      if ( peek( c.io.uop.idx.valid) == 1) {
        expect( c.io.uop.idx.bits, uop_indices.dequeue())
      }
      if ( peek( c.io.acc.rd(0).idx.valid) == 1) {
        expect( c.io.acc.rd(0).idx.bits, acc_indices.dequeue())
      }
      if ( peek( c.io.inp.rd(0).idx.valid) == 1) {
        expect( c.io.inp.rd(0).idx.bits, inp_indices.dequeue())
      }
      if ( peek( c.io.wgt.rd(0).idx.valid) == 1) {
        expect( c.io.wgt.rd(0).idx.bits, wgt_indices.dequeue())
      }
      if ( peek( c.io.acc.wr(0).valid) == 1) {
        expect( c.io.acc.wr(0).bits.idx, accout_indices.dequeue())
      }
      if ( peek( c.io.out.wr(0).valid) == 1) {
        expect( c.io.out.wr(0).bits.idx, out_indices.dequeue())
      }
    }

    def test_if_done() {
      assert( uop_indices.isEmpty)
      assert( acc_indices.isEmpty)
      assert( inp_indices.isEmpty)
      assert( wgt_indices.isEmpty)
      assert( accout_indices.isEmpty)
      assert( out_indices.isEmpty)
    }
  }

  val mocks = new Mocks
  for { cnt_o <- 0 until lp_0
        cnt_i <- 0 until lp_1
        uop_idx <- uop_begin until uop_end} {
    mocks.uop_indices.enqueue( uop_idx)
    mocks.acc_indices.enqueue( u0 + acc_0*cnt_o + acc_1*cnt_i)
    mocks.inp_indices.enqueue( u1 + inp_0*cnt_o + inp_1*cnt_i)
    mocks.wgt_indices.enqueue( u2 + wgt_0*cnt_o + wgt_1*cnt_i)
    mocks.accout_indices.enqueue( u0 + acc_0*cnt_o + acc_1*cnt_i)

    if ( dec_reset == 0) {
      mocks.out_indices.enqueue( u0 + acc_0*cnt_o + acc_1*cnt_i)
    }
  }

  poke( c.io.start, 0)

  step( 1)

  expect( c.io.state, c.sIdle)

  poke( c.io.start, 1)

  while( peek( c.io.done) == 0) {
    mocks.logical_step( 0, 0)
    poke( c.io.start, 0)
  }

  mocks.test_if_done()
}

class TensorGemmResetTest extends GenericTest( "TensorGemmReset", (p:Parameters) => new TensorGemm()(p), (c:TensorGemm) => new TensorGemmResetTester(c))

class TensorGemmJsonTester(c: TensorGemmPipelinedSplit, fn : String = "jsons/x.json") extends PeekPokeTester(c) {

  val bufferedSource = Source.fromFile( fn)
  val mapper = new ObjectMapper() with ScalaObjectMapper
  mapper.registerModule(DefaultScalaModule)
  val archState = mapper.readValue[Map[String, Object]](bufferedSource.reader())
  bufferedSource.close

  val inst = archState("inst").asInstanceOf[Map[String,String]]

  def build_scratchpad( tag: String) : Array[Array[BigInt]] = {
    val arr = archState(tag).asInstanceOf[Seq[Map[String,Object]]]
    (for { (m,i) <- arr zipWithIndex} yield {
       val idx = BigInt( m("idx").asInstanceOf[String], 16)
       assert( BigInt(i) == idx)
       val vec = m("vec").asInstanceOf[Seq[String]]
       (for { v <- vec} yield {
          BigInt( v, 16)
       }).toArray
    }).toArray
  }

  val inp_scratchpad = build_scratchpad( "inp")
  val wgt_scratchpad = build_scratchpad( "wgt")
  val uop_scratchpad = build_scratchpad( "uop")
  val acc_scratchpad = build_scratchpad( "acc_i")
  val acc_o_scratchpad = build_scratchpad( "acc_o")

  poke( c.io.start, 0)

  val dec_reset = BigInt( inst("reset"), 16)
  val uop_begin = BigInt( inst("uop_begin"), 16)
  val uop_end = BigInt( inst("uop_end"), 16)
  assert( uop_begin < uop_end)
  val lp_0 = BigInt( inst("lp_0"), 16)
  val lp_1 = BigInt( inst("lp_1"), 16)
  val acc_0 = BigInt( inst("acc_0"), 16)
  val inp_0 = BigInt( inst("inp_0"), 16)
  val wgt_0 = BigInt( inst("wgt_0"), 16)
  val acc_1 = BigInt( inst("acc_1"), 16)
  val inp_1 = BigInt( inst("inp_1"), 16)
  val wgt_1 = BigInt( inst("wgt_1"), 16)

  poke( c.io.dec.reset, dec_reset)

  poke( c.io.dec.uop_begin, uop_begin)
  poke( c.io.dec.uop_end, uop_end)
  poke( c.io.dec.lp_0, lp_0)
  poke( c.io.dec.lp_1, lp_1)
  poke( c.io.dec.acc_0, acc_0)
  poke( c.io.dec.acc_1, acc_1)
  poke( c.io.dec.inp_0, inp_0)
  poke( c.io.dec.inp_1, inp_1)
  poke( c.io.dec.wgt_0, wgt_0)
  poke( c.io.dec.wgt_1, wgt_1)
  // Don't need empty_0,{push,pop}_{next,prev},op

  class TensorMasterMock( tm: TensorMaster, scratchpad : Array[Array[BigInt]]) {
    poke( tm.rd(0).data.valid, 0)
    var valid = peek(tm.rd(0).idx.valid)
    var idx : Int = 0
    def logical_step() {
      if (valid == 1) {
        poke( tm.rd(0).data.valid, 1)
	val cols = tm.rd(0).data.bits(0).size
	for { i <- 0 until tm.rd(0).data.bits.size
	      j <- 0 until cols} {
	  poke( tm.rd(0).data.bits(i)(j), scratchpad(idx)(i*cols+j))
        }
      } else {
        poke( tm.rd(0).data.valid, 0)
      }
      valid = peek( tm.rd(0).idx.valid)
      idx = peek( tm.rd(0).idx.bits).toInt
    }
  }

  class TensorMasterMockWr( tm: TensorMaster, scratchpad : Array[Array[BigInt]]) {
    def logical_step() {
      if ( peek( tm.wr(0).valid) == 1) {
        val idx = peek( tm.wr(0).bits.idx).toInt
	val cols = tm.wr(0).bits.data(0).size
	for { i <- 0 until tm.wr(0).bits.data.size
	      j <- 0 until cols} {
 	  scratchpad(idx)(i*cols+j) = peek( tm.wr(0).bits.data(i)(j))
        }
      }
    }
  }

  class UopMasterMock( um: UopMaster, scratchpad: Array[Array[BigInt]]) {
    poke( um.data.valid, 0)
    var valid = peek( um.idx.valid)
    var idx : Int = 0
    def logical_step() {
      if (valid == 1) {
        poke( um.data.valid, 1)
	poke( um.data.bits.u0, scratchpad(idx)(0))
	poke( um.data.bits.u1, scratchpad(idx)(1))
	poke( um.data.bits.u2, scratchpad(idx)(2))
      } else {
        poke( um.data.valid, 0)
      }
      valid = peek( um.idx.valid)
      idx = peek( um.idx.bits).toInt
    }
  }

  class Mocks {
    val uop_mock = new UopMasterMock( c.io.uop, uop_scratchpad)
    val inp_mock = new TensorMasterMock( c.io.inp, inp_scratchpad)
    val wgt_mock = new TensorMasterMock( c.io.wgt, wgt_scratchpad)
    val acc_mock = new TensorMasterMock( c.io.acc, acc_scratchpad)
    val acc_mock_wr = new TensorMasterMockWr( c.io.acc, acc_scratchpad)

    val uop_indices = new scala.collection.mutable.Queue[BigInt]
    val acc_indices = new scala.collection.mutable.Queue[BigInt]
    val inp_indices = new scala.collection.mutable.Queue[BigInt]
    val wgt_indices = new scala.collection.mutable.Queue[BigInt]
    val accout_indices = new scala.collection.mutable.Queue[BigInt]
    val out_indices = new scala.collection.mutable.Queue[BigInt]

    def logical_step() {
      step(1)
      uop_mock.logical_step()
      inp_mock.logical_step()
      wgt_mock.logical_step()
      acc_mock.logical_step()
      acc_mock_wr.logical_step()

      if ( peek( c.io.uop.idx.valid) == 1) {
        expect( c.io.uop.idx.bits, uop_indices.dequeue())
      }
      if ( peek( c.io.acc.rd(0).idx.valid) == 1) {
        expect( c.io.acc.rd(0).idx.bits, acc_indices.dequeue())
      }
      if ( peek( c.io.inp.rd(0).idx.valid) == 1) {
        expect( c.io.inp.rd(0).idx.bits, inp_indices.dequeue())
      }
      if ( peek( c.io.wgt.rd(0).idx.valid) == 1) {
        expect( c.io.wgt.rd(0).idx.bits, wgt_indices.dequeue())
      }
      if ( peek( c.io.acc.wr(0).valid) == 1) {
        expect( c.io.acc.wr(0).bits.idx, accout_indices.dequeue())
      }
      if ( peek( c.io.out.wr(0).valid) == 1) {
        expect( c.io.out.wr(0).bits.idx, out_indices.dequeue())
      }
    }

    def test_if_done() {
      println( s"uop_indices should be empty ${uop_indices.size}")
      println( s"acc_indices should be empty ${acc_indices.size}")
      println( s"inp_indices should be empty ${inp_indices.size}")
      println( s"wgt_indices should be empty ${wgt_indices.size}")
      println( s"accout_indices should be empty ${accout_indices.size}")
      println( s"out_indices should be empty ${out_indices.size}")
    }

    def check() = {
      val result = for { ((x,y),idx) <- (acc_scratchpad, acc_o_scratchpad).zipped.toList.zipWithIndex} yield {
        (for { ((xx,yy),jdx) <- (x,y).zipped.toList.zipWithIndex} yield {
          if ( xx != yy) {
            println( s"Value mismatch at $idx $jdx: $xx (actual) != $yy (expected)")
          }
          xx == yy
        }).reduce((x,y) => x&&y)
      }
//      println(result)
      val result2 = result.reduce((x,y) => x&&y)
//      println(result2.toStr)
      result2
    }

  }

  val mocks = new Mocks

  for { cnt_o <- BigInt(0) until lp_0
        cnt_i <- BigInt(0) until lp_1
        uop_idx <- uop_begin until uop_end} {

    val u0 = uop_scratchpad(uop_idx.toInt)(0)
    val u1 = uop_scratchpad(uop_idx.toInt)(1)
    val u2 = uop_scratchpad(uop_idx.toInt)(2)

    mocks.uop_indices.enqueue( uop_idx)
    mocks.acc_indices.enqueue( u0 + acc_0*cnt_o + acc_1*cnt_i)
    mocks.inp_indices.enqueue( u1 + inp_0*cnt_o + inp_1*cnt_i)
    mocks.wgt_indices.enqueue( u2 + wgt_0*cnt_o + wgt_1*cnt_i)
    mocks.accout_indices.enqueue( u0 + acc_0*cnt_o + acc_1*cnt_i)

    if ( dec_reset == 0) {
      mocks.out_indices.enqueue( u0 + acc_0*cnt_o + acc_1*cnt_i)
    }
  }

  poke( c.io.start, 0)

  mocks.logical_step()

  expect( c.io.state, c.sIdle)

  poke( c.io.start, 1)

  val total_steps = (uop_end-uop_begin)*lp_0*lp_1

  val max_count = 100 + 4*total_steps
  var count = 0
  while ( peek(c.io.done) == 0 && count < max_count) {
    if (count % 100 == 0) {
      println( s"logical_step $count")
    }
    mocks.logical_step()
    if ( count == 0 ) {
      poke( c.io.start, 0)
    }
    count += 1
  }

  if ( peek(c.io.done) == 1) {
    println( s"Signal done high after $count steps.")
  } else {
    println( s"Signal done never high even after $count steps.")
    assert( false)
  }

  mocks.logical_step()
  expect( c.io.done, 0)

  val cc = mocks.check()
  println( s"Checking acc with acc_o ${cc}")
  assert( cc)

  println( s"Total active steps: ${total_steps}")
  mocks.test_if_done()

}

class TensorGemmJsonTestx extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelinedSplit()(p), (c:TensorGemmPipelinedSplit) => new TensorGemmJsonTester(c, "jsons/x.json"))

class TensorGemmJsonTesty extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelinedSplit()(p), (c:TensorGemmPipelinedSplit) => new TensorGemmJsonTester(c, "jsons/y.json"))

/*
class TensorGemmJsonTest0000 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0000.json"))
class TensorGemmJsonTest0001 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0001.json"))
class TensorGemmJsonTest0002 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0002.json"))
class TensorGemmJsonTest0003 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0003.json"))
class TensorGemmJsonTest0004 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0004.json"))
class TensorGemmJsonTest0005 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0005.json"))
class TensorGemmJsonTest0006 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0006.json"))
class TensorGemmJsonTest0007 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0007.json"))
class TensorGemmJsonTest0008 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0008.json"))
class TensorGemmJsonTest0009 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0009.json"))
class TensorGemmJsonTest0010 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0010.json"))
class TensorGemmJsonTest0011 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0011.json"))
class TensorGemmJsonTest0012 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0012.json"))
class TensorGemmJsonTest0013 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0013.json"))
class TensorGemmJsonTest0014 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0014.json"))
class TensorGemmJsonTest0015 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0015.json"))
class TensorGemmJsonTest0016 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0016.json"))
class TensorGemmJsonTest0017 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0017.json"))
class TensorGemmJsonTest0018 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0018.json"))
class TensorGemmJsonTest0019 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0019.json"))
class TensorGemmJsonTest0020 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0020.json"))
class TensorGemmJsonTest0021 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0021.json"))
class TensorGemmJsonTest0022 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0022.json"))
class TensorGemmJsonTest0023 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0023.json"))
class TensorGemmJsonTest0024 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0024.json"))
class TensorGemmJsonTest0025 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0025.json"))
class TensorGemmJsonTest0026 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0026.json"))
class TensorGemmJsonTest0027 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0027.json"))
class TensorGemmJsonTest0028 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0028.json"))
class TensorGemmJsonTest0029 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0029.json"))
class TensorGemmJsonTest0030 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0030.json"))
class TensorGemmJsonTest0031 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0031.json"))
class TensorGemmJsonTest0032 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0032.json"))
class TensorGemmJsonTest0033 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0033.json"))
class TensorGemmJsonTest0034 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0034.json"))
class TensorGemmJsonTest0035 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0035.json"))
class TensorGemmJsonTest0036 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0036.json"))
class TensorGemmJsonTest0037 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0037.json"))
class TensorGemmJsonTest0038 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0038.json"))
class TensorGemmJsonTest0039 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0039.json"))
class TensorGemmJsonTest0040 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0040.json"))
class TensorGemmJsonTest0041 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0041.json"))
class TensorGemmJsonTest0042 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0042.json"))
class TensorGemmJsonTest0043 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0043.json"))
class TensorGemmJsonTest0044 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0044.json"))
class TensorGemmJsonTest0045 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0045.json"))
class TensorGemmJsonTest0046 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0046.json"))
class TensorGemmJsonTest0047 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0047.json"))
class TensorGemmJsonTest0048 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0048.json"))
class TensorGemmJsonTest0049 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0049.json"))
class TensorGemmJsonTest0050 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0050.json"))
class TensorGemmJsonTest0051 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0051.json"))
class TensorGemmJsonTest0052 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0052.json"))
class TensorGemmJsonTest0053 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0053.json"))
class TensorGemmJsonTest0054 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0054.json"))
class TensorGemmJsonTest0055 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0055.json"))
class TensorGemmJsonTest0056 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0056.json"))
class TensorGemmJsonTest0057 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0057.json"))
class TensorGemmJsonTest0058 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0058.json"))
class TensorGemmJsonTest0059 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0059.json"))
class TensorGemmJsonTest0060 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0060.json"))
class TensorGemmJsonTest0061 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0061.json"))
class TensorGemmJsonTest0062 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0062.json"))
class TensorGemmJsonTest0063 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0063.json"))
class TensorGemmJsonTest0064 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0064.json"))
class TensorGemmJsonTest0065 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0065.json"))
class TensorGemmJsonTest0066 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0066.json"))
class TensorGemmJsonTest0067 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0067.json"))
class TensorGemmJsonTest0068 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0068.json"))
class TensorGemmJsonTest0069 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0069.json"))
class TensorGemmJsonTest0070 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0070.json"))
class TensorGemmJsonTest0071 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0071.json"))
class TensorGemmJsonTest0072 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0072.json"))
class TensorGemmJsonTest0073 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0073.json"))
class TensorGemmJsonTest0074 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0074.json"))
class TensorGemmJsonTest0075 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0075.json"))
class TensorGemmJsonTest0076 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0076.json"))
class TensorGemmJsonTest0077 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0077.json"))
class TensorGemmJsonTest0078 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0078.json"))
class TensorGemmJsonTest0079 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0079.json"))
class TensorGemmJsonTest0080 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0080.json"))
class TensorGemmJsonTest0081 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0081.json"))
class TensorGemmJsonTest0082 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0082.json"))
class TensorGemmJsonTest0083 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0083.json"))
class TensorGemmJsonTest0084 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0084.json"))
class TensorGemmJsonTest0085 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0085.json"))
class TensorGemmJsonTest0086 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0086.json"))
class TensorGemmJsonTest0087 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0087.json"))
class TensorGemmJsonTest0088 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0088.json"))
class TensorGemmJsonTest0089 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0089.json"))
class TensorGemmJsonTest0090 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0090.json"))
class TensorGemmJsonTest0091 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0091.json"))
class TensorGemmJsonTest0092 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0092.json"))
class TensorGemmJsonTest0093 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0093.json"))
class TensorGemmJsonTest0094 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0094.json"))
class TensorGemmJsonTest0095 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0095.json"))
class TensorGemmJsonTest0096 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0096.json"))
class TensorGemmJsonTest0097 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0097.json"))
class TensorGemmJsonTest0098 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0098.json"))
class TensorGemmJsonTest0099 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0099.json"))
class TensorGemmJsonTest0100 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0100.json"))
class TensorGemmJsonTest0101 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0101.json"))
class TensorGemmJsonTest0102 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0102.json"))
class TensorGemmJsonTest0103 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0103.json"))
class TensorGemmJsonTest0104 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0104.json"))
class TensorGemmJsonTest0105 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0105.json"))
class TensorGemmJsonTest0106 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0106.json"))
class TensorGemmJsonTest0107 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0107.json"))
class TensorGemmJsonTest0108 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0108.json"))
class TensorGemmJsonTest0109 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0109.json"))
class TensorGemmJsonTest0110 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0110.json"))
class TensorGemmJsonTest0111 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0111.json"))
class TensorGemmJsonTest0112 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0112.json"))
class TensorGemmJsonTest0113 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0113.json"))
class TensorGemmJsonTest0114 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0114.json"))
class TensorGemmJsonTest0115 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0115.json"))
class TensorGemmJsonTest0116 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0116.json"))
class TensorGemmJsonTest0117 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0117.json"))
class TensorGemmJsonTest0118 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0118.json"))
class TensorGemmJsonTest0119 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0119.json"))
class TensorGemmJsonTest0120 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0120.json"))
class TensorGemmJsonTest0121 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0121.json"))
class TensorGemmJsonTest0122 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0122.json"))
class TensorGemmJsonTest0123 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0123.json"))
class TensorGemmJsonTest0124 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0124.json"))
class TensorGemmJsonTest0125 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0125.json"))
class TensorGemmJsonTest0126 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0126.json"))
class TensorGemmJsonTest0127 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0127.json"))
class TensorGemmJsonTest0128 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0128.json"))
class TensorGemmJsonTest0129 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0129.json"))
class TensorGemmJsonTest0130 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0130.json"))
class TensorGemmJsonTest0131 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0131.json"))
class TensorGemmJsonTest0132 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0132.json"))
class TensorGemmJsonTest0133 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0133.json"))
class TensorGemmJsonTest0134 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0134.json"))
class TensorGemmJsonTest0135 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0135.json"))
class TensorGemmJsonTest0136 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0136.json"))
class TensorGemmJsonTest0137 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0137.json"))
class TensorGemmJsonTest0138 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0138.json"))
class TensorGemmJsonTest0139 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0139.json"))
class TensorGemmJsonTest0140 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0140.json"))
class TensorGemmJsonTest0141 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0141.json"))
class TensorGemmJsonTest0142 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0142.json"))
class TensorGemmJsonTest0143 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0143.json"))
class TensorGemmJsonTest0144 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0144.json"))
class TensorGemmJsonTest0145 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0145.json"))
class TensorGemmJsonTest0146 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0146.json"))
class TensorGemmJsonTest0147 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0147.json"))
class TensorGemmJsonTest0148 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0148.json"))
class TensorGemmJsonTest0149 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0149.json"))
class TensorGemmJsonTest0150 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0150.json"))
class TensorGemmJsonTest0151 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0151.json"))
class TensorGemmJsonTest0152 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0152.json"))
class TensorGemmJsonTest0153 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0153.json"))
class TensorGemmJsonTest0154 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0154.json"))
class TensorGemmJsonTest0155 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0155.json"))
class TensorGemmJsonTest0156 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0156.json"))
class TensorGemmJsonTest0157 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0157.json"))
class TensorGemmJsonTest0158 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0158.json"))
class TensorGemmJsonTest0159 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0159.json"))
class TensorGemmJsonTest0160 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0160.json"))
class TensorGemmJsonTest0161 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0161.json"))
class TensorGemmJsonTest0162 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0162.json"))
class TensorGemmJsonTest0163 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0163.json"))
class TensorGemmJsonTest0164 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0164.json"))
class TensorGemmJsonTest0165 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0165.json"))
class TensorGemmJsonTest0166 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0166.json"))
class TensorGemmJsonTest0167 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0167.json"))
class TensorGemmJsonTest0168 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0168.json"))
class TensorGemmJsonTest0169 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0169.json"))
class TensorGemmJsonTest0170 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0170.json"))
class TensorGemmJsonTest0171 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0171.json"))
class TensorGemmJsonTest0172 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0172.json"))
class TensorGemmJsonTest0173 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0173.json"))
class TensorGemmJsonTest0174 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0174.json"))
class TensorGemmJsonTest0175 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0175.json"))
class TensorGemmJsonTest0176 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0176.json"))
class TensorGemmJsonTest0177 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0177.json"))
class TensorGemmJsonTest0178 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0178.json"))
class TensorGemmJsonTest0179 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0179.json"))
class TensorGemmJsonTest0180 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0180.json"))
class TensorGemmJsonTest0181 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0181.json"))
class TensorGemmJsonTest0182 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0182.json"))
class TensorGemmJsonTest0183 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0183.json"))
class TensorGemmJsonTest0184 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0184.json"))
class TensorGemmJsonTest0185 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0185.json"))
class TensorGemmJsonTest0186 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0186.json"))
class TensorGemmJsonTest0187 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0187.json"))
class TensorGemmJsonTest0188 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0188.json"))
class TensorGemmJsonTest0189 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0189.json"))
class TensorGemmJsonTest0190 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0190.json"))
class TensorGemmJsonTest0191 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0191.json"))
class TensorGemmJsonTest0192 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0192.json"))
class TensorGemmJsonTest0193 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0193.json"))
class TensorGemmJsonTest0194 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0194.json"))
class TensorGemmJsonTest0195 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0195.json"))
class TensorGemmJsonTest0196 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0196.json"))
class TensorGemmJsonTest0197 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0197.json"))
class TensorGemmJsonTest0198 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0198.json"))
class TensorGemmJsonTest0199 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0199.json"))
class TensorGemmJsonTest0200 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0200.json"))
class TensorGemmJsonTest0201 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0201.json"))
class TensorGemmJsonTest0202 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0202.json"))
class TensorGemmJsonTest0203 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0203.json"))
class TensorGemmJsonTest0204 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0204.json"))
class TensorGemmJsonTest0205 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0205.json"))
class TensorGemmJsonTest0206 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0206.json"))
class TensorGemmJsonTest0207 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0207.json"))
class TensorGemmJsonTest0208 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0208.json"))
class TensorGemmJsonTest0209 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0209.json"))
class TensorGemmJsonTest0210 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0210.json"))
class TensorGemmJsonTest0211 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0211.json"))
class TensorGemmJsonTest0212 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0212.json"))
class TensorGemmJsonTest0213 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0213.json"))
class TensorGemmJsonTest0214 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0214.json"))
class TensorGemmJsonTest0215 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0215.json"))
class TensorGemmJsonTest0216 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0216.json"))
class TensorGemmJsonTest0217 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0217.json"))
class TensorGemmJsonTest0218 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0218.json"))
class TensorGemmJsonTest0219 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0219.json"))
class TensorGemmJsonTest0220 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0220.json"))
class TensorGemmJsonTest0221 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0221.json"))
class TensorGemmJsonTest0222 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0222.json"))
class TensorGemmJsonTest0223 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0223.json"))
class TensorGemmJsonTest0224 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0224.json"))
class TensorGemmJsonTest0225 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0225.json"))
class TensorGemmJsonTest0226 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0226.json"))
class TensorGemmJsonTest0227 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0227.json"))
class TensorGemmJsonTest0228 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0228.json"))
class TensorGemmJsonTest0229 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0229.json"))
class TensorGemmJsonTest0230 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0230.json"))
class TensorGemmJsonTest0231 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0231.json"))
class TensorGemmJsonTest0232 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0232.json"))
class TensorGemmJsonTest0233 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0233.json"))
class TensorGemmJsonTest0234 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0234.json"))
class TensorGemmJsonTest0235 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0235.json"))
class TensorGemmJsonTest0236 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0236.json"))
class TensorGemmJsonTest0237 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0237.json"))
class TensorGemmJsonTest0238 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0238.json"))
class TensorGemmJsonTest0239 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0239.json"))
class TensorGemmJsonTest0240 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0240.json"))
class TensorGemmJsonTest0241 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0241.json"))
class TensorGemmJsonTest0242 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0242.json"))
class TensorGemmJsonTest0243 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0243.json"))
class TensorGemmJsonTest0244 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0244.json"))
class TensorGemmJsonTest0245 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0245.json"))
class TensorGemmJsonTest0246 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0246.json"))
class TensorGemmJsonTest0247 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0247.json"))
class TensorGemmJsonTest0248 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0248.json"))
class TensorGemmJsonTest0249 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0249.json"))
class TensorGemmJsonTest0250 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0250.json"))
class TensorGemmJsonTest0251 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0251.json"))
class TensorGemmJsonTest0252 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0252.json"))
class TensorGemmJsonTest0253 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0253.json"))
class TensorGemmJsonTest0254 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0254.json"))
class TensorGemmJsonTest0255 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0255.json"))
class TensorGemmJsonTest0256 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0256.json"))
class TensorGemmJsonTest0257 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0257.json"))
class TensorGemmJsonTest0258 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0258.json"))
class TensorGemmJsonTest0259 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0259.json"))
class TensorGemmJsonTest0260 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0260.json"))
class TensorGemmJsonTest0261 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0261.json"))
class TensorGemmJsonTest0262 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0262.json"))
class TensorGemmJsonTest0263 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0263.json"))
class TensorGemmJsonTest0264 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0264.json"))
class TensorGemmJsonTest0265 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0265.json"))
class TensorGemmJsonTest0266 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0266.json"))
class TensorGemmJsonTest0267 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0267.json"))
class TensorGemmJsonTest0268 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0268.json"))
class TensorGemmJsonTest0269 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0269.json"))
class TensorGemmJsonTest0270 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0270.json"))
class TensorGemmJsonTest0271 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0271.json"))
class TensorGemmJsonTest0272 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0272.json"))
class TensorGemmJsonTest0273 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0273.json"))
class TensorGemmJsonTest0274 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0274.json"))
class TensorGemmJsonTest0275 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0275.json"))
class TensorGemmJsonTest0276 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0276.json"))
class TensorGemmJsonTest0277 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0277.json"))
class TensorGemmJsonTest0278 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0278.json"))
class TensorGemmJsonTest0279 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0279.json"))
class TensorGemmJsonTest0280 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0280.json"))
class TensorGemmJsonTest0281 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0281.json"))
class TensorGemmJsonTest0282 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0282.json"))
class TensorGemmJsonTest0283 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0283.json"))
class TensorGemmJsonTest0284 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0284.json"))
class TensorGemmJsonTest0285 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0285.json"))
class TensorGemmJsonTest0286 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0286.json"))
class TensorGemmJsonTest0287 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0287.json"))
class TensorGemmJsonTest0288 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0288.json"))
class TensorGemmJsonTest0289 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0289.json"))
class TensorGemmJsonTest0290 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0290.json"))
class TensorGemmJsonTest0291 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0291.json"))
class TensorGemmJsonTest0292 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0292.json"))
class TensorGemmJsonTest0293 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0293.json"))
class TensorGemmJsonTest0294 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0294.json"))
class TensorGemmJsonTest0295 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0295.json"))
class TensorGemmJsonTest0296 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0296.json"))
class TensorGemmJsonTest0297 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0297.json"))
class TensorGemmJsonTest0298 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0298.json"))
class TensorGemmJsonTest0299 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0299.json"))
class TensorGemmJsonTest0300 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0300.json"))
class TensorGemmJsonTest0301 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0301.json"))
class TensorGemmJsonTest0302 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0302.json"))
class TensorGemmJsonTest0303 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0303.json"))
class TensorGemmJsonTest0304 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0304.json"))
class TensorGemmJsonTest0305 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0305.json"))
class TensorGemmJsonTest0306 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0306.json"))
class TensorGemmJsonTest0307 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0307.json"))
class TensorGemmJsonTest0308 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0308.json"))
class TensorGemmJsonTest0309 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0309.json"))
class TensorGemmJsonTest0310 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0310.json"))
class TensorGemmJsonTest0311 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0311.json"))
class TensorGemmJsonTest0312 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0312.json"))
class TensorGemmJsonTest0313 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0313.json"))
class TensorGemmJsonTest0314 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0314.json"))
class TensorGemmJsonTest0315 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0315.json"))
class TensorGemmJsonTest0316 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0316.json"))
class TensorGemmJsonTest0317 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0317.json"))
class TensorGemmJsonTest0318 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0318.json"))
class TensorGemmJsonTest0319 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0319.json"))
class TensorGemmJsonTest0320 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0320.json"))
class TensorGemmJsonTest0321 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0321.json"))
class TensorGemmJsonTest0322 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0322.json"))
class TensorGemmJsonTest0323 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0323.json"))
class TensorGemmJsonTest0324 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0324.json"))
class TensorGemmJsonTest0325 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0325.json"))
class TensorGemmJsonTest0326 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0326.json"))
class TensorGemmJsonTest0327 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0327.json"))
class TensorGemmJsonTest0328 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0328.json"))
class TensorGemmJsonTest0329 extends GenericTest( "TensorGemmJson", (p:Parameters) => new TensorGemmPipelined()(p), (c:TensorGemmPipelined) => new TensorGemmJsonTester(c, "jsons/simLOG0329.json"))
*/
