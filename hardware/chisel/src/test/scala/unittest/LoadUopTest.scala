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
import chisel3.iotesters.PeekPokeTester
import vta.core._
import vta.shell._
import vta.util.config._

class LoadUopTester(c: LoadUop, sram_offset: BigInt = 0, dram_offset: BigInt = 0, xsize: BigInt = 8, wait_max: Int = 4) extends PeekPokeTester(c) {

  class VME() {
    val uopsPerMemXfer = c.p(ShellKey).memParams.dataBits / c.p(CoreKey).uopBits
    assert(c.p(ShellKey).memParams.dataBits % c.p(CoreKey).uopBits == 0)
    assert(uopsPerMemXfer == 1 || uopsPerMemXfer == 2)

    var len = BigInt(0)
    var addr = BigInt(0) // In bytes
    var cnt = BigInt(0)  // Count in 8-byte cache lines
    var wait_counter = 0
    def logical_step() {

      if ( wait_counter == 0 && len > 0) {
        poke( c.io.vme_rd.data.valid, 1)

        if (uopsPerMemXfer == 2) {
          // pack in two 32-bit words into 64-bits (chisel vector ordering)
          val v = (addr>>2) + (2*cnt)
          poke( c.io.vme_rd.data.bits.data, ((v+1) << 32) + v)
        } else {
          // pack in a single 64-bit word
          val v = (addr>>3) + (cnt)
          poke( c.io.vme_rd.data.bits.data, v)
        }

        len -= 1
        cnt += 1
        poke( c.io.vme_rd.cmd.ready, if ( len == 0) 1 else 0)
      } else {
        poke( c.io.vme_rd.data.valid, 0)
        poke( c.io.vme_rd.cmd.ready, if ( wait_counter == 0) 1 else 0)
      }

      if ( wait_counter > 0) {
        wait_counter -= 1
      }

      if ( peek( c.io.vme_rd.cmd.valid) == 1) {
        assert( len == 0)
        // bits.len means one less than number of transfers
        len = peek( c.io.vme_rd.cmd.bits.len) + 1
        addr = peek( c.io.vme_rd.cmd.bits.addr)
        assert( addr % 8 == 0)
        cnt = 0
        wait_counter = wait_max
      }

    }
  }

  val vme = new VME

  def logical_step() = {
    vme.logical_step()
    val done = peek( c.io.done)
    step(1)
    done
  }

  println("LoadUopTester")      

  poke( c.io.start, 0)
  poke( c.io.baddr, 0)
  poke( c.io.dec.xsize, xsize)

  poke( c.io.dec.sram_offset, sram_offset)
  poke( c.io.dec.dram_offset, dram_offset)

  poke( c.io.vme_rd.cmd.ready, 1)

  poke( c.io.vme_rd.data.valid, 0)
  poke( c.io.vme_rd.data.bits.data, 0)

  poke( c.io.uop.idx.valid, 0)
  poke( c.io.uop.idx.bits, 0)

  logical_step()

  val snapshot_size = 40
  val initial_values = collection.mutable.Map[Int,BigInt]()

  poke( c.io.uop.idx.valid, 1)
  for { i <- BigInt(0) until snapshot_size} {
    poke( c.io.uop.idx.bits, i)
    logical_step()
    expect( c.io.uop.data.valid, 1)
    initial_values(i.toInt) = peek( c.io.uop.data.bits.u0)
  }
  poke( c.io.uop.idx.valid, 0)
  logical_step()
  expect( c.io.uop.data.valid, 0)

  poke( c.io.start, 1)
  logical_step()

  var count = 0
  var done = BigInt(0)
  val max_count = 100
  while ( done == 0  && count < max_count) {
    poke( c.io.start, 0)
    done = logical_step()
    count += 1
  }

  assert( count < 100)

  poke( c.io.uop.idx.valid, 1)
  /*
  for { i <- BigInt(0) until xsize} {
    poke( c.io.uop.idx.bits, i + sram_offset)
    logical_step()
    expect( c.io.uop.data.valid, 1)
    expect( c.io.uop.data.bits.u0, i + dram_offset)
  }
  */
  for { i <- BigInt(0) until snapshot_size} {
    poke( c.io.uop.idx.bits, i)
    logical_step()
    poke( c.io.uop.idx.bits, snapshot_size) // check if read latency is one
    expect( c.io.uop.data.valid, 1)
    if ( sram_offset <= i && i < sram_offset + xsize) {
      expect( c.io.uop.data.bits.u0, i - sram_offset + dram_offset)
    } else {
      //Not working now
      expect( c.io.uop.data.bits.u0, initial_values(i.toInt))
    }
  }

  poke( c.io.uop.idx.valid, 0)
  logical_step()
  expect( c.io.uop.data.valid, 0)

}

class LoadUopTest008 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=0, dram_offset=0, xsize=8))

class LoadUopTest007 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=0, dram_offset=0, xsize=7))
class LoadUopTest009 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=0, dram_offset=0, xsize=9))

class LoadUopTest208 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=2, dram_offset=0, xsize=8))
class LoadUopTest028 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=0, dram_offset=2, xsize=8))
class LoadUopTest228 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=2, dram_offset=2, xsize=8))

class LoadUopTest108 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=1, dram_offset=0, xsize=8))

class LoadUopTest118 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=1, dram_offset=1, xsize=8))

class LoadUopTest018 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=0, dram_offset=1, xsize=8))

class LoadUopTest107 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=1, dram_offset=0, xsize=7))

class LoadUopTest117 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=1, dram_offset=1, xsize=7))

class LoadUopTest017 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=0, dram_offset=1, xsize=7))

class LoadUopTest109 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=1, dram_offset=0, xsize=9))

class LoadUopTest119 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=1, dram_offset=1, xsize=9))

class LoadUopTest019 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=0, dram_offset=1, xsize=9))

class LoadUopTest001 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=0, dram_offset=0, xsize=1))

class LoadUopTest101 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=1, dram_offset=0, xsize=1))

class LoadUopTest111 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=1, dram_offset=1, xsize=1))

class LoadUopTest011 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=0, dram_offset=1, xsize=1))

class LoadUopTest0010 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=0, dram_offset=0, xsize=1, wait_max=0))

class LoadUopTest1010 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=1, dram_offset=0, xsize=1, wait_max=0))

class LoadUopTest1110 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=1, dram_offset=1, xsize=1, wait_max=0))

class LoadUopTest0110 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=0, dram_offset=1, xsize=1, wait_max=0))

class LoadUopTest01_32 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=0, dram_offset=1, xsize=32, wait_max=0))

class LoadUopTest01_33 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=0, dram_offset=1, xsize=33, wait_max=0))

class LoadUopTest01_31 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=0, dram_offset=1, xsize=31, wait_max=0))

class LoadUopTest00_32 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=0, dram_offset=0, xsize=32, wait_max=0))

class LoadUopTest00_33 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=0, dram_offset=0, xsize=33, wait_max=0))

class LoadUopTest00_34 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=0, dram_offset=0, xsize=34, wait_max=0))

class LoadUopTest00_36 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=0, dram_offset=0, xsize=36, wait_max=0))

class LoadUopTest00_31 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=0, dram_offset=0, xsize=31, wait_max=0))

class LoadUopTest10_32 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=1, dram_offset=0, xsize=32, wait_max=0))

class LoadUopTest10_33 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=1, dram_offset=0, xsize=33, wait_max=0))

class LoadUopTest10_31 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=1, dram_offset=0, xsize=31, wait_max=0))

class LoadUopTest11_32 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=1, dram_offset=1, xsize=32, wait_max=0))

class LoadUopTest11_33 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=1, dram_offset=1, xsize=33, wait_max=0))

class LoadUopTest11_31 extends GenericTest( "LoadUopTest", (p:Parameters) => new LoadUop()(p), (c:LoadUop) => new LoadUopTester(c, sram_offset=1, dram_offset=1, xsize=31, wait_max=0))

