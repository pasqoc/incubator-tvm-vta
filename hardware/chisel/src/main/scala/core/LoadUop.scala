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

package vta.core

import chisel3._
import chisel3.util._
import vta.util.config._
import vta.shell._
import vta.verif.{TraceMgr => trace_mgr}

/** UopMaster.
 *
 * Uop interface used by a master module, i.e. TensorAlu or TensorGemm,
 * to request a micro-op (uop) from the uop-scratchpad. The index (idx) is
 * used as an address to find the uop in the uop-scratchpad.
 */
class UopMaster(implicit p: Parameters) extends Bundle {
  val addrBits = log2Ceil(p(CoreKey).uopMemDepth)
  val idx = ValidIO(UInt(addrBits.W))
  val data = Flipped(ValidIO(new UopDecode))
  override def cloneType = new UopMaster().asInstanceOf[this.type]
}

/** UopClient.
 *
 * Uop interface used by a client module, i.e. LoadUop, to receive
 * a request from a master module, i.e. TensorAlu or TensorGemm.
 * The index (idx) is used as an address to find the uop in the uop-scratchpad.
 */
class UopClient(implicit p: Parameters) extends Bundle {
  val addrBits = log2Ceil(p(CoreKey).uopMemDepth)
  val idx = Flipped(ValidIO(UInt(addrBits.W)))
  val data = ValidIO(new UopDecode)
  override def cloneType = new UopClient().asInstanceOf[this.type]
}

/** LoadUopTop.
 *
 * Top wrapper of load uop implementations.
 */
class LoadUopTop(debug: Boolean = false)(implicit val p: Parameters) extends Module {
  val mp = p(ShellKey).memParams
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())
    val inst = Input(UInt(INST_BITS.W))
    val baddr = Input(UInt(mp.addrBits.W))
    val vme_rd = new VMEReadMaster
    val uop = new UopClient
  })


  if (JSONFeatures.loadUopOld()) {
    require(mp.dataBits == 64, "-F- Original LoadUop supports only 64 bit memory data transfer")

    val loadUop = Module(new LoadUop(debug))

    loadUop.io.start := io.start
    io.done := loadUop.io.done
    loadUop.io.baddr := io.baddr
    loadUop.io.vme_rd <> io.vme_rd

    loadUop.io.dec := io.inst.asTypeOf( new MemDecode)
    loadUop.io.uop.idx <> io.uop.idx
    io.uop <> loadUop.io.uop

  } else {
    require(!JSONFeatures.tensorLoadOld(),
      "-F- New LoadUop requires new tensorLoad. Disable VTA_FEATURE_ORIGINAL_TENSOR_LOAD" +
      " or set VTA_FEATURE_ORIGINAL_LOAD_UOP")
    val loadUop = Module(new TensorLoad(tensorType = "uop"))
    loadUop.io.tensor.tieoffWrite()

    loadUop.io.start := io.start
    io.done := loadUop.io.done
    loadUop.io.baddr := io.baddr
    loadUop.io.vme_rd <> io.vme_rd

    loadUop.io.inst := io.inst
    require(loadUop.tp.splitWidth == 1 && loadUop.tp.splitLength == 1, "-F- UOP tensor split read is not expected")
    loadUop.io.tensor.rd(0).idx <> io.uop.idx
    io.uop.data.valid := loadUop.io.tensor.rd(0).data.valid
    io.uop.data.bits <> loadUop.io.tensor.rd(0).data.bits.asTypeOf(new UopDecode)

  }
}

/** LoadUop.
 *
 * Load micro-ops (uops) from memory, i.e. DRAM, and store them in the
 * uop-scratchpad. Currently, micro-ops are 32-bit wide and loaded in
 * group of 2 given the fact that the DRAM payload is 8-bytes. This module
 * should be modified later on to support different DRAM sizes efficiently.
 */
class LoadUop(debug: Boolean = false)(implicit val p: Parameters) extends Module {
  val mp = p(ShellKey).memParams
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())
    val dec = Input(new MemDecode)
    val baddr = Input(UInt(mp.addrBits.W))
    val vme_rd = new VMEReadMaster
    val uop = new UopClient
  })
  val uopsPerMemXfer = p(ShellKey).memParams.dataBits / p(CoreKey).uopBits
  require(p(ShellKey).memParams.dataBits % p(CoreKey).uopBits == 0)
  //require(uopsPerMemXfer == 1 || uopsPerMemXfer == 2)

  val uopBits = p(CoreKey).uopBits
  val uopBytes = uopBits / 8
  val uopDepth = p(CoreKey).uopMemDepth / uopsPerMemXfer
  val dataBytes = mp.dataBits / 8

  val dec = io.dec
  val raddr = Reg(chiselTypeOf(io.vme_rd.cmd.bits.addr))
  val xcnt = Reg(chiselTypeOf(io.vme_rd.cmd.bits.len))
  val xlen = Reg(chiselTypeOf(io.vme_rd.cmd.bits.len))
  val xrem = Reg(chiselTypeOf(dec.xsize))
  val xmax = (1 << mp.lenBits).U
  val xmax_bytes = ((1 << mp.lenBits) * dataBytes).U
  // Align DRAM address to data and do not cross page boundary.
  val data_align_bits = WireInit(UInt(raddr.getWidth.W), dataBytes.U - 1.U)
  val beat_bytes_bits = log2Ceil(mp.dataBits >> 3)
  val xfer_bytes = Reg(chiselTypeOf(xmax_bytes))
  // DRAM address width must be the same as AXI araddr since anything above will
  // be silently truncated, enforce this here.
  val dram_byte_addr = WireInit(UInt(raddr.getWidth.W), dec.dram_offset << log2Ceil(uopBytes))
  // Here we are assuming io.baddr | dram_byte_addr === io.baddr + dram_byte_addr.
  val unaligned_addr = io.baddr | dram_byte_addr
  val xfer_init_addr = unaligned_addr & ~data_align_bits
  val xfer_next_addr = raddr + xfer_bytes
  val xfer_init_bytes = xmax_bytes - xfer_init_addr % xmax_bytes
  val xfer_init_beats = xfer_init_bytes >> beat_bytes_bits
  val xfer_next_bytes = xmax_bytes - xfer_next_addr % xmax_bytes
  val xfer_next_beats = xfer_next_bytes >> beat_bytes_bits

  val dram_even = (dec.dram_offset % 2.U) === 0.U
  val sram_even = (dec.sram_offset % 2.U) === 0.U
  val sizeIsEven = (dec.xsize % 2.U) === 0.U

  val sIdle :: sReadCmd :: sReadData :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val first = RegInit(init=false.B)

  // control
  switch(state) {
    is(sIdle) {
      xfer_bytes := xfer_init_bytes
      when(io.start) {
        state := sReadCmd
        first := true.B
        raddr := xfer_init_addr
        // Number of total beats in the load transfer.
        val xsize = if (uopsPerMemXfer == 1) {
          dec.xsize
        } else {
          ((dec.xsize +& 1.U + dec.dram_offset(0)) >> 1)
        }

        when(xsize <= xfer_init_beats) {
          xlen := xsize - 1.U
          xrem := 0.U
        }.otherwise {
          xlen := xfer_init_beats - 1.U
          xrem := xsize - xfer_init_beats
        }
      }
    }
    is(sReadCmd) {
      when(io.vme_rd.cmd.ready) {
        state := sReadData
      }
    }
    is(sReadData) {
      when(io.vme_rd.data.valid) {
        when(xcnt === xlen) {
          when(xrem === 0.U) {
            state := sIdle
          }.otherwise {
            state := sReadCmd
            raddr := xfer_next_addr
            xfer_bytes := xfer_next_bytes
            when(xrem <= xfer_next_beats) {
              xlen := xrem - 1.U
              xrem := 0.U
            }.otherwise {
              xlen := xfer_next_beats - 1.U
              xrem := xrem - xfer_next_beats
            }
          }
        }
      }
    }
  }

  // read-from-dram
  io.vme_rd.cmd.valid := state === sReadCmd
  io.vme_rd.cmd.bits.addr := raddr
  io.vme_rd.cmd.bits.len := xlen
  io.vme_rd.cmd.bits.tag := dec.sram_offset

  io.vme_rd.data.ready := state === sReadData

  when(state =/= sReadData) {
    xcnt := 0.U
  }.elsewhen(io.vme_rd.data.fire()) {
    xcnt := xcnt + 1.U
  }

  val waddr = IndexedSeq.fill( uopsPerMemXfer) { Reg(UInt(log2Ceil(uopDepth).W))}
  when(state === sIdle) {
    val so = dec.sram_offset >> log2Ceil(uopsPerMemXfer)
    if (uopsPerMemXfer == 1) {
      waddr(0) := so
    } else {
      when ( !sram_even &&  dram_even) { // 10
        waddr(0) := so + 1.U
        waddr(1) := so
      }.elsewhen (  sram_even && !dram_even) { // 01
        waddr(0) := so
        waddr(1) := so - 1.U
      }.otherwise {
        waddr(0) := so
        waddr(1) := so
      }
    }
  }.elsewhen(io.vme_rd.data.fire()) {
    for (i <- 0 until uopsPerMemXfer) {
      waddr(i) := waddr(i) + 1.U
    }
  }

  val mems = IndexedSeq.fill( uopsPerMemXfer) { SyncReadMem( uopDepth, UInt(uopBits.W))}
  val last = (xcnt === xlen) && (xrem === 0.U)

  val wmask = Wire(Vec(uopsPerMemXfer, Bool()))
  for (i <- 0 until uopsPerMemXfer) {
    wmask(i) := true.B
  }

  when ( io.vme_rd.data.fire()) {
    when ( first) {
      first := false.B

      if (uopsPerMemXfer == 2) {
        when( !sram_even && !dram_even) {
          wmask(0) := false.B
        }
      }
    }
    when( last) {
      if (uopsPerMemXfer == 2) {
        when( dram_even ^ sizeIsEven) {
          when ( sram_even ^ sizeIsEven) {
            wmask(1) := false.B
          }.otherwise{
            wmask(0) := false.B
          }
        }
      }
    }
  }

  val wdata = Wire( Vec(uopsPerMemXfer, UInt(uopBits.W)))
  wdata := io.vme_rd.data.bits.data.asTypeOf(wdata)
  if (uopsPerMemXfer == 2) {
    when( dram_even =/= sram_even) { // swap
      wdata(0) := io.vme_rd.data.bits.data.asTypeOf(wdata)(1)
      wdata(1) := io.vme_rd.data.bits.data.asTypeOf(wdata)(0)
    }
  }

  when(io.vme_rd.data.fire()) {
    for { i <- 0 until mems.size} {
      when ( wmask(i)) {
        mems(i).write(waddr(i), wdata(i))
      }
    }
  }

  io.done := io.vme_rd.data.fire() & last

  // ----------- read-from-sram -------------

  io.uop.data.valid := RegNext(io.uop.idx.valid)

  // delay LSB of idx by a cycle because of the one-cycle memory read latency
  val rIdx = io.uop.idx.bits >> log2Ceil(uopsPerMemXfer)
  val m0 = mems(0).read(rIdx, io.uop.idx.valid)

  if (uopsPerMemXfer == 2) {
    val m1 = mems(1).read(rIdx, io.uop.idx.valid)
    val sIdx = RegNext(io.uop.idx.bits % uopsPerMemXfer.U)
    io.uop.data.bits <> Mux( sIdx =/= 0.U, m1, m0).asTypeOf(io.uop.data.bits)
  } else {
    io.uop.data.bits <> m0.asTypeOf(io.uop.data.bits)
  }

  if ( false) {
    // Report initial part of the uop state after
    //   the clock transition where io.done is high
    val memDumpGuard = RegNext(io.done,init=false.B)
    when ( memDumpGuard) {
      for {
        idx <- 0 until scala.math.min(8,uopDepth)
        i <- 0 until uopsPerMemXfer} {
        val s = mems(i)(idx).asTypeOf(io.uop.data.bits)
        printf( s"uop: $idx $i u0: %x u1: %x u2: %x\n", s.u0, s.u1, s.u2)
      }
    }
  }

  // trace
  if (p(VerifKey).trace) {
    when(io.vme_rd.data.fire()) {
      trace_mgr.Event("LD_SRAM", "%x", waddr(0))
      for {i <- 0 until mems.size} {
        when (wmask(i)) {
          trace_mgr.Event("+LD_SRAM", " %x", wdata(i))
        }
      }
      trace_mgr.Event("+LD_SRAM", "\n")
    }
  }

  // debug
  if (debug) {
    when(io.vme_rd.cmd.fire()) {
      printf("[LoadUop] cmd addr:%x len:%x rem:%x\n", raddr, xlen, xrem)
    }
  }
}

class LoadUopOrig(debug: Boolean = false)(implicit p: Parameters) extends Module {
  val mp = p(ShellKey).memParams
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())
    val dec = Input(new MemDecode)
    val baddr = Input(UInt(mp.addrBits.W))
    val vme_rd = new VMEReadMaster
    val uop = new UopClient
  })
  val numUop = 2 // store two uops per sram word
  val uopBits = p(CoreKey).uopBits
  val uopBytes = uopBits / 8
  val uopDepth = p(CoreKey).uopMemDepth / numUop

  val dec = io.dec
  val raddr = Reg(chiselTypeOf(io.vme_rd.cmd.bits.addr))
  val xcnt = Reg(chiselTypeOf(io.vme_rd.cmd.bits.len))
  val xlen = Reg(chiselTypeOf(io.vme_rd.cmd.bits.len))
  val xrem = Reg(chiselTypeOf(dec.xsize))
  val xsize = (dec.xsize >> log2Ceil(numUop)) + dec.xsize(0) + (dec.sram_offset % 2.U) - 1.U
  val xmax = (1 << mp.lenBits).U
  val xmax_bytes = ((1 << mp.lenBits) * mp.dataBits / 8).U

  val dram_even = (dec.dram_offset % 2.U) === 0.U
  val sram_even = (dec.sram_offset % 2.U) === 0.U
  val sizeIsEven = (dec.xsize % 2.U) === 0.U

  val sIdle :: sReadCmd :: sReadData :: Nil = Enum(3)
  val state = RegInit(sIdle)

  // control
  switch(state) {
    is(sIdle) {
      when(io.start) {
        state := sReadCmd
        when(xsize < xmax) {
          xlen := xsize
          xrem := 0.U
        }.otherwise {
          xlen := xmax - 1.U
          xrem := xsize - xmax
        }
      }
    }
    is(sReadCmd) {
      when(io.vme_rd.cmd.ready) {
        state := sReadData
      }
    }
    is(sReadData) {
      when(io.vme_rd.data.valid) {
        when(xcnt === xlen) {
          when(xrem === 0.U) {
            state := sIdle
          }.otherwise {
            raddr := raddr + xmax_bytes
            when(xrem < xmax) {
              state := sReadCmd
              xlen := xrem
              xrem := 0.U
            }
            .otherwise {
              state := sReadCmd
              xlen := xmax - 1.U
              xrem := xrem - xmax
            }
          }
        }
      }
    }
  }

  // read-from-dram
  val maskOffset = VecInit(Seq.fill(M_DRAM_OFFSET_BITS)(true.B)).asUInt
  when(state === sIdle) {
    when(dram_even) {
      raddr := io.baddr | (maskOffset & (dec.dram_offset << log2Ceil(uopBytes)))
    }.otherwise {
      raddr := (io.baddr | (maskOffset & (dec.dram_offset << log2Ceil(uopBytes)))) - uopBytes.U
    }
  }

  io.vme_rd.cmd.valid := state === sReadCmd
  io.vme_rd.cmd.bits.addr := raddr
  io.vme_rd.cmd.bits.len := xlen

  io.vme_rd.data.ready := state === sReadData

  when(state =/= sReadData) {
    xcnt := 0.U
  }.elsewhen(io.vme_rd.data.fire()) {
    xcnt := xcnt + 1.U
  }

  val waddr = Reg(UInt(log2Ceil(uopDepth).W))
  when(state === sIdle) {
    waddr := dec.sram_offset >> log2Ceil(numUop)
  }.elsewhen(io.vme_rd.data.fire()) {
    waddr := waddr + 1.U
  }

  val wdata = Wire(Vec(numUop, UInt(uopBits.W)))
  val mem = SyncReadMem(uopDepth, chiselTypeOf(wdata))
  val wmask = Reg(Vec(numUop, Bool()))

  when(sram_even) {
    when(sizeIsEven) {
      wmask := "b_11".U.asTypeOf(wmask)
    }.elsewhen(io.vme_rd.cmd.fire()) {
      when(dec.xsize === 1.U) {
        wmask := "b_01".U.asTypeOf(wmask)
      }.otherwise {
        wmask := "b_11".U.asTypeOf(wmask)
      }
    }.elsewhen(io.vme_rd.data.fire()) {
      when((xcnt === xlen - 1.U) && (xrem === 0.U)) {
        wmask := "b_01".U.asTypeOf(wmask)
      }.otherwise {
        wmask := "b_11".U.asTypeOf(wmask)
      }
    }
  }.otherwise {
    when(io.vme_rd.cmd.fire()) {
      wmask := "b_10".U.asTypeOf(wmask)
    }.elsewhen(io.vme_rd.data.fire()) {
      when(sizeIsEven && (xcnt === xlen - 1.U) && (xrem === 0.U)) {
        wmask := "b_01".U.asTypeOf(wmask)
      }.otherwise {
        wmask := "b_11".U.asTypeOf(wmask)
      }
    }
  }

  wdata := io.vme_rd.data.bits.data.asTypeOf(wdata)
  when(dram_even === false.B && sram_even) {
    wdata(0) := io.vme_rd.data.bits.data.asTypeOf(wdata)(1)
  }.elsewhen(sram_even === false.B && dram_even) {
    wdata(1) := io.vme_rd.data.bits.data.asTypeOf(wdata)(0)
  }

  when(io.vme_rd.data.fire()) {
    mem.write(waddr, wdata, wmask)
  }

  // read-from-sram
  io.uop.data.valid := RegNext(io.uop.idx.valid)

  // delay LSB of idx by a cycle because of the one-cycle memory read latency
  val sIdx = RegNext(io.uop.idx.bits % numUop.U)
  val rIdx = io.uop.idx.bits >> log2Ceil(numUop)
  val memRead = mem.read(rIdx, io.uop.idx.valid)
  val sWord = memRead.asUInt.asTypeOf(wdata)
  val sUop = sWord(sIdx).asTypeOf(io.uop.data.bits)

  io.uop.data.bits <> sUop

  // done
  io.done := state === sReadData & io.vme_rd.data.valid & xcnt === xlen & xrem === 0.U

  if ( false) {
    // Report initial part of the uop state after
    //   the clock transition where io.done is high
    val memDumpGuard = RegNext(io.done,init=false.B)
    when ( memDumpGuard) {
      for {
        idx <- 0 until scala.math.min(8,uopDepth)
        i <- 0 until numUop} {
        val s = mem(idx)(i).asTypeOf(io.uop.data.bits)
        printf( s"uop: $idx $i u0: %x u1: %x u2: %x\n", s.u0, s.u1, s.u2)
      }
    }
  }

  // debug
  if (debug) {
    when(io.vme_rd.cmd.fire()) {
      printf("[LoadUop] cmd addr:%x len:%x rem:%x\n", raddr, xlen, xrem)
    }
  }
}
