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
import vta.util._
import vta.shell._
import vta.verif.{TraceMgr => trace_mgr}

/** Load.
 *
 * Load inputs and weights from memory (DRAM) into scratchpads (SRAMs).
 * This module instantiate the TensorLoad unit which is in charge of
 * loading 1D and 2D tensors to scratchpads, so it can be used by
 * other modules such as Compute.
 */
class Load(debug: Boolean = false)(implicit p: Parameters) extends Module {
  val mp = p(ShellKey).memParams
  val io = IO(new Bundle {
    val i_post = Input(Bool())
    val o_post = Output(Bool())
    val inst = Flipped(Decoupled(UInt(INST_BITS.W)))
    val inp_baddr = Input(UInt(mp.addrBits.W))
    val wgt_baddr = Input(UInt(mp.addrBits.W))
    val vme_rd = Vec(2, new VMEReadMaster)
    val inp = new TensorClient(tensorType = "inp")
    val wgt = new TensorClient(tensorType = "wgt")
    val inp_ld_event = Output(Bool())
    val wgt_ld_event = Output(Bool())
    val inp_stall_event = Output(Bool())
    val wgt_stall_event = Output(Bool())
    val idle_ld_event = Output(Bool())
  })
  val sIdle :: sSync :: sExe :: Nil = Enum(3)
  val state = RegInit(sIdle)

  val s = Module(new Semaphore(counterBits = 8, counterInitValue = 0))
  val inst_q = Module(new SyncQueueVTA(UInt(INST_BITS.W), p(CoreKey).instQueueEntries))

  val dec = Module(new LoadDecode)
  dec.io.inst := inst_q.io.deq.bits

  val tensorType = Seq("inp", "wgt")
  val tensorDec = Seq(dec.io.isInput, dec.io.isWeight)
  val tensorLoad =
    Seq.tabulate(2)(i => Module(new TensorLoad(tensorType = tensorType(i))))

  val start = inst_q.io.deq.valid & Mux(dec.io.pop_next, s.io.sready, true.B)
  val done = Mux(dec.io.isInput, tensorLoad(0).io.done, tensorLoad(1).io.done)

  // control
  switch(state) {
    is(sIdle) {
      when(start) {
        when(dec.io.isSync) {
          state := sSync
        }.elsewhen(dec.io.isInput || dec.io.isWeight) {
          state := sExe
        }
      }
    }
    is(sSync) {
      state := sIdle
    }
    is(sExe) {
      when(done) {
        state := sIdle
      }
    }
  }

  // instructions
  inst_q.io.enq <> io.inst
  inst_q.io.deq.ready := (state === sExe & done) | (state === sSync)

  // load tensor
  // [0] input (inp)
  // [1] weight (wgt)
  val ptr = Seq(io.inp_baddr, io.wgt_baddr)
  val tsor = Seq(io.inp, io.wgt)
  for (i <- 0 until 2) {
    tensorLoad(i).io.start := state === sIdle & start & tensorDec(i)
    tensorLoad(i).io.inst := inst_q.io.deq.bits
    tensorLoad(i).io.baddr := ptr(i)
    tensorLoad(i).io.tensor <> tsor(i)
    io.vme_rd(i) <> tensorLoad(i).io.vme_rd
  }

  // events
  io.inp_ld_event := io.vme_rd(0).data.fire()
  io.wgt_ld_event := io.vme_rd(1).data.fire()
  io.inp_stall_event := io.vme_rd(0).data.ready && !io.vme_rd(0).data.valid ||
    io.vme_rd(0).cmd.valid  && !io.vme_rd(0).cmd.ready
  io.wgt_stall_event := io.vme_rd(1).data.ready && !io.vme_rd(1).data.valid ||
    io.vme_rd(1).cmd.valid  && !io.vme_rd(1).cmd.ready
  io.idle_ld_event := state === sIdle

  // semaphore
  s.io.spost := io.i_post
  s.io.swait := dec.io.pop_next & (state === sIdle & start)
  io.o_post := dec.io.push_next & ((state === sExe & done) | (state === sSync))

  // trace
  if (p(VerifKey).trace) {
    when(state === sIdle && start) {
      when(dec.io.isSync) {
        trace_mgr.Event("EXE", "LNOP %x\n", dec.io.inst)
      }.elsewhen(dec.io.isInput) {
        trace_mgr.Event("EXE", "LINP %x\n", dec.io.inst)
      }.elsewhen(dec.io.isWeight) {
        trace_mgr.Event("EXE", "LWGT %x\n", dec.io.inst)
      }
    }.elsewhen(state === sExe && done) {
      when(dec.io.isInput) {
        trace_mgr.Event("RET", "LINP %x\n", dec.io.inst)
      }.elsewhen(dec.io.isWeight) {
        trace_mgr.Event("RET", "LWGT %x\n", dec.io.inst)
      }
    }.elsewhen(state === sSync) {
      trace_mgr.Event("RET", "LNOP %x\n", dec.io.inst)
    }
    when(state === sExe) {
      when(dec.io.isInput) {
        when(io.inp_stall_event) {
          trace_mgr.Event("STALL", "LINP %x\n", dec.io.inst)
        }
      }.elsewhen(dec.io.isWeight) {
        when(io.wgt_stall_event) {
          trace_mgr.Event("STALL", "LWGT %x\n", dec.io.inst)
        }
      }
    }
  }

  // debug
  if (debug) {
    // start
    when(state === sIdle && start) {
      when(dec.io.isSync) {
        printf("[Load] start sync\n")
      }.elsewhen(dec.io.isInput) {
        printf("[Load] start input\n")
      }.elsewhen(dec.io.isWeight) {
        printf("[Load] start weight\n")
      }
    }
    // done
    when(state === sSync) {
      printf("[Load] done sync\n")
    }
    when(state === sExe) {
      when(done) {
        when(dec.io.isInput) {
          printf("[Load] done input\n")
        }.elsewhen(dec.io.isWeight) {
          printf("[Load] done weight\n")
        }
      }
    }
  }
}
