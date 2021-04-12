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
import scala.math.pow
import chisel3.util.experimental.BoringUtils
import vta.verif.{TraceMgr => trace_mgr}


/** TensorGemm.
 *
 * This unit instantiate the MatrixVectorMultiplication and go over the
 * micro-ops (uops) which are used to read inputs, weights and biases,
 * and writes results back to the acc and out scratchpads.
 *
 * Also, the TensorGemm uses the reset field in the Gemm instruction to
 * clear or zero-out the acc-scratchpad locations based on the micro-ops.
 */
class TensorGemmOrig(debug: Boolean = false)(implicit p: Parameters) extends TensorGemmIfc {

  require(p(CoreKey).blockOutFactor == 1,
    "-F- Split GEMM not supported. Use TensorGemmPipelinedSplit or set blockOutFactor to 1")
  val sIdle :: sReadUop :: sComputeIdx :: sReadTensor :: sExe :: sWait :: Nil = Enum(6)
  val state = RegInit(sIdle)
  io.state := state
  val mvc = Module(new MatrixVectorMultiplication)
  val dec = io.dec
  val uop_idx = Reg(chiselTypeOf(dec.uop_end))
  val uop_end = dec.uop_end
  val uop_acc = Reg(chiselTypeOf(dec.uop_end))
  val uop_inp = Reg(chiselTypeOf(dec.uop_end))
  val uop_wgt = Reg(chiselTypeOf(dec.uop_end))
  val cnt_o = Reg(chiselTypeOf(dec.lp_0))
  val acc_o = Reg(chiselTypeOf(dec.uop_end))
  val inp_o = Reg(chiselTypeOf(dec.uop_end))
  val wgt_o = Reg(chiselTypeOf(dec.uop_end))
  val cnt_i = Reg(chiselTypeOf(dec.lp_1))
  val acc_i = Reg(chiselTypeOf(dec.uop_end))
  val inp_i = Reg(chiselTypeOf(dec.uop_end))
  val wgt_i = Reg(chiselTypeOf(dec.uop_end))

  val inflight = Reg(UInt(inflightBits.W))
  io.inflight := inflight
  // Latency is defined as two in the following, because there is one cycle in the MAC module,
  // and another cycle in the pipelined adders as the first layer of the accumulator
  val wrpipe = Module(new Pipe(chiselTypeOf(dec.uop_end), latency = 2))
  val cond = cnt_o === dec.lp_0 - 1.U &
    cnt_i === dec.lp_1 - 1.U &
    uop_idx === uop_end - 1.U

  val done = inflight === 0.U &
    ((state === sExe) & cond | state === sWait)

  switch(state) {
    is(sIdle) {
      when(io.start) {
        state := sReadUop
      }
    }
    is(sReadUop) {
      state := sComputeIdx
    }
    is(sComputeIdx) {
      state := sReadTensor
    }
    is(sReadTensor) {
      state := sExe
    }
    is(sExe) {
      when(cond) {
        when(inflight =/= 0.U) {
          state := sWait
        }.otherwise {
          state := sIdle
        }
      }.otherwise {
        state := sReadUop
      }
    }
    is(sWait) {
      when(inflight === 0.U) {
        state := sIdle
      }
    }
  }

  when(state === sIdle) {
    inflight := 0.U
  }.elsewhen(!dec.reset) {
    when((state === sReadTensor) && mvc.io.acc_o.data.valid) { // issue & commit
    }.elsewhen(state === sReadTensor) { // issue a tensor
      assert( inflight =/= ((1<<inflightBits)-1).U)
      inflight := inflight + 1.U
    }.elsewhen(mvc.io.acc_o.data.valid) { // commit a tensor
      assert( inflight =/= 0.U)
      inflight := inflight - 1.U
    }
  }

  when(
    state === sIdle ||
      (state === sExe &&
        uop_idx === uop_end - 1.U)) {
    uop_idx := dec.uop_begin
  }.elsewhen(state === sExe && dec.uop_begin =/= uop_end) {
    uop_idx := uop_idx + 1.U
  }

  when(state === sIdle) {
    cnt_o := 0.U
    acc_o := 0.U
    inp_o := 0.U
    wgt_o := 0.U
  }.elsewhen(
    state === sExe &&
      uop_idx === uop_end - 1.U &&
      cnt_i === dec.lp_1 - 1.U) {
    cnt_o := cnt_o + 1.U
    acc_o := acc_o + dec.acc_0
    inp_o := inp_o + dec.inp_0
    wgt_o := wgt_o + dec.wgt_0
  }

  when(state === sIdle) {
    cnt_i := 0.U
    acc_i := 0.U
    inp_i := 0.U
    wgt_i := 0.U
  }.elsewhen(state === sReadUop && cnt_i === dec.lp_1) {
    cnt_i := 0.U
    acc_i := acc_o
    inp_i := inp_o
    wgt_i := wgt_o
  }.elsewhen(state === sExe && uop_idx === uop_end - 1.U) {
    cnt_i := cnt_i + 1.U
    acc_i := acc_i + dec.acc_1
    inp_i := inp_i + dec.inp_1
    wgt_i := wgt_i + dec.wgt_1
  }

  when(state === sComputeIdx && io.uop.data.valid) {
    uop_acc := io.uop.data.bits.u0 + acc_i
    uop_inp := io.uop.data.bits.u1 + inp_i
    uop_wgt := io.uop.data.bits.u2 + wgt_i
  }

  wrpipe.io.enq.valid := state === sExe & ~dec.reset
  wrpipe.io.enq.bits := uop_acc

  // uop
  io.uop.idx.valid := state === sReadUop
  io.uop.idx.bits := uop_idx

  // inp
  io.inp.rd(0).idx.valid := state === sReadTensor
  io.inp.rd(0).idx.bits := uop_inp
  io.inp.tieoffWrite() // read-only

  // wgt
  io.wgt.rd(0).idx.valid := state === sReadTensor
  io.wgt.rd(0).idx.bits := uop_wgt
  io.wgt.tieoffWrite() // read-only

  // acc_i
  io.acc.rd(0).idx.valid := state === sReadTensor
  io.acc.rd(0).idx.bits := uop_acc

  // mvc
  mvc.io.reset := dec.reset & state === sExe
  mvc.io.inp.data <> io.inp.rd(0).data
  mvc.io.wgt.data <> io.wgt.rd(0).data
  mvc.io.acc_i.data <> io.acc.rd(0).data

  // acc_o
  io.acc.wr(0).valid := mvc.io.acc_o.data.valid &
    Mux(dec.reset, true.B, wrpipe.io.deq.valid)
  io.acc.wr(0).bits.idx := Mux(dec.reset, uop_acc, wrpipe.io.deq.bits)
  io.acc.wr(0).bits.data <> mvc.io.acc_o.data.bits

  // out
  io.out.wr(0).valid := mvc.io.out.data.valid & wrpipe.io.deq.valid
  io.out.wr(0).bits.idx := wrpipe.io.deq.bits
  io.out.wr(0).bits.data <> mvc.io.out.data.bits
  io.out.tieoffRead() // write-only

  io.done := done

  if ( debug) {
    printf( "[TensorGemm] [state]:%d [inflight]:%d\n", state, inflight)

    when(state === sReadUop && ~dec.reset) {
      printf("[TensorGemm] [uop] idx:%x\n", uop_idx)
    }

    when(state === sReadTensor && ~dec.reset) {
      printf("[TensorGemm] [uop] acc:%x inp:%x wgt:%x\n", uop_acc, uop_inp, uop_wgt)
    }

    io.inp.rd(0).data.bits.zipWithIndex.foreach {
      case (r, i) =>
        when(io.inp.rd(0).data.valid && ~dec.reset) {
          printf("[TensorGemm] [inp] i:%x val:%x\n", i.U, r.asUInt)
        }
    }

    io.wgt.rd(0).data.bits.zipWithIndex.foreach {
      case (r, i) =>
        when(io.wgt.rd(0).data.valid && ~dec.reset) {
          printf("[TensorGemm] [wgt] i:%x val:%x\n", i.U, r.asUInt)
        }
    }

    io.acc.rd(0).data.bits.foreach { tensor =>
      tensor.zipWithIndex.foreach {
        case (elem, i) =>
          when(io.acc.rd(0).data.valid && ~dec.reset) {
            printf("[TensorGemm] [acc_i] i:%x val:%x\n", i.U, elem)
          }
      }
    }

    mvc.io.acc_o.data.bits.foreach { tensor =>
      tensor.zipWithIndex.foreach {
        case (elem, i) =>
          when(mvc.io.acc_o.data.valid && ~dec.reset) {
            printf("[TensorGemm] [acc_o] i:%x val:%x\n", i.U, elem)
          }
      }
    }

    mvc.io.out.data.bits.foreach { tensor =>
      tensor.zipWithIndex.foreach {
        case (elem, i) =>
          when(mvc.io.out.data.valid && ~dec.reset) {
            printf("[TensorGemm] [out] i:%x val:%x\n", i.U, elem)
          }
      }
    }
  }
}
