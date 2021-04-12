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
import vta.verif.{TraceMgr => trace_mgr}


/** TensorAlu.
 * This unit instantiate the ALU vector unit (AluVector) and go over the
 * micro-ops (uops) which are used to read the source operands (vectors)
 * from the acc-scratchpad and then they are written back the same
 * acc-scratchpad.
 */
class TensorAluOrig(debug: Boolean = false)(implicit p: Parameters) extends TensorAluIfc {
  val sIdle :: sReadUop :: sComputeIdx :: sReadTensorA :: sReadTensorB :: sExe :: Nil =
    Enum(6)
  val state = RegInit(sIdle)
  val alu = Module(new AluVector)
  val dec = io.dec
  val uop_idx = Reg(chiselTypeOf(dec.uop_end))
  val uop_end = dec.uop_end
  val uop_dst = Reg(chiselTypeOf(io.uop.data.bits.u0)) // width can address entire acc
  val uop_src = Reg(chiselTypeOf(io.uop.data.bits.u0)) // width can address entire acc
  val cnt_o = Reg(chiselTypeOf(dec.lp_0))
  val dst_o = Reg(chiselTypeOf(io.uop.data.bits.u0))
  val src_o = Reg(chiselTypeOf(io.uop.data.bits.u0))
  val cnt_i = Reg(chiselTypeOf(dec.lp_1))
  val dst_i = Reg(chiselTypeOf(io.uop.data.bits.u0))
  val src_i = Reg(chiselTypeOf(io.uop.data.bits.u0))
  val done =
    state === sExe &
      alu.io.out.data.valid &
      (cnt_o === dec.lp_0 - 1.U) &
      (cnt_i === dec.lp_1 - 1.U) &
      (uop_idx === uop_end - 1.U)

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
      state := sReadTensorA
    }
    is(sReadTensorA) {
      state := sReadTensorB
    }
    is(sReadTensorB) {
      state := sExe
    }
    is(sExe) {
      when(alu.io.out.data.valid) {
        when(
          (cnt_o === dec.lp_0 - 1.U) &&
            (cnt_i === dec.lp_1 - 1.U) &&
            (uop_idx === uop_end - 1.U)) {
          state := sIdle
        }.otherwise {
          state := sReadUop
        }
      }
    }
  }

  when(
    state === sIdle ||
      (state === sExe &&
        alu.io.out.data.valid &&
        uop_idx === uop_end - 1.U)) {
    uop_idx := dec.uop_begin
  }.elsewhen(state === sExe && alu.io.out.data.valid) {
    uop_idx := uop_idx + 1.U
  }

  when(state === sIdle) {
    cnt_o := 0.U
    dst_o := 0.U
    src_o := 0.U
  }.elsewhen(
    state === sExe &&
      alu.io.out.data.valid &&
      uop_idx === uop_end - 1.U &&
      cnt_i === dec.lp_1 - 1.U) {
    cnt_o := cnt_o + 1.U
    dst_o := dst_o + dec.dst_0
    src_o := src_o + dec.src_0
  }

  when(state === sIdle) {
    cnt_i := 0.U
    dst_i := 0.U
    src_i := 0.U
  }.elsewhen(state === sReadUop && cnt_i === dec.lp_1) {
    cnt_i := 0.U
    dst_i := dst_o
    src_i := src_o
  }.elsewhen(state === sExe && alu.io.out.data.valid && uop_idx === uop_end - 1.U) {
    cnt_i := cnt_i + 1.U
    dst_i := dst_i + dec.dst_1
    src_i := src_i + dec.src_1
  }

  when(state === sComputeIdx && io.uop.data.valid) {
    uop_dst := io.uop.data.bits.u0 + dst_i
    uop_src := ((io.uop.data.bits.u2.asTypeOf(UInt(width = uop_dst.getWidth.W)) << log2Ceil(p(CoreKey).inpMemDepth))
      | io.uop.data.bits.u1.asTypeOf(UInt(width = uop_dst.getWidth.W))) + src_i
  }

  // uop
  io.uop.idx.valid := state === sReadUop
  io.uop.idx.bits := uop_idx

  val dataSplitFactor = p(CoreKey).blockOutFactor

  val accRdValid = state === sReadTensorA | (state === sReadTensorB & ~dec.alu_use_imm)
  val accRdIdx = Mux(state === sReadTensorA, uop_dst, uop_src)
  for (idx <- 0 until dataSplitFactor) {
    // acc_i
    io.acc.rd(idx).idx.valid := accRdValid
    io.acc.rd(idx).idx.bits := accRdIdx

    // imm
    val tensorImm = Wire(new TensorClientData(tensorType = "acc"))
    tensorImm.data.valid := state === sReadTensorB
    tensorImm.data.bits.foreach { b =>
      b.foreach { c =>
        c := Mux(dec.alu_imm(C_ALU_IMM_BITS - 1),
          Cat(-1.S((aluBits - C_ALU_IMM_BITS).W), dec.alu_imm), dec.alu_imm)
      }
    }

    // alu
    val isSHR = (dec.alu_op === ALU_OP(3))
    val neg_shift = isSHR & dec.alu_imm(C_ALU_IMM_BITS - 1)
    val fixme_alu_op = Mux(neg_shift, ALU_OP((1 << C_ALU_DEC_BITS) - 1), dec.alu_op) // all bits set is neg shift
    alu.io.opcode := fixme_alu_op
    alu.io.acc_a.data.valid := io.acc.rd(idx).data.valid & state === sReadTensorB
    alu.io.acc_a.data.bits <> io.acc.rd(idx).data.bits
    alu.io.acc_b.data.valid := Mux(dec.alu_use_imm,
      tensorImm.data.valid,
      io.acc.rd(idx).data.valid & state === sExe)
    alu.io.acc_b.data.bits <> Mux(dec.alu_use_imm,
      tensorImm.data.bits,
      io.acc.rd(idx).data.bits)

    // acc_o
    io.acc.wr(idx).valid := alu.io.acc_y.data.valid
    io.acc.wr(idx).bits.idx := uop_dst
    io.acc.wr(idx).bits.data <> alu.io.acc_y.data.bits

    // out
    io.out.wr(idx).valid := alu.io.out.data.valid
    io.out.wr(idx).bits.idx := uop_dst
    io.out.wr(idx).bits.data <> alu.io.out.data.bits
  }
  io.out.tieoffRead() // write-only
  io.done := done

  // trace
  if (p(VerifKey).trace) {
    when(io.acc.wr(0).valid) {
      trace_mgr.Event("ALU_ITR", "%x %x %x %x",
      cnt_o, cnt_i, uop_idx, io.acc.wr(0).bits.idx)
      io.acc.wr(0).bits.data.foreach { tensor =>
        tensor.foreach { elem =>
          //for (i <- 0 until elem.getWidth by 8) {
          //  trace_mgr.Event("+ALU_ITR", " %x", elem(i+7,i))
          //}
          // Only the first byte for now.
          //trace_mgr.Event("+ALU_ITR", " %x", elem(7, 0))
          trace_mgr.Event("+ALU_ITR", " %x", elem)
        }
      }
      trace_mgr.Event("+ALU_ITR", "\n")
    }

    val inst = io.dec.asUInt
    when(state === sIdle && io.start) {
      when(dec.alu_use_imm) {
        when(dec.alu_op === ALU_OP(0)) {
          trace_mgr.Event("EXE", "MINI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(1)) {
          trace_mgr.Event("EXE", "MAXI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(2)) {
          trace_mgr.Event("EXE", "ADDI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(3)) {
          trace_mgr.Event("EXE", "SHRI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(4)) {
          trace_mgr.Event("EXE", "CLPI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(5)) {
          trace_mgr.Event("EXE", "MOVI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(6)) {
          trace_mgr.Event("EXE", "MULI %x\n", inst)
        }
      }.otherwise {
        when(dec.alu_op === ALU_OP(0)) {
          trace_mgr.Event("EXE", "MIN  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(1)) {
          trace_mgr.Event("EXE", "MAX  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(2)) {
          trace_mgr.Event("EXE", "ADD  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(3)) {
          trace_mgr.Event("EXE", "SHR  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(4)) {
          trace_mgr.Event("EXE", "CLP  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(5)) {
          trace_mgr.Event("EXE", "MOV  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(6)) {
          trace_mgr.Event("EXE", "MUL  %x\n", inst)
        }
      }
      trace_mgr.Event("ALU_LOOP", "%x %x %x %x\n",
      dec.lp_0, dec.lp_1, dec.uop_begin, dec.uop_end)
    }.elsewhen(state === sExe && io.done) {
      when(dec.alu_use_imm) {
        when(dec.alu_op === ALU_OP(0)) {
          trace_mgr.Event("RET", "MINI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(1)) {
          trace_mgr.Event("RET", "MAXI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(2)) {
          trace_mgr.Event("RET", "ADDI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(3)) {
          trace_mgr.Event("RET", "SHRI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(4)) {
          trace_mgr.Event("RET", "CLPI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(5)) {
          trace_mgr.Event("RET", "MOVI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(6)) {
          trace_mgr.Event("RET", "MULI %x\n", inst)
        }
      }.otherwise {
        when(dec.alu_op === ALU_OP(0)) {
          trace_mgr.Event("RET", "MIN  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(1)) {
          trace_mgr.Event("RET", "MAX  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(2)) {
          trace_mgr.Event("RET", "ADD  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(3)) {
          trace_mgr.Event("RET", "SHR  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(4)) {
          trace_mgr.Event("RET", "CLP  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(5)) {
          trace_mgr.Event("RET", "MOV  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(6)) {
          trace_mgr.Event("RET", "MUL  %x\n", inst)
        }
      }
    }
  }

  if ( false) {
    when ( state === sIdle && io.start) {
      printf( "uop_{begin,end}: %x %x lp_{0,1}: %x %x {src,dest}_{0,1}: %x %x %x %x\n",
      dec.uop_begin, dec.uop_end,
      dec.lp_0, dec.lp_1,
      dec.src_0, dec.src_1,
      dec.dst_0, dec.dst_1)
    }
  }

  if (debug) {
    when(state === sReadUop) {
      printf("[TensorAlu] [uop] idx:%x\n", uop_idx)
    }

    when(state === sReadTensorA) {
      printf("[TensorAlu] [uop] dst:%x src:%x\n", uop_dst, uop_src)
    }

    when(state === sIdle && io.start) {
      printf(p"[TensorAlu] decode:$dec\n")
    }

    alu.io.acc_a.data.bits.foreach { tensor =>
      tensor.zipWithIndex.foreach {
        case (elem, i) =>
          when(alu.io.acc_a.data.valid) {
            printf("[TensorAlu] [a] i:%x val:%x\n", i.U, elem)
          }
      }
    }

    alu.io.acc_b.data.bits.foreach { tensor =>
      tensor.zipWithIndex.foreach {
        case (elem, i) =>
          when(alu.io.acc_b.data.valid) {
            printf("[TensorAlu] [b] i:%x val:%x\n", i.U, elem)
          }
      }
    }

    alu.io.acc_y.data.bits.foreach { tensor =>
      tensor.zipWithIndex.foreach {
        case (elem, i) =>
          when(alu.io.acc_y.data.valid) {
            printf("[TensorAlu] [y] i:%x val:%x\n", i.U, elem)
          }
      }
    }

    alu.io.out.data.bits.foreach { tensor =>
      tensor.zipWithIndex.foreach {
        case (elem, i) =>
          when(alu.io.out.data.valid) {
            printf("[TensorAlu] [out] i:%x val:%x\n", i.U, elem)
          }
      }
    }
  }
}
