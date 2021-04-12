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

/** EventCounters.
 *
 * This unit contains all the event counting logic. One common event tracked in
 * hardware is the number of clock cycles taken to achieve certain task. We
 * can count the total number of clock cycles spent in a VTA run by checking
 * launch and finish signals.
 *
 * The event counter value is passed to the VCR module via the ecnt port, so
 * they can be accessed by the host. The number of event counters (nECnt) is
 * defined in the Shell VCR module as a parameter, see VCRParams.
 *
 * If one would like to add an event counter, then the value of nECnt must be
 * changed in VCRParams together with the corresponding counting logic here.
 */
class EventCounters(implicit p: Parameters) extends Module {
  val vp = p(ShellKey).vcrParams
  val io = IO(new Bundle {
    val launch = Input(Bool())
    val finish = Input(Bool())
    val ecnt = Vec(vp.nECnt, ValidIO(UInt(vp.regBits.W)))
    val ucnt = Vec(vp.nUCnt, ValidIO(UInt(vp.regBits.W)))
    val acc_wr_event = Input(Bool())
    val acc_ld_event = Input(Bool())
    val inp_ld_event = Input(Bool())
    val wgt_ld_event = Input(Bool())
    val uop_ld_event = Input(Bool())
    val out_st_event = Input(Bool())
    val alu_lp_event = Input(Bool())
    val gem_lp_event = Input(Bool())
    val idle_ld_event = Input(Bool())
    val idle_st_event = Input(Bool())
    val idle_cp_event = Input(Bool())
    val stall_ld_event = Input(Bool())
    val stall_st_event = Input(Bool())
    val stall_cp_event = Input(Bool())
  })
  val cycle_cnt = RegInit(0.U(vp.regBits.W))
  when(io.launch && !io.finish) {
    cycle_cnt := cycle_cnt + 1.U
  }.otherwise {
    cycle_cnt := 0.U
  }
  io.ecnt(0).valid := io.finish
  io.ecnt(0).bits := cycle_cnt

  val acc_wr_cnt = Reg(UInt(vp.regBits.W))
  val acc_ld_cnt = Reg(UInt(vp.regBits.W))
  val inp_ld_cnt = Reg(UInt(vp.regBits.W))
  val wgt_ld_cnt = Reg(UInt(vp.regBits.W))
  val uop_ld_cnt = Reg(UInt(vp.regBits.W))
  val out_st_cnt = Reg(UInt(vp.regBits.W))
  val alu_lp_cnt = Reg(UInt(vp.regBits.W))
  val gem_lp_cnt = Reg(UInt(vp.regBits.W))
  val idle_ld_cnt = Reg(UInt(vp.regBits.W))
  val idle_st_cnt = Reg(UInt(vp.regBits.W))
  val idle_cp_cnt = Reg(UInt(vp.regBits.W))
  val stall_ld_cnt = Reg(UInt(vp.regBits.W))
  val stall_st_cnt = Reg(UInt(vp.regBits.W))
  val stall_cp_cnt = Reg(UInt(vp.regBits.W))

  when (!io.launch) {
    acc_wr_cnt := 0.U
    acc_ld_cnt := 0.U
    inp_ld_cnt := 0.U
    wgt_ld_cnt := 0.U
    uop_ld_cnt := 0.U
    out_st_cnt := 0.U
    alu_lp_cnt := 0.U
    gem_lp_cnt := 0.U
    idle_ld_cnt := 0.U
    idle_st_cnt := 0.U
    idle_cp_cnt := 0.U
    stall_ld_cnt := 0.U
    stall_st_cnt := 0.U
    stall_cp_cnt := 0.U
  }.otherwise {
    when (io.acc_wr_event) {
      acc_wr_cnt := acc_wr_cnt + 1.U
    }
    when (io.acc_ld_event) {
      acc_ld_cnt := acc_ld_cnt + 1.U
    }
    when (io.inp_ld_event) {
      inp_ld_cnt := inp_ld_cnt + 1.U
    }
    when (io.wgt_ld_event) {
      wgt_ld_cnt := wgt_ld_cnt + 1.U
    }
    when (io.uop_ld_event) {
      uop_ld_cnt := uop_ld_cnt + 1.U
    }
    when (io.out_st_event) {
      out_st_cnt := out_st_cnt + 1.U
    }
    when (io.alu_lp_event) {
      alu_lp_cnt := alu_lp_cnt + 1.U
    }
    when (io.gem_lp_event) {
      gem_lp_cnt := gem_lp_cnt + 1.U
    }
    when (io.idle_ld_event) {
      idle_ld_cnt := idle_ld_cnt + 1.U
    }
    when (io.idle_st_event) {
      idle_st_cnt := idle_st_cnt + 1.U
    }
    when (io.idle_cp_event) {
      idle_cp_cnt := idle_cp_cnt + 1.U
    }
    when (io.stall_ld_event) {
      stall_ld_cnt := stall_ld_cnt + 1.U
    }
    when (io.stall_st_event) {
      stall_st_cnt := stall_st_cnt + 1.U
    }
    when (io.stall_cp_event) {
      stall_cp_cnt := stall_cp_cnt + 1.U
    }
  }

  for (i <- 0 until vp.nUCnt) {
    io.ucnt(i).valid := io.finish
  }
  io.ucnt(0).bits := acc_wr_cnt
  io.ucnt(1).bits := acc_ld_cnt
  io.ucnt(2).bits := inp_ld_cnt
  io.ucnt(3).bits := wgt_ld_cnt
  io.ucnt(4).bits := uop_ld_cnt
  io.ucnt(5).bits := out_st_cnt
  io.ucnt(6).bits := alu_lp_cnt
  io.ucnt(7).bits := gem_lp_cnt
  io.ucnt(8).bits := idle_ld_cnt
  io.ucnt(9).bits := idle_st_cnt
  io.ucnt(10).bits := idle_cp_cnt
  io.ucnt(11).bits := stall_ld_cnt
  io.ucnt(12).bits := stall_st_cnt
  io.ucnt(13).bits := stall_cp_cnt

  // trace
  if (p(VerifKey).trace) {
    when(io.launch && !io.finish) {
      trace_mgr.Event("CYCLE", "%x\n", cycle_cnt)
    }
    when (io.acc_wr_event) {
      trace_mgr.Event("ACC_BWR", "%x\n", acc_wr_cnt)
    }
    when (io.acc_ld_event) {
      trace_mgr.Event("PULSE", "LACC %x\n", acc_ld_cnt)
    }
    when (io.inp_ld_event) {
      trace_mgr.Event("PULSE", "LINP %x\n", inp_ld_cnt)
    }
    when (io.wgt_ld_event) {
      trace_mgr.Event("PULSE", "LWGT %x\n", wgt_ld_cnt)
    }
    when (io.uop_ld_event) {
      trace_mgr.Event("PULSE", "LUOP %x\n", uop_ld_cnt)
    }
    when (io.out_st_event) {
      trace_mgr.Event("PULSE", "SOUT %x\n", out_st_cnt)
    }
    when (io.alu_lp_event) {
      trace_mgr.Event("LOOP", "ALU  %x\n", alu_lp_cnt)
    }
    when (io.gem_lp_event) {
      trace_mgr.Event("LOOP", "GEM  %x\n", gem_lp_cnt)
    }
    when (io.idle_ld_event) {
      trace_mgr.Event("IDLE", "LD   %x\n", idle_ld_cnt)
    }
    when (io.idle_st_event) {
      trace_mgr.Event("IDLE", "ST   %x\n", idle_st_cnt)
    }
    when (io.idle_cp_event) {
      trace_mgr.Event("IDLE", "CP   %x\n", idle_cp_cnt)
    }
    when(io.finish && io.launch) {
      // TODO: use parameter for multiplier.
      trace_mgr.Event("FINISH", "   ACC_LD_BYTES:%d\n", acc_ld_cnt * 8.U)
      trace_mgr.Event("FINISH", "   INP_LD_BYTES:%d\n", inp_ld_cnt * 8.U)
      trace_mgr.Event("FINISH", "   WGT_LD_BYTES:%d\n", wgt_ld_cnt * 8.U)
      trace_mgr.Event("FINISH", "   UOP_LD_BYTES:%d\n", uop_ld_cnt * 8.U)
      trace_mgr.Event("FINISH", "   OUT_ST_BYTES:%d\n", out_st_cnt * 8.U)
      trace_mgr.Event("FINISH", "   ALU_LP_COUNT: %d\n", alu_lp_cnt)
      trace_mgr.Event("FINISH", "   GEM_LP_COUNT: %d\n", gem_lp_cnt)
      trace_mgr.Event("FINISH", "    CORE_CYCLES: %d\n", cycle_cnt)
      trace_mgr.Event("FINISH", " IDLE_LD_CYCLES: %d\n", idle_ld_cnt)
      trace_mgr.Event("FINISH", " IDLE_ST_CYCLES: %d\n", idle_st_cnt)
      trace_mgr.Event("FINISH", " IDLE_CP_CYCLES: %d\n", idle_cp_cnt)
      trace_mgr.Event("FINISH", "STALL_LD_CYCLES: %d\n", stall_ld_cnt)
      trace_mgr.Event("FINISH", "STALL_ST_CYCLES: %d\n", stall_st_cnt)
      trace_mgr.Event("FINISH", "STALL_CP_CYCLES: %d\n", stall_cp_cnt)
    }
  }
}
