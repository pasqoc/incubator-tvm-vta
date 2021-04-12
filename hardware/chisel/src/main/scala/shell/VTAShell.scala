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

package vta.shell

import chisel3._
import vta.util.config._
import vta.interface.axi._
import vta.core._
import chisel3.util.experimental.BoringUtils


/** Shell parameters. */
case class ShellParams(
    hostParams: AXIParams,
    memParams: AXIParams,
    vcrParams: VCRParams,
    vmeParams: VMEParams
)

case object ShellKey extends Field[ShellParams]

/** VTAShell.
 *
 * The VTAShell is based on a VME, VCR and core. This creates a complete VTA
 * system that can be used for simulation or real hardware.
 */
class VTAShell(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val host = new AXILiteClient(p(ShellKey).hostParams)
    val mem = new AXIMaster(p(ShellKey).memParams)
  })

  val vcr = Module(new VCR)
  val vme = Module(new VMETop)
  val core = Module(new Core)

  core.io.vcr <> vcr.io.vcr
  vme.io.vme <> core.io.vme

  vcr.io.host <> io.host
  io.mem <> vme.io.mem

  /*
   * Bore some signals to the top so we don't need to collect deep traces
   * USE_TRACE_DEPTH=2 works for analyze_vcd
   */

  val alu_start = WireInit( init=false.B)
  val alu_done = WireInit( init=false.B)
  val gemm_start = WireInit( init=false.B)
  val gemm_done = WireInit( init=false.B)
  val loadUop_start = WireInit( init=false.B)
  val loadUop_done = WireInit( init=false.B)
  val tensorAcc_start = WireInit( init=false.B)
  val tensorAcc_done = WireInit( init=false.B)
  val inp_start = WireInit( init=false.B)
  val inp_done = WireInit( init=false.B)
  val wgt_start = WireInit( init=false.B)
  val wgt_done = WireInit( init=false.B)
  val store_start = WireInit( init=false.B)
  val store_done = WireInit( init=false.B)
  val rd_0_data_valid = WireInit( init=false.B)
  val rd_1_data_valid = WireInit( init=false.B)
  val rd_2_data_valid = WireInit( init=false.B)
  val rd_3_data_valid = WireInit( init=false.B)
  val wr_0_data_valid = WireInit( init=false.B)
  val wr_0_data_ready = WireInit( init=false.B)

  dontTouch(alu_start)
  dontTouch(alu_done)
  dontTouch(gemm_start)
  dontTouch(gemm_done)
  dontTouch(loadUop_start)
  dontTouch(loadUop_done)
  dontTouch(tensorAcc_start)
  dontTouch(tensorAcc_done)
  dontTouch(inp_start)
  dontTouch(inp_done)
  dontTouch(wgt_start)
  dontTouch(wgt_done)
  dontTouch(store_start)
  dontTouch(store_done)
  dontTouch(rd_0_data_valid)
  dontTouch(rd_1_data_valid)
  dontTouch(rd_2_data_valid)
  dontTouch(rd_3_data_valid)
  dontTouch(wr_0_data_valid)
  dontTouch(wr_0_data_ready)


  BoringUtils.bore( core.compute.tensorAlu.io.start, Seq(alu_start))
  BoringUtils.bore( core.compute.tensorAlu.io.done, Seq(alu_done))
  BoringUtils.bore( core.compute.tensorGemm.io.start, Seq(gemm_start))
  BoringUtils.bore( core.compute.tensorGemm.io.done, Seq(gemm_done))
  BoringUtils.bore( core.compute.loadUop.io.start, Seq(loadUop_start))
  BoringUtils.bore( core.compute.loadUop.io.done, Seq(loadUop_done))
  BoringUtils.bore( core.compute.tensorAcc.io.start, Seq(tensorAcc_start))
  BoringUtils.bore( core.compute.tensorAcc.io.done, Seq(tensorAcc_done))
  BoringUtils.bore( core.load.tensorLoad(0).io.start, Seq(inp_start))
  BoringUtils.bore( core.load.tensorLoad(0).io.done, Seq(inp_done))
  BoringUtils.bore( core.load.tensorLoad(1).io.start, Seq(wgt_start))
  BoringUtils.bore( core.load.tensorLoad(1).io.done, Seq(wgt_done))
  BoringUtils.bore( core.store.tensorStore.io.start, Seq(store_start))
  BoringUtils.bore( core.store.tensorStore.io.done, Seq(store_done))

  BoringUtils.bore( vme.io.vme.rd(0).data.valid, Seq(rd_0_data_valid))
  BoringUtils.bore( vme.io.vme.rd(1).data.valid, Seq(rd_1_data_valid))
  BoringUtils.bore( vme.io.vme.rd(2).data.valid, Seq(rd_2_data_valid))
  BoringUtils.bore( vme.io.vme.rd(3).data.valid, Seq(rd_3_data_valid))

  BoringUtils.bore( vme.io.vme.wr(0).data.valid, Seq(wr_0_data_valid))
  BoringUtils.bore( vme.io.vme.wr(0).data.ready, Seq(wr_0_data_ready))

}
