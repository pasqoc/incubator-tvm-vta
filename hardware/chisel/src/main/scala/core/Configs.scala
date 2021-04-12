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

import vta.util.config._

/** CoreConfig.
 *
 * This is one supported configuration for VTA. This file will
 * be eventually filled out with class configurations that can be
 * mixed/matched with Shell configurations for different backends.
 */
class CoreConfig extends Config((site, here, up) => {
  case CoreKey =>
    CoreParams(
      batch = 1,
      blockOut = 16,
      // factor is used to split, pipe INP/INSTR data
      // to physically distribute MVM blocks and respective WGT/ACC memories
      blockOutFactor = 1,
      blockIn = 16,
      inpBits = 8,
      wgtBits = 8,
      uopBits = 32,
      accBits = 32,
      outBits = 8,
      uopMemDepth = 2048,
      inpMemDepth = 2048,
      wgtMemDepth = 1024,
      accMemDepth = 2048,
      outMemDepth = 2048,
      inpFactorBits = 11,
      wgtFactorBits = 10,
      accFactorBits =  11,
      instQueueEntries = 512
    )
})

class JSONCoreConfig extends Config((site, here, up) => {
  case CoreKey =>
    val inpMemBits = JSONConfig.m("LOG_INP_BUFF_SIZE").asInstanceOf[Int] -
      JSONConfig.m("LOG_BATCH").asInstanceOf[Int] - JSONConfig.m("LOG_BLOCK_IN").asInstanceOf[Int] -
      JSONConfig.m("LOG_INP_WIDTH").asInstanceOf[Int] + 3
    val accMemBits = JSONConfig.m("LOG_ACC_BUFF_SIZE").asInstanceOf[Int] -
      JSONConfig.m("LOG_BATCH").asInstanceOf[Int] - JSONConfig.m("LOG_BLOCK_OUT").asInstanceOf[Int] -
      JSONConfig.m("LOG_ACC_WIDTH").asInstanceOf[Int] + 3
    val wgtMemBits = JSONConfig.m("LOG_WGT_BUFF_SIZE").asInstanceOf[Int] -
      JSONConfig.m("LOG_BLOCK_IN").asInstanceOf[Int] - JSONConfig.m("LOG_BLOCK_OUT").asInstanceOf[Int] -
      JSONConfig.m("LOG_WGT_WIDTH").asInstanceOf[Int] + 3
    val accMemDepth = scala.math.pow(2, accMemBits).toInt
    val uopBits = scala.math.pow(2, JSONConfig.m("LOG_UOP_WIDTH").asInstanceOf[Int]).toInt
    require(inpMemBits > JSONConfig.m("LOG_INP_FACTOR_RESTRICT").asInstanceOf[Int],
      "Inp factor bits must be greater than 0")
    require(wgtMemBits > JSONConfig.m("LOG_WGT_FACTOR_RESTRICT").asInstanceOf[Int],
      "Wgt factor bits must be greater than 0")
    require(accMemBits > JSONConfig.m("LOG_ACC_FACTOR_RESTRICT").asInstanceOf[Int],
      "Acc factor bits must be greater than 0")

    CoreParams(
      batch = scala.math.pow(2, JSONConfig.m("LOG_BATCH").asInstanceOf[Int]).toInt,
      blockOut = scala.math.pow(2, JSONConfig.m("LOG_BLOCK_OUT").asInstanceOf[Int]).toInt,
      blockOutFactor = JSONCoreOptions.blockOutSplitFactor(),
      blockIn = scala.math.pow(2, JSONConfig.m("LOG_BLOCK_IN").asInstanceOf[Int]).toInt,
      inpBits = scala.math.pow(2, JSONConfig.m("LOG_INP_WIDTH").asInstanceOf[Int]).toInt,
      wgtBits = scala.math.pow(2, JSONConfig.m("LOG_WGT_WIDTH").asInstanceOf[Int]).toInt,
      accBits = scala.math.pow(2, JSONConfig.m("LOG_ACC_WIDTH").asInstanceOf[Int]).toInt,
      outBits = scala.math.pow(2, JSONConfig.m("LOG_INP_WIDTH").asInstanceOf[Int]).toInt,
      uopBits = uopBits,
      uopMemDepth = scala.math.pow(2, JSONConfig.m("LOG_UOP_BUFF_SIZE").asInstanceOf[Int]).toInt / (uopBits/8),
      inpMemDepth = scala.math.pow(2, inpMemBits).toInt,
      wgtMemDepth = scala.math.pow(2, wgtMemBits).toInt,
      accMemDepth = accMemDepth,
      outMemDepth = accMemDepth,
      inpFactorBits = inpMemBits - JSONConfig.m("LOG_INP_FACTOR_RESTRICT").asInstanceOf[Int],
      wgtFactorBits = wgtMemBits - JSONConfig.m("LOG_WGT_FACTOR_RESTRICT").asInstanceOf[Int],
      accFactorBits = accMemBits - JSONConfig.m("LOG_ACC_FACTOR_RESTRICT").asInstanceOf[Int],
      instQueueEntries = 512
    )

  case VerifKey =>
    val chisel_trace = System.getenv("CHISEL_TRACE")
    VerifParams(
      trace = if (chisel_trace == "1") true else false
    )
})

object JSONCoreOptions {
  val defaults = new CoreConfig
  def blockOutSplitFactor() = {
    val default_result = defaults(CoreKey).blockOutFactor
    JSONConfig.m.get( "VTA_CORE_BLOCK_OUT_SPLIT_FACTOR") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Int]
        result
    }
  }
  def inpSpSplitFactor() = {
    val default_result = 1
    JSONConfig.m.get( "VTA_CORE_INP_SP_SPLIT_FACTOR") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Int]
        result
    }
  }
  def pipeDelayWrWgt() = {
    val default_result = 0
    JSONConfig.m.get( "VTA_CORE_WRITE_PIPE_WGT") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Int]
        result
    }
  }
  def pipeDelayWrAcc() = {
    val default_result = 0
    JSONConfig.m.get( "VTA_CORE_WRITE_PIPE_ACC") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Int]
        result
    }
  }
  def pipeDelayWrInp() = {
    val default_result = 0
    JSONConfig.m.get( "VTA_CORE_WRITE_PIPE_INP") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Int]
        result
    }
  }
  def pipeDelayWrOut() = {
    val default_result = 1
    JSONConfig.m.get( "VTA_CORE_WRITE_PIPE_OUT") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Int]
        result
    }
  }
  def pipeDelayVmeInp() = {
    val default_result = 0
    JSONConfig.m.get( "VTA_CORE_VME_DATA_PIPE_INP") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Int]
        result
    }
  }
  def pipeDelayVmeWgt() = {
    val default_result = 0
    JSONConfig.m.get( "VTA_CORE_VME_DATA_PIPE_WGT") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Int]
        result
    }
  }
  def pipeDelayVmeAcc() = {
    val default_result = 0
    JSONConfig.m.get( "VTA_CORE_VME_DATA_PIPE_ACC") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Int]
        result
    }
  }
  def pipeDelayVmeUop() = {
    val default_result = 0
    JSONConfig.m.get( "VTA_CORE_VME_DATA_PIPE_UOP") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Int]
        result
    }
  }
  def pipeDelayGemmRdInpWgt() = {
    val default_result = 0
    JSONConfig.m.get( "VTA_CORE_GEMM_RD_DATA_PIPE") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Int]
        result
    }
  }
  def pipeDelayGemmRdUop() = {
    val default_result = 0
    JSONConfig.m.get( "VTA_CORE_GEMM_RD_UOP_PIPE") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Int]
        result
    }
  }
  def pipeDelayGemmInpIdx() = {
    val default_result = 0
    JSONConfig.m.get( "VTA_CORE_GEMM_INP_IDX_PIPE") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Int]
        result
    }
  }
  def pipeDelayAluRdData() = {
    val default_result = 0
    JSONConfig.m.get( "VTA_CORE_ALU_RD_DATA_PIPE") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Int]
        result
    }
  }
}
