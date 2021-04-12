<!--- Licensed to the Apache Software Foundation (ASF) under one -->
<!--- or more contributor license agreements.  See the NOTICE file -->
<!--- distributed with this work for additional information -->
<!--- regarding copyright ownership.  The ASF licenses this file -->
<!--- to you under the Apache License, Version 2.0 (the -->
<!--- "License"); you may not use this file except in compliance -->
<!--- with the License.  You may obtain a copy of the License at -->

<!---   http://www.apache.org/licenses/LICENSE-2.0 -->

<!--- Unless required by applicable law or agreed to in writing, -->
<!--- software distributed under the License is distributed on an -->
<!--- "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY -->
<!--- KIND, either express or implied.  See the License for the -->
<!--- specific language governing permissions and limitations -->
<!--- under the License. -->

<!--- Modified by contributors from Intel Labs -->

VTA Hardware Design Stack
=========================
[![Build Status](https://ci.tlcpack.ai/job/tvm-vta/job/main/badge/icon)](https://ci.tlcpack.ai/job/tvm-vta/job/main/)

VTA (versatile tensor accelerator) is an open-source deep learning accelerator complemented with an end-to-end TVM-based compiler stack.

The key features of VTA include:

- Generic, modular, open-source hardware
  - Streamlined workflow to deploy to FPGAs.
  - Simulator support to prototype compilation passes on regular workstations.
- Driver and JIT runtime for both simulator and FPGA hardware back-end.
- End-to-end TVM stack integration
  - Direct optimization and deployment of models from deep learning frameworks via TVM.
  - Customized and extensible TVM compiler back-end.
  - Flexible RPC support to ease deployment, and program FPGAs with the convenience of Python.

VTA configuration options:
VTA chisel specific configuration parameters are defined in Config.scala chisel files.
VTA-TVM shared parameters are defined in json config file.
Environment variable VTA_CONFIG is used to control json configration file name.

export VTA_CONFIG=vta_config
config/vta_config.json

Parameters:

Original fetaures represent VTA memory system before optimizations.
The default value is false.
"VTA_FEATURE_ORIGINAL_VME" : false/true, - original VME can handle only one request at a time
"VTA_FEATURE_ORIGINAL_TENSOR_LOAD" : false/true, - original one is limited by *_DATA_BITS <= tensor bits
"VTA_FEATURE_ORIGINAL_FETCH" : false/true, - original fetch is limited by *_DATA_BITS = 64
"VTA_FEATURE_ORIGINAL_LOAD_UOP" : false/true, - original loadUop is limited by *_DATA_BITS = 64
"VTA_FEATURE_ORIGINAL_TENSOR_STORE" : false/true, - original one is limited by *_DATA_BITS <= tensor bits
"VTA_FEATURE_ORIGINAL_QUEUES" : false/true, - original Queues use Chisel Queue. New ones use SyncMemory 1r1w.


AXI parameters:
"SHL_MEM_AXI_DATA_BITS" : 64, - supported 64,128,256,512. should match DPI_DATA_BITS
"SHL_VME_CLT_TAG_BITS" : 19, - should have enough bits to cover largest scratchpad address + 2*offsets of tensor in data bits

DPI parameters:

"DPI_LEN_BITS" : 8, - the number of pulses in a single transaction
"DPI_ADDR_BITS" : 64,
"DPI_DATA_BITS" : 64, - supported 64,128,256,512. should match SHL_MEM_AXI_DATA_BITS
"DPI_TAG_BITS" : 8,
"DPI_DELAY" : 16,


Data pipelining parameters:

GEMM/ALU is split into a   VTA_CORE_BLOCK_OUT_SPLIT_FACTOR number of groups which
write same acc memory module and read same wgt modules.
"VTA_CORE_BLOCK_OUT_SPLIT_FACTOR" : 1, - the default value is 1. Used to group a few MVC/ALU blocks which write same acc memory module. Instr Gemm/Alu mux is split per grpoup.
"VTA_CORE_INP_SP_SPLIT_FACTOR" : 1, - the default value is 1. Used to split INP memory modules to pipe write/read them separately

WRITE_PIPE defines a pipe per macro memory on a memory write path
It doesnt pipe direct write on tensor access interface
"VTA_CORE_WRITE_PIPE_WGT" : 0, - default is 0. Pipes memory load write operation per macro memory
"VTA_CORE_WRITE_PIPE_INP" : 0, - default is 0. Pipes memory load write operation per macro memory
"VTA_CORE_WRITE_PIPE_OUT" : 0, - default is 0. Pipes memory direct write operation 
"VTA_CORE_WRITE_PIPE_ACC" : 0, - default is 0. Pipes memory load write operation per macro memory

VME_DATA_PIPE defines a single pipe on VME data load path
"VTA_CORE_VME_DATA_PIPE_INP" : 0, - default is 0. Pipes vme data.
"VTA_CORE_VME_DATA_PIPE_WGT" : 0, - default is 0. Pipes vme data.
"VTA_CORE_VME_DATA_PIPE_ACC" : 0, - default is 0. Pipes vme data.
"VTA_CORE_VME_DATA_PIPE_UOP" : 0, - default is 0. Pipes vme data.
Fetch VME read pipe delay is 1

GEMM internal pipes. Used to pipe scrachpad/uop reads
"VTA_CORE_GEMM_RD_DATA_PIPE" : 0,- default is 0. Pipes scratchpad read data.
"VTA_CORE_GEMM_INP_IDX_PIPE" : 0,- default is 0. Pipes Inp read index.
