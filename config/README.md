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

# VTA Configuration

Each VTA runtime/hardware configuration is specified through file vta_config.json containing the values of HW parameters, and through file vta_target.json containing the name and version of a supported target, e.g. sim, tsim, de10nano, etc.

The path to both config and target files can be overridden thorough the use of corresponding environment variables VTA_CONFIG and VTA_TARGET.
Specifically, when these variables are present the resolved file paths are, respectively:

```sh
${VTA_HW_PATH}/config/${VTA_CONFIG}.json
${VTA_HW_PATH}/config/${VTA_TARGET}.json
```

Note that, for backward compatibility, only file vta_config.json is needed, provided it contains the TARGET and HW_VER setting information.

Splitting parameter configuration and target setting enables to more easily run workloads for different targets that use the same parameter configuration which does not require vta runtime recompilation.

You can copy the vta_config.json to tvm project root and modify the configuration before you type make.

The config is going to affect the behavior of python package as well as
the hardware runtime build.

<!--- The following files in this directory were modified by Intel Labs contributors: -->
<!---                                      -->
<!--- 1x32x32_12_15_15_15_256_burst1.json -->
<!--- 1x32x32_12_15_15_15_256_burst2.json -->
<!--- 1x32x32_12_15_15_15_256.json -->
<!--- 1x32x8_15_15_18_17.json -->
<!--- 1x8x32_15_15_18_17.json -->
<!--- 4x8x8_11_16_16_18.json -->
<!--- batch2.json -->
<!--- block32_128.json -->
<!--- block32.json -->
<!--- both_batch2_block32.json -->
<!--- bsim_target.json -->
<!--- de10nano_target.json -->
<!--- fsim_target.json -->
<!--- log_batch-1.json -->
<!--- log_block-5.json -->
<!--- minimal.json -->
<!--- pynq_target.json -->
<!--- tsim_target.json -->
<!--- ultra96_target.json -->
<!--- vta_config_be_huge.json -->
<!--- vta_config_be_large.json -->
<!--- vta_config_be_small.json -->
<!--- vta_config_burst1.json -->
<!--- vta_config_burst2.json -->
<!--- vta_config.json -->
<!--- vta_target.json -->
<!--- vta_target.py -->
