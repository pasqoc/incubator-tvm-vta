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

package vta.dpi

import vta.util.config._

/** CoreConfig.
 *
 * This is one supported configuration for VTA. This file will
 * be eventually filled out with class configurations that can be
 * mixed/matched with Shell configurations for different backends.
 */
class DpiConfig extends Config((site, here, up) => {
  case DpiKey =>
    VTAMemDPIParams(
      dpiDelay  = JSONConfig.m("DPI_DELAY").asInstanceOf[Int],
      dpiLenBits = 8,
      dpiAddrBits = 64,
      dpiDataBits = 64,
      dpiTagBits  = 8
    )
})
class JSONDpiConfig extends Config((site, here, up) => {

  case DpiKey =>
    VTAMemDPIParams(
      dpiDelay    = JSONConfig.m("DPI_DELAY").asInstanceOf[Int],
      dpiLenBits  = JSONConfig.m("DPI_LEN_BITS").asInstanceOf[Int],
      dpiAddrBits = JSONConfig.m("DPI_ADDR_BITS").asInstanceOf[Int],
      dpiDataBits = JSONConfig.m("DPI_DATA_BITS").asInstanceOf[Int],
      dpiTagBits  = JSONConfig.m("DPI_TAG_BITS").asInstanceOf[Int]
    )
})
