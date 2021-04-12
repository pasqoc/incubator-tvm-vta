# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

# Modified by contributors from Intel Labs

export VTA_TARGET = pynq

# Directories
ROOTDIR = $(CURDIR)
VTA_HW_DIR = $(shell readlink -f $(CURDIR)/../..)
BUILD_DIR = $(VTA_HW_DIR)/build/hardware/xilinx
SCRIPT_DIR = $(CURDIR)/scripts
SRC_DEPS := $(wildcard $(CURDIR)/src/*)
SRC_DEPS += $(wildcard $(CURDIR)/sim/*)
SRC_DEPS += $(VTA_HW_DIR)/include/vta/hw_spec.h
SRC_DEPS += $(wildcard $(VTA_HW_DIR)/tests/hardware/common/*)

# Executables
VIVADO_HLS = vivado_hls
VIVADO = vivado

# Process VTA JSON config
CONFIG_PY := $(VTA_HW_DIR)/config/vta_config.py

# Derive config name
CONF   := $(shell VTA_TARGET=$(VTA_TARGET) python3 $(CONFIG_PY) --cfg-str)
HW_VER := $(shell VTA_TARGET=$(VTA_TARGET) python3 $(CONFIG_PY) --hw-ver)
TARGET := $(shell VTA_TARGET=$(VTA_TARGET) python3 $(CONFIG_PY) --target)
IP_BUILD_PATH := $(BUILD_DIR)/hls/$(CONF)
HW_BUILD_PATH := $(BUILD_DIR)/vivado/$(CONF)

# Config files
CONFIG_TCL = $(BUILD_DIR)/include/vta_config.tcl

ifeq ($(VTA_CACHE_PATH),)
  VTA_CACHE_PATH := $(HOME)/.vta_cache
endif
CACHE_DIR := $(VTA_CACHE_PATH)/$(TARGET)/$(HW_VER)

# IP file path
IP_PATH := $(BUILD_DIR)/hls/$(CONF)/vta_compute/soln/impl/ip/xilinx_com_hls_compute_1_0.zip

# Bitstream file path
BIT_PATH := $(BUILD_DIR)/vivado/$(CONF)/export/vta.bit

.PHONY: all ip bit clean clean_all

all: bit
ip: $(IP_PATH)
bit: $(BIT_PATH)

$(BUILD_DIR)/include:
	mkdir -p $@

$(CONFIG_TCL): $(BUILD_DIR)/include
	VTA_TARGET=$(VTA_TARGET) python3 $(CONFIG_PY) --export-tcl $@

$(IP_PATH): $(SRC_DEPS) ${CONFIG_TCL}
	rm -rf $(IP_BUILD_PATH)
	mkdir -p $(IP_BUILD_PATH)
	cd $(IP_BUILD_PATH) && \
		$(VIVADO_HLS) \
		-f $(SCRIPT_DIR)/hls.tcl \
		-tclargs \
			$(VTA_HW_DIR) \
			${CONFIG_TCL}

$(BIT_PATH): $(IP_PATH)
	rm -rf $(HW_BUILD_PATH)
	mkdir -p $(HW_BUILD_PATH)
	cd $(HW_BUILD_PATH) && \
		$(VIVADO) \
		-mode tcl \
		-source $(SCRIPT_DIR)/vivado.tcl \
		-tclargs \
			$(BUILD_DIR)/hls/$(CONF) \
			${CONFIG_TCL}; \
	echo "checksum: `sha1sum $(BIT_PATH) | cut -d' ' -f1`"

program:
	python3 $(SCRIPT_DIR)/program.py $(BIT_PATH) && \
	echo "checksum: `sha1sum $(BIT_PATH) | cut -d' ' -f1`"

install:
	mkdir -p $(CACHE_DIR); \
	conf=$(CONF); \
	cp $(BIT_PATH) $(CACHE_DIR)/$${conf#*_}.bit

clean:
	rm -rf *.out *.log

cleanall: clean
	rm -rf $(BUILD_DIR)
