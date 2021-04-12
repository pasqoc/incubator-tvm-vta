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
 *
 * \file de10-nano_driver.cc
 * \brief VTA driver for DE10_Nano board.
 */

// Modified by contributors from Intel Labs

#include "de10nano_driver.h"
#include "de10nano_mgr.h"

#include <string.h>
#include <vta/driver.h>
#include <tvm/runtime/registry.h>
#include <dmlc/logging.h>
#include <thread>
#include <string>
#include "cma_api.h"

void* VTAMemAlloc(size_t size, int cached) {
  static int _ = cma_init(); (void)_;
  if (cached) {
    return cma_alloc_cached(size);
  } else {
    return cma_alloc_noncached(size);
  }
}

void VTAMemFree(void* buf) {
  cma_free(buf);
}

vta_phy_addr_t VTAMemGetPhyAddr(void* buf) {
  return cma_get_phy_addr(buf) + 0x80000000;
}

void VTAMemCopyFromHost(void* dst, const void* src, size_t size) {
  // For SoC-based FPGAs that used shared memory with the CPU, use memcopy()
  memcpy(dst, src, size);
}

void VTAMemCopyToHost(void* dst, const void* src, size_t size) {
  // For SoC-based FPGAs that used shared memory with the CPU, use memcopy()
  memcpy(dst, src, size);
}

void VTAFlushCache(void * offset, vta_phy_addr_t buf, int size) {
  CHECK(false) << "VTAFlushCache not implemented for de10nano";
  printf("VTAFlushCache not implemented for de10nano");
}

void VTAInvalidateCache(void * offset, vta_phy_addr_t buf, int size) {
  CHECK(false) << "VTAInvalidateCache not implemented for de10nano";
  printf("VTAInvalidateCache not implemented for de10nano");
}

void *VTAMapRegister(uint32_t addr) {
  // Align the base address with the pages
  uint32_t virt_base = addr & ~(getpagesize() - 1);
  // Calculate base address offset w.r.t the base address
  uint32_t virt_offset = addr - virt_base;
  // Open file and mmap
  uint32_t mmap_file = open("/dev/mem", O_RDWR|O_SYNC);
  // Note that if virt_offset != 0, i.e. addr is not page aligned
  // munmap will not be unmapping all memory.
  void *vmem = mmap(NULL,
              (VTA_IP_REG_MAP_RANGE + virt_offset),
              PROT_READ|PROT_WRITE,
              MAP_SHARED,
              mmap_file,
              virt_base);
  close(mmap_file);
  return vmem;
}

void VTAUnmapRegister(void *vta) {
  // Unmap memory
  int status = munmap(vta, VTA_IP_REG_MAP_RANGE);
  assert(status == 0);
}

void VTAWriteMappedReg(void* base_addr, uint32_t offset, uint32_t val) {
  *((volatile uint32_t *) (reinterpret_cast<char *>(base_addr) + offset)) = val;
}

uint32_t VTAReadMappedReg(void* base_addr, uint32_t offset) {
  return *((volatile uint32_t *) (reinterpret_cast<char *>(base_addr) + offset));
}

class Profiler {
 public:
  Profiler() {
    counters_ = new int[num_counters_];
    this->ClearAll();
  }

  ~Profiler() {
    delete [] counters_;
  }

  /*! \brief update one event counter */
  void Update(uint32_t idx, uint32_t value) {
    counters_[idx] += value;
  }

  /*! \brief clear one event counter*/
  void Clear(uint32_t idx) {
    counters_[idx] = 0;
  }

  /*! \brief clear all event counters */
  void ClearAll() {
    for (uint32_t i = 0; i < num_counters_; i++) {
      counters_[i] = 0;
    }
  }

  /*! \brief return counters as json */
  std::string AsJSON() {
    uint32_t bytes_per_pulse = 8;
    std::ostringstream os;
    os << "{\n"
       << " \"cycle_counter\":" << counters_[0] << ",\n"
       << " \"inp_load_nbytes\":" << counters_[3] * bytes_per_pulse << ",\n"
       << " \"wgt_load_nbytes\":" << counters_[4] * bytes_per_pulse << ",\n"
       << " \"acc_load_nbytes\":" << counters_[2] * bytes_per_pulse << ",\n"
       << " \"uop_load_nbytes\":" << counters_[5] * bytes_per_pulse << ",\n"
       << " \"out_store_nbytes\":" << counters_[6] * bytes_per_pulse << ",\n"
       << " \"gemm_counter\":" << counters_[8] << ",\n"
       << " \"alu_counter\":" << counters_[7] << ",\n"
       << " \"acc_wr_counter\":" << counters_[1] << ",\n"
       << " \"idle_ld_cycles\":" << counters_[9] << ",\n"
       << " \"idle_st_cycles\":" << counters_[10] << ",\n"
       << " \"idle_cp_cycles\":" << counters_[11] << ",\n"
       << " \"stall_ld_cycles\":" << counters_[12] << ",\n"
       << " \"stall_st_cycles\":" << counters_[13] << ",\n"
       << " \"stall_cp_cycles\":" << counters_[14] << "\n"
       <<"}\n";
    return os.str();
  }

  static Profiler* Global() {
    static Profiler inst;
    return &inst;
  }

 private:
  /*! \brief total number of event counters */
  uint32_t num_counters_{15};
  /*! \brief event counters */
  int* counters_{nullptr};
};

class VTADevice {
 public:
  VTADevice() {
    // VTA stage handles
    vta_host_handle_ = VTAMapRegister(VTA_HOST_ADDR);
    prof_ = Profiler::Global();
  }

  ~VTADevice() {
    // Close VTA stage handle
    VTAUnmapRegister(vta_host_handle_);
  }

  int Run(vta_phy_addr_t insn_phy_addr,
          uint32_t insn_count,
          uint32_t wait_cycles) {
    VTAWriteMappedReg(vta_host_handle_, 0x04, 0);
    VTAWriteMappedReg(vta_host_handle_, 0x08, insn_count);
    VTAWriteMappedReg(vta_host_handle_, 0x0c, insn_phy_addr);
    VTAWriteMappedReg(vta_host_handle_, 0x10, 0);
    VTAWriteMappedReg(vta_host_handle_, 0x14, 0);
    VTAWriteMappedReg(vta_host_handle_, 0x18, 0);
    VTAWriteMappedReg(vta_host_handle_, 0x1c, 0);
    VTAWriteMappedReg(vta_host_handle_, 0x20, 0);
    VTAWriteMappedReg(vta_host_handle_, 0x24, 0); // acc_wr_event
    VTAWriteMappedReg(vta_host_handle_, 0x28, 0); // acc_ld_event
    VTAWriteMappedReg(vta_host_handle_, 0x2c, 0); // inp_ld_event
    VTAWriteMappedReg(vta_host_handle_, 0x30, 0); // wgt_ld_event
    VTAWriteMappedReg(vta_host_handle_, 0x34, 0); // uop_ld_event
    VTAWriteMappedReg(vta_host_handle_, 0x38, 0); // out_st_event
    VTAWriteMappedReg(vta_host_handle_, 0x3c, 0); // alu_lp_event
    VTAWriteMappedReg(vta_host_handle_, 0x40, 0); // gem_lp_event
    VTAWriteMappedReg(vta_host_handle_, 0x44, 0); // idle_ld_event
    VTAWriteMappedReg(vta_host_handle_, 0x48, 0); // idle_st_event
    VTAWriteMappedReg(vta_host_handle_, 0x4c, 0); // idle_cp_event
    VTAWriteMappedReg(vta_host_handle_, 0x50, 0); // stall_ld_event
    VTAWriteMappedReg(vta_host_handle_, 0x54, 0); // stall_st_event
    VTAWriteMappedReg(vta_host_handle_, 0x58, 0); // stall_cp_event

    // VTA start
    VTAWriteMappedReg(vta_host_handle_, 0x0, VTA_START);

    // Loop until the VTA is done
    unsigned t, flag = 0;
    for (t = 0; t < wait_cycles; ++t) {
      flag = VTAReadMappedReg(vta_host_handle_, 0x00);
      flag &= 0x2;
      if (flag == 0x2) break;
      std::this_thread::yield();
    }

    prof_->Update(0, VTAReadMappedReg(vta_host_handle_, 0x04));
    for (uint i = 1, a = 0x24; i < 15; i++, a+=0x4)
      prof_->Update(i, VTAReadMappedReg(vta_host_handle_, a));

    // Report error if timeout
    return t < wait_cycles ? 0 : 1;
  }

 private:
  // VTA handles (register maps)
  void* vta_host_handle_{nullptr};
  // Profiler
  Profiler* prof_;
};

VTADeviceHandle VTADeviceAlloc() {
  return new VTADevice();
}

void VTADeviceFree(VTADeviceHandle handle) {
  delete static_cast<VTADevice*>(handle);
}

int VTADeviceRun(VTADeviceHandle handle,
                 vta_phy_addr_t insn_phy_addr,
                 uint32_t insn_count,
                 uint32_t wait_cycles) {
  return static_cast<VTADevice*>(handle)->Run(
      insn_phy_addr, insn_count, wait_cycles);
}

void VTAProgram(const char *rbf) {
  De10NanoMgr mgr;
  CHECK(mgr.mapped()) << "de10nano: mapping of /dev/mem failed";
  CHECK(mgr.program_rbf(rbf)) << "Programming of the de10nano failed.\n"
  "This is usually due to the use of an RBF file that is incompatible "
  "with the MSEL switches on the DE10-Nano board. The recommended RBF "
  "format is FastPassiveParallel32 with compression enabled, "
  "corresponding to MSEL 01010. An RBF file in FPP32 mode can be "
  "generated in a Quartus session with the command "
  "'quartus_cpf -o bitstream_compression=on -c <file>.sof <file>.rbf'.";
}

using tvm::runtime::TVMRetValue;
using tvm::runtime::TVMArgs;

TVM_REGISTER_GLOBAL("vta.de10nano.program")
.set_body([](TVMArgs args, TVMRetValue* rv) {
    std::string bitstream = args[0];
    VTAProgram(bitstream.c_str());
});

TVM_REGISTER_GLOBAL("vta.de10nano.profiler_clear")
.set_body([](TVMArgs args, TVMRetValue* rv) {
    Profiler::Global()->ClearAll();
  });

TVM_REGISTER_GLOBAL("vta.de10nano.profiler_status")
.set_body([](TVMArgs args, TVMRetValue* rv) {
    *rv = Profiler::Global()->AsJSON();
  });
