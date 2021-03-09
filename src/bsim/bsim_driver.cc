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

/*!
 * \file bsim_driver.cc
 * \brief VTA driver for simulated behavioral model backend.
 */
#include <dmlc/logging.h>
#include <vta/driver.h>
#include <vta/hw_spec.h>
#include <tvm/runtime/registry.h>
#include <tvm/runtime/packed_func.h>
#include <vta/sim_tlpp.h>
#include <type_traits>
#include <mutex>
#include <map>
#include <unordered_map>
#include <cstring>
#include <sstream>

#define IGNORE_PRINTF
#ifdef IGNORE_PRINTF
#define printf(fmt, ...) (void)(0)
#endif

tvm::runtime::PackedFunc *BehavioralModel;            // python function

namespace vta {
namespace bsim {

class TensorBackedVirtualMemoryManager {
public:
  ~TensorBackedVirtualMemoryManager() {
    delete []pages_inuse;
  }
  /*!
   * \brief Get virtual address given physical address.
   * \param phy_addr The simulator phyiscal address.
   * \return The true virtual address;
   */
  void* GetAddr(uint64_t phy_addr) {
    return (void*)(reinterpret_cast<uint64_t>(this->dltensor_->data) + phy_addr);
  }
  /*!
   * \brief Get physical address
   * \param buf The virtual address.
   * \return The true physical address;
   */
  vta_phy_addr_t GetPhyAddr(const void* buf) {
    return (vta_phy_addr_t)(
      reinterpret_cast<uint64_t>(buf) - reinterpret_cast<uint64_t>(this->dltensor_->data)
    );
  }
  /*!
   * \brief Allocate memory from manager
   * \param size The size of memory
   * \return The virtual address
   */
  void* Alloc(size_t size) {
    std::lock_guard<std::mutex> lock(mutex_);
    size_t npage = (size + kPageSize - 1) / kPageSize;
    uint32_t loc = this->find(npage);
    printf("Alloc %ld pages at page %d\n", npage, loc);
    this->pages_inuse[loc] = npage;
    for (size_t i=1; i<npage; i++)
      this->pages_inuse[loc+i] = -1;
    return (void*)(reinterpret_cast<uint64_t>(this->dltensor_->data) + (loc << kPageBits));
  }

  /*!
   * \brief Free the memory.
   * \param data The virtual address
   */
  void Free(void* data) {
    std::lock_guard<std::mutex> lock(mutex_);
    uint32_t loc = this->GetPhyAddr(data) >> kPageBits;
    size_t npage = this->pages_inuse[loc];
    printf("Free at page %d for %ld pages\n", loc, npage);
    memset(data, 0xAA, npage*kPageSize); // paint memory to 0xAA
    CHECK_GT(npage, 0) << "trying to free invalid page";
    for (size_t i=0; i<npage; i++)
      this->pages_inuse[loc+i] = 0;
  }

  /*!
   * \brief Copy from the host memory to device memory (virtual).
   * \param dst The device memory address (virtual)
   * \param src The host memory address
   * \param size The size of memory
   */
  void MemCopyFromHost(void* dst, const void * src, size_t size) {
    memcpy(dst, src, size);
  }

  /*!
   * \brief Copy from the device memory (virtual) to host memory.
   * \param dst The host memory address
   * \param src The device memory address (virtual)
   * \param size The size of memory
   */
  void MemCopyToHost(void* dst, const void * src, size_t size) {
    memcpy(dst, src, size);
  }

  // first time: user must provide a valid dltensor Global(dltensor)
  // for future use, Global() will return the instance
  static TensorBackedVirtualMemoryManager* Global(DLTensor *dl=nullptr) {
    static TensorBackedVirtualMemoryManager *inst = nullptr;
    if (dl) {
    //  CHECK_EQ(inst, nullptr) << "TensorBackedVirtualMemoryManager instance exists";
      inst = new TensorBackedVirtualMemoryManager(dl);
    } else {
      CHECK_NOTNULL(inst);
    }
    return inst;
  }

/*
  void PrintDLTensor() {
    printf("PrintDLTensor DLTensor size_in_bytes=%ld\n", size_in_bytes);
    u_int8_t *p = (u_int8_t*)dltensor_->data;
    for(int i=0x200; i<0x200+16; i++)
      printf("%p: %d, ", p+i, p[i]);
    printf("\n");
  }
*/

private:
  // The bits in page table
  static constexpr vta_phy_addr_t kPageBits = VTA_PAGE_BITS;
  // page size, also the maximum allocable size 16 K
  static constexpr vta_phy_addr_t kPageSize = VTA_PAGE_BYTES;

  // Internal lock
  std::mutex mutex_;

  DLTensor* dltensor_;  // memory is backed by this tensor
  size_t size_in_bytes;
  size_t num_pages;

  // pages_inuse[2]=5 means 5 pages allocated starting from loc 2
  // 0 means that page has not been allocated
  // -1 means that it's allocated, as part of bigger chunk
  int* pages_inuse;

  TensorBackedVirtualMemoryManager(DLTensor *dltensor) : dltensor_(dltensor) {
    uint32_t len = dltensor->shape[0];
    size_in_bytes = (dltensor->dtype.bits >> 3) * len;
    num_pages = size_in_bytes >> kPageBits;
    pages_inuse = new int[num_pages];
    std::fill(pages_inuse, pages_inuse+num_pages, 0);
    pages_inuse[0] = 1; // reserve the first page
    printf("TensorBackedVirtualMemoryManager::base addr: %p\n", dltensor_->data);
  }

  uint32_t find(size_t pages) {
    for (size_t i=0; i<num_pages-pages; i++) {
      bool avail = true;
      for (size_t j=0; j<pages; j++) {
        if (pages_inuse[i+j] != 0) {
          avail = false;
          break;
        }
      }
      if (avail)
        return i;
    }
    CHECK(false) << "no enough pages!";
    return 0; // should NOT reach here
  }
};


class Device {
 public:
  Device() {}

  int Run(vta_phy_addr_t insn_phy_addr, uint32_t insn_count, uint32_t wait_cycles) {
    CHECK_NOTNULL(BehavioralModel);
    printf("Device::Run Begin");
    (*BehavioralModel)(insn_phy_addr, insn_count);
    printf("Device::Run End");
    return 0;
  }
};
}
}

using tvm::runtime::TVMRetValue;
using tvm::runtime::TVMArgs;
using vta::bsim::TensorBackedVirtualMemoryManager;

TVM_REGISTER_GLOBAL("vta.bsim.set_behavioral_model")
.set_body([](TVMArgs args, TVMRetValue* rv) {
    tvm::runtime::PackedFunc f = args[0];
    BehavioralModel = new tvm::runtime::PackedFunc(f);

    DLTensor *array = args[1];
    TensorBackedVirtualMemoryManager::Global(array);
  });


void* VTAMemAlloc(size_t size, int cached) {
  return TensorBackedVirtualMemoryManager::Global()->Alloc(size);
}

void VTAMemFree(void* buf) {
  TensorBackedVirtualMemoryManager::Global()->Free(buf);
}

vta_phy_addr_t VTAMemGetPhyAddr(void* buf) {
  vta_phy_addr_t pa = TensorBackedVirtualMemoryManager::Global()->GetPhyAddr(buf);
  return pa;
}

void VTAMemCopyFromHost(void* dst, const void* src, size_t size) {
  memcpy(dst, src, size);
}

void VTAMemCopyToHost(void* dst, const void* src, size_t size) {
  memcpy(dst, src, size);
}

void VTAFlushCache(void* vir_addr, vta_phy_addr_t phy_addr, int size) {
  LOG(FATAL) << "VTAFlushCache called!";
}

void VTAInvalidateCache(void* vir_addr, vta_phy_addr_t phy_addr, int size) {
  LOG(FATAL) << "VTAInvalidateCache called!";
}

VTADeviceHandle VTADeviceAlloc() {
  CHECK_NOTNULL(BehavioralModel);
  return new vta::bsim::Device();
}

void VTADeviceFree(VTADeviceHandle handle) {
  delete static_cast<vta::bsim::Device*>(handle);
}

int VTADeviceRun(VTADeviceHandle handle,
                 vta_phy_addr_t insn_phy_addr,
                 uint32_t insn_count,
                 uint32_t wait_cycles) {
  int ret = static_cast<vta::bsim::Device*>(handle)->Run(
      insn_phy_addr, insn_count, wait_cycles);
  return ret;
}

void VTAProgram(const char* bitstream) {
  LOG(FATAL) << "VTAProgram called!";
}