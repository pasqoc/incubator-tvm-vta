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

/*!
 * \file sim_driver.cc
 * \brief VTA driver for simulated backend.
 */
#include <vta/driver.h>
#include <vta/hw_spec.h>
#include <tvm/runtime/registry.h>
#include <vta/sim_tlpp.h>
#include <type_traits>
#include <mutex>
#include <map>
#include <unordered_map>
#include <cstring>
#include <sstream>
#include <cmath>

#include <iomanip>

#include "../vmem/virtual_memory.h"
#include "../vta/runtime/trace_mgr.h"

namespace vta {
namespace sim {

//#define DIVERGENCE_VERIFICATION
#ifdef DIVERGENCE_VERIFICATION
// Define here the address and value of the divergence point as
// identified in the fsim trace.
uint32_t dp_trace_addr = 0x10;
uint32_t dp_trace_data = 0x7f;
#endif // DIVERGENCE_VERIFICATION

/*! \brief debug flag for skipping computation */
enum DebugFlagMask {
  kSkipExec = 0
};

/*!
 * \brief Helper class to pack and unpack bits
 *  Applies truncation when pack to low level bits.
 *
 * \tparam bits The number of bits in integer.
 * \note This implementation relies on little endian.
 */
template<uint32_t bits>
class BitPacker {
 public:
  explicit BitPacker(void* data) {
    data_ = static_cast<uint32_t*>(data);
  }

  uint32_t GetUnsigned(uint32_t index) const {
    if (bits == 32) {
      return data_[index];
    } else if (bits == 16) {
      return reinterpret_cast<uint16_t*>(data_)[index];
    } else if (bits == 8) {
      return reinterpret_cast<uint8_t*>(data_)[index];
    } else {
      uint32_t offset = index / kNumPackElem;
      uint32_t shift = index % kNumPackElem;
      return (data_[offset] >> shift) & kMask;
    }
  }

  int32_t GetSigned(uint32_t index) const {
    if (bits == 32) {
      return reinterpret_cast<int32_t*>(data_)[index];
    } else if (bits == 16) {
      return reinterpret_cast<int16_t*>(data_)[index];
    } else if (bits == 8) {
      return reinterpret_cast<int8_t*>(data_)[index];
    } else {
      uint32_t offset = index / kNumPackElem;
      uint32_t shift = (index % kNumPackElem) * bits;
      int32_t uvalue = static_cast<int32_t>(
          (data_[offset] >> shift) & kMask);
      int kleft = 32 - bits;
      return (uvalue << kleft) >> kleft;
    }
  }

  void SetUnsigned(uint32_t index, uint32_t value) {
    if (bits == 32) {
      data_[index] = value;
    } else if (bits == 16) {
      reinterpret_cast<uint16_t*>(data_)[index] = value;
    } else if (bits == 8) {
      reinterpret_cast<uint8_t*>(data_)[index] = value;
    } else {
      uint32_t offset = index / kNumPackElem;
      uint32_t shift = (index % kNumPackElem) * bits;
      data_[offset] &= (~(kMask << shift));
      data_[offset] |= (value & kMask) << shift;
    }
  }

  void SetSigned(uint32_t index, int32_t value) {
    if (bits == 32) {
      reinterpret_cast<int32_t*>(data_)[index] = value;
    } else if (bits == 16) {
      reinterpret_cast<int16_t*>(data_)[index] = value;
    } else if (bits == 8) {
      reinterpret_cast<int8_t*>(data_)[index] = value;
    } else {
      uint32_t offset = index / kNumPackElem;
      uint32_t shift = (index % kNumPackElem) * bits;
      data_[offset] &= (~(kMask << shift));
      data_[offset] |= static_cast<uint32_t>(value & kMask) << shift;
    }
  }

 private:
  uint32_t* data_;
  static constexpr uint32_t kNumPackElem = 32 / bits;
  static constexpr uint32_t kMask = (1U << (bits >= 32U ? 31U : bits)) - 1U;
};

/*!
 * \brief DRAM memory manager
 *  Implements simple paging to allow physical address translation.
 */
using DRAM = ::vta::vmem::VirtualMemoryManager;

/*!
 * \brief Register file.
 * \tparam kBits Number of bits of one value.
 * \tparam kLane Number of lanes in one element.
 * \tparam kMaxNumElem Maximum number of element.
 */
template<int kBits, int kLane, int kMaxNumElem>
class SRAM {
 public:
  /*! \brief Bytes of single vector element */
  static const int kElemBytes = (kBits * kLane + 7) / 8;
  /*! \brief content data type */
  using DType = typename std::aligned_storage<kElemBytes, kElemBytes>::type;
  SRAM() {
    data_ = new DType[kMaxNumElem];
  }
  ~SRAM() {
    delete [] data_;
  }
  // Get the i-th index
  void* BeginPtr(uint32_t index) {
    CHECK_LT(index, kMaxNumElem);
    return &(data_[index]);
  }
  // Execute the load instruction on this SRAM
  uint32_t Load(const VTAMemInsn* op,
            DRAM* dram,
            uint64_t* load_counter,
            bool skip_exec) {
    // DRAM transfer data should come from JSON.
    uint64_t dram_data_bytes = 8, transfer_pulses = 16, transfer_cycles = 4;
    uint64_t load_bytes = (op->x_size * op->y_size) * kElemBytes;
    uint64_t transfer_waste_bytes = load_bytes % dram_data_bytes;
    if (transfer_waste_bytes)
      load_bytes += dram_data_bytes - transfer_waste_bytes;
    load_counter[0] += load_bytes;
    uint64_t pulses = load_bytes / dram_data_bytes;
    uint64_t transfers = (pulses + transfer_pulses - 1) / transfer_pulses;
    uint32_t cycles = transfer_cycles * transfers + pulses;
    if (skip_exec) return cycles;
    DType* sram_ptr = data_ + op->sram_base;
    uint8_t* dram_ptr = static_cast<uint8_t*>(dram->GetAddr(
        op->dram_base * kElemBytes));
    uint64_t xtotal = op->x_size + op->x_pad_0 + op->x_pad_1;
    uint32_t ytotal = op->y_size + op->y_pad_0 + op->y_pad_1;
    uint64_t sram_end = op->sram_base + xtotal * ytotal;
    CHECK_LE(sram_end, kMaxNumElem);

    uint8_t pad_value[kElemBytes]; // a block of kElemBytes bytes is used for padding
    for (uint32_t p = 0; p < kElemBytes; p++) { 
      if (op->is_pad_min_value && (p % (kBits/8) == (kBits/8) - 1)) {
        pad_value[p] = 0x80;
      } else {
        pad_value[p] = 0x00;
      }
    }

    //memset(sram_ptr, pad_value, kElemBytes * xtotal * op->y_pad_0);
    for (uint32_t p = 0; p < xtotal * op->y_pad_0; p++) {
      memcpy(sram_ptr + p, pad_value, kElemBytes);
    }
    sram_ptr += xtotal * op->y_pad_0;

    for (uint32_t y = 0; y < op->y_size; ++y) {
      //memset(sram_ptr, pad_value, kElemBytes * op->x_pad_0);
      for (uint32_t p = 0; p < op->x_pad_0; p++) {
        memcpy(sram_ptr + p, pad_value, kElemBytes);
      }
      sram_ptr += op->x_pad_0;
      memcpy(sram_ptr, dram_ptr, kElemBytes * op->x_size);
      sram_ptr += op->x_size;
      //memset(sram_ptr, pad_value, kElemBytes * op->x_pad_1);
      for (uint32_t p = 0; p < op->x_pad_1; p++) {
        memcpy(sram_ptr + p, pad_value, kElemBytes);
      }
      sram_ptr += op->x_pad_1;
      dram_ptr += kElemBytes * op->x_stride;
    }
    //memset(sram_ptr, pad_value, kElemBytes * xtotal * op->y_pad_1);
    for (uint32_t p = 0; p < xtotal * op->y_pad_1; p++) {
      memcpy(sram_ptr + p, pad_value, kElemBytes);
    }
    uint8_t *sram_bptr = reinterpret_cast<uint8_t*>(data_ + op->sram_base);
    uint32_t *sram_wptr = reinterpret_cast<uint32_t*>(data_ + op->sram_base);
    uint32_t sram_addr = op->sram_base;
    trace_mgr.Header("LD_SRAM", "%3s\n", "IDX");
    for (uint32_t y = 0; y < ytotal; ++y) {
      for (uint32_t x = 0; x < xtotal; ++x) {
        // Trace one vector element per line.
        trace_mgr.Event("LD_SRAM", "%03x", sram_addr);
        if (kBits == 32) {
          for (uint32_t b = 0; b < kLane; ++b)
            trace_mgr.Event("+LD_SRAM", " %08x", *sram_wptr++);
        } else {
          for (uint32_t b = 0; b < kElemBytes; ++b)
            trace_mgr.Event("+LD_SRAM", " %02x", *sram_bptr++);
        }
        trace_mgr.Event("+LD_SRAM", "\n");
        sram_addr++;
      }
    }
    return cycles;
  }
  // Execute the store instruction on this SRAM applying truncation.
  // This relies on the elements being 32 bits wide.
  template<int target_bits>
  void TruncStore(const VTAMemInsn* op, DRAM* dram) {
    CHECK_EQ(op->x_pad_0, 0);
    CHECK_EQ(op->x_pad_1, 0);
    CHECK_EQ(op->y_pad_0, 0);
    CHECK_EQ(op->y_pad_1, 0);
    int target_width = (target_bits * kLane + 7) / 8;
    BitPacker<kBits> src(data_ + op->sram_base);
    BitPacker<target_bits> dst(dram->GetAddr(op->dram_base * target_width));
    for (uint32_t y = 0; y < op->y_size; ++y) {
      for (uint32_t x = 0; x < op->x_size; ++x) {
        uint32_t sram_base = y * op->x_size + x;
        uint32_t dram_base = y * op->x_stride + x;
        trace_mgr.Event("ST_SRAM", "%03x", op->sram_base + sram_base);
        for (int i = 0; i < kLane; ++i) {
          dst.SetSigned(dram_base * kLane + i,
                        src.GetSigned(sram_base * kLane + i));
          trace_mgr.Event("+ST_SRAM", " %02x",
              0xff & dst.GetSigned(dram_base * kLane + i));
        }
        trace_mgr.Event("+ST_SRAM", "\n");
      }
    }
  }

 private:
  /*! \brief internal data content */
  DType* data_;
};


/*!
 * \brief Memory information of special memory region.
 *  Use MemoryInfo as its container type
 */
class Profiler {
 public:
  /*! \brief The memory load statistics */
  uint64_t inp_load_nbytes{0};
  /*! \brief The memory load statistics */
  uint64_t wgt_load_nbytes{0};
  /*! \brief The ACC memory load statistics */
  uint64_t acc_load_nbytes{0};
  /*! \brief The ACC memory load statistics */
  uint64_t uop_load_nbytes{0};
  /*! \brief The ACC memory load statistics */
  uint64_t out_store_nbytes{0};
  /*! \brief instr counter for gemm */
  uint64_t gemm_counter{0};
  /*! \brief instr counter for ALU ops */
  uint64_t alu_counter{0};
  /*! \brief counter for ACC write events */
  uint64_t acc_wr_counter{0};
  /*! \brief counter for estimated cycles */
  uint64_t cycle_counter{0};
  /*! \brief counter for estimated load unit idle cycles */
  uint64_t idle_ld_counter{0};
  /*! \brief counter for estimated store unit idle cycles */
  uint64_t idle_st_counter{0};
  /*! \brief counter for estimated compute unit idle cycles */
  uint64_t idle_cp_counter{0};
  /*! \brief set debug mode */
  int64_t debug_flag{0};
  /*! \brief clear the profiler */
  void Clear() {
    inp_load_nbytes = 0;
    wgt_load_nbytes = 0;
    acc_load_nbytes = 0;
    uop_load_nbytes = 0;
    out_store_nbytes = 0;
    gemm_counter = 0;
    alu_counter = 0;
    acc_wr_counter = 0;
    cycle_counter = 0;
    idle_ld_counter = 0;
    idle_st_counter = 0;
    idle_cp_counter = 0;
  }
  /*! \return Whether we should skip execution. */
  bool SkipExec() const {
    return (debug_flag & DebugFlagMask::kSkipExec) != 0;
  }

  std::string AsJSON() {
    std::ostringstream os;
    os << "{\n"
       << " \"cycle_counter\":" << cycle_counter << ",\n"
       << " \"inp_load_nbytes\":" << inp_load_nbytes << ",\n"
       << " \"wgt_load_nbytes\":" << wgt_load_nbytes << ",\n"
       << " \"acc_load_nbytes\":" << acc_load_nbytes << ",\n"
       << " \"uop_load_nbytes\":" << uop_load_nbytes << ",\n"
       << " \"out_store_nbytes\":" << out_store_nbytes << ",\n"
       << " \"gemm_counter\":" << gemm_counter << ",\n"
       << " \"alu_counter\":" << alu_counter << ",\n"
       << " \"acc_wr_counter\":" << acc_wr_counter << ",\n"
       << " \"idle_ld_cycles\":" << idle_ld_counter << ",\n"
       << " \"idle_st_cycles\":" << idle_st_counter << ",\n"
       << " \"idle_cp_cycles\":" << idle_cp_counter << "\n"
       <<"}\n";
    return os.str();
  }

  static Profiler* ThreadLocal() {
    static thread_local Profiler inst;
    return &inst;
  }
};


// Simulate device
// TODO(tqchen,thierry): queue based event driven simulation.
class Device {
 public:
  Device() {
    prof_ = Profiler::ThreadLocal();
    dram_ = DRAM::Global();
    ptlpp = TlppVerify::Global();
  }

  int Run(vta_phy_addr_t insn_phy_addr,
          uint32_t insn_count,
          uint32_t wait_cycles) {
    VTAGenericInsn* insn = static_cast<VTAGenericInsn*>(
        dram_->GetAddr(insn_phy_addr));
    insn_start_ = insn;
    finish_counter_ = 0;
    trace_mgr.Flush();
    trace_mgr.Event("RUN", "%8u instructions\n", insn_count);
    // Register events here.
    trace_mgr.Header("EXE", "%4s %32s\n", "OP", "INSTRUCTION");
    trace_mgr.Header("ALU_ITR", "%4s %4s %4s %3s %-8s\n",
                      "ITRO", "ITRI", "UOPI", "OFS", "ACC_WR");
    trace_mgr.Header("GEM_ITR", "%4s %4s %4s %3s %-8s\n",
                      "ITRO", "ITRI", "UOPI", "OFS", "ACC_WR");
    for (uint32_t i = 0; i < insn_count; ++i) {
      this->Run(insn + i);
    }

    uint64_t acc_load_nbytes  = prof_->acc_load_nbytes;
    uint64_t inp_load_nbytes  = prof_->inp_load_nbytes;
    uint64_t wgt_load_nbytes  = prof_->wgt_load_nbytes;
    uint64_t uop_load_nbytes  = prof_->uop_load_nbytes;
    uint64_t out_store_nbytes = prof_->out_store_nbytes;
    uint64_t alu_counter      = prof_->alu_counter;
    uint64_t gemm_counter     = prof_->gemm_counter;

    this->TlppSynchronization();

    // Update profiler with cycle estimation.
    prof_->cycle_counter   += ptlpp->core_time_cycles_[2];
    prof_->idle_cp_counter += ptlpp->core_idle_cycles_[0];
    prof_->idle_ld_counter += ptlpp->core_idle_cycles_[1];
    prof_->idle_st_counter += ptlpp->core_idle_cycles_[2];

    // Report per-run stats.
    trace_mgr.Event("FINISH", "  ACC_LD_BYTES: %d\n", prof_->acc_load_nbytes - acc_load_nbytes);
    trace_mgr.Event("FINISH", "  INP_LD_BYTES: %d\n", prof_->inp_load_nbytes - inp_load_nbytes);
    trace_mgr.Event("FINISH", "  WGT_LD_BYTES: %d\n", prof_->wgt_load_nbytes - wgt_load_nbytes);
    trace_mgr.Event("FINISH", "  UOP_LD_BYTES: %d\n", prof_->uop_load_nbytes - uop_load_nbytes);
    trace_mgr.Event("FINISH", "  OUT_ST_BYTES: %d\n", prof_->out_store_nbytes - out_store_nbytes);
    trace_mgr.Event("FINISH", "  ALU_LP_COUNT: %d\n", prof_->alu_counter - alu_counter);
    trace_mgr.Event("FINISH", "  GEM_LP_COUNT: %d\n", prof_->gemm_counter - gemm_counter);
    trace_mgr.Event("FINISH", "   CORE_CYCLES: %d %d %d\n", ptlpp->core_time_cycles_[0]);
    trace_mgr.Event("FINISH", "IDLE_LD_CYCLES: %d\n", ptlpp->core_idle_cycles_[1]);
    trace_mgr.Event("FINISH", "IDLE_ST_CYCLES: %d\n", ptlpp->core_idle_cycles_[2]);
    trace_mgr.Event("FINISH", "IDLE_CP_CYCLES: %d\n", ptlpp->core_idle_cycles_[0]);
    trace_mgr.Flush();
    ptlpp->ClearCycle();
    return 0;
  }

 private:
  static uint32_t Run_Insn(const VTAGenericInsn* insn, void * dev) {
    Device * device = reinterpret_cast<Device *> (dev);
    trace_mgr.Event("INSN_IDX", "%4x\n", insn - device->insn_start_);
    const VTAMemInsn* mem = reinterpret_cast<const VTAMemInsn*>(insn);
    const VTAGemInsn* gem = reinterpret_cast<const VTAGemInsn*>(insn);
    const VTAAluInsn* alu = reinterpret_cast<const VTAAluInsn*>(insn);
    uint32_t cycles = 4;
    switch (mem->opcode) {
      case VTA_OPCODE_LOAD:  cycles += device->RunLoad(mem); break;
      case VTA_OPCODE_STORE: cycles += device->RunStore(mem); break;
      case VTA_OPCODE_GEMM:  cycles += device->RunGEMM(gem); break;
      case VTA_OPCODE_ALU:   cycles += device->RunALU(alu); break;
      case VTA_OPCODE_FINISH: 
        ++(device->finish_counter_);
        trace_mgr.Event("EXE", "FIN  %016" PRIx64 "%016" PRIx64 "\n",
        *((uint64_t*)insn+1), *((uint64_t*)insn));
        cycles += 4;
        trace_mgr.Event("RET", "FIN  %016" PRIx64 "%016" PRIx64 "\n",
        *((uint64_t*)insn+1), *((uint64_t*)insn));
      break;
      default: {
        LOG(FATAL) << "Unknown op_code" << mem->opcode;
      }
    }
    return cycles;
  }

 private:
  // This is where instructions are fetched and loaded into corresponding queues.
  void Run(const VTAGenericInsn* insn) {
    ptlpp->TlppPushInsn(insn);
  }

  // This is where instructions are randomly loaded from the queues in a round
  // robin fashion and dispatched according to depencencies.
  void TlppSynchronization(void) {
    ptlpp->TlppSynchronization(Run_Insn, reinterpret_cast<void *> (this));
  }

  uint32_t RunLoad(const VTAMemInsn* op) {
    uint32_t cycles = 0;
    if (op->x_size == 0) {
      if (op->memory_type == VTA_MEM_ID_ACC || op->memory_type == VTA_MEM_ID_UOP)
        trace_mgr.Event("EXE", "CNOP %016" PRIx64 "%016" PRIx64 "\n",
        *((uint64_t*)op+1), *((uint64_t*)op));
      else
        trace_mgr.Event("EXE", "LNOP %016" PRIx64 "%016" PRIx64 "\n",
        *((uint64_t*)op+1), *((uint64_t*)op));
      cycles = 4;
      if (op->memory_type == VTA_MEM_ID_ACC || op->memory_type == VTA_MEM_ID_UOP)
        trace_mgr.Event("RET", "CNOP %016" PRIx64 "%016" PRIx64 "\n",
        *((uint64_t*)op+1), *((uint64_t*)op));
      else
        trace_mgr.Event("RET", "LNOP %016" PRIx64 "%016" PRIx64 "\n",
        *((uint64_t*)op+1), *((uint64_t*)op));
    } else if (op->memory_type == VTA_MEM_ID_INP) {
      trace_mgr.Event("EXE", "LINP %016" PRIx64 "%016" PRIx64 "\n",
      *((uint64_t*)op+1), *((uint64_t*)op));
      cycles = inp_.Load(op, dram_, &(prof_->inp_load_nbytes), prof_->SkipExec());
      trace_mgr.Event("RET", "LINP %016" PRIx64 "%016" PRIx64 "\n",
      *((uint64_t*)op+1), *((uint64_t*)op));
    } else if (op->memory_type == VTA_MEM_ID_WGT) {
      trace_mgr.Event("EXE", "LWGT %016" PRIx64 "%016" PRIx64 "\n",
      *((uint64_t*)op+1), *((uint64_t*)op));
      cycles = wgt_.Load(op, dram_, &(prof_->wgt_load_nbytes), prof_->SkipExec());
      trace_mgr.Event("RET", "LWGT %016" PRIx64 "%016" PRIx64 "\n",
      *((uint64_t*)op+1), *((uint64_t*)op));
    } else if (op->memory_type == VTA_MEM_ID_ACC) {
      trace_mgr.Event("EXE", "LACC %016" PRIx64 "%016" PRIx64 "\n",
      *((uint64_t*)op+1), *((uint64_t*)op));
      cycles = acc_.Load(op, dram_, &(prof_->acc_load_nbytes), prof_->SkipExec());
      trace_mgr.Event("RET", "LACC %016" PRIx64 "%016" PRIx64 "\n",
      *((uint64_t*)op+1), *((uint64_t*)op));
    } else if (op->memory_type == VTA_MEM_ID_UOP) {
      // always load in uop, since uop is stateful
      // subsequent non-debug mode exec can depend on it.
      trace_mgr.Event("EXE", "LUOP %016" PRIx64 "%016" PRIx64 "\n",
      *((uint64_t*)op+1), *((uint64_t*)op));
      cycles = uop_.Load(op, dram_, &(prof_->uop_load_nbytes), false);
      trace_mgr.Event("RET", "LUOP %016" PRIx64 "%016" PRIx64 "\n",
      *((uint64_t*)op+1), *((uint64_t*)op));
    } else {
      LOG(FATAL) << "Unknown memory_type=" << op->memory_type;
    }
    return cycles;
  }

  uint32_t RunStore(const VTAMemInsn* op) {
    uint32_t cycles = 0;
    uint64_t dram_data_bytes = 8, transfer_pulses = 16, transfer_cycles = 4;
    if (op->x_size == 0) {
      trace_mgr.Event("EXE", "SNOP %016" PRIx64 "%016" PRIx64 "\n",
      *((uint64_t*)op+1), *((uint64_t*)op));
      cycles = 4;
      trace_mgr.Event("RET", "SNOP %016" PRIx64 "%016" PRIx64 "\n",
      *((uint64_t*)op+1), *((uint64_t*)op));
    } else if (op->memory_type == VTA_MEM_ID_ACC ||
               op->memory_type == VTA_MEM_ID_UOP) {
      uint32_t store_bytes = op->x_size * op->y_size * VTA_BATCH * VTA_BLOCK_OUT * VTA_OUT_WIDTH / 8;
      uint64_t transfer_waste_bytes = store_bytes % dram_data_bytes;
      if (transfer_waste_bytes)
        store_bytes += dram_data_bytes - transfer_waste_bytes;
      prof_->out_store_nbytes += store_bytes;
      uint64_t pulses = store_bytes / dram_data_bytes;
      uint64_t transfers = (pulses + transfer_pulses - 1) / transfer_pulses;
      cycles += transfer_cycles * transfers + pulses;
      if (!prof_->SkipExec()) {
        trace_mgr.Event("EXE", "SOUT %016" PRIx64 "%016" PRIx64 "\n",
        *((uint64_t*)op+1), *((uint64_t*)op));
        acc_.TruncStore<VTA_OUT_WIDTH>(op, dram_);
        trace_mgr.Event("RET", "SOUT %016" PRIx64 "%016" PRIx64 "\n",
        *((uint64_t*)op+1), *((uint64_t*)op));
      }
    } else {
      LOG(FATAL) << "Store do not support memory_type="
                 << op->memory_type;
    }
    return cycles;
  }

  uint32_t RunGEMM(const VTAGemInsn* op) {
    uint32_t cycles = 1;
#ifdef TRACE_FOR_CHISEL_TESTER
    std::cout << std::hex << std::setfill( '0') << std::setw( 1) << "Inst: reset: " << op->reset_reg 
	      << " uop_{begin,end}: "
	      << std::hex << std::setfill( '0') << std::setw( 4) << op->uop_bgn << " "
	      << std::hex << std::setfill( '0') << std::setw( 4) << op->uop_end
	      << " lp_{0,1}: " << std::hex << std::setfill( '0') << std::setw( 4) << op->iter_out << " " << std::hex << std::setfill( '0') << std::setw( 4) << op->iter_in
	      << " {acc,inp,wgt}_{0,1}:"
	      << " " << std::hex << std::setfill( '0') << std::setw( 3) << op->dst_factor_out << " " << std::hex << std::setfill( '0') << std::setw( 3) << op->dst_factor_in
	      << " " << std::hex << std::setfill( '0') << std::setw( 3) << op->src_factor_out << " " << std::hex << std::setfill( '0') << std::setw( 3) << op->src_factor_in
	      << " " << std::hex << std::setfill( '0') << std::setw( 3) << op->wgt_factor_out << " " << std::hex << std::setfill( '0') << std::setw( 3) << op->wgt_factor_in
	      << std::dec << std::endl;

    for ( uint32_t idx = 0; idx < VTA_INP_BUFF_DEPTH; ++idx) {
      std::cout << "inp: " << std::hex << std::setfill( '0') << std::setw( 8) << idx;
      BitPacker<VTA_INP_WIDTH> inp(inp_.BeginPtr(idx));
   
      for ( uint32_t j = 0; j < VTA_BATCH * VTA_BLOCK_IN; ++j) {
	std::cout << " " << std::hex << std::setfill( '0') << std::setw( 2) << inp.GetUnsigned(j);
      }
      std::cout << std::endl;
    }

    for ( uint32_t idx = 0; idx < VTA_WGT_BUFF_DEPTH; ++idx) {
      std::cout << "wgt: " << std::hex << std::setfill( '0') << std::setw( 8) << idx;
      BitPacker<VTA_WGT_WIDTH> wgt(wgt_.BeginPtr(idx));
   
      for ( uint32_t j = 0; j < VTA_BLOCK_IN * VTA_BLOCK_OUT; ++j) {
	std::cout << " " << std::hex << std::setfill( '0') << std::setw( 2) << wgt.GetUnsigned(j);
      }
      std::cout << std::endl;
    }

    for ( uint32_t idx = 0; idx < VTA_ACC_BUFF_DEPTH; ++idx) {
      std::cout << "acc_i: " << std::hex << std::setfill( '0') << std::setw( 8) << idx;
      BitPacker<VTA_ACC_WIDTH> acc(acc_.BeginPtr(idx));
   
      for ( uint32_t j = 0; j < VTA_BATCH * VTA_BLOCK_OUT; ++j) {
	std::cout << " " << std::hex << std::setfill( '0') << std::setw( 8) << acc.GetUnsigned(j);
      }
      std::cout << std::endl;
    }

    for ( uint32_t idx = 0; idx < VTA_UOP_BUFF_DEPTH; ++idx) {
      std::cout << "uop: " << std::hex << std::setfill( '0') << std::setw( 8) << idx;
      VTAUop* uop_ptr = static_cast<VTAUop*>(uop_.BeginPtr(idx));

      std::cout << " " << std::hex << std::setfill( '0') << std::setw( 8) << uop_ptr->dst_idx;
      std::cout << " " << std::hex << std::setfill( '0') << std::setw( 8) << uop_ptr->src_idx;
      std::cout << " " << std::hex << std::setfill( '0') << std::setw( 8) << uop_ptr->wgt_idx;

      std::cout << std::endl;
    }
#endif

    trace_mgr.Event("EXE", "GEM  %016" PRIx64 "%016" PRIx64 "\n",
    *((uint64_t*)op+1), *((uint64_t*)op));
    trace_mgr.Event("GEM_LOOP", "%04x %04x %04x %04x\n",
        op->iter_out, op->iter_in, op->uop_bgn, op->uop_end);
    if (prof_->SkipExec())
    {
      prof_->gemm_counter += op->iter_out * op->iter_in * (op->uop_end - op->uop_bgn);
      return cycles;
    }
    cycles += 5;
    if (!op->reset_reg) {
      for (uint32_t y = 0; y < op->iter_out; ++y) {
        for (uint32_t x = 0; x < op->iter_in; ++x) {
          for (uint32_t uindex = op->uop_bgn; uindex < op->uop_end; ++uindex) {
            VTAUop* uop_ptr = static_cast<VTAUop*>(uop_.BeginPtr(uindex));
            // Read in memory indices
            uint32_t acc_idx = uop_ptr->gem.dst_idx;
            uint32_t inp_idx = uop_ptr->gem.src_idx;
            uint32_t wgt_idx = uop_ptr->gem.wgt_idx;

            acc_idx += y * op->dst_factor_out + x * op->dst_factor_in;
            inp_idx += y * op->src_factor_out + x * op->src_factor_in;
            wgt_idx += y * op->wgt_factor_out + x * op->wgt_factor_in;
            BitPacker<VTA_ACC_WIDTH> acc(acc_.BeginPtr(acc_idx));
            BitPacker<VTA_INP_WIDTH> inp(inp_.BeginPtr(inp_idx));
            BitPacker<VTA_WGT_WIDTH> wgt(wgt_.BeginPtr(wgt_idx));

            // gemm loop
            for (uint32_t i = 0; i < VTA_BATCH; ++i) {
              // Count of block size acc writes.
              trace_mgr.Event("ACC_BWR", "%08x\n", prof_->acc_wr_counter);
              uint32_t blk_offset = i * VTA_BLOCK_OUT;
              trace_mgr.Event("GEM_ITR", "%04x %04x %04x %03x",
                              y, x, uindex, acc_idx + blk_offset);
              for (uint32_t j = 0; j < VTA_BLOCK_OUT; ++j) {
                uint32_t acc_offset = blk_offset + j;
                int32_t sum = acc.GetSigned(acc_offset);
                for (uint32_t k = 0; k < VTA_BLOCK_IN; ++k) {
                  sum +=
                      inp.GetSigned(i * VTA_BLOCK_IN + k) *
                      wgt.GetSigned(j * VTA_BLOCK_IN + k);
                }
                trace_mgr.Event("+GEM_ITR", " %08x", sum);
                acc.SetSigned(acc_offset, sum);
#ifdef DIVERGENCE_VERIFICATION
                // The divergence point is located at the address in the
                // accumulator that corresponds to the address in the store
                // scratchpad of the first byte that does not match. 
                // Because the accumulator width is 4 bytes we need to 
                // multiply by 4 to get to the byte address.
                // In the if statement replace the values of the divergence
                // point address and value as taken from the trace.
                // You can also print other invariant information such as
                // the instruction address, content, etc.
                uint32_t dp_addr = (acc_idx + acc_offset) * 4;
                uint32_t dp_data = sum & 0xff;
                if (dp_addr == dp_trace_addr && dp_data == dp_trace_data) {
                  printf("DIVERGENCE_POINT@GEMM: addr: 0x%04x, data: 0x%2x, cnt: %u\n",
                         dp_addr, dp_data, prof_->acc_wr_counter);
                  fflush(stdout);
                }
#endif // DIVERGENCE_VERIFICATION
              }
              trace_mgr.Event("+GEM_ITR", "\n");
              // End of event GEM_ITR.
              ++prof_->acc_wr_counter;
              cycles++;
            }
            ++prof_->gemm_counter;
          }
        }
      }
    } else {
      // reset
      for (uint32_t y = 0; y < op->iter_out; ++y) {
        for (uint32_t x = 0; x < op->iter_in; ++x) {
          for (uint32_t uindex = op->uop_bgn; uindex < op->uop_end; ++uindex) {
            VTAUop* uop_ptr = static_cast<VTAUop*>(uop_.BeginPtr(uindex));
            uint32_t acc_idx = uop_ptr->gem.dst_idx;
            acc_idx += y * op->dst_factor_out + x * op->dst_factor_in;
            BitPacker<VTA_ACC_WIDTH> acc(acc_.BeginPtr(acc_idx));
            for (uint32_t i = 0; i < VTA_BATCH; ++i) {
              trace_mgr.Event("ACC_BWR", "%08x\n", prof_->acc_wr_counter);
              int32_t blk_offset = i * VTA_BLOCK_OUT;
              trace_mgr.Event("GEM_ITR", "%04x %04x %04x %03x",
                                 y, x, uindex, acc_idx + blk_offset);
              for (uint32_t j = 0; j < VTA_BLOCK_OUT; ++j) {
                uint32_t acc_offset = blk_offset + j;
                acc.SetSigned(acc_offset, 0);
                trace_mgr.Event("+GEM_ITR", " %08x", acc.GetSigned(acc_offset));
              }
              trace_mgr.Event("+GEM_ITR", "\n");
              ++prof_->acc_wr_counter;
              cycles++;
            }
            ++prof_->gemm_counter;
          }
        }
      }
    }

#ifdef TRACE_FOR_CHISEL_TESTER
    for ( uint32_t idx = 0; idx < VTA_ACC_BUFF_DEPTH; ++idx) {
      std::cout << "acc_o: " << std::hex << std::setfill( '0') << std::setw( 8) << idx;
      BitPacker<VTA_ACC_WIDTH> acc(acc_.BeginPtr(idx));
   
      for ( uint32_t j = 0; j < VTA_BATCH * VTA_BLOCK_OUT; ++j) {
	std::cout << " " << std::hex << std::setfill( '0') << std::setw( 8) << acc.GetUnsigned(j);
      }
      std::cout << std::endl;
    }
#endif

    trace_mgr.Event("RET", "GEM  %016" PRIx64 "%016" PRIx64 "\n",
    *((uint64_t*)op+1), *((uint64_t*)op));

    return cycles;
  }

  uint32_t RunALU(const VTAAluInsn* op) {
    if (op->use_imm)
      return RunALU_<true>(op);
    return RunALU_<false>(op);
  }

  template<bool use_imm>
  uint32_t RunALU_(const VTAAluInsn* op) {
    switch (op->alu_opcode) {
      case VTA_ALU_OPCODE_ADD: {
        trace_mgr.Event("EXE", "%4s %016" PRIx64 "%016" PRIx64 "\n", (use_imm ? "ADDI" : "ADD"),
        *((uint64_t*)op+1), *((uint64_t*)op));
        return RunALULoop<use_imm>(op, [](int32_t x, int32_t y) {
            return x + y;
          });
      }
      case VTA_ALU_OPCODE_MAX: {
        trace_mgr.Event("EXE", "%4s %016" PRIx64 "%016" PRIx64 "\n", (use_imm ? "MAXI" : "MAX"),
        *((uint64_t*)op+1), *((uint64_t*)op));
        return RunALULoop<use_imm>(op, [](int32_t x, int32_t y) {
            return std::max(x, y);
          });
      }
      case VTA_ALU_OPCODE_MIN: {
        trace_mgr.Event("EXE", "%4s %016" PRIx64 "%016" PRIx64 "\n", (use_imm ? "MINI" : "MIN"),
        *((uint64_t*)op+1), *((uint64_t*)op));
        return RunALULoop<use_imm>(op, [](int32_t x, int32_t y) {
            return std::min(x, y);
          });
      }
      case VTA_ALU_OPCODE_SHR: {
        if (op->imm >= 0) {
          trace_mgr.Event("EXE", "%4s %016" PRIx64 "%016" PRIx64 "\n", (use_imm ? "SHRI" : "SHR"),
          *((uint64_t*)op+1), *((uint64_t*)op));
        } else {
          trace_mgr.Event("EXE", "%4s %016" PRIx64 "%016" PRIx64 "\n", (use_imm ? "SHLI" : "SHL"),
          *((uint64_t*)op+1), *((uint64_t*)op));
        }
        return RunALULoop<use_imm>(op, [](int32_t x, int32_t y) {
            if (y >= 0) {
              return x >> y;
            } else {
              return x << (-y);
            }
          });
      }
      case VTA_ALU_OPCODE_CLP: {
        trace_mgr.Event("EXE", "%4s %016" PRIx64 "%016" PRIx64 "\n", (use_imm ? "CLPI" : "CLP"),
        *((uint64_t*)op+1), *((uint64_t*)op));
        return RunALULoop<use_imm>(op, [](int32_t x, int32_t y) {
            return std::max(std::min(x, y), -1 * y);
          });
      }
      case VTA_ALU_OPCODE_MOV: {
        trace_mgr.Event("EXE", "%4s %016" PRIx64 "%016" PRIx64 "\n", (use_imm ? "MOVI" : "MOV"),
        *((uint64_t*)op+1), *((uint64_t*)op));
        return RunALULoop<use_imm>(op, [](int32_t x, int32_t y) {
            return y;
          });
      }
      case VTA_ALU_OPCODE_MUL: {
        trace_mgr.Event("EXE", "%4s %016" PRIx64 "%016" PRIx64 "\n", (use_imm ? "MULI" : "MUL"),
        *((uint64_t*)op+1), *((uint64_t*)op));
        return RunALULoop<use_imm>(op, [](int32_t x, int32_t y) {
            return reinterpret_cast<int8_t &>(x) * reinterpret_cast<int8_t &>(y);
          });
      }
      default: {
        LOG(FATAL) << "Unknown ALU code " << op->alu_opcode;
      }
    }
    return 0;
  }

  template<bool use_imm, typename F>
  uint32_t RunALULoop(const VTAAluInsn* op, F func) {
    // No pipeline here.
    uint32_t cycles = 1;
    if (prof_->SkipExec()) {
      prof_->alu_counter += op->iter_out * op->iter_in * (op->uop_end - op->uop_bgn);
      return cycles;
    }
    trace_mgr.Event("ALU_LOOP", "%04x %04x %04x %04x\n",
        op->iter_out, op->iter_in, op->uop_bgn, op->uop_end);
    for (int y = 0; y < op->iter_out; ++y) {
      for (int x = 0; x < op->iter_in; ++x) {
        for (int u = op->uop_bgn; u < op->uop_end; ++u) {
          // Read micro op
          VTAUop* uop_ptr = static_cast<VTAUop*>(uop_.BeginPtr(u));
          uint32_t dst_index = uop_ptr->alu.dst_idx;
          uint32_t src_index = uop_ptr->alu.src_idx;
          dst_index += y * op->dst_factor_out + x * op->dst_factor_in;
          src_index += y * op->src_factor_out + x * op->src_factor_in;
          BitPacker<VTA_ACC_WIDTH> dst(acc_.BeginPtr(dst_index));
          BitPacker<VTA_ACC_WIDTH> src(acc_.BeginPtr(src_index));
          for (uint32_t i = 0; i < VTA_BATCH; ++i) {
            trace_mgr.Event("ACC_BWR", "%08x\n", prof_->acc_wr_counter);
            uint32_t blk_offset = i * VTA_BLOCK_OUT;
            trace_mgr.Event("ALU_ITR", "%04x %04x %04x %03x",
                                y, x, u, dst_index + blk_offset);
            for (uint32_t j = 0; j < VTA_BLOCK_OUT; ++j) {
              uint32_t k = blk_offset + j;
              if (use_imm) {
                dst.SetSigned(k, func(src.GetSigned(k), op->imm));
              } else {
                dst.SetSigned(k, func(dst.GetSigned(k), src.GetSigned(k)));
              }
              trace_mgr.Event("+ALU_ITR", " %08x", dst.GetSigned(k));
#ifdef DIVERGENCE_VERIFICATION
              uint32_t dp_addr = (dst_index + k) * 4;
              uint32_t dp_data = dst.GetSigned(k) & 0xff;
              if (dp_addr == dp_trace_addr && dp_data == dp_trace_data)
              {
                printf("DIVERGENCE_POINT@ALU: addr: 0x%04x, data: 0x%2x, cnt: %u\n",
                dp_addr, dp_data, prof_->acc_wr_counter);
                fflush(stdout);
              }
#endif // DIVERGENCE_VERIFICATION
            }
            trace_mgr.Event("+ALU_ITR", "\n");
            // End of ALU_ITR event.
            ++prof_->acc_wr_counter;
            // 5 cycles per batch: todo from json.
            cycles += 5;
          }
          ++prof_->alu_counter;
        }
      }
    }
    switch (op->alu_opcode) {
      case VTA_ALU_OPCODE_ADD:
        trace_mgr.Event("RET", "%4s %016" PRIx64 "%016" PRIx64 "\n", (use_imm ? "ADDI" : "ADD"),
        *((uint64_t*)op+1), *((uint64_t*)op));
      break;
      case VTA_ALU_OPCODE_MAX:
        trace_mgr.Event("RET", "%4s %016" PRIx64 "%016" PRIx64 "\n", (use_imm ? "MAXI" : "MAX"),
        *((uint64_t*)op+1), *((uint64_t*)op));
      break;
      case VTA_ALU_OPCODE_MIN:
        trace_mgr.Event("RET", "%4s %016" PRIx64 "%016" PRIx64 "\n", (use_imm ? "MINI" : "MIN"),
        *((uint64_t*)op+1), *((uint64_t*)op));
      break;
      case VTA_ALU_OPCODE_SHR:
        if (op->imm >= 0) {
          trace_mgr.Event("RET", "%4s %016" PRIx64 "%016" PRIx64 "\n", (use_imm ? "SHRI" : "SHR"),
          *((uint64_t*)op+1), *((uint64_t*)op));
        } else {
          trace_mgr.Event("RET", "%4s %016" PRIx64 "%016" PRIx64 "\n", (use_imm ? "SHLI" : "SHL"),
          *((uint64_t*)op+1), *((uint64_t*)op));
        }
      break;
      case VTA_ALU_OPCODE_CLP:
        trace_mgr.Event("RET", "%4s %016" PRIx64 "%016" PRIx64 "\n", (use_imm ? "CLPI" : "CLP"),
        *((uint64_t*)op+1), *((uint64_t*)op));
      break;
      case VTA_ALU_OPCODE_MOV:
        trace_mgr.Event("RET", "%4s %016" PRIx64 "%016" PRIx64 "\n", (use_imm ? "MOVI" : "MOV"),
        *((uint64_t*)op+1), *((uint64_t*)op));
      case VTA_ALU_OPCODE_MUL:
        trace_mgr.Event("RET", "%4s %016" PRIx64 "%016" PRIx64 "\n", (use_imm ? "MULI" : "MUL"),
        *((uint64_t*)op+1), *((uint64_t*)op));
      break;
      default:
      break;
    }
    return cycles;
  }
  // the finish counter
  int finish_counter_{0};
  // Prof_
  Profiler* prof_;
  // The DRAM interface
  DRAM* dram_;
  TlppVerify* ptlpp;
  VTAGenericInsn *insn_start_;
  // The SRAM
  SRAM<VTA_INP_WIDTH, VTA_BATCH * VTA_BLOCK_IN, VTA_INP_BUFF_DEPTH> inp_;
  SRAM<VTA_WGT_WIDTH, VTA_BLOCK_IN * VTA_BLOCK_OUT, VTA_WGT_BUFF_DEPTH> wgt_;
  SRAM<VTA_ACC_WIDTH, VTA_BATCH * VTA_BLOCK_OUT, VTA_ACC_BUFF_DEPTH> acc_;
  SRAM<VTA_UOP_WIDTH, 1, VTA_UOP_BUFF_DEPTH> uop_;
};

using tvm::runtime::TVMRetValue;
using tvm::runtime::TVMArgs;

TVM_REGISTER_GLOBAL("vta.simulator.profiler_clear")
.set_body([](TVMArgs args, TVMRetValue* rv) {
    Profiler::ThreadLocal()->Clear();
  });
TVM_REGISTER_GLOBAL("vta.simulator.profiler_status")
.set_body([](TVMArgs args, TVMRetValue* rv) {
    *rv = Profiler::ThreadLocal()->AsJSON();
  });
TVM_REGISTER_GLOBAL("vta.simulator.profiler_debug_mode")
.set_body([](TVMArgs args, TVMRetValue* rv) {
    Profiler::ThreadLocal()->debug_flag = args[0];
  });
}  // namespace sim
}  // namespace vta

void* VTAMemAlloc(size_t size, int cached) {
  return vta::sim::DRAM::Global()->Alloc(size);
}

void VTAMemFree(void* buf) {
  vta::sim::DRAM::Global()->Free(buf);
}

vta_phy_addr_t VTAMemGetPhyAddr(void* buf) {
  return vta::sim::DRAM::Global()->GetPhyAddr(buf);
}

void VTAMemCopyFromHost(void* dst, const void* src, size_t size) {
  memcpy(dst, src, size);
}

void VTAMemCopyToHost(void* dst, const void* src, size_t size) {
  memcpy(dst, src, size);
}

void VTAFlushCache(void* vir_addr, vta_phy_addr_t phy_addr, int size) {
}

void VTAInvalidateCache(void* vir_addr, vta_phy_addr_t phy_addr, int size) {
}

VTADeviceHandle VTADeviceAlloc() {
  return new vta::sim::Device();
}

void VTADeviceFree(VTADeviceHandle handle) {
  delete static_cast<vta::sim::Device*>(handle);
}

int VTADeviceRun(VTADeviceHandle handle,
                 vta_phy_addr_t insn_phy_addr,
                 uint32_t insn_count,
                 uint32_t wait_cycles) {
  return static_cast<vta::sim::Device*>(handle)->Run(
      insn_phy_addr, insn_count, wait_cycles);
}

void VTAProgram(const char* bitstream) {
}
