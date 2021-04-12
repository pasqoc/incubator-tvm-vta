#!/usr/bin/env/python3

# Modified by contributors from Intel Labs

"""
Design Space Explorer
"""

import itertools
import numpy as numpy
import vta

class DSExplorer():

    def __init__(self, batch=0, 
                 blockin=4, blockout=4,
                 inp_buf=15, wgt_buf=18,
                 acc_buf=17, mac=256,
                 max_batch=4, max_block=6,
                 max_buf=18, min_buf=15):
        self.batch = batch
        self.blockin, self.blockout = blockin, blockout
        self.inp_buf = inp_buf
        self.wgt_buf = wgt_buf
        self.acc_buf = acc_buf
        self.mac = mac
        self.max_batch = max_batch
        self.max_block = max_block
        self.max_buf = max_buf
        self.min_buf = min_buf
        self.inp_width = 3
        self.wgt_width = 3
        self.acc_width = 5
        self.uop_buf = 15
        self.def_batch = 0
        self.def_blockin = 4
        self.def_blockout = 4
        self.def_inp_buf = 15
        self.def_wgt_buf = 18
        self.def_acc_buf = 17 
        self.combinations = None
        self.create_combinations()
        self.print_combinations()

    def is_valid(self, candidate):
        ba, blin, blout, ib, wb, ab = candidate
        minb = self.min_buf
        if self.batch != self.def_batch:
            if ba != self.batch: return False
        if self.blockin != self.def_blockin:
            if blin != self.blockin: return False
        if self.blockout != self.def_blockout:
            if blout != self.blockout: return False
        if self.inp_buf != self.def_inp_buf:
            if ib != self.inp_buf: return False
        if self.wgt_buf != self.def_wgt_buf:
            if wb != self.wgt_buf: return False
        if self.acc_buf != self.def_acc_buf:
            if ab != self.acc_buf: return False
        if ib < minb or wb < minb or ab < minb:
            return False
        if 1 << (ba + blin + blout) != self.mac:
            return False
        iw = self.inp_width
        ww = self.wgt_width
        aw = self.acc_width
        inp_depth = ib - ba - blin - iw + 3
        if inp_depth <= 0:
            return False
        wgt_depth = wb - blout - blin - ww + 3
        if wgt_depth <= 0:
            return False
        acc_depth = ab - ba - blout - aw + 3
        if acc_depth <= 0:
            return False
        if inp_depth + wgt_depth + acc_depth > 32:
            return False
        return True

    def create_combinations(self):
        max_batch, max_block = self.max_batch, self.max_block
        max_buf = self.max_buf
        limits = [max_batch, max_block, max_block, max_buf, max_buf, max_buf]
        candidates = list(itertools.product(*(range(x+1) for x in limits)))
        valid = []
        for candidate in candidates:
            if self.is_valid(candidate):
                valid.append(candidate)
        self.combinations = valid
    
    def print_combinations(self):
        for combination in self.combinations:
            print(f'{1<<combination[0]}x{1<<combination[1]}x{1<<combination[2]}'
                  f'_{self.uop_buf}_{combination[3]}'
                  f'_{combination[4]}_{combination[5]}')
        print(f'{len(self.combinations)} configurations found.')
    

if __name__ == '__main__':
    import sys
    from argparse import ArgumentParser
    if sys.argv[0].endswith('dsexplorer.py'):
        ap = ArgumentParser(description = 'Design Space Explorer - create valid VTA configs')
        ap.add_argument('-ba', '--batch', type=int, default=0,
          help='LOG_BATCH (default: %(default)d)')
        ap.add_argument('-blin', '--blockin', type=int, default=4,
          help='LOG_BLOCK_IN (default: %(default)d)')
        ap.add_argument('-blout', '--blockout', type=int, default=4,
          help='LOG_BLOCK_OUT (default: %(default)d)')
        ap.add_argument('-ib', '--inp-buf', type=int, default=15,
          help='LOG_INP_BUFF_SIZE (default: %(default)d)')
        ap.add_argument('-wb', '--wgt-buf', type=int, default=18,
          help='LOG_WGT_BUFF_SIZE (default: %(default)d)')
        ap.add_argument('-ab', '--acc-buf', type=int, default=17,
          help='LOG_ACC_BUFF_SIZE (default: %(default)d)')
        ap.add_argument('-m', '--mac', type=int, default=256,
          help='Number of MAC units (default: %(default)d)')
        ap.add_argument('-mba', '--max-batch', type=int, default=4,
          help='Maximum LOG_BATCH (default: %(default)d)')
        ap.add_argument('-mbl', '--max-block', type=int, default=6,
          help='Maximum LOG_BLOCK (default: %(default)d)')
        ap.add_argument('-maxb', '--max-buf', type=int, default=18,
          help='Maximum buffer size (default: %(default)d)')
        ap.add_argument('-minb', '--min-buf', type=int, default=15,
          help='Minimum buffer size (default: %(default)d)')
        args = ap.parse_args()
        DSExplorer(batch=args.batch, blockin=args.blockin, blockout=args.blockout,
                    inp_buf=args.inp_buf, wgt_buf=args.wgt_buf, acc_buf=args.acc_buf,
                    mac=args.mac, max_batch=args.max_batch, max_block=args.max_block, 
                    max_buf=args.max_buf, min_buf=args.min_buf)
