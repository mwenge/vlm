#!/usr/bin/env python3

"""
Extract the binary components from the original t2k rom
"""

import sys
import os
from itertools import chain

sf = open("../src/objs/vlm.sym", 'rb')
bs = sf.read()

f = open("vlm-syms.txt", 'w')
s = 0x326
while s < len(bs):
    n = bs[s:s+8].rstrip(b'\x00').decode("utf-8")
    s += 10
    a = (bs[s:s+4]).hex().lstrip('0').upper()
    s += 4
    print(n, a, file=f, sep='\t')

