#!/usr/bin/env python3

"""
Extract the binary components from the original t2k rom
"""

import sys
import os
import re

sf = open("vlmdisassemble/vlm.sym", 'rb')
bs = sf.read()

syms = {}
s = 0x326
ws = ["sub_", "loc_", "$", "word_", "byte_", "dword_", "asc_"]
while s < len(bs):
    n = bs[s:s+8].rstrip(b'\x00').decode("utf-8")
    s += 10
    a = (bs[s:s+4]).hex().lstrip('0').upper()
    s += 4
    if not a:
        continue
    syms[n] = a


lf = open("../errs.txt", 'r')
f = open("vlm.inc", 'w')
while True:
    l = lf.readline().strip()
    if not l:
        break
    
    print(l + " EQU $" + syms[l] if l in syms else l, file=f)

