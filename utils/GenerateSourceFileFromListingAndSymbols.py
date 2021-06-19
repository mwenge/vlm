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
    syms[a] = n
    for w in ws: syms[w + a] = n 

def addSymbols(l):
    l1 = re.split("([\n :,\(\)])",l)
    # Ignore any symbols that occur after a dc.b statement
    idc = l1.index("dc.b") if "dc.b" in l1 else 9999
    o = [syms[c] if c in syms and idc > i else c for i,c in enumerate(l1)]
    return ''.join(o)

lf = open("vlmdisassemble/vlm.txt.asm", 'r')
f = open("vlmdisassemble/vlm.s", 'w')
while True:
    l = lf.readline()
    if not l:
        break
    print(addSymbols(l).strip(), file=f)

