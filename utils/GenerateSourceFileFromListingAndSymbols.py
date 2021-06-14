#!/usr/bin/env python3

"""
Extract the binary components from the original t2k rom
"""

import sys
import os
import re

sf = open("../src/objs/vlm.sym", 'rb')
bs = sf.read()

syms = {}
s = 0x326
while s < len(bs):
    n = bs[s:s+8].rstrip(b'\x00').decode("utf-8")
    s += 10
    a = (bs[s:s+4]).hex().lstrip('0').upper()
    s += 4
    if not a:
        continue
    syms[a] = n
    syms["sub_" + a] = n
    syms["loc_" + a] = n
    syms["$" + a] = n
    syms["word_" + a] = n

print(syms)

def addSymbols(l):
    l1 = re.split("([\n :,\(\)])",l)
    o = [syms[c] if c in syms else c for c in l1]
    return ''.join(o)

lf = open("../src/objs/vlm.txt.lst", 'r')
f = open("../src/objs/vlm.s", 'w')
while True:
    l = lf.readline()
    if not l:
        break
    print(addSymbols(l).strip(), file=f)

