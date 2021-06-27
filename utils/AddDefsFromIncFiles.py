#!/usr/bin/env python3

"""
Extract the binary components from the original t2k rom
"""

import sys
import os
import re

from inc import syms

def addSymbols(l):
    l1 = re.split("([\n :,\(\)#])",l)
    # Ignore any symbols that occur after a dc.b statement
    idc = l1.index("dc.b") if "dc.b" in l1 else 9999
    o = [syms[c.upper()] if c.upper() in syms and idc > i else c for i,c in enumerate(l1)]
    return ''.join(o)

ws = sys.stdin.readlines()

for w in ws:
    n = addSymbols(w)
    print(n.rstrip())

