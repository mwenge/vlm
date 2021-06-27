#!/usr/bin/env python3

"""
Extract the binary components from the original t2k rom
"""

import sys
import re


ls = [re.split("([\n #\$])",l.rstrip()) for l in sys.stdin.readlines()]
lsd = {l[24][-8:].upper():l for l in ls}
jumps = {l[-1]:"routine"+str(i) for i,l in enumerate(ls) if l[-1] in lsd}

for i,l in enumerate(ls):
    j = l[-1]
    if j in jumps:
        l[-9] = jumps[j]
        l[-10] = ''
    a = l[24][-8:].upper()
    if a in jumps:
        print(" " * 43 + jumps[a] + ':')
    print(''.join(l))


