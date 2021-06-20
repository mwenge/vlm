#!/usr/bin/env python3

"""
Extract the binary components from the original t2k rom
"""

import sys
import os

if len(sys.argv) < 2:
    print("Not enough filenames given")
    exit()

romname = sys.argv[1]
rom = open(romname, 'wb')

args = len(sys.argv)

f = open(sys.argv[2], 'rb')
rom.write(f.read())

f = open(sys.argv[3], 'rb')
rom.write(f.read()[0xa8:0x2d268])


