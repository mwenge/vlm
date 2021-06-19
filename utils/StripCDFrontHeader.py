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
rom = open(romname, 'rb')

outname = sys.argv[2]
out = open(outname, 'wb')
out.write(rom.read()[168:])

