#!/usr/bin/env python3

"""
Extract the binary components from the original t2k rom
"""

import sys
import os
from itertools import chain


f = open("vlmdisassemble/vlm.txt", 'rb')
bs = f.read()

o = open("vlmdisassemble/1-vlm-firstsectionofcode.bin", 'wb')
o.write(bs[:0x4ae8])

o = open("vlmdisassemble/2-sines.bin", 'wb')
o.write(bs[0x4ae8:0x4de8])

o = open("vlmdisassemble/3-bin.bin", 'wb')
o.write(bs[0x4de8:0x5154])

o = open("vlmdisassemble/4-strings.bin", 'wb')
o.write(bs[0x5154:0x6a50])

o = open("vlmdisassemble/5-other.bin", 'wb')
o.write(bs[0x6a50:0x6cb0])


o = open("vlmdisassemble/6-font.bin", 'wb')
o.write(bs[0x6cb0:0x6a50])

o = open("vlmdisassemble/8-vlm-grafix.cry", 'wb')
o.write(bs[0x8dd0:0x9028])

o = open("vlmdisassemble/8a-vlm-grafix.cry", 'wb')
o.write(bs[0x9028:0x9328])
o = open("vlmdisassemble/9-ians.bin", 'wb')
o.write(bs[0x1a1a0:0x1a244])
o = open("vlmdisassemble/10-last.bin", 'wb')
o.write(bs[0x1a244:])
