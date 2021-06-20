#!/usr/bin/env python3

"""
Extract the binary components from the original vlm txt file
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

o = open("vlmdisassemble/3-sympad.bin", 'wb')
o.write(bs[0x4de8:0x5154])

o = open("vlmdisassemble/4-strings.bin", 'wb')
o.write(bs[0x5154:0x6a50])

o = open("vlmdisassemble/5-other.bin", 'wb')
o.write(bs[0x6a50:0x6cb0])


o = open("vlmdisassemble/6-font.bin", 'wb')
o.write(bs[0x6cb0:0x70c8])

o = open("vlmdisassemble/7-jlogo2.cry", 'wb')
o.write(bs[0x70c8:0x7990])

o = open("vlmdisassemble/7-jaglogo.cry", 'wb')
o.write(bs[0x7c44:0x8dd0])

o = open("vlmdisassemble/8-vlm-logo.cry", 'wb')
o.write(bs[0x8dd0:0x9050])

o = open("vlmdisassemble/8a-avbank.bin", 'wb')
o.write(bs[0x90c8:0x14800])

o = open("vlmdisassemble/8c-videoini.o", 'wb')
o.write(bs[0x14838:0x149f0])

o = open("vlmdisassemble/8d-alpha.bin", 'wb')
o.write(bs[0x149f0:0x153c8])
o = open("vlmdisassemble/8d-beta.bin", 'wb')
o.write(bs[0x153c8:0x159f0])
o = open("vlmdisassemble/8d-gamma.bin", 'wb')
o.write(bs[0x159f0:0x16430])
o = open("vlmdisassemble/8d-omega.bin", 'wb')
o.write(bs[0x16430:0x168c0])
o = open("vlmdisassemble/8d-psi.bin", 'wb')
o.write(bs[0x168c0:0x16fe8])
o = open("vlmdisassemble/8d-delta.bin", 'wb')
o.write(bs[0x16fe8:0x17520])
o = open("vlmdisassemble/8d-epsilon.bin", 'wb')
o.write(bs[0x17520:0x17f08])
o = open("vlmdisassemble/8d-theta.bin", 'wb')
o.write(bs[0x17f08:0x18138])
o = open("vlmdisassemble/8d-sigma.bin", 'wb')
o.write(bs[0x18138:0x18b38])
o = open("vlmdisassemble/8d-tau.bin", 'wb')
o.write(bs[0x18b38:0x193c8])
o = open("vlmdisassemble/8d-shu.bin", 'wb')
o.write(bs[0x193c8:0x199c0])
o = open("vlmdisassemble/8d-dbeast.bin", 'wb')
o.write(bs[0x199c0:0x19b08])

o = open("vlmdisassemble/8d-vlmlogo2.cry", 'wb')
o.write(bs[0x19cc6:0x1a1a0])

o = open("vlmdisassemble/9-ians.bin", 'wb')
o.write(bs[0x1a1a0:0x1a244])
o = open("vlmdisassemble/10-last.bin", 'wb')
o.write(bs[0x1a244:])
