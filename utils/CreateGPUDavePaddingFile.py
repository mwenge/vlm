#!/usr/bin/env python3

"""
Extract the binary components from the original t2k rom
"""

import sys
import os
from itertools import chain


o = open("../src/gpudave-header.bin", 'wb')
o.write(b'\x00\xf0\x30\x00\x00\x00\x01\xcc')
