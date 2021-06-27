#!/usr/bin/env python3

"""
Pad labels with spaces
"""

import sys
import os
import re

ls = sys.stdin.readlines()
for l in ls:
    if ":" in l:
        print(l.rstrip())
    else:
        print(" " * 20 + l.rstrip())

