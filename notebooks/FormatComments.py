import sys
import os
import re
"""
Reformat source code lines in a file such as yak.s so that the
comments are uniformly aligned.
"""
lines = sys.stdin.readlines()
code_lines = [l.split(';')[0].rstrip() for l in lines]
comm_lines = ["; "+l.split(';')[1].strip() if ";" in l else ""  for l in lines]
max_code_line = max([len(l) for l in code_lines])

for i,l in enumerate(code_lines):
  comment = (" " * (max_code_line - len(l))) + (" " * 3) + comm_lines[i] if comm_lines[i] else ""
  if l.strip():
      print(l + comment)
  else:
      print((" " * 8) + comm_lines[i])




