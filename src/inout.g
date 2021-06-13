mac -u inouts
xdefsym inout.das
gasm -CDSP -R1 -S -l -c -o inout.das
ltxconv -lDSP inout
aln -l -f -a 1Ac1a0 x x -o inout.abs inouts.o inout.o
filefix4 inout.abs
fixrom inout.abs
