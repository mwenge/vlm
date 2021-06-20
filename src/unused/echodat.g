mac -u echos
xdefsym echodat.das
gasm -CDSP -R1 -S -l -c -o echodat.das
ltxconv -lDSP echodat
aln -l -f -a 1Ab330 x x -o echodat.abs echos.o echodat.o
filefix4 echodat.abs
fixrom echodat.abs
