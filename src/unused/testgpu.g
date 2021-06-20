mac -u testgpus
xdefsym testgpu.gas
gasm -CGPU -R1 -S -l -c -o -ID:\BIN\ testgpu.gas
ltxconv -lGPU testgpu
aln -l -f -a 40000 x x -o testgpu.abs testgpus.o testgpu.o
filefix4 testgpu.abs
