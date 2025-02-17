.PHONY: all clean

DIRS=src/bin

all: clean virtuallightmachine.jag

virtuallightmachine.jag: cdfront.abs vlm.abs
	$(shell mkdir -p $(DIRS))
	./rmac/rmac -fb -mjerry -isrc src/rsam.das -o src/bin/rsam.o
	./rmac/rmac -fb -mjerry -isrc src/md5.das -o src/bin/md5.o
	./rmac/rmac -fa -mjerry -isrc src/readrsa.das -o src/bin/readrsa.o
	./rmac/rmac -fb -mjerry -isrc src/gettoc.das -o src/bin/gettoc.o
	./rmac/rmac -fr -mtom -isrc src/testgpu.gas -o src/bin/testgpu.o
	./rmac/rmac -fr -rw -isrc src/cdbios.s -o src/bin/cdbios.cof
	./rmac/rmac -fb -rw -u -isrc src/cdboot1.s -o src/bin/cdboot1.cof\
	 	-dRDRSA_S=8404434 -dRDRSA_E=8405662\
	 	-dMD5_S=8403110 -dMD5_E=8404434\
	 	-dRSA_S=8402222 -dRSA_E=8403110\
	 	-dDSP_S=8401410 -dDSP_E=8402222
	./rln/rln -z -l -rw -e -a 802000 x FB000 -o src/bin/cdboot1.abs src/bin/cdboot1.cof\
		src/bin/gettoc.o src/bin/rsam.o src/bin/md5.o src/bin/readrsa.o\
		-i src/images/cd1.cry cd1\
		-i src/images/quest.cry quest\
		-i src/images/cdback.cry cdback\
		-i src/images/arrow.cry arrow\
		-i src/bin/cdbios.cof cd_bios\
		-i src/bin/cdfront.abs cdfront\
		-i src/bin/vlm-stripped.abs vlm
	./utils/CreateCart.py VirtualLightMachine.jag  src/incbin/romheader.bin src/bin/cdboot1.abs
	#echo "f3144937d9f65fe7a1cf7f652d99a71d  VirtualLightMachine.jag" | md5sum -c
	# Edit mode uses pause button
	echo "cc268a46764dc1d8f47d8612cd3c4c8c  VirtualLightMachine.jag" | md5sum -c

vlm.gpu:
	./rmac/rmac -fr -mtom -l -isrc -isrc/vlm/gpu src/vlm/gpu/dbeast.gas -o src/bin/dbeast.o
	./rmac/rmac -fr -mtom -l -isrc -isrc/vlm/gpu src/vlm/gpu/theta.gas -o src/bin/theta.o
	./rmac/rmac -fr -mtom -l -isrc -isrc/vlm/gpu src/vlm/gpu/tau.gas -o src/bin/tau.o
	./rmac/rmac -fr -mtom -l -isrc -isrc/vlm/gpu src/vlm/gpu/sigma.gas -o src/bin/sigma.o
	./rmac/rmac -fr -mtom -l -isrc -isrc/vlm/gpu src/vlm/gpu/alpha.gas -o src/bin/alpha.o
	./rmac/rmac -fr -mtom -l -isrc -isrc/vlm/gpu src/vlm/gpu/beta.gas -o src/bin/beta.o
	./rmac/rmac -fr -mtom -l -isrc -isrc/vlm/gpu src/vlm/gpu/delta.gas -o src/bin/delta.o
	./rmac/rmac -fr -mtom -l -isrc -isrc/vlm/gpu src/vlm/gpu/epsilon.gas -o src/bin/epsilon.o
	./rmac/rmac -fr -mtom -l -isrc -isrc/vlm/gpu src/vlm/gpu/gamma.gas -o src/bin/gamma.o
	./rmac/rmac -fr -mtom -l -isrc -isrc/vlm/gpu src/vlm/gpu/omega.gas -o src/bin/omega.o
	./rmac/rmac -fr -mtom -l -isrc -isrc/vlm/gpu src/vlm/gpu/psi.gas -o src/bin/psi.o
	./rmac/rmac -fr -mtom -l -isrc -isrc/vlm/gpu src/vlm/gpu/shu.gas -o src/bin/shu.o

vlm.abs: vlm.gpu
	$(shell mkdir -p $(DIRS))
	./rmac/rmac ~o1 -fb -u -l -isrc -isrc/vlm src/vlm/vlm.s -o src/bin/vlm.cof
	./rmac/rmac ~o1 -fb -u -isrc -isrc/vlm src/vlm/images.s -o src/bin/images.cof
	./rmac/rmac ~o1 -fb -u -isrc -isrc/vlm src/vlm/vidinit.s -o src/bin/vidinit.cof
	./rmac/rmac ~o1 -fb -u -isrc -isrc/vlm src/vlm/vlmgpu.s -o src/bin/vlmgpu.cof
	./rmac/rmac ~o1 -fb -u -isrc -isrc/vlm src/vlm/ians.s -o src/bin/ians.cof
	./rln/rln -m -z -e -rw -a 192000 x x -o src/bin/vlm.abs src/bin/vlm.cof\
		src/bin/vidinit.cof\
		src/bin/vlmgpu.cof\
		src/bin/images.cof\
		src/bin/ians.cof
	./utils/StripAbsHeader.py src/bin/vlm.abs src/bin/vlm-stripped.abs

cdfront.abs: 
	$(shell mkdir -p $(DIRS))
	./rmac/rmac -fb -u -isrc -isrc/cdfront src/cdfront/pack.s -o src/bin/pack.cof
	./rmac/rmac -fr -mtom -isrc -isrc/cdfront src/cdfront/gpudave.gas -o src/bin/gpudave.o
	./rmac/rmac -fr -rw -u -isrc -isrc/cdfront src/cdfront/gpudave.s -o src/bin/gpudave.cof
	./rmac/rmac -fb -rw -u -isrc -isrc/cdfront src/cdfront/cdfront.s -o src/bin/cdfront.cof
	./rln/rln -z -e -rw -a 80000 x x -o src/bin/cdfront-temp.abs src/bin/cdfront.cof src/bin/pack.cof\
		-i src/cdfront/images/onepage8.cry onepage3\
		-i src/cdfront/rgb.pal rgbpal\
		-i src/bin/gpudave.cof gpuend\
		-i src/cdfront/images/cdnumb2.cry cnumber
	./utils/StripAbsHeader.py src/bin/cdfront-temp.abs src/bin/cdfront.abs


clean:
	-rm src/bin/*.o
	-rm src/bin/*.abs
	-rm src/bin/*.cof
	-rm VirtualLightMachine.jag
