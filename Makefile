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
		-i src/incbin/cdbios.txt cd_bios\
		-i src/bin/cdfront.abs cdfront\
		-i src/bin/vlm.abs vlm
	./utils/CreateCart.py VirtualLightMachine.jag  src/incbin/romheader.bin src/bin/cdboot1.abs
	echo "0c29604b11b8bf2f98548f8f423f8f8f  VirtualLightMachine.jag" | md5sum -c

vlm.abs: 
	$(shell mkdir -p $(DIRS))
	./rmac/rmac ~o1 -fb -isrc -isrc/vlm src/vlm/vlm.s -o src/bin/vlm.cof
	./rln/rln -e -rw -a 192000 x x -o src/bin/vlm-temp.abs src/bin/vlm.cof
	./utils/StripCDFrontHeader.py src/bin/vlm-temp.abs src/bin/vlm.abs

cdfront.abs: 
	$(shell mkdir -p $(DIRS))
	./rmac/rmac -fb -u -isrc src/pack.s -o src/bin/pack.cof
	./rmac/rmac -fr -mtom -isrc src/gpudave.gas -o src/bin/gpudave.o
	./rmac/rmac -fr -rw -u -isrc src/gpudave.s -o src/bin/gpudave.cof
	./rmac/rmac -fb -rw -u -isrc src/cdfront.s -o src/bin/cdfront.cof
	./rln/rln -e -rw -a 80000 x x -o src/bin/cdfront-temp.abs src/bin/cdfront.cof src/bin/pack.cof\
		-i src/images/onepage8.cry onepage3\
		-i src/rgb.pal rgbpal\
		-i src/bin/gpudave.cof gpuend\
		-i src/images/cdnumb2.cry cnumber
	./utils/StripCDFrontHeader.py src/bin/cdfront-temp.abs src/bin/cdfront.abs


clean:
	-rm src/bin/*.o
	-rm src/bin/*.abs
	-rm src/bin/*.cof
	-rm VirtualLightMachine.jag