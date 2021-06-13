#====================================================================
#
# Makefile for CD boot code
#
#====================================================================
#
#
.SUFFIXES:	.o .s
.s.o:
	mac  -u $*


.SUFFIXES:	.bin .gas
.gas.bin:
	gasm -o -CGPU -R1 -l $*.GAS
	ltxconv -a $*


.SUFFIXES:	.o .gas
.gas.o:
	xdefsym $*.GAS
	gasm -l -o -c -CGPU -R1 -S -Id:\bin $*.GAS
	ltxconv -lGPU $*


.SUFFIXES:	.bin .das
.das.bin:
	gasm -o -CDSP -R1 -l $*.DAS
	ltxconv -a $*


.SUFFIXES:	.o .das
.das.o:
	xdefsym $*.DAS
	gasm -l -o -c -CDSP -R1 -S -Id:\bin $*.DAS
	ltxconv -lDSP $*

#
#====================================================================
#	EXECUTABLES
#====================================================================
#
cdboot1.db: cdboot1.o gettoc.o cdbios.txt cdfront.abs vlm.abs md5.o rsam.o readrsa.o makefile 
	aln -l -f -a 802000 x FB000 -o cdboot1.abs cdboot1.o\
		gettoc.o rsam.o md5.o readrsa.o\
		-i cd1.cry cd1\
		-i quest.cry quest\
		-i cdback.cry cdback\
		-i arrow.cry arrow\
		-i cdbios.txt cd_bios\
		-i cdfront.abs cdfront\
		-i vlm.abs vlm
	filefix4 cdboot1.abs
	fixrom cdboot1.abs
#
rsam.o: rsam.das
	xdefsym RSAM.DAS
	gasm -l -o -c -CDSP -R1 -S -Id:\bin RSAM.DAS
	ltxconv -lRSA $*
#
md5.o: md5.das
	xdefsym MD5.DAS
	gasm -l -o -c -CDSP -R1 -S -Id:\bin MD5.DAS
	ltxconv -lMD5 $*
#
readrsa.o: readrsa.das
	xdefsym READRSA.DAS
	gasm -l -o -c -CDSP -R1 -S -Id:\bin READRSA.DAS
	ltxconv -lRDRSA $*
#
cdfront.abs: cdfront.s onepage8.cry,cdnumb2.cry gpudave.bin pack.o vlm.equ 
	mac -u cdfront
	aln -l -f -a 80000 x x -o cdfront.abs cdfront.o pack.o\
		-i onepage8.cry onepage3\
		-i rgb.pal rgbpal\
		-i gpudave.bin gpuend\
		-i cdnumb2.cry cnumber
	filefix4 cdfront.abs
	fixrom cdfront.abs
#
cdboot1.o: cdboot1.s vlm.equ cd.inc public.key publicd.key rsam.o testgpu.o
gettoc.o: gettoc.das
pack.o: pack.s
gpudave.bin: gpudave.gas
