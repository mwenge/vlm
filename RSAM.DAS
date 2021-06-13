	PLEN		95
	NOLIST
	Include		"JAGUAR.INC"
	Include		"RSAM.XDF"
	LIST
;
;
;****************************************************************
;								*
;								*
;  RSA decrypt code for the JAGUAR GPU, using MULT instruction	*
;								*
;								*
;****************************************************************
;
;
;
;
;    Programmer:  Dave Staugas
;   Last Update:  6-Oct-93
;
;
;  This is the RSA decryption code for the authentication
;  of Jaguar game cartridges.  Its purpose is to read blocks
;  of data from the signature address space of a game cartridge,
;  decrypt them, then pass control to whatever data results,
;  hoping that the GPU can make sense of it as code.  This (hopefully)
;  code will perform the Message Digest (MD5) algorithm on the entire 
;  address space of the cartridge and compare it with the embedded
;  signature previously computed for it.  If the signatures match, we
;  will permit the Kart to run.
;  
;
;  This RSA decryption code is loaded into the GPU by the Boot-ROM.
;  The MD5 code that performs cartridge authentication is encrypted
;  on each cartridge and will be run immediately after the RSA code
;  finishes executing.  Then, the 68k will interpret the results
;  of the authentication and determine whether to run the Kart or not. 
;
;
;
;
origin	equ	D_RAM			;start of DSP code
cartbeg	equ	$800000			;location to start decrypting
keysize	equ	65			;size of key (518-bits used)
ksize	equ	(keysize+3) & $fffffffc	;size of key to nearest long
;
;
;
;
;	R E G I S T E R S
;
;
;
;  These reg definitions are for the alternate register page (#1)
;
lastcodebyte		REGSET		R0
Dstptr			REGSET		R1
BlockCnt		REGSET		R2
;
;
;
;  These reg definitions are for the base register page (#0)
;
;
cartptr			REGSET		R0
longcnt			REGSET		R1
bytecnt			REGSET		R2
;
;  the following register block is local to mult section..
plierbase		REGSET		R3
plierptr		REGSET		R4
mplier			REGSET		R5
candbase		REGSET		R6
candend			REGSET		R7
candptr			REGSET		R8
mcand			REGSET		R9
mcand1			REGSET		R10
prodcur			REGSET		R11
;  end mult reg block
;
;
;  the following register block is local to div section..
;
dendbase		REGSET		R3
dsorbase		REGSET		R4
dsorend			REGSET		R5
bitcnt			REGSET		R6
rolcount		REGSET		R7
dendALT			REGSET		R8
dendcur			REGSET		R9
;
;  end div reg block
;
;
prodptr			REGSET		R12
;
accum			REGSET		R13
accum1			REGSET		R16
accum2			REGSET		R17
zeroconst		REGSET		R18
;
candlpr			REGSET		R19
plierlpr		REGSET		R20
dendend			REGSET		R21
;
masklo			REGSET		R22
maskhi			REGSET		R23
;
carryhold		REGSET		R24
deboog			REGSET		R25
;
cartsigr		REGSET		R26
ksizer			REGSET		R27
multlpr			REGSET		R28
blkloopR		REGSET		R29
;
;
;
;
;  Here's our local RAM storage
;
;   These are defined using equates instead of labels on DS.L blocks
;    since the assembler insists on stuffing 00's in the so-called
;    uninitialized data blocks..
;
rama		set	origin

PublicKey	equ	rama
rama		set	rama+ksize

accumA	equ	rama	
rama		set	rama+(3*ksize)

accumB	equ	rama
rama		set	rama+(3*ksize)

accumC	equ	rama
rama		set	rama+(3*ksize)

cartsig	equ	rama
rama		set	rama+ksize
;
	org	rama
;
;
;
;
;   Start of code proper.
;
;   These should be the first GPU instructions executed by the
;   production Jaguar at power-up
;		 
;
RSAgpu:
;
; Global labels and constants needed ..
;
	movei	#blkloop,blkloopR	;label for the main outer loop
	movei	#cartbeg,cartptr	;start reading cart at signature space
	movei	#ksize,ksizer		;we need the basic block size often
	movei	#cartsig,cartsigr	;buffer ptr for inverted input block
;
	loadb	(cartptr),accum		;fetch negative block count, this run
	moveta	accum,BlockCnt		;save in alternate reg
;
	movei	#(bootII-4),accum
	moveta	accum,Dstptr		;here's where the unRSA goes
;
	moveq	#0,accum
	moveta	accum,lastcodebyte	;start with 0 here
;
blkloop:
;
;  Copy next (keysize) bytes to "cartsig" in inverted order
;
	movei	#$000000ff,maskhi
	move	cartsigr,R14		;local destination for keysize bytes from cart
	move	ksizer,longcnt		;use this as index
loadsig:	
	moveq	#0,accum		;accum = 0
	moveq	#4,bytecnt		;4 times thru this loop
swapit4:		
	addq	#1,cartptr		;advance cartridge ptr
	loadb	(cartptr),accum1	;get next byte
	or	accum1,accum		;place at LSB position
	subq	#1,bytecnt		;do it 4 times
	jr	NE,swapit4		;br always does next instr:
	rorq	#8,accum		;  move to MSB position
;
	subq	#4,longcnt		;pre-decrement DST
	jr	NE,loadsig		;go for all ksize bytes
	store	accum,(R14+longcnt)	;save inverted order to cartsig
;
	and	maskhi,accum		;extra 3 bytes are dummy read
	store	accum,(R14)		;we needed to fill out the long word
	subq	#3,cartptr		;so clear it and adjust cartptr
;
;
	rorq	#8,maskhi	;=FF000000
	sharq	#8,maskhi	;=FFFF0000
	move	maskhi,masklo	;these masks needed for odd word alignments
	not	masklo		;=0000FFFF
;
	movei	#plierlp,plierlpr	;set-up outer multiply loop JUMP ptr
	movei	#candlp,candlpr		;inner-loop JUMP ptr
	movei	#multlp,multlpr		;loop JUMP ptr for 2nd multiply
	moveq	#0,zeroconst		;constant of 0 is handy
;
	move	cartsigr,plierptr	;1st multiplier is cart data
	add	ksizer,plierptr		; ptr starts at LSB
	move	plierptr,candend	;1st multiplicand is also cart data

	move	cartsigr,plierbase	;
	move	cartsigr,candbase
;
	movei	#accumA,prodptr		;accumA is our product area
	move	ksizer,accum		;clear 2*ksizer bytes
	sharq	#1,accum
;
;
;  Each multiple precision multiply starts here
;
;
;  These registers should be set-up as follows:
;
;  prodptr   -> MSB of product area (need not be cleared)
;    accum   = # of longs in product (to be cleared)
;
;  plierbase -> MSB of multiplier
;  plierptr  -> LSB (+1) of multiplier
;
;  candbase  -> MSB of multiplicand
;  candend   -> LSB (+1) of multiplicand
;
;     maskhi = FFFF0000
;     masklo = 0000FFFF
;
;     zeroconst = 0  , a constant of zero (which is useful)
;
multlp:
	move	candend,candptr		;start multiplicand ptr @ LSB (+1)
;
;  Clear product area
;
	subq	#1,accum
clearprod:
	store	zeroconst,(prodptr)	;zero a long
	addqt	#4,prodptr
	jr	NE,clearprod
	subq	#1,accum		; accum times
;
	move	prodptr,dendend		;prodptr -> LSB (+1) of product
;				 save for possible use as dividend end
;
;   Get next Most Signif long of multiplier
;
plierlp:
	subq	#4,plierptr		;adjust ptr
	move	prodptr,prodcur		;prodcur starts here & moves to MSB
	load	(plierptr),mplier	;get multiplier long
	subq	#4,prodptr
	move	mplier,longcnt
	rorq	#16,longcnt
candlp:
	subq	#4,candptr
	load	(candptr),mcand		;get next multiplicand long
;
;  16-bit word operand chunks are represented by letters..
;
;         . . . A B C D
;      x  . . . E F G H
;             __________
;                  (H*D)     <- product is sum of these partial results
;                (H*C)           added with this alignment
;                (G*D)
;              (G*C)
;
;              (F*D)
;            (F*C)
;            (E*D)
;          (E*C)
;
;   etc..
;
	move	mcand,mcand1		;copy CD to mcand1
	move	mcand,accum		;mcand will be just "D" in accum
	rorq	#16,mcand1		;mcand1 will be DC (actually just "C")
	mult	mplier,accum		;     accum = (H*D)
	move	mcand1,accum1		;copy "C" to accum1
	mult	longcnt,mcand		;     mcand = (G*D)
	mult	mplier,accum1		;    accum1 = (H*C)
	mult	longcnt,mcand1		;    mcand1 = (G*C)
;
;  add the 4 partial products with above illustrated alignment
;    to produce the 64-bit result of the 32x32 multiply
;
	add	accum1,mcand		;mcand = (H*C) + (G*D) 
	moveq	#0,accum1		; we need this to save any CY
	addc	accum1,accum1		;  capture any carry-out
	rorq	#16,mcand		;mcand  = LLLLMMMM
	rorq	#16,accum1		;accum1 = 000C0000
	move	mcand,accum2		;copy to accum2 for masking
;
	and	masklo,accum2		;accum2 = 0000MMMM
	and	maskhi,mcand		;mcand  = LLLL0000
	or	accum2,accum1		;accum1 = 000CMMMM
;
	subq	#4,prodcur		;pre-decrement destination product ptr
	add	mcand,accum		;   MSL LSL
	load	(prodcur),mcand		;fetch previous accumulations
	addc	mcand1,accum1		;accum1 accum is 64-bit result
;
	add	accum,mcand		;add our new LSL
	move	prodcur,mcand1		;use mcand1 as temp ptr
	store	mcand,(prodcur)		;so we can add new results
;
	subqt	#4,mcand1		;  that can move up toward the MSB
;
	load	(mcand1),accum		;fetch previous for our MSL add
	addc	accum1,accum		;add w/CY new MSL
	jr	CC,noCYout		;and skip CY propagate if none
	store	accum,(mcand1)		;  save results
;
;   CY propagate loop...
CYlp:
	subqt	#4,mcand1		;move to next MSL
	load	(mcand1),accum		;fetch previous
	addc	zeroconst,accum		;  add 0 (w/CY)
	jr	CS,CYlp			;keep going until no more CY
	store	accum,(mcand1)		;save results
;
;  Next multiplicand long please..
;
noCYout:
	cmp	candptr,candbase	;reached MSL of multiplicand?
	jump	NE,(candlpr)		;br for more if not
;	nop
;
;  Finished entire multiplicand, go for next multiplier long
;
	cmp	plierptr,plierbase	;reached MSL of multiplier?
	move	candend,candptr		;restore multiplicand to LSL
	jump	NE,(plierlpr)		;br if not
;	nop
;
;  We're done with this multiply..
;    check to see if it was the square or the cube operation..
;
	cmp	candbase,cartsigr	;was previous multiply our cube? 
	move	ksizer,accum		;start building product clear count
	jr	NE,cubedone		; br if so (candbase is "accumA" for cube)
	add	ksizer,plierptr
;
;
	movei	#accumA,candbase	;make just formed product our next multiplicand
	movei	#accumB,prodptr		;accumB is now our product area
;
	move	candbase,candend
	add	ksizer,accum		;accum = 2*ksizer
	add	accum,candend		;candend -> LSL (+1) of multiplicand		
	add	ksizer,accum		;accum = 3*ksizer for cube
	jump	(multlpr)		;go for next multiply
	sharq	#2,accum		;adjust product clear count for longs
;
;
cubedone:
;
;
;   accumB has the cube we just computed--it will be the dividend
;   PublicKey will be normalized & copied to accumA--it will be the divisor
;   
	movei	#accumB,dendbase	;dividend base (MSL)	
	movei	#accumA,dsorbase	;divisor base  (MSL)
	movei	#accumC,dendALT		;alternate dividend base buffer
;	
	move	dendbase,R14		;use the dedicated index base regs
	move	dsorbase,R15

	moveq	#0,longcnt		;find 1st non-zero long
msbcheck:
	load	(R14+longcnt),accum	;in the dividend
	store	zeroconst,(R15+longcnt)	; clear out divisor while we're at it
	or	accum,accum		;got non-zero?
	jr	EQ,msbcheck		;br if not
	addq	#4,longcnt		;advance index
;
;
;
	move	ksizer,bitcnt		;dividend is 3*ksizer, divisor is 1*ksizer
	subq	#4,longcnt		;adjust index for 1 excess addq
	add	ksizer,bitcnt		;2*ksizer is difference
	sub	longcnt,bitcnt		; but we can skip 0 longs in dividend MSL
	shlq	#3,bitcnt		;*8 for bit count to shift & subtract
;
;
;	
	movei	#PublicKey,R14		;copy our divisor to shift area
	add	longcnt,dsorbase	;starting divisor MSL
	add	longcnt,dendbase	;starting dividend MSL
	add	longcnt,dendALT		;starting dividend alternate MSL
	move	dsorbase,R15		;use R15 ptr for copy destination
	subq	#4,R15			;-4 is more convenient for our skewed loops
;
;  We still have the 1st non-zero MSL of the dividend in accum
;
	moveq	#0,accum1		;count how many MSBit 0's
msbit:
	add	accum,accum		;shift left, CY = next MSBit
	jr	CC,msbit		;repeat if still zero
	addq	#1,accum1		;count it
;
;  accum1 = # of MSBit zeros (+1)
;
	moveq	#26,rolcount	;26 bits of msb zeros in Public Key (adjust for new PK)
	movei	#noshift,maskhi
	sub	accum1,rolcount	;rolcount = # of bits to left shift
;
	jump	CS,(maskhi)	;br if no left shift needed
	addq	#1,rolcount	;adjust for extra add in loop
;
	add	rolcount,bitcnt	;total # of shifts & subtracts required
;
	neg	rolcount	;we need left-shift in a right-shift world
	moveq	#0,maskhi	;form masks for this much shifting
	not	maskhi
	sh	rolcount,maskhi	;maskhi = %1111...0000 (# of 0's given by rolcount)
	move	maskhi,masklo
	not	masklo		;masklo = %0000...1111 (# of 1's given by rolcount)
;
;  copy divisor from PublicKey and bit-wise align to our Cube dividend  
;
	moveq	#0,longcnt		;start at MSL index position
getkey:
	load	(R14+longcnt),accum1	;pick up next PublicKey divisor
	ror	rolcount,accum1		;align to dividend
;	and	maskhi,accum1	;LSBits are in high end, MSbits are already 0's
;
;  accum1 has previous LSBits in high end with low end masked to 0's
;
shiflp:
	addq	#4,longcnt		;adjust index for next SRC
	load	(R14+longcnt),accum2	;get next SRC	
	ror	rolcount,accum2		;align to DST
	move	accum2,accum		;need 2 copies for masking
	and	masklo,accum2		;form MSBits in low position
	and	maskhi,accum		;and LSBits in high position
	or	accum2,accum1		;mate new MSBits in low with
	cmp	longcnt,ksizer		; have we picked up all the SRC?
	store	accum1,(R15+longcnt)	; previous LSBits in high--save to DST
	jr	NE,shiflp		;br for more if not
	move	accum,accum1		; use our LSBits for next iter
;
	or	accum1,accum1		;do we have any LSBits to worry about?
	jr	EQ,finish0		; if not, we're done
	nop
;
	jr	finish0			;skip over non-shifted version
	store	accum1,(R15+longcnt)	;save one more
;
;  DST R15 has been adjusted by -4 so we can
;     do direct copy the following way:
;
noshift:
	moveq	#0,longcnt
copkey:
	load	(R14+longcnt),accum	;no shift required
	addq	#4,longcnt
	cmp	ksizer,longcnt		;done it all?
	jr	NE,copkey		;repeat if not
	store	accum,(R15+longcnt)
;
;
finish0:
	move	R15,dsorend		;we need a divisor end ptr
	move	dendbase,R14		;SRC
	add	longcnt,dsorend		;this is our divisor LSL (not +1!)
;
;   Copy dividend to alternate buffer
;
	move	dendALT,R15		;DST
;	
	add	ksizer,R14		;we only need 2*ksizer LSL's
	move	ksizer,longcnt
	add	ksizer,R15	 
;
	add	ksizer,longcnt
;
;
altcopy:
	subq	#4,longcnt
	load	(R14+longcnt),accum
	jr	PL,altcopy
	store	accum,(R15+longcnt)
;
	movei	#divloop,rolcount
	movei	#doritesh,prodptr	;we have to jump to far on this
;
	move	dsorbase,R14
	move	dendbase,R15
;
;  now see if we can do some subtracts & shifts
;
divloop:
	load	(R14),accum	;get MSLong of divisor
	load	(R15),accum1	;and MSLong of dividend
	cmp	accum,accum1	;quick check: can we cancel the sub?
	jump	CS,(prodptr)	;br if so--go to rite shift section
	or	accum,accum1	;since we have hi end, marched off right yet?
;
	jr	NE,skiplfm	;br if 1's still found in MSlongs
	move	dsorend,longcnt	;it won't hurt to always do sub set-up
;
	addq	#4,R14		;advance divisor base ptr to next long
	addq	#4,R15		;and dividend base too
	jr	divloop		;re-check the MSLongs (max iter here is 1)
	addq	#4,dendALT	;also advance alt difference buffer base
;
skiplfm:
;
; finish Subtraction set-up..
	sub	R14,longcnt	;form offset to LSLongs (not +1!)
;
	move	dendALT,dendcur	;we wouldn't need if there was a 3rd 
	add	longcnt,dendcur	;   indirect indexed register! CY=0
;
subloop:
	load	(R14+longcnt),accum	;get next divisor LSLong
	load	(R15+longcnt),accum1	; & next dividend LSLong
	subc	accum,accum1		;accum1 = (dividend - divisor)
	store	accum1,(dendcur)	;save result in alt diff buffer
	addc	zeroconst,zeroconst	;capture CY in bit0
	subq	#4,longcnt		;& advance index for others LSL's
	subqt	#4,dendcur		;advance to next LSLong in alt
	jr	PL,subloop		;do all indexes (including 0)
	sharq	#1,zeroconst		;restore CY from previous subc
;
	jr	CS,doritesh	;CY=1 means throw out result
	move	dendALT,dendcur	;won't hurt to always do
;	
	move	R15,dendALT	;else, swap dividend & alt diff
	move	dendcur,R15	; for a successful (pos result) sub
;
;   Time to right-shift divisor
;   (DSP is really ugly on multiple-precision rite-shifts)
;
doritesh:
	move	R14,dendcur	;divisor MSL ptr to temp
	moveq	#0,accum1	;we need this clear (bit31 holder)
nextrite:
	moveq	#0,zeroconst	;and this too
	load	(dendcur),accum	;pick up next MSLong divisor
	shrq	#1,accum	;shift right 1x  (0-> LONG -> CY)
	addc	zeroconst,zeroconst	;capture CY in bit0
	or	accum1,accum	;OR in bit31 from previous rite sh
	store	accum,(dendcur)	;save new shifted LONG with prev bit31
	move	zeroconst,accum1	;copy shrq's CY to bit31 holder
	rorq	#1,accum1		;put it in bit31 position
	cmp	dsorend,dendcur		;have we reached LSLong?
	jr	CS,nextrite	;br for all divisor longs	
	addq	#4,dendcur	;advance to next LSLong
;
	or	accum1,accum1	;does bit31 show a rite-shift carry-out?
	jr	EQ,skiprtm	;br if not
	moveq	#0,zeroconst	;always do
;
	store	accum1,(dendcur)	;save $80000000 to new LSLong
	move	dendcur,dsorend		; & update divisor LSLong ptr
skiprtm:
	subq	#1,bitcnt	;shift all the way down to dividend's LSLong
	jump	PL,(rolcount)	;will do an unnecessary rite-shift at end
	nop

shiftdone:
;
;   Now "unroll" the decrypted residue into our space
;
;
	movefa	lastcodebyte,accum2	;get these from the alternate reg set	
	movefa	Dstptr,R14
	movefa	BlockCnt,rolcount
;
;
	move	ksizer,longcnt
	movei	#$000000ff,masklo
nextrol:
	moveq	#4,bytecnt
;nextrolx:
	subq	#4,longcnt
	moveq	#0,accum1
	load	(R15+longcnt),accum
rollong:
;
	add	accum,accum2
;*test	move	accum,accum2
;
	rorq	#24,accum1
	and	masklo,accum2
	or	accum2,accum1
	subq	#1,bytecnt
	jr	NE,rollong
	rorq	#8,accum
;
	addq	#4,R14
	cmpq	#4,longcnt
	jr	NE,nextrol	
	store	accum1,(R14)
;
	moveta	accum2,lastcodebyte
	moveta	R14,Dstptr
;	
	addq	#1,rolcount
;
;  check for reasonable deRSA result
;
	moveq	#$15,accum2		;most signifigant (throw-away) byte
	load	(R15),accum		; must always be $15
	cmp	accum,accum2
	jr	NE,cartbad
	and	masklo,rolcount
;
	jump	NE,(blkloopR)
	moveta	rolcount,BlockCnt
;
	movei	#$ABCDEFFF,R4		;neg # means we passed so far
erastop:
	movei	#PublicKey,R2
	jr	cartgood		;we can proceed
	nop
;
;  Come here if the cart is fishy...
;
cartbad:
	movei	#$12345678,R4		;put in magic death longword
	movei	#bootII,R2		; & clear out this too
cartgood:
	move	R4,R3

	movei	#PublicKey,R0		;clear out our code
	movei	#erastop,R1		; up to here
;
;	movei	#$f03ff0,R0
;	movei	#$f03ff0,R1
;	movei	#$f03ff0,R2
;	
cartclr:
	store	R3,(R0)
	store	R3,(R2)
	addq	#4,R0
	addq	#4,R2
	cmp	R1,R0
	jr	CS,cartclr
	moveq	#0,R3
;
	or	R4,R4			;are we good or bad?
;
	jr	MI,bootIIa		;minus is good on pass 1, bad on 2
;	nop


stopper:
	moveq	#0,R3
	movei	#D_CTRL,R31
gameover:
	store	R3,(R31)
	jr	gameover
	nop
;
;   Some regs we take care of for the MD5 block...
;
accum		REGSET	R0
scratch2	REGSET	R1
hashptr		REGSET	R7
;
hashcur		REGSET	R8
ourkey		REGSET	R25
loopcnt		REGSET	R9
rolcnt		REGSET	R5
;
hashbase	equ	bootII+$24
hashswap	equ	bootII+$bc
;
overhash	equ	bootII+$48
hashman		equ	bootII+$150
;
bootIIa:
	movei	#hashswap,hashcur	;ptr to 1st swap longword in MD5 code
	movei	#hashbase,ourkey
;
	moveq	#16,loopcnt
	shlq	#2,loopcnt		;loopcnt <- 64
	moveq	#8,rolcnt
;
;
swapit:
	load	(hashcur),accum		;get embedded signature long
	load	(ourkey),scratch2	;and table entry to be swapped
	store	accum,(ourkey)		;now swap
	store	scratch2,(hashcur)	;  the two
	add	loopcnt,hashcur		;advance embedded block ptr to next block
	subq	#1,rolcnt		;count off 8 to do
	jr	NE,swapit		;do all 8
	addq	#4,ourkey		; advance table ptr
;
;  code is now in place, signature hash is in its table & God is in heaven
;   but we still need to relocate the code before executing...
;
;
;  we need to relocate 5 swapped movei's
;
	movei	#(bootII-$f035ac),R17	;offset to add
	movei	#ovreltab,R18	
	moveq	#16,R16
	move	PC,R19		;table of addresses to relocate
	jump	(R18)
	addq	#6,R19
relotab:
	dc.l	bootII+$4A
	dc.l	bootII+$7A
	dc.l	bootII+$80

	dc.l	bootII+$150
	dc.l	bootII+$19c
	dc.l	bootII+$1e8
	dc.l	bootII+$234

	dc.l	bootII+$11e

ovreltab:
	load	(R19),R5
	btst	#1,R5
	jr	NE,stradl
	addq	#4,R19
;
	load	(R5),R0
	ror	R16,R0
	add	R17,R0
	ror	R16,R0
	store	R0,(R5)
;
	movei	#checke,R0
	jump	(R0)
	moveq	#0,R16
;
stradl:
	subq	#2,R5
	load	(R5),R0		;llllLLLL
	addq	#4,R5
	load	(R5),R1		;HHHHhhhh
	move	R0,R2
	move	R1,R3
	shlq	#16,R2
	shrq	#16,R3
	or	R3,R2
	rorq	#16,R2
	add	R17,R2
	move	R2,R3		;HHHHLLLL
	shlq	#16,R2		;LLLL0000
	shrq	#16,R0		;0000llll
	shrq	#16,R3		;0000HHHH
	shlq	#16,R1		;hhhh0000
	or	R2,R0		;LLLLllll
	or	R3,R1		;hhhhHHHH
	rorq	#16,R0		;llllLLLL
	rorq	#16,R1		;HHHHhhhh
	store	R1,(R5)
	subq	#4,R5
	store	R0,(R5)
checke:
	move	R18,R0
	sub	R19,R0
	cmpq	#5,R0
	jump	CC,(R18)
	nop
;
	movei	#$F1B000-$F03000,R17
	cmpq	#0,R0
	moveq	#16,R16
	jump	NE,(R18)
	nop
;
	movei	#overhash,scratch2
	movei	#hashman,hashptr
	jump	(scratch2)		;skip over the signature we just built
	nop
;
;	movei	#stopper,R0
;	jump	(R0)
;	nop
;
	align	long
;
;  This is the code generated that we now execute...
;
bootII:
;	ds.l	2*ksize		;here's where the MD5 code goes
;
;
