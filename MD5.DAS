	PLEN		95
	NOLIST
	Include		"JAGUAR.INC"
	Include		"MD5.XDF"
	LIST
;
;********************************************************
;							*
;							*
;   Message Digest (MD5) for JAGUAR CD rom		*
;							*
;							*
;********************************************************
;
;
;
;   Programmer:  Dave Staugas
;  Last Update:  9-Jan-94	
;
;
;
;
;
accum			REGSET		R0
scratch1		REGSET		R1
abit6			REGSET		R2
index			REGSET		R3
indexinc		REGSET		R4
rolcnt			REGSET		R5
speciesR		REGSET		R6
;
;
hashptr			REGSET		R7		
hashcur			REGSET		R8

loopcnt			REGSET		R9
inptptre		REGSET		R10

mask31			REGSET		R11
temp			REGSET		R12
command			REGSET		R13
;
;
regAx			REGSET		R16
regBx			REGSET		R17
regCCx			REGSET		R18
regDx			REGSET		R19
;
nextblkR		REGSET		R20
hashendR		REGSET		R21
mask63			REGSET		R22
blckbase		REGSET		R23
nextserR		REGSET		R24
ourkey			REGSET		R25
;
regA			REGSET		R26
regB			REGSET		R27
regCC			REGSET		R28
regD			REGSET		R29
;
;
;  this needs to follow right after RSA code loaded by bootrom
;
	org	D_RAM
;
baseaddress:
	movei	#sramout,r0	; (interrupt 0 - CPU)
	jump	(r0)
	nop
	nop
	nop
	nop
;
i2s_vec:
	movei	#i2s_start,r30	;(interrupt 1 - I2S)
	jump	(r30)
	nop
	nop
	nop
	nop
;
;
MD5semi:
	dc.l	-1	;=0 start MD5 from I2S, =1 start MD5 from addr range 
MD5head:
	dc.l	0	;start MD5 when 16 of these have been read
MD5leng:
	dc.l	0	;# of blocks to MD5 before quitting
;
MD5result:
	dc.l	0	;stuff hash here when done
	dc.l	0
	dc.l	0
	dc.l	0
;
	dc.l	0	;test (count of irq's)
MD5start:
;
;  load up bank #0 regs with constants for I2S irq
;
	movei	#D_RAM+$2000,R31	;setup stack at SRAM top
	movei	#D_FLAGS,R29		;flags
	movei	#LTXD,R28		;left receive
	movei	#RTXD,R27		;right receive
	movei	#$F1C000,R26		;data DST pointer

	movei	#$8000,R20		;time-out maximum count
;	movei	#$100000,R20		;time-out maximum count

	movei	#$ffffff80,R25
	movei	#D_HIDATA,R24		;equates have misnomer for "D_MOD"
	store	R25,(R24)		;set modulo mask for circular queue
;
	moveq	#0,R25			;start pattern
	moveq	#0,R24			;length (in longs)
;
	moveq	#16,R23			;pattern count
	movei	#($981E0000+(i2s_end & $FFFF)),R22	;secondary irq
	movei	#i2s_vec,R21		;vector location to stuff
;
;  now switch to register bank 1 for foreground
;
	moveta	R29,R29		;just in case of trouble here
	load	(R29),R0	;get D_FLAGS


;		   3         2         1
;		  10987654321098765432109876543210
	movei	#%00000000000000000111111000110000,R1	;bits to set
	movei	#%11111111111111110111111000111111,R2	;bits to clear
	or	R1,R0
	and	R2,R0
;
;	bset	#14,R0		;switch to bank 1
;	bset	#5,R0		;enable i2s interrupt
;	bset	#4,R0		;enable cpu interrupt
;
	store	R0,(R29)	;we're in bank 1 now, with i2s goin'
	nop
	nop
;
;
;   Come back up here for new command
;
;
	moveq	#0,R0		;sucessful completion code (1 if time out)
foreloop:
	shlq	#16,R0		;move completion code to high word
;
;  Here's where we load up the MD5 constants..
;
	movei	#hashend,hashendR
	movei	#hashman,hashptr
;
	moveq	#0,abit6		;we need bit6 set for xor
	moveta	abit6,R19		;set time-out counter to zero

	bset	#6,abit6
	movei	#$F1C000,blckbase	;start off using low buffer
;
	moveta	blckbase,R26		;restore ptr
;
	moveq	#31,mask31		;$1f, 5 bits of LSBs for mask
	move	mask31,mask63
	add	mask63,mask63
;
	movei	#$67452301,regAx	;init state reg's
	movei	#$EFCDAB89,regBx	; to constants
	movei	#$98BADCFE,regCCx
	movei	#$10325476,regDx
;
;
	movei	#nextrans,nextserR
	movei	#nexti2s,nextblkR	;assume we want i2s input
;
;-----------------------------------------------
;
;  Time to wait for new command
;
	movei	#MD5semi,loopcnt
	not	R0
	store	R0,(loopcnt)		;tell 'em we're thru
;
cmdwait:
	load	(loopcnt),command
	or	command,command
	jr	MI,cmdwait	;wait til non-negative comes in
	nop
;
;  commands are: 0 - perform MD5 on-the-fly segment CD reads
;		 1 - perform MD5 on dRAM or sRAM range
;		 2 - load boot sector
;		 3 - shut the DSP down
;
	addq	#4,loopcnt	;ptr to head
	load	(loopcnt),R15	;head pattern (or memory addr)
	addq	#4,loopcnt
	load	(loopcnt),inptptre	;length in blocks (or memory addr)
;
	cmpq	#2,command
	jr	CS,doMD			;br if 0 or 1
	nop
;
	movei	#shutboot,nextblkR	;need to check shutdown or boot
	jump	(nextblkR)		; (commands 2, 3)
	nop

doMD:
	movei	#justmem,RegA	;assume br to memory hash only
	cmpq	#1,command	;was it memory or circular I2S?
	jump	EQ,(RegA)	;br if memory
	nop
;
;
;  else, start up the circular que business
;
cirqdue:
;
;  first, let's wait for "FOUND" from DSA
;
;
; ***  3-May-95  Addition added so DSP waits for ACK after seek
;
	movei	#$DFFF00,RegA	;ptr to BUTCH
;
reACK:
	movei	#5000,RegB	;delay timer
ACKdelay:
	subq	#1,RegB
	jr	NE,ACKdelay
	nop
;
	load	(RegA),RegB	;get BUTCH
	btst	#13,RegB	;check DSA RX pending
	jr	EQ,reACK
	nop
;
	addq	#$A,RegA	;--> DS_DATA
	loadw	(RegA),RegB	;fetch return code

;
;****22-May-95 we don't clear DSA_rx flag since it might also clear DSA_tx
;	subq	#6,RegA		;--> DSCNTRL
;
;	load	(RegA),RegCC	;clear DSA Rx pending
;


	movei	#$100,RegA	;get "FOUND"
	or	RegCC,RegCC	;fix scoreboard bug for dummy load
	cmp	RegA,RegB	;did we get found?
	jr	EQ,foundgot	;br if so
	nop
;
;  we have a seek error...
;
	movei	#foreloop,command
	jump	(command)
	moveq	#2,R0		;indicate seek error
;
;
foundgot:
	movei	#$DFFF10,RegA
	load	(RegA),RegB	;clear any CD Err
	or	RegB,RegB	;fix scoreboard bug for dummy load
;
;  *** End 3-May-95 addition
;
	move	inptptre,index
	shlq	#4,index
	addq	#1,index
	moveta	index,R24	;set length in longs
	moveta	R15,R25		;set start pattern
;
	movei	#firstI2S,command
	jump	(command)
	moveq	#0,R15
;
;
;
;
justmem:
	movei	#nextblk,nextblkR
;
;
;=========================================================
;
;   Load next 64-bytes of Kart in our local RAM and
;    futz, grind, hack and ickify it.
;
nextblk:
	move	hashptr,hashcur
	move	hashendR,scratch1
;
;   The following code copies a 64-byte block of Kart into
;    GPU space.  If each ROM block is always long-word aligned,
;    then the simple routine will suffice.  Else, the other
;    is required.
;
;  Use this for long-word aligned SRC..
;   
	moveq	#16,loopcnt
locallp:
	load	(R15),accum
	addq	#4,R15
	store	accum,(scratch1)
	subq	#1,loopcnt
	jr	NE,locallp
	addqt	#4,scratch1
;
	movei	#nextboth,loopcnt
	jump	(loopcnt)
;	jr	nextboth		;skip over i2s nextblock setup
	move	hashendR,R14	
;
;
;
;  First time, just wait for pointer to start moving (=0 if time-out)
;
firstI2S:
	movefa	R26,index
tmwait:
	cmp	blckbase,index		;has pointer started to move?
	jr	EQ,tmwait		;br if not
	movefa	R26,index
;
; it's moving--check for time-out
;
	or	index,index		;this will be zero'd if time-out
	jr	NE,nextnext		;br if not timed out
	nop
;
	movei	#foreloop,command	;else, we're done--timeout
	jump	(command)
	moveq	#1,R0			;indicate time-out
;
;  here is our loop point if reading i2s
;
nexti2s:
	movefa	R26,index		;fetch i2s routine's block ptr
nextnext:
	xor	blckbase,index		;i2s must not be filling the buffer
	btst	#6,index		;     we want to use for hashing
	jr	EQ,nextnext
	movefa	R26,index
;
	subq	#1,inptptre
;
;  opposite buffer being filled, go hash ours
;
	move	blckbase,R14
	xor	abit6,blckbase	;set for alternate block next time around
nextboth:
	move	regAx,regA
	move	regBx,regB
	move	regCCx,regCC
	move	regDx,regD
;
;
nextrans:
	load	(hashcur),speciesR	;load up function addr
	addq	#4,hashcur
;
	load	(hashcur),indexinc	;load up index increment,init
	addq	#4,hashcur
	move	indexinc,index
	and	mask31,index
	rorq	#16,indexinc
	and	mask31,indexinc
;
	load	(hashcur),rolcnt	;get 4 packed ROR counts
	addq	#4,hashcur
;		
	jump	(speciesR)
	moveq	#16,loopcnt		;jump in at 1 of 4 functions
;
;
futz:
;  accum = (regB & regCC) | (~regB & regD)
	move	regB,accum
	move	regB,scratch1
	and	regCC,accum
	not	scratch1
	jr	endspec1
	and	regD,scratch1
;	jr	endspec
;	or	scratch1,accum
;
grind:
;  accum = (regB & regD) | (regCC & ~regD)
	move	regD,accum
	move	regD,scratch1
	and	regB,accum
	not	scratch1
	and	regCC,scratch1
endspec1:
	jr	endspec
	or	scratch1,accum
;
hack:
;  accum = regB ^ regCC ^ regD
	move	regD,accum
	xor	regB,accum
	jr	endspec
	xor	regCC,accum
;
ickify:
;  accum = regCC ^ (regB | ~regD)
	move	regD,accum
	not	accum
	or	regB,accum
	xor	regCC,accum
;
;
;
endspec:
	load	(R14+index),scratch1
	add	indexinc,index
	add	scratch1,accum
;
	load	(hashcur),scratch1
	and	mask63,index
	add	scratch1,accum
	addq	#4,hashcur
	add	regA,accum
;
	move	rolcnt,temp
	rorq	#8,rolcnt
	and	mask31,temp	
	ror	temp,accum
	add	regB,accum
;
	subq	#1,loopcnt
;
	move	regD,regA
	move	regCC,regD
	move	regB,regCC
;
	jump	NE,(speciesR)		;do all 16, for this function
	move	accum,regB
;
	cmp	hashcur,hashendR	;do all 4 functions
	jump	NE,(nextserR)
	nop
;
	add	regA,regAx
	add	regB,regBx
	add	regCC,regCCx
	add	regD,regDx
;	
	cmp	inptptre,R15		;have we reached end of Kart?
	jump	CS,(nextblkR)		;br if not
	move	hashptr,hashcur
;
;  We have computed the MD5 for the address range specified.
;   Now overwrite the code at "overhash" with the results
;
wedone:
	movei	#MD5result,speciesR	;stuff results here and quit
;
	store	regAx,(speciesR)
	addq	#4,speciesR
;
	store	regBx,(speciesR)
	addq	#4,speciesR
;
	store	regCCx,(speciesR)
	addq	#4,speciesR
;
	store	regDx,(speciesR)
;
	movei	#foreloop,command		;go wait for another command
	jump	(command)
	moveq	#0,R0				;set for sucessful completion
;
;
;  We got either a shutdown or BOOT load command...
;
shutboot:
	cmpq	#3,command	;3 = shutdown
	jr	EQ,shutdown
	nop
;
;   Must be 2 -- do bootload
;
	movei	#DOBOOT,command
	jump	(command)
	nop
;
;
shutdown:
	movei	#D_FLAGS,R29	;just in case
	load	(R29),R0	;get D_FLAGS
	bclr	#5,R0		;kill i2s interrupt
	bclr	#4,R0		;kill cpu interrupt
	bclr	#14,R0		;reg set is bank 0
	store	R0,(R29)
	nop
	nop
;
	movei	#D_CTRL,R0
	moveq	#0,R1

;	moveq	#0,R2
;	movei	#SMODE,R29
;	store	R2,(R29)

realdone:
	store	R1,(R0)
	jr	realdone
	nop
;
;
;
;-----------------------------------------------------------------
;
;  Read in the boot sector
;
DOBOOT:
	movei	#($981E0000+(I2S_BOOT & $FFFF)),R0	;secondary irq
	moveta	R0,R22
;
	moveq	#16,R0
	moveta	R0,R23		;make sure we look for 16 longs
;
	movei	#$41545249,R0	;look for 16 longs of "ATRI"
	moveta	R0,R25		;start the unit up
;
bootfin:
	movefa	R25,R0
	or	R0,R0
	jr	NE,bootfin
	nop
;
	movefa	R18,R0
	movei	#MD5result,R1
	store	R0,(R1)		;tell 68k the start addr
	addq	#4,R1
	movefa	R15,R0		;get length too for grins
	store	R0,(R1)
;
;	movei	#MD5semi,R1
;	moveq	#0,R0
;	not	R0
;	store	R0,(R1)
;
;	movei	#($981E0000+(i2s_shut & $FFFF)),R22	;secondary irq
;	movefa	R21,R21
;	store	R22,(R21)
;
	movei	#foreloop,command
	jump	(command)
	moveq	#0,R0		;indicate no time-out
;
;
;
;
;
;
;______________________________________________________________________________
;
; interrupt service routines
;
;
;
;		HASH-ON-THE-FLY				BOOTLOAD
;
;   R30 irq temp (D_FLAGS)				"
;   R29 -> D_FLAGS ptr contant				"
;   R28 -> LTXD ptr contant				"
;   R27 -> RTXD ptr constant				"
;   R26 -> DST ptr (circular queue)/0 if time-out	dRAM DST ptr 
;   R25 -> start pattern (=0 if none)			"
;   R24 -> long count					long count
;   R23 -> pattern count (init to 16)			" + offset to data
;   R22 -> "i2s_end"
;   R21 -> (i2s_vector)
;   R20 -> time-out constant (=$2000 longs before pattern is found)
;   R19 -> time-out counter (init to 0)
;
;  This is the idler routine to use when waiting for a start pattern
;
i2s_start:
	load	(R29),R30		;get D_FLAGS in R30
	load	(R28),R16		;get left channel
	load	(R27),R17		;get rite channel
	shlq	#16,R16
	shlq	#16,R17
	shrq	#16,R16
	or	R25,R25			;check for null pattern 
	jr	EQ,staroff		;br if we're not waiting
	or	R17,R16			;form R16, our input longword
;
;  we're looking for a non-zero header pattern (marker)
;
	cmp	R16,R25			;are we matching desired pattern?
	jr	NE,repat		;br if not
	addq	#1,R19			;advance time-out count
;
	subq	#1,R23			;count off 16 marker patterns
	jr	NE,staroff		;br if not all in yet
	nop
;
;  We got 16 longs matching our pattern
;
	or	R23,R23
	store	R22,(R21)		;use secondary I2S routine next
;
	jr	staroff			;reload with 8 not 16
	moveq	#8,R23			;we need in case this is boot
;
repat:
	moveq	#16,R23
staroff:
	cmp	R20,R19			;check for time-out
	jr	CS,starxit		;br if not a time-out
	nop
;
;  we timed-out.. tell foreground we're done 
;
	moveq	#0,R25			;we're done
	moveq	#0,R26			;set time-out indication
starxit:
	bclr	#3,R30			;exit i2s routine
	load	(R31),R17
	bset	#10,R30
	addq	#2,R17
	addq	#4,R31
	jump	(R17)
	store	R30,(R29)
;
;
;  This is the secondary routine to use for HASH-ON-THE-FLY
;  Circular queue is filled until length in R24 is reached  
;
i2s_end:
	load	(R29),R30		;get D_FLAGS in R30
	load	(R28),R16		;get left channel
	load	(R27),R17		;get rite channel
	shlq	#16,R16
	shlq	#16,R17
	shrq	#16,R16
	or	R17,R16			;else form R16, our input longword
;
	subq	#1,R24			;decrement length
	store	R16,(R26)		;save data in cicular queue
	jr	NE,endoff		;reached end?
	addqmod	#4,R26			;advance circular queue ptr
;
	moveq	#0,R25			;this is for i2s_start
;
	movei	#($981E0000+(i2s_start & $FFFF)),R16	;secondary irq
	store	R16,(R21)
endoff:
	bclr	#3,R30			;exit i2s routine
	load	(R31),R17
	bset	#10,R30
	addq	#2,R17
	addq	#4,R31
	jump	(R17)
	store	R30,(R29)
;
;	
;
;  This is the secondary routine after 16 "ATRI"'s have been found
;   to pass over the 8 longs of "ATARI APPROVED .."
;
;
I2S_BOOT:
	load	(R29),R30		;get D_FLAGS in R30
	load	(R28),R16		;get left channel
	subq	#1,R23			;count off 8 longs before we go
	jr	NE,I2done
	load	(R27),R17		;get rite channel
;
	movei	#($981E0000+(I2S_BOOX & $FFFF)),R24	;tertiary irq
	store	R24,(R21)
	moveq	#0,R26			;set DST ptr to 0 (indicating not set)
I2done:
	bclr	#3,R30			;exit i2s routine
	load	(R31),R17
	bset	#10,R30
	addq	#2,R17
	addq	#4,R31
	jump	(R17)
	store	R30,(R29)
;
;
;  This is the tertiary routine after 8 longs of "ATARI APPROVED.." have
;   been passed over.  R26 =
;			 0 on entry
;		- DST ADDR  right after DST addr is read (waiting for length)
;
;
I2S_BOOX:	
	load	(R29),R30		;get D_FLAGS in R30
	load	(R28),R16		;get left channel
	load	(R27),R17		;get rite channel
	shlq	#16,R16
	shlq	#16,R17
	shrq	#16,R16
	or	R17,R16			;else form R16, our input longword
;
	or	R26,R26			;have we loaded DST ptr yet?
	jr	EQ,loadDST		;br if not yet
	nop
;
	jr	PL,loadBOOT		;if legit ptr, br to load
	nop
;
	move	R16,R24		;  & save current input as length
	move	R16,R15		; save length because we want to know it later
	jr	exitboo
	subq	#1,R24		;adjust length so terminate logic works
;
loadDST:
	move	R16,R26		;set start DST ptr
	move	R16,R18		;save start DST ptr for use by foreground & 68k
exitboo:
	jr	exitboot	
	not	R26		;tell them we need length next
;
;
;
loadBOOT:
	or	R24,R24			;ready to terminate?
	jr	PL,stoboot		;br if not
	nop
;
	movei	#($981E0000+(i2s_start & $FFFF)),R22	;secondary irq
	store	R22,(R21)
	jr	exitboot
	moveq	#0,R25			;pattern=0 means we're done
stoboot:
	store	R16,(R26)		;else, save data to deired location
	addq	#4,R26			;advance DST ptr
	subq	#4,R24			;and decrement length
exitboot:
	bclr	#3,R30			;exit i2s routine
	load	(R31),R17
	bset	#10,R30
	addq	#2,R17
	addq	#4,R31
	jump	(R17)
	store	R30,(R29)
;
;
i2s_shut:
	movei	#%100011111000000000,R30
	store	R30,(R29)
	movei	#D_CTRL,R17
	moveq	#0,R30
waitshut:
	nop
	store	R30,(R17)
	jr	waitshut
	nop
;
;
;
;
sramout:
	load	(R29),R30
	movei	#sramo,R7
	load	(R7),R6		;get SRC
	addq	#4,R7
	load	(R7),R8		;get DST
	addq	#4,R7
	load	(R7),R9		;get count
	addq	#4,R7
;
fetchy:
	load	(R6),R10
	addq	#4,R6
	store	R10,(R8)
	subq	#1,R9
	jr	NE,fetchy
	addq	#4,R8
;
	store	R9,(R7)
;
	bclr	#3,R30			;exit cpu routine
	load	(R31),R7
	bset	#9,R30
	addq	#2,R7
	addq	#4,R31
	jump	(R7)
	store	R30,(R29)
;
;
;
	ALIGN LONG
;
;  This is used by the cpu burp to copy a block to dRAM
;
sramo:
	dc.l	MD5result	;SRC
	dc.l	$4000		;DST
	dc.l	4		;count
;
	dc.l	-1		;cpu irq semiphore
;	ALIGN	LONG
;
;	
;	align	long
;
;  Constant data goes here..
;
;
hashman:
	dc.l	futz				;function loop addr
	dc.w	4,0				;index increment, initial
	dc.b	(32-22),(32-17),(32-12),(32-7)	;left shift counts 
;

	dc.l	$D76AA478
	dc.l	$E8C7B756
	dc.l	$242070DB
	dc.l	$C1BDCEEE
	dc.l	$F57C0FAF	
	dc.l	$4787C62A
	dc.l	$A8304613
	dc.l	$FD469501
	dc.l	$698098D8
	dc.l	$8B44F7AF
	dc.l	$FFFF5BB1
	dc.l	$895CD7BE
	dc.l	$6B901122
	dc.l	$FD987193
	dc.l	$A679438E
	dc.l	$49B40821
;
;
;
;
;
	dc.l	grind
	dc.w	20,4				;index increment, initial
	dc.b	(32-20),(32-14),(32-9) ,(32-5)	;left shift counts
;
	dc.l	$F61E2562
	dc.l	$C040B340
	dc.l	$265E5A51
	dc.l	$E9B6C7AA
	dc.l	$D62F105D
	dc.l	$02441453
	dc.l	$D8A1E681
	dc.l	$E7D3FBC8
	dc.l	$21E1CDE6
	dc.l	$C33707D6
	dc.l	$F4D50D87
	dc.l	$455A14ED
	dc.l	$A9E3E905
	dc.l	$FCEFA3F8
	dc.l	$676F02D9
	dc.l	$8D2A4C8A
;
;
;
	dc.l	hack
	dc.w	12,20				;index increment, initial
	dc.b	(32-23),(32-16),(32-11),(32-4)	;left shift counts
;
	dc.l	$FFFA3942
	dc.l	$8771F681
	dc.l	$6D9D6122
	dc.l	$FDE5380C
	dc.l	$A4BEEA44
	dc.l	$4BDECFA9
	dc.l	$F6BB4B60
	dc.l	$BEBFBC70
	dc.l	$289B7EC6
	dc.l	$EAA127FA
	dc.l	$D4EF3085
	dc.l	$04881D05
	dc.l	$D9D4D039
	dc.l	$E6DB99E5
	dc.l	$1FA27CF8
	dc.l	$C4AC5665
;
;
;
	dc.l	ickify
	dc.w	28,0				;index increment, initial
	dc.b	(32-21),(32-15),(32-10),(32-6)	;left shift counts
;
	dc.l	$F4292244
	dc.l	$432AFF97
	dc.l	$AB9423A7
	dc.l	$FC93A039
	dc.l	$655B59C3
	dc.l	$8F0CCC92
	dc.l	$FFEFF47D
	dc.l	$85845DD1
	dc.l	$6FA87E4F
	dc.l	$FE2CE6E0
	dc.l	$A3014314
	dc.l	$4E0811A1
	dc.l	$F7537E82
	dc.l	$BD3AF235
	dc.l	$2AD7D2BB
	dc.l	$EB86D391
;
hashend:
		
