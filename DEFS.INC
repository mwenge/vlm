;*======================================================================*
;* TITLE	: DEFS.INC						*
;* Function	: Common Definitions					*
;*									*
;* Project #	: JAGUAR						*
;* Programmer	: Andrew J. Burgess					*
;*									*
;* MODIFIED	: Pradip K Fatehpuria	10/27/93			*
;*									*
;* COPYRIGHT(c) 1992,1993 Atari Computer Corporation			*
;* UNATHORIZED REPRODUCTION, ADAPTATION, DISTRIBUTION, PERFORMANCE OR	*
;* DISPLAY OF THIS COMPUTER PROGRAM OR THE ASSOCIATED AUDIOVISUAL WORK	*
;* IS STRICTLY PROHIBITED.						*
;* ALL RIGHTS RESERVED.							*
;*									*
;*======================================================================*

;*======================================================================*
;*	GLOBAL DEFINES							*
;*======================================================================*

OFF		EQU	0
ON		EQU	1

FALSE		EQU	0
TRUE		EQU	1

;*======================================================================*

DRAM		EQU	$000000		; start of system RAM
ENDRAM		EQU	$200000		; end of system RAM
STACK		EQU	DRAM+$5000-$4	; start of system stack

NUMFACE		EQU	$6		; offset into model pointer for
					; number of faces data

FACELIST	EQU	$10		; offset into model pointer for
					; facelist address

BUFFERSIZE	EQU	(320*240*2)	; screen 320x240 in 16.16 color mode

RAMSIZE		EQU	$1000		; 4K bytes

INPUT6		EQU	G_ENDRAM-$4	; pointer to parameter 6 for GPU
INPUT5		EQU	INPUT6-$4	; pointer to parameter 5 for GPU
INPUT4		EQU	INPUT5-$4	; pointer to parameter 4 for GPU
INPUT3		EQU	INPUT4-$4	; pointer to parameter 3 for GPU
INPUT2		EQU	INPUT3-$4	; pointer to parameter 2 for GPU
INPUT1		EQU	INPUT2-$4	; pointer to parameter 1 for GPU

VDI27		EQU	G_ENDRAM-$4
VDI26		EQU	VDI27-$4
VDI25		EQU	VDI26-$4
VDI24		EQU	VDI25-$4
VDI23		EQU	VDI24-$4
VDI22		EQU	VDI23-$4
VDI21		EQU	VDI22-$4
VDI20		EQU	VDI21-$4
VDI19		EQU	VDI20-$4
VDI18		EQU	VDI19-$4
VDI17		EQU	VDI18-$4
VDI16		EQU	VDI17-$4
VDI15		EQU	VDI16-$4
VDI14		EQU	VDI15-$4
VDI13		EQU	VDI14-$4
VDI12		EQU	VDI13-$4
VDI11		EQU	VDI12-$4
VDI10		EQU	VDI11-$4
VDI9		EQU	VDI10-$4
VDI8		EQU	VDI9-$4
VDI7		EQU	VDI8-$4
VDI6		EQU	VDI7-$4
VDI5		EQU	VDI6-$4
VDI4		EQU	VDI5-$4
VDI3		EQU	VDI4-$4
VDI2		EQU	VDI3-$4
VDI1		EQU	VDI2-$4

;*======================================================================*
;*	RGB COLORS	 GGRRXXBB					*
;*======================================================================*

RGB_BLACK	EQU	$00000000
RGB_BLUE	EQU	$000000FF
RGB_GREEN	EQU	$FF000000
RGB_RED 	EQU	$00FF0000
RGB_CYAN	EQU	$FF0000FF
RGB_YELLOW	EQU	$FFFF0000
RGB_MAGENTA 	EQU	$00FF00FF
RGB_WHITE	EQU	$FFFF00FF

RGB_AQUA	EQU	$80000080
RGB_RUST	EQU	$00800000
RGB_VIOLET	EQU	$00800080
RGB_BROWN	EQU	$50800000
RGB_ORANGE	EQU	$80FF0000
RGB_DKGREEN	EQU	$30080000
RGB_LTGREEN	EQU	$FFB000B0
RGB_LTGREY	EQU	$B0B000B0
RGB_GREY	EQU	$80800080
RGB_DKGREY	EQU	$50500050

;*======================================================================*

R60Hz		EQU	1
R30Hz		EQU	2
R20Hz		EQU	3
R10Hz		Equ	4

;*======================================================================*
;*	GENERIC DEFINES							*
;*======================================================================*

SCRNORM		EQU	$4C1		; 320x200 normal pixels
SCROVER		EQU	$6C1		; 320x200 overscan square pixels

;*======================================================================*
;*	CPU INTERRUPT LOCATIONS						*
;*======================================================================*

LEVEL2		EQU	4*26		; 68000 Level2 Autovector ($68)
LEVEL0		EQU	$100		; 68000 Level0 Autovector (9*26)

;*======================================================================*
;*	MACROS								*
;*======================================================================*

.macro	TIMEBAR		color
.if	BAR_ON != 0
	move.l	#\color,BORD1
.endif
.endm

.macro	LONGALIGN	addr
	addq.l	#3,\addr		; make sure the address will
	andi.l	#$FFFFFFFC,\addr	; remain within the buffer area
.endm


;*======================================================================*
;* EOF									*
;*======================================================================*
