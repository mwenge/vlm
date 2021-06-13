;**************************************************************************
; (C)1993 ATARI CORP.       SECRET & CONFIDENTIAL       ALL RIGHTS RESERVED
;
;
;                                echodat.gas
;
;       This program uses the i2s external sample word clock to generate
;	an interrupt, set up by the calling 68K host program, to time data
;	moves from the i2s port input to i2s output; a simple loopthru.
;
;
;                                REVISION HISTORY
;
; REV.  DATE       BY            DESCRIPTION OF EDIT
; """"  """"       ""            """""""""""""""""""
; 1.00  15 Sep 93  DMS	Based on loop.gas, used by butch.415
; 2.00  12 Nov 93  DMS	Add shutdown semaphore, per bugfix
;**************************************************************************
	.include	"jaguar.inc"
					; addresses of...
;LTXD		equ	$F1A148		; left TX i2s
;RTXD		equ	$F1A14C		; right TX
;LRXD		equ	$F1A148		; left receive data from i2s
;RRXD		equ	$F1A14C		; right " "
;DSP_FLAGS	equ	$F1A100		; flag register
;
STACKPOS	equ	$F1CF00		; stack in DSP SRAM
DSP_stop	equ	$F1C310		; Stop-DSP semaphore from M68K

ORG	$F1B000				; put this program at start of DSP SRAM

DSPORG:
	movei	d_start,r30		; (interrupt 0  - DSP Control)
	jump	(r30)
	nop
	nop
state:
	dc.l	1	;if set: b2=shutdown, b1=save data, b0=play
;
;
;
	movei	#i2s_isr,r30		; (interrupt 1 - I2S)
	jump	(r30)
	nop
	nop
pointer:
	dc.l	0

d_start:
	movei	#pointer,r0		;pointer to pointer to save buffer
	movei	#state,r26		;pointer to state long
	movei	#LTXD,r27		;global use constants
	movei	#RTXD,r28
	movei	#D_FLAGS,r29
	load	(r0),r25		;pointer to save buffer
;
	movei	#STACKPOS,r31

	movei	#D_HIDATA,r0
	movei	#$ffff0000,r1		;set mod mask
	store	r1,(r0)			; to 64k
;
	load	(r29),r0
	bset	5,r0		; enable I2S interrupt
	store	r0,(r29) 	; save dsp flags

fake_it:			; a dummy loop to encapsulate the ISR
	jr	fake_it
	nop
;
;
;
;
				; Sample pair-rate ISR
i2s_isr:
	load	(r29),r30	; load DSP_FLAGS flags
	load	(r27),r0	; fetch left channel input data
	load	(r28),r1	; fetch right channel input data
;
	store	r0,(r27)	;play left channel
	store	r1,(r28)	;play right channel

clean_up:			; do the housekeeping, per Leonard
	bclr	#3,r30		; clear IMASK
	bset	#10,r30		; set I2S interrupt clear bit
	load	(r31),r0	; get last instruction address
	addq	2,r0		; point at next to be executed
	addq	4,r31		; update the stack pointer
	jump	(r0)		; and return
	store	r30,(r29)	; restore flags
;
;
;
;
;
shutd:				; do this to stop the damn thing
	movei	#$DEADC0DE,r9	; breadcrumb
	movei	0,r1
	movei	#$F1A154,r0	; turn off i2s port
	store	r1,(r0)
	movei	#$23E00,r1
	movei	#$F1A100,r0	; kill interrupts
	store	r1,(r0)
	movei	0,r1
	movei	#$F1A114,r0	; and halt it
	store	r1,(r0)

verydead:			; if the DSP is not stopped now
	nop			; try turning the power off
	jr	verydead
	nop
	nop
	end                                        

