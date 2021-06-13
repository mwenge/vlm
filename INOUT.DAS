	PLEN	95
;{{{  Atari's title
;**************************************************************************
; (C)1994 ATARI CORP.       SECRET & CONFIDENTIAL       ALL RIGHTS RESERVED
;
;                       Code to read CD data in thru Jerry
;                               A counter is stored at
;                               $F1B000 to show capacity
;
;                       This code will keep up with a 8x oversample rate
;
;**************************************************************************
;}}}  
;{{{  (c) vlc
; FFT code. Copyright The Virtual Light Company Limited 1994
; All rights reserved.
; Written Ian Bennett, April / May 1994
;}}}  
;{{{  fft notes
;{{{  Interpreting fft output bins
; Interpreting fft output bins :
; There are 127 useful bins, though the 2nd half seem to mirror the first half,
;  so use the 1st 63 - don't use the first one.
; These are found at address $0f1b034 onwards (buffer starts at $0f1b030 but
;  don't use the 1st bin - it's meaningless).
; Each bin is a 32-bit +ve integer representing the magnitude.
; What these numbers mean, is that if there's a huge number in bin (index) n,
;  (huge wrt the other numbers), then this represents a frequency f where
;  f = (sample_freq / no_samples) * n
; The sample freq is the frequency of the no_samples samples as seen by the FFT.
; For example, at full cd rate of 44 KHz (no_samples_per == 1)
;  base freq = 44 000 / 256 = 172 Hz
;  so a huge number in bin 10, say, would indicate a dominant frequency of
;  about 1.7 KHz.
; The full CD sample rate is too quick, though, because this would give you
;  a bin 1 of 171 Hz, and all the base beats would be in the 1st 2 or 3 bins.
; So what the FFT sees is a subsample rate of the full CD rate, controlled
;  by the constant "no_samples_per". Setting this to 1 would select the full
;  CD rate, but setting it to values around 10 to 20 make bin 1 a reasonable
;  value.
; CD rate = 44.1 KHz or 8 * this ???
;}}}  
;{{{  FFT modes.
; FFT modes.
; These are controlled by the variable "fft_mode", surprise surprise !
;  mode = 0 -> average left & right (mono)
;         1 -> continually alternate between l & r (stereo)
;         2 -> left all the time
;         3 -> right all the time
;}}}  
;{{{  Tuning
; Tuning
; It is possible to tune the FFT frequency response by altering the
;  variable "no_samples_per", as mentioned above. Try it. 10 seems to work
;  fairly well - but I'm not sure what frequency we're seeing,
;  so can't be sure.
; You can also tune the input level (sensitivity) to the FFT by altering
;  the sharq value of the r and l values.
;  Remember that the sharq for mono (average) must be one more than that
;  for r or l.
;}}}  
; Ian Bennett. Copyright VLC Ltd 1994. All Rights Reserved.
;}}}  
;{{{  Ians_screen_mode equ
Ians_screen_mode equ 0 ; set to != 0 for screen plotting stuff
                       ; set to 0 for not
;}}}  
;{{{  equ
; buffers are now at the start of DSP ram (after intr vectors & 2 vars)
; mag_buf_st  == $0f1b000 + 32 + 8                         == $0f1b030
; real_buf_st == $0f1b000 + 32 + 8 + (128 * 4)             == $0f1b230
; imag_buf_st == $0f1b000 + 32 + 8 + (128 * 4) + (256 * 4) == $0f1b630
no_passes equ 8 ; hardwired
no_passes_plus_1 equ 9 ; hardwired
no_samples_per equ 7 ; say (this controls how many samples the fft skips
                ; NOTE no_samples_per must be less than 32 (uses moveq)
no_samples equ 256 ; hardwired
no_samples_div2 equ 128
mirror_shift equ 22 ; 32 - no_passes - 2 for INT32 offset
;}}}  
;{{{  regequ
;{{{  isr stuff (always 31 - 22, sampling 21 - 20)
; ones req'd all the time
isr_stack_addr    regequ r31
isr_jump_reg      regequ r30 ; these two overlap !
isr_fft_datal     regequ r30 ; (treated carefully in code)
isr_fft_datar     regequ r29
isr_cd_inpl       regequ r28
isr_flags         regequ r27
isr_flags_addr    regequ r26
isr_volume        regequ r25 ;
isr_fft_temp_addr regequ r25 ;
isr_volume_addr   regequ r24
isr_cd_data       regequ r23 ; as above, _mode not used 'til _data not wanted
isr_fft_mode      regequ r23 ;
isr_no_samples_to_get  regequ r22
; ones only req'd when sampling
isr_sample_delay_count regequ r21 ; overlap regs used in fft, but these
isr_target_addr   regequ      r20 ; are mutually exclusive operations
;}}}  
;{{{  Ian's regequ
;{{{  fft guts regs r4 - r18 (- r14, r15)
fft_m  regequ  r4 ; left r0 - r3 for general purpose use
fft_m1 regequ  r5
fft_k  regequ  r6
fft_u1 regequ  r7
fft_u2 regequ  r8
fft_w1 regequ  r9
fft_w2 regequ r10
fft_j  regequ r11
fft_l2 regequ r12
fft_l1 regequ r13
fft_t1 regequ r16 ; using r14, r15 for indexing
fft_t2 regequ r17
fft_u3 regequ r18
;}}}  
;{{{  fundamental regs r19 - r21
real_buf         regequ r19
imag_buf         regequ r20 ; see notes in isr_regs above
mag_buf          regequ r21 ;
;}}}  
;{{{  scaling regs
size_limit       regequ r7 ; u1
no_left_to_check regequ r8 ; u2
do_scale_addr    regequ r9 ; w1
;}}}  
;{{{  plotting graph regs r4 - r21 (-..-)
plot_pixel_proc regequ  r4
vert_line_proc  regequ  r21
index_inc       regequ  r20
index           regequ  r5 ; keep at r5, used above
value           regequ  r6
largest_value   regequ  r7
x_coord         regequ  r8
y_coord         regequ  r9
scr_width       regequ  r10
scr_height      regequ r11
no_to_plot      regequ r12
next_index_label regequ r13
return_address  regequ r16
max_height      regequ r17
height          regequ r18
stride          regequ r19
;
;
;
;===================================================
;
;  Dave's alternate registers used:
;
SAMPLE		regequ	R7	;sample data from last irq
SAMPCOUNT	regequ	R8	;wait this long before new sample
RESULTBASE	regequ	R9	;=$2800 (constant)
FINECRC		regequ	R10	;desired CRC
VOLREG		regequ	R11	;volume ptr
CLEANDAVE	regequ	R12	;"clean_up_dave"
DATATRK		regequ	R13	;copy of TPACK0 if crc validated
;
TPACK0		regequ	R17	;0..3 packed putative q-subcodes
TPACK1		regequ	R18	;4..7 packed putative q-subcodes
TPACK2		regequ	R19	;8..B packed putative q-subcodes
;
SUBSTAT		regequ	R20	;keep current index and status here
SCODEPTR	regequ	R21	;=$DFFF1A (constant)
CURCODE		regequ	R22	;last q-subcode retrieved
PACK4		regequ	R23	;packed longword with up to 4 subcodes
CURCRC		regequ	R24	;current CRC
RESULTPTR	regequ	R25	;result field computed from ADR ($28x0) 
CLEANUP		regequ	R26	;"clean_up" (constant)
XMDMTAB		regequ	R27	;=$2A00 (constant) xmdm CRC quick look-up table
CRCDONE		regequ	R28	;"crcdone" (constant)
FETCHER		regequ	R29	;"fetcher" (constant)
;
;=======================================================
;}}}  
;}}}  
;}}}  
;{{{  misc
	NOLIST
include         "jaguar.inc"
include		"INOUT.XDF"
	LIST
; addresses of...
STACKPOS        equ     $F1CF00         ; stack in DSP SRAM

;		XDEF    JERI_B
;		XDEF    VOLUME
ORG     $F1B97c                         ; put this program at start of DSP SRAM

;}}}  
;{{{  DSPORG:
;DSPORG:
realorg:
DSPORG	equ	$F1B000
        movei   JERI_B, isr_jump_reg               ; (interrupt 0  - DSP Control)
        jump    (isr_jump_reg)
        nop
        nop
        nop
        nop
;}}}  
;{{{  I2S_VEC:
;I2S_VEC:
I2S_VEC	equ	$F1B010
        movei   #i2s_isr, isr_jump_reg            ; (interrupt 1 - I2S)
        jump    (isr_jump_reg)
        nop
        nop
        nop
        nop
;}}}  
;{{{  vars & buffers
;{{{  4 vars
;fft_is_active:
fft_is_active	equ	$F1B020
	dc.l	1	;fft will start active
;scrbase:
scrbase		equ	$F1B024
	dc.l	0
;fft_mode:
fft_mode	equ	$F1B028
	dc.l	0	;mode will be mono
;fft_r_l:
fft_r_l		equ	$F1B02C
	dc.l	0
;
;   decl mag_buf as an array (128 INT32s)
mag_buf_st	equ	$F1B030
;}}}  
;{{{  decl real_buf as an array (256 INT32s)
real_buf_st	equ	$F1B230
;}}}  
;}}}  
;{{{  decl imag_buf as an array (256 INT32s)
imag_buf_st	equ	$F1B630
;}}}  
;}}}
startup:
	movei	#$F1B000,R10
	movei	#realorg,R1
 	moveq	#12,R2
coplp:
	load	(R1),R3
	addq	#4,R1
	store	R3,(R10)
	subq	#1,R2
	jr	NE,coplp
	addq	#4,R10
;
;
;  load up some constants in alt reg set
;
	moveq	#$10,R0
	moveta	R0,SUBSTAT
;
	movei	#$DFFF1A,R0
;	movei	#$f1c400,R0
	moveta	R0,SCODEPTR
;
	moveq	#0,R0
	moveta	R0,CURCRC
;
;	movei	#$2800,R0
	movei	#$f1c000,R0
;	movei	#$90000,R0
	moveta	R0,RESULTBASE
;
	movei	#clean_up,R0
	moveta	R0,CLEANUP
;
	movei	#$f1c400,R0
	moveta	R0,XMDMTAB
;
	movei	#crcdone,R0
	moveta	R0,CRCDONE
;
	movei	#fetcher,R0
	moveta	R0,FETCHER
;
	movei	#$1d0f,R0
	moveta	R0,FINECRC
;
	movei	#VOLUME,R0
	moveta	R0,VOLREG
;
	movei	#clean_up_dave,R0
	moveta	R0,CLEANDAVE
;
	moveq	#0,R0
	moveta	R0,SAMPCOUNT
;
	movei	#$40000000,R0
;	movei	#$00000000,R0
	moveta	R0,DATATRK
stopzero:
;
	moveq	#0,R2
	movei	#stopzero,R1
fill0:
	store	R2,(R10)
	addq	#4,R10
	cmp	R1,R10
	jr	CS,fill0
	nop
;
;
;    fall thru to Ian's code
;	
;}}}  
;{{{  JERI_B:
;{{{  init for isr
JERI_B:
;{{{  init fft sampling in isr
; init fft sampling in isr
	movei	#imag_buf_st,isr_target_addr

;*Dave	moveq	#no_samples_per,isr_sample_delay_count
	moveq	#no_samples_per,isr_volume_addr	;global delay count

	movei	#no_samples,isr_no_samples_to_get ; enable sampling
;}}}  
;
        movei   #STACKPOS, isr_stack_addr   ; Set up a stack
        movei   #D_FLAGS, isr_flags_addr ; get flags ptr
        movei   #LTXD, isr_cd_inpl ; address of i2s input port
;*Dave	movei   #VOLUME,isr_volume_addr	;we need this reg

        load    (isr_flags_addr), r0
        bset    5,r0            ; enable I2S interrupt
        store   r0,(isr_flags_addr)         ; save dsp flags
;}}}  
;}}}  
;{{{  loop forever (fft lives in here)
fft_then_sample_loop:
;{{{  wait for data acquisition complete
wait_for_256: ; wait 'til we've got 256 samples
or isr_no_samples_to_get, isr_no_samples_to_get ; == 0 for 256 samples,
                                                ; then disables acq
jr nz, wait_for_256 ; debug
nop
;}}}  
; data acquisition complete & disabled
;{{{  wait for ok from 68k
if Ians_screen_mode
wait_for_68k:
movei #fft_is_active, r0
load (r0), r1
cmpq #2, r1
jr nz, wait_for_68k
nop
endif
;}}}  
;{{{  set fft_is_active flag true
movei #fft_is_active, r0
moveq #1, r1
store r1, (r0)   ; set fft_is_active flag true
;}}}  
;{{{  fft and support code
;{{{  fft
;{{{  fft notes
; assume : at #no_ram is the no of samples (single pwr of 2)
;          NOTE this is now hardwired to 256
;          at #real_start is the address of the real buffer
;          and similarly at #imag_start & mag_start are the addrs of
;            these two buffers.
; the magnitude output is the average of the abs (r) and abs (i), as sqr
;  roots tend to be costly.
; All buffers are INT32, and are assumed to be of size no_samples * 4 bytes.
; Buffer starts are now in registers "real_buf" etc.
; Ian Bennett Copyright VLC Ltd 1994
;}}}  
;{{{  init fft controls
movei #real_buf_st, real_buf
movei #imag_buf_st, imag_buf
movei #mag_buf_st, mag_buf
;}}}  
;{{{  mirror 'imag_buf to real, clearing imag as we go go
;{{{  init
move real_buf, r15 ; dest
moveq #0, r0 ; index of source
movei #no_samples, r10 ; for end count compare
;}}}  
;{{{  loop
movei #mirror_data_loop, r3 ; for loop jump
mirror_data_loop:
move r0, r1 ; get index
mirror r1 ; mirror it
                        ; now shift it to the correct place as offset
shrq #mirror_shift, r1 ; where nn = 32 - (log2 (no_samples) + 2)
move imag_buf, r14 ; src
add r1, r14
load (r14), r2 ; mirrored posn data
store r2, (r15) ; put data to non-mirrored posn
moveq #0, r1 ; should be preloaded NOTE
store r1, (r14) ; clear imag as we go go
addq #4, r15 ; step to next data
addq #1, r0 ; step index
subq #1, r10
jump ne (r3)
nop
;}}}  
;}}}  
;{{{  do the thang, loop for m, pass
moveq #1, fft_m ; pass number
next_m_loop:
  movei #256, fft_u1;  u1 = (int) (1 * 256); /* 1 * 256, fixed point */
  moveq #0, fft_u2 ; u2 = (int) 0;
  ;{{{  m1 = 1 << m;
  move fft_m, r0
  moveq #1, fft_m1 ; m1 = 1 << m; could use a lut
  neg r0 ; +ve shifts to right
  sh r0, fft_m1 ; m1 = 1 << m;
  ;}}}  
  ;{{{  k = m1 >> 1;
  move fft_m1, fft_k ; k = m1 >> 1;
  shrq #1, fft_k
  ;}}}  
  ;{{{  w1 = (int) cos_table [m - 1];
  ;    w1 = (int) cos_table [m - 1];
  move fft_m, r0
  subq #1, r0
  movei #cos_table, r14
  shlq #2, r0 ; * 4 for INT32s
  add r0, r14
  load (r14), fft_w1
  ;}}}  
  ;{{{  w2 = (int) sin_table [m - 1];
  ;    w2 = (int) sin_table [m - 1];
  movei #sin_table, r14
  add r0, r14
  load (r14), fft_w2 ; r0 is still offset
  ;}}}  
  ;{{{  init r14, 15
  move real_buf, r14
  move imag_buf, r15
  ;}}}  
  ;{{{  loop for j, start of pair
  moveq #0, fft_j ; for (j = 0; j < k; j++)
    next_j_loop:
    move fft_j, fft_l2 ; for (l2 = j; l2 < no_values; l2 += m1)
      next_l2_loop:
      ;{{{  l1 = (l2 + k) as byte offset
      move fft_l2, fft_l1 ; l1 = l2 + k;
      add fft_k, fft_l1
      shlq #2, fft_l1 ; index -> INT32 byte offset
      ;}}}  
      ; note l1 is now offset, not index
      ;{{{  t1 = (((long) a [l1] * u1) - ((long) b [l1] * u2)) / 256;
      ;    t1 = (((long) a [l1] * u1) - ((long) b [l1] * u2)) / 256;
      ; here, r14 -> real
      ;       r15 -> imag
      load (r14 + fft_l1), r2 ; a [l1]
      load (r15 + fft_l1), r3 ; b [l1]
      ;{{{  do 24 bit * 16 bit (r2 * u1, r3 * u2)
      move r2, r0 ; tempa
      move r3, r1 ; tempb
      sharq #15, r0 ; keep top 17 bits & sign
      sharq #15, r1
      shlq #17, r2 ; lose top 17 bits & sign
      shlq #17, r3
      imult fft_u1, r0 ; r0 = top_16 * u1
      imult fft_u2, r1 ; r1 = top_16 * u2
      shrq #17, r2 ; shift back down, unsigned
      shrq #17, r3
      shlq #15, r0 ; sh back to posn
      shlq #15, r1
      imult fft_u1, r2 ; a [l1]lo_16 * u1
      imult fft_u2, r3 ; b [l1]lo_16 * u2
      add r0, r2 ; add on top bits
      add r1, r3
      ;}}}  
      sub r3, r2
      sharq #8, r2 ; / 256 - ib fixed point
      move r2, fft_t1
      ;}}}  
      ;{{{  t2 = (((long) b [l1] * u1) + ((long) a [l1] * u2)) / 256;
      ;    t2 = (((long) b [l1] * u1) + ((long) a [l1] * u2)) / 256;
      ; here, r14 -> real
      ;       r15 -> imag
      load (r15 + fft_l1), r2 ; b [l1]
      load (r14 + fft_l1), r3 ; a [l1]
      ;{{{  do 24 bit * 16 bit (r2 * u1, r3 * u2)
      move r2, r0 ; tempa
      move r3, r1 ; tempb
      sharq #15, r0 ; keep top 17 bits & sign
      sharq #15, r1
      shlq #17, r2 ; lose top 17 bits & sign
      shlq #17, r3
      imult fft_u1, r0 ; r0 = top_16 * u1
      imult fft_u2, r1 ; r1 = top_16 * u2
      shrq #17, r2 ; shift back down, unsigned
      shrq #17, r3
      shlq #15, r0 ; sh back to posn
      shlq #15, r1
      imult fft_u1, r2 ; a [l1]lo_16 * u1
      imult fft_u2, r3 ; b [l1]lo_16 * u2
      add r0, r2 ; add on top bits
      add r1, r3
      ;}}}  
      add r3, r2
      sharq #8, r2 ; / 256 - ib fixed point
      move r2, fft_t2
      ;}}}  
      ;{{{  precalc l2 offset into r3 as byte offset
      move fft_l2, r3 ; index
      shlq #2, r3
      ;}}}  
      ;{{{  a [l1] = a [l2] - t1;
      ;    a [l1] = a [l2] - t1;
      ; here, r14 -> real
      ;       r15 -> imag
      ;       r3 = l2 as offset
      load (r14 + r3), r0 ; a [l2]
      sub fft_t1, r0 ; a [l2] - t1
      store r0, (r14 + fft_l1) ; a [l1] = a [l2] - t1
      ;}}}  
      ;{{{  b [l1] = b [l2] - t2;
      ;    b [l1] = b [l2] - t2;
      ; here, r14 -> real
      ;       r15 -> imag
      ;       r3 = l2 as offset
      load (r15 + r3), r1 ; b [l2]
      sub fft_t2, r1 ; b [l2] - t2
      store r1, (r15 + fft_l1) ; b [l1] = b [l2] - t2
      ;}}}  
      ;{{{  a [l2] = a [l2] + t1;
      ; a [l2] = a [l2] + t1;
      ; here, r14 -> real
      ;       r15 -> imag
      ;       r3 = l2 as offset
      load (r14 + r3), r0 ; a [l2]
      add fft_t1, r0 ; a [l2] + t1
      store r0, (r14 + r3) ; a [l2] = a [l2] + t1
      ;}}}  
      ;{{{  b [l2] = b [l2] + t2;
      ; b [l2] = b [l2] + t2;
      ; here, r14 -> real
      ;       r15 -> imag
      ;       r3 = l2 as offset
      load (r15 + r3), r1 ; b [l2]
      add fft_t2, r1 ; b [l2] + t2
      store r1, (r15 + r3) ; b [l2] = b [l2] + t2
      ;}}}  
      ;{{{  loop l2 ?
      add fft_m1, fft_l2
      movei #next_l2_loop, r1
      move fft_l2, r0
      movei #no_samples, r2
      ;sub no_samples, r0
      sub r2, r0
      ;cmp no_samples, l2
      jump mi (r1) ; jmp if negative, ie l2 < no_values
      nop
      ;}}}  
    move fft_u1, fft_u3 ; u3 = u1
    ;{{{  u1 = (((long) u1 * w1) - ((long) u2 * w2)) / 256;
    ;    u1 = (((long) u1 * w1) - ((long) u2 * w2)) / 256;
    move fft_w1, r2
    move fft_w2, r1
    imult fft_u1, r2 ; u1 * w1
    imult fft_u2, r1 ; u2 * w2
    sub r1, r2 ; (u1 * w1) - (u2 * w2)
    sharq #8, r2 ; / 256 - ib fixed point
    move r2, fft_u1
    ;}}}  
    ;{{{  u2 = (((long) u2 * w1) + ((long) u3 * w2)) / 256;
    ;    u2 = (((long) u2 * w1) + ((long) u3 * w2)) / 256;
    move fft_u2, r2
    move fft_u3, r1
    imult fft_w1, r2 ; u2 * w1
    imult fft_w2, r1 ; u3 * w2
    add r1, r2 ; (u2 * w1) + (u3 * w2)
    sharq #8, r2 ; / 256 - ib fixed point
    move r2, fft_u2
    ;}}}  
    ;{{{  loop j ?
    addq #1, fft_j
    movei #next_j_loop, r1
    cmp fft_k, fft_j
    jump mi (r1) ; jmp if negative, ie j < k
    nop
    ;}}}  
  ;}}}  
  ;{{{  loop m ?
  addq #1, fft_m
  movei #next_m_loop, r1
  move fft_m, r0
  moveq #no_passes_plus_1, r2
  sub r0, r2
  ;cmp m, no_passes_plus_1 ; 8 + 1 ; n is the last + 1 pass
  jump nz (r1) ; jmp if m != 9, ie m <= 8
  nop
  ;}}}  
;}}}  
;{{{  mag := average (real, imag)
;{{{  get no_samples / 2
movei #no_samples, r3 ; counter
shrq #1, r3 ; only 1st 1/2 are useful
;}}}  
;{{{  init
move real_buf, r14
move imag_buf, r15
move mag_buf, r16
movei #fft_abs_loop, r2 ; jump reg
;}}}  
fft_abs_loop:
;{{{  get r & i
load (r14), r0
load (r15), r1
;}}}  
;{{{  abs
abs r0
abs r1
;}}}  
addq #4, r14
add r0, r1 ; real [n] + imag [n]
addq #4, r15
;shrq #1, r1 ; average
;shrq #1, r1 ; compensate for i/p level
store r1, (r16)
subq #1, r3
jump nz, (r2)
addq #4, r16 ; doesn't save much
nop
;}}}  
;}}}  
;{{{  plot the freq graph (of the mag)
if Ians_screen_mode
;{{{  init plot_pixel proc call addr
movei #plot_pixel, plot_pixel_proc
movei #vert_line, vert_line_proc
;}}}  
;{{{  init screen sizes etc
movei #384, scr_width
movei #240, scr_height
movei #768, stride
;}}}  
;{{{  plot a pixel 50, 50
movei #50, x_coord
movei #50, y_coord
movei #return_2, return_address
jump (plot_pixel_proc)
nop
return_2:
;}}}  
;{{{  init loop addr regs
movei #next_index, next_index_label
;}}}  
;{{{  init
movei #127, no_to_plot ; no values to plot
;move mag_buf, r14
;movei #imag_buf_st, r14 ; DEBUG
movei #mag_buf_st, r14
addq #4, r14 ; step over 1st value
movei #200, max_height ; max height
movei #0, index_inc
;}}}  
next_index:
;{{{  plot n values
addq #1, index_inc
move index_inc, index
load (r14), value ; value 0 - ?
addq #4, r14
move index, r0
shlq #1, index ; double for x offset
add r0, index ; 127 * 3 < 384 !
sharq #8, value ; debug
move value, height
cmpq #0, height
jr z, return_1 ; if 0 length, skip
nop
;jr n, return_1 ; if -ve length, skip ; DEBUG
;nop
move index, x_coord ; x
movei #200, y_coord ; y
movei #return_1, return_address
jump (vert_line_proc)
nop
return_1:
subq #1, no_to_plot
jump nz, (next_index_label)
nop
;}}}  
endif
;}}}  
;{{{  jump over luts, procs
movei #code_skip, r0
jump (r0)
nop
;}}}  
if Ians_screen_mode
;{{{  PROC vert_line
;{{{  notes
; note vert line now goes up
; plot the vert line up to x_start = x_coord, y = y_coord
;   height in "height"
; return address in "return_address"
; ibb
;}}}  
vert_line:
;{{{  test coords
cmp scr_width, x_coord ; test x
jump pl, (return_address)
nop
cmp scr_height, y_coord ; test end y
jump pl, (return_address)
nop
cmp scr_height, height
jr n, ok_1
nop
;move scr_height, height ; clip
movei #200, height ; clip ; debug
ok_1:
;}}}  
shlq #1, x_coord ; x to point at words
move y_coord, r0
mult stride, r0 ; offset in lines to bytes
add x_coord, r0
movei #scrbase, r1
load (r1), r2
add r2, r0 ; y_coord now points at pixel
movei #$ffff, r2 ; pixel colour ?????
movei #vert_line_loop, r1
vert_line_loop:
storew r2, (r0) ; plot the pixel
sub stride, r0 ; step up
jump n, (return_address) ; gone off screen !
nop
subq #1, height
jump nz, (r1)
nop
jump (return_address)
nop
;}}}  
;{{{  PROC plot pixel
plot_pixel:
; plot the pixel x = x_coord, y = y_coord
; return address in "return_address"
; ibb
;{{{  clip
cmp scr_width, x_coord
jump pl, (return_address) ; clip max X
nop
cmp scr_height, y_coord ; no harm if this is done whatever
jump pl, (return_address)
nop
;}}}  
shlq #1, x_coord ; x to point at words
mult stride, y_coord ; offset in lines to bytes
add x_coord, y_coord
movei #scrbase, r1
load (r1), r0
add r0, y_coord ; y_coord now points at pixel
movei #$ffff, r1 ; pixel colour ?????
storew r1, (y_coord) ; plot the pixel
jump (return_address)
nop
;}}}  
endif
;{{{  l/u tables, data
align long
; These tables are of Pi / n where n = 1, 2, 4, 8, ... (radians)
;  you need as many entries as the highest log2 (no_samples)
;  I have catered for up to 1024 samples with 10 entries.
; These values are * 256 for ib fixed point, and sign of the sin
;  table is inverted also.
; int cos_table [10] = {-256, 0, 181, 236, 251, 255, 255, 256};
cos_table: dc.l        -256, 0, 181, 236, 251, 255, 255, 256, 256, 256
; int sin_table [10] = {0, -255, -181, -98, -50, -25, -13,  -6};
sin_table: dc.l        0, -255, -181, -98, -50, -25, -13,  -6, -3, -2
;}}}  
;{{{  code_skip:
code_skip:
;}}}  
;}}}  
;{{{  set fft_is_active flag false
movei #fft_is_active, r0
moveq #0, r1
store r1, (r0)   ; set fft_is_active flag false
;}}}  
;{{{  init for next sampling
movei #imag_buf_st, isr_target_addr
moveq #no_samples_per, isr_sample_delay_count
movei #no_samples, isr_no_samples_to_get ; enable sampling
;}}}  
movei #fft_then_sample_loop, r0
jump (r0)
nop
;
;

	or	isr_volume,isr_volume
muteVOL:
;
;
;
;}}}  
;{{{  i2s_isr:
;{{{  do l,r
; fft gets data before volume adj
;
i2s_isr:					; Sample pair-rate ISR
	load	(isr_flags_addr),isr_flags	; load flags
	movefa	DATATRK,R30
	btst	#30,R30
	jr	EQ,NOmute
	nop
;
	load	(isr_cd_inpl),isr_cd_data	; data from right channel
	moveq	#0,isr_fft_datal
	jr	i2s_next
	moveq	#0,isr_fft_datar
;
NOmute:
	movefa	VOLREG,isr_volume		;*Dave pick up constant from alt
	load	(isr_volume),isr_volume		;*Dave
;
	load	(isr_cd_inpl),isr_cd_data	; data from right channel
	move	isr_cd_data,isr_fft_datal	; left for fft
	imult	isr_volume,isr_cd_data		; Volume adjust

	addq	#4,isr_cd_inpl			;8-May-95 *Dave reverse channels

	sharq	#15,isr_cd_data

;8-May-95	store	isr_cd_data,(isr_cd_inpl)
;8-May-95	addq	#4,isr_cd_inpl

	store	isr_cd_data,(isr_cd_inpl)	;8-May-95

	load	(isr_cd_inpl),isr_cd_data
	move	isr_cd_data,isr_fft_datar	; right for fft
	imult	isr_volume,isr_cd_data		; Volume adjust

	subq	#4,isr_cd_inpl			;8-May-95

	sharq	#15,isr_cd_data

	store	isr_cd_data,(isr_cd_inpl)	;8-May-95

;8-May-95	store	isr_cd_data,(isr_cd_inpl)
;8-May-95	subq	#4,isr_cd_inpl
;
;***********test junk
;	movei	#clean_up,isr_jump_reg
;	jump	(isr_jump_reg)
;	nop
;***********end test junk
;
;}}}  
;
;
;}}}  
;{{{  do we need to do a sample ?
	.if	0
	or	isr_no_samples_to_get,isr_no_samples_to_get
					; > 0 to enable sampling
	jr	nz,maybe_do_one
			; avoid reloading jump reg unless don't want a sample
			; 'cos it's the same reg as isr_fft_datal
	nop
	movei	#clean_up,isr_jump_reg
	jump	(isr_jump_reg)
	nop
maybe_do_one:
	subq 	#1,isr_sample_delay_count
	jr	z,yes_do_one
	nop
	movei	#clean_up,isr_jump_reg
	jump	(isr_jump_reg)	; not ready for another sample yet
	nop
	.endif
;
;***Dave's stuff
;
i2s_next:
	subq 	#1,isr_volume_addr	;we use a global
	jr	z,maybe_do_one
	nop
isr_xit:
;	movei	#clean_up_dave,isr_jump_reg
	movefa	CLEANDAVE,isr_jump_reg
	jump	(isr_jump_reg)	; not ready for another sample yet
;*Dave	nop
maybe_do_one:
	or	isr_no_samples_to_get,isr_no_samples_to_get
					; > 0 to enable sampling
	jr	z,isr_xit
	moveq	#no_samples_per,isr_volume_addr	;**Dave reset delay
	
			; avoid reloading jump reg unless don't want a sample
			; 'cos it's the same reg as isr_fft_datal
yes_do_one:
; here r + l samples in isr_fft_datal, isr_fft_datar
;}}}  
;{{{  do mode & jump table
;
	movei	#fft_mode,isr_fft_temp_addr		;address of var
;*Dave	moveq	#no_samples_per,isr_sample_delay_count	;reset delay
	load	(isr_fft_temp_addr),isr_fft_mode	;get mode requested
	moveq	#3,isr_fft_temp_addr
	and	isr_fft_temp_addr,isr_fft_mode	;only cmds 0-3 are implemented
	movei	#jump_table_start,isr_fft_temp_addr
	shlq	#2,isr_fft_mode				;make into INT32 offset
	add	isr_fft_mode,isr_fft_temp_addr		;add offset from start
	load	(isr_fft_temp_addr),isr_fft_temp_addr	;get appropriate address
	jump	(isr_fft_temp_addr)			;jump to it
	nop
;{{{  jump table
	dc.l $49616e20 ; 'Ian '
align long
jump_table_start:	; fft modes
	dc.l fft_mono	; mode 0
	dc.l fft_stereo	; mode 1
	dc.l fft_left	; mode 2
	dc.l fft_right	; mode 3
align byte
;}}}  
;}}}  
;{{{  ya, do mono
fft_mono:
	shlq	#16,isr_fft_datar		; move to enable sign bit
	shlq	#16,isr_fft_datal
	sharq	#20,isr_fft_datar		; signed right shift
	sharq	#20,isr_fft_datal
	add	isr_fft_datal,isr_fft_datar	; average l and r
	sharq	#1,isr_fft_datar		; keep lsb

	movei	#clean_up,isr_jump_reg
;	movei	#clean_up_dave,isr_jump_reg

	store	isr_fft_datar,(isr_target_addr)
	subq	#1,isr_no_samples_to_get
             ; when ..._to_get == 0, notifies got 256, and disables sampling
	jump	(isr_jump_reg)
	addq	#4,isr_target_addr
	nop
;}}}  
;{{{  ya, do stereo or r, l
;{{{  fft_stereo -> don't change the position of these 2 folds
fft_stereo:
	movei	#fft_r_l,isr_fft_temp_addr	; address of right or left flag
	load	(isr_fft_temp_addr),isr_fft_mode
	or	isr_fft_mode,isr_fft_mode	; set flags
	jr	z,fft_left			; zero goes left
	nop
	movei	#fft_right,isr_fft_temp_addr
	jump	(isr_fft_temp_addr)
	nop
;}}}  
;{{{  fft_left   ->
fft_left:
	shlq	#16,isr_fft_datal	; move to enable sign bit
	sharq	#20,isr_fft_datal	; signed right shift
	subq	#1,isr_no_samples_to_get
             ; when ..._to_get == 0, notifies got 256, and disables sampling
	store	isr_fft_datal,(isr_target_addr)
	movei	#clean_up,isr_jump_reg ; outahere
	addq	#4,isr_target_addr
	movei	#fft_r_l,isr_fft_temp_addr	; address of right or left
	moveq	#1,isr_fft_mode			; set mode for right next time
	jump	(isr_jump_reg)
	store	isr_fft_mode,(isr_fft_temp_addr)	; set flag for next time
	nop
;}}}  
;{{{  fft_right
fft_right:
	shlq	#16, isr_fft_datar	; move to enable sign bit
	sharq	#20, isr_fft_datar	; signed right shift
	subq	#1, isr_no_samples_to_get
             ; when ..._to_get == 0, notifies got 256, and disables sampling
	store	isr_fft_datar,(isr_target_addr)
	movei	#clean_up,isr_jump_reg
	addq	#4,isr_target_addr
	movei	#fft_r_l,isr_fft_temp_addr	; address of right or left
	xor	isr_fft_mode,isr_fft_mode	; clear mode for left next time
	jump	(isr_jump_reg)
	store	isr_fft_mode,(isr_fft_temp_addr)	; set flag for next time
	nop
;}}}  
;}}}  
;{{{  clean_up:
clean_up:                 	      ; do the housekeeping, per THE BOOK
        bclr    3,isr_flags           ; clear IMASK
        bset    10,isr_flags          ; set I2S interrupt clear bit
        load    (isr_stack_addr),isr_jump_reg       ; get last instruction address
        addq    2,isr_jump_reg		; point at next to be executed
        addq    4,isr_stack_addr	; update the stack pointer
        jump    (isr_jump_reg)		; and return
        store   isr_flags,(isr_flags_addr)	; restore flags
;}}}
;}}}  
;}}}  
;{{{  COMMENT clean_up_dave:
;
;
clean_up_dave: ; Dave's code prior to clean_up exit
;	cmpq	#3,isr_volume_addr	;2/7-duty (only 1,or 2)
;	jr	CC,clean_up
;	nop
;
	.if	0
	movefa	SCODEPTR,R25
	movefa	RESULTBASE,R30
;
	load	(r25),r23
	moveq	#$1f,r25	
	and	r23,r25
	sharq	#$8,r23
	add	r30,r25
	storeb	r23,(r25)
	movefa	CLEANUP,R30

	jump	(R30)
	nop
	.endif
;
;
	.if	0
	movefa	SCODEPTR,R25
	movefa	RESULTBASE,R30
	movefa	CURCRC,R23
	add	R23,R30
	addq	#2,R23
	loadw	(R25),R29
	btst	#12,R23
	jr	EQ,skipwrap
	storew	R29,(R30)
;
	moveq	#0,R23
skipwrap:
	moveta	R23,CURCRC

	movefa	CLEANUP,R30
	jump	(R30)
	nop
	.endif
;
;
;
;  R23,R25,R29,R30 are available
;
;  Primary registers available:
;
regi23	regequ	R23
regi25	regequ	R25
regi29	regequ	R29
;
regi30	regequ	R30
;regi30	regequ	R20
;
;
;*******test code
	.if	0
testp:
;
;
;  load up some constants in alt reg set
;
	moveq	#$10,R0
	moveta	R0,SUBSTAT
;
;	movei	#$DFFF1A,R0
	movei	#$e0000,R0
	moveta	R0,SCODEPTR
;
	moveq	#0,R0
	moveta	R0,CURCRC
;
	movei	#$2810,R0
	moveta	R0,RESULTPTR
;
;	movei	#clean_up,R0
	movei	#testloop,R0
	moveta	R0,CLEANUP
;
;	movei	#$2a00,R0
	movei	#$f1c400,R0
	moveta	R0,XMDMTAB
;
	movei	#crcdone,R0
	moveta	R0,CRCDONE
;
	movei	#fetcher,R0
	moveta	R0,FETCHER
;
	movei	#$1d0f,R0
	moveta	R0,FINECRC
;
	movei	#VOLUME,R0
	moveta	R0,VOLREG
;
	movei	#$2830,R0
	moveta	R0,CLEANDAVE
;
;;;	
testloop:
;*********end test code
;
	.endif
;
;
;
	movefa	SAMPCOUNT,regi23
	movefa	SCODEPTR,regi30	;->DFFF1A
	subq	#1,regi23
	jr	PL,skipsamp
	movefa	CLEANUP,regi25
;
	loadw	(regi30),regi29	;fetch subcode data
	btst	#4,regi29
	jump	EQ,(regi25)
	moveq	#20,regi23
;
	moveta	regi29,SAMPLE
	add	regi23,regi23
	jump	(regi25)
skipsamp:
	moveta	regi23,SAMPCOUNT
;	or	regi30,regi30
	movefa	SAMPLE,regi30

;	movefa	SUBSTAT,regi29
;
;
;***test code
;
	.if	0
	addq	#2,regi25
	moveta	regi25,SCODEPTR
;***end test
;
	.endif

	.if	0
	btst	#4,regi30		;*********test
	jump	EQ,(regi25)
	moveq	#0,regi23
;
	moveta	regi23,SAMPLE
	move	regi30,regi29
	shlq	#24,regi30
	movefa	RESULTBASE,regi23
	shrq	#22,regi30
	add	regi30,regi23
	jump	(regi25)
	store	regi29,(regi23)		;*********end test
;
	.endif
;
;
	movefa	SUBSTAT,regi29	;substat($10,$01,$11,$02,$12,$03,..$1B,$0C,$1C)
;	btst	$4,regi30
;	jr	EQ,clean_up	;br if it's not what we want
;	jump	EQ,(regi25)	;br if it's not what we want
;	moveq	#0,regi25
;
	movefa	FETCHER,regi23
;	moveta	regi25,SAMPLE
;
	btst	#4,regi29	;do we fetch & pack or calculate CRC?
	jump	NE,(regi23)	;br to fetcher if fetch & pack
	moveq	#$1c,regi23
;
;
;  do CRC calc here
;
	movefa	CURCODE,regi25	;get last byte read from hardware
	movefa	CURCRC,regi23	;get running CRC
crcalc:
	shrq	#8,regi23	;hi byte in lo
	movefa	XMDMTAB,regi30
	xor	regi23,regi25
	bset	#4,regi29	;advance SUBSTAT
;	shlq	#1,regi25
	shlq	#2,regi25
	movefa	CURCRC,regi23	;reget CURCRC
	add	regi25,regi30
	shlq	#8,regi23
;	loadw	(regi30),regi25
	load	(regi30),regi25
	xor	regi25,regi23
	shlq	#16,regi23
	moveta	regi29,SUBSTAT
	shrq	#16,regi23
	moveta	regi23,CURCRC
;
;
;    end CRC
cleaout:
	movefa	CLEANUP,regi30
	jump	(regi30)
	nop
;
cleaout1:
	movefa	CLEANUP,regi30
	jump	(regi30)
	moveta	regi25,DATATRK

goodcrc:
	movefa	RESULTPTR,regi29
	movefa	TPACK0,regi25
	movefa	TPACK1,regi23
	store	regi25,(regi29)
	addq	#4,regi29
	movefa	TPACK2,regi30
	store	regi23,(regi29)
	addq	#4,regi29
	jr	cleaout1
	store	regi30,(regi29)
;
;
;  Check for good CRC
;
crcdone:
fezz0:
	movefa	CURCRC,regi30
	movefa	FINECRC,regi29
	moveta	regi23,CURCRC
	cmp	regi30,regi29
	moveq	#$10,regi23
	jr	EQ,goodcrc
;	jr	goodcrc
	moveta	regi23,SUBSTAT
;
	movefa	CLEANUP,regi30
	jump	(regi30)
;	nop
;	
fetcher:
fezz1:
;	moveq	#$1c,regi23
	cmp	regi23,regi29
	jr	EQ,crcdone
	moveq	#0,regi23
;
	xor	regi30,regi29		;this is like a 5-bit compare for EQ
	shlq	#27,regi29		;is this the one we want?
	jr	NE,notyet		;br if not
	movefa	SUBSTAT,regi29		;reget substat
;
	shrq	#8,regi30		;q-subcode to LSbyte
	subq	#$f,regi29		;set status for CRC calc next time
	movefa	PACK4,regi23		;get previous pack for next q-subcode
	moveta	regi29,SUBSTAT		;save SUBSTAT
	shlq	#8,regi23		;clear out LSbyte of subcode pack
	or	regi30,regi23
	shlq	#30,regi29		;check for 0 mod 4
	moveta	regi30,CURCODE		;save current q-subcode for CRC calc
	jr	EQ,regstuff
	moveta	regi23,PACK4		;save packed subcodes
;
notyet:
	movefa	CLEANUP,regi30
	jump	(regi30)
;	nop
;
;  stuff in one of three regs used for temp storage of the 12-byte record 
;
regstuff:
	movefa	SUBSTAT,regi29	;this must be 4,8,or $c
	moveq	#8,regi25
	cmp	regi25,regi29	;
	jr	CS,itsa3
	moveta	regi23,TPACK2	;this won't hurt if done for any of the 3
;
	jr	NE,notyet
	nop
;
	jr	notyet
	moveta	regi23,TPACK1
	
itsa3:
	moveta	regi23,TPACK0
	shlq	#6,regi23
	movefa	RESULTBASE,regi25
	shrq	#30,regi23
	movefa	CLEANUP,regi30
	shlq	#4,regi23
	add	regi25,regi23
	jump	(regi30)
	moveta	regi23,RESULTPTR
;
;	jr	notyet
;	moveta	regi23,TPACK0
;
;
;
;
;}}}  
;{{{  misc
        end                                        

        ALIGN long
;}}}  
;{{{  vars
VOLUME:
        dc.l    0
;
;junkptr:
;	dc.l	junktab
junktab:


;}}}  
