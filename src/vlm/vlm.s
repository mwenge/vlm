; *******************************************************************
; Jeff Minter's Virtual Light Machine for the Atari Jaguar.
;
; %TuuuuuuuuuuuuuuuuuunTj-   _f555555555F6{         %uLLLLL7'..       ..%nLLLLuu
; _;^)iiiiiiiiiiiiiii)!#j- .`_?[[[[[[[[12m{         _;;/"//|1o)      `[ol//"/*Tu
;   ./""""""""""""""""}Jj- .`_l{sssssss*2m{            ':'':|)la!. <a[\)=''''lTu
;  .`=+/////////////=^*Cu_ .`_xlllllllcs3m{           .'''''''+>)?[l><'''''''lTu
;   .__,^^========^=r*x)<. .`_i%vvvvvvvc3ms           ._''''''___vx"__''''''_lTu
;      _,,^^^^^^^,';L#%    .`_<>>>>>>>>v2mL}}}}**r`   .__________--_________-cTu
;      `__:::,,::"ii{*|    .`_|\))))))|<toozzz7Cq2_   .---------------------`cTu
;       ..-__''-.cT7`      .`-+/////////======^!6y_   .``````````````````````cTu
;       ..``-__""la]-      .`-;^^^^^^^^^^^^^^^:!qy_   .`````````````````````.c#n
;         ..``'tzi         .``''''''''''''''''-rwL_    ..................... ioa
;            ..``.         .`````````````````  .``                           ...
;
; This is the reverse-engineered source code for the 'Virtual Light Machine'
; written by Jeff Minter in 1994 for the Atari Jaguar.
;
; The original code from which this source is derived is the copyright of Jeff
; Minter.
;
; The original home of this file is at: https://github.com/mwenge/vlm
;
; To the extent to which any copyright may apply to the act of disassembling
; and reconstructing the code from its binary, the author disclaims copyright
; to this source code.  In place of a legal notice, here is a blessing:
;
;    May you do good and not evil.
;    May you find forgiveness for yourself and forgive others.
;    May you share freely, never taking more than you give.
; *******************************************************************

.include "../jaguar.inc"
.include "../blitter.inc"
.include "vlm.inc"

; *******************************************************************
; Constants for the joystick controller.
; *******************************************************************
;        allbutts     EQU $22002000
;        allkeys      EQU $000f00ff
;        acheat       EQU $000a000a
;        a147         EQU $200e0000
;        six          EQU $00000004
;        abutton      EQU $20000000
;        bbutton      EQU $02000000
;        cbutton      EQU $00002000
;        pausebutton  EQU $10000000
;        optionbutton EQU $00000200
;        view1        EQU $000e0000
;        view2        EQU $000000e0
;        view3        EQU $0000000e
;        allpad       EQU $00f00000
;        somepad      EQU $00300000
;        bigreset     EQU $00010001
; *******************************************************************
; LaunchVLM
; *******************************************************************
LaunchVLM:
        move    #5,skid
        movea.l #stack,sp
        move.l  #rrts,davesvec
        move    #0,started
        move    #1,freerun
        move    #9,imatrix ; Set Bank Number to 9
        bsr.w   everything
        lea     davesobj,a0
        rts

        nop
; *******************************************************************
; audio?
; Initialize the 'demo mode' which runs in reactive mode to the audio
; input.
; *******************************************************************
audio:
        movea.l #stack,sp
        move.l  #rrts,davesvec
        move    #4,skid
        move    #0,imatrix ; Set Bank Number to 0
        move    #0,started
        move.l  #$FFFFFFFF,CLUT
        move.l  #$FFFFFFFF,CLUT+4

; *******************************************************************
; Main Loop?
; This is invoked from cdfront.s which manages the Audio CD control 
; functions and interface to the Virtual Light Machine.
; *******************************************************************
goagain:
        clr.w   freerun ; Allow the VLM to run freely, like a demo mode?
        bsr.w   everything ; The main business of the main loop.
        bra.s   goagain ; Loop ad-infinitum.
        illegal

; *******************************************************************
; everything
; Main business of the main loop.
; This populates our 'beasties' list with all the objects to be drawn. This
; list will later be converted by RunBeasties into a full Object List for the
; Object Processor to draw to the screen.

; While each of the entries in the beasties list references regions of pixel
; data to be drawn and where to draw it, we have to actually fill out that
; pixel data with stuff to draw. This is done by 'titlescr' which calls the
; GPU to draw the data.
; *******************************************************************
everything:
        bsr.w   gogo ; Initialize the VLM if required.
        tst.w   freerun ; Are we in audio reactive mode?
        bne.w   nodoit ; If not, don't initialize the sound DSP module.
        jsr     iansdoit ; Call the DSP initialistion routine in ians.s.

nodoit:
        clr.l   udud
        clr.l   aded
        clr.l   cskr

        ; Create 6 initialized fx objects and store them at refblock.
        lea     refblock,a6
        move    #5,d7 ; We'll do 6 objects.
irb:    bsr.w   ifxobj ; Initialize the object.
        lea     1024(a6),a6
        dbf     d7,irb

        ; Create an fx object in fxspace and point to it in a few
        ; different places.
        movea.l #fxspace,a6
        move.l  a6,fx1
        move.l  a6,fxobj
        move.l  a6,fxedbase
        move.l  a6,i_fxobj
        bsr.w   ifxobj
        bsr.w   zapdel
        jsr     gm

        lea     cubeobj,a0
        jsr     makeclear

        lea     genobj,a0
        jsr     makecube

        lea     monoobj,a0
        jsr     monovert

        lea     board,a0
        jsr     cleol

        move    #$FFFF,actime

        ; Draw the version details.
        movea.l #versionp,a0  ; "Virtual Light Machine v0.9//(c) 1994 Vi"...
        move    #1,cursx
        move    #1,cursy
        bsr.w   print

        ; We create a bunch of entries in the 'beasties' list. These will be converted
        ; by RunBeasties into entries into the actual Object List stored at 'blist'.
        ; When the time comes to actually get the Object Processor to write pixels to
        ; the screen we will copy the contents of the blist to dlist. It is dlist that
        ; the Object Processor will treat as the final Object List for writing.

        ; Create the main screen object in the beasties display backing list.
        lea     beasties,a0
        move.l  draw_screen,d2 ; Make draw_screen the screen data.
        move    #$FFF8,d0
        sub.w   palside,d0
        move    #$2C,d1 ; ','
        add.w   paltop,d1
        sub.w   #$B0,d1
        move    #1,skale ; Set scale to 1.
        swap    d0
        swap    d1
        move    #0,d5 ; Set Object Type to 0.
        jsr     makeit ; Create the screen object in the display list.

        tst.w   freerun ; Are we in audio reactive mode?
        bne.w   zippo

        ; Is this the display list object for the controls screen?
        lea     beasties+128,a0
        move.l  #board,d2 ; Make board the screen data for the object.
        move    #24,d0
        sub.w   palside,d0 ; Set the X position.
        move    #60,d1 
        add.w   paltop,d1 ; Set the Y position.
        swap    d0
        swap    d1
        move    #1,d5 ; Set the Object Type (ObTypes) to 1.
        move    #$24,d3 ; Set the width.
        move    #$24,d4 ; Set the hight.
        jsr     makeit_transparent

        move    #$FFFF,beasties+140
        lea     beasties+64,a0

zippo:  lea     beasties+192,a0
        move    #$64,d0 ; 'd'
        move    #$104,d1
        move.l  #jaglogo + $04,d2 ; Make the jaguar logo the screen data.
        move    #5,d5 ; Set the Object Type to 5
        jsr     makeit_transparent
        move    #$10,$16(a0)
        move    #$FFFF,12(a0) ; Set Display Object's Mode to off.
        move    #$88FF,$F00420
        move    #$80FF,$F00422

        lea     davesobj,a0
        move    #$50,d0 ; 'P'
        move    #$104,d1
        swap    d0
        swap    d1
        move.l  #$4000,d2
        move    #6,d5 ; Set the Object Type to 6.
        jsr     makeit_transparent
        move    #4,$16(a0)
        move    #$FFFF,12(a0) ; Set Display Object's Mode to off.
        move    #1,$1E(a0)

        lea     beasties+320,a0
        move    #$50,d0 ; 'P'
        move    #$104,d1
        swap    d0
        swap    d1
        move.l  #$4000,d2
        move    #6,d5
        jsr     makeit_transparent
        move    #$FFFF,12(a0) ; Set Display Object's Mode to off.
        move    #$14,$16(a0)
        move    #1,$1E(a0)

        lea     beasties+384,a0
        move    #$50,d0 ; 'P'
        move    #$104,d1
        move.l  #$4000,d2
        move    #6,d5
        swap    d0
        swap    d1
        jsr     makeit_transparent
        move    #$FFFF,12(a0) ; Set Display Object's Mode to off.
        move    #$24,$16(a0) ; '$'
        move    #1,$1E(a0)

        lea     beasties+448,a0
        move    #$10A,d0
        move    #$190,d1
        move.l  #vlmlogo,d2
        move    #7,d5
        swap    d0
        swap    d1
        jsr     makeit_transparent
        move    #$FFFF,12(a0) ; Set Display Object's Mode to off.
        move    #$FFFF,vlmtim

        ; Clear the environment variables.
        lea     envvals,a0
        move    #7,d0
xx:     clr.l   (a0)+
        dbf     d0,xx

        clr.w   XLO
        clr.w   YLO
        move    #$17F,XHI
        move    #$17F,YHI
        move.l  #$C00000,XCEN
        move.l  #$C00000,YCEN
        move.l  #fx1,fx
        move    #1,fxed

        ; Create the fx objects.
        lea     fxspace+1024,a6
        move    #4,d7
iprep:  movem.l d7/a5-a6,-(sp) ; Stash some values in the stack so we can restore them later.
        bsr.w   ifxobj
        movem.l (sp)+,d7/a5-a6
        lea     1024(a6),a6
        dbf     d7,iprep
        move.l  #fxspace+1536,fxobj

        move    #0,monitor
        move    #$104,monx
        move    #$F0,mony
        move    #3,monitand
        move.l  #symadj,routine
        clr.l   action
        jmp     titlescr ; Draw the screen. Do all the GPU stuff.

; *******************************************************************
; makeno?
; *******************************************************************
makeno:
        bsr.w   gadd
        clr.l   (a0)

; *******************************************************************
; ggg
; *******************************************************************
ggg:
        move.l  a6,fxedbase
        move.l  a6,fxobj
        addi.l  #4,esp
        jmp     initedit
        ; Returns

; *******************************************************************
; makepoly
; Unused.
; *******************************************************************
makepoly:
        bsr.w   gadd
        bsr.w   initwave
        move    #$15,d1
        move.l  #$40FFFF,d0
        bsr.w   wlink
        move    #$16,d1
        move.l  #$80FFFF,d0
        bsr.w   wlink
        move    #$17,d1
        move.l  #$1FFFF,d0
        bsr.w   wlink
        move    #24,d1
        move.l  #$2FFFF,d0
        bsr.w   wlink
        move    #$19,d1
        move.l  #$40FFFF,d0
        bsr.w   wlink
        move    #$1A,d1
        move.l  #$80FFFF,d0
        bsr.w   wlink
        move.l  a6,(a0)
        bra.s   ggg
        ; Returns

; *******************************************************************
; gogo
; Start everything up if necessary.
; *******************************************************************
gogo:
        tst.w   started
        bne.w   rrts
        move    #1,started
        move.l  #$70007,G_END
        move.l  #$70007,D_END
        move    #$100,JOYSTICK
        move    #1,INT1
        move.l  #dumint,($100).w
        move.l  #$FFFFFFFF,action
        bsr.w   startup ; Initialize everything.
        move.l  #$FFFFFFFF,action
        rts

; *******************************************************************
; testobj
; *******************************************************************
testobj:
        lea     (WID256).w,a0
        move    #$7F,d0

tdo:
        move    #$FF,d1

tdo2:
        move    d0,d2
        add.w   d1,d2
        move.b  d2,(a0)+
        dbf     d1,tdo2
        dbf     d0,tdo
        rts

; *******************************************************************
; mdvf2
; *******************************************************************
mdvf2:
        bsr.w   makecfb
        move    #1,thang(a6)
        move    #$60,($18).w ; '`'
        move    #$60,($1C).w ; '`'
        move    #$60,(vfb_xpos).w ; '`'
        move    #$60,(vfb_ypos).w ; '`'
        move    #$78,($08).w ; 'x'
        rts

; *******************************************************************
; mdvf4
; *******************************************************************
mdvf4:
        bsr.w   makecfb
        move    #2,thang(a6)
        move    #$30,($18).w ; '0'
        move    #$30,($1C).w ; '0'
        move    #$30,(vfb_xpos).w ; '0'
        move    #$30,(vfb_ypos).w ; '0'
        move    #$38,($08).w ; '8'
        rts

; *******************************************************************
; makefb
; *******************************************************************
makefb:
        bsr.w   gadd
        move.l  #1,$8C(a6)
        bra.w   fnop

makecfb:
        bsr.w   gadd
        move.l  #0,$8C(a6)

fnop:
        bsr.w   initdvf
        move.l  a6,(a0)
        bra.w   ggg
        rts

; *******************************************************************
; makering
; *******************************************************************
makering:
        bsr.w   gadd
        bsr.w   initring
        move.l  a6,(a0)
        bra.w   ggg

; *******************************************************************
; ipm
; *******************************************************************
ipm:
        bsr.w   gadd
        clr.l   4(a6)
        move.l  #$D,info(a6)
        move.l  #$60000,gpu(a6)
        move.l  a6,(a0)
        bra.w   ggg

; *******************************************************************
; itau
; *******************************************************************
itau:
        bsr.w   mpo
        move.l  #$80000,d_x(a6)
        move.l  #$80000,64(a6)
        move.l  #$80000,deltaz(a6)
        move.l  #$80000,gpu(a6)
        rts

; *******************************************************************
; mopo
; *******************************************************************
mopo:
        bsr.w   mpo
        move.l  #$70000,gpu(a6)
        rts

; *******************************************************************
; mpo
; *******************************************************************
mpo:
        bsr.w   makestar
        move.l  #$C,info(a6)
        move.l  #$50000,gpu(a6)
        rts

; *******************************************************************
; makeshun
; *******************************************************************
makeshun:
        bsr.w   makestar
        move.l  #$E,info(a6)
        move.l  #$90000,gpu(a6)
        rts

; *******************************************************************
; makestar
; *******************************************************************
makestar:
        bsr.w   gadd
        bsr.w   initpsf
        move.l  a6,(a0)
        bra.w   ggg

; *******************************************************************
; amkesurf
; *******************************************************************
amkesurf:
        bsr.w   gadd
        bsr.w   initwsur
        move.l  a6,(a0)
        bra.w   ggg

; *******************************************************************
; makemono
; *******************************************************************
makemono:
        bsr.w   gadd
        bsr.w   initmono
        move.l  a6,(a0)
        bra.w   ggg

; *******************************************************************
; mplaz2
; *******************************************************************
mplaz2:
        bsr.w   mplaz1
        move.l  #$40004,gpu(a6)
        rts

; *******************************************************************
; mplaz1
; *******************************************************************
mplaz1:
        bsr.s   makemono
        move.l  #$40002,gpu(a6)
        move.l  #$2000,j(a6)
        move.l  #$2000,k(a6)
        move.l  #$A,info(a6)
        rts

; *******************************************************************
; mjaglogo
; Unused.
; *******************************************************************
mjaglogo:
        bsr.s   makemono
        move.l  #$40003,gpu(a6)
        move.l  #$B,info(a6)
        rts

; *******************************************************************
; makeiri
; *******************************************************************
makeiri:
        bsr.s   makemono
        move.l  #$40001,gpu(a6)
        rts

; *******************************************************************
; gadd
; *******************************************************************
gadd:
        movea.l fx,a0
        move    og,d0
        lsl.w   #2,d0
        lea     (a0,d0.w),a0
        move.l  (a0),d6
        beq.w   zzzero
        movea.l d6,a6
        clr.l   (a0)
        bra.w   ifxobj

; *******************************************************************
; zzzero
; *******************************************************************
zzzero:
        movea.l fx,a6
        movea.l (a6),a6
        move    og,d0
        mulu.w  #1024,d0
        adda.l  d0,a6
        bra.w   ifxobj

; *******************************************************************
; initmono
; *******************************************************************
initmono:
        move.l  #6,info(a6)
        move.l  #$40000,gpu(a6)
        rts

; *******************************************************************
; initwsur
; *******************************************************************
initwsur:
        bsr.w   initpsf
        move.l  #$800000,dstoffz(a6)
        move.l  #0,phase1(a6)
        move.l  #$100000,phase2(a6)
        move.l  #$20000,phase5(a6)
        move.l  #$100000,i(a6)
        move.l  #$100000,$8C(a6)
        move.l  #$100000,d_x(a6)
        move.l  #$10000,64(a6)
        move.l  #2,gpu(a6)
        move.l  #5,info(a6)
        rts

; *******************************************************************
; initpsf
; *******************************************************************
initpsf:
        move.l  #3,plot_mod(a6)
        move.l  #0,gpu(a6)
        move.l  #2,info(a6)
        rts

; *******************************************************************
; initwave
; *******************************************************************
initwave:
        move.l  #$20000,gpu(a6)
        move.l  #1,info(a6)
        rts

; *******************************************************************
; initdvf
; *******************************************************************
initdvf:
        move.l  #4,info(a6)
        move.l  #$10000,gpu(a6)
        move.l  #dvf_buf,i(a6)
        move.l  #p_sines,sine_bas(a6)
        rts

; *******************************************************************
; initring
; *******************************************************************
initring:
        clr.l   _i1(a6)
        clr.l   _i2(a6)
        move.l  #9,gpu(a6)
        move.l  #3,info(a6)
        move.l  #2,plot_mod(a6)
        rts

; *******************************************************************
; wavelink
; *******************************************************************
wavelink:
        movea.l fxobj,a6


; *******************************************************************
; wlink
; *******************************************************************
wlink:
        lsl.w   #2,d1
        lea     optionbu(a6),a5
        lea     (a5,d1.w),a5
        lea     (a6,d1.w),a4
        move.l  (a4),(a5)
        move.l  d0,UPDA1F(a4)
        rts

; *******************************************************************
; symadj
; *******************************************************************
symadj:
        lea     pad_now,a1
        bsr.w   doadsr
        tst.w   vlm_mode
        beq.w   rrts
        lea     pad_now,a1
        tst.w   editing
        beq.w   shoop
        lea     pad_now+4,a1
        ; Falls through.

; *******************************************************************
; shoop
; *******************************************************************
shoop:
        move.b  SRCEN(a1),d0
        rol.b   #2,d0
        lea     pixcon,a0
        jsr     pinertco
        move.b  SRCEN(a1),d0
        rol.b   #4,d0
        lea     piycon,a0
        jsr     pinertco
        move.l  pixcon,d0
        lsr.l   #8,d0
        and.l   #$FFFF,d0
        move.l  d0,px
        move.l  piycon,d0
        lsr.l   #8,d0
        and.l   #$FFFF,d0
        move.l  d0,py
        rts

; *******************************************************************
; doadsr
; *******************************************************************
doadsr:
        tst.w   vlm_mode
        bne.w   ago
        lea     zero,a1

ago:
        movea.l a1,a3
        lea     envvals,a1
        lea     $A(a1),a1
        lea     adsrc,a0
        move.l  (a3),d0
        and.l   #$2000,d0
        bsr.w   do_adsr
        lea     2(a1),a1
        lea     adsrb,a0
        move.l  (a3),d0
        and.l   #$2000000,d0
        bsr.w   do_adsr
        lea     2(a1),a1
        lea     adsra,a0
        move.l  (a3),d0
        and.l   #$20000000,d0

; *******************************************************************
; do_adsr
; *******************************************************************
do_adsr:
        move    8(a0),d1
        lea     adsrvex,a4
        lsl.w   #2,d1
        movea.l (a4,d1.w),a4
        jmp     (a4)

; *******************************************************************
; adsrvex
; *******************************************************************
adsrvex:
        dc.l trigwait, attack, decay, sustain, release

; *******************************************************************
; trigwait
; *******************************************************************
trigwait:
        tst.l   d0
        beq.w   rrts

setatac:
        move    #1,8(a0)
        rts

; *******************************************************************
; attack
; *******************************************************************
attack:
        tst.l   d0
        beq.w   srel
        move    (a1),d1
        move    (a0),d2
        lsr.w   #2,d2
        and.l   #$FFFF,d1
        and.l   #$FFFF,d2
        add.l   d2,d1
        cmp.l   #$FFFF,d1
        ble.w   setay
        move.l  #$FFFF,d1
        move    #2,8(a0)

setay:  move    d1,(a1)
        rts

; *******************************************************************
; decay
; *******************************************************************
decay:
        tst.l   d0
        beq.w   srel
        move    (a1),d1
        move    2(a0),d2
        lsr.w   #2,d2
        move    4(a0),d3
        and.l   #$FFFF,d1
        and.l   #$FFFF,d2
        and.l   #$FFFF,d3
        sub.l   d2,d1
        cmp.l   d3,d1
        bpl.s   setay
        move.l  d3,d1
        move    #3,8(a0)
        bra.s   setay

; *******************************************************************
; sustain
; *******************************************************************
sustain:
        tst.l   d0
        bne.w   rrts
srel:   move    #4,8(a0)
        rts

; *******************************************************************
; release
; *******************************************************************
release:
        tst.l   d0
        bne.w   setatac
        move    (a1),d1
        move    6(a0),d2
        lsr.w   #2,d2
        and.l   #$FFFF,d1
        and.l   #$FFFF,d2
        sub.l   d2,d1
        bpl.s   setay
        clr.w   (a1)
        move    #0,8(a0)
        rts

; *******************************************************************
; kpad
; *******************************************************************
kpad:
        bsr.w   gkpad
        bra.w   pn_butte

; *******************************************************************
; gkpad
; *******************************************************************
gkpad:
        lea     ud_butts,a0

; *******************************************************************
; gkp
; *******************************************************************
gkp:
        lea     pad_now,a1
        tst.w   editing
        beq.w   gnana
        movea.l cpad+4,a3
        move    #$B,d0

flashem:
        tst.b   (a3)
        bmi.w   nxtk
        tst.b   3(a3)
        beq.w   nxtk
        subi.b  #1,3(a3)
        move.b  3(a3),d1
        and.w   #3,d1
        bne.w   nxtk
        move.l  a0,action
        bchg    #7,2(a3)

nxtk:
        lea     4(a3),a3
        dbf     d0,flashem

gnana:
        lea     cpad,a0
        movea.l 4(a0),a3
        movea.l (a0),a0
        movea.l i_fxobj,a6
        move    og,d0
        movea.l fx,a6
        lsl.w   #2,d0
        move.l  (a6,d0.w),d1
        beq.w   rrts
        movea.l d1,a6
        move.l  d1,i_fxobj
        move    (a1),d6
        move    #3,d7

gk1:
        movea.l (a0)+,a2
        btst    #0,d6
        beq.w   bodb
        tst.b   3(a3)
        bne.w   bodb
        bra.w   execute

bodb:
        lsr.w   #1,d6
        lea     4(a3),a3
        dbf     d7,gk1
        move    2(a1),d6
        move    #7,d7

gk2:
        movea.l (a0)+,a2
        btst    #0,d6
        beq.w   bodb2
        tst.b   3(a3)
        bne.w   bodb2
        bra.w   execute

bodb2:  lsr.w   #1,d6
        lea     4(a3),a3
        dbf     d7,gk2
        rts

someeother:
        move.l  (a1),d0
        and.l   #$22002000,d0
        bne.w   somebutt
        clr.w   symed
        rts

somebutt:
        move.l  d0,-(sp) ; Stash some values in the stack so we can restore them later.
        movea.l (a0)+,a2
        cmp.l   #$2000,d0
        bne.w   notc
        jsr     (a2)

notc:   move.l  (sp)+,d0
        move.l  d0,-(sp) ; Stash some values in the stack so we can restore them later.
        movea.l (a0)+,a2
        cmp.l   #bbutton,d0
        bne.w   notb
        jsr     (a2)

notb:
        move.l  (sp)+,d0
        move.l  d0,-(sp) ; Stash some values in the stack so we can restore them later.
        movea.l (a0)+,a2
        cmp.l   #abutton,d0
        bne.w   nota
        jsr     (a2)

nota:
        move.l  (sp)+,d0
        move.l  d0,-(sp) ; Stash some values in the stack so we can restore them later.
        movea.l (a0)+,a2
        cmp.l   #$2002000,d0 ; abutton or bbutton
        bne.w   notab
        jsr     (a2)

notab:
        move.l  (sp)+,d0
        rts

; *******************************************************************
; dorsym1
; *******************************************************************
dorsym1:
        lea     ((sympad+3+$12D)).l,a3
        move    #2,symed
        bra.w   gjoy
; *******************************************************************
; dorsym2
; *******************************************************************
dorsym2:
        lea     ((sympad+3+$13D)).l,a3
        move    #3,symed
        bra.w   gjoy
; *******************************************************************
; dorsym3
; *******************************************************************
dorsym3:
        lea     ((sympad+3+$14D)).l,a3
        move    #4,symed
        bra.w   *+4

; *******************************************************************
; gjoy
; Get joystick/controller input.
; *******************************************************************
gjoy:
        move.l  (a1),d0
        move.l  d0,d1
        and.l   #$400000,d0
        beq.w   npleft
        movea.l (a3),a4
        jsr     (a4)

npleft:
        move.l  (a1),d0
        and.l   #$800000,d0
        beq.w   npright
        movea.l 4(a3),a4
        jsr     (a4)

npright:
        move.l  (a1),d0
        and.l   #$100000,d0
        beq.w   npup
        movea.l 8(a3),a4
        jsr     (a4)

npup:
        move.l  (a1),d0
        and.l   #$200000,d0
        beq.w   npdn
        movea.l 12(a3),a4
        jsr     (a4)

npdn:
        rts

; *******************************************************************
; srcentre
; *******************************************************************
srcentre:
        move    #1,symed
        move.b  SRCEN(a1),d0
        rol.b   #2,d0
        bsr.w   rxs
        move.b  SRCEN(a1),d0
        rol.b   #4,d0
        bra.w   rys

; *******************************************************************
; execute
; *******************************************************************
execute:
        tst.w   editing
        beq.w   exec1
        move.b  #$1C,3(a3)
        btst    #0,2(a3)
        beq.w   exec1
        move.b  #$20,3(a3) ; ' '

exec1:
        move.l  a3,-(sp) ; Stash some values in the stack so we can restore them later.
        jsr     (a2)
        movea.l (sp)+,a3
        tst.w   editing
        beq.w   rrts
        cmpi.w  #2,antelope
        bne.w   anteclr
        clr.w   2(a3)
        clr.w   antelope
        move    #1,seldb

anteclr:
        cmpi.l  #keydb,routine
        beq.w   rrts
        move.l  routine,ov
        move.l  #keydb,routine
        rts

; *******************************************************************
; keydb
; *******************************************************************
keydb:
        move.l  pad_now,d1
        and.l   #$E00FF,d1
        bne.w   rrts
        move.l  ov,routine
        rts

; *******************************************************************
; zapdel
; *******************************************************************
zapdel:
        move    #2,delayf
        clr.w   delayp
        move    #2,delayt
        move    #$F,delayn
        clr.w   d2elayp
        move    #2,d2elayt
        move    #$F,d2elayn
        move    #3,zerstart
        lea     delaylin,a0
        move.l  a0,dline
        movea.l a0,a1
        lea     $300(a1),a1
        move.l  a1,d2line
        move    #$7F,d0

idli:
        clr.l   info(a0)
        lea     $300(a0),a0
        dbf     d0,idli
        rts

; *******************************************************************
; setedit
; *******************************************************************
setedit:
        tst.w   vedit
        bne.w   rrts
        move    vlm_mode,ovlm_mod
        move    #1,vedit
        move    #3,vlm_mode
        move    #1,beasties+140
        move.l  #initedit,action
        rts

; *******************************************************************
; symclr
; *******************************************************************
symclr:
        move.l  #1,asym_fla(a6)
        bra.w   setstat

syminv: eori.l  #$100FF,asym_fla(a6)

setstat:
        bsr.w   isymbut
        move.l  #ud_butts,action
        rts

; ---------------------------------------------------------------------------
sp1:
        bchg    #0,$A3(a6)
        rts
; ---------------------------------------------------------------------------
sp2:
        bchg    #1,$A3(a6)
        rts
; ---------------------------------------------------------------------------
sp3:
        bchg    #2,$A3(a6)
        rts
; ---------------------------------------------------------------------------
sp4:
        bchg    #3,$A3(a6)
        rts
; ---------------------------------------------------------------------------
sp5:
        bchg    #4,$A3(a6)
        rts
; ---------------------------------------------------------------------------
sp6:
        bchg    #5,$A3(a6)
        rts
; ---------------------------------------------------------------------------
sp7:
        bchg    #6,$A3(a6)
        rts
; ---------------------------------------------------------------------------
sp8:
        bchg    #7,$A3(a6)
        rts
; ---------------------------------------------------------------------------
sp9:
        bchg    #0,$A1(a6)
        rts
; ---------------------------------------------------------------------------
bp1:
        bchg    #0,$F7(a6)
        rts
; ---------------------------------------------------------------------------
bp2:
        bchg    #1,$F7(a6)
        rts
; ---------------------------------------------------------------------------
bp3:
        bchg    #2,$F7(a6)
        rts
; ---------------------------------------------------------------------------
bp4:
        bchg    #3,$F7(a6)
        rts
; ---------------------------------------------------------------------------
bp5:
        bchg    #4,$F7(a6)
        rts
; ---------------------------------------------------------------------------
bp6:
        bchg    #5,$F7(a6)
        rts
; ---------------------------------------------------------------------------
bp7:
        bchg    #6,$F7(a6)
        rts
; ---------------------------------------------------------------------------
bp8:
        bchg    #7,$F7(a6)
        rts
; ---------------------------------------------------------------------------
rsymi:
        move    fframes,d2
        and.w   #7,d2
        bne.w   rrts
        addi.w  #1,$4A(a6)
        rts
; ---------------------------------------------------------------------------
rsymd:
        move    fframes,d2
        and.w   #7,d2
        bne.w   rrts
        cmpi.w  #0,$4A(a6)
        beq.w   rrts
        subi.w  #1,$4A(a6)
        rts
; ---------------------------------------------------------------------------
invxinc:
        addi.l  #1,roscal2(a6)
        rts
; ---------------------------------------------------------------------------
invxdec:
        subi.l  #1,roscal2(a6)
        rts
; ---------------------------------------------------------------------------
invyinc:
        addi.l  #1,roscalei(a6)
        rts
; ---------------------------------------------------------------------------
invydec:
        subi.l  #1,roscalei(a6)
        rts
; ---------------------------------------------------------------------------
irscale:
        addi.l  #$10,roscale(a6)
        rts
; ---------------------------------------------------------------------------
drscale:
        subi.l  #$10,roscale(a6)
        rts
; ---------------------------------------------------------------------------
istep:
        addi.l  #$1000,rsym_ste(a6)
        rts
; ---------------------------------------------------------------------------
dstep:
        subi.l  #$1000,rsym_ste(a6)
        rts
; ---------------------------------------------------------------------------
istepi:
        addi.l  #$10,rsym_ist(a6)
        rts
; ---------------------------------------------------------------------------
dstepi:
        subi.l  #$10,rsym_ist(a6)
        rts

; *******************************************************************
; rxs
; *******************************************************************
rxs:
        lea     ixcon,a0
        jsr     inertcon
        move    (a0),d0
        swap    d0
        clr.w   d0
        move.l  d0,rxcen(a6)
        move.l  d0,RXCEN
        rts

; *******************************************************************
; rys
; *******************************************************************
rys:
        lea     iycon,a0
        jsr     inertcon
        move    (a0),d0
        swap    d0
        clr.w   d0
        move.l  d0,rycen(a6)
        move.l  d0,RYCEN
        rts

; *******************************************************************
; titlescr
; Draw the screen.
; This calls the 'omega' GPU module to fill out the screen RAM with pixels
; to draw. It signals to the 'Frame' vertical sync interrupt that things
; are ready to draw by setting the 'screen_ready' to 1.
; *******************************************************************
titlescr:
        movea.l draw_screen,a0
        move    #1,db_on ; Enable double-buffering.

        ; Populate our fx object, which will be passed to omega.
        movea.l fx,a6
        movea.l (a6),a6
        move.l  #$C8,d0
        move.l  #$C8,d1
        move.l  #$180,d2
        move.l  #$180,d3
        sub.l   d0,d2
        sub.l   d1,d3
        lsr.l   #1,d2
        lsr.l   #1,d3
        swap    d3
        move    d2,d3
        lea     dvf_buf,a0
        move.l  cscreen,(a0)+
        move.l  d0,(a0)+
        move.l  d1,(a0)+
        move.l  vfb_xsca(a6),(a0)+
        move.l  vfb_ysca(a6),(a0)+
        move.l  vfb_angl(a6),d0
        clr.w   d0
        swap    d0
        move.l  d0,(a0)+
        move.l  $18(a6),(a0)+
        move.l  $1C(a6),(a0)+
        move.l  d3,(a0)+
        move.l  $20(a6),(a0)+
        move.l  #p_sines,(a0)+
        move.l  draw_screen,(a0)+
        move.l  a6,(a0)+

        move.l  #cobuf,(in_buf + $1C).l
        move.l  #lol,in_buf ; Pointer to screen objects.
        move.l  #oscvals,(in_buf + $4).l
        move.l  fx,(in_buf + $8).l
        move.l  #results,(in_buf + $C).l
        move.l  #pbinfo,(in_buf + $10).l ; "Parameter not yet defined    "
        move.l  #delayf,(in_buf + $14).l
        move.l  #polyos,(in_buf + $18).l

        move.l  #gpumods,mods
        move.l  #1,screen_ready ; Signal screen ready for display (omega also does this!).
        moveq   #$A,d0
        lea     omega,a0
        jsr     gpurun

editloop:
        clr.w   moomoomo
        tst.w   freerun ; Are we in audio reactive mode?
        beq.w   eloop
        rts

eloop:  tst.w   _fsync
        beq.s   editloop
        move    #1,moomoomo
        bsr.w   yakedit
        move    #2,moomoomo
        movea.l davesvec,a0
        jsr     (a0)
        move    #3,moomoomo
        clr.w   _fsync
        move    #4,moomoomo
        bra.s   editloop

; *******************************************************************
; yakedit
; *******************************************************************
yakedit:
        tst.w   actime
        bmi.w   no_ac
        subi.w  #1,actime
        bpl.w   no_ac
        lea     clearstr,a0 ; "@"
        bsr.w   print

no_ac:
        movea.l fx,a6
        move    fxed,d0
        lsl.w   #2,d0
        movea.l (a6,d0.w),a6
        move.l  a6,fxobj
        tst.l   action
        beq.w   noact
        movea.l action,a0
        movea.l fxedbase,a6
        jsr     (a0)
        clr.l   action

noact:
        move.l  ixcon,d0
        cmp.l   ixcon,d0
        bne.s   noact
        move.l  d0,iixcon

noact2:
        move.l  iycon,d0
        cmp.l   iycon,d0
        bne.s   noact2
        move.l  d0,iiycon
        move    symed,d0
        beq.w   blib
        lea     edvex,a0
        subq.w  #1,d0
        lsl.w   #2,d0
        movea.l (a0,d0.w),a0
        jsr     (a0)

blib:
        movea.l fxedbase,a6
        bsr.w   shad
        rts

; *******************************************************************
; elcon
; *******************************************************************
elcon:
        move.l  a0,-(sp) ; Stash some values in the stack so we can restore them later.
        lea     8(a0),a0
        lea     pbinfo,a2 ; "Parameter not yet defined    "
        lea     elinx,a3
        lea     eltxt,a5
        move.l  #elvex,elvp
        move    #$3F,d1 ; '?'

elc1:
        clr.l   (a3)+
        dbf     d1,elc1
        lea     elinx,a3
        move    #1,d4

elc:
        move.b  (a1)+,d2
        bmi.w   elcend
        and.w   #$FF,d2
        move    d2,d7
        tst.b   (a3,d2.w)
        bne.s   elc
        lsl.w   #3,d2
        move    d2,d3
        lsl.w   #2,d2
        add.w   d3,d2
        lea     (a2,d2.w),a4
        move.l  a4,(a0)+
        move.l  #eparam,d7
        tst.b   $20(a4)
        bne.w   ucan
        move.l  #unquit,d7

ucan:
        move.l  d7,(a0)+
        movem.l a0-a1,-(sp) ; Stash some values in the stack so we can restore them later.
        movea.l elvp,a0
        lea     $1E(a4),a1
        move.l  a1,(a0)+
        move.l  a0,elvp
        movem.l (sp)+,a0-a1
        btst    #0,$1E(a4)
        beq.w   nolbit
        move.l  a5,-8(a0)

spect:
        move.b  (a4)+,d5
        move.b  d5,(a5)+
        cmp.b   #$3A,d5 ; ':'
        bne.s   spect
        move.b  #$20,(a5)+ ; ' '
        move.b  #$28,(a5)+ ; '('
        move.b  SRCEN(a4),(a5)+
        move.b  d4,(a3,d7.w)
        lea     (a2,d2.w),a4

llink:
        move.b  $1F(a4),d5
        and.w   #$FF,d5
        move.b  d4,(a3,d5.w)
        lsl.w   #3,d5
        move    d5,d6
        lsl.w   #2,d5
        add.w   d6,d5
        lea     (a2,d5.w),a4
        move.l  a4,-(sp) ; Stash some values in the stack so we can restore them later.

lcolon:
        move.b  (a4)+,d5
        cmp.b   #$3A,d5 ; ':'
        bne.s   lcolon
        move.b  #$2C,(a5)+ ; ','
        move.b  SRCEN(a4),(a5)+
        movea.l (sp)+,a4
        btst    #0,$1E(a4)
        bne.s   llink
        move.b  #$29,(a5)+ ; ')'
        move.b  #0,(a5)+
        add.w   #1,d4

nolbit:
        add.w   #1,d1
        cmp.w   d0,d1
        bne.w   elc

elcend:
        movea.l (sp)+,a0
        move    d1,(a0)
        move    #0,2(a0)
        move.l  #bline2,4(a0) ; "~g1:$20:Joypad to select, any FIRE to ed"...
        rts

; *******************************************************************
; ifxobj
; Initialize fx object.
; *******************************************************************
ifxobj:
        movea.l a6,a1
        move.l  #$FF,d0

xxxa:
        clr.l   (a1)+
        dbf     d0,xxxa
        move.l  #$8000,col1(a6)
        move.l  #$C00000,dstoffx(a6)
        move.l  #$C00000,dstoffy(a6)
        clr.l   dstoffz(a6)
        move.l  #0,phase1(a6)
        move.l  #1024,phase2(a6)
        move.l  #$140000,i(a6)
        move.l  #1024,j(a6)
        move.l  #1024,k(a6)
        move.l  #1024,zamp(a6)
        move.l  #3,asym_fla(a6)
        move.l  #p_sines,sine_bas(a6)
        move.l  #$200000,dy(a6)
        move.l  #8,rsym_ord(a6)
        move.l  #$80000,rsym_ste(a6)
        move.l  #0,rsym_ist(a6)
        move.l  #$C00000,rxcen(a6)
        move.l  #$C00000,rycen(a6)
        move.l  #$800,roscale(a6)
        move.l  #0,roscal2(a6)
        move.l  #0,roscalei(a6)
        move.l  #0,drxcen(a6)
        move.l  #0,drycen(a6)
        move.l  #$1000,cvx(a6)
        move.l  #$20000,cvy(a6)
        move.l  #$80000,radius(a6)
        move.l  #$8000,_i1(a6)
        move.l  #$8000,_i2(a6)
        move.l  #$8000,_i3(a6)
        move.l  #$8000,_i4(a6)
        move.l  #$8000,_i5(a6)
        move.l  #$8000,_i6(a6)
        move.l  #p_sines,wave_2(a6)
        move.l  #0,phase4(a6)
        move.l  #$21555,phase5(a6)
        move.l  #0,plot_mod(a6)
        move.l  #$1850000,vfb_xsca(a6)
        move.l  #$1850000,vfb_ysca(a6)
        move.l  #0,vfb_angl(a6)
        move.l  #$C00000,$18(a6)
        move.l  #$C00000,$1C(a6)
        move.l  #$780000,$20(a6)
        move.l  #0,4(a6)
        move.l  #$E00000,8(a6)
        move.l  #$C00000,vfb_xpos(a6)
        move.l  #$C00000,vfb_ypos(a6)
        move.l  #$8F000,colx(a6)
        move.l  #$8F000,coly(a6)
        move.l  #$10000,cvx2(a6)
        move.l  #$10000,cvy2(a6)
        move.l  #0,pixsize(a6)
        move.l  #$400000,phase4(a6)
        clr.l   thang(a6)
        move.l  #0,info(a6)
        clr.l   height(a6)
        clr.l   _mtrig(a6)
        lea     UPDA1F(a6),a5
        lea     results,a4
        lea     optionbu(a6),a3
        move    #$3F,d0 ; '?'

zlop:
        move.l  #$FFFF,(a5)+
        move    #0,(a4)+
        move.l  #0,(a3)+
        dbf     d0,zlop
        lea     $300(a6),a3
        lea     oscbank,a4
        move    #$1F,d0

gbsb:
        move.l  (a4)+,(a3)+
        dbf     d0,gbsb
        movem.l a0-a1,-(sp) ; Stash some values in the stack so we can restore them later.
        lea     $380(a6),a1
        lea     pixcon,a0
        move.l  #$6C,d0 ; 'l'
        jsr     copybloc
        movem.l (sp)+,a0-a1
        rts

; *******************************************************************
; startup
; Routine to initialize everything for the VLM.
; *******************************************************************
startup:
        jsr     VideoIni
        jsr     InitList ; Initialize blist, dlist, and ddlist.

        ; Store ddlist (which points to dlist) in the Object List Pointer. The 
        ; contents of dlist will be used by the Object PRocessor as the list of all objects
        ; to be painted to the screen.  See 'Frame'.
        move.l  ddlist,d0 ; Store ddlist in d0.
        move    #0,ODP ; We don't use the ODP.
        swap    d0 ; Swap the bytes for some reason.
        move.l  d0,OLP ; Point the Object Display List pointer to ddlist.

        ; Zero out the RAM at zerstart.
        lea     zerstart,a0
        move.l  #$794,d0
        lsr.l   #2,d0
cram:   clr.l   (a0)+
        sub.l   #1,d0
        bpl.s   cram

        move.b  #1,intmask
        ; Initialize the backing list we used for the 'blist' Object List.
        jsr     InitBeasties ; Initialize the beasties object list.

        move    #$FFFF,db_on ; Enable double-buffering.
        clr.l   screen_ready ; Signal that we don't have a screen ready for the Object Processor yet.
        move.l  #rrts,routine
        move.l  #rrts,_fx
        move.l  #Frame,($100).w ; Make Frame the vertical sync interrupt routine.
        move    n_vde,d0
        or.w    #1,d0
        move    d0,VI
        move.b  intmask,d0 ; Store the interrupt mask in d0.
        move    d0,INT1 ; Set the interrupt mask.
        move    sr,d0
        and.w   #$F8FF,d0
        move    d0,sr
        move    #$6C1,VMODE

        ; Clear the screens used for drawing to: cscreen and dscreen (double-buffered screen).
        movea.l #$100000,a0
        jsr     clrscreen
        movea.l #$148000,a0
        jsr     clrscreen
        move.l  #$100000,cscreen
        move.l  #$148000,dscreen
        move.l  dscreen,draw_screen

        ; Populate the p_sines and p_rect tables.
        lea     sines,a0 
        move.b  (a0),sines+256
        lea     p_sines,a1
        lea     p_rect,a2
        move    #$100,d0
mpsines:
        move.b  (a0)+,d1
        ext.w   d1
        move    d1,d2
        bpl.w   mrect1
        neg.w   d2
mrect1: lsl.w   #1,d2
        move.b  d2,(a2)+
        add.w   #$80,d1
        move.b  d1,(a1)+
        dbf     d0,mpsines ; Loop for 256 values.

        ; Populate the p_saw and p_ramp tables.
        lea     p_saw,a0
        lea     p_ramp,a1
        clr.w   d0
gsaw:   move    d0,d1
        move    d0,d2
        lsr.w   #1,d2
        move.b  d2,(a1)+
        sub.w   #$FF,d1
        bpl.w   gsaw1
        neg.w   d1
gsaw1:  move.b  d1,(a0)+
        add.w   #2,d0
        cmp.w   #$200,d0
        blt.s   gsaw

        move.b  p_saw,(p_square-4).l

        ; Populate the p_square table.
        lea     p_square,a0
        move    #$FF,d0
gsqu:   clr.w   d1
        cmp.w   #$80,d0
        blt.w   gsqu2
        move    #$FF,d1
gsqu2:  move.b  d1,(a0)+
        dbf     d0,gsqu

        move.b  p_square,p_square+$100

        ; Initialize the PAL fixups (ifnecessary).
        clr.w   palside
        clr.w   paltop
        tst.w   pal ; Are we on a PAL screen?
        beq.w   notpal1 ; If not, skip.
        move    #6,palside ; Set the X offset for PAL.
        move    #$1E,paltop ; Set the Y offset for PAL.

notpal1:
        rts

; *******************************************************************
; dumint
; *******************************************************************
dumint:
        move    #$101,INT1
        move    #$101,INT2
        rte

; *******************************************************************
; wsync
; *******************************************************************
wsync:
        move    frames,d7

; *******************************************************************
; wsnc
; *******************************************************************
wsnc:
        cmp.w   frames,d7
        beq.s   wsnc
        rts

; *******************************************************************
; clrscreen
; *******************************************************************
clrscreen:
        clr.w   d0
        clr.w   d1
        move    #$180,d2
        move    #$180,d3
        move    #$7700,d4
        bra.w   blitblock

; *******************************************************************
; makeit_rmw
; *******************************************************************
makeit_rmw:
        bsr.w   makeit
        move    #0,20(a0) ; Set index to postfixups list: make_rmw.
        rts

; *******************************************************************
; makeit_transparent
; *******************************************************************
makeit_transparent:
        bsr.w   makeit
        move    #1,20(a0) ; Set index to postfixups list: make_transparent.
        rts

; *******************************************************************
; makeit
; Make the entry in the beasties object display list.
; *******************************************************************
makeit:
        move    d5,14(a0) ; Set the Object Type (ObTypes).
        move.l  d0,(a0)    ; Set the X position.
        move.l  d1,4(a0)   ; Set the Y position.
        move    d3,8(a0)   ; Set the width?
        move    d4,10(a0)  ; Set the height?
        move.l  d2,16(a0) ; Set the pointer to screen data.
        move    #$FFFF,20(a0) ; Caller will set the postfixup.
        clr.w   22(a0)
        move    d5,12(a0) ; Set the object's mode (index to ModeVex).
        lsl.w   #3,d5
        lea     ObTypes,a1
        move    (a1,d5.w),$18(a0)
        move    2(a1,d5.w),$1A(a0)
        move    4(a1,d5.w),$1C(a0)
        clr.w   $1E(a0)
        rts

; *******************************************************************
; copybloc
; *******************************************************************
copybloc:
        movem.l d0/a0-a1,-(sp) ; Stash some values in the stack so we can restore them later.

copb:
        move.b  (a0)+,(a1)+
        dbf     d0,copb
        movem.l (sp)+,d0/a0-a1
        rts

; *******************************************************************
; gpus
; *******************************************************************
gpus:
        dc.l alpha, beta


; *******************************************************************
; thangg
; Unused.
; *******************************************************************
thangg:
        bsr.w   db
        movea.l draw_routine,a0
        jsr     (a0)
        move.l  #1,screen_ready
        addi.l  #1,fframes
        rts

; *******************************************************************
; wfm
; *******************************************************************
wfm:
        move    monx,d0
        move    mony,d1
        movea.l draw_screen,a0
        sub.w   #$40,d1 ; '@'
        move    #$40,d2 ; '@'
        move    #$40,d3 ; '@'
        move    #$C0,d4
        jsr     sblitblockk
        move.l  draw_screen,_ein_buf
        move    selected,d2
        lsl.w   #4,d2
        movea.l edwave,a0
        lea     (a0,d2.w),a0
        cmpi.w  #7,$E(a0)
        bge.w   joyxy
        move.l  a0,_ein_buf+4
        add.w   #$40,d1 ; '@'
        swap    d1
        move    d0,d1
zz0:
        move.l  d1,_ein_buf+8
        move    #5,_fmode
        bra.w   fmodewai

; *******************************************************************
; shspec
; *******************************************************************
shspec:
        movea.l draw_screen,a0
        move    mony,d5
        add.w   #$10,d5
        swap    d5
        move    #60,d0 ; '<'
        move    #3,d2
        lea     $F1B038,a1
        lea     absdelta,a1
        lea     maxes,a3
        move    #$3F,d5 ; '?'
        lea     avbank,a4
        move    band,d3
        lsl.w   #3,d3
        lea     (a4,d3.w),a4
        move    (a4),d3
        move    2(a4),d6
        cmp.w   d3,d6
        bpl.w   shfr1
        exg     d3,d6

shfr1:
        move    d3,bandl
        move    d6,bandh
        swap    d6
        move    d3,d6

shfr:
        move.l  (a1)+,d3
        and.l   #$FFFF,d3
        lsr.l   #8,d3
        add.w   #1,d3
        swap    d5
        move    d5,d1
        sub.w   d3,d1
        move    #$8FC0,d4
        swap    d5
        move    #$3F,d7 ; '?'
        sub.w   d5,d7
        cmp.w   d6,d7
        blt.w   isgrn
        swap    d6
        cmp.w   d6,d7
        bgt.w   isgrn0
        move    #$F0C0,d4

isgrn0:
        swap    d6

isgrn:
        jsr     sblitblockk
        add.w   #4,d0
        dbf     d5,shfr
        move    #60,d0 ; '<'
        move    bandl,d1
        lsl.w   #2,d1
        add.w   d1,d0
        move    bandh,d2
        lsl.w   #2,d2
        sub.w   d1,d2
        add.w   #3,d2
        move    #1,d3
        move.l  #$FFFF,d4
        lea     envvals,a1
        clr.l   d6
        move    band,d5
        lsl.w   #1,d5
        move    (a1,d5.w),d6
        lsr.w   #8,d6
        lsr.w   #1,d6
        move    mony,d1
        add.w   #$10,d1
        sub.w   d6,d1
        bmi.w   rrts
        jsr     sblitblockk
        move    #60,d0 ; '<'
        move    #$100,d2
        move    4(a4),d6
        lsr.w   #8,d6
        lsr.w   #1,d6
        move    mony,d1
        add.w   #$10,d1
        sub.w   d6,d1
        bmi.w   rrts
        move    #$88FF,d4
        jsr     sblitblockk
        move    6(a4),d6
        lsr.w   #8,d6
        move    mony,d1
        add.w   #$10,d1
        sub.w   d6,d1
        bmi.w   rrts
        move    #$4480,d4
        jsr     sblitblockk
        move    #$96,d0
        move    #$78,d1 ; 'x'
        move    #$A,d2
        move    #$A,d3
        move    #0,d5

zizz:
        move    #$F0F0,d4
        move.l  trig,d6
        btst    d5,d6
        beq.w   nggn
        move    #$8FF0,d4

nggn:
        bsr.w   sblitblockk
        add.w   #$10,d0
        add.w   #1,d5
        cmp.w   #5,d5
        bne.s   zizz
        rts

; *******************************************************************
; shenv
; *******************************************************************
shenv:
        move.l  #avbank,_ein_buf
        move.l  #envvals,_ein_buf+4
        move    #9,_fmode
        bsr.w   fmodewai
        rts

; *******************************************************************
; ?
; *******************************************************************
        movea.l draw_screen,a0
        move    mony,d5
        add.w   #$10,d5
        swap    d5
        move    #$50,d0 ; 'P'
        move    #$8FC0,d4
        move    #3,d2
        lea     envvals,a1
        move    #7,d5

shar:
        move    (a1)+,d3
        lsr.w   #8,d3
        lsr.w   #2,d3
        add.w   #1,d3
        swap    d5
        move    d5,d1
        sub.w   d3,d1
        jsr     sblitblockk
        add.w   #4,d0
        swap    d5
        dbf     d5,shar

; *******************************************************************
; shad
; *******************************************************************
shad:
        tst.l   aded
        beq.w   rrts
        move.l  aded,_ein_buf
        move    #1,_fmode
        bsr.w   fmodewai
        movea.l draw_screen,a0
        movea.l aded,a1
        lea     adsrcols,a2
        move    #$6E,d1 ; 'n'
        move    #$4C,d0 ; 'L'
        move    #7,d3
        move    #3,d5

shadsr:
        move    (a1)+,d2
        lsr.w   #8,d2
        add.w   #1,d2
        move    (a2)+,d4
        jsr     sblitblockk
        add.w   #9,d1
        dbf     d5,shadsr
        rts

; *******************************************************************
; adsrcols
; *******************************************************************
adsrcols:     dc.b   0
        dc.b $FF
        dc.b $F0
        dc.b $FF
        dc.b $8F
        dc.b $FF
        dc.b $80
        dc.b $FF

; *******************************************************************
; joyxy
; *******************************************************************
joyxy:
        bne.w   joyy
        move    pixcon,d2
        lsr.w   #2,d2
        and.w   #$3F,d2 ; '?'
        add.w   d2,d0
        move    #1,d2

zeb:
        move    #$88FF,d4
        movea.l draw_screen,a0
        jmp     sblitblockk

joyy:
        move    piycon,d3
        lsr.w   #2,d3
        and.w   #$3F,d3 ; '?'
        add.w   #$3F,d1 ; '?'
        sub.w   d3,d1
        move    #1,d3
        move    #$40,d2 ; '@'
        bra.s   zeb

; *******************************************************************
; domonito
; *******************************************************************
domonito:
        tst.w   _m
        beq.w   rrts
        bmi.w   wfm
        move.l  mon1,d1
        move.l  mon2,d2
        bpl.w   a_1
        cmp.l   d1,d2
        beq.w   rrts

a_1:    move    monx,d0
        move    mony,d1
        movea.l draw_screen,a0
        sub.w   #$40,d1 ; '@'
        move    #$40,d2 ; '@'
        move    #$40,d3 ; '@'
        move    #$C0,d4
        jsr     sblitblockk
        move.l  mon1,d6
        bmi.w   nox1
        lea     UPDA1F(a6),a5
        lsl.w   #2,d6
        move.b  2(a5,d6.w),d5
        lsr.w   #3,d5
        and.w   #$1F,d5
        movem.w d0/d5,-(sp) ; Stash some values in the stack so we can restore them later.
        add.w   #$20,d0 ; ' '
        sub.w   d5,d0
        move    #1,d2
        move    #$80C0,d4
        jsr     sblitblockk
        movem.w (sp)+,d0/d5
        move    d0,-(sp) ; Stash some values in the stack so we can restore them later.
        add.w   #$20,d0 ; ' '
        add.w   d5,d0
        jsr     sblitblockk
        move    (sp)+,d0

nox1:
        move.l  mon2,d6
        bmi.w   w2nope
        move    #$40,d2 ; '@'
        lea     UPDA1F(a6),a5
        lsl.w   #2,d6
        move.b  2(a5,d6.w),d5
        lsr.w   #3,d5
        and.w   #$1F,d5
        movem.w d1/d5,-(sp) ; Stash some values in the stack so we can restore them later.
        add.w   #$20,d1 ; ' '
        sub.w   d5,d1
        move    #1,d3
        move    #$80C0,d4
        jsr     sblitblockk
        movem.w (sp)+,d1/d5
        move    d1,-(sp) ; Stash some values in the stack so we can restore them later.
        add.w   #$20,d1 ; ' '
        add.w   d5,d1
        jsr     sblitblockk
        move    (sp)+,d1
        add.w   #$40,d1 ; '@'
        move.l  draw_screen,_ein_buf
        move.l  #monbuf,_ein_buf+4
        swap    d1
        move    d0,d1
        move.l  d1,-(sp) ; Stash some values in the stack so we can restore them later.
        move.l  d1,_ein_buf+8
        move    #$FF00,d0
        swap    d0
        move    monptr,d0
        move.l  d0,dword_1A6810
        move    #8,_fmode
        jsr     fmodewai
        move.l  draw_screen,_ein_buf
        move.l  #monbuf+$40,_ein_buf+4
        move.l  (sp)+,d1
        move.l  d1,_ein_buf+8
        move    #$8800,d0
        swap    d0
        move    monptr,d0
        move.l  d0,dword_1A6810
        move    #7,_fmode
        bsr.w   fmodewai
        move.l  #draw_screen,_ein_buf
        move.l  #monbuf,_ein_buf+4
        bra.w   w2sngl

; *******************************************************************
; fmodewai
; *******************************************************************
fmodewai:
        tst.w   _fmode
        bne.s   fmodewai
        rts

; *******************************************************************
; w2nope
; *******************************************************************
w2nope:
        add.w   #$40,d1 ; '@'
        move.l  draw_screen,_ein_buf
        move.l  #monbuf,_ein_buf+4
        swap    d1
        move    d0,d1

; *******************************************************************
; w2sngl
; *******************************************************************
w2sngl:
        move.l  d1,_ein_buf+8
        tst.l   mon1
        bmi.w   rrts
        move    #$8800,d0
        swap    d0
        move    monptr,d0
        move.l  d0,dword_1A6810
        move    #6,_fmode
        bra.s   fmodewai

; *******************************************************************
; varadd
; *******************************************************************
varadd:
        lsl.w   #3,d0
        move    d0,d2
        lsl.w   #2,d0
        add.w   d2,d0
        lea     pbinfo,a0
        lea     editinginfo-pbinfo(a0,d0.w),a0 ; "Parameter not yet defined    "
        rts

; *******************************************************************
; edvex
; *******************************************************************
edvex:
        dc.l crot, snglx, sisnglx, sidbl, dvect, rrts, wsshow, shspec

; *******************************************************************
; dvect
; *******************************************************************
dvect:
        bsr.w   avxy
        move    #8,d2
        lsr.w   d2,d0
        lsr.w   d2,d1
        sub.w   #$7F,d0
        sub.w   #$7F,d1
        add.w   #$BB,d0
        add.w   #$BB,d1
        move    #$A,d2
        move    #$A,d3
        move    #$80FF,d4
        movea.l draw_screen,a0
        jmp     sblitblockk

; ---------------------------------------------------------------------------
        move    d0,X_2
        move    d1,Y_2
        move    #$C0,X_1
        move    #$C0,Y_1
        move    #$88FF,LINCOL
        move.l  draw_screen,_ein_buf
        move.l  #X_1,_ein_buf+4
        move    #3,_fmode
        jsr     fmodewai
        movem.w (sp)+,d0-d1
        asr.w   #1,d0
        asr.w   #1,d1
        move    d0,d2
        move    d1,d3
        asr.w   #1,d2
        asr.w   #1,d3
        add.w   d1,d2
        sub.w   d0,d3
        add.w   #$C0,d2
        add.w   #$C0,d3
        move    d2,X_1
        move    d3,Y_1
        move    d0,-(sp) ; Stash some values in the stack so we can restore them later.
        move    #3,_fmode
        jsr     fmodewai
        move    (sp)+,d0
        move    d0,d2
        move    d1,d3
        asr.w   #1,d2
        asr.w   #1,d3
        sub.w   d1,d2
        add.w   d0,d3
        add.w   #$C0,d2
        add.w   #$C0,d3
        move    d2,X_1
        move    d3,Y_1
        move    #3,_fmode
        jsr     fmodewai
        rts

; *******************************************************************
; sidbl
; *******************************************************************
sidbl:
        bsr.w   avxy
        movem.l d0-d1,-(sp) ; Stash some values in the stack so we can restore them later.
        move    #$3E,d0 ; '>'
        move    #$BD,d1
        move    #$102,d2
        move    #7,d3
        move    #$80FF,d4
        movea.l draw_screen,a0
        jsr     sblitblockk
        move    #$3E,d1 ; '>'
        move    #$BD,d0
        move    #$102,d3
        move    #7,d2
        move    #$80FF,d4
        movea.l draw_screen,a0
        jsr     sblitblockk
        move.l  (sp)+,d5
        move    #$C0,d0
        move    #$BF,d1
        move    #$FFFF,d4
        move    #3,d3
        lsr.w   #8,d5
        sub.w   #$7F,d5
        bpl.w   n1add5
        add.w   d5,d0
        neg.w   d5

n1add5:
        add.w   #1,d5
        move    d5,d2
        jsr     sblitblockk
        move.l  (sp)+,d5
        move    #$C0,d1
        move    #$BF,d0
        move    #$FFFF,d4
        move    #3,d2
        lsr.w   #8,d5
        sub.w   #$7F,d5
        bpl.w   n2add5
        add.w   d5,d1
        neg.w   d5

n2add5:
        add.w   #1,d5
        move    d5,d3
        jmp     sblitblockk

; *******************************************************************
; sisnglx
; *******************************************************************
sisnglx:
        bsr.w   avx
        move.l  d1,-(sp) ; Stash some values in the stack so we can restore them later.
        move    #$3E,d0 ; '>'
        move    #$94,d1
        move    #$102,d2
        move    #$E,d3
        move    #$80FF,d4
        movea.l draw_screen,a0
        jsr     sblitblockk
        move.l  (sp)+,d5
        move    #$C0,d0
        move    #$96,d1
        move    #$FF00,d4
        move    #$A,d3
        lsr.w   #8,d5
        sub.w   #$7F,d5
        bpl.w   nadd5
        add.w   d5,d0
        neg.w   d5

nadd5:
        or.w    d5,d4
        add.w   #1,d5
        move    d5,d2
        jmp     sblitblockk

; *******************************************************************
; snglx
; *******************************************************************
snglx:
        bsr.w   avx
        lsr.l   #8,d1
        move    d1,-(sp) ; Stash some values in the stack so we can restore them later.
        move    #$3E,d0 ; '>'
        move    #$94,d1
        move    #$102,d2
        move    #$E,d3
        move    #$80FF,d4
        movea.l draw_screen,a0
        jsr     sblitblockk
        move    #$40,d0 ; '@'
        move    #$FF00,d4
        move    (sp)+,d2
        or.w    d2,d4
        add.w   #1,d2
        move    #$96,d1
        move    #$A,d3
        jsr     sblitblockk
        rts

; *******************************************************************
; wsshow
; *******************************************************************
wsshow:
        movea.l edwave,a1
        movea.l draw_screen,a0
        move    #$50,d0 ; 'P'
        move    #$71,d1 ; 'q'
        move    #7,d5

wssh:
        move.l  4(a1),d2
        move    #$58FF,d4
        move    #3,d3
        rol.l   #3,d2
        clr.w   d2
        swap    d2
        add.w   #1,d2
        jsr     sblitblockk
        move    (a1),d2
        and.w   #$FF,d2
        add.w   #1,d2
        move    #1,d3
        add.w   #1,d1
        move    #$80FF,d4
        jsr     sblitblockk
        lea     vfb_ysca(a1),a1
        add.w   #8,d1
        dbf     d5,wssh
        bsr.w   wfm
        rts

; *******************************************************************
; avx
; *******************************************************************
avx:
        movea.l eddie,a2
        movea.l fxobj,a3
        bsr.w   lnkchk
        move.l  a2,_ein_buf
        move.l  iixcon,d1
        lsr.l   #8,d1
        and.l   #$FFFF,d1
        move.l  d1,_ein_buf+4
        move.l  a3,_ein_buf+8
        move    #2,_fmode
        jmp     fmodewai

; *******************************************************************
; avxy
; *******************************************************************
avxy:
        movea.l eddie,a2
        move.b  SRCEN(a2),d0
        and.w   #$FF,d0
        lsl.w   #3,d0
        move    d0,d1
        lsl.w   #2,d1
        add.w   d1,d0
        lea     pbinfo,a1
        lea     editinginfo-pbinfo(a1,d0.w),a1 ; "Parameter not yet defined    "
        movea.l fxobj,a3
        bsr.w   lnkchk
        move.l  a2,_ein_buf
        move.l  iixcon,d1
        lsr.l   #8,d1
        and.l   #$FFFF,d1
        move    d1,-(sp) ; Stash some values in the stack so we can restore them later.
        move.l  d1,_ein_buf+4
        move.l  a3,_ein_buf+8
        move    #2,_fmode
        jsr     fmodewai
        movea.l fxobj,a3
        bsr.w   lnkchk2
        move.l  a1,_ein_buf
        move.l  iiycon,d1
        lsr.l   #8,d1
        and.l   #$FFFF,d1
        move.l  d1,_ein_buf+4
        move.l  a3,_ein_buf+8
        move    #2,_fmode
        jsr     fmodewai
        move    (sp)+,d0
        rts

; *******************************************************************
; lnkchk2
; *******************************************************************
lnkchk2:
        move    4(a1),d5
        bra.w   lnkch

; *******************************************************************
; lnkchk
; *******************************************************************
lnkchk:
        move    4(a2),d5

lnkch:
        lsl.w   #2,d5
        lea     UPDA1F(a3),a4
        tst.w   (a4,d5.w)
        beq.w   rrts
        lea     optionbu(a3),a3
        rts

; *******************************************************************
; crot
; *******************************************************************
crot:
        bsr.w   avxy
        move    4(a2),d2
        move    4(a1),d3
        lsl.w   #2,d2
        lsl.w   #2,d3
        move    (a3,d2.w),d0
        move    (a3,d3.w),d1
        move    d1,-(sp) ; Stash some values in the stack so we can restore them later.
        tst.w   d0
        bmi.w   novrt
        cmp.w   #$180,d0
        bge.w   novrt
        move    #0,d1
        move    #1,d2
        move    #$180,d3
        move    #$88FF,d4
        movea.l draw_screen,a0
        jsr     blitblock

novrt:
        move    (sp)+,d1
        bmi.w   nohrz
        cmp.w   #$180,d1
        bge.w   nohrz
        move    #0,d0
        move    #$180,d2
        move    #1,d3
        move    #$88FF,d4
        movea.l draw_screen,a0
        jsr     blitblock

nohrz:
        rts

; *******************************************************************
; thisfx
; *******************************************************************
thisfx:
        movea.l esp,a0
        move.l  #_thisfx,-(a0)
        move.l  a0,esp

_thisfx:
        lea     elspace,a0
        move    #5,(a0)+
        move    #0,(a0)+
        move.l  #bline2,(a0)+ ; "~g1:$20:Joypad to select, any FIRE to ed"...
        movea.l fx,a1
        move    #5,d7

lfx:
        move.l  (a1)+,d0
        bne.w   listit
        move.l  #empt,(a0)+ ; "<Empty>"
        move.l  #initedit,(a0)+
        bra.w   lfx2

listit:
        movea.l d0,a2
        move.l  info(a2),d6
        sub.w   #1,d6
        lea     vars,a3 ; "Draw a polygon object        "
        lsl.w   #2,d6
        movea.l (a3,d6.w),a3
        move.l  a3,(a0)+
        move.l  #edit2,(a0)+

lfx2:
        dbf     d7,lfx
        lea     elspace,a1
        lea     subfxhea,a0 ; "@~g1:SRCEN:Choose a subeffect slot to edit~"...
        bra.w   giedit

; *******************************************************************
; foredit
; *******************************************************************
foredit:
        movea.l esp,a0
        move.l  #fored,-(a0)
        move.l  a0,esp

fored:
        move.l  #fored,ledit
        clr.w   _m
        move    fxed,d0
        movea.l fx,a0
        lsl.w   #2,d0
        movea.l (a0,d0.w),a0
        move.l  info(a0),d6
        lea     vars,a1 ; "Draw a polygon object        "
        sub.w   #1,d6
        lsl.w   #2,d6
        movea.l (a1,d6.w),a1
        lea     $1E(a1),a1
        lea     wavedhea,a0 ; "@~g1:SRCEN:Editing: Wave Plotter~e3:3:"
gie:
        move.l  a0,-(sp) ; Stash some values in the stack so we can restore them later.
        lea     elspace,a0
        move    #$10,d0
        bsr.w   elcon
        lea     elspace,a1
        movea.l (sp)+,a0
        bra.w   giedit

; *******************************************************************
; ahead2
; *******************************************************************
ahead2:
        movea.l esp,a0
        move.l  #_ded2,-(a0)
        move.l  a0,esp

_ded2:
        lea     adsra,a0
        move    selected,d0
        move    d0,d1
        lsl.w   #3,d0
        lsl.w   #1,d1
        add.w   d1,d0
        lea     (a0,d0.w),a0
        move.l  a0,aded
        lea     option6,a1
        lea     adedhead2,a0
        bsr.w   giedit
        move    #$B,editing
        rts

; *******************************************************************
; ispec
; *******************************************************************
ispec:
        move.l  #isp1,ledit
        movea.l esp,a0
        move.l  #isp1,-(a0)
        move.l  a0,esp

isp1:
        move    #8,symed
        movea.l #isphead1,a0  ; "@~g1:SRCEN:Spectrum and Triggers~e3:3:"
        movea.l #option7,a1
        bsr.w   giedit
        move    #$E,editing
        rts

; *******************************************************************
; ispec2
; *******************************************************************
ispec2:
        move.l  #isp2,ledit
        movea.l esp,a0
        move.l  #isp2,-(a0)
        move.l  a0,esp

isp2:
        move    #8,symed
        movea.l #isphead2,a0  ; "@~g1:SRCEN:Trigger Settings~e3:3:"
        movea.l #option8,a1
        bsr.w   giedit
        rts

; *******************************************************************
; ispec3
; *******************************************************************
ispec3:
        move.l  #isp3,ledit
        movea.l esp,a0
        move.l  #isp3,-(a0)
        move.l  a0,esp

isp3:
        lea     isphead3,a0 ; "@~g1:SRCEN:Adjust width using joypad"
        bsr.w   print
        move    #$F,editing
        rts

; *******************************************************************
; ispec4
; *******************************************************************
ispec4:
        move.l  #isp4,ledit
        movea.l esp,a0
        move.l  #isp4,-(a0)
        move.l  a0,esp

isp4:
        lea     isphead4,a0
        bsr.w   print
        move    #$10,editing
        rts

; *******************************************************************
; wsedit
; *******************************************************************
wsedit:
        move.l  #wsed,ledit
        movea.l esp,a0
        move.l  #wsed,-(a0)
        move.l  a0,esp

wsed:
        movea.l fxedbase,a0
        lea     $300(a0),a0
        move.l  a0,edwave
        move    #7,symed
        lea     option4,a1
        lea     wshead,a0 ; "@~g1:SRCEN:Edit basic waveforms~e3:3:"
        bsr.w   giedit
        move.l  #wtud,action
        move    #$A,editing
        rts

; *******************************************************************
; dvfedit
; *******************************************************************
dvfedit:
        move.l  #dvfed,ledit
        movea.l esp,a0
        move.l  #dvfed,-(a0)
        move.l  a0,esp

dvfed:
        lea     dvfvars,a1
        clr.w   _m
        lea     elspace,a0
        move    #$10,d0
        bsr.w   elcon
        lea     elspace,a1
        lea     dvfedhea,a0
        bra.w   giedit

; *******************************************************************
; iiiogo
; *******************************************************************
iiiogo:
        move    selected,og
        bra.w   *+4
; ---------------------------------------------------------------------------

inogg:
        move.l  #iogo,ledit
        movea.l esp,a0
        move.l  #iogo,-(a0)
        move.l  a0,esp

iogo:
        lea     fxphead,a0 ; "@~g1:SRCEN:Choose fx page~e3:3:"
        lea     fxopt,a1
        bra.w   giedit

; *******************************************************************
; setp1
; *******************************************************************
setp1:
        move.l  #ssp1,ledit
        movea.l esp,a0
        move.l  #ssp1,-(a0)
        move.l  a0,esp

; *******************************************************************
; ssp1
; *******************************************************************
ssp1:
        lea     availobj,a1
        bra.w   doggo

; *******************************************************************
; setp2
; *******************************************************************
setp2:
        move.l  #ssp2,ledit
        movea.l esp,a0
        move.l  #ssp2,-(a0)
        move.l  a0,esp

ssp2:
        lea     avail2,a1

doggo:
        lea     ogohead,a0 ; "@~g1:SRCEN:Object Giver Outer~e3:3:"
        bra.w   giedit

; *******************************************************************
; symedit
; *******************************************************************
symedit:
        move.l  #simedit,ledit
        movea.l esp,a0
        move.l  #simedit,-(a0)
        move.l  a0,esp

simedit:
        clr.w   _m
        lea     symvars,a1
        lea     elspace,a0
        move    #$10,d0
        bsr.w   elcon
        movea.l #elspace,a1
        lea     symedhea,a0 ; "@~g1:SRCEN:Editing: Symmetry Generator~e3:3"...
        bra.w   giedit

; *******************************************************************
; edit2
; *******************************************************************
edit2:
        movea.l esp,a0
        move.l  #_edit2,-(a0)
        move.l  a0,esp
        move    selected,d0
        move    d0,fxed
        move    d0,og

_edit2:
        move    fxed,d0
        lsl.w   #2,d0
        movea.l fx,a0
        move.l  (a0,d0.w),fxedbase
        lea     edit2hea,a0
        lea     option2,a1
        bra.w   giedit

; *******************************************************************
; ahead1
; *******************************************************************
ahead1:
        movea.l esp,a0
        move.l  #_ah1,-(a0)
        move.l  a0,esp

_ah1:
        lea     adedhead,a0
        clr.l   aded
        lea     option5,a1
        bra.w   giedit

; *******************************************************************
; compr
; *******************************************************************
compr:
        movea.l esp,a0
        move.l  #_cmp1,-(a0)
        move.l  a0,esp

_cmp1:
        lea     stashhea,a0
        jsr     print
        bsr.w   dostash
        move    #$D,editing
        rts

; *******************************************************************
; rset
; *******************************************************************
rset:
        movea.l esp,a0
        move.l  #_rset,-(a0)
        move.l  a0,esp

_rset:
        lea     rsethead,a0 ; "@~g1:SRCEN:Reset ROM save pointers//Press a"...
        jsr     print
        move.l  #0,datn
        move.l  #$900200,datp
        move.l  #$900010,dattp
        move    #$D,editing
        rts

; *******************************************************************
; mset
; *******************************************************************
mset:
        movea.l esp,a0
        move.l  #_mset,-(a0)
        move.l  a0,esp

_mset:
        bsr.w   getmatrix
        move    #$D,editing
        rts

; *******************************************************************
; dostash
; *******************************************************************
dostash:
        lea     ((tp0+2)).l,a0 ; "~g1:6:"
        jsr     print
        movea.l dattp,a0
        move.l  datp,d0
        move.l  d0,(a0)+
        move.l  a0,dattp
        addi.l  #1,datn
        move    #1,cursx
        lea     tp1,a0
        jsr     print
        lea     refblock,a0
        clr.l   d6
        move    #$17FF,d1
        movea.l datp,a2
        lea     matrix,a1
        move.l  a1,cskr
        movem.l a0-a1,-(sp) ; Stash some values in the stack so we can restore them later.
        bsr.w   deltablo
        move.l  d0,aded+$10
        movem.l (sp)+,a0-a1
        move.l  a2,datp
        lea     (fxedbase-2).l,a4
        jsr     xxnum
        movea.l a4,a0
        jsr     print
        lea     tp2,a0
        jsr     print
        move    #7,d7
        move.l  #$1D9BF8,udud+$10

remblox:
        move    d7,-(sp) ; Stash some values in the stack so we can restore them later.
        move    #1,cursx
        lea     tp1,a0
        jsr     print
        movea.l cskr,a0
        movea.l udud+$10,a1
        move    #$17FF,d1
        movea.l datp,a2
        movem.l a0-a1,-(sp) ; Stash some values in the stack so we can restore them later.
        bsr.w   deltablo
        add.l   d0,cskr+8
        movem.l (sp)+,a0-a1
        move.l  udud+$10,cskr
        addi.l  #$1800,udud+$10
        move.l  a2,datp
        lea     (fxedbase-2).l,a4
        jsr     xxnum
        movea.l a4,a0
        jsr     print
        lea     tp2,a0
        jsr     print
        move    (sp)+,d7
        dbf     d7,remblox
        move    #1,cursx
        lea     tp3,a0
        jsr     print
        move.l  cskr+8,d0
        lea     (fxedbase-2).l,a4
        jsr     xxnum
        movea.l a4,a0
        jsr     print
        lea     tp2,a0
        jsr     print
        rts

; *******************************************************************
; edline
; *******************************************************************
edline:
        movea.l esp,a0
        move.l  #_edl,-(a0)
        move.l  a0,esp

_edl:
        move    delayf,d0
        bpl.w   eddd
        clr.w   d0

eddd:
        move    d0,dlset+2
        move.l  #dlset,editlist
        lea     edlhead,a0
        jsr     print
        move    #$C,editing

udedg:
        lea     edlgauge,a0
        lea     edlgauge+6-edlgauge(a0),a1 ; "~g3:5:                                 "
        move    #$1F,d0

udedg1:
        move.b  #1,(a1)+
        dbf     d0,udedg1
        lea     6(a0),a1
        move    delayt,d1
        move    delayn,d2
        clr.w   d0

shado:
        add.w   d1,d0
        bsr.w   setblok
        dbf     d2,shado
        bra.w   print


; *******************************************************************
; setblok
; *******************************************************************
setblok:
        and.w   #$3F,d0 ; '?'
        move    d0,d3
        lsr.w   #1,d3
        lea     (a1,d3.w),a2
        move.b  (a2),d4
        sub.w   #1,d4
        btst    #0,d0
        beq.w   d0notodd
        add.w   #4,d4

d0notodd:
        lea     evens,a3
        and.w   #7,d4
        move.b  (a3,d4.w),(a2)
        rts

; *******************************************************************
; evens
; *******************************************************************
evens:
        dc.b $02,$02,$04,$04
        btst    d1,d4
        btst    d1,d4

; *******************************************************************
; wfa
; *******************************************************************
wfa:
        bsr.w   estack
        movea.l eddie,a2
        btst    #0,(a2)
        bne.w   rwfa
        clr.w   symed
        bra.w   iawfx

; *******************************************************************
; rwfa
; *******************************************************************
rwfa:

        lea     wfahead,a0
        lea     option3,a1
        clr.w   symed
        move    #1,cbuttf
        bra.w   ogiedit

; *******************************************************************
; initedit
; *******************************************************************
initedit:
        move.l  #e_stac,esp
        move.l  #0,e_stac
        clr.w   sec_cnt
        move    #1,sec_en
        bra.w   ispec

; *******************************************************************
; old_edit
; *******************************************************************
old_edit:
        move.l  #e_stac,esp
        move.l  #0,e_stac
        clr.w   sec_en
        movea.l esp,a0
        move.l  #_iied,-(a0)
        move.l  a0,esp

_iied:
        lea     edithead,a0 ; "@~g1:SRCEN:Edit Mode~e3:$04:"
        lea     option1,a1
        clr.w   symed

; *******************************************************************
; giedit
; *******************************************************************
giedit:
        clr.w   cbuttf

ogiedit:
        move    #$FFFF,actime
        move.l  a1,editlist
        bsr.w   print
        move    #1,editing
        rts

; *******************************************************************
; wtud
; *******************************************************************
wtud:
        lea     wt,a0 ; "~g2:12:"
        jsr     print
        move    selected,d0
        lsl.w   #4,d0
        movea.l edwave,a1
        lea     (a1,d0.w),a1
        move    $E(a1),d0
        lsl.w   #2,d0
        lea     wts,a2
        movea.l (a2,d0.w),a0
        cmp.w   #24,d0
        beq.w   grint
        move.l  $24(a2,d0.w),8(a1)

grint:
        jsr     print

ud_selcu:
        move    cursorx,d0
        move    cursory,d1
        move.b  #$20,d2 ; ' '
        lea     board,a0
        jsr     charblit
        move    selx,d0
        move    sely,d1
        add.w   selected,d1
        sub.w   #1,d0
        move    d0,cursorx
        move    d1,cursory
        clr.b   d2
        jmp     charblit

; *******************************************************************
; reedit
; *******************************************************************
reedit:
        bsr.w   eunstack
        bra.w   eparam


eunstack:
        movea.l esp,a0
        move.l  (a0)+,editlist
        move    (a0)+,selected
        move    (a0)+,selectab
        move.l  a0,esp
        rts

; *******************************************************************
; estack
; *******************************************************************
estack:
        movea.l esp,a0
        move    selectab,-(a0)
        move    selected,-(a0)
        move.l  editlist,-(a0)
        move.l  a0,esp
        rts

; *******************************************************************
; eparam
; *******************************************************************
eparam:
        clr.l   ixcon+4
        clr.l   iycon+4
        move    selected,d0
        lsl.w   #2,d0
        lea     elvex,a0
        movea.l (a0,d0.w),a0
        move.l  a0,eddie
        move.b  2(a0),d0
        and.w   #$FF,d0
        cmp.w   #7,d0
        beq.w   init_byt
        cmp.w   #6,d0
        bne.w   crunt
        movea.l 6(a0),a1
        jmp     (a1)

init_byt:
        lea     awfb2,a0
        lea     padbits,a1
        move.l  _mtrig(a6),d2
        move    #7,d7

imabit:
        move.b  (a1)+,d1
        and.w   #$FF,d1
        lsl.w   #2,d1
        lea     (a0,d1.w),a2
        bclr    #7,2(a2)
        btst    #0,d2
        beq.w   imooff
        bset    #7,2(a2)

imooff:
        move.b  #0,3(a2)
        lsr.w   #1,d2
        dbf     d7,imabit
        move.l  #(sympad+3+$3D),cpad
        move.l  #awfb2,cpad+4
        move    #6,symed
        move    #7,editing
        lea     bytemask,a0
        bsr.w   cprint
        bra.w   ud_butts

; *******************************************************************
; init_sym
; *******************************************************************
init_sym:
        bsr.w   isymbut
        move    d0,symed
        add.w   #1,d0
        move    d0,editing
        lea     symplane,a0 ; "Editing: Symmetry Planes and Types//Pre"...
        bsr.w   cprint
        bsr.w   ppmes
        bra.w   ud_butts

; *******************************************************************
; crunt
; *******************************************************************
crunt:
        move    d0,symed
        move    d0,d1
        sub.w   #1,d1
        add.w   #1,d0
        move    d0,editing
        bsr.w   primexy
        move.l  a0,-(sp) ; Stash some values in the stack so we can restore them later.
        lea     eparm0,a0
        bsr.w   cprint
        movea.l (sp)+,a0
        lea     -$1E(a0),a0
        bsr.w   print
        lea     eparm2,a0 ; "~g1:$18:Press ~i+*~i- to attach waveform"...
        bsr.w   print

; *******************************************************************
; ppmes
; *******************************************************************
ppmes:
        movea.l #eparm1,a0  ; "~g1:$20:<A> Prev   <B> Menu   <C> Next"
        bra.w   print

; *******************************************************************
; isymbut
; *******************************************************************
isymbut:
        lea     symbutts,a0
        lea     padbits,a1
        move.l  asym_fla(a6),d2
        move    #7,d7

isybit:
        move.b  (a1)+,d1
        and.w   #$FF,d1
        lsl.w   #2,d1
        lea     (a0,d1.w),a2
        bclr    #7,2(a2)
        btst    #0,d2
        beq.w   isooff
        bset    #7,2(a2)

isooff:
        move.b  #0,3(a2)
        lsr.w   #1,d2
        dbf     d7,isybit
        lea     20(a0),a2
        bclr    #7,2(a2)
        btst    #8,d2
        beq.w   idun
        bset    #7,2(a2)

idun:
        move.l  #sympad,cpad
        move.l  #symbutts,eddie+$10
        rts

; *******************************************************************
; primexy
; *******************************************************************
primexy:
        move    selected,lselecte
        movea.l fxobj,a3
        move    4(a0),d2
        move    d2,monitor
        addi.w  #1,monitor
        move    #1,_m
        move.l  6(a0),d3
        lea     ixcon,a2
        bsr.w   s16
        movea.l fxobj,a3
        btst    #0,(a0)
        beq.w   rrts
        move.b  SRCEN(a0),d0
        and.w   #$FF,d0
        lsl.w   #3,d0
        move    d0,d2
        lsl.w   #2,d0
        add.w   d2,d0
        lea     pbinfo,a0
        lea     editinginfo-pbinfo(a0,d0.w),a0 ; "Parameter not yet defined    "
        move    4(a0),d2
        move.l  6(a0),d3
        lea     iycon,a2

; *******************************************************************
; s16
; *******************************************************************
s16:
        move    4(a0),d0
        lsl.w   #2,d0
        lea     UPDA1F(a3),a4
        lea     (a3,d0.w),a3
        tst.w   (a4,d0.w)
        beq.w   shall
        lea     optionbu(a3),a3

shall:
        move.l  (a3),d7
        move.b  3(a0),d0
        btst    #7,d0
        beq.w   nsiggn
        move.l  d3,d4
        lsr.l   #1,d4
        add.l   d4,d7

nsiggn:
        and.w   #$7F,d0
        lsl.w   #2,d0
        lea     vtypes,a3
        movea.l (a3,d0.w),a3
        jmp     (a3)

; *******************************************************************
; vtypes
; *******************************************************************
vtypes:
        dc.l xbyte, xword, x1616

; *******************************************************************
; xbyte
; *******************************************************************
xbyte:  rts

; *******************************************************************
; xword
; *******************************************************************
xword:  swap    d7
        divu.w  d3,d7
zach:   and.l   #$FFFF,d7
        lsl.l   #8,d7
        move.l  d7,(a2)
        rts

; *******************************************************************
; x1616
; *******************************************************************
x1616:
        lsr.l   #8,d3
        lsr.l   #4,d3
        divu.w  d3,d7
        lsl.l   #8,d7
        lsl.l   #4,d7
        and.l   #$FFFFFF,d7
        move.l  d7,(a2)
        rts

; *******************************************************************
; unquit
; *******************************************************************
unquit:
        lea     unimp,a0 ; "This function not yet implemented~c30:"
        bra.w   eq

editquit:
        lea     normal,a0

eq:
        bsr.w   cprint
        clr.w   editing
        clr.l   star_on
        move    ovlm_mod,vlm_mode
        rts

; *******************************************************************
; cprint
; *******************************************************************
cprint:
        move.l  a0,-(sp) ; Stash some values in the stack so we can restore them later.
        lea     clearhom,a0
        bsr.w   print
        movea.l (sp)+,a0

; *******************************************************************
; print
; *******************************************************************
print:
        movea.l a0,a4
        movea.l #board,a0
        move    cursx,d0
        move    cursy,d1

prnt:   move.b  (a4)+,d2
        beq.w   strngend
        cmp.b   #$2F,d2 ; '/'
        bne.w   pcmd1
        add.w   #1,d1
        move    cursx,d0
        bra.s   prnt

pcmd1:
        cmp.b   #$40,d2 ; '@'
        bne.w   pcmd2
        lea     board,a0
        movem.w d0-d1,-(sp) ; Stash some values in the stack so we can restore them later.
        jsr     cleol
        movem.w (sp)+,d0-d1
        bra.s   prnt

pcmd2:
        cmp.b   #$5E,d2 ; '^'
        bne.w   pcmd3
        jsr     cleol
        bra.s   prnt

pcmd3:
        cmp.b   #$60,d2 ; '`'
        bne.w   pcmd4
        sub.w   #1,d1
        move    cursx,d0
        bra.s   prnt

pcmd4:
        cmp.b   #$7E,d2 ; '~'
        bne.w   pcmd5
        move.b  (a4)+,d2
        cmp.b   #$63,d2 ; 'c'
        beq.w   aclear
        cmp.b   #$67,d2 ; 'g'
        beq.w   goto
        cmp.b   #$65,d2 ; 'e'
        beq.w   edtable
        cmp.b   #$69,d2 ; 'i'
        beq.w   setinv
        bra.w   prnt

setinv:
        move.b  (a4)+,d2
        cmp.b   #$2B,d2 ; '+'
        beq.w   invon
        clr.w   inverse
        bra.w   prnt

invon:
        move    #1,inverse
        bra.w   prnt

edtable:
        movea.l editlist,a5
        move.l  a4,-(sp) ; Stash some values in the stack so we can restore them later.
        movea.l 4(a5),a4
        bsr.w   prnt
        movea.l (sp)+,a4
        bsr.w   gnum
        move    d2,d0
        move    d2,selx
        bsr.w   gnum
        move    d2,d1
        move    d2,sely
        move.l  a4,-(sp) ; Stash some values in the stack so we can restore them later.
        movea.l editlist,a5
        move    (a5)+,d3
        move    d3,selectab
        move    (a5)+,selected
        lea     4(a5),a5

dedtab:
        movea.l (a5)+,a4
        lea     4(a5),a5
        move    d3,-(sp) ; Stash some values in the stack so we can restore them later.
        bsr.w   prnt
        move    (sp)+,d3
        add.w   #1,d1
        move    selx,d0
        dbf     d3,dedtab
        clr.w   d2
        sub.w   #1,d0
        move    selected,d1
        add.w   sely,d1
        move    d0,cursorx
        move    d1,cursory
        jsr     charblit
        movea.l (sp)+,a4
        bra.w   prnt

goto:
        bsr.w   gnum
        move    d2,d0
        bsr.w   gnum
        move    d2,d1
        bra.w   prnt

aclear:
        bsr.w   gnum
        move    d2,actime
        bra.w   prnt

pcmd5:
        jsr     charblit
        add.w   #1,d0
        bra.w   prnt

strngend:
        move    d0,cursx
        move    d1,cursy
        rts

; *******************************************************************
; gnum
; *******************************************************************
gnum:
        clr.l   d2
        clr.l   d3

gnuml:
        move.b  (a4)+,d3
        cmp.b   #$3A,d3 ; ':'
        beq.w   rrts
        move.l  d2,d4
        lsl.l   #3,d2
        lsl.l   #1,d4
        add.l   d4,d2
        sub.b   #$30,d3 ; '0'
        add.l   d3,d2
        bra.s   gnuml

; *******************************************************************
; dodraw
; Unused.
; *******************************************************************
dodraw:
        move.l  #1,screen_ready
        move    #1,db_on
        clr.w   x_end

gog:    bsr.w   thangg
        tst.w   meltim
        bpl.s   gog
        move.l  #rrts,routine
        rts

; *******************************************************************
; db
; Unused.
; *******************************************************************
db:
        move.l  #1,sync
dboo:   tst.l   sync
        bne.s   dboo
        move.l  dscreen,draw_screen
        rts

; *******************************************************************
; mfade
; Unused.
; *******************************************************************
mfade:
        move.l  #mymelt,draw_routine
        move    #60,d0 ; '<'
        move    skale,d1
        lsl.w   d1,d0
        move    d0,meltim
        bra.s   dodraw

; *******************************************************************
; mymelt
; Unused.
; *******************************************************************
mymelt:
        subi.w  #1,meltim
        bsr.w   WaitBlit
        movea.l cscreen,a0
        movea.l dscreen,a1
        tst.w   skale
        bne.w   lilmelt
        move.l  a1,A2_BASE
        move.l  #$14420,A2_FLAGS
        move    #$10,d0
        swap    d0
        move    #$10,d0
        move.l  d0,A2_PIXEL
        move    #1,d0
        swap    d0
        move    #$FEA0,d0
        move.l  d0,A2_STEP
        move.l  a0,A1_BASE
        move.l  #$34420,A1_FLAGS
        move    #$16,d0
        swap    d0
        move    #$16,d0
        move.l  d0,A1_PIXEL
        move.l  #0,A1_INC
        move.l  #$F800,A1_FINC
        move    #0,d0
        swap    d0
        move    #$FEAB,d0
        move.l  d0,A1_STEP
        move.l  #$F8000000,A1_FSTEP
        move.l  #$FC0000,d0
        move.l  d0,B_IINC
        move.l  d0,$F02274
        move.l  d0,$F02278
        move.l  d0,$F0227C
        move    #$160,d0
        swap    d0
        move    #$160,d0
        move.l  d0,B_COUNT
        move.l  #$41802F01,B_CMD
        bra.w   WaitBlit

lilmelt:
        cmpi.w  #2,skale
        beq.w   justfade
        move.l  a1,A2_BASE
        move.l  #$14420,A2_FLAGS
        move    #0,d0
        swap    d0
        move    #0,d0
        move.l  d0,A2_PIXEL
        move    #1,d0
        swap    d0
        move    #$FF40,d0
        move.l  d0,A2_STEP
        move.l  a0,A1_BASE
        move.l  #$34420,A1_FLAGS
        move    #3,d0
        swap    d0
        move    #4,d0
        move.l  d0,A1_PIXEL
        move.l  #0,A1_FPIXE
        move.l  #0,A1_INC
        move.l  #$F800,A1_FINC
        move    #0,d0
        swap    d0
        move    #$FF46,d0
        move.l  d0,A1_STEP
        move.l  #$F8000000,A1_FSTEP
        move.l  #$FE0000,d0
        move.l  d0,B_IINC
        move.l  d0,$F02274
        move.l  d0,$F02278
        move.l  d0,$F0227C
        move    #$C0,d0
        swap    d0
        move    #$C0,d0
        move.l  d0,B_COUNT
        move.l  #$41802F01,B_CMD
        bra.w   WaitBlit

justfade:
        move.l  a1,A2_BASE
        move.l  #$4420,A2_FLAGS
        move    #0,d0
        swap    d0
        move    #0,d0
        move.l  d0,A2_PIXEL
        move    #1,d0
        swap    d0
        move    #$FE80,d0
        move.l  d0,A2_STEP
        move.l  a0,A1_BASE
        move.l  #$4420,A1_FLAGS
        move    #0,d0
        swap    d0
        move    #0,d0
        move.l  d0,A1_PIXEL
        move    #1,d0
        swap    d0
        move    #$FE80,d0
        move.l  d0,A1_STEP
        move.l  #$FE0000,d0
        move.l  d0,B_IINC
        move.l  d0,$F02274
        move.l  d0,$F02278
        move.l  d0,$F0227C
        move    #$180,d0
        swap    d0
        move    #$180,d0
        move.l  d0,B_COUNT
        move.l  #$41802E01,B_CMD
        bra.w   WaitBlit
        rts

; *******************************************************************
; Frame
; 'Frame' is called at every vertical sync interrupt (also known as a vertical
; blank), which might be up to 30 times per second.
;
; This routine is responsible for refreshing the Object List used by the
; Jaguar's Object Processor to draw stuff on the screen. When it has done that
; it builds the basis of a new Object List in 'RunBeasties' called 'blist'.
; 'Frame' also checks for input from the joypad controller.
;
; The counterpart to this routine is 'goagain', which is VLM's main loop. 
; 'goagain' is responsible for preparing the Object List (blist) itself
; containing all the items to be drawn and calling the required GPU modules
; (see omega.gas) to prepare the pixel data that each Object references (for
; example, draw_screen). Once 'goagain' has a new blist ready to go it signals
; to 'Frame' using the 'sync' and 'screen ready' variables.
; *******************************************************************
Frame:
        movem.l d0-d5/a0-a2,-(sp) ; Stash some values in the stack so we can restore them later.

        ; Check if we're in a vertical blank or not.
        move    INT1,d0
        move    d0,-(sp) ; Stash some values in the stack so we can restore them later.
        btst    #0,d0         ; Are we in a vertical blank?
        beq.w   CheckTimer      ; If not, skip everything and check for input only.

        addi.w  #1,frames
        tst.w   clut_sha
        beq.w   ncc
        move    clut_sha,$F00482
        clr.w   clut_sha

        ; We're in a vertical blank so can update some state.
        ; Copy the display list we built in 'blist' to 'dlist'. The
        ; display list will be used by the Objects Processor to paint
        ; the next frame.
        ; blist is the the shadow hardware display list.
        ; dlist is the hardware display list.
ncc:    movem.l d6-d7/a3-a6,-(sp) ; Stash some values in the stack so we can restore them later.
        movea.l blist,a0; Stash blist in a0.
        movea.l dlist,a1; Stash dlist in a1.

        moveq   #$40,d0      ; 0x40 units of 4 bytes each to be copied.
xlst:   move.l  (a0)+,(a1)+  ; Copy 4 bytes from blist to dlist.
        dbf     d0,xlst      ; Keep copying until we run out of bytes.

        ; Build the display list in 'blist' from 'beasties' for the next frame.
        ; The 'beasties' list is populated mostly in 'everything' which runs in our 'goagain' mainloop.
        bsr.w   RunBeasties

        ; In our 'goagain' mainloop we prepare draw_screen (double-buffered screen) with
        ; objects drawn by the GPU/Blitter. Once it's ready we set
        ; screen_ready. Here we check if screen_ready is set and if so, we swap
        ; draw_screen into 'cscreen' (current screen). We will then update the
        ; main screen object in the Display List to reference this new address.

        ; Check if we can swap in the new screen prepared in 'omega.gas'.
        tst.l   screen_ready; has 'omega' finished preparing a new screen?
        beq.w   no_new_screen   ; If no, go to no_new_screen.
        tst.l   sync        ; Has omega.gas signalled it safe to swap screens?
        beq.w   no_new_screen   ; If no, go to new_screen.

        ; Swap draw_screen into cscreen so that the new screen can be used in the Object List.
        ; Note that dscreen points to draw_screen so it is draw_screen we are swapping in here.
        move.l  cscreen,d1            ; Stash cscreen in d1.                          
        move.l  dscreen,cscreen   ; Overwrite cscreen with dscreen.
        move.l  d1,dscreen            ; Overwrite dscreen with stashed cscreen.
        move.l  d1,draw_screen        ; Overwrite draw_screen with stashed cscreen.
        clr.l   screen_ready          ; Signal that a new screen is required before we come here again.
        clr.l   sync                  ; Signal to omega it can build a new screen.

no_new_screen:
        move.l  cscreen,d5
        movea.l dlist,a0 ; Point a0 at the display list
        move    db_on,d7 ;  Is double-buffering enabled
        bmi.w   no_db        ; If not, skip to warp flash.
        tst.w   scron
        beq.w   stdb
        bpl.w   no_db
        clr.w   scron
        bra.w   no_db

        ; Update the main screen item in the Object List with the address of the new screen contained
        ; in cscreen. This has the effect of ensuring all the objects we drew with the GPU
        ; and Blitter in 'mainloop' are actually written to the screen by the Object Processor.
stdb:   move.l  d5,d6
        add.l   hango,d6
        and.l   #$FFFFFFF8,d6 ; lose three LSB's
        lsl.l   #8,d6         ; move to correct bit position
        move.l  (a0),d1       ; get first word of the BMO
        and.l   #$7FF,d1      ; clear data pointer
        or.l    d6,d1         ; mask in new pointer
        move.l  d1,(a0)       ; replace in OL
        lea     $40(a0),a0    ; This skips to the next object in the object list.
        add.l   dbadj,d5 ; Move to the next object in cscreen. 
        dbf     d7,stdb ; Keep looping until we've done all double-buffered screens.

no_db:  tst.w   flash
        beq.w   cflash
        subi.w  #$10,flash
        move    flash,d0
        or.w    #$7700,d0
        move    d0,d1
        swap    d0
        move    d1,d0
        move.l  d0,BG
        move.l  d0,BG+4
        tst.w   flash
        bne.w   cflash
        move    #$FFFF,scron

cflash: cmpi.w  #1,vlm_mode
        bne.w   dflash
        move.l  pad_now,d0
        and.l   #$2000,d0
        beq.w   dflash
        tst.w   flash_db
        bne.w   eflash
        move    #1,flash_db
        move    #$100,flash
        move    #1,scron
        bra.w   eflash

dflash:
        clr.w   flash_db

eflash:
        tst.w   vlmtim
        bmi.w   ncanc
        subi.w  #1,vlmtim
        bpl.w   ncanc
        move    #$FFFF,davesobj+204

        ; Check for input from the controller.
ncanc:  jsr     readpad
        tst.w   seldb
        beq.w   do_ed
        move.l  pad_now,d1
        or.l    pad_now+4,d1
        and.l   #$22FE20FF,d1
        bne.w   no_ksel
        clr.w   seldb

do_ed:
        tst.w   vlm_mode ; Are the VLM controls active?
        beq.w   no_ed ; If not, skip to no_ed.

        ; The VLM edit controls are active.
        move    editing,d0 ; Put current editing mode in d0.
        beq.w   no_ed ; If none, skip to no_end.
        tst.l   action ; Are we in the middle of an action?
        bne.w   no_ksel ; If we are, skip to no_ksel.
        subq.w  #1,d0 ; Subtract 1 from the current editin gmode.
        lea     editvex,a0 ; Point a0 at edit vex.
        lsl.w   #2,d0 ; Multiply d0 by 4.
        movea.l (a0,d0.w),a0 ; Use d0 as an index into 'editvex' to get the edit routine.
        jsr     (a0) ; Run the edit routine.
        bra.w   no_ksel ; Skip over the 'not editing' activity below.

        ; We're not in editing mode.
no_ed:  tst.l   action
        bne.w   no_ksel
        move.l  pad_now,d0
        and.l   #$200,d0
        bra.w   nothash

vlm_on: move    #1,seldb
        move    #1,vlm_mode
        move    #1,beasties+140
        move    #7,davesobj+204
        move    #$1F4,vlmtim

nothash:
        tst.w   editing
        bne.w   no_ksel
        move.l  pad_now,d0
        move.l  d0,d1
        move.l  d1,d0
        and.l   #$10000,d0
        beq.w   knobby
        move.l  d1,d0
        and.l   #$E00FE,d0
        beq.w   no_ksel
        bsr.w   dcode
        and.w   #$FF,d2
        sub.b   #$30,d2 ; '0'
        bne.w   pharty
        bra.w   no_ksel

pharty:
        move    star_on+2,star_on
        move    d2,star_on+2
        move    #1,seldb
        bra.w   no_ksel

knobby:
        tst.l   star_on
        beq.w   no_ksel
        move    star_on,d0
        bne.w   setbth
        move    star_on+2,skid
        move.l  #skidoo,action
        bra.w   n_ks

setbth:
        sub.w   #1,d0
        move    d0,imatrix ; Update Bank number
        move    star_on+2,skid
        move.l  #gm,action

n_ks:
        clr.l   star_on

no_ksel:
        move.l  pad_now,d0 ; Get button press.
        cmp.l   #$90018,d0 ; Was edit selected?
        bne.w   nse1 ; Edit not selected
        jsr     setedit ; Enable Edit mode.
        bra.w   nse2 ; Skip next line, leave editing enabled.

nse1:   clr.w   vedit ; Clear edit enabled.

        ; Perform whatever the current 'fx' routine is. Can be one of:
        ;   - symadj, keydb, ov, rrts.
nse2:   movea.l _fx,a0
        jsr     (a0)
        tst.l   action
        bne.w   gharbaj

        ; Perform whatever the current 'routine' is. Can be one of:
        ;   - symadj, keydb, ov, rrts.
        movea.l routine,a0
        jsr     (a0)

gharbaj:
        movem.l (sp)+,d6-d7/a3-a6 ; Restore stashed values from the stack.

CheckTimer:
        move    (sp)+,d0 ; Restore stashed values from the stack.
        move    d0,-(sp) ; Stash some values in the stack so we can restore them later.
        btst    #3,d0
        beq.w   *+4

exxit:  move    (sp)+,d0 ; Restore stashed values from the stack.
        lsl.w   #8,d0
        move.b  intmask,d0
        move    d0,INT1
        move    d0,INT2
        movem.l (sp)+,d0-d5/a0-a2 ; Restore stashed values from the stack.
        rte

; *******************************************************************
; ski1
; *******************************************************************
ski1:
        move    #1,d0
        bra.w   selett
; ---------------------------------------------------------------------------
ski2:
        move    #2,d0
        bra.w   selett
; ---------------------------------------------------------------------------
ski3:
        move    #3,d0
        bra.w   selett
; ---------------------------------------------------------------------------
ski4:
        move    #4,d0
        bra.w   selett
; ---------------------------------------------------------------------------
ski5:
        move    #5,d0
        bra.w   selett
; ---------------------------------------------------------------------------
ski6:
        move    #6,d0
        bra.w   selett
; ---------------------------------------------------------------------------
ski7:
        move    #7,d0
        bra.w   selett
; ---------------------------------------------------------------------------
ski8:
        move    #8,d0
        bra.w   selett
; ---------------------------------------------------------------------------
ski9:
        move    #9,d0
        bra.w   selett
; ---------------------------------------------------------------------------
gski1:
        move    #0,d0
        rts
; ---------------------------------------------------------------------------
gski2:
        move    #1,d0
        rts
; ---------------------------------------------------------------------------
gski3:
        move    #2,d0
        rts
; ---------------------------------------------------------------------------
gski4:
        move    #3,d0
        rts
; ---------------------------------------------------------------------------
gski5:
        move    #4,d0
        rts
; ---------------------------------------------------------------------------
gski6:
        move    #5,d0
        rts
; ---------------------------------------------------------------------------
gski7:
        move    #6,d0
        rts
; ---------------------------------------------------------------------------
gski8:
        move    #7,d0
        rts
; ---------------------------------------------------------------------------
gski9:
        move    #8,d0
        rts

; *******************************************************************
; selmat
; *******************************************************************
selmat:
        cmp.w   #7,d0
        bgt.w   rrts
        move.l  #getmatrix,action
        move    d0,imatrix ; Update Bank number
        rts

; *******************************************************************
; selett
; *******************************************************************
selett:
        move.l  #skidoo,action
        move    d0,skid
        rts

; *******************************************************************
; skidoo
; *******************************************************************
skidoo:

        movea.l #fxedbase+8,a0
        move    skid,d0
        sub.w   #1,d0
        mulu.w  #$1800,d0
        lea     matrix,a1
        adda.l  d0,a1
        move    #5,d0

slett:
        clr.l   (a0)
        tst.l   info(a1)
        beq.w   sletto
        move.l  a1,(a0)
        lea     $300(a1),a2
        move    #7,d2
        lea     word_197D4A,a3

yuz:
        move    $E(a2),d3
        lsl.w   #2,d3
        cmp.w   #24,d3
        beq.w   yuz2
        move.l  (a3,d3.w),8(a2)

yuz2:
        lea     vfb_ysca(a2),a2
        dbf     d2,yuz

sletto:
        lea     1024(a1),a1
        lea     4(a0),a0
        dbf     d0,slett
        lea     pixcon,a1
        movea.l fx1,a0
        lea     $380(a0),a0
        move.l  #$6C,d0 ; 'l'
        jsr     dcopyblo
        bsr.w   zapdel
        clr.w   og
        move    #$FFFF,beasties+204
        rts

; ---------------------------------------------------------------------------
        rts

; *******************************************************************
; dcopyblo
; *******************************************************************
dcopyblo:
        move    delayp,-(sp) ; Stash some values in the stack so we can restore them later.
        move.l  dline,-(sp) ; Stash some values in the stack so we can restore them later.
        move    d2elayp,-(sp) ; Stash some values in the stack so we can restore them later.
        move.l  d2line,-(sp) ; Stash some values in the stack so we can restore them later.
        bsr.w   copybloc
        move.l  (sp)+,d2line
        move    (sp)+,d2elayp
        move.l  (sp)+,dline
        move    (sp)+,delayp
        rts

; *******************************************************************
; editvex
; Array of available actions during editing mode.
; *******************************************************************
editvex:
        dc.l selector, xy1, x_one
        dc.l x_one, xy1, xy1, kpad
        dc.l awf_x, awf_y, spdinc, adsred
        dc.l dladj, padex, speced, speced2
        dc.l speced3

; *******************************************************************
; speced3
; *******************************************************************
speced3:
        move    band,d0
        lsl.w   #3,d0
        lea     avbank,a0
        lea     (a0,d0.w),a0
        move.l  pad_now,d0
        move.l  d0,d1
        and.l   #$100000,d0
        beq.w   fobb
        addi.w  #$100,6(a0)
        bra.w   padex

; *******************************************************************
; fobb
; *******************************************************************
fobb:
        and.l   #$200000,d1
        beq.w   padex
        subi.w  #$100,6(a0)
        bra.w   padex

; *******************************************************************
; speced2
; *******************************************************************
speced2:
        move    frames,d0
        and.w   #7,d0
        bne.w   padex
        move    band,d0
        lsl.w   #3,d0
        lea     avbank,a0
        lea     (a0,d0.w),a0
        move.l  pad_now,d0
        move.l  d0,d1
        and.l   #$100000,d0
        beq.w   speced2a
        tst.w   (a0)
        beq.w   wied1
        subi.w  #1,(a0)

; *******************************************************************
; wied1
; *******************************************************************
wied1:
        cmpi.w  #$3F,2(a0) ; '?'
        bge.w   padex
        addi.w  #1,2(a0)
        bra.w   padex

; *******************************************************************
; speced2a
; *******************************************************************
speced2a:
        move.l  d1,d0
        and.l   #$200000,d0
        beq.w   speced2b
        move    2(a0),d0
        sub.w   (a0),d0
        cmp.w   #2,d0
        ble.w   padex
        addi.w  #1,(a0)
        subi.w  #1,2(a0)
        bra.w   padex

; *******************************************************************
; speced2b
; *******************************************************************
speced2b:
        move.l  d1,d0
        and.l   #$400000,d0
        beq.w   speced2c
        tst.w   (a0)
        beq.w   padex
        subi.w  #1,(a0)
        subi.w  #1,2(a0)
        bra.w   padex

; *******************************************************************
; speced2c
; *******************************************************************
speced2c:
        and.l   #$800000,d1
        beq.w   padex
        cmpi.w  #$3F,2(a0) ; '?'
        bge.w   padex
        addi.w  #1,(a0)
        addi.w  #1,2(a0)
        bra.w   padex

; *******************************************************************
; speced
; *******************************************************************
speced:
        move    selected,band
        bra.w   selector

; *******************************************************************
; iawfx
; *******************************************************************
iawfx:
        bsr.w   iwf
        move    #8,editing
        movea.l cwed1,a0
        tst.w   UPDA1F(a0)
        bne.w   istat
        move    cwave1,d1
        sub.w   #1,d1
        move.l  #$8000,d0
        bsr.w   wavelink

; *******************************************************************
; istat
; *******************************************************************
istat:
        lea     awfbutts,a1
        lea     awfb2,a4
        lea     pad8bits,a2
        move    #7,d0
        move    UPDA1F(a0),d1

; *******************************************************************
; issi
; *******************************************************************
issi:
        move.b  (a2)+,d2
        and.w   #$FF,d2
        lsl.w   #2,d2
        lea     2(a1,d2.w),a3
        lea     2(a4,d2.w),a5
        clr.b   (a3)
        clr.w   (a5)
        btst    #0,d1
        beq.w   issi2
        bset    #7,(a3)

; *******************************************************************
; issi2
; *******************************************************************
issi2:
        btst    #8,d1
        beq.w   issi3
        bset    #7,(a5)

; *******************************************************************
; issi3
; *******************************************************************
issi3:
        lsr.w   #1,d1
        dbf     d0,issi
        clr.l   d0
        move    $102(a0),d0
        lsl.l   #8,d0
        move.l  d0,ixcon
        bra.w   dud_butt

; *******************************************************************
; kpass
; *******************************************************************
kpass:
        movea.l #kpasshea,a0  ; "@~g1:SRCEN:Assign effect to keypad//Press t"...
        jsr     print
        move    #7,editing
        move.l  #(sympad+3+$6D),cpad
        move.l  #kpassbut,cpad+4
        bsr.w   ppmes
        bra.w   ud_butts
; ---------------------------------------------------------------------------
ass1:
        move    #1,fass
        move.l  #dofass,action
        rts
; ---------------------------------------------------------------------------
ass2:
        move    #2,fass
        move.l  #dofass,action
        rts
; ---------------------------------------------------------------------------
ass3:
        move    #3,fass
        move.l  #dofass,action
        rts
; ---------------------------------------------------------------------------
ass4:
        move    #4,fass
        move.l  #dofass,action
        rts
; ---------------------------------------------------------------------------
ass5:
        move    #5,fass
        move.l  #dofass,action
        rts
; ---------------------------------------------------------------------------
ass6:
        move    #6,fass
        move.l  #dofass,action
        rts
; ---------------------------------------------------------------------------
ass7:
        move    #7,fass
        move.l  #dofass,action
        rts
; ---------------------------------------------------------------------------
ass8:
        move    #8,fass
        move.l  #dofass,action
        rts
; ---------------------------------------------------------------------------
ass9:
        move    #9,fass
        move.l  #dofass,action
        rts

; *******************************************************************
; dofass
; *******************************************************************
dofass:
        movea.l fx,a5
        movea.l (a5),a1
        lea     $380(a1),a1
        lea     pixcon,a0
        move.l  #$6C,d0 ; 'l'
        jsr     copybloc
        move    fass,d0
        sub.w   #1,d0
        mulu.w  #$1800,d0
        lea     matrix,a4
        adda.l  d0,a4
        movea.l a4,a1
        movea.l (a5),a0
        move.l  #$1800,d0
        jsr     blitcopy
        bra.w   editquit

; *******************************************************************
; dfass
; *******************************************************************
dfass:
        move.l  (a5)+,d0
        bne.w   copyit
        move.l  #0,info(a4)
        bra.w   nxfass

; *******************************************************************
; copyit
; *******************************************************************
copyit:
        movea.l d0,a0
        movea.l a4,a1
        move.l  #1024,d0
        jsr     blitcopy

; *******************************************************************
; nxfass
; *******************************************************************
nxfass:
        lea     1024(a4),a4
        dbf     d5,dfass
        rts

; *******************************************************************
; iwf
; *******************************************************************
iwf:
        movea.l #awhead,a0  ; "Attach and Adjust Waveforms//Press keys"...
        clr.w   antelope
        jsr     cprint
        clr.l   ixcon
        clr.l   ixcon+4
        move    monitor,d0
        move    d0,cwave1
        sub.w   #1,d0
        move    d0,d1
        lsl.w   #2,d1
        movea.l fxobj,a6
        lea     (a6,d1.w),a5
        move.l  a5,cwed1
        bsr.w   varadd
        move.b  SRCEN(a0),d0
        and.w   #$FF,d0
        move    d0,cwave2
        lsl.w   #2,d0
        lea     (a6,d0.w),a5
        move.l  a5,cwed2
        move.l  #(sympad+3+$FD),cpad
        move.l  #awfbutts,cpad+4
        rts

; *******************************************************************
; iawfy
; *******************************************************************
iawfy:
        bsr.s   iwf
        move    #9,editing
        movea.l cwed2,a0
        tst.w   UPDA1F(a0)
        bne.w   istat
        move    cwave2,d1
        move.l  #$8000,d0
        bsr.w   wavelink
        bra.w   istat

; *******************************************************************
; awf_y
; *******************************************************************
awf_y:
        lea     pad_now,a1
        move.b  SRCEN(a1),d0
        rol.b   #4,d0
        movea.l cwed2,a4
        bra.w   wset

; *******************************************************************
; awf_x
; *******************************************************************
awf_x:
        lea     pad_now,a1
        move.b  SRCEN(a1),d0
        rol.b   #2,d0
        movea.l cwed1,a4

; *******************************************************************
; wset
; *******************************************************************
wset:
        lea     ixcon,a0
        jsr     inertcon
        move.l  ixcon,d1
        lsr.l   #8,d1
        move    d1,$102(a4)
        move.l  a4,udud
        lea     udud_but,a0
        bsr.w   gkp
        move.l  (a1),d0
        and.l   #$22002000,d0
        beq.w   rrts
        movea.l eddie,a2
        btst    #0,(a2)
        beq.w   soxx
        move.l  #rwfa,action
        bra.w   sdb

; *******************************************************************
; setantel
; *******************************************************************
setantel:
        move    #1,antelope
        rts
; ---------------------------------------------------------------------------
lul1:
        tst.w   antelope
        bne.w   ant1
        bchg    #0,$101(a4)
        rts
; ---------------------------------------------------------------------------
lul2:
        tst.w   antelope
        bne.w   ant2
        bchg    #1,$101(a4)
        rts
; ---------------------------------------------------------------------------
lul3:
        tst.w   antelope
        bne.w   ant3
        bchg    #2,$101(a4)
        rts
; ---------------------------------------------------------------------------
lul4:
        tst.w   antelope
        bne.w   ant4
        bchg    #3,$101(a4)
        rts
; ---------------------------------------------------------------------------
lul5:
        tst.w   antelope
        bne.w   ant5
        bchg    #4,$101(a4)
        rts
; ---------------------------------------------------------------------------
lul6:
        tst.w   antelope
        bne.w   ant6
        bchg    #5,$101(a4)
        rts
; ---------------------------------------------------------------------------
lul7:
        tst.w   antelope
        bne.w   ant7
        bchg    #6,$101(a4)
        rts
; ---------------------------------------------------------------------------
lul8:
        tst.w   antelope
        bne.w   ant8
        bchg    #7,$101(a4)
        rts

; *******************************************************************
; ant1
; *******************************************************************
ant1:
        bchg    #0,UPDA1F(a4)

; *******************************************************************
; clant
; *******************************************************************
clant:
        move    #2,antelope
        clr.w   ant_data
        rts
; ---------------------------------------------------------------------------

ant2:
        bchg    #1,UPDA1F(a4)
        bra.s   clant
; ---------------------------------------------------------------------------

ant3:
        bchg    #2,UPDA1F(a4)
        bra.s   clant
; ---------------------------------------------------------------------------

ant4:
        bchg    #3,UPDA1F(a4)
        bra.s   clant
; ---------------------------------------------------------------------------

ant5:
        bchg    #4,UPDA1F(a4)
        bra.s   clant
; ---------------------------------------------------------------------------

ant6:
        bchg    #5,UPDA1F(a4)
        bra.s   clant
; ---------------------------------------------------------------------------

ant7:
        bchg    #6,UPDA1F(a4)
        bra.s   clant
; ---------------------------------------------------------------------------

ant8:
        bchg    #7,UPDA1F(a4)
        bra.s   clant

; *******************************************************************
; x_one
; *******************************************************************
x_one:
        lea     pad_now,a1
        move.b  SRCEN(a1),d0
        rol.b   #2,d0
        lea     ixcon,a0
        jsr     inertcon
        bra.w   spn_butt

; *******************************************************************
; xy1
; *******************************************************************
xy1:
        lea     pad_now,a1
        move.b  SRCEN(a1),d0
        rol.b   #2,d0
        lea     ixcon,a0
        jsr     inertcon
        move.b  SRCEN(a1),d0
        rol.b   #4,d0
        lea     iycon,a0
        jsr     inertcon

; *******************************************************************
; spn_butt
; *******************************************************************
spn_butt:
        move.l  (a1),d0
        and.l   #$10000,d0
        beq.w   pn_butte
        move.l  #wfa,action
        clr.w   editing
        bra.w   sdb

; *******************************************************************
; pn_butte
; *******************************************************************
pn_butte:
        tst.l   action
        bne.w   rrts
        lea     pad_now,a1
        move.l  (a1),d0
        move.l  #$2000000,d1
        and.l   d0,d1
        bne.w   bbexit
        move.l  #$20000000,d1
        and.l   d0,d1
        bne.w   prevexit
        and.l   #$2000,d0
        beq.w   rrts
        move    selected,d0
        add.w   #1,d0
        cmp.w   selectab,d0
        ble.w   slecset
        clr.w   d0

slecset:
        move    d0,selected
        bra.w   sted

prevexit:
        move    selected,d0
        sub.w   #1,d0
        bpl.s   slecset
        move    selectab,d0
        bra.s   slecset
buttex:
        move.l  (a1),d0
        and.l   #$22002000,d0
        beq.w   rrts

bexit:
        move.l  #editquit,action
        clr.w   symed
        rts

bbexit:
        move.l  ledit,action
        clr.w   symed
        bra.w   sdb

xselup:
        bsr.w   selup
        move.l  #wtud,action
        rts

xseldn:
        bsr.w   seldn
        move.l  #wtud,action
        rts

; *******************************************************************
; dladj
; *******************************************************************
dladj:
        move.l  pad_now,d0
        move.l  d0,d1
        and.l   #$100000,d0
        bne.w   sninc
        move.l  d1,d0
        and.l   #$200000,d0
        bne.w   sndec
        move.l  d1,d0
        and.l   #$400000,d0
        bne.w   spdec
        move.l  d1,d0
        and.l   #$800000,d0
        bne.w   spinc
        move.l  d1,d0
        and.l   #$2000000,d0
        beq.w   padex
        move.l  pad_now,d0
        move.l  d0,d1
        and.l   #$100000,d0
        bsr.w   selup
        bra.w   gnek
        move.l  d1,d0
        and.l   #$200000,d0
        bsr.w   seldn

gnek:
        move    selected,d0
        cmp.w   #1,d0
        bne.w   gnek2
        clr.w   d0

gnek2:
        sub.w   #1,d0
        move    d0,delayf
        rts

spinc:
        addi.w  #1,delayt

sic:
        andi.w  #$3F,delayt ; '?'

spex:
        move    #1,seldb
        move.l  #udedg,action
        rts

spdec:
        subi.w  #1,delayt
        bra.s   sic

sninc:
        addi.w  #1,delayn

snc:
        andi.w  #$3F,delayn ; '?'
        bra.s   spex

sndec:
        subi.w  #1,delayn
        bra.s   snc

; *******************************************************************
; adsred
; *******************************************************************
adsred:
        move.l  pad_now,d0
        move.l  d0,d1
        and.l   #$100000,d0
        bne.w   selup
        move.l  d1,d0
        and.l   #$200000,d0
        bne.w   seldn
        movea.l aded,a0
        move    selected,d2
        lsl.w   #1,d2
        lea     (a0,d2.w),a0
        move.l  d1,d0
        and.l   #$400000,d0
        bne.w   intdec
        move.l  d1,d0
        and.l   #$800000,d0
        bne.w   intinc

; *******************************************************************
; padex
; *******************************************************************
padex:
        move.l  pad_now,d1
        and.l   #$22002000,d1
        bne.w   owwt
        rts

; *******************************************************************
; intdec
; *******************************************************************
intdec:
        subi.w  #$80,(a0)
        rts

; *******************************************************************
; intinc
; *******************************************************************
intinc:
        addi.w  #$80,(a0)
        rts

; *******************************************************************
; spdinc
; *******************************************************************
spdinc:
        move.l  pad_now,d0
        move.l  d0,d1
        and.l   #$100000,d0
        bne.w   xselup
        move.l  d1,d0
        and.l   #$200000,d0
        bne.w   xseldn
        move.l  d1,d0
        and.l   #$400000,d0
        bne.w   ph_dec
        move.l  d1,d0
        and.l   #$800000,d0
        bne.w   ph_inc
        lea     wfpad,a0
        lea     pad_now,a1
        move    (a1),d6
        move    #3,d7

_gk1:
        move.l  (a0)+,d0
        beq.w   _bodb
        btst    #0,d6
        beq.w   _bodb
        movea.l d0,a2
        jmp     (a2)

_bodb:
        lsr.w   #1,d6
        dbf     d7,_gk1
        move    2(a1),d6
        move    #7,d7

_gk2:
        move.l  (a0)+,d0
        beq.w   _bodb2
        movea.l d0,a2
        btst    #0,d6
        beq.w   _bodb2
        jmp     (a2)
; ---------------------------------------------------------------------------

_bodb2:
        lsr.w   #1,d6
        dbf     d7,_gk2
        move.l  pad_now,d1
        and.l   #$22002000,d1
        beq.w   rrts
        move    selected,d2
        movea.l edwave,a0
        lsl.w   #4,d2
        lea     4(a0,d2.w),a0
        cmp.l   #$2000000,d1
        bne.w   zinc

owwt:
        move    #1,editing
        clr.w   symed
        bra.w   onstak
; ---------------------------------------------------------------------------

zinc:
        cmp.l   #$2000,d1
        bne.w   stinc
        subi.l  #$1000,(a0)

stanc:
        andi.l  #$1FFFFF,(a0)
        rts
; ---------------------------------------------------------------------------

stinc:
        addi.l  #$1000,(a0)
        bra.s   stanc
; ---------------------------------------------------------------------------

; *******************************************************************
; swf1
; *******************************************************************
swf1:
        move    #0,d0

swfe:
        bsr.w   git
        move    d0,$E(a0)
        move.l  #wtud,action
        rts

; *******************************************************************
; swf2
; *******************************************************************
swf2:
        move    #1,d0
        bra.s   swfe

; ---------------------------------------------------------------------------
swf3:
        move    #2,d0
        bra.s   swfe
; ---------------------------------------------------------------------------

swf4:
        move    #3,d0
        bra.s   swfe
; ---------------------------------------------------------------------------

swf5:
        move    #4,d0
        bra.s   swfe
; ---------------------------------------------------------------------------

swf6:
        move    #5,d0
        bra.s   swfe
; ---------------------------------------------------------------------------

swf7:
        move    #7,d0
        bra.s   swfe
; ---------------------------------------------------------------------------

swf8:
        move    #8,d0
        bra.s   swfe
; ---------------------------------------------------------------------------

swf9:
        move    #6,d0
        bra.s   swfe
; ---------------------------------------------------------------------------

ph_inc:
        bsr.w   git
        addi.l  #$10000,(a0)
        rts
; ---------------------------------------------------------------------------

ph_dec:
        bsr.w   git
        subi.l  #$10000,(a0)
        rts

; *******************************************************************
; git
; *******************************************************************
git:
        move    selected,d2
        movea.l edwave,a0
        lsl.w   #4,d2
        lea     (a0,d2.w),a0
        rts

; *******************************************************************
; selector
; *******************************************************************
selector:
        move.l  pad_now,d0
        move.l  d0,d1
        and.l   #$100000,d0
        bne.w   selup
        move.l  d1,d0
        and.l   #$200000,d0
        bne.w   seldn
        and.l   #$22002000,d1
        beq.w   rrts
        and.l   #$2000,d1
        beq.w   seuss
        tst.w   cbuttf
        beq.w   onstak

soxx:
        move.l  #reedit,action
        bra.w   sdb

; *******************************************************************
; onstak
; *******************************************************************
onstak:
        movea.l esp,a0
        move.l  (a0)+,d0
        move.l  (a0),d0
        move.l  a0,esp
        tst.l   d0
        beq.w   bexit
        move.l  d0,action
        bra.w   sdb

; *******************************************************************
; seuss
; *******************************************************************
seuss:
        move    selected,d0

; *******************************************************************
; sted
; *******************************************************************
sted:
        lsl.w   #3,d0
        movea.l editlist,a0
        move.l  $0C(a0,d0.w),action
        clr.w   editing
        bra.w   sdb

; *******************************************************************
; selup
; *******************************************************************
selup:
        subi.w  #1,selected
        bpl.w   sud
        move    selectab,selected

sud:
        move.l  #ud_selcu,action

sdb:
        move    #1,seldb
        rts

; *******************************************************************
; seldn
; *******************************************************************
seldn:
        addi.w  #1,selected
        move    selectab,d0
        cmp.w   selected,d0
        bpl.s   sud
        clr.w   selected
        tst.w   sec_en
        beq.s   sud
        addi.w  #1,sec_cnt
        cmpi.w  #8,sec_cnt
        blt.s   sud
        clr.w   sec_cnt
        move.l  #old_edit,action
        bra.s   sdb

; *******************************************************************
; InitBeasties
; Initialize the Object List. We make it 12 items long. The main object
; is the first one, which is a full screen of pixel data generated by the
; GPU. Each entry is 64 bytes long.
;
; Data structure of items in 'beasties':
; 0-3      X
; 4-7      Y 
; 12-13    Object Mode
; 14-15    Object Type
; 16-18    Pointer to screen data.
; 20-22    Index to post-creation routine in 'postfixupsps'.
; *******************************************************************
InitBeasties:
        lea     beasties,a0    ; Point a0 at the first entry in beasties.
        move    #12,d7         ; Move 12 to d7.
        move    d7,nbeastie    ; There will be 12 entries in beasties.
ibeasts: 
        move    #$FFFF,12(a0) ; Initialize the mode of the entry to 'inactive'
        lea     64(a0),a0      ; Each entry is 64 bytes long.
        dbf     d7,ibeasts     ; Go to the next entry.
        rts

; *******************************************************************
; ccc
; *******************************************************************
ccc:
        clr.w   snoop
        rts

; *******************************************************************
; cc
; *******************************************************************
cc:
        addi.w  #1,snoop
ccat:
        tst.l   $804850
        bne.w   rrts

cow:
        bra.s   cow
        rts

; *******************************************************************
; RunBeasties
; Create the shadow hardware display list in blist using the backing list
; in the beasties list.
;
; Create the Object List (aka Display List). This is used by the
; Jaguar's Object Processor to write pixel data to the screen. It has
; a fixed length of 12 objects in the list (fixed by InitBeastiesties above).
; The main object is the first one (see ObTypes), which is a full screen
; of pixel data populated by the GPU.
;
; Data structure of items in 'beasties':
; 0-3      X
; 4-7      Y 
; 12-13    Object Mode
; 14-15    Object Type
; 16-18    Pointer to screen data.
; 20-22    Index to post-creation routine in 'postfixupsps'.
;
; Modes are:
;              0      1      2        3      4      5      6      7
; Routine      clip0, clip1, stoptop, clip2, clip1, clip1, clip1, clip1
; *******************************************************************
RunBeasties:
        movea.l blist,a0       ; Point a0 at blist.
        movea.l dlist,a4       ; Point a4 at dlist.
        lea     beasties,a2    ; Point a2 at beasties.
        move    nbeastie,d7    ; There are 12 entries in the beasties list.
        tst.w   scron          ; Do we already have an initial display list object?
        beq.w   RBeasts        ; If not, skip to intialising from the start.
        bmi.w   RBeasts        ; If not, skip to intialising from the start.
        ; We already have an entry at the start of the list.
        sub.w   #1,d7          ; Need to initialize one less.
        lea     64(a2),a2      ; Move the pointer to the second entry in the beasties list.
        
        ; Loop through beasties and create objects for each entry in
        ; blist. The 'mode' in each object will determine which ModeVex
        ; routine to call and that will create the object and add it to
        ; blist. Our pointer to blist in this operation is a0.
RBeasts:move    d7,-(sp)       ; Stash some values in the stack so we can restore them later.
        move    12(a2),d0      ; Get the object's 'Mode'.
        bmi.w   nxbeast        ; Skip if it doesn't have a mode set.
        lea     ModeVex,a3     ; Point a3 at the ModeVex list.
        asl.w   #2,d0          ; Multiply d0 by 4.
        movea.l (a3,d0.w),a3   ; Point a3 at the indexed entry in ModeVex.
        ; Our selected ModeVex routine will use either MakeScaledObject or
        ; MakeUnScaledObject to create an entry for the object in blist.
        jsr     (a3)           ; Run the routine for the mode selected from ModeVex (e.g. clip1).
        
nxbeast:
        move    (sp)+,d7       ; Restore the value of d7 stashed in the stack.
        lea     64(a2),a2      ; Move pointer to the next entry in beasties.
        dbf     d7,RBeasts     ; Do the next entry unless we've finished.

        ; Add the footer or 'stop' object to the Object List. 
        bra.w   StopList


; *******************************************************************
; postfixups
; *******************************************************************
postfixups:
        dc.l  make_rmw, make_transparent

; *******************************************************************
; make_rmw
; *******************************************************************
make_rmw:
        lea     -$20(a0),a3
        bset    #6,$A(a3)
        bset    #7,$A(a3)

setref:
        tst.w   $1E(a2)
        bne.w   setrf
        bclr    #0,9(a3)
        rts

setrf:
        bset    #0,9(a3)
        rts

; *******************************************************************
; make_transparent
; *******************************************************************
make_transparent:
        lea     -$20(a0),a3
        bset    #7,$A(a3)
        bra.s   setref

; *******************************************************************
; ModeVex
; *******************************************************************
ModeVex:
        dc.l clip0, clip1, stoptop, clip2
        dc.l clip1, clip1, clip1, clip1

; *******************************************************************
; moo
; *******************************************************************
moo:
        clr.w   frames
        jmp     makeit_transparent

; *******************************************************************
; stoptop
; *******************************************************************
stoptop:
        move    #$B4,d3
        sub.w   palside,d3
        move    #$1A4,d4
        add.w   paltop,d4
        sub.w   #$B0,d4
        tst.w   vlm_mode
        beq.w   clip1
        move    pixcon,d0
        sub.w   #$7F,d0
        add.w   d3,d0
        move    d0,(a2)
        move    piycon,d0
        sub.w   #$7F,d0
        lsl.w   #1,d0
        add.w   d4,d0
        move    d0,4(a2)
        bmi.w   rrts
        bra.w   clip1

; *******************************************************************
; clip0
; *******************************************************************
clip0:
        move    skale,d0
        beq.w   clip00
        add.w   #2,d0
        move    d0,$E(a2)
        move    #1,bo
        bra.w   clip2

clip00:
        clr.w   $E(a2)
        move    #1,bo
        bra.w   clipc

; *******************************************************************
; clip1
; *******************************************************************
clip1:
        clr.w   bo

clipc:
        move    (a2),d0
        move    4(a2),d1
        cmpi.w  #5,$E(a2)
        bne.w   fixpal
        sub.w   palside,d0
        add.w   paltop,d1

fixpal:
        and.w   #$FFF,d0
        clr.w   d6
        tst.w   d1
        bpl.w   ponscr
        move    d1,d6
        neg.w   d6
        lsr.w   #1,d6

ponscr:
        bclr    #0,d1
        swap    d6
        move    8(a2),d6
        move    $A(a2),d3
        lsl.w   #8,d3
        or.w    d6,d3
        swap    d3
        swap    d6
        move    $E(a2),d7
        lea     ObTypes,a3
        asl.w   #3,d7
        move    $18(a2),d3
        move    $1A(a2),d4
        move    $1C(a2),d5
        move    $16(a2),d2
        movea.l vfb_ysca(a2),a1
        tst.w   d6
        beq.w   nohang
        move    d3,d7
        mulu.w  d6,d7
        lsl.l   #3,d7
        move.l  d7,hango
        adda.l  d7,a1
        clr.w   d1

nohang:
        bsr.w   MakeUnScaledObject
        move    20(a2),d0 ; Get the postfixups index.
        bmi.w   rrts
        lea     postfixups,a3
        asl.w   #2,d0 ; Multiply d0 by 4.
        movea.l (a3,d0.w),a3
        jmp     (a3)

; *******************************************************************
; clip2
; *******************************************************************
clip2:
        move    (a2),d0
        and.w   #$FFF,d0
        clr.w   d6
        move    4(a2),d1
        bpl.w   sponscr
        move    d1,d6
        neg.w   d6
        move    skale,d3
        add.w   #1,d3
        lsr.w   d3,d6

sponscr:
        bclr    #0,d1
        swap    d6
        move    8(a2),d6
        move    $A(a2),d3
        lsl.w   #8,d3
        or.w    d6,d3
        swap    d3
        swap    d6
        move    $E(a2),d7
        lea     ObTypes,a3
        asl.w   #3,d7
        move    (a3,d7.w),d3
        move    2(a3,d7.w),d4
        move    4(a3,d7.w),d5
        move    6(a3,d7.w),d2
        movea.l vfb_ysca(a2),a1
        tst.w   d6
        beq.w   snohang
        move    d3,d7
        mulu.w  d6,d7
        lsl.l   #3,d7
        move.l  d7,hango
        adda.l  d7,a1
        clr.w   d1

snohang:
        bsr.w   MakeScaledObject
        move    20(a2),d0 ; Get the postfixups index.
        bmi.w   rrts
        lea     postfixups,a3
        asl.w   #2,d0 ; Multiply d0 by 4.
        movea.l (a3,d0.w),a3
        jmp     (a3)

; *******************************************************************
; InitList
; Set up our dlist and blist object list pointers. Each points to a separate 624 byte
; region of RAM (list1 and list2 respectively) that will contain a list of objects for the Object Processor
; to paint to the screen.
;
; We point ddlist at dlist. After this routine is called, 'startup' will store ddlist
; in the OLP (Object List Pointer), which is what the Object Processor
; will use as the address of the object list to use for all objects to be
; painted to the screen. So the Object Processor will always process whatever
; object list dlist is pointing to.
;
; We also set up blist, which is where all our objects will actually get
; updated. When ready to be drawn we will copy blist into dlist so that it
; can be processed by the Object Processor. It's InitBeasties and RunBeasties
; that create all the items and adds them to blist, while 'everything' fills
; them out.
; *******************************************************************
InitList:
        ; Make ddlist a pointer to list1.
        move.l  #list1,d0
        and.l   #$FFFFFFE0,d0

        movea.l d0,a0
        move.l  #0,(a0)+
        move.l  #4,(a0)+
        lea     8(a0),a0
        move.l  a0,ddlist

        lsl.l   #5,d0
        move    n_vdb,d1
        move    n_vde,d2
        lsl.w   #3,d1
        lsl.w   #3,d2
        or.w    #3,d1
        or.w    #3,d2
        bset    #$E,d1
        bset    #$F,d2

        swap    d0
        move    #0,(a0)+
        move    d0,(a0)+
        swap    d0
        move    d0,(a0)+
        move    d1,(a0)+

        swap    d0
        move    #0,(a0)+
        move    d0,(a0)+
        swap    d0
        move    d0,(a0)+
        move    d2,(a0)+

        ; Make dlist a pointer to list1. This will be where the contents
        ; of blist get copied to when they are ready to be painted.
        move.l  a0,dlist
        bsr.w   StopList

        ; Make blist a pointer to list2. This will be where the objects
        ; are created and updated. When ready they will be copied to dlist.
        move.l  #list2,d0
        and.l   #$FFFFFFE0,d0
        move.l  d0,blist
        movea.l d0,a0
        bra.w   StopList

; *******************************************************************
; StopList
; Create a Stop Object at the end of the Object List.
; *******************************************************************
StopList:
        move    #15,d0
sl:     move.l  #0,(a0)+
        move.l  #4,(a0)+ ; A 4 indicates that this is a stop objfect.
        dbf     d0,sl ; Keep looping until we've done 16 bytes.
        rts

; *******************************************************************
; MakeScaledObject
; *******************************************************************
MakeScaledObject:
        tst.w   bo
        beq.w   nmulto2
        movem.l d0-d5/a1,-(sp) ; Stash some values in the stack so we can restore them later.
        bsr.w   nmulto2
        move    #$180,d0
        move    #0,d1
        move    #2,d2
        move    #1,d3
        move    #$180,d4
        move    #3,d5
        movea.l #LaunchVLM,a1
        clr.w   bo
        bsr.w   nmulto
        move    #1,bo
        lea     -$20(a0),a3
        bset    #0,9(a3)
        movem.l (sp)+,d0-d5/a1
        move    d3,d6
        move    skale,d7
        add.w   #1,d7
        lsr.w   d7,d6
        lsl.w   #2,d6
        move    d6,d7
        lsl.w   #1,d7
        and.l   #$FFFF,d7
        move.l  d7,dbadj
        adda.l  d7,a1
        move    skale,d7
        lsl.w   d7,d6
        add.w   d6,d0
        bsr.w   nmulto2
        clr.w   bo
        rts

; *******************************************************************
; nmulto2
; *******************************************************************
nmulto2:
        move    d0,-(sp) ; Stash some values in the stack so we can restore them later.
        move.l  a0,d6
        and.w   #$1F,d6
        beq.w   muso
        move.l  a0,d6
        and.l   #$FFFFE0,d6
        movea.l d6,a0
        lea     $20(a0),a0

muso:
        move.l  a1,d6
        and.l   #$FFFFFFF8,d6
        lsl.l   #8,d6
        move.l  d6,(a0)+
        lea     $20(a4),a6
        move.l  a6,d6
        lsl.l   #5,d6
        swap    d6
        or.w    d6,-2(a0)
        moveq   #0,d7
        move    d4,d7
        ror.l   #2,d7
        swap    d6
        and.w   #$FF00,d6
        or.w    d7,d6
        move    d6,(a0)+
        swap    d7
        move    d1,d6
        lsl.w   #3,d6
        or.w    d7,d6
        bset    #0,d6
        move    d6,(a0)+
        clr.w   (a0)+
        move    d2,d7
        lsl.w   #7,d7
        moveq   #0,d6
        move    d3,d6
        move    skale,d0
        add.w   bo,d0
        lsr.w   d0,d6
        ror.l   #4,d6
        bclr    #$F,d6
        or.w    d7,d6
        move    d6,(a0)+
        move    (sp)+,d0
        and.w   #$FFF,d0
        move    d3,d7
        lsl.w   #2,d7
        swap    d6
        or.w    d7,d6
        move    d6,(a0)+
        move    d5,d6
        ror.w   #4,d6
        or.w    d0,d6
        bset    #$F,d6
        move    d6,(a0)+
        clr.l   (a0)+
        move.l  #$14040,d0
        cmpi.w  #2,skale
        bne.w   khluj
        move.l  #$18080,d0

khluj:
        move.l  d0,(a0)+
        lea     8(a0),a0
        lea     $20(a4),a4
        rts

; *******************************************************************
; MakeUnScaledObject
; *******************************************************************
MakeUnScaledObject:
        tst.w   bo
        beq.w   nmulto
        movem.l d0-d5/a1,-(sp) ; Stash some values in the stack so we can restore them later.
        bsr.w   nmulto
        move    #$180,d0
        move    #0,d1
        move    #2,d2
        move    #1,d3
        move    #$180,d4
        move    #3,d5
        movea.l #LaunchVLM,a1
        clr.w   bo
        bsr.w   nmulto
        move    #1,bo
        lea     -$20(a0),a3
        bset    #0,9(a3)
        movem.l (sp)+,d0-d5/a1
        move    d3,d6
        move    skale,d7
        add.w   #1,d7
        lsr.w   d7,d6
        lsl.w   #2,d6
        move    d6,d7
        lsl.w   #1,d7
        and.l   #$FFFF,d7
        move.l  d7,dbadj
        adda.l  d7,a1
        move    skale,d7
        lsl.w   d7,d6
        add.w   d6,d0
        bsr.w   nmulto
        clr.w   bo
        rts

; *******************************************************************
; nmulto
; *******************************************************************
nmulto:
        move.l  a0,d6
        and.w   #$1F,d6
        beq.w   mumuso
        move.l  a0,d6
        and.l   #$FFFFE0,d6
        movea.l d6,a0
        lea     $20(a0),a0

mumuso:
        move.l  a1,d6
        and.l   #$FFFFFFF8,d6
        lsl.l   #8,d6
        move.l  d6,(a0)+
        lea     $20(a4),a6
        move.l  a6,d6
        lsl.l   #5,d6
        swap    d6
        or.w    d6,-2(a0)
        moveq   #0,d7
        move    d4,d7
        ror.l   #2,d7
        swap    d6
        and.w   #$FF00,d6
        or.w    d7,d6
        move    d6,(a0)+
        swap    d7
        move    d1,d6
        lsl.w   #3,d6
        or.w    d7,d6
        move    d6,(a0)+
        clr.w   (a0)+
        move    d2,d7
        lsl.w   #7,d7
        moveq   #0,d6
        move    d3,d6
        tst.w   bo
        beq.w   snoke
        move    d0,-(sp) ; Stash some values in the stack so we can restore them later.
        move    skale,d0
        add.w   bo,d0
        lsr.w   d0,d6
        move    (sp)+,d0

snoke:
        and.w   #$FFF,d0
        ror.l   #4,d6
        bclr    #$F,d6
        or.w    d7,d6
        move    d6,(a0)+
        move    d3,d7
        lsl.w   #2,d7
        swap    d6
        or.w    d7,d6
        move    d6,(a0)+
        move    d5,d6
        ror.w   #4,d6
        or.w    d0,d6
        bset    #$F,d6
        move    d6,(a0)+
        lea     vfb_ysca(a0),a0
        lea     $20(a4),a4
        rts

; *******************************************************************
; udud_but
; *******************************************************************
udud_but:
        movea.l udud,a0
        bra.w   istat

; *******************************************************************
; dud_butt
; *******************************************************************
dud_butt:
        move.l  cpad+4,-(sp) ; Stash some values in the stack so we can restore them later.
        move.l  #awfb2,cpad+4
        bsr.w   ud_butts
        move.l  (sp)+,cpad+4

; *******************************************************************
; ud_butts
; *******************************************************************
ud_butts:
        move    inverse,-(sp) ; Stash some values in the stack so we can restore them later.
        movea.l cpad+4,a3
        move    #$B,d3
        lea     padchars,a4
        lea     board,a0

ud_b:
        move.b  (a4)+,d2
        tst.b   (a3)
        bmi.w   nud
        clr.w   inverse
        btst    #7,2(a3)
        beq.w   ud_i
        move    #1,inverse

ud_i:
        move.b  (a3),d0
        move.b  SRCEN(a3),d1
        and.w   #$FF,d0
        and.w   #$FF,d1
        bsr.w   charblit

nud:
        lea     4(a3),a3
        dbf     d3,ud_b
        move    (sp)+,inverse
        rts

; *******************************************************************
; charblit
; *******************************************************************
charblit:
        movem.w d0-d2,-(sp) ; Stash some values in the stack so we can restore them later.
        and.w   #$FF,d2
        move.l  #$11800,A2_FLAGS
        lsl.w   #1,d2
        lea     myFont,a1
        move    word_198CB4-myFont(a1,d2.w),d2
        lea     (a1,d2.w),a2
        move.l  a2,A2_BASE
        move.l  #0,A2_PIXEL
        move.l  #$1FFF8,A2_STEP
        move.l  #$14200,A1_FLAGS
        move.l  #board,A1_BASE
        lsl.w   #3,d0
        move    d1,d2
        lsl.w   #3,d1
        add.w   d2,d1
        swap    d1
        move    d0,d1
        move.l  d1,A1_PIXEL
        move.l  #$1FFF8,A1_STEP
        move.l  #$80008,B_COUNT
        move.l  #0,B_PATD
        move.l  #0,$F0226C
        move.l  #$1800609,d7
        tst.w   inverse
        beq.w   notinv
        move.l  #$600609,d7

notinv:
        move.l  d7,B_CMD
        bsr.w   WaitBlit
        movem.w (sp)+,d0-d2
        rts

; *******************************************************************
; cleol
; *******************************************************************
cleol:
        clr.w   d0
        clr.w   d1
        move    #$140,d2
        move    #$F0,d3
        moveq   #0,d4
        move.l  #$14200,d7
        move.l  #$10208,-(sp) ; Stash some values in the stack so we can restore them later.
        move    #1,nphrase
        bra.w   bblo

; *******************************************************************
; sblitblockk
; *******************************************************************
sblitblockk:
        movem.w d0-d3,-(sp) ; Stash some values in the stack so we can restore them later.
        move    skale,d7
        lsr.w   d7,d0
        lsr.w   d7,d1
        lsr.w   d7,d2
        bne.w   slogg
        move    #1,d2

slogg:
        lsr.w   d7,d3
        bne.w   slogg2
        move    #1,d3

slogg2:
        bsr.w   blitblock
        movem.w (sp)+,d0-d3
        rts

; *******************************************************************
; blitblock
; *******************************************************************
blitblock:
        clr.w   nphrase
        move.l  #$4420,d7
        move.l  #$10200,-(sp) ; Stash some values in the stack so we can restore them later.

bblo:
        move.l  d7,A1_FLAGS
        move.l  a0,d7
        move.l  d7,A1_BASE
        move    d1,d7
        swap    d7
        move    d0,d7
        move.l  d7,A1_PIXEL
        move.l  #0,A1_FPIXE
        moveq   #1,d7
        move.l  d7,A1_INC
        move.l  #0,A1_FINC
        move    #1,d7
        swap    d7
        move    d2,d7
        tst.w   nphrase
        bne.w   nnphr
        move.l  #$14420,A1_FLAGS

nnphr:
        neg.w   d7
        move.l  d7,A1_STEP
        move.l  #0,A1_FSTEP
        move    d3,d7
        swap    d7
        move    d2,d7
        move.l  d7,B_COUNT
        move    d4,d7
        swap    d7
        move    d4,d7
        move.l  d7,B_PATD
        move.l  d7,$F0226C
        move.l  (sp)+,d7
        move.l  d7,B_CMD

; *******************************************************************
; WaitBlit
; *******************************************************************
WaitBlit:
        move.l  B_CMD,d7
        btst    #0,d7
        beq.s   WaitBlit

rrts:   rts
; ---------------------------------------------------------------------------
        dc.b $2E ; .
        dc.b $3C ; <
        dc.b   0
        dc.b   3
; ---------------------------------------------------------------------------
ecopy:
        neg.b   -(a0)
        bra.w   eec

; *******************************************************************
; CopyBlock
; *******************************************************************
CopyBlock:
        move.l  #$34220,d7

eec:
        move.l  d7,A1_FLAGS
        move.l  #$54420,d7
        move.l  d7,A2_FLAGS
        move    d3,d7
        swap    d7
        move    d2,d7
        move.l  d7,B_COUNT
        move    d1,d7
        swap    d7
        move    d0,d7
        move.l  d7,A1_PIXEL
        move    d5,d7
        swap    d7
        move    d4,d7
        move.l  d7,A2_PIXEL
        move.l  #0,A1_FPIXE
        move.l  #1,A1_INC
        move.l  #0,A1_FINC
        move    #1,d7
        swap    d7
        move    d2,d7
        neg.w   d7
        move.l  d7,A1_STEP
        move.l  d7,A2_STEP
        move.l  a0,d7
        move.l  d7,A1_BASE
        move.l  a1,d7
        move.l  d7,A2_BASE
        move.l  #$1800E01,d7
        move.l  d7,B_CMD
        bra.w   WaitBlit

; *******************************************************************
; MergeBlock
; *******************************************************************
MergeBlock:
        move.l  #0,B_PATD
        move.l  #0,$F0226C
        move.l  #$34420,d7
        move.l  d7,A1_FLAGS
        move.l  #$54420,d7
        move.l  d7,A2_FLAGS
        move    d3,d7
        swap    d7
        move    d2,d7
        move.l  d7,B_COUNT
        move    d1,d7
        swap    d7
        move    d0,d7
        move.l  d7,A1_PIXEL
        move    d5,d7
        swap    d7
        move    d4,d7
        move.l  d7,A2_PIXEL
        move.l  #0,A1_FPIXE
        move.l  #1,A1_INC
        move.l  #0,A1_FINC
        move    #1,d7
        swap    d7
        move    d2,d7
        neg.w   d7
        move.l  d7,A1_STEP
        move.l  d7,A2_STEP
        move.l  a0,d7
        move.l  d7,A1_BASE
        move.l  a1,d7
        move.l  d7,A2_BASE
        move.l  #$9800E01,d7
        move.l  d7,B_CMD
        bra.w   WaitBlit

; *******************************************************************
; dcode
; *******************************************************************
dcode:
        lea     codez,a3
        move    #$1F,d1

dco:
        move.b  (a3)+,d2
        btst    d1,d0
        bne.w   rrts
        dbf     d1,dco
        clr.w   d2
        rts

; *******************************************************************
; codez
; *******************************************************************
codez:  dc.b  $78, $78, $41, $50, $78, $78, $42, $78
        dc.b  $52, $4C, $44, $55, $31, $34, $37, $2A
        dc.b  $78, $78, $43, $78, $78, $78, $4F, $78
        dc.b  $32, $35, $38, $30, $33, $36, $39, $23

; *******************************************************************
; readpad
; "Jeff's joystick routine."
; Populates pad_now with input from the controller.
; *******************************************************************
readpad:
        movem.l d0-d2,-(sp) ; Stash some values in the stack so we can restore them later.
        move.l  #$F0FFFFFC,d1
        moveq   #$FFFFFFFF,d2
        move    #$81FE,JOYSTICK
        move.l  JOYSTICK,d0
        or.l    d1,d0
        ror.l   #4,d0
        and.l   d0,d2
        move    #$81FD,JOYSTICK
        move.l  JOYSTICK,d0
        or.l    d1,d0
        ror.l   #8,d0
        and.l   d0,d2
        move    #$81FB,JOYSTICK
        move.l  JOYSTICK,d0
        or.l    d1,d0
        rol.l   #6,d0
        rol.l   #6,d0
        and.l   d0,d2
        move    #$81F7,JOYSTICK
        move.l  JOYSTICK,d0
        or.l    d1,d0
        rol.l   #8,d0
        and.l   d0,d2
        moveq   #$FFFFFFFF,d1
        eor.l   d2,d1
        move.l  pad_now,d0
        move.l  d1,pad_now
        eor.l   d1,d0
        and.l   d1,d0
        move.l  d0,pad_shot
        move.l  #$FFFFFF3,d1
        moveq   #$FFFFFFFF,d2
        move    #$817F,JOYSTICK
        move.l  JOYSTICK,d0
        or.l    d1,d0
        rol.b   #2,d0
        ror.l   #8,d0
        and.l   d0,d2
        move    #$81BF,JOYSTICK
        move.l  JOYSTICK,d0
        or.l    d1,d0
        rol.b   #2,d0
        ror.l   #8,d0
        ror.l   #4,d0
        and.l   d0,d2
        move    #$81DF,JOYSTICK
        move.l  JOYSTICK,d0
        or.l    d1,d0
        rol.b   #2,d0
        rol.l   #8,d0
        and.l   d0,d2
        move    #$81EF,JOYSTICK
        move.l  JOYSTICK,d0
        or.l    d1,d0
        rol.b   #2,d0
        rol.l   #4,d0
        and.l   d0,d2
        moveq   #$FFFFFFFF,d1
        eor.l   d2,d1
        move.l  pad_now+4,d0
        move.l  d1,pad_now+4
        eor.l   d1,d0
        and.l   d1,d0
        move.l  d0,pad_shot+4
        movem.l (sp)+,d0-d2
        rts

; *******************************************************************
; initobject
; Unused.
; *******************************************************************
initobject:
        move.l  #$FFFFFFFF,activeob
        lea     objects,a0
        move    #$3F,d0 ; '?'
        move    d0,ofree
        move.l  a0,freeobje
        movea.l #$FFFFFFFF,a1

IniA:
        move.l  a1,(a0)
        movea.l a0,a1
        lea     64(a0),a0
        move.l  a0,4(a1)
        dbf     d0,IniA
        move.l  #$FFFFFFFF,4(a1)
        rts

; *******************************************************************
; insertobject
; Unused.
; *******************************************************************
insertobject:
        movea.l 0(a0),a1
        movea.l 4(a0),a2
        subi.w  #1,ofree
        lea     activeob,a3
        lea     freeobje,a4
        bra.w   mlink

; *******************************************************************
; unlinkobject
; Unused.
; *******************************************************************
unlinkobject:
        movea.l 0(a0),a1
        movea.l 4(a0),a2
        addi.w  #1,ofree
        lea     freeobje,a3
        lea     activeob,a4

; *******************************************************************
; mlink
; *******************************************************************
mlink:
        cmpa.l  #$FFFFFFFF,a1
        bne.w   ML1
        move.l  a2,(a4)
        cmpa.l  a1,a2
        beq.w   NewLink

ML1:
        cmpa.l  #$FFFFFFFF,a2
        bne.w   ML2
        move.l  a2,4(a1)
        bra.w   NewLink
; ---------------------------------------------------------------------------

ML2:
        cmpa.l  #$FFFFFFFF,a1
        beq.w   ml3
        move.l  a2,4(a1)

ml3:
        move.l  a1,(a2)

; *******************************************************************
; NewLink
; *******************************************************************
NewLink:
        movea.l (a3),a4
        cmpa.l  #$FFFFFFFF,a4
        bne.w   NL1
        move.l  a0,(a3)
        move.l  #$FFFFFFFF,(a0)
        move.l  #$FFFFFFFF,4(a0)
        clr.w   d0
        rts

NL1:
        move.l  #$FFFFFFFF,(a0)
        move.l  a0,(a4)
        move.l  a4,4(a0)
        move.l  a0,(a3)
        clr.w   d0
        rts

; *******************************************************************
; pinertco
; *******************************************************************
pinertco:
        move    d0,d1
        move    #1,d3
        and.w   #3,d1
        beq.w   friction
        bra.w   uuu

; *******************************************************************
; inertcon
; *******************************************************************
inertcon:
        move    #1,d3
        move    d0,d1
        and.w   #3,d1
        beq.w   iinstop

uuu:
        btst    #0,d1
        beq.w   ininc
        move.l  (a0),d1
        move.l  vfb_angl(a0),d2
        cmp.l   vfb_ysca(a0),d2
        beq.w   nolim0
        cmp.l   d2,d1
        bmi.w   instop
        bra.w   nolim1

nolim0:
        clr.w   d3

nolim1:
        move.l  4(a0),d0
        move.l  $18(a0),d2
        neg.l   d2
        cmp.l   d2,d0
        bmi.w   inmove
        move.l  8(a0),d0
        sub.l   d0,4(a0)

inmove:
        move.l  4(a0),d0
        add.l   d0,(a0)
        tst.w   d3
        beq.w   rrts
        move.l  (a0),d0
        move.l  vfb_angl(a0),d2
        cmp.l   d2,d0
        bmi.w   instop
        move.l  vfb_ysca(a0),d2
        cmp.l   d2,d0
        bpl.w   instop
        rts

ininc:
        move.l  (a0),d1
        move.l  vfb_ysca(a0),d2
        cmp.l   vfb_angl(a0),d2
        beq.w   nolim3
        cmp.l   d2,d1
        bpl.w   instop
        bra.w   nolim2

nolim3:
        clr.w   d3

nolim2:
        move.l  4(a0),d0
        move.l  $18(a0),d2
        cmp.l   d2,d0
        bpl.s   inmove
        move.l  8(a0),d0
        add.l   d0,4(a0)
        bra.s   inmove

instop:
        move.l  d2,(a0)

iinstop:
        clr.l   4(a0)
        rts

; *******************************************************************
; friction
; *******************************************************************
friction:
        move.l  vfb_ysca(a0),d0
        cmp.l   vfb_angl(a0),d0
        bne.w   derange
        clr.w   d3

derange:
        move.l  4(a0),d0
        move.l  vfb_xsca(a0),d1
        move.l  d1,d4
        move.l  d0,d2
        bpl.w   sposk
        neg.l   d2
        neg.l   d4

sposk:
        cmp.l   d1,d2
        bmi.s   iinstop
        sub.l   d4,4(a0)
        bra.w   inmove

score2nu:
        lea     8(a0),a0
        clr.l   d0
        clr.l   d4
        move    #1,d1
        move    #3,d7

s2n:
        move.b  -(a0),d2
        and.w   #$F,d2
        mulu.w  d1,d2
        add.l   d2,d0
        mulu.w  #$A,d1
        dbf     d7,s2n
        move    #1,d1
        move    #3,d7

s3n:
        move.b  -(a0),d2
        and.w   #$F,d2
        mulu.w  d1,d2
        add.l   d2,d4
        mulu.w  #$A,d1
        dbf     d7,s3n
        mulu.w  #$2710,d4
        add.l   d4,d0
        rts

; *******************************************************************
; xxnum
; *******************************************************************
xxnum:
        move.l  d0,d2
        divu.w  #$2710,d2
        and.l   #$FFFF,d2
        move    d2,d3
        mulu.w  #$2710,d3
        sub.l   d3,d0
        move    #3,d3

xscr:
        divu.w  #$A,d0
        swap    d0
        add.b   #$30,d0 ; '0'
        move.b  d0,-(a4)
        clr.w   d0
        swap    d0
        dbf     d3,xscr

xscr2:
        divu.w  #$A,d2
        swap    d2
        add.b   #$30,d2 ; '0'
        move.b  d2,-(a4)
        clr.w   d2
        swap    d2
        tst.w   d2
        bne.s   xscr2
        rts

; *******************************************************************
; deltablock
; *******************************************************************
deltablock:
        move    #7,d0
        move.l  a2,d4
        lea     $20(a2),a3

gennett:
        move    #$1F,d1
        clr.l   d3

gnet:
        move.b  (a1)+,d2
        bclr    d1,d3
        cmp.b   (a0)+,d2
        beq.w   gnet2
        bset    d1,d3
        move.b  d2,(a3)+

gnet2:
        dbf     d1,gnet
        move    #3,d2

wbm:
        rol.l   #8,d3
        move.b  d3,(a2)+
        dbf     d2,wbm
        dbf     d0,gennett
        movea.l a3,a2
        move.l  a3,d0
        sub.l   d4,d0
        rts

; *******************************************************************
; deltablo
; *******************************************************************
deltablo:
        move    d5,-(sp) ; Stash some values in the stack so we can restore them later.
        move    #7,d0
        move.l  a2,d4
        clr.w   d5
        move    #$17FF,d1

dblo2:
        move.b  (a1)+,d2
        cmp.b   #$FF,d5
        beq.w   fwrite2
        cmp.b   #0,d1
        beq.w   fwrite2
        cmp.b   (a0),d2
        beq.w   dblo3

fwrite2:
        move.b  d5,d3
        lsl.w   #8,d3
        move.b  d2,d3
        move    d3,(a2)+
        move.b  #$FF,d5

dblo3:
        lea     SRCEN(a0),a0
        add.b   #1,d5
        dbf     d1,dblo2
        move.l  a2,d0
        sub.l   d4,d0
        move    (sp)+,d5
        rts

; *******************************************************************
; getmatrix
; *******************************************************************
getmatrix:

        move    #1,skid

; *******************************************************************
; gm
; *******************************************************************
gm:

        lea     matrix,a1
        move    #$35FF,d0

ivtb:
        move.l  #$FFFFFFFF,(a1)+
        dbf     d0,ivtb
        movea.l sympad+3+$361,a0
        move.l  a0,d1
        lea     vfb_ysca(a0),a0
        move    imatrix,d0 ; Get the bank number
        lsl.w   #2,d0
        move.l  (a0,d0.w),d0
        sub.l   #$900000,d0
        add.l   d1,d0
        movea.l d0,a0
        lea     matrix,a1
        movem.l a0-a1,-(sp) ; Stash some values in the stack so we can restore them later.
        lea     refblock,a0
        move.l  #$1800,d0
        bsr.w   blitcopy
        movem.l (sp)+,a0-a1
        clr.w   d0
        clr.w   d1

unp:
        move.b  (a0)+,d1
        move.b  (a0)+,d2
        cmp.w   #$1800,d0
        bpl.w   taiga
        add.w   d1,d0
        cmp.w   #$1800,d0
        bpl.w   taiga
        move.b  d2,(a1,d0.w)
        add.w   #1,d0
        bra.s   unp

taiga:
        move    #7,d5

rrest:
        move.l  a0,-(sp) ; Stash some values in the stack so we can restore them later.
        movea.l a1,a0
        lea     WID8(a1),a1
        move.l  #$1800,d0
        bsr.w   blitcopy
        movea.l (sp)+,a0
        lea     -2(a0),a0
        clr.w   d0
        clr.w   d1

unpk2:
        move.b  (a0)+,d1
        move.b  (a0)+,d2
        cmp.w   #$1800,d0
        bpl.w   taiga2
        add.w   d1,d0
        cmp.w   #$1800,d0
        bpl.w   taiga2
        move.b  d2,(a1,d0.w)
        add.w   #1,d0
        bra.s   unpk2
; ---------------------------------------------------------------------------

taiga2:
        dbf     d5,rrest
        bsr.w   skidoo
        clr.w   bank_mod
        lea     banclr,a0 ; "~g1:$20:     "
        bra.w   print

; *******************************************************************
; prin
; *******************************************************************
prin:
        movem.l d0-a6,-(sp) ; Stash some values in the stack so we can restore them later.
        movea.l d7,a0
        jsr     print
        movem.l (sp)+,d0-a6
        rts

; *******************************************************************
; printban
; *******************************************************************
printban:
        lea     banset,a0 ; "~g1:$20:Bank>"
        bra.w   print

; *******************************************************************
; blitcopy
; *******************************************************************
blitcopy:
        move.l  #$4020,d1
        move.l  d1,A1_FLAGS
        move.l  d1,A2_FLAGS
        move.l  a1,d2
        move.l  d2,d1
        and.l   #$FFFFFFF8,d2
        sub.l   d2,d1
        move.l  d2,A1_BASE
        asr.l   #1,d1
        move.l  d1,A1_PIXEL
        asr.l   #1,d0
        or.l    #$10000,d0
        move.l  d0,B_COUNT
        move.l  a0,d0
        move.l  d0,d2
        and.l   #$FFFFFFF8,d0
        sub.l   d0,d2
        move.l  d0,A2_BASE
        asr.l   #1,d2
        move.l  d2,A2_PIXEL
        or.l    d1,d2
        beq.s   .aligned
        move.l  #$1800005,d0
        bra.s   .blit_go

.aligned:
        move.l  #$1800001,d0

.blit_go:
        move.l  d0,B_CMD
        bra.w   WaitBlit

; *******************************************************************
; monovert
; *******************************************************************
monovert:
        movea.l a0,a1
        lea     2(a0),a0
        move    #$FFFF,d7
        move    #1,d0

icu:
        move    #1,d1

icu2:
        move    #1,d2

icu3:
        move    d2,d3
        lsl.w   #2,d3
        sub.w   #2,d3
        move    d3,(a0)+
        move    d1,d3
        lsl.w   #2,d3
        sub.w   #2,d3
        move    d3,(a0)+
        move    d0,d3
        lsl.w   #2,d3
        sub.w   #2,d3
        move    d3,(a0)+
        add.w   #1,d7
        dbf     d2,icu3
        dbf     d1,icu2
        dbf     d0,icu
        move    d7,(a1)
        lea     spherobj,a0
        move    #$2F,(a0)+ ; '/'
        lea     sines,a1 ; ;sines.bin
        move    #$F,d0
        clr.w   d1
; ---------------------------------------------------------------------------
gsphr:          dc.b $14
        dc.b $31 ; 1
        dc.b $10
        dc.b   0
; ---------------------------------------------------------------------------
        add.b   #$40,d1 ; '@'
        move.b  (a1,d1.w),d3
        sub.b   #$30,d1 ; '0'
        ext.w   d2
        ext.w   d3
        move    d2,(a0)+
        move    d3,(a0)+
        clr.w   (a0)+
        move    d2,(a0)+
        clr.w   (a0)+
        move    d3,(a0)+
        clr.w   (a0)+
        move    d2,(a0)+
        move    d3,(a0)+
        dbf     d0,gsphr
        rts

; *******************************************************************
; makeclear
; *******************************************************************
makeclear:
        move.l  #$FFFFFFFF,(a0)+
        move    #$1FF,d0

macle:
        clr.l   (a0)+
        clr.l   (a0)+
        clr.l   (a0)+
        clr.l   (a0)+
        clr.l   (a0)+
        clr.l   (a0)+
        clr.l   (a0)+
        clr.l   (a0)+
        dbf     d0,macle
        rts

; *******************************************************************
; makecube
; *******************************************************************
makecube:
        lea     cmask1,a3
        movea.l a0,a2
        lea     4(a0),a0
        moveq   #$FFFFFFFF,d7
        move    #7,d0

iccu:
        move    #7,d1
        movea.l a3,a1

iccu2:
        move    #7,d2
        move.b  (a1)+,d6

iccu3:
        btst    d2,d6
        beq.w   nomatey
        move    d2,d3
        lsl.w   #2,d3
        sub.w   #$10,d3
        swap    d3
        move.l  d3,(a0)+
        move    d1,d3
        lsl.w   #2,d3
        sub.w   #$10,d3
        swap    d3
        move.l  d3,(a0)+
        move    d0,d3
        lsl.w   #2,d3
        sub.w   #$10,d3
        swap    d3
        move.l  d3,(a0)+
        move    d0,d4
        lsl.w   #5,d4
        move    d1,d5
        lsl.w   #1,d5
        or.w    d5,d4
        lsl.w   #8,d4
        move    d4,(a0)+
        move    d2,d4
        lsl.w   #4,d4
        lsl.w   #8,d4
        add.w   #$8000,d4
        move    d4,(a0)+
        clr.l   (a0)+
        clr.l   (a0)+
        clr.l   (a0)+
        clr.l   (a0)+
        add.l   #1,d7

nexx:
        dbf     d2,iccu3
        dbf     d1,iccu2
        dbf     d0,iccu
        move.l  d7,(a2)
        rts

nomatey:
        move    #0,$E(a0)
        lea     $20(a0),a0
        bra.s   nexx

; ---------------------------------------------------------------------------
        dc.b $00,$01,$03,$02
; ---------------------------------------------------------------------------

; *******************************************************************
; ObTypes
; The 8 different object types in our display object list. The first one is the
; main one and it is this one that the GPU and Blitter draws most
; stuff to.
; Each entry is four words (16 bytes) long and is of the form:
; - Width
; - Height
; - Depth
; - Offset to Colour Lookup Table (CLUT).
; *******************************************************************
ObTypes:         dc.w    96,  384,  4,    0 ; Full screen
                 dc.w     5,  240,  0,    0
                 dc.w     1,    8,  0,    0
                 dc.w    96,  192,  4,    0
                 dc.w    96,   96,  4,    0
                 dc.w     5,  112,  0,   16
                 dc.w    16,   48,  2,    4
                 dc.w     8,   10,  4,    0

;sines.bin

.include "sines.dat"
.include "sympad.dat"

versionp:       dc.b 'Virtual Light Machine v0.9//(c) 1994 Virtual Light Company Ltd.'
                dc.b '/Jaguar CD-ROM version/(c) 1994 Atari Corporation//FFT code by '
                dc.b 'ib2/Grafix code by Yak/~c100:',0
clearstr:       dc.b '@',0
banset:         dc.b '~g1:20:Bank>',0
banclr:         dc.b '~g1:20:     ',0
edithead:       dc.b '@~g1:1:Edit Mode~e3:4:',0

edit2hea:       dc.b '@~g1:1:Editing: Effect~e3:4:',0

symedhea:       dc.b '@~g1:1:Editing: Symmetry Generator~e3:3:',0

dvfedhea:       dc.b '@~g1:1:Editing: Digital Video Feedback~e3:3:',0
wavedhea:       dc.b '@~g1:1:Editing: Wave Plotter~e3:3:',0

isphead1:       dc.b '@~g1:1:Spectrum and Triggers~e3:3:',0

isphead2:       dc.b '@~g1:1:Trigger Settings~e3:3:',0

isphead3:       dc.b '@~g1:1:Adjust width using joypad',0

isphead4:       dc.b '@~g1:1:Adjust trigger minimum with pad',0

kpasshea:       dc.b '@~g1:1:Assign effect to keypad//Press the number key 1-9 to whi'
                dc.b 'ch//you want this effect attached',0
ogohead:        dc.b '@~g1:1:Object Giver Outer~e3:3:',0

adedhead:       dc.b '@~g1:1:Edit ADSR channel settings~e3:3:',0
adedhead2:      dc.b '@~g1:1:Edit ADSR envelope shape~e3:3:',0

wfahead:        dc.b '@~g1:1:Attach and Adjust Waveforms~e3:3:',0
subfxhea:       dc.b '@~g1:1:Choose a subeffect slot to edit~e3:3:',0

awhead:         dc.b 'Attach and Adjust Waveforms//Press keys 1 to 8 to link waveform'
                dc.b 's//Use the joypad to change amplitude~g1:20:Press any FIRE butt'
                dc.b 'on to exit',0
wshead:         dc.b '@~g1:1:Edit basic waveforms~e3:3:',0

fxphead:        dc.b '@~g1:1:Choose fx page~e3:3:',0

bline3:         dc.b '~g1:20:<A> ADD    <B> EXIT   <C> SUB',0

clearhom:
                dc.b '@~g1:1:',0
normal:         dc.b 'Standard Mode~c30:',0
unimp:          dc.b 'This function not yet implemented~c30:',0

op11:           dc.b 'Edit this effect',0
                dc.b 'Assign this effect to keypad',0
                dc.b 'Change system settings',0
                dc.b 'Edit envelopes and triggers',0
                dc.b 'Delayline settings',0
                dc.b 'Compress and store matrix',0
                dc.b 'Reset save pointers in ROMulator',0
                dc.b 'Test matrix retrieve',0
op19:           dc.b 'Spectrum and triggers',0

op21:           dc.b 'Edit source function',0

op22:           dc.b 'Edit symmetry generator',0

op23:           dc.b 'Edit source waves',0
op24:           dc.b 'Change effect',0
op31:           dc.b 'Waveform attach (x)',0

op32:           dc.b 'Waveform attach (y)',0

op71:           dc.b 'Trigger 1',0
op72:           dc.b 'Trigger 2',0
op73:           dc.b 'Trigger 3',0
op74:           dc.b 'Trigger 4',0
op75:           dc.b 'Trigger 5',0
op81:           dc.b 'Set Width',0
op82:           dc.b 'Set Trigger Minimum',0

bline1:         dc.b '~g1:20:<A> Edit   <B> Edit   <C> Back',0

bline2:         dc.b '~g1:20:Joypad to select, any FIRE to edit',0

bline4:         dc.b '~g1:20:Up,Down to choose, L,R to change',0

bline5:         dc.b '~g1:18:Hold down b and use up,down to//change channel',0

edlhead:        dc.b '@~g1:1:Delay line settings//U,D changes number L,R changes spac'
        dc.b 'ing~e3:7:',0
stashhea:       dc.b '@~g1:1:Compressing matrix to ROM//Press any fire to exit',0
rsethead:       dc.b '@~g1:1:Reset ROM save pointers//Press any fire to exit',0

eparm0:         dc.b 'Edit: ',0
eparm1:         dc.b '~g1:20:<A> Prev   <B> Menu   <C> Next',0

eparm2:         dc.b '~g1:18:Press ~i+*~i- to attach waveforms',0

empt:           dc.b '<Empty>',0
symplane:       dc.b 'Editing: Symmetry Planes and Types//Press number keys to turn o'
                dc.b 'ff or on~g9:8:----- Rotational symmetry~g12:14:- Clear~g12:16:-'
                dc.b ' Invert',0
bytemask:       dc.b 'Byte bitmap.  Press buttons to//change a bit.',0
                dc.b 'Nothing',0
                dc.b 'Poly object',0
                dc.b 'DVF area plus clear',0
                dc.b 'DVF area no clear',0
                dc.b 'Ring of pixels',0
                dc.b '3D starfield',0
                dc.b 'WaveSurf',0
                dc.b 'Psychedelic Bitmap',0
                dc.b 'I-Ripple Bitmap',0
                dc.b 'DVF --> scale2',0
                dc.b 'DVF --> scale4',0
                dc.b 'Colour plasma 1',0
                dc.b 'Colour plasma 2',0
                dc.b 'Particle Object',0
                dc.b 'Particle Motion',0
                dc.b 'Mono particle object',0
                dc.b 'Matrix',0
                dc.b 'Spectrum as intensities',0
                dc.b 'Jaguar Logo',0
dlo1:           dc.b 'Off',0
dlo2:           dc.b 'channel 1',0
dlo3:           dc.b 'channel 2',0
dlo4:           dc.b 'channel 3',0
dlo5:           dc.b 'channel 4',0
dlo6:           dc.b 'channel 5',0
dlo7:           dc.b 'channel 6',0
                dc.b 'FX page 1',0
                dc.b 'FX page 2',0
        .even
fxopt:          dc.w 1
                dc.l $19, $775A0019, $7B0D0019, $405E0019, $7B170019
                dc.b $40, $84
availobj:       dc.w $10
                dc.l $19, $775A0019, $79AD0019, $23800019, $79B50019, $23A20019
                dc.l $79C10019, $24DA0019, $79D50019, $24CA0019, $79E70019
                dc.l $24F20019, $79F60019, $257E0019, $7A030019, $258C0019
                dc.l $7A0C0019, $259A0019, $7A1F0019, $25EE0019, $7A2F0019
                dc.l $24760019, $7A3E0019, $24A00019, $7A4D0019, $25B60019
                dc.l $7A5D0019, $25A80019, $7AC10019, $25DA0019, $7A6D0019
                dc.l $25520019, $7A7D0019, $25000019, $7AA20019, $251E0019
                dc.l $7A8D0019
                dc.b $25, $44
avail2:         dc.w 1
                dc.l $19, $775A0019, $7AA20019, $251E0019, $7AA90019
tp0:            dc.b '%h~g1:6:',0
tp1:            dc.b 'DELTABLOCK generated ',0
tp2:            dc.b ' bytes/',0
tp3:            dc.b '55296 bytes ---> ',0
op51:           dc.b 'Edit ADSR a',0
op52:           dc.b 'Edit ADSR b',0
op53:           dc.b 'Edit ADSR c',0
op61:           dc.b 'A:',0
op62:           dc.b 'D:',0
op63:           dc.b 'S:',0
op64:           dc.b 'R:',0
op41:           dc.b '1:',0
op42:           dc.b '2:',0
op43:           dc.b '3:',0
op44:           dc.b '4:',0
op45:           dc.b '5:',0
op46:           dc.b '6:',0
op47:           dc.b '7:',0
op48:           dc.b '8:',0
                dc.b 'Sine wave          ',0
                dc.b 'Sawtooth wave      ',0
                dc.b 'Square wave        ',0
                dc.b 'Ramp               ',0
                dc.b 'Rectified sine wave',0
                dc.b 'Noise              ',0
                dc.b 'Constant           ',0
                dc.b 'User control Y     ',0
                dc.b 'User control X     ',0
wt:             dc.b '~g2:12:',0
                dc.b 0
wts:            dc.b 0
                dc.b $19
                dc.l $7C690019, $7C7D0019, $7C910019, $7CA50019, $7CB90019
                dc.l $7CCD0019, $7CE10019, $7CF50019
                dc.b $7D, 9
word_197D4A:    dc.w $1E
                dc.l $7540001E, $7644001E, $7748001E, $784C001E, $79500019
                dc.l $6F4C001E
                dcb.l 2,$7540001E
                dc.b $75, $40
padchars:       dc.w $2A87
                dc.b $84
                dc.b $81, $23, $89
                dc.b $86
                dc.b $83, $80, $88
                dc.b $85
                dc.b $82
padbits:        dc.b 3, $B
                dc.b  $A
                dc.b 6, 9, 7
                dc.b   2
                dc.b 1
pad8bits:       dc.b 3, $B
                dc.b   7
                dc.b 2, $A, 6
                dc.b   1
unp0:           dc.b 9,'@~g1:1:Unpacking matrix//',0
unp1:           dc.b 'Copying reference block/',0
unp2:           dc.b 'Unpacking base effect/',0
unp3:           dc.b 'Unpacking subsequent effect/',0
unp4:           dc.b 'Activating slot 1//',0
unp5:           dc.b 'Done~c50:',0
cmask1:         dc.b $FF
cmask2:         dcb.l 2,0
cmask3:
cmask4:
                dc.l $FF000000, 0
pbinfo:         dc.b 'Parameter not yet defined    ',0

editinginfo:    dc.w 0
                dcb.l 2,0
                dc.b 'DVF window size: X           ',0
                dc.w $102
                dc.l $4020001, $1800000
                dc.b 'DVF window size: Y           ',0
                .dphrase
                dc.l $4020002, $1800000
                dc.b 'DVF scale: X                 ',0
                dc.w $104
                dc.l $4020003, $3FFFFFF
                dc.b 'DVF scale: Y                 ',0
                .dphrase
                dc.l $4020004, $3FFFFFF
                dc.b 'DVF rotate angle             ',0
                .long
                dc.l $2820005, $1FFFFFF
                dc.b 'DVF centre of rotation: X    ',0
                dc.w $107
                dc.l $1020006, $1800000
                dc.b 'DVF centre of rotation: Y    ',0
                .long
                dc.l $1020007, $1800000
                dc.b 'DVF Delta Intensity          ',0
                .dphrase
                dc.l $2020008, $FFFFFF
                dc.b 'Destination position: X      ',0
                dc.w ntsc_vmi
                dc.l $1020009, $1800000
                dc.b 'Destination position: Y      ',0
                .dphrase
                dc.l $102000A, $1800000
                dc.b 'DVF window centre: X         ',0
                dc.w $10C
                dc.l $102000B, $1800000
                dc.b 'DVF window centre: Y         ',0
                .dphrase
                dc.l $102000C, $1800000
                dc.b 'Destination position: Z      ',0
                .long
                dc.l $302000D, $7FFFFFF
                dc.b 'Vector: X                    ',0
                dc.w $110
                dc.l $582000E, $FFFFFF
                dc.b 'Destination Y offset         ',0
                .long
                dc.l $382000F, $1800000
                dc.b 'Vector: Y                    ',0
                .dphrase
                dc.l $5820010, $FFFFFF
                dc.b 'Symmetry Types               ',0
                .long
                dc.b   6
                dc.b 3, 0, $11
                dc.l init_sym
                dc.b 'Rotational Symmetry Order    ',0
                .dphrase
                dc.l $2010012, UPDA1F
                dc.b 'Rotational Angle Step        ',0
                .long
                dc.l $2020013, $FFFFFF
                dc.b 'Rotational Angle Step Delta  ',0
                .dphrase
                dc.l $3820014, $FFFFF
                dc.b 'Intensity 1                  ',0
                .long
                dc.l $2020015, $FFFF
                dc.b 'Intensity 2                  ',0
                .dphrase
                dc.l $2020016, $FFFF
                dc.b 'Intensity 3                  ',0
                .long
                dc.l $2020017, $FFFF
                dc.b 'Intensity 4                  ',0
                .dphrase
                dc.l $2020018, $FFFF
                dc.b 'Intensity 5                  ',0
                .long
                dc.l $2020019, $FFFF
                dc.b 'Intensity 6                  ',0
                .dphrase
                dc.l $202001A, $FFFF
                dc.b 'Z amplitude                  ',0
                .long
                dc.l $201001B, $FFFF
                dc.b 'Fixed point phase 1          ',0
                .dphrase
                dc.l $382001C, $1FFFFFF
                dc.b 'Delta phase 1                ',0
                .long
                dc.l $382001D, $1FFFFF
                dc.b 'Rotational sym overall phase ',0
                .dphrase
                dc.l $382001E, $1FFFFFF, $46697865, $6420706F, $696E7420
                dc.l $70686173, $65203220
                dcb.l 2,$20202020
                dc.l abutton, $382001F, $1FFFFFF
                dc.b 'Number of iterations         ',0,0,0,2,2,0,' ',1,0,0,0,'X ampli'
                dc.b 'tude                  ',0,0,0,2,1,0,'!',0,0,$FF,$FF,'Y amplitud'
                dc.b 'e                  ',0,0,0,2,1,0,'"',0,0,$FF,$FF,'Nu'
                dc.b 'mber of other iterations   ',0
                .long
                dc.l $2020023, LFU_A
                dc.b 'Parameter not yet defined    ',0
                .dphrase
                dcb.l 2,0
                dc.b 'delta Z                      ',0
                .long
                dc.l $2820025, $FFFFFF
                dc.b 'choice of Thang              ',0
                .dphrase
                dc.l $3020026, $FFFFFF
                dc.b 'Parameter not yet defined    ',0
                .dphrase
                dc.b 'Parameter not yet defined    ',0
                .dphrase
                dcb.l 2,0
                dc.b 'Parameter not yet defined    ',0
                .dphrase
                dc.b 'Rotational Sym centre: X     ',0
                dc.w $12B
                dc.l $102002A, $1800000
                dc.b 'Rotational Sym centre: Y     ',0
                .long
                dc.l $2002B, $1800000
                dc.b 'Rotational Symmetry scale    ',0
                .dphrase
                dc.l $381002C, $3FFF
                dc.b 'Rotational scale delta: X    ',0
                dc.w $130
                dc.l $481002D, $3FF
                dc.b 'Colour generator vector: X   ',0
                dc.w $12F
                dc.l $582002E, $7FFFF
                dc.b 'Colour generator vector: Y   ',0
                .long
                dc.l $582002F, $7FFFF
                dc.b 'Rotational scale delta: Y    ',0
                .dphrase
                dc.l $5810030, $1000
                dc.b 'Rotational centre delta: X   ',0
                dc.w $132
                dc.l $5820031, $FFFFF
                dc.b 'Rotational centre delta: Y   ',0
                .dphrase
                dc.l $4820032, $FFFFF
                dc.b 'Delta phase 2                ',0
                .long
                dc.l $3820033, $1FFFFF
                dc.b 'Parameter not yet defined    ',0
                .dphrase
                dcb.l 2,0
                dc.b 'Radius                       ',0
                .long
                dc.l $3820035, $FFFFFFF
                dc.b 'Base col generator vector: X ',0
                dc.w $137
                dc.l $5820036, $1FFFFF
                dc.b 'Base col generator vector: Y ',0
                .long
                dc.l $5820037, $1FFFFF
                dc.b 'Base colour: X               ',0
                dc.w $139
                dc.l $5820038, $1FFFFF
                dc.b 'Base colour: Y               ',0
                .long
                dc.l $5820039, $1FFFFF
                dc.b 'Destination plot routine     ',0
                .dphrase
                dc.b   6
                dc.b 3, 0, $3A
                dc.l init_sym
                dc.b 'Maximum pixel size           ',0
                .long
                dc.l $202003B, $7FFFFF
                dc.b 'Parameter not yet defined    ',0
                .dphrase
                dcb.l 2,0
                dc.b 'Trigger mask                 ',0
                .long
                dc.l $703003D, 0
                dc.b 'Parameter not yet defined    ',0
                .dphrase
                dcb.l 2,0
                dc.b 'Parameter not yet defined    ',0
                .dphrase
symvars:        dc.l $111E1213, $142A2B2C, $2D302E2F, $313206FF
dvfvars:        dc.l $1030506, $80B38FF
vars:           dc.l polyvars ; "Draw a polygon object        "
                dc.l sfvars         ; "Draw 3D starfield            "
                dc.l ringvars         ; "Draw a ring of pixels        "
                dc.l dvf_vars         ; "Digital Video Feedback area  "
                dc.l wsu_vars         ; "Wave Surface Thang           "
                dc.l monomapv         ; "Draw mono bitmap coloured    "
                dc.l monomapv2        ; "Draw mono bitmap i-shaded    "
                dc.l dvf_vars         ; "Digital Video Feedback area  "
                dc.l dvf_vars         ; "Digital Video Feedback area  "
                dc.l plas1var         ; "Colour plasma area type SRCEN    "
                dc.l logovars         ; "Big Jaguar hardware sprite   "
                dc.l obvars         ; "Draw particle object         "
                dc.l pmvars         ; "Do particle motion           "
                dc.l shuuvars         ; "Spectrum as intensities      "
polyvars:       dc.b 'Draw a polygon object        ',0
                dc.w $921
                dc.l $221C1516, $1718191A
                dc.b $26, $3D, $FF
sfvars:         dc.b 'Draw 3D starfield            ',0
                dc.b 9, $D, $20
                dc.l $3B3839FF
obvars:         dc.b 'Draw particle object         ',0
                dc.w $90D
                dc.l $3B0E2122, $38392E2F, $1C1D1F15, $161B2526
                dc.b $FF
pmvars:         dc.b 'Do particle motion           ',0
                dc.b $16
                dc.l $2023090D, $E251718, $1938152E
                dc.b $33, $FF
ringvars:       dc.b 'Draw a ring of pixels        ',0
                dc.l $91C1F20, $3536383B, $E1516FF
dvf_vars:       dc.b 'Digital Video Feedback area  ',0
                dc.w $103
                dc.l $506080B
                dc.b $38, $FF
wsu_vars:       dc.b 'Wave Surface Thang           ',0
                dc.l $90D1C1D, $33202322, $E3B3638
                dc.b $FF
monomapv:       dc.b 'Draw mono bitmap coloured    ',0
                dc.b 9
                dc.l $2E363815
                dc.b $26, $3D, $FF
monomapv2:      dc.b 'Draw mono bitmap i-shaded    ',0
                dc.b 9, $2E, $36
                dc.l $3815263D
                dc.b $FF
plas1var:       dc.b 'Colour plasma area type 1    ',0
                dc.b 9
                dc.l $2E363821
                dc.b $22, $3D, $FF
logovars:       dc.b 'Big Jaguar hardware sprite   ',0
                dc.b 9, $38, $15
                dc.b $FF
shuuvars:       dc.b 'Spectrum as intensities      ',0
                dc.b 9

; other.bin
                dc.l $38363B22, $FF000000, 0
                dc.l $FFFFFFFF, 0
                dc.l $FFFFFFFF, 0
                dc.l $FFFFFFFF, 0
                dc.l $FFFFFFFF, 0
                dc.l $FFFFFFFF, 0
                dc.l $FFFFFFFF, 0
                dc.l $FFFFFFFF, 0
                dc.l $FFFFFFFF, 0
                dc.l eparm1         ; "~g1:$20:<A> Prev   <B> Menu   <C> Next"
option1:        dc.l star, bline1, op11, thisfx, op19, ispec

option2:        dc.l $20000, bline1, op21, foredit, op22, symedit
                dc.l op23, wsedit
option3:        dc.l star, bline1, op31, iawfx, op32, iawfy

option4:        dc.l $70000, bline3, op41, rrts, op42, rrts
                dc.l op43, rrts, op44, rrts, op45, rrts
                dc.l op46, rrts, op47, rrts, op48, rrts
option5:        dc.l $20000, bline1, op51, ahead2, op52, ahead2
                dc.l op53, ahead2
option6:        dc.l XADDINC, bline4, op61, rrts, op62, rrts
                dc.l op63, rrts, op64, rrts
option7:        dc.l $40000, bline1, op71, ispec2, op72, ispec2
                dc.l op73, ispec2, op74, ispec2, op75, ispec2
option8:        dc.l star, bline1, op81, ispec3, op82, ispec4
kselbutt:       dc.l $FF000000, $10070000, $A070000, $4070000, $FF070000
                dc.l $70000, $E070000, $8070000, $FF070000, $12070000
                dc.l $C070000, $6070000
wfpad:          dc.l  0
                dc.l swf9
                dc.l swf4
                dc.l swf1
                dc.l 0
                dc.l swf8
                dc.l swf6
                dc.l swf3
                dc.l 0
                dc.l swf7
                dc.l swf5
                dc.l swf2
clut_sha:       dcb.w 2,0
lol:            dc.l bmobj
                dc.l skale, beasties, pobjs, mpobjs, mmasks
                dc.l maxes, absdelta, zerstart, avbank, envvals
                dc.l _fsync, freerun
bmobj:          dc.l jlogo2
                dc.l jaglogo
                dc.l blokk
                dc.l blokk
pobjs:          dc.l cubeobj
                dc.l genobj
mpobjs:         dc.l monoobj
                dc.l spherobj
mmasks:         dc.l cmask1
                dc.l cmask2
                dc.l cmask4
                dc.l cmask4
                dc.l $4FFFC
                dc.l $FFFC0000
                dc.l $FFFEFFFE
                dc.l 0
                dc.l 0
                dc.l $20001, $04, $30001

; Signal whether we're in audio reactive mode.
; Put a 2 in here to stop GPU in free run
freerun:        dcb.w 2,0

; font.bin
.include "font.dat"

jlogo2:         .incbin "images/jlogo2.cry"

blokk:          dc.l $1003F
davesvec:       dc.l rrts
                dcb.l $20,0
                dc.l $18000000, 0
                dc.l $18000000, 0
                dc.l $18000000, 0
                dc.l $FF000000, 0
                dc.l $FF000000, 0
                dc.l $18000000, 0
                dc.l $18000000, 0
                dc.l $18000000, 0
awfbutts:       dc.l $FF000000, $10070000, $A070000, $4070000
                dc.b $19, 9
ant_data:       dc.w 0
                dc.l $FF070000, $E070000, $8070000, $FF070000, $12070000
                dc.l $C070000, $6070000
awfb2:          dc.l $FF000000, $10090000, $A090000, $4090000
                dcb.l 2,$FF070000
                dc.l $E090000, $8090000, $FF070000, $12090000, $C090000
                dc.l $6090000
kpassbut:       dc.l $FF000000, $70C0000, $70A0000, $7080000, $FF0C0000
                dc.l $B0C0000, $B0A0000, $B080000, $FF070000, $90C0000
                dc.l $90A0000, $9080000
symbutts:       dc.l $80E0100, $5090000, $B090000, $7050000, $8100100
                dc.l $8080000, $70B0000, $B070000, $FF000000, $5070000
                dc.l $90B0000, $9050000
oscbank:        dc.l $400000, $11100, p_sines
                dcb.l 2,0
                dc.l $16300, p_sines
                dcb.l 2,0
                dc.l $3000, p_sines
                dcb.l 2,0
                dc.l $C000, p_sines
                dcb.l 2,0
                dc.l star, p_sines
                dcb.l 2,0
                dc.l $E000, p_sines
                dcb.l 2,0
                dc.l $C300, p_sines
                dcb.l 2,0
                dc.l $F310, p_sines, 0
pixcon:         dc.l $800000, 0
                dc.l $1600, $3000, $FFFF00, 0
                dc.l $40000
piycon:         dc.l $800000, 0
                dc.l $1600, $3000, $FFFF00, 0
                dc.l $40000
adsra:          dc.b   $A,   0,   1,   0, $C0,   0,   5,   0
                dc.b    0,   4
adsrb:          dc.b   $B,   0,   1,   0, $C0,   0,   5,   0
                dc.b    0,   4
adsrc:          dc.b   $C,   0,   2,   0, $C0,   0,   5,   0
                dc.b    0,   4,   0,   0
py:             dc.l 0
px:             dc.l 0
delayf:         dc.w 0
delayp:         dc.w 0
dline:          dc.l 0
delayt:         dc.w 0
delayn:         dcb.w 9,0
edlgauge:       dc.b '~g3:5:                                 ',0
edlgauge2:      dcb.l 2,0

jaglogo:        .incbin "images/jaglogo.cry"
vlmlogo:        .incbin "images/vlmlogo.cry"

ixcon:          dc.l $800000
                dc.l 0
                dc.l UPDA2, $20000, $FFFF00, 0
                dc.l $20000
iycon:          dc.l $800000
                dc.l 0
                dc.l UPDA2, $20000, $FFFF00, 0
                dc.l $20000
dlset:          dc.b 0, 6
                dc.w 0
                dc.l bline5         ; "~g1:$18:Hold down b and use up,down to//"...
                dc.l dlo1         ; "Off"
                dc.l rrts
                dc.l dlo2         ; "channel 1"
                dc.l rrts
                dc.l dlo3         ; "channel 2"
                dc.l rrts
                dc.l dlo4         ; "channel 3"
                dc.l rrts
                dc.l dlo5         ; "channel 4"
                dc.l rrts
                dc.l dlo6         ; "channel 5"
                dc.l rrts
                dc.l dlo7         ; "channel 6"
                dc.l rrts

avbank:
				dc.b  $00,$01,$00,$06,$70,$00,$10,$00 ; 0x0
				dc.b  $00,$07,$00,$0A,$60,$00,$10,$00 ; 0x8
				dc.b  $00,$0B,$00,$18,$50,$00,$10,$00 ; 0x10
				dc.b  $00,$19,$00,$28,$40,$00,$10,$00 ; 0x18
				dc.b  $00,$29,$00,$3F,$30,$00,$10,$00 ; 0x20
				dc.b  $00,$00,$00,$00,$00,$00,$00,$00 ; 0x28
				dc.b  $00,$00,$00,$0A,$00,$90,$B6,$9C ; 0x30
				dc.b  $00,$90,$00,$38,$F4,$35,$2A,$C6 ; 0x38
				dc.b  $00,$90,$02,$00,$00,$90,$18,$56 ; 0x40
				dc.b  $00,$90,$34,$A4,$00,$90,$44,$82 ; 0x48
				dc.b  $00,$90,$57,$76,$00,$90,$61,$D8 ; 0x50
				dc.b  $00,$90,$75,$A6,$00,$90,$88,$06 ; 0x58
				dc.b  $00,$90,$9A,$72,$00,$90,$A6,$B4 ; 0x60
				dc.b  $66,$04,$9E,$A9,$C8,$31,$65,$28 ; 0x68
				dc.b  $8A,$ED,$DC,$28,$08,$74,$24,$27 ; 0x70
				dc.b  $42,$2B,$CE,$06,$B3,$08,$5A,$2B ; 0x78
				dc.b  $27,$34,$55,$82,$D6,$8A,$42,$42 ; 0x80
				dc.b  $DA,$A7,$48,$2F,$4E,$E8,$29,$2B ; 0x88
				dc.b  $F8,$08,$43,$06,$7F,$AF,$24,$98 ; 0x90
				dc.b  $42,$20,$3E,$C5,$FE,$C9,$04,$D6 ; 0x98
				dc.b  $3E,$E3,$CA,$D8,$C5,$3C,$A7,$09 ; 0xa0
				dc.b  $A5,$0A,$C5,$00,$A3,$83,$DE,$52 ; 0xa8
				dc.b  $6F,$0E,$FE,$86,$38,$4B,$8A,$FA ; 0xb0
				dc.b  $7A,$E8,$8C,$4D,$83,$D5,$0A,$2E ; 0xb8
				dc.b  $81,$0A,$65,$3A,$4D,$70,$AC,$A7 ; 0xc0
				dc.b  $42,$66,$2B,$EF,$78,$AF,$63,$F8 ; 0xc8
				dc.b  $6F,$10,$BA,$B8,$30,$BF,$02,$28 ; 0xd0
				dc.b  $0E,$24,$8D,$FA,$3C,$26,$F8,$8E ; 0xd8
				dc.b  $CB,$56,$75,$C2,$36,$55,$64,$2A ; 0xe0
				dc.b  $A6,$6E,$2A,$C0,$29,$2B,$0E,$24 ; 0xe8
				dc.b  $F2,$DC,$30,$EA,$30,$D0,$EE,$F0 ; 0xf0
				dc.b  $05,$2C,$46,$B0,$F6,$54,$CF,$FC ; 0xf8
				dc.b  $75,$87,$04,$CE,$CD,$83,$10,$70 ; 0x100
				dc.b  $94,$9A,$2E,$22,$D2,$AB,$4B,$11 ; 0x108
				dc.b  $4E,$2E,$6D,$47,$00,$A5,$A9,$2B ; 0x110
				dc.b  $14,$CE,$E1,$12,$69,$59,$12,$A4 ; 0x118
				dc.b  $E9,$12,$E8,$45,$13,$27,$32,$55 ; 0x120
				dc.b  $03,$26,$40,$A3,$18,$30,$71,$41 ; 0x128
				dc.b  $14,$4D,$83,$14,$89,$1C,$6A,$E2 ; 0x130
				dc.b  $41,$18,$E7,$23,$15,$62,$33,$15 ; 0x138
				dc.b  $A4,$82,$25,$04,$10,$2A,$62,$BC ; 0x140
				dc.b  $99,$45,$60,$2D,$27,$B8,$38,$C5 ; 0x148
				dc.b  $4A,$43,$4F,$BC,$EA,$0A,$83,$01 ; 0x150
				dc.b  $49,$88,$D6,$A2,$62,$6F,$C7,$52 ; 0x158
				dc.b  $FA,$42,$54,$F2,$62,$1E,$3C,$C5 ; 0x160
				dc.b  $DC,$2E,$87,$4A,$A5,$63,$EE,$6B ; 0x168
				dc.b  $06,$32,$51,$C8,$18,$88,$C2,$22 ; 0x170
				dc.b  $6C,$26,$E0,$D8,$27,$62,$74,$50 ; 0x178
				dc.b  $E0,$3A,$23,$16,$C9,$63,$0E,$13 ; 0x180
				dc.b  $C4,$F0,$7A,$86,$44,$B3,$07,$3A ; 0x188
				dc.b  $C0,$0E,$3F,$1F,$22,$15,$08,$45 ; 0x190
				dc.b  $60,$B0,$E2,$6C,$9E,$EC,$24,$3A ; 0x198
				dc.b  $45,$6C,$2E,$50,$B4,$25,$8E,$38 ; 0x1a0
				dc.b  $21,$4A,$44,$74,$CA,$3A,$56,$11 ; 0x1a8
				dc.b  $12,$31,$E6,$A6,$30,$E8,$A6,$71 ; 0x1b0
				dc.b  $22,$2A,$A1,$E7,$F5,$07,$25,$D0 ; 0x1b8
				dc.b  $46,$CC,$8A,$A3,$E8,$F8,$A4,$82 ; 0x1c0
				dc.b  $82,$38,$E2,$9A,$BD,$A5,$A2,$8C ; 0x1c8
				dc.b  $F1,$63,$18,$44,$32,$ED,$46,$CA ; 0x1d0
				dc.b  $2C,$C6,$EE,$AF,$A7,$C8,$AE,$E2 ; 0x1d8
				dc.b  $2E,$4A,$A8,$C8,$81,$0D,$27,$25 ; 0x1e0
				dc.b  $12,$F1,$23,$2D,$D7,$18,$CC,$BE ; 0x1e8
				dc.b  $55,$EA,$5C,$AB,$87,$22,$74,$CE ; 0x1f0
				dc.b  $1E,$51,$0B,$D2,$D0,$12,$4F,$27 ; 0x1f8
				dc.b  $4B,$7E,$AD,$AD,$B7,$31,$D2,$5E ; 0x200
				dc.b  $A7,$12,$62,$94,$A4,$CC,$DD,$52 ; 0x208
				dc.b  $6C,$8E,$87,$77,$27,$24,$FA,$E2 ; 0x210
				dc.b  $A7,$DF,$27,$C4,$AB,$28,$12,$7D ; 0x218
				dc.b  $28,$72,$F8,$45,$92,$8E,$E9,$92 ; 0x220
				dc.b  $92,$49,$32,$96,$DF,$6A,$88,$4A ; 0x228
				dc.b  $01,$14,$00,$80,$01,$01,$00,$48 ; 0x230
				dc.b  $00,$38,$01,$01,$00,$12,$00,$20 ; 0x238
				dc.b  $0F,$FC,$03,$78,$02,$5E,$00,$EB ; 0x240
				dc.b  $00,$FF,$5D,$00,$00,$65,$00,$40 ; 0x248
				dc.b  $0B,$01,$60,$65,$00,$21,$00,$93 ; 0x250
				dc.b  $00,$02,$07,$04,$01,$01,$01,$00 ; 0x258
				dc.b  $1A,$80,$00,$00,$02,$80,$00,$00 ; 0x260
				dc.b  $DF,$FF,$19,$EF,$00,$28,$02,$B9 ; 0x268
				dc.b  $00,$DF,$E0,$00,$00,$32,$00,$FA ; 0x270
				dc.b  $00,$B1,$0D,$8E,$00,$DC,$00,$43 ; 0x278
				dc.b  $0D,$35,$00,$EE,$00,$30,$0D,$D7 ; 0x280
				dc.b  $00,$B8,$00,$C0,$0D,$1F,$00,$A1 ; 0x288
				dc.b  $0E,$FB,$00,$AC,$00,$E0,$0D,$DB ; 0x290
				dc.b  $00,$17,$00,$A3,$0D,$11,$00,$17 ; 0x298
				dc.b  $00,$CD,$00,$10,$6F,$90,$01,$1B ; 0x2a0
				dc.b  $00,$F3,$00,$F0,$17,$00,$FF,$00 ; 0x2a8
				dc.b  $FF,$FF,$FF,$00,$E0,$FF,$00,$FF ; 0x2b0
				dc.b  $03,$01,$00,$78,$00,$C0,$17,$00 ; 0x2b8
				dc.b  $01,$14,$00,$80,$16,$BF,$00,$70 ; 0x2c0
				dc.b  $0A,$E3,$00,$D4,$00,$80,$01,$DF ; 0x2c8
				dc.b  $00,$1E,$00,$80,$0D,$09,$00,$0C ; 0x2d0
				dc.b  $05,$FF,$00,$B4,$00,$10,$2E,$5A ; 0x2d8
				dc.b  $00,$EA,$0E,$06,$00,$4D,$20,$FF ; 0x2e0
				dc.b  $30,$01,$00,$06,$00,$80,$03,$08 ; 0x2e8
				dc.b  $03,$08,$02,$0D,$00,$E5,$00,$80 ; 0x2f0
				dc.b  $00,$FF,$00,$F6,$00,$A2,$00,$80 ; 0x2f8
				dc.b  $03,$02,$04,$65,$00,$21,$00,$93 ; 0x300
				dc.b  $00,$02,$07,$03,$03,$09,$25,$10 ; 0x308
				dc.b  $00,$80,$00,$00,$01,$20,$00,$80 ; 0x310
				dc.b  $00,$00,$44,$04,$00,$04,$0E,$04 ; 0x318
				dc.b  $00,$04,$00,$23,$00,$80,$50,$10 ; 0x320
				dc.b  $00,$04,$00,$13,$00,$24,$27,$FF ; 0x328
				dc.b  $25,$BE,$00,$56,$02,$BE,$00,$08 ; 0x330
				dc.b  $AA,$F5,$29,$00,$00,$C2,$00,$7D ; 0x338
				dc.b  $00,$75,$0D,$FC,$00,$95,$00,$4F ; 0x340
				dc.b  $0D,$01,$00,$41,$00,$10,$02,$01 ; 0x348
				dc.b  $00,$80,$08,$06,$00,$88,$00,$9B ; 0x350
				dc.b  $00,$C0,$0D,$B6,$00,$25,$0E,$9F ; 0x358
				dc.b  $00,$60,$00,$60,$0D,$8A,$00,$BE ; 0x360
				dc.b  $00,$2F,$0D,$AC,$00,$F0,$00,$81 ; 0x368
				dc.b  $00,$50,$6E,$0D,$00,$DE,$02,$73 ; 0x370
				dc.b  $18,$00,$01,$14,$00,$80,$21,$01 ; 0x378
				dc.b  $00,$00,$00,$38,$01,$01,$00,$05 ; 0x380
				dc.b  $00,$8E,$00,$80,$0C,$FF,$00,$ED ; 0x388
				dc.b  $00,$AC,$05,$FF,$00,$A2,$00,$8C ; 0x390
				dc.b  $3E,$07,$00,$90,$20,$FF,$31,$50 ; 0x398
				dc.b  $03,$00,$00,$EE,$01,$FF,$00,$FF ; 0x3a0
				dc.b  $00,$C0,$02,$05,$00,$3F,$00,$E0 ; 0x3a8
				dc.b  $01,$0B,$00,$EA,$00,$C0,$03,$02 ; 0x3b0
				dc.b  $01,$01,$00,$A3,$00,$FF,$00,$65 ; 0x3b8
				dc.b  $00,$21,$00,$93,$00,$02,$07,$03 ; 0x3c0
				dc.b  $03,$09,$25,$10,$00,$80,$00,$00 ; 0x3c8
				dc.b  $01,$20,$00,$80,$00,$00,$45,$10 ; 0x3d0
				dc.b  $10,$80,$00,$00,$50,$01,$00,$04 ; 0x3d8
				dc.b  $00,$20,$00,$80,$08,$01,$00,$04 ; 0x3e0
				dc.b  $03,$20,$19,$FF,$24,$01,$01,$38 ; 0x3e8
				dc.b  $02,$F3,$00,$90,$56,$07,$00,$90 ; 0x3f0
				dc.b  $51,$FF,$00,$FD,$03,$01,$00,$28 ; 0x3f8
				dc.b  $07,$24,$02,$08,$00,$EB,$18,$00 ; 0x400
				dc.b  $00,$0C,$00,$73,$00,$80,$0D,$5C ; 0x408
				dc.b  $00,$C2,$00,$80,$0D,$01,$00,$41 ; 0x410
				dc.b  $00,$70,$0C,$06,$00,$BC,$00,$A0 ; 0x418
				dc.b  $0E,$FB,$00,$80,$0E,$DC,$00,$10 ; 0x420
				dc.b  $0E,$BF,$00,$92,$00,$80,$0D,$EE ; 0x428
				dc.b  $00,$CA,$00,$38,$6D,$FF,$00,$FF ; 0x430
				dc.b  $04,$63,$18,$00,$01,$14,$00,$80 ; 0x438
				dc.b  $53,$00,$03,$00,$16,$67,$00,$8C ; 0x440
				dc.b  $0A,$49,$00,$44,$02,$2C,$00,$F0 ; 0x448
				dc.b  $20,$FF,$31,$48,$00,$D0,$0A,$05 ; 0x450
				dc.b  $00,$9A,$02,$0F,$00,$FF,$00,$E0 ; 0x458
				dc.b  $03,$02,$01,$01,$00,$A3,$00,$FF ; 0x460
				dc.b  $00,$65,$00,$21,$00,$93,$00,$02 ; 0x468
				dc.b  $07,$03,$03,$09,$71,$40,$0B,$01 ; 0x470
				dc.b  $56,$10,$00,$04,$00,$54,$00,$37 ; 0x478
				dc.b  $27,$FF,$7D,$17,$00,$80,$55,$FF ; 0x480
				dc.b  $00,$FC,$29,$00,$00,$4C,$00,$10 ; 0x488
				dc.b  $00,$98,$0D,$62,$00,$96,$00,$48 ; 0x490
				dc.b  $0E,$3A,$00,$80,$02,$01,$00,$60 ; 0x498
				dc.b  $08,$06,$00,$35,$00,$52,$0E,$47 ; 0x4a0
				dc.b  $00,$18,$0E,$3E,$00,$35,$0E,$36 ; 0x4a8
				dc.b  $00,$27,$00,$48,$0D,$43,$00,$80 ; 0x4b0
				dc.b  $00,$39,$00,$80,$6E,$12,$00,$EF ; 0x4b8
				dc.b  $03,$F8,$17,$00,$01,$14,$00,$80 ; 0x4c0
				dc.b  $22,$51,$00,$96,$02,$82,$00,$A4 ; 0x4c8
				dc.b  $5F,$00,$18,$FF,$34,$FF,$00,$FD ; 0x4d0
				dc.b  $00,$3D,$00,$80,$01,$04,$00,$05 ; 0x4d8
				dc.b  $01,$FF,$00,$FD,$00,$73,$00,$60 ; 0x4e0
				dc.b  $00,$FF,$00,$F6,$00,$89,$04,$03 ; 0x4e8
				dc.b  $01,$04,$00,$E3,$00,$FF,$00,$65 ; 0x4f0
				dc.b  $00,$21,$00,$93,$00,$02,$07,$0E ; 0x4f8
				dc.b  $01,$09,$01,$00,$FF,$FF,$FF,$00 ; 0x500
				dc.b  $00,$20,$00,$37,$00,$9A,$0D,$29 ; 0x508
				dc.b  $00,$91,$00,$AE,$0D,$05,$00,$9E ; 0x510
				dc.b  $00,$E0,$0D,$16,$00,$7B,$00,$80 ; 0x518
				dc.b  $0D,$1D,$00,$FA,$0E,$1A,$00,$3A ; 0x520
				dc.b  $00,$C0,$0D,$16,$00,$D5,$00,$6E ; 0x528
				dc.b  $0D,$1C,$00,$76,$00,$2D,$00,$A0 ; 0x530
				dc.b  $6E,$02,$00,$36,$1B,$00,$09,$0F ; 0x538
				dc.b  $00,$0E,$02,$84,$00,$FF,$00,$FF ; 0x540
				dc.b  $01,$84,$00,$FF,$00,$FF,$DE,$91 ; 0x548
				dc.b  $00,$00,$0B,$00,$FF,$FF,$FF,$00 ; 0x550
				dc.b  $00,$CF,$00,$AC,$00,$A2,$0D,$0D ; 0x558
				dc.b  $00,$BA,$00,$46,$0D,$24,$00,$78 ; 0x560
				dc.b  $00,$60,$0D,$91,$00,$E1,$00,$80 ; 0x568
				dc.b  $0D,$C2,$00,$82,$0E,$AA,$00,$31 ; 0x570
				dc.b  $00,$C0,$0D,$94,$00,$29,$00,$06 ; 0x578
				dc.b  $0D,$B8,$00,$AD,$00,$8E,$00,$20 ; 0x580
				dc.b  $6F,$00,$1B,$00,$FF,$00,$FF,$FF ; 0x588
				dc.b  $FF,$00,$FF,$00,$01,$00,$00,$00 ; 0x590
				dc.b  $16,$C0,$00,$00,$0A,$C0,$00,$00 ; 0x598
				dc.b  $00,$00,$01,$C0,$00,$00,$00,$00 ; 0x5a0
				dc.b  $0D,$00,$00,$00,$05,$00,$00,$00 ; 0x5a8
				dc.b  $00,$00,$2E,$00,$00,$00,$0E,$14 ; 0x5b0
				dc.b  $00,$00,$20,$03,$30,$00,$00,$08 ; 0x5b8
				dc.b  $00,$00,$03,$00,$03,$00,$02,$08 ; 0x5c0
				dc.b  $00,$F0,$00,$00,$00,$00,$00,$08 ; 0x5c8
				dc.b  $00,$F0,$00,$00,$03,$00,$04,$00 ; 0x5d0
				dc.b  $00,$00,$00,$00,$00,$00,$07,$00 ; 0x5d8
				dc.b  $03,$00,$25,$00,$00,$FF,$00,$FF ; 0x5e0
				dc.b  $01,$00,$00,$FF,$00,$FF,$44,$00 ; 0x5e8
				dc.b  $00,$00,$0E,$00,$00,$00,$00,$FF ; 0x5f0
				dc.b  $00,$FF,$50,$00,$00,$00,$00,$FF ; 0x5f8
				dc.b  $00,$FF,$27,$FF,$25,$00,$00,$00 ; 0x600
				dc.b  $02,$00,$00,$00,$AA,$00,$29,$00 ; 0x608
				dc.b  $00,$00,$00,$40,$00,$00,$0D,$00 ; 0x610
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0x618
				dc.b  $00,$00,$02,$00,$00,$30,$08,$00 ; 0x620
				dc.b  $00,$00,$00,$00,$00,$00,$0D,$00 ; 0x628
				dc.b  $00,$00,$0E,$00,$00,$00,$00,$00 ; 0x630
				dc.b  $0D,$00,$00,$00,$00,$00,$0D,$00 ; 0x638
				dc.b  $00,$00,$00,$00,$00,$00,$6C,$FF ; 0x640
				dc.b  $00,$FF,$00,$00,$02,$01,$00,$78 ; 0x648
				dc.b  $00,$C0,$17,$00,$24,$00,$00,$C0 ; 0x650
				dc.b  $00,$60,$01,$00,$00,$BF,$00,$CA ; 0x658
				dc.b  $00,$00,$45,$56,$00,$FC,$0E,$10 ; 0x660
				dc.b  $00,$78,$20,$03,$30,$01,$00,$3D ; 0x668
				dc.b  $00,$80,$01,$FF,$00,$FF,$00,$C4 ; 0x670
				dc.b  $02,$F5,$00,$C6,$00,$80,$01,$0A ; 0x678
				dc.b  $00,$A2,$00,$C0,$01,$09,$00,$76 ; 0x680
				dc.b  $00,$20,$0A,$91,$00,$00,$0B,$09 ; 0x688
				dc.b  $25,$00,$03,$00,$AC,$54,$00,$B8 ; 0x690
				dc.b  $08,$04,$03,$08,$00,$04,$19,$FF ; 0x698
				dc.b  $24,$00,$00,$BF,$00,$B8,$02,$BD ; 0x6a0
				dc.b  $00,$F0,$56,$0A,$00,$20,$5E,$09 ; 0x6a8
				dc.b  $00,$08,$02,$09,$00,$08,$18,$00 ; 0x6b0
				dc.b  $00,$A9,$00,$25,$00,$71,$0D,$DB ; 0x6b8
				dc.b  $00,$A0,$00,$83,$1D,$76,$00,$C8 ; 0x6c0
				dc.b  $00,$C0,$0D,$9E,$00,$61,$0E,$8A ; 0x6c8
				dc.b  $00,$94,$00,$E0,$0D,$78,$00,$A3 ; 0x6d0
				dc.b  $00,$E3,$0D,$96,$00,$5F,$00,$F9 ; 0x6d8
				dc.b  $00,$10,$8B,$00,$25,$BE,$00,$F8 ; 0x6e0
				dc.b  $02,$B8,$00,$38,$20,$01,$01,$19 ; 0x6e8
				dc.b  $00,$5F,$00,$FF,$06,$80,$03,$80 ; 0x6f0
				dc.b  $16,$00,$00,$00,$0A,$40,$00,$00 ; 0x6f8
				dc.b  $02,$14,$00,$00,$07,$00,$16,$01 ; 0x700
				dc.b  $10,$0F,$00,$33,$21,$08,$00,$00 ; 0x708
				dc.b  $02,$03,$00,$96,$00,$80,$01,$00 ; 0x710
				dc.b  $00,$20,$00,$80,$00,$FF,$00,$FF ; 0x718
				dc.b  $00,$E7,$01,$FF,$00,$FF,$00,$26 ; 0x720
				dc.b  $00,$80,$03,$03,$01,$03,$00,$17 ; 0x728
				dc.b  $03,$91,$00,$00,$07,$0E,$01,$09 ; 0x730
				dc.b  $01,$00,$71,$00,$0B,$00,$56,$00 ; 0x738
				dc.b  $00,$00,$00,$FF,$00,$FF,$27,$FF ; 0x740
				dc.b  $7D,$00,$00,$00,$55,$00,$00,$00 ; 0x748
				dc.b  $29,$00,$00,$52,$00,$14,$00,$7C ; 0x750
				dc.b  $0D,$6A,$00,$68,$00,$B4,$0D,$0E ; 0x758
				dc.b  $00,$63,$00,$40,$02,$00,$00,$30 ; 0x760
				dc.b  $08,$00,$00,$39,$00,$8D,$0E,$4C ; 0x768
				dc.b  $00,$BC,$0E,$43,$00,$24,$00,$80 ; 0x770
				dc.b  $0D,$3A,$00,$73,$00,$34,$0D,$48 ; 0x778
				dc.b  $00,$DB,$00,$3F,$00,$C0,$6C,$FF ; 0x780
				dc.b  $00,$FF,$00,$00,$00,$00,$02,$63 ; 0x788
				dc.b  $00,$00,$17,$00,$01,$00,$00,$00 ; 0x790
				dc.b  $22,$C0,$00,$00,$02,$C0,$00,$00 ; 0x798
				dc.b  $5F,$04,$18,$03,$34,$00,$00,$01 ; 0x7a0
				dc.b  $00,$00,$00,$00,$01,$01,$00,$00 ; 0x7a8
				dc.b  $01,$00,$00,$08,$00,$F0,$00,$00 ; 0x7b0
				dc.b  $00,$00,$00,$08,$00,$F0,$04,$00 ; 0x7b8
				dc.b  $01,$00,$00,$00,$00,$00,$00,$00 ; 0x7c0
				dc.b  $00,$00,$00,$00,$00,$00,$07,$00 ; 0x7c8
				dc.b  $01,$00,$01,$00,$FF,$FF,$FF,$00 ; 0x7d0
				dc.b  $00,$00,$00,$40,$00,$00,$0D,$00 ; 0x7d8
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0x7e0
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x7e8
				dc.b  $0D,$00,$00,$00,$0E,$00,$00,$00 ; 0x7f0
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x7f8
				dc.b  $0D,$00,$00,$00,$00,$00,$00,$00 ; 0x800
				dc.b  $6C,$FF,$00,$FF,$00,$FA,$00,$DE ; 0x808
				dc.b  $01,$01,$00,$78,$00,$C0,$17,$00 ; 0x810
				dc.b  $09,$0A,$00,$40,$02,$85,$00,$00 ; 0x818
				dc.b  $00,$00,$01,$85,$00,$00,$00,$00 ; 0x820
				dc.b  $DE,$82,$00,$18,$0B,$00,$FF,$FF ; 0x828
				dc.b  $FF,$00,$00,$4C,$00,$5A,$00,$2C ; 0x830
				dc.b  $0D,$AF,$00,$DA,$00,$C4,$0D,$3A ; 0x838
				dc.b  $00,$64,$00,$40,$0D,$E9,$00,$91 ; 0x840
				dc.b  $00,$00,$0D,$37,$00,$6C,$0E,$10 ; 0x848
				dc.b  $00,$7E,$00,$80,$0D,$ED,$00,$37 ; 0x850
				dc.b  $00,$44,$0D,$27,$00,$AE,$00,$FA ; 0x858
				dc.b  $00,$C0,$6E,$01,$00,$7E,$1B,$00 ; 0x860
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x868
				dc.b  $01,$14,$00,$80,$22,$7C,$00,$E7 ; 0x870
				dc.b  $00,$80,$01,$D9,$00,$4B,$00,$80 ; 0x878
				dc.b  $15,$7F,$00,$FF,$2E,$2A,$00,$5C ; 0x880
				dc.b  $0E,$02,$00,$34,$20,$FF,$35,$02 ; 0x888
				dc.b  $00,$13,$00,$80,$01,$02,$00,$3C ; 0x890
				dc.b  $01,$FF,$00,$FA,$00,$B9,$01,$FF ; 0x898
				dc.b  $00,$F0,$00,$66,$04,$02,$01,$01 ; 0x8a0
				dc.b  $00,$99,$00,$FF,$00,$65,$00,$21 ; 0x8a8
				dc.b  $00,$82,$00,$18,$07,$03,$03,$09 ; 0x8b0
				dc.b  $25,$10,$00,$B3,$00,$E4,$00,$01 ; 0x8b8
				dc.b  $00,$04,$00,$80,$00,$00,$45,$20 ; 0x8c0
				dc.b  $0E,$04,$00,$04,$00,$20,$00,$64 ; 0x8c8
				dc.b  $7B,$FF,$25,$BE,$00,$C8,$02,$B5 ; 0x8d0
				dc.b  $00,$20,$D4,$00,$00,$9F,$00,$F9 ; 0x8d8
				dc.b  $00,$37,$0D,$CF,$00,$B2,$00,$F5 ; 0x8e0
				dc.b  $0D,$03,$00,$42,$00,$40,$02,$02 ; 0x8e8
				dc.b  $00,$10,$08,$06,$00,$70,$00,$55 ; 0x8f0
				dc.b  $00,$40,$0D,$95,$00,$C7,$0E,$83 ; 0x8f8
				dc.b  $00,$0E,$00,$20,$0D,$72,$00,$16 ; 0x900
				dc.b  $00,$95,$0D,$8E,$00,$35,$00,$41 ; 0x908
				dc.b  $00,$70,$6C,$00,$00,$01,$01,$0D ; 0x910
				dc.b  $01,$1B,$00,$63,$00,$E8,$17,$00 ; 0x918
				dc.b  $25,$41,$00,$92,$00,$80,$01,$B4 ; 0x920
				dc.b  $00,$5A,$46,$79,$0F,$0A,$00,$0C ; 0x928
				dc.b  $20,$FF,$30,$00,$00,$49,$00,$A0 ; 0x930
				dc.b  $01,$00,$00,$01,$00,$31,$00,$80 ; 0x938
				dc.b  $00,$00,$00,$02,$00,$A2,$00,$00 ; 0x940
				dc.b  $01,$09,$00,$21,$01,$FF,$00,$F4 ; 0x948
				dc.b  $00,$84,$00,$A0,$0A,$82,$00,$18 ; 0x950
				dc.b  $0B,$09,$25,$40,$03,$80,$58,$FF ; 0x958
				dc.b  $00,$FF,$52,$20,$00,$14,$27,$FF ; 0x960
				dc.b  $25,$6E,$00,$40,$02,$94,$00,$08 ; 0x968
				dc.b  $56,$00,$00,$00,$52,$E9,$0B,$06 ; 0x970
				dc.b  $00,$F4,$01,$FF,$00,$F3,$00,$F9 ; 0x978
				dc.b  $18,$00,$00,$25,$00,$D2,$00,$FB ; 0x980
				dc.b  $0D,$7D,$00,$C1,$00,$01,$1D,$CE ; 0x988
				dc.b  $00,$78,$00,$40,$0D,$13,$00,$4B ; 0x990
				dc.b  $0E,$F0,$00,$E1,$00,$A0,$0D,$D1 ; 0x998
				dc.b  $00,$B2,$00,$21,$0D,$05,$00,$61 ; 0x9a0
				dc.b  $00,$65,$00,$B0,$8B,$00,$25,$C0 ; 0x9a8
				dc.b  $00,$00,$02,$C0,$00,$00,$20,$00 ; 0x9b0
				dc.b  $01,$08,$00,$00,$00,$00,$29,$02 ; 0x9b8
				dc.b  $00,$3E,$37,$12,$00,$4A,$24,$FF ; 0x9c0
				dc.b  $00,$F8,$00,$ED,$00,$00,$01,$0C ; 0x9c8
				dc.b  $00,$45,$00,$00,$00,$00,$00,$0F ; 0x9d0
				dc.b  $00,$FF,$00,$E0,$00,$00,$00,$0F ; 0x9d8
				dc.b  $00,$FF,$00,$E0,$05,$02,$00,$69 ; 0x9e0
				dc.b  $03,$82,$00,$18,$0B,$00,$79,$20 ; 0x9e8
				dc.b  $00,$80,$00,$00,$83,$FF,$FF,$00 ; 0x9f0
				dc.b  $00,$5A,$00,$71,$00,$D4,$0D,$75 ; 0x9f8
				dc.b  $00,$49,$00,$3C,$0D,$0F,$00,$DB ; 0xa00
				dc.b  $00,$C0,$0D,$3F,$00,$6F,$0E,$54 ; 0xa08
				dc.b  $00,$94,$0E,$4A,$00,$01,$0E,$40 ; 0xa10
				dc.b  $00,$6C,$00,$BC,$0D,$50,$00,$4D ; 0xa18
				dc.b  $00,$C5,$00,$40,$6C,$00,$00,$02 ; 0xa20
				dc.b  $00,$56,$00,$F5,$02,$73,$18,$00 ; 0xa28
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0xa30
				dc.b  $09,$0D,$00,$10,$E7,$90,$00,$00 ; 0xa38
				dc.b  $0B,$00,$FF,$FF,$FF,$00,$00,$2F ; 0xa40
				dc.b  $00,$81,$00,$4E,$0D,$D7,$00,$3C ; 0xa48
				dc.b  $00,$8A,$0D,$62,$00,$54,$00,$A0 ; 0xa50
				dc.b  $0D,$89,$00,$52,$00,$80,$0D,$0C ; 0xa58
				dc.b  $00,$6E,$0E,$CA,$00,$E0,$00,$40 ; 0xa60
				dc.b  $0D,$8F,$00,$77,$00,$CA,$0D,$F1 ; 0xa68
				dc.b  $00,$ED,$00,$30,$00,$E0,$6F,$B7 ; 0xa70
				dc.b  $1B,$00,$01,$14,$00,$80,$22,$7F ; 0xa78
				dc.b  $00,$E0,$02,$BF,$00,$A0,$5F,$01 ; 0xa80
				dc.b  $00,$F7,$17,$FF,$35,$08,$00,$97 ; 0xa88
				dc.b  $02,$04,$00,$37,$00,$80,$01,$03 ; 0xa90
				dc.b  $00,$19,$00,$80,$01,$03,$00,$65 ; 0xa98
				dc.b  $04,$03,$01,$03,$00,$C5,$00,$FF ; 0xaa0
				dc.b  $00,$65,$00,$21,$00,$90,$08,$0E ; 0xaa8
				dc.b  $01,$09,$01,$00,$FF,$FF,$FF,$00 ; 0xab0
				dc.b  $00,$E4,$00,$1F,$00,$9F,$0D,$28 ; 0xab8
				dc.b  $00,$51,$00,$AD,$0D,$28,$00,$10 ; 0xac0
				dc.b  $00,$D0,$0D,$A0,$00,$43,$00,$40 ; 0xac8
				dc.b  $0D,$D5,$00,$AF,$0E,$BA,$00,$F9 ; 0xad0
				dc.b  $00,$20,$0D,$A2,$00,$C4,$00,$4D ; 0xad8
				dc.b  $0D,$CA,$00,$E2,$00,$77,$00,$F0 ; 0xae0
				dc.b  $6C,$00,$00,$02,$00,$58,$00,$5B ; 0xae8
				dc.b  $01,$1B,$00,$73,$00,$E8,$17,$00 ; 0xaf0
				dc.b  $25,$8F,$00,$B5,$00,$00,$01,$D3 ; 0xaf8
				dc.b  $00,$8A,$0F,$50,$05,$FF,$00,$9A ; 0xb00
				dc.b  $00,$B1,$00,$01,$07,$00,$0A,$0A ; 0xb08
				dc.b  $00,$87,$02,$00,$16,$6B,$00,$0C ; 0xb10
				dc.b  $06,$6B,$00,$0C,$03,$2A,$02,$00 ; 0xb18
				dc.b  $00,$02,$1E,$01,$10,$0C,$00,$77 ; 0xb20
				dc.b  $06,$21,$00,$58,$02,$18,$16,$C1 ; 0xb28
				dc.b  $03,$00,$00,$7D,$01,$FF,$00,$FF ; 0xb30
				dc.b  $00,$6E,$01,$00,$00,$02,$00,$A1 ; 0xb38
				dc.b  $01,$00,$00,$02,$00,$C9,$00,$80 ; 0xb40
				dc.b  $05,$00,$00,$01,$00,$00,$02,$90 ; 0xb48
				dc.b  $00,$00,$0B,$09,$26,$86,$00,$CC ; 0xb50
				dc.b  $00,$00,$00,$80,$00,$DF,$00,$64 ; 0xb58
				dc.b  $15,$40,$00,$80,$00,$00,$35,$20 ; 0xb60
				dc.b  $02,$01,$00,$04,$00,$80,$00,$00 ; 0xb68
				dc.b  $02,$30,$35,$01,$00,$04,$1C,$80 ; 0xb70
				dc.b  $00,$00,$14,$01,$00,$04,$00,$12 ; 0xb78
				dc.b  $00,$90,$0F,$FF,$25,$C0,$00,$48 ; 0xb80
				dc.b  $02,$BA,$00,$F0,$15,$FF,$00,$80 ; 0xb88
				dc.b  $01,$01,$39,$40,$3C,$20,$1A,$C1 ; 0xb90
				dc.b  $29,$00,$00,$2F,$00,$85,$00,$E9 ; 0xb98
				dc.b  $02,$00,$00,$C1,$09,$F7,$00,$14 ; 0xba0
				dc.b  $00,$BB,$1D,$10,$00,$16,$00,$C0 ; 0xba8
				dc.b  $0D,$6A,$00,$C9,$0E,$00,$00,$EB ; 0xbb0
				dc.b  $00,$10,$02,$07,$00,$90,$03,$78 ; 0xbb8
				dc.b  $00,$4C,$03,$03,$00,$14,$00,$57 ; 0xbc0
				dc.b  $00,$1B,$0D,$58,$00,$73,$00,$77 ; 0xbc8
				dc.b  $00,$90,$8B,$00,$25,$C0,$00,$00 ; 0xbd0
				dc.b  $00,$00,$01,$C0,$00,$00,$0D,$00 ; 0xbd8
				dc.b  $00,$00,$00,$00,$05,$00,$00,$64 ; 0xbe0
				dc.b  $00,$D0,$13,$00,$03,$00,$16,$76 ; 0xbe8
				dc.b  $00,$5C,$0E,$0E,$00,$5C,$20,$03 ; 0xbf0
				dc.b  $30,$FF,$00,$FF,$00,$C0,$03,$08 ; 0xbf8
				dc.b  $00,$00,$01,$01,$00,$08,$02,$08 ; 0xc00
				dc.b  $00,$FB,$00,$80,$00,$00,$00,$03 ; 0xc08
				dc.b  $00,$79,$00,$80,$05,$00,$00,$00 ; 0xc10
				dc.b  $00,$00,$02,$90,$00,$00,$0B,$09 ; 0xc18
				dc.b  $25,$00,$00,$FF,$00,$FF,$01,$00 ; 0xc20
				dc.b  $00,$FF,$00,$FF,$45,$20,$64,$4C ; 0xc28
				dc.b  $00,$43,$08,$00,$00,$00,$02,$00 ; 0xc30
				dc.b  $00,$00,$08,$80,$00,$00,$0F,$FF ; 0xc38
				dc.b  $25,$00,$00,$00,$02,$00,$00,$00 ; 0xc40
				dc.b  $AA,$FF,$03,$00,$00,$00,$06,$00 ; 0xc48
				dc.b  $00,$00,$01,$00,$00,$00,$00,$00 ; 0xc50
				dc.b  $18,$00,$00,$C7,$00,$47,$00,$D3 ; 0xc58
				dc.b  $0D,$02,$00,$D0,$00,$09,$0E,$4C ; 0xc60
				dc.b  $00,$40,$0D,$8B,$00,$FA,$0E,$BA ; 0xc68
				dc.b  $00,$A3,$0E,$A3,$00,$4E,$0E,$8E ; 0xc70
				dc.b  $00,$2A,$00,$29,$0D,$B1,$00,$34 ; 0xc78
				dc.b  $00,$63,$00,$30,$6C,$00,$00,$02 ; 0xc80
				dc.b  $00,$67,$00,$4F,$02,$73,$18,$00 ; 0xc88
				dc.b  $01,$00,$00,$00,$48,$08,$2D,$00 ; 0xc90
				dc.b  $00,$00,$0F,$04,$16,$00,$01,$03 ; 0xc98
				dc.b  $0E,$08,$00,$00,$24,$00,$00,$01 ; 0xca0
				dc.b  $00,$00,$02,$01,$00,$00,$02,$08 ; 0xca8
				dc.b  $00,$F0,$00,$00,$01,$08,$00,$F0 ; 0xcb0
				dc.b  $00,$00,$03,$00,$01,$00,$00,$00 ; 0xcb8
				dc.b  $00,$00,$00,$00,$00,$00,$00,$00 ; 0xcc0
				dc.b  $00,$00,$07,$00,$01,$00,$01,$00 ; 0xcc8
				dc.b  $79,$00,$00,$FF,$00,$FF,$83,$FF ; 0xcd0
				dc.b  $FF,$00,$00,$00,$00,$40,$00,$00 ; 0xcd8
				dc.b  $0D,$00,$00,$00,$00,$00,$0D,$00 ; 0xce0
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0xce8
				dc.b  $0E,$00,$00,$00,$0E,$00,$00,$00 ; 0xcf0
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0xcf8
				dc.b  $0D,$00,$00,$00,$00,$00,$00,$00 ; 0xd00
				dc.b  $6E,$07,$00,$AC,$02,$63,$18,$00 ; 0xd08
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0xd10
				dc.b  $09,$15,$00,$1A,$E7,$94,$0C,$00 ; 0xd18
				dc.b  $FF,$FF,$FF,$00,$00,$0B,$00,$80 ; 0xd20
				dc.b  $00,$5B,$0D,$A8,$00,$6B,$00,$21 ; 0xd28
				dc.b  $0D,$5C,$00,$00,$00,$10,$0D,$70 ; 0xd30
				dc.b  $00,$00,$00,$40,$0D,$EA,$00,$AB ; 0xd38
				dc.b  $0E,$AD,$00,$55,$00,$A0,$0D,$75 ; 0xd40
				dc.b  $00,$C0,$00,$41,$0D,$D1,$00,$DE ; 0xd48
				dc.b  $00,$FB,$00,$B0,$6F,$1E,$1B,$00 ; 0xd50
				dc.b  $01,$00,$00,$00,$22,$C0,$00,$00 ; 0xd58
				dc.b  $02,$C0,$00,$00,$5F,$04,$00,$00 ; 0xd60
				dc.b  $17,$03,$35,$01,$00,$00,$02,$01 ; 0xd68
				dc.b  $00,$00,$00,$00,$01,$08,$00,$F0 ; 0xd70
				dc.b  $00,$00,$01,$08,$00,$F0,$04,$00 ; 0xd78
				dc.b  $01,$00,$00,$00,$00,$00,$00,$00 ; 0xd80
				dc.b  $00,$00,$00,$00,$08,$00,$01,$00 ; 0xd88
				dc.b  $01,$00,$FF,$FF,$FF,$00,$00,$00 ; 0xd90
				dc.b  $00,$40,$00,$00,$0D,$00,$00,$00 ; 0xd98
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0xda0
				dc.b  $0D,$00,$00,$00,$00,$00,$0D,$00 ; 0xda8
				dc.b  $00,$00,$0E,$00,$00,$00,$00,$00 ; 0xdb0
				dc.b  $0D,$00,$00,$00,$00,$00,$0D,$00 ; 0xdb8
				dc.b  $00,$00,$00,$00,$00,$00,$6E,$6D ; 0xdc0
				dc.b  $00,$9A,$1B,$00,$25,$C7,$00,$62 ; 0xdc8
				dc.b  $02,$EC,$00,$FB,$16,$75,$00,$AA ; 0xdd0
				dc.b  $00,$00,$12,$12,$00,$5F,$19,$18 ; 0xdd8
				dc.b  $00,$5C,$06,$18,$00,$5C,$02,$67 ; 0xde0
				dc.b  $00,$8E,$02,$05,$00,$40,$1E,$00 ; 0xde8
				dc.b  $17,$01,$00,$5C,$00,$70,$19,$34 ; 0xdf0
				dc.b  $00,$20,$02,$02,$00,$13,$01,$00 ; 0xdf8
				dc.b  $00,$02,$00,$3C,$02,$03,$00,$03 ; 0xe00
				dc.b  $00,$80,$01,$0F,$00,$DA,$00,$00 ; 0xe08
				dc.b  $05,$01,$00,$24,$03,$94,$0C,$09 ; 0xe10
				dc.b  $80,$00,$00,$00,$00,$80,$00,$00 ; 0xe18
				dc.b  $50,$01,$00,$04,$00,$1B,$00,$2C ; 0xe20
				dc.b  $16,$0E,$00,$C8,$0F,$FF,$41,$8B ; 0xe28
				dc.b  $00,$D8,$00,$00,$3D,$05,$00,$40 ; 0xe30
				dc.b  $51,$FF,$00,$F1,$29,$00,$00,$16 ; 0xe38
				dc.b  $00,$11,$00,$E6,$0D,$C8,$00,$43 ; 0xe40
				dc.b  $00,$52,$1D,$F6,$00,$C4,$00,$80 ; 0xe48
				dc.b  $0D,$49,$00,$06,$0E,$01,$00,$98 ; 0xe50
				dc.b  $00,$60,$0D,$FA,$00,$9F,$00,$92 ; 0xe58
				dc.b  $0D,$38,$00,$65,$00,$42,$00,$60 ; 0xe60
				dc.b  $8B,$00,$25,$71,$00,$40,$02,$DC ; 0xe68
				dc.b  $00,$9B,$16,$7A,$00,$64,$13,$0F ; 0xe70
				dc.b  $00,$77,$18,$FF,$00,$90,$00,$10 ; 0xe78
				dc.b  $0A,$25,$00,$5C,$02,$0B,$00,$E8 ; 0xe80
				dc.b  $20,$FF,$30,$00,$00,$1B,$00,$40 ; 0xe88
				dc.b  $01,$FF,$00,$F5,$00,$98,$00,$A0 ; 0xe90
				dc.b  $01,$03,$00,$93,$00,$80,$01,$00 ; 0xe98
				dc.b  $00,$1A,$00,$00,$00,$FF,$00,$FF ; 0xea0
				dc.b  $00,$A5,$00,$00,$0A,$94,$0C,$09 ; 0xea8
				dc.b  $25,$10,$03,$20,$00,$AB,$00,$5C ; 0xeb0
				dc.b  $45,$40,$0B,$80,$00,$80,$00,$00 ; 0xeb8
				dc.b  $02,$8C,$00,$D0,$52,$0D,$00,$10 ; 0xec0
				dc.b  $01,$01,$00,$11,$00,$04,$01,$02 ; 0xec8
				dc.b  $00,$08,$00,$20,$0E,$FF,$00,$FF ; 0xed0
				dc.b  $0F,$FF,$25,$C0,$03,$C0,$53,$40 ; 0xed8
				dc.b  $03,$0A,$00,$C0,$52,$FB,$02,$FF ; 0xee0
				dc.b  $00,$F5,$00,$E1,$00,$80,$01,$03 ; 0xee8
				dc.b  $00,$B3,$00,$80,$1F,$00,$00,$6B ; 0xef0
				dc.b  $00,$97,$00,$28,$0D,$8B,$00,$94 ; 0xef8
				dc.b  $00,$F8,$0E,$41,$00,$90,$02,$01 ; 0xf00
				dc.b  $00,$40,$09,$4B,$00,$7E,$00,$00 ; 0xf08
				dc.b  $0D,$64,$00,$A8,$0E,$58,$00,$13 ; 0xf10
				dc.b  $00,$00,$0D,$4C,$00,$AB,$00,$F8 ; 0xf18
				dc.b  $0D,$5F,$00,$91,$00,$C2,$00,$80 ; 0xf20
				dc.b  $6E,$81,$00,$13,$1B,$00,$FF,$00 ; 0xf28
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0xf30
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$01,$10 ; 0xf38
				dc.b  $00,$00,$06,$13,$00,$DC,$E7,$9A ; 0xf40
				dc.b  $00,$08,$0B,$00,$FF,$FF,$FF,$00 ; 0xf48
				dc.b  $00,$F8,$00,$7C,$00,$36,$0D,$8F ; 0xf50
				dc.b  $00,$B0,$00,$C2,$0D,$58,$00,$A8 ; 0xf58
				dc.b  $00,$20,$0D,$62,$00,$A0,$00,$80 ; 0xf60
				dc.b  $0D,$D8,$00,$D6,$0E,$9D,$00,$BB ; 0xf68
				dc.b  $00,$40,$0D,$68,$00,$2B,$00,$02 ; 0xf70
				dc.b  $0D,$C0,$00,$F0,$00,$AF,$00,$60 ; 0xf78
				dc.b  $6E,$02,$00,$0C,$1B,$00,$01,$10 ; 0xf80
				dc.b  $32,$FF,$00,$1A,$4B,$5C,$00,$C0 ; 0xf88
				dc.b  $68,$03,$04,$65,$00,$21,$00,$9A ; 0xf90
				dc.b  $00,$08,$07,$02,$03,$00,$35,$08 ; 0xf98
				dc.b  $C9,$FF,$FF,$00,$00,$3C,$00,$36 ; 0xfa0
				dc.b  $00,$EB,$0D,$4D,$00,$F9,$00,$D1 ; 0xfa8
				dc.b  $0D,$0A,$00,$8B,$00,$10,$0D,$AA ; 0xfb0
				dc.b  $00,$46,$00,$80,$02,$03,$00,$30 ; 0xfb8
				dc.b  $03,$78,$00,$4C,$03,$03,$00,$38 ; 0xfc0
				dc.b  $00,$3B,$0E,$31,$00,$33,$00,$A0 ; 0xfc8
				dc.b  $0D,$2A,$00,$D4,$00,$F1,$0D,$35 ; 0xfd0
				dc.b  $00,$63,$00,$84,$00,$B0,$6E,$C2 ; 0xfd8
				dc.b  $00,$B3,$1B,$00,$01,$10,$00,$00 ; 0xfe0
				dc.b  $22,$BF,$00,$46,$02,$C0,$00,$18 ; 0xfe8
				dc.b  $00,$00,$0C,$FF,$00,$D7,$00,$B4 ; 0xff0
				dc.b  $05,$00,$00,$87,$00,$BC,$13,$80 ; 0xff8
				dc.b  $00,$00,$02,$80,$16,$3D,$00,$8C ; 0x1000
				dc.b  $06,$3D,$00,$8C,$02,$A4,$00,$68 ; 0x1008
				dc.b  $03,$48,$37,$6D,$00,$A0,$19,$6E ; 0x1010
				dc.b  $0B,$02,$00,$23,$00,$60,$00,$FF ; 0x1018
				dc.b  $00,$F7,$00,$35,$00,$A0,$05,$02 ; 0x1020
				dc.b  $00,$72,$00,$80,$02,$9A,$00,$08 ; 0x1028
				dc.b  $0B,$09,$25,$00,$03,$00,$52,$04 ; 0x1030
				dc.b  $01,$E1,$00,$20,$02,$70,$00,$88 ; 0x1038
				dc.b  $52,$3E,$00,$04,$09,$40,$03,$80 ; 0x1040
				dc.b  $08,$1E,$00,$10,$0F,$FF,$40,$00 ; 0x1048
				dc.b  $00,$7F,$00,$FF,$3E,$00,$00,$00 ; 0x1050
				dc.b  $52,$CC,$00,$80,$0B,$34,$01,$FF ; 0x1058
				dc.b  $00,$F5,$00,$84,$18,$00,$00,$08 ; 0x1060
				dc.b  $00,$A0,$00,$51,$0D,$AF,$00,$88 ; 0x1068
				dc.b  $00,$F3,$1D,$E9,$00,$64,$00,$C0 ; 0x1070
				dc.b  $0D,$37,$00,$31,$0E,$7A,$00,$BD ; 0x1078
				dc.b  $00,$90,$0D,$ED,$00,$0A,$00,$53 ; 0x1080
				dc.b  $0D,$27,$00,$76,$00,$F6,$00,$10 ; 0x1088
				dc.b  $8B,$00,$01,$10,$00,$00,$22,$C0 ; 0x1090
				dc.b  $00,$00,$02,$C0,$00,$00,$16,$7D ; 0x1098
				dc.b  $00,$DF,$13,$80,$00,$00,$02,$80 ; 0x10a0
				dc.b  $15,$00,$00,$0D,$00,$DC,$0A,$40 ; 0x10a8
				dc.b  $00,$00,$02,$03,$00,$C4,$52,$0C ; 0x10b0
				dc.b  $00,$20,$01,$00,$00,$01,$00,$08 ; 0x10b8
				dc.b  $00,$00,$01,$01,$00,$08,$00,$00 ; 0x10c0
				dc.b  $00,$FF,$00,$F2,$00,$0F,$00,$80 ; 0x10c8
				dc.b  $01,$FE,$00,$A9,$0B,$9A,$00,$08 ; 0x10d0
				dc.b  $0B,$09,$25,$01,$00,$80,$00,$00 ; 0x10d8
				dc.b  $01,$02,$00,$80,$00,$00,$45,$20 ; 0x10e0
				dc.b  $0B,$00,$00,$FF,$00,$FF,$02,$FF ; 0x10e8
				dc.b  $00,$FF,$52,$07,$00,$5C,$01,$00 ; 0x10f0
				dc.b  $00,$FF,$00,$FF,$01,$00,$00,$FF ; 0x10f8
				dc.b  $00,$FF,$1F,$FF,$7D,$00,$03,$00 ; 0x1100
				dc.b  $00,$00,$52,$F9,$02,$00,$00,$00 ; 0x1108
				dc.b  $00,$00,$00,$00,$01,$00,$00,$00 ; 0x1110
				dc.b  $00,$00,$1F,$00,$00,$34,$00,$06 ; 0x1118
				dc.b  $00,$5D,$0C,$08,$00,$43,$00,$53 ; 0x1120
				dc.b  $00,$87,$0C,$07,$00,$02,$00,$3E ; 0x1128
				dc.b  $00,$50,$02,$00,$00,$30,$09,$24 ; 0x1130
				dc.b  $00,$69,$00,$C0,$0D,$30,$00,$8D ; 0x1138
				dc.b  $0E,$2A,$00,$7B,$00,$60,$0D,$24 ; 0x1140
				dc.b  $00,$FB,$00,$67,$0D,$2E,$00,$18 ; 0x1148
				dc.b  $00,$DF,$00,$D0,$6E,$CA,$00,$61 ; 0x1150
				dc.b  $1B,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x1158
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x1160
				dc.b  $FF,$00,$01,$14,$00,$80,$06,$09 ; 0x1168
				dc.b  $00,$EC,$E7,$96,$00,$01,$0B,$00 ; 0x1170
				dc.b  $FF,$FF,$FF,$00,$00,$08,$00,$EB ; 0x1178
				dc.b  $00,$2E,$0D,$F1,$00,$F4,$00,$2A ; 0x1180
				dc.b  $0D,$88,$00,$8E,$00,$A0,$0D,$22 ; 0x1188
				dc.b  $00,$3A,$0F,$4E,$0E,$7D,$00,$44 ; 0x1190
				dc.b  $0E,$2A,$00,$C3,$00,$6A,$0D,$B3 ; 0x1198
				dc.b  $00,$7F,$00,$8E,$00,$E0,$0D,$8B ; 0x11a0
				dc.b  $00,$C6,$1A,$70,$00,$3C,$3B,$70 ; 0x11a8
				dc.b  $00,$3C,$02,$8B,$00,$C6,$03,$54 ; 0x11b0
				dc.b  $1B,$00,$01,$14,$00,$80,$31,$00 ; 0x11b8
				dc.b  $00,$53,$00,$F0,$6C,$01,$4E,$96 ; 0x11c0
				dc.b  $00,$01,$0B,$00,$FF,$FF,$FF,$00 ; 0x11c8
				dc.b  $00,$4C,$00,$A5,$00,$E3,$0D,$B0 ; 0x11d0
				dc.b  $00,$3D,$00,$39,$0D,$3A,$00,$71 ; 0x11d8
				dc.b  $00,$90,$0D,$D8,$00,$95,$00,$00 ; 0x11e0
				dc.b  $0D,$37,$00,$B3,$0E,$10,$00,$BC ; 0x11e8
				dc.b  $0E,$ED,$00,$6D,$00,$59,$0D,$27 ; 0x11f0
				dc.b  $00,$F2,$00,$64,$00,$30,$8B,$00 ; 0x11f8
				dc.b  $01,$14,$00,$80,$22,$C8,$00,$E5 ; 0x1200
				dc.b  $02,$B1,$00,$8D,$0E,$CE,$00,$D0 ; 0x1208
				dc.b  $06,$82,$00,$EF,$08,$06,$01,$20 ; 0x1210
				dc.b  $00,$7F,$00,$FF,$06,$13,$00,$5F ; 0x1218
				dc.b  $02,$00,$16,$50,$00,$9E,$07,$7C ; 0x1220
				dc.b  $02,$72,$00,$F0,$02,$13,$00,$00 ; 0x1228
				dc.b  $20,$A5,$16,$62,$00,$78,$19,$51 ; 0x1230
				dc.b  $00,$F0,$01,$FF,$00,$FF,$00,$6D ; 0x1238
				dc.b  $01,$FF,$00,$FF,$00,$8C,$02,$00 ; 0x1240
				dc.b  $00,$F0,$00,$20,$01,$F4,$00,$87 ; 0x1248
				dc.b  $00,$C0,$06,$2A,$00,$00,$02,$96 ; 0x1250
				dc.b  $00,$01,$0B,$09,$25,$40,$00,$CB ; 0x1258
				dc.b  $02,$80,$00,$FF,$00,$FF,$28,$02 ; 0x1260
				dc.b  $00,$04,$00,$80,$00,$00,$18,$01 ; 0x1268
				dc.b  $00,$04,$10,$89,$00,$D8,$50,$02 ; 0x1270
				dc.b  $01,$27,$00,$3B,$16,$1B,$00,$84 ; 0x1278
				dc.b  $0F,$FF,$25,$C1,$00,$E0,$02,$BD ; 0x1280
				dc.b  $00,$60,$4E,$04,$00,$70,$06,$05 ; 0x1288
				dc.b  $00,$40,$52,$F3,$00,$00,$28,$00 ; 0x1290
				dc.b  $00,$C9,$00,$39,$00,$C9,$0D,$11 ; 0x1298
				dc.b  $00,$CC,$00,$5B,$1D,$A8,$00,$FE ; 0x12a0
				dc.b  $0E,$36,$00,$A9,$0E,$06,$00,$B9 ; 0x12a8
				dc.b  $00,$10,$0D,$AF,$00,$A2,$00,$BB ; 0x12b0
				dc.b  $0C,$08,$00,$1A,$00,$05,$00,$D5 ; 0x12b8
				dc.b  $00,$90,$0B,$07,$7F,$00,$01,$14 ; 0x12c0
				dc.b  $00,$80,$26,$96,$00,$46,$00,$80 ; 0x12c8
				dc.b  $45,$1A,$00,$6A,$0E,$0A,$00,$A4 ; 0x12d0
				dc.b  $20,$A5,$31,$2B,$00,$90,$01,$FF ; 0x12d8
				dc.b  $00,$FF,$00,$F2,$01,$FF,$00,$FD ; 0x12e0
				dc.b  $00,$52,$01,$00,$00,$0C,$00,$2A ; 0x12e8
				dc.b  $00,$00,$01,$FA,$00,$3A,$06,$01 ; 0x12f0
				dc.b  $00,$F3,$00,$FF,$02,$96,$00,$01 ; 0x12f8
				dc.b  $0B,$09,$25,$00,$4A,$10,$00,$04 ; 0x1300
				dc.b  $10,$84,$00,$B0,$50,$10,$01,$3D ; 0x1308
				dc.b  $00,$50,$27,$FF,$29,$9C,$00,$30 ; 0x1310
				dc.b  $56,$03,$00,$C0,$7C,$00,$00,$44 ; 0x1318
				dc.b  $00,$75,$00,$55,$0D,$A5,$00,$96 ; 0x1320
				dc.b  $00,$EF,$1D,$E4,$00,$03,$0F,$05 ; 0x1328
				dc.b  $0E,$7A,$00,$51,$03,$01,$00,$90 ; 0x1330
				dc.b  $03,$78,$00,$4C,$03,$03,$00,$E7 ; 0x1338
				dc.b  $00,$93,$00,$CF,$0D,$20,$00,$A7 ; 0x1340
				dc.b  $00,$BF,$00,$50,$8B,$00,$FF,$00 ; 0x1348
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0x1350
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$01,$10 ; 0x1358
				dc.b  $00,$00,$06,$0A,$00,$82,$E7,$8E ; 0x1360
				dc.b  $00,$00,$0B,$00,$FF,$FF,$FF,$00 ; 0x1368
				dc.b  $00,$2B,$00,$7A,$00,$16,$0D,$1E ; 0x1370
				dc.b  $00,$E4,$00,$62,$0D,$8E,$00,$A2 ; 0x1378
				dc.b  $00,$20,$0D,$3A,$00,$88,$0E,$F8 ; 0x1380
				dc.b  $00,$B6,$0E,$99,$00,$9F,$0E,$43 ; 0x1388
				dc.b  $00,$72,$00,$A2,$0D,$D2,$00,$44 ; 0x1390
				dc.b  $00,$4D,$00,$60,$0D,$FF,$00,$FF ; 0x1398
				dc.b  $1A,$FF,$00,$FF,$3B,$FF,$00,$FF ; 0x13a0
				dc.b  $02,$FF,$00,$FF,$02,$01,$00,$28 ; 0x13a8
				dc.b  $1B,$00,$01,$10,$00,$00,$31,$01 ; 0x13b0
				dc.b  $00,$81,$BC,$8E,$00,$00,$0B,$00 ; 0x13b8
				dc.b  $FF,$FF,$FF,$00,$00,$6F,$00,$34 ; 0x13c0
				dc.b  $00,$CB,$0D,$DD,$00,$2D,$00,$71 ; 0x13c8
				dc.b  $0D,$40,$00,$85,$00,$10,$0D,$3F ; 0x13d0
				dc.b  $00,$E0,$00,$80,$0D,$58,$00,$1B ; 0x13d8
				dc.b  $0E,$2D,$00,$17,$0E,$06,$00,$1C ; 0x13e0
				dc.b  $00,$91,$0D,$46,$00,$B7,$00,$22 ; 0x13e8
				dc.b  $00,$B0,$8B,$00,$01,$10,$00,$00 ; 0x13f0
				dc.b  $22,$C0,$00,$00,$02,$C0,$00,$00 ; 0x13f8
				dc.b  $0D,$00,$00,$12,$00,$30,$06,$5B ; 0x1400
				dc.b  $00,$85,$08,$08,$01,$08,$00,$00 ; 0x1408
				dc.b  $00,$00,$06,$00,$00,$00,$19,$04 ; 0x1410
				dc.b  $00,$D6,$06,$00,$00,$00,$02,$A0 ; 0x1418
				dc.b  $00,$FC,$02,$0B,$00,$B4,$20,$FF ; 0x1420
				dc.b  $0E,$08,$00,$00,$05,$00,$00,$10 ; 0x1428
				dc.b  $00,$00,$02,$00,$16,$7E,$00,$E0 ; 0x1430
				dc.b  $01,$00,$00,$01,$00,$08,$00,$00 ; 0x1438
				dc.b  $00,$00,$00,$01,$00,$08,$02,$07 ; 0x1440
				dc.b  $00,$46,$00,$00,$01,$FB,$00,$A6 ; 0x1448
				dc.b  $00,$00,$05,$00,$00,$00,$03,$8E ; 0x1450
				dc.b  $00,$00,$0B,$09,$25,$00,$00,$FF ; 0x1458
				dc.b  $00,$FF,$01,$00,$17,$80,$12,$00 ; 0x1460
				dc.b  $00,$00,$00,$FF,$00,$FF,$18,$10 ; 0x1468
				dc.b  $08,$00,$02,$00,$00,$10,$00,$FF ; 0x1470
				dc.b  $00,$FF,$02,$FF,$00,$FF,$34,$00 ; 0x1478
				dc.b  $00,$00,$1A,$04,$01,$8E,$00,$60 ; 0x1480
				dc.b  $09,$00,$03,$00,$06,$00,$00,$00 ; 0x1488
				dc.b  $00,$FF,$00,$FF,$0F,$FF,$25,$00 ; 0x1490
				dc.b  $00,$00,$02,$00,$00,$00,$16,$62 ; 0x1498
				dc.b  $00,$8C,$36,$00,$00,$00,$06,$00 ; 0x14a0
				dc.b  $00,$00,$37,$00,$1A,$F0,$0C,$00 ; 0x14a8
				dc.b  $01,$00,$00,$00,$00,$00,$18,$00 ; 0x14b0
				dc.b  $00,$9C,$00,$54,$00,$1C,$02,$01 ; 0x14b8
				dc.b  $00,$11,$09,$CA,$00,$F5,$00,$94 ; 0x14c0
				dc.b  $0D,$01,$00,$43,$00,$20,$03,$00 ; 0x14c8
				dc.b  $09,$6D,$00,$C5,$00,$00,$0D,$92 ; 0x14d0
				dc.b  $00,$5C,$0E,$80,$00,$10,$00,$80 ; 0x14d8
				dc.b  $02,$00,$00,$E0,$03,$75,$00,$40 ; 0x14e0
				dc.b  $03,$00,$00,$6F,$00,$7C,$00,$14 ; 0x14e8
				dc.b  $0C,$00,$00,$8A,$00,$F6,$00,$79 ; 0x14f0
				dc.b  $00,$C0,$0B,$00,$01,$81,$00,$66 ; 0x14f8
				dc.b  $1A,$8B,$00,$D6,$3B,$8B,$00,$D6 ; 0x1500
				dc.b  $02,$81,$00,$66,$01,$02,$00,$D7 ; 0x1508
				dc.b  $00,$74,$02,$73,$18,$00,$01,$10 ; 0x1510
				dc.b  $00,$00,$22,$EF,$00,$FE,$00,$80 ; 0x1518
				dc.b  $01,$EF,$00,$FE,$15,$FF,$00,$B7 ; 0x1520
				dc.b  $00,$17,$13,$00,$03,$00,$16,$01 ; 0x1528
				dc.b  $00,$56,$0E,$04,$00,$04,$20,$FF ; 0x1530
				dc.b  $31,$09,$00,$A0,$01,$00,$00,$01 ; 0x1538
				dc.b  $00,$10,$01,$00,$00,$01,$00,$10 ; 0x1540
				dc.b  $01,$FF,$00,$F2,$00,$10,$02,$FE ; 0x1548
				dc.b  $00,$B0,$06,$00,$00,$00,$00,$00 ; 0x1550
				dc.b  $02,$8E,$00,$00,$0B,$09,$25,$01 ; 0x1558
				dc.b  $4A,$08,$65,$6D,$00,$F4,$27,$FF ; 0x1560
				dc.b  $29,$C0,$00,$00,$D4,$00,$00,$67 ; 0x1568
				dc.b  $00,$04,$00,$3D,$0D,$D2,$00,$87 ; 0x1570
				dc.b  $00,$27,$1D,$FC,$00,$51,$0E,$50 ; 0x1578
				dc.b  $00,$6D,$0E,$AC,$00,$F3,$00,$E0 ; 0x1580
				dc.b  $0D,$00,$00,$43,$00,$07,$0D,$3F ; 0x1588
				dc.b  $00,$6C,$00,$7D,$00,$D0,$8B,$00 ; 0x1590
				dc.b  $01,$10,$22,$01,$00,$22,$00,$85 ; 0x1598
				dc.b  $01,$01,$00,$1F,$00,$0D,$46,$7F ; 0x15a0
				dc.b  $00,$FC,$0A,$28,$03,$47,$00,$E4 ; 0x15a8
				dc.b  $20,$FF,$30,$FF,$00,$F5,$04,$10 ; 0x15b0
				dc.b  $03,$10,$02,$02,$00,$A0,$00,$20 ; 0x15b8
				dc.b  $01,$11,$00,$38,$00,$C0,$03,$02 ; 0x15c0
				dc.b  $01,$01,$00,$A7,$00,$FF,$00,$65 ; 0x15c8
				dc.b  $00,$21,$00,$8E,$08,$03,$03,$09 ; 0x15d0
				dc.b  $25,$40,$03,$80,$47,$40,$62,$40 ; 0x15d8
				dc.b  $00,$04,$00,$5E,$00,$04,$08,$02 ; 0x15e0
				dc.b  $00,$04,$02,$01,$00,$04,$19,$FF ; 0x15e8
				dc.b  $25,$C2,$00,$88,$02,$BF,$00,$10 ; 0x15f0
				dc.b  $A9,$FF,$00,$F5,$0B,$02,$00,$38 ; 0x15f8
				dc.b  $02,$0D,$00,$40,$18,$00,$00,$84 ; 0x1600
				dc.b  $00,$BB,$00,$FC,$0D,$AC,$00,$47 ; 0x1608
				dc.b  $00,$34,$0D,$01,$00,$3E,$00,$40 ; 0x1610
				dc.b  $02,$01,$00,$D0,$08,$06,$00,$5D ; 0x1618
				dc.b  $00,$2D,$0E,$7C,$00,$3C,$0E,$6C ; 0x1620
				dc.b  $00,$B4,$00,$80,$0D,$5E,$00,$A1 ; 0x1628
				dc.b  $00,$B4,$0C,$08,$00,$75,$00,$F4 ; 0x1630
				dc.b  $00,$B7,$00,$C0,$0B,$07,$01,$81 ; 0x1638
				dc.b  $00,$66,$1A,$8B,$00,$D6,$3B,$8B ; 0x1640
				dc.b  $00,$D6,$02,$81,$00,$66,$02,$ED ; 0x1648
				dc.b  $00,$94,$02,$73,$18,$00,$FF,$00 ; 0x1650
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$01,$14 ; 0x1658
				dc.b  $00,$80,$06,$17,$00,$1E,$E7,$95 ; 0x1660
				dc.b  $0C,$00,$FF,$FF,$FF,$00,$00,$27 ; 0x1668
				dc.b  $00,$77,$00,$23,$0D,$19,$00,$AC ; 0x1670
				dc.b  $00,$F9,$0D,$8D,$00,$ED,$00,$90 ; 0x1678
				dc.b  $0D,$37,$00,$B6,$00,$40,$0D,$F4 ; 0x1680
				dc.b  $00,$F3,$0E,$96,$00,$54,$00,$A0 ; 0x1688
				dc.b  $0D,$40,$00,$95,$00,$19,$0D,$CE ; 0x1690
				dc.b  $00,$B1,$00,$F8,$00,$30,$29,$80 ; 0x1698
				dc.b  $00,$C1,$3B,$80,$00,$C1,$07,$11 ; 0x16a0
				dc.b  $1B,$00,$01,$14,$00,$80,$32,$8B ; 0x16a8
				dc.b  $00,$B0,$BB,$95,$0C,$00,$FF,$FF ; 0x16b0
				dc.b  $FF,$00,$00,$6B,$00,$31,$00,$D8 ; 0x16b8
				dc.b  $0D,$D7,$00,$F6,$00,$08,$0D,$3F ; 0x16c0
				dc.b  $00,$D0,$00,$80,$0D,$33,$00,$E2 ; 0x16c8
				dc.b  $00,$F0,$0D,$54,$00,$58,$0E,$29 ; 0x16d0
				dc.b  $00,$CD,$00,$00,$0D,$03,$00,$3F ; 0x16d8
				dc.b  $00,$08,$0D,$43,$00,$24,$00,$CD ; 0x16e0
				dc.b  $00,$80,$8B,$00,$01,$14,$00,$80 ; 0x16e8
				dc.b  $22,$C2,$00,$28,$02,$BB,$00,$80 ; 0x16f0
				dc.b  $15,$FF,$00,$A8,$00,$0F,$08,$00 ; 0x16f8
				dc.b  $01,$26,$00,$CF,$00,$FF,$0A,$12 ; 0x1700
				dc.b  $00,$5F,$14,$FF,$00,$B7,$00,$00 ; 0x1708
				dc.b  $06,$4F,$00,$90,$02,$34,$00,$C0 ; 0x1710
				dc.b  $03,$5C,$1E,$01,$01,$33,$05,$8C ; 0x1718
				dc.b  $00,$4C,$02,$BB,$00,$14,$03,$0E ; 0x1720
				dc.b  $00,$43,$21,$0D,$00,$70,$03,$00 ; 0x1728
				dc.b  $03,$00,$03,$3A,$00,$80,$02,$A2 ; 0x1730
				dc.b  $0B,$95,$0C,$09,$26,$80,$00,$00 ; 0x1738
				dc.b  $02,$80,$00,$00,$44,$00,$00,$10 ; 0x1740
				dc.b  $07,$80,$03,$00,$04,$80,$00,$00 ; 0x1748
				dc.b  $52,$82,$00,$63,$27,$FF,$25,$C2 ; 0x1750
				dc.b  $00,$34,$02,$BB,$00,$8C,$15,$FF ; 0x1758
				dc.b  $00,$94,$00,$2B,$3E,$14,$7D,$00 ; 0x1760
				dc.b  $00,$98,$00,$51,$00,$29,$0D,$C5 ; 0x1768
				dc.b  $00,$BE,$00,$2B,$1D,$6A,$00,$F2 ; 0x1770
				dc.b  $00,$C0,$0D,$8E,$00,$99,$0E,$7C ; 0x1778
				dc.b  $00,$C5,$00,$E0,$0D,$6C,$00,$9E ; 0x1780
				dc.b  $00,$8B,$0D,$87,$00,$64,$00,$24 ; 0x1788
				dc.b  $00,$90,$8B,$00,$01,$14,$00,$80 ; 0x1790
				dc.b  $26,$C0,$00,$46,$15,$00,$00,$7D ; 0x1798
				dc.b  $00,$DF,$08,$03,$01,$34,$00,$97 ; 0x17a0
				dc.b  $00,$FF,$0A,$80,$15,$FF,$00,$93 ; 0x17a8
				dc.b  $00,$30,$0F,$00,$1E,$01,$01,$03 ; 0x17b0
				dc.b  $31,$0D,$00,$30,$1B,$95,$0C,$09 ; 0x17b8
				dc.b  $70,$00,$00,$20,$10,$80,$00,$00 ; 0x17c0
				dc.b  $52,$83,$00,$14,$27,$FF,$81,$04 ; 0x17c8
				dc.b  $00,$00,$7C,$00,$00,$63,$00,$01 ; 0x17d0
				dc.b  $00,$4A,$0D,$CD,$00,$4F,$00,$BE ; 0x17d8
				dc.b  $1D,$F9,$00,$7F,$00,$80,$0D,$4C ; 0x17e0
				dc.b  $00,$AA,$0E,$A7,$00,$13,$00,$30 ; 0x17e8
				dc.b  $0D,$FD,$00,$65,$00,$7E,$0D,$3B ; 0x17f0
				dc.b  $00,$DA,$00,$28,$00,$A0,$8B,$00 ; 0x17f8
				dc.b  $01,$14,$00,$80,$25,$00,$00,$BF ; 0x1800
				dc.b  $00,$9E,$00,$80,$1F,$06,$01,$11 ; 0x1808
				dc.b  $00,$4B,$00,$FF,$06,$00,$03,$00 ; 0x1810
				dc.b  $26,$2E,$00,$A8,$1E,$01,$01,$03 ; 0x1818
				dc.b  $0E,$0E,$00,$74,$26,$00,$03,$00 ; 0x1820
				dc.b  $02,$08,$00,$09,$00,$80,$01,$07 ; 0x1828
				dc.b  $00,$D5,$00,$A0,$06,$AB,$03,$95 ; 0x1830
				dc.b  $0C,$09,$82,$80,$00,$00,$52,$30 ; 0x1838
				dc.b  $00,$18,$27,$FF,$81,$14,$5F,$07 ; 0x1840
				dc.b  $00,$1C,$00,$80,$01,$02,$00,$E8 ; 0x1848
				dc.b  $18,$00,$00,$80,$00,$B9,$00,$09 ; 0x1850
				dc.b  $0D,$A7,$00,$0F,$00,$CB,$1D,$5A ; 0x1858
				dc.b  $00,$5A,$00,$C0,$0D,$78,$00,$79 ; 0x1860
				dc.b  $0E,$69,$00,$69,$00,$E0,$0D,$5B ; 0x1868
				dc.b  $00,$C4,$00,$2B,$0D,$72,$00,$62 ; 0x1870
				dc.b  $00,$62,$00,$90,$8B,$00,$FF,$00 ; 0x1878
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$01,$14 ; 0x1880
				dc.b  $00,$80,$01,$01,$00,$48,$00,$38 ; 0x1888
				dc.b  $01,$01,$00,$11,$00,$D2,$0F,$FC ; 0x1890
				dc.b  $03,$78,$02,$6D,$00,$77,$00,$FF ; 0x1898
				dc.b  $5D,$00,$00,$65,$00,$40,$0B,$01 ; 0x18a0
				dc.b  $60,$65,$00,$21,$00,$90,$08,$04 ; 0x18a8
				dc.b  $01,$01,$01,$00,$1A,$80,$00,$00 ; 0x18b0
				dc.b  $02,$80,$00,$00,$DF,$FF,$19,$EF ; 0x18b8
				dc.b  $00,$28,$02,$B9,$00,$DF,$E0,$00 ; 0x18c0
				dc.b  $00,$B7,$00,$DD,$00,$0D,$0D,$3B ; 0x18c8
				dc.b  $00,$A8,$00,$97,$0D,$4D,$00,$4B ; 0x18d0
				dc.b  $00,$70,$0D,$35,$00,$2D,$00,$C0 ; 0x18d8
				dc.b  $0D,$9C,$00,$3D,$0E,$68,$00,$B5 ; 0x18e0
				dc.b  $00,$60,$0D,$3A,$00,$02,$00,$77 ; 0x18e8
				dc.b  $0D,$87,$00,$67,$00,$AA,$00,$D0 ; 0x18f0
				dc.b  $0D,$F1,$00,$E3,$1A,$FF,$00,$FF ; 0x18f8
				dc.b  $3B,$FF,$00,$FF,$02,$F1,$00,$E3 ; 0x1900
				dc.b  $02,$02,$00,$14,$01,$1B,$00,$F3 ; 0x1908
				dc.b  $00,$F0,$17,$00,$01,$14,$00,$80 ; 0x1910
				dc.b  $22,$D3,$00,$20,$02,$88,$00,$92 ; 0x1918
				dc.b  $09,$01,$00,$15,$00,$37,$00,$FF ; 0x1920
				dc.b  $1E,$88,$00,$7F,$02,$88,$00,$7F ; 0x1928
				dc.b  $15,$66,$00,$9A,$02,$05,$01,$20 ; 0x1930
				dc.b  $04,$FF,$00,$FF,$00,$20,$24,$FF ; 0x1938
				dc.b  $14,$FF,$00,$FF,$00,$FC,$01,$FF ; 0x1940
				dc.b  $00,$FF,$00,$FB,$00,$60,$21,$07 ; 0x1948
				dc.b  $00,$02,$00,$C0,$01,$01,$00,$17 ; 0x1950
				dc.b  $00,$40,$03,$03,$01,$02,$00,$2F ; 0x1958
				dc.b  $00,$FF,$00,$65,$00,$21,$00,$90 ; 0x1960
				dc.b  $08,$0C,$01,$05,$01,$00,$25,$40 ; 0x1968
				dc.b  $00,$52,$00,$A0,$01,$80,$00,$AB ; 0x1970
				dc.b  $00,$5C,$09,$40,$00,$40,$00,$FC ; 0x1978
				dc.b  $39,$10,$00,$D0,$00,$7C,$01,$40 ; 0x1980
				dc.b  $6B,$10,$03,$20,$19,$FF,$25,$C0 ; 0x1988
				dc.b  $03,$C0,$0B,$C4,$00,$FF,$00,$FF ; 0x1990
				dc.b  $3E,$14,$69,$FF,$00,$FF,$00,$23 ; 0x1998
				dc.b  $01,$FF,$00,$FF,$00,$C1,$00,$80 ; 0x19a0
				dc.b  $17,$00,$00,$DC,$00,$33,$00,$51 ; 0x19a8
				dc.b  $0D,$1E,$00,$04,$00,$23,$0D,$26 ; 0x19b0
				dc.b  $00,$AC,$00,$30,$0D,$9A,$00,$B0 ; 0x19b8
				dc.b  $00,$C0,$0D,$CE,$00,$41,$0E,$B4 ; 0x19c0
				dc.b  $00,$78,$00,$E0,$0D,$9D,$00,$1B ; 0x19c8
				dc.b  $00,$83,$0D,$C3,$00,$D4,$00,$97 ; 0x19d0
				dc.b  $00,$10,$6E,$50,$00,$0F,$1B,$00 ; 0x19d8
				dc.b  $01,$14,$00,$80,$21,$01,$00,$15 ; 0x19e0
				dc.b  $00,$68,$01,$01,$00,$1F,$00,$FD ; 0x19e8
				dc.b  $16,$48,$00,$48,$2D,$FF,$00,$39 ; 0x19f0
				dc.b  $00,$1E,$00,$01,$0D,$00,$21,$FF ; 0x19f8
				dc.b  $31,$21,$00,$C0,$03,$08,$03,$08 ; 0x1a00
				dc.b  $0C,$02,$04,$65,$00,$21,$00,$90 ; 0x1a08
				dc.b  $08,$03,$03,$09,$25,$40,$03,$80 ; 0x1a10
				dc.b  $47,$20,$00,$80,$00,$00,$0C,$40 ; 0x1a18
				dc.b  $00,$04,$00,$59,$00,$EF,$7B,$FF ; 0x1a20
				dc.b  $25,$C0,$03,$C0,$46,$FF,$02,$01 ; 0x1a28
				dc.b  $8B,$00,$00,$E0,$00,$57,$00,$17 ; 0x1a30
				dc.b  $03,$31,$09,$05,$00,$C7,$00,$F5 ; 0x1a38
				dc.b  $0D,$03,$00,$39,$00,$B0,$02,$02 ; 0x1a40
				dc.b  $00,$10,$08,$06,$00,$8D,$00,$95 ; 0x1a48
				dc.b  $00,$40,$0D,$BC,$00,$C7,$0E,$A5 ; 0x1a50
				dc.b  $00,$2E,$00,$20,$0D,$8F,$00,$CB ; 0x1a58
				dc.b  $00,$95,$0C,$08,$00,$B3,$00,$3C ; 0x1a60
				dc.b  $00,$B1,$00,$70,$0B,$07,$62,$59 ; 0x1a68
				dc.b  $00,$2C,$1B,$00,$01,$14,$00,$80 ; 0x1a70
				dc.b  $22,$B6,$00,$70,$02,$BD,$00,$C0 ; 0x1a78
				dc.b  $0E,$06,$00,$60,$05,$FF,$00,$FD ; 0x1a80
				dc.b  $00,$83,$13,$00,$03,$13,$00,$DB ; 0x1a88
				dc.b  $16,$24,$0A,$0F,$00,$F0,$02,$0C ; 0x1a90
				dc.b  $00,$A0,$20,$FF,$30,$FF,$00,$F9 ; 0x1a98
				dc.b  $00,$E0,$02,$04,$00,$68,$02,$03 ; 0x1aa0
				dc.b  $00,$AE,$01,$FF,$00,$FF,$00,$62 ; 0x1aa8
				dc.b  $00,$80,$00,$FF,$00,$FF,$00,$E8 ; 0x1ab0
				dc.b  $00,$80,$03,$02,$04,$65,$00,$21 ; 0x1ab8
				dc.b  $00,$90,$08,$03,$03,$09,$25,$10 ; 0x1ac0
				dc.b  $00,$00,$00,$00,$01,$20,$00,$00 ; 0x1ac8
				dc.b  $00,$00,$0E,$80,$00,$00,$04,$04 ; 0x1ad0
				dc.b  $00,$04,$00,$80,$00,$00,$2C,$10 ; 0x1ad8
				dc.b  $00,$04,$00,$80,$00,$00,$09,$10 ; 0x1ae0
				dc.b  $00,$80,$00,$00,$54,$02,$00,$04 ; 0x1ae8
				dc.b  $00,$5C,$00,$44,$27,$FF,$25,$B6 ; 0x1af0
				dc.b  $00,$70,$02,$BD,$00,$C0,$0E,$06 ; 0x1af8
				dc.b  $00,$60,$05,$FF,$00,$FD,$00,$20 ; 0x1b00
				dc.b  $3A,$4E,$00,$80,$55,$FF,$00,$F5 ; 0x1b08
				dc.b  $29,$00,$00,$93,$00,$54,$00,$AC ; 0x1b10
				dc.b  $0D,$BF,$00,$42,$00,$44,$0E,$42 ; 0x1b18
				dc.b  $00,$A0,$02,$01,$00,$D0,$08,$06 ; 0x1b20
				dc.b  $00,$67,$00,$71,$0E,$AC,$00,$BA ; 0x1b28
				dc.b  $00,$20,$02,$03,$00,$60,$09,$C1 ; 0x1b30
				dc.b  $00,$32,$00,$40,$02,$03,$00,$90 ; 0x1b38
				dc.b  $09,$69,$00,$0E,$00,$C4,$0D,$82 ; 0x1b40
				dc.b  $00,$F3,$00,$A2,$00,$C0,$0D,$6C ; 0x1b48
				dc.b  $00,$98,$1A,$D7,$00,$A1,$3B,$D7 ; 0x1b50
				dc.b  $00,$A1,$02,$6C,$00,$98,$02,$18 ; 0x1b58
				dc.b  $00,$C0,$02,$A3,$18,$00,$01,$14 ; 0x1b60
				dc.b  $00,$80,$26,$77,$00,$9A,$20,$03 ; 0x1b68
				dc.b  $01,$40,$00,$3F,$00,$FF,$02,$08 ; 0x1b70
				dc.b  $03,$6C,$00,$87,$02,$42,$00,$36 ; 0x1b78
				dc.b  $02,$54,$00,$FE,$02,$70,$00,$A7 ; 0x1b80
				dc.b  $02,$6C,$00,$87,$02,$42,$00,$36 ; 0x1b88
				dc.b  $0D,$1F,$00,$20,$0B,$47,$00,$73 ; 0x1b90
				dc.b  $02,$46,$00,$4B,$0D,$04,$00,$FF ; 0x1b98
				dc.b  $00,$FF,$05,$01,$10,$10,$00,$1F ; 0x1ba0
				dc.b  $3C,$65,$00,$21,$00,$90,$04,$01 ; 0x1ba8
				dc.b  $03,$01,$01,$02,$01,$00,$55,$40 ; 0x1bb0
				dc.b  $03,$80,$03,$01,$03,$02,$03,$40 ; 0x1bb8
				dc.b  $03,$80,$95,$FF,$56,$80,$03,$80 ; 0x1bc0
				dc.b  $03,$80,$03,$80,$03,$80,$03,$80 ; 0x1bc8
				dc.b  $94,$00,$00,$A0,$00,$9E,$00,$82 ; 0x1bd0
				dc.b  $0D,$D0,$00,$89,$00,$E6,$0D,$1C ; 0x1bd8
				dc.b  $00,$32,$00,$60,$0D,$70,$00,$C9 ; 0x1be0
				dc.b  $00,$80,$0D,$96,$00,$62,$0E,$83 ; 0x1be8
				dc.b  $00,$95,$00,$C0,$0D,$72,$00,$8C ; 0x1bf0
				dc.b  $00,$A6,$0D,$8E,$00,$C8,$00,$6C ; 0x1bf8
				dc.b  $00,$20,$0D,$81,$00,$9D,$1B,$37 ; 0x1c00
				dc.b  $3C,$37,$02,$81,$00,$9D,$02,$0C ; 0x1c08
				dc.b  $00,$4A,$02,$A3,$18,$00,$01,$14 ; 0x1c10
				dc.b  $00,$80,$36,$1D,$00,$54,$06,$14 ; 0x1c18
				dc.b  $00,$C2,$13,$12,$00,$8B,$02,$25 ; 0x1c20
				dc.b  $00,$9F,$02,$36,$00,$6B,$06,$83 ; 0x1c28
				dc.b  $00,$DB,$19,$00,$38,$00,$00,$20 ; 0x1c30
				dc.b  $00,$FF,$00,$FF,$00,$E9,$00,$A0 ; 0x1c38
				dc.b  $20,$FF,$00,$FA,$00,$6A,$00,$C0 ; 0x1c40
				dc.b  $00,$FF,$00,$FB,$00,$FA,$00,$C0 ; 0x1c48
				dc.b  $08,$65,$00,$21,$00,$90,$08,$0D ; 0x1c50
				dc.b  $01,$06,$01,$00,$39,$01,$07,$02 ; 0x1c58
				dc.b  $12,$01,$00,$04,$8B,$10,$03,$20 ; 0x1c60
				dc.b  $19,$FF,$56,$14,$00,$B7,$88,$FF ; 0x1c68
				dc.b  $00,$F8,$00,$82,$01,$FF,$00,$F4 ; 0x1c70
				dc.b  $00,$94,$18,$00,$00,$63,$00,$B7 ; 0x1c78
				dc.b  $00,$A6,$06,$19,$00,$6F,$00,$0C ; 0x1c80
				dc.b  $03,$05,$00,$81,$00,$58,$00,$12 ; 0x1c88
				dc.b  $06,$19,$00,$6F,$00,$0C,$03,$05 ; 0x1c90
				dc.b  $00,$04,$00,$BE,$00,$60,$02,$02 ; 0x1c98
				dc.b  $00,$70,$08,$06,$00,$45,$00,$F4 ; 0x1ca0
				dc.b  $00,$80,$0D,$B6,$00,$75,$00,$D0 ; 0x1ca8
				dc.b  $02,$04,$00,$D0,$09,$B3,$00,$4E ; 0x1cb0
				dc.b  $00,$90,$02,$05,$00,$20,$09,$47 ; 0x1cb8
				dc.b  $00,$0C,$00,$52,$0D,$58,$00,$8F ; 0x1cc0
				dc.b  $00,$46,$00,$60,$0D,$6D,$00,$28 ; 0x1cc8
				dc.b  $1A,$90,$00,$0C,$3B,$90,$00,$0C ; 0x1cd0
				dc.b  $02,$6D,$00,$28,$03,$55,$1B,$00 ; 0x1cd8
				dc.b  $01,$10,$00,$00,$06,$0E,$00,$E4 ; 0x1ce0
				dc.b  $02,$84,$00,$FF,$00,$FF,$01,$84 ; 0x1ce8
				dc.b  $00,$FF,$00,$FF,$DE,$80,$0C,$00 ; 0x1cf0
				dc.b  $FF,$FF,$FF,$00,$00,$C9,$00,$E0 ; 0x1cf8
				dc.b  $00,$31,$0D,$53,$00,$14,$00,$C3 ; 0x1d00
				dc.b  $0D,$50,$00,$76,$00,$30,$0D,$41 ; 0x1d08
				dc.b  $00,$D8,$0E,$AD,$00,$21,$0E,$77 ; 0x1d10
				dc.b  $00,$7C,$00,$E0,$0D,$46,$00,$E0 ; 0x1d18
				dc.b  $00,$23,$0D,$97,$00,$71,$00,$25 ; 0x1d20
				dc.b  $00,$10,$0D,$18,$00,$A7,$1A,$55 ; 0x1d28
				dc.b  $00,$73,$3B,$55,$00,$73,$02,$18 ; 0x1d30
				dc.b  $00,$A7,$02,$01,$00,$7F,$1B,$00 ; 0x1d38
				dc.b  $01,$10,$00,$00,$22,$BB,$00,$4A ; 0x1d40
				dc.b  $02,$91,$00,$60,$00,$80,$08,$00 ; 0x1d48
				dc.b  $00,$B1,$00,$3F,$3A,$51,$00,$6E ; 0x1d50
				dc.b  $01,$FF,$00,$FE,$00,$DC,$00,$C0 ; 0x1d58
				dc.b  $2B,$A5,$3D,$05,$00,$62,$02,$07 ; 0x1d60
				dc.b  $00,$A1,$0B,$80,$0C,$00,$FF,$FF ; 0x1d68
				dc.b  $FF,$00,$00,$EE,$00,$36,$00,$75 ; 0x1d70
				dc.b  $0D,$35,$00,$70,$00,$4F,$0D,$29 ; 0x1d78
				dc.b  $00,$D6,$00,$F0,$0D,$A7,$00,$5B ; 0x1d80
				dc.b  $0E,$DF,$00,$25,$0E,$C3,$00,$40 ; 0x1d88
				dc.b  $00,$60,$0D,$A9,$00,$F9,$00,$2F ; 0x1d90
				dc.b  $0D,$D3,$00,$DE,$00,$11,$00,$50 ; 0x1d98
				dc.b  $8B,$00,$01,$10,$00,$00,$21,$00 ; 0x1da0
				dc.b  $00,$72,$00,$7C,$00,$80,$00,$00 ; 0x1da8
				dc.b  $00,$A0,$00,$15,$00,$80,$14,$FF ; 0x1db0
				dc.b  $00,$A1,$00,$EC,$13,$00,$03,$00 ; 0x1db8
				dc.b  $15,$FE,$00,$F0,$00,$70,$30,$A5 ; 0x1dc0
				dc.b  $31,$08,$00,$00,$03,$20,$03,$20 ; 0x1dc8
				dc.b  $01,$FF,$00,$FD,$00,$46,$00,$80 ; 0x1dd0
				dc.b  $00,$FF,$00,$F7,$00,$DA,$0B,$80 ; 0x1dd8
				dc.b  $0C,$09,$D4,$40,$00,$04,$00,$37 ; 0x1de0
				dc.b  $00,$40,$27,$FF,$D5,$08,$29,$00 ; 0x1de8
				dc.b  $00,$F4,$00,$76,$00,$BB,$0D,$1D ; 0x1df0
				dc.b  $00,$34,$00,$21,$1D,$9A,$00,$40 ; 0x1df8
				dc.b  $0E,$CD,$00,$AB,$0E,$B3,$00,$F5 ; 0x1e00
				dc.b  $00,$A0,$0D,$9C,$00,$A9,$00,$41 ; 0x1e08
				dc.b  $0D,$C3,$00,$46,$00,$2B,$00,$B0 ; 0x1e10
				dc.b  $8B,$00,$01,$10,$00,$00,$3E,$FF ; 0x1e18
				dc.b  $00,$04,$2E,$04,$00,$48,$0B,$80 ; 0x1e20
				dc.b  $24,$A5,$30,$00,$00,$14,$00,$C0 ; 0x1e28
				dc.b  $1B,$80,$0C,$09,$FF,$FF,$FF,$00 ; 0x1e30
				dc.b  $00,$A5,$00,$57,$00,$D0,$0D,$D6 ; 0x1e38
				dc.b  $00,$AE,$00,$70,$1D,$74,$00,$1C ; 0x1e40
				dc.b  $0E,$E5,$00,$BB,$00,$A0,$0D,$FD ; 0x1e48
				dc.b  $00,$5E,$00,$80,$0D,$75,$00,$EC ; 0x1e50
				dc.b  $00,$70,$0D,$92,$00,$FD,$00,$1D ; 0x1e58
				dc.b  $00,$00,$8B,$00,$01,$00,$00,$00 ; 0x1e60
				dc.b  $26,$C0,$00,$00,$20,$08,$01,$08 ; 0x1e68
				dc.b  $00,$00,$00,$00,$02,$00,$03,$80 ; 0x1e70
				dc.b  $00,$00,$02,$80,$00,$00,$02,$80 ; 0x1e78
				dc.b  $00,$00,$02,$80,$00,$00,$02,$80 ; 0x1e80
				dc.b  $00,$00,$02,$80,$00,$00,$0D,$00 ; 0x1e88
				dc.b  $00,$00,$0B,$04,$00,$00,$02,$04 ; 0x1e90
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x1e98
				dc.b  $05,$00,$10,$08,$00,$00,$3C,$00 ; 0x1ea0
				dc.b  $00,$00,$00,$00,$04,$00,$03,$00 ; 0x1ea8
				dc.b  $01,$00,$01,$00,$55,$00,$03,$00 ; 0x1eb0
				dc.b  $03,$00,$03,$00,$03,$00,$03,$00 ; 0x1eb8
				dc.b  $95,$FF,$56,$00,$03,$00,$03,$00 ; 0x1ec0
				dc.b  $03,$00,$03,$00,$03,$00,$94,$00 ; 0x1ec8
				dc.b  $00,$00,$00,$40,$00,$00,$0D,$00 ; 0x1ed0
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0x1ed8
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x1ee0
				dc.b  $0D,$00,$00,$00,$0E,$00,$00,$00 ; 0x1ee8
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x1ef0
				dc.b  $0D,$00,$00,$00,$00,$00,$00,$00 ; 0x1ef8
				dc.b  $0D,$F1,$00,$E3,$1A,$FF,$00,$FF ; 0x1f00
				dc.b  $3B,$FF,$00,$FF,$02,$F1,$00,$E3 ; 0x1f08
				dc.b  $02,$05,$00,$AF,$02,$B3,$18,$00 ; 0x1f10
				dc.b  $01,$10,$00,$00,$36,$33,$00,$CA ; 0x1f18
				dc.b  $06,$29,$00,$FB,$11,$FF,$00,$FF ; 0x1f20
				dc.b  $00,$E1,$00,$31,$89,$F0,$00,$A2 ; 0x1f28
				dc.b  $00,$00,$01,$EE,$00,$E6,$00,$00 ; 0x1f30
				dc.b  $0A,$80,$0C,$00,$FF,$FF,$FF,$00 ; 0x1f38
				dc.b  $00,$75,$00,$BA,$00,$CA,$07,$6E ; 0x1f40
				dc.b  $00,$F4,$04,$98,$00,$C4,$00,$3E ; 0x1f48
				dc.b  $07,$6E,$00,$F4,$14,$52,$00,$9F ; 0x1f50
				dc.b  $0E,$07,$00,$BF,$00,$10,$0D,$09 ; 0x1f58
				dc.b  $00,$DF,$00,$10,$0D,$53,$00,$E9 ; 0x1f60
				dc.b  $00,$FE,$0D,$68,$00,$98,$00,$C0 ; 0x1f68
				dc.b  $00,$A0,$8B,$00,$0A,$12,$0F,$F0 ; 0x1f70
				dc.b  $D8,$02,$0B,$00,$FF,$FF,$FF,$00 ; 0x1f78
				dc.b  $00,$0E,$00,$BC,$00,$F4,$0D,$AC ; 0x1f80
				dc.b  $00,$A0,$00,$9C,$0D,$5C,$00,$91 ; 0x1f88
				dc.b  $00,$C0,$0D,$72,$00,$47,$00,$00 ; 0x1f90
				dc.b  $0D,$ED,$00,$B4,$0E,$AF,$00,$FD ; 0x1f98
				dc.b  $00,$80,$0D,$78,$00,$10,$00,$1C ; 0x1fa0
				dc.b  $0D,$D4,$00,$C0,$00,$B7,$00,$40 ; 0x1fa8
				dc.b  $0D,$80,$00,$7E,$1A,$00,$00,$00 ; 0x1fb0
				dc.b  $3B,$00,$00,$00,$02,$80,$00,$7E ; 0x1fb8
				dc.b  $02,$02,$00,$3B,$1B,$00,$25,$D9 ; 0x1fc0
				dc.b  $00,$F8,$02,$F9,$00,$0A,$09,$01 ; 0x1fc8
				dc.b  $00,$95,$00,$0F,$39,$FF,$00,$9B ; 0x1fd0
				dc.b  $00,$04,$01,$00,$00,$06,$00,$C8 ; 0x1fd8
				dc.b  $2C,$FF,$3C,$FF,$00,$F7,$00,$63 ; 0x1fe0
				dc.b  $00,$00,$00,$FF,$00,$F7,$00,$E1 ; 0x1fe8
				dc.b  $00,$80,$0B,$02,$0B,$00,$FF,$FF ; 0x1ff0
				dc.b  $34,$01,$00,$28,$00,$1F,$C8,$00 ; 0x1ff8
				dc.b  $00,$33,$00,$13,$00,$38,$0D,$8E ; 0x2000
				dc.b  $00,$FC,$00,$28,$0D,$35,$00,$F2 ; 0x2008
				dc.b  $00,$80,$0D,$D7,$00,$CA,$00,$00 ; 0x2010
				dc.b  $0D,$1F,$00,$B8,$0E,$FB,$00,$C1 ; 0x2018
				dc.b  $00,$00,$0D,$DB,$00,$29,$00,$28 ; 0x2020
				dc.b  $0D,$11,$00,$2D,$00,$A3,$00,$80 ; 0x2028
				dc.b  $8B,$00,$25,$C0,$00,$5B,$02,$60 ; 0x2030
				dc.b  $00,$01,$45,$FF,$00,$0E,$00,$9E ; 0x2038
				dc.b  $30,$FF,$4F,$02,$0B,$09,$FF,$FF ; 0x2040
				dc.b  $FF,$00,$00,$41,$00,$65,$00,$DE ; 0x2048
				dc.b  $0D,$76,$00,$BF,$00,$FA,$1D,$CA ; 0x2050
				dc.b  $00,$AE,$00,$80,$0D,$0E,$00,$3E ; 0x2058
				dc.b  $0E,$EC,$00,$76,$00,$40,$0D,$CD ; 0x2060
				dc.b  $00,$D9,$00,$3A,$0D,$00,$00,$95 ; 0x2068
				dc.b  $00,$BD,$00,$E0,$8B,$00,$01,$00 ; 0x2070
				dc.b  $23,$C0,$00,$00,$02,$C0,$00,$00 ; 0x2078
				dc.b  $0E,$00,$00,$00,$05,$00,$00,$00 ; 0x2080
				dc.b  $00,$00,$13,$80,$03,$80,$00,$00 ; 0x2088
				dc.b  $15,$00,$00,$00,$0A,$40,$00,$00 ; 0x2090
				dc.b  $02,$14,$00,$00,$20,$03,$31,$08 ; 0x2098
				dc.b  $00,$00,$02,$01,$00,$00,$02,$01 ; 0x20a0
				dc.b  $00,$00,$01,$00,$00,$08,$00,$F0 ; 0x20a8
				dc.b  $00,$00,$00,$00,$00,$08,$00,$F0 ; 0x20b0
				dc.b  $00,$00,$03,$00,$04,$00,$00,$00 ; 0x20b8
				dc.b  $00,$00,$08,$00,$03,$00,$25,$00 ; 0x20c0
				dc.b  $00,$FF,$00,$FF,$01,$00,$00,$FF ; 0x20c8
				dc.b  $00,$FF,$0E,$FF,$00,$FF,$04,$00 ; 0x20d0
				dc.b  $00,$00,$00,$FF,$00,$FF,$2C,$00 ; 0x20d8
				dc.b  $00,$00,$00,$FF,$00,$FF,$09,$00 ; 0x20e0
				dc.b  $00,$FF,$00,$FF,$54,$00,$00,$00 ; 0x20e8
				dc.b  $00,$FF,$00,$FF,$27,$FF,$25,$00 ; 0x20f0
				dc.b  $00,$00,$02,$00,$00,$00,$0E,$00 ; 0x20f8
				dc.b  $00,$00,$05,$00,$00,$00,$00,$00 ; 0x2100
				dc.b  $3A,$00,$00,$00,$55,$00,$00,$00 ; 0x2108
				dc.b  $29,$00,$00,$00,$00,$40,$00,$00 ; 0x2110
				dc.b  $0D,$00,$00,$00,$00,$00,$0E,$00 ; 0x2118
				dc.b  $00,$00,$02,$00,$00,$30,$08,$00 ; 0x2120
				dc.b  $00,$00,$00,$00,$0E,$00,$00,$00 ; 0x2128
				dc.b  $00,$00,$02,$01,$00,$00,$09,$00 ; 0x2130
				dc.b  $00,$00,$00,$00,$02,$00,$00,$E0 ; 0x2138
				dc.b  $09,$00,$00,$00,$00,$00,$0D,$00 ; 0x2140
				dc.b  $00,$00,$00,$00,$0E,$18,$00,$A7 ; 0x2148
				dc.b  $1A,$55,$00,$73,$3B,$55,$00,$73 ; 0x2150
				dc.b  $02,$18,$00,$A7,$02,$01,$00,$05 ; 0x2158
				dc.b  $02,$B3,$18,$00,$FF,$00,$FF,$FF ; 0x2160
				dc.b  $FF,$00,$FF,$00,$38,$FF,$00,$DD ; 0x2168
				dc.b  $00,$D0,$06,$26,$00,$80,$13,$DD ; 0x2170
				dc.b  $00,$4F,$02,$20,$00,$3F,$0A,$90 ; 0x2178
				dc.b  $00,$4A,$79,$F8,$00,$0A,$02,$ED ; 0x2180
				dc.b  $00,$52,$0C,$02,$0B,$00,$64,$04 ; 0x2188
				dc.b  $00,$04,$99,$FF,$66,$95,$00,$EF ; 0x2190
				dc.b  $97,$00,$00,$BA,$00,$97,$00,$8D ; 0x2198
				dc.b  $0D,$F2,$00,$50,$00,$17,$1D,$83 ; 0x21a0
				dc.b  $00,$0D,$00,$C0,$0D,$3E,$00,$82 ; 0x21a8
				dc.b  $00,$80,$0D,$54,$00,$D0,$00,$70 ; 0x21b0
				dc.b  $0D,$85,$00,$19,$00,$F7,$0D,$A5 ; 0x21b8
				dc.b  $00,$E8,$00,$52,$00,$D0,$8B,$00 ; 0x21c0
				dc.b  $09,$0B,$00,$00,$0F,$FC,$D7,$9C ; 0x21c8
				dc.b  $00,$16,$0B,$00,$FF,$FF,$FF,$00 ; 0x21d0
				dc.b  $00,$32,$00,$F1,$00,$17,$0D,$DB ; 0x21d8
				dc.b  $00,$B4,$00,$95,$0D,$62,$00,$EF ; 0x21e0
				dc.b  $00,$50,$0D,$8B,$00,$BD,$00,$40 ; 0x21e8
				dc.b  $0D,$0F,$00,$A7,$0E,$CD,$00,$B2 ; 0x21f0
				dc.b  $00,$20,$0D,$91,$00,$EC,$00,$35 ; 0x21f8
				dc.b  $0D,$F4,$00,$FC,$00,$7F,$00,$70 ; 0x2200
				dc.b  $0D,$A1,$00,$18,$1A,$53,$00,$A7 ; 0x2208
				dc.b  $3B,$53,$00,$A7,$02,$A1,$00,$18 ; 0x2210
				dc.b  $02,$00,$00,$EA,$1B,$00,$25,$C3 ; 0x2218
				dc.b  $00,$CF,$02,$E1,$00,$00,$00,$00 ; 0x2220
				dc.b  $09,$38,$00,$17,$16,$1C,$00,$F3 ; 0x2228
				dc.b  $00,$FF,$21,$A6,$00,$6A,$02,$01 ; 0x2230
				dc.b  $00,$10,$00,$00,$29,$01,$01,$03 ; 0x2238
				dc.b  $3D,$F8,$00,$43,$01,$00,$00,$01 ; 0x2240
				dc.b  $00,$B3,$00,$40,$0A,$9C,$00,$16 ; 0x2248
				dc.b  $0B,$00,$FF,$FF,$FF,$00,$00,$57 ; 0x2250
				dc.b  $00,$47,$00,$5B,$0D,$BE,$00,$10 ; 0x2258
				dc.b  $00,$21,$0D,$3C,$00,$50,$00,$10 ; 0x2260
				dc.b  $0D,$F1,$00,$40,$00,$40,$0D,$41 ; 0x2268
				dc.b  $00,$AB,$0E,$19,$00,$75,$00,$A0 ; 0x2270
				dc.b  $0D,$F5,$00,$05,$00,$41,$0D,$31 ; 0x2278
				dc.b  $00,$69,$00,$6B,$00,$B0,$8B,$00 ; 0x2280
				dc.b  $25,$D8,$00,$CF,$00,$00,$01,$9E ; 0x2288
				dc.b  $00,$BC,$15,$00,$00,$7F,$00,$FF ; 0x2290
				dc.b  $08,$02,$01,$3A,$00,$83,$00,$FF ; 0x2298
				dc.b  $21,$36,$00,$EE,$0E,$01,$00,$9A ; 0x22a0
				dc.b  $1E,$01,$01,$03,$0E,$0E,$00,$3F ; 0x22a8
				dc.b  $21,$28,$00,$A0,$03,$28,$03,$28 ; 0x22b0
				dc.b  $0E,$01,$00,$31,$00,$FF,$02,$9C ; 0x22b8
				dc.b  $00,$16,$0B,$09,$80,$04,$01,$37 ; 0x22c0
				dc.b  $51,$04,$01,$47,$00,$28,$27,$FF ; 0x22c8
				dc.b  $FF,$00,$00,$69,$00,$D8,$00,$61 ; 0x22d0
				dc.b  $0D,$A5,$00,$D3,$00,$F3,$1D,$E4 ; 0x22d8
				dc.b  $00,$24,$00,$C0,$0D,$30,$00,$31 ; 0x22e0
				dc.b  $0E,$0A,$00,$2A,$00,$E0,$0D,$E7 ; 0x22e8
				dc.b  $00,$B5,$00,$53,$0D,$20,$00,$D1 ; 0x22f0
				dc.b  $00,$86,$00,$10,$8B,$00,$FF,$00 ; 0x22f8
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0x2300
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$38,$00 ; 0x2308
				dc.b  $00,$07,$00,$1E,$05,$FF,$00,$FB ; 0x2310
				dc.b  $00,$B8,$13,$D9,$00,$29,$02,$27 ; 0x2318
				dc.b  $00,$C7,$0A,$8E,$00,$87,$79,$F3 ; 0x2320
				dc.b  $00,$8F,$03,$21,$0B,$9C,$00,$16 ; 0x2328
				dc.b  $0B,$00,$FF,$FF,$FF,$00,$00,$DE ; 0x2330
				dc.b  $00,$CB,$00,$B0,$0D,$21,$00,$64 ; 0x2338
				dc.b  $00,$10,$1D,$9C,$00,$84,$00,$00 ; 0x2340
				dc.b  $0D,$E1,$00,$E3,$00,$F0,$0D,$02 ; 0x2348
				dc.b  $00,$CD,$00,$D0,$0D,$9E,$00,$F6 ; 0x2350
				dc.b  $00,$10,$0D,$C6,$00,$24,$00,$1B ; 0x2358
				dc.b  $00,$00,$8B,$00,$09,$0C,$00,$B6 ; 0x2360
				dc.b  $02,$85,$00,$00,$00,$00,$01,$85 ; 0x2368
				dc.b  $00,$00,$00,$00,$DE,$8D,$00,$1E ; 0x2370
				dc.b  $0B,$00,$FF,$FF,$FF,$00,$00,$68 ; 0x2378
				dc.b  $00,$0C,$00,$B4,$0D,$20,$00,$C3 ; 0x2380
				dc.b  $00,$DC,$0D,$6C,$00,$45,$00,$C0 ; 0x2388
				dc.b  $0D,$B1,$00,$17,$00,$00,$0D,$41 ; 0x2390
				dc.b  $00,$74,$0E,$F9,$00,$45,$00,$80 ; 0x2398
				dc.b  $0D,$B7,$00,$DB,$00,$5C,$0D,$24 ; 0x23a0
				dc.b  $00,$45,$00,$33,$00,$40,$0D,$75 ; 0x23a8
				dc.b  $00,$8D,$1A,$77,$00,$80,$3B,$77 ; 0x23b0
				dc.b  $00,$80,$02,$75,$00,$8D,$02,$01 ; 0x23b8
				dc.b  $00,$8D,$1B,$00,$25,$CF,$00,$8B ; 0x23c0
				dc.b  $00,$80,$01,$C0,$0A,$00,$00,$00 ; 0x23c8
				dc.b  $00,$00,$00,$00,$13,$05,$01,$31 ; 0x23d0
				dc.b  $00,$37,$07,$80,$00,$00,$02,$80 ; 0x23d8
				dc.b  $00,$00,$14,$00,$00,$00,$00,$00 ; 0x23e0
				dc.b  $02,$00,$00,$04,$01,$FF,$00,$A2 ; 0x23e8
				dc.b  $00,$40,$01,$00,$00,$40,$00,$00 ; 0x23f0
				dc.b  $0B,$02,$00,$8B,$2C,$00,$00,$00 ; 0x23f8
				dc.b  $00,$10,$01,$00,$00,$02,$00,$00 ; 0x2400
				dc.b  $00,$00,$19,$09,$00,$D0,$02,$03 ; 0x2408
				dc.b  $00,$A0,$01,$00,$00,$08,$00,$18 ; 0x2410
				dc.b  $02,$00,$00,$8A,$00,$00,$05,$06 ; 0x2418
				dc.b  $00,$7F,$03,$8D,$00,$1E,$07,$0E ; 0x2420
				dc.b  $01,$09,$01,$00,$24,$01,$00,$04 ; 0x2428
				dc.b  $00,$3E,$00,$20,$01,$00,$00,$FF ; 0x2430
				dc.b  $00,$FF,$09,$00,$00,$FF,$00,$FF ; 0x2438
				dc.b  $39,$00,$00,$FF,$00,$FF,$01,$00 ; 0x2440
				dc.b  $03,$20,$10,$00,$00,$00,$55,$00 ; 0x2448
				dc.b  $03,$00,$19,$FF,$29,$00,$0A,$00 ; 0x2450
				dc.b  $00,$00,$00,$00,$00,$00,$3E,$00 ; 0x2458
				dc.b  $69,$00,$00,$00,$00,$00,$01,$00 ; 0x2460
				dc.b  $00,$00,$00,$00,$00,$00,$17,$00 ; 0x2468
				dc.b  $00,$45,$00,$45,$00,$49,$0D,$59 ; 0x2470
				dc.b  $00,$C0,$00,$8B,$0D,$03,$01,$B0 ; 0x2478
				dc.b  $02,$02,$00,$10,$08,$06,$00,$30 ; 0x2480
				dc.b  $00,$8A,$00,$C0,$0D,$40,$00,$B9 ; 0x2488
				dc.b  $0E,$38,$00,$A1,$00,$E0,$0D,$31 ; 0x2490
				dc.b  $00,$4C,$00,$EB,$0D,$3D,$00,$73 ; 0x2498
				dc.b  $00,$A6,$00,$90,$0D,$7C,$00,$69 ; 0x24a0
				dc.b  $1A,$A3,$00,$FD,$3B,$A3,$00,$FD ; 0x24a8
				dc.b  $02,$7C,$00,$69,$02,$02,$00,$B3 ; 0x24b0
				dc.b  $02,$A3,$18,$00,$25,$B8,$00,$29 ; 0x24b8
				dc.b  $02,$B9,$00,$A0,$00,$00,$0D,$18 ; 0x24c0
				dc.b  $00,$E8,$06,$43,$00,$40,$08,$04 ; 0x24c8
				dc.b  $01,$23,$00,$1B,$07,$80,$03,$80 ; 0x24d0
				dc.b  $15,$FE,$00,$C1,$00,$00,$0E,$00 ; 0x24d8
				dc.b  $00,$00,$2F,$10,$00,$E5,$21,$08 ; 0x24e0
				dc.b  $00,$00,$03,$08,$03,$08,$01,$00 ; 0x24e8
				dc.b  $00,$08,$00,$60,$00,$C0,$00,$00 ; 0x24f0
				dc.b  $00,$08,$00,$80,$06,$00,$00,$00 ; 0x24f8
				dc.b  $00,$00,$02,$8D,$00,$1E,$0B,$09 ; 0x2500
				dc.b  $80,$40,$01,$35,$00,$B7,$50,$00 ; 0x2508
				dc.b  $00,$00,$00,$FF,$00,$FF,$09,$40 ; 0x2510
				dc.b  $03,$80,$19,$FF,$D5,$00,$0B,$09 ; 0x2518
				dc.b  $00,$08,$02,$09,$00,$08,$18,$00 ; 0x2520
				dc.b  $00,$A5,$00,$2D,$00,$9E,$0D,$EA ; 0x2528
				dc.b  $00,$E3,$00,$3A,$1D,$09,$00,$7E ; 0x2530
				dc.b  $00,$80,$0D,$61,$00,$FE,$0E,$35 ; 0x2538
				dc.b  $00,$BE,$00,$40,$0D,$0D,$00,$A4 ; 0x2540
				dc.b  $00,$7A,$0D,$50,$00,$1A,$00,$39 ; 0x2548
				dc.b  $00,$E0,$8B,$00,$FF,$00,$FF,$FF ; 0x2550
				dc.b  $FF,$00,$81,$7C,$00,$69,$1A,$A3 ; 0x2558
				dc.b  $00,$FD,$3B,$A3,$00,$FD,$02,$7C ; 0x2560
				dc.b  $00,$69,$02,$02,$00,$08,$02,$A3 ; 0x2568
				dc.b  $18,$00,$01,$10,$27,$51,$00,$EB ; 0x2570
				dc.b  $00,$80,$1F,$06,$01,$24,$00,$47 ; 0x2578
				dc.b  $00,$FF,$02,$08,$03,$A5,$00,$F3 ; 0x2580
				dc.b  $02,$4D,$00,$0C,$02,$44,$00,$94 ; 0x2588
				dc.b  $02,$4D,$00,$D5,$02,$A5,$00,$F3 ; 0x2590
				dc.b  $02,$4D,$00,$0C,$06,$98,$06,$14 ; 0x2598
				dc.b  $00,$30,$0B,$65,$00,$EA,$02,$46 ; 0x25a0
				dc.b  $00,$4B,$0D,$04,$00,$FF,$00,$FF ; 0x25a8
				dc.b  $05,$01,$10,$10,$00,$1F,$3C,$65 ; 0x25b0
				dc.b  $00,$21,$00,$8D,$00,$1E,$03,$01 ; 0x25b8
				dc.b  $03,$01,$01,$02,$01,$00,$28,$01 ; 0x25c0
				dc.b  $00,$04,$2B,$40,$03,$80,$03,$01 ; 0x25c8
				dc.b  $03,$02,$03,$40,$03,$80,$95,$FF ; 0x25d0
				dc.b  $29,$77,$00,$88,$2B,$80,$03,$80 ; 0x25d8
				dc.b  $03,$80,$03,$80,$03,$80,$03,$80 ; 0x25e0
				dc.b  $0E,$1F,$00,$20,$84,$00,$00,$50 ; 0x25e8
				dc.b  $00,$CE,$00,$29,$0D,$B5,$00,$A5 ; 0x25f0
				dc.b  $00,$2B,$0D,$33,$00,$E8,$00,$90 ; 0x25f8
				dc.b  $02,$01,$00,$E0,$08,$06,$00,$EC ; 0x2600
				dc.b  $00,$B2,$00,$C0,$0D,$3B,$00,$99 ; 0x2608
				dc.b  $0E,$1B,$00,$B4,$00,$70,$02,$05 ; 0x2610
				dc.b  $00,$D0,$03,$78,$00,$4C,$03,$03 ; 0x2618
				dc.b  $00,$F0,$00,$65,$00,$8B,$0D,$2B ; 0x2620
				dc.b  $00,$A5,$00,$F4,$00,$90,$0D,$81 ; 0x2628
				dc.b  $00,$9D,$1A,$80,$00,$37,$3B,$00 ; 0x2630
				dc.b  $00,$37,$02,$81,$00,$9D,$02,$0C ; 0x2638
				dc.b  $00,$4A,$02,$A3,$18,$00,$34,$FF ; 0x2640
				dc.b  $00,$53,$03,$00,$00,$00,$05,$00 ; 0x2648
				dc.b  $00,$00,$00,$00,$11,$00,$00,$00 ; 0x2650
				dc.b  $00,$80,$00,$00,$02,$80,$00,$00 ; 0x2658
				dc.b  $02,$80,$00,$00,$06,$80,$00,$00 ; 0x2660
				dc.b  $19,$41,$00,$B8,$37,$10,$00,$00 ; 0x2668
				dc.b  $00,$00,$00,$02,$00,$00,$00,$00 ; 0x2670
				dc.b  $20,$00,$00,$08,$00,$F0,$01,$00 ; 0x2678
				dc.b  $00,$08,$00,$F0,$04,$03,$06,$8D ; 0x2680
				dc.b  $00,$1E,$07,$02,$01,$00,$01,$00 ; 0x2688
				dc.b  $35,$20,$03,$00,$07,$00,$12,$00 ; 0x2690
				dc.b  $00,$00,$0E,$00,$00,$00,$7B,$00 ; 0x2698
				dc.b  $03,$00,$19,$FF,$56,$00,$00,$00 ; 0x26a0
				dc.b  $0E,$00,$00,$00,$78,$00,$00,$00 ; 0x26a8
				dc.b  $00,$00,$01,$00,$00,$00,$00,$00 ; 0x26b0
				dc.b  $18,$00,$00,$75,$00,$2E,$00,$06 ; 0x26b8
				dc.b  $06,$1E,$00,$75,$00,$40,$03,$00 ; 0x26c0
				dc.b  $00,$98,$00,$0D,$00,$32,$06,$1E ; 0x26c8
				dc.b  $00,$75,$00,$40,$03,$00,$00,$14 ; 0x26d0
				dc.b  $00,$8F,$00,$20,$02,$00,$00,$30 ; 0x26d8
				dc.b  $08,$00,$00,$52,$00,$3C,$00,$80 ; 0x26e0
				dc.b  $0D,$6D,$00,$A6,$00,$00,$02,$01 ; 0x26e8
				dc.b  $00,$00,$09,$5F,$00,$F1,$00,$40 ; 0x26f0
				dc.b  $02,$00,$00,$E0,$09,$53,$00,$85 ; 0x26f8
				dc.b  $00,$72,$0D,$68,$00,$1B,$00,$6C ; 0x2700
				dc.b  $00,$60,$0D,$BF,$00,$68,$1A,$8E ; 0x2708
				dc.b  $00,$02,$3B,$8E,$00,$02,$02,$BF ; 0x2710
				dc.b  $00,$68,$02,$20,$00,$6E,$02,$A3 ; 0x2718
				dc.b  $18,$00,$01,$14,$00,$80,$06,$0B ; 0x2720
				dc.b  $00,$F0,$E7,$84,$00,$00,$0B,$00 ; 0x2728
				dc.b  $FF,$FF,$FF,$00,$00,$EB,$00,$73 ; 0x2730
				dc.b  $00,$6C,$0D,$CB,$00,$A2,$00,$84 ; 0x2738
				dc.b  $0D,$83,$00,$60,$00,$40,$0D,$0D ; 0x2740
				dc.b  $00,$81,$0E,$BC,$00,$AC,$0E,$65 ; 0x2748
				dc.b  $00,$16,$0E,$15,$00,$B7,$00,$04 ; 0x2750
				dc.b  $0D,$99,$00,$43,$00,$0E,$00,$C0 ; 0x2758
				dc.b  $0D,$69,$00,$6F,$1A,$69,$00,$ED ; 0x2760
				dc.b  $23,$18,$00,$80,$00,$03,$00,$80 ; 0x2768
				dc.b  $00,$CE,$00,$80,$00,$13,$11,$69 ; 0x2770
				dc.b  $00,$ED,$02,$69,$00,$6F,$03,$BC ; 0x2778
				dc.b  $1B,$00,$01,$14,$00,$80,$22,$A7 ; 0x2780
				dc.b  $00,$28,$00,$00,$01,$C6,$00,$18 ; 0x2788
				dc.b  $09,$01,$00,$AC,$00,$7F,$00,$FF ; 0x2790
				dc.b  $01,$02,$00,$3C,$06,$0F,$00,$64 ; 0x2798
				dc.b  $08,$08,$01,$08,$00,$00,$00,$00 ; 0x27a0
				dc.b  $25,$05,$00,$6B,$00,$C0,$00,$00 ; 0x27a8
				dc.b  $00,$00,$00,$00,$06,$18,$00,$C4 ; 0x27b0
				dc.b  $06,$01,$00,$16,$00,$02,$01,$18 ; 0x27b8
				dc.b  $00,$40,$12,$00,$2B,$09,$00,$01 ; 0x27c0
				dc.b  $00,$20,$09,$01,$00,$00,$02,$01 ; 0x27c8
				dc.b  $00,$00,$02,$0E,$00,$77,$00,$C0 ; 0x27d0
				dc.b  $01,$10,$00,$F7,$00,$C0,$05,$03 ; 0x27d8
				dc.b  $00,$2F,$03,$84,$00,$00,$07,$05 ; 0x27e0
				dc.b  $01,$00,$01,$02,$24,$00,$00,$00 ; 0x27e8
				dc.b  $00,$FF,$00,$FF,$12,$80,$00,$00 ; 0x27f0
				dc.b  $06,$80,$00,$00,$31,$10,$03,$00 ; 0x27f8
				dc.b  $0E,$04,$00,$04,$43,$20,$00,$DF ; 0x2800
				dc.b  $00,$EC,$11,$10,$03,$20,$19,$FF ; 0x2808
				dc.b  $25,$00,$13,$02,$00,$10,$06,$0C ; 0x2810
				dc.b  $00,$C0,$31,$FF,$00,$FF,$00,$FC ; 0x2818
				dc.b  $13,$96,$00,$03,$41,$02,$00,$10 ; 0x2820
				dc.b  $12,$09,$00,$08,$02,$09,$00,$08 ; 0x2828
				dc.b  $18,$00,$00,$8E,$00,$2A,$00,$54 ; 0x2830
				dc.b  $0D,$B8,$00,$8A,$00,$BC,$0D,$0E ; 0x2838
				dc.b  $00,$2C,$00,$00,$02,$01,$00,$E0 ; 0x2840
				dc.b  $09,$63,$00,$CF,$00,$00,$0D,$85 ; 0x2848
				dc.b  $00,$14,$08,$76,$00,$44,$03,$01 ; 0x2850
				dc.b  $00,$69,$00,$FF,$00,$80,$02,$03 ; 0x2858
				dc.b  $00,$00,$03,$76,$00,$44,$03,$01 ; 0x2860
				dc.b  $00,$65,$00,$5E,$00,$3C,$0D,$7E ; 0x2868
				dc.b  $00,$5A,$00,$4D,$00,$40,$0D,$75 ; 0x2870
				dc.b  $00,$8D,$1A,$77,$00,$80,$3B,$77 ; 0x2878
				dc.b  $00,$80,$02,$75,$00,$8D,$02,$03 ; 0x2880
				dc.b  $00,$6A,$02,$B3,$18,$00,$01,$14 ; 0x2888
				dc.b  $00,$80,$22,$AF,$00,$12,$00,$80 ; 0x2890
				dc.b  $01,$AF,$00,$71,$0F,$F0,$05,$FF ; 0x2898
				dc.b  $00,$B4,$00,$24,$13,$00,$03,$00 ; 0x28a0
				dc.b  $16,$E8,$00,$A0,$2E,$00,$01,$C3 ; 0x28a8
				dc.b  $31,$9C,$00,$C0,$0A,$0B,$00,$05 ; 0x28b0
				dc.b  $00,$E0,$01,$02,$00,$44,$00,$C0 ; 0x28b8
				dc.b  $0A,$84,$00,$00,$0B,$09,$D6,$80 ; 0x28c0
				dc.b  $00,$00,$27,$FF,$D5,$08,$0B,$0C ; 0x28c8
				dc.b  $00,$6F,$02,$03,$00,$A6,$18,$00 ; 0x28d0
				dc.b  $00,$37,$00,$FB,$00,$56,$0D,$95 ; 0x28d8
				dc.b  $00,$C1,$00,$E2,$1D,$65,$00,$E8 ; 0x28e0
				dc.b  $0E,$DD,$00,$36,$0E,$A1,$00,$8F ; 0x28e8
				dc.b  $0E,$6B,$00,$80,$00,$22,$0D,$C5 ; 0x28f0
				dc.b  $00,$18,$00,$15,$00,$60,$8B,$00 ; 0x28f8
				dc.b  $01,$14,$00,$80,$22,$C6,$00,$3D ; 0x2900
				dc.b  $00,$80,$01,$C1,$00,$05,$16,$38 ; 0x2908
				dc.b  $00,$C1,$13,$55,$00,$DF,$19,$45 ; 0x2910
				dc.b  $00,$9C,$0E,$0D,$00,$A0,$52,$1C ; 0x2918
				dc.b  $00,$B0,$0A,$06,$00,$B7,$00,$C0 ; 0x2920
				dc.b  $00,$FF,$00,$FC,$00,$8B,$00,$E0 ; 0x2928
				dc.b  $03,$02,$04,$65,$00,$21,$00,$84 ; 0x2930
				dc.b  $08,$03,$03,$09,$25,$01,$03,$02 ; 0x2938
				dc.b  $00,$D8,$00,$9C,$0E,$80,$00,$00 ; 0x2940
				dc.b  $04,$01,$00,$04,$00,$DB,$00,$F8 ; 0x2948
				dc.b  $2D,$20,$10,$80,$00,$00,$50,$01 ; 0x2950
				dc.b  $00,$04,$00,$11,$00,$58,$09,$08 ; 0x2958
				dc.b  $03,$10,$00,$80,$00,$00,$17,$FF ; 0x2960
				dc.b  $25,$BE,$00,$50,$02,$9A,$00,$98 ; 0x2968
				dc.b  $16,$0D,$00,$60,$3E,$0D,$00,$A8 ; 0x2970
				dc.b  $51,$FF,$00,$E6,$0C,$50,$01,$FF ; 0x2978
				dc.b  $00,$FB,$00,$B4,$18,$00,$00,$F2 ; 0x2980
				dc.b  $00,$03,$00,$87,$02,$02,$00,$B1 ; 0x2988
				dc.b  $09,$18,$00,$14,$00,$85,$02,$03 ; 0x2990
				dc.b  $00,$13,$0A,$44,$00,$F0,$02,$01 ; 0x2998
				dc.b  $00,$A0,$08,$06,$00,$4E,$00,$59 ; 0x29a0
				dc.b  $00,$40,$0D,$68,$00,$77,$0E,$5B ; 0x29a8
				dc.b  $00,$68,$00,$20,$0D,$4F,$00,$92 ; 0x29b0
				dc.b  $00,$A5,$0D,$63,$00,$2F,$00,$7C ; 0x29b8
				dc.b  $00,$70,$0D,$95,$00,$30,$1A,$71 ; 0x29c0
				dc.b  $00,$F8,$3B,$71,$00,$F8,$02,$95 ; 0x29c8
				dc.b  $00,$30,$02,$20,$00,$07,$02,$B3 ; 0x29d0
				dc.b  $18,$00,$01,$00,$27,$C0,$00,$00 ; 0x29d8
				dc.b  $00,$00,$1F,$08,$01,$08,$00,$00 ; 0x29e0
				dc.b  $00,$00,$02,$00,$03,$80,$00,$00 ; 0x29e8
				dc.b  $02,$80,$00,$00,$02,$80,$00,$00 ; 0x29f0
				dc.b  $02,$80,$00,$00,$02,$80,$00,$00 ; 0x29f8
				dc.b  $02,$80,$00,$00,$06,$00,$06,$00 ; 0x2a00
				dc.b  $00,$00,$0B,$04,$00,$00,$02,$04 ; 0x2a08
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x2a10
				dc.b  $05,$00,$10,$08,$00,$00,$3C,$00 ; 0x2a18
				dc.b  $00,$00,$00,$00,$00,$00,$03,$00 ; 0x2a20
				dc.b  $03,$00,$01,$00,$01,$00,$28,$00 ; 0x2a28
				dc.b  $00,$00,$2B,$00,$03,$00,$03,$00 ; 0x2a30
				dc.b  $03,$00,$03,$00,$03,$00,$95,$FF ; 0x2a38
				dc.b  $29,$00,$00,$00,$2B,$00,$03,$00 ; 0x2a40
				dc.b  $03,$00,$03,$00,$03,$00,$03,$00 ; 0x2a48
				dc.b  $0E,$00,$00,$00,$84,$00,$00,$00 ; 0x2a50
				dc.b  $00,$40,$00,$00,$0D,$00,$00,$00 ; 0x2a58
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x2a60
				dc.b  $02,$00,$00,$30,$08,$00,$00,$00 ; 0x2a68
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0x2a70
				dc.b  $0E,$00,$00,$00,$00,$00,$02,$00 ; 0x2a78
				dc.b  $00,$E0,$03,$25,$00,$38,$03,$00 ; 0x2a80
				dc.b  $00,$00,$00,$00,$00,$00,$0D,$00 ; 0x2a88
				dc.b  $00,$00,$00,$00,$00,$00,$0D,$75 ; 0x2a90
				dc.b  $00,$8D,$1A,$77,$00,$80,$3B,$77 ; 0x2a98
				dc.b  $00,$80,$02,$75,$00,$8D,$02,$01 ; 0x2aa0
				dc.b  $00,$77,$02,$B3,$18,$00,$01,$14 ; 0x2aa8
				dc.b  $00,$80,$31,$FE,$00,$08,$BC,$84 ; 0x2ab0
				dc.b  $00,$00,$0B,$00,$FF,$FF,$FF,$00 ; 0x2ab8
				dc.b  $00,$F8,$00,$94,$00,$BE,$0D,$42 ; 0x2ac0
				dc.b  $00,$EB,$00,$DA,$0D,$2B,$00,$A9 ; 0x2ac8
				dc.b  $00,$A0,$0D,$AE,$00,$A6,$0E,$E8 ; 0x2ad0
				dc.b  $00,$DE,$0E,$CB,$00,$C2,$0E,$B1 ; 0x2ad8
				dc.b  $00,$61,$00,$1A,$0D,$DD,$00,$19 ; 0x2ae0
				dc.b  $00,$47,$00,$E0,$8B,$00,$0A,$4E ; 0x2ae8
				dc.b  $E7,$92,$00,$1E,$0B,$00,$FF,$FF ; 0x2af0
				dc.b  $FF,$00,$00,$4C,$00,$5F,$00,$7F ; 0x2af8
				dc.b  $0D,$49,$00,$AB,$00,$4D,$0D,$94 ; 0x2b00
				dc.b  $00,$6A,$00,$D0,$0D,$51,$00,$AB ; 0x2b08
				dc.b  $00,$40,$0D,$17,$00,$8F,$0E,$B4 ; 0x2b10
				dc.b  $00,$9D,$00,$20,$0D,$5A,$00,$F1 ; 0x2b18
				dc.b  $00,$ED,$0D,$EF,$00,$8E,$00,$35 ; 0x2b20
				dc.b  $00,$F0,$0D,$D3,$00,$26,$1A,$CF ; 0x2b28
				dc.b  $00,$56,$3B,$CF,$00,$56,$02,$D3 ; 0x2b30
				dc.b  $00,$26,$02,$02,$00,$57,$1B,$00 ; 0x2b38
				dc.b  $34,$00,$00,$E9,$00,$9F,$02,$04 ; 0x2b40
				dc.b  $00,$70,$06,$14,$00,$9E,$32,$06 ; 0x2b48
				dc.b  $00,$EB,$0A,$0C,$00,$18,$07,$13 ; 0x2b50
				dc.b  $00,$55,$01,$10,$00,$7C,$14,$FF ; 0x2b58
				dc.b  $28,$FF,$00,$FF,$00,$8C,$12,$0F ; 0x2b60
				dc.b  $00,$F7,$02,$06,$00,$28,$00,$00 ; 0x2b68
				dc.b  $05,$02,$00,$1F,$03,$92,$00,$1E ; 0x2b70
				dc.b  $0B,$02,$38,$08,$00,$04,$06,$02 ; 0x2b78
				dc.b  $00,$04,$BD,$FF,$3A,$60,$4F,$93 ; 0x2b80
				dc.b  $00,$56,$73,$00,$00,$EF,$00,$16 ; 0x2b88
				dc.b  $00,$67,$0D,$36,$00,$93,$00,$85 ; 0x2b90
				dc.b  $1D,$A7,$00,$F9,$00,$40,$0D,$DF ; 0x2b98
				dc.b  $00,$F7,$0E,$7A,$00,$A8,$0E,$AA ; 0x2ba0
				dc.b  $00,$99,$00,$25,$0D,$D4,$00,$A5 ; 0x2ba8
				dc.b  $00,$74,$00,$70,$8B,$00,$25,$F5 ; 0x2bb0
				dc.b  $00,$62,$02,$E8,$00,$09,$00,$80 ; 0x2bb8
				dc.b  $0C,$FF,$00,$FA,$00,$80,$05,$00 ; 0x2bc0
				dc.b  $00,$4F,$00,$20,$2D,$FF,$00,$40 ; 0x2bc8
				dc.b  $00,$9C,$0E,$05,$00,$54,$20,$FF ; 0x2bd0
				dc.b  $30,$FF,$00,$E9,$00,$00,$03,$40 ; 0x2bd8
				dc.b  $03,$40,$02,$05,$00,$8F,$00,$A0 ; 0x2be0
				dc.b  $01,$01,$00,$47,$00,$20,$05,$01 ; 0x2be8
				dc.b  $00,$63,$00,$FF,$02,$92,$00,$1E ; 0x2bf0
				dc.b  $0B,$09,$26,$DB,$00,$2F,$02,$AC ; 0x2bf8
				dc.b  $00,$4F,$46,$FF,$00,$FF,$0C,$00 ; 0x2c00
				dc.b  $00,$00,$00,$24,$00,$DF,$50,$40 ; 0x2c08
				dc.b  $00,$04,$00,$67,$00,$F0,$27,$FF ; 0x2c10
				dc.b  $D4,$FF,$00,$E9,$0B,$00,$00,$5D ; 0x2c18
				dc.b  $00,$80,$00,$FF,$00,$FC,$00,$52 ; 0x2c20
				dc.b  $18,$00,$00,$A4,$00,$43,$00,$C9 ; 0x2c28
				dc.b  $0D,$13,$00,$CA,$00,$AB,$1D,$AA ; 0x2c30
				dc.b  $00,$12,$00,$C0,$0D,$38,$00,$19 ; 0x2c38
				dc.b  $0E,$F1,$00,$15,$00,$E0,$0D,$B0 ; 0x2c40
				dc.b  $00,$BB,$00,$0B,$0D,$1B,$00,$63 ; 0x2c48
				dc.b  $00,$3C,$00,$90,$8B,$00,$25,$7A ; 0x2c50
				dc.b  $00,$19,$00,$00,$01,$A1,$00,$44 ; 0x2c58
				dc.b  $00,$80,$14,$FF,$00,$AE,$00,$F0 ; 0x2c60
				dc.b  $00,$01,$12,$00,$00,$00,$02,$00 ; 0x2c68
				dc.b  $15,$FF,$00,$CD,$00,$40,$0E,$07 ; 0x2c70
				dc.b  $00,$70,$20,$FF,$31,$F1,$00,$30 ; 0x2c78
				dc.b  $03,$08,$03,$08,$01,$FF,$00,$F9 ; 0x2c80
				dc.b  $00,$48,$00,$00,$01,$F3,$00,$35 ; 0x2c88
				dc.b  $06,$01,$00,$5D,$00,$FF,$02,$92 ; 0x2c90
				dc.b  $00,$1E,$0B,$09,$D6,$49,$00,$00 ; 0x2c98
				dc.b  $27,$FF,$40,$FF,$00,$80,$00,$00 ; 0x2ca0
				dc.b  $00,$01,$91,$F8,$0A,$FF,$00,$F9 ; 0x2ca8
				dc.b  $00,$F0,$02,$F0,$00,$0E,$18,$00 ; 0x2cb0
				dc.b  $00,$E6,$00,$A0,$00,$7A,$0D,$2F ; 0x2cb8
				dc.b  $00,$7C,$00,$5E,$1D,$92,$00,$83 ; 0x2cc0
				dc.b  $00,$80,$0D,$C3,$00,$5A,$0E,$AA ; 0x2cc8
				dc.b  $00,$EE,$00,$C0,$0D,$94,$00,$CD ; 0x2cd0
				dc.b  $00,$8E,$0D,$B9,$00,$7A,$00,$A3 ; 0x2cd8
				dc.b  $00,$A0,$8B,$00,$FF,$00,$FF,$FF ; 0x2ce0
				dc.b  $FF,$00,$FF,$00,$34,$01,$00,$EC ; 0x2ce8
				dc.b  $00,$70,$BB,$92,$00,$1E,$0B,$00 ; 0x2cf0
				dc.b  $FF,$FF,$FF,$00,$00,$59,$00,$80 ; 0x2cf8
				dc.b  $00,$D1,$0D,$C0,$00,$F4,$00,$A3 ; 0x2d00
				dc.b  $0D,$3C,$00,$B4,$00,$30,$0D,$F2 ; 0x2d08
				dc.b  $00,$D0,$00,$C0,$0D,$43,$00,$C1 ; 0x2d10
				dc.b  $0E,$1B,$00,$48,$00,$E0,$0D,$F6 ; 0x2d18
				dc.b  $00,$9C,$00,$03,$0D,$33,$00,$64 ; 0x2d20
				dc.b  $00,$6F,$00,$10,$8B,$00,$09,$0F ; 0x2d28
				dc.b  $00,$32,$E7,$87,$00,$18,$0B,$00 ; 0x2d30
				dc.b  $FF,$FF,$FF,$00,$00,$A5,$00,$C0 ; 0x2d38
				dc.b  $00,$4F,$0D,$BD,$00,$E4,$00,$BD ; 0x2d40
				dc.b  $0D,$A4,$00,$21,$0E,$90,$00,$87 ; 0x2d48
				dc.b  $0E,$6B,$00,$5F,$0E,$FD,$00,$F3 ; 0x2d50
				dc.b  $0E,$9A,$00,$C9,$00,$5D,$0D,$3F ; 0x2d58
				dc.b  $00,$21,$00,$E2,$0E,$00,$00,$00 ; 0x2d60
				dc.b  $1A,$34,$00,$BC,$3B,$34,$00,$BC ; 0x2d68
				dc.b  $02,$00,$00,$00,$02,$00,$00,$00 ; 0x2d70
				dc.b  $1B,$00,$36,$7F,$02,$06,$00,$1B ; 0x2d78
				dc.b  $06,$15,$00,$0F,$08,$05,$01,$29 ; 0x2d80
				dc.b  $00,$67,$00,$FF,$25,$00,$2B,$01 ; 0x2d88
				dc.b  $01,$03,$14,$FF,$00,$FF,$00,$EA ; 0x2d90
				dc.b  $00,$A0,$01,$00,$00,$0E,$0D,$00 ; 0x2d98
				dc.b  $00,$06,$00,$53,$00,$60,$11,$09 ; 0x2da0
				dc.b  $03,$0D,$00,$E7,$00,$C0,$0A,$87 ; 0x2da8
				dc.b  $00,$18,$0B,$02,$FF,$FF,$39,$04 ; 0x2db0
				dc.b  $00,$24,$C4,$00,$00,$48,$00,$77 ; 0x2db8
				dc.b  $00,$37,$0D,$AA,$00,$CC,$00,$F5 ; 0x2dc0
				dc.b  $1D,$E6,$00,$D5,$0E,$33,$00,$C7 ; 0x2dc8
				dc.b  $0E,$76,$00,$18,$0E,$EA,$00,$70 ; 0x2dd0
				dc.b  $00,$95,$0D,$24,$00,$39,$00,$21 ; 0x2dd8
				dc.b  $8C,$00,$01,$00,$00,$00,$22,$C0 ; 0x2de0
				dc.b  $00,$00,$00,$00,$01,$C0,$00,$00 ; 0x2de8
				dc.b  $00,$00,$0C,$00,$00,$00,$00,$00 ; 0x2df0
				dc.b  $06,$00,$00,$00,$08,$08,$01,$08 ; 0x2df8
				dc.b  $00,$00,$00,$00,$06,$80,$03,$80 ; 0x2e00
				dc.b  $15,$00,$00,$00,$00,$00,$00,$00 ; 0x2e08
				dc.b  $0D,$14,$00,$00,$20,$03,$0E,$08 ; 0x2e10
				dc.b  $00,$00,$20,$00,$00,$08,$04,$00 ; 0x2e18
				dc.b  $03,$00,$02,$08,$00,$F0,$00,$00 ; 0x2e20
				dc.b  $01,$08,$00,$F0,$00,$00,$03,$00 ; 0x2e28
				dc.b  $01,$00,$00,$00,$00,$00,$00,$00 ; 0x2e30
				dc.b  $00,$00,$00,$00,$00,$00,$07,$00 ; 0x2e38
				dc.b  $03,$00,$25,$00,$00,$FF,$00,$FF ; 0x2e40
				dc.b  $01,$00,$00,$FF,$00,$FF,$45,$00 ; 0x2e48
				dc.b  $10,$FF,$00,$FF,$50,$00,$00,$00 ; 0x2e50
				dc.b  $00,$FF,$00,$FF,$09,$00,$03,$00 ; 0x2e58
				dc.b  $19,$FF,$25,$00,$03,$00,$46,$00 ; 0x2e60
				dc.b  $02,$00,$60,$00,$00,$00,$0C,$00 ; 0x2e68
				dc.b  $00,$00,$00,$00,$00,$00,$00,$00 ; 0x2e70
				dc.b  $18,$00,$00,$00,$00,$40,$00,$00 ; 0x2e78
				dc.b  $03,$11,$09,$00,$00,$00,$00,$00 ; 0x2e80
				dc.b  $0D,$00,$00,$00,$00,$00,$02,$00 ; 0x2e88
				dc.b  $00,$30,$08,$00,$00,$00,$00,$00 ; 0x2e90
				dc.b  $00,$00,$0D,$00,$00,$00,$0E,$00 ; 0x2e98
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0x2ea0
				dc.b  $00,$00,$0C,$00,$00,$00,$00,$00 ; 0x2ea8
				dc.b  $00,$00,$00,$00,$0B,$00,$01,$00 ; 0x2eb0
				dc.b  $1B,$34,$00,$BC,$23,$18,$00,$80 ; 0x2eb8
				dc.b  $00,$03,$00,$80,$00,$CE,$00,$80 ; 0x2ec0
				dc.b  $00,$13,$11,$34,$00,$BC,$06,$00 ; 0x2ec8
				dc.b  $00,$53,$01,$1B,$00,$C3,$00,$F0 ; 0x2ed0
				dc.b  $17,$00,$01,$00,$00,$00,$22,$C0 ; 0x2ed8
				dc.b  $00,$00,$02,$C0,$00,$00,$00,$00 ; 0x2ee0
				dc.b  $14,$00,$00,$00,$00,$00,$00,$00 ; 0x2ee8
				dc.b  $12,$80,$03,$80,$15,$00,$00,$00 ; 0x2ef0
				dc.b  $00,$00,$0E,$14,$00,$00,$20,$03 ; 0x2ef8
				dc.b  $31,$08,$00,$00,$03,$00,$03,$00 ; 0x2f00
				dc.b  $01,$00,$00,$08,$00,$F0,$01,$00 ; 0x2f08
				dc.b  $00,$08,$00,$F0,$00,$00,$03,$00 ; 0x2f10
				dc.b  $01,$00,$00,$00,$00,$00,$00,$00 ; 0x2f18
				dc.b  $00,$00,$00,$00,$00,$00,$07,$00 ; 0x2f20
				dc.b  $03,$00,$25,$00,$03,$00,$00,$FF ; 0x2f28
				dc.b  $00,$FF,$0E,$FF,$00,$FF,$04,$00 ; 0x2f30
				dc.b  $00,$00,$00,$FF,$00,$FF,$2D,$00 ; 0x2f38
				dc.b  $10,$FF,$00,$FF,$50,$00,$00,$00 ; 0x2f40
				dc.b  $00,$FF,$00,$FF,$09,$00,$03,$00 ; 0x2f48
				dc.b  $00,$FF,$00,$FF,$17,$FF,$25,$00 ; 0x2f50
				dc.b  $00,$00,$02,$00,$00,$00,$15,$00 ; 0x2f58
				dc.b  $00,$00,$01,$00,$3D,$00,$00,$00 ; 0x2f60
				dc.b  $51,$00,$00,$00,$0A,$00,$00,$00 ; 0x2f68
				dc.b  $00,$00,$01,$00,$00,$00,$00,$00 ; 0x2f70
				dc.b  $18,$00,$00,$00,$00,$40,$00,$00 ; 0x2f78
				dc.b  $02,$01,$00,$11,$09,$00,$00,$00 ; 0x2f80
				dc.b  $00,$00,$02,$01,$00,$63,$0A,$00 ; 0x2f88
				dc.b  $00,$00,$02,$00,$00,$30,$08,$00 ; 0x2f90
				dc.b  $00,$00,$00,$00,$00,$00,$0D,$00 ; 0x2f98
				dc.b  $00,$00,$0E,$00,$00,$00,$00,$00 ; 0x2fa0
				dc.b  $0D,$00,$00,$00,$00,$00,$0D,$00 ; 0x2fa8
				dc.b  $00,$00,$00,$00,$00,$00,$0D,$00 ; 0x2fb0
				dc.b  $00,$00,$1A,$34,$00,$BC,$23,$18 ; 0x2fb8
				dc.b  $00,$80,$00,$03,$00,$80,$00,$CE ; 0x2fc0
				dc.b  $00,$80,$00,$13,$11,$34,$00,$BC ; 0x2fc8
				dc.b  $02,$00,$00,$00,$02,$00,$00,$53 ; 0x2fd0
				dc.b  $01,$1B,$00,$C3,$00,$F0,$17,$00 ; 0x2fd8
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x2fe0
				dc.b  $34,$FE,$00,$A5,$00,$00,$BB,$87 ; 0x2fe8
				dc.b  $00,$18,$0B,$00,$FF,$FF,$FF,$00 ; 0x2ff0
				dc.b  $00,$B2,$00,$E1,$00,$A1,$0D,$35 ; 0x2ff8
				dc.b  $00,$2E,$00,$13,$0D,$4C,$00,$6B ; 0x3000
				dc.b  $0E,$31,$00,$AC,$0E,$97,$00,$91 ; 0x3008
				dc.b  $0E,$64,$00,$9E,$0E,$36,$00,$73 ; 0x3010
				dc.b  $00,$73,$0D,$82,$00,$F8,$00,$1C ; 0x3018
				dc.b  $8C,$00,$01,$10,$00,$00,$06,$0E ; 0x3020
				dc.b  $00,$D2,$E7,$94,$00,$02,$0B,$00 ; 0x3028
				dc.b  $FF,$FF,$FF,$00,$00,$63,$00,$CE ; 0x3030
				dc.b  $00,$08,$0D,$68,$00,$23,$00,$98 ; 0x3038
				dc.b  $0D,$98,$00,$89,$00,$80,$0D,$62 ; 0x3040
				dc.b  $00,$26,$00,$00,$0D,$2D,$00,$88 ; 0x3048
				dc.b  $0E,$C7,$00,$D7,$00,$00,$0D,$6B ; 0x3050
				dc.b  $00,$AE,$00,$98,$0D,$04,$00,$6A ; 0x3058
				dc.b  $00,$F0,$00,$80,$0D,$83,$00,$55 ; 0x3060
				dc.b  $1A,$5F,$00,$52,$23,$1F,$00,$00 ; 0x3068
				dc.b  $00,$10,$01,$C0,$00,$00,$00,$0F ; 0x3070
				dc.b  $00,$80,$10,$5F,$00,$52,$02,$83 ; 0x3078
				dc.b  $00,$55,$02,$02,$00,$D0,$1B,$00 ; 0x3080
				dc.b  $01,$00,$00,$00,$22,$C0,$00,$00 ; 0x3088
				dc.b  $02,$C0,$00,$00,$0A,$00,$00,$00 ; 0x3090
				dc.b  $00,$00,$01,$00,$00,$00,$06,$00 ; 0x3098
				dc.b  $00,$00,$08,$08,$01,$08,$00,$00 ; 0x30a0
				dc.b  $00,$00,$26,$04,$00,$00,$09,$14 ; 0x30a8
				dc.b  $00,$00,$06,$00,$00,$04,$00,$00 ; 0x30b0
				dc.b  $01,$00,$00,$00,$12,$00,$16,$00 ; 0x30b8
				dc.b  $00,$00,$00,$10,$00,$00,$01,$02 ; 0x30c0
				dc.b  $00,$00,$0E,$02,$00,$15,$00,$55 ; 0x30c8
				dc.b  $11,$08,$00,$F0,$00,$00,$01,$08 ; 0x30d0
				dc.b  $00,$F0,$00,$00,$03,$00,$01,$00 ; 0x30d8
				dc.b  $00,$00,$00,$00,$00,$00,$00,$00 ; 0x30e0
				dc.b  $00,$00,$00,$00,$07,$00,$03,$00 ; 0x30e8
				dc.b  $38,$00,$00,$00,$00,$FF,$00,$FF ; 0x30f0
				dc.b  $04,$00,$00,$00,$00,$FF,$00,$FF ; 0x30f8
				dc.b  $31,$00,$12,$00,$00,$00,$00,$FF ; 0x3100
				dc.b  $00,$FF,$41,$00,$00,$FF,$00,$FF ; 0x3108
				dc.b  $11,$00,$03,$00,$19,$FF,$39,$00 ; 0x3110
				dc.b  $00,$00,$06,$00,$00,$00,$31,$00 ; 0x3118
				dc.b  $00,$00,$00,$00,$13,$00,$00,$00 ; 0x3120
				dc.b  $41,$00,$00,$00,$12,$00,$00,$00 ; 0x3128
				dc.b  $02,$00,$00,$00,$18,$00,$00,$00 ; 0x3130
				dc.b  $00,$40,$00,$00,$0D,$00,$00,$00 ; 0x3138
				dc.b  $00,$00,$0D,$00,$00,$00,$03,$00 ; 0x3140
				dc.b  $00,$30,$08,$00,$00,$00,$00,$00 ; 0x3148
				dc.b  $00,$00,$0D,$00,$00,$00,$08,$25 ; 0x3150
				dc.b  $00,$38,$03,$00,$00,$00,$00,$00 ; 0x3158
				dc.b  $00,$00,$02,$00,$00,$E0,$03,$25 ; 0x3160
				dc.b  $00,$38,$03,$00,$00,$00,$00,$00 ; 0x3168
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x3170
				dc.b  $00,$00,$0D,$00,$00,$00,$1A,$83 ; 0x3178
				dc.b  $00,$4A,$3B,$83,$00,$4A,$02,$00 ; 0x3180
				dc.b  $00,$00,$02,$00,$00,$6C,$1B,$00 ; 0x3188
				dc.b  $01,$10,$23,$C2,$00,$7D,$00,$80 ; 0x3190
				dc.b  $01,$A7,$00,$7D,$00,$80,$0C,$FF ; 0x3198
				dc.b  $00,$F7,$00,$84,$06,$75,$00,$E0 ; 0x31a0
				dc.b  $08,$04,$01,$23,$00,$1B,$00,$FF ; 0x31a8
				dc.b  $06,$00,$03,$00,$15,$FF,$00,$10 ; 0x31b0
				dc.b  $00,$9C,$00,$01,$09,$51,$00,$4E ; 0x31b8
				dc.b  $02,$00,$1F,$01,$10,$10,$00,$E5 ; 0x31c0
				dc.b  $26,$10,$03,$10,$02,$09,$00,$49 ; 0x31c8
				dc.b  $00,$20,$01,$07,$00,$09,$00,$20 ; 0x31d0
				dc.b  $03,$02,$01,$01,$00,$87,$00,$FF ; 0x31d8
				dc.b  $00,$65,$00,$21,$00,$94,$00,$02 ; 0x31e0
				dc.b  $07,$03,$03,$09,$25,$40,$03,$80 ; 0x31e8
				dc.b  $46,$02,$00,$04,$0A,$04,$00,$04 ; 0x31f0
				dc.b  $02,$40,$00,$04,$00,$14,$00,$33 ; 0x31f8
				dc.b  $5D,$40,$03,$80,$19,$FF,$25,$C0 ; 0x3200
				dc.b  $03,$C0,$46,$FF,$02,$01,$09,$40 ; 0x3208
				dc.b  $63,$09,$00,$14,$02,$09,$00,$14 ; 0x3210
				dc.b  $18,$00,$00,$BE,$00,$71,$00,$72 ; 0x3218
				dc.b  $03,$31,$09,$32,$00,$42,$00,$F6 ; 0x3220
				dc.b  $0D,$03,$00,$39,$00,$B0,$02,$02 ; 0x3228
				dc.b  $00,$10,$08,$06,$00,$BA,$00,$8D ; 0x3230
				dc.b  $00,$80,$0D,$4E,$00,$12,$0E,$04 ; 0x3238
				dc.b  $00,$4F,$00,$C0,$0D,$C1,$00,$77 ; 0x3240
				dc.b  $00,$B6,$0C,$08,$00,$30,$00,$3F ; 0x3248
				dc.b  $00,$F7,$00,$20,$0B,$07,$01,$80 ; 0x3250
				dc.b  $1B,$80,$00,$00,$23,$0B,$00,$00 ; 0x3258
				dc.b  $00,$01,$00,$00,$00,$C0,$00,$00 ; 0x3260
				dc.b  $00,$05,$11,$00,$00,$00,$06,$59 ; 0x3268
				dc.b  $00,$2C,$01,$00,$00,$00,$00,$00 ; 0x3270
				dc.b  $17,$00,$01,$10,$22,$01,$00,$0B ; 0x3278
				dc.b  $00,$0D,$00,$80,$01,$CC,$00,$9D ; 0x3280
				dc.b  $00,$80,$0C,$FF,$00,$BC,$00,$2C ; 0x3288
				dc.b  $06,$4F,$00,$27,$2E,$10,$00,$BE ; 0x3290
				dc.b  $0A,$51,$00,$72,$02,$05,$00,$EC ; 0x3298
				dc.b  $20,$FF,$31,$91,$00,$E0,$03,$9D ; 0x32a0
				dc.b  $00,$60,$02,$99,$00,$60,$01,$00 ; 0x32a8
				dc.b  $00,$0C,$01,$FF,$00,$FF,$00,$3B ; 0x32b0
				dc.b  $00,$E0,$03,$02,$01,$02,$00,$21 ; 0x32b8
				dc.b  $00,$FF,$00,$65,$00,$21,$00,$94 ; 0x32c0
				dc.b  $00,$02,$07,$03,$03,$09,$24,$01 ; 0x32c8
				dc.b  $00,$04,$02,$04,$00,$04,$10,$80 ; 0x32d0
				dc.b  $00,$00,$04,$01,$00,$04,$00,$DB ; 0x32d8
				dc.b  $00,$F8,$2C,$02,$00,$04,$0A,$04 ; 0x32e0
				dc.b  $00,$04,$04,$80,$00,$00,$50,$01 ; 0x32e8
				dc.b  $00,$04,$00,$38,$00,$3C,$00,$08 ; 0x32f0
				dc.b  $00,$04,$02,$10,$00,$04,$03,$08 ; 0x32f8
				dc.b  $03,$10,$00,$80,$00,$00,$17,$FF ; 0x3300
				dc.b  $25,$C1,$00,$B0,$02,$BF,$00,$88 ; 0x3308
				dc.b  $16,$25,$00,$20,$3A,$40,$03,$0D ; 0x3310
				dc.b  $00,$A8,$51,$FF,$00,$E6,$03,$01 ; 0x3318
				dc.b  $00,$10,$02,$01,$00,$10,$03,$50 ; 0x3320
				dc.b  $01,$FF,$00,$FB,$00,$B4,$18,$00 ; 0x3328
				dc.b  $00,$21,$00,$C3,$00,$A3,$02,$02 ; 0x3330
				dc.b  $00,$B1,$09,$73,$00,$08,$00,$D9 ; 0x3338
				dc.b  $02,$03,$00,$13,$0A,$44,$00,$F0 ; 0x3340
				dc.b  $02,$01,$00,$A0,$08,$06,$00,$A2 ; 0x3348
				dc.b  $00,$FE,$00,$40,$0D,$D9,$00,$53 ; 0x3350
				dc.b  $0E,$BE,$00,$28,$00,$A0,$0D,$A5 ; 0x3358
				dc.b  $00,$8A,$00,$39,$0D,$CE,$00,$57 ; 0x3360
				dc.b  $00,$5E,$00,$30,$0D,$95,$00,$30 ; 0x3368
				dc.b  $1A,$71,$00,$F8,$23,$0B,$00,$00 ; 0x3370
				dc.b  $00,$01,$00,$00,$00,$C0,$00,$00 ; 0x3378
				dc.b  $00,$05,$11,$71,$00,$F8,$02,$95 ; 0x3380
				dc.b  $00,$30,$02,$20,$00,$07,$01,$00 ; 0x3388
				dc.b  $00,$B3,$00,$00,$17,$00,$01,$10 ; 0x3390
				dc.b  $23,$3E,$00,$A4,$00,$80,$01,$79 ; 0x3398
				dc.b  $00,$03,$00,$80,$44,$FF,$00,$DE ; 0x33a0
				dc.b  $00,$40,$0A,$48,$00,$D6,$24,$FF ; 0x33a8
				dc.b  $31,$35,$00,$10,$0B,$F1,$00,$C0 ; 0x33b0
				dc.b  $01,$0D,$00,$DC,$00,$40,$03,$02 ; 0x33b8
				dc.b  $01,$01,$00,$25,$00,$FF,$00,$65 ; 0x33c0
				dc.b  $00,$21,$00,$94,$00,$02,$07,$03 ; 0x33c8
				dc.b  $03,$09,$25,$01,$00,$80,$00,$00 ; 0x33d0
				dc.b  $01,$02,$00,$80,$00,$00,$45,$20 ; 0x33d8
				dc.b  $0A,$08,$00,$04,$56,$10,$00,$04 ; 0x33e0
				dc.b  $00,$EB,$00,$20,$09,$40,$03,$80 ; 0x33e8
				dc.b  $19,$FF,$25,$5E,$00,$E0,$02,$85 ; 0x33f0
				dc.b  $00,$B0,$52,$40,$56,$FF,$00,$F6 ; 0x33f8
				dc.b  $0B,$09,$00,$08,$02,$09,$00,$08 ; 0x3400
				dc.b  $18,$00,$00,$02,$00,$9E,$00,$7A ; 0x3408
				dc.b  $02,$09,$0A,$48,$00,$8A,$00,$EE ; 0x3410
				dc.b  $02,$06,$00,$C3,$09,$01,$00,$3F ; 0x3418
				dc.b  $03,$02,$00,$10,$08,$06,$00,$4C ; 0x3420
				dc.b  $00,$3F,$00,$80,$0D,$65,$00,$AA ; 0x3428
				dc.b  $0E,$58,$00,$F4,$00,$C0,$07,$75 ; 0x3430
				dc.b  $00,$40,$04,$02,$00,$FF,$00,$4E ; 0x3438
				dc.b  $02,$08,$00,$A3,$09,$0A,$00,$64 ; 0x3440
				dc.b  $00,$E8,$00,$A0,$01,$0B,$00,$A3 ; 0x3448
				dc.b  $0A,$21,$00,$2C,$1A,$76,$3C,$76 ; 0x3450
				dc.b  $03,$21,$00,$2C,$02,$03,$00,$0D ; 0x3458
				dc.b  $03,$F8,$17,$00,$01,$00,$00,$00 ; 0x3460
				dc.b  $31,$00,$00,$00,$4B,$14,$00,$00 ; 0x3468
				dc.b  $68,$00,$04,$00,$00,$00,$00,$00 ; 0x3470
				dc.b  $00,$00,$07,$00,$03,$00,$35,$00 ; 0x3478
				dc.b  $C9,$FF,$FF,$00,$00,$00,$00,$40 ; 0x3480
				dc.b  $00,$00,$07,$25,$00,$38,$04,$00 ; 0x3488
				dc.b  $00,$00,$00,$00,$07,$25,$00,$38 ; 0x3490
				dc.b  $04,$00,$00,$00,$00,$00,$0D,$00 ; 0x3498
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0x34a0
				dc.b  $0E,$00,$00,$00,$00,$00,$0D,$00 ; 0x34a8
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0x34b0
				dc.b  $00,$00,$00,$00,$0D,$75,$00,$8D ; 0x34b8
				dc.b  $1A,$77,$00,$80,$3B,$77,$00,$80 ; 0x34c0
				dc.b  $02,$75,$00,$8D,$02,$0C,$00,$58 ; 0x34c8
				dc.b  $02,$B3,$18,$00,$01,$10,$02,$01 ; 0x34d0
				dc.b  $00,$31,$00,$D0,$01,$01,$00,$0A ; 0x34d8
				dc.b  $00,$04,$16,$4F,$00,$FB,$00,$C4 ; 0x34e0
				dc.b  $5D,$00,$00,$62,$00,$C0,$0B,$01 ; 0x34e8
				dc.b  $60,$65,$00,$21,$00,$91,$08,$04 ; 0x34f0
				dc.b  $01,$01,$01,$00,$FF,$FF,$FF,$00 ; 0x34f8
				dc.b  $00,$E1,$00,$D4,$00,$04,$0D,$58 ; 0x3500
				dc.b  $00,$E8,$00,$CC,$0D,$DB,$00,$B4 ; 0x3508
				dc.b  $00,$C0,$0D,$6E,$00,$D3,$0E,$93 ; 0x3510
				dc.b  $00,$C4,$0E,$01,$00,$4B,$00,$80 ; 0x3518
				dc.b  $0D,$7C,$00,$8E,$00,$4C,$0D,$58 ; 0x3520
				dc.b  $00,$8C,$00,$48,$00,$40,$6E,$03 ; 0x3528
				dc.b  $00,$01,$01,$1B,$00,$F3,$00,$F0 ; 0x3530
				dc.b  $17,$00,$01,$10,$32,$FF,$00,$95 ; 0x3538
				dc.b  $00,$38,$14,$00,$2D,$70,$00,$BC ; 0x3540
				dc.b  $06,$71,$00,$AC,$00,$1C,$1D,$01 ; 0x3548
				dc.b  $01,$01,$47,$03,$01,$02,$00,$87 ; 0x3550
				dc.b  $00,$FC,$00,$65,$00,$21,$00,$91 ; 0x3558
				dc.b  $08,$02,$03,$00,$35,$01,$43,$20 ; 0x3560
				dc.b  $85,$FF,$FF,$00,$00,$EF,$00,$65 ; 0x3568
				dc.b  $00,$4F,$07,$78,$00,$4C,$03,$03 ; 0x3570
				dc.b  $00,$D0,$00,$C3,$00,$BD,$0D,$84 ; 0x3578
				dc.b  $00,$11,$00,$D0,$0D,$10,$00,$47 ; 0x3580
				dc.b  $00,$40,$0D,$C0,$00,$5F,$0E,$68 ; 0x3588
				dc.b  $00,$53,$00,$20,$0D,$18,$00,$88 ; 0x3590
				dc.b  $00,$5D,$0D,$9C,$00,$C6,$00,$32 ; 0x3598
				dc.b  $00,$F0,$0D,$07,$00,$EC,$5B,$07 ; 0x35a0
				dc.b  $00,$EC,$02,$41,$00,$73,$01,$01 ; 0x35a8
				dc.b  $00,$76,$00,$40,$03,$1B,$13,$00 ; 0x35b0
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$E2,$03 ; 0x35b8
				dc.b  $00,$01,$01,$1B,$00,$F3,$00,$F0 ; 0x35c0
				dc.b  $17,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x35c8
				dc.b  $81,$07,$00,$EC,$5B,$07,$00,$EC ; 0x35d0
				dc.b  $03,$5A,$01,$01,$00,$78,$00,$98 ; 0x35d8
				dc.b  $17,$00,$01,$10,$23,$B9,$00,$D6 ; 0x35e0
				dc.b  $00,$49,$01,$B1,$00,$1E,$00,$46 ; 0x35e8
				dc.b  $1F,$0C,$01,$14,$00,$03,$00,$FF ; 0x35f0
				dc.b  $06,$63,$00,$E7,$02,$61,$00,$4D ; 0x35f8
				dc.b  $02,$5E,$00,$E7,$02,$7F,$00,$BF ; 0x3600
				dc.b  $02,$5B,$00,$9F,$02,$61,$00,$4D ; 0x3608
				dc.b  $05,$7C,$00,$5C,$13,$72,$00,$CF ; 0x3610
				dc.b  $02,$73,$00,$75,$0D,$05,$00,$2F ; 0x3618
				dc.b  $00,$FD,$05,$01,$01,$FC,$0E,$1A ; 0x3620
				dc.b  $00,$99,$00,$FF,$00,$FF,$00,$FF ; 0x3628
				dc.b  $00,$FF,$38,$65,$00,$21,$00,$91 ; 0x3630
				dc.b  $04,$08,$03,$01,$01,$02,$01,$00 ; 0x3638
				dc.b  $55,$40,$03,$80,$03,$01,$03,$02 ; 0x3640
				dc.b  $03,$40,$03,$80,$07,$08,$8D,$FF ; 0x3648
				dc.b  $56,$88,$00,$48,$02,$80,$03,$80 ; 0x3650
				dc.b  $03,$80,$03,$80,$03,$80,$94,$00 ; 0x3658
				dc.b  $00,$7C,$00,$E9,$00,$45,$0D,$EE ; 0x3660
				dc.b  $00,$FF,$00,$BF,$0D,$42,$00,$ED ; 0x3668
				dc.b  $00,$F0,$0D,$EA,$00,$FC,$00,$60 ; 0x3670
				dc.b  $02,$02,$00,$30,$03,$78,$00,$4C ; 0x3678
				dc.b  $03,$03,$00,$64,$00,$F5,$0E,$38 ; 0x3680
				dc.b  $00,$56,$00,$60,$0D,$0F,$00,$E6 ; 0x3688
				dc.b  $00,$9F,$0D,$52,$00,$EA,$00,$DE ; 0x3690
				dc.b  $00,$50,$6C,$FF,$00,$FF,$01,$F9 ; 0x3698
				dc.b  $01,$01,$00,$78,$00,$98,$17,$00 ; 0x36a0
				dc.b  $01,$10,$25,$4C,$01,$6C,$00,$54 ; 0x36a8
				dc.b  $00,$2A,$21,$1B,$00,$CF,$00,$F4 ; 0x36b0
				dc.b  $02,$07,$00,$FF,$02,$48,$00,$C8 ; 0x36b8
				dc.b  $02,$5F,$00,$82,$02,$A9,$00,$22 ; 0x36c0
				dc.b  $02,$9D,$00,$2A,$02,$48,$00,$C8 ; 0x36c8
				dc.b  $02,$5F,$00,$82,$05,$81,$00,$FF ; 0x36d0
				dc.b  $00,$E5,$04,$FF,$00,$CE,$0C,$61 ; 0x36d8
				dc.b  $00,$10,$02,$58,$00,$64,$0D,$04 ; 0x36e0
				dc.b  $00,$AB,$00,$FE,$05,$01,$10,$0D ; 0x36e8
				dc.b  $00,$86,$3C,$65,$00,$21,$00,$91 ; 0x36f0
				dc.b  $04,$02,$03,$01,$01,$02,$01,$00 ; 0x36f8
				dc.b  $55,$40,$03,$80,$03,$01,$03,$02 ; 0x3700
				dc.b  $03,$40,$03,$80,$0F,$08,$85,$FF ; 0x3708
				dc.b  $56,$80,$03,$80,$03,$80,$03,$80 ; 0x3710
				dc.b  $03,$80,$03,$80,$94,$00,$00,$8D ; 0x3718
				dc.b  $00,$62,$00,$E8,$0D,$04,$00,$6C ; 0x3720
				dc.b  $00,$38,$0D,$45,$00,$D3,$00,$80 ; 0x3728
				dc.b  $0D,$17,$00,$4E,$08,$78,$00,$4C ; 0x3730
				dc.b  $03,$03,$00,$74,$00,$68,$0E,$45 ; 0x3738
				dc.b  $00,$DB,$0E,$1B,$00,$AB,$00,$38 ; 0x3740
				dc.b  $0D,$61,$00,$95,$00,$FE,$00,$80 ; 0x3748
				dc.b  $6C,$FF,$00,$FF,$01,$F9,$01,$01 ; 0x3750
				dc.b  $00,$78,$00,$98,$17,$00,$01,$14 ; 0x3758
				dc.b  $00,$80,$02,$45,$00,$80,$02,$0F ; 0x3760
				dc.b  $00,$38,$01,$00,$00,$7E,$00,$73 ; 0x3768
				dc.b  $00,$FF,$01,$9E,$00,$A3,$00,$FF ; 0x3770
				dc.b  $00,$FF,$02,$20,$03,$60,$03,$60 ; 0x3778
				dc.b  $01,$72,$00,$03,$00,$FF,$5E,$65 ; 0x3780
				dc.b  $00,$40,$0B,$00,$62,$92,$0C,$00 ; 0x3788
				dc.b  $0D,$40,$00,$AC,$00,$88,$01,$80 ; 0x3790
				dc.b  $00,$80,$00,$00,$0C,$01,$00,$04 ; 0x3798
				dc.b  $00,$AC,$00,$88,$DB,$FF,$0C,$01 ; 0x37a0
				dc.b  $00,$26,$00,$EF,$00,$FF,$00,$01 ; 0x37a8
				dc.b  $00,$2D,$00,$FF,$00,$FF,$0D,$4D ; 0x37b0
				dc.b  $00,$87,$00,$FF,$DB,$00,$00,$3D ; 0x37b8
				dc.b  $00,$F4,$00,$AD,$0D,$50,$00,$3D ; 0x37c0
				dc.b  $00,$77,$0D,$06,$00,$3F,$00,$E0 ; 0x37c8
				dc.b  $02,$01,$00,$70,$08,$06,$00,$2B ; 0x37d0
				dc.b  $00,$65,$00,$C0,$0D,$39,$00,$DD ; 0x37d8
				dc.b  $0E,$32,$00,$A1,$00,$60,$0D,$6C ; 0x37e0
				dc.b  $00,$C5,$00,$17,$02,$03,$00,$E3 ; 0x37e8
				dc.b  $09,$5C,$00,$33,$00,$54,$00,$D0 ; 0x37f0
				dc.b  $01,$02,$00,$C3,$03,$79,$00,$50 ; 0x37f8
				dc.b  $03,$04,$01,$D6,$00,$8B,$1A,$C0 ; 0x3800
				dc.b  $00,$CF,$3B,$C0,$00,$CF,$02,$D6 ; 0x3808
				dc.b  $00,$8B,$02,$00,$00,$00,$01,$00 ; 0x3810
				dc.b  $00,$00,$00,$00,$17,$00,$01,$14 ; 0x3818
				dc.b  $00,$80,$31,$00,$00,$00,$00,$00 ; 0x3820
				dc.b  $14,$08,$2D,$00,$00,$00,$06,$14 ; 0x3828
				dc.b  $00,$00,$00,$00,$02,$1F,$00,$F8 ; 0x3830
				dc.b  $02,$20,$16,$00,$01,$03,$14,$FF ; 0x3838
				dc.b  $00,$FE,$00,$DD,$00,$C7,$00,$FF ; 0x3840
				dc.b  $00,$FD,$00,$9C,$00,$23,$18,$FF ; 0x3848
				dc.b  $00,$F6,$00,$9C,$00,$03,$00,$FF ; 0x3850
				dc.b  $00,$FE,$00,$54,$00,$E2,$01,$04 ; 0x3858
				dc.b  $00,$9A,$00,$80,$01,$02,$00,$C8 ; 0x3860
				dc.b  $00,$C0,$03,$00,$01,$00,$00,$00 ; 0x3868
				dc.b  $00,$00,$02,$92,$04,$94,$03,$0A ; 0x3870
				dc.b  $01,$04,$01,$02,$35,$00,$43,$00 ; 0x3878
				dc.b  $3F,$10,$00,$FD,$00,$04,$01,$20 ; 0x3880
				dc.b  $1B,$40,$03,$80,$03,$01,$03,$02 ; 0x3888
				dc.b  $19,$FF,$B8,$FF,$00,$FF,$00,$93 ; 0x3890
				dc.b  $00,$A7,$00,$FF,$00,$FF,$00,$94 ; 0x3898
				dc.b  $00,$23,$18,$FF,$00,$FE,$00,$7C ; 0x38a0
				dc.b  $00,$03,$00,$FF,$00,$FE,$00,$9E ; 0x38a8
				dc.b  $00,$02,$00,$FF,$00,$FF,$00,$32 ; 0x38b0
				dc.b  $01,$FF,$00,$FE,$00,$BC,$18,$00 ; 0x38b8
				dc.b  $00,$4B,$00,$7E,$00,$7F,$07,$75 ; 0x38c0
				dc.b  $00,$40,$03,$08,$00,$61,$00,$D8 ; 0x38c8
				dc.b  $00,$4D,$0C,$07,$00,$0D,$00,$3A ; 0x38d0
				dc.b  $0E,$34,$00,$EB,$0E,$46,$00,$8F ; 0x38d8
				dc.b  $0E,$3D,$00,$BD,$0E,$35,$00,$BE ; 0x38e0
				dc.b  $00,$ED,$0D,$42,$00,$FE,$00,$25 ; 0x38e8
				dc.b  $0E,$39,$00,$10,$57,$A1,$03,$39 ; 0x38f0
				dc.b  $00,$10,$00,$FF,$00,$FF,$00,$00 ; 0x38f8
				dc.b  $00,$DE,$02,$78,$00,$C0,$03,$0F ; 0x3900
				dc.b  $13,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x3908
				dc.b  $E0,$FF,$00,$FF,$00,$00,$00,$DE ; 0x3910
				dc.b  $01,$01,$00,$78,$00,$C0,$17,$00 ; 0x3918
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x3920
				dc.b  $01,$00,$23,$C0,$00,$00,$00,$00 ; 0x3928
				dc.b  $01,$C0,$00,$00,$00,$00,$1F,$08 ; 0x3930
				dc.b  $01,$08,$00,$00,$00,$00,$06,$80 ; 0x3938
				dc.b  $00,$00,$02,$80,$00,$00,$02,$80 ; 0x3940
				dc.b  $00,$00,$02,$80,$00,$00,$02,$80 ; 0x3948
				dc.b  $00,$00,$02,$80,$00,$00,$05,$00 ; 0x3950
				dc.b  $00,$00,$13,$04,$00,$00,$02,$04 ; 0x3958
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x3960
				dc.b  $05,$00,$01,$03,$0E,$08,$00,$00 ; 0x3968
				dc.b  $00,$00,$00,$00,$00,$00,$00,$00 ; 0x3970
				dc.b  $38,$00,$00,$00,$00,$00,$04,$00 ; 0x3978
				dc.b  $03,$00,$01,$00,$01,$00,$55,$00 ; 0x3980
				dc.b  $03,$00,$03,$00,$03,$00,$03,$00 ; 0x3988
				dc.b  $03,$00,$07,$00,$8D,$FF,$56,$00 ; 0x3990
				dc.b  $00,$00,$02,$00,$03,$00,$03,$00 ; 0x3998
				dc.b  $03,$00,$03,$00,$94,$00,$00,$00 ; 0x39a0
				dc.b  $00,$40,$00,$00,$0D,$00,$00,$00 ; 0x39a8
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x39b0
				dc.b  $0D,$00,$00,$00,$00,$00,$02,$00 ; 0x39b8
				dc.b  $00,$C0,$03,$EA,$00,$08,$03,$00 ; 0x39c0
				dc.b  $00,$00,$00,$00,$0E,$00,$00,$00 ; 0x39c8
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x39d0
				dc.b  $0D,$00,$00,$00,$00,$00,$00,$00 ; 0x39d8
				dc.b  $6F,$DE,$03,$C0,$17,$00,$01,$00 ; 0x39e0
				dc.b  $25,$00,$01,$C0,$00,$00,$00,$00 ; 0x39e8
				dc.b  $21,$08,$00,$00,$00,$00,$02,$00 ; 0x39f0
				dc.b  $00,$00,$02,$80,$00,$00,$02,$80 ; 0x39f8
				dc.b  $00,$00,$02,$80,$00,$00,$02,$80 ; 0x3a00
				dc.b  $00,$00,$02,$80,$00,$00,$02,$80 ; 0x3a08
				dc.b  $00,$00,$05,$00,$00,$00,$00,$00 ; 0x3a10
				dc.b  $04,$00,$00,$00,$0C,$04,$00,$00 ; 0x3a18
				dc.b  $02,$04,$00,$00,$0D,$00,$00,$00 ; 0x3a20
				dc.b  $00,$00,$05,$00,$10,$08,$00,$00 ; 0x3a28
				dc.b  $3C,$00,$00,$00,$00,$00,$04,$00 ; 0x3a30
				dc.b  $03,$00,$01,$00,$01,$00,$55,$00 ; 0x3a38
				dc.b  $03,$00,$03,$00,$03,$00,$03,$00 ; 0x3a40
				dc.b  $03,$00,$0F,$00,$85,$FF,$56,$00 ; 0x3a48
				dc.b  $03,$00,$03,$00,$03,$00,$03,$00 ; 0x3a50
				dc.b  $03,$00,$94,$00,$00,$00,$00,$40 ; 0x3a58
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x3a60
				dc.b  $0D,$00,$00,$00,$00,$00,$0D,$00 ; 0x3a68
				dc.b  $00,$00,$08,$EA,$00,$08,$03,$00 ; 0x3a70
				dc.b  $00,$00,$00,$00,$0E,$00,$00,$00 ; 0x3a78
				dc.b  $0E,$00,$00,$00,$00,$00,$0D,$00 ; 0x3a80
				dc.b  $00,$00,$00,$00,$00,$00,$6F,$DE ; 0x3a88
				dc.b  $03,$C0,$17,$00,$01,$10,$00,$00 ; 0x3a90
				dc.b  $06,$24,$00,$8C,$01,$01,$00,$7F ; 0x3a98
				dc.b  $00,$1F,$02,$9F,$00,$AF,$01,$00 ; 0x3aa0
				dc.b  $00,$76,$00,$7A,$00,$00,$01,$BF ; 0x3aa8
				dc.b  $00,$B2,$00,$5F,$01,$BF,$00,$70 ; 0x3ab0
				dc.b  $00,$01,$01,$84,$00,$C7,$CF,$93 ; 0x3ab8
				dc.b  $0C,$00,$0D,$00,$00,$FF,$00,$FF ; 0x3ac0
				dc.b  $01,$00,$00,$FF,$00,$FF,$01,$80 ; 0x3ac8
				dc.b  $00,$A9,$00,$4C,$0A,$FF,$00,$FF ; 0x3ad0
				dc.b  $DB,$FF,$0C,$00,$00,$00,$00,$00 ; 0x3ad8
				dc.b  $00,$00,$00,$00,$00,$00,$00,$00 ; 0x3ae0
				dc.b  $00,$00,$01,$CB,$00,$20,$0A,$5E ; 0x3ae8
				dc.b  $00,$AB,$DC,$00,$00,$4E,$00,$DC ; 0x3af0
				dc.b  $00,$27,$0D,$66,$00,$38,$00,$C5 ; 0x3af8
				dc.b  $0D,$07,$00,$39,$00,$50,$02,$02 ; 0x3b00
				dc.b  $00,$90,$09,$37,$00,$49,$00,$40 ; 0x3b08
				dc.b  $0D,$49,$00,$B7,$0E,$40,$00,$80 ; 0x3b10
				dc.b  $00,$20,$0D,$38,$00,$26,$00,$65 ; 0x3b18
				dc.b  $02,$00,$00,$C3,$09,$45,$00,$FD ; 0x3b20
				dc.b  $00,$50,$00,$70,$01,$00,$00,$F3 ; 0x3b28
				dc.b  $03,$75,$00,$40,$03,$08,$01,$00 ; 0x3b30
				dc.b  $00,$00,$1A,$FF,$00,$FF,$3B,$FF ; 0x3b38
				dc.b  $00,$FF,$02,$00,$00,$00,$1F,$00 ; 0x3b40
				dc.b  $01,$10,$00,$00,$22,$C3,$00,$78 ; 0x3b48
				dc.b  $02,$B8,$00,$15,$00,$80,$5A,$08 ; 0x3b50
				dc.b  $00,$5C,$02,$0F,$00,$D0,$2C,$00 ; 0x3b58
				dc.b  $00,$00,$00,$A0,$00,$30,$01,$FE ; 0x3b60
				dc.b  $00,$45,$00,$00,$18,$00,$00,$06 ; 0x3b68
				dc.b  $00,$DE,$00,$DF,$00,$00,$00,$01 ; 0x3b70
				dc.b  $00,$30,$00,$C0,$01,$11,$00,$13 ; 0x3b78
				dc.b  $00,$C0,$02,$C2,$00,$E0,$0A,$93 ; 0x3b80
				dc.b  $04,$01,$07,$04,$25,$10,$00,$1C ; 0x3b88
				dc.b  $00,$08,$01,$20,$00,$19,$00,$58 ; 0x3b90
				dc.b  $8E,$FF,$00,$FF,$43,$FF,$25,$C0 ; 0x3b98
				dc.b  $00,$90,$02,$C0,$00,$D8,$8D,$00 ; 0x3ba0
				dc.b  $00,$00,$00,$12,$00,$80,$00,$00 ; 0x3ba8
				dc.b  $00,$00,$00,$1D,$00,$00,$18,$00 ; 0x3bb0
				dc.b  $00,$00,$00,$25,$00,$FF,$01,$FF ; 0x3bb8
				dc.b  $01,$00,$00,$00,$00,$09,$00,$14 ; 0x3bc0
				dc.b  $01,$00,$00,$09,$00,$14,$18,$00 ; 0x3bc8
				dc.b  $00,$4C,$00,$1C,$00,$53,$0C,$07 ; 0x3bd0
				dc.b  $00,$62,$00,$A5,$00,$89,$0C,$00 ; 0x3bd8
				dc.b  $01,$56,$00,$90,$0D,$35,$00,$5A ; 0x3be0
				dc.b  $0F,$74,$00,$30,$02,$08,$00,$50 ; 0x3be8
				dc.b  $09,$D3,$00,$B1,$00,$00,$02,$0B ; 0x3bf0
				dc.b  $0A,$37,$00,$29,$00,$49,$02,$04 ; 0x3bf8
				dc.b  $00,$F3,$09,$25,$00,$77,$00,$9B ; 0x3c00
				dc.b  $00,$30,$01,$04,$00,$A3,$88,$00 ; 0x3c08
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x3c10
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x3c18
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x3c20
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x3c28
				dc.b  $01,$14,$00,$80,$01,$00,$00,$54 ; 0x3c30
				dc.b  $00,$00,$01,$00,$00,$5C,$00,$AC ; 0x3c38
				dc.b  $02,$8B,$00,$FF,$02,$A2,$00,$FF ; 0x3c40
				dc.b  $01,$FF,$00,$00,$00,$00,$00,$01 ; 0x3c48
				dc.b  $01,$2A,$00,$F0,$00,$15,$01,$2E ; 0x3c50
				dc.b  $00,$F2,$00,$0E,$01,$A4,$00,$BD ; 0x3c58
				dc.b  $00,$E0,$09,$2B,$00,$38,$00,$15 ; 0x3c60
				dc.b  $01,$2F,$00,$40,$00,$17,$65,$02 ; 0x3c68
				dc.b  $47,$0D,$00,$20,$00,$0D,$01,$0C ; 0x3c70
				dc.b  $00,$37,$00,$7A,$0A,$9C,$0C,$00 ; 0x3c78
				dc.b  $15,$00,$00,$15,$00,$BC,$C9,$10 ; 0x3c80
				dc.b  $00,$87,$00,$44,$01,$20,$19,$FF ; 0x3c88
				dc.b  $14,$FF,$00,$00,$00,$00,$00,$20 ; 0x3c90
				dc.b  $09,$7C,$00,$2F,$00,$E0,$BD,$09 ; 0x3c98
				dc.b  $00,$07,$00,$ED,$01,$09,$00,$07 ; 0x3ca0
				dc.b  $00,$BA,$17,$00,$00,$AC,$00,$2C ; 0x3ca8
				dc.b  $00,$B8,$0D,$DF,$00,$90,$00,$A8 ; 0x3cb0
				dc.b  $0D,$08,$00,$1F,$00,$E0,$03,$D0 ; 0x3cb8
				dc.b  $09,$78,$00,$EA,$00,$00,$0D,$A1 ; 0x3cc0
				dc.b  $00,$38,$0E,$8D,$00,$11,$00,$00 ; 0x3cc8
				dc.b  $0D,$7A,$00,$CD,$00,$A8,$0D,$99 ; 0x3cd0
				dc.b  $00,$12,$00,$3B,$00,$80,$0B,$00 ; 0x3cd8
				dc.b  $01,$91,$00,$71,$1A,$9C,$00,$67 ; 0x3ce0
				dc.b  $3B,$9C,$00,$67,$02,$91,$00,$71 ; 0x3ce8
				dc.b  $1F,$00,$01,$14,$00,$80,$22,$2E ; 0x3cf0
				dc.b  $00,$F8,$02,$2F,$00,$B8,$00,$00 ; 0x3cf8
				dc.b  $5A,$18,$00,$A3,$02,$24,$00,$44 ; 0x3d00
				dc.b  $2C,$FF,$00,$FF,$00,$1D,$00,$FF ; 0x3d08
				dc.b  $02,$30,$00,$3C,$18,$FF,$00,$FD ; 0x3d10
				dc.b  $00,$E7,$00,$FF,$00,$FF,$00,$F8 ; 0x3d18
				dc.b  $00,$D8,$00,$80,$01,$0A,$00,$36 ; 0x3d20
				dc.b  $00,$E0,$01,$0A,$00,$E6,$00,$40 ; 0x3d28
				dc.b  $0A,$9C,$0C,$04,$25,$00,$00,$FF ; 0x3d30
				dc.b  $00,$FF,$01,$00,$00,$FF,$00,$FF ; 0x3d38
				dc.b  $D3,$FF,$25,$00,$00,$00,$02,$00 ; 0x3d40
				dc.b  $00,$00,$90,$7F,$02,$1C,$00,$FC ; 0x3d48
				dc.b  $1A,$2D,$03,$A4,$03,$20,$03,$20 ; 0x3d50
				dc.b  $18,$00,$00,$B2,$00,$9B,$00,$40 ; 0x3d58
				dc.b  $0C,$08,$00,$E7,$00,$ED,$00,$C0 ; 0x3d60
				dc.b  $0C,$07,$00,$61,$00,$B6,$00,$00 ; 0x3d68
				dc.b  $02,$06,$00,$20,$09,$B0,$00,$86 ; 0x3d70
				dc.b  $00,$30,$02,$05,$00,$B0,$09,$C1 ; 0x3d78
				dc.b  $00,$94,$00,$60,$02,$07,$00,$10 ; 0x3d80
				dc.b  $09,$8D,$00,$B6,$00,$30,$02,$03 ; 0x3d88
				dc.b  $00,$00,$09,$57,$00,$F3,$00,$E0 ; 0x3d90
				dc.b  $02,$02,$0A,$9C,$00,$D5,$00,$A4 ; 0x3d98
				dc.b  $00,$00,$01,$02,$00,$F3,$88,$00 ; 0x3da0
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x3da8
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x3db0
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x3db8
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x3dc0
				dc.b  $05,$9F,$03,$91,$00,$B0,$02,$BD ; 0x3dc8
				dc.b  $00,$DF,$02,$3B,$00,$BF,$06,$5D ; 0x3dd0
				dc.b  $00,$D8,$00,$00,$01,$5F,$00,$7C ; 0x3dd8
				dc.b  $00,$00,$01,$75,$00,$6D,$00,$FF ; 0x3de0
				dc.b  $09,$5E,$00,$F2,$00,$00,$01,$60 ; 0x3de8
				dc.b  $00,$1E,$00,$00,$4D,$1A,$00,$A0 ; 0x3df0
				dc.b  $00,$78,$15,$01,$47,$08,$00,$F0 ; 0x3df8
				dc.b  $00,$00,$01,$08,$00,$F0,$00,$00 ; 0x3e00
				dc.b  $0A,$85,$0C,$00,$0E,$80,$00,$00 ; 0x3e08
				dc.b  $01,$20,$00,$80,$00,$00,$02,$FF ; 0x3e10
				dc.b  $00,$FF,$C9,$00,$00,$FF,$00,$FF ; 0x3e18
				dc.b  $01,$00,$19,$FF,$0C,$01,$00,$5F ; 0x3e20
				dc.b  $00,$7F,$00,$FF,$00,$01,$00,$5F ; 0x3e28
				dc.b  $00,$3F,$00,$FF,$00,$00,$02,$00 ; 0x3e30
				dc.b  $09,$56,$00,$F3,$00,$FF,$BD,$00 ; 0x3e38
				dc.b  $00,$00,$00,$00,$01,$00,$00,$00 ; 0x3e40
				dc.b  $00,$00,$17,$00,$00,$82,$00,$1A ; 0x3e48
				dc.b  $00,$04,$0D,$A8,$00,$DA,$00,$CC ; 0x3e50
				dc.b  $0D,$06,$00,$3C,$00,$B0,$02,$00 ; 0x3e58
				dc.b  $00,$30,$09,$5B,$00,$53,$0E,$79 ; 0x3e60
				dc.b  $00,$C4,$0E,$6A,$00,$8B,$00,$80 ; 0x3e68
				dc.b  $0D,$5C,$00,$C0,$00,$4C,$0D,$73 ; 0x3e70
				dc.b  $00,$9C,$00,$A8,$00,$40,$0D,$80 ; 0x3e78
				dc.b  $00,$00,$1A,$80,$00,$00,$22,$00 ; 0x3e80
				dc.b  $09,$00,$09,$00,$04,$80,$00,$00 ; 0x3e88
				dc.b  $02,$80,$00,$00,$05,$1B,$00,$F3 ; 0x3e90
				dc.b  $00,$F0,$01,$02,$01,$0F,$13,$00 ; 0x3e98
				dc.b  $25,$5D,$00,$F0,$02,$60,$00,$F0 ; 0x3ea0
				dc.b  $5B,$0C,$00,$5C,$02,$87,$00,$FF ; 0x3ea8
				dc.b  $2E,$08,$00,$38,$00,$00,$00,$01 ; 0x3eb0
				dc.b  $00,$90,$00,$F0,$19,$FA,$00,$40 ; 0x3eb8
				dc.b  $00,$83,$01,$FB,$00,$38,$00,$42 ; 0x3ec0
				dc.b  $00,$FF,$00,$FF,$00,$32,$00,$00 ; 0x3ec8
				dc.b  $00,$FF,$00,$FE,$00,$BC,$00,$00 ; 0x3ed0
				dc.b  $0A,$85,$0C,$02,$BA,$FD,$00,$04 ; 0x3ed8
				dc.b  $43,$FF,$B8,$FF,$00,$FF,$00,$9A ; 0x3ee0
				dc.b  $00,$80,$00,$FF,$00,$FF,$00,$9B ; 0x3ee8
				dc.b  $00,$00,$18,$FF,$00,$FE,$00,$7C ; 0x3ef0
				dc.b  $00,$03,$01,$FE,$00,$9E,$00,$02 ; 0x3ef8
				dc.b  $00,$FF,$00,$FF,$00,$32,$01,$FF ; 0x3f00
				dc.b  $00,$FE,$00,$BC,$18,$00,$00,$B6 ; 0x3f08
				dc.b  $00,$1F,$00,$4C,$0D,$EC,$00,$80 ; 0x3f10
				dc.b  $00,$24,$0D,$1F,$00,$FA,$00,$40 ; 0x3f18
				dc.b  $02,$00,$00,$30,$09,$7F,$00,$E9 ; 0x3f20
				dc.b  $00,$00,$02,$00,$00,$C0,$09,$AA ; 0x3f28
				dc.b  $00,$8C,$00,$00,$02,$01,$00,$00 ; 0x3f30
				dc.b  $09,$95,$00,$3A,$00,$80,$02,$00 ; 0x3f38
				dc.b  $00,$E0,$09,$81,$00,$E8,$00,$A4 ; 0x3f40
				dc.b  $02,$00,$00,$C3,$09,$A1,$00,$ED ; 0x3f48
				dc.b  $00,$8C,$00,$C0,$01,$00,$89,$00 ; 0x3f50
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x3f58
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x3f60
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x3f68
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x3f70
				dc.b  $01,$10,$00,$00,$03,$0C,$05,$00 ; 0x3f78
				dc.b  $00,$E5,$00,$C3,$02,$BE,$00,$FF ; 0x3f80
				dc.b  $01,$00,$02,$00,$06,$B8,$02,$78 ; 0x3f88
				dc.b  $00,$DB,$CF,$8F,$00,$02,$0B,$00 ; 0x3f90
				dc.b  $0D,$20,$00,$9C,$00,$9C,$01,$00 ; 0x3f98
				dc.b  $00,$FF,$00,$FF,$EB,$FF,$0D,$6C ; 0x3fa0
				dc.b  $02,$00,$00,$00,$00,$00,$00,$00 ; 0x3fa8
				dc.b  $0D,$41,$00,$1F,$DC,$00,$00,$56 ; 0x3fb0
				dc.b  $00,$9A,$00,$9A,$0D,$70,$00,$4A ; 0x3fb8
				dc.b  $00,$AE,$0D,$09,$00,$36,$00,$10 ; 0x3fc0
				dc.b  $0D,$3C,$00,$BB,$00,$80,$0D,$50 ; 0x3fc8
				dc.b  $00,$FA,$0E,$5E,$00,$D4,$00,$E0 ; 0x3fd0
				dc.b  $02,$02,$00,$20,$09,$3D,$00,$AE ; 0x3fd8
				dc.b  $00,$6E,$0D,$4C,$00,$E2,$00,$5D ; 0x3fe0
				dc.b  $00,$A0,$4D,$04,$09,$04,$09,$04 ; 0x3fe8
				dc.b  $0F,$00,$00,$00,$00,$00,$01,$00 ; 0x3ff0
				dc.b  $01,$00,$13,$00,$01,$10,$00,$00 ; 0x3ff8
				dc.b  $83,$AA,$00,$4D,$02,$09,$00,$4C ; 0x4000
				dc.b  $2D,$FD,$00,$A8,$00,$60,$00,$FF ; 0x4008
				dc.b  $00,$FF,$00,$22,$00,$00,$19,$F7 ; 0x4010
				dc.b  $00,$D4,$00,$C3,$00,$00,$00,$06 ; 0x4018
				dc.b  $00,$21,$00,$E2,$02,$26,$00,$01 ; 0x4020
				dc.b  $02,$B0,$00,$02,$0A,$8F,$00,$02 ; 0x4028
				dc.b  $03,$00,$07,$02,$FF,$FF,$E2,$26 ; 0x4030
				dc.b  $00,$01,$02,$B0,$00,$02,$17,$00 ; 0x4038
				dc.b  $00,$8A,$00,$9F,$00,$E2,$0D,$B3 ; 0x4040
				dc.b  $00,$F0,$00,$06,$0D,$18,$00,$54 ; 0x4048
				dc.b  $00,$60,$0D,$61,$00,$51,$00,$80 ; 0x4050
				dc.b  $0D,$81,$00,$C2,$0E,$71,$00,$89 ; 0x4058
				dc.b  $00,$C0,$0D,$62,$00,$D6,$00,$C6 ; 0x4060
				dc.b  $0D,$7B,$00,$33,$00,$42,$00,$20 ; 0x4068
				dc.b  $8B,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x4070
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x4078
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x4080
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x4088
				dc.b  $FF,$00,$01,$14,$00,$80,$02,$B6 ; 0x4090
				dc.b  $00,$E8,$02,$B7,$00,$18,$01,$02 ; 0x4098
				dc.b  $00,$06,$00,$FF,$02,$FD,$00,$7B ; 0x40a0
				dc.b  $02,$24,$00,$B6,$02,$58,$00,$50 ; 0x40a8
				dc.b  $02,$60,$00,$D8,$02,$86,$00,$1B ; 0x40b0
				dc.b  $00,$DE,$09,$59,$00,$C4,$00,$2C ; 0x40b8
				dc.b  $01,$61,$00,$08,$00,$30,$4D,$00 ; 0x40c0
				dc.b  $00,$65,$00,$40,$6E,$9A,$00,$00 ; 0x40c8
				dc.b  $0B,$00,$0D,$40,$00,$8A,$00,$90 ; 0x40d0
				dc.b  $01,$40,$00,$70,$00,$C4,$01,$04 ; 0x40d8
				dc.b  $00,$80,$00,$00,$08,$00,$00,$00 ; 0x40e0
				dc.b  $DD,$FF,$0D,$7E,$00,$9F,$01,$01 ; 0x40e8
				dc.b  $00,$8E,$00,$7F,$00,$FF,$01,$21 ; 0x40f0
				dc.b  $00,$C0,$0A,$00,$00,$00,$00,$00 ; 0x40f8
				dc.b  $DB,$00,$00,$CF,$00,$E4,$00,$16 ; 0x4100
				dc.b  $0D,$0E,$00,$02,$00,$62,$0D,$24 ; 0x4108
				dc.b  $00,$82,$00,$20,$0C,$08,$00,$92 ; 0x4110
				dc.b  $00,$08,$0E,$C2,$00,$B6,$0E,$AA ; 0x4118
				dc.b  $00,$5F,$00,$40,$02,$00,$00,$E0 ; 0x4120
				dc.b  $09,$78,$00,$3C,$00,$42,$02,$03 ; 0x4128
				dc.b  $00,$43,$09,$B8,$00,$DE,$00,$ED ; 0x4130
				dc.b  $00,$60,$0D,$85,$00,$F0,$1A,$DB ; 0x4138
				dc.b  $00,$E7,$3B,$DB,$00,$E7,$02,$85 ; 0x4140
				dc.b  $00,$F0,$1F,$00,$01,$14,$00,$80 ; 0x4148
				dc.b  $22,$57,$00,$A8,$02,$5E,$00,$FE ; 0x4150
				dc.b  $5B,$09,$00,$45,$02,$07,$00,$F3 ; 0x4158
				dc.b  $2C,$00,$00,$00,$00,$31,$00,$70 ; 0x4160
				dc.b  $00,$00,$00,$01,$00,$8B,$00,$F0 ; 0x4168
				dc.b  $18,$00,$00,$00,$00,$9E,$00,$80 ; 0x4170
				dc.b  $02,$86,$00,$00,$02,$90,$00,$C0 ; 0x4178
				dc.b  $00,$00,$00,$04,$00,$7A,$00,$40 ; 0x4180
				dc.b  $0A,$9A,$00,$00,$03,$01,$07,$02 ; 0x4188
				dc.b  $BA,$FF,$00,$FF,$43,$FF,$BA,$A1 ; 0x4190
				dc.b  $03,$A2,$1B,$88,$00,$00,$02,$AA ; 0x4198
				dc.b  $00,$00,$02,$32,$00,$00,$02,$BC ; 0x41a0
				dc.b  $00,$00,$17,$00,$00,$E4,$00,$82 ; 0x41a8
				dc.b  $00,$CC,$0D,$28,$00,$D2,$00,$A4 ; 0x41b0
				dc.b  $0D,$28,$00,$22,$00,$40,$0D,$A0 ; 0x41b8
				dc.b  $00,$89,$00,$00,$0D,$D6,$00,$0C ; 0x41c0
				dc.b  $0E,$BB,$00,$4A,$00,$80,$0D,$A3 ; 0x41c8
				dc.b  $00,$0B,$00,$24,$0D,$CB,$00,$3A ; 0x41d0
				dc.b  $00,$C4,$00,$C0,$8B,$00,$FF,$00 ; 0x41d8
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0x41e0
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0x41e8
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0x41f0
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$01,$10 ; 0x41f8
				dc.b  $00,$00,$02,$A1,$00,$D0,$00,$50 ; 0x4200
				dc.b  $01,$99,$00,$12,$00,$4C,$00,$01 ; 0x4208
				dc.b  $00,$33,$00,$3B,$02,$A9,$00,$FF ; 0x4210
				dc.b  $02,$29,$00,$DC,$0A,$9B,$00,$1F ; 0x4218
				dc.b  $00,$FF,$BD,$0A,$00,$94,$00,$60 ; 0x4220
				dc.b  $01,$0F,$00,$FF,$00,$E0,$0A,$95 ; 0x4228
				dc.b  $00,$08,$0B,$00,$0D,$10,$00,$DD ; 0x4230
				dc.b  $00,$AC,$01,$00,$00,$FF,$00,$FF ; 0x4238
				dc.b  $01,$08,$00,$FF,$00,$FF,$E7,$FF ; 0x4240
				dc.b  $0D,$C1,$00,$3F,$01,$00,$00,$00 ; 0x4248
				dc.b  $00,$00,$00,$00,$00,$FF,$00,$FF ; 0x4250
				dc.b  $00,$A0,$E8,$00,$00,$78,$00,$A2 ; 0x4258
				dc.b  $00,$13,$0D,$9C,$00,$8A,$00,$C9 ; 0x4260
				dc.b  $0D,$15,$00,$2A,$00,$90,$0C,$00 ; 0x4268
				dc.b  $00,$54,$00,$AA,$00,$40,$07,$78 ; 0x4270
				dc.b  $00,$4C,$03,$03,$00,$70,$00,$E3 ; 0x4278
				dc.b  $0E,$62,$00,$C6,$00,$A0,$0D,$55 ; 0x4280
				dc.b  $00,$FC,$00,$E9,$02,$00,$00,$C3 ; 0x4288
				dc.b  $09,$6B,$00,$2E,$00,$87,$00,$30 ; 0x4290
				dc.b  $0D,$80,$00,$00,$1A,$80,$00,$00 ; 0x4298
				dc.b  $3B,$80,$00,$00,$02,$80,$00,$00 ; 0x42a0
				dc.b  $1F,$00,$01,$10,$00,$00,$22,$58 ; 0x42a8
				dc.b  $00,$5C,$00,$2C,$01,$61,$00,$A4 ; 0x42b0
				dc.b  $00,$30,$5A,$14,$00,$16,$02,$0D ; 0x42b8
				dc.b  $00,$44,$2D,$01,$00,$81,$00,$BF ; 0x42c0
				dc.b  $00,$FF,$00,$FD,$00,$B8,$00,$A3 ; 0x42c8
				dc.b  $18,$FF,$00,$F6,$00,$BC,$00,$03 ; 0x42d0
				dc.b  $00,$FF,$00,$FC,$00,$9C,$00,$62 ; 0x42d8
				dc.b  $02,$32,$00,$00,$00,$FF,$00,$FE ; 0x42e0
				dc.b  $00,$BC,$00,$00,$0A,$95,$00,$08 ; 0x42e8
				dc.b  $0B,$02,$BA,$FD,$00,$04,$43,$FF ; 0x42f0
				dc.b  $BA,$93,$00,$A7,$02,$94,$00,$23 ; 0x42f8
				dc.b  $1A,$7C,$00,$03,$02,$9E,$00,$02 ; 0x4300
				dc.b  $1F,$00,$00,$8D,$00,$40,$00,$C9 ; 0x4308
				dc.b  $0D,$B7,$00,$5B,$00,$0B,$0D,$18 ; 0x4310
				dc.b  $00,$CA,$00,$B0,$0D,$63,$00,$2A ; 0x4318
				dc.b  $00,$C0,$0D,$84,$00,$39,$0E,$73 ; 0x4320
				dc.b  $00,$B1,$00,$E0,$0D,$64,$00,$B7 ; 0x4328
				dc.b  $00,$6B,$0D,$7D,$00,$8A,$00,$5E ; 0x4330
				dc.b  $00,$90,$8B,$00,$FF,$00,$FF,$FF ; 0x4338
				dc.b  $FF,$00,$FF,$00,$FF,$00,$FF,$FF ; 0x4340
				dc.b  $FF,$00,$FF,$00,$FF,$00,$FF,$FF ; 0x4348
				dc.b  $FF,$00,$FF,$00,$FF,$00,$FF,$FF ; 0x4350
				dc.b  $FF,$00,$FF,$00,$01,$14,$00,$80 ; 0x4358
				dc.b  $02,$A9,$00,$0E,$00,$00,$01,$B2 ; 0x4360
				dc.b  $00,$C2,$00,$00,$01,$9E,$00,$13 ; 0x4368
				dc.b  $02,$C3,$00,$03,$01,$FF,$00,$8C ; 0x4370
				dc.b  $00,$7E,$03,$38,$06,$B8,$00,$F8 ; 0x4378
				dc.b  $BE,$08,$00,$F0,$00,$00,$01,$08 ; 0x4380
				dc.b  $00,$F0,$00,$00,$0B,$00,$0B,$00 ; 0x4388
				dc.b  $0C,$01,$00,$04,$00,$FF,$00,$FF ; 0x4390
				dc.b  $00,$01,$00,$04,$03,$40,$03,$10 ; 0x4398
				dc.b  $00,$00,$00,$00,$01,$20,$00,$00 ; 0x43a0
				dc.b  $00,$00,$00,$01,$00,$04,$DD,$FF ; 0x43a8
				dc.b  $0C,$00,$00,$D2,$00,$AF,$02,$F7 ; 0x43b0
				dc.b  $00,$9F,$00,$FF,$01,$EA,$00,$C0 ; 0x43b8
				dc.b  $02,$58,$00,$38,$02,$60,$00,$D8 ; 0x43c0
				dc.b  $02,$86,$00,$1F,$00,$FF,$DB,$00 ; 0x43c8
				dc.b  $00,$CF,$00,$49,$00,$75,$07,$79 ; 0x43d0
				dc.b  $00,$50,$03,$04,$00,$0D,$00,$39 ; 0x43d8
				dc.b  $00,$4F,$07,$79,$00,$50,$03,$04 ; 0x43e0
				dc.b  $00,$18,$00,$36,$00,$B0,$02,$1A ; 0x43e8
				dc.b  $00,$20,$08,$06,$00,$98,$00,$C5 ; 0x43f0
				dc.b  $00,$A0,$02,$06,$00,$70,$09,$C2 ; 0x43f8
				dc.b  $00,$25,$0E,$A9,$00,$E0,$00,$60 ; 0x4400
				dc.b  $0D,$93,$00,$E2,$00,$2F,$0C,$08 ; 0x4408
				dc.b  $00,$B8,$00,$55,$00,$41,$00,$50 ; 0x4410
				dc.b  $0D,$21,$00,$BE,$1A,$87,$00,$96 ; 0x4418
				dc.b  $3B,$87,$00,$96,$02,$21,$00,$BE ; 0x4420
				dc.b  $1F,$00,$01,$14,$00,$80,$B5,$FF ; 0x4428
				dc.b  $00,$FF,$00,$CE,$00,$E7,$02,$AE ; 0x4430
				dc.b  $1A,$F9,$00,$5A,$00,$C3,$01,$F7 ; 0x4438
				dc.b  $00,$3C,$00,$82,$01,$F9,$00,$41 ; 0x4440
				dc.b  $00,$E1,$01,$FF,$00,$29,$00,$22 ; 0x4448
				dc.b  $0B,$00,$0B,$02,$FF,$FF,$E2,$26 ; 0x4450
				dc.b  $00,$01,$02,$B0,$00,$02,$17,$00 ; 0x4458
				dc.b  $00,$E3,$00,$E8,$00,$2B,$0D,$28 ; 0x4460
				dc.b  $00,$09,$00,$91,$0D,$28,$00,$07 ; 0x4468
				dc.b  $00,$10,$0D,$A0,$00,$1C,$00,$40 ; 0x4470
				dc.b  $0D,$D5,$00,$7B,$0E,$BA,$00,$CB ; 0x4478
				dc.b  $00,$A0,$0D,$A2,$00,$9C,$00,$B1 ; 0x4480
				dc.b  $0D,$CA,$00,$B1,$00,$18,$00,$B0 ; 0x4488
				dc.b  $8B,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x4490
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x4498
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x44a0
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x44a8
				dc.b  $FF,$00,$01,$14,$00,$80,$02,$A1 ; 0x44b0
				dc.b  $00,$D0,$02,$86,$00,$28,$01,$02 ; 0x44b8
				dc.b  $00,$54,$00,$9F,$00,$B5,$00,$02 ; 0x44c0
				dc.b  $00,$3F,$00,$2F,$00,$DC,$01,$20 ; 0x44c8
				dc.b  $00,$6C,$02,$58,$00,$50,$02,$60 ; 0x44d0
				dc.b  $00,$D8,$02,$59,$00,$ED,$00,$FF ; 0x44d8
				dc.b  $09,$59,$00,$B8,$02,$61,$00,$08 ; 0x44e0
				dc.b  $4E,$00,$00,$65,$00,$40,$15,$01 ; 0x44e8
				dc.b  $46,$FF,$00,$F6,$00,$E0,$00,$02 ; 0x44f0
				dc.b  $00,$FF,$00,$F9,$00,$CF,$00,$FF ; 0x44f8
				dc.b  $08,$65,$00,$21,$00,$90,$08,$04 ; 0x4500
				dc.b  $01,$01,$01,$00,$15,$C1,$00,$80 ; 0x4508
				dc.b  $00,$00,$08,$01,$00,$04,$BF,$10 ; 0x4510
				dc.b  $03,$20,$19,$FF,$21,$56,$00,$4B ; 0x4518
				dc.b  $00,$FF,$BC,$FF,$00,$FE,$00,$C0 ; 0x4520
				dc.b  $00,$02,$02,$1B,$00,$FF,$17,$00 ; 0x4528
				dc.b  $00,$92,$00,$11,$00,$8D,$0D,$BD ; 0x4530
				dc.b  $00,$9E,$00,$17,$0D,$0A,$00,$41 ; 0x4538
				dc.b  $00,$80,$0C,$06,$00,$66,$00,$8D ; 0x4540
				dc.b  $00,$C0,$0D,$88,$00,$BD,$0E,$77 ; 0x4548
				dc.b  $00,$A5,$00,$60,$0D,$68,$00,$27 ; 0x4550
				dc.b  $00,$F7,$0D,$81,$00,$D3,$00,$F2 ; 0x4558
				dc.b  $00,$D0,$8B,$00,$FF,$00,$FF,$FF ; 0x4560
				dc.b  $FF,$00,$E0,$FF,$00,$FF,$03,$01 ; 0x4568
				dc.b  $00,$78,$00,$C0,$17,$00,$FF,$00 ; 0x4570
				dc.b  $FF,$FF,$FF,$00,$E0,$FF,$00,$FF ; 0x4578
				dc.b  $01,$DE,$01,$01,$00,$78,$00,$C0 ; 0x4580
				dc.b  $17,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x4588
				dc.b  $81,$07,$00,$EC,$5B,$07,$00,$EC ; 0x4590
				dc.b  $03,$5A,$01,$01,$00,$78,$00,$98 ; 0x4598
				dc.b  $17,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x45a0
				dc.b  $E0,$FF,$00,$FF,$01,$DE,$01,$01 ; 0x45a8
				dc.b  $00,$78,$00,$C0,$17,$00,$FF,$00 ; 0x45b0
				dc.b  $FF,$FF,$FF,$00,$E0,$FF,$00,$FF ; 0x45b8
				dc.b  $01,$DE,$01,$01,$00,$78,$00,$C0 ; 0x45c0
				dc.b  $17,$00,$01,$10,$00,$00,$02,$CC ; 0x45c8
				dc.b  $00,$B4,$02,$AB,$00,$12,$01,$01 ; 0x45d0
				dc.b  $00,$26,$00,$77,$00,$FF,$00,$01 ; 0x45d8
				dc.b  $00,$7F,$00,$D3,$00,$FF,$00,$FF ; 0x45e0
				dc.b  $00,$FF,$00,$D8,$02,$6D,$00,$83 ; 0x45e8
				dc.b  $02,$5A,$00,$A6,$00,$80,$01,$6D ; 0x45f0
				dc.b  $00,$C2,$BD,$00,$00,$10,$00,$04 ; 0x45f8
				dc.b  $00,$C0,$00,$00,$00,$0A,$00,$A1 ; 0x4600
				dc.b  $00,$C0,$0A,$9C,$00,$15,$0B,$00 ; 0x4608
				dc.b  $0D,$40,$03,$40,$00,$AE,$00,$EC ; 0x4610
				dc.b  $01,$02,$00,$FF,$00,$FF,$01,$10 ; 0x4618
				dc.b  $00,$13,$00,$24,$01,$20,$00,$11 ; 0x4620
				dc.b  $00,$20,$0D,$40,$00,$00,$00,$00 ; 0x4628
				dc.b  $01,$80,$00,$00,$00,$00,$B0,$04 ; 0x4630
				dc.b  $00,$04,$19,$FF,$0C,$02,$00,$18 ; 0x4638
				dc.b  $00,$2F,$00,$FF,$00,$02,$00,$24 ; 0x4640
				dc.b  $00,$FF,$00,$FF,$00,$FF,$00,$F1 ; 0x4648
				dc.b  $00,$60,$02,$66,$00,$C0,$02,$60 ; 0x4650
				dc.b  $00,$D8,$02,$30,$00,$47,$0A,$59 ; 0x4658
				dc.b  $00,$B8,$02,$61,$00,$08,$AD,$00 ; 0x4660
				dc.b  $00,$08,$00,$7A,$00,$00,$01,$09 ; 0x4668
				dc.b  $00,$0E,$00,$00,$17,$00,$00,$CD ; 0x4670
				dc.b  $00,$F7,$00,$67,$0D,$20,$00,$01 ; 0x4678
				dc.b  $00,$29,$02,$06,$00,$23,$08,$08 ; 0x4680
				dc.b  $00,$09,$00,$40,$03,$04,$0A,$44 ; 0x4688
				dc.b  $00,$B9,$00,$40,$0D,$9B,$00,$4C ; 0x4690
				dc.b  $00,$50,$02,$02,$00,$70,$09,$D1 ; 0x4698
				dc.b  $00,$C8,$00,$80,$02,$01,$00,$20 ; 0x46a0
				dc.b  $09,$49,$00,$CC,$00,$25,$0D,$9B ; 0x46a8
				dc.b  $00,$15,$00,$84,$00,$70,$0D,$8E ; 0x46b0
				dc.b  $00,$7C,$5B,$8E,$00,$7C,$05,$1B ; 0x46b8
				dc.b  $00,$F3,$00,$F0,$17,$00,$01,$10 ; 0x46c0
				dc.b  $23,$52,$00,$9E,$02,$4A,$00,$79 ; 0x46c8
				dc.b  $20,$03,$01,$51,$00,$B7,$00,$FF ; 0x46d0
				dc.b  $02,$08,$03,$26,$00,$6F,$02,$1B ; 0x46d8
				dc.b  $00,$13,$14,$FF,$00,$B0,$00,$66 ; 0x46e0
				dc.b  $06,$1F,$00,$FC,$02,$5F,$00,$FC ; 0x46e8
				dc.b  $02,$5B,$00,$70,$1E,$01,$01,$01 ; 0x46f0
				dc.b  $05,$66,$00,$CC,$02,$5D,$00,$18 ; 0x46f8
				dc.b  $03,$0D,$00,$AB,$21,$75,$0B,$0F ; 0x4700
				dc.b  $00,$09,$00,$40,$01,$01,$00,$37 ; 0x4708
				dc.b  $00,$E0,$03,$02,$01,$01,$00,$4F ; 0x4710
				dc.b  $00,$FF,$00,$65,$00,$21,$00,$9C ; 0x4718
				dc.b  $00,$15,$07,$03,$03,$09,$25,$10 ; 0x4720
				dc.b  $00,$44,$00,$4B,$01,$02,$00,$3D ; 0x4728
				dc.b  $00,$2B,$45,$10,$00,$D2,$00,$14 ; 0x4730
				dc.b  $05,$20,$03,$20,$02,$01,$00,$04 ; 0x4738
				dc.b  $52,$01,$00,$04,$00,$23,$00,$E7 ; 0x4740
				dc.b  $09,$40,$03,$80,$19,$FF,$25,$66 ; 0x4748
				dc.b  $00,$06,$02,$5A,$00,$FC,$52,$40 ; 0x4750
				dc.b  $03,$1D,$00,$78,$51,$FF,$00,$EA ; 0x4758
				dc.b  $0B,$09,$00,$08,$02,$09,$00,$08 ; 0x4760
				dc.b  $18,$00,$00,$DE,$00,$FA,$00,$9C ; 0x4768
				dc.b  $0D,$21,$00,$A1,$00,$14,$0D,$01 ; 0x4770
				dc.b  $00,$FA,$00,$30,$02,$01,$00,$90 ; 0x4778
				dc.b  $03,$28,$00,$44,$03,$06,$00,$9C ; 0x4780
				dc.b  $00,$A5,$0E,$D0,$00,$DC,$0E,$87 ; 0x4788
				dc.b  $00,$A0,$03,$02,$00,$70,$03,$78 ; 0x4790
				dc.b  $00,$4C,$03,$03,$00,$3F,$00,$5C ; 0x4798
				dc.b  $00,$74,$02,$16,$00,$F3,$09,$76 ; 0x47a0
				dc.b  $00,$BA,$00,$01,$00,$C0,$01,$15 ; 0x47a8
				dc.b  $00,$33,$69,$00,$00,$02,$03,$1B ; 0x47b0
				dc.b  $00,$B3,$00,$F0,$17,$00,$FF,$00 ; 0x47b8
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0x47c0
				dc.b  $FF,$FF,$FF,$00,$81,$FF,$00,$FF ; 0x47c8
				dc.b  $5B,$FF,$00,$FF,$03,$00,$01,$1B ; 0x47d0
				dc.b  $00,$B3,$00,$F0,$17,$00,$FF,$00 ; 0x47d8
				dc.b  $FF,$FF,$FF,$00,$E0,$00,$00,$02 ; 0x47e0
				dc.b  $01,$00,$01,$1B,$00,$B3,$00,$F8 ; 0x47e8
				dc.b  $17,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x47f0
				dc.b  $FF,$00,$01,$14,$00,$80,$02,$A9 ; 0x47f8
				dc.b  $00,$5C,$02,$8A,$00,$36,$01,$02 ; 0x4800
				dc.b  $00,$F9,$00,$73,$01,$02,$00,$F0 ; 0x4808
				dc.b  $00,$AB,$01,$00,$00,$13,$00,$F2 ; 0x4810
				dc.b  $02,$50,$00,$1D,$00,$80,$01,$65 ; 0x4818
				dc.b  $00,$4C,$00,$00,$01,$8B,$00,$7D ; 0x4820
				dc.b  $BE,$08,$00,$81,$00,$40,$00,$FF ; 0x4828
				dc.b  $00,$F5,$00,$6E,$00,$00,$0A,$93 ; 0x4830
				dc.b  $00,$00,$0B,$00,$0E,$B5,$00,$2C ; 0x4838
				dc.b  $01,$80,$00,$E6,$00,$6C,$02,$80 ; 0x4840
				dc.b  $00,$00,$02,$32,$00,$2C,$02,$08 ; 0x4848
				dc.b  $00,$D8,$0D,$00,$00,$FF,$00,$FF ; 0x4850
				dc.b  $01,$00,$00,$FF,$00,$FF,$AC,$01 ; 0x4858
				dc.b  $00,$04,$02,$00,$00,$10,$19,$FF ; 0x4860
				dc.b  $0D,$F0,$02,$03,$00,$03,$02,$00 ; 0x4868
				dc.b  $00,$00,$00,$00,$02,$5A,$00,$D8 ; 0x4870
				dc.b  $02,$62,$00,$10,$02,$62,$00,$C3 ; 0x4878
				dc.b  $0A,$00,$00,$00,$02,$00,$00,$00 ; 0x4880
				dc.b  $AE,$03,$00,$6A,$01,$FF,$00,$F9 ; 0x4888
				dc.b  $00,$FE,$18,$00,$00,$58,$00,$45 ; 0x4890
				dc.b  $00,$29,$0D,$BF,$00,$5A,$00,$2B ; 0x4898
				dc.b  $02,$01,$00,$63,$09,$0A,$04,$00 ; 0x48a0
				dc.b  $0A,$F1,$00,$F2,$00,$C0,$0D,$42 ; 0x48a8
				dc.b  $00,$99,$00,$00,$02,$01,$00,$00 ; 0x48b0
				dc.b  $09,$1A,$00,$45,$00,$E0,$02,$00 ; 0x48b8
				dc.b  $00,$E0,$09,$50,$00,$7D,$00,$7B ; 0x48c0
				dc.b  $02,$06,$00,$23,$09,$BC,$00,$83 ; 0x48c8
				dc.b  $00,$94,$00,$90,$01,$09,$00,$13 ; 0x48d0
				dc.b  $0A,$A7,$00,$E8,$1A,$DA,$00,$C4 ; 0x48d8
				dc.b  $3B,$DA,$00,$C4,$02,$A7,$00,$E8 ; 0x48e0
				dc.b  $05,$00,$00,$00,$00,$00,$17,$00 ; 0x48e8
				dc.b  $01,$00,$23,$C0,$00,$00,$02,$C0 ; 0x48f0
				dc.b  $00,$00,$20,$08,$01,$08,$00,$00 ; 0x48f8
				dc.b  $00,$00,$02,$00,$03,$80,$00,$00 ; 0x4900
				dc.b  $02,$80,$00,$00,$14,$00,$00,$00 ; 0x4908
				dc.b  $00,$00,$06,$00,$00,$00,$02,$40 ; 0x4910
				dc.b  $00,$00,$02,$14,$00,$00,$1E,$00 ; 0x4918
				dc.b  $01,$03,$05,$C0,$00,$00,$02,$C0 ; 0x4920
				dc.b  $00,$00,$03,$08,$00,$00,$21,$08 ; 0x4928
				dc.b  $0B,$08,$00,$F0,$00,$00,$01,$08 ; 0x4930
				dc.b  $00,$F0,$00,$00,$03,$00,$01,$00 ; 0x4938
				dc.b  $00,$00,$00,$00,$00,$00,$00,$00 ; 0x4940
				dc.b  $00,$00,$00,$00,$07,$00,$03,$00 ; 0x4948
				dc.b  $25,$00,$00,$FF,$00,$FF,$01,$00 ; 0x4950
				dc.b  $00,$FF,$00,$FF,$45,$00,$00,$FF ; 0x4958
				dc.b  $00,$FF,$05,$00,$03,$00,$02,$00 ; 0x4960
				dc.b  $00,$00,$52,$00,$00,$00,$00,$FF ; 0x4968
				dc.b  $00,$FF,$09,$00,$03,$00,$19,$FF ; 0x4970
				dc.b  $25,$00,$00,$00,$02,$00,$00,$00 ; 0x4978
				dc.b  $52,$00,$03,$00,$00,$00,$51,$00 ; 0x4980
				dc.b  $00,$00,$0B,$00,$00,$00,$02,$00 ; 0x4988
				dc.b  $00,$00,$18,$00,$00,$00,$00,$40 ; 0x4990
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x4998
				dc.b  $0D,$00,$00,$00,$00,$00,$02,$00 ; 0x49a0
				dc.b  $00,$30,$03,$25,$00,$38,$03,$00 ; 0x49a8
				dc.b  $00,$00,$00,$00,$0E,$00,$00,$00 ; 0x49b0
				dc.b  $0E,$00,$00,$00,$03,$00,$00,$E0 ; 0x49b8
				dc.b  $03,$25,$00,$38,$03,$00,$00,$00 ; 0x49c0
				dc.b  $00,$00,$00,$00,$02,$00,$00,$C3 ; 0x49c8
				dc.b  $09,$00,$00,$00,$00,$00,$00,$00 ; 0x49d0
				dc.b  $01,$00,$00,$F3,$69,$FF,$00,$FF ; 0x49d8
				dc.b  $03,$01,$00,$78,$00,$C0,$17,$00 ; 0x49e0
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x49e8
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$81,$07 ; 0x49f0
				dc.b  $00,$EC,$5B,$07,$00,$EC,$03,$5A ; 0x49f8
				dc.b  $01,$01,$00,$78,$00,$98,$17,$00 ; 0x4a00
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$E0,$FF ; 0x4a08
				dc.b  $00,$FF,$01,$DE,$01,$01,$00,$78 ; 0x4a10
				dc.b  $00,$C0,$17,$00,$01,$14,$00,$80 ; 0x4a18
				dc.b  $22,$81,$00,$9C,$02,$4C,$00,$AA ; 0x4a20
				dc.b  $5B,$34,$00,$33,$02,$36,$00,$79 ; 0x4a28
				dc.b  $2C,$FF,$00,$FE,$00,$69,$00,$08 ; 0x4a30
				dc.b  $01,$00,$00,$4B,$00,$10,$19,$08 ; 0x4a38
				dc.b  $00,$57,$00,$C0,$00,$FF,$00,$FF ; 0x4a40
				dc.b  $00,$52,$02,$01,$00,$64,$00,$C0 ; 0x4a48
				dc.b  $01,$10,$00,$C0,$00,$E0,$08,$65 ; 0x4a50
				dc.b  $00,$21,$00,$93,$04,$01,$03,$0A ; 0x4a58
				dc.b  $01,$04,$01,$02,$25,$04,$00,$D3 ; 0x4a60
				dc.b  $00,$10,$01,$08,$00,$88,$00,$40 ; 0x4a68
				dc.b  $8D,$01,$03,$02,$1B,$10,$03,$20 ; 0x4a70
				dc.b  $03,$40,$03,$80,$19,$FF,$25,$58 ; 0x4a78
				dc.b  $00,$80,$02,$5F,$00,$A0,$8D,$FF ; 0x4a80
				dc.b  $00,$FF,$00,$22,$00,$80,$00,$FF ; 0x4a88
				dc.b  $00,$FF,$00,$E9,$1A,$01,$00,$18 ; 0x4a90
				dc.b  $02,$01,$00,$18,$02,$09,$00,$08 ; 0x4a98
				dc.b  $02,$09,$00,$08,$18,$00,$00,$52 ; 0x4aa0
				dc.b  $00,$8F,$00,$1F,$0D,$6B,$00,$08 ; 0x4aa8
				dc.b  $00,$2D,$0D,$28,$00,$16,$00,$80 ; 0x4ab0
				dc.b  $02,$05,$00,$20,$09,$98,$00,$8F ; 0x4ab8
				dc.b  $00,$80,$02,$06,$00,$F0,$09,$4D ; 0x4ac0
				dc.b  $00,$2F,$0E,$43,$00,$89,$00,$20 ; 0x4ac8
				dc.b  $0D,$3A,$00,$CA,$00,$CD,$0D,$49 ; 0x4ad0
				dc.b  $00,$48,$00,$6F,$00,$F0,$0D,$A7 ; 0x4ad8
				dc.b  $00,$E8,$1A,$DA,$00,$C4,$3B,$DA ; 0x4ae0
				dc.b  $00,$C4,$02,$A7,$00,$E8,$00,$00 ; 0x4ae8
				dc.b  $00,$02,$01,$00,$01,$1B,$00,$B3 ; 0x4af0
				dc.b  $00,$F0,$17,$00,$01,$10,$00,$00 ; 0x4af8
				dc.b  $02,$B7,$00,$90,$02,$AB,$00,$18 ; 0x4b00
				dc.b  $02,$8D,$00,$07,$02,$66,$00,$47 ; 0x4b08
				dc.b  $02,$02,$00,$50,$02,$58,$00,$50 ; 0x4b10
				dc.b  $00,$00,$01,$60,$00,$D8,$02,$7A ; 0x4b18
				dc.b  $00,$FC,$0B,$C4,$00,$2C,$03,$30 ; 0x4b20
				dc.b  $AD,$0F,$00,$FF,$00,$E0,$00,$00 ; 0x4b28
				dc.b  $00,$0B,$00,$49,$00,$E0,$0A,$87 ; 0x4b30
				dc.b  $0C,$00,$0E,$80,$00,$00,$02,$80 ; 0x4b38
				dc.b  $00,$00,$05,$00,$00,$FF,$00,$FF ; 0x4b40
				dc.b  $01,$00,$00,$FF,$00,$FF,$C0,$00 ; 0x4b48
				dc.b  $00,$00,$03,$00,$19,$FF,$0D,$35 ; 0x4b50
				dc.b  $00,$3F,$01,$02,$00,$31,$02,$FF ; 0x4b58
				dc.b  $00,$F8,$00,$E0,$02,$00,$00,$00 ; 0x4b60
				dc.b  $02,$00,$00,$00,$02,$49,$00,$0F ; 0x4b68
				dc.b  $BD,$FF,$00,$FE,$00,$DE,$01,$00 ; 0x4b70
				dc.b  $00,$00,$00,$44,$18,$00,$00,$03 ; 0x4b78
				dc.b  $00,$C7,$00,$3E,$0D,$51,$00,$7B ; 0x4b80
				dc.b  $00,$5A,$0E,$41,$0E,$B6,$00,$86 ; 0x4b88
				dc.b  $00,$80,$0D,$E3,$00,$EC,$03,$07 ; 0x4b90
				dc.b  $00,$D0,$09,$39,$00,$84,$00,$90 ; 0x4b98
				dc.b  $02,$09,$00,$50,$03,$79,$00,$50 ; 0x4ba0
				dc.b  $03,$04,$00,$B9,$00,$60,$00,$9A ; 0x4ba8
				dc.b  $02,$00,$00,$C3,$09,$E7,$00,$11 ; 0x4bb0
				dc.b  $00,$6F,$00,$E0,$01,$00,$00,$F3 ; 0x4bb8
				dc.b  $0A,$92,$00,$E5,$1A,$80,$00,$00 ; 0x4bc0
				dc.b  $3B,$80,$00,$00,$02,$92,$00,$E5 ; 0x4bc8
				dc.b  $1F,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x4bd0
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x4bd8
				dc.b  $FF,$00,$01,$10,$23,$5B,$00,$68 ; 0x4be0
				dc.b  $02,$5D,$00,$F0,$15,$FF,$00,$AA ; 0x4be8
				dc.b  $00,$20,$13,$00,$03,$00,$15,$FF ; 0x4bf0
				dc.b  $00,$C6,$00,$D2,$09,$FF,$00,$CD ; 0x4bf8
				dc.b  $00,$C0,$02,$15,$00,$74,$20,$01 ; 0x4c00
				dc.b  $31,$6A,$00,$40,$02,$03,$00,$EE ; 0x4c08
				dc.b  $00,$80,$01,$02,$00,$24,$02,$01 ; 0x4c10
				dc.b  $00,$D7,$00,$E0,$01,$05,$00,$E2 ; 0x4c18
				dc.b  $00,$20,$03,$02,$01,$01,$00,$77 ; 0x4c20
				dc.b  $00,$FF,$00,$65,$00,$21,$00,$87 ; 0x4c28
				dc.b  $08,$03,$03,$09,$71,$20,$00,$80 ; 0x4c30
				dc.b  $00,$20,$09,$20,$56,$01,$00,$02 ; 0x4c38
				dc.b  $00,$28,$00,$70,$09,$40,$03,$80 ; 0x4c40
				dc.b  $19,$FF,$7D,$40,$56,$FF,$00,$ED ; 0x4c48
				dc.b  $0B,$09,$00,$08,$02,$09,$00,$08 ; 0x4c50
				dc.b  $18,$00,$00,$71,$00,$33,$00,$9B ; 0x4c58
				dc.b  $0D,$03,$00,$3A,$00,$13,$02,$00 ; 0x4c60
				dc.b  $00,$B3,$08,$06,$00,$13,$00,$DC ; 0x4c68
				dc.b  $00,$10,$0D,$4F,$00,$70,$00,$40 ; 0x4c70
				dc.b  $0D,$69,$00,$EB,$0E,$5C,$00,$AD ; 0x4c78
				dc.b  $00,$A0,$0D,$50,$00,$AE,$00,$01 ; 0x4c80
				dc.b  $0D,$64,$00,$90,$00,$AF,$00,$B0 ; 0x4c88
				dc.b  $0D,$80,$00,$00,$5B,$80,$00,$00 ; 0x4c90
				dc.b  $03,$00,$01,$1B,$00,$B3,$00,$F0 ; 0x4c98
				dc.b  $17,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x4ca0
				dc.b  $FF,$00,$01,$00,$00,$00,$22,$C0 ; 0x4ca8
				dc.b  $00,$00,$02,$C0,$00,$00,$5B,$04 ; 0x4cb0
				dc.b  $00,$00,$02,$04,$00,$00,$2C,$00 ; 0x4cb8
				dc.b  $00,$00,$00,$10,$00,$00,$01,$02 ; 0x4cc0
				dc.b  $00,$00,$00,$00,$19,$01,$00,$00 ; 0x4cc8
				dc.b  $00,$00,$00,$00,$00,$01,$00,$00 ; 0x4cd0
				dc.b  $02,$08,$00,$F0,$00,$00,$01,$08 ; 0x4cd8
				dc.b  $00,$F0,$00,$00,$08,$00,$00,$00 ; 0x4ce0
				dc.b  $00,$00,$04,$00,$03,$00,$01,$00 ; 0x4ce8
				dc.b  $01,$00,$25,$00,$00,$FF,$00,$FF ; 0x4cf0
				dc.b  $01,$00,$00,$FF,$00,$FF,$8D,$00 ; 0x4cf8
				dc.b  $03,$00,$1B,$00,$03,$00,$03,$00 ; 0x4d00
				dc.b  $03,$00,$19,$FF,$25,$00,$00,$00 ; 0x4d08
				dc.b  $02,$00,$00,$00,$8D,$00,$00,$00 ; 0x4d10
				dc.b  $00,$00,$00,$00,$00,$00,$00,$00 ; 0x4d18
				dc.b  $00,$00,$1A,$00,$00,$00,$02,$00 ; 0x4d20
				dc.b  $00,$00,$02,$00,$00,$00,$02,$00 ; 0x4d28
				dc.b  $00,$00,$18,$00,$00,$00,$00,$40 ; 0x4d30
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x4d38
				dc.b  $0D,$00,$00,$00,$00,$00,$02,$00 ; 0x4d40
				dc.b  $00,$30,$09,$00,$00,$00,$00,$00 ; 0x4d48
				dc.b  $02,$00,$00,$C0,$09,$00,$00,$00 ; 0x4d50
				dc.b  $0E,$00,$00,$00,$00,$00,$0D,$00 ; 0x4d58
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0x4d60
				dc.b  $00,$00,$00,$00,$0D,$FF,$00,$FF ; 0x4d68
				dc.b  $1A,$80,$00,$00,$3B,$80,$00,$00 ; 0x4d70
				dc.b  $02,$FF,$00,$FF,$00,$FF,$00,$FF ; 0x4d78
				dc.b  $01,$DE,$01,$01,$00,$78,$00,$C0 ; 0x4d80
				dc.b  $17,$00,$01,$14,$00,$80,$09,$01 ; 0x4d88
				dc.b  $00,$DF,$00,$83,$01,$00,$00,$65 ; 0x4d90
				dc.b  $00,$D3,$02,$26,$00,$8A,$0A,$73 ; 0x4d98
				dc.b  $00,$DB,$BD,$FF,$00,$F8,$00,$44 ; 0x4da0
				dc.b  $00,$00,$01,$02,$00,$F2,$00,$C0 ; 0x4da8
				dc.b  $0A,$82,$0C,$00,$0E,$C5,$00,$6C ; 0x4db0
				dc.b  $02,$BB,$00,$B0,$0C,$00,$00,$00 ; 0x4db8
				dc.b  $BF,$10,$03,$20,$19,$FF,$0C,$01 ; 0x4dc0
				dc.b  $00,$23,$00,$1F,$01,$01,$00,$11 ; 0x4dc8
				dc.b  $00,$5F,$0E,$35,$00,$8F,$BF,$CC ; 0x4dd0
				dc.b  $03,$2C,$18,$00,$00,$0D,$00,$75 ; 0x4dd8
				dc.b  $00,$92,$0D,$5E,$00,$12,$00,$16 ; 0x4de0
				dc.b  $1D,$BD,$00,$55,$0E,$2A,$00,$D8 ; 0x4de8
				dc.b  $00,$40,$0D,$8E,$00,$0E,$00,$D0 ; 0x4df0
				dc.b  $07,$75,$00,$40,$03,$00,$00,$C0 ; 0x4df8
				dc.b  $00,$4A,$00,$D6,$0D,$EF,$00,$AF ; 0x4e00
				dc.b  $00,$FD,$00,$20,$0D,$DB,$00,$59 ; 0x4e08
				dc.b  $5B,$DB,$00,$59,$1F,$00,$FF,$00 ; 0x4e10
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0x4e18
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$01,$14 ; 0x4e20
				dc.b  $00,$80,$22,$60,$00,$60,$02,$5C ; 0x4e28
				dc.b  $00,$B8,$0E,$09,$00,$4C,$06,$80 ; 0x4e30
				dc.b  $00,$00,$00,$01,$09,$20,$00,$FB ; 0x4e38
				dc.b  $00,$FF,$21,$A8,$00,$A0,$09,$00 ; 0x4e40
				dc.b  $00,$40,$00,$00,$02,$08,$00,$30 ; 0x4e48
				dc.b  $1E,$01,$07,$5C,$00,$94,$02,$5C ; 0x4e50
				dc.b  $00,$D6,$03,$14,$00,$A7,$21,$52 ; 0x4e58
				dc.b  $00,$20,$01,$FF,$00,$FE,$00,$AA ; 0x4e60
				dc.b  $00,$00,$01,$00,$00,$ED,$00,$80 ; 0x4e68
				dc.b  $01,$00,$00,$3E,$00,$00,$00,$FF ; 0x4e70
				dc.b  $00,$FA,$00,$48,$00,$00,$05,$00 ; 0x4e78
				dc.b  $00,$29,$03,$82,$0C,$09,$72,$FF ; 0x4e80
				dc.b  $00,$FF,$09,$00,$63,$10,$03,$20 ; 0x4e88
				dc.b  $19,$FF,$7D,$00,$63,$00,$00,$5E ; 0x4e90
				dc.b  $01,$FF,$00,$FF,$00,$BE,$18,$00 ; 0x4e98
				dc.b  $00,$7A,$00,$E1,$00,$EF,$1D,$11 ; 0x4ea0
				dc.b  $00,$71,$0D,$06,$00,$56,$00,$3F ; 0x4ea8
				dc.b  $0E,$72,$00,$FF,$0E,$64,$00,$9F ; 0x4eb0
				dc.b  $00,$20,$0D,$57,$00,$98,$00,$3D ; 0x4eb8
				dc.b  $0D,$6D,$00,$2F,$00,$3C,$00,$F0 ; 0x4ec0
				dc.b  $8B,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x4ec8
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x4ed0
				dc.b  $FF,$00,$0D,$4F,$00,$3F,$01,$01 ; 0x4ed8
				dc.b  $00,$77,$00,$AB,$01,$FF,$00,$DD ; 0x4ee0
				dc.b  $00,$66,$02,$5A,$00,$EA,$02,$5D ; 0x4ee8
				dc.b  $00,$06,$02,$4D,$00,$F2,$BD,$00 ; 0x4ef0
				dc.b  $00,$05,$00,$A6,$00,$C0,$00,$FF ; 0x4ef8
				dc.b  $00,$FD,$00,$88,$00,$00,$0A,$8D ; 0x4f00
				dc.b  $0C,$00,$0D,$80,$00,$98,$00,$30 ; 0x4f08
				dc.b  $06,$FF,$00,$FF,$02,$80,$00,$00 ; 0x4f10
				dc.b  $04,$01,$00,$04,$00,$63,$00,$B4 ; 0x4f18
				dc.b  $DB,$FF,$0D,$32,$00,$AF,$02,$54 ; 0x4f20
				dc.b  $00,$6F,$06,$5E,$00,$8D,$00,$80 ; 0x4f28
				dc.b  $05,$3E,$00,$FF,$DC,$00,$00,$86 ; 0x4f30
				dc.b  $00,$71,$00,$35,$0D,$FB,$00,$64 ; 0x4f38
				dc.b  $00,$8F,$1D,$12,$00,$6B,$00,$C0 ; 0x4f40
				dc.b  $0D,$A1,$00,$2A,$00,$B0,$0D,$AE ; 0x4f48
				dc.b  $00,$8D,$00,$C0,$0D,$B1,$00,$A2 ; 0x4f50
				dc.b  $00,$AF,$02,$02,$00,$73,$09,$28 ; 0x4f58
				dc.b  $00,$77,$01,$50,$01,$03,$00,$03 ; 0x4f60
				dc.b  $0A,$64,$00,$87,$5B,$64,$00,$87 ; 0x4f68
				dc.b  $1F,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x4f70
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x4f78
				dc.b  $FF,$00,$01,$00,$00,$00,$22,$C0 ; 0x4f80
				dc.b  $00,$00,$02,$C0,$00,$00,$0E,$00 ; 0x4f88
				dc.b  $00,$00,$05,$00,$00,$00,$01,$00 ; 0x4f90
				dc.b  $09,$08,$00,$00,$00,$00,$06,$80 ; 0x4f98
				dc.b  $03,$80,$15,$00,$00,$00,$00,$00 ; 0x4fa0
				dc.b  $0E,$14,$00,$00,$1E,$00,$01,$03 ; 0x4fa8
				dc.b  $05,$C0,$00,$00,$02,$C0,$00,$00 ; 0x4fb0
				dc.b  $03,$08,$00,$00,$21,$08,$00,$00 ; 0x4fb8
				dc.b  $01,$00,$00,$01,$00,$00,$02,$01 ; 0x4fc0
				dc.b  $00,$00,$00,$00,$01,$08,$00,$F0 ; 0x4fc8
				dc.b  $01,$00,$00,$08,$00,$F0,$04,$00 ; 0x4fd0
				dc.b  $02,$00,$00,$00,$00,$00,$00,$00 ; 0x4fd8
				dc.b  $00,$00,$08,$00,$03,$00,$71,$00 ; 0x4fe0
				dc.b  $62,$00,$00,$00,$00,$FF,$00,$FF ; 0x4fe8
				dc.b  $09,$00,$03,$00,$19,$FF,$D4,$00 ; 0x4ff0
				dc.b  $00,$00,$0C,$00,$01,$00,$00,$00 ; 0x4ff8
				dc.b  $00,$00,$18,$00,$00,$00,$00,$40 ; 0x5000
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x5008
				dc.b  $02,$01,$00,$63,$08,$00,$00,$00 ; 0x5010
				dc.b  $00,$00,$00,$00,$0C,$00,$00,$00 ; 0x5018
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0x5020
				dc.b  $0E,$00,$00,$00,$00,$00,$0D,$00 ; 0x5028
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0x5030
				dc.b  $00,$00,$00,$00,$0D,$DB,$00,$59 ; 0x5038
				dc.b  $5B,$DB,$00,$59,$1F,$00,$01,$14 ; 0x5040
				dc.b  $00,$80,$22,$32,$00,$B8,$02,$5D ; 0x5048
				dc.b  $00,$F0,$22,$1D,$00,$0F,$00,$FF ; 0x5050
				dc.b  $28,$FF,$00,$BC,$00,$2A,$0F,$00 ; 0x5058
				dc.b  $16,$01,$01,$01,$05,$5C,$00,$2E ; 0x5060
				dc.b  $02,$5E,$00,$7A,$09,$FF,$00,$FF ; 0x5068
				dc.b  $00,$C3,$02,$00,$1A,$FF,$00,$F4 ; 0x5070
				dc.b  $00,$EA,$00,$80,$00,$FF,$00,$FE ; 0x5078
				dc.b  $00,$EF,$02,$01,$00,$E4,$02,$0A ; 0x5080
				dc.b  $00,$87,$00,$C0,$03,$03,$01,$03 ; 0x5088
				dc.b  $00,$23,$00,$FF,$00,$65,$00,$21 ; 0x5090
				dc.b  $00,$8D,$08,$0E,$01,$09,$01,$00 ; 0x5098
				dc.b  $25,$10,$00,$80,$00,$00,$02,$80 ; 0x50a0
				dc.b  $00,$00,$4D,$02,$67,$10,$03,$20 ; 0x50a8
				dc.b  $19,$FF,$25,$5D,$00,$D8,$02,$5E ; 0x50b0
				dc.b  $00,$20,$B6,$09,$00,$14,$02,$09 ; 0x50b8
				dc.b  $00,$14,$18,$00,$00,$79,$00,$7A ; 0x50c0
				dc.b  $00,$8E,$0D,$BC,$00,$3C,$00,$2A ; 0x50c8
				dc.b  $03,$03,$03,$78,$00,$4C,$03,$03 ; 0x50d0
				dc.b  $00,$15,$00,$50,$00,$A0,$0D,$55 ; 0x50d8
				dc.b  $00,$42,$00,$80,$0D,$71,$00,$AE ; 0x50e0
				dc.b  $0E,$63,$00,$78,$00,$40,$0D,$56 ; 0x50e8
				dc.b  $00,$97,$00,$8A,$0D,$6B,$00,$EF ; 0x50f0
				dc.b  $00,$44,$00,$E0,$0D,$DB,$00,$59 ; 0x50f8
				dc.b  $5B,$DB,$00,$59,$00,$00,$00,$02 ; 0x5100
				dc.b  $01,$00,$01,$1B,$00,$B3,$00,$F0 ; 0x5108
				dc.b  $17,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x5110
				dc.b  $FF,$00,$05,$A7,$00,$BE,$02,$8B ; 0x5118
				dc.b  $00,$CE,$01,$02,$00,$51,$00,$6F ; 0x5120
				dc.b  $01,$03,$00,$E6,$00,$CB,$01,$00 ; 0x5128
				dc.b  $00,$13,$00,$F2,$02,$60,$00,$21 ; 0x5130
				dc.b  $02,$5E,$00,$D2,$00,$80,$01,$9F ; 0x5138
				dc.b  $00,$4B,$0B,$B8,$00,$00,$03,$00 ; 0x5140
				dc.b  $AD,$0A,$00,$FB,$00,$00,$01,$FC ; 0x5148
				dc.b  $00,$3D,$00,$C0,$0A,$99,$0C,$00 ; 0x5150
				dc.b  $0D,$40,$00,$B5,$00,$2C,$02,$E6 ; 0x5158
				dc.b  $00,$6C,$02,$80,$00,$00,$01,$10 ; 0x5160
				dc.b  $00,$32,$00,$2C,$01,$20,$00,$08 ; 0x5168
				dc.b  $00,$D8,$02,$FF,$00,$FF,$BC,$01 ; 0x5170
				dc.b  $00,$04,$03,$10,$19,$FF,$0C,$02 ; 0x5178
				dc.b  $00,$F0,$00,$2F,$01,$03,$00,$03 ; 0x5180
				dc.b  $00,$FF,$01,$00,$00,$00,$00,$00 ; 0x5188
				dc.b  $02,$5A,$00,$D8,$00,$00,$01,$62 ; 0x5190
				dc.b  $00,$10,$02,$62,$00,$C3,$BD,$00 ; 0x5198
				dc.b  $00,$03,$00,$6A,$01,$FF,$00,$F9 ; 0x51a0
				dc.b  $00,$FE,$18,$00,$00,$4A,$00,$40 ; 0x51a8
				dc.b  $00,$B4,$0D,$AD,$00,$1F,$00,$DC ; 0x51b0
				dc.b  $0E,$40,$0E,$E8,$00,$17,$00,$00 ; 0x51b8
				dc.b  $0D,$35,$00,$74,$00,$00,$02,$01 ; 0x51c0
				dc.b  $00,$00,$09,$0E,$00,$C5,$00,$80 ; 0x51c8
				dc.b  $02,$00,$00,$E0,$09,$FF,$00,$D3 ; 0x51d0
				dc.b  $00,$6C,$02,$06,$00,$23,$09,$45 ; 0x51d8
				dc.b  $00,$3C,$00,$03,$00,$40,$01,$09 ; 0x51e0
				dc.b  $00,$13,$0A,$A7,$00,$E8,$1A,$DA ; 0x51e8
				dc.b  $00,$C4,$3B,$DA,$00,$C4,$02,$A7 ; 0x51f0
				dc.b  $00,$E8,$1F,$00,$01,$14,$00,$80 ; 0x51f8
				dc.b  $22,$97,$00,$FE,$02,$78,$00,$F0 ; 0x5200
				dc.b  $22,$1C,$00,$3B,$00,$FF,$06,$A7 ; 0x5208
				dc.b  $00,$1A,$02,$B8,$00,$F6,$03,$0A ; 0x5210
				dc.b  $02,$A5,$00,$16,$02,$A7,$00,$1A ; 0x5218
				dc.b  $02,$B8,$00,$F6,$0D,$25,$00,$FC ; 0x5220
				dc.b  $0B,$4C,$00,$84,$02,$23,$00,$BD ; 0x5228
				dc.b  $0D,$04,$00,$DB,$00,$FF,$05,$01 ; 0x5230
				dc.b  $07,$5D,$00,$A2,$02,$5F,$00,$E8 ; 0x5238
				dc.b  $03,$0A,$00,$31,$3C,$65,$00,$21 ; 0x5240
				dc.b  $00,$99,$04,$01,$03,$01,$01,$02 ; 0x5248
				dc.b  $01,$00,$55,$40,$03,$80,$03,$01 ; 0x5250
				dc.b  $03,$02,$03,$40,$03,$80,$0F,$08 ; 0x5258
				dc.b  $85,$FF,$56,$80,$03,$80,$03,$80 ; 0x5260
				dc.b  $03,$80,$03,$80,$03,$80,$94,$00 ; 0x5268
				dc.b  $00,$44,$00,$00,$00,$08,$0D,$58 ; 0x5270
				dc.b  $00,$19,$00,$98,$0D,$0B,$00,$E9 ; 0x5278
				dc.b  $00,$80,$07,$75,$00,$40,$04,$2F ; 0x5280
				dc.b  $00,$A6,$08,$78,$00,$4C,$03,$03 ; 0x5288
				dc.b  $00,$3F,$00,$88,$0E,$37,$00,$97 ; 0x5290
				dc.b  $08,$75,$00,$40,$04,$30,$00,$64 ; 0x5298
				dc.b  $00,$98,$0D,$3C,$00,$52,$00,$10 ; 0x52a0
				dc.b  $00,$80,$0D,$A7,$00,$E8,$1A,$DA ; 0x52a8
				dc.b  $00,$C4,$3B,$DA,$00,$C4,$02,$A7 ; 0x52b0
				dc.b  $00,$E8,$00,$00,$00,$02,$03,$1B ; 0x52b8
				dc.b  $00,$B3,$00,$F0,$17,$00,$FF,$00 ; 0x52c0
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0x52c8
				dc.b  $FF,$FF,$FF,$00,$81,$07,$00,$EC ; 0x52d0
				dc.b  $5B,$07,$00,$EC,$03,$5A,$01,$01 ; 0x52d8
				dc.b  $00,$78,$00,$98,$17,$00,$01,$00 ; 0x52e0
				dc.b  $00,$00,$22,$C0,$00,$00,$02,$C0 ; 0x52e8
				dc.b  $00,$00,$22,$08,$00,$00,$00,$00 ; 0x52f0
				dc.b  $28,$00,$00,$00,$00,$00,$0F,$04 ; 0x52f8
				dc.b  $16,$00,$01,$03,$05,$C0,$00,$00 ; 0x5300
				dc.b  $02,$C0,$00,$00,$09,$00,$00,$00 ; 0x5308
				dc.b  $00,$10,$02,$02,$1A,$00,$00,$01 ; 0x5310
				dc.b  $00,$00,$00,$00,$00,$00,$00,$01 ; 0x5318
				dc.b  $00,$00,$02,$08,$00,$F0,$02,$08 ; 0x5320
				dc.b  $00,$F0,$00,$00,$03,$00,$01,$00 ; 0x5328
				dc.b  $00,$00,$00,$00,$00,$00,$00,$00 ; 0x5330
				dc.b  $00,$00,$08,$00,$01,$00,$01,$00 ; 0x5338
				dc.b  $25,$00,$00,$FF,$00,$FF,$02,$FF ; 0x5340
				dc.b  $00,$FF,$4D,$00,$67,$00,$03,$00 ; 0x5348
				dc.b  $19,$FF,$25,$00,$00,$00,$02,$00 ; 0x5350
				dc.b  $00,$00,$B6,$00,$00,$00,$02,$00 ; 0x5358
				dc.b  $00,$00,$18,$00,$00,$00,$00,$40 ; 0x5360
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x5368
				dc.b  $03,$63,$03,$25,$00,$38,$03,$00 ; 0x5370
				dc.b  $00,$00,$00,$00,$00,$00,$0D,$00 ; 0x5378
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0x5380
				dc.b  $0E,$00,$00,$00,$00,$00,$0D,$00 ; 0x5388
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0x5390
				dc.b  $00,$00,$00,$00,$0D,$FF,$00,$FF ; 0x5398
				dc.b  $5B,$FF,$00,$FF,$00,$FF,$00,$FF ; 0x53a0
				dc.b  $01,$DE,$01,$01,$00,$78,$00,$C0 ; 0x53a8
				dc.b  $17,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x53b0
				dc.b  $FF,$00,$05,$B3,$00,$58,$02,$AE ; 0x53b8
				dc.b  $00,$D8,$01,$01,$00,$DF,$00,$DF ; 0x53c0
				dc.b  $01,$01,$00,$E0,$00,$1F,$02,$00 ; 0x53c8
				dc.b  $00,$00,$02,$59,$00,$EF,$00,$80 ; 0x53d0
				dc.b  $01,$60,$00,$6A,$02,$6F,$00,$EF ; 0x53d8
				dc.b  $0A,$56,$00,$88,$02,$60,$00,$D8 ; 0x53e0
				dc.b  $AE,$08,$00,$F0,$01,$00,$00,$08 ; 0x53e8
				dc.b  $00,$F0,$00,$00,$0A,$8C,$0C,$00 ; 0x53f0
				dc.b  $0D,$00,$00,$FF,$00,$FF,$01,$00 ; 0x53f8
				dc.b  $00,$FF,$00,$FF,$01,$00,$00,$FF ; 0x5400
				dc.b  $00,$FF,$01,$40,$00,$0A,$00,$FC ; 0x5408
				dc.b  $01,$80,$00,$0F,$00,$80,$00,$00 ; 0x5410
				dc.b  $00,$00,$BE,$00,$00,$00,$03,$00 ; 0x5418
				dc.b  $19,$FF,$0C,$00,$00,$00,$00,$00 ; 0x5420
				dc.b  $00,$00,$00,$00,$00,$00,$00,$00 ; 0x5428
				dc.b  $00,$00,$05,$59,$00,$A0,$02,$60 ; 0x5430
				dc.b  $00,$00,$02,$00,$00,$00,$00,$00 ; 0x5438
				dc.b  $BD,$00,$00,$00,$01,$00,$00,$00 ; 0x5440
				dc.b  $00,$00,$18,$00,$00,$52,$00,$F2 ; 0x5448
				dc.b  $00,$4C,$0D,$6B,$00,$89,$00,$24 ; 0x5450
				dc.b  $0C,$00,$00,$0E,$00,$8A,$00,$40 ; 0x5458
				dc.b  $0C,$00,$00,$3A,$00,$29,$0E,$4D ; 0x5460
				dc.b  $00,$8C,$0E,$43,$00,$DA,$0E,$3B ; 0x5468
				dc.b  $00,$11,$00,$A4,$02,$00,$00,$C3 ; 0x5470
				dc.b  $08,$08,$00,$49,$00,$A0,$00,$BC ; 0x5478
				dc.b  $00,$C0,$01,$00,$00,$F3,$08,$07 ; 0x5480
				dc.b  $01,$89,$00,$C5,$1A,$89,$00,$33 ; 0x5488
				dc.b  $3B,$89,$00,$33,$02,$89,$00,$C5 ; 0x5490
				dc.b  $1F,$00,$01,$00,$00,$00,$22,$C0 ; 0x5498
				dc.b  $00,$00,$02,$C0,$00,$00,$22,$08 ; 0x54a0
				dc.b  $00,$00,$00,$00,$06,$80,$00,$00 ; 0x54a8
				dc.b  $02,$80,$00,$00,$03,$00,$02,$80 ; 0x54b0
				dc.b  $00,$00,$02,$80,$00,$00,$02,$80 ; 0x54b8
				dc.b  $00,$00,$0D,$00,$00,$00,$0B,$04 ; 0x54c0
				dc.b  $00,$00,$02,$04,$00,$00,$0D,$00 ; 0x54c8
				dc.b  $00,$00,$00,$00,$05,$00,$07,$C0 ; 0x54d0
				dc.b  $00,$00,$02,$C0,$00,$00,$03,$08 ; 0x54d8
				dc.b  $00,$00,$3C,$00,$00,$00,$00,$00 ; 0x54e0
				dc.b  $04,$00,$03,$00,$01,$00,$01,$00 ; 0x54e8
				dc.b  $55,$00,$03,$00,$03,$00,$03,$00 ; 0x54f0
				dc.b  $03,$00,$03,$00,$0F,$00,$85,$FF ; 0x54f8
				dc.b  $56,$00,$03,$00,$03,$00,$03,$00 ; 0x5500
				dc.b  $03,$00,$03,$00,$94,$00,$00,$00 ; 0x5508
				dc.b  $00,$40,$00,$00,$0D,$00,$00,$00 ; 0x5510
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x5518
				dc.b  $07,$25,$00,$38,$04,$00,$00,$00 ; 0x5520
				dc.b  $08,$25,$00,$38,$03,$00,$00,$00 ; 0x5528
				dc.b  $00,$00,$0E,$00,$00,$00,$08,$25 ; 0x5530
				dc.b  $00,$38,$04,$00,$00,$00,$00,$00 ; 0x5538
				dc.b  $0D,$00,$00,$00,$00,$00,$00,$00 ; 0x5540
				dc.b  $0D,$FF,$00,$FF,$1A,$80,$00,$00 ; 0x5548
				dc.b  $3B,$80,$00,$00,$02,$FF,$00,$FF ; 0x5550
				dc.b  $00,$FF,$00,$FF,$03,$01,$00,$78 ; 0x5558
				dc.b  $00,$C0,$17,$00,$FF,$00,$FF,$FF ; 0x5560
				dc.b  $FF,$00,$FF,$00,$FF,$00,$FF,$FF ; 0x5568
				dc.b  $FF,$00,$FF,$00,$FF,$00,$FF,$FF ; 0x5570
				dc.b  $FF,$00,$81,$80,$00,$00,$5B,$00 ; 0x5578
				dc.b  $00,$00,$1F,$00,$01,$14,$00,$80 ; 0x5580
				dc.b  $16,$5D,$00,$F0,$02,$61,$00,$C8 ; 0x5588
				dc.b  $06,$9F,$01,$5A,$01,$B5,$00,$98 ; 0x5590
				dc.b  $00,$5A,$1F,$00,$00,$FF,$00,$FB ; 0x5598
				dc.b  $00,$80,$2A,$39,$00,$B8,$0F,$01 ; 0x55a0
				dc.b  $00,$B7,$15,$01,$01,$FF,$05,$B2 ; 0x55a8
				dc.b  $00,$14,$00,$59,$01,$B5,$00,$EC ; 0x55b0
				dc.b  $00,$5A,$02,$04,$00,$C6,$24,$FF ; 0x55b8
				dc.b  $00,$FD,$00,$9C,$00,$84,$01,$00 ; 0x55c0
				dc.b  $00,$57,$00,$FF,$00,$FF,$00,$FE ; 0x55c8
				dc.b  $00,$7B,$00,$C4,$01,$01,$00,$D9 ; 0x55d0
				dc.b  $00,$BF,$03,$03,$01,$04,$00,$97 ; 0x55d8
				dc.b  $00,$FD,$00,$65,$00,$21,$00,$8C ; 0x55e0
				dc.b  $08,$0E,$01,$09,$01,$00,$4D,$08 ; 0x55e8
				dc.b  $2B,$80,$67,$10,$03,$80,$00,$80 ; 0x55f0
				dc.b  $00,$00,$17,$FF,$E0,$FF,$00,$FD ; 0x55f8
				dc.b  $00,$BC,$00,$04,$02,$0B,$00,$FF ; 0x5600
				dc.b  $17,$00,$00,$B5,$00,$8E,$00,$44 ; 0x5608
				dc.b  $0D,$EB,$00,$C3,$00,$8C,$0D,$1F ; 0x5610
				dc.b  $00,$E0,$00,$C0,$0D,$7F,$00,$83 ; 0x5618
				dc.b  $0E,$AA,$00,$04,$0E,$94,$00,$C3 ; 0x5620
				dc.b  $00,$80,$0D,$81,$00,$81,$00,$0C ; 0x5628
				dc.b  $0D,$A1,$00,$6C,$00,$6C,$00,$40 ; 0x5630
				dc.b  $6F,$00,$1B,$00,$07,$59,$03,$04 ; 0x5638
				dc.b  $00,$02,$00,$03,$00,$67,$01,$02 ; 0x5640
				dc.b  $00,$4B,$00,$FB,$01,$FF,$00,$F0 ; 0x5648
				dc.b  $00,$BB,$00,$FF,$01,$5D,$00,$9A ; 0x5650
				dc.b  $00,$AC,$01,$61,$00,$D5,$00,$B0 ; 0x5658
				dc.b  $01,$7D,$00,$B3,$0C,$2B,$02,$DE ; 0x5660
				dc.b  $00,$30,$BE,$9F,$0C,$00,$0D,$10 ; 0x5668
				dc.b  $00,$B2,$00,$A0,$01,$20,$00,$80 ; 0x5670
				dc.b  $00,$00,$01,$02,$00,$23,$00,$D8 ; 0x5678
				dc.b  $02,$29,$00,$EC,$02,$22,$00,$AC ; 0x5680
				dc.b  $DF,$FF,$0C,$01,$00,$80,$00,$3F ; 0x5688
				dc.b  $00,$FF,$00,$01,$00,$F7,$00,$7F ; 0x5690
				dc.b  $00,$FF,$02,$FF,$00,$FF,$01,$58 ; 0x5698
				dc.b  $00,$F8,$00,$2C,$03,$30,$DF,$00 ; 0x56a0
				dc.b  $00,$88,$00,$E4,$00,$42,$0D,$B1 ; 0x56a8
				dc.b  $00,$AF,$00,$26,$0D,$18,$00,$06 ; 0x56b0
				dc.b  $00,$60,$0D,$60,$00,$19,$00,$80 ; 0x56b8
				dc.b  $0D,$80,$00,$22,$0E,$70,$00,$1D ; 0x56c0
				dc.b  $00,$C0,$0D,$61,$00,$99,$00,$E6 ; 0x56c8
				dc.b  $0D,$79,$00,$A8,$00,$48,$00,$20 ; 0x56d0
				dc.b  $0D,$A5,$00,$C3,$1A,$92,$00,$19 ; 0x56d8
				dc.b  $3B,$92,$00,$19,$02,$A5,$00,$C3 ; 0x56e0
				dc.b  $1F,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x56e8
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x56f0
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x56f8
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x5700
				dc.b  $FF,$00,$19,$5F,$00,$AC,$02,$5F ; 0x5708
				dc.b  $00,$CA,$2D,$00,$00,$12,$00,$BE ; 0x5710
				dc.b  $2A,$12,$00,$16,$0F,$00,$00,$00 ; 0x5718
				dc.b  $26,$01,$00,$C7,$25,$FF,$00,$56 ; 0x5720
				dc.b  $02,$01,$00,$39,$00,$3F,$00,$00 ; 0x5728
				dc.b  $00,$03,$00,$E6,$00,$80,$01,$05 ; 0x5730
				dc.b  $00,$55,$00,$A0,$05,$07,$00,$13 ; 0x5738
				dc.b  $00,$FF,$02,$9F,$0C,$00,$D9,$40 ; 0x5740
				dc.b  $00,$B6,$00,$7C,$01,$80,$00,$B9 ; 0x5748
				dc.b  $00,$1C,$1F,$FF,$D8,$FF,$00,$FD ; 0x5750
				dc.b  $00,$A8,$00,$04,$02,$67,$00,$FF ; 0x5758
				dc.b  $00,$00,$00,$08,$00,$A6,$00,$80 ; 0x5760
				dc.b  $01,$04,$00,$C5,$00,$00,$17,$00 ; 0x5768
				dc.b  $00,$EB,$00,$80,$00,$3A,$0D,$31 ; 0x5770
				dc.b  $00,$E9,$00,$8E,$0D,$29,$00,$5C ; 0x5778
				dc.b  $00,$E0,$0D,$A5,$00,$73,$00,$80 ; 0x5780
				dc.b  $0D,$DC,$00,$9A,$0E,$C1,$00,$06 ; 0x5788
				dc.b  $00,$C0,$0D,$A8,$00,$09,$00,$4E ; 0x5790
				dc.b  $0C,$08,$00,$D1,$00,$73,$00,$F7 ; 0x5798
				dc.b  $00,$A0,$0B,$07,$7F,$00,$01,$14 ; 0x57a0
				dc.b  $00,$80,$01,$01,$00,$48,$00,$38 ; 0x57a8
				dc.b  $01,$01,$00,$0F,$00,$38,$0E,$EF ; 0x57b0
				dc.b  $00,$28,$02,$C3,$00,$C0,$02,$73 ; 0x57b8
				dc.b  $00,$E3,$00,$FF,$5D,$00,$00,$65 ; 0x57c0
				dc.b  $00,$40,$0B,$01,$60,$65,$00,$21 ; 0x57c8
				dc.b  $00,$90,$08,$04,$01,$01,$01,$00 ; 0x57d0
				dc.b  $1A,$80,$00,$00,$02,$14,$00,$FC ; 0x57d8
				dc.b  $DF,$FF,$19,$EF,$00,$28,$02,$C0 ; 0x57e0
				dc.b  $00,$D8,$E0,$00,$00,$94,$00,$23 ; 0x57e8
				dc.b  $00,$8E,$0D,$C0,$00,$4F,$00,$4A ; 0x57f0
				dc.b  $0D,$1A,$01,$A0,$0D,$68,$00,$02 ; 0x57f8
				dc.b  $00,$80,$0D,$8A,$00,$AE,$0E,$79 ; 0x5800
				dc.b  $00,$58,$00,$40,$0D,$69,$00,$A2 ; 0x5808
				dc.b  $00,$8A,$0D,$83,$00,$AB,$00,$D4 ; 0x5810
				dc.b  $00,$E0,$71,$1B,$00,$C3,$00,$F0 ; 0x5818
				dc.b  $17,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x5820
				dc.b  $E0,$FF,$00,$FF,$03,$01,$00,$78 ; 0x5828
				dc.b  $00,$C0,$17,$00,$FF,$00,$FF,$FF ; 0x5830
				dc.b  $FF,$00,$E0,$FF,$00,$FF,$01,$DE ; 0x5838
				dc.b  $01,$01,$00,$78,$00,$C0,$17,$00 ; 0x5840
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$E5,$1B ; 0x5848
				dc.b  $00,$C3,$00,$F0,$17,$00,$01,$14 ; 0x5850
				dc.b  $00,$80,$19,$01,$00,$25,$00,$1C ; 0x5858
				dc.b  $06,$CD,$00,$6E,$02,$BF,$00,$B8 ; 0x5860
				dc.b  $20,$03,$01,$22,$00,$93,$00,$FF ; 0x5868
				dc.b  $29,$77,$00,$A0,$0F,$02,$00,$AB ; 0x5870
				dc.b  $15,$01,$10,$12,$00,$C3,$2D,$02 ; 0x5878
				dc.b  $00,$03,$00,$ED,$01,$0F,$00,$B7 ; 0x5880
				dc.b  $00,$AD,$03,$03,$01,$03,$00,$87 ; 0x5888
				dc.b  $00,$FE,$00,$65,$00,$21,$00,$90 ; 0x5890
				dc.b  $08,$0E,$01,$09,$01,$00,$26,$8E ; 0x5898
				dc.b  $00,$1C,$02,$A9,$00,$10,$B5,$10 ; 0x58a0
				dc.b  $03,$20,$19,$FF,$25,$BD,$00,$F0 ; 0x58a8
				dc.b  $02,$BD,$00,$A8,$B6,$09,$00,$13 ; 0x58b0
				dc.b  $00,$ED,$01,$09,$00,$13,$00,$ED ; 0x58b8
				dc.b  $17,$00,$00,$9C,$00,$AA,$00,$7D ; 0x58c0
				dc.b  $0D,$CB,$00,$65,$00,$E7,$0D,$1B ; 0x58c8
				dc.b  $00,$80,$00,$70,$0D,$6E,$00,$01 ; 0x58d0
				dc.b  $00,$C0,$0D,$92,$00,$AD,$0E,$80 ; 0x58d8
				dc.b  $00,$57,$00,$60,$0D,$6F,$00,$B9 ; 0x58e0
				dc.b  $00,$C7,$0D,$8B,$00,$43,$00,$61 ; 0x58e8
				dc.b  $00,$D0,$6C,$FF,$00,$FF,$03,$01 ; 0x58f0
				dc.b  $00,$78,$00,$C0,$17,$00,$FF,$00 ; 0x58f8
				dc.b  $FF,$FF,$FF,$00,$E0,$FF,$00,$FF ; 0x5900
				dc.b  $01,$DE,$01,$01,$00,$78,$00,$C0 ; 0x5908
				dc.b  $17,$00,$01,$10,$00,$00,$07,$14 ; 0x5910
				dc.b  $16,$79,$00,$47,$DC,$00,$FF,$FF ; 0x5918
				dc.b  $FF,$00,$00,$B7,$00,$07,$00,$C6 ; 0x5920
				dc.b  $0D,$ED,$00,$AE,$00,$72,$0D,$20 ; 0x5928
				dc.b  $00,$23,$00,$20,$0D,$80,$00,$8C ; 0x5930
				dc.b  $0E,$AB,$00,$66,$0E,$95,$00,$F9 ; 0x5938
				dc.b  $0E,$82,$00,$8E,$00,$B2,$0D,$A2 ; 0x5940
				dc.b  $00,$BC,$00,$88,$00,$60,$8B,$00 ; 0x5948
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x5950
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x5958
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x5960
				dc.b  $01,$10,$00,$00,$76,$9E,$00,$1E ; 0x5968
				dc.b  $66,$0D,$00,$E3,$00,$AD,$01,$07 ; 0x5970
				dc.b  $00,$B5,$00,$ED,$17,$00,$4A,$80 ; 0x5978
				dc.b  $00,$00,$2D,$10,$00,$80,$00,$00 ; 0x5980
				dc.b  $83,$FF,$4B,$03,$2D,$77,$00,$A0 ; 0x5988
				dc.b  $84,$00,$00,$BF,$00,$8E,$00,$B5 ; 0x5990
				dc.b  $0D,$F8,$00,$C5,$00,$0F,$0D,$21 ; 0x5998
				dc.b  $00,$A2,$00,$F0,$0D,$86,$00,$8B ; 0x59a0
				dc.b  $0E,$B3,$00,$65,$0E,$9C,$00,$F8 ; 0x59a8
				dc.b  $0E,$88,$00,$A5,$00,$EF,$0D,$AA ; 0x59b0
				dc.b  $00,$54,$00,$15,$00,$50,$8B,$00 ; 0x59b8
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x59c0
				dc.b  $01,$14,$00,$80,$06,$09,$00,$8C ; 0x59c8
				dc.b  $02,$84,$00,$FF,$00,$FF,$01,$84 ; 0x59d0
				dc.b  $00,$FF,$00,$FF,$05,$C0,$00,$D8 ; 0x59d8
				dc.b  $00,$60,$01,$C0,$00,$DE,$00,$60 ; 0x59e0
				dc.b  $01,$71,$00,$8F,$8D,$FF,$00,$FF ; 0x59e8
				dc.b  $00,$FF,$00,$16,$3E,$9F,$00,$1F ; 0x59f0
				dc.b  $0B,$00,$16,$80,$00,$00,$06,$09 ; 0x59f8
				dc.b  $00,$68,$DF,$FF,$1A,$3E,$00,$D8 ; 0x5a00
				dc.b  $03,$60,$DF,$00,$00,$47,$00,$DE ; 0x5a08
				dc.b  $00,$B9,$0D,$5D,$00,$21,$00,$DB ; 0x5a10
				dc.b  $0D,$0C,$00,$97,$00,$B0,$0D,$32 ; 0x5a18
				dc.b  $00,$5E,$00,$C0,$0D,$43,$00,$29 ; 0x5a20
				dc.b  $0E,$3A,$00,$C3,$00,$E0,$0D,$33 ; 0x5a28
				dc.b  $00,$28,$00,$3B,$0D,$3F,$00,$C4 ; 0x5a30
				dc.b  $00,$1D,$00,$90,$72,$F3,$18,$00 ; 0x5a38
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x5a40
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x5a48
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$81,$07 ; 0x5a50
				dc.b  $00,$EC,$5B,$07,$00,$EC,$03,$5A ; 0x5a58
				dc.b  $01,$01,$00,$78,$00,$98,$17,$00 ; 0x5a60
				dc.b  $01,$14,$00,$80,$19,$00,$00,$C0 ; 0x5a68
				dc.b  $00,$00,$06,$71,$00,$88,$00,$22 ; 0x5a70
				dc.b  $01,$BE,$00,$0E,$00,$5F,$1F,$04 ; 0x5a78
				dc.b  $01,$34,$00,$2F,$00,$F2,$29,$7D ; 0x5a80
				dc.b  $00,$FC,$0F,$00,$00,$00,$26,$0B ; 0x5a88
				dc.b  $00,$63,$25,$0F,$00,$C6,$00,$C0 ; 0x5a90
				dc.b  $01,$09,$00,$B4,$00,$6C,$01,$0F ; 0x5a98
				dc.b  $00,$A3,$02,$01,$00,$33,$0B,$9F ; 0x5aa0
				dc.b  $00,$1F,$0B,$00,$4A,$FF,$00,$FF ; 0x5aa8
				dc.b  $2D,$40,$00,$FF,$00,$FF,$83,$FF ; 0x5ab0
				dc.b  $27,$5E,$03,$5E,$1F,$00,$2D,$00 ; 0x5ab8
				dc.b  $00,$00,$84,$00,$00,$50,$00,$65 ; 0x5ac0
				dc.b  $00,$A8,$0D,$68,$00,$38,$00,$78 ; 0x5ac8
				dc.b  $0D,$0E,$00,$17,$00,$80,$0D,$38 ; 0x5ad0
				dc.b  $00,$5E,$00,$00,$0D,$4B,$00,$28 ; 0x5ad8
				dc.b  $0E,$41,$00,$C3,$00,$00,$0D,$39 ; 0x5ae0
				dc.b  $00,$3F,$00,$78,$0D,$47,$00,$5B ; 0x5ae8
				dc.b  $00,$AA,$00,$80,$8B,$00,$FF,$00 ; 0x5af0
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$04,$00 ; 0x5af8
				dc.b  $00,$A9,$02,$00,$00,$A3,$00,$F8 ; 0x5b00
				dc.b  $01,$02,$00,$2F,$00,$5B,$01,$02 ; 0x5b08
				dc.b  $00,$4C,$00,$A3,$01,$FE,$00,$FF ; 0x5b10
				dc.b  $00,$68,$00,$01,$01,$58,$00,$3E ; 0x5b18
				dc.b  $00,$00,$01,$64,$00,$AA,$00,$00 ; 0x5b20
				dc.b  $01,$75,$00,$63,$0A,$59,$00,$0A ; 0x5b28
				dc.b  $00,$2C,$01,$65,$00,$82,$00,$32 ; 0x5b30
				dc.b  $5B,$00,$09,$01,$16,$00,$00,$00 ; 0x5b38
				dc.b  $00,$08,$00,$00,$3E,$9C,$00,$00 ; 0x5b40
				dc.b  $0B,$00,$0D,$10,$00,$2C,$00,$50 ; 0x5b48
				dc.b  $01,$20,$00,$38,$00,$C0,$01,$04 ; 0x5b50
				dc.b  $00,$A4,$00,$A8,$02,$FF,$00,$FF ; 0x5b58
				dc.b  $02,$FF,$00,$FF,$DF,$FF,$0C,$02 ; 0x5b60
				dc.b  $00,$03,$00,$BF,$00,$FF,$00,$02 ; 0x5b68
				dc.b  $00,$2F,$00,$7F,$00,$FF,$00,$FF ; 0x5b70
				dc.b  $02,$01,$01,$00,$00,$00,$00,$00 ; 0x5b78
				dc.b  $01,$00,$00,$00,$00,$00,$DF,$00 ; 0x5b80
				dc.b  $00,$9B,$00,$23,$00,$1E,$0D,$C9 ; 0x5b88
				dc.b  $00,$68,$00,$FA,$0D,$1B,$00,$3B ; 0x5b90
				dc.b  $00,$A0,$0C,$08,$00,$6C,$00,$EE ; 0x5b98
				dc.b  $00,$80,$0D,$91,$00,$3E,$0E,$7F ; 0x5ba0
				dc.b  $00,$16,$00,$40,$0D,$6E,$00,$A2 ; 0x5ba8
				dc.b  $00,$3A,$0D,$89,$00,$E6,$00,$ED ; 0x5bb0
				dc.b  $00,$E0,$0D,$7F,$00,$15,$5B,$7F ; 0x5bb8
				dc.b  $00,$15,$06,$C3,$18,$00,$FF,$00 ; 0x5bc0
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0x5bc8
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0x5bd0
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$19,$60 ; 0x5bd8
				dc.b  $00,$AE,$02,$5F,$00,$52,$06,$65 ; 0x5be0
				dc.b  $00,$64,$00,$00,$01,$74,$00,$52 ; 0x5be8
				dc.b  $00,$00,$4D,$2D,$00,$78,$30,$60 ; 0x5bf0
				dc.b  $03,$60,$02,$04,$00,$F3,$25,$00 ; 0x5bf8
				dc.b  $00,$0E,$00,$80,$01,$02,$00,$2E ; 0x5c00
				dc.b  $00,$00,$01,$07,$00,$59,$00,$80 ; 0x5c08
				dc.b  $01,$04,$00,$E3,$00,$80,$0A,$9C ; 0x5c10
				dc.b  $00,$00,$0B,$00,$79,$00,$67,$00 ; 0x5c18
				dc.b  $03,$00,$19,$FF,$E2,$2C,$00,$00 ; 0x5c20
				dc.b  $02,$2C,$00,$00,$17,$00,$00,$D9 ; 0x5c28
				dc.b  $00,$F5,$00,$97,$0D,$1B,$00,$1A ; 0x5c30
				dc.b  $00,$15,$0D,$26,$00,$47,$00,$50 ; 0x5c38
				dc.b  $0D,$99,$00,$1D,$00,$40,$0D,$CC ; 0x5c40
				dc.b  $00,$27,$0E,$B2,$00,$A2,$00,$20 ; 0x5c48
				dc.b  $0D,$9B,$00,$81,$00,$B5,$0D,$C1 ; 0x5c50
				dc.b  $00,$D5,$00,$C7,$00,$70,$8B,$00 ; 0x5c58
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x5c60
				dc.b  $01,$10,$00,$00,$01,$01,$00,$54 ; 0x5c68
				dc.b  $00,$20,$02,$E8,$00,$A4,$02,$16 ; 0x5c70
				dc.b  $00,$1F,$02,$24,$00,$BF,$01,$00 ; 0x5c78
				dc.b  $00,$00,$00,$00,$00,$00,$01,$C0 ; 0x5c80
				dc.b  $00,$00,$02,$C0,$00,$00,$02,$78 ; 0x5c88
				dc.b  $00,$00,$00,$00,$09,$C0,$00,$00 ; 0x5c90
				dc.b  $00,$00,$01,$C0,$00,$00,$00,$00 ; 0x5c98
				dc.b  $4D,$1A,$00,$A0,$00,$78,$15,$00 ; 0x5ca0
				dc.b  $58,$86,$0C,$00,$0D,$00,$00,$FF ; 0x5ca8
				dc.b  $00,$FF,$01,$00,$00,$FF,$00,$FF ; 0x5cb0
				dc.b  $01,$00,$00,$FF,$00,$FF,$E7,$FF ; 0x5cb8
				dc.b  $0C,$00,$00,$00,$00,$00,$00,$00 ; 0x5cc0
				dc.b  $00,$00,$00,$00,$00,$00,$00,$00 ; 0x5cc8
				dc.b  $00,$00,$02,$00,$E7,$00,$00,$18 ; 0x5cd0
				dc.b  $00,$24,$00,$E8,$0D,$1F,$00,$12 ; 0x5cd8
				dc.b  $00,$38,$0D,$04,$00,$33,$00,$80 ; 0x5ce0
				dc.b  $0C,$00,$00,$10,$00,$CE,$00,$00 ; 0x5ce8
				dc.b  $0D,$16,$00,$68,$0E,$13,$00,$9B ; 0x5cf0
				dc.b  $00,$00,$0D,$11,$00,$11,$00,$38 ; 0x5cf8
				dc.b  $0D,$15,$00,$46,$00,$1E,$00,$80 ; 0x5d00
				dc.b  $0D,$80,$00,$00,$5B,$00,$00,$00 ; 0x5d08
				dc.b  $05,$00,$00,$00,$00,$F8,$17,$00 ; 0x5d10
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x5d18
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x5d20
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x5d28
				dc.b  $01,$10,$00,$00,$16,$BA,$00,$78 ; 0x5d30
				dc.b  $02,$BC,$00,$40,$05,$01,$00,$02 ; 0x5d38
				dc.b  $00,$12,$01,$01,$00,$32,$00,$7E ; 0x5d40
				dc.b  $4D,$FF,$00,$CD,$00,$94,$26,$00 ; 0x5d48
				dc.b  $01,$0F,$0E,$07,$00,$B3,$25,$0F ; 0x5d50
				dc.b  $00,$C6,$00,$C0,$01,$09,$00,$B4 ; 0x5d58
				dc.b  $00,$6C,$01,$06,$00,$DF,$00,$ED ; 0x5d60
				dc.b  $01,$07,$00,$27,$00,$ED,$06,$71 ; 0x5d68
				dc.b  $00,$FF,$02,$86,$0C,$00,$79,$40 ; 0x5d70
				dc.b  $67,$10,$03,$20,$19,$FF,$E2,$1F ; 0x5d78
				dc.b  $00,$ED,$02,$1F,$00,$ED,$17,$00 ; 0x5d80
				dc.b  $00,$B3,$00,$5B,$00,$34,$0D,$E8 ; 0x5d88
				dc.b  $00,$E7,$00,$5C,$0D,$1F,$00,$7D ; 0x5d90
				dc.b  $00,$C0,$0D,$7D,$00,$F7,$00,$00 ; 0x5d98
				dc.b  $0D,$A7,$00,$F4,$0E,$92,$00,$F5 ; 0x5da0
				dc.b  $00,$80,$0D,$7F,$00,$EE,$00,$DC ; 0x5da8
				dc.b  $0D,$9F,$00,$77,$00,$1B,$00,$40 ; 0x5db0
				dc.b  $8B,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x5db8
				dc.b  $FF,$00,$05,$25,$00,$28,$01,$01 ; 0x5dc0
				dc.b  $00,$08,$00,$9C,$01,$01,$00,$E1 ; 0x5dc8
				dc.b  $00,$BF,$01,$01,$00,$E5,$00,$3F ; 0x5dd0
				dc.b  $01,$FF,$00,$FF,$00,$E0,$03,$06 ; 0x5dd8
				dc.b  $02,$BF,$00,$68,$00,$80,$01,$73 ; 0x5de0
				dc.b  $00,$6B,$00,$FF,$CE,$8E,$00,$06 ; 0x5de8
				dc.b  $0B,$00,$0E,$34,$00,$AC,$02,$1B ; 0x5df0
				dc.b  $00,$10,$02,$15,$00,$F8,$01,$04 ; 0x5df8
				dc.b  $00,$1E,$00,$BB,$01,$08,$00,$38 ; 0x5e00
				dc.b  $00,$98,$DF,$FF,$0C,$02,$00,$06 ; 0x5e08
				dc.b  $00,$BF,$00,$FF,$00,$02,$00,$12 ; 0x5e10
				dc.b  $00,$3F,$00,$FF,$00,$FF,$00,$FF ; 0x5e18
				dc.b  $00,$E0,$02,$BF,$00,$58,$02,$BF ; 0x5e20
				dc.b  $00,$88,$E0,$00,$00,$88,$00,$42 ; 0x5e28
				dc.b  $00,$2A,$0D,$B0,$00,$DC,$00,$5E ; 0x5e30
				dc.b  $0D,$17,$00,$E9,$00,$E0,$0C,$08 ; 0x5e38
				dc.b  $00,$5F,$00,$A7,$00,$80,$0C,$07 ; 0x5e40
				dc.b  $00,$7F,$00,$8A,$0E,$6F,$00,$98 ; 0x5e48
				dc.b  $00,$C0,$0D,$61,$00,$26,$00,$1E ; 0x5e50
				dc.b  $0D,$79,$00,$17,$00,$F6,$00,$A0 ; 0x5e58
				dc.b  $0D,$87,$00,$92,$1A,$7F,$00,$46 ; 0x5e60
				dc.b  $22,$00,$09,$00,$09,$00,$04,$7F ; 0x5e68
				dc.b  $00,$46,$02,$87,$00,$92,$01,$02 ; 0x5e70
				dc.b  $03,$1B,$00,$C3,$00,$F0,$01,$02 ; 0x5e78
				dc.b  $01,$0F,$13,$00,$FF,$00,$FF,$FF ; 0x5e80
				dc.b  $FF,$00,$FF,$00,$FF,$00,$FF,$FF ; 0x5e88
				dc.b  $FF,$00,$FF,$00,$FF,$00,$FF,$FF ; 0x5e90
				dc.b  $FF,$00,$FF,$00,$19,$BF,$00,$58 ; 0x5e98
				dc.b  $02,$C1,$00,$26,$05,$00,$00,$BF ; 0x5ea0
				dc.b  $00,$58,$01,$00,$00,$C1,$00,$50 ; 0x5ea8
				dc.b  $4D,$00,$00,$0B,$00,$42,$0F,$03 ; 0x5eb0
				dc.b  $00,$16,$17,$FF,$34,$FF,$00,$F8 ; 0x5eb8
				dc.b  $00,$F4,$00,$00,$00,$FF,$00,$FF ; 0x5ec0
				dc.b  $00,$CC,$00,$00,$01,$0D,$00,$47 ; 0x5ec8
				dc.b  $00,$C0,$01,$06,$00,$D4,$00,$00 ; 0x5ed0
				dc.b  $05,$02,$00,$37,$03,$8E,$00,$06 ; 0x5ed8
				dc.b  $0B,$00,$FF,$FF,$E2,$38,$00,$00 ; 0x5ee0
				dc.b  $02,$38,$00,$00,$17,$00,$00,$23 ; 0x5ee8
				dc.b  $00,$78,$00,$76,$0D,$7A,$00,$B1 ; 0x5ef0
				dc.b  $00,$82,$0D,$33,$00,$34,$00,$20 ; 0x5ef8
				dc.b  $0D,$CC,$00,$D0,$00,$80,$0D,$11 ; 0x5f00
				dc.b  $00,$16,$0E,$EE,$00,$F3,$00,$40 ; 0x5f08
				dc.b  $0D,$D0,$00,$03,$00,$C2,$0D,$03 ; 0x5f10
				dc.b  $00,$48,$00,$F3,$00,$60,$8B,$00 ; 0x5f18
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x5f20
				dc.b  $01,$14,$00,$80,$06,$1B,$00,$50 ; 0x5f28
				dc.b  $02,$F9,$00,$7F,$02,$EA,$00,$EF ; 0x5f30
				dc.b  $06,$BF,$00,$83,$00,$80,$02,$58 ; 0x5f38
				dc.b  $00,$00,$01,$79,$00,$1F,$CF,$9A ; 0x5f40
				dc.b  $00,$00,$0B,$00,$1A,$06,$00,$AB ; 0x5f48
				dc.b  $01,$00,$E1,$FF,$FF,$00,$00,$9E ; 0x5f50
				dc.b  $00,$69,$00,$50,$0D,$CD,$00,$AA ; 0x5f58
				dc.b  $00,$F0,$0D,$1B,$00,$CF,$00,$00 ; 0x5f60
				dc.b  $0D,$6F,$00,$3C,$00,$00,$0D,$94 ; 0x5f68
				dc.b  $00,$50,$0E,$81,$00,$C6,$00,$00 ; 0x5f70
				dc.b  $0D,$70,$00,$F8,$00,$F0,$0D,$8C ; 0x5f78
				dc.b  $00,$D1,$00,$35,$00,$00,$0D,$88 ; 0x5f80
				dc.b  $00,$C5,$1A,$80,$00,$CE,$3B,$80 ; 0x5f88
				dc.b  $00,$CE,$02,$88,$00,$C5,$1F,$00 ; 0x5f90
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x5f98
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x5fa0
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x5fa8
				dc.b  $01,$14,$00,$80,$75,$FF,$00,$95 ; 0x5fb0
				dc.b  $00,$28,$28,$0F,$3D,$03,$00,$34 ; 0x5fb8
				dc.b  $00,$00,$01,$0E,$00,$EB,$00,$C0 ; 0x5fc0
				dc.b  $0A,$9A,$00,$00,$0B,$00,$FF,$FF ; 0x5fc8
				dc.b  $E2,$44,$03,$44,$18,$00,$00,$39 ; 0x5fd0
				dc.b  $00,$9F,$00,$9C,$0D,$97,$00,$80 ; 0x5fd8
				dc.b  $00,$14,$0D,$37,$00,$19,$00,$40 ; 0x5fe0
				dc.b  $0D,$DC,$00,$65,$00,$00,$0D,$25 ; 0x5fe8
				dc.b  $00,$DC,$0E,$01,$00,$20,$00,$80 ; 0x5ff0
				dc.b  $0D,$DF,$00,$D6,$00,$94,$0D,$17 ; 0x5ff8
				dc.b  $00,$02,$00,$31,$00,$C0,$8B,$00 ; 0x6000
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x6008
				dc.b  $09,$14,$00,$B4,$01,$02,$00,$12 ; 0x6010
				dc.b  $00,$FF,$02,$D2,$00,$3F,$0A,$C7 ; 0x6018
				dc.b  $00,$50,$D3,$81,$0C,$00,$FF,$FF ; 0x6020
				dc.b  $FF,$00,$00,$D0,$00,$D5,$00,$18 ; 0x6028
				dc.b  $0D,$0F,$00,$3B,$00,$C8,$0D,$24 ; 0x6030
				dc.b  $00,$AC,$00,$80,$0D,$92,$00,$B2 ; 0x6038
				dc.b  $0E,$C3,$00,$98,$0E,$AB,$00,$25 ; 0x6040
				dc.b  $0E,$94,$00,$FC,$00,$C8,$0D,$B9 ; 0x6048
				dc.b  $00,$B5,$00,$81,$00,$80,$8B,$00 ; 0x6050
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x6058
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x6060
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x6068
				dc.b  $28,$01,$00,$17,$00,$C0,$4E,$9B ; 0x6070
				dc.b  $00,$D8,$0F,$05,$00,$5D,$17,$01 ; 0x6078
				dc.b  $3D,$0F,$00,$63,$00,$C0,$01,$09 ; 0x6080
				dc.b  $00,$53,$06,$05,$00,$39,$03,$81 ; 0x6088
				dc.b  $0C,$00,$FF,$FF,$FF,$00,$00,$6C ; 0x6090
				dc.b  $00,$0B,$00,$64,$0D,$D9,$00,$10 ; 0x6098
				dc.b  $00,$EC,$0D,$3F,$00,$F6,$00,$C0 ; 0x60a0
				dc.b  $0D,$FF,$00,$DB,$0E,$55,$00,$24 ; 0x60a8
				dc.b  $0E,$2A,$00,$7F,$0E,$03,$00,$DA ; 0x60b0
				dc.b  $00,$6C,$0D,$43,$00,$E6,$00,$7E ; 0x60b8
				dc.b  $00,$40,$8B,$00,$FF,$00,$FF,$FF ; 0x60c0
				dc.b  $FF,$00,$FF,$00,$05,$43,$00,$58 ; 0x60c8
				dc.b  $02,$06,$00,$BC,$02,$0D,$00,$3F ; 0x60d0
				dc.b  $02,$D7,$00,$7F,$07,$D7,$02,$B5 ; 0x60d8
				dc.b  $00,$80,$02,$7F,$00,$0D,$CF,$9B ; 0x60e0
				dc.b  $0C,$00,$20,$01,$00,$02,$00,$E0 ; 0x60e8
				dc.b  $00,$33,$DB,$FF,$21,$48,$00,$BF ; 0x60f0
				dc.b  $00,$FF,$DB,$00,$00,$9D,$00,$49 ; 0x60f8
				dc.b  $00,$61,$0D,$8C,$00,$3E,$00,$40 ; 0x6100
				dc.b  $0C,$06,$00,$48,$00,$9F,$00,$30 ; 0x6108
				dc.b  $0D,$22,$00,$7C,$00,$C0,$0D,$83 ; 0x6110
				dc.b  $00,$51,$0E,$52,$00,$E6,$00,$E0 ; 0x6118
				dc.b  $0D,$27,$00,$06,$00,$B3,$0D,$6F ; 0x6120
				dc.b  $00,$BE,$00,$18,$00,$10,$0D,$99 ; 0x6128
				dc.b  $00,$A4,$5B,$99,$00,$A4,$1F,$00 ; 0x6130
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x6138
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x6140
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x6148
				dc.b  $24,$01,$00,$18,$00,$AA,$02,$2B ; 0x6150
				dc.b  $00,$88,$4E,$B1,$00,$AE,$0F,$04 ; 0x6158
				dc.b  $00,$CC,$17,$0C,$34,$00,$00,$0A ; 0x6160
				dc.b  $00,$23,$00,$C0,$00,$00,$00,$01 ; 0x6168
				dc.b  $00,$96,$00,$60,$01,$0C,$00,$A6 ; 0x6170
				dc.b  $00,$40,$01,$0E,$00,$65,$00,$40 ; 0x6178
				dc.b  $05,$03,$00,$8B,$03,$9B,$0C,$00 ; 0x6180
				dc.b  $D9,$20,$02,$04,$00,$04,$00,$80 ; 0x6188
				dc.b  $00,$00,$02,$90,$00,$6F,$02,$C9 ; 0x6190
				dc.b  $00,$87,$17,$FF,$D9,$02,$00,$44 ; 0x6198
				dc.b  $03,$02,$02,$0F,$00,$FF,$00,$E0 ; 0x61a0
				dc.b  $01,$08,$00,$32,$00,$80,$17,$00 ; 0x61a8
				dc.b  $00,$38,$00,$7F,$00,$AD,$0D,$E2 ; 0x61b0
				dc.b  $00,$EE,$00,$77,$0D,$59,$00,$42 ; 0x61b8
				dc.b  $00,$70,$02,$02,$00,$A0,$08,$06 ; 0x61c0
				dc.b  $00,$8F,$00,$A5,$00,$C0,$0D,$14 ; 0x61c8
				dc.b  $00,$DD,$0E,$D2,$00,$41,$00,$60 ; 0x61d0
				dc.b  $0D,$95,$00,$E4,$00,$57,$0D,$F9 ; 0x61d8
				dc.b  $00,$EF,$00,$14,$00,$D0,$8B,$00 ; 0x61e0
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$81,$84 ; 0x61e8
				dc.b  $00,$D1,$1B,$CE,$3C,$CE,$02,$84 ; 0x61f0
				dc.b  $00,$D1,$00,$00,$00,$02,$01,$00 ; 0x61f8
				dc.b  $01,$1B,$00,$C3,$00,$F0,$17,$00 ; 0x6200
				dc.b  $01,$14,$00,$80,$06,$DF,$00,$F8 ; 0x6208
				dc.b  $0E,$EF,$00,$28,$03,$90,$02,$73 ; 0x6210
				dc.b  $00,$E3,$00,$FF,$5D,$00,$00,$65 ; 0x6218
				dc.b  $00,$40,$0B,$01,$60,$65,$00,$21 ; 0x6220
				dc.b  $00,$95,$08,$04,$01,$01,$01,$00 ; 0x6228
				dc.b  $1A,$80,$00,$00,$02,$14,$00,$FC ; 0x6230
				dc.b  $DF,$FF,$19,$EF,$00,$28,$02,$C0 ; 0x6238
				dc.b  $00,$D8,$E0,$00,$00,$C5,$00,$00 ; 0x6240
				dc.b  $00,$7F,$0D,$4C,$00,$BE,$00,$4D ; 0x6248
				dc.b  $0D,$4F,$00,$9A,$00,$D0,$0D,$3E ; 0x6250
				dc.b  $00,$6B,$00,$40,$0D,$A8,$00,$8F ; 0x6258
				dc.b  $0E,$73,$00,$7D,$00,$20,$0D,$43 ; 0x6260
				dc.b  $00,$64,$00,$ED,$0D,$93,$00,$1A ; 0x6268
				dc.b  $00,$45,$00,$F0,$0D,$8A,$00,$6B ; 0x6270
				dc.b  $1A,$FF,$00,$FF,$3B,$FF,$00,$FF ; 0x6278
				dc.b  $02,$8A,$00,$6B,$02,$11,$00,$E9 ; 0x6280
				dc.b  $1B,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x6288
				dc.b  $E0,$FF,$00,$FF,$03,$01,$00,$78 ; 0x6290
				dc.b  $00,$C0,$17,$00,$01,$14,$00,$80 ; 0x6298
				dc.b  $21,$01,$00,$1B,$00,$12,$00,$80 ; 0x62a0
				dc.b  $01,$FE,$00,$A4,$00,$80,$0D,$0C ; 0x62a8
				dc.b  $00,$48,$06,$2E,$00,$30,$13,$25 ; 0x62b0
				dc.b  $00,$BB,$02,$00,$26,$03,$00,$D5 ; 0x62b8
				dc.b  $20,$C3,$3C,$FF,$00,$FA,$00,$10 ; 0x62c0
				dc.b  $02,$03,$00,$95,$00,$C0,$03,$02 ; 0x62c8
				dc.b  $04,$65,$00,$21,$00,$95,$08,$03 ; 0x62d0
				dc.b  $03,$09,$25,$01,$03,$02,$56,$02 ; 0x62d8
				dc.b  $00,$04,$00,$FD,$00,$1C,$5D,$10 ; 0x62e0
				dc.b  $03,$20,$19,$FF,$25,$C0,$03,$C0 ; 0x62e8
				dc.b  $B6,$FF,$00,$FF,$00,$80,$01,$FF ; 0x62f0
				dc.b  $00,$FB,$00,$F8,$18,$00,$00,$47 ; 0x62f8
				dc.b  $00,$4B,$00,$8F,$0D,$5C,$00,$62 ; 0x6300
				dc.b  $00,$7D,$0E,$44,$00,$D0,$02,$01 ; 0x6308
				dc.b  $00,$E0,$08,$06,$00,$31,$00,$F7 ; 0x6310
				dc.b  $00,$40,$0D,$42,$00,$9F,$0E,$3A ; 0x6318
				dc.b  $00,$4B,$00,$20,$0D,$32,$00,$BF ; 0x6320
				dc.b  $00,$1D,$0D,$3F,$00,$41,$00,$16 ; 0x6328
				dc.b  $00,$F0,$71,$1B,$00,$F3,$00,$F8 ; 0x6330
				dc.b  $17,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x6338
				dc.b  $E5,$1B,$00,$C3,$00,$F0,$17,$00 ; 0x6340
				dc.b  $01,$14,$00,$80,$36,$39,$00,$59 ; 0x6348
				dc.b  $05,$FF,$00,$F2,$00,$0D,$13,$1D ; 0x6350
				dc.b  $00,$D3,$02,$2B,$00,$7F,$0A,$CD ; 0x6358
				dc.b  $00,$CF,$19,$00,$12,$FF,$00,$87 ; 0x6360
				dc.b  $00,$D8,$22,$01,$00,$BA,$00,$70 ; 0x6368
				dc.b  $01,$01,$00,$A6,$00,$C8,$0D,$09 ; 0x6370
				dc.b  $00,$CB,$00,$C0,$11,$10,$00,$B7 ; 0x6378
				dc.b  $00,$C0,$01,$02,$00,$20,$09,$65 ; 0x6380
				dc.b  $00,$21,$00,$95,$08,$0D,$01,$06 ; 0x6388
				dc.b  $01,$00,$39,$01,$07,$02,$12,$01 ; 0x6390
				dc.b  $00,$04,$00,$DA,$00,$48,$61,$40 ; 0x6398
				dc.b  $03,$01,$0E,$01,$00,$04,$13,$40 ; 0x63a0
				dc.b  $03,$80,$19,$FF,$40,$FF,$00,$F9 ; 0x63a8
				dc.b  $00,$7C,$13,$20,$00,$DF,$60,$FF ; 0x63b0
				dc.b  $00,$FF,$00,$CE,$00,$80,$00,$FF ; 0x63b8
				dc.b  $00,$FF,$00,$DC,$0E,$0A,$00,$3E ; 0x63c0
				dc.b  $12,$09,$00,$08,$02,$09,$00,$08 ; 0x63c8
				dc.b  $18,$00,$00,$36,$00,$0D,$00,$B3 ; 0x63d0
				dc.b  $06,$19,$00,$6F,$00,$0C,$03,$05 ; 0x63d8
				dc.b  $00,$92,$00,$DB,$00,$A9,$06,$19 ; 0x63e0
				dc.b  $00,$6F,$00,$0C,$03,$05,$00,$0D ; 0x63e8
				dc.b  $00,$C7,$00,$90,$02,$02,$00,$60 ; 0x63f0
				dc.b  $08,$06,$00,$D9,$00,$E2,$00,$40 ; 0x63f8
				dc.b  $0D,$22,$00,$83,$0E,$FE,$00,$32 ; 0x6400
				dc.b  $00,$A0,$0D,$DD,$00,$49,$00,$C9 ; 0x6408
				dc.b  $0D,$13,$00,$D4,$00,$81,$00,$30 ; 0x6410
				dc.b  $71,$1B,$00,$C3,$00,$F0,$17,$00 ; 0x6418
				dc.b  $01,$14,$00,$80,$22,$C4,$00,$B6 ; 0x6420
				dc.b  $02,$F6,$00,$75,$09,$01,$00,$AD ; 0x6428
				dc.b  $00,$7F,$00,$FF,$38,$FF,$00,$FE ; 0x6430
				dc.b  $00,$68,$01,$FF,$00,$FF,$00,$DC ; 0x6438
				dc.b  $00,$80,$05,$2B,$00,$78,$6C,$03 ; 0x6440
				dc.b  $01,$03,$00,$65,$00,$FF,$00,$65 ; 0x6448
				dc.b  $00,$21,$00,$95,$08,$0C,$01,$05 ; 0x6450
				dc.b  $01,$00,$25,$40,$03,$80,$D5,$FF ; 0x6458
				dc.b  $25,$BC,$00,$E8,$02,$96,$00,$78 ; 0x6460
				dc.b  $D4,$00,$00,$39,$00,$69,$00,$39 ; 0x6468
				dc.b  $0D,$97,$00,$39,$00,$5B,$0D,$37 ; 0x6470
				dc.b  $00,$0F,$00,$B0,$0D,$DC,$00,$3E ; 0x6478
				dc.b  $00,$C0,$0D,$25,$00,$A9,$0F,$F3 ; 0x6480
				dc.b  $00,$E0,$0D,$DF,$00,$AF,$00,$BB ; 0x6488
				dc.b  $0C,$08,$00,$16,$00,$D4,$00,$C5 ; 0x6490
				dc.b  $00,$90,$0B,$07,$65,$1B,$00,$C3 ; 0x6498
				dc.b  $00,$F0,$17,$00,$01,$10,$00,$00 ; 0x64a0
				dc.b  $01,$01,$00,$2C,$00,$A8,$01,$01 ; 0x64a8
				dc.b  $00,$0B,$00,$06,$E7,$93,$0C,$00 ; 0x64b0
				dc.b  $FF,$FF,$FF,$00,$00,$D3,$00,$DE ; 0x64b8
				dc.b  $00,$80,$0D,$60,$00,$13,$00,$80 ; 0x64c0
				dc.b  $0D,$52,$00,$38,$00,$00,$0D,$48 ; 0x64c8
				dc.b  $00,$E0,$00,$00,$0D,$B6,$00,$80 ; 0x64d0
				dc.b  $0E,$7F,$00,$B0,$00,$00,$0D,$4E ; 0x64d8
				dc.b  $00,$03,$00,$80,$0D,$A0,$00,$56 ; 0x64e0
				dc.b  $00,$E8,$00,$00,$0D,$80,$00,$00 ; 0x64e8
				dc.b  $1A,$80,$00,$00,$3B,$80,$00,$00 ; 0x64f0
				dc.b  $02,$80,$00,$00,$02,$00,$00,$00 ; 0x64f8
				dc.b  $1B,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x6500
				dc.b  $FF,$00,$01,$00,$00,$00,$21,$00 ; 0x6508
				dc.b  $00,$C0,$00,$00,$00,$00,$01,$C0 ; 0x6510
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0x6518
				dc.b  $06,$00,$00,$00,$13,$80,$00,$00 ; 0x6520
				dc.b  $02,$80,$26,$14,$00,$00,$20,$03 ; 0x6528
				dc.b  $3C,$00,$00,$08,$00,$F0,$02,$08 ; 0x6530
				dc.b  $00,$F0,$00,$00,$03,$00,$04,$00 ; 0x6538
				dc.b  $00,$00,$00,$00,$08,$00,$03,$00 ; 0x6540
				dc.b  $25,$00,$03,$00,$56,$00,$00,$00 ; 0x6548
				dc.b  $00,$FF,$00,$FF,$5D,$00,$03,$00 ; 0x6550
				dc.b  $19,$FF,$25,$00,$03,$00,$B6,$00 ; 0x6558
				dc.b  $00,$00,$00,$00,$01,$00,$00,$00 ; 0x6560
				dc.b  $00,$00,$18,$00,$00,$00,$00,$40 ; 0x6568
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x6570
				dc.b  $0E,$00,$00,$00,$02,$00,$00,$30 ; 0x6578
				dc.b  $08,$00,$00,$00,$00,$00,$00,$00 ; 0x6580
				dc.b  $0D,$00,$00,$00,$0E,$00,$00,$00 ; 0x6588
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x6590
				dc.b  $0D,$00,$00,$00,$00,$00,$00,$00 ; 0x6598
				dc.b  $6C,$FF,$00,$FF,$01,$DE,$01,$01 ; 0x65a0
				dc.b  $00,$78,$00,$C0,$17,$00,$01,$10 ; 0x65a8
				dc.b  $23,$E0,$00,$50,$00,$80,$01,$DF ; 0x65b0
				dc.b  $00,$0C,$00,$80,$2A,$19,$00,$E7 ; 0x65b8
				dc.b  $02,$00,$15,$FF,$00,$E8,$00,$80 ; 0x65c0
				dc.b  $0E,$31,$00,$8C,$52,$43,$00,$D0 ; 0x65c8
				dc.b  $0A,$03,$00,$E5,$00,$80,$00,$FF ; 0x65d0
				dc.b  $00,$FD,$00,$8A,$00,$80,$03,$02 ; 0x65d8
				dc.b  $04,$65,$00,$21,$00,$93,$08,$03 ; 0x65e0
				dc.b  $03,$09,$25,$02,$00,$80,$00,$00 ; 0x65e8
				dc.b  $01,$01,$00,$80,$00,$00,$45,$10 ; 0x65f0
				dc.b  $00,$80,$00,$00,$60,$04,$00,$04 ; 0x65f8
				dc.b  $00,$36,$00,$B4,$27,$FF,$25,$CE ; 0x6600
				dc.b  $00,$88,$02,$C0,$AA,$FF,$00,$F7 ; 0x6608
				dc.b  $29,$00,$00,$64,$00,$1C,$00,$F0 ; 0x6610
				dc.b  $02,$04,$00,$31,$09,$2F,$00,$0F ; 0x6618
				dc.b  $00,$D0,$0E,$3E,$00,$F0,$02,$01 ; 0x6620
				dc.b  $00,$E0,$08,$06,$00,$19,$00,$74 ; 0x6628
				dc.b  $0E,$21,$00,$F0,$0E,$1D,$00,$B2 ; 0x6630
				dc.b  $0E,$19,$00,$D9,$00,$D0,$0D,$20 ; 0x6638
				dc.b  $00,$38,$00,$EF,$73,$F3,$00,$F8 ; 0x6640
				dc.b  $17,$00,$01,$10,$00,$00,$36,$24 ; 0x6648
				dc.b  $00,$4E,$06,$C3,$00,$7A,$11,$FF ; 0x6650
				dc.b  $00,$FF,$00,$F6,$00,$3C,$60,$FF ; 0x6658
				dc.b  $00,$FE,$00,$BD,$00,$60,$01,$00 ; 0x6660
				dc.b  $00,$FE,$00,$70,$0D,$03,$00,$FD ; 0x6668
				dc.b  $00,$E0,$11,$04,$00,$C3,$00,$80 ; 0x6670
				dc.b  $01,$0C,$00,$3E,$00,$60,$0A,$93 ; 0x6678
				dc.b  $0C,$00,$FF,$FF,$FF,$00,$00,$44 ; 0x6680
				dc.b  $00,$EB,$00,$B4,$07,$6E,$00,$F4 ; 0x6688
				dc.b  $04,$A6,$00,$30,$00,$DC,$07,$6E ; 0x6690
				dc.b  $00,$F4,$14,$E4,$00,$57,$00,$00 ; 0x6698
				dc.b  $0D,$30,$00,$74,$0E,$0A,$00,$65 ; 0x66a0
				dc.b  $00,$80,$0D,$E7,$00,$E8,$00,$5C ; 0x66a8
				dc.b  $0D,$21,$00,$11,$00,$23,$00,$40 ; 0x66b0
				dc.b  $8B,$00,$01,$10,$00,$00,$22,$7F ; 0x66b8
				dc.b  $00,$92,$02,$C9,$00,$F0,$09,$00 ; 0x66c0
				dc.b  $00,$D0,$00,$3F,$1F,$91,$00,$8F ; 0x66c8
				dc.b  $02,$88,$00,$7F,$14,$00,$00,$00 ; 0x66d0
				dc.b  $00,$18,$01,$00,$00,$05,$00,$FA ; 0x66d8
				dc.b  $00,$00,$05,$3D,$00,$A0,$6E,$04 ; 0x66e0
				dc.b  $00,$E5,$03,$93,$0C,$00,$25,$00 ; 0x66e8
				dc.b  $03,$00,$4B,$80,$89,$FF,$25,$00 ; 0x66f0
				dc.b  $00,$00,$02,$00,$00,$00,$4A,$05 ; 0x66f8
				dc.b  $00,$FA,$88,$00,$00,$48,$00,$47 ; 0x6700
				dc.b  $00,$3A,$0D,$AA,$00,$8E,$00,$8E ; 0x6708
				dc.b  $0D,$39,$00,$AC,$00,$E0,$0D,$E6 ; 0x6710
				dc.b  $00,$B3,$00,$80,$0D,$33,$00,$9A ; 0x6718
				dc.b  $0E,$0D,$00,$26,$00,$C0,$0D,$EA ; 0x6720
				dc.b  $00,$4E,$00,$4E,$0C,$00,$00,$24 ; 0x6728
				dc.b  $00,$0E,$00,$67,$00,$A0,$0B,$08 ; 0x6730
				dc.b  $7F,$00,$01,$14,$00,$80,$02,$3D ; 0x6738
				dc.b  $00,$28,$02,$0A,$00,$6A,$E7,$84 ; 0x6740
				dc.b  $0C,$00,$FF,$FF,$FF,$00,$00,$35 ; 0x6748
				dc.b  $00,$2F,$00,$E2,$0D,$DE,$00,$A0 ; 0x6750
				dc.b  $00,$06,$0D,$63,$00,$54,$00,$60 ; 0x6758
				dc.b  $0D,$8D,$00,$51,$00,$80,$0D,$11 ; 0x6760
				dc.b  $00,$C2,$0E,$CF,$00,$89,$00,$C0 ; 0x6768
				dc.b  $0D,$93,$00,$86,$00,$C6,$0D,$F6 ; 0x6770
				dc.b  $00,$FC,$00,$42,$00,$20,$0D,$81 ; 0x6778
				dc.b  $00,$CA,$1A,$85,$00,$16,$3B,$85 ; 0x6780
				dc.b  $00,$16,$02,$81,$00,$CA,$02,$08 ; 0x6788
				dc.b  $00,$8A,$1B,$00,$FF,$00,$FF,$FF ; 0x6790
				dc.b  $FF,$00,$FF,$00,$01,$14,$00,$80 ; 0x6798
				dc.b  $22,$C1,$00,$54,$00,$80,$01,$C3 ; 0x67a0
				dc.b  $00,$CD,$00,$80,$15,$7F,$00,$FF ; 0x67a8
				dc.b  $13,$00,$03,$00,$16,$2E,$00,$EE ; 0x67b0
				dc.b  $0E,$17,$00,$5C,$5E,$02,$00,$B8 ; 0x67b8
				dc.b  $02,$03,$00,$AE,$04,$02,$04,$65 ; 0x67c0
				dc.b  $00,$21,$00,$84,$08,$03,$03,$09 ; 0x67c8
				dc.b  $25,$40,$03,$80,$46,$02,$00,$04 ; 0x67d0
				dc.b  $00,$80,$00,$00,$8B,$FF,$25,$C0 ; 0x67d8
				dc.b  $03,$C0,$D5,$00,$00,$20,$00,$AF ; 0x67e0
				dc.b  $00,$0A,$0D,$2A,$00,$2C,$00,$FE ; 0x67e8
				dc.b  $0D,$01,$00,$42,$00,$10,$02,$02 ; 0x67f0
				dc.b  $00,$70,$08,$06,$00,$16,$00,$CF ; 0x67f8
				dc.b  $00,$80,$0D,$1E,$00,$6A,$0E,$1A ; 0x6800
				dc.b  $00,$9C,$00,$C0,$0D,$17,$00,$2A ; 0x6808
				dc.b  $00,$BE,$0C,$08,$00,$1C,$00,$E0 ; 0x6810
				dc.b  $00,$84,$00,$A0,$0B,$07,$41,$00 ; 0x6818
				dc.b  $09,$00,$09,$00,$04,$80,$03,$80 ; 0x6820
				dc.b  $01,$00,$00,$02,$01,$00,$01,$1B ; 0x6828
				dc.b  $00,$F3,$00,$F8,$01,$02,$01,$0F ; 0x6830
				dc.b  $13,$00,$01,$00,$23,$C0,$00,$00 ; 0x6838
				dc.b  $00,$00,$01,$C0,$00,$00,$00,$00 ; 0x6840
				dc.b  $2A,$80,$00,$00,$02,$80,$15,$00 ; 0x6848
				dc.b  $00,$00,$00,$00,$0E,$14,$00,$00 ; 0x6850
				dc.b  $52,$08,$00,$00,$0A,$08,$00,$F0 ; 0x6858
				dc.b  $00,$00,$00,$00,$00,$08,$00,$F0 ; 0x6860
				dc.b  $00,$00,$03,$00,$04,$00,$00,$00 ; 0x6868
				dc.b  $00,$00,$08,$00,$03,$00,$25,$00 ; 0x6870
				dc.b  $00,$FF,$00,$FF,$01,$00,$00,$FF ; 0x6878
				dc.b  $00,$FF,$45,$00,$00,$FF,$00,$FF ; 0x6880
				dc.b  $60,$00,$00,$00,$00,$FF,$00,$FF ; 0x6888
				dc.b  $27,$FF,$25,$00,$00,$00,$02,$00 ; 0x6890
				dc.b  $AA,$00,$00,$00,$29,$00,$00,$00 ; 0x6898
				dc.b  $00,$40,$00,$00,$02,$01,$00,$11 ; 0x68a0
				dc.b  $09,$00,$00,$00,$00,$00,$0E,$00 ; 0x68a8
				dc.b  $00,$00,$02,$00,$00,$30,$08,$00 ; 0x68b0
				dc.b  $00,$00,$00,$00,$0E,$00,$00,$00 ; 0x68b8
				dc.b  $0E,$00,$00,$00,$0E,$00,$00,$00 ; 0x68c0
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x68c8
				dc.b  $73,$C3,$00,$F0,$17,$00,$01,$14 ; 0x68d0
				dc.b  $00,$80,$35,$FF,$00,$D7,$00,$B9 ; 0x68d8
				dc.b  $05,$00,$00,$14,$00,$81,$13,$FA ; 0x68e0
				dc.b  $00,$64,$02,$5C,$00,$A7,$5C,$00 ; 0x68e8
				dc.b  $00,$00,$00,$BE,$00,$D8,$00,$FF ; 0x68f0
				dc.b  $00,$FE,$00,$99,$00,$C8,$0E,$CD ; 0x68f8
				dc.b  $00,$40,$11,$0C,$00,$C9,$00,$60 ; 0x6900
				dc.b  $01,$01,$00,$50,$00,$20,$0A,$84 ; 0x6908
				dc.b  $0C,$00,$FF,$FF,$56,$26,$00,$53 ; 0x6910
				dc.b  $A7,$00,$00,$A6,$00,$3D,$00,$16 ; 0x6918
				dc.b  $0D,$24,$00,$BD,$00,$62,$1D,$28 ; 0x6920
				dc.b  $00,$C8,$00,$80,$0D,$8B,$00,$B6 ; 0x6928
				dc.b  $0E,$5A,$00,$3F,$00,$40,$0D,$2D ; 0x6930
				dc.b  $00,$6B,$00,$A2,$0D,$77,$00,$B6 ; 0x6938
				dc.b  $00,$7D,$00,$60,$8B,$00,$01,$14 ; 0x6940
				dc.b  $00,$80,$32,$71,$00,$1F,$14,$04 ; 0x6948
				dc.b  $01,$19,$00,$13,$00,$FF,$06,$80 ; 0x6950
				dc.b  $00,$00,$02,$80,$00,$00,$15,$45 ; 0x6958
				dc.b  $00,$28,$01,$FF,$00,$FF,$00,$E6 ; 0x6960
				dc.b  $02,$29,$00,$B0,$02,$37,$00,$C8 ; 0x6968
				dc.b  $22,$01,$4B,$03,$00,$65,$03,$84 ; 0x6970
				dc.b  $0C,$00,$75,$00,$89,$FF,$75,$00 ; 0x6978
				dc.b  $00,$00,$88,$00,$00,$A9,$00,$98 ; 0x6980
				dc.b  $00,$9C,$0D,$29,$00,$1B,$00,$14 ; 0x6988
				dc.b  $0D,$4A,$00,$C9,$00,$40,$0D,$2B ; 0x6990
				dc.b  $00,$25,$00,$00,$0D,$8E,$00,$DC ; 0x6998
				dc.b  $0E,$5D,$00,$00,$00,$80,$0D,$2F ; 0x69a0
				dc.b  $00,$D1,$00,$94,$0D,$7A,$00,$B3 ; 0x69a8
				dc.b  $00,$C1,$00,$C0,$0B,$00,$7F,$00 ; 0x69b0
				dc.b  $05,$3E,$00,$18,$02,$0E,$00,$1E ; 0x69b8
				dc.b  $16,$7C,$00,$37,$CF,$8B,$00,$0A ; 0x69c0
				dc.b  $0B,$00,$FF,$FF,$FF,$00,$00,$E3 ; 0x69c8
				dc.b  $00,$25,$00,$03,$0D,$73,$00,$F0 ; 0x69d0
				dc.b  $00,$99,$0D,$65,$00,$68,$00,$D0 ; 0x69d8
				dc.b  $02,$01,$00,$40,$08,$06,$00,$53 ; 0x69e0
				dc.b  $00,$9E,$00,$40,$0D,$C4,$00,$D3 ; 0x69e8
				dc.b  $0E,$8C,$00,$38,$00,$A0,$0D,$58 ; 0x69f0
				dc.b  $00,$EC,$00,$B9,$0D,$AD,$00,$F0 ; 0x69f8
				dc.b  $00,$96,$00,$30,$0D,$80,$00,$00 ; 0x6a00
				dc.b  $1A,$80,$00,$00,$3B,$80,$00,$00 ; 0x6a08
				dc.b  $02,$80,$00,$00,$02,$1A,$00,$06 ; 0x6a10
				dc.b  $01,$1B,$00,$F3,$00,$F8,$17,$00 ; 0x6a18
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x6a20
				dc.b  $25,$AE,$00,$9D,$02,$A6,$00,$12 ; 0x6a28
				dc.b  $16,$5F,$00,$80,$08,$00,$01,$07 ; 0x6a30
				dc.b  $00,$FF,$00,$FF,$06,$13,$00,$7F ; 0x6a38
				dc.b  $18,$FF,$00,$E2,$00,$14,$06,$72 ; 0x6a40
				dc.b  $00,$3C,$06,$04,$00,$91,$1E,$01 ; 0x6a48
				dc.b  $01,$A5,$0E,$0D,$00,$10,$2C,$FF ; 0x6a50
				dc.b  $00,$FD,$00,$29,$01,$FF,$00,$F7 ; 0x6a58
				dc.b  $00,$D5,$00,$80,$06,$D7,$00,$FF ; 0x6a60
				dc.b  $02,$8B,$00,$0A,$0B,$09,$25,$01 ; 0x6a68
				dc.b  $00,$80,$00,$00,$01,$02,$00,$80 ; 0x6a70
				dc.b  $00,$00,$1E,$80,$00,$00,$24,$00 ; 0x6a78
				dc.b  $00,$40,$07,$20,$06,$08,$00,$04 ; 0x6a80
				dc.b  $00,$89,$00,$78,$7B,$FF,$FF,$00 ; 0x6a88
				dc.b  $00,$1E,$00,$F0,$00,$37,$0D,$27 ; 0x6a90
				dc.b  $00,$E7,$00,$F5,$0D,$00,$00,$44 ; 0x6a98
				dc.b  $00,$A0,$02,$00,$00,$30,$09,$15 ; 0x6aa0
				dc.b  $00,$95,$00,$40,$0D,$1C,$00,$C7 ; 0x6aa8
				dc.b  $0E,$19,$00,$2E,$00,$20,$0D,$15 ; 0x6ab0
				dc.b  $00,$EB,$00,$95,$0C,$00,$00,$1B ; 0x6ab8
				dc.b  $00,$52,$00,$B1,$00,$70,$0B,$00 ; 0x6ac0
				dc.b  $7F,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x6ac8
				dc.b  $FF,$00,$38,$00,$00,$2E,$00,$7F ; 0x6ad0
				dc.b  $06,$31,$00,$B0,$13,$E6,$00,$53 ; 0x6ad8
				dc.b  $02,$69,$00,$67,$0A,$7B,$00,$4F ; 0x6ae0
				dc.b  $2D,$E0,$00,$A0,$21,$FF,$00,$FE ; 0x6ae8
				dc.b  $00,$04,$00,$D0,$00,$00,$00,$01 ; 0x6af0
				dc.b  $00,$4F,$00,$F8,$0C,$FF,$00,$FA ; 0x6af8
				dc.b  $00,$45,$00,$80,$11,$01,$00,$E1 ; 0x6b00
				dc.b  $00,$40,$01,$02,$00,$2A,$00,$E0 ; 0x6b08
				dc.b  $0A,$8B,$00,$0A,$0B,$00,$56,$FF ; 0x6b10
				dc.b  $00,$FF,$A7,$FF,$42,$80,$13,$13 ; 0x6b18
				dc.b  $00,$9B,$74,$FF,$00,$FF,$00,$EE ; 0x6b20
				dc.b  $00,$80,$2F,$00,$00,$54,$00,$32 ; 0x6b28
				dc.b  $00,$37,$0D,$BA,$00,$0D,$00,$F5 ; 0x6b30
				dc.b  $0E,$C1,$0E,$EF,$00,$15,$00,$40 ; 0x6b38
				dc.b  $0D,$3E,$00,$C7,$0E,$16,$00,$EE ; 0x6b40
				dc.b  $00,$20,$0D,$F2,$00,$D1,$00,$95 ; 0x6b48
				dc.b  $0D,$2E,$00,$AA,$00,$D1,$00,$70 ; 0x6b50
				dc.b  $8B,$00,$25,$96,$00,$E1,$02,$40 ; 0x6b58
				dc.b  $00,$30,$00,$80,$09,$88,$00,$9F ; 0x6b60
				dc.b  $14,$10,$01,$0F,$00,$2F,$07,$88 ; 0x6b68
				dc.b  $00,$7F,$18,$FF,$00,$E1,$00,$D4 ; 0x6b70
				dc.b  $03,$E4,$02,$00,$00,$00,$01,$FF ; 0x6b78
				dc.b  $00,$B7,$00,$20,$24,$01,$4E,$8B ; 0x6b80
				dc.b  $00,$0A,$0B,$00,$25,$40,$03,$80 ; 0x6b88
				dc.b  $47,$10,$00,$82,$00,$80,$09,$20 ; 0x6b90
				dc.b  $81,$FF,$25,$3A,$00,$3E,$02,$8F ; 0x6b98
				dc.b  $00,$10,$45,$FF,$00,$FE,$00,$60 ; 0x6ba0
				dc.b  $0A,$2B,$00,$80,$80,$00,$00,$57 ; 0x6ba8
				dc.b  $00,$8D,$00,$BD,$0D,$BE,$00,$6B ; 0x6bb0
				dc.b  $00,$A7,$0D,$3C,$00,$5C,$00,$70 ; 0x6bb8
				dc.b  $0D,$F1,$00,$71,$00,$C0,$0D,$41 ; 0x6bc0
				dc.b  $00,$ED,$0E,$19,$00,$AF,$00,$60 ; 0x6bc8
				dc.b  $0D,$F5,$00,$37,$00,$87,$0D,$31 ; 0x6bd0
				dc.b  $00,$A8,$00,$15,$00,$D0,$8B,$00 ; 0x6bd8
				dc.b  $01,$10,$00,$00,$02,$46,$00,$70 ; 0x6be0
				dc.b  $02,$0C,$00,$26,$16,$4C,$00,$FB ; 0x6be8
				dc.b  $CF,$8F,$00,$00,$0B,$00,$FF,$FF ; 0x6bf0
				dc.b  $FF,$00,$00,$FB,$00,$BC,$00,$02 ; 0x6bf8
				dc.b  $0D,$93,$00,$EA,$00,$66,$1D,$64 ; 0x6c00
				dc.b  $00,$E9,$00,$80,$0D,$DB,$00,$E2 ; 0x6c08
				dc.b  $0E,$A0,$00,$65,$00,$C0,$0D,$6A ; 0x6c10
				dc.b  $00,$7D,$00,$26,$0D,$C3,$00,$D5 ; 0x6c18
				dc.b  $00,$44,$00,$20,$4D,$00,$09,$00 ; 0x6c20
				dc.b  $09,$00,$0B,$02,$00,$00,$00,$00 ; 0x6c28
				dc.b  $01,$1C,$00,$03,$02,$02,$01,$0F ; 0x6c30
				dc.b  $13,$00,$01,$10,$23,$81,$00,$54 ; 0x6c38
				dc.b  $02,$BF,$00,$76,$5F,$00,$18,$0F ; 0x6c40
				dc.b  $35,$03,$00,$9F,$02,$03,$00,$24 ; 0x6c48
				dc.b  $00,$80,$01,$03,$00,$5A,$00,$40 ; 0x6c50
				dc.b  $00,$FF,$00,$FF,$00,$DA,$04,$03 ; 0x6c58
				dc.b  $01,$02,$00,$C9,$00,$FF,$00,$65 ; 0x6c60
				dc.b  $00,$21,$00,$8F,$08,$0E,$01,$09 ; 0x6c68
				dc.b  $01,$00,$E1,$01,$1D,$FF,$E0,$FF ; 0x6c70
				dc.b  $00,$FF,$00,$CC,$1C,$00,$00,$26 ; 0x6c78
				dc.b  $00,$12,$00,$F8,$0D,$31,$00,$2F ; 0x6c80
				dc.b  $00,$68,$0D,$06,$00,$A6,$00,$80 ; 0x6c88
				dc.b  $0D,$1A,$00,$9A,$0E,$23,$00,$78 ; 0x6c90
				dc.b  $0E,$1F,$00,$09,$0E,$1B,$00,$04 ; 0x6c98
				dc.b  $00,$68,$0D,$21,$00,$AD,$00,$1F ; 0x6ca0
				dc.b  $00,$80,$6C,$00,$00,$02,$03,$1B ; 0x6ca8
				dc.b  $00,$F3,$00,$F8,$17,$00,$01,$00 ; 0x6cb0
				dc.b  $00,$00,$22,$C0,$00,$00,$00,$00 ; 0x6cb8
				dc.b  $01,$C0,$00,$00,$00,$00,$15,$00 ; 0x6cc0
				dc.b  $00,$00,$08,$08,$01,$08,$00,$00 ; 0x6cc8
				dc.b  $00,$00,$06,$80,$00,$00,$02,$80 ; 0x6cd0
				dc.b  $15,$00,$00,$00,$00,$00,$06,$00 ; 0x6cd8
				dc.b  $00,$00,$06,$14,$00,$00,$1E,$00 ; 0x6ce0
				dc.b  $01,$03,$0E,$08,$00,$00,$2C,$00 ; 0x6ce8
				dc.b  $00,$08,$00,$F0,$01,$00,$00,$08 ; 0x6cf0
				dc.b  $00,$F0,$00,$00,$03,$00,$02,$00 ; 0x6cf8
				dc.b  $00,$00,$00,$00,$00,$00,$00,$00 ; 0x6d00
				dc.b  $00,$00,$07,$00,$03,$00,$25,$00 ; 0x6d08
				dc.b  $00,$FF,$00,$FF,$01,$00,$00,$FF ; 0x6d10
				dc.b  $00,$FF,$1E,$FF,$00,$FF,$25,$00 ; 0x6d18
				dc.b  $00,$FF,$00,$FF,$05,$00,$06,$00 ; 0x6d20
				dc.b  $00,$00,$00,$FF,$00,$FF,$7B,$FF ; 0x6d28
				dc.b  $25,$00,$03,$00,$D5,$00,$00,$00 ; 0x6d30
				dc.b  $00,$40,$00,$00,$0D,$00,$00,$00 ; 0x6d38
				dc.b  $00,$00,$0E,$00,$00,$00,$0C,$00 ; 0x6d40
				dc.b  $00,$00,$00,$00,$00,$00,$0D,$00 ; 0x6d48
				dc.b  $00,$00,$0E,$00,$00,$00,$00,$00 ; 0x6d50
				dc.b  $0D,$00,$00,$00,$00,$00,$0D,$00 ; 0x6d58
				dc.b  $00,$00,$00,$00,$00,$00,$4D,$04 ; 0x6d60
				dc.b  $09,$04,$09,$04,$04,$00,$03,$00 ; 0x6d68
				dc.b  $01,$FF,$00,$FF,$01,$DE,$01,$01 ; 0x6d70
				dc.b  $00,$78,$00,$C0,$01,$00,$01,$00 ; 0x6d78
				dc.b  $13,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x6d80
				dc.b  $FF,$00,$01,$10,$00,$00,$36,$04 ; 0x6d88
				dc.b  $00,$9E,$05,$FF,$00,$D3,$00,$22 ; 0x6d90
				dc.b  $13,$DC,$00,$7B,$60,$00,$00,$01 ; 0x6d98
				dc.b  $00,$22,$00,$58,$01,$00,$00,$00 ; 0x6da0
				dc.b  $00,$F0,$0D,$F9,$00,$0A,$12,$0E ; 0x6da8
				dc.b  $00,$57,$00,$60,$01,$06,$00,$10 ; 0x6db0
				dc.b  $00,$20,$0A,$8F,$00,$00,$0B,$00 ; 0x6db8
				dc.b  $FF,$FF,$FF,$00,$00,$6C,$00,$C9 ; 0x6dc0
				dc.b  $00,$36,$07,$6F,$00,$0C,$04,$DA ; 0x6dc8
				dc.b  $00,$07,$00,$C2,$07,$6F,$00,$0C ; 0x6dd0
				dc.b  $14,$00,$00,$60,$00,$80,$0D,$55 ; 0x6dd8
				dc.b  $00,$D6,$0E,$2B,$00,$1B,$00,$40 ; 0x6de0
				dc.b  $0D,$04,$00,$62,$00,$02,$0D,$44 ; 0x6de8
				dc.b  $00,$8F,$00,$7F,$00,$60,$8B,$00 ; 0x6df0
				dc.b  $01,$10,$00,$00,$21,$FF,$00,$DD ; 0x6df8
				dc.b  $00,$35,$02,$71,$00,$98,$20,$09 ; 0x6e00
				dc.b  $01,$07,$00,$73,$22,$F8,$00,$C4 ; 0x6e08
				dc.b  $0A,$CB,$00,$80,$24,$0F,$05,$75 ; 0x6e10
				dc.b  $00,$DE,$02,$CD,$00,$0E,$43,$8F ; 0x6e18
				dc.b  $00,$00,$0B,$00,$FF,$FF,$FF,$00 ; 0x6e20
				dc.b  $00,$70,$00,$24,$00,$BC,$0D,$DE ; 0x6e28
				dc.b  $00,$65,$00,$74,$0D,$40,$00,$AF ; 0x6e30
				dc.b  $00,$40,$0D,$02,$00,$BD,$00,$00 ; 0x6e38
				dc.b  $0D,$58,$00,$FC,$0E,$2D,$00,$DC ; 0x6e40
				dc.b  $00,$80,$0D,$06,$00,$C7,$00,$F4 ; 0x6e48
				dc.b  $0D,$47,$00,$8C,$00,$C3,$00,$C0 ; 0x6e50
				dc.b  $8B,$00,$01,$14,$00,$80,$02,$3E ; 0x6e58
				dc.b  $00,$18,$02,$0A,$00,$E2,$16,$55 ; 0x6e60
				dc.b  $00,$73,$CF,$83,$0C,$00,$FF,$FF ; 0x6e68
				dc.b  $FF,$00,$00,$41,$00,$36,$00,$99 ; 0x6e70
				dc.b  $0D,$EE,$00,$43,$00,$7B,$1D,$95 ; 0x6e78
				dc.b  $00,$C6,$00,$C0,$0D,$1D,$00,$09 ; 0x6e80
				dc.b  $0E,$D9,$00,$67,$00,$E0,$0D,$9C ; 0x6e88
				dc.b  $00,$1D,$00,$DB,$0D,$01,$00,$B1 ; 0x6e90
				dc.b  $00,$5B,$00,$90,$8B,$00,$01,$14 ; 0x6e98
				dc.b  $00,$80,$22,$52,$00,$DA,$02,$DF ; 0x6ea0
				dc.b  $00,$08,$20,$03,$01,$3F,$00,$0B ; 0x6ea8
				dc.b  $00,$FF,$29,$09,$00,$EC,$26,$01 ; 0x6eb0
				dc.b  $01,$01,$16,$13,$00,$80,$00,$FF ; 0x6eb8
				dc.b  $00,$FF,$00,$EC,$00,$C0,$19,$02 ; 0x6ec0
				dc.b  $00,$E0,$00,$80,$01,$06,$00,$6C ; 0x6ec8
				dc.b  $00,$00,$01,$08,$00,$F0,$00,$00 ; 0x6ed0
				dc.b  $00,$00,$00,$08,$00,$F0,$06,$04 ; 0x6ed8
				dc.b  $00,$E7,$03,$83,$0C,$00,$79,$20 ; 0x6ee0
				dc.b  $67,$00,$1D,$FF,$E0,$00,$00,$00 ; 0x6ee8
				dc.b  $00,$00,$1C,$00,$00,$4E,$00,$BC ; 0x6ef0
				dc.b  $00,$29,$0D,$66,$00,$0F,$00,$2B ; 0x6ef8
				dc.b  $0D,$0D,$00,$CC,$00,$B0,$0D,$37 ; 0x6f00
				dc.b  $00,$32,$00,$C0,$0D,$49,$00,$99 ; 0x6f08
				dc.b  $0E,$95,$00,$89,$00,$F0,$02,$02 ; 0x6f10
				dc.b  $00,$A0,$03,$78,$00,$4C,$03,$03 ; 0x6f18
				dc.b  $00,$38,$00,$0F,$00,$8B,$0D,$45 ; 0x6f20
				dc.b  $00,$E0,$00,$D4,$00,$90,$8B,$00 ; 0x6f28
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x6f30
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x6f38
				dc.b  $01,$14,$00,$80,$35,$FF,$00,$FD ; 0x6f40
				dc.b  $00,$3E,$06,$F7,$00,$3F,$13,$E7 ; 0x6f48
				dc.b  $00,$4F,$60,$FF,$00,$FF,$00,$EF ; 0x6f50
				dc.b  $00,$08,$00,$FF,$00,$FF,$00,$C5 ; 0x6f58
				dc.b  $0E,$FA,$00,$65,$00,$00,$11,$09 ; 0x6f60
				dc.b  $00,$8A,$00,$20,$01,$0C,$00,$CB ; 0x6f68
				dc.b  $00,$A0,$0A,$83,$0C,$00,$FF,$FF ; 0x6f70
				dc.b  $FF,$00,$00,$B2,$00,$43,$00,$CD ; 0x6f78
				dc.b  $07,$6E,$00,$F4,$04,$34,$00,$60 ; 0x6f80
				dc.b  $00,$D7,$07,$6E,$00,$F4,$14,$31 ; 0x6f88
				dc.b  $00,$3D,$00,$C0,$0D,$96,$00,$FD ; 0x6f90
				dc.b  $0E,$64,$00,$1D,$00,$60,$0D,$36 ; 0x6f98
				dc.b  $00,$02,$00,$B7,$0D,$82,$00,$6B ; 0x6fa0
				dc.b  $00,$96,$00,$D0,$8B,$00,$01,$14 ; 0x6fa8
				dc.b  $00,$80,$21,$00,$00,$F8,$00,$1D ; 0x6fb0
				dc.b  $00,$80,$01,$E9,$00,$F1,$00,$00 ; 0x6fb8
				dc.b  $25,$03,$00,$75,$00,$C0,$1C,$00 ; 0x6fc0
				dc.b  $00,$2F,$00,$4E,$0A,$CF,$00,$C0 ; 0x6fc8
				dc.b  $24,$A5,$05,$6D,$00,$56,$02,$DF ; 0x6fd0
				dc.b  $00,$FE,$04,$FF,$39,$01,$00,$EF ; 0x6fd8
				dc.b  $03,$83,$0C,$00,$FF,$FF,$25,$C5 ; 0x6fe0
				dc.b  $00,$1C,$02,$B7,$00,$60,$D4,$00 ; 0x6fe8
				dc.b  $00,$B5,$00,$9F,$00,$53,$0D,$38 ; 0x6ff0
				dc.b  $00,$BE,$00,$89,$0D,$4C,$00,$E6 ; 0x6ff8
				dc.b  $00,$90,$0D,$33,$00,$9A,$00,$40 ; 0x7000
				dc.b  $0D,$9A,$00,$23,$0E,$66,$00,$DE ; 0x7008
				dc.b  $00,$A0,$0D,$38,$00,$68,$00,$A9 ; 0x7010
				dc.b  $0D,$85,$00,$68,$00,$DB,$00,$30 ; 0x7018
				dc.b  $8B,$00,$09,$0E,$00,$E4,$16,$78 ; 0x7020
				dc.b  $00,$3F,$CF,$8F,$0C,$00,$FF,$FF ; 0x7028
				dc.b  $FF,$00,$00,$16,$00,$AF,$00,$A8 ; 0x7030
				dc.b  $0D,$B6,$00,$F6,$00,$78,$1D,$77 ; 0x7038
				dc.b  $00,$DE,$00,$00,$0D,$F5,$00,$28 ; 0x7040
				dc.b  $0E,$B6,$00,$83,$00,$00,$0D,$7D ; 0x7048
				dc.b  $00,$BD,$00,$78,$0D,$DB,$00,$D4 ; 0x7050
				dc.b  $00,$4A,$00,$80,$4D,$04,$09,$04 ; 0x7058
				dc.b  $09,$04,$0B,$00,$03,$1B,$00,$F3 ; 0x7060
				dc.b  $02,$00,$01,$00,$13,$00,$25,$C0 ; 0x7068
				dc.b  $00,$00,$02,$C0,$00,$00,$20,$08 ; 0x7070
				dc.b  $01,$08,$00,$00,$00,$00,$29,$00 ; 0x7078
				dc.b  $00,$00,$26,$00,$01,$FF,$16,$10 ; 0x7080
				dc.b  $00,$00,$00,$00,$00,$02,$00,$00 ; 0x7088
				dc.b  $00,$00,$18,$FF,$00,$F7,$00,$8A ; 0x7090
				dc.b  $03,$B5,$02,$05,$00,$07,$00,$80 ; 0x7098
				dc.b  $01,$09,$00,$08,$07,$F5,$03,$8F ; 0x70a0
				dc.b  $0C,$00,$79,$00,$67,$80,$00,$90 ; 0x70a8
				dc.b  $00,$C4,$1B,$FF,$E1,$09,$00,$08 ; 0x70b0
				dc.b  $1C,$00,$00,$0A,$00,$28,$00,$FB ; 0x70b8
				dc.b  $0D,$0C,$00,$E3,$00,$01,$0D,$01 ; 0x70c0
				dc.b  $00,$BE,$00,$10,$0D,$06,$00,$F8 ; 0x70c8
				dc.b  $00,$40,$0D,$09,$00,$4B,$0E,$08 ; 0x70d0
				dc.b  $00,$21,$00,$A0,$02,$00,$00,$E0 ; 0x70d8
				dc.b  $03,$75,$00,$40,$03,$00,$00,$07 ; 0x70e0
				dc.b  $00,$14,$00,$21,$0D,$08,$00,$D2 ; 0x70e8
				dc.b  $00,$C5,$00,$B0,$8B,$00,$FF,$00 ; 0x70f0
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0x70f8
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$38,$00 ; 0x7100
				dc.b  $00,$0E,$00,$08,$06,$D3,$00,$94 ; 0x7108
				dc.b  $13,$E2,$00,$8B,$61,$FE,$00,$53 ; 0x7110
				dc.b  $00,$E0,$00,$00,$00,$00,$00,$4C ; 0x7118
				dc.b  $00,$40,$0D,$F9,$00,$CC,$00,$80 ; 0x7120
				dc.b  $11,$03,$00,$1D,$00,$80,$01,$06 ; 0x7128
				dc.b  $00,$3F,$00,$00,$0A,$8F,$0C,$00 ; 0x7130
				dc.b  $FF,$FF,$FF,$00,$00,$87,$00,$BC ; 0x7138
				dc.b  $00,$DC,$0D,$FD,$00,$13,$00,$D4 ; 0x7140
				dc.b  $1D,$13,$00,$55,$00,$00,$0D,$6F ; 0x7148
				dc.b  $00,$1C,$0E,$41,$00,$38,$00,$80 ; 0x7150
				dc.b  $0D,$17,$00,$A2,$00,$54,$0D,$5C ; 0x7158
				dc.b  $00,$8E,$00,$85,$00,$C0,$8B,$00 ; 0x7160
				dc.b  $25,$4C,$00,$D8,$02,$73,$00,$C9 ; 0x7168
				dc.b  $00,$80,$45,$3E,$00,$98,$09,$00 ; 0x7170
				dc.b  $00,$19,$25,$FF,$49,$03,$00,$65 ; 0x7178
				dc.b  $03,$8F,$0C,$00,$FF,$FF,$25,$3A ; 0x7180
				dc.b  $00,$3E,$02,$8F,$00,$10,$D4,$00 ; 0x7188
				dc.b  $00,$8B,$00,$18,$00,$62,$0D,$01 ; 0x7190
				dc.b  $00,$71,$00,$86,$0D,$45,$00,$6C ; 0x7198
				dc.b  $00,$60,$0D,$15,$00,$B1,$00,$80 ; 0x71a0
				dc.b  $0D,$72,$00,$42,$0E,$43,$00,$F9 ; 0x71a8
				dc.b  $00,$C0,$0D,$1A,$00,$08,$00,$46 ; 0x71b0
				dc.b  $0D,$5F,$00,$8B,$00,$CA,$00,$20 ; 0x71b8
				dc.b  $8B,$00,$09,$0C,$00,$AA,$E7,$8A ; 0x71c0
				dc.b  $00,$08,$0B,$00,$FF,$FF,$FF,$00 ; 0x71c8
				dc.b  $00,$90,$00,$85,$00,$E8,$0D,$55 ; 0x71d0
				dc.b  $00,$65,$00,$38,$1D,$CD,$00,$8E ; 0x71d8
				dc.b  $0E,$67,$00,$68,$0E,$1A,$00,$7B ; 0x71e0
				dc.b  $0E,$D4,$00,$C4,$00,$38,$0D,$48 ; 0x71e8
				dc.b  $00,$4E,$00,$2E,$6F,$0F,$00,$31 ; 0x71f0
				dc.b  $1B,$00,$01,$00,$00,$00,$87,$04 ; 0x71f8
				dc.b  $18,$03,$34,$00,$00,$01,$00,$00 ; 0x7200
				dc.b  $00,$00,$01,$01,$00,$00,$02,$08 ; 0x7208
				dc.b  $00,$F0,$00,$00,$01,$08,$00,$F0 ; 0x7210
				dc.b  $04,$00,$01,$00,$00,$00,$00,$00 ; 0x7218
				dc.b  $00,$00,$00,$00,$00,$00,$08,$00 ; 0x7220
				dc.b  $01,$00,$01,$00,$E1,$00,$00,$FF ; 0x7228
				dc.b  $00,$FF,$1B,$FF,$E1,$00,$00,$00 ; 0x7230
				dc.b  $1C,$00,$00,$00,$00,$40,$00,$00 ; 0x7238
				dc.b  $0D,$00,$00,$00,$00,$00,$0D,$00 ; 0x7240
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0x7248
				dc.b  $00,$00,$0D,$00,$00,$00,$0E,$00 ; 0x7250
				dc.b  $00,$00,$00,$00,$07,$65,$05,$00 ; 0x7258
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0x7260
				dc.b  $00,$00,$00,$00,$6C,$FF,$00,$FF ; 0x7268
				dc.b  $03,$01,$00,$78,$00,$C0,$17,$00 ; 0x7270
				dc.b  $01,$14,$00,$80,$22,$9F,$00,$B7 ; 0x7278
				dc.b  $02,$EE,$00,$DE,$00,$80,$0D,$01 ; 0x7280
				dc.b  $00,$40,$06,$12,$00,$EF,$13,$00 ; 0x7288
				dc.b  $03,$00,$16,$4F,$00,$FC,$30,$05 ; 0x7290
				dc.b  $3D,$07,$00,$F1,$00,$40,$00,$FF ; 0x7298
				dc.b  $00,$FC,$00,$A5,$00,$A0,$03,$02 ; 0x72a0
				dc.b  $04,$65,$00,$21,$00,$8A,$00,$08 ; 0x72a8
				dc.b  $07,$03,$03,$09,$25,$01,$00,$80 ; 0x72b0
				dc.b  $00,$00,$01,$02,$00,$80,$00,$00 ; 0x72b8
				dc.b  $0E,$80,$00,$00,$05,$01,$00,$AB ; 0x72c0
				dc.b  $00,$5C,$2C,$04,$00,$04,$6F,$40 ; 0x72c8
				dc.b  $03,$80,$00,$A7,$00,$00,$17,$FF ; 0x72d0
				dc.b  $25,$C0,$03,$C0,$0F,$01,$00,$40 ; 0x72d8
				dc.b  $06,$2F,$00,$C0,$2E,$08,$00,$40 ; 0x72e0
				dc.b  $6F,$88,$01,$FF,$00,$FF,$00,$4C ; 0x72e8
				dc.b  $18,$00,$00,$12,$00,$E0,$00,$F8 ; 0x72f0
				dc.b  $0D,$18,$00,$39,$00,$68,$0D,$01 ; 0x72f8
				dc.b  $00,$42,$00,$10,$02,$01,$00,$80 ; 0x7300
				dc.b  $08,$06,$00,$0D,$00,$1A,$0E,$11 ; 0x7308
				dc.b  $00,$78,$0E,$0F,$00,$49,$0E,$0D ; 0x7310
				dc.b  $00,$4E,$00,$68,$0D,$10,$00,$95 ; 0x7318
				dc.b  $00,$FF,$00,$80,$4D,$00,$09,$00 ; 0x7320
				dc.b  $09,$00,$04,$80,$03,$80,$01,$00 ; 0x7328
				dc.b  $00,$02,$01,$00,$01,$1B,$00,$F3 ; 0x7330
				dc.b  $00,$F8,$01,$02,$01,$0F,$13,$00 ; 0x7338
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x7340
				dc.b  $38,$FF,$00,$DE,$00,$EE,$05,$00 ; 0x7348
				dc.b  $00,$0D,$00,$E4,$13,$E3,$00,$DD ; 0x7350
				dc.b  $02,$4D,$00,$D3,$5E,$21,$01,$FF ; 0x7358
				dc.b  $00,$FE,$00,$D3,$00,$70,$0E,$F6 ; 0x7360
				dc.b  $00,$C0,$11,$02,$00,$55,$02,$0A ; 0x7368
				dc.b  $00,$9B,$00,$80,$0A,$8A,$00,$08 ; 0x7370
				dc.b  $0B,$00,$FF,$FF,$FF,$00,$00,$01 ; 0x7378
				dc.b  $00,$93,$00,$1C,$0D,$9B,$00,$82 ; 0x7380
				dc.b  $00,$94,$1D,$69,$00,$05,$0E,$E1 ; 0x7388
				dc.b  $00,$5C,$0E,$A5,$00,$30,$0E,$6E ; 0x7390
				dc.b  $00,$A9,$00,$14,$0D,$C9,$00,$08 ; 0x7398
				dc.b  $00,$69,$8C,$00,$25,$5C,$00,$08 ; 0x73a0
				dc.b  $02,$9B,$00,$D4,$20,$10,$01,$0F ; 0x73a8
				dc.b  $00,$33,$01,$FF,$00,$FF,$00,$F6 ; 0x73b0
				dc.b  $00,$00,$02,$91,$00,$8F,$02,$88 ; 0x73b8
				dc.b  $00,$7F,$14,$FF,$00,$FB,$00,$50 ; 0x73c0
				dc.b  $05,$FF,$00,$D6,$00,$40,$02,$8A ; 0x73c8
				dc.b  $00,$F8,$24,$05,$05,$BA,$00,$90 ; 0x73d0
				dc.b  $02,$BA,$00,$42,$05,$FF,$00,$FF ; 0x73d8
				dc.b  $00,$FE,$00,$01,$08,$FF,$00,$FF ; 0x73e0
				dc.b  $00,$FF,$00,$1E,$2E,$8A,$00,$08 ; 0x73e8
				dc.b  $0B,$00,$79,$20,$02,$01,$00,$04 ; 0x73f0
				dc.b  $81,$FF,$FF,$00,$00,$04,$00,$EE ; 0x73f8
				dc.b  $00,$A2,$0D,$9F,$00,$E0,$00,$46 ; 0x7400
				dc.b  $0D,$4A,$00,$41,$03,$02,$09,$06 ; 0x7408
				dc.b  $00,$6B,$00,$61,$0E,$E4,$00,$82 ; 0x7410
				dc.b  $0E,$A7,$00,$F1,$0E,$71,$00,$0F ; 0x7418
				dc.b  $00,$06,$0D,$CC,$00,$05,$00,$AE ; 0x7420
				dc.b  $8C,$00,$01,$10,$00,$00,$02,$40 ; 0x7428
				dc.b  $00,$FA,$03,$38,$E7,$9E,$00,$13 ; 0x7430
				dc.b  $0B,$00,$FF,$FF,$FF,$00,$00,$80 ; 0x7438
				dc.b  $00,$FE,$00,$58,$0D,$41,$00,$33 ; 0x7440
				dc.b  $00,$88,$1D,$C2,$00,$A2,$0E,$58 ; 0x7448
				dc.b  $00,$D8,$0E,$0D,$00,$BD,$0E,$C9 ; 0x7450
				dc.b  $00,$AC,$00,$88,$0D,$3A,$00,$7A ; 0x7458
				dc.b  $00,$95,$6F,$0C,$00,$A4,$1B,$00 ; 0x7460
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x7468
				dc.b  $01,$10,$00,$00,$22,$91,$00,$20 ; 0x7470
				dc.b  $02,$D5,$00,$96,$00,$00,$0D,$00 ; 0x7478
				dc.b  $00,$00,$06,$20,$00,$40,$2D,$FF ; 0x7480
				dc.b  $00,$82,$00,$98,$0E,$04,$00,$A1 ; 0x7488
				dc.b  $20,$A5,$3C,$FF,$00,$FD,$00,$0F ; 0x7490
				dc.b  $00,$80,$01,$FA,$00,$CE,$00,$00 ; 0x7498
				dc.b  $0A,$9E,$00,$13,$0B,$09,$41,$80 ; 0x74a0
				dc.b  $00,$80,$00,$00,$2C,$00,$00,$40 ; 0x74a8
				dc.b  $0E,$08,$00,$04,$00,$5B,$00,$C0 ; 0x74b0
				dc.b  $5D,$00,$03,$00,$00,$FF,$00,$FF ; 0x74b8
				dc.b  $17,$FF,$39,$00,$00,$00,$06,$3B ; 0x74c0
				dc.b  $00,$D0,$2E,$00,$00,$00,$6F,$00 ; 0x74c8
				dc.b  $01,$00,$00,$00,$00,$00,$18,$00 ; 0x74d0
				dc.b  $00,$10,$00,$B9,$00,$A3,$0D,$15 ; 0x74d8
				dc.b  $00,$6C,$00,$79,$0E,$41,$00,$C0 ; 0x74e0
				dc.b  $02,$02,$00,$30,$09,$0B,$00,$96 ; 0x74e8
				dc.b  $00,$40,$0D,$0F,$00,$73,$0E,$0D ; 0x74f0
				dc.b  $00,$84,$00,$A0,$0D,$0B,$00,$C4 ; 0x74f8
				dc.b  $00,$99,$0D,$0E,$00,$AB,$00,$20 ; 0x7500
				dc.b  $00,$30,$8B,$00,$FF,$00,$FF,$FF ; 0x7508
				dc.b  $FF,$00,$FF,$00,$01,$10,$00,$00 ; 0x7510
				dc.b  $35,$00,$00,$19,$00,$F8,$06,$1B ; 0x7518
				dc.b  $00,$98,$13,$D5,$00,$29,$62,$FD ; 0x7520
				dc.b  $00,$D0,$00,$00,$00,$00,$00,$AB ; 0x7528
				dc.b  $00,$C0,$0D,$F8,$00,$20,$00,$40 ; 0x7530
				dc.b  $11,$05,$00,$C5,$00,$40,$01,$10 ; 0x7538
				dc.b  $00,$A4,$00,$C0,$0A,$9E,$00,$13 ; 0x7540
				dc.b  $0B,$00,$FF,$FF,$FF,$00,$00,$F2 ; 0x7548
				dc.b  $00,$0B,$00,$8C,$0D,$87,$00,$50 ; 0x7550
				dc.b  $00,$E4,$1D,$5E,$00,$19,$0E,$D2 ; 0x7558
				dc.b  $00,$CC,$0E,$98,$00,$72,$0E,$63 ; 0x7560
				dc.b  $00,$91,$00,$64,$0D,$BB,$00,$34 ; 0x7568
				dc.b  $00,$D0,$8C,$00,$01,$10,$00,$00 ; 0x7570
				dc.b  $22,$27,$00,$7F,$02,$E8,$00,$5D ; 0x7578
				dc.b  $46,$E9,$00,$78,$05,$00,$00,$78 ; 0x7580
				dc.b  $00,$BC,$02,$A8,$00,$60,$24,$A5 ; 0x7588
				dc.b  $1C,$00,$00,$00,$00,$00,$00,$FA ; 0x7590
				dc.b  $2E,$9E,$00,$13,$0B,$00,$C1,$80 ; 0x7598
				dc.b  $00,$80,$00,$00,$3B,$FF,$C0,$FF ; 0x75a0
				dc.b  $00,$FF,$00,$FF,$00,$1E,$3B,$00 ; 0x75a8
				dc.b  $00,$F5,$00,$67,$00,$12,$0D,$8B ; 0x75b0
				dc.b  $00,$AE,$00,$96,$1D,$60,$00,$75 ; 0x75b8
				dc.b  $0E,$D5,$00,$F2,$0E,$9B,$00,$33 ; 0x75c0
				dc.b  $0E,$65,$00,$F7,$00,$56,$0D,$BE ; 0x75c8
				dc.b  $00,$32,$00,$15,$8C,$00,$01,$14 ; 0x75d0
				dc.b  $00,$80,$01,$01,$00,$3E,$00,$18 ; 0x75d8
				dc.b  $01,$01,$00,$0D,$00,$82,$0E,$EF ; 0x75e0
				dc.b  $00,$28,$02,$C4,$00,$EC,$03,$3F ; 0x75e8
				dc.b  $00,$FF,$5D,$00,$00,$65,$00,$40 ; 0x75f0
				dc.b  $0B,$01,$60,$65,$00,$21,$00,$9B ; 0x75f8
				dc.b  $08,$04,$01,$01,$01,$00,$1A,$80 ; 0x7600
				dc.b  $00,$00,$02,$14,$00,$FC,$DF,$FF ; 0x7608
				dc.b  $19,$EF,$00,$28,$02,$C0,$00,$D8 ; 0x7610
				dc.b  $E0,$00,$00,$E4,$00,$77,$00,$0F ; 0x7618
				dc.b  $0D,$C2,$00,$8C,$00,$FD,$0D,$65 ; 0x7620
				dc.b  $00,$68,$00,$D0,$02,$01,$00,$40 ; 0x7628
				dc.b  $08,$06,$00,$08,$00,$97,$00,$40 ; 0x7630
				dc.b  $0D,$B6,$00,$1F,$0E,$5F,$00,$5B ; 0x7638
				dc.b  $00,$20,$0D,$10,$00,$B9,$00,$9D ; 0x7640
				dc.b  $0D,$93,$00,$0A,$00,$CE,$00,$F0 ; 0x7648
				dc.b  $8B,$00,$01,$14,$00,$80,$22,$71 ; 0x7650
				dc.b  $00,$E8,$01,$01,$00,$58,$00,$E8 ; 0x7658
				dc.b  $0A,$E1,$00,$BF,$00,$FF,$B3,$03 ; 0x7660
				dc.b  $01,$07,$00,$1D,$00,$FF,$00,$65 ; 0x7668
				dc.b  $00,$21,$00,$9B,$08,$0C,$01,$05 ; 0x7670
				dc.b  $01,$00,$25,$80,$D9,$FF,$25,$71 ; 0x7678
				dc.b  $00,$E8,$D8,$00,$00,$5B,$00,$63 ; 0x7680
				dc.b  $00,$E7,$0D,$76,$00,$84,$00,$05 ; 0x7688
				dc.b  $0D,$10,$00,$06,$00,$50,$0D,$40 ; 0x7690
				dc.b  $00,$19,$00,$40,$0D,$55,$00,$77 ; 0x7698
				dc.b  $0E,$4A,$00,$C8,$00,$20,$0D,$41 ; 0x76a0
				dc.b  $00,$19,$00,$A5,$0D,$51,$00,$25 ; 0x76a8
				dc.b  $00,$4C,$00,$70,$0B,$08,$65,$1B ; 0x76b0
				dc.b  $00,$C3,$00,$F0,$17,$00,$FF,$00 ; 0x76b8
				dc.b  $FF,$FF,$FF,$00,$E0,$FF,$00,$FF ; 0x76c0
				dc.b  $01,$DE,$01,$01,$00,$78,$00,$C0 ; 0x76c8
				dc.b  $17,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x76d0
				dc.b  $E5,$1B,$00,$C3,$00,$F0,$17,$00 ; 0x76d8
				dc.b  $01,$14,$00,$80,$35,$FF,$00,$E9 ; 0x76e0
				dc.b  $00,$F6,$05,$FF,$00,$BB,$00,$EF ; 0x76e8
				dc.b  $11,$FF,$00,$FF,$00,$F2,$00,$16 ; 0x76f0
				dc.b  $02,$58,$00,$7F,$02,$79,$00,$0B ; 0x76f8
				dc.b  $02,$7F,$00,$23,$02,$DE,$00,$8F ; 0x7700
				dc.b  $19,$00,$13,$14,$00,$8B,$21,$FF ; 0x7708
				dc.b  $00,$FE,$00,$76,$00,$B8,$00,$FF ; 0x7710
				dc.b  $00,$FF,$00,$3E,$00,$D0,$0C,$FF ; 0x7718
				dc.b  $00,$F0,$00,$A3,$00,$60,$11,$03 ; 0x7720
				dc.b  $00,$A8,$00,$E0,$01,$01,$00,$38 ; 0x7728
				dc.b  $09,$65,$00,$21,$00,$9B,$08,$0D ; 0x7730
				dc.b  $01,$06,$01,$00,$39,$01,$07,$02 ; 0x7738
				dc.b  $12,$01,$00,$04,$10,$80,$00,$00 ; 0x7740
				dc.b  $2C,$01,$00,$04,$23,$40,$03,$01 ; 0x7748
				dc.b  $0E,$01,$00,$04,$13,$40,$03,$80 ; 0x7750
				dc.b  $19,$FF,$38,$FF,$00,$FD,$00,$9C ; 0x7758
				dc.b  $05,$FF,$00,$F9,$00,$80,$13,$13 ; 0x7760
				dc.b  $00,$9B,$0E,$6A,$00,$D2,$2D,$36 ; 0x7768
				dc.b  $00,$10,$21,$FF,$00,$FF,$00,$CE ; 0x7770
				dc.b  $00,$80,$00,$FF,$00,$FF,$00,$DC ; 0x7778
				dc.b  $0D,$FF,$00,$F4,$00,$D4,$12,$09 ; 0x7780
				dc.b  $00,$08,$02,$09,$00,$08,$18,$00 ; 0x7788
				dc.b  $00,$55,$00,$84,$00,$43,$06,$19 ; 0x7790
				dc.b  $00,$6F,$00,$0C,$03,$05,$00,$08 ; 0x7798
				dc.b  $00,$AA,$00,$59,$06,$19,$00,$6F ; 0x77a0
				dc.b  $00,$0C,$03,$05,$00,$0D,$00,$C1 ; 0x77a8
				dc.b  $00,$90,$02,$02,$00,$60,$08,$06 ; 0x77b0
				dc.b  $00,$A4,$00,$0E,$00,$40,$0D,$30 ; 0x77b8
				dc.b  $00,$13,$0E,$EA,$00,$10,$00,$A0 ; 0x77c0
				dc.b  $0D,$AA,$00,$9E,$00,$79,$0D,$13 ; 0x77c8
				dc.b  $00,$C5,$00,$0A,$00,$30,$71,$1B ; 0x77d0
				dc.b  $00,$C3,$00,$F0,$17,$00,$01,$14 ; 0x77d8
				dc.b  $00,$80,$22,$6D,$00,$9B,$02,$8F ; 0x77e0
				dc.b  $00,$10,$09,$01,$00,$34,$00,$BF ; 0x77e8
				dc.b  $00,$FF,$13,$10,$01,$0F,$00,$33 ; 0x77f0
				dc.b  $00,$FF,$00,$FF,$00,$FF,$00,$F6 ; 0x77f8
				dc.b  $03,$9B,$00,$3F,$02,$88,$00,$7F ; 0x7800
				dc.b  $15,$3E,$00,$16,$01,$FF,$00,$FF ; 0x7808
				dc.b  $00,$E4,$01,$FF,$00,$8D,$00,$E0 ; 0x7810
				dc.b  $01,$FF,$00,$B9,$00,$60,$22,$01 ; 0x7818
				dc.b  $07,$77,$00,$7D,$00,$80,$01,$CC ; 0x7820
				dc.b  $00,$46,$00,$80,$02,$18,$00,$9A ; 0x7828
				dc.b  $00,$FF,$00,$FF,$00,$FF,$00,$E3 ; 0x7830
				dc.b  $08,$FF,$00,$FF,$00,$FF,$00,$87 ; 0x7838
				dc.b  $27,$03,$01,$03,$00,$65,$00,$FF ; 0x7840
				dc.b  $00,$65,$00,$21,$00,$9B,$08,$0C ; 0x7848
				dc.b  $01,$05,$01,$00,$25,$40,$00,$C8 ; 0x7850
				dc.b  $00,$37,$01,$80,$47,$10,$00,$82 ; 0x7858
				dc.b  $00,$80,$05,$20,$03,$20,$2B,$01 ; 0x7860
				dc.b  $03,$02,$03,$10,$03,$80,$0B,$40 ; 0x7868
				dc.b  $3D,$FF,$25,$65,$00,$D6,$02,$8F ; 0x7870
				dc.b  $00,$10,$45,$FF,$00,$FE,$00,$60 ; 0x7878
				dc.b  $0A,$2B,$00,$80,$2A,$BA,$00,$90 ; 0x7880
				dc.b  $02,$BA,$00,$30,$03,$08,$00,$FB ; 0x7888
				dc.b  $00,$FF,$00,$FF,$00,$FF,$00,$E3 ; 0x7890
				dc.b  $08,$FF,$00,$FF,$00,$FF,$00,$1E ; 0x7898
				dc.b  $3B,$00,$00,$58,$00,$DF,$00,$C9 ; 0x78a0
				dc.b  $0D,$0D,$00,$08,$00,$0B,$0D,$4A ; 0x78a8
				dc.b  $00,$41,$00,$60,$02,$02,$09,$06 ; 0x78b0
				dc.b  $00,$A6,$00,$6A,$00,$C0,$0D,$33 ; 0x78b8
				dc.b  $00,$39,$0E,$EC,$00,$D1,$00,$E0 ; 0x78c0
				dc.b  $0D,$AD,$00,$04,$00,$6B,$0D,$16 ; 0x78c8
				dc.b  $00,$C2,$00,$4E,$00,$90,$0B,$08 ; 0x78d0
				dc.b  $65,$1B,$00,$C3,$00,$F0,$17,$00 ; 0x78d8
				dc.b  $01,$10,$00,$00,$06,$0F,$00,$4A ; 0x78e0
				dc.b  $02,$84,$00,$FF,$00,$FF,$01,$84 ; 0x78e8
				dc.b  $00,$FF,$00,$FF,$09,$C0,$00,$90 ; 0x78f0
				dc.b  $D3,$9A,$00,$03,$0B,$00,$FF,$FF ; 0x78f8
				dc.b  $FF,$00,$00,$5D,$00,$74,$00,$D4 ; 0x7900
				dc.b  $0D,$5F,$00,$E2,$00,$3C,$1D,$5D ; 0x7908
				dc.b  $00,$AF,$00,$00,$0D,$27,$00,$94 ; 0x7910
				dc.b  $0E,$C2,$00,$A1,$00,$80,$0D,$67 ; 0x7918
				dc.b  $00,$25,$00,$BC,$0D,$FE,$00,$C3 ; 0x7920
				dc.b  $00,$F5,$00,$40,$8B,$00,$01,$10 ; 0x7928
				dc.b  $00,$00,$22,$C7,$00,$98,$01,$00 ; 0x7930
				dc.b  $00,$C7,$00,$8C,$09,$02,$00,$50 ; 0x7938
				dc.b  $00,$9F,$39,$FF,$00,$C5,$0B,$6B ; 0x7940
				dc.b  $00,$7C,$24,$05,$4E,$9A,$00,$03 ; 0x7948
				dc.b  $0B,$00,$25,$00,$4B,$10,$0B,$20 ; 0x7950
				dc.b  $81,$FF,$25,$00,$00,$00,$56,$40 ; 0x7958
				dc.b  $81,$00,$00,$D4,$00,$61,$00,$AC ; 0x7960
				dc.b  $0D,$13,$00,$D9,$00,$44,$0D,$25 ; 0x7968
				dc.b  $00,$4C,$00,$40,$0D,$95,$00,$31 ; 0x7970
				dc.b  $00,$00,$0D,$C6,$00,$EC,$0E,$AE ; 0x7978
				dc.b  $00,$0E,$00,$80,$0D,$97,$00,$85 ; 0x7980
				dc.b  $00,$C4,$0D,$BC,$00,$DE,$00,$72 ; 0x7988
				dc.b  $00,$C0,$0B,$00,$7F,$00,$FF,$00 ; 0x7990
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0x7998
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$01,$10 ; 0x79a0
				dc.b  $00,$00,$36,$F4,$00,$98,$05,$00 ; 0x79a8
				dc.b  $00,$25,$00,$06,$13,$E4,$00,$19 ; 0x79b0
				dc.b  $02,$1A,$00,$4B,$02,$81,$00,$0F ; 0x79b8
				dc.b  $02,$7A,$00,$9A,$02,$AB,$00,$43 ; 0x79c0
				dc.b  $2D,$06,$00,$8E,$21,$00,$00,$00 ; 0x79c8
				dc.b  $00,$4D,$00,$90,$02,$93,$00,$E0 ; 0x79d0
				dc.b  $0D,$EE,$00,$E3,$00,$C0,$11,$0B ; 0x79d8
				dc.b  $00,$04,$00,$40,$01,$09,$00,$4E ; 0x79e0
				dc.b  $00,$C0,$0A,$9A,$00,$03,$0B,$00 ; 0x79e8
				dc.b  $FF,$FF,$FF,$00,$00,$6D,$00,$F9 ; 0x79f0
				dc.b  $00,$A8,$02,$0C,$00,$61,$02,$1E ; 0x79f8
				dc.b  $00,$75,$00,$40,$03,$00,$00,$1C ; 0x7a00
				dc.b  $00,$1F,$00,$08,$02,$0C,$00,$43 ; 0x7a08
				dc.b  $02,$1E,$00,$75,$00,$40,$03,$00 ; 0x7a10
				dc.b  $10,$F9,$00,$26,$00,$00,$0D,$A1 ; 0x7a18
				dc.b  $00,$88,$0E,$4D,$00,$57,$00,$00 ; 0x7a20
				dc.b  $0D,$01,$00,$0A,$00,$98,$0D,$7F ; 0x7a28
				dc.b  $00,$7E,$00,$30,$00,$80,$8B,$00 ; 0x7a30
				dc.b  $01,$10,$00,$00,$21,$01,$00,$0A ; 0x7a38
				dc.b  $00,$55,$02,$66,$00,$DF,$00,$80 ; 0x7a40
				dc.b  $08,$00,$00,$00,$00,$00,$00,$00 ; 0x7a48
				dc.b  $13,$08,$01,$08,$00,$00,$00,$00 ; 0x7a50
				dc.b  $00,$00,$00,$00,$00,$00,$03,$00 ; 0x7a58
				dc.b  $00,$00,$02,$00,$00,$00,$14,$FF ; 0x7a60
				dc.b  $00,$BE,$00,$DC,$01,$00,$00,$00 ; 0x7a68
				dc.b  $00,$04,$01,$00,$00,$00,$00,$00 ; 0x7a70
				dc.b  $01,$00,$00,$46,$00,$6C,$02,$07 ; 0x7a78
				dc.b  $00,$5C,$1E,$00,$01,$05,$05,$C0 ; 0x7a80
				dc.b  $00,$00,$00,$00,$01,$C0,$00,$00 ; 0x7a88
				dc.b  $00,$00,$02,$08,$00,$00,$00,$00 ; 0x7a90
				dc.b  $00,$00,$00,$00,$00,$00,$08,$00 ; 0x7a98
				dc.b  $00,$00,$00,$00,$00,$00,$11,$22 ; 0x7aa0
				dc.b  $00,$90,$03,$08,$03,$08,$02,$05 ; 0x7aa8
				dc.b  $00,$20,$02,$10,$00,$6B,$00,$60 ; 0x7ab0
				dc.b  $03,$02,$01,$00,$00,$9F,$00,$00 ; 0x7ab8
				dc.b  $02,$9A,$00,$03,$07,$03,$01,$00 ; 0x7ac0
				dc.b  $01,$09,$25,$10,$00,$FF,$00,$FF ; 0x7ac8
				dc.b  $01,$20,$00,$F1,$00,$70,$45,$40 ; 0x7ad0
				dc.b  $00,$8B,$00,$94,$05,$00,$02,$10 ; 0x7ad8
				dc.b  $00,$04,$00,$80,$00,$00,$00,$01 ; 0x7ae0
				dc.b  $00,$04,$00,$27,$00,$AF,$25,$00 ; 0x7ae8
				dc.b  $03,$00,$03,$00,$03,$00,$0B,$00 ; 0x7af0
				dc.b  $12,$08,$00,$04,$00,$56,$00,$F0 ; 0x7af8
				dc.b  $09,$01,$00,$80,$00,$00,$01,$02 ; 0x7b00
				dc.b  $06,$04,$00,$04,$00,$0D,$00,$68 ; 0x7b08
				dc.b  $0F,$FF,$25,$C0,$00,$00,$02,$C0 ; 0x7b10
				dc.b  $00,$00,$45,$00,$00,$00,$00,$00 ; 0x7b18
				dc.b  $0A,$40,$00,$00,$2A,$00,$00,$00 ; 0x7b20
				dc.b  $02,$00,$00,$00,$03,$00,$00,$00 ; 0x7b28
				dc.b  $00,$00,$00,$00,$00,$00,$00,$00 ; 0x7b30
				dc.b  $08,$00,$00,$00,$00,$00,$00,$00 ; 0x7b38
				dc.b  $10,$FF,$00,$E8,$0B,$09,$00,$08 ; 0x7b40
				dc.b  $02,$09,$00,$08,$18,$00,$00,$48 ; 0x7b48
				dc.b  $00,$C6,$00,$22,$0D,$5E,$00,$4E ; 0x7b50
				dc.b  $00,$C6,$0D,$01,$00,$3F,$00,$E0 ; 0x7b58
				dc.b  $02,$01,$00,$90,$09,$33,$00,$01 ; 0x7b60
				dc.b  $00,$80,$0D,$D9,$00,$24,$00,$90 ; 0x7b68
				dc.b  $02,$04,$00,$30,$09,$58,$00,$BC ; 0x7b70
				dc.b  $00,$F0,$02,$07,$00,$00,$09,$33 ; 0x7b78
				dc.b  $00,$CD,$00,$86,$0D,$40,$00,$92 ; 0x7b80
				dc.b  $00,$26,$00,$20,$0B,$00,$66,$D3 ; 0x7b88
				dc.b  $18,$00,$01,$14,$00,$80,$06,$0C ; 0x7b90
				dc.b  $00,$BC,$E7,$92,$00,$00,$0B,$00 ; 0x7b98
				dc.b  $FF,$FF,$FF,$00,$00,$4B,$00,$88 ; 0x7ba0
				dc.b  $00,$15,$0D,$48,$00,$93,$00,$2F ; 0x7ba8
				dc.b  $1D,$51,$00,$13,$00,$C0,$0D,$16 ; 0x7bb0
				dc.b  $00,$C5,$0E,$B3,$00,$EC,$00,$60 ; 0x7bb8
				dc.b  $0D,$5A,$00,$58,$00,$0F,$0D,$EE ; 0x7bc0
				dc.b  $00,$CE,$00,$6B,$00,$50,$8B,$00 ; 0x7bc8
				dc.b  $01,$14,$00,$80,$6D,$00,$00,$52 ; 0x7bd0
				dc.b  $00,$FC,$0A,$A6,$00,$3C,$24,$FF ; 0x7bd8
				dc.b  $4E,$92,$00,$00,$0B,$00,$FF,$FF ; 0x7be0
				dc.b  $FF,$00,$00,$C2,$00,$74,$00,$ED ; 0x7be8
				dc.b  $0D,$FC,$00,$8A,$00,$37,$0D,$22 ; 0x7bf0
				dc.b  $00,$25,$00,$70,$0D,$88,$00,$95 ; 0x7bf8
				dc.b  $00,$C0,$0D,$B6,$00,$1D,$0E,$9F ; 0x7c00
				dc.b  $00,$59,$00,$60,$0D,$8A,$00,$B8 ; 0x7c08
				dc.b  $00,$17,$0D,$AC,$00,$E8,$00,$E8 ; 0x7c10
				dc.b  $00,$D0,$8B,$00,$FF,$00,$FF,$FF ; 0x7c18
				dc.b  $FF,$00,$FF,$00,$FF,$00,$FF,$FF ; 0x7c20
				dc.b  $FF,$00,$FF,$00,$01,$14,$00,$80 ; 0x7c28
				dc.b  $36,$D9,$00,$F1,$06,$00,$00,$C7 ; 0x7c30
				dc.b  $13,$D6,$00,$93,$3C,$FF,$00,$F9 ; 0x7c38
				dc.b  $00,$08,$22,$01,$00,$C6,$00,$70 ; 0x7c40
				dc.b  $01,$FE,$00,$BE,$00,$A8,$0D,$ED ; 0x7c48
				dc.b  $00,$33,$00,$00,$11,$10,$00,$E7 ; 0x7c50
				dc.b  $00,$C0,$01,$07,$00,$58,$00,$E0 ; 0x7c58
				dc.b  $0A,$92,$00,$00,$0B,$00,$FF,$FF ; 0x7c60
				dc.b  $FF,$00,$00,$9D,$00,$E7,$00,$39 ; 0x7c68
				dc.b  $0D,$4E,$00,$04,$00,$DB,$1D,$EC ; 0x7c70
				dc.b  $00,$8A,$00,$C0,$0D,$90,$00,$B9 ; 0x7c78
				dc.b  $0E,$3E,$00,$A1,$00,$E0,$0D,$F4 ; 0x7c80
				dc.b  $00,$3C,$00,$EB,$0D,$6F,$00,$88 ; 0x7c88
				dc.b  $00,$A6,$00,$90,$8B,$00,$01,$14 ; 0x7c90
				dc.b  $00,$80,$21,$00,$00,$61,$00,$80 ; 0x7c98
				dc.b  $02,$E9,$00,$A3,$00,$00,$2A,$56 ; 0x7ca0
				dc.b  $00,$A7,$19,$FF,$00,$D4,$0A,$43 ; 0x7ca8
				dc.b  $00,$40,$02,$25,$00,$D6,$20,$FF ; 0x7cb0
				dc.b  $30,$FF,$00,$FA,$00,$F0,$03,$10 ; 0x7cb8
				dc.b  $03,$10,$03,$D6,$00,$20,$01,$08 ; 0x7cc0
				dc.b  $00,$FF,$00,$20,$05,$01,$00,$4B ; 0x7cc8
				dc.b  $00,$FF,$02,$92,$00,$00,$0B,$09 ; 0x7cd0
				dc.b  $80,$02,$01,$FF,$00,$FF,$68,$00 ; 0x7cd8
				dc.b  $00,$00,$00,$FF,$00,$FF,$0F,$FF ; 0x7ce0
				dc.b  $FF,$00,$00,$36,$00,$D9,$00,$63 ; 0x7ce8
				dc.b  $0D,$46,$00,$FF,$00,$B9,$1D,$26 ; 0x7cf0
				dc.b  $00,$66,$00,$40,$0D,$92,$00,$C1 ; 0x7cf8
				dc.b  $00,$C0,$0D,$E3,$00,$13,$0E,$26 ; 0x7d00
				dc.b  $00,$FF,$00,$D9,$0D,$30,$00,$9C ; 0x7d08
				dc.b  $00,$9C,$00,$30,$8B,$00,$01,$10 ; 0x7d10
				dc.b  $00,$00,$02,$5A,$00,$20,$02,$11 ; 0x7d18
				dc.b  $00,$00,$02,$85,$00,$00,$00,$00 ; 0x7d20
				dc.b  $01,$85,$00,$00,$00,$00,$0A,$6C ; 0x7d28
				dc.b  $02,$7D,$00,$CF,$CF,$8C,$0C,$00 ; 0x7d30
				dc.b  $FF,$FF,$FF,$00,$00,$09,$00,$EF ; 0x7d38
				dc.b  $00,$61,$0D,$40,$00,$2B,$00,$53 ; 0x7d40
				dc.b  $1D,$D6,$00,$FC,$0E,$C9,$00,$51 ; 0x7d48
				dc.b  $0E,$50,$00,$26,$00,$E0,$0D,$E2 ; 0x7d50
				dc.b  $01,$B3,$0D,$98,$00,$54,$00,$78 ; 0x7d58
				dc.b  $00,$10,$6E,$01,$00,$8E,$01,$1B ; 0x7d60
				dc.b  $00,$D3,$00,$F0,$17,$00,$01,$10 ; 0x7d68
				dc.b  $00,$00,$1A,$BE,$00,$F2,$15,$00 ; 0x7d70
				dc.b  $00,$79,$02,$FF,$00,$CC,$00,$09 ; 0x7d78
				dc.b  $06,$24,$00,$39,$08,$18,$01,$0A ; 0x7d80
				dc.b  $00,$37,$00,$FF,$20,$FF,$00,$95 ; 0x7d88
				dc.b  $00,$00,$05,$FF,$00,$83,$00,$44 ; 0x7d90
				dc.b  $01,$FF,$00,$78,$00,$40,$07,$03 ; 0x7d98
				dc.b  $00,$FE,$19,$01,$01,$A5,$05,$9B ; 0x7da0
				dc.b  $00,$B2,$02,$E5,$00,$AA,$03,$0F ; 0x7da8
				dc.b  $00,$E1,$00,$FF,$00,$FF,$00,$FF ; 0x7db0
				dc.b  $00,$FC,$08,$FF,$00,$FF,$00,$FA ; 0x7db8
				dc.b  $00,$54,$29,$00,$00,$03,$00,$00 ; 0x7dc0
				dc.b  $02,$8C,$0C,$00,$39,$01,$07,$02 ; 0x7dc8
				dc.b  $37,$40,$2F,$01,$00,$80,$00,$00 ; 0x7dd0
				dc.b  $01,$02,$00,$80,$00,$00,$06,$80 ; 0x7dd8
				dc.b  $00,$00,$0A,$80,$00,$00,$28,$01 ; 0x7de0
				dc.b  $00,$04,$00,$18,$00,$E0,$0F,$FF ; 0x7de8
				dc.b  $38,$FF,$00,$FC,$00,$50,$05,$FF ; 0x7df0
				dc.b  $00,$F4,$00,$E0,$39,$FF,$00,$B3 ; 0x7df8
				dc.b  $00,$E0,$2A,$BF,$00,$E8,$02,$C2 ; 0x7e00
				dc.b  $00,$28,$05,$FF,$00,$FF,$00,$FF ; 0x7e08
				dc.b  $00,$FF,$47,$00,$00,$80,$00,$DC ; 0x7e10
				dc.b  $00,$39,$0D,$F4,$00,$22,$00,$5B ; 0x7e18
				dc.b  $0D,$21,$00,$3D,$00,$00,$02,$02 ; 0x7e20
				dc.b  $00,$90,$08,$06,$00,$0E,$00,$7E ; 0x7e28
				dc.b  $0E,$68,$00,$A9,$0E,$3B,$00,$93 ; 0x7e30
				dc.b  $00,$E0,$0D,$12,$01,$BB,$0D,$56 ; 0x7e38
				dc.b  $00,$6E,$00,$F5,$00,$90,$8B,$00 ; 0x7e40
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x7e48
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x7e50
				dc.b  $01,$10,$00,$00,$36,$C0,$00,$2A ; 0x7e58
				dc.b  $06,$57,$00,$80,$11,$00,$00,$00 ; 0x7e60
				dc.b  $00,$D7,$00,$CF,$02,$C7,$00,$BB ; 0x7e68
				dc.b  $38,$00,$00,$36,$00,$5A,$21,$FF ; 0x7e70
				dc.b  $00,$FF,$00,$DF,$00,$50,$01,$FF ; 0x7e78
				dc.b  $00,$EC,$00,$08,$0D,$F4,$00,$E1 ; 0x7e80
				dc.b  $00,$40,$11,$0F,$00,$23,$00,$E0 ; 0x7e88
				dc.b  $00,$FF,$00,$EF,$00,$AD,$00,$01 ; 0x7e90
				dc.b  $0A,$8C,$0C,$00,$38,$02,$07,$08 ; 0x7e98
				dc.b  $13,$00,$00,$00,$62,$02,$03,$10 ; 0x7ea0
				dc.b  $28,$20,$19,$FF,$39,$BF,$00,$FC ; 0x7ea8
				dc.b  $05,$00,$00,$55,$00,$B4,$13,$14 ; 0x7eb0
				dc.b  $00,$DF,$62,$DD,$03,$EB,$0F,$D8 ; 0x7eb8
				dc.b  $12,$08,$00,$6A,$01,$FF,$00,$F0 ; 0x7ec0
				dc.b  $00,$00,$00,$01,$17,$00,$00,$40 ; 0x7ec8
				dc.b  $00,$1E,$00,$45,$0D,$DB,$00,$4F ; 0x7ed0
				dc.b  $00,$7F,$0D,$0E,$00,$42,$04,$90 ; 0x7ed8
				dc.b  $09,$72,$00,$73,$0E,$43,$00,$45 ; 0x7ee0
				dc.b  $0E,$EC,$00,$FD,$00,$F0,$02,$03 ; 0x7ee8
				dc.b  $00,$B0,$09,$31,$00,$56,$00,$5F ; 0x7ef0
				dc.b  $02,$05,$00,$43,$09,$19,$00,$0E ; 0x7ef8
				dc.b  $00,$B3,$00,$50,$8B,$00,$01,$00 ; 0x7f00
				dc.b  $00,$00,$22,$C0,$00,$00,$02,$C0 ; 0x7f08
				dc.b  $00,$00,$2B,$80,$00,$00,$02,$80 ; 0x7f10
				dc.b  $15,$00,$00,$00,$00,$00,$0A,$40 ; 0x7f18
				dc.b  $00,$00,$02,$14,$00,$00,$20,$03 ; 0x7f20
				dc.b  $30,$00,$00,$08,$00,$00,$03,$00 ; 0x7f28
				dc.b  $03,$00,$02,$08,$00,$F0,$00,$00 ; 0x7f30
				dc.b  $02,$F0,$00,$00,$03,$00,$01,$00 ; 0x7f38
				dc.b  $00,$00,$00,$00,$00,$00,$00,$00 ; 0x7f40
				dc.b  $00,$00,$08,$00,$03,$00,$25,$00 ; 0x7f48
				dc.b  $03,$00,$00,$FF,$00,$FF,$45,$00 ; 0x7f50
				dc.b  $00,$FF,$00,$FF,$08,$00,$00,$00 ; 0x7f58
				dc.b  $00,$FF,$00,$FF,$00,$00,$00,$00 ; 0x7f60
				dc.b  $52,$00,$00,$00,$00,$FF,$00,$FF ; 0x7f68
				dc.b  $09,$00,$00,$FF,$00,$FF,$01,$00 ; 0x7f70
				dc.b  $19,$FF,$25,$00,$03,$00,$53,$00 ; 0x7f78
				dc.b  $56,$00,$00,$00,$0B,$00,$00,$00 ; 0x7f80
				dc.b  $02,$00,$00,$00,$18,$00,$00,$00 ; 0x7f88
				dc.b  $00,$40,$00,$00,$0D,$00,$00,$00 ; 0x7f90
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0x7f98
				dc.b  $02,$00,$00,$30,$08,$00,$00,$00 ; 0x7fa0
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0x7fa8
				dc.b  $00,$00,$02,$01,$00,$00,$09,$00 ; 0x7fb0
				dc.b  $00,$00,$00,$00,$02,$00,$00,$E0 ; 0x7fb8
				dc.b  $09,$00,$00,$00,$00,$00,$0D,$00 ; 0x7fc0
				dc.b  $00,$00,$00,$00,$00,$00,$72,$C3 ; 0x7fc8
				dc.b  $18,$00,$05,$46,$00,$10,$16,$C8 ; 0x7fd0
				dc.b  $00,$9A,$02,$74,$00,$C3,$CF,$94 ; 0x7fd8
				dc.b  $00,$0A,$0B,$00,$FF,$FF,$FF,$00 ; 0x7fe0
				dc.b  $00,$EF,$00,$27,$00,$74,$0D,$1D ; 0x7fe8
				dc.b  $00,$58,$00,$1C,$1D,$C4,$00,$27 ; 0x7ff0
				dc.b  $00,$00,$0D,$B0,$00,$34,$0E,$3A ; 0x7ff8
				dc.b  $00,$2D,$00,$80,$0D,$CF,$00,$37 ; 0x8000
				dc.b  $00,$9C,$0D,$80,$00,$7C,$00,$5F ; 0x8008
				dc.b  $00,$40,$6E,$00,$00,$00,$01,$1C ; 0x8010
				dc.b  $00,$04,$00,$00,$17,$00,$25,$41 ; 0x8018
				dc.b  $00,$50,$00,$80,$00,$01,$00,$79 ; 0x8020
				dc.b  $00,$D0,$0A,$92,$00,$FF,$01,$00 ; 0x8028
				dc.b  $00,$00,$00,$00,$06,$00,$00,$00 ; 0x8030
				dc.b  $08,$0A,$00,$FF,$00,$F6,$00,$C0 ; 0x8038
				dc.b  $00,$00,$21,$DB,$06,$00,$00,$00 ; 0x8040
				dc.b  $00,$00,$02,$F3,$00,$00,$08,$FF ; 0x8048
				dc.b  $1B,$03,$05,$B8,$00,$45,$00,$80 ; 0x8050
				dc.b  $01,$DF,$00,$D4,$03,$0A,$00,$0D ; 0x8058
				dc.b  $03,$FD,$08,$00,$00,$00,$00,$00 ; 0x8060
				dc.b  $00,$00,$2A,$9B,$03,$94,$00,$0A ; 0x8068
				dc.b  $0B,$00,$25,$80,$00,$80,$00,$00 ; 0x8070
				dc.b  $02,$80,$00,$00,$0D,$00,$07,$00 ; 0x8078
				dc.b  $0B,$10,$00,$80,$00,$00,$29,$00 ; 0x8080
				dc.b  $2E,$04,$00,$04,$00,$F1,$00,$70 ; 0x8088
				dc.b  $01,$00,$00,$FF,$00,$FF,$02,$80 ; 0x8090
				dc.b  $00,$00,$3A,$38,$00,$A8,$0F,$FF ; 0x8098
				dc.b  $25,$5A,$00,$A8,$01,$01,$00,$79 ; 0x80a0
				dc.b  $00,$D6,$0D,$00,$00,$00,$00,$00 ; 0x80a8
				dc.b  $05,$00,$00,$00,$00,$00,$39,$00 ; 0x80b0
				dc.b  $00,$40,$00,$00,$2A,$AA,$00,$FA ; 0x80b8
				dc.b  $02,$00,$00,$00,$03,$0A,$00,$0E ; 0x80c0
				dc.b  $4B,$00,$00,$66,$00,$14,$00,$4C ; 0x80c8
				dc.b  $0D,$D1,$00,$4F,$00,$24,$1D,$FB ; 0x80d0
				dc.b  $00,$A9,$00,$00,$0D,$4F,$00,$8C ; 0x80d8
				dc.b  $0E,$25,$00,$9A,$00,$80,$0D,$FF ; 0x80e0
				dc.b  $00,$97,$00,$A4,$0D,$3E,$00,$96 ; 0x80e8
				dc.b  $00,$DC,$00,$C0,$8B,$00,$FF,$00 ; 0x80f0
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0x80f8
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$39,$C4 ; 0x8100
				dc.b  $00,$6C,$06,$59,$00,$03,$17,$3E ; 0x8108
				dc.b  $00,$1F,$39,$3B,$00,$8C,$23,$FF ; 0x8110
				dc.b  $00,$40,$02,$FA,$00,$78,$0D,$F5 ; 0x8118
				dc.b  $00,$87,$00,$80,$12,$E6,$00,$40 ; 0x8120
				dc.b  $01,$F5,$00,$1F,$00,$C1,$0A,$94 ; 0x8128
				dc.b  $00,$0A,$0B,$00,$FF,$FF,$FF,$00 ; 0x8130
				dc.b  $00,$09,$00,$3E,$00,$48,$0D,$A7 ; 0x8138
				dc.b  $00,$60,$00,$E8,$1D,$5F,$00,$9E ; 0x8140
				dc.b  $00,$00,$0D,$2A,$00,$28,$0E,$90 ; 0x8148
				dc.b  $00,$63,$00,$00,$0D,$AD,$00,$32 ; 0x8150
				dc.b  $00,$C8,$0D,$01,$00,$36,$00,$9A ; 0x8158
				dc.b  $00,$80,$8B,$00,$FF,$00,$FF,$FF ; 0x8160
				dc.b  $FF,$00,$FF,$00,$05,$5A,$00,$20 ; 0x8168
				dc.b  $16,$C0,$00,$6C,$02,$7D,$00,$CF ; 0x8170
				dc.b  $CF,$9C,$00,$00,$0B,$00,$FF,$FF ; 0x8178
				dc.b  $FF,$00,$00,$E8,$00,$95,$00,$BB ; 0x8180
				dc.b  $0D,$14,$00,$CD,$00,$41,$1D,$BF ; 0x8188
				dc.b  $00,$88,$00,$40,$0D,$AA,$00,$0B ; 0x8190
				dc.b  $0E,$34,$00,$C9,$00,$A0,$0D,$CA ; 0x8198
				dc.b  $00,$86,$00,$61,$0D,$7A,$00,$A3 ; 0x81a0
				dc.b  $00,$11,$00,$B0,$6E,$01,$00,$8E ; 0x81a8
				dc.b  $01,$1B,$00,$D3,$00,$F0,$17,$00 ; 0x81b0
				dc.b  $25,$C7,$00,$98,$00,$00,$00,$00 ; 0x81b8
				dc.b  $00,$C7,$00,$8C,$0A,$79,$00,$9F ; 0x81c0
				dc.b  $01,$FF,$00,$F8,$00,$73,$05,$FF ; 0x81c8
				dc.b  $00,$B6,$00,$04,$09,$00,$00,$0D ; 0x81d0
				dc.b  $00,$03,$00,$FF,$20,$00,$00,$51 ; 0x81d8
				dc.b  $00,$FC,$09,$00,$00,$BA,$00,$FC ; 0x81e0
				dc.b  $08,$FE,$1B,$FF,$05,$BE,$00,$BF ; 0x81e8
				dc.b  $00,$00,$06,$0F,$00,$E2,$03,$FE ; 0x81f0
				dc.b  $35,$01,$00,$0C,$03,$9C,$00,$00 ; 0x81f8
				dc.b  $0B,$00,$25,$00,$00,$FF,$00,$FF ; 0x8200
				dc.b  $02,$FF,$00,$FF,$0D,$01,$07,$02 ; 0x8208
				dc.b  $0B,$00,$00,$FF,$00,$FF,$62,$FF ; 0x8210
				dc.b  $00,$FF,$3A,$18,$00,$E0,$0F,$FF ; 0x8218
				dc.b  $25,$00,$00,$00,$01,$00,$00,$00 ; 0x8220
				dc.b  $00,$00,$0D,$FF,$00,$FC,$00,$50 ; 0x8228
				dc.b  $05,$FF,$00,$F4,$00,$E0,$6F,$00 ; 0x8230
				dc.b  $00,$00,$4B,$00,$00,$5F,$00,$82 ; 0x8238
				dc.b  $00,$93,$0D,$C8,$00,$C4,$00,$49 ; 0x8240
				dc.b  $1D,$F7,$00,$0A,$00,$40,$0D,$49 ; 0x8248
				dc.b  $00,$63,$0E,$20,$00,$36,$00,$A0 ; 0x8250
				dc.b  $0D,$FA,$00,$E6,$00,$69,$0D,$38 ; 0x8258
				dc.b  $00,$BD,$00,$8F,$00,$30,$8B,$00 ; 0x8260
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x8268
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x8270
				dc.b  $39,$BE,$00,$EB,$06,$50,$00,$47 ; 0x8278
				dc.b  $17,$C7,$00,$BB,$39,$4B,$00,$AA ; 0x8280
				dc.b  $23,$66,$00,$20,$02,$E8,$00,$30 ; 0x8288
				dc.b  $0D,$F7,$00,$8B,$00,$40,$11,$00 ; 0x8290
				dc.b  $00,$BA,$00,$00,$01,$E9,$00,$06 ; 0x8298
				dc.b  $00,$01,$0A,$9C,$00,$00,$0B,$00 ; 0x82a0
				dc.b  $FF,$FF,$FF,$00,$00,$BC,$00,$FC ; 0x82a8
				dc.b  $00,$BF,$0D,$5B,$00,$D8,$00,$2D ; 0x82b0
				dc.b  $1D,$5A,$00,$FF,$00,$40,$0D,$23 ; 0x82b8
				dc.b  $00,$FF,$0E,$79,$00,$AB,$00,$D0 ; 0x82c0
				dc.b  $0D,$8C,$00,$C9,$00,$0D,$0D,$FB ; 0x82c8
				dc.b  $00,$5D,$00,$4C,$00,$F0,$8B,$00 ; 0x82d0
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x82d8
				dc.b  $01,$14,$00,$80,$1A,$B7,$00,$96 ; 0x82e0
				dc.b  $02,$79,$00,$AF,$CF,$95,$0C,$00 ; 0x82e8
				dc.b  $FF,$FF,$FF,$00,$00,$43,$00,$55 ; 0x82f0
				dc.b  $00,$64,$0D,$8A,$00,$CE,$00,$EC ; 0x82f8
				dc.b  $1D,$FF,$00,$5B,$00,$00,$0D,$FF ; 0x8300
				dc.b  $00,$24,$0E,$7F,$00,$3F,$00,$80 ; 0x8308
				dc.b  $0D,$0B,$00,$58,$00,$6C,$0D,$CB ; 0x8310
				dc.b  $00,$6F,$00,$1E,$00,$40,$0D,$7E ; 0x8318
				dc.b  $00,$76,$5B,$7E,$00,$76,$02,$00 ; 0x8320
				dc.b  $00,$00,$01,$1C,$00,$04,$00,$00 ; 0x8328
				dc.b  $17,$00,$01,$14,$00,$80,$17,$A8 ; 0x8330
				dc.b  $03,$E0,$06,$B3,$00,$C8,$00,$80 ; 0x8338
				dc.b  $00,$01,$00,$2F,$00,$18,$0A,$48 ; 0x8340
				dc.b  $00,$FF,$01,$00,$00,$3B,$00,$4E ; 0x8348
				dc.b  $07,$60,$08,$21,$01,$1B,$00,$C3 ; 0x8350
				dc.b  $22,$0B,$06,$FF,$00,$83,$00,$8C ; 0x8358
				dc.b  $01,$FF,$00,$43,$00,$60,$08,$FD ; 0x8360
				dc.b  $02,$03,$00,$FF,$17,$03,$05,$8C ; 0x8368
				dc.b  $00,$16,$02,$6B,$00,$4C,$04,$E1 ; 0x8370
				dc.b  $03,$FC,$08,$FF,$00,$FF,$00,$FA ; 0x8378
				dc.b  $00,$54,$29,$02,$00,$99,$00,$80 ; 0x8380
				dc.b  $02,$95,$0C,$00,$25,$80,$53,$40 ; 0x8388
				dc.b  $2E,$00,$00,$01,$00,$00,$00,$00 ; 0x8390
				dc.b  $01,$02,$00,$00,$00,$00,$3E,$22 ; 0x8398
				dc.b  $00,$C0,$0F,$FF,$25,$B4,$00,$F0 ; 0x83a0
				dc.b  $55,$FF,$00,$B3,$00,$E0,$2A,$8C ; 0x83a8
				dc.b  $00,$16,$02,$6B,$00,$4C,$50,$00 ; 0x83b0
				dc.b  $00,$BA,$00,$42,$00,$3C,$0D,$3E ; 0x83b8
				dc.b  $00,$C5,$00,$F4,$1D,$36,$00,$DD ; 0x83c0
				dc.b  $00,$00,$0D,$9E,$00,$7C,$0E,$6A ; 0x83c8
				dc.b  $00,$AC,$00,$80,$0D,$3B,$00,$B8 ; 0x83d0
				dc.b  $00,$74,$0D,$89,$00,$89,$00,$9B ; 0x83d8
				dc.b  $00,$C0,$0B,$08,$7F,$00,$FF,$00 ; 0x83e0
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0x83e8
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$01,$14 ; 0x83f0
				dc.b  $00,$80,$36,$CE,$00,$67,$06,$5C ; 0x83f8
				dc.b  $00,$C8,$52,$5C,$00,$61,$23,$A3 ; 0x8400
				dc.b  $00,$C8,$00,$00,$00,$00,$00,$0A ; 0x8408
				dc.b  $00,$D0,$0D,$F9,$00,$A2,$00,$20 ; 0x8410
				dc.b  $11,$06,$00,$2D,$00,$80,$01,$F1 ; 0x8418
				dc.b  $00,$7F,$00,$C1,$0A,$95,$0C,$00 ; 0x8420
				dc.b  $FF,$FF,$FF,$00,$00,$DA,$00,$67 ; 0x8428
				dc.b  $00,$38,$0D,$6F,$00,$49,$00,$B8 ; 0x8430
				dc.b  $1D,$9A,$00,$D2,$00,$00,$0D,$79 ; 0x8438
				dc.b  $00,$18,$0E,$B3,$00,$78,$00,$00 ; 0x8440
				dc.b  $0D,$4C,$00,$8B,$00,$98,$0D,$4C ; 0x8448
				dc.b  $00,$29,$00,$59,$00,$80,$8B,$00 ; 0x8450
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x8458
				dc.b  $01,$10,$00,$00,$02,$59,$00,$18 ; 0x8460
				dc.b  $06,$84,$00,$FF,$00,$FF,$01,$84 ; 0x8468
				dc.b  $00,$FF,$00,$FF,$09,$C0,$00,$00 ; 0x8470
				dc.b  $02,$7C,$00,$77,$CF,$8B,$00,$10 ; 0x8478
				dc.b  $0B,$00,$FF,$FF,$FF,$00,$00,$AB ; 0x8480
				dc.b  $00,$58,$00,$7D,$0D,$12,$00,$0F ; 0x8488
				dc.b  $00,$E7,$1D,$48,$00,$81,$00,$C0 ; 0x8490
				dc.b  $0D,$60,$00,$AD,$0E,$D4,$00,$97 ; 0x8498
				dc.b  $00,$60,$0D,$55,$00,$A3,$00,$C7 ; 0x84a0
				dc.b  $0D,$28,$00,$0A,$00,$41,$00,$D0 ; 0x84a8
				dc.b  $0D,$80,$00,$00,$5B,$80,$00,$00 ; 0x84b0
				dc.b  $1F,$00,$01,$10,$00,$00,$16,$B4 ; 0x84b8
				dc.b  $00,$7E,$02,$B4,$00,$54,$06,$C7 ; 0x84c0
				dc.b  $00,$98,$00,$00,$00,$00,$00,$C7 ; 0x84c8
				dc.b  $00,$80,$09,$03,$00,$89,$00,$77 ; 0x84d0
				dc.b  $03,$24,$06,$FF,$00,$45,$08,$02 ; 0x84d8
				dc.b  $01,$56,$00,$DB,$07,$88,$00,$7F ; 0x84e0
				dc.b  $18,$FF,$00,$85,$00,$40,$03,$14 ; 0x84e8
				dc.b  $01,$00,$00,$0B,$00,$6A,$02,$A8 ; 0x84f0
				dc.b  $00,$80,$08,$FE,$02,$04,$00,$00 ; 0x84f8
				dc.b  $15,$00,$01,$A5,$05,$BA,$00,$C6 ; 0x8500
				dc.b  $02,$BD,$00,$78,$03,$0A,$00,$A5 ; 0x8508
				dc.b  $04,$FF,$00,$FF,$00,$EA,$00,$20 ; 0x8510
				dc.b  $00,$FF,$00,$FF,$00,$D2,$00,$C0 ; 0x8518
				dc.b  $2D,$01,$00,$AC,$00,$00,$02,$8B ; 0x8520
				dc.b  $00,$10,$0B,$00,$25,$00,$0F,$08 ; 0x8528
				dc.b  $00,$80,$00,$00,$3E,$80,$00,$00 ; 0x8530
				dc.b  $76,$18,$00,$E0,$0F,$FF,$25,$00 ; 0x8538
				dc.b  $00,$00,$0D,$03,$00,$72,$00,$FF ; 0x8540
				dc.b  $00,$FF,$3E,$0C,$32,$BA,$00,$C6 ; 0x8548
				dc.b  $02,$BD,$00,$78,$50,$00,$00,$22 ; 0x8550
				dc.b  $00,$45,$00,$55,$0D,$C6,$00,$06 ; 0x8558
				dc.b  $00,$EF,$1D,$80,$00,$03,$00,$C0 ; 0x8560
				dc.b  $0D,$0C,$00,$CA,$00,$40,$02,$00 ; 0x8568
				dc.b  $00,$90,$09,$CD,$00,$FB,$00,$E0 ; 0x8570
				dc.b  $03,$20,$09,$86,$00,$03,$00,$CF ; 0x8578
				dc.b  $0D,$E6,$00,$24,$00,$BF,$00,$50 ; 0x8580
				dc.b  $0B,$00,$7F,$00,$FF,$00,$FF,$FF ; 0x8588
				dc.b  $FF,$00,$FF,$00,$FF,$00,$FF,$FF ; 0x8590
				dc.b  $FF,$00,$FF,$00,$01,$10,$00,$00 ; 0x8598
				dc.b  $36,$C2,$00,$21,$06,$55,$00,$00 ; 0x85a0
				dc.b  $13,$E6,$00,$2F,$02,$27,$00,$87 ; 0x85a8
				dc.b  $39,$4B,$00,$DA,$21,$00,$00,$00 ; 0x85b0
				dc.b  $00,$65,$00,$38,$00,$FF,$00,$FF ; 0x85b8
				dc.b  $00,$EE,$00,$68,$0D,$F7,$00,$91 ; 0x85c0
				dc.b  $00,$40,$11,$0D,$00,$66,$00,$20 ; 0x85c8
				dc.b  $01,$F5,$00,$C5,$0B,$8B,$00,$10 ; 0x85d0
				dc.b  $0B,$00,$FF,$FF,$E2,$82,$1C,$00 ; 0x85d8
				dc.b  $00,$91,$00,$C8,$00,$21,$06,$19 ; 0x85e0
				dc.b  $00,$6E,$00,$F4,$03,$05,$00,$1B ; 0x85e8
				dc.b  $00,$3C,$00,$93,$06,$19,$00,$6E ; 0x85f0
				dc.b  $00,$F4,$03,$05,$00,$0F,$00,$4F ; 0x85f8
				dc.b  $07,$19,$00,$6E,$00,$F4,$03,$05 ; 0x8600
				dc.b  $00,$E3,$00,$F8,$00,$C0,$0D,$DA ; 0x8608
				dc.b  $00,$A1,$0E,$1B,$00,$21,$00,$30 ; 0x8610
				dc.b  $0D,$CF,$00,$1B,$00,$23,$02,$01 ; 0x8618
				dc.b  $00,$33,$09,$85,$00,$7C,$00,$CD ; 0x8620
				dc.b  $00,$10,$02,$93,$88,$00,$FF,$00 ; 0x8628
				dc.b  $FF,$FF,$FF,$00,$E6,$D3,$18,$00 ; 0x8630
				dc.b  $06,$24,$06,$85,$00,$00,$00,$00 ; 0x8638
				dc.b  $01,$85,$00,$00,$00,$00,$0D,$75 ; 0x8640
				dc.b  $00,$97,$CF,$9F,$00,$00,$0B,$00 ; 0x8648
				dc.b  $FF,$FF,$FF,$00,$00,$6F,$00,$07 ; 0x8650
				dc.b  $00,$FE,$0D,$C3,$00,$A1,$00,$9A ; 0x8658
				dc.b  $1D,$1E,$00,$16,$00,$80,$0D,$28 ; 0x8660
				dc.b  $00,$1E,$0E,$A3,$00,$1A,$00,$40 ; 0x8668
				dc.b  $0D,$2A,$00,$8E,$00,$DA,$0D,$F2 ; 0x8670
				dc.b  $00,$56,$00,$FB,$00,$E0,$6E,$01 ; 0x8678
				dc.b  $00,$8E,$01,$1B,$00,$D3,$00,$F0 ; 0x8680
				dc.b  $17,$00,$19,$C0,$00,$00,$02,$BE ; 0x8688
				dc.b  $00,$F2,$0B,$8C,$0A,$DB,$00,$3F ; 0x8690
				dc.b  $01,$FF,$00,$EB,$00,$91,$06,$D0 ; 0x8698
				dc.b  $00,$BE,$08,$00,$0A,$80,$00,$00 ; 0x86a0
				dc.b  $18,$00,$00,$1D,$00,$FC,$03,$04 ; 0x86a8
				dc.b  $01,$FF,$00,$CD,$00,$A6,$02,$9D ; 0x86b0
				dc.b  $00,$20,$22,$01,$01,$FF,$05,$9F ; 0x86b8
				dc.b  $00,$BA,$02,$99,$00,$42,$09,$00 ; 0x86c0
				dc.b  $00,$00,$00,$10,$00,$00,$00,$00 ; 0x86c8
				dc.b  $00,$02,$00,$00,$00,$00,$2E,$1E ; 0x86d0
				dc.b  $03,$9F,$00,$00,$0B,$00,$35,$00 ; 0x86d8
				dc.b  $00,$FF,$00,$FF,$3E,$FF,$00,$FF ; 0x86e0
				dc.b  $87,$FF,$34,$00,$00,$00,$00,$00 ; 0x86e8
				dc.b  $00,$00,$3E,$00,$32,$9F,$00,$BA ; 0x86f0
				dc.b  $02,$99,$00,$42,$50,$00,$00,$E5 ; 0x86f8
				dc.b  $00,$F4,$00,$D6,$0D,$77,$00,$98 ; 0x8700
				dc.b  $00,$A2,$1D,$55,$00,$98,$00,$80 ; 0x8708
				dc.b  $0D,$C7,$00,$76,$00,$00,$02,$01 ; 0x8710
				dc.b  $00,$00,$09,$8E,$00,$87,$00,$40 ; 0x8718
				dc.b  $03,$E0,$09,$5A,$00,$EE,$00,$E2 ; 0x8720
				dc.b  $0D,$B0,$00,$71,$00,$79,$00,$60 ; 0x8728
				dc.b  $8B,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x8730
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x8738
				dc.b  $FF,$00,$39,$AD,$00,$B4,$06,$5A ; 0x8740
				dc.b  $00,$42,$17,$1C,$00,$43,$39,$30 ; 0x8748
				dc.b  $00,$B7,$23,$A7,$00,$70,$02,$DB ; 0x8750
				dc.b  $00,$20,$0D,$F4,$00,$2C,$00,$E0 ; 0x8758
				dc.b  $11,$10,$00,$47,$00,$60,$01,$EC ; 0x8760
				dc.b  $00,$A6,$00,$01,$0A,$9F,$00,$00 ; 0x8768
				dc.b  $0B,$00,$FF,$FF,$FF,$00,$00,$D5 ; 0x8770
				dc.b  $00,$A5,$00,$F2,$0D,$65,$00,$BB ; 0x8778
				dc.b  $00,$26,$0D,$7E,$00,$61,$00,$20 ; 0x8780
				dc.b  $0D,$B9,$00,$8D,$00,$80,$0D,$A2 ; 0x8788
				dc.b  $00,$12,$0E,$4A,$00,$91,$00,$E0 ; 0x8790
				dc.b  $0D,$8B,$00,$47,$00,$A6,$0D,$64 ; 0x8798
				dc.b  $00,$FF,$00,$27,$00,$20,$8B,$00 ; 0x87a0
				dc.b  $01,$10,$23,$BE,$00,$E6,$03,$9C ; 0x87a8
				dc.b  $2B,$14,$00,$6B,$02,$00,$26,$42 ; 0x87b0
				dc.b  $00,$EC,$20,$FF,$30,$FF,$00,$A1 ; 0x87b8
				dc.b  $00,$30,$03,$08,$03,$08,$02,$09 ; 0x87c0
				dc.b  $00,$F9,$00,$80,$01,$01,$00,$3F ; 0x87c8
				dc.b  $00,$E0,$03,$02,$04,$65,$00,$21 ; 0x87d0
				dc.b  $00,$9F,$08,$03,$03,$09,$D4,$08 ; 0x87d8
				dc.b  $00,$04,$00,$ED,$00,$9F,$09,$40 ; 0x87e0
				dc.b  $03,$80,$19,$FF,$D4,$FF,$00,$E6 ; 0x87e8
				dc.b  $0B,$09,$00,$08,$02,$09,$00,$08 ; 0x87f0
				dc.b  $18,$00,$00,$01,$00,$44,$00,$4A ; 0x87f8
				dc.b  $0C,$06,$00,$1B,$00,$0E,$00,$A9 ; 0x8800
				dc.b  $0D,$03,$00,$A8,$00,$90,$0D,$0E ; 0x8808
				dc.b  $00,$A2,$00,$40,$0D,$13,$00,$83 ; 0x8810
				dc.b  $0E,$11,$00,$12,$00,$A0,$0D,$28 ; 0x8818
				dc.b  $00,$05,$00,$09,$02,$03,$00,$33 ; 0x8820
				dc.b  $09,$2E,$00,$B8,$00,$81,$00,$30 ; 0x8828
				dc.b  $01,$03,$00,$B3,$88,$00,$01,$14 ; 0x8830
				dc.b  $00,$80,$01,$01,$00,$4E,$00,$B0 ; 0x8838
				dc.b  $01,$01,$00,$24,$00,$86,$02,$D5 ; 0x8840
				dc.b  $00,$FF,$00,$FF,$01,$C8,$00,$8F ; 0x8848
				dc.b  $00,$FF,$01,$1D,$00,$08,$0A,$70 ; 0x8850
				dc.b  $00,$6F,$00,$FF,$5D,$1A,$00,$C0 ; 0x8858
				dc.b  $00,$70,$6C,$65,$00,$21,$00,$9C ; 0x8860
				dc.b  $00,$04,$07,$04,$01,$01,$01,$00 ; 0x8868
				dc.b  $15,$40,$00,$80,$00,$00,$02,$80 ; 0x8870
				dc.b  $00,$00,$06,$ED,$00,$9F,$DB,$FF ; 0x8878
				dc.b  $14,$FF,$00,$F9,$00,$40,$02,$C0 ; 0x8880
				dc.b  $07,$30,$00,$9F,$00,$FF,$DB,$00 ; 0x8888
				dc.b  $00,$2B,$00,$23,$00,$B8,$0D,$37 ; 0x8890
				dc.b  $00,$C5,$00,$A8,$0D,$02,$00,$48 ; 0x8898
				dc.b  $00,$20,$02,$01,$00,$60,$08,$06 ; 0x88a0
				dc.b  $00,$1E,$00,$2A,$0E,$28,$00,$38 ; 0x88a8
				dc.b  $0E,$23,$00,$31,$0E,$1E,$00,$A2 ; 0x88b0
				dc.b  $00,$A8,$0C,$08,$00,$26,$00,$2F ; 0x88b8
				dc.b  $00,$AB,$00,$80,$0D,$C7,$00,$95 ; 0x88c0
				dc.b  $3E,$00,$09,$00,$09,$00,$04,$80 ; 0x88c8
				dc.b  $03,$C7,$00,$95,$01,$02,$03,$1C ; 0x88d0
				dc.b  $00,$03,$00,$F8,$01,$02,$01,$0F ; 0x88d8
				dc.b  $13,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x88e0
				dc.b  $E5,$1B,$00,$D3,$00,$F0,$17,$00 ; 0x88e8
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$E0,$FF ; 0x88f0
				dc.b  $00,$FF,$01,$DE,$01,$01,$00,$78 ; 0x88f8
				dc.b  $00,$C0,$17,$00,$FF,$00,$FF,$FF ; 0x8900
				dc.b  $FF,$00,$E5,$1B,$00,$C3,$00,$F0 ; 0x8908
				dc.b  $17,$00,$01,$14,$00,$80,$36,$27 ; 0x8910
				dc.b  $00,$7E,$06,$0B,$00,$A2,$11,$FF ; 0x8918
				dc.b  $00,$FF,$00,$E4,$00,$69,$02,$58 ; 0x8920
				dc.b  $00,$7F,$02,$79,$00,$0B,$02,$7F ; 0x8928
				dc.b  $00,$23,$02,$DE,$00,$8F,$19,$00 ; 0x8930
				dc.b  $13,$06,$00,$DE,$21,$FF,$00,$FE ; 0x8938
				dc.b  $00,$04,$00,$20,$01,$01,$00,$2B ; 0x8940
				dc.b  $00,$10,$0C,$FF,$00,$EE,$00,$ED ; 0x8948
				dc.b  $00,$C0,$11,$01,$00,$DE,$00,$80 ; 0x8950
				dc.b  $01,$0C,$00,$85,$09,$65,$00,$21 ; 0x8958
				dc.b  $00,$9C,$00,$04,$07,$0D,$01,$06 ; 0x8960
				dc.b  $01,$00,$39,$01,$07,$02,$12,$01 ; 0x8968
				dc.b  $00,$04,$10,$80,$00,$00,$2C,$01 ; 0x8970
				dc.b  $00,$04,$23,$40,$03,$01,$0E,$01 ; 0x8978
				dc.b  $00,$04,$13,$40,$03,$80,$19,$FF ; 0x8980
				dc.b  $38,$FF,$00,$FD,$00,$9C,$05,$FF ; 0x8988
				dc.b  $00,$F9,$00,$80,$13,$13,$00,$9B ; 0x8990
				dc.b  $0E,$6A,$00,$D2,$2D,$36,$00,$10 ; 0x8998
				dc.b  $21,$FF,$00,$FF,$00,$CE,$00,$80 ; 0x89a0
				dc.b  $00,$FF,$00,$FF,$00,$DC,$0D,$FF ; 0x89a8
				dc.b  $00,$F4,$00,$D4,$12,$09,$00,$08 ; 0x89b0
				dc.b  $02,$09,$00,$08,$18,$00,$00,$7A ; 0x89b8
				dc.b  $00,$31,$00,$F8,$06,$19,$00,$6F ; 0x89c0
				dc.b  $00,$0C,$03,$05,$00,$38,$00,$5C ; 0x89c8
				dc.b  $00,$68,$06,$19,$00,$6F,$00,$0C ; 0x89d0
				dc.b  $03,$05,$00,$0D,$00,$C1,$00,$90 ; 0x89d8
				dc.b  $02,$02,$00,$60,$08,$06,$00,$BD ; 0x89e0
				dc.b  $00,$DA,$0E,$52,$00,$78,$0E,$08 ; 0x89e8
				dc.b  $00,$29,$0E,$C4,$00,$D1,$00,$68 ; 0x89f0
				dc.b  $0D,$34,$00,$6D,$00,$0F,$00,$80 ; 0x89f8
				dc.b  $71,$1B,$00,$C3,$00,$F0,$17,$00 ; 0x8a00
				dc.b  $01,$14,$00,$80,$22,$96,$00,$BE ; 0x8a08
				dc.b  $00,$80,$01,$BE,$00,$90,$00,$80 ; 0x8a10
				dc.b  $08,$04,$00,$E5,$00,$7F,$00,$FF ; 0x8a18
				dc.b  $01,$7F,$00,$FF,$06,$36,$00,$D8 ; 0x8a20
				dc.b  $08,$10,$01,$0F,$00,$FF,$00,$FF ; 0x8a28
				dc.b  $00,$FF,$00,$FF,$00,$F6,$03,$9B ; 0x8a30
				dc.b  $00,$3F,$02,$88,$00,$7F,$14,$FF ; 0x8a38
				dc.b  $00,$D3,$00,$0C,$01,$FF,$00,$FF ; 0x8a40
				dc.b  $00,$E4,$01,$FF,$00,$C0,$00,$C0 ; 0x8a48
				dc.b  $01,$FF,$00,$EC,$00,$40,$22,$01 ; 0x8a50
				dc.b  $01,$A5,$05,$A3,$00,$98,$02,$D1 ; 0x8a58
				dc.b  $00,$46,$03,$1A,$00,$4C,$00,$FF ; 0x8a60
				dc.b  $00,$FF,$00,$FF,$00,$FF,$0B,$08 ; 0x8a68
				dc.b  $27,$03,$01,$03,$00,$5F,$00,$FF ; 0x8a70
				dc.b  $00,$65,$00,$21,$00,$9C,$00,$04 ; 0x8a78
				dc.b  $07,$0C,$01,$05,$01,$00,$25,$40 ; 0x8a80
				dc.b  $03,$80,$0C,$80,$00,$00,$39,$10 ; 0x8a88
				dc.b  $00,$82,$00,$80,$05,$20,$03,$20 ; 0x8a90
				dc.b  $2A,$08,$00,$04,$02,$02,$00,$04 ; 0x8a98
				dc.b  $51,$FF,$25,$3A,$00,$3E,$02,$8F ; 0x8aa0
				dc.b  $00,$10,$09,$04,$00,$E5,$00,$BF ; 0x8aa8
				dc.b  $00,$FF,$38,$FF,$00,$FE,$00,$60 ; 0x8ab0
				dc.b  $0A,$2B,$00,$80,$2A,$93,$00,$60 ; 0x8ab8
				dc.b  $02,$BE,$00,$38,$03,$08,$00,$FA ; 0x8ac0
				dc.b  $00,$FF,$00,$FF,$00,$FF,$00,$E2 ; 0x8ac8
				dc.b  $08,$FF,$00,$FF,$00,$FF,$00,$1E ; 0x8ad0
				dc.b  $3B,$00,$00,$7D,$00,$8D,$00,$7E ; 0x8ad8
				dc.b  $0D,$3C,$00,$BA,$00,$1A,$0D,$4A ; 0x8ae0
				dc.b  $00,$41,$00,$60,$02,$02,$09,$06 ; 0x8ae8
				dc.b  $00,$C0,$00,$36,$00,$80,$0D,$55 ; 0x8af0
				dc.b  $00,$9E,$0E,$0A,$00,$EA,$00,$40 ; 0x8af8
				dc.b  $0D,$C7,$00,$37,$00,$5A,$0D,$37 ; 0x8b00
				dc.b  $00,$6A,$00,$53,$00,$E0,$71,$1B ; 0x8b08
				dc.b  $00,$C3,$00,$F0,$17,$00,$01,$10 ; 0x8b10
				dc.b  $00,$00,$02,$23,$00,$D8,$02,$15 ; 0x8b18
				dc.b  $00,$50,$01,$02,$00,$7C,$00,$7F ; 0x8b20
				dc.b  $01,$02,$00,$5B,$00,$1F,$01,$FF ; 0x8b28
				dc.b  $00,$F4,$00,$6C,$0A,$77,$00,$8F ; 0x8b30
				dc.b  $CF,$9D,$00,$09,$0B,$00,$FF,$FF ; 0x8b38
				dc.b  $FF,$00,$00,$36,$00,$39,$00,$6D ; 0x8b40
				dc.b  $0D,$46,$00,$2F,$00,$B7,$1D,$25 ; 0x8b48
				dc.b  $00,$F5,$00,$C0,$0D,$32,$00,$9D ; 0x8b50
				dc.b  $0E,$2C,$00,$49,$00,$60,$0D,$26 ; 0x8b58
				dc.b  $00,$8D,$00,$97,$0D,$30,$00,$0E ; 0x8b60
				dc.b  $00,$30,$00,$D0,$0D,$76,$00,$59 ; 0x8b68
				dc.b  $5B,$76,$00,$59,$1F,$00,$FF,$00 ; 0x8b70
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0x8b78
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0x8b80
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$01,$10 ; 0x8b88
				dc.b  $00,$00,$36,$10,$00,$DC,$05,$FF ; 0x8b90
				dc.b  $00,$C3,$00,$EA,$13,$DE,$00,$EA ; 0x8b98
				dc.b  $02,$3D,$00,$DF,$39,$01,$00,$5F ; 0x8ba0
				dc.b  $22,$FD,$00,$D6,$00,$80,$01,$00 ; 0x8ba8
				dc.b  $00,$76,$00,$00,$0E,$3D,$00,$E0 ; 0x8bb0
				dc.b  $12,$28,$00,$00,$01,$10,$00,$9E ; 0x8bb8
				dc.b  $00,$80,$0A,$9D,$00,$09,$0B,$00 ; 0x8bc0
				dc.b  $FF,$FF,$FF,$00,$00,$85,$00,$47 ; 0x8bc8
				dc.b  $00,$AD,$07,$6E,$00,$F4,$04,$46 ; 0x8bd0
				dc.b  $00,$C6,$00,$77,$07,$6E,$00,$F4 ; 0x8bd8
				dc.b  $14,$C5,$00,$A5,$00,$C0,$0D,$5C ; 0x8be0
				dc.b  $00,$DD,$0E,$11,$00,$41,$00,$60 ; 0x8be8
				dc.b  $0D,$CC,$00,$BC,$00,$57,$0D,$3E ; 0x8bf0
				dc.b  $00,$4B,$00,$94,$00,$D0,$8B,$00 ; 0x8bf8
				dc.b  $01,$10,$00,$00,$22,$81,$00,$28 ; 0x8c00
				dc.b  $02,$EB,$00,$6B,$00,$00,$08,$05 ; 0x8c08
				dc.b  $00,$2E,$00,$1F,$14,$03,$24,$00 ; 0x8c10
				dc.b  $00,$02,$00,$F4,$05,$00,$00,$07 ; 0x8c18
				dc.b  $00,$DC,$01,$00,$00,$33,$00,$5C ; 0x8c20
				dc.b  $24,$FF,$06,$D8,$00,$80,$01,$FC ; 0x8c28
				dc.b  $00,$45,$43,$9D,$00,$09,$0B,$00 ; 0x8c30
				dc.b  $FF,$FF,$FF,$00,$00,$88,$00,$A3 ; 0x8c38
				dc.b  $00,$33,$0D,$4B,$00,$24,$00,$29 ; 0x8c40
				dc.b  $1D,$C8,$00,$02,$00,$40,$0D,$60 ; 0x8c48
				dc.b  $00,$03,$0E,$14,$00,$02,$00,$A0 ; 0x8c50
				dc.b  $0D,$CF,$00,$22,$00,$49,$0D,$41 ; 0x8c58
				dc.b  $00,$48,$00,$D9,$00,$30,$8B,$00 ; 0x8c60
				dc.b  $05,$45,$00,$0C,$00,$80,$01,$34 ; 0x8c68
				dc.b  $00,$B0,$00,$80,$00,$00,$00,$DC ; 0x8c70
				dc.b  $03,$E1,$00,$3F,$02,$E0,$00,$24 ; 0x8c78
				dc.b  $0A,$6E,$00,$EF,$CF,$9E,$00,$10 ; 0x8c80
				dc.b  $0B,$00,$FF,$FF,$FF,$00,$00,$4E ; 0x8c88
				dc.b  $00,$65,$00,$C8,$0D,$65,$00,$9E ; 0x8c90
				dc.b  $00,$D8,$1D,$36,$00,$F6,$00,$00 ; 0x8c98
				dc.b  $0D,$49,$00,$48,$0E,$40,$00,$1F ; 0x8ca0
				dc.b  $00,$00,$0D,$37,$00,$D1,$00,$D8 ; 0x8ca8
				dc.b  $0D,$45,$00,$93,$00,$EC,$00,$80 ; 0x8cb0
				dc.b  $0D,$4D,$00,$C8,$3E,$04,$09,$04 ; 0x8cb8
				dc.b  $09,$04,$04,$00,$03,$4D,$00,$C8 ; 0x8cc0
				dc.b  $01,$00,$00,$01,$00,$8E,$01,$1B ; 0x8cc8
				dc.b  $00,$D3,$00,$F0,$01,$00,$01,$00 ; 0x8cd0
				dc.b  $13,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x8cd8
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x8ce0
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x8ce8
				dc.b  $FF,$00,$39,$04,$00,$02,$06,$ED ; 0x8cf0
				dc.b  $00,$58,$13,$E7,$00,$AB,$02,$88 ; 0x8cf8
				dc.b  $3A,$0A,$00,$20,$22,$FF,$00,$20 ; 0x8d00
				dc.b  $00,$A0,$02,$0F,$00,$30,$0D,$EF ; 0x8d08
				dc.b  $00,$56,$00,$00,$11,$06,$00,$68 ; 0x8d10
				dc.b  $00,$80,$01,$0B,$00,$F8,$00,$C0 ; 0x8d18
				dc.b  $0A,$9E,$00,$10,$0B,$00,$FF,$FF ; 0x8d20
				dc.b  $E2,$20,$03,$20,$18,$00,$00,$9D ; 0x8d28
				dc.b  $00,$74,$00,$08,$0D,$66,$00,$35 ; 0x8d30
				dc.b  $00,$98,$1D,$D6,$00,$A6,$00,$00 ; 0x8d38
				dc.b  $0D,$73,$00,$88,$0E,$25,$00,$17 ; 0x8d40
				dc.b  $00,$00,$0D,$4A,$00,$8E,$00,$28 ; 0x8d48
				dc.b  $02,$06,$00,$F3,$09,$E2,$00,$70 ; 0x8d50
				dc.b  $00,$D0,$00,$80,$01,$09,$00,$23 ; 0x8d58
				dc.b  $88,$00,$25,$7C,$00,$1E,$02,$A3 ; 0x8d60
				dc.b  $00,$36,$00,$80,$08,$03,$00,$42 ; 0x8d68
				dc.b  $00,$7F,$39,$FF,$00,$C3,$00,$C0 ; 0x8d70
				dc.b  $05,$FF,$00,$97,$00,$80,$01,$FF ; 0x8d78
				dc.b  $00,$C3,$00,$00,$22,$00,$07,$AB ; 0x8d80
				dc.b  $00,$B7,$00,$00,$02,$7B,$01,$FF ; 0x8d88
				dc.b  $00,$FF,$00,$ED,$00,$D7,$39,$08 ; 0x8d90
				dc.b  $00,$99,$03,$9E,$00,$10,$0B,$00 ; 0x8d98
				dc.b  $24,$01,$00,$04,$02,$10,$00,$04 ; 0x8da0
				dc.b  $C4,$83,$00,$9C,$0F,$FF,$26,$38 ; 0x8da8
				dc.b  $C6,$03,$00,$5F,$00,$FF,$0F,$00 ; 0x8db0
				dc.b  $00,$A0,$00,$CF,$00,$8E,$0D,$6A ; 0x8db8
				dc.b  $00,$93,$00,$4A,$1D,$D9,$01,$80 ; 0x8dc0
				dc.b  $0D,$76,$00,$AE,$0E,$27,$00,$D8 ; 0x8dc8
				dc.b  $00,$40,$0D,$E0,$00,$66,$00,$8A ; 0x8dd0
				dc.b  $0D,$56,$00,$CE,$00,$94,$00,$E0 ; 0x8dd8
				dc.b  $8B,$00,$01,$14,$00,$80,$03,$08 ; 0x8de0
				dc.b  $00,$00,$02,$A0,$00,$00,$00,$01 ; 0x8de8
				dc.b  $00,$E3,$00,$0F,$01,$01,$00,$E2 ; 0x8df0
				dc.b  $00,$7F,$01,$00,$00,$21,$00,$A8 ; 0x8df8
				dc.b  $0A,$75,$00,$F3,$CF,$85,$00,$00 ; 0x8e00
				dc.b  $0B,$00,$FF,$FF,$FF,$00,$00,$78 ; 0x8e08
				dc.b  $00,$C1,$00,$00,$0D,$9C,$00,$B3 ; 0x8e10
				dc.b  $00,$00,$1D,$54,$00,$C0,$0E,$71 ; 0x8e18
				dc.b  $00,$00,$0E,$62,$00,$E0,$0E,$56 ; 0x8e20
				dc.b  $00,$13,$00,$00,$0D,$6B,$00,$4A ; 0x8e28
				dc.b  $00,$10,$00,$00,$0D,$D0,$00,$D6 ; 0x8e30
				dc.b  $1A,$82,$00,$46,$3B,$82,$00,$46 ; 0x8e38
				dc.b  $02,$D0,$00,$D6,$1F,$00,$01,$14 ; 0x8e40
				dc.b  $00,$80,$23,$AE,$02,$A4,$00,$9A ; 0x8e48
				dc.b  $2B,$00,$03,$00,$26,$16,$00,$20 ; 0x8e50
				dc.b  $20,$FF,$31,$4C,$00,$70,$0A,$06 ; 0x8e58
				dc.b  $00,$4B,$00,$40,$01,$05,$00,$18 ; 0x8e60
				dc.b  $04,$02,$04,$65,$00,$21,$00,$85 ; 0x8e68
				dc.b  $08,$03,$03,$09,$D4,$01,$00,$04 ; 0x8e70
				dc.b  $00,$17,$00,$64,$09,$01,$00,$80 ; 0x8e78
				dc.b  $00,$00,$01,$02,$00,$80,$00,$00 ; 0x8e80
				dc.b  $17,$FF,$D4,$FF,$00,$F4,$0B,$09 ; 0x8e88
				dc.b  $00,$08,$02,$09,$00,$08,$18,$00 ; 0x8e90
				dc.b  $00,$2B,$00,$E0,$00,$79,$0D,$38 ; 0x8e98
				dc.b  $00,$BB,$00,$1B,$0E,$43,$00,$B0 ; 0x8ea0
				dc.b  $0C,$06,$00,$1E,$00,$AE,$00,$C0 ; 0x8ea8
				dc.b  $0D,$28,$00,$E9,$0E,$23,$00,$CB ; 0x8eb0
				dc.b  $00,$E0,$0D,$1F,$00,$29,$00,$7B ; 0x8eb8
				dc.b  $0D,$26,$00,$D7,$00,$B9,$00,$90 ; 0x8ec0
				dc.b  $0D,$00,$7D,$00,$FF,$00,$FF,$FF ; 0x8ec8
				dc.b  $FF,$00,$FF,$00,$FF,$00,$FF,$FF ; 0x8ed0
				dc.b  $FF,$00,$FF,$00,$01,$14,$00,$80 ; 0x8ed8
				dc.b  $22,$99,$00,$F0,$02,$AE,$00,$DE ; 0x8ee0
				dc.b  $0E,$00,$00,$00,$05,$00,$00,$00 ; 0x8ee8
				dc.b  $00,$00,$11,$00,$00,$00,$00,$00 ; 0x8ef0
				dc.b  $00,$00,$02,$00,$00,$00,$02,$80 ; 0x8ef8
				dc.b  $00,$00,$02,$80,$00,$00,$02,$80 ; 0x8f00
				dc.b  $00,$00,$19,$14,$13,$00,$00,$00 ; 0x8f08
				dc.b  $0C,$FF,$14,$00,$00,$00,$00,$10 ; 0x8f10
				dc.b  $00,$00,$01,$02,$00,$00,$00,$00 ; 0x8f18
				dc.b  $0C,$00,$00,$02,$00,$15,$00,$55 ; 0x8f20
				dc.b  $05,$26,$00,$F0,$03,$08,$03,$08 ; 0x8f28
				dc.b  $02,$04,$00,$14,$00,$00,$00,$FF ; 0x8f30
				dc.b  $00,$FF,$00,$7A,$00,$00,$03,$02 ; 0x8f38
				dc.b  $06,$85,$00,$00,$07,$03,$01,$00 ; 0x8f40
				dc.b  $01,$09,$39,$00,$07,$00,$12,$00 ; 0x8f48
				dc.b  $00,$00,$10,$FF,$00,$FF,$2C,$00 ; 0x8f50
				dc.b  $00,$00,$23,$00,$03,$00,$0E,$00 ; 0x8f58
				dc.b  $00,$00,$06,$04,$00,$04,$00,$2F ; 0x8f60
				dc.b  $00,$00,$09,$00,$03,$00,$19,$FF ; 0x8f68
				dc.b  $38,$00,$00,$00,$00,$00,$05,$00 ; 0x8f70
				dc.b  $00,$00,$00,$00,$13,$00,$00,$00 ; 0x8f78
				dc.b  $0E,$00,$00,$00,$2D,$00,$00,$00 ; 0x8f80
				dc.b  $21,$00,$00,$00,$00,$00,$00,$00 ; 0x8f88
				dc.b  $00,$00,$00,$00,$00,$00,$0D,$00 ; 0x8f90
				dc.b  $00,$00,$00,$00,$05,$FF,$00,$F4 ; 0x8f98
				dc.b  $0B,$00,$00,$00,$02,$00,$00,$00 ; 0x8fa0
				dc.b  $18,$00,$00,$22,$00,$E7,$00,$6F ; 0x8fa8
				dc.b  $06,$1E,$00,$75,$00,$40,$03,$00 ; 0x8fb0
				dc.b  $00,$2D,$00,$10,$00,$1D,$06,$1E ; 0x8fb8
				dc.b  $00,$75,$00,$40,$03,$00,$00,$00 ; 0x8fc0
				dc.b  $00,$3F,$00,$70,$02,$01,$00,$B0 ; 0x8fc8
				dc.b  $09,$18,$00,$5F,$00,$40,$0D,$20 ; 0x8fd0
				dc.b  $00,$7F,$0E,$1C,$00,$6F,$00,$20 ; 0x8fd8
				dc.b  $0D,$18,$00,$C0,$00,$BD,$02,$00 ; 0x8fe0
				dc.b  $00,$C3,$09,$1E,$00,$DA,$00,$94 ; 0x8fe8
				dc.b  $00,$F0,$01,$00,$00,$F3,$0A,$00 ; 0x8ff0
				dc.b  $64,$D3,$18,$00,$01,$14,$00,$80 ; 0x8ff8
				dc.b  $22,$C7,$00,$F8,$00,$00,$01,$90 ; 0x9000
				dc.b  $00,$76,$09,$00,$00,$00,$00,$00 ; 0x9008
				dc.b  $00,$00,$01,$00,$00,$00,$06,$00 ; 0x9010
				dc.b  $00,$00,$08,$08,$01,$08,$00,$00 ; 0x9018
				dc.b  $00,$00,$00,$00,$00,$00,$00,$00 ; 0x9020
				dc.b  $03,$00,$00,$00,$02,$00,$00,$00 ; 0x9028
				dc.b  $14,$00,$00,$00,$00,$00,$01,$00 ; 0x9030
				dc.b  $00,$00,$00,$04,$01,$00,$00,$00 ; 0x9038
				dc.b  $00,$00,$01,$00,$00,$40,$03,$0A ; 0x9040
				dc.b  $00,$EB,$26,$C0,$00,$00,$02,$C0 ; 0x9048
				dc.b  $00,$00,$01,$00,$00,$00,$00,$08 ; 0x9050
				dc.b  $00,$00,$00,$00,$00,$00,$00,$00 ; 0x9058
				dc.b  $00,$00,$00,$FF,$00,$FD,$00,$6B ; 0x9060
				dc.b  $00,$C0,$02,$56,$00,$60,$03,$00 ; 0x9068
				dc.b  $11,$33,$00,$E0,$09,$FF,$00,$F9 ; 0x9070
				dc.b  $00,$3C,$00,$80,$01,$0E,$00,$79 ; 0x9078
				dc.b  $04,$02,$01,$00,$00,$00,$00,$00 ; 0x9080
				dc.b  $02,$85,$00,$00,$07,$03,$01,$00 ; 0x9088
				dc.b  $01,$09,$24,$00,$00,$01,$00,$80 ; 0x9090
				dc.b  $00,$00,$00,$00,$00,$02,$00,$80 ; 0x9098
				dc.b  $00,$00,$0A,$FF,$00,$FF,$39,$00 ; 0x90a0
				dc.b  $00,$FF,$00,$FF,$05,$00,$03,$00 ; 0x90a8
				dc.b  $02,$10,$00,$04,$26,$00,$00,$00 ; 0x90b0
				dc.b  $02,$00,$00,$00,$26,$08,$00,$04 ; 0x90b8
				dc.b  $00,$65,$00,$A0,$16,$FF,$00,$FF ; 0x90c0
				dc.b  $0F,$FF,$25,$C0,$00,$78,$02,$BF ; 0x90c8
				dc.b  $0A,$00,$00,$00,$00,$00,$00,$00 ; 0x90d0
				dc.b  $38,$00,$00,$00,$00,$00,$0A,$00 ; 0x90d8
				dc.b  $00,$00,$2A,$00,$00,$00,$02,$00 ; 0x90e0
				dc.b  $00,$00,$03,$00,$00,$00,$00,$00 ; 0x90e8
				dc.b  $00,$00,$00,$00,$00,$00,$08,$00 ; 0x90f0
				dc.b  $00,$00,$00,$00,$00,$00,$10,$FF ; 0x90f8
				dc.b  $00,$F6,$17,$00,$00,$00,$00,$00 ; 0x9100
				dc.b  $0F,$00,$00,$1E,$00,$06,$00,$AC ; 0x9108
				dc.b  $0D,$26,$00,$B8,$00,$44,$0D,$01 ; 0x9110
				dc.b  $00,$42,$00,$F0,$02,$00,$0A,$14 ; 0x9118
				dc.b  $00,$F1,$00,$00,$0D,$1B,$00,$EC ; 0x9120
				dc.b  $0E,$18,$00,$6E,$00,$80,$0D,$15 ; 0x9128
				dc.b  $00,$44,$00,$C4,$0D,$1A,$00,$82 ; 0x9130
				dc.b  $00,$C2,$00,$C0,$0D,$00,$64,$D3 ; 0x9138
				dc.b  $18,$00,$01,$10,$00,$00,$03,$80 ; 0x9140
				dc.b  $02,$1F,$00,$46,$02,$85,$00,$00 ; 0x9148
				dc.b  $00,$00,$01,$85,$00,$00,$00,$00 ; 0x9150
				dc.b  $01,$00,$00,$00,$0A,$79,$00,$9F ; 0x9158
				dc.b  $6C,$01,$62,$90,$0C,$00,$15,$00 ; 0x9160
				dc.b  $00,$FF,$00,$FF,$02,$FF,$00,$FF ; 0x9168
				dc.b  $06,$FF,$00,$FF,$DB,$FF,$14,$00 ; 0x9170
				dc.b  $00,$00,$00,$00,$02,$00,$07,$00 ; 0x9178
				dc.b  $00,$00,$00,$00,$DB,$00,$00,$31 ; 0x9180
				dc.b  $00,$95,$00,$73,$0D,$40,$00,$26 ; 0x9188
				dc.b  $00,$E9,$0D,$08,$00,$AC,$00,$90 ; 0x9190
				dc.b  $02,$00,$00,$30,$08,$00,$00,$22 ; 0x9198
				dc.b  $00,$B2,$00,$40,$0D,$2E,$00,$43 ; 0x91a0
				dc.b  $0E,$28,$00,$7A,$00,$A0,$0D,$23 ; 0x91a8
				dc.b  $00,$3D,$00,$09,$0C,$00,$00,$2B ; 0x91b0
				dc.b  $00,$EC,$00,$7D,$00,$30,$0D,$8C ; 0x91b8
				dc.b  $00,$9E,$5B,$8C,$00,$9E,$02,$00 ; 0x91c0
				dc.b  $00,$00,$01,$1C,$00,$04,$00,$00 ; 0x91c8
				dc.b  $17,$00,$01,$10,$00,$00,$6D,$FF ; 0x91d0
				dc.b  $00,$C1,$00,$80,$62,$8C,$00,$90 ; 0x91d8
				dc.b  $0A,$07,$00,$70,$00,$00,$01,$0C ; 0x91e0
				dc.b  $00,$47,$00,$E0,$0A,$90,$0C,$09 ; 0x91e8
				dc.b  $71,$20,$00,$80,$00,$00,$62,$2B ; 0x91f0
				dc.b  $00,$84,$27,$FF,$E2,$38,$03,$38 ; 0x91f8
				dc.b  $18,$00,$00,$95,$00,$93,$00,$00 ; 0x9200
				dc.b  $02,$04,$00,$E1,$09,$E8,$00,$5B ; 0x9208
				dc.b  $00,$80,$02,$09,$00,$13,$19,$41 ; 0x9210
				dc.b  $00,$C4,$00,$00,$0D,$57,$00,$B0 ; 0x9218
				dc.b  $0E,$4C,$00,$BA,$00,$00,$0D,$42 ; 0x9220
				dc.b  $00,$CB,$00,$10,$0D,$53,$00,$41 ; 0x9228
				dc.b  $00,$8B,$00,$00,$8B,$00,$FF,$00 ; 0x9230
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0x9238
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$01,$10 ; 0x9240
				dc.b  $00,$00,$6E,$2A,$00,$02,$61,$02 ; 0x9248
				dc.b  $00,$85,$00,$00,$0A,$01,$00,$63 ; 0x9250
				dc.b  $00,$A0,$00,$00,$00,$06,$00,$DF ; 0x9258
				dc.b  $00,$80,$0A,$90,$0C,$09,$71,$40 ; 0x9260
				dc.b  $00,$80,$00,$00,$62,$D5,$00,$24 ; 0x9268
				dc.b  $09,$01,$00,$C1,$00,$68,$01,$02 ; 0x9270
				dc.b  $19,$FF,$E1,$04,$00,$3C,$01,$FF ; 0x9278
				dc.b  $00,$FF,$00,$92,$18,$00,$00,$9C ; 0x9280
				dc.b  $00,$94,$00,$16,$02,$07,$00,$31 ; 0x9288
				dc.b  $09,$C9,$00,$50,$00,$12,$02,$09 ; 0x9290
				dc.b  $1A,$3B,$00,$74,$00,$80,$0D,$4F ; 0x9298
				dc.b  $00,$46,$0E,$45,$00,$5D,$00,$40 ; 0x92a0
				dc.b  $0D,$3C,$00,$62,$00,$52,$0D,$4B ; 0x92a8
				dc.b  $00,$44,$00,$66,$00,$60,$8B,$00 ; 0x92b0
				dc.b  $01,$10,$00,$00,$22,$C4,$00,$33 ; 0x92b8
				dc.b  $00,$80,$01,$94,$00,$83,$00,$00 ; 0x92c0
				dc.b  $55,$06,$00,$C5,$52,$48,$00,$40 ; 0x92c8
				dc.b  $0B,$93,$00,$A0,$01,$0B,$00,$0A ; 0x92d0
				dc.b  $00,$40,$0A,$90,$0C,$09,$E1,$01 ; 0x92d8
				dc.b  $00,$80,$00,$00,$01,$02,$00,$80 ; 0x92e0
				dc.b  $00,$00,$17,$FF,$E0,$FF,$00,$F9 ; 0x92e8
				dc.b  $00,$44,$02,$0E,$00,$96,$18,$00 ; 0x92f0
				dc.b  $00,$80,$00,$03,$00,$53,$02,$07 ; 0x92f8
				dc.b  $00,$41,$09,$8B,$00,$D2,$00,$89 ; 0x9300
				dc.b  $02,$06,$00,$33,$19,$38,$00,$06 ; 0x9308
				dc.b  $00,$40,$0D,$4A,$00,$B3,$0E,$41 ; 0x9310
				dc.b  $00,$5C,$00,$A0,$0D,$38,$00,$E6 ; 0x9318
				dc.b  $00,$59,$0D,$46,$00,$EC,$00,$94 ; 0x9320
				dc.b  $00,$30,$8B,$00,$01,$14,$00,$80 ; 0x9328
				dc.b  $02,$48,$00,$B0,$02,$0F,$00,$50 ; 0x9330
				dc.b  $16,$55,$00,$D3,$5F,$D0,$0C,$00 ; 0x9338
				dc.b  $6F,$00,$FF,$FF,$FF,$00,$00,$48 ; 0x9340
				dc.b  $00,$D2,$00,$EE,$0D,$5E,$00,$5F ; 0x9348
				dc.b  $00,$6A,$0D,$0C,$00,$C2,$00,$A0 ; 0x9350
				dc.b  $0D,$33,$00,$0A,$00,$80,$0D,$44 ; 0x9358
				dc.b  $00,$0E,$0E,$3B,$00,$8C,$00,$40 ; 0x9360
				dc.b  $0D,$33,$00,$D6,$00,$AA,$0D,$40 ; 0x9368
				dc.b  $00,$9D,$00,$8A,$00,$E0,$8B,$00 ; 0x9370
				dc.b  $01,$14,$00,$80,$53,$14,$00,$03 ; 0x9378
				dc.b  $18,$00,$00,$06,$00,$8E,$0E,$4D ; 0x9380
				dc.b  $00,$E8,$52,$96,$00,$60,$0A,$0C ; 0x9388
				dc.b  $00,$DD,$00,$40,$01,$06,$00,$69 ; 0x9390
				dc.b  $00,$00,$05,$01,$00,$D9,$00,$FF ; 0x9398
				dc.b  $0F,$09,$FF,$FF,$E2,$20,$03,$20 ; 0x93a0
				dc.b  $18,$00,$00,$91,$00,$4D,$00,$55 ; 0x93a8
				dc.b  $02,$01,$00,$11,$09,$BC,$00,$9E ; 0x93b0
				dc.b  $00,$EF,$02,$01,$00,$63,$19,$66 ; 0x93b8
				dc.b  $00,$03,$00,$C0,$0D,$88,$00,$05 ; 0x93c0
				dc.b  $0E,$77,$00,$04,$00,$60,$0D,$67 ; 0x93c8
				dc.b  $00,$9B,$00,$CF,$0D,$81,$00,$25 ; 0x93d0
				dc.b  $00,$3F,$00,$50,$8B,$00,$FF,$00 ; 0x93d8
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0x93e0
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$01,$14 ; 0x93e8
				dc.b  $00,$80,$53,$48,$00,$D7,$02,$41 ; 0x93f0
				dc.b  $00,$67,$15,$3C,$00,$06,$0E,$BC ; 0x93f8
				dc.b  $00,$38,$52,$2F,$00,$C0,$0A,$09 ; 0x9400
				dc.b  $00,$62,$00,$00,$00,$FF,$00,$FD ; 0x9408
				dc.b  $00,$43,$00,$20,$05,$01,$00,$EB ; 0x9410
				dc.b  $00,$FF,$0F,$09,$FF,$FF,$E2,$28 ; 0x9418
				dc.b  $03,$86,$18,$00,$00,$88,$00,$54 ; 0x9420
				dc.b  $00,$4B,$02,$01,$00,$11,$09,$B0 ; 0x9428
				dc.b  $00,$F3,$00,$F1,$02,$01,$1A,$5F ; 0x9430
				dc.b  $00,$B4,$00,$40,$0D,$7F,$00,$9B ; 0x9438
				dc.b  $0E,$6F,$00,$A7,$00,$A0,$0D,$61 ; 0x9440
				dc.b  $00,$33,$00,$11,$0D,$79,$00,$28 ; 0x9448
				dc.b  $00,$1A,$00,$B0,$8B,$00,$01,$14 ; 0x9450
				dc.b  $00,$80,$22,$D8,$00,$C0,$00,$00 ; 0x9458
				dc.b  $01,$96,$00,$34,$00,$80,$2A,$31 ; 0x9460
				dc.b  $00,$87,$29,$3E,$00,$FE,$52,$4F ; 0x9468
				dc.b  $00,$50,$03,$08,$03,$08,$03,$3C ; 0x9470
				dc.b  $00,$80,$01,$0E,$00,$79,$00,$00 ; 0x9478
				dc.b  $05,$01,$00,$5D,$00,$FF,$0F,$09 ; 0x9480
				dc.b  $26,$DD,$00,$AC,$02,$AD,$00,$B8 ; 0x9488
				dc.b  $54,$00,$55,$49,$00,$9C,$09,$00 ; 0x9490
				dc.b  $00,$FF,$00,$FF,$01,$00,$00,$FF ; 0x9498
				dc.b  $00,$FF,$17,$FF,$E0,$00,$00,$00 ; 0x94a0
				dc.b  $00,$00,$02,$00,$00,$00,$18,$00 ; 0x94a8
				dc.b  $00,$83,$00,$73,$00,$88,$02,$01 ; 0x94b0
				dc.b  $00,$11,$09,$AA,$00,$9C,$00,$18 ; 0x94b8
				dc.b  $02,$01,$00,$63,$19,$5C,$00,$46 ; 0x94c0
				dc.b  $00,$00,$0D,$7B,$00,$08,$0E,$6B ; 0x94c8
				dc.b  $00,$A7,$00,$00,$0D,$5D,$00,$B7 ; 0x94d0
				dc.b  $00,$18,$0D,$74,$00,$D0,$00,$48 ; 0x94d8
				dc.b  $00,$80,$8B,$00,$05,$67,$00,$C4 ; 0x94e0
				dc.b  $02,$00,$00,$38,$01,$02,$00,$0E ; 0x94e8
				dc.b  $00,$4F,$00,$FF,$00,$02,$00,$19 ; 0x94f0
				dc.b  $00,$CF,$00,$FF,$0D,$68,$00,$5B ; 0x94f8
				dc.b  $CF,$8B,$00,$06,$0B,$00,$FF,$FF ; 0x9500
				dc.b  $FF,$00,$00,$54,$00,$0C,$00,$E5 ; 0x9508
				dc.b  $0D,$6C,$00,$F8,$00,$9F,$0D,$0E ; 0x9510
				dc.b  $00,$BB,$00,$F0,$0D,$3A,$00,$EF ; 0x9518
				dc.b  $00,$C0,$0D,$4E,$00,$95,$0E,$44 ; 0x9520
				dc.b  $00,$C2,$00,$60,$0D,$3B,$00,$DB ; 0x9528
				dc.b  $00,$7F,$0D,$4A,$00,$9C,$00,$58 ; 0x9530
				dc.b  $00,$50,$8B,$00,$4B,$02,$01,$19 ; 0x9538
				dc.b  $00,$5B,$00,$FF,$21,$3E,$00,$BE ; 0x9540
				dc.b  $2E,$01,$01,$03,$05,$AE,$00,$15 ; 0x9548
				dc.b  $02,$FB,$00,$38,$00,$80,$25,$98 ; 0x9550
				dc.b  $00,$A0,$0A,$08,$00,$68,$00,$E0 ; 0x9558
				dc.b  $01,$0D,$00,$01,$06,$00,$00,$00 ; 0x9560
				dc.b  $00,$00,$02,$8B,$00,$06,$0B,$09 ; 0x9568
				dc.b  $A9,$01,$03,$02,$00,$9F,$00,$80 ; 0x9570
				dc.b  $4F,$FF,$A9,$BF,$00,$40,$02,$C1 ; 0x9578
				dc.b  $00,$38,$50,$00,$00,$9C,$00,$87 ; 0x9580
				dc.b  $00,$4C,$0D,$CB,$00,$38,$00,$24 ; 0x9588
				dc.b  $1D,$6D,$00,$E9,$00,$00,$0D,$92 ; 0x9590
				dc.b  $00,$8C,$0E,$80,$00,$3A,$00,$80 ; 0x9598
				dc.b  $0D,$6F,$00,$A0,$00,$A4,$0D,$8B ; 0x95a0
				dc.b  $00,$24,$00,$0C,$00,$C0,$8B,$00 ; 0x95a8
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x95b0
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x95b8
				dc.b  $24,$01,$00,$1B,$00,$39,$00,$80 ; 0x95c0
				dc.b  $00,$01,$00,$0D,$00,$EB,$20,$02 ; 0x95c8
				dc.b  $01,$2F,$00,$C7,$00,$FF,$06,$EC ; 0x95d0
				dc.b  $00,$BB,$19,$3D,$00,$F0,$05,$FF ; 0x95d8
				dc.b  $00,$EE,$00,$66,$06,$7F,$00,$EC ; 0x95e0
				dc.b  $1E,$01,$01,$03,$05,$F0,$00,$A8 ; 0x95e8
				dc.b  $02,$B5,$00,$CE,$26,$E1,$00,$70 ; 0x95f0
				dc.b  $0A,$02,$00,$16,$00,$E0,$01,$FC ; 0x95f8
				dc.b  $00,$FE,$00,$E0,$05,$00,$00,$00 ; 0x9600
				dc.b  $00,$00,$02,$8B,$00,$06,$0B,$09 ; 0x9608
				dc.b  $25,$40,$03,$80,$4F,$02,$00,$80 ; 0x9610
				dc.b  $00,$00,$83,$FF,$25,$BE,$00,$50 ; 0x9618
				dc.b  $02,$C0,$00,$C0,$4E,$02,$00,$A0 ; 0x9620
				dc.b  $84,$00,$00,$93,$00,$8E,$00,$42 ; 0x9628
				dc.b  $0D,$BF,$00,$8D,$00,$26,$1D,$67 ; 0x9630
				dc.b  $00,$99,$00,$80,$0D,$8A,$00,$22 ; 0x9638
				dc.b  $0E,$78,$00,$DD,$00,$C0,$0D,$69 ; 0x9640
				dc.b  $00,$37,$00,$E6,$0D,$83,$00,$26 ; 0x9648
				dc.b  $00,$E8,$00,$20,$8B,$00,$25,$76 ; 0x9650
				dc.b  $00,$71,$02,$FD,$00,$4D,$00,$00 ; 0x9658
				dc.b  $1F,$02,$01,$24,$00,$8F,$00,$FF ; 0x9660
				dc.b  $51,$01,$01,$03,$31,$56,$00,$30 ; 0x9668
				dc.b  $16,$00,$00,$00,$00,$00,$02,$8B ; 0x9670
				dc.b  $00,$06,$0B,$09,$FF,$FF,$FF,$00 ; 0x9678
				dc.b  $00,$8E,$00,$AD,$00,$7F,$0D,$B9 ; 0x9680
				dc.b  $00,$35,$00,$4D,$1D,$64,$00,$2B ; 0x9688
				dc.b  $00,$40,$0D,$85,$00,$8F,$0E,$74 ; 0x9690
				dc.b  $00,$DD,$00,$20,$0D,$65,$00,$BB ; 0x9698
				dc.b  $00,$ED,$0D,$7E,$00,$CF,$00,$15 ; 0x96a0
				dc.b  $00,$F0,$8B,$00,$05,$5A,$00,$80 ; 0x96a8
				dc.b  $02,$09,$00,$68,$02,$25,$00,$EF ; 0x96b0
				dc.b  $02,$2A,$00,$3F,$02,$01,$00,$42 ; 0x96b8
				dc.b  $0A,$69,$00,$BF,$CF,$85,$00,$00 ; 0x96c0
				dc.b  $0B,$00,$15,$20,$00,$80,$00,$00 ; 0x96c8
				dc.b  $E7,$FF,$FF,$00,$00,$07,$00,$30 ; 0x96d0
				dc.b  $00,$A2,$0D,$09,$00,$06,$00,$46 ; 0x96d8
				dc.b  $0D,$01,$00,$38,$00,$60,$0D,$04 ; 0x96e0
				dc.b  $00,$E1,$00,$80,$0D,$06,$00,$82 ; 0x96e8
				dc.b  $0E,$05,$00,$B1,$00,$C0,$0C,$08 ; 0x96f0
				dc.b  $00,$04,$00,$F5,$00,$06,$0D,$06 ; 0x96f8
				dc.b  $00,$2D,$00,$CE,$00,$20,$0D,$82 ; 0x9700
				dc.b  $00,$88,$1A,$99,$00,$FA,$3B,$99 ; 0x9708
				dc.b  $00,$FA,$02,$82,$00,$88,$02,$01 ; 0x9710
				dc.b  $00,$8E,$01,$1B,$00,$E3,$00,$F0 ; 0x9718
				dc.b  $17,$00,$4B,$08,$24,$FF,$00,$F2 ; 0x9720
				dc.b  $00,$F0,$0E,$95,$00,$A8,$1E,$00 ; 0x9728
				dc.b  $01,$FF,$05,$C1,$00,$42,$00,$80 ; 0x9730
				dc.b  $01,$BB,$00,$08,$00,$00,$25,$96 ; 0x9738
				dc.b  $00,$60,$0A,$09,$00,$4D,$00,$60 ; 0x9740
				dc.b  $01,$08,$00,$CE,$00,$20,$0A,$85 ; 0x9748
				dc.b  $00,$00,$0B,$09,$FF,$FF,$E2,$38 ; 0x9750
				dc.b  $03,$38,$18,$00,$00,$55,$00,$7E ; 0x9758
				dc.b  $00,$C5,$02,$06,$00,$81,$09,$D1 ; 0x9760
				dc.b  $00,$84,$00,$4F,$02,$09,$00,$53 ; 0x9768
				dc.b  $19,$8C,$00,$2B,$00,$C0,$0D,$BA ; 0x9770
				dc.b  $00,$E5,$0E,$A3,$00,$88,$00,$60 ; 0x9778
				dc.b  $0D,$1B,$00,$CE,$00,$8F,$02,$06 ; 0x9780
				dc.b  $00,$03,$09,$63,$00,$55,$00,$0D ; 0x9788
				dc.b  $00,$50,$01,$07,$00,$93,$88,$00 ; 0x9790
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x9798
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x97a0
				dc.b  $24,$00,$00,$A2,$00,$9D,$01,$00 ; 0x97a8
				dc.b  $00,$D7,$00,$73,$20,$05,$24,$FF ; 0x97b0
				dc.b  $00,$ED,$00,$88,$05,$00,$00,$2A ; 0x97b8
				dc.b  $00,$AC,$06,$C8,$00,$B8,$1E,$00 ; 0x97c0
				dc.b  $01,$FF,$30,$01,$00,$56,$00,$D0 ; 0x97c8
				dc.b  $0A,$05,$00,$3F,$01,$00,$00,$04 ; 0x97d0
				dc.b  $00,$93,$00,$A0,$0A,$85,$00,$00 ; 0x97d8
				dc.b  $0B,$09,$FF,$FF,$E2,$3C,$03,$92 ; 0x97e0
				dc.b  $18,$00,$00,$C8,$00,$07,$00,$2B ; 0x97e8
				dc.b  $02,$0B,$00,$B1,$09,$5F,$00,$63 ; 0x97f0
				dc.b  $00,$F1,$02,$05,$00,$93,$19,$85 ; 0x97f8
				dc.b  $00,$DC,$00,$40,$0D,$B2,$00,$7B ; 0x9800
				dc.b  $0E,$9C,$00,$2B,$00,$A0,$0D,$87 ; 0x9808
				dc.b  $00,$F3,$00,$B1,$0D,$A9,$00,$75 ; 0x9810
				dc.b  $01,$B0,$8B,$00,$25,$71,$00,$A0 ; 0x9818
				dc.b  $02,$F5,$00,$3A,$20,$08,$55,$00 ; 0x9820
				dc.b  $01,$FF,$31,$24,$00,$F0,$03,$10 ; 0x9828
				dc.b  $03,$10,$13,$85,$00,$00,$0B,$09 ; 0x9830
				dc.b  $FF,$FF,$FF,$00,$00,$B9,$00,$B4 ; 0x9838
				dc.b  $00,$68,$0D,$F1,$00,$28,$00,$B8 ; 0x9840
				dc.b  $1D,$82,$00,$6E,$00,$00,$0D,$AD ; 0x9848
				dc.b  $00,$E8,$0E,$98,$00,$2B,$00,$00 ; 0x9850
				dc.b  $0D,$84,$00,$77,$00,$B8,$0D,$A5 ; 0x9858
				dc.b  $00,$1E,$00,$16,$00,$80,$8B,$00 ; 0x9860
				dc.b  $04,$00,$00,$00,$00,$00,$01,$00 ; 0x9868
				dc.b  $00,$F6,$00,$30,$02,$1F,$00,$BF ; 0x9870
				dc.b  $02,$26,$02,$FF,$00,$FF,$00,$F6 ; 0x9878
				dc.b  $02,$BF,$00,$E2,$03,$48,$02,$59 ; 0x9880
				dc.b  $00,$57,$CF,$86,$0C,$00,$0D,$40 ; 0x9888
				dc.b  $00,$00,$00,$00,$01,$40,$00,$00 ; 0x9890
				dc.b  $00,$00,$EB,$FF,$0C,$02,$00,$1F ; 0x9898
				dc.b  $00,$BF,$00,$FF,$00,$02,$00,$26 ; 0x98a0
				dc.b  $00,$3F,$00,$FF,$01,$0D,$00,$E0 ; 0x98a8
				dc.b  $E8,$00,$00,$9B,$00,$48,$00,$71 ; 0x98b0
				dc.b  $0D,$C9,$00,$99,$00,$83,$0D,$1B ; 0x98b8
				dc.b  $00,$42,$00,$30,$0D,$6D,$00,$08 ; 0x98c0
				dc.b  $00,$C0,$0D,$91,$00,$61,$0E,$7F ; 0x98c8
				dc.b  $00,$34,$00,$E0,$0D,$6E,$00,$BC ; 0x98d0
				dc.b  $00,$E3,$0D,$8A,$00,$08,$00,$29 ; 0x98d8
				dc.b  $00,$10,$0D,$64,$00,$2C,$5B,$64 ; 0x98e0
				dc.b  $00,$2C,$02,$17,$00,$D8,$1B,$00 ; 0x98e8
				dc.b  $25,$BD,$00,$D8,$02,$BB,$00,$2C ; 0x98f0
				dc.b  $0D,$FF,$00,$FB,$00,$F3,$06,$02 ; 0x98f8
				dc.b  $00,$90,$2E,$C7,$00,$80,$36,$B4 ; 0x9900
				dc.b  $00,$18,$00,$00,$01,$ED,$00,$B1 ; 0x9908
				dc.b  $26,$9D,$00,$F0,$0A,$08,$00,$C1 ; 0x9910
				dc.b  $00,$00,$01,$0C,$00,$31,$00,$60 ; 0x9918
				dc.b  $0A,$86,$0C,$09,$FF,$FF,$FF,$00 ; 0x9920
				dc.b  $00,$01,$00,$84,$00,$B8,$0D,$C8 ; 0x9928
				dc.b  $00,$22,$00,$98,$1D,$A0,$00,$02 ; 0x9930
				dc.b  $00,$00,$0D,$D5,$00,$58,$0E,$BA ; 0x9938
				dc.b  $00,$AD,$00,$00,$0D,$BA,$00,$CF ; 0x9940
				dc.b  $00,$E8,$0D,$2B,$00,$AB,$00,$BD ; 0x9948
				dc.b  $00,$80,$8B,$00,$01,$14,$00,$80 ; 0x9950
				dc.b  $22,$E8,$00,$39,$00,$80,$01,$94 ; 0x9958
				dc.b  $00,$4D,$0E,$0E,$00,$AC,$06,$7F ; 0x9960
				dc.b  $00,$FF,$13,$00,$03,$00,$15,$FF ; 0x9968
				dc.b  $00,$F2,$00,$E4,$0E,$07,$00,$6C ; 0x9970
				dc.b  $20,$FF,$35,$02,$00,$B0,$03,$87 ; 0x9978
				dc.b  $00,$80,$0B,$02,$04,$65,$00,$21 ; 0x9980
				dc.b  $00,$86,$08,$03,$03,$09,$25,$01 ; 0x9988
				dc.b  $00,$80,$00,$00,$01,$02,$00,$85 ; 0x9990
				dc.b  $00,$14,$45,$20,$00,$24,$00,$30 ; 0x9998
				dc.b  $0C,$10,$00,$04,$7D,$FF,$25,$C0 ; 0x99a0
				dc.b  $03,$C0,$D5,$00,$00,$1D,$00,$56 ; 0x99a8
				dc.b  $00,$B7,$0D,$25,$00,$D3,$00,$75 ; 0x99b0
				dc.b  $0D,$04,$00,$42,$00,$70,$02,$02 ; 0x99b8
				dc.b  $00,$20,$08,$06,$00,$14,$00,$75 ; 0x99c0
				dc.b  $00,$40,$0D,$1B,$00,$47,$0E,$17 ; 0x99c8
				dc.b  $00,$DE,$00,$20,$0D,$14,$00,$C7 ; 0x99d0
				dc.b  $00,$15,$0D,$19,$00,$E6,$00,$19 ; 0x99d8
				dc.b  $00,$70,$0D,$64,$00,$2C,$1A,$99 ; 0x99e0
				dc.b  $00,$FA,$3B,$99,$00,$FA,$02,$64 ; 0x99e8
				dc.b  $00,$2C,$00,$00,$00,$02,$01,$00 ; 0x99f0
				dc.b  $01,$1B,$00,$E3,$00,$F0,$17,$00 ; 0x99f8
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x9a00
				dc.b  $25,$F5,$00,$7C,$00,$00,$01,$96 ; 0x9a08
				dc.b  $00,$A2,$45,$00,$00,$24,$00,$C8 ; 0x9a10
				dc.b  $05,$FF,$00,$CA,$00,$1C,$59,$02 ; 0x9a18
				dc.b  $00,$08,$00,$90,$0A,$0A,$00,$2E ; 0x9a20
				dc.b  $01,$FF,$00,$F8,$00,$81,$0B,$86 ; 0x9a28
				dc.b  $0C,$09,$FF,$FF,$FF,$00,$00,$FD ; 0x9a30
				dc.b  $00,$41,$00,$AE,$0D,$F2,$00,$D2 ; 0x9a38
				dc.b  $00,$FA,$1D,$99,$00,$B2,$00,$80 ; 0x9a40
				dc.b  $0D,$CC,$00,$EE,$0E,$B3,$00,$50 ; 0x9a48
				dc.b  $00,$40,$0D,$9C,$00,$19,$00,$4A ; 0x9a50
				dc.b  $0D,$C2,$00,$92,$00,$B8,$00,$E0 ; 0x9a58
				dc.b  $8B,$00,$25,$95,$00,$0A,$02,$88 ; 0x9a60
				dc.b  $00,$4A,$AA,$38,$00,$A0,$1B,$86 ; 0x9a68
				dc.b  $0C,$09,$FF,$FF,$FF,$00,$00,$D5 ; 0x9a70
				dc.b  $00,$E9,$00,$0B,$0D,$15,$00,$D6 ; 0x9a78
				dc.b  $00,$31,$1D,$96,$00,$44,$00,$40 ; 0x9a80
				dc.b  $0D,$C8,$00,$5B,$0E,$AF,$00,$4F ; 0x9a88
				dc.b  $00,$A0,$0D,$98,$00,$9D,$00,$51 ; 0x9a90
				dc.b  $0D,$BE,$00,$3A,$00,$E6,$00,$B0 ; 0x9a98
				dc.b  $8B,$00,$01,$10,$07,$F6,$00,$30 ; 0x9aa0
				dc.b  $01,$02,$00,$1F,$00,$BF,$00,$FF ; 0x9aa8
				dc.b  $00,$02,$00,$26,$00,$3F,$00,$FF ; 0x9ab0
				dc.b  $00,$FF,$00,$F0,$00,$6A,$02,$BF ; 0x9ab8
				dc.b  $00,$E2,$03,$48,$02,$59,$00,$57 ; 0x9ac0
				dc.b  $00,$FF,$5D,$1A,$00,$D0,$00,$70 ; 0x9ac8
				dc.b  $6C,$65,$00,$21,$00,$81,$08,$04 ; 0x9ad0
				dc.b  $01,$01,$01,$00,$0D,$40,$00,$00 ; 0x9ad8
				dc.b  $00,$00,$01,$40,$00,$00,$00,$00 ; 0x9ae0
				dc.b  $01,$20,$00,$80,$00,$00,$E7,$FF ; 0x9ae8
				dc.b  $0C,$02,$00,$1F,$00,$BF,$00,$FF ; 0x9af0
				dc.b  $00,$02,$00,$26,$00,$3F,$00,$FF ; 0x9af8
				dc.b  $01,$0D,$00,$E0,$E8,$00,$00,$EE ; 0x9b00
				dc.b  $00,$7E,$00,$F9,$0D,$35,$00,$CE ; 0x9b08
				dc.b  $00,$9B,$0D,$29,$00,$E3,$00,$B0 ; 0x9b10
				dc.b  $0D,$A7,$00,$8E,$00,$C0,$0D,$DF ; 0x9b18
				dc.b  $00,$69,$0E,$C3,$00,$7B,$00,$E0 ; 0x9b20
				dc.b  $0C,$08,$00,$AA,$00,$2C,$00,$FB ; 0x9b28
				dc.b  $0D,$D4,$00,$1E,$00,$A1,$00,$90 ; 0x9b30
				dc.b  $0D,$45,$00,$15,$1A,$8A,$00,$CA ; 0x9b38
				dc.b  $3B,$8A,$00,$CA,$02,$45,$00,$15 ; 0x9b40
				dc.b  $02,$37,$00,$AA,$01,$1B,$00,$E3 ; 0x9b48
				dc.b  $00,$F0,$17,$00,$01,$10,$33,$A9 ; 0x9b50
				dc.b  $00,$F0,$4A,$9B,$69,$03,$04,$65 ; 0x9b58
				dc.b  $00,$21,$00,$81,$08,$02,$03,$00 ; 0x9b60
				dc.b  $35,$04,$C9,$FF,$FF,$00,$00,$54 ; 0x9b68
				dc.b  $00,$3C,$00,$E2,$0D,$6D,$00,$37 ; 0x9b70
				dc.b  $00,$06,$0D,$0B,$00,$AA,$00,$80 ; 0x9b78
				dc.b  $03,$E0,$03,$78,$00,$4C,$03,$03 ; 0x9b80
				dc.b  $00,$3B,$00,$11,$00,$80,$0D,$4E ; 0x9b88
				dc.b  $00,$C2,$0E,$44,$00,$E9,$00,$C0 ; 0x9b90
				dc.b  $0D,$3B,$00,$FD,$00,$C6,$0D,$4A ; 0x9b98
				dc.b  $00,$C7,$00,$12,$00,$20,$0D,$64 ; 0x9ba0
				dc.b  $00,$2C,$1A,$99,$00,$FA,$3B,$99 ; 0x9ba8
				dc.b  $00,$FA,$02,$64,$00,$2C,$03,$9C ; 0x9bb0
				dc.b  $01,$1B,$00,$E3,$00,$F0,$17,$00 ; 0x9bb8
				dc.b  $01,$10,$23,$93,$00,$CF,$02,$C7 ; 0x9bc0
				dc.b  $00,$38,$16,$7F,$00,$FF,$13,$1B ; 0x9bc8
				dc.b  $00,$F6,$02,$00,$15,$FF,$00,$92 ; 0x9bd0
				dc.b  $0F,$00,$56,$FF,$00,$FC,$00,$F8 ; 0x9bd8
				dc.b  $03,$08,$01,$FF,$00,$FE,$00,$52 ; 0x9be0
				dc.b  $00,$A0,$00,$FF,$00,$FF,$00,$08 ; 0x9be8
				dc.b  $00,$20,$03,$02,$04,$65,$00,$21 ; 0x9bf0
				dc.b  $00,$81,$08,$03,$03,$09,$25,$01 ; 0x9bf8
				dc.b  $03,$02,$00,$E4,$00,$A4,$45,$10 ; 0x9c00
				dc.b  $0E,$40,$00,$04,$00,$99,$00,$D4 ; 0x9c08
				dc.b  $5D,$01,$00,$80,$00,$00,$01,$02 ; 0x9c10
				dc.b  $00,$80,$00,$00,$17,$FF,$25,$C0 ; 0x9c18
				dc.b  $03,$C0,$B8,$2A,$01,$FF,$00,$FE ; 0x9c20
				dc.b  $00,$B2,$18,$00,$00,$45,$00,$36 ; 0x9c28
				dc.b  $00,$5B,$0C,$08,$00,$59,$00,$AD ; 0x9c30
				dc.b  $00,$21,$0C,$07,$00,$06,$00,$43 ; 0x9c38
				dc.b  $00,$E0,$02,$04,$00,$90,$08,$06 ; 0x9c40
				dc.b  $00,$30,$00,$80,$00,$40,$0D,$40 ; 0x9c48
				dc.b  $00,$AB,$0E,$38,$00,$95,$00,$A0 ; 0x9c50
				dc.b  $0D,$31,$00,$42,$00,$41,$0D,$3D ; 0x9c58
				dc.b  $00,$66,$00,$5B,$00,$B0,$0D,$64 ; 0x9c60
				dc.b  $00,$2C,$1A,$99,$00,$FA,$3B,$99 ; 0x9c68
				dc.b  $00,$FA,$02,$64,$00,$2C,$02,$02 ; 0x9c70
				dc.b  $00,$19,$01,$1B,$00,$E3,$00,$F0 ; 0x9c78
				dc.b  $17,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x9c80
				dc.b  $E5,$1B,$00,$C3,$00,$F0,$17,$00 ; 0x9c88
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$81,$64 ; 0x9c90
				dc.b  $00,$2C,$1A,$99,$00,$FA,$3B,$99 ; 0x9c98
				dc.b  $00,$FA,$02,$64,$00,$2C,$02,$02 ; 0x9ca0
				dc.b  $00,$19,$01,$1B,$00,$E3,$00,$F0 ; 0x9ca8
				dc.b  $17,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x9cb0
				dc.b  $81,$64,$00,$2C,$1A,$99,$00,$FA ; 0x9cb8
				dc.b  $3B,$99,$00,$FA,$02,$64,$00,$2C ; 0x9cc0
				dc.b  $02,$02,$00,$19,$01,$1B,$00,$E3 ; 0x9cc8
				dc.b  $00,$F0,$17,$00,$14,$00,$00,$00 ; 0x9cd0
				dc.b  $00,$E4,$DB,$80,$00,$02,$0B,$00 ; 0x9cd8
				dc.b  $FF,$FF,$FF,$00,$00,$FF,$00,$41 ; 0x9ce0
				dc.b  $00,$20,$0D,$4B,$00,$99,$00,$60 ; 0x9ce8
				dc.b  $0D,$2C,$00,$D6,$00,$00,$0D,$B3 ; 0x9cf0
				dc.b  $00,$58,$00,$00,$0D,$EF,$00,$20 ; 0x9cf8
				dc.b  $0E,$D1,$00,$3C,$00,$00,$0D,$B6 ; 0x9d00
				dc.b  $00,$25,$00,$60,$0D,$E3,$00,$0A ; 0x9d08
				dc.b  $00,$52,$00,$00,$0D,$66,$00,$09 ; 0x9d10
				dc.b  $1A,$47,$00,$A3,$3B,$47,$00,$A3 ; 0x9d18
				dc.b  $02,$66,$00,$09,$02,$08,$00,$E5 ; 0x9d20
				dc.b  $1B,$00,$34,$FF,$00,$AA,$00,$80 ; 0x9d28
				dc.b  $6C,$A5,$4E,$80,$00,$02,$0B,$00 ; 0x9d30
				dc.b  $FF,$FF,$FF,$00,$00,$64,$00,$FF ; 0x9d38
				dc.b  $00,$09,$0D,$83,$00,$01,$00,$CB ; 0x9d40
				dc.b  $0D,$19,$00,$6A,$00,$A0,$0D,$46 ; 0x9d48
				dc.b  $00,$DA,$00,$C0,$0D,$5E,$00,$79 ; 0x9d50
				dc.b  $0E,$52,$00,$A9,$00,$E0,$0D,$47 ; 0x9d58
				dc.b  $00,$F6,$00,$2B,$0D,$59,$00,$B2 ; 0x9d60
				dc.b  $00,$C2,$00,$90,$8B,$00,$25,$AC ; 0x9d68
				dc.b  $00,$86,$02,$9A,$00,$3E,$45,$00 ; 0x9d70
				dc.b  $00,$54,$00,$FC,$30,$A5,$3D,$FF ; 0x9d78
				dc.b  $00,$5A,$00,$40,$01,$FC,$00,$EF ; 0x9d80
				dc.b  $00,$00,$0A,$80,$00,$02,$0B,$09 ; 0x9d88
				dc.b  $FF,$FF,$FF,$00,$00,$55,$00,$F8 ; 0x9d90
				dc.b  $00,$82,$0D,$6F,$00,$77,$00,$E6 ; 0x9d98
				dc.b  $1D,$3C,$00,$49,$00,$80,$0D,$50 ; 0x9da0
				dc.b  $00,$62,$0E,$46,$00,$55,$00,$C0 ; 0x9da8
				dc.b  $0D,$3D,$00,$3A,$00,$A6,$0D,$4C ; 0x9db0
				dc.b  $00,$52,$00,$0C,$00,$20,$8B,$00 ; 0x9db8
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x9dc0
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x9dc8
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0x9dd0
				dc.b  $01,$14,$00,$80,$12,$27,$00,$34 ; 0x9dd8
				dc.b  $DB,$92,$00,$1E,$0B,$00,$FF,$FF ; 0x9de0
				dc.b  $FF,$00,$00,$12,$00,$48,$00,$78 ; 0x9de8
				dc.b  $0D,$64,$00,$57,$00,$E8,$0D,$30 ; 0x9df0
				dc.b  $00,$2E,$00,$80,$0D,$C0,$00,$BA ; 0x9df8
				dc.b  $0E,$00,$00,$F8,$0E,$E0,$00,$D9 ; 0x9e00
				dc.b  $0E,$C3,$00,$BC,$00,$E8,$0D,$F3 ; 0x9e08
				dc.b  $00,$FB,$00,$77,$00,$80,$0D,$B2 ; 0x9e10
				dc.b  $00,$AD,$1A,$46,$00,$12,$3B,$46 ; 0x9e18
				dc.b  $00,$12,$02,$B2,$00,$AD,$02,$25 ; 0x9e20
				dc.b  $00,$64,$1B,$00,$01,$14,$00,$80 ; 0x9e28
				dc.b  $31,$FE,$00,$1E,$4B,$6A,$00,$28 ; 0x9e30
				dc.b  $20,$FF,$4E,$92,$00,$1E,$0B,$00 ; 0x9e38
				dc.b  $FF,$FF,$FF,$00,$00,$78,$00,$06 ; 0x9e40
				dc.b  $00,$61,$0D,$9B,$00,$C0,$00,$53 ; 0x9e48
				dc.b  $0D,$29,$00,$07,$0E,$54,$00,$3C ; 0x9e50
				dc.b  $0E,$70,$00,$51,$0E,$62,$00,$46 ; 0x9e58
				dc.b  $0E,$55,$00,$8D,$00,$B3,$0D,$6A ; 0x9e60
				dc.b  $00,$A3,$00,$E8,$00,$10,$8B,$00 ; 0x9e68
				dc.b  $01,$14,$00,$80,$22,$E5,$00,$FF ; 0x9e70
				dc.b  $00,$80,$01,$99,$00,$31,$00,$80 ; 0x9e78
				dc.b  $21,$07,$00,$FF,$00,$FF,$02,$08 ; 0x9e80
				dc.b  $03,$05,$00,$BF,$19,$7C,$31,$FF ; 0x9e88
				dc.b  $35,$FF,$00,$76,$01,$FF,$00,$FE ; 0x9e90
				dc.b  $00,$6C,$01,$00,$00,$02,$00,$5D ; 0x9e98
				dc.b  $02,$FD,$00,$6E,$00,$80,$0A,$92 ; 0x9ea0
				dc.b  $00,$1E,$0B,$09,$82,$77,$00,$48 ; 0x9ea8
				dc.b  $7B,$FF,$E2,$C8,$02,$FF,$00,$3E ; 0x9eb0
				dc.b  $18,$00,$00,$68,$00,$FF,$00,$DA ; 0x9eb8
				dc.b  $0D,$88,$00,$36,$00,$6E,$1D,$49 ; 0x9ec0
				dc.b  $00,$AB,$0E,$62,$00,$3A,$0E,$55 ; 0x9ec8
				dc.b  $00,$F2,$0E,$4A,$00,$D2,$00,$2E ; 0x9ed0
				dc.b  $0D,$5D,$00,$43,$00,$31,$00,$A0 ; 0x9ed8
				dc.b  $8B,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x9ee0
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x9ee8
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0x9ef0
				dc.b  $FF,$00,$14,$FF,$00,$D8,$00,$38 ; 0x9ef8
				dc.b  $DB,$86,$00,$0C,$0B,$00,$FF,$FF ; 0x9f00
				dc.b  $FF,$00,$00,$26,$00,$05,$00,$1A ; 0x9f08
				dc.b  $0D,$7E,$00,$02,$00,$2E,$0D,$33 ; 0x9f10
				dc.b  $00,$A6,$00,$E0,$0D,$CE,$00,$9B ; 0x9f18
				dc.b  $00,$80,$0D,$13,$00,$7A,$0E,$F1 ; 0x9f20
				dc.b  $00,$0A,$00,$C0,$0D,$D1,$00,$D5 ; 0x9f28
				dc.b  $00,$EE,$0D,$05,$00,$8E,$00,$05 ; 0x9f30
				dc.b  $00,$A0,$0D,$14,$00,$B0,$1A,$83 ; 0x9f38
				dc.b  $00,$95,$3B,$83,$00,$95,$02,$14 ; 0x9f40
				dc.b  $00,$B0,$02,$02,$00,$B6,$1B,$00 ; 0x9f48
				dc.b  $35,$E5,$4B,$9B,$00,$00,$20,$03 ; 0x9f50
				dc.b  $4E,$86,$00,$0C,$0B,$00,$FF,$FF ; 0x9f58
				dc.b  $FF,$00,$00,$8B,$00,$C3,$00,$03 ; 0x9f60
				dc.b  $0D,$B5,$00,$6A,$00,$99,$0D,$39 ; 0x9f68
				dc.b  $00,$39,$00,$60,$0D,$62,$00,$1E ; 0x9f70
				dc.b  $00,$40,$0D,$82,$00,$D3,$0E,$72 ; 0x9f78
				dc.b  $00,$78,$00,$A0,$0D,$63,$00,$A6 ; 0x9f80
				dc.b  $00,$B9,$0D,$7C,$00,$36,$00,$76 ; 0x9f88
				dc.b  $00,$30,$8B,$00,$25,$6F,$00,$84 ; 0x9f90
				dc.b  $00,$00,$01,$C2,$00,$65,$0E,$08 ; 0x9f98
				dc.b  $00,$90,$06,$7A,$00,$FC,$08,$05 ; 0x9fa0
				dc.b  $02,$CB,$03,$00,$03,$1B,$00,$F6 ; 0x9fa8
				dc.b  $18,$FF,$00,$EA,$00,$30,$2E,$01 ; 0x9fb0
				dc.b  $01,$03,$0E,$0C,$00,$82,$25,$FE ; 0x9fb8
				dc.b  $03,$FB,$00,$8C,$01,$FF,$00,$FC ; 0x9fc0
				dc.b  $00,$CF,$00,$80,$01,$FE,$00,$CE ; 0x9fc8
				dc.b  $0B,$86,$00,$0C,$0B,$09,$72,$2C ; 0x9fd0
				dc.b  $00,$50,$0E,$20,$00,$1C,$52,$80 ; 0x9fd8
				dc.b  $00,$00,$27,$FF,$D5,$08,$0C,$2A ; 0x9fe0
				dc.b  $02,$FE,$00,$B2,$18,$00,$00,$7C ; 0x9fe8
				dc.b  $00,$BC,$00,$7C,$0D,$A1,$00,$E0 ; 0x9ff0
				dc.b  $00,$B4,$1D,$57,$00,$8D,$00,$00 ; 0x9ff8
				dc.b  $0D,$74,$00,$BC,$0E,$66,$00,$24 ; 0xa000
				dc.b  $00,$80,$0D,$58,$00,$EB,$00,$34 ; 0xa008
				dc.b  $0D,$6E,$00,$D5,$00,$BF,$00,$C0 ; 0xa010
				dc.b  $8B,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0xa018
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0xa020
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0xa028
				dc.b  $FF,$00,$04,$01,$00,$46,$00,$40 ; 0xa030
				dc.b  $01,$01,$00,$0C,$00,$98,$01,$01 ; 0xa038
				dc.b  $00,$85,$00,$00,$00,$00,$00,$01 ; 0xa040
				dc.b  $00,$85,$00,$00,$00,$00,$00,$00 ; 0xa048
				dc.b  $00,$00,$00,$00,$02,$C0,$00,$00 ; 0xa050
				dc.b  $03,$00,$02,$75,$00,$03,$6C,$01 ; 0xa058
				dc.b  $62,$84,$00,$00,$0B,$00,$0D,$00 ; 0xa060
				dc.b  $00,$FF,$00,$FF,$01,$00,$00,$FF ; 0xa068
				dc.b  $00,$FF,$01,$00,$00,$FF,$00,$FF ; 0xa070
				dc.b  $E7,$FF,$0C,$00,$00,$00,$00,$00 ; 0xa078
				dc.b  $00,$00,$00,$00,$00,$00,$00,$00 ; 0xa080
				dc.b  $00,$00,$01,$00,$00,$00,$E8,$00 ; 0xa088
				dc.b  $00,$14,$00,$C6,$00,$2F,$0D,$1A ; 0xa090
				dc.b  $00,$B0,$00,$5D,$0D,$03,$00,$9B ; 0xa098
				dc.b  $00,$D0,$0D,$0E,$00,$6F,$00,$40 ; 0xa0a0

				dc.b  $0E,$3F,$0E,$10,$00,$D7,$00,$20 ; 0xa0a8
				dc.b  $0C,$00,$00,$0E,$00,$A8,$00,$FD ; 0xa0b0
				dc.b  $0D,$12,$00,$46,$00,$00,$00,$F0 ; 0xa0b8
				dc.b  $0D,$D7,$00,$30,$1A,$AE,$00,$CC ; 0xa0c0
				dc.b  $3B,$AE,$00,$CC,$02,$D7,$00,$30 ; 0xa0c8
				dc.b  $02,$0E,$00,$09,$1B,$00,$01,$00 ; 0xa0d0
				dc.b  $00,$00,$31,$00,$00,$00,$00,$00 ; 0xa0d8
				dc.b  $4A,$14,$69,$00,$04,$00,$00,$00 ; 0xa0e0
				dc.b  $00,$00,$00,$00,$07,$00,$03,$00 ; 0xa0e8
				dc.b  $35,$00,$C9,$FF,$FF,$00,$00,$00 ; 0xa0f0
				dc.b  $00,$40,$00,$00,$0D,$00,$00,$00 ; 0xa0f8
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0xa100
				dc.b  $03,$30,$03,$55,$00,$38,$03,$00 ; 0xa108
				dc.b  $00,$00,$00,$00,$00,$00,$0D,$00 ; 0xa110
				dc.b  $00,$00,$0E,$00,$00,$00,$00,$00 ; 0xa118
				dc.b  $0D,$00,$00,$00,$00,$00,$0D,$00 ; 0xa120
				dc.b  $00,$00,$00,$00,$00,$00,$0D,$51 ; 0xa128
				dc.b  $00,$3C,$1A,$85,$00,$2B,$3B,$85 ; 0xa130
				dc.b  $00,$2B,$02,$51,$00,$3C,$02,$04 ; 0xa138
				dc.b  $00,$71,$1B,$00,$24,$01,$00,$01 ; 0xa140
				dc.b  $00,$61,$02,$DF,$00,$56,$00,$00 ; 0xa148
				dc.b  $45,$F3,$00,$5C,$30,$A5,$3C,$00 ; 0xa150
				dc.b  $00,$02,$00,$E3,$00,$60,$00,$00 ; 0xa158
				dc.b  $00,$00,$00,$28,$00,$40,$0A,$84 ; 0xa160
				dc.b  $00,$00,$0B,$09,$FF,$FF,$FF,$00 ; 0xa168
				dc.b  $00,$95,$00,$1E,$00,$29,$0D,$C1 ; 0xa170
				dc.b  $00,$95,$00,$2B,$1D,$68,$00,$B2 ; 0xa178
				dc.b  $00,$C0,$0D,$8B,$00,$99,$0E,$7A ; 0xa180
				dc.b  $00,$25,$00,$E0,$0D,$6A,$00,$55 ; 0xa188
				dc.b  $00,$8B,$0D,$84,$00,$8A,$00,$F4 ; 0xa190
				dc.b  $00,$90,$8B,$00,$FF,$00,$FF,$FF ; 0xa198
				dc.b  $FF,$00,$FF,$00,$FF,$00,$FF,$FF ; 0xa1a0
				dc.b  $FF,$00,$FF,$00,$FF,$00,$FF,$FF ; 0xa1a8
				dc.b  $FF,$00,$FF,$00,$01,$10,$00,$00 ; 0xa1b0
				dc.b  $02,$43,$00,$58,$02,$0D,$00,$58 ; 0xa1b8
				dc.b  $02,$84,$00,$FF,$00,$FF,$01,$84 ; 0xa1c0
				dc.b  $00,$FF,$00,$FF,$DE,$8C,$0C,$00 ; 0xa1c8
				dc.b  $FF,$FF,$FF,$00,$00,$1A,$00,$BA ; 0xa1d0
				dc.b  $00,$14,$0D,$22,$00,$6D,$00,$FC ; 0xa1d8
				dc.b  $0D,$04,$00,$A7,$00,$C0,$0D,$12 ; 0xa1e0
				dc.b  $00,$9F,$00,$00,$0D,$18,$00,$D4 ; 0xa1e8
				dc.b  $0E,$15,$00,$B9,$00,$80,$0D,$12 ; 0xa1f0
				dc.b  $00,$E9,$00,$7C,$0D,$17,$00,$92 ; 0xa1f8
				dc.b  $00,$C9,$00,$40,$6E,$0D,$00,$A3 ; 0xa200
				dc.b  $1B,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0xa208
				dc.b  $FF,$00,$01,$10,$00,$00,$21,$00 ; 0xa210
				dc.b  $00,$60,$00,$01,$00,$80,$01,$6A ; 0xa218
				dc.b  $00,$42,$00,$80,$44,$00,$00,$13 ; 0xa220
				dc.b  $00,$BA,$0E,$01,$00,$77,$20,$FF ; 0xa228
				dc.b  $3C,$FF,$00,$FC,$00,$2A,$00,$00 ; 0xa230
				dc.b  $00,$FF,$00,$FA,$00,$B2,$00,$00 ; 0xa238
				dc.b  $0A,$8C,$0C,$09,$FF,$FF,$FF,$00 ; 0xa240
				dc.b  $00,$9B,$00,$12,$00,$0E,$0D,$C9 ; 0xa248
				dc.b  $00,$52,$00,$CA,$1D,$6C,$00,$E2 ; 0xa250
				dc.b  $00,$80,$0D,$91,$00,$2E,$0E,$7F ; 0xa258
				dc.b  $00,$08,$00,$40,$0D,$6E,$00,$96 ; 0xa260
				dc.b  $00,$0A,$0D,$89,$00,$D7,$00,$BC ; 0xa268
				dc.b  $00,$E0,$8B,$00,$FF,$00,$FF,$FF ; 0xa270
				dc.b  $FF,$00,$FF,$00,$FF,$00,$FF,$FF ; 0xa278
				dc.b  $FF,$00,$FF,$00,$FF,$00,$FF,$FF ; 0xa280
				dc.b  $FF,$00,$FF,$00,$01,$14,$00,$80 ; 0xa288
				dc.b  $01,$00,$00,$00,$00,$00,$01,$00 ; 0xa290
				dc.b  $00,$DF,$00,$F8,$E7,$8F,$00,$02 ; 0xa298
				dc.b  $0B,$00,$FF,$FF,$FF,$00,$00,$55 ; 0xa2a0
				dc.b  $00,$15,$00,$5D,$0D,$6E,$00,$50 ; 0xa2a8
				dc.b  $00,$87,$0D,$0E,$00,$EA,$00,$70 ; 0xa2b0
				dc.b  $0D,$3B,$00,$A9,$00,$C0,$0D,$4F ; 0xa2b8
				dc.b  $00,$8D,$0E,$45,$00,$9B,$00,$60 ; 0xa2c0
				dc.b  $0D,$3C,$00,$98,$00,$67,$0D,$4B ; 0xa2c8
				dc.b  $00,$87,$00,$CF,$00,$D0,$0D,$64 ; 0xa2d0
				dc.b  $00,$26,$1A,$72,$00,$4E,$3B,$72 ; 0xa2d8
				dc.b  $00,$4E,$02,$64,$00,$26,$02,$02 ; 0xa2e0
				dc.b  $00,$F6,$1B,$00,$01,$14,$00,$80 ; 0xa2e8
				dc.b  $32,$E6,$00,$9F,$00,$FF,$01,$01 ; 0xa2f0
				dc.b  $00,$FC,$06,$0A,$00,$98,$31,$FF ; 0xa2f8
				dc.b  $00,$E9,$00,$AD,$00,$20,$09,$10 ; 0xa300
				dc.b  $08,$0B,$00,$EB,$01,$10,$15,$FF ; 0xa308
				dc.b  $29,$01,$00,$00,$00,$C0,$11,$05 ; 0xa310
				dc.b  $00,$D8,$02,$09,$00,$08,$04,$03 ; 0xa318
				dc.b  $04,$65,$00,$21,$00,$8F,$00,$02 ; 0xa320
				dc.b  $07,$05,$03,$02,$75,$01,$00,$ED ; 0xa328
				dc.b  $00,$B4,$55,$02,$00,$7A,$00,$84 ; 0xa330
				dc.b  $11,$10,$00,$80,$00,$00,$1B,$FF ; 0xa338
				dc.b  $74,$FF,$00,$F0,$00,$10,$56,$02 ; 0xa340
				dc.b  $00,$10,$12,$09,$00,$08,$1C,$00 ; 0xa348
				dc.b  $00,$1C,$00,$AB,$00,$06,$0D,$24 ; 0xa350
				dc.b  $00,$F4,$00,$32,$0D,$04,$00,$FF ; 0xa358
				dc.b  $00,$20,$07,$75,$00,$40,$04,$13 ; 0xa360
				dc.b  $00,$FC,$00,$80,$0D,$1A,$00,$A6 ; 0xa368
				dc.b  $0E,$17,$00,$51,$00,$40,$0D,$14 ; 0xa370
				dc.b  $00,$4C,$00,$72,$0D,$19,$00,$4D ; 0xa378
				dc.b  $00,$3C,$00,$60,$0D,$96,$00,$A9 ; 0xa380
				dc.b  $1A,$64,$00,$48,$3B,$64,$00,$48 ; 0xa388
				dc.b  $02,$96,$00,$A9,$02,$27,$00,$5E ; 0xa390
				dc.b  $1B,$00,$01,$14,$00,$80,$3D,$FF ; 0xa398
				dc.b  $00,$80,$00,$00,$00,$01,$0A,$BF ; 0xa3a0
				dc.b  $07,$00,$00,$00,$18,$FF,$00,$F3 ; 0xa3a8
				dc.b  $00,$8A,$0E,$29,$00,$3C,$1E,$00 ; 0xa3b0
				dc.b  $12,$FF,$00,$FF,$00,$FF,$00,$FE ; 0xa3b8
				dc.b  $08,$FF,$00,$FF,$00,$FA,$00,$59 ; 0xa3c0
				dc.b  $11,$28,$00,$10,$03,$82,$03,$94 ; 0xa3c8
				dc.b  $03,$32,$03,$B8,$0B,$8F,$00,$02 ; 0xa3d0
				dc.b  $0B,$09,$80,$00,$00,$00,$52,$40 ; 0xa3d8
				dc.b  $00,$04,$00,$30,$00,$18,$27,$FF ; 0xa3e0
				dc.b  $D4,$FF,$00,$FB,$0C,$32,$03,$B8 ; 0xa3e8
				dc.b  $18,$00,$00,$D5,$00,$6D,$00,$57 ; 0xa3f0
				dc.b  $0D,$15,$00,$35,$00,$55,$1D,$95 ; 0xa3f8
				dc.b  $00,$ED,$00,$40,$0D,$C7,$00,$E7 ; 0xa400
				dc.b  $0E,$AE,$00,$EA,$00,$20,$0D,$98 ; 0xa408
				dc.b  $00,$44,$00,$F5,$0D,$BD,$00,$CC ; 0xa410
				dc.b  $00,$C3,$00,$70,$8B,$00,$FF,$00 ; 0xa418
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0xa420
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0xa428
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$F2,$8D ; 0xa430
				dc.b  $00,$00,$0B,$00,$FF,$FF,$FF,$00 ; 0xa438
				dc.b  $00,$89,$00,$F6,$00,$53,$0D,$B3 ; 0xa440
				dc.b  $00,$13,$00,$89,$0D,$18,$00,$36 ; 0xa448
				dc.b  $00,$90,$0D,$60,$00,$DA,$00,$40 ; 0xa450
				dc.b  $0D,$81,$00,$23,$0E,$70,$00,$FE ; 0xa458
				dc.b  $00,$A0,$0D,$62,$00,$5D,$00,$A9 ; 0xa460
				dc.b  $0D,$7A,$00,$9C,$00,$4B,$00,$30 ; 0xa468
				dc.b  $0D,$77,$00,$5D,$1A,$00,$00,$42 ; 0xa470
				dc.b  $3B,$00,$00,$42,$02,$77,$00,$5D ; 0xa478
				dc.b  $02,$01,$00,$12,$1B,$00,$25,$A6 ; 0xa480
				dc.b  $00,$E1,$00,$80,$01,$92,$00,$79 ; 0xa488
				dc.b  $0A,$00,$00,$00,$00,$00,$01,$00 ; 0xa490
				dc.b  $00,$00,$06,$00,$00,$00,$31,$00 ; 0xa498
				dc.b  $00,$00,$00,$04,$00,$00,$09,$7A ; 0xa4a0
				dc.b  $00,$6C,$07,$04,$00,$00,$01,$00 ; 0xa4a8
				dc.b  $15,$03,$29,$02,$00,$15,$00,$55 ; 0xa4b0
				dc.b  $11,$08,$00,$F0,$02,$08,$00,$F0 ; 0xa4b8
				dc.b  $0B,$8D,$00,$00,$07,$02,$03,$00 ; 0xa4c0
				dc.b  $25,$40,$03,$80,$00,$FD,$00,$04 ; 0xa4c8
				dc.b  $49,$00,$00,$FF,$00,$FF,$55,$00 ; 0xa4d0
				dc.b  $00,$FF,$00,$FF,$11,$00,$00,$FF ; 0xa4d8
				dc.b  $00,$FF,$1B,$FF,$25,$C0,$03,$C0 ; 0xa4e0
				dc.b  $4A,$00,$00,$00,$00,$00,$56,$00 ; 0xa4e8
				dc.b  $00,$00,$12,$00,$00,$00,$1C,$00 ; 0xa4f0
				dc.b  $00,$0B,$00,$63,$00,$92,$0D,$0E ; 0xa4f8
				dc.b  $00,$7C,$00,$16,$0D,$01,$00,$F5 ; 0xa500
				dc.b  $00,$60,$0D,$07,$00,$D5,$0E,$0A ; 0xa508
				dc.b  $00,$72,$0E,$09,$00,$23,$00,$C0 ; 0xa510
				dc.b  $0D,$07,$00,$F4,$00,$D6,$0D,$09 ; 0xa518
				dc.b  $00,$EA,$00,$DD,$00,$20,$0D,$2A ; 0xa520
				dc.b  $00,$6A,$1A,$15,$00,$18,$3B,$15 ; 0xa528
				dc.b  $00,$18,$02,$2A,$00,$6A,$02,$07 ; 0xa530
				dc.b  $00,$BF,$1B,$00,$25,$6F,$00,$75 ; 0xa538
				dc.b  $00,$00,$23,$03,$24,$00,$00,$01 ; 0xa540
				dc.b  $00,$62,$0E,$0B,$00,$F4,$1E,$01 ; 0xa548
				dc.b  $01,$03,$31,$7C,$00,$C0,$03,$88 ; 0xa550
				dc.b  $03,$98,$03,$DE,$00,$E0,$02,$BE ; 0xa558
				dc.b  $0B,$8D,$00,$00,$0B,$09,$FF,$FF ; 0xa560
				dc.b  $E2,$3A,$03,$BE,$18,$00,$00,$0A ; 0xa568
				dc.b  $00,$4E,$00,$4D,$0D,$59,$00,$F8 ; 0xa570
				dc.b  $00,$57,$1D,$BB,$00,$1D,$00,$C0 ; 0xa578
				dc.b  $0D,$F9,$00,$7D,$0E,$DA,$00,$4D ; 0xa580
				dc.b  $00,$60,$0D,$BE,$00,$0A,$00,$37 ; 0xa588
				dc.b  $0D,$EC,$00,$E1,$00,$3E,$00,$D0 ; 0xa590
				dc.b  $8B,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0xa598
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0xa5a0
				dc.b  $FF,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0xa5a8
				dc.b  $FF,$00,$01,$10,$00,$00,$01,$01 ; 0xa5b0
				dc.b  $00,$45,$00,$B0,$01,$01,$00,$0D ; 0xa5b8
				dc.b  $00,$58,$E7,$8C,$00,$0A,$0B,$00 ; 0xa5c0
				dc.b  $FF,$FF,$FF,$00,$00,$B6,$00,$1D ; 0xa5c8
				dc.b  $00,$2A,$0D,$EC,$00,$7D,$00,$5E ; 0xa5d0
				dc.b  $0D,$1F,$00,$F9,$00,$E0,$0D,$7F ; 0xa5d8
				dc.b  $00,$E7,$00,$80,$0D,$AA,$00,$8A ; 0xa5e0
				dc.b  $0E,$95,$00,$38,$00,$C0,$0D,$81 ; 0xa5e8
				dc.b  $00,$E7,$00,$1E,$0D,$A1,$00,$EB ; 0xa5f0
				dc.b  $00,$A6,$00,$A0,$0D,$0D,$00,$B4 ; 0xa5f8
				dc.b  $1A,$20,$00,$BB,$3B,$20,$00,$BB ; 0xa600
				dc.b  $02,$0D,$00,$B4,$02,$1B,$00,$24 ; 0xa608
				dc.b  $1B,$00,$01,$10,$00,$00,$22,$AA ; 0xa610
				dc.b  $00,$FD,$00,$00,$01,$FC,$00,$21 ; 0xa618
				dc.b  $56,$3D,$00,$2C,$20,$C3,$4E,$8C ; 0xa620
				dc.b  $00,$0A,$0B,$00,$2A,$FF,$00,$FF ; 0xa628
				dc.b  $D3,$FF,$FF,$00,$00,$19,$00,$65 ; 0xa630
				dc.b  $00,$E5,$0D,$20,$00,$B3,$00,$9F ; 0xa638
				dc.b  $0D,$04,$00,$6B,$00,$F0,$0D,$11 ; 0xa640
				dc.b  $00,$AF,$00,$C0,$0D,$17,$00,$95 ; 0xa648
				dc.b  $0E,$14,$00,$A2,$00,$60,$0D,$11 ; 0xa650
				dc.b  $00,$F6,$00,$7F,$0D,$16,$00,$63 ; 0xa658
				dc.b  $00,$E8,$00,$50,$0D,$00,$00,$00 ; 0xa660
				dc.b  $1A,$3C,$00,$D2,$3B,$3C,$00,$D2 ; 0xa668
				dc.b  $02,$00,$00,$00,$03,$0F,$1B,$00 ; 0xa670
				dc.b  $01,$10,$00,$00,$21,$01,$00,$1F ; 0xa678
				dc.b  $00,$FD,$49,$FF,$00,$F2,$00,$52 ; 0xa680
				dc.b  $30,$C3,$30,$FF,$00,$FB,$00,$00 ; 0xa688
				dc.b  $09,$00,$00,$04,$00,$39,$0F,$8C ; 0xa690
				dc.b  $00,$0A,$0B,$09,$FF,$FF,$FF,$00 ; 0xa698
				dc.b  $00,$36,$00,$75,$00,$24,$0D,$93 ; 0xa6a0
				dc.b  $00,$62,$00,$2C,$1D,$DA,$00,$2B ; 0xa6a8
				dc.b  $00,$00,$0D,$22,$00,$E4,$0E,$FE ; 0xa6b0
				dc.b  $00,$87,$00,$80,$0D,$DD,$00,$93 ; 0xa6b8
				dc.b  $00,$AC,$0D,$14,$00,$30,$00,$9A ; 0xa6c0
				dc.b  $00,$40,$8B,$00,$FF,$00,$FF,$FF ; 0xa6c8
				dc.b  $FF,$00,$FF,$00,$FF,$00,$FF,$FF ; 0xa6d0
				dc.b  $FF,$00,$FF,$00,$FF,$00,$FF,$FF ; 0xa6d8
				dc.b  $FF,$00,$FF,$00,$01,$10,$02,$01 ; 0xa6e0
				dc.b  $00,$4E,$00,$92,$01,$01,$00,$0E ; 0xa6e8
				dc.b  $00,$C0,$0F,$FC,$03,$78,$02,$6D ; 0xa6f0
				dc.b  $00,$77,$00,$FF,$5D,$00,$00,$65 ; 0xa6f8
				dc.b  $00,$40,$0B,$01,$60,$65,$00,$21 ; 0xa700
				dc.b  $00,$8E,$00,$01,$07,$04,$01,$01 ; 0xa708
				dc.b  $01,$00,$1A,$80,$00,$00,$02,$80 ; 0xa710
				dc.b  $00,$00,$DF,$FF,$19,$EF,$00,$28 ; 0xa718
				dc.b  $02,$B9,$00,$DF,$E0,$00,$00,$37 ; 0xa720
				dc.b  $00,$CE,$00,$A8,$0D,$95,$00,$23 ; 0xa728
				dc.b  $00,$78,$0D,$36,$00,$C7,$00,$80 ; 0xa730
				dc.b  $0D,$DB,$00,$1E,$0E,$24,$00,$28 ; 0xa738
				dc.b  $0E,$FF,$00,$A3,$0E,$DE,$00,$8A ; 0xa740
				dc.b  $00,$78,$0D,$15,$00,$64,$00,$3A ; 0xa748
				dc.b  $00,$80,$0D,$8F,$00,$64,$1A,$70 ; 0xa750
				dc.b  $00,$A9,$22,$00,$09,$00,$09,$00 ; 0xa758
				dc.b  $04,$70,$00,$A9,$02,$8F,$00,$64 ; 0xa760
				dc.b  $01,$02,$00,$04,$00,$49,$01,$1C ; 0xa768
				dc.b  $00,$04,$02,$02,$01,$0F,$13,$00 ; 0xa770
				dc.b  $01,$10,$23,$A8,$00,$82,$00,$80 ; 0xa778
				dc.b  $01,$86,$00,$58,$0A,$62,$00,$77 ; 0xa780
				dc.b  $00,$FF,$1E,$88,$00,$7F,$02,$88 ; 0xa788
				dc.b  $00,$7F,$15,$5B,$00,$32,$01,$FF ; 0xa790
				dc.b  $00,$FA,$00,$03,$00,$80,$04,$FF ; 0xa798
				dc.b  $00,$FF,$00,$20,$24,$FF,$47,$03 ; 0xa7a0
				dc.b  $04,$65,$00,$21,$00,$8E,$00,$01 ; 0xa7a8
				dc.b  $07,$0C,$01,$05,$01,$00,$25,$40 ; 0xa7b0
				dc.b  $00,$52,$00,$A0,$01,$80,$00,$AB ; 0xa7b8
				dc.b  $00,$5C,$09,$40,$00,$40,$00,$FC ; 0xa7c0
				dc.b  $39,$10,$00,$D0,$00,$7C,$01,$40 ; 0xa7c8
				dc.b  $89,$FF,$25,$C0,$03,$C0,$0B,$C4 ; 0xa7d0
				dc.b  $00,$FF,$00,$FF,$3E,$14,$88,$00 ; 0xa7d8
				dc.b  $00,$5C,$00,$24,$00,$EC,$0D,$77 ; 0xa7e0
				dc.b  $00,$7F,$00,$04,$0D,$10,$00,$28 ; 0xa7e8
				dc.b  $00,$40,$0D,$40,$00,$A1,$0E,$56 ; 0xa7f0
				dc.b  $00,$2C,$0E,$4B,$00,$66,$00,$80 ; 0xa7f8
				dc.b  $0D,$41,$00,$A3,$00,$84,$0D,$51 ; 0xa800
				dc.b  $00,$D1,$00,$26,$00,$C0,$6E,$50 ; 0xa808
				dc.b  $00,$0F,$1B,$00,$01,$10,$22,$01 ; 0xa810
				dc.b  $00,$11,$00,$25,$00,$80,$01,$91 ; 0xa818
				dc.b  $00,$ED,$00,$80,$15,$48,$00,$48 ; 0xa820
				dc.b  $2D,$FF,$00,$27,$00,$BE,$00,$01 ; 0xa828
				dc.b  $0D,$07,$00,$E8,$20,$FF,$47,$02 ; 0xa830
				dc.b  $04,$65,$00,$21,$00,$8E,$00,$01 ; 0xa838
				dc.b  $07,$03,$03,$09,$25,$40,$00,$D2 ; 0xa840
				dc.b  $00,$47,$01,$80,$00,$DF,$00,$7F ; 0xa848
				dc.b  $45,$20,$00,$80,$00,$00,$0E,$AC ; 0xa850
				dc.b  $00,$70,$7B,$FF,$24,$01,$00,$07 ; 0xa858
				dc.b  $00,$AC,$02,$9B,$00,$FA,$45,$FF ; 0xa860
				dc.b  $02,$01,$8B,$00,$00,$51,$00,$46 ; 0xa868
				dc.b  $00,$12,$03,$31,$09,$5F,$00,$42 ; 0xa870
				dc.b  $00,$D6,$0D,$03,$00,$39,$00,$B0 ; 0xa878
				dc.b  $02,$02,$00,$10,$08,$06,$00,$33 ; 0xa880
				dc.b  $00,$85,$00,$80,$0D,$44,$00,$B2 ; 0xa888
				dc.b  $0E,$3C,$00,$1B,$00,$C0,$0D,$34 ; 0xa890
				dc.b  $00,$53,$00,$96,$0C,$08,$00,$41 ; 0xa898
				dc.b  $00,$39,$00,$41,$00,$20,$0B,$07 ; 0xa8a0
				dc.b  $62,$59,$00,$2C,$1B,$00,$FF,$00 ; 0xa8a8
				dc.b  $FF,$FF,$FF,$00,$E2,$59,$00,$2C ; 0xa8b0
				dc.b  $1B,$00,$01,$10,$23,$1A,$00,$A0 ; 0xa8b8
				dc.b  $02,$5F,$00,$40,$2B,$CD,$00,$17 ; 0xa8c0
				dc.b  $89,$00,$00,$93,$00,$80,$00,$FF ; 0xa8c8
				dc.b  $00,$F0,$00,$00,$00,$01,$08,$65 ; 0xa8d0
				dc.b  $00,$21,$00,$8E,$00,$01,$07,$0B ; 0xa8d8
				dc.b  $01,$04,$01,$03,$55,$20,$00,$D2 ; 0xa8e0
				dc.b  $00,$14,$A7,$FF,$56,$9B,$00,$3F ; 0xa8e8
				dc.b  $A7,$00,$00,$2B,$00,$27,$00,$FC ; 0xa8f0
				dc.b  $0D,$37,$00,$CB,$00,$34,$0D,$07 ; 0xa8f8
				dc.b  $00,$8B,$00,$40,$0D,$1E,$00,$2D ; 0xa900
				dc.b  $0E,$28,$00,$3C,$0E,$23,$00,$34 ; 0xa908
				dc.b  $00,$80,$0D,$1E,$00,$A5,$00,$B4 ; 0xa910
				dc.b  $0D,$26,$00,$33,$00,$77,$00,$C0 ; 0xa918
				dc.b  $0D,$8F,$00,$64,$1A,$70,$00,$A9 ; 0xa920
				dc.b  $3B,$70,$00,$A9,$02,$8F,$00,$64 ; 0xa928
				dc.b  $02,$03,$00,$35,$02,$83,$00,$E8 ; 0xa930
				dc.b  $17,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0xa938
				dc.b  $E0,$FF,$00,$FF,$01,$DE,$01,$01 ; 0xa940
				dc.b  $00,$78,$00,$C0,$17,$00,$05,$48 ; 0xa948
				dc.b  $00,$38,$02,$0A,$00,$16,$16,$49 ; 0xa950
				dc.b  $00,$5B,$CF,$98,$00,$00,$0B,$00 ; 0xa958
				dc.b  $FF,$FF,$FF,$00,$00,$2E,$00,$93 ; 0xa960
				dc.b  $00,$80,$0D,$89,$00,$22,$00,$80 ; 0xa968
				dc.b  $0D,$35,$00,$28,$00,$00,$0D,$D4 ; 0xa970
				dc.b  $00,$A0,$0E,$1B,$00,$80,$0E,$F8 ; 0xa978
				dc.b  $00,$10,$0E,$D7,$00,$F2,$00,$80 ; 0xa980
				dc.b  $0D,$0D,$00,$2C,$00,$38,$00,$00 ; 0xa988
				dc.b  $6E,$00,$00,$00,$1B,$00,$25,$BD ; 0xa990
				dc.b  $00,$B4,$00,$00,$01,$C0,$00,$0C ; 0xa998
				dc.b  $0A,$15,$00,$1F,$39,$FF,$00,$F6 ; 0xa9a0
				dc.b  $00,$3A,$01,$00,$00,$02,$00,$3E ; 0xa9a8
				dc.b  $00,$00,$21,$05,$00,$4F,$00,$FF ; 0xa9b0
				dc.b  $07,$01,$49,$02,$00,$F5,$00,$FF ; 0xa9b8
				dc.b  $02,$98,$00,$00,$0B,$00,$25,$00 ; 0xa9c0
				dc.b  $03,$00,$0B,$00,$C9,$FF,$FF,$00 ; 0xa9c8
				dc.b  $00,$52,$00,$E9,$00,$C4,$0D,$6B ; 0xa9d0
				dc.b  $00,$7E,$00,$0C,$0D,$0E,$00,$88 ; 0xa9d8
				dc.b  $00,$C0,$0D,$3A,$00,$23,$0E,$4D ; 0xa9e0
				dc.b  $00,$84,$0E,$43,$00,$D3,$0E,$3B ; 0xa9e8
				dc.b  $00,$0B,$00,$8C,$0D,$49,$00,$99 ; 0xa9f0
				dc.b  $00,$24,$00,$40,$8B,$00,$01,$00 ; 0xa9f8
				dc.b  $22,$00,$00,$C0,$00,$00,$00,$00 ; 0xaa00
				dc.b  $01,$C0,$00,$00,$00,$00,$15,$00 ; 0xaa08
				dc.b  $00,$00,$2D,$00,$00,$00,$00,$00 ; 0xaa10
				dc.b  $00,$00,$0D,$14,$00,$00,$20,$03 ; 0xaa18
				dc.b  $47,$00,$04,$00,$00,$00,$00,$00 ; 0xaa20
				dc.b  $00,$00,$07,$00,$03,$00,$25,$00 ; 0xaa28
				dc.b  $00,$FF,$00,$FF,$01,$00,$00,$FF ; 0xaa30
				dc.b  $00,$FF,$45,$00,$00,$FF,$00,$FF ; 0xaa38
				dc.b  $0E,$FF,$00,$FF,$7B,$FF,$24,$00 ; 0xaa40
				dc.b  $00,$00,$00,$00,$02,$00,$00,$00 ; 0xaa48
				dc.b  $45,$00,$02,$00,$8B,$00,$00,$00 ; 0xaa50
				dc.b  $00,$40,$00,$00,$03,$11,$09,$00 ; 0xaa58
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0xaa60
				dc.b  $00,$00,$02,$00,$00,$30,$08,$00 ; 0xaa68
				dc.b  $00,$00,$00,$00,$00,$00,$0D,$00 ; 0xaa70
				dc.b  $00,$00,$0E,$00,$00,$00,$00,$00 ; 0xaa78
				dc.b  $0D,$00,$00,$00,$00,$00,$0C,$00 ; 0xaa80
				dc.b  $00,$00,$00,$00,$00,$00,$00,$00 ; 0xaa88
				dc.b  $0B,$00,$01,$8F,$00,$64,$1A,$70 ; 0xaa90
				dc.b  $00,$A9,$3B,$70,$00,$A9,$02,$8F ; 0xaa98
				dc.b  $00,$64,$02,$01,$00,$4D,$02,$83 ; 0xaaa0
				dc.b  $00,$E8,$17,$00,$FF,$00,$FF,$FF ; 0xaaa8
				dc.b  $FF,$00,$FF,$00,$56,$8A,$00,$9B ; 0xaab0
				dc.b  $8A,$EE,$00,$00,$0E,$98,$00,$00 ; 0xaab8
				dc.b  $0B,$03,$FF,$FF,$56,$91,$00,$8F ; 0xaac0
				dc.b  $A7,$00,$00,$21,$00,$EC,$00,$D4 ; 0xaac8
				dc.b  $0D,$2B,$00,$CA,$00,$3C,$0D,$05 ; 0xaad0
				dc.b  $00,$EB,$00,$C0,$0D,$17,$00,$AF ; 0xaad8
				dc.b  $0E,$1F,$00,$94,$0E,$31,$00,$85 ; 0xaae0
				dc.b  $00,$50,$02,$02,$00,$40,$09,$18 ; 0xaae8
				dc.b  $00,$0D,$00,$BC,$0D,$1D,$00,$FB ; 0xaaf0
				dc.b  $00,$75,$00,$40,$8B,$00,$FF,$00 ; 0xaaf8
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$05,$4A ; 0xab00
				dc.b  $00,$18,$02,$1A,$00,$36,$02,$E4 ; 0xab08
				dc.b  $00,$BF,$00,$FF,$01,$E9,$00,$2F ; 0xab10
				dc.b  $00,$FF,$06,$00,$03,$00,$02,$7C ; 0xab18
				dc.b  $00,$B7,$5E,$1A,$00,$70,$00,$68 ; 0xab20
				dc.b  $0B,$00,$62,$92,$00,$10,$0B,$00 ; 0xab28
				dc.b  $1A,$FF,$00,$FF,$02,$FF,$00,$FF ; 0xab30
				dc.b  $DF,$FF,$19,$00,$00,$00,$02,$00 ; 0xab38
				dc.b  $00,$00,$E0,$00,$00,$12,$00,$C3 ; 0xab40
				dc.b  $00,$1C,$0D,$18,$00,$12,$00,$94 ; 0xab48
				dc.b  $0D,$03,$00,$41,$00,$40,$0D,$0D ; 0xab50
				dc.b  $00,$05,$0E,$11,$00,$5C,$0E,$0F ; 0xab58
				dc.b  $00,$30,$00,$80,$0D,$0D,$00,$39 ; 0xab60
				dc.b  $00,$14,$0D,$10,$00,$7B,$00,$69 ; 0xab68
				dc.b  $00,$C0,$8B,$00,$25,$C0,$00,$00 ; 0xab70
				dc.b  $03,$00,$0A,$00,$00,$00,$00,$00 ; 0xab78
				dc.b  $1E,$80,$00,$00,$02,$80,$00,$00 ; 0xab80
				dc.b  $14,$00,$00,$00,$00,$00,$02,$00 ; 0xab88
				dc.b  $00,$04,$05,$00,$00,$40,$00,$00 ; 0xab90
				dc.b  $07,$1F,$00,$FF,$02,$20,$0E,$00 ; 0xab98
				dc.b  $00,$00,$00,$00,$07,$03,$14,$FF ; 0xaba0
				dc.b  $00,$FE,$00,$E4,$01,$FF,$00,$FF ; 0xaba8
				dc.b  $00,$B8,$00,$80,$18,$FF,$00,$F7 ; 0xabb0
				dc.b  $00,$9C,$00,$40,$01,$06,$00,$9D ; 0xabb8
				dc.b  $00,$80,$02,$FC,$03,$FC,$04,$00 ; 0xabc0
				dc.b  $01,$00,$00,$00,$00,$00,$02,$92 ; 0xabc8
				dc.b  $00,$10,$07,$0A,$01,$04,$01,$02 ; 0xabd0
				dc.b  $26,$FF,$00,$FF,$02,$FF,$00,$FF ; 0xabd8
				dc.b  $0A,$FF,$00,$FF,$39,$00,$00,$FF ; 0xabe0
				dc.b  $00,$FF,$01,$00,$43,$10,$03,$20 ; 0xabe8
				dc.b  $1B,$40,$03,$80,$21,$FF,$25,$00 ; 0xabf0
				dc.b  $03,$00,$0B,$00,$00,$00,$00,$00 ; 0xabf8
				dc.b  $3E,$00,$41,$FF,$00,$FF,$00,$D4 ; 0xac00
				dc.b  $01,$FF,$00,$FF,$00,$CA,$00,$80 ; 0xac08
				dc.b  $18,$FF,$00,$FF,$00,$68,$01,$FF ; 0xac10
				dc.b  $00,$FF,$00,$DE,$20,$00,$00,$1A ; 0xac18
				dc.b  $00,$75,$00,$D4,$0D,$22,$00,$15 ; 0xac20
				dc.b  $00,$3C,$0D,$04,$00,$9B,$0E,$12 ; 0xac28
				dc.b  $00,$6F,$0E,$18,$00,$94,$0E,$15 ; 0xac30
				dc.b  $00,$81,$0E,$12,$00,$B8,$00,$BC ; 0xac38
				dc.b  $0D,$17,$00,$56,$00,$05,$0E,$8F ; 0xac40
				dc.b  $00,$64,$1A,$70,$00,$A9,$3B,$70 ; 0xac48
				dc.b  $00,$A9,$02,$8F,$00,$64,$02,$00 ; 0xac50
				dc.b  $00,$00,$02,$83,$00,$E8,$17,$00 ; 0xac58
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0xac60
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0xac68
				dc.b  $56,$AC,$00,$A9,$9A,$92,$00,$10 ; 0xac70
				dc.b  $0B,$03,$FF,$FF,$FF,$00,$00,$2D ; 0xac78
				dc.b  $00,$B8,$00,$E4,$0D,$3B,$00,$21 ; 0xac80
				dc.b  $00,$6C,$0D,$07,$00,$FE,$0E,$1F ; 0xac88
				dc.b  $00,$FB,$0E,$2A,$00,$A4,$0E,$4A ; 0xac90
				dc.b  $00,$69,$0E,$20,$00,$7A,$00,$EC ; 0xac98
				dc.b  $0D,$28,$00,$7C,$00,$56,$8C,$00 ; 0xaca0
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0xaca8
				dc.b  $01,$14,$00,$80,$01,$00,$00,$AE ; 0xacb0
				dc.b  $02,$00,$00,$92,$00,$04,$02,$C4 ; 0xacb8
				dc.b  $00,$7F,$02,$FA,$00,$FF,$06,$5A ; 0xacc0
				dc.b  $00,$5A,$02,$5C,$00,$9A,$02,$78 ; 0xacc8
				dc.b  $00,$00,$00,$00,$09,$5B,$00,$5C ; 0xacd0
				dc.b  $02,$5C,$00,$52,$66,$01,$58,$96 ; 0xacd8
				dc.b  $00,$02,$0B,$00,$FF,$FF,$FF,$00 ; 0xace0
				dc.b  $00,$26,$00,$BF,$00,$BA,$0D,$32 ; 0xace8
				dc.b  $00,$10,$00,$0E,$0D,$06,$00,$C4 ; 0xacf0
				dc.b  $00,$E0,$0D,$1B,$00,$13,$00,$80 ; 0xacf8
				dc.b  $0D,$24,$00,$1A,$0E,$1F,$00,$96 ; 0xad00
				dc.b  $00,$C0,$0D,$1B,$00,$7F,$00,$CE ; 0xad08
				dc.b  $0D,$22,$00,$46,$00,$EF,$00,$A0 ; 0xad10
				dc.b  $8B,$00,$01,$14,$00,$80,$22,$59 ; 0xad18
				dc.b  $00,$88,$02,$5F,$00,$E8,$5B,$16 ; 0xad20
				dc.b  $00,$B2,$02,$7A,$00,$47,$2E,$18 ; 0xad28
				dc.b  $01,$00,$00,$00,$00,$18,$00,$70 ; 0xad30
				dc.b  $1A,$A8,$00,$00,$01,$07,$00,$BD ; 0xad38
				dc.b  $00,$C0,$12,$96,$00,$02,$0B,$02 ; 0xad40
				dc.b  $FF,$FF,$FF,$00,$00,$36,$00,$74 ; 0xad48
				dc.b  $00,$14,$0D,$46,$00,$7B,$00,$FC ; 0xad50
				dc.b  $0D,$09,$00,$87,$0E,$26,$00,$1F ; 0xad58
				dc.b  $0E,$32,$00,$D4,$0E,$2C,$00,$79 ; 0xad60
				dc.b  $0E,$26,$00,$B7,$00,$7C,$0D,$30 ; 0xad68
				dc.b  $00,$42,$00,$69,$8C,$00,$FF,$00 ; 0xad70
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0xad78
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$01,$14 ; 0xad80
				dc.b  $00,$80,$53,$99,$00,$C9,$9A,$96 ; 0xad88
				dc.b  $00,$02,$0B,$03,$FF,$FF,$FF,$00 ; 0xad90
				dc.b  $00,$49,$00,$B7,$00,$24,$0D,$5F ; 0xad98
				dc.b  $00,$88,$00,$2C,$0D,$0C,$00,$EA ; 0xada0
				dc.b  $0E,$33,$00,$AB,$0E,$44,$00,$E4 ; 0xada8
				dc.b  $0E,$85,$00,$79,$0E,$34,$00,$79 ; 0xadb0
				dc.b  $00,$AC,$0D,$41,$00,$68,$00,$BA ; 0xadb8
				dc.b  $8C,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0xadc0
				dc.b  $FF,$00,$01,$10,$00,$00,$06,$8E ; 0xadc8
				dc.b  $00,$5C,$02,$E9,$00,$AF,$02,$D9 ; 0xadd0
				dc.b  $08,$48,$02,$5D,$00,$5A,$02,$79 ; 0xadd8
				dc.b  $00,$DF,$00,$FF,$0A,$50,$02,$5D ; 0xade0
				dc.b  $00,$84,$BF,$99,$00,$00,$0B,$00 ; 0xade8
				dc.b  $FF,$FF,$FF,$00,$00,$34,$00,$CC ; 0xadf0
				dc.b  $00,$B7,$0D,$44,$00,$55,$00,$75 ; 0xadf8
				dc.b  $0D,$09,$00,$3D,$00,$50,$0D,$24 ; 0xae00
				dc.b  $00,$F5,$00,$40,$0D,$31,$00,$47 ; 0xae08
				dc.b  $0E,$2B,$00,$1E,$00,$20,$0D,$25 ; 0xae10
				dc.b  $00,$89,$00,$15,$0D,$2E,$00,$C9 ; 0xae18
				dc.b  $00,$79,$00,$70,$8B,$00,$01,$10 ; 0xae20
				dc.b  $00,$00,$83,$B2,$00,$19,$02,$0D ; 0xae28
				dc.b  $00,$52,$2D,$FF,$00,$DF,$00,$F0 ; 0xae30
				dc.b  $00,$FF,$00,$FF,$00,$D4,$00,$F0 ; 0xae38
				dc.b  $1A,$88,$01,$FF,$00,$F8,$00,$0D ; 0xae40
				dc.b  $00,$20,$12,$99,$00,$00,$0B,$02 ; 0xae48
				dc.b  $FF,$FF,$FF,$00,$00,$44,$00,$81 ; 0xae50
				dc.b  $00,$11,$0D,$58,$00,$C1,$00,$63 ; 0xae58
				dc.b  $0D,$0C,$00,$00,$00,$30,$0D,$30 ; 0xae60
				dc.b  $00,$00,$00,$C0,$0D,$40,$00,$01 ; 0xae68
				dc.b  $0E,$38,$00,$00,$00,$E0,$0D,$30 ; 0xae70
				dc.b  $00,$C0,$00,$C3,$0D,$3C,$00,$C4 ; 0xae78
				dc.b  $00,$F3,$00,$10,$8B,$00,$FF,$00 ; 0xae80
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$FF,$00 ; 0xae88
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$01,$10 ; 0xae90
				dc.b  $00,$00,$53,$B4,$00,$E4,$9A,$99 ; 0xae98
				dc.b  $00,$00,$0B,$03,$FF,$FF,$FF,$00 ; 0xaea0
				dc.b  $00,$57,$00,$C4,$00,$21,$0D,$71 ; 0xaea8
				dc.b  $00,$CD,$00,$93,$0D,$0F,$00,$63 ; 0xaeb0
				dc.b  $00,$30,$0D,$3D,$00,$8C,$00,$C0 ; 0xaeb8
				dc.b  $0D,$52,$00,$11,$0E,$A3,$00,$1E ; 0xaec0
				dc.b  $00,$90,$0D,$3E,$00,$82,$00,$F3 ; 0xaec8
				dc.b  $0D,$4D,$00,$EB,$00,$44,$00,$10 ; 0xaed0
				dc.b  $8B,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0xaed8
				dc.b  $FF,$00,$01,$14,$00,$80,$06,$86 ; 0xaee0
				dc.b  $00,$4C,$02,$F6,$00,$3F,$02,$F1 ; 0xaee8
				dc.b  $00,$7F,$07,$F0,$02,$5C,$00,$E2 ; 0xaef0
				dc.b  $D3,$85,$0C,$00,$FF,$FF,$FF,$00 ; 0xaef8
				dc.b  $00,$6E,$00,$26,$00,$FF,$0D,$8E ; 0xaf00
				dc.b  $00,$E9,$00,$CD,$0D,$13,$00,$52 ; 0xaf08
				dc.b  $00,$D0,$0D,$4D,$00,$4B,$0E,$67 ; 0xaf10
				dc.b  $00,$0F,$0E,$5A,$00,$2D,$0E,$4E ; 0xaf18
				dc.b  $00,$80,$00,$6D,$0D,$61,$00,$D9 ; 0xaf20
				dc.b  $00,$AD,$00,$F0,$8B,$00,$01,$00 ; 0xaf28
				dc.b  $23,$C0,$00,$00,$02,$C0,$00,$00 ; 0xaf30
				dc.b  $5B,$04,$00,$00,$02,$04,$00,$00 ; 0xaf38
				dc.b  $2C,$00,$00,$00,$00,$10,$00,$00 ; 0xaf40
				dc.b  $00,$00,$00,$02,$00,$00,$00,$00 ; 0xaf48
				dc.b  $18,$00,$00,$01,$00,$00,$01,$00 ; 0xaf50
				dc.b  $00,$01,$00,$00,$00,$00,$02,$F0 ; 0xaf58
				dc.b  $03,$F0,$09,$00,$00,$00,$00,$00 ; 0xaf60
				dc.b  $08,$00,$01,$00,$01,$00,$B9,$00 ; 0xaf68
				dc.b  $03,$00,$1B,$00,$03,$00,$21,$FF ; 0xaf70
				dc.b  $B8,$00,$00,$00,$00,$00,$01,$00 ; 0xaf78
				dc.b  $00,$00,$00,$00,$00,$00,$18,$00 ; 0xaf80
				dc.b  $00,$00,$00,$00,$01,$00,$00,$00 ; 0xaf88
				dc.b  $00,$00,$20,$00,$00,$00,$00,$40 ; 0xaf90
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0xaf98
				dc.b  $0D,$00,$01,$00,$0D,$00,$01,$00 ; 0xafa0
				dc.b  $0D,$00,$00,$00,$0E,$00,$01,$00 ; 0xafa8
				dc.b  $0D,$00,$00,$00,$00,$00,$0D,$00 ; 0xafb0
				dc.b  $00,$00,$00,$00,$00,$00,$8B,$00 ; 0xafb8
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0xafc0
				dc.b  $FF,$00,$FF,$FF,$FF,$00,$FF,$00 ; 0xafc8
				dc.b  $01,$14,$00,$80,$22,$51,$00,$60 ; 0xafd0
				dc.b  $02,$64,$00,$F2,$2B,$80,$00,$00 ; 0xafd8
				dc.b  $62,$85,$00,$B0,$01,$00,$00,$20 ; 0xafe0
				dc.b  $00,$B8,$19,$00,$00,$90,$01,$FF ; 0xafe8
				dc.b  $00,$FC,$00,$F9,$00,$60,$01,$10 ; 0xaff0
				dc.b  $00,$7C,$00,$C0,$00,$00,$00,$01 ; 0xaff8
				dc.b  $00,$7C,$00,$20,$0A,$85,$08,$06 ; 0xb000
				dc.b  $03,$00,$55,$00,$00,$FF,$00,$FF ; 0xb008
				dc.b  $61,$10,$00,$DC,$00,$83,$01,$20 ; 0xb010
				dc.b  $00,$80,$00,$00,$19,$40,$00,$80 ; 0xb018
				dc.b  $00,$00,$01,$80,$00,$8A,$00,$D8 ; 0xb020
				dc.b  $01,$01,$03,$02,$19,$FF,$56,$00 ; 0xb028
				dc.b  $00,$00,$60,$FF,$00,$FF,$00,$5A ; 0xb030
				dc.b  $01,$FF,$00,$FF,$00,$DE,$19,$FF ; 0xb038
				dc.b  $00,$FC,$00,$F2,$01,$FF,$00,$FF ; 0xb040
				dc.b  $00,$10,$02,$09,$00,$14,$02,$09 ; 0xb048
				dc.b  $00,$14,$18,$00,$00,$3B,$00,$4E ; 0xb050
				dc.b  $00,$71,$0D,$4C,$00,$CB,$00,$83 ; 0xb058
				dc.b  $0D,$0A,$00,$62,$0E,$29,$00,$88 ; 0xb060
				dc.b  $0E,$37,$00,$61,$0E,$30,$00,$74 ; 0xb068
				dc.b  $00,$E0,$02,$00,$00,$E0,$09,$2A ; 0xb070
				dc.b  $00,$2E,$00,$E3,$0D,$34,$00,$94 ; 0xb078
				dc.b  $00,$89,$6F,$00,$00,$00,$1B,$00 ; 0xb080
				dc.b  $01,$14,$00,$80,$22,$1A,$00,$16 ; 0xb088
				dc.b  $02,$61,$00,$B0,$2B,$A3,$00,$D1 ; 0xb090
				dc.b  $89,$00,$00,$8E,$01,$FF,$00,$F0 ; 0xb098
				dc.b  $00,$00,$00,$01,$08,$65,$00,$21 ; 0xb0a0
				dc.b  $00,$85,$08,$0B,$01,$04,$01,$03 ; 0xb0a8
				dc.b  $55,$80,$A9,$FF,$56,$91,$00,$8F ; 0xb0b0
				dc.b  $A7,$00,$00,$32,$00,$D8,$00,$92 ; 0xb0b8
				dc.b  $0D,$41,$00,$CB,$00,$16,$0D,$08 ; 0xb0c0
				dc.b  $00,$E5,$00,$60,$0D,$23,$00,$95 ; 0xb0c8
				dc.b  $00,$80,$0D,$2F,$00,$72,$0E,$29 ; 0xb0d0
				dc.b  $00,$83,$00,$C0,$0D,$24,$00,$23 ; 0xb0d8
				dc.b  $00,$D6,$0D,$2D,$00,$0C,$00,$2D ; 0xb0e0
				dc.b  $00,$20,$0D,$8F,$00,$64,$1A,$70 ; 0xb0e8
				dc.b  $00,$A9,$3B,$70,$00,$A9,$02,$8F ; 0xb0f0
				dc.b  $00,$64,$00,$00,$00,$02,$01,$00 ; 0xb0f8
				dc.b  $01,$1B,$00,$83,$00,$E8,$17,$00 ; 0xb100
				dc.b  $01,$10,$00,$00,$01,$01,$00,$47 ; 0xb108
				dc.b  $00,$5A,$01,$01,$00,$0B,$00,$6C ; 0xb110
				dc.b  $02,$85,$00,$00,$00,$00,$01,$85 ; 0xb118
				dc.b  $00,$00,$00,$00,$05,$C0,$00,$00 ; 0xb120
				dc.b  $02,$C0,$00,$00,$02,$6A,$00,$7B ; 0xb128
				dc.b  $0A,$C0,$00,$00,$02,$C0,$00,$00 ; 0xb130
				dc.b  $5C,$01,$09,$00,$58,$90,$00,$02 ; 0xb138
				dc.b  $0B,$00,$FF,$FF,$FF,$00,$00,$1D ; 0xb140
				dc.b  $00,$54,$00,$95,$0D,$25,$00,$D0 ; 0xb148
				dc.b  $00,$AF,$0D,$05,$00,$1C,$00,$F0 ; 0xb150
				dc.b  $0D,$14,$00,$73,$00,$C0,$0D,$1B ; 0xb158
				dc.b  $00,$45,$0E,$17,$00,$DC,$00,$60 ; 0xb160
				dc.b  $0D,$14,$00,$C5,$00,$8F,$0D,$19 ; 0xb168
				dc.b  $00,$E4,$00,$33,$00,$50,$4D,$04 ; 0xb170
				dc.b  $09,$04,$09,$04,$0B,$00,$03,$00 ; 0xb178
				dc.b  $00,$00,$02,$00,$01,$00,$13,$00 ; 0xb180
				dc.b  $01,$10,$32,$FE,$00,$3C,$15,$01 ; 0xb188
				dc.b  $01,$30,$00,$BF,$00,$FF,$28,$FF ; 0xb190
				dc.b  $00,$8F,$07,$F1,$00,$B4,$20,$01 ; 0xb198
				dc.b  $47,$03,$04,$65,$00,$21,$00,$90 ; 0xb1a0
				dc.b  $00,$02,$07,$02,$03,$00,$35,$08 ; 0xb1a8
				dc.b  $43,$08,$85,$FF,$FF,$00,$00,$19 ; 0xb1b0
				dc.b  $00,$ED,$00,$54,$0D,$21,$00,$63 ; 0xb1b8
				dc.b  $00,$BC,$0D,$04,$00,$83,$00,$C0 ; 0xb1c0
				dc.b  $0D,$12,$00,$0F,$08,$78,$00,$4C ; 0xb1c8
				dc.b  $03,$03,$00,$18,$00,$14,$0E,$15 ; 0xb1d0
				dc.b  $00,$11,$00,$80,$0D,$12,$00,$57 ; 0xb1d8
				dc.b  $00,$3C,$0D,$16,$00,$DC,$00,$7D ; 0xb1e0
				dc.b  $00,$40,$8B,$00,$FF,$00,$FF,$FF ; 0xb1e8
				dc.b  $FF,$00,$FF,$00,$FF,$00,$FF,$FF ; 0xb1f0
				dc.b  $FF,$00,$81,$8F,$00,$64,$1A,$70 ; 0xb1f8
				dc.b  $00,$A9,$3B,$70,$00,$A9,$02,$8F ; 0xb200
				dc.b  $00,$64,$02,$00,$00,$00,$02,$83 ; 0xb208
				dc.b  $00,$E8,$17,$00,$01,$10,$00,$00 ; 0xb210
				dc.b  $22,$B1,$00,$42,$02,$C3,$00,$AE ; 0xb218
				dc.b  $8D,$FF,$00,$FE,$00,$A3,$00,$60 ; 0xb220
				dc.b  $02,$C6,$00,$F8,$18,$FF,$00,$FA ; 0xb228
				dc.b  $00,$A3,$01,$00,$00,$00,$00,$26 ; 0xb230
				dc.b  $00,$E0,$02,$C3,$03,$E3,$00,$60 ; 0xb238
				dc.b  $0A,$90,$00,$02,$0B,$00,$FF,$FF ; 0xb240
				dc.b  $FF,$00,$00,$52,$00,$49,$00,$CE ; 0xb248
				dc.b  $0D,$6A,$00,$AE,$00,$0A,$0D,$0E ; 0xb250
				dc.b  $00,$6C,$00,$A0,$0D,$39,$00,$B2 ; 0xb258
				dc.b  $00,$80,$0D,$4C,$00,$EE,$0E,$43 ; 0xb260
				dc.b  $00,$50,$00,$40,$0D,$3A,$00,$99 ; 0xb268
				dc.b  $00,$4A,$0D,$49,$00,$0A,$00,$B8 ; 0xb270
				dc.b  $00,$E0,$8B,$00,$01,$10,$00,$00 ; 0xb278
				dc.b  $22,$24,$00,$48,$01,$01,$00,$43 ; 0xb280
				dc.b  $00,$AC,$2B,$8E,$00,$05,$8A,$87 ; 0xb288
				dc.b  $00,$80,$01,$F5,$00,$B2,$00,$00 ; 0xb290
				dc.b  $0A,$90,$00,$02,$0B,$03,$FF,$FF ; 0xb298
				dc.b  $FF,$00,$00,$49,$00,$D3,$00,$EF ; 0xb2a0
				dc.b  $0D,$5F,$00,$AD,$00,$9D,$0D,$0C ; 0xb2a8
				dc.b  $00,$EF,$00,$D0,$0D,$33,$00,$BF ; 0xb2b0
				dc.b  $00,$40,$0D,$44,$00,$FF,$0E,$3C ; 0xb2b8
				dc.b  $00,$5F,$00,$20,$0D,$34,$00,$8E ; 0xb2c0
				dc.b  $00,$3D,$0D,$41,$00,$82,$00,$5C ; 0xb2c8
				dc.b  $00,$F0,$8B,$00,$05,$4B,$00,$A4 ; 0xb2d0
				dc.b  $02,$10,$00,$FA,$E7,$92,$00,$04 ; 0xb2d8
				dc.b  $0B,$00,$FF,$FF,$FF,$00,$00,$40 ; 0xb2e0
				dc.b  $00,$1E,$00,$24,$0D,$53,$00,$0D ; 0xb2e8
				dc.b  $00,$2C,$0D,$0B,$00,$3A,$00,$C0 ; 0xb2f0
				dc.b  $0D,$2C,$00,$EB,$00,$00,$0D,$3B ; 0xb2f8
				dc.b  $00,$E4,$0E,$34,$00,$67,$00,$80 ; 0xb300
				dc.b  $0D,$2D,$00,$9E,$00,$AC,$0D,$38 ; 0xb308
				dc.b  $00,$DD,$00,$2A,$00,$40,$4D,$00 ; 0xb310
				dc.b  $09,$00,$09,$00,$0B,$02,$03,$1C ; 0xb318
				dc.b  $00,$04,$02,$02,$01,$0F,$13,$00 ; 0xb320
				dc.b  $25,$BE,$00,$6E,$02,$BC,$00,$EE ; 0xb328
				dc.b  $09,$01,$00,$0F,$00,$3F,$00,$FF ; 0xb330
				dc.b  $01,$04,$00,$58,$06,$06,$00,$F0 ; 0xb338
				dc.b  $08,$08,$01,$08,$00,$00,$00,$00 ; 0xb340
				dc.b  $24,$FF,$00,$EA,$00,$00,$01,$00 ; 0xb348
				dc.b  $00,$00,$07,$15,$00,$40,$07,$17 ; 0xb350
				dc.b  $00,$7A,$01,$12,$00,$FC,$14,$FF ; 0xb358
				dc.b  $28,$FF,$00,$FC,$00,$14,$00,$00 ; 0xb360
				dc.b  $08,$FF,$00,$FF,$00,$FF,$01,$FF ; 0xb368
				dc.b  $00,$FE,$00,$93,$00,$80,$01,$00 ; 0xb370
				dc.b  $00,$13,$02,$0F,$00,$FF,$00,$E0 ; 0xb378
				dc.b  $0A,$92,$00,$04,$07,$05,$03,$02 ; 0xb380
				dc.b  $35,$00,$3F,$10,$03,$00,$53,$20 ; 0xb388
				dc.b  $31,$FF,$74,$FF,$00,$F0,$00,$10 ; 0xb390
				dc.b  $56,$02,$00,$20,$30,$00,$00,$2A ; 0xb398
				dc.b  $00,$4E,$00,$70,$0D,$36,$00,$B0 ; 0xb3a0
				dc.b  $00,$50,$0D,$07,$00,$65,$00,$00 ; 0xb3a8
				dc.b  $0D,$1D,$00,$94,$08,$75,$00,$40 ; 0xb3b0
				dc.b  $03,$00,$00,$27,$00,$70,$08,$76 ; 0xb3b8
				dc.b  $00,$44,$03,$01,$00,$41,$00,$8F ; 0xb3c0
				dc.b  $00,$20,$02,$02,$00,$B0,$03,$76 ; 0xb3c8
				dc.b  $00,$44,$03,$01,$00,$1E,$00,$0A ; 0xb3d0
				dc.b  $00,$50,$0D,$25,$00,$71,$00,$C7 ; 0xb3d8
				dc.b  $00,$00,$8B,$00,$FF,$00,$FF,$FF ; 0xb3e0
				dc.b  $FF,$00,$FF,$00,$FF,$00,$FF,$FF ; 0xb3e8
				dc.b  $FF,$00,$FF,$00,$01,$00,$23,$C0 ; 0xb3f0
				dc.b  $00,$00,$02,$C0,$00,$00,$8D,$00 ; 0xb3f8
				dc.b  $00,$00,$00,$10,$00,$00,$01,$02 ; 0xb400
				dc.b  $00,$00,$00,$00,$18,$00,$00,$01 ; 0xb408
				dc.b  $00,$00,$02,$01,$00,$00,$00,$00 ; 0xb410
				dc.b  $01,$08,$00,$F0,$00,$00,$01,$08 ; 0xb418
				dc.b  $00,$F0,$00,$00,$08,$00,$00,$00 ; 0xb420
				dc.b  $00,$00,$00,$00,$07,$00,$01,$00 ; 0xb428
				dc.b  $01,$00,$B9,$00,$00,$FF,$00,$FF ; 0xb430
				dc.b  $01,$00,$00,$FF,$00,$FF,$19,$00 ; 0xb438
				dc.b  $00,$FF,$00,$FF,$01,$00,$00,$FF ; 0xb440
				dc.b  $00,$FF,$01,$00,$03,$00,$19,$FF ; 0xb448
				dc.b  $B8,$00,$00,$00,$00,$00,$01,$00 ; 0xb450
				dc.b  $00,$00,$00,$00,$19,$00,$00,$00 ; 0xb458
				dc.b  $00,$00,$01,$00,$00,$00,$00,$00 ; 0xb460
				dc.b  $02,$00,$00,$00,$02,$00,$00,$00 ; 0xb468
				dc.b  $18,$00,$00,$00,$00,$40,$00,$00 ; 0xb470
				dc.b  $0D,$00,$00,$00,$00,$00,$0D,$00 ; 0xb478
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0xb480
				dc.b  $00,$00,$0D,$00,$00,$00,$0E,$00 ; 0xb488
				dc.b  $00,$00,$00,$00,$0D,$00,$00,$00 ; 0xb490
				dc.b  $00,$00,$0D,$00,$00,$00,$00,$00 ; 0xb498
				dc.b  $00,$00,$8B,$00,$25,$1A,$00,$94 ; 0xb4a0
				dc.b  $01,$00,$00,$5D,$00,$90,$2B,$98 ; 0xb4a8
				dc.b  $00,$91,$8A,$8E,$00,$00,$01,$F0 ; 0xb4b0
				dc.b  $00,$00,$00,$01,$0A,$92,$00,$04 ; 0xb4b8
				dc.b  $0B,$03,$FF,$FF,$FF,$00,$00,$6C ; 0xb4c0
				dc.b  $00,$9D,$00,$7E,$0D,$8C,$00,$EA ; 0xb4c8
				dc.b  $00,$1A,$0D,$13,$00,$0D,$00,$A0 ; 0xb4d0
				dc.b  $0D,$4C,$00,$36,$00,$80,$0D,$65 ; 0xb4d8
				dc.b  $00,$9E,$0E,$58,$00,$EA,$00,$40 ; 0xb4e0
				dc.b  $0D,$4D,$00,$67,$00,$5A,$0D,$60 ; 0xb4e8
				dc.b  $00,$7B,$00,$53,$00,$E0,$8B,$00 ; 0xb4f0
				dc.b  $01,$14,$00,$80,$02,$3F,$00,$62 ; 0xb4f8
				dc.b  $02,$13,$00,$BE,$E7,$9C,$0C,$00 ; 0xb500
				dc.b  $FF,$FF,$FF,$00,$00,$33,$00,$D3 ; 0xb508
				dc.b  $00,$2D,$0D,$43,$00,$10,$00,$F7 ; 0xb510
				dc.b  $0D,$09,$00,$11,$00,$70,$0D,$24 ; 0xb518
				dc.b  $00,$45,$00,$C0,$0D,$30,$00,$5D ; 0xb520
				dc.b  $0E,$2A,$00,$51,$00,$60,$0D,$24 ; 0xb528
				dc.b  $00,$D6,$00,$D7,$0D,$2D,$00,$EB ; 0xb530
				dc.b  $00,$4C,$00,$D0,$8B,$00,$01,$14 ; 0xb538
				dc.b  $00,$80,$22,$C0,$00,$00,$02,$C0 ; 0xb540
				dc.b  $00,$00,$0A,$83,$00,$F0,$00,$00 ; 0xb548
				dc.b  $01,$00,$00,$00,$06,$00,$00,$00 ; 0xb550
				dc.b  $08,$01,$01,$30,$00,$BF,$00,$FF ; 0xb558
				dc.b  $24,$00,$00,$00,$00,$04,$02,$60 ; 0xb560
				dc.b  $00,$FC,$06,$F1,$00,$B4,$07,$04 ; 0xb568
				dc.b  $00,$00,$01,$00,$00,$00,$14,$01 ; 0xb570
				dc.b  $28,$00,$00,$02,$00,$15,$00,$55 ; 0xb578
				dc.b  $08,$00,$00,$01,$00,$00,$01,$00 ; 0xb580
				dc.b  $00,$01,$00,$00,$00,$00,$01,$08 ; 0xb588
				dc.b  $00,$F0,$02,$08,$00,$F0,$00,$00 ; 0xb590
				dc.b  $0A,$9C,$08,$02,$03,$00,$35,$08 ; 0xb598
				dc.b  $3F,$00,$03,$08,$53,$00,$31,$FF ; 0xb5a0
				dc.b  $74,$00,$00,$00,$00,$00,$56,$00 ; 0xb5a8
				dc.b  $00,$00,$30,$00,$00,$30,$00,$6B ; 0xb5b0
				dc.b  $00,$EC,$0D,$3E,$00,$A4,$00,$04 ; 0xb5b8
				dc.b  $0D,$08,$00,$78,$00,$40,$0D,$21 ; 0xb5c0
				dc.b  $00,$E1,$08,$78,$00,$4C,$03,$03 ; 0xb5c8
				dc.b  $00,$2D,$00,$2C,$08,$75,$00,$40 ; 0xb5d0
				dc.b  $03,$00,$00,$27,$00,$86,$00,$80 ; 0xb5d8
				dc.b  $02,$00,$00,$E0,$03,$75,$00,$40 ; 0xb5e0
				dc.b  $03,$00,$00,$22,$00,$68,$00,$84 ; 0xb5e8
				dc.b  $0D,$2A,$00,$E3,$00,$96,$00,$C0 ; 0xb5f0
				dc.b  $8B,$00,$FF,$00,$FF,$FF,$FF,$00 ; 0xb5f8
				dc.b  $FF,$00,$01,$14,$00,$80,$22,$C1 ; 0xb600
				dc.b  $00,$FE,$02,$BB,$00,$B6,$16,$7F ; 0xb608
				dc.b  $00,$FF,$13,$00,$03,$00,$16,$51 ; 0xb610
				dc.b  $00,$4C,$0E,$08,$00,$D0,$20,$FF ; 0xb618
				dc.b  $36,$08,$02,$04,$00,$91,$0C,$02 ; 0xb620
				dc.b  $04,$65,$00,$21,$00,$9C,$00,$04 ; 0xb628
				dc.b  $07,$03,$03,$09,$71,$20,$8D,$FF ; 0xb630
				dc.b  $FF,$00,$00,$1B,$00,$B3,$00,$9E ; 0xb638
				dc.b  $0D,$23,$00,$B2,$00,$7A,$0D,$04 ; 0xb640
				dc.b  $00,$D3,$00,$A0,$0D,$13,$00,$4E ; 0xb648
				dc.b  $00,$80,$0D,$19,$00,$BE,$0E,$20 ; 0xb650
				dc.b  $00,$D1,$00,$50,$02,$01,$00,$00 ; 0xb658
				dc.b  $03,$78,$00,$4C,$03,$03,$00,$13 ; 0xb660
				dc.b  $00,$9B,$00,$BA,$0D,$18,$00,$70 ; 0xb668
				dc.b  $00,$F5,$00,$E0,$8B,$00,$FF,$00 ; 0xb670
				dc.b  $FF,$FF,$FF,$00,$FF,$00,$01,$14 ; 0xb678
				dc.b  $00,$80,$22,$15,$00,$B4,$02,$5F ; 0xb680
				dc.b  $00,$DC,$2B,$8C,$00,$8E,$9A,$9C ; 0xb688
				dc.b  $0C,$03,$FF,$FF,$56,$9B,$00,$3F ; 0xb690
				dc.b  $A7,$00,$00,$60,$00,$52,$00,$87 ; 0xb698
				dc.b  $0D,$7C,$00,$ED,$00,$E5,$0D,$10 ; 0xb6a0
				dc.b  $00,$E4,$00,$50,$0D,$43,$00,$91 ; 0xb6a8
				dc.b  $00,$40,$0D,$5A,$00,$17,$0E,$4E ; 0xb6b0
				dc.b  $00,$D4,$00,$20,$0D,$44,$00,$9F ; 0xb6b8
				dc.b  $00,$85,$0D,$55,$00,$89,$00,$76 ; 0xb6c0
				dc.b  $00,$70,$8B,$00,$00,$00,$00,$80 ; 0xb6c8
				dc.b  $00,$40,$C0,$02,$28,$24,$0E,$0D ; 0xb6d0
				dc.b  $07,$83,$C2,$40,$00,$28,$64,$28 ; 0xb6d8
				dc.b  $20,$03,$19,$8C,$60,$A2,$08,$54 ; 0xb6e0
				dc.b  $1C,$28,$33,$89,$8D,$08,$84,$08 ; 0xb6e8
				dc.b  $B8,$00,$89,$21,$90,$90,$48,$84 ; 0xb6f0
				dc.b  $18,$F4,$5C,$23,$29,$95,$04,$48 ; 0xb6f8
				dc.b  $81,$08,$F8,$06,$4B,$2C,$99,$47 ; 0xb700
				dc.b  $C0,$00,$10,$88,$50,$83,$29,$97 ; 0xb708
				dc.b  $4D,$20,$00,$00,$00,$00,$00,$52 ; 0xb710
				dc.b  $00,$16,$00,$08,$00,$02,$FF,$FF ; 0xb718
				dc.b  $00,$00,$00,$80,$00,$40,$C0,$00 ; 0xb720
				dc.b  $21,$04,$10,$00,$22,$84,$42,$E0 ; 0xb728
				dc.b  $00,$00,$00,$00,$00,$00,$00,$00 ; 0xb730


                END
; vim:ft=asm68k ts=2
