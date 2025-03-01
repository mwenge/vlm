; ******************************************************************************
; This is Jeff Minter's Virtual Light Machine for the Atari Jaguar.
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
; vlm.s
;
; This is the reverse-engineered Motorola 68000 assembly code for the 'Virtual Light Machine'
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
;
; 
; VLM: Mode of Operation
; ------------------------------------------------------------------------------
; Two routines work in lockstep to generate, update, and draw pixels to the
; screen while VLM is in progress. These are:
; 
;  - 'Frame': This routine is a 'vertical sync interrupt handler'. This means
;    that the Atari Jaguar's CPU runs it every time the Object Processor has
;    finished painting the screen and is about to start painting it again. This
;    creates a brief interlude in which state can be updated and even some
;    preparation performed of the next batch of pixels to be drawn to the screen.
;    Very little pixel preparation is done in 'Frame': it focuses principally on
;    updating the state of all the objects in the game. This routine is called up
;    to 60 times a second.
;
;  - 'RunFXObjModules': This routine is the game's main loop. It is responsible
;    for nearly all of the 'drawing' in the VLM. By 'drawing' we mean preparing
;    the pixel data in RAM that the Jaguar's Object Processor will use to paint
;    the screen after the next vertical sync. You will find 'RunFXObjModules' in
;    the file 'gpu/omega.gas'. This is because (rather unusually) the 'main loop'
;    is being run from within the GPU. 'omega.gas' contains more detail about the
;    implementation, including about how GPU modules are loaded and run by VLM.
;
; The mechanism 'Frame' and 'RunFXObjModules' use to hand off work to each
; other is the 'sync' variable. When the Jaguar's Object Processor has
; completed painting to screen, 'Frame' will reset 'sync' to 0. When
; 'RunFXObjModules' sees that 'sync' is 0 it will start preparing a new frame
; and 'draw' it in the GPU RAM. When it is finished it will set sync to 1, so
; that 'Frame' knows it is time to paint a new frame to the screen again.
;
; Effects and Sub-Effects
; ------------------------------------------------------------------------------
; You may already know that the VLM has 9 'Banks', each with 9 different 'effects'.
; The user selects a bank and effect, and the VLM does the rest: displaying pretty
; pictures synchronized to music played on the system or in response to user input
; on the joypad.
;
; Every effect is defined by 6 'sub-effects'. In this file, sub-effects go by the
; moniker 'fxobj'. To get a sense of the data structure of 'fxobj' take a look
; at the routine 'ifxobj'. A quick glance will tell you there's a lot going on in there,
; but there are two particularly important data points: the type of
; object that it is ('info') and the GPU routine responsible for rendering it ('gpu').
; By way of example, here is the 'info' and 'gpu' data for the 6 sub-effects that make
; up the effect in 'Bank 1-1':
;
; Sub-Effect   Object Description             'info'    'gpu'       GPU Module
; ---------    ---------------------------    --------  --------    ------------
; 1            Digital Video Feedback area    00000004  00010000    gpu/beta.gas
; 2            <Empty>                        00000000  00000000    N/A
; 3            Draw a ring of pixels          00000003  00000009    gpu/alpha.gas
; 4            Draw a ring of pixels          00000003  00000009    gpu/alpha.gas 
; 5            Draw a ring of pixels          00000003  00000009    gpu/alpha.gas 
; 6            Spectrum as intensities        0000000E  00090000    gpu/shu.gas 
;
; The value in 'info' acts as an index into 'vars' for identifying the
; sub-effect during editing. Meanwhile 'gpu' acts as an index into the array
; 'gpumods' in vlmgpu.s and tells 'omega.gas' (which is responsible for
; orchestrating all GPU modules) the specific GPU module to load for the
; particular sub-effect. (Note that the 'gpu' index is shifted 16 bits to the
; right before use: so $00090000 becomes $09.)
;
; If you are interested in the mechanics of drawing the effects and how each
; effect works, 'gpu/omega.gas' is where you will have to start. That file 
; attempts to explain in detail how the sub-effects are processed and is your
; gateway into the other GPU modules that implement each type of sub-effect.
; There are ten types of sub-effect implemented in total, though only eight are
; actually used:
;
;    Module     Description                 'gpu' 
;    --------------------------------------------------
;    alpha      Draw 3D Starfield           $00000
;    beta       Digital Feedback Video Area $10000
;    gamma      Draw a Polygon Object       $20000
;    psi        Post-Load for each effect.  $30000
;    delta      Colour Plasma Area          $40000
;    epsilon    Draw Particle Object        $50000
;    theta      Do Particle motion          $60000
;    sigma      Mono Particle Object        $70000 - UNUSED
;    tau        Matrix object               $80000 - UNUSED
;    shu        Spectrum as intensities     $90000
;    dbeast     Pre-Load for each effect.   $A0000
;
; Every effect is a combination of six sub-effects, as we saw in 'Bank 1-1' above,
; and every sub-effect is one of the eight possible types listed above.
;
; You may wonder what distinguishes two sub-effects of the same type when in use
; in the same effect, such as 'Draw a ring of pixels' which is used three times in
; 'Bank 1-1'. The answer lies in the 64 or so parameters that sit in the first 256
; bytes of the 'fxobj' data structure representing the sub-effect. 
;
; If you want to know more about these other parameters or the contents of the
; data structure of each and every effect in each and every 'Bank': then
; 'banks.s' is for you. It gives an overview of how the bank data is stored,
; decompressed, and details the contents of all effects.
; 
; As a taster, here is the first 256 bytes of the first sub-effect/fxobj for
; Bank 1-1. Keep in mind there are 5 more of these, just to describe the full
; effect of 1-1! Note that 'info' and 'gpu' are at the very end. 
;
; Data as 'Long'    Byte Offset     Offset Name  Description
; --------------    -------------   -----------  -------------------------------
; dc.l $00148000    Byte 0-4                     
; dc.l $01483800    Byte 4-8        dvf_ws_x     DVF window size: X
; dc.l $01122000    Byte 8-12       dvf_ws_y     DVF window size: Y
; dc.l $01850000    Byte 12-16      vfb_xsca     DVF scale: X
; dc.l $01850000    Byte 16-20      vfb_ysca     DVF scale: Y
; dc.l $00000000    Byte 20-24      vfb_angl     DVF rotate angle
; dc.l $00C0FC00    Byte 24-28                   DVF centre of rotation: X
; dc.l $00C07800    Byte 28-32                   DVF centre of rotation: Y
; dc.l $005EEBFF    Byte 32-36                   DVF Delta Intensity
; dc.l $00C00000    Byte 36-40      dstoffx      Destination position: X
; dc.l $00C00000    Byte 40-44      dstoffy      Destination position: Y
; dc.l $00C00000    Byte 44-48      vfb_xpos     DVF window centre: X
; dc.l $00C00000    Byte 48-52      vfb_ypos     DVF window centre: Y
; dc.l $00000000    Byte 52-56      dstoffz      Destination position: Z
; dc.l $00000000    Byte 56-60      vectorx      Vector: X
; dc.l $00200000    Byte 60-64      dy           Destination Y offset
; dc.l $00000000    Byte 64-68      vectory      Vector: Y
; dc.l $00000000    Byte 68-72                   Symmetry Types
; dc.l $00000008    Byte 72-76      rsym_ord     Rotational Symmetry Order
; dc.l $00080000    Byte 76-80      rsym_ste     Rotational Angle Step
; dc.l $00000000    Byte 80-84      rsym_ist     Rotational Angle Step Delta
; dc.l $00008000    Byte 84-88      _i1          Intensity 1
; dc.l $00008000    Byte 88-92      _i2          Intensity 2
; dc.l $00008000    Byte 92-96      _i3          Intensity 3
; dc.l $00008000    Byte 96-100     _i4          Intensity 4
; dc.l $00008000    Byte 100-104    _i5          Intensity 5
; dc.l $00008000    Byte 104-108    _i6          Intensity 6
; dc.l $00000400    Byte 108-112    zamp         Z amplitude
; dc.l $00000000    Byte 112-116    phase1       Fixed point phase 1
; dc.l $00000400    Byte 116-120    phase2       Delta phase 1
; dc.l $00000000    Byte 120-124                 Rotational sym overall phase
; dc.l $00400000    Byte 124-128    phase4       Fixed point phase 2
; dc.l $00006540    Byte 128-132    i            Number of iterations
; dc.l $00000400    Byte 132-136    j            X amplitude
; dc.l $00000400    Byte 136-140    k            Y amplitude
; dc.l $00000001    Byte 140-144                 Number of other iterations
; dc.l $00008000    Byte 144-148    col1         
; dc.l $00000000    Byte 148-152    deltaz       delta Z
; dc.l $00000000    Byte 152-156    thang        choice of Thang
; dc.l $00000000    Byte 156-160                 
; dc.l $00000003    Byte 160-164    asym_fla     
; dc.l $001E7540    Byte 164-168    sine_bas     Sine Table
; dc.l $00C00000    Byte 168-172    rxcen        Rotational Sym centre: X
; dc.l $00C00000    Byte 172-176    rycen        Rotational Sym centre: Y
; dc.l $00000800    Byte 176-180    roscale      Rotational Symmetry scale
; dc.l $00000000    Byte 180-184    roscal2      Rotational scale delta: X
; dc.l $00001000    Byte 184-188    cvx          Colour generator vector: X
; dc.l $00020000    Byte 188-192    cvy          Colour generator vector: Y
; dc.l $00000000    Byte 192-196    roscalei     Rotational scale delta: Y
; dc.l $00000000    Byte 196-200    drxcen       Rotational centre delta: X
; dc.l $00000000    Byte 200-204    drycen       Rotational centre delta: Y
; dc.l $00021555    Byte 204-208    phase5       Delta phase 2
; dc.l $001E7540    Byte 208-212    wave_2       Sine Table
; dc.l $00080000    Byte 212-216    radius       Radius
; dc.l $00010000    Byte 216-220    cvx2         Base col generator vector: X
; dc.l $00010000    Byte 220-224    cvy2         Base col generator vector: Y
; dc.l $0008F000    Byte 224-228    colx         Base colour: X
; dc.l $0008F000    Byte 228-232    coly         Base colour: Y
; dc.l $00000000    Byte 232-236    plot_mod     Destination plot routine
; dc.l $00000000    Byte 236-240    pixsize      Maximum pixel size
; dc.l $65219302    Byte 240-244    height       
; dc.l $00000000    Byte 244-248    _mtrig       Trigger mask
; dc.l $00000004    Byte 248-252    info         Sub-Effect Type: Digital Video Feedback area  
; dc.l $00010000    Byte 252-256    gpu          GPU Module: beta
;
; Most of these parameters are editable through the VLM's super-sekrit editing
; panel.
;
; Editing Effects
; ------------------------------------------------------------------------------
; I've hacked VLM to allow you to access the editing panel without an
; elaborate button sequence: just press 'Pause'. Then to access the 'real'
; editing panel, keep pressing down until the screen changes.
;
; This Is Not A Place of Honor
; ------------------------------------------------------------------------------
; If you're seeing this notice then that means I still haven't figured everything
; out. If you see a mistake or don't understand something, please let me know at
; https://github.com/mwenge/vlm.
;
; ******************************************************************************

.include "../jaguar.inc"
.include "../blitter.inc"
.include "vlm.inc"

; *******************************************************************
; LaunchVLM
;
; This is where execution starts. Set the initial Bank and Effect to
; 9-5 and enter the main loop.
; *******************************************************************
free:
LaunchVLM:
        move    #5,skid          ; Select Effect 5
        movea.l #stack,sp        ; Set 'sp' as our stack pointer.
        move.l  #rrts,davesvec
        move    #0,started ; Indicate we haven't started the VLM yet.
        move    #1,freerun ; Displaying 'Jaguar' 'wait mode'
        move    #9,imatrix       ; Set Bank Number to 10 (9+1)
        bsr.w   everything       ; Set up everything.
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
        move    #4,skid ; Set Effect Number to 4.
        move    #0,imatrix ; Set Bank Number to 1 (0+1).
        move    #0,started ; Indicate we haven't started the VLM yet.
        move.l  #-1,CLUT
        move.l  #-1,CLUT+4

; *******************************************************************
; Main Loop - Sort Of
; This is invoked from cdfront.s which manages the Audio CD control 
; functions and interface to the Virtual Light Machine.
;
; The 'real' main loop is 'RunFXObjModules' in 'omega.gas', which you
; will find invoked by 'titlescr' at the very end of the execution flow
; of the routine 'everything' below.
; *******************************************************************
goagain:
        clr.w   freerun ;  Signal we're in audio reactive mode.
        bsr.w   everything ; The main business of the main loop.
        bra.s   goagain ; Loop ad-infinitum.
        illegal

; *******************************************************************
; everything
; Main business of the main loop.
; This populates our 'beasties' list with all the objects to be drawn. This
; list will later be converted by RunBeasties into a full Object List for the
; Object Processor to draw to the screen.
;
; While each of the entries in the beasties list references regions of pixel
; data to be drawn and where to draw it, we have to actually fill out that
; pixel data with stuff to draw. This is done by 'titlescr' which calls the
; GPU to draw the data.
; *******************************************************************
everything:
        bsr.w   gogo                  ; Initialize the VLM if required.
        tst.w   freerun               ; Are we in audio reactive mode?
        bne.w   nodoit                ; If not, don't initialize the sound DSP module.
        jsr     iansdoit              ; Call the DSP initialistion routine in ians.s.
        
nodoit:
        clr.l   udud
        clr.l   aded
        clr.l   cskr
        
        ; Create 6 initialized fx objects and store them at refblock.
        lea     refblock,a6
        move    #5,d7                 ; We'll do 6 objects.
irb:    bsr.w   ifxobj                ; Initialize the object.
        lea     1024(a6),a6           ; Each fx object is 1024 bytes long.
        dbf     d7,irb                ; Loop until we've done 6 objects.
        
        ; Create the first fx object in fxspace and point to it in a few
        ; different places.
        movea.l #fxspace,a6
        move.l  a6,fx1
        move.l  a6,fxobj
        move.l  a6,fxedbase
        move.l  a6,i_fxobj
        bsr.w   ifxobj                ; Initialize the fx object.
        bsr.w   zapdel                ; Clear the delay parameters
        jsr     gm                    ; Generate the matrices
        
        lea     cubeobj,a0
        jsr     makeclear
        
        lea     genobj,a0
        jsr     makecube
        
        lea     monoobj,a0
        jsr     monovert
        
        lea     board,a0
        jsr     cleol
        
        move    #-1,actime
        
        ; Draw the version details.
        movea.l #versionp,a0          ; "Virtual Light Machine v0.9//(c) 1994 Vi"...
        move    #1,cursx
        move    #1,cursy
        bsr.w   print
        
        ; We create a bunch of entries in the 'beasties' list. These will be converted
        ; by RunBeasties into entries into the actual Object List stored at 'blist'.
        ; When the time comes to actually get the Object Processor to write pixels to
        ; the screen we will copy the contents of the blist to dlist. It is dlist that
        ; the Object Processor will treat as the final Object List for writing.
        
        ; Create the main screen object in the beasties display backing list.
        ; Beastie 0 - Main VLM display.
        lea     beasties,a0
        move.l  draw_screen,d2        ; Make draw_screen the screen data.
        move    #$FFF8,d0
        sub.w   palside,d0
        move    #$2C,d1               ; ','
        add.w   paltop,d1
        sub.w   #$B0,d1
        move    #1,skale              ; Set scale to 1.
        swap    d0
        swap    d1
        move    #0,d5                 ; Set Object Type to 0.
        jsr     makeit                ; Create the screen object in the display list.
        
        tst.w   freerun               ; Are we in audio reactive mode?
        bne.w   zippo ; If not, skip initiatlizing beasties 1 + 2.
        
        ; Beastie 2 -  The editing screen etc.
        lea     beasties+128,a0
        move.l  #board,d2             ; Make board the screen data for the object.
        move    #24,d0
        sub.w   palside,d0            ; Set the X position.
        move    #60,d1
        add.w   paltop,d1             ; Set the Y position.
        swap    d0
        swap    d1
        move    #1,d5                 ; Set the Object Type (ObTypes) to 1.
        move    #$24,d3               ; Set the width.
        move    #$24,d4               ; Set the hight.
        jsr     makeit_transparent
        move    #-1,beasties+140 ; Set Display Object's Mode to off.

        ; Beastie 1 - Unused
        lea     beasties+64,a0
        
        ; Beastie 3 - Jaguar Logo
zippo:  lea     beasties+192,a0
        move    #100,d0               ; X pos
        move    #260,d1               ; Y Pos
        move.l  #jaglogo + $04,d2     ; Make the jaguar logo the screen data.
        move    #5,d5                 ; Set the Object Type to 5
        jsr     makeit_transparent
        move    #16,22(a0)
        move    #-1,OBJ_MODE(a0)         ; Set Display Object's Mode to off.
        move    #$88FF,$F00420 ; Colour lookup table
        move    #$80FF,$F00422 ; Colour lookup table
        
        ; Beastie 4 - Dave's Overlay Object, i.e. the controls?
        lea     davesobj,a0 ; beasties + 256
        move    #80,d0               ; 'P'
        move    #260,d1
        swap    d0
        swap    d1
        move.l  #$4000,d2
        move    #6,d5                 ; Set the Object Type to 6.
        jsr     makeit_transparent
        move    #4,22(a0)
        move    #-1,OBJ_MODE(a0)         ; Set Display Object's Mode to off.
        move    #1,30(a0)
        
        ; Beastie 5 - 
        lea     beasties+320,a0
        move    #$50,d0               ; 'P'
        move    #$104,d1
        swap    d0
        swap    d1
        move.l  #$4000,d2
        move    #6,d5                 ; Set the Object Type to 6.
        jsr     makeit_transparent
        move    #-1,OBJ_MODE(a0)         ; Set Display Object's Mode to off.
        move    #20,22(a0)
        move    #1,30(a0)
        
        ; Beastie 6 - 
        lea     beasties+384,a0
        move    #$50,d0               ; 'P'
        move    #$104,d1
        move.l  #$4000,d2
        move    #6,d5                 ; Set the Object Type to 6.
        swap    d0
        swap    d1
        jsr     makeit_transparent
        move    #-1,OBJ_MODE(a0)         ; Set Display Object's Mode to off.
        move    #36,22(a0)          ; '$'
        move    #1,30(a0)
        
        ; Beastie 7 -  VLM Logo
        lea     beasties+448,a0
        move    #$10A,d0
        move    #$190,d1
        move.l  #vlmlogo,d2
        move    #7,d5 ; Object Type 7
        swap    d0
        swap    d1
        jsr     makeit_transparent
        move    #-1,OBJ_MODE(a0)         ; Set Display Object's Mode to off.
        move    #-1,vlmtim ; Turn off the VLM logo timer.
        
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
        
        ; Create the reamaining 5 fx objects, giving 6 in total.
        lea     fxspace+1024,a6
        move    #4,d7
iprep:  movem.l d7/a5-a6,-(sp)        ; Stash some values in the stack so we can restore them later.
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
        clr.l   action ; Clear the current edit or effect-load action.
        jmp     titlescr              ; Draw the screen. Do all the GPU stuff.


; *******************************************************************
; The routines below are for creating objects from scratch from within
; the editor.
; *******************************************************************
; *******************************************************************
; makeno?
; Make a null object.
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
; Make a poly object from the edit menu.
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
        tst.w   started ; Have we started the VLM already?
        bne.w   rrts ; If so, return.
        move    #1,started ; Indicate we've started the VLM.
        move.l  #$70007,G_END
        move.l  #$70007,D_END
        move    #$100,JOYSTICK
        move    #1,INT1
        move.l  #dumint,($100).w
        move.l  #-1,action ; Clear any existing action.
        bsr.w   startup ; Initialize everything.
        move.l  #-1,action ; Clear any existing action.
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
; Make a DVF (Digital Video Feedback) Scale object from the edit menu.
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
; Make a DVF (Digital Video Feedback) Scale 4 object from the edit menu.
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
; Make a DVF (Digital Video Feedback) area (without clear) from the edit menu.
; *******************************************************************
makefb:
        bsr.w   gadd
        move.l  #1,$8C(a6)
        bra.w   fnop

; *******************************************************************
; makecfb
; Make a DVF (Digital Video Feedback) area (plus clear) from the edit menu.
; *******************************************************************
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
; Make a Ring of Pixels object from the edit menu.
; *******************************************************************
makering:
        bsr.w   gadd
        bsr.w   initring
        move.l  a6,(a0)
        bra.w   ggg

; *******************************************************************
; ipm
; Make a Particle Motion object from the edit menu.
; *******************************************************************
ipm:
        bsr.w   gadd
        clr.l   4(a6)
        move.l  #13,info(a6)
        move.l  #$60000,gpu(a6)
        move.l  a6,(a0)
        bra.w   ggg

; *******************************************************************
; itau
; Make a Matrix object from the edit menu.
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
; Make a Mono Particle object from the edit menu.
; *******************************************************************
mopo:
        bsr.w   mpo
        move.l  #$70000,gpu(a6)
        rts

; *******************************************************************
; mpo
; Make a Particle Object from the edit menu.
; *******************************************************************
mpo:
        bsr.w   makestar
        move.l  #12,info(a6)
        move.l  #$50000,gpu(a6)
        rts

; *******************************************************************
; makeshun
; Make a Spectrum as Intensities object from the edit menu.
; *******************************************************************
makeshun:
        bsr.w   makestar
        move.l  #14,info(a6)
        move.l  #$90000,gpu(a6)
        rts

; *******************************************************************
; makestar
; Make a 3D Starfield from the edit menu.
; *******************************************************************
makestar:
        bsr.w   gadd
        bsr.w   initpsf
        move.l  a6,(a0)
        bra.w   ggg

; *******************************************************************
; amkesurf
; Make a Wave Surf object from the edit menu.
; *******************************************************************
amkesurf:
        bsr.w   gadd
        bsr.w   initwsur
        move.l  a6,(a0)
        bra.w   ggg

; *******************************************************************
; makemono
; Make a Psychedelic Bitmap object from the edit menu.
; *******************************************************************
makemono:
        bsr.w   gadd
        bsr.w   initmono
        move.l  a6,(a0)
        bra.w   ggg

; *******************************************************************
; mplaz2
; Make a Colour Plasma 2 object from the edit menu.
; *******************************************************************
mplaz2:
        bsr.w   mplaz1
        move.l  #$40004,gpu(a6)
        rts

; *******************************************************************
; mplaz1
; Make a Colour Plasma 1 object from the edit menu.
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
; Make a Jaguar Logo object from the edit menu.
; *******************************************************************
mjaglogo:
        bsr.s   makemono
        move.l  #$40003,gpu(a6)
        move.l  #11,info(a6)
        rts

; *******************************************************************
; makeiri
; Make an I-Ripple Bitmap object from the edit menu.
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
        lsl     #2,d0 ; Multiply d0 by 4.
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
; Customize a Psychedelic Bitmap object from the edit menu.
; *******************************************************************
initmono:
        move.l  #6,info(a6)
        move.l  #$40000,gpu(a6)
        rts

; *******************************************************************
; initwsur
; Customize a Wafe Surf object from the edit menu.
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
; Create a 3D Starfield object.
; *******************************************************************
initpsf:
        move.l  #3,plot_mod(a6)
        move.l  #0,gpu(a6)
        move.l  #2,info(a6)
        rts

; *******************************************************************
; initwave
; Create a Polygon object.
; *******************************************************************
initwave:
        move.l  #$20000,gpu(a6)
        move.l  #1,info(a6)
        rts

; *******************************************************************
; initdvf
; Create a Digital Video Feedback Area object.
; *******************************************************************
initdvf:
        move.l  #4,info(a6)
        move.l  #$10000,gpu(a6)
        move.l  #dvf_buf,i(a6)
        move.l  #p_sines,sine_bas(a6)
        rts

; *******************************************************************
; initring
; Create a Ring of Pixels object.
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
; Attach a waveform
; *******************************************************************
wavelink:
        movea.l fxobj,a6

; *******************************************************************
; wlink
; *******************************************************************
wlink:
        lsl     #2,d1 ; Multiply d1 by 4.
        lea     512(a6),a5
        lea     (a5,d1.w),a5
        lea     (a6,d1.w),a4
        move.l  (a4),(a5)
        move.l  d0,256(a4)
        rts

; *******************************************************************
; symadj
;
; Responds to up/down/left/right controller movement and adjusts the
; symmetry accordingly.
; *******************************************************************
symadj:
        ; Update the adsra/adsrb/adsrc arrays.
        lea     pad_now,a1 ; Get the button presses
        bsr.w   doadsr ; Respond to a/b/c being pressed.

        tst.w   vlm_mode ; Are we in audio control mode?
        beq.w   rrts ; If so, return.

        lea     pad_now,a1 ; Get the button presses
        tst.w   editing ; Is editing mode active?
        beq.w   shoop ; If not, skip to shoop.

        lea     pad_now+4,a1 ; Get the button presses from edit mode.

        ; Respond to up/down/left/right in VLM mode.

        ; Update pixcon array.
shoop:  move.b  1(a1),d0
        rol.b   #2,d0
        lea     pixcon,a0
        jsr     pinertco

        ; Update piycon array.
        move.b  1(a1),d0
        rol.b   #4,d0
        lea     piycon,a0
        jsr     pinertco

        ; Update px and py.
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
; Do Attack Decay Sustain Release
; Updates the adsra/adsrb/adsrc array as appropriate. This is used
; in the fxobj.
;
; a1 -> pad_now (buttons pressed)
; *******************************************************************
doadsr:
        tst.w   vlm_mode
        bne.w   ago
        lea     zero,a1

ago:
        movea.l a1,a3 ; Store pad_now/zero in a3.
        lea     envvals,a1

        lea     10(a1),a1
        lea     adsrc,a0
        move.l  (a3),d0 ; Put button presses in d0.
        and.l   #cbutton,d0 ; Set d0 if c button pressed.
        bsr.w   do_adsr

        lea     2(a1),a1
        lea     adsrb,a0
        move.l  (a3),d0 ; Put button presses in d0.
        and.l   #bbutton,d0 ; Set d0 if b button pressed.
        bsr.w   do_adsr

        lea     2(a1),a1
        lea     adsra,a0
        move.l  (a3),d0 ; Put button presses in d0.
        and.l   #abutton,d0 ; Set d0 if a button pressed.

do_adsr:
        move    8(a0),d1
        lea     adsrvex,a4
        lsl     #2,d1 ; Multiply d1 by 4.
        movea.l (a4,d1.w),a4
        jmp     (a4)
        ; Returns

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
; a0 -> adsra for a button/adsrb for b button/adsrc for c button.
; d0 -> whether corresponding button pressed (a,b,c).
; *******************************************************************
attack:
        tst.l   d0
        beq.w   srel
        move    (a1),d1
        move    (a0),d2
        lsr     #2,d2
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
        lsr     #2,d2
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
        lsr     #2,d2
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
        lea     pad_now,a1 ; Get the most recent button presses.
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
        lsl     #2,d0 ; Multiply d0 by 4.
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
        lsr     #1,d6
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

bodb2:  lsr     #1,d6
        lea     4(a3),a3
        dbf     d7,gk2
        rts

someeother:
        move.l  (a1),d0
        and.l   #allbutts,d0 ; A, B, or C pressed?
        bne.w   somebutt
        clr.w   symed
        rts

somebutt:
        move.l  d0,-(sp) ; Stash some values in the stack so we can restore them later.
        movea.l (a0)+,a2
        cmp.l   #cbutton,d0
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
        cmp.l   #(bbutton|cbutton),d0 ; cbutton or bbutton
        bne.w   notab
        jsr     (a2)

notab:
        move.l  (sp)+,d0
        rts

; *******************************************************************
; dorsym1
; *******************************************************************
dorsym1:
        lea     (syminvxdec).l,a3
        move    #SNGLX,symed ; Set snglx as the control display routine.
        bra.w   gjoy
; *******************************************************************
; dorsym2
; *******************************************************************
dorsym2:
        lea     (sympadrsmyd).l,a3
        move    #SISNGLX,symed ; Set sisnglx as the display routine.
        bra.w   gjoy
; *******************************************************************
; dorsym3
; *******************************************************************
dorsym3:
        lea     (sympaddstep).l,a3
        move    #SIDBL,symed ; Set sidbl as the control display routine.
        bra.w   gjoy

; *******************************************************************
; gjoy
; Get joystick/controller input.
; *******************************************************************
gjoy:
        move.l  (a1),d0
        move.l  d0,d1
        and.l   #left,d0
        beq.w   npleft
        movea.l (a3),a4
        jsr     (a4)

npleft:
        move.l  (a1),d0
        and.l   #right,d0
        beq.w   npright
        movea.l 4(a3),a4
        jsr     (a4)

npright:
        move.l  (a1),d0
        and.l   #up,d0
        beq.w   npup
        movea.l 8(a3),a4
        jsr     (a4)

npup:
        move.l  (a1),d0
        and.l   #down,d0
        beq.w   npdn
        movea.l 12(a3),a4
        jsr     (a4)

npdn:
        rts

; *******************************************************************
; srcentre
; *******************************************************************
srcentre:
        move    #CROT,symed ; Set crot as the display routine.
        move.b  1(a1),d0
        rol.b   #2,d0
        bsr.w   rxs
        move.b  1(a1),d0
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
        move    #1,seldb ; Set the selection debounce flag.

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
        move.l  pad_now,d1 ; Get the most recent button presses.
        and.l   #(hash|zerobutton|one|two|three|four|five|six|seven|eight|nine),d1 ; Were any of the number buttons pressed?
        bne.w   rrts ; If yes, return.
        move.l  ov,routine ; Store the old 'routine' in routine.
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
        lea     768(a1),a1
        move.l  a1,d2line

        move    #127,d0
idli:   clr.l   248(a0)
        lea     768(a0),a0
        dbf     d0,idli
        rts

; *******************************************************************
; setedit
; Enable Edit Mode. 
; *******************************************************************
setedit:
        tst.w   vedit ; Are we already editing?
        bne.w   rrts ; If so, return early.
        move    vlm_mode,ovlm_mod ; Stash the current VLM mode.
        move    #1,vedit ; Signal we are in edit mode.
        move    #EDITING_MODE,vlm_mode ; Change mode to editing.
        move    #1,beasties+140
        move.l  #initedit,action ; Set action routine.
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
        bchg    #0,_mtrig+3(a6)
        rts
; ---------------------------------------------------------------------------
bp2:
        bchg    #1,_mtrig+3(a6)
        rts
; ---------------------------------------------------------------------------
bp3:
        bchg    #2,_mtrig+3(a6)
        rts
; ---------------------------------------------------------------------------
bp4:
        bchg    #3,_mtrig+3(a6)
        rts
; ---------------------------------------------------------------------------
bp5:
        bchg    #4,_mtrig+3(a6)
        rts
; ---------------------------------------------------------------------------
bp6:
        bchg    #5,_mtrig+3(a6)
        rts
; ---------------------------------------------------------------------------
bp7:
        bchg    #6,_mtrig+3(a6)
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
;
; Note that the 'main loop' is actually in omega.gas which will loop
; ad-infinitum running the GPU modules for the current effects.
; If it ever bails out, we would end up back in 'goagain' which will
; reinitialize everything.
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
        lsr.l   #1,d2 ; Divide d2 by 2.
        lsr.l   #1,d3 ; Divide d3 by 2.
        swap    d3
        move    d2,d3

        ; dvf_buf is at position 128 of the Digital Video Feedback fx objects.
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
        move.l  dvf_crot_x(a6),(a0)+
        move.l  dvf_crot_y(a6),(a0)+
        move.l  d3,(a0)+
        move.l  dvf_deltai(a6),(a0)+
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
        moveq   #10,d0
        lea     omega,a0
        jsr     gpurun

editloop:
        clr.w   moomoomo
        tst.w   freerun ; Are we in audio reactive mode?
        beq.w   eloop ; If we are, allo editing (if enabled). 
        rts

        ; Do the editing.
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
        bra.s   editloop ; Exit I guess?

; *******************************************************************
; yakedit
;
; Display the current editing screen.
;
; Runs any current 'action' routine.
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
        lsl     #2,d0 ; Multiply d0 by 4.
        movea.l (a6,d0.w),a6
        move.l  a6,fxobj

        ; If we have an active 'action' routine, do it now.
        tst.l   action ; Do we have an action routine?
        beq.w   noact ; If not, skip.
        movea.l action,a0 ; Point a0 at our action routine.
        movea.l fxedbase,a6 ; Point a6 at our fx editor base.
        jsr     (a0) ; Run the action.
        clr.l   action ; Clear it.

noact:  move.l  ixcon,d0
        cmp.l   ixcon,d0
        bne.s   noact
        move.l  d0,iixcon

noact2: move.l  iycon,d0
        cmp.l   iycon,d0
        bne.s   noact2
        move.l  d0,iiycon

        ; Display the controls for whatever we're currently editing.
        move    symed,d0 ; 
        beq.w   blib
        lea     edvex,a0
        subq.w  #1,d0
        lsl     #2,d0 ; Multiply d0 by 4.
        movea.l (a0,d0.w),a0
        jsr     (a0)

blib:   movea.l fxedbase,a6
        bsr.w   shad
        rts

; *******************************************************************
; elcon
; Display a screen of parameters to edit for an effect specified by
; elspace (a0) and vars (a1).
;
; For example if we are displaying the parameters for 'Draw a Ring
; of Pixels' a1 will point to the parameters after the text in:
; ringvars:       dc.b 'Draw a ring of pixels        ',0
;                 dc.b $09 ; Destination Position <--- a1 will point here.
;                 dc.b $1C ; Fixed Point Phase 1
;                 dc.b $1F ; Fixed Point Phase 2
;                 dc.b $20 ; Number of iterations
;                 dc.b $35 ; Radius
;                 dc.b $36 ; Base col generator vector
;                 dc.b $38 ; Base Color
;                 dc.b $3B ; Maximum Pixel size
;                 dc.b $0E ; Vector 
;                 dc.b $15 ; Intensity 1
;                 dc.b $16 ; Intensity 2
;                 dc.b $FF ; End Sentinel
;
; This routine will take care of display each of the parameters on screen
; for selection.
;
; elspace is built up to contain the relevant info for each parameter. It
; is constructed as follows:
;  - Pointer to text of associated entry in editinginfo.
;  - Pointer to the edit routine for the parameter (eparam or unquit).
;  - Pointer to the variables after the text in the associated entry in editinginfo.
; *******************************************************************
elcon:
        move.l  a0,-(sp)        ; Stash some values in the stack so we can restore them later.
        lea     8(a0),a0
        lea     pbinfo,a2       ; "Parameter not yet defined    "
        lea     elinx,a3
        lea     eltxt,a5
        move.l  #elvex,elvp     ; In eparam elvex will point at the variables after the text in the entry in editinginfo.
        
        ; Clear elinx.
        move    #63,d1          ; 
elc1:   clr.l   (a3)+
        dbf     d1,elc1
        
        ; Iterate through all the parameters listed in the vars entry (see above).
        lea     elinx,a3
        move    #1,d4
elc:    move.b  (a1)+,d2        ; Put the current parameter in d2.
        bmi.w   elcend          ; If it's 'FF' we've reached the end of the list, so exit hte loop.
        and.w   #$FF,d2         ; Make sure we're just picking up the byte value.
        move    d2,d7           ; Store it in d7.
        tst.b   (a3,d2)
        bne.s   elc
        
        lsl     #3,d2           ; Multiply the value of the current parameter by 8.
        move    d2,d3
        lsl     #2,d2           ; Multiply the value of the current parameter by 4.
        add.w   d3,d2
        lea     (a2,d2),a4      ; Use the parameter as an index into editinginfo to get the associated entry.
        
        move.l  a4,(a0)+        ; Store the address in editinginfo in elspace.
        move.l  #eparam,d7      ; Set the default edit routine for the parameter.
        tst.b   32(a4)
        bne.w   ucan
        move.l  #unquit,d7      ; 00 indicates it can't be edited?
        
ucan:   move.l  d7,(a0)+
        ; Point elvp at the variable data after the text in the editinginfo entry.
        movem.l a0-a1,-(sp)     ; Stash some values in the stack so we can restore them later.
        movea.l elvp,a0         ; Point a0 at elvp.
        lea     30(a4),a1       ; Point a1 at the beginning of the variables.
        move.l  a1,(a0)+        ; Store it in elspace.
        move.l  a0,elvp         ; Update elvp to point to the new position.
        movem.l (sp)+,a0-a1
        
        ; If the the first byte of the variables is set to 0 then we don't need to fix
        ; up the entry for display. Just skip to the next item. A '01' at this position
        ; indicates the entry has a colon that we need to add more detail to the end of.
        btst    #0,30(a4)       ; Does the entry require fixing up?
        beq.w   nolbit          ; If not, go to next entry.
        
        ; Otherwise we need to fix up the entry, by adding '(X,Y)' or similar. We'll use
        ; eltxt to populate this fixed up text.
        ; First we move our pointer to the position after the colon (:) in the entry.
        move.l  a5,-8(a0)       ; Point the 'text entry' portion of elspace at eltxt.
spect:  move.b  (a4)+,d5
        move.b  d5,(a5)+
        cmp.b   #':',d5         ; ':' Is there a colon?
        bne.s   spect           ; If not, continue looking.
        
        ; Add a space and open parenthesis after the colon.
        move.b  #' ',(a5)+      ; ' '
        move.b  #'(',(a5)+      ; '('
        move.b  1(a4),(a5)+
        move.b  d4,(a3,d7.w)
        lea     (a2,d2),a4
        
llink:  move.b  31(a4),d5
        and.w   #$FF,d5
        move.b  d4,(a3,d5.w)
        lsl     #3,d5           ; Multiply d5 by 8.
        move    d5,d6
        lsl     #2,d5           ; Multiply d5 by 4.
        add.w   d6,d5
        lea     (a2,d5.w),a4
        
        ; Find the colon's position again.
        move.l  a4,-(sp)        ; Stash some values in the stack so we can restore them later.
lcolon: move.b  (a4)+,d5
        cmp.b   #$3A,d5         ; ':'
        bne.s   lcolon
        
        ; Add a comma after it.
        move.b  #$2C,(a5)+      ; ','
        ; Add the 'Y' or similar after it.
        move.b  1(a4),(a5)+
        movea.l (sp)+,a4
        btst    #0,$1E(a4)
        bne.s   llink
        
        ; Close the entry with a close parenthesis.
        move.b  #$29,(a5)+      ; ')'
        move.b  #0,(a5)+
        add.w   #1,d4
        
nolbit: add.w   #1,d1
        cmp.w   d0,d1
        bne.w   elc             ; Loop and process next entry in the parameter list.
        
elcend:
        movea.l (sp)+,a0
        move    d1,(a0)
        move    #0,2(a0)
        move.l  #bline2,4(a0)   ; Joypad to select, any FIRE to edit
        rts

; *******************************************************************
; ifxobj
; Initialize fx object of 1024 bytes. Each of the 9 'effects' in a bank consists
; of 6 sub-effect fx objects giving a total of 6144 (1024 *6) bytes per effect.
;
; a6 points to the position in RAM where we should initialize the object.
;
; The first 256 bytes of a sub-effect fx object are as follows:
;             Offset        Variable Name        Proper Name
;             --------------------------------------------------------------
;  Index 0 -  Byte 0-4:                           
;  Index 1 -  Byte 4-8:     dvf_ws_x             DVF window size: X           
;  Index 2 -  Byte 8-12:    dvf_ws_y             DVF window size: Y           
;  Index 3 -  Byte 12-16:   vfb_xsca             DVF scale: X                 
;  Index 4 -  Byte 16-20:   vfb_ysca             DVF scale: Y                 
;  Index 5 -  Byte 20-24:   vfb_angl             DVF rotate angle             
;  Index 6 -  Byte 24-28:   $18                  DVF centre of rotation: X    
;  Index 7 -  Byte 28-32:   $1C                  DVF centre of rotation: Y    
;  Index 8 -  Byte 32-36:   $20                  DVF Delta Intensity          
;  Index 9 -  Byte 36-40:   dstoffx              Destination position: X      
;  Index 10 - Byte 40-44:   dstoffy              Destination position: Y      
;  Index 11 - Byte 44-48:   vfb_xpos             DVF window centre: X         
;  Index 12 - Byte 48-52:   vfb_ypos             DVF window centre: Y         
;  Index 13 - Byte 52-56:   dstoffz              Destination position: Z      
;  Index 14 - Byte 56-60:   vectorx              Vector: X                    
;  Index 15 - Byte 60-64:   dy                   Destination Y offset         
;  Index 16 - Byte 64-68:   vectory              Vector: Y                    
;  Index 17 - Byte 68-72:                        Symmetry Types               
;  Index 18 - Byte 72-76:   rsym_ord             Rotational Symmetry Order    
;  Index 19 - Byte 76-80:   rsym_ste             Rotational Angle Step        
;  Index 20 - Byte 80-84:   rsym_ist             Rotational Angle Step Delta  
;  Index 21 - Byte 84-88:   _i1                  Intensity 1                  
;  Index 22 - Byte 88-92:   _i2                  Intensity 2                  
;  Index 23 - Byte 92-96:   _i3                  Intensity 3                  
;  Index 24 - Byte 96-100:  _i4                  Intensity 4                  
;  Index 25 - Byte 100-104: _i5                  Intensity 5                  
;  Index 26 - Byte 104-108: _i6                  Intensity 6                  
;  Index 27 - Byte 108-112: zamp                 Z amplitude                  
;  Index 28 - Byte 112-116: phase1               Fixed point phase 1          
;  Index 29 - Byte 116-120: phase2               Delta phase 1                
;  Index 30 - Byte 120-124:                      Rotational sym overall phase 
;  Index 31 - Byte 124-128: phase4               Fixed point phase 2          
;  Index 32 - Byte 128-132: i                    Number of iterations         
;  Index 33 - Byte 132-136: j                    X amplitude                  
;  Index 34 - Byte 136-140: k                    Y amplitude                  
;  Index 35 - Byte 140-144:                      Number of other iterations   
;  Index 36 - Byte 144-148: col1                 Parameter not yet defined    
;  Index 37 - Byte 148-152:                      delta Z                      
;  Index 38 - Byte 152-156: thang                choice of Thang              
;  Index 39 - Byte 156-160:                      Parameter not yet defined    
;  Index 40 - Byte 160-164: asym_fla             Parameter not yet defined    
;  Index 41 - Byte 164-168: sine_bas             Parameter not yet defined    
;  Index 42 - Byte 168-172: rxcen                Rotational Sym centre: X     
;  Index 43 - Byte 172-176: rycen                Rotational Sym centre: Y     
;  Index 44 - Byte 176-180: roscale              Rotational Symmetry scale    
;  Index 45 - Byte 180-184: roscal2              Rotational scale delta: X    
;  Index 46 - Byte 184-188: cvx                  Colour generator vector: X   
;  Index 47 - Byte 188-192: cvy                  Colour generator vector: Y   
;  Index 48 - Byte 192-196: roscalei             Rotational scale delta: Y    
;  Index 49 - Byte 196-200: drxcen               Rotational centre delta: X   
;  Index 50 - Byte 200-204: drycen               Rotational centre delta: Y   
;  Index 51 - Byte 204-208: phase5               Delta phase 2                
;  Index 52 - Byte 208-212: wave_2               Parameter not yet defined    
;  Index 53 - Byte 212-216: radius               Radius                       
;  Index 54 - Byte 216-220: cvx2                 Base col generator vector: X 
;  Index 55 - Byte 220-224: cvy2                 Base col generator vector: Y 
;  Index 56 - Byte 224-228: colx                 Base colour: X               
;  Index 57 - Byte 228-232: coly                 Base colour: Y               
;  Index 58 - Byte 232-236: plot_mod             Destination plot routine     
;  Index 59 - Byte 236-240: pixsize              Maximum pixel size           
;  Index 60 - Byte 240-244: height               Parameter not yet defined    
;  Index 61 - Byte 244-248: _mtrig               Trigger mask                 
;  Index 62-  Byte 248-252: info                 Sub-Effect Type, e.g. 'Ring of Pixels'
;  Index 63-  Byte 252-256: gpu                  Some kind of indicator for the GPU module.
; *******************************************************************
ifxobj:
        ; Clear the 256 4-byte elements (1024 bytes) of the object.
        movea.l a6,a1
        move.l  #255,d0
xxxa:   clr.l   (a1)+
        dbf     d0,xxxa

        ; Initialize the object.
        move.l  #$00008000,col1(a6)
        move.l  #$00C00000,dstoffx(a6)
        move.l  #$00C00000,dstoffy(a6)
        clr.l   dstoffz(a6)
        move.l  #$00000000,phase1(a6)
        move.l  #$00000400,phase2(a6)
        move.l  #$00140000,i(a6)
        move.l  #$00000400,j(a6)
        move.l  #$00000400,k(a6)
        move.l  #$00000400,zamp(a6)
        move.l  #3,asym_fla(a6)
        move.l  #p_sines,sine_bas(a6)
        move.l  #$00200000,dy(a6)
        move.l  #8,rsym_ord(a6)
        move.l  #$00080000,rsym_ste(a6)
        move.l  #$00000000,rsym_ist(a6)
        move.l  #$00C00000,rxcen(a6)
        move.l  #$00C00000,rycen(a6)
        move.l  #$00000800,roscale(a6)
        move.l  #$00000000,roscal2(a6)
        move.l  #$00000000,roscalei(a6)
        move.l  #$00000000,drxcen(a6)
        move.l  #$00000000,drycen(a6)
        move.l  #$00001000,cvx(a6)
        move.l  #$00020000,cvy(a6)
        move.l  #$00080000,radius(a6)
        move.l  #$00008000,_i1(a6)
        move.l  #$00008000,_i2(a6)
        move.l  #$00008000,_i3(a6)
        move.l  #$00008000,_i4(a6)
        move.l  #$00008000,_i5(a6)
        move.l  #$00008000,_i6(a6)
        move.l  #p_sines,wave_2(a6)
        move.l  #$00000000,phase4(a6)
        move.l  #$00021555,phase5(a6)
        move.l  #$00000000,plot_mod(a6)
        move.l  #$1850000,vfb_xsca(a6)
        move.l  #$1850000,vfb_ysca(a6)
        move.l  #$00000000,vfb_angl(a6)
        move.l  #$00C00000,$18(a6)
        move.l  #$00C00000,$1C(a6)
        move.l  #$00780000,$20(a6)
        move.l  #$00000000,4(a6)
        move.l  #$00E00000,8(a6)
        move.l  #$00C00000,vfb_xpos(a6)
        move.l  #$00C00000,vfb_ypos(a6)
        move.l  #$0008F000,colx(a6)
        move.l  #$0008F000,coly(a6)
        move.l  #$00010000,cvx2(a6)
        move.l  #$00010000,cvy2(a6)
        move.l  #$00000000,pixsize(a6)
        move.l  #$00400000,phase4(a6)
        clr.l   thang(a6)
        move.l  #$00000000,info(a6) ; The type of effect, e.g. 'Draw a Ring of Pixels'. See 'vars'. 
        clr.l   height(a6)
        clr.l   _mtrig(a6)

        ; Initialize the 256 bytes in Bytes 256 to 512 of the object with 0XFF.
        ; Initialize the 256 bytes in Bytes 512 to 768 of the object with 0X00.
        ; Initialize the 128 bytes in Bytes 0 to 128 of 'results' with 0x00.
        lea     256(a6),a5
        lea     results,a4
        lea     512(a6),a3
        move    #63,d0 ; 64 longs
zlop:   move.l  #$FFFF,(a5)+
        move.w  #0,(a4)+ ; word
        move.l  #0,(a3)+
        dbf     d0,zlop

        ; Store the 128 bytes (32*4) at oscbank at Bytes 768 to 896 in the fx object.
        lea     768(a6),a3
        lea     oscbank,a4
        move    #31,d0 ; 32 longs
gbsb:   move.l  (a4)+,(a3)+
        dbf     d0,gbsb

        movem.l a0-a1,-(sp) ; Stash a0/a1 in the stack so we can restore them below.

        ; Copy the 108 bytes from 'pixcon' to 'delayn' to to bytes 896 to 1004 in the fx object.
        lea     896(a6),a1
        lea     pixcon,a0
        move.l  #108,d0 ;
        jsr     copybloc

        movem.l (sp)+,a0-a1 ; Restore a0/a1 after copying.
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
        lsr.l   #2,d0 ; Divide d0 by 4.
cram:   clr.l   (a0)+
        sub.l   #1,d0
        bpl.s   cram

        move.b  #1,intmask
        ; Initialize the backing list we used for the 'blist' Object List.
        jsr     InitBeasties ; Initialize the beasties object list.

        move    #-1,db_on ; Enable double-buffering.
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
mrect1: lsl     #1,d2 ; Multiply d2 by 2.
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
        lsr     #1,d2
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
wsnc:   cmp.w   frames,d7
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
        move    #-1,20(a0) ; Caller will set the postfixup.
        clr.w   22(a0)
        move    d5,OBJ_MODE(a0) ; Set the object's mode (index to ModeVex).
        lsl     #3,d5 ; Multiply d5 by 8.
        lea     ObTypes,a1 ; Point a1 at ObTypes
        move    (a1,d5.w),24(a0) ; Put the ObType width in the object.
        move    2(a1,d5.w),26(a0) ; Put the ObType height in the object.
        move    4(a1,d5.w),28(a0) ; Put the ObType depth in the object.
        clr.w   30(a0)
        rts

; *******************************************************************
; copybloc
; Copy a0 to a1 for the number of bytes specified by d0.
; *******************************************************************
copybloc:
        movem.l d0/a0-a1,-(sp) ; Stash some values in the stack so we can restore them later.
copb:   move.b  (a0)+,(a1)+
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
; Waveform
; Help draw the waveform during the 'Edit Basic Waveform' screen. e.g.
;        ....................................-`     
;       .,;;;;;;;;;;;;;;;;;;;;;;;;,,,,+v1tT#TFc     
;       .,;;;;;;;;;;;;;;;;;;;;;;;/%[7f5worr)|+      
;       .,;;;;;;;;;;;;;;;;;;;;i]25we*)=;;;;;;:      
;       .,;;;;;;;;;;;;;;;;;ce2F7r^;;;;;;;;;;;:      
;       .,;;;;;;;;;;;;;=vjFp1%^;;;;;;;;;;;;;;:      
;       .,;;;;;;;;;;;%LSf1);;;;;;;;;;;;;;;;;;:      
;       .,;;;;;;;;/[CSt);;;;;;;;;;;;;;;;;;;;;:      
;       .,;;;;;+?wq#r=;;;;;;;;;;;;;;;;;;;;;;;:      
;       .,;;"!y5yl/;;;;;;;;;;;;;;;;;;;;;;;;;;:      
;       .,iuhus=;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:      
;       'wSt\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:      
;       `)^;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:      
;       .,;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:      
;       .,;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:      
;       .,;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:      
;       .;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,      
;        ____________________________________-      
;                                                   
;                                                
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
        lsl     #4,d2 ; Multiply d2 by 16.
        movea.l edwave,a0
        lea     (a0,d2),a0
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
; Show Spectrum

; Show the current state of the spectrum while in edit mode.
; This consists of a purple horizontal line the player can move up
; or down to adjust the minimum trigger value, and a red and green
; band near the bottom of the screen where the width and position of the
; red band represents the position and width of the band on the spectrum
; that should be used.
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
        lsl     #3,d3 ; Multiply d3 by 8.
        lea     (a4,d3.w),a4
        move    (a4),d3 ; Put the 'Spectrum Start' in d3.
        move    2(a4),d6 ; Put the 'Spectrum End' in d6.
        cmp.w   d3,d6
        bpl.w   shfr1
        exg     d3,d6

shfr1:
        move    d3,bandl ; Put 'Spectrum Start' in bandl.
        move    d6,bandh ; Put 'Spectrum End' in bandh.
        swap    d6
        move    d3,d6

        ; Loop
shfr:   move.l  (a1)+,d3
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
isgrn0: swap    d6
isgrn:  jsr     sblitblockk
        add.w   #4,d0
        dbf     d5,shfr

        move    #60,d0 ; '<'
        move    bandl,d1
        lsl     #2,d1 ; Multiply d1 by 4.
        add.w   d1,d0
        move    bandh,d2
        lsl     #2,d2 ; Multiply d2 by 4.
        sub.w   d1,d2
        add.w   #3,d2
        move    #1,d3
        move.l  #$FFFF,d4
        lea     envvals,a1
        clr.l   d6
        move    band,d5
        lsl     #1,d5 ; Multiply d5 by 2.
        move    (a1,d5.w),d6
        lsr     #8,d6
        lsr     #1,d6
        move    mony,d1
        add.w   #$10,d1
        sub.w   d6,d1
        bmi.w   rrts
        jsr     sblitblockk
        move    #60,d0 ; '<'
        move    #$100,d2
        move    4(a4),d6
        lsr     #8,d6
        lsr     #1,d6
        move    mony,d1
        add.w   #$10,d1
        sub.w   d6,d1
        bmi.w   rrts
        move    #$88FF,d4
        jsr     sblitblockk
        move    6(a4),d6
        lsr     #8,d6
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
        lsr     #8,d3
        lsr     #2,d3
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
        lsr     #8,d2
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
        lsr     #2,d2
        and.w   #$3F,d2 ; '?'
        add.w   d2,d0
        move    #1,d2

zeb:
        move    #$88FF,d4
        movea.l draw_screen,a0
        jmp     sblitblockk

joyy:
        move    piycon,d3
        lsr     #2,d3
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
        lea     256(a6),a5
        lsl     #2,d6 ; Multiply d6 by 4.
        move.b  2(a5,d6.w),d5
        lsr     #3,d5
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
        lea     256(a6),a5
        lsl     #2,d6 ; Multiply d6 by 4.
        move.b  2(a5,d6.w),d5
        lsr     #3,d5
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
        move.l  d0,_monptr
        move    #8,_fmode
        jsr     fmodewai
        move.l  draw_screen,_ein_buf
        move.l  #monbuf+$40,_ein_buf+4
        move.l  (sp)+,d1
        move.l  d1,_ein_buf+8
        move    #$8800,d0
        swap    d0
        move    monptr,d0
        move.l  d0,_monptr
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
        move.l  d0,_monptr
        move    #6,_fmode
        bra.s   fmodewai

; *******************************************************************
; varadd
; Pull out some variables from the relevant entry in editinginfo.
; *******************************************************************
varadd:
        lsl     #3,d0 ; Multiply d0 by 8.
        move    d0,d2
        lsl     #2,d0 ; Multiply d0 by 4.
        add.w   d2,d0 ; 
        lea     pbinfo,a0
        ; Use the index in d0 to select the relevant entry in editinginfo
        ; and get the first byte after the text entry.
        lea     30(a0,d0.w),a0 ;  Get the first byte after the text entry in editinginfo.
        rts

CROT    EQU 1
SNGLX   EQU 2
SISNGLX EQU 3
SIDBL   EQU 4
DVECT   EQU 5
RRTS    EQU 6
WSSHOW  EQU 7
SHSPEC  EQU 8

; *******************************************************************
; edvex
; Routines for display the editing controls for the current screen.
; *******************************************************************
edvex:  dc.l crot, snglx, sisnglx, sidbl, dvect, rrts, wsshow, shspec

; *******************************************************************
; dvect
;
; For example:
;
;    Edit: Base Colour Y
;                                  
;                                  
;                                  
;                                  
;                                    ;;;;;
;                                    ;;;;;
;                                  
;                                  
;                                  
;                                  
;    Press * to attach waveforms    
;   <A> Prev   <B> Menu   <C> Next 
; dv
; *******************************************************************
dvect:
        bsr.w   avxy
        move    #8,d2
        lsr     d2,d0
        lsr     d2,d1
        sub.w   #$7F,d0
        sub.w   #$7F,d1
        add.w   #$BB,d0
        add.w   #$BB,d1
        move    #$A,d2
        move    #$A,d3
        move    #$80FF,d4
        movea.l draw_screen,a0
        jmp     sblitblockk
        ; Returns

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
        asr     #1,d0 ; Divide d0 by 2.
        asr     #1,d1 ; Divide d1 by 2.
        move    d0,d2
        move    d1,d3
        asr     #1,d2 ; Divide d2 by 2.
        asr     #1,d3 ; Divide d3 by 2.
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
        asr     #1,d2 ; Divide d2 by 2.
        asr     #1,d3 ; Divide d3 by 2.
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
;                                                                           
;                                     |}-                                   
;                                     >t_                                   
;    Edit DVF Window Size: Y          >t_
;                                     <t_                                   
;                                     >t_                                   
;                                     >t_                                   
;                                     >t_                                   
;                                     >t_                                   
;                                     >t_                                   
;                                     <t_                                   
;                                     >t'                                   
;         ^cxxxxxxxxxxxxxxxxxxxxxxxxxx!Fa111111111111111111*xxxxxxc,        
;          ...........................lg:```````````````````.......         
;                                     cS_                                   
;                                     cS_                                   
;                                     cg_                                   
;    Press * to attach waveforms     `xF^                                   
;   <A> Prev   <B> Menu   <C> Next    lu
;                                     >t_                                   
;                                     >t_                                   
;                                     |*-                                   
;                                                                                 
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
        move    #-1,d4
        move    #3,d3
        lsr     #8,d5
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
        move    #-1,d4
        move    #3,d2
        lsr     #8,d5
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
        lsr     #8,d5
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
;                                                                           
; For example:
;                                    
;    Edit Y Amplitude               
;                                  
;       ............................                                     
;      /[[[[[[[[[[[[[[[[[[[[[[[[[[[[!???????????????????????????=        
;      ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%viiiiiiiiiiiiiiiiiiiiiiiiiiv:        
;                                  
;                                  
;                                  
;    Press * to attach waveforms    
;   <A> Prev   <B> Menu   <C> Next 
;                                 
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
; Wave Source Show
; 
; Displays the animated wave form during the 'Edit basic waveforms' screen.
;
; For example:
;
;        ....................................-`     
;       .,;;;;;;;;;;;;;;;;;;;;;;;;,,,,+v1tT#TFc     
;       .,;;;;;;;;;;;;;;;;;;;;;;;/%[7f5worr)|+      
;       .,;;;;;;;;;;;;;;;;;;;;i]25we*)=;;;;;;:      
;       .,;;;;;;;;;;;;;;;;;ce2F7r^;;;;;;;;;;;:      
;       .,;;;;;;;;;;;;;=vjFp1%^;;;;;;;;;;;;;;:      
;       .,;;;;;;;;;;;%LSf1);;;;;;;;;;;;;;;;;;:      
;       .,;;;;;;;;/[CSt);;;;;;;;;;;;;;;;;;;;;:      
;       .,;;;;;+?wq#r=;;;;;;;;;;;;;;;;;;;;;;;:      
;       .,;;"!y5yl/;;;;;;;;;;;;;;;;;;;;;;;;;;:      
;       .,iuhus=;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:      
;       'wSt\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:      
;       `)^;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:      
;       .,;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:      
;       .,;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:      
;       .,;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:      
;       .;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,      
;        ____________________________________-      
;                                                   
;                                                
; *******************************************************************
wsshow:
        movea.l edwave,a1
        movea.l draw_screen,a0
        move    #$50,d0 ; 'P'
        move    #$71,d1 ; 'q'

        move    #7,d5
wssh:   move.l  4(a1),d2
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
        move.b  1(a2),d0
        and.w   #$FF,d0
        lsl     #3,d0 ; Multiply d0 by 8.
        move    d0,d1
        lsl     #2,d1 ; Multiply d1 by 4.
        add.w   d1,d0
        lea     pbinfo,a1
        lea     30(a1,d0.w),a1 ; "Parameter not yet defined    "
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
        lsl     #2,d5 ; Multiply d5 by 4.
        lea     256(a3),a4
        tst.w   (a4,d5.w)
        beq.w   rrts
        lea     512(a3),a3
        rts

; *******************************************************************
; crot
;                                                                                                                         
; For example:
;
;    Edit: Destination position Y
;                                  
;                                       ->                                    
;                                       ->                                    
;                                       ->                                    
;                                       ->                                    
;                                       ->                                    
;                                       ->                                    
;                                       ->                                    
;                                       ->                                    
;                                       ->                                    
;                                       ->                                    
;  .;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;/l;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;.   
;                                       ->                                    
;                                       ->                                    
;                                       ->                               
;                                       ->                               
;                                       ->                               
;                                       ->                               
;                                       ->                               
;                                       ->                               
;                                  
;    Press * to attach waveforms    
;   <A> Prev   <B> Menu   <C> Next 
;                                                                           
; *******************************************************************
crot:
        bsr.w   avxy
        move    4(a2),d2
        move    4(a1),d3
        lsl     #2,d2 ; Multiply d2 by 4.
        lsl     #2,d3 ; Multiply d3 by 4.
        move    (a3,d2),d0
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
; Display the 'Choose a subeffect slot to edit' screen.
; *******************************************************************
thisfx:
        movea.l esp,a0
        move.l  #_thisfx,-(a0)
        move.l  a0,esp

_thisfx:
        lea     elspace,a0 ; The array where we'll keep the items from vars to display.
        move    #5,(a0)+
        move    #0,(a0)+
        move.l  #bline2,(a0)+     ; Joypad to select, any FIRE to edit
        
        ; Display the available sub-effects used by the effect.
        ; There are 6 possible sub-effects in total.
        movea.l fx,a1             ; Get the current fx object.
        move    #5,d7             ; We will have 6 entries.
lfx:    move.l  (a1)+,d0          ; Get the next subeffect from fx.
        bne.w   listit            ; If it's not empty, display it.
        ; It's empty.
        move.l  #empt,(a0)+       ; Show the '<Empty>' entry.
        move.l  #initedit,(a0)+   ; Display 'Spectrums and Triggers' if this entry is selected.
        bra.w   lfx2              ; Skip to the next entry.
        ; Show the entry for the sub-effect.
listit: movea.l d0,a2
        move.l  info(a2),d6       ; Use info in the fx object as an index into vars.
        sub.w   #1,d6             ; Subtract 1 from it.
        lea     vars,a3           ; Point a3 at our list of edit options.
        lsl     #2,d6             ; Multiply d6 by 4.
        movea.l (a3,d6.w),a3      ; Use it as an index into vars to get the entry to display.
        move.l  a3,(a0)+          ; Add the entry to elspace.
        move.l  #edit2,(a0)+      ; Set the 'Editing: Effect' screen to be used if this entry is selected.
lfx2:   dbf     d7,lfx            ; Loop until all 6 entries done.
        
        lea     elspace,a1 ; Use elspace populated above as the list of items to display.
        lea     subfxhea,a0       ; Heading is "Choose a subeffect slot to edit"
        bra.w   giedit ; Display the items.
        ; Returns

; *******************************************************************
; foredit
; Edit the Source Function
;
; Editing: Wave Plotter
;   Lists relevant items for this object using 'elcon'.
; *******************************************************************
foredit:
        movea.l esp,a0
        move.l  #fored,-(a0)
        move.l  a0,esp

fored:  move.l  #fored,ledit
        clr.w   _m

        ; Get the effect we have chosen to edit.
        move    fxed,d0
        movea.l fx,a0
        lsl     #2,d0 ; Multiply d0 by 4.
        movea.l (a0,d0.w),a0

        ; Get the appropriate entry in 'vars' for the effect we are editing.
        move.l  info(a0),d6
        lea     vars,a1 ; "Draw a polygon object        "
        sub.w   #1,d6
        lsl     #2,d6 ; Multiply d6 by 4.
        movea.l (a1,d6.w),a1

        ; Use elcon to display the items in the chosen vars entry.
        lea     $1E(a1),a1 ; Point a1 at the parameters stored after the text in our entry.
        lea     wavedhea,a0 ; Display "Editing: Wave Plotter" as header.
        move.l  a0,-(sp) ; Stash some values in the stack so we can restore them later.
        lea     elspace,a0
        move    #$10,d0
        bsr.w   elcon
        lea     elspace,a1
        movea.l (sp)+,a0
        bra.w   giedit

; *******************************************************************
; ahead2
; Edit ADSR envelope shape.
; *******************************************************************
ahead2:
        movea.l esp,a0
        move.l  #_ded2,-(a0)
        move.l  a0,esp

_ded2:
        lea     adsra,a0
        move    selected,d0
        move    d0,d1
        lsl     #3,d0 ; Multiply d0 by 8.
        lsl     #1,d1 ; Multiply d1 by 2.
        add.w   d1,d0
        lea     (a0,d0.w),a0
        move.l  a0,aded
        lea     option6,a1 ; Up/Down to Chooose, L/R to change.
        lea     adedhead2,a0 ; Edit ADSR envelope shape.
        bsr.w   giedit
        move    #ADSRED,editing ; Select adsred as editing routine.
        rts

; *******************************************************************
; ispec
; Display the Spectrum and Triggers edit screen.
;
; Spectrum and Triggers
;  > Trigger 1
;  > Trigger 2
;  > Trigger 3
;  > Trigger 4
;  > Trigger 5
; 
; This is the one displayed when you enter the cheat code.
; *******************************************************************
ispec:
        move.l  #isp1,ledit
        movea.l esp,a0
        move.l  #isp1,-(a0)
        move.l  a0,esp

isp1:   move    #SHSPEC,symed ; Set shpec as the control display routine.
        movea.l #isphead1,a0  ; Screen heading: "Spectrum and Triggers"
        movea.l #option7,a1 ; Option details for display.
        bsr.w   giedit
        move    #SPECED,editing ; Select speced as edit routine.
        rts

; *******************************************************************
; ispec2
; Display the 'Trigger Settings' edit screen.
;
; Trigger Settings
;  > Set Width
;  > Set Trigger Minimum
;
; *******************************************************************
ispec2:
        ; Put this new screen on the stack.
        move.l  #isp2,ledit
        movea.l esp,a0
        move.l  #isp2,-(a0)
        move.l  a0,esp

isp2:   move    #SHSPEC,symed ; Set shpec as the control display routine.
        movea.l #isphead2,a0  ; Screen heading: Trigger Settings
        movea.l #option8,a1 ; Option details for display.
        ; Continue to use speced as the editing routine.
        bsr.w   giedit ; Set up the editing routine.
        rts

; *******************************************************************
; ispec3
; Part ot the 'Spectrum and Triggers' edit module.
; Display the 'Adjust Width Using Joypad' screen.
;   Left/Right moves the position of the trigger.
;   Up/Down adjusts its width.
; *******************************************************************
ispec3:
        move.l  #isp3,ledit
        movea.l esp,a0
        move.l  #isp3,-(a0)
        move.l  a0,esp

isp3:   lea     isphead3,a0 ; Screen heading: Adjust width using joypad
        bsr.w   print
        move    #SPECED2, editing ; Select speced2 as edit routine.
        rts

; *******************************************************************
; ispec4
; Part ot the 'Spectrum and Triggers' edit module.
; Display the 'Adjust trigger minimum with pad' screen.
;   Up/Down adjusts the trigger minimum..
; *******************************************************************
ispec4:
        move.l  #isp4,ledit
        movea.l esp,a0
        move.l  #isp4,-(a0)
        move.l  a0,esp

isp4:
        lea     isphead4,a0 ; "Adjust trigger minimum with pad"
        bsr.w   print
        move    #SPECED3,editing ; Selected speced3 as edit routine.
        rts

; *******************************************************************
; wsedit
; Set up the Edit Source Waves screen.
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
        move    #WSSHOW,symed ; Set the routine for displaying the editing tools to wsshow.
        lea     option4,a1 ; Load the list of waves 1-7 to a1.
        lea     wshead,a0 ; Show the "Edit basic waveforms" text
        bsr.w   giedit ; Set up the list of waves to display.
        move.l  #wtud,action
        move    #SPDINC,editing
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
        bra.w   inogg 
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
; Set up FX Page 1 in edit mode.
;
; Displays
;  'Poly object'
;  'DVF area plus clear'
;  'DVF area no clear'
;  'Ring of pixels'
;  '3D starfield'
;  'WaveSurf'
;  'Psychedelic Bitmap'
;  'I-Ripple Bitmap'
;  'DVF --> scale2'
;  'DVF --> scale4'
;  'Colour plasma 1'
;  'Colour plasma 2'
;   Jaguar Logo
;  'Particle Object'
;  'Particle Motion'
;   Matrix
;   Mono Particle object 
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
; Set up FX Page 2 in edit mode.
; *******************************************************************
setp2:
        move.l  #ssp2,ledit
        movea.l esp,a0
        move.l  #ssp2,-(a0)
        move.l  a0,esp

ssp2:
        lea     avail2,a1

doggo:  lea     ogohead,a0 ; "@~g1:SRCEN:Object Giver Outer~e3:3:"
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
; Display the 'Editing: Effect' screen.
;   Editing Source function -> foredit
;   Editing Symmetry generator -> symedit
;   Edit source waves -> wsedit
; *******************************************************************
edit2:
        movea.l esp,a0
        move.l  #_edit2,-(a0)
        move.l  a0,esp

        move    selected,d0          ; Get the index of the item we selected.
        move    d0,fxed              ; Use that as our index into the fx list.
        move    d0,og                ; And for 'og'.
        
_edit2:
        move    fxed,d0              ; Put the index of fx we're going to edit in d0.
        lsl     #2,d0                ; Multiply d0 by 4.
        movea.l fx,a0                ; Point a0 at our list of current fx objects.
        move.l  (a0,d0.w),fxedbase   ; Point fxedbase at the object we're going to edit.
        lea     edit2hea,a0          ; Display 'Editing: Effect' as the header.
        lea     option2,a1           ; Use 'option2' as the list to display (along with associated routine for each item).
        bra.w   giedit               ; Display the editing and the items.

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
        move    #PADEX,editing
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
        move    #PADEX,editing
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
        move    #PADEX,editing
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
        move    #DLADJ,editing

; *******************************************************************
; udedg
; *******************************************************************
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
        lsr     #1,d3
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
        ; Returns

; *******************************************************************
; old_edit
; Display the 'Edit Mode' screen. THis is accessed when you cycle
; down through the 'Spectrum and Triggers' screen 8 times. 
; *******************************************************************
old_edit:
        move.l  #e_stac,esp
        move.l  #0,e_stac
        clr.w   sec_en
        movea.l esp,a0
        move.l  #_iied,-(a0)
        move.l  a0,esp

_iied:
        lea     edithead,a0 ; Display "Edit Mode"
        lea     option1,a1 ; Use the 'Edit Mode' options list.
        clr.w   symed ; Clear symed, as there's no controls to display.
        ; Fall through

; *******************************************************************
; giedit
; a1 contains the array for display the option list, e.g. option7
; *******************************************************************
giedit:
        clr.w   cbuttf

ogiedit:
        move    #-1,actime
        move.l  a1,editlist ; Store the options array in editlist.
        bsr.w   print ; Display the options
        move    #SELECTOR,editing ; Set selector as the editing routine.
        rts

; *******************************************************************
; wtud
; *******************************************************************
wtud:
        lea     wt,a0 ; "~g2:12:"
        jsr     print
        move    selected,d0
        lsl     #4,d0 ; Multiply d0 by 16.
        movea.l edwave,a1
        lea     (a1,d0.w),a1
        move    $E(a1),d0
        lsl     #2,d0 ; Multiply d0 by 4.
        lea     wts,a2
        movea.l (a2,d0.w),a0
        cmp.w   #24,d0
        beq.w   grint
        move.l  $24(a2,d0.w),8(a1)

grint:
        jsr     print

; *******************************************************************
; ud_selcu
; Move the cursor selection up in edit mode.
; *******************************************************************
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
; Fetch the previous edit screen from the stack.
; *******************************************************************
reedit:
        bsr.w   eunstack
        bra.w   eparam

eunstack:
        movea.l esp,a0
        move.l  (a0)+,editlist
        move    (a0)+,selected
        move    (a0)+,selectable
        move.l  a0,esp
        rts

; *******************************************************************
; estack
; Move the current edit screen on to the stack.
; *******************************************************************
estack:
        movea.l esp,a0
        move    selectable,-(a0)
        move    selected,-(a0)
        move.l  editlist,-(a0)
        move.l  a0,esp
        rts

; *******************************************************************
; eparam
; Display the screen for editing a parameter.
; *******************************************************************
eparam:
        clr.l   ixcon+4
        clr.l   iycon+4
        move    selected,d0
        lsl     #2,d0          ; Multiply d0 by 4.
        lea     elvex,a0
        movea.l (a0,d0.w),a0
        move.l  a0,eddie       ; eddie now points to the variables after the text in the relevant editinginfo entry.
        move.b  2(a0),d0
        and.w   #$FF,d0
        cmp.w   #7,d0          ; Does it use init_byt for the edit routine (Trigger Mask)?
        beq.w   init_byt       ; Go to init_byt
        cmp.w   #6,d0          ; Does it have a bespoke routine for editing? (e.g. Destination Plot Routine and init_sym).
        bne.w   crunt          ; If not, use crunt and return.
        movea.l 6(a0),a1       ; If it does, execute it.
        jmp     (a1)
        ; returns

; *******************************************************************
; init_byt
;
; Used by 'Trigger Mask'
; *******************************************************************
init_byt:
        lea     awfb2,a0
        lea     padbits,a1
        move.l  _mtrig(a6),d2
        move    #7,d7

imabit:
        move.b  (a1)+,d1
        and.w   #$FF,d1
        lsl     #2,d1 ; Multiply d1 by 4.
        lea     (a0,d1.w),a2
        bclr    #7,2(a2)
        btst    #0,d2
        beq.w   imooff
        bset    #7,2(a2)

imooff:
        move.b  #0,3(a2)
        lsr     #1,d2
        dbf     d7,imabit
        move.l  #(dorsymrts),cpad
        move.l  #awfb2,cpad+4
        move    #RRTS,symed
        move    #KPAD,editing
        lea     bytemask,a0
        bsr.w   cprint
        bra.w   ud_butts

; *******************************************************************
; init_sym
;
; Display the following screen:
;
; Editing: Symmetry Planes and Types
; Press number keys to turn off or on
;      1  2
;  8         3
;        9        ------ Rotational symmetry
;  7         4
;     6   5
; 
;   * - Clear
;   # -  Invert
;
; *******************************************************************
init_sym:
        bsr.w   isymbut
        move    d0,symed
        add.w   #1,d0
        move    d0,editing
        lea     symplane,a0 ; Point a0 at the text for the screen.
        bsr.w   cprint ; clear the screen and display the text
        bsr.w   ppmes ; Print the menu options at the bottom
        bra.w   ud_butts ; Display the 0-9 buttons on the screen
        ; Returns

; *******************************************************************
; crunt
; Display the parameter editing screen for whatever parameter is currently
; selected.
;
; d0 -> index value into edvex for edit routine (see editinginfo).
; a0 points to the variables after the text in the parameter's 
; relevant entry in editinginfo.
; *******************************************************************
crunt:
        move    d0,symed     ; Make d0 the index value used for the edit tool display (index to edvex).
        move    d0,d1
        sub.w   #1,d1
        add.w   #1,d0
        move    d0,editing   ; Make d0+1 the index value used for the edit update routine (index to editvex).
        bsr.w   primexy      ; Initialize X/Y Values.
        
        move.l  a0,-(sp)     ; Stash some values in the stack so we can restore them later.
        lea     eparm0,a0    ; Display the 'Edit:' text.
        bsr.w   cprint       ; Clear the message area.
        movea.l (sp)+,a0     ; Restore a0 to point to the vars again.
        
        lea     -30(a0),a0   ; Point at the text in it.
        bsr.w   print        ; Display it.
        lea     eparm2,a0    ; Display "Press ~i+*~i- to attach waveform"
        bsr.w   print
        ; Fall through

; *******************************************************************
; ppmes
; Display: <A> Prev   <B> Menu   <C> Next
; *******************************************************************
ppmes:
        movea.l #eparm1,a0  ; Display <A> Prev   <B> Menu   <C> Next"
        bra.w   print
        ; Returns

; *******************************************************************
; isymbut
; *******************************************************************
isymbut:
        lea     symbutts,a0
        lea     padbits,a1
        move.l  asym_fla(a6),d2

        move    #7,d7
isybit: move.b  (a1)+,d1
        and.w   #$FF,d1
        lsl     #2,d1 ; Multiply d1 by 4.
        lea     (a0,d1.w),a2
        bclr    #7,2(a2)
        btst    #0,d2
        beq.w   isooff
        bset    #7,2(a2)
isooff: move.b  #0,3(a2)
        lsr     #1,d2
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
; Prime the current X and Y values for display.
;
; a0 points to the variables after the text in the parameter's 
; relevant entry in editinginfo.
; *******************************************************************
primexy:
        move    selected,lselecte
        movea.l fxobj,a3
        move    4(a0),d2 ; Store the index to current value in fxobj in d2.
        move    d2,monitor ; Store it in monitor.
        addi.w  #1,monitor ; Add 1.
        move    #1,_m
        move.l  6(a0),d3 ; Store max in d3
        lea     ixcon,a2
        bsr.w   s16 ; Update the value in ixcon to the appropriate length.

        movea.l fxobj,a3
        btst    #0,(a0) ; Should we get the Y value from the entry after this one?
        beq.w   rrts ; If not. return now.
        
        ; Get our Y value from the next entry after it.
        ; Get the second value after the text in the editinginfo entry.
        ; Turn it into an index we can use to get the Y value from the 
        ; entry after it. So if we're editing 'DVF Window size: X', this
        ; will fetch the values from 'DVF WIndow size: Y' in the entry after it.
        move.b  1(a0),d0
        and.w   #$FF,d0
        lsl     #3,d0 ; Multiply d0 by 8.
        move    d0,d2
        lsl     #2,d0 ; Multiply d0 by 4.
        add.w   d2,d0

        ; Use the default parameters associated iwth the first entry in editinginfo.
        lea     pbinfo,a0
        lea     30(a0,d0.w),a0 ; Move to the position after the 'Parameter not defined text'.
        move    4(a0),d2 ; Put min value in d2?
        move.l  6(a0),d3 ; Put max value in d3?

        lea     iycon,a2

; *******************************************************************
; s16
; Gets the current value in the fx object for displaying.
;
; d3 -> the current value for x or y in the fx object.
; a3 -> fxobj (current effect object)
; a2 -> pointer to ixcon/iycon
; a0 -> points to the variables after the text in the parameter's 
; relevant entry in editinginfo.
; *******************************************************************
s16:
        ; Third variable in the editinginfo entry contains an index
        ; to the value in fxobj we want to edit. 
        move    4(a0),d0 ; Put the index to value in fxobj in d0.
        lsl     #2,d0 ; Multiply d0 by 4.
        lea     256(a3),a4
        lea     (a3,d0.w),a3 ; Point a3 at the location in fxobj of the value we want to edit.
        tst.w   (a4,d0.w)
        beq.w   shall

        ; Something to do with the maximum possible value?
        lea     512(a3),a3
shall:  move.l  (a3),d7
        move.b  3(a0),d0 ; Put the type of the value from the editinginfo entry in d0.
        btst    #7,d0
        beq.w   nsiggn
        move.l  d3,d4
        lsr.l   #1,d4 ; Divide d4 by 2.
        add.l   d4,d7

nsiggn: and.w   #$7F,d0 ; Make sure our 'type' index is a positive value.
        lsl     #2,d0 ; Multiply d0 by 4.
        lea     vtypes,a3 ; Point a3 at vtypes below.
        movea.l (a3,d0.w),a3 ; Get the entry into vtypes for our 'type' index.
        jmp     (a3) ; Run it.
        ; returns

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

; *******************************************************************
; editquit
; Quit out of the editing mode.
; *******************************************************************
editquit:
        lea     normal,a0 ; Set on-screen text to: "Standard Mode"
eq:     bsr.w   cprint ; Print it to the screen.
        clr.w   editing ; Signal we're not editing anymore.
        clr.l   star_on ; Clear bank/effect selection so we don't attempt to load it again.
        move    ovlm_mod,vlm_mode ; Restore the VLM mode we were in before entering edit mode.
        rts

; *******************************************************************
; cprint
; Clear the message area.
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

       ; Display the edit option list, e.g. the contents of option7.
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
        move    d3,selectable
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
        lsl     d1,d0
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
        movem.l d0-d5/a0-a2,-(sp)   ; Stash some values in the stack so we can restore them later.
        
        ; Check if we're in a vertical blank or not.
        move    INT1,d0
        move    d0,-(sp)            ; Stash some values in the stack so we can restore them later.
        btst    #0,d0               ; Are we in a vertical blank?
        beq.w   CheckTimer          ; If not, skip everything and check for input only.
        
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
ncc:    movem.l d6-d7/a3-a6,-(sp)   ; Stash some values in the stack so we can restore them later.
        movea.l blist,a0            ; Stash blist in a0.
        movea.l dlist,a1            ; Stash dlist in a1.
        
        moveq   #$40,d0             ; 0x40 units of 4 bytes each to be copied.
xlst:   move.l  (a0)+,(a1)+         ; Copy 4 bytes from blist to dlist.
        dbf     d0,xlst             ; Keep copying until we run out of bytes.
        
        ; Build the display list in 'blist' from 'beasties' for the next frame.
        ; The 'beasties' list is populated mostly in 'everything' which runs in our 'goagain' mainloop.
        bsr.w   RunBeasties
        
        ; In our 'goagain' mainloop we prepare draw_screen (double-buffered screen) with
        ; objects drawn by the GPU/Blitter. Once it's ready we set
        ; screen_ready. Here we check if screen_ready is set and if so, we swap
        ; draw_screen into 'cscreen' (current screen). We will then update the
        ; main screen object in the Display List to reference this new address.
        
        ; Check if we can swap in the new screen prepared in 'omega.gas'.
        tst.l   screen_ready        ; has 'omega' finished preparing a new screen?
        beq.w   no_new_screen       ; If no, go to no_new_screen.
        tst.l   sync                ; Has omega.gas signalled it is safe to swap screens, i.e. a new screen is awaiting rendering?
        beq.w   no_new_screen       ; If not, go to no_new_screen.
        
        ; Swap draw_screen into cscreen so that the new screen can be used in the Object List.
        ; Note that dscreen points to draw_screen so it is draw_screen we are swapping in here.
        move.l  cscreen,d1          ; Stash cscreen in d1.
        move.l  dscreen,cscreen     ; Overwrite cscreen with dscreen.
        move.l  d1,dscreen          ; Overwrite dscreen with stashed cscreen.
        move.l  d1,draw_screen      ; Overwrite draw_screen with stashed cscreen.
        clr.l   screen_ready        ; Signal that a new screen is required before we come here again.
        clr.l   sync                ; Signal to omega it can build a new screen.
        
no_new_screen:
        move.l  cscreen,d5
        movea.l dlist,a0            ; Point a0 at the display list
        move    db_on,d7            ; Is double-buffering enabled
        bmi.w   no_db               ; If not, skip to warp flash.
        tst.w   scron ; Are we strobing?
        beq.w   stdb ; If not, skip to stdb.
        bpl.w   no_db ; If yes skip updating the main screen item in the Object list.
        clr.w   scron ; Reset strobing.
        bra.w   no_db ; Skip to no_db.
        
        ; Update the main screen item in the Object List with the address of the new screen contained
        ; in cscreen. This has the effect of ensuring all the objects we drew with the GPU
        ; and Blitter in 'mainloop' are actually written to the screen by the Object Processor.
stdb:   move.l  d5,d6
        add.l   hango,d6
        and.l   #$FFFFFFF8,d6       ; lose three LSB's
        lsl.l   #8,d6               ; move to correct bit position
        move.l  (a0),d1             ; get first word of the BMO
        and.l   #$7FF,d1            ; clear data pointer
        or.l    d6,d1               ; mask in new pointer
        move.l  d1,(a0)             ; replace in OL
        lea     $40(a0),a0          ; This skips to the next object in the object list.
        add.l   dbadj,d5            ; Move to the next object in cscreen.
        dbf     d7,stdb             ; Keep looping until we've done all double-buffered screens.

no_db:  tst.w   flash                ; Are we strobing?
        beq.w   cflash               ; If not, skip to 'cflash'.
        subi.w  #16,flash            ; Decrement the strobe timer.
        move    flash,d0             ; Store the timer in d0.
        or.w    #$7700,d0            ; Make sure some of the top bits are set.
        move    d0,d1                ; Store in d1.
        swap    d0                   ; Swap the words in d0.
        move    d1,d0                ; Store the value in d1 in the lower word of d0.
        move.l  d0,BG                ; Use d0 to set our background color.
        move.l  d0,BG+4              ; Use d0 to set our background color.
        tst.w   flash                ; Have we used up the strobe timer yet?
        bne.w   cflash               ; If not, skip to cflash.
        move    #-1,scron            ; If the strobe timer is finished, turn off strobing.
        
        ; Maybe do a 'Strobe'..
cflash: cmpi.w  #VLM_MODE,vlm_mode   ; Are we in VLM mode?
        bne.w   dflash               ; If not, skip. Otherwise..
        move.l  pad_now,d0           ; Get the button presses.
        and.l   #cbutton,d0          ; Was the C button pressed?
        beq.w   dflash               ; If not, skip to 'dflash'
        tst.w   flash_db             ; Otherwise, check if we are strobing already.
        bne.w   eflash               ; If we are, skip to 'eflash'.
        move    #1,flash_db          ; Turn on strobing selection debounce (to prevent selection again).
        move    #256,flash           ; Start the strobe timer.
        move    #1,scron             ; Turn on strobing
        bra.w   eflash               ; Skip to eflash.
        
dflash: clr.w   flash_db             ; Make sure strobing debounce is off.
        
        ; Check the VLM logo timer.
eflash: tst.w   vlmtim               ; Is the VLM logo timer active?
        bmi.w   ncanc                ; If not, skip to 'ncanc'.
        subi.w  #1,vlmtim            ; Decrement the VLM logo timer.
        bpl.w   ncanc                ; If it's still active, skip to 'ncanc'.
        move    #-1,davesobj+204     ; Otherwise, turn off the VLM logo.
        
        ; Check for input from the controller. We get a new reading
        ; and compare with the previous so that we 'debounce' the input,
        ; i.e. check the button was definitely pressed.
ncanc:  jsr     readpad              ; Update pad_now.
        tst.w   seldb                ; Is the debounce flag set?
        beq.w   do_ed                ; If not, go ahead.
        move.l  pad_now,d1           ; Store the current reading.
        or.l    pad_now+4,d1         ; Compare with the old reading.
        and.l   #anybutton,d1        ; Were any of the buttons pressed in both the old and new reading?
        bne.w   no_ksel              ; Skip past everything.
        
        ; A button was definitely pressed and should be acted upon.
        clr.w   seldb                ; Clear the debounce flag.
do_ed:  tst.w   vlm_mode             ; Are the VLM edit controls active?
        beq.w   no_ed                ; If not, skip to no_ed.
        
        ; The VLM edit controls are active.
        move    editing,d0           ; Put current editing mode in d0.
        beq.w   no_ed                ; If none, skip to no_end.

        ; We're in editing mode - run an edit routine if necessary.
        tst.l   action               ; Are we in the middle of an action?
        bne.w   no_ksel              ; If we are, skip to no_ksel.
        subq.w  #1,d0                ; Subtract 1 from the current editin gmode.
        lea     editvex,a0           ; Point a0 at edit vex.
        lsl     #2,d0                ; Multiply d0 by 4.
        movea.l (a0,d0.w),a0         ; Use d0 as an index into 'editvex' to get the edit routine.
        jsr     (a0)                 ; Run the edit routine.
        bra.w   no_ksel              ; Skip over the 'not editing' activity below.

        ; We're not in editing mode.
no_ed:  tst.l   action ; Do we have an active 'action' routine?
        bne.w   no_ksel ; If so, skip bank/level selection.
        move.l  pad_now,d0 ; Get buttons pressed.
        and.l   #optionbutton,d0     ; Was the option button pressed?
        bra.w   nothash              ; If not, skip to 'nothash', otherwise..
        
        ; ..turn on the VLM logo and VLM mode.
vlm_on: move    #1,seldb             ; Set the selection debounce flag.
        move    #VLM_MODE,vlm_mode
        move    #1,beasties+140
        move    #7,davesobj+204      ; Turn on the logo.
        move    #500,vlmtim          ; Set the VLM logo timer.
        
nothash:
        tst.w   editing ; Are we in editing mode?
        bne.w   no_ksel ; If so, skip to no_ksel (can't change effect during editing?).
        move.l  pad_now,d0 ; Get the most recent button presses.
        move.l  d0,d1
        move.l  d1,d0
        and.l   #asterisk,d0
        beq.w   knobby

        ; Check if we've selected a new bank combination for a new effect (e.g. 8-9).
        move.l  d1,d0
        and.l   #(zerobutton|one|two|three|four|five|six|seven|eight|nine),d0 ; Were any of the number buttons pressed?
        beq.w   no_ksel             ; No, skip to no_ksel.
        
        ; Get the number that was pressed.
        bsr.w   dcode               ; Translate the number selected to an effect number.
        and.w   #$FF,d2             ; We're only interested in the last byte.
        sub.b   #$30,d2             ; Subtract out the 30.
        bne.w   pharty              ; If we have a valid value, let's enter it.
        bra.w   no_ksel             ; Otherwise skip to no_ksel (no effect was selected).
        
        ; An effect was selected, so shuffle the current effect value into the
        ; bank value, and set debounce on to ensure the button is pressed..
pharty: move    star_on+2,star_on   ; Shuffle the effect value into the bank value.
        move    d2,star_on+2        ; Store the new button press as the effect.
        move    #1,seldb            ; Set the selection debounce flag.
        bra.w   no_ksel             ; Skip to no_ksel.
        
        ; Was a new bank or effect selected?
knobby: tst.l   star_on             ; Test if one was turned on.
        beq.w   no_ksel             ; If not, skip to no_ksel.
        
        ; A new bank or effect (or both) was selected.
        move    star_on,d0          ; Store bank selection in d0.
        bne.w   setbth              ; If star_on is non-zero we have both a bank and effect selected, so do both.
        ; Just a new effect was selected.
        move    star_on+2,skid      ; Get the selected effect.
        move.l  #skidoo,action      ; Our next call to the 'action' routine will enable.
        bra.w   n_ks                ; Skip to n_ks.
        
        ; We've got an effect to load so load it.
setbth: sub.w   #1,d0               ; Decrement the effect number so we can use it as an index.
        move    d0,imatrix          ; Set the bank number
        move    star_on+2,skid      ; Set the effect number within the bank.
        move.l  #gm,action          ; Load the bank and the effect.
        
n_ks:   clr.l   star_on             ; Clear bank/effect selection so we don't attempt to load it again.
        
no_ksel:
        ; Detect the cheat-mode enable for editing.
        move.l  pad_now,d0          ; Get button press.
        
        ; Temporarily use pause button to enter edit mode.
        ; cmp.l   #(seven|asterisk|zerobutton|three),d0
        cmp.l   #pausebutton,d0     ; Was the edit cheat code (037#) selected?
        bne.w   nse1                ; Edit not selected, skip to nse1.
        ; Edit mode was enabled!
        jsr     setedit             ; Enable Edit mode.
        bra.w   nse2                ; Skip next line, leave editing enabled.
        
nse1:   clr.w   vedit               ; Clear edit enabled.
        
        ; Perform whatever the current 'fx' routine is. Can be one of:
        ; - symadj, keydb, ov, rrts.
nse2:   movea.l _fx,a0
        jsr     (a0)

        tst.l   action ; Do we have an active 'action' routine?
        bne.w   gharbaj ; If so, don't run the 'routine' routine - skip to gharbaj.
        
        ; Perform whatever the current 'routine' is. Can be one of:
        ; - symadj, keydb, ov, rrts.
        movea.l routine,a0
        jsr     (a0)
        
        ; Return from the interrupt. Restore stuff stashed in the stack etc.
gharbaj:
        movem.l (sp)+,d6-d7/a3-a6   ; Restore stashed values from the stack.
        
CheckTimer:
        move    (sp)+,d0            ; Restore stashed values from the stack.
        move    d0,-(sp)            ; Stash some values in the stack so we can restore them later.
        btst    #3,d0
        beq.w   exxit
        
exxit:  move    (sp)+,d0            ; Restore stashed values from the stack.
        lsl     #8,d0
        move.b  intmask,d0
        move    d0,INT1
        move    d0,INT2
        movem.l (sp)+,d0-d5/a0-a2   ; Restore stashed values from the stack.
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
; Select matrix
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
; This copies the selected effect from selected bank into the current fx object.
; *******************************************************************
skidoo:

        ; Point a1 at the beginning of our selected effect data.
        movea.l #fxedbase+8,a0
        move    skid,d0 ; Store the selected effect in d0.
        sub.w   #1,d0 ; Subtract 1.
        mulu.w  #6144,d0 ; Multiply it by the length of effect data (6144 bytes)
        lea     matrix,a1 ; Point a1 at our bank data.
        adda.l  d0,a1 ; Add d0 to point a1 at the bank data of our selected effect.

        ; Loop to copy the 6144 bytes of effect data (6 sub-effects of 1024 bytes
        ; each) from our selected effect into fxedbase.
        ; a0 - fxedbase
        ; a1 - points to bank data in matrix
        move    #5,d0
slett:  clr.l   (a0)
        tst.l   info(a1)
        beq.w   sletto
        move.l  a1,(a0)
        
        ; Inner loop
        lea     768(a1),a2
        move    #7,d2
        lea     sineslookup,a3
yuz:    move    14(a2),d3
        lsl     #2,d3 ; Multiply d3 by 4.
        cmp.w   #24,d3
        beq.w   yuz2
        move.l  (a3,d3.w),8(a2)
yuz2:   lea     16(a2),a2
        dbf     d2,yuz

sletto: lea     1024(a1),a1
        lea     4(a0),a0
        dbf     d0,slett ; Loop for all 6.

        lea     pixcon,a1
        movea.l fx1,a0
        lea     $380(a0),a0
        move.l  #108,d0 ; 'l'
        jsr     dcopyblo
        bsr.w   zapdel
        clr.w   og
        move    #-1,beasties+204
        rts
        rts

; *******************************************************************
; dcopyblo
; Copy a0 to a1 for the number of bytes specified by d0.
; Stash the delay variables while you're doing it so they don't get
; overwritten.
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
SELECTOR EQU 1
XY1      EQU 2
X_ONE    EQU 3
X_ONE_2  EQU 4
XY1_2    EQU 5
XY1_3    EQU 6
KPAD     EQU 7
AWF_X    EQU 8
AWF_Y    EQU 9
SPDINC   EQU 10
ADSRED   EQU 11
DLADJ    EQU 12
PADEX    EQU 13
SPECED   EQU 14
SPECED2  EQU 15
SPECED3  EQU 16

editvex:
        dc.l selector, xy1, x_one
        dc.l x_one, xy1, xy1, kpad
        dc.l awf_x, awf_y, spdinc, adsred
        dc.l dladj, padex, speced, speced2
        dc.l speced3

; *******************************************************************
; speced3
; Edit a spectrum. This is the first forty bytes in avbank, five sets
; of four words (byte-pairs) representing one of the 5 Spectrum/Triggers.
;
; Display the 'Adjust trigger minimum with pad' screen.
;   Up/Down adjusts the trigger minimum..
; *******************************************************************
speced3:
        move    band,d0        ; Store the current selected Trigger/Spectrum in d0.
        lsl     #3,d0          ; Multiply d0 by 8.
        lea     avbank,a0      ; Point a0 at avbank.
        lea     (a0,d0.w),a0   ; Point a0 at the selected band in avbank.
        
        move.l  pad_now,d0     ; Store the button pressed in d0.
        move.l  d0,d1          ; And store in d1 too.
        and.l   #up,d0         ; Was 'Up' pressed?
        beq.w   fobb
        addi.w  #$100,6(a0)    ; Add 256 to the minimum trigger.
        bra.w   padex          ; Return and check if user has clicked to exit screen.
        
fobb:   and.l   #down,d1       ; Was 'Down' pressed?
        beq.w   padex          ; Ifso, return and check if user has clicked to exit screen.
        subi.w  #$100,6(a0)    ; Deduct 256 from the minimum trigger.
        bra.w   padex          ; Return and check if user has clicked to exit screen.

; *******************************************************************
; speced2
; Edit a spectrum. This is the first forty bytes in avbank, five sets
; of four words (byte-pairs) representing one of the 5 Spectrum/Triggers.
;
; Display the 'Adjust Width Using Joypad' screen.
;   Left/Right moves the position of the trigger.
;   Up/Down adjusts its width.
; *******************************************************************
speced2:
        ; Debounce, don't react to quickly to up/down/left/right input.
        ; Only update after every 8 frames.
        move    frames,d0
        and.w   #7,d0
        bne.w   padex          ; Return and check if user has clicked to exit screen.
        
        ; Figure out which Trigger/Spectrum we are editing and point at the
        ; associated band of 8 bytes of data at the start of avbank.
        move    band,d0        ; Store the current selected Trigger/Spectrum in d0.
        lsl     #3,d0          ; Multiply d0 by 8.
        lea     avbank,a0      ; Point a0 at avbank.
        lea     (a0,d0.w),a0   ; Point a0 at the selected band in avbank.
        
        move.l  pad_now,d0     ; Store the button pressed in d0.
        move.l  d0,d1          ; And store in d1 too.
        and.l   #up,d0         ; Was 'Up' pressed?
        beq.w   speced2a       ; No, check if down was pressed.
        
        ; Up was pressed.
        tst.w   (a0)
        beq.w   wied1
        subi.w  #1,(a0)        ; Subtract the start of the spectrum to widen it on the left.
wied1:  cmpi.w  #$3F,2(a0)     ; Are we at the maximum value of 63?
        bge.w   padex          ; If so, return and check if user has clicked to exit screen.
        addi.w  #1,2(a0)       ; Otherwise increment the spectrum to widen it on the right.
        bra.w   padex          ; Return and check if user has clicked to exit screen.
        ; Returns
        
        ; Check if down was pressed.
speced2a:
        move.l  d1,d0
        and.l   #down,d0       ; Was 'Down' pressed?
        beq.w   speced2b       ; If not, check if left pressed.
        
        ; Down was Pressed.
        move    2(a0),d0
        sub.w   (a0),d0
        cmp.w   #2,d0
        ble.w   padex          ; Check if user has clicked to exit screen.
        addi.w  #1,(a0)        ; Shortedn it on the left.
        subi.w  #1,2(a0)       ; Shorten it on the right.
        bra.w   padex          ; Check if user has clicked to exit screen.
        
        ; Check if 'Left' was pressed.
speced2b:
        move.l  d1,d0
        and.l   #left,d0
        beq.w   speced2c
        tst.w   (a0)
        beq.w   padex          ; Return and check if user has clicked to exit screen.
        subi.w  #1,(a0)        ; Move the start point to the left.
        subi.w  #1,2(a0)       ; Move the end point to the left.
        bra.w   padex          ; Return and check if user has clicked to exit screen.
        
        ; Check if 'Right' was pressed.
speced2c:
        and.l   #right,d1
        beq.w   padex          ; Return and check if user has clicked to exit screen.
        cmpi.w  #$3F,2(a0)     ; '?'
        bge.w   padex          ; Return and check if user has clicked to exit screen.
        addi.w  #1,(a0)        ; Move the end point to the right.
        addi.w  #1,2(a0)       ; Move the end point to the right.
        bra.w   padex          ; Return and check if user has clicked to exit screen.
        ;Returns

; *******************************************************************
; speced
; *******************************************************************
speced:
        move    selected,band
        bra.w   selector

; *******************************************************************
; iawfx
; Waveform Attach (X)
; *******************************************************************
iawfx:
        bsr.w   iwf
        move    #AWF_X,editing ; Select awf_x as edit routine.
        movea.l cwed1,a0
        tst.w   256(a0)
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
        move    256(a0),d1

; *******************************************************************
; issi
; *******************************************************************
issi:
        move.b  (a2)+,d2
        and.w   #$FF,d2
        lsl     #2,d2 ; Multiply d2 by 4.
        lea     2(a1,d2),a3
        lea     2(a4,d2),a5
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
        lsr     #1,d1
        dbf     d0,issi
        clr.l   d0
        move    $102(a0),d0
        lsl.l   #8,d0
        move.l  d0,ixcon
        bra.w   dud_butt

; *******************************************************************
; kpass
; Assign effect to keypad
; Press the number key 1-9 to which you want this effect attached
; *******************************************************************
kpass:
        movea.l #kpasshea,a0  ; Assign effect to keypad//Press t"...
        jsr     print
        move    #KPAD,editing ; Set kpad as editing routine.
        move.l  #(bpsymrts),cpad
        move.l  #kpassbut,cpad+4
        bsr.w   ppmes ; Display the assign button options.
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
; Attach and Adjust Waveforms
; Press keys 1 to 8 to link waveforms
; Use the joypad to change amplitude
; Press any FIRE button to exit
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
        lsl     #2,d1 ; Multiply d1 by 4.
        movea.l fxobj,a6
        lea     (a6,d1.w),a5
        move.l  a5,cwed1
        bsr.w   varadd
        move.b  1(a0),d0
        and.w   #$FF,d0
        move    d0,cwave2
        lsl     #2,d0 ; Multiply d0 by 4.
        lea     (a6,d0.w),a5
        move.l  a5,cwed2
        move.l  #gskirts,cpad
        move.l  #awfbutts,cpad+4
        rts

; *******************************************************************
; iawfy
; Waveform attach (Y)
; *******************************************************************
iawfy:
        bsr.s   iwf
        move    #AWF_Y,editing ; Select awf_y as edit routine.
        movea.l cwed2,a0
        tst.w   256(a0)
        bne.w   istat
        move    cwave2,d1
        move.l  #$8000,d0
        bsr.w   wavelink
        bra.w   istat

; *******************************************************************
; awf_y
; *******************************************************************
awf_y:
        lea     pad_now,a1 ; Get the most recent button presses.
        move.b  1(a1),d0
        rol.b   #4,d0
        movea.l cwed2,a4
        bra.w   wset

; *******************************************************************
; awf_x
; *******************************************************************
awf_x:
        lea     pad_now,a1 ; Get the most recent button presses.
        move.b  1(a1),d0
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
        and.l   #allbutts,d0; A, B, or C pressed?
        beq.w   rrts
        movea.l eddie,a2
        btst    #0,(a2)
        beq.w   soxx
        move.l  #rwfa,action
        bra.w   sdb

; *******************************************************************
; setantelope
; *******************************************************************
setantelope:
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
        bchg    #0,256(a4)

; *******************************************************************
; clant
; *******************************************************************
clant:
        move    #2,antelope
        clr.w   ant_data
        rts
; ---------------------------------------------------------------------------

ant2:
        bchg    #1,256(a4)
        bra.s   clant
; ---------------------------------------------------------------------------

ant3:
        bchg    #2,256(a4)
        bra.s   clant
; ---------------------------------------------------------------------------

ant4:
        bchg    #3,256(a4)
        bra.s   clant
; ---------------------------------------------------------------------------

ant5:
        bchg    #4,256(a4)
        bra.s   clant
; ---------------------------------------------------------------------------

ant6:
        bchg    #5,256(a4)
        bra.s   clant
; ---------------------------------------------------------------------------

ant7:
        bchg    #6,256(a4)
        bra.s   clant
; ---------------------------------------------------------------------------

ant8:
        bchg    #7,256(a4)
        bra.s   clant

; *******************************************************************
; x_one
; *******************************************************************
x_one:
        lea     pad_now,a1 ; Get the most recent button presses.
        move.b  1(a1),d0
        rol.b   #2,d0
        lea     ixcon,a0
        jsr     inertcon
        bra.w   spn_butt

; *******************************************************************
; xy1
; *******************************************************************
xy1:
        lea     pad_now,a1 ; Get the most recent button presses.
        move.b  1(a1),d0
        rol.b   #2,d0
        lea     ixcon,a0
        jsr     inertcon
        move.b  1(a1),d0
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
        lea     pad_now,a1 ; Get the most recent button presses.
        move.l  (a1),d0
        move.l  #bbutton,d1 ; Put b button in d1.
        and.l   d0,d1 ; Was the b button pressed?
        bne.w   bbexit
        move.l  #abutton,d1 ; Put a button in d1.
        and.l   d0,d1 ; Was the a button pressed?
        bne.w   prevexit
        and.l   #cbutton,d0 ; Was the c button pressed?
        beq.w   rrts
        move    selected,d0
        add.w   #1,d0
        cmp.w   selectable,d0
        ble.w   slecset
        clr.w   d0

slecset:
        move    d0,selected
        bra.w   sted

prevexit:
        move    selected,d0
        sub.w   #1,d0
        bpl.s   slecset
        move    selectable,d0
        bra.s   slecset
buttex:
        move.l  (a1),d0
        and.l   #allbutts,d0; A, B, or C pressed?
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
        move.l  pad_now,d0 ; Get the most recent button presses.
        move.l  d0,d1
        and.l   #up,d0 ; Did the player press up?
        bne.w   sninc
        move.l  d1,d0
        and.l   #down,d0 ; Did the player press down?
        bne.w   sndec
        move.l  d1,d0
        and.l   #left,d0 ; Did the player press left?
        bne.w   spdec
        move.l  d1,d0
        and.l   #right,d0 ; Did the player press right?
        bne.w   spinc
        move.l  d1,d0
        and.l   #bbutton,d0 ; Did the player press b?
        beq.w   padex
        move.l  pad_now,d0 ; Get the most recent button presses.
        move.l  d0,d1
        and.l   #up,d0 ; Did the player press up?
        bsr.w   selup
        bra.w   gnek
        move.l  d1,d0
        and.l   #down,d0 ; Did the player press down?
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
        move    #1,seldb ; Set the selection debounce flag.
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
        move.l  pad_now,d0 ; Get the most recent button presses.
        move.l  d0,d1
        and.l   #up,d0 ; Did the player pressup?
        bne.w   selup
        move.l  d1,d0
        and.l   #down,d0 ; Did the player press down?
        bne.w   seldn
        movea.l aded,a0
        move    selected,d2
        lsl     #1,d2 ; Multiply d2 by 2.
        lea     (a0,d2),a0
        move.l  d1,d0
        and.l   #left,d0 ; Did the player press left?
        bne.w   intdec
        move.l  d1,d0
        and.l   #right,d0 ; Did the player press right?
        bne.w   intinc

; *******************************************************************
; padex
; Check if we should exit the current screen (user has pressed a, b, or c).
; *******************************************************************
padex:
        move.l  pad_now,d1 ; Get the most recent button presses.
        and.l   #(abutton|bbutton|cbutton),d1 ; Did the player press a,b, or c?
        bne.w   owwt ; If so, exit the screen.
        rts ; Otherwise do nothing.

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
        move.l  pad_now,d0 ; Get the most recent button presses.
        move.l  d0,d1
        and.l   #up,d0
        bne.w   xselup
        move.l  d1,d0
        and.l   #down,d0
        bne.w   xseldn
        move.l  d1,d0
        and.l   #left,d0
        bne.w   ph_dec
        move.l  d1,d0
        and.l   #right,d0
        bne.w   ph_inc
        lea     wfpad,a0
        lea     pad_now,a1 ; Get the most recent button presses.
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
        lsr     #1,d6
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
        lsr     #1,d6
        dbf     d7,_gk2
        move.l  pad_now,d1 ; Get the most recent button presses.
        and.l   #(abutton|bbutton|cbutton),d1 ; Did the player press a,b, or c?
        beq.w   rrts
        move    selected,d2
        movea.l edwave,a0
        lsl     #4,d2 ; Multiply d2 by 16.
        lea     4(a0,d2),a0
        cmp.l   #bbutton,d1
        bne.w   zinc

; *******************************************************************
; owwt
; Exit the current screen.
; *******************************************************************
owwt:
        move    #SELECTOR,editing ; Set selector as the editing routine.
        clr.w   symed
        bra.w   onstak
        ; Returns
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
        lsl     #4,d2 ; Multiply d2 by 16.
        lea     (a0,d2),a0
        rts

; *******************************************************************
; selector
; Process user input in edit mode where the screen is displaying
; "<A> Edit   <B> Edit   <C> Back" and the ability to move up or down
; to change selection.
; *******************************************************************
selector:
        move.l  pad_now,d0 ; Get the most recent button presses.
        move.l  d0,d1
        and.l   #up,d0 ; Up selected?
        bne.w   selup ; Move up.
        move.l  d1,d0
        and.l   #down,d0 ; Down selected?
        bne.w   seldn ; Move down?
        and.l   #(abutton|bbutton|cbutton),d1 ; A,B, or C pressed?
        beq.w   rrts ; None of them pressed.
        and.l   #cbutton,d1 ; C pressed?
        beq.w   seuss ; If not, select the associated edit action for the current option.
        tst.w   cbuttf
        beq.w   onstak

soxx:   move.l  #reedit,action
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
; Execute the associated action with the current selected option.
; *******************************************************************
seuss:
        move    selected,d0

; *******************************************************************
; sted
; THe user has selected an option.
; Activate the routine associated with the selected action, e.g.
; ispec2 when op71 (Trigger 1) has been selected in the 'Spectrum
; and Triggers' screen (option7).
; *******************************************************************
sted:
        lsl     #3,d0 ; Multiply d0 by 8.
        movea.l editlist,a0 ; Point a0 at the current edit list.
        move.l  $0C(a0,d0.w),action ; Store the associated action routine in action.
        clr.w   editing ; Clear the current editing pointer.
        bra.w   sdb ; Signal a selection has been made.

; *******************************************************************
; selup
; Move the cursor up and show the new item as selected.
; *******************************************************************
selup:
        subi.w  #1,selected
        bpl.w   sud
        move    selectable,selected
sud:    move.l  #ud_selcu,action

; *******************************************************************
; sdb
; Signal a selection has been made.
; *******************************************************************
sdb:
        move    #1,seldb ; Set the selection debounce flag.
        rts

; *******************************************************************
; seldn
; User has pressed 'Down'. 
; Checks if they have cycled through the screen eight times yet
; and 'Edit Mode' should be displayed!
; *******************************************************************
seldn:
        addi.w  #1,selected
        move    selectable,d0
        cmp.w   selected,d0
        bpl.s   sud
        clr.w   selected
        tst.w   sec_en ; Have we reached the bottom of selectable options yet?
        beq.s   sud

        ; Check if we can display 'Edit Mode' yet!
        addi.w  #1,sec_cnt
        cmpi.w  #8,sec_cnt ; Have we cycled through the menu 8 times yet?
        blt.s   sud ; If not, bail.
        clr.w   sec_cnt ; Reset the count.
        move.l  #old_edit,action ; Enter the 'Edit Mode' screen.
        bra.s   sdb

; *******************************************************************
; InitBeasties
; Initialize the Object List. We make it 12 items long. The main object
; is the first one, which is a full screen of pixel data generated by the
; GPU. Each entry is 64 bytes long - though we only use 32?
;
; Data structure of items in 'beasties':
; Bytes 0-3      X
; Bytes 4-7      Y 
; Bytes 8-9      Width
; Bytes 10-11    Height
; Bytes 12-13    Object Mode - Index to ModeVex
; Bytes 14-15    Object Type - see ObTypes
; Bytes 16-20    Pointer to screen data.
; Bytes 20-22    Index to post-creation routine in 'postfixups', e.g. make_transparent, make_rmw.
; Bytes 22-24    
; Bytes 24-26    - Width of Obj from ObTypes
; Bytes 26-28    - Height of Obj from ObTypes
; Bytes 28-30    - Depth of Obj from ObTypes
; Bytes 30-32    - 
; *******************************************************************
InitBeasties:
        lea     beasties,a0    ; Point a0 at the first entry in beasties.
        move    #12,d7         ; Move 12 to d7.
        move    d7,nbeastie    ; There will be 12 entries in beasties.
ibeasts: 
        move    #-1,OBJ_MODE(a0) ; Initialize the mode of the entry to 'inactive'
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

OBJ_TYPE EQU 14
OBJ_MODE EQU 12
OBJ_Y EQU 4
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
; 10-11    Transparency?
; 12-13    Object Mode
; 14-15    Object Type
; 16-19    Pointer to screen data.
; 20-21    Index to post-creation routine in 'postfixupsps'.
; 22-23
; 24-25    Width of Obj from ObTypes
; 26-27    Height of Obj from ObTypes
; 28-29    Depth of Obj from ObTypes
; 30-31    Unused?
; 32-33    Something to do with edit mode?
; 33-34    Unused?
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
        tst.w   scron          ; Are we strobing?
        beq.w   RBeasts        ; If not, skip to intialising from the start.
        bmi.w   RBeasts        ; If not, skip to intialising from the start.

        ; If we're strobing we have one less Beastie to worry about. 
        sub.w   #1,d7          ; Need to initialize one less.
        lea     64(a2),a2      ; Move the pointer to the second entry in the beasties list.
        
        ; Loop through beasties and create objects for each entry in
        ; blist. The 'mode' in each object will determine which ModeVex
        ; routine to call and that will create the object and add it to
        ; blist. Our pointer to blist in this operation is a0.
RBeasts:move    d7,-(sp)       ; Stash some values in the stack so we can restore them later.
        move    OBJ_MODE(a2),d0      ; Get the object's 'Mode'.
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
        bset    #6,10(a3)
        bset    #7,10(a3)

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
        bset    #7,10(a3)
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
; a2 -> pointer to object in beasties list
; *******************************************************************
stoptop:
        move    #$B4,d3
        sub.w   palside,d3
        move    #$1A4,d4
        add.w   paltop,d4
        sub.w   #$B0,d4

        tst.w   vlm_mode ; Are we in audio reactive mode?
        beq.w   clip1 ; If so, skip to clip1.

        ; Update the x position using cursor?
        move    pixcon,d0
        sub.w   #127,d0
        add.w   d3,d0
        move    d0,(a2)

        ; Update the y position using cursor?
        move    piycon,d0
        sub.w   #127,d0
        lsl     #1,d0 ; Multiply d0 by 2.
        add.w   d4,d0
        move    d0,OBJ_Y(a2)

        bmi.w   rrts
        bra.w   clip1

; *******************************************************************
; clip0
; a2 -> pointer to object in beasties list
; *******************************************************************
clip0:
        move    skale,d0
        beq.w   clip00
        add.w   #2,d0
        move    d0,OBJ_TYPE(a2)
        move    #1,bo
        bra.w   clip2

clip00:
        clr.w   OBJ_TYPE(a2)
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
        cmpi.w  #5,OBJ_TYPE(a2)
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
        lsr     #1,d6

ponscr:
        bclr    #0,d1
        swap    d6
        move    8(a2),d6
        move    10(a2),d3
        lsl     #8,d3
        or.w    d6,d3
        swap    d3
        swap    d6
        move    OBJ_TYPE(a2),d7
        lea     ObTypes,a3
        asl.w   #3,d7
        move    $18(a2),d3
        move    $1A(a2),d4
        move    $1C(a2),d5
        move    $16(a2),d2
        movea.l $10(a2),a1
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
        lsr     d3,d6

sponscr:
        bclr    #0,d1
        swap    d6
        move    8(a2),d6
        move    $A(a2),d3
        lsl     #8,d3
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
        lsl     #3,d1 ; Multiply d1 by 8.
        lsl     #3,d2 ; Multiply d2 by 8.
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
        dbf     d0,sl ; Keep looping until we've done 16 stop objects.
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
        lsr     d7,d6
        lsl     #2,d6 ; Multiply d6 by 4.
        move    d6,d7
        lsl     #1,d7 ; Multiply d7 by 2.
        and.l   #$FFFF,d7
        move.l  d7,dbadj
        adda.l  d7,a1
        move    skale,d7
        lsl     d7,d6
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
        lsl     #3,d6 ; Multiply d6 by 8.
        or.w    d7,d6
        bset    #0,d6
        move    d6,(a0)+
        clr.w   (a0)+
        move    d2,d7
        lsl     #7,d7
        moveq   #0,d6
        move    d3,d6
        move    skale,d0
        add.w   bo,d0
        lsr     d0,d6
        ror.l   #4,d6
        bclr    #$F,d6
        or.w    d7,d6
        move    d6,(a0)+
        move    (sp)+,d0
        and.w   #$FFF,d0
        move    d3,d7
        lsl     #2,d7 ; Multiply d7 by 4.
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
        lea     -32(a0),a3
        bset    #0,9(a3)
        movem.l (sp)+,d0-d5/a1
        move    d3,d6
        move    skale,d7
        add.w   #1,d7
        lsr     d7,d6
        lsl     #2,d6 ; Multiply d6 by 4.
        move    d6,d7
        lsl     #1,d7 ; Multiply d7 by 2.
        and.l   #$FFFF,d7
        move.l  d7,dbadj
        adda.l  d7,a1
        move    skale,d7
        lsl     d7,d6
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
        lea     32(a0),a0

mumuso:
        move.l  a1,d6
        and.l   #$FFFFFFF8,d6
        lsl.l   #8,d6
        move.l  d6,(a0)+
        lea     32(a4),a6
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
        lsl     #3,d6 ; Multiply d6 by 8.
        or.w    d7,d6
        move    d6,(a0)+
        clr.w   (a0)+
        move    d2,d7
        lsl     #7,d7
        moveq   #0,d6
        move    d3,d6
        tst.w   bo
        beq.w   snoke
        move    d0,-(sp) ; Stash some values in the stack so we can restore them later.
        move    skale,d0
        add.w   bo,d0
        lsr     d0,d6
        move    (sp)+,d0

snoke:
        and.w   #$FFF,d0
        ror.l   #4,d6
        bclr    #$F,d6
        or.w    d7,d6
        move    d6,(a0)+
        move    d3,d7
        lsl     #2,d7 ; Multiply d7 by 4.
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
; Draw the rotary dial of numbers in the symmetry selection screen.
; For example:
;
;      1  2
;  8         3
;        9        ------ Rotational symmetry
;  7         4
;     6   5
;
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
        move.b  1(a3),d1
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
        lsl     #1,d2 ; Multiply d2 by 2.
        lea     myFont,a1 ; Point a1 at the font info.
        move    4(a1,d2),d2 ; Use d2 as an index into the first 140 bytes and store in d2.
        lea     (a1,d2),a2 ; Now use the value we retrieved as index into the address list in myFont.
        move.l  a2,A2_BASE
        move.l  #0,A2_PIXEL
        move.l  #$1FFF8,A2_STEP
        move.l  #$14200,A1_FLAGS
        move.l  #board,A1_BASE
        lsl     #3,d0 ; Multiply d0 by 8.
        move    d1,d2
        lsl     #3,d1 ; Multiply d1 by 8.
        add.w   d2,d1
        swap    d1
        move    d0,d1
        move.l  d1,A1_PIXEL
        move.l  #$1FFF8,A1_STEP
        move.l  #$80008,B_COUNT
        move.l  #0,B_PATD
        move.l  #0,B_PATD+4
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
        lsr     d7,d0
        lsr     d7,d1
        lsr     d7,d2
        bne.w   slogg
        move    #1,d2

slogg:
        lsr     d7,d3
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

        move.l  #$34420,d7
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
; Get the number that was pressed and store the result in d2.
; d0 -> the button code that was pressed.
; *******************************************************************
dcode:
        lea     codez,a3   ; Point a3 at the codez array below.
        
        ; Find a hit for d0 in the codez array.
        move    #31,d1     ; Loop through all 32 entries in codez.
dco:    move.b  (a3)+,d2   ; Put current entry in codez in d2 and advance one position.
        btst    d1,d0      ; Have we reached the code selected by d0?
        bne.w   rrts       ; If so, d2 is now the value we want: return.
        dbf     d1,dco     ; Otherwise, keep looping.
        
        clr.w   d2         ; We didn't get a hit - so clear d2.
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
;
; Populates pad_now with input from the controller.
;
; The end result is a 4-byte long with the bits set as follows for
; each possible input. For example if 9 was pressed, you can see that
; bit 2 would be set:
;   xxAPxxBx RLDU741* xxCxxxOx 2580369#
;
; This means we can set equates to recognize button pressed as follows:
;        pausebutton  EQU $10000000
;        abutton      EQU $20000000
;        bbutton      EQU $02000000
;        right        EQU $00800000
;        left         EQU $00400000
;        down         EQU $00200000
;        up           EQU $00100000
;        seven        EQU $00080000
;        four         EQU $00040000
;        one          EQU $00020000
;        asterisk     EQU $00010000
;        cbutton      EQU $00002000
;        optionbutton EQU $00000200
;        two          EQU $00000080
;        five         EQU $00000040
;        eight        EQU $00000020
;        zerobutton   EQU $00000010
;        three        EQU $00000008
;        six          EQU $00000004
;        nine         EQU $00000002
;        hash         EQU $00000001
; *******************************************************************
readpad:
        movem.l d0-d2,-(sp) ; Stash some values in the stack so we can restore them later.

        ;scan for player 1
        move.l  #$F0FFFFFC,d1      ; d1 = Joypad data mask
        moveq   #-1,d2             ; d2 = Cumulative joypad reading
                                                                                                         
        move    #$81FE,JOYSTICK                                                                          
        move.l  JOYSTICK,d0        ; Read joypad, pause button, A button
        or.l    d1,d0              ; Mask off unused bits
        ror.l   #4,d0                                                                                    
        and.l   d0,d2              ; d2 = xxAPxxxx RLDUxxxx xxxxxxxx xxxxxxxx
        move    #$81FD,JOYSTICK                                                                          
        move.l  JOYSTICK,d0        ; Read *741 keys, B button
        or.l    d1,d0              ; Mask off unused bits
        ror.l   #8,d0                                                                                    
        and.l   d0,d2              ; d2 = xxAPxxBx RLDU741* xxxxxxxx xxxxxxxx
        move    #$81FB,JOYSTICK                                                                          
        move.l  JOYSTICK,d0        ; Read 2580 keys, C button
        or.l    d1,d0              ; Mask off unused bits
        rol.l   #6,d0                                                                                    
        rol.l   #6,d0                                                                                    
        and.l   d0,d2              ; d2 = xxAPxxBx RLDU741* xxCxxxxx 2580xxxx
        move    #$81F7,JOYSTICK                                                                          
        move.l  JOYSTICK,d0        ; Read 369# keys, Option button
        or.l    d1,d0              ; Mask off unused bits
        rol.l   #8,d0                                                                                    
        and.l   d0,d2              ; d2 = xxAPxxBx RLDU741* xxCxxxOx 2580369# <== inputs active low
                                                                                                         
        moveq   #-1,d1                                                                                   
        eor.l   d2,d1              ; d1 = xxAPxxBx RLDU741* xxCxxxOx 2580369# <== now inputs active high
                                                                                                         
        move.l  pad_now,d0         ; old joycur needed for determining the new joyedge
        move.l  d1,pad_now         ; Current joypad reading stored into joycur
        eor.l   d1,d0                                                                                    
        and.l   d1,d0                                                                                    
        move.l  d0,pad_shot        ;joypad, buttons, keys that were just pressed

        ;scan for player 2
        move.l  #$FFFFFF3,d1         ; d1 = Joypad data mask
        moveq   #-1,d2        ; d2 = Cumulative joypad reading
                                                                                                           
        move    #$817F,JOYSTICK                                                                            
        move.l  JOYSTICK,d0          ; Read joypad, pause button, A button
        or.l    d1,d0                ; Mask off unused bits
        rol.b   #2,d0                ; note the size of rol
        ror.l   #8,d0                                                                                      
        and.l   d0,d2                ; d2 = xxAPxxxx RLDUxxxx xxxxxxxx xxxxxxxx
        move    #$81BF,JOYSTICK                                                                            
        move.l  JOYSTICK,d0          ; Read *741 keys, B button
        or.l    d1,d0                ; Mask off unused bits
        rol.b   #2,d0                ; note the size of rol
        ror.l   #8,d0                                                                                      
        ror.l   #4,d0                                                                                      
        and.l   d0,d2                ; d2 = xxAPxxBx RLDU741* xxxxxxxx xxxxxxxx
        move    #$81DF,JOYSTICK                                                                            
        move.l  JOYSTICK,d0          ; Read 2580 keys, C button
        or.l    d1,d0                ; Mask off unused bits
        rol.b   #2,d0                ; note the size of rol
        rol.l   #8,d0                                                                                      
        and.l   d0,d2                ; d2 = xxAPxxBx RLDU741* xxCxxxxx 2580xxxx
        move    #$81EF,JOYSTICK                                                                            
        move.l  JOYSTICK,d0          ; Read 369# keys, Option button
        or.l    d1,d0                ; Mask off unused bits
        rol.b   #2,d0                ; note the size of rol
        rol.l   #4,d0                                                                                      
        and.l   d0,d2                ; d2 = xxAPxxBx RLDU741* xxCxxxOx 2580369# <== inputs active low
                                                                                                           
        moveq   #-1,d1                                                                                     
        eor.l   d2,d1                ; d1 = xxAPxxBx RLDU741* xxCxxxOx 2580369# <== now inputs active high
                                                                                                           
        move.l  pad_now+4,d0         ; old joycur needed for determining the new joyedge
        move.l  d1,pad_now+4         ; Current joypad reading stored into joycur
        eor.l   d1,d0                                                                                      
        and.l   d1,d0                                                                                      
        move.l  d0,pad_shot+4        ;joypad, buttons, keys that were just pressed

        movem.l (sp)+,d0-d2
        rts

; *******************************************************************
; initobject
; Unused.
; *******************************************************************
initobject:
        move.l  #-1,activeob
        lea     objects,a0
        move    #$3F,d0 ; '?'
        move    d0,ofree
        move.l  a0,freeobje
        movea.l #-1,a1

IniA:
        move.l  a1,(a0)
        movea.l a0,a1
        lea     64(a0),a0
        move.l  a0,4(a1)
        dbf     d0,IniA
        move.l  #-1,4(a1)
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
        cmpa.l  #-1,a1
        bne.w   ML1
        move.l  a2,(a4)
        cmpa.l  a1,a2
        beq.w   NewLink

ML1:
        cmpa.l  #-1,a2
        bne.w   ML2
        move.l  a2,4(a1)
        bra.w   NewLink
; ---------------------------------------------------------------------------

ML2:
        cmpa.l  #-1,a1
        beq.w   ml3
        move.l  a2,4(a1)

ml3:
        move.l  a1,(a2)

; *******************************************************************
; NewLink
; *******************************************************************
NewLink:
        movea.l (a3),a4
        cmpa.l  #-1,a4
        bne.w   NL1
        move.l  a0,(a3)
        move.l  #-1,(a0)
        move.l  #-1,4(a0)
        clr.w   d0
        rts

NL1:
        move.l  #-1,(a0)
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
        ; Returns

; *******************************************************************
; inertcon
; a0 -> pixcon/piycon
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
        lsl     #8,d3
        move.b  d2,d3
        move    d3,(a2)+
        move.b  #$FF,d5

dblo3:
        lea     1(a0),a0
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
;
; Load a bank with 9 effects. 'matrix' is where we store the bank of 9 effects.
;
; Each of the 9 'effects' in the 'bank' consists of 6 sub-effect 'fx objects'
; (see ifxobj). At launch we created a set of 6 barebones sub-effect fx objects
; and stored them in refblock. This acts as our base layer for the first
; effect. Loading a bank of 9 effect to 'matrix' (which is where the active
; bank is stored) entails the following steps:
;
;  - Given the selected bank, figure out the location in 'av1' (see banks.s)
;  that the bank data starts at.
;
;  - Copy refblock to matrix so that it has the base layer for all 6 fx objects
;  for the first effect.
;
;  - Read in the selected bank's data for the first effect from av1. This data
;  consists of byte pairs such as ($01,$12). Each byte pair is like an
;  instruction. In this example $01 means advance 1 position in 'matrix' and
;  store $12 in the new position. We continue doing that until we've advanced
;  6144 positions, i.e.  the full length of the data for the first effect (6 fx
;  objects, each 1024 bytes long).
;
;  - Loop through the rest of av1 for the remaining 8 effects. For each effect,
;  use the previous effect as a base layer, then read through av1 plugging in
;  the data from the byte pairs read in from av1 using the same method as the
;  step above.
;
; So as you can see each bank's data in av1 is 'compressed' - it doesn't
; contain the whole 6144 bytes of data for each of the 9 effects - just the
; bytes that are different from the default fx objects we initialized and
; stored in 'refblock' when the game was launched. Reading in the data from av1
; is a case of filling in the parts of we got from refblock that are different
; from the default values.
;
; The high level structure of each 1024 byte sub-effect object is:
;   Bytes 0   - 256  (64 4-byte longs): Parameters for the object. 
;   Bytes 256 - 512  (64 4-byte longs): Some kind of buffer? 
;   Bytes 512 - 768  (64 4-byte longs): Some kind of buffer? 
;   Bytes 768 - 896  (64 4-byte longs): The contents of oscbank. 
;   Bytes 896 - 1004 (64 4-byte longs): The contents of 'pixcon' to 'delayn'. 
;
; See also banks.s for more detail and the uncompressed contents of all banks!
; *******************************************************************
gm:
        ; Clear the matrix RAM.
        lea     matrix,a1       ; Point a1 at matrix, where we'll store the full bank.
        move    #13823,d0       ; Clear the 6144 * 9 bytes required to store the full bank in matrix.
ivtb:   move.l  #-1,(a1)+       ; Clear a 4-byte phrase.
        dbf     d0,ivtb         ; Keep going until we've done all (13824 * 4) bytes.
        
        ; - Given the selected bank, figure out the location in 'av1' that the
        ; bank starts at.
        ; Point a0 at the position in av1 given by the bank number in imatrix.
        movea.l av1ref,a0
        move.l  a0,d1
        lea     16(a0),a0 ; Move a0 past the unused offsets.
        move    imatrix,d0      ; Get the bank number
        lsl     #2,d0           ; Multiply d0 by 4.
        move.l  (a0,d0.w),d0
        sub.l   #$00900000,d0   ; Subtract our the first two bytes to get our offset into av1.
        add.l   d1,d0           ; Add the offset + av1 to get the address of the selected bank.
        movea.l d0,a0           ; Store the address of the selected bank in a0.
        
        ; - Copy refblock to matrix so that it has the base layer for all 6 fx
        ; objects.  Copy the contents of refblock (a0) to matrix (a1).
        ; Remember that refblock contains six clean, initialized fx objects.
        lea     matrix,a1
        movem.l a0-a1,-(sp)     ; Stash some values in the stack so we can restore them later.
        lea     refblock,a0
        move.l  #6144,d0        ; Copy 6144 bytes.
        bsr.w   blitcopy
        movem.l (sp)+,a0-a1     ; a0 is now position of selected bank in av1 again, and a1 is matrix.
        
        ; - Read in the effect's data from av1. This data consists of byte
        ; pairs such as ($01,$12). Each byte pair is like an instruction. In this
        ; example $01 means advance 1 position in 'matrix' and store $12 in the new
        ; position. We continue doing that until we've advanced 6144 positions, i.e.
        ; the full length of the matrix data (6 fx objects, each 1024 bytes long).
        clr.w   d0              ; Set counter to 0
        clr.w   d1              ; Set steps to advance to 0
unp:    move.b  (a0)+,d1        ; First byte: steps to advance.
        move.b  (a0)+,d2        ; Second byte: value to write to new position.
        cmp.w   #6144,d0        ; Have we finished the 6144 bytes of the bank yet?
        bpl.w   taiga           ; If yes, exit loop.
        add.w   d1,d0           ; Advance the no. of steps in the first byte.
        cmp.w   #6144,d0        ; Have we finished the 6144 bytes of the bank yet?
        bpl.w   taiga           ; If yes, exit loop.
        move.b  d2,(a1,d0.w)    ; Write the second byte as the value at current position in matrix(a1).
        add.w   #1,d0           ; Advance at least 1 position.
        bra.s   unp             ; Keep looping.
        
        ; - Loop through the rest of av1 for the remaining 8 effects. For each
        ; effect, use the previous effect as a base layer, then read through
        ; av1 plugging in the data from the byte pairs read in from av1 using
        ; the same method as the step above.
taiga:  move    #7,d5           ; Loop through all 8 objects?
        ; a0 is the current position in av1 containing the effect info for the bank.
        ; a1 is the pointer to matrix where we will unpack it all to.
        ; The start of our outer loop.
rrest:  move.l  a0,-(sp)        ; Stash some values in the stack so we can restore them later.

        ; Copy the previous effect in the bank as a base layer.
        movea.l a1,a0           ; Point a0 at the previous effect.
        lea     6144(a1),a1     ; Point a1 at the position in matrix for the next effect.
        move.l  #6144,d0        ; Copy 6144 bytes.
        bsr.w   blitcopy        ; Copy the bytes.
        movea.l (sp)+,a0        ; Restore a0 to point to av1 again.
        
        ; Read in the effect's data using the same process as 'unp' above.
        lea     -2(a0),a0 ; Point back to the byte pair that we bailed on for the previous effect.
        clr.w   d0              ; Set counter to 0.
        clr.w   d1              ; Set steps to advance to 0
unpk2:  move.b  (a0)+,d1        ; First byte: steps to advance.
        move.b  (a0)+,d2        ; Second byte: value to write to new position.
        cmp.w   #6144,d0        ; Have we finished the 6144 bytes of the bank yet?
        bpl.w   taiga2          ; If yes, exit loop.
        add.w   d1,d0           ; Advance the no. of steps in the first byte.
        cmp.w   #6144,d0        ; Have we finished the 6144 bytes of the bank yet?
        bpl.w   taiga2          ; If yes, exit loop.
        move.b  d2,(a1,d0.w)    ; Write the second byte as the value at current position in matrix(a1).
        add.w   #1,d0           ; Advance at least 1 position.
        bra.s   unpk2           ; Keep looping.

taiga2: dbf     d5,rrest        ; Loop for all 8 effects.
        
        ; We've copied all 9 effects into 'matrix'. So now we can load the effect
        ; in the bank that was selected to 'fx'.
        bsr.w   skidoo          ; Load the selected effect.
        clr.w   bank_mod
        lea     banclr,a0       ; "~g1:$20:     "
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
; Copy a bunch of bytes from a0 to a1.
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
        move    #-1,d7
        move    #1,d0

icu:
        move    #1,d1

icu2:
        move    #1,d2

icu3:
        move    d2,d3
        lsl     #2,d3 ; Multiply d3 by 4.
        sub.w   #2,d3
        move    d3,(a0)+
        move    d1,d3
        lsl     #2,d3 ; Multiply d3 by 4.
        sub.w   #2,d3
        move    d3,(a0)+
        move    d0,d3
        lsl     #2,d3 ; Multiply d3 by 4.
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
gsphr:  move.b  (a1,d1.w),d2
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
        move.l  #-1,(a0)+
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
        moveq   #-1,d7
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
        lsl     #2,d3 ; Multiply d3 by 4.
        sub.w   #$10,d3
        swap    d3
        move.l  d3,(a0)+
        move    d1,d3
        lsl     #2,d3 ; Multiply d3 by 4.
        sub.w   #$10,d3
        swap    d3
        move.l  d3,(a0)+
        move    d0,d3
        lsl     #2,d3 ; Multiply d3 by 4.
        sub.w   #$10,d3
        swap    d3
        move.l  d3,(a0)+
        move    d0,d4
        lsl     #5,d4 ; Multiply d4 by 32.
        move    d1,d5
        lsl     #1,d5 ; Multiply d5 by 2.
        or.w    d5,d4
        lsl     #8,d4
        move    d4,(a0)+
        move    d2,d4
        lsl     #4,d4 ; Multiply d4 by 16.
        lsl     #8,d4
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
; - Width (in longs, so multiply by four)
; - Height
; - Depth: 0 = 1 bit per pixel, 2 = 4 bit per pixel, 4 = 16 bit per pixel
; - Offset to Colour Lookup Table (CLUT).
; *******************************************************************
ObTypes:         dc.w    96,  384,  4,    0  ; 0: 384x384, 16bit
                 dc.w     5,  240,  0,    0  ; 1: 20x240
                 dc.w     1,    8,  0,    0  ; 2: 4x8
                 dc.w    96,  192,  4,    0  ; 3: 384x192, 16 bit
                 dc.w    96,   96,  4,    0  ; 4: 384x384, 16 bit
                 dc.w     5,  112,  0,   16  ; 5: 20x112
                 dc.w    16,   48,  2,    4  ; 6: 64x48
                 dc.w     8,   10,  4,    0  ; 7: 32x10

; *******************************************************************
; sines
; A sine table
; *******************************************************************
.include "sines.s"

sympad:         dc.l   symclr
                dc.l   sp8,  sp7
                dc.l   sp1,  syminv
                dc.l   sp9,  sp4
                dc.l   sp6,  rrts
                dc.l   sp5,  sp3
                dc.l   sp2,  srcentre
                dc.l   dorsym1,  dorsym2
                dc.l   dorsym3
dorsymrts:      dc.l  rrts

                dc.l   bp7,  bp4
                dc.l   bp1,  rrts
                dc.l   rrts,  bp6
                dc.l   bp3,  rrts
                dc.l   bp8,  bp5
                dc.l   bp2
bpsymrts:       dc.l  rrts

                dc.l   ass7,  ass4
                dc.l   ass1,  rrts
                dc.l   ass9,  ass6
                dc.l   ass3,  rrts
                dc.l   ass8,  ass5
                dc.l   ass2,  rrts

                dc.l   ski7,  ski4
                dc.l   ski1,  rrts
                dc.l   ski9,  ski6
                dc.l   ski3,  rrts
                dc.l   ski8,  ski5
                dc.l   ski2,  rrts

                dc.l   gski7,  gski4
                dc.l   gski1,  rrts
                dc.l   gski9,  gski6
                dc.l   gski3,  rrts
                dc.l   gski8,  gski5
                dc.l   gski2
gskirts:        dc.l   rrts

                dc.l   lul7,  lul4
                dc.l   lul1,  setantelope
                dc.l   rrts,  lul6
                dc.l   lul3,  rrts
                dc.l   lul8,  lul5
                dc.l   lul2

syminvxdec:     dc.l  invxdec
                dc.l   invxinc,  invydec
                dc.l   invyinc

sympadrsmyd:    dc.l   rsymd
                dc.l   rsymi,  irscale
                dc.l   drscale

sympaddstep:    dc.l  dstep
                dc.l   istep,  dstepi
                dc.l   istepi

; *******************************************************************
; A table for seeding random numbers
; *******************************************************************
rantab:
                dc.b $DC,$A9,$B1
                dc.b $54,$F5,$4E,$55,$4A,$2C,$09,$65
                dc.b $A3,$C2,$F9,$F0,$FD,$A8,$BF,$49
                dc.b $EC,$0E,$F7,$3F,$D2,$82,$82,$09
                dc.b $39,$BD,$F7,$BC,$2D,$2A,$68,$44
                dc.b $11,$58,$22,$3B,$77,$56,$7E,$47
                dc.b $1A,$24,$3A,$FA,$79,$1A,$13,$DA
                dc.b $E8,$98,$5D,$6F,$3C,$E2,$E6,$2B
                dc.b $6C,$E0,$1E,$E9,$4F,$52,$BC,$12
                dc.b $ED,$5E,$EF,$6E,$99,$AD,$B4,$C4
                dc.b $45,$D9,$DA,$AE,$D2,$CE,$C3,$B7
                dc.b $9D,$B7,$E6,$72,$25,$A7,$CB,$37
                dc.b $4B,$78,$E5,$CF,$79,$BD,$6F,$0D
                dc.b $E4,$7B,$42,$30,$33,$36,$A2,$57
                dc.b $10,$07,$89,$64,$EF,$B1,$73,$B2
                dc.b $10,$91,$0B,$05,$0A,$5F,$74,$9D
                dc.b $31,$9E,$6A,$C5,$B0,$1E,$86,$3F
                dc.b $F6,$70,$17,$83,$61,$01,$35,$1C
                dc.b $9C,$98,$18,$EA,$C7,$1D,$93,$A0
                dc.b $43,$14,$AC,$62,$04,$69,$F6,$CD
                dc.b $F0,$FC,$99,$8B,$7A,$9C,$EC,$2D
                dc.b $35,$61,$14,$27,$7F,$CD,$38,$00
                dc.b  $E,$D0,$27,$A9,$2A,$4C,$94,$81
                dc.b $22,$32,$53,$4A,$AF,$C1,$77,$9F
                dc.b $FB,$7E,$A1,$D8,$13,$8F,$F2,$48
                dc.b $7C,$DB,$BE,$0C,$1F,$8D,$75,$D5
                dc.b  $D,$C9,$FA,$B9,$DF,$1B,$C6,$86
                dc.b $34,$85,$08,$59,$20,$FF,$66,$BB
                dc.b $E4,$36,$5C,$76,$76,$26,$4E,$93
                dc.b $8D,$6A,$68,$D6,$CF,$55,$A6,$7B
                dc.b $96,$DE,$B3,$18,$DD,$47,$15,$F1
                dc.b $23,$C7,$2E,$4F,$00,$6D,$92,$8A
                dc.b $20,$9E,$11,$08,$7A,$BA,$38,$87
                dc.b $C3,$A2,$4C,$81,$83,$50,$03,$D3
                dc.b $80,$54,$71,$87,$29,$60,$5A,$1D
                dc.b $A4,$C5,$EB,$2F,$60,$E0,$8A,$30
                dc.b $AC,$41,$F5,$63,$05,$37,$CA,$AA
                dc.b $CA,$1F,$3C,$88,$39,$69,$15,$F1
                dc.b $21,$E7,$5B,$6E,$B6,$DF,$B8,$5E
                dc.b $21,$0F,$1C,$51,$FB,$19,$D1,$BA
                dc.b $AB,$3E,$1B,$AF,$06,$49,$DD,$02
                dc.b $3A,$F8,$4D,$80,$46,$31,$90,$01
                dc.b $AD,$FD,$A4,$89,$3D,$04,$C9,$B6
                dc.b $3E,$5B,$DB,$D8,$9B,$C1,$BF,$74
                dc.b $90,$64,$84,$0B,$2B,$5F,$5C,$9F
                dc.b $FE,$91,$73,$E1,$07,$4D,$9A,$C2
                dc.b $71,$E2,$C8,$D0,$58,$6B,$BB,$63
                dc.b $A5,$E9,$AE,$A1,$B5,$84,$E5,$D7
                dc.b $2F,$6B,$E1,$9B,$F3,$75,$5A,$DC
                dc.b $56,$FE,$D6,$E3,$67,$A7,$8E,$50
                dc.b $CC,$C0,$D4,$43,$42,$A8,$92,$57
                dc.b $A6,$B4,$EA,$BE,$25,$B5,$28,$97
                dc.b $94,$EE,$40,$0C,$66,$7D,$16,$51
                dc.b $45,$B2,$FC,$F4,$D1,$95,$5D,$B0
                dc.b $52,$A3,$48,$E7,$96,$E3,$46,$8C
                dc.b $41,$D3,$B8,$CB,$A5,$C4,$A0,$B3
                dc.b $D7,$03,$EE,$95,$59,$F2,$33,$0F
                dc.b $53,$70,$CC,$8E,$3D,$8B,$EB,$DE
                dc.b $6D,$C8,$88,$6C,$ED,$7D,$28,$40
                dc.b $7F,$7C,$19,$85,$24,$32,$E8,$2C
                dc.b $8C,$67,$3B,$17,$12,$8F,$4B,$F3
                dc.b $78,$D4,$D5,$29,$C6,$9A,$AA,$34
                dc.b $23,$C0,$97,$CE,$26,$65,$0A,$16
                dc.b $F9,$D9,$44,$F8,$AB,$02,$62,$B9
                dc.b $06,$2E,$F4,$FF,$72,$00,$00,$00
                dc.b $00

av1ref:         dc.l av1 ; $19B0FB
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

clearhom:       dc.b '@~g1:1:',0
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
nothing:        dc.b 'Nothing',0
polyo:          dc.b 'Poly object',0
dvfarepc:       dc.b 'DVF area plus clear',0
dvfareanc:      dc.b 'DVF area no clear',0
ringpix:        dc.b 'Ring of pixels',0
star3d:         dc.b '3D starfield',0
wavesurf:       dc.b 'WaveSurf',0
psybitmap:      dc.b 'Psychedelic Bitmap',0
iripbitmap:     dc.b 'I-Ripple Bitmap',0
dvfscale2:      dc.b 'DVF --> scale2',0
dvfscale4:      dc.b 'DVF --> scale4',0
colplasma1:     dc.b 'Colour plasma 1',0
colplasma2:     dc.b 'Colour plasma 2',0
particleob:     dc.b 'Particle Object',0
particlemo:     dc.b 'Particle Motion',0
monoparticle:   dc.b 'Mono particle object',0
matrixob:       dc.b 'Matrix',0
specintens:     dc.b 'Spectrum as intensities',0
jaglogolit:     dc.b 'Jaguar Logo',0

dlo1:           dc.b 'Off',0
dlo2:           dc.b 'channel 1',0
dlo3:           dc.b 'channel 2',0
dlo4:           dc.b 'channel 3',0
dlo5:           dc.b 'channel 4',0
dlo6:           dc.b 'channel 5',0
dlo7:           dc.b 'channel 6',0
dlo8:           dc.b 'FX page 1',0
dlo9:           dc.b 'FX page 2',0
        .even
fxopt:          dc.l $00010000
                dc.l bline2
                dc.l dlo8, setp1 ; FX Page 1
                dc.l dlo9, setp2 ; FX Page 2

; *******************************************************************
; availobj
; The FX object to display on FX Page 1
; Each entry is the text literal followed by the routine that will
; create the selected object.
; *******************************************************************
availobj:       dc.l $00100000
                dc.l bline2
                dc.l nothing, makeno         ; Make a null object
                dc.l polyo, makepoly         ; 'Poly object'
                dc.l dvfarepc, makecfb       ; 'DVF area plus clear'
                dc.l dvfareanc, makefb       ; 'DVF area no clear'
                dc.l ringpix, makering       ; 'Ring of pixels'
                dc.l star3d, makestar        ; '3D starfield'
                dc.l wavesurf, amkesurf      ; 'WaveSurf'
                dc.l psybitmap, makemono     ; 'Psychedelic Bitmap'
                dc.l iripbitmap, makeiri     ; 'I-Ripple Bitmap'
                dc.l dvfscale2, mdvf2        ; 'DVF --> scale2'
                dc.l dvfscale4, mdvf4        ; 'DVF --> scale4'
                dc.l colplasma1, mplaz1      ; 'Colour plasma 1'
                dc.l colplasma2, mplaz2      ; 'Colour plasma 2'
                dc.l jaglogolit, mjaglogo    ;  Jaguar Logo
                dc.l particleob, mpo         ; 'Particle Object'
                dc.l particlemo, ipm         ; 'Particle Motion'
                dc.l matrixob, itau          ; Matrix
                dc.l monoparticle, mopo      ;Mono Particle object 
                                               
; *******************************************************************
; avail2
; The FX objects to display on FX Page 2
; Each entry is the text literal followed by the routine that will
; create the selected object.
; *******************************************************************
avail2:         dc.l $00010000
                dc.l bline2
                dc.l matrixob, itau ; Matrix Object
                dc.l specintens ; Spectrum as intensities.
                dc.w $0019    ; makeshun

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

sinewave:       dc.b 'Sine wave          ',0
sawtoothwave:   dc.b 'Sawtooth wave      ',0
squarewave:     dc.b 'Square wave        ',0
ramp:           dc.b 'Ramp               ',0
rectsinewave:   dc.b 'Rectified sine wave',0
noise:          dc.b 'Noise              ',0
constant:       dc.b 'Constant           ',0
usercontroly:   dc.b 'User control Y     ',0
usercontrolx:   dc.b 'User control X     ',0

wt:             dc.b '~g2:12:',0
                dc.b 0
wts:            
                dc.l sinewave
                dc.l sawtoothwave
                dc.l squarewave
                dc.l ramp
                dc.l rectsinewave
                dc.l noise
                dc.l constant
                dc.l usercontroly
                dc.l usercontrolx

sineslookup:    dc.l p_sines
                dc.l p_saw
                dc.l p_square
                dc.l p_ramp
                dc.l p_rect
                dc.l rantab
                dc.l p_sines
                dc.l p_sines
                dc.l p_sines

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
cmask4:         dc.l $FF000000, 0

; *******************************************************************
; pbinfo/editinginfo
;
; This array contains the detail of each parameter that can be edited in a
; subeffect object (fxobj).
; *******************************************************************
pbinfo:         dc.b 'Parameter not yet defined    ',0
editinginfo:    dc.w 0
                dcb.l 2,0

                dc.b 'DVF window size: X           ',0  ; 1
                dc.b $01 
                dc.b $02                                 ; Index in this list of complementary 'Y' entry to use when displaying editing tool.
                                                         ; So in this case '2' means get the start/end values from 'DVF window size: Y' too.
                dc.b $04                                 ; Index to edit routine variable in edvex: sidbl and editvex: xy1. (Index + 1)
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0001                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                                                         ; So for example '2' would refer to byte 8 in the fx object.
                dc.l $1800000
        
                dc.b 'DVF window size: Y           ',0
                dc.b $00
                dc.b $00                                 ; Index in this list of complementary 'Y' entry to use when displaying editing tool, 0 means none.
                dc.b $04                                 ; Index to edit routine variable in edvex: sidbl and editvex: xy1. (Index + 1)
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0002                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $1800000
        
                dc.b 'DVF scale: X                 ',0   ; 3
                dc.b $01
                dc.b $04                                 ; Index in this list of complementary 'Y' entry to use when displaying editing tool.
                dc.b $04                                 ; Index to edit routine variable in edvex: sidbl and editvex: xy1. (Index + 1)
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0003                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $03FFFFFF
        
                dc.b 'DVF scale: Y                 ',0
                dc.b $00
                dc.b $00
                dc.b $04                                 ; Index to edit routine variable in edvex: sidbl and editvex: xy1. (Index + 1)
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0004                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $03FFFFFF
        
                dc.b 'DVF rotate angle             ',0   ; 5
                dc.b $00
                dc.b $00
                dc.b $02                                 ; Index to edit routine variable in edvex: snglx and editvex: x_one (i.e. index + 1).
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0005                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $01FFFFFF
        
                dc.b 'DVF centre of rotation: X    ',0   ; 5
                dc.b $01
                dc.b $07                                 ; Index in this list of complementary 'Y' entry to use when displaying editing tool.
                dc.b $01                                 ; Index to edit routine variable in edvex: crot and editvex: x_one (i.e. index + 1).
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0006                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $01800000
        
                dc.b 'DVF centre of rotation: Y    ',0
                dc.b $00
                dc.b $00
                dc.b $01                                 ; Index to edit routine variable in edvex: crot and editvex: x_one (i.e. index + 1).
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0007                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $01800000
        
                dc.b 'DVF Delta Intensity          ',0   ; 8
                dc.b $00
                dc.b $00
                dc.b $02                                 ; Index to edit routine variable in edvex: snglx and editvex: x_one (i.e. index + 1).
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0008                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $00FFFFFF
        
                dc.b 'Destination position: X      ',0
                dc.b $01
                dc.b $0A
                dc.b $01                                 ; Index to edit routine variable in edvex: crot and editvex: x_one (i.e. index + 1).
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0009                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $01800000
        
                dc.b 'Destination position: Y      ',0   ; 10
                dc.b $00
                dc.b $00
                dc.b $01                                 ; Index to edit routine variable in edvex: crot and editvex: x_one (i.e. index + 1).
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $000A                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $01800000
        
                dc.b 'DVF window centre: X         ',0
                dc.b $01
                dc.b $0C
                dc.b $01                                 ; Index to edit routine variable in edvex: crot and editvex: x_one (i.e. index + 1).
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $000B                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $01800000
        
                dc.b 'DVF window centre: Y         ',0   ; 12
                dc.b $00
                dc.b $00
                dc.b $01                                 ; Index to edit routine variable in edvex: crot and editvex: x_one (i.e. index + 1).
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $000C                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $01800000
        
                dc.b 'Destination position: Z      ',0
                dc.b $00
                dc.b $00
                dc.b $03                                 ; Index to edit routine variable in edvex: sisinglx.
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $000D                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $07FFFFFF
        
                dc.b 'Vector: X                    ',0   ; 14
                dc.b $01
                dc.b $10
                dc.b $05                                 ; Index to edit routine variable in edvex: dvect and editvex: xy1 (i.e. index + 1).
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $000E                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $00FFFFFF
        
                dc.b 'Destination Y offset         ',0
                dc.b $00
                dc.b $00
                dc.b $03                                 ; Index to edit routine variable in edvex: sisinglx.
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $000F                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $01800000
        
                dc.b 'Vector: Y                    ',0   ; 16
                dc.b $00
                dc.b $00
                dc.b $05                                 ; Index to edit routine variable in edvex: dvect and editvex: xy1 (i.e. index + 1).
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0010                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $00FFFFFF
        
                dc.b 'Symmetry Types               ',0
                dc.b $00
                dc.b $00
                dc.b $06                                 ; 6 -> routine at last 4 bytes here (init_sym).
                dc.b $03                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0011                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l init_sym                            ; 
        
                dc.b 'Rotational Symmetry Order    ',0   ; 18
                dc.b $00
                dc.b $00
                dc.b $02                                 ; Index to edit routine variable in edvex: crot and editvex: x_one (i.e. index + 1).
                dc.b $01                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0012                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $00000100
        
                dc.b 'Rotational Angle Step        ',0
                dc.b $00
                dc.b $00
                dc.b $02                                 ; Index to edit routine variable in edvex: snglx and editvex: x_one (i.e. index + 1).
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0013                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $00FFFFFF
        
                dc.b 'Rotational Angle Step Delta  ',0   ; 20
                dc.b $00
                dc.b $00
                dc.b $03                                 ; Index to edit routine variable in edvex: sisinglx.
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0014                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $0000FFFFF
        
                dc.b 'Intensity 1                  ',0
                dc.b $00
                dc.b $00
                dc.b $02                                 ; Index to edit routine variable in edvex: snglx and editvex: x_one (i.e. index + 1).
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0015                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $0000FFFF
                dc.b 'Intensity 2                  ',0   ; 22
                dc.b $00
                dc.b $00
                dc.b $02                                 ; Index to edit routine variable in edvex: snglx and editvex: x_one (i.e. index + 1).
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0016                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $0000FFFF
                dc.b 'Intensity 3                  ',0
                dc.b $00
                dc.b $00
                dc.b $02                                 ; Index to edit routine variable in edvex: snglx and editvex: x_one (i.e. index + 1).
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0017                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $0000FFFF
                dc.b 'Intensity 4                  ',0   ; 24
                dc.b $00
                dc.b $00
                dc.b $02                                 ; Index to edit routine variable in edvex: snglx and editvex: x_one (i.e. index + 1).
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0018                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $0000FFFF
                dc.b 'Intensity 5                  ',0
                dc.b $00
                dc.b $00
                dc.b $02                                 ; Index to edit routine variable in edvex: snglx and editvex: x_one (i.e. index + 1).
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0019                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $0000FFFF
                dc.b 'Intensity 6                  ',0   ; 26
                dc.b $00
                dc.b $00
                dc.b $02                                 ; Index to edit routine variable in edvex: snglx and editvex: x_one (i.e. index + 1).
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $001A                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $0000FFFF
                dc.b 'Z amplitude                  ',0
                dc.b $00
                dc.b $00
                dc.b $02                                 ; Index to edit routine variable in edvex: snglx and editvex: x_one (i.e. index + 1).
                dc.b $01                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $001B                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $0000FFFF
                dc.b 'Fixed point phase 1          ',0   ; 28
                dc.b $00
                dc.b $00
                dc.b $03                                 ; Index to edit routine variable in edvex: sisnglx and editvex: x_one (i.e. index + 1).
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $001C                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $01FFFFFF
        
                dc.b 'Delta phase 1                ',0
                dc.b $00
                dc.b $00
                dc.b $03                                 ; Index to edit routine variable in edvex: sisnglx and editvex: x_one (i.e. index + 1).
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $001D                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $001FFFFF
        
                dc.b 'Rotational sym overall phase ',0   ; 30
                dc.b $00
                dc.b $00
                dc.b $03                                 ; Index to edit routine variable in edvex: sisnglx and editvex: x_one (i.e. index + 1).
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $001E                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $01FFFFFF
        
                dc.b 'Fixed point phase 2          ',0
                dc.b $00
                dc.b $00
                dc.b $03                                 ; Index to edit routine variable in edvex: sisnglx and editvex: x_one (i.e. index + 1).
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $001F                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $01FFFFFF
        
                dc.b 'Number of iterations         ',0   ; 32
                dc.b 0,0,2,2,0,' ',1,0,0,0
        
                dc.b 'X amplitude                  ',0
                dc.b 0,0,2,1,0,'!',0,0,$FF,$FF
        
                dc.b 'Y amplitude                  ',0   ; 34
                dc.b 0,0
                dc.b $02                                 ; Index to edit routine variable in edvex: snglx and editvex: x_one (i.e. index + 1).
                dc.b $01                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0022                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $0000FFFF
        
                dc.b 'Number of other iterations   ',0
                dc.b $00
                dc.b $00
                dc.b $02                                 ; Index to edit routine variable in edvex: snglx and editvex: x_one (i.e. index + 1).
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0023                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $1000000
        
                dc.b 'Parameter not yet defined    ',0   ; 36
                dc.b $00
                dc.b $00
                dcb.l 2,0
        
                dc.b 'delta Z                      ',0
                dc.b $00
                dc.b $00
                dc.b $02                                 ; Index to edit routine variable in edvex: snglx and editvex: x_one (i.e. index + 1).
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0025                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $00FFFFFF
        
                dc.b 'choice of Thang              ',0   ; 38
                dc.b $00
                dc.b $00
                dc.b $03                                 ; Index to edit routine variable in edvex: sisnglx and editvex: x_one (i.e. index + 1).
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0026                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $00FFFFFF
        
                dc.b 'Parameter not yet defined    ',0
                .dphrase
        
                dc.b 'Parameter not yet defined    ',0   ; 40
                .dphrase
                dcb.l 2,0
        
                dc.b 'Parameter not yet defined    ',0
                .dphrase
        
                dc.b 'Rotational Sym centre: X     ',0   ; 42
                dc.w $12B
                dc.b $01                                 ; Index to edit routine variable in edvex: crot and editvex: x_one (i.e. index + 1).
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $002A                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $01800000
        
                dc.b 'Rotational Sym centre: Y     ',0
                dc.b $00
                dc.b $00
                dc.b $00
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $002B                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $01800000
        
                dc.b 'Rotational Symmetry scale    ',0   ; 44
                dc.b $00
                dc.b $00
                dc.b $03                                 ; Index to edit routine variable in edvex: sisnglx and editvex: x_one (i.e. index + 1).
                dc.b $81                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $002C                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $00003FFF
        
                dc.b 'Rotational scale delta: X    ',0
                dc.w $130
                dc.b $04                                 ; Index to edit routine variable in edvex: sidbl and editvex: xy1. (Index + 1)
                dc.b $81                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $002D                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $000003FF
        
                dc.b 'Colour generator vector: X   ',0   ; 46
                dc.w $12F
                dc.b $05                                 ; Index to edit routine variable in edvex: dvect and editvex: xy1 (i.e. index + 1).
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $002E                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $00007FFFF
        
                dc.b 'Colour generator vector: Y   ',0
                dc.b $00
                dc.b $00
                dc.b $05                                 ; Index to edit routine variable in edvex: dvect and editvex: xy1 (i.e. index + 1).
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $002F                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $0007FFFF
        
                dc.b 'Rotational scale delta: Y    ',0   ; 48
                dc.b $00
                dc.b $00
                dc.b $05                                 ; Index to edit routine variable in edvex: dvect and editvex: xy1 (i.e. index + 1).
                dc.b $81                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0030                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $00001000
        
                dc.b 'Rotational centre delta: X   ',0
                dc.w $132
                dc.b $05                                 ; Index to edit routine variable in edvex: dvect and editvex: xy1 (i.e. index + 1).
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0031                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $000FFFFF
        
                dc.b 'Rotational centre delta: Y   ',0   ; 50
                dc.b $00
                dc.b $00
                dc.b $04                                 ; Index to edit routine variable in edvex: sidbl and editvex: xy1. (Index + 1)
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0032                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $0000FFFFF
        
                dc.b 'Delta phase 2                ',0
                dc.b $00
                dc.b $00
                dc.b $03                                 ; Index to edit routine variable in edvex: sisinglx.
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0033                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $001FFFFF
        
                dc.b 'Parameter not yet defined    ',0   ; 52
                dc.b $00
                dc.b $00
                dcb.l 2,0
        
                dc.b 'Radius                       ',0
                dc.b $00
                dc.b $00
                dc.b $03
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0035                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $0FFFFFFF
        
                dc.b 'Base col generator vector: X ',0   ; 54
                dc.b $01
                dc.b $37
                dc.b $05                                 ; Index to edit routine variable in edvex: dvect and editvex: xy1 (i.e. index + 1).
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0036                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $001FFFFF
        
                dc.b 'Base col generator vector: Y ',0
                dc.b $00
                dc.b $00
                dc.b $05                                 ; Index to edit routine variable in edvex: dvect and editvex: xy1 (i.e. index + 1).
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0037                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $001FFFFF
        
                dc.b 'Base colour: X               ',0   ; 56
                dc.b $01
                dc.b $39
                dc.b $05                                 ; Index to edit routine variable in edvex: dvect and editvex: xy1 (i.e. index + 1).
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0038                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $001FFFFF
        
                dc.b 'Base colour: Y               ',0
                dc.b $00
                dc.b $00
                dc.b $05                                 ; Index to edit routine variable in edvex: dvect and editvex: xy1 (i.e. index + 1).
                dc.b $82                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $0039                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $001FFFFF
        
                dc.b 'Destination plot routine     ',0   ; 58
                dc.b $00
                dc.b $00
                dc.b   6                                 ; Index to edit routine variable in edvex: init_sym.
                dc.b $03                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $003A                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l init_sym
        
                dc.b 'Maximum pixel size           ',0
                dc.b $00
                dc.b $00
                dc.b $02                                 ; Index to edit routine variable in edvex: snglx and editvex: x_one (i.e. index + 1).
                dc.b $02                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $003B                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $007FFFFF
        
                dc.b 'Parameter not yet defined    ',0   ; 60
                dc.b $00
                dc.b $00
                dcb.l 2,0
        
                dc.b 'Trigger mask                 ',0
                dc.b $00
                dc.b $00
                dc.b $07                                 ; Index to edit routine variable in edvex: init_byt.
                dc.b $03                                 ; Get the type of the value to display: 1 = byte, 2 = word, 3=16:16.
                dc.w $003D                               ; Index in 4-byte longs to the value to be edited in the fxobj (see ifxobj for a list).
                dc.l $00000000
        
                dc.b 'Parameter not yet defined    ',0 ; 62
                .dphrase
                dcb.l 2,0
                dc.b 'Parameter not yet defined    ',0 
                .dphrase

symvars:        dc.l $111E1213, $142A2B2C, $2D302E2F, $313206FF
dvfvars:        dc.l $1030506, $80B38FF
vars:           dc.l polyvars    ; "Draw a polygon object        "
                dc.l sfvars      ; "Draw 3D starfield            "
                dc.l ringvars    ; "Draw a ring of pixels        "
                dc.l dvf_vars    ; "Digital Video Feedback area  "
                dc.l wsu_vars    ; "Wave Surface Thang           "
                dc.l monomapv    ; "Draw mono bitmap coloured    "
                dc.l monomapv2   ; "Draw mono bitmap i-shaded    "
                dc.l dvf_vars    ; "Digital Video Feedback area  "
                dc.l dvf_vars    ; "Digital Video Feedback area  "
                dc.l plas1var    ; "Colour plasma area type SRCEN    "
                dc.l logovars    ; "Big Jaguar hardware sprite   "
                dc.l obvars      ; "Draw particle object         "
                dc.l pmvars      ; "Do particle motion           "
                dc.l shuuvars    ; "Spectrum as intensities      "

; *******************************************************************
; The description of each effect followed by a list of indices to 
; the parameter descriptions for each in the 'editinginfo' list.
; *******************************************************************
polyvars:       dc.b 'Draw a polygon object        ',0
                dc.w $921
                dc.l $221C1516, $1718191A
                dc.b $26, $3D, $FF
sfvars:         dc.b 'Draw 3D starfield            ',0
                dc.b $09 ; Destination Position X
                dc.b $0D ; Destination Position Z
                dc.b $20 ; X Amplitude
                dc.b $3B
                dc.b $38
                dc.b $39
                dc.b $FF

obvars:         dc.b 'Draw particle object         ',0
                dc.w $090D
                dc.l $3B0E2122, $38392E2F, $1C1D1F15, $161B2526
                dc.b $FF
pmvars:         dc.b 'Do particle motion           ',0
                dc.b $16
                dc.l $2023090D, $E251718, $1938152E
                dc.b $33, $FF
ringvars:       dc.b 'Draw a ring of pixels        ',0
                dc.b $09 ; Destination Position
                dc.b $1C ; Fixed Point Phase 1
                dc.b $1F ; Fixed Point Phase 2
                dc.b $20 ; Number of iterations
                dc.b $35 ; Radius
                dc.b $36 ; Base col generator vector
                dc.b $38 ; Base Color
                dc.b $3B ; Maximum Pixel size
                dc.b $0E ; Vector 
                dc.b $15 ; Intensity 1
                dc.b $16 ; Intensity 2
                dc.b $FF ; End Sentinel

dvf_vars:       dc.b 'Digital Video Feedback area  ',0
                dc.b $01 ; DVF Window Size
                dc.b $03 ; DVF Scale
                dc.b $05 ; DVF Rotate Angle
                dc.b $06 ; DVF Centre of Rotation
                dc.b $08 ; DVF Delta Intensity
                dc.b $0B ; DVF Window Centre
                dc.b $38 ; Base Color
                dc.b $FF ; End Sentinel

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
                dc.l $38363B22, $FF000000, 0
                dc.l -1, 0
                dc.l -1, 0
                dc.l -1, 0
                dc.l -1, 0
                dc.l -1, 0
                dc.l -1, 0
                dc.l -1, 0
                dc.l -1, 0
                dc.l eparm1         ; "~g1:$20:<A> Prev   <B> Menu   <C> Next"

; *******************************************************************
; Option Listing for the 'Edit Mode' screen.
; *******************************************************************
option1:        dc.l star
                dc.l bline1 ; <A> Edit   <B> Edit   <C> Back
                dc.l op11, thisfx ; 'Edit this Effect' and thisfx as associated routine.
                dc.l op19, ispec ; 'Spectrum and Triggers' and ispec as associated routine.

; *******************************************************************
; Option Listing for the 'Editing: Effect' screen.
; *******************************************************************
option2:        dc.l $20000
                dc.l bline1
                dc.l op21, foredit ; 'Edit Source Function'
                dc.l op22, symedit ; 'Edit Symmetry Generator'
                dc.l op23, wsedit ; 'Edit Source Waves'

option3:        dc.l star, bline1, op31, iawfx, op32, iawfy

; *******************************************************************
; Option Listing for the 'Edit basic waveforms' screen.
; *******************************************************************
option4:        dc.l $70000, bline3, op41, rrts, op42, rrts
                dc.l op43, rrts, op44, rrts, op45, rrts
                dc.l op46, rrts, op47, rrts, op48, rrts

option5:        dc.l $20000, bline1, op51, ahead2, op52, ahead2
                dc.l op53, ahead2
option6:        dc.l XADDINC, bline4, op61, rrts, op62, rrts
                dc.l op63, rrts, op64, rrts

; *******************************************************************
; Option Listing for the 'Spectrum and Triggers' screen.
; *******************************************************************
option7:        dc.l $40000
                dc.l bline1 ; <A> Edit   <B> Edit   <C> Back
                dc.l op71, ispec2 ; Trigger 1 and associated edit routine ispec2.
                dc.l op72, ispec2 ; Trigger 2 and associated edit routine ispec2.
                dc.l op73, ispec2 ; Trigger 3 and associated edit routine ispec2.
                dc.l op74, ispec2 ; Trigger 4 and associated edit routine ispec2.
                dc.l op75, ispec2 ; Trigger 5 and associated edit routine ispec2.

; *******************************************************************
; Option Listing for the 'Trigger Settings' screen.
; *******************************************************************
option8:        dc.l star
                dc.l bline1 ; <A> Edit   <B> Edit   <C> Back
                dc.l op81, ispec3 ; 'Set Width' and associated edit routine ispec3
                dc.l op82, ispec4 ; 'Set Trigger Minimum ' and edit routine ispec4.

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

lol:            dc.l bmobj, skale, beasties, pobjs, mpobjs, mmasks, maxes, absdelta
                dc.l zerstart, avbank, envvals ,_fsync, freerun

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

; *******************************************************************
; Definition for a font.
; *******************************************************************
.include "font.s"

; A sprite-sheet in 1-bit pixels containing two small 'Jaguar' logos side by side at the top
; followed by the clipped middle section of a large Jaguar logo.
;
;     v77y6gP6Tu5pgw.^IeT?: 'tSL_*t) `)!yu>s1Tf7oq5#)^,           }5njJGK5aLXEG5.\1T71^ :ag!;eyl ;)eSJl1T223yqdyc^,       
;     .':%O0SIJm6OQGgOo{eJ5nbGjab@hoLF28Qy)JMZ6gw8&f>              _.vZB3}udSGQE6K1]!qm#Yg]e8&SnJ53PQ#|5QGupdObJ<         
; +";=|{fXm#5Fs:`*4o*L!*[Cwf5%lXBgga/_ {@*#Um:cn6XYhJes=_.    ="/;>!wg3oy6%_ vgtvo??}T#ujv*ZDS3a+. ib{#8f_"afSdh3n!\'.    
; _+|%lr<_ /=                  +\^      :"%^     `/){eTt[elc": '^,">|` ;:                  ,\,       |),     .'/%o#11ol%"_
;                 ^fyyy#_  _oy2[=fyyy>.zffyL\         ]Fq4bI:_offfye   _Iyn)eyyyy;'}TLzyy27+    =Jyy32t             '/||-`
;                 yQQQQu  +XQQErTQQQQl2QQQy"   =)[5fs{RQQBl  rQQQQK)  [&QQ0UQQQQ8: `/c8QQU< 1r<t&QQQD3"                   
;                cNQQQE``\GQQQ@D8MQQQ?KQ0J  vwG@NQQQS$QQP>  =&QQQY^`iqQQQWkSdQQQE   -XQQ$c>w@Q0QQW2r,                     
;               )$QQQ&=)bWQ@2e{\lQQQQ?@QQ1 ,j[%)8QQ@bQQX: ^eAQQQA{m8QQD7I". tQQQ6  .FQQ0t5BNQQQM$&V}.                     
;              _h@$$b\ r8@b<    v$$$8)m$$P/    >$$$pA$Y^+eP8K$$8c)8K$Ox     ?$$$}  *$$$X`{EZ8$$UZ62O2<                    
;
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


; *******************************************************************
; oscbank
; Used by omega.gas
;
; Element 1 - Bytes 1-4 - Initial Value
; Element 2 - Bytes 4-8 - Value to increment 'Initial Value' by.
;
; *******************************************************************
oscbank:        dc.l $0400000,$0011100, p_sines,$0000000
                dc.l $0000000,$0016300, p_sines,$0000000
                dc.l $0000000,$0003000, p_sines,$0000000
                dc.l $0000000,$000C000, p_sines,$0000000
                dc.l $0000000,$0010000, p_sines,$0000000
                dc.l $0000000,$000E000, p_sines,$0000000
                dc.l $0000000,$000C300, p_sines,$0000000
                dc.l $0000000,$000F310, p_sines,$0000000

pixcon:         dc.l $00800000, $00000000
                dc.l $00001600, $00003000, $00FFFF00, $00000000
                dc.l $00040000
piycon:         dc.l $00800000, $00000000
                dc.l $00001600, $00003000, $00FFFF00, $00000000
                dc.l $00040000

adsra:          dc.b $0A,   0,   1,   0, $C0,   0,   5,   0
                dc.b   0,   4
adsrb:          dc.b $0B,   0,   1,   0, $C0,   0,   5,   0
                dc.b   0,   4
adsrc:          dc.b $0C,   0,   2,   0, $C0,   0,   5,   0
                dc.b   0,   4,   0,   0
py:             dc.l $00000000
px:             dc.l $00000000
delayf:         dc.w $0000
delayp:         dc.w $0000
dline:          dc.l $00000000
delayt:         dc.w $0000
delayn:         dc.w $0000,$0000

                dc.w $0000,$0000,$0000,$0000,$0000,$0000,$0000

edlgauge:       dc.b '~g3:5:                                 ',0
edlgauge2:      dcb.l 2,0


; The jaguar logo in a 1-bit per pixel bitmap, which looks like:
;
;        ``.:;+\x}a#F4A@Au1{.               '"                         _;:.                 
;      `CA&AKWMM0QQQWX2ty#mP!   -%oyp1;    1&Wy. ia!:     1G5a  ^%]n6EP8H@Y2*..^'           
;      .tZYE4F5DQQM}= :hQQQQG  }ZQQQKYa   L0QH!`nRQQe  .IERQQG1bWQQQQQBHB0QQQm-)/           
;             )WQQ2  xk02UQQy^dQQXa|.    TQQU) yQQQy- i4$qNQQnfA8KQQb?-`+gQQQd`             
;            .XQQU: [MQgTBQQzgQH{ .i1wX2eQQb+ }QQQC ;6QQNKQQN| .cKQ4'l#aX0BZa-              
;            jQQ0{vw0MYgC$QQJKQ# %5FPQQODQg- x$QQFcC$QP2?sBQB^ ,OQ$oqDQQRHL`                
;           ?MQQe VQb\. .$QQaPQk|-  wQNYQY\1dNQQhxMQBI   ^NQb. 6QQ7rYWQ0WZGSr_              
;         `jMQWa;hQZ=    hQQ?=J@BPF5G8NfH8Zf]RQ0X&Qb\    `gQg eRQ$: '{GAAWWKOGL).           
; rj\`  ^!bQ0S>"&QG^     =To"  :vejo<_%`;+,  sTHQBC'      )ML|WQBt    .x2gSkBM@P5{'         
; :12FmPH0@Fi  55?_                          ,EY6\         aiA&f1-       <[yf3Y8dgyzi_      
;    '|i>|_    `                             `|.             l^            .>]npFCjT2?[).   
;                                                                             _)1u1zC]le#ex'
;                                                                                _)]7Ju*1\\:
;                                                                                   -<[e<   
jaglogo:        .incbin "images/jaglogo.cry"

; The VLM logo in 16-bit per pixel cry format, which looks like:
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

; *******************************************************************
; avbank
; The bank data referenced by 1-1,1-2 ... 9-8,9-9 etc.
; *******************************************************************
avbank:
        ; This the Spectrum/Triggers data for each Trigger.
        ; Each spectrum consists of 4 words consisting of:
        ;  - Spectrum Start
        ;  - Spectrum End
        ;  - Minimum value to trigger.
        ;  - Unused.
        dc.w  $0001,$0006,$7000,$1000 ; Spectrum 1 
        dc.w  $0007,$000A,$6000,$1000 ; Spectrum 2
        dc.w  $000B,$0018,$5000,$1000 ; Spectrum 3
        dc.w  $0019,$0028,$4000,$1000 ; Spectrum 4
        dc.w  $0029,$003F,$3000,$1000 ; Spectrum 5
        dc.w  $0000,$0000,$0000,$0000 
av1:
        dc.b  $00,$00,$00,$0A,$00,$90,$B6,$9C ; 0x30
        dc.b  $00,$90,$00,$38,$F4,$35,$2A,$C6 ; 0x38

        ; Offsets into each bank. The 'gm' routine uses the
        ; last two bytes (e.g. $0200, $1856) as an index into 
        ; each bank's data in banks.s.
        dc.l  $00900200 ; Bank 1
        dc.l  $00901856 ; Bank 2
        dc.l  $009034A4 ; Bank 3
        dc.l  $00904482 ; Bank 4
        dc.l  $00905776 ; Bank 5
        dc.l  $009061D8 ; Bank 6
        dc.l  $009075A6 ; Bank 7
        dc.l  $00908806 ; Bank 8
        dc.l  $00909A72 ; Bank 9
        dc.l  $0090A6B4 ; Bank 10 - Used by the splash screen only.


; *******************************************************************
; banks.s
; The data for all 9 banks. Each bank contains 9 effects. Each effect
; consists of 6 fx objects (see ifxobj) of 1024 bytes (6144 bytes in total).
; *******************************************************************
.include "banks.s"

END
; vim:ft=asm68k ts=2
