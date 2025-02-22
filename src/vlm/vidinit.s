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
; vidinit.s
; Routines to set up video, e.g. NTSC/PAL.
;
; ******************************************************************************
.include "../jaguar.inc"
.include "../blitter.inc"
.include "vlm.inc"

.globl _fsync
.globl _fmode
.globl _ein_buf
.globl _monptr
.globl VideoIni
.globl ispal
.globl doit

_fsync:     dc.w 0              
_fmode:     dc.w 0              
_ein_buf:   dc.l 0             
            dc.l 0             
            dc.l 0             
_monptr:    dcb.l $A,0     
                               
; *******************************************************************
; VideoIni
; Check if NTSC or PAL
; For now assume NTSC
; *******************************************************************
VideoIni:       movem.l d0-d6,-(sp)
                clr.w   pal
                move.w  ($F14002).l,d0
                and.w   #$10,d0
                beq.w   ispal
                move.w  #$337,d2
                move.w  #$581,d0
                move.w  #$10A,d6
                move.w  #$F1,d4
                bra.w   doit

ispal:          move.w  #$34B,d2
                move.w  #$565,d0
                move.w  #$142,d6
                move.w  #$11F,d4
                move.w  #1,pal

doit:           move.w  d0,width2
                move.w  d4,height2
                move.w  d0,d1
                asr.w   #1,d1
                sub.w   d1,d2
                add.w   #4,d2
                sub.w   #1,d1
                or.w    #$400,d1
                move.w  d1,a_hde
                move.w  d1,HDE
                move.w  d2,a_hdb
                move.w  d2,HDB1
                move.w  d2,HDB2
                move.w  d6,d5
                sub.w   d4,d5
                move.w  d5,n_vdb
                add.w   d4,d6
                move.w  d6,n_vde
                move.w  #$FFFF,VDE
                move.w  #0,BG
                move.l  #0,BORD1
                movem.l (sp)+,d0-d6
                rts

                .dphrase
; vim:ft=asm68k
