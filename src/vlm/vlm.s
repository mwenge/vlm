; This is the reverse-engineered source code for the 'Virtual Light Machine'
; written by Jeff Minter in 1995 for the Atari Jaguar.
;
; The original code from which this source is derived is the copyright of Jeff Minter.
;
; The original home of this file is at: https://github.com/mwenge/vlm
;
; To the extent to which any copyright may apply to the act of disassembling and reconstructing
; the code from its binary, the author disclaims copyright to this source code.  In place of
; a legal notice, here is a blessing:
;
;    May you do good and not evil.
;    May you find forgiveness for yourself and forgive others.
;    May you share freely, never taking more than you give.

; =============== S U B R O U T I N E =======================================

.include "../jaguar.inc"
.include "../blitter.inc"
.include "vlm.inc"

LaunchVLM:
                move.w  #5,(skid).l
                movea.l #stack,sp
                move.l  #rrts,(davesvec).l
                move.w  #0,(started).l
                move.w  #1,(freerun).l
                move.w  #9,(imatrix).l
                bsr.w   everythi
                lea     (davesobj).l,a0
                rts
; End of function LaunchVLM

; ---------------------------------------------------------------------------
                nop
                movea.l #stack,sp
                move.l  #rrts,(davesvec).l
                move.w  #4,(skid).l
                move.w  #0,(imatrix).l
                move.w  #0,(started).l
                move.l  #$FFFFFFFF,(CLUT).l
                move.l  #$FFFFFFFF,($F00404).l

goag:
                clr.w   (freerun).l
                bsr.w   everythi
                bra.s   goag
; ---------------------------------------------------------------------------
                illegal

; =============== S U B R O U T I N E =======================================


everythi:
                                        ; ROM:00192080↑p

; FUNCTION CHUNK AT 00192D4A SIZE 00000138 BYTES

                bsr.w   gogo
                tst.w   (freerun).l
                bne.w   nodoit
                jsr     iansdoit

nodoit:
                clr.l   (udud).l
                clr.l   (aded).l
                clr.l   (cskr).l
                lea     (refblock).l,a6
                move.w  #5,d7

irb:
                bsr.w   ifxobj
                lea     UPDA2(a6),a6
                dbf     d7,irb
                movea.l #$1B63F8,a6
                move.l  a6,(fx1).l
                move.l  a6,(fxobj).l
                move.l  a6,(fxedbase).l
                move.l  a6,(i_fxobj).l
                bsr.w   ifxobj
                bsr.w   zapdel
                jsr     gm
                lea     (cubeobj).l,a0
                jsr     makeclea
                lea     (genobj).l,a0
                jsr     makecube
                lea     (monoobj).l,a0
                jsr     monovert
                lea     (board).l,a0
                jsr     cleol
                move.w  #$FFFF,(actime).l
                movea.l #versionp,a0  ; "Virtual Light Machine v0.9//(c) 1994 Vi"...
                move.w  #1,(cursx).l
                move.w  #1,(cursy).l
                bsr.w   print
                lea     (beasties).l,a0
                move.l  (draw_scr).l,d2
                move.w  #$FFF8,d0
                sub.w   (palside).l,d0
                move.w  #$2C,d1 ; ','
                add.w   (paltop).l,d1
                sub.w   #$B0,d1
                move.w  #1,(skale).l
                swap    d0
                swap    d1
                move.w  #0,d5
                jsr     makeit
                tst.w   (freerun).l
                bne.w   zippo
                lea     ($1AE0C8).l,a0
                move.l  #$1B00F8,d2
                move.w  #$18,d0
                sub.w   (palside).l,d0
                move.w  #$3C,d1 ; '<'
                add.w   (paltop).l,d1
                swap    d0
                swap    d1
                move.w  #1,d5
                move.w  #$24,d3 ; '$'
                move.w  #$24,d4 ; '$'
                jsr     makeit_t
                move.w  #$FFFF,($1AE0D4).l
                lea     ($1AE088).l,a0

zippo:
                lea     ($1AE108).l,a0
                move.w  #$64,d0 ; 'd'
                move.w  #$104,d1
                move.l  #jaglogo + $04,d2
                move.w  #5,d5
                jsr     makeit_t
                move.w  #$10,$16(a0)
                move.w  #$FFFF,vfb_xsca(a0)
                move.w  #$88FF,($F00420).l
                move.w  #$80FF,($F00422).l
                lea     (davesobj).l,a0
                move.w  #$50,d0 ; 'P'
                move.w  #$104,d1
                swap    d0
                swap    d1
                move.l  #$4000,d2
                move.w  #6,d5
                jsr     makeit_t
                move.w  #4,$16(a0)
                move.w  #$FFFF,vfb_xsca(a0)
                move.w  #1,$1E(a0)
                lea     ($1AE188).l,a0
                move.w  #$50,d0 ; 'P'
                move.w  #$104,d1
                swap    d0
                swap    d1
                move.l  #$4000,d2
                move.w  #6,d5
                jsr     makeit_t
                move.w  #$FFFF,vfb_xsca(a0)
                move.w  #$14,$16(a0)
                move.w  #1,$1E(a0)
                lea     ($1AE1C8).l,a0
                move.w  #$50,d0 ; 'P'
                move.w  #$104,d1
                move.l  #$4000,d2
                move.w  #6,d5
                swap    d0
                swap    d1
                jsr     makeit_t
                move.w  #$FFFF,vfb_xsca(a0)
                move.w  #$24,$16(a0) ; '$'
                move.w  #1,$1E(a0)
                lea     ($1AE208).l,a0
                move.w  #$10A,d0
                move.w  #$190,d1
                move.l  #vlmlogo,d2
                move.w  #7,d5
                swap    d0
                swap    d1
                jsr     makeit_t
                move.w  #$FFFF,vfb_xsca(a0)
                move.w  #$FFFF,(vlmtim).l
                lea     (envvals).l,a0
                move.w  #7,d0

xx:
                clr.l   (a0)+
                dbf     d0,xx
                clr.w   (XLO).l
                clr.w   (YLO).l
                move.w  #$17F,(XHI).l
                move.w  #$17F,(YHI).l
                move.l  #$C00000,(XCEN).l
                move.l  #$C00000,(YCEN).l
                move.l  #$1AE550,(fx).l
                move.w  #1,(fxed).l
                lea     ($1B67F8).l,a6
                move.w  #4,d7

iprep:
                movem.l d7/a5-a6,-(sp)
                bsr.w   ifxobj
                movem.l (sp)+,d7/a5-a6
                lea     UPDA2(a6),a6
                dbf     d7,iprep
                move.l  #$1B69F8,(fxobj).l
                move.w  #0,(monitor).l
                move.w  #$104,(monx).l
                move.w  #$F0,(mony).l
                move.w  #3,(monitand).l
                move.l  #symadj,(routine).l
                clr.l   (action).l
                jmp     titlescr
; End of function everythi

; ---------------------------------------------------------------------------
                bsr.w   gadd
                clr.l   (a0)
; START OF FUNCTION CHUNK FOR makecfb
;   ADDITIONAL PARENT FUNCTION makestar
;   ADDITIONAL PARENT FUNCTION makemono

ggg:
                                        ; sub_1924DA+12↓j ...
                move.l  a6,(fxedbase).l
                move.l  a6,(fxobj).l
                addi.l  #4,(esp).l
                jmp     initedit
; END OF FUNCTION CHUNK FOR makecfb
; ---------------------------------------------------------------------------
makepoly:
                bsr.w   gadd
                bsr.w   initwave
                move.w  #$15,d1
                move.l  #$40FFFF,d0
                bsr.w   wlink
                move.w  #$16,d1
                move.l  #$80FFFF,d0
                bsr.w   wlink
                move.w  #$17,d1
                move.l  #$1FFFF,d0
                bsr.w   wlink
                move.w  #$18,d1
                move.l  #$2FFFF,d0
                bsr.w   wlink
                move.w  #$19,d1
                move.l  #$40FFFF,d0
                bsr.w   wlink
                move.w  #$1A,d1
                move.l  #$80FFFF,d0
                bsr.w   wlink
                move.l  a6,(a0)
                bra.s   ggg

; =============== S U B R O U T I N E =======================================


gogo:
                tst.w   (started).l
                bne.w   rrts
                move.w  #1,(started).l
                move.l  #$70007,(G_END).l
                move.l  #$70007,(D_END).l
                move.w  #$100,(JOYSTICK).l
                move.w  #1,(INT1).l
                move.l  #dumint,(UPDA1F).w
                move.l  #$FFFFFFFF,(action).l
                bsr.w   startup
                move.l  #$FFFFFFFF,(action).l
                rts
; End of function gogo

; ---------------------------------------------------------------------------
                lea     (WID256).w,a0
                move.w  #$7F,d0

tdo:
                move.w  #$FF,d1

tdo2:
                move.w  d0,d2
                add.w   d1,d2
                move.b  d2,(a0)+
                dbf     d1,tdo2
                dbf     d0,tdo
                rts
; ---------------------------------------------------------------------------
mdvf2:
                bsr.w   makecfb
                move.w  #1,thang(a6)
                move.w  #$60,($18).w ; '`'
                move.w  #$60,($1C).w ; '`'
                move.w  #$60,(vfb_xpos).w ; '`'
                move.w  #$60,(vfb_ypos).w ; '`'
                move.w  #$78,($08).w ; 'x'
                rts
; ---------------------------------------------------------------------------
mdvf4:
                bsr.w   makecfb
                move.w  #2,thang(a6)
                move.w  #$30,($18).w ; '0'
                move.w  #$30,($1C).w ; '0'
                move.w  #$30,(vfb_xpos).w ; '0'
                move.w  #$30,(vfb_ypos).w ; '0'
                move.w  #$38,($08).w ; '8'
                rts
; ---------------------------------------------------------------------------
makefb:
                bsr.w   gadd
                move.l  #1,$8C(a6)
                bra.w   fnop

; =============== S U B R O U T I N E =======================================


makecfb:
                bsr.w   gadd
                move.l  #0,$8C(a6)

fnop:
                bsr.w   initdvf
                move.l  a6,(a0)
                bra.w   ggg
                rts
; ---------------------------------------------------------------------------
makering:
                bsr.w   gadd
                bsr.w   initring
                move.l  a6,(a0)
                bra.w   ggg
; ---------------------------------------------------------------------------
ipm:
                bsr.w   gadd
                clr.l   $04(a6)
                move.l  #$D,info(a6)
                move.l  #$60000,gpu(a6)
                move.l  a6,(a0)
                bra.w   ggg
; ---------------------------------------------------------------------------
itau:
                bsr.w   mpo
                move.l  #$80000,d_x(a6)
                move.l  #$80000,d_z(a6)
                move.l  #$80000,deltaz(a6)
                move.l  #$80000,gpu(a6)
                rts
; ---------------------------------------------------------------------------
mopo:
                bsr.w   mpo
                move.l  #$70000,gpu(a6)
                rts

; =============== S U B R O U T I N E =======================================


mpo:
                                        ; ROM:00192544↑p
                bsr.w   makestar
                move.l  #$C,info(a6)
                move.l  #$50000,gpu(a6)
                rts
; End of function mpo

; ---------------------------------------------------------------------------
makeshun:
                bsr.w   makestar
                move.l  #$E,info(a6)
                move.l  #$90000,gpu(a6)
                rts

; =============== S U B R O U T I N E =======================================


makestar:
                                        ; ROM:00192568↑p

; FUNCTION CHUNK AT 00192386 SIZE 0000001C BYTES

                bsr.w   gadd
                bsr.w   initpsf
                move.l  a6,(a0)
                bra.w   ggg
; End of function makestar

; ---------------------------------------------------------------------------
amkesurf:
                bsr.w   gadd
                bsr.w   initwsur
                move.l  a6,(a0)
                bra.w   ggg

; =============== S U B R O U T I N E =======================================


makemono:
                                        ; ROM:001925DA↓p ...

; FUNCTION CHUNK AT 00192386 SIZE 0000001C BYTES

                bsr.w   gadd
                bsr.w   initmono
                move.l  a6,(a0)
                bra.w   ggg
; End of function makemono

; ---------------------------------------------------------------------------
mplaz2:
                bsr.w   mplaz1
                move.l  #$40004,gpu(a6)
                rts

; =============== S U B R O U T I N E =======================================


mplaz1:
                bsr.s   makemono
                move.l  #$40002,gpu(a6)
                move.l  #$2000,j(a6)
                move.l  #$2000,k(a6)
                move.l  #$A,info(a6)
                rts
; End of function mplaz1

; ---------------------------------------------------------------------------
mjaglogo:
                bsr.s   makemono
                move.l  #$40003,gpu(a6)
                move.l  #$B,info(a6)
                rts
; ---------------------------------------------------------------------------
makeiri:
                bsr.s   makemono
                move.l  #$40001,gpu(a6)
                rts

; =============== S U B R O U T I N E =======================================


gadd:
                                        ; ROM:001923A2↑p ...
                movea.l (fx).l,a0
                move.w  (og).l,d0
                lsl.w   #2,d0
                lea     (a0,d0.w),a0
                move.l  (a0),d6
                beq.w   zzzero
                movea.l d6,a6
                clr.l   (a0)
                bra.w   ifxobj
; ---------------------------------------------------------------------------

zzzero:
                movea.l (fx).l,a6
                movea.l (a6),a6
                move.w  (og).l,d0
                mulu.w  #$400,d0
                adda.l  d0,a6
                bra.w   ifxobj
; End of function gadd


; =============== S U B R O U T I N E =======================================


initmono:
                move.l  #6,info(a6)
                move.l  #$40000,gpu(a6)
                rts
; End of function initmono


; =============== S U B R O U T I N E =======================================


initwsur:
                bsr.w   initpsf
                move.l  #$800000,dstoffz(a6)
                move.l  #0,phase1(a6)
                move.l  #$100000,phase2(a6)
                move.l  #$20000,phase5(a6)
                move.l  #$100000,i(a6)
                move.l  #$100000,$8C(a6)
                move.l  #$100000,d_x(a6)
                move.l  #$10000,d_z(a6)
                move.l  #2,gpu(a6)
                move.l  #5,info(a6)
                rts
; End of function initwsur


; =============== S U B R O U T I N E =======================================


initpsf:
                                        ; sub_192644↑p
                move.l  #3,plot_mod(a6)
                move.l  #0,gpu(a6)
                move.l  #2,info(a6)
                rts
; End of function initpsf


; =============== S U B R O U T I N E =======================================


initwave:
                move.l  #$20000,gpu(a6)
                move.l  #1,info(a6)
                rts
; End of function initwave


; =============== S U B R O U T I N E =======================================


initdvf:
                move.l  #4,info(a6)
                move.l  #$10000,gpu(a6)
                move.l  #$1AF078,i(a6)
                move.l  #$1E7540,sine_bas(a6)
                rts
; End of function initdvf


; =============== S U B R O U T I N E =======================================


initring:
                clr.l   _i1(a6)
                clr.l   _i2(a6)
                move.l  #9,gpu(a6)
                move.l  #3,info(a6)
                move.l  #2,plot_mod(a6)
                rts
; End of function initring


; =============== S U B R O U T I N E =======================================


wavelink:
                                        ; ROM:00195508↓p
                movea.l (fxobj).l,a6
; End of function wavelink


; =============== S U B R O U T I N E =======================================


wlink:
                                        ; ROM:001923C2↑p ...
                lsl.w   #2,d1
                lea     optionbu(a6),a5
                lea     (a5,d1.w),a5
                lea     (a6,d1.w),a4
                move.l  (a4),(a5)
                move.l  d0,UPDA1F(a4)
                rts
; End of function wlink


; =============== S U B R O U T I N E =======================================


symadj:
                lea     (pad_now).l,a1
                bsr.w   doadsr
                tst.w   (vlm_mode).l
                beq.w   rrts
                lea     (pad_now).l,a1
                tst.w   (editing).l
                beq.w   shoop
                lea     ($1AE010).l,a1

shoop:
                move.b  SRCEN(a1),d0
                rol.b   #2,d0
                lea     (pixcon).l,a0
                jsr     pinertco
                move.b  SRCEN(a1),d0
                rol.b   #4,d0
                lea     (piycon).l,a0
                jsr     pinertco
                move.l  (pixcon).l,d0
                lsr.l   #8,d0
                and.l   #$FFFF,d0
                move.l  d0,(px).l
                move.l  (piycon).l,d0
                lsr.l   #8,d0
                and.l   #$FFFF,d0
                move.l  d0,(py).l
                rts
; End of function symadj


; =============== S U B R O U T I N E =======================================


doadsr:
                tst.w   (vlm_mode).l
                bne.w   ago
                lea     (zero).l,a1

ago:
                movea.l a1,a3
                lea     (envvals).l,a1
                lea     $A(a1),a1
                lea     (adsrc).l,a0
                move.l  (a3),d0
                and.l   #$2000,d0
                bsr.w   do_adsr
                lea     2(a1),a1
                lea     (adsrb).l,a0
                move.l  (a3),d0
                and.l   #$2000000,d0
                bsr.w   do_adsr
                lea     2(a1),a1
                lea     (adsra).l,a0
                move.l  (a3),d0
                and.l   #$20000000,d0
; End of function doadsr


; =============== S U B R O U T I N E =======================================


do_adsr:
                                        ; sub_19279E+40↑p
                move.w  $08(a0),d1
                lea     (adsrvex).l,a4
                lsl.w   #2,d1
                movea.l (a4,d1.w),a4
                jmp     (a4)
; End of function do_adsr

; ---------------------------------------------------------------------------

adsrvex:
                dc.l trigwait, attack, decay, sustain, release
                tst.l   d0
                beq.w   rrts

setatac:
                move.w  #1,$08(a0)
                rts
; ---------------------------------------------------------------------------
                tst.l   d0
                beq.w   srel
                move.w  (a1),d1
                move.w  (a0),d2
                lsr.w   #2,d2
                and.l   #$FFFF,d1
                and.l   #$FFFF,d2
                add.l   d2,d1
                cmp.l   #$FFFF,d1
                ble.w   setay
                move.l  #$FFFF,d1
                move.w  #2,$08(a0)

setay:
                                        ; ROM:00192884↓j ...
                move.w  d1,(a1)
                rts
; ---------------------------------------------------------------------------
                tst.l   d0
                beq.w   srel
                move.w  (a1),d1
                move.w  2(a0),d2
                lsr.w   #2,d2
                move.w  $04(a0),d3
                and.l   #$FFFF,d1
                and.l   #$FFFF,d2
                and.l   #$FFFF,d3
                sub.l   d2,d1
                cmp.l   d3,d1
                bpl.s   setay
                move.l  d3,d1
                move.w  #3,$08(a0)
                bra.s   setay
; ---------------------------------------------------------------------------
                tst.l   d0
                bne.w   rrts

srel:
                                        ; ROM:0019285E↑j
                move.w  #4,$08(a0)
                rts
; ---------------------------------------------------------------------------
                tst.l   d0
                bne.w   setatac
                move.w  (a1),d1
                move.w  6(a0),d2
                lsr.w   #2,d2
                and.l   #$FFFF,d1
                and.l   #$FFFF,d2
                sub.l   d2,d1
                bpl.s   setay
                clr.w   (a1)
                move.w  #0,$08(a0)
                rts
; ---------------------------------------------------------------------------
                bsr.w   gkpad
                bra.w   pn_butte

; =============== S U B R O U T I N E =======================================


gkpad:
                lea     (ud_butts).l,a0
; End of function gkpad


; =============== S U B R O U T I N E =======================================


gkp:

; FUNCTION CHUNK AT 00192D2C SIZE 0000001E BYTES

                lea     (pad_now).l,a1
                tst.w   (editing).l
                beq.w   gnana
                movea.l ($1AF056).l,a3
                move.w  #$B,d0

flashem:
                tst.b   (a3)
                bmi.w   nxtk
                tst.b   3(a3)
                beq.w   nxtk
                subi.b  #1,3(a3)
                move.b  3(a3),d1
                and.w   #3,d1
                bne.w   nxtk
                move.l  a0,(action).l
                bchg    #7,2(a3)

nxtk:
                                        ; sub_1928D4+24↑j ...
                lea     $04(a3),a3
                dbf     d0,flashem

gnana:
                lea     (cpad).l,a0
                movea.l $04(a0),a3
                movea.l (a0),a0
                movea.l (i_fxobj).l,a6
                move.w  (og).l,d0
                movea.l (fx).l,a6
                lsl.w   #2,d0
                move.l  (a6,d0.w),d1
                beq.w   rrts
                movea.l d1,a6
                move.l  d1,(i_fxobj).l
                move.w  (a1),d6
                move.w  #3,d7

gk1:
                movea.l (a0)+,a2
                btst    #0,d6
                beq.w   bodb
                tst.b   3(a3)
                bne.w   bodb
                bra.w   execute
; ---------------------------------------------------------------------------

bodb:
                                        ; sub_1928D4+92↑j
                lsr.w   #1,d6
                lea     $04(a3),a3
                dbf     d7,gk1
                move.w  2(a1),d6
                move.w  #7,d7

gk2:
                movea.l (a0)+,a2
                btst    #0,d6
                beq.w   bodb2
                tst.b   3(a3)
                bne.w   bodb2
                bra.w   execute
; ---------------------------------------------------------------------------

bodb2:
                                        ; sub_1928D4+BA↑j
                lsr.w   #1,d6
                lea     $04(a3),a3
                dbf     d7,gk2
                rts
; ---------------------------------------------------------------------------
                move.l  (a1),d0
                and.l   #$22002000,d0
                bne.w   somebutt
                clr.w   (symed).l
                rts
; ---------------------------------------------------------------------------

somebutt:
                move.l  d0,-(sp)
                movea.l (a0)+,a2
                cmp.l   #$2000,d0
                bne.w   notc
                jsr     (a2)

notc:
                move.l  (sp)+,d0
                move.l  d0,-(sp)
                movea.l (a0)+,a2
                cmp.l   #$2000000,d0
                bne.w   notb
                jsr     (a2)

notb:
                move.l  (sp)+,d0
                move.l  d0,-(sp)
                movea.l (a0)+,a2
                cmp.l   #$20000000,d0
                bne.w   nota
                jsr     (a2)

nota:
                move.l  (sp)+,d0
                move.l  d0,-(sp)
                movea.l (a0)+,a2
                cmp.l   #$2002000,d0
                bne.w   notab
                jsr     (a2)

notab:
                move.l  (sp)+,d0
                rts
; ---------------------------------------------------------------------------
dorsym1:
                lea     ((byte_196DEF+$12D)).l,a3
                move.w  #2,(symed).l
                bra.w   gjoy
; ---------------------------------------------------------------------------
dorsym2:
                lea     ((byte_196DEF+$13D)).l,a3
                move.w  #3,(symed).l
                bra.w   gjoy
; ---------------------------------------------------------------------------
dorsym3:
                lea     ((byte_196DEF+$14D)).l,a3
                move.w  #4,(symed).l
                bra.w   *+4
; ---------------------------------------------------------------------------

gjoy:
                                        ; sub_1928D4+14C↑j ...
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
                movea.l $04(a3),a4
                jsr     (a4)

npright:
                move.l  (a1),d0
                and.l   #$100000,d0
                beq.w   npup
                movea.l $08(a3),a4
                jsr     (a4)

npup:
                move.l  (a1),d0
                and.l   #$200000,d0
                beq.w   npdn
                movea.l vfb_xsca(a3),a4
                jsr     (a4)

npdn:
                rts
; ---------------------------------------------------------------------------
                move.w  #1,(symed).l
                move.b  SRCEN(a1),d0
                rol.b   #2,d0
                bsr.w   rxs
                move.b  SRCEN(a1),d0
                rol.b   #4,d0
                bra.w   rys
; ---------------------------------------------------------------------------

execute:
                                        ; sub_1928D4+BE↑j
                tst.w   (editing).l
                beq.w   exec1
                move.b  #$1C,3(a3)
                btst    #0,2(a3)
                beq.w   exec1
                move.b  #$20,3(a3) ; ' '

exec1:
                                        ; sub_1928D4+1DE↑j
                move.l  a3,-(sp)
                jsr     (a2)
                movea.l (sp)+,a3
                tst.w   (editing).l
                beq.w   rrts
                cmpi.w  #2,(antelope).l
                bne.w   anteclr
                clr.w   2(a3)
                clr.w   (antelope).l
                move.w  #1,(seldb).l

anteclr:
                cmpi.l  #keydb,(routine).l
                beq.w   rrts
                move.l  (routine).l,(ov).l
                move.l  #keydb,(routine).l
                rts
; End of function gkp


; =============== S U B R O U T I N E =======================================


keydb:
                                        ; sub_1928D4+22E↑o
                move.l  (pad_now).l,d1
                and.l   #$E00FF,d1
                bne.w   rrts
                move.l  (ov).l,(routine).l
                rts
; End of function keydb


; =============== S U B R O U T I N E =======================================


zapdel:
                                        ; skidoo+7E↓p
                move.w  #2,(delayf).l
                clr.w   (delayp).l
                move.w  #2,(delayt).l
                move.w  #$F,(delayn).l
                clr.w   (d2elayp).l
                move.w  #2,(d2elayt).l
                move.w  #$F,(d2elayn).l
                move.w  #3,(zerstart).l
                lea     (delaylin).l,a0
                move.l  a0,(dline).l
                movea.l a0,a1
                lea     $300(a1),a1
                move.l  a1,(d2line).l
                move.w  #$7F,d0

idli:
                clr.l   info(a0)
                lea     $300(a0),a0
                dbf     d0,idli
                rts
; End of function zapdel


; =============== S U B R O U T I N E =======================================


setedit:
                tst.w   (vedit).l
                bne.w   rrts
                move.w  (vlm_mode).l,(ovlm_mod).l
                move.w  #1,(vedit).l
                move.w  #3,(vlm_mode).l
                move.w  #1,($1AE0D4).l
                move.l  #initedit,(action).l
                rts
; End of function setedit


; =============== S U B R O U T I N E =======================================


symclr:
                move.l  #1,asym_fla(a6)
                bra.w   setstat
; ---------------------------------------------------------------------------
                eori.l  #$100FF,asym_fla(a6)

setstat:
                bsr.w   isymbut
                move.l  #ud_butts,(action).l
                rts
; End of function symclr

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
                move.w  (fframes).l,d2
                and.w   #7,d2
                bne.w   rrts
                addi.w  #1,$4A(a6)
                rts
; ---------------------------------------------------------------------------
rsymd:
                move.w  (fframes).l,d2
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

; =============== S U B R O U T I N E =======================================


rxs:
                lea     (ixcon).l,a0
                jsr     inertcon
                move.w  (a0),d0
                swap    d0
                clr.w   d0
                move.l  d0,rxcen(a6)
                move.l  d0,(RXCEN).l
                rts
; End of function rxs

; ---------------------------------------------------------------------------
; START OF FUNCTION CHUNK FOR gkp

rys:
                lea     (iycon).l,a0
                jsr     inertcon
                move.w  (a0),d0
                swap    d0
                clr.w   d0
                move.l  d0,rycen(a6)
                move.l  d0,(RYCEN).l
                rts
; END OF FUNCTION CHUNK FOR gkp
; ---------------------------------------------------------------------------
; START OF FUNCTION CHUNK FOR everythi

titlescr:
                movea.l (draw_scr).l,a0
                move.w  #1,(db_on).l
                movea.l (fx).l,a6
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
                move.w  d2,d3
                lea     (dvf_buf).l,a0
                move.l  (cscreen).l,(a0)+
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
                move.l  #$1E7540,(a0)+
                move.l  (draw_scr).l,(a0)+
                move.l  a6,(a0)+
                move.l  #$1E8274,($F03F7C).l
                move.l  #lol,(in_buf).l
                move.l  #$1E8094,($F03F64).l
                move.l  (fx).l,($F03F68).l
                move.l  #$1E81F4,($F03F6C).l
                move.l  #pbinfo,($F03F70).l ; "Parameter not yet defined    "
                move.l  #delayf,($F03F74).l
                move.l  #polyos,($F03F78).l
                move.l  #gpumods,(mods).l
                move.l  #1,(screen_r).l
                moveq   #$A,d0
                lea     (omega).l,a0
                jsr     gpurun

editloop:
                                        ; sub_192088+DF8↓j
                clr.w   (moomoomo).l
                tst.w   (freerun).l
                beq.w   eloop
                rts
; ---------------------------------------------------------------------------

eloop:
                tst.w   (_fsync).l
                beq.s   editloop
                move.w  #1,(moomoomo).l
                bsr.w   yakedit
                move.w  #2,(moomoomo).l
                movea.l (davesvec).l,a0
                jsr     (a0)
                move.w  #3,(moomoomo).l
                clr.w   (_fsync).l
                move.w  #4,(moomoomo).l
                bra.s   editloop
; END OF FUNCTION CHUNK FOR everythi

; =============== S U B R O U T I N E =======================================


yakedit:
                tst.w   (actime).l
                bmi.w   no_ac
                subi.w  #1,(actime).l
                bpl.w   no_ac
                lea     (clearstr).l,a0 ; "@"
                bsr.w   print

no_ac:
                                        ; sub_192E82+12↑j
                movea.l (fx).l,a6
                move.w  (fxed).l,d0
                lsl.w   #2,d0
                movea.l (a6,d0.w),a6
                move.l  a6,(fxobj).l
                tst.l   (action).l
                beq.w   noact
                movea.l (action).l,a0
                movea.l (fxedbase).l,a6
                jsr     (a0)
                clr.l   (action).l

noact:
                                        ; sub_192E82+62↓j
                move.l  (ixcon).l,d0
                cmp.l   (ixcon).l,d0
                bne.s   noact
                move.l  d0,(iixcon).l

noact2:
                move.l  (iycon).l,d0
                cmp.l   (iycon).l,d0
                bne.s   noact2
                move.l  d0,(iiycon).l
                move.w  (symed).l,d0
                beq.w   blib
                lea     (edvex).l,a0
                subq.w  #1,d0
                lsl.w   #2,d0
                movea.l (a0,d0.w),a0
                jsr     (a0)

blib:
                movea.l (fxedbase).l,a6
                bsr.w   shad
                rts
; End of function yakedit


; =============== S U B R O U T I N E =======================================


elcon:
                                        ; ROM:00194010↓p ...
                move.l  a0,-(sp)
                lea     $08(a0),a0
                lea     (pbinfo).l,a2 ; "Parameter not yet defined    "
                lea     (elinx).l,a3
                lea     (eltxt).l,a5
                move.l  #$1E8054,(elvp).l
                move.w  #$3F,d1 ; '?'

elc1:
                clr.l   (a3)+
                dbf     d1,elc1
                lea     (elinx).l,a3
                move.w  #1,d4

elc:
                                        ; sub_192F26+F8↓j
                move.b  (a1)+,d2
                bmi.w   elcend
                and.w   #$FF,d2
                move.w  d2,d7
                tst.b   (a3,d2.w)
                bne.s   elc
                lsl.w   #3,d2
                move.w  d2,d3
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
                movem.l a0-a1,-(sp)
                movea.l (elvp).l,a0
                lea     $1E(a4),a1
                move.l  a1,(a0)+
                move.l  a0,(elvp).l
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
                move.w  d5,d6
                lsl.w   #2,d5
                add.w   d6,d5
                lea     (a2,d5.w),a4
                move.l  a4,-(sp)

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
                move.w  d1,(a0)
                move.w  #0,2(a0)
                move.l  #bline2,$04(a0) ; "~g1:$20:Joypad to select, any FIRE to ed"...
                rts
; End of function elcon


; =============== S U B R O U T I N E =======================================


ifxobj:
                                        ; sub_192088+5A↑p ...
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
                move.l  #$400,phase2(a6)
                move.l  #$140000,i(a6)
                move.l  #$400,j(a6)
                move.l  #$400,k(a6)
                move.l  #$400,zamp(a6)
                move.l  #3,asym_fla(a6)
                move.l  #$1E7540,sine_bas(a6)
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
                move.l  #$1E7540,wave_2(a6)
                move.l  #0,phase4(a6)
                move.l  #$21555,phase5(a6)
                move.l  #0,plot_mod(a6)
                move.l  #$1850000,vfb_xsca(a6)
                move.l  #$1850000,vfb_ysca(a6)
                move.l  #0,vfb_angl(a6)
                move.l  #$C00000,$18(a6)
                move.l  #$C00000,$1C(a6)
                move.l  #$780000,$20(a6)
                move.l  #0,$04(a6)
                move.l  #$E00000,$08(a6)
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
                lea     (results).l,a4
                lea     optionbu(a6),a3
                move.w  #$3F,d0 ; '?'

zlop:
                move.l  #$FFFF,(a5)+
                move.w  #0,(a4)+
                move.l  #0,(a3)+
                dbf     d0,zlop
                lea     $300(a6),a3
                lea     (oscbank).l,a4
                move.w  #$1F,d0

gbsb:
                move.l  (a4)+,(a3)+
                dbf     d0,gbsb
                movem.l a0-a1,-(sp)
                lea     $380(a6),a1
                lea     (pixcon).l,a0
                move.l  #$6C,d0 ; 'l'
                jsr     copybloc
                movem.l (sp)+,a0-a1
                rts
; End of function ifxobj


; =============== S U B R O U T I N E =======================================


startup:
                jsr     VideoIni
                jsr     InitList
                move.l  (ddlist).l,d0
                move.w  #0,(ODP).l
                swap    d0
                move.l  d0,(OLP).l
                lea     (zerstart).l,a0
                move.l  #$794,d0
                lsr.l   #2,d0

cram:
                clr.l   (a0)+
                sub.l   #1,d0
                bpl.s   cram
                move.b  #1,(intmask).l
                jsr     InitBeas
                move.w  #$FFFF,(db_on).l
                clr.l   (screen_r).l
                move.l  #rrts,(routine).l
                move.l  #rrts,(_fx).l
                move.l  #Frame,(UPDA1F).w
                move.w  (n_vde).l,d0
                or.w    #1,d0
                move.w  d0,(VI).l
                move.b  (intmask).l,d0
                move.w  d0,(INT1).l
                move    sr,d0
                and.w   #$F8FF,d0
                move    d0,sr
                move.w  #$6C1,(VMODE).l
                movea.l #$100000,a0
                jsr     clrscree
                movea.l #$148000,a0
                jsr     clrscree
                move.l  #$100000,(cscreen).l
                move.l  #$148000,(dscreen).l
                move.l  (dscreen).l,(draw_scr).l
                lea     (sines).l,a0 ; ;sines.bin
                move.b  (a0),(byte_196B1D+$CB).l
                lea     (p_sines).l,a1
                lea     (p_rect).l,a2
                move.w  #$100,d0

mpsines:
                move.b  (a0)+,d1
                ext.w   d1
                move.w  d1,d2
                bpl.w   mrect1
                neg.w   d2

mrect1:
                lsl.w   #1,d2
                move.b  d2,(a2)+
                add.w   #$80,d1
                move.b  d1,(a1)+
                dbf     d0,mpsines
                lea     (p_saw).l,a0
                lea     (p_ramp).l,a1
                clr.w   d0

gsaw:
                move.w  d0,d1
                move.w  d0,d2
                lsr.w   #1,d2
                move.b  d2,(a1)+
                sub.w   #$FF,d1
                bpl.w   gsaw1
                neg.w   d1

gsaw1:
                move.b  d1,(a0)+
                add.w   #2,d0
                cmp.w   #$200,d0
                blt.s   gsaw
                move.b  (p_saw).l,($1E7744).l
                lea     (p_square).l,a0
                move.w  #$FF,d0

gsqu:
                clr.w   d1
                cmp.w   #$80,d0
                blt.w   gsqu2
                move.w  #$FF,d1

gsqu2:
                move.b  d1,(a0)+
                dbf     d0,gsqu
                move.b  (p_square).l,($1E7848).l
                clr.w   (palside).l
                clr.w   (paltop).l
                tst.w   (pal).l
                beq.w   notpal1
                move.w  #6,(palside).l
                move.w  #$1E,(paltop).l

notpal1:
                rts
; End of function startup


; =============== S U B R O U T I N E =======================================


dumint:
                move.w  #$101,(INT1).l
                move.w  #$101,(INT2).l
                rte
; End of function dumint

; ---------------------------------------------------------------------------
                move.w  (frames).l,d7

wsnc:
                cmp.w   (frames).l,d7
                beq.s   wsnc
                rts

; =============== S U B R O U T I N E =======================================


clrscree:
                                        ; sub_19324E+B0↑p
                clr.w   d0
                clr.w   d1
                move.w  #$180,d2
                move.w  #$180,d3
                move.w  #$7700,d4
                bra.w   blitbloc
; End of function clrscree

; ---------------------------------------------------------------------------
                bsr.w   makeit
                move.w  #0,vfb_angl(a0)
                rts

; =============== S U B R O U T I N E =======================================


makeit_t:
                                        ; sub_192088+15A↑p ...
                bsr.w   makeit
                move.w  #1,vfb_angl(a0)
                rts
; End of function makeit_t


; =============== S U B R O U T I N E =======================================


makeit:
                                        ; ROM:00193414↑p ...
                move.w  d5,$E(a0)
                move.l  d0,(a0)
                move.l  d1,$04(a0)
                move.w  d3,$08(a0)
                move.w  d4,$A(a0)
                move.l  d2,vfb_ysca(a0)
                move.w  #$FFFF,vfb_angl(a0)
                clr.w   $16(a0)
                move.w  d5,vfb_xsca(a0)
                lsl.w   #3,d5
                lea     (ObTypes).l,a1
                move.w  (a1,d5.w),$18(a0)
                move.w  2(a1,d5.w),$1A(a0)
                move.w  $04(a1,d5.w),$1C(a0)
                clr.w   $1E(a0)
                rts
; End of function makeit


; =============== S U B R O U T I N E =======================================


copybloc:
                                        ; sub_19510A+18↓p ...
                movem.l d0/a0-a1,-(sp)

copb:
                move.b  (a0)+,(a1)+
                dbf     d0,copb
                movem.l (sp)+,d0/a0-a1
                rts
; End of function copybloc

; ---------------------------------------------------------------------------
                dc.l alpha, beta
; =============== S U B R O U T I N E =======================================


thangg:
                bsr.w   db
                movea.l (draw_rou).l,a0
                jsr     (a0)
                move.l  #1,(screen_r).l
                addi.l  #1,(fframes).l
                rts
; End of function thangg


; =============== S U B R O U T I N E =======================================


wfm:
                                        ; ROM:00193C18↓p

; FUNCTION CHUNK AT 00193762 SIZE 00000042 BYTES

                move.w  (monx).l,d0
                move.w  (mony).l,d1
                movea.l (draw_scr).l,a0
                sub.w   #$40,d1 ; '@'
                move.w  #$40,d2 ; '@'
                move.w  #$40,d3 ; '@'
                move.w  #$C0,d4
                jsr     sblitblo
                move.l  (draw_scr).l,(_ein_buf).l
                move.w  (selected).l,d2
                lsl.w   #4,d2
                movea.l (edwave).l,a0
                lea     (a0,d2.w),a0
                cmpi.w  #7,$E(a0)
                bge.w   joyxy
                move.l  a0,(_ein_buf+4).l
                add.w   #$40,d1 ; '@'
                swap    d1
                move.w  d0,d1
zz0:
                move.l  d1,(_ein_buf+8).l
                move.w  #5,(_fmode).l
                bra.w   fmodewai
; End of function wfm

; ---------------------------------------------------------------------------
shspec:
                movea.l (draw_scr).l,a0
                move.w  (mony).l,d5
                add.w   #$10,d5
                swap    d5
                move.w  #$3C,d0 ; '<'
                move.w  #3,d2
                lea     ($F1B038).l,a1
                lea     (absdelta).l,a1
                lea     (maxes).l,a3
                move.w  #$3F,d5 ; '?'
                lea     (avbank).l,a4
                move.w  (band).l,d3
                lsl.w   #3,d3
                lea     (a4,d3.w),a4
                move.w  (a4),d3
                move.w  2(a4),d6
                cmp.w   d3,d6
                bpl.w   shfr1
                exg     d3,d6

shfr1:
                move.w  d3,(bandl).l
                move.w  d6,(bandh).l
                swap    d6
                move.w  d3,d6

shfr:
                move.l  (a1)+,d3
                and.l   #$FFFF,d3
                lsr.l   #8,d3
                add.w   #1,d3
                swap    d5
                move.w  d5,d1
                sub.w   d3,d1
                move.w  #$8FC0,d4
                swap    d5
                move.w  #$3F,d7 ; '?'
                sub.w   d5,d7
                cmp.w   d6,d7
                blt.w   isgrn
                swap    d6
                cmp.w   d6,d7
                bgt.w   isgrn0
                move.w  #$F0C0,d4

isgrn0:
                swap    d6

isgrn:
                jsr     sblitblo
                add.w   #4,d0
                dbf     d5,shfr
                move.w  #$3C,d0 ; '<'
                move.w  (bandl).l,d1
                lsl.w   #2,d1
                add.w   d1,d0
                move.w  (bandh).l,d2
                lsl.w   #2,d2
                sub.w   d1,d2
                add.w   #3,d2
                move.w  #1,d3
                move.l  #$FFFF,d4
                lea     (envvals).l,a1
                clr.l   d6
                move.w  (band).l,d5
                lsl.w   #1,d5
                move.w  (a1,d5.w),d6
                lsr.w   #8,d6
                lsr.w   #1,d6
                move.w  (mony).l,d1
                add.w   #$10,d1
                sub.w   d6,d1
                bmi.w   rrts
                jsr     sblitblo
                move.w  #$3C,d0 ; '<'
                move.w  #$100,d2
                move.w  $04(a4),d6
                lsr.w   #8,d6
                lsr.w   #1,d6
                move.w  (mony).l,d1
                add.w   #$10,d1
                sub.w   d6,d1
                bmi.w   rrts
                move.w  #$88FF,d4
                jsr     sblitblo
                move.w  6(a4),d6
                lsr.w   #8,d6
                move.w  (mony).l,d1
                add.w   #$10,d1
                sub.w   d6,d1
                bmi.w   rrts
                move.w  #$4480,d4
                jsr     sblitblo
                move.w  #$96,d0
                move.w  #$78,d1 ; 'x'
                move.w  #$A,d2
                move.w  #$A,d3
                move.w  #0,d5

zizz:
                move.w  #$F0F0,d4
                move.l  (trig).l,d6
                btst    d5,d6
                beq.w   nggn
                move.w  #$8FF0,d4

nggn:
                bsr.w   sblitblo
                add.w   #$10,d0
                add.w   #1,d5
                cmp.w   #5,d5
                bne.s   zizz
                rts
; ---------------------------------------------------------------------------
shenv:
                move.l  #avbank,(_ein_buf).l
                move.l  #$1E80B4,(_ein_buf+4).l
                move.w  #9,(_fmode).l
                bsr.w   fmodewai
                rts
; ---------------------------------------------------------------------------
                movea.l (draw_scr).l,a0
                move.w  (mony).l,d5
                add.w   #$10,d5
                swap    d5
                move.w  #$50,d0 ; 'P'
                move.w  #$8FC0,d4
                move.w  #3,d2
                lea     (envvals).l,a1
                move.w  #7,d5

shar:
                move.w  (a1)+,d3
                lsr.w   #8,d3
                lsr.w   #2,d3
                add.w   #1,d3
                swap    d5
                move.w  d5,d1
                sub.w   d3,d1
                jsr     sblitblo
                add.w   #4,d0
                swap    d5
                dbf     d5,shar

; =============== S U B R O U T I N E =======================================


shad:
                tst.l   (aded).l
                beq.w   rrts
                move.l  (aded).l,(_ein_buf).l
                move.w  #1,(_fmode).l
                bsr.w   fmodewai
                movea.l (draw_scr).l,a0
                movea.l (aded).l,a1
                lea     (adsrcols).l,a2
                move.w  #$6E,d1 ; 'n'
                move.w  #$4C,d0 ; 'L'
                move.w  #7,d3
                move.w  #3,d5

shadsr:
                move.w  (a1)+,d2
                lsr.w   #8,d2
                add.w   #1,d2
                move.w  (a2)+,d4
                jsr     sblitblo
                add.w   #9,d1
                dbf     d5,shadsr
                rts
; End of function shad

; ---------------------------------------------------------------------------
adsrcols:     dc.b   0
                dc.b $FF
                dc.b $F0
                dc.b $FF
                dc.b $8F
                dc.b $FF
                dc.b $80
                dc.b $FF
; ---------------------------------------------------------------------------
; START OF FUNCTION CHUNK FOR wfm

joyxy:
                bne.w   joyy
                move.w  (pixcon).l,d2
                lsr.w   #2,d2
                and.w   #$3F,d2 ; '?'
                add.w   d2,d0
                move.w  #1,d2

zeb:
                move.w  #$88FF,d4
                movea.l (draw_scr).l,a0
                jmp     sblitblo
; ---------------------------------------------------------------------------

joyy:
                move.w  (piycon).l,d3
                lsr.w   #2,d3
                and.w   #$3F,d3 ; '?'
                add.w   #$3F,d1 ; '?'
                sub.w   d3,d1
                move.w  #1,d3
                move.w  #$40,d2 ; '@'
                bra.s   zeb
; END OF FUNCTION CHUNK FOR wfm
; ---------------------------------------------------------------------------
domonito:
                tst.w   (_m).l
                beq.w   rrts
                bmi.w   wfm
                move.l  (mon1).l,d1
                move.l  (mon2).l,d2
                bpl.w   a_1
                cmp.l   d1,d2
                beq.w   rrts

a_1:
                move.w  (monx).l,d0
                move.w  (mony).l,d1
                movea.l (draw_scr).l,a0
                sub.w   #$40,d1 ; '@'
                move.w  #$40,d2 ; '@'
                move.w  #$40,d3 ; '@'
                move.w  #$C0,d4
                jsr     sblitblo
                move.l  (mon1).l,d6
                bmi.w   nox1
                lea     UPDA1F(a6),a5
                lsl.w   #2,d6
                move.b  2(a5,d6.w),d5
                lsr.w   #3,d5
                and.w   #$1F,d5
                movem.w d0/d5,-(sp)
                add.w   #$20,d0 ; ' '
                sub.w   d5,d0
                move.w  #1,d2
                move.w  #$80C0,d4
                jsr     sblitblo
                movem.w (sp)+,d0/d5
                move.w  d0,-(sp)
                add.w   #$20,d0 ; ' '
                add.w   d5,d0
                jsr     sblitblo
                move.w  (sp)+,d0

nox1:
                move.l  (mon2).l,d6
                bmi.w   w2nope
                move.w  #$40,d2 ; '@'
                lea     UPDA1F(a6),a5
                lsl.w   #2,d6
                move.b  2(a5,d6.w),d5
                lsr.w   #3,d5
                and.w   #$1F,d5
                movem.w d1/d5,-(sp)
                add.w   #$20,d1 ; ' '
                sub.w   d5,d1
                move.w  #1,d3
                move.w  #$80C0,d4
                jsr     sblitblo
                movem.w (sp)+,d1/d5
                move.w  d1,-(sp)
                add.w   #$20,d1 ; ' '
                add.w   d5,d1
                jsr     sblitblo
                move.w  (sp)+,d1
                add.w   #$40,d1 ; '@'
                move.l  (draw_scr).l,(_ein_buf).l
                move.l  #$1E80F4,(_ein_buf+4).l
                swap    d1
                move.w  d0,d1
                move.l  d1,-(sp)
                move.l  d1,(_ein_buf+8).l
                move.w  #$FF00,d0
                swap    d0
                move.w  (monptr).l,d0
                move.l  d0,(dword_1A6810).l
                move.w  #8,(_fmode).l
                jsr     fmodewai
                move.l  (draw_scr).l,(_ein_buf).l
                move.l  #$1E8134,(_ein_buf+4).l
                move.l  (sp)+,d1
                move.l  d1,(_ein_buf+8).l
                move.w  #$8800,d0
                swap    d0
                move.w  (monptr).l,d0
                move.l  d0,(dword_1A6810).l
                move.w  #7,(_fmode).l
                bsr.w   fmodewai
                move.l  #$F03F54,(_ein_buf).l
                move.l  #$1E80F4,(_ein_buf+4).l
                bra.w   w2sngl

; =============== S U B R O U T I N E =======================================


fmodewai:
                                        ; ROM:001936B0↑p ...
                tst.w   (_fmode).l
                bne.s   fmodewai
                rts
; End of function fmodewai

; ---------------------------------------------------------------------------

w2nope:
                add.w   #$40,d1 ; '@'
                move.l  (draw_scr).l,(_ein_buf).l
                move.l  #$1E80F4,(_ein_buf+4).l
                swap    d1
                move.w  d0,d1

w2sngl:
                move.l  d1,(_ein_buf+8).l
                tst.l   (mon1).l
                bmi.w   rrts
                move.w  #$8800,d0
                swap    d0
                move.w  (monptr).l,d0
                move.l  d0,(dword_1A6810).l
                move.w  #6,(_fmode).l
                bra.s   fmodewai

; =============== S U B R O U T I N E =======================================


varadd:
                lsl.w   #3,d0
                move.w  d0,d2
                lsl.w   #2,d0
                add.w   d2,d0
                lea     (pbinfo).l,a0
                lea     word_197E3E-pbinfo(a0,d0.w),a0 ; "Parameter not yet defined    "
                rts
; End of function varadd

; ---------------------------------------------------------------------------

edvex:
                dc.l crot, snglx, sisnglx, sidbl, dvect, rrts, wsshow, shspec

dvect:
                bsr.w   avxy
                move.w  #8,d2
                lsr.w   d2,d0
                lsr.w   d2,d1
                sub.w   #$7F,d0
                sub.w   #$7F,d1
                add.w   #$BB,d0
                add.w   #$BB,d1
                move.w  #$A,d2
                move.w  #$A,d3
                move.w  #$80FF,d4
                movea.l (draw_scr).l,a0
                jmp     sblitblo
; ---------------------------------------------------------------------------
                move.w  d0,(X_2).l
                move.w  d1,(Y_2).l
                move.w  #$C0,(X_1).l
                move.w  #$C0,(Y_1).l
                move.w  #$88FF,(LINCOL).l
                move.l  (draw_scr).l,(_ein_buf).l
                move.l  #$1AEFB0,(_ein_buf+4).l
                move.w  #3,(_fmode).l
                jsr     fmodewai
                movem.w (sp)+,d0-d1
                asr.w   #1,d0
                asr.w   #1,d1
                move.w  d0,d2
                move.w  d1,d3
                asr.w   #1,d2
                asr.w   #1,d3
                add.w   d1,d2
                sub.w   d0,d3
                add.w   #$C0,d2
                add.w   #$C0,d3
                move.w  d2,(X_1).l
                move.w  d3,(Y_1).l
                move.w  d0,-(sp)
                move.w  #3,(_fmode).l
                jsr     fmodewai
                move.w  (sp)+,d0
                move.w  d0,d2
                move.w  d1,d3
                asr.w   #1,d2
                asr.w   #1,d3
                sub.w   d1,d2
                add.w   d0,d3
                add.w   #$C0,d2
                add.w   #$C0,d3
                move.w  d2,(X_1).l
                move.w  d3,(Y_1).l
                move.w  #3,(_fmode).l
                jsr     fmodewai
                rts
; ---------------------------------------------------------------------------
sidbl:
                bsr.w   avxy
                movem.l d0-d1,-(sp)
                move.w  #$3E,d0 ; '>'
                move.w  #$BD,d1
                move.w  #$102,d2
                move.w  #7,d3
                move.w  #$80FF,d4
                movea.l (draw_scr).l,a0
                jsr     sblitblo
                move.w  #$3E,d1 ; '>'
                move.w  #$BD,d0
                move.w  #$102,d3
                move.w  #7,d2
                move.w  #$80FF,d4
                movea.l (draw_scr).l,a0
                jsr     sblitblo
                move.l  (sp)+,d5
                move.w  #$C0,d0
                move.w  #$BF,d1
                move.w  #$FFFF,d4
                move.w  #3,d3
                lsr.w   #8,d5
                sub.w   #$7F,d5
                bpl.w   n1add5
                add.w   d5,d0
                neg.w   d5

n1add5:
                add.w   #1,d5
                move.w  d5,d2
                jsr     sblitblo
                move.l  (sp)+,d5
                move.w  #$C0,d1
                move.w  #$BF,d0
                move.w  #$FFFF,d4
                move.w  #3,d2
                lsr.w   #8,d5
                sub.w   #$7F,d5
                bpl.w   n2add5
                add.w   d5,d1
                neg.w   d5

n2add5:
                add.w   #1,d5
                move.w  d5,d3
                jmp     sblitblo
; ---------------------------------------------------------------------------
                bsr.w   avx
                move.l  d1,-(sp)
                move.w  #$3E,d0 ; '>'
                move.w  #$94,d1
                move.w  #$102,d2
                move.w  #$E,d3
                move.w  #$80FF,d4
                movea.l (draw_scr).l,a0
                jsr     sblitblo
                move.l  (sp)+,d5
                move.w  #$C0,d0
                move.w  #$96,d1
                move.w  #$FF00,d4
                move.w  #$A,d3
                lsr.w   #8,d5
                sub.w   #$7F,d5
                bpl.w   nadd5
                add.w   d5,d0
                neg.w   d5

nadd5:
                or.w    d5,d4
                add.w   #1,d5
                move.w  d5,d2
                jmp     sblitblo
; ---------------------------------------------------------------------------
snglx:
                bsr.w   avx
                lsr.l   #8,d1
                move.w  d1,-(sp)
                move.w  #$3E,d0 ; '>'
                move.w  #$94,d1
                move.w  #$102,d2
                move.w  #$E,d3
                move.w  #$80FF,d4
                movea.l (draw_scr).l,a0
                jsr     sblitblo
                move.w  #$40,d0 ; '@'
                move.w  #$FF00,d4
                move.w  (sp)+,d2
                or.w    d2,d4
                add.w   #1,d2
                move.w  #$96,d1
                move.w  #$A,d3
                jsr     sblitblo
                rts
; ---------------------------------------------------------------------------
wsshow:
                movea.l (edwave).l,a1
                movea.l (draw_scr).l,a0
                move.w  #$50,d0 ; 'P'
                move.w  #$71,d1 ; 'q'
                move.w  #7,d5

wssh:
                move.l  $04(a1),d2
                move.w  #$58FF,d4
                move.w  #3,d3
                rol.l   #3,d2
                clr.w   d2
                swap    d2
                add.w   #1,d2
                jsr     sblitblo
                move.w  (a1),d2
                and.w   #$FF,d2
                add.w   #1,d2
                move.w  #1,d3
                add.w   #1,d1
                move.w  #$80FF,d4
                jsr     sblitblo
                lea     vfb_ysca(a1),a1
                add.w   #8,d1
                dbf     d5,wssh
                bsr.w   wfm
                rts

; =============== S U B R O U T I N E =======================================


avx:
                                        ; ROM:00193B74↑p
                movea.l (eddie).l,a2
                movea.l (fxobj).l,a3
                bsr.w   lnkchk
                move.l  a2,(_ein_buf).l
                move.l  (iixcon).l,d1
                lsr.l   #8,d1
                and.l   #$FFFF,d1
                move.l  d1,(_ein_buf+4).l
                move.l  a3,(_ein_buf+8).l
                move.w  #2,(_fmode).l
                jmp     fmodewai
; End of function avx


; =============== S U B R O U T I N E =======================================


avxy:
                                        ; ROM:00193A80↑p ...
                movea.l (eddie).l,a2
                move.b  SRCEN(a2),d0
                and.w   #$FF,d0
                lsl.w   #3,d0
                move.w  d0,d1
                lsl.w   #2,d1
                add.w   d1,d0
                lea     (pbinfo).l,a1
                lea     word_197E3E-pbinfo(a1,d0.w),a1 ; "Parameter not yet defined    "
                movea.l (fxobj).l,a3
                bsr.w   lnkchk
                move.l  a2,(_ein_buf).l
                move.l  (iixcon).l,d1
                lsr.l   #8,d1
                and.l   #$FFFF,d1
                move.w  d1,-(sp)
                move.l  d1,(_ein_buf+4).l
                move.l  a3,(_ein_buf+8).l
                move.w  #2,(_fmode).l
                jsr     fmodewai
                movea.l (fxobj).l,a3
                bsr.w   lnkchk2
                move.l  a1,(_ein_buf).l
                move.l  (iiycon).l,d1
                lsr.l   #8,d1
                and.l   #$FFFF,d1
                move.l  d1,(_ein_buf+4).l
                move.l  a3,(_ein_buf+8).l
                move.w  #2,(_fmode).l
                jsr     fmodewai
                move.w  (sp)+,d0
                rts
; End of function avxy


; =============== S U B R O U T I N E =======================================


lnkchk2:
                move.w  $04(a1),d5
                bra.w   lnkch
; End of function lnkchk2


; =============== S U B R O U T I N E =======================================


lnkchk:
                                        ; sub_193C5C+26↑p
                move.w  $04(a2),d5

lnkch:
                lsl.w   #2,d5
                lea     UPDA1F(a3),a4
                tst.w   (a4,d5.w)
                beq.w   rrts
                lea     optionbu(a3),a3
                rts
; End of function lnkchk

; ---------------------------------------------------------------------------
crot:
                bsr.w   avxy
                move.w  $04(a2),d2
                move.w  $04(a1),d3
                lsl.w   #2,d2
                lsl.w   #2,d3
                move.w  (a3,d2.w),d0
                move.w  (a3,d3.w),d1
                move.w  d1,-(sp)
                tst.w   d0
                bmi.w   novrt
                cmp.w   #$180,d0
                bge.w   novrt
                move.w  #0,d1
                move.w  #1,d2
                move.w  #$180,d3
                move.w  #$88FF,d4
                movea.l (draw_scr).l,a0
                jsr     blitbloc

novrt:
                                        ; ROM:00193D36↑j
                move.w  (sp)+,d1
                bmi.w   nohrz
                cmp.w   #$180,d1
                bge.w   nohrz
                move.w  #0,d0
                move.w  #$180,d2
                move.w  #1,d3
                move.w  #$88FF,d4
                movea.l (draw_scr).l,a0
                jsr     blitbloc

nohrz:
                                        ; ROM:00193D60↑j
                rts
; ---------------------------------------------------------------------------
                movea.l (esp).l,a0
                move.l  #_thisfx,-(a0)
                move.l  a0,(esp).l

_thisfx:
                lea     (elspace).l,a0
                move.w  #5,(a0)+
                move.w  #0,(a0)+
                move.l  #bline2,(a0)+ ; "~g1:$20:Joypad to select, any FIRE to ed"...
                movea.l (fx).l,a1
                move.w  #5,d7

lfx:
                move.l  (a1)+,d0
                bne.w   listit
                move.l  #empt,(a0)+ ; "<Empty>"
                move.l  #initedit,(a0)+
                bra.w   lfx2
; ---------------------------------------------------------------------------

listit:
                movea.l d0,a2
                move.l  info(a2),d6
                sub.w   #1,d6
                lea     (vars).l,a3 ; "Draw a polygon object        "
                lsl.w   #2,d6
                movea.l (a3,d6.w),a3
                move.l  a3,(a0)+
                move.l  #edit2,(a0)+

lfx2:
                dbf     d7,lfx
                lea     (elspace).l,a1
                lea     (subfxhea).l,a0 ; "@~g1:SRCEN:Choose a subeffect slot to edit~"...
                bra.w   giedit
; ---------------------------------------------------------------------------
foredit:
                movea.l (esp).l,a0
                move.l  #fored,-(a0)
                move.l  a0,(esp).l

fored:
                                        ; ROM:fored↓o
                move.l  #fored,(ledit).l
                clr.w   (_m).l
                move.w  (fxed).l,d0
                movea.l (fx).l,a0
                lsl.w   #2,d0
                movea.l (a0,d0.w),a0
                move.l  info(a0),d6
                lea     (vars).l,a1 ; "Draw a polygon object        "
                sub.w   #1,d6
                lsl.w   #2,d6
                movea.l (a1,d6.w),a1
                lea     $1E(a1),a1
                lea     (wavedhea).l,a0 ; "@~g1:SRCEN:Editing: Wave Plotter~e3:3:"
gie:
                move.l  a0,-(sp)
                lea     (elspace).l,a0
                move.w  #$10,d0
                bsr.w   elcon
                lea     (elspace).l,a1
                movea.l (sp)+,a0
                bra.w   giedit
; ---------------------------------------------------------------------------
ahead2:
                movea.l (esp).l,a0
                move.l  #_ded2,-(a0)
                move.l  a0,(esp).l

_ded2:
                lea     (adsra).l,a0
                move.w  (selected).l,d0
                move.w  d0,d1
                lsl.w   #3,d0
                lsl.w   #1,d1
                add.w   d1,d0
                lea     (a0,d0.w),a0
                move.l  a0,(aded).l
                lea     (option6).l,a1
                lea     (adedhead2).l,a0
                bsr.w   giedit
                move.w  #$B,(editing).l
                rts
; ---------------------------------------------------------------------------
; START OF FUNCTION CHUNK FOR makecfb

ispec:
                move.l  #isp1,(ledit).l
                movea.l (esp).l,a0
                move.l  #isp1,-(a0)
                move.l  a0,(esp).l

isp1:
                                        ; sub_1924DA+19E8↑o
                move.w  #8,(symed).l
                movea.l #isphead1,a0  ; "@~g1:SRCEN:Spectrum and Triggers~e3:3:"
                movea.l #option7,a1
                bsr.w   giedit
                move.w  #$E,(editing).l
                rts
; END OF FUNCTION CHUNK FOR makecfb
; ---------------------------------------------------------------------------
ispec2:
                move.l  #isp2,(ledit).l
                movea.l (esp).l,a0
                move.l  #isp2,-(a0)
                move.l  a0,(esp).l

isp2:
                                        ; ROM:00193F00↑o
                move.w  #8,(symed).l
                movea.l #isphead2,a0  ; "@~g1:SRCEN:Trigger Settings~e3:3:"
                movea.l #option8,a1
                bsr.w   giedit
                rts
; ---------------------------------------------------------------------------
ispec3:
                move.l  #isp3,(ledit).l
                movea.l (esp).l,a0
                move.l  #isp3,-(a0)
                move.l  a0,(esp).l

isp3:
                                        ; ROM:00193F36↑o
                lea     (isphead3).l,a0 ; "@~g1:SRCEN:Adjust width using joypad"
                bsr.w   print
                move.w  #$F,(editing).l
                rts
; ---------------------------------------------------------------------------
ispec4:
                move.l  #isp4,(ledit).l
                movea.l (esp).l,a0
                move.l  #isp4,-(a0)
                move.l  a0,(esp).l

isp4:
                                        ; ROM:00193F66↑o
                lea     (isphead4).l,a0
                bsr.w   print
                move.w  #$10,(editing).l
                rts
; ---------------------------------------------------------------------------
wsedit:
                move.l  #wsed,(ledit).l
                movea.l (esp).l,a0
                move.l  #wsed,-(a0)
                move.l  a0,(esp).l

wsed:
                                        ; ROM:00193F96↑o
                movea.l (fxedbase).l,a0
                lea     $300(a0),a0
                move.l  a0,(edwave).l
                move.w  #7,(symed).l
                lea     (option4).l,a1
                lea     (wshead).l,a0 ; "@~g1:SRCEN:Edit basic waveforms~e3:3:"
                bsr.w   giedit
                move.l  #wtud,(action).l
                move.w  #$A,(editing).l
                rts
; ---------------------------------------------------------------------------
dvfedit:
                move.l  #dvfed,(ledit).l
                movea.l (esp).l,a0
                move.l  #dvfed,-(a0)
                move.l  a0,(esp).l

dvfed:
                                        ; ROM:00193FEE↑o
                lea     (dvfvars).l,a1
                clr.w   (_m).l
                lea     (elspace).l,a0
                move.w  #$10,d0
                bsr.w   elcon
                lea     (elspace).l,a1
                lea     (dvfedhea).l,a0
                bra.w   giedit
; ---------------------------------------------------------------------------
iiiogo:
                move.w  (selected).l,(og).l
                bra.w   *+4
; ---------------------------------------------------------------------------

inogg:
                move.l  #iogo,(ledit).l
                movea.l (esp).l,a0
                move.l  #iogo,-(a0)
                move.l  a0,(esp).l

iogo:
                                        ; ROM:00194042↑o
                lea     (fxphead).l,a0 ; "@~g1:SRCEN:Choose fx page~e3:3:"
                lea     (fxopt).l,a1
                bra.w   giedit
; ---------------------------------------------------------------------------
setp1:
                move.l  #ssp1,(ledit).l
                movea.l (esp).l,a0
                move.l  #ssp1,-(a0)
                move.l  a0,(esp).l

ssp1:
                                        ; ROM:0019406E↑o
                lea     (availobj).l,a1
                bra.w   doggo
; ---------------------------------------------------------------------------
setp2:
                move.l  #ssp2,(ledit).l
                movea.l (esp).l,a0
                move.l  #ssp2,-(a0)
                move.l  a0,(esp).l

ssp2:
                                        ; ROM:00194094↑o
                lea     (avail2).l,a1

doggo:
                lea     (ogohead).l,a0 ; "@~g1:SRCEN:Object Giver Outer~e3:3:"
                bra.w   giedit
; ---------------------------------------------------------------------------
symedit:
                move.l  #simedit,(ledit).l
                movea.l (esp).l,a0
                move.l  #simedit,-(a0)
                move.l  a0,(esp).l

simedit:
                                        ; ROM:001940C0↑o
                clr.w   (_m).l
                lea     (symvars).l,a1
                lea     (elspace).l,a0
                move.w  #$10,d0
                bsr.w   elcon
                movea.l #$1E7A54,a1
                lea     (symedhea).l,a0 ; "@~g1:SRCEN:Editing: Symmetry Generator~e3:3"...
                bra.w   giedit
; ---------------------------------------------------------------------------

edit2:
                movea.l (esp).l,a0
                move.l  #_edit2,-(a0)
                move.l  a0,(esp).l
                move.w  (selected).l,d0
                move.w  d0,(fxed).l
                move.w  d0,(og).l

_edit2:
                move.w  (fxed).l,d0
                lsl.w   #2,d0
                movea.l (fx).l,a0
                move.l  (a0,d0.w),(fxedbase).l
                lea     (edit2hea).l,a0
                lea     (option2).l,a1
                bra.w   giedit
; ---------------------------------------------------------------------------
ahead1:
                movea.l (esp).l,a0
                move.l  #_ah1,-(a0)
                move.l  a0,(esp).l

_ah1:
                lea     (adedhead).l,a0
                clr.l   (aded).l
                lea     (option5).l,a1
                bra.w   giedit
; ---------------------------------------------------------------------------
compr:
                movea.l (esp).l,a0
                move.l  #_cmp1,-(a0)
                move.l  a0,(esp).l

_cmp1:
                lea     (stashhea).l,a0
                jsr     print
                bsr.w   dostash
                move.w  #$D,(editing).l
                rts
; ---------------------------------------------------------------------------
rset:
                movea.l (esp).l,a0
                move.l  #_rset,-(a0)
                move.l  a0,(esp).l

_rset:
                lea     (rsethead).l,a0 ; "@~g1:SRCEN:Reset ROM save pointers//Press a"...
                jsr     print
                move.l  #0,(datn).l
                move.l  #$900200,(datp).l
                move.l  #$900010,(dattp).l
                move.w  #$D,(editing).l
                rts
; ---------------------------------------------------------------------------
mset:
                movea.l (esp).l,a0
                move.l  #_mset,-(a0)
                move.l  a0,(esp).l

_mset:
                bsr.w   getmatri
                move.w  #$D,(editing).l
                rts

; =============== S U B R O U T I N E =======================================


dostash:
                lea     ((asc_197BE8+2)).l,a0 ; "~g1:6:"
                jsr     print
                movea.l (dattp).l,a0
                move.l  (datp).l,d0
                move.l  d0,(a0)+
                move.l  a0,(dattp).l
                addi.l  #1,(datn).l
                move.w  #1,(cursx).l
                lea     (tp1).l,a0
                jsr     print
                lea     (refblock).l,a0
                clr.l   d6
                move.w  #$17FF,d1
                movea.l (datp).l,a2
                lea     (matrix).l,a1
                move.l  a1,(cskr).l
                movem.l a0-a1,-(sp)
                bsr.w   deltablo
                move.l  d0,($1AE790).l
                movem.l (sp)+,a0-a1
                move.l  a2,(datp).l
                lea     ($1AE546).l,a4
                jsr     xxnum
                movea.l a4,a0
                jsr     print
                lea     (tp2).l,a0
                jsr     print
                move.w  #7,d7
                move.l  #$1D9BF8,($1AE78C).l

remblox:
                move.w  d7,-(sp)
                move.w  #1,(cursx).l
                lea     (tp1).l,a0
                jsr     print
                movea.l (cskr).l,a0
                movea.l ($1AE78C).l,a1
                move.w  #$17FF,d1
                movea.l (datp).l,a2
                movem.l a0-a1,-(sp)
                bsr.w   deltablo
                add.l   d0,($1AE790).l
                movem.l (sp)+,a0-a1
                move.l  ($1AE78C).l,(cskr).l
                addi.l  #$1800,($1AE78C).l
                move.l  a2,(datp).l
                lea     ($1AE546).l,a4
                jsr     xxnum
                movea.l a4,a0
                jsr     print
                lea     (tp2).l,a0
                jsr     print
                move.w  (sp)+,d7
                dbf     d7,remblox
                move.w  #1,(cursx).l
                lea     (tp3).l,a0
                jsr     print
                move.l  ($1AE790).l,d0
                lea     ($1AE546).l,a4
                jsr     xxnum
                movea.l a4,a0
                jsr     print
                lea     (tp2).l,a0
                jsr     print
                rts
; End of function dostash

; ---------------------------------------------------------------------------
edline:
                movea.l (esp).l,a0
                move.l  #_edl,-(a0)
                move.l  a0,(esp).l

_edl:
                move.w  (delayf).l,d0
                bpl.w   eddd
                clr.w   d0

eddd:
                move.w  d0,(word_19B08A).l
                move.l  #dlset,(editlist).l
                lea     (edlhead).l,a0
                jsr     print
                move.w  #$C,(editing).l

udedg:
                lea     ($199C14).l,a0
                lea     asc_199C14+6-asc_199C14(a0),a1 ; "~g3:5:                                 "
                move.w  #$1F,d0

udedg1:
                move.b  #1,(a1)+
                dbf     d0,udedg1
                lea     6(a0),a1
                move.w  (delayt).l,d1
                move.w  (delayn).l,d2
                clr.w   d0

shado:
                add.w   d1,d0
                bsr.w   setblok
                dbf     d2,shado
                bra.w   print

; =============== S U B R O U T I N E =======================================


setblok:
                and.w   #$3F,d0 ; '?'
                move.w  d0,d3
                lsr.w   #1,d3
                lea     (a1,d3.w),a2
                move.b  (a2),d4
                sub.w   #1,d4
                btst    #0,d0
                beq.w   d0notodd
                add.w   #4,d4

d0notodd:
                lea     (evens).l,a3
                and.w   #7,d4
                move.b  (a3,d4.w),(a2)
                rts
; End of function setblok


; =============== S U B R O U T I N E =======================================


evens:

; FUNCTION CHUNK AT 0019528A SIZE 0000008C BYTES
; FUNCTION CHUNK AT 001960AA SIZE 0000001A BYTES

                dc.b $02,$02,$04,$04
                btst    d1,d4
                btst    d1,d4

wfa:
                bsr.w   estack
                movea.l (eddie).l,a2
                btst    #0,(a2)
                bne.w   rwfa
                clr.w   (symed).l
                bra.w   iawfx
; ---------------------------------------------------------------------------

rwfa:

                lea     (wfahead).l,a0
                lea     (option3).l,a1
                clr.w   (symed).l
                move.w  #1,(cbuttf).l
                bra.w   ogiedit
; End of function evens

; ---------------------------------------------------------------------------
; START OF FUNCTION CHUNK FOR makecfb

initedit:

                move.l  #$1E5CFC,(esp).l
                move.l  #0,(e_stac).l
                clr.w   (sec_cnt).l
                move.w  #1,(sec_en).l
                bra.w   ispec
; END OF FUNCTION CHUNK FOR makecfb

; =============== S U B R O U T I N E =======================================


old_edit:
                move.l  #$1E5CFC,(esp).l
                move.l  #0,(e_stac).l
                clr.w   (sec_en).l
                movea.l (esp).l,a0
                move.l  #_iied,-(a0)
                move.l  a0,(esp).l

_iied:
                lea     (edithead).l,a0 ; "@~g1:SRCEN:Edit Mode~e3:$04:"
                lea     (option1).l,a1
                clr.w   (symed).l
; End of function old_edit


; =============== S U B R O U T I N E =======================================


giedit:
                                        ; ROM:00193E64↑j ...
                clr.w   (cbuttf).l

ogiedit:
                move.w  #$FFFF,(actime).l
                move.l  a1,(editlist).l
                bsr.w   print
                move.w  #1,(editing).l
                rts
; End of function giedit


; =============== S U B R O U T I N E =======================================


wtud:
                                        ; ROM:00195774↓o ...
                lea     (wt).l,a0 ; "~g2:12:"
                jsr     print
                move.w  (selected).l,d0
                lsl.w   #4,d0
                movea.l (edwave).l,a1
                lea     (a1,d0.w),a1
                move.w  $E(a1),d0
                lsl.w   #2,d0
                lea     (wts).l,a2
                movea.l (a2,d0.w),a0
                cmp.w   #$18,d0
                beq.w   grint
                move.l  $24(a2,d0.w),$08(a1)

grint:
                jsr     print

ud_selcu:
                move.w  (cursorx).l,d0
                move.w  (cursory).l,d1
                move.b  #$20,d2 ; ' '
                lea     (board).l,a0
                jsr     charblit
                move.w  (selx).l,d0
                move.w  (sely).l,d1
                add.w   (selected).l,d1
                sub.w   #1,d0
                move.w  d0,(cursorx).l
                move.w  d1,(cursory).l
                clr.b   d2
                jmp     charblit
; End of function wtud


; =============== S U B R O U T I N E =======================================


reedit:
                bsr.w   eunstack
                bra.w   eparam
; End of function reedit


; =============== S U B R O U T I N E =======================================


eunstack:
                movea.l (esp).l,a0
                move.l  (a0)+,(editlist).l
                move.w  (a0)+,(selected).l
                move.w  (a0)+,(selectab).l
                move.l  a0,(esp).l
                rts
; End of function eunstack


; =============== S U B R O U T I N E =======================================


estack:
                movea.l (esp).l,a0
                move.w  (selectab).l,-(a0)
                move.w  (selected).l,-(a0)
                move.l  (editlist).l,-(a0)
                move.l  a0,(esp).l
                rts
; End of function estack

; ---------------------------------------------------------------------------
; START OF FUNCTION CHUNK FOR reedit

eparam:

                clr.l   (dword_19B054).l
                clr.l   (dword_19B070).l
                move.w  (selected).l,d0
                lsl.w   #2,d0
                lea     (elvex).l,a0
                movea.l (a0,d0.w),a0
                move.l  a0,(eddie).l
                move.b  2(a0),d0
                and.w   #$FF,d0
                cmp.w   #7,d0
                beq.w   init_byt
                cmp.w   #6,d0
                bne.w   crunt
                movea.l 6(a0),a1
                jmp     (a1)
; ---------------------------------------------------------------------------

init_byt:
                lea     (awfb2).l,a0
                lea     (padbits).l,a1
                move.l  _mtrig(a6),d2
                move.w  #7,d7

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
                move.l  #(byte_196DEF+$3D),(cpad).l
                move.l  #awfb2,($1AF056).l
                move.w  #6,(symed).l
                move.w  #7,(editing).l
                lea     (bytemask).l,a0
                bsr.w   cprint
                bra.w   ud_butts
; END OF FUNCTION CHUNK FOR reedit

; =============== S U B R O U T I N E =======================================


init_sym:
                                        ; ROM:00198754↓o
                bsr.w   isymbut
                move.w  d0,(symed).l
                add.w   #1,d0
                move.w  d0,(editing).l
                lea     (symplane).l,a0 ; "Editing: Symmetry Planes and Types//Pre"...
                bsr.w   cprint
                bsr.w   ppmes
                bra.w   ud_butts
; End of function init_sym

; ---------------------------------------------------------------------------
; START OF FUNCTION CHUNK FOR reedit

crunt:
                move.w  d0,(symed).l
                move.w  d0,d1
                sub.w   #1,d1
                add.w   #1,d0
                move.w  d0,(editing).l
                bsr.w   primexy
                move.l  a0,-(sp)
                lea     (eparm0).l,a0
                bsr.w   cprint
                movea.l (sp)+,a0
                lea     -$1E(a0),a0
                bsr.w   print
                lea     (eparm2).l,a0 ; "~g1:$18:Press ~i+*~i- to attach waveform"...
                bsr.w   print
; END OF FUNCTION CHUNK FOR reedit

; =============== S U B R O U T I N E =======================================


ppmes:
                                        ; ROM:0019533E↓p
                movea.l #eparm1,a0  ; "~g1:$20:<A> Prev   <B> Menu   <C> Next"
                bra.w   print
; End of function ppmes


; =============== S U B R O U T I N E =======================================


isymbut:
                                        ; sub_194648↑p
                lea     (symbutts).l,a0
                lea     (padbits).l,a1
                move.l  asym_fla(a6),d2
                move.w  #7,d7

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
                lea     vfb_angl(a0),a2
                bclr    #7,2(a2)
                btst    #8,d2
                beq.w   idun
                bset    #7,2(a2)

idun:
                move.l  #sympad,(cpad).l
                move.l  #symbutts,($1AF056).l
                rts
; End of function isymbut


; =============== S U B R O U T I N E =======================================


primexy:
                move.w  (selected).l,(lselecte).l
                movea.l (fxobj).l,a3
                move.w  $04(a0),d2
                move.w  d2,(monitor).l
                addi.w  #1,(monitor).l
                move.w  #1,(_m).l
                move.l  6(a0),d3
                lea     (ixcon).l,a2
                bsr.w   s16
                movea.l (fxobj).l,a3
                btst    #0,(a0)
                beq.w   rrts
                move.b  SRCEN(a0),d0
                and.w   #$FF,d0
                lsl.w   #3,d0
                move.w  d0,d2
                lsl.w   #2,d0
                add.w   d2,d0
                lea     (pbinfo).l,a0
                lea     word_197E3E-pbinfo(a0,d0.w),a0 ; "Parameter not yet defined    "
                move.w  $04(a0),d2
                move.l  6(a0),d3
                lea     (iycon).l,a2
; End of function primexy


; =============== S U B R O U T I N E =======================================


s16:
                move.w  $04(a0),d0
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
                lea     (vtypes).l,a3
                movea.l (a3,d0.w),a3
                jmp     (a3)
; End of function s16

; ---------------------------------------------------------------------------

vtypes:
                dc.l $1947da, $1947dc, $1947ec 
                rts
; ---------------------------------------------------------------------------
xword:
                swap    d7
                divu.w  d3,d7
zach:
                and.l   #$FFFF,d7
                lsl.l   #8,d7
                move.l  d7,(a2)
                rts
; ---------------------------------------------------------------------------
x1616:
                lsr.l   #8,d3
                lsr.l   #4,d3
                divu.w  d3,d7
                lsl.l   #8,d7
                lsl.l   #4,d7
                and.l   #$FFFFFF,d7
                move.l  d7,(a2)
                rts

; =============== S U B R O U T I N E =======================================


unquit:
                lea     (unimp).l,a0 ; "This function not yet implemented~c30:"
                bra.w   eq
; ---------------------------------------------------------------------------

editquit:

                lea     (normal).l,a0

eq:
                bsr.w   cprint
                clr.w   (editing).l
                clr.l   (star_on).l
                move.w  (ovlm_mod).l,(vlm_mode).l
                rts
; End of function unquit


; =============== S U B R O U T I N E =======================================


cprint:
                                        ; sub_194648+1A↑p ...
                move.l  a0,-(sp)
                lea     (clearhom).l,a0
                bsr.w   print
                movea.l (sp)+,a0
; End of function cprint


; =============== S U B R O U T I N E =======================================


print:
                                        ; sub_192E82+1C↑p ...
                movea.l a0,a4
                movea.l #$1B00F8,a0
                move.w  (cursx).l,d0
                move.w  (cursy).l,d1
; End of function print


; =============== S U B R O U T I N E =======================================


prnt:
                                        ; sub_19484E+36↓j ...
                move.b  (a4)+,d2
                beq.w   strngend
                cmp.b   #$2F,d2 ; '/'
                bne.w   pcmd1
                add.w   #1,d1
                move.w  (cursx).l,d0
                bra.s   prnt
; ---------------------------------------------------------------------------

pcmd1:
                cmp.b   #$40,d2 ; '@'
                bne.w   pcmd2
                lea     (board).l,a0
                movem.w d0-d1,-(sp)
                jsr     cleol
                movem.w (sp)+,d0-d1
                bra.s   prnt
; ---------------------------------------------------------------------------

pcmd2:
                cmp.b   #$5E,d2 ; '^'
                bne.w   pcmd3
                jsr     cleol
                bra.s   prnt
; ---------------------------------------------------------------------------

pcmd3:
                cmp.b   #$60,d2 ; '`'
                bne.w   pcmd4
                sub.w   #1,d1
                move.w  (cursx).l,d0
                bra.s   prnt
; ---------------------------------------------------------------------------

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
; ---------------------------------------------------------------------------

setinv:
                move.b  (a4)+,d2
                cmp.b   #$2B,d2 ; '+'
                beq.w   invon
                clr.w   (inverse).l
                bra.w   prnt
; ---------------------------------------------------------------------------

invon:
                move.w  #1,(inverse).l
                bra.w   prnt
; ---------------------------------------------------------------------------

edtable:
                movea.l (editlist).l,a5
                move.l  a4,-(sp)
                movea.l $04(a5),a4
                bsr.w   prnt
                movea.l (sp)+,a4
                bsr.w   gnum
                move.w  d2,d0
                move.w  d2,(selx).l
                bsr.w   gnum
                move.w  d2,d1
                move.w  d2,(sely).l
                move.l  a4,-(sp)
                movea.l (editlist).l,a5
                move.w  (a5)+,d3
                move.w  d3,(selectab).l
                move.w  (a5)+,(selected).l
                lea     $04(a5),a5

dedtab:
                movea.l (a5)+,a4
                lea     $04(a5),a5
                move.w  d3,-(sp)
                bsr.w   prnt
                move.w  (sp)+,d3
                add.w   #1,d1
                move.w  (selx).l,d0
                dbf     d3,dedtab
                clr.w   d2
                sub.w   #1,d0
                move.w  (selected).l,d1
                add.w   (sely).l,d1
                move.w  d0,(cursorx).l
                move.w  d1,(cursory).l
                jsr     charblit
                movea.l (sp)+,a4
                bra.w   prnt
; ---------------------------------------------------------------------------

goto:
                bsr.w   gnum
                move.w  d2,d0
                bsr.w   gnum
                move.w  d2,d1
                bra.w   prnt
; ---------------------------------------------------------------------------

aclear:
                bsr.w   gnum
                move.w  d2,(actime).l
                bra.w   prnt
; ---------------------------------------------------------------------------

pcmd5:
                jsr     charblit
                add.w   #1,d0
                bra.w   prnt
; ---------------------------------------------------------------------------

strngend:
                move.w  d0,(cursx).l
                move.w  d1,(cursy).l
                rts
; End of function prnt


; =============== S U B R O U T I N E =======================================


gnum:
                                        ; sub_19484E+C8↑p ...
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
; End of function gnum

; ---------------------------------------------------------------------------

dodraw:
                move.l  #1,(screen_r).l
                move.w  #1,(db_on).l
                clr.w   (x_end).l

gog:
                bsr.w   thangg
                tst.w   (meltim).l
                bpl.s   gog
                move.l  #rrts,(routine).l
                rts

; =============== S U B R O U T I N E =======================================


db:
                move.l  #1,(sync).l

dboo:
                tst.l   (sync).l
                bne.s   dboo
                move.l  (dscreen).l,(draw_scr).l
                rts
; End of function db

; ---------------------------------------------------------------------------
                move.l  #mymelt,(draw_rou).l
                move.w  #$3C,d0 ; '<'
                move.w  (skale).l,d1
                lsl.w   d1,d0
                move.w  d0,(meltim).l
                bra.s   dodraw
; ---------------------------------------------------------------------------

mymelt:
                subi.w  #1,(meltim).l
                bsr.w   WaitBlit
                movea.l (cscreen).l,a0
                movea.l (dscreen).l,a1
                tst.w   (skale).l
                bne.w   lilmelt
                move.l  a1,(A2_BASE).l
                move.l  #$14420,(A2_FLAGS).l
                move.w  #$10,d0
                swap    d0
                move.w  #$10,d0
                move.l  d0,(A2_PIXEL).l
                move.w  #1,d0
                swap    d0
                move.w  #$FEA0,d0
                move.l  d0,(A2_STEP).l
                move.l  a0,(A1_BASE).l
                move.l  #$34420,(A1_FLAGS).l
                move.w  #$16,d0
                swap    d0
                move.w  #$16,d0
                move.l  d0,(A1_PIXEL).l
                move.l  #0,(A1_INC).l
                move.l  #$F800,(A1_FINC).l
                move.w  #0,d0
                swap    d0
                move.w  #$FEAB,d0
                move.l  d0,(A1_STEP).l
                move.l  #$F8000000,(A1_FSTEP).l
                move.l  #$FC0000,d0
                move.l  d0,(B_IINC).l
                move.l  d0,($F02274).l
                move.l  d0,($F02278).l
                move.l  d0,($F0227C).l
                move.w  #$160,d0
                swap    d0
                move.w  #$160,d0
                move.l  d0,(B_COUNT).l
                move.l  #$41802F01,(B_CMD).l
                bra.w   WaitBlit
; ---------------------------------------------------------------------------

lilmelt:
                cmpi.w  #2,(skale).l
                beq.w   justfade
                move.l  a1,(A2_BASE).l
                move.l  #$14420,(A2_FLAGS).l
                move.w  #0,d0
                swap    d0
                move.w  #0,d0
                move.l  d0,(A2_PIXEL).l
                move.w  #1,d0
                swap    d0
                move.w  #$FF40,d0
                move.l  d0,(A2_STEP).l
                move.l  a0,(A1_BASE).l
                move.l  #$34420,(A1_FLAGS).l
                move.w  #3,d0
                swap    d0
                move.w  #4,d0
                move.l  d0,(A1_PIXEL).l
                move.l  #0,(A1_FPIXE).l
                move.l  #0,(A1_INC).l
                move.l  #$F800,(A1_FINC).l
                move.w  #0,d0
                swap    d0
                move.w  #$FF46,d0
                move.l  d0,(A1_STEP).l
                move.l  #$F8000000,(A1_FSTEP).l
                move.l  #$FE0000,d0
                move.l  d0,(B_IINC).l
                move.l  d0,($F02274).l
                move.l  d0,($F02278).l
                move.l  d0,($F0227C).l
                move.w  #$C0,d0
                swap    d0
                move.w  #$C0,d0
                move.l  d0,(B_COUNT).l
                move.l  #$41802F01,(B_CMD).l
                bra.w   WaitBlit
; ---------------------------------------------------------------------------

justfade:
                move.l  a1,(A2_BASE).l
                move.l  #$4420,(A2_FLAGS).l
                move.w  #0,d0
                swap    d0
                move.w  #0,d0
                move.l  d0,(A2_PIXEL).l
                move.w  #1,d0
                swap    d0
                move.w  #$FE80,d0
                move.l  d0,(A2_STEP).l
                move.l  a0,(A1_BASE).l
                move.l  #$4420,(A1_FLAGS).l
                move.w  #0,d0
                swap    d0
                move.w  #0,d0
                move.l  d0,(A1_PIXEL).l
                move.w  #1,d0
                swap    d0
                move.w  #$FE80,d0
                move.l  d0,(A1_STEP).l
                move.l  #$FE0000,d0
                move.l  d0,(B_IINC).l
                move.l  d0,($F02274).l
                move.l  d0,($F02278).l
                move.l  d0,($F0227C).l
                move.w  #$180,d0
                swap    d0
                move.w  #$180,d0
                move.l  d0,(B_COUNT).l
                move.l  #$41802E01,(B_CMD).l
                bra.w   WaitBlit
                rts

; =============== S U B R O U T I N E =======================================


Frame:
                movem.l d0-d5/a0-a2,-(sp)
                move.w  (INT1).l,d0
                move.w  d0,-(sp)
                btst    #0,d0
                beq.w   CheckTim
                addi.w  #1,(frames).l
                tst.w   (clut_sha).l
                beq.w   ncc
                move.w  (clut_sha).l,($F00482).l
                clr.w   (clut_sha).l

ncc:
                movem.l d6-d7/a3-a6,-(sp)
                movea.l (blist).l,a0
                movea.l (dlist).l,a1
                moveq   #$40,d0 ; '@'

xlst:
                move.l  (a0)+,(a1)+
                dbf     d0,xlst
                bsr.w   RunBeast
                tst.l   (screen_r).l
                beq.w   no_new_s
                tst.l   (sync).l
                beq.w   no_new_s
                move.l  (cscreen).l,d1
                move.l  (dscreen).l,(cscreen).l
                move.l  d1,(dscreen).l
                move.l  d1,(draw_scr).l
                clr.l   (screen_r).l
                clr.l   (sync).l

no_new_s:
                                        ; Frame+62↑j
                move.l  (cscreen).l,d5
                movea.l (dlist).l,a0
                move.w  (db_on).l,d7
                bmi.w   no_db
                tst.w   (scron).l
                beq.w   stdb
                bpl.w   no_db
                clr.w   (scron).l
                bra.w   no_db
; ---------------------------------------------------------------------------

stdb:
                                        ; Frame+E2↓j
                move.l  d5,d6
                add.l   (hango).l,d6
                and.l   #$FFFFFFF8,d6
                lsl.l   #8,d6
                move.l  (a0),d1
                and.l   #$7FF,d1
                or.l    d6,d1
                move.l  d1,(a0)
                lea     d_z(a0),a0
                add.l   (dbadj).l,d5
                dbf     d7,stdb

no_db:
                                        ; Frame+AE↑j ...
                tst.w   (flash).l
                beq.w   cflash
                subi.w  #$10,(flash).l
                move.w  (flash).l,d0
                or.w    #$7700,d0
                move.w  d0,d1
                swap    d0
                move.w  d1,d0
                move.l  d0,(BG).l
                move.l  d0,($F0005C).l
                tst.w   (flash).l
                bne.w   cflash
                move.w  #$FFFF,(scron).l

cflash:
                                        ; Frame+11A↑j
                cmpi.w  #1,(vlm_mode).l
                bne.w   dflash
                move.l  (pad_now).l,d0
                and.l   #$2000,d0
                beq.w   dflash
                tst.w   (flash_db).l
                bne.w   eflash
                move.w  #1,(flash_db).l
                move.w  #$100,(flash).l
                move.w  #1,(scron).l
                bra.w   eflash
; ---------------------------------------------------------------------------

dflash:
                                        ; Frame+13E↑j
                clr.w   (flash_db).l

eflash:
                                        ; Frame+164↑j
                tst.w   (vlmtim).l
                bmi.w   ncanc
                subi.w  #1,(vlmtim).l
                bpl.w   ncanc
                move.w  #$FFFF,($1AE214).l

ncanc:
                                        ; Frame+180↑j
                jsr     readpad
                tst.w   (seldb).l
                beq.w   do_ed
                move.l  (pad_now).l,d1
                or.l    ($1AE010).l,d1
                and.l   #$22FE20FF,d1
                bne.w   no_ksel
                clr.w   (seldb).l

do_ed:
                tst.w   (vlm_mode).l
                beq.w   no_ed
                move.w  (editing).l,d0
                beq.w   no_ed
                tst.l   (action).l
                bne.w   no_ksel
                subq.w  #1,d0
                lea     (editvex).l,a0
                lsl.w   #2,d0
                movea.l (a0,d0.w),a0
                jsr     (a0)
                bra.w   no_ksel
; ---------------------------------------------------------------------------

no_ed:
                                        ; Frame+1C8↑j
                tst.l   (action).l
                bne.w   no_ksel
                move.l  (pad_now).l,d0
                and.l   #$200,d0
                bra.w   nothash
; ---------------------------------------------------------------------------
                move.w  #1,(seldb).l
                move.w  #1,(vlm_mode).l
                move.w  #1,($1AE0D4).l
                move.w  #7,($1AE214).l
                move.w  #$1F4,(vlmtim).l

nothash:
                tst.w   (editing).l
                bne.w   no_ksel
                move.l  (pad_now).l,d0
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
; ---------------------------------------------------------------------------

pharty:
                move.w  ($1AE026).l,(star_on).l
                move.w  d2,($1AE026).l
                move.w  #1,(seldb).l
                bra.w   no_ksel
; ---------------------------------------------------------------------------

knobby:
                tst.l   (star_on).l
                beq.w   no_ksel
                move.w  (star_on).l,d0
                bne.w   setbth
                move.w  ($1AE026).l,(skid).l
                move.l  #skidoo,(action).l
                bra.w   n_ks
; ---------------------------------------------------------------------------

setbth:
                sub.w   #1,d0
                move.w  d0,(imatrix).l
                move.w  ($1AE026).l,(skid).l
                move.l  #gm,(action).l

n_ks:
                clr.l   (star_on).l

no_ksel:
                                        ; Frame+1D2↑j ...
                move.l  (pad_now).l,d0
                cmp.l   #$90018,d0
                bne.w   nse1
                jsr     setedit
                bra.w   nse2
; ---------------------------------------------------------------------------

nse1:
                clr.w   (vedit).l

nse2:
                movea.l (_fx).l,a0
                jsr     (a0)
                tst.l   (action).l
                bne.w   gharbaj
                movea.l (routine).l,a0
                jsr     (a0)

gharbaj:
                movem.l (sp)+,d6-d7/a3-a6

CheckTim:
                move.w  (sp)+,d0
                move.w  d0,-(sp)
                btst    #3,d0
                beq.w   *+4

exxit:
                move.w  (sp)+,d0
                lsl.w   #8,d0
                move.b  (intmask).l,d0
                move.w  d0,(INT1).l
                move.w  d0,(INT2).l
                movem.l (sp)+,d0-d5/a0-a2
                rte
; End of function Frame

; ---------------------------------------------------------------------------
ski1:
                move.w  #1,d0
                bra.w   selett
; ---------------------------------------------------------------------------
ski2:
                move.w  #2,d0
                bra.w   selett
; ---------------------------------------------------------------------------
ski3:
                move.w  #3,d0
                bra.w   selett
; ---------------------------------------------------------------------------
ski4:
                move.w  #4,d0
                bra.w   selett
; ---------------------------------------------------------------------------
ski5:
                move.w  #5,d0
                bra.w   selett
; ---------------------------------------------------------------------------
ski6:
                move.w  #6,d0
                bra.w   selett
; ---------------------------------------------------------------------------
ski7:
                move.w  #7,d0
                bra.w   selett
; ---------------------------------------------------------------------------
ski8:
                move.w  #8,d0
                bra.w   selett
; ---------------------------------------------------------------------------
ski9:
                move.w  #9,d0
                bra.w   selett
; ---------------------------------------------------------------------------
gski1:
                move.w  #0,d0
                rts
; ---------------------------------------------------------------------------
gski2:
                move.w  #1,d0
                rts
; ---------------------------------------------------------------------------
gski3:
                move.w  #2,d0
                rts
; ---------------------------------------------------------------------------
gski4:
                move.w  #3,d0
                rts
; ---------------------------------------------------------------------------
gski5:
                move.w  #4,d0
                rts
; ---------------------------------------------------------------------------
gski6:
                move.w  #5,d0
                rts
; ---------------------------------------------------------------------------
gski7:
                move.w  #6,d0
                rts
; ---------------------------------------------------------------------------
gski8:
                move.w  #7,d0
                rts
; ---------------------------------------------------------------------------
gski9:
                move.w  #8,d0
                rts
; ---------------------------------------------------------------------------
selmat:
                cmp.w   #7,d0
                bgt.w   rrts
                move.l  #getmatri,(action).l
                move.w  d0,(imatrix).l
                rts
; ---------------------------------------------------------------------------

selett:
                                        ; ROM:00194FD8↑j ...
                move.l  #skidoo,(action).l
                move.w  d0,(skid).l
                rts

; =============== S U B R O U T I N E =======================================


skidoo:

                movea.l #$1AE550,a0
                move.w  (skid).l,d0
                sub.w   #1,d0
                mulu.w  #$1800,d0
                lea     (matrix).l,a1
                adda.l  d0,a1
                move.w  #5,d0

slett:
                clr.l   (a0)
                tst.l   info(a1)
                beq.w   sletto
                move.l  a1,(a0)
                lea     $300(a1),a2
                move.w  #7,d2
                lea     (word_197D4A).l,a3

yuz:
                move.w  $E(a2),d3
                lsl.w   #2,d3
                cmp.w   #$18,d3
                beq.w   yuz2
                move.l  (a3,d3.w),$08(a2)

yuz2:
                lea     vfb_ysca(a2),a2
                dbf     d2,yuz

sletto:
                lea     UPDA2(a1),a1
                lea     $04(a0),a0
                dbf     d0,slett
                lea     (pixcon).l,a1
                movea.l (fx1).l,a0
                lea     $380(a0),a0
                move.l  #$6C,d0 ; 'l'
                jsr     dcopyblo
                bsr.w   zapdel
                clr.w   (og).l
                move.w  #$FFFF,($1AE114).l
                rts
; End of function skidoo

; ---------------------------------------------------------------------------
                rts

; =============== S U B R O U T I N E =======================================


dcopyblo:
                move.w  (delayp).l,-(sp)
                move.l  (dline).l,-(sp)
                move.w  (d2elayp).l,-(sp)
                move.l  (d2line).l,-(sp)
                bsr.w   copybloc
                move.l  (sp)+,(d2line).l
                move.w  (sp)+,(d2elayp).l
                move.l  (sp)+,(dline).l
                move.w  (sp)+,(delayp).l
                rts
; End of function dcopyblo

; ---------------------------------------------------------------------------

editvex:
                dc.l selector, xy1, $00195670
                dc.l $00195670, $0019568c, $0019568c, $001928c6
                dc.l $00195526, $00195510, $001958c0, $0019585a
                dc.l $00195790, $001958a2, $0019527c, $001951c2
                dc.l $00195180

                move.w  (band).l,d0
                lsl.w   #3,d0
                lea     (avbank).l,a0
                lea     (a0,d0.w),a0
                move.l  (pad_now).l,d0
                move.l  d0,d1
                and.l   #$100000,d0
                beq.w   fobb
                addi.w  #$100,6(a0)
                bra.w   padex
; ---------------------------------------------------------------------------

fobb:
                and.l   #$200000,d1
                beq.w   padex
                subi.w  #$100,6(a0)
                bra.w   padex
; ---------------------------------------------------------------------------
speced2:
                move.w  (frames).l,d0
                and.w   #7,d0
                bne.w   padex
                move.w  (band).l,d0
                lsl.w   #3,d0
                lea     (avbank).l,a0
                lea     (a0,d0.w),a0
                move.l  (pad_now).l,d0
                move.l  d0,d1
                and.l   #$100000,d0
                beq.w   speced2a
                tst.w   (a0)
                beq.w   wied1
                subi.w  #1,(a0)

wied1:
                cmpi.w  #$3F,2(a0) ; '?'
                bge.w   padex
                addi.w  #1,2(a0)
                bra.w   padex
; ---------------------------------------------------------------------------

speced2a:
                move.l  d1,d0
                and.l   #$200000,d0
                beq.w   speced2b
                move.w  2(a0),d0
                sub.w   (a0),d0
                cmp.w   #2,d0
                ble.w   padex
                addi.w  #1,(a0)
                subi.w  #1,2(a0)
                bra.w   padex
; ---------------------------------------------------------------------------

speced2b:
                move.l  d1,d0
                and.l   #$400000,d0
                beq.w   speced2c
                tst.w   (a0)
                beq.w   padex
                subi.w  #1,(a0)
                subi.w  #1,2(a0)
                bra.w   padex
; ---------------------------------------------------------------------------

speced2c:
                and.l   #$800000,d1
                beq.w   padex
                cmpi.w  #$3F,2(a0) ; '?'
                bge.w   padex
                addi.w  #1,(a0)
                addi.w  #1,2(a0)
                bra.w   padex
; ---------------------------------------------------------------------------
                move.w  (selected).l,(band).l
                bra.w   selector
; ---------------------------------------------------------------------------
; START OF FUNCTION CHUNK FOR evens

iawfx:
                bsr.w   iwf
                move.w  #8,(editing).l
                movea.l (cwed1).l,a0
                tst.w   UPDA1F(a0)
                bne.w   istat
                move.w  (cwave1).l,d1
                sub.w   #1,d1
                move.l  #$8000,d0
                bsr.w   wavelink

istat:
                                        ; ROM:001954F8↓j ...
                lea     (awfbutts).l,a1
                lea     (awfb2).l,a4
                lea     (pad8bits).l,a2
                move.w  #7,d0
                move.w  UPDA1F(a0),d1

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

issi2:
                btst    #8,d1
                beq.w   issi3
                bset    #7,(a5)

issi3:
                lsr.w   #1,d1
                dbf     d0,issi
                clr.l   d0
                move.w  $102(a0),d0
                lsl.l   #8,d0
                move.l  d0,(ixcon).l
                bra.w   dud_butt
; END OF FUNCTION CHUNK FOR evens
; ---------------------------------------------------------------------------
kpass:
                movea.l #kpasshea,a0  ; "@~g1:SRCEN:Assign effect to keypad//Press t"...
                jsr     print
                move.w  #7,(editing).l
                move.l  #(byte_196DEF+$6D),(cpad).l
                move.l  #kpassbut,($1AF056).l
                bsr.w   ppmes
                bra.w   ud_butts
; ---------------------------------------------------------------------------
ass1:
                move.w  #1,(fass).l
                move.l  #dofass,(action).l
                rts
; ---------------------------------------------------------------------------
ass2:
                move.w  #2,(fass).l
                move.l  #dofass,(action).l
                rts
; ---------------------------------------------------------------------------
ass3:
                move.w  #3,(fass).l
                move.l  #dofass,(action).l
                rts
; ---------------------------------------------------------------------------
ass4:
                move.w  #4,(fass).l
                move.l  #dofass,(action).l
                rts
; ---------------------------------------------------------------------------
ass5:
                move.w  #5,(fass).l
                move.l  #dofass,(action).l
                rts
; ---------------------------------------------------------------------------
ass6:
                move.w  #6,(fass).l
                move.l  #dofass,(action).l
                rts
; ---------------------------------------------------------------------------
ass7:
                move.w  #7,(fass).l
                move.l  #dofass,(action).l
                rts
; ---------------------------------------------------------------------------
ass8:
                move.w  #8,(fass).l
                move.l  #dofass,(action).l
                rts
; ---------------------------------------------------------------------------
ass9:
                move.w  #9,(fass).l
                move.l  #dofass,(action).l
                rts

; =============== S U B R O U T I N E =======================================


dofass:
                                        ; ROM:00195362↑o ...
                movea.l (fx).l,a5
                movea.l (a5),a1
                lea     $380(a1),a1
                lea     (pixcon).l,a0
                move.l  #$6C,d0 ; 'l'
                jsr     copybloc
                move.w  (fass).l,d0
                sub.w   #1,d0
                mulu.w  #$1800,d0
                lea     (matrix).l,a4
                adda.l  d0,a4
                movea.l a4,a1
                movea.l (a5),a0
                move.l  #$1800,d0
                jsr     blitcopy
                bra.w   editquit
; End of function dofass

; ---------------------------------------------------------------------------

dfass:
                move.l  (a5)+,d0
                bne.w   copyit
                move.l  #0,info(a4)
                bra.w   nxfass
; ---------------------------------------------------------------------------

copyit:
                movea.l d0,a0
                movea.l a4,a1
                move.l  #$400,d0
                jsr     blitcopy

nxfass:
                lea     UPDA2(a4),a4
                dbf     d5,dfass
                rts

; =============== S U B R O U T I N E =======================================


iwf:
                                        ; ROM:001954E4↓p
                movea.l #awhead,a0  ; "Attach and Adjust Waveforms//Press keys"...
                clr.w   (antelope).l
                jsr     cprint
                clr.l   (ixcon).l
                clr.l   (dword_19B054).l
                move.w  (monitor).l,d0
                move.w  d0,(cwave1).l
                sub.w   #1,d0
                move.w  d0,d1
                lsl.w   #2,d1
                movea.l (fxobj).l,a6
                lea     (a6,d1.w),a5
                move.l  a5,(cwed1).l
                bsr.w   varadd
                move.b  SRCEN(a0),d0
                and.w   #$FF,d0
                move.w  d0,(cwave2).l
                lsl.w   #2,d0
                lea     (a6,d0.w),a5
                move.l  a5,(cwed2).l
                move.l  #(byte_196DEF+$FD),(cpad).l
                move.l  #awfbutts,($1AF056).l
                rts
; End of function iwf

; ---------------------------------------------------------------------------
iawfy:
                bsr.s   iwf
                move.w  #9,(editing).l
                movea.l (cwed2).l,a0
                tst.w   UPDA1F(a0)
                bne.w   istat
                move.w  (cwave2).l,d1
                move.l  #$8000,d0
                bsr.w   wavelink
                bra.w   istat
; ---------------------------------------------------------------------------
awf_y:
                lea     (pad_now).l,a1
                move.b  SRCEN(a1),d0
                rol.b   #4,d0
                movea.l (cwed2).l,a4
                bra.w   wset
; ---------------------------------------------------------------------------
awf_x:
                lea     (pad_now).l,a1
                move.b  SRCEN(a1),d0
                rol.b   #2,d0
                movea.l (cwed1).l,a4

wset:
                lea     (ixcon).l,a0
                jsr     inertcon
                move.l  (ixcon).l,d1
                lsr.l   #8,d1
                move.w  d1,$102(a4)
                move.l  a4,(udud).l
                lea     (udud_but).l,a0
                bsr.w   gkp
                move.l  (a1),d0
                and.l   #$22002000,d0
                beq.w   rrts
                movea.l (eddie).l,a2
                btst    #0,(a2)
                beq.w   soxx
                move.l  #rwfa,(action).l
                bra.w   sdb
; ---------------------------------------------------------------------------
setantel:
                move.w  #1,(antelope).l
                rts
; ---------------------------------------------------------------------------
lul1:
                tst.w   (antelope).l
                bne.w   ant1
                bchg    #0,$101(a4)
                rts
; ---------------------------------------------------------------------------
lul2:
                tst.w   (antelope).l
                bne.w   ant2
                bchg    #1,$101(a4)
                rts
; ---------------------------------------------------------------------------
lul3:
                tst.w   (antelope).l
                bne.w   ant3
                bchg    #2,$101(a4)
                rts
; ---------------------------------------------------------------------------
lul4:
                tst.w   (antelope).l
                bne.w   ant4
                bchg    #3,$101(a4)
                rts
; ---------------------------------------------------------------------------
lul5:
                tst.w   (antelope).l
                bne.w   ant5
                bchg    #4,$101(a4)
                rts
; ---------------------------------------------------------------------------
lul6:
                tst.w   (antelope).l
                bne.w   ant6
                bchg    #5,$101(a4)
                rts
; ---------------------------------------------------------------------------
lul7:
                tst.w   (antelope).l
                bne.w   ant7
                bchg    #6,$101(a4)
                rts
; ---------------------------------------------------------------------------
lul8:
                tst.w   (antelope).l
                bne.w   ant8
                bchg    #7,$101(a4)
                rts
; ---------------------------------------------------------------------------

ant1:
                bchg    #0,UPDA1F(a4)

clant:
                move.w  #2,(antelope).l
                clr.w   (word_199A6A).l
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
; ---------------------------------------------------------------------------
                lea     (pad_now).l,a1
                move.b  SRCEN(a1),d0
                rol.b   #2,d0
                lea     (ixcon).l,a0
                jsr     inertcon
                bra.w   spn_butt
; ---------------------------------------------------------------------------
xy1:
                lea     (pad_now).l,a1
                move.b  SRCEN(a1),d0
                rol.b   #2,d0
                lea     (ixcon).l,a0
                jsr     inertcon
                move.b  SRCEN(a1),d0
                rol.b   #4,d0
                lea     (iycon).l,a0
                jsr     inertcon

spn_butt:
                move.l  (a1),d0
                and.l   #$10000,d0
                beq.w   pn_butte
                move.l  #wfa,(action).l
                clr.w   (editing).l
                bra.w   sdb
; ---------------------------------------------------------------------------

pn_butte:
                                        ; ROM:001956BE↑j
                tst.l   (action).l
                bne.w   rrts
                lea     (pad_now).l,a1
                move.l  (a1),d0
                move.l  #$2000000,d1
                and.l   d0,d1
                bne.w   bbexit
                move.l  #$20000000,d1
                and.l   d0,d1
                bne.w   prevexit
                and.l   #$2000,d0
                beq.w   rrts
                move.w  (selected).l,d0
                add.w   #1,d0
                cmp.w   (selectab).l,d0
                ble.w   slecset
                clr.w   d0

slecset:
                                        ; ROM:00195734↓j ...
                move.w  d0,(selected).l
                bra.w   sted
; ---------------------------------------------------------------------------

prevexit:
                move.w  (selected).l,d0
                sub.w   #1,d0
                bpl.s   slecset
                move.w  (selectab).l,d0
                bra.s   slecset
; ---------------------------------------------------------------------------
                move.l  (a1),d0
                and.l   #$22002000,d0
                beq.w   rrts

bexit:
                move.l  #editquit,(action).l
                clr.w   (symed).l
                rts
; ---------------------------------------------------------------------------

bbexit:
                move.l  (ledit).l,(action).l
                clr.w   (symed).l
                bra.w   sdb
; ---------------------------------------------------------------------------

xselup:
                bsr.w   selup
                move.l  #wtud,(action).l
                rts
; ---------------------------------------------------------------------------

xseldn:
                bsr.w   seldn
                move.l  #wtud,(action).l
                rts
; ---------------------------------------------------------------------------
dladj:
                move.l  (pad_now).l,d0
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
                move.l  (pad_now).l,d0
                move.l  d0,d1
                and.l   #$100000,d0
                bsr.w   selup
                bra.w   gnek
                move.l  d1,d0
                and.l   #$200000,d0
                bsr.w   seldn

gnek:
                move.w  (selected).l,d0
                cmp.w   #1,d0
                bne.w   gnek2
                clr.w   d0

gnek2:
                sub.w   #1,d0
                move.w  d0,(delayf).l
                rts
; ---------------------------------------------------------------------------

spinc:
                addi.w  #1,(delayt).l

sic:
                andi.w  #$3F,(delayt).l ; '?'

spex:
                move.w  #1,(seldb).l
                move.l  #udedg,(action).l
                rts
; ---------------------------------------------------------------------------

spdec:
                subi.w  #1,(delayt).l
                bra.s   sic
; ---------------------------------------------------------------------------

sninc:
                addi.w  #1,(delayn).l

snc:
                andi.w  #$3F,(delayn).l ; '?'
                bra.s   spex
; ---------------------------------------------------------------------------

sndec:
                subi.w  #1,(delayn).l
                bra.s   snc
; ---------------------------------------------------------------------------
adsred:
                move.l  (pad_now).l,d0
                move.l  d0,d1
                and.l   #$100000,d0
                bne.w   selup
                move.l  d1,d0
                and.l   #$200000,d0
                bne.w   seldn
                movea.l (aded).l,a0
                move.w  (selected).l,d2
                lsl.w   #1,d2
                lea     (a0,d2.w),a0
                move.l  d1,d0
                and.l   #$400000,d0
                bne.w   intdec
                move.l  d1,d0
                and.l   #$800000,d0
                bne.w   intinc

padex:
                                        ; ROM:001951B4↑j ...
                move.l  (pad_now).l,d1
                and.l   #$22002000,d1
                bne.w   owwt
                rts
; ---------------------------------------------------------------------------

intdec:
                subi.w  #$80,(a0)
                rts
; ---------------------------------------------------------------------------

intinc:
                addi.w  #$80,(a0)
                rts
; ---------------------------------------------------------------------------
spdinc:
                move.l  (pad_now).l,d0
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
                lea     (wfpad).l,a0
                lea     (pad_now).l,a1
                move.w  (a1),d6
                move.w  #3,d7

_gk1:
                move.l  (a0)+,d0
                beq.w   _bodb
                btst    #0,d6
                beq.w   _bodb
                movea.l d0,a2
                jmp     (a2)
; ---------------------------------------------------------------------------

_bodb:
                                        ; ROM:00195912↑j
                lsr.w   #1,d6
                dbf     d7,_gk1
                move.w  2(a1),d6
                move.w  #7,d7

_gk2:
                move.l  (a0)+,d0
                beq.w   _bodb2
                movea.l d0,a2
                btst    #0,d6
                beq.w   _bodb2
                jmp     (a2)
; ---------------------------------------------------------------------------

_bodb2:
                                        ; ROM:00195934↑j
                lsr.w   #1,d6
                dbf     d7,_gk2
                move.l  (pad_now).l,d1
                and.l   #$22002000,d1
                beq.w   rrts
                move.w  (selected).l,d2
                movea.l (edwave).l,a0
                lsl.w   #4,d2
                lea     $04(a0,d2.w),a0
                cmp.l   #$2000000,d1
                bne.w   zinc

owwt:
                move.w  #1,(editing).l
                clr.w   (symed).l
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

swf1:
                move.w  #0,d0
; START OF FUNCTION CHUNK FOR swf2

swfe:
                                        ; ROM:001959C0↓j ...
                bsr.w   git
                move.w  d0,$E(a0)
                move.l  #wtud,(action).l
                rts
; END OF FUNCTION CHUNK FOR swf2

; =============== S U B R O U T I N E =======================================


swf2:

; FUNCTION CHUNK AT 001959A2 SIZE 00000014 BYTES

                move.w  #1,d0
                bra.s   swfe
; End of function swf2

; ---------------------------------------------------------------------------

swf3:
                move.w  #2,d0
                bra.s   swfe
; ---------------------------------------------------------------------------

swf4:
                move.w  #3,d0
                bra.s   swfe
; ---------------------------------------------------------------------------

swf5:
                move.w  #4,d0
                bra.s   swfe
; ---------------------------------------------------------------------------

swf6:
                move.w  #5,d0
                bra.s   swfe
; ---------------------------------------------------------------------------

swf7:
                move.w  #7,d0
                bra.s   swfe
; ---------------------------------------------------------------------------

swf8:
                move.w  #8,d0
                bra.s   swfe
; ---------------------------------------------------------------------------

swf9:
                move.w  #6,d0
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

; =============== S U B R O U T I N E =======================================


git:
                                        ; ROM:loc_1959E6↑p ...
                move.w  (selected).l,d2
                movea.l (edwave).l,a0
                lsl.w   #4,d2
                lea     (a0,d2.w),a0
                rts
; End of function git

; ---------------------------------------------------------------------------

selector:
                move.l  (pad_now).l,d0
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
                tst.w   (cbuttf).l
                beq.w   onstak

soxx:
                move.l  #reedit,(action).l
                bra.w   sdb
; ---------------------------------------------------------------------------

onstak:
                                        ; ROM:00195A4A↑j
                movea.l (esp).l,a0
                move.l  (a0)+,d0
                move.l  (a0),d0
                move.l  a0,(esp).l
                tst.l   d0
                beq.w   bexit
                move.l  d0,(action).l
                bra.w   sdb
; ---------------------------------------------------------------------------

seuss:
                move.w  (selected).l,d0

sted:
                lsl.w   #3,d0
                movea.l (editlist).l,a0
                move.l  $0C(a0,d0.w),(action).l
                clr.w   (editing).l
                bra.w   sdb

; =============== S U B R O U T I N E =======================================


selup:
                                        ; ROM:001957E0↑p ...
                subi.w  #1,(selected).l
                bpl.w   sud
                move.w  (selectab).l,(selected).l

sud:
                                        ; sub_195AC6+14↓j ...
                move.l  #ud_selcu,(action).l

sdb:
                                        ; ROM:001956D2↑j ...
                move.w  #1,(seldb).l
                rts
; End of function selup


; =============== S U B R O U T I N E =======================================


seldn:
                                        ; ROM:001957F0↑p ...
                addi.w  #1,(selected).l
                move.w  (selectab).l,d0
                cmp.w   (selected).l,d0
                bpl.s   sud
                clr.w   (selected).l
                tst.w   (sec_en).l
                beq.s   sud
                addi.w  #1,(sec_cnt).l
                cmpi.w  #8,(sec_cnt).l
                blt.s   sud
                clr.w   (sec_cnt).l
                move.l  #old_edit,(action).l
                bra.s   sdb
; End of function seldn


; =============== S U B R O U T I N E =======================================


InitBeas:
                lea     (beasties).l,a0
                move.w  #$C,d7
                move.w  d7,(nbeastie).l

ibeasts:
                move.w  #$FFFF,vfb_xsca(a0)
                lea     d_z(a0),a0
                dbf     d7,ibeasts
                rts
; End of function InitBeas

; ---------------------------------------------------------------------------
ccc:
                clr.w   (snoop).l
                rts
; ---------------------------------------------------------------------------
cc:
                addi.w  #1,(snoop).l
ccat:
                tst.l   ($804850).l
                bne.w   rrts

cow:
                bra.s   cow
                rts

; =============== S U B R O U T I N E =======================================


RunBeast:
                movea.l (blist).l,a0
                movea.l (dlist).l,a4
                lea     (beasties).l,a2
                move.w  (nbeastie).l,d7
                tst.w   (scron).l
                beq.w   RBeasts
                bmi.w   RBeasts
                sub.w   #1,d7
                lea     d_z(a2),a2

RBeasts:
                                        ; sub_195B4C+22↑j ...
                move.w  d7,-(sp)
                move.w  vfb_xsca(a2),d0
                bmi.w   nxbeast
                lea     (ModeVex).l,a3
                asl.w   #2,d0
                movea.l (a3,d0.w),a3
                jsr     (a3)

nxbeast:
                move.w  (sp)+,d7
                lea     d_z(a2),a2
                dbf     d7,RBeasts
                bra.w   StopList
; End of function RunBeast

; ---------------------------------------------------------------------------

postfixu:
                                        ; ROM:00195D92↓o
                dc.l  make_rmw, make_tra
make_rmw:
                lea     -$20(a0),a3
                bset    #6,$A(a3)
                bset    #7,$A(a3)

setref:
                tst.w   $1E(a2)
                bne.w   setrf
                bclr    #0,9(a3)
                rts
; ---------------------------------------------------------------------------

setrf:
                bset    #0,9(a3)
                rts
; ---------------------------------------------------------------------------
make_tra:
                lea     -$20(a0),a3
                bset    #7,$A(a3)
                bra.s   setref
; ---------------------------------------------------------------------------

ModeVex:
                dc.l clip0, clip1, stoptop, clip2
                dc.l clip1, clip1, clip1, clip1

                clr.w   (frames).l
                jmp     makeit_t
; ---------------------------------------------------------------------------
stoptop:
                move.w  #$B4,d3
                sub.w   (palside).l,d3
                move.w  #$1A4,d4
                add.w   (paltop).l,d4
                sub.w   #$B0,d4
                tst.w   (vlm_mode).l
                beq.w   clip1
                move.w  (pixcon).l,d0
                sub.w   #$7F,d0
                add.w   d3,d0
                move.w  d0,(a2)
                move.w  (piycon).l,d0
                sub.w   #$7F,d0
                lsl.w   #1,d0
                add.w   d4,d0
                move.w  d0,$04(a2)
                bmi.w   rrts
                bra.w   clip1
; ---------------------------------------------------------------------------
clip0:
                move.w  (skale).l,d0
                beq.w   clip00
                add.w   #2,d0
                move.w  d0,$E(a2)
                move.w  #1,(bo).l
                bra.w   clip2
; ---------------------------------------------------------------------------

clip00:
                clr.w   $E(a2)
                move.w  #1,(bo).l
                bra.w   clipc
; ---------------------------------------------------------------------------

clip1:
                                        ; ROM:00195C4E↑j
                clr.w   (bo).l

clipc:
                move.w  (a2),d0
                move.w  $04(a2),d1
                cmpi.w  #5,$E(a2)
                bne.w   fixpal
                sub.w   (palside).l,d0
                add.w   (paltop).l,d1

fixpal:
                and.w   #$FFF,d0
                clr.w   d6
                tst.w   d1
                bpl.w   ponscr
                move.w  d1,d6
                neg.w   d6
                lsr.w   #1,d6

ponscr:
                bclr    #0,d1
                swap    d6
                move.w  $08(a2),d6
                move.w  $A(a2),d3
                lsl.w   #8,d3
                or.w    d6,d3
                swap    d3
                swap    d6
                move.w  $E(a2),d7
                lea     (ObTypes).l,a3
                asl.w   #3,d7
                move.w  $18(a2),d3
                move.w  $1A(a2),d4
                move.w  $1C(a2),d5
                move.w  $16(a2),d2
                movea.l vfb_ysca(a2),a1
                tst.w   d6
                beq.w   nohang
                move.w  d3,d7
                mulu.w  d6,d7
                lsl.l   #3,d7
                move.l  d7,(hango).l
                adda.l  d7,a1
                clr.w   d1

nohang:
                bsr.w   MakeUnSc
                move.w  vfb_angl(a2),d0
                bmi.w   rrts
                lea     (postfixu).l,a3
                asl.w   #2,d0
                movea.l (a3,d0.w),a3
                jmp     (a3)
; ---------------------------------------------------------------------------

clip2:
                move.w  (a2),d0
                and.w   #$FFF,d0
                clr.w   d6
                move.w  $04(a2),d1
                bpl.w   sponscr
                move.w  d1,d6
                neg.w   d6
                move.w  (skale).l,d3
                add.w   #1,d3
                lsr.w   d3,d6

sponscr:
                bclr    #0,d1
                swap    d6
                move.w  $08(a2),d6
                move.w  $A(a2),d3
                lsl.w   #8,d3
                or.w    d6,d3
                swap    d3
                swap    d6
                move.w  $E(a2),d7
                lea     (ObTypes).l,a3
                asl.w   #3,d7
                move.w  (a3,d7.w),d3
                move.w  2(a3,d7.w),d4
                move.w  $04(a3,d7.w),d5
                move.w  6(a3,d7.w),d2
                movea.l vfb_ysca(a2),a1
                tst.w   d6
                beq.w   snohang
                move.w  d3,d7
                mulu.w  d6,d7
                lsl.l   #3,d7
                move.l  d7,(hango).l
                adda.l  d7,a1
                clr.w   d1

snohang:
                bsr.w   MakeScal
                move.w  vfb_angl(a2),d0
                bmi.w   rrts
                lea     (postfixu).l,a3
                asl.w   #2,d0
                movea.l (a3,d0.w),a3
                jmp     (a3)

; =============== S U B R O U T I N E =======================================


InitList:
                move.l  #$1AE81C,d0
                and.l   #$FFFFFFE0,d0
                movea.l d0,a0
                move.l  #0,(a0)+
                move.l  #4,(a0)+
                lea     $08(a0),a0
                move.l  a0,(ddlist).l
                lsl.l   #5,d0
                move.w  (n_vdb).l,d1
                move.w  (n_vde).l,d2
                lsl.w   #3,d1
                lsl.w   #3,d2
                or.w    #3,d1
                or.w    #3,d2
                bset    #$E,d1
                bset    #$F,d2
                swap    d0
                move.w  #0,(a0)+
                move.w  d0,(a0)+
                swap    d0
                move.w  d0,(a0)+
                move.w  d1,(a0)+
                swap    d0
                move.w  #0,(a0)+
                move.w  d0,(a0)+
                swap    d0
                move.w  d0,(a0)+
                move.w  d2,(a0)+
                move.l  a0,(dlist).l
                bsr.w   StopList
                move.l  #$1AEA8C,d0
                and.l   #$FFFFFFE0,d0
                move.l  d0,(blist).l
                movea.l d0,a0
                bra.w   *+4
; End of function InitList


; =============== S U B R O U T I N E =======================================


StopList:
                                        ; sub_195DA0+68↑p ...
                move.w  #$F,d0

sl:
                move.l  #0,(a0)+
                move.l  #4,(a0)+
                dbf     d0,sl
                rts
; End of function StopList


; =============== S U B R O U T I N E =======================================


MakeScal:
                tst.w   (bo).l
                beq.w   nmulto2
                movem.l d0-d5/a1,-(sp)
                bsr.w   nmulto2
                move.w  #$180,d0
                move.w  #0,d1
                move.w  #2,d2
                move.w  #1,d3
                move.w  #$180,d4
                move.w  #3,d5
                movea.l #LaunchVLM,a1
                clr.w   (bo).l
                bsr.w   nmulto
                move.w  #1,(bo).l
                lea     -$20(a0),a3
                bset    #0,9(a3)
                movem.l (sp)+,d0-d5/a1
                move.w  d3,d6
                move.w  (skale).l,d7
                add.w   #1,d7
                lsr.w   d7,d6
                lsl.w   #2,d6
                move.w  d6,d7
                lsl.w   #1,d7
                and.l   #$FFFF,d7
                move.l  d7,(dbadj).l
                adda.l  d7,a1
                move.w  (skale).l,d7
                lsl.w   d7,d6
                add.w   d6,d0
                bsr.w   nmulto2
                clr.w   (bo).l
                rts
; End of function MakeScal


; =============== S U B R O U T I N E =======================================


nmulto2:
                                        ; sub_195E3A+E↑p ...
                move.w  d0,-(sp)
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
                move.w  d4,d7
                ror.l   #2,d7
                swap    d6
                and.w   #$FF00,d6
                or.w    d7,d6
                move.w  d6,(a0)+
                swap    d7
                move.w  d1,d6
                lsl.w   #3,d6
                or.w    d7,d6
                bset    #0,d6
                move.w  d6,(a0)+
                clr.w   (a0)+
                move.w  d2,d7
                lsl.w   #7,d7
                moveq   #0,d6
                move.w  d3,d6
                move.w  (skale).l,d0
                add.w   (bo).l,d0
                lsr.w   d0,d6
                ror.l   #4,d6
                bclr    #$F,d6
                or.w    d7,d6
                move.w  d6,(a0)+
                move.w  (sp)+,d0
                and.w   #$FFF,d0
                move.w  d3,d7
                lsl.w   #2,d7
                swap    d6
                or.w    d7,d6
                move.w  d6,(a0)+
                move.w  d5,d6
                ror.w   #4,d6
                or.w    d0,d6
                bset    #$F,d6
                move.w  d6,(a0)+
                clr.l   (a0)+
                move.l  #$14040,d0
                cmpi.w  #2,(skale).l
                bne.w   khluj
                move.l  #$18080,d0

khluj:
                move.l  d0,(a0)+
                lea     $08(a0),a0
                lea     $20(a4),a4
                rts
; End of function nmulto2


; =============== S U B R O U T I N E =======================================


MakeUnSc:
                tst.w   (bo).l
                beq.w   nmulto
                movem.l d0-d5/a1,-(sp)
                bsr.w   nmulto
                move.w  #$180,d0
                move.w  #0,d1
                move.w  #2,d2
                move.w  #1,d3
                move.w  #$180,d4
                move.w  #3,d5
                movea.l #LaunchVLM,a1
                clr.w   (bo).l
                bsr.w   nmulto
                move.w  #1,(bo).l
                lea     -$20(a0),a3
                bset    #0,9(a3)
                movem.l (sp)+,d0-d5/a1
                move.w  d3,d6
                move.w  (skale).l,d7
                add.w   #1,d7
                lsr.w   d7,d6
                lsl.w   #2,d6
                move.w  d6,d7
                lsl.w   #1,d7
                and.l   #$FFFF,d7
                move.l  d7,(dbadj).l
                adda.l  d7,a1
                move.w  (skale).l,d7
                lsl.w   d7,d6
                add.w   d6,d0
                bsr.w   nmulto
                clr.w   (bo).l
                rts
; End of function MakeUnSc


; =============== S U B R O U T I N E =======================================


nmulto:
                                        ; sub_195F78+6↑j ...
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
                move.w  d4,d7
                ror.l   #2,d7
                swap    d6
                and.w   #$FF00,d6
                or.w    d7,d6
                move.w  d6,(a0)+
                swap    d7
                move.w  d1,d6
                lsl.w   #3,d6
                or.w    d7,d6
                move.w  d6,(a0)+
                clr.w   (a0)+
                move.w  d2,d7
                lsl.w   #7,d7
                moveq   #0,d6
                move.w  d3,d6
                tst.w   (bo).l
                beq.w   snoke
                move.w  d0,-(sp)
                move.w  (skale).l,d0
                add.w   (bo).l,d0
                lsr.w   d0,d6
                move.w  (sp)+,d0

snoke:
                and.w   #$FFF,d0
                ror.l   #4,d6
                bclr    #$F,d6
                or.w    d7,d6
                move.w  d6,(a0)+
                move.w  d3,d7
                lsl.w   #2,d7
                swap    d6
                or.w    d7,d6
                move.w  d6,(a0)+
                move.w  d5,d6
                ror.w   #4,d6
                or.w    d0,d6
                bset    #$F,d6
                move.w  d6,(a0)+
                lea     vfb_ysca(a0),a0
                lea     $20(a4),a4
                rts
; End of function nmulto


; =============== S U B R O U T I N E =======================================


udud_but:
                movea.l (udud).l,a0
                bra.w   istat
; End of function udud_but

; ---------------------------------------------------------------------------

dud_butt:
                move.l  ($1AF056).l,-(sp)
                move.l  #awfb2,($1AF056).l
                bsr.w   ud_butts
                move.l  (sp)+,($1AF056).l

; =============== S U B R O U T I N E =======================================


ud_butts:
                                        ; sub_194648+22↑j ...
                move.w  (inverse).l,-(sp)
                movea.l ($1AF056).l,a3
                move.w  #$B,d3
                lea     (padchars).l,a4
                lea     (board).l,a0

ud_b:
                move.b  (a4)+,d2
                tst.b   (a3)
                bmi.w   nud
                clr.w   (inverse).l
                btst    #7,2(a3)
                beq.w   ud_i
                move.w  #1,(inverse).l

ud_i:
                move.b  (a3),d0
                move.b  SRCEN(a3),d1
                and.w   #$FF,d0
                and.w   #$FF,d1
                bsr.w   charblit

nud:
                lea     $04(a3),a3
                dbf     d3,ud_b
                move.w  (sp)+,(inverse).l
                rts
; End of function ud_butts


; =============== S U B R O U T I N E =======================================


charblit:
                                        ; wtud+82↑j ...
                movem.w d0-d2,-(sp)
                and.w   #$FF,d2
                move.l  #$11800,(A2_FLAGS).l
                lsl.w   #1,d2
                lea     (myFont).l,a1
                move.w  word_198CB4-myFont(a1,d2.w),d2
                lea     (a1,d2.w),a2
                move.l  a2,(A2_BASE).l
                move.l  #0,(A2_PIXEL).l
                move.l  #$1FFF8,(A2_STEP).l
                move.l  #$14200,(A1_FLAGS).l
                move.l  #$1B00F8,(A1_BASE).l
                lsl.w   #3,d0
                move.w  d1,d2
                lsl.w   #3,d1
                add.w   d2,d1
                swap    d1
                move.w  d0,d1
                move.l  d1,(A1_PIXEL).l
                move.l  #$1FFF8,(A1_STEP).l
                move.l  #$80008,(B_COUNT).l
                move.l  #0,(B_PATD).l
                move.l  #0,($F0226C).l
                move.l  #$1800609,d7
                tst.w   (inverse).l
                beq.w   notinv
                move.l  #$600609,d7

notinv:
                move.l  d7,(B_CMD).l
                bsr.w   WaitBlit
                movem.w (sp)+,d0-d2
                rts
; End of function charblit


; =============== S U B R O U T I N E =======================================


cleol:
                                        ; sub_19484E+2C↑p ...
                clr.w   d0
                clr.w   d1
                move.w  #$140,d2
                move.w  #$F0,d3
                moveq   #0,d4
                move.l  #$14200,d7
                move.l  #$10208,-(sp)
                move.w  #1,(nphrase).l
                bra.w   bblo
; End of function cleol


; =============== S U B R O U T I N E =======================================


sblitblo:
                                        ; ROM:loc_1935AC↑p ...
                movem.w d0-d3,-(sp)
                move.w  (skale).l,d7
                lsr.w   d7,d0
                lsr.w   d7,d1
                lsr.w   d7,d2
                bne.w   slogg
                move.w  #1,d2

slogg:
                lsr.w   d7,d3
                bne.w   slogg2
                move.w  #1,d3

slogg2:
                bsr.w   blitbloc
                movem.w (sp)+,d0-d3
                rts
; End of function sblitblo


; =============== S U B R O U T I N E =======================================


blitbloc:
                                        ; ROM:00193D50↑p ...
                clr.w   (nphrase).l
                move.l  #$4420,d7
                move.l  #$10200,-(sp)

bblo:
                move.l  d7,(A1_FLAGS).l
                move.l  a0,d7
                move.l  d7,(A1_BASE).l
                move.w  d1,d7
                swap    d7
                move.w  d0,d7
                move.l  d7,(A1_PIXEL).l
                move.l  #0,(A1_FPIXE).l
                moveq   #1,d7
                move.l  d7,(A1_INC).l
                move.l  #0,(A1_FINC).l
                move.w  #1,d7
                swap    d7
                move.w  d2,d7
                tst.w   (nphrase).l
                bne.w   nnphr
                move.l  #$14420,(A1_FLAGS).l

nnphr:
                neg.w   d7
                move.l  d7,(A1_STEP).l
                move.l  #0,(A1_FSTEP).l
                move.w  d3,d7
                swap    d7
                move.w  d2,d7
                move.l  d7,(B_COUNT).l
                move.w  d4,d7
                swap    d7
                move.w  d4,d7
                move.l  d7,(B_PATD).l
                move.l  d7,($F0226C).l
                move.l  (sp)+,d7
                move.l  d7,(B_CMD).l
; End of function blitbloc

; START OF FUNCTION CHUNK FOR blitcopy

WaitBlit:
                                        ; ROM:00194B1E↑j ...
                move.l  (B_CMD).l,d7
                btst    #0,d7
                beq.s   WaitBlit
; END OF FUNCTION CHUNK FOR blitcopy

rrts:
                rts
; ---------------------------------------------------------------------------
                dc.b $2E ; .
                dc.b $3C ; <
                dc.b   0
                dc.b   3
; ---------------------------------------------------------------------------
ecopy:
                neg.b   -(a0)
                bra.w   eec
; ---------------------------------------------------------------------------
CopyBloc:
                move.l  #$34220,d7

eec:
                move.l  d7,(A1_FLAGS).l
                move.l  #$54420,d7
                move.l  d7,(A2_FLAGS).l
                move.w  d3,d7
                swap    d7
                move.w  d2,d7
                move.l  d7,(B_COUNT).l
                move.w  d1,d7
                swap    d7
                move.w  d0,d7
                move.l  d7,(A1_PIXEL).l
                move.w  d5,d7
                swap    d7
                move.w  d4,d7
                move.l  d7,(A2_PIXEL).l
                move.l  #0,(A1_FPIXE).l
                move.l  #1,(A1_INC).l
                move.l  #0,(A1_FINC).l
                move.w  #1,d7
                swap    d7
                move.w  d2,d7
                neg.w   d7
                move.l  d7,(A1_STEP).l
                move.l  d7,(A2_STEP).l
                move.l  a0,d7
                move.l  d7,(A1_BASE).l
                move.l  a1,d7
                move.l  d7,(A2_BASE).l
                move.l  #$1800E01,d7
                move.l  d7,(B_CMD).l
                bra.w   WaitBlit
; ---------------------------------------------------------------------------
MergeBlo:
                move.l  #0,(B_PATD).l
                move.l  #0,($F0226C).l
                move.l  #$34420,d7
                move.l  d7,(A1_FLAGS).l
                move.l  #$54420,d7
                move.l  d7,(A2_FLAGS).l
                move.w  d3,d7
                swap    d7
                move.w  d2,d7
                move.l  d7,(B_COUNT).l
                move.w  d1,d7
                swap    d7
                move.w  d0,d7
                move.l  d7,(A1_PIXEL).l
                move.w  d5,d7
                swap    d7
                move.w  d4,d7
                move.l  d7,(A2_PIXEL).l
                move.l  #0,(A1_FPIXE).l
                move.l  #1,(A1_INC).l
                move.l  #0,(A1_FINC).l
                move.w  #1,d7
                swap    d7
                move.w  d2,d7
                neg.w   d7
                move.l  d7,(A1_STEP).l
                move.l  d7,(A2_STEP).l
                move.l  a0,d7
                move.l  d7,(A1_BASE).l
                move.l  a1,d7
                move.l  d7,(A2_BASE).l
                move.l  #$9800E01,d7
                move.l  d7,(B_CMD).l
                bra.w   WaitBlit

; =============== S U B R O U T I N E =======================================


dcode:
                lea     (codez).l,a3
                move.w  #$1F,d1

dco:
                move.b  (a3)+,d2
                btst    d1,d0
                bne.w   rrts
                dbf     d1,dco
                clr.w   d2
                rts
; End of function dcode

; ---------------------------------------------------------------------------

codez:    dc.b  $78, $78, $41, $50, $78, $78, $42, $78
                dc.b  $52, $4C, $44, $55, $31, $34, $37, $2A
                dc.b  $78, $78, $43, $78, $78, $78, $4F, $78
                dc.b  $32, $35, $38, $30, $33, $36, $39, $23
; ---------------------------------------------------------------------------

readpad:
                movem.l d0-d2,-(sp)
                move.l  #$F0FFFFFC,d1
                moveq   #$FFFFFFFF,d2
                move.w  #$81FE,(JOYSTICK).l
                move.l  (JOYSTICK).l,d0
                or.l    d1,d0
                ror.l   #4,d0
                and.l   d0,d2
                move.w  #$81FD,(JOYSTICK).l
                move.l  (JOYSTICK).l,d0
                or.l    d1,d0
                ror.l   #8,d0
                and.l   d0,d2
                move.w  #$81FB,(JOYSTICK).l
                move.l  (JOYSTICK).l,d0
                or.l    d1,d0
                rol.l   #6,d0
                rol.l   #6,d0
                and.l   d0,d2
                move.w  #$81F7,(JOYSTICK).l
                move.l  (JOYSTICK).l,d0
                or.l    d1,d0
                rol.l   #8,d0
                and.l   d0,d2
                moveq   #$FFFFFFFF,d1
                eor.l   d2,d1
                move.l  (pad_now).l,d0
                move.l  d1,(pad_now).l
                eor.l   d1,d0
                and.l   d1,d0
                move.l  d0,(pad_shot).l
                move.l  #$FFFFFF3,d1
                moveq   #$FFFFFFFF,d2
                move.w  #$817F,(JOYSTICK).l
                move.l  (JOYSTICK).l,d0
                or.l    d1,d0
                rol.b   #2,d0
                ror.l   #8,d0
                and.l   d0,d2
                move.w  #$81BF,(JOYSTICK).l
                move.l  (JOYSTICK).l,d0
                or.l    d1,d0
                rol.b   #2,d0
                ror.l   #8,d0
                ror.l   #4,d0
                and.l   d0,d2
                move.w  #$81DF,(JOYSTICK).l
                move.l  (JOYSTICK).l,d0
                or.l    d1,d0
                rol.b   #2,d0
                rol.l   #8,d0
                and.l   d0,d2
                move.w  #$81EF,(JOYSTICK).l
                move.l  (JOYSTICK).l,d0
                or.l    d1,d0
                rol.b   #2,d0
                rol.l   #4,d0
                and.l   d0,d2
                moveq   #$FFFFFFFF,d1
                eor.l   d2,d1
                move.l  ($1AE010).l,d0
                move.l  d1,($1AE010).l
                eor.l   d1,d0
                and.l   d1,d0
                move.l  d0,($1AE01C).l
                movem.l (sp)+,d0-d2
                rts
; ---------------------------------------------------------------------------
initobje:
                move.l  #$FFFFFFFF,(activeob).l
                lea     (objects).l,a0
                move.w  #$3F,d0 ; '?'
                move.w  d0,(ofree).l
                move.l  a0,(freeobje).l
                movea.l #$FFFFFFFF,a1

IniA:
                move.l  a1,(a0)
                movea.l a0,a1
                lea     d_z(a0),a0
                move.l  a0,$04(a1)
                dbf     d0,IniA
                move.l  #$FFFFFFFF,$04(a1)
                rts
; ---------------------------------------------------------------------------
insertobje:
                movea.l 0(a0),a1
                movea.l $04(a0),a2
                subi.w  #1,(ofree).l
                lea     (activeob).l,a3
                lea     (freeobje).l,a4
                bra.w   mlink
; ---------------------------------------------------------------------------
unlinkob:
                movea.l 0(a0),a1
                movea.l $04(a0),a2
                addi.w  #1,(ofree).l
                lea     (freeobje).l,a3
                lea     (activeob).l,a4

mlink:
                cmpa.l  #$FFFFFFFF,a1
                bne.w   ML1
                move.l  a2,(a4)
                cmpa.l  a1,a2
                beq.w   NewLink

ML1:
                cmpa.l  #$FFFFFFFF,a2
                bne.w   ML2
                move.l  a2,$04(a1)
                bra.w   NewLink
; ---------------------------------------------------------------------------

ML2:
                cmpa.l  #$FFFFFFFF,a1
                beq.w   ml3
                move.l  a2,$04(a1)

ml3:
                move.l  a1,(a2)

NewLink:
                                        ; ROM:001965DC↑j
                movea.l (a3),a4
                cmpa.l  #$FFFFFFFF,a4
                bne.w   NL1
                move.l  a0,(a3)
                move.l  #$FFFFFFFF,(a0)
                move.l  #$FFFFFFFF,$04(a0)
                clr.w   d0
                rts
; ---------------------------------------------------------------------------

NL1:
                move.l  #$FFFFFFFF,(a0)
                move.l  a0,(a4)
                move.l  a4,$04(a0)
                move.l  a0,(a3)
                clr.w   d0
                rts

; =============== S U B R O U T I N E =======================================


pinertco:
                move.w  d0,d1
                move.w  #1,d3
                and.w   #3,d1
                beq.w   friction
                bra.w   uuu


; =============== S U B R O U T I N E =======================================


inertcon:
                                        ; sub_1928D4+45E↑p ...
                move.w  #1,d3
                move.w  d0,d1
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
; ---------------------------------------------------------------------------

nolim0:
                clr.w   d3

nolim1:
                move.l  $04(a0),d0
                move.l  $18(a0),d2
                neg.l   d2
                cmp.l   d2,d0
                bmi.w   inmove
                move.l  $08(a0),d0
                sub.l   d0,$04(a0)

inmove:
                                        ; sub_196634+90↓j ...
                move.l  $04(a0),d0
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
; ---------------------------------------------------------------------------

ininc:
                move.l  (a0),d1
                move.l  vfb_ysca(a0),d2
                cmp.l   vfb_angl(a0),d2
                beq.w   nolim3
                cmp.l   d2,d1
                bpl.w   instop
                bra.w   nolim2
; ---------------------------------------------------------------------------

nolim3:
                clr.w   d3

nolim2:
                move.l  $04(a0),d0
                move.l  $18(a0),d2
                cmp.l   d2,d0
                bpl.s   inmove
                move.l  $08(a0),d0
                add.l   d0,$04(a0)
                bra.s   inmove
; ---------------------------------------------------------------------------

instop:
                                        ; sub_196634+5C↑j ...
                move.l  d2,(a0)

iinstop:
                                        ; sub_196622+DA↓j
                clr.l   $04(a0)
                rts
; End of function inertcon

; ---------------------------------------------------------------------------
; START OF FUNCTION CHUNK FOR pinertco

friction:
                move.l  vfb_ysca(a0),d0
                cmp.l   vfb_angl(a0),d0
                bne.w   derange
                clr.w   d3

derange:
                move.l  $04(a0),d0
                move.l  vfb_xsca(a0),d1
                move.l  d1,d4
                move.l  d0,d2
                bpl.w   sposk
                neg.l   d2
                neg.l   d4

sposk:
                cmp.l   d1,d2
                bmi.s   iinstop
                sub.l   d4,$04(a0)
                bra.w   inmove
; END OF FUNCTION CHUNK FOR pinertco
; ---------------------------------------------------------------------------
                lea     $08(a0),a0
                clr.l   d0
                clr.l   d4
                move.w  #1,d1
                move.w  #3,d7

s2n:
                move.b  -(a0),d2
                and.w   #$F,d2
                mulu.w  d1,d2
                add.l   d2,d0
                mulu.w  #$A,d1
                dbf     d7,s2n
                move.w  #1,d1
                move.w  #3,d7

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

; =============== S U B R O U T I N E =======================================


xxnum:
                                        ; sub_1941FA+100↑p ...
                move.l  d0,d2
                divu.w  #$2710,d2
                and.l   #$FFFF,d2
                move.w  d2,d3
                mulu.w  #$2710,d3
                sub.l   d3,d0
                move.w  #3,d3

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
; End of function xxnum

; ---------------------------------------------------------------------------
                move.w  #7,d0
                move.l  a2,d4
                lea     $20(a2),a3

gennett:
                move.w  #$1F,d1
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
                move.w  #3,d2

wbm:
                rol.l   #8,d3
                move.b  d3,(a2)+
                dbf     d2,wbm
                dbf     d0,gennett
                movea.l a3,a2
                move.l  a3,d0
                sub.l   d4,d0
                rts

; =============== S U B R O U T I N E =======================================


deltablo:
                                        ; sub_1941FA+D2↑p
                move.w  d5,-(sp)
                move.w  #7,d0
                move.l  a2,d4
                clr.w   d5
                move.w  #$17FF,d1

dblo2:
                move.b  (a1)+,d2
                cmp.b   #$FF,d5
                beq.w   fwrite2
                cmp.b   #0,d1
                beq.w   fwrite2
                cmp.b   (a0),d2
                beq.w   dblo3

fwrite2:
                                        ; sub_1967C6+1C↑j
                move.b  d5,d3
                lsl.w   #8,d3
                move.b  d2,d3
                move.w  d3,(a2)+
                move.b  #$FF,d5

dblo3:
                lea     SRCEN(a0),a0
                add.b   #1,d5
                dbf     d1,dblo2
                move.l  a2,d0
                sub.l   d4,d0
                move.w  (sp)+,d5
                rts
; End of function deltablo


; =============== S U B R O U T I N E =======================================


getmatri:

                move.w  #1,(skid).l
; End of function getmatri


; =============== S U B R O U T I N E =======================================


gm:

                lea     (matrix).l,a1
                move.w  #$35FF,d0

ivtb:
                move.l  #$FFFFFFFF,(a1)+
                dbf     d0,ivtb
                movea.l (byte_196DEF+$361).l,a0
                move.l  a0,d1
                lea     vfb_ysca(a0),a0
                move.w  (imatrix).l,d0
                lsl.w   #2,d0
                move.l  (a0,d0.w),d0
                sub.l   #$900000,d0
                add.l   d1,d0
                movea.l d0,a0
                lea     (matrix).l,a1
                movem.l a0-a1,-(sp)
                lea     (refblock).l,a0
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
; ---------------------------------------------------------------------------

taiga:
                                        ; gm+6A↑j
                move.w  #7,d5

rrest:
                move.l  a0,-(sp)
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
                                        ; gm+AA↑j
                dbf     d5,rrest
                bsr.w   skidoo
                clr.w   (bank_mod).l
                lea     (banclr).l,a0 ; "~g1:$20:     "
                bra.w   print
; End of function gm

; ---------------------------------------------------------------------------
prin:
                movem.l d0-a6,-(sp)
                movea.l d7,a0
                jsr     print
                movem.l (sp)+,d0-a6
                rts
; ---------------------------------------------------------------------------
printban:
                lea     (banset).l,a0 ; "~g1:$20:Bank>"
                bra.w   print

; =============== S U B R O U T I N E =======================================


blitcopy:
                                        ; ROM:0019545E↑p ...

; FUNCTION CHUNK AT 001962C0 SIZE 0000000C BYTES

                move.l  #$4020,d1
                move.l  d1,(A1_FLAGS).l
                move.l  d1,(A2_FLAGS).l
                move.l  a1,d2
                move.l  d2,d1
                and.l   #$FFFFFFF8,d2
                sub.l   d2,d1
                move.l  d2,(A1_BASE).l
                asr.l   #1,d1
                move.l  d1,(A1_PIXEL).l
                asr.l   #1,d0
                or.l    #$10000,d0
                move.l  d0,(B_COUNT).l
                move.l  a0,d0
                move.l  d0,d2
                and.l   #$FFFFFFF8,d0
                sub.l   d0,d2
                move.l  d0,(A2_BASE).l
                asr.l   #1,d2
                move.l  d2,(A2_PIXEL).l
                or.l    d1,d2
                beq.s   .aligned
                move.l  #$1800005,d0
                bra.s   .blit_go
; ---------------------------------------------------------------------------

.aligned:
                move.l  #$1800001,d0

.blit_go:
                move.l  d0,(B_CMD).l
                bra.w   WaitBlit
; End of function blitcopy


; =============== S U B R O U T I N E =======================================


monovert:
                movea.l a0,a1
                lea     2(a0),a0
                move.w  #$FFFF,d7
                move.w  #1,d0

icu:
                move.w  #1,d1

icu2:
                move.w  #1,d2

icu3:
                move.w  d2,d3
                lsl.w   #2,d3
                sub.w   #2,d3
                move.w  d3,(a0)+
                move.w  d1,d3
                lsl.w   #2,d3
                sub.w   #2,d3
                move.w  d3,(a0)+
                move.w  d0,d3
                lsl.w   #2,d3
                sub.w   #2,d3
                move.w  d3,(a0)+
                add.w   #1,d7
                dbf     d2,icu3
                dbf     d1,icu2
                dbf     d0,icu
                move.w  d7,(a1)
                lea     (spherobj).l,a0
                move.w  #$2F,(a0)+ ; '/'
                lea     (sines).l,a1 ; ;sines.bin
                move.w  #$F,d0
                clr.w   d1
; ---------------------------------------------------------------------------
gsphr:    dc.b $14
                dc.b $31 ; 1
                dc.b $10
                dc.b   0
; ---------------------------------------------------------------------------
                add.b   #$40,d1 ; '@'
                move.b  (a1,d1.w),d3
                sub.b   #$30,d1 ; '0'
                ext.w   d2
                ext.w   d3
                move.w  d2,(a0)+
                move.w  d3,(a0)+
                clr.w   (a0)+
                move.w  d2,(a0)+
                clr.w   (a0)+
                move.w  d3,(a0)+
                clr.w   (a0)+
                move.w  d2,(a0)+
                move.w  d3,(a0)+
                dbf     d0,gsphr
                rts
; End of function monovert


; =============== S U B R O U T I N E =======================================


makeclea:
                move.l  #$FFFFFFFF,(a0)+
                move.w  #$1FF,d0

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
; End of function makeclea


; =============== S U B R O U T I N E =======================================


makecube:
                lea     (cmask1).l,a3
                movea.l a0,a2
                lea     $04(a0),a0
                moveq   #$FFFFFFFF,d7
                move.w  #7,d0

iccu:
                move.w  #7,d1
                movea.l a3,a1

iccu2:
                move.w  #7,d2
                move.b  (a1)+,d6

iccu3:
                btst    d2,d6
                beq.w   nomatey
                move.w  d2,d3
                lsl.w   #2,d3
                sub.w   #$10,d3
                swap    d3
                move.l  d3,(a0)+
                move.w  d1,d3
                lsl.w   #2,d3
                sub.w   #$10,d3
                swap    d3
                move.l  d3,(a0)+
                move.w  d0,d3
                lsl.w   #2,d3
                sub.w   #$10,d3
                swap    d3
                move.l  d3,(a0)+
                move.w  d0,d4
                lsl.w   #5,d4
                move.w  d1,d5
                lsl.w   #1,d5
                or.w    d5,d4
                lsl.w   #8,d4
                move.w  d4,(a0)+
                move.w  d2,d4
                lsl.w   #4,d4
                lsl.w   #8,d4
                add.w   #$8000,d4
                move.w  d4,(a0)+
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
; ---------------------------------------------------------------------------

nomatey:
                move.w  #0,$E(a0)
                lea     $20(a0),a0
                bra.s   nexx
; End of function makecube

; ---------------------------------------------------------------------------
                dc.b $00,$01,$03,$02
; ---------------------------------------------------------------------------
ObTypes:    dc.b   0,$60,  1,$80,  0,  4,  0,  0

                                        ; ROM:00195CCE↑o ...
                dc.b   0,  5,  0,$F0,  0,  0,  0,  0
                dc.b   0,  1,  0,  8,  0,  0,  0,  0
                dc.b   0,$60,  0,$C0,  0,  4,  0,  0
                dc.b   0,$60,  0,$60,  0,  4,  0,  0
                dc.b   0,  5,  0,$70,  0,  0,  0,$10
                dc.b   0,$10,  0,$30,  0,  2,  0,  4
                dc.b   0,  8,  0, $A,  0,  4,  0,  0
;sines.bin

.include "sines.dat"
.include "sympad.dat"

versionp:     dc.b 'Virtual Light Machine v0.9//(c) 1994 Virtual Light Company Ltd.'

                dc.b '/Jaguar CD-ROM version/(c) 1994 Atari Corporation//FFT code by '
                dc.b 'ib2/Grafix code by Yak/~c100:',0
clearstr:     dc.b '@',0
banset:     dc.b '~g1:20:Bank>',0
banclr:     dc.b '~g1:20:     ',0
edithead:     dc.b '@~g1:1:Edit Mode~e3:4:',0

                dc.b '@~g1:1:Editing: Effect~e3:4:',0
symedhea:     dc.b '@~g1:1:Editing: Symmetry Generator~e3:3:',0

                dc.b '@~g1:1:Editing: Digital Video Feedback~e3:3:',0
wavedhea:     dc.b '@~g1:1:Editing: Wave Plotter~e3:3:',0

isphead1:     dc.b '@~g1:1:Spectrum and Triggers~e3:3:',0

isphead2:     dc.b '@~g1:1:Trigger Settings~e3:3:',0

isphead3:     dc.b '@~g1:1:Adjust width using joypad',0

                dc.b '@~g1:1:Adjust trigger minimum with pad',0
kpasshea:     dc.b '@~g1:1:Assign effect to keypad//Press the number key 1-9 to whi'

                dc.b 'ch//you want this effect attached',0
ogohead:     dc.b '@~g1:1:Object Giver Outer~e3:3:',0

adedhead:
                dc.b '@~g1:1:Edit ADSR channel settings~e3:3:',0
adedhead2:
                dc.b '@~g1:1:Edit ADSR envelope shape~e3:3:',0
                dc.b '@~g1:1:Attach and Adjust Waveforms~e3:3:',0
subfxhea:     dc.b '@~g1:1:Choose a subeffect slot to edit~e3:3:',0

awhead:     dc.b 'Attach and Adjust Waveforms//Press keys 1 to 8 to link waveform'

                dc.b 's//Use the joypad to change amplitude~g1:20:Press any FIRE butt'
                dc.b 'on to exit',0
wshead:     dc.b '@~g1:1:Edit basic waveforms~e3:3:',0

fxphead:     dc.b '@~g1:1:Choose fx page~e3:3:',0

                dc.b '~g1:20:<A> ADD    <B> EXIT   <C> SUB',0
                dc.b '@~g1:1:',0
                dc.b 'Standard Mode~c30:',0
unimp:     dc.b 'This function not yet implemented~c30:',0


asc_197595:     dc.b 'Edit this effect',0
                                        ; DATA XREF: ROM:off_198A9C↓o
                dc.b 'Assign this effect to keypad',0
                dc.b 'Change system settings',0
                dc.b 'Edit envelopes and triggers',0
                dc.b 'Delayline settings',0
                dc.b 'Compress and store matrix',0
                dc.b 'Reset save pointers in ROMulator',0
                dc.b 'Test matrix retrieve',0
asc_197659:     dc.b 'Spectrum and triggers',0
                                        ; DATA XREF: ROM:off_198A9C↓o
asc_19766F:     dc.b 'Edit source function',0
                                        ; DATA XREF: ROM:off_198AB4↓o
asc_197684:     dc.b 'Edit symmetry generator',0
                                        ; DATA XREF: ROM:off_198AB4↓o
asc_19769C:     dc.b 'Edit source waves',0
                                        ; DATA XREF: ROM:off_198AB4↓o
                dc.b 'Change effect',0
asc_1976BC:     dc.b 'Waveform attach (x)',0
                                        ; DATA XREF: ROM:off_198AD4↓o
asc_1976D0:     dc.b 'Waveform attach (y)',0
                                        ; DATA XREF: ROM:off_198AD4↓o
asc_1976E4:     dc.b 'Trigger 1',0      ; DATA XREF: ROM:off_198B7C↓o
asc_1976EE:     dc.b 'Trigger 2',0      ; DATA XREF: ROM:off_198B7C↓o
asc_1976F8:     dc.b 'Trigger 3',0      ; DATA XREF: ROM:off_198B7C↓o
asc_197702:     dc.b 'Trigger 4',0      ; DATA XREF: ROM:off_198B7C↓o
asc_19770C:     dc.b 'Trigger 5',0      ; DATA XREF: ROM:off_198B7C↓o
asc_197716:     dc.b 'Set Width',0      ; DATA XREF: ROM:off_198BAC↓o
asc_197720:     dc.b 'Set Trigger Minimum',0
                                        ; DATA XREF: ROM:off_198BAC↓o
asc_197734:     dc.b '~g1:20:<A> Edit   <B> Edit   <C> Back',0
                                        ; DATA XREF: ROM:off_198A9C↓o
                                        ; ROM:off_198AB4↓o ...
bline2:     dc.b '~g1:20:Joypad to select, any FIRE to edit',0

                                        ; ROM:00193DA2↑o
                dc.b '~g1:20:Up,Down to choose, L,R to change',0
bline5:     dc.b '~g1:18:Hold down b and use up,down to//change channel',0

                dc.b '@~g1:1:Delay line settings//U,D changes number L,R changes spac'
                dc.b 'ing~e3:7:',0
                dc.b '@~g1:1:Compressing matrix to ROM//Press any fire to exit',0
rsethead:     dc.b '@~g1:1:Reset ROM save pointers//Press any fire to exit',0

                dc.b 'Edit: ',0
eparm1:     dc.b '~g1:20:<A> Prev   <B> Menu   <C> Next',0

                                        ; ROM:00198A98↓o
eparm2:     dc.b '~g1:18:Press ~i+*~i- to attach waveforms',0

empt:     dc.b '<Empty>',0
symplane:     dc.b 'Editing: Symmetry Planes and Types//Press number keys to turn o'

                dc.b 'ff or on~g9:8:----- Rotational symmetry~g12:14:- Clear~g12:16:-'
                dc.b ' Invert',0
                dc.b 'Byte bitmap.  Press buttons to//change a bit.',0
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
dlo1:     dc.b 'Off',0
dlo2:     dc.b 'channel 1',0
dlo3:     dc.b 'channel 2',0
dlo4:     dc.b 'channel 3',0
dlo5:     dc.b 'channel 4',0
dlo6:     dc.b 'channel 5',0
dlo7:     dc.b 'channel 6',0
                dc.b 'FX page 1',0
                dc.b 'FX page 2',0
                .even
fxopt:    dc.w SRCEN
                dc.l $19, $775A0019, $7B0D0019, $405E0019, $7B170019
                dc.b $40, $84
availobj:    dc.w vfb_ysca
                dc.l $19, $775A0019, $79AD0019, $23800019, $79B50019, $23A20019
                dc.l $79C10019, $24DA0019, $79D50019, $24CA0019, $79E70019
                dc.l $24F20019, $79F60019, $257E0019, $7A030019, $258C0019
                dc.l $7A0C0019, $259A0019, $7A1F0019, $25EE0019, $7A2F0019
                dc.l $24760019, $7A3E0019, $24A00019, $7A4D0019, $25B60019
                dc.l $7A5D0019, $25A80019, $7AC10019, $25DA0019, $7A6D0019
                dc.l $25520019, $7A7D0019, $25000019, $7AA20019, $251E0019
                dc.l $7A8D0019
                dc.b $25, $44
avail2:    dc.w SRCEN
                dc.l $19, $775A0019, $7AA20019, $251E0019, $7AA90019
asc_197BE8:     dc.b '%h~g1:6:',0
                dc.b 'DELTABLOCK generated ',0
                dc.b ' bytes/',0
                dc.b '55296 bytes ---> ',0
asc_197C21:     dc.b 'Edit ADSR a',0    ; DATA XREF: ROM:off_198B34↓o
asc_197C2D:     dc.b 'Edit ADSR b',0    ; DATA XREF: ROM:off_198B34↓o
asc_197C39:     dc.b 'Edit ADSR c',0    ; DATA XREF: ROM:off_198B34↓o
asc_197C45:     dc.b 'A:',0             ; DATA XREF: ROM:off_198B54↓o
asc_197C48:     dc.b 'D:',0             ; DATA XREF: ROM:off_198B54↓o
asc_197C4B:     dc.b 'S:',0             ; DATA XREF: ROM:off_198B54↓o
asc_197C4E:     dc.b 'R:',0             ; DATA XREF: ROM:off_198B54↓o
asc_197C51:     dc.b '1:',0             ; DATA XREF: ROM:off_198AEC↓o
asc_197C54:     dc.b '2:',0             ; DATA XREF: ROM:off_198AEC↓o
asc_197C57:     dc.b '3:',0             ; DATA XREF: ROM:off_198AEC↓o
asc_197C5A:     dc.b '4:',0             ; DATA XREF: ROM:off_198AEC↓o
asc_197C5D:     dc.b '5:',0             ; DATA XREF: ROM:off_198AEC↓o
asc_197C60:     dc.b '6:',0             ; DATA XREF: ROM:off_198AEC↓o
asc_197C63:     dc.b '7:',0             ; DATA XREF: ROM:off_198AEC↓o
asc_197C66:     dc.b '8:',0             ; DATA XREF: ROM:off_198AEC↓o
                dc.b 'Sine wave          ',0
                dc.b 'Sawtooth wave      ',0
                dc.b 'Square wave        ',0
                dc.b 'Ramp               ',0
                dc.b 'Rectified sine wave',0
                dc.b 'Noise              ',0
                dc.b 'Constant           ',0
                dc.b 'User control Y     ',0
                dc.b 'User control X     ',0
wt:     dc.b '~g2:12:',0
                dcb.b 2,0
                dc.b $19
                dc.l $7C690019, $7C7D0019, $7C910019, $7CA50019, $7CB90019
                dc.l $7CCD0019, $7CE10019, $7CF50019
                dc.b $7D, 9
word_197D4A:    dc.w $1E
                dc.l $7540001E, $7644001E, $7748001E, $784C001E, $79500019
                dc.l $6F4C001E
                dcb.l 2,$7540001E
                dc.b $75, $40
padchars:    dc.w $2A87
                dc.b $84
                dc.b $81, $23, $89
                dc.b $86
                dc.b $83, $80, $88
                dc.b $85
                dc.b $82, 3, $B
                dc.b  $A
                dc.b 6, 9, 7
                dc.b   2
                dc.b 1, 3, $B
                dc.b   7
                dc.b 2, $A, 6
                dc.b   1
                dc.b 9,'@~g1:1:Unpacking matrix//',0
                dc.b 'Copying reference block/',0
                dc.b 'Unpacking base effect/',0
                dc.b 'Unpacking subsequent effect/',0
                dc.b 'Activating slot 1//',0
                dc.b 'Done~c50:',0
                dc.b $FF
                dcb.l 2,0
                dc.l $FF000000, 0
pbinfo:     dc.b 'Parameter not yet defined    ',0

                                        ; sub_192F26+6↑o ...
word_197E3E:    dc.w 0
                                        ; sub_193C5C+1C↑o ...
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
                dc.l $111E1213, $142A2B2C, $2D302E2F, $313206FF
dvfvars:   dc.l $1030506, $80B38FF
vars:     dc.l polyvars
                                        ; ROM:00193E32↑o
                                        ; "Draw a polygon object        "
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
polyvars:     dc.b 'Draw a polygon object        ',0

                dc.w $921
                dc.l $221C1516, $1718191A
                dc.b $26, $3D, $FF
sfvars:     dc.b 'Draw 3D starfield            ',0

                dc.b 9, $D, $20
                dc.l $3B3839FF
obvars:     dc.b 'Draw particle object         ',0

                dc.w $90D
                dc.l $3B0E2122, $38392E2F, $1C1D1F15, $161B2526
                dc.b $FF
pmvars:     dc.b 'Do particle motion           ',0

                dc.b $16
                dc.l $2023090D, $E251718, $1938152E
                dc.b $33, $FF
ringvars:     dc.b 'Draw a ring of pixels        ',0

                dc.l $91C1F20, $3536383B, $E1516FF
dvf_vars:     dc.b 'Digital Video Feedback area  ',0

                                        ; ROM:00198854↑o ...
                dc.w $103
                dc.l $506080B
                dc.b $38, $FF
wsu_vars:     dc.b 'Wave Surface Thang           ',0

                dc.l $90D1C1D, $33202322, $E3B3638
                dc.b $FF
monomapv:     dc.b 'Draw mono bitmap coloured    ',0

                dc.b 9
                dc.l $2E363815
                dc.b $26, $3D, $FF
monomapv2:      dc.b 'Draw mono bitmap i-shaded    ',0

                dc.b 9, $2E, $36
                dc.l $3815263D
                dc.b $FF
plas1var:     dc.b 'Colour plasma area type 1    ',0

                dc.b 9
                dc.l $2E363821
                dc.b $22, $3D, $FF
logovars:     dc.b 'Big Jaguar hardware sprite   ',0

                dc.b 9, $38, $15
                dc.b $FF
shuuvars:     dc.b 'Spectrum as intensities      ',0

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
option1:   dc.l star, bline1, op11, thisfx, op19, ispec

option2:   dc.l $20000, bline1, op21, foredit, op22, symedit

                dc.l op23, wsedit
option3:   dc.l star, bline1, op31, iawfx, op32, iawfy

option4:   dc.l $70000, bline3, op41, rrts, op42, rrts

                dc.l op43, rrts, op44, rrts, op45, rrts
                dc.l op46, rrts, op47, rrts, op48, rrts
option5:   dc.l $20000, bline1, op51, ahead2, op52, ahead2

                dc.l op53, ahead2
option6:   dc.l XADDINC, bline4, op61, rrts, op62, rrts

                dc.l op63, rrts, op64, rrts
option7:   dc.l $40000, bline1, op71, ispec2, op72, ispec2

                dc.l op73, ispec2, op74, ispec2, op75, ispec2
option8:   dc.l star, bline1, op81, ispec3, op82, ispec4

                dc.l $FF000000, $10070000, $A070000, $4070000, $FF070000
                dc.l $70000, $E070000, $8070000, $FF070000, $12070000
                dc.l $C070000, $6070000, 0
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
clut_sha:    dcb.w 2,0
                                        ; Frame+26↑r ...
lol:     dc.l bmobj
                dc.l skale, beasties, pobjs, mpobjs, mmasks, maxes
                dc.l absdelta, zerstart, avbank, envvals, _fsync, freerun
bmobj:     dc.l jlogo2
                dc.l jaglogo
                dc.l blokk
                dc.l blokk
                dc.l cubeobj, genobj, monoobj, spherobj, cmask1
                dc.b 0, $19
                dc.w $7E10, $19, $7E18, $19, $7E18, $04
                dcb.w 2,$FFFC
                dc.w 0
                dc.l $FFFEFFFE
                dcb.l 2,0
                dc.l $20001, $04, $30001
freerun:    dcb.w 2,0
                                        ; ROM:loc_19207A↑w ...
; font.bin
.include "font.dat"

jlogo2:
.incbin "images/jlogo2.cry"

blokk:   dc.l $1003F
                                        ; ROM:00198C68↑o
davesvec:     dc.l rrts
                                        ; ROM:00192044↑w ...
                dcb.l $20,0
                dc.l $18000000, 0
                dc.l $18000000, 0
                dc.l $18000000, 0
                dc.l $FF000000, 0
                dc.l $FF000000, 0
                dc.l $18000000, 0
                dc.l $18000000, 0
                dc.l $18000000, 0
awfbutts:   dc.l $FF000000, $10070000, $A070000, $4070000

                dc.b $19, 9
word_199A6A:    dc.w 0
                dc.l $FF070000, $E070000, $8070000, $FF070000, $12070000
                dc.l $C070000, $6070000
awfb2:   dc.l $FF000000, $10090000, $A090000, $4090000

                                        ; sub_1943FC+1CB4↑o
                dcb.l 2,$FF070000
                dc.l $E090000, $8090000, $FF070000, $12090000, $C090000
                dc.l $6090000
kpassbut:   dc.l $FF000000, $70C0000, $70A0000, $7080000, $FF0C0000

                dc.l $B0C0000, $B0A0000, $B080000, $FF070000, $90C0000
                dc.l $90A0000, $9080000
symbutts:   dc.l $80E0100, $5090000, $B090000, $7050000, $8100100

                dc.l $8080000, $70B0000, $B070000, $FF000000, $5070000
                dc.l $90B0000, $9050000, $400000, $11100, $1E7540
                dcb.l 2,0
                dc.l $16300, $1E7540
                dcb.l 2,0
                dc.l $3000, $1E7540
                dcb.l 2,0
                dc.l $C000, $1E7540
                dcb.l 2,0
                dc.l star, $1E7540
                dcb.l 2,0
                dc.l $E000, $1E7540
                dcb.l 2,0
                dc.l $C300, $1E7540
                dcb.l 2,0
                dc.l $F310, $1E7540, 0
pixcon:   dc.l $800000, 0
                                        ; symadj+4E↑r ...
                dc.l $1600, $3000, $FFFF00, 0
                dc.l $40000
piycon:   dc.l $800000, 0
                                        ; symadj+62↑r ...
                dc.l $1600, $3000, $FFFF00, 0
                dc.l $40000
adsra:   dc.l $A000100, $C0000500, $40B00, $100C000, $5000004, $C000200

                                        ; ROM:_ded2↑o
                dc.l $C0000500, $40000
py:   dc.l 0
px:   dc.l 0
delayf:    dc.w 0
                                        ; sub_192088+D76↑o ...
delayp:    dc.w 0
                                        ; sub_19510A↑r ...
dline:   dc.l 0
                                        ; sub_19510A+6↑r ...
delayt:    dc.w 0
                                        ; ROM:001943B2↑r ...
delayn:    dcb.w 9,0
                                        ; ROM:001943B8↑r ...
asc_199C14:     dc.b '~g3:5:                                 ',0

                                        ; ROM:0019439E↑o
                dcb.l 2,0
jaglogo:
.incbin "images/jaglogo.cry"
; vlm-grafix.cry
vlmlogo:
.incbin "images/vlmlogo.cry"
ixcon:   dc.l $800000
                                        ; yakedit:loc_192ED8↑r ...
dword_19B054:   dc.l 0
                                        ; sub_19546E+18↑w
                dc.l UPDA2, $20000, $FFFF00, 0
                dc.l $20000
iycon:   dc.l $800000
                                        ; yakedit:loc_192EEC↑r ...
dword_19B070:   dc.l 0
                dc.l UPDA2, $20000, $FFFF00, 0
                dc.l $20000
dlset:    dc.b 0, 6
word_19B08A:    dc.w 0
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

                END
