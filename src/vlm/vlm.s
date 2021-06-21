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

LaunchVLM:                             ; DATA XREF: sub_195E3A+2A↓o
                                        ; sub_195F78+2A↓o
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

goag:                             ; CODE XREF: ROM:00192084↓j
                clr.w   (freerun).l
                bsr.w   everythi
                bra.s   goag
; ---------------------------------------------------------------------------
                illegal

; =============== S U B R O U T I N E =======================================


everythi:                             ; CODE XREF: LaunchVLM+30↑p
                                        ; ROM:00192080↑p

; FUNCTION CHUNK AT 00192D4A SIZE 00000138 BYTES

                bsr.w   gogo
                tst.w   (freerun).l
                bne.w   nodoit
                jsr     iansdoit

nodoit:                             ; CODE XREF: sub_192088+A↑j
                clr.l   (udud).l
                clr.l   (aded).l
                clr.l   (cskr).l
                lea     (refblock).l,a6
                move.w  #5,d7

irb:                             ; CODE XREF: sub_192088+38↓j
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

zippo:                             ; CODE XREF: sub_192088+FA↑j
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

xx:                             ; CODE XREF: sub_192088+254↓j
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

iprep:                             ; CODE XREF: sub_192088+2B4↓j
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

ggg:                             ; CODE XREF: ROM:00192400↓j
                                        ; sub_1924DA+12↓j ...
                move.l  a6,(fxedbase).l
                move.l  a6,(fxobj).l
                addi.l  #4,(esp).l
                jmp     initedit
; END OF FUNCTION CHUNK FOR makecfb
; ---------------------------------------------------------------------------
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


gogo:                             ; CODE XREF: sub_192088↑p
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

tdo:                             ; CODE XREF: ROM:00192470↓j
                move.w  #$FF,d1

tdo2:                             ; CODE XREF: ROM:0019246C↓j
                move.w  d0,d2
                add.w   d1,d2
                move.b  d2,(a0)+
                dbf     d1,tdo2
                dbf     d0,tdo
                rts
; ---------------------------------------------------------------------------
                bsr.w   makecfb
                move.w  #1,thang(a6)
                move.w  #$60,($18).w ; '`'
                move.w  #$60,($1C).w ; '`'
                move.w  #$60,(vfb_xpos).w ; '`'
                move.w  #$60,(vfb_ypos).w ; '`'
                move.w  #$78,($08).w ; 'x'
                rts
; ---------------------------------------------------------------------------
                bsr.w   makecfb
                move.w  #2,thang(a6)
                move.w  #$30,($18).w ; '0'
                move.w  #$30,($1C).w ; '0'
                move.w  #$30,(vfb_xpos).w ; '0'
                move.w  #$30,(vfb_ypos).w ; '0'
                move.w  #$38,($08).w ; '8'
                rts
; ---------------------------------------------------------------------------
                bsr.w   gadd
                move.l  #1,$8C(a6)
                bra.w   fnop

; =============== S U B R O U T I N E =======================================


makecfb:                             ; CODE XREF: ROM:00192476↑p
                                        ; ROM:001924A0↑p

; FUNCTION CHUNK AT 00192386 SIZE 0000001C BYTES
; FUNCTION CHUNK AT 00193EB2 SIZE 0000003E BYTES
; FUNCTION CHUNK AT 0019443E SIZE 00000026 BYTES

                bsr.w   gadd
                move.l  #0,$8C(a6)

fnop:                             ; CODE XREF: ROM:001924D6↑j
                bsr.w   initdvf
                move.l  a6,(a0)
                bra.w   ggg
; End of function makecfb

; ---------------------------------------------------------------------------
                rts
; ---------------------------------------------------------------------------
                bsr.w   gadd
                bsr.w   initring
                move.l  a6,(a0)
                bra.w   ggg
; ---------------------------------------------------------------------------
                bsr.w   gadd
                clr.l   $04(a6)
                move.l  #$D,info(a6)
                move.l  #$60000,gpu(a6)
                move.l  a6,(a0)
                bra.w   ggg
; ---------------------------------------------------------------------------
                bsr.w   mpo
                move.l  #$80000,d_x(a6)
                move.l  #$80000,d_z(a6)
                move.l  #$80000,deltaz(a6)
                move.l  #$80000,gpu(a6)
                rts
; ---------------------------------------------------------------------------
                bsr.w   mpo
                move.l  #$70000,gpu(a6)
                rts

; =============== S U B R O U T I N E =======================================


mpo:                             ; CODE XREF: ROM:0019251E↑p
                                        ; ROM:00192544↑p
                bsr.w   makestar
                move.l  #$C,info(a6)
                move.l  #$50000,gpu(a6)
                rts
; End of function mpo

; ---------------------------------------------------------------------------
                bsr.w   makestar
                move.l  #$E,info(a6)
                move.l  #$90000,gpu(a6)
                rts

; =============== S U B R O U T I N E =======================================


makestar:                             ; CODE XREF: sub_192552↑p
                                        ; ROM:00192568↑p

; FUNCTION CHUNK AT 00192386 SIZE 0000001C BYTES

                bsr.w   gadd
                bsr.w   initpsf
                move.l  a6,(a0)
                bra.w   ggg
; End of function makestar

; ---------------------------------------------------------------------------
                bsr.w   gadd
                bsr.w   initwsur
                move.l  a6,(a0)
                bra.w   ggg

; =============== S U B R O U T I N E =======================================


makemono:                             ; CODE XREF: sub_1925B6↓p
                                        ; ROM:001925DA↓p ...

; FUNCTION CHUNK AT 00192386 SIZE 0000001C BYTES

                bsr.w   gadd
                bsr.w   initmono
                move.l  a6,(a0)
                bra.w   ggg
; End of function makemono

; ---------------------------------------------------------------------------
                bsr.w   mplaz1
                move.l  #$40004,gpu(a6)
                rts

; =============== S U B R O U T I N E =======================================


mplaz1:                             ; CODE XREF: ROM:001925A8↑p
                bsr.s   makemono
                move.l  #$40002,gpu(a6)
                move.l  #$2000,j(a6)
                move.l  #$2000,k(a6)
                move.l  #$A,info(a6)
                rts
; End of function mplaz1

; ---------------------------------------------------------------------------
                bsr.s   makemono
                move.l  #$40003,gpu(a6)
                move.l  #$B,info(a6)
                rts
; ---------------------------------------------------------------------------
                bsr.s   makemono
                move.l  #$40001,gpu(a6)
                rts

; =============== S U B R O U T I N E =======================================


gadd:                             ; CODE XREF: ROM:00192380↑p
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

zzzero:                             ; CODE XREF: sub_1925FA+14↑j
                movea.l (fx).l,a6
                movea.l (a6),a6
                move.w  (og).l,d0
                mulu.w  #$400,d0
                adda.l  d0,a6
                bra.w   ifxobj
; End of function gadd


; =============== S U B R O U T I N E =======================================


initmono:                             ; CODE XREF: sub_19259A+4↑p
                move.l  #6,info(a6)
                move.l  #$40000,gpu(a6)
                rts
; End of function initmono


; =============== S U B R O U T I N E =======================================


initwsur:                             ; CODE XREF: ROM:00192590↑p
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


initpsf:                             ; CODE XREF: sub_19257E+4↑p
                                        ; sub_192644↑p
                move.l  #3,plot_mod(a6)
                move.l  #0,gpu(a6)
                move.l  #2,info(a6)
                rts
; End of function initpsf


; =============== S U B R O U T I N E =======================================


initwave:                             ; CODE XREF: ROM:001923A6↑p
                move.l  #$20000,gpu(a6)
                move.l  #1,info(a6)
                rts
; End of function initwave


; =============== S U B R O U T I N E =======================================


initdvf:                             ; CODE XREF: makecfb:loc_1924E6↑p
                move.l  #4,info(a6)
                move.l  #$10000,gpu(a6)
                move.l  #$1AF078,i(a6)
                move.l  #$1E7540,sine_bas(a6)
                rts
; End of function initdvf


; =============== S U B R O U T I N E =======================================


initring:                             ; CODE XREF: ROM:001924F6↑p
                clr.l   _i1(a6)
                clr.l   _i2(a6)
                move.l  #9,gpu(a6)
                move.l  #3,info(a6)
                move.l  #2,plot_mod(a6)
                rts
; End of function initring


; =============== S U B R O U T I N E =======================================


wavelink:                             ; CODE XREF: sub_1943FC+EB8↓p
                                        ; ROM:00195508↓p
                movea.l (fxobj).l,a6
; End of function wavelink


; =============== S U B R O U T I N E =======================================


wlink:                             ; CODE XREF: ROM:001923B4↑p
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


symadj:                             ; DATA XREF: sub_192088+2E2↑o
                lea     (pad_now).l,a1
                bsr.w   doadsr
                tst.w   (vlm_mode).l
                beq.w   rrts
                lea     (pad_now).l,a1
                tst.w   (editing).l
                beq.w   shoop
                lea     ($1AE010).l,a1

shoop:                             ; CODE XREF: symadj+20↑j
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


doadsr:                             ; CODE XREF: symadj+6↑p
                tst.w   (vlm_mode).l
                bne.w   ago
                lea     (zero).l,a1

ago:                             ; CODE XREF: sub_19279E+6↑j
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


do_adsr:                             ; CODE XREF: sub_19279E+2A↑p
                                        ; sub_19279E+40↑p
                move.w  $08(a0),d1
                lea     (adsrvex).l,a4
                lsl.w   #2,d1
                movea.l (a4,d1.w),a4
                jmp     (a4)
; End of function do_adsr

; ---------------------------------------------------------------------------

adsrvex:                             ; DATA XREF: sub_1927F4+4↑o
                dc.l trigwait, attack, decay, sustain, release
                ;ori.b   #$1A,(a1)+
                ;ori.b   #$28,(a1)+ ; '('
                ;ori.b   #$5C,(a1)+ ; '\'
                ;ori.b   #$90,(a1)+
                ;ori.b   #$9E,(a1)+
                tst.l   d0
                beq.w   rrts

setatac:                             ; CODE XREF: ROM:001928A0↓j
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

setay:                             ; CODE XREF: ROM:00192848↑j
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

srel:                             ; CODE XREF: ROM:0019282A↑j
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


gkpad:                             ; CODE XREF: ROM:001928C6↑p
                lea     (ud_butts).l,a0
; End of function gkpad


; =============== S U B R O U T I N E =======================================


gkp:                             ; CODE XREF: ROM:0019555C↓p

; FUNCTION CHUNK AT 00192D2C SIZE 0000001E BYTES

                lea     (pad_now).l,a1
                tst.w   (editing).l
                beq.w   gnana
                movea.l ($1AF056).l,a3
                move.w  #$B,d0

flashem:                             ; CODE XREF: sub_1928D4+4A↓j
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

nxtk:                             ; CODE XREF: sub_1928D4+1C↑j
                                        ; sub_1928D4+24↑j ...
                lea     $04(a3),a3
                dbf     d0,flashem

gnana:                             ; CODE XREF: sub_1928D4+C↑j
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

gk1:                             ; CODE XREF: sub_1928D4+A0↓j
                movea.l (a0)+,a2
                btst    #0,d6
                beq.w   bodb
                tst.b   3(a3)
                bne.w   bodb
                bra.w   execute
; ---------------------------------------------------------------------------

bodb:                             ; CODE XREF: sub_1928D4+8A↑j
                                        ; sub_1928D4+92↑j
                lsr.w   #1,d6
                lea     $04(a3),a3
                dbf     d7,gk1
                move.w  2(a1),d6
                move.w  #7,d7

gk2:                             ; CODE XREF: sub_1928D4+C8↓j
                movea.l (a0)+,a2
                btst    #0,d6
                beq.w   bodb2
                tst.b   3(a3)
                bne.w   bodb2
                bra.w   execute
; ---------------------------------------------------------------------------

bodb2:                             ; CODE XREF: sub_1928D4+B2↑j
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

somebutt:                             ; CODE XREF: sub_1928D4+D6↑j
                move.l  d0,-(sp)
                movea.l (a0)+,a2
                cmp.l   #$2000,d0
                bne.w   notc
                jsr     (a2)

notc:                             ; CODE XREF: sub_1928D4+EC↑j
                move.l  (sp)+,d0
                move.l  d0,-(sp)
                movea.l (a0)+,a2
                cmp.l   #$2000000,d0
                bne.w   notb
                jsr     (a2)

notb:                             ; CODE XREF: sub_1928D4+FE↑j
                move.l  (sp)+,d0
                move.l  d0,-(sp)
                movea.l (a0)+,a2
                cmp.l   #$20000000,d0
                bne.w   nota
                jsr     (a2)

nota:                             ; CODE XREF: sub_1928D4+110↑j
                move.l  (sp)+,d0
                move.l  d0,-(sp)
                movea.l (a0)+,a2
                cmp.l   #$2002000,d0
                bne.w   notab
                jsr     (a2)

notab:                             ; CODE XREF: sub_1928D4+122↑j
                move.l  (sp)+,d0
                rts
; ---------------------------------------------------------------------------
                lea     ((byte_196DEF+$12D)).l,a3
                move.w  #2,(symed).l
                bra.w   gjoy
; ---------------------------------------------------------------------------
                lea     ((byte_196DEF+$13D)).l,a3
                move.w  #3,(symed).l
                bra.w   gjoy
; ---------------------------------------------------------------------------
                lea     ((byte_196DEF+$14D)).l,a3
                move.w  #4,(symed).l
                bra.w   *+4
; ---------------------------------------------------------------------------

gjoy:                             ; CODE XREF: sub_1928D4+13A↑j
                                        ; sub_1928D4+14C↑j ...
                move.l  (a1),d0
                move.l  d0,d1
                and.l   #$400000,d0
                beq.w   npleft
                movea.l (a3),a4
                jsr     (a4)

npleft:                             ; CODE XREF: sub_1928D4+16C↑j
                move.l  (a1),d0
                and.l   #$800000,d0
                beq.w   npright
                movea.l $04(a3),a4
                jsr     (a4)

npright:                             ; CODE XREF: sub_1928D4+17C↑j
                move.l  (a1),d0
                and.l   #$100000,d0
                beq.w   npup
                movea.l $08(a3),a4
                jsr     (a4)

npup:                             ; CODE XREF: sub_1928D4+18E↑j
                move.l  (a1),d0
                and.l   #$200000,d0
                beq.w   npdn
                movea.l vfb_xsca(a3),a4
                jsr     (a4)

npdn:                          ; CODE XREF: sub_1928D4+1A0↑j
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

execute:                             ; CODE XREF: sub_1928D4+96↑j
                                        ; sub_1928D4+BE↑j
                tst.w   (editing).l
                beq.w   exec1
                move.b  #$1C,3(a3)
                btst    #0,2(a3)
                beq.w   exec1
                move.b  #$20,3(a3) ; ' '

exec1:                             ; CODE XREF: sub_1928D4+1CE↑j
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

anteclr:                             ; CODE XREF: sub_1928D4+200↑j
                cmpi.l  #keydb,(routine).l
                beq.w   rrts
                move.l  (routine).l,(ov).l
                move.l  #keydb,(routine).l
                rts
; End of function gkp


; =============== S U B R O U T I N E =======================================


keydb:                             ; DATA XREF: gkp:loc_192AEA↑o
                                        ; sub_1928D4+22E↑o
                move.l  (pad_now).l,d1
                and.l   #$E00FF,d1
                bne.w   rrts
                move.l  (ov).l,(routine).l
                rts
; End of function keydb


; =============== S U B R O U T I N E =======================================


zapdel:                             ; CODE XREF: sub_192088+5E↑p
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

idli:                             ; CODE XREF: sub_192B2A+60↓j
                clr.l   info(a0)
                lea     $300(a0),a0
                dbf     d0,idli
                rts
; End of function zapdel


; =============== S U B R O U T I N E =======================================


setedit:                             ; CODE XREF: Frame+2E6↓p
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

setstat:                             ; CODE XREF: sub_192BC8+8↑j
                bsr.w   isymbut
                move.l  #ud_butts,(action).l
                rts
; End of function symclr

; ---------------------------------------------------------------------------
                bchg    #0,$A3(a6)
                rts
; ---------------------------------------------------------------------------
                bchg    #1,$A3(a6)
                rts
; ---------------------------------------------------------------------------
                bchg    #2,$A3(a6)
                rts
; ---------------------------------------------------------------------------
                bchg    #3,$A3(a6)
                rts
; ---------------------------------------------------------------------------
                bchg    #4,$A3(a6)
                rts
; ---------------------------------------------------------------------------
                bchg    #5,$A3(a6)
                rts
; ---------------------------------------------------------------------------
                bchg    #6,$A3(a6)
                rts
; ---------------------------------------------------------------------------
                bchg    #7,$A3(a6)
                rts
; ---------------------------------------------------------------------------
                bchg    #0,$A1(a6)
                rts
; ---------------------------------------------------------------------------
                bchg    #0,$F7(a6)
                rts
; ---------------------------------------------------------------------------
                bchg    #1,$F7(a6)
                rts
; ---------------------------------------------------------------------------
                bchg    #2,$F7(a6)
                rts
; ---------------------------------------------------------------------------
                bchg    #3,$F7(a6)
                rts
; ---------------------------------------------------------------------------
                bchg    #4,$F7(a6)
                rts
; ---------------------------------------------------------------------------
                bchg    #5,$F7(a6)
                rts
; ---------------------------------------------------------------------------
                bchg    #6,$F7(a6)
                rts
; ---------------------------------------------------------------------------
                bchg    #7,$F7(a6)
                rts
; ---------------------------------------------------------------------------
                move.w  (fframes).l,d2
                and.w   #7,d2
                bne.w   rrts
                addi.w  #1,$4A(a6)
                rts
; ---------------------------------------------------------------------------
                move.w  (fframes).l,d2
                and.w   #7,d2
                bne.w   rrts
                cmpi.w  #0,$4A(a6)
                beq.w   rrts
                subi.w  #1,$4A(a6)
                rts
; ---------------------------------------------------------------------------
                addi.l  #1,roscal2(a6)
                rts
; ---------------------------------------------------------------------------
                subi.l  #1,roscal2(a6)
                rts
; ---------------------------------------------------------------------------
                addi.l  #1,roscalei(a6)
                rts
; ---------------------------------------------------------------------------
                subi.l  #1,roscalei(a6)
                rts
; ---------------------------------------------------------------------------
                addi.l  #$10,roscale(a6)
                rts
; ---------------------------------------------------------------------------
                subi.l  #$10,roscale(a6)
                rts
; ---------------------------------------------------------------------------
                addi.l  #$1000,rsym_ste(a6)
                rts
; ---------------------------------------------------------------------------
                subi.l  #$1000,rsym_ste(a6)
                rts
; ---------------------------------------------------------------------------
                addi.l  #$10,rsym_ist(a6)
                rts
; ---------------------------------------------------------------------------
                subi.l  #$10,rsym_ist(a6)
                rts

; =============== S U B R O U T I N E =======================================


rxs:                             ; CODE XREF: sub_1928D4+1BA↑p
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

rys:                             ; CODE XREF: sub_1928D4+1C4↑j
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

titlescr:                             ; CODE XREF: sub_192088+2F2↑j
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

editloop:                             ; CODE XREF: sub_192088+DC4↓j
                                        ; sub_192088+DF8↓j
                clr.w   (moomoomo).l
                tst.w   (freerun).l
                beq.w   eloop
                rts
; ---------------------------------------------------------------------------

eloop:                             ; CODE XREF: sub_192088+DB8↑j
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


yakedit:                             ; CODE XREF: sub_192088+DCE↑p
                tst.w   (actime).l
                bmi.w   no_ac
                subi.w  #1,(actime).l
                bpl.w   no_ac
                lea     (clearstr).l,a0 ; "@"
                bsr.w   print

no_ac:                             ; CODE XREF: sub_192E82+6↑j
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

noact:                             ; CODE XREF: sub_192E82+3E↑j
                                        ; sub_192E82+62↓j
                move.l  (ixcon).l,d0
                cmp.l   (ixcon).l,d0
                bne.s   noact
                move.l  d0,(iixcon).l

noact2:                             ; CODE XREF: sub_192E82+76↓j
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

blib:                             ; CODE XREF: sub_192E82+84↑j
                movea.l (fxedbase).l,a6
                bsr.w   shad
                rts
; End of function yakedit


; =============== S U B R O U T I N E =======================================


elcon:                             ; CODE XREF: ROM:00193E58↓p
                                        ; ROM:00194010↓p ...
                move.l  a0,-(sp)
                lea     $08(a0),a0
                lea     (pbinfo).l,a2 ; "Parameter not yet defined    "
                lea     (elinx).l,a3
                lea     (eltxt).l,a5
                move.l  #$1E8054,(elvp).l
                move.w  #$3F,d1 ; '?'

elc1:                             ; CODE XREF: sub_192F26+28↓j
                clr.l   (a3)+
                dbf     d1,elc1
                lea     (elinx).l,a3
                move.w  #1,d4

elc:                             ; CODE XREF: sub_192F26+46↓j
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

ucan:                             ; CODE XREF: sub_192F26+60↑j
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

spect:                             ; CODE XREF: sub_192F26+9C↓j
                move.b  (a4)+,d5
                move.b  d5,(a5)+
                cmp.b   #$3A,d5 ; ':'
                bne.s   spect
                move.b  #$20,(a5)+ ; ' '
                move.b  #$28,(a5)+ ; '('
                move.b  SRCEN(a4),(a5)+
                move.b  d4,(a3,d7.w)
                lea     (a2,d2.w),a4

llink:                             ; CODE XREF: sub_192F26+E4↓j
                move.b  $1F(a4),d5
                and.w   #$FF,d5
                move.b  d4,(a3,d5.w)
                lsl.w   #3,d5
                move.w  d5,d6
                lsl.w   #2,d5
                add.w   d6,d5
                lea     (a2,d5.w),a4
                move.l  a4,-(sp)

lcolon:                             ; CODE XREF: sub_192F26+D2↓j
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

nolbit:                             ; CODE XREF: sub_192F26+8C↑j
                add.w   #1,d1
                cmp.w   d0,d1
                bne.w   elc

elcend:                             ; CODE XREF: sub_192F26+38↑j
                movea.l (sp)+,a0
                move.w  d1,(a0)
                move.w  #0,2(a0)
                move.l  #bline2,$04(a0) ; "~g1:$20:Joypad to select, any FIRE to ed"...
                rts
; End of function elcon


; =============== S U B R O U T I N E =======================================


ifxobj:                             ; CODE XREF: everythi:loc_1920B8↑p
                                        ; sub_192088+5A↑p ...
                movea.l a6,a1
                move.l  #$FF,d0

xxxa:                             ; CODE XREF: sub_193036+A↓j
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

zlop:                             ; CODE XREF: sub_193036+1E0↓j
                move.l  #$FFFF,(a5)+
                move.w  #0,(a4)+
                move.l  #0,(a3)+
                dbf     d0,zlop
                lea     $300(a6),a3
                lea     (oscbank).l,a4
                move.w  #$1F,d0

gbsb:                             ; CODE XREF: sub_193036+1F4↓j
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


startup:                             ; CODE XREF: sub_192402+48↑p
                jsr     VideoIni
                jsr     InitList
                move.l  (ddlist).l,d0
                move.w  #0,(ODP).l
                swap    d0
                move.l  d0,(OLP).l
                lea     (zerstart).l,a0
                move.l  #$794,d0
                lsr.l   #2,d0

cram:                             ; CODE XREF: sub_19324E+38↓j
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

mpsines:                             ; CODE XREF: sub_19324E+106↓j
                move.b  (a0)+,d1
                ext.w   d1
                move.w  d1,d2
                bpl.w   mrect1
                neg.w   d2

mrect1:                             ; CODE XREF: sub_19324E+F6↑j
                lsl.w   #1,d2
                move.b  d2,(a2)+
                add.w   #$80,d1
                move.b  d1,(a1)+
                dbf     d0,mpsines
                lea     (p_saw).l,a0
                lea     (p_ramp).l,a1
                clr.w   d0

gsaw:                             ; CODE XREF: sub_19324E+134↓j
                move.w  d0,d1
                move.w  d0,d2
                lsr.w   #1,d2
                move.b  d2,(a1)+
                sub.w   #$FF,d1
                bpl.w   gsaw1
                neg.w   d1

gsaw1:                             ; CODE XREF: sub_19324E+124↑j
                move.b  d1,(a0)+
                add.w   #2,d0
                cmp.w   #$200,d0
                blt.s   gsaw
                move.b  (p_saw).l,($1E7744).l
                lea     (p_square).l,a0
                move.w  #$FF,d0

gsqu:                             ; CODE XREF: sub_19324E+15A↓j
                clr.w   d1
                cmp.w   #$80,d0
                blt.w   gsqu2
                move.w  #$FF,d1

gsqu2:                             ; CODE XREF: sub_19324E+150↑j
                move.b  d1,(a0)+
                dbf     d0,gsqu
                move.b  (p_square).l,($1E7848).l
                clr.w   (palside).l
                clr.w   (paltop).l
                tst.w   (pal).l
                beq.w   notpal1
                move.w  #6,(palside).l
                move.w  #$1E,(paltop).l

notpal1:                          ; CODE XREF: sub_19324E+17A↑j
                rts
; End of function startup


; =============== S U B R O U T I N E =======================================


dumint:                             ; DATA XREF: sub_192402+36↑o
                move.w  #$101,(INT1).l
                move.w  #$101,(INT2).l
                rte
; End of function dumint

; ---------------------------------------------------------------------------
                move.w  (frames).l,d7

wsnc:                             ; CODE XREF: ROM:001933FC↓j
                cmp.w   (frames).l,d7
                beq.s   wsnc
                rts

; =============== S U B R O U T I N E =======================================


clrscree:                             ; CODE XREF: sub_19324E+A4↑p
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


makeit_t:                             ; CODE XREF: sub_192088+12E↑p
                                        ; sub_192088+15A↑p ...
                bsr.w   makeit
                move.w  #1,vfb_angl(a0)
                rts
; End of function makeit_t


; =============== S U B R O U T I N E =======================================


makeit:                             ; CODE XREF: sub_192088+EE↑p
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


copybloc:                             ; CODE XREF: sub_193036+20C↑p
                                        ; sub_19510A+18↓p ...
                movem.l d0/a0-a1,-(sp)

copb:                             ; CODE XREF: sub_193470+6↓j
                move.b  (a0)+,(a1)+
                dbf     d0,copb
                movem.l (sp)+,d0/a0-a1
                rts
; End of function copybloc

; ---------------------------------------------------------------------------
                dc.l alpha, beta
; =============== S U B R O U T I N E =======================================


thangg:                             ; CODE XREF: ROM:loc_1949F2↓p
                bsr.w   db
                movea.l (draw_rou).l,a0
                jsr     (a0)
                move.l  #1,(screen_r).l
                addi.l  #1,(fframes).l
                rts
; End of function thangg


; =============== S U B R O U T I N E =======================================


wfm:                             ; CODE XREF: ROM:001937AE↓j
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
                move.l  d1,(_ein_buf+8).l
                move.w  #5,(_fmode).l
                bra.w   fmodewai
; End of function wfm

; ---------------------------------------------------------------------------
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

shfr1:                             ; CODE XREF: ROM:00193562↑j
                move.w  d3,(bandl).l
                move.w  d6,(bandh).l
                swap    d6
                move.w  d3,d6

shfr:                             ; CODE XREF: ROM:001935B6↓j
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

isgrn0:                             ; CODE XREF: ROM:001935A2↑j
                swap    d6

isgrn:                             ; CODE XREF: ROM:0019359A↑j
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

zizz:                             ; CODE XREF: ROM:00193690↓j
                move.w  #$F0F0,d4
                move.l  (trig).l,d6
                btst    d5,d6
                beq.w   nggn
                move.w  #$8FF0,d4

nggn:                             ; CODE XREF: ROM:00193678↑j
                bsr.w   sblitblo
                add.w   #$10,d0
                add.w   #1,d5
                cmp.w   #5,d5
                bne.s   zizz
                rts
; ---------------------------------------------------------------------------
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

shar:                             ; CODE XREF: ROM:001936FA↓j
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


shad:                             ; CODE XREF: sub_192E82+9E↑p
                tst.l   (aded).l
                beq.w   rrts
                move.l  (aded).l,(_ein_buf).l
                move.w  #1,(_fmode).l
                bsr.w   fmodewai
                movea.l (draw_scr).l,a0
                movea.l (aded).l,a1
                lea     (unk_19375A).l,a2
                move.w  #$6E,d1 ; 'n'
                move.w  #$4C,d0 ; 'L'
                move.w  #7,d3
                move.w  #3,d5

shadsr:                             ; CODE XREF: sub_1936FE+56↓j
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
unk_19375A:     dc.b   0                ; DATA XREF: sub_1936FE+2C↑o
                dc.b $FF
                dc.b $F0
                dc.b $FF
                dc.b $8F
                dc.b $FF
                dc.b $80
                dc.b $FF
; ---------------------------------------------------------------------------
; START OF FUNCTION CHUNK FOR wfm

joyxy:                             ; CODE XREF: sub_1934AA+4A↑j
                bne.w   joyy
                move.w  (pixcon).l,d2
                lsr.w   #2,d2
                and.w   #$3F,d2 ; '?'
                add.w   d2,d0
                move.w  #1,d2

zeb:                             ; CODE XREF: sub_1934AA+2F8↓j
                move.w  #$88FF,d4
                movea.l (draw_scr).l,a0
                jmp     sblitblo
; ---------------------------------------------------------------------------

joyy:                             ; CODE XREF: wfm:loc_193762↑j
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
                tst.w   (_m).l
                beq.w   rrts
                bmi.w   wfm
                move.l  (mon1).l,d1
                move.l  (mon2).l,d2
                bpl.w   a_1
                cmp.l   d1,d2
                beq.w   rrts

a_1:                             ; CODE XREF: ROM:001937BE↑j
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

nox1:                             ; CODE XREF: ROM:001937F6↑j
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


fmodewai:                             ; CODE XREF: sub_1934AA+6A↑j
                                        ; ROM:001936B0↑p ...
                tst.w   (_fmode).l
                bne.s   fmodewai
                rts
; End of function fmodewai

; ---------------------------------------------------------------------------

w2nope:                             ; CODE XREF: ROM:0019383C↑j
                add.w   #$40,d1 ; '@'
                move.l  (draw_scr).l,(_ein_buf).l
                move.l  #$1E80F4,(_ein_buf+4).l
                swap    d1
                move.w  d0,d1

w2sngl:                             ; CODE XREF: ROM:00193912↑j
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


varadd:                             ; CODE XREF: sub_19546E+42↓p
                lsl.w   #3,d0
                move.w  d0,d2
                lsl.w   #2,d0
                add.w   d2,d0
                lea     (pbinfo).l,a0
                lea     word_197E3E-pbinfo(a0,d0.w),a0 ; "Parameter not yet defined    "
                rts
; End of function varadd

; ---------------------------------------------------------------------------

edvex:                             ; DATA XREF: sub_192E82+88↑o
                dc.l crot, snglx, sisnglx, sidbl, dvect, rrts, wsshow, shspec

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

n1add5:                             ; CODE XREF: ROM:00193AE0↑j
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

n2add5:                             ; CODE XREF: ROM:00193B0C↑j
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

nadd5:                             ; CODE XREF: ROM:00193B5E↑j
                or.w    d5,d4
                add.w   #1,d5
                move.w  d5,d2
                jmp     sblitblo
; ---------------------------------------------------------------------------
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
                movea.l (edwave).l,a1
                movea.l (draw_scr).l,a0
                move.w  #$50,d0 ; 'P'
                move.w  #$71,d1 ; 'q'
                move.w  #7,d5

wssh:                             ; CODE XREF: ROM:00193C14↓j
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


avx:                             ; CODE XREF: ROM:00193B20↑p
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


avxy:                             ; CODE XREF: ROM:0019399C↑p
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


lnkchk2:                             ; CODE XREF: sub_193C5C+60↑p
                move.w  $04(a1),d5
                bra.w   lnkch
; End of function lnkchk2


; =============== S U B R O U T I N E =======================================


lnkchk:                             ; CODE XREF: sub_193C1E+C↑p
                                        ; sub_193C5C+26↑p
                move.w  $04(a2),d5

lnkch:                             ; CODE XREF: sub_193CF2+4↑j
                lsl.w   #2,d5
                lea     UPDA1F(a3),a4
                tst.w   (a4,d5.w)
                beq.w   rrts
                lea     optionbu(a3),a3
                rts
; End of function lnkchk

; ---------------------------------------------------------------------------
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

novrt:                             ; CODE XREF: ROM:00193D2E↑j
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

nohrz:                          ; CODE XREF: ROM:00193D58↑j
                                        ; ROM:00193D60↑j
                rts
; ---------------------------------------------------------------------------
                movea.l (esp).l,a0
                move.l  #_thisfx,-(a0)
                move.l  a0,(esp).l

_thisfx:                             ; DATA XREF: ROM:00193D88↑o
                lea     (elspace).l,a0
                move.w  #5,(a0)+
                move.w  #0,(a0)+
                move.l  #bline2,(a0)+ ; "~g1:$20:Joypad to select, any FIRE to ed"...
                movea.l (fx).l,a1
                move.w  #5,d7

lfx:                             ; CODE XREF: ROM:loc_193DE6↓j
                move.l  (a1)+,d0
                bne.w   listit
                move.l  #empt,(a0)+ ; "<Empty>"
                move.l  #initedit,(a0)+
                bra.w   lfx2
; ---------------------------------------------------------------------------

listit:                             ; CODE XREF: ROM:00193DB4↑j
                movea.l d0,a2
                move.l  info(a2),d6
                sub.w   #1,d6
                lea     (vars).l,a3 ; "Draw a polygon object        "
                lsl.w   #2,d6
                movea.l (a3,d6.w),a3
                move.l  a3,(a0)+
                move.l  #edit2,(a0)+

lfx2:                             ; CODE XREF: ROM:00193DC4↑j
                dbf     d7,lfx
                lea     (elspace).l,a1
                lea     (subfxhea).l,a0 ; "@~g1:SRCEN:Choose a subeffect slot to edit~"...
                bra.w   giedit
; ---------------------------------------------------------------------------
                movea.l (esp).l,a0
                move.l  #fored,-(a0)
                move.l  a0,(esp).l

fored:                             ; DATA XREF: ROM:00193E00↑o
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
                move.l  a0,-(sp)
                lea     (elspace).l,a0
                move.w  #$10,d0
                bsr.w   elcon
                lea     (elspace).l,a1
                movea.l (sp)+,a0
                bra.w   giedit
; ---------------------------------------------------------------------------
                movea.l (esp).l,a0
                move.l  #_ded2,-(a0)
                move.l  a0,(esp).l

_ded2:                             ; DATA XREF: ROM:00193E6E↑o
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

ispec:                             ; CODE XREF: sub_1924DA+1F86↓j
                move.l  #isp1,(ledit).l
                movea.l (esp).l,a0
                move.l  #isp1,-(a0)
                move.l  a0,(esp).l

isp1:                             ; DATA XREF: makecfb:loc_193EB2↑o
                                        ; sub_1924DA+19E8↑o
                move.w  #8,(symed).l
                movea.l #isphead1,a0  ; "@~g1:SRCEN:Spectrum and Triggers~e3:3:"
                movea.l #option7,a1
                bsr.w   giedit
                move.w  #$E,(editing).l
                rts
; END OF FUNCTION CHUNK FOR makecfb
; ---------------------------------------------------------------------------
                move.l  #isp2,(ledit).l
                movea.l (esp).l,a0
                move.l  #isp2,-(a0)
                move.l  a0,(esp).l

isp2:                             ; DATA XREF: ROM:00193EF0↑o
                                        ; ROM:00193F00↑o
                move.w  #8,(symed).l
                movea.l #isphead2,a0  ; "@~g1:SRCEN:Trigger Settings~e3:3:"
                movea.l #option8,a1
                bsr.w   giedit
                rts
; ---------------------------------------------------------------------------
                move.l  #isp3,(ledit).l
                movea.l (esp).l,a0
                move.l  #isp3,-(a0)
                move.l  a0,(esp).l

isp3:                             ; DATA XREF: ROM:00193F26↑o
                                        ; ROM:00193F36↑o
                lea     (isphead3).l,a0 ; "@~g1:SRCEN:Adjust width using joypad"
                bsr.w   print
                move.w  #$F,(editing).l
                rts
; ---------------------------------------------------------------------------
                move.l  #isp4,(ledit).l
                movea.l (esp).l,a0
                move.l  #isp4,-(a0)
                move.l  a0,(esp).l

isp4:                             ; DATA XREF: ROM:00193F56↑o
                                        ; ROM:00193F66↑o
                lea     (isphead4).l,a0
                bsr.w   print
                move.w  #$10,(editing).l
                rts
; ---------------------------------------------------------------------------
                move.l  #wsed,(ledit).l
                movea.l (esp).l,a0
                move.l  #wsed,-(a0)
                move.l  a0,(esp).l

wsed:                             ; DATA XREF: ROM:00193F86↑o
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
                move.l  #dvfed,(ledit).l
                movea.l (esp).l,a0
                move.l  #dvfed,-(a0)
                move.l  a0,(esp).l

dvfed:                             ; DATA XREF: ROM:00193FDE↑o
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
                move.w  (selected).l,(og).l
                bra.w   *+4
; ---------------------------------------------------------------------------

inogg:                             ; CODE XREF: ROM:0019402E↑j
                move.l  #iogo,(ledit).l
                movea.l (esp).l,a0
                move.l  #iogo,-(a0)
                move.l  a0,(esp).l

iogo:                             ; DATA XREF: ROM:loc_194032↑o
                                        ; ROM:00194042↑o
                lea     (fxphead).l,a0 ; "@~g1:SRCEN:Choose fx page~e3:3:"
                lea     (fxopt).l,a1
                bra.w   giedit
; ---------------------------------------------------------------------------
                move.l  #ssp1,(ledit).l
                movea.l (esp).l,a0
                move.l  #ssp1,-(a0)
                move.l  a0,(esp).l

ssp1:                             ; DATA XREF: ROM:0019405E↑o
                                        ; ROM:0019406E↑o
                lea     (availobj).l,a1
                bra.w   doggo
; ---------------------------------------------------------------------------
                move.l  #ssp2,(ledit).l
                movea.l (esp).l,a0
                move.l  #ssp2,-(a0)
                move.l  a0,(esp).l

ssp2:                             ; DATA XREF: ROM:00194084↑o
                                        ; ROM:00194094↑o
                lea     (avail2).l,a1

doggo:                             ; CODE XREF: ROM:00194080↑j
                lea     (ogohead).l,a0 ; "@~g1:SRCEN:Object Giver Outer~e3:3:"
                bra.w   giedit
; ---------------------------------------------------------------------------
                move.l  #simedit,(ledit).l
                movea.l (esp).l,a0
                move.l  #simedit,-(a0)
                move.l  a0,(esp).l

simedit:                             ; DATA XREF: ROM:001940B0↑o
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

edit2:                             ; DATA XREF: ROM:00193DE0↑o
                movea.l (esp).l,a0
                move.l  #_edit2,-(a0)
                move.l  a0,(esp).l
                move.w  (selected).l,d0
                move.w  d0,(fxed).l
                move.w  d0,(og).l

_edit2:                             ; DATA XREF: ROM:001940FC↑o
                move.w  (fxed).l,d0
                lsl.w   #2,d0
                movea.l (fx).l,a0
                move.l  (a0,d0.w),(fxedbase).l
                lea     (edit2hea).l,a0
                lea     (option2).l,a1
                bra.w   giedit
; ---------------------------------------------------------------------------
                movea.l (esp).l,a0
                move.l  #_ah1,-(a0)
                move.l  a0,(esp).l

_ah1:                             ; DATA XREF: ROM:00194146↑o
                lea     (adedhead).l,a0
                clr.l   (aded).l
                lea     (option5).l,a1
                bra.w   giedit
; ---------------------------------------------------------------------------
                movea.l (esp).l,a0
                move.l  #_cmp1,-(a0)
                move.l  a0,(esp).l

_cmp1:                             ; DATA XREF: ROM:0019416E↑o
                lea     (stashhea).l,a0
                jsr     print
                bsr.w   dostash
                move.w  #$D,(editing).l
                rts
; ---------------------------------------------------------------------------
                movea.l (esp).l,a0
                move.l  #_rset,-(a0)
                move.l  a0,(esp).l

_rset:                             ; DATA XREF: ROM:0019419A↑o
                lea     (rsethead).l,a0 ; "@~g1:SRCEN:Reset ROM save pointers//Press a"...
                jsr     print
                move.l  #0,(datn).l
                move.l  #$900200,(datp).l
                move.l  #$900010,(dattp).l
                move.w  #$D,(editing).l
                rts
; ---------------------------------------------------------------------------
                movea.l (esp).l,a0
                move.l  #_mset,-(a0)
                move.l  a0,(esp).l

_mset:                             ; DATA XREF: ROM:001941E0↑o
                bsr.w   getmatri
                move.w  #$D,(editing).l
                rts

; =============== S U B R O U T I N E =======================================


dostash:                             ; CODE XREF: ROM:00194186↑p
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

remblox:                             ; CODE XREF: sub_1941FA+11C↓j
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
                movea.l (esp).l,a0
                move.l  #_edl,-(a0)
                move.l  a0,(esp).l

_edl:                             ; DATA XREF: ROM:0019435C↑o
                move.w  (delayf).l,d0
                bpl.w   eddd
                clr.w   d0

eddd:                             ; CODE XREF: ROM:0019436E↑j
                move.w  d0,(word_19B08A).l
                move.l  #dlset,(editlist).l
                lea     (edlhead).l,a0
                jsr     print
                move.w  #$C,(editing).l

udedg:                             ; DATA XREF: ROM:00195828↓o
                lea     ($199C14).l,a0
                lea     asc_199C14+6-asc_199C14(a0),a1 ; "~g3:5:                                 "
                move.w  #$1F,d0

udedg1:                             ; CODE XREF: ROM:001943AA↓j
                move.b  #1,(a1)+
                dbf     d0,udedg1
                lea     6(a0),a1
                move.w  (delayt).l,d1
                move.w  (delayn).l,d2
                clr.w   d0

shado:                             ; CODE XREF: ROM:001943C6↓j
                add.w   d1,d0
                bsr.w   setblok
                dbf     d2,shado
                bra.w   print

; =============== S U B R O U T I N E =======================================


setblok:                             ; CODE XREF: ROM:001943C2↑p
                and.w   #$3F,d0 ; '?'
                move.w  d0,d3
                lsr.w   #1,d3
                lea     (a1,d3.w),a2
                move.b  (a2),d4
                sub.w   #1,d4
                btst    #0,d0
                beq.w   d0notodd
                add.w   #4,d4

d0notodd:                             ; CODE XREF: sub_1943CE+16↑j
                lea     (evens).l,a3
                and.w   #7,d4
                move.b  (a3,d4.w),(a2)
                rts
; End of function setblok


; =============== S U B R O U T I N E =======================================


evens:                             ; DATA XREF: setblok:loc_1943EC↑o

; FUNCTION CHUNK AT 0019528A SIZE 0000008C BYTES
; FUNCTION CHUNK AT 001960AA SIZE 0000001A BYTES

                dc.b $02,$02,$04,$04
                btst    d1,d4
                btst    d1,d4

wfa:                             ; DATA XREF: ROM:001956C2↓o
                bsr.w   estack
                movea.l (eddie).l,a2
                btst    #0,(a2)
                bne.w   rwfa
                clr.w   (symed).l
                bra.w   iawfx
; ---------------------------------------------------------------------------

rwfa:                             ; CODE XREF: sub_1943FC+16↑j
                                        ; DATA XREF: ROM:0019557A↓o
                lea     (wfahead).l,a0
                lea     (option3).l,a1
                clr.w   (symed).l
                move.w  #1,(cbuttf).l
                bra.w   ogiedit
; End of function evens

; ---------------------------------------------------------------------------
; START OF FUNCTION CHUNK FOR makecfb

initedit:                             ; CODE XREF: sub_1924DA-13E↑j
                                        ; DATA XREF: sub_192B90+2C↑o ...
                move.l  #$1E5CFC,(esp).l
                move.l  #0,(e_stac).l
                clr.w   (sec_cnt).l
                move.w  #1,(sec_en).l
                bra.w   ispec
; END OF FUNCTION CHUNK FOR makecfb

; =============== S U B R O U T I N E =======================================


old_edit:                             ; DATA XREF: sub_195AC6+3C↓o
                move.l  #$1E5CFC,(esp).l
                move.l  #0,(e_stac).l
                clr.w   (sec_en).l
                movea.l (esp).l,a0
                move.l  #_iied,-(a0)
                move.l  a0,(esp).l

_iied:                             ; DATA XREF: old_edit+20↑o
                lea     (edithead).l,a0 ; "@~g1:SRCEN:Edit Mode~e3:$04:"
                lea     (option1).l,a1
                clr.w   (symed).l
; End of function old_edit


; =============== S U B R O U T I N E =======================================


giedit:                             ; CODE XREF: ROM:00193DF6↑j
                                        ; ROM:00193E64↑j ...
                clr.w   (cbuttf).l

ogiedit:                             ; CODE XREF: sub_1943FC+3E↑j
                move.w  #$FFFF,(actime).l
                move.l  a1,(editlist).l
                bsr.w   print
                move.w  #1,(editing).l
                rts
; End of function giedit


; =============== S U B R O U T I N E =======================================


wtud:                             ; DATA XREF: ROM:00193FCA↑o
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

grint:                             ; CODE XREF: wtud+32↑j
                jsr     print

ud_selcu:                             ; DATA XREF: selup:loc_195AB2↓o
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


reedit:                             ; DATA XREF: ROM:loc_195A4E↓o

; FUNCTION CHUNK AT 00194594 SIZE 000000B4 BYTES
; FUNCTION CHUNK AT 0019466E SIZE 0000003A BYTES

                bsr.w   eunstack
                bra.w   eparam
; End of function reedit


; =============== S U B R O U T I N E =======================================


eunstack:                             ; CODE XREF: reedit↑p
                movea.l (esp).l,a0
                move.l  (a0)+,(editlist).l
                move.w  (a0)+,(selected).l
                move.w  (a0)+,(selectab).l
                move.l  a0,(esp).l
                rts
; End of function eunstack


; =============== S U B R O U T I N E =======================================


estack:                             ; CODE XREF: evens:wfa↑p
                movea.l (esp).l,a0
                move.w  (selectab).l,-(a0)
                move.w  (selected).l,-(a0)
                move.l  (editlist).l,-(a0)
                move.l  a0,(esp).l
                rts
; End of function estack

; ---------------------------------------------------------------------------
; START OF FUNCTION CHUNK FOR reedit

eparam:                             ; CODE XREF: reedit+4↑j
                                        ; DATA XREF: sub_192F26+56↑o
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

init_byt:                             ; CODE XREF: reedit+78↑j
                lea     (awfb2).l,a0
                lea     (padbits).l,a1
                move.l  _mtrig(a6),d2
                move.w  #7,d7

imabit:                             ; CODE XREF: reedit+C6↓j
                move.b  (a1)+,d1
                and.w   #$FF,d1
                lsl.w   #2,d1
                lea     (a0,d1.w),a2
                bclr    #7,2(a2)
                btst    #0,d2
                beq.w   imooff
                bset    #7,2(a2)

imooff:                             ; CODE XREF: reedit+B4↑j
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


init_sym:                             ; DATA XREF: ROM:001980EC↓o
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

crunt:                             ; CODE XREF: reedit+80↑j
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


ppmes:                             ; CODE XREF: sub_194648+1E↑p
                                        ; ROM:0019533E↓p
                movea.l #eparm1,a0  ; "~g1:$20:<A> Prev   <B> Menu   <C> Next"
                bra.w   print
; End of function ppmes


; =============== S U B R O U T I N E =======================================


isymbut:                             ; CODE XREF: symclr:loc_192BDC↑p
                                        ; sub_194648↑p
                lea     (symbutts).l,a0
                lea     (padbits).l,a1
                move.l  asym_fla(a6),d2
                move.w  #7,d7

isybit:                             ; CODE XREF: sub_1946B2+3C↓j
                move.b  (a1)+,d1
                and.w   #$FF,d1
                lsl.w   #2,d1
                lea     (a0,d1.w),a2
                bclr    #7,2(a2)
                btst    #0,d2
                beq.w   isooff
                bset    #7,2(a2)

isooff:                             ; CODE XREF: sub_1946B2+2A↑j
                move.b  #0,3(a2)
                lsr.w   #1,d2
                dbf     d7,isybit
                lea     vfb_angl(a0),a2
                bclr    #7,2(a2)
                btst    #8,d2
                beq.w   idun
                bset    #7,2(a2)

idun:                             ; CODE XREF: sub_1946B2+4E↑j
                move.l  #sympad,(cpad).l
                move.l  #symbutts,($1AF056).l
                rts
; End of function isymbut


; =============== S U B R O U T I N E =======================================


primexy:                             ; CODE XREF: reedit+138↑p
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


s16:                             ; CODE XREF: sub_194720+34↑p
                move.w  $04(a0),d0
                lsl.w   #2,d0
                lea     UPDA1F(a3),a4
                lea     (a3,d0.w),a3
                tst.w   (a4,d0.w)
                beq.w   shall
                lea     optionbu(a3),a3

shall:                             ; CODE XREF: sub_19478E+12↑j
                move.l  (a3),d7
                move.b  3(a0),d0
                btst    #7,d0
                beq.w   nsiggn
                move.l  d3,d4
                lsr.l   #1,d4
                add.l   d4,d7

nsiggn:                             ; CODE XREF: sub_19478E+24↑j
                and.w   #$7F,d0
                lsl.w   #2,d0
                lea     (vtypes).l,a3
                movea.l (a3,d0.w),a3
                jmp     (a3)
; End of function s16

; ---------------------------------------------------------------------------

vtypes:                             ; DATA XREF: sub_19478E+34↑o
                dc.l $1947da, $1947dc, $1947ec 
                rts
; ---------------------------------------------------------------------------
                swap    d7
                divu.w  d3,d7
                and.l   #$FFFF,d7
                lsl.l   #8,d7
                move.l  d7,(a2)
                rts
; ---------------------------------------------------------------------------
                lsr.l   #8,d3
                lsr.l   #4,d3
                divu.w  d3,d7
                lsl.l   #8,d7
                lsl.l   #4,d7
                and.l   #$FFFFFF,d7
                move.l  d7,(a2)
                rts

; =============== S U B R O U T I N E =======================================


unquit:                             ; DATA XREF: sub_192F26+64↑o
                lea     (unimp).l,a0 ; "This function not yet implemented~c30:"
                bra.w   eq
; ---------------------------------------------------------------------------

editquit:                             ; CODE XREF: dofass+44↓j
                                        ; DATA XREF: ROM:loc_19574A↓o
                lea     (normal).l,a0

eq:                             ; CODE XREF: unquit+6↑j
                bsr.w   cprint
                clr.w   (editing).l
                clr.l   (star_on).l
                move.w  (ovlm_mod).l,(vlm_mode).l
                rts
; End of function unquit


; =============== S U B R O U T I N E =======================================


cprint:                             ; CODE XREF: reedit+F4↑p
                                        ; sub_194648+1A↑p ...
                move.l  a0,-(sp)
                lea     (clearhom).l,a0
                bsr.w   print
                movea.l (sp)+,a0
; End of function cprint


; =============== S U B R O U T I N E =======================================


print:                             ; CODE XREF: sub_192088+B6↑p
                                        ; sub_192E82+1C↑p ...
                movea.l a0,a4
                movea.l #$1B00F8,a0
                move.w  (cursx).l,d0
                move.w  (cursy).l,d1
; End of function print


; =============== S U B R O U T I N E =======================================


prnt:                             ; CODE XREF: sub_19484E+18↓j
                                        ; sub_19484E+36↓j ...
                move.b  (a4)+,d2
                beq.w   strngend
                cmp.b   #$2F,d2 ; '/'
                bne.w   pcmd1
                add.w   #1,d1
                move.w  (cursx).l,d0
                bra.s   prnt
; ---------------------------------------------------------------------------

pcmd1:                             ; CODE XREF: sub_19484E+A↑j
                cmp.b   #$40,d2 ; '@'
                bne.w   pcmd2
                lea     (board).l,a0
                movem.w d0-d1,-(sp)
                jsr     cleol
                movem.w (sp)+,d0-d1
                bra.s   prnt
; ---------------------------------------------------------------------------

pcmd2:                             ; CODE XREF: sub_19484E+1E↑j
                cmp.b   #$5E,d2 ; '^'
                bne.w   pcmd3
                jsr     cleol
                bra.s   prnt
; ---------------------------------------------------------------------------

pcmd3:                             ; CODE XREF: sub_19484E+3C↑j
                cmp.b   #$60,d2 ; '`'
                bne.w   pcmd4
                sub.w   #1,d1
                move.w  (cursx).l,d0
                bra.s   prnt
; ---------------------------------------------------------------------------

pcmd4:                             ; CODE XREF: sub_19484E+4C↑j
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

setinv:                             ; CODE XREF: sub_19484E+82↑j
                move.b  (a4)+,d2
                cmp.b   #$2B,d2 ; '+'
                beq.w   invon
                clr.w   (inverse).l
                bra.w   prnt
; ---------------------------------------------------------------------------

invon:                             ; CODE XREF: sub_19484E+90↑j
                move.w  #1,(inverse).l
                bra.w   prnt
; ---------------------------------------------------------------------------

edtable:                             ; CODE XREF: sub_19484E+7A↑j
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

dedtab:                             ; CODE XREF: sub_19484E+106↓j
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

goto:                             ; CODE XREF: sub_19484E+72↑j
                bsr.w   gnum
                move.w  d2,d0
                bsr.w   gnum
                move.w  d2,d1
                bra.w   prnt
; ---------------------------------------------------------------------------

aclear:                             ; CODE XREF: sub_19484E+6A↑j
                bsr.w   gnum
                move.w  d2,(actime).l
                bra.w   prnt
; ---------------------------------------------------------------------------

pcmd5:                             ; CODE XREF: sub_19484E+60↑j
                jsr     charblit
                add.w   #1,d0
                bra.w   prnt
; ---------------------------------------------------------------------------

strngend:                             ; CODE XREF: sub_19484E+2↑j
                move.w  d0,(cursx).l
                move.w  d1,(cursy).l
                rts
; End of function prnt


; =============== S U B R O U T I N E =======================================


gnum:                             ; CODE XREF: sub_19484E+BC↑p
                                        ; sub_19484E+C8↑p ...
                clr.l   d2
                clr.l   d3

gnuml:                             ; CODE XREF: sub_1949BC+1C↓j
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

dodraw:                             ; CODE XREF: ROM:00194A44↓j
                move.l  #1,(screen_r).l
                move.w  #1,(db_on).l
                clr.w   (x_end).l

gog:                             ; CODE XREF: ROM:001949FC↓j
                bsr.w   thangg
                tst.w   (meltim).l
                bpl.s   gog
                move.l  #rrts,(routine).l
                rts

; =============== S U B R O U T I N E =======================================


db:                             ; CODE XREF: sub_193488↑p
                move.l  #1,(sync).l

dboo:                             ; CODE XREF: sub_194A0A+10↓j
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

mymelt:                             ; DATA XREF: ROM:00194A28↑o
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

lilmelt:                             ; CODE XREF: ROM:00194A64↑j
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

justfade:                             ; CODE XREF: ROM:00194B2A↑j
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
; ---------------------------------------------------------------------------
                rts

; =============== S U B R O U T I N E =======================================


Frame:                             ; DATA XREF: sub_19324E+6A↑o
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

ncc:                             ; CODE XREF: Frame+22↑j
                movem.l d6-d7/a3-a6,-(sp)
                movea.l (blist).l,a0
                movea.l (dlist).l,a1
                moveq   #$40,d0 ; '@'

xlst:                             ; CODE XREF: Frame+4A↓j
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

no_new_s:                             ; CODE XREF: Frame+58↑j
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

stdb:                             ; CODE XREF: Frame+AA↑j
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

no_db:                             ; CODE XREF: Frame+A0↑j
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

cflash:                             ; CODE XREF: Frame+EC↑j
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

dflash:                             ; CODE XREF: Frame+12E↑j
                                        ; Frame+13E↑j
                clr.w   (flash_db).l

eflash:                             ; CODE XREF: Frame+148↑j
                                        ; Frame+164↑j
                tst.w   (vlmtim).l
                bmi.w   ncanc
                subi.w  #1,(vlmtim).l
                bpl.w   ncanc
                move.w  #$FFFF,($1AE214).l

ncanc:                             ; CODE XREF: Frame+174↑j
                                        ; Frame+180↑j
                jsr     byte_196442+4
                tst.w   (seldb).l
                beq.w   do_ed
                move.l  (pad_now).l,d1
                or.l    ($1AE010).l,d1
                and.l   #$22FE20FF,d1
                bne.w   no_ksel
                clr.w   (seldb).l

do_ed:                             ; CODE XREF: Frame+198↑j
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

no_ed:                             ; CODE XREF: Frame+1BE↑j
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

nothash:                             ; CODE XREF: Frame+200↑j
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

pharty:                             ; CODE XREF: Frame+262↑j
                move.w  ($1AE026).l,(star_on).l
                move.w  d2,($1AE026).l
                move.w  #1,(seldb).l
                bra.w   no_ksel
; ---------------------------------------------------------------------------

knobby:                             ; CODE XREF: Frame+246↑j
                tst.l   (star_on).l
                beq.w   no_ksel
                move.w  (star_on).l,d0
                bne.w   setbth
                move.w  ($1AE026).l,(skid).l
                move.l  #skidoo,(action).l
                bra.w   n_ks
; ---------------------------------------------------------------------------

setbth:                             ; CODE XREF: Frame+296↑j
                sub.w   #1,d0
                move.w  d0,(imatrix).l
                move.w  ($1AE026).l,(skid).l
                move.l  #gm,(action).l

n_ks:                             ; CODE XREF: Frame+2AE↑j
                clr.l   (star_on).l

no_ksel:                             ; CODE XREF: Frame+1AE↑j
                                        ; Frame+1D2↑j ...
                move.l  (pad_now).l,d0
                cmp.l   #$90018,d0
                bne.w   nse1
                jsr     setedit
                bra.w   nse2
; ---------------------------------------------------------------------------

nse1:                             ; CODE XREF: Frame+2E2↑j
                clr.w   (vedit).l

nse2:                             ; CODE XREF: Frame+2EC↑j
                movea.l (_fx).l,a0
                jsr     (a0)
                tst.l   (action).l
                bne.w   gharbaj
                movea.l (routine).l,a0
                jsr     (a0)

gharbaj:                             ; CODE XREF: Frame+304↑j
                movem.l (sp)+,d6-d7/a3-a6

CheckTim:                             ; CODE XREF: Frame+10↑j
                move.w  (sp)+,d0
                move.w  d0,-(sp)
                btst    #3,d0
                beq.w   *+4

exxit:                             ; CODE XREF: Frame+31C↑j
                move.w  (sp)+,d0
                lsl.w   #8,d0
                move.b  (intmask).l,d0
                move.w  d0,(INT1).l
                move.w  d0,(INT2).l
                movem.l (sp)+,d0-d5/a0-a2
                rte
; End of function Frame

; ---------------------------------------------------------------------------
                move.w  #1,d0
                bra.w   selett
; ---------------------------------------------------------------------------
                move.w  #2,d0
                bra.w   selett
; ---------------------------------------------------------------------------
                move.w  #3,d0
                bra.w   selett
; ---------------------------------------------------------------------------
                move.w  #4,d0
                bra.w   selett
; ---------------------------------------------------------------------------
                move.w  #5,d0
                bra.w   selett
; ---------------------------------------------------------------------------
                move.w  #6,d0
                bra.w   selett
; ---------------------------------------------------------------------------
                move.w  #7,d0
                bra.w   selett
; ---------------------------------------------------------------------------
                move.w  #8,d0
                bra.w   selett
; ---------------------------------------------------------------------------
                move.w  #9,d0
                bra.w   selett
; ---------------------------------------------------------------------------
                move.w  #0,d0
                rts
; ---------------------------------------------------------------------------
                move.w  #1,d0
                rts
; ---------------------------------------------------------------------------
                move.w  #2,d0
                rts
; ---------------------------------------------------------------------------
                move.w  #3,d0
                rts
; ---------------------------------------------------------------------------
                move.w  #4,d0
                rts
; ---------------------------------------------------------------------------
                move.w  #5,d0
                rts
; ---------------------------------------------------------------------------
                move.w  #6,d0
                rts
; ---------------------------------------------------------------------------
                move.w  #7,d0
                rts
; ---------------------------------------------------------------------------
                move.w  #8,d0
                rts
; ---------------------------------------------------------------------------
                cmp.w   #7,d0
                bgt.w   rrts
                move.l  #getmatri,(action).l
                move.w  d0,(imatrix).l
                rts
; ---------------------------------------------------------------------------

selett:                             ; CODE XREF: ROM:00194FD0↑j
                                        ; ROM:00194FD8↑j ...
                move.l  #skidoo,(action).l
                move.w  d0,(skid).l
                rts

; =============== S U B R O U T I N E =======================================


skidoo:                             ; CODE XREF: gm+BC↓p
                                        ; DATA XREF: Frame+2A4↑o ...
                movea.l #$1AE550,a0
                move.w  (skid).l,d0
                sub.w   #1,d0
                mulu.w  #$1800,d0
                lea     (matrix).l,a1
                adda.l  d0,a1
                move.w  #5,d0

slett:                             ; CODE XREF: skidoo+5E↓j
                clr.l   (a0)
                tst.l   info(a1)
                beq.w   sletto
                move.l  a1,(a0)
                lea     $300(a1),a2
                move.w  #7,d2
                lea     (word_197D4A).l,a3

yuz:                             ; CODE XREF: skidoo+52↓j
                move.w  $E(a2),d3
                lsl.w   #2,d3
                cmp.w   #$18,d3
                beq.w   yuz2
                move.l  (a3,d3.w),$08(a2)

yuz2:                             ; CODE XREF: skidoo+44↑j
                lea     vfb_ysca(a2),a2
                dbf     d2,yuz

sletto:                             ; CODE XREF: skidoo+26↑j
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


dcopyblo:                             ; CODE XREF: skidoo+78↑p
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

editvex:                             ; DATA XREF: Frame+1D8↑o
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

fobb:                             ; CODE XREF: ROM:001951A0↑j
                and.l   #$200000,d1
                beq.w   padex
                subi.w  #$100,6(a0)
                bra.w   padex
; ---------------------------------------------------------------------------
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

wied1:                             ; CODE XREF: ROM:001951F6↑j
                cmpi.w  #$3F,2(a0) ; '?'
                bge.w   padex
                addi.w  #1,2(a0)
                bra.w   padex
; ---------------------------------------------------------------------------

speced2a:                             ; CODE XREF: ROM:001951F0↑j
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

speced2b:                             ; CODE XREF: ROM:0019521A↑j
                move.l  d1,d0
                and.l   #$400000,d0
                beq.w   speced2c
                tst.w   (a0)
                beq.w   padex
                subi.w  #1,(a0)
                subi.w  #1,2(a0)
                bra.w   padex
; ---------------------------------------------------------------------------

speced2c:                             ; CODE XREF: ROM:00195242↑j
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

iawfx:                             ; CODE XREF: sub_1943FC+20↑j
                bsr.w   iwf
                move.w  #8,(editing).l
                movea.l (cwed1).l,a0
                tst.w   UPDA1F(a0)
                bne.w   istat
                move.w  (cwave1).l,d1
                sub.w   #1,d1
                move.l  #$8000,d0
                bsr.w   wavelink

istat:                             ; CODE XREF: sub_1943FC+EA4↑j
                                        ; ROM:001954F8↓j ...
                lea     (awfbutts).l,a1
                lea     (awfb2).l,a4
                lea     (pad8bits).l,a2
                move.w  #7,d0
                move.w  UPDA1F(a0),d1

issi:                             ; CODE XREF: sub_1943FC+F04↓j
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

issi2:                             ; CODE XREF: sub_1943FC+EEE↑j
                btst    #8,d1
                beq.w   issi3
                bset    #7,(a5)

issi3:                             ; CODE XREF: sub_1943FC+EFA↑j
                lsr.w   #1,d1
                dbf     d0,issi
                clr.l   d0
                move.w  $102(a0),d0
                lsl.l   #8,d0
                move.l  d0,(ixcon).l
                bra.w   dud_butt
; END OF FUNCTION CHUNK FOR evens
; ---------------------------------------------------------------------------
                movea.l #kpasshea,a0  ; "@~g1:SRCEN:Assign effect to keypad//Press t"...
                jsr     print
                move.w  #7,(editing).l
                move.l  #(byte_196DEF+$6D),(cpad).l
                move.l  #kpassbut,($1AF056).l
                bsr.w   ppmes
                bra.w   ud_butts
; ---------------------------------------------------------------------------
                move.w  #1,(fass).l
                move.l  #dofass,(action).l
                rts
; ---------------------------------------------------------------------------
                move.w  #2,(fass).l
                move.l  #dofass,(action).l
                rts
; ---------------------------------------------------------------------------
                move.w  #3,(fass).l
                move.l  #dofass,(action).l
                rts
; ---------------------------------------------------------------------------
                move.w  #4,(fass).l
                move.l  #dofass,(action).l
                rts
; ---------------------------------------------------------------------------
                move.w  #5,(fass).l
                move.l  #dofass,(action).l
                rts
; ---------------------------------------------------------------------------
                move.w  #6,(fass).l
                move.l  #dofass,(action).l
                rts
; ---------------------------------------------------------------------------
                move.w  #7,(fass).l
                move.l  #dofass,(action).l
                rts
; ---------------------------------------------------------------------------
                move.w  #8,(fass).l
                move.l  #dofass,(action).l
                rts
; ---------------------------------------------------------------------------
                move.w  #9,(fass).l
                move.l  #dofass,(action).l
                rts

; =============== S U B R O U T I N E =======================================


dofass:                             ; DATA XREF: ROM:0019534E↑o
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

dfass:                             ; CODE XREF: ROM:00195468↓j
                move.l  (a5)+,d0
                bne.w   copyit
                move.l  #0,info(a4)
                bra.w   nxfass
; ---------------------------------------------------------------------------

copyit:                             ; CODE XREF: ROM:00195444↑j
                movea.l d0,a0
                movea.l a4,a1
                move.l  #$400,d0
                jsr     blitcopy

nxfass:                             ; CODE XREF: ROM:00195450↑j
                lea     UPDA2(a4),a4
                dbf     d5,dfass
                rts

; =============== S U B R O U T I N E =======================================


iwf:                             ; CODE XREF: evens:loc_19528A↑p
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
                lea     (pad_now).l,a1
                move.b  SRCEN(a1),d0
                rol.b   #4,d0
                movea.l (cwed2).l,a4
                bra.w   wset
; ---------------------------------------------------------------------------
                lea     (pad_now).l,a1
                move.b  SRCEN(a1),d0
                rol.b   #2,d0
                movea.l (cwed1).l,a4

wset:                             ; CODE XREF: ROM:00195522↑j
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
                move.w  #1,(antelope).l
                rts
; ---------------------------------------------------------------------------
                tst.w   (antelope).l
                bne.w   ant1
                bchg    #0,$101(a4)
                rts
; ---------------------------------------------------------------------------
                tst.w   (antelope).l
                bne.w   ant2
                bchg    #1,$101(a4)
                rts
; ---------------------------------------------------------------------------
                tst.w   (antelope).l
                bne.w   ant3
                bchg    #2,$101(a4)
                rts
; ---------------------------------------------------------------------------
                tst.w   (antelope).l
                bne.w   ant4
                bchg    #3,$101(a4)
                rts
; ---------------------------------------------------------------------------
                tst.w   (antelope).l
                bne.w   ant5
                bchg    #4,$101(a4)
                rts
; ---------------------------------------------------------------------------
                tst.w   (antelope).l
                bne.w   ant6
                bchg    #5,$101(a4)
                rts
; ---------------------------------------------------------------------------
                tst.w   (antelope).l
                bne.w   ant7
                bchg    #6,$101(a4)
                rts
; ---------------------------------------------------------------------------
                tst.w   (antelope).l
                bne.w   ant8
                bchg    #7,$101(a4)
                rts
; ---------------------------------------------------------------------------

ant1:                             ; CODE XREF: ROM:00195598↑j
                bchg    #0,UPDA1F(a4)

clant:                             ; CODE XREF: ROM:0019563E↓j
                                        ; ROM:00195646↓j ...
                move.w  #2,(antelope).l
                clr.w   (word_199A6A).l
                rts
; ---------------------------------------------------------------------------

ant2:                             ; CODE XREF: ROM:001955AA↑j
                bchg    #1,UPDA1F(a4)
                bra.s   clant
; ---------------------------------------------------------------------------

ant3:                             ; CODE XREF: ROM:001955BC↑j
                bchg    #2,UPDA1F(a4)
                bra.s   clant
; ---------------------------------------------------------------------------

ant4:                             ; CODE XREF: ROM:001955CE↑j
                bchg    #3,UPDA1F(a4)
                bra.s   clant
; ---------------------------------------------------------------------------

ant5:                             ; CODE XREF: ROM:001955E0↑j
                bchg    #4,UPDA1F(a4)
                bra.s   clant
; ---------------------------------------------------------------------------

ant6:                             ; CODE XREF: ROM:001955F2↑j
                bchg    #5,UPDA1F(a4)
                bra.s   clant
; ---------------------------------------------------------------------------

ant7:                             ; CODE XREF: ROM:00195604↑j
                bchg    #6,UPDA1F(a4)
                bra.s   clant
; ---------------------------------------------------------------------------

ant8:                             ; CODE XREF: ROM:00195616↑j
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

spn_butt:                             ; CODE XREF: ROM:00195688↑j
                move.l  (a1),d0
                and.l   #$10000,d0
                beq.w   pn_butte
                move.l  #wfa,(action).l
                clr.w   (editing).l
                bra.w   sdb
; ---------------------------------------------------------------------------

pn_butte:                             ; CODE XREF: ROM:001928CA↑j
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

slecset:                             ; CODE XREF: ROM:0019571A↑j
                                        ; ROM:00195734↓j ...
                move.w  d0,(selected).l
                bra.w   sted
; ---------------------------------------------------------------------------

prevexit:                             ; CODE XREF: ROM:001956FC↑j
                move.w  (selected).l,d0
                sub.w   #1,d0
                bpl.s   slecset
                move.w  (selectab).l,d0
                bra.s   slecset
; ---------------------------------------------------------------------------
                move.l  (a1),d0
                and.l   #$22002000,d0
                beq.w   rrts

bexit:                             ; CODE XREF: ROM:00195A6E↓j
                move.l  #editquit,(action).l
                clr.w   (symed).l
                rts
; ---------------------------------------------------------------------------

bbexit:                             ; CODE XREF: ROM:001956F0↑j
                move.l  (ledit).l,(action).l
                clr.w   (symed).l
                bra.w   sdb
; ---------------------------------------------------------------------------

xselup:                             ; CODE XREF: ROM:001958CE↓j
                bsr.w   selup
                move.l  #wtud,(action).l
                rts
; ---------------------------------------------------------------------------

xseldn:                             ; CODE XREF: ROM:001958DA↓j
                bsr.w   seldn
                move.l  #wtud,(action).l
                rts
; ---------------------------------------------------------------------------
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
; ---------------------------------------------------------------------------
                move.l  d1,d0
                and.l   #$200000,d0
                bsr.w   seldn

gnek:                             ; CODE XREF: ROM:001957E4↑j
                move.w  (selected).l,d0
                cmp.w   #1,d0
                bne.w   gnek2
                clr.w   d0

gnek2:                             ; CODE XREF: ROM:001957FE↑j
                sub.w   #1,d0
                move.w  d0,(delayf).l
                rts
; ---------------------------------------------------------------------------

spinc:                             ; CODE XREF: ROM:001957C2↑j
                addi.w  #1,(delayt).l

sic:                             ; CODE XREF: ROM:0019583C↓j
                andi.w  #$3F,(delayt).l ; '?'

spex:                             ; CODE XREF: ROM:0019584E↓j
                move.w  #1,(seldb).l
                move.l  #udedg,(action).l
                rts
; ---------------------------------------------------------------------------

spdec:                             ; CODE XREF: ROM:001957B6↑j
                subi.w  #1,(delayt).l
                bra.s   sic
; ---------------------------------------------------------------------------

sninc:                             ; CODE XREF: ROM:0019579E↑j
                addi.w  #1,(delayn).l

snc:                             ; CODE XREF: ROM:00195858↓j
                andi.w  #$3F,(delayn).l ; '?'
                bra.s   spex
; ---------------------------------------------------------------------------

sndec:                             ; CODE XREF: ROM:001957AA↑j
                subi.w  #1,(delayn).l
                bra.s   snc
; ---------------------------------------------------------------------------
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

padex:                             ; CODE XREF: ROM:001951AA↑j
                                        ; ROM:001951B4↑j ...
                move.l  (pad_now).l,d1
                and.l   #$22002000,d1
                bne.w   owwt
                rts
; ---------------------------------------------------------------------------

intdec:                             ; CODE XREF: ROM:00195892↑j
                subi.w  #$80,(a0)
                rts
; ---------------------------------------------------------------------------

intinc:                             ; CODE XREF: ROM:0019589E↑j
                addi.w  #$80,(a0)
                rts
; ---------------------------------------------------------------------------
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

_gk1:                             ; CODE XREF: ROM:0019591C↓j
                move.l  (a0)+,d0
                beq.w   _bodb
                btst    #0,d6
                beq.w   _bodb
                movea.l d0,a2
                jmp     (a2)
; ---------------------------------------------------------------------------

_bodb:                             ; CODE XREF: ROM:0019590A↑j
                                        ; ROM:00195912↑j
                lsr.w   #1,d6
                dbf     d7,_gk1
                move.w  2(a1),d6
                move.w  #7,d7

_gk2:                             ; CODE XREF: ROM:0019593C↓j
                move.l  (a0)+,d0
                beq.w   _bodb2
                movea.l d0,a2
                btst    #0,d6
                beq.w   _bodb2
                jmp     (a2)
; ---------------------------------------------------------------------------

_bodb2:                             ; CODE XREF: ROM:0019592A↑j
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

owwt:                             ; CODE XREF: ROM:001958AE↑j
                move.w  #1,(editing).l
                clr.w   (symed).l
                bra.w   onstak
; ---------------------------------------------------------------------------

zinc:                             ; CODE XREF: ROM:00195968↑j
                cmp.l   #$2000,d1
                bne.w   stinc
                subi.l  #$1000,(a0)

stanc:                             ; CODE XREF: ROM:0019599C↓j
                andi.l  #$1FFFFF,(a0)
                rts
; ---------------------------------------------------------------------------

stinc:                             ; CODE XREF: ROM:00195984↑j
                addi.l  #$1000,(a0)
                bra.s   stanc
; ---------------------------------------------------------------------------

swf1:                             ; DATA XREF: ROM:00198C00↓o
                move.w  #0,d0
; START OF FUNCTION CHUNK FOR swf2

swfe:                             ; CODE XREF: sub_1959B6+4↓j
                                        ; ROM:001959C0↓j ...
                bsr.w   git
                move.w  d0,$E(a0)
                move.l  #wtud,(action).l
                rts
; END OF FUNCTION CHUNK FOR swf2

; =============== S U B R O U T I N E =======================================


swf2:                             ; DATA XREF: ROM:00198C20↓o

; FUNCTION CHUNK AT 001959A2 SIZE 00000014 BYTES

                move.w  #1,d0
                bra.s   swfe
; End of function swf2

; ---------------------------------------------------------------------------

swf3:                             ; DATA XREF: ROM:00198C10↓o
                move.w  #2,d0
                bra.s   swfe
; ---------------------------------------------------------------------------

swf4:                             ; DATA XREF: ROM:00198BFC↓o
                move.w  #3,d0
                bra.s   swfe
; ---------------------------------------------------------------------------

swf5:                             ; DATA XREF: ROM:00198C1C↓o
                move.w  #4,d0
                bra.s   swfe
; ---------------------------------------------------------------------------

swf6:                             ; DATA XREF: ROM:00198C0C↓o
                move.w  #5,d0
                bra.s   swfe
; ---------------------------------------------------------------------------

swf7:                             ; DATA XREF: ROM:00198C18↓o
                move.w  #7,d0
                bra.s   swfe
; ---------------------------------------------------------------------------

swf8:                             ; DATA XREF: ROM:00198C08↓o
                move.w  #8,d0
                bra.s   swfe
; ---------------------------------------------------------------------------

swf9:                             ; DATA XREF: ROM:00198BF8↓o
                move.w  #6,d0
                bra.s   swfe
; ---------------------------------------------------------------------------

ph_inc:                             ; CODE XREF: ROM:001958F2↑j
                bsr.w   git
                addi.l  #$10000,(a0)
                rts
; ---------------------------------------------------------------------------

ph_dec:                             ; CODE XREF: ROM:001958E6↑j
                bsr.w   git
                subi.l  #$10000,(a0)
                rts

; =============== S U B R O U T I N E =======================================


git:                             ; CODE XREF: swf2:loc_1959A2↑p
                                        ; ROM:loc_1959E6↑p ...
                move.w  (selected).l,d2
                movea.l (edwave).l,a0
                lsl.w   #4,d2
                lea     (a0,d2.w),a0
                rts
; End of function git

; ---------------------------------------------------------------------------

selector:                             ; CODE XREF: ROM:00195286↑j
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

soxx:                             ; CODE XREF: ROM:00195576↑j
                move.l  #reedit,(action).l
                bra.w   sdb
; ---------------------------------------------------------------------------

onstak:                             ; CODE XREF: ROM:0019597A↑j
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

seuss:                             ; CODE XREF: ROM:00195A40↑j
                move.w  (selected).l,d0

sted:                             ; CODE XREF: ROM:00195726↑j
                lsl.w   #3,d0
                movea.l (editlist).l,a0
                move.l  $0C(a0,d0.w),(action).l
                clr.w   (editing).l
                bra.w   sdb

; =============== S U B R O U T I N E =======================================


selup:                             ; CODE XREF: ROM:loc_195770↑p
                                        ; ROM:001957E0↑p ...
                subi.w  #1,(selected).l
                bpl.w   sud
                move.w  (selectab).l,(selected).l

sud:                             ; CODE XREF: sub_195A9C+8↑j
                                        ; sub_195AC6+14↓j ...
                move.l  #ud_selcu,(action).l

sdb:                             ; CODE XREF: ROM:00195584↑j
                                        ; ROM:001956D2↑j ...
                move.w  #1,(seldb).l
                rts
; End of function selup


; =============== S U B R O U T I N E =======================================


seldn:                             ; CODE XREF: ROM:loc_195780↑p
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


InitBeas:                             ; CODE XREF: sub_19324E+42↑p
                lea     (beasties).l,a0
                move.w  #$C,d7
                move.w  d7,(nbeastie).l

ibeasts:                             ; CODE XREF: sub_195B0E+1A↓j
                move.w  #$FFFF,vfb_xsca(a0)
                lea     d_z(a0),a0
                dbf     d7,ibeasts
                rts
; End of function InitBeas

; ---------------------------------------------------------------------------
                clr.w   (snoop).l
                rts
; ---------------------------------------------------------------------------
                addi.w  #1,(snoop).l
                tst.l   ($804850).l
                bne.w   rrts

cow:                             ; CODE XREF: ROM:loc_195B48↓j
                bra.s   cow
; ---------------------------------------------------------------------------
                rts

; =============== S U B R O U T I N E =======================================


RunBeast:                             ; CODE XREF: Frame+4E↑p
                movea.l (blist).l,a0
                movea.l (dlist).l,a4
                lea     (beasties).l,a2
                move.w  (nbeastie).l,d7
                tst.w   (scron).l
                beq.w   RBeasts
                bmi.w   RBeasts
                sub.w   #1,d7
                lea     d_z(a2),a2

RBeasts:                             ; CODE XREF: sub_195B4C+1E↑j
                                        ; sub_195B4C+22↑j ...
                move.w  d7,-(sp)
                move.w  vfb_xsca(a2),d0
                bmi.w   nxbeast
                lea     (ModeVex).l,a3
                asl.w   #2,d0
                movea.l (a3,d0.w),a3
                jsr     (a3)

nxbeast:                             ; CODE XREF: sub_195B4C+34↑j
                move.w  (sp)+,d7
                lea     d_z(a2),a2
                dbf     d7,RBeasts
                bra.w   StopList
; End of function RunBeast

; ---------------------------------------------------------------------------

postfixu:                             ; DATA XREF: ROM:00195D0C↓o
                                        ; ROM:00195D92↓o
                dc.l  make_rmw, make_tra
make_rmw:
                lea     -$20(a0),a3
                bset    #6,$A(a3)
                bset    #7,$A(a3)

setref:                             ; CODE XREF: ROM:00195BDA↓j
                tst.w   $1E(a2)
                bne.w   setrf
                bclr    #0,9(a3)
                rts
; ---------------------------------------------------------------------------

setrf:                             ; CODE XREF: ROM:00195BBC↑j
                bset    #0,9(a3)
                rts
; ---------------------------------------------------------------------------
make_tra:
                lea     -$20(a0),a3
                bset    #7,$A(a3)
                bra.s   setref
; ---------------------------------------------------------------------------

ModeVex:                             ; DATA XREF: sub_195B4C+38↑o
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

clip00:                             ; CODE XREF: ROM:00195C58↑j
                clr.w   $E(a2)
                move.w  #1,(bo).l
                bra.w   clipc
; ---------------------------------------------------------------------------

clip1:                             ; CODE XREF: ROM:00195C26↑j
                                        ; ROM:00195C4E↑j
                clr.w   (bo).l

clipc:                             ; CODE XREF: ROM:00195C7C↑j
                move.w  (a2),d0
                move.w  $04(a2),d1
                cmpi.w  #5,$E(a2)
                bne.w   fixpal
                sub.w   (palside).l,d0
                add.w   (paltop).l,d1

fixpal:                             ; CODE XREF: ROM:00195C92↑j
                and.w   #$FFF,d0
                clr.w   d6
                tst.w   d1
                bpl.w   ponscr
                move.w  d1,d6
                neg.w   d6
                lsr.w   #1,d6

ponscr:                             ; CODE XREF: ROM:00195CAA↑j
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

nohang:                             ; CODE XREF: ROM:00195CEC↑j
                bsr.w   MakeUnSc
                move.w  vfb_angl(a2),d0
                bmi.w   rrts
                lea     (postfixu).l,a3
                asl.w   #2,d0
                movea.l (a3,d0.w),a3
                jmp     (a3)
; ---------------------------------------------------------------------------

clip2:                             ; CODE XREF: ROM:00195C6C↑j
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

sponscr:                             ; CODE XREF: ROM:00195D26↑j
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

snohang:                             ; CODE XREF: ROM:00195D72↑j
                bsr.w   MakeScal
                move.w  vfb_angl(a2),d0
                bmi.w   rrts
                lea     (postfixu).l,a3
                asl.w   #2,d0
                movea.l (a3,d0.w),a3
                jmp     (a3)

; =============== S U B R O U T I N E =======================================


InitList:                             ; CODE XREF: sub_19324E+6↑p
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


StopList:                             ; CODE XREF: sub_195B4C+50↑j
                                        ; sub_195DA0+68↑p ...
                move.w  #$F,d0

sl:                             ; CODE XREF: sub_195E24+10↓j
                move.l  #0,(a0)+
                move.l  #4,(a0)+
                dbf     d0,sl
                rts
; End of function StopList


; =============== S U B R O U T I N E =======================================


MakeScal:                             ; CODE XREF: ROM:loc_195D86↑p
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


nmulto2:                             ; CODE XREF: sub_195E3A+6↑j
                                        ; sub_195E3A+E↑p ...
                move.w  d0,-(sp)
                move.l  a0,d6
                and.w   #$1F,d6
                beq.w   muso
                move.l  a0,d6
                and.l   #$FFFFE0,d6
                movea.l d6,a0
                lea     $20(a0),a0

muso:                             ; CODE XREF: sub_195EC2+8↑j
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

khluj:                             ; CODE XREF: sub_195EC2+A0↑j
                move.l  d0,(a0)+
                lea     $08(a0),a0
                lea     $20(a4),a4
                rts
; End of function nmulto2


; =============== S U B R O U T I N E =======================================


MakeUnSc:                             ; CODE XREF: ROM:loc_195D00↑p
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


nmulto:                             ; CODE XREF: sub_195E3A+36↑p
                                        ; sub_195F78+6↑j ...
                move.l  a0,d6
                and.w   #$1F,d6
                beq.w   mumuso
                move.l  a0,d6
                and.l   #$FFFFE0,d6
                movea.l d6,a0
                lea     $20(a0),a0

mumuso:                             ; CODE XREF: sub_196000+6↑j
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

snoke:                             ; CODE XREF: sub_196000+5C↑j
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


udud_but:                             ; DATA XREF: ROM:00195556↑o
                movea.l (udud).l,a0
                bra.w   istat
; End of function udud_but

; ---------------------------------------------------------------------------
; START OF FUNCTION CHUNK FOR evens

dud_butt:                             ; CODE XREF: sub_1943FC+F16↑j
                move.l  ($1AF056).l,-(sp)
                move.l  #awfb2,($1AF056).l
                bsr.w   ud_butts
                move.l  (sp)+,($1AF056).l
; END OF FUNCTION CHUNK FOR evens

; =============== S U B R O U T I N E =======================================


ud_butts:                             ; CODE XREF: reedit+F8↑j
                                        ; sub_194648+22↑j ...
                move.w  (inverse).l,-(sp)
                movea.l ($1AF056).l,a3
                move.w  #$B,d3
                lea     (padchars).l,a4
                lea     (board).l,a0

ud_b:                             ; CODE XREF: ud_butts+52↓j
                move.b  (a4)+,d2
                tst.b   (a3)
                bmi.w   nud
                clr.w   (inverse).l
                btst    #7,2(a3)
                beq.w   ud_i
                move.w  #1,(inverse).l

ud_i:                             ; CODE XREF: ud_butts+30↑j
                move.b  (a3),d0
                move.b  SRCEN(a3),d1
                and.w   #$FF,d0
                and.w   #$FF,d1
                bsr.w   charblit

nud:                             ; CODE XREF: ud_butts+20↑j
                lea     $04(a3),a3
                dbf     d3,ud_b
                move.w  (sp)+,(inverse).l
                rts
; End of function ud_butts


; =============== S U B R O U T I N E =======================================


charblit:                             ; CODE XREF: wtud+58↑p
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

notinv:                             ; CODE XREF: sub_196122+96↑j
                move.l  d7,(B_CMD).l
                bsr.w   WaitBlit
                movem.w (sp)+,d0-d2
                rts
; End of function charblit


; =============== S U B R O U T I N E =======================================


cleol:                             ; CODE XREF: sub_192088+92↑p
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


sblitblo:                             ; CODE XREF: sub_1934AA+22↑p
                                        ; ROM:loc_1935AC↑p ...
                movem.w d0-d3,-(sp)
                move.w  (skale).l,d7
                lsr.w   d7,d0
                lsr.w   d7,d1
                lsr.w   d7,d2
                bne.w   slogg
                move.w  #1,d2

slogg:                             ; CODE XREF: sub_1961F8+10↑j
                lsr.w   d7,d3
                bne.w   slogg2
                move.w  #1,d3

slogg2:                             ; CODE XREF: sub_1961F8+1A↑j
                bsr.w   blitbloc
                movem.w (sp)+,d0-d3
                rts
; End of function sblitblo


; =============== S U B R O U T I N E =======================================


blitbloc:                             ; CODE XREF: sub_193400+10↑j
                                        ; ROM:00193D50↑p ...
                clr.w   (nphrase).l
                move.l  #$4420,d7
                move.l  #$10200,-(sp)

bblo:                             ; CODE XREF: sub_1961D2+22↑j
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

nnphr:                             ; CODE XREF: sub_196224+56↑j
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

WaitBlit:                             ; CODE XREF: ROM:00194A4E↑p
                                        ; ROM:00194B1E↑j ...
                move.l  (B_CMD).l,d7
                btst    #0,d7
                beq.s   WaitBlit
; END OF FUNCTION CHUNK FOR blitcopy

rrts:                          ; CODE XREF: sub_192402+6↑j
                                        ; symadj+10↑j ...
                rts
; ---------------------------------------------------------------------------
                dc.b $2E ; .
                dc.b $3C ; <
                dc.b   0
                dc.b   3
; ---------------------------------------------------------------------------
                neg.b   -(a0)
                bra.w   eec
; ---------------------------------------------------------------------------
                move.l  #$34220,d7

eec:                             ; CODE XREF: ROM:001962D4↑j
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


dcode:                             ; CODE XREF: Frame+256↑p
                lea     (codez).l,a3
                move.w  #$1F,d1

dco:                             ; CODE XREF: sub_19640C+12↓j
                move.b  (a3)+,d2
                btst    d1,d0
                bne.w   rrts
                dbf     d1,dco
                clr.w   d2
                rts
; End of function dcode

; ---------------------------------------------------------------------------

codez:                             ; DATA XREF: sub_19640C↑o
                moveq   #$78,d4 ; 'x'
; ---------------------------------------------------------------------------
                dc.b $41 ; A
                dc.b $50 ; P
; ---------------------------------------------------------------------------
                moveq   #$78,d4 ; 'x'
                clr.w   ($524C).w
                neg.w   (a5)
; ---------------------------------------------------------------------------
                dc.b $31 ; 1
                dc.b $34 ; 4
; ---------------------------------------------------------------------------
                move.w  $7878(a2),-(a3)
; ---------------------------------------------------------------------------
                dc.b $43 ; C
                dc.b $78 ; x
; ---------------------------------------------------------------------------
                moveq   #$78,d4 ; 'x'
; ---------------------------------------------------------------------------
                dc.b $4F ; O
                dc.b $78 ; x
; ---------------------------------------------------------------------------
                move.w  $30(a5,d3.l),d1
; ---------------------------------------------------------------------------
byte_196442:    dc.b  $33, $36, $39, $23, $48, $E7, $E0,   0
                                        ; CODE XREF: Frame:loc_194E1C↑p
                dc.b  $22, $3C, $F0, $FF, $FF, $FC
; ---------------------------------------------------------------------------
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
                move.l  #$FFFFFFFF,(activeob).l
                lea     (objects).l,a0
                move.w  #$3F,d0 ; '?'
                move.w  d0,(ofree).l
                move.l  a0,(freeobje).l
                movea.l #$FFFFFFFF,a1

IniA:                             ; CODE XREF: ROM:00196572↓j
                move.l  a1,(a0)
                movea.l a0,a1
                lea     d_z(a0),a0
                move.l  a0,$04(a1)
                dbf     d0,IniA
                move.l  #$FFFFFFFF,$04(a1)
                rts
; ---------------------------------------------------------------------------
                movea.l 0(a0),a1
                movea.l $04(a0),a2
                subi.w  #1,(ofree).l
                lea     (activeob).l,a3
                lea     (freeobje).l,a4
                bra.w   mlink
; ---------------------------------------------------------------------------
                movea.l 0(a0),a1
                movea.l $04(a0),a2
                addi.w  #1,(ofree).l
                lea     (freeobje).l,a3
                lea     (activeob).l,a4

mlink:                             ; CODE XREF: ROM:0019659C↑j
                cmpa.l  #$FFFFFFFF,a1
                bne.w   ML1
                move.l  a2,(a4)
                cmpa.l  a1,a2
                beq.w   NewLink

ML1:                             ; CODE XREF: ROM:001965C2↑j
                cmpa.l  #$FFFFFFFF,a2
                bne.w   ML2
                move.l  a2,$04(a1)
                bra.w   NewLink
; ---------------------------------------------------------------------------

ML2:                             ; CODE XREF: ROM:001965D4↑j
                cmpa.l  #$FFFFFFFF,a1
                beq.w   ml3
                move.l  a2,$04(a1)

ml3:                             ; CODE XREF: ROM:001965E6↑j
                move.l  a1,(a2)

NewLink:                             ; CODE XREF: ROM:001965CA↑j
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

NL1:                             ; CODE XREF: ROM:001965F8↑j
                move.l  #$FFFFFFFF,(a0)
                move.l  a0,(a4)
                move.l  a4,$04(a0)
                move.l  a0,(a3)
                clr.w   d0
                rts

; =============== S U B R O U T I N E =======================================


pinertco:                             ; CODE XREF: symadj+36↑p
                                        ; symadj+48↑p

; FUNCTION CHUNK AT 001966D8 SIZE 0000002E BYTES

                move.w  d0,d1
                move.w  #1,d3
                and.w   #3,d1
                beq.w   friction
                bra.w   uuu
; End of function pinertco


; =============== S U B R O U T I N E =======================================


inertcon:                             ; CODE XREF: sub_192D0E+6↑p
                                        ; sub_1928D4+45E↑p ...
                move.w  #1,d3
                move.w  d0,d1
                and.w   #3,d1
                beq.w   iinstop

uuu:                             ; CODE XREF: sub_196622+E↑j
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

nolim0:                             ; CODE XREF: sub_196634+20↑j
                clr.w   d3

nolim1:                             ; CODE XREF: sub_196634+2A↑j
                move.l  $04(a0),d0
                move.l  $18(a0),d2
                neg.l   d2
                cmp.l   d2,d0
                bmi.w   inmove
                move.l  $08(a0),d0
                sub.l   d0,$04(a0)

inmove:                             ; CODE XREF: sub_196634+3C↑j
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

ininc:                             ; CODE XREF: sub_196634+12↑j
                move.l  (a0),d1
                move.l  vfb_ysca(a0),d2
                cmp.l   vfb_angl(a0),d2
                beq.w   nolim3
                cmp.l   d2,d1
                bpl.w   instop
                bra.w   nolim2
; ---------------------------------------------------------------------------

nolim3:                             ; CODE XREF: sub_196634+76↑j
                clr.w   d3

nolim2:                             ; CODE XREF: sub_196634+80↑j
                move.l  $04(a0),d0
                move.l  $18(a0),d2
                cmp.l   d2,d0
                bpl.s   inmove
                move.l  $08(a0),d0
                add.l   d0,$04(a0)
                bra.s   inmove
; ---------------------------------------------------------------------------

instop:                             ; CODE XREF: sub_196634+26↑j
                                        ; sub_196634+5C↑j ...
                move.l  d2,(a0)

iinstop:                             ; CODE XREF: sub_196634+A↑j
                                        ; sub_196622+DA↓j
                clr.l   $04(a0)
                rts
; End of function inertcon

; ---------------------------------------------------------------------------
; START OF FUNCTION CHUNK FOR pinertco

friction:                             ; CODE XREF: sub_196622+A↑j
                move.l  vfb_ysca(a0),d0
                cmp.l   vfb_angl(a0),d0
                bne.w   derange
                clr.w   d3

derange:                             ; CODE XREF: sub_196622+BE↑j
                move.l  $04(a0),d0
                move.l  vfb_xsca(a0),d1
                move.l  d1,d4
                move.l  d0,d2
                bpl.w   sposk
                neg.l   d2
                neg.l   d4

sposk:                             ; CODE XREF: sub_196622+D0↑j
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

s2n:                             ; CODE XREF: ROM:00196724↓j
                move.b  -(a0),d2
                and.w   #$F,d2
                mulu.w  d1,d2
                add.l   d2,d0
                mulu.w  #$A,d1
                dbf     d7,s2n
                move.w  #1,d1
                move.w  #3,d7

s3n:                             ; CODE XREF: ROM:0019673E↓j
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


xxnum:                             ; CODE XREF: sub_1941FA+7A↑p
                                        ; sub_1941FA+100↑p ...
                move.l  d0,d2
                divu.w  #$2710,d2
                and.l   #$FFFF,d2
                move.w  d2,d3
                mulu.w  #$2710,d3
                sub.l   d3,d0
                move.w  #3,d3

xscr:                             ; CODE XREF: sub_19674A+28↓j
                divu.w  #$A,d0
                swap    d0
                add.b   #$30,d0 ; '0'
                move.b  d0,-(a4)
                clr.w   d0
                swap    d0
                dbf     d3,xscr

xscr2:                             ; CODE XREF: sub_19674A+3E↓j
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

gennett:                             ; CODE XREF: ROM:001967BA↓j
                move.w  #$1F,d1
                clr.l   d3

gnet:                             ; CODE XREF: ROM:loc_1967AA↓j
                move.b  (a1)+,d2
                bclr    d1,d3
                cmp.b   (a0)+,d2
                beq.w   gnet2
                bset    d1,d3
                move.b  d2,(a3)+

gnet2:                             ; CODE XREF: ROM:001967A2↑j
                dbf     d1,gnet
                move.w  #3,d2

wbm:                             ; CODE XREF: ROM:001967B6↓j
                rol.l   #8,d3
                move.b  d3,(a2)+
                dbf     d2,wbm
                dbf     d0,gennett
                movea.l a3,a2
                move.l  a3,d0
                sub.l   d4,d0
                rts

; =============== S U B R O U T I N E =======================================


deltablo:                             ; CODE XREF: sub_1941FA+60↑p
                                        ; sub_1941FA+D2↑p
                move.w  d5,-(sp)
                move.w  #7,d0
                move.l  a2,d4
                clr.w   d5
                move.w  #$17FF,d1

dblo2:                             ; CODE XREF: sub_1967C6+3A↓j
                move.b  (a1)+,d2
                cmp.b   #$FF,d5
                beq.w   fwrite2
                cmp.b   #0,d1
                beq.w   fwrite2
                cmp.b   (a0),d2
                beq.w   dblo3

fwrite2:                             ; CODE XREF: sub_1967C6+14↑j
                                        ; sub_1967C6+1C↑j
                move.b  d5,d3
                lsl.w   #8,d3
                move.b  d2,d3
                move.w  d3,(a2)+
                move.b  #$FF,d5

dblo3:                             ; CODE XREF: sub_1967C6+22↑j
                lea     SRCEN(a0),a0
                add.b   #1,d5
                dbf     d1,dblo2
                move.l  a2,d0
                sub.l   d4,d0
                move.w  (sp)+,d5
                rts
; End of function deltablo


; =============== S U B R O U T I N E =======================================


getmatri:                             ; CODE XREF: ROM:_mset↑p
                                        ; DATA XREF: ROM:00195052↑o
                move.w  #1,(skid).l
; End of function getmatri


; =============== S U B R O U T I N E =======================================


gm:                             ; CODE XREF: sub_192088+62↑p
                                        ; DATA XREF: Frame+2C6↑o
                lea     (matrix).l,a1
                move.w  #$35FF,d0

ivtb:                             ; CODE XREF: gm+10↓j
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

unp:                             ; CODE XREF: gm+76↓j
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

taiga:                             ; CODE XREF: gm+60↑j
                                        ; gm+6A↑j
                move.w  #7,d5

rrest:                             ; CODE XREF: gm:loc_1968CC↓j
                move.l  a0,-(sp)
                movea.l a1,a0
                lea     WID8(a1),a1
                move.l  #$1800,d0
                bsr.w   blitcopy
                movea.l (sp)+,a0
                lea     -2(a0),a0
                clr.w   d0
                clr.w   d1

unpk2:                             ; CODE XREF: gm+B6↓j
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

taiga2:                             ; CODE XREF: gm+A0↑j
                                        ; gm+AA↑j
                dbf     d5,rrest
                bsr.w   skidoo
                clr.w   (bank_mod).l
                lea     (banclr).l,a0 ; "~g1:$20:     "
                bra.w   print
; End of function gm

; ---------------------------------------------------------------------------
                movem.l d0-a6,-(sp)
                movea.l d7,a0
                jsr     print
                movem.l (sp)+,d0-a6
                rts
; ---------------------------------------------------------------------------
                lea     (banset).l,a0 ; "~g1:$20:Bank>"
                bra.w   print

; =============== S U B R O U T I N E =======================================


blitcopy:                             ; CODE XREF: dofass+3E↑p
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

.aligned:                             ; CODE XREF: sub_196900+56↑j
                move.l  #$1800001,d0

.blit_go:                             ; CODE XREF: sub_196900+5E↑j
                move.l  d0,(B_CMD).l
                bra.w   WaitBlit
; End of function blitcopy


; =============== S U B R O U T I N E =======================================


monovert:                             ; CODE XREF: sub_192088+86↑p
                movea.l a0,a1
                lea     2(a0),a0
                move.w  #$FFFF,d7
                move.w  #1,d0

icu:                             ; CODE XREF: sub_196970+40↓j
                move.w  #1,d1

icu2:                             ; CODE XREF: sub_196970+3C↓j
                move.w  #1,d2

icu3:                             ; CODE XREF: sub_196970+38↓j
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
gsphr:    dc.b $14                ; CODE XREF: sub_196970+82↓j
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


makeclea:                             ; CODE XREF: sub_192088+6E↑p
                move.l  #$FFFFFFFF,(a0)+
                move.w  #$1FF,d0

macle:                             ; CODE XREF: sub_1969F8+1A↓j
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


makecube:                             ; CODE XREF: sub_192088+7A↑p
                lea     (cmask1).l,a3
                movea.l a0,a2
                lea     $04(a0),a0
                moveq   #$FFFFFFFF,d7
                move.w  #7,d0

iccu:                             ; CODE XREF: sub_196A18+78↓j
                move.w  #7,d1
                movea.l a3,a1

iccu2:                             ; CODE XREF: sub_196A18+74↓j
                move.w  #7,d2
                move.b  (a1)+,d6

iccu3:                             ; CODE XREF: makecube:loc_196A88↓j
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

nexx:                             ; CODE XREF: sub_196A18+8A↓j
                dbf     d2,iccu3
                dbf     d1,iccu2
                dbf     d0,iccu
                move.l  d7,(a2)
                rts
; ---------------------------------------------------------------------------

nomatey:                             ; CODE XREF: sub_196A18+20↑j
                move.w  #0,$E(a0)
                lea     $20(a0),a0
                bra.s   nexx
; End of function makecube

; ---------------------------------------------------------------------------
                dc.b $00,$01,$03,$02
; ---------------------------------------------------------------------------
ObTypes:    dc.b   0,$60,  1,$80,  0,  4,  0,  0
                                        ; DATA XREF: sub_19342C+26↑o
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
                                        ; DATA XREF: sub_192088+A0↑o
                dc.b '/Jaguar CD-ROM version/(c) 1994 Atari Corporation//FFT code by '
                dc.b 'ib2/Grafix code by Yak/~c100:',0
clearstr:     dc.b '@',0              ; DATA XREF: sub_192E82+16↑o
banset:     dc.b '~g1:20:Bank>',0   ; DATA XREF: ROM:001968F6↑o
banclr:     dc.b '~g1:20:     ',0   ; DATA XREF: gm+C6↑o
edithead:     dc.b '@~g1:1:Edit Mode~e3:4:',0
                                        ; DATA XREF: old_edit:_iied↑o
                dc.b '@~g1:1:Editing: Effect~e3:4:',0
symedhea:     dc.b '@~g1:1:Editing: Symmetry Generator~e3:3:',0
                                        ; DATA XREF: ROM:001940EC↑o
                dc.b '@~g1:1:Editing: Digital Video Feedback~e3:3:',0
wavedhea:     dc.b '@~g1:1:Editing: Wave Plotter~e3:3:',0
                                        ; DATA XREF: ROM:00193E46↑o
isphead1:     dc.b '@~g1:1:Spectrum and Triggers~e3:3:',0
                                        ; DATA XREF: sub_1924DA+19FC↑o
isphead2:     dc.b '@~g1:1:Trigger Settings~e3:3:',0
                                        ; DATA XREF: ROM:00193F14↑o
isphead3:     dc.b '@~g1:1:Adjust width using joypad',0
                                        ; DATA XREF: ROM:isp3↑o
                dc.b '@~g1:1:Adjust trigger minimum with pad',0
kpasshea:     dc.b '@~g1:1:Assign effect to keypad//Press the number key 1-9 to whi'
                                        ; DATA XREF: ROM:00195316↑o
                dc.b 'ch//you want this effect attached',0
ogohead:     dc.b '@~g1:1:Object Giver Outer~e3:3:',0
                                        ; DATA XREF: ROM:loc_1940A6↑o
adedhead:
                dc.b '@~g1:1:Edit ADSR channel settings~e3:3:',0
adedhead2:
                dc.b '@~g1:1:Edit ADSR envelope shape~e3:3:',0
                dc.b '@~g1:1:Attach and Adjust Waveforms~e3:3:',0
subfxhea:     dc.b '@~g1:1:Choose a subeffect slot to edit~e3:3:',0
                                        ; DATA XREF: ROM:00193DF0↑o
awhead:     dc.b 'Attach and Adjust Waveforms//Press keys 1 to 8 to link waveform'
                                        ; DATA XREF: sub_19546E↑o
                dc.b 's//Use the joypad to change amplitude~g1:20:Press any FIRE butt'
                dc.b 'on to exit',0
wshead:     dc.b '@~g1:1:Edit basic waveforms~e3:3:',0
                                        ; DATA XREF: ROM:00193FC0↑o
fxphead:     dc.b '@~g1:1:Choose fx page~e3:3:',0
                                        ; DATA XREF: ROM:iogo↑o
                dc.b '~g1:20:<A> ADD    <B> EXIT   <C> SUB',0
                dc.b '@~g1:1:',0
                dc.b 'Standard Mode~c30:',0
unimp:     dc.b 'This function not yet implemented~c30:',0
                                        ; DATA XREF: unquit↑o
                dc.b 'Edit this effect',0
                dc.b 'Assign this effect to keypad',0
                dc.b 'Change system settings',0
                dc.b 'Edit envelopes and triggers',0
                dc.b 'Delayline settings',0
                dc.b 'Compress and store matrix',0
                dc.b 'Reset save pointers in ROMulator',0
                dc.b 'Test matrix retrieve',0
                dc.b 'Spectrum and triggers',0
                dc.b 'Edit source function',0
                dc.b 'Edit symmetry generator',0
                dc.b 'Edit source waves',0
                dc.b 'Change effect',0
                dc.b 'Waveform attach (x)',0
                dc.b 'Waveform attach (y)',0
                dc.b 'Trigger 1',0
                dc.b 'Trigger 2',0
                dc.b 'Trigger 3',0
                dc.b 'Trigger 4',0
                dc.b 'Trigger 5',0
                dc.b 'Set Width',0
                dc.b 'Set Trigger Minimum',0
                dc.b '~g1:20:<A> Edit   <B> Edit   <C> Back',0
bline2:     dc.b '~g1:20:Joypad to select, any FIRE to edit',0
                                        ; DATA XREF: sub_192F26+106↑o
                                        ; ROM:00193DA2↑o
                dc.b '~g1:20:Up,Down to choose, L,R to change',0
bline5:     dc.b '~g1:18:Hold down b and use up,down to//change channel',0
                                        ; DATA XREF: ROM:0019B08C↓o
                dc.b '@~g1:1:Delay line settings//U,D changes number L,R changes spac'
                dc.b 'ing~e3:7:',0
                dc.b '@~g1:1:Compressing matrix to ROM//Press any fire to exit',0
rsethead:     dc.b '@~g1:1:Reset ROM save pointers//Press any fire to exit',0
                                        ; DATA XREF: ROM:_rset↑o
                dc.b 'Edit: ',0
eparm1:     dc.b '~g1:20:<A> Prev   <B> Menu   <C> Next',0
                                        ; DATA XREF: sub_1946A8↑o
                                        ; ROM:00198A98↓o
eparm2:     dc.b '~g1:18:Press ~i+*~i- to attach waveforms',0
                                        ; DATA XREF: reedit+152↑o
empt:     dc.b '<Empty>',0        ; DATA XREF: ROM:00193DB8↑o
symplane:     dc.b 'Editing: Symmetry Planes and Types//Press number keys to turn o'
                                        ; DATA XREF: sub_194648+14↑o
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
dlo1:     dc.b 'Off',0            ; DATA XREF: ROM:0019B090↓o
dlo2:     dc.b 'channel 1',0      ; DATA XREF: ROM:0019B098↓o
dlo3:     dc.b 'channel 2',0      ; DATA XREF: ROM:0019B0A0↓o
dlo4:     dc.b 'channel 3',0      ; DATA XREF: ROM:0019B0A8↓o
dlo5:     dc.b 'channel 4',0      ; DATA XREF: ROM:0019B0B0↓o
dlo6:     dc.b 'channel 5',0      ; DATA XREF: ROM:0019B0B8↓o
dlo7:     dc.b 'channel 6',0      ; DATA XREF: ROM:0019B0C0↓o
                dc.b 'FX page 1',0
                dc.b 'FX page 2',0
                .even
fxopt:    dc.w SRCEN                  ; DATA XREF: ROM:00194054↑o
                dc.l $19, $775A0019, $7B0D0019, $405E0019, $7B170019
                dc.b $40, $84
availobj:    dc.w vfb_ysca                ; DATA XREF: ROM:ssp1↑o
                dc.l $19, $775A0019, $79AD0019, $23800019, $79B50019, $23A20019
                dc.l $79C10019, $24DA0019, $79D50019, $24CA0019, $79E70019
                dc.l $24F20019, $79F60019, $257E0019, $7A030019, $258C0019
                dc.l $7A0C0019, $259A0019, $7A1F0019, $25EE0019, $7A2F0019
                dc.l $24760019, $7A3E0019, $24A00019, $7A4D0019, $25B60019
                dc.l $7A5D0019, $25A80019, $7AC10019, $25DA0019, $7A6D0019
                dc.l $25520019, $7A7D0019, $25000019, $7AA20019, $251E0019
                dc.l $7A8D0019
                dc.b $25, $44
avail2:    dc.w SRCEN                  ; DATA XREF: ROM:ssp2↑o
                dc.l $19, $775A0019, $7AA20019, $251E0019, $7AA90019
asc_197BE8:     dc.b '%h~g1:6:',0       ; DATA XREF: sub_1941FA↑o
                dc.b 'DELTABLOCK generated ',0
                dc.b ' bytes/',0
                dc.b '55296 bytes ---> ',0
                dc.b 'Edit ADSR a',0
                dc.b 'Edit ADSR b',0
                dc.b 'Edit ADSR c',0
                dc.b 'A:',0
                dc.b 'D:',0
                dc.b 'S:',0
                dc.b 'R:',0
                dc.b '1:',0
                dc.b '2:',0
                dc.b '3:',0
                dc.b '4:',0
                dc.b '5:',0
                dc.b '6:',0
                dc.b '7:',0
                dc.b '8:',0
                dc.b 'Sine wave          ',0
                dc.b 'Sawtooth wave      ',0
                dc.b 'Square wave        ',0
                dc.b 'Ramp               ',0
                dc.b 'Rectified sine wave',0
                dc.b 'Noise              ',0
                dc.b 'Constant           ',0
                dc.b 'User control Y     ',0
                dc.b 'User control X     ',0
wt:     dc.b '~g2:12:',0        ; DATA XREF: wtud↑o
                dcb.b 2,0
                dc.b $19
                dc.l $7C690019, $7C7D0019, $7C910019, $7CA50019, $7CB90019
                dc.l $7CCD0019, $7CE10019, $7CF50019
                dc.b $7D, 9
word_197D4A:    dc.w $1E                ; DATA XREF: skidoo+34↑o
                dc.l $7540001E, $7644001E, $7748001E, $784C001E, $79500019
                dc.l $6F4C001E
                dcb.l 2,$7540001E
                dc.b $75, $40
padchars:    dc.w $2A87              ; DATA XREF: ud_butts+10↑o
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
                                        ; DATA XREF: sub_192088+D6C↑o
                                        ; sub_192F26+6↑o ...
word_197E3E:    dc.w 0                  ; DATA XREF: sub_193968+E↑o
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
dvfvars:   dc.l $1030506, $80B38FF ; DATA XREF: ROM:dvfed↑o
vars:     dc.l polyvars         ; DATA XREF: ROM:00193DD2↑o
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
                                        ; DATA XREF: ROM:vars↑o
                dc.w $921
                dc.l $221C1516, $1718191A
                dc.b $26, $3D, $FF
sfvars:     dc.b 'Draw 3D starfield            ',0
                                        ; DATA XREF: ROM:0019883C↑o
                dc.b 9, $D, $20
                dc.l $3B3839FF
obvars:     dc.b 'Draw particle object         ',0
                                        ; DATA XREF: ROM:00198864↑o
                dc.w $90D
                dc.l $3B0E2122, $38392E2F, $1C1D1F15, $161B2526
                dc.b $FF
pmvars:     dc.b 'Do particle motion           ',0
                                        ; DATA XREF: ROM:00198868↑o
                dc.b $16
                dc.l $2023090D, $E251718, $1938152E
                dc.b $33, $FF
ringvars:     dc.b 'Draw a ring of pixels        ',0
                                        ; DATA XREF: ROM:00198840↑o
                dc.l $91C1F20, $3536383B, $E1516FF
dvf_vars:     dc.b 'Digital Video Feedback area  ',0
                                        ; DATA XREF: ROM:00198844↑o
                                        ; ROM:00198854↑o ...
                dc.w $103
                dc.l $506080B
                dc.b $38, $FF
wsu_vars:     dc.b 'Wave Surface Thang           ',0
                                        ; DATA XREF: ROM:00198848↑o
                dc.l $90D1C1D, $33202322, $E3B3638
                dc.b $FF
monomapv:     dc.b 'Draw mono bitmap coloured    ',0
                                        ; DATA XREF: ROM:0019884C↑o
                dc.b 9
                dc.l $2E363815
                dc.b $26, $3D, $FF
monomapv2:      dc.b 'Draw mono bitmap i-shaded    ',0
                                        ; DATA XREF: ROM:00198850↑o
                dc.b 9, $2E, $36
                dc.l $3815263D
                dc.b $FF
plas1var:     dc.b 'Colour plasma area type 1    ',0
                                        ; DATA XREF: ROM:0019885C↑o
                dc.b 9
                dc.l $2E363821
                dc.b $22, $3D, $FF
logovars:     dc.b 'Big Jaguar hardware sprite   ',0
                                        ; DATA XREF: ROM:00198860↑o
                dc.b 9, $38, $15
                dc.b $FF
shuuvars:     dc.b 'Spectrum as intensities      ',0
                                        ; DATA XREF: ROM:0019886C↑o
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
                                        ; DATA XREF: old_edit+32↑o
option2:   dc.l $20000, bline1, op21, foredit, op22, symedit
                                        ; DATA XREF: ROM:00194136↑o
                dc.l op23, wsedit
option3:   dc.l star, bline1, op31, iawfx, op32, iawfy
                                        ; DATA XREF: sub_1943FC+2A↑o
option4:   dc.l $70000, bline3, op41, rrts, op42, rrts
                                        ; DATA XREF: ROM:00193FBA↑o
                dc.l op43, rrts, op44, rrts, op45, rrts
                dc.l op46, rrts, op47, rrts, op48, rrts
option5:   dc.l $20000, bline1, op51, ahead2, op52, ahead2
                                        ; DATA XREF: ROM:0019415E↑o
                dc.l op53, ahead2
option6:   dc.l XADDINC, bline4, op61, rrts, op62, rrts
                                        ; DATA XREF: ROM:00193E98↑o
                dc.l op63, rrts, op64, rrts
option7:   dc.l YADD1, bline1, op71, ispec2, op72, ispec2
                                        ; DATA XREF: sub_1924DA+1A02↑o
                dc.l op73, ispec2, op74, ispec2, op75, ispec2
option8:   dc.l star, bline1, op81, ispec3, op82, ispec4
                                        ; DATA XREF: ROM:00193F1A↑o
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
clut_sha:    dcb.w 2,0               ; DATA XREF: Frame+1C↑r
                                        ; Frame+26↑r ...
lol:     dc.l bmobj         ; DATA XREF: sub_192088+D44↑o
                dc.l skale, beasties, pobjs, mpobjs, mmasks, maxes
                dc.l absdelta, zerstart, avbank, envvals, _fsync, freerun
bmobj:     dc.l jlogo2       ; DATA XREF: ROM:lol↑o
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
freerun:    dcb.w 2,0               ; DATA XREF: LaunchVLM+20↑w
                                        ; ROM:loc_19207A↑w ...
; font.bin
.include "font.dat"

jlogo2:
.incbin "images/jlogo2.cry"

blokk:   dc.l $1003F             ; DATA XREF: ROM:00198C64↑o
                                        ; ROM:00198C68↑o
davesvec:     dc.l rrts      ; DATA XREF: LaunchVLM+E↑w
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
                                        ; DATA XREF: sub_19546E+6A↑o
                dc.b $19, 9
word_199A6A:    dc.w 0                  ; DATA XREF: ROM:00195630↑w
                dc.l $FF070000, $E070000, $8070000, $FF070000, $12070000
                dc.l $C070000, $6070000
awfb2:   dc.l $FF000000, $10090000, $A090000, $4090000
                                        ; DATA XREF: reedit+D4↑o
                                        ; sub_1943FC+1CB4↑o
                dcb.l 2,$FF070000
                dc.l $E090000, $8090000, $FF070000, $12090000, $C090000
                dc.l $6090000
kpassbut:   dc.l $FF000000, $70C0000, $70A0000, $7080000, $FF0C0000
                                        ; DATA XREF: ROM:00195334↑o
                dc.l $B0C0000, $B0A0000, $B080000, $FF070000, $90C0000
                dc.l $90A0000, $9080000
symbutts:   dc.l $80E0100, $5090000, $B090000, $7050000, $8100100
                                        ; DATA XREF: sub_1946B2+62↑o
                dc.l $8080000, $70B0000, $B070000, $FF000000, $5070000
                dc.l $90B0000, $9050000, padleft, $11100, p_sines
                dcb.l 2,0
                dc.l $16300, p_sines
                dcb.l 2,0
                dc.l CD_init, p_sines
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
pixcon:   dc.l padright, 0         ; DATA XREF: symadj+30↑o
                                        ; symadj+4E↑r ...
                dc.l $1600, CD_init, $FFFF00, 0
                dc.l YADD1
piycon:   dc.l padright, 0         ; DATA XREF: symadj+42↑o
                                        ; symadj+62↑r ...
                dc.l $1600, CD_init, $FFFF00, 0
                dc.l YADD1
adsra:   dc.l $A000100, $C0000500, $40B00, $100C000, $5000004, $C000200
                                        ; DATA XREF: sub_19279E+48↑o
                                        ; ROM:_ded2↑o
                dc.l $C0000500, YADD1
py:   dc.l 0                  ; DATA XREF: symadj+70↑w
px:   dc.l 0                  ; DATA XREF: symadj+5C↑w
delayf:    dc.w 0                  ; DATA XREF: sub_192B2A↑w
                                        ; sub_192088+D76↑o ...
delayp:    dc.w 0                  ; DATA XREF: sub_192B2A+8↑w
                                        ; sub_19510A↑r ...
dline:   dc.l 0                  ; DATA XREF: sub_192B2A+42↑w
                                        ; sub_19510A+6↑r ...
delayt:    dc.w 0                  ; DATA XREF: sub_192B2A+E↑w
                                        ; ROM:001943B2↑r ...
delayn:    dcb.w 9,0               ; DATA XREF: sub_192B2A+16↑w
                                        ; ROM:001943B8↑r ...
asc_199C14:     dc.b '~g3:5:                                 ',0
                                        ; DATA XREF: ROM:0019439E↑o
                                        ; ROM:0019439E↑o
                dcb.l 2,0
jaglogo:
.incbin "images/jaglogo.cry"
; vlm-grafix.cry
vlmlogo:
.incbin "images/vlmlogo.cry"
ixcon:   dc.l padright            ; DATA XREF: sub_192D0E↑o
                                        ; yakedit:loc_192ED8↑r ...
dword_19B054:   dc.l 0                  ; DATA XREF: reedit:eparam↑w
                                        ; sub_19546E+18↑w
                dc.l UPDA2, $20000, $FFFF00, 0
                dc.l $20000
iycon:   dc.l padright            ; DATA XREF: gkp:loc_192D2C↑o
                                        ; yakedit:loc_192EEC↑r ...
dword_19B070:   dc.l 0                  ; DATA XREF: reedit+4E↑w
                dc.l UPDA2, $20000, $FFFF00, 0
                dc.l $20000
dlset:    dc.b 0, 6               ; DATA XREF: ROM:0019437A↑o
word_19B08A:    dc.w 0                  ; DATA XREF: ROM:loc_194374↑w
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

avbank:   dc.l $10006, $70001000, $7000A, $60001000, $B0018, $50001000
                                        ; DATA XREF: ROM:00193548↑o
                                        ; ROM:00193694↑o ...
                dc.l $190028, $40001000, $29003F, $30001000
                dcb.l 2,0
                dc.l $A, $90B69C, $900038, $F4352AC6, datdb, $901856
                dc.l $9034A4, $904482, $905776, $9061D8, $9075A6, $908806
                dc.l $909A72, $90A6B4, $66049EA9, $C8316528, $8AEDDC28
                dc.l $8742427, $422BCE06, $B3085A2B, $27345582, $D68A4242
                dc.l $DAA7482F, $4EE8292B, $F8084306, $7FAF2498, $42203EC5
                dc.l $FEC904D6, $3EE3CAD8, $C53CA709, $A50AC500, $A383DE52
                dc.l $6F0EFE86, $384B8AFA, $7AE88C4D, $83D50A2E, $810A653A
                dc.l $4D70ACA7, $42662BEF, $78AF63F8, $6F10BAB8, $30BF0228
                dc.l $E248DFA, $3C26F88E, $CB5675C2, $3655642A, $A66E2AC0
                dc.l $292B0E24, $F2DC30EA, $30D0EEF0, $52C46B0, $F654CFFC
                dc.l $758704CE, $CD831070, $949A2E22, $D2AB4B11, $4E2E6D47
                dc.l $A5A92B, $14CEE112, $695912A4, $E912E845, $13273255
                dc.l $32640A3, $18307141, $144D8314, $891C6AE2, $4118E723
                dc.l $15623315, $A4822504, $102A62BC, $9945602D, $27B838C5
                dc.l $4A434FBC, $EA0A8301, $4988D6A2, $626FC752, $FA4254F2
                dc.l $621E3CC5, $DC2E874A, $A563EE6B, $63251C8, $1888C222
                dc.l $6C26E0D8, $27627450, $E03A2316, $C9630E13, $C4F07A86
                dc.l $44B3073A, $C00E3F1F, $22150845, $60B0E26C, $9EEC243A
                dc.l $456C2E50, $B4258E38, $214A4474, $CA3A5611, $1231E6A6
                dc.l $30E8A671, $222AA1E7, $F50725D0, $46CC8AA3, $E8F8A482
                dc.l $8238E29A, $BDA5A28C, $F1631844, $32ED46CA, $2CC6EEAF
                dc.l $A7C8AEE2, $2E4AA8C8, $810D2725, $12F1232D, $D718CCBE
                dc.l $55EA5CAB, $872274CE, $1E510BD2, $D0124F27, $4B7EADAD
                dc.l $B731D25E, $A7126294, $A4CCDD52, $6C8E8777, $2724FAE2
                dc.l $A7DF27C4, $AB28127D, $2872F845, $928EE992, $92493296
                dc.l $DF6A884A, $1140080, $1010048, $380101, $120020, $FFC0378
                dc.l $25E00EB, $FF5D00, $650040, $B016065, $210093, $20704
                dc.l $1010100, $1A800000, $2800000, $DFFF19EF, $2802B9
                dc.l $DFE000, $3200FA, $B10D8E, $DC0043, $D3500EE, $300DD7
                dc.l $B800C0, $D1F00A1, $EFB00AC, $E00DDB, $1700A3, $D110017
                dc.l $CD0010, $6F90011B, $F300F0, $1700FF00, $FFFFFF00
                dc.l $E0FF00FF, $3010078, $C01700, $1140080, $16BF0070
                dc.l $AE300D4, $8001DF, $1E0080, $D09000C, $5FF00B4, $102E5A
                dc.l $EA0E06, $4D20FF, $30010006, $800308, $308020D, $E50080
                dc.l $FF00F6, $A20080, $3020465, $210093, $20703, $3092510
                dc.l padright, $1200080, $4404, $40E04, $40023, $805010
                dc.l $40013, $2427FF, $25BE0056, $2BE0008, $AAF52900, $C2007D
                dc.l $750DFC, $95004F, $D010041, $100201, $800806, $88009B
                dc.l $C00DB6, $250E9F, $600060, $D8A00BE, $2F0DAC, $F00081
                dc.l $506E0D, $DE0273, $18000114, $802101, d_x, $1010005
                dc.l $8E0080, $CFF00ED, $AC05FF, $A2008C, $3E070090, $20FF3150
                dc.l $30000EE, $1FF00FF, $C00205, $3F00E0, $10B00EA, $C00302
                dc.l $10100A3, $FF0065, $210093, $20703, $3092510, padright
                dc.l $1200080, $4510, $10800000, $50010004, $200080, $8010004
                dc.l $32019FF, $24010138, $2F30090, $56070090, $51FF00FD
                dc.l $3010028, $7240208, $EB1800, $C0073, $800D5C, $C20080
                dc.l $D010041, $700C06, $BC00A0, $EFB0080, $EDC0010, $EBF0092
                dc.l $800DEE, $CA0038, $6DFF00FF, $4631800, $1140080, $53000300
                dc.l $1667008C, $A490044, $22C00F0, $20FF3148, $D00A05
                dc.l $9A020F, $FF00E0, $3020101, $A300FF, $650021, $930002
                dc.l $7030309, $71400B01, $56100004, $540037, $27FF7D17
                dc.l $8055FF, $FC2900, $4C0010, $980D62, $960048, $E3A0080
                dc.l $2010060, $8060035, $520E47, $180E3E, $350E36, $270048
                dc.l $D430080, $390080, $6E1200EF, $3F81700, $1140080
                dc.l $22510096, $28200A4, $5F0018FF, $34FF00FD, $3D0080
                dc.l $1040005, $1FF00FD, $730060, $FF00F6, $890403, $10400E3
                dc.l $FF0065, $210093, $2070E, $1090100, $FFFFFF00, $200037
                dc.l $9A0D29, $9100AE, $D05009E, $E00D16, $7B0080, $D1D00FA
                dc.l $E1A003A, $C00D16, $D5006E, $D1C0076, $2D00A0, $6E020036
                dc.l $1B00090F, $E0284, $FF00FF, $18400FF, $FFDE91, $B00
                dc.l $FFFFFF00, $CF00AC, $A20D0D, $BA0046, $D240078, $600D91
                dc.l $E10080, $DC20082, $EAA0031, $C00D94, $290006, $DB800AD
                dc.l $8E0020, $6F001B00, $FF00FFFF, $FF00FF00, LFU_A
                dc.l $16C00000, $AC00000, $1C0, 0
                dc.l $D000000, $5000000, $2E00, $E14, $2003, $30000008
                dc.l $300, $3000208, allpad, $08, allpad, $3000400, 0
                dc.l $700, $3002500, $FF00FF, $10000FF, $FF4400, $E00
                dc.l $FF, $FF5000, $FF, $FF27FF, $25000000, bbutton, $AA002900
                dc.l d_z, $D00, 0
                dc.l $D000000, optionbu, $300800, 0
                dc.l $D00, $E00, 0
                dc.l $D000000, $D00, 0
                dc.l $6CFF, $FF0000, $2010078, $C01700, $240000C0, $600100
                dc.l $BF00CA, $4556, $FC0E10, $782003, $3001003D, $8001FF
                dc.l $FF00C4, $2F500C6, $80010A, $A200C0, $1090076, $200A91
                dc.l $B09, $25000300, $AC5400B8, $8040308, $419FF, $240000BF
                dc.l $B802BD, $F0560A, $205E09, $80209, $81800, $A90025
                dc.l $710DDB, $A00083, $1D7600C8, $C00D9E, $610E8A, $9400E0
                dc.l $D7800A3, $E30D96, $5F00F9, $108B00, $25BE00F8, $2B80038
                dc.l $20010119, $5F00FF, $6800380, $16000000, $A400000
                dc.l $2140000, $7001601, $100F0033, $21080000, $2030096
                dc.l $800100, $200080, $FF00FF, $E701FF, $FF0026, $800303
                dc.l $1030017, $3910000, $70E0109, $1007100, $B005600
                dc.l $FF, $FF27FF, $7D000000, $55000000, $29000052, $14007C
                dc.l $D6A0068, $B40D0E, $630040, $2000030, $8000039, $8D0E4C
                dc.l $BC0E43, $240080, $D3A0073, $340D48, $DB003F, $C06CFF
                dc.l $FF0000, $263, $1700, LFU_A, $22C00000, $2C00000
                dc.l $5F041803, $34000001, 0
                dc.l $1010000, $1000008, allpad, $08, CLUT, LFU_A
                dcb.l 2,0
                dc.l $700, $1000100, $FFFFFF00, d_z, $D00, 0
                dc.l $D000000, $D00, 0
                dc.l $D000000, $E000000, $D00, 0
                dc.l $D000000, 0
                dc.l $6CFF00FF, $FA00DE, $1010078, $C01700, $90A0040, $2850000
                dc.l $185, 0
                dc.l $DE820018, $B00FFFF, $FF00004C, $5A002C, $DAF00DA
                dc.l $C40D3A, $640040, $DE90091, $D37, $6C0E10, $7E0080
                dc.l $DED0037, $440D27, $AE00FA, $C06E01, $7E1B00, $FF00FFFF
                dc.l $FF00FF00, $1140080, $227C00E7, $8001D9, $4B0080
                dc.l $157F00FF, $2E2A005C, $E020034, $20FF3502, $130080
                dc.l $102003C, $1FF00FA, $B901FF, $F00066, $4020101, $9900FF
                dc.l $650021, $820018, $7030309, $251000B3, $E40001, $40080
                dc.l $4520, $E040004, $200064, $7BFF25BE, $C802B5, $20D400
                dc.l $9F00F9, $370DCF, $B200F5, $D030042, $400202, $100806
                dc.l $700055, $400D95, $C70E83, $E0020, $D720016, $950D8E
                dc.l $350041, $706C00, $1010D, $11B0063, $E81700, $25410092
                dc.l $8001B4, $5A4679, $F0A000C, $20FF3000, $4900A0, $1000001
                dc.l $310080, 2, $A20000, $1090021, $1FF00F4, $8400A0
                dc.l $A820018, $B092540, $38058FF, $FF5220, $1427FF, $256E0040
                dc.l $2940008, $56000000, $52E90B06, $F401FF, $F300F9
                dc.l $18000025, $D200FB, $D7D00C1, $11DCE, $780040, $D13004B
                dc.l $EF000E1, $A00DD1, $B20021, $D050061, $6500B0, $8B0025C0
                dc.l $2C0, cbutton, $1080000, $2902, $3E3712, $4A24FF, $F800ED
                dc.l $10C, $450000, $F, $FF00E0, $F, $FF00E0, $5020069
                dc.l $3820018, $B007920, padright, $83FFFF00, $5A0071, $D40D75
                dc.l $49003C, $D0F00DB, $C00D3F, $6F0E54, $940E4A, $10E40
                dc.l $6C00BC, $D50004D, $C50040, $6C000002, $5600F5, $2731800
                dc.l $FF00FFFF, $FF00FF00, $90D0010, $E7900000, $B00FFFF
                dc.l $FF00002F, $81004E, $DD7003C, $8A0D62, $5400A0, $D890052
                dc.l $800D0C, $6E0ECA, $E00040, $D8F0077, $CA0DF1, $ED0030
                dc.l $E06FB7, $1B000114, $80227F, $E002BF, $A05F01, $F717FF
                dc.l $35080097, $2040037, $800103, $190080, $1030065, $4030103
                dc.l $C500FF, $650021, $90080E, $1090100, $FFFFFF00, $E4001F
                dc.l $9F0D28, $5100AD, $D280010, $D00DA0, $430040, $DD500AF
                dc.l $EBA00F9, $200DA2, $C4004D, $DCA00E2, $7700F0, $6C000002
                dc.l $58005B, $11B0073, $E81700, $258F00B5, $1D3, $8A0F50
                dc.l $5FF009A, $B10001, $7000A0A, $870200, $166B000C, $66B000C
                dc.l $32A0200, $21E01, $100C0077, $6210058, $21816C1, $300007D
                dc.l $1FF00FF, $6E0100, $200A1, $1000002, $C90080, $5000001
                dc.l $290, $B09, $268600CC, i, $DF0064, $15400080, $3520
                dc.l $2010004, padright, $2303501, $41C80, $1401, $40012
                dc.l $900FFF, $25C00048, $2BA00F0, $15FF0080, $1013940
                dc.l $3C201AC1, $2900002F, $8500E9, $20000C1, $9F70014
                dc.l $BB1D10, $1600C0, $D6A00C9, $E0000EB, $100207, $900378
                dc.l $4C0303, $140057, $1B0D58, $730077, $908B00, $25C00000
                dc.l $1C0, $D00, 0
                dc.l $5000064, $D01300, $3001676, $5C0E0E, $5C2003, $30FF00FF
                dc.l $C00308, $101, $80208, $FB0080, 3, $790080, $5000000
                dc.l $290, $B09, $250000FF, $FF0100, $FF00FF, $4520644C
                dc.l $430800, optionbu, $880, $FFF, $25000000, bbutton, $AAFF0300
                dc.l $600, UPDA1F, 0
                dc.l $180000C7, $4700D3, $D0200D0, $90E4C, $400D8B, $FA0EBA
                dc.l $A30EA3, $4E0E8E, $2A0029, $DB10034, $630030, $6C000002
                dc.l $67004F, $2731800, LFU_A, $48082D00, $F04, $16000103
                dc.l $E080000, $24000001, $201, $208, allpad, $10800F0
                dc.l $300, LFU_A
                dcb.l 2,0
                dc.l $700, $1000100, $790000FF, $FF83FF, $FF000000, padleft
                dc.l $D000000, $D00, 0
                dc.l $D000000
                dcb.l 2,$E000000
                dc.l $D00, 0
                dc.l $D000000, 0
                dc.l $6E0700AC, $2631800, $FF00FFFF, $FF00FF00, $915001A
                dc.l $E7940C00, $FFFFFF00, $B0080, $5B0DA8, $6B0021, $D5C0000
                dc.l $100D70, d_z, $DEA00AB, $EAD0055, $A00D75, $C00041
                dc.l $DD100DE, $FB00B0, $6F1E1B00, LFU_A, $22C00000
                dc.l $2C00000, $5F040000, $17033501, $201, 0
                dc.l $10800F0, $108, CLUT, LFU_A
                dcb.l 2,0
                dc.l $8000100, $100FFFF, $FF000000, padleft, $D000000
                dc.l $D00, 0
                dc.l $D000000, $D00, $E00, 0
                dc.l $D000000, $D00, 0
                dc.l $6E6D, $9A1B00, $25C70062, $2EC00FB, $167500AA, $1212
                dc.l $5F1918, $5C0618, $5C0267, $8E0205, $401E00, $1701005C
                dc.l $701934, $200202, $130100, $2003C, $2030003, $80010F
                dc.l $DA0000, $5010024, $3940C09, $80000000, padright, $50010004
                dc.l $1B002C, $160E00C8, $FFF418B, $D80000, $3D050040
                dc.l $51FF00F1, $29000016, $1100E6, $DC80043, $521DF6
                dc.l $C40080, $D490006, $E010098, $600DFA, $9F0092, $D380065
                dc.l $420060, $8B002571, $4002DC, $9B167A, $64130F, $7718FF
                dc.l dattb, $A25005C, $20B00E8, $20FF3000, $1B0040, $1FF00F5
                dc.l $9800A0, $1030093, $800100, $1A0000, $FF00FF, $A50000
                dc.l $A940C09, $25100320, $AB005C, $45400B80, padright
                dc.l $28C00D0, $520D0010, $1010011, $40102, $80020, $EFF00FF
                dc.l $FFF25C0, $3C05340, $30A00C0, $52FB02FF, $F500E1
                dc.l $800103, $B30080, $1F00006B, $970028, $D8B0094, $F80E41
                dc.l $900201, $40094B, $7E0000, $D6400A8, $E580013, $D4C
                dc.l $AB00F8, $D5F0091, $C20080, $6E810013, $1B00FF00
                dc.l $FFFFFF00, $FF00FF00, $FFFFFF00, $FF000110, $613
                dc.l $DCE79A, $80B00, $FFFFFF00, $F8007C, $360D8F, $B000C2
                dc.l $D5800A8, $200D62, $A00080, $DD800D6, $E9D00BB, $400D68
                dc.l $2B0002, $DC000F0, $AF0060, $6E02000C, $1B000110
                dc.l $32FF001A, $4B5C00C0, $68030465, $21009A, $80702
                dc.l $3003508, $C9FFFF00, $3C0036, $EB0D4D, $F900D1, $D0A008B
                dc.l $100DAA, $460080, $2030030, $378004C, $3030038, $3B0E31
                dc.l $3300A0, $D2A00D4, $F10D35, $630084, $B06EC2, $B31B00
                dc.l $1100000, $22BF0046, $2C00018, $CFF, $D700B4, $5000087
                dc.l $BC1380, $280, $163D008C, $63D008C, $2A40068, $348376D
                dc.l $A0196E, $B020023, $6000FF, $F70035, $A00502, $720080
                dc.l $29A0008, $B092500, $3005204, $1E10020, $2700088
                dc.l $523E0004, $9400380, $81E0010, $FFF4000, $7F00FF
                dc.l $3E000000, $52CC0080, $B3401FF, $F50084, $18000008
                dc.l $A00051, $DAF0088, $F31DE9, $6400C0, $D370031, $E7A00BD
                dc.l $900DED, $A0053, $D270076, $F60010, $8B000110, $22C0
                dc.l $2C0, $167D, $DF1380, $280, $1500000D, $DC0A40, $203
                dc.l $C4520C, $200100, $10008, $101, $80000, $FF00F2, $F0080
                dc.l $1FE00A9, $B9A0008, $B092501, padright, $1020080, $4520
                dc.l $B0000FF, $FF02FF, $FF5207, $5C0100, $FF00FF, $10000FF
                dc.l $FF1FFF, $7D000300, $52F9, bbutton, 0
                dc.l LFU_A, $1F00, $340006, $5D0C08, $430053, $870C07
                dc.l $2003E, $500200, $300924, $6900C0, $D30008D, $E2A007B
                dc.l $600D24, $FB0067, $D2E0018, $DF00D0, $6ECA0061, $1B00FF00
                dc.l $FFFFFF00, $FF00FF00, $FFFFFF00, $FF000114, $800609
                dc.l $ECE796, $10B00, $FFFFFF00, $800EB, $2E0DF1, $F4002A
                dc.l $D88008E, $A00D22, $3A0F4E, $E7D0044, $E2A00C3, $6A0DB3
                dc.l $7F008E, $E00D8B, $C61A70, $3C3B70, $3C028B, $C60354
                dc.l $1B000114, $803100, $5300F0, $6C014E96, $10B00, $FFFFFF00
                dc.l $4C00A5, $E30DB0, $3D0039, $D3A0071, $900DD8, $950000
                dc.l $D3700B3, $E1000BC, $EED006D, $590D27, $F20064, $308B00
                dc.l $1140080, $22C800E5, $2B1008D, $ECE00D0, $68200EF
                dc.l $8060120, $7F00FF, $613005F, $2001650, $9E077C, $27200F0
                dc.l $2130000, $20A51662, $781951, $F001FF, $FF006D, $1FF00FF
                dc.l $8C0200, OLP, $1F40087, $C0062A, $296, $10B09
                dc.l $254000CB, $28000FF, $FF2802, $40080, $1801, $41089
                dc.l $D85002, $127003B, $161B0084, $FFF25C1, $E002BD, $604E04
                dc.l $700605, $4052F3, $2800, $C90039, $C90D11, $CC005B
                dc.l $1DA800FE, $E3600A9, $E0600B9, $100DAF, $A200BB, $C08001A
                dc.l $500D5, $900B07, $7F000114, $802696, $460080, $451A006A
                dc.l $E0A00A4, $20A5312B, $9001FF, $FF00F2, $1FF00FD, $520100
                dc.l $C002A, $1FA, $3A0601, $F300FF, $2960001, $B092500
                dc.l $4A100004, $108400B0, $5010013D, $5027FF, $299C0030
                dc.l $560300C0, $7C000044, $750055, $DA50096, $EF1DE4
                dc.l $30F05, $E7A0051, $3010090, $378004C, $30300E7, $9300CF
                dc.l $D2000A7, $BF0050, $8B00FF00, $FFFFFF00, $FF00FF00
                dc.l $FFFFFF00, $FF000110, $60A, $82E78E, $B00, $FFFFFF00
                dc.l $2B007A, $160D1E, $E40062, $D8E00A2, $200D3A, $880EF8
                dc.l $B60E99, $9F0E43, $7200A2, $DD20044, $4D0060, $DFF00FF
                dc.l $1AFF00FF, $3BFF00FF, $2FF00FF, $2010028, $1B000110
                dc.l $3101, $81BC8E, $B00, $FFFFFF00, $6F0034, $CB0DDD
                dc.l $2D0071, $D400085, $100D3F, $E00080, $D58001B, $E2D0017
                dc.l $E06001C, $910D46, $B70022, $B08B00, $1100000, $22C00000
                dc.l $2C00000, $D000012, $30065B, $850808, $1080000, $600
                dc.l $1904, $D60600, $2A0, $FC020B, $B420FF, $E080000
                dc.l $5000010, optionbu, $167E00E0, $1000001, $80000, SRCEN, $80207
                dc.l $460000, $1FB00A6, $500, $38E, $B09, $250000FF, $FF0100
                dc.l $17801200, $FF, $FF1810, $8000200, $1000FF, $FF02FF
                dc.l $FF3400, $1A04, $18E0060, $9000300, $6000000, $FF00FF
                dc.l $FFF2500, optionbu, $1662, $8C3600, $600, $3700, $1AF00C00
                dc.l LFU_A, WID8, $9C0054, $1C0201, $1109CA, $F50094
                dc.l $D010043, $200300, $96D00C5, $D92, $5C0E80, $100080
                dc.l $20000E0, $3750040, $300006F, $7C0014, $C00008A, $F60079
                dc.l $C00B00, $1810066, $1A8B00D6, $3B8B00D6, $2810066
                dc.l $10200D7, $740273, $18000110, $22EF, $FE0080, $1EF00FE
                dc.l $15FF00B7, $171300, $3001601, $560E04, $420FF, $310900A0
                dc.l $1000001, $100100, $10010, $1FF00F2, $1002FE, $B00600
                dc.l 0
                dc.l $28E0000, $B092501, $4A08656D, $F427FF, $29C00000
                dc.l $D4000067, $4003D, $DD20087, $271DFC, $510E50, $6D0EAC
                dc.l $F300E0, $D000043, $70D3F, $6C007D, $D08B00, $1102201
                dc.l $220085, $101001F, $D467F, $FC0A28, $34700E4, $20FF30FF
                dc.l $F50410, $3100202, $A00020, $1110038, $C00302, $10100A7
                dc.l $FF0065, $21008E, $8030309, $25400380, $47406240
                dc.l $4005E, $40802, $40201, $419FF, $25C20088, $2BF0010
                dc.l $A9FF00F5, $B020038, $20D0040, $18000084, $BB00FC
                dc.l $DAC0047, $340D01, $3E0040, $20100D0, $806005D, $2D0E7C
                dc.l $3C0E6C, $B40080, $D5E00A1, $B40C08, $7500F4, $B700C0
                dc.l $B070181, $661A8B, $D63B8B, $D60281, $6602ED, $940273
                dc.l $1800FF00, $FFFFFF00, $FF000114, $800617, $1EE795
                dc.l $C00FFFF, $FF000027, $770023, $D1900AC, $F90D8D, $ED0090
                dc.l $D3700B6, $400DF4, $F30E96, $5400A0, $D400095, $190DCE
                dc.l $B100F8, $302980, $C13B80, $C10711, $1B000114, $80328B
                dc.l $B0BB95, $C00FFFF, $FF00006B, $3100D8, $DD700F6, $80D3F
                dc.l $D00080, $D3300E2, $F00D54, $580E29, $CD0000, $D03003F
                dc.l $80D43, $2400CD, $808B00, $1140080, $22C20028, $2BB0080
                dc.l $15FF00A8, $F0800, $12600CF, $FF0A12, $5F14FF, $B70000
                dc.l $64F0090, $23400C0, $35C1E01, $133058C, $4C02BB, $14030E
                dc.l $43210D, $700300, $300033A, $8002A2, $B950C09, $26800000
                dc.l $2800000, $44000010, $7800300, $4800000, $52820063
                dc.l $27FF25C2, $3402BB, $8C15FF, $94002B, $3E147D00, $980051
                dc.l $290DC5, $BE002B, $1D6A00F2, $C00D8E, $990E7C, $C500E0
                dc.l $D6C009E, $8B0D87, $640024, $908B00, $1140080, $26C00046
                dc.l $1500007D, $DF0803, $1340097, $FF0A80, $15FF0093
                dc.l $300F00, $1E010103, $310D0030, $1B950C09, $70000020
                dc.l $10800000, $52830014, $27FF8104, $7C00, $630001, $4A0DCD
                dc.l $4F00BE, $1DF9007F, $800D4C, $AA0EA7, $130030, $DFD0065
                dc.l $7E0D3B, $DA0028, $A08B00, $1140080, $250000BF, $9E0080
                dc.l $1F060111, $4B00FF, $6000300, $262E00A8, $1E010103
                dc.l $E0E0074, $26000300, $2080009, $800107, $D500A0, $6AB0395
                dc.l $C098280, $5230, $1827FF, $81145F07, $1C0080, $10200E8
                dc.l $18000080, $B90009, $DA7000F, $CB1D5A, $5A00C0, $D780079
                dc.l $E690069, $E00D5B, $C4002B, $D720062, $620090, $8B00FF00
                dc.l $FFFFFF00, $FF000114, $800101, $480038, $1010011
                dc.l $D20FFC, $378026D, $7700FF, $5D000065, $400B01, $60650021
                dc.l $900804, $1010100, $1A800000, $2800000, $DFFF19EF
                dc.l $2802B9, $DFE000, $B700DD, $D0D3B, $A80097, $D4D004B
                dc.l $700D35, $2D00C0, $D9C003D, $E6800B5, $600D3A, $20077
                dc.l $D870067, $AA00D0, $DF100E3, $1AFF00FF, $3BFF00FF
                dc.l $2F100E3, $2020014, $11B00F3, $F01700, $1140080, $22D30020
                dc.l $2880092, $9010015, $3700FF, $1E88007F, $288007F
                dc.l $1566009A, $2050120, $4FF00FF, $2024FF, $14FF00FF
                dc.l $FC01FF, $FF00FB, $602107, $200C0, $1010017, $400303
                dc.l $102002F, $FF0065, $210090, $80C0105, $1002540, $5200A0
                dc.l $18000AB, $5C0940, $4000FC, $391000D0, $7C0140, $6B100320
                dc.l $19FF25C0, $3C00BC4, $FF00FF, $3E1469FF, $FF0023
                dc.l $1FF00FF, $C10080, $170000DC, $330051, $D1E0004, $230D26
                dc.l $AC0030, $D9A00B0, $C00DCE, $410EB4, $7800E0, $D9D001B
                dc.l $830DC3, $D40097, $106E50, $F1B00, $1140080, $21010015
                dc.l $680101, $1F00FD, $16480048, $2DFF0039, $1E0001, $D0021FF
                dc.l $312100C0, $3080308, $C020465, $210090, $8030309
                dc.l $25400380, $47200080, $C40, $40059, $EF7BFF, $25C003C0
                dc.l $46FF0201, $8B0000E0, $570017, $3310905, $C700F5
                dc.l $D030039, $B00202, $100806, $8D0095, $400DBC, $C70EA5
                dc.l $2E0020, $D8F00CB, $950C08, $B3003C, $B10070, $B076259
                dc.l $2C1B00, $1140080, $22B60070, $2BD00C0, $E060060
                dc.l $5FF00FD, $831300, $31300DB, $16240A0F, $F0020C, $A020FF
                dc.l $30FF00F9, $E00204, $680203, $AE01FF, $FF0062, $8000FF
                dc.l $FF00E8, $800302, $4650021, $900803, $3092510, 0
                dc.l $1200000, $E80, $404, $40080, $2C10, $40080, $910
                dc.l padright, $54020004, $5C0044, $27FF25B6, $7002BD, $C00E06
                dc.l $6005FF, $FD0020, $3A4E0080, $55FF00F5, $29000093
                dc.l $5400AC, $DBF0042, $440E42, $A00201, $D00806, $670071
                dc.l $EAC00BA, $200203, $6009C1, $320040, $2030090, $969000E
                dc.l $C40D82, $F300A2, $C00D6C, $981AD7, $A13BD7, $A1026C
                dc.l $980218, $C002A3, $18000114, $802677, $9A2003, $140003F
                dc.l $FF0208, $36C0087, $2420036, $25400FE, $27000A7, $26C0087
                dc.l $2420036, $D1F0020, $B470073, $246004B, $D0400FF
                dc.l $FF0501, $1010001F, $3C650021, $900401, $3010102
                dc.l $1005540, $3800301, $3020340, $38095FF, $56800380
                dcb.l 2,$3800380
                dc.l $940000A0, $9E0082, $DD00089, $E60D1C, $320060, $D7000C9
                dc.l $800D96, $620E83, $9500C0, $D72008C, $A60D8E, $C8006C
                dc.l $200D81, $9D1B37, $3C370281, $9D020C, $4A02A3, $18000114
                dc.l $80361D, $540614, $C21312, $8B0225, $9F0236, $6B0683
                dc.l $DB1900, $38000020, $FF00FF, $E900A0, $20FF00FA, $6A00C0
                dc.l $FF00FB, $FA00C0, $8650021, $90080D, $1060100, $39010702
                dc.l $12010004, $8B100320, $19FF5614, $B788FF, $F80082
                dc.l $1FF00F4, $941800, $6300B7, $A60619, $6F000C, $3050081
                dc.l $580012, $619006F, $C0305, $400BE, $600202, $700806
                dc.l $4500F4, $800DB6, $7500D0, $20400D0, $9B3004E, $900205
                dc.l $200947, $C0052, $D58008F, $460060, $D6D0028, $1A90000C
                dc.l $3B90000C, $26D0028, $3551B00, $1100000, $60E00E4
                dc.l $28400FF, $FF0184, $FF00FF, $DE800C00, $FFFFFF00
                dc.l $C900E0, $310D53, $1400C3, $D500076, $300D41, $D80EAD
                dc.l $210E77, $7C00E0, $D4600E0, $230D97, $710025, $100D18
                dc.l $A71A55, $733B55, $730218, $A70201, $7F1B00, $1100000
                dc.l $22BB004A, $2910060, $800800, $B1003F, $3A51006E
                dc.l $1FF00FE, $DC00C0, $2BA53D05, $620207, $A10B80, $C00FFFF
                dc.l $FF0000EE, $360075, $D350070, $4F0D29, $D600F0, $DA7005B
                dc.l $EDF0025, $EC30040, $600DA9, $F9002F, $DD300DE, $110050
                dc.l $8B000110, $2100, $72007C, padright, $A00015, $8014FF
                dc.l $A100EC, $13000300, $15FE00F0, $7030A5, $31080000
                dc.l $3200320, $1FF00FD, $460080, $FF00F7, $DA0B80, $C09D440
                dc.l $40037, $4027FF, $D5082900, $F40076, $BB0D1D, $340021
                dc.l $1D9A0040, $ECD00AB, $EB300F5, $A00D9C, $A90041, $DC30046
                dc.l $2B00B0, $8B000110, $3EFF, $42E04, $480B80, $24A53000
                dc.l $1400C0, $1B800C09, $FFFFFF00, $A50057, $D00DD6, $AE0070
                dc.l $1D74001C, $EE500BB, $A00DFD, $5E0080, $D7500EC, $700D92
                dc.l $FD001D, $8B00, LFU_A, $26C00000, $20080108, 0
                dc.l $2000380
                dcb.l 5,$280
                dc.l $D00, $B04, $204, $D00, 0
                dc.l $5001008, $3C00, 0
                dc.l $4000300, $1000100, $55000300
                dcb.l 2,$3000300
                dc.l $95FF5600
                dcb.l 2,$3000300
                dc.l $3009400, d_z, $D00, 0
                dc.l $D000000, $D00, 0
                dc.l $D000000, $E000000, $D00, 0
                dc.l $D000000, 0
                dc.l $DF100E3, $1AFF00FF, $3BFF00FF, $2F100E3, $20500AF
                dc.l $2B31800, $1100000, $363300CA, $62900FB, $11FF00FF
                dc.l $E10031, $89F000A2, $1EE, $E60000, $A800C00, $FFFFFF00
                dc.l $7500BA, $CA076E, $F40498, $C4003E, $76E00F4, $1452009F
                dc.l $E0700BF, $100D09, $DF0010, $D5300E9, $FE0D68, $9800C0
                dc.l $A08B00, $A120FF0, $D8020B00, $FFFFFF00, $E00BC, $F40DAC
                dc.l $A0009C, $D5C0091, $C00D72, $470000, $DED00B4, $EAF00FD
                dc.l $800D78, $10001C, $DD400C0, $B70040, $D80007E, $1A000000
                dc.l $3B000000, $280007E, $202003B, $1B0025D9, $F802F9
                dc.l $A0901, $95000F, $39FF009B, $40100, $600C8, $2CFF3CFF
                dc.l $F70063, $FF, $F700E1, $800B02, $B00FFFF, $34010028
                dc.l $1FC800, $330013, $380D8E, $FC0028, $D3500F2, $800DD7
                dc.l $CA0000, $D1F00B8, $EFB00C1, $DDB, $290028, $D11002D
                dc.l $A30080, $8B0025C0, $5B0260, $145FF, $E009E, $30FF4F02
                dc.l $B09FFFF, $FF000041, $6500DE, $D7600BF, $FA1DCA, $AE0080
                dc.l $D0E003E, $EEC0076, $400DCD, $D9003A, $D000095, $BD00E0
                dc.l $8B000100, $23C00000, $2C00000, $E000000, $5000000
                dc.l $1380, $3800000, $15000000, $A400000, $2140000, $20033108
                dcb.l 2,$201
                dc.l UPDA1F, $800F0, 0
                dc.l $800F0, $300, $4000000, DSTA2, $3002500, $FF00FF, $10000FF
                dc.l $FF0EFF, $FF0400, $FF, $FF2C00, $FF, $FF0900, $FF00FF
                dc.l $54000000, $FF00FF, $27FF2500, optionbu, $E00, $500, 0
                dc.l $3A000000, $55000000, $29000000, padleft, $D000000
                dc.l $E00, optionbu, $300800, 0
                dc.l $E000000, $201, $900, 0
                dc.l $20000E0, $9000000, $D00, 0
                dc.l $E1800A7, $1A550073, $3B550073, $21800A7, $2010005
                dc.l $2B31800, $FF00FFFF, $FF00FF00, $38FF00DD, $D00626
                dc.l $8013DD, $4F0220, $3F0A90, $4A79F8, $A02ED, $520C02
                dc.l $B006404, $499FF, $669500EF, $970000BA, $97008D, $DF20050
                dc.l $171D83, $D00C0, $D3E0082, $800D54, $D00070, $D850019
                dc.l $F70DA5, $E80052, $D08B00, $90B0000, $FFCD79C, $160B00
                dc.l $FFFFFF00, $3200F1, $170DDB, $B40095, $D6200EF, $500D8B
                dc.l $BD0040, $D0F00A7, $ECD00B2, $200D91, $EC0035, $DF400FC
                dc.l $7F0070, $DA10018, $1A5300A7, $3B5300A7, $2A10018
                dc.l $20000EA, $1B0025C3, $CF02E1, 0
                dc.l $9380017, $161C00F3, $FF21A6, $6A0201, screen1, $29010103
                dc.l $3DF80043, $1000001, $B30040, $A9C0016, $B00FFFF
                dc.l $FF000057, $47005B, $DBE0010, $210D3C, $500010, $DF10040
                dc.l $400D41, $AB0E19, $7500A0, $DF50005, $410D31, $69006B
                dc.l $B08B00, $25D800CF, $19E, $BC1500, $7F00FF, $802013A
                dc.l $8300FF, $213600EE, $E01009A, $1E010103, $E0E003F
                dc.l $212800A0, $3280328, $E010031, $FF029C, $160B09, $80040137
                dc.l $51040147, $2827FF, $FF000069, $D80061, $DA500D3
                dc.l $F31DE4, $2400C0, $D300031, $E0A002A, $E00DE7, $B50053
                dc.l $D2000D1, $860010, $8B00FF00, $FFFFFF00, $FF00FF00
                dc.l $FFFFFF00, $FF003800, $7001E, $5FF00FB, $B813D9, $290227
                dc.l $C70A8E, $8779F3, $8F0321, $B9C0016, $B00FFFF, $FF0000DE
                dc.l $CB00B0, $D210064, $101D9C, $840000, $DE100E3, $F00D02
                dc.l $CD00D0, $D9E00F6, $100DC6, $24001B, $8B00, $90C00B6
                dc.l $2850000, $185, 0
                dc.l $DE8D001E, $B00FFFF, $FF000068, $C00B4, $D2000C3
                dc.l $DC0D6C, $4500C0, $DB10017, $D41, $740EF9, $450080
                dc.l $DB700DB, $5C0D24, $450033, $400D75, $8D1A77, $803B77
                dc.l $800275, $8D0201, $8D1B00, $25CF008B, $8001C0, $A000000
                dc.l 0
                dc.l $13050131, $370780, $280, $1400, 0
                dc.l $2000004, $1FF00A2, $400100, padleft, $B02008B, $2C000000
                dc.l $100100, $20000, $1909, $D00203, $A00100, $80018
                dc.l $200008A, $506, $7F038D, $1E070E, $1090100, $24010004
                dc.l $3E0020, $10000FF, $FF0900, $FF00FF, $390000FF, $FF0100
                dc.l $3201000, $5500, $30019FF, $29000A00, 0
                dc.l $3E00, $69000000, UPDA1F, 0
                dc.l $1700, $450045, $490D59, $C0008B, $D0301B0, $2020010
                dc.l $8060030, $8A00C0, $D4000B9, $E3800A1, $E00D31, $4C00EB
                dc.l $D3D0073, $A60090, $D7C0069, $1AA300FD, $3BA300FD
                dc.l $27C0069, $20200B3, $2A31800, $25B80029, $2B900A0
                dc.l $D18, $E80643, $400804, $123001B, $7800380, $15FE00C1
                dc.l $E00, $2F10, $E52108, $308, $3080100, $80060, $C00000
                dc.l $80080, $6000000, $28D, $1E0B09, $80400135, $B75000
                dc.l $FF, $FF0940, $38019FF, $D5000B09, $80209, $81800
                dc.l $A5002D, $9E0DEA, $E3003A, $1D09007E, $800D61, $FE0E35
                dc.l $BE0040, $D0D00A4, $7A0D50, $1A0039, $E08B00, $FF00FFFF
                dc.l $FF00817C, $691AA3, $FD3BA3, $FD027C, $690202, $802A3
                dc.l $18000110, $275100EB, $801F06, $1240047, $FF0208
                dc.l $3A500F3, $24D000C, $2440094, $24D00D5, $2A500F3
                dc.l $24D000C, $6980614, $300B65, $EA0246, $4B0D04, $FF00FF
                dc.l $5011010, $1F3C65, $21008D, $1E0301, $3010102, $1002801
                dc.l $42B40, $3800301, $3020340, $38095FF, $29770088, $2B800380
                dcb.l 2,$3800380
                dc.l $E1F0020, $84000050, $CE0029, $DB500A5, $2B0D33, $E80090
                dc.l $20100E0, $80600EC, $B200C0, $D3B0099, $E1B00B4, $700205
                dc.l $D00378, $4C0303, $F00065, $8B0D2B, $A500F4, $900D81
                dc.l $9D1A80, $373B00, $370281, $9D020C, $4A02A3, $180034FF
                dc.l $530300, $500, 0
                dc.l $11000000, padright
                dcb.l 2,$2800000
                dc.l $6800000, $194100B8, $37100000, 2, 0
                dc.l $20000008, $F00100, $800F0, $403068D, $1E0702, $1000100
                dc.l $35200300, $7001200, $E00, $7B00, $30019FF, $56000000
                dc.l $E000000, $78000000, UPDA1F, 0
                dc.l $18000075, $2E0006, $61E0075, $400300, $98000D, $32061E
                dc.l $750040, $3000014, $8F0020, $2000030, $8000052, $3C0080
                dc.l $D6D00A6, $201, $95F, $F10040, $20000E0, $9530085
                dc.l $720D68, $1B006C, $600DBF, $681A8E, $23B8E, $202BF
                dc.l $680220, $6E02A3, $18000114, $80060B, $F0E784, $B00
                dc.l $FFFFFF00, $EB0073, $6C0DCB, $A20084, $D830060, $400D0D
                dc.l $810EBC, $AC0E65, $160E15, $B70004, $D990043, $E00C0
                dc.l $D69006F, $1A6900ED, $23180080, $30080, $CE0080, $131169
                dc.l $ED0269, $6F03BC, $1B000114, $8022A7, $280000, $1C60018
                dc.l $90100AC, $7F00FF, $102003C, $60F0064, $8080108, 0
                dc.l $2505006B, $C00000, 0
                dc.l $61800C4, $6010016, $20118, $401200, $2B090001, $200901
                dc.l $201, $20E, $7700C0, $11000F7, $C00503, $2F0384, $705
                dc.l $1000102, $24000000, $FF00FF, $12800000, $6800000
                dc.l $31100300, $E040004, $432000DF, $EC1110, $32019FF
                dc.l $25001302, $10060C, $C031FF, $FF00FC, $13960003, $41020010
                dc.l $12090008, $2090008, $1800008E, $2A0054, $DB8008A
                dc.l $BC0D0E, $2C0000, $20100E0, $96300CF, $D85, $140876
                dc.l $440301, $6900FF, $800203, $376, $440301, $65005E
                dc.l $3C0D7E, $5A004D, $400D75, $8D1A77, $803B77, $800275
                dc.l $8D0203, $6A02B3, $18000114, $8022AF, $120080, $1AF0071
                dc.l $FF005FF, $B40024, $13000300, $16E800A0, $2E0001C3
                dc.l $319C00C0, $A0B0005, $E00102, $4400C0, $A840000, $B09D680
                dc.l $27FF, $D5080B0C, $6F0203, $A61800, $3700FB, $560D95
                dc.l $C100E2, $1D6500E8, $EDD0036, $EA1008F, $E6B0080
                dc.l $220DC5, $180015, $608B00, $1140080, $22C6003D, $8001C1
                dc.l $51638, $C11355, $DF1945, $9C0E0D, $A0521C, $B00A06
                dc.l $B700C0, $FF00FC, $8B00E0, $3020465, $210084, $8030309
                dc.l $25010302, $D8009C, $E800000, $4010004, $DB00F8, $2D201080
                dc.l $5001, $40011, $580908, $3100080, $17FF, $25BE0050
                dc.l $29A0098, $160D0060, $3E0D00A8, $51FF00E6, $C5001FF
                dc.l $FB00B4, $180000F2, $30087, $20200B1, $9180014, $850203
                dc.l $130A44, $F00201, $A00806, $4E0059, $400D68, $770E5B
                dc.l $680020, $D4F0092, $A50D63, $2F007C, $700D95, $301A71
                dc.l $F83B71, $F80295, $300220, $702B3, $18000100, $27C00000
                dc.l $1F08, $1080000, optionbu, $3800000
                dcb.l 5,$2800000
                dc.l $6000600, $B04, $204, $D00, 0
                dc.l $5001008, $3C00, 0
                dc.l $300, $3000100, $1002800, $2B00
                dcb.l 2,$3000300
                dc.l $30095FF, $29000000, $2B000300
                dcb.l 2,$3000300
                dc.l $E000000, $84000000, padleft, $D000000, $D00, 0
                dc.l $2000030, DCOMPEN, 0
                dc.l $D000000, $E000000, optionbu, $E00325, $380300, 0
                dc.l $D00, 0
                dc.l $D75, $8D1A77, $803B77, $800275, $8D0201, $7702B3
                dc.l $18000114, $8031FE, $8BC84, $B00, $FFFFFF00, $F80094
                dc.l $BE0D42, $EB00DA, $D2B00A9, $A00DAE, $A60EE8, $DE0ECB
                dc.l $C20EB1, $61001A, $DDD0019, $4700E0, $8B000A4E, $E792001E
                dc.l $B00FFFF, $FF00004C, $5F007F, $D4900AB, $4D0D94, $6A00D0
                dc.l $D5100AB, $400D17, $8F0EB4, $9D0020, $D5A00F1, $ED0DEF
                dc.l $8E0035, $F00DD3, $261ACF, $563BCF, $5602D3, $260202
                dc.l $571B00, $340000E9, $9F0204, $700614, $9E3206, $EB0A0C
                dc.l $180713, $550110, $7C14FF, $28FF00FF, $8C120F, $F70206
                dc.l $280000, $502001F, $392001E, $B023808, $40602, $4BDFF
                dc.l $3A604F93, $567300, $EF0016, $670D36, $930085, $1DA700F9
                dc.l $400DDF, $F70E7A, $A80EAA, $990025, $DD400A5, $740070
                dc.l $8B0025F5, $6202E8, $90080, $CFF00FA, $800500, $4F0020
                dc.l $2DFF0040, $9C0E05, $5420FF, $30FF00E9, $340, $3400205
                dc.l $8F00A0, $1010047, $200501, $6300FF, $292001E, $B0926DB
                dc.l $2F02AC, $4F46FF, $FF0C00, dstoffx, $DF5040, $40067, $F027FF
                dc.l $D4FF00E9, $B00005D, $8000FF, $FC0052, $180000A4
                dc.l $4300C9, $D1300CA, $AB1DAA, $1200C0, $D380019, $EF10015
                dc.l $E00DB0, $BB000B, $D1B0063, $3C0090, $8B00257A, $190000
                dc.l $1A10044, $8014FF, $AE00F0, $11200, optionbu, $15FF00CD
                dc.l $400E07, $7020FF, $31F10030, $3080308, $1FF00F9, $480000
                dc.l $1F30035, $601005D, $FF0292, $1E0B09, $D6490000, $27FF40FF
                dc.l padright, $191F8, $AFF00F9, $F002F0, $E1800, $E600A0
                dc.l $7A0D2F, $7C005E, $1D920083, $800DC3, $5A0EAA, $EE00C0
                dc.l $D9400CD, $8E0DB9, $7A00A3, $A08B00, $FF00FFFF, $FF00FF00
                dc.l $340100EC, $70BB92, $1E0B00, $FFFFFF00, $590080, $D10DC0
                dc.l $F400A3, $D3C00B4, $300DF2, $D000C0, $D4300C1, $E1B0048
                dc.l $E00DF6, $9C0003, $D330064, $6F0010, $8B00090F, $32E787
                dc.l $180B00, $FFFFFF00, $A500C0, $4F0DBD, $E400BD, $DA40021
                dc.l $E900087, $E6B005F, $EFD00F3, $E9A00C9, $5D0D3F, $2100E2
                dc.l $E000000, $1A3400BC, $3B3400BC
                dcb.l 2,bbutton
                dc.l $1B00367F, $206001B, $615000F, $8050129, $6700FF
                dc.l $25002B01, $10314FF, $FF00EA, $A00100, $E0D00, $60053
                dc.l $601109, $30D00E7, $C00A87, $180B02, $FFFF3904, $24C400
                dc.l $480077, $370DAA, $CC00F5, $1DE600D5, $E3300C7, $E760018
                dc.l $EEA0070, $950D24, $390021, $8C000100, $22C0, 0
                dc.l $1C00000, $C00, 0
                dc.l $6000000, $8080108, 0
                dc.l $6800380, $15000000, 0
                dc.l $D140000, $20030E08, cbutton, $80400, $3000208, allpad
                dc.l $10800F0, $300, LFU_A
                dcb.l 2,0
                dc.l $700, $3002500, $FF00FF, $10000FF, $FF4500, $10FF00FF
                dc.l $50000000, $FF00FF, $9000300, $19FF2500, $3004600
                dc.l $2006000, $C00
                dcb.l 2,0
                dc.l $18000000, padleft, $3110900, 0
                dc.l $D000000, optionbu, $300800, 0
                dc.l $D00, $E00, 0
                dc.l $D000000, $C00
                dcb.l 2,0
                dc.l $B000100, $1B3400BC, $23180080, $30080, $CE0080, $131134
                dc.l $BC0600, $53011B, $C300F0, $17000100, $22C0, $2C0
                dc.l 0
                dc.l $14000000, 0
                dc.l $12800380, $15000000, $E14, $2003, $31080000, $3000300
                dc.l $1000008, $F00100, $800F0, $300, LFU_A
                dcb.l 2,0
                dc.l $700, $3002500, $30000FF, $FF0EFF, $FF0400, $FF, $FF2D00
                dc.l $10FF00FF, $50000000, $FF00FF, $9000300, $FF00FF
                dc.l $17FF2500, optionbu, $1500, UPDA1F, $3D000000, $51000000
                dc.l $A000000, UPDA1F, 0
                dc.l $18000000, padleft, $2010011, $9000000, $201, $630A00
                dc.l optionbu, $300800, 0
                dc.l $D00, $E00, 0
                dc.l $D000000, $D00, 0
                dc.l $D00, $1A34, $BC2318, $800003, $8000CE, $800013, $113400BC
                dc.l bbutton, $2000053, $11B00C3, $F01700, $FF00FFFF
                dc.l $FF00FF00, $34FE00A5, $BB87, $180B00, $FFFFFF00, $B200E1
                dc.l $A10D35, $2E0013, $D4C006B, $E3100AC, $E970091, $E64009E
                dc.l $E360073, $730D82, $F8001C, $8C000110, $60E, $D2E794
                dc.l $20B00, $FFFFFF00, $6300CE, $80D68, $230098, $D980089
                dc.l $800D62, $260000, $D2D0088, $EC700D7, $D6B, $AE0098
                dc.l $D04006A, $F00080, $D830055, $1A5F0052, $231F0000
                dc.l $1001C0, $F, $80105F, $520283, $550202, $D01B00, LFU_A
                dc.l $22C00000, $2C00000, $A000000, UPDA1F, $600, $808, $1080000
                dc.l $2604, $914, $600, YADD1, LFU_A, $12001600, vfb_ysca
                dc.l $102, $E02, $150055, $110800F0, $108, allpad, $3000100
                dcb.l 3,0
                dc.l $7000300, $38000000, $FF00FF, $4000000, $FF00FF, $31001200
                dc.l $FF, $FF4100, $FF00FF, $11000300, $19FF3900, $600
                dc.l $3100, 0
                dc.l $13000000, $41000000, $12000000, bbutton, $18000000
                dc.l padleft, $D000000, $D00, $300, $300800, 0
                dc.l $D00, $825, $380300, 0
                dc.l optionbu, $E00325, $380300, 0
                dc.l $D00, 0
                dc.l $D00, $1A83, $4A3B83, $4A0200, optionbu, $6C1B00, $11023C2
                dc.l $7D0080, $1A7007D, $800CFF, $F70084, $67500E0, $8040123
                dc.l $1B00FF, $6000300, $15FF0010, $9C0001, $951004E, $2001F01
                dc.l $101000E5, $26100310, $2090049, $200107, $90020, $3020101
                dc.l $8700FF, $650021, $940002, $7030309, $25400380, $46020004
                dc.l $A040004, $2400004, $140033, $5D400380, $19FF25C0
                dc.l $3C046FF, $2010940, $63090014, $2090014, $180000BE
                dc.l $710072, $3310932, $4200F6, $D030039, $B00202, $100806
                dc.l $BA008D, $800D4E, $120E04, $4F00C0, $DC10077, $B60C08
                dc.l $30003F, $F70020, $B070180, $1B800000, $230B0000
                dc.l star, $C00000, $51100, $659, $2C0100, 0
                dc.l $17000110, $2201000B, $D0080, $1CC009D, $800CFF, $BC002C
                dc.l $64F0027, $2E1000BE, $A510072, $20500EC, $20FF3191
                dc.l $E0039D, $600299, $600100, $C01FF, $FF003B, $E00302
                dc.l $1020021, $FF0065, $210094, $20703, $3092401, $40204
                dc.l $41080, $401, $400DB, $F82C02, $40A04, $40480, $5001
                dc.l $40038, $3C0008, $40210, $40308, $3100080, $17FF
                dc.l $25C100B0, $2BF0088, $16250020, $3A40030D, $A851FF
                dc.l $E60301, $100201, $100350, $1FF00FB, $B41800, $2100C3
                dc.l $A30202, $B10973, $800D9, $2030013, $A4400F0, $20100A0
                dc.l $80600A2, $FE0040, $DD90053, $EBE0028, $A00DA5, $8A0039
                dc.l $DCE0057, $5E0030, $D950030, $1A7100F8, $230B0000
                dc.l star, $C00000, $51171, $F80295, $300220, $70100
                dc.l $B30000, $17000110, $233E00A4, $800179, $30080, $44FF00DE
                dc.l $400A48, $D624FF, $31350010, $BF100C0, $10D00DC, $400302
                dc.l $1010025, $FF0065, $210094, $20703, $3092501, padright
                dc.l $1020080, $4520, $A080004, $56100004, $EB0020, $9400380
                dc.l $19FF255E, $E00285, $B05240, $56FF00F6, $B090008
                dc.l $2090008, $18000002, $9E007A, $2090A48, $8A00EE, $20600C3
                dc.l $901003F, $3020010, $806004C, $3F0080, $D6500AA, $E5800F4
                dc.l $C00775, $400402, $FF004E, $20800A3, $90A0064, $E800A0
                dc.l $10B00A3, $A21002C, $1A763C76, $321002C, $203000D
                dc.l $3F81700, LFU_A, $31000000, $4B140000, $68000400
                dc.l 0
                dc.l $700, $3003500, $C9FFFF00, d_z, $725, $380400, 0
                dc.l $7250038, $4000000, $D00, 0
                dc.l $D000000, $E000000, $D00, 0
                dc.l $D000000, 0
                dc.l $D75008D, $1A770080, $3B770080, $275008D, $20C0058
                dc.l $2B31800, $1100201, $3100D0, $101000A, $4164F, $FB00C4
                dc.l $5D000062, $C00B01, $60650021, $910804, $1010100
                dc.l $FFFFFF00, $E100D4, $40D58, $E800CC, $DDB00B4, $C00D6E
                dc.l $D30E93, $C40E01, $4B0080, $D7C008E, $4C0D58, $8C0048
                dc.l $406E03, $1011B, $F300F0, $17000110, $32FF0095, $381400
                dc.l $2D7000BC, $67100AC, $1C1D01, $1014703, $1020087
                dc.l $FC0065, $210091, $8020300, $35014320, $85FFFF00
                dc.l $EF0065, $4F0778, $4C0303, $D000C3, $BD0D84, $1100D0
                dc.l $D100047, $400DC0, $5F0E68, $530020, $D180088, $5D0D9C
                dc.l $C60032, $F00D07, $EC5B07, $EC0241, $730101, $760040
                dc.l $31B1300, $FF00FFFF, $FF00E203, $1011B, $F300F0, $1700FF00
                dc.l $FFFFFF00, $810700EC, $5B0700EC, $35A0101, $780098
                dc.l $17000110, $23B900D6, $4901B1, $1E0046, $1F0C0114
                dc.l $300FF, $66300E7, $261004D, $25E00E7, $27F00BF, $25B009F
                dc.l $261004D, $57C005C, $137200CF, $2730075, $D05002F
                dc.l $FD0501, $1FC0E1A, $9900FF, $FF00FF, $FF3865, $210091
                dc.l $4080301, $1020100, $55400380, $3010302, $3400380
                dc.l $7088DFF, $56880048, $2800380, $3800380, $3809400
                dc.l $7C00E9, $450DEE, $FF00BF, $D4200ED, $F00DEA, $FC0060
                dc.l $2020030, $378004C, $3030064, $F50E38, $560060, $D0F00E6
                dc.l $9F0D52, $EA00DE, $506CFF, $FF01F9, $1010078, $981700
                dc.l $110254C, $16C0054, $2A211B, $CF00F4, $20700FF, $24800C8
                dc.l $25F0082, $2A90022, $29D002A, $24800C8, $25F0082
                dc.l $58100FF, $E504FF, $CE0C61, $100258, $640D04, $AB00FE
                dc.l $501100D, $863C65, $210091, $4020301, $1020100, $55400380
                dc.l $3010302, $3400380, $F0885FF, $56800380
                dcb.l 2,$3800380
                dc.l $9400008D, $6200E8, $D04006C, $380D45, $D30080, $D17004E
                dc.l $878004C, $3030074, $680E45, $DB0E1B, $AB0038, $D610095
                dc.l $FE0080, $6CFF00FF, $1F90101, $780098, $17000114
                dc.l $800245, $80020F, $380100, $7E0073, $FF019E, $A300FF
                dc.l $FF0220, $3600360, $1720003, $FF5E65, $400B00, $62920C00
                dc.l $D4000AC, $880180, padright, $C010004, $AC0088, $DBFF0C01
                dc.l $2600EF, $FF0001, $2D00FF, $FF0D4D, $8700FF, $DB00003D
                dc.l $F400AD, $D50003D, $770D06, $3F00E0, $2010070, $806002B
                dc.l $6500C0, $D3900DD, $E3200A1, $600D6C, $C50017, $20300E3
                dc.l $95C0033, $5400D0, $10200C3, $3790050, $30401D6, $8B1AC0
                dc.l $CF3BC0, $CF02D6, $8B0200, UPDA1F, 0
                dc.l $17000114, $803100, 0
                dc.l $14082D00, $614, 0
                dc.l $21F00F8, $2201600, $10314FF, $FE00DD, $C700FF, $FD009C
                dc.l $2318FF, $F6009C, $300FF, $FE0054, $E20104, $9A0080
                dc.l $10200C8, $C00300, LFU_A, $292, $494030A, $1040102
                dc.l $35004300, $3F1000FD, $40120, $1B400380, $3010302
                dc.l $19FFB8FF, $FF0093, $A700FF, $FF0094, $2318FF, $FE007C
                dc.l $300FF, $FE009E, $200FF, $FF0032, $1FF00FE, $BC1800
                dc.l $4B007E, $7F0775, $400308, $6100D8, $4D0C07, $D003A
                dc.l $E3400EB, $E46008F, $E3D00BD, $E3500BE, $ED0D42, $FE0025
                dc.l $E390010, $57A10339, $1000FF, $FF0000, $DE0278, $C0030F
                dc.l $1300FF00, $FFFFFF00, $E0FF00FF, $DE, $1010078, $C01700
                dc.l $FF00FFFF, $FF00FF00, $10023C0, 0
                dc.l $1C00000, $1F08, $1080000, $680
                dcb.l 5,$280
                dc.l $500, $1304, $204, $D00, 0
                dc.l $5000103, $E080000
                dcb.l 2,0
                dc.l $38000000, UPDA2, $3000100, $1005500
                dcb.l 2,$3000300
                dc.l $3000700, $8DFF5600, optionbu
                dcb.l 2,$3000300
                dc.l $94000000, padleft, $D000000, $D00, 0
                dc.l $D000000, optionbu, $C003EA, $80300, 0
                dc.l $E000000, $D00, 0
                dc.l $D000000, 0
                dc.l $6FDE03C0, $17000100, $250001C0, 0
                dc.l $21080000, optionbu
                dcb.l 6,$280
                dc.l $500, 0
                dc.l $4000000, $C040000, $2040000, $D000000, $500, $10080000
                dc.l $3C000000, UPDA2, $3000100, $1005500
                dcb.l 2,$3000300
                dc.l $3000F00, $85FF5600
                dcb.l 2,$3000300
                dc.l $3009400, d_z, $D00, 0
                dc.l $D000000, $D00, $8EA, $80300, 0
                dcb.l 2,$E000000
                dc.l $D00, 0
                dc.l $6FDE, $3C01700, $1100000, $624008C, $101007F, $1F029F
                dc.l $AF0100, $76007A, $1BF, $B2005F, $1BF0070, $10184
                dc.l $C7CF93, $C000D00, $FF00FF, $10000FF, $FF0180, $A9004C
                dc.l $AFF00FF, $DBFF0C00
                dcb.l 3,0
                dc.l $1CB, $200A5E, $ABDC00, $4E00DC, $270D66, $3800C5
                dc.l $D070039, $500202, $900937, $490040, $D4900B7, $E400080
                dc.l $200D38, $260065, $20000C3, $94500FD, $500070, $10000F3
                dc.l $3750040, $3080100, $1AFF, $FF3BFF, $FF0200, $1F00
                dc.l $1100000, $22C30078, $2B80015, $805A08, $5C020F, $D02C00
                dc.l asym_fla, $3001FE, $450000, $18000006, $DE00DF, SRCEN, $3000C0
                dc.l $1110013, $C002C2, $E00A93, $4010704, $2510001C, $80120
                dc.l $190058, $8EFF00FF, $43FF25C0, $9002C0, $D88D00, $12
                dc.l padright, $1D, WID8, $25, $FF01FF, LFU_A, $90014
                dc.l $1000009, $141800, $4C001C, $530C07, $6200A5, $890C00
                dc.l $1560090, $D35005A, $F740030, $2080050, $9D300B1
                dc.l $20B, $A370029, $490204, $F30925, $77009B, $300104
                dc.l $A38800, $FF00FFFF, $FF00FF00, $FF00FFFF, $FF00FF00
                dc.l $FF00FFFF, $FF00FF00, $FF00FFFF, $FF00FF00, $1140080
                dc.l $1000054, UPDA1F, $5C00AC, $28B00FF, $2A200FF, $1FF0000
                dc.l SRCEN, $12A00F0, $15012E, $F2000E, $1A400BD, $E0092B
                dc.l $380015, $12F0040, $176502, $470D0020, $D010C, $37007A
                dc.l $A9C0C00, $15000015, $BCC910, $870044, $12019FF, $14FF0000
                dc.l $20, $97C002F, $E0BD09, $700ED, $1090007, $BA1700
                dc.l $AC002C, $B80DDF, $9000A8, $D08001F, $E003D0, $97800EA
                dc.l $DA1, $380E8D, $110000, $D7A00CD, $A80D99, $12003B
                dc.l $800B00, $1910071, $1A9C0067, $3B9C0067, $2910071
                dc.l $1F000114, $80222E, $F8022F, $B80000, $5A1800A3, $2240044
                dc.l $2CFF00FF, $1D00FF, $230003C, $18FF00FD, $E700FF
                dc.l $FF00F8, $D80080, $10A0036, $E0010A, $E60040, $A9C0C04
                dc.l $250000FF, $FF0100, $FF00FF, $D3FF2500, optionbu, $907F
                dc.l $21C00FC, $1A2D03A4, $3200320, $180000B2, $9B0040
                dc.l $C0800E7, $ED00C0, $C070061, $B60000, $2060020, $9B00086
                dc.l $300205, $B009C1, $940060, $2070010, $98D00B6, $300203
                dc.l $957, $F300E0, $2020A9C, $D500A4, $102, $F38800, $FF00FFFF
                dc.l $FF00FF00, $FF00FFFF, $FF00FF00, $FF00FFFF, $FF00FF00
                dc.l $FF00FFFF, $FF00FF00, $59F0391, $B002BD, $DF023B
                dc.l $BF065D, $D80000, $15F007C, $175, $6D00FF, $95E00F2
                dc.l $160, $1E0000, $4D1A00A0, $781501, $470800F0, $108
                dc.l allpad, $A850C00, $E800000, $1200080, $2FF, $FFC900
                dc.l $FF00FF, $10019FF, $C01005F, $7F00FF, $1005F, $3F00FF
                dc.l optionbu, $95600F3, $FFBD00, 0
                dc.l LFU_A, $1700, $82001A, $40DA8, $DA00CC, $D06003C
                dc.l $B00200, $30095B, $530E79, $C40E6A, $8B0080, $D5C00C0
                dc.l $4C0D73, $9C00A8, $400D80, $1A80, $2200, $9000900
                dc.l $4800000, $2800000, $51B00F3, $F00102, $10F1300, $255D00F0
                dc.l $26000F0, $5B0C005C, $28700FF, $2E080038, SRCEN, $9000F0
                dc.l $19FA0040, $8301FB, $380042, $FF00FF, $320000, $FF00FE
                dc.l $BC0000, $A850C02, $BAFD0004, $43FFB8FF, $FF009A
                dc.l $8000FF, $FF009B, $18FF, $FE007C, $301FE, $9E0002
                dc.l $FF00FF, $3201FF, $FE00BC, $180000B6, $1F004C, $DEC0080
                dc.l $240D1F, $FA0040, $2000030, $97F00E9, optionbu, $C009AA
                dc.l $8C0000, $2010000, $995003A, $800200, $E00981, $E800A4
                dc.l $20000C3, $9A100ED, $8C00C0, $1008900, $FF00FFFF
                dc.l $FF00FF00, $FF00FFFF, $FF00FF00, $FF00FFFF, $FF00FF00
                dc.l $FF00FFFF, $FF00FF00, $1100000, $30C0500, $E500C3
                dc.l $2BE00FF, $1000200, $6B80278, $DBCF8F, $20B00, $D20009C
                dc.l $9C0100, $FF00FF, $EBFF0D6C, bbutton, 0
                dc.l $D41001F, $DC000056, $9A009A, $D70004A, $AE0D09, $360010
                dc.l $D3C00BB, $800D50, $FA0E5E, $D400E0, $2020020, $93D00AE
                dc.l $6E0D4C, $E2005D, $A04D04, $9040904, $F000000, UPDA1F
                dc.l $1001300, $1100000, $83AA004D, $209004C, $2DFD00A8
                dc.l $6000FF, $FF0022, $19F7, $D400C3, 6, $2100E2, $2260001
                dc.l $2B00002, $A8F0002, $3000702, $FFFFE226, $102B0, $21700
                dc.l $8A009F, $E20DB3, $F00006, $D180054, $600D61, $510080
                dc.l $D8100C2, $E710089, $C00D62, $D600C6, $D7B0033, $420020
                dc.l $8B00FF00, $FFFFFF00, $FF00FF00, $FFFFFF00, $FF00FF00
                dc.l $FFFFFF00, $FF00FF00, $FFFFFF00, $FF000114, $8002B6
                dc.l $E802B7, $180102, $600FF, $2FD007B, $22400B6, $2580050
                dc.l $26000D8, $286001B, $DE0959, $C4002C, $1610008, $304D00
                dc.l $650040, $6E9A0000, $B000D40, $8A0090, $1400070, $C40104
                dc.l padright, DCOMPEN, $DDFF0D7E, $9F0101, $8E007F, $FF0121
                dc.l $C00A00, 0
                dc.l $DB0000CF, $E40016, $D0E0002, $620D24, $820020, $C080092
                dc.l $80EC2, $B60EAA, $5F0040, $20000E0, $978003C, $420203
                dc.l $4309B8, $DE00ED, $600D85, $F01ADB, $E73BDB, $E70285
                dc.l $F01F00, $1140080, $225700A8, $25E00FE, $5B090045
                dc.l $20700F3, $2C000000, $310070, SRCEN, $8B00F0, $18000000
                dc.l $9E0080, $2860000, $29000C0, $04, $7A0040, $A9A0000
                dc.l $3010702, $BAFF00FF, $43FFBAA1, $3A21B88, $2AA, $232
                dc.l $2BC, $1700, $E40082, $CC0D28, $D200A4, $D280022
                dc.l $400DA0, $890000, $DD6000C, $EBB004A, $800DA3, $B0024
                dc.l $DCB003A, $C400C0, $8B00FF00, $FFFFFF00, $FF00FF00
                dc.l $FFFFFF00, $FF00FF00, $FFFFFF00, $FF00FF00, $FFFFFF00
                dc.l $FF000110, $2A1, $D00050, $1990012, $4C0001, $33003B
                dc.l $2A900FF, $22900DC, $A9B001F, $FFBD0A, $940060, $10F00FF
                dc.l $E00A95, $80B00, $D1000DD, $AC0100, $FF00FF, $10800FF
                dc.l $FFE7FF, $DC1003F, LFU_A, 0
                dc.l $FF00FF, $A0E800, $7800A2, $130D9C, $8A00C9, $D15002A
                dc.l $900C00, $5400AA, $400778, $4C0303, $7000E3, $E6200C6
                dc.l $A00D55, $FC00E9, $20000C3, $96B002E, $870030, $D800000
                dc.l $1A800000, $3B800000, $2800000, $1F000110, $2258
                dc.l $5C002C, $16100A4, $305A14, $16020D, $442D01, $8100BF
                dc.l $FF00FD, $B800A3, $18FF00F6, $BC0003, $FF00FC, $9C0062
                dc.l $2320000, $FF00FE, $BC0000, $A950008, $B02BAFD, $443FF
                dc.l $BA9300A7, $2940023, $1A7C0003, $29E0002, $1F00008D
                dc.l $4000C9, $DB7005B, $B0D18, $CA00B0, $D63002A, $C00D84
                dc.l $390E73, $B100E0, $D6400B7, $6B0D7D, $8A005E, $908B00
                dc.l $FF00FFFF, $FF00FF00, $FF00FFFF, $FF00FF00, $FF00FFFF
                dc.l $FF00FF00, $FF00FFFF, $FF00FF00, $1140080, $2A9000E
                dc.l $1B2, $C20000, $19E0013, $2C30003, $1FF008C, $7E0338
                dc.l $6B800F8, $BE0800F0, $108, allpad, $B000B00, $C010004
                dc.l $FF00FF, $10004, $3400310, 0
                dc.l $1200000, SRCEN, $4DDFF, $C0000D2, $AF02F7, $9F00FF, $1EA00C0
                dc.l $2580038, $26000D8, $286001F, $FFDB00, $CF0049, $750779
                dc.l $500304, $D0039, $4F0779, $500304, $180036, $B0021A
                dc.l $200806, $9800C5, $A00206, $7009C2, $250EA9, $E00060
                dc.l $D9300E2, $2F0C08, $B80055, $410050, $D2100BE, $1A870096
                dc.l $3B870096, $22100BE, $1F000114, $80B5FF, $FF00CE
                dc.l $E702AE, $1AF9005A, $C301F7, $3C0082, $1F90041, $E101FF
                dc.l $290022, $B000B02, $FFFFE226, $102B0, $21700, $E300E8
                dc.l $2B0D28, $90091, $D280007, $100DA0, $1C0040, $DD5007B
                dc.l $EBA00CB, $A00DA2, $9C00B1, $DCA00B1, $1800B0, $8B00FF00
                dc.l $FFFFFF00, $FF00FF00, $FFFFFF00, $FF00FF00, $FFFFFF00
                dc.l $FF00FF00, $FFFFFF00, $FF000114, $8002A1, $D00286
                dc.l $280102, $54009F, $B50002, $3F002F, $DC0120, $6C0258
                dc.l $500260, $D80259, $ED00FF, $95900B8, $2610008, $4E000065
                dc.l $401501, $46FF00F6, $E00002, $FF00F9, $CF00FF, $8650021
                dc.l $900804, $1010100, $15C10080, $801, $4BF10, $32019FF
                dc.l $2156004B, $FFBCFF, $FE00C0, $2021B, $FF1700, $920011
                dc.l $8D0DBD, $9E0017, $D0A0041, $800C06, $66008D, $C00D88
                dc.l $BD0E77, $A50060, $D680027, $F70D81, $D300F2, $D08B00
                dc.l $FF00FFFF, $FF00E0FF, $FF0301, $7800C0, $1700FF00
                dc.l $FFFFFF00, $E0FF00FF, $1DE0101, $7800C0, $1700FF00
                dc.l $FFFFFF00, $810700EC, $5B0700EC, $35A0101, $780098
                dc.l $1700FF00, $FFFFFF00, $E0FF00FF, $1DE0101, $7800C0
                dc.l $1700FF00, $FFFFFF00, $E0FF00FF, $1DE0101, $7800C0
                dc.l $17000110, $2CC, $B402AB, $120101, $260077, $FF0001
                dc.l $7F00D3, $FF00FF, $FF00D8, $26D0083, $25A00A6, $80016D
                dc.l $C2BD00, $100004, $C00000, $A00A1, $C00A9C, $150B00
                dc.l $D400340, $AE00EC, $10200FF, $FF0110, $130024, $1200011
                dc.l $200D40, 0
                dc.l $1800000, $B004, $419FF, $C020018, $2F00FF, $20024
                dc.l $FF00FF, $FF00F1, $600266, $C00260, $D80230, $470A59
                dc.l $B80261, $8AD00, $8007A, $109, $E0000, $170000CD
                dc.l $F70067, $D200001, $290206, $230808, $90040, $3040A44
                dc.l $B90040, $D9B004C, $500202, $7009D1, $C80080, $2010020
                dc.l $94900CC, $250D9B, $150084, $700D8E, $7C5B8E, $7C051B
                dc.l $F300F0, $17000110, $2352009E, $24A0079, $20030151
                dc.l $B700FF, $2080326, $6F021B, $1314FF, $B00066, $61F00FC
                dc.l $25F00FC, $25B0070, $1E010101, $56600CC, $25D0018
                dc.l $30D00AB, $21750B0F, $90040, $1010037, $E00302, $101004F
                dc.l $FF0065, $21009C, $150703, $3092510, $44004B, $102003D
                dc.l $2B4510, $D20014, $5200320, $2010004, $52010004, $2300E7
                dc.l $9400380, $19FF2566, $6025A, $FC5240, $31D0078, $51FF00EA
                dc.l $B090008, $2090008, $180000DE, $FA009C, $D2100A1
                dc.l $140D01, $FA0030, $2010090, $3280044, $306009C, $A50ED0
                dc.l $DC0E87, $A00302, $700378, $4C0303, $3F005C, $740216
                dc.l $F30976, $BA0001, $C00115, $336900, $2031B, $B300F0
                dc.l $1700FF00, $FFFFFF00, $FF00FF00, $FFFFFF00, $81FF00FF
                dc.l $5BFF00FF, $300011B, $B300F0, $1700FF00, $FFFFFF00
                dc.l $E0000002, $100011B, $B300F8, $1700FF00, $FFFFFF00
                dc.l $FF000114, $8002A9, $5C028A, $360102, $F90073, $10200F0
                dc.l $AB0100, $1300F2, $250001D, $800165, $4C0000, $18B007D
                dc.l $BE080081, $4000FF, $F5006E, $A93, $B00, $EB5002C
                dc.l $18000E6, $6C0280, $232, $2C0208, $D80D00, $FF00FF
                dc.l $10000FF, $FFAC01, $40200, $1019FF, $DF00203, $30200
                dc.l 0
                dc.l $25A00D8, $2620010, $26200C3, $A000000, bbutton
                dc.l $AE03006A, $1FF00F9, $FE1800, $580045, $290DBF, $5A002B
                dc.l $2010063, $90A0400, $AF100F2, $C00D42, $990000, $2010000
                dc.l $91A0045, $E00200, $E00950, $7D007B, $2060023, $9BC0083
                dc.l $940090, $1090013, $AA700E8, $1ADA00C4, $3BDA00C4
                dc.l $2A700E8, $5000000, $1700, $10023C0, $2C0, $2008
                dc.l $1080000, optionbu, $3800000, $2800000, $14000000, $600
                dc.l $240, $214, $1E00, $10305C0, $2C0, $308, $2108, $B0800F0
                dc.l $108, allpad, $3000100
                dcb.l 3,0
                dc.l $7000300, $250000FF, $FF0100, $FF00FF, $450000FF
                dc.l $FF0500, $3000200, $5200, $FF, $FF0900, $30019FF
                dc.l $25000000, bbutton, $52000300, $5100, $B00, optionbu
                dc.l WID8, d_z, $D00, 0
                dc.l $D000000, optionbu, $300325, $380300, 0
                dcb.l 2,$E000000
                dc.l $30000E0, $3250038, $3000000, 0
                dc.l $20000C3, $9000000, 0
                dc.l $10000F3, $69FF00FF, $3010078, $C01700, $FF00FFFF
                dc.l $FF00FF00, $FF00FFFF, $FF008107, $EC5B07, $EC035A
                dc.l $1010078, $981700, $FF00FFFF, $FF00E0FF, $FF01DE
                dc.l $1010078, $C01700, $1140080, $2281009C, $24C00AA
                dc.l $5B340033, $2360079, $2CFF00FE, $690008, $100004B
                dc.l $101908, $5700C0, $FF00FF, $520201, $6400C0, $11000C0
                dc.l $E00865, $210093, $401030A, $1040102, $250400D3, $100108
                dc.l $880040, $8D010302, $1B100320, $3400380, $19FF2558
                dc.l $80025F, $A08DFF, $FF0022, $8000FF, $FF00E9, $1A010018
                dc.l $2010018
                dcb.l 2,$2090008
                dc.l $18000052, $8F001F, $D6B0008, $2D0D28, $160080, $2050020
                dc.l $998008F, $800206, $F0094D, $2F0E43, $890020, $D3A00CA
                dc.l $CD0D49, $48006F, $F00DA7, $E81ADA, $C43BDA, $C402A7
                dc.l $E80000, $20100, $11B00B3, $F01700, $1100000, $2B70090
                dc.l $2AB0018, $28D0007, $2660047, $2020050, $2580050
                dc.l $160, $D8027A, $FC0BC4, $2C0330, $AD0F00FF, $E00000
                dc.l $B0049, $E00A87, $C000E80, $280, $500, $FF00FF, $10000FF
                dc.l $FFC000, $300, $19FF0D35, $3F0102, $3102FF, $F800E0
                dcb.l 2,bbutton
                dc.l $249000F, $BDFF00FE, $DE0100, $44, $18000003, $C7003E
                dc.l $D51007B, $5A0E41, $EB60086, $800DE3, $EC0307, $D00939
                dc.l $840090, $2090050, $3790050, $30400B9, $60009A, $20000C3
                dc.l $9E70011, $6F00E0, $10000F3, $A9200E5, $1A800000
                dc.l $3B800000, $29200E5, $1F00FF00, $FFFFFF00, $FF00FF00
                dc.l $FFFFFF00, $FF000110, $235B0068, $25D00F0, $15FF00AA
                dc.l $201300, $30015FF, $C600D2, $9FF00CD, $C00215, $742001
                dc.l $316A0040, $20300EE, $800102, $240201, $D700E0, $10500E2
                dc.l $200302, $1010077, $FF0065, $210087, $8030309, $71200080
                dc.l $200920, $56010002, $280070, $9400380, $19FF7D40
                dc.l $56FF00ED, $B090008, $2090008, $18000071, $33009B
                dc.l $D03003A, $130200, $B30806, $1300DC, $100D4F, $700040
                dc.l $D6900EB, $E5C00AD, $A00D50, $AE0001, $D640090, $AF00B0
                dc.l $D800000, $5B800000, $300011B, $B300F0, $1700FF00
                dc.l $FFFFFF00, $FF000100, $22C0, $2C0, $5B04, $204, $2C00
                dc.l vfb_ysca, $102, 0
                dc.l $19010000, 0
                dc.l star, $20800F0, $108, allpad, DCOMPEN, UPDA2, $3000100
                dc.l $1002500, $FF00FF, $10000FF, $FF8D00, $3001B00, $3000300
                dc.l $30019FF, $25000000, bbutton, $8D000000
                dcb.l 2,0
                dc.l $1A00
                dcb.l 3,optionbu
                dc.l WID8, d_z, $D00, 0
                dc.l $D000000, optionbu, $300900, 0
                dc.l $20000C0, $9000000, $E000000, $D00, 0
                dc.l $D000000, 0
                dc.l $DFF00FF, $1A800000, $3B800000, $2FF00FF, $FF00FF
                dc.l $1DE0101, $7800C0, $17000114, $800901, $DF0083, $1000065
                dc.l $D30226, $8A0A73, $DBBDFF, $F80044, $102, $F200C0
                dc.l $A820C00, $EC5006C, $2BB00B0, $C000000, $BF100320
                dc.l $19FF0C01, $23001F, $1010011, $5F0E35, $8FBFCC, $32C1800
                dc.l $D0075, $920D5E, $120016, $1DBD0055, $E2A00D8, $400D8E
                dc.l $E00D0, $7750040, $30000C0, $4A00D6, $DEF00AF, $FD0020
                dc.l $DDB0059, $5BDB0059, $1F00FF00, $FFFFFF00, $FF00FF00
                dc.l $FFFFFF00, $FF000114, $802260, $60025C, $B80E09, $4C0680
                dc.l SRCEN, $92000FB, $FF21A8, $A00900, padleft, $2080030
                dc.l $1E01075C, $94025C, $D60314, $A72152, $2001FF, $FE00AA
                dc.l UPDA1F, $ED0080, $100003E, $FF, $FA0048, $500, $290382
                dc.l $C0972FF, $FF0900, $63100320, $19FF7D00, $6300005E
                dc.l $1FF00FF, $BE1800, $7A00E1, $EF1D11, $710D06, $56003F
                dc.l $E7200FF, $E64009F, $200D57, $98003D, $D6D002F, $3C00F0
                dc.l $8B00FF00, $FFFFFF00, $FF00FF00, $FFFFFF00, $FF000D4F
                dc.l $3F0101, $7700AB, $1FF00DD, $66025A, $EA025D, $6024D
                dc.l $F2BD00, $500A6, $C000FF, $FD0088, $A8D, $C000D80
                dc.l $980030, $6FF00FF, $2800000, $4010004, $6300B4, $DBFF0D32
                dc.l $AF0254, $6F065E, $8D0080, $53E00FF, $DC000086, $710035
                dc.l $DFB0064, $8F1D12, $6B00C0, $DA1002A, $B00DAE, $8D00C0
                dc.l $DB100A2, $AF0202, $730928, $770150, $1030003, $A640087
                dc.l $5B640087, $1F00FF00, $FFFFFF00, $FF00FF00, $FFFFFF00
                dc.l $FF000100, $22C0, $2C0, $E00, $500, UPDA1F, $9080000
                dc.l $680, $3801500, 0
                dc.l $E140000, $1E000103, $5C00000, $2C00000, $3080000
                dc.l $21080000, $1000001, $201, 0
                dc.l $10800F0, $1000008, CLUT, bbutton, 0
                dc.l DSTA2, $3007100, $62000000, $FF00FF, $9000300, $19FFD400
                dc.l $C00, LFU_A, WID8, d_z, $D00, 0
                dc.l $2010063, DCOMPEN, 0
                dc.l $C000000, 0
                dc.l $D000000, $E000000, $D00, 0
                dc.l $D000000, 0
                dc.l $DDB0059, $5BDB0059, $1F000114, $802232, $B8025D
                dc.l $F0221D, $F00FF, $28FF00BC, $2A0F00, $16010101, $55C002E
                dc.l $25E007A, $9FF00FF, $C30200, $1AFF00F4, $EA0080, $FF00FE
                dc.l $EF0201, $E4020A, $8700C0, $3030103, $2300FF, $650021
                dc.l $8D080E, $1090100, $25100080, $280, $4D02, $67100320
                dc.l $19FF255D, $D8025E, $20B609, $140209, $141800, $79007A
                dc.l $8E0DBC, $3C002A, $3030378, $4C0303, $150050, $A00D55
                dc.l $420080, $D7100AE, $E630078, $400D56, $97008A, $D6B00EF
                dc.l $4400E0, $DDB0059, $5BDB0059, 2, $100011B, $B300F0
                dc.l $1700FF00, $FFFFFF00, $FF0005A7, $BE028B, $CE0102
                dc.l $51006F, $10300E6, $CB0100, $1300F2, $2600021, $25E00D2
                dc.l $80019F, $4B0BB8, $300, $AD0A00FB, $1FC, $3D00C0
                dc.l $A990C00, $D4000B5, $2C02E6, $6C0280, $110, $32002C
                dc.l $1200008, $D802FF, $FFBC01, $40310, $19FF0C02, $F0002F
                dc.l $1030003, $FF0100, 0
                dc.l $25A00D8, $162, $100262, $C3BD00, $3006A, $1FF00F9
                dc.l $FE1800, $4A0040, $B40DAD, $1F00DC, $E400EE8, $170000
                dc.l $D350074, $201, $90E, $C50080, $20000E0, $9FF00D3
                dc.l $6C0206, $230945, $3C0003, $400109, $130AA7, $E81ADA
                dc.l $C43BDA, $C402A7, $E81F00, $1140080, $229700FE, $27800F0
                dc.l $221C003B, $FF06A7, $1A02B8, $F6030A, $2A50016, $2A7001A
                dc.l $2B800F6, $D2500FC, $B4C0084, $22300BD, $D0400DB
                dc.l $FF0501, $75D00A2, $25F00E8, $30A0031, $3C650021
                dc.l $990401, $3010102, $1005540, $3800301, $3020340, $3800F08
                dc.l $85FF5680
                dcb.l 2,$3800380
                dc.l $3809400, $440000, $80D58, $190098, $D0B00E9, $800775
                dc.l $40042F, $A60878, $4C0303, $3F0088, $E370097, $8750040
                dc.l $4300064, $980D3C, $520010, $800DA7, $E81ADA, $C43BDA
                dc.l $C402A7, $E80000, $2031B, $B300F0, $1700FF00, $FFFFFF00
                dc.l $FF00FF00, $FFFFFF00, $810700EC, $5B0700EC, $35A0101
                dc.l $780098, $17000100, $22C0, $2C0, $2208, 0
                dc.l $28000000, $F04, $16000103, $5C00000, $2C00000, $9000000
                dc.l $100202, $1A000001, 0
                dc.l SRCEN, $208, $F00208, allpad, $3000100
                dcb.l 2,0
                dc.l DSTA2, $1000100, $250000FF, $FF02FF, $FF4D00, $67000300
                dc.l $19FF2500, optionbu, $B600, optionbu, WID8, d_z, $D00, 0
                dc.l $3630325, $380300, 0
                dc.l $D00, 0
                dc.l $D000000, $E000000, $D00, 0
                dc.l $D000000, 0
                dc.l $DFF00FF, $5BFF00FF, $FF00FF, $1DE0101, $7800C0, $1700FF00
                dc.l $FFFFFF00, $FF0005B3, $5802AE, $D80101, $DF00DF, $10100E0
                dc.l $1F0200, $259, $EF0080, $160006A, $26F00EF, $A560088
                dc.l $26000D8, $AE0800F0, $1000008, allpad, $A8C0C00
                dc.l $D0000FF, $FF0100, $FF00FF, $10000FF, $FF0140, $A00FC
                dc.l $180000F, padright, $BE00, $300, $19FF0C00
                dcb.l 3,0
                dc.l $559, $A00260, optionbu, 0
                dc.l $BD000000, LFU_A, WID8, $5200F2, $4C0D6B, $890024
                dc.l $C00000E, $8A0040, $C00003A, $290E4D, $8C0E43, $DA0E3B
                dc.l $1100A4, $20000C3, $8080049, $A000BC, $C00100, $F30807
                dc.l $18900C5, $1A890033, $3B890033, $28900C5, $1F000100
                dc.l $22C0, $2C0, $2208, 0
                dc.l $6800000, $2800000, $3000280
                dcb.l 2,$280
                dc.l $D00, $B04, $204, $D00, 0
                dc.l $50007C0, $2C0, $308, $3C00, 0
                dc.l $4000300, $1000100, $55000300
                dcb.l 2,$3000300
                dc.l $F0085FF, $56000300
                dcb.l 2,$3000300
                dc.l $94000000, padleft, $D000000, $D00, 0
                dc.l $7250038, $4000000, $8250038, $3000000, $E00, $825
                dc.l $380400, 0
                dc.l $D000000, 0
                dc.l $DFF00FF, $1A800000, $3B800000, $2FF00FF, $FF00FF
                dc.l $3010078, $C01700, $FF00FFFF, $FF00FF00, $FF00FFFF
                dc.l $FF00FF00, $FF00FFFF, $FF008180, $5B00, $1F00, $1140080
                dc.l $165D00F0, $26100C8, $69F015A, $1B50098, $5A1F00
                dc.l $FF00FB, $802A39, $B80F01, $B71501, $1FF05B2, $140059
                dc.l $1B500EC, $5A0204, $C624FF, $FD009C, $840100, $5700FF
                dc.l $FF00FE, $7B00C4, $10100D9, $BF0303, $1040097, $FD0065
                dc.l $21008C, $80E0109, $1004D08, $2B806710, $3800080
                dc.l $17FF, $E0FF00FD, $BC0004, $20B00FF, $170000B5, $8E0044
                dc.l $DEB00C3, $8C0D1F, $E000C0, $D7F0083, $EAA0004, $E9400C3
                dc.l $800D81, $81000C, $DA1006C, $6C0040, $6F001B00, $7590304
                dc.l $20003, $670102, $4B00FB, $1FF00F0, $BB00FF, $15D009A
                dc.l $AC0161, $D500B0, $17D00B3, $C2B02DE, $30BE9F, $C000D10
                dc.l $B200A0, $1200080, $102, $2300D8, $22900EC, $22200AC
                dc.l $DFFF0C01, $80003F, $FF0001, $F7007F, $FF02FF, $FF0158
                dc.l $F8002C, $330DF00, $8800E4, $420DB1, $AF0026, $D180006
                dc.l $600D60, $190080, $D800022, $E70001D, $C00D61, $9900E6
                dc.l $D7900A8, $480020, $DA500C3, $1A920019, $3B920019
                dc.l $2A500C3, $1F00FF00, $FFFFFF00, $FF00FF00, $FFFFFF00
                dc.l $FF00FF00, $FFFFFF00, $FF00FF00, $FFFFFF00, $FF00195F
                dc.l $AC025F, $CA2D00, $1200BE, $2A120016, $F000000, $260100C7
                dc.l $25FF0056, $2010039, $3F0000, $300E6, $800105, $5500A0
                dc.l $5070013, $FF029F, $C00D940, $B6007C, $18000B9, $1C1FFF
                dc.l $D8FF00FD, $A80004, $26700FF, $08, $A60080, $10400C5
                dc.l $1700, $EB0080, $3A0D31, $E9008E, $D29005C, $E00DA5
                dc.l $730080, $DDC009A, $EC10006, $C00DA8, $9004E, $C0800D1
                dc.l $7300F7, $A00B07, $7F000114, $800101, $480038, $101000F
                dc.l $380EEF, $2802C3, $C00273, $E300FF, $5D000065, $400B01
                dc.l $60650021, $900804, $1010100, $1A800000, $21400FC
                dc.l $DFFF19EF, $2802C0, $D8E000, $940023, $8E0DC0, $4F004A
                dc.l $D1A01A0, $D680002, $800D8A, $AE0E79, $580040, $D6900A2
                dc.l $8A0D83, $AB00D4, $E0711B, $C300F0, $1700FF00, $FFFFFF00
                dc.l $E0FF00FF, $3010078, $C01700, $FF00FFFF, $FF00E0FF
                dc.l $FF01DE, $1010078, $C01700, $FF00FFFF, $FF00E51B
                dc.l $C300F0, $17000114, $801901, $25001C, $6CD006E, $2BF00B8
                dc.l $20030122, $9300FF, $297700A0, $F0200AB, $15011012
                dc.l $C32D02, $300ED, $10F00B7, $AD0303, $1030087, $FE0065
                dc.l $210090, $80E0109, $100268E, $1C02A9, $10B510, $32019FF
                dc.l $25BD00F0, $2BD00A8, $B6090013, $ED0109, $1300ED
                dc.l $1700009C, $AA007D, $DCB0065, $E70D1B, $800070, $D6E0001
                dc.l $C00D92, $AD0E80, $570060, $D6F00B9, $C70D8B, $430061
                dc.l $D06CFF, $FF0301, $7800C0, $1700FF00, $FFFFFF00, $E0FF00FF
                dc.l $1DE0101, $7800C0, $17000110, $714, $16790047, $DC00FFFF
                dc.l $FF0000B7, $700C6, $DED00AE, $720D20, $230020, $D80008C
                dc.l $EAB0066, $E9500F9, $E82008E, $B20DA2, $BC0088, $608B00
                dc.l $FF00FFFF, $FF00FF00, $FF00FFFF, $FF00FF00, $FF00FFFF
                dc.l $FF00FF00, $1100000, $769E001E, $660D00E3, $AD0107
                dc.l $B500ED, $17004A80, $2D10, padright, $83FF4B03, $2D7700A0
                dc.l $840000BF, $8E00B5, $DF800C5, $F0D21, $A200F0, $D86008B
                dc.l $EB30065, $E9C00F8, $E8800A5, $EF0DAA, $540015, $508B00
                dc.l $FF00FFFF, $FF00FF00, $1140080, $609008C, $28400FF
                dc.l $FF0184, $FF00FF, $5C000D8, $6001C0, $DE0060, $171008F
                dc.l $8DFF00FF, $FF0016, $3E9F001F, $B001680, $609, $68DFFF
                dc.l $1A3E00D8, $360DF00, $4700DE, $B90D5D, $2100DB, $D0C0097
                dc.l $B00D32, $5E00C0, $D430029, $E3A00C3, $E00D33, $28003B
                dc.l $D3F00C4, $1D0090, $72F31800, $FF00FFFF, $FF00FF00
                dc.l $FF00FFFF, $FF00FF00, $FF00FFFF, $FF008107, $EC5B07
                dc.l $EC035A, $1010078, $981700, $1140080, $190000C0, $671
                dc.l $880022, $1BE000E, $5F1F04, $134002F, $F2297D, $FC0F00
                dc.l $260B, $63250F, $C600C0, $10900B4, $6C010F, $A30201
                dc.l $330B9F, $1F0B00, $4AFF00FF, $2D4000FF, $FF83FF, $275E035E
                dc.l $1F002D00, $8400, $500065, $A80D68, $380078, $D0E0017
                dc.l $800D38, $5E0000, $D4B0028, $E4100C3, $D39, $3F0078
                dc.l $D47005B, $AA0080, $8B00FF00, $FFFFFF00, $FF000400
                dc.l $A90200, $A300F8, $102002F, $5B0102, $4C00A3, $1FE00FF
                dc.l $680001, $158003E, $164, $AA0000, $1750063, $A59000A
                dc.l $2C0165, $820032, $5B000901, $16000000, $80000, $3E9C0000
                dc.l $B000D10, $2C0050, $1200038, $C00104, $A400A8
                dcb.l 2,$2FF00FF
                dc.l $DFFF0C02, $300BF, $FF0002, $2F007F, $FF00FF, $2010100
                dc.l 0
                dc.l LFU_A, $DF00, $9B0023, $1E0DC9, $6800FA, $D1B003B
                dc.l $A00C08, $6C00EE, $800D91, $3E0E7F, $160040, $D6E00A2
                dc.l $3A0D89, $E600ED, $E00D7F, $155B7F, $1506C3, $1800FF00
                dc.l $FFFFFF00, $FF00FF00, $FFFFFF00, $FF00FF00, $FFFFFF00
                dc.l $FF001960, $AE025F, $520665, $640000, $1740052, $4D2D
                dc.l $783060, $3600204, $F32500, $E0080, $102002E, $107
                dc.l $590080, $10400E3, $800A9C, $B00, $79006700, $30019FF
                dc.l $E22C0000, $22C0000, $170000D9, $F50097, $D1B001A
                dc.l $150D26, $470050, $D99001D, $400DCC, $270EB2, $A20020
                dc.l $D9B0081, $B50DC1, $D500C7, $708B00, $FF00FFFF, $FF00FF00
                dc.l $1100000, $1010054, $2002E8, $A40216, $1F0224, $BF0100
                dc.l 0
                dc.l $1C0, $2C0, $278, 0
                dc.l $9C00000, $1C0, 0
                dc.l $4D1A00A0, $781500, $58860C00, $D0000FF, $FF0100
                dc.l $FF00FF, $10000FF, $FFE7FF, $C000000
                dcb.l 3,0
                dc.l optionbu, $E7000018, $2400E8, $D1F0012, $380D04, $330080
                dc.l $C000010, $CE0000, $D160068, $E13009B, $D11, $110038
                dc.l $D150046, $1E0080, $D800000, $5B000000, $5000000
                dc.l $F81700, $FF00FFFF, $FF00FF00, $FF00FFFF, $FF00FF00
                dc.l $FF00FFFF, $FF00FF00, $1100000, $16BA0078, $2BC0040
                dc.l $5010002, $120101, $32007E, $4DFF00CD, $942600, $10F0E07
                dc.l $B3250F, $C600C0, $10900B4, $6C0106, $DF00ED, $1070027
                dc.l $ED0671, $FF0286, $C007940, $67100320, $19FFE21F
                dc.l $ED021F, $ED1700, $B3005B, $340DE8, $E7005C, $D1F007D
                dc.l $C00D7D, $F70000, $DA700F4, $E9200F5, $800D7F, $EE00DC
                dc.l $D9F0077, $1B0040, $8B00FF00, $FFFFFF00, $FF000525
                dc.l $280101, $8009C, $10100E1, $BF0101, $E5003F, $1FF00FF
                dc.l $E00306, $2BF0068, $800173, $6B00FF, $CE8E0006, $B000E34
                dc.l $AC021B, $100215, $F80104, $1E00BB, $1080038, $98DFFF
                dc.l $C020006, $BF00FF, $20012, $3F00FF, $FF00FF, $E002BF
                dc.l $5802BF, $88E000, $880042, $2A0DB0, $DC005E, $D1700E9
                dc.l $E00C08, $5F00A7, $800C07, $7F008A, $E6F0098, $C00D61
                dc.l $26001E, $D790017, $F600A0, $D870092, $1A7F0046, $22000900
                dc.l $900047F, $460287, $920102, $31B00C3, $F00102, $10F1300
                dc.l $FF00FFFF, $FF00FF00, $FF00FFFF, $FF00FF00, $FF00FFFF
                dc.l $FF00FF00, $19BF0058, $2C10026, $50000BF, $580100
                dc.l $C10050, $4D00000B, $420F03, $1617FF, $34FF00F8, $F40000
                dc.l $FF00FF, $CC0000, $10D0047, $C00106, $D40000, $5020037
                dc.l $38E0006, $B00FFFF, $E2380000, $2380000, $17000023
                dc.l $780076, $D7A00B1, $820D33, $340020, $DCC00D0, $800D11
                dc.l $160EEE, $F30040, $DD00003, $C20D03, $4800F3, $608B00
                dc.l $FF00FFFF, $FF00FF00, $1140080, $61B0050, $2F9007F
                dc.l $2EA00EF, $6BF0083, $800258, $179, $1FCF9A, $B00
                dc.l $1A0600AB, $100E1FF, $FF00009E, $690050, $DCD00AA
                dc.l $F00D1B, $CF0000, $D6F003C, $D94, $500E81, $C60000
                dc.l $D7000F8, $F00D8C, $D10035, $D88, $C51A80, $CE3B80
                dc.l $CE0288, $C51F00, $FF00FFFF, $FF00FF00, $FF00FFFF
                dc.l $FF00FF00, $FF00FFFF, $FF00FF00, $1140080, $75FF0095
                dc.l $28280F, $3D030034, $10E, $EB00C0, $A9A0000, $B00FFFF
                dc.l $E2440344, $18000039, $9F009C, $D970080, $140D37
                dc.l $190040, $DDC0065, $D25, $DC0E01, $200080, $DDF00D6
                dc.l $940D17, $20031, $C08B00, $FF00FFFF, $FF00FF00, $91400B4
                dc.l $1020012, $FF02D2, $3F0AC7, $50D381, $C00FFFF, $FF0000D0
                dc.l $D50018, $D0F003B, $C80D24, $AC0080, $D9200B2, $EC30098
                dc.l $EAB0025, $E9400FC, $C80DB9, $B50081, $808B00, $FF00FFFF
                dc.l $FF00FF00, $FF00FFFF, $FF00FF00, $FF00FFFF, $FF00FF00
                dc.l $28010017, $C04E9B, $D80F05, $5D1701, $3D0F0063, $C00109
                dc.l $530605, $390381, $C00FFFF, $FF00006C, $B0064, $DD90010
                dc.l $EC0D3F, $F600C0, $DFF00DB, $E550024, $E2A007F, $E0300DA
                dc.l $6C0D43, $E6007E, $408B00, $FF00FFFF, $FF00FF00, $5430058
                dc.l $20600BC, $20D003F, $2D7007F, $7D702B5, $80027F, $DCF9B
                dc.l $C002001, $200E0, $33DBFF, $214800BF, $FFDB00, $9D0049
                dc.l $610D8C, $3E0040, $C060048, $9F0030, $D22007C, $C00D83
                dc.l $510E52, $E600E0, $D270006, $B30D6F, $BE0018, $100D99
                dc.l $A45B99, $A41F00, $FF00FFFF, $FF00FF00, $FF00FFFF
                dc.l $FF00FF00, $FF00FFFF, $FF00FF00, $24010018, $AA022B
                dc.l $884EB1, $AE0F04, $CC170C, $3400000A, $2300C0, SRCEN
                dc.l $960060, $10C00A6, $40010E, $650040, $503008B, $39B0C00
                dc.l $D9200204, $40080, $290, $6F02C9, $8717FF, $D9020044
                dc.l $302020F, $FF00E0, $1080032, $801700, $38007F, $AD0DE2
                dc.l $EE0077, $D590042, $700202, $A00806, $8F00A5, $C00D14
                dc.l $DD0ED2, $410060, $D9500E4, $570DF9, $EF0014, $D08B00
                dc.l $FF00FFFF, $FF008184, $D11BCE, $3CCE0284, $D10000
                dc.l $20100, $11B00C3, $F01700, $1140080, $6DF00F8, $EEF0028
                dc.l $3900273, $E300FF, $5D000065, $400B01, $60650021
                dc.l $950804, $1010100, $1A800000, $21400FC, $DFFF19EF
                dc.l $2802C0, $D8E000, $C50000, $7F0D4C, $BE004D, $D4F009A
                dc.l $D00D3E, $6B0040, $DA8008F, $E73007D, $200D43, $6400ED
                dc.l $D93001A, $4500F0, $D8A006B, $1AFF00FF, $3BFF00FF
                dc.l $28A006B, $21100E9, $1B00FF00, $FFFFFF00, $E0FF00FF
                dc.l $3010078, $C01700, $1140080, $2101001B, $120080, $1FE00A4
                dc.l $800D0C
                dc.b 0, $48, 6
                dc.b $2E
                dc.l $301325, $BB0200, $260300D5, $20C33CFF, $FA0010, $2030095
                dc.l $C00302, $4650021, $950803, $3092501, $3025602, $400FD
                dc.l $1C5D10, $32019FF, $25C003C0, $B6FF00FF, $8001FF
                dc.l $FB00F8, $18000047, $4B008F, $D5C0062, $7D0E44, $D00201
                dc.l $E00806, $3100F7, $400D42, $9F0E3A, $4B0020, $D3200BF
                dc.l $1D0D3F, $410016, $F0711B, $F300F8, $1700FF00, $FFFFFF00
                dc.l $E51B00C3, $F01700, $1140080, $36390059, $5FF00F2
                dc.l $D131D, $D3022B, $7F0ACD, $CF1900, $12FF0087, $D82201
                dc.l $BA0070, $10100A6, $C80D09, $CB00C0, $111000B7, $C00102
                dc.l $200965, $210095, $80D0106, $1003901, $7021201, $400DA
                dc.l $486140, $3010E01, $41340, $38019FF, $40FF00F9, $7C1320
                dc.l $DF60FF, $FF00CE, $8000FF, $FF00DC, $E0A003E, $12090008
                dc.l $2090008, $18000036, $D00B3, $619006F, $C0305, $9200DB
                dc.l $A90619, $6F000C, $305000D, $C70090, $2020060, $80600D9
                dc.l $E20040, $D220083, $EFE0032, $A00DDD, $4900C9, $D1300D4
                dc.l $810030, $711B00C3, $F01700, $1140080, $22C400B6
                dc.l $2F60075, $90100AD, $7F00FF, $38FF00FE, $6801FF, $FF00DC
                dc.l $80052B, $786C03, $1030065, $FF0065, $210095, $80C0105
                dc.l $1002540, $380D5FF, $25BC00E8, $2960078, $D4000039
                dc.l $690039, $D970039, $5B0D37, $F00B0, $DDC003E, $C00D25
                dc.l $A90FF3, $E00DDF, $AF00BB, $C080016, $D400C5, $900B07
                dc.l $651B00C3, $F01700, $1100000, $101002C, $A80101, $B0006
                dc.l $E7930C00, $FFFFFF00, $D300DE, $800D60, $130080, $D520038
                dc.l $D48, $E00000, $DB60080, $E7F00B0, $D4E, $30080, $DA00056
                dc.l $E80000, $D800000, $1A800000, $3B800000, $2800000
                dc.l bbutton, $1B00FF00, $FFFFFF00, $FF000100, $2100
                dc.l $C00000, $1C0, 0
                dc.l $D000000, $6000000, $13800000, $2802614, $2003, $3C000008
                dc.l $F00208, allpad, $3000400, 0
                dc.l $8000300, $25000300, $56000000, $FF00FF, $5D000300
                dc.l $19FF2500, $300B600, 0
                dc.l LFU_A, WID8, d_z, $D00, 0
                dc.l $E000000, $2000030, DCOMPEN, 0
                dc.l $D000000, $E000000, $D00, 0
                dc.l $D000000, 0
                dc.l $6CFF00FF, $1DE0101, $7800C0, $17000110, $23E00050
                dc.l $8001DF, $C0080, $2A1900E7, $20015FF, $E80080, $E31008C
                dc.l $524300D0, $A0300E5, $8000FF, $FD008A, $800302, $4650021
                dc.l $930803, $3092502, padright, $1010080, $4510, padright
                dc.l $60040004, $3600B4, $27FF25CE, $8802C0, $AAFF00F7
                dc.l $29000064, $1C00F0, $2040031, $92F000F, $D00E3E, $F00201
                dc.l $E00806, $190074, $E2100F0, $E1D00B2, $E1900D9, $D00D20
                dc.l $3800EF, $73F300F8, $17000110, $3624, $4E06C3, $7A11FF
                dc.l $FF00F6, $3C60FF, $FE00BD, $600100, $FE0070, $D0300FD
                dc.l $E01104, $C30080, $10C003E, $600A93, $C00FFFF, $FF000044
                dc.l $EB00B4, $76E00F4, $4A60030, $DC076E, $F414E4, $570000
                dc.l $D300074, $E0A0065, $800DE7, $E8005C, $D210011, $230040
                dc.l $8B000110, $227F, $9202C9, $F00900, $D0003F, $1F91008F
                dc.l $288007F, $14000000, $180100, $500FA, $53D, $A06E04
                dc.l $E50393, $C002500, $3004B80, $89FF2500, optionbu, $4A05
                dc.l $FA8800, $480047, $3A0DAA, $8E008E, $D3900AC, $E00DE6
                dc.l $B30080, $D33009A, $E0D0026, $C00DEA, $4E004E, $C000024
                dc.l $E0067, $A00B08, $7F000114, $80023D, $28020A, $6AE784
                dc.l $C00FFFF, $FF000035, $2F00E2, $DDE00A0, $60D63, $540060
                dc.l $D8D0051, $800D11, $C20ECF, $8900C0, $D930086, $C60DF6
                dc.l $FC0042, $200D81, $CA1A85, $163B85, $160281, $CA0208
                dc.l $8A1B00, $FF00FFFF, $FF00FF00, $1140080, $22C10054
                dc.l $8001C3, $CD0080, $157F00FF, $13000300, $162E00EE
                dc.l $E17005C, $5E0200B8, $20300AE, $4020465, $210084
                dc.l $8030309, $25400380, $46020004, padright, $8BFF25C0
                dc.l $3C0D500, $2000AF, $A0D2A, $2C00FE, $D010042, $100202
                dc.l $700806, $1600CF, $800D1E, $6A0E1A, $9C00C0, $D17002A
                dc.l $BE0C08, $1C00E0, $8400A0, $B074100, $9000900, $4800380
                dc.l $1000002, $100011B, $F300F8, $102010F, $13000100
                dc.l $23C00000, $1C0, 0
                dc.l $2A800000, $2801500, 0
                dc.l $E140000, $52080000, $A0800F0, 0
                dc.l $800F0, $300, $4000000, DSTA2, $3002500, $FF00FF, $10000FF
                dc.l $FF4500, $FF00FF, $60000000, $FF00FF, $27FF2500, optionbu
                dc.l $AA000000, $29000000, padleft, $2010011, $9000000
                dc.l $E00, optionbu, $300800, 0
                dcb.l 3,$E000000
                dc.l $D00, 0
                dc.l $73C300F0, $17000114, $8035FF, $D700B9, $5000014
                dc.l $8113FA, $64025C, $A75C00, $BE, $D800FF, $FE0099
                dc.l $C80ECD, $40110C, $C90060, $1010050, $200A84, $C00FFFF
                dc.l $56260053, $A70000A6, $3D0016, $D2400BD, $621D28
                dc.l $C80080, $D8B00B6, $E5A003F, $400D2D, $6B00A2, $D7700B6
                dc.l $7D0060, $8B000114, $803271, $1F1404, $1190013, $FF0680
                dc.l $280, $1545, $2801FF, $FF00E6, $22900B0, $23700C8
                dc.l $22014B03, $650384, $C007500, $89FF7500, $8800, $A90098
                dc.l $9C0D29, $1B0014, $D4A00C9, $400D2B, $250000, $D8E00DC
                dc.l $E5D0000, $800D2F, $D10094, $D7A00B3, $C100C0, $B007F00
                dc.l $53E0018, $20E001E, $167C0037, $CF8B000A, $B00FFFF
                dc.l $FF0000E3, $250003, $D7300F0, $990D65, $6800D0, $2010040
                dc.l $8060053, $9E0040, $DC400D3, $E8C0038, $A00D58, $EC00B9
                dc.l $DAD00F0, $960030, $D800000, $1A800000, $3B800000
                dc.l $2800000, $21A0006, $11B00F3, $F81700, $FF00FFFF
                dc.l $FF00FF00, $25AE009D, $2A60012, $165F0080, $8000107
                dc.l $FF00FF, $613007F, $18FF00E2, $140672, $3C0604, $911E01
                dc.l $1A50E0D, $102CFF, $FD0029, $1FF00F7, $D50080, $6D700FF
                dc.l $28B000A, $B092501, padright, $1020080, $1E80, $2400
                dc.l $400720, $6080004, $890078, $7BFFFF00, $1E00F0, $370D27
                dc.l $E700F5, $D000044, $A00200, $300915, $950040, $D1C00C7
                dc.l $E19002E, $200D15, $EB0095, $C00001B, $5200B1, $700B00
                dc.l $7F00FF00, $FFFFFF00, $FF003800, $2E007F, $63100B0
                dc.l $13E60053, $2690067, $A7B004F, $2DE000A0, $21FF00FE
                dc.l $400D0, SRCEN, $4F00F8, $CFF00FA, $450080, $110100E1
                dc.l $400102, $2A00E0, $A8B000A, $B0056FF, $FFA7FF, $42801313
                dc.l $9B74FF, $FF00EE, $802F00, $540032, $370DBA, $D00F5
                dc.l $EC10EEF, $150040, $D3E00C7, $E1600EE, $200DF2, $D10095
                dc.l $D2E00AA, $D10070, $8B002596, $E10240, $300080, $988009F
                dc.l $1410010F, $2F0788, $7F18FF, $E100D4, $3E40200, $1FF
                dc.l $B70020, $24014E8B, $A0B00, $25400380, $47100082
                dc.l $800920, $81FF253A, $3E028F, $1045FF, $FE0060, $A2B0080
                dc.l $80000057, $8D00BD, $DBE006B, $A70D3C, $5C0070, $DF10071
                dc.l $C00D41, $ED0E19, $AF0060, $DF50037, $870D31, $A80015
                dc.l $D08B00, $1100000, $2460070, $20C0026, $164C00FB
                dc.l $CF8F0000, $B00FFFF, $FF0000FB, $BC0002, $D9300EA
                dc.l $661D64, $E90080, $DDB00E2, $EA00065, $C00D6A, $7D0026
                dc.l $DC300D5, $440020, $4D000900, $9000B02, 0
                dc.l $11C0003, $202010F, $13000110, $23810054, $2BF0076
                dc.l $5F00180F, $3503009F, $2030024, $800103, $5A0040
                dc.l $FF00FF, $DA0403, $10200C9, $FF0065, $21008F, $80E0109
                dc.l $100E101, $1DFFE0FF, $FF00CC, $1C000026, $1200F8
                dc.l $D31002F, $680D06, $A60080, $D1A009A, $E230078, $E1F0009
                dc.l $E1B0004, $680D21, $AD001F, $806C00, $2031B, $F300F8
                dc.l $17000100, $22C0, 0
                dc.l $1C00000, $1500, $808, $1080000, $680, $280, $15000000
                dc.l $600, $614, $1E00, $1030E08, $2C00, $800F0, $1000008
                dc.l allpad, $3000200
                dcb.l 2,0
                dc.l $700, $3002500, $FF00FF, $10000FF, $FF1EFF, $FF2500
                dc.l $FF00FF, $5000600, $FF, $FF7BFF, $25000300, $D5000000
                dc.l padleft, $D000000, $E00, $C00, 0
                dc.l $D00, $E00, 0
                dc.l $D000000, $D00, 0
                dc.l $4D04, $9040904, $4000300, $1FF00FF, $1DE0101, $7800C0
                dc.l $1000100, $1300FF00, $FFFFFF00, $FF000110, $3604
                dc.l $9E05FF, $D30022, $13DC007B, $60000001, $220058, LFU_A
                dc.l $F00DF9, $A120E, $570060, $1060010, $200A8F, $B00
                dc.l $FFFFFF00, $6C00C9, $36076F, $C04DA, $700C2, $76F000C
                dc.l $14000060, $800D55, $D60E2B, $1B0040, $D040062, $20D44
                dc.l $8F007F, $608B00, $1100000, $21FF00DD, $350271, $982009
                dc.l $1070073, $22F800C4, $ACB0080, $240F0575, $DE02CD
                dc.l $E438F, $B00, $FFFFFF00, $700024, $BC0DDE, $650074
                dc.l $D4000AF, $400D02, $BD0000, $D5800FC, $E2D00DC, $800D06
                dc.l $C700F4, $D47008C, $C300C0, $8B000114, $80023E, $18020A
                dc.l $E21655, $73CF83, $C00FFFF, $FF000041, $360099, $DEE0043
                dc.l $7B1D95, $C600C0, $D1D0009, $ED90067, $E00D9C, $1D00DB
                dc.l $D0100B1, $5B0090, $8B000114, $802252, $DA02DF, $82003
                dc.l $13F000B, $FF2909, $EC2601, $1011613, $8000FF, $FF00EC
                dc.l $C01902, $E00080, $106006C, $108, allpad, $08, $F00604
                dc.l $E70383, $C007920, $67001DFF, $E0000000, $1C00, $4E00BC
                dc.l $290D66, $F002B, $D0D00CC, $B00D37, $3200C0, $D490099
                dc.l $E950089, $F00202, $A00378, $4C0303, $38000F, $8B0D45
                dc.l $E000D4, $908B00, $FF00FFFF, $FF00FF00, $FF00FFFF
                dc.l $FF00FF00, $1140080, $35FF00FD, $3E06F7, $3F13E7
                dc.l $4F60FF, $FF00EF, $800FF, $FF00C5, $EFA0065, $1109
                dc.l $8A0020, $10C00CB, $A00A83, $C00FFFF, $FF0000B2, $4300CD
                dc.l $76E00F4, $4340060, $D7076E, $F41431, $3D00C0, $D9600FD
                dc.l $E64001D, $600D36, $200B7, $D82006B, $9600D0, $8B000114
                dc.l $802100, $F8001D, $8001E9, $F10000, $25030075, $C01C00
                dc.l $2F004E, $ACF00C0, $24A5056D, $5602DF, $FE04FF, $390100EF
                dc.l $3830C00, $FFFF25C5, $1C02B7, $60D400, $B5009F, $530D38
                dc.l $BE0089, $D4C00E6, $900D33, $9A0040, $D9A0023, $E6600DE
                dc.l $A00D38, $6800A9, $D850068, $DB0030, $8B00090E, $E41678
                dc.l $3FCF8F, $C00FFFF, $FF000016, $AF00A8, $DB600F6, $781D77
                dc.l $DE0000, $DF50028, $EB60083, $D7D, $BD0078, $DDB00D4
                dc.l $4A0080, $4D040904, $9040B00, $31B00F3, $2000100
                dc.l $130025C0, $2C0, $2008, $1080000, $2900, $2600, $1FF1610
                dc.l 0
                dc.l $20000, $18FF, $F7008A, $3B50205, $70080, $1090008
                dc.l $7F5038F, $C007900, $67800090, $C41BFF, $E1090008
                dc.l $1C00000A, $2800FB, $D0C00E3, $10D01, $BE0010, $D0600F8
                dc.l $400D09, $4B0E08, $2100A0, $20000E0, $3750040, $3000007
                dc.l $140021, $D0800D2, $C500B0, $8B00FF00, $FFFFFF00
                dc.l $FF00FF00, $FFFFFF00, $FF003800, $E0008, $6D30094
                dc.l $13E2008B, $61FE0053, $E00000, rsym_ste, $400DF9, $CC0080
                dc.l $1103001D, $800106, $3F0000, $A8F0C00, $FFFFFF00
                dc.l $8700BC, $DC0DFD, $1300D4, $1D130055, $D6F, $1C0E41
                dc.l $380080, $D1700A2, $540D5C, $8E0085, $C08B00, $254C00D8
                dc.l $27300C9, $80453E, $980900, $1925FF, $49030065, $38F0C00
                dc.l $FFFF253A, $3E028F, $10D400, $8B0018, $620D01, $710086
                dc.l $D45006C, $600D15, $B10080, $D720042, $E4300F9, $C00D1A
                dc.l $80046, $D5F008B, $CA0020, $8B00090C, $AAE78A, $80B00
                dc.l $FFFFFF00, $900085, $E80D55, $650038, $1DCD008E, $E670068
                dc.l $E1A007B, $ED400C4, $380D48, $4E002E, $6F0F0031, $1B000100
                dc.l $8704, $18033400, star, $101, $208, allpad, $10800F0
                dc.l $4000100
                dcb.l 2,0
                dc.l DSTA2, $1000100, $E10000FF, $FF1BFF, $E1000000, $1C000000
                dc.l padleft, $D000000, $D00, 0
                dc.l $D000000, $D00, $E00, 0
                dc.l $7650500, 0
                dc.l $D000000, 0
                dc.l $6CFF00FF, $3010078, $C01700, $1140080, $229F00B7
                dc.l $2EE00DE, $800D01, $400612, $EF1300, $300164F, $FC3005
                dc.l $3D0700F1, $4000FF, $FC00A5, $A00302, $4650021, $8A0008
                dc.l $7030309, $25010080, $102, padright, $E800000, $50100AB
                dc.l $5C2C04, $46F40, $38000A7, $17FF, $25C003C0, $F010040
                dc.l $62F00C0, $2E080040, $6F8801FF, $FF004C, $18000012
                dc.l $E000F8, $D180039, $680D01, $420010, $2010080, $806000D
                dc.l $1A0E11, $780E0F, $490E0D, $4E0068, $D100095, $FF0080
                dc.l $4D000900, $9000480, $3800100, $20100, $11B00F3, $F80102
                dc.l $10F1300, $FF00FFFF, $FF00FF00, $38FF00DE, $EE0500
                dc.l $D00E4, $13E300DD, $24D00D3, $5E2101FF, $FE00D3, $700EF6
                dc.l $C01102, $55020A, $9B0080, $A8A0008, $B00FFFF, $FF000001
                dc.l $93001C, $D9B0082, $941D69, $50EE1, $5C0EA5, $300E6E
                dc.l $A90014, $DC90008, $698C00, $255C0008, $29B00D4, $2010010F
                dc.l $3301FF, $FF00F6, $291, $8F0288, $7F14FF, $FB0050
                dc.l $5FF00D6, $40028A, $F82405, $5BA0090, $2BA0042, $5FF00FF
                dc.l $FE0001, $8FF00FF, $FF001E, $2E8A0008, $B007920, $2010004
                dc.l $81FFFF00, $400EE, $A20D9F, $E00046, $D4A0041, $3020906
                dc.l $6B0061, $EE40082, $EA700F1, $E71000F, $60DCC, $500AE
                dc.l $8C000110, $240, $FA0338, $E79E0013, $B00FFFF, $FF000080
                dc.l $FE0058, $D410033, $881DC2, $A20E58, $D80E0D, $BD0EC9
                dc.l $AC0088, $D3A007A, $956F0C, $A41B00, $FF00FFFF, $FF00FF00
                dc.l $1100000, $22910020, $2D50096, $D00, $620, $402DFF
                dc.l $820098, $E0400A1, $20A53CFF, $FD000F, $8001FA, $CE0000
                dc.l $A9E0013, $B094180, padright, $2C000040, $E080004
                dc.l $5B00C0, $5D000300, $FF00FF, $17FF3900, $63B, $D02E00
                dc.l $6F00, LFU_A, WID8, $1000B9, $A30D15, $6C0079
                dc.l $E4100C0, $2020030, $90B0096, $400D0F, $730E0D, $8400A0
                dc.l $D0B00C4, $990D0E, $AB0020, $308B00, $FF00FFFF, $FF00FF00
                dc.l $1100000, $35000019, $F8061B, $9813D5, $2962FD, $D00000
                dc.l $AB, $C00DF8, $200040, $110500C5, $400110, $A400C0
                dc.l $A9E0013, $B00FFFF, $FF0000F2, $B008C, $D870050, $E41D5E
                dc.l $190ED2, $CC0E98, $720E63, $910064, $DBB0034, $D08C00
                dc.l $1100000, $2227007F, $2E8005D, $46E90078, $5000078
                dc.l $BC02A8, $6024A5, $1C000000, $FA, $2E9E0013, $B00C180
                dc.l padright, $3BFFC0FF, $FF00FF, $1E3B00, $F50067, $120D8B
                dc.l $AE0096, $1D600075, $ED500F2, $E9B0033, $E6500F7
                dc.l $560DBE, $320015, $8C000114, $800101, $3E0018, $101000D
                dc.l $820EEF, $2802C4, $EC033F, $FF5D00, $650040, $B016065
                dc.l $21009B, $8040101, $1001A80, $214, $FCDFFF, $19EF0028
                dc.l $2C000D8, $E00000E4, $77000F, $DC2008C, $FD0D65, $6800D0
                dc.l $2010040, $8060008, $970040, $DB6001F, $E5F005B, $200D10
                dc.l $B9009D, $D93000A, $CE00F0, $8B000114, $802271, $E80101
                dc.l $5800E8, $AE100BF, $FFB303, $107001D, $FF0065, $21009B
                dc.l $80C0105, $1002580, $D9FF2571, $E8D800, $5B0063, $E70D76
                dc.l $840005, $D100006, $500D40, $190040, $D550077, $E4A00C8
                dc.l $200D41, $1900A5, $D510025, $4C0070, $B08651B, $C300F0
                dc.l $1700FF00, $FFFFFF00, $E0FF00FF, $1DE0101, $7800C0
                dc.l $1700FF00, $FFFFFF00, $E51B00C3, $F01700, $1140080
                dc.l $35FF00E9, $F605FF, $BB00EF, $11FF00FF, $F20016, $258007F
                dc.l $279000B, $27F0023, $2DE008F, $19001314, $8B21FF
                dc.l $FE0076, $B800FF, $FF003E, $D00CFF, $F000A3, $601103
                dc.l $A800E0, $1010038, $9650021, $9B080D, $1060100, $39010702
                dc.l $12010004, $10800000, $2C010004, $23400301, $E010004
                dc.l $13400380, $19FF38FF, $FD009C, $5FF00F9, $801313
                dc.l $9B0E6A, $D22D36, $1021FF, $FF00CE, $8000FF, $FF00DC
                dc.l $DFF00F4, $D41209, $80209, $81800, $550084, $430619
                dc.l $6F000C, $3050008, $AA0059, $619006F, $C0305, $D00C1
                dc.l $900202, $600806, $A4000E, $400D30, $130EEA, $1000A0
                dc.l $DAA009E, $790D13, $C5000A, $30711B, $C300F0, $17000114
                dc.l $80226D, $9B028F, $100901, $3400BF, $FF1310, $10F0033
                dc.l $FF00FF, $FF00F6, $39B003F, $288007F, $153E0016, $1FF00FF
                dc.l $E401FF, $8D00E0, $1FF00B9, $602201, $777007D, $8001CC
                dc.l $460080, $218009A, $FF00FF, $FF00E3, $8FF00FF, $FF0087
                dc.l $27030103, $6500FF, $650021, $9B080C, $1050100, $254000C8
                dc.l $370180, $47100082, $800520, $3202B01, $3020310, $3800B40
                dc.l $3DFF2565, $D6028F, $1045FF, $FE0060, $A2B0080, $2ABA0090
                dc.l $2BA0030, $30800FB, $FF00FF, $FF00E3, $8FF00FF, $FF001E
                dc.l $3B000058, $DF00C9, $D0D0008, $B0D4A, $410060, $2020906
                dc.l $A6006A, $C00D33, $390EEC, $D100E0, $DAD0004, $6B0D16
                dc.l $C2004E, $900B08, $651B00C3, $F01700, $1100000, $60F004A
                dc.l $28400FF, $FF0184, $FF00FF, $9C00090, $D39A0003, $B00FFFF
                dc.l $FF00005D, $7400D4, $D5F00E2, $3C1D5D, $AF0000, $D270094
                dc.l $EC200A1, $800D67, $2500BC, $DFE00C3, $F50040, $8B000110
                dc.l $22C7, $980100, $C7008C, $9020050, $9F39FF, $C50B6B
                dc.l $7C2405, $4E9A0003, $B002500, $4B100B20, $81FF2500
                dc.l $5640, $810000D4, $6100AC, $D1300D9, $440D25, $4C0040
                dc.l $D950031, $DC6, $EC0EAE, $E0080, $D970085, $C40DBC
                dc.l $DE0072, $C00B00, $7F00FF00, $FFFFFF00, $FF00FF00
                dc.l $FFFFFF00, $FF000110, $36F4, $980500, $250006, $13E40019
                dc.l $21A004B, $281000F, $27A009A, $2AB0043, $2D06008E
                dc.l $21000000, $4D0090, $29300E0, $DEE00E3, $C0110B, $40040
                dc.l $109004E, $C00A9A, $30B00, $FFFFFF00, $6D00F9, $A8020C
                dc.l $61021E, $750040, $300001C, $1F0008, $20C0043, $21E0075
                dc.l $400300, $10F90026, $DA1, $880E4D, $570000, $D01000A
                dc.l $980D7F, $7E0030, $808B00, $1100000, $2101000A, $550266
                dc.l $DF0080, DCOMPEN, 0
                dc.l $13080108
                dcb.l 2,0
                dc.l $300, optionbu, $14FF, $BE00DC, LFU_A, $40100, 0
                dc.l $1000046, $6C0207, $5C1E00, $10505C0, 0
                dc.l $1C00000, $208
                dcb.l 2,0
                dc.l DSTA2, 0
                dc.l $1122, $900308, $3080205, $200210, $6B0060, $3020100
                dc.l $9F0000, $29A0003, $7030100, $1092510, $FF00FF, $12000F1
                dc.l $704540, $8B0094, $5000210, $40080, SRCEN, $40027, $AF2500
                dc.l $3000300, $3000B00, $12080004, $5600F0, $9010080
                dc.l $102, $6040004, $D0068, $FFF25C0, $2C0, $4500, 0
                dc.l $A400000, $2A000000, bbutton, $3000000
                dcb.l 2,0
                dc.l DCOMPEN, 0
                dc.l $10FF00E8, $B090008, $2090008, $18000048, $C60022
                dc.l $D5E004E, $C60D01, $3F00E0, $2010090, $9330001, $800DD9
                dc.l $240090, $2040030, $95800BC, $F00207, $933, $CD0086
                dc.l $D400092, $260020, $B0066D3, $18000114, $80060C, $BCE792
                dc.l $B00, $FFFFFF00, $4B0088, $150D48, $93002F, $1D510013
                dc.l $C00D16, $C50EB3, $EC0060, $D5A0058, $F0DEE, $CE006B
                dc.l $508B00, $1140080, $6D000052, $FC0AA6, $3C24FF, $4E920000
                dc.l $B00FFFF, $FF0000C2, $7400ED, $DFC008A, $370D22, $250070
                dc.l $D880095, $C00DB6, $1D0E9F, $590060, $D8A00B8, $170DAC
                dc.l $E800E8, $D08B00, $FF00FFFF, $FF00FF00, $FF00FFFF
                dc.l $FF00FF00, $1140080, $36D900F1, $60000C7, $13D60093
                dc.l $3CFF00F9, $82201, $C60070, $1FE00BE, $A80DED, $330000
                dc.l $111000E7, $C00107, $5800E0, $A920000, $B00FFFF, $FF00009D
                dc.l $E70039, $D4E0004, $DB1DEC, $8A00C0, $D9000B9, $E3E00A1
                dc.l $E00DF4, $3C00EB, $D6F0088, $A60090, $8B000114, $802100
                dc.l $610080, $2E900A3, $2A56, $A719FF, $D40A43, $400225
                dc.l $D620FF, $30FF00FA, $F00310, $31003D6, $200108, $FF0020
                dc.l $501004B, $FF0292, $B09, $800201FF, $FF6800, $FF
                dc.l $FF0FFF, $FF000036, $D90063, $D4600FF, $B91D26, $660040
                dc.l $D9200C1, $C00DE3, $130E26, $FF00D9, $D30009C, $9C0030
                dc.l $8B000110, $25A, $200211, $285, 0
                dc.l $1850000, $A6C, $27D00CF, $CF8C0C00, $FFFFFF00, $900EF
                dc.l $610D40, $2B0053, $1DD600FC, $EC90051, $E500026, $E00DE2
                dc.l $1B30D98, $540078, $106E01, $8E011B, $D300F0, $17000110
                dc.l $1ABE, $F21500, $7902FF, $CC0009, $6240039, $818010A
                dc.l $3700FF, $20FF0095, $5FF, $830044, $1FF0078, $400703
                dc.l $FE1901, $1A5059B, $B202E5, $AA030F, $E100FF, $FF00FF
                dc.l $FC08FF, $FF00FA, $542900, XADDINC, $28C0C00, $39010702
                dc.l $37402F01, padright, $1020080, $680, $A80, $2801, $40018
                dc.l $E00FFF, $38FF00FC, $5005FF, $F400E0, $39FF00B3, $E02ABF
                dc.l $E802C2, $2805FF, $FF00FF, $FF4700, $8000DC, $390DF4
                dc.l $22005B, $D21003D, $202, $900806, $E007E, $E6800A9
                dc.l $E3B0093, $E00D12, $1BB0D56, $6E00F5, $908B00, $FF00FFFF
                dc.l $FF00FF00, $FF00FFFF, $FF00FF00, $1100000, $36C0002A
                dc.l $6570080, $11000000, $D700CF, $2C700BB, $38000036
                dc.l $5A21FF, $FF00DF, $5001FF, $EC0008, $DF400E1, $40110F
                dc.l $2300E0, $FF00EF, $AD0001, $A8C0C00, $38020708, $13000000
                dc.l $62020310, $282019FF, $39BF00FC, $5000055, $B41314
                dc.l $DF62DD, $3EB0FD8, $1208006A, $1FF00F0, SRCEN, $17000040
                dc.l $1E0045, $DDB004F, $7F0D0E, $420490, $9720073, $E430045
                dc.l $EEC00FD, $F00203, $B00931, $56005F, $2050043, $919000E
                dc.l $B30050, $8B000100, $22C0, $2C0, $2B80, $280, $15000000
                dc.l $A40, $214, $2003, $30000008, $300, $3000208, allpad
                dc.l $2F00000, $3000100
                dcb.l 2,0
                dc.l DSTA2, $3002500, $30000FF, $FF4500, $FF00FF, DCOMPEN
                dc.l $FF00FF, 0
                dc.l $52000000, $FF00FF, $90000FF, $FF0100, $19FF2500
                dc.l $3005300, $56000000, $B000000, bbutton, $18000000
                dc.l padleft, $D000000, $D00, 0
                dc.l $2000030, DCOMPEN, 0
                dc.l $D000000, $201, $900, 0
                dc.l $20000E0, $9000000, $D00, 0
                dc.l $72C3, $18000546, $1016C8, $9A0274, $C3CF94, $A0B00
                dc.l $FFFFFF00, $EF0027, $740D1D, $58001C, $1DC40027, $DB0
                dc.l $340E3A, $2D0080, $DCF0037, $9C0D80, $7C005F, $406E00
                dc.l $11C, YADD1, $17002541, $500080, $10079, $D00A92
                dc.l $FF0100, 0
                dc.l $6000000, $80A00FF, $F600C0, $21DB, $6000000, $2F3
                dc.l $8FF, $1B0305B8, $450080, $1DF00D4, $30A000D, $3FD0800
                dc.l 0
                dc.l $2A9B, $394000A, $B002580, padright, $2800000, $D000700
                dc.l $B100080, $2900, $2E040004, $F10070, $10000FF, $FF0280
                dc.l $3A38, $A80FFF, $255A00A8, $1010079, $D60D00, 0
                dc.l $5000000, $3900, padleft, $2AAA00FA, bbutton, $30A000E
                dc.l $4B000066, $14004C, $DD1004F, $241DFB, $A90000, $D4F008C
                dc.l $E25009A, $800DFF, $9700A4, $D3E0096, $DC00C0, $8B00FF00
                dc.l $FFFFFF00, $FF00FF00, $FFFFFF00, $FF0039C4, $6C0659
                dc.l $3173E, $1F393B, $8C23FF, $4002FA, $780DF5, $870080
                dc.l $12E60040, $1F5001F, $C10A94, $A0B00, $FFFFFF00, $9003E
                dc.l $480DA7, $6000E8, $1D5F009E, $D2A, $280E90, $630000
                dc.l $DAD0032, $C80D01, $36009A, $808B00, $FF00FFFF, $FF00FF00
                dc.l $55A0020, $16C0006C, $27D00CF, $CF9C0000, $B00FFFF
                dc.l $FF0000E8, $9500BB, $D1400CD, $411DBF, $880040, $DAA000B
                dc.l $E3400C9, $A00DCA, $860061, $D7A00A3, $1100B0, $6E01008E
                dc.l $11B00D3, $F01700, $25C70098, 0
                dc.l $C7008C, $A79009F, $1FF00F8, $7305FF, $B60004, $900000D
                dc.l $300FF, $20000051, $FC0900, $BA00FC, $8FE1BFF, $5BE00BF
                dc.l $60F, $E203FE, $3501000C, $39C0000, $B002500, $FF00FF
                dc.l $2FF00FF, $D010702, $B0000FF, $FF62FF, $FF3A18, $E00FFF
                dc.l $25000000, LFU_A, $DFF, $FC0050, $5FF00F4, $E06F00
                dc.l $4B00, $5F0082, $930DC8, $C40049, $1DF7000A, $400D49
                dc.l $630E20, $3600A0, $DFA00E6, $690D38, $BD008F, $308B00
                dc.l $FF00FFFF, $FF00FF00, $FF00FFFF, $FF00FF00, $39BE00EB
                dc.l $6500047, $17C700BB, $394B00AA, $23660020, $2E80030
                dc.l $DF7008B, $401100, $BA0000, $1E90006, $10A9C, $B00
                dc.l $FFFFFF00, $BC00FC, $BF0D5B, $D8002D, $1D5A00FF, $400D23
                dc.l $FF0E79, $AB00D0, $D8C00C9, $D0DFB, $5D004C, $F08B00
                dc.l $FF00FFFF, $FF00FF00, $1140080, $1AB70096, $27900AF
                dc.l $CF950C00, $FFFFFF00, $430055, $640D8A, $CE00EC, $1DFF005B
                dc.l $DFF, $240E7F, $3F0080, $D0B0058, $6C0DCB, $6F001E
                dc.l $400D7E, $765B7E, $760200, $11C, YADD1, $17000114
                dc.l $8017A8, $3E006B3, $C80080, $1002F, $180A48, $FF0100
                dc.l $3B004E, $7600821, $11B00C3, $220B06FF, $83008C, $1FF0043
                dc.l $6008FD, $20300FF, $1703058C, $16026B, $4C04E1, $3FC08FF
                dc.l $FF00FA, $542902, $990080, $2950C00, $25805340, $2E000001
                dc.l 0
                dc.l $1020000, $3E22, $C00FFF, $25B400F0, $55FF00B3, $E02A8C
                dc.l $16026B, $4C5000, $BA0042, $3C0D3E, $C500F4, $1D3600DD
                dc.l $D9E, $7C0E6A, $AC0080, $D3B00B8, $740D89, $89009B
                dc.l $C00B08, $7F00FF00, $FFFFFF00, $FF00FF00, $FFFFFF00
                dc.l $FF000114, $8036CE, $67065C, $C8525C, $6123A3, $C80000
                dc.l $A, $D00DF9, $A20020, $1106002D, $8001F1, $7F00C1
                dc.l $A950C00, $FFFFFF00, $DA0067, $380D6F, $4900B8, $1D9A00D2
                dc.l $D79, $180EB3, $780000, $D4C008B, $980D4C, $290059
                dc.l $808B00, $FF00FFFF, $FF00FF00, $1100000, $2590018
                dc.l $68400FF, $FF0184, $FF00FF, $9C00000, $27C0077, $CF8B0010
                dc.l $B00FFFF, $FF0000AB, $58007D, $D12000F, $E71D48, $8100C0
                dc.l $D6000AD, $ED40097, $600D55, $A300C7, $D28000A, $4100D0
                dc.l $D800000, $5B800000, $1F000110, $16B4, $7E02B4, $5406C7
                dc.l $980000, $C7, $800903, $890077, $32406FF, $450802
                dc.l $15600DB, $788007F, $18FF0085, $400314, $100000B
                dc.l $6A02A8, $8008FE, $2040000, $150001A5, $5BA00C6, $2BD0078
                dc.l $30A00A5, $4FF00FF, $EA0020, $FF00FF, $D200C0, $2D0100AC
                dc.l $28B, $100B00, $25000F08, padright, $3E800000, $761800E0
                dc.l $FFF2500, $D03, $7200FF, $FF3E0C, $32BA00C6, $2BD0078
                dc.l $50000022, $450055, $DC60006, $EF1D80, $300C0, $D0C00CA
                dc.l $400200, $9009CD, $FB00E0, $3200986, $300CF, $DE60024
                dc.l $BF0050, $B007F00, $FF00FFFF, $FF00FF00, $FF00FFFF
                dc.l $FF00FF00, $1100000, $36C20021, $6550000, $13E6002F
                dc.l $2270087, $394B00DA, $21000000, $650038, $FF00FF
                dc.l $EE0068, $DF70091, $40110D, $660020, $1F500C5, $B8B0010
                dc.l $B00FFFF, $E2821C00, $9100C8, $210619, $6E00F4, $305001B
                dc.l $3C0093, $619006E, $F40305, $F004F, $719006E, $F40305
                dc.l $E300F8, $C00DDA, $A10E1B, $210030, $DCF001B, $230201
                dc.l $330985, $7C00CD, $100293, $8800FF00, $FFFFFF00, $E6D31800
                dc.l $6240685, 0
                dc.l $1850000, $D75, $97CF9F, $B00, $FFFFFF00, $6F0007
                dc.l $FE0DC3, $A1009A, $1D1E0016, $800D28, $1E0EA3, $1A0040
                dc.l $D2A008E, $DA0DF2, $5600FB, $E06E01, $8E011B, $D300F0
                dc.l $170019C0, $2BE, $F20B8C, $ADB003F, $1FF00EB, $9106D0
                dc.l $BE0800, $A800000, $1800001D, $FC0304, $1FF00CD, $A6029D
                dc.l $202201, $1FF059F, $BA0299, $420900, vfb_ysca, 0
                dc.l $20000, $2E1E, $39F0000, $B003500, $FF00FF, $3EFF00FF
                dc.l $87FF3400, 0
                dc.l $3E00, $329F00BA, $2990042, $500000E5, $F400D6, $D770098
                dc.l $A21D55, $980080, $DC70076, $201, $98E, $870040, $3E0095A
                dc.l $EE00E2, $DB00071, $790060, $8B00FF00, $FFFFFF00
                dc.l $FF00FF00, $FFFFFF00, $FF0039AD, $B4065A, $42171C
                dc.l $433930, $B723A7, $7002DB, $200DF4, $2C00E0, $11100047
                dc.l $6001EC, $A60001, $A9F0000, $B00FFFF, $FF0000D5, $A500F2
                dc.l $D6500BB, $260D7E, $610020, $DB9008D, $800DA2, $120E4A
                dc.l $9100E0, $D8B0047, $A60D64, $FF0027, $208B00, $11023BE
                dc.l $E6039C, $2B14006B, $2002642, $EC20FF, $30FF00A1
                dc.l $300308, $3080209, $F90080, $101003F, $E00302, $4650021
                dc.l $9F0803, $309D408, $400ED, $9F0940, $38019FF, $D4FF00E6
                dc.l $B090008, $2090008, $18000001, $44004A, $C06001B
                dc.l $E00A9, $D0300A8, $900D0E, $A20040, $D130083, $E110012
                dc.l $A00D28, $50009, $2030033, $92E00B8, $810030, $10300B3
                dc.l $88000114, $800101, $4E00B0, $1010024, $8602D5, $FF00FF
                dc.l $1C8008F, $FF011D, $80A70, $6F00FF, $5D1A00C0, $706C65
                dc.l $21009C, $40704, $1010100, $15400080, $280, $6ED
                dc.l $9FDBFF, $14FF00F9, $4002C0, $730009F, $FFDB00, $2B0023
                dc.l $B80D37, $C500A8, $D020048, $200201, $600806, $1E002A
                dc.l $E280038, $E230031, $E1E00A2, $A80C08, $26002F, $AB0080
                dc.l $DC70095, $3E000900, $9000480, $3C70095, $102031C
                dc.l $300F8, $102010F, $1300FF00, $FFFFFF00, $E51B00D3
                dc.l $F01700, $FF00FFFF, $FF00E0FF, $FF01DE, $1010078
                dc.l $C01700, $FF00FFFF, $FF00E51B, $C300F0, $17000114
                dc.l $803627, $7E060B, $A211FF, $FF00E4, $690258, $7F0279
                dc.l $B027F, $2302DE, $8F1900, $130600DE, $21FF00FE, $40020
                dc.l $101002B, $100CFF, $EE00ED, $C01101, $DE0080, $10C0085
                dc.l $9650021, $9C0004, $70D0106, $1003901, $7021201, $41080
                dc.l $2C01, $42340, $3010E01, $41340, $38019FF, $38FF00FD
                dc.l $9C05FF, $F90080, $1313009B, $E6A00D2, $2D360010
                dc.l $21FF00FF, $CE0080, $FF00FF, $DC0DFF, $F400D4, $12090008
                dc.l $2090008, $1800007A, $3100F8, $619006F, $C0305, $38005C
                dc.l $680619, $6F000C, $305000D, $C10090, $2020060, $80600BD
                dc.l $DA0E52, $780E08, $290EC4, $D10068, $D34006D, $F0080
                dc.l $711B00C3, $F01700, $1140080, $229600BE, $8001BE
                dc.l $900080, $80400E5, $7F00FF, $17F00FF, $63600D8, $810010F
                dcb.l 2,$FF00FF
                dc.l $F6039B, $3F0288, $7F14FF, $D3000C, $1FF00FF, $E401FF
                dc.l $C000C0, $1FF00EC, $402201, $1A505A3, $9802D1, $46031A
                dc.l $4C00FF, $FF00FF, $FF0B08, $27030103, $5F00FF, $650021
                dc.l $9C0004, $70C0105, $1002540, $3800C80, $3910, $820080
                dc.l $5200320, $2A080004, $2020004, $51FF253A, $3E028F
                dc.l $100904, $E500BF, $FF38FF, $FE0060, $A2B0080, $2A930060
                dc.l $2BE0038, $30800FA, $FF00FF, $FF00E2, $8FF00FF, $FF001E
                dc.l $3B00007D, $8D007E, $D3C00BA, $1A0D4A, $410060, $2020906
                dc.l $C00036, $800D55, $9E0E0A, $EA0040, $DC70037, $5A0D37
                dc.l $6A0053, $E0711B, $C300F0, $17000110, $223, $D80215
                dc.l $500102, $7C007F, $102005B, $1F01FF, $F4006C, $A77008F
                dc.l $CF9D0009, $B00FFFF, $FF000036, $39006D, $D46002F
                dc.l $B71D25, $F500C0, $D32009D, $E2C0049, $600D26, $8D0097
                dc.l $D30000E, $3000D0, $D760059, $5B760059, $1F00FF00
                dc.l $FFFFFF00, $FF00FF00, $FFFFFF00, $FF00FF00, $FFFFFF00
                dc.l $FF000110, $3610, $DC05FF, $C300EA, $13DE00EA, $23D00DF
                dc.l $3901005F, $22FD00D6, $800100, $760000, $E3D00E0
                dc.l $12280000, $110009E, $800A9D, $90B00, $FFFFFF00, $850047
                dc.l $AD076E, $F40446, $C60077, $76E00F4, $14C500A5, $C00D5C
                dc.l $DD0E11, $410060, $DCC00BC, $570D3E, $4B0094, $D08B00
                dc.l $1100000, $22810028, $2EB006B, $805, $2E001F, $14032400
                dc.l $200F4, $5000007, $DC0100, $33005C, $24FF06D8, $8001FC
                dc.l $45439D, $90B00, $FFFFFF00, $8800A3, $330D4B, $240029
                dc.l $1DC80002, $400D60, $30E14, $200A0, $DCF0022, $490D41
                dc.l $4800D9, $308B00, $545000C, $800134, $B00080, cvy2
                dc.l $3E1003F, $2E00024, $A6E00EF, $CF9E0010, $B00FFFF
                dc.l $FF00004E, $6500C8, $D65009E, $D81D36, $F60000, $D490048
                dc.l $E40001F, $D37, $D100D8, $D450093, $EC0080, $D4D00C8
                dc.l $3E040904, $9040400, $34D00C8, $1000001, $8E011B
                dc.l $D300F0, $1000100, $1300FF00, $FFFFFF00, $FF00FF00
                dc.l $FFFFFF00, $FF00FF00, $FFFFFF00, $FF003904, $206ED
                dc.l $5813E7, $AB0288, $3A0A0020, $22FF0020, $A0020F, $300DEF
                dc.l $560000, $11060068, $80010B, $F800C0, $A9E0010, $B00FFFF
                dc.l $E2200320, $1800009D, $740008, $D660035, $981DD6
                dc.l $A60000, $D730088, $E250017, $D4A, $8E0028, $20600F3
                dc.l $9E20070, $D00080, $1090023, $8800257C, $1E02A3, $360080
                dc.l $8030042, $7F39FF, $C300C0, $5FF0097, $8001FF, $C30000
                dc.l $220007AB, $B70000, $27B01FF, $FF00ED, $D73908, $99039E
                dc.l $100B00, $24010004, $2100004, $C483009C, $FFF2638
                dc.l $C603005F, $FF0F00, $A000CF, $8E0D6A, $93004A, $1DD90180
                dc.l $D7600AE, $E2700D8, $400DE0, $66008A, $D5600CE, $9400E0
                dc.l $8B000114, $800308, $2A0, SRCEN, $E3000F, $10100E2, $7F0100
                dc.l $2100A8, $A7500F3, $CF850000, $B00FFFF, $FF000078
                dc.l $C10000, $D9C00B3, $1D54, $C00E71, $E62, $E00E56
                dc.l $130000, $D6B004A, screen1, $DD000D6, $1A820046, $3B820046
                dc.l $2D000D6, $1F000114, $8023AE, $2A4009A, $2B000300
                dc.l $26160020, $20FF314C, $700A06, $4B0040, $1050018
                dc.l $4020465, $210085, $8030309, $D4010004, $170064, $9010080
                dc.l $102, padright, $17FFD4FF, $F40B09, $80209, $81800
                dc.l $2B00E0, $790D38, $BB001B, $E4300B0, $C06001E, $AE00C0
                dc.l $D2800E9, $E2300CB, $E00D1F, $29007B, $D2600D7, $B90090
                dc.l $D007D00, $FF00FFFF, $FF00FF00, $FF00FFFF, $FF00FF00
                dc.l $1140080, $229900F0, $2AE00DE, $E000000, $5000000
                dc.l $1100
                dc.l 0
                dc.l optionbu
                dcb.l 3,$280
                dc.l $1914, $13000000, $CFF1400, vfb_ysca, $102, 0
                dc.l $C000002, $150055, $52600F0, $3080308, $2040014, $FF
                dc.l $FF007A, $302, $6850000, $7030100, $1093900, $7001200
                dc.l $10FF, $FF2C00, $2300, $3000E00, $604, $4002F, $900
                dc.l $30019FF, $38000000, $500, 0
                dc.l $13000000, $E000000, $2D000000, $21000000
                dcb.l 2,0
                dc.l $D00, 0
                dc.l $5FF00F4, $B000000, bbutton, $18000022, $E7006F
                dc.l $61E0075, $400300, $2D0010, $1D061E, $750040, $3000000
                dc.l $3F0070, $20100B0, $918005F, $400D20, $7F0E1C, $6F0020
                dc.l $D1800C0, $BD0200, $C3091E, $DA0094, $F00100, $F30A00
                dc.l $64D31800, $1140080, $22C700F8, $190, $760900, 0
                dc.l UPDA1F, $600, $808, $1080000
                dcb.l 2,0
                dc.l $3000000, bbutton, $14000000, UPDA1F, $04, LFU_A
                dc.l UPDA1F, $40030A, $EB26C0, $2C0, UPDA1F, $08
                dcb.l 2,0
                dc.l $FF, $FD006B, $C00256, $600300, $113300E0, $9FF00F9
                dc.l $3C0080, $10E0079, $4020100, 0
                dc.l $2850000, $7030100, $1092400, $10080, 0
                dc.l $20080, $AFF, $FF3900, $FF00FF, $5000300, $2100004
                dc.l $26000000, bbutton, $26080004, $6500A0, $16FF00FF
                dc.l $FFF25C0, $7802BF, $A000000, 0
                dc.l $38000000, $A00, $2A00, optionbu, $300
                dcb.l 2,0
                dc.l DSTA2, 0
                dc.l $10FF, $F61700, 0
                dc.l $F00001E, $600AC, $D2600B8, $440D01, $4200F0, $2000A14
                dc.l $F10000, $D1B00EC, $E18006E, $800D15, $4400C4, $D1A0082
                dc.l $C200C0, $D0064D3, $18000110, $380, $21F0046, $2850000
                dc.l $185, 0
                dc.l LFU_A, $A79009F, $6C016290, $C001500, $FF00FF
                dc.l $2FF00FF, $6FF00FF, $DBFF1400, 0
                dc.l $2000700, 0
                dc.l $DB000031, $950073, $D400026, $E90D08, $AC0090, $2000030
                dc.l $8000022, $B20040, $D2E0043, $E28007A, $A00D23, $3D0009
                dc.l $C00002B, $EC007D, $300D8C, $9E5B8C, $9E0200, $11C
                dc.l YADD1, $17000110, $6DFF, $C10080, $628C0090, $A070070
                dc.l $10C, $4700E0, $A900C09, $71200080, $622B, $8427FF
                dc.l $E2380338, $18000095, $930000, $20400E1, $9E8005B
                dc.l $800209, $131941, $C40000, $D5700B0, $E4C00BA, $D42
                dc.l $CB0010, $D530041, $8B0000, $8B00FF00, $FFFFFF00
                dc.l $FF00FF00, $FFFFFF00, $FF000110, $6E2A, $26102, $850000
                dc.l $A010063, $A00000, $600DF, $800A90, $C097140, padright
                dc.l $62D50024, $90100C1, $680102, $19FFE104, $3C01FF
                dc.l $FF0092, $1800009C, $940016, $2070031, $9C90050, $120209
                dc.l $1A3B0074, $800D4F, $460E45, $5D0040, $D3C0062, $520D4B
                dc.l $440066, $608B00, $1100000, $22C40033, $800194, $830000
                dc.l $550600C5, $52480040, $B9300A0, $10B000A, $400A90
                dc.l $C09E101, padright, $1020080, $17FF, $E0FF00F9, $44020E
                dc.l $961800, $800003, $530207, $41098B, $D20089, $2060033
                dc.l $19380006, $400D4A, $B30E41, $5C00A0, $D3800E6, $590D46
                dc.l $EC0094, $308B00, $1140080, $24800B0, $20F0050, $165500D3
                dc.l $5FD00C00, $6F00FFFF, $FF000048, $D200EE, $D5E005F
                dc.l $6A0D0C, $C200A0, $D33000A, $800D44, $E0E3B, $8C0040
                dc.l $D3300D6, $AA0D40, $9D008A, $E08B00, $1140080, $53140003
                dc.l $18000006, $8E0E4D, $E85296, $600A0C, $DD0040, $1060069
                dc.l $501, $D900FF, $F09FFFF, $E2200320, $18000091, $4D0055
                dc.l $2010011, $9BC009E, $EF0201, $631966, $300C0, $D880005
                dc.l $E770004, $600D67, $9B00CF, $D810025, $3F0050, $8B00FF00
                dc.l $FFFFFF00, $FF00FF00, $FFFFFF00, $FF000114, $805348
                dc.l $D70241, $67153C, $60EBC, $38522F, $C00A09, $620000
                dc.l $FF00FD, $430020, $50100EB, $FF0F09, $FFFFE228, $3861800
                dc.l $880054, $4B0201, $1109B0, $F300F1, $2011A5F, $B40040
                dc.l $D7F009B, $E6F00A7, $A00D61, $330011, $D790028, $1A00B0
                dc.l $8B000114, $8022D8, $C00000, $1960034, $802A31, $87293E
                dc.l $FE524F, $500308, $308033C, $80010E, $790000, $501005D
                dc.l $FF0F09, $26DD00AC, $2AD00B8, $54005549, $9C0900
                dc.l $FF00FF, $10000FF, $FF17FF, $E0000000, optionbu, WID8
                dc.l $830073, $880201, $1109AA, $9C0018, $2010063, $195C0046
                dc.l $D7B, $80E6B, $A70000, $D5D00B7, $180D74, $D00048
                dc.l $808B00, $56700C4, $2000038, $102000E, $4F00FF, $20019
                dc.l $CF00FF, $D68005B, $CF8B0006, $B00FFFF, $FF000054
                dc.l $C00E5, $D6C00F8, $9F0D0E, $BB00F0, $D3A00EF, $C00D4E
                dc.l $950E44, $C20060, $D3B00DB, $7F0D4A, $9C0058, $508B00
                dc.l $4B020119, $5B00FF, $213E00BE, $2E010103, $5AE0015
                dc.l $2FB0038, $802598, $A00A08, $6800E0, $10D0001, $6000000
                dc.l $28B, $60B09, $A9010302, $9F0080, $4FFFA9BF, $4002C1
                dc.l $385000, $9C0087, $4C0DCB, $380024, $1D6D00E9, $D92
                dc.l $8C0E80, $3A0080, $D6F00A0, $A40D8B, $24000C, $C08B00
                dc.l $FF00FFFF, $FF00FF00, $FF00FFFF, $FF00FF00, $2401001B
                dc.l $390080, $1000D, $EB2002, $12F00C7, $FF06EC, $BB193D
                dc.l $F005FF, $EE0066, $67F00EC, $1E010103, $5F000A8, $2B500CE
                dc.l $26E10070, $A020016, $E001FC, $FE00E0, $5000000, $28B
                dc.l $60B09, $25400380, $4F020080, $83FF, $25BE0050, $2C000C0
                dc.l $4E0200A0, $84000093, $8E0042, $DBF008D, $261D67
                dc.l $990080, $D8A0022, $E7800DD, $C00D69, $3700E6, $D830026
                dc.l $E80020, $8B002576, $7102FD, $4D0000, $1F020124, $8F00FF
                dc.l $51010103, $31560030, $16000000, $28B, $60B09, $FFFFFF00
                dc.l $8E00AD, $7F0DB9, $35004D, $1D64002B, $400D85, $8F0E74
                dc.l $DD0020, $D6500BB, $ED0D7E, $CF0015, $F08B00, $55A0080
                dc.l $2090068, $22500EF, $22A003F, $2010042, $A6900BF
                dc.l $CF850000, $B001520, padright, $E7FFFF00, $70030, $A20D09
                dc.l $60046, $D010038, $600D04, $E10080, $D060082, $E0500B1
                dc.l $C00C08, $400F5, $60D06, $2D00CE, $200D82, $881A99
                dc.l $FA3B99, $FA0282, $880201, $8E011B, $E300F0, $17004B08
                dc.l $24FF00F2, $F00E95, $A81E00, $1FF05C1, $420080, $1BB0008
                dc.l $2596, $600A09, $4D0060, $10800CE, $200A85, $B09
                dc.l $FFFFE238, $3381800, $55007E, $C50206, $8109D1, $84004F
                dc.l $2090053, $198C002B, $C00DBA, $E50EA3, $880060, $D1B00CE
                dc.l $8F0206, $30963, $55000D, $500107, $938800, $FF00FFFF
                dc.l $FF00FF00, $FF00FFFF, $FF00FF00, $240000A2, $9D0100
                dc.l $D70073, $200524FF, $ED0088, $500002A, $AC06C8, $B81E00
                dc.l $1FF3001, $5600D0, $A05003F, $1000004, $9300A0, $A850000
                dc.l $B09FFFF, $E23C0392, $180000C8, $7002B, $20B00B1
                dc.l $95F0063, $F10205, $931985, $DC0040, $DB2007B, $E9C002B
                dc.l $A00D87, $F300B1, $DA90075, $1B08B00, $257100A0, $2F5003A
                dc.l $20085500, $1FF3124, $F00310, $3101385, $B09, $FFFFFF00
                dc.l $B900B4, $680DF1, $2800B8, $1D82006E, $DAD, $E80E98
                dc.l $2B0000, $D840077, $B80DA5, $1E0016, $808B00, $4000000
                dc.l UPDA1F, $F60030, $21F00BF, $22602FF, $FF00F6, $2BF00E2
                dc.l $3480259, $57CF86, $C000D40, 0
                dc.l $1400000, $EBFF, $C02001F, $BF00FF, $20026, $3F00FF
                dc.l $10D00E0, $E800009B, $480071, $DC90099, $830D1B, $420030
                dc.l $D6D0008, $C00D91, $610E7F, $3400E0, $D6E00BC, $E30D8A
                dc.l $80029, $100D64, $2C5B64, $2C0217, $D81B00, $25BD00D8
                dc.l $2BB002C, $DFF00FB, $F30602, $902EC7, $8036B4, $180000
                dc.l $1ED00B1, $269D00F0, $A0800C1, $10C, $310060, $A860C09
                dc.l $FFFFFF00, $10084, $B80DC8, $220098, $1DA00002, $DD5
                dc.l $580EBA, $AD0000, $DBA00CF, $E80D2B, $AB00BD, $808B00
                dc.l $1140080, $22E80039, $800194, $4D0E0E, $AC067F, $FF1300
                dc.l $30015FF, $F200E4, $E07006C, $20FF3502, $B00387, $800B02
                dc.l $4650021, $860803, $3092501, padright, $1020085, $144520
                dc.l $240030, $C100004, $7DFF25C0, $3C0D500, $1D0056, $B70D25
                dc.l $D30075, $D040042, $700202, $200806, $140075, $400D1B
                dc.l $470E17, $DE0020, $D1400C7, $150D19, $E60019, $700D64
                dc.l $2C1A99, $FA3B99, $FA0264, $2C0000, $20100, $11B00E3
                dc.l $F01700, $FF00FFFF, $FF00FF00, $25F5007C, $196, $A24500
                dc.l $2400C8, $5FF00CA, $1C5902, $80090, $A0A002E, $1FF00F8
                dc.l $810B86, $C09FFFF, $FF0000FD, $4100AE, $DF200D2, $FA1D99
                dc.l $B20080, $DCC00EE, $EB30050, $400D9C, $19004A, $DC20092
                dc.l $B800E0, $8B002595, $A0288, $4AAA38, $A01B86, $C09FFFF
                dc.l $FF0000D5, $E9000B, $D1500D6, $311D96, $440040, $DC8005B
                dc.l $EAF004F, $A00D98, $9D0051, $DBE003A, $E600B0, $8B000110
                dc.l $7F60030, $102001F, $BF00FF, $20026, $3F00FF, $FF00F0
                dc.l $6A02BF, $E20348, $2590057, $FF5D1A, $D00070, $6C650021
                dc.l $810804, $1010100, $D400000, $140, 0
                dc.l $1200080, $E7FF, $C02001F, $BF00FF, $20026, $3F00FF
                dc.l $10D00E0, $E80000EE, $7E00F9, $D3500CE, $9B0D29, $E300B0
                dc.l $DA7008E, $C00DDF, $690EC3, $7B00E0, $C0800AA, $2C00FB
                dc.l $DD4001E, $A10090, $D450015, $1A8A00CA, $3B8A00CA
                dc.l $2450015, $23700AA, $11B00E3, $F01700, $11033A9, $F04A9B
                dc.l $69030465, $210081, $8020300, $3504C9FF, $FF000054
                dc.l $3C00E2, $D6D0037, $60D0B, $AA0080, $3E00378, $4C0303
                dc.l $3B0011, $800D4E, $C20E44, $E900C0, $D3B00FD, $C60D4A
                dc.l $C70012, $200D64, $2C1A99, $FA3B99, $FA0264, $2C039C
                dc.l $11B00E3, $F01700, $1102393, $CF02C7, $38167F, $FF131B
                dc.l $F60200, $15FF0092, $F0056FF, $FC00F8, $30801FF, $FE0052
                dc.l $A000FF, $FF0008, $200302, $4650021, $810803, $3092501
                dc.l $30200E4, $A44510, $E400004, $9900D4, $5D010080, $102
                dc.l padright, $17FF25C0, $3C0B82A, $1FF00FE, $B21800, $450036
                dc.l $5B0C08, $5900AD, $210C07, $60043, $E00204, $900806
                dc.l $300080, $400D40, $AB0E38, $9500A0, $D310042, $410D3D
                dc.l $66005B, $B00D64, $2C1A99, $FA3B99, $FA0264, $2C0202
                dc.l $19011B, $E300F0, $1700FF00, $FFFFFF00, $E51B00C3
                dc.l $F01700, $FF00FFFF, $FF008164, $2C1A99, $FA3B99, $FA0264
                dc.l $2C0202, $19011B, $E300F0, $1700FF00, $FFFFFF00, $8164002C
                dc.l $1A9900FA, $3B9900FA, $264002C, $2020019, $11B00E3
                dc.l $F01700, $14000000, $E4DB80, $20B00, $FFFFFF00, $FF0041
                dc.l $200D4B, $990060, $D2C00D6, $DB3, $580000, $DEF0020
                dc.l $ED1003C, $DB6, $250060, $DE3000A, $520000, $D660009
                dc.l $1A4700A3, $3B4700A3, $2660009, $20800E5, $1B0034FF
                dc.l $AA0080, $6CA54E80, $20B00, $FFFFFF00, $6400FF, $90D83
                dc.l $100CB, $D19006A, $A00D46, $DA00C0, $D5E0079, $E5200A9
                dc.l $E00D47, $F6002B, $D5900B2, $C20090, $8B0025AC, $86029A
                dc.l $3E4500, $5400FC, $30A53DFF, $5A0040, $1FC00EF, $A80
                dc.l $20B09, $FFFFFF00, $5500F8, $820D6F, $7700E6, $1D3C0049
                dc.l $800D50, $620E46, $5500C0, $D3D003A, $A60D4C, $52000C
                dc.l $208B00, $FF00FFFF, $FF00FF00, $FF00FFFF, $FF00FF00
                dc.l $FF00FFFF, $FF00FF00, $1140080, $12270034, $DB92001E
                dc.l $B00FFFF, $FF000012, $480078, $D640057, $E80D30, $2E0080
                dc.l $DC000BA, $E0000F8, $EE000D9, $EC300BC, $E80DF3, $FB0077
                dc.l $800DB2, $AD1A46, $123B46, $1202B2, $AD0225, $641B00
                dc.l $1140080, $31FE001E, $4B6A0028, $20FF4E92, $1E0B00
                dc.l $FFFFFF00, $780006, $610D9B, $C00053, $D290007, $E54003C
                dc.l $E700051, $E620046, $E55008D, $B30D6A, $A300E8, $108B00
                dc.l $1140080, $22E500FF, $800199, $310080, $210700FF
                dc.l $FF0208, $30500BF, $197C31FF, $35FF0076, $1FF00FE
                dc.l $6C0100, $2005D, $2FD006E, $800A92, $1E0B09, $82770048
                dc.l $7BFFE2C8, $2FF003E, $18000068, $FF00DA, $D880036
                dc.l $6E1D49, $AB0E62, $3A0E55, $F20E4A, $D2002E, $D5D0043
                dc.l $3100A0, $8B00FF00, $FFFFFF00, $FF00FF00, $FFFFFF00
                dc.l $FF00FF00, $FFFFFF00, $FF0014FF, $D80038, $DB86000C
                dc.l $B00FFFF, $FF000026, $5001A, $D7E0002, $2E0D33, $A600E0
                dc.l $DCE009B, $800D13, $7A0EF1, $A00C0, $DD100D5, $EE0D05
                dc.l $8E0005, $A00D14, $B01A83, $953B83, $950214, $B00202
                dc.l $B61B00, $35E54B9B, $2003, $4E86000C, $B00FFFF, $FF00008B
                dc.l $C30003, $DB5006A, $990D39, $390060, $D62001E, $400D82
                dc.l $D30E72, $7800A0, $D6300A6, $B90D7C, $360076, $308B00
                dc.l $256F0084, $1C2, $650E08, $90067A, $FC0805, $2CB0300
                dc.l $31B00F6, $18FF00EA, $302E01, $1030E0C, $8225FE, $3FB008C
                dc.l $1FF00FC, $CF0080, $1FE00CE, $B86000C, $B09722C, $500E20
                dc.l $1C5280, $27FF, $D5080C2A, $2FE00B2, $1800007C, $BC007C
                dc.l $DA100E0, $B41D57, $8D0000, $D7400BC, $E660024, $800D58
                dc.l $EB0034, $D6E00D5, $BF00C0, $8B00FF00, $FFFFFF00
                dc.l $FF00FF00, $FFFFFF00, $FF00FF00, $FFFFFF00, $FF000401
                dc.l $460040, $101000C, $980101, $850000, SRCEN, $850000
                dcb.l 2,0
                dc.l $2C00000, $3000275, $36C01, $62840000, $B000D00, $FF00FF
                dc.l $10000FF, $FF0100, $FF00FF, $E7FF0C00
                dcb.l 3,0
                dc.l UPDA1F, $E800, $1400C6, $2F0D1A, $B0005D, $D03009B
                dc.l $D00D0E, $6F0040, $E3F0E10, $D70020, $C00000E, $A800FD
                dc.l $D120046, height, $DD70030, $1AAE00CC, $3BAE00CC, $2D70030
                dc.l $20E0009, $1B000100, $3100, 0
                dc.l $4A146900, $4000000, 0
                dc.l $7000300, $3500C9FF, $FF000000, padleft, $D000000
                dc.l $D00, 0
                dc.l $3300355, $380300, 0
                dc.l $D00, $E00, 0
                dc.l $D000000, $D00, 0
                dc.l $D51, $3C1A85, $2B3B85, $2B0251, $3C0204, $711B00
                dc.l $24010001, $6102DF, $560000, $45F3005C, $30A53C00
                dc.l $200E3, $600000, dstoffy, $400A84, $B09, $FFFFFF00, $95001E
                dc.l $290DC1, $95002B, $1D6800B2, $C00D8B, $990E7A, $2500E0
                dc.l $D6A0055, $8B0D84, $8A00F4, $908B00, $FF00FFFF, $FF00FF00
                dc.l $FF00FFFF, $FF00FF00, $FF00FFFF, $FF00FF00, $1100000
                dc.l $2430058, $20D0058, $28400FF, $FF0184, $FF00FF, $DE8C0C00
                dc.l $FFFFFF00, $1A00BA, $140D22, $6D00FC, $D0400A7, $C00D12
                dc.l $9F0000, $D1800D4, $E1500B9, $800D12, $E9007C, $D170092
                dc.l $C90040, $6E0D00A3, $1B00FF00, $FFFFFF00, $FF000110
                dc.l $2100, $600001, $80016A, $420080, $44000013, $BA0E01
                dc.l $7720FF, $3CFF00FC, $2A0000, $FF00FA, $B20000, $A8C0C09
                dc.l $FFFFFF00, $9B0012, $E0DC9, $5200CA, $1D6C00E2, $800D91
                dc.l $2E0E7F, $80040, $D6E0096, $A0D89, $D700BC, $E08B00
                dc.l $FF00FFFF, $FF00FF00, $FF00FFFF, $FF00FF00, $FF00FFFF
                dc.l $FF00FF00, $1140080, LFU_A, UPDA1F, $DF00F8, $E78F0002
                dc.l $B00FFFF, $FF000055, $15005D, $D6E0050, $870D0E, $EA0070
                dc.l $D3B00A9, $C00D4F, $8D0E45, $9B0060, $D3C0098, $670D4B
                dc.l $8700CF, $D00D64, $261A72, $4E3B72, $4E0264, $260202
                dc.l $F61B00, $1140080, $32E6009F, $FF0101, $FC060A, $9831FF
                dc.l $E900AD, $200910, $80B00EB, $11015FF, $29010000, $C01105
                dc.l $D80209, $80403, $4650021, $8F0002, $7050302, $750100ED
                dc.l $B45502, $7A0084, $11100080, $1BFF, $74FF00F0, $105602
                dc.l $101209, $81C00, $1C00AB, $60D24, $F40032, $D0400FF
                dc.l $200775, $400413, $FC0080, $D1A00A6, $E170051, $400D14
                dc.l $4C0072, $D19004D, $3C0060, $D9600A9, $1A640048, $3B640048
                dc.l $29600A9, $227005E, $1B000114, $803DFF, padright, $10ABF
                dc.l $7000000, $18FF00F3, $8A0E29, $3C1E00, $12FF00FF
                dc.l $FF00FE, $8FF00FF, $FA0059, $11280010, $3820394, $33203B8
                dc.l $B8F0002, $B098000, $5240, $40030, $1827FF, $D4FF00FB
                dc.l $C3203B8, $180000D5, $6D0057, $D150035, $551D95, $ED0040
                dc.l $DC700E7, $EAE00EA, $200D98, $4400F5, $DBD00CC, $C30070
                dc.l $8B00FF00, $FFFFFF00, $FF00FF00, $FFFFFF00, $FF00FF00
                dc.l $FFFFFF00, $FF00F28D, $B00, $FFFFFF00, $8900F6, $530DB3
                dc.l $130089, $D180036, $900D60, $DA0040, $D810023, $E7000FE
                dc.l $A00D62, $5D00A9, $D7A009C, $4B0030, $D77005D, $1A000042
                dc.l $3B000042, $277005D, $2010012, $1B0025A6, $E10080
                dc.l $1920079, $A000000, UPDA1F, $600, $3100, $04, $97A, $6C0704
                dc.l UPDA1F, $15032902, $150055, $110800F0, $20800F0, $B8D0000
                dc.l $7020300, $25400380, $FD0004, $490000FF, $FF5500
                dc.l $FF00FF, $110000FF, $FF1BFF, $25C003C0, $4A000000
                dc.l $5600, $1200, $1C00, $B0063, $920D0E, $7C0016, $D0100F5
                dc.l $600D07, $D50E0A, $720E09, $2300C0, $D0700F4, $D60D09
                dc.l $EA00DD, $200D2A, $6A1A15, $183B15, $18022A, $6A0207
                dc.l $BF1B00, $256F0075, $2303, $24000001, $620E0B, $F41E01
                dc.l $103317C, $C00388, $39803DE, $E002BE, $B8D0000, $B09FFFF
                dc.l $E23A03BE, $1800000A, $4E004D, $D5900F8, $571DBB
                dc.l $1D00C0, $DF9007D, $EDA004D, $600DBE, $A0037, $DEC00E1
                dc.l $3E00D0, $8B00FF00, $FFFFFF00, $FF00FF00, $FFFFFF00
                dc.l $FF00FF00, $FFFFFF00, $FF000110, $101, $4500B0, $101000D
                dc.l $58E78C, $A0B00, $FFFFFF00, $B6001D, $2A0DEC, $7D005E
                dc.l $D1F00F9, $E00D7F, $E70080, $DAA008A, $E950038, $C00D81
                dc.l $E7001E, $DA100EB, $A600A0, $D0D00B4, $1A2000BB, $3B2000BB
                dc.l $20D00B4, $21B0024, $1B000110, $22AA, $FD0000, $1FC0021
                dc.l $563D002C, $20C34E8C, $A0B00, $2AFF00FF, $D3FFFF00
                dc.l $190065, $E50D20, $B3009F, $D04006B, $F00D11, $AF00C0
                dc.l $D170095, $E1400A2, $600D11, $F6007F, $D160063, $E80050
                dc.l $D000000, $1A3C00D2, $3B3C00D2, bbutton, $30F1B00
                dc.l $1100000, $2101001F, $FD49FF, $F20052, $30C330FF
                dc.l $FB0000, $9000004, $390F8C, $A0B09, $FFFFFF00, $360075
                dc.l $240D93, $62002C, $1DDA002B, $D22, $E40EFE, $870080
                dc.l $DDD0093, $AC0D14, $30009A, $408B00, $FF00FFFF, $FF00FF00
                dc.l $FF00FFFF, $FF00FF00, $FF00FFFF, $FF00FF00, $1100201
                dc.l $4E0092, $101000E, $C00FFC, $378026D, $7700FF, $5D000065
                dc.l $400B01, $60650021, $8E0001, $7040101, $1001A80, $280
                dc.l $DFFF, $19EF0028, $2B900DF, $E0000037, $CE00A8, $D950023
                dc.l $780D36, $C70080, $DDB001E, $E240028, $EFF00A3, $EDE008A
                dc.l $780D15, $64003A, $800D8F, $641A70, $A92200, $9000900
                dc.l $47000A9, $28F0064, $1020004, $49011C, $40202, $10F1300
                dc.l $11023A8, $820080, $1860058, $A620077, $FF1E88, $7F0288
                dc.l $7F155B, $3201FF, $FA0003, $8004FF, $FF0020, $24FF4703
                dc.l $4650021, $8E0001, $70C0105, $1002540, $5200A0, $18000AB
                dc.l $5C0940, $4000FC, $391000D0, $7C0140, $89FF25C0, $3C00BC4
                dc.l $FF00FF, $3E148800, $5C0024, $EC0D77, $7F0004, $D100028
                dc.l $400D40, $A10E56, $2C0E4B, $660080, $D4100A3, $840D51
                dc.l $D10026, $C06E50, $F1B00, $1102201, $110025, $800191
                dc.l $ED0080, $15480048, $2DFF0027, $BE0001, $D0700E8
                dc.l $20FF4702, $4650021, $8E0001, $7030309, $254000D2
                dc.l $470180, $DF007F, $45200080, $EAC, $707BFF, $24010007
                dc.l $AC029B, $FA45FF, $2018B00, $510046, $120331, $95F0042
                dc.l $D60D03, $3900B0, $2020010, $8060033, $850080, $D4400B2
                dc.l $E3C001B, $C00D34, $530096, $C080041, $390041, $200B07
                dc.l $6259002C, $1B00FF00, $FFFFFF00, $E259002C, $1B000110
                dc.l $231A00A0, $25F0040, $2BCD0017, $89000093, $8000FF
                dc.l allpad, $10865, $21008E, $1070B, $1040103, $552000D2
                dc.l $14A7FF, $569B003F, $A700002B, $2700FC, $D3700CB
                dc.l $340D07, $8B0040, $D1E002D, $E28003C, $E230034, $800D1E
                dc.l $A500B4, $D260033, $7700C0, $D8F0064, $1A7000A9, $3B7000A9
                dc.l $28F0064, $2030035, $28300E8, $1700FF00, $FFFFFF00
                dc.l $E0FF00FF, $1DE0101, $7800C0, $17000548, $38020A
                dc.l $161649, $5BCF98, $B00, $FFFFFF00, $2E0093, $800D89
                dc.l $220080, $D350028, $DD4, $A00E1B, $800EF8, $100ED7
                dc.l $F20080, $D0D002C, $380000, $6E000000, $1B0025BD
                dc.l $B40000, $1C0000C, $A15001F, $39FF00F6, $3A0100, $2003E
                dc.l $2105, $4F00FF, $7014902, $F500FF, $2980000, $B002500
                dc.l $3000B00, $C9FFFF00, $5200E9, $C40D6B, $7E000C, $D0E0088
                dc.l $C00D3A, $230E4D, $840E43, $D30E3B, $B008C, $D490099
                dc.l $240040, $8B000100, $220000C0, 0
                dc.l $1C00000, $1500, $2D00, 0
                dc.l $D14, $2003, $47000400, 0
                dc.l $700, $3002500, $FF00FF, $10000FF, $FF4500, $FF00FF
                dc.l $EFF00FF, $7BFF2400, 0
                dc.l bbutton, $45000200, $8B000000, padleft, $3110900
                dc.l 0
                dc.l $D000000, optionbu, $300800, 0
                dc.l $D00, $E00, 0
                dc.l $D000000, $C00
                dcb.l 2,0
                dc.l $B00018F, $641A70, $A93B70, $A9028F, $640201, $4D0283
                dc.l $E81700, $FF00FFFF, $FF00FF00, $568A009B, $8AEE0000
                dc.l $E980000, $B03FFFF, $5691008F, $A7000021, $EC00D4
                dc.l $D2B00CA, $3C0D05, $EB00C0, $D1700AF, $E1F0094, $E310085
                dc.l $500202, $400918, $D00BC, $D1D00FB, $750040, $8B00FF00
                dc.l $FFFFFF00, $FF00054A, $18021A, $3602E4, $BF00FF, $1E9002F
                dc.l $FF0600, $300027C, $B75E1A, $700068, $B006292, $100B00
                dc.l $1AFF00FF, $2FF00FF, $DFFF1900, optionbu, $E000, $1200C3
                dc.l $1C0D18, $120094, $D030041, $400D0D, $50E11, $5C0E0F
                dc.l $300080, $D0D0039, $140D10, $7B0069, $C08B00, $25C00000
                dc.l $3000A00, 0
                dc.l $1E800000, $2800000, $14000000, optionbu, $40500, padleft
                dc.l $71F00FF, $2200E00, 0
                dc.l $70314FF, $FE00E4, $1FF00FF, $B80080, $18FF00F7, $9C0040
                dc.l $106009D, $8002FC, $3FC0400, LFU_A, $292, $10070A
                dc.l $1040102, $26FF00FF, $2FF00FF, $AFF00FF, $390000FF
                dc.l $FF0100, $43100320, $1B400380, $21FF2500, $3000B00
                dc.l 0
                dc.l $3E0041FF, $FF00D4, $1FF00FF, $CA0080, $18FF00FF
                dc.l $6801FF, $FF00DE, $2000001A, $7500D4, $D220015, $3C0D04
                dc.l $9B0E12, $6F0E18, $940E15, $810E12, $B800BC, $D170056
                dc.l $50E8F, $641A70, $A93B70, $A9028F, $640200, $283
                dc.l $E81700, $FF00FFFF, $FF00FF00, $FF00FFFF, $FF00FF00
                dc.l $56AC00A9, $9A920010, $B03FFFF, $FF00002D, $B800E4
                dc.l $D3B0021, $6C0D07, $FE0E1F, $FB0E2A, $A40E4A, $690E20
                dc.l $7A00EC, $D28007C, $568C00, $FF00FFFF, $FF00FF00
                dc.l $1140080, $10000AE, $2000092, $402C4, $7F02FA, $FF065A
                dc.l $5A025C, $9A0278, 0
                dc.l $95B005C, $25C0052, $66015896, $20B00, $FFFFFF00
                dc.l $2600BF, $BA0D32, $10000E, $D0600C4, $E00D1B, $130080
                dc.l $D24001A, $E1F0096, $C00D1B, $7F00CE, $D220046, $EF00A0
                dc.l $8B000114, $802259, $88025F, $E85B16, $B2027A, $472E18
                dc.l LFU_A, $180070, $1AA80000, $10700BD, $C01296, $20B02
                dc.l $FFFFFF00, $360074, $140D46, $7B00FC, $D090087, $E26001F
                dc.l $E3200D4, $E2C0079, $E2600B7, $7C0D30, $420069, $8C00FF00
                dc.l $FFFFFF00, $FF00FF00, $FFFFFF00, $FF000114, $805399
                dc.l $C99A96, $20B03, $FFFFFF00, $4900B7, $240D5F, $88002C
                dc.l $D0C00EA, $E3300AB, $E4400E4, $E850079, $E340079
                dc.l $AC0D41, $6800BA, $8C00FF00, $FFFFFF00, $FF000110
                dc.l $68E, $5C02E9, $AF02D9, $848025D, $5A0279, $DF00FF
                dc.l $A50025D, $84BF99, $B00, $FFFFFF00, $3400CC, $B70D44
                dc.l $550075, $D09003D, $500D24, $F50040, $D310047, $E2B001E
                dc.l $200D25, $890015, $D2E00C9, $790070, $8B000110, $83B2
                dc.l $19020D, $522DFF, $DF00F0, $FF00FF, $D400F0, $1A8801FF
                dc.l $F8000D, $201299, $B02, $FFFFFF00, $440081, $110D58
                dc.l $C10063, $D0C0000, $300D30, roscalei, $D400001, $E380000
                dc.l $E00D30, $C000C3, $D3C00C4, $F30010, $8B00FF00, $FFFFFF00
                dc.l $FF00FF00, $FFFFFF00, $FF000110, $53B4, $E49A99, $B03
                dc.l $FFFFFF00, $5700C4, $210D71, $CD0093, $D0F0063, $300D3D
                dc.l $8C00C0, $D520011, $EA3001E, $900D3E, $8200F3, $D4D00EB
                dc.l $440010, $8B00FF00, $FFFFFF00, $FF000114, $800686
                dc.l $4C02F6, $3F02F1, $7F07F0, $25C00E2, $D3850C00, $FFFFFF00
                dc.l $6E0026, $FF0D8E, $E900CD, $D130052, $D00D4D, $4B0E67
                dc.l $F0E5A, $2D0E4E, $80006D, $D6100D9, $AD00F0, $8B000100
                dc.l $23C00000, $2C00000, $5B040000, $2040000, $2C000000
                dc.l screen1, 2, 0
                dc.l $18000001, UPDA1F, star, $2F0, $3F00900, 0
                dc.l $8000100, $100B900, $3001B00, $30021FF, $B8000000
                dc.l UPDA1F, 0
                dc.l WID8, 0
                dc.l LFU_A, cbutton, d_z, $D00, 0
                dcb.l 2,$D000100
                dc.l $D000000, $E000100, $D000000, $D00, 0
                dc.l $8B00, $FF00FFFF, $FF00FF00, $FF00FFFF, $FF00FF00
                dc.l $1140080, $22510060, $26400F2, $2B800000, $628500B0
                dc.l $1000020, $B81900, $9001FF, $FC00F9, $600110, $7C00C0
                dc.l SRCEN, $7C0020, $A850806, $3005500, $FF00FF, $611000DC
                dc.l $830120, padright, $19400080, width, $8A00D8, $1010302
                dc.l $19FF5600, $60FF, $FF005A, $1FF00FF, $DE19FF, $FC00F2
                dc.l $1FF00FF, $100209, $140209, $141800, $3B004E, $710D4C
                dc.l $CB0083, $D0A0062, $E290088, $E370061, $E300074, $E00200
                dc.l $E0092A, $2E00E3, $D340094, $896F00, $1B00, $1140080
                dc.l $221A0016, $26100B0, $2BA300D1, $8900008E, $1FF00F0
                dc.l SRCEN, $8650021, $85080B, $1040103, $5580A9FF, $5691008F
                dc.l $A7000032, $D80092, $D4100CB, $160D08, $E50060, $D230095
                dc.l $800D2F, $720E29, $8300C0, $D240023, $D60D2D, $C002D
                dc.l $200D8F, $641A70, $A93B70, $A9028F, $640000, $20100
                dc.l $11B0083, $E81700, $1100000, $1010047, $5A0101, $B006C
                dc.l $2850000, $185, 0
                dc.l $5C00000, $2C00000, $26A007B, $AC00000, $2C00000
                dc.l $5C010900, $58900002, $B00FFFF, $FF00001D, $540095
                dc.l $D2500D0, $AF0D05, $1C00F0, $D140073, $C00D1B, $450E17
                dc.l $DC0060, $D1400C5, $8F0D19, $E40033, $504D04, $9040904
                dc.l $B000300, optionbu, $1001300, $11032FE, $3C1501, $13000BF
                dc.l $FF28FF, $8F07F1, $B42001, $47030465, $210090, $20702
                dc.l $3003508, $430885FF, $FF000019, $ED0054, $D210063
                dc.l $BC0D04, $8300C0, $D12000F, $878004C, $3030018, $140E15
                dc.l $110080, $D120057, $3C0D16, $DC007D, $408B00, $FF00FFFF
                dc.l $FF00FF00, $FF00FFFF, $FF00818F, $641A70, $A93B70
                dc.l $A9028F, $640200, $283, $E81700, $1100000, $22B10042
                dc.l $2C300AE, $8DFF00FE, $A30060, $2C600F8, $18FF00FA
                dc.l $A30100, $26, $E002C3, $3E30060, $A900002, $B00FFFF
                dc.l $FF000052, $4900CE, $D6A00AE, $A0D0E, $6C00A0, $D3900B2
                dc.l $800D4C, $EE0E43, $500040, $D3A0099, $4A0D49, $A00B8
                dc.l $E08B00, $1100000, $22240048, $1010043, $AC2B8E, $58A87
                dc.l $8001F5, $B20000, $A900002, $B03FFFF, $FF000049, $D300EF
                dc.l $D5F00AD, $9D0D0C, $EF00D0, $D3300BF, $400D44, $FF0E3C
                dc.l $5F0020, $D34008E, $3D0D41, $82005C, $F08B00, $54B00A4
                dc.l $21000FA, $E7920004, $B00FFFF, $FF000040, $1E0024
                dc.l $D53000D, $2C0D0B, $3A00C0, $D2C00EB, $D3B, $E40E34
                dc.l $670080, $D2D009E, $AC0D38, $DD002A, $404D00, $9000900
                dc.l $B02031C, $40202, $10F1300, $25BE006E, $2BC00EE, $901000F
                dc.l $3F00FF, $1040058, $60600F0, $8080108, 0
                dc.l $24FF00EA, UPDA1F, $715, $400717, $7A0112, $FC14FF
                dc.l $28FF00FC, $140000, $8FF00FF, $FF01FF, $FE0093, $800100
                dc.l $13020F, $FF00E0, $A920004, $7050302, $35003F10, $3005320
                dc.l $31FF74FF, $F00010, $56020020, $3000002A, $4E0070
                dc.l $D3600B0, $500D07, $650000, $D1D0094, $8750040, $3000027
                dc.l $700876, $440301, $41008F, $200202, $B00376, $440301
                dc.l $1E000A, $500D25, $7100C7, $8B00, $FF00FFFF, $FF00FF00
                dc.l $FF00FFFF, $FF00FF00, $10023C0, $2C0, $8D00, vfb_ysca
                dc.l $102, 0
                dc.l $18000001, $201, 0
                dc.l $10800F0, $108, allpad, DCOMPEN, 0
                dc.l $7000100, $100B900, $FF00FF, $10000FF, $FF1900, $FF00FF
                dc.l $10000FF, $FF0100, $30019FF, $B8000000, UPDA1F, 0
                dc.l $19000000, UPDA1F, 0
                dcb.l 2,bbutton
                dc.l $18000000, padleft, $D000000, $D00, 0
                dc.l $D000000, $D00, $E00, 0
                dc.l $D000000, $D00, 0
                dc.l $8B00, $251A0094, $100005D, $902B98, $918A8E, $1F0
                dc.l SRCEN, $A920004, $B03FFFF, $FF00006C, $9D007E, $D8C00EA
                dc.l $1A0D13, $D00A0, $D4C0036, $800D65, $9E0E58, $EA0040
                dc.l $D4D0067, $5A0D60, $7B0053, $E08B00, $1140080, $23F0062
                dc.l $21300BE, $E79C0C00, $FFFFFF00, $3300D3, $2D0D43
                dc.l $1000F7, $D090011, $700D24, $4500C0, $D30005D, $E2A0051
                dc.l $600D24, $D600D7, $D2D00EB, $4C00D0, $8B000114, $8022C0
                dc.l $2C0, $A83, allpad, LFU_A, $6000000, $8010130
                dc.l $BF00FF, $24000000, $40260, $FC06F1, $B40704, UPDA1F
                dc.l $1401, $28000002, $150055, $8000001, UPDA1F, star
                dc.l $108, $F00208, allpad, $A9C0802, $3003508, $3F000308
                dc.l $530031FF, $74000000, $5600, CD_init, $30006B, $EC0D3E
                dc.l $A40004, $D080078, $400D21, $E10878, $4C0303, $2D002C
                dc.l $8750040, $3000027, $860080, $20000E0, $3750040, $3000022
                dc.l $680084, $D2A00E3, $9600C0, $8B00FF00, $FFFFFF00
                dc.l $FF000114, $8022C1, $FE02BB, $B6167F, $FF1300, $3001651
                dc.l $4C0E08, $D020FF, $36080204, $910C02, $4650021, $9C0004
                dc.l $7030309, $71208DFF, $FF00001B, $B3009E, $D2300B2
                dc.l $7A0D04, $D300A0, $D13004E, $800D19, $BE0E20, $D10050
                dc.l $2010000, $378004C, $3030013, $9B00BA, $D180070, $F500E0
                dc.l $8B00FF00, $FFFFFF00, $FF000114, $802215, $B4025F
                dc.l $DC2B8C, $8E9A9C, $C03FFFF, $569B003F, $A7000060
                dc.l $520087, $D7C00ED, $E50D10, $E40050, $D430091, $400D5A
                dc.l $170E4E, $D40020, $D44009F, $850D55, $890076, $708B00
                dc.l i, $40C002, $28240E0D, $783C240, $286428, $2003198C
                dc.l $60A20854, $1C283389, $8D088408, $B8008921, $90904884
                dc.l $18F45C23, $29950448, $8108F806, $4B2C9947, $C0001088
                dc.l $50832997, $4D200000, $52, $160008, $2FFFF, i, $40C000
                dc.l $21041000, $228442E0
                dcb.l 2,0
_fsync:    dc.w 0                  ; DATA XREF: everythi:loc_192E46↑r
                                        ; sub_192088+DEA↑w
_fmode:    dc.w 0                  ; DATA XREF: sub_1934AA+62↑w
                                        ; ROM:001936A8↑w ...
_ein_buf:   dc.l 0                  ; DATA XREF: sub_1934AA+28↑w
                                        ; ROM:00193694↑w ...
            dc.l 0                  ; DATA XREF: sub_1934AA+4E↑w
                                        ; ROM:0019369E↑w ...
            dc.l 0                  ; DATA XREF: sub_1934AA+5C↑w
                                        ; ROM:0019389E↑w ...
dword_1A6810:   dcb.l $A,0              ; DATA XREF: ROM:001938B0↑w
                                        ; ROM:001938EC↑w ...
; ---------------------------------------------------------------------------
;vidinit.s


VideoIni:                             ; CODE XREF: sub_19324E↑p
                movem.l d0-d6,-(sp)
                clr.w   (pal).l
                move.w  ($F14002).l,d0
                and.w   #$10,d0
                beq.w   ispal
                move.w  #$337,d2
                move.w  #$581,d0
                move.w  #$10A,d6
                move.w  #$F1,d4
                bra.w   doit
; ---------------------------------------------------------------------------

ispal:                             ; CODE XREF: ROM:001A684C↑j
                move.w  #$34B,d2
                move.w  #$565,d0
                move.w  #$142,d6
                move.w  #$11F,d4
                move.w  #1,(pal).l

doit:                             ; CODE XREF: ROM:001A6860↑j
                move.w  d0,(width2).l
                move.w  d4,(height2).l
                move.w  d0,d1
                asr.w   #1,d1
                sub.w   d1,d2
                add.w   #4,d2
                sub.w   #1,d1
                or.w    #$400,d1
                move.w  d1,(a_hde).l
                move.w  d1,(HDE).l
                move.w  d2,(a_hdb).l
                move.w  d2,(HDB1).l
                move.w  d2,(HDB2).l
                move.w  d6,d5
                sub.w   d4,d5
                move.w  d5,(n_vdb).l
                add.w   d4,d6
                move.w  d6,(n_vde).l
                move.w  #$FFFF,(VDE).l
                move.w  #0,(BG).l
                move.l  #0,(BORD1).l
                movem.l (sp)+,d0-d6
                rts
; ---------------------------------------------------------------------------
                .dphrase

; =============== S U B R O U T I N E =======================================


gpuload:                             ; CODE XREF: ROM:loc_1A6986↓p
                movem.l d0-d2/a0-a1,-(sp)
                clr.l   d0
                move.l  d0,(G_CTRL).l
                move.l  #0,(A1_CLIP).l
                move.l  #$4020,d0
                move.l  d0,(A1_FLAGS).l
                move.l  d0,(A2_FLAGS).l
                move.l  (a0)+,d0
                move.l  d0,d1
                and.l   #$FFFFFFF8,d0
                sub.l   d0,d1
                move.l  d0,(A1_BASE).l
                asr.l   #1,d1
                move.l  d1,(A1_PIXEL).l
                move.l  (a0)+,d0
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

.aligned:                             ; CODE XREF: sub_1A68F0+6E↑j
                move.l  #$1800001,d0

.blit_go:                             ; CODE XREF: sub_1A68F0+76↑j
                move.l  d0,(B_CMD).l

wblit:                             ; CODE XREF: sub_1A68F0+8E↓j
                move.l  (B_CMD).l,d0
                btst    #0,d0
                beq.s   wblit
                movem.l (sp)+,d0-d2/a0-a1
                rts
; End of function gpuload

; ---------------------------------------------------------------------------

gpurun:                             ; CODE XREF: sub_192088+DA6↑p
                bsr.w   gpuload
                movem.l d0-d1/a0,-(sp)
                move.l  (a0)+,(G_PC).l
                move.l  #$11,(G_CTRL).l
                movem.l (sp)+,d0-d1/a0
                rts
; ---------------------------------------------------------------------------
                movem.l d0/a0,-(sp)
                lea     (G_CTRL).l,a0

.gpuwt:                             ; CODE XREF: ROM:001A69B4↓j
                move.l  (a0),d0
                btst    #0,d0
                bne.s   .gpuwt
                movem.l (sp)+,d0/a0
                rts
; ---------------------------------------------------------------------------
                .dphrase
gpumods:     dc.l alpha       ; DATA XREF: sub_192088+D8A↑o
                dc.l beta
                dc.l gamma
                dc.l psi
                dc.l delta
                dc.l epsilon
                dc.l theta
                dc.l sigma
                dc.l tau
                dc.l shu
                dc.l dbeast
                .dphrase
alpha:   dc.l G_RAM, $9D0, $98003F5C, $F0980E, $3F8000F0, $A4019800
                                        ; DATA XREF: ROM:gpumods↑o
                dc.l $480000, $60A401, $9800004C, _i4, $A402BDC1, $C4229800
                dc.l $500000, $60A401, $980000A4, _i4, $A402C441, $C4622C21
                dc.l $C4819800, $3F4000F0, $A4179800, $A00000, $60A416
                dc.l $980A0300, $980F, $17E0000, $980D00FF, $9800, $E80000
                dc.l $60A401, $63C19800, $39B000F0, $20A401, $C6219800
                dc.l $3F5C00F0, $A41D7C3D, $D4C1E400, $98003392, $F0D000
                dc.l $E4009800, $364600F0, $7D3DD002, $E4009800, $31C800F0
                dc.l $7C5DD002, $E4009800, $E00000, $60A402, $980000E4
                dc.l _i4, $A4018840, $8FDE881F, $8825661F, $660527DF, $27C5643E
                dc.l $13DFD454, $E400201F, $13C5D454, $E4002005, $639F2BE5
                dc.l $63059800, $31BC00F0, $2800C5E0, $98000024, _i4, $A4069800
                dc.l $280000, $60A407, $98000034, _i4, $A4189800, $EC0000
                dc.l $60A401, $66012821, $C7616D86, $6D876D98, $98000084
                dc.l _i4, $A4159813, $31BC00F0, $9814316C, $F09809, $35000000
                dc.l $980B01FF, $981B, $FFFF0000, $9800009C, _i4, $A41C8CE8
                dc.l $8DFA9800, padright, $60A404, $8286604, $3408D442
                dc.l $64282D28, $E4008911, $3415D442, $64352D35, $E4008AB2
                dc.l $341AD442, $643A2D3A, $E4008B59, $88C088E1, $8B026480
                dc.l $64816482, $110032, $592571, $25729800, $FF0000, $25791011
                dc.l $1012980C, $394400F0, $D180E400, $1824D294, $E400AEA0
                dc.l $D000E400, $9800334E, $F0C5E0, $98000088, _i4, $A401C741
                dc.l $980000EC, _i4, $A4016601, $C7619800, $240000, $60A406
                dc.l $98000028, _i4, $A4079800, $340000, $60A418, $98000000
                dc.l $C01006, $10079800, padright, $60A404, $9800008C, _i4
                dc.l $A4146604, $66149800, $380000, $60A408, $98000040
                dc.l _i4, $A4158900, $88816201, $883E8C1B, $8B7C5800, $79B5801
                dc.l $79B883C, $401C881D, $661C661D, $403D6601, $3BC883D
                dc.l $401D6600, $3BC4020, $62000380, $341BD442, $8BC12000
                dc.l $6C201006, $98000074, _i4, $A4009009, $980000CC, _i4
                dc.l $A41A9800, $700000, $60A410, $980000E0, _i4, $A4029800
                dc.l $E40000, $60A401, $C782C7A1, $904A90C5, $908694A6
                dc.l $94C40350, $92079800, $DC0000, $60A401, $AFA00020
                dc.l $9541C7A0, $C781E400, $E400AF82, $AFA18840, $8FDE881F
                dc.l $8825661F, $660527DF, $27C5643E, $13DFD454, $E400201F
                dc.l $13C5D454, $E4002005, $639F2BE5, $63059800, $D80000
                dc.l $60A401, $22C782, $AC628A00, $660025A0, $409C1E, $8209C1C
                dc.l $13DC8A02, $6502631E, $25A2445C, $3DCAF41, $403C6D1C
                dc.l $88D188F2, $8B190392, $66199208, $980C3948, $F0D180
                dc.l $E4009510, $1069800, $32D600F0, $95211824, $D0140030
                dc.l $9800337C, $F012B8, $D01894F0, $1834D018, $E4009800
                dc.l $32BA00F0, $D000E400, $AEA0D000, $E4009801, $211400F0
                dc.l $A4203C00, $BC20D7E0, $E400981F, $355400F0, $3616D481
                dc.l $E400ADF3, $D3E0E400, $C5456505, $9801000F, $88A0
                dc.l $64852420, $62056200, $C565C580, $98000048, _i4, $A4019800
                dc.l $4C0000, $60A402, $BDC1C422, $98000050, _i4, $A4019800
                dc.l $A40000, $60A402, $C441C462, $98000078, _i4, $A401C481
                dc.l $981D0080, $9800, $A80000, $60A401, $980000AC, _i4
                dc.l $A402C5A1, $C5C26E01, $6E021031, $1052C4B1, $C4D2AC20
                dc.l $AC81AC42, $10040, $C420AC62, $C4816601, $25A18820
                dc.l $8010040, $8019C1E, $8209C13, $13D3AC9F, $13BE651F
                dc.l $631E25BF, $47F3027E, $25A18BC0, $414640, $9C220821
                dc.l $9C331053, $13A26302, $47F30262, $463E4451, $44521011
                dc.l $3D26DF1, $6DF29800, $B00000, $60A401, $44314432
                dc.l $6D916D92, $ADA1ADC2, $6E016E02, $310052, $AD60AD81
                dc.l $8FDE881F, $8825661F, $660527DF, $27C5643E, $13DFD454
                dc.l $E400201F, $13C5D454, $E4002005, $639F2BE5, $6305881F
                dc.l $980000B8, _i4, $A41E03DF, $C57F9800, $BC0000, $60A41E
                dc.l $3C1C581, $981F3554, $F09813, $34F000F0, $D3E0E400
                dc.l $ACB1ACD2, $980000B4, _i4, $A4019800, $C00000, $60A402
                dc.l $981E1000, $3C1, $3C24431, $44526D91, $6D92ADA1, $ADC29800
                dc.l $C40000, $60A41E, $3C1C5A1, $980000C8, _i4, $A41E03C2
                dc.l $C5C2A5C0, $9801341E, $F01820, $D478BDC0, $D020E400
                dc.l $ADF3AD45, $D260E400, $981F35DA, $F0C4F3, $C5363416
                dc.l $D4C2E400, $9813356E, $F0D3E0, $E40089E0, $981F35DA
                dc.l $F01220, $88113436, $D4C2E400, $9813358A, $F0D3E0
                dc.l $E40089E0, $12408812, $3456D4C2, $E4009813, $35A000F0
                dc.l $D3E0E400, $89E01220, $981335B6, $F08811, $3476D462
                dc.l $E400D3E0, $E40089E0, $12206496, $8A518812, $3496D4A2
                dc.l $E400ACF3, $AD36D260, $E4003916, $9800355E, $F0D000
                dc.l $E400AE20, $D000E400, $79F1D274, $79F2D274, $7C11D278
                dc.l $7C12D278, $8A218A42, $63E14142, $2202E2, $8B606500
                dc.l $28A0B840, $D260E400, $79F1D274, $79F2D274, $7C11D278
                dc.l $7C12D278, $8A218A42, $63E14142, $2202E2, $8B606500
                dc.l $28A0B840, $842B840, $142B840, $1842B840, $D260E400
                dc.l $98003382, $F0D000, $E4009800, $540000, $60A402, $98000058
                dc.l _i4, $A4016582, $65810822, $8219049, $902A9800, $E00000
                dc.l $60A402, $980000E4, _i4, $A4018840, $8FDE881F, $8825661F
                dc.l $660527DF, $27C5643E, $13DFD454, $E400201F, $13C5D454
                dc.l $E4002005, $639F2BE5, $63059800, $240000, $60A404
                dc.l $98000028, _i4, $A4069800, $700000, $60A415, $98000080
                dc.l _i4, $A4189800, $D40000, $60A419, $6E189800, $7C0000
                dc.l $60A409, $9800387C, $F07C38, $D0188B21, $6E017C81
                dc.l $D018E400, $980037E6, $F0C5E0, $980B0000, $100570B
                dc.l $889188D2, $AC628AA0, $9521981D, padright, $66004020
                dc.l $25A00040, $9C1E0820, $9C1A13DA, $8AA213BE, $65024022
                dc.l $631E25A2, $445A03DA, $8B3E8C1B, $8B7C581A, $79B5819
                dc.l $79B8B3C, $435C8B5D, $661C661D, $433D6619, $3BC8B3D
                dc.l $435D661A, $3BC433A, $621A039A, $341BD442, $8BD9201A
                dc.l $92AB955B, $135981D, padright, $AC628AA0, $66004360
                dc.l $25A00040, $9C1F0820, $9C0113E1, $8AA213BF, $65024362
                dc.l $631F25A2, $4441003F, $8B3E8C1B, $8B7C581F, $79B5819
                dc.l $79B8B3C, $43FC8BFD, $661C661D, $433D6619, $3BC8B3D
                dc.l $43FD661F, $3BC433F, $621F039F, $341BD442, $8BD9201F
                dc.l $95750352, $3F16611, $6612980C, $339200F0, $981BFFFF
                dc.l $D180, $E4009800, $370400F0, $1838D014, $175AEA0
                dc.l $98013F58
                dc.l $F0A422, $3402D002, $E4009800, padleft, $60A401, $6C610039
                dc.l $980000D4, _i4, $BC199800, $E00000, $60A404, $980000E4
                dc.l _i4, $A4059800, $D80000, $60A406, $980000DC, _i4
                dc.l $A40700C4, $E59800, $E00000, $60BC04, $980000E4, _i4
                dc.l $BC059800, $EC0000, $60A404, $98000038, _i4, $A4056CE5
                dc.l $A49800, $EC0000, $60BC04, $AEA0D000, $E4009800, $EC0000
                dc.l $60A401, $980035E0, $F06601, $98023608, $F00821, $7C21D002
                dc.l $E4007C41, $D042E400, $98002238, $F0A402, $3402D7A2
                dc.l $E4009800, $220000F0, $BC179802, $44200001, $880BC02
                dc.l $89F01030, $8A3C8822, $6422105C, $7C1CD278, $7B90D278
                dc.l $8B808A5C, $6200105C, $7C1CD278, $7B90D278, $6600621C
                dc.l $281C9800, $220C00F0, $BC1C8822, $20020880, $62026602
                dc.l $3A028B7C, $BC02651C, $9800226C, $F028BC, $8B82621C
                dc.l $285CBC1C, $1880BC1C, $88229800, $223C00F0, $62012841
                dc.l $BC019802, $2000001, $1880BC02, $A4013401, $D7A2E400
                dc.l $D260E400, $62516252, $981B01FF, $2779, $133B63D9
                dc.l $633B8A3D, $58115731, $37FDD442, $E4002011, $8A5D5812
                dc.l $573237FD, $D442E400, $20126CB1, $6CB29800, $C00000
                dc.l $110012, $98003392, $F0D000, $E400AF60, $8B614001
                dc.l $98000000, $7F2401, $9800388C, $F0D000, $E4009800
                dc.l $339200F0, $D000E400, 0
                dcb.l 2,$F03608
                dc.l $F03882, $F0398A
                dcb.l $04,0
beta:   dc.l G_RAM, $61E, $88729252, $980000E0, $240, $A4029800
                                        ; DATA XREF: ROM:001A69C4↑o
                dc.l $E40000, $240A401, $88408FDE, $881F8827, $661F6607
                dc.l $27DF27C7, $643E13DF, $D454E400, $201F13C7, $D454E400
                dc.l $2007639F, $2BE76307, $98000004, $240, $A4176617
                dc.l $98000008, $240, $A4186618, $981D34F6, $F0980B, $1800000
                dc.l $980C0180, $9800, $980000, $240A401, $98003F60, $F06601
                dc.l $A4020882, $A440B801, $9800002C, $240, $A4039800
                dc.l $300000, $240A404, $66036604, $12EB130C, $642B642C
                dc.l $981B00C0, $981A, $30E000F0, $8B7C1363, $37C1364
                dc.l $6BD358, $E4007B8B, $D3B4E400, $981A30EE, $F09819
                dc.l $30F200F0, $8CD358, $E4007B8C, $D3B4E400, $D320E400
                dc.l $1772D6B, $980030C4, $F0D000, $E4000198, $2D8C981C
                dc.l $1800000, $7C17D3A2, $E400D3B8, $E4007C18, $D3A2E400
                dc.l $D3B8E400, $7B97D3B4, $E4007B98, $D3B4E400, $98000000
                dc.l $240, $A4089810, $1800000, $98113168, $F09806, $319A00F0
                dc.l $7B10D222, $2C632C84, $9805017F, $9806, $17F0000
                dc.l $98003598, $F0CC1E, $D00008DE, $98060180, $8B04, $1842C63
                dc.l $8A059800, $359800F0, $9811319A, $F07AF0, $D2222C63
                dc.l $89848986, $3068965, $98003598, $F09805, $1800000
                dc.l $8AE38984, $89860163, $3061883, $98003598, $F0620C
                dc.l $98003F80, $F0296C, $BC0C980D, $223800F0, $981A2F41
                dc.l $4180980E, $220000F0, $980F2224, $F09801, i, $98000020
                dc.l $240, $A4001020, $9801FFFF, $FF2420, $98012268, $F07C00
                dc.l $D481E400, $981A0F41, $180BC20, $98012274, $F0BC20
                dc.l $98012270, $F0BC20, $9801227C, $F0BC20
                dcb.l 3,$881BC20
                dc.l $980A0000, $FFFF9809, $FFFF0000, $980000A4, $240
                dc.l $A4139800, 0
                dc.l $240A401, $BDE19802, $352200F0, $9800008C, $240, $A4157C15
                dc.l $D041E400, $98154420, $3C435, $28159815, $44200001
                dc.l $C8359800, $3F4400F0, $A414BDD4, $98000004, $240
                dc.l $A4176617, $98000008, $240, $A4186618, $8B146214
                dc.l $2AF49814, $1800180, $C4548AEB, $8B0C9800, $C0000
                dc.l $240A414, $6614428B, $98000010, $240, $A4146614, $428C6509
                dc.l $8A629800, $140000, $240A414, $8A806600, $25200040
                dc.l $9C1E0820, $9C1C13DC, $8A826502, $631E2522, $445C03DC
                dc.l $98018000, $103C, $8B901260, $9801003F, $20, $25200260
                dc.l $9C1E0820, $9C1C13DC, $8A826502, $631E2522, $445C03DC
                dc.l $98018000, $103C, $8B91630B, $630C8963, $92208C1B
                dc.l $8B7C5803, $79B5811, $79B8A3C, $407C887D, $661C661D
                dc.l $423D6611, $3BC8A3D, $407D6603, $3BC4223, $62030383
                dc.l $341BD442, $94112003, $89649200, $8C1B8B7C, $5804079B
                dc.l $5810079B, $8A1C409C, $889D661C, $661D421D, $661003BC
                dc.l $8A1D409D, $660403BC, $42046204, $384341B, $D4429410
                dc.l $20048985, $92008C1B, $8B7C5805, $79B5810, $79B8A1C
                dc.l $40BC88BD, $661C661D, $421D6610, $3BC8A1D, $40BD6605
                dc.l $3BC4205, $62050385, $341BD442, $94102005, $20058986
                dc.l $92208C1B, $8B7C5806, $79B5811, $79B8A3C, $40DC88DD
                dc.l $661C661D, $423D6611, $3BC8A3D, $40DD6606, $3BC4226
                dc.l $62060386, $341BD442, $94112006, $9809FFFF, $8867
                dc.l $88F588FB, $580756E7, $37FBD442, $E4002007, $88E86607
                dc.l $2528888B, $8976897B, $580B56EB, $37FBD442, $E400200B
                dc.l $896C254B, $620C2967, $298890E7, $910888A7, $88FB5807
                dc.l $570737FB, $D442E400, $200712A7, $88E86607, $252888CB
                dc.l $897B580B, $570B37FB, $D442E400, $200B12CB, $896C254B
                dc.l $620C2967, $2988981B, $80000000, $98000018, $240
                dc.l $A40B036B, $9800001C, $240, $A40C036C, $6C236C24
                dc.l $106B6C25, $108C6C26, $10AB10CC, $89658966, $66052526
                dc.l $898B620C, $254B2986, $2965A5A0, $3400D7A2, $E400C465
                dc.l $C4C6C487, $C4A894E7, $9508C4E7, $C5088AE0, $20002520
                dc.l $3A00C880, $98003F80, $F0A415, $C8756218, $2B179800
                dc.l $223C00F0, $BC17BDBA, $A5A03400, $D7A2E400, $98003516
                dc.l $F0D000, $E4009801, $211400F0, $A4203C00, $BC20D7E0
                dc.l $E4009800, $3F5400F0, $A4082C63, $2C849805, $1800000
                dc.l $88A69800, $359800F0, $CC1ED000, $8DE980E, $3F8000F0
                dc.l $AEA0D000, $E400981A, $2E014180, $98154420, $C435
                dc.l $28159815, $44200000, $C8359800, $3F4400F0, $A414BDD4
                dc.l $8AE02000, $25203A00, $C880C480, $98003F80, $F0A415
                dc.l $2AB5C875, $9800001C, $240, $A40C9800, roscalei, $89346214
                dc.l $100C268C, $195C475, $62182B17, $9800223C, $F0BC17
                dc.l $BDBAA5A0, $3400D7A2, $E4009800, $351600F0, $D000E400
                dc.l $981D361A, $F07865, $D3B8E400, $7886D3B8, $E400980E
                dc.l $220000F0, $98112238, $F08880, $62002860, $C46088E0
                dc.l $620028E0, $98012268, $F0BC20, $881BC20, $BDC89800
                dc.l $44200000, $C420981D, $361A00F0, $88BB88DC, $107BD3B8
                dc.l $109CD3B8, $83B083C, $E4008B60, $20006200, $66003A00
                dc.l $C4809800, $223C00F0, $621C2B7C, $BC1C9800, $2000001
                dc.l $BE20A620, $3400D7A2, $E400D3C0, 0
gamma:   dc.l G_RAM, $A38, $980E3F80, $F09800, allpad, $60A401
                                        ; DATA XREF: ROM:001A69C8↑o
                dc.l $980000F4, _i4, $A4029800, $392000F0, $7C02D482, $24417841
                dc.l $D001E400, $98000070, _i4, $A4069805, $FF0000, $660688C7
                dc.l $980000A4, _i4, $A4049802, padleft, $24A60047, $8624A7
                dc.l $9CC80087, $98000080, $9CE9, $10081009, $C748C769
                dc.l $98000084, _i4, $A4019804, $80000000, $98000088, _i4
                dc.l $A4021081, $C7811082, $C7A29800, $3F7800F0, $A4019800
                dc.l $980000, $60A414, $8CF56614, $26B463D4, $281A426
                dc.l $A0C80846, $A0C90846, $A0CA9129, $914A0846, $89076387
                dc.l $C7980E, $3F8000F0, $94639800, $34AE00F0, $CC1ED000
                dc.l $8DE9800, 0
                dc.l $60A417, $980000A0, _i4, $A416980A, $3000000, $980F017E
                dc.l $980D, $FF0000, $98000024, _i4, $A41E9800, $280000
                dc.l $60A41F, $6E1E6E1F, $98003F7C, $F0A401, $2821C601
                dc.l $A0C50846, $6305A0C1, $846A0DB, $84663C1, $E1A031
                dc.l $841A032, $95209541, $10111032, $64916492, $AF98AFB9
                dc.l $47114732, $6D716D72, $8A388A59, $2012AF5C, $AF7D47B1
                dc.l $47924798, $47B90251, $3198B32, $6CF16CF2, $3D103F2
                dc.l $980C334C, $F09800, $318400F0, $2800C5E0, $D180E400
                dc.l $98003000, $AE01, $12821, $C6019800, $240000, $60A41E
                dc.l $98000028, _i4, $A41F6E1E, $6E1FA0C1, $846A0DB, $84663C1
                dc.l $E1A031, $841A032, $95209541, $10111032, $64916492
                dc.l $AF98AFB9, $47114732, $6D716D72, $8A388A59, $2012AF5C
                dc.l $AF7D47B1, $47924798, $47B90251, $3198B32, $6CF16CF2
                dc.l $3D103F2, $980C334C, $F09800, $320800F0, $2800C5E0
                dc.l $D180E400, $98003000, $AE01, $12821, $C6019800, $240000
                dc.l $60A41E, $98000028, _i4, $A41F6E1E, $6E1FA0C1, $846A0DB
                dc.l $88663C1, $E1A031, $841A032, $95209541, $10111032
                dc.l $64916492, $AF98AFB9, $47114732, $6D716D72, $8A388A59
                dc.l $2012AF5C, $AF7D47B1, $47924798, $47B90251, $3198B32
                dc.l $6CF16CF2, $3D103F2, $980C334C, $F09800, $328C00F0
                dc.l $2800C5E0, $D180E400, $90C690E7, $91089800, $3F7C00F0
                dc.l $A4012821, $C601C644, $9800331C, $F0C5E0, $AE009801
                dc.l $2FFA0000, $A0030840, $A0040840, $A0140840, $8A972800
                dc.l $C6000020, $A0050840, $A0060840, $A0180840, $20A007
                dc.l $840A008, $840A019, $8F8C947A, $98000054, $1A, $25972598
                dc.l $25990357, $3580359, $A6E9A70A, $A72B6203, $62046205
                dc.l $62066207, $62086E03, $6E046E05, $6E066E07, $6E089801
                dc.l $34F400F0, $D020E400, $AE409801, $32A800F0, $18202800
                dc.l $C6407C00, $D021E400, $94C694E7, $95089800, $30C200F0
                dc.l $1828D001, $E4009800, $392000F0, $D000E400, $AE00981F
                dc.l $347600F0, $C6202C84, $3616D481, $E400ADF3, $D3E0E400
                dc.l $980034AE, $F0CC1E, $D00008DE, $980000A8, _i4, $A4019800
                dc.l $AC0000, $60A402, $C5A1C5C2, $6E016E02, $10311052
                dc.l $C4B1C4D2, $AC20AC81, $1AC62, $C4816601, $25A18820
                dc.l $8010040, $8019C1E, $8209C13, $13D3AC9F, $13BE651F
                dc.l $631E25BF, $47F3027E, $25A18BC0, $414640, $9C220821
                dc.l $9C331053, $13A26302, $47F30262, $463E4451, $44521011
                dc.l $3D26DF1, $6DF29800, $B00000, $60A401, $44314432
                dc.l $6D916D92, $ADA1ADC2, $6E016E02, $310052, $981F3476
                dc.l $F09813, $341400F0, $D3E0E400, $ACB1ACD2, $980000B4
                dc.l _i4, $A4019800, $C00000, $60A402, $981E1000, $3C1
                dc.l $3C24431, $44526D91, $6D92ADA1, $ADC29800, $C40000
                dc.l $60A41E, $3C1C5A1, $980000C8, _i4, $A41E03C2, $C5C2A5C0
                dc.l $98013390, $F01820, $D478BDC0, $D020E400, $ADF3D260
                dc.l $E400981F, $349200F0, $C4F3C536, $9813348A, $F0D3E0
                dc.l $E400ACF3, $AD36D260, $E4000824, $AE20B811, $840B812
                dc.l $88A12B61, $840B801, $8402800, $C620D260, $E4009800
                dc.l $480000, $60A401, $9800004C, _i4, $A402BDC1, $C4229800
                dc.l $500000, $60A401, $980000A4, _i4, $A402C441, $C4629800
                dc.l $780000, $60A401, $C481981D, padright, $D3C0E400, $65149294
                dc.l $980E2200, $F0980D, $223800F0, $98021040, $1A5BF
                dc.l $341FD7A2, $E4009800, $3F4000F0, $A4012821, $BDC19800
                dc.l $1800180, $2800C440, $E4006309, $630A630B, $7886D534
                dc.l $887F88A3, $8BE5889F, $88C48BE6, $893F8949, $8BEA7888
                dc.l $D534887F, $88E38BE7, $889F8904, $8BE8893F, $89698BEB
                dc.l $78C8D534, $88BF88E5, $8BE788DF, $89068BE8, $895F896A
                dc.l $8BEB88D2, $89131092, $10D38916, $8321096, $8330836
                dc.l $88F088AC, $1070106C, $8971894F, $1131112F, $6210620C
                dc.l $899F580C, $564C37FF, $D442E400, $200C8A1F, $581056D0
                dc.l $37FFD442, $E4002010, $89FF580F, $564F37FF, $D442E400
                dc.l $200F8A3F, $581156D1, $37FFD442, $E4002011, $96969814
                dc.l $392C00F0, $8AC09815, $392E00F0, $62168877, $28168938
                dc.l $63168879, $893A6217, $62199800, $379000F0, $7C04D014
                dc.l $E4009800, $36A400F0, $7C06D562, $E400D014, $E4007C08
                dc.l $D4D4E400, $98003914, $F0D000, $E4008A40, $62008A01
                dc.l $8A3F901F, $8C1B8B7C, $5801079B, $5800079B, $881C403C
                dc.l $883D661C, $661D401D, $660003BC, $881D403D, $660103BC
                dc.l $40016201, $381341B, $D44297E0, $2001901F, $8C1B8B7C
                dc.l $581F079B, $5800079B, $881C43FC, $8BFD661C, $661D401D
                dc.l $660003BC, $881D43FD, $661F03BC, $401F621F, $39F341B
                dc.l $D44297E0, $201F0039, $3FA2C84, $88B78958, $62179800
                dc.l $37C400F0, $D000E400, $88800092, $20000832, $62008981
                dc.l $89FF901F, $8C1B8B7C, $5801079B, $5800079B, $881C403C
                dc.l $883D661C, $661D401D, $660003BC, $881D403D, $660103BC
                dc.l $40016201, $381341B, $D44297E0, $2001901F, $8C1B8B7C
                dc.l $581F079B, $5800079B, $881C43FC, $8BFD661C, $661D401D
                dc.l $660003BC, $881D43FD, $661F03BC, $401F621F, $39F341B
                dc.l $D44297E0, $201F0037, $3F88A01, $8A3F901F, $8C1B8B7C
                dc.l $5801079B, $5800079B, $881C403C, $883D661C, $661D401D
                dc.l $660003BC, $881D403D, $660103BC, $40016201, $381341B
                dc.l $D44297E0, $2001901F, $8C1B8B7C, $581F079B, $5800079B
                dc.l $881C43FC, $8BFD661C, $661D401D, $660003BC, $881D43FD
                dc.l $661F03BC, $401F621F, $39F341B, $D44297E0, $201F0039
                dc.l $3FA8C04, $9800013F, $981F, $391400F0, $7804D3F4
                dc.l $E4009801, $37BC00F0, $7806D038, $E40088C1, $10011032
                dc.l $D3F8E400, $D3E2E400, $6204CC1E, $D28008DE, $88EC896F
                dc.l $10AC114F, $620C899F, $580C566C, $37FFD442, $E400200C
                dc.l $89FF580F, $566F37FF, $D442E400, $200F9800, $38E000F0
                dc.l $7C06D014, $E40088C0, $D32000, $8336200, $898189FF
                dc.l $901F8C1B, $8B7C5801, $79B5800, $79B881C, $403C883D
                dc.l $661C661D, $401D6600, $3BC881D, $403D6601, $3BC4001
                dc.l $62010381, $341BD442, $97E02001, $901F8C1B, $8B7C581F
                dc.l $79B5800, $79B881C, $43FC8BFD, $661C661D, $401D6600
                dc.l $3BC881D, $43FD661F, $3BC401F, $621F039F, $341BD442
                dc.l $97E0201F, $3703F8, $8A018A3F, $901F8C1B, $8B7C5801
                dc.l $79B5800, $79B881C, $403C883D, $661C661D, $401D6600
                dc.l $3BC881D, $403D6601, $3BC4001, $62010381, $341BD442
                dc.l $97E02001, $901F8C1B, $8B7C581F, $79B5800, $79B881C
                dc.l $43FC8BFD, $661C661D, $401D6600, $3BC881D, $43FD661F
                dc.l $3BC401F, $621F039F, $341BD442, $97E0201F
                dc.l $3903FA, $9800013F, $981F, $391400F0, $7806D3F4, $E4009801
                dc.l $390C00F0, $7808D038, $E4008901, $10011033, $D3F8E400
                dc.l $D3E2E400, $8A72CC1E, $D28008DE, $980E3F80, $F0ADE0
                dc.l $D000E400, $980E3F80, $F0AEA0, $D000E400, $93DE8AFB
                dc.l $8B1C8B3D, $8B5E7AF9, $D4D48B7F, $8BBB8BFD, $8B9F8BDC
                dc.l $8BFE137D, $139E6E1B, $661D083D, $8BDF581E, $57BE37FF
                dc.l $D442E400, $201EA5BF, $341FD7A2, $E4009801, $226800F0
                dc.l $BC360881, $BC369801, $44200000, $8BA06480, $D4C2E400
                dc.l $980039A0, $F0D000, $E4003A01, $9800227C, $F0BC1C
                dc.l $980039EA, $F0C421, $D000E400, $C4219380, $8B608C7F
                dc.l $27E07C00, $D4A2E400, $13DC1820, $D740E400, $8B806700
                dc.l $D4E2E400, $9800398A, $F0941C, $D000E400, $8BC163DE
                dc.l $98002288, $F0BC1C, $3C1880, $BC1C003C, $1880BC1C
                dc.l $3C1880, $BC1C9800, $227000F0, $9801FFFF, $FF243E
                dc.l $BC1E9801, $3A1E00F0, $7C04D038, $E4009801, $FFFF0000
                dc.l $243B289B, $C47B3A1D, $9800223C, $F0BC1D, $BDA29800
                dc.l SRCEN, $40197, $1F80219, $1832D2A1, $23A97DE, $D3C0E400
omega:   dc.l $F03A78, $484, $980E3F80, $F09800, $3F4C00F0, $8C21BC01
                                        ; DATA XREF: sub_192088+DA0↑o
                dc.l $A4017C01, $D7A1E400, $98023F68, $F08CA1, $A4402821
                dc.l $C7019802, $3DC000F0, $A4030880, $7C03D042, $E4002863
                dc.l $2800C6E3, $C7209801, $A, $98023AD0, $F09800, $3E8200F0
                dc.l $2842C6A2, $D000E400, $98003B64, $F07C01, $D002E400
                dc.l $8B549801, $3F0000, $24349800, $6000000, $40140374
                dc.l $8A839801, $3B4C00F0, $980000F8, _i4, $A4027C02, $D022E400
                dc.l $98003F40, $F0A401, $98000000, _i4, $BC019800, $FC0000
                dc.l $60A401, $935A937B, $93189339, $98023B40, $F09800
                dc.l $3E8200F0, $2842C6A2, $D000E400, $946395CE, $975A977B
                dc.l $97189739, $33A9800, $3ADC00F0, $1838D001, $E40097BD
                dc.l $85DA3A0, $820BBA0, $AEE39800, $3F5400F0, $A4019800
                dc.l 0
                dc.l $60BC01, $98003F3C, $F0A401, $980000F0, _i4, $BC012C21
                dc.l $98003F58, $F0BC01, $980000FC, _i4, $A4019802, $3BB200F0
                dc.l $98003E82, $F02842, $C6A2D000, $E4009463, $95CE9800
                dc.l $3000000, $8CE10003, $98143BC8, $F08876, $A6D50896
                dc.l $A6C00916, $A6C41996, $7CC4D441, $E4002C00, $15BED5
                dc.l $A161821, $D294E400, $98003F64, $F0A404, $A009814
                dc.l $FFFF0000, $981500FF, $9816, $80000, $98173C10, $F09818
                dc.l $3C5400F0, $A46A0903, $A46B0883, $A46C0883, $7CECD518
                dc.l $898118E1, $A40263C1, $19020022, $A44CD300, $7C6CD461
                dc.l $894C650C, $D300894F, $660A26AA, $14B9D6C, $82B9D6D
                dc.l $650F118D, $26AF630C, $45ED01AC, $B88C0844, $1836D2E1
                dc.l $E4009800, $3F6C00F0, $AEF6981A, $3F0000, $98020100
                dc.l $8AD7, $8AD90059, $570059, $A418A2E1, $98023DAA, $F00857
                dc.l $7C01D042, $6201A2E2, $28419800, $3F0000, $134063A0
                dc.l $880263C0, $409802, $3F7000F0, $A45B0360, $8FDB0360
                dc.l $1982A448, $883C8B35, $88048910, $8B930A10, $661C6433
                dc.l $A108D03, $98057FFF, $9807, $3CE800F0, $24B388BB
                dc.l $63FB980A, $3CF800F0, $98118000, $2E52, $341CD142
                dc.l $E400A101, $12214425, $6E0500B2, $351CD442, $643CA21B
                dc.l $8501823, $D0E10848, $43736613, $46726DF2, $7C12D434
                dc.l $2322A52, $C6D28A45, $AEE60864, $9C890824, $A0800844
                dc.l $63C0A488, $6890A, $2CE7642A, $34E9D442, $3CE98C27
                dc.l $98003D56, $F07C49, $D002E400, $40A86608, $98003D90
                dc.l $F0D000, $E40088BE, $8C1B8B7C, $5808079B, $5805079B
                dc.l $88BC411C, $891D661C, $661D40BD, $660503BC, $88BD411D
                dc.l $660803BC, $40A86208, $388341B, $D4428BC5, $20087C15
                dc.l $D461E400, $7C27D441, $E4001148, $7C15D462, $E400A6A0
                dc.l $8BCC8, $AEC02800, $BB000858, $8570899, $8969800
                dc.l $3C7E00F0, $183AD014, $E400AF20, $AF021822, $98033A9E
                dc.l $F02842, $C702D074, $E4009802, $3DEC00F0, $98010000
                dc.l $39800, $3E8200F0, $2842C6A2, $D000E400, $8C209801
                dc.l $3F5000F0, $BC209800, $3A7E00F0, $D000E400, $98142238
                dc.l $F0A695, $3415D7A2, $E4009814, $220000F0, $98152224
                dc.l $F08839, $981AFFF8, $FFFF8B38, $27591338, $BE990894
                dc.l $98164020, $BE96, $9146438, $BE988819, $88172759
                dc.l $1337BEB9, $895BEB6, $9156437, $BEB76422, $9819223C
                dc.l $F03A02, $BF229816, $10180, $2AF8D482, $E4009816
                dc.l $50180, $98142238, $F0BE96, $A6953415, $D7A2E400
                dc.l $AEB4D280, $E4009063, $91CE8824, $88256204, $66056604
                dc.l $98003F5C, $F0BC04, $98003000, $F09801, $3EF800F0
                dc.l $A4227845, $D002E400, $BC259800, $3F4800F0, $A40463C5
                dc.l $85A4A4, $A4810884, $A4820884, $8880AEA4, $98053EE4
                dc.l $F09806, $3E0000F0, $98148000, $28A5, $281C6A5, $D0C0E400
                dc.l $2884C6A4, $98003000, $F0D000, $E4000000, 0
                dc.l $FFFF0000, 0
psi:   dc.l G_RAM, $71C, $98003F60, $F0980E, $3F8000F0, $A4010B81
                                        ; DATA XREF: ROM:001A69CC↑o
                dc.l $A4200A01, $A4220882, $C7C29818, $FC0000, $9801B034
                dc.l $F10018, $980AB130, $F19802, $3F0000, $98043040, $F09805
                dc.l $30000000, $88348C75, $2ED6A697, $8941835, $D79402F6
                dc.l $64568AC6, $A4072F39, $881A089A, $A75B089A, $A75C7B67
                dc.l $D4947B7C, $D454E400, $8C390881, $BF1910A7, $D4540898
                dc.l $2CE778C7, $D454E400, $88C7BC07, $1822D094, $8809803
                dc.l $365A00F0, $CC1ED060, $8DE9800, $3F6000F0, $A4010801
                dc.l $981A420, $881A422, $A0437C03, $D4C2E400, $98013176
                dc.l $F0D020, $E4008C21, $B8019800, $3F6000F0, $A4010801
                dc.l $981A420, $8409801, $314A00F0, $A0027C02, $D022E400
                dc.l $7D02D481, $E4009803, $341A00F0, $7CC2D481, $E4009803
                dc.l $33E400F0, $7CE2D481, $E4009803, $33AE00F0, $7C62D481
                dc.l $E4009803, $322E00F0, $7C42D481, $E4009803, $318E00F0
                dc.l $7CA2D481, $E4009803, $34AC00F0, $7C22D481, $E4009803
                dc.l $351200F0, $CC1ED060, $8DE9800, $3F6000F0, $A4010801
                dc.l $981A420, $8402C21, $B8019800, $3F6000F0, $A4010801
                dc.l $981A420, $A0017C01, $D4C2E400, $980030BE, $F0D000
                dc.l $E400980E, $3F8000F0, $AEA0D000, $E4007C23, $D702E400
                dc.l $2C639801, $211400F0, $A4203C00, $BC20D7E0, $E4002EB5
                dc.l $980E3F80, $F0AFC0, $A4040880, $A4050880, $A4060880
                dc.l $8649C89, $824A080, $84463C0, $A4880006, $890A2CE7
                dc.l $642A34E9, $D4423CE9, $8C279800, $31DC00F0, $7C49D002
                dc.l $E40040A8, $66089800, $321600F0, $D000E400, $90BF8C1B
                dc.l $8B7C5808, $79B5805, $79B88BC, $411C891D, $661C661D
                dc.l $40BD6605, $3BC88BD, $411D6608, $3BC40A8, $62080388
                dc.l $341BD442, $97E52008, $7C15D461, $E4007C27, $D441E400
                dc.l $11487C15, $D462E400, $A6A00008, $BCC8D3C0, $E400980E
                dc.l $3F8000F0, $AFC0A401, $8809803, $220000F0, $A402BC61
                dc.l $A0490842, $A04A0842, $A04B0842, $A04C0842, $A0539814
                dc.l $223800F0, $9816220C, $F09815, $223C00F0, $98004420
                dc.l $39801, $220400F0, $BC20981C, $32E400F0, $CC1FD380
                dc.l $8DF8A60, $62002A60, $980332E0, $F088FC, $3E1C7C1C
                dc.l $D062E400, $98012268, $F0BC20, $881BC20, $98012220
                dc.l $F0BC28, $9801221C, $F0BC25, $BEC68C00, $98012218
                dc.l $F0BC20, $88E08801, $980201FF, $2441, $3A01BEA1, $98000000
                dc.l $1BE80, $A69C341C, $D7A2E400, $D3C0E400, $112B114C
                dc.l $89466206, $29268960, $89815800, $58017801, $D4D8E400
                dc.l $98003354, $F0D000, $E400981C, $33A800F0, $7C00D382
                dc.l $E400620C, $8C03786C, $D474E400, $3803580C, $540C8807
                dc.l $8273A07, $3403E400, $D442E400, $200C8988, $62089803
                dc.l $FFFF, $89852465, $8C207C0B, $D494E400, $9800FFFF
                dc.l $2805, $D3E0E400, $981C33A8, $F07C01, $D382E400, $620B8C03
                dc.l $786BD474, $E4003803, $580B542B, $88270827, $3A073403
                dc.l $E400D442, $E400200B, $89689803, $FFFF0000, $24688965
                dc.l $66059800, SRCEN, $7C0CE400, $D494E400, $98000000, $FFFF2805
                dc.l $D3E0E400, $8C07D3E0, $E4009801, $345C00F0, $CC1FD020
                dc.l $8DF9813, $33DA00F0, $981433C8, $F088D1, $89C0082E
                dc.l $256088F2, $1009C01, $1032D180, $E4001824, $D2940831
                dc.l $D3C0E400, $9801345C, $F0CC1F, $D02008DF, $98133410
                dc.l $F09814, $33FE00F0, $88F289C0, $82E2560, $88D10100
                dc.l $9C010031, $D180E400, $1824D294, $1832D3C0, $E4009801
                dc.l $345C00F0, $CC1FD020, $8DF9813, $345200F0, $98143438
                dc.l $F0981A, padleft, $89C0082E, $256088D1, $10088F2
                dc.l $9C010340, $319C01, $1032D180, $E4001824, $D294E400
                dc.l $D3C0E400, $980E3F80, $F0AFC0, $A4170880, $A4080880
                dc.l $A4070880, $88E66607, $980DFFFF, $980B, $3F0000, $25A6A40E
                dc.l $89C5256E, $6605980A, $3000000, $980F017F, $981B
                dc.l $FFFF0000, $9804003F, $980C, $362600F0, $D3E0E400
                dc.l $9801345C, $F0CC1F, $D02008DF, $A50E0888, $A51C0888
                dc.l $A5089813, $350600F0, $981434DC, $F0980B, $FF0000
                dc.l $98058800, $88D1, $89C0038E, $66002560, $88F20100
                dc.l $9C016441, $10329231, $98183F60, $F0A719, $899A738
                dc.l $A3195F31, $5F32D180, $E4009631, $1824D294, $831D3C0
                dc.l $E4009800, $3F4000F0, $A4179806, 0
                dc.l $980700C0, $9805, 0
                dc.l $981BFFFF, $980A, $3000000, $980F017F, $9808, $4000000
                dc.l $980E3F80, $F0AFC0, $2F9CA408, $A1169804, $FFFF0000
                dc.l $6456980C, $350E00F0, $981333AA, $F09800, $360A00F0
                dc.l $CC1FD000, $8DF02DC, $8267B84, $D4D8E400, $98003566
                dc.l $F0D000, $E4009805, $F0000000, $848889C, $A1160848
                dc.l $6456A104, $9800360A, $F0CC1F, $D00008DF, $12DC0826
                dc.l $7B84D4D4, $E4009800, $359800F0, $D000E400, $889C8E09
                dc.l $98058F00, $9800, $360A00F0, $CC1FD000, $8DF0886
                dc.l $1829D4D8, $E4009800, $35C200F0, $D000E400, $848A116
                dc.l $98058000, $6456, $9800360A, $F0CC1F, $D00008DF, $82612DC
                dc.l $D198E400, $980035EC, $F0D000, $E40078C8, $D19888D1
                dc.l $98000040, $6431, $981200E0, $11, $8B816541, $103279F1
                dc.l $D27479F2, $D2747C11, $D2787C12, $D2788A21, $8A4263E1
                dc.l $41420022, $2E28B60, $650028A0, $B8400842, $B8400142
                dc.l $B8401842, $B840D260, $E4009800, $3F6000F0, $A4010B81
                dc.l $A4240901, $A4290881, $A42A9813, bbutton, $8C8B980C
                dc.l $369000F0, $980D36AA, $F09800, $3F3C00F0, $A4142E31
                dc.l $65148C3C, $A1252CE7, $849A126, $88C80849, $10A8D474
                dc.l $E4002008, $88C563C5, $85A4A0, $78E0D458, $8858807
                dc.l $1828D734, $E40063E7, $8407A152, $127278F2, $D454E400
                dc.l $88F2B952, $981D0300, $A120, $849A121, $2C4278E0
                dc.l $D4B4E400, $2B8288E0, $D4C0E400, $2C4213A0, $D454E400
                dc.l $2C0078E1, $D458E400, $2C422851, $2C541849, $B9200889
                dc.l $63FC182B, $D194084A, $63149800, $3F3C00F0, $2A91BC11
                dc.l $D3C0E400, 0
delta:   dc.l G_RAM, $530, $98000098, _i4, $A4028C64, $98003F60
                                        ; DATA XREF: ROM:001A69D0↑o
                dc.l $F06602, $A4012482, $A42063C2, $40A418, $980E3F80
                dc.l $F09800, $3F5C00F0, $A4017C61, $D4C1E400, $98003380
                dc.l $F0D000, $E400A310, $980A0300, $858, $98003F40, $F0A313
                dc.l $980F017E, $858, $A4179800, $240000, $60A411, $98000028
                dc.l _i4, $A4126611, $66128A00, $8A616380, $64211011, $10329800
                dc.l $540000, $60A41B, $980000B8, _i4, $A409651B, $980000BC
                dc.l _i4, $A40B9800, $D80000, $60A40C, $980000DC, _i4
                dc.l $A40D9800, $E00000, $60A407, $980000E4, _i4, $A4088A54
                dc.l $8A214154, $63E102F4, $349294, $90E79108, $92108FF6
                dc.l $A71A9800, $3F5C00F0, $A4017C01, $D4C1E400, $9800312C
                dc.l $F0D000, $E4007C41, $D4C1E400, $98003284, $F0D000
                dc.l $E4007C81, $D4C1E400, $9800341E, $F0D000, $E4009819
                dc.l $324E00F0, $9815322E, $F09800, $31E200F0, $D000E400
                dc.l $980000F0, _i4, $A4019800, $F40000, $60A402, $980031DC
                dc.l $F07C02, $D4822441, $7841D001, $E4006C8C, $6C8D6C49
                dc.l $6C4B981C, padleft, $981DFFFF, $1F9819, $31AC00F0
                dc.l $98153172, $F037FA, $D32288E2, $89018840, $8BBE881F
                dc.l $882527DF, $27C5643E, $13DFD454, $E400201F, $13C5D454
                dc.l $E4002005, $88BB661F, $6605639F, $2BE565BB, $6305039B
                dc.l $2B65BA85, $85463FA, $1271836, $D2B40168, $8FF60898
                dc.l $A71A1830, $D2B4E400, $94E79508, $96949610, $18701A8
                dc.l $15490E7, $91081833, $D2B49294, $AEA0D000, $E4006C4B
                dc.l $6C8D88E2, $89018840, $8FDE881F, $8825661F, $660527DF
                dc.l $27C5643E, $13DFD454, $E400201F, $13C5D454, $E4002005
                dc.l $639F2BE5, $63059800, $540000, $60A41C, $631C981D
                dc.l $FFFE01FF, $981F0040, $939C, $912937FA, $D3228B82
                dc.l $88408BBE, $881B27DB, $643E13DB, $D454E400, $201B663B
                dc.l $3FB28BB, $BA9B0854, $16963FA, $13C1836, $D2B4E400
                dc.l $8FF60898, $A71A1830, $D2B4E400, $9529979C, $969401AC
                dc.l $9610019C, $154939C, $1833D2B4, $9294AEA0, $D000E400
                dc.l $98153310, $F09800, $240000, $60A411, $98000028, _i4
                dc.l $A4126611, $66129800, $840000, $60A410, $98000088
                dc.l _i4, $A4136510, $65138A00, $8A616420, $64211011, $10328A54
                dc.l $8A214154, $63E102F4, $349294, $92106C8C, $6C8D6C49
                dc.l $6C4B981C, padleft, $981DFFFF, $1F9800, allpad, $60A401
                dc.l $980000F4, _i4, $A4026501, $980031DC, $F07C02, $D442E400
                dc.l $24229042, $88E28901, $88408BBE, $881F8825, $27DF27C5
                dc.l $643E13DF, $D454E400, $201F13C5, $D454E400, $200588BB
                dc.l $661F6605, $639F2BE5, $65BB9801, $FF0000, $94427C02
                dc.l $D442E400, $2C256305, $39B2B65, $BA850854, $1270168
                dc.l $1830D2B4, $E40094E7, $95089694, $96100187, $1A80154
                dc.l $90E79108, $1833D2B4, $9294AEA0, $D000E400, $98003F60
                dc.l $F0A401, $901A422, $980000C0, 2, $98000024, _i4, $A4049800
                dc.l $280000, $60A405, $63E5BC44, $882BC45, $9029800, $50000
                dc.l $B8409800, $540000, $60A41B, $651B9800, $E00000, $60A407
                dc.l $980000E4, _i4, $A408981D, $FFFF001F, $890188E0, $8BBE881F
                dc.l $882527DF, $27C5643E, $13DFD454, $E400201F, $13C5D454
                dc.l $E4002005, $661F6605, $639F2BE5, $63059800, $3F6000F0
                dc.l $2B65A401, $1881B825, $AEA0D000, $E4006C8C, $6C8D6C49
                dc.l $6C4B981C, padleft, $981DFFFF, $1F9815, $34B400F0
                dc.l $980000A4, _i4, $A4169800, $240000, $60A411, $98000028
                dc.l _i4, $A4126611, $66129800, $840000, $60A410, $98000088
                dc.l _i4, $A41364F0, $65138A00, $8A616420, $64211011, $10328A54
                dc.l $8A214154, $63E102F4, $349294, $92109800, allpad
                dc.l $60A401, $980000F4, _i4, $A4026501, $980031DC, $F07C02
                dc.l $D442E400, $24229042, $88E28901, $88408BBE, $881F8825
                dc.l $27DF27C5, $643E13DF, $D454E400, $201F13C5, $D454E400
                dc.l $200588BB, $659F6585, $2DF02C5, $9FE09CA1, $64806481
                dc.l $63802801, $980000FF, $9442, $7C02D442, $E4002C01
                dc.l $65BB6301, $39B2B61, $BA810854, $1270168, $1830D2B4
                dc.l $E40094E7, $95089694, $96100187, $1A80154, $90E79108
                dc.l $1833D2B4, $9294AEA0, $D000E400
epsilon:   dc.l G_RAM, $9E0, $98003F5C, $F0980E, $3F8000F0, $A4019800
                                        ; DATA XREF: ROM:001A69D4↑o
                dc.l $480000, $60A401, $9800004C, _i4, $A402BDC1, $C4229800
                dc.l $500000, $60A401, $980000A4, _i4, $A402C441, $C4622C21
                dc.l $C4819800, 0
                dc.l $60A417, $980000A0, _i4, $A416980A, $3000000, $980F017E
                dc.l $980D, $FF0000, $980000E8, _i4, $A40163C1, $980039C0
                dc.l OLP, $A401C621, $E4009800, $E00000, $60A401, $980000E4
                dc.l _i4, $A4028840, $981EFFFF, $1F881F, $882527DF, $27C5643E
                dc.l $13DFD454, $E400201F, $13C5D454, $E4002005, $661F6605
                dc.l $639F2BE5, $63059800, $7C0000, $60A414, $98090000
                dc.l $40AC62, $8A80981D, padright, $660025A0, $409C1E, $8209C1A
                dc.l $13DA8A82, $13BE6502, $631E25A2, $445A03DA, $93540134
                dc.l $AC628A80, $981D0080, $6600, $25A00040, $9C1E0820
                dc.l $9C1A13DA, $8A8213BE, $6502631E, $25A2445A, $3DA9355
                dc.l $98000074, _i4, $A4146394, $AC628A80, $981D0080, $6600
                dc.l $25A00040, $9C1E0820, $9C1A13DA, $8A8213BE, $6502631E
                dc.l $25A2445A, $3DA9352, $134AC62, $8A80981D, padright
                dc.l $660025A0, $409C1E, $8209C1A, $13DA8A82, $13BE6502
                dc.l $631E25A2, $445A03DA, $93539800, $700000, $60A414
                dc.l $AC628A80, $981D0080, $6600, $25A00040, $9C1E0820
                dc.l $9C1A13DA, $8A8213BE, $6502631E, $25A2445A, $3DA8B55
                dc.l $134AC62, $8A80981D, padright, $660025A0, $409C1E
                dc.l $8209C1A, $13DA8A82, $13BE6502, $631E25A2, $445A03DA
                dc.l $8B5A9800, $980000, $60A402, $8C216602, $98003F60
                dc.l $F02422, $A40163C2, $981A420, $40A40B, $A1689800
                dc.l $357800F0, $84B2800, $C5E09800, $240000, $60A406
                dc.l $98000028, _i4, $A4079800, $340000, $60A418, $980000EC
                dc.l _i4, $A4016601, $2821C761, $98000000, $C01006, $10079813
                dc.l $327E00F0, $98000054, _i4, $A41B9800, $9C0000, $60A41C
                dc.l $98000084, _i4, $A4099800, $880000, $60A414, $9800006C
                dc.l _i4, $A404184B, $A5689811, $358400F0, $88B7C08, $D238E400
                dc.l $A171088B, $A172088B, $A179088B, $45314499, $46926311
                dc.l $63126319, $A165084B, $A17B0A4B, $7C1BD4C1, $E4000828
                dc.l $98003578, $F0D000, $8B5C8AA0, $8ABE8B5F, $92208C01
                dc.l $8822581C, $4415811, $4418A22, $43828B9D, $6602661D
                dc.l $423D6611, $3A28A3D, $439D661C, $3A2423C, $621C005C
                dc.l $3401D442, $9411201C, $92408C01, $8822581E, $4415812
                dc.l $4418A42, $43C28BDD, $6602661D, $425D6612, $3A28A5D
                dc.l $43DD661E, $3A2425E, $621E005E, $3401D442, $9412201E
                dc.l $92208C01, $88225800, $4415811, $4418A22, $4002881D
                dc.l $6602661D, $423D6611, $3A28A3D, $401D6600, $3A24220
                dc.l $62000040, $3401D442, $94112000, $92408C01, $8822581F
                dc.l $4415812, $4418A42, $43E28BFD, $6602661D, $425D6612
                dc.l $3A28A5D, $43FD661F, $3A2425F, $621F005F, $3401D442
                dc.l $9412201F, $13DC03E0, $8B918812, $96BC9680, $969E96BF
                dc.l $93208C01, $8822581C, $4415819, $4418B22, $43828B9D
                dc.l $6602661D, $433D6619, $3A28B3D, $439D661C, $3A2433C
                dc.l $621C005C, $3401D442, $9419201C, $92408C01, $8822581E
                dc.l $4415812, $4418A42, $43C28BDD, $6602661D, $425D6612
                dc.l $3A28A5D, $43DD661E, $3A2425E, $621E005E, $3401D442
                dc.l $9412201E, $93208C01, $88225800, $4415819, $4418B22
                dc.l $4002881D, $6602661D, $433D6619, $3A28B3D, $401D6600
                dc.l $3A24320, $62000040, $3401D442, $94192000, $92408C01
                dc.l $8822581F, $4415812, $4418A42, $43E28BFD, $6602661D
                dc.l $425D6612, $3A28A5D, $43FD661F, $3A2425F, $621F005F
                dc.l $3401D442, $9412201F, $13DC03E0, $8B998812, $967C9640
                dc.l $965E967F, $92208C01, $8822581C, $4415811, $4418A22
                dc.l $43828B9D, $6602661D, $423D6611, $3A28A3D, $439D661C
                dc.l $3A2423C, $621C005C, $3401D442, $9411201C, $93208C01
                dc.l $8822581E, $4415819, $4418B22, $43C28BDD, $6602661D
                dc.l $433D6619, $3A28B3D, $43DD661E, $3A2433E, $621E005E
                dc.l $3401D442, $9419201E, $92208C01, $88225800, $4415811
                dc.l $4418A22, $4002881D, $6602661D, $423D6611, $3A28A3D
                dc.l $401D6600, $3A24220, $62000040, $3401D442, $94112000
                dc.l $93208C01, $8822581F, $4415819, $4418B22, $43E28BFD
                dc.l $6602661D, $433D6619, $3A28B3D, $43FD661F, $3A2433F
                dc.l $621F005F, $3401D442, $9419201F, $13DC03E0, $8B918819
                dc.l $88C088E1, $8B020011, $320059, $ADF3980C, $396C00F0
                dc.l $D180E400, $98003290, $F01828, $D014E400, $AEA09801
                dc.l $3F5800F0, $A4223402, $D002E400, $98000038, _i4, $A4019800
                dc.l $940000, $60A415, $98000040, _i4, $A4026D81, $6D826D95
                dc.l $290054, $2A49800, $840000, $60BC09, $98000088, _i4
                dc.l $BC149800, $6C0000, $60BC04, $98000058, _i4, $A4016201
                dc.l $6E61003B, $98000054, _i4, $BC1BAEA0, $D000E400, $981F37BE
                dc.l $F03616, $D481E400, $ADF3D3E0, $E400C545, $65059801
                dc.l $F0000, $88A06485, $24206205, $6200C565, $C5809800
                dc.l $480000, $60A401, $9800004C, _i4, $A402BDC1, $C4229800
                dc.l $500000, $60A401, $980000A4, _i4, $A402C441, $C4629800
                dc.l $780000, $60A401, $C481981D, padright, $980000A8, _i4
                dc.l $A4019800, $AC0000, $60A402, $C5A1C5C2, $6E016E02
                dc.l $10311052, $C4B1C4D2, $AC20AC81, $AC420001, $40C420
                dc.l $AC62C481, $660125A1, $88200801, $400801, $9C1E0820
                dc.l $9C1313D3, $AC9F13BE, $651F631E, $25BF47F3, $27E25A1
                dc.l $8BC00041, $46409C22, $8219C33, $105313A2, $630247F3
                dc.l $262463E, $44514452, $101103D2, $6DF16DF2, $980000B0
                dc.l _i4, $A4014431, $44326D91, $6D92ADA1, $ADC26E01, $6E020031
                dc.l $52AD60, $AD818FDE, $881F8825, $661F6605, $27DF27C5
                dc.l $643E13DF, $D454E400, $201F13C5, $D454E400, $2005639F
                dc.l $2BE56305, $881F9800, $B80000, $60A41E, $3DFC57F
                dc.l $980000BC, _i4, $A41E03C1, $C581981F, $37BE00F0, $9813375A
                dc.l $F0D3E0, $E400ACB1, $ACD29800, $B40000, $60A401, $980000C0
                dc.l _i4, $A402981E, $10000000, $3C103C2, $44314452, $6D916D92
                dc.l $ADA1ADC2, $980000C4, _i4, $A41E03C1, $C5A19800, $C80000
                dc.l $60A41E, $3C2C5C2, $A5C09801, $368800F0, $1820D478
                dc.l $BDC0D020, $E400ADF3, $AD45D260, $E400981F, $384400F0
                dc.l $C4F3C536, $3416D4C2, $E4009813, $37D800F0, $D3E0E400
                dc.l $89E0981F, $384400F0, $12208811, $3436D4C2, $E4009813
                dc.l $37F400F0, $D3E0E400, $89E01240, $88123456, $D4C2E400
                dc.l $9813380A, $F0D3E0, $E40089E0, $12209813, $382000F0
                dc.l $88113476, $D462E400, $D3E0E400, $89E01220, $64968A51
                dc.l $88123496, $D4A2E400, $ACF3AD36, $D260E400, $39169800
                dc.l $37C800F0, $D000E400, $AE20D000
                dc.l $E40079F1, $D27479F2, $D2747C11, $D2787C12, $D2788A21
                dc.l $8A4263E1, $41420022, $2E28B60, $650028A0, $B840D260
                dc.l $E40079F1, $D27479F2, $D2747C11, $D2787C12, $D2788A21
                dc.l $8A4263E1, $41420022, $2E28B60, $650028A0, $B8400842
                dc.l $B8400142, $B8401842, $B840D260, $E4009800, $EC0000
                dc.l $60A401, $9800384A, $F06601, $98023872, $F00821, $7C21D002
                dc.l $E4007C41, $D042E400, $98002238, $F0A402, $3402D7A2
                dc.l $E4009800, $220000F0, $BC179802, $44200001, $880BC02
                dc.l $89F01030, $8A3C8822, $6422105C, $7C1CD278, $7B90D278
                dc.l $8B808A5C, $6200105C, $7C1CD278, $7B90D278, $6600621C
                dc.l $281C9800, $220C00F0, $BC1C8822, $20020880, $62026602
                dc.l $3A028B7C, $BC02651C, $9800226C, $F028BC, $8B82621C
                dc.l $285CBC1C, $1880BC1C, $88229800, $223C00F0, $62012841
                dc.l $BC019802, $2000001, $1880BC02, $A4013401, $D7A2E400
                dc.l $D260E400, $62516252, $6DD97C39, $D2788A3D, $58115731
                dc.l $37FDD442, $E4002011, $8A5D5812, $573237FD, $D442E400
                dc.l $20126CF1, $6CF29800, $C00000, $110012, $980035FC
                dc.l $F0D000, $E400AF60, $8B614001, $98000000, $7F2401
                dc.l $980038B0, $F0D000, $E4000000
                dcb.l 2,$F03872
                dc.l $F038A6, $F039A6
                dcb.l $04,0
theta:   dc.l G_RAM, $224, $980E3F80, $F09800, $3F6000F0, $A4010981
                                        ; DATA XREF: ROM:001A69D8↑o
                dc.l $A422A444, $A486888B, $98000024, _i4, $A4109800, $280000
                dc.l $60A411, $98010000, $C09800, $340000, $60A412, $10301031
                dc.l $98000038, _i4, $A4139800, padleft, $60A414, $98000094
                dc.l _i4, $A4156C93, $6C946C95, $980000B8, _i4, $A4169800
                dc.l $BC0000, $60A417, $980000CC, _i4, $A41863F6, $63F76C38
                dc.l $2D302F4, $3159801, $80000000, $9800005C, _i4, $A4169800
                dc.l $600000, $60A417, $98000064, _i4, $A4181036, $10371038
                dc.l $98000054, _i4, $A41A381A, $7C1AD441, $E4008C3A, $980000E4
                dc.l _i4, $A4019800, $E00000, $60A402, $981DFFFF, $1F8840
                dc.l $8BBE881F, $883927DF, $27D9643E, $13DFD454, $E400201F
                dc.l $13D9D454, $E4002019, $661F6619, $639F2BF9, $63199800
                dc.l $580000, $60A405, $8846485, $98073206, $F02D8C, $980131AE
                dc.l $F09800, YADD1, $60A402, $18229800, YADD1, $60BC02
                dc.l $7C02D034, $E4009800, padright, $60A402, $66029800
                dc.l YADD1, $60BC02, $981E01F4, $10DE, $D038E400, $826BD66
                dc.l $8889980A, $317400F0, $9C9A120, $7C00D462, $E400D140
                dc.l $A498921, $9800FFFF, $19C1, $B920BC30, $881BC31, $881BC32
                dc.l $881B839, $841B83A, $841BC33, $881BC34, $881BC35
                dc.l $A49981E, $1FF0000, $9C4A088, $7C08D0E2, $E4008880
                dc.l $888119C0, $841A434, $A4150295, $BC1502D4, $880BC34
                dc.l $881A434, $A4150295, $BC1502F4, $880BC34, $881A434
                dc.l $A4150295, $BC150314, $880BC34, $88110A8, $7C28D474
                dc.l $E4002D08, $82CB888, $18269800, $31B400F0, $A44183E
                dc.l $D4787C06, $D014E400, $A5601180, $BD60AEA0, $D000E400
                dc.l 0
sigma:   dc.l G_RAM, $9F8, $98003F5C, $F0980E, $3F8000F0, $A4019800
                                        ; DATA XREF: ROM:001A69DC↑o
                dc.l $480000, $60A401, $9800004C, _i4, $A402BDC1, $C4229800
                dc.l $500000, $60A401, $980000A4, _i4, $A402C441, $C4622C21
                dc.l $C4819800, 0
                dc.l $60A417, $980000A0, _i4, $A416980A, $3000000, $980F017E
                dc.l $980D, $FF0000, $980000E8, _i4, $A40163C1, $980039D8
                dc.l OLP, $A401C621, $E4009800, $E00000, $60A401, $980000E4
                dc.l _i4, $A4028840, $981EFFFF, $1F881F, $882527DF, $27C5643E
                dc.l $13DFD454, $E400201F, $13C5D454, $E4002005, $661F6605
                dc.l $639F2BE5, $63059800, $7C0000, $60A414, $98090000
                dc.l $40AC62, $8A80981D, padright, $660025A0, $409C1E, $8209C1A
                dc.l $13DA8A82, $13BE6502, $631E25A2, $445A03DA, $93540134
                dc.l $AC628A80, $981D0080, $6600, $25A00040, $9C1E0820
                dc.l $9C1A13DA, $8A8213BE, $6502631E, $25A2445A, $3DA9355
                dc.l $98000074, _i4, $A4146394, $AC628A80, $981D0080, $6600
                dc.l $25A00040, $9C1E0820, $9C1A13DA, $8A8213BE, $6502631E
                dc.l $25A2445A, $3DA9352, $134AC62, $8A80981D, padright
                dc.l $660025A0, $409C1E, $8209C1A, $13DA8A82, $13BE6502
                dc.l $631E25A2, $445A03DA, $93539800, $700000, $60A414
                dc.l $AC628A80, $981D0080, $6600, $25A00040, $9C1E0820
                dc.l $9C1A13DA, $8A8213BE, $6502631E, $25A2445A, $3DA8B55
                dc.l $134AC62, $8A80981D, padright, $660025A0, $409C1E
                dc.l $8209C1A, $13DA8A82, $13BE6502, $631E25A2, $445A03DA
                dc.l $8B5A9800, $980000, $60A402, $8C216602, $98003F60
                dc.l $F02422, $A40163C2, $A01A420, $40A40B, $A1689800
                dc.l $354E00F0, $84B2800, $C5E09800, $240000, $60A406
                dc.l $98000028, _i4, $A4079800, $340000, $60A418, $980000EC
                dc.l _i4, $A4016601, $2821C761, $98000000, $C01006, $10079813
                dc.l $354E00F0, $98000054, _i4, $A41B9800, $9C0000, $60A41C
                dc.l $98000084, _i4, $A4099800, $880000, $60A414, $9800006C
                dc.l _i4, $A404A171, $84BA172, $84BA179, $84B4531, $44994692
                dc.l $63116312, $63198B5C, $8AA08ABE, $8B5F9220, $8C018822
                dc.l $581C0441, $58110441, $8A224382, $8B9D6602, $661D423D
                dc.l $661103A2, $8A3D439D, $661C03A2, $423C621C, $5C3401
                dc.l $D4429411, $201C9240, $8C018822, $581E0441, $58120441
                dc.l $8A4243C2, $8BDD6602, $661D425D, $661203A2, $8A5D43DD
                dc.l $661E03A2, $425E621E, $5E3401, $D4429412, $201E9220
                dc.l $8C018822, $58000441, $58110441, $8A224002, $881D6602
                dc.l $661D423D, $661103A2, $8A3D401D, $660003A2, $42206200
                dc.l $403401, $D4429411, $20009240, $8C018822, $581F0441
                dc.l $58120441, $8A4243E2, $8BFD6602, $661D425D, $661203A2
                dc.l $8A5D43FD, $661F03A2, $425F621F, $5F3401, $D4429412
                dc.l $201F13DC, $3E08B91, $881296BC, $9680969E, $96BF9320
                dc.l $8C018822, $581C0441, $58190441, $8B224382, $8B9D6602
                dc.l $661D433D, $661903A2, $8B3D439D, $661C03A2, $433C621C
                dc.l $5C3401, $D4429419, $201C9240, $8C018822, $581E0441
                dc.l $58120441, $8A4243C2, $8BDD6602, $661D425D, $661203A2
                dc.l $8A5D43DD, $661E03A2, $425E621E, $5E3401, $D4429412
                dc.l $201E9320, $8C018822, $58000441, $58190441, $8B224002
                dc.l $881D6602, $661D433D, $661903A2, $8B3D401D, $660003A2
                dc.l $43206200, $403401, $D4429419, $20009240, $8C018822
                dc.l $581F0441, $58120441, $8A4243E2, $8BFD6602, $661D425D
                dc.l $661203A2, $8A5D43FD, $661F03A2, $425F621F, $5F3401
                dc.l $D4429412, $201F13DC, $3E08B99, $8812967C, $9640965E
                dc.l $967F9320, $8C018822, $581C0441, $58190441, $8B224382
                dc.l $8B9D6602, $661D433D, $661903A2, $8B3D439D, $661C03A2
                dc.l $433C621C, $5C3401, $D4429419, $201C9220, $8C018822
                dc.l $581E0441, $58110441, $8A2243C2, $8BDD6602, $661D423D
                dc.l $661103A2, $8A3D43DD, $661E03A2, $423E621E, $5E3401
                dc.l $D4429411, $201E9320, $8C018822, $58000441, $58190441
                dc.l $8B224002, $881D6602, $661D433D, $661903A2, $8B3D401D
                dc.l $660003A2, $43206200, $403401, $D4429419, $20009220
                dc.l $8C018822, $581F0441, $58110441, $8A2243E2, $8BFD6602
                dc.l $661D423D, $661103A2, $8A3D43FD, $661F03A2, $423F621F
                dc.l $5F3401, $D4429411, $201F13DC, $3E08B99, $881188C0
                dc.l $88E18B02, $110032, $59ADF3, $980C3982, $F0D180, $E4009800
                dc.l $327E00F0, $1828D014, $E400AEA0, $98013F58, $F0A422
                dc.l $3402D002, $E4009800, $380000, $60A401, $98000094
                dc.l _i4, $A4159800, padleft, $60A402, $6D816D82, $6D950029
                dc.l $5402A4, $98000084, _i4, $BC099800, $880000, $60BC14
                dc.l $9800006C, _i4, $BC049800, $E00000, $60A401, $980000E4
                dc.l _i4, $A4029800, $B80000, $60A414, $980000BC, _i4
                dc.l $A4150281, $2A29800, $E00000, $60BC01, $980000E4
                dc.l _i4, $BC029800, $580000, $60A401, $62016E61, $3B9800
                dc.l $540000, $60BC1B, $AEA0D000, $E400981F, $37D400F0
                dc.l $3616D481, $E400ADF3, $D3E0E400, $C5456505, $9801000F
                dc.l $88A0, $64852420, $62056200, $C565C580, $98000048
                dc.l _i4, $A4019800, $4C0000, $60A402, $BDC1C422, $98000050
                dc.l _i4, $A4019800, $A40000, $60A402, $C441C462, $98000078
                dc.l _i4, $A401C481, $981D0080, $9800, $A80000, $60A401
                dc.l $980000AC, _i4, $A402C5A1, $C5C26E01, $6E021031, $1052C4B1
                dc.l $C4D2AC20, $AC81AC42, $10040, $C420AC62, $C4816601
                dc.l $25A18820, $8010040, $8019C1E, $8209C13, $13D3AC9F
                dc.l $13BE651F, $631E25BF, $47F3027E, $25A18BC0, $414640
                dc.l $9C220821, $9C331053, $13A26302, $47F30262, $463E4451
                dc.l $44521011, $3D26DF1, $6DF29800, $B00000, $60A401
                dc.l $44314432, $6D916D92, $ADA1ADC2, $6E016E02, $310052
                dc.l $AD60AD81, $8FDE881F, $8825661F, $660527DF, $27C5643E
                dc.l $13DFD454, $E400201F, $13C5D454, $E4002005, $639F2BE5
                dc.l $6305881F, $980000B8, _i4, $A41E03DF, $C57F9800, $BC0000
                dc.l $60A41E, $3C1C581, $981F37D4, $F09813, $377000F0
                dc.l $D3E0E400, $ACB1ACD2, $980000B4, _i4, $A4019800, $C00000
                dc.l $60A402, $981E1000, $3C1, $3C24431, $44526D91, $6D92ADA1
                dc.l $ADC29800, $C40000, $60A41E, $3C1C5A1, $980000C8
                dc.l _i4, $A41E03C2, $C5C2A5C0, $9801369E, $F01820, $D478BDC0
                dc.l $D020E400, $ADF3AD45, $D260E400, $981F385A, $F0C4F3
                dc.l $C5363416, $D4C2E400, $981337EE, $F0D3E0, $E40089E0
                dc.l $981F385A, $F01220, $88113436, $D4C2E400, $9813380A
                dc.l $F0D3E0, $E40089E0, $12408812, $3456D4C2, $E4009813
                dc.l $382000F0, $D3E0E400, $89E01220, $98133836, $F08811
                dc.l $3476D462, $E400D3E0, $E40089E0, $12206496, $8A518812
                dc.l $3496D4A2, $E400ACF3
                dc.l $AD36D260, $E4003916, $980037DE, $F0D000, $E400AE20
                dc.l $D000E400, $79F1D274, $79F2D274, $7C11D278, $7C12D278
                dc.l $8A218A42, $63E14142, $2202E2, $8B606500, $28A0B840
                dc.l $D260E400, $79F1D274, $79F2D274, $7C11D278, $7C12D278
                dc.l $8A218A42, $63E14142, $2202E2, $8B606500, $28A0B840
                dc.l $842B840, $142B840, $1842B840, $D260E400, $980000EC
                dc.l _i4, $A4019800, $386000F0, $66019802, $388800F0, $8217C21
                dc.l $D002E400, $7C41D042, $E4009800, $223800F0, $A4023402
                dc.l $D7A2E400, $98002200, $F0BC17, $98024420, $10880
                dc.l $BC0289F0, $10308A3C, $88226422, $105C7C1C, $D2787B90
                dc.l $D2788B80, $8A5C6200, $105C7C1C, $D2787B90, $D2786600
                dc.l $621C281C, $9800220C, $F0BC1C, $88222002, $8806202
                dc.l $66023A02, $8B7CBC02, $651C9800, $226C00F0, $28BC8B82
                dc.l $621C285C, $BC1C1880, $BC1C8822, $9800223C, $F06201
                dc.l $2841BC01, $98020200, $11880, $BC02A401, $3401D7A2
                dc.l $E400D260, $E4006251, $62527C39, $D27865D9, $8A3D5811
                dc.l $573137FD, $D442E400, $20118A5D, $58125732, $37FDD442
                dc.l $E4002012, $6CF16CF2, $980000C0, $11, $129800, $361200F0
                dc.l $D000E400, $AF608B61, $40019800, $7F, $24019800, $38C600F0
                dc.l $D000E400, 0
                dcb.l 2,$F03888
                dc.l $F038BC, $F039BC
                dcb.l $04,0
tau:   dc.l G_RAM, $888, $98003F5C, $F0980E, $3F8000F0, $A4019800
                                        ; DATA XREF: ROM:001A69E0↑o
                dc.l $A40000, $60A402, $C4622C21, $C4819800, 0
                dc.l $60A417, $980A0300, $980F, $17E0000, $980D00FF, $E400
                dc.l $98000098, _i4, $A4046604, $98000003, $2404, $98003F60
                dc.l $F0A401, $A8163C4, $A4200080, $A41C9382, $98003076
                dc.l $F09801, $369C00F0, $C5E0D020, $E4009800, $33F800F0
                dc.l $2800C5E0, $981333F8, $F09800, $380000, $60A40B, $98000040
                dc.l _i4, $A4089800, $940000, $60A416, $9800FFFF, $1F240B
                dc.l $24082416, $660B6608, $66169164, $910592C6, $89718912
                dc.l $8AD96431, $64326439, $20112012, $2019922A, $924B932C
                dc.l $8901897E, $945C259E, $258163BE, $3C03DC, $9F908A00
                dc.l $63002A00, $88106200, $28103410, $D4A17430, $980033F8
                dc.l $F0D000, $AFA1AF80, $1600101, $981E001E, $881F, $882527DF
                dc.l $27C5643E, $13DFD454, $E400201F, $8BFB13C5, $D454E400
                dc.l $2005639F, $62BB2BE5, $39FB6305, $45314499, $46926311
                dc.l $63126319, $8B5C8AA0, $8ABE8B5F, $92208C01, $8822581C
                dc.l $4415811, $4418A22, $43828B9D, $6602661D, $423D6611
                dc.l $3A28A3D, $439D661C, $3A2423C, $621C005C, $3401D442
                dc.l $9411201C, $92408C01, $8822581E, $4415812, $4418A42
                dc.l $43C28BDD, $6602661D, $425D6612, $3A28A5D, $43DD661E
                dc.l $3A2425E, $621E005E, $3401D442, $9412201E, $92208C01
                dc.l $88225800, $4415811, $4418A22, $4002881D, $6602661D
                dc.l $423D6611, $3A28A3D, $401D6600, $3A24220, $62000040
                dc.l $3401D442, $94112000, $92408C01, $8822581F, $4415812
                dc.l $4418A42, $43E28BFD, $6602661D, $425D6612, $3A28A5D
                dc.l $43FD661F, $3A2425F, $621F005F, $3401D442, $9412201F
                dc.l $13DC03E0, $8B918812, $96BC9680, $969E96BF, $93208C01
                dc.l $8822581C, $4415819, $4418B22, $43828B9D, $6602661D
                dc.l $433D6619, $3A28B3D, $439D661C, $3A2433C, $621C005C
                dc.l $3401D442, $9419201C, $92408C01, $8822581E, $4415812
                dc.l $4418A42, $43C28BDD, $6602661D, $425D6612, $3A28A5D
                dc.l $43DD661E, $3A2425E, $621E005E, $3401D442, $9412201E
                dc.l $93208C01, $88225800, $4415819, $4418B22, $4002881D
                dc.l $6602661D, $433D6619, $3A28B3D, $401D6600, $3A24320
                dc.l $62000040, $3401D442, $94192000, $92408C01, $8822581F
                dc.l $4415812, $4418A42, $43E28BFD, $6602661D, $425D6612
                dc.l $3A28A5D, $43FD661F, $3A2425F, $621F005F, $3401D442
                dc.l $9412201F, $13DC03E0, $8B998812, $967C9640, $965E967F
                dc.l $92208C01, $8822581C, $4415811, $4418A22, $43828B9D
                dc.l $6602661D, $423D6611, $3A28A3D, $439D661C, $3A2423C
                dc.l $621C005C, $3401D442, $9411201C, $93208C01, $8822581E
                dc.l $4415819, $4418B22, $43C28BDD, $6602661D, $433D6619
                dc.l $3A28B3D, $43DD661E, $3A2433E, $621E005E, $3401D442
                dc.l $9419201E, $92208C01, $88225800, $4415811, $4418A22
                dc.l $4002881D, $6602661D, $423D6611, $3A28A3D, $401D6600
                dc.l $3A24220, $62000040, $3401D442, $94112000, $93208C01
                dc.l $8822581F, $4415819, $4418B22, $43E28BFD, $6602661D
                dc.l $433D6619, $3A28B3D, $43FD661F, $3A2433F, $621F005F
                dc.l $3401D442, $9419201F, $13DC03E0, $8B918819, $88C088E1
                dc.l $8B020011, $320059, $ADF39800, $365400F0, $D000E400
                dc.l $95519572, $95999800, $30F200F0, $8391836, $D014932C
                dc.l $18288901, $897E945C, $259E2581, $63BE003C, $3DC9F90
                dc.l $8A016301, $2A018830, $62012830, $94D61839, $12D9932C
                dc.l $8327C08, $D014924B, $94A81832, $1112924B, $831182B
                dc.l $D014922A, $AEA0D000, $E4009800, $346400F0, $9801369C
                dc.l $F0C5E0, $D020E400, $980034D6, $F02800, $C5E09813
                dc.l $34D600F0, $98000098, _i4, $A41E981F, XADDINC, $661E27FE
                dc.l $98003F60, $F063DE, $A401A420, $3C0A41C, $A38B085C
                dc.l $A388085C, $2F398971, $89126431, $64322011, $20088A36
                dc.l $922A924B, $916CA790, $3410D4A1, $74309800, $34D600F0
                dc.l $D000AFA1, $AF80ADF3, $98003654, $F0D000, $E4009551
                dc.l $95729800, $34B800F0, $8311836, $D014932C, $18288901
                dc.l $897E945C, $259E2581, $63BE003C, $3DC9F90, $8A016301
                dc.l $2A018830, $62012830, $94D61839, $12D9932C, $8327C08
                dc.l $D014924B, $94A81832, $1112924B, $831182B, $D014922A
                dc.l $AEA0D000, $E400AE20, $D000E400, $79F1D274, $79F2D274
                dc.l $7C11D278, $7C12D278, $8A218A42, $63E14142, $2202E2
                dc.l $8B606500, $28A0B840, $D260E400, $79F1D274, $79F2D274
                dc.l $7C11D278, $7C12D278, $8A218A42, $63E14142, $2202E2
                dc.l $8B606500, $28A0B840, $842B840, $142B840, $1842B840
                dc.l $D260E400, $980000EC, _i4, $A4019800, $353400F0, $66019802
                dc.l $355C00F0, $8217C21, $D002E400, $7C41D042, $E4009800
                dc.l $223800F0, $A4023402, $D7A2E400, $98002200, $F0BC17
                dc.l $98024420, $10880, $BC0289F9, $10398822, $64221051
                dc.l $7C11D278, $7A39D278, $8A208A51, $62001051, $7C11D278
                dc.l $7A39D278, $66006211, $28119800, $220C00F0, $BC118822
                dc.l $20020880, $62026602, $3A028B71, $BC026511, $9800226C
                dc.l $F028B1, $8A226211, $2851BC11, $1880BC11, $88229800
                dc.l $223C00F0, $62012841, $BC019802, $2000001, $1880BC02
                dc.l $A4013401, $D7A2E400, $D260E400, $62516252, $7C39D278
                dc.l $65D98A3D, $58115731, $37FDD442, $E4002011, $8A5D5812
                dc.l $573237FD, $D442E400, $20126CF1, $6CF29800, $C00000
                dc.l $110012, $AF608B61, $40019800, $7F, $24019800, $359A00F0
                dc.l $D000E400, $980000E0, _i4, $A4019800, $E40000, $60A402
                dc.l $66016602, $C781C7A2, $8840981E, $1F0000, $881F8825
                dc.l $27DF27C5, $643E13DF, $D454E400, $201F13C5, $D454E400
                dc.l $2005661F, $6605639F, $2BE56305, $9800007C, _i4, $A4148CEC
                dc.l $98090000, $40AC62, $8A80981D, padright, $660025A0
                dc.l $409C1E, $8209C1A, $13DA8A82, $13BE6502, $631E25A2
                dc.l $445A03DA, $93540134, $AC628A80, $981D0080, $6600
                dc.l $25A00040, $9C1E0820, $9C1A13DA, $8A8213BE, $6502631E
                dc.l $25A2445A, $3DA9355, $98000074, _i4, $A4146394, $AC628A80
                dc.l $981D0080, $6600, $25A00040, $9C1E0820, $9C1A13DA
                dc.l $8A8213BE, $6502631E, $25A2445A, $3DA9352, $134AC62
                dc.l $8A80981D, padright, $660025A0, $409C1E, $8209C1A
                dc.l $13DA8A82, $13BE6502, $631E25A2, $445A03DA, $93539800
                dc.l $700000, $60A414, $AC628A80, $981D0080, $6600, $25A00040
                dc.l $9C1E0820, $9C1A13DA, $8A8213BE, $6502631E, $25A2445A
                dc.l $3DA8B55, $134AC62, $8A80981D, padright, $660025A0
                dc.l $409C1E, $8209C1A, $13DA8A82, $13BE6502, $631E25A2
                dc.l $445A03DA, $8B5A9800, $540000, $60A41B, $98000084
                dc.l _i4, $A4099800, $880000, $60A414, $9800006C
                dc.l _i4, $A4049800, $240000, $60A406, $98000028, _i4
                dc.l $A4079800, $340000, $60A418, $980000EC, _i4, $A4016601
                dc.l $2821C761, $98000000, $C01006, $1007ADE0, $D000E400
                dcb.l 2,$F0355C
                dc.l $F03590, $F03684
                dcb.l $04,0
shu:   dc.l G_RAM, $5F0, $98003F5C, $F0980E, $3F8000F0, $A4019800
                                        ; DATA XREF: ROM:001A69E4↑o
                dc.l $480000, $60A401, $9800004C, _i4, $A402BDC1, $C4229800
                dc.l $180000, $60A401, $9800001C, _i4, $A4029800, $C00000
                dc.l $66016602, $10011002, $C7C1C7E2, $98000050, _i4, $A4019800
                dc.l $A40000, $60A402, $C441C462, $2C21C481, $98000000
                dc.l _i4, $A4179800, $A00000, $60A416, $980A0300, $980F
                dc.l $17E0000, $980D00FF, $9800, $E80000, $60A401, $63C19800
                dc.l $35D000F0, $20A401, $C6219800, $E00000, $60A402, $980000E4
                dc.l _i4, $A4019049, $902A9800, $31E000F0, $2800C5E0, $9804003F
                dc.l $9800, $3F6000F0, $A4010B81, $A4269800, $D80000, $60A401
                dc.l $980000DC, _i4, $A4026C81, $6C829024, $90459800, $780000
                dc.l $60A401, $90269800, $240000, $60A401, $98000028, _i4
                dc.l $A4026601, $66029800, $EC0000, $60A41D, $661D8BBE
                dc.l $637E643D, $13C113A2, $90309051, $98000088, _i4, $A4016501
                dc.l $38019032, $980000EC, _i4, $A4016601, $2821C761, $981BFFFF
                dc.l $9813, $31E000F0, $9814316C, $F09804, $3F0000, $9632A4DB
                dc.l $63DB841B, $8B7E63FB, $637E8B60, $65601012, $94C103C1
                dc.l $98000078, _i4, $BC019522, $95418840, $8FDE881F, $8825661F
                dc.l $660527DF, $27C5643E, $13DFD454, $E400201F, $13C5D454
                dc.l $E4002005, $639F2BE5, $6305949E, $63DB94BF, $841B03C1
                dc.l $3E2902A, $90490886, $AF608891, $40119600, $11980C
                dc.l $31F800F0, $D180E400, $1824D294, $E40094C1, $98000078
                dc.l _i4, $BC01AEA0, $D000E400, $981F33BA, $F03616, $D481E400
                dc.l $ADF3D3E0, $E400C545, $65059801, $F0000, $88A06485
                dc.l $24206205, $6200C565, $C5809800, $480000, $60A401
                dc.l $9800004C, _i4, $A402BDC1, $C4229800, $500000, $60A401
                dc.l $980000A4, _i4, $A402C441, $C4629800, $780000, $60A401
                dc.l $C481981D, padright, $980000A8, _i4, $A4019800, $AC0000
                dc.l $60A402, $C5A1C5C2, $6E016E02, $10311052, $C4B1C4D2
                dc.l $AC20AC81, $AC420001, $40C420, $AC62C481, $660125A1
                dc.l $88200801, $400801, $9C1E0820, $9C1313D3, $AC9F13BE
                dc.l $651F631E, $25BF47F3, $27E25A1, $8BC00041, $46409C22
                dc.l $8219C33, $105313A2, $630247F3, $262463E, $44514452
                dc.l $101103D2, $6DF16DF2, $980000B0, _i4, $A4014431, $44326D91
                dc.l $6D92ADA1, $ADC26E01, $6E020031, $52AD60, $AD818FDE
                dc.l $881F8825, $661F6605, $27DF27C5, $643E13DF, $D454E400
                dc.l $201F13C5, $D454E400, $2005639F, $2BE56305, $881F9800
                dc.l $B80000, $60A41E, $3DFC57F, $980000BC, _i4, $A41E03C1
                dc.l $C581981F, $33BA00F0, $98133356, $F0D3E0, $E400ACB1
                dc.l $ACD29800, $B40000, $60A401, $980000C0, _i4, $A402981E
                dc.l $10000000, $3C103C2, $44314452, $6D916D92, $ADA1ADC2
                dc.l $980000C4, _i4, $A41E03C1, $C5A19800, $C80000, $60A41E
                dc.l $3C2C5C2, $A5C09801, $328400F0, $1820D478, $BDC0D020
                dc.l $E400ADF3, $AD45D260, $E400981F, $344E00F0, $C4F3C536
                dc.l $C651C672, $3416D4C2, $E4009813, $33D800F0, $D3E0E400
                dc.l $89E0AE51, $AE721220, $88113436, $D4C2E400, $981333F2
                dc.l $F0D3E0, $E40089E0, $AE51AE72, $12408812, $3456D4C2
                dc.l $E4009813, $340C00F0, $D3E0E400, $89E0AE51, $AE721220
                dc.l $98133426, $F08811, $3476D462, $E400D3E0, $E40089E0
                dc.l $AE51AE72, $12206496, $8A518812, $3496D4A2, $E400ACF3
                dc.l $AD36D260, $E4003916, $980033C8, $F0D000, $E400C651
                dc.l $C672AFC0, $AFE10011, $32AE20, $D000E400, $79F1D274
                dc.l $79F2D274, $7C11D278, $7C12D278, $8A218A42, $63E14142
                dc.l $2202E2, $8B606500, $28A0B840, $D260E400, $79F1D274
                dc.l $79F2D274, $7C11D278, $7C12D278, $8A218A42, $63E14142
                dc.l $2202E2, $8B606500, $28A0B840, $842B840, $142B840
                dc.l $1842B840, $D260E400, $980000EC, _i4, $A4019800, $346000F0
                dc.l $96419802, $348800F0, $7C01D002, $E4007C21, $D042E400
                dc.l $98002238, $F0A402, $3402D7A2, $E4009800, $220000F0
                dc.l $BC179802, $44200001, $880BC02, $89F01030, $8A3C8822
                dc.l $6422105C, $7C1CD278, $7B90D278, $8B808A5C, $6200105C
                dc.l $7C1CD278, $7B90D278, $6600621C, $281C9800, $220C00F0
                dc.l $BC1C8822, $20020880, $62026602, $3A028B7C, $BC02651C
                dc.l $9800226C, $F028BC, $8B82621C, $285CBC1C, $1880BC1C
                dc.l $88229800, $223C00F0, $62012841, $BC019802, $2000001
                dc.l $1880BC02, $A4013401, $D7A2E400, $D260E400, $62516252
                dc.l $981B01FF, $2779, $133B63D9, $633B8A3D, $58115731
                dc.l $37FDD442, $E4002011, $8A5D5812, $573237FD, $D442E400
                dc.l $20126CB1, $6CB29800, $C00000, $110012, $980031F8
                dc.l $F0D000, $E400AF61, $62019800, $34C600F0, $D000E400
                dc.l $F03460, $F03488, $F034BC, $F035C2
                dcb.l $04,0
dbeast:   dc.l G_RAM, $13C, $98003F74, $F0A41D, $93BDA3BB, $980030AC
                                        ; DATA XREF: ROM:001A69E8↑o
                dc.l $F07C1B, $D018E400, $9800304E, $F0AF02, $8CA11041
                dc.l $783BD002, $E4009800, $3F6000F0, $A4010801, $A43D93BD
                dc.l $A3BB9800, $30AC00F0, $7C1BD018, $E4008CA1, $1041783B
                dc.l $D001085D, $9800003F, $A3BC, $241C8B9A, $98000600
                dc.l $85D, $401AA7A1, $3A9802, $3000000, $88608B41, $981430BA
                dc.l $F09815, $308800F0, $2AB5C7F5, $D280E400, $89DA3BA
                dc.l $85D8B59, $A3B8431A, $201A039A, $18DDA7BB, $98003F58
                dc.l $F08C21, $BC018C21, $D440E400, $2C21980E, $3F8000F0
                dc.l $AEA0D000, $E4009814, $223800F0, $A6953415, $D7A2E400
                dc.l $98142200, $F09815, $222400F0, $8839981A, $FFF8FFFF
                dc.l $8B382759, $1338BE99, $8949816, $40200000, $BE960914
                dc.l $6438BE98, $88198817, $27591337, $BEB90895, $BEB60915
                dc.l $6437BEB7, $64229819, $223C00F0, $3A02BF22, $98160001
                dc.l $1802AF8, $D482E400, $98160005, $1809814, $223800F0
                dc.l $BE96A695, $3415D7A2, $E400AFF4, $D280E400, 0
polyos:     dc.l vlmlogo2        ; DATA XREF: sub_192088+D80↑o
                dc.l chev1
                dc.l chev
                dc.l square
                dc.l chev2
                dc.l chev3
                dc.l chev4
                dc.l chev5
chev:   dc.l $20050, $600048, 0 ; DATA XREF: ROM:001ABB10↑o
                dc.l $10004, $20008, $49, XADDINC, $10004, $20008, 0
                dc.l $C00050, rsym_ist, $8000A0
                dc.b 0, $C0
chev5:    dc.w 2                  ; DATA XREF: ROM:001ABB24↑o
                dc.l $500060, $FE0000, SRCEN, $40002, $80000, $FD0003, SRCEN, $40002
                dc.l $80000, roscalei, $500000, $500080, $A000C0
chev1:   dc.l $20050, $6000F1, 0 ; DATA XREF: ROM:001ABB0C↑o
                dc.l $10004, $20008, $F2, XADDINC, $10004, $20008, 0
                dc.l $C00050, rsym_ist, $8000A0
                dc.b 0, $C0
chev2:    dc.w 2                  ; DATA XREF: ROM:001ABB18↑o
                dc.l $500060, $810000, SRCEN, $40002, $80000, $820003, SRCEN, $40002
                dc.l $80000, roscalei, $500000, $500080, $A000C0
chev3:   dc.l $20050, $60008F, 0 ; DATA XREF: ROM:001ABB1C↑o
                dc.l $10004, $20008, $8E, XADDINC, $10004, $20008, 0
                dc.l $C00050, rsym_ist, $8000A0
                dc.b 0, $C0
chev4:    dc.w 2                  ; DATA XREF: ROM:001ABB20↑o
                dc.l $500060, $120000, SRCEN, $40002, $80000, $130003, SRCEN, $40002
                dc.l $80000, roscalei, $500000, $500080, $A000C0
square:   dc.l $40100, $1000004, star, $20004, vfb_ysca, $04, $20004
                                        ; DATA XREF: ROM:001ABB14↑o
                dc.l $40008, vfb_ysca, $04, $40008, $3000C, vfb_ysca, $04, $3000C, star
                dc.l vfb_ysca, UPDA1F, LFU_A, optionbu, 0
                dc.l $2000200
                dc.b 2, 0
vlmlogo2:       dc.w $08                  ; DATA XREF: ROM:polyos↑o
                dc.l $E00040, allpad, $100001, $C0002, $140000, $8F0003
                dc.l $80004, $C0005, screen1, $8F0004, $C0006, $140005
                dc.l screen1, $8F0006, $140007, $C0005, screen1, $8F0007
                dc.l $C0008, $140005, screen1, 9, $14000A, $8000C, $C0000
                dc.l $A, $8000D, $10000C, $C0000, $B, $14000A, $8000D
                dc.l screen1, 0
                dc.l padright, $400080, $A00000, $E00000, $A00080, $E00040
                dc.l $1200040, $1200080, $1400000, $1800040, $1C00000
                dc.l $1400080, $1C00080, 0
                dc.l $80008, $39803F8, $4000408, $4100118
                dcb.l $D,$1180118
                dc.l $1180130, $1380140, $1480150, $1580160, $1680170
                dc.l $1780180, $1880190, $19801A0, $1A801B0, $1B801C0
                dc.l $1C801D0, $1D801E0, $1E801F0, $3A00118, $1280118
                dc.l $1200118, $11802C8, $2D002D8, $2E002E8, $2F002F8
                dc.l $3000308, $3100318, $3200328, $3300338, $3400348
                dc.l $3500358, $3600368, $3700378, $3800388, $3900118
                dcb.l 2,$1180118
                dc.l $11801F8, $2000208, $2100218, $2200228, $2300238
                dc.l $2400248, $2500258, $2600268, $2700278, $2800288
                dc.l $2900298, $2A002A8, $2B002B8, $2C00118
                dcb.l 2,$1180118
                dc.l $3A803B0, $3B803C0, $3C803D0, $3D803E0, $3E803F0
                dcb.l 2,0
                dc.l $20100804
                dcb.l 2,$4081020
                dc.l $20100804, $20202020, $20000020, $50505000, 0
                dc.l $4848FE48, $48FE4848, $107E9070, $1014FE20, $60620408
                dc.l $10204C8C, $20508870, $64989A64, $102040, 0
                dc.l $10204040, $40402010, $20100808, $8081020, $925438
                dc.l $38549200, $101010FE, $10101000, 0
                dc.l $101020, $FE
                dcb.l 2,0
                dc.l $606000, $20408, $10204080, $7C82868A, $92A2C27C
                dc.l $10305010, $101010FE, $78840408, $102040FE, $7C820204
                dc.l $7C02827C, $40C1424, $4484FE04, $FE8080FC, $202827C
                dc.l $384080FC, $8282827C, $FE020408, $10204080, $7C82827C
                dc.l $8282827C, $7C828282, $7E02827C, phase4, $27E827E, $808080FC
                dc.l $828282FC, $7E, $8080807E, $202027E, $8282827E, phase4
                dc.l $82FC807C, $3E4040, $FE404040, $7E, $827E027C, $808080FC
                dc.l $82828282, $100030, $10101038, $100010, $101010E0
                dc.l $80808088, $90F08884, $60202020, $20202070, phase4, $92929282
                dc.l $78, $84848484, phase4, $8282827C, gpu, $82FC8080, $7E
                dc.l $827E0202, gpu, $82808080, $7E, $403804FC, $10FE
                dc.l $1010100C, $82, $8282867A, $82, $82442810, $82, $8292AAC6
                dc.l j, $48304884, $82, $827E02FC, $FE, $41040FE, $10284482
                dc.l $FE828282, $FC8284FC, $828282FC, $7EC08080, $8080C07E
                dc.l $FC868282, $828286FC, $FE8080FE, $808080FE, $FE8080FE
                dc.l $80808080, $7EC08080, $8086C27C, $828282FE, $82828282
                dc.l $FE101010, $101010FE, $FE080808, $8888870, $848890E0
                dc.l $90888482, $80808080, $808080FE, $82C6AA92, $82828282
                dc.l $82C2A292, $8A868282, $7CC68282, $8282C67C, $FC828282
                dc.l $FC808080, $7CC68282, $828AC67A, $FC828282, $FC888482
                dc.l $7E80C030, $1C0602FC, $FE101010, $10101010, $82828282
                dc.l $8282827C, $82828244, $44282810, $82828282, $8292AAC6
                dc.l $82824438, $28448282, $82828244, $28101010, $FE020408
                dc.l $102040FE, $C0F0FCFE, $FEFCF0C0, CD_init, $300000, $3C6666
                dc.l $66663C00, $381818, $18183C00, $386C0C, $18307C00
                dc.l $7E060C, $6663C00, $1C3C6C, $7E0C0C00, $7E607C, $6067C00
                dc.l $3C607C, $66663C00, $3E060C, $18181800, $3C663C, $66663C00
                dc.l $3C663E, $6063C00
                dcb.l 2,$FFFFFFFF
                dc.l $FF8F8F8F, $8F8F8FFF, $FFF1F1F1, $F1F1F1FF, $FF818181
                dc.l $818181FF
; ians.gas

; =============== S U B R O U T I N E =======================================

;dc.w $303a, $005c, $6702, $4e75, $33fc, $ffff, $001a, $c1fe

iansdoit:                             ; CODE XREF: sub_192088+E↑p
.incbin "incbin/ians.bin"

                END
