.include "../jaguar.inc"
.include "../blitter.inc"

; =============== S U B R O U T I N E =======================================

.globl gpuload
.globl wblit
.globl gpurun
.globl gpumods
.globl alpha
.globl beta
.globl gamma
.globl omega
.globl psi
.globl delta
.globl epsilon
.globl tau
.globl shu
.globl dbeast
.globl theta

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
gpumods:        dc.l alpha       ; DATA XREF: sub_192088+D8A↑o
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
alpha:
dc.w $00f0, $3000, $0000, $09d0
.incbin '../bin/alpha.o'
beta:
dc.w $00f0, $3000, $0000, $061e
.incbin '../bin/beta.o'
gamma:
.incbin 'gpu/gamma.bin'
omega:
.incbin 'gpu/omega.bin'
psi:   
.incbin 'gpu/psi.bin'
delta:
dc.w $00f0, $3000, $0000, $0530
.incbin '../bin/delta.o'
epsilon: 
dc.w $00f0, $3000, $0000, $09e0
.incbin '../bin/epsilon.o'
theta: 
dc.w $00f0, $3000, $0000, $0224
.incbin '../bin/theta.o'
sigma: 
dc.w $00f0, $3000, $0000, $09f8
.incbin '../bin/sigma.o'
tau:
dc.w $00f0, $3000, $0000, $0888
.incbin '../bin/tau.o'
shu:
.incbin 'gpu/shu.bin'
dbeast:   
dc.w $00f0, $3000, $0000, $013c
.incbin '../bin/dbeast.o'
