; *******************************************************************
; vlmgpu.s
; *******************************************************************
.include "../jaguar.inc"
.include "../blitter.inc"

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

; *******************************************************************
; gpuload
; *******************************************************************
gpuload:
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

.aligned:
                move.l  #$1800001,d0

.blit_go:
                move.l  d0,(B_CMD).l

wblit: 
                move.l  (B_CMD).l,d0
                btst    #0,d0
                beq.s   wblit
                movem.l (sp)+,d0-d2/a0-a1
                rts

; *******************************************************************
; gpurun
; *******************************************************************
gpurun:
                bsr.w   gpuload
                movem.l d0-d1/a0,-(sp)
                move.l  (a0)+,(G_PC).l
                move.l  #$11,(G_CTRL).l
                movem.l (sp)+,d0-d1/a0
                rts

; *******************************************************************
; gpuwait:
; *******************************************************************
gpuwait:
                movem.l d0/a0,-(sp)
                lea     (G_CTRL).l,a0
.gpuwt: 
                move.l  (a0),d0
                btst    #0,d0
                bne.s   .gpuwt
                movem.l (sp)+,d0/a0
                rts
                .dphrase

; *******************************************************************
; gpumods
; *******************************************************************
gpumods:        dc.l alpha     ; Draw 3D Starfield  - $0000
                dc.l beta      ; Digital Feedback Video Area - $10000
                dc.l gamma     ; Draw a Polygon Object - $20000
                dc.l psi       ; Post-amble for each effect. - $30000
                dc.l delta     ; Colour Plasma Area - $40000
                dc.l epsilon   ; Draw Particle Object - $50000
                dc.l theta     ; Do Particle motion - $60000
                dc.l sigma     ; Mono Particle Object (Appears to be unused?) - $70000
                dc.l tau       ; Matrix object (Appears to be unused?) - $80000
                dc.l shu       ; Spectrum as intensities - $90000
                dc.l dbeast    ; Preamble for each effect. - $a0000

; *******************************************************************
;
;       CONSTANT DATA (GPU PROGRAMS)
;
; The extra bytes before each include are the output header the MAC
; assembler would have produced for each. Unfortunately neither rmac nor
; vasm support this output format, so we have to use rmac to create an 
; absolute binary and append the headers here.
;
; There is a trick in the way these modules are used. The 'omega' module
; orchestrates the loading of the others. Notice that 'omega' is loaded to 
; address $F03A78 in the GPU's RAM. It loads all the other modules as they
; are required. It can do this because all the others get loaded to $F03000 in 
; the GPU's RAM and none of them is longer than $A78 bytes! This means
; that 'omega' can stay resident in RAM at $F03a78 and load the others to $F03000 
; without getting overwritten by them!
; *******************************************************************
                .dphrase
alpha:
                dc.w $00f0, $3000 ; The address in RAM to load the module at.
                dc.w $0000, $09d0 ; The length in bytes of the module.
                .incbin '../bin/alpha.o'
beta:
                dc.w $00f0, $3000 ; The address in RAM to load the module at.
                dc.w $0000, $061e ; The length in bytes of the module.
                .incbin '../bin/beta.o'
gamma:
                dc.w $00f0, $3000 ; The adddress in RAM to load the module at.
                dc.w  $0000, $0a38 ; The length in bytes of the module.
                .incbin '../bin/gamma.o'
omega:
                dc.w $00f0, $3a78 ; The adddress in RAM to load the module at.
                dc.w $0000, $0484 ; The length in bytes of the module.
                .incbin '../bin/omega.o'
psi:   
                dc.w $00f0, $3000 ; The adddress in RAM to load the module at.
                dc.w $0000, $071c ; The length in bytes of the module.
                .incbin '../bin/psi.o'
delta:
                dc.w $00f0, $3000 ; The adddress in RAM to load the module at.
                dc.w $0000, $0530 ; The length in bytes of the module.
                .incbin '../bin/delta.o'
epsilon: 
                dc.w $00f0, $3000 ; The adddress in RAM to load the module at.
                dc.w $0000, $09e0 ; The length in bytes of the module.
                .incbin '../bin/epsilon.o'
theta: 
                dc.w $00f0, $3000 ; The adddress in RAM to load the module at.
                dc.w $0000, $0224 ; The length in bytes of the module.
                .incbin '../bin/theta.o'
sigma: 
                dc.w $00f0, $3000 ; The adddress in RAM to load the module at.
                dc.w $0000, $09f8 ; The length in bytes of the module.
                .incbin '../bin/sigma.o'
tau:
                dc.w $00f0, $3000 ; The adddress in RAM to load the module at.
                dc.w $0000, $0888 ; The length in bytes of the module.
                .incbin '../bin/tau.o'
shu:
                dc.w $00f0, $3000 ; The adddress in RAM to load the module at.
                dc.w $0000, $05f0 ; The length in bytes of the module.
                .incbin '../bin/shu.o'
dbeast:   
                dc.w $00f0, $3000 ; The adddress in RAM to load the module at.
                dc.w $0000, $013c ; The length in bytes of the module.
                .incbin '../bin/dbeast.o'
; vim:ft=asm68k ts=2
