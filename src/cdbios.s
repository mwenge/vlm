; Segment type: Pure code
; segment "ROM"

.org $3000

CD_init:
                bra.s   CDInit
; ---------------------------------------------------------------------------
                nop
; ---------------------------------------------------------------------------
                dc.b    4,   5
; ---------------------------------------------------------------------------

CD_mode:
                bra.w   CDMode
; ---------------------------------------------------------------------------
                nop

CD_ack:
                bra.w   CDAck
; ---------------------------------------------------------------------------
                nop

CD_jeri:
                bra.w   CDJeri
; ---------------------------------------------------------------------------
                nop

CD_spin:
                bra.w   CDSpin
; ---------------------------------------------------------------------------
                nop

CD_stop:
                bra.w   CDStop
; ---------------------------------------------------------------------------
                nop

CD_mute:
                bra.w   CDMute
; ---------------------------------------------------------------------------
                nop

CD_umute:
                bra.w   CDUmute
; ---------------------------------------------------------------------------
                nop

CD_paus:
                bra.w   CDPaus
; ---------------------------------------------------------------------------
                nop

CD_upaus:
                bra.w   CDUPaus
; ---------------------------------------------------------------------------
                nop

CD_read:
                bra.w   CDRead
; ---------------------------------------------------------------------------
                nop

CD_uread:
                bra.w   CDURead
; ---------------------------------------------------------------------------
                nop

CD_setup:
                bra.w   CDSetup
; ---------------------------------------------------------------------------
                nop

CD_ptr:
                bra.w   CDPtr
; ---------------------------------------------------------------------------
                nop

CD_osamp:
                bra.w   CDOSamp
; ---------------------------------------------------------------------------
                nop

CD_unused1:
                bra.w   CDUnused1
; ---------------------------------------------------------------------------
                nop

CD_unused2:
                bra.w   CDUnused2
; ---------------------------------------------------------------------------
                nop

CD_unused3:
                bra.w   CDUnused3
; ---------------------------------------------------------------------------
                nop

CD_unused4:
                bra.w   CDUnused4
; ---------------------------------------------------------------------------
                nop
; ---------------------------------------------------------------------------
byte_3072:      dcb.b 2,0               ; DATA XREF: ROM:000030C2↓w
                                        ; ROM:00003214↓w ...
dword_3074:     dc.l 0                  ; DATA XREF: ROM:CDInit↓w
                                        ; ROM:CDUnused3↓w ...
; ---------------------------------------------------------------------------

CDInit:                                 ; CODE XREF: ROM:CD_init↑j
                move.l  a0,(dword_3074).l
                move.l  a0,d0
                add.l   #$C,d0
                and.l   #$FFFF,d0
                or.l    #$981E0000,d0
                move.l  d0,($F03010).l
                move.l  #$F0D3C0,($F03014).l
                move.l  #$E400E400,($F03018).l

loc_30AC:
                movea.l #$30EA,a1
                move.l  #$E0,d0
                asr.l   #2,d0
                subq.l  #1,d0

loc_30BC:                               ; CODE XREF: ROM:000030BE↓j
                move.l  (a1)+,(a0)+
                dbf     d0,loc_30BC
                move.b  #0,(byte_3072).l
                move.w  ($DFFF0A).l,d0
                move.l  ($DFFF04).l,d0
                move.l  ($F02100).l,d0
                or.l    #$20,d0 ; ' '
                move.l  d0,($F02100).l
                rts
; ---------------------------------------------------------------------------
CDInitData:     dc.b    0,   0,   0,   0,   0,   0,   0,   0
                dc.b    0,   0,   0,   0, $98, $1E, $21,   0
                dc.b    0, $F0, $A7, $DD, $18, $9F, $BF, $F9
                dc.b  $18, $9F, $BF, $F8, $98, $18, $FF,   0
                dc.b    0, $DF, $18, $9F, $BF, $FB, $18, $9F
                dc.b  $BF, $FA, $18, $9F, $BF, $F7, $CC, $17
                dc.b  $98, $1C,   0, $2E,   0,   0, $13, $97
                dc.b  $CC, $19, $98, $1B,   0, $72,   0,   0
                dc.b    3, $79, $A7, $1B, $35, $BB, $D5, $E2
                dc.b  $3C, $BB, $38, $3B, $BF, $1B,  $A, $18
                dc.b  $A7, $1B, $38, $5B, $BF, $1B, $19, $98
                dc.b  $A7, $1A,   8, $D8, $A3, $1B, $35, $5B
                dc.b  $D4, $A1, $2B, $5A, $D3, $20, $35, $DB
                dc.b  $D5, $22, $3B, $FB,  $A, $18, $A7, $1C
                dc.b  $2B, $9C, $1A, $18, $A6, $FC,   9, $17
                dc.b  $BE, $FC, $19, $17, $A6, $FA,   8, $97
                dc.b  $A6, $FC, $18, $97, $7B, $5C, $D4, $54
                dc.b  $3C, $1B, $BF, $1B, $98, $1B, $FF, $24
                dc.b    0, $DF, $8B, $79,   8, $9B, $8C, $78
                dc.b  $A7, $7C, $A7, $3E,   8, $9A, $18, $38
                dc.b  $BF, $5C,  $C, $9A, $D7, $34, $BF, $5E
                dc.b  $BE, $FA, $98, $18,   0, $20,   0, $F1
                dc.b  $8C, $3C, $39, $1C, $BB, $1C, $A7, $F7
                dc.b    8, $9F, $A7, $FA,   8, $9F, $A7, $FB
                dc.b    8, $9F, $A7, $F8,   8, $9F, $A7, $F9
                dc.b    8, $9F, $98, $1E, $21,   0,   0, $F0
                dc.b  $3C, $7D, $39, $5D, $A7, $FC,   8, $5C
                dc.b    8, $9F, $D3, $80, $BF, $DD,   0,   0
; ---------------------------------------------------------------------------

CDUnused3:                              ; CODE XREF: ROM:CD_unused3↑j
                move.l  a0,(dword_3074).l
                move.l  a0,d0
                add.l   #$C,d0
                and.l   #$FFFF,d0
                or.l    #$981E0000,d0
                move.l  d0,($F03010).l
                move.l  #$F0D3C0,($F03014).l
                move.l  #$E400E400,($F03018).l
                movea.l #$323C,a1
                move.l  #$D4,d0
                asr.l   #2,d0
                subq.l  #1,d0

loc_320E:                               ; CODE XREF: ROM:00003210↓j
                move.l  (a1)+,(a0)+
                dbf     d0,loc_320E
                move.b  #1,(byte_3072).l
                move.w  ($DFFF0A).l,d0
                move.l  ($DFFF04).l,d0
                move.l  ($F02100).l,d0
                or.l    #$20,d0 ; ' '
                move.l  d0,($F02100).l
                rts
; ---------------------------------------------------------------------------
                dc.b    0,   0,   0,   0,   0,   0,   0,   0
                dc.b    0,   0,   0,   0, $98, $1E, $21,   0
                dc.b    0, $F0, $A7, $DD, $98, $18, $FF,   0
                dc.b    0, $DF, $CC, $17, $98, $1C,   0, $1A
                dc.b    0,   0, $13, $97, $CC, $19, $98, $1B
                dc.b    0, $90,   0,   0,   3, $79, $A7, $1B
                dc.b  $35, $BB, $D5, $E2, $3C, $BB, $38, $3B
                dc.b  $BF, $1B,  $A, $18, $A7, $1B, $38, $5B
                dc.b  $BF, $1B, $19, $98, $A7, $1A,   8, $D8
                dc.b  $A3, $1B, $35, $5B, $D4, $A1, $2B, $5A
                dc.b  $D3, $20, $35, $DB, $D5, $22, $3B, $FB
                dc.b   $A, $18, $A7, $1C, $2B, $9C, $1A, $18
                dc.b  $A6, $FC,   9, $17, $BE, $FC, $19, $17
                dc.b  $A6, $FA,   8, $97, $A6, $FC, $18, $97
                dc.b  $7B, $5C, $D4, $54, $3C, $1B, $BF, $1B
                dc.b  $98, $1B, $FF, $24,   0, $DF, $8B, $79
                dc.b    8, $9B, $A7, $7C, $A7, $3E, $A7, $75
                dc.b  $A7, $36, $A7, $78, $A7, $34, $A7, $73
                dc.b  $A7, $32,   8, $9A, $BF, $5C,  $C, $9A
                dc.b  $BF, $5E,  $C, $9A, $BF, $55,  $C, $9A
                dc.b  $BF, $56,  $C, $9A, $BF, $58,  $C, $9A
                dc.b  $BF, $54,  $C, $9A, $BF, $53,  $C, $9A
                dc.b  $BF, $52, $BE, $FA, $98, $18,   0, $20
                dc.b    0, $F1, $8C, $3C, $39, $1C, $BB, $1C
                dc.b  $98, $1E, $21,   0,   0, $F0, $3C, $7D
                dc.b  $39, $5D, $A7, $FC,   8, $5C,   8, $9F
                dc.b  $D3, $80, $BF, $DD
; ---------------------------------------------------------------------------

CDUnused2:                              ; CODE XREF: ROM:CD_unused2↑j
                move.l  a0,(dword_3074).l
                move.l  a0,d0
                add.l   #$14,d0
                and.l   #$FFFF,d0
                or.l    #$981E0000,d0
                move.l  d0,($F03010).l
                move.l  #$F0D3C0,($F03014).l
                move.l  #$E400E400,($F03018).l
                movea.l #$3382,a1
                move.l  #$150,d0
                asr.l   #2,d0
                subq.l  #1,d0

loc_3354:                               ; CODE XREF: ROM:00003356↓j
                move.l  (a1)+,(a0)+
                dbf     d0,loc_3354
                move.w  ($DFFF0A).l,d0
                move.l  ($DFFF04).l,d0
                move.b  #$FF,(byte_3072).l
                move.l  ($F02100).l,d0
                or.l    #$20,d0 ; ' '
                move.l  d0,($F02100).l
                rts
; ---------------------------------------------------------------------------
                dc.b    0,   0,   0,   0,   0,   0,   0,   0
                dc.b    0,   0,   0,   0,   0,   0,   0,   0
                dc.b    0,   0,   0,   0, $98, $1E, $21,   0
                dc.b    0, $F0, $A7, $DD, $18, $9F, $BF, $F9
                dc.b  $18, $9F, $BF, $F8, $98, $18, $FF,   0
                dc.b    0, $DF, $18, $9F, $BF, $FB, $A7, $1B
                dc.b  $18, $9F, $BF, $FA, $18, $9F, $BF, $F7
                dc.b  $18, $9F, $BF, $F6, $CC, $17, $98, $1C
                dc.b    0, $3C,   0,   0, $13, $97, $CC, $19
                dc.b  $98, $1A,   0, $88,   0,   0,   3, $59
                dc.b  $35, $BB, $D5, $E2, $3C, $BB, $38, $3B
                dc.b  $BF, $1B,  $A, $18, $A7, $1B, $38, $5B
                dc.b  $BF, $1B, $19, $98, $A7, $1A,   8, $D8
                dc.b  $A3, $1B, $35, $5B, $D5, $E1, $2B, $5A
                dc.b  $D3, $20,   9, $97, $A6, $FA, $7C, $1A
                dc.b  $D4, $C2, $19, $97, $8B, $3C,  $B, $9C
                dc.b    9, $97,  $B, $9C, $D3, $80, $35, $DB
                dc.b  $D5, $22, $3B, $FB,  $A, $18, $A7, $1C
                dc.b  $2B, $9C, $1A, $18, $A6, $FC,   9, $17
                dc.b  $BE, $FC, $19, $17, $A6, $FA,   8, $97
                dc.b  $A6, $FC, $18, $97, $7B, $5C, $D4, $54
                dc.b  $3C, $1B, $BF, $1B, $98, $1B, $FF, $24
                dc.b    0, $DF, $8B, $79,   8, $9B, $8C, $78
                dc.b  $A7, $7C, $A7, $3E,   8, $9A, $3C, $1A
                dc.b  $BF, $5C,   8, $9A, $3C, $1A, $18, $38
                dc.b  $D6, $F4, $BF, $5E, $BE, $FA, $98, $18
                dc.b    0, $20,   0, $F1, $8C, $3C, $39, $1C
                dc.b  $BB, $1C, $A7, $F6,   8, $9F, $A7, $F7
                dc.b    8, $9F, $A7, $FA,   8, $9F, $A7, $FB
                dc.b    8, $9F, $A7, $F8,   8, $9F, $A7, $F9
                dc.b    8, $9F, $98, $1E, $21,   0,   0, $F0
                dc.b  $3C, $7D, $39, $5D, $A7, $FC,   8, $5C
                dc.b    8, $9F, $D3, $80, $BF, $DD, $98, $1B
                dc.b  $FF, $24,   0, $DF, $8C, $7E, $8D, $36
                dc.b  $63, $DE,   8, $97, $A6, $F8, $18, $97
                dc.b  $D4, $60, $E4,   0, $8E, $1A, $BE, $FA
                dc.b  $18, $36, $D3, $22, $E4,   0, $2F, $DB
                dc.b  $A7, $7C, $7B, $98, $D6, $E1, $E4,   0
                dc.b  $18, $3A, $D6, $C1, $BE, $FA, $19, $97
                dc.b  $A6, $FA, $2F, $DB,  $C, $9A, $A7, $7C
                dc.b  $BF, $5C, $18, $36, $D7, $41, $E4,   0
                dc.b  $BE, $FA, $D3, $20, $E4,   0,   0,   0
; ---------------------------------------------------------------------------

CDMode:                                 ; CODE XREF: ROM:CD_mode↑j
                                        ; ROM:00003502↓j
                move.w  d0,d2
                and.w   #1,d2
                add.w   #1,d2
                btst    #1,d0
                beq.s   loc_34E6
                bset    #3,d2

loc_34E6:                               ; CODE XREF: ROM:000034E0↑j
                or.w    #$1500,d2
                move.w  d2,($DFFF0A).l
                bsr.s   CDAck
                bset    #9,d2
                cmp.w   d1,d2
                beq.s   locret_3504
                move.w  #$F,d1

loc_34FE:                               ; CODE XREF: ROM:loc_34FE↓j
                dbf     d1,loc_34FE
                bra.s   CDMode
; ---------------------------------------------------------------------------

locret_3504:                            ; CODE XREF: ROM:000034F8↑j
                rts
; ---------------------------------------------------------------------------

CDOSamp:                                ; CODE XREF: ROM:CD_osamp↑j
                cmp.w   #3,d0
                bmi.s   loc_3510
                move.w  #2,d0

loc_3510:                               ; CODE XREF: ROM:0000350A↑j
                add.w   #1,d0
                or.w    #$7000,d0
                move.w  d0,($DFFF0A).l
                bsr.s   CDAck
                rts
; ---------------------------------------------------------------------------

CDSpin:                                 ; CODE XREF: ROM:CD_spin↑j
                or.w    #$1800,d1
                move.w  d1,($DFFF0A).l
                tst.w   d0
                beq.s   locret_3532
                bsr.s   CDAck

locret_3532:                            ; CODE XREF: ROM:0000352E↑j
                rts
; ---------------------------------------------------------------------------

CDStop:                                 ; CODE XREF: ROM:CD_stop↑j
                move.w  #$200,($DFFF0A).l
                tst.w   d0
                beq.s   locret_3542
                bsr.s   CDAck

locret_3542:                            ; CODE XREF: ROM:0000353E↑j
                rts

; =============== S U B R O U T I N E =======================================


CDAck:                                  ; CODE XREF: ROM:CD_ack↑j
                                        ; ROM:000034F0↑p ...
                move.l  ($DFFF00).l,d1
                and.w   #$2000,d1
                beq.s   CDAck
                move.w  ($DFFF0A).l,d1
                move.w  d1,-(sp)
                tst.l   ($DFFF04).l
                andi.w  #$FF00,d1
                cmpi.w  #$400,d1
                bne.w   loc_3572
                move.w  #0,($3E00).w
                bra.s   loc_357A
; ---------------------------------------------------------------------------

loc_3572:                               ; CODE XREF: CDAck+22↑j
                move.w  #1,($3E00).l

loc_357A:                               ; CODE XREF: CDAck+2C↑j
                move.w  (sp)+,d1
                rts
; End of function CDAck

; ---------------------------------------------------------------------------

CDMute:                                 ; CODE XREF: ROM:CD_mute↑j
                move.w  #$5100,($DFFF0A).l
                tst.w   d0
                beq.s   locret_358C
                bsr.s   CDAck

locret_358C:                            ; CODE XREF: ROM:00003588↑j
                rts
; ---------------------------------------------------------------------------

CDUmute:                                ; CODE XREF: ROM:CD_umute↑j
                move.w  #$51FF,($DFFF0A).l
                tst.w   d0
                beq.s   locret_359C
                bsr.s   CDAck

locret_359C:                            ; CODE XREF: ROM:00003598↑j
                rts
; ---------------------------------------------------------------------------

CDPaus:                                 ; CODE XREF: ROM:CD_paus↑j
                move.w  #$400,($DFFF0A).l
                tst.w   d0
                beq.s   locret_35AC
                bsr.s   CDAck

locret_35AC:                            ; CODE XREF: ROM:000035A8↑j
                rts
; ---------------------------------------------------------------------------

CDUPaus:                                ; CODE XREF: ROM:CD_upaus↑j
                move.w  #$500,($DFFF0A).l
                tst.w   d0
                beq.s   locret_35BC
                bsr.s   CDAck

locret_35BC:                            ; CODE XREF: ROM:000035B8↑j
                rts
; ---------------------------------------------------------------------------

CDJeri:                                 ; CODE XREF: ROM:CD_jeri↑j
                move.l  ($DFFF10).l,d1
                tst.w   d0
                bne.s   loc_35CE
                bclr    #1,d1
                bra.s   loc_35D2
; ---------------------------------------------------------------------------

loc_35CE:                               ; CODE XREF: ROM:000035C6↑j
                bset    #1,d1

loc_35D2:                               ; CODE XREF: ROM:000035CC↑j
                bset    #2,d1
                move.l  d1,($DFFF10).l
                rts
; ---------------------------------------------------------------------------

CDSetup:                                ; CODE XREF: ROM:CD_setup↑j
                move.l  #$180000,($DFFF00).l
                move.l  #$10000,($DFFF04).l
                move.l  #7,($DFFF10).l
                move.l  #1,($DFFF10).l
                move.w  #$7001,($DFFF0A).l
                rts
; ---------------------------------------------------------------------------

CDPtr:                                  ; CODE XREF: ROM:CD_ptr↑j
                movea.l (dword_3074).l,a0
                movea.l a0,a1
                adda.l  #8,a1
                movea.l (a0),a0
                movea.l (a1),a1
                rts
; ---------------------------------------------------------------------------

CDRead:                                 ; CODE XREF: ROM:CD_read↑j
                btst    #$1F,d0
                bne.w   loc_36DC
                subq.l  #4,a0
                move.l  d0,-(sp)
                move.l  ($DFFF00).l,d0
                and.l   #$FFFF0000,d0
                move.l  d0,($DFFF00).l
                move.l  (sp)+,d0
                move.w  #$101,($F10020).l
                move.l  d1,-(sp)
                move.l  ($DFFF10).l,d1
                bclr    #2,d1
                move.l  d1,($DFFF10).l
                move.l  (sp)+,d1
                movea.l (dword_3074).l,a2
                move.l  a0,(a2)+
                move.l  a1,(a2)+
                move.l  #0,(a2)+
                btst    #7,(byte_3072).l
                beq.w   loc_36A6
                movea.l (dword_3074).l,a0
                asl.l   #5,d2
                move.l  d2,-(sp)
                or.l    #$89A3C1A,d2
                move.l  d2,$BC(a0)
                move.l  (sp)+,d2
                swap    d2
                or.l    #$3C1A1838,d2
                move.l  d2,$C4(a0)
                move.l  #$10,(a2)+
                move.l  d1,(a2)

loc_36A6:                               ; CODE XREF: ROM:00003678↑j
                move.w  ($DFFF0A).l,d1
                move.l  ($DFFF10).l,d1

loc_36B2:                               ; CODE XREF: ROM:000036C2↓j
                move.l  ($DFFF24).l,d1
                move.l  ($DFFF10).l,d1
                btst    #4,d1
                bne.s   loc_36B2
                move.l  ($DFFF00).l,d1
                and.l   #$FFFF0000,d1
                or.l    #$21,d1 ; '!'
                move.l  d1,($DFFF00).l

loc_36DC:                               ; CODE XREF: ROM:00003628↑j
                move.l  d0,d1
                lsr.l   #8,d1
                lsr.w   #8,d1
                or.w    #$1000,d1
                move.w  d1,($DFFF0A).l
                bsr.s   sub_3712
                move.l  d0,d1
                lsr.w   #8,d1
                or.w    #$1100,d1
                move.w  d1,($DFFF0A).l
                bsr.s   sub_3712
                move.l  d0,d1
                and.w   #$FF,d1
                or.w    #$1200,d1
                move.w  d1,($DFFF0A).l
                bsr.s   sub_3712
                rts

; =============== S U B R O U T I N E =======================================


sub_3712:                               ; CODE XREF: ROM:000036EC↑p
                                        ; ROM:000036FC↑p ...
                move.w  #0,d1

loc_3716:                               ; CODE XREF: sub_3712:loc_3716↓j
                dbf     d1,loc_3716
                move.l  ($DFFF00).l,d1
                and.l   #$1000,d1
                beq.s   sub_3712
                move.l  ($DFFF04).l,d1
                rts
; End of function sub_3712

; ---------------------------------------------------------------------------

CDURead:                                ; CODE XREF: ROM:CD_uread↑j
                move.l  ($DFFF10).l,d0
                bclr    #2,d0
                move.l  d0,($DFFF10).l
                rts
; ---------------------------------------------------------------------------

CDUnused1:                              ; CODE XREF: ROM:CD_unused1↑j
                                        ; ROM:000039C8↓j
                movem.l d0-d7/a1-a6,-(sp)
                lea     ($DFFF00).l,a4
                lea     $384(a0),a3
                movea.l a0,a1
                moveq   #0,d0
                move.w  #$FF,d1

loc_3758:                               ; CODE XREF: ROM:0000375A↓j
                move.l  d0,(a1)+
                dbf     d1,loc_3758
                moveq   #$FFFFFFFF,d6
                moveq   #0,d7
                move.b  d6,6(a0)

loc_3766:                               ; CODE XREF: ROM:000038D6↓j
                move.w  d7,d0
                or.w    #$300,d0
                moveq   #5,d1
                bsr.w   sub_390E
                move.w  d2,d0
                lsr.w   #8,d0
                cmpi.w  #4,d0
                bne.s   loc_3784
                moveq   #$FFFFFFFF,d0
                move.l  d0,(a0)
                bra.w   loc_3906
; ---------------------------------------------------------------------------

loc_3784:                               ; CODE XREF: ROM:0000377A↑j
                moveq   #4,d3
                move.l  a3,-(sp)

loc_3788:                               ; CODE XREF: ROM:loc_379E↓j
                move.w  (a3)+,d0
                move.w  d0,d1
                lsr.w   #8,d1
                subi.w  #$20,d1 ; ' '
                bcs.s   loc_379E
                cmpi.w  #5,d1
                bcc.s   loc_379E
                move.b  d0,(a0,d1.w)

loc_379E:                               ; CODE XREF: ROM:00003792↑j
                                        ; ROM:00003798↑j
                dbf     d3,loc_3788
                movea.l (sp)+,a3
                move.b  1(a0),d3
                cmp.b   7(a0),d3
                bls.s   loc_37B2
                move.b  d3,7(a0)

loc_37B2:                               ; CODE XREF: ROM:000037AC↑j
                move.b  (a0),d5
                moveq   #0,d4
                move.b  d3,d4
                sub.b   d5,d4
                addq.l  #1,d4
                cmp.b   6(a0),d5
                bcc.s   loc_37C6
                move.b  d5,6(a0)

loc_37C6:                               ; CODE XREF: ROM:000037C0↑j
                move.b  d7,5(a0)
                move.w  d7,d0
                or.w    #$1400,d0
                move.w  d0,$A(a4)
                move.w  #$32,d0 ; '2'
                tst.w   $A(a4)
                moveq   #0,d5
                bra.s   loc_383E
; ---------------------------------------------------------------------------

loc_37E0:                               ; CODE XREF: ROM:000037E6↓j
                                        ; ROM:loc_383E↓j
                move.l  (a4),d2
                btst    #$D,d2
                beq.s   loc_37E0
                move.w  $A(a4),d2
                tst.l   4(a4)
                move.w  d2,d1
                lsr.w   #8,d1
                cmpi.w  #$60,d1 ; '`'
                bne.s   loc_3828
                move.w  #$32,d0 ; '2'
                moveq   #0,d5
                andi.w  #$FF,d2
                beq.w   loc_383E
                cmp.b   d2,d3
                bcs.w   loc_383E
                move.w  d2,d5
                lsl.w   #3,d5
                tst.b   (a0,d5.w)
                bne.s   loc_383E
                move.b  d2,(a0,d5.w)
                move.b  d7,4(a0,d5.w)
                subq.w  #1,d4
                bne.s   loc_383E
                moveq   #4,d0
                bra.s   loc_383E
; ---------------------------------------------------------------------------

loc_3828:                               ; CODE XREF: ROM:000037F8↑j
                subi.w  #$61,d1 ; 'a'
                bls.s   loc_383E
                cmpi.w  #5,d1
                bcc.s   loc_383E
                tst.w   d5
                beq.s   loc_383E
                add.w   d5,d1
                move.b  d2,(a0,d1.w)

loc_383E:                               ; CODE XREF: ROM:000037DE↑j
                                        ; ROM:00003804↑j ...
                dbf     d0,loc_37E0
                tst.w   d4
                beq.s   loc_3850
                move.l  #$FFFF0000,(a0)
                bra.w   loc_3906
; ---------------------------------------------------------------------------

loc_3850:                               ; CODE XREF: ROM:00003844↑j
                move.w  (a0),d0
                move.w  d0,d1
                lsr.w   #8,d0
                andi.w  #$FF,d1
                sub.w   d0,d1
                lsl.w   #3,d0
                movea.l a0,a1
                adda.w  d0,a1
                addq.l  #4,a1
                lea     8(a1),a2
                bra.s   loc_389A
; ---------------------------------------------------------------------------

loc_386A:                               ; CODE XREF: ROM:loc_389A↓j
                                        ; ROM:000038AC↓j
                move.b  -(a2),d5
                sub.b   -(a1),d5
                bcc.s   loc_3874
                add.b   #$4B,d5 ; 'K'

loc_3874:                               ; CODE XREF: ROM:0000386E↑j
                move.b  d5,4(a1)
                move.b  -(a2),d5
                move.b  -(a1),d4
                subx.b  d4,d5
                bcc.s   loc_3884
                add.b   #$3C,d5 ; '<'

loc_3884:                               ; CODE XREF: ROM:0000387E↑j
                move.b  d5,4(a1)
                move.b  -(a2),d5
                move.b  -(a1),d4
                subx.b  d4,d5
                move.b  d5,4(a1)
                addq.l  #8,a1
                addq.l  #3,a1
                addq.l  #8,a2
                addq.l  #3,a2

loc_389A:                               ; CODE XREF: ROM:00003868↑j
                dbf     d1,loc_386A
                lea     $D(a0),a5
                cmpa.l  a2,a5
                bcc.s   loc_38AE
                lea     5(a0),a2
                moveq   #0,d1
                bra.s   loc_386A
; ---------------------------------------------------------------------------

loc_38AE:                               ; CODE XREF: ROM:000038A4↑j
                tst.w   d6
                bpl.s   loc_38D2
                move.w  #$1800,d0
                moveq   #1,d1
                bsr.w   sub_390E
                moveq   #$1E,d0

loc_38BE:                               ; CODE XREF: ROM:loc_38BE↓j
                dbf     d0,loc_38BE
                move.w  #$5400,d0
                moveq   #1,d1
                bsr.w   sub_390E
                move.w  d2,d6
                andi.w  #$FF,d6

loc_38D2:                               ; CODE XREF: ROM:000038B0↑j
                addq.w  #1,d7
                cmp.w   d6,d7
                bcs.w   loc_3766
                move.w  #$1800,d0
                moveq   #1,d1
                bsr.w   sub_390E
                move.l  2(a0),d0
                addq.l  #1,d0
                clr.w   (a0)
                ror.l   #8,d0
                move.w  6(a0),d1
                move.w  d1,2(a0)
                move.l  d0,4(a0)
                move.w  #0,($3E00).w

loc_3900:                               ; CODE XREF: ROM:0000390C↓j
                movem.l (sp)+,d0-d7/a1-a6
                rts
; ---------------------------------------------------------------------------

loc_3906:                               ; CODE XREF: ROM:00003780↑j
                                        ; ROM:0000384C↑j
                move.w  #1,($3E00).w
                bra.s   loc_3900

; =============== S U B R O U T I N E =======================================


sub_390E:                               ; CODE XREF: ROM:0000376E↑p
                                        ; ROM:000038B8↑p ...
                move.l  a3,-(sp)
                move.l  (a4),d2
                btst    #$D,d2
                beq.w   loc_3922
                move.w  $A(a4),d2
                tst.l   4(a4)

loc_3922:                               ; CODE XREF: sub_390E+8↑j
                move.w  d0,$A(a4)
                bra.s   loc_394C
; ---------------------------------------------------------------------------

loc_3928:                               ; CODE XREF: sub_390E+26↓j
                                        ; sub_390E:loc_394C↓j
                moveq   #$F,d2

loc_392A:                               ; CODE XREF: sub_390E:loc_392A↓j
                dbf     d2,loc_392A
                move.l  (a4),d2
                btst    #$D,d2
                beq.s   loc_3928
                move.w  $A(a4),d2
                move.w  d2,(a3)+
                tst.l   4(a4)
                ror.w   #8,d2
                cmpi.b  #4,d2
                bne.s   loc_394A
                moveq   #0,d1

loc_394A:                               ; CODE XREF: sub_390E+38↑j
                ror.w   #8,d2

loc_394C:                               ; CODE XREF: sub_390E+18↑j
                dbf     d1,loc_3928
                movea.l (sp)+,a3
                rts
; End of function sub_390E

; ---------------------------------------------------------------------------

CDUnused4:                              ; CODE XREF: ROM:CD_unused4↑j
                move.l  #$100000,($DFFF00).l

loc_395E:                               ; CODE XREF: ROM:0000397A↓j
                                        ; ROM:00003980↓j
                move.w  #$5000,($DFFF0A).l
                bsr.w   sub_39CC
                move.w  ($DFFF0A).l,d0
                move.w  d0,d1
                and.w   #$FF00,d1
                cmp.w   #$300,d1
                bne.s   loc_395E
                btst    #0,d0
                bne.s   loc_395E

loc_3982:                               ; CODE XREF: ROM:000039B0↓j
                                        ; ROM:000039B6↓j
                move.w  #$200,($DFFF0A).l
                bsr.w   sub_39CC
                move.w  ($DFFF0A).l,d0
                cmp.w   #$402,d0
                bne.w   loc_39B2
                move.w  #$200,d1

loc_39A0:                               ; CODE XREF: ROM:000039AA↓j
                                        ; ROM:000039AC↓j
                move.w  ($F00006).l,d0
                cmp.w   #1,d0
                bne.s   loc_39A0
                dbf     d1,loc_39A0
                bra.s   loc_3982
; ---------------------------------------------------------------------------

loc_39B2:                               ; CODE XREF: ROM:00003998↑j
                cmp.w   #$200,d0
                bne.s   loc_3982
                move.l  #$180000,($DFFF00).l
                movea.l #$2C00,a0
                bra.w   CDUnused1

; =============== S U B R O U T I N E =======================================


sub_39CC:                               ; CODE XREF: ROM:00003966↑p
                                        ; ROM:0000398A↑p ...
                move.w  #$1000,d1

loc_39D0:                               ; CODE XREF: sub_39CC:loc_39D0↓j
                dbf     d1,loc_39D0
                move.l  ($DFFF00).l,d1
                and.l   #$2000,d1
                beq.s   sub_39CC
                move.l  ($DFFF04).l,d1
; End of function sub_39CC

; ---------------------------------------------------------------------------
                dc.b $4E
                dc.b $75 ; u
; end of 'ROM'


                END
