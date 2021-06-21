.include "../jaguar.inc"
.include "../blitter.inc"
.include "vlm.inc"

.globl _fsync
.globl _fmode
.globl _ein_buf
.globl dword_1A6810
.globl VideoIni
.globl ispal
.globl doit

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
