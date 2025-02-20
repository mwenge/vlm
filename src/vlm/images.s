.globl polyos

polyos:         dc.l vlmlogo2        ; DATA XREF: sub_192088+D80↑o
                dc.l chev1
                dc.l chev
                dc.l square
                dc.l chev2
                dc.l chev3
                dc.l chev4
                dc.l chev5

chev:           dc.l $20050, $600048, 0 ; DATA XREF: ROM:001ABB10↑o
                dc.l $10004, $20008, $49, $30000, $10004, $20008, 0
                dc.l $C00050, $50, $8000A0
                dc.b 0, $C0

chev5:          dc.w 2                  ; DATA XREF: ROM:001ABB24↑o
                dc.l $500060, $FE0000, $01, $40002, $80000, $FD0003, $01, $40002
                dc.l $80000, $C0, $500000, $500080, $A000C0

chev1:          dc.l $20050, $6000F1, 0 ; DATA XREF: ROM:001ABB0C↑o
                dc.l $10004, $20008, $F2, $30000, $10004, $20008, 0
                dc.l $C00050, $50, $8000A0
                dc.b 0, $C0

chev2:          dc.w 2                  ; DATA XREF: ROM:001ABB18↑o
                dc.l $500060, $810000, $01, $40002, $80000, $820003, $01, $40002
                dc.l $80000, $C0, $500000, $500080, $A000C0

chev3:          dc.l $20050, $60008F, 0 ; DATA XREF: ROM:001ABB1C↑o
                dc.l $10004, $20008, $8E, $30000, $10004, $20008, 0
                dc.l $C00050, $50, $8000A0
                dc.b 0, $C0

chev4:          dc.w 2                  ; DATA XREF: ROM:001ABB20↑o
                dc.l $500060, $120000, $01, $40002, $80000, $130003, $01, $40002
                dc.l $80000, $C0, $500000, $500080, $A000C0

square:         dc.l $40100, $1000004, $10000, $20004, $10, $04, $20004
                dc.l $40008, $10, $04, $40008, $3000C, $10, $04, $3000C, $10000
                dc.l $10, $100, $1000000, $200, 0
                dc.l $2000200
                dc.b 2, 0

vlmlogo2:       .incbin "images/vlmlogo2.cry"

; vim:ft=asm68k ts=2
