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
; sines.s
;
; A sine table.
; ******************************************************************************
sines:          dc.b $00,$03,$06,$09,$0C,$0F,$12,$15
                dc.b $18,$1B,$1E,$21,$24,$27,$2A,$2D
                dc.b $30,$33,$36,$39,$3B,$3E,$41,$44
                dc.b $46,$49,$4B,$4E,$50,$53,$55,$57
                dc.b $59,$5C,$5E,$60,$62,$64,$66,$67
                dc.b $69,$6B,$6D,$6E,$70,$71,$72,$74
                dc.b $75,$76,$77,$78,$79
                dc.b $7A,$7B,$7B,$7C,$7D,$7D,$7E,$7E
                dc.b $7E,$7E,$7E,$7E,$7E,$7E,$7E,$7E
                dc.b $7E,$7D,$7D,$7C,$7B,$7B,$7A,$79
                dc.b $78,$77,$76,$75,$73,$72,$71,$6F
                dc.b $6E,$6C,$6B,$69,$67,$65,$63,$61
                dc.b $5F,$5D,$5B,$59,$57,$55,$52,$50
                dc.b $4D,$4B,$48,$46,$43,$40,$3E,$3B
                dc.b $38,$35,$33,$30,$2D,$2A,$27,$24
                dc.b $21,$1E,$1B,$18,$15,$12,$0F,$0C
                dc.b $08,$05,$02,$00,$FD,$FA,$F7,$F4
                dc.b $F0,$ED,$EA,$E7,$E4,$E1,$DE,$DB
                dc.b $D8,$D5,$D2,$CF,$CD,$CA,$C7,$C4
                dc.b $C1,$BF,$BC,$B9,$B7,$B4,$B2,$B0
                dc.b $AD,$AB,$A9,$A6,$A4,$A2,$A0,$9E
                dc.b $9C,$9A,$98,$97,$95,$93,$92,$90
                dc.b $8F,$8D,$8C,$8B,$8A,$89,$88,$87
                dc.b $86,$85,$84,$84,$83,$83,$82,$82
                dc.b $82,$82,$82,$82,$82,$82,$82,$82
                dc.b $83,$83,$83,$84,$85,$85,$86,$87
                dc.b $88,$89,$8A,$8B,$8D,$8E,$8F,$91
                dc.b $92,$94,$96,$97,$99,$9B,$9D,$9F
                dc.b $A1,$A3,$A5,$A7,$AA,$AC,$AE,$B1
                dc.b $B3,$B6,$B8,$BB,$BD,$C0,$C3,$C5
                dc.b $C8,$CB,$CE,$D1,$D4,$D7,$D9,$DC
                dc.b $DF,$E2,$E6,$E9,$EC,$EF,$F2,$F5
                dc.b $F8,$FB,$FE,$00,$00,$01,$92,$03
                dc.b $23,$04,$B5,$06,$45,$07,$D5,$09
                dc.b $63,$0A,$F0,$0C,$7C,$0E,$05,$0F
                dc.b $8C,$11,$11,$12,$93,$14,$13,$15
                dc.b $8F,$17,$08,$18,$7D,$19,$EF,$1B
                dc.b $5C,$1C,$C5,$1E,$2A,$1F,$8B,$20
                dc.b $E6,$22,$3C,$23,$8D,$24,$D9,$26
                dc.b $1F,$27,$5F,$28,$99,$29,$CC,$2A
                dc.b $FA,$2C,$20,$2D,$40,$2E,$59,$2F
                dc.b $6B,$30,$75,$31,$78,$32,$73,$33
                dc.b $66,$34,$52,$35,$35,$36,$11,$36
                dc.b $E4,$37,$AE,$38,$70,$39,$29,$39
                dc.b $DA,$3A,$81,$3B,$1F,$3B,$B5,$3C
                dc.b $41,$3C,$C4,$3D,$3D,$3D,$AD,$3E
                dc.b $14,$3E,$70,$3E,$C4,$3F,$0D,$3F
                dc.b $4D,$3F,$83,$3F,$B0,$3F,$D2,$3F
                dc.b $EB,$3F,$FA,$3F,$FF,$3F,$FA,$3F
                dc.b $EB,$3F,$D2,$3F,$B0,$3F,$83,$3F
                dc.b $4D,$3F,$0D,$3E,$C4,$3E,$70,$3E
                dc.b $14,$3D,$AD,$3D,$3D,$3C,$C4,$3C
                dc.b $41,$3B,$B5,$3B,$1F,$3A,$81,$39
                dc.b $DA,$39,$29,$38,$70,$37,$AE,$36
                dc.b $E4,$36,$11,$35,$35,$34,$52,$33
                dc.b $66,$32,$73,$31,$78,$30,$75,$2F
                dc.b $6B,$2E,$59,$2D,$40,$2C,$20,$2A
                dc.b $FA,$29,$CC,$28,$99,$27,$5F,$26
                dc.b $1F,$24,$D9,$23,$8D,$22,$3C,$20
                dc.b $E6,$1F,$8B,$1E,$2A,$1C,$C5,$1B
                dc.b $5C,$19,$EF,$18,$7D,$17,$08,$15
                dc.b $8F,$14,$13,$12,$93,$11,$11,$0F
                dc.b $8C,$0E,$05,$0C,$7C,$0A,$F0,$09
                dc.b $63,$07,$D5,$06,$45,$04,$B5,$03
                dc.b $23,$01,$92,$00,$00,$FF,$6E,$FD
                dc.b $DD,$FC,$4B,$FA,$BB,$F9,$2B,$F7
                dc.b $9D,$F6,$10,$F4,$84,$F2,$FB,$F1
                dc.b $74,$EF,$EF,$EE,$6D,$EC,$ED,$EB
                dc.b $71,$E9,$F8,$E8,$83,$E7,$11,$E5
                dc.b $A4,$E4,$3B,$E2,$D6,$E1,$75,$E0
                dc.b $1A,$DE,$C4,$DD,$73,$DC,$27,$DA
                dc.b $E1,$D9,$A1,$D8,$67,$D7,$34,$D6
                dc.b $06,$D4,$E0,$D3,$C0,$D2,$A7,$D1
                dc.b $95,$D0,$8B,$CF,$88,$CE,$8D,$CD
                dc.b $9A,$CC,$AE,$CB,$CB,$CA,$EF,$CA
                dc.b $1C,$C9,$52,$C8,$90,$C7,$D7,$C7
                dc.b $26,$C6,$7F,$C5,$E1,$C5,$4B,$C4
                dc.b $BF,$C4,$3C,$C3,$C3,$C3,$53,$C2
                dc.b $EC,$C2,$90,$C2,$3C,$C1,$F3,$C1
                dc.b $B3,$C1,$7D,$C1,$50,$C1,$2E,$C1
                dc.b $15,$C1,$06,$C1,$01,$C1,$06,$C1
                dc.b $15,$C1,$2E,$C1,$50,$C1,$7D,$C1
                dc.b $B3,$C1,$F3,$C2,$3C,$C2,$90,$C2
                dc.b $EC,$C3,$53,$C3,$C3,$C4,$3C,$C4
                dc.b $BF,$C5,$4B,$C5,$E1,$C6,$7F,$C7
                dc.b $26,$C7,$D7,$C8,$90,$C9,$52,$CA
                dc.b $1C,$CA,$EF,$CB,$CB,$CC,$AE,$CD
                dc.b $9A,$CE,$8D,$CF,$88,$D0,$8B,$D1
                dc.b $95,$D2,$A7,$D3,$C0,$D4,$E0,$D6
                dc.b $06,$D7,$34,$D8,$67,$D9,$A1,$DA
                dc.b $E1,$DC,$27,$DD,$73,$DE,$C4,$E0
                dc.b $1A,$E1,$75,$E2,$D6,$E4,$3B,$E5
                dc.b $A4,$E7,$11,$E8,$83,$E9,$F8,$EB
                dc.b $71,$EC,$ED,$EE,$6D,$EF,$EF,$F1
                dc.b $74,$F2,$FB,$F4,$84,$F6,$10,$F7
                dc.b $9D,$F9,$2B,$FA,$BB,$FC,$4B,$FD
                dc.b $DD,$FF,$6E,$00,$00,$00,$00
; vim:ft=asm68k ts=2
