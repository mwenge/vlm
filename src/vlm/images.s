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
; images.s
; ******************************************************************************

.globl polyos

; *******************************************************************
; polyos
; *******************************************************************
polyos:
                dc.l vlmlogo2       
                dc.l chev1
                dc.l chev
                dc.l square
                dc.l chev2
                dc.l chev3
                dc.l chev4
                dc.l chev5

chev:            
                dc.b  $00,$02,$00,$50,$00,$60,$00,$48
                dc.b  $00,$00,$00,$00,$00,$01,$00,$04
                dc.b  $00,$02,$00,$08,$00,$00,$00,$49
                dc.b  $00,$03,$00,$00,$00,$01,$00,$04
                dc.b  $00,$02,$00,$08,$00,$00,$00,$00
                dc.b  $00,$C0,$00,$50,$00,$00,$00,$50
                dc.b  $00,$80,$00,$A0,$00,$C0

chev5:         
                dc.b  $00,$02,$00,$50,$00,$60,$00,$FE
                dc.b  $00,$00,$00,$00,$00,$01,$00,$04
                dc.b  $00,$02,$00,$08,$00,$00,$00,$FD
                dc.b  $00,$03,$00,$00,$00,$01,$00,$04
                dc.b  $00,$02,$00,$08,$00,$00,$00,$00
                dc.b  $00,$C0,$00,$50,$00,$00,$00,$50
                dc.b  $00,$80,$00,$A0,$00,$C0

chev1:         
                dc.b  $00,$02,$00,$50,$00,$60,$00,$F1
                dc.b  $00,$00,$00,$00,$00,$01,$00,$04
                dc.b  $00,$02,$00,$08,$00,$00,$00,$F2
                dc.b  $00,$03,$00,$00,$00,$01,$00,$04
                dc.b  $00,$02,$00,$08,$00,$00,$00,$00
                dc.b  $00,$C0,$00,$50,$00,$00,$00,$50
                dc.b  $00,$80,$00,$A0,$00,$C0

chev2:          
                dc.b  $00,$02,$00,$50,$00,$60,$00,$81
                dc.b  $00,$00,$00,$00,$00,$01,$00,$04
                dc.b  $00,$02,$00,$08,$00,$00,$00,$82
                dc.b  $00,$03,$00,$00,$00,$01,$00,$04
                dc.b  $00,$02,$00,$08,$00,$00,$00,$00
                dc.b  $00,$C0,$00,$50,$00,$00,$00,$50
                dc.b  $00,$80,$00,$A0,$00,$C0

chev3:          
                dc.b  $00,$02,$00,$50,$00,$60,$00,$8F
                dc.b  $00,$00,$00,$00,$00,$01,$00,$04
                dc.b  $00,$02,$00,$08,$00,$00,$00,$8E
                dc.b  $00,$03,$00,$00,$00,$01,$00,$04
                dc.b  $00,$02,$00,$08,$00,$00,$00,$00
                dc.b  $00,$C0,$00,$50,$00,$00,$00,$50
                dc.b  $00,$80,$00,$A0,$00,$C0

chev4:         
                dc.b  $00,$02,$00,$50,$00,$60,$00,$12 
                dc.b  $00,$00,$00,$00,$00,$01,$00,$04 
                dc.b  $00,$02,$00,$08,$00,$00,$00,$13 
                dc.b  $00,$03,$00,$00,$00,$01,$00,$04 
                dc.b  $00,$02,$00,$08,$00,$00,$00,$00 
                dc.b  $00,$C0,$00,$50,$00,$00,$00,$50 
                dc.b  $00,$80,$00,$A0,$00,$C0


square:         dc.b  $00,$04,$01,$00,$01,$00,$00,$04 
                dc.b  $00,$01,$00,$00,$00,$02,$00,$04 
                dc.b  $00,$00,$00,$10,$00,$00,$00,$04 
                dc.b  $00,$02,$00,$04,$00,$04,$00,$08 
                dc.b  $00,$00,$00,$10,$00,$00,$00,$04 
                dc.b  $00,$04,$00,$08,$00,$03,$00,$0C 
                dc.b  $00,$00,$00,$10,$00,$00,$00,$04 
                dc.b  $00,$03,$00,$0C,$00,$01,$00,$00 
                dc.b  $00,$00,$00,$10,$00,$00,$01,$00 
                dc.b  $01,$00,$00,$00,$00,$00,$02,$00 
                dc.b  $00,$00,$00,$00,$02,$00,$02,$00 
                dc.b  $02,$00


; *******************************************************************
; vlmlogo2
; Contains a character font.
; *******************************************************************
vlmlogo2:       .incbin "images/vlmlogo2.cry"

; vim:ft=asm68k ts=2
