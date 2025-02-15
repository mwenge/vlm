; *******************************************************************
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
; *******************************************************************
; free    EQU $00192018 ; start of Free run code
; audio   EQU $0019203E ; start of audio reactive
; goag    EQU $0019207A ; real start of audio reactive
; board   EQU $001B00F8 ; base of 1-bit/pixel 320x240 display
; frames    EQU $001AE4D2 ;.w Jeff's frame count
; pad_now   EQU $001AE00C ;.w Jeff's joystick word
; freerun   EQU $00198CAC ;.w put a 2 in here to stop GPU in free run
; davesvec  EQU $00199994 ;.l vector for dave's mainloop
; cursimg   EQU $00199A18 ; cursor image (to be cleared)
; davesobj  EQU $001AE148 ; base addr of dave's overlay obj
; vlm_mode  EQU $001AE02A ;.w 0=no VLM controls, 1=VLM controls active
; skid    EQU $001AF05E ;.w 0..9 setting for free running screen
; ObTypes   EQU $00196AA8 ; ptr to object type definitions
; skidoo    EQU $00195076 ; value to stuff in 'action'
; imatrix   EQU $001AF068 ;.w 0..9 bank number
; action    EQU $001AE524 ; vector to initiate bank/effect switch
; gm    EQU $00196814 ; value to stuff in 'action'
; beasties  EQU $001AE048 ; start of object list (davesobj is subset)
; print   EQU $0019483A ; routine to print errors
; cursx   EQU $001AE520 ;.w horz position associated with 'print'
; cursy   EQU $001AE522 ;.w vert position associated with 'print'
; iansdoit  EQU $001AC1A0 ; entry point for DSP init routine
; vlmtim    EQU $001AF06E ;.w turn-off timer for VLM logo
; blist   EQU $001AEC7C ;.l ptr to build (shadow) display list
; dlist   EQU $001AEC80 ;.l ptr to hardware display list
; RunBeast  EQU $00195B4C ; Jeff's object build routine
; Frame   EQU $00194C90 ; Jeff's v_blank routine
; readpad   EQU $00196446 ; Jeff's joystick routine
; vlmlogo   EQU $0019ADD0 ; Base addr of VLM logo grafix
; no_ksel   EQU $00194F66 ;vlm label,@ +8.w bit15 set disables vlmedit
; pal   EQU $001FD426 ;.w =1 if on PAL system, =0 for NTSC
; myFont    EQU $00198CB0 ;base address of internal font
fxspace EQU $1B63F8
vfb_ysca    EQU $10
star        EQU $10000
screen1     EQU $100000
ntsc_vmi    EQU $10A
vfb_angl    EQU $14
width       EQU $180
zerstart    EQU $1AE000
d2elayp     EQU $1AE002
d2line      EQU $1AE004
d2elayt     EQU $1AE008
d2elayn     EQU $1AE00A
pad_now     EQU $1AE00C
pad_shot    EQU $1AE018
zero        EQU $1AE020
star_on     EQU $1AE024
editing     EQU $1AE028
vlm_mode    EQU $1AE02A
ovlm_mod    EQU $1AE02C
bank_mod    EQU $1AE02E
started     EQU $1AE030
flash       EQU $1AE032
scron       EQU $1AE034
editlist    EQU $1AE036
selx        EQU $1AE03A
sely        EQU $1AE03C
selectable    EQU $1AE03E
selected    EQU $1AE040
nbeastie    EQU $1AE042
beasties    EQU $1AE048
davesobj    EQU $1AE148
routine     EQU $1AE4C0
_fx         EQU $1AE4C4
draw_routine    EQU $1AE4C8
db_on       EQU $1AE4CC
frames      EQU $1AE4D2
fframes     EQU $1AE4D4
palside     EQU $1AE4D8
paltop      EQU $1AE4DA
intmask     EQU $1AE4DC
ofree       EQU $1AE4E0
activeob    EQU $1AE4E2
freeobje    EQU $1AE4E6
x_end       EQU $1AE4EA
monitor     EQU $1AE4EC
monitand    EQU $1AE4EE
monptr      EQU $1AE4F0
monx        EQU $1AE4F2
mony        EQU $1AE4F4
symed       EQU $1AE4F6
cursx       EQU $1AE520
cursy       EQU $1AE522
action      EQU $1AE524
inverse     EQU $1AE528
mon2        EQU $1AE52A
mon1        EQU $1AE52E
edwave      EQU $1AE532
fx          EQU $1AE536
fxed        EQU $1AE53A
fxedbase    EQU $1AE548
fx1         EQU $1AE550
cbuttf      EQU $1AE568
_m          EQU $1AE57A
absdelta    EQU $1AE57C
udud        EQU $1AE77C
aded        EQU $1AE780
cskr        EQU $1AE788
list1       EQU	$1AE81C
list2	      EQU $1AEA8C
blist       EQU $1AEC7C
dlist       EQU $1AEC80
ddlist      EQU $1AEC84
stack	      EQU $1AEFA8
X_1         EQU $1AEFB0
Y_1         EQU $1AEFB2
X_2         EQU $1AEFB4
Y_2         EQU $1AEFB6
LINCOL      EQU $1AEFB8
XLO         EQU $1AEFBA
XHI         EQU $1AEFBC
YLO         EQU $1AEFBE
YHI         EQU $1AEFC0
XCEN        EQU $1AEFC2
YCEN        EQU $1AEFC6
RXCEN       EQU $1AEFCA
RYCEN       EQU $1AEFCE
hango       EQU $1AEFD2
ov          EQU $1AEFD6
cwave1      EQU $1AEFDA
cwave2      EQU $1AEFDC
cwed1       EQU $1AEFDE
cwed2       EQU $1AEFE2
fxobj       EQU $1AEFE6
i_fxobj     EQU $1AEFEA
lselecte    EQU $1AF00A
antelope    EQU $1AF00C
bandh       EQU $1AF00E
bandl       EQU $1AF010
band        EQU $1AF012
actime      EQU $1AF038
cursorx     EQU $1AF03A
cursory     EQU $1AF03C
seldb       EQU $1AF03E
fass        EQU $1AF040
elvp        EQU $1AF042
eddie       EQU $1AF046
iixcon      EQU $1AF04A
iiycon      EQU $1AF04E
cpad        EQU $1AF052
esp         EQU $1AF05A
skid        EQU $1AF05E
nphrase     EQU $1AF060
og          EQU $1AF062
ledit       EQU $1AF064
imatrix     EQU $1AF068
snoop       EQU $1AF06A
skale       EQU $1AF06C
vlmtim      EQU $1AF06E
sec_cnt     EQU $1AF070
sec_en      EQU $1AF072
meltim      EQU $1AF074
dvf_buf     EQU $1AF078
objects     EQU $1AF0F8
board       EQU $1B00F8
refblock    EQU $1B4BF8
delaylin    EQU $1C03F8
matrix      EQU $1D83F8
e_stac      EQU $1E5CFC
p_sines     EQU $1E7540
p_saw       EQU $1E7644
p_square    EQU $1E7748
p_ramp      EQU $1E784C
p_rect      EQU $1E7950
elspace     EQU $1E7A54
elinx       EQU $1E7B54
eltxt       EQU $1E7C54
elvex       EQU $1E8054
envvals     EQU $1E80B4
monbuf EQU $1E80F4
moomoomo    EQU $1E80D4
results     EQU $1E81F4
oscvals EQU $1E8094
cobuf EQU $1E8274
monoobj     EQU $1F4A78
cubeobj     EQU $1F5278
genobj      EQU $1F927C
spherobj    EQU $1FD280
maxes       EQU $1FD314
dbadj       EQU $1FD414
bo          EQU $1FD418
flash_db    EQU $1FD41C
vedit       EQU $1FD41E
height2     EQU $1FD420
n_vdb       EQU $1FD422
n_vde       EQU $1FD424
pal         EQU $1FD426
width2      EQU $1FD428
a_hdb       EQU $1FD42A
a_hde       EQU $1FD42C
dstoffx     EQU $24
dstoffy     EQU $28
vfb_xpos    EQU $2C
vfb_ypos    EQU $30
CD_init     EQU $3000
CD_mode     EQU $3006
CD_jeri     EQU $3012
CD_read     EQU $303C
CD_setup    EQU $3048
CD_osamp    EQU $3054
dstoffz     EQU $34
vectorx     EQU 56
vectory     EQU 64
d_x         EQU $38
dy          EQU $3C
d_z         EQU $40
padleft     EQU $400000
rsym_ord    EQU $48
rsym_ste    EQU $4C
rsym_ist    EQU $50
_i1         EQU $54
_i2         EQU $58
_i3         EQU $5C
_i4         EQU $60
_i5         EQU $64
_i6         EQU $68
zamp        EQU $6C
phase1      EQU $70
phase2      EQU $74
phase4      EQU $7C
i           EQU $80
padright    EQU $800000
j           EQU $84
k           EQU $88
col1        EQU $90
datn        EQU $900000
datp        EQU $900004
dattp       EQU $900008
dattb       EQU $900010
datdb       EQU $900200
deltaz      EQU $94
thang       EQU $98
asym_fla    EQU $A0
sine_bas    EQU $A4
rxcen       EQU $A8
rycen       EQU $AC
roscale     EQU $B0
roscal2     EQU $B4
cvx         EQU $B8
cvy         EQU $BC
vfb_xsca    EQU $C
roscalei    EQU $C0
drxcen      EQU $C4
drycen      EQU $C8
phase5      EQU $CC
wave_2      EQU $D0
radius      EQU $D4
cvx2        EQU $D8
cvy2        EQU $DC
colx        EQU $E0
coly        EQU $E4
plot_mod    EQU $E8
pixsize     EQU $EC
height      EQU $F0
allpad      EQU $F00000
A1_FPIXE    EQU $F02218
trig        EQU $F03F3C
dscreen     EQU $F03F40
cscreen     EQU $F03F44
mods        EQU $F03F48
sync        EQU $F03F4C
screen_ready    EQU $F03F50
draw_screen    EQU $F03F54
in_buf      EQU $F03F60
JOYSTICK    EQU $F14000
fft_on      EQU $F1B020
VOLUME      EQU $F1BD60
_mtrig      EQU $F4
info        EQU $F8
gpu         EQU $FC
dvf_ws_x     EQU 4
dvf_ws_y     EQU 8
dvf_crot_x   EQU $18
dvf_crot_y   EQU $1C
dvf_deltai   EQU $20
; vim:ft=asm68k ts=2
