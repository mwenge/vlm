free		EQU	$00192018 ; start of Free run code
audio		EQU	$0019203E ; start of audio reactive
goag		EQU	$0019207A ; real start of audio reactive
board		EQU	$001B00F8 ; base of 1-bit/pixel 320x240 display
frames		EQU	$001AE4D2 ;.w Jeff's frame count
pad_now		EQU	$001AE00C ;.w Jeff's joystick word
freerun		EQU	$00198C8C ;.w put a 2 in here to stop GPU in free run
davesvec	EQU	$00199974 ;.l vector for dave's mainloop
cursimg		EQU	$001999F8 ; cursor image (to be cleared)
davesobj	EQU	$001AE148 ; base addr of dave's overlay obj
vlm_mode	EQU	$001AE02A ;.w 0=no VLM controls, 1=VLM controls active
skid		EQU	$001AF05E ;.w 0..9 setting for free running screen
ObTypes		EQU	$00196A88 ; ptr to object type definitions
skidoo		EQU	$00195050 ; value to stuff in 'action'
imatrix		EQU	$001AF068 ;.w 0..9 bank number
action		EQU	$001AE524 ; vector to initiate bank/effect switch
gm		EQU	$001967EE ; value to stuff in 'action'
beasties	EQU	$001AE048 ; start of object list (davesobj is subset)
print		EQU	$00194814 ; routine to print errors
cursx		EQU	$001AE520 ;.w horz position associated with 'print'
cursy		EQU	$001AE522 ;.w vert position associated with 'print'
iansdoit	EQU	$001AC168 ; entry point for DSP init routine
vlmtim		EQU	$001AF06E ;.w turn-off timer for VLM logo
blist		EQU	$001AEC7C ;.l ptr to build (shadow) display list
dlist		EQU	$001AEC80 ;.l ptr to hardware display list
RunBeast	EQU	$00195B26 ; Jeff's object build routine
Frame		EQU	$00194C6A ; Jeff's v_blank routine
readpad		EQU	$00196420 ; Jeff's joystick routine
vlmlogo		EQU	$0019ADB0 ; Base addr of VLM logo grafix
no_ksel		EQU	$00194F40 ;vlm label,@ +8.w bit15 set disables vlmedit
pal		EQU	$001FD406 ;.w =1 if on PAL system, =0 for NTSC
