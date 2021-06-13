;*======================================================================*
;* TITLE	: OBJLIST.INC						*
;* Function	: Hardware Equates for OBJECT list			*
;*									*
;* Project #	: JAGUAR						*
;*									*
;* MODIFIED	: Pradip K Fatehpuria	10/27/93			*
;*									*
;* COPYRIGHT(c) 1992,1993 Atari Computer Corporation			*
;* UNATHORIZED REPRODUCTION, ADAPTATION, DISTRIBUTION, PERFORMANCE OR	*
;* DISPLAY OF THIS COMPUTER PROGRAM OR THE ASSOCIATED AUDIOVISUAL WORK	*
;* IS STRICTLY PROHIBITED.						*
;* ALL RIGHTS RESERVED.							*
;*									*
;*======================================================================*

MAXOBJS		EQU	10

;*======================================================================*
;*	OFFSETS FOR EACH OBJECT IN THE USER OBJECT LIST			*
;*======================================================================*

O_XPOS		EQU	0		; word	object xpos
O_YPOS		EQU	O_XPOS+2	; word	object ypos in half lines
O_DATA		EQU	O_YPOS+2	; long	phrase aligned RAM address of object data
O_HEIGHT	EQU	O_DATA+4	; word	height of object in pixels
O_DWIDTH	EQU	O_HEIGHT+2	; word	phrases per line of data in object (bytes per line / 8)
O_IWIDTH	EQU	O_DWIDTH+2	; word	phrases per line (bytes per line / 8)
					;	this is how many phrases of data to draw per line
					;	this should be the same as O_DWIDTH to draw the whole object
O_FLAGS		EQU	O_IWIDTH+2	; byte	flags for reflect, RMW, transparent, release
					;	settings for bits in O_FLAGS byte

	HFLIP_OFF	EQU	0
	HFLIP_ON	EQU	1	; bit 0 = horizontal flip (reflect)
	RMW_OFF		EQU	0
	RMW_ON		EQU	2	; bit 1 = RMW flag
	TRANS_OFF	EQU	0
	TRANS_ON	EQU	4	; bit 2 = transparent flag
	RELEASE_OFF	EQU	0
	RELEASE_ON	EQU	8	; bit 3 = release

O_FIRSTPIX	EQU	O_FLAGS+1	; byte	first pixel to be displayed in one line of an object
					;	pixels to skip = O_FIRSTPIX / bits per pixel
O_TYPE		EQU	O_FIRSTPIX+1	; byte	object type
					;	0 = bit mapped object
					;	1 = scaled bit mapped object
					;	2 = graphics processor object
					;	3 = branch object
					;	4 = stop object
	DELETE_OBJ	EQU	$80	; if negative bit is set in O_TYPE
					;	object is not included in the final object list

O_DEPTH		EQU	O_TYPE+1	; byte	pixel depth
					;	0 = 1 bit per pixel
					;	1 = 2 bits per pixel
					;	2 = 4 bits per pixel
					;	3 = 8 bits per pixel
					;	4 = 16 bits per pixel (CRY)
					;	5 = 24 bits per pixel
O_PITCH		EQU	O_DEPTH+1	; byte	pitch
					;	how many phrases to skip each time a new phrase is fetched
					;	0 = repeat the same phrase
					;	1 = contiguous data
					;	2 = get every other phrase
					;	3 = get every third phrase, etc.
O_INDEX		EQU	O_PITCH+1	; byte	palette index
					;	4 bits per pixel objects can use 0 - 120 (multiples of 8 only)
					;	2 bits per pixel objects can use 0 - 126 (multiples of 2 only)
					;	1 bit  per pixel objects can use 0 - 127
					; starting color number will be index * 2
O_DESC		EQU	O_INDEX+1	; word	object description (can be set to anything by user - this is not used by object list builder)
O_SCALE		EQU	O_DESC+2	; long	scaling information to write in 2nd long of phrase 3
					;	1st byte is unused
O_REMAINDER	EQU	O_SCALE+1	; 2nd byte is remainder, same as vscale
O_VSCALE	EQU	O_REMAINDER+1	; 3rd byte is vscale, 3 bits + 5 bit fraction
O_HSCALE	EQU	O_VSCALE+1	; 4th byte is hscale, 3 bits + 5 bit fraction

O_ROM		EQU	O_SCALE+4	; ROM address of object image
O_MODE		EQU	O_ROM+4		; Object flags
OBJSIZE		EQU	O_MODE+4	; total bytes used for one object


;*======================================================================*
;*	MODE VALUES							*
;*======================================================================*

M_EMPTY		EQU	$00000000	; inactive or empty Object
M_ACTIVE	EQU	$00000001	; Active Object
M_UNUSED1	EQU	$00000002	; Moveable Object
M_UNUSED2	EQU	$00000004	; Seperatable Object action
M_UNUSED3	EQU	$00000008	; left-hand piece (special case)
M_UNUSED4	EQU	$00000010	; unused
M_UNUSED5	EQU	$00000020	; unused
M_UNUSED6	EQU	$00000040	; unused
M_UNUSED7	EQU	$00000080	; unused
M_UNUSED8	EQU	$00000100	; unused
M_UNUSED9	EQU	$00000200	; unused
M_UNUSED10	EQU	$00000400	; unused
M_UNUSED11	EQU	$00000800	; unused
M_UNUSED12	EQU	$00001000	; unused
M_UNUSED13	EQU	$00002000	; unused
M_UNUSED14	EQU	$00004000	; unused
M_UNUSED15	EQU	$00008000	; unused
M_UNUSED16	EQU	$00010000	; unused
M_UNUSED17	EQU	$00020000	; unused
M_UNUSED18	EQU	$00040000	; unused
M_UNUSED19	EQU	$00080000	; unused
M_UNUSED20	EQU	$00100000	; unused
M_UNUSED21	EQU	$00200000	; unused
M_UNUSED22	EQU	$00400000	; unused
M_UNUSED23	EQU	$00800000	; unused
M_UNUSED24	EQU	$01000000	; unused
M_UNUSED25	EQU	$02000000	; unused
M_UNUSED26	EQU	$04000000	; unused
M_UNUSED27	EQU	$08000000	; unused
M_UNUSED28	EQU	$10000000	; unused
M_UNUSED29	EQU	$20000000	; unused
M_UNUSED30	EQU	$40000000	; unused
M_UNUSED31	EQU	$80000000	; unused

;*======================================================================*
;*	OBJECT IDs and OFFSETS						*
;*======================================================================*

BITOBJ		EQU	0
SCBITOBJ	EQU	1
GPUOBJ		EQU	2
BRANCHOBJ	EQU	3
STOPOBJ		EQU	4

BITSIZE		EQU	(16>>3)		; phrase offset
SCBITSIZE	EQU	(32>>3)
GPUSIZE		EQU	(8>>3)
BRANCHSIZE	EQU	(8>>3)
STOPSIZE	EQU	(8>>3)

;*======================================================================*
;* EOF									*
;*======================================================================*
