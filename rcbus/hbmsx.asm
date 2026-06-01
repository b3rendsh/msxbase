; ------------------------------------------------------------------------------
; hbmsx.asm
; HBDOS MSX DOS/BASIC/ROM loader
;
; (C) 2026 All rights reserved. 
; ------------------------------------------------------------------------------

		INCLUDE "base.inc"
		INCLUDE	"hbmsx.inc"

BDOS		equ	$05
LF		equ	$0a
CR		equ	$0d
CTRL_Z		equ	$1a
fcb		equ	$5c		; File Control Block
buff		equ	$80		; DMA buffer

; ------------------------------------------------------------------------------
		
		ORG	$100

		jp	main

		db	CR,LF
		db	"HBDOS MSX V0.3",CR,LF
		db	CTRL_Z

msx:          	INCBIN	"obj/bios.bin"
	IF MSXBOOT = 2
		INCBIN	"obj/init.bin"
	ELSE
		INCBIN	"obj/basic.bin"
	IF MSXBOOT = 1
		INCBIN	"obj/hbdos1__.bin"
	ENDIF
	ENDIF
		
main:		call	check_ident
		ld	de,t_nohbios
		jp	nz,err_exit
		
		call	check_hbdos
		ld	de,t_hbdos
		jp	z,err_exit

		; get interrupt mode
		ld	bc,BC_SYSINT_INFO
		call	HB_INVOKE
		ld	a,d
		ld	(intmode),a
		or	a
		ld	de,t_intmode
		jp	z,err_exit

	IF MSXBOOT != 2
		; parse commandline parameters
		call	parse_params
		jp	nz,err_exit
	ENDIF

		ld	de,t_loading
		call	PrintText
	
	IF MSXVDP > 0
		; get vdp i/o
		ld	b,BF_VDADEV		; vdp device info
		ld	c,0			; unit 0
		call	HB_INVOKE
		ld	a,3			; 3=TMS
		cp	d			; is device type TMS?
		jr	nz,vdp1			; nz=no, skip vdp setting
		ld	a,l			; get i/o base
		ld	(vdpio),a
vdp1:		
	ENDIF
	  
	IF MSXPSG > 0
		; get psg i/o
		ld	b,BF_SNDQUERY		; psg device info
		ld	c,0			; unit 0
		ld	e,4			; get device detials
		call	HB_INVOKE
		ld	a,1			; 1=ay-3-8910
		cp	b			; is device type ay-3-8910?
		jr	nz,psg1			; nz=no, skip psg setting
		ld	(psgio),de		; get psg address and data i/o
psg1:		
	ENDIF

		; get frequency
		ld	bc,BC_SYSGET_TIMER
		call	HB_INVOKE
		ld	a,c
		ld	(freq),a
		cp	50
		jr	z,freq1
		ld	a,'6'
		ld	(t_freq),a
freq1:		

	IF MSXBOOT != 2
		; optionally print detected settings
		ld	a,(param_debug)
		or	a
		jr	z,runmsx
		ld	hl,t_vdp
		ld	a,(vdpio)
		call	ToHex
		ld	hl,t_psg0
		ld	a,(psgio+1)
		call	ToHex
		ld	hl,t_psg1
		ld	a,(psgio+0)
		call	ToHex
		ld	hl,t_int
		ld	a,(intmode)
		call	ToHex
		ld	hl,t_output
		ld	a,(param_vdp)
		call	ToHex
		ld	de,t_settings
		call	PrintText
		; wait for key
		call	GetChar
		ld	de,t_exit
		res	5,a		; to uppercase
		cp	'Q'
		jp	z,err_exit

	ELSE ; MSBOOT = 2
		; open ROM file 
		ld	de,fcb
		ld	c,$0f		; FOPEN
		call	BDOS
		ld	de,t_error
		inc	a		; error opening file?
		jp	z,err_exit	; z=yes
		
loop:		ld	de,fcb		; Read from file
		ld	c,$14		; FREAD
		call	BDOS
		or	a
		jr	nz,eof		; Non-zero A return value means end of file

		ld	hl,buff         ; Copy from DMA buffer to destination
		ld	de,(dest)
		ld	a,$c0-1
		cp	d		; max 32K filesize reached?
		jr	c,fsize		; c=yes
		
		ld	bc,$80
		ldir
		ld	(dest),de	; Increment next destination address
		jr	loop
		
fsize:		ld	de,fcb		; Close the file
		ld	c,$10		; FCLOSE
		call	BDOS
		ld	de,t_notrom
		jp	err_exit

eof:		ld	de,fcb		; Close the file
		ld	c,$10		; FCLOSE
		call	BDOS
		
		; check if it is a valid MSX ROM file
		ld	de,t_notrom
		ld	hl,$4000
		ld	a,'A'
		cp	(hl)
		jp	nz,err_exit
		inc	hl
		ld	a,'B'
		cp	(hl)
		jp	nz,err_exit
		inc	hl
		inc	hl
		ld	a,(hl)
		cp	$40		; valid init address?
		jp	c,err_exit
		cp	$80		; relocate to $8000?
		jr	c,runmsx	; c=no
		cp	$c0		; valid init address
		jp	nc,err_exit
		
		; relocate ROM data to $8000
		ld	hl,$4000
		ld	de,$8000
		ld	bc,$4000
		ldir
		; clear $4000-$8000
		ld	hl,$4000
		ld	(hl),$00
		ld	de,$4001
		ld	bc,$3fff
		ldir	
	
	ENDIF ; MSXBOOT

; ------------------------------------------------------------------------------
; Copy and patch system rom

runmsx:		di
		ld	bc,main-msx
		ld	hl,msx 
		ld	de,0
		ldir

	IF MSXVDP > 0
		; patch vdp i/o
		ld	a,(vdpio)		; get i/o base
		ld	hl,PATCH_VDP0
		ld	b,2			; patch 2 vdp i/o addresses
		call	patch_rom
	ENDIF

	IF MSXPSG > 0
		; patch psg i/o
		ld	a,(psgio+1)
		ld	hl,PATCH_PSG0
		ld	b,1
		call	patch_rom
		ld	a,(psgio+0)
		ld	hl,PATCH_PSG1
		ld	b,1
		call	patch_rom
	ENDIF
	
		; patch IDBYT0 bit 7: 0=60Hz 1=50Hz
		ld	a,(freq)
		cp	50
		ld	a,(IDBYT0)
		jr	z,set50
		res	7,a
		jr	setfreq
set50:		set	7,a
setfreq:	ld	(IDBYT0),a

	IF MSXBOOT != 2
		; patch BASIC tone duration setting
		ld	a,INTHZ1
		ld	(I7754),a
		ld	a,INTHZ2
		ld	(I7754+3),a

		; patch screen output settings
		ld	a,(param_vdp)
		cp	MSXVDP
		jr	z,runmsx1
		cp	1
		jr	nz,dual_out
		ld	a,0			; nop
		ld	(PATCH_CHPUT),a
		ld	hl,0			; nop
		ld	(PATCH_CHPUT+1),hl
		jr	runmsx1
dual_out:	ld	a,$cd			; call instruction
		ld	(PATCH_CHPUT),a
		ld	hl,HBCHPUT		; output text to console
		ld	(PATCH_CHPUT+1),hl
runmsx1:
	ENDIF
	
; ------------------------------------------------------------------------------
		
		; set MSX HBDOS signature
		ld	hl,MSX_HB
		ld	(hl),'H'
		inc	hl
		ld	(hl),'B'
		
		; get HBIOS BID_USR bank-id and set as MSX main bank-id
		ld	bc,BC_SYSGET_BNKINFO
		call	HB_INVOKE
		ld	a,e
		ld	(MSX_BANKID),a
		
		; init interrupt hook and mutex
		ld	a,$c3
		ld	(MSX_HKEYINT),a
		ld	hl,MSX_KEYINT
		ld	(MSX_HKEYINT+1),hl
		ld	a,$fe
		ld	(MSX_MUTEX),a
		
		; load custom HB_INVOKE routine with mutex
		ld	bc,HBI_SIZE
		ld	hl,HBI_START 
		ld	de,MSX_HBINVOKE
		ldir
		
		; load custom interrupt routine
		ld	a,(intmode)
		cp	1
		jr	nz,load_im2

		; IM 1 handler
		ld	bc,IM1_SIZE
		ld	hl,IM1_START 
		ld	de,MSX_INT
		ldir

		; Boot MSX
		jp	0

; ------------------------------------------------------------------------------

		; IM 2 handler
load_im2:	ld	bc,IM2_SIZE
		ld	hl,IM2_START 
		ld	de,MSX_INT
		ldir

		; Patch IM 1 entry point with ret instruction
		ld	a,$c9
		ld	($0038),a

		; Set PRT0 interrupt vector to timer routine
		ld	hl,timer2
		ld	(HBX_IVT+$02*2),hl

		; Boot MSX
		jp	0

; ------------------------------------------------------------------------------

; display message and return to CP/M without restart
err_exit:	ld	c,9
		call	BDOS
		ret

	IF MSXBOOT = 2
dest:		dw	$4000
t_loading:	db	"MSX ROM loader for RomWBW...",CR,LF,"$"
t_error:	db	"Error loading ROM file",CR,LF,"$"
t_notrom:	db	"Error: invalid MSX ROM file",CR,LF,"$"
t_freq:		db	"50 Hz"
	ELSE
t_loading:	db	CR,LF,"Loading MSX for RomWBW...",CR,LF,"$"
t_settings:	db	"Device settings"
		db	CR,LF,"VDP I/O   : 0x"
t_vdp:		db	"00"		
		db	CR,LF,"PSG I/O   : 0x"
t_psg0:		db	"00 / 0x"
t_psg1:		db	"00"
		db	CR,LF,"Frequency : "
t_freq:		db	"50 Hz"
		db	CR,LF,"Int. mode : "
t_int:		db	"00"
		db	CR,LF,"Output    : "
t_output:	db	"00"
		db	CR,LF,"Joystick  : "
	  IF MSXJOY != 1
		db	"off"
	  ELSE
		db	"on"
	  ENDIF
		db	CR,LF,LF,"Press [ENTER] to continue or [Q] to quit.. "
t_exit:		db	CR,LF,"$"
	ENDIF ; MSXBOOT

t_nohbios:	db	"Error: HBIOS not detected",CR,LF,"$"
t_hbdos:	db	"Error: MSX already active",CR,LF,"$"
t_intmode:	db	"Error: interrupt mode is 0",CR,LF,"$"

vdpio:		db	VDP0
psgio:		db	PSG1,PSG0
freq:		db	INTHZ
intmode:	db	$00

; -----------------------------------------------------------------------------
; Parse commandline parameters
; -----------------------------------------------------------------------------

get_one:	inc	b
		dec	b
		ret	z
		inc	hl
		ld	a,(hl)
		dec	b
		ret

parse_params:	ld	hl,$0080
		ld	a,(hl)			; Length of command line parameters
		ld	b,a
parse_l1:	call	get_one
		ret	z
		cp	'/'			; option?
		jr	z,option
		cp	$20			; skip spaces
		jr	nz,usage
		jr	parse_l1

option:		call	get_one
		res	5,a			; to uppercase (MSX-DOS)
		cp	'C'
		jr	z,option_c
		cp	'D'
		jr	z,option_d
		jr	usage

option_c:	ld	a,2
		ld	(param_vdp),a
		jr	parse_l1
		
option_d:	ld	a,$ff
		ld	(param_debug),a
		jr	parse_l1

usage:		ld	de,t_usage
		xor	a
		dec	a			; nz=error
		ret

t_usage:	db	"Commandline options:",CR,LF
		db	"  /C = copy text output to console",CR,LF
		db	"  /D = print detected settings",CR,LF
		db	"$"

param_debug:	db	0
param_vdp:	db	1
		
; ------------------------------------------------------------------------------
; ROM patches
; ------------------------------------------------------------------------------

; INTHZ (use inverse logic to change settings!)
IDBYT0		equ	$002B
I7754		equ	$7754
	IF INTHZ = 50
INTHZ1		equ	$40
INTHZ2		equ	$14
	ELSE
INTHZ1		equ	$00
INTHZ2		equ	$12
	ENDIF

; Screen output
PATCH_CHPUT	equ	$08C0
	
; VDP I/O
PATCH_VDP0:	dw	$0006,$0007,$05F0,$0642,$0715,$073B,$074A,$07D5
		dw	$07DD,$081B,$0AFC,$0BE2,$0BEF,$FFFF
PATCH_VDP1:	dw	$0582,$0587,$07E2,$07E9,$07EF,$07F4,$144A,$FFFF

; PSG I/O
PATCH_PSG0:	dw	$03C2,$FFFF
PATCH_PSG1:	dw	$03C5,$FFFF
PATCH_PSG2:	dw	$FFFF


; Patch ROM with alternative i/o addresses
; Input: hl = patch table
;        b  = number of i/o registers
patch_rom:	ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		bit	7,d
		jr	nz,patch_next
		ld	(de),a
		jr	patch_rom
patch_next:	inc	a
		djnz	patch_rom
		ret

; ------------------------------------------------------------------------------
; Subroutines
; ------------------------------------------------------------------------------

; Check for RomWBW HBIOS
check_ident:	ld	hl,(HB_IDENT)
		ld	a,'W'
		cp	(hl)
		ret	nz
		inc	hl
		ld	a,~'W'
		cp	(hl)
		ret

; Check for MSX HBDOS
check_hbdos:	ld	hl,MSX_HB
		ld	a,'H'
		cp	(hl)
		ret	nz
		inc	hl
		ld	a,'B'
		cp	(hl)
		ret

; Byte to hex string
ToHex:		ld	b,a
		and	$f0
		rrca
		rrca
		rrca	
		rrca
		add	a,'0'
		cp	'9'+1
		jr	c,digit1
		add	a,7
digit1:		ld	(hl),a
		inc	hl
		ld	a,b
		and	$0f
		add	a,'0'
		cp	'9'+1
		jr	c,digit2
		add	a,7
digit2:		ld	(hl),a
		ret
		
; Print Text terminated with $
PrintText:	ld	c,9
		jp	BDOS
		

; Get console input
GetChar:	ld	c,1
		jp	BDOS  
; ----------------------------------------------------------
; Invoke HBIOS call handler with mutex
; ----------------------------------------------------------
HBI_START:
		PHASE	MSX_HBINVOKE

hbInvoke:	push	hl
		ld	hl,MSX_MUTEX
mutex_wait:	sra	(hl)			; request mutex
		jr	c,mutex_wait
		pop	hl
		call	HB_INVOKE
		push	hl
		ld	hl,MSX_MUTEX
		ld	(hl),$fe		; release mutex
		pop	hl
		ret
		
		DEPHASE
		
HBI_SIZE	equ	$-HBI_START		
		
; ------------------------------------------------------------------------------
; Custom interrupt mode 1 handler
; ------------------------------------------------------------------------------
IM1_START:
		PHASE	MSX_INT

		; set interrupt return address to msx_keyint
int1:		ld	(hlsav1+1),hl
		ld	hl,msx_keyint1
		push	hl
hlsav1:		ld	hl,$0000

		; first call the HBIOS interrupt handler
		call	HBX_INT
		db	$10 << 2		; use special vector

		; then call the MSX interrupt handler
msx_keyint1:	push	af
		push	hl
		ld	hl,MSX_MUTEX
		sra	(hl)			; request mutex
		jr	c,int1_end		; c=locked
		
		; check timer tick
		push	bc
		push	de
		ld	bc,BC_SYSGET_TIMER
		call	HB_INVOKE
		pop	de
		pop	bc
		ld	a,l
tick1:		cp	$00			; next tick?
		jr	z,mutex1_end		; z=no
		ld	(tick1+1),a		; update tick

		; invoke MSX interrupt handler
		pop	hl
		pop	af
		call	MSX_HKEYINT
		push	af
		push	hl
mutex1_end:	ld	hl,MSX_MUTEX
		ld	(hl),$fe		; release mutex
		
int1_end:	pop	hl
		pop	af
		ret

		DEPHASE

IM1_SIZE	equ	$-IM1_START


; ------------------------------------------------------------------------------
; Custom interrupt mode 2 handler
; ------------------------------------------------------------------------------
IM2_START:
		PHASE	MSX_INT

		; set interrupt return address to msx_keyint
timer2:		ld	(hlsav2+1),hl
		ld	hl,msx_keyint2
		push	hl
hlsav2:		ld	hl,$0000

		; first call the HBIOS interrupt handler
		call	HBX_INT
		db	$02 << 2		; use PRT0 vector

		; then call the MSX interrupt handler
msx_keyint2:	push	af
		push	hl
		ld	hl,MSX_MUTEX
		sra	(hl)			; request mutex
		jr	c,int2_end		; c=locked
		
		; invoke MSX interrupt handler
		pop	hl
		pop	af
		call	MSX_HKEYINT
		push	af
		push	hl
		ld	hl,MSX_MUTEX
		ld	(hl),$fe		; release mutex
		
int2_end:	pop	hl
		pop	af
		ret

		DEPHASE

IM2_SIZE	equ	$-IM2_START

