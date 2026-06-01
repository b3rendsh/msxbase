; ------------------------------------------------------------------------------
; hbmsx.asm
; HBDOS MSX DOS/BASIC/ROM loader
;
; (C) 2026 All rights reserved. 
; ------------------------------------------------------------------------------

		INCLUDE "base.inc"
		INCLUDE	"hbmsx.inc"

fcb		equ	$5c		; File Control Block
buff		equ	$80		; DMA buffer
		
; ------------------------------------------------------------------------------
		
		ORG	$100

		jp	main

		db	13,10
		db	"HBDOS MSX V0.3",13,10
		db	26

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

		ld	de,t_loading
		ld	c,9
		call	5
		
	IF MSXBOOT = 2
		; open ROM file 
		ld	de,fcb
		ld	c,$0f		; FOPEN
		call	5
		ld	de,t_error
		inc	a		; error opening file?
		jp	z,err_exit	; z=yes
		
loop:		ld	de,fcb		; Read from file
		ld	c,$14		; FREAD
		call	5
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
		call	5
		ld	de,t_notrom
		jp	err_exit

eof:		ld	de,fcb		; Close the file
		ld	c,$10		; FCLOSE
		call	5
		
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

runmsx:		di
	
		; load system rom	
		ld	bc,main-msx
		ld	hl,msx 
		ld	de,0
		ldir

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
		call	5
		ret

	IF MSXBOOT = 2
dest:		dw	$4000
t_loading:	db	"MSX ROM loader for RomWBW...",13,10,"$"
t_error:	db	"Error loading ROM file",13,10,"$"
t_notrom:	db	"Error: invalid MSX ROM file",13,10,"$"
	ELSE
t_loading:	db	"Loading MSX for RomWBW...",13,10,"$"
	ENDIF
t_nohbios:	db	"Error: HBIOS not detected",13,10,"$"
t_hbdos:	db	"Error: MSX already active",13,10,"$"
t_intmode:	db	"Error: interrupt mode is 0",13,10,"$"

intmode:	db	$00

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

