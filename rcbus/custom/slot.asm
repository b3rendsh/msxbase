; ------------------------------------------------------------------------------
; slot.asm
; Custom MSX HBIOS slot routines, MSX 1 version
;
; (C) 2025 All rights reserved.
; ------------------------------------------------------------------------------
; HBIOS to MSX slot mapping:
; In MSX the 64KB memory is split in 4 pages of 16KB, in HBIOS 2 banks of 32KB.
;
; |----- HBIOS -----||----------------------- MSX -----------------------------------|
;  RAMBANK  NAME      DESCRIPTION            MEMORY    SLOT ID  NAME  COMMENT
;  NRAM-1   BID_COM   Common TPA RAM memory  Page 2+3  SLOT 0   SYS   always active
;  NRAM-2   BID_USR   Main ROM (BIOS/BASIC)  Page 0+1  SLOT 0   SYS   BASIC mode
;  NRAM-3   BID_AUX   DOS TPA RAM memory     Page 0+1  SLOT 1   RAM   DOS mode
;  NRAM-4   BID_BUF   Extension/disk ROM     Page 0+1  SLOT 2   DISK  DOS/BASIC mode
;  NRAM-5   BID_APPx  User/game ROM          Page 0+1  SLOT 3   CART  use ROM loader
;  ...
;  0x81     Reserved  Not used in MSX mode (HBIOS boot loader or ramdisk)
;  0x80     BID_BIOS  HBIOS
;
; MSX BASIC mode requires 4 HBIOS RAM banks (128KB), MSX DOS mode requires 2 more banks.
; (MSX_BANKID) = NRAM-2 = 0x80 + number of HBIOS RAM banks - 2
;
; The custom slot switching routines disable interrupts for MSX compatibility,
; therefore the HBIOS memory routines can be invoked without using MSX mutex.
; ------------------------------------------------------------------------------

		ALIGN	018CH

 		; this area is reserved for extra routines
		
; ---------------------------------------------------------
; Read the value of an address in another slot
; Input:  A  = slot number
;         HL = address to read
; Output: A  = value
; HBIOS: switch bank if address < 0x8000
; ---------------------------------------------------------
		ALIGN	001B6H
RDSLT:		di
		bit	7,h
		jr	z,read_bank
		ld	a,(hl)
		ret
		
read_bank:	push	bc
		push	de
		ld	b,a
		ld	a,(MSX_BANKID)
		sub	b
		ld	d,a
		ld	b,BF_SYSPEEK
		call	HB_INVOKE
		ld	a,e
		pop	de
		pop	bc
		ret				
		
; ---------------------------------------------------------
; Write a value to an address in another slot
; Input:  A  = slot number
;         HL = address to write
;         E  = value
; HBIOS: switch bank if address < 0x8000
; ---------------------------------------------------------
		ALIGN	001D1H
WRSLT:		di
		bit	7,h
		jr	z,write_bank
		ld	(hl),a
		ret

write_bank:	push 	af
		push	bc
		push	de
		ld	a,(MSX_BANKID)
		sub	b
		ld	d,a
		ld	b,BF_SYSPOKE
		call	HB_INVOKE
		pop	de
		pop	bc
		pop	af
		ret

; ---------------------------------------------------------
; Interslot call to BASIC subroutine
; Input:  IX  = subroutine address
; HBIOS: the BIOS and BASIC bank are activated together
; ---------------------------------------------------------
		ALIGN	001FFH
CALBAS:		di
		jp	(ix)
		
; ---------------------------------------------------------
; Interslot call
; Parameters: slot id (byte), address (word)
; HBIOS: unchanged
;        the BIOS jump table entry is patched by the Disk BASIC installer
;        to jump to the HBDOS PH_CALLF routine
; ---------------------------------------------------------
		ALIGN	00205H
CALLF:		ex	(sp),hl			; store HL, restore return address
		push	af			; store AF
		push	de			; store DE
		ld	a,(hl)			; slot id
		push	af
		pop	iy			; slot id in IYH
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl			; address
		push	de
		pop	ix			; address in IX
		pop	de			; restore DE
		pop	af			; restore AF
		ex	(sp),hl			; store new return address, restore HL

; ---------------------------------------------------------
; Interslot call
; Input: IY = high byte is slot id
;        IX = subroutine address
; HBIOS: this routine should never be called in the BIOS
;        the BIOS jump table entry is patched by the Disk BASIC installer
;        to jump to the HBDOS PH_CALSLT routine
; ---------------------------------------------------------
		ALIGN	00217H
CALSLT:		di
		ret

; ---------------------------------------------------------
; Enables slot at indicated page
; Input: A = slot id
;        H = bit 6 and 7 contain page number
; HBIOS: this routine should never be called in the BIOS
;        the BIOS jump table entry is patched by the Disk BASIC installer
;        to jump to the HBDOS PH_ENASLT routine
; ---------------------------------------------------------
		ALIGN	0025EH
ENASLT:		di
		ret
