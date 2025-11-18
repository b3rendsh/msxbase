; ------------------------------------------------------------------------------
; init.asm
; Custom MSX BIOS/BASIC HBIOS initialization, MSX 1 version
;
; (C) 2025 All rights reserved.
; ------------------------------------------------------------------------------

; System initialization routine
; Remark: RAM is invoked, EXPTBL and SLTTBL are initialized
INIT:		
C7C76:		LD	SP,VARWRK-10		; temporary stack

		; Hooks are disabled in HBIOS mode except for H.KEYI and H.TIMI
		; The system area from 0xFDA4 to 0xFDFF is used by the custom timer interrupt 
		; The system area from 0xFE00 to 0xFFFF is used by the HBIOS proxy
		LD	BC,2*5-1
		LD	DE,H_KEYI+1
		LD	HL,H_KEYI
		LD	(HL),0C9H
		LDIR				; initialize hooks
		LD	HL,VARWRK
		LD	(HIMEM),HL		; highest BASIC RAM address
		CALL	C7D5D			; search lowest BASIC RAM address
		LD	(BOTTOM),HL		; update BOTTOM
		LD	BC,00090H
		LD	DE,VARWRK
		LD	HL,I7F27
		LDIR				; initialize some systemvariables
		CALL	INIFNK			; initialize functionkeys
		XOR	A
		LD	(ENDBUF),A		; endmarker for BUF
		LD	(NLONLY),A		; reset loading BASIC program, leave I/O channels open flag
		LD	A,','
		LD	(BUFMIN),A		; dummy prefix for BUF
		LD	A,':'
		LD	(KBFMIN),A		; dummy prefix for KBUF
		LD	HL,(CGTABL)
		LD	(CGPNT+1),HL		; address charactergenerator (but what about the slot id in CGPNT+0 ??)
		LD	HL,PRMSTK
		LD	(PRMPRV),HL		; initialize previous FN block pointer
		LD	(STKTOP),HL		; Z80 stack temporary at PRMSTK
		LD	BC,200
		ADD	HL,BC
		LD	(MEMSIZ),HL		; a fake string heap of 200 bytes
		LD	A,01H
		LD	(VARTAB+1),A		; a fake simple variable start at 0100H
		CALL	ALCFIL			; allocate 1 I/O channel (also reinitialize STKTOP, MEMSIZ)
		CALL	STKINI			; initialize BASIC stack
		LD	HL,(BOTTOM)
		XOR	A
		LD	(HL),A			; end of BASIC line token before BASIC text
		INC	HL
		LD	(TXTTAB),HL		; start of BASIC text
		CALL	SCRTCH			; clear BASIC program
		CALL	INITIO			; initialize I/O devices (PSG and LPT)
		call	INIT32			; screen 1
		call	CLRSPR			; clear sprites
		ld	hl,00A0BH
		ld	(CSRY),hl		; cursor at 10,11
		ld	hl,T7ED8
		call	STROUT			; print MSX system
		ld	hl,00A0CH
		ld	(CSRY),hl		; cursor at 10,12
		ld	hl,T7EE4
		call	STROUT			; print version 1.0
		ld	hl,0020EH
		ld	(CSRY),hl		; cursor at 2,14
		ld	hl,T7EFD
		call	STROUT			; print copyright 1983 by Microsoft
		ld	b,BTSCNT
A7D0D:		dec	hl
		ld	a,l
		or	h
		jr	nz,A7D0D
		djnz	A7D0D			; wait some seconds
J7D14:		; don't do extension roms

	IF MSXBOOT = 1
		; run HBDOS loader
		call	MSX_DOS+3
	ENDIF
	
		ALIGN	$7D17

; Entrypoint used by diskrom
; used when H.STKE was hooked by a extension ROM. BASIC programs in extension ROMs are NOT executed
M7D17:
J7D17:		LD	HL,(BOTTOM)
		XOR	A
		LD	(HL),A			; end of BASIC line before BASIC text
		INC	HL
		LD	(TXTTAB),HL
		CALL	SCRTCH			; clear BASIC program
		CALL	C7D29			; display BASIC startscreen
		JP	READY			; ok and mainloop

; Subroutine display BASIC startscreen
C7D29:		ld	a,0FFH
		ld	(CNSDFG),a		; KEY ON

; No Kanji support
M7D2E:		call	INITXT			; select text mode 40x24

M7D31:		ld	hl,T7EF2
		call	STROUT
		ld	hl,T7EE4
		call	STROUT
		LD	HL,T7EFD		; copyright message
		CALL	STROUT			; message to interpreter output
		LD	HL,(VARTAB)
		EX	DE,HL
		LD	HL,(STKTOP)
		LD	A,L
		SUB	E
		LD	L,A
		LD	A,H
		SBC	A,D
		LD	H,A
		LD	BC,-14
		ADD	HL,BC
		CALL	LINPRT			; number to interpreter output
		LD	HL,I7F1B
		JP	STROUT			; message to interpreter output

; Subroutine search for start of ram 0EFFFH - 08000H area (downwards)
C7D5D:		LD	HL,0EF00H
J7D60:		LD	A,(HL)
		CPL
		LD	(HL),A
		CP	(HL)
		CPL
		LD	(HL),A
		JR	NZ,J7D71		; no RAM, quit search
		INC	L
		JR	NZ,J7D60
		LD	A,H
		DEC	A
		RET	P
		LD	H,A
		JR	J7D60

J7D71:		LD	L,0
		INC	H
		RET

; ------------------------------------------------------------------------------
; Custom HBIOS routines
; ------------------------------------------------------------------------------

; Cold boot RomWBW HBIOS (warm boot won't work)
REBOOT:		di
		ld	b,BF_SYSRESET
		ld	c,BF_SYSRES_COLD
		call	HB_INVOKE		; never return
		jr	$			
		

INITSIZE	EQU	$-INIT
