; ------------------------------------------------------------------------------
; init.asm
; BASIC initialization, MSX 2 version (version 2.0)
; ------------------------------------------------------------------------------

; System initialization routine
; Remark: RAM is invoked, EXPTBL and SLTTBL are initialized
INIT:
C7C76:		LD	SP,VARWRK-10		; temporary stack
		LD	BC,00230H-1
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
		JR	J7D14			; jump over patch code

; patch: extended SET statement
J7CE3:		SYSHOOK	H_SETS			; hook for SET statement expansion
		LD	IX,S_SETS
		JP	EXTROM			; MSX2 and above SET statements

; Subroutine INITXT replacement (setup BASIC screen)
C7CED:		LD	IX,S_SDFSCR
		JP	EXTROM

; patch: extended PUT SPRITE statement handler, because of new spritesmodes
J7CF4:		LD	A,(SCRMOD)
		CP	04H
		JP	C,J7AB6			; MSX 1 style sprites, use classic
		LD	IX,S_PUTSPRT
		JP	EXTROM

; patch: extended COPY statement
J7D03:		LD	IX,S_SCOPY
		CALL	EXTROM
		RET	NC			; handled by subrom, quit
		SYSHOOK	H_COPY
		JP	FCERR			; illegal function call
		
		ALIGN	7D14H
		
J7D14:		CALL	CHKSLZ			; MSX2 patch: check subrom and do extension ROMs

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

C7D29:		JR	J7D31			; skip over unused space
		ALIGN	7D2FH

D7D2F:		DEFW	C7CED			; initialize BASIC screen (done by S.SDFSCR)

J7D31:  	CALL	C7BEE			; initialize BASIC screen + print BASIC version (done by S.SETSCR)
		LD	HL,SLTWRK+0
		LD	A,(HL)
		AND	20H
		LD	(HL),A			; ramdisk invalid, but flag ramdisk disabled remains unchanged

		ALIGN	7D3DH			; align to MSX1 code

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

; Subroutine do extension ROMs
C7D75:		DI
		LD	C,0			; primary slot = 0
		LD	DE,EXPTBL
		LD	HL,SLTATR
J7D7E:		LD	A,(DE)			; slot expanded flag
		OR	C
		LD	C,A			; store slot id
		PUSH	DE			; store pointer in EXPTBL
J7D82:		INC	HL			; update pointer in SLTATR
		PUSH	HL			; store pointer in SLTATR
		LD	HL,04000H		; page 1
J7D87:		CALL	RDWEXP			; read word from extension ROM
		PUSH	HL			; store address
		LD	HL,04241H
		RST	R_DCOMPR		; extension ROM header ?
		POP	HL			; restore address
		LD	B,0			; reset extension flags
		JR	NZ,J7DBE		; nope, next slot
		CALL	RDWEXP			; read word from extension ROM (INIT)
		PUSH	HL			; store address
		PUSH	BC			; store extension flags, slot id
		PUSH	DE
		POP	IX			; IX = INIT address
		LD	A,C			; slot id
		PUSH	AF
		POP	IY			; IYH = slot id
		CALL	NZ,C7FF5		; if extension ROM has INIT, call INIT
		POP	BC			; restore extension flags, slot id
		POP	HL			; restore address
		CALL	RDWEXP			; read word from extension ROM
		ADD	A,0FFH
		RR	B			; extension STATEMENT flag
		CALL	RDWEXP			; read word from extension ROM
		ADD	A,0FFH
		RR	B			; extension DEVICE flag
		CALL	RDWEXP			; read word from extension ROM
		ADD	A,0FFH
		RR	B			; extension BASIC PROGRAM flag
		LD	DE,-8
		ADD	HL,DE			; update address
J7DBE:		EX	(SP),HL			; store address, restore pointer in SLTATR
		LD	(HL),B			; adjust SLTATR
		INC	HL			; update pointer in SLTATR
		EX	(SP),HL			; store pointer in SLTATR, restore address
		LD	DE,04000H-2
		ADD	HL,DE			; update address to next page
		LD	A,H
		CP	0C0H			; page 3 ?
		JR	C,J7D87			; nope, next page
		POP	HL			; restore pointer in SLTATR
		INC	HL
		LD	A,C			; slot id
		AND	A			; slot expanded ?
		LD	DE,16-4
		JP	P,J7DE0			; nope, next primary
		ADD	A,4
		LD	C,A			; update slot id
		CP	90H			; finished all secondary slots ?
		JR	C,J7D82			; nope, next secondary slot
		AND	03H
		LD	C,A			; next primary slot
		DEFB	03EH			; LD A,xx, trick to skip next instruction
J7DE0:		ADD	HL,DE
		POP	DE			; restore pointer in EXPTBL
		INC	DE			; update pointer in EXPTBL
		INC	C			; update primary slot
		LD	A,C
		CP	3+1			; finished all primary slots ?
		JR	C,J7D7E			; nope, next primary slot

		LD	HL,SLTATR
		LD	B,4*4*4			; extension page counter
J7DEE:		LD	A,(HL)			; attributes extension page
		ADD	A,A			; has BASIC program ?
		JR	C,J7DF6			; yep, start BASIC program in extension ROM
		INC	HL
		DJNZ	J7DEE			; next extension page
		RET
