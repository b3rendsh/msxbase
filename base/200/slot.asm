; ------------------------------------------------------------------------------
; slot.asm
; BIOS slot functions, MSX 2 version (version 2.0)
; ------------------------------------------------------------------------------

		ALIGN	018CH

		DEFS	001B6H-$,0

; Subroutine CALSLT for expanded slot 0 page 0
; Remark: this is done by this special routine, otherwise calslt crashes
;         code to handle a interslot call to the subrom in a secondary
;			of slot 0.

J01B6:		POP	AF
		LD	D,0FCH
		AND	0CH
		RRCA
		RRCA
		LD	H,A
		IN	A,(0A8H)
		LD	B,A
		AND	3FH
		OUT	(0A8H),A
		LD	A,(D_FFFF)
		CPL
		LD	C,A
		AND	D
		OR	H
		LD	E,A
		LD	(D_FFFF),A

; from this point code continues to execute in subrom
Q01D0:		LD	A,B
		AND	D
		OUT	(0A8H),A
		LD	A,E
		LD	(SLTTBL),A
		PUSH	BC
		EXX
		EX	AF,AF'
		CALL	CLPRM1
		DI
		EX	AF,AF'
		EXX
		POP	BC
		LD	A,B
		AND	3FH
		OUT	(0A8H),A
		LD	A,C
		LD	(D_FFFF),A

; from this point code returns executing from the subrom 
Q01EB:		LD	A,B
		OUT	(0A8H),A
		LD	A,C
		LD	(SLTTBL),A
		EX	AF,AF'
		EXX
		RET

; Subroutine RDSLT
RDSLT:
C01F5:		CALL	C0353			; calculate masks
		JP	M,J0205			; handle expanded slot
		IN	A,(0A8H)
		LD	D,A
		AND	C
		OR	B
		CALL	RDPRIM
		LD	A,E
		RET

; Subroutine RDSLT for expanded slot
J0205:		CALL	C028C			; prm. slot 0, page 0 ?
		JR	NZ,J021A		; nop, use old method
		PUSH	HL
		CALL	C0255			; basicrom on page 1
		EX	(SP),HL
		CALL	C7FBE			; special routine, so bios does not crash
		JR	J0244

		DEFS	0217H-$,0		; align to 0217H

; Subroutine CALSLT
; Remark: Just a jump at this adres to have same entry point as MSX1
;         early RS232 ROM version uses this (illegal entry point)
CALSLT:
C0217:		JP	J02D8

; Subroutine RDSLT for expanded slot, not slot 0 page 0
J021A:		PUSH	HL
		CALL	C0378			; adjust secondary slotregister
		EX	(SP),HL
		PUSH	BC
		CALL	C01F5			; do RDSLT on only the primary slot
		JR	J0279			; restore secondary slotegister

; Subroutine WRSLT
WRSLT:
C0225:		PUSH	DE
		CALL	C0353			; calculate masks
		JP	M,J0235			; expanded slot,
		POP	DE
		IN	A,(0A8H)
		LD	D,A
		AND	C
		OR	B
		JP	WRPRIM

; Subroutine WRSLT for expanded slot
J0235:		CALL	C028C			; prm. slot 0, page 0 ?
		JP	NZ,J026E		; nope, use old method
		POP	DE
		PUSH	HL
		CALL	C0255			; basicrom on page 1
		EX	(SP),HL
		CALL	C7FC4			; special routine, so bios does not crash
J0244:	EX	(SP),HL
		PUSH	AF
		LD	A,L
		AND	3FH
		OUT	(0A8H),A
		LD	A,H
		LD	(D_FFFF),A
		LD	A,L
		OUT	(0A8H),A
		POP	AF
		POP	HL
		RET

; Subroutine basicrom on page 1
C0255:		PUSH	AF
		IN	A,(0A8H)
		LD	L,A
		AND	3FH
		OUT	(0A8H),A		; page 3 = prm. slot 0 (for access to sec. slotreg)
		LD	A,(D_FFFF)
		CPL
		LD	H,A
		AND	0F3H
		LD	(D_FFFF),A		; page 1 = sec. slot 0
		LD	A,L
		AND	0F3H
		OUT	(0A8H),A		; restore page 3 prm. slot 0, page 1 = prm. slot 0
		POP	AF
		RET

; Subroutine WRSLT for expanded slot, not slot 0 page 0
J026E:		EX	(SP),HL
		PUSH	HL
		CALL	C0378			; adjust secondary slotregister
		POP	DE
		EX	(SP),HL
		PUSH	BC
		CALL	C0225			; do WRSLT on only the primary slot
J0279:		POP	BC
		EX	(SP),HL
		PUSH	AF
		LD	A,B
		AND	3FH
		OR	C
		OUT	(0A8H),A
		LD	A,L
		LD	(D_FFFF),A		; restore secondary slotregister
		LD	A,B
		OUT	(0A8H),A
		POP	AF
		POP	HL
		RET

; Subroutine check if primary slot 0,page 0
; Output: Zx if primary slot 0, page 0

C028C:		INC	D
		DEC	D			; primary slot 0 ?
		RET	NZ
		LD	B,A
		LD	A,E
		CP	03H			; page 0 ?
		LD	A,B
		RET

; Subroutine SUBROM
SUBROM:
J0295:		CALL	C029B			; EXTROM
		POP	IX
		RET

; Subroutine EXTROM
EXTROM:
C029B:		EXX
		EX	AF,AF'
		PUSH	HL
		PUSH	DE
		PUSH	BC
		PUSH	AF
		LD	A,I
		PUSH	AF			; store IFF2 flag
		EXX
		EX	AF,AF'
		PUSH	IY
		LD	IY,(EXBRSA-1)		; slotid subrom
		CALL	C0217			; CALSLT
		POP	IY
		EX	AF,AF'
		EXX
		POP	AF			; restore IFF2 flag
		JP	PO,J02B8		; maskable interrupts where disabled, leave disabled
		EI
J02B8:		POP	AF
		POP	BC
		POP	DE
		POP	HL
		EXX
		EX	AF,AF'
		RET

; Subroutine CALBAS
CALBAS:
C02BF:		LD	IY,(EXPTBL+0-1)		; slotid basicrom
		JP	C0217			; CALSLT

; Subroutine CALLF
CALLF:
C02C6:		EX	(SP),HL
		PUSH	AF
		PUSH	DE
		LD	A,(HL)
		PUSH	AF
		POP	IY
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		PUSH	DE
		POP	IX
		POP	DE
		POP	AF
		EX	(SP),HL

; Subroutine CALSLT
J02D8:		EXX
		EX	AF,AF'
		PUSH	IY
		POP	AF
		PUSH	IX
		POP	HL
		CALL	C0353			; calculate masks
		JP	M,J02EF			; expanded slot,
		IN	A,(0A8H)
		PUSH	AF
		AND	C
		OR	B
		EXX
		JP	CLPRIM

; Subroutine CALSLT for expanded slot
J02EF:		PUSH	AF
		AND	03H			; primary slot 0 ?
		JR	NZ,J02FA		; nope, just the old methode
		LD	A,H
		AND	0C0H
		JP	Z,J01B6			; calslt to page 0 in some secondary slot 0, handle special to avoid crash
J02FA:		POP	AF
		CALL	C0378			; adjust secondary slotregister
		PUSH	AF
		POP	IY
		PUSH	HL
		PUSH	BC
		LD	C,A
		LD	B,0
		LD	A,L
		AND	H
		OR	D
		LD	HL,SLTTBL
		ADD	HL,BC
		LD	(HL),A			; update SLTTBL
		PUSH	HL
		EX	AF,AF'
		EXX
		CALL	C0217			; do a CALSLT on only the primary slot
		EXX
		EX	AF,AF'
		POP	HL
		POP	BC
		POP	DE
		LD	A,I
		PUSH	AF			; store IFF2 flag
		LD	A,B
		AND	3FH
		OR	C
		DI
		OUT	(0A8H),A
		LD	A,E
		LD	(D_FFFF),A		; restore secondary slotregister
		LD	A,B
		OUT	(0A8H),A
		LD	(HL),E			; restore SLTTBL
		POP	AF			; restore IFF2 flag
		JP	PO,J0330		; maskable interrupts where disabled, leave disabled
		EI
J0330:		EX	AF,AF'
		EXX
		RET

; Subroutine ENASLT
ENASLT:
C0333:		CALL	C0353			; calculate masks
		JP	M,J0340			; expanded slot,
		IN	A,(0A8H)
		AND	C
		OR	B
		OUT	(0A8H),A
		RET

; Subroutine ENASLT for expanded slot
J0340:		PUSH	HL
		CALL	C0378			; adjust secondary slotregister
		LD	C,A
		LD	B,0
		LD	A,L
		AND	H
		OR	D
		LD	HL,SLTTBL
		ADD	HL,BC
		LD	(HL),A			; update SLTTBL
		POP	HL
		LD	A,C
		JR	C0333			; do a ENASLT on only the primary slot

; Subroutine calculate masks
; Input:  A  = slotid
;         HL = adres
; Output: P set if expanded slot
;         A = slotid
;         D = PPPPPPPP
;         E = page select mask
;         B = primary slot or mask
;         C = page clear mask
C0353:		DI
		PUSH	AF
		LD	A,H
		RLCA
		RLCA
		AND	03H
		LD	E,A			; page
		LD	A,0C0H
J035D:		RLCA
		RLCA
		DEC	E
		JP	P,J035D
		LD	E,A			; page select mask
		CPL
		LD	C,A			; page clear mask
		POP	AF
		PUSH	AF
		AND	03H
		INC	A
		LD	B,A
		LD	A,0ABH
J036E:		ADD	A,55H
		DJNZ	J036E
		LD	D,A			; PPPPPPPP
		AND	E
		LD	B,A			; prim. slot select
		POP	AF
		AND	A
		RET

; Subroutine Adjust secondary slotregister
; Input:  A = slotid
; Output: A = slotid primary slot
C0378:		PUSH	AF
		LD	A,D
		AND	0C0H
		LD	C,A
		POP	AF
		PUSH	AF
		LD	D,A
		IN	A,(0A8H)
		LD	B,A
		AND	3FH
		OR	C
		OUT	(0A8H),A		; set page 3 to requested primary slot (to access sec. slotreg)
		LD	A,D
		RRCA
		RRCA
		AND	03H
		LD	D,A
		LD	A,0ABH
J0390:		ADD	A,55H
		DEC	D
		JP	P,J0390
		AND	E
		LD	D,A
		LD	A,E
		CPL
		LD	H,A
		LD	A,(D_FFFF)
		CPL
		LD	L,A
		AND	H
		OR	D
		LD	(D_FFFF),A
		LD	A,B
		OUT	(0A8H),A		; restore primary slotreg
		POP	AF
		AND	03H
		RET

; Subroutine CHKSLZ
CHKSLZ:
J03AC:		DI
		LD	C,0			; primary slot 0
		LD	DE,EXPTBL
		LD	HL,SLTATR
J03B5:		LD	A,(DE)
		OR	C
		LD	C,A			; set expanded slot flag, if slot is expanded
		PUSH	DE
J03B9:		PUSH	HL
		LD	HL,0
		CALL	RDWEXP
		PUSH	HL
		LD	HL,04443H		; subrom identifier
		RST	20H
		POP	HL
		LD	B,0			; reset extension flags
		JR	NZ,J03EF		; not a subrom, skip init and flags
		CALL	RDWEXP
		PUSH	HL			; store address
		PUSH	BC			; store extension flags, slotid
		PUSH	DE
		POP	IX			; IX = address INIT
		LD	A,C
		PUSH	AF
		POP	IY			; IYH = slotid
		CALL	NZ,C0217		; extension has a INIT, CALSLT
		POP	BC			; restore extension flags, slotid
		POP	HL			; restore address
		CALL	RDWEXP
		ADD	A,0FFH
		RR	B			; extension statement handler flag
		CALL	RDWEXP
		ADD	A,0FFH
		RR	B			; extension device handler flag
		SRL	B			; extension basic program flag (no basic program)
		LD	DE,-8
		ADD	HL,DE
J03EF:		EX	(SP),HL
		LD	(HL),B			; store extension flags
		EX	(SP),HL
		POP	HL
		INC	HL
		INC	HL
		INC	HL
		INC	HL
		LD	A,C
		AND	A			; expanded slot ?
		LD	DE,12
		JP	P,J040A			; nope, next primary slot
		ADD	A,4			; next secondary slot
		LD	C,A
		CP	90H			; all secondary slots done ?
		JR	C,J03B9			; nope, next
		AND	03H
		LD	C,A
		DEFB	03EH			; next primary slot
J040A:		ADD	HL,DE
		POP	DE
		INC	DE
		INC	C			; next primary slot
		LD	A,C
		CP	04H			; all primary slots done ?
		JR	C,J03B5			; nope, next
		JP	C7D75			; handle extension ROM
