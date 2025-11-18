; ------------------------------------------------------------------------------
; msxgrp.asm
; BIOS graphic functions, MSX 2 version (version 2.0)
; ------------------------------------------------------------------------------

		ALIGN	150EH

; subrom symbols
S_GRPPRT		EQU	0089h
S_MAPXYC		EQU	0091h
S_READC			EQU	0095h
S_SETC			EQU	009Dh
S_TRIGHT		EQU	00A1h
S_RIGHTC		EQU	00A5h
S_TLEFTC		EQU	00A9h
S_LEFTC			EQU	00ADh
S_TDOWNC		EQU	00B1h
S_DOWNC			EQU	00B5h
S_TUPC			EQU	00B9h
S_UPC			EQU	00BDh
S_SCANR			EQU	00C1h
S_SCANL			EQU	00C5h
	

; Subroutine GRPPRT
GRPPRT:
J150E:		CALL	CHKNEW			; CHKNEW (check if new style screenmode)
		JR	C,J151C			; nope,
		PUSH	IX
		LD	IX,S_GRPPRT
		JP	SUBROM			; new style GRPPRT handled by SUBROM

J151C:		PUSH	HL
		PUSH	DE
		PUSH	BC
		PUSH	AF
		CALL	CNVCHR			; CNVCHR
		JR	NC,J1587
		JR	NZ,J152F
		CP	0DH
		JR	Z,J158A
		CP	20H
		JR	C,J1587
J152F:		CALL	GETPAT			; copy charpattern to PATWRK
		LD	A,(FORCLR)
		LD	(ATRBYT),A
		LD	HL,(GRPACY)
		EX	DE,HL
		LD	BC,(GRPACX)
		CALL	SCALXY			; SCALXY
		JR	NC,J1587
		CALL	MAPXYC			; MAPXYC
		LD	DE,PATWRK
		LD	C,8			; 8 pixels vertical
J154D:		LD	B,8			; 8 pixels horizontal
		CALL	FETCHC			; FETCHC
		PUSH	HL
		PUSH	AF
		LD	A,(DE)
J1555:		ADD	A,A
		PUSH	AF
		CALL	C,SETC			; has dot, SETC
		CALL	C1735			; pixel pos right if possible
		POP	HL
		JR	C,J1564			; not possible, quit with this row
		PUSH	HL
		POP	AF
		DJNZ	J1555
J1564:		POP	AF
		POP	HL
		CALL	STOREC			; STOREC
		CALL	TDOWNC			; TDOWNC (pixel pos down if possible)
		JR	C,J1572			; not possible, quit
		INC	DE
		DEC	C
		JR	NZ,J154D
J1572:		CALL	C15FB			; check if screenmode 2 or 4
		LD	A,(GRPACX)
		JR	Z,J1580			; yep,
		ADD	A,20H
		JR	C,J158A
		JR	J1584

J1580:		ADD	A,08H
		JR	C,J158A
J1584:		LD	(GRPACX),A
J1587:		JP	POPALL

J158A:		XOR	A
		LD	(GRPACX),A
		CALL	C15FB			; check if screenmode 2 or 4
		LD	A,(GRPACY)
		JR	Z,J1599			; yep,
		ADD	A,20H
		DEFB	001H			; skip next instruction
J1599:		ADD	A,08H
		CP	0C0H
		JR	C,J15A0
		XOR	A
J15A0:		LD	(GRPACY),A
		JR	J1587

; Subroutine SCALXY
SCALXY:
C15A5:		PUSH	HL
		PUSH	BC
		LD	B,1
		EX	DE,HL
		LD	A,H
		ADD	A,A
		JR	NC,J15B3
		LD	HL,0
		JR	J15C4

J15B3:		LD	DE,192			; msx1 style screenmodes have ymax=192
		LD	A,(SCRMOD)
		CP	5
		JR	C,J15BF
		LD	E,0D4H
J15BF:		RST	R_DCOMPR
		JR	C,J15C6			; y-coordinate in range
		EX	DE,HL
		DEC	HL			 ; use max y-coordinate
J15C4:		LD	B,0
J15C6:		EX	(SP),HL
		LD	A,H
		ADD	A,A
		JR	NC,J15D0
		LD	HL,0			; negative x-coordinate, use x=0
		JR	J15E3

J15D0:		LD	DE,256			; most screenmodes have xmax 256
		LD	A,(SCRMOD)
		AND	07H
		CP	06H
		JR	C,J15DE
		LD	D,512 / 256		; screenmode 6 and 7, xmax is 512
J15DE:		RST	R_DCOMPR
		JR	C,J15E5			; x-coordinate in range
		EX	DE,HL
		DEC	HL			 ; use max x-coordinate
J15E3:		LD	B,0
J15E5:		POP	DE
		LD	A,(SCRMOD)
		CP	03H
		JR	NZ,J15F5
		SRL	L
		SRL	L
		SRL	E
		SRL	E				; screenmode 3, divide coordinates by 4
J15F5:		LD	A,B
		RRCA
		LD	B,H
		LD	C,L
		POP	HL
		RET

; Subroutine check if screenmode 2 or 4
C15FB:		LD	A,(SCRMOD)
		CP	4
		RET	Z
		CP	2
		RET

; Subroutine MAPXYC
MAPXYC:
C1604:		LD	A,(SCRMOD)
		CP	5
		JR	NC,J163E
		CP	3
		JR	Z,J1648
		PUSH	BC
		LD	D,C
		LD	A,C
		AND	07H
		LD	C,A
		LD	HL,I1636
		ADD	HL,BC
		LD	A,(HL)
		LD	(CMASK),A
		LD	A,E
		RRCA
		RRCA
		RRCA
		AND	1FH
		LD	B,A
		LD	A,D
		AND	0F8H
		LD	C,A
		LD	A,E
		AND	07H
		OR	C
		LD	C,A
		LD	HL,(GRPCGP)
		ADD	HL,BC
		LD	(CLOC),HL
		POP	BC
		RET

I1636:		defb	10000000b
		defb	01000000b
		defb	00100000b
		defb	00010000b
		defb	00001000b
		defb	00000100b
		defb	00000010b
		defb	00000001b

J163E:		LD	H,B
		LD	L,C
		LD	(CLOC),HL
		LD	A,E
		LD	(CMASK),A
		RET

J1648:		PUSH	IX
		LD	IX,S_MAPXYC
		JP	SUBROM			; SUBROM

; Subroutine FETCHC
FETCHC:
C1651:		LD	A,(CMASK)
		LD	HL,(CLOC)
		RET

; Subroutine STOREC
STOREC:
C1658:		LD	(CMASK),A
		LD	(CLOC),HL
		RET

; Subroutine READC
READC:
C165F:		CALL	CHKNEW			; CHKNEW (check if new style screenmode)
		JR	C,J166D			; nope,
J1664:		PUSH	IX
		LD	IX,S_READC
		JP	SUBROM			; READC new style handled by SUBROM

J166D:		CALL	C15FB			; check if screenmode 2 or 4
		JR	NZ,J1664			; nope, use subrom version of READC
		PUSH	BC
		PUSH	HL
		CALL	FETCHC			; FETCHC
		LD	B,A
		CALL	RDVRM			; RDVRM
		AND	B
		PUSH	AF
		LD	BC,2000H
		ADD	HL,BC
		CALL	RDVRM			; RDVRM
		LD	B,A
		POP	AF
		LD	A,B
		JR	Z,J168D
		RRCA
		RRCA
		RRCA
		RRCA
J168D:		AND	0FH
		POP	HL
		POP	BC
		RET

; Subroutine SETATR
SETATR:
J1692:		CALL	C169A
		RET	C
		LD	(ATRBYT),A
		RET

; Subroutine validate graphical attribute
; Input:  A  = attribute
; Output: Cx = set if error
C169A:		PUSH	AF
		LD	A,(SCRMOD)
		CP	06H
		JR	Z,J16AE			; screenmode 6 has 4 colors
		CP	08H
		JR	Z,J16AB			; screenmode 8 has 256 colors, so it is always valid
		POP	AF
		CP	10H			; screenmode 2,3, 5 and 7 have 16 colors
		CCF
		RET

J16AB:		POP	AF
		AND	A
		RET

J16AE:		POP	AF
		CP	20H
		CCF
		RET	C
		CP	10H
		JR	C,J16BA
		AND	0FH
		RET

J16BA:		AND	03H
		PUSH	BC
		LD	B,A
		ADD	A,A
		ADD	A,A
		ADD	A,B
		POP	BC
		RET

; Subroutine SETC
SETC:
C16C3:		LD	A,(SCRMOD)
		CP	05H
		JR	NC,J16E7			; new style screenmode, use VDP command
		CP	03H
		JR	Z,J16DE			; screenmode 3 SETC handled by SUBROM
		PUSH	HL
		PUSH	BC
		PUSH	DE
		LD	A,(CMASK)
		LD	HL,(CLOC)
		CALL	C18A6
		POP	DE
		POP	BC
		POP	HL
		RET

J16DE:		PUSH	IX
		LD	IX,S_SETC
		JP	SUBROM			; SUBROM

J16E7:		LD	HL,(CLOC)
		LD	A,(CMASK)
		PUSH	AF
J16EE:		DI
		LD	A,02H
		OUT	(99H),A
		LD	A,8FH
		OUT	(99H),A
		PUSH	HL
		POP	HL
		IN	A,(99H)
		PUSH	AF
		XOR	A
		OUT	(99H),A
		LD	A,8FH
		OUT	(99H),A
		POP	AF
		EI
		RRCA
		JR	C,J16EE
		DI
		LD	A,24H
		OUT	(99H),A
		LD	A,91H
		OUT	(99H),A
		LD	A,L
		OUT	(9BH),A
		LD	A,H
		OUT	(9BH),A
		POP	AF
		OUT	(9BH),A
		LD	A,(ACPAGE)
		OUT	(9BH),A
		LD	A,2CH
		OUT	(99H),A
		LD	A,91H
		OUT	(99H),A
		LD	A,(ATRBYT)
		OUT	(9BH),A
		XOR	A
		OUT	(9BH),A
		LD	A,50H
		OUT	(9BH),A
		EI
		RET

; Subroutine pixel pos right if possible
C1735:		CALL	C15FB			; check if screenmode 2 or 4
		JR	Z,J1743			; yep,
		PUSH	IX
		LD	IX,S_TRIGHT
		JP	SUBROM			; SUBROM

J1743:		PUSH	HL
		CALL	FETCHC			; FETCHC
		RRCA
		JR	NC,J17AE
		LD	A,L
		AND	0F8H
		CP	0F8H
		LD	A,80H
		JR	NZ,J176B
		JP	J181C

; Subroutine RIGHTC
RIGHTC:
C1756:		CALL	C15FB			; check if screenmode 2 or 4
		JR	Z,J1764			; yep,
		PUSH	IX
		LD	IX,S_RIGHTC
		JP	SUBROM			; SUBROM

J1764:		PUSH	HL
		CALL	FETCHC			; FETCHC
		RRCA
		JR	NC,J17AE
J176B:		PUSH	DE
		LD	DE,8
		JR	J17A9

; Subroutine pixel pos left if possible
C1771:		CALL	C15FB			; check if screenmode 2 or 4
		JR	Z,J177F
		PUSH	IX
		LD	IX,S_TLEFTC
		JP	SUBROM			; SUBROM

J177F:		PUSH	HL
		CALL	FETCHC			; FETCHC
		RLCA
		JR	NC,J17AE
		LD	A,L
		AND	0F8H
		LD	A,01H
		JR	NZ,J17A5
		JP	J181C

; Subroutine LEFTC
LEFTC:
J1790:		CALL	C15FB			; check if screenmode 2 or 4
		JR	Z,J179E
		PUSH	IX
		LD	IX,S_LEFTC
		JP	SUBROM			; SUBROM

J179E:		PUSH	HL
		CALL	FETCHC			; FETCHC
		RLCA
		JR	NC,J17AE
J17A5:		PUSH	DE
		LD	DE,-8
J17A9:		ADD	HL,DE
		LD	(CLOC),HL
		POP	DE
J17AE:		LD	(CMASK),A
		AND	A
		POP	HL
		RET

; Subroutine TDOWNC
TDOWNC:
C17B4:		CALL	C15FB			; check if screenmode 2 or 4
		JR	Z,J17C2
		PUSH	IX
		LD	IX,S_TDOWNC
		JP	SUBROM			; SUBROM

J17C2:		PUSH	HL
		PUSH	DE
		LD	HL,(CLOC)
		PUSH	HL
		LD	HL,(GRPCGP)
		LD	DE,01700H
		ADD	HL,DE
		EX	DE,HL
		POP	HL
		RST	R_DCOMPR
		JR	C,J17EF
		LD	A,L
		INC	A
		AND	07H
		JR	NZ,J17EF
		JR	J181B

; Subroutine DOWNC
DOWNC:
J17DC:		CALL	C15FB			; check if screenmode 2 or 4
		JR	Z,J17EA
		PUSH	IX
		LD	IX,S_DOWNC
		JP	SUBROM			; SUBROM

J17EA:		PUSH	HL
		PUSH	DE
		LD	HL,(CLOC)
J17EF:		INC	HL
		LD	A,L
		LD	DE,00F8H
		JR	J1837

; Subroutine TUPC
TUPC:
J17F6:		CALL	C15FB			; check if screenmode 2 or 4
		JR	Z,J1804
		PUSH	IX
		LD	IX,S_TUPC
		JP	SUBROM			; SUBROM

J1804:		PUSH	HL
		PUSH	DE
		LD	HL,(CLOC)
		PUSH	HL
		LD	HL,(GRPCGP)
		LD	DE,0100H
		ADD	HL,DE
		EX	DE,HL
		POP	HL
		RST	R_DCOMPR
		JR	NC,J1832
		LD	A,L
		AND	07H
		JR	NZ,J1832
J181B:		POP	DE
J181C:		SCF
		POP	HL
		RET

; Subroutine UPC
UPC:
J181F:		CALL	C15FB			; check if screenmode 2 or 4
		JR	Z,J182D
		PUSH	IX
		LD	IX,S_UPC
		JP	SUBROM			; SUBROM

J182D:		PUSH	HL
		PUSH	DE
		LD	HL,(CLOC)
J1832:		LD	A,L
		DEC	HL
		LD	DE,0FF08H
J1837:		AND	07H
		JR	NZ,J183C
		ADD	HL,DE
J183C:		LD	(CLOC),HL
		AND	A
		POP	DE
		POP	HL
		RET

; Subroutine NSETCX
NSETCX:
C1843:		CALL	C15FB			; check if screenmode 2 or 4
		JP	NZ,J18F5
		PUSH	HL
		CALL	FETCHC			; FETCHC
		EX	(SP),HL
		ADD	A,A
		JR	C,J1869
		PUSH	AF
		LD	BC,-1
		RRCA
J1856:		ADD	HL,BC
		JR	NC,J189E
		RRCA
		JR	NC,J1856
		POP	AF
		DEC	A
		EX	(SP),HL
		PUSH	HL
		CALL	C18A6
		POP	HL
		LD	DE,8
		ADD	HL,DE
		EX	(SP),HL
J1869:		LD	A,L
		AND	07H
		LD	C,A
		LD	A,H
		RRCA
		LD	A,L
		RRA
		RRCA
		RRCA
		AND	3FH
		POP	HL
		LD	B,A
		JR	Z,J188D
J1879:		XOR	A
		CALL	WRTVRM			; WRTVRM
		LD	DE,2000H
		ADD	HL,DE
		LD	A,(ATRBYT)
		CALL	WRTVRM			; WRTVRM
		LD	DE,2008H
		ADD	HL,DE
		DJNZ	J1879
J188D:		DEC	C
		RET	M
		PUSH	HL
		LD	HL,I1897
		ADD	HL,BC
		LD	A,(HL)
		JR	J18A5

I1897:		defb	10000000b
		defb	11000000b
		defb	11100000b
		defb	11110000b
		defb	11111000b
		defb	11111100b
		defb	11111110b

J189E:		ADD	A,A
		DEC	A
		CPL
		LD	B,A
		POP	AF
		DEC	A
		AND	B
J18A5:		POP	HL

C18A6:		LD	B,A
		CALL	RDVRM			; RDVRM
		LD	C,A
		LD	DE,2000H
		ADD	HL,DE
		CALL	RDVRM			; RDVRM
		PUSH	AF
		AND	0FH
		LD	E,A
		POP	AF
		SUB	E
		LD	D,A
		LD	A,(ATRBYT)
		CP	E
		JR	Z,J18D8
		ADD	A,A
		ADD	A,A
		ADD	A,A
		ADD	A,A
		CP	D
		JR	Z,C18DC
		PUSH	AF
		LD	A,B
		OR	C
		CP	0FFH
		JR	Z,J18E4
		PUSH	HL
		PUSH	DE
		CALL	C18DC
		POP	DE
		POP	HL
		POP	AF
		OR	E
		JR	J18F2

J18D8:		LD	A,B
		CPL
		AND	C
		DEFB	011H			; LD DE,xxxx (skip next 2 instructions)
C18DC:		LD	A,B
		OR	C

C18DE:		LD	DE,2000H
		ADD	HL,DE
		JR	J18F2

J18E4:		POP	AF
		LD	A,B
		CPL
		PUSH	HL
		PUSH	DE
		CALL	C18DE
		POP	DE
		POP	HL
		LD	A,(ATRBYT)
		OR	D
J18F2:		JP	WRTVRM			; WRTVRM

J18F5:		PUSH	HL
		CALL	SETC			; SETC
		CALL	RIGHTC			; RIGHTC
		POP	HL
		DEC	L
		JR	NZ,J18F5
		RET

; Subroutine GTASPC
GTASPC:
J1901:	LD	HL,(ASPCT1)
		EX	DE,HL
		LD	HL,(ASPCT2)
		RET

; Subroutine PNTINI
PNTINI:
J1909:		PUSH	AF
		CALL	C15FB			; check if screenmode 2 or 4
		JR	Z,J1915
		POP	AF
		CP	10H
		CCF
		JR	J191A

J1915:		POP	AF
		LD	A,(ATRBYT)
		AND	A
J191A:		LD	(BRDATR),A
		RET

; Subroutine SCANR
SCANR:
J191E:		LD	HL,0
		LD	C,L
		CALL	C15FB			; check if screenmode 2 or 4
		JR	Z,J1930
		PUSH	IX
		LD	IX,S_SCANR
		JP	SUBROM			; SUBROM

J1930:		LD	A,B
		LD	(FILNAM+0),A
		XOR	A
		LD	(FILNAM+3),A
		LD	A,(BRDATR)
		LD	B,A
J193C:		CALL	READC			; READC
		CP	B
		JR	NZ,J194F
		DEC	DE
		LD	A,D
		OR	E
		RET	Z
		CALL	C1735			; pixel pos right if possible
		JR	NC,J193C			; is possible,
		LD	DE,0
		RET

J194F:		CALL	C19D1
		PUSH	DE
		CALL	FETCHC			; FETCHC
		LD	(CSAVEA),HL
		LD	(CSAVEM),A
		LD	DE,0
J195F:		INC	DE
		CALL	C1735			; pixel pos right if possible
		JR	C,J1970			; not possible,
		CALL	READC			; READC
		CP	B
		JR	Z,J1970
		CALL	C19D1
		JR	J195F

J1970:		PUSH	DE
		CALL	FETCHC			; FETCHC
		PUSH	HL
		PUSH	AF
		LD	HL,(CSAVEA)
		LD	A,(CSAVEM)
		CALL	STOREC			; STOREC
		EX	DE,HL
		LD	(FILNAM+1),HL
		LD	A,(FILNAM+0)
		AND	A
		CALL	NZ,NSETCX			; NSETCX
		POP	AF
		POP	HL
		CALL	STOREC			; STOREC
		POP	HL
		POP	DE
		JP	J19CC

; Subroutine SCANL
SCANL:
J1994:		LD	HL,0
		LD	C,L
		CALL	C15FB			; check if screenmode 2 or 4
		JR	Z,J19A6
		PUSH	IX
		LD	IX,S_SCANL
		JP	SUBROM			; SUBROM

J19A6:		XOR	A
		LD	(FILNAM+3),A
		LD	A,(BRDATR)
		LD	B,A
J19AE:		CALL	C1771			; pixel pos left if possible
		JR	C,J19C2			; not possible,
		CALL	READC			; READC
		CP	B
		JR	Z,J19BF
		CALL	C19D1
		INC	HL
		JR	J19AE

J19BF:		CALL	RIGHTC			; RIGHTC
J19C2:		PUSH	HL
		LD	DE,(FILNAM+1)
		ADD	HL,DE
		CALL	NSETCX			; NSETCX
		POP	HL
J19CC:		LD	A,(FILNAM+3)
		LD	C,A
		RET

C19D1:		PUSH	HL
		LD	HL,ATRBYT
		CP	(HL)
		POP	HL
		RET	Z
		INC	A
		LD	(FILNAM+3),A
		RET

