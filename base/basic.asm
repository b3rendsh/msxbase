; ------------------------------------------------------------------------------
; basic.asm
; MSX 1 BASIC version 1.0
; MSX 2 BASIC version 2.0
;
; Modules: BASHDR,F4,BINTRP,MSXEXP,MACLNG,GENGRP,ADVGRP,BIPTRG,BIPRTU,
;	  BIMISC,BISTRS,SPCDSK,DSKCOM,SPCDEV,BASBIO,MSXSTS,INIT
;
; Code copyright by ASCII and maybe others
; Source comments by Arjen Zeilemaker
;
; Source code supplied for STUDY ONLY
; Recreation NOT permitted without authorisation of the copyright holder(s)
;
; H.J. Berends:
; Merged modules and converted sources to assemble with z88dk z80asm
; Restructured and reviewed comments
; Merged MSX 1 and MSX 2 BASIC common modules
; ------------------------------------------------------------------------------

DEFINE	_BASIC_	; avoid duplicate label definitions

		INCLUDE	"base.inc"
		INCLUDE "msx.inc"

; Symbols defined in BIOS module
CGTABL		EQU	0004H
RDSLT		EQU	000CH
OUTDO		EQU	0018H
CALSLT		EQU	001CH
ENASLT		EQU	0024H
INITIO		EQU	003BH
INIFNK		EQU	003EH
WRTVDP		EQU	0047H
WRTVRM		EQU	004DH
RDVRM		EQU	004AH
FILVRM		EQU	0056H
LDIRMV		EQU	0059H
LDIRVM		EQU	005CH
CHGMOD		EQU	005FH
CHGCLR		EQU	0062H
CLRSPR		EQU	0069H
INITXT		EQU	006CH
INIT32		EQU	006FH
SETTXT		EQU	0078H
SETGRP		EQU	007EH
SETMLT		EQU	0081H
CALPAT		EQU	0084H
CALATR		EQU	0087H
GSPSIZ		EQU	008AH
GRPPRT		EQU	008DH
GICINI		EQU	0090H
WRTPSG		EQU	0093H
STRTMS		EQU	0099H
CHSNS		EQU	009CH
CHGET		EQU	009FH
CHPUT		EQU	00A2H
LPTOUT		EQU	00A5H
CNVCHR		EQU	00ABH
PINLIN		EQU	00AEH
INLIN		EQU	00B1H
QINLIN		EQU	00B4H
ISCNTC		EQU	00BAH
CKCNTC		EQU	00BDH
BEEP		EQU	00C0H
CLS		EQU	00C3H
POSIT		EQU	00C6H
FNKSB		EQU	00C9H
ERAFNK		EQU	00CCH
DSPFNK		EQU	00CFH
TOTEXT		EQU	00D2H
GTSTCK		EQU	00D5H
GTTRIG		EQU	00D8H
GTPAD		EQU	00DBH
GTPDL		EQU	00DEH
TAPION		EQU	00E1H
TAPIN		EQU	00E4H
TAPIOF		EQU	00E7H
TAPOON		EQU	00EAH
TAPOUT		EQU	00EDH
TAPOOF		EQU	00F0H
STMOTR		EQU	00F3H
LFTQ		EQU	00F6H
PUTQ		EQU	00F9H
ISFLIO		EQU	014AH
OUTDLP		EQU	014DH
GETVCP		EQU	0150H
GETVC2		EQU	0153H

	IFDEF MSX2
; Symbols defined in SUBROM module
S_PAINT		EQU	0069H
S_PSET		EQU	006DH
S_GLINE		EQU	0075H
S_DOGRPH	EQU	0085H
S_PUTSPRT       EQU     0151H
S_COLOR		EQU	0155H
S_SCREEN	EQU	0159H
S_WIDTHS	EQU	015DH
S_VDP		EQU	0161H
S_VDPF		EQU	0165H
S_BASE		EQU	0169H
S_BASEF		EQU	016DH
S_VPOKE		EQU	0171H
S_VPEEK		EQU	0175H
S_SETS          EQU     0179H
S_PROMPT	EQU	0181H
S_SDFSCR        EQU     0185H
S_SETSCR	EQU	0189H
S_SCOPY         EQU     018DH
S_GETPUT	EQU	01B1H
; Symbols defined in BIOS module
EXTROM		EQU	015FH
CHKSLZ		EQU	0162H
CHKNEW		EQU	0165H
BIGFIL		EQU	016BH
J0431		EQU	0431H
; Symbol defined in DISK system
BDOS		EQU	0F37DH
	ENDIF

SYSHOOK		MACRO	H
		IFDEF SYSHOOKS
		call	H
		ELSE
		nop
		nop
		nop
		ENDIF
		ENDM

; ------------------------------------------------------------------------------
; BASHDR.MAC
; BASIC jump table
; ------------------------------------------------------------------------------

		PHASE	2680H
		
; entrypoint for BASIC interpreter init
INIENT:
A2680:		jp	INIT

; entrypoint for SYNCHR
SYNENT:
A2683:		jp	SYNCHR

; entrypoint for CHRGTR
CHRENT:
A2686:		jp	CHRGTR

; entrypoint for GETYPR
GETENT:
A2689:		jp	GETYPR


; ------------------------------------------------------------------------------
; F4.MAC
; BASIC MATH functions
; ------------------------------------------------------------------------------

		ALIGN	268CH

; Subroutine DECSUB (dbl subtract)
; Input:  (DAC) = first operand
;	  (ARG) = second operand
DECSUB:
C268C:		LD	HL,ARG+0
		LD	A,(HL)
		OR	A			; 2nd operand zero ?
		RET	Z			; yep, quit (result in DAC)
		XOR	80H
		LD	(HL),A			; negate 2nd operand
		JR	J26A0			; and do ADD

; Subroutine double real addition
; Input:  (DAC) = first operand, (HL) = second operand
DECADM:
C2697:		CALL	VMOVAM			; ARG = HL

; Subroutine DECADD (dbl addition)
; Input:  (DAC) = first operand, (ARG) = second operand
DECADD:
C269A:		LD	HL,ARG+0
		LD	A,(HL)
		OR	A
		RET	Z			; 2nd operand zero, quit (result in DAC)
J26A0:		AND	7FH
		LD	B,A			; store exponent 2nd operand without sign
		LD	DE,DAC+0
		LD	A,(DE)
		OR	A			; 1st operand zero ?
		JP	Z,C2F05		 	; yep, DAC = ARG and quit
		AND	7FH			; exponent 1st operand without sign
		SUB	B
		JR	NC,J26C1		; skip swap
		CPL
		INC	A			; negate
		PUSH	AF			; store exponent delta
		PUSH	HL			; store pointer to 2nd operand
		LD	B,8
J26B6:		LD	A,(DE)
		LD	C,(HL)
		LD	(HL),A
		LD	A,C
		LD	(DE),A
		INC	DE
		INC	HL
		DJNZ	J26B6		   	; swap DAC and ARG
		POP	HL			; restore pointer to 2nd operand
		POP	AF			; restore exponent delta
J26C1:		CP	15+1			; exponent delta > 15 ?
		RET	NC			; yep, result = DAC
		PUSH	AF			; store exponent delta
		XOR	A
		LD	(DAC+8),A		; clear expansion byte DAC
		LD	(ARG+8),A		; clear expansion byte ARG
		LD	HL,ARG+1
		POP	AF			; restore exponent delta
		CALL	C27A3		   	; shift mantissa right
		LD	HL,ARG+0
		LD	A,(DAC+0)
		XOR	(HL)			; signs equal ?
		JP	M,J26F7		 	; nope,
		LD	A,(ARG+8)
		LD	(DAC+8),A		; expansion byte DAC
		CALL	C2759		   	; add DAC mantissa with ARG mantissa
		JP	NC,C273C		; no overflow, round up DAC and quit
		EX	DE,HL
		LD	A,(HL)
		INC	(HL)			; increase exponent
		XOR	(HL)			; exponent overflow ?
		JP	M,OVERR		 	; yep, overflow error
		CALL	C27DB		   	; shift mantissa 1 digit right
		SET	4,(HL)
		JR	C273C		   	; round up DAC and quit

J26F7:		CALL	C276B		   	; subtract mantissa

; Subroutine DECNRM (normalize DAC)
DECNRM:
C26FA:		LD	HL,DAC+1
		LD	BC,8*256+0		; mantissa + expansion byte, clear counter
J2700:		LD	A,(HL)
		OR	A			; both BCD digits zero ?
		JR	NZ,J270C		; nope,
		INC	HL
		DEC	C
		DEC	C
		DJNZ	J2700		   	; next BCD byte
		JP	J2E7D		   	; DAC = 0 and quit

J270C:		AND	0F0H			; high BCD digit zero ?
		JR	NZ,J2716		; nope,
		PUSH	HL
		CALL	C2797		   	; shift mantissa DAC 1 digit left
		POP	HL
		DEC	C
J2716:		LD	A,8
		SUB	B
		JR	Z,J272D
		PUSH	AF
		PUSH	BC
		LD	C,B
		LD	DE,DAC+1
		LD	B,0
		LDIR
		POP	BC
		POP	AF
		LD	B,A
		XOR	A
J2729:		LD	(DE),A
		INC	DE
		DJNZ	J2729
J272D:		LD	A,C
		OR	A
		JR	Z,C273C		 	; round up DAC and quit
		LD	HL,DAC+0
		LD	B,(HL)
		ADD	A,(HL)
		LD	(HL),A
		XOR	B
		JP	M,OVERR		 	; overflow error
		RET	Z

; Subroutine DECROU (round up DAC)
DECROU:
C273C:		LD	HL,DAC+8		; mantissa + expansion byte
		LD	B,7			; number of bytes = 7

; Subroutine round up
C2741:		LD	A,(HL)
		CP	50H			; expansion byte > 49 ?
		RET	C			; nope, quit
		DEC	HL
		XOR	A
		SCF
J2748:		ADC	A,(HL)
		DAA
		LD	(HL),A
		RET	NC
		DEC	HL
		DJNZ	J2748		   	; increase mantissa
		LD	A,(HL)
		INC	(HL)			; increase exponent
		XOR	(HL)			; exponent overflow ?
		JP	M,OVERR		 	; yep, overflow error
		INC	HL
		LD	(HL),10H
		RET

; Subroutine add DAC mantissa with ARG mantissa
C2759:		LD	HL,ARG+7
		LD	DE,DAC+7
		LD	B,7

; Subroutine add mantissa's
C2761:		XOR	A
J2762:		LD	A,(DE)
		ADC	A,(HL)
		DAA
		LD	(DE),A
		DEC	DE
		DEC	HL
		DJNZ	J2762
		RET

; Subroutine subtract mantissa
C276B:		LD	HL,ARG+8
		LD	A,(HL)
		CP	50H			; expansion byte ARG = 50 ?
		JR	NZ,J2774		; nope,
		INC	(HL)			; expansion byte ARG = 51
J2774:		LD	DE,DAC+8		; include expansion byte
		LD	B,8			; mantissa + expansion byte
		XOR	A
J277A:		LD	A,(DE)
		SBC	A,(HL)
		DAA
		LD	(DE),A
		DEC	DE
		DEC	HL
		DJNZ	J277A		   	; subtract DAC mantissa with ARG mantissa
		RET	NC
		EX	DE,HL
		LD	A,(HL)
		XOR	80H
		LD	(HL),A		  	; flip sign
		LD	HL,DAC+8		; include expansion byte
		LD	B,8			; mantissa + expansion byte
		XOR	A
J278E:		LD	A,0
		SBC	A,(HL)
		DAA
		LD	(HL),A
		DEC	HL
		DJNZ	J278E
		RET

; Subroutine shift mantissa DAC 1 digit left
C2797:		LD	HL,DAC+8		; include expansion byte

; Subroutine shift mantissa 1 digit left
C279A:		PUSH	BC
		XOR	A
J279C:		RLD
		DEC	HL
		DJNZ	J279C
		POP	BC
		RET

; Subroutine shift mantissa right
C27A3:		OR	A
		RRA
		PUSH	AF
		OR	A
		JP	Z,J27E2
		PUSH	AF
		CPL
		INC	A
		LD	C,A
		LD	B,0FFH
		LD	DE,7
		ADD	HL,DE
		LD	D,H
		LD	E,L
		ADD	HL,BC
		LD	A,8
		ADD	A,C
		LD	C,A
		PUSH	BC
		LD	B,0
		LDDR
		POP	BC
		POP	AF
		INC	HL
		INC	DE
		PUSH	DE
		LD	B,A
		XOR	A
J27C7:		LD	(HL),A
		INC	HL
		DJNZ	J27C7
		POP	HL
		POP	AF
		RET	NC
		LD	A,C
J27CF:		PUSH	HL
		PUSH	BC
		LD	B,A
		XOR	A
J27D3:		RRD
		INC	HL
		DJNZ	J27D3
		POP	BC
		POP	HL
		RET

; Subroutine shift mantissa 1 digit right
C27DB:		LD	HL,DAC+1
J27DE:		LD	A,8
		JR	J27CF

J27E2:		POP	AF
		RET	NC
		JR	J27DE

; Subroutine DECMUL
DECMUL:
C27E6:		CALL	SIGN			; get sign DAC
		RET	Z			; DAC is zero, quit (result is zero)
		LD	A,(ARG+0)
		OR	A			; 2nd operand zero ?
		JP	Z,J2E7D		 	; yep, DAC = 0 and quit
		LD	B,A			; exponent 2nd operand
		LD	HL,DAC+0
		XOR	(HL)
		AND	80H
		LD	C,A			; store exponent sign difference
		RES	7,B			; exponent 2nd operand without sign
		LD	A,(HL)
		AND	7FH			; exponent 1st operand without sign
		ADD	A,B
		LD	B,A
		LD	(HL),0			; DAC = 0.0
		AND	0C0H
		RET	Z
		CP	0C0H
		JR	NZ,J280C
		JP	OVERR			; overflow error

J280C:		LD	A,B
		ADD	A,40H
		AND	7FH
		RET	Z
		OR	C
		DEC	HL
		LD	(HL),A
		LD	DE,HOLD8+56+7
		LD	BC,8
		LD	HL,DAC+7
		PUSH	DE
		LDDR				; DAC copy to HOLD8+56
		INC	HL
		XOR	A
		LD	B,8
J2825:		LD	(HL),A
		INC	HL
		DJNZ	J2825		   	; clear DAC (0.0)
		POP	DE			; HOLD8+56+7
		LD	BC,J2883
		PUSH	BC			; after this,

; Subroutine construct multiply factors
; Input:  DE = pointer to X+7, ARG = multiplier
C282E:		CALL	C288A			; construct X*2, X*4, X*8
		PUSH	HL
		LD	BC,8
		EX	DE,HL
		LDDR				; copy X*8
		EX	DE,HL
		LD	HL,HOLD8+55
		LD	B,8			; X*2
		CALL	C2761			; add mantissa's
		POP	DE
		CALL	C288A			; construct X*10, X*20, X*40
		LD	C,7			; number of BCD bytes
		LD	DE,ARG+7
J284A:		LD	A,(DE)
		OR	A			; BCD byte = 0 ?
		JR	NZ,J2852		; nope,
		DEC	DE
		DEC	C
		JR	J284A			; next byte

J2852:		LD	A,(DE)			; BCD byte
		DEC	DE
		PUSH	DE
		LD	HL,HOLD8+7		; first factor (X*40)
J2858:		ADD	A,A			; shift bit out
		JR	C,J2863			; bit = 1, add factor to DAC
		JR	Z,J2871			; no more 1 bits, quit
J285D:		LD	DE,8
		ADD	HL,DE			; to next factor
		JR	J2858			; next bit

J2863:		PUSH	AF			; store byte
		LD	B,8
		LD	DE,DAC+7
		PUSH	HL			; store pointer to factor
		CALL	C2761		   	; add mantissa's
		POP	HL			; restore pointer to factor
		POP	AF			; restore byte
		JR	J285D		   	; next factor and bit

J2871:		LD	B,15
		LD	DE,DAC+14		; source
		LD	HL,DAC+15		; destination
		CALL	C2EFE			; copy bytes (decrease), this will shift DAC 2 BCD digits to right
		LD	(HL),0
		POP	DE
		DEC	C			; update number BCD bytes
		JR	NZ,J2852		; next BCD byte
		RET

J2883:		DEC	HL
		LD	A,(HL)
		INC	HL
		LD	(HL),A
		JP	C26FA			; normalize DAC

; Subroutine construct X*2, X*4, X*8
; Input:  DE = pointer to X+7
C288A:		LD	HL,-8
		ADD	HL,DE
		LD	C,3
J2890:		LD	B,8
		OR	A			; clear Cx
J2893:		LD	A,(DE)
		ADC	A,A
		DAA
		LD	(HL),A
		DEC	HL
		DEC	DE
		DJNZ	J2893
		DEC	C
		JR	NZ,J2890
		RET

; Subroutine DECDIV (dbl division)
; Input:  (DAC) = first operand
;	 (ARG) = second operand
DECDIV:
C289F:		LD	A,(ARG+0)
		OR	A			; operand2 is zero ?
		JP	Z,DV0ERR		; yep, division by zero error
		LD	B,A
		LD	HL,DAC+0
		LD	A,(HL)
		OR	A
		JP	Z,J2E7D			; DAC = 0 and quit
		XOR	B
		AND	80H
		LD	C,A
		RES	7,B
		LD	A,(HL)
		AND	7FH
		SUB	B
		LD	B,A
		RRA
		XOR	B
		AND	40H
		LD	(HL),00H
		JR	Z,J28C9
		LD	A,B
		AND	80H
		RET	NZ
J28C6:		JP	OVERR			; overflow error

J28C9:		LD	A,B
		ADD	A,41H
		AND	7FH
		LD	(HL),A
		JR	Z,J28C6
		OR	C
		LD	(HL),00H
		DEC	HL
		LD	(HL),A
		LD	DE,DAC+7
		LD	HL,ARG+7
		LD	B,7
		XOR	A
J28DF:		CP	(HL)
		JR	NZ,J28E6
		DEC	DE
		DEC	HL
		DJNZ	J28DF
J28E6:		LD	(DECTM2),HL
		EX	DE,HL
		LD	(DECTMP),HL
		LD	A,B
		LD	(DECCNT),A
		LD	HL,HOLD8+56
J28F4:		LD	B,0FH
J28F6:		PUSH	HL
		PUSH	BC
		LD	HL,(DECTM2)
		EX	DE,HL
		LD	HL,(DECTMP)
		LD	A,(DECCNT)
		LD	C,0FFH
J2904:		INC	C
		LD	B,A
		PUSH	HL
		PUSH	DE
		XOR	A
		EX	DE,HL
J290A:		LD	A,(DE)
		SBC	A,(HL)
		DAA
		LD	(DE),A
		DEC	HL
		DEC	DE
		DJNZ	J290A
		LD	A,(DE)
		SBC	A,B
		LD	(DE),A
		POP	DE
		POP	HL
		LD	A,(DECCNT)
		JR	NC,J2904
		LD	B,A
		EX	DE,HL
		CALL	C2761		   	; add mantissa's
		JR	NC,J2925
		EX	DE,HL
		INC	(HL)
J2925:		LD	A,C
		POP	BC
		LD	C,A
		PUSH	BC
		SRL	B
		INC	B
		LD	E,B
		LD	D,0
		LD	HL,DAC-1
		ADD	HL,DE
		CALL	C279A		   	; shift mantissa 1 digit left
		POP	BC
		POP	HL
		LD	A,B
		INC	C
		DEC	C
		JR	NZ,J2973
		CP	0FH
		JR	Z,J2964
		RRCA
		RLCA
		JR	NC,J2973
		PUSH	BC
		PUSH	HL
		LD	HL,DAC
		LD	B,8
		XOR	A
J294D:		CP	(HL)
		JR	NZ,J295F
		INC	HL
		DJNZ	J294D
		POP	HL
		POP	BC
		SRL	B
		INC	B
		XOR	A
J2959:		LD	(HL),A
		INC	HL
		DJNZ	J2959
		JR	J2985

J295F:		POP	HL
		POP	BC
		LD	A,B
		JR	J2973

J2964:		LD	A,(DECCNT+1)
		LD	E,A
		DEC	A
		LD	(DECCNT+1),A
		XOR	E
		JP	P,J28F4
		JP	J2E7D		   	; DAC = 0 and quit

J2973:		RRA
		LD	A,C
		JR	C,J297C
		OR	(HL)
		LD	(HL),A
		INC	HL
		JR	J2981

J297C:		ADD	A,A
		ADD	A,A
		ADD	A,A
		ADD	A,A
		LD	(HL),A
J2981:		DEC	B
		JP	NZ,J28F6
J2985:		LD	HL,DAC+8		; include expansion byte
		LD	DE,HOLD8+63
		LD	B,8			; mantissa + expansion byte
		CALL	C2EFE			; copy bytes (decrease)
		JP	J2883

; Subroutine COS function
COS:
C2993:		LD	HL,I2D63
		CALL	C2C3B			; DAC = DAC * 1/(2*pi)
		LD	A,(DAC+0)
		AND	7FH
		LD	(DAC+0),A		; clear sign bit ( cos(-x) = cos(x) )
		LD	HL,I2D23
		CALL	C2C32			; DAC = DAC - 0.25
		CALL	C2E8D			; NEG DAC
		JR	J29B2

; Subroutine SIN function
SIN:
C29AC:		LD	HL,I2D63
		CALL	C2C3B			; DAC = DAC * 1/(2*pi)
J29B2:		LD	A,(DAC+0)
		OR	A
		CALL	M,C2C80			; negative, start operation (code after call), negate DAC before and after (SIN (-x) = - SIN(x))
		CALL	C2CCC			; push DAC
		CALL	VINT			; INT DAC
		CALL	C2C4D			; ARG = DAC
		CALL	C2CE1			; pop DAC
		CALL	DECSUB			; DAC - ARG
		LD	A,(DAC+0)
		CP	40H
		JP	C,J29F5
		LD	A,(DAC+1)
		CP	25H
		JP	C,J29F5
		CP	75H
		JP	NC,J29EC
		CALL	C2C4D			; ARG = DAC
		LD	HL,I2D11
		CALL	C2C5C			; DAC = 0.5
		CALL	DECSUB			; DAC - ARG
		JP	J29F5

J29EC:		LD	HL,FONE
		CALL	C2C50			; ARG = 1.0
		CALL	DECSUB			; DAC - ARG
J29F5:		LD	HL,I2DEF		; sinus polynomial series
		JP	C2C88			; polynomial approximation odd series

; Subroutine TAN function
TAN:
C29FB:		CALL	C2CCC			; push DAC
		CALL	COS			; COS DAC
		CALL	C2C6F			; exchange DAC with stack
		CALL	SIN			; SIN DAC
		CALL	C2CDC			; pop ARG
		LD	A,(ARG+0)
		OR	A
		JP	NZ,DECDIV		; DAC / ARG and quit
		JP	OVERR			; overflow error

; Subroutine ATN function
ATN:
C2A14:		LD	A,(DAC+0)
		OR	A			; DAC zero ?
		RET	Z			; yep, result zero
		CALL	M,C2C80			; negative, start operation (code after call), negate DAC before and after (ATAN(X)=-ATAN(-X))
		CP	41H
		JP	C,C2A3C
		CALL	C2C4D			; ARG = DAC
		LD	HL,FONE
		CALL	C2C5C			; DAC = 1.0
		CALL	DECDIV			; DAC / ARG
		CALL	C2A3C
		CALL	C2C4D			; ARG = DAC
		LD	HL,I2D43
		CALL	C2C5C			; DAC = 1.5707963267949 (PI/2)
		JP	DECSUB			; DAC - ARG

C2A3C:		LD	HL,I2D4B
		CALL	C2C47			; compare DAC with 0.26794919243112 (tan(pi/12))
		JP	M,C2A6C
		CALL	C2CCC			; push DAC
		LD	HL,I2D53
		CALL	C2C2C			; DAC = DAC + 1.7320508075689 (sqr(3))
		CALL	C2C6F			; exchange DAC with stack
		LD	HL,I2D53
		CALL	C2C3B			; DAC = DAC * 1.7320508075689 (sqr(3))
		LD	HL,FONE
		CALL	C2C32			; DAC = DAC - 1.0
		CALL	C2CDC			; pop ARG
		CALL	DECDIV			; DAC / ARG
		CALL	C2A6C
		LD	HL,I2D5B
		JP	C2C2C			; DAC = DAC + 0.52359877559830 (pi/6)

C2A6C:		LD	HL,I2E30		; atan polynomial series
		JP	C2C88			; polynomial approximation odd series

; Subroutine LOG function
LOG:
C2A72:		CALL	SIGN			; get sign DAC
		JP	M,FCERR			; DAC is negative, illegal function call
		JP	Z,FCERR			; DAC is zero, illegal function call
		LD	HL,DAC+0
		LD	A,(HL)
		PUSH	AF
		LD	(HL),41H
		LD	HL,I2D2B
		CALL	C2C47			; compare DAC with 3.1622776601684 (sqr(10))
		JP	M,J2A92
		POP	AF
		INC	A
		PUSH	AF
		LD	HL,DAC+0
		DEC	(HL)
J2A92:		POP	AF
		LD	(TEMP3),A
		CALL	C2CCC			; push DAC
		LD	HL,FONE
		CALL	C2C2C			; DAC = DAC + 1.0
		CALL	C2C6F			; exchange DAC with stack
		LD	HL,FONE
		CALL	C2C32			; DAC = DAC - 1.0
		CALL	C2CDC			; pop ARG
		CALL	DECDIV			; DAC / ARG
		CALL	C2CCC			; push DAC
		CALL	C2C38			; DAC = DAC^2
		CALL	C2CCC			; push DAC
		CALL	C2CCC			; push DAC
		LD	HL,I2DC6		; log polynomial series #2
		CALL	C2CA3			; polynomial approximation
		CALL	C2C6F			; exchange DAC with stack
		LD	HL,I2DA5		; log polynomial series #1
		CALL	C2CA3			; polynomial approximation
		CALL	C2CDC			; pop ARG
		CALL	DECDIV			; DAC / ARG
		CALL	C2CDC			; pop ARG
		CALL	DECMUL			; DAC * ARG
		LD	HL,I2D33
		CALL	C2C2C			; DAC = DAC + 0.86858896380650 (2^log(e))
		CALL	C2CDC			; pop ARG
		CALL	DECMUL			; DAC * ARG
		CALL	C2CCC			; push DAC
		LD	A,(TEMP3)
		SUB	41H
		LD	L,A
		ADD	A,A
		SBC	A,A
		LD	H,A
		CALL	CONSIH			; convert integer to single real
		CALL	C3042			; convert DAC from single real to double real
		CALL	C2CDC			; pop ARG
		CALL	DECADD			; DAC + ARG
		LD	HL,I2D3B
		JP	C2C3B			; DAC = DAC * 2.3025850929940 (1/log(e))

; Subroutine SQR function
SQR:
C2AFF:		CALL	SIGN			; get sign DAC
		RET	Z			; DAC is zero, quit (result is zero)
		JP	M,FCERR			; DAC is negative, illegal function call
		CALL	C2C4D			; ARG = DAC
		LD	A,(DAC+0)
		OR	A
		RRA
		ADC	A,20H
		LD	(ARG+0),A
		LD	A,(DAC+1)
		OR	A			; clear Cx
		RRCA
		OR	A			; clear Cx
		RRCA
		AND	33H
		ADD	A,10H
		LD	(ARG+1),A
		LD	A,7
J2B23:		LD	(TEMP3),A
		CALL	C2CCC			; push DAC
		CALL	C2CC7			; push ARG
		CALL	DECDIV			; DAC / ARG
		CALL	C2CDC			; pop ARG
		CALL	DECADD			; DAC + ARG
		LD	HL,I2D11
		CALL	C2C3B			; DAC = DAC * 0.5
		CALL	C2C4D			; ARG = DAC
		CALL	C2CE1			; pop DAC
		LD	A,(TEMP3)
		DEC	A
		JR	NZ,J2B23
		JP	C2C59			; DAC = ARG

; Subroutine EXP function
EXP:
C2B4A:		LD	HL,I2D09
		CALL	C2C3B			; DAC = DAC * 0.43429448190324 (log(e))
		CALL	C2CCC			; push DAC
		CALL	FRCINT			; convert DAC to integer
		LD	A,L
		RLA
		SBC	A,A
		CP	H
		JR	Z,J2B70
		LD	A,H
		OR	A
		JP	P,J2B6D			; overflow error
		CALL	C304F			; DAC type = double real
		CALL	C2CE1			; pop DAC
		LD	HL,I2D13
		JP	C2C5C			; DAC = 0.0

J2B6D:		JP	OVERR			; overflow error

J2B70:		LD	(TEMP3),HL
		CALL	FRCDBL			; convert DAC to double real
		CALL	C2C4D			; ARG = DAC
		CALL	C2CE1			; pop DAC
		CALL	DECSUB			; DAC - ARG
		LD	HL,I2D11
		CALL	C2C47			; compare DAC with 0.5
		PUSH	AF
		JR	Z,J2B90
		JR	C,J2B90
		LD	HL,I2D11
		CALL	C2C32			; DAC = DAC - 0.5
J2B90:		CALL	C2CCC			; push DAC
		LD	HL,I2D8C
		CALL	C2C88			; polynomial approximation odd series
		CALL	C2C6F			; exchange DAC with stack
		LD	HL,I2D6B
		CALL	C2C9A			; polynomial approximation even series
		CALL	C2CDC			; pop ARG
		CALL	C2CC7			; push ARG
		CALL	C2CCC			; push DAC
		CALL	DECSUB			; DAC - ARG
		LD	HL,HOLD8+56
		CALL	C2C67			; = DAC
		CALL	C2CDC			; pop ARG
		CALL	C2CE1			; pop DAC
		CALL	DECADD			; DAC + ARG
		LD	HL,HOLD8+56
		CALL	C2C50			; ARG =
		CALL	DECDIV			; DAC / ARG
		POP	AF
		JR	C,J2BD1
		JR	Z,J2BD1
		LD	HL,I2D2B
		CALL	C2C3B			; DAC = DAC * 3.1622776601684 (sqr(10))
J2BD1:		LD	A,(TEMP3)
		LD	HL,DAC+0
		LD	C,(HL)
		ADD	A,(HL)
		LD	(HL),A
		XOR	C
		RET	P
		JP	OVERR			; overflow error

; Subroutine RND function
RND:
C2BDF:		CALL	SIGN			; get sign DAC
		LD	HL,RNDX
		JR	Z,J2C15			; DAC is zero, use current RNDX value
		CALL	M,C2C67			; DAC is negative, RNDX = DAC
		LD	HL,HOLD8+56
		LD	DE,RNDX
		CALL	C2C6A			; HOLD8+56 = RNDX
		LD	HL,I2CF9
		CALL	C2C50			; ARG = 0.21132486540519
		LD	HL,I2CF1
		CALL	C2C5C			; DAC = 0.14389820420821
		LD	DE,HOLD8+56+7
		CALL	C282E			; construct multiply factors
		LD	DE,DAC+8
		LD	HL,RNDX+1
		LD	B,7
		CALL	MOVE1			; copy mantissa to RNDX
		LD	HL,RNDX+0
		LD	(HL),0
J2C15:		CALL	C2C5C			; DAC = RNDX
		LD	HL,DAC+0
		LD	(HL),40H		; in 0-1 range
		XOR	A
		LD	(DAC+8),A		; clear expansion byte DAC
		JP	C26FA			; normalize DAC

; Subroutine initialize RNDX
RNDINI:
C2C24:		LD	DE,I2D01
		LD	HL,RNDX
		JR	C2C6A			; RNDX = 0.40649651372358 and quit

; Subroutine DAC = DAC + operand
; Input:  HL = pointer to operand
C2C2C:		CALL	C2C50			; ARG =
		JP	DECADD			; DAC + ARG

; Subroutine DAC = DAC - operand
; Input:  HL = pointer to operand
C2C32:		CALL	C2C50			; ARG =
		JP	DECSUB			; DAC - ARG

; Subroutine DAC = DAC^2
C2C38:		LD	HL,DAC

; Subroutine DAC = DAC * operand
; Input:  HL = pointer to operand
C2C3B:		CALL	C2C50			; ARG =
		JP	DECMUL			; DAC * ARG and quit

; Subroutine DAC = DAC / operand
; Input:  HL = pointer to operand
; Remark: Unused Code
; Not called from anywhere, leftover from a early Microsoft BASIC
Q2C41:		CALL	C2C50			; ARG =
		JP	DECDIV			; DAC / ARG and quit

; Subroutine compare with DAC
; Input:  HL = pointer to operand
C2C47:		CALL	C2C50			; ARG =
		JP	XDCOMP			; compare double real

; Subroutine MAF (copy DAC to ARG)
MAF:
C2C4D:		LD	HL,DAC

; Subroutine MAM (copy HL to ARG)
MAM:
C2C50:		LD	DE,ARG

; Subroutine MOV8DH (copy HL to DE)
; Input:  HL = source, DE = destination
MOV8DH:
J2C53:		EX	DE,HL
		CALL	C2C6A			; copy double real
		EX	DE,HL
		RET

; Subroutine MFA (copy ARG to DAC)
MFA:
C2C59:		LD	HL,ARG

; Subroutine MFM (copy HL to DAC)
; Input:  HL = source
MFM:
C2C5C:		LD	DE,DAC
		JR	J2C53

; Subroutine initialize RNDX
; Input:  HL = value
; Remark: Unused Code
; Not called from anywhere, leftover from a early Microsoft BASIC
Q2C61:		CALL	CONSIH			; convert integer to single real
		LD	HL,RNDX

; Subroutine MMF (copy DAC to HL)
C2C67:		LD	DE,DAC

; Subroutine MOV8HD (copy double real)
; Input:  DE = source, HL = destination
MOV8HD:
C2C6A:		LD	B,8
		JP	MOVE1			; copy bytes

; Subroutine XTF (exchange DAC with stack)
XTF:
C2C6F:		POP	HL
		LD	(FBUFFR),HL		; store return address
		CALL	C2CDC			; pop ARG
		CALL	C2CCC			; push DAC
		CALL	C2C59			; DAC = ARG
		LD	HL,(FBUFFR)
		JP	(HL)			; continue at return address

; Subroutine start operation (code after call), negate DAC before and after
; Input:  DAC = operand
; Output: DAC = result
C2C80:		CALL	C2E8D			; NEG DAC
		LD	HL,C2E8D
		EX	(SP),HL			; NEG DAC afterwards
		JP	(HL)			; start operation

; Subroutine polynomial approximation odd series
; Input:  HL = pointer to polynomial series
C2C88:		LD	(FBUFFR),HL		; store pointer to polynomial series
		CALL	C2CCC			; push DAC
		LD	HL,(FBUFFR)		; restore pointer to polynomial series
		CALL	C2C9A			; polynomial approximation even series
		CALL	C2CDC			; pop ARG
		JP	DECMUL			; DAC * ARG and quit

; Subroutine polynomial approximation even series
; Input:  HL = pointer to polynomial series
C2C9A:		LD	(FBUFFR),HL		; store pointer to polynomial series
		CALL	C2C38			; DAC = DAC^2
		LD	HL,(FBUFFR)		; restore pointer to polynomial series

; Subroutine polynomial approximation
; Input:  HL = pointer to polynomial series
C2CA3:		LD	A,(HL)
		PUSH	AF			; store length of series
		INC	HL
		PUSH	HL
		LD	HL,FBUFFR
		CALL	C2C67			; FBUFFR = DAC
		POP	HL
		CALL	C2C5C			; DAC = 1st entry of series
J2CB1:		POP	AF
		DEC	A			; end of series ?
		RET	Z			; yep, quit
		PUSH	AF
		PUSH	HL
		LD	HL,FBUFFR
		CALL	C2C3B			; DAC = DAC * FBUFFR
		POP	HL
		CALL	C2C50			; ARG = entry of series
		PUSH	HL
		CALL	DECADD			; DAC + ARG
		POP	HL
		JR	J2CB1			; next

; Subroutine PHA (push ARG on stack)
PHA:
C2CC7:		LD	HL,ARG+7
		JR	J2CCF


; Subroutine PHF (push DAC on stack)
PHF:
C2CCC:		LD	HL,DAC+7
J2CCF:		LD	A,4
		POP	DE
J2CD2:		LD	B,(HL)
		DEC	HL
		LD	C,(HL)
		DEC	HL
		PUSH	BC
		DEC	A
		JR	NZ,J2CD2
		EX	DE,HL
		JP	(HL)

; Subroutine PPA (pop ARG from stack)
PPA:
C2CDC:		LD	HL,ARG
		JR	J2CE4

; Subroutine PPF (pop DAC from stack)
PPF:
C2CE1:		LD	HL,DAC
J2CE4:		LD	A,4
		POP	DE
J2CE7:		POP	BC
		LD	(HL),C
		INC	HL
		LD	(HL),B
		INC	HL
		DEC	A
		JR	NZ,J2CE7
		EX	DE,HL
		JP	(HL)

; rnd #1
I2CF1:		DEFB	000H,014H,038H,098H,020H,042H,008H,021H		; 0.14389820420821

; rnd #2
I2CF9:		DEFB	000H,021H,013H,024H,086H,054H,005H,019H		; 0.21132486540519

; rnd
I2D01:		DEFB	000H,040H,064H,096H,051H,037H,023H,058H		; 0.40649651372358

; log(e)
I2D09:		DEFB	040H,043H,042H,094H,048H,019H,003H,024H		; 0.43429448190324

; 1/2
I2D11:		DEFB	040H,050H					; 0.5

; 0
I2D13:		DEFB	000H,000H,000H,000H,000H,000H,000H,000H		; 0.0

; 1
FONE:
I2D1B:		DEFB	041H,010H,000H,000H,000H,000H,000H,000H		; 1.0

; 1/4
I2D23:		DEFB	040H,025H,000H,000H,000H,000H,000H,000H		; 0.25

; sqr(10)
I2D2B:		DEFB	041H,031H,062H,027H,076H,060H,016H,084H		; 3.1622776601684

; 2^log(e)
I2D33:		DEFB	040H,086H,085H,088H,096H,038H,006H,050H		; 0.86858896380650

; 1/log(e)
I2D3B:		DEFB	041H,023H,002H,058H,050H,092H,099H,040H		; 2.3025850929940

; pi/2
I2D43:		DEFB	041H,015H,070H,079H,063H,026H,079H,049H		; 1.5707963267949

; tan(pi/12)
I2D4B:		DEFB	040H,026H,079H,049H,019H,024H,031H,012H		; 0.26794919243112

; sqr(3)
I2D53:		DEFB	041H,017H,032H,005H,008H,007H,056H,089H		; 1.7320508075689

; pi/6
I2D5B:		DEFB	040H,052H,035H,098H,077H,055H,098H,030H		; 0.52359877559830

; 1/(2*pi)
I2D63:		DEFB	040H,015H,091H,054H,094H,030H,091H,090H		; 0.15915494309190

; exp polynomial series #1
I2D6B:		DEFB	4
		DEFB	041H,010H,000H,000H,000H,000H,000H,000H		; 1.0
		DEFB	043H,015H,093H,074H,015H,023H,060H,031H		; 159.37415236031
		DEFB	044H,027H,009H,031H,069H,040H,085H,016H		; 2709.3169408516
		DEFB	044H,044H,097H,063H,035H,057H,040H,058H		; 4497.6335574058

; exp polynomial series #2
I2D8C:		DEFB	3
		DEFB	042H,018H,031H,023H,060H,015H,092H,075H		; 18.312360159275
		DEFB	043H,083H,014H,006H,072H,012H,093H,071H		; 831.40672129371
		DEFB	044H,051H,078H,009H,019H,091H,051H,062H		; 5178.0919915162

; log polynomial series #1
I2DA5:		DEFB	4
		DEFB	0C0H,071H,043H,033H,082H,015H,032H,026H		; -0.71433382153226
		DEFB	041H,062H,050H,036H,051H,012H,079H,008H		; 6.2503651127908
		DEFB	0C2H,013H,068H,023H,070H,024H,015H,003H		; -13.682370241503
		DEFB	041H,085H,016H,073H,019H,087H,023H,089H		; 8.5167319872389

; log polynomial series #2
I2DC6:		DEFB	5
		DEFB	041H,010H,000H,000H,000H,000H,000H,000H		; 1.0
		DEFB	0C2H,013H,021H,004H,078H,035H,001H,056H		; -13.210478350156
		DEFB	042H,047H,092H,052H,056H,004H,038H,073H		; 47.925256043873
		DEFB	0C2H,064H,090H,066H,082H,074H,009H,043H		; -64.906682740943
		DEFB	042H,029H,041H,057H,050H,017H,023H,023H		; 29.415750172323

; sinus polynomial series
I2DEF:		DEFB	8
		DEFB	0C0H,069H,021H,056H,092H,029H,018H,009H		; -0.69215692291809
		DEFB	041H,038H,017H,028H,086H,038H,057H,071H		; 3.8172886385771
		DEFB	0C2H,015H,009H,044H,099H,047H,048H,001H		; -15.094499474801
		DEFB	042H,042H,005H,086H,089H,066H,073H,055H		; 42.058689667355
		DEFB	0C2H,076H,070H,058H,059H,068H,032H,091H		; -76.705859683291
		DEFB	042H,081H,060H,052H,049H,027H,055H,013H		; 81.605249275513
		DEFB	0C2H,041H,034H,017H,002H,024H,003H,098H		; -41.341702240398
		DEFB	041H,062H,083H,018H,053H,007H,017H,096H		; 6.2831853071796

; atan polynomial series
I2E30:		DEFB	8
		DEFB	0BFH,052H,008H,069H,039H,004H,000H,000H		; -0.05208693904000
		DEFB	03FH,075H,030H,071H,049H,013H,048H,000H		; 0.07530714913480
		DEFB	0BFH,090H,081H,034H,032H,024H,070H,050H		; -0.09081343224705
		DEFB	040H,011H,011H,007H,094H,018H,040H,029H		; 0.11110794184029
		DEFB	0C0H,014H,028H,057H,008H,055H,048H,084H		; -0.14285708554884
		DEFB	040H,019H,099H,099H,099H,094H,089H,067H		; 0.19999999948967
		DEFB	0C0H,033H,033H,033H,033H,033H,031H,060H		; -0.33333333333160
		DEFB	041H,010H,000H,000H,000H,000H,000H,000H		; 1.0

; Subroutine SIGN
; Output: Zx set and A = 000H if zero, Zx reset and Cx set and A = 0FFH if negative, Zx reset and Cx reset and A = 001H if postive
SIGN:
C2E71:		LD	A,(DAC+0)
		OR	A			; DAC is zero ?
		RET	Z			; yep, quit
		DEFB	0FEH			; CP xx, trick to skip the next instruction
J2E77:		CPL
J2E78:		RLA

SIGNS:
J2E79:		SBC	A,A			; negative ?
		RET	NZ			; yep, quit
		INC	A
		RET

; Subroutine DAC zero
	J2E7D:		XOR	A
		LD	(DAC+0),A
		RET

; Subroutine ABS function
ABSFN:
C2E82:		CALL	VSIGN			; get sign DAC
		RET	P			; already postive, quit

; Subroutine negate
VNEG:
C2E86:		RST	R_GETYPR		; get DAC type
		JP	M,J322B			; integer, negate integer in DAC and quit
		JP	Z,TMERR			; string, type mismatch error

; Subroutine NEG (for single and double real)
NEG:
C2E8D:		LD	HL,DAC+0
		LD	A,(HL)
		OR	A			; operand is zero ?
		RET	Z			; yep, quit
		XOR	80H
		LD	(HL),A			; negate sign bit
		RET

; Subroutine SGN function
SGN:
C2E97:		CALL	VSIGN			; get sign DAC

; Subroutine convert signed byte to integer and store in DAC
CONIA:
C2E9A:		LD	L,A
		RLA
		SBC	A,A
		LD	H,A
		JP	MAKINT			; store integer in DAC and quit

; Subroutine get sign DAC
VSIGN:
C2EA1:		RST	R_GETYPR		; get DAC type
		JP	Z,TMERR			; string, type mismatch error
		JP	P,SIGN			; single or double real, get sign DAC and quit
		LD	HL,(DAC+2)		; integer value

; Subroutine get sign of integer
ISIGN:
C2EAB:		LD	A,H
		OR	L			; integer 0 ?
		RET	Z			; yep, quit
		LD	A,H
		JR	J2E78			; sign in A

; Subroutine PUSHF (push DAC on stack, single real)
; Input:  DAC = single real
PUSHF:
C2EB1:		EX	DE,HL
		LD	HL,(DAC+2)
		EX	(SP),HL
		PUSH	HL
		LD	HL,(DAC+0)
		EX	(SP),HL
		PUSH	HL
		EX	DE,HL
		RET

; Subroutine MOVFM (DAC =)
; Input:  HL = address
MOVFM:
C2EBE:		CALL	MOVRM			; load from HL (single)

; Subroutine MOVFR (DAC =)
; Input:  DEBC = single real
MOVFR:
C2EC1:		EX	DE,HL
		LD	(DAC+2),HL
		LD	H,B
		LD	L,C
		LD	(DAC+0),HL
		EX	DE,HL
		RET

; Subroutine MOVRF (= DAC)
; Input:  none
; Output: DEBC = single real
MOVRF:
C2ECC:		LD	HL,(DAC+2)
		EX	DE,HL
		LD	HL,(DAC+0)
		LD	C,L
		LD	B,H
		RET

; Subroutine MOVRMI
; Input:  HL = address
; Output: DEBC = single real
MOVRMI:
C2ED6:		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		RET

; Subroutine MOVRM
; Input:  HL = address
; Output: BCDE =
MOVRM:
C2EDF:		LD	E,(HL)
		INC	HL

; Subroutine get size and address of string
; Input:  HL = pointer to stringdescriptor
; Output: D = size of string, BC = address of string
GETBCD:
C2EE1:		LD	D,(HL)
		INC	HL
		LD	C,(HL)
		INC	HL
		LD	B,(HL)

C2EE6:		INC	HL
		RET

; Subroutine MOVMF (DAC =)
MOVMF:
C2EE8:		LD	DE,DAC

; Subroutine MOVE ( =)
MOVE:
C2EEB:		LD	B,4
		JR	MOVE1			; copy bytes

; Subroutine VMOVAM (ARG =)
VMOVAM:
C2EEF:		LD	DE,ARG

; Subroutine MOVVFM (DE = HL)
MOVVFM:
C2EF2:		EX	DE,HL

; Subroutine VMOVE (copy variable content)
; Input:  HL = source variable pointer, DE = destination variable pointer, (VALTYP) = variabletype
VMOVE:
C2EF3:		LD	A,(VALTYP)
		LD	B,A

; Subroutine copy bytes
; Input:  DE = source, HL = destination, B = number of bytes
MOVE1:
C2EF7:		LD	A,(DE)
		LD	(HL),A
		INC	DE
		INC	HL
		DJNZ	C2EF7
		RET

; Subroutine copy bytes (decrease)
; Input:  DE = source, HL = destination, B = number of bytes
C2EFE:		LD	A,(DE)
		LD	(HL),A
		DEC	DE
		DEC	HL
		DJNZ	C2EFE
		RET

; Subroutine VMOVFA (DAC = ARG)
VMOVFA:
C2F05:		LD	HL,ARG

; Subroutine VMOVFM (DAC = HL)
VMOVFM:
C2F08:		LD	DE,C2EF2		; afterwards DE = HL
		JR	J2F13			; load DAC address

; Subroutine VMOVAF (ARG = DAC)
VMOVAF:
C2F0D:		LD	HL,ARG

; Subroutine VMOVMF (HL = DAC)
VMOVMF:
C2F10:		LD	DE,VMOVE		; afterwards HL = DE
J2F13:		PUSH	DE
		LD	DE,DAC+0
		LD	A,(VALTYP)
		CP	4
		RET	NC
		LD	DE,DAC+2
		RET

; Subroutine FCOMP (single real compare)
; Input:  DEBC = first operand, (DAC) = second operand
FCOMP:
C2F21:		LD	A,C
		OR	A
		JP	Z,SIGN			; get sign DAC and quit
		LD	HL,J2E77
		PUSH	HL
		CALL	SIGN			; get sign DAC
		LD	A,C
		RET	Z			; DAC is zero,
		LD	HL,DAC+0
		XOR	(HL)
		LD	A,C
		RET	M
		CALL	C2F3B
		RRA
		XOR	C
		RET

C2F3B:		LD	A,C
		CP	(HL)
		RET	NZ
		INC	HL
		LD	A,B
		CP	(HL)
		RET	NZ
		INC	HL
		LD	A,E
		CP	(HL)
		RET	NZ
		INC	HL
		LD	A,D
		SUB	(HL)
		RET	NZ
		POP	HL
		POP	HL
		RET

; Subroutine ICOMP (compare integer)
ICOMP:
C2F4D:		LD	A,D
		XOR	H			; sign equal ?
		LD	A,H
		JP	M,J2E78			; nope, sign in A
		CP	D			; high byte equal ?
		JR	NZ,J2F59		; nope,
		LD	A,L
		SUB	E
		RET	Z
J2F59:		JP	SIGNS

; Subroutine XDCOMP (compare double real)
XDCOMP:
C2F5C:		LD	DE,ARG+0
		LD	A,(DE)
		OR	A
		JP	Z,SIGN			; get sign DAC and quit
		LD	HL,J2E77
		PUSH	HL
		CALL	SIGN			; get sign DAC
		LD	A,(DE)
		LD	C,A
		RET	Z			; DAC is zero,
		LD	HL,DAC+0
		XOR	(HL)
		LD	A,C
		RET	M
		LD	B,8
J2F76:		LD	A,(DE)
		SUB	(HL)
		JR	NZ,J2F80
		INC	DE
		INC	HL
D2F7C:		DJNZ	J2F76
		POP	BC
		RET

J2F80:		RRA
		XOR	C
		RET

DCOMP:
C2F83:		CALL	XDCOMP			; compare double real
		JP	NZ,J2E77
		RET

; Subroutine FRCINT (convert DAC to integer), also CINT function
FRCINT:
C2F8A:		RST	R_GETYPR		; get DAC type
		LD	HL,(DAC+2)
		RET	M			; already integer, quit
		JP	Z,TMERR			; string, type mismatch error

; Subroutine convert single real or double real to integer
FDBINT:
C2F92:		CALL	C305D			; convert real to signed integer
		JP	C,OVERR			; overflow error
		EX	DE,HL

; Subroutine store integer in DAC
MAKINT:
C2F99:		LD	(DAC+2),HL


; Subroutine DAC type = integer
VALINT:
C2F9C:		LD	A,2
J2F9E:		LD	(VALTYP),A
		RET

; Subroutine if single real is -32768, convert to integer -32768
; Remark: does funny stuff to the stack when single real is -32768
CONIS2:
C2FA2:		LD	BC,032C5H
		LD	DE,08076H
		CALL	FCOMP			; single real compare
		RET	NZ			; not -32768 SGN, quit
		LD	HL,08000H		; -32768 INT
J2FAF:		POP	DE			; remove return address
		JR	MAKINT			; store integer in DAC and quit

; Subroutine FRCSNG (convert DAC to single real), also CSNG function
FRCSNG:
C2FB2:		RST	R_GETYPR		; get DAC type
		RET	PO			; already single real, quit
		JP	M,C2FC8			; integer, convert integer (DAC) to single real and quit
		JP	Z,TMERR			; string, type mismatch
		CALL	C3053			; DAC type = single real

		CALL	C3752			; get number of BCD digits DAC type
		INC	HL
		LD	A,B
		OR	A			; clear Cx
		RRA
		LD	B,A			; number of bytes DAC type

		; why not LD B,3 LD HL,DAC+4 instead of previous complex code ?

		JP	C2741			; round up

; Subroutine convert integer (DAC) to single real
C2FC8:		LD	HL,(DAC+2)

; Subroutine convert integer to single real
; Input:  HL = integer value
CONSIH:
C2FCB:		LD	A,H
J2FCC:		OR	A
		PUSH	AF			; store sign flag
		CALL	M,C3221			; negative, negate integer and store in DAC
		CALL	C3053			; DAC type = single real
		EX	DE,HL
		LD	HL,0
		LD	(DAC+0),HL
		LD	(DAC+2),HL		; DAC = 0.0
		LD	A,D
		OR	E			; integer is zero ?
		JP	Z,PPSWRT		; yep, clean up stack and quit
		LD	BC,5*256+0		; number of table entries = 5, clear ? flag
		LD	HL,DAC+1
		PUSH	HL			; store pointer in mantissa
		LD	HL,J3030		; table with adders
J2FED:		LD	A,-1			; initialize count
		PUSH	DE			; store integer
		LD	E,(HL)
		INC	HL
		LD	D,(HL)			; get adder
		INC	HL
		EX	(SP),HL			; restore integer, store pointer in table
		PUSH	BC			; store entry counter
J2FF6:		LD	B,H
		LD	C,L
		ADD	HL,DE
		INC	A			; update count
		JR	C,J2FF6			; no underflow, continue
		LD	H,B
		LD	L,C
		POP	BC			; restore entry counter
		POP	DE			; restore pointer in table
		EX	DE,HL
		INC	C
		DEC	C			; ? flag cleared ?
		JR	NZ,J3010		; nope, skip
		OR	A			; count is zero ?
		JR	Z,J3024			; yep,
		PUSH	AF			; store count
		LD	A,40H
		ADD	A,B
		LD	(DAC+0),A
		POP	AF			; restore count
J3010:		INC	C			; update ? flag
		EX	(SP),HL			; restore position in mantissa, store pointer in table
		PUSH	AF			; store count
		LD	A,C
		RRA				; high BCD digit ?
		JR	NC,J301F		; nope,
		POP	AF			; restore count
		ADD	A,A
		ADD	A,A
		ADD	A,A
		ADD	A,A
		LD	(HL),A			; put high BCD digit in mantissa
		JR	J3023

J301F:		POP	AF			; restore count
		OR	(HL)			; combine low BCD digit with high BCD digit
		LD	(HL),A			; update BCD byte in mantissa
		INC	HL			; update position in mantissa
J3023:		EX	(SP),HL			; store position in mantissa, restore pointer in table
J3024:		LD	A,D
		OR	E			; integer is zero ?
		JR	Z,J302A			; yep, quit
		DJNZ	J2FED			; next entry
J302A:		POP	HL
		POP	AF			; restore sign flag
		RET	P			; positive integer, quit
		JP	C2E8D			; NEG DAC

J3030:		DEFW	-10000
		DEFW	-1000
		DEFW	-100
		DEFW	-10
		DEFW	-1

; Subroutine FRCDBL (convert DAC to double real), also CDBL function
FRCDBL:
C303A:		RST	R_GETYPR		; get DAC type
		RET	NC			; already double real, quit
		JP	Z,TMERR			; string, type mismatch error
		CALL	M,C2FC8			; integer, convert integer (DAC) to single real

; Subroutine convert DAC from single real to double real
CONDS:
C3042:		LD	HL,0
		LD	(DAC+4),HL
		LD	(DAC+6),HL
		LD	A,H
		LD	(DAC+8),A		; clear expansion byte DAC

; Subroutine DAC type = double real
C304F:		LD	A,8
		JR	J3055

; Subroutine DAC type = single real
C3053:		LD	A,4
J3055:		JP	J2F9E


; Subroutine check if string
CHKSTR:
C3058:		RST	R_GETYPR		; get DAC type
		RET	Z			; already string, quit
		JP	TMERR			; not a string, type mismatch error

; Subroutine convert real to signed integer
; Input:  DAC = real
; Output: DE = signed integer
C305D:		LD	HL,C30BA
		PUSH	HL
		LD	HL,DAC+0
		LD	A,(HL)
		AND	7FH
		CP	46H
		RET	NC
		SUB	41H
		JR	NC,J3074
		OR	A
		POP	DE
		LD	DE,0
		RET

J3074:		INC	A
		LD	B,A
		LD	DE,0
		LD	C,D
		INC	HL
J307B:		LD	A,C
		INC	C
		RRA
		LD	A,(HL)
		JR	C,J3087
		RRA
		RRA
		RRA
		RRA
		JR	J3088

J3087:		INC	HL
J3088:		AND	0FH
		LD	(DECTMP),HL
		LD	H,D
		LD	L,E
		ADD	HL,HL
		RET	C
		ADD	HL,HL
		RET	C
		ADD	HL,DE
		RET	C
		ADD	HL,HL
		RET	C
		LD	E,A
		LD	D,00H
		ADD	HL,DE
		RET	C
		EX	DE,HL
		LD	HL,(DECTMP)
		DJNZ	J307B
		LD	HL,08000H
		RST	R_DCOMPR
		LD	A,(DAC+0)
		RET	C
		JR	Z,J30B6
		POP	HL
		OR	A
		RET	P
		EX	DE,HL
		CALL	C3221			; negate integer and store in DAC
		EX	DE,HL
		OR	A
		RET

J30B6:		OR	A
		RET	P
		POP	HL
		RET

C30BA:		SCF
		RET

; Subroutine ?
; Unused Code: Not called from anywhere, leftover from a early Microsoft BASIC
Q30BC:		DEC	BC
		RET

; Subroutine FIXER (round DAC), also FIX function
FIXER:
C30BE:		RST	R_GETYPR		; get DAC type
		RET	M			; integer, quit doing nothing
		CALL	SIGN			; get sign DAC
		JP	P,VINT			; DAC is postive, just do a INT
		CALL	C2E8D			; NEG DAC
		CALL	VINT			; INT DAC
		JP	VNEG			; negate

; Subroutine INT function
VINT:
C30CF:		RST	R_GETYPR		; get DAC type
		RET	M			; integer, quit doing nothing

INT:		LD	HL,DAC+7+1
		LD	C,14			; 14 BCD digits
		JR	NC,J30E0		; double real,
		JP	Z,TMERR			; string, type mismatch error
		LD	HL,DAC+3+1
		LD	C,6			; 6 BCD digits
J30E0:		LD	A,(DAC+0)
		OR	A			; negative exponent ?
		JP	M,J3100			; yep,
		AND	7FH			; ?? why, we already known it is positive
		SUB	41H
		JP	C,J2E7D			; DAC = 0 and quit
		INC	A
		SUB	C
		RET	NC
		CPL
		INC	A
		LD	B,A			; number of BCD digits to zero
J30F4:		DEC	HL
		LD	A,(HL)
		AND	0F0H
		LD	(HL),A			; lower BCD digit = 0
		DEC	B
		RET	Z
		XOR	A
		LD	(HL),A			; both BCD digits = 0
		DJNZ	J30F4			; next
		RET

J3100:		AND	7FH			; force exponent positive
		SUB	41H
		JR	NC,J310C
		LD	HL,-1
		JP	MAKINT			; store integer in DAC and quit

J310C:		INC	A
		SUB	C
		RET	NC
		CPL
		INC	A
		LD	B,A			; number of BCD digits
		LD	E,0			; mantissa not updated
J3114:		DEC	HL
		LD	A,(HL)
		LD	D,A
		AND	0F0H
		LD	(HL),A			; lower BCD digit = 0
		CP	D			; was lower BCD digit already zero ?
		JR	Z,J311E			; yep,
		INC	E			; mantissa updated
J311E:		DEC	B			; update BCD count
		JR	Z,J3129			; all BCD digits finished, finish
		XOR	A
		LD	(HL),A			; both BCD digits = 0
		CP	D			; where both BCD digits already zero ?
		JR	Z,J3127			; yep, skip
		INC	E			; mantissa updated
J3127:		DJNZ	J3114			; next
J3129:		INC	E
		DEC	E			; mantissa updated ?
		RET	Z			; nope, quit
		LD	A,C
		CP	6			; single real ?
		LD	BC,10C1H
		LD	DE,0			; single real -1.0
		JP	Z,SGNADD		; yep, single real addition and quit
		EX	DE,HL
		LD	(ARG+6),HL
		LD	(ARG+4),HL
		LD	(ARG+2),HL
		LD	H,B
		LD	L,C
		LD	(ARG+0),HL		; ARG = double real -1.0
		JP	DECADD			; DAC + ARG

; Subroutine UMULT (unsigned integer multiply)
UMULT:
C314A:		PUSH	HL
		LD	HL,0
		LD	A,B
		OR	C
		JR	Z,J3164
		LD	A,16
J3154:		ADD	HL,HL
		JP	C,BSERR			; subscript out of range
		EX	DE,HL
		ADD	HL,HL
		EX	DE,HL
		JR	NC,J3161
		ADD	HL,BC
		JP	C,BSERR			; subscript out of range
J3161:		DEC	A
		JR	NZ,J3154
J3164:		EX	DE,HL
		POP	HL
		RET

; Subroutine ISUB (subtract integer)
; Input:  DE = operand1, HL = operand2
ISUB:
C3167:		LD	A,H
		RLA
		SBC	A,A
		LD	B,A			; sign flag
		CALL	C3221			; negate integer and store in DAC
		LD	A,C			; A = 0
		SBC	A,B			; sign flag
		JR	J3175

; Subroutine IADD (add integer)
; Input:  DE = operand1, HL = operand2
IADD:
C3172:		LD	A,H
		RLA
		SBC	A,A			; sign flag
J3175:		LD	B,A
		PUSH	HL			; store operand2
		LD	A,D
		RLA
		SBC	A,A
		ADD	HL,DE
		ADC	A,B
		RRCA
		XOR	H
		JP	P,J2FAF			; clean up stack, store integer in DAC and quit
		PUSH	BC
		EX	DE,HL
		CALL	CONSIH			; convert integer (operand1) to single real
		POP	AF
		POP	HL			; restore operand2
		CALL	PUSHF			; push DAC (single)
		CALL	CONSIH			; convert integer (operand2) to single real
		POP	BC
		POP	DE			; restore single real from stack
		JP	SGNADD			; single real addition

; Subroutine IMULT (multiply integer)
; Input:  DE = operand1, HL = operand2
IMULT:
C3193:		LD	A,H
		OR	L
		JP	Z,MAKINT		; store integer 0 in DAC and quit
		PUSH	HL			; store operand2
		PUSH	DE			; store operand1
		CALL	C3215			; force postive operands and store operand1 in DAC
		PUSH	BC			; store sign difference flag
		LD	B,H
		LD	C,L			; operand1
		LD	HL,0
		LD	A,16
J31A5:		ADD	HL,HL
		JR	C,J31C7
		EX	DE,HL
		ADD	HL,HL
		EX	DE,HL
		JR	NC,J31B0
		ADD	HL,BC
		JR	C,J31C7
J31B0:		DEC	A
		JR	NZ,J31A5		; next
		POP	BC
		POP	DE
J31B5:		LD	A,H
		OR	A
		JP	M,J31BF
		POP	DE
		LD	A,B
		JP	J321D			; force postive integer and store in DAC (with sign flag)

J31BF:		XOR	80H
		OR	L
		JR	Z,J31D8
		EX	DE,HL
		JR	J31C9

J31C7:		POP	BC
		POP	HL
J31C9:		CALL	CONSIH			; convert integer to single real
		POP	HL
		CALL	PUSHF			; push DAC (single)
		CALL	CONSIH			; convert integer to single real
		POP	BC
		POP	DE			; restore single real from stack
		JP	SGNMUL			; single real multiply

J31D8:		LD	A,B
		OR	A
		POP	BC
		JP	M,MAKINT		; store integer in DAC and quit
		PUSH	DE
		CALL	CONSIH			; convert integer to single real
		POP	DE
		JP	C2E8D			; NEG DAC

; Subroutine IDIV (integer divide)
; Input:  DE = operand1, HL = operand2
IDIV:
C31E6:		LD	A,H
		OR	L			; operand2 is zero ?
		JP	Z,DV0ERR		; yep, division by zero error
		CALL	C3215			; force postive operands and store operand1 in DAC
		PUSH	BC			; store sign difference flag
		EX	DE,HL			; operand2
		CALL	C3221			; negate integer and store in DAC
		LD	B,H
		LD	C,L			;
		LD	HL,0
		LD	A,17
		OR	A			; clear Cx
		JR	J3206

J31FD:		PUSH	HL
		ADD	HL,BC
		JR	NC,J3205
		INC	SP
		INC	SP
		SCF
		DEFB	030H			; JR NC,xx, trick to skip the next instruction
J3205:		POP	HL
J3206:		RL	E
		RL	D
		ADC	HL,HL
		DEC	A
		JR	NZ,J31FD		; next
		EX	DE,HL
		POP	BC			; restore sign difference flag
		PUSH	DE
		JP	J31B5

; Subroutine force postive operands and store operand1 in DAC
; Input:  DE = operand1, HL = operand2
; Output: B = sign difference flag, DAC = operand1
C3215:		LD	A,H
		XOR	D
		LD	B,A			; store sign difference flag
		CALL	C321C			; force postive integer and store in DAC
		EX	DE,HL

; Subroutine force postive integer and store in DAC
; Input:  HL = integer
C321C:		LD	A,H
J321D:		OR	A
		JP	P,MAKINT		; store integer in DAC and quit

; Subroutine negate integer and store in DAC
; Input:  HL = integer
C3221:		XOR	A
		LD	C,A
		SUB	L
		LD	L,A
		LD	A,C
		SBC	A,H
		LD	H,A
		JP	MAKINT			; store integer in DAC and quit

; Subroutine negate integer in DAC
J322B:		LD	HL,(DAC+2)
		CALL	C3221			; negate integer and store in DAC
		LD	A,H
		XOR	80H
		OR	L			; integer = -32768 ?
		RET	NZ			; nope, quit

; Subroutine convert unsigned integer to single real
CONSUI:
C3236:		XOR	A			; sign flag = positive
		JP	J2FCC

; Subroutine IMOD (integer mod)
IMOD:
C323A:		PUSH	DE
		CALL	IDIV			; integer divide
		XOR	A
		ADD	A,D
		RRA
		LD	H,A
		LD	A,E
		RRA
		LD	L,A
		CALL	C2F9C			; DAC type = integer
		POP	AF
		JR	J321D			; force postive integer and store in DAC (with sign flag)

; Subroutine ?
; Unused Code: Not called from anywhere, leftover from a early Microsoft BASIC
Q324B:		CALL	MOVRM			; load from HL (single)

; Subroutine single real addition
SGNADD:
C324E:		CALL	C3280			; store single real as double real in ARG
		CALL	C3042			; convert DAC from single real to double real
		JP	DECADD			; DAC + ARG

; Subroutine single real subtract
SGNSUB:
C3257:		CALL	C2E8D			; NEG DAC
		JR	SGNADD			; single real addition

; Subroutine single real multiply
SGNMUL:
C325C:		CALL	C3280			; store single real as double real in ARG
		CALL	C3042			; convert DAC from single real to double real
		JP	DECMUL			; DAC * ARG and quit

SGNDVT:
J3265:		POP	BC
		POP	DE

; Subroutine single real divide
SGNDIV:
C3267:		LD	HL,(DAC+2)
		EX	DE,HL
		LD	(DAC+2),HL
		PUSH	BC
		LD	HL,(DAC+0)
		EX	(SP),HL
		LD	(DAC+0),HL
		POP	BC
		CALL	C3280			; store single real as double real in ARG
		CALL	C3042			; convert DAC from single real to double real
		JP	DECDIV			; DAC / ARG and quit

; Subroutine store single real as double real in ARG
; Input:  DEBC = single real
C3280:		EX	DE,HL
		LD	(ARG+2),HL
		LD	H,B
		LD	L,C
		LD	(ARG+0),HL
		LD	HL,0
		LD	(ARG+4),HL
		LD	(ARG+6),HL
		RET

; Subroutine decrease A
DCRART:
C3293:		DEC	A
		RET

; Subroutine decrease HL
DCXHRT:
C3295:		DEC	HL
		RET

; Subroutine restore HL from stack and quit (should not be called)
POPHRT:
J3297:		POP	HL
		RET

; Subroutine FIN (convert text to number)
FIN:
C3299:		EX	DE,HL			; store pointer to text
		LD	BC,00FFH		; decimal point count = 0, no decimal point flag
		LD	H,B
		LD	L,B			; result = 0
		CALL	MAKINT			; store integer in DAC
		EX	DE,HL			; restore pointer to text, also exponent (E) = 0, number of digits (D) = 0
		LD	A,(HL)
		CP	'&'
		JP	Z,OCTCNS		; convert text with radix indication to number
		CP	'-'			; minus character ?
		PUSH	AF			; store minus flag
		JR	Z,J32B3			; yep, start FIN
		CP	'+'			; plus character ?
		JR	Z,J32B3			; yep, start FIN
		DEC	HL

J32B3:		RST	R_CHRGTR		; get next BASIC character
		JP	C,J3386			; numeric,
		CP	'.'			; decimal point ?
		JP	Z,J334F			; yep, handle floating point notation
		CP	'e'			; possible exponent ?
		JR	Z,J32C2			; yep,
		CP	'E'			; possible exponent ?
J32C2:		JR	NZ,J32DE		; nope, check if type indicator
		PUSH	HL
		RST	R_CHRGTR		; get next BASIC character
		CP	'l'			; is it a EL(SE) ?
		JR	Z,J32D4			; yep, no exponent
		CP	'L'			; is it a EL(SE) ?
		JR	Z,J32D4			; yep, no exponent
		CP	'q'			; is it a EQ(V) ?
		JR	Z,J32D4			; yep, no exponent
		CP	'Q'			; is it a EQ(V) ?
J32D4:		POP	HL
		JR	Z,J32DD			; no exponent, check if type indicator
		RST	R_GETYPR		; get DAC type
		JR	NC,J32F5		; double real, convert to double real and continue
		XOR	A			; single real
		JR	J32F6			; convert to single real and continue

J32DD:		LD	A,(HL)
J32DE:		CP	'%'			; integer type indicator ?
		JP	Z,J3362			; yep, finish FIN with a convert to integer
		CP	'#'			; double real indicator ?
		JP	Z,J3370			; yep, finish FIN with a convert to double real
		CP	'!'			; single real indicator ?
		JP	Z,J3371			; yep, finish FIN with a convert to single real
		CP	'd'
		JR	Z,J32F5			; yep, convert to double real and continue
		CP	'D'
		JR	NZ,J331E		; nope, finish FIN
J32F5:		OR	A			; double real

;  handle exponent
J32F6:		CALL	C3377			; convert DAC to real
		RST	R_CHRGTR		; get next BASIC character
		PUSH	DE			; store exponent and flag
		LD	D,0			; initialize flag
		CALL	MINPLS			; minus or plus character (including token) ?
		LD	C,D			; store flag
		POP	DE			; restore exponent and flag
J3302:		RST	R_CHRGTR		; get next BASIC character
		JR	NC,J3318		; not numeric,
		LD	A,E
		CP	12			; exponent becomes overflowed ?
		JR	NC,J3314		; yep, make sure it does
		RLCA
		RLCA				; *4
		ADD	A,E			; *5
		RLCA				; *10
		ADD	A,(HL)
		SUB	'0'
		LD	E,A			; update exponent
		JR	J3302			; next exponent digit

J3314:		LD	E,80H			; force exponent to be overflowed
		JR	J3302			; next exponent digit

; exponent is finished
J3318:		INC	C			; minus character ?
		JR	NZ,J331E		; nope, finish FIN
		XOR	A
		SUB	E
		LD	E,A			; negate exponent

FINE:
J331E:		RST	R_GETYPR		; get DAC type
		JP	M,J3334			; integer,
		LD	A,(DAC+0)
		OR	A			; DAC zero ?
		JR	Z,J3334			; yep, skip exponent
		LD	A,D
		SUB	B
		ADD	A,E
		ADD	A,40H
		LD	(DAC+0),A
		OR	A
		CALL	M,C334C			; overflow error (wierd, why a call which never returns? why not JP M,OVERR)
J3334:		POP	AF			; restore sign flag
		PUSH	HL			; store pointer
		CALL	Z,VNEG			; negate
		RST	R_GETYPR		; get DAC type
		JR	NC,J3347		; double real,
		POP	HL
		RET	PE			; no single real, quit
		PUSH	HL			; store pointer
		LD	HL,POPHRT
		PUSH	HL			; restore pointer routine (all this strange stuff because CONIS2 does wierd stuff with stack)
		CALL	CONIS2			; if single real is -32768, convert to integer -32768
		RET

J3347:		CALL	C273C			; round up DAC
		POP	HL			; restore pointer
		RET

; Subroutine overflow error
C334C:		JP	OVERR			; overflow error

; FIN, handle floating point notation
J334F:		RST	R_GETYPR		; get DAC type
		INC	C			; first point character ?
		JR	NZ,J331E		; nope, finish FIN
		JR	NC,J335F		; DAC = real,
		CALL	C3377			; convert DAC to single real
		LD	A,(DAC+0)
		OR	A			; DAC is zero ?
		JR	NZ,J335F		; nope,
		LD	D,A
J335F:		JP	J32B3			; continue FIN

J3362:		RST	R_CHRGTR		; get next BASIC character
		POP	AF			; restore minus flag
		PUSH	HL			; store pointer
		LD	HL,POPHRT
		PUSH	HL			; restore pointer routine
		LD	HL,FRCINT
		PUSH	HL			; convert DAC to integer on stack
		PUSH	AF			; store minus flag
		JR	J331E			; finish FIN

J3370:		OR	A			; double real
J3371:		CALL	C3377			; convert DAC to real
		RST	R_CHRGTR		; get next BASIC character
		JR	J331E			; finish FIN

; Subroutine convert DAC to real
; Input:  Zx set = convert DAC to single real, Zx reset = convert DAC to double real
C3377:		PUSH	HL
		PUSH	DE
		PUSH	BC
		PUSH	AF			; store flag
		CALL	Z,FRCSNG		; convert DAC to single real
		POP	AF			; restore flag
		CALL	NZ,FRCDBL		; convert DAC to double real
		POP	BC
		POP	DE
		POP	HL
		RET

; FIN, handle numeric
J3386:		SUB	'0'			; zero digit ?
		JP	NZ,J3393		; nope,
		OR	C			; decimal point found ?
		JP	Z,J3393			; yep,
		AND	D			; digits other than zero ?
		JP	Z,J32B3			; nope, continue FIN
J3393:		INC	D			; increase number of digits
		LD	A,D
		CP	6+1			; > 6 digits ?
		JR	NZ,J339D		; nope,
		OR	A			; double real
		CALL	C3377			; convert DAC to real
J339D:		PUSH	DE			; store number of digits, store exponent
		LD	A,B
		ADD	A,C
		INC	A
		LD	B,A
		PUSH	BC			; store
		PUSH	HL			; store pointer
		LD	A,(HL)
		SUB	'0'
		PUSH	AF			; store value
		RST	R_GETYPR		; get DAC type
		JP	P,J33D1			; not a integer,
		LD	HL,(DAC+2)
		LD	DE,0+(32767/10)+1
		RST	R_DCOMPR		; can mutiply by 10 without overflow ?
		JR	NC,J33CE		; nope, convert to single real
		LD	D,H
		LD	E,L
		ADD	HL,HL
		ADD	HL,HL			; *4
		ADD	HL,DE			; *5
		ADD	HL,HL			; *10
		POP	AF			; restore value
		LD	C,A
		ADD	HL,BC			; add value
		LD	A,H
		OR	A			; overflow ?
		JP	M,J33CC			; yep, convert to single real
		LD	(DAC+2),HL		; update integer in DAC
J33C6:		POP	HL			; restore pointer
		POP	BC
		POP	DE
		JP	J32B3			; continue FIN

J33CC:		LD	A,C
		PUSH	AF
J33CE:		CALL	C2FC8			; convert integer (DAC) to single real
J33D1:		POP	AF
		POP	HL			; restore pointer
		POP	BC			; restore
		POP	DE
		JR	NZ,J33E3
		LD	A,(DAC+0)
		OR	A			; operand is zero ?
		LD	A,0
		JR	NZ,J33E3		; nope,
		LD	D,A
		JP	J32B3			; continue FIN

J33E3:		PUSH	DE
		PUSH	BC
		PUSH	HL
		PUSH	AF
		LD	HL,DAC+0
		LD	(HL),01H
		LD	A,D
		CP	10H
		JR	C,J33F4
		POP	AF
		JR	J33C6			; restore and continue FIN

J33F4:		INC	A
		OR	A
		RRA
		LD	B,0
		LD	C,A
		ADD	HL,BC
		POP	AF
		LD	C,A
		LD	A,D
		RRA
		LD	A,C
		JR	NC,J3406
		ADD	A,A
		ADD	A,A
		ADD	A,A
		ADD	A,A
J3406:		OR	(HL)
		LD	(HL),A
		JR	J33C6			; restore and continue FIN

; Subroutine " in " number to interpreter output
INPRT:
C340A:		PUSH	HL
		LD	HL,INTXT
		CALL	STROUT			; message to interpreter output
		POP	HL

; Subroutine number to interpreter output
; Input:  HL = number
LINPRT:
C3412:		LD	BC,STROUI
		PUSH	BC			; after this: skip first char, message to interpreter
		CALL	MAKINT			; store integer in DAC
		XOR	A
		LD	(TEMP3),A		; clear format flags
		LD	HL,FBUFFR+1
		LD	(HL),' '
		OR	(HL)			; clear Cx
		JR	J3441

; Subroutine FOUT (convert DAC to text, unformatted)
; Input:  B = number of digits before floating point, C = number of digits after floating point
FOUT:
C3425:		XOR	A			; clear format flags

; Subroutine PUFOUT (convert DAC to text, formatted)
; Input:  A = format flags, B = number of digits before floating point, C = number of digits after floating point
PUFOUT:
C3426:		CALL	C375F			; initialize for DAC convert to text
		AND	08H			; force sign flag ?
		JR	Z,J342F			; nope,
		LD	(HL),'+'
J342F:		EX	DE,HL
		CALL	VSIGN			; get sign DAC
		EX	DE,HL
		JP	P,J3441			; postive, skip
		LD	(HL),'-'
		PUSH	BC
		PUSH	HL
		CALL	VNEG			; negate
		POP	HL
		POP	BC
		OR	H			; clear Cx
J3441:		INC	HL
		LD	(HL),'0'
		LD	A,(TEMP3)		; format flags
		LD	D,A
		RLA
		LD	A,(VALTYP)
		JP	C,J34F7			; formatted output flag,
		JP	Z,J34EF			; no flags,
		CP	4			; real ?
		JP	NC,J34A1		; yep,
		LD	BC,0			; decimal point count = 0, comma count = 0
		CALL	C36DB			; put integer (DAC) in buffer

; Subroutine supress zero's with fill character
FOUTZS:
C345D:		LD	HL,FBUFFR+1
		LD	B,(HL)
		LD	C,' '			; assume fill = space character
		LD	A,(TEMP3)		; format flags
		LD	E,A
		AND	20H			; fill with asterix flag ?
		JR	Z,J3477			; nope,
		LD	A,B
		CP	C			; currently a space character ?
		LD	C,'*'			; fill = asterix character
		JR	NZ,J3477		; nope,
		LD	A,E
		AND	04H			; sign after flag ?
		JR	NZ,J3477		; yep,
		LD	B,C
J3477:		LD	(HL),C			; fill character
		RST	R_CHRGTR		; get next BASIC character
		JR	Z,J348F			; end of statement or line,
		CP	'E'
		JR	Z,J348F
		CP	'D'
		JR	Z,J348F
		CP	'0'			; zero digit ?
		JR	Z,J3477			; yep, next
		CP	','			; thousand seperator ?
		JR	Z,J3477			; yep, next
		CP	'.'			; decimal point ?
		JR	NZ,J3492		; nope,
J348F:		DEC	HL
		LD	(HL),'0'
J3492:		LD	A,E
		AND	10H			; currency before flag ?
		JR	Z,J349A			; nope, skip
		DEC	HL
		LD	(HL),CHRCUR		; currency character
J349A:		LD	A,E
		AND	04H			; sign after flag ?
		RET	NZ			; yep, quit
		DEC	HL
		LD	(HL),B
		RET

; PUFOUT for single or double real
J34A1:		PUSH	HL
		CALL	C3752			; get number of BCD digits DAC type
		LD	D,B
		INC	D
		LD	BC,3*256+0		; assume decimal point count = 3, comma count = 0
		LD	A,(DAC+0)
		SUB	3FH
		JR	C,J34B9
		INC	D
		CP	D
		JR	NC,J34B9
		INC	A
		LD	B,A			; decimal point count
		LD	A,2
J34B9:		SUB	2
		POP	HL
		PUSH	AF
		CALL	C368E			; put comma or point with 0 digits when needed
		LD	(HL),'0'
		CALL	Z,C2EE6
		CALL	C36B3			; put real mantissa in buffer
J34C8:		DEC	HL
		LD	A,(HL)
		CP	'0'
		JR	Z,J34C8
		CP	'.'
		CALL	NZ,C2EE6
		POP	AF
		JR	Z,J34F0			; put string end marker in buffer and quit

; Subroutine put exponent as E notation in buffer
; Input:  A = exponent, HL = pointer in buffer
C34D6:		LD	(HL),'E'
		INC	HL
		LD	(HL),'+'
		JP	P,J34E2
		LD	(HL),'-'
		CPL
		INC	A
J34E2:		LD	B,'0'-1
J34E4:		INC	B
		SUB	10
		JR	NC,J34E4
		ADD	A,'0'+10
		INC	HL
		LD	(HL),B
		INC	HL
		LD	(HL),A
J34EF:		INC	HL
J34F0:		LD	(HL),0			; string end marker
		EX	DE,HL
		LD	HL,FBUFFR+1
		RET

J34F7:		INC	HL
		PUSH	BC
		CP	04H
		LD	A,D
		JP	NC,J3566
		RRA
		JP	C,J35EF
		LD	BC,6*256+3		; assume decimal point count = 6, comma count = 3
		CALL	C3686			; comma count = 0 when thousends seperator flag is reset
		POP	DE
		LD	A,D
		SUB	05H
		CALL	P,C3666
		CALL	C36DB			; put integer (DAC) in buffer
J3513:		LD	A,E
		OR	A
		CALL	Z,DCXHRT		; pointer 1 back
		DEC	A
		CALL	P,C3666
J351C:		PUSH	HL			; store pointer
		CALL	FOUTZS			; supress zero's with fill character
		POP	HL			; restore pointer
		JR	Z,J3525
		LD	(HL),B
		INC	HL
J3525:		LD	(HL),0			; string end marker
		LD	HL,FBUFFR
J352A:		INC	HL
J352B:		LD	A,(TEMP2)		; pointer to decimal point
		SUB	L
		SUB	D
		RET	Z
		LD	A,(HL)
		CP	' '
		JR	Z,J352A
		CP	'*'
		JR	Z,J352A
		DEC	HL
		PUSH	HL
I353C:		PUSH	AF
		LD	BC,I353C
		PUSH	BC
		RST	R_CHRGTR		; get next BASIC character
		CP	'-'
		RET	Z
		CP	'+'
		RET	Z
		CP	CHRCUR			; currency character ?
		RET	Z			; yep, quit
		POP	BC
		CP	'0'
		JR	NZ,J355F
		INC	HL
		RST	R_CHRGTR		; get next BASIC character
		JR	NC,J355F		; not numeric,
		DEC	HL
		JR	J3559

J3557:		DEC	HL
		LD	(HL),A
J3559:		POP	AF
		JR	Z,J3557
		POP	BC
		JR	J352B

J355F:		POP	AF
		JR	Z,J355F
		POP	HL
		LD	(HL),'%'
		RET

J3566:		PUSH	HL
		RRA
		JP	C,J35F5
		CALL	C3752			; get number of BCD digits DAC type
		LD	D,B
		LD	A,(DAC+0)
		SUB	4FH
		JR	C,J3581
		POP	HL
		POP	BC
		CALL	FOUT			; convert DAC to text, unformatted
		LD	HL,FBUFFR
		LD	(HL),'%'
		RET

J3581:		CALL	SIGN			; get sign DAC
		CALL	NZ,C37A2		; DAC not zero,
		POP	HL
		POP	BC
		JP	M,J35A6
		PUSH	BC
		LD	E,A
		LD	A,B
		SUB	D
		SUB	E
		CALL	P,C3666
		CALL	C367A			; calculate decimal point count and comma count
		CALL	C36B3			; put real mantissa in buffer
		OR	E
		CALL	NZ,C3674
		OR	E
		CALL	NZ,C36A0		; put point or comma in buffer when needed
		POP	DE
		JP	J3513

J35A6:		LD	E,A
		LD	A,C
		OR	A			; already zero ?
		CALL	NZ,DCRART		; nope, decrease
		ADD	A,E
		JP	M,J35B1
		XOR	A
J35B1:		PUSH	BC
		PUSH	AF
		CALL	M,C377B
		POP	BC
		LD	A,E
		SUB	B
		POP	BC
		LD	E,A
		ADD	A,D
		LD	A,B
		JP	M,J35CB
		SUB	D
		SUB	E
		CALL	P,C3666
		PUSH	BC
		CALL	C367A			; calculate decimal point count and comma count
		JR	J35DC

J35CB:		CALL	C3666
		LD	A,C
		CALL	C36A3			; put point in buffer, update comma count
		LD	C,A
		XOR	A
		SUB	D
		SUB	E
		CALL	C3666
		PUSH	BC
		LD	B,A
		LD	C,A
J35DC:		CALL	C36B3			; put real mantissa in buffer
		POP	BC
		OR	C
		JR	NZ,J35E6
		LD	HL,(TEMP2)		; pointer to decimal point
J35E6:		ADD	A,E
		DEC	A
		CALL	P,C3666
		LD	D,B
		JP	J351C

J35EF:		PUSH	HL
		PUSH	DE
		CALL	C2FC8			; convert integer (DAC) to single real
		POP	DE
J35F5:		CALL	C3752			; get number of BCD digits DAC type
		LD	E,B
		CALL	SIGN			; get sign DAC
		PUSH	AF
		CALL	NZ,C37A2		; DAC not zero,
		POP	AF
		POP	HL
		POP	BC
		PUSH	AF
		LD	A,C
		OR	A			; already zero ?
		PUSH	AF
		CALL	NZ,DCRART		; nope, decrease
		ADD	A,B
		LD	C,A
		LD	A,D
		AND	04H
		CP	01H
		SBC	A,A
		LD	D,A
		ADD	A,C
		LD	C,A
		SUB	E
		PUSH	AF
		JP	P,J3628
		CALL	C377B
		JR	NZ,J3628
		PUSH	HL
		CALL	C27DB			; shift mantissa 1 digit right
		LD	HL,DAC+0
		INC	(HL)
		POP	HL
J3628:		POP	AF
		PUSH	BC
		PUSH	AF
		JP	M,J362F
		XOR	A
J362F:		CPL
		INC	A
		ADD	A,B
		INC	A
		ADD	A,D
		LD	B,A			; decimal point count
		LD	C,0			; comma count = 0
		CALL	Z,C368E			; put comma or point with 0 digits when needed
		CALL	C36B3			; put real mantissa in buffer
		POP	AF
		CALL	P,C366E
		CALL	C36A0			; put point or comma in buffer when needed
		POP	BC
		POP	AF
		JR	NZ,J3654
		CALL	DCXHRT			; pointer 1 back
		LD	A,(HL)
		CP	'.'
		CALL	NZ,C2EE6
		LD	(TEMP2),HL		; store pointer to decimal point
J3654:		POP	AF
		LD	A,(DAC+0)
		JR	Z,J365D
		ADD	A,E
		SUB	B
		SUB	D
J365D:		PUSH	BC
		CALL	C34D6			; put exponent as E notation in buffer
		EX	DE,HL
		POP	DE
		JP	J351C

C3666:		OR	A
J3667:		RET	Z
		DEC	A
		LD	(HL),'0'
		INC	HL
		JR	J3667

C366E:		JR	NZ,C3674
J3670:		RET	Z
		CALL	C36A0			; put point or comma in buffer when needed

C3674:		LD	(HL),'0'
		INC	HL
		DEC	A
		JR	J3670

; Subroutine calculate decimal point count and comma count
; Output: B = decimal point count, C = comma count
C367A:		LD	A,E
		ADD	A,D
		INC	A
		LD	B,A
		INC	A
J367F:		SUB	3
		JR	NC,J367F
		ADD	A,3+2
		LD	C,A

; Subroutine comma count = 0 when thousends seperator flag is reset
; Input:  C = comma count
; Output: C = updated command count
C3686:		LD	A,(TEMP3)
		AND	40H			; thousends seperator flag ?
		RET	NZ			; yep, quit
		LD	C,A			; C = 0
		RET

; Subroutine put comma or point with 0 digits when needed
; Input:  B = decimal point count, C = comma count
C368E:		DEC	B			; update decimal point count
		JP	P,J36A1
		LD	(TEMP2),HL		; store pointer to decimal point
		LD	(HL),'.'
J3697:		INC	HL
		LD	(HL),'0'
		INC	B
		LD	C,B			; update comma count
		JR	NZ,J3697
		INC	HL
		RET

; Subroutine put point or comma in buffer when needed
; Input:  B = decimal point count, C = comma count
C36A0:		DEC	B			; update decimal point count
J36A1:		JR	NZ,J36AB

; Subroutine put point in buffer, update comma count
; Input:  B = decimal point count, C = comma count
C36A3:		LD	(HL),'.'
		LD	(TEMP2),HL		; store pointer to decimal point
		INC	HL
		LD	C,B			; update comma count
		RET

J36AB:		DEC	C			; update comma count
		RET	NZ			; counter is not zero, quit
		LD	(HL),','
		INC	HL
		LD	C,3			; comma count = 3
		RET

; Subroutine put real mantissa in buffer
C36B3:		PUSH	DE
		PUSH	HL
		PUSH	BC
		CALL	C3752			; get number of BCD digits DAC type
		LD	A,B			; number of BCD digits
		POP	BC
		POP	HL
		LD	DE,DAC+1		; start of mantissa
		SCF				; set high BCD digit flag
J36C0:		PUSH	AF			; store number of BCD digits, BCD digit flag
		CALL	C36A0			; put point or comma in buffer when needed
		LD	A,(DE)			; get BCD byte
		JR	NC,J36CD		; low BCD digit,
		RRA
		RRA
		RRA
		RRA				; shift high BCD digit
		JR	J36CE

J36CD:		INC	DE
J36CE:		AND	0FH
		ADD	A,'0'
		LD	(HL),A
		INC	HL
		POP	AF			; restore number of BCD digits, BCD digit flag
		DEC	A			; update
		CCF				; flip BCD digit flag
		JR	NZ,J36C0		; next digit
		JR	J370A

; Subroutine put integer (DAC) in buffer
; Input:  B = decimal point count, C = comma count
C36DB:		PUSH	DE
		LD	DE,I3710		; table with dividers
		LD	A,5			; number of entries in table
J36E1:		CALL	C36A0			; put point or comma in buffer when needed
		PUSH	BC
		PUSH	AF			; store counter
		PUSH	HL			; store pointer in buffer
		EX	DE,HL
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		PUSH	BC			; store divider
		INC	HL
		EX	(SP),HL			; store pointer in table, restore divider
		EX	DE,HL
		LD	HL,(DAC+2)
		LD	B,'0'-1
J36F4:		INC	B
		LD	A,L
		SUB	E
		LD	L,A
		LD	A,H
		SBC	A,D
		LD	H,A
		JR	NC,J36F4
		ADD	HL,DE
		LD	(DAC+2),HL
		POP	DE			; restore pointer in table
		POP	HL			; restore pointer in buffer
		LD	(HL),B
		INC	HL
		POP	AF			; restore counter
		POP	BC
		DEC	A
		JR	NZ,J36E1		; next entry
J370A:		CALL	C36A0			; put point or comma in buffer when needed
		LD	(HL),A
		POP	DE
		RET

I3710:		DEFW	10000
		DEFW	1000
		DEFW	100
		DEFW	10
		DEFW	1

; Subroutine FOUTB (convert integer to binary text)
; Output: HL = pointer to text buffer
FOUTB:
C371A:		LD	B,1			; bit count = bit
		JR	J3724

; Subroutine FOUTO (convert integer to octal text)
; Output: HL = pointer to text buffer
FOUTO:
C371E:		LD	B,3			; bit count = octal
		JR	J3724

; Subroutine FOUTH (convert integer to hexadecimal text)
; Output: HL = pointer to text buffer
FOUTH:
C3722:		LD	B,4			; bit count = hexdecimal
J3724:		PUSH	BC			; store bit count
		CALL	FRQINT			; convert address to integer
		LD	DE,FBUFFR+1+16
		XOR	A
		LD	(DE),A			; string end marker
		POP	BC			; restore bit count
		LD	C,A			; clear result
J372F:		PUSH	BC			; store bit count
		DEC	DE			; to new position in buffer
J3731:		AND	A
		LD	A,H
		RRA
		LD	H,A
		LD	A,L
		RRA
		LD	L,A
		LD	A,C
		RRA
		LD	C,A			; shift bit in result
		DJNZ	J3731			; next bit
		POP	BC			; restore bit count
		PUSH	BC			; store bit count
J373F:		RLCA
		DJNZ	J373F			; result from higher bits to lower bits
		ADD	A,'0'
		CP	'9'+1
		JR	C,J374A
		ADD	A,7
J374A:		LD	(DE),A
		POP	BC			; restore bit count
		LD	A,L
		OR	H			; remaining bits zero ?
		JR	NZ,J372F		; nope, continue
		EX	DE,HL
		RET

; Subroutine get number of BCD digits DAC type
C3752:		RST	R_GETYPR		; get DAC type
		LD	HL,DAC+7
		LD	B,14			; 14 BCD digits
		RET	NC			; double real, quit
		LD	HL,DAC+3
		LD	B,6			; 6 BCD digits
		RET

; Subroutine initialize for DAC convert to text
C375F:		LD	(TEMP3),A		; store format flags
		PUSH	AF
		PUSH	BC
		PUSH	DE
		CALL	FRCDBL			; convert DAC to double real
		LD	HL,I2D13
		LD	A,(DAC+0)
		AND	A			; operand is zero ?
		CALL	Z,C2C5C			; yep, DAC = 0.0 (but why ? DAC is already zero)
		POP	DE
		POP	BC
		POP	AF
		LD	HL,FBUFFR+1
		LD	(HL),' '
		RET

C377B:		PUSH	HL
		PUSH	DE
		PUSH	BC
		PUSH	AF
		CPL
		INC	A
		LD	E,A
		LD	A,1
		JP	Z,J379C
		CALL	C3752			; get number of BCD digits DAC type
		PUSH	HL			; store end of DAC
J378B:		CALL	C27DB			; shift mantissa 1 digit right
		DEC	E
		JR	NZ,J378B
		POP	HL			; restore end of DAC
		INC	HL
		LD	A,B
		RRCA
		LD	B,A			; number of bytes
		CALL	C2741			; round up
		CALL	C37B4
J379C:		POP	BC
		ADD	A,B
		POP	BC
		POP	DE
		POP	HL
		RET

C37A2:		PUSH	BC
		PUSH	HL
		CALL	C3752			; get number of BCD digits DAC type
		LD	A,(DAC+0)
		SUB	40H
		SUB	B
		LD	(DAC+0),A
		POP	HL
		POP	BC
		OR	A
		RET

C37B4:		PUSH	BC
		CALL	C3752			; get number of BCD digits DAC type
J37B8:		LD	A,(HL)
		AND	0FH			; 2nd BCD digit zero ?
		JR	NZ,J37C5		; nope, quit
		DEC	B
		LD	A,(HL)
		OR	A			; 1st and 2nd BCD digit zero ?
		JR	NZ,J37C5		; nope, quit
		DEC	HL
		DJNZ	J37B8
J37C5:		LD	A,B
		POP	BC
		RET

; Subroutine SGNEXP (single real to the power)
SGNEXP:
C37C8:		CALL	C3280			; store single real as double real in ARG
		CALL	C3042			; convert DAC from single real to double real
		CALL	C2CC7			; push ARG
		CALL	C2C6F			; exchange DAC with stack
		CALL	C2CDC			; pop ARG

; Subroutine DBLEXP (double real to the power)
DBLEXP:
C37D7:		LD	A,(ARG+0)
		OR	A			; 2nd operand is zero ?
		JP	Z,J3843			; yep, store integer 1 in DAC
		LD	H,A			; store sign flag 2nd operand
		LD	A,(DAC+0)
		OR	A			; 1st operand is zero ?
		JP	Z,J384D			; yep,
		CALL	C2CCC			; push DAC
		CALL	C391A			; check if ARG is whole number
		JR	C,J382A			; nope,
		EX	DE,HL
		LD	(TEMP8),HL		; store operand2 as integer (can not use stack)
		CALL	C304F			; DAC type = double real
		CALL	C2CDC			; pop ARG
		CALL	C391A			; check if ARG is whole number
		CALL	C304F			; DAC type = double real
		LD	HL,(TEMP8)		; restore operand2 as integer
		JP	NC,J385A		; yep, use INTEXP
		LD	A,(ARG+0)
		PUSH	AF
		PUSH	HL
		CALL	C2C59			; DAC = ARG
		LD	HL,FBUFFR
		CALL	C2C67			; = DAC
		LD	HL,FONE
		CALL	C2C5C			; DAC = 1.0
		POP	HL
		LD	A,H
		OR	A
		PUSH	AF
		JP	P,J3826
		XOR	A
		LD	C,A
		SUB	L
		LD	L,A
		LD	A,C
		SBC	A,H
		LD	H,A
J3826:		PUSH	HL
		JP	J3894

J382A:		CALL	C304F			; DAC type = double real
		CALL	C2C59			; DAC = ARG
		CALL	C2C6F			; exchange DAC with stack
		CALL	LOG			; LOG DAC
		CALL	C2CDC			; pop ARG
		CALL	DECMUL			; DAC * ARG
		JP	EXP			; EXP DAC

; Subroutine INTEXP (integer to the power)
; Input:  DE = operand1, HL = operand2
INTEXP:
C383F:		LD	A,H
		OR	L			; 2nd operand is zero ?
		JR	NZ,J3849		; nope,
J3843:		LD	HL,1
		JP	J3857			; store integer 1 in DAC and quit

J3849:		LD	A,D
		OR	E			; 1st operand is zero ?
		JR	NZ,J385A		; nope,
J384D:		LD	A,H
		RLA				; operand negative ?
		JR	NC,J3854		; nope, store integer 0 in DAC and quit
		JP	DV0ERR			; division by zero error

J3854:		LD	HL,0
J3857:		JP	MAKINT			; store integer in DAC and quit

J385A:		LD	(TEMP8),HL		; store 2nd operand
		PUSH	DE
		LD	A,H
		OR	A
		PUSH	AF			; store sign flag 2nd operand
		CALL	M,C3221			; 2nd operand negative, negate integer and store in DAC
		LD	B,H
		LD	C,L
		LD	HL,1
J3869:		OR	A
		LD	A,B
		RRA
		LD	B,A
		LD	A,C
		RRA
		LD	C,A
		JR	NC,J3877
		CALL	C390D			; multiply (check if result is integer)
		JR	NZ,J38C3		; result is not an integer,
J3877:		LD	A,B
		OR	C
		JR	Z,J38DE
		PUSH	HL
		LD	H,D
		LD	L,E
		CALL	C390D			; multiply (check if result is integer)
		EX	DE,HL
		POP	HL
		JR	Z,J3869			; result is integer,
		PUSH	BC
		PUSH	HL
		LD	HL,FBUFFR
		CALL	C2C67			; = DAC
		POP	HL
		CALL	CONSIH			; convert integer to single real
		CALL	C3042			; convert DAC from single real to double real
J3894:		POP	BC
		LD	A,B
		OR	A
		RRA
		LD	B,A
		LD	A,C
		RRA
		LD	C,A
		JR	NC,J38A6
		PUSH	BC
		LD	HL,FBUFFR
		CALL	C2C3B			; DAC = DAC * .
		POP	BC
J38A6:		LD	A,B
		OR	C
		JR	Z,J38DE
		PUSH	BC
		CALL	C2CCC			; push DAC
		LD	HL,FBUFFR
		PUSH	HL
		CALL	C2C5C			; DAC =
		POP	HL
		PUSH	HL
		CALL	C2C3B			; DAC = DAC * .
		POP	HL
		CALL	C2C67			; = DAC
		CALL	C2CE1			; pop DAC
		JR	J3894

J38C3:		PUSH	BC
		PUSH	DE
		CALL	FRCDBL			; convert DAC to double real
		CALL	C2C4D			; ARG = DAC
		POP	HL
		CALL	CONSIH			; convert integer to single real
		CALL	C3042			; convert DAC from single real to double real
		LD	HL,FBUFFR
		CALL	C2C67			; = DAC
		CALL	C2C59			; DAC = ARG
		POP	BC
		JR	J38A6

J38DE:		POP	AF
		POP	BC
		RET	P
		LD	A,(VALTYP)
		CP	2
		JR	NZ,J38F0
		PUSH	BC
		CALL	CONSIH			; convert integer to single real
		CALL	C3042			; convert DAC from single real to double real
		POP	BC
J38F0:		LD	A,(DAC+0)
		OR	A
		JR	NZ,J3901
		LD	HL,(TEMP8)
		OR	H
		RET	P
		LD	A,L
		RRCA
		AND	B
		JP	OVERR			; overflow error

J3901:		CALL	C2C4D			; ARG = DAC
		LD	HL,FONE
		CALL	C2C5C			; DAC = 1.0
		JP	DECDIV			; DAC / ARG

; Subroutine multiply (check if result is integer)
; Input:  DE = operand1, HL = operand2
C390D:		PUSH	BC
		PUSH	DE			; store operand1
		CALL	IMULT			; integer multiply
		LD	A,(VALTYP)
		CP	2
		POP	DE			; store operand1
		POP	BC
		RET

; Subroutine check if ARG is whole number
; Output: Cx reset if ARG is whole number, DE = whole number
C391A:		CALL	C2C59			; DAC = ARG
		CALL	C2CC7			; push ARG
		CALL	VINT			; INT DAC
		CALL	C2CDC			; pop ARG
		CALL	XDCOMP			; compare double real
		SCF
		RET	NZ			; not equal, quit
		JP	C305D			; convert real to signed integer

; ------------------------------------------------------------------------------
; BINTRP.MAC
; BASIC interpreter main functions
; ------------------------------------------------------------------------------

		ALIGN	392EH


; BASIC tokens
;
; 00			line end marker
; 01-0A		 unknown
; 0B LLHH		octal number
; 0C LLHH		hexadecimal number
; 0D LLHH		line pointer
; 0E LLHH		line number
; 0F LL		 integer 11-255
; 10			internal token
; 11			integer 0
; 12			integer 1
; 13			integer 2
; 14			integer 3
; 15			integer 4
; 16			integer 5
; 17			integer 6
; 18			integer 7
; 19			integer 8
; 1A			integer 9
; 1B			integer 10
; 1C LLHH		integer
; 1D EEDDDDDD	   single real
; 1E			internal token
; 1F EEDDDDDDDDDDDDDD   double real
; 20-7F		 ASCII chars
; 80			unknown token
; 81-D8		 statement tokens
; D9-ED		 other tokens
; EE			>
; EF			=
; F0			<
; F1			+
; F2			-
; F3			*
; F4			/
; F5			^
; F6			AND
; F7			OR
; F8			XOR
; F9			EQV
; FA			IMP
; FB			MOD
; FC			\
; FD			unknown token, other microsoft basic versions use this to extend token range
; FE			unknown token, other microsoft basic versions use this to extend token range
; FF TT		 function token


; Table addresses of BASIC statement token service routines
; Remark: start with token 081H, ends with token 0D8H

I392E:		DEFW	ENDS			; 81H END
		DEFW	FOR			; 82H FOR
		DEFW	NEXT			; 83H NEXT
		DEFW	DATA			; 84H DATA, skip to end of statement
		DEFW	INPUT			; 85H INPUT
		DEFW	DIM			; 86H DIM
		DEFW	READ			; 87H READ
		DEFW	LET			; 88H LET
		DEFW	GOTO			; 89H GOTO
		DEFW	RUN			; 8AH RUN
		DEFW	IFS			; 8BH IF
		DEFW	RESTOR			; 8CH RESTORE
		DEFW	GOSUB			; 8DH GOSUB
		DEFW	RETURN			; 8EH RETURN
		DEFW	REM			; 8FH REM, skip to end of BASIC line
		DEFW	STOPS			; 90H STOP
		DEFW	PRINT			; 91H PRINT
		DEFW	CLEAR			; 92H CLEAR
		DEFW	LIST			; 93H LIST
		DEFW	SCRATH			; 94H NEW
		DEFW	ONGOTO			; 95H ON
		DEFW	FNWAIT			; 96H WAIT
		DEFW	DEF			; 97H DEF
		DEFW	POKE			; 98H POKE
		DEFW	CONT			; 99H CONT
		DEFW	CSAVE			; 9AH CSAVE (token not used with GW-BASIC)
		DEFW	CLOAD			; 9BH CLOAD (token not used with GW-BASIC)
		DEFW	FNOUT			; 9CH OUT
		DEFW	LPRINT			; 9DH LPRINT
		DEFW	LLIST			; 9EH LLIST
		DEFW	CLS			; 9FH CLS (token not used with GW-BASIC)
		DEFW	WIDTH			; A0H WIDTH
		DEFW	ELSES			; A1H ELSE, skip to end of BASIC line
		DEFW	TON			; A2H TRON
		DEFW	TOFF			; A3H TROFF
		DEFW	SWAP			; A4H SWAP
		DEFW	ERASE			; A5H ERASE
		DEFW	ERRORS			; A6H ERROR
		DEFW	RESUME			; A7H RESUME
		DEFW	DELETE			; A8H DELETE
		DEFW	AUTO			; A9H AUTO
		DEFW	RESEQ			; AAH RENUM
		DEFW	DEFSTR			; ABH DEFSTR
		DEFW	DEFINT			; ACH DEFINT
		DEFW	DEFREA			; ADH DEFSNG
		DEFW	DEFDBL			; AEH DEFDBL
		DEFW	LINE			; AFH LINE

		; from here tokens are incompatible with GW-BASIC

		DEFW	OPEN			; B0H OPEN
		DEFW	FIELD			; B1H FIELD
		DEFW	GETS			; B2H GET
		DEFW	PUTS			; B3H PUT
		DEFW	CLOSE			; B4H CLOSE
		DEFW	LOAD			; B5H LOAD
		DEFW	MERGE			; B6H MERGE
		DEFW	FILES			; B7H FILES
		DEFW	LSET			; B8H LSET
		DEFW	RSET			; B9H RSET
		DEFW	SAVE			; BAH SAVE
		DEFW	LFILES			; BBH LFILES
		DEFW	CIRCLE			; BCH CIRCLE
		DEFW	COLOR			; BDH COLOR
		DEFW	DRAW			; BEH DRAW
		DEFW	PAINT			; BFH PAINT
		DEFW	BEEP			; C0H BEEP
		DEFW	PLAYS			; C1H PLAY
		DEFW	PSET			; C2H PSET
		DEFW	PRESET			; C3H PRESET
		DEFW	SOUND			; C4H SOUND
		DEFW	SCREEN			; C5H SCREEN
		DEFW	VPOKE			; C6H VPOKE
		DEFW	SPRITE			; C7H SPRITE
		DEFW	VDPS			; C8H VDP
		DEFW	BASES			; C9H BASE
		DEFW	CALLS2			; CAH CALL
		DEFW	TIMES			; CBH TIME
		DEFW	KEYS			; CCH KEY
		DEFW	MAXS			; CDH MAX
		DEFW	MOTOR			; CEH MOTOR
		DEFW	BLOAD			; CFH BLOAD
		DEFW	BSAVE			; D0H BSAVE
		DEFW	DSKOS			; D1H DSKO$
		DEFW	SETS			; D2H SET
		DEFW	NAME			; D3H NAME
		DEFW	KILL			; D4H KILL
		DEFW	IPL			; D5H IPL
		DEFW	COPY			; D6H COPY
		DEFW	CMD			; D7H CMD
		DEFW	LOCATE			; D8H LOCATE

; Table addresses of BASIC function token service routines
I39DE:		DEFW	LEFTS			; 81H LEFT$
		DEFW	RIGHTS			; 82H RIGHT$
		DEFW	MIDS1			; 83H MID$
		DEFW	SGN			; 84H SGN
		DEFW	VINT			; 85H INT
		DEFW	ABSFN			; 86H ABS
		DEFW	SQR			; 87H SQR
		DEFW	RND			; 88H RND
		DEFW	SIN			; 89H SIN
		DEFW	LOG			; 8AH LOG
		DEFW	EXP			; 8BH EXP
		DEFW	COS			; 8CH COS
		DEFW	TAN			; 8DH TAN
		DEFW	ATN			; 8EH ATN
		DEFW	FRE			; 8FH FRE
		DEFW	FNINP			; 90H INP
		DEFW	POS			; 91H POS
		DEFW	LEN			; 92H LEN
		DEFW	STRS			; 93H STRS
		DEFW	VAL			; 94H VAL
		DEFW	ASC			; 95H ASC
		DEFW	CHRS			; 96H CHR$
		DEFW	PEEK			; 97H PEEK
		DEFW	VPEEK			; 98H VPEEK
		DEFW	SPACES			; 99H SPACE$
		DEFW	OCTS			; 9AH OCTS
		DEFW	HEXS			; 9BH HEXS
		DEFW	LPOS			; 9CH LPOS
		DEFW	BINS			; 9DH BINS
		DEFW	FRCINT			; 9EH CINT
		DEFW	FRCSNG			; 9FH CSNG
		DEFW	FRCDBL			; A0H CDBL
		DEFW	FIXER			; A1H FIX
		DEFW	STICK			; A2H STICK
		DEFW	TRIG			; A3H TRIG
		DEFW	PDL			; A4H PDL
		DEFW	PAD			; A5H PAD
		DEFW	DSKF			; A6H DSKF
		DEFW	FPOS			; A7H FPOS
		DEFW	CVI			; A8H CVI
		DEFW	CVS			; A9H CVS
		DEFW	CVD			; AAH CVD
		DEFW	EOF			; ABH EOF
		DEFW	LOC			; ACH LOC
		DEFW	LOF			; ADH LOF
		DEFW	MKIS			; AEH MKI$
		DEFW	MKSS			; AFH MKS$
		DEFW	MKDS			; B0H MKD$

; Table pointers to the start of keywords of a given letter
; Remark: start with the pointer for all "A" keywords and ends with "Z" keywords
I3A3E:		DEFW	T3A72
		DEFW	T3A88
		DEFW	T3A9F
		DEFW	T3AF3
		DEFW	T3B2E
		DEFW	T3B4F
		DEFW	T3B69
		DEFW	T3B7B
		DEFW	T3B80
		DEFW	T3B9F
		DEFW	T3BA0
		DEFW	T3BA8
		DEFW	T3BE8
		DEFW	T3C09
		DEFW	T3C18
		DEFW	T3C2B
		DEFW	T3C5D
		DEFW	T3C5E
		DEFW	T3C8E
		DEFW	T3CDB
		DEFW	T3CF6
		DEFW	T3CFF
		DEFW	T3D16
		DEFW	T3D20
		DEFW	T3D24
		DEFW	T3D25 
		
; Keywords starting with 'A'
T3A72:		DEFM	"UT", 'O'|$80, $A9		; AUTO
		DEFM	"N", 'D'|$80, $F6		; AND
		DEFM	"B", 'S'|$80, $06		; ABS
		DEFM	"T", 'N'|$80, $0E		; ATN
		DEFM	"S", 'C'|$80, $15		; ASC
		DEFM	"TTR", '$'|$80, $E9		; ATTR$
		DEFB	0

; Keywords starting with 'B'
T3A88:		DEFM	"AS", 'E'|$80, $C9		; BASE
		DEFM	"SAV", 'E'|$80, $D0		; BSAVE
		DEFM	"LOA", 'D'|$80, $CF		; BLOAD
		DEFM	"EE", 'P'|$80, $C0		; BEEP
		DEFM	"IN", '$'|$80, $1D		; BIN$
		DEFB	0

; Keywords starting with 'C'
T3A9F:		DEFM	"AL", 'L'|$80, $CA		; CALL
		DEFM	"LOS", 'E'|$80, $B4		; CLOSE
		DEFM	"OP", 'Y'|$80, $D6		; COPY
		DEFM	"ON", 'T'|$80, $99		; CONT
		DEFM	"LEA", 'R'|$80, $92		; CLEAR
		DEFM	"LOA", 'D'|$80, $9B		; CLOAD
		DEFM	"SAV", 'E'|$80, $9A		; CSAVE
		DEFM	"SRLI", 'N'|$80, $E8		; CSRLIN
		DEFM	"IN", 'T'|$80, $1E		; CINT
		DEFM	"SN", 'G'|$80, $1F		; CSNG
		DEFM	"DB", 'L'|$80, $20		; CDBL
		DEFM	"V", 'I'|$80, $28		; CVI
		DEFM	"V", 'S'|$80, $29		; CVS
		DEFM	"V", 'D'|$80, $2A		; CVD
		DEFM	"O", 'S'|$80, $0C		; COS
		DEFM	"HR", '$'|$80, $16		; CHR$
		DEFM	"IRCL", 'E'|$80, $BC		; CIRCLE
		DEFM	"OLO", 'R'|$80, $BD		; COLOR
		DEFM	"L", 'S'|$80, $9F		; CLS
		DEFM	"M", 'D'|$80, $D7		; CMD
		DEFB	0

; Keywords starting with 'D'
T3AF3:		DEFM	"ELET", 'E'|$80, $A8		; DELETE
		DEFM	"AT", 'A'|$80, $84		; DATA
		DEFM	"I", 'M'|$80, $86		; DIM
		DEFM	"EFST", 'R'|$80, $AB		; DEFSTR
		DEFM	"EFIN", 'T'|$80, $AC		; DEFINT
		DEFM	"EFSN", 'G'|$80, $AD		; DEFSNG
		DEFM	"EFDB", 'L'|$80, $AE		; DEFDBL
		DEFM	"SKO", '$'|$80, $D1		; DSKO$
		DEFM	"E", 'F'|$80, $97		; DEF
		DEFM	"SKI", '$'|$80, $EA		; DSKI$
		DEFM	"SK", 'F'|$80, $26		; DSKF
		DEFM	"RA", 'W'|$80, $BE		; DRAW
		DEFB	0

; Keywords starting with 'E'
T3B2E:		DEFM	"LS", 'E'|$80, $A1		; ELSE
		DEFM	"N", 'D'|$80, $81		; END
		DEFM	"RAS", 'E'|$80, $A5		; ERASE
		DEFM	"RRO", 'R'|$80, $A6		; ERROR
		DEFM	"R", 'L'|$80, $E1		; ERL
		DEFM	"R", 'R'|$80, $E2		; ERR
		DEFM	"X", 'P'|$80, $0B		; EXP
		DEFM	"O", 'F'|$80, $2B		; EOF
		DEFM	"Q", 'V'|$80, $F9		; EQV
		DEFB	0

; Keywords starting with 'F'
T3B4F:		DEFM	"O", 'R'|$80, $82		; FOR
		DEFM	"IEL", 'D'|$80, $B1		; FIELD
		DEFM	"ILE", 'S'|$80, $B7		; FILES
		DEFM	"", 'N'|$80, $DE			; FN
		DEFM	"R", 'E'|$80, $0F		; FRE
		DEFM	"I", 'X'|$80, $21		; FIX
		DEFM	"PO", 'S'|$80, $27		; FPOS
		DEFB	0

; Keywords starting with 'G'
T3B69:		DEFM	"OT", 'O'|$80, $89		; GOTO
		DEFM	"O T", 'O'|$80, $89		; GO TO
		DEFM	"OSU", 'B'|$80, $8D		; GOSUB
		DEFM	"E", 'T'|$80, $B2		; GET
		DEFB	0

; Keywords starting with 'H'
T3B7B:		DEFM	"EX", '$'|$80, $1B		; HEX$
		DEFB	0

; Keywords starting with 'I'
T3B80:		DEFM	"NPU", 'T'|$80, $85		; INPUT
		DEFM	"", 'F'|$80, $8B			; IF
		DEFM	"NST", 'R'|$80, $E5		; INSTR
		DEFM	"N", 'T'|$80, $05		; INT
		DEFM	"N", 'P'|$80, $10		; INP
		DEFM	"M", 'P'|$80, $FA		; IMP
		DEFM	"NKEY", '$'|$80, $EC		; INKEY$
		DEFM	"P", 'L'|$80, $D5		; IPL
		DEFB	0

; Keywords starting with 'J'
T3B9F:		DEFB	0

; Keywords starting with 'K'
T3BA0:		DEFM	"IL", 'L'|$80, $D4		; KILL
		DEFM	"E", 'Y'|$80, $CC		; KEY
		DEFB	0

; Keywords starting with 'L'
T3BA8:		DEFM	"PRIN", 'T'|$80, $9D		; LPRINT
		DEFM	"LIS", 'T'|$80, $9E		; LLIST
		DEFM	"PO", 'S'|$80, $1C		; LPOS
		DEFM	"E", 'T'|$80, $88		; LET
		DEFM	"OCAT", 'E'|$80, $D8		; LOCATE
		DEFM	"IN", 'E'|$80, $AF		; LINE
		DEFM	"OA", 'D'|$80, $B5		; LOAD
		DEFM	"SE", 'T'|$80, $B8		; LSET
		DEFM	"IS", 'T'|$80, $93		; LIST
		DEFM	"FILE", 'S'|$80, $BB		; LFILES
		DEFM	"O", 'G'|$80, $0A		; LOG
		DEFM	"O", 'C'|$80, $2C		; LOC
		DEFM	"E", 'N'|$80, $12		; LEN
		DEFM	"EFT", '$'|$80, $01		; LEFT$
		DEFM	"O", 'F'|$80, $2D		; LOF
		DEFB	0

; Keywords starting with 'M'
T3BE8:		DEFM	"OTO", 'R'|$80, $CE		; MOTOR
		DEFM	"ERG", 'E'|$80, $B6		; MERGE
		DEFM	"O", 'D'|$80, $FB		; MOD
		DEFM	"KI", '$'|$80, $2E		; MKI$
		DEFM	"KS", '$'|$80, $2F		; MKS$
		DEFM	"KD", '$'|$80, $30		; MKD$
		DEFM	"ID", '$'|$80, $03		; MID$
		DEFM	"A", 'X'|$80, $CD		; MAX
		DEFB	0

; Keywords starting with 'N'
T3C09:		DEFM	"EX", 'T'|$80, $83		; NEXT
		DEFM	"AM", 'E'|$80, $D3		; NAME
		DEFM	"E", 'W'|$80, $94		; NEW
		DEFM	"O", 'T'|$80, $E0		; NOT
		DEFB	0

; Keywords starting with 'O'
T3C18:		DEFM	"PE", 'N'|$80, $B0		; OPEN
		DEFM	"U", 'T'|$80, $9C		; OUT
		DEFM	"", 'N'|$80, $95			; ON
		DEFM	"", 'R'|$80, $F7			; OR
		DEFM	"CT", '$'|$80, $1A		; OCT$
		DEFM	"F", 'F'|$80, $EB		; OFF
		DEFB	0

; Keywords starting with 'P'
T3C2B:		DEFM	"RIN", 'T'|$80, $91		; PRINT
		DEFM	"U", 'T'|$80, $B3		; PUT
		DEFM	"OK", 'E'|$80, $98		; POKE
		DEFM	"O", 'S'|$80, $11		; POS
		DEFM	"EE", 'K'|$80, $17		; PEEK
		DEFM	"SE", 'T'|$80, $C2		; PSET
		DEFM	"RESE", 'T'|$80, $C3		; PRESET
		DEFM	"OIN", 'T'|$80, $ED		; POINT
		DEFM	"AIN", 'T'|$80, $BF		; PAINT
		DEFM	"D", 'L'|$80, $24		; PDL
		DEFM	"A", 'D'|$80, $25		; PAD
		DEFM	"LA", 'Y'|$80, $C1		; PLAY
		DEFB	0

; Keywords starting with 'Q'
T3C5D:		DEFB	0

; Keywords starting with 'R'
T3C5E:		DEFM	"ETUR", 'N'|$80, $8E		; RETURN
		DEFM	"EA", 'D'|$80, $87		; READ
		DEFM	"U", 'N'|$80, $8A		; RUN
		DEFM	"ESTOR", 'E'|$80, $8C		; RESTORE
		DEFM	"E", 'M'|$80, $8F		; REM
		DEFM	"ESUM", 'E'|$80, $A7		; RESUME
		DEFM	"SE", 'T'|$80, $B9		; RSET
		DEFM	"IGHT", '$'|$80, $02		; RIGHT$
		DEFM	"N", 'D'|$80, $08		; RND
		DEFM	"ENU", 'M'|$80, $AA		; RENUM
		DEFB	0

; Keywords starting with 'S'
T3C8E:		DEFM	"CREE", 'N'|$80, $C5		; SCREEN
		DEFM	"PRIT", 'E'|$80, $C7		; SPRITE
		DEFM	"TO", 'P'|$80, $90		; STOP
		DEFM	"WA", 'P'|$80, $A4		; SWAP
		DEFM	"E", 'T'|$80, $D2		; SET
		DEFM	"AV", 'E'|$80, $BA		; SAVE
		DEFM	"PC", '('|$80, $DF		; SPC(
		DEFM	"TE", 'P'|$80, $DC		; STEP
		DEFM	"G", 'N'|$80, $04		; SGN
		DEFM	"Q", 'R'|$80, $07		; SQR
		DEFM	"I", 'N'|$80, $09		; SIN
		DEFM	"TR", '$'|$80, $13		; STR$
		DEFM	"TRING", '$'|$80, $E3		; STRING$
		DEFM	"PACE", '$'|$80, $19		; SPACE$
		DEFM	"OUN", 'D'|$80, $C4		; SOUND
		DEFM	"TIC", 'K'|$80, $22		; STICK
		DEFM	"TRI", 'G'|$80, $23		; STRIG
		DEFB	0

; Keywords starting with 'T'
T3CDB:		DEFM	"HE", 'N'|$80, $DA		; THEN
		DEFM	"RO", 'N'|$80, $A2		; TRON
		DEFM	"ROF", 'F'|$80, $A3		; TROFF
		DEFM	"AB", '('|$80, $DB		; TAB(
		DEFM	"", 'O'|$80, $D9			; TO
		DEFM	"IM", 'E'|$80, $CB		; TIME
		DEFM	"A", 'N'|$80, $0D		; TAN
		DEFB	0

; Keywords starting with 'U'
T3CF6:		DEFM	"SIN", 'G'|$80, $E4		; USING
		DEFM	"S", 'R'|$80, $DD		; USR
		DEFB	0

; Keywords starting with 'V'
T3CFF:		DEFM	"A", 'L'|$80, $14		; VAL
		DEFM	"ARPT", 'R'|$80, $E7		; VARPTR
		DEFM	"D", 'P'|$80, $C8		; VDP
		DEFM	"POK", 'E'|$80, $C6		; VPOKE
		DEFM	"PEE", 'K'|$80, $18		; VPEEK
		DEFB	0

; Keywords starting with 'W'
T3D16:		DEFM	"IDT", 'H'|$80, $A0		; WIDTH
		DEFM	"AI", 'T'|$80, $96		; WAIT
		DEFB	0

; Keywords starting with 'X'
T3D20:		DEFM	"O", 'R'|$80, $F8		; XOR
		DEFB	0

; Keywords starting with 'Y'
T3D24:		DEFB	0

; Keywords starting with 'Z'
T3D25:		DEFB	0

I3D26:		DEFB	'+'|$80,$F1
		DEFB	'-'|$80,$F2
		DEFB	'*'|$80,$F3
		DEFB	'/'|$80,$F4
		DEFB	'^'|$80,$F5
		DEFB	'\\'|$80,$FC
		DEFB	'\''|$80,$E6
		DEFB	'>'|$80,$EE
		DEFB	'='|$80,$EF
		DEFB	'<'|$80,$F0
		DEFB	0

I3D3B:		DEFB	079H			; +
		DEFB	079H			; -
		DEFB	07CH			; *
		DEFB	07CH			; /
		DEFB	07FH			; ^
		DEFB	050H			; AND
		DEFB	046H			; OR
		DEFB	03CH			; XOR
		DEFB	032H			; EQV
		DEFB	028H			; IMP
		DEFB	07AH			; MOD
		DEFB	07BH			; \

I3D47:		DEFW	FRCDBL			; convert DAC to double real
		DEFW	0
		DEFW	FRCINT			; convert DAC to integer
		DEFW	CHKSTR			; check if string (error if not)
		DEFW	FRCSNG			; convert DAC to single real

I3D51:		DEFW	DECADD			; double real addition DECADD
		DEFW	DECSUB			; double real subtract DECSUB
		DEFW	DECMUL			; double real multiply DECMUL
		DEFW	DECDIV			; double real divide DECDIV
		DEFW	DBLEXP			; double real to the power
		DEFW	DCOMP			; double real compare

I3D5D:		DEFW	SGNADD			; single real addition
		DEFW	SGNSUB			; single real subtract
		DEFW	SGNMUL			; single real muliply
		DEFW	SGNDIV			; single real divide
		DEFW	SGNEXP			; single real to the power
		DEFW	FCOMP			; single real compare (FCOMP)

I3D69:		DEFW	IADD			; integer addition
		DEFW	ISUB			; integer subtract
		DEFW	IMULT			; integer multiply
		DEFW	INTDIV			; integer divide
		DEFW	INTEXP			; integer to the power
		DEFW	ICOMP			; integer compare

I3D75:		DEFB	0
		DEFB	"NEXT without FOR",0
		DEFB	"Syntax error",0
		DEFB	"RETURN without GOSUB",0
		DEFB	"Out of DATA",0
		DEFB	"Illegal function call",0
		DEFB	"Overflow",0
		DEFB	"Out of memory",0
		DEFB	"Undefined line number",0
		DEFB	"Subscript out of range",0
		DEFB	"Redimensioned array",0
		DEFB	"Division by zero",0
		DEFB	"Illegal direct",0
		DEFB	"Type mismatch",0
		DEFB	"Out of string space",0
		DEFB	"String too long",0
		DEFB	"String formula too complex",0
		DEFB	"Can't CONTINUE",0
		DEFB	"Undefined user function",0
		DEFB	"Device I/O error",0
		DEFB	"Verify error",0
		DEFB	"No RESUME",0
		DEFB	"RESUME without error",0
		DEFB	"Unprintable error",0
		DEFB	"Missing operand",0
		DEFB	"Line buffer overflow",0
		DEFB	"FIELD overflow",0
		DEFB	"Internal error",0
		DEFB	"Bad file number",0
		DEFB	"File not found",0
		DEFB	"File already open",0
		DEFB	"Input past end",0
		DEFB	"Bad file name",0
		DEFB	"Direct statement in file",0
		DEFB	"Sequential I/O only",0
		DEFB	"File not OPEN",0 

INTXT:
I3FD2:		DEFB	" in "

NULSTR:	DEFB	0

REDDY:
I3FD7:		DEFB	"Ok",13,10,0

BRKTXT:
I3FDC:		DEFB	"Break",0

; Subroutine search FOR block on stack (skip 2 words)
; Input:  DE = address loop variable (0 if any FOR block)
; Output: Zx set if found, Zx reset if other block found first
FNDFOR:
C3FE2:		LD	HL,4			; skip this routine return address and main loop return address
		ADD	HL,SP

; Subroutine search FOR block
C3FE6:		LD	A,(HL)
		INC	HL
		CP	82H			; FOR block ?
		RET	NZ			; nope, quit
		LD	C,(HL)
		INC	HL
		LD	B,(HL)			; address loop variable
		INC	HL
		PUSH	HL
		LD	H,B
		LD	L,C
		LD	A,D
		OR	E			; variable specified ?
		EX	DE,HL
		JR	Z,J3FF9			; nope, found
		EX	DE,HL
		RST	R_DCOMPR		; same variable ?
J3FF9:		LD	BC,25-3
		POP	HL
		RET	Z			; yep, quit
		ADD	HL,BC
		JR	C3FE6			; next block

; Subroutine INP function
FNINP:
C4001:		CALL	FRQINT			; convert address to integer
		LD	B,H
		LD	C,L			; I/O address
		IN	A,(C)			; read I/O port
		JP	SNGFLT			; byte to DAC

; Subroutine evaluate address operand and byte operand seperated by a ","
C400B:		CALL	GETUIN			; evaluate address operand
		PUSH	DE			; store address
		RST	R_SYNCHR
		DEFB	","			; check for ,
		CALL	GETBYT			; evaluate byte operand
		POP	BC			; restore address
		RET

; Subroutine OUT statement
FNOUT:
C4016:		CALL	C400B			; evaluate address operand and byte operand seperated by a ","
		OUT	(C),A
		RET

; Subroutine WAIT statement
FNWAIT:
C401C:		CALL	C400B			; evaluate address operand and byte operand seperated by a ","
		PUSH	BC			; store I/O address
		PUSH	AF			; store AND parameter
		LD	E,0			; assume no XOR parameter
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		JR	Z,J402C			; yep, start wait
		RST	R_SYNCHR
		DEFB	","			; check for ,
		CALL	GETBYT			; evaluate byte operand
J402C:		POP	AF			; restore AND parameter
		LD	D,A			; AND parameter
		POP	BC			; restore I/O address
J402F:		CALL	CKCNTC			; handle CTRL/STOP or STOP pressed, no resume
		IN	A,(C)			; read I/O port
		XOR	E
		AND	D
		JR	Z,J402F
		RET

; Subroutine at end of BASIC program
J4039:		SYSHOOK	H_PRGE			; hook program ends
		LD	HL,(CURLIN)
		LD	A,H
		AND	L
		INC	A			; interpreter in direct mode ?
		JR	Z,J404C			; yep, skip error handling stuff
		LD	A,(ONEFLG)
		OR	A			; in ERROR handling routine ?
		LD	E,21
		JR	NZ,ERROR		; yep, no resume error
J404C:		JP	ENDCON			; END without closing I/O channels, clearing

J404F:		LD	HL,(DATLIN)
		LD	(CURLIN),HL

SNERR:
J4055:		LD	E,2
		DEFB	001H			; LD BC,xxxx, trick to skip next instruction

DV0ERR:
J4058:		LD	E,11
		DEFB	001H			; LD BC,xxxx, trick to skip next instruction

NFERR:
J405B:		LD	E,1
		DEFB	001H			; LD BC,xxxx, trick to skip next instruction

DDERR:
J405E:		LD	E,10
		DEFB	001H			; LD BC,xxxx, trick to skip next instruction

UFERR:
J4061:		LD	E,18
		DEFB	001H			; LD BC,xxxx, trick to skip next instruction

REERR:
J4064:		LD	E,22
		DEFB	001H			; LD BC,xxxx, trick to skip next instruction

OVERR:
J4067:		LD	E,6
		DEFB	001H			; LD BC,xxxx, trick to skip next instruction

MOERR:
J406A:		LD	E,24
		DEFB	001H			; LD BC,xxxx, trick to skip next instruction

TMERR:
J406D:		LD	E,13

; Subroutine BASIC error
; Input:  E = errornumber
ERROR:
J406F:		SYSHOOK	H_ERRO			; hook start of the BASIC error routine
		XOR	A
	IFDEF MSX1
		LD	(NLONLY),A		; reset loading BASIC program, leave I/O channels open flag
	ELSE
		CALL	C7987			; MSX2 patch: stop VDP command
	ENDIF	
		LD	HL,(VLZADR)
		LD	A,H
		OR	L
		JR	Z,J4087
		LD	A,(VLZDAT)
		LD	(HL),A
		LD	HL,0
		LD	(VLZADR),HL
J4087:		EI
		LD	HL,(CURLIN)
		LD	(ERRLIN),HL		; line number at error
		LD	A,H
		AND	L
		INC	A			; error occured in direct mode ?
		JR	Z,J4096
		LD	(DOT),HL		; nope, set DOT
J4096:		LD	BC,I40A4		; continue with error after stack intialize
		JR	J409E

; Subroutine warm start MSX BASIC
READYR:
J409B:		LD	BC,STPRDY		; continue with ok and main loop (+POP)
J409E:		LD	HL,(SAVSTK)		; restore stack pointer
		JP	STKERR			; reinitialize stack

; BASIC error continued

I40A4:		POP	BC
		LD	A,E			; store error code
		LD	C,E			; store error code
		LD	(ERRFLG),A		; update last error code
		LD	HL,(SAVTXT)		; restore BASIC pointer
		LD	(ERRTXT),HL		; update BASIC pointer at error
		EX	DE,HL			; store BASIC pointer at error
		LD	HL,(ERRLIN)
		LD	A,H
		AND	L
		INC	A			; error occured in direct mode ?
		JR	Z,J40C0			; yep, skip updating OLDLIN and OLDTXT
		LD	(OLDLIN),HL		; update line number for CONT
		EX	DE,HL			; restore BASIC pointer at error
		LD	(OLDTXT),HL		; update BASIC pointer for CONT
J40C0:		LD	HL,(ONELIN)
		LD	A,H
		OR	L			; ERROR handler defined ?
		EX	DE,HL			; store ERROR handler pointer
		LD	HL,ONEFLG
		JR	Z,J40D3			; nope, abort
		AND	(HL)			; already in a ERROR handler ?
		JR	NZ,J40D3		; yep, abort
		DEC	(HL)			; set in ERROR handler flag
		EX	DE,HL			; BASIC pointer = ERROR handler pointer
		JP	J4620			; execute statement (execute ERROR handler)

J40D3:		XOR	A
		LD	(HL),A			; not in ERROR handler
		LD	E,C			; errorcode
		CALL	CRDONZ			; fresh line to interpreter output
		LD	HL,I3D75
		SYSHOOK	H_ERRP			; hook error pointer
		LD	A,E
		CP	60			; errorcode 60-255 ?
		JR	NC,J40EC		; yep, use unprintable error string
		CP	50			; errorcode 50-59 ?
		JR	NC,J40EE		; adjust and search errorstring
		CP	26			; errorcode 1-25 ?
		JR	C,J40F1			; yep, search errorstring
J40EC:		LD	A,47			; unprintable error
J40EE:		SUB	24
		LD	E,A
J40F1:		CALL	REM			; skip to end of BASIC line (to search end of errorstring)
		INC	HL
		DEC	E
		JR	NZ,J40F1		; next errorstring
		PUSH	HL
		LD	HL,(ERRLIN)
		EX	(SP),HL			; store line number error

ERRFIN:
J40FD:		SYSHOOK	H_ERRF
		PUSH	HL
		CALL	TOTEXT			; force to text screenmode
		POP	HL
		LD	A,(HL)
		CP	'?'			; errorstring start with a "?"
		JR	NZ,J4110		; nope, print errormessage
		POP	HL			; error line number
		LD	HL,I3D75
		JR	J40EC			; use unprintable error

J4110:		
	IFDEF MSX1
		LD	A,7
		RST	R_OUTDO			; beep to interpreter output
		CALL	STROUT			; message to interpreter output
	ELSE
		CALL    STROUT			; message to interpreter output
		LD	A,7
		RST	R_OUTDO			; beep to interpreter output
	ENDIF
		POP	HL			; error line number
		LD	A,H
		AND	L
		INC	A			; error occured in direct mode ?
		CALL	NZ,INPRT		; nope, "in" number to interpreter output
		DEFB	03EH			; LD A,xx, trick to skip next instruction

; Subroutine ok and main loop (+POP)
STPRDY:
J411E:		POP	BC

; Subroutine ok and main loop
READY:
J411F:		CALL	TOTEXT			; force text screenmode
		CALL	FINLPT			; end printeroutput
		CALL	PRGFIN			; close I/O channel 0 and restore BASIC pointer from (TEMP)
		SYSHOOK	H_READ			; hook prompt ready
	IFDEF MSX1
		CALL	CRDONZ			; fresh line to interpreter output
		LD	HL,REDDY		; "Ok" message
		CALL	STROUT			; message to interpreter output
	ELSE
		CALL    C7BE8			; MSX2 patch: display prompt
		ALIGN	4134H			; keep code aligned
	ENDIF

; Subroutine main loop
MAIN:
J4134:		SYSHOOK	H_MAIN			; hook start main loop
		LD	HL,0FFFFH
		LD	(CURLIN),HL		; interpreter in direct mode
		LD	HL,ENDPRG
		LD	(SAVTXT),HL		; stored BASIC pointer, points to dummy code which ends program
		LD	A,(AUTFLG)
		OR	A			; in auto line number mode ?
		JR	Z,J415F			; nope, skip auto line number
		LD	HL,(AUTLIN)		; current auto line number
		PUSH	HL			; store auto line number
		CALL	LINPRT			; number to interpreter output
		POP	DE			; restore auto line number
		PUSH	DE			; store auto line number
		CALL	FNDLIN			; search line number from start of program
		LD	A,'*'
		JR	C,J415B			; found, existing line indicator
		LD	A,' '			; not found, new line indicator
J415B:		RST	R_OUTDO			; lineindicator to interpreter output
		LD	(AUTFLG),A		; store line exist status
J415F:		CALL	ISFLIO			; is BASIC interpreter I/O redirected to I/O channel ?
		JR	NZ,J4170		; yep, get line from I/O channel
		CALL	PINLIN			; get line from keyboard
		JR	NC,J4173		; not aborted, continue
		XOR	A
		LD	(AUTFLG),A		; quit auto line number mode
		JP	MAIN			; main loop

J4170:		CALL	DSKCHI			; get line from interpreter input file

NTSTOP:
J4173:		RST	R_CHRGTR		; get next BASIC character
		INC	A
		DEC	A			; empty line ?
		JR	Z,MAIN			; yep, restart main loop
		PUSH	AF			; store line has line number
		CALL	LINGET			; collect line number
		JR	NC,J4184		; line number ok,
		CALL	ISFLIO			; is BASIC interpreter I/O redirected to I/O channel ?
		JP	Z,SNERR			; nope, syntax error
J4184:		CALL	C4514			; skip space chars
		LD	A,(AUTFLG)
		OR	A			; in auto line number mode ?
		JR	Z,J4195			; nope, skip check
		CP	'*'			; existing line number ?
		JR	NZ,J4195		; nope,
		CP	(HL)			; yep, is this the "*" char ?
		JR	NZ,J4195
		INC	HL			; yep, skip
J4195:		LD	A,D
		OR	E			; line number = 0 ?
		JR	Z,J419F			; 
		LD	A,(HL)
		CP	' '			; space ?
		JR	NZ,J419F
		INC	HL			; yep, skip
J419F:		PUSH	DE			; store line number
		CALL	C42B2			; encode BASIC line
		POP	DE			; restore line number
		POP	AF			; restore line has line number
		LD	(SAVTXT),HL		; store BASIC pointer = KBFMIN
		SYSHOOK	H_DIRD
		JR	C,J41B4			; line has line number, add line to BASIC program
		XOR	A
		LD	(AUTFLG),A		; quit auto line number mode
		JP	DIRDO			; handle direct statement

J41B4:		PUSH	DE			; store line number
		PUSH	BC			; store size of BASIC line
		RST	R_CHRGTR		; get next BASIC character
		OR	A			; empty line flag (Zx set), no auto line flag
		PUSH	AF			; store empty and auto line flag
		LD	A,(AUTFLG)
		AND	A			; in auto line number mode ?
		JR	Z,J41C2			; nope,
		POP	AF			; restore empty and auto line flag
		SCF				; set auto line flag
		PUSH	AF			; store empty and auto line flag
J41C2:		LD	(DOT),DE		; update last line number
		LD	HL,(AUTINC)
		ADD	HL,DE			; next auto line number = current line number + auto increment value
		JR	C,J41D7			; overflow, end auto line number mode
		PUSH	DE			; store current line number
		LD	DE,65530
		RST	R_DCOMPR		; next auto line number < 65530 ?
		POP	DE			; restore current line number
		LD	(AUTLIN),HL		; update auto line number
		JR	C,J41DB			; yep, continue
J41D7:		XOR	A
		LD	(AUTFLG),A		; quit auto line number mode
J41DB:		CALL	FNDLIN			; search line number from start of program
		JR	C,C41ED			; extact match found,

		; line not found
		POP	AF			; restore empty and auto line flag
		PUSH	AF			; store empty and auto line flag
		JR	NZ,J41EA		; not found + non empty line,
		JP	NC,USERR		; not found + no auto + empty line, undefined line number error

J41E7:		PUSH	BC			; store pointer to BASIC line
		JR	FINI			; exit

; not found + non empty line, add line
J41EA:		OR	A			; line flag = no remove first
		JR	J41F4			; continue

; line found
C41ED:		POP	AF			; restore empty and auto line flag
		PUSH	AF			; store empty and auto line flag
		JR	NZ,J41F3		; found + non empty line, remove first
		JR	C,J41E7			; found + auto + empty line, do nothing

J41F3:		SCF				; line flag = remove first
J41F4:		PUSH	BC			; store pointer to BASIC line
		PUSH	AF			; store remove line first flag
		PUSH	HL			; store pointer to next BASIC line
		CALL	DEPTR			; convert to line pointers to line numbers if needed
		POP	HL			; restore pointer to next BASIC line
		POP	AF			; restore remove line first flag
		POP	BC			; restore pointer to BASIC line
		PUSH	BC			; store pointer to BASIC line
		CALL	C,C5405			; remove line first, remove line
		POP	DE			; restore pointer to BASIC line
		POP	AF			; restore empty and auto line flag
		PUSH	DE			; store pointer to BASIC line
		JR	Z,FINI			; empty line, skip line adding
		POP	DE			; restore pointer to BASIC line
		LD	HL,0
		LD	(ONELIN),HL		; disable ERROR handler
		LD	HL,(VARTAB)
		EX	(SP),HL			; store start of variable area, restore size of BASIC line
		POP	BC			; restore start of variable area
		PUSH	HL			; store size of BASIC line
		ADD	HL,BC			; new start of variable area
		PUSH	HL			; store new start of variable area
		CALL	BLTU			; check for enough stackspace and move data (moves the BASIC variables)
		POP	HL			; restore new start of variable area
		LD	(VARTAB),HL		; update start of variable area
		EX	DE,HL
		LD	(HL),H			; update line link (invalid)
		POP	BC			; restore size of BASIC line
		POP	DE			; restore line number
		PUSH	HL			; store pointer to BASIC line
		INC	HL
		INC	HL			; skip line link
		LD	(HL),E
		INC	HL
		LD	(HL),D			; update line number
		INC	HL
		LD	DE,KBUF
		DEC	BC
		DEC	BC
		DEC	BC
		DEC	BC			; exclude line link and number
J422E:		LD	A,(DE)
		LD	(HL),A
		INC	HL
		INC	DE
		DEC	BC
		LD	A,C
		OR	B
		JR	NZ,J422E		; copy BASIC line

FINI:
J4237:		SYSHOOK	H_FINI
		POP	DE			; restore pointer to BASIC line
	IFDEF MSX1
		CALL	C4257			; setup BASIC line links from this point
	ELSE
		  CALL	C79A1			; MSX2 bugfix for VARTAB
	ENDIF
		LD	HL,(PTRFIL)
		LD	(TEMP2),HL		; store BASIC interpreter I/O channel
		CALL	RUNC			; initialize interpreter, BASIC pointer at start of program
		SYSHOOK	H_FINE
		LD	HL,(TEMP2)
		LD	(PTRFIL),HL		; restore BASIC interpreter I/O channel
		JP	MAIN			; main loop

; Subroutine setup BASIC line links
LINKER:
C4253:		LD	HL,(TXTTAB)
		EX	DE,HL

; Subroutine setup BASIC line links from this point
CHEAD:
C4257:		LD	H,D
		LD	L,E
		LD	A,(HL)
		INC	HL
		OR	(HL)			; end of program ?
		RET	Z			; yep, quit
		INC	HL
		INC	HL			; skip over line number
J425F:		INC	HL
		LD	A,(HL)
J4261:		OR	A			; end of BASIC line ?
		JR	Z,J4272			; yep, fill line link
		CP	20H			; character or non numeric token ?
		JR	NC,J425F		; yep, next
		CP	0BH			; multi byte numeric token ?
		JR	C,J425F			; nope, next
		CALL	C466A			; get BASIC character (numeric token)
		RST	R_CHRGTR		; get next BASIC character (get value)
		JR	J4261

J4272:		INC	HL
		EX	DE,HL
		LD	(HL),E
		INC	HL
		LD	(HL),D			; update line link
		JR	C4257			; next line

; Subroutine evaluate line number (range) and search start line number
C4279:		LD	DE,0			; default start line number = 0
		PUSH	DE
		JR	Z,J4288			; end of statement, skip start
		POP	DE
		CALL	LINSPC			; collect line number (with DOT supported)
		PUSH	DE			; store start line number
		JR	Z,J4291			; end of statement,
		RST	R_SYNCHR
		DEFB	0F2H			; check for -
J4288:		LD	DE,65530		; default end line number = 65530
		CALL	NZ,LINSPC		; not end of statement, collect line number (with DOT supported)
		JP	NZ,SNERR		; not end of statement, syntax error
J4291:		EX	DE,HL
		POP	DE			; start line number

; Subroutine store BASIC pointer and search line number from start of program
; Input:  DE = line number to search
; Output: Zx set if line number found, Cx reset,Zx reset if bigger line number found, BC = start of found line, HL = start of next line
C4293:		EX	(SP),HL			; store BASIC pointer, restore return address
		PUSH	HL			; store return address

; Subroutine search line number from start of program
; Input:  DE = line number to search
; Output: Zx set if line number found, Cx reset,Zx reset if bigger line number found, BC = start of found line, HL = start of next line
FNDLIN:
C4295:		LD	HL,(TXTTAB)		; start of BASIC program

; Subroutine search line number
; Input:  HL = current pointer, DE = line number to search
; Output: Zx set if line number found, Cx reset,Zx reset if bigger line number found, BC = start of found line, HL = start of next line
C4298:		LD	B,H
		LD	C,L			; store start of line
		LD	A,(HL)
		INC	HL
		OR	(HL)			; end of BASIC program ?
		DEC	HL
		RET	Z			; yep, quit
		INC	HL
		INC	HL
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A			; line number
		RST	R_DCOMPR		; compare with the one we search
		LD	H,B
		LD	L,C			; restore start of line
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A			; get line link, start of next line
		CCF
		RET	Z			; exact match, quit (Cx set)
		CCF
		RET	NC			; bigger found, quit (Cx reset)
		JR	C4298			; continue search

; Subroutine encode BASIC line
C42B2:		XOR	A
		LD	(DONUM),A		; normal behavior numeric constant
		LD	(DORES),A		; not in DATA statement
		SYSHOOK	H_CRUN
		LD	BC,315
		LD	DE,KBUF
J42C2:		LD	A,(HL)
		OR	A			; end of line ?
		JR	NZ,J42D9		; nope,
J42C6:		LD	HL,320
		LD	A,L
		SUB	C
		LD	C,A
		LD	A,H
		SBC	A,B
		LD	B,A
		LD	HL,KBFMIN		; statement seperator, before KBUF
		XOR	A
		LD	(DE),A			; end of BASIC line
		INC	DE
		LD	(DE),A
		INC	DE
		LD	(DE),A			; pointer, end of BASIC program
		RET

J42D9:		CP	'"'			; start of string ?
		JP	Z,J4316			; yep, put " in KBUF and all chars that follow until " or end of line reached
		CP	' '			; space ?
		JR	Z,J42E9			; yep, put in KBUF and continue
		LD	A,(DORES)
		OR	A			; in DATA statement ?
		LD	A,(HL)
		JR	Z,J4326			; nope, normal behavior
J42E9:		INC	HL
		PUSH	AF
		CP	01H			; MSX graphic char header ?
		JR	NZ,J42F3		; nope, put in KBUF
		LD	A,(HL)
		AND	A			; end of line ?
		LD	A,01H
J42F3:		CALL	NZ,C44E0		; nope, put in KBUF
		POP	AF
		SUB	':'			; statement seperator ?
		JR	Z,J4301			; yep, not in DATA statement and normal numeric behavior
		CP	84H-':'			; DATA token ?
		JR	NZ,J4307		; nope, skip
		LD	A,1			; yep, set DATA statement flag, numeric to line number
J4301:		LD	(DORES),A		; in DATA statement
		LD	(DONUM),A		; numeric behavior
J4307:		SUB	8FH-':'			; REM token ?
		JR	NZ,J42C2		; nope,
		PUSH	AF			; no special end char
J430C:		LD	A,(HL)
		OR	A			; end of line ?
		EX	(SP),HL
		LD	A,H
		POP	HL
		JR	Z,J42C6			; yep, stop encoding
		CP	(HL)
		JR	Z,J42E9			; yep, put in KBUF and continue
J4316:		PUSH	AF
		LD	A,(HL)
J4318:		INC	HL
		CP	1			; MSX graphic char header ?
		JR	NZ,J4321		; nope, put in KBUF
		LD	A,(HL)
		AND	A			; end of line ?
		LD	A,1
J4321:		CALL	NZ,C44E0		; nope, put in KBUF
		JR	J430C

J4326:		INC	HL
		OR	A			; 80H-0FFH ?
		JP	M,J42C2			; yep, skip
		CP	01H			; MSX graphic char header ?
		JR	NZ,J4336		; nope,
		LD	A,(HL)
		AND	A			; end of line ?
		JR	Z,J42C6			; yep, stop encoding
		INC	HL
		JR	J42C2			; skip MSX graphic char

J4336:		DEC	HL
		CP	'?'			; short for PRINT ?
		LD	A,91H			; PRINT token
		PUSH	DE
		PUSH	BC
		JP	Z,J43A3			; yep,
		LD	A,(HL)
		CP	'_'			; short for CALL ?
		JP	Z,J43A3			; yep,
		LD	DE,I3D26		; ??, useless instruction
		CALL	MAKUPL			; get char uppercase
		CALL	ISLET2			; is upcase letter character ?
		JP	C,J441D			; nope, not a keyword
		PUSH	HL
		SYSHOOK	H_CRUS
		LD	HL,I3A3E
		SUB	'A'
		ADD	A,A
		LD	C,A
		LD	B,0
		ADD	HL,BC
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		POP	HL			; get pointer the keywords which start with the given letter
		INC	HL
J4365:		PUSH	HL
J4366:		CALL	MAKUPL			; get char uppercase
		LD	C,A
		LD	A,(DE)
		AND	7FH			; end of the keyword list ?
		JP	Z,J44EB			; yep, no keyword
		INC	HL
		CP	C			; match ?
		JR	NZ,J4398		; no, next keyword
		LD	A,(DE)
		INC	DE
		OR	A			; end of keyword ?
		JP	P,J4366			; nope, next char
		POP	AF			; remove pointer from stack
		LD	A,(DE)			; token
		SYSHOOK	H_ISRE
		OR	A			; function token ?
		JP	M,J43A2			; nope,
		POP	BC
		POP	DE			; KBUF vars
		OR	80H			; set b7
		PUSH	AF
		LD	A,0FFH			; function token header
		CALL	C44E0			; put in KBUF
		XOR	A
		LD	(DONUM),A		; back to normal numeric behavior
		POP	AF			; function token
		CALL	C44E0			; put in KBUF
		JP	J42C2			; next

J4398:		POP	HL
J4399:		LD	A,(DE)
		INC	DE
		OR	A			; end of keyword ?
		JP	P,J4399			; nope, skip next
		INC	DE
		JR	J4365			; try next keyword

J43A2:		DEC	HL
J43A3:		PUSH	AF
		SYSHOOK	H_NTFN
		LD	DE,I43B5		; table tokens with line number as operand
		LD	C,A
J43AB:		LD	A,(DE)
		OR	A			; end of table ?
		JR	Z,J43C4			; yep, not a line number token
		INC	DE
		CP	C			; match ?
		JR	NZ,J43AB		; nope, next
		JR	J43C6			; line number token

; Table tokens with line number as operand
I43B5:		DEFB	08CH			; RESTORE
		DEFB	0A9H			; AUTO
		DEFB	0AAH			; RENUM
		DEFB	0A8H			; DELETE
		DEFB	0A7H			; RESUME
		DEFB	0E1H			; ERL
		DEFB	0A1H			; ELSE
		DEFB	08AH			; RUN
		DEFB	093H			; LIST
		DEFB	09EH			; LLIST
		DEFB	089H			; GOTO
		DEFB	08EH			; RETURN
		DEFB	0DAH			; THEN
		DEFB	08DH			; GOSUB
		DEFB	0

J43C4:		XOR	A			; normal numeric behavior
		DEFB	0C2H			; JP NZ,xxxx, trick to skip next instruction
J43C6:		LD	A,1			; numeric is line number
J43C8:		LD	(DONUM),A
		POP	AF			; char/token
J43CC:		POP	BC
		POP	DE			; KBUF vars
		CP	0A1H			; ELSE token ?
		PUSH	AF
		CALL	Z,C44DE			; yep, put statement seperator in KBUF
		POP	AF
		CP	0CAH			; CALL token ?
		JR	Z,J43DD			; yep, put in KBUF
		CP	'_'			; short for CALL ?
		JR	NZ,J4406		; nope, other
J43DD:		CALL	NC,C44E0		; yep, put in KBUF
J43E0:		INC	HL
		CALL	MAKUPL			; get char uppercase
		AND	A			; end of line ?
J43E5:		JP	Z,J42C6			; yep, stop encoding
		JP	M,J43E0			; 80H-0FFH, skip
		CP	01H			; MSX graphic char header ?
		JR	NZ,J43F6		; nope,
		INC	HL
		LD	A,(HL)
		AND	A			; end of line ?
		JR	Z,J43E5			; yep, stop encoding
		JR	J43E0			; next

J43F6:		CP	' '			; space char ?
		JR	Z,J43DD			; yep, put in KBUF and continue
		CP	':'			; statement seperator ?
		JR	Z,J443A			; yep, put in KBUF and continue
		CP	'('			; start of a parenthesized operand ?
		JR	Z,J443A			; yep, put in KBUF and continue
		CP	'0'			; 30H-7FH ?
		JR	J43DD			; yep, put in KBUF and continue else skip and continue

J4406:		CP	0E6H			; token for ' ?
		JP	NZ,J44B4		; nope, other
		PUSH	AF
		CALL	C44DE			; put statement seperator in KBUF
		LD	A,8FH			; REM token
		CALL	C44E0			; put in KBUF
		POP	AF
		PUSH	HL
		LD	HL,0
		EX	(SP),HL			; no special end char
		JP	J4318			; put in REM token KBUF and all chars that follow until end of line is reached

J441D:		LD	A,(HL)
		CP	'.'
		JR	Z,J442C
		CP	'9'+1
		JP	NC,J44A2		; not numeric, check for 1 character tokens
		CP	'0'
		JP	C,J44A2			; not numeric, check for 1 character tokens
J442C:		LD	A,(DONUM)
		OR	A
		LD	A,(HL)
		POP	BC
		POP	DE
		JP	M,J42E9			; no numeric conversion, put in KBUF and continue
		JR	Z,J4457			; normal behavior
		CP	'.'
J443A:		JP	Z,J42E9			; put in KBUF and continue
		LD	A,0EH			; line number token
		CALL	C44E0			; put in KBUF
		PUSH	DE
		CALL	LINGET			; collect line number
		CALL	C4514			; skip space chars
J4449:		EX	(SP),HL
		EX	DE,HL
J444B:		LD	A,L
		CALL	C44E0			; put in KBUF
		LD	A,H
J4450:		POP	HL
		CALL	C44E0			; put in KBUF
		JP	J42C2			; next

J4457:		PUSH	DE
		PUSH	BC
		LD	A,(HL)
		CALL	FIN			; convert text to number
		CALL	C4514			; skip space chars
		POP	BC
		POP	DE
		PUSH	HL
		LD	A,(VALTYP)
		CP	2			; integer number ?
		JR	NZ,J447F		; nope, put constant in KBUF
		LD	HL,(DAC+2)
		LD	A,H
		OR	A			; number 0-255 ?
		LD	A,2
		JR	NZ,J447F		; nope, put integer in KBUF
		LD	A,L
		LD	H,L			; number
		LD	L,0FH			; token for numeric byte constant
		CP	10			; number 0-9 ?
		JR	NC,J444B		; nope, put word in KBUF and continue
		ADD	A,11H			; tokens for numeric constant 0-9
		JR	J4450			; put byte in KBUF and continue

J447F:		PUSH	AF
		RRCA
		ADD	A,1BH			; 1CH for integer, 1DH for single real, 1FH for double real
		CALL	C44E0			; put in KBUF
		LD	HL,DAC
		LD	A,(VALTYP)
		CP	2
		JR	NZ,J4493
		LD	HL,DAC+2
J4493:		POP	AF
J4494:		PUSH	AF
		LD	A,(HL)
		CALL	C44E0			; put in KBUF
		POP	AF
		INC	HL
		DEC	A
		JR	NZ,J4494
		POP	HL
		JP	J42C2			; next

J44A2:		LD	DE,I3D26-1		; special 1 character tokens
J44A5:		INC	DE
		LD	A,(DE)
		AND	7FH			; end of table ?
		JP	Z,J44FA			; yep, others
		INC	DE
		CP	(HL)			; match ?
		LD	A,(DE)
		JR	NZ,J44A5		; nope, next
		JP	J4509			; yep,

J44B4:		CP	'&'			; header for other radix ?
		JP	NZ,J42E9		; nope, put in KBUF and continue
		PUSH	HL
		RST	R_CHRGTR		; get next BASIC character (a bit strange but works)
		POP	HL
		CALL	MAKUPS			; upcase char
		CP	'H'			; hexadecimal ?
		JR	Z,J44D0			; yep, put hexadecimal constant in KBUF
		CP	'O'			; octal ?
		JR	Z,J44CC			; yep, put octal constant in KBUF
		LD	A,'&'
		JP	J42E9			; put "&" in KBUF and continue

J44CC:		LD	A,0BH			; token for octal constant
		JR	J44D2

J44D0:		LD	A,0CH			; token for hexadecimal constant
J44D2:		CALL	C44E0			; put in KBUF
		PUSH	DE
		PUSH	BC
		CALL	C4EB8			; convert text with radix indication to number
		POP	BC
		JP	J4449			; put word on stack in KBUF

; Subroutine put statement seperator in KBUF
C44DE:		LD	A,':'

; Subroutine put in KBUF
; Input:  A = data
C44E0:		LD	(DE),A
		INC	DE
		DEC	BC
		LD	A,C
		OR	B
		RET	NZ
		LD	E,25
		JP	ERROR			; line buffer overflow error

J44EB:		SYSHOOK	H_NOTR
		POP	HL
		DEC	HL
		DEC	A
		LD	(DONUM),A		; numeric not converted
		CALL	MAKUPL			; get char uppercase
		JP	J43CC

J44FA:		LD	A,(HL)
		CP	20H			; 20H-7FH ?
		JR	NC,J4509		; yep,
		CP	09H			; TAB ?
		JR	Z,J4509			; yep,
		CP	0AH			; LF ?
		JR	Z,J4509			; yep,
		LD	A,' '			; others are replaced by a space
J4509:		PUSH	AF
		LD	A,(DONUM)
		INC	A			; numeric not converted ?
		JR	Z,J4511			; yep, back to normal numeric behavior
		DEC	A			; numeric is not converted
J4511:		JP	J43C8


; Subroutine skip space chars
C4514:		DEC	HL
		LD	A,(HL)
		CP	' '
		JR	Z,C4514
		CP	09H
		JR	Z,C4514
		CP	0AH
		JR	Z,C4514
		INC	HL
		RET

; Subroutine FOR statement
FOR:
C4524:		LD	A,64H
		LD	(SUBFLG),A		; variable search flag = loop variable
		CALL	LET			; LET statement (initialize loop variable)
		POP	BC
		PUSH	HL
		CALL	DATA			; skip to end of statement
		LD	(ENDFOR),HL
		LD	HL,2
		ADD	HL,SP			; skip 1st word
J4538:		CALL	C3FE6			; search FOR block on stack
		JR	NZ,J4554		; not found, continue
		ADD	HL,BC			; to next block
		PUSH	DE
		DEC	HL
		LD	D,(HL)
		DEC	HL
		LD	E,(HL)			; ENDFOR address
		INC	HL
		INC	HL
		PUSH	HL
		LD	HL,(ENDFOR)
		RST	R_DCOMPR		; same as this ENDFOR ?
		POP	HL
		POP	DE
		JR	NZ,J4538		; nope, search next FOR block
		POP	DE			; restore BASIC pointer
		LD	SP,HL			; remove FOR blocks from stack
		LD	(SAVSTK),HL		; store stack pointer
		DEFB	00EH			; LD C,xx, trick to skip next instruction
J4554:		POP	DE
		EX	DE,HL
		LD	C,12			; words = 12
		CALL	GETSTK			; check if enough stackspace
		PUSH	HL
		LD	HL,(ENDFOR)
		EX	(SP),HL
		PUSH	HL
		LD	HL,(CURLIN)
		EX	(SP),HL
		RST	R_SYNCHR
		DEFB	0D9H			; check for TO
		RST	R_GETYPR		; get DAC type
		JP	Z,TMERR			; string, type mismatch error
		PUSH	AF
		CALL	FRMEVL			; evaluate expression
		POP	AF
		PUSH	HL
		JR	NC,J458B		; loop variable double real,
		JP	P,J45C2			; loop variable single real,
		CALL	FRCINT			; convert DAC to integer
		EX	(SP),HL
		LD	DE,1			; default STEP value is 1
		LD	A,(HL)
		CP	0DCH			; STEP token ?
		CALL	Z,C520E			; yep, skip STEP token and evaluate integer operand
		PUSH	DE
		PUSH	HL
		EX	DE,HL
		CALL	ISIGN			; get sign of integer
		JR	J45E8			; push step value on stack and continue

J458B:		CALL	FRCDBL			; convert DAC to double real
		POP	DE
		LD	HL,-8
		ADD	HL,SP
		LD	SP,HL
		PUSH	DE
		CALL	VMOVMF			; HL = DAC
		POP	HL
		LD	A,(HL)
		CP	0DCH			; STEP token ?
		LD	DE,FONE			; 1.0 double real
		LD	A,1			; sign is positive
		JR	NZ,J45B2		; nope, use 1.0 as STEP value
		RST	R_CHRGTR		; get next BASIC character
		CALL	FRMEVL			; evaluate expression
		PUSH	HL
		CALL	FRCDBL			; convert DAC to double real
		CALL	SIGN			; get sign DAC
		LD	DE,DAC
		POP	HL
J45B2:		LD	B,H
		LD	C,L
		LD	HL,-8
		ADD	HL,SP
		LD	SP,HL
		PUSH	AF
		PUSH	BC
		CALL	VMOVE			; HL = DE (valtyp)
		POP	HL
		POP	AF
		JR	J45EF			; continue

J45C2:		CALL	FRCSNG			; convert DAC to single real
		CALL	MOVRF			; DEBC = DAC (single)
		POP	HL
		PUSH	BC
		PUSH	DE
		LD	BC,1041H
		LD	DE,0000H		; 1.0 single real
		SYSHOOK	H_SNGF
		LD	A,(HL)
		CP	0DCH			; STEP token ?
		LD	A,1			; sign is positive
		JR	NZ,J45E9		; nope, push step value on stack and continue
		CALL	FRMCHK			; skip character and evaluate expression
		PUSH	HL
		CALL	FRCSNG			; convert DAC to single real
		CALL	MOVRF			; DEBC = DAC (single real)
		CALL	SIGN			; get sign DAC
J45E8:		POP	HL
J45E9:		PUSH	DE
		PUSH	BC
		PUSH	BC
		PUSH	BC
		PUSH	BC
		PUSH	BC
J45EF:		OR	A
		JR	NZ,J45F4
		LD	A,2
J45F4:		LD	C,A
		RST	R_GETYPR		; get DAC type
		LD	B,A			;
		PUSH	BC
		PUSH	HL
		LD	HL,(TEMP)
		EX	(SP),HL

NXTCON:
J45FD:		LD	B,82H
		PUSH	BC
		INC	SP

; Subroutine execute new statement
NEWSTT:
C4601:		SYSHOOK	H_NEWS
		LD	(SAVSTK),SP		; store stack pointer
		CALL	ISCNTC			; handle CTRL/STOP or STOP pressed
		LD	A,(ONGSBF)
		OR	A			; trap occured ?
		CALL	NZ,GOTRP		; yep, handle trap

NEWSTN:
J4612:		EI
		LD	(SAVTXT),HL		; store BASIC pointer (at start of new statement)
		LD	A,(HL)
		CP	':'
		JR	Z,GONE			; statement seperator, skip new line stuff
		OR	A
		JP	NZ,SNERR		; spurious text after statement, syntax error
		INC	HL
J4620:		LD	A,(HL)
		INC	HL
		OR	(HL)			; end of BASIC program ?
		JP	Z,J4039			; yep, at end of BASIC program
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)			; line number
		EX	DE,HL			; store BASIC pointer
		LD	(CURLIN),HL		; update current line number
		LD	A,(TRCFLG)
		OR	A			; trace mode ?
		JR	Z,J463F			; skip trace
		PUSH	DE			; store BASIC pointer
		LD	A,'['
		RST	R_OUTDO			; "[" to interpreter output
		CALL	LINPRT			; number to interpreter output
		LD	A,']'
		RST	R_OUTDO			; "]" to interpreter output
		POP	DE			; restore BASIC pointer
J463F:		EX	DE,HL			; restore BASIC pointer
GONE:
J4640:		RST	R_CHRGTR		; get next BASIC character
		LD	DE,NEWSTT
		PUSH	DE
		RET	Z			; end of BASIC line, execute new statement
J4646:		SYSHOOK	H_GONE
		CP	'_'			; CALL ?
		JP	Z,CALLS1		; yep, execute CALL statement
		SUB	81H			; statement token ? (081H-0D8H)
		JP	C,LET			; nope, LET statement
		CP	0D9H-081H		; valid statement token ?
		JP	NC,J51AD		; nope, check if function token allowed as statement
		RLCA
		LD	C,A
		LD	B,0
		EX	DE,HL
		LD	HL,I392E
		ADD	HL,BC
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		PUSH	BC			; execute statement after this
		EX	DE,HL

; Subroutine CHRGTR (get next BASIC char)
; Output: Zx set if end of statement, Cx set if numeric
CHRGTR:
C4666:		SYSHOOK	H_CHRG
		INC	HL

; Subroutine get BASIC char
CHRGT2:
C466A:		LD	A,(HL)
		CP	3AH			; 3AH-FFH, quit with Cx reset. Zx set if statement seperator
		RET	NC
		CP	' '			; SPACE ?
		JR	Z,CHRGTR		; yep, skip
		JR	NC,J46E0		; 21H-39H,
		OR	A			; end of BASIC line ?
		RET	Z			; yep, quit with Cx reset, Zx set
		CP	0BH			; 01H-0AH ?
		JR	C,J46DB
		CP	1EH			; 1EH token ?
		JR	NZ,J4683		; nope (0BH-1FH),
		LD	A,(CONSAV)
		OR	A			; yep, original numeric token
		RET

J4683:		CP	10H			; 10H token ?
		JR	Z,J46BB			; yep, return to original BASIC pointer
		PUSH	AF			; store numeric token
		INC	HL
		LD	(CONSAV),A		; store token
		SUB	1CH			; 1CH, 1DH or 1FH token ?
		JR	NC,J46C0		; yep, numeric constant
		SUB	11H-1CH			; 11H-1BH token ?
		JR	NC,J469A		; yep, numeric constant 0-10
		CP	0FEH			; 0FH token ?
		JR	NZ,J46AE		; nope, token 0BH,0CH,0DH or 0EH, numeric word constant
		LD	A,(HL)			; yep, get byte constant
		INC	HL
J469A:		LD	(CONTXT),HL		; where BASIC pointer continues
		LD	H,0			; clear highbyte
J469F:		LD	L,A
		LD	(CONLO),HL		; store constant
		LD	A,2
		LD	(CONTYP),A		; integer type
		LD	HL,I46E6		; special internal token sequence
		POP	AF			; restore numeric token
		OR	A			; Cx reset, Zx reset
		RET

J46AE:		LD	A,(HL)			; get lowbyte constant
		INC	HL
		INC	HL
		LD	(CONTXT),HL		; where BASIC pointer continues
		DEC	HL
		LD	H,(HL)			; get highbyte constant
		JR	J469F			; store constant

J46B8:		CALL	C46E8			; get numeric constant (in DAC)
J46BB:		LD	HL,(CONTXT)		; restore BASIC pointer
		JR	C466A			; get BASIC character

J46C0:		INC	A			; 1,2 or 4
		RLCA				; 2,4 or 8
		LD	(CONTYP),A		; type
		PUSH	DE
		PUSH	BC
		LD	DE,CONLO
		EX	DE,HL
		LD	B,A
		CALL	MOVE1
		EX	DE,HL
		POP	BC
		POP	DE			; copy to CONLO
		LD	(CONTXT),HL		; where BASIC pointer continues
		POP	AF
		LD	HL,I46E6		; special internal token sequence
		OR	A			; Cx reset, Zx reset
		RET

J46DB:		CP	09H			; 09H or 0AH ?
		JP	NC,CHRGTR		; yep, skip
J46E0:		CP	'0'
		CCF				; Cx set if digit
		INC	A
		DEC	A			; Zx reset
		RET

I46E6:		DEFB	01EH			; internal token for returning the original numeric token
		DEFB	010H			; resume BASIC pointer

; Subroutine get numeric constant (in DAC)
C46E8:		LD	A,(CONSAV)
		CP	0FH			; numeric tokens 0FH,11H-1BH,1AH,1BH,1CH,1DH,1FH ?
		JR	NC,J4702		; yep,
		CP	0DH			; numeric tokens 0BH,0CH ?
		JR	C,J4702			; yep,
		LD	HL,(CONLO)		; line number/line pointer
		JR	NZ,J46FF		; numeric token 0EH, line number
		INC	HL
		INC	HL
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		EX	DE,HL			; line number

FLTLIN:
J46FF:		JP	CONSUI			; convert unsigned integer to single real

J4702:		LD	A,(CONTYP)
		LD	(VALTYP),A
		CP	2
		JR	NZ,J4712
		LD	HL,(CONLO)
		LD	(DAC+2),HL
J4712:		LD	HL,CONLO
		JP	VMOVFM			; DAC = CONLO

; Subroutine DEFSTR statement
DEFSTR:
C4718:		LD	E,3			; string
		DEFB	01H			; LD BC,xxxx, trick to skip next instruction

; Subroutine DEFINT statement
DEFINT:
C471B:		LD	E,2			; integer
		DEFB	01H			; LD BC,xxxx, trick to skip next instruction

; Subroutine DEFSNG statement
DEFREA:
C471E:		LD	E,4			; single real
		DEFB	01H			; LD BC,xxxx, trick to skip next instruction

; Subroutine DEFDBL statement
DEFDBL:
C4721:		LD	E,8			; double real
J4723:		CALL	ISLET			; is current BASIC character a upcase letter ?
		LD	BC,SNERR
		PUSH	BC
		RET	C			; nope, syntax error
		SUB	'A'
		LD	C,A
		LD	B,A
		RST	R_CHRGTR		; get next BASIC character (upcase letter)
		CP	0F2H			; - token ?
		JR	NZ,J473D		; nope,
		RST	R_CHRGTR		; get next BASIC character (- token)
		CALL	ISLET			; is current BASIC character a upcase letter ?
		RET	C			; nope, syntax error
		SUB	'A'
		LD	B,A
		RST	R_CHRGTR		; get next BASIC character (upcase letter)
J473D:		LD	A,B
		SUB	C			; postive range ?
		RET	C			; nope, syntax error
		INC	A
		EX	(SP),HL			; store BASIC pointer (and remove syntax error address)
		LD	HL,DEFTBL
		LD	B,0
		ADD	HL,BC			; to start letter
J4748:		LD	(HL),E			; default variabele type
		INC	HL
		DEC	A
		JR	NZ,J4748		; next
		POP	HL			; restore BASIC pointer
		LD	A,(HL)
		CP	','			; an other range follows ?
		RET	NZ			; nope, quit
		RST	R_CHRGTR		; get next BASIC character (, char)
		JR	J4723			; next range


; Subroutine skip basic char, evaluate word operand and check for 0-32767 range
INTIDX:
C4755:		RST	R_CHRGTR		; get next BASIC character

; Subroutine evaluate word operand and check for 0-32767 range
INTID2:
C4756:		CALL	GETIN2			; evaluate integer operand
		RET	P

; Subroutine illegal function call
FCERR:
C475A:		LD	E,5
		JP	ERROR			; illegal function call

; Subroutine collect line number (with DOT supported)
LINSPC:
C475F:		LD	A,(HL)
		CP	'.'
		LD	DE,(DOT)
		JP	Z,CHRGTR		; get next BASIC character and quit

; Subroutine collect line number
LINGET:
C4769:		DEC	HL

; Subroutine collect line number
LINGT2:
C476A:		RST	R_CHRGTR		; get next BASIC character
		CP	0EH			; line number token ?
		JR	Z,C4771			; yep,
		CP	0DH			; line pointer token ?

; Subroutine get line number or line pointer
; Output: DE = line number or line pointer
LINGT3:
C4771:		LD	DE,(CONLO)
		JP	Z,CHRGTR		; yep, get next BASIC character and quit
		XOR	A
		LD	(CONSAV),A
		LD	DE,0
		DEC	HL
J4780:		RST	R_CHRGTR		; get next BASIC character
		RET	NC			; no digit, quit
		PUSH	HL			; store BASIC pointer
		PUSH	AF			; store digit
		LD	HL,6552			; 65530/10-1
		RST	R_DCOMPR		; line number will fit ?
		JR	C,J479B			; nope, quit
		LD	H,D
		LD	L,E
		ADD	HL,DE			; *2
		ADD	HL,HL			; *4
		ADD	HL,DE			; *5
		ADD	HL,HL			; *10
		POP	AF			; restore digit
		SUB	'0'			; from ASCII to decimal
		LD	E,A
		LD	D,0
		ADD	HL,DE
		EX	DE,HL
		POP	HL			; restore BASIC pointer
		JR	J4780			; next digit

J479B:		POP	AF			; restore digit
		POP	HL			; restore BASIC pointer
		RET

; Subroutine RUN statement
RUN:
C479E:		JP	Z,RUNC			; end of statement, initialize interpreter, basic pointer at start of program and quit (which start the program!)
		CP	0EH			; line number token ?
		JR	Z,J47AA			; yep, RUN line
		CP	0DH			; line pointer token ?
		JP	NZ,LRUN			; nope, RUN file
J47AA:		CALL	CLEARC			; initialize interpreter
		LD	BC,NEWSTT
		JR	J47E7			; execute GOTO statement, after that execute new statement

; Subroutine GOSUB statement
GOSUB:
C47B2:		LD	C,3			; words = 3
		CALL	GETSTK			; check if enough stackspace
		CALL	LINGET			; collect line number
		POP	BC
		PUSH	HL			; store BASIC pointer
		PUSH	HL
		LD	HL,(CURLIN)
		EX	(SP),HL			; store current line number
		LD	BC,0
		PUSH	BC			; 0, not a trapentry
		LD	BC,NEWSTT
		LD	A,8DH
		PUSH	AF
		INC	SP			; GOSUB parameter block
		PUSH	BC			; after this, execute new statement
		JR	C47EB			; goto line number

; Subroutine GOSUB trap handler
; Input:  HL = BASIC pointer, DE = pointer to trap handler, BC = pointer to trap entry
GOSUBT:
J47CF:		PUSH	HL			; store BASIC pointer
		PUSH	HL			; store BASIC pointer
		LD	HL,(CURLIN)
		EX	(SP),HL			; store current line number, restore BASIC pointer
		PUSH	BC			; store pointer to trap entry
		LD	A,8DH
		PUSH	AF
		INC	SP			; GOSUB parameter block
		EX	DE,HL
		DEC	HL
		LD	(SAVTXT),HL		; store BASIC pointer = end of line before trap handler
		INC	HL
		LD	(SAVSTK),SP		; store stack pointer
		JP	J4620			; start executing trap handler

	J47E7:		PUSH	BC

; Subroutine GOTO statement
GOTO:
C47E8:		CALL	LINGET			; collect line number

; Subroutine goto line number
C47EB:		LD	A,(CONSAV)
		CP	0DH			; line pointer token ?
		EX	DE,HL
		RET	Z			; yep, quit
		CP	0EH			; line number token ?
		JP	NZ,SNERR		; nope, syntax error
		EX	DE,HL
		PUSH	HL
		LD	HL,(CONTXT)
		EX	(SP),HL
		CALL	REM			; skip to end of BASIC line
		INC	HL
		PUSH	HL
		LD	HL,(CURLIN)
		RST	R_DCOMPR		; forward in program ?
		POP	HL
		CALL	C,C4298			; yep, search line number from current (faster)
		CALL	NC,FNDLIN		; not forward or exact match not found, search line number from start of program
		JR	NC,USERR		; no exact match found, undefined line number error
		DEC	BC			; end of line before found line
		LD	A,0DH
		LD	(PTRFLG),A
		POP	HL
		CALL	C5583
		LD	H,B
		LD	L,C			; update BASIC pointer
		RET

USERR:
J481C:		LD	E,8
		JP	ERROR			; undefined line number error

; Subroutine RETURN statement
RETURN:
C4821:		SYSHOOK	H_RETU			; hook return statement
		LD	(TEMP),HL		; store BASIC pointer
		LD	D,0FFH			; DE=0FFxxH (impossible loop variable address)
		CALL	FNDFOR			; search FOR block on stack (skip 2 words)
		CP	8DH			; search stopped by a GOSUB block ?
		JR	Z,J4831			; yep,
		DEC	HL
J4831:		LD	SP,HL
		LD	(SAVSTK),HL		; store stack pointer
		LD	E,3
		JP	NZ,ERROR		; nope, return without gosub error
		POP	HL			; restore pointer to trap entry
		LD	A,H
		OR	L			; return from trap handler ?
		JR	Z,J4845			; nope,
		LD	A,(HL)
		AND	00000001B		; trap enabled ?
		CALL	NZ,RSTTRP		; yep, unpause trap
J4845:		POP	BC			; restore line number (of GOSUB)
		LD	HL,NEWSTT
		EX	(SP),HL			; after this, execute new statement, restore BASIC pointer (of GOSUB)
		EX	DE,HL			; store BASIC pointer (of GOSUB)
		LD	HL,(TEMP)		; restore BASIC pointer
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		JP	NZ,GOTO			; not end of statement (line number specified), goto statement
		LD	H,B
		LD	L,C
		LD	(CURLIN),HL		; update current line number
		EX	DE,HL			; restore BASIC pointer (of GOSUB)
		DEFB	03EH			; LD A,xx, trick to skip next instruction


; Subroutine skip to end of statement/data
J485A:		POP	HL

; Subroutine skip to end of statement/data (also DATA statement handler)
DATA:
C485B:		DEFB	001H			; LD BC,xx3AH, normal end marker = end of statement, skip to 485E
		DEFB	":"

; Subroutine skip to end of BASIC line (also REM/ELSE statement handler)
ELSES:
REM:
C485D:		LD	C,0			; normal end marker = end of line
		LD	B,0			; in string end marker = end of line

J4861:		LD	A,C
		LD	C,B
		LD	B,A			; swap end markers
J4864:		DEC	HL
J4865:		RST	R_CHRGTR		; get next BASIC character
		OR	A			; end of BASIC line ?
		RET	Z			; yep, quit
		CP	B			; end mark character ?
		RET	Z			; yep, quit
		INC	HL
		CP	'"'			; string identifier ?
		JR	Z,J4861			; yep, swap end marker
		INC	A			; function token header ?
		JR	Z,J4865			; skip function token as well
		SUB	8BH+1			; IF token ?
		JR	NZ,J4864		; nope, next
		CP	B			; end marker = end of line ?
		ADC	A,D
		LD	D,A			; nope, increase IF nesting level
		JR	J4864			; next

LETCON:
J487B:		POP	AF			; restore DAC type
		ADD	A,3			; to DAC type
		JR	J4892

; Subroutine LET statement
LET:
C4880:		CALL	PTRGET			; locate variable
		RST	R_SYNCHR
		DEFB	0EFH			; check for =
		LD	(TEMP),DE		; store pointer to variable
		PUSH	DE			; store pointer to variable
		LD	A,(VALTYP)
		PUSH	AF			; store variable type
		CALL	FRMEVL			; evaluate expression
		POP	AF			; restore variable type
J4892:		EX	(SP),HL			; store BASIC pointer, restore pointer to variable

INPCOM:
J4893:		LD	B,A
		LD	A,(VALTYP)
		CP	B
		LD	A,B			; result expression of the same type as variable ?
		JR	Z,J48A1			; yep, no need to convert
		CALL	DOCNVF			; convert to DAC to new type
J489E:		LD	A,(VALTYP)
J48A1:		LD	DE,DAC
		CP	2			; integer ?
		JR	NZ,J48AB
		LD	DE,DAC+2		; yep, use DAC+2
J48AB:		PUSH	HL			; store pointer to variable
		CP	3			; string ?
		JR	NZ,J48DE		; nope, just copy value in variable
		LD	HL,(DAC+2)
		PUSH	HL			; store pointer to string descriptor
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)			; pointer to string
		LD	HL,KBUF-1
		RST	R_DCOMPR
		JR	C,J48D2			; string in KBUF, copy in string storage area
		LD	HL,(STREND)
		RST	R_DCOMPR
		POP	DE			; restore pointer to string descriptor
		JR	NC,J48DA		; string in programtext, no need to copy
		LD	HL,DSCTMP-1
		RST	R_DCOMPR
		JR	C,J48D1			; temporary stringdescriptor, copy string in string storage area
		LD	HL,TEMPST-1
		RST	R_DCOMPR
		JR	C,J48DA			; no temporary result string descriptor, no need to copy
J48D1:		DEFB	03EH			; LD A,xx, trick to skip next instruction
J48D2:		POP	DE			; restore pointer to string descriptor
		CALL	FRETMS			; free descriptor if temporary and on top of heap
		EX	DE,HL
		CALL	STRCPY			; copy string to new temporary string
J48DA:		CALL	FRETMS			; free descriptor if temporary and on top of heap
		EX	(SP),HL
J48DE:		CALL	VMOVE			; HL=DE (valtyp)
		POP	DE			; restore pointer to variable
		POP	HL			; restore BASIC pointer
		RET

; Subroutine ON statement
ONGOTO:
C48E4:		CP	0A6H			; ERROR token ?
		JR	NZ,J490D

		; ON ERROR
		RST	R_CHRGTR		; get next BASIC character (ERROR token)
		RST	R_SYNCHR
		DEFB	089H			; check for GOTO token
		CALL	LINGET			; collect line number
		LD	A,D
		OR	E			; line number zero ?
		JR	Z,J48FB			; yep, no ERROR handling
		CALL	C4293			; store BASIC pointer and search line number from start of program
		LD	D,B
		LD	E,C			; store pointer to found line number
		POP	HL			; restore BASIC pointer
		JP	NC,USERR		; line number not found, undefined line number error
J48FB:		LD	(ONELIN),DE		; update ERROR handler pointer
		RET	C			; not ON ERROR GOTO 0, quit
		LD	A,(ONEFLG)
		OR	A			; in ERROR handling routine ?
		LD	A,E			; ?? unneeded instruction ??
		RET	Z			; nope, quit
		LD	A,(ERRFLG)
		LD	E,A			; saved errorcode
		JP	J4096			; BASIC error

; Subroutine ON statement (not ON ERROR)
J490D:		CALL	ONGOT			; trap token ?
		JR	C,J4943			; nope, ON value GOTO/GOSUB
		PUSH	BC			; store trap base number, maximum number of traps
		RST	R_CHRGTR		; get next BASIC character
		RST	R_SYNCHR
		DEFB	08DH			; check for GOSUB token
		XOR	A			; line number count = 0
J4917:		POP	BC			; restore trap base number, maximum number of traps
		PUSH	BC			; store trap base number, maximum number of traps
		CP	C			; linenuber count exceeds maximum number of traps ?
		JP	NC,SNERR		; yep, syntax error
		PUSH	AF			; store line number count
		CALL	LINGET			; collect line number
		LD	A,D
		OR	E			; line number = 0 ?
		JR	Z,J492E			; yep, skip line number check
		CALL	C4293			; store BASIC pointer and search line number from start of program
		LD	D,B
		LD	E,C			; store pointer to found line number
		POP	HL			; restore BASIC pointer
		JP	NC,USERR		; not found, undefined line number error
J492E:		POP	AF			; restore line number count
		POP	BC			; restore trap base number, maximum number of traps
		PUSH	AF			; store line number count
		ADD	A,B			; + trap base number
		PUSH	BC			; store trap base number, maximum number of traps
		CALL	SETGSB			; update trap handler line number
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		POP	BC			; restore trap base number, maximum number of traps
		POP	DE			; restore line number count
		RET	Z			; yep, quit
		PUSH	BC			; store trap base number, maximum number of traps
		PUSH	DE			; store line number count
		RST	R_SYNCHR
		DEFB	","			; check for ,
		POP	AF			; restore line number count
		INC	A			; update line number count
		JR	J4917			; next line number

; ON value GOTO/GOSUB
J4943:		CALL	GETBYT			; evaluate byte operand
		LD	A,(HL)
		LD	B,A			; store token
		CP	8DH			; GOSUB token ?
		JR	Z,J494F
		RST	R_SYNCHR
		DEFB	089H			; check for GOTO token
		DEC	HL
J494F:		LD	C,E			; store value
J4950:		DEC	C			; decrease value, found the right line number ?
		LD	A,B			; restore token
		JP	Z,J4646			; yep, execute GOTO/GOSUB
		CALL	C476A			; collect line number (skip over line number)
		CP	','			; more line numbers ?
		RET	NZ			; nope, quit
		JR	J4950			; try next line number

; Subroutine RESUME statement
RESUME:
C495D:		LD	A,(ONEFLG)
		OR	A			; in ERROR handling routine ?
		JR	NZ,J496C		; yep,
		LD	(ONELIN+0),A
		LD	(ONELIN+1),A		; disable ERROR handler
		JP	J4064			; resume without error

J496C:		INC	A
		LD	(ERRFLG),A		; clear errorcode
		LD	A,(HL)
		CP	83H			; NEXT token ?
		JR	Z,J4985			; yep,
		CALL	LINGET			; collect line number
		RET	NZ			; not end of statement, quit (which generates syntax error)
		LD	A,D
		OR	E			; line number zero ?
		JR	Z,J4989			; yep, resume at error position
		CALL	C47EB			; goto line number
		XOR	A
		LD	(ONEFLG),A		; not in ERROR handling routine anymore
		RET

J4985:		RST	R_CHRGTR		; get next BASIC character
		RET	NZ			; not end of statement, quit (which generates syntax error)
		JR	J498E			; flag RESUME 0

J4989:		XOR	A
		LD	(ONEFLG),A		; not in ERROR handling routine
		INC	A			; flag RESUME NEXT
J498E:		LD	HL,(ERRTXT)
		EX	DE,HL
		LD	HL,(ERRLIN)
		LD	(CURLIN),HL		; current line number = line number when error occured
		EX	DE,HL			; BASIC pointer = BASIC pointer when error occured
		RET	NZ			; RESUME NEXT, quit
		LD	A,(HL)
		OR	A			; at end of BASIC line ?
		JR	NZ,J49A2		; nope,
		INC	HL
		INC	HL
		INC	HL
		INC	HL			; skip linelink and line number
J49A2:		INC	HL
		XOR	A
		LD	(ONEFLG),A		; not in ERROR handling routine
		JP	DATA			; skip to end of statement

; Subroutine ERROR statement
ERRORS:
C49AA:		CALL	GETBYT			; evaluate byte operand
		RET	NZ			; not end of statement, quit (which generates a syntax error)
		OR	A			; errornumber 0 ?
		JP	Z,FCERR			; yep, illegal function call
		JP	ERROR			; BASIC error

; Subroutine AUTO statement
AUTO:
C49B5:		LD	DE,10			; default step value = 10
		PUSH	DE			; default starting line number = 10
		JR	Z,J49D1			; end of statement, start auto
		CALL	LINSPC			; collect line number (with DOT supported)
		EX	DE,HL
		EX	(SP),HL			; replace starting line number
		JR	Z,J49D2			; end of statement, start auto
		EX	DE,HL
		RST	R_SYNCHR
		DEFB	","			; check for ,
		LD	DE,(AUTINC)
		JR	Z,J49D1			; end of statement, use previous step value and start auto
		CALL	LINGET			; collect line number (step value)
		JP	NZ,SNERR		; not end of statement, syntax error
J49D1:		EX	DE,HL
J49D2:		LD	A,H
		OR	L			; step value is 0 ?
		JP	Z,FCERR			; yep, illegal function call error
		LD	(AUTINC),HL
		LD	(AUTFLG),A		; in auto line number mode
		POP	HL
		LD	(AUTLIN),HL
		POP	BC
		JP	MAIN			; main loop

; Subroutine IF statement
IFS:
C49E5:		CALL	FRMEVL			; evaluate expression
		LD	A,(HL)
		CP	','
		CALL	Z,CHRGTR		; yep, get next BASIC character
		CP	89H			; GOTO token ?
		JR	Z,J49F5			; yep,
		RST	R_SYNCHR
		DEFB	0DAH			; check for THEN token
		DEC	HL
J49F5:		PUSH	HL
		CALL	VSIGN			; get sign DAC
		POP	HL
		JR	Z,J4A0C			; DAC is zero, execute ELSE part (if any)
J49FC:		RST	R_CHRGTR		; get next BASIC character
		RET	Z			; end of statement, quit
		CP	0EH			; line number follows ?
		JP	Z,GOTO			; yep, goto statement
		CP	0DH			; line pointer follows ?
		JP	NZ,J4646		; nope, execute THEN part
		LD	HL,(CONLO)
		RET

J4A0C:		LD	D,1			; IF nesting level=1
J4A0E:		CALL	DATA			; skip to end of statement
		OR	A			; end of BASIC line ?
		RET	Z			; yep, quit
		RST	R_CHRGTR		; get next BASIC character
		CP	0A1H			; ELSE token ?
		JR	NZ,J4A0E		; nope, skip more
		DEC	D
		JR	NZ,J4A0E
		JR	J49FC

; Subroutine LPRINT statement
LPRINT:
C4A1D:		LD	A,1
		LD	(PRTFLG),A		; BASIC interpreter output = printer
		JR	J4A29

; Subroutine PRINT statement
PRINT:
C4A24:		LD	C,2			; compatible file mode = sequential output
		CALL	FILGET			; redirect interpreter output if I/O channel specified
J4A29:		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		CALL	Z,CRDO			; yep, newline to interpreter output
J4A2E:		JP	Z,FINPRT		; end BASIC interpreter redirect
		CP	0E4H
		JP	Z,PRINUS		; USING token,
		CP	0DBH
		JP	Z,J4AC6			; TAB( token,
		CP	0DFH
		JP	Z,J4AC6			; SPC( token,
		PUSH	HL
		CP	','
		JR	Z,J4A94
		CP	';'
		JP	Z,J4AFA
		POP	BC
		CALL	FRMEVL			; evaluate expression
		PUSH	HL
		RST	R_GETYPR		; get DAC type
		JR	Z,J4A8D			; string,
		CALL	FOUT			; convert DAC to text, unformatted
		CALL	STRLIT			; analyse string and create temporary stringdescriptor
		LD	(HL),20H
		LD	HL,(DAC+2)
		INC	(HL)
		SYSHOOK	H_PRTF
		CALL	ISFLIO			; is BASIC interpreter I/O redirected to I/O channel ?
		JR	NZ,J4A89		; yep,
		LD	HL,(DAC+2)
		LD	A,(PRTFLG)
		OR	A			; BASIC interpreter output to screen ?
		JR	Z,J4A77			; yep,
		LD	A,(LPTPOS)
		ADD	A,(HL)
		CP	255
		JR	J4A81

J4A77:		LD	A,(LINLEN)
		LD	B,A
		LD	A,(TTYPOS)
		ADD	A,(HL)
		DEC	A
		CP	B
J4A81:		JR	C,J4A89
		CALL	Z,CRFIN			; yep, interpreter output pos = 0
		CALL	NZ,CRDO			; nope, newline to interpreter output
J4A89:		CALL	STRPRT			; free string and string to interpreter output
		OR	A
J4A8D:		CALL	Z,STRPRT		; free string and string to interpreter output
		POP	HL
		JP	J4A29

J4A94:		SYSHOOK	H_COMP
		LD	BC,8
		LD	HL,(PTRFIL)		; BASIC interpreter I/O channel
		ADD	HL,BC			; +8
		CALL	ISFLIO			; is BASIC interpreter I/O redirected to I/O channel ?
		LD	A,(HL)			; position in buffer
		JR	NZ,J4ABF		; yep,
		LD	A,(PRTFLG)
		OR	A			; BASIC interpreter output to screen ?
		JR	Z,J4AB1			; yep,
		LD	A,(LPTPOS)
		CP	238
		JR	J4AB9

J4AB1:		LD	A,(CLMLST)
		LD	B,A
		LD	A,(TTYPOS)
		CP	B
J4AB9:		CALL	NC,CRDO			; new line to interpreter output
		JP	NC,J4AFA
J4ABF:		SUB	14
		JR	NC,J4ABF
		CPL
		JR	J4AF3

J4AC6:		PUSH	AF
		CALL	GTBYTC			; skip basic char and evaluate byte operand
		RST	R_SYNCHR
		DEFB	")"			; check for )
		DEC	HL
		POP	AF
		SUB	0DFH			; SPC( token ?
		PUSH	HL
		JR	Z,J4AEF			; yep,
		LD	BC,8
		LD	HL,(PTRFIL)		; BASIC interpreter I/O channel
		ADD	HL,BC			; +8
		CALL	ISFLIO			; is BASIC interpreter I/O redirected to I/O channel ?
		LD	A,(HL)			; position in buffer
		JR	NZ,J4AEF		; yep,
		LD	A,(PRTFLG)
		OR	A			; BASIC interpreter output to screen ?
		JP	Z,J4AEC			; yep,
		LD	A,(LPTPOS)		; printer position in line
		JR	J4AEF

J4AEC:		LD	A,(TTYPOS)		; screen position in line
J4AEF:		CPL
		ADD	A,E
		JR	NC,J4AFA
J4AF3:		INC	A
		LD	B,A
		LD	A,' '
J4AF7:		RST	OUTDO			; space to interpreter output
		DJNZ	J4AF7
J4AFA:		POP	HL
		RST	R_CHRGTR		; get next BASIC character
		JP	J4A2E

; Subroutine end BASIC interpreter redirect
FINPRT:
C4AFF:		SYSHOOK	H_FINP
		XOR	A
		LD	(PRTFLG),A		; BASIC interpreter output = screen
		PUSH	HL
		LD	H,A
		LD	L,A
		LD	(PTRFIL),HL		; end BASIC interpreter redirect
		POP	HL
		RET

; Subroutine LINE statement
LINE:
C4B0E:		CP	85H			; next character INPUT token ?
		JP	NZ,GLINE		; nope, graphics LINE statement
		RST	R_SYNCHR
		DEFB	085H			; check for INPUT token
		CP	'#'			; I/O channel indicator ?
		JP	Z,DLINE			; yep, LINE INPUT for I/O channel
		CALL	C4B7B			; optional input prompt string to interpreter output
		CALL	PTRGET			; locate variable
		CALL	CHKSTR			; check if string
		PUSH	DE			; store pointer to variable
		PUSH	HL			; store BASIC pointer
		CALL	INLIN			; get input line
		POP	DE			; restore BASIC pointer
		POP	BC			; restore pointer to variable
		JP	C,STPEND		; aborted,
		PUSH	BC			; store pointer to variable
		PUSH	DE			; store BASIC pointer
		LD	B,0			; end character = none
		CALL	STRLT3			; analyze string with specified endmaker (1st char is skipped) and create temporary stringdescriptor
		POP	HL			; restore BASIC pointer
		LD	A,3			; DAC type = string
		JP	J4892

I4B3A:		DEFB	"?Redo from start",13,10,0

J4B4D:		SYSHOOK	H_TRMN
		LD	A,(FLGINP)
		OR	A			; READ statement ?
		JP	NZ,J404F		; yep, update CURLIN with DATA line number and quit with syntax error
		POP	BC
		LD	HL,I4B3A		; redo message
		CALL	STROUT			; message to interpreter output
		LD	HL,(SAVTXT)		; restore BASIC pointer to start of INPUT statement
		RET

; INPUT for I/O channel
J4B62:		CALL	FILINP			; evaluate I/O channel number, update BASIC interpreter I/O channel and check for compatible input file mode
		PUSH	HL			; store BASIC pointer
		LD	HL,BUFMIN		; before BUF
		JP	J4B9B

; Subroutine INPUT statement
INPUT:
C4B6C:		CP	'#'			; I/O channel indicator ?
		JR	Z,J4B62			; yep, INPUT for I/O channel
		PUSH	HL			; store BASIC pointer
		PUSH	AF			; store BASIC character
		CALL	TOTEXT			; force text screenmode
		POP	AF			; restore BASIC character
		POP	HL			; restore BASIC pointer
		LD	BC,C4B8B
		PUSH	BC			; after this, continue with INPUT

; Subroutine optional input prompt string to interpreter output
C4B7B:		CP	'"'			; input prompt specified ?
		LD	A,0
		RET	NZ			; nope, start input
		CALL	STRLTI			; analyze string with " as end marker (1st char is skipped) and create temporary stringdescriptor
		RST	R_SYNCHR
		DEFB	";"			; check for ;
		PUSH	HL			; store BASIC pointer
		CALL	STRPRT			; free string and string to interpreter output
		POP	HL			; restore BASIC pointer
		RET

; INPUT continued
C4B8B:		PUSH	HL			; store BASIC pointer
		CALL	QINLIN			; get input line with question mark
		POP	BC			; restore BASIC pointer
		JP	C,STPEND		; aborted,
		INC	HL
		LD	A,(HL)
		OR	A			; empty input line ?
		DEC	HL
		PUSH	BC			; store BASIC pointer
		JP	Z,J485A			; yep, skip to end of statement and continue with program
J4B9B:		LD	(HL),','		; data seperator at the begin of data
		JR	J4BA4			; next input

; Subroutine READ statement
READ:
C4B9F:		PUSH	HL			; store BASIC pointer
		LD	HL,(DATPTR)		; DATA pointer
		DEFB	0F6H			; XOR 0AFH (set READ statement flag), trick to skip next instruction

; entry point for next INPUT
J4BA4:		XOR	A
		LD	(FLGINP),A		; clear READ statement flag
		EX	(SP),HL			; store DATA pointer, restore BASIC pointer
		DEFB	001H			; LD BC,xxxx, trick to skip next instruction
J4BAA:		RST	R_SYNCHR
		DEFB	","			; check for ,
		CALL	PTRGET			; locate variable
		EX	(SP),HL			; store BASIC pointer, restore DATA pointer
		PUSH	DE			; store pointer to variable
		LD	A,(HL)
		CP	','			; data seperator ?
		JR	Z,J4BD1			; yep,
		LD	A,(FLGINP)
J4BB9:		OR	A			; READ statement ?
		JP	NZ,J4C40		; yep,
		LD	A,'?'
		RST	OUTDO			; "?" to interpreter output
		CALL	QINLIN			; get input line with question mark
		POP	DE			; restore pointer to variable
		POP	BC			; restore BASIC pointer
		JP	C,STPEND		; aborted,
		INC	HL
		LD	A,(HL)
		DEC	HL
		OR	A			; empty input line ?
		PUSH	BC			; store BASIC pointer
		JP	Z,J485A			; yep, skip to end of data
		PUSH	DE			; store pointer to variable

J4BD1:		CALL	ISFLIO			; is BASIC interpreter I/O redirected to I/O channel ?
		JP	NZ,FILIND		; yep,
		RST	R_GETYPR		; get DAC type
		PUSH	AF			; store DAC type
		JR	NZ,J4BFD		; not a string,
		RST	R_CHRGTR		; get next BASIC character
		LD	D,A
		LD	B,A
		CP	'"'
		JR	Z,J4BEE
		LD	A,(FLGINP)
		OR	A			; READ statement ?
		LD	D,A			; end character2 = none
		JR	Z,J4BEB			; nope,
		LD	D,':'			; end character2 = statement seperator
J4BEB:		LD	B,','			; end character1 = comma
		DEC	HL
J4BEE:		CALL	STRLT2			; analyse string with specified end markers (1st char is skipped) and create temporary stringdescriptor

DOASIG:
J4BF1:		POP	AF			; restore DAC type
		ADD	A,3			; to DAC type
		EX	DE,HL			; store BASIC pointer
		LD	HL,C4C05
		EX	(SP),HL			; store subroutine, restore pointer to variable
		PUSH	DE			; store BASIC pointer
		JP	INPCOM			; assign value to variable

J4BFD:		RST	R_CHRGTR		; get next BASIC character
		LD	BC,DOASIG
		PUSH	BC			; after this,
		JP	FIN			; convert text to number

C4C05:		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		JR	Z,J4C0E			; yep,
		CP	','			; seperator character ?
		JP	NZ,J4B4D		; nope,
J4C0E:		EX	(SP),HL			; store DATA pointer, restore BASIC pointer
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		JP	NZ,J4BAA		; nope, next
		POP	DE			; restore DATA pointer
		LD	A,(FLGINP)
		OR	A			; READ statement ?
		EX	DE,HL			; to DATA pointer
		JP	NZ,RESFIN		; yep, set new DATA pointer and quit
		PUSH	DE			; store BASIC pointer
		CALL	ISFLIO			; is BASIC interpreter I/O redirected to I/O channel ?
		JR	NZ,J4C2B		; yep, skip extra ignored
		LD	A,(HL)
		OR	A			; DATA ends ?

C4C25:		LD	HL,I4C2F		; message = extra ignored
		CALL	NZ,STROUT		; nope, message to interpreter output
J4C2B:		POP	HL			; restore BASIC pointer
		JP	FINPRT			; end BASIC interpreter redirect

I4C2F:		DEFB	"?Extra ignored",13,10,0

J4C40:		CALL	DATA			; skip to end of data
		OR	A			; end of line ?
		JR	NZ,J4C57		; nope,
		INC	HL
		LD	A,(HL)
		INC	HL
		OR	(HL)			; end of program ?
		LD	E,4
		JP	Z,ERROR			; yep, out of data error
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)			; line number
		LD	(DATLIN),DE		; update DATA line number
J4C57:		RST	R_CHRGTR		; get next BASIC character
		CP	84H			; DATA token ?
		JR	NZ,J4C40		; nope,
		JP	J4BD1

; Subroutine evaluate = expression
; Input:  HL = BASIC pointer
FRMEQL:
C4C5F:		RST	R_SYNCHR
		DEFB	0EFH			; check for =
		DEFB	001H			; LD BC,xxxx, trick to skip next instruction

; Subroutine evaluate ( expression
; Input:  HL = BASIC pointer
FRMPRN:
C4C62:		RST	R_SYNCHR
		DEFB	"("			; check for (

; Subroutine FRMEVL (Expression Evaluator)
; Input:  HL = BASIC pointer
FRMEVL:
C4C64:		DEC	HL


; Subroutine skip character and evaluate expression
FRMCHK:
C4C65:		LD	D,0			; current precedence level 0

; Subroutine skip character and evaluate expression with precendence level
LPOPER:
C4C67:		PUSH	DE			; store current precedence level
		LD	C,1			; word = 1
		CALL	GETSTK			; check if enough stackspace
		SYSHOOK	H_FRME
		CALL	EVAL			; evaluate factor
TSTOP:
I4C73:		LD	(TEMP2),HL		; store BASIC pointer
J4C76:		LD	HL,(TEMP2)		; restore BASIC pointer
		POP	BC			; restore current precedence level
		LD	A,(HL)			; get BASIC character
		LD	(TEMP3),HL		; store BASIC pointer
		CP	0EEH			; token EEH or above ?
		RET	C			; nope, quit (expression ends)
		CP	0F1H			; math operator ?
		JR	C,J4CE4			; nope, relational operators (EEH-F0H)
		SUB	0F1H
		LD	E,A			; store math operator (0 based)
		JR	NZ,J4C93		; not a addition
		LD	A,(VALTYP)
		CP	3			; string ?
		LD	A,E			; restore math operator (0 based)
		JP	Z,CAT			; yep, concat strings
J4C93:		CP	0FDH-0F1H		; token FDH-FFH ?
		RET	NC			; yep, quit
		LD	HL,I3D3B		; table for math operator precedence level
		LD	D,0
		ADD	HL,DE
		LD	A,B			; current precedence level
		LD	D,(HL)
		CP	D			; same or smaller precedence level ?
		RET	NC			; yep, quit
J4CA0:		PUSH	BC			; store current precedence level
		LD	BC,J4C76
		PUSH	BC			; after this, return
		LD	A,D
		SYSHOOK	H_NTPL
		CP	51H
		JR	C,J4CFD
		AND	0FEH
		CP	7AH
		JR	Z,J4CFD
J4CB3:		LD	HL,DAC+2
		LD	A,(VALTYP)
		SUB	3
		JP	Z,TMERR			; type mismatch
		OR	A
		LD	HL,(DAC+2)
		PUSH	HL
		JP	M,J4CD5			; integer
		LD	HL,(DAC+0)
		PUSH	HL
		JP	PO,J4CD5
		LD	HL,(DAC+6)
		PUSH	HL
		LD	HL,(DAC+4)
		PUSH	HL
J4CD5:		ADD	A,3			; to DAC type
		LD	C,E
		LD	B,A
		PUSH	BC
		LD	BC,I4D22
J4CDD:		PUSH	BC
		LD	HL,(TEMP3)
		JP	LPOPER			; skip character and evaluate expression with precendence level

J4CE4:		LD	D,0			; clear flag
J4CE6:		SUB	0EEH			; relational operators ?
		JR	C,J4D08			; nope,
		CP	0F0H-0EEH+1
		JR	NC,J4D08		; nope,
		CP	0EFH-0EEH
		RLA				; b0 is >, b1 is =, b2 is <
		XOR	D
		CP	D			; > = > >= >< >=< =< 
		LD	D,A
		JP	C,SNERR			; nope, syntax error
		LD	(TEMP3),HL
		RST	R_CHRGTR		; get next BASIC character
		JR	J4CE6

J4CFD:		PUSH	DE
		CALL	FRCINT			; convert DAC to integer
		POP	DE
		PUSH	HL
		LD	BC,I4F78
		JR	J4CDD

J4D08:		LD	A,B
		CP	100
		RET	NC
		PUSH	BC
		PUSH	DE
		LD	DE,100*256+5
		LD	HL,I4F57
		PUSH	HL
		RST	R_GETYPR		; get DAC type
		JP	NZ,J4CB3		; not a string,
		LD	HL,(DAC+2)
		PUSH	HL			; store pointer to string descriptor
		LD	BC,STRCMP		; string compare routine
		JR	J4CDD

; Subroutine apply infix math operator
I4D22:		POP	BC
		LD	A,C
		LD	(DORES),A
		LD	A,(VALTYP)
		CP	B
		JR	NZ,J4D38
		CP	2
		JR	Z,J4D50
		CP	4
		JP	Z,J4D9D
		JR	NC,J4D63
J4D38:		LD	D,A
		LD	A,B
		CP	8
		JR	Z,J4D60
		LD	A,D
		CP	8
		JR	Z,J4D87
		LD	A,B
		CP	4
		JR	Z,J4D9A
		LD	A,D
		CP	3
		JP	Z,TMERR			; type mismatch
		JR	NC,J4DA4
J4D50:		LD	HL,I3D69
		LD	B,00H
		ADD	HL,BC
		ADD	HL,BC
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		POP	DE
		LD	HL,(DAC+2)
		PUSH	BC
		RET

J4D60:		CALL	FRCDBL			; convert DAC to double real
J4D63:		CALL	VMOVAF			; ARG = DAC
		POP	HL
		LD	(DAC+4),HL
		POP	HL
		LD	(DAC+6),HL
J4D6E:		POP	BC
		POP	DE
		CALL	MOVFR			; DAC = (single)
J4D73:		CALL	FRCDBL			; convert DAC to double real
		LD	HL,I3D51
J4D79:		LD	A,(DORES)
		RLCA
		ADD	A,L
		LD	L,A
		ADC	A,H
		SUB	L
		LD	H,A
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		JP	(HL)

J4D87:		LD	A,B
		PUSH	AF
		CALL	VMOVAF			; ARG = DAC
		POP	AF
		LD	(VALTYP),A
		CP	4
		JR	Z,J4D6E
		POP	HL
		LD	(DAC+2),HL
		JR	J4D73

J4D9A:		CALL	FRCSNG			; convert DAC to single real
J4D9D:		POP	BC
		POP	DE
J4D9F:		LD	HL,I3D5D
		JR	J4D79

J4DA4:		POP	HL
		CALL	PUSHF			; push DAC (single)
		CALL	CONSIH			; convert to single precision real
		CALL	MOVRF			; DEBC = DAC (single)
		POP	HL
		LD	(DAC+0),HL
		POP	HL
		LD	(DAC+2),HL
		JR	J4D9F

INTDIV:
C4DB8:		PUSH	HL
		EX	DE,HL
		CALL	CONSIH			; convert to single precision real
		POP	HL
		CALL	PUSHF			; push DAC (single)
		CALL	CONSIH			; convert to single precision real
		JP	SGNDVT

; Subroutine Factor Evaluator
EVAL:
C4DC7:		RST	R_CHRGTR		; get next BASIC character
		JP	Z,J406A			; end of statement, missing operand error
		JP	C,FIN			; convert text to number
		CALL	ISLET2			; is upcase letter character ?
		JP	NC,ISVAR		; yep, get variable value
		CP	20H			; numeric token ?
		JP	C,J46B8			; yep, get constant value
		SYSHOOK	H_EVAL
		INC	A			; function token ?
		JP	Z,J4EFC			; yep, handle function
		DEC	A
		CP	0F1H			; + token ?
		JR	Z,EVAL			; yep, again
		CP	0F2H			; - token ?
		JP	Z,J4E8D			; yep,
		CP	'"'
		JP	Z,STRLTI		; analyze string with " as end marker (1st char is skipped) and create temporary stringdescriptor and quit
		CP	0E0H			; NOT token ?
		JP	Z,J4F63			; yep,
		CP	'&'
		JP	Z,C4EB8			; convert text with radix indication to number
		CP	0E2H			; ERR token ?
		JR	NZ,J4E07		; nope, other

		; Subroutine ERR function
		RST	R_CHRGTR		; get next BASIC character
		LD	A,(ERRFLG)
		PUSH	HL
		CALL	SNGFLT			; byte to DAC
		POP	HL
		RET

J4E07:		CP	0E1H			; ERL token ?
		JR	NZ,J4E15		; nope, other

		; Subroutine ERL function
		RST	R_CHRGTR		; get next BASIC character
		PUSH	HL
		LD	HL,(ERRLIN)
		CALL	CONSUI			; convert unsigned integer to single real
		POP	HL
		RET

J4E15:		CP	0EDH			; POINT token ?
		JP	Z,POINT			; yep, POINT function handler
		CP	0CBH			; TIME token ?
		JP	Z,TIMEF			; yep, TIME function handler
		CP	0C7H			; SPRITE token ?
		JP	Z,SPRITF		; yep, SPRITE function handler
		CP	0C8H			; VDP token ?
		JP	Z,VDPF			; yep, VDP function handler
		CP	0C9H			; BASE token ?
		JP	Z,BASEF			; yep, BASE function handler
		CP	0C1H			; PLAY token ?
		JP	Z,PLAYF			; yep, PLAY function handler
		CP	0EAH			; DSKI$ token ?
		JP	Z,DSKIS			; yep, DSKI$ function handler
		CP	0E9H			; ATTR$ token ?
		JP	Z,ATTRS			; yep, ATTR$ function handler
		CP	0E7H			; VARPTR token ?
		JR	NZ,J4E64		; nope, other

		; Subroutine VARPTR function
		RST	R_CHRGTR		; get next BASIC character
		RST	R_SYNCHR
		DEFB	"("			; check for (
		CP	'#'			; I/O channel indicator ?
		JR	NZ,J4E53		; nope, varptr for variables
		CALL	GTBYTC			; skip basic char and evaluate byte operand
		PUSH	HL			; store BASIC pointer
		CALL	FILIDX			; get pointer to I/O channel
		EX	DE,HL
		POP	HL			; restore BASIC pointer
		JR	J4E56

J4E53:		CALL	PTRGTV			; locate variable (search only)
J4E56:		RST	R_SYNCHR
		DEFB	")"			; check for )
		PUSH	HL
		EX	DE,HL
		LD	A,H
		OR	L			; variable found ?
		JP	Z,FCERR			; nope, illegal function call
		CALL	MAKINT			; put HL in DAC
		POP	HL
		RET

J4E64:		CP	0DDH			; USR token ?
		JP	Z,USRFN			; yep, USR function handler
		CP	0E5H			; INSTR token ?
		JP	Z,INSTR			; yep, INSTR function handler
		CP	0ECH			; INKEY$ token ?
		JP	Z,INKEY			; yep, INKEY$ function handler
		CP	0E3H			; STRING$ token ?
		JP	Z,STRNGS		; yep, STRING$ function handler
		CP	85H			; INPUT token ?
		JP	Z,FIXINP		; yep, INPUT function handler
		CP	0E8H			; CSRLIN token ?
		JP	Z,CSRLIN		; yep, CSRLIN function handler
		CP	0DEH			; FN token ?
		JP	Z,FNDOER		; yep, FN function handler

; Subroutine evaluate ( expression )
C4E87:		CALL	FRMPRN			; evaluate ( expression
		RST	R_SYNCHR
		DEFB	")"			; check for )
		RET

J4E8D:		LD	D,7DH
		CALL	LPOPER			; skip character and evaluate expression with precendence level
		LD	HL,(TEMP2)
		PUSH	HL
		CALL	VNEG			; negate
I4E99:		POP	HL
		RET

; Subroutine get variable value
ISVAR:
C4E9B:		CALL	PTRGET			; locate variable (without creation)
		PUSH	HL
		EX	DE,HL
		LD	(DAC+2),HL
		RST	R_GETYPR		; get DAC type
		CALL	NZ,VMOVFM		; not a string, DAC = HL
		POP	HL
		RET

MAKUPL:
C4EA9:		LD	A,(HL)

; Subroutine upcase char
MAKUPS:
C4EAA:		CP	'a'
		RET	C
		CP	'z'+1
		RET	NC
		AND	5FH
		RET

; Subroutine ?
; Unused Code: Not called from anywhere, leftover from a early Microsoft BASIC
Q4EB3:		CP	'&'
		JP	NZ,LINGET		; collect line number

; Subroutine convert text with radix indication to number
OCTCNS:
C4EB8:		LD	DE,0
		RST	R_CHRGTR		; get next BASIC character
		CALL	MAKUPS
		LD	BC,0102H
		CP	'B'
		JR	Z,J4ED5
		LD	BC,0308H
		CP	'O'
		JR	Z,J4ED5
		LD	BC,0410H
		CP	'H'
		JP	NZ,SNERR		; nope, syntax error
J4ED5:		INC	HL
		LD	A,(HL)
		EX	DE,HL
		CALL	MAKUPS
		CP	'9'+1
		JR	C,J4EE5
		CP	'A'
		JR	C,J4EF7
		SUB	7
J4EE5:		SUB	'0'
		CP	C
		JR	NC,J4EF7
		PUSH	BC
J4EEB:		ADD	HL,HL
		JP	C,OVERR			; overflow error
		DJNZ	J4EEB
		POP	BC
		OR	L
		LD	L,A
		EX	DE,HL
		JR	J4ED5

J4EF7:		CALL	MAKINT			; put HL in DAC
		EX	DE,HL
		RET

J4EFC:		INC	HL
		LD	A,(HL)
		SUB	81H
		LD	B,0
		RLCA
		LD	C,A
		PUSH	BC
		RST	R_CHRGTR		; get next BASIC character
		LD	A,C
		CP	05H
		JR	NC,J4F21
		CALL	FRMPRN			; evaluate ( expression
		RST	R_SYNCHR
		DEFB	","			; check for ,
		CALL	CHKSTR			; check if string
		EX	DE,HL
		LD	HL,(DAC+2)
		EX	(SP),HL
		PUSH	HL
		EX	DE,HL
		CALL	GETBYT			; evaluate byte operand
		EX	DE,HL
		EX	(SP),HL
		JR	J4F3B

J4F21:		CALL	C4E87			; evaluate ( expression )
		EX	(SP),HL
		LD	A,L
		CP	0CH
		JR	C,J4F37

C4F2A:		CP	1BH
		SYSHOOK	H_OKNO
		JR	NC,J4F37
		RST	R_GETYPR		; get DAC type
		PUSH	HL
		CALL	C,FRCDBL		; not a double real, convert DAC to double real
		POP	HL
J4F37:		LD	DE,I4E99
		PUSH	DE
J4F3B:		LD	BC,I39DE
		SYSHOOK	H_FING

C4F41:		ADD	HL,BC
		LD	C,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,C
		JP	(HL)

; Subroutine check if sign, go back if not
; Input:  D = 0, HL = pointer
; Output: Zx set if sign was found, D = FFH when - sign found

MINPLS:
C4F47:		DEC	D
		CP	0F2H			; - token ?
		RET	Z			; yep, quit
		CP	'-'
		RET	Z
		INC	D
		CP	'+'
		RET	Z
		CP	0F1H			; + token ?
		RET	Z			; yep, quit
		DEC	HL
		RET

; Subroutine apply infix relational operator
I4F57:		INC	A
		ADC	A,A
		POP	BC
		AND	B
		ADD	A,0FFH
		SBC	A,A
		CALL	CONIA			; convert signed byte to integer and store in DAC
		JR	J4F75

J4F63:		LD	D,5AH
		CALL	LPOPER			; skip character and evaluate expression with precendence level
		CALL	FRCINT			; convert DAC to integer
		LD	A,L
		CPL
		LD	L,A
		LD	A,H
		CPL
		LD	H,A
		LD	(DAC+2),HL
		POP	BC
J4F75:		JP	J4C76

; Subroutine apply infix logical operator
I4F78:		LD	A,B
		PUSH	AF
		CALL	FRCINT			; convert DAC to integer
		POP	AF
		POP	DE
		CP	7AH
		JP	Z,IMOD			; integer mod
		CP	7BH
		JP	Z,IDIV			; integer divide
		LD	BC,GIVINT
		PUSH	BC
		CP	46H
		JR	NZ,J4F97
		LD	A,E
		OR	L
		LD	L,A
		LD	A,H
		OR	D
		RET

J4F97:		CP	50H
		JR	NZ,J4FA1
		LD	A,E
		AND	L
		LD	L,A
		LD	A,H
		AND	D
		RET

J4FA1:		CP	3CH
		JR	NZ,J4FAB
		LD	A,E
		XOR	L
		LD	L,A
		LD	A,H
		XOR	D
		RET

J4FAB:		CP	32H
		JR	NZ,J4FB7
		LD	A,E
		XOR	L
		CPL
		LD	L,A

C4FB3:		LD	A,H
		XOR	D
		CPL
		RET

J4FB7:		LD	A,L
		CPL
		AND	E
		CPL
		LD	L,A
		LD	A,H
		CPL
		AND	D
		CPL
		RET

GIVDBL:
J4FC1:		OR	A
		SBC	HL,DE
		JP	CONSUI			; convert unsigned integer to single real

; Subroutine LPOS function
LPOS:
C4FC7:		LD	A,(LPTPOS)
		JR	SNGFLT			; byte to DAC

; Subroutine POS function
POS:
C4FCC:		LD	A,(TTYPOS)

; Subroutine put byte in DAC
SNGFLT:
C4FCF:		LD	L,A
		XOR	A

GIVINT:
C4FD1:		LD	H,A
		JP	MAKINT			; put HL in DAC

; Subroutine USR function
; Remark: user machinecode subroutine gets the following parameters:
;         HL = DAC, A = variable type, DE = start of string

USRFN:
J4FD5:		CALL	C4FF4			; get user number and pointer to USRTAB entry
		PUSH	DE			; store pointer to USRTAB entry
		CALL	C4E87			; evaluate ( expression )
		EX	(SP),HL			; store BASIC pointer, restore pointer to USRTAB entry
		LD	E,(HL)
		INC	HL
		LD	D,(HL)			; get address of user defined machine language routine
		LD	HL,POPHRT
		PUSH	HL			; return here to restore basictext pointer after user subroutine
		PUSH	DE			; address of user defined machine language routine
		LD	A,(VALTYP)
		PUSH	AF			; store DAC type
		CP	3
		CALL	Z,FREDAC		; parameter is a string, free temporary string in DAC
		POP	AF			; restore DAC type
		EX	DE,HL
		LD	HL,DAC
		RET				; start user machinecode subroutine

; Subroutine get user number and pointer to USRTAB entry
C4FF4:		RST	R_CHRGTR		; get next BASIC character
		LD	BC,0			; default = 0
		CP	1BH			; valid integer token ?
		JR	NC,J5007		; nope, use default
		CP	11H			; valid integer token ?
		JR	C,J5007			; nope, use default
		RST	R_CHRGTR		; get next BASIC character
		LD	A,(CONLO)
		OR	A			; clear Cx
		RLA				; *2
		LD	C,A			; offset
J5007:		EX	DE,HL			; store BASIC pointer
		LD	HL,USRTAB
		ADD	HL,BC
		EX	DE,HL			; restore BASIC pointer
		RET

; Subroutine DEF USR statement
J500E:		CALL	C4FF4			; get user number and pointer to USRTAB entry
		PUSH	DE			; store pointer to USRTAB entry
		RST	R_SYNCHR
		DEFB	0EFH			; check for =
		CALL	GETUIN			; evaluate address operand
		EX	(SP),HL			; store BASIC pointer, restore pointer to USRTAB entry
		LD	(HL),E
		INC	HL
		LD	(HL),D			; update address of user defined machine language subroutine
		POP	HL			; restore BASIC pointer
		RET

; Subroutine DEF statement
DEF:
C501D:		CP	0DDH
		JR	Z,J500E			; USR token, DEFUSR
		CALL	C51A1			; check for FN and create functionname variable
		CALL	C5193			; illegal direct when in direct mode
		EX	DE,HL
		LD	(HL),E
		INC	HL
		LD	(HL),D
		EX	DE,HL			; store pointer to functiondefinition
		LD	A,(HL)
		CP	'('
		JP	NZ,DATA			; no parameters, skip to next statement and continue
		RST	R_CHRGTR		; get next BASIC character
J5033:		CALL	PTRGET			; locate variable
		LD	A,(HL)
		CP	')'
		JP	Z,DATA			; end of parameters, skip to next statement and continue
		RST	R_SYNCHR
		DEFB	","			; check for ,
		JR	J5033			; parse through parameters

; Subroutine FN function
FNDOER:
J5040:		CALL	C51A1			; check for FN and locate functionname variable
		LD	A,(VALTYP)
		OR	A
		PUSH	AF
		LD	(TEMP2),HL		; store BASIC pointer
		EX	DE,HL
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A			; pointer to functiondefinition
		LD	A,H
		OR	L
		JP	Z,J4061			; not defined, undefined user function error
		LD	A,(HL)
		CP	'('
		JP	NZ,J50F4		; no parameters, skip
		RST	R_CHRGTR		; get next BASIC character
		LD	(TEMP3),HL		; store function definition pointer
		EX	DE,HL
		LD	HL,(TEMP2)
		RST	R_SYNCHR
		DEFB	"("			; check for (
		XOR	A
		PUSH	AF
		PUSH	HL
		EX	DE,HL
J5069:		LD	A,80H
		LD	(SUBFLG),A		; variable search flag = function variable
		CALL	PTRGET			; locate variable
		EX	DE,HL
		EX	(SP),HL
		LD	A,(VALTYP)
		PUSH	AF
		PUSH	DE
		CALL	FRMEVL			; evaluate expression
		LD	(TEMP2),HL		; store BASIC pointer
		POP	HL
		LD	(TEMP3),HL
		POP	AF
		CALL	DOCNVF			; convert to DAC to new type
		LD	C,4			; words = 4
		CALL	GETSTK			; check if enough stackspace
		LD	HL,-8
		ADD	HL,SP
		LD	SP,HL
		CALL	VMOVMF			; HL = DAC
		LD	A,(VALTYP)
		PUSH	AF
		LD	HL,(TEMP2)		; restore basictext pointer
		LD	A,(HL)
		CP	')'
		JR	Z,J50AD
		RST	R_SYNCHR
		DEFB	","			; check for ,
		PUSH	HL
		LD	HL,(TEMP3)
		RST	R_SYNCHR
		DEFB	","			; check for ,
		JR	J5069

I50A9:		POP	AF
		LD	(PRMLN2),A
J50AD:		POP	AF
		OR	A
		JR	Z,J50E9
		LD	(VALTYP),A
		LD	HL,0
		ADD	HL,SP
		CALL	VMOVFM			; DAC = HL
		LD	HL,8
		ADD	HL,SP
		LD	SP,HL
		POP	DE
		LD	L,03H
		DEC	DE
		DEC	DE
		DEC	DE
		LD	A,(VALTYP)
		ADD	A,L
		LD	B,A
		LD	A,(PRMLN2)
		LD	C,A
		ADD	A,B
		CP	64H
		JP	NC,FCERR		; illegal function call
		PUSH	AF
		LD	A,L
		LD	B,00H
		LD	HL,PARM2
		ADD	HL,BC
		LD	C,A
		CALL	C518E
		LD	BC,I50A9
		PUSH	BC
		PUSH	BC
		JP	J489E

J50E9:		LD	HL,(TEMP2)
		RST	R_CHRGTR		; get next BASIC character
		PUSH	HL
		LD	HL,(TEMP3)
		RST	R_SYNCHR
		DEFB	")"			; check for )
		DEFB	03EH			; LD A,xx, trick to skip next instruction
J50F4:		PUSH	DE
		LD	(TEMP3),HL
		LD	A,(PRMLEN)
		ADD	A,4
		PUSH	AF
		RRCA
		LD	C,A			; number of words
		CALL	GETSTK			; check if enough stackspace
		POP	AF
		LD	C,A
		CPL
		INC	A
		LD	L,A
		LD	H,0FFH
		ADD	HL,SP
		LD	SP,HL
		PUSH	HL
		LD	DE,PRMSTK
		CALL	C518E
		POP	HL
		LD	(PRMSTK),HL
		LD	HL,(PRMLN2)
		LD	(PRMLEN),HL
		LD	B,H
		LD	C,L
		LD	HL,PARM1
		LD	DE,PARM2
		CALL	C518E
		LD	H,A
		LD	L,A
		LD	(PRMLN2),HL
		LD	HL,(FUNACT)
		INC	HL
		LD	(FUNACT),HL
		LD	A,H
		OR	L
		LD	(NOFUNS),A
		LD	HL,(TEMP3)
		CALL	FRMEQL			; evaluate = expression
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		JP	NZ,SNERR		; nope, syntax error
		RST	R_GETYPR		; get DAC type
		JR	NZ,J5156		; not a string,
		LD	DE,DSCTMP
		LD	HL,(DAC+2)
		RST	R_DCOMPR
		JR	C,J5156
		CALL	STRCPY			; copy string to new temporary string
		CALL	PUTTMP			; push descriptor to temporary descriptor heap
J5156:		LD	HL,(PRMSTK)
		LD	D,H
		LD	E,L
		INC	HL
		INC	HL
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		INC	BC
		INC	BC
		INC	BC
		INC	BC
		LD	HL,PRMSTK
		CALL	C518E
		EX	DE,HL
		LD	SP,HL
		LD	HL,(FUNACT)
		DEC	HL
		LD	(FUNACT),HL
		LD	A,H
		OR	L
		LD	(NOFUNS),A
		POP	HL
		POP	AF

; Subroutine convert DAC to other type
; Input:  A = new type, (VALTYP) = current type, (DAC) = current value
DOCNVF:
C517A:		PUSH	HL
		AND	07H
		LD	HL,I3D47
		LD	C,A
		LD	B,0
		ADD	HL,BC
		CALL	C4F41
		POP	HL
		RET

J5189:		LD	A,(DE)
		LD	(HL),A
		INC	HL
		INC	DE
		DEC	BC

C518E:		LD	A,B
		OR	C
		JR	NZ,J5189
		RET

C5193:		PUSH	HL
		LD	HL,(CURLIN)
		INC	HL
		LD	A,H
		OR	L			; interpreter in direct mode ?
		POP	HL
		RET	NZ			; nope, quit
		LD	E,12
		JP	ERROR			; illegal direct error

C51A1:		RST	R_SYNCHR
		DEFB	0DEH			; check for FN token
		LD	A,080H
		LD	(SUBFLG),A		; variable search flag = function variable
		OR	(HL)
		LD	C,A			; first varletter with b7 set
		JP	PTRGT2			; locate functionname variable

; Subroutine check if function token allowed as statement
J51AD:		CP	0FFH-081H		; function token header ?
		JR	NZ,J51C6		; nope, syntax error
		INC	HL
		LD	A,(HL)			; get function token
		INC	HL
		CP	83H			; MID$ ?
		JP	Z,MIDS2			; yep, execute MID$ statement
		CP	0A3H			; STRIG ?
		JP	Z,STRIGS		; yep, execute STRIG statement
		CP	85H			; INT ?
		JP	Z,INTS			; yep, check if INTERVAL
		SYSHOOK	H_ISMI			; hook for more function tokens as statement
	J51C6:		JP	SNERR			; syntax error

; Subroutine WIDTH statement
WIDTH:
C51C9:		CALL	GETBYT			; evaluate byte operand
		SYSHOOK	H_WIDT
		AND	A			; width 0 ?
	IFDEF MSX1
		jr	z,A51DF
		ld	a,(OLDSCR)
		and	a
		ld	a,e
		jr	z,A51DD
		cp	32+1
		jr	nc,A51DF
A51DD:		cp	40+1
A51DF:		jp	nc,FCERR
		ld	a,(LINLEN)
		cp	e
		ret	z
		ld	a,00CH
		RST	OUTDO
		ld	a,e
		ld	(LINLEN),a
		ld	a,(OLDSCR)
		dec	a
		ld	a,e
		jr	nz,A51FA
		ld	(LINL32),a
		jr	A51FD

A51FA:		ld	(LINL40),a
A51FD:		ld	a,00CH
		RST	OUTDO
		ld	a,e

MORCP2:
A5201:		sub	14
		jr	nc,A5201
		add	a,2*14
		cpl
		inc	a
		add	a,e
		ld	(CLMLST),a
		ret
	ELSE ; MSX2
		LD	IX,S_WIDTHS
		JP	EXTROM
		ALIGN	520EH			; address alignment
	ENDIF

GETINT:
C520E:		RST	R_CHRGTR		; get next BASIC character

; Subroutine evaluate integer operand
GETIN2:
C520F:		CALL	FRMEVL			; evaluate expression

INTFR2:
C5212:		PUSH	HL
		CALL	FRCINT			; convert DAC to integer
		EX	DE,HL
		POP	HL
		LD	A,D
		OR	A
		RET

; Subroutine skip basic char and evaluate byte operand
GTBYTC:
C521B:		RST	R_CHRGTR		; get next BASIC character

; Subroutine evaluate byte operand
GETBYT:
C521C:		CALL	FRMEVL			; evaluate expression

; Subroutine check for byte value
CONINT:
C521F:		CALL	INTFR2
		JP	NZ,FCERR		; illegal function call
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		LD	A,E
		RET

; Subroutine LLIST statement
LLIST:
C5229:		LD	A,1
		LD	(PRTFLG),A		; BASIC interpreter output = printer

; Subroutine LIST statement
LIST:
C522E:		SYSHOOK	H_LIST
		POP	BC
		CALL	C4279			; evaluate line number (range) and search start line number
		PUSH	BC			; store pointer to startline
J5236:		LD	HL,0FFFFH
		LD	(CURLIN),HL		; interpreter in direct mode
		POP	HL			; pointer startline
		POP	DE			; end line number
		LD	C,(HL)
		INC	HL
		LD	B,(HL)			; get line pointer
		INC	HL
		LD	A,B
		OR	C			; end of program ?
		JP	Z,READY			; yep, ok and main loop
		CALL	ISFLIO			; is BASIC interpreter I/O redirected to I/O channel ?
		CALL	Z,ISCNTC		; nope, handle CTRL/STOP or STOP pressed
		PUSH	BC			; store start of next line
		LD	C,(HL)
		INC	HL
		LD	B,(HL)			; line number
		INC	HL
		PUSH	BC
		EX	(SP),HL
		EX	DE,HL
		RST	R_DCOMPR		; compare with end line number
		POP	BC
		JP	C,STPRDY		; all done, ok and main loop (+POP)
		EX	(SP),HL
		PUSH	HL
		PUSH	BC
		EX	DE,HL
		LD	(DOT),HL
		CALL	LINPRT			; number to interpreter output
		POP	HL
		LD	A,(HL)
		CP	09H			; TAB ?
		JR	Z,J526D			; yep, skip space
		LD	A,' '
		RST	OUTDO			; space to interpreter output
J526D:		CALL	BUFLIN			; decode BASIC line
		LD	HL,BUF
		CALL	C527B			; string to interpreter output
		CALL	CRDO			; newline to interpreter output
		JR	J5236			; next line

; Subroutine string to interpreter output
C527B:		LD	A,(HL)
		OR	A
		RET	Z
		CALL	OUTCH1			; char to interpreter output, LF expanded
		INC	HL
		JR	C527B

; Subroutine decode BASIC line
BUFLIN:
C5284:		LD	BC,BUF
		LD	D,255
		XOR	A
		LD	(DORES),A		; clear decode status
		JR	J5293

J528F:		INC	BC
		INC	HL
		DEC	D
		RET	Z			; buffer full, quit
J5293:		LD	A,(HL)
		OR	A			; end of BASIC line ?
		LD	(BC),A
		RET	Z			; yep, quit
		CP	0BH			; 01H-0AH ?
		JR	C,J52C0			; yep, next BASIC char
		CP	20H			; 0BH-1FH ?
		JP	C,J5361			; yep, numeric token
		CP	'"'			; begin/end of string ?
		JR	NZ,J52AE		; nope,
		LD	A,(DORES)
		XOR	01H
		LD	(DORES),A		; toggle string status
		LD	A,'"'
J52AE:		CP	':'			; statement seperator ?
		JR	NZ,J52C0		; nope,
		LD	A,(DORES)
		RRA				; in string ?
		JR	C,J52BE			; yep,
		RLA
		AND	0FDH
		LD	(DORES),A		; clear data statement flag
J52BE:		LD	A,':'
J52C0:		OR	A			; 01H-7FH ?
		JP	P,J528F			; yep, next BASIC char
		LD	A,(DORES)
		RRA				; in string ?
		JR	C,J52F8			; yep, next BASIC char
		RRA
		RRA				; in rem statement ?
		JR	NC,J530C		; nope,
		LD	A,(HL)
		CP	0E6H			; ' token ?
		PUSH	HL
		PUSH	BC
		LD	HL,I52F5
		PUSH	HL			; when quit, next BASIC char
		RET	NZ			; nope, next BASIC char
		DEC	BC
		LD	A,(BC)
		CP	'M'
		RET	NZ
		DEC	BC
		LD	A,(BC)
		CP	'E'
		RET	NZ
		DEC	BC
		LD	A,(BC)
		CP	'R'
		RET	NZ
		DEC	BC
		LD	A,(BC)
		CP	':'			; preceeded by :REM ?
		RET	NZ			; nope, next BASIC char
		POP	AF			; remove returnaddress
		POP	AF			; remove buf pointer
		POP	HL			; restore line pointer
		INC	D
		INC	D
		INC	D
		INC	D			; remove :REM
		JR	J531A			; translate '

I52F5:		POP	BC
		POP	HL
		LD	A,(HL)
J52F8:		JP	J528F

; Subroutine set data statement flag
C52FB:		LD	A,(DORES)
		OR	02H
J5300:		LD	(DORES),A
		XOR	A
		RET

; Subroutine set rem statement flag
C5305:		LD	A,(DORES)
		OR	04H
		JR	J5300

J530C:		RLA				; in data statement ?
		JR	C,J52F8			; yep, next BASIC char
		LD	A,(HL)
		CP	84H			; DATA token ?
		CALL	Z,C52FB			; yep, set data statement flag
		CP	8FH			; REM token ?
		CALL	Z,C5305			; yep, set rem statement flag
J531A:		LD	A,(HL)
		INC	A			; function token header ?
		LD	A,(HL)
		JR	NZ,J5323		; nope,
		INC	HL
		LD	A,(HL)			; function token
		AND	7FH			; to 000H-07FH range
J5323:		INC	HL
		CP	0A1H			; ELSE token ?
		JR	NZ,J532A		; nope,
		DEC	BC
		INC	D
J532A:		PUSH	HL
		PUSH	BC
		PUSH	DE
		SYSHOOK	H_BUFL
		LD	HL,T3A72-1
		LD	B,A
		LD	C,'A'-1
J5336:		INC	C
J5337:		INC	HL
		LD	D,H
		LD	E,L
J533A:		LD	A,(HL)
		OR	A
		JR	Z,J5336
		INC	HL
		JP	P,J533A
		LD	A,(HL)
		CP	B
		JR	NZ,J5337
		EX	DE,HL
		LD	A,C
		POP	DE
		POP	BC
		CP	'Z'+1			; end of normal tokenlist ?
		JR	NZ,J5350		; nope,
J534E:		LD	A,(HL)			; single char token
		INC	HL
J5350:		LD	E,A
		AND	7FH
		LD	(BC),A
		INC	BC
		DEC	D
		JP	Z,PPSWRT		; pop af, ret
		OR	E
		JP	P,J534E
		POP	HL
		JP	J5293

; Subroutine decode numeric tokens
J5361:		DEC	HL
		RST	R_CHRGTR		; get numeric token
		PUSH	DE
		PUSH	BC
		PUSH	AF
		CALL	C46E8			; get numeric constant
		POP	AF
		LD	BC,I537E
		PUSH	BC
		CP	0BH
		JP	Z,FOUTO			; convert integer to octal text
		CP	0CH
		JP	Z,FOUTH			; convert integer to hexadecimal text
		LD	HL,(CONLO)

JPFOUT: JP	FOUT			; convert DAC to text, unformatted

I537E:		POP	BC
		POP	DE
		LD	A,(CONSAV)
		LD	E,'O'
		CP	0BH			; octal constant ?
		JR	Z,J538F			; yep, &O
		CP	0CH			; hexadecimal constant ?
		LD	E,'H'
		JR	NZ,J539A		; nope, skip &x
J538F:		LD	A,'&'
		LD	(BC),A
		INC	BC
		DEC	D
		RET	Z
		LD	A,E
		LD	(BC),A
		INC	BC
		DEC	D
		RET	Z
J539A:		LD	A,(CONTYP)
		CP	4			; single real ?
		LD	E,0
		JR	C,J53A9			; integer, 
		LD	E,'!'
		JR	Z,J53A9			; single real,
		LD	E,'#'			; double real
J53A9:		LD	A,(HL)
		CP	' '
		JR	NZ,J53AF

C53AE:		INC	HL
J53AF:		LD	A,(HL)
		INC	HL
		OR	A
		JR	Z,J53D4
		LD	(BC),A

C53B5:		INC	BC
		DEC	D
		RET	Z
		LD	A,(CONTYP)
		CP	4
		JR	C,J53AF
		DEC	BC
		LD	A,(BC)
		INC	BC
		JR	NZ,J53C8
		CP	'.'
		JR	Z,J53D0
J53C8:		CP	'D'
		JR	Z,J53D0
		CP	'E'
		JR	NZ,J53AF
J53D0:		LD	E,00H
		JR	J53AF

J53D4:		LD	A,E
		OR	A
		JR	Z,J53DC
		LD	(BC),A
		INC	BC
		DEC	D
		RET	Z
J53DC:		LD	HL,(CONTXT)
		JP	J5293

; Subroutine DELETE statement
DELETE:
C53E2:		CALL	C4279			; evaluate line number (range) and search start line number
		PUSH	BC
		CALL	DEPTR			; convert to line pointers to line numbers if needed
		POP	BC
		POP	DE
		PUSH	BC
		PUSH	BC
		CALL	FNDLIN			; search line number from start of program (end line number)
		JR	NC,J53F7		; no exact match found, illegal function call
		LD	D,H
		LD	E,L
		EX	(SP),HL
		PUSH	HL
		RST	R_DCOMPR
J53F7:		JP	NC,FCERR		; illegal function call
		LD	HL,REDDY
	IFDEF MSX1
		CALL	STROUT			; message to interpreter output
	ELSE
		CALL	C7BE8			; MSX2 patch: display prompt
	ENDIF
		POP	BC
		LD	HL,FINI
		EX	(SP),HL

; Subroutine remove line(s)
; Input:  HL = start of BASIC text that follows, BC = start of deleted BASIC text
C5405:		EX	DE,HL
		LD	HL,(VARTAB)
J5409:		LD	A,(DE)
		LD	(BC),A
		INC	BC
		INC	DE
		RST	R_DCOMPR
		JR	NZ,J5409
		LD	H,B
		LD	L,C
		LD	(VARTAB),HL		; start variable area
		LD	(ARYTAB),HL		; start arrayvariable area = start variable area (no variables)
		LD	(STREND),HL		; start free area = start variable area (no arrayvariables)
		RET

; Subroutine PEEK function
PEEK:
C541C:		CALL	FRQINT			; convert address to integer
		LD	A,(HL)
		JP	SNGFLT			; byte to DAC

; Subroutine POKE statement
POKE:
C5423:		CALL	GETUIN			; evaluate address operand
		PUSH	DE
		RST	R_SYNCHR
		DEFB	","			; check for ,
		CALL	GETBYT			; evaluate byte operand
		POP	DE
		LD	(DE),A
		RET

; Subroutine evaluate address operand
GETUIN:
C542F:		CALL	FRMEVL			; evaluate expression

GETUI:		PUSH	HL			; store BASIC pointer
		CALL	FRQINT			; convert address to integer
		EX	DE,HL
		POP	HL			; restore BASIC pointer
		RET

; Subroutine convert address to integer
FRQINT:
C5439:		LD	BC,FRCINT
		PUSH	BC			; convert DAC to integer
		RST	R_GETYPR		; get DAC type
		RET	M			; already a integer, quit
		SYSHOOK	H_FRQI
		CALL	SIGN			; get sign DAC
		RET	M			; DAC is negative, just convert
		CALL	FRCSNG			; convert DAC to single real
		LD	BC,03245H
		LD	DE,08076H		; 32768
		CALL	FCOMP			; single real compare
		RET	C			; smaller as 32768, just convert
		LD	BC,06545H
		LD	DE,06053H		; 65536
		CALL	FCOMP			; single real compare
		JP	NC,OVERR		; bigger as 65535, overflow error
		LD	BC,065C5H
		LD	DE,06053H		; -65536
		JP	SGNADD			; single real addition

; Subroutine RENUM statement
RESEQ:
C5468:		LD	BC,10			; new line number default = 10
		PUSH	BC			; step default = 10
		LD	D,B
		LD	E,B			; starting line number of renum default = 0 (start of program)
		JR	Z,J5496			; end of statement, start renum with default
		CP	','
		JR	Z,J547D			; new line number not specified, use default
		PUSH	DE
		CALL	LINSPC			; collect line number (with DOT supported)
		LD	B,D
		LD	C,E			; new line number
		POP	DE
		JR	Z,J5496			; end of statement, start renum
J547D:		RST	R_SYNCHR
		DEFB	","			; check for ,
		CALL	LINSPC			; collect line number (with DOT supported)
		JR	Z,J5496			; end of statement, start renum
		POP	AF
		RST	R_SYNCHR
		DEFB	","			; check for ,
		PUSH	DE
		CALL	LINGET			; collect line number (step)
		JP	NZ,SNERR		; not end of statement, syntax error
		LD	A,D
		OR	E			; step is 0 ?
		JP	Z,FCERR			; yep, illegal function call
		EX	DE,HL
		EX	(SP),HL
		EX	DE,HL			; update step value

J5496:		PUSH	BC
		CALL	FNDLIN			; search line number (starting of renum) from start of program
		POP	DE
		PUSH	DE
		PUSH	BC			; store line pointer
		CALL	FNDLIN			; search line number (new) from start of program
		LD	H,B
		LD	L,C
		POP	DE
		RST	R_DCOMPR
		EX	DE,HL
		JP	C,FCERR			; illegal function call
		POP	DE
		POP	BC
		POP	AF
		PUSH	HL
		PUSH	DE
		JR	J54BD

J54AF:		ADD	HL,BC
		JP	C,FCERR			; illegal function call
		EX	DE,HL
		PUSH	HL
		LD	HL,0FFF9H
		RST	R_DCOMPR
		POP	HL
		JP	C,FCERR			; illegal function call
J54BD:		PUSH	DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		LD	A,D
		OR	E
		EX	DE,HL

C54C4:		POP	DE
		JR	Z,J54CE
		LD	A,(HL)
		INC	HL
		OR	(HL)
		DEC	HL
		EX	DE,HL
		JR	NZ,J54AF
J54CE:		PUSH	BC
		CALL	SCCLIN			; convert line numbers to pointers
		POP	BC
		POP	DE
		POP	HL
J54D5:		PUSH	DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		LD	A,D
		OR	E
		JR	Z,SCCALL
		EX	DE,HL
		EX	(SP),HL
		EX	DE,HL
		INC	HL
		LD	(HL),E
		INC	HL
		LD	(HL),D
		EX	DE,HL
		ADD	HL,BC
		EX	DE,HL
		POP	HL
		JR	J54D5

; Subroutine convert to line pointers to line numbers if needed
DEPTR:
C54EA:		LD	A,(PTRFLG)
		OR	A
		RET	Z
		JR	SCCPTR			; convert pointers to line numbers

SCCALL:
J54F1:		LD	BC,STPRDY
		PUSH	BC			; ok and main loop (+POP)
		DEFB	0FEH			; CP xx, skip to J54F7

; Subroutine convert line numbers to pointers
SCCLIN:
C54F6:		DEFB	0F6H			; OR xx, skip next instruction, A<>0 (now line pointers)

; Subroutine convert pointers to line numbers
SCCPTR:
J54F7:		XOR	A			; now line numbers
		LD	(PTRFLG),A
		LD	HL,(TXTTAB)
		DEC	HL
J54FF:		INC	HL
		LD	A,(HL)
		INC	HL
		OR	(HL)			; endpointer ?
		RET	Z			; yep, quit
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)			; line number
J5508:		RST	R_CHRGTR		; get next BASIC character
J5509:		OR	A			; end of line ?
		JR	Z,J54FF			; yep, next line
		LD	C,A
		LD	A,(PTRFLG)
		OR	A			; convert to line numbers ?
		LD	A,C
		JR	Z,J556A			; yep, handle
		SYSHOOK	H_SCNE
		CP	0A6H			; ERROR token ?
		JR	NZ,J552F		; nope,
		RST	R_CHRGTR		; get next BASIC character
		CP	89H			; GOTO token ?
		JR	NZ,J5509		; nope,
		RST	R_CHRGTR		; get next BASIC character
		CP	0EH			; line number token ?
		JR	NZ,J5509		; nope,
		PUSH	DE
		CALL	C4771			; get line number
		LD	A,D
		OR	E			; line number 0 ?
		JR	NZ,J5537		; nope,
		JR	J5556

J552F:		CP	0EH			; line number token ?
		JR	NZ,J5508		; nope, next char
		PUSH	DE
		CALL	C4771			; get line number
J5537:		PUSH	HL
		CALL	FNDLIN			; search line number from start of program
		DEC	BC			; to end of line before found line number
		LD	A,0DH			; line pointer token
		JR	C,J557C			; found, replace line number with pointer
		CALL	CRDONZ			; fresh line to interpreter output
		LD	HL,I555A
		PUSH	DE
		CALL	STROUT			; message to interpreter output
		POP	HL
		CALL	LINPRT			; number to interpreter output
		POP	BC
		POP	HL
		PUSH	HL
		PUSH	BC
		CALL	INPRT			; "in" number to interpreter output
I5555:		POP	HL
J5556:		POP	DE
		DEC	HL
J5558:		JR	J5508			; next

I555A:		DEFB	"Undefined line ",0

J556A:		CP	0DH			; line pointer token ?
		JR	NZ,J5558		; nope, next
		PUSH	DE
		CALL	C4771			; get line pointer
		PUSH	HL
		EX	DE,HL
		INC	HL
		INC	HL
		INC	HL
		LD	C,(HL)
		INC	HL
		LD	B,(HL)			; get line number
		LD	A,0EH			; line number token
J557C:		LD	HL,I5555
		PUSH	HL
		LD	HL,(CONTXT)

C5583:		PUSH	HL
		DEC	HL
		LD	(HL),B
		DEC	HL
		LD	(HL),C
		DEC	HL
		LD	(HL),A
		POP	HL
		RET

; Subroutine SYNCHR
SYNCHR:
C558C:		LD	A,(HL)			; get BASIC character
		EX	(SP),HL
		CP	(HL)			; same as required ?
		INC	HL
		EX	(SP),HL
		JP	NZ,SNERR		; nope, quit with syntax error
		JP	CHRGTR			; get next BASIC character

; Subroutine GETYPR
GETYPR:
C5597:		LD	A,(VALTYP)
		CP	8			; double float ?
		JR	NC,J55A3		; yep,
		SUB	3
		OR	A			; set Zx if string, set Sx if integer
		SCF				; set Cx
		RET

J55A3:		SUB	3			; A = 5
		OR	A			; reset Zx, reset Cx, reset Sx
		RET

; ------------------------------------------------------------------------------
; MSXEXP.MAC
; BASIC expansion functions
; ------------------------------------------------------------------------------

		ALIGN	55A7H

CALLS1:
J55A7:		RST	R_CHRGTR		; get next BASIC character

; Subroutine CALL statement
CALLS2:
C55A8:		LD	DE,PROCNM		; CALL statement name buffer
		LD	B,16-1			; buffer size left counter
J55AD:		LD	A,(HL)
		AND	A			; end of BASIC line ?
		JR	Z,J55BE			; yep, end of name
		CP	':'			; BASIC statement seperator ?
		JR	Z,J55BE			; yep, end of name
		CP	'('			; parameter indicator ?
		JR	Z,J55BE			; yep, end of name
		LD	(DE),A
		INC	DE
		INC	HL
		DJNZ	J55AD			; next statement name character
J55BE:		LD	A,B
		CP	16-1			; empty statement name ?
		JR	Z,J55D8			; yep, syntax error
J55C3:		XOR	A
		LD	(DE),A			; statement name end marker
		DEC	DE
		LD	A,(DE)
		CP	' '			; statement name end with space ?
		JR	Z,J55C3			; yep, remove space from statement name
		LD	B,4*4*4			; extension page counter
		LD	DE,SLTATR
J55D0:		LD	A,(DE)			; attributes extension page
J55D1:		AND	20H			; has CALL statement handler ?
		JR	NZ,J55DB		; yep, execute CALL statement handler
J55D5:		INC	DE
		DJNZ	J55D0			; next extension page
J55D8:		JP	SNERR			; syntax error

; execute CALL statement handler
J55DB:		PUSH	BC			; store extension page counter
		PUSH	DE			; store pointer in SLTATR
		PUSH	HL			; store BASIC pointer
		CALL	ATRSL2			; translate extension page counter to address and slot id
		PUSH	AF			; store slot id
		LD	C,A			; slot id
		LD	L,4			; offset CALL statement entry
		CALL	RDWEXP			; read word from extension ROM
		PUSH	DE
		POP	IX			; IX =  CALL statement handler address
		POP	IY			; IYH = slot id
		POP	HL			; restore BASIC pointer
		DEC	HL
		RST	R_CHRGTR		; get current BASIC character
		CALL	CALSLT			; execute CALL statement handler
		POP	DE			; restore pointer in SLTATR
		POP	BC			; restore extension page counter
		JR	C,J55D5			; statement not recognized, try next extension page
		RET				; statement handled, quit

; external device handler
EXTDEV:
J55F8:		POP	HL			; restore pointer to device name
		LD	A,B
		CP	16-1+1			; size of device name to long ?
		JR	C,J5600			; nope, continue
		LD	B,16-1			; use maximium device name length

J5600:
	IF NDEVFIX = 0
		; early MSX 1 version code
		LD	DE,PROCNM
	ELSE
		; later MSX versions have a patch for zero length device name
		CALL	CHKZDN		  
	ENDIF

J5603:		CALL	MAKUPL			; get char uppercase
		LD	(DE),A
		INC	HL
		INC	DE
		DJNZ	J5603			; copy device name in PROCNM
		XOR	A
		LD	(DE),A			; device name end marker
		LD	B,4*4*4			; extension page counter
		LD	DE,SLTATR
J5612:		LD	A,(DE)			; attributes extension page
		AND	40H			; has BASIC device handler ?
		JR	NZ,J561D		; yep, execute BASIC device handler
J5617:		INC	DE
		DJNZ	J5612			; next extension page
J561A:		JP	DERNMF			; device name not recognized, bad filename error

J561D:		PUSH	BC			; store extension page counter
		PUSH	DE			; store pointer in SLTATR
		CALL	ATRSL2			; translate extension page counter to address and slotid
		PUSH	AF			; store slot id
		LD	C,A			; slot id
		LD	L,6			; offset BASIC device entry
		CALL	RDWEXP			; read word from extension ROM
		PUSH	DE
		POP	IX			; IX = BASIC device handler
		POP	IY			; IYH = slot id
		LD	A,0FFH			; function = device inquire
		CALL	CALSLT			; execute device handler
		POP	DE			; restore pointer in SLTATR
		POP	BC			; restore extension page counter
		JR	C,J5617			; device name not recognized, try next extension page
		LD	C,A			; relative device id
		LD	A,4*4*4
		SUB	B
		ADD	A,A
		ADD	A,A
		OR	C			; calculate device id
		CP	9			; device id in the disk drive device id range ?
		JR	C,J561A			; yep, bad filename error
		CP	0FCH			; device id in the internal device id range ?
		JR	NC,J561A		; yep, bad filename error
		POP	HL			; restore BASIC pointer
		POP	DE			; restore file specification length left
		AND	A			; Cx reset
		RET

; Subroutine i/o function dispatcher for extension ROM
EXTDFN:
J564A:		PUSH	BC			; store BC
		PUSH	AF			; store device id
		RRA
		RRA
		AND	3FH			; translate device id to SLTATR entry
		CALL	ATRSLI			; translate SLTATR entry number to address and slot id
		PUSH	AF			; store slot id
		LD	C,A			; slot id
		LD	L,6			; offset BASIC device entry
		CALL	RDWEXP			; read word from extension ROM
		PUSH	DE
		POP	IX			; IX = BASIC device handler
		POP	IY			; IYH = slot id
		POP	AF			; restore device id
		AND	03H
		LD	(DEVICE),A		; update relative device id
		POP	BC			; restore BC
		POP	AF			; store function
		POP	DE			; store file mode
		POP	HL			; store pointer to I/O channel
		JP	CALSLT			; execute BASIC device handler

; ------------------------------------------------------------------------------
; MACLNG.MAC
; BASIC macro language functions
; ------------------------------------------------------------------------------

		ALIGN	566CH

; Subroutine macro language parser
; Input:  HL = BASIC pointer, DE = pointer to macro language command table
MACLNG:
J566C:		LD	(MCLTAB),DE		; macro language command table
		CALL	FRMEVL			; evaluate expression
		PUSH	HL			; store BASIC pointer
		LD	DE,0
		PUSH	DE			; null macro string pointer (stops marco string parser)
		PUSH	AF			; dummy size of macro string
J5679:		CALL	FRESTR			; free temporary string with type check

	IF OPTM = 0
		CALL	MOVRM			; load from HL (single)
		LD	B,C
		LD	C,D			; pointer to string
		LD	D,E			; size of string
	ELSE
		CALL	GETBCD
	ENDIF

		LD	A,B
		OR	C			; undefined string ?
		JR	Z,J568C			; yep, resume macro string behind X command (if any)
		LD	A,D
		OR	A			; empty string ?
		JR	Z,J568C			; yep, resume macro string behind X command (if any)
		PUSH	BC			; store pointer to string
		PUSH	DE			; store size of string
J568C:		POP	AF			; restore size of string
		LD	(MCLLEN),A
		POP	HL			; restore pointer to string
		LD	A,H
		OR	L			; null macro string pointer ?
		JR	NZ,J569F		; nope, continue with macro string
		LD	A,(MCLFLG)
		OR	A			; macro language for DRAW ?
		JP	Z,J5709			; yep, restore BASIC pointer and quit
		JP	PLYEOS			; handle end of macro string for PLAY

J569F:		LD	(MCLPTR),HL		; update macro string pointer

; Subroutine execute macro command (if any)
MCLSCN:
J56A2:		CALL	FETCHR			; fetch macro string character
		JR	Z,J568C			; end of macro string,
		ADD	A,A
		LD	C,A			; command character shift left
		LD	HL,(MCLTAB)		; macro language command table
J56AC:		LD	A,(HL)
		ADD	A,A			; end of table ?
J56AE:
	IF OPTM = 0
		CALL	Z,FCERR			; yep, illegal function call
	ELSE
		JP	Z,FCERR
	ENDIF
		CP	C			; found command ?
		JR	Z,J56B9			; yep, handle
		INC	HL
		INC	HL
		INC	HL
		JR	J56AC			; next command

J56B9:		LD	BC,MCLSCN
		PUSH	BC			; after command, continue with next command
		LD	A,(HL)
		LD	C,A
		ADD	A,A			; command with parameter(s) ?
		JR	NC,J56E2		; nope, execute handle
		OR	A
		RRA
		LD	C,A
		PUSH	BC			; store command character
		PUSH	HL			; store pointer
		CALL	FETCHR			; fetch macro string character
		LD	DE,1

	IF OPTM = 0
		JP	Z,J56DF			; end of macro string, use default value 1
	ELSE
		JR	Z,J56DF			; end of macro string, use default value 1
	ENDIF

		CALL	ISLET2			; is upcase letter character ?

	IF OPTM = 0
		JP	NC,J56DC		; yep,
	ELSE
		JR	NC,J56DC		; yep,
	ENDIF

		CALL	C571C			; parse numeric operand in macro string (first character fetched)
		SCF				; value specified
		JR	J56E0

J56DC:		CALL	DECFET			; to previous macro string character
J56DF:		OR	A			; value not specified
J56E0:		POP	HL			; restore pointer
		POP	BC			; restore command
J56E2:		INC	HL
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A			; address command handler
		JP	(HL)			; execute command handler

; Subroutine fetch macro string character, error if end of macro string
; Output: A = character
FETCHZ:
C56E8:		CALL	FETCHR			; fetch macro string character
		JR	Z,J56AE			; end of macro string, illegal function call
		RET

; Subroutine fetch macro string character
; Output: A = character, Zx = set if no character
FETCHR:
C56EE:		PUSH	HL
J56EF:		LD	HL,MCLLEN
		LD	A,(HL)
		OR	A			; end of macro string ?
		JR	Z,J5709			; yep, quit
		DEC	(HL)			; update size left in macro string
		LD	HL,(MCLPTR)
		LD	A,(HL)
		INC	HL
		LD	(MCLPTR),HL		; update pointer in macro string
		CP	' '			; space ?
		JR	Z,J56EF			; yep, ignore and fetch next
		CP	60H			; lower case ?
		JR	C,J5709			; nope, quit
		SUB	20H			; to upper
J5709:		POP	HL
		RET

; Subroutine to previous macro string character
DECFET:
C570B:		PUSH	HL
		LD	HL,MCLLEN
		INC	(HL)
		LD	HL,(MCLPTR)
		DEC	HL
		LD	(MCLPTR),HL
		POP	HL
		RET

; Subroutine parse numeric operand in macro string
; Output: DE = value
VALSCN:
C5719:		CALL	FETCHZ			; fetch macro string character, error if end of macro string

; Subroutine parse numeric operand in macro string (first character fetched)
; Input:  A = first character of numeric operand string
; Output: DE = value
VALSC3:
C571C:		CP	'='			; numeric operand a variable ?
		JP	Z,J577A			; yep, parse variable name and fetch value
		CP	'+'
		JR	Z,VALSCN		; parse numeric operand in macro string
		CP	'-'
		JR	NZ,C572F		; nope, parse optional numeric constant value in macro string
		LD	DE,C5795
		PUSH	DE			; after parse, negate value
		JR	VALSCN			; parse numeric operand in macro string

; Subroutine parse optional numeric constant value in macro string
; Output: DE = value

VALSC2:
C572F:		LD	DE,0			; default value = 0
J5732:		CP	','
		JR	Z,DECFET		; to previous macro string character and quit
		CP	';'
		RET	Z
		CP	'9'+1			; digit ?
		JR	NC,DECFET		; nope, to previous macro string character and quit
		CP	'0'			; digit ?
		JR	C,DECFET		; nope, to previous macro string character and quit
		LD	HL,0
		LD	B,10
J5746:		ADD	HL,DE
		JR	C,J5773			; overflow, illegal function call
		DJNZ	J5746			; value = value *10
		SUB	'0'
		LD	E,A
		LD	D,0
		ADD	HL,DE			; + digit
		JR	C,J5773			; overflow, illegal function call
		EX	DE,HL
		CALL	FETCHR			; fetch macro string character
		JR	NZ,J5732		; not end of macro string, continue with value
		RET

; Subroutine parse variable name and fetch value
; Output: DE = value
SCNVR2:
C575A:		CALL	FETCHZ			; fetch macro string character, error if end of macro string
		LD	DE,BUF
		PUSH	DE			; store pointer to BUF
		LD	B,40			; variable name is max 40 characters
		CALL	ISLET2			; is upcase letter character ?
		JR	C,J5773			; nope, illegal function call
J5768:		LD	(DE),A
		INC	DE
		CP	';'			; end of variablename ?
		JR	Z,J5776			; yep,
		CALL	FETCHZ			; fetch macro string character, error if end of macro string
		DJNZ	J5768
J5773:
	IF OPTM = 0
		CALL	FCERR			; illegal function call
	ELSE
		JP	FCERR
	ENDIF

J5776:		POP	HL			; restore pointer to BUF
		JP	ISVAR			; get variable value

J577A:		CALL	C575A			; parse variable name, fetch value and convert to integer
		CALL	FRCINT			; convert DAC to integer
		EX	DE,HL			; value in DE
		RET

; Subroutine macro language X command
MCLXEQ:
C5782:		CALL	C575A			; parse variable name and fetch value
		LD	A,(MCLLEN)
		LD	HL,(MCLPTR)
		EX	(SP),HL			; store current macro string pointer, discard return address
		PUSH	AF			; store current macro string size
		LD	C,2
		CALL	GETSTK			; check if enough stackspace for 2 words
		JP	J5679			; continue parsing with string

; Subroutine negate value
; Input:  DE = value
; Output: DE = -value
NEGD:
C5795:		XOR	A
		SUB	E
		LD	E,A
		SBC	A,D
		SUB	E
		LD	D,A
		RET

; ------------------------------------------------------------------------------
; GENGRP.MAC
; BASIC generic graphic statements
; ------------------------------------------------------------------------------

		ALIGN	579CH


; Subroutine evaluate complex graphic coordinatepair
SCAN1:
C579C:		LD	A,(HL)
		CP	'@'
		CALL	Z,CHRGTR		; yep, get next BASIC character
		LD	BC,0			; assume X = 0
		LD	D,B
		LD	E,C			; assume Y = 0
		CP	0F2H			; - token ?
		JR	Z,J57C1			; yep,

; Subroutine evaluate simple graphic coordinatepair
SCAND:
C57AB:		LD	A,(HL)
		CP	0DCH			; STEP token ?
		PUSH	AF			; store STEP flag
		CALL	Z,CHRGTR		; yep, get next BASIC character
		RST	R_SYNCHR
		DEFB	"("			; check for (
		CALL	GETIN2			; evaluate integer operand
		PUSH	DE			; store X
		RST	R_SYNCHR
		DEFB	","			; check for ,
		CALL	GETIN2			; evaluate integer operand
		RST	R_SYNCHR
		DEFB	")"			; check for )
		POP	BC			; restore X
		POP	AF			; restore STEP flag
J57C1:		PUSH	HL			; store BASIC pointer
		LD	HL,(GRPACX)
		JR	Z,J57CA			; STEP, relative from current X
		LD	HL,0			; absolut X
J57CA:		ADD	HL,BC
		LD	(GRPACX),HL		; update current X
		LD	(GXPOS),HL
		LD	B,H
		LD	C,L			; BC = X
		LD	HL,(GRPACY)
		JR	Z,J57DB			; STEP, relative from current Y
		LD	HL,0			; absolut Y
J57DB:		ADD	HL,DE
		LD	(GRPACY),HL
		LD	(GYPOS),HL
		EX	DE,HL			; DE = Y
		POP	HL			; restore BASIC pointer
		RET

; Subroutine PRESET statement
PRESET:
C57E5:		LD	A,(BAKCLR)
		JR	J57ED

; Subroutine PSET statement
PSET:
C57EA:		LD	A,(FORCLR)
	IFDEF MSX1
J57ED:		PUSH	AF
		CALL	SCAND			; evaluate simple graphic coordinatepair
	ELSE
J57ED:		JP	J79D6			; MSX2 PSET expansion code
		DB	057H			; address alignment (leftover code)
	ENDIF
J57F1:		POP	AF
		CALL	ATRENT
		PUSH	HL
		CALL	SCALXY
		JR	NC,J5801
		CALL	MAPXYC
		CALL	SETC
J5801:		POP	HL
		RET

; Subroutine POINT function
POINT:
J5803:		RST	R_CHRGTR		; get next BASIC character
		PUSH	HL
		CALL	FETCHC
		POP	DE
		PUSH	HL
		PUSH	AF
		LD	HL,(GYPOS)
		PUSH	HL
		LD	HL,(GXPOS)
		PUSH	HL
		LD	HL,(GRPACY)
		PUSH	HL
		LD	HL,(GRPACX)
		PUSH	HL
		EX	DE,HL
		CALL	SCAND			; evaluate simple graphic coordinatepair
		PUSH	HL
		CALL	SCALXY
		LD	HL,0FFFFH
		JR	NC,J5831
		CALL	MAPXYC
		CALL	READC
		LD	L,A
		LD	H,0
J5831:		CALL	MAKINT			; put HL in DAC
		POP	DE
		POP	HL
		LD	(GRPACX),HL
		POP	HL
		LD	(GRPACY),HL
		POP	HL
		LD	(GXPOS),HL
		POP	HL
		LD	(GYPOS),HL
		POP	AF
		POP	HL
		PUSH	DE
		CALL	STOREC
		POP	HL
		RET

ATRSCN:
C584D:		LD	A,(FORCLR)

ATRENT:
C5850:		PUSH	BC
		PUSH	DE
		LD	E,A
		CALL	C59BC
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		JR	Z,J5863			; yep,
		RST	R_SYNCHR
		DEFB	","			; check for ,
		CP	','
		JR	Z,J5863
		CALL	GETBYT			; evaluate byte operand
J5863:		LD	A,E
		PUSH	HL
		CALL	SETATR
		JP	C,FCERR			; illegal function call
		POP	HL
		POP	DE
		POP	BC
		JP	CHRGT2			; get BASIC character

XDELT:
C5871:		LD	HL,(GXPOS)
		LD	A,L
		SUB	C
		LD	L,A
		LD	A,H
		SBC	A,B
		LD	H,A
J587A:		RET	NC

NEGHL:
C587B:		XOR	A
		SUB	L
		LD	L,A
		SBC	A,H
		SUB	L
		LD	H,A
		SCF
		RET

YDELT:
C5883:		LD	HL,(GYPOS)
		LD	A,L
		SUB	E
		LD	L,A
		LD	A,H
		SBC	A,D
		LD	H,A
		JR	J587A

XCHGY:
C588E:		PUSH	HL
		LD	HL,(GYPOS)
		EX	DE,HL
		LD	(GYPOS),HL
		POP	HL
		RET

XCHGAC:
C5898:		CALL	C588E

XCHGX:
C589B:		PUSH	HL
		PUSH	BC
		LD	HL,(GXPOS)
		EX	(SP),HL
		LD	(GXPOS),HL
		POP	BC
		POP	HL
		RET

GLINE:
	IFDEF MSX1
J58A7:		CALL	SCAN1			; evaluate complex graphic coordinatepair
	ELSE
J58A7:		JP	J79E9			; MSX2 LINE expansion code
	ENDIF
J58AA:		PUSH	BC
		PUSH	DE
		RST	R_SYNCHR
		DEFB	0F2H			; check for -
		CALL	SCAND			; evaluate simple graphic coordinatepair
		CALL	ATRSCN
		POP	DE
		POP	BC
		JR	Z,C58FC
		RST	R_SYNCHR
		DEFB	","
		RST	R_SYNCHR
		DEFB	"B"			; check for ,B
		JP	Z,J5912			; box option

		; line box fill option (,BF)
		RST	R_SYNCHR
		DEFB	"F"			; check for F

DOBOXF:		PUSH	HL
		CALL	SCALXY
		CALL	C5898
		CALL	SCALXY
		CALL	C5883
		CALL	C,C588E
		INC	HL
		PUSH	HL
		CALL	C5871
		CALL	C,C589B
		INC	HL
		PUSH	HL
		CALL	MAPXYC
		POP	DE
		POP	BC
J58E0:		PUSH	DE
		PUSH	BC
J58E2:		CALL	FETCHC
		PUSH	AF
		PUSH	HL
		EX	DE,HL
		CALL	NSETCX
		POP	HL
		POP	AF
		CALL	STOREC
		CALL	DOWNC
		POP	BC
		POP	DE
		DEC	BC
		LD	A,B
		OR	C
		JR	NZ,J58E0
		POP	HL
		RET

DOLINE:
C58FC:		PUSH	BC
		PUSH	DE
		PUSH	HL
		CALL	DOGRPH
		LD	HL,(GRPACX)
		LD	(GXPOS),HL
		LD	HL,(GRPACY)
		LD	(GYPOS),HL
		POP	HL
		POP	DE
		POP	BC
		RET

BOXLIN:
J5912:		PUSH	HL
		LD	HL,(GYPOS)
		PUSH	HL
		PUSH	DE
		EX	DE,HL
		CALL	C58FC
		POP	HL
		LD	(GYPOS),HL
		EX	DE,HL
		CALL	C58FC
		POP	HL
		LD	(GYPOS),HL
		LD	HL,(GXPOS)
		PUSH	BC
		LD	B,H
		LD	C,L
		CALL	C58FC
		POP	HL
		LD	(GXPOS),HL
		LD	B,H
		LD	C,L
		CALL	C58FC
		POP	HL
		RET

; Subroutine draw line
DOGRPH:
C593C:		SYSHOOK	H_DOGR
	IFDEF MSX1
		CALL	SCALXY
	ELSE
		JP	J7A0D			; MSX2 draw line expansion code	
	ENDIF
J5942:		CALL	C5898
		CALL	SCALXY
		CALL	C5883
		CALL	C,C5898
		PUSH	DE
		PUSH	HL
		CALL	C5871
		EX	DE,HL
		LD	HL,RIGHTC
		JR	NC,J595C
		LD	HL,LEFTC
J595C:		EX	(SP),HL
		RST	R_DCOMPR
		JR	NC,J5970
		LD	(MINDEL),HL
		POP	HL
		LD	(MAXUPD+1),HL
		LD	HL,DOWNC
		LD	(MINUPD+1),HL
		EX	DE,HL
		JR	J597F

J5970:		EX	(SP),HL
		LD	(MINUPD+1),HL
		LD	HL,DOWNC
		LD	(MAXUPD+1),HL
		EX	DE,HL
		LD	(MINDEL),HL
		POP	HL
J597F:		POP	DE
		PUSH	HL
		CALL	NEGHL
		LD	(MAXDEL),HL
		CALL	MAPXYC
		POP	DE
		PUSH	DE
		CALL	HLFDE
		POP	BC
		INC	BC
		JR	J599A

J5993:		POP	HL
		LD	A,B
		OR	C
		RET	Z
J5997:		CALL	MAXUPD
J599A:		CALL	SETC
		DEC	BC
		PUSH	HL
		LD	HL,(MINDEL)
		ADD	HL,DE
		EX	DE,HL
		LD	HL,(MAXDEL)
		ADD	HL,DE
		JR	NC,J5993
		EX	DE,HL
		POP	HL
		LD	A,B
		OR	C
		RET	Z
		CALL	MINUPD
		JR	J5997

; Subroutine divide by 2
HLFDE:
C59B4:		LD	A,D
		OR	A
		RRA
		LD	D,A
		LD	A,E
		RRA
		LD	E,A
		RET

C59BC:		LD	A,(SCRMOD)
		CP	2
		RET	P
		JP	FCERR			; illegal function call

; ------------------------------------------------------------------------------
; ADVGRP.MAC
; BASIC advanced graphic statements
; ------------------------------------------------------------------------------

		ALIGN	59C5H

; Subroutine PAINT statement
PAINT:
	IFDEF MSX1
C59C5:		CALL	SCAN1			; evaluate complex graphic coordinatepair
	ELSE
C59C5:		JP	J79FB			; MSX2 PAINT expansion code
	ENDIF
J59C8:		PUSH	BC			; store Y
		PUSH	DE			; store X
		CALL	ATRSCN
		LD	A,(ATRBYT)
		LD	E,A			; default paint border color = current color
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		JR	Z,J59DA			; yep,
		RST	R_SYNCHR
		DEFB	","			; check for ,
		CALL	GETBYT			; evaluate byte operand
J59DA:		LD	A,E			; paint border color
		CALL	PNTINI			; initialize for paint
		JP	C,FCERR			; invalid, illegal function call
		POP	DE			; restore X
		POP	BC			; restore Y

M59E3:		PUSH	HL			; store BASIC pointer
		CALL	C5E91			; check for in range
		CALL	MAPXYC
		LD	DE,1
		LD	B,0
		CALL	C5ADC
		JR	Z,J5A08
		PUSH	HL
		CALL	C5AED
		POP	DE
		ADD	HL,DE
		EX	DE,HL
		XOR	A
		CALL	C5ACE
		LD	A,40H
		CALL	C5ACE
		LD	B,0C0H
		JR	J5A26

J5A08:		POP	HL
		RET

J5A0A:		CALL	CKCNTC			; handle CTRL/STOP or STOP pressed, no resume
		LD	A,(LOHDIR)
		OR	A
		JR	Z,J5A1F
		LD	HL,(LOHADR)
		PUSH	HL
		LD	HL,(LOHMSK)
		PUSH	HL
		LD	HL,(LOHCNT)
		PUSH	HL
J5A1F:		POP	DE
		POP	BC
		POP	HL
		LD	A,C
		CALL	STOREC
J5A26:		LD	A,B
		LD	(PDIREC),A
		ADD	A,A
		JR	Z,J5A08
		PUSH	DE
		JR	NC,J5A35
		CALL	TUPC
		JR	J5A38

J5A35:		CALL	TDOWNC
J5A38:		POP	DE
		JR	C,J5A1F
		LD	B,0
		CALL	C5ADC
		JP	Z,J5A1F
		XOR	A
		LD	(LOHDIR),A
		CALL	C5AED
		LD	E,L
		LD	D,H
		OR	A
		JR	Z,J5A69
		DEC	HL
		DEC	HL
		LD	A,H
		ADD	A,A
		JR	C,J5A69
		LD	(LOHCNT),DE
		CALL	FETCHC
		LD	(LOHADR),HL
		LD	(LOHMSK),A
		LD	A,(PDIREC)
		CPL
		LD	(LOHDIR),A
J5A69:		LD	HL,(MOVCNT)
		ADD	HL,DE
		EX	DE,HL
		CALL	C5AC2
J5A71:		LD	HL,(CSAVEA)
		LD	A,(CSAVEM)
		CALL	STOREC
J5A7A:		LD	HL,(SKPCNT)
		LD	DE,(MOVCNT)
		OR	A
		SBC	HL,DE
		JR	Z,J5ABF
		JR	C,J5AA4
		EX	DE,HL
		LD	B,1
		CALL	C5ADC
		JR	Z,J5ABF
		OR	A
		JR	Z,J5A7A
		EX	DE,HL
		LD	HL,(CSAVEA)
		LD	A,(CSAVEM)
		LD	C,A
		LD	A,(PDIREC)
		LD	B,A
		CALL	C5AD3			; push on stack
		JR	J5A7A

J5AA4:		CALL	NEGHL
		DEC	HL
		DEC	HL
		LD	A,H
		ADD	A,A
		JR	C,J5ABF
		INC	HL
		PUSH	HL
J5AAF:		CALL	LEFTC
		DEC	HL
		LD	A,H
		OR	L
		JR	NZ,J5AAF
		POP	DE
		LD	A,(PDIREC)
		CPL
		CALL	C5ACE
J5ABF:		JP	J5A0A

ENTSLR:
C5AC2:		LD	A,(LFPROG)
		LD	C,A
		LD	A,(RTPROG)
		OR	C
		RET	Z
		LD	A,(PDIREC)

ENTST1:
C5ACE:		LD	B,A
		CALL	FETCHC
		LD	C,A

; Subroutine push on stack
C5AD3:		EX	(SP),HL			; restore return address
		PUSH	BC
		PUSH	DE
		PUSH	HL			; store return address
		LD	C,2
		JP	GETSTK			; check if enough stackspace for 2 words and quit

SCANR1:
C5ADC:		CALL	SCANR
		LD	(SKPCNT),DE
		LD	(MOVCNT),HL
		LD	A,H
		OR	L
		LD	A,C
		LD	(RTPROG),A
		RET

SCANL1:
C5AED:		CALL	FETCHC
		PUSH	HL
		PUSH	AF
		LD	HL,(CSAVEA)
		LD	A,(CSAVEM)
		CALL	STOREC
		POP	AF
		POP	HL
		LD	(CSAVEA),HL
		LD	(CSAVEM),A
		CALL	SCANL
		LD	A,C
		LD	(LFPROG),A
		RET

; Subroutine negate DE
NEGDE:
C5B0B:		EX	DE,HL
		CALL	NEGHL
		EX	DE,HL
		RET

; Subroutine CIRCLE statement
CIRCLE:
C5B11:		CALL	SCAN1			; evaluate complex graphic coordinatepair
		RST	R_SYNCHR
		DEFB	","			; check for ,
		CALL	GETIN2			; evaluate integer operand
		PUSH	HL
		EX	DE,HL
		LD	(GXPOS),HL
		CALL	MAKINT			; put HL in DAC
		CALL	FRCSNG			; convert DAC to single real
		LD	BC,7040H
		LD	DE,0771H
		CALL	SGNMUL			; single real muliply
		CALL	FRCINT			; convert DAC to integer
		LD	(CNPNTS),HL
		XOR	A
		LD	(CLINEF),A
		LD	(CSCLXY),A
		POP	HL
		CALL	ATRSCN
		LD	C,01H
		LD	DE,0
		CALL	C5D17
		PUSH	DE
		LD	C,80H
		LD	DE,0FFFFH
		CALL	C5D17
		EX	(SP),HL
		XOR	A
		EX	DE,HL
		RST	R_DCOMPR
		LD	A,00H
		JR	NC,J5B66
		DEC	A
		EX	DE,HL
		PUSH	AF
		LD	A,(CLINEF)
		LD	C,A
		RLCA
		RLCA
		OR	C
		RRCA
		LD	(CLINEF),A
		POP	AF
J5B66:		LD	(CPLOTF),A
		LD	(CSTCNT),DE
		LD	(CENCNT),HL
		POP	HL
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		JR	NZ,J5B85		; nope,
		PUSH	HL
		CALL	GTASPC
		LD	A,H
		OR	A
		JR	Z,J5BAF
		LD	A,01H
		LD	(CSCLXY),A
		EX	DE,HL
		JR	J5BAF

J5B85:		RST	R_SYNCHR
		DEFB	","			; check for ,
		CALL	FRMEVL			; evaluate expression
		PUSH	HL
		CALL	FRCSNG			; convert DAC to single real
		CALL	SIGN			; get sign DAC
		JP	Z,FCERR			; DAC is zero, illegal function call
		JP	M,FCERR			; DAC is negative, illegal function call
		CALL	C5D63
		JR	NZ,J5BA3
		INC	A
		LD	(CSCLXY),A
		CALL	SGNDIV			; single real divide
J5BA3:		LD	BC,02543H
		LD	DE,00060H
		CALL	SGNMUL			; single real muliply
		CALL	FRCINT			; convert DAC to integer
J5BAF:		LD	(ASPECT),HL
		LD	DE,0
		LD	(CRCSUM),DE
		LD	HL,(GXPOS)
		ADD	HL,HL
J5BBD:		CALL	CKCNTC			; handle CTRL/STOP or STOP pressed, no resume
		LD	A,E
		RRA
		JR	C,J5BDA
		PUSH	DE
		PUSH	HL
		INC	HL
		EX	DE,HL
		CALL	HLFDE			; divide by 2
		EX	DE,HL
		INC	DE
		CALL	HLFDE			; divide by 2
		CALL	C5C06
		POP	DE
		POP	HL
		RST	R_DCOMPR
		JP	NC,J5A08
		EX	DE,HL
J5BDA:		LD	B,H
		LD	C,L
		LD	HL,(CRCSUM)
		INC	HL
		ADD	HL,DE
		ADD	HL,DE
		LD	A,H
		ADD	A,A
		JR	C,J5BF2
		PUSH	DE
		EX	DE,HL
		LD	H,B
		LD	L,C
		ADD	HL,HL
		DEC	HL
		EX	DE,HL
		OR	A
		SBC	HL,DE
		DEC	BC
		POP	DE
J5BF2:		LD	(CRCSUM),HL
		LD	H,B
		LD	L,C
		INC	DE
		JR	J5BBD

CPLSCX:
C5BFA:		PUSH	DE
		CALL	C5CEB
		POP	HL
		LD	A,(CSCLXY)
		OR	A
		RET	Z
		EX	DE,HL
		RET

CPLOT8:
C5C06:		LD	(CPCNT),DE
		PUSH	HL
		LD	HL,0
		LD	(CPCNT8),HL
		CALL	C5BFA
		LD	(CXOFF),HL
		POP	HL
		EX	DE,HL
		PUSH	HL
		CALL	C5BFA
		LD	(CYOFF),DE
		POP	DE
		CALL	C5B0B			; negate DE
		CALL	C5C48
		PUSH	HL
		PUSH	DE
		LD	HL,(CNPNTS)
		LD	(CPCNT8),HL
		LD	DE,(CPCNT)
		OR	A
		SBC	HL,DE
		LD	(CPCNT),HL
		LD	HL,(CXOFF)
		CALL	NEGHL
		LD	(CXOFF),HL
		POP	DE
		POP	HL
		CALL	C5B0B			; negate DE

CPLOT4:
C5C48:		LD	A,04H
J5C4A:		PUSH	AF
		PUSH	HL
		PUSH	DE
		PUSH	HL
		PUSH	DE
		LD	DE,(CPCNT8)
		LD	HL,(CNPNTS)
		ADD	HL,HL
		ADD	HL,DE
		LD	(CPCNT8),HL
		LD	HL,(CPCNT)
		ADD	HL,DE
		EX	DE,HL
		LD	HL,(CSTCNT)
		RST	R_DCOMPR
		JR	Z,J5C80
		JR	NC,J5C70
		LD	HL,(CENCNT)
		RST	R_DCOMPR
		JR	Z,J5C78
		JR	NC,J5C90
J5C70:		LD	A,(CPLOTF)
		OR	A
		JR	NZ,J5C9A
		JR	J5C96

J5C78:		LD	A,(CLINEF)
		ADD	A,A
		JR	NC,J5C9A
		JR	J5C86

J5C80:		LD	A,(CLINEF)
		RRA
		JR	NC,J5C9A
J5C86:		POP	DE
		POP	HL
		CALL	C5CDC			; to absolute using GRPAC
		CALL	C5CCD			; draw line
		JR	J5CAA

J5C90:		LD	A,(CPLOTF)
		OR	A
		JR	Z,J5C9A
J5C96:		POP	DE
		POP	HL
		JR	J5CAA

J5C9A:		POP	DE
		POP	HL
		CALL	C5CDC			; to absolute using GRPAC
		CALL	SCALXY
		JR	NC,J5CAA
		CALL	MAPXYC
		CALL	SETC
J5CAA:		POP	DE
		POP	HL
		POP	AF
		DEC	A
		RET	Z
		PUSH	AF
		PUSH	DE
		LD	DE,(CXOFF)
		CALL	C5B0B			; negate DE
		LD	(CXOFF),HL
		EX	DE,HL
		POP	DE
		PUSH	HL
		LD	HL,(CYOFF)
		EX	DE,HL
		LD	(CYOFF),HL
		CALL	C5B0B			; negate DE
		POP	HL
		POP	AF
		JP	J5C4A

; Subroutine draw line
; Input:  GRPACX = X start, GRPACY = Y end, BC = X end, DE = Y end
CLINE2:
C5CCD:		LD	HL,(GRPACX)
		LD	(GXPOS),HL
		LD	HL,(GRPACY)
		LD	(GYPOS),HL
		JP	DOGRPH

; Subroutine to absolute using GRPAC
; Input:  HL = X offset, DE = Y offset
; Output: BC = X, DE = Y
GTABSC:
C5CDC:		PUSH	DE
		LD	DE,(GRPACX)
		ADD	HL,DE
		LD	B,H
		LD	C,L
		POP	DE
		LD	HL,(GRPACY)
		ADD	HL,DE
		EX	DE,HL
		RET

SCALEY:
C5CEB:		LD	HL,(ASPECT)
		LD	A,L
		OR	A
		JR	NZ,J5CF6
		OR	H
		RET	NZ
		EX	DE,HL
		RET

J5CF6:		LD	C,D
		LD	D,00H
		PUSH	AF
		CALL	C5D0A
		LD	E,80H
		ADD	HL,DE
		LD	E,C
		LD	C,H
		POP	AF
		CALL	C5D0A
		LD	E,C
		ADD	HL,DE
		EX	DE,HL
		RET

SCAL2:
C5D0A:		LD	B,8
		LD	HL,0
J5D0F:		ADD	HL,HL
		ADD	A,A
		JR	NC,J5D14
		ADD	HL,DE
J5D14:		DJNZ	J5D0F
		RET

CGTCNT:
C5D17:		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		RET	Z			; yep, quit
		RST	R_SYNCHR
		DEFB	","			; check for ,
		CP	','
		RET	Z
		PUSH	BC
		CALL	FRMEVL			; evaluate expression
		EX	(SP),HL
		PUSH	HL
		CALL	FRCSNG			; convert DAC to single real
		POP	BC
		LD	HL,DAC
		LD	A,(HL)
		OR	A
		JP	P,J5D3A
		AND	7FH
		LD	(HL),A
		LD	HL,CLINEF
		LD	A,(HL)
		OR	C
		LD	(HL),A
J5D3A:		LD	BC,1540H
		LD	DE,5591H
		CALL	SGNMUL			; single real muliply
		CALL	C5D63
		JP	Z,FCERR			; illegal function call
		CALL	PUSHF			; push DAC (single)
		LD	HL,(CNPNTS)
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		CALL	MAKINT			; put HL in DAC
		CALL	FRCSNG			; convert DAC to single real
		POP	BC
		POP	DE
		CALL	SGNMUL			; single real muliply
		CALL	FRCINT			; convert DAC to integer
		POP	DE
		EX	DE,HL
		RET

CMPONE:
C5D63:		LD	BC,1041H
		LD	DE,0000H
		CALL	FCOMP			; single real compare
		DEC	A
		RET

; Subroutine DRAW statement
DRAW:
C5D6E:		LD	A,(SCRMOD)
		CP	2			; in a graphic mode ?
		JP	C,FCERR			; nope, illegal function call
		LD	DE,I5D83		; macro language DRAW
		XOR	A
		LD	(DRWFLG),A		; clear DRWFLG
		LD	(MCLFLG),A		; graphic MCL
		JP	MACLNG			; macro language parser

; macro language DRAW command table
I5D83:		DEFB	'U'+128			; move up, optional value
		DEFW	C5DB1
		DEFB	'D'+128			; move down, optional value
		DEFW	C5DB4
		DEFB	'L'+128			; move left, optional value
		DEFW	C5DB9
		DEFB	'R'+128			; move right, optional value
		DEFW	C5DBC
		DEFB	'M'			; move
		DEFW	C5DD8
		DEFB	'E'+128			; move up+right, optional value
		DEFW	C5DCA
		DEFB	'F'+128			; move down+right, optional value
		DEFW	C5DC6
		DEFB	'G'+128			; move down+left, optional value
		DEFW	C5DD1
		DEFB	'H'+128			; move up+left, optional value
		DEFW	C5DC3
		DEFB	'A'+128			; change angle, optional value
		DEFW	C5E4E
		DEFB	'B'			; moves, but no plot
		DEFW	C5E46
		DEFB	'N'			; moves, but returns to start
		DEFW	C5E42
		DEFB	'X'			; macro substring
		DEFW	MCLXEQ
		DEFB	'C'+128			; change color, optional value
		DEFW	C5E87
		DEFB	'S'+128			; change scale, optional value
		DEFW	C5E59
		DEFB	0

; Subroutine DRAW: move up
; Input:  DE = X offset
DRUP:
C5DB1:		CALL	C5B0B			; negate DE (offset = - value)

; Subroutine DRAW: move down
; Input:  DE = X offset
DRDOWN:
C5DB4:		LD	BC,0			; Y offset = 0
		JR	J5DFF			; move

; Subroutine DRAW: move left
; Input:  DE = Y offset
DRLEFT:
C5DB9:		CALL	C5B0B			; negate DE (value = - value)

; Subroutine DRAW: move right
; Input:  DE = Y offset
DRIGHT:
C5DBC:		LD	B,D
		LD	C,E			; Y offset = value
		LD	DE,0			; X offset = 0
		JR	J5DFF			; move

; Subroutine DRAW: move up+left
DRWHHH:
C5DC3:		CALL	C5B0B			; negate DE (value = -value)

; Subroutine DRAW: move down+right
DRWFFF:
C5DC6:		LD	B,D
		LD	C,E			; Y offset = X offsset = value
		JR	J5DFF			; move

; Subroutine DRAW: move up+right
DRWEEE:
C5DCA:		LD	B,D
		LD	C,E			; Y offset = X offset = value
J5DCC:		CALL	C5B0B			; negate DE (X offset = - value)
		JR	J5DFF			; move

; Subroutine DRAW: move down+left
DRWGGG:
C5DD1:		CALL	C5B0B			; negate DE (value = -value)
		LD	B,D
		LD	C,E			; X offset = Y offset = -value
		JR	J5DCC			; negate X offset and move

; Subroutine DRAW: move
DMOVE:
C5DD8:		CALL	FETCHZ			; fetch macro string character, error if end of macro string
		LD	B,0			; assume sign
		CP	'+'
		JR	Z,J5DE6			; yep, it is
		CP	'-'
		JR	Z,J5DE6
		INC	B			; no sign specified
J5DE6:		LD	A,B
		PUSH	AF			; store sign flag
		CALL	DECFET			; to previous macro string character
		CALL	VALSCN			; parse numeric operand in macro string
		PUSH	DE			; store X offset
		CALL	FETCHZ			; fetch macro string character, error if end of macro string
		CP	','
		JP	NZ,FCERR		; illegal function call
		CALL	VALSCN			; parse numeric operand in macro string
		POP	BC			; restore X offset
		POP	AF			; restore sign flag
		OR	A			; sign specified ?
		JR	NZ,J5E22		; nope,

; move
J5DFF:		CALL	C5E66			; scale (Y offset)
		PUSH	DE			; store scaled Y offset
		LD	D,B
		LD	E,C			; X offset
		CALL	C5E66			; scale (X offset)
		EX	DE,HL
		POP	DE			; restore scaled Y offset
		LD	A,(DRWANG)
		RRA				; angle 1 or 3 ?
		JR	NC,J5E16		; nope,
		PUSH	AF			; store angle
		CALL	NEGHL			; negate (scaled X offset)
		EX	DE,HL
		POP	AF			; restore angle
J5E16:		RRA				; angle 2 or 3 ?
		JR	NC,J5E1F		; nope, angle 0 or 1
		CALL	NEGHL			; negate (scaled Y offset)
		CALL	C5B0B			; negate DE
J5E1F:		CALL	C5CDC			; to absolute using GRPAC
J5E22:		LD	A,(DRWFLG)
		ADD	A,A			; do not plot flag ?
		JR	C,J5E31			; yep, skip draw line
		PUSH	AF			; store flags
		PUSH	BC			; store X end
		PUSH	DE			; store Y end
		CALL	C5CCD			; draw line
		POP	DE			; restore Y end
		POP	BC			; restore X end
		POP	AF			; restore flag
J5E31:		ADD	A,A			; do not move flag ?
		JR	C,J5E3D			; yep, skip GRPAC update, clear DRWFLG and quit
		LD	(GRPACY),DE
		LD	H,B
		LD	L,C
		LD	(GRPACX),HL		; update GRPAC
J5E3D:		XOR	A
		LD	(DRWFLG),A		; clear DRWFLG
		RET

; Subroutine DRAW: moves, but returns to start
DNOMOV:
C5E42:		LD	A,40H			; no move flag
		JR	J5E48			; update DRWFLG

; Subroutine DRAW: moves, but no plot
DNOPLT:
C5E46:		LD	A,80H			; no plot flag
J5E48:		LD	HL,DRWFLG
		OR	(HL)
		LD	(HL),A
		RET

; Subroutine DRAW: change angle
DANGLE:
C5E4E:		JR	NC,C5E59		; no value specified, illegal function call
		LD	A,E
		CP	3+1			; valid angle value ?
		JR	NC,C5E59		; nope, illegal function call
		LD	(DRWANG),A		; update angle
		RET

; Subroutine DRAW: change scale factor
NCFCER:
C5E59:		JP	NC,FCERR		; no value specified, illegal function call
		LD	A,D
		OR	A			; value < 256 ?
		JP	NZ,FCERR		; nope, illegal function call
		LD	A,E
		LD	(DRWSCL),A		; update scale
		RET

; Subroutine scale
; Input:  DE = offset
; Output: DE = scaled offset
DSCLDE:
C5E66:		LD	A,(DRWSCL)
		OR	A			; scale = 0 ?
		RET	Z			; yep, quit (no scale)
		LD	HL,0
J5E6E:		ADD	HL,DE
		DEC	A
		JR	NZ,J5E6E
		EX	DE,HL
		LD	A,D
		ADD	A,A			; negative ?
		PUSH	AF			; store sign
		JR	NC,J5E79		; nope,
		DEC	DE
J5E79:		CALL	HLFDE			; divide by 2
		CALL	HLFDE			; divide by 2
		POP	AF			; restore sign
		RET	NC			; positive, quit
		LD	A,D
		OR	0C0H
		LD	D,A
		INC	DE
		RET

; Subroutine DRAW: change color
; Input:  DE = value
DCOLR:
C5E87:		JR	NC,C5E59		; no value specified, illegal function call
		LD	A,E			; color = value
		CALL	SETATR			; set color
		JP	C,FCERR			; error, illegal function call
		RET

; Subroutine check for in range
CHKRNG:
C5E91:		PUSH	HL			; store
		CALL	SCALXY
		JP	NC,FCERR		; illegal function call
		POP	HL			; restore
		RET

; ------------------------------------------------------------------------------
; BIPTRG.MAC
; BASIC DIMENSION & VARIABLE SEARCHING functions
; ------------------------------------------------------------------------------

		ALIGN	5E9AH

I5E9A:		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		RET	Z			; yep, quit
		RST	R_SYNCHR
		DEFB	","			; check for ,

; Subroutine DIM statement
DIM:
C5E9F:		LD	BC,I5E9A
		PUSH	BC			; 

; Subroutine locate array variable
		DEFB	0F6H			; OR 0AFH, trick to skip next instruction

; Subroutine locate variable
PTRGET:
C5EA4:		XOR	A			; not DIM
		LD	(DIMFLG),A
		LD	C,(HL)
PTRGT2:
J5EA9:		SYSHOOK	H_PTRG
		CALL	ISLET			; is current BASIC character a upcase letter ?
		JP	C,SNERR			; nope, syntax error
		XOR	A
		LD	B,A			; assume no 2nd variablename character
		RST	R_CHRGTR		; get next BASIC character
		JR	C,J5EBC			; digit, use as 2nd variablename character
		CALL	ISLET2			; is upcase letter character ?
		JR	C,J5EC5			; nope, may be a variabletype indicator
J5EBC:		LD	B,A			; 2nd variablename character
J5EBD:		RST	R_CHRGTR		; get next BASIC character
		JR	C,J5EBD			; digit, skip
		CALL	ISLET2			; is upcase letter character ?
		JR	NC,J5EBD		; yep, skip
J5EC5:		CP	26H
		JR	NC,J5EE0		; speedup for 26H-0FFH
		LD	DE,I5EEE
		PUSH	DE
		LD	D,2			; integer
		CP	'%'			; integer type indicator ?
		RET	Z			; yep, do not use default type
		INC	D			; string
		CP	'$'			; string type indicator ?
		RET	Z			; yep, do not use default type
		INC	D			; single real
		CP	'!'			; single real indicator ?
		RET	Z			; yep, do not use default type
		LD	D,8			; double real
		CP	'#'			; double real indicator ?
		RET	Z			; yep, do not use default type
		POP	AF
J5EE0:		LD	A,C
		AND	7FH			; clear b7 (for function variable)
		LD	E,A
		LD	D,0
		PUSH	HL
		LD	HL,DEFTBL-'A'
		ADD	HL,DE
		LD	D,(HL)			; default type
		POP	HL
		DEC	HL			; BASIC pointer back to compensate upcoming CHRGTR
I5EEE:		LD	A,D
		LD	(VALTYP),A		; set DAC type
		RST	R_CHRGTR		; get next BASIC character (type indicator)
		LD	A,(SUBFLG)
		DEC	A			; variable search flags
		JP	Z,J5FE8			; search for ERASE statement,
		JP	P,J5F08			; search for function variables or loop variables, do not check for subscript
		LD	A,(HL)
		SUB	'('
		JP	Z,J5FBA			; array variable
		SUB	'['-'('
		JP	Z,J5FBA			; array variable
J5F08:		XOR	A
		LD	(SUBFLG),A		; flag normal variable
		PUSH	HL			; store BASIC pointer
		LD	A,(NOFUNS)
		OR	A			; local function variables ?
		LD	(PRMFLG),A		; yep, search simple variables afterwards
		JR	Z,J5F52			; nope, continue with simple variables
		LD	HL,(PRMLEN)
		LD	DE,PARM1		; start of the local function variables
		ADD	HL,DE
		LD	(ARYTA2),HL		; end of the local function variables
		EX	DE,HL
		JR	J5F3A			; start search

J5F23:		LD	A,(DE)
		LD	L,A			; variable type (also the length)
		INC	DE
		LD	A,(DE)			; first variablename character
		INC	DE
		CP	C			; match ?
		JR	NZ,J5F36		; nope,
		LD	A,(VALTYP)
		CP	L			; correct variabletype ?
		JR	NZ,J5F36		; nope,
		LD	A,(DE)			; second variablename character
		CP	B			; match ?
		JP	Z,J5FA4			; variable found, quit
J5F36:		INC	DE
		LD	H,0
		ADD	HL,DE			; to next variable
J5F3A:		EX	DE,HL
		LD	A,(ARYTA2+0)
		CP	E			; end of area ?
		JP	NZ,J5F23		; nope, next variable
		LD	A,(ARYTA2+1)
		CP	D			; end of area ?
		JR	NZ,J5F23		; nope, next variable
		LD	A,(PRMFLG)
		OR	A			; in local function variable search ?
		JR	Z,SMKVAR		; nope, not found
		XOR	A
		LD	(PRMFLG),A		; now search the simple variables
J5F52:		LD	HL,(ARYTAB)
		LD	(ARYTA2),HL		; end of searcharea is the start of the array variable area
		LD	HL,(VARTAB)		; start of area is the start of the simple variable area
		JR	J5F3A			; start search

; Subroutine locate variable (search only)
PTRGTV:
C5F5D:		CALL	PTRGET			; locate variable
		RET

J5F61:		LD	D,A
		LD	E,A			; null pointer
		POP	BC
		EX	(SP),HL
		RET

SMKVAR:
J5F66:		POP	HL
		EX	(SP),HL			; call address
		PUSH	DE
		LD	DE,PTRGTV+3
		RST	R_DCOMPR		; called from VARPTR ?
		JR	Z,J5F61			; yep, return without creating a variable
		LD	DE,ISVAR+3
		RST	R_DCOMPR		; called from factor evaluator ?
		POP	DE
		JR	Z,J5FA7			; yep, return value 0
		EX	(SP),HL
		PUSH	HL
		PUSH	BC
		LD	A,(VALTYP)
		LD	C,A
		PUSH	BC
		LD	B,0			; size of variable
		INC	BC
		INC	BC
		INC	BC			; three bytes for housekeeping
		LD	HL,(STREND)
		PUSH	HL
		ADD	HL,BC
		POP	BC
		PUSH	HL
		CALL	BLTU			; check for enough stackspace and move data
		POP	HL
		LD	(STREND),HL
		LD	H,B
		LD	L,C
		LD	(ARYTAB),HL
J5F96:		DEC	HL
		LD	(HL),0
		RST	R_DCOMPR
		JR	NZ,J5F96		; clear variable
		POP	DE
		LD	(HL),E			; variable type
		INC	HL
		POP	DE
		LD	(HL),E
		INC	HL
		LD	(HL),D			; variable name
		EX	DE,HL
J5FA4:		INC	DE
		POP	HL
		RET

J5FA7:		LD	(DAC+0),A		; if single real or double real, DAC = 0
		LD	H,A
		LD	L,A
		LD	(DAC+2),HL		; if integer, DAC = 0
		RST	R_GETYPR		; get DAC type
		JR	NZ,J5FB8		; not a string, quit
		LD	HL,NULSTR
		LD	(DAC+2),HL		; empty string
J5FB8:		POP	HL			; restore BASIC pointer
		RET

; Subroutine locate array variable with subscript
J5FBA:		PUSH	HL
		LD	HL,(DIMFLG)
		EX	(SP),HL			; store DIMFLG
		LD	D,A			; index 0
J5FC0:		PUSH	DE
		PUSH	BC
		CALL	INTIDX			; skip basic char, evaluate word operand and check for 0-32767 range
		POP	BC
		POP	AF
		EX	DE,HL
		EX	(SP),HL
		PUSH	HL
		EX	DE,HL			; subscript on stack
		INC	A
		LD	D,A			; next index
		LD	A,(HL)
		CP	','			; more subscripts ?
		JP	Z,J5FC0			; yep, get next subscript
		CP	')'
		JR	Z,J5FDC
		CP	']'
		JP	NZ,SNERR		; nope, syntax error
J5FDC:		RST	R_CHRGTR		; get next BASIC character
		LD	(TEMP2),HL		; store BASIC pointer
		POP	HL
		LD	(DIMFLG),HL		; restore DIMFLG
		LD	E,0
		PUSH	DE
		DEFB	011H			; LD DE,xxxx, trick to skip next 2 instruction
J5FE8:		PUSH	HL
		PUSH	AF
		LD	HL,(ARYTAB)		; start of the array variable area
		DEFB	03EH			; LD A,xx, trick to skip next instruction
J5FEE:		ADD	HL,DE
		LD	DE,(STREND)
		RST	R_DCOMPR		; end of the array variable area ?
		JR	Z,J6023			; yep, create array
		LD	E,(HL)			; arraytype
		INC	HL
		LD	A,(HL)			; first name character
		INC	HL
		CP	C			; match ?
		JR	NZ,J6005		; nope, not found
		LD	A,(VALTYP)
		CP	E			; correct arraytype ?
		JR	NZ,J6005		; nope, not found
		LD	A,(HL)			; second name character
		CP	B			; match ?
J6005:		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)			; offset to next array
		INC	HL
		JR	NZ,J5FEE		; no match, next array
		LD	A,(DIMFLG)
		OR	A			; DIM statement ?
		JP	NZ,DDERR		; yep, redimensioned array error
		POP	AF
		LD	B,H
		LD	C,L
		JP	Z,POPHRT		; ERASE, restore BASIC pointer and quit
		SUB	(HL)			; dimension correct ?
		JP	Z,J607D			; yep,

BSERR:
J601D:
	IF OPTM = 0
		LD	DE,9			; ?? LD E,9 should be enough ??
	ELSE
		LD	E,9
	ENDIF

		JP	ERROR			; subscript out of range error

J6023:		LD	A,(VALTYP)
		LD	(HL),A
		INC	HL
		LD	E,A
		LD	D,0
		POP	AF
		JP	Z,FCERR			; ERASE, illegal function call
		LD	(HL),C
		INC	HL
		LD	(HL),B			; variablename
		INC	HL
		LD	C,A			; number of words
		CALL	GETSTK			; check if enough stackspace
		INC	HL
		INC	HL			; leave offset empty for now
		LD	(TEMP3),HL
		LD	(HL),C			; dimension
		INC	HL
		LD	A,(DIMFLG)
		RLA				; DIM statement ?
		LD	A,C
J6043:		LD	BC,11
		JR	NC,J604A		; nope, use a default of 11
		POP	BC			; subscript
		INC	BC
J604A:		LD	(HL),C
		PUSH	AF
		INC	HL
		LD	(HL),B
		INC	HL
		CALL	UMULT			; unsigned integer multiply
		POP	AF
		DEC	A
		JR	NZ,J6043		; next
		PUSH	AF
		LD	B,D
		LD	C,E
		EX	DE,HL
		ADD	HL,DE
		JP	C,OMERR			; out of memory
		CALL	REASON			; check if enough stackspace left
		LD	(STREND),HL		; new end of array area
J6064:		DEC	HL
		LD	(HL),0
		RST	R_DCOMPR
		JR	NZ,J6064		; clear array
		INC	BC
		LD	D,A
		LD	HL,(TEMP3)
		LD	E,(HL)
		EX	DE,HL
		ADD	HL,HL
		ADD	HL,BC
		EX	DE,HL
		DEC	HL
		DEC	HL
		LD	(HL),E
		INC	HL
		LD	(HL),D
		INC	HL
		POP	AF
		JR	C,J60AD
J607D:		LD	B,A
		LD	C,A
		LD	A,(HL)
		INC	HL
		DEFB	016H			; LD D,xx, trick to skip next instruction
J6082:		POP	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		EX	(SP),HL
		PUSH	AF
		RST	R_DCOMPR
		JP	NC,J601D		; subscript out of range
		CALL	UMULT			; unsigned integer multiply
		ADD	HL,DE
		POP	AF
		DEC	A
		LD	B,H
		LD	C,L
		JR	NZ,J6082
		LD	A,(VALTYP)
		LD	B,H
		LD	C,L
		ADD	HL,HL
		SUB	4
		JR	C,J60A5
		ADD	HL,HL
		JR	Z,J60AA
		ADD	HL,HL
J60A5:		OR	A
		JP	PO,J60AA
		ADD	HL,BC
J60AA:		POP	BC
		ADD	HL,BC
		EX	DE,HL
J60AD:		LD	HL,(TEMP2)
		RET

; ------------------------------------------------------------------------------
; BIPRTU.MAC
; BASIC PRINT USING
; ------------------------------------------------------------------------------

		ALIGN	60B1H

; Subroutine PRINT USING statement
PRINUS:
J60B1:		CALL	FRMCHK			; skip character and evaluate expression
		CALL	CHKSTR			; check if string
		RST	R_SYNCHR
		DEFB	";"			; check for ";"
		EX	DE,HL
		LD	HL,(DAC+2)		; formatstring descriptor
		JR	J60C7

J60BF:		LD	A,(FLGINP)
		OR	A			; READ statement ?
		JR	Z,J60D2			; nope,
		POP	DE
		EX	DE,HL
J60C7:		PUSH	HL
		XOR	A
		LD	(FLGINP),A		; clear READ statement flag
		INC	A
		PUSH	AF
		PUSH	DE
		LD	B,(HL)
		INC	B
		DEC	B			; empty format string ?
J60D2:		JP	Z,FCERR			; yep, illegal function call
		INC	HL
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A			; start of string
		JR	PRCCHR

; part of string handler
J60DC:		LD	E,B			; store length
		PUSH	HL			; store pointer
		LD	C,2			; at least 2 chars
J60E0:		LD	A,(HL)
		INC	HL
		CP	CHRFLN
		JP	Z,J6210			; end marker, print string
		CP	' '
		JR	NZ,J60EE		; not a space between markers, so no part string!
		INC	C
		DJNZ	J60E0			; next char, non left means no endmarker and no part string!
J60EE:		POP	HL			; restore pointer
		LD	B,E			; restore length
		LD	A,CHRFLN		; just print the CHRFLN char
J60F2:		CALL	PLSPRT
		RST	OUTDO
PRCCHR:
J60F6:		XOR	A
		LD	E,A
		LD	D,A
J60F9:		CALL	PLSPRT
		LD	D,A
		LD	A,(HL)
		INC	HL
		CP	'!'
		JP	Z,J620D			; "1 char of string" format char, handle
		CP	'#'
		JR	Z,J6144			; # numeric format char, handle
		CP	CHRVLN
		JP	Z,J6209			; "whole string" format char, handle
		DEC	B			; other format chars need at least 1 extra char
		JP	Z,J61F5			; not there, end it
		CP	'+'
		LD	A,08H
		JR	Z,J60F9			; +, set sign flag and continue
		DEC	HL
		LD	A,(HL)
		INC	HL
		CP	'.'
		JR	Z,J615E			; may be .# combi, check
		CP	CHRFLN
		JR	Z,J60DC			; "part of string" format char, handle
		CP	(HL)
		JR	NZ,J60F2		; not two equal chars, just print it
		CP	CHRCUR
		JR	Z,J613D			; double currency char, handle
		CP	'*'
		JR	NZ,J60F2

		; ** format
		INC	HL
		LD	A,B
		CP	02H
		JR	C,J6136			; none or only 1 char follows,
		LD	A,(HL)
		CP	CHRCUR
J6136:		LD	A,' '
		JR	NZ,J6141

		; **cur format
		DEC	B
		INC	E
		DEFB	0FEH			; CP xx, trick to skip next instruction
J613D:		XOR	A
		ADD	A,10H
		INC	HL
J6141:		INC	E
		ADD	A,D
		LD	D,A
J6144:		INC	E
		LD	C,00H
		DEC	B
		JR	Z,J6192
		LD	A,(HL)
		INC	HL
		CP	'.'
		JR	Z,J6169
		CP	'#'
		JR	Z,J6144
		CP	','
		JR	NZ,J6173
		LD	A,D
		OR	40H
		LD	D,A
		JR	J6144

J615E:		LD	A,(HL)
		CP	'#'
		LD	A,'.'
		JP	NZ,J60F2
		LD	C,01H
		INC	HL
J6169:		INC	C
		DEC	B
		JR	Z,J6192
		LD	A,(HL)
		INC	HL
		CP	'#'
		JR	Z,J6169
J6173:		PUSH	DE
		LD	DE,I6190
		PUSH	DE
		LD	D,H
		LD	E,L
		CP	5EH
		RET	NZ
		CP	(HL)
		RET	NZ
		INC	HL
		CP	(HL)
		RET	NZ
		INC	HL
		CP	(HL)
		RET	NZ
		INC	HL
		LD	A,B
		SUB	04H
		RET	C
		POP	DE
		POP	DE
		LD	B,A
		INC	D
		INC	HL
		DEFB	0CAH			; JP Z,xxxx trick to skip next 2 instructions
I6190:		EX	DE,HL
		POP	DE
J6192:		LD	A,D
		DEC	HL
		INC	E
		AND	08H
		JR	NZ,J61AE
		DEC	E
		LD	A,B
		OR	A
		JR	Z,J61AE
		LD	A,(HL)
		SUB	'-'
		JR	Z,J61A9
		CP	0FEH
		JR	NZ,J61AE
		LD	A,08H
J61A9:		ADD	A,04H
		ADD	A,D
		LD	D,A
		DEC	B
J61AE:		POP	HL
		POP	AF
		JR	Z,J61FE
		PUSH	BC
		PUSH	DE
		CALL	FRMEVL			; evaluate expression
		POP	DE
		POP	BC
		PUSH	BC
		PUSH	HL
		LD	B,E
		LD	A,B
		ADD	A,C
		CP	19H
		JP	NC,FCERR		; illegal function call
		LD	A,D
		OR	80H
		CALL	PUFOUT			; convert DAC to text, formatted
		CALL	STROUT			; message to interpreter output
J61CC:		POP	HL
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		SCF
		JR	Z,J61DD			; yep,
		LD	(FLGINP),A		; update READ statement flag
		CP	';'
		JR	Z,J61DC
		RST	R_SYNCHR
		DEFB	","			; check for ,
		DEFB	006H			; LD B,xx, trick to skip next instruction
J61DC:		RST	R_CHRGTR		; get next BASIC character
J61DD:		POP	BC
		EX	DE,HL
		POP	HL
		PUSH	HL
		PUSH	AF
		PUSH	DE
		LD	A,(HL)
		SUB	B
		INC	HL
		LD	D,00H
		LD	E,A
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		ADD	HL,DE
		LD	A,B
		OR	A
		JP	NZ,PRCCHR
		JR	FINUSI

J61F5:		CALL	PLSPRT
		RST	OUTDO			; char to interpreter output
FINUSI:
J61F9:		POP	HL
		POP	AF
		JP	NZ,J60BF
J61FE:		CALL	C,CRDO			; newline to interpreter output
		EX	(SP),HL
		CALL	FRETM2			; free temporary string (descriptor in HL)
		POP	HL
		JP	FINPRT			; end BASIC interpreter redirect

; handle char
J6209:		LD	C,00H
		JR	J6211

; handle ! char
J620D:		LD	C,01H
		DEFB	03EH			; LD A,xx, trick to skip next instruction
J6210:		POP	AF
J6211:		DEC	B
		CALL	PLSPRT
		POP	HL
		POP	AF
		JR	Z,J61FE
		PUSH	BC
		CALL	FRMEVL			; evaluate expression
		CALL	CHKSTR			; check if string
		POP	BC
		PUSH	BC
		PUSH	HL
		LD	HL,(DAC+2)
		LD	B,C
		LD	C,00H
		LD	A,B
		PUSH	AF
		OR	A
		CALL	NZ,LEFTUS
		CALL	STRPRT			; free string and string to interpreter output
		LD	HL,(DAC+2)
		POP	AF
		OR	A
		JP	Z,J61CC
		SUB	(HL)
		LD	B,A
		LD	A,' '
		INC	B
J623F:		DEC	B
		JP	Z,J61CC
		RST	OUTDO			; space to interpreter output
		JR	J623F

PLSPRT:
C6246:		PUSH	AF
		LD	A,D
		OR	A
		LD	A,'+'
		CALL	NZ,OUTDO		; yep, "+" to interpreter output
		POP	AF
		RET

; ------------------------------------------------------------------------------
; BIMSIC.MAC
; BASIC interpreter miscellaneous functions
; ------------------------------------------------------------------------------

		ALIGN	6250H

; Subroutine check for enough stackspace and move data
BLTU:
C6250:		CALL	REASON			; check if enough stackspace left

; Subroutine move data
; Input:  BC = source end, DE = source start, HL = destination end
; Output: BC = destination start, DE = source start, HL = source start
BLTUC:
C6253:		PUSH	BC			; store source end
		EX	(SP),HL			; store destination end, restore source end
		POP	BC			; restore destination end
J6256:		RST	R_DCOMPR		; source start reached ?
		LD	A,(HL)			; byte from source
		LD	(BC),A			; byte to destination
		RET	Z			; yep, quit
		DEC	BC			; update destination
		DEC	HL			; update source
		JR	J6256			; next

; Subroutine check if enough stackspace
; Input:  C = number of words
GETSTK:
C625E:		PUSH	HL
		LD	HL,(STREND)
		LD	B,0
		ADD	HL,BC
		ADD	HL,BC
		DEFB	03EH			; LD A,xx, trick to skip next instruction

; Subroutine check if enough stackspace left
; Input:  HL = end of area to use
REASON:
C6267:		PUSH	HL
	IFDEF MSX1
		LD	A,088H			; Low byte -120
	ELSE
		LD	A,060H			; MSX2: low byte -160 (?)
	ENDIF
		SUB	L
		LD	L,A
		LD	A,0FFH			; High byte -120 / -160
		SBC	A,H
		LD	H,A
		JR	C,OMERR			; out of memory
		ADD	HL,SP
		POP	HL
		RET	C

OMERR:
J6275:		CALL	LINKER			; setup BASIC linelinks
		LD	HL,(STKTOP)
		DEC	HL
		DEC	HL
		LD	(SAVSTK),HL

	IF OPTM = 0
		LD	DE,7			; ?? LD E,7 should be enough ??
	ELSE
		LD	E,7
	ENDIF

		JP	ERROR			; out of memory error

; Subroutine NEW statement
SCRATH:
C6286:		RET	NZ			; not end of statement, quit (which will generate a syntax error)

; Subroutine clear BASIC program
SCRTCH:
C6287:		LD	HL,(TXTTAB)
		CALL	C6439			; trace off
		LD	(AUTFLG),A		; quit auto line number mode
		LD	(PTRFLG),A		; output to screen
		LD	(HL),A
		INC	HL
		LD	(HL),A			; endpointer at basic text (no program text)
		INC	HL
		LD	(VARTAB),HL		; initialize start of basic variable area

; Subroutine initialize interpreter, BASIC pointer at start of program
RUNC:
C629A:		SYSHOOK	H_RUNC
		LD	HL,(TXTTAB)
		DEC	HL

; Subroutine initialize interpreter
; Input:  HL = BASIC pointer
CLEARC:
C62A1:		SYSHOOK	H_CLEA
		LD	(TEMP),HL		; store BASIC pointer

; Subroutine initialize interpreter, restore BASIC pointer from TEMP
; Remark: CLEAR2 is not the offical name
CLEAR2:
C62A7:		CALL	INITRP			; clear trap variables
		LD	B,26
		LD	HL,DEFTBL
		SYSHOOK	H_LOPD
J62B2:		LD	(HL),8
		INC	HL
		DJNZ	J62B2			; default type for variables is double real
		CALL	RNDINI			; initialize RNDX
		XOR	A
		LD	(ONEFLG),A		; not in ERROR handling routine
		LD	L,A
		LD	H,A
		LD	(ONELIN),HL		; no "on error" handler
		LD	(OLDTXT),HL		; CONT statement not possible
		LD	HL,(MEMSIZ)
		LD	(FRETOP),HL		; empty stringspace
		CALL	RESTOR			; restore statement
		LD	HL,(VARTAB)
		LD	(ARYTAB),HL		; begin of arrayvariables space = begin of variable space (no variables) 
		LD	(STREND),HL		; end of basicprogram workarea = begin of variable space (no array variables)
		CALL	CLSALL			; close all I/O channels
		LD	A,(NLONLY)
		AND	01H			; loading BASIC program ?
		JR	NZ,STKINI		; yep, skip
		LD	(NLONLY),A		; reset leave I/O channels open flag

; Subroutine initialize stack
STKINI:
C62E5:		POP	BC			; get return address from stack
		LD	HL,(STKTOP)
		DEC	HL
		DEC	HL
		LD	(SAVSTK),HL
		INC	HL
		INC	HL

; Subroutine reinitialize stack, reset interpreter output, clear FN vars, clear variable search flag
; Input:  HL = top of new stack
STKERR:
J62F0:		SYSHOOK	H_STKE
		LD	SP,HL			; initialize stackpointer
		LD	HL,TEMPST
		LD	(TEMPPT),HL		; clear string descriptor stack
		CALL	FINLPT			; end printer output
		CALL	FINPRT			; end BASIC interpreter redirect
		XOR	A
		LD	H,A
		LD	L,A
		LD	(PRMLEN),HL
		LD	(NOFUNS),A
		LD	(PRMLN2),HL
		LD	(FUNACT),HL
		LD	(PRMSTK),HL		; clear FN variables
		LD	(SUBFLG),A		; clear variable search flag
		PUSH	HL			; terminator zero word for FOR and GOSUB
		PUSH	BC			; return address back on stack
GTMPRT:
C6317:		LD	HL,(TEMP)
		RET

; Subroutine enable trap
; Input:  HL = pointer to trap block
ONTRP:
J631B:		DI
		LD	A,(HL)
		AND	00000100B		; keep trap occured flag
		OR	00000001B		; trap enabled
		CP	(HL)			; trap already enabled AND not paused ?
		LD	(HL),A
		JR	Z,J6329			; yep, quit
		AND	00000100B		; trap occured ?
		JR	NZ,SETTP2		; yep, increase trap counter and quit
J6329:		EI
		RET

; Subroutine disable trap
OFFTRP:
J632B:		DI
		LD	A,(HL)
		LD	(HL),0			; clear trap occured, trap not paused, trap disabled
		JR	J6338			; decrease trap counter if needed

; Subroutine pause trap
STPTRP:
C6331:		DI
		LD	A,(HL)
		PUSH	AF
		OR	00000010B
		LD	(HL),A			; trap paused
		POP	AF
J6338:		XOR	00000101B		; trap occured AND trap was not paused AND trap enabled ?
		JR	Z,J6362			; yep, decrease trap counter
		EI
		RET

; Subroutine unpause trap
RSTTRP:
C633E:		DI
		LD	A,(HL)
		AND	00000101B		; keep trap occured and trap enabled flags
		CP	(HL)			; was trap paused ?
		LD	(HL),A			; trap not paused anymore
		JR	NZ,SETCHK		; yep, update trap counter if trap occured (during pause)
		EI
		RET

SETCHK:
J6348:		XOR	00000101B		; trap occured AND trap enabled ?
		JR	Z,SETTP2		; yep, increase trap counter and quit
		EI
		RET

; Subroutine increase trap counter
; Unused Code: Not called from anywhere
SETTRP:
Q634E:		DI

; Subroutine increase trap counter
SETTP2:
J634F:		LD	A,(ONGSBF)
		INC	A
		LD	(ONGSBF),A
		EI
		RET

; Subroutine acknowledge trap
FRETRP:
C6358:		DI
		LD	A,(HL)
		AND	00000011B		; keep trap paused and trap enabled flags
		CP	(HL)			; trap occured ?
		LD	(HL),A			; clear trap occured
		JR	NZ,J6362		; yep, decrease trap counter
J6360:		EI
		RET

; Subroutine decrease trap counter
FRETP2:
J6362:		LD	A,(ONGSBF)
		SUB	1
		JR	C,J6360			; already zero, quit
		LD	(ONGSBF),A
		EI
		RET

; Subroutine clear trap variables
INITRP:
C636E:		LD	HL,TRPTBL
		LD	B,26
		XOR	A
J6374:		LD	(HL),A			; clear trap flags
		INC	HL
		LD	(HL),A
		INC	HL
		LD	(HL),A			; clear trap handler
		INC	HL
		DJNZ	J6374
		LD	HL,FNKFLG
		LD	B,10
J6381:		LD	(HL),A
		INC	HL
		DJNZ	J6381			; clear function key has trap enabled
		LD	(ONGSBF),A		; clear trap counter
		RET

; Subroutine handle trap
; Input:  HL = BASIC pointer
GOTRP:
C6389:		LD	A,(ONEFLG)
		OR	A			; in ERROR handling routine ?
		RET	NZ			; yep, quit
		PUSH	HL			; store BASIC pointer
		LD	HL,(CURLIN)
		LD	A,H
		AND	L
		INC	A			; interpreter in direct mode ?
		JR	Z,J63A6			; yep, quit
		LD	HL,TRPTBL
		LD	B,26
J639C:		LD	A,(HL)
		CP	00000101B		; trap occured AND trap not paused AND trap enabled ?
		JR	Z,J63A8			; yep, handle trap
J63A1:		INC	HL
		INC	HL
		INC	HL
		DJNZ	J639C			; next trap
J63A6:		POP	HL			; restore BASIC pointer
		RET

J63A8:		PUSH	BC
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)			; pointer to trap handler
		DEC	HL
		DEC	HL
		LD	A,D
		OR	E			; has this trap a handler ?
		POP	BC
		JR	Z,J63A1			; nope, next trap
		PUSH	DE			; store pointer to trap handler
		PUSH	HL			; store pointer to trap entry
		CALL	C6358			; acknowledge trap
		CALL	C6331			; pause trap
		LD	C,3
		CALL	GETSTK			; check if enough stackspace for 3 words
		POP	BC			; restore pointer to trap entry
		POP	DE			; restore pointer to trap handler
		POP	HL			; restore BASIC pointer
		EX	(SP),HL			; restore return address, store BASIC pointer
		POP	HL			; restore BASIC pointer
		JP	GOSUBT			; GOSUB trap handler

; Subroutine RESTORE statement
RESTOR:
C63C9:		EX	DE,HL
		LD	HL,(TXTTAB)
		JR	Z,J63DD			; end of statement, use start of BASIC program
		EX	DE,HL
		CALL	LINGET			; collect line number
		PUSH	HL
		CALL	FNDLIN			; search line number from start of program
		LD	H,B
		LD	L,C
		POP	DE
		JP	NC,USERR		; not found, undefined line number error
J63DD:		DEC	HL
RESFIN:
J63DE:		LD	(DATPTR),HL		; new DATA pointer
		EX	DE,HL
		RET

; Subroutine STOP statement (normal STOP statement or STOP <trap device> statement)
STOPS:
C63E3:		JP	NZ,STOPT		; not end of statement, STOP statement (trap)

; Subroutine STOP statement
STOP:
C63E6:		RET	NZ			; ?? because it is always called with Zx set, RET NZ could be removed
		INC	A			; set STOP flag (indicate STOP statement)
		JR	CONSTP

; Subroutine END statement
ENDS:
C63EA:		RET	NZ			; not end of statement, quit (which generates syntax error)
		XOR	A
		LD	(ONEFLG),A		; not in ERROR handler routine anymore
		PUSH	AF
		CALL	Z,CLSALL		; close all I/O channels (?? should be CALL CLSALL ??)
		POP	AF			; reset STOP flag (indicate END statement)

CONSTP:
J63F4:		LD	(SAVTXT),HL		; store BASIC pointer
		LD	HL,TEMPST
		LD	(TEMPPT),HL		; clear string descriptor stack
		DEFB	021H			; LD HL,xxxx, trick to skip next instruction

STPEND:
J63FE:		OR	0FFH			; flag aborted input

		POP	BC

ENDCON:
J6401:		LD	HL,(CURLIN)
		PUSH	HL			; store current line number
		PUSH	AF			; store flag
		LD	A,L
		AND	H
		INC	A			; interpreter in direct mode ?
		JR	Z,J6414			; yep, skip updating OLDLIN and OLDTXT
		LD	(OLDLIN),HL		; update line number for CONT
		LD	HL,(SAVTXT)		; restore BASIC pointer
		LD	(OLDTXT),HL		; update BASIC pointer for CONT
J6414:		CALL	FINLPT			; end printeroutput
		CALL	CRDONZ			; fresh line to interpreter output
		POP	AF			; END statement ?
		LD	HL,BRKTXT
		JP	NZ,ERRFIN		; nope, aborted input or STOP,
		JP	STPRDY			; END, ok and mainloop (+POP)

; Subroutine CONT statement
CONT:
C6424:		LD	HL,(OLDTXT)		; BASIC pointer = BASIC pointer for CONT
		LD	A,H
		OR	L			; is pointer valid ?

	IF OPTM = 0
		LD	DE,17			; ?? LD E,17 should be enough ??
	ELSE
		LD	E,17
	ENDIF

		JP	Z,ERROR			; nope, can not continue error
		LD	DE,(OLDLIN)
		LD	(CURLIN),DE		; update current line number = line number for CONT
		RET

; Subroutine TRON
TON:
C6438:		DEFB	03EH			; LD A,0AFH, trick to skip next instruction

; Subroutine TROFF
TOFF:
C6439:		XOR	A
		LD	(TRCFLG),A
		RET

; Subroutine SWAP statement
SWAP:
C643E:		CALL	PTRGET			; locate variable
		PUSH	DE
		PUSH	HL
		LD	HL,SWPTMP
		CALL	VMOVE			; HL = DE (valtyp), store 1st variable in SWPTMP
		LD	HL,(ARYTAB)
		EX	(SP),HL			; store start of BASIC array variables, restore BASIC pointer
		RST	R_GETYPR		; get DAC type
		PUSH	AF			; store type
		RST	R_SYNCHR
		DEFB	","			; check for ,
		CALL	PTRGET			; locate variable
		POP	AF
		LD	B,A
		RST	R_GETYPR
		CP	B
		JP	NZ,TMERR		; not the same type, type mismatch error
		EX	(SP),HL
		EX	DE,HL
		PUSH	HL
		LD	HL,(ARYTAB)		; has (ARYTAB) changed (2nd variable does not exists) ?
		RST	R_DCOMPR
		JR	NZ,J6474		; yep, illegal function call
		POP	DE			; pointer to 2nd variable
		POP	HL
		EX	(SP),HL			; store BASIC pointer, get pointer to 1st variable
		PUSH	DE
		CALL	VMOVE			; HL = DE (valtyp), 1st variable = 2nd variable
		POP	HL
		LD	DE,SWPTMP
		CALL	VMOVE			; HL = DE (valtyp), 2nd variable = SWPTMP
		POP	HL
		RET

J6474:		JP	FCERR			; illegal function call

; Subroutine ERASE statement
ERASE:
C6477:		LD	A,1
		LD	(SUBFLG),A		; variable search flag = arrayvariable
		CALL	PTRGET			; locate variable
		PUSH	HL
		LD	(SUBFLG),A
		LD	H,B
		LD	L,C
		DEC	BC
		DEC	BC
		DEC	BC
		DEC	BC
		DEC	BC
		ADD	HL,DE
		EX	DE,HL
		LD	HL,(STREND)
J648F:		RST	R_DCOMPR
		LD	A,(DE)
		LD	(BC),A
		INC	DE
		INC	BC
		JR	NZ,J648F
		DEC	BC
		LD	H,B
		LD	L,C
		LD	(STREND),HL
		POP	HL
		LD	A,(HL)
		CP	','
		RET	NZ
		RST	R_CHRGTR		; get next BASIC character
		JR	C6477

; Subroutine 
; Unused Code: Not called from anywhere, leftover from a early Microsoft BASIC ?
Q64A4:		POP	AF
		POP	HL
		RET

; Subroutine is current BASIC character a upcase letter ?
ISLET:
C64A7:		LD	A,(HL)

; Subroutine is upcase letter character ?
ISLET2:
C64A8:		CP	'A'
		RET	C
		CP	'Z'+1
		CCF
		RET

; Subroutine CLEAR statement
CLEAR:
C64AF:		JP	Z,CLEARC		; end of statement, initialize interpreter and quit
		CALL	INTID2			; evaluate word operand and check for 0-32767 range
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		PUSH	HL
		LD	HL,(HIMEM)
		LD	B,H
		LD	C,L			; current top BASIC memory
		LD	HL,(MEMSIZ)		; current top of string heap
		JR	Z,J64EC			; end of statement (no new top specified), use current
		POP	HL
		RST	R_SYNCHR
		DEFB	","			; check for ,
		PUSH	DE
		CALL	GETUIN			; evaluate address operand
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		JP	NZ,SNERR		; nope, syntax error
		EX	(SP),HL
		EX	DE,HL
		LD	A,H
		AND	A			; top BASIC memory in 8000H-0FFFFH range ?
		JP	P,FCERR			; nope, illegal function call
		PUSH	DE
		LD	DE,VARWRK+1
		RST	R_DCOMPR		; top BASIC memory <= VARWRK ?
		JP	NC,FCERR		; nope, illegal function call
		POP	DE			; size of string heap
		PUSH	HL
		LD	BC,-(256+9+2)
		LD	A,(MAXFIL)
J64E5:		ADD	HL,BC
		DEC	A
	IFDEF MSX1
		JP	P,J64E5			; next I/O channel
	ELSE
		JP	J7A1F			; patch: next I/O channel
	ENDIF
J64EA:		POP	BC			; new top address
		DEC	HL			; new top of string heap
J64EC:		LD	A,L
		SUB	E
		LD	E,A
		LD	A,H
		SBC	A,D
		LD	D,A			; bottom of string heap
		JP	C,OMERR			; <0, out of memory
		PUSH	HL
		LD	HL,(VARTAB)
		PUSH	BC
		LD	BC,160
		ADD	HL,BC
		POP	BC
		RST	R_DCOMPR		; enough space for stack ?
		JP	NC,OMERR		; nope, out of memory
		EX	DE,HL
		LD	(STKTOP),HL		; new start Z80 stack
		LD	H,B
		LD	L,C
		LD	(HIMEM),HL		; new top BASIC memory
		POP	HL
		LD	(MEMSIZ),HL		; new top of string heap
		POP	HL			; restore BASIC pointer
		CALL	CLEARC			; initialize interpreter
		LD	A,(MAXFIL)		; number of I/O channels
		CALL	ALCFIL			; allocate I/O channels
		LD	HL,(TEMP)		; restore BASIC pointer
		JP	NEWSTT			; execute new statement

; Subroutine DE=HL-DE
; Unused Code: Not called from anywhere, leftover from a early Microsoft BASIC ?
SUBDE:
Q6520:		LD	A,L
		SUB	E
		LD	E,A
		LD	A,H
		SBC	A,D
		LD	D,A
		RET

; Subroutine NEXT statement
NEXT:
C6527:		LD	DE,0			; any loop variable

; Subroutine NEXT with explicit loop variable
C652A:		CALL	NZ,PTRGET		; not end of statement, locate variable
		LD	(TEMP),HL		; store BASIC pointer
		CALL	FNDFOR			; search FOR block on stack (skip 2 words)
		JP	NZ,NFERR		; not found,
		LD	SP,HL
		PUSH	DE
		LD	A,(HL)
		PUSH	AF
		INC	HL
		PUSH	DE
		LD	A,(HL)
		INC	HL
		OR	A
		JP	M,J656B
		DEC	A
		JR	NZ,J6549
		LD	BC,8
		ADD	HL,BC
J6549:		ADD	A,4
		LD	(VALTYP),A
		CALL	VMOVFM			; DAC = HL
		EX	DE,HL
		EX	(SP),HL
		PUSH	HL
		RST	R_GETYPR		; get DAC type
		JR	NC,J65A5		; double real,
		CALL	MOVRMI			; load from HL
		CALL	SGNADD			; single real addition
		POP	HL
		CALL	MOVMF			; DAC = (single)
		POP	HL
		CALL	MOVRM			; load from HL
		PUSH	HL
		CALL	FCOMP			; single real compare
		JR	J6594

J656B:		LD	BC,000CH
		ADD	HL,BC
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		INC	HL
		EX	(SP),HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		PUSH	HL
		LD	L,C
		LD	H,B
		CALL	IADD			; add integer
		LD	A,(VALTYP)
		CP	2
		JP	NZ,OVERR		; overflow error
		EX	DE,HL
		POP	HL
		LD	(HL),D
		DEC	HL
		LD	(HL),E
		POP	HL
		PUSH	DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		EX	(SP),HL
		CALL	ICOMP			; compare integer
J6594:		POP	HL
		POP	BC
		SUB	B
		CALL	MOVRM			; load from HL
		JR	Z,J65B6
		EX	DE,HL
		LD	(CURLIN),HL
		LD	L,C
		LD	H,B
		JP	NXTCON

J65A5:		CALL	DECADM			; double real addition (HL)
		POP	HL
		CALL	VMOVMF			; HL = DAC
		POP	HL
		CALL	VMOVAM			; ARG = HL
		PUSH	DE
		CALL	XDCOMP			; compare double real
		JR	J6594

J65B6:		LD	SP,HL
		LD	(SAVSTK),HL
		EX	DE,HL
		LD	HL,(TEMP)		; restore BASIC pointer
		LD	A,(HL)
		CP	','			; parameter seperator ?
		JP	NZ,NEWSTT		; nope, execute new statement
		RST	R_CHRGTR		; get next BASIC character
		CALL	C652A			; NEXT with explicit loop variable


; ------------------------------------------------------------------------------
; BISTRS.MAC
; BASIC STRING functions
; ------------------------------------------------------------------------------

		ALIGN	65C8H

STRCMP:
C65C8:		CALL	FRESTR			; check for string and free temporary string descriptor and string (string1)
		LD	A,(HL)			; size of string1
		INC	HL
		LD	C,(HL)
		INC	HL
		LD	B,(HL)			; pointer to string1
		POP	DE			; restore pointer to temporary string descriptor (string2)
		PUSH	BC			; store pointer to string1
		PUSH	AF			; store size of string1
		CALL	FRETMP			; free temporary string (descriptor in DE) (string2)
		POP	AF			; restore size of string1
		LD	D,A			; store size of string1
		LD	E,(HL)			; size of string2
		INC	HL
		LD	C,(HL)
		INC	HL
		LD	B,(HL)			; pointer to string2
		POP	HL
J65DE:		LD	A,E
		OR	D			; end of both strings ?
		RET	Z			; yep, quit (A=0, equal)
		LD	A,D
		SUB	1			; end of string1 ?
		RET	C			; yep, quit (A=FF, less)
		XOR	A
		CP	E			; end of string2 ?
		INC	A
		RET	NC			; yep, quit (A=1, bigger)
		DEC	D
		DEC	E			; adjust counter
		LD	A,(BC)			; character string2
		INC	BC
		CP	(HL)			; equal to character string1 ?
		INC	HL
		JR	Z,J65DE			; yep, next
		CCF				; flip Cx
		JP	SIGNS			; set compare value

; Subroutine OCTS function
OCTS:
C65F5:		CALL	FOUTO			; convert integer to octal text
		JR	J6607

; Subroutine HEXS function
HEXS:
C65FA:		CALL	FOUTH			; convert integer to hexadecimal text
		JR	J6607

; Subroutine BINS function
BINS:
C65FF:		CALL	FOUTB			; convert integer to binary text
		JR	J6607

; Subroutine STRS function
STRS:
C6604:		CALL	FOUT			; convert DAC to text, unformatted
J6607:		CALL	STRLIT			; get string literal and create temporary string descriptor
		CALL	FREDAC			; free temporary string descriptor and string (DAC)
		LD	BC,J6825
		PUSH	BC			; copy string to new temporary string, temporary stringdescriptor to heap and quit

; Subroutine copy string to new temporary string
; Input:  HL = source string descriptor
STRCPY:
C6611:		LD	A,(HL)			; size of string
		INC	HL
		PUSH	HL			; store pointer in string descriptor
		CALL	GETSPA			; allocate string space
		POP	HL			; restore pointer in string descriptor
		LD	C,(HL)
		INC	HL
		LD	B,(HL)			; pointer to string
		CALL	STRAD2			; create temporary string descriptor
		PUSH	HL			; store pointer to temporary string descriptor
		LD	L,A			; size of string
		CALL	C67C7			; copy string
		POP	DE			; restore pointer to temporary string descriptor
		RET

; Subroutine allocate temporary string of 1 char
; Output: DE = pointer to string, HL = pointer to temporary string descriptor
STRIN1:
C6625:		LD	A,1

; Subroutine allocate temporary string
; Input:  A = size of string
; Output: DE = pointer to string, HL = pointer to temporary string descriptor
STRINI:
C6627:		CALL	GETSPA			; allocate string space

; Subroutine create temporary string descriptor
; Input:  A = size of string, DE = pointer to string
; Output: HL = pointer to temporary string descriptor
STRAD2:
C662A:		LD	HL,DSCTMP
		PUSH	HL
		LD	(HL),A
		INC	HL
		LD	(HL),E
		INC	HL
		LD	(HL),D
		POP	HL
		RET

; Subroutine get string literal and create temporary string descriptor
; Input:  HL = pointer to string literal-1
STRLIT:
C6635:		DEC	HL

; Subroutine get string literal and create temporary string descriptor
; Input:  HL = pointer to string literal
STRLTI:
C6636:		LD	B,'"'

; Subroutine get string literal (with specified endmarker) and create temporary string descriptor
STRLT3:
C6638:		LD	D,B

; Subroutine get string literal (with specified endmarkers) and create temporary string descriptor
; Input:  HL = pointer to string to be analysed, B = end character 1, D = end character 2
STRLT2:
C6639:		PUSH	HL			; store pointer to string
		LD	C,-1			; size = -1
J663C:		INC	HL
		LD	A,(HL)
		INC	C
		OR	A			; end of BASIC line/string ?
		JR	Z,J6648			; yep,
		CP	D			; end character 1 ?
		JR	Z,J6648			; yep,
		CP	B			; end character 2 ?
		JR	NZ,J663C		; nope, skip
J6648:		CP	'"'			; string marker ?
		CALL	Z,CHRGTR		; yep, get next BASIC character
		EX	(SP),HL			; store pointer in string (restored later), restore pointer to string
		INC	HL
		EX	DE,HL
		LD	A,C			; size of string
		CALL	STRAD2			; create temporary string descriptor

; Subroutine push temporary sring descriptor to temporary string descriptor heap
PUTNEW:
J6654:		LD	DE,DSCTMP		; string descriptor = temporary string descriptor
		DEFB	03EH			; LD A,xx, trick to skip next instruction

; Subroutine push string descriptor to temporary string descriptor heap
; Input:  DE = pointer to string descriptor
PUTTMP:
C6658:		PUSH	DE			; store pointer to string descriptor
		LD	HL,(TEMPPT)
		LD	(DAC+2),HL
		LD	A,3
		LD	(VALTYP),A		; DAC = new string descriptor
		CALL	VMOVE			; HL = DE (valtyp), copy temporary string descriptor to temporary string descriptor heap
		LD	DE,TEMPST+30+3
		RST	R_DCOMPR		; temporary descriptor heap full ?
		LD	(TEMPPT),HL
		POP	HL			; restore pointer to string descriptor
		LD	A,(HL)			; size of string
		RET	NZ
STERR:
	IF OPTM = 0
		LD	DE,16			; ?? LD E,16 should be enough ??
	ELSE
		LD	E,16
	ENDIF

		JP	ERROR			; yep, string formula too complex error

; Subroutine skip first character, message to interpreter output

STROUI:
C6677:		INC	HL

; Subroutine message to interpreter output
STROUT:
C6678:		CALL	STRLIT			; get string literal and create temporary string descriptor

; Subroutine free string and string to interpreter output
STRPRT:
C667B:		CALL	FREDAC			; free temporary string descriptor and string (DAC)
		CALL	GETBCD			; get size and pointer to string
		INC	D			; size+1
J6682:		DEC	D
		RET	Z
		LD	A,(BC)
		RST	OUTDO			; char to interpreter output
		CP	0DH			; CR ?
		CALL	Z,CRFIN			; yep, interpreter output pos = 0
		INC	BC
		JR	J6682

; Subroutine allocate string space
; Input:  A = size of string
; Output: DE = pointer to string space
GETSPA:
C668E:		OR	A			; because size<>0, Zx is reset (no garbage collect done)
		DEFB	00EH			; LD C,xx, trick to skip next instruction

C6690:		POP	AF			; restore garbage collect flag
		PUSH	AF			; store garbage collect flag
		LD	HL,(STKTOP)		; end of string heap (= top of BASIC stack)
		EX	DE,HL
		LD	HL,(FRETOP)		; string heap pointer

	IF OPTM = 0
		CPL
		LD	C,A
		LD	B,0FFH			; -(size-1)
		ADD	HL,BC
		INC	HL			; new string heap pointer
	ELSE
		LD	C,A
		LD	B,0
		SBC	HL,BC			; new string heap pointer
	ENDIF

		RST	R_DCOMPR		; compare end of string heap with new string heap pointer
		JR	C,J66A9			; out of string heap space, try garbage collect
		LD	(FRETOP),HL		; update string heap pointer
		INC	HL
		EX	DE,HL
PPSWRT:		POP	AF			; discard garbage collect flag
		RET

J66A9:		POP	AF			; restore garbage collect flag
	IF OPTM = 0
		LD	DE,14			; ?? LD E,14 should be enough ??
	ELSE
		LD	E,14
	ENDIF
		JP	Z,ERROR			; garbage collect done, out of string space error
		CP	A			; Zx set (garbage collect done)
		PUSH	AF			; store garbage collect flag
		LD	BC,C6690
		PUSH	BC			; do a garbage collect and try allocate again

; Subroutine garbage collect
C66B6:		LD	HL,(MEMSIZ)		; start of string heap (use as string heap pointer starting point)
J66B9:		LD	(FRETOP),HL		; update string heap pointer
		LD	HL,0
		PUSH	HL			; pointer to string descriptor (+3) of top unheaped string = none found
		LD	HL,(STREND)		; end of variable area
		PUSH	HL			; pointer to top unheaped string = end of variable area (undefined)
		LD	HL,TEMPST		; temporary string descriptor pointer = start of temporary string descriptor heap

; loop through temporary string descriptors
C66C7:		LD	DE,(TEMPPT)		; end of temporary string descriptor heap
		RST	R_DCOMPR		; compare temporary string descriptor pointer with end of temporary string descriptor heap
		LD	BC,C66C7		; continue with next temporary descriptor
		JP	NZ,J6742		; not end of temporary string descriptor heap, update top of unheaped string if higher

		LD	HL,PRMPRV
		LD	(TEMP9),HL
		LD	HL,(ARYTAB)		; start of array variable area
		LD	(ARYTA2),HL		; end of variable area = end of simple variable area
		LD	HL,(VARTAB)		; variable pointer = start of simple variable area

; loop through variables
J66E1:		LD	DE,(ARYTA2)		; end of variable area
		RST	R_DCOMPR		; compare variable pointer with end of variable area
		JR	Z,J66FA			; end of variable area,
		LD	A,(HL)			; variable type
		INC	HL
		INC	HL
		INC	HL			; skip variable name, to variable value
		CP	3			; string ?
		JR	NZ,J66F4		; nope, offset = variable type, to next variable
		CALL	C6743			; update top of unheaped string if higher
		XOR	A			; offset = 0 (pointer already updated)
J66F4:		LD	E,A
		LD	D,0			; offset
		ADD	HL,DE			; update pointer to next variable
		JR	J66E1			; next variable

J66FA:		LD	HL,(TEMP9)		; current FN parameter block
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		LD	A,D
		OR	E			; end of FN parameter block list ?
		LD	HL,(ARYTAB)		; start of array variable area
		JR	Z,J671A			; yep, continue with the array variables
		EX	DE,HL
		LD	(TEMP9),HL		; update current FN parameter block
		INC	HL
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		EX	DE,HL
		ADD	HL,DE
		LD	(ARYTA2),HL		; update end of variable area
		EX	DE,HL
		JR	J66E1			; search FN parameter block variables

J6719:		POP	BC			; discard pointer in array variable

; loop through array variables
J671A:		LD	DE,(STREND)		; end of variable area
		RST	R_DCOMPR		; end of array variables ?
		JP	Z,J6763			; yep, move topstring up when possible
		LD	A,(HL)			; variable type
		INC	HL
		CALL	MOVRM			; load from HL (array offset and variable name)
		PUSH	HL			; store pointer in array variable
		ADD	HL,BC			; pointer to next array variable
		CP	3			; string ?
		JR	NZ,J6719		; nope, next array variable
		LD	(TEMP8),HL		; store pointer to next array variable
		POP	HL			; restore pointer in array variable
		LD	C,(HL)
		LD	B,0			; array dimension
		ADD	HL,BC
		ADD	HL,BC
		INC	HL			; to the first array element
C6737:		EX	DE,HL
		LD	HL,(TEMP8)		; pointer to next array variable
		EX	DE,HL
		RST	R_DCOMPR		; compare pointer to next array variable with pointer to next variable
		JR	Z,J671A			; end of array variable, next array variable
		LD	BC,C6737		; continue with next array element

J6742:		PUSH	BC

; Subroutine update top of unheaped string if higher
; Input:  HL = pointer to string descriptor
C6743:		XOR	A
		OR	(HL)			; empty string ?
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)			; pointer to string
		INC	HL
		RET	Z			; empty string, quit
		LD	B,H
		LD	C,L			; store pointer to next variable
		LD	HL,(FRETOP)		; string heap pointer
		RST	R_DCOMPR		; compare string heap pointer with pointer to string
		LD	H,B
		LD	L,C			; restore pointer to next variable
		RET	C			; string is in string heap, quit
		POP	HL			; return address
		EX	(SP),HL			; store return address, restore pointer to top unheaped string
		RST	R_DCOMPR		; compare end of variable area with pointer to string
		EX	(SP),HL			; store pointer to top unheaped string, restore return address
		PUSH	HL			; store return address
		LD	H,B
		LD	L,C			; restore pointer to next variable
		RET	NC			; string is literal (in BASIC program), quit
		POP	BC			; restore return address
		POP	AF			; discard pointer to top unheaped string
		POP	AF			; discard pointer to string descriptor (+3) of top unheaped string
		PUSH	HL			; store new pointer to string descriptor (+3) of top unheaped string
		PUSH	DE			; store new pointer to top unheaped string
		PUSH	BC			; store return addres
		RET

J6763:		POP	DE			; restore
		POP	HL			; restore pointer to next string descriptor
		LD	A,H
		OR	L			; topstring found ?
		RET	Z			; nope, quit with garabage collect
		DEC	HL
		LD	B,(HL)
		DEC	HL
		LD	C,(HL)			; pointer to string
		PUSH	HL			; store pointer to string descriptor +1
		DEC	HL
		LD	L,(HL)			; size of string
		LD	H,0
		ADD	HL,BC
		LD	D,B
		LD	E,C			; start of string
		DEC	HL
		LD	B,H
		LD	C,L			; end of string
		LD	HL,(FRETOP)		; string heap pointer
		CALL	BLTUC			; move data (copy string to bottom of the string heap)
		POP	HL			; restore pointer to string descriptor +1
		LD	(HL),C
		INC	HL
		LD	(HL),B			; update pointer to string
		LD	H,B
		LD	L,C
		DEC	HL
		JP	J66B9			; update string heap pointer and restart garbage collect

CAT:
J6787:		PUSH	BC
		PUSH	HL
		LD	HL,(DAC+2)
		EX	(SP),HL
		CALL	EVAL			; evaluate factor
		EX	(SP),HL
		CALL	CHKSTR			; check if DAC has string
		LD	A,(HL)			; size of 1st string
		PUSH	HL
		LD	HL,(DAC+2)
		PUSH	HL
		ADD	A,(HL)			; + size of 2nd string
	IF OPTM = 0
		LD	DE,15			; ?? LD E,15 should be enough ??
	ELSE
		LD	E,15
	ENDIF
		JP	C,ERROR			; resulting length >255, string too long error
		CALL	STRINI			; allocate temporary string for result
		POP	DE
		CALL	FRETMP			; free temporary string (descriptor in DE) -> free 2nd string
		EX	(SP),HL
		CALL	FRETM2			; free temporary string (descriptor in HL) -> free 1st string
		PUSH	HL
		LD	HL,(DSCTMP+1)
		EX	DE,HL
		CALL	C67BF			; copy string (descriptor on stack) -> copy 1st string
		CALL	C67BF			; copy string (descriptor on stack) -> copy 2nd string
		LD	HL,TSTOP
		EX	(SP),HL
		PUSH	HL
		JP	PUTNEW			; push temporary descriptor to temporary descriptor heap and quit

; Subroutine copy string (descriptor on stack)
; Input:  string descriptor on stack, DE = destination string
; Remark: works only if this routine is CALLed

C67BF:		POP	HL
		EX	(SP),HL			; get descriptor from stack
		LD	A,(HL)
		INC	HL
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		LD	L,A

; Subroutine copy string
; Input:  L = size of string, BC = source string, DE = destination string
C67C7:		INC	L
J67C8:		DEC	L
		RET	Z
		LD	A,(BC)
		LD	(DE),A
		INC	BC
		INC	DE
		JR	J67C8

; Subroutine FRESTR (check for string and free temporary string descriptor and string)
FRESTR:
C67D0:		CALL	CHKSTR			; check if string

; Subroutine free temporary string descriptor and string (DAC)
; Input:  DAC = pointer to temporary string descriptor
FREDAC:
C67D3:		LD	HL,(DAC+2)		; pointer to temporary string descriptor

; Subroutine free temporary string descriptor and string
; Input:  HL = string descriptor
FRETM2:
C67D6:		EX	DE,HL

; Subroutine free temporary string descriptor and string
; Input:  DE = pointer to temporary string descriptor
; Output: HL = pointer to temporary string descriptor
FRETMP:
C67D7:		CALL	FRETMS			; free temporary string descriptor if on top of the heap
		EX	DE,HL
		RET	NZ			; not freed, quit
		PUSH	DE			; store pointer to freed temporary string descriptor
		LD	D,B
		LD	E,C			; pointer to string
		DEC	DE
		LD	C,(HL)			; size of string
		LD	HL,(FRETOP)		; string heap pointer
		RST	R_DCOMPR		; on top of string heap ?
		JR	NZ,J67EC		; nope, quit
		LD	B,A			; 0
		ADD	HL,BC
		LD	(FRETOP),HL		; update string heap pointer
J67EC:		POP	HL			; restore pointer to freed temporary string desciptor
		RET

; Subroutine free temporary string descriptor if on top of the heap
; Input:  DE = pointer to string descriptor
; Output: BC = pointer to string, Zx set if freed
FRETMS:
C67EE:		SYSHOOK	H_FRET
		LD	HL,(TEMPPT)
		DEC	HL
		LD	B,(HL)
		DEC	HL
		LD	C,(HL)			; pointer to string
		DEC	HL
		RST	R_DCOMPR		; descriptor on top of the heap ?
		RET	NZ			; nope, quit
		LD	(TEMPPT),HL		; release descriptor from heap
		RET

; Subroutine LEN function
LEN:
C67FF:		LD	BC,SNGFLT
		PUSH	BC			; after this, byte to DAC

; Subroutine free temporary string and get size
C6803:		CALL	FRESTR			; check for string and free temporary string descriptor and string
		XOR	A
		LD	D,A
		LD	A,(HL)			; size of string
		OR	A			; Zx set if empty string
		RET

; Subroutine ASC function
ASC:
C680B:		LD	BC,SNGFLT		; after this, byte to DAC
		PUSH	BC

; Subroutine free temporary string and get first character
ASC2:
C680F:		CALL	C6803			; free temporary string and get size
		JP	Z,FCERR			; empty string, illegal function call
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)			; pointer to string
		LD	A,(DE)			; first character
		RET

; Subroutine CHR$ function
CHRS:
C681B:		CALL	STRIN1			; allocate temporary string of 1 char
		CALL	CONINT			; check for byte value

; Subroutine set first character of temporary string and put on heap
SETSTR:
C6821:		LD	HL,(DSCTMP+1)
		LD	(HL),E
FINBCK:
J6825:		POP	BC
		JP	PUTNEW			; push temporary descriptor to temporary descriptor heap and quit

; Subroutine STRING$ function
STRNGS:
J6829:		RST	R_CHRGTR		; get next BASIC character
		RST	R_SYNCHR
		DEFB	"("			; check for (
		CALL	GETBYT			; evaluate byte operand
		PUSH	DE
		RST	R_SYNCHR
		DEFB	","			; check for ,
		CALL	FRMEVL			; evaluate expression
		RST	R_SYNCHR
		DEFB	")"			; check for )
		EX	(SP),HL
		PUSH	HL
		RST	R_GETYPR		; get DAC type
		JR	Z,J6841			; string,
		CALL	CONINT			; check for byte value
		JR	J6844

J6841:		CALL	ASC2			; free temporary string and get first character
J6844:		POP	DE			; number of characters
		CALL	C684D			; create string with characters and quit

; Subroutine SPACE$ function
SPACES:
C6848:		CALL	CONINT			; check for byte value
		LD	A,' '

; Subroutine create string with characters
C684D:		PUSH	AF
		LD	A,E			; number of characters
		CALL	STRINI			; allocate temporary string
		LD	B,A
		POP	AF
		INC	B
		DEC	B			; stringsize zero ?
		JR	Z,J6825			; yep, temporary stringdescriptor to heap and quit
		LD	HL,(DSCTMP+1)		; pointer to temporary string
J685B:		LD	(HL),A
		INC	HL
		DJNZ	J685B			; fill string
		JR	J6825			; temporary stringdescriptor to heap and quit

; Subroutine LEFT$ function
LEFTS:
C6861:		CALL	C68E3
		XOR	A
J6865:		EX	(SP),HL
		LD	C,A
		DEFB	03EH			; LD A,xx, trick to skip next instruction
LEFTUS:
C6868:		PUSH	HL
C6869:		PUSH	HL
		LD	A,(HL)			; size of string
		CP	B
		JR	C,J6870
		LD	A,B			; size of string
		DEFB	011H			; LD DE,xxxx, trick to skip next instruction
J6870:		LD	C,0
		PUSH	BC
		CALL	GETSPA			; allocate stringspace
		POP	BC
		POP	HL
		PUSH	HL
		INC	HL
		LD	B,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,B
		LD	B,0
		ADD	HL,BC
		LD	B,H
		LD	C,L
		CALL	STRAD2			; create temporary string descriptor
		LD	L,A
		CALL	C67C7			; copy string
		POP	DE
		CALL	FRETMP			; free temporary string (descriptor in DE)
		JP	PUTNEW			; push temporary descriptor to temporary descriptor heap and quit

; Subroutine RIGHT$ function
RIGHTS:
C6891:		CALL	C68E3
		POP	DE
		PUSH	DE
		LD	A,(DE)
		SUB	B
		JR	J6865

; Subroutine MID$ function
MIDS1:
C689A:		EX	DE,HL
		LD	A,(HL)
		CALL	C68E6
		INC	B
		DEC	B
		JP	Z,FCERR			; illegal function call
		PUSH	BC
		CALL	C69E4
		POP	AF
		EX	(SP),HL
		LD	BC,C6869
		PUSH	BC
		DEC	A
		CP	(HL)
		LD	B,00H
		RET	NC
		LD	C,A
		LD	A,(HL)
		SUB	C
		CP	E
		LD	B,A
		RET	C
		LD	B,E
		RET

; Subroutine VAL function
VAL:
C68BB:		CALL	C6803			; free temporary string and get size
		JP	Z,SNGFLT		; empty string, byte (size) to DAC
		LD	E,A
		INC	HL
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		PUSH	HL
		ADD	HL,DE
		LD	B,(HL)
		LD	(VLZADR),HL
		LD	A,B
		LD	(VLZDAT),A
		LD	(HL),D
		EX	(SP),HL
		PUSH	BC
		DEC	HL
		RST	R_CHRGTR		; get next BASIC character
		CALL	FIN			; convert text to number
		LD	HL,0
		LD	(VLZADR),HL
		POP	BC
		POP	HL
		LD	(HL),B
		RET

C68E3:		EX	DE,HL
		RST	R_SYNCHR
		DEFB	")"			; check for )

C68E6:		POP	BC
		POP	DE
		PUSH	BC
		LD	B,E
		RET

; Subroutine INSTR function
INSTR:
J68EB:		RST	R_CHRGTR		; get next BASIC character
		CALL	FRMPRN			; evaluate ( expression
		RST	R_GETYPR		; get DAC type
		LD	A,1
		PUSH	AF
		JR	Z,J6906			; string,
		POP	AF
		CALL	CONINT			; check for byte value
		OR	A
		JP	Z,FCERR			; illegal function call
		PUSH	AF
		RST	R_SYNCHR
		DEFB	","			; check for ,
		CALL	FRMEVL			; evaluate expression
		CALL	CHKSTR			; check if string
J6906:		RST	R_SYNCHR
		DEFB	","			; check for ,
		PUSH	HL
		LD	HL,(DAC+2)
		EX	(SP),HL
		CALL	FRMEVL			; evaluate expression
		RST	R_SYNCHR
		DEFB	")"			; check for )
		PUSH	HL
		CALL	FRESTR			; check for string and free temporary string descriptor and string
		EX	DE,HL
		POP	BC
		POP	HL
		POP	AF
		PUSH	BC
		LD	BC,POPHRT
		PUSH	BC
		LD	BC,SNGFLT
		PUSH	BC			; after this, byte to DAC
		PUSH	AF
		PUSH	DE
		CALL	FRETM2			; free temporary string (descriptor in HL)
		POP	DE
		POP	AF
		LD	B,A
		DEC	A
		LD	C,A
		CP	(HL)
		LD	A,0
		RET	NC
		LD	A,(DE)
		OR	A
		LD	A,B
		RET	Z
		LD	A,(HL)
		INC	HL
		LD	B,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,B
		LD	B,0
		ADD	HL,BC
J693E:		SUB	C
		LD	B,A
		PUSH	BC
		PUSH	DE
		EX	(SP),HL
		LD	C,(HL)
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		POP	HL
J6949:		PUSH	HL
		PUSH	DE
		PUSH	BC
J694C:		LD	A,(DE)
		CP	(HL)
		JR	NZ,J6966
		INC	DE
		DEC	C
		JR	Z,J695D
		INC	HL
		DJNZ	J694C
		POP	DE
		POP	DE
		POP	BC
J695A:		POP	DE
		XOR	A
		RET

J695D:		POP	HL
		POP	DE
		POP	DE
		POP	BC
		LD	A,B
		SUB	H
		ADD	A,C
		INC	A
		RET

J6966:		POP	BC
		POP	DE
		POP	HL
		INC	HL
		DJNZ	J6949
		JR	J695A

; Subroutine MID$ statement
MIDS2:
J696E:		RST	R_SYNCHR
		DEFB	"("			; check for (
		CALL	PTRGET			; locate variable
		CALL	CHKSTR			; check if string
		PUSH	HL
		PUSH	DE
		EX	DE,HL
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		LD	HL,(STREND)		; end of variable area
		RST	R_DCOMPR
		JR	C,J6993
		LD	HL,(TXTTAB)
		RST	R_DCOMPR
		JR	NC,J6993
		POP	HL
		PUSH	HL
		CALL	STRCPY			; copy string to new temporary string
		POP	HL
		PUSH	HL
		CALL	VMOVE			; HL = DE (valtyp)
J6993:		POP	HL
		EX	(SP),HL
		RST	R_SYNCHR
		DEFB	","			; check for ,
		CALL	GETBYT			; evaluate byte operand
		OR	A			; startpos 0 ?
		JP	Z,FCERR			; yep, illegal function call
		PUSH	AF
		LD	A,(HL)
		CALL	C69E4
		PUSH	DE
		CALL	FRMEQL			; evaluate = expression
		PUSH	HL
		CALL	FRESTR			; check for string and free temporary string descriptor and string
		EX	DE,HL
		POP	HL
		POP	BC
		POP	AF
		LD	B,A
		EX	(SP),HL
		PUSH	HL
		LD	HL,POPHRT
		EX	(SP),HL
		LD	A,C
		OR	A
		RET	Z
		LD	A,(HL)
		SUB	B
		JP	C,FCERR			; illegal function call
		INC	A
		CP	C
		JR	C,J69C3
		LD	A,C
J69C3:		LD	C,B
		DEC	C
		LD	B,00H
		PUSH	DE
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,E
		ADD	HL,BC
		LD	B,A
		POP	DE
		EX	DE,HL
		LD	C,(HL)
		INC	HL
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		EX	DE,HL
		LD	A,C
		OR	A
		RET	Z
J69DB:		LD	A,(DE)
		LD	(HL),A
		INC	DE
		INC	HL
		DEC	C
		RET	Z
		DJNZ	J69DB
		RET

C69E4:		LD	E,0FFH
		CP	')'
		JR	Z,J69EF
		RST	R_SYNCHR
		DEFB	","			; check for ,
		CALL	GETBYT			; evaluate byte operand
J69EF:		RST	R_SYNCHR
		DEFB	")"			; check for )
		RET

; Subroutine FRE function
FRE:
C69F2:		LD	HL,(STREND)
		EX	DE,HL			; end of variable area
		LD	HL,0
		ADD	HL,SP			; Z80 stack pointer
		RST	R_GETYPR		; get DAC type
		JP	NZ,GIVDBL		; not a string, subtract and convert to double float (number of bytes of free BASIC memory)
		CALL	FREDAC			; free temporary string descriptor and string (DAC)
		CALL	C66B6			; garbage collect
		LD	DE,(STKTOP)		; end of string heap (= top of BASIC stack)
		LD	HL,(FRETOP)		; string heap pointer
		JP	GIVDBL			; subtract and convert to double float (number of bytes of free string heap space)

; ------------------------------------------------------------------------------
; SPCDSK.MAC
; BASIC file statements
; ------------------------------------------------------------------------------

		ALIGN	6A0EH

; Subroutine evaluate file specification
FILEVL:
C6A0E:		CALL	FRMEVL			; evaluate expression
		PUSH	HL			; store BASIC pointer
		CALL	FRESTR			; free temporary string with type check
		LD	A,(HL)
		OR	A			; empty string ?
		JR	Z,J6A47			; yep, bad filename error
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,E			; pointer to string
		LD	E,A			; size of string
		CALL	PARDEV			; parse device name
		PUSH	AF			; store device id
		LD	BC,FILNAM
		LD	D,8+3
		INC	E
J6A29:		DEC	E			; end of file specification string ?
		JR	Z,J6A61			; yep, fill remaining FILNAM with spaces
		LD	A,(HL)
		CP	20H			; control character ?
		JR	C,J6A47			; yep, bad filename error
		CP	'.'			; file name/extension seperator ?
		JR	Z,J6A4D			; yep, handle extension
		LD	(BC),A
		INC	BC
		INC	HL
		DEC	D			; FILNAM full ?
		JR	NZ,J6A29		; nope, next
J6A3B:		POP	AF			; restore device id
		PUSH	AF			; store device id
		LD	D,A			; store device id
		LD	A,(FILNAM+0)
		INC	A			; first character FILNAM charactercode 255 ?
		JR	Z,J6A47			; yep, bad filename error (because this is internally used as run flag)
		POP	AF			; restore device id
		POP	HL			; restore BASIC pointer
		RET

J6A47:		JP	DERNMF			; bad filename

J6A4A:		INC	HL
		JR	J6A29

J6A4D:		LD	A,D
		CP	8+3			; empty file name ?
		JP	Z,J6A47			; yep, bad file name error
		CP	3			; size file name > 8 ?
		JP	C,J6A47			; yep, bad file name error
		JR	Z,J6A4A			; size file name = 8, skip over seperator and continue with file extension
		LD	A,' '
		LD	(BC),A
		INC	BC
		DEC	D			; add space
		JR	J6A4D			; next

J6A61:		LD	A,' '
		LD	(BC),A
		INC	BC
		DEC	D
		JR	NZ,J6A61		; next
		JR	J6A3B			; finish

; Subroutine get pointer to I/O channel (DAC)
; Input:  (DAC) = I/O channel number
; Output: HL = I/O channel pointer
C6A6A:		CALL	CONINT			; check for byte value

; Subroutine get pointer to I/O channel
; Input:  A = I/O channel number
; Output: HL = I/O channel pointer
FILIDX:
C6A6D:		LD	L,A			; store I/O channel number
		LD	A,(MAXFIL)
		CP	L			; I/O channel number valid ?
		JP	C,DERBFN		; nope, bad filenumber error
		LD	H,0
		ADD	HL,HL
		EX	DE,HL
		LD	HL,(FILTAB)		; table with I/O channel pointers
		ADD	HL,DE
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A			; pointer to I/O channel
		LD	A,(NLONLY)
		INC	A			; NLONLY 0FFH ?
		RET	Z			; yep, quit
		LD	A,(HL)
		OR	A			; I/O channel open ?
		RET	Z			; nope, quit
		PUSH	HL			; store pointer to I/O channel
		LD	DE,4
		ADD	HL,DE			; +4
		LD	A,(HL)			; device id
		CP	9			; device = disk drive ?
		JR	NC,J6A99		; nope, not a disk drive device
		SYSHOOK	H_GETP			; hook for disk
		JP	DERIER			; internal error (should not return to here)

J6A99:		POP	HL			; restore pointer to I/O channel
		LD	A,(HL)
		OR	A			; Zx reset (I/O channel is open)
		SCF				; I/O channel device is not a disk drive
		RET

; Subroutine evaluate I/O channel operand and set current I/O channel, store BASIC pointer on stack
C6A9E:		DEC	HL
		RST	R_CHRGTR		; get next BASIC character
		CP	'#'
		CALL	Z,CHRGTR		; yep, get next BASIC character
		CALL	GETBYT			; evaluate byte operand
		EX	(SP),HL			; store BASIC pointer, restore return address
		PUSH	HL			; store return address

; Subroutine redirect interpreter input/output to file
; Input:  A = I/O channel number
SETFIL:
C6AAA:		CALL	FILIDX			; get pointer to I/O channel
		JP	Z,DERFNO		; I/O channel not open, file not open error
		LD	(PTRFIL),HL		; redirect BASIC interpreter to I/O channel
		SYSHOOK	H_SETF
		RET

; Subroutine OPEN statement
OPEN:
C6AB7:		LD	BC,FINPRT
		PUSH	BC			; after this, end BASIC interpreter redirect
		CALL	FILEVL			; evaluate file specification
		LD	A,(HL)
		CP	82H			; FOR token ?
		LD	E,4			; random i/o mode
		JR	NZ,J6AE4		; nope, open in random i/o mode
		RST	R_CHRGTR		; get next BASIC character
		CP	85H			; INPUT token ?
		LD	E,1			; input file mode
		JR	Z,J6AE3			; yep, open in input mode
		CP	9CH			; OUT token ?
		JR	Z,J6ADC			; yep, may be OUTPUT
		RST	R_SYNCHR
		DEFB	'A'
		RST	R_SYNCHR
		DEFB	'P'
		RST	R_SYNCHR
		DEFB	'P'
		RST	R_SYNCHR
		DEFB	081H			; check for APPEND
		LD	E,8			; append file mode
		JR	J6AE4			; open in append mode

J6ADC:		RST	R_CHRGTR		; get next BASIC character
		RST	R_SYNCHR
		DEFB	0B3H			; check for PUT token
		LD	E,2			; output file mode
		JR	J6AE4			; open in output mode

J6AE3:		RST	R_CHRGTR		; get next BASIC character
J6AE4:		RST	R_SYNCHR
		DEFB	'A'
		RST	R_SYNCHR
		DEFB	'S'			; check for AS
		PUSH	DE			; store device id, file mode
		LD	A,(HL)
		CP	'#'
		CALL	Z,CHRGTR		; yep, get next BASIC character
		CALL	GETBYT			; evaluate byte operand
		OR	A			; I/O channel 0 ?
		JP	Z,DERBFN		; yep, bad file number
		SYSHOOK	H_NOFO			; open statement extension hook
		DEFB	01EH			; LD E,xx, trick to skip next instruction

; Subroutine open I/O channel
; Input:  A = I/O channel number, D = device id, D = device id, E = file mode, HL = BASIC pointer
OPNFIL:
C6AFA:		PUSH	DE			; store device id, file mode
		DEC	HL
		LD	E,A			; store I/O channel number
		RST	R_CHRGTR		; end of statement ?
		JP	NZ,SNERR		; nope, syntax error
		EX	(SP),HL			; store BASIC pointer, restore device id, file mode
		LD	A,E			; I/O channel number
		PUSH	AF			; store I/O channel number
		PUSH	HL			; store device id and file mode
		CALL	FILIDX			; get pointer to I/O channel
		JP	NZ,DERFAO		; I/O channel already open, file already open
		POP	DE			; restore device id, file mode
		LD	A,D
		CP	9			; disk drive device ?
		SYSHOOK	H_NULO			; open for disk hook
		JP	C,DERIER		; internal error
		PUSH	HL			; store pointer to I/O channel
		LD	BC,4
		ADD	HL,BC			; +4
		LD	(HL),D			; update device id
		LD	A,0			; function = open
		POP	HL			; restore pointer to I/O channel
		CALL	GENDSP			; i/o function dispatcher
		POP	AF			; restore I/O channel number
		POP	HL			; restore BASIC pointer
		RET

; Subroutine close I/O channel
; Input:  A = I/O channel number
CLSFIL:
C6B24:		PUSH	HL			; store HL
		OR	A			; I/O channel 0 (system) ?
		JR	NZ,J6B30		; nope, skip check
		LD	A,(NLONLY)
		AND	01H			; loading BASIC program ?
		JP	NZ,POPHR2		; yep, restore HL and quit
J6B30:		CALL	FILIDX			; get pointer to I/O channel
		JR	Z,J6B4A			; I/O channel not open, skip close I/O channel
		LD	(PTRFIL),HL		; redirect BASIC interpreter to I/O channel
		PUSH	HL			; store pointer to I/O channel
		JR	C,J6B41			; not a disk drive device, use i/o function dispatcher
		SYSHOOK	H_NTFL			; close for disk hook
		JP	DERIER			; internal error (should not return to here)

J6B41:		LD	A,2			; function = close
		CALL	GENDSP			; i/o function dispatcher
		CALL	C6CEA			; clear buffer of interpreter I/O channel
		POP	HL			; restore pointer to I/O channel
J6B4A:		PUSH	HL			; store pointer to I/O channel
		LD	DE,7
		ADD	HL,DE			; +7
		LD	(HL),A			; clear I/O channel flags
		LD	H,A
		LD	L,A
		LD	(PTRFIL),HL		; end BASIC interpreter redirect
		POP	HL			; restore pointer to I/O channel
		ADD	A,(HL)			; file mode, Zx set if I/O channel already closed, Zx reset if I/O channel closed
		LD	(HL),0			; I/O channel closed
		POP	HL			; restore HL
		RET

; Subroutine RUN statement (with file specification)
; Input:  Zx reset (reset MERGE flag)
LRUN:
J6B5B:		SCF				; Cx=1 (set RUN flag)
		DEFB	011H			; LD DE,xxxx, skip to 6B5F

; Subroutine LOAD statement
LOAD:
C6B5D:		DEFB	0F6H			; OR 0AFH, so Zx reset (reset MERGE flag) and Cx reset (reset RUN flag)

; Subroutine MERGE statement
MERGE:
C6B5E:		XOR	A			; Zx set (set MERGE flag) and Cx reset (reset RUN flag)
		PUSH	AF			; store flags
		CALL	FILEVL			; evaluate file specification
		SYSHOOK	H_MERG
		POP	AF			; restore flags
		PUSH	AF			; store flags
		JR	Z,J6B76			; MERGE statement, skip ,R option check, reset leave I/O channels open flag
		LD	A,(HL)
		SUB	','			; extra operand ?
		OR	A
		JR	NZ,J6B76		; nope, reset leave I/O channels open flag
		RST	R_CHRGTR		; get next BASIC character
		RST	R_SYNCHR
		DEFB	'R'			; check for R
		POP	AF			; restore flags
		SCF				; set Cx (set RUN flag, set leave I/O channels open flag)
		PUSH	AF			; store run and merge flags
J6B76:		PUSH	AF			; store leave I/O channels open flag
		XOR	A			; I/O channel 0 (system)
		LD	E,1			; file mode = sequential input
		CALL	OPNFIL			; open I/O channel
		LD	HL,(PTRFIL)		; BASIC interpreter I/O channel
		LD	BC,7
		ADD	HL,BC			; +7
		POP	AF			; restore leave I/O channels open flag
		SBC	A,A
		AND	80H			; update leave I/O channels open flag
		OR	01H			; set BASIC program loading flag
		LD	(NLONLY),A
		POP	AF			; restore run and merge flags
		PUSH	AF			; store run and merge flags
		SBC	A,A
		LD	(FILNAM+0),A		; 000H for LOAD only, 0FFH for LOAD and RUN
		LD	A,(HL)			; I/O channel flags
		OR	A			; load binary BASIC file ?
		JP	M,J6BD4			; yep, handle
		POP	AF			; restore run and merge flags
		CALL	NZ,SCRTCH		; LOAD or RUN, clear BASIC program
		XOR	A			; I/O channel 0 (system)
		CALL	SETFIL			; redirect interpreter input/output to I/O channel
		JP	MAIN			; mainloop

; Subroutine SAVE statement
SAVE:
C6BA3:		CALL	FILEVL			; evaluate file specification
		SYSHOOK	H_SAVE
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		LD	E,80H			; file mode = save binary BASIC
		SCF				; flag = binary save
		JR	Z,J6BB7			; yep (no ,A option), use binary save
		RST	R_SYNCHR
		DEFB	','
		RST	R_SYNCHR
		DEFB	'A'			; check for ,A
		OR	A			; flag = ASCII save
		LD	E,2			; file mode = sequential output
J6BB7:		PUSH	AF			; store save type flag
		LD	A,D
		CP	9			; disk drive device ?
		JR	C,J6BC2			; yep,
		LD	E,2			; file mode = sequential output
		POP	AF			; restore save type flag
		XOR	A			; flag = ASCII save
		PUSH	AF			; store save type flag
J6BC2:		XOR	A			; I/O channel 0 (system)
		CALL	OPNFIL			; open I/O channel
		POP	AF			; restore save type flag
		JR	C,J6BCE			; binary save, handle
		DEC	HL
		RST	R_CHRGTR		; get next BASIC character
		JP	LIST			; list statement

J6BCE:		SYSHOOK	H_BINS
		JP	DERNMF			; bad filename

J6BD4:		SYSHOOK	H_BINL
		JP	DERNMF			; bad filename

; Subroutine get device id of BASIC interpreter I/O channel
; Input:  (PTRFIL) = I/O channel
; Unused Code: Not called from anywhere, leftover from a early Microsoft BASIC ?
Q6BDA:		PUSH	HL
		PUSH	DE
		LD	HL,(PTRFIL)		; BASIC interpreter I/O channel
		LD	DE,4
		ADD	HL,DE			; +4
		LD	A,(HL)			; device id
		POP	DE
		POP	HL
		RET


	IF OPTM = 0

; Subroutine close I/O channel(s)
RTALLR:
J6BE7:		JR	NZ,J6C02		; not end of statement, close specified I/O channels

; close all I/O channels
J6BE9:		PUSH	HL			; store BASIC pointer
J6BEA:		PUSH	BC			; store subroutine
		PUSH	AF			; store I/O channel number
		LD	DE,I6BF3
		PUSH	DE			; continue here
		PUSH	BC			; store subroutine
		OR	A			; ??
		RET				; execute subroutine

; continue close all I/O channels
I6BF3:		POP	AF			; restore I/O channel number
		POP	BC			; restore subroutine
		DEC	A
		JP	P,J6BEA			; next channel
		POP	HL			; restore BASIC pointer
		RET

; continue close specified I/O channels
I6BFB:		POP	BC			; restore subroutine
		POP	HL			; restore BASIC pointer
		LD	A,(HL)
		CP	','			; more operands ?
		RET	NZ			; nope, quit
		RST	R_CHRGTR		; get next BASIC character

; close specified I/O channels
J6C02:		PUSH	BC			; store subroutine
		LD	A,(HL)
		CP	'#'			; I/O channel indicator ?
		CALL	Z,CHRGTR		; yep, get next BASIC character
		CALL	GETBYT			; evaluate byte operand
		EX	(SP),HL			; store BASIC pointer, restore subroutine
		PUSH	HL			; store subroutine
		LD	DE,I6BFB
		PUSH	DE			; continue here
		SCF				; ??
		JP	(HL)			; execute subroutine

	ELSE

; close all I/O channels
J6BE7:		PUSH	HL			; store BASIC pointer
		LD	A,(MAXFIL)		; highest I/O channel number
J6BE8:		PUSH	AF			; store I/O channel number
		CALL	CLSFIL			; close I/O channel
		POP	AF			; restore I/O channel number
		DEC	A
		JP	P,J6BE8			; next channel
		POP	HL			; restore BASIC pointer
		RET

; close specified I/O channels
J6C02:		LD	A,(HL)
		CP	'#'			; I/O channel indicator ?
		CALL	Z,CHRGTR		; yep, get next BASIC character
		CALL	GETBYT			; evaluate byte operand
		PUSH	HL			; store BASIC pointer
		CALL	CLSFIL			; close I/O channel
		POP	HL			; restore BASIC pointer
		LD	A,(HL)
		CP	','			; more operands ?
		RET	NZ			; nope, quit
		RST	R_CHRGTR		; get next BASIC character
		JR	J6C02			; next operand

		ALIGN	6C14H			; keep CLOSE at same entry point

	ENDIF ; OPTM


; Subroutine CLOSE statement
CLOSE:
C6C14:
	IF OPTM = 0
		LD	BC,CLSFIL		; close I/O channel routine
		LD	A,(MAXFIL)		; default = all I/O channels
		JR	J6BE7			; close I/O channel(s)
	ELSE
		JR	NZ,J6C02		; close specific I/O channels
		JR	J6BE7			; close all I/O channels

		ALIGN	6C1CH			; keep CLSALL at same entry point
	ENDIF


; Subroutine close all I/O channels
CLSALL:
C6C1C:		LD	A,(NLONLY)
		OR	A			; leave I/O channels open ?
		RET	M			; yep, quit doing nothing

	IF OPTM = 0
		LD	BC,CLSFIL		; close I/O channel routine
		XOR	A			; 'end of statement' flag
		LD	A,(MAXFIL)		; all I/O channels
		JR	J6BE7			; close I/O channels
	ELSE
		JR	J6BE7			; close all I/O channels

		ALIGN	6C2AH			; keep LFILES at same entry point
	ENDIF


; Subroutine LFILES statement
LFILES:
C6C2A:		LD	A,1
		LD	(PRTFLG),A		; BASIC interpreter output = printer

; Subroutine FILES statement
FILES:
C6C2F:		SYSHOOK	H_FILE			; FILES hook
		JP	FCERR			; illegal function call

; Subroutine do random input/output
; Input:  HL = BASIC pointer, A = GET/PUT flag
GETPUT:
J6C35:		PUSH	AF			; store GET/PUT flag
		CALL	C6A9E			; evaluate I/O channel operand and set current I/O channel, store BASIC pointer on stack
		JR	C,J6C41			; device is not a disk drive,
		SYSHOOK	H_DGET			; GET/PUT for disk hook
		JP	DERNMF			; bad filename

J6C41:		POP	DE			; restore BASIC pointer
		POP	BC			; restore GET/PUT flag
		LD	A,4			; function = random i/o
		JP	GENDSP			; i/o function dispatcher

; Subroutine do sequential output
; Input:  A = character
FILOUT:
C6C48:		PUSH	HL
		PUSH	DE
		PUSH	BC
		PUSH	AF
		CALL	ISDDEV			; is BASIC interpreter I/O channel a disk device ?
		JR	NC,J6C57		; nope,
		SYSHOOK	H_FILO			; sequential output for disk hook
		JP	DERNMF			; bad filename

J6C57:		POP	AF
		PUSH	AF
		LD	C,A			; character
		LD	A,6			; function = sequential output
		CALL	GENDSP			; i/o function dispatcher
		JP	POPAL2			; pop registers and quit

; Subroutine is BASIC interpreter I/O channel a disk device ?
; Output: HL = pointer to I/O channel, A = device id, Cx set if disk drive device
ISDDEV:
C6C62:		PUSH	DE			; store DE
		LD	HL,(PTRFIL)		; BASIC interpreter I/O channel
		EX	DE,HL
		LD	HL,4
		ADD	HL,DE			; +4
		LD	A,(HL)			; device id
		EX	DE,HL
		POP	DE			; restore DE
		CP	9			; is a disk drive device ?
		RET

; Subroutine get sequential input from I/O channel
INDSKC:
C6C71:		PUSH	HL
J6C72:		PUSH	DE
		PUSH	BC
		CALL	ISDDEV			; is BASIC interpreter I/O channel a disk device ?
		JR	NC,J6C7F		; nope,
I6C79:		SYSHOOK	H_INDS			; sequential input for disk hook
		JP	DERIER			; internal error (should not return to here)

J6C7F:		LD	A,8			; function = sequential input
		CALL	GENDSP			; i/o function dispatcher
		JP	POPAL3			; pop registers and quit

; Subroutine INPUT$ function
FIXINP:
J6C87:		RST	R_CHRGTR		; get next BASIC character
		RST	R_SYNCHR
		DEFB	'$'
		RST	R_SYNCHR
		DEFB	'('			; check for $(
		PUSH	HL			; store BASIC pointer
		LD	HL,(PTRFIL)
		PUSH	HL			; store BASIC interpreter I/O channel
		LD	HL,0
		LD	(PTRFIL),HL		; end BASIC interpreter redirect
		POP	HL			; restore BASIC interpreter I/O redirect
		EX	(SP),HL			; store BASIC interpreter I/O redirect, restore BASIC pointer
		CALL	GETBYT			; evaluate byte operand
		PUSH	DE			; store string size
		LD	A,(HL)
		CP	','			; operand seperator ?
		JR	NZ,J6CB3		; nope, skip I/O channel operand, use keyboard
		RST	R_CHRGTR		; get next BASIC character
		CALL	C6A9E			; evaluate I/O channel operand and set current I/O channel, store BASIC pointer on stack
		CP	1			; sequential input mode ?
		JP	Z,J6CB0			; yep,
		CP	4			; random i/o mode ?
		JP	NZ,DERRPE		; nope, input past end error
J6CB0:		POP	HL			; restore BASIC pointer
		XOR	A			; clear keyboard flag
		LD	A,(HL)
J6CB3:		PUSH	AF			; store keyboard flag
		RST	R_SYNCHR
		DEFB	')'			; check for )
		POP	AF			; restore keyboard flag
		EX	(SP),HL			; store BASIC pointer, restore string size
		PUSH	AF			; store keyboard flag
		LD	A,L
		OR	A			; string size = 0 ?
		JP	Z,FCERR			; yep, illegal function call
		PUSH	HL			; store string size
		CALL	STRINI			; allocate temporary string
		EX	DE,HL
		POP	BC			; restore string size
J6CC4:		POP	AF			; restore keyboard flag
		PUSH	AF			; store keyboard flag
		JR	Z,J6CE2			; input from I/O channel,
		CALL	CHGET			; get character from keyboard
		PUSH	AF			; store character
		CALL	CKCNTC			; handle CTRL/STOP or STOP pressed, no resume
		POP	AF			; restore character
J6CD0:		LD	(HL),A			; store character in string
		INC	HL
		DEC	C			; finished string ?
		JR	NZ,J6CC4		; nope, next character
		POP	AF			; restore keyboard flag
		POP	BC			; restore BASIC pointer
		POP	HL			; restore BASIC interpreter I/O channel
		SYSHOOK	H_RSLF
		LD	(PTRFIL),HL		; redirect BASIC interpreter to I/O channel
		PUSH	BC			; store BASIC pointer
		JP	PUTNEW			; push temporary descriptor to temporary descriptor heap and quit

J6CE2:		CALL	INDSKC			; get sequential input from I/O channel
		JP	C,DERRPE		; error, input past end error
		JR	J6CD0			; continue

; Subroutine clear buffer of interpreter I/O channel
; Input:  HL = pointer to I/O channel
C6CEA:		CALL	C6CFB			; get pointer to buffer of BASIC interpreter I/O channel
		PUSH	HL			; store pointer to channel buffer
		LD	B,0			; 256 bytes
		CALL	C6CF5			; clear I/O channel buffer
POPHR2: POP	HL			; restore pointer to channel buffer
		RET

; Subroutine clear I/O channel buffer
; Input:  HL = pointer to channel buffer, B = number of bytes to clear
C6CF5:		XOR	A
J6CF6:		LD	(HL),A
		INC	HL
		DJNZ	J6CF6
		RET

; Subroutine get pointer to buffer of BASIC interpreter I/O channel
; Output: HL = pointer to I/O channel buffer
C6CFB:		LD	HL,(PTRFIL)		; BASIC interpreter I/O channel
		LD	DE,9
		ADD	HL,DE			; +9
		RET

; Subroutine LOC function
LOC:
C6D03:		SYSHOOK	H_SAVD
		CALL	C6A6A			; get pointer to I/O channel (DAC)
		JR	Z,J6D2B			; I/O channel not open, file not open
		LD	A,10			; function = loc
		JR	C,J6D30			; not a disk drive device, i/o function dispatcher and quit
		SYSHOOK	H_LOC			; LOC for disk
		JR	J6D36			; internal error (should not return to here)

; Subroutine LOF function
LOF:
C6D14:		SYSHOOK	H_SAVD
		CALL	C6A6A			; get pointer to I/O channel (DAC)
		JR	Z,J6D2B			; I/O channel not open, file not open
		LD	A,12			; function = lof
		JR	C,J6D30			; not a disk drive device, i/o function dispatcher and quit
		SYSHOOK	H_LOF			; LOF for disk
		JR	J6D36			; internal error (should not return to here)

; Subroutine EOF function
EOF:
C6D25:		SYSHOOK	H_SAVD
		CALL	C6A6A			; get pointer to I/O channel (DAC)
J6D2B:		JP	Z,DERFNO		; I/O channel not open, file not open
		LD	A,14			; function = eof
J6D30:		JP	C,GENDSP		; not a disk drive device, i/o function dispatcher and quit
		SYSHOOK	H_EOF			; EOF for disk
J6D36:		JP	DERIER			; internal error (should not return to here)

; Subroutine FPOS function
FPOS:
C6D39:		SYSHOOK	H_SAVD
		CALL	C6A6A			; get pointer to I/O channel (DAC)
		LD	A,16			; function = fpos
		JR	C,J6D30			; I/O channel open AND not a disk drive device, i/o function dispatcher and quit
		SYSHOOK	H_FPOS			; FPOS for disk
		JR	J6D36			; internal error (should not return to here)

; Subroutine direct statement
DIRDO:
J6D48:		CALL	ISFLIO			; is BASIC interpreter I/O redirected to I/O channel ?
		JP	Z,GONE			; nope, execute direct statement
		XOR	A			; I/O channel = 0 (system)
		CALL	CLSFIL			; close I/O channel
		JP	ERRFDR			; direct statement in file error

; ------------------------------------------------------------------------------
; DSKCOM.MAC
; COMMON 'DISK' BASIC functions
; ------------------------------------------------------------------------------

		ALIGN	6D55H

; Subroutine evaluate I/O channel number, update BASIC interpreter I/O channel and check for compatible input file mode
FILINP:
C6D55:		LD	C,1			; compatible file mode = sequential input

; Subroutine evaluate I/O channel number, update BASIC interpreter I/O channel and check for compatible file mode
; Input:  C = compatible file mode
FILGET:
C6D57:		CP	'#'			; I/O channel indicator ?
		RET	NZ			; nope, quit
		PUSH	BC			; store required file mode
		CALL	GTBYTC			; skip basic char and evaluate byte operand
		RST	R_SYNCHR
		DEFB	","			; check for ,
		LD	A,E
		PUSH	HL			; store BASIC pointer
		CALL	SETFIL			; redirect interpreter input/output to I/O channel
		LD	A,(HL)			; file mode
		POP	HL			; restore BASIC pointer
		POP	BC			; restore required file mode
		CP	C			; correct file mode ?
		JR	Z,J6D79			; yep, quit
		CP	4			; random mode ?
		JR	Z,J6D79			; yep, quit
		CP	8			; append mode ?
		JR	NZ,J6D76		; nope, bad filenumber error
		LD	A,C
		CP	2			; request output mode ?
J6D76:		JP	NZ,DERBFN		; nope, bad filenumber
J6D79:		LD	A,(HL)			; BASIC character
		RET

; Subroutine close system I/O channel and load HL from (TEMP)
PRGFIN:
C6D7B:		LD	BC,GTMPRT
		PUSH	BC			; after this, restore BASIC pointer from (TEMP)
		XOR	A			; I/O channel = 0 (system)
		JP	CLSFIL			; close I/O channel

FILIND:
J6D83:		RST	R_GETYPR		; get DAC type
		LD	BC,DOASIG		; after line input = 
		LD	DE,256*',' + ' '	; seperator1 = comma, seperator2 = space
		JR	NZ,J6DA3		; not a string,
		LD	E,D			; seperator1 = comma, seperator2 = comma
		JR	J6DA3


; Subroutine LINE INPUT I/O channel
DLINE:
J6D8F:		LD	BC,FINPRT
		PUSH	BC			; after this, end BASIC interpreter redirect
		CALL	FILINP			; evaluate I/O channel number, update BASIC interpreter I/O channel and check for compatible input file mode
		CALL	PTRGET			; locate variable
		CALL	CHKSTR			; check if string
		PUSH	DE			; store pointer to variable
		LD	BC,LETCON		; after line input = assign to variable
		XOR	A			; DAC type = string
		LD	D,A			; seperator1 = none
		LD	E,A			; seperator2 = none

J6DA3:		PUSH	AF			; store DAC type
		PUSH	BC			; store after line input routine
		PUSH	HL			; store BASIC pointer
J6DA6:		CALL	INDSKC			; get sequential input from I/O channel
		JP	C,DERRPE		; error, input past end error
		CP	' '			; space ?
		JR	NZ,J6DB4		; nope, continue
		INC	D
		DEC	D			; seperator1 = none ?
		JR	NZ,J6DA6		; yep, skip
J6DB4:		CP	'"'			; string indicator ?
		JR	NZ,J6DC6		; nope,
		LD	A,E
		CP	','			; seperator2 = comma (numeric) ?
		LD	A,'"'
		JR	NZ,J6DC6		; nope,
		LD	D,A			; seperator1 = string indicator
		LD	E,A			; seperator2 = string indicator
		CALL	INDSKC			; get sequential input from I/O channel
		JR	C,J6E0D			; error,
J6DC6:		LD	HL,BUF			; pointer in buffer
		LD	B,255			; buffer left counter
J6DCB:		LD	C,A			; store character
		LD	A,D
		CP	'"'			; seperator1 = string indicator ?
		LD	A,C			; restore character
		JR	Z,J6DFC			; yep,
		CP	0DH			; cr ?
		PUSH	HL			; store pointer in buffer
		JR	Z,J6E27			; yep, handle end of line
		POP	HL			; restore pointer in buffer
		CP	0AH			; lf ?
		JR	NZ,J6DFC		; nope,
J6DDC:		LD	C,A			; store character
		LD	A,E
		CP	','			; seperator2 = comma (numeric) ?
		LD	A,C			; restore character
		CALL	NZ,C6E61		; nope, put character in buffer, finish if buffer full
		CALL	INDSKC			; get sequential input from I/O channel
		JR	C,J6E0D			; error,
		CP	0AH			; lf ?
		JR	Z,J6DDC			; yep, next
		CP	0DH			; cr ?
		JR	NZ,J6DFC		; nope,
		LD	A,E
		CP	' '			; seperator2 = space ?
		JR	Z,J6E08			; yep, ignore
		CP	','			; seperator2 = comma ?
		LD	A,0DH			; cr
		JR	Z,J6E08			; yep, ignore

J6DFC:		OR	A			; end marker ?
		JR	Z,J6E08			; yep, ignore
		CP	D			; seperator1 ?
		JR	Z,J6E0D			; yep,
		CP	E			; seperator2 ?
		JR	Z,J6E0D			; yep,
		CALL	C6E61			; put character in buffer, finish if buffer full
	J6E08:		CALL	INDSKC			; get sequential input from I/O channel
		JR	NC,J6DCB		; no error, next

J6E0D:		PUSH	HL			; store pointer in buffer
		CP	'"'			; string indicator ?
		JR	Z,J6E16			; yep, ignore
		CP	' '			; space ?
		JR	NZ,J6E41		; nope, finish
J6E16:		CALL	INDSKC			; get sequential input from I/O channel
		JR	C,J6E41			; error, finish
		CP	' '			; space ?
		JR	Z,J6E16			; yep, ignore
		CP	','			; comma ?
		JR	Z,J6E41			; yep, finish
		CP	0DH			; cr ?
		JR	NZ,J6E30		; nope, backup character and finish

J6E27:		CALL	INDSKC			; get sequential input from I/O channel
		JR	C,J6E41			; error, finish
		CP	0AH			; lf ?
		JR	Z,J6E41			; yep, finish

; backup character and finish
J6E30:		LD	C,A			; store character
		CALL	ISDDEV			; is BASIC interpreter I/O channel a disk device ?
		JR	NC,J6E3C		; nope, use device dispatcher
		SYSHOOK	H_BAKU			; backup for disk hook
		JP	DERIER			; internal error (should not return to here)

J6E3C:		LD	A,18			; function = backup
		CALL	GENDSP			; i/o function dispatcher

NOKCR:
J6E41:		POP	HL			; restore pointer in buffer
J6E42:		LD	(HL),0			; end marker
		LD	HL,BUFMIN
		LD	A,E
		SUB	' '			; seperator2 = space ?
		JR	Z,J6E53			; yep,
		LD	B,0			; end character = none
		CALL	STRLT3			; analyze string with specified endmaker (1st char is skipped) and create temporary stringdescriptor
		POP	HL			; restore BASIC pointer
		RET

J6E53:		RST	R_GETYPR		; get DAC type
		PUSH	AF			; store DAC type
		RST	R_CHRGTR		; get next BASIC character
		POP	AF			; restore DAC type
		PUSH	AF			; store DAC type
		CALL	C,FIN			; not a double real, convert text to number
		POP	AF			; restore DAC type
		CALL	NC,FIN			; double real, convert text to number
		POP	HL			; restore BASIC pointer
		RET

; Subroutine put character in buffer, finish if buffer full
C6E61:		OR	A			; end marker ?
		RET	Z			; yep, quit
		LD	(HL),A			; put character in buffer
		INC	HL			; update pointer in buffer
		DEC	B			; update buffer count
		RET	NZ			; buffer not full, quit
		POP	AF			; discard return address
		JP	J6E42			; finish

; bad filename error
DERNMF:
J6E6B:		LD	E,56
		DEFB	001H			; LD BC,xxxx, trick to skip next instruction

; file already open error
DERFAO:
J6E6E:		LD	E,54
		DEFB	001H			; LD BC,xxxx, trick to skip next instruction

; direct statement in file error
ERRFDR:
J6E71:		LD	E,57
		DEFB	001H			; LD BC,xxxx, trick to skip next instruction

; file not found error
DERFNF:
J6E74:		LD	E,53
		DEFB	001H			; LD BC,xxxx, trick to skip next instruction

; file not open error
DERFNO:
J6E77:		LD	E,59
		DEFB	001H			; LD BC,xxxx, trick to skip next instruction

	; field overflow error
DERFOV:
J6E7A:		LD	E,50
		DEFB	001H			; LD BC,xxxx, trick to skip next instruction

; bad file number error
DERBFN:
J6E7D:		LD	E,52
		DEFB	001H			; LD BC,xxxx, trick to skip next instruction

; internal error
DERIER:
J6E80:		LD	E,51
		DEFB	001H			; LD BC,xxxx, trick to skip next instruction

; input past end error
DERRPE:
J6E83:		LD	E,55
		DEFB	001H			; LD BC,xxxx, trick to skip next instruction

; sequential i/o only error
DERSOO:
J6E86:		LD	E,58
		XOR	A
		LD	(NLONLY),A		; reset loading BASIC program, leave I/O channels open flag
		LD	(FLBMEM),A		; I/O channel raw mode = off
		JP	ERROR

; Subroutine BSAVE statement
BSAVE:
C6E92:		CALL	FILEVL			; evaluate filespecification
		PUSH	DE			; store device id
		RST	R_SYNCHR
		DEFB	","			; check for ,
		CALL	C6F0B			; evaluate BLOAD/BSAVE address operand
		EX	DE,HL
		LD	(SAVENT),HL
		EX	DE,HL			; assume start address = execute address
		PUSH	DE			; store start address
		RST	R_SYNCHR
		DEFB	","			; check for ,
		CALL	C6F0B			; evaluate BLOAD/BSAVE address operand
		EX	DE,HL
		LD	(SAVEND),HL
		EX	DE,HL			; store end address
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		JR	Z,J6EB9			; yep, skip execute address
		RST	R_SYNCHR
		DEFB	","			; check for ,
		CALL	C6F0B			; evaluate BLOAD/BSAVE address operand
		EX	DE,HL
		LD	(SAVENT),HL
		EX	DE,HL			; store execute address
J6EB9:		POP	BC			; restore start address
		POP	DE			; restore device id
		PUSH	HL
		PUSH	BC
		LD	A,D
		CP	0FFH			; device is CAS ?
		JP	Z,CBSAVE		; yep, BSAVE to cassette
		JP	DERNMF			; bad filename error

; Subroutine BLOAD statement
BLOAD:
C6EC6:		CALL	FILEVL			; evaluate filespecification
		PUSH	DE
		XOR	A
		LD	(RUNBNF),A		; assume no execute after load
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		LD	BC,0			; assume offset 0
		JR	Z,J6EE8			; yep, go
		RST	R_SYNCHR
		DEFB	","			; check for ,
		CP	'R'			; run option ?
		JR	NZ,J6EE3		; nope, offset option
		LD	(RUNBNF),A		; execute after load
		RST	R_CHRGTR		; get next BASIC character
		JR	Z,J6EE8			; end of statement, go
		RST	R_SYNCHR
		DEFB	","			; check for ,
J6EE3:		CALL	C6F0B			; evaluate BLOAD/BSAVE address operand (offset)
		LD	B,D
		LD	C,E			; offset
J6EE8:		POP	DE			; restore device id
		PUSH	HL			; store BASIC pointer
		PUSH	BC			; store offset
		LD	A,D
		CP	0FFH			; device is CAS ?
		JP	Z,CBLOAD		; BLOAD from cassette
		JP	DERNMF			; bad filename

; Subroutine finish BLOAD
BLDFIN:
J6EF4:		LD	A,(RUNBNF)
		OR	A			; execute after load ?
		JR	Z,J6F06			; nope, close channel and quit
		XOR	A			; I/O channel = 0 (system)
		CALL	CLSFIL			; close I/O channel 0
		LD	HL,POPHR2
		PUSH	HL			; after this, retore BASIC pointer and continue
		LD	HL,(SAVENT)		; execute address
		JP	(HL)			; start code

J6F06:		POP	HL			; restore BASIC pointer
		XOR	A			; I/O channel = 0 (system)
		JP	CLSFIL			; close I/O channel 0

; Subroutine evaluate BLOAD/BSAVE address operand
; Remark: duplicate of GETUIN
M6F0B:
C6F0B:		CALL	FRMEVL			; evaluate expression
		PUSH	HL			; store BASIC pointer
		CALL	FRQINT			; convert address to integer
		POP	DE			; restore BASIC pointer
		EX	DE,HL
		RET

; ------------------------------------------------------------------------------
; SPCDEV.MAC
; BASIC load and save statements
; ------------------------------------------------------------------------------

		ALIGN	6F15H

; Subroutine device name parser
PARDEV:
C6F15:		SYSHOOK	H_PARD			; hook device name parser: start of parser
		LD	A,(HL)
		CP	3AH			; 00H-39H ?
		JR	C,J6F37			; yep, bad filename
		PUSH	HL
		LD	D,E
		LD	A,(HL)
		INC	HL
		DEC	E
		JR	Z,J6F2E			; filespec has length 1, no device
J6F24:		CP	':'			; device seperator ?
		JR	Z,J6F3D			; yep,
		LD	A,(HL)
		INC	HL
		DEC	E
		JP	P,J6F24			; check for device
J6F2E:		LD	E,D
		POP	HL
		XOR	A			; Zx set
		LD	A,0FFH			; device id = CAS
		SYSHOOK	H_NODE			; hook device name parser: no device specified
		RET

J6F37:		SYSHOOK	H_POSD			; hook device name parser: first character filespecification has code 00-39H
		JP	DERNMF			; bad filename

J6F3D:		LD	A,D
		SUB	E
		DEC	A			; length of device name
		POP	BC
		PUSH	DE
		PUSH	BC
		LD	C,A
		LD	B,A
		LD	DE,I6F76		; internal device name table
		EX	(SP),HL
		PUSH	HL
J6F4A:		CALL	MAKUPL			; get char uppercase
		PUSH	BC
		LD	B,A
		LD	A,(DE)
		INC	HL
		INC	DE
		CP	B			; match ?
		POP	BC
		JR	NZ,J6F63		; nope,
		DEC	C
		JR	NZ,J6F4A		; next
J6F59:		LD	A,(DE)
		OR	A			; name in table also ends ?
		JP	P,J6F63			; nope, this is not it!
		POP	HL			; yep, A = device id
		POP	HL
		POP	DE
		OR	A			; Zx reset
		RET

J6F63:		OR	A			; already at device id ?
		JP	M,J6F59			; yep, found device!
J6F67:		LD	A,(DE)
		ADD	A,A
		INC	DE
		JR	NC,J6F67		; skip to next device name in table
		LD	C,B
		POP	HL
		PUSH	HL
		LD	A,(DE)
		OR	A
		JR	NZ,J6F4A		; try next device name
		JP	EXTDEV			; try external devices (in expansion roms)

I6F76:		DEFB	"CAS",0FFH
		DEFB	"LPT",0FEH
		DEFB	"CRT",0FDH
		DEFB	"GRP",0FCH
		DEFB	0

I6F87:		DEFW	C71C7			; CAS jumptable
		DEFW	C72A6			; LPT jumptable
		DEFW	C71A2			; CRT jumptable
		DEFW	C7182			; GRP jumptable

; Subroutine i/o function dispatcher
; Input:  A = function, E = file mode, HL = pointer to I/O channel
GENDSP:
C6F8F:		SYSHOOK	H_GEND
		PUSH	HL			; store pointer to I/O channel
		PUSH	DE			; store file mode
		PUSH	AF			; store function
		LD	DE,4
		ADD	HL,DE			; +4
		LD	A,(HL)			; device id
		CP	0FCH			; internal BASIC device ?
		JP	C,EXTDFN		; nope, execute i/o function in expansion ROM
		LD	A,0FFH
		SUB	(HL)
		ADD	A,A
		LD	E,A
		LD	HL,I6F87		; internal device jumptable pointer table
		ADD	HL,DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		POP	AF			; restore function
		LD	L,A
		LD	H,0
		ADD	HL,DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)			; address device function handler
		EX	DE,HL
		POP	DE			; restore file mode
		EX	(SP),HL			; store device function handler, restore pointer to I/O channel
		RET

; Subroutine CSAVE statement
CSAVE:
C6FB7:		call	A7098
		dec	hl
		RST	R_CHRGTR		; end of statement ?
		jr	z,A6FC3			; yep,
		RST	R_SYNCHR
		db	','			; check for ,
		call	GETCSP
A6FC3:		push	hl
		ld	a,0D3H			; header type = CSAVE file
		call	A7125			; write file header to cassete
		ld	hl,(VARTAB)
		ld	(SAVEND),hl
		ld	hl,(TXTTAB)		; start of BASIC program
		call	A713E			; save BASIC program
		pop	hl
		ret

; Subroutine BSAVE to cassette
; Input:  start address on stack
CBSAVE:
J6FD7:		ld	a,0D0H			; header type = BSAVE file
		call	A7125			; write file header to cassete
		xor	a
		call	C72F8			; start tape for output
		pop	hl			; restore start address
		push	hl			; store start address
		call	A7003			; write word to cassette
		ld	hl,(SAVEND)		; restore end address
		push	hl			; store end address
		call	A7003			; write word to cassette
		ld	hl,(SAVENT)		; execute address
		call	A7003			; write word to cassette
		pop	de			; restore end address
		pop	hl			; restore start address
A6FF4:		ld	a,(hl)
		call	C72DE			; write byte to cassette
		RST	R_DCOMPR		; end address reached ?
		jr	nc,A6FFE		; yep, finish
		inc	hl
		jr	A6FF4			; next

A6FFE:		call	TAPOOF			; stop tape for output
		pop	hl
		ret

; Subroutine write word to cassette
; Input:  HL = word
A7003:		ld	a,l
		call	C72DE			; write byte to cassette
		ld	a,h
		jp	C72DE			; write byte to cassette

; Subroutine read word from cassette
; Output: HL = word
A700B:		call	C72D4			; read byte from cassette
		ld	l,a
		call	C72D4			; read byte from cassette
		ld	h,a
		ret

; Subroutine BLOAD from cassette
; Input:  offset on stack
CBLOAD:
J7014:		ld	c,0D0H			; header type = BSAVE file
		call	A70B8			; read correct file header from cassete
		call	C72E9			; start tape for input
		pop	bc			; restore offset
		call	A700B			; read word from cassette
		add	hl,bc			; start address + offset
		ex	de,hl			; store start address
		call	A700B			; read word from cassette
		add	hl,bc			; end address + offset
		push	hl			; store end address
		call	A700B			; read word from cassette
		ld	(SAVENT),hl		; store execute address
		ex	de,hl			; restore start address
		pop	de			; restore end address
A702F:		call	C72D4			; read byte from cassette
		ld	(hl),a			; store byte
		RST	R_DCOMPR		; end address reached ?
		jr	z,A7039			; yep, finish
		inc	hl
		jr	A702F			; next

A7039:		call	TAPIOF			; stop tape for input
		jp	BLDFIN			; finish BLOAD

; Subroutine CLOAD statement
CLOAD:
C703F:		sub	091H			; ? (PRINT) token ?
		jr	z,A7045			; yep, verify
		xor	a			; reset verify flag
		db	001H			; LD BC,xxxx: trick to skip next 2 instructions
A7045:		cpl				; set verify flag
		inc	hl			; update BASIC pointer (eat ?)
		cp	1			; verify ?
		push	af			; store verify flag
		call	A708C			; evaluate optional filename
		ld	c,0D3H			; header type = CSAVE file
		call	A70B8			; read correct file header from cassete
		pop	af			; restore verify flag
		ld	(DAC+2),a		; store verify flag
		call	c,SCRTCH		; CLOAD, clear BASIC program
		ld	a,(DAC+2)		; restore verify flag
		cp	1			; verify ?
		ld	(FRCNEW),a		; ?? store verify flag
		push	af			; store verify flag
		call	DEPTR			; force line numbers
		pop	af			; restore verify flag
		ld	hl,(TXTTAB)		; start of BASIC program
		call	A715D			; load/verify BASIC program from cassette
		jr	nz,A707E		; verify error,
		ld	(VARTAB),hl		; update start of variable area
A7071:
		ld	hl,REDDY		; prompt message
	IFDEF MSX1
		CALL	STROUT			; message to interpreter output
	ELSE
		CALL	C7BE8			 ; message to interpreter output
	ENDIF
		ld	hl,(TXTTAB)
		push	hl			; store start of BASIC program
		jp	FINI			; manage pointers & vars, headloop

A707E:		inc	hl
		ex	de,hl
		ld	hl,(VARTAB)
		RST	R_DCOMPR		; difference in program area ?
		jp	c,A7071			; nope, then it is ok
		ld	e,20
		jp	ERROR			; Verify error

; Subroutine evaluate optional filename
; Input:  HL = BASIC pointer
A708C:		dec	hl
		RST	R_CHRGTR		; end of statement ?
		jr	nz,A7098		; nope,
		push	hl			; store BASIC pointer
		ld	hl,FILNAM
		ld	b,6
		jr	A70B1			; FILNAM = default

A7098:		call	FRMEVL			; evaluate expression
		push	hl			; store BASIC pointer
		call	ASC2			; free if temp
		dec	hl
		dec	hl
		ld	b,(hl)			; length of string
		ld	c,6
		ld	hl,FILNAM
A70A7:		ld	a,(de)
		ld	(hl),a
		inc	hl
		inc	de
		dec	c
		jr	z,A70B6			; FILNAM filled, quit
		djnz	A70A7
		ld	b,c
A70B1:		ld	(hl),' '
		inc	hl
		djnz	A70B1			; fill remainer with spaces
A70B6:		pop	hl			; restore BASIC pointer
		ret

; Subroutine read correct file header from cassete
; Input:  C = header type, FILNAM = filename
A70B8:		call	C72E9			; start tape for input
		ld	b,10
A70BD:		call	C72D4			; read byte from cassette
		cp	c			; correct file header type ?
		jr	nz,A70B8		; nope, try next file header
		djnz	A70BD
		ld	hl,FILNM2
		push	hl			; store FILNM2
		ld	b,6
A70CB:		call	C72D4			; read byte from cassette
		ld	(hl),a
		inc	hl
		djnz	A70CB			; read filename
		pop	hl			; restore FILNM2
		ld	de,FILNAM
		ld	b,6
A70D8:		ld	a,(de)
		inc	de
		cp	' '			; space ?
		jr	nz,A70E2		; nope, compare filename
		djnz	A70D8			; FILNAM all spaces ?
		jr	A70EF			; yep, found

A70E2:		ld	de,FILNAM
		ld	b,6
A70E7:		ld	a,(de)
		cp	(hl)
		jr	nz,A70F5		; not the same, skip
		inc	hl
		inc	de
		djnz	A70E7			; compare next
A70EF:		ld	hl,T70FF
		jp	A710D			; found

A70F5:		push	bc
		ld	hl,T7106
		call	A710D			; print Skip
		pop	bc
		jr	A70B8			; try next file header
	;
T70FF:		db	"Found:",0

T7106:		db	"Skip :",0

A710D:		ld	de,(CURLIN)
		inc	de
		ld	a,d
		or	e			; in direct mode ?
		ret	nz			; nope, quit
		call	STROUT
		ld	hl,FILNM2
		ld	b,6
A711D:		ld	a,(hl)
		inc	hl
		RST	OUTDO
		djnz	A711D
		jp	CRDO			; OUTDO next line

; Subroutine write file header to cassete
; Input:  A = header type, FILNAM = filename
A7125:		call	C72F8			; start tape for output
		ld	b,10
A712A:		call	C72DE			; write byte to cassette
		djnz	A712A
		ld	b,6
		ld	hl,FILNAM
A7134:		ld	a,(hl)
		inc	hl
		call	C72DE			; write byte to cassette
		djnz	A7134
		jp	TAPOOF			; stop tape for output and quit

; Subroutine save BASIC program to cassette
; Input:  HL = pointer to start of BASIC program
A713E:		push	hl
		call	DEPTR			; force line numbers
		xor	a
		call	C72F8			; start tape for output
		pop	de
		ld	hl,(SAVEND)
A714A:		ld	a,(de)
		inc	de
		call	C72DE			; write byte to cassette
		RST	R_DCOMPR		; end of BASIC program reached ?
		jr	nz,A714A		; nope, next
		ld	l,7
A7154:		call	C72DE			; write byte to cassette
		dec	l
		jr	nz,A7154
		jp	TAPOOF			; stop tape for output and quit

; Subroutine load/verify BASIC program from cassette
; Input:  Cx = load flag, HL = pointer to start of BASIC program
; Output: HL = pointer to end of BASIC program, Zx set if verify ok
A715D:		call	C72E9			; start tape for input
		sbc	a,a
		cpl
		ld	d,a			; store verify mask
A7163:		ld	b,10			; ten zero bytes = end of program
A7165:		call	C72D4			; read byte from cassette
		ld	e,a			; store byte
		call	REASON			; check if enough stackspace left
		ld	a,e			; restore byte
		sub	(hl)
		and	d			; verify mask
		jp	nz,TAPIOF		; verify failed, stop tape for input and quit
		ld	(hl),e			; store byte
		ld	a,(hl)
		or	a			; zero byte ?
		inc	hl
		jr	nz,A7163		; nope, reset zero counter
		djnz	A7165			; next
		ld	bc,-6
		add	hl,bc			; update pointer to end of BASIC program
		xor	a
		jp	TAPIOF			; stop tape for input and quit

; Table GRP device
C7182:		DEFW	J71B6			; open, check if output/append and open
		DEFW	J71C2			; close, quit
		DEFW	DERSOO			; random i/o, sequential i/o only error
		DEFW	J7196			; sequential output, output char to screen
		DEFW	FCERR			; sequential input, illegal function call
		DEFW	FCERR			; loc, illegal function call
		DEFW	FCERR			; lof, illegal function call
		DEFW	FCERR			; eof, illegal function call
		DEFW	FCERR			; fpos, illegal function call
		DEFW	FCERR			; backup, illegal function call

; Subroutine sequential output (GRP device)
J7196:		LD	A,(SCRMOD)
		CP	2			; in graphic screenmode ?
		JP	C,FCERR			; nope, illegal function call
		LD	A,C
		JP	GRPPRT			; output character to graphic screen

; Table CRT device
C71A2:		DEFW	J71B6			; open, check if output/append and open
		DEFW	J71C2			; close, quit
		DEFW	DERSOO			; random i/o, sequential i/o only error
		DEFW	J71C3			; sequential output, output char to screen
		DEFW	FCERR			; sequential input, illegal function call
		DEFW	FCERR			; loc, illegal function call
		DEFW	FCERR			; lof, illegal function call
		DEFW	FCERR			; eof, illegal function call
		DEFW	FCERR			; fpos, illegal function call
		DEFW	FCERR			; backup, illegal function call

J71B6:		CALL	C72CD			; bad filename error if in random i/o mode
		CP	1			; sequential input mode ?
		JP	Z,DERNMF		; yep, bad filename error
J71BE:		LD	(PTRFIL),HL		; redirect BASIC interpreter to I/O channel
		LD	(HL),E			; update I/O channel file mode
J71C2:		RET

; Subroutine sequential output (CRT device)
J71C3:		LD	A,C
		JP	CHPUT

; Table CAS device
C71C7:		DEFW	C71DB			; open,
		DEFW	J7205			; close,
		DEFW	DERSOO			; random i/o, sequential i/o only error
		DEFW	J722A			; sequential output,
		DEFW	C723F			; sequential input,
		DEFW	FCERR			; loc, illegal function call
		DEFW	FCERR			; lof, illegal function call
		DEFW	J726D			; eof
		DEFW	FCERR			; fpos, illegal function call
		DEFW	J727C			; backup

; Subroutine open (CAS device)
; Input:  HL = I/O channel pointer, E = file mode
C71DB:		push	hl			; store pointer to channel
		push	de			; store file mode
		ld	bc,6
		add	hl,bc
		xor	a
		ld	(hl),a			; position in buffer = 0
		ld	(CASPRV),a		; clear saved input
		call	C72CD			; bad filename error if in random i/o mode
		cp	4			; sequential append mode ?
		jp	z,DERNMF		; yep, bad filename error
		cp	1			; sequential input mode ?
		jr	z,A71FB			; yep,

	; sequential output mode

		ld	a,0EAH			; header type = ASCII file
		call	A7125			; write file header to cassete
A71F7:		pop	de			; restore file mode
		pop	hl			; restore pointer to channel
		jr	J71BE			; redir interpreter input/output to I/O channel, update file mode

; sequential input mode
A71FB:		ld	c,0EAH			; header type = ASCII file
		call	A70B8			; read correct file header from cassete
		call	TAPIOF			; stop tape for input
		jr	A71F7			; restore, redir interpreter input/output to I/O channel, update file mode

; Subroutine close (CAS device)
; Input:  HL = I/O channel pointer
J7205:		ld	a,(hl)
		cp	1			; sequential input mode ?
		jr	z,A7225			; yep, clear saved input and quit
		ld	a,01AH			; EOF
		push	hl			; store pointer to I/O channel
		call	A728B			; put in I/O channel buffer
		call	z,A722F			; buffer full, write buffer to tape
		pop	hl			; restore pointer to I/O channel
		call	A7281			; get current position in channel buffer, current position = 0
		jr	z,A7225			; buffer is empty (EOF already written), clear saved input and quit
		push	hl			; store pointer to channel buffer
		add	hl,bc
A721B:		ld	(hl),01AH
		inc	hl
		inc	c
		jr	nz,A721B		; fill remainer of buffer with EOF
		pop	hl			; restore pointer to channel buffer
		call	A722F			; write buffer to tape
A7225:		xor	a
		ld	(CASPRV),a
		ret

; Subroutine sequential output (CAS device)
; Input:  HL = I/O channel pointer, C = character
J722A:		ld	a,c
		call	A728B			; put in I/O channel buffer
		ret	nz			; buffer not full, quit
A722F:		xor	a
		call	C72F8			; start tape for output
		ld	b,0			; 256 bytes
A7235:		ld	a,(hl)
		call	C72DE			; write byte to cassette
		inc	hl
		djnz	A7235
		jp	TAPOOF			; stop tape for output and quit

; Subroutine sequential input (CAS device)
; Input:  HL = I/O channel pointer
; Output: A = character, Cx set if EOF
C723F:		ex	de,hl			; store pointer to channel
		ld	hl,CASPRV
		call	C72BE			; get cassette putback character, quit if any
		ex	de,hl			; restore pointer to channel
		call	A729B			; increase current position in channel buffer
		jr	nz,A7260		; buffer was not empty, return character from buffer
		push	hl			; store pointer to channel buffer
		call	C72E9			; start tape for input
		pop	hl			; restore pointer to channel buffer
		ld	b,0			; 256 bytes
A7253:		call	C72D4			; read byte from cassette
		ld	(hl),a			; store in channel buffer
		inc	hl
		djnz	A7253			; next byte
		call	TAPIOF			; stop tape for input
		dec	h			; pointer to start of channel buffer
		xor	a			; current position = 0
		ld	b,a			; clear high byte
A7260:		ld	c,a
		add	hl,bc			; pointer in channel buffer
		ld	a,(hl)			; get character
		cp	01AH			; EOF ?
		scf
		ccf
		ret	nz			; nope, return with Cx reset
		ld	(CASPRV),a		; saved input = EOF
		scf
		ret				; return with Cx set

; Subroutine eof (CAS device)
; Input:  HL = I/O channel pointer
J726D:		call	C723F			; sequential input (CAS device)
		ld	hl,CASPRV
		ld	(hl),a			; store character (for next sequential input)
		sub	01AH			; EOF -> 0
		sub	1
		sbc	a,a			; EOF -> FFH, otherwise 00H
		jp	CONIA			; convert signed byte to integer and store in DAC

; Subroutine backup (CAS device)
; Input:  HL = I/O channel pointer, C = character
J727C:		ld	hl,CASPRV
		ld	(hl),c			; update saved input
		ret

; Subroutine get current position in channel buffer, current position = 0
; Input:  HL = I/O channel pointer
; Output: A = C = current position, Zx set if empty buffer
A7281:		ld	bc,6
		add	hl,bc
		ld	a,(hl)
		ld	c,a			; store current position in channel buffer
		ld	(hl),0			; update current position = 0
		jr	A72A1			; return pointer to channel buffer

; Subroutine put in I/O channel buffer
; Input:  A = character
A728B:		ld	e,a			; store character
		ld	bc,6
		add	hl,bc
		ld	a,(hl)			; current position in buffer
		inc	(hl)			; update
		inc	hl
		inc	hl
		inc	hl			; buffer
		push	hl			; store pointer to channel buffer
		ld	c,a
		add	hl,bc
		ld	(hl),e			; put character in buffer
		pop	hl			; restore pointer to channel buffer
		ret

; Subroutine increase current position in channel buffer
; Input:  HL = I/O channel pointer
; Output: A = previous position, Zx set if buffer was empty
A729B:		ld	bc,6
		add	hl,bc
		ld	a,(hl)			; current position
		inc	(hl)			; increase position
A72A1:		inc	hl
		inc	hl
		inc	hl			; pointer to channel buffer
		and	a			; Zx set if buffer was empty
		ret


; Table LPT device
C72A6:		DEFW	J71B6			; open, check if output/append and open
		DEFW	J71C2			; close, quit
		DEFW	DERSOO			; random i/o, sequential i/o only error
		DEFW	J72BA			; sequential output, output char to printer
		DEFW	FCERR			; sequential input, illegal function call
		DEFW	FCERR			; loc, illegal function call
		DEFW	FCERR			; lof, illegal function call
		DEFW	FCERR			; eof, illegal function call
		DEFW	FCERR			; fpos, illegal function call
		DEFW	FCERR			; backup, illegal function call

; Subroutine sequential output (LPT device)
J72BA:		LD	A,C
		JP	OUTDLP

; Subroutine get cassette putback character, quit if any
; Input:  HL = CASPRV
C72BE:		LD	A,(HL)			; saved input
		LD	(HL),0			; clear saved input
		AND	A			; valid saved input ?
		RET	Z			; nope, quit
		INC	SP
		INC	SP			; dispose return address
		CP	1AH			; EOF ?
		SCF
		CCF
		RET	NZ			; nope, return with Cx reset
		LD	(HL),A			; saved input = EOF
		SCF
		RET				; return with Cx set

; Subroutine bad filename error if in random i/o mode
; Input:  E = file mode
C72CD:		LD	A,E
		CP	8
		JP	Z,DERNMF		; bad filename
		RET

; Subroutine read byte from cassette
; Output: A = byte
C72D4:		PUSH	HL
		PUSH	DE
		PUSH	BC
		CALL	TAPIN			; read byte from tape
		JR	NC,POPAL3		; no error, pop registers and quit
		JR	J72F2			; stop tape and quit with device I/O error

; Subroutine write byte to cassette
; Input:  A = byte
C72DE:		PUSH	HL
		PUSH	DE
		PUSH	BC
		PUSH	AF
		CALL	TAPOUT			; write byte to tape
		JR	NC,POPAL2		; no error, pop registers and quit
		JR	J72F2			; stop tape and quit with device I/O error

; Subroutine start cassette input
C72E9:		PUSH	HL
		PUSH	DE
		PUSH	BC
		PUSH	AF
		CALL	TAPION			; start tape for input
		JR	NC,POPAL2		; no error, pop registers and quit
J72F2:		CALL	TAPIOF			; stop tape for input
		JP	DIOERR			; quit with device I/O error

; Subroutine start cassette output
C72F8:		PUSH	HL
		PUSH	DE
		PUSH	BC
		PUSH	AF
		CALL	TAPOON			; start tape for output
POPAL2: POP	AF
POPAL3: POP	BC
		POP	DE
		POP	HL
		RET


; ------------------------------------------------------------------------------
; BASBIO.MAC
; BASIC I/O functions
; ------------------------------------------------------------------------------

		ALIGN	7304H

; Subroutine end printeroutput
FINLPT:
C7304:		XOR	A
		LD	(PRTFLG),A		; BASIC interpreter output = screen
		LD	A,(LPTPOS)
		OR	A			; printer position at start of line ?
		RET	Z			; yep, quit
		LD	A,0DH			; CR
		CALL	C731C			; byte to printer, device I/O error if error
		LD	A,0AH			; LF
		CALL	C731C			; byte to printer, device I/O error if error
		XOR	A
		LD	(LPTPOS),A		; printer position = 0
		RET

; Subroutine byte to printer, device I/O error if error
C731C:		CALL	LPTOUT			; byte to printer
		RET	NC			; no error, quit
		JP	DIOERR			; device I/O error

; Subroutine fresh line to interpreter output
CRDONZ:
C7323:		LD	A,(TTYPOS)
		OR	A			; screen position at start of line ?
		RET	Z			; yep, quit

; Subroutine new line to interpreter output
CRDO:
C7328:		SYSHOOK	H_CRDO			; hook
		LD	A,0DH			; CR
		RST	OUTDO			; byte to BASIC interpreter output
		LD	A,0AH			; LF
		RST	OUTDO			; byte to BASIC interpreter output

; Subroutine interpreter output pos = 0
CRFIN:
C7331:		CALL	ISFLIO			; is BASIC interpreter I/O redirected to I/O channel ?
		JR	Z,J7338			; nope,
		XOR	A
		RET

J7338:		LD	A,(PRTFLG)		; BASIC interpreter output flag
		OR	A			; to screen ?
		JR	Z,J7343			; yep, update screen position
		XOR	A
		LD	(LPTPOS),A		; printer position = 0
		RET

J7343:		LD	(TTYPOS),A		; screen position = 0
		RET

; Subroutine INKEY$ function
INKEY:
J7347:		RST	R_CHRGTR		; get next BASIC character
		PUSH	HL			; store BASIC pointer
		CALL	CHSNS			; keyboard input available ?
		JR	Z,J735A			; nope, return empty string
		CALL	CHGET			; get keyboard input
		PUSH	AF			; store keyboard input
		CALL	STRIN1			; allocate temporary string of 1 char
		POP	AF			; restore keyboard input
		LD	E,A			; store keyboard input
		CALL	SETSTR			; set first character of temporary string and put on heap and quit
J735A:		LD	HL,NULSTR
		LD	(DAC+2),HL		; pointer to empty string
		LD	A,3
		LD	(VALTYP),A		; empty string
		POP	HL			; restore BASIC pointer
		RET

; Subroutine char to interpreter output, LF expanded
OUTCH1:
C7367:		RST	OUTDO			; byte to BASIC interpreter output
		CP	0AH			; LF ?
		RET	NZ			; nope, quit
		LD	A,0DH			; CR
		RST	OUTDO			; byte to BASIC interpreter output
		CALL	CRFIN			; interpreter output pos = 0
		LD	A,0AH			; restore LF
		RET

; Subroutine get line from I/O channel
DSKCHI:
C7374:		SYSHOOK	H_DSKC			; hook
		LD	B,255			; number of space left in buffer
		LD	HL,BUF			; pointer in buffer = input buffer
J737C:		CALL	INDSKC			; get sequential input from I/O channel
		JR	C,J7397			; end of file, handle
		LD	(HL),A			; store character
		CP	0DH			; CR ?
		JR	Z,J7391			; yep, stop input
		CP	09H			; TAB ?
		JR	Z,J738E			; yep, continue


		IF	CHIFIX = 1

		CP	0AH			; LF ?
		JR	Z,J737C			; yep, ignore
		
		ELSE

		CP	20H			; control character ?
		JR	C,J737C			; yep, ignore
		
		ENDIF


J738E:		INC	HL
		DJNZ	J737C			; next
J7391:		XOR	A
		LD	(HL),A			; add end marker
		LD	HL,BUFMIN		; return pointer to before buffer (,)
		RET

J7397:		INC	B			; empty line ?
		JR	NZ,J7391		; nope, return line

LDREOF:	LD	A,(NLONLY)
		AND	80H
		LD	(NLONLY),A		; reset loading BASIC program flag
		CALL	PRGFIN			; close I/O channel 0 and restore BASIC pointer from (TEMP)
		LD	A,(FILNAM+0)
		AND	A			; RUN after LOAD ?
		JP	Z,STPRDY		; nope, ok and mainloop (+POP)
		CALL	RUNC			; initialize interpreter, BASIC pointer at start of program
		JP	NEWSTT			; execute new statement

; Subroutine device I/O error
DIOERR:
J73B2:		LD	E,19
		JP	ERROR			; device i/o error

; ------------------------------------------------------------------------------
; MSXSTS.MAC
; MSX BASIC statements
; ------------------------------------------------------------------------------

		ALIGN	73B7H


; Subroutine MOTOR statement
MOTOR:
C73B7:		ld	e,0FFH			; default = toggle
		jr	z,A73C6			; end of statement, toggle
		sub	0EBH			; OFF token ?
		ld	e,a			; off
		jr	z,A73C5			; yep, off
		RST	R_SYNCHR
		defb	095H			; check for ON token
		ld	e,1			; on
		defb	03EH			; LD A,xx: trick to skip next instruction
A73C5:		RST	R_CHRGTR
A73C6:		ld	a,e
		jp	STMOTR			; change cassette motor status

; Subroutine SOUND statement
SOUND:
C73CA:		CALL	GETBYT			; evaluate byte operand
		CP	13+1			; register 0-13 ?
		JP	NC,FCERR		; nope, illegal function call
		PUSH	AF			; store register
		RST	R_SYNCHR
		DEFB	','			; check for ,
		CALL	GETBYT			; evaluate byte operand
		POP	AF			; restore register
		CP	7			; register 7 ?
		JR	NZ,J73E1		; nope, write PSG register
		RES	6,E
		SET	7,E			; make sure PSG I/O port definition is not changed
J73E1:		JP	WRTPSG			; write PSG register

I73E4:		DEFB	' '

; PSG VCB structure:
;
;	+0,2	timer
;	+2	size of macro string
;	+3,2	pointer to macro string
;	+5,2	top of stack
;	+7	size music packet
;	+8,7	music packet
;		+0, high byte duration counter + music packet size (b7-b5)
;		+1, low byte duration counter
;		+2, music data. b7+b6 = type
;			type 00hhhhhh = frequency packet, h = high byte frequency, next byte = low byte frequency
;			type 1x0svvvv = volume/envelope specified, s = shape bit, vvvv = volume/shape
;			type x10svvvv = envelope period specified, next byte = low byte period, next byte = high byte period
;	+15	octave
;	+16	note length
;	+17	tempo
;	+18	volume
;	+19,2   envelope period
;	+21,16  stack (8 words, maximium of 3 macro sub strings)

; Subroutine PLAY statement
PLAYS:
C73E5:		SYSHOOK	H_PLAY
		PUSH	HL			; store BASIC pointer
		LD	HL,I752E
		LD	(MCLTAB),HL		; macro language command table = play macro language
		LD	A,0			; voice = 0
		LD	(PRSCNT),A		; no macro string of any voice finished, music dequeueing not in progress
		LD	HL,-10
		ADD	HL,SP
		LD	(SAVSP),HL		; during PLAY, 5 words extra are on stack
		POP	HL			; restore BASIC pointer
		PUSH	AF			; store voice
J73FD:		CALL	FRMEVL			; evaluate expression
		EX	(SP),HL			; store BASIC pointer, restore voice
		PUSH	HL			; store voice
		CALL	FRESTR			; free temporary string with type check

	IF OPTM = 0
		CALL	MOVRM			; load from HL
		LD	A,E
		OR	A			; stringsize zero ?
		JR	NZ,J7413		; nope,
		LD	E,1			; string size = 1
		LD	BC,I73E4		; string with space
		LD	D,C
		LD	C,B
J7413:		POP	AF			; restore voice
		PUSH	AF			; store voice
		CALL	GETVCP			; get pointer to macro string info voice buffer
		LD	(HL),E			; size of macro string
		INC	HL
		LD	(HL),D
		INC	HL
		LD	(HL),C			; pointer to macro string

	ELSE
		CALL	GETBCD			; load from HL
		LD	A,D
		OR	A			; stringsize zero ?
		JR	NZ,J7413		; nope,
		LD	D,1			; string size = 1
		LD	BC,I73E4		; string with space
J7413:		POP	AF			; restore voice
		PUSH	AF			; store voice
		CALL	GETVCP			; get pointer to macro string info voice buffer
		LD	(HL),D			; size of macro string
		INC	HL
		LD	(HL),C
		INC	HL
		LD	(HL),B			; pointer to macro string
	ENDIF

		INC	HL
		LD	D,H
		LD	E,L			; pointer to top of stack
		LD	BC,36-3-5
		ADD	HL,BC
		EX	DE,HL			; null pointer macor string and dummy size of macro string on stack
		LD	(HL),E
		INC	HL
		LD	(HL),D			; initialize stack
		POP	BC			; restore voice
		POP	HL			; restore BASIC pointer
		INC	B			; update voice
		LD	A,B
		CP	2+1			; done all voices ?
		JR	NC,J7446		; yep, finish
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		JR	Z,J7439			; yep,
		PUSH	BC			; store voice
		RST	R_SYNCHR
		DEFB	','			; check for ,
		JR	J73FD			; next voice

J7439:		LD	A,B
		LD	(VOICEN),A		; update current voice
		CALL	C7507			; end of queue mark in current voice queue
		INC	B			; update voice
		LD	A,B
		CP	2+1			; done all voices ?
		JR	C,J7439			; nope, next voice
J7446:		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		JP	NZ,SNERR		; nope, syntax error
		PUSH	HL			; store BASIC pointer

; start processing macro strings
J744C:		XOR	A			; voice = 0
J744D:		PUSH	AF			; store voice
		LD	(VOICEN),A		; update current voice
		LD	B,A			; store voice
		CALL	C7521			; current voice queue full ?
		JP	C,J74D6			; yep, skip to next voice
		LD	A,B			; restore voice
		CALL	GETVCP			; get pointer to macro string info voice buffer
		LD	A,(HL)
		OR	A			; macro string size = 0 ?
		JP	Z,J74D6			; yep, skip to next voice
		LD	(MCLLEN),A		; update macro string size
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)			; pointer to macro string
		INC	HL
		LD	(MCLPTR),DE		; update macro string pointer
		LD	E,(HL)
		INC	HL
		LD	D,(HL)			; source start = top of stack
		INC	HL
		PUSH	HL			; store pointer to size of packet
		LD	L,36			; offset = start of stack
		CALL	GETVC2			; get pointer in current voice buffer
		PUSH	HL
		LD	HL,(SAVSP)
		DEC	HL			; destination end = below current Z80 stack
		POP	BC			; source end =
		DI				; no interrupts during stack manupilation
		CALL	BLTUC			; move data (copy stack data from VCB to Z80 stack)
		POP	DE			; restore pointer to size of packet
		LD	H,B
		LD	L,C
		LD	SP,HL			; stack pointer = destination start
		EI				; finished stack manupilation
		LD	A,0FFH
		LD	(MCLFLG),A		; MCL = music
		JP	MCLSCN			; execute macro command (if any)

; queue is full
J748E:		LD	A,(MCLLEN)
		OR	A			; end of macro string ?
		JR	NZ,J7497		; nope, skip end of queue marker

; end of macro string
PLYEOS:
J7494:		CALL	C7507			; end of queue mark in current voice queue

; update VCB, remove VCB stack and continue with next voice
J7497:		LD	A,(VOICEN)		; current voice
		CALL	GETVCP			; get pointer to macro string info voice buffer
		LD	A,(MCLLEN)
		LD	(HL),A			; update size of macro string
		INC	HL
		LD	DE,(MCLPTR)
		LD	(HL),E
		INC	HL
		LD	(HL),D			; update pointer to macro string
		LD	HL,0
		ADD	HL,SP
		EX	DE,HL			; source start = top of stack
		LD	HL,(SAVSP)
		DI				; no interrupts during stack manupilation
		LD	SP,HL			; restore to Z80 stack
		POP	BC
		POP	BC
		POP	BC			; cleanup stack
		PUSH	HL			; store stack pointer
		OR	A
		SBC	HL,DE			; stack empty ?
		JR	Z,J74D4			; yep, continue with next voice
		LD	A,0F0H
		AND	L
		OR	H			; size on stack < 16 bytes ?
		JP	NZ,FCERR		; nope, illegal function call
		LD	L,36			; offset = start of stack
		CALL	GETVC2			; get pointer in current voice buffer
		POP	BC			; source end = start of stack
		DEC	BC
		CALL	BLTUC			; move data (copy stack data from Z80 stack to VCB)
		POP	HL			; restore pointer to size of packet
		DEC	HL
		LD	(HL),B
		DEC	HL
		LD	(HL),C			; update top of stack
		JR	J74D6			; next voice

J74D4:		POP	BC
		POP	BC			; clean up stack

; next voice
J74D6:		EI				; finished stack manupilation
		POP	AF			; restore voice
		INC	A			; update voice
		CP	2+1			; done all voices ?
		JP	C,J744D			; nope, next voice
		DI				; no interrupts
		LD	A,(INTFLG)
		CP	3			; CTRL/STOP pressed ?
		JR	Z,J7502			; yep, silence PSG and quit
		LD	A,(PRSCNT)
		RLCA				; music dequeueing in progress ?
		JR	C,J74F3			; yep, skip start music dequeueing
		LD	HL,PLYCNT
		INC	(HL)			; increase number of play sequences
		CALL	STRTMS			; start music dequeuing
J74F3:		EI
		LD	HL,PRSCNT
		LD	A,(HL)
		OR	80H
		LD	(HL),A			; set music dequeueing in progress flag
		CP	80H+3			; macro strings of all voices finished ?
		JP	NZ,J744C		; nope, continue processing macro strings
J7500:		POP	HL			; restore BASIC pointer
		RET

J7502:		CALL	GICINI			; initialize PSG (silence)
		JR	J7500			; restore BASIC pointer and quit

; Subroutine end of queue mark in current voice queue
C7507:		LD	A,(PRSCNT)
		INC	A
		LD	(PRSCNT),A		; increase macro string voice finished
		LD	E,0FFH			; end of queue mark

; Subroutine put in current voice queue
; Input:  E = byte
C7510:		PUSH	HL
		PUSH	BC
J7512:		PUSH	DE
		LD	A,(VOICEN)		; current voice
		DI				; no interrupts during queue operation
		CALL	PUTQ			; put byte in queue
		EI				; queue operation finished, allow interrupts
		POP	DE
		JR	Z,J7512			; queue is full, retry
		POP	BC
		POP	HL
		RET

; Subroutine current voice queue full ?
; Output: Cx set if queue full
C7521:		LD	A,(VOICEN)		; current voice
		PUSH	BC
		DI				; no interrupts during queue operation
		CALL	LFTQ
		EI				; queue operation finished, allow interrupts
		POP	BC
		CP	8
		RET

I752E:		DEFB	'A'			; note A
		DEFW	PLYNOT
		DEFB	'B'			; note B
		DEFW	PLYNOT
		DEFB	'C'			; note C
		DEFW	PLYNOT
		DEFB	'D'			; note D
		DEFW	PLYNOT
		DEFB	'E'			; note E
		DEFW	PLYNOT
		DEFB	'F'			; note F
		DEFW	PLYNOT
		DEFB	'G'			; note G
		DEFW	PLYNOT
		DEFB	'M'+128			; change envelope period, numeric operand
		DEFW	PENVLP
		DEFB	'V'+128			; change volume, numeric operand
		DEFW	PVOLUM
		DEFB	'S'+128			; change shape, numeric operand
		DEFW	PSHAPE
		DEFB	'N'+128			; note number, numeric operand
		DEFW	PLYNUM
		DEFB	'O'+128			; change octave, numeric operand
		DEFW	POCTAV
		DEFB	'R'+128			; rest note, numeric operand
		DEFW	PLYRST
		DEFB	'T'+128			; change tempo, numeric operand
		DEFW	PTEMPO
		DEFB	'L'+128			; change note length, numeric operand
		DEFW	PLYLEN
		DEFB	'X'			; macro substring
		DEFW	MCLXEQ
		DEFB	0

NOTXLT:
I755F:		DEFB	8*2			; A-
		DEFB	9*2			; A
		DEFB	10*2			; A# or B-
		DEFB	11*2			; B or C-
		DEFB	0*2			; B#
		DEFB	0*2			; C
		DEFB	1*2			; C# or D-
		DEFB	2*2			; D
		DEFB	3*2			; D# or E-
		DEFB	4*2			; E or F-
		DEFB	5*2			; E#
		DEFB	5*2			; F
		DEFB	6*2			; F# or G-
		DEFB	7*2			; G
		DEFB	8*2			; G#

NOTTAB:
I756E:		DEFW	00D5DH			; C
		DEFW	00C9CH			; C#
		DEFW	00BE7H			; D
		DEFW	00B3CH			; D#
		DEFW	00A9BH			; E
		DEFW	00A02H			; F
		DEFW	00973H			; F#
		DEFW	008EBH			; G
		DEFW	0086BH			; G#
		DEFW	007F2H			; A
		DEFW	00780H			; A#
		DEFW	00714H			; B

; Subroutine change volume
; Input:  DE = value, Cx set if value specified
PVOLUM:
C7586:		JR	C,J758A			; value specified,
		LD	E,8			; use default volume
J758A:		LD	A,15
		CP	E			; valid volume ?
		JR	C,J75DF			; nope, illegal function call
J758F:		XOR	A
		OR	D			; volume > 255 ?
		JR	NZ,J75DF		; yep, illegal function call
		LD	L,18			; offset = volume
		CALL	GETVC2			; get pointer in current voice buffer
		LD	A,40H
		AND	(HL)			; clear volume bits, clear must include volume, keep envelope flag
		OR	E
		LD	(HL),A
		RET

; Subroutine change envelope period
; Input:  DE = value, Cx set if value specified
PENVLP:
C759E:		LD	A,E
		JR	C,J75A4			; value specified,
		CPL
		INC	A
		LD	E,A			; use default envelope period = 255
J75A4:		OR	D
		JR	Z,J75DF			; illegal function call
		LD	L,19			; offset = envelope period
		CALL	GETVC2			; get pointer in current voice buffer
		PUSH	HL
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A			; get current envelope period
		RST	R_DCOMPR		; same envelope periode ?
		POP	HL
		RET	Z			; yep, quit
		LD	(HL),E
		INC	HL
		LD	(HL),D			; update envelope period
		DEC	HL
		DEC	HL
		LD	A,40H
		OR	(HL)
		LD	(HL),A			; set envelope flag
		RET

; Subroutine change shape
; Input:  DE = value, Cx set if value specified
PSHAPE:
C75BE:		LD	A,E
		CP	15+1			; valid shape value ?
		JR	NC,J75DF		; nope, illegal function call
		OR	10H			; set MODE bit
		LD	E,A
		JR	J758F			; continue in volume

; Subroutine change note length
; Input:  DE = value, Cx set if value specified
PLYLEN:
C75C8:		JR	C,J75CC			; value specified,
		LD	E,4			; use default note length
J75CC:		LD	A,E
		CP	64+1			; valid note length ?
		JR	NC,J75DF		; nope, illegal function call
		LD	L,16			; offset = note length

; get pointer in current voice buffer, check for <256 and <>0, update value
J75D3:		CALL	GETVC2			; get pointer in current voice buffer
		XOR	A
		OR	D			; value > 255 ?
		JR	NZ,J75DF		; yep, illegal function call
		OR	E			; value = 0 ?
		JR	Z,J75DF			; yep, illegal function call
		LD	(HL),A			; update value
		RET

; illegal function call
J75DF:
	IF OPTM = 0
		CALL	FCERR			; illegal function call
	ELSE
		JP	FCERR
	ENDIF

; Subroutine change tempo
; Input:  DE = value, Cx set if value specified
PTEMPO:
C75E2:		JR	C,J75E6			; value specified,
		LD	E,120			; use default tempo
J75E6:		LD	A,E
		CP	32			; valid tempo ?
		JR	C,J75DF			; nope, illegal function call
		LD	L,17			; offset = tempo
		JR	J75D3			; get pointer in current voice buffer, check for <256 and <>0, update value

; Subroutine change octave
; Input:  DE = value, Cx set if value specified
POCTAV:
C75EF:		JR	C,J75F3			; value specified,
		LD	E,4			; use default octave
J75F3:		LD	A,E
		CP	8+1			; valid octave ?
		JR	NC,J75DF		; nope, illegal function call
		LD	L,15			; offset = octave
		JR	J75D3			; get pointer in current voice buffer, check for <256 and <>0, update value

; Subroutine rest note
; Input:  DE = value, Cx set if value specified
PLYRST:
C75FC:		JR	C,J7600			; value specified,
		LD	E,4			; use default rest length
J7600:		XOR	A
		OR	D			; rest value > 255 ?
		JR	NZ,J75DF		; yep, illegal function call
		OR	E			; rest value = 0 ?
		JR	Z,J75DF			; yep, illegal function call
		CP	64+1			; valid rest value ?
		JR	NC,J75DF		; nope, illegal function call

; rest note
J760B:		LD	HL,0			; frequency = 0 (rest note)
		PUSH	HL			; store frequency
		LD	L,16			; offset = note length
		CALL	GETVC2			; get pointer in current voice buffer
		PUSH	HL			; store pointer to note length
		INC	HL
		INC	HL
		LD	A,(HL)			; current volume
		LD	(SAVVOL),A		; store current volume
		LD	(HL),80H		; update volume = 0, must include volume in packet
		DEC	HL
		DEC	HL
		JR	J769C

; Subroutine play note number
; Input:  DE = value, Cx set if value specified
PLYNUM:
C7621:		JR	NC,J75DF		; value not specified, illegal function call
		XOR	A
		OR	D			; note number > 255 ?
		JR	NZ,J75DF		; yep, illegal function call
		OR	E			; note number = 0 ?
		JR	Z,J760B			; yep, rest note
		CP	96+1			; valid note number ?
		JR	NC,J75DF		; nope, illegal function call
		LD	A,E
		LD	B,0			; high byte frequency offset = 0
		LD	E,B			; octave = 0
	J7632:		SUB	12
		INC	E
		JR	NC,J7632
		ADD	A,12
		ADD	A,A
		LD	C,A			; frequency offset
		JP	J7673			; continue with play frequency

; Subroutine play note
; Input:  DE = value, Cx set if value specified
PLYNOT:
C763E:		LD	B,C			; store note letter
		LD	A,C
		SUB	'A'-1
		ADD	A,A
		LD	C,A			; note letter to note table offset (assumes sharp)
		CALL	FETCHR			; fetch macro string character
		JR	Z,J7665			; end of macro string,
		CP	'#'			; sharp ?
		JR	Z,J7666			; yep, note offset is already sharp
		CP	'+'			; plus ?
		JR	Z,J7666			; yep, note offset is already sharp
		CP	'-'			; minus ?
		JR	Z,J765A			; yep, adjust note offset
		CALL	DECFET			; to previous macro string character
		JR	J7665			; no note extender

J765A:		DEC	C			; decrease note table offset
		LD	A,B			; restore note letter
		CP	'C'			; C- ?
		JR	Z,J7664			; yep, from C# to C- is note table offset -3
		CP	'F'			; F- ?
		JR	NZ,J7665		; nope, note table offset -2
J7664:		DEC	C
J7665:		DEC	C
J7666:		LD	L,15			; offset = octave
		CALL	GETVC2			; get pointer in current voice buffer
		LD	E,(HL)			; store current octave
		LD	B,0
		LD	HL,NOTXLT		; note table
		ADD	HL,BC			; + note offset
		LD	C,(HL)			; frequency offset

; play frequency
J7673:		LD	HL,NOTTAB		; frequency table
		ADD	HL,BC
		LD	A,E			; restore current octave
		LD	E,(HL)
		INC	HL
		LD	D,(HL)			; base frequency
J767B:		DEC	A			; done octaves ?
		JR	Z,J7687			; yep,
		SRL	D
		RR	E			; frequency = frequency / 2
		JR	J767B

J7684:
	IF OPTM = 0
		CALL	FCERR			; illegal function call
	ELSE
		JP	FCERR
	ENDIF

J7687:		ADC	A,E
		LD	E,A
		ADC	A,D
		SUB	E
		LD	D,A			; frequency +1 if Cx
		PUSH	DE			; store frequency
		LD	L,16			; offset = note length
		CALL	GETVC2			; get pointer in current voice buffer
		LD	C,(HL)			; store current note length
		PUSH	HL			; store pointer to note length
		CALL	FETCHR			; fetch macro string character
		JR	Z,J76A9			; end of macro string,
		CALL	VALSC2			; parse optional numeric constant value in macro string
J769C:		LD	A,63+1
		CP	E			; valid note length ?
		JR	C,J7684			; nope, illegal function call
		XOR	A
		OR	D			; note length > 255 ?
		JR	NZ,J7684		; yep, illegal function call
		OR	E			; note length = 0 ?
		JR	Z,J76A9			; yep, use current note length
		LD	C,E			; note length
J76A9:		POP	HL			; restore pointer to note length
		LD	D,0
		LD	B,D			; BC = note length
		INC	HL
		LD	E,(HL)			; current tempo
		PUSH	HL			; store pointer to tempo
		CALL	UMULT			; unsigned integer multiply
		EX	DE,HL
		CALL	CONSIH			; convert to single precision real
		CALL	VMOVAF			; ARG = DAC
		LD	HL,I7754		; number of ticks * tempo (120) * note length (4) *0.5 seconds
		CALL	MOVFM			; DAC = (single)
		CALL	DECDIV			; DAC / ARG
		CALL	FRCINT			; convert DAC to integer
		LD	D,H
		LD	E,L			; store duration (ticks)
J76C8:		CALL	FETCHR			; fetch macro string character
		JR	Z,J76E3			; end of macro string,
		CP	'.'			; dot ?
		JR	NZ,J76E0		; nope, duration determined
		SRL	D
		RR	E
		ADC	HL,DE			; duration = duration*1.5
		LD	A,0E0H
		AND	H			; duration high byte still fits in b4-b0 ?
		JR	Z,J76C8			; yep, continue
		XOR	H
		LD	H,A			; clear b7-b5
		JR	J76E3			; duration is determined

J76E0:		CALL	DECFET			; to previous macro string character
J76E3:		LD	DE,5
		RST	R_DCOMPR		; duration < 5 ?
		JR	C,J76EA			; yep, use duration = 5
		EX	DE,HL			; duration in DE
J76EA:		LD	BC,8-17
		POP	HL			; restore pointer to tempo
		PUSH	HL			; store pointer to tempo
		ADD	HL,BC
		LD	(HL),D			; high byte duration counter
		INC	HL
		LD	(HL),E			; low byte duration counter
		INC	HL
		LD	C,2			; packet length = 2 bytes (duration bytes)
		EX	(SP),HL			; store pointer in packet, restore pointer to tempo
		INC	HL
		LD	E,(HL)			; store current volume
		LD	A,E
		AND	0BFH
		LD	(HL),A			; clear must include volume in packet flag
		EX	(SP),HL			; store pointer to volume, restore pointer in packet
		LD	A,80H			; volume specified in packet
		OR	E
		LD	(HL),A			; packet has volume specified
		INC	HL
		INC	C			; packet size +1
		EX	(SP),HL			; store pointer in packet, restore pointer to volume
		LD	A,E
		AND	40H			; envelope specified ?
		JR	Z,J7716			; nope, skip envelope
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)			; envelop period
		POP	HL			; restore pointer in packet
		LD	(HL),D
		INC	HL
		LD	(HL),E
		INC	HL			; put envelop period in packet
		INC	C
		INC	C			; packet size +2
		DEFB	0FEH			; CP xx, trick to skip next instruction
J7716:		POP	HL			; restore pointer in packet
		POP	DE			; restore frequency
		LD	A,D
		OR	E			; rest note ?
		JR	Z,J7721			; yep, skip frequency
		LD	(HL),D
		INC	HL
		LD	(HL),E			; put frequency in packet
		INC	C
		INC	C			; packet size +2
J7721:		LD	L,7			; offset = size of packet
		CALL	GETVC2			; get pointer in current voice buffer
		LD	(HL),C			; update size of packet
		LD	A,C
		SUB	2			; size with duration bytes
		RRCA
		RRCA
		RRCA				; put in b7-b5
		INC	HL
		OR	(HL)
		LD	(HL),A			; include in high byte duration
		DEC	HL
		LD	A,D
		OR	E			; rest note ?
		JR	NZ,J7741		; nope, skip volume restore
		PUSH	HL			; store pointer to size of packet
		LD	A,(SAVVOL)
		OR	80H			; include volume in packet
		LD	BC,18-7
		ADD	HL,BC
		LD	(HL),A			; restore volume before rest
		POP	HL			; restore pointer to size of packet
J7741:		POP	DE			; restore
		LD	B,(HL)			; size of packet
		INC	HL
J7744:		LD	E,(HL)
		INC	HL
		CALL	C7510			; put in current voice queue
		DJNZ	J7744
		CALL	C7521			; current voice queue full ?
		JP	C,J748E			; yep, wait for the queue to empty
		JP	MCLSCN			; execute next macro command (if any)

	IF INTHZ = 60
I7754:		DEFB	040H,000H,045H,014H		; 14400 (60*120*4*0.5)
	ELSE
I7754:		DEFB	000H,000H,045H,012H		; 12000 (50*120*4*0.5)
	ENDIF


; Subroutine PUT statement
PUTS:
C7758:		LD	B,80H			; PUT
		DEFB	011H			; LD DE,xxxx, trick to skip next instruction

; Subroutine GET statement
GETS:
C775B:		LD	B,0			; GET
		CP	0C7H			; SPRITE token follows ?
	IFDEF MSX1
		JP	Z,J7AAF			; yep, handle PUT SPRITE
	ELSE
		JP	J7993			; MSX2 PUT SPRITE patch
	ENDIF
J7762:		LD	A,B			; GET/PUT flag
		JP	GETPUT			; do random input/output

; Subroutine LOCATE statement
LOCATE:
C7766:		LD	DE,(CSRY)
		PUSH	DE			; store current coordinates as default
		CP	','			; X coordinate not specified ?
		JR	Z,J777A			; yep, use current
		CALL	GETBYT			; evaluate byte operand
		INC	A			; X coordinate (1 based)
		POP	DE			; restore current coordinates
		LD	D,A			; update X
		PUSH	DE			; store coordinates
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		JR	Z,J779F			; yep, set cursor position and quit
J777A:		RST	R_SYNCHR
		DEFB	','			; check for ,
		CP	','			; Y coordinate not specified ?
		JR	Z,J778B			; yep, use current
		CALL	GETBYT			; evaluate byte operand
		INC	A			; Y coordinate (1 based)
		POP	DE			; restore current coordinates
		LD	E,A			; update Y
		PUSH	DE			; store coordinates
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		JR	Z,J779F			; yep, set cursor position and quit
J778B:		RST	R_SYNCHR
		DEFB	','			; check for ,
		CALL	GETBYT			; evaluate byte operand
		AND	A			; 0 (cursor off) ?
		LD	A,'y'
		JR	NZ,J7796		; nope, cursor on (ESC y 5)
		DEC	A			; yep, cursor off (ESC x 5)
J7796:		PUSH	AF
		LD	A,1BH
		RST	OUTDO			; ESC to interpreter output
		POP	AF
		RST	OUTDO			; "x" or "y" to interpreter output
		LD	A,'5'
		RST	OUTDO			; "5" to interpreter output
J779F:		EX	(SP),HL			; store BASIC pointer, restore cursor coordinates
		CALL	POSIT			; set cursor position
		POP	HL			; restore BASIC pointer
		RET

; Subroutine STOP statement (trap)
STOPT:
J77A5:		PUSH	HL			; store BASIC pointer
		LD	HL,TRPTBL+10*3		; STOP entry
		JR	J77CF			; handle trap keyword

; Subroutine SPRITE statement (trap)
SPRITT:
J77AB:		PUSH	HL			; store BASIC pointer
		LD	HL,TRPTBL+11*3		; SPRITE entry
		JR	J77CF			; handle trap keyword

; Subroutine INTERVAL statement
INTS:
J77B1:		RST	R_SYNCHR
		DEFB	'E'
		RST	R_SYNCHR
		DEFB	'R'
		RST	R_SYNCHR
		DEFB	0FFH
		RST	R_SYNCHR
		DEFB	094H			; check for ERVAL
		PUSH	HL			; store BASIC pointer
		LD	HL,TRPTBL+17*3		; INTERVAL entry
		JR	J77CF			; handle trap keyword

; Subroutine STRIG statement
STRIGS:
J77BF:		LD	A,4			; max is 4 
		CALL	C7C08			; evaluate parenthesized byte operand with a maximum
		DEC	HL
		RST	R_CHRGTR		; get next BASIC character
		PUSH	HL			; store BASIC pointer
		LD	D,0
		LD	HL,TRPTBL+12*3
		ADD	HL,DE
		ADD	HL,DE
		ADD	HL,DE			; STRIG entry

; handle trap keyword
J77CF:		CALL	C77FE			; check for trap tokens and act upon
		JR	J77E2			; new statement without CTRL-STOP and trap check

; Subroutine KEY statement (trap)
KEYT:
J77D4:		CALL	GETBYT			; evaluate byte operand
		DEC	A
		CP	9+1			; functionkeynumber 1-10 ?
		JP	NC,FCERR		; nope, illegal function call
		LD	A,(HL)			; ?? function key number already in A ??
		PUSH	HL			; store BASIC pointer
		CALL	C77E8			; set trap entry of functionkey
J77E2:		POP	HL			; restore BASIC pointer
		POP	AF			; dispose return address
		RST	R_CHRGTR		; get next BASIC character
		JP	NEWSTN			; new statement without CTRL-STOP and trap check

; Subroutine set trap entry of functionkey
C77E8:		LD	D,0
		LD	HL,FNKFLG-1
		ADD	HL,DE
		PUSH	HL			; store FNKFLG pointer
		LD	HL,TRPTBL+0*3-(1*3)
		ADD	HL,DE
		ADD	HL,DE
		ADD	HL,DE			; KEY entry
		CALL	C77FE			; check for trap tokens and act upon
		LD	A,(HL)
		AND	01H			; functionkey trap enabled ?
		POP	HL			; restore FNKFLG pointer
		LD	(HL),A			; set FNKFLG if trap enabled
		RET

; Subroutine check for trap tokens and act upon
C77FE:		CP	95H			; ON token ?
		JP	Z,ONTRP			; yep, enable trap
		CP	0EBH			; OFF token ?
		JP	Z,OFFTRP		; yep, disable trap
		CP	90H			; STOP token ?
		JP	Z,STPTRP		; yep, pause trap
		JP	SNERR			; syntax error

; Subroutine trap token ?
ONGOT:
C7810:		SYSHOOK	H_ONGO
		LD	BC,0*256+10		; trap base number = 0, maximum number of traps = 10
		CP	0CCH			; KEY token ?
		RET	Z			; yep, quit (trap token)
		LD	BC,10*256+1		; trap base number = 10, maximum number of traps = 1
		CP	90H			; STOP token ?
		RET	Z			; yep, quit (trap token)
		INC	B			; trap base number = 11, maximum number of traps = 1
		CP	0C7H			; SPRITE token ?
		RET	Z			; yep, quit (trap token)
		CP	0FFH			; function token ?
		RET	C			; nope, quit (no trap token)
		PUSH	HL			; store BASIC pointer
		RST	R_CHRGTR		; get next BASIC character
		CP	0A3H			; TRIG token ?
		JR	Z,J7833			; yep, quit (trap token)
		CP	85H			; INT token ?
		JR	Z,J7838			; yep, on INTERVAL
J7830:		POP	HL			; restore BASIC pointer
		SCF				; no trap token
		RET

J7833:		POP	BC			; dispose stored BASIC pointer
		LD	BC,12*256+5		; trap base number = 12, maximum number of traps = 5
		RET

J7838:		RST	R_CHRGTR		; get next BASIC character
		CP	'E'
		JR	NZ,J7830
		POP	BC			; dispose stored BASIC pointer
		RST	R_CHRGTR		; get next BASIC character
		RST	R_SYNCHR
		DEFB	'R'
		RST	R_SYNCHR
		DEFB	0FFH
		RST	R_SYNCHR
		DEFB	094H
		RST	R_SYNCHR
		DEFB	0EFH			; check for RVAL=
		CALL	GETUIN			; evaluate address operand
		LD	A,D
		OR	E			; interval value = 0 ?
		JP	Z,FCERR			; yep, illegal function call
		EX	DE,HL
		LD	(INTVAL),HL		; update interval value
		LD	(INTCNT),HL		; reset interval counter
		EX	DE,HL
		LD	BC,256*17+1		; trap base number = 17, maximum number of traps = 1
		DEC	HL
		RET

; Subroutine update trap handler line number
SETGSB:
C785C:		PUSH	HL
		LD	B,A
		ADD	A,A
		ADD	A,B
		LD	L,A
		LD	H,0			; *3
		LD	BC,TRPTBL+1
		ADD	HL,BC
		LD	(HL),E
		INC	HL
		LD	(HL),D
		POP	HL
		RET

; Subroutine KEY statement
KEYS:
C786C:		CP	93H			; LIST token ?
		JR	NZ,J78AE		; nope, other KEY statement variants

		; KEY LIST
		RST	R_CHRGTR		; get next BASIC character
		PUSH	HL			; store BASIC pointer
		LD	HL,FNKSTR
		LD	C,10			; 10 function keys
J7877:		LD	B,16			; maximum function definition length
J7879:		LD	A,(HL)			; get character
		INC	HL
		CALL	CNVCHR			; graphic header character ?
		JR	C,J7891			; nope,
		DEC	B
		JR	Z,J789E			; no more characters, ignore graphic header and finish this definition
		LD	A,(HL)			; get character
		INC	HL
		LD	E,A			; store character
		CALL	CNVCHR			; graphic character ?
		JR	Z,J7891			; nope,
		LD	A,1			; graphic character header
		RST	OUTDO			; MSX to interpreter output
		LD	A,E			; restore graphic character
		JR	J789B

J7891:		CP	7FH			; DEL ?
		JR	Z,J7899			; yep, display space
		CP	20H			; control character ?
		JR	NC,J789B		; nope, display character
J7899:		LD	A,' '
J789B:		RST	OUTDO			; char to interpreter output
		DJNZ	J7879			; next definition character
J789E:		CALL	CRDO			; newline to interpreter output
		DEC	C			; done all function keys ?
		JR	NZ,J7877		; nope, next
		POP	HL			; restore BASIC pointer
		RET

	; function key display on
J78A6:		RST	R_CHRGTR		; get next BASIC character
		JP	DSPFNK

	; function key display off
J78AA:		RST	R_CHRGTR		; get next BASIC character
		JP	ERAFNK

; other KEY statement variants
J78AE:		CP	'('
		JP	Z,KEYT			; yep, KEY statement (trap)
		CP	95H			; ON token ?
		JR	Z,J78A6			; yep, enable display functionkeys
		CP	0EBH			; OFF token ?
		JR	Z,J78AA			; yep, disable display functionkeys
		CALL	GETBYT			; evaluate byte operand
		DEC	A
		CP	9+1			; functionkeynumber 1-10 ?
		JP	NC,FCERR		; nope, illegal function call
		EX	DE,HL			; store BASIC pointer
		LD	L,A
		LD	H,0
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL			; * 16
		LD	BC,FNKSTR
		ADD	HL,BC
		PUSH	HL			; store pointer in function key definition
		EX	DE,HL			; restore BASIC pointer
		RST	R_SYNCHR
		DEFB	','			; check for ,
		CALL	FRMEVL			; evaluate expression
		PUSH	HL			; store BASIC pointer
		CALL	FRESTR			; free temporary string with type check
		LD	B,(HL)
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		POP	HL			; restore BASIC pointer
		EX	(SP),HL			; store BASIC pointer, restore pointer in function key definition
		LD	C,16-1			; maximum defintion length
		LD	A,B
		AND	A			; empty defintion string ?
		JR	Z,J78F5			; yep,
J78E8:		LD	A,(DE)
		AND	A			; NULL character ?
		JP	Z,FCERR			; yep, illegal function call
		LD	(HL),A
		INC	DE
		INC	HL
		DEC	C			; decrease length left
		JR	Z,J78FA			; definition length reached, ignore remainer and put end marker
		DJNZ	J78E8			; next definition character
J78F5:		LD	(HL),B
		INC	HL
		DEC	C
		JR	NZ,J78F5		; fill remainer of defintion with end markers
J78FA:		LD	(HL),C			; put end marker
		CALL	FNKSB			; update function key display
		POP	HL			; restore BASIC pointer
		RET

; Subroutine TIME function
TIMEF:
J7900:		RST	R_CHRGTR		; get next BASIC character
		PUSH	HL			; store BASIC pointer
		LD	HL,(JIFFY)		; current JIFFY value
		CALL	CONSUI			; convert unsigned integer to single real
		POP	HL			; restore BASIC pointer
		RET

; Subroutine CSRLIN function
CSRLIN:
J790A:		RST	R_CHRGTR		; get next BASIC character
		PUSH	HL			; store BASIC pointer
		LD	A,(CSRY)
		JR	J7932			; decrease, convert signed byte to integer and store in DAC, restore BASIC pointer and quit

; Subroutine TIME statement
TIMES:
C7911:		RST	R_SYNCHR
		DEFB	0EFH			; check for =
		CALL	GETUIN			; evaluate address operand
		LD	(JIFFY),DE		; update JIFFY
		RET

; Subroutine PLAY function
PLAYF:
J791B:		RST	R_CHRGTR		; get next BASIC character
		LD	A,3			; max is 3
		CALL	C7C08			; evaluate parenthesized byte operand with a maximum
		PUSH	HL			; store BASIC pointer
		LD	A,(MUSICF)		; voice active flags
		DEC	E			; operand = 0 ?
		JP	M,J7938			; yep,
J7929:		RRCA				; this voice active ?
		DEC	E			; this voice ?
		JP	P,J7929			; nope, next
		LD	A,0
		JR	NC,J7933		; voice not active, return 0
J7932:		DEC	A
J7933:		CALL	CONIA			; convert signed byte to integer and store in DAC
		POP	HL			; restore BASIC pointer
		RET

J7938:		AND	07H			; all voices inactive ?
		JR	Z,J7933			; yep, return 0
		LD	A,0FFH
		JR	J7933			; return 255

; Subroutine STICK function
STICK:
C7940:		CALL	CONINT			; check for byte value
		CP	03H
		JR	NC,J7951
		CALL	GTSTCK
		JR	J7966

; Subroutine TRIG function
TRIG:
C794C:		CALL	CONINT			; check for byte value
		CP	05H
J7951:		JP	NC,FCERR		; illegal function call
		CALL	GTTRIG
J7957:		JP	CONIA			; convert signed byte to integer and store in DAC

; Subroutine PDL function
PDL:
C795A:		CALL	CONINT			; check for byte value
		DEC	A
		CP	0CH
		JR	NC,J7951
		INC	A
		CALL	GTPDL
J7966:		JP	SNGFLT			; byte to DAC

; Subroutine PAD function
PAD:
C7969:		CALL	CONINT			; check for byte value
	IFDEF MSX1
		CP	8
	ELSE
		CP	20
	ENDIF
		JR	NC,J7951
		PUSH	AF
		CALL	GTPAD
		LD	B,A
		POP	AF
	IFDEF MSX1
		AND	03H			; ignore port bit
		DEC	A			; 0 and 3 are boolean
	ELSE
		CALL	C7B3E			; MSX2 patch routine for extra pad numbers
	ENDIF	
		CP	2
		LD	A,B
		JR	C,J7966
		JR	J7957		; 0 or -1

; Subroutine COLOR statement
COLOR:
	IFDEF MSX1
C7980:		ld	bc,FCERR
		push	bc
		ld	de,(FORCLR)
		push	de
		cp	02CH
		jr	z,A799A
		call	GETBYT
		pop	de
		cp	010H
		ret	nc
		ld	e,a
		push	de
		dec	hl
		RST	R_CHRGTR		; end of statement ?
		jr	z,A79BC			; yep,
A799A:		RST	R_SYNCHR
		defb	','
		jr	z,A79BC
		cp	02CH
		jr	z,A79AF
		call	GETBYT
		pop	de
		cp	010H
		ret	nc
		ld	d,a
		push	de
		dec	hl
		RST	R_CHRGTR		; end of statement ?
		jr	z,A79BC			; yep,
A79AF:		RST	R_SYNCHR
		defb	','
		call	GETBYT
		pop	de
		cp	010H
		ret	nc
		ld	(BDRCLR),a
		push	de
A79BC:		pop	de
		pop	af
		push	hl
		ex	de,hl
		ld	(FORCLR),hl
		ld	a,l
		ld	(ATRBYT),a
		call	CHGCLR
		pop	hl
		ret
	ELSE ; MSX2
C7980:		LD	IX,S_COLOR
		JP	EXTROM			; MSX2 patch routine COLOR statement

; Patch: stop vdp command in error handling routine
C7987:		LD	(NLONLY),A		; not loading basic program, close I/O channels when requested
		DI
		OUT	(99H),A
		LD	A,0AEH
		OUT	(99H),A			; cancel any vdp command in progress
		EI
		RET

; Patch: handle extended PUT/GET statement
J7993:		JP	Z,J7AAF			; SPRITE token follows,
		LD	IX,S_GETPUT
		CALL	EXTROM
		RET	NC			; handled by subrom, quit
		JP	J7762			; back to MSX 1 style PUT/GET

; Bugfix: adjust VARTAB correctly
C79A1:		CALL    CHEAD			; setup BASIC linelinks from this point
		INC	HL
		LD	(VARTAB),HL
		RET
		
		ALIGN	79CCH
	ENDIF

; Subroutine SCREEN statement
SCREEN:
	IFDEF MSX1
C79CC:		SYSHOOK	H_SCRE
		cp	02CH
		jr	z,A79EA
		call	GETBYT
		cp	004H
		jp	nc,FCERR
		push	hl
		call	CHGMOD
		ld	a,(LINLEN)
		ld	e,a
		call	MORCP2
		pop	hl
		dec	hl
		RST	R_CHRGTR		; end of statement ?
		ret	z			; yep, quit
A79EA:		RST	R_SYNCHR
		defb	','
		cp	','
		jr	z,A7A09
		call	GETBYT
		cp	004H
		jp	nc,FCERR
		ld	a,(RG1SAV)
		and	0FCH
		or	e
		ld	(RG1SAV),a
		push	hl
		call	CLRSPR
		pop	hl
		dec	hl
		RST	R_CHRGTR		; end of statement ?
		ret	z			; yep, quit
A7A09:		RST	R_SYNCHR
		defb	','
		cp	02CH
		jr	z,A7A18
		call	GETBYT
		ld	(CLIKSW),a
		dec	hl
		RST	R_CHRGTR		; end of statement ?
		ret	z			; yep, quit
A7A18:		RST	R_SYNCHR
		defb	','
		cp	02CH
		jr	z,A7A24
		call	GETCSP
		dec	hl
		RST	R_CHRGTR		; end of statement ?
		ret	z			; yep, quit
A7A24:		RST	R_SYNCHR
		defb	','
		call	GETBYT
		ld	(NTMSXP),a
		ret
	ELSE ; MSX2
C79CC:		SYSHOOK	H_SCRE
		ld	IX,S_SCREEN
		jp	EXTROM			; MSX2 patch routine SCREEN statement
		
; patch: extend PSET statement for MSX2 screens
J79D6:		CALL	CHKNEW
		JR	C,J79E2
		LD	IX,S_PSET
		JP	EXTROM

J79E2:		PUSH	AF
		CALL	SCAND			; evaluate simple graphic coordinatepair
		JP	J57F1			; resume orginal PSET routine

; patch: extend LINE statement for MSX2 screens
J79E9:		CALL	CHKNEW
		JR	C,J79F5
		LD	IX,S_GLINE
		JP	EXTROM

J79F5:		CALL	SCAN1			; evaluate complex graphic coordinatepair
		JP	J58AA			; resume orginal LINE routine

; patch: extend PAINT statement for MSX2 screens
J79FB:		CALL	CHKNEW
		JR	C,J7A07
		LD	IX,S_PAINT
		JP	EXTROM

J7A07:		CALL	SCAN1			; evaluate complex graphic coordinatepair
		JP	J59C8			; resume orginal PAINT routine

; patch: extend draw line for MSX2 screens
J7A0D:		CALL	CHKNEW
		JR	C,J7A19
		LD	IX,S_DOGRPH
		JP	EXTROM

J7A19:		CALL	SCALXY
		JP	J5942			; resume orginal draw line routine

; bugfix: CLEAR statement
J7A1F:		JP	P,J64E5
		DEC	HL
		JP	J64EA
		
		ALIGN	7A2DH
	ENDIF ; MSX1/MSX2
	
GETCSP:
A7A2D:		call	GETBYT
		dec	a
		cp	002H
		jp	nc,FCERR
		push	hl
		ld	bc,5
		and	a
		ld	hl,CS1200
		jr	z,A7A41
		add	hl,bc
A7A41:		ld	de,LOW_
		ldir
		pop	hl
		ret

; Subroutine SPRITE statement
SPRITE:
C7A48:		CP	'$'
		JP	NZ,SPRITT
		LD	A,(SCRMOD)
		AND	A
		JP	Z,FCERR			; illegal function call
		CALL	C7AA0
		PUSH	DE
		CALL	FRMEQL			; evaluate = expression
		EX	(SP),HL
		PUSH	HL
		CALL	FRESTR			; free temporary string with type check
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		CALL	GSPSIZ
		LD	C,A
		LD	B,00H
		DEC	HL
		DEC	HL
		DEC	A
		CP	(HL)
		LD	A,(HL)
		JR	C,J7A7D
		POP	HL
		PUSH	HL
		PUSH	AF
		XOR	A
	IFDEF MSX1
		CALL	FILVRM
	ELSE
		CALL	BIGFIL			; MSX2 patch: use BIGFIL
	ENDIF
		POP	AF
		AND	A
		LD	C,A
		LD	B,00H
J7A7D:		EX	DE,HL
		POP	DE
		CALL	NZ,LDIRVM
		POP	HL
		RET

; Subroutine SPRITE function
SPRITF:
J7A84:		CALL	C7A9F
		PUSH	HL
		PUSH	DE
		CALL	GSPSIZ
		LD	C,A
		LD	B,00H
		PUSH	BC
		CALL	STRINI			; allocate temporary string
		LD	HL,(DSCTMP+1)
		EX	DE,HL
		POP	BC
		POP	HL
		CALL	LDIRMV
		JP	PUTNEW			; push temporary descriptor to temporary descriptor heap and quit

; Subroutine 
C7A9F:		RST	R_CHRGTR		; get next BASIC character

; Subroutine 
C7AA0:		RST	R_SYNCHR
		DEFB	'$'			; check for $
		LD	A,255			; max is 255
		CALL	C7C08			; evaluate parenthesized byte operand with a maximum
		PUSH	HL
		LD	A,E
		CALL	CALPAT
		EX	DE,HL
		POP	HL
		RET

; Subroutine PUT/GET SPRITE
J7AAF:		DEC	B			; PUT ?
		JP	M,FCERR			; nope, illegal function call
	IFDEF MSX1
		ld	a,(SCRMOD)
	ELSE
		JP	J7CF4			; MSX2 patch PUT SPRITE statement
	ENDIF
J7AB6:		and	a
		JP	Z,FCERR			; illegal function call
		RST	R_CHRGTR		; get next BASIC character
		CALL	GETBYT			; evaluate byte operand
		CP	32			; planenumber 0-31 ?
		JP	NC,FCERR		; nope, illegal function call
		PUSH	HL
		CALL	CALATR
		EX	(SP),HL
		RST	R_SYNCHR
		DEFB	','			; check for ,
		CP	','
		JR	Z,J7AF9
		CALL	SCAN1			; evaluate complex graphic coordinatepair
		EX	(SP),HL
		LD	A,E
		CALL	WRTVRM
		LD	A,B
		ADD	A,A
		LD	A,C
		LD	B,00H
		JR	NC,J7AE1
		ADD	A,20H
		LD	B,80H
J7AE1:		INC	HL
		CALL	WRTVRM
		INC	HL
		INC	HL
		CALL	RDVRM
		AND	0FH
		OR	B
		CALL	WRTVRM
		DEC	HL
		DEC	HL
		DEC	HL
		EX	(SP),HL
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		POP	BC
		RET	Z			; yep, quit
		PUSH	BC
J7AF9:		RST	R_SYNCHR
		DEFB	','			; check for ,
		CP	','
		JR	Z,J7B1D
		CALL	GETBYT			; evaluate byte operand
		CP	16			; colornumber 0-15 ?
		JP	NC,FCERR		; nope, illegal function call
		EX	(SP),HL
		INC	HL
		INC	HL
		INC	HL
		CALL	RDVRM
		AND	80H
		OR	E
		CALL	WRTVRM
		DEC	HL
		DEC	HL
		DEC	HL
		EX	(SP),HL
		DEC	HL
		RST	R_CHRGTR		; end of statement ?
		POP	BC
		RET	Z			; yep, quit
		PUSH	BC
J7B1D:		RST	R_SYNCHR
		DEFB	','			; check for ,
		CALL	GETBYT			; evaluate byte operand
		CALL	GSPSIZ
		LD	A,E
		JR	NC,J7B2F		; 8x8 sprite, spritenumber ok
		CP	64			; 16x16 sprite, sprite number 0-63 ?
		JP	NC,FCERR		; nope, illegal function call
		ADD	A,A
		ADD	A,A			; *4 = spritenumber used by the VDP
J7B2F:		EX	(SP),HL
		INC	HL
		INC	HL
		CALL	WRTVRM
		POP	HL
		RET

; Subroutine VDP statement
VDPS:
	IFDEF MSX1
C7B37:		ld	a,7
		call	C7C08
		push	de
		RST	R_SYNCHR
		defb	0EFH			; check for =
		call	GETBYT
		pop	bc
		ld	b,a
		jp	WRTVDP
	ELSE
C7B37:		LD	IX,S_VDP
		JP	EXTROM			; MSX2 patch: VDP statement

; Patch: PAD function for extra pad numbers
C7B3E:		ADD	A,04H
		AND	0F3H
		DEC	A
		RET
		
		ALIGN	7B47H
	ENDIF

; Subroutine VDP function
VDPF:
	IFDEF MSX1
J7B47:		RST	R_CHRGTR
		ld	a,8
		call	C7C08
		push	hl
		ld	d,000H
		ld	hl,RG0SAV
		add	hl,de
		ld	a,(hl)
		call	SNGFLT
		pop	hl
		ret
	ELSE
J7B47:		LD	IX,S_VDPF
		JP	EXTROM			; MSX2 patch: VDP function
		ALIGN	7B5AH
	ENDIF

; Subroutine BASE statement
BASES:
	IFDEF MSX1
C7B5A:		ld	a,19
		call	C7C08
		ld	d,000H
		push	de
		RST	R_SYNCHR
		defb	0EFH			; check for =
		call	FRMEVL			; evaluate expression
		ex	(sp),hl
		push	hl
		call	A7BFE
		ld	c,l
		ld	b,h
		pop	hl
		ld	a,l
		push	af
		add	hl,hl
		ex	de,hl
		ld	hl,T7BA3
		add	hl,de
		ld	a,c
		and	(hl)
		jr	nz,A7B7E
		inc	hl
		ld	a,b
		and	(hl)
A7B7E:		jp	nz,FCERR
		ld	hl,TXTNAM
		add	hl,de
		ld	(hl),c
		inc	hl
		ld	(hl),b
		pop	af
		ld	e,0FFH
A7B8B:		inc	e
		sub	005H
		jr	nc,A7B8B
		ld	a,(SCRMOD)
		cp	e
		call	z,A7B99
		pop	hl
		ret

A7B99:		dec	a
		jp	m,SETTXT
		jp	z,SETGRP
		jp	SETMLT

T7BA3:		dw	003FFH
		dw	0003FH
		dw	007FFH
		dw	0007FH
		dw	007FFH
		dw	003FFH
		dw	0003FH
		dw	007FFH
		dw	0007FH
		dw	007FFH
		dw	003FFH
		dw	01FFFH
		dw	01FFFH
		dw	0007FH
		dw	007FFH
		dw	003FFH
		dw	0003FH
		dw	007FFH
		dw	0007FH
		dw	007FFH
	ELSE ; MSX2
C7B5A:		LD	IX,S_BASE
		JP	EXTROM			; MSX2 patch: BASE statement
		
; Subroutine: system initialization, part 2
; Remark: System initialization routine continues here
; MSX1 has this in page 0, with MSX2 it is moved here because of space left in page 0
J7B61:		LD	HL,0
		ADD	HL,SP
		LD	A,H
		OUT	(0A8H),A		; select primary slot RAM in page 2 and 3
		LD	A,L
		LD	(D_FFFF),A		; if primary slot is expanded, select secondary slot RAM in page 2 and 3
		LD	A,C			; save expansion flag
		LD	BC,0C77H
		LD	DE,VARWRK+1
		LD	HL,VARWRK
		LD	(HL),0
		LDIR				; clear system variables
		LD	C,A			; expansion flag
		LD	B,4			; 4 primary slots
		LD	HL,EXPTBL+3
J7B80:		RR	C
		SBC	A,A
		AND	80H			; 080H for expanded slot, 000H for non-expanded slot
		LD	(HL),A			; set expansion flag
		DEC	HL
		DJNZ	J7B80			; next slot
		IN	A,(0A8H)
		LD	C,A			; save current primary slotregister
		XOR	A
		OUT	(0A8H),A		; primary slot 0 in all pages
		LD	A,(D_FFFF)
		CPL
		LD	L,A			; current secondary slotregister slot 0
		LD	A,40H
		OUT	(0A8H),A		; primary slot 1 in page 3, primary slot 0 in other pages
		LD	A,(D_FFFF)
		CPL
		LD	H,A			; current secondary slotregister slot 1
		LD	A,80H
		OUT	(0A8H),A		; primary slot 2 in page 3, primary slot 0 in other pages
		LD	A,(D_FFFF)
		CPL
		LD	E,A			; current secondary slotregister slot 2
		LD	A,0C0H
		OUT	(0A8H),A		; primary slot 3 in page 3, primary slot 0 in other pages
		LD	A,(D_FFFF)
		CPL
		LD	D,A			; current secondary slotregister slot 3
		LD	A,C
		OUT	(0A8H),A		; restore primary slotregister
		LD	(SLTTBL+0),HL
		EX	DE,HL
		LD	(SLTTBL+2),HL		; save secondary slotregisters
		IM	1			; switch to interrupt mode 1 (Z80 defaults to mode 0)
		JP	INIT			; start BASIC interpreter

	IF INTHZ = 50
; Subroutine: patchroutine for PAL setup of VDP (part 2)
PTCPA2:		LD	HL,0A0F9H
PTCPA1:		DEC	HL
		LD	A,H
		OR	L
		JR	NZ,PTCPA1		; wait
		JP	J0431			; resume orginal routine
	  ENDIF
		
		ALIGN	7BCBH
	ENDIF ; MSX1/MSX2

; Subroutine BASE function
BASEF:
	IFDEF MSX1
J7BCB:		RST	R_CHRGTR
		ld	a,19
		call	C7C08
		push	hl
		ld	d,000H
		ld	hl,TXTNAM
		add	hl,de
		add	hl,de
A7BD9:		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		call	CONSUI
		pop	hl
		ret
	ELSE ; MSX2
J7BCB:		LD	IX,S_BASEF
		JP	EXTROM			; MSX2 patch: BASE function
        IF INTHZ = 50
; patch: routine for PAL setup of VDP (part 1)
PTCPAL:		LD	A,50H
		OUT	(0AAH),A
		LD	A,2
		OUT	(99H),A
		LD	A,89H
		OUT	(99H),A
		JP	PTCPA2
        ENDIF
		ALIGN	7BE2H
	ENDIF ; MSX1/MSX2

; Subroutine VPOKE statement
VPOKE:
	IFDEF MSX1
C7BE2:		call	FRMEVL			; evaluate expression
		push	hl
		call	A7BFE
		ex	(sp),hl
		RST	R_SYNCHR
		defb	','			; check for ,
		call	GETBYT
		ex	(sp),hl
		call	WRTVRM
		pop	hl
		ret
	ELSE
C7BE2:		LD	IX,S_VPOKE
		JR	J7BF2			; MSX2 patch: VPOKE statement
		
; Subroutine display prompt
C7BE8:		LD	IX,S_PROMPT
		JR	J7BF2
		
; Subroutine initialize BASIC screen + print BASIC version
C7BEE:		LD	IX,S_SETSCR
J7BF2:		JP	EXTROM
	
		ALIGN	7BF5H
	ENDIF
	
; Subroutine VPEEK function
VPEEK:
	IFDEF MSX1
C7BF5:		call	A7BFE
		call	RDVRM
		jp	SNGFLT

A7BFE:		call	FRCINT
		ld	de,04000H
		RST	R_DCOMPR
		ret	c
		jr	J7C73
	ELSE
C7BF5:		LD	IX,S_VPEEK
		JP	EXTROM			; MSX2 patch: VPEEK function
		ALIGN	7C08H
	ENDIF

; Subroutine evaluate parenthesized byte operand with a maximum
; Input:  A = maximum
; Output: A = value, E = value
C7C08:		PUSH	AF
		RST	R_SYNCHR
		DEFB	'('			; check for (
		CALL	GETBYT			; evaluate byte operand
		POP	AF
		CP	E
		JR	C,J7C73
		RST	R_SYNCHR
		DEFB	')'			; check for )
		LD	A,E
		RET

; Subroutine DSKO$ statement
DSKOS:
C7C16:		SYSHOOK	H_DSKO
		JR	J7C73

; Subroutine SET statement
SETS:
	IFDEF MSX1
C7C1B:		SYSHOOK	H_SETS
		JR	J7C73
	ELSE
C7C1B:		JP	J7CE3			; MSX2 patch: SET statement
		ALIGN	7C20H
	ENDIF

; Subroutine NAME statement
NAME:
C7C20:		SYSHOOK	H_NAME
		JR	J7C73

; Subroutine KILL statement
KILL:
C7C25:		SYSHOOK	H_KILL
		JR	J7C73

; Subroutine IPL statement
IPL:
C7C2A:		SYSHOOK	H_IPL
		JR	J7C73

; Subroutine COPY statement
COPY:
	IFDEF MSX1
C7C2F:		SYSHOOK	H_COPY
		JR	J7C73
	ELSE
C7C2F:		JP	J7D03			; MSX2 patch: COPY statement
		ALIGN	7C34H
	ENDIF

; Subroutine CMD statement
CMD:
C7C34:		SYSHOOK	H_CMD
		JR	J7C73

; Subroutine DSKF function
DSKF:
C7C39:		SYSHOOK	H_DSKF
		JR	J7C73

; Subroutine DSKI$ function
DSKIS:
J7C3E:		SYSHOOK	H_DSKI
		JR	J7C73

; Subroutine ATTR$ function
ATTRS:
J7C43:		SYSHOOK	H_ATTR
		JR	J7C73

; Subroutine LSET statement
LSET:
C7C48:		SYSHOOK	H_LSET
		JR	J7C73

; Subroutine RSET statement
RSET:
C7C4D:		SYSHOOK	H_RSET
		JR	J7C73

; Subroutine FIELD statement
FIELD:
C7C52:		SYSHOOK	H_FIEL
		JR	J7C73

; Subroutine MKI$ function
MKIS:
C7C57:		SYSHOOK	H_MKIS
		JR	J7C73

; Subroutine MKS$ function
MKSS:
C7C5C:		SYSHOOK	H_MKSS
		JR	J7C73

; Subroutine MKD$ function
MKDS:
C7C61:		SYSHOOK	H_MKDS
		JR	J7C73

; Subroutine CVI function
CVI:
C7C66:		SYSHOOK	H_CVI
		JR	J7C73

; Subroutine CVS function
CVS:
C7C6B:		SYSHOOK	H_CVS
		JR	J7C73

; Subroutine CVD function
CVD:
C7C70:		SYSHOOK	H_CVD
J7C73:		JP	FCERR			; illegal function call

; ------------------------------------------------------------------------------
; INIT.MAC
; BASIC INIT
; ------------------------------------------------------------------------------

		ALIGN	07C76H

	IFDEF MSX1
		INCLUDE	"100/init.asm"
	ELSE
		INCLUDE	"200/init.asm"
	ENDIF

; ----------------------------------------------------------
; Common init routines
; ----------------------------------------------------------

		ALIGN	7DF6H

; Subroutine start BASIC program in extension ROM
J7DF6:		CALL	ATRSL2			; translate extension page counter to address and slot id
		CALL	ENASLT			; enable slot on page 2
		LD	HL,(VARTAB)
		LD	DE,0C000H
		RST	R_DCOMPR		; VARTAB in page 3 ?
		JR	NC,J7E09		; yep, skip
		EX	DE,HL
		LD	(VARTAB),HL		; VARTAB = 0C000H (otherwise VARTAB would point into ROM)
J7E09:		LD	HL,(08000H+8)
		INC	HL
		LD	(TXTTAB),HL		; update pointer to start of BASIC program
		LD	A,H
		LD	(BASROM),A		; flag execution of BASIC program in ROM (cannot be aborted)

; Entrypoint used by diskrom to start extension ROM with BASIC program
M7E14:		CALL	RUNC			; initialize interpreter, basic pointer at start of program
		JP	NEWSTT			; execute new statement

; Subroutine read word from extension ROM
; Input:  HL = address, C = slot id
; Output: HL = address + 2, DE = word, Zx set if word is zero
RDWEXP:
C7E1A:		CALL	C7E1E			; read byte from extension ROM
		LD	E,D			; store low byte
C7E1E:		LD	A,C			; slot id
		PUSH	BC			; store slot id
		PUSH	DE			; store word
		CALL	RDSLT			; read byte from slot
		POP	DE			; restore word
		POP	BC			; restore slot id
		LD	D,A			; update word
		OR	E			; is word 0 ?
		INC	HL			; update address
		RET

; Subroutine translate extension page counter to address and slot id
; Input:  B = extension page counter
ATRSL2:
C7E2A:		LD	A,4*4*4
		SUB	B			; translate to SLTATR entry number

; Subroutine translate SLTATR entry number to address and slot id
; Input:  A = SLTATR entry number
; Output: HL = address (of page), A = slot id
ATRSLI:
C7E2D:		LD	B,A			; store SLTATR entry number
		LD	H,0			; address = 00xxH (page 0)
		RRA
		RR	H
		RRA
		RR	H			; update address (page)
		RRA
		RRA
		AND	00000011b		; primary slot
		LD	C,A			; store primary slot
		LD	A,B			; restore SLTATR entry number
		LD	B,0
		PUSH	HL			; store address
		LD	HL,EXPTBL
		ADD	HL,BC
		AND	00001100b		; secondary slot
		OR	C			; + primary slot
		LD	C,A
		LD	A,(HL)			; expanded slot flag
		POP	HL			; restore address
		OR	C			; + expanded slot flag = slot id
		RET

; Subroutine MAX statement
MAXS:
C7E4B:		RST	R_SYNCHR
		DEFB	0B7H
		RST	R_SYNCHR
		DEFB	0EFH			; check for FILES=
		CALL	GETBYT			; evaluate byte operand
		JP	NZ,SNERR		; not end of statement, syntax error
		CP	15+1			; number of I/O channels <16 ?
		JP	NC,FCERR		; nope, illegal function call
		LD	(TEMP),HL		; store BASIC pointer in TEMP
		PUSH	AF			; store number of I/O channels
		CALL	CLSALL			; close all I/O channels
		POP	AF			; restore number of I/O channels
		CALL	ALCFIL			; allocate I/O channels
		CALL	CLEAR2			; initialize interpreter, BASIC pointer from TEMP
		JP	NEWSTT			; execute new statement

; Subroutine allocate I/O channels
; Input:  A = number of user I/O channels
ALCFIL:
C7E6B:		PUSH	AF
		LD	HL,(HIMEM)
		LD	DE,-(256+9+2)
J7E72:		ADD	HL,DE
		DEC	A
		JP	P,J7E72
		EX	DE,HL			; calculate location I/O channel pointer table
		LD	HL,(STKTOP)
		LD	B,H
		LD	C,L
		LD	HL,(MEMSIZ)
		LD	A,L
		SUB	C
		LD	L,A
		LD	A,H
		SBC	A,B
		LD	H,A			; size of the string heap
		POP	AF
		PUSH	HL
		PUSH	AF
		LD	BC,140
		ADD	HL,BC
		LD	B,H
		LD	C,L			; size of the string heap +140
		LD	HL,(VARTAB)		; start of the simple variables
		ADD	HL,BC			; + size
		RST	R_DCOMPR		; does this fit ?
		JP	NC,OMERR		; nope, out of memory
		POP	AF
		LD	(MAXFIL),A		; set number of I/O channels (excluding I/O channel 0)
		LD	L,E
		LD	H,D
		LD	(FILTAB),HL		; update location I/O channel pointer table
		DEC	HL
		DEC	HL			; ?? why need a extra byte ??
		LD	(MEMSIZ),HL		; start of the string heap
		POP	BC			; size of the string heap
		LD	A,L
		SUB	C
		LD	L,A
		LD	A,H
		SBC	A,B
		LD	H,A
		LD	(STKTOP),HL		; start of Z80 stack, end of string heap
		DEC	HL
		DEC	HL
		POP	BC			; return address
		LD	SP,HL			; new stack with dummy word on stack
		PUSH	BC			; return address on stack
		LD	A,(MAXFIL)
		LD	L,A
		INC	L			; number of I/O channels
		LD	H,0
		ADD	HL,HL			; *2
		ADD	HL,DE
		EX	DE,HL
		PUSH	DE			; start of I/O channel buffers
		LD	BC,-2+256+9+2
J7EC2:		LD	(HL),E
		INC	HL
		LD	(HL),D
		INC	HL			; pointer to I/O channel buffer
		EX	DE,HL
		LD	(HL),0			; I/O channel closed
		ADD	HL,BC			; to the next I/O channel buffer
		EX	DE,HL
		DEC	A
		JP	P,J7EC2			; next I/O channel
		POP	HL			; start of I/O channel buffer
		LD	BC,9
		ADD	HL,BC
		LD	(NULBUF),HL		; pointer to the I/O channel 0 buffer
		RET

T7ED8:		DEFB	"MSX  system"
		DEFB	0

T7EE4:		DEFB	"version 1.0",13,10
		DEFB	0

T7EF2:		DEFB	"MSX BASIC "
		DEFB	0
T7EFD:		DEFB	"Copyright 1983 by Microsoft",13,10
		DEFB	0

I7F1B:		DEFB	" Bytes free"
		DEFB	0

; Initial Workarea variables
I7F27:
		DEPHASE
		PHASE  VARWRK

RDPRIM: 	OUT	(0A8H),A		; update primary slot register
		LD	E,(HL)			; read byte from slot
		JR	J7F2F			; restore primary slot register

WRPRIM: 	OUT	(0A8H),A		; update primary slot register
		LD	(HL),E			; write byte to slot
J7F2F:		LD	A,D			; original primary slot register
		OUT	(0A8H),A		; update primary slot register
		RET

CLPRIM: 	OUT	(0A8H),A		; update primary slot register
		EX	AF,AF'			; restore AF
		CALL	CLPRM1			; call subroutine in slot
		EX	AF,AF'			; store AF
		POP	AF			; restore original primary slot register
		OUT	(0A8H),A		; update primary slot register
		EX	AF,AF'			; restore AF
		RET

CLPRM1: 	JP	(IX)

USRTAB: 	defw	FCERR			; illegal function call
		defw	FCERR			; illegal function call
		defw	FCERR			; illegal function call
		defw	FCERR			; illegal function call
		defw	FCERR			; illegal function call
		defw	FCERR			; illegal function call
		defw	FCERR			; illegal function call
		defw	FCERR			; illegal function call
		defw	FCERR			; illegal function call
		defw	FCERR			; illegal function call

LINL40:
	IF BASVER = 0
		defb	39
	ELSE
		defb	37
	ENDIF

LINL32: 	defb	29

LINLEN:
	IF BASVER = 0
		defb	29
	ELSE
		defb	37
	ENDIF

CRTCNT:		defb	24

CLMLST:		defb	14

TXTNAM:		defw	0
TXTCOL:		defw	0
TXTCGP:		defw	00800H
TXTATR:		defw	0
TXTPAT:		defw	0

T32NAM:		defw	01800H
T32COL:		defw	02000H
T32CGP:		defw	0
T32ATR:		defw	01B00H
T32PAT:		defw	03800H

GRPNAM:		defw	01800H
GRPCOL:		defw	02000H
GRPCGP:		defw	0
GRPATR:		defw	01B00H
GRPPAT:		defw	03800H

MLTNAM:		defw	00800H
MLTCOL:		defw	0
MLTCGP:		defw	0
MLTATR:		defw	01B00H
MLTPAT:		defw	03800H

CLIKSW:		defb	1
CSRY:		 defb	1
CSRX:		 defb	1
CNSDFG:		defb	0

RG0SAV:		defb	000H
RG1SAV:		defb	0E0H
RG2SAV:		defb	000H
RG3SAV:		defb	000H
RG4SAV:		defb	000H
RG5SAV:		defb	000H
RG6SAV:		defb	000H
RG7SAV:		defb	000H
STATFL:		defb	000H
TRGFLG:		defb	0FFH
FORCLR:		defb	15
BAKCLR:		defb	4

BDRCLR:
	IF BASVER = 0
		defb	7
	ELSE
		defb	4
	ENDIF

MAXUPD:		jp	0
MINUPD:		jp	0
ATRBYT:		defb	15
QUEUES:		defw	QUETAB
FRCNEW:		defb	0FFH
SCNCNT:		defb	1
REPCNT:		defb	50
PUTPNT:		defw	KEYBUF
GETPNT:		defw	KEYBUF
CS1200:		defb	053H,05CH,026H,02DH,00FH
CS2400:		defb	025H,02DH,00EH,016H,01FH
		defb	053H,05CH
		defb	026H,02DH
		defb	00FH
ASPCT1:		defw	00100H
ASPCT2:		defw	00100H
ENDPRG:		defb	':'

		DEPHASE
		PHASE	7FB7H

	IF MSX2 || NDEVFIX
; Bugfix check for zero length device names (e.g. ":xxx" filenames)
CHKZDN:
C7FB7:		LD	DE,PROCNM
		AND	A
		RET	NZ
		INC	B			; use length 1 (name ":" is used)
		RET
	ENDIF

	IF MSX1 && SLOTFIX
; Subroutine read from primary slot 0
C7FBE:		CALL	C7FCB			; store and change secondary slot register
		LD	E,(HL)			; read byte from slot
		JR	J7FC8			; restore secondary slot register

; Subroutine write to primary slot 0
C7FC4:		CALL	C7FCB			; store and change secondary slot register
		LD	(HL),E			; write byte to slot
J7FC8:		LD	A,B			; original secondary slot register
		JR	J7FD9			; update secondary slot register

; Subroutine store and change secondary slot register
C7FCB:		RRCA
		RRCA
		AND	00000011b		; secondary slot
		LD	D,A			; store secondary slot
		LD	A,(D_FFFF)
		CPL				; read secondary slot register
		LD	B,A			; store secondary slot register
		AND	11111100b		; clear slot of page 0
		OR	D			; update slot of page 0
		LD	D,A			; store secondary slot register
J7FD9:		LD	(D_FFFF),A		; update secondary slot register
		LD	A,E			; byte
		RET
	ENDIF
	
	IFDEF MSX2
		; Very similar to MSX1 SLOTFIX
C7FBE:		CALL	C7FD1
		LD	E,(HL)
		JR	J7FC8

C7FC4:		CALL	C7FD1
		LD	(HL),E
J7FC8:		IN	A,(0A8H)
		AND	3FH
		OUT	(0A8H),A
		LD	A,C			; saved secondary slotregister
		JR	J7FE6			; restore orginal page 0

; Subroutine switch page 0 primary slot 0
C7FD1:  	RRCA
		RRCA
		AND	03H
		LD	D,A
		IN	A,(0A8H)
		LD	B,A			; save primary slotregister
		AND	3FH
		OUT	(0A8H),A		; prim. slot 0 in page 3 (access the sec. slotreg)
		LD	A,(D_FFFF)
		CPL
		LD	C,A			; save secondary slotregister
		AND	0FCH
		OR	D
		LD	D,A
J7FE6:  	LD	(D_FFFF),A		; switch sec. slot page 0 (rombios gone!)
		LD	A,B
		OUT	(0A8H),A		; restore primary slotregister
		LD	A,E
		RET
		
; Subroutine CALSLT after Vertical Retrace
C7FF5:		IN	A,(99H)
		RLCA
		JR	NC,C7FF5		; wait for VR
		JP	CALSLT

; Subroutine entrypoint for disksystem
; Remark: used to call disksystem with basic rom active
C7FFD:		JP	BDOS
		
	ENDIF ; MSX2
		
		ALIGN	8000H			; fill unused space with zero's

		DEPHASE

