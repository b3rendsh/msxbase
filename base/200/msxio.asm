; ------------------------------------------------------------------------------
; msxio.asm
; BIOS MSX I/O function, MSX 2 version (version 2.0)
; ------------------------------------------------------------------------------

		ALIGN	0416H

CHKRAM:
J0416:		XOR	A
		OUT	(0FFH),A		; initalize memory mapper page 3 register (mapper page 0)
		INC	A
		OUT	(0FEH),A		; initalize memory mapper page 2 register (mapper page 1)
		INC	A
		OUT	(0FDH),A		; initalize memory mapper page 1 register (mapper page 2)
		INC	A
		OUT	(0FCH),A		; initalize memory mapper page 0 register (mapper page 3)
		LD	A,08H
		OUT	(0BBH),A		; initialize lightpen (enable interrupt)
		LD	A,82H
		OUT	(0ABH),A		; initialize PPI (active, group A mode 0, group A output, upper port C output, group B mode 0, group B input, lower port C output)
		XOR	A
		OUT	(0A8H),A		; select primary slot 0 on all pages
	IF INTHZ = 60
		LD	A,50H
		OUT	(0AAH),A		; CAPS off, motor off, keyboard row 0
	ELSE
		JP	PTCPAL			; patch routine to setup VDP for PAL
		NOP
	ENDIF

J0431:		LD	DE,0FFFFH		; initialize lowest RAM address found (page 2)
		XOR	A			; start with primary slot 0
		LD	C,A			; initialize expanded slot flags
J0436:		OUT	(0A8H),A		; select primary slot on page 2 and 3
		SLA	C			; shift expanded slot flags
		LD	B,0			; assume not expanded slot
		LD	HL,D_FFFF
		LD	(HL),0F0H
		LD	A,(HL)
		SUB	0FH			; is slot expanded (writen value readback inverted) ?
		JR	NZ,J0451		; nope,
		LD	(HL),A
		LD	A,(HL)
		INC	A			; is slot expanded (writen value readback inverted) ?
		JR	NZ,J0451		; nope,
		INC	B
		SET	0,C			; flag slot expanded
J044E:		LD	(D_FFFF),A
J0451:		LD	HL,0BFFFH-255		; page 2
J0454:		LD	A,(HL)
		CPL
		LD	(HL),A
		CP	(HL)			; RAM found (write inverted correctly read back) ?
		CPL
		LD	(HL),A			; restore orginal value
		JR	NZ,J0463		; no RAM
		INC	L
		JR	NZ,J0454
		DEC	H
		JP	M,J0454
J0463:		LD	L,0
		INC	H				; round up 256 bytes boundary
		LD	A,L
		SUB	E
		LD	A,H
		SBC	A,D			; page 2 with more RAM ?
		JR	NC,J0476		; nope,
		EX	DE,HL			; new lowest RAM address found
		LD	A,(D_FFFF)
		CPL
		LD	L,A
		IN	A,(0A8H)
		LD	H,A
		LD	SP,HL			; save primary and secondary slotregister of lowest RAM address found (page 2)
J0476:		LD	A,B
		AND	A				; slot expanded ?
		JR	Z,J0484			; nope, next primary slot
		LD	A,(D_FFFF)
		CPL
		ADD	A,10H			; next secondary slot page 2
		CP	40H			; all secondary slots done ?
		JR	C,J044E			; nope, next
J0484:		IN	A,(0A8H)
		ADD	A,50H			; next primary slot page 2 and 3
		JR	NC,J0436		; all primary slots done ? nope, next
		LD	HL,0
		ADD	HL,SP
		LD	A,H
		OUT	(0A8H),A
		LD	A,L
		LD	(D_FFFF),A		; select slot with lowest RAM address found (page 2)
		LD	A,C
		RLCA
		RLCA
		RLCA
		RLCA
		LD	C,A			; expanded slot flags in b7-b4
		LD	DE,0FFFFH		; initialize lowest RAM address found (page 3)
		IN	A,(0A8H)
		AND	3FH			; primary slot 0 in page 3
J04A2:		OUT	(0A8H),A		; select new primary slot in page 3
		LD	B,0
		RLC	C			; slot expanded ?
		JR	NC,J04B4		; nope,
		INC	B			; flag slot expanded
		LD	A,(D_FFFF)
		CPL
		AND	3FH			; secondary slot 0 in page 3
J04B1:		LD	(D_FFFF),A		; select new secondary slot in page 3
J04B4:		LD	HL,0FEFFH-255		; page 3 (leave upper 256 bytes out for secondary slot register)
J04B7:		LD	A,(HL)
		CPL
		LD	(HL),A
		CP	(HL)			; RAM found (write inverted correctly read back) ?
		CPL
		LD	(HL),A			; restore orginal value
		JR	NZ,J04C8		; no RAM
		INC	L
		JR	NZ,J04B7
		DEC	H
		LD	A,H
		CP	0C0H
		JR	NC,J04B7
J04C8:		LD	L,0
		INC	H			; round up 256 bytes boundary
		LD	A,L
		SUB	E
		LD	A,H
		SBC	A,D			; page 3 with more RAM ?
		JR	NC,J04DB		; nope,
		EX	DE,HL			; new lowest RAM address found
		LD	A,(D_FFFF)
		CPL
		LD	L,A
		IN	A,(0A8H)
		LD	H,A
		LD	SP,HL			; save primary and secondary slotregister of lowest RAM address found (page 3)
J04DB:		LD	A,B
		AND	A			; slot expanded ?
		JR	Z,J04E7			; nope,
		LD	A,(D_FFFF)
		CPL
		ADD	A,40H			; next secondary slot page 3
		JR	NC,J04B1		; all secondary slots done ? nope, next
J04E7:		IN	A,(0A8H)
		ADD	A,40H			; next primary slot page 3
		JR	NC,J04A2		; all primary slots done ? nope, next
		JP	J7B61			; continue with hardware intialization, part 2

; Subroutine ISCNTC
;	Inputs		________________________
;	Outputs		________________________

ISCNTC:
C04F0:	LD	A,(BASROM)
		AND	A
		RET	NZ			 ; execute extension ROM with BASIC text, quit
		PUSH	HL
		LD	HL,INTFLG
		DI
		LD	A,(HL)
		EI
		LD	(HL),0			; reset STOP status
		POP	HL
		AND	A
		RET	Z			; STOP or CTRL/STOP not pressed, quit
		CP	03H
		JR	Z,J0521			; handle CTRL/STOP
		PUSH	HL
		PUSH	DE
		PUSH	BC
		CALL	C0A37			; display cursor when disabled
		LD	HL,INTFLG
J050E:		DI
		LD	A,(HL)
		EI
		LD	(HL),0			; reset STOP status
		AND	A
		JR	Z,J050E			; STOP or CTRL/STOP not pressed yet, keep waiting
		PUSH	AF
		CALL	C0A84			; remove cursor when disabled
		POP	AF
		POP	BC
		POP	DE
		POP	HL
		CP	03H
		RET	NZ
J0521:		PUSH	HL
		CALL	C055D			; KILBUF
		CALL	C0549			; excute ON STOP GOSUB valid ?
		JR	NC,J0534		; nope, stop basic program
		LD	HL,TRPTBL+10*3
		DI
		CALL	REQTRP			; raise CTRL-STOP trap
		EI
		POP	HL
		RET

J0534:		CALL	C089E			; TOTEXT (force textmode)
		LD	A,(EXPTBL+0)
		LD	H,40H
		CALL	ENASLT			; ENASLT (basicrom on page 1)
		POP	HL
		XOR	A
		LD	SP,(SAVSTK)		; restore stack
		PUSH	BC
		JP	CSTOP			; execute STOP statement

; Subroutine check if ON STOP valid
CKSTTP:
C0549:		LD	A,(TRPTBL+10*3+0)
		RRCA
		RET	NC			 ; ON STOP not ON, quit
		LD	HL,(TRPTBL+10*3+1)
		LD	A,H
		OR	L
		RET	Z				; has not STOP subroutine, quit
		LD	HL,(CURLIN)
		INC	HL
		LD	A,H
		OR	L
		RET	Z				; basic interpeter in direct mode, quit
		SCF					; flag execute ON STOP GOSUB valid
		RET

; Subroutine KILBUF (clears keyboardbuffer)
KILBUF:
C055D:		LD	HL,(PUTPNT)
		LD	(GETPNT),HL
		RET

; Subroutine BREAKX
BREAKX:
C0564:		IN	A,(0AAH)
		AND	0F0H
		OR	07H
		OUT	(0AAH),A
		IN	A,(0A9H)
		AND	10H
		RET	NZ
		IN	A,(0AAH)
		DEC	A
		OUT	(0AAH),A
		IN	A,(0A9H)
		AND	02H
		RET	NZ
		PUSH	HL
		LD	HL,(PUTPNT)
		LD	(GETPNT),HL		; clear keyboardbuffer
		POP	HL
		LD	A,(OLDKEY+7)
		AND	0EFH
		LD	(OLDKEY+7),A
	IF INTHZ = 50
		LD	A,14H
	ELSE
		LD	A,20H
	ENDIF
		LD	(REPCNT),A		; reset REPCNT
		SCF
		RET

; Subroutine INITIO
INITIO:
J0592:		LD	A,7
		LD	E,80H
		CALL	C1102			; WRTPSG
		LD	A,15
		LD	E,0CFH
		CALL	C1102			; WRTPSG
		LD	A,11
		LD	E,A
		CALL	C1102			; WRTPSG
		CALL	C110C
		AND	40H
		LD	(KANAMD),A		; save kanji layout
		LD	A,0FFH
		OUT	(90H),A

; Subroutine GICINI
GICINI:
C05B2:		PUSH	HL
		PUSH	DE
		PUSH	BC
		PUSH	AF
		LD	HL,MUSICF
		LD	B,71H
		XOR	A
J05BC:		LD	(HL),A
		INC	HL
		DJNZ	J05BC
		LD	DE,VOICAQ
		LD	B,7FH
		LD	HL,0080H
J05C8:		PUSH	HL
		PUSH	DE
		PUSH	BC
		PUSH	AF
		CALL	INITQ
		POP	AF
		ADD	A,8
		LD	E,0
		CALL	C1102			; WRTPSG
		SUB	08H
		PUSH	AF
		LD	L,0FH
		CALL	C0C11
		EX	DE,HL
		LD	HL,I05FD
		LD	BC,6
		LDIR
		POP	AF
		POP	BC
		POP	HL
		POP	DE
		ADD	HL,DE
		EX	DE,HL
		INC	A
		CP	03H
		JR	C,J05C8
		LD	A,7
		LD	E,0B8H
		CALL	C1102			; WRTPSG
		JP	POPALL

I05FD:		DEFB	4
		DEFB	4
		DEFB	120
		DEFB	8+128
		DEFW	255

; Subroutine ENASCR
ENASCR:
J0603:		LD	A,(RG1SAV)
		OR	40H
		JR	J060F

; Subroutine DISSCR
DISSCR:
J060A:		LD	A,(RG1SAV)
		AND	0BFH
J060F:		LD	B,A
		LD	C,1			; vdp register 1

; Subroutine WRTVDP
WRTVDP:
J0612:		LD	A,C
		AND	A
		JR	NZ,J061D
		LD	A,(RG0SAV)
		XOR	B
		RRCA
		JR	NC,J0629		; external vdp input bit unchanged, old way
						; otherwise A>=080H and due next code handled by the subrom
J061D:		CP	08H
		JR	C,J0629			; vdp registers 1-7 handled the old way
		PUSH	IX
		LD	IX,S_WRTVDP
		JR	J0644			; others are handled by the subrom routine

J0629:		LD	A,B
		DI
		OUT	(99H),A
		LD	A,C
		OR	80H
		EI
		OUT	(99H),A
		PUSH	HL
		LD	A,B
		LD	B,0
		LD	HL,RG0SAV
		ADD	HL,BC
		LD	(HL),A
		POP	HL
		RET

; Subroutine INITXT
INITXT:
J063E:		PUSH	IX
		LD	IX,S_INITXT
J0644:		JP	SUBROM			; INITXT handled by SUBROM

; Subroutine INIT32
INIT32:
J0647:		PUSH	IX
		LD	IX,S_INIT32
		JR	J0644			; INIT32 handled by SUBROM

; Subroutine INIGRP
INIGRP:
J064F:		PUSH	IX
		LD	IX,S_INIGRP
		JR	J0644			; INIGRP handled by SUBROM

; Subroutine INIMLT
INIMLT:
J0657:		PUSH	IX
		LD	IX,S_INIMLT
		JR	J0644			; INIMLT handled by SUBROM

; Subroutine SETTXT
SETTXT:
J065F:		PUSH	IX
		LD	IX,S_SETTXT
		JR	J0644			; SETTXT handled by SUBROM

; Subroutine SETT32
SETT32:
J0667:		PUSH	IX
		LD	IX,S_SETT32
		JR	J0644			; SETT32 handled by SUBROM

; Subroutine SETGRP
SETGRP:
J066F:		PUSH	IX
		LD	IX,S_SETGRP
		JR	J0644			; SETGRP handled by SUBROM

; Subroutine SETMLT
SETMLT:
J0677:		PUSH	IX
		LD	IX,S_SETMLT
		JR	J0644			; SETMLT handled by SUBROM

; Subroutine CLRSPR
CLRSPR:
J067F:		PUSH	IX
		LD	IX,S_CLRSPR
		JR	J0644			; CLRSPR handled by SUBROM

; Subroutine setup for INIR/OTIR
; Input:  HL = vram adres
;         DE = ram adres
;         BC = size
; Output: DE = vram adres
;         HL = ram adres
;         B  = initial repeatcount
;         A  = loopcount
C0687:		EX	DE,HL
		LD	A,C
		OR	A
		LD	A,B
		LD	B,C
		RET	Z				; low byte size zero, quit
		INC	A				; extra loop for mod 256
		RET

; Subroutine convert base vram adres to screenpage vram adres
; Input:  HL  = base vram adres
;         D   = active screenpage
; Output: DHL = vram adres for screenpage
C068F:		EX	DE,HL
		LD	A,(SCRMOD)
		SUB	5
		JR	C,J06A6			; MSX1 style screenmode, screenpage ignored
		AND	02H
		LD	A,H
		JR	NZ,J069D		; screenmode 7 and 8 (and MSX2+ screenmodes), screensize 64Kb
		RRA					; extra shift for screensize 32Kb
J069D:		RRA
		LD	H,A
		LD	A,0
		LD	L,A
		ADC	A,A
		ADD	HL,DE
		LD	D,A
		RET

J06A6:		LD	D,0
		RET

; Subroutine CHKNEW (check if new style screenmode)
CHKNEW:
C06A9:		PUSH	BC
		LD	B,A
		LD	A,(SCRMOD)
		CP	5
		LD	A,B
		POP	BC
		RET

; Subroutine NSTWRT
NSTWRT:
C06B3:		PUSH	BC
		PUSH	DE
		PUSH	HL
		LD	A,(ACPAGE)
		AND	A
		LD	D,A			; A16=0 / active screenpage
		CALL	NZ,C068F		; active screenpage non zero, convert baseadres to screenpage adres
		LD	A,H
		AND	3FH
		OR	40H
		JR	J06D3

; Subroutine NSETRD
; Input:  HL=vram adres (range:0-0FFFFH)
NSETRD:
C06C5:		PUSH	BC
		PUSH	DE
		PUSH	HL
		LD	A,(ACPAGE)
		AND	A
		LD	D,A			; A16=0 / active screenpage
		CALL	NZ,C068F		; active screenpage non zero, convert baseadres to screenpage adres
		LD	A,H
		AND	3FH
J06D3:		PUSH	AF
		LD	A,H
		AND	0C0H
		OR	D
		RLCA
		RLCA
		DI
		OUT	(99H),A
		LD	A,128+14
		OUT	(99H),A			; vdp register 14 (setup A16-A14 of vram adres)
		LD	A,L
		OUT	(99H),A
		POP	AF
		EI
		OUT	(99H),A
		POP	HL
		POP	DE
		POP	BC
		RET

; Subroutine CALPAT
CALPAT:
J06EC:		LD	L,A
		LD	H,0
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		CALL	C070C			; GSPSIZ
		CP	08H
		JR	Z,J06FB
		ADD	HL,HL
		ADD	HL,HL
J06FB:		EX	DE,HL
		LD	HL,(PATBAS)
		ADD	HL,DE
		RET

; Subroutine CALATR
CALATR:
J0701:		LD	L,A
		LD	H,0
		ADD	HL,HL
		ADD	HL,HL
		EX	DE,HL
		LD	HL,(ATRBAS)
		ADD	HL,DE
		RET

; Subroutine GSPSIZ
GSPSIZ:
C070C:		LD	A,(RG1SAV)
		RRCA
		RRCA
		LD	A,8
		RET	NC
		LD	A,32
		RET

; Subroutine Initialize character pattern table
; Remark: Unused code, same code is in subrom
;         code was used in INITXT and INIT32, but these too are moved in subrom

Q0717:		SYSHOOK	H_INIP
		LD	HL,(CGPBAS)
		CALL	C07F4			; SETWRT
		LD	A,(CGPNT+0)
		LD	HL,(CGPNT+1)
		LD	BC,0800H
		PUSH	AF
J072A:		POP	AF
		PUSH	AF
		PUSH	BC
		DI
		CALL	RDSLT			; RDSLT
		EI
		POP	BC
		OUT	(98H),A
		INC	HL
		DEC	BC
		LD	A,C
		OR	B
		JR	NZ,J072A
		POP	AF
		RET

; Subroutine LDIRMV (vram to memory)
; Input: HL = vram adres
;        DE = ram adres
;        BC = size
LDIRMV:
C073D:		LD	A,(SCRMOD)
		CP	4
		JR	NC,J075A		; MSX2 screenmode, use faster methode
		LD	A,(MODE)
		AND	08H
		JR	NZ,J075A		; use fast transfer for MSX1 screenmodes, use fast methode
		CALL	C0808			; SETRD
		EX	(SP),HL
		EX	(SP),HL
J0750:		IN	A,(98H)
		LD	(DE),A
		INC	DE
		DEC	BC
		LD	A,C
		OR	B
		JR	NZ,J0750
		RET

J075A:		CALL	C06C5			; NSETRD
		CALL	C0687			; setup for INIR
		LD	C,98H
J0762:		INIR
		DEC	A
		JR	NZ,J0762
		EX	DE,HL
		RET

; Subroutine NWRVRM
NWRVRM:
J0769:		PUSH	AF
		CALL	C06B3			; NSTWRT
		EX	(SP),HL
		EX	(SP),HL
		POP	AF
		OUT	(98H),A
		RET

		DEFS	0777H-$,0

; This routine has same entrypoint as MSX1
C0777:		PUSH	IX
		LD	IX,S_CLS
		JP	SUBROM			; SUBROM

; Subroutine LDIRVM (memory to vram)
; Input:  HL = RAM adres
;         DE = VRAM adres
;         BC = size
LDIRVM:
C0780:		EX	DE,HL
		LD	A,(SCRMOD)
		CP	04H
		JR	NC,J079C		; MSX2 screenmode, use fast methode
		LD	A,(MODE)
		AND	08H
		JR	NZ,J079C		; use fast transfer for MSX1 screenmodes, use fast methode
		CALL	C07F4			; SETWRT
J0792:		LD	A,(DE)
		OUT	(98H),A
		INC	DE
		DEC	BC
		LD	A,C
		OR	B
		JR	NZ,J0792
		RET

J079C:		CALL	C06B3			; NSTWRT
		CALL	C0687			; setup for OTIR
		LD	C,98H
J07A4:		OTIR
		DEC	A
		JR	NZ,J07A4
		EX	DE,HL
		RET

; Subroutine copy charpattern to PATWRK
; Input:  A = charcode
GETPAT:
C07AB:		LD	H,0
		LD	L,A
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		EX	DE,HL
		LD	HL,(CGPNT+1)
		ADD	HL,DE
		LD	DE,PATWRK
		LD	B,8
J07BB:		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	A,(CGPNT+0)
		CALL	RDSLT			; RDSLT
		EI
		POP	BC
		POP	DE
		POP	HL
		LD	(DE),A
		INC	DE
		INC	HL
		DJNZ	J07BB
		RET

; Subroutine do cls
C07CE:		PUSH	IX
		LD	IX,S_CLRTXT
		JP	SUBROM			; SUBROM

; Subroutine WRTVRM
WRTVRM:
C07D7:		PUSH	AF
		CALL	C07F4			; SETWRT
		EX	(SP),HL
		EX	(SP),HL
		POP	AF
		OUT	(98H),A
		RET

; Subroutine RDVRM
RDVRM:
C07E1:		CALL	C0808			; SETRD
		EX	(SP),HL
		EX	(SP),HL
		IN	A,(98H)
		RET

; Subroutine NRDVRM
NRDVRM:
J07E9:		CALL	C06C5			; NSETRD
		EX	(SP),HL
		EX	(SP),HL
		IN	A,(98H)
		RET

; Subroutine set VDP for VRAM write at screenlocation
; Input:  H=x pos, L=y pos
C07F1:		CALL	C0B98			; convert screenlocation to VRAM adres

; Subroutine SETWRT
; Input:  HL=vram adres
SETWRT:
C07F4:		XOR	A
		DI
		OUT	(99H),A
		LD	A,128+14
		OUT	(99H),A			; vdp register 14 = 0 (adres the first 16 Kb vram)
		LD	A,L
		OUT	(99H),A
		LD	A,H
		AND	3FH
		OR	40H
		OUT	(99H),A			; setup vdp for writing
		EI
		RET

; Subroutine SETRD
SETRD:
C0808:		XOR	A
		DI
		OUT	(99H),A
		LD	A,128+14
		OUT	(99H),A			; vdp register 14 = 0 (adres the first 16 Kb vram)
		LD	A,L
		OUT	(99H),A
		LD	A,H
		AND	3FH
		OUT	(99H),A
		EI
		RET

; Subroutine CHGCLR
CHGCLR:
J081A:		LD	A,(SCRMOD)
		DEC	A
		JP	M,J0881
		PUSH	AF
		CALL	C088E
		POP	AF
		RET	NZ
		LD	A,(FORCLR)
		ADD	A,A
		ADD	A,A
		ADD	A,A
		ADD	A,A
		LD	HL,BAKCLR
		OR	(HL)
		LD	HL,(T32COL)
		LD	BC,32

; Subroutine BIGFIL
BIGFIL:
C0838:		PUSH	AF
		JR	J0841

; Subroutine FILVRM
; Input:  HL = vram adres
;         BC = size
;         A  = data
FILVRM:
J083B:		PUSH	AF
		CALL	C06A9			; CHKNEW (check if new style screenmode)
		JR	C,J0853			; nope,
J0841:		CALL	C06B3			; NSTWRT
		LD	A,C
		OR	A
		JR	Z,J0849
		INC	B
J0849:		POP	AF
J084A:		OUT	(98H),A
		DEC	C
		JP	NZ,J084A
		DJNZ	J084A
		RET

J0853:		POP	AF
		PUSH	HL
		PUSH	DE
		LD	E,A
		LD	A,H
		AND	3FH
		LD	H,A			; vram adres back in 16 Kb range
		PUSH	HL
		ADD	HL,BC
		DEC	HL
		LD	A,H
		CP	40H			; do we cross the 16 Kb border with this fill ?
		POP	HL
		JR	C,J087A			; nope,
		PUSH	BC
		XOR	A
		SUB	L
		LD	C,A
		LD	A,40H
		SBC	A,H
		LD	B,A			; size = 4000H-vram adres
		LD	A,E			; data
		CALL	C0838			; BIGFIL
		POP	BC
		ADD	HL,BC
		LD	C,L
		LD	A,H
		SUB	40H
		LD	B,A			; size = size-
		LD	HL,0			; start at vram adres 0
J087A:		LD	A,E			; data
		CALL	C0838			; BIGFIL
		POP	DE
		POP	HL
		RET


J0881:		LD	A,(FORCLR)
		ADD	A,A
		ADD	A,A
		ADD	A,A
		ADD	A,A
		LD	HL,BAKCLR
		OR	(HL)
		JR	J0891

C088E:		LD	A,(BDRCLR)
J0891:		LD	B,A
		LD	C,7
		JP	J0612			; WRTVDP

; Subroutine CLS
CLS:
J0897:		RET	NZ
		PUSH	HL
		CALL	C0777
		POP	HL
		RET

; Subroutine TOTEXT
TOTEXT:
C089E:		CALL	C0B4E			; check if screen is in textmode
		RET	C				; yep, do nothing
		LD	A,(OLDSCR)
		SYSHOOK	H_TOTE
		PUSH	IX
		LD	IX,S_CHGMDP
		JP	SUBROM			; SUBROM

; Subroutine CHGMOD
CHGMOD:
J08B1:		PUSH	IX
		LD	IX,S_CHGMOD
		JP	SUBROM			; CHGMOD is in SUBROM

; Subroutine LPTOUT
LPTOUT:
C08BA:		SYSHOOK	H_LPTO
		PUSH	AF
J08BE:		CALL	C0564			; BREAKX
		JR	C,J08D5
		CALL	C08E1			; LPTSTT
		JR	Z,J08BE
		POP	AF

; Subroutine write byte to printerport
C08C9:		PUSH	AF
		OUT	(91H),A
		XOR	A
		OUT	(90H),A			; strobe
		DEC	A
		OUT	(90H),A
		POP	AF
		AND	A
		RET

J08D5:		XOR	A
		LD	(LPTPOS),A
		LD	A,0DH
		CALL	C08C9			; write CR to printerport
		POP	AF
		SCF
		RET

; Subroutine LPTSTT
LPTSTT:
C08E1:		SYSHOOK	H_LPTS
		IN	A,(90H)
		RRCA
		RRCA
		CCF
		SBC	A,A
		RET

; Subroutine POSIT
POSIT:
C08EB:		LD	A,1BH
		RST	R_OUTDO
		LD	A,'Y'
		RST	R_OUTDO
		LD	A,L
		ADD	A,1FH
		RST	R_OUTDO
		LD	A,H
		ADD	A,1FH
		RST	R_OUTDO
		RET

; Subroutine CNVCHR
CNVCHR:
C08FA:		PUSH	HL
		PUSH	AF
		LD	HL,GRPHED
		XOR	A
		CP	(HL)
		LD	(HL),A
		JR	Z,J0911
		POP	AF
		SUB	40H
		CP	20H
		JR	C,J090F
		ADD	A,40H
J090D:		CP	A
		SCF
J090F:		POP	HL
		RET

J0911:		POP	AF
		CP	1
		JR	NZ,J090D
		LD	(HL),A
		POP	HL
		RET

; Subroutine CHPUT
CHPUT:
J0919:		PUSH	HL
		PUSH	DE
		PUSH	BC
		PUSH	AF
		SYSHOOK	H_CHPU
		CALL	C0B4E			; check if screen is in textmode
		JR	NC,J0937		; nope, quit
		CALL	C0A8B			; remove cursor when enabled
		POP	AF
		PUSH	AF
		CALL	C093C
		CALL	C0A3E			; display cursor when enabled
		LD	A,(CSRX)
		DEC	A
		LD	(TTYPOS),A
POPALL:
J0937:		POP	AF
J0938:		POP	BC
		POP	DE
		POP	HL
		RET

C093C:		CALL	C08FA			; CNVCHR
		RET	NC
		LD	C,A
		JR	NZ,J0950
		LD	HL,ESCCNT
		LD	A,(HL)
		AND	A
		JP	NZ,J09EC
		LD	A,C
		CP	20H
		JR	C,J0971
J0950:		LD	HL,(CSRY)
		CP	7FH
		JP	Z,J0AFA
		CALL	C0B8D			; write char at screenlocation
		CALL	C0AA1			; move cursor right if possible
		RET	NZ			 ; was possible, quit
		XOR	A
		CALL	C0BDB			; mark line as succession
		LD	H,1

; Subroutine screencode LF
C0965:		CALL	C0ABE			; cursor down if possible
		RET	NZ			 ; possible, quit
		CALL	C0AC6			; update cursorpos
		LD	L,1
		JP	C0AE5			; delete line 1

J0971:		LD	HL,D098C-2
		LD	C,12

INDJMP:
C0976:		INC	HL
		INC	HL
		AND	A				; clear Cx
		DEC	C				; end of table ?
		RET	M				; yep, quit
		CP	(HL)
		INC	HL
		JR	NZ,C0976
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		LD	HL,(CSRY)
		CALL	C098A
		XOR	A				; clear Cx, set Zx
		RET

; Subroutine call subroutine
; Input:  BC = adres of subroutine
C098A:		PUSH	BC
		RET

D098C:		DEFB	7
		DEFW	C1113			; BEEP
		DEFB	8
		DEFW	C0AA9			; BS, cursor left with warp
		DEFB	9
		DEFW	C0ACE
		DEFB	10
		DEFW	C0965
		DEFB	11
		DEFW	C0ADC
		DEFB	12
		DEFW	C07CE			; do CLS
		DEFB	13
		DEFW	C0ADE
		DEFB	27
		DEFW	C09E6
		DEFB	28
		DEFW	C0AB8			; RIGHT, cursor right with linewarp
		DEFB	29
		DEFW	C0AA9			; LEFT, cursor left with linewarp
		DEFB	30
		DEFW	C0AB4			; UP, cursor up if possible
		DEFB	31
		DEFW	C0ABE			; DOWN, cursor down if possible

; ESC table
D09B0:		DEFB	"j"
		DEFW	C07CE			; ESC j, clear screen
		DEFB	"E"
		DEFW	C07CE			; ESC E, clear screen
		DEFB	"K"
		DEFW	EOL			; ESC K, clear to end of line
		DEFB	"J"
		DEFW	C0B19			; ESC J, clear to end of screen
		DEFB	"l"
		DEFW	C0B03			; ESC l, clear line
		DEFB	"L"
		DEFW	C0AEE			; ESC L, insert line
		DEFB	"M"
		DEFW	C0AE2			; ESC M, delete line
		DEFB	"Y"
		DEFW	C09E3			; ESC Y, locate cursor
		DEFB	"A"
		DEFW	C0AB4			; ESC A, cursor up if possible
		DEFB	"B"
		DEFW	C0ABE			; ESC B, cursor down if possible
		DEFB	"C"
		DEFW	C0AA1			; ESC C, cursor right if possible
		DEFB	"D"
		DEFW	C0AB2			; ESC D, cursor left if possible
		DEFB	"H"
		DEFW	C0ADC			; ESC H, cursor home
		DEFB	"x"
		DEFW	C09DD			; ESC x
		DEFB	"y"
		DEFW	C09E0			; ESC y

; ESC x
C09DD:		LD	A,1
		DEFB	001H			; LD BC,xxxx (skip next instruction)

; ESC y
C09E0:		LD	A,2
		DEFB	001H			; LD BC,xxxx (skip next instruction)

; ESC Y
C09E3:		LD	A,4
		DEFB	001H			; LD BC,xxxx (skip next instruction)

; ESC handler
C09E6:		LD	A,0FFH
		LD	(ESCCNT),A		; mark start of ESC sequence
		RET

J09EC:		JP	P,J09FA			; esc sequence with parameter, handle
		LD	(HL),0			; esc sequence ends
		LD	A,C
		LD	HL,D09B0-2
		LD	C,15
		JP	INDJMP

J09FA:		DEC	A
		JR	Z,J0A1B			; esc x
		DEC	A
		JR	Z,J0A25			; esc y
		DEC	A
		LD	(HL),A
		LD	A,(LINLEN)		; max x coord
		LD	DE,CSRX
		JR	Z,J0A10			; busy with the x coord
		LD	(HL),3
		CALL	C0BE2			; get max linenumber 
		DEC	DE			 ; CSRY
J0A10:		LD	B,A
		LD	A,C
		SUB	20H
		CP	B				; coord in range ?
		INC	A				; 1 based
		LD	(DE),A			; save cursor coord
		RET	C				; yep, quit
		LD	A,B
		LD	(DE),A			; nope, use the max
		RET

; ESC x handler
J0A1B:		LD	(HL),A			; esc sequence ends
		LD	A,C
		SUB	'4'
		JR	Z,J0A2C			; ESC x4, block cursor
		DEC	A
		JR	Z,J0A33			; ESC x5, cursor disabled
		RET					; other ESC x, ignore

; ESC y handler
J0A25:		LD	(HL),A			; esc sequence ends
		LD	A,C
		SUB	'4'
		JR	NZ,J0A30
		INC	A				; ESC y4, "insert" cursor
J0A2C:		LD	(CSTYLE),A
		RET

J0A30:		DEC	A
		RET	NZ			; other ESC y, ignore
		INC	A			; ESC y5, enable cursor
J0A33:		LD	(CSRSW),A
		RET

; Subroutine display cursor when disabled
C0A37:		LD	A,(CSRSW)
		AND	A
		RET	NZ			; cursor enabled, quit
		JR	J0A43			; display cursor

; Subroutine display cursor when enabled
CKDPCS:
C0A3E:		LD	A,(CSRSW)
		AND	A
		RET	Z			; cursor disabled, quit
J0A43:		SYSHOOK	H_DSPC
		CALL	C0B4E			; check if screen is in textmode
		RET	NC			; nope, quit
		LD	HL,(CSRY)
		PUSH	HL
		CALL	C0B83			; read char from screenlocation
		LD	(CURSAV),A		; save it
		LD	L,A
		LD	H,0
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		EX	DE,HL
		LD	HL,(CGPBAS)
		PUSH	HL
		ADD	HL,DE
		CALL	C0B54			; copy charpattern to LINWRK
		LD	HL,LINWRK+7
		LD	B,8 
		LD	A,(CSTYLE)
		AND	A
		JR	Z,J0A70			; "block" cursor
		LD	B,3			; "insert" cursor
J0A70:		LD	A,(HL)
		CPL
		LD	(HL),A
		DEC	HL
		DJNZ	J0A70
		POP	HL
		LD	BC,255*8
		ADD	HL,BC			; the cursor pattern
		CALL	C0B6B			; copy LINWRK to charpattern
		POP	HL
		LD	C,0FFH			; the cursor
		JP	C0B8D			; write char at screenlocation

; Subroutine remove cursor when disabled
C0A84:		LD	A,(CSRSW)
		AND	A
		RET	NZ			; cursor enabled, quit
		JR	J0A90			; remove cursor from screen

; Subroutine remove cursor when enabled
CKERCS:
C0A8B:		LD	A,(CSRSW)
		AND	A
		RET	Z			; cursor disabled, quit
J0A90:		SYSHOOK	H_ERAC
		CALL	C0B4E			; check if screen is in textmode
		RET	NC			; nope, quit
		LD	HL,(CSRY)		; cursorlocation
		LD	A,(CURSAV)
		LD	C,A			; orginal char at cursorlocation
		JP	C0B8D			; write char at screenlocation

; Subroutine move cursor right if possible
C0AA1:		LD	A,(LINLEN)
		CP	H
		RET	Z			; already at end of line, quit
		INC	H			; cursor right
		JR	C0AC6			; update cursorpos

; Subroutine move cursor left with linewarp
; BS (note: duplicate label)
C0AA9:		CALL	C0AB2			; cursor left if possible
		RET	NZ			; was possible, quit
		LD	A,(LINLEN)
		LD	H,A			; cursor at end of line
		DEFB	011H			; LD DE,xxxx (skip next 2 instructions), so cursor line up

; Subroutine move cursor left if possible
C0AB2:		DEC	H
		DEFB	03EH			; LD A,xx (skip next instruction)

; Subroutine move cursor up if possible
C0AB4:		DEC	L
		RET	Z			; already at top of screen, quit
		JR	C0AC6			; update cursorpos

; Subroutine move cursor right with linewarp
ADVCUR:
C0AB8:		CALL	C0AA1			; cursor right if possible
		RET	NZ			; was possible, quit
		LD	H,1			; start of line and linedown

; Subroutine cursor down if possible
C0ABE:		CALL	C0BE2			; get max linenumber
		CP	L
		RET	Z			; already at the bottom of screen, quit
		JR	C,J0ACA
		INC	L			; cursor down

C0AC6:		LD	(CSRY),HL
		RET

J0ACA:		DEC	L
		XOR	A
		JR	C0AC6			; update cursorpos

C0ACE:		LD	A,20H	; " "
		CALL	C093C
		LD	A,(CSRX)
		DEC	A
		AND	07H
		JR	NZ,C0ACE
		RET

C0ADC:		LD	L,1

; Subroutine screencode CR
C0ADE:		LD	H,1
		JR	C0AC6			; update cursorpos

C0AE2:		CALL	C0ADE

DELLN0:
C0AE5:		PUSH	IX
		LD	IX,S_DELLNO
		JP	SUBROM			; SUBROM

C0AEE:		CALL	C0ADE

INSLN0:
C0AF1:		PUSH	IX
		LD	IX,S_INSLNO
		JP	SUBROM			; SUBROM

J0AFA:		CALL	C0AA9			; cursor left with warp
		RET	Z
		LD	C,' '
		JP	C0B8D			; write char at screenlocation

C0B03:	LD	H,1

; Subroutine EOL
EOL:
C0B05:		CALL	C0BD9			; unmark line as succession
		PUSH	HL
		CALL	C07F1			; set VDP for VRAM write at screenlocation
		POP	HL
J0B0D:		LD	A,' '
		OUT	(98H),A
		INC	H
		LD	A,(LINLEN)
		CP	H
		JR	NC,J0B0D		; write blanks until the end of line
		RET

C0B19:		PUSH	HL
		CALL	EOL			; EOL
		POP	HL
		CALL	C0BE2			; get max linenumber
		CP	L
		RET	C
		RET	Z
		LD	H,01H	; 1 
		INC	L
		JR	C0B19

; Subroutine ERAFNK
ERAFNK:
J0B29:		SYSHOOK	H_ERAF
		XOR	A
		CALL	C0B4B
		RET	NC
		PUSH	HL
		LD	HL,(CRTCNT)
		CALL	C0B03
		POP	HL
		RET

; Subroutine FNKSB
FNKSB:
J0B3A:		LD	A,(CNSDFG)
		AND	A
		RET	Z

; Subroutine DSPFNK
DSPFNK:
C0B3F:		SYSHOOK	H_DSPF
		PUSH	IX
		LD	IX,S_DSPFNK
		JP	SUBROM			; SUBROM

C0B4B:		LD	(CNSDFG),A

; Subroutine check if in textmode
; Output: Cx set if in textmode
C0B4E:		LD	A,(SCRMOD)
		CP	02H
		RET

; Subroutine copy 8 bytes of vram to LINWRK
; Input:  HL = vram adres
C0B54:		PUSH	HL
		LD	C,8
		JR	J0B61

; Subroutine copy screenline to LINWRK
; Input:  L = linenumber
; Remark: Unused code, code is in subrom
;         This code will not even work if screen is in 80 col mode because LINWRK is only 40 bytes!
Q0B59:		PUSH	HL
		CALL	C0B96			; convert linenumber to VRAM adres
		LD	A,(LINLEN)		; screenwidth
		LD	C,A

J0B61:		LD	B,0
		LD	DE,LINWRK
		CALL	C073D			; LDIRMV (copy to LINWRK)
		POP	HL
		RET

; Subroutine copy 8 bytes from LINWRK to vram
; Input:  HL = vram adres
C0B6B:		PUSH	HL
		LD	C,8
		JR	J0B78

; Subroutine copy LINWRK to screenline
; Input:  L = linenumber
; Remark: Unused code, code is in subrom
;         This code will not even work if screen is in 80 col mode because LINWRK is only 40 bytes!
Q0B70:		PUSH	HL
		CALL	C0B96			; convert linenumber to VRAM adres
		LD	A,(LINLEN)
		LD	C,A

J0B78:		LD	B,0
		EX	DE,HL
		LD	HL,LINWRK
		CALL	C0780			; LDIRVM
		POP	HL
		RET

; Subroutine read char from screenlocation
; Input:  H=x pos, L=y pos
; Output: A=char, C=char
GETVRM:
C0B83:		PUSH	HL
		CALL	C0B98			; convert screenlocation to VRAM adres
		CALL	C07E1			; RDVRM
		LD	C,A
		POP	HL
		RET

; Subroutine write char at screenlocation
; Input:  H=x pos, L=y pos, C=char
PUTVRM:
C0B8D:		PUSH	HL
		CALL	C07F1			; set VDP for VRAM write at screenlocation
		LD	A,C
		OUT	(98H),A
		POP	HL
		RET

; Subroutine convert linenumber to VRAM adres
; Input:  L = y pos
; Output: HL = vram adres in nametable
C0B96:		LD	H,1			; x pos = 1

; Subroutine convert screenlocation to VRAM adres
; Input:  H = x pos, L = y pos
; Output: HL = vram adres in nametable
C0B98:		PUSH	BC
		DEC	H
		DEC	L				; make cursorpos 0 based
		LD	E,H
		LD	H,0
		LD	D,H
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		LD	C,L
		LD	B,H
		ADD	HL,HL
		ADD	HL,HL			; y pos*32
		LD	A,(SCRMOD)
		AND	A
		LD	A,(LINLEN)
		JR	Z,J0BB3			; screenmode 0,
		SBC	A,32+2
		JR	J0BC0			; screenmode 1, linlen-34

J0BB3:		CP	29H
		JR	C,J0BBD			; 40 kol mode,
		ADD	HL,BC			; y pos*40
		ADD	HL,HL			; y pos*80
		SBC	A,80+2
		JR	J0BC0			; screenmode 0 80 kol, linlen-82

J0BBD:		ADD	HL,BC			; y pos*40
		SBC	A,40+2
J0BC0:		ADD	HL,DE
		CPL
		AND	A
		RRA				; /2
		LD	E,A
		ADD	HL,DE
		EX	DE,HL
		LD	HL,(NAMBAS)
		ADD	HL,DE
		POP	BC
		RET

; Subroutine get LINTTB entry
; Input:  L = linenumber
; Output: DE = LINTTB entry for line
;         Zx when the one physiscal line is the succession of the line above
GETTRM:
C0BCD:		PUSH	HL
		LD	DE,LINTTB-1
		LD	H,0
		ADD	HL,DE
		LD	A,(HL)
		EX	DE,HL
		POP	HL
		AND	A
		RET

; Subroutine unmark line as succession
; Input:  L = linenumber
TERMIN:
C0BD9:		DEFB	03EH			; LD A,xx (skip next instruction)

UNTERM:
C0BDA:		XOR	A

C0BDB:		PUSH	AF
		CALL	C0BCD			; get LINTTB entry
		POP	AF
		LD	(DE),A
		RET

; Subroutine get max linenumber
GETLEN:
C0BE2:		LD	A,(CNSDFG)
		PUSH	HL
		LD	HL,CRTCNT
		ADD	A,(HL)
		POP	HL
		RET

; Subroutine SNSMAT
SNSMAT:
J0BEC:		LD	C,A
		DI
		IN	A,(0AAH)
		AND	0F0H
		ADD	A,C
		OUT	(0AAH),A
		EI
		IN	A,(0A9H)
		RET

; Subroutine ISFLIO
ISFLIO:
C0BF9:		SYSHOOK	H_ISFL
		PUSH	HL
		LD	HL,(PTRFIL)
		LD	A,L
		OR	H
		POP	HL
		RET

; Subroutine DCOMPR
DCOMPR:
J0C04:		LD	A,H
		SUB	D
		RET	NZ
		LD	A,L
		SUB	E
		RET

; Subroutine GETVCP
GETVCP:
C0C0A:		LD	L,2
		JR	C0C11

; Subroutine GETVC2
GETVC2:
J0C0E:		LD	A,(VOICEN)

C0C11:		PUSH	DE
		LD	DE,VCBA
		LD	H,0
		ADD	HL,DE
		OR	A
		JR	Z,J0C22
		LD	DE,37
J0C1E:		ADD	HL,DE
		DEC	A
		JR	NZ,J0C1E
J0C22:		POP	DE
		RET

; The following NOP really puzzels me
; It has nothing to do with compatibility to MSX1 or so
; Maybe is just leftover space and 0C25 was pushed against 0C3C

		DEFS	0C25H-$,0

J0C25:		LD	A,(NEWKEY+6)
		RRCA
		JR	C,J0C34			; SHIFT not pressed, just a KANA ON
		XOR	A
		LD	(CHRCNT),A
		INC	A			; 1
		SET	0,(HL)			; enter SHIFT KANA mode
		JR	J0C39			; KANA led ON

J0C34:		LD	A,0FFH			; KANA on
J0C36:		LD	(KANAST),A

J0C39:		JP	J0F29			; KANA led

; Subroutine KEYINT
; Remark: Entry at the same adres as MSX1
KEYINT:
J0C3C:		PUSH	HL
		PUSH	DE
		PUSH	BC
		PUSH	AF
		EXX
		EX	AF,AF'
		PUSH	HL
		PUSH	DE
		PUSH	BC
		PUSH	AF
		PUSH	IY
		PUSH	IX
		CALL	H_KEYI
		CALL	C1479			; MSX2 patch handle lightpen hardware
		JP	P,J0D02
		CALL	H_TIMI
		EI
		LD	(STATFL),A
		AND	20H
		LD	HL,TRPTBL+11*3
		CALL	NZ,REQTRP		; yep, raise SPRITE trap
		LD	HL,(INTCNT)
		DEC	HL			 ; decrease interval counter
		LD	A,H
		OR	L
		JR	NZ,J0C73		; not time for interval yet, skip
		LD	HL,TRPTBL+17*3
		CALL	REQTRP			; raise INTERVAL trap
		LD	HL,(INTVAL)
J0C73:		LD	(INTCNT),HL
		LD	HL,(JIFFY)
		INC	HL
		LD	(JIFFY),HL		; update JIFFY counter
		LD	A,(MUSICF)
		LD	C,A
		XOR	A				; playqueue 0
J0C82:		RR	C
		PUSH	AF
		PUSH	BC
		CALL	C,C1131			; queue active, do queue
		POP	BC
		POP	AF
		INC	A
		CP	3
		JR	C,J0C82			; do all 3 playqueues
		LD	HL,SCNCNT
		DEC	(HL)			; time for a keyboard scan ?
		JR	NZ,J0D02		; nope, quit interrupt routine
	IF INTHZ = 60
		LD	(HL),2			; scanfrequency 2*1000/60 = 33 ms
	ELSE
		LD	(HL),1			; scanfrequency 1*1000/50 = 20 ms
	ENDIF
		XOR	A
		CALL	C1202
		AND	30H			; read joystick 0 trigger status
		PUSH	AF
		LD	A,1
		CALL	C1202
		AND	30H			; read joystick 1 trigger status
		RLCA
		RLCA
		POP	BC
		OR	B
		PUSH	AF
		CALL	C121C
		AND	01H			; read spacekey status
		POP	BC
		OR	B
		LD	C,A
		LD	HL,TRGFLG
		XOR	(HL)
		AND	(HL)
		LD	(HL),C
		LD	C,A
		RRCA
		LD	HL,TRPTBL+12*3
		CALL	C,REQTRP			; space key pressed, raise STRIG(0) trap
		RL	C
		LD	HL,TRPTBL+16*3
		CALL	C,REQTRP			; trigger B joystick 1 pressed, raise STRIG(4) trap
		RL	C
		LD	HL,TRPTBL+14*3
		CALL	C,REQTRP			; trigger A joystick 1 pressed, raise STRIG(2) trap
		RL	C
		LD	HL,TRPTBL+15*3
		CALL	C,REQTRP			; trigger B joystick 0 pressed, raise STRIG(3) trap
		RL	C
		LD	HL,TRPTBL+13*3
		CALL	C,REQTRP			; trigger A joystick 0 pressed, raise STRIG(1) trap
		XOR	A
		LD	(CLIKFL),A
		CALL	C0D12			; scan keyboard and translate to keycodes
		JR	NZ,J0D02		; keyboard buffer not empty, quit interrupt routine
		LD	HL,REPCNT
		DEC	(HL)
		JR	NZ,J0D02		; no autorepeat yet, quit interrupt routine
	IF INTHZ = 50
		LD	(HL),2
	ELSE
		LD	(HL),1			; autorepeat every keyboardscan
	ENDIF
		LD	HL,OLDKEY
		LD	DE,OLDKEY+1
		LD	BC,11-1
		LD	(HL),0FFH
		LDIR				; reinitialize OLDKEY (to force new transitions)
		CALL	C0D4E			; scan for transitions and translate to keycodes
J0D02:		POP	IX
		POP	IY
		POP	AF
		POP	BC
		POP	DE
		POP	HL
		EX	AF,AF'
		EXX
		POP	AF
		POP	BC
		POP	DE
		POP	HL
		EI
		RET

; Subroutine scan keyboard and translate to keycodes
; Output: Zx set if keyboardbuffer is empty
; Remark: Entry at the same adres as MSX1
C0D12:		IN	A,(0AAH)
		AND	0F0H
		LD	C,A
		LD	B,11
		LD	HL,NEWKEY
J0D1C:		LD	A,C
		OUT	(0AAH),A
		IN	A,(0A9H)
		LD	(HL),A
		INC	C
		INC	HL
		DJNZ	J0D1C
		LD	A,(ENSTOP)
		AND	A
		JR	Z,J0D3A
		LD	A,(NEWKEY+6)
		CP	0E8H
		JR	NZ,J0D3A
		LD	IX,READYR
		JP	CALBAS			; CALBAS

J0D3A:		LD	DE,OLDKEY+11
		LD	B,11
J0D3F:		DEC	DE
		DEC	HL
		LD	A,(DE)
		CP	(HL)
		JR	NZ,J0D49		; transition, reset REPCNT
		DJNZ	J0D3F
		JR	C0D4E

J0D49:
	IF INTHZ = 60
		LD	A,14H			; autorepeat at 20 keyboardscans (= 666 ms)
	ELSE
		LD	A,20H			; autorepeat at 32 keyboardscans (= 640 ms)
	ENDIF
		LD	(REPCNT),A

; Subroutine scan for transitions and translate to keycodes
C0D4E:		LD	B,11
		LD	HL,OLDKEY
		LD	DE,NEWKEY
J0D56:		LD	A,(DE)
		LD	C,A
		XOR	(HL)			; only keys which changed status
		AND	(HL)			; and are being pressed
		LD	(HL),C
		CALL	NZ,C0D89		; convert to scancode and handle
		INC	DE
		INC	HL
		DJNZ	J0D56

; Subroutine check if keyboardbuffer is empty
C0D62:		LD	HL,(GETPNT)
		LD	A,(PUTPNT)
		SUB	L
		RET

; Subroutine CHSNS
; Remark: Entry at the same adres as MSX1
CHSNS:
C0D6A:		EI
		PUSH	HL
		PUSH	DE
		PUSH	BC
		CALL	C0B4E			; check if screen is in textmode
		JR	NC,J0D82		; nope, skip functionkey display
		LD	A,(FNKSWI)
		LD	HL,NEWKEY+6
		XOR	(HL)
		LD	HL,CNSDFG
		AND	(HL)
		RRCA
		CALL	C,C0B3F			; DSPFNK
J0D82:		CALL	C0D62			; check keyboardbuffer is empty
		POP	BC
		POP	DE
		POP	HL
		RET

; Subroutine convert to scancode and handle
; Remark: Entry at the same adres as MSX1
C0D89:		PUSH	HL
		PUSH	DE
		PUSH	BC
		PUSH	AF
		LD	A,11
		SUB	B
		ADD	A,A
		ADD	A,A
		ADD	A,A
		LD	C,A
		LD	B,8
		POP	AF
J0D97:		RRA
		PUSH	BC
		PUSH	AF
		CALL	C,KEYCOD
		POP	AF
		POP	BC
		INC	C
		DJNZ	J0D97
		JP	J0938

		INCLUDE "local/keyint1.asm"
		
		ALIGN	010C2H

; Subroutine increase keyboardbuffer pointer
C10C2:		INC	HL			 ; increase pointer
		LD	A,L
KEYLOW		EQU	(KEYBUF+40) % 256
		CP	KEYLOW
		RET	NZ			 ; not the end of buffer, quit
		LD	HL,KEYBUF		; warp around to start of buffer
		RET

; Subroutine CHGET
CHGET:
C10CB:		PUSH	HL
		PUSH	DE
		PUSH	BC
		SYSHOOK	H_CHGE
		CALL	C0D6A			; CHSNS
		JR	NZ,J10E1
		CALL	C0A37			; display cursor when disabled
J10D9:		CALL	C0D6A			; CHSNS
		JR	Z,J10D9			; wait for input
		CALL	C0A84			; remove cursor when disabled
J10E1:		LD	HL,INTFLG
		LD	A,(HL)
		CP	4
		JR	NZ,J10EB
		LD	(HL),0
J10EB:		LD	HL,(GETPNT)
		LD	C,(HL)
		CALL	C10C2			; increase keyboardbuffer pointer (warp around)
		LD	(GETPNT),HL
		LD	A,C
		JP	J0938

; Subroutine CKCNTC
CKCNTC:
J10F9:		PUSH	HL
		LD	HL,0
		CALL	C04F0			; ISCNTC
		POP	HL
		RET

; Subroutine WRTPSG
WRTPSG:
C1102:		DI
		OUT	(0A0H),A
		PUSH	AF
		LD	A,E
		EI
		OUT	(0A1H),A
		POP	AF
		RET

C110C:	LD	A,0EH

; Subroutine RDPSG
RDPSG:
C110E:		OUT	(0A0H),A
		IN	A,(0A2H)
		RET

; Subroutine BEEP
BEEP:
C1113:		PUSH	IX
		LD	IX,S_BEEP
		JP	SUBROM			; SUBROM

J111C:		PUSH	AF
		LD	A,0FH
		OUT	(0A0H),A
		IN	A,(0A2H)
		AND	7FH
		LD	B,A
		POP	AF
		OR	A
		LD	A,80H
		JR	Z,J112D
		XOR	A
J112D:		OR	B
		OUT	(0A1H),A
		RET

C1131:		LD	B,A
		CALL	C0C0A			; GETVCP
		DEC	HL
		LD	D,(HL)
		DEC	HL
		LD	E,(HL)
		DEC	DE
		LD	(HL),E
		INC	HL
		LD	(HL),D
		LD	A,D
		OR	E
		RET	NZ
		LD	A,B
		LD	(QUEUEN),A
		CALL	C11D8
		CP	0FFH
		JR	Z,J11A6
		LD	D,A
		AND	0E0H
		RLCA
		RLCA
		RLCA
		LD	C,A
		LD	A,D
		AND	1FH
		LD	(HL),A
		CALL	C11D8
		DEC	HL
		LD	(HL),A
		INC	C
J115C:		DEC	C
		RET	Z
		CALL	C11D8
		LD	D,A
		AND	0C0H
		JR	NZ,J1177
		CALL	C11D8
		LD	E,A
		LD	A,B
		RLCA
		CALL	WRTPSG			; WRTPSG
		INC	A
		LD	E,D
		CALL	WRTPSG			; WRTPSG
		DEC	C
		JR	J115C

J1177:		LD	H,A
		AND	80H
		JR	Z,J118B
		LD	E,D
		LD	A,B
		ADD	A,8
		CALL	WRTPSG			; WRTPSG
		LD	A,E
		AND	10H
		LD	A,13
		CALL	NZ,WRTPSG		; WRTPSG
J118B:		LD	A,H
		AND	40H
		JR	Z,J115C
		CALL	C11D8
		LD	D,A
		CALL	C11D8
		LD	E,A
		LD	A,11
		CALL	WRTPSG			; WRTPSG
		INC	A
		LD	E,D
		CALL	WRTPSG			; WRTPSG
		DEC	C
		DEC	C
		JR	J115C

J11A6:		LD	A,B
		ADD	A,08H	; 8 
		LD	E,0
		CALL	WRTPSG			; WRTPSG
		INC	B
		LD	HL,MUSICF
		XOR	A
		SCF
J11B4:		RLA
		DJNZ	J11B4
		AND	(HL)
		XOR	(HL)
		LD	(HL),A

; Subroutine STRTMS
STRTMS:
J11BA:		LD	A,(MUSICF)
		OR	A
		RET	NZ
		LD	HL,PLYCNT
		LD	A,(HL)
		OR	A
		RET	Z
		DEC	(HL)
		LD	HL,1
		LD	(VCBA+0),HL
		LD	(VCBB+0),HL
		LD	(VCBC+0),HL
		LD	A,07H	; 7 
		LD	(MUSICF),A
		RET

C11D8:	LD	A,(QUEUEN)
		PUSH	HL
		PUSH	DE
		PUSH	BC
		CALL	GETQ
		JP	J0938

; Subroutine GTSTCK
GTSTCK:
J11E4:		DEC	A
		JP	M,J11F6
		CALL	C1202
		LD	HL,I1229
J11EE:		AND	0FH
		LD	E,A
		LD	D,0
		ADD	HL,DE
		LD	A,(HL)
		RET

J11F6:		CALL	C121C			; read keyboard row 8
		RRCA
		RRCA
		RRCA
		RRCA
		LD	HL,I1239
		JR	J11EE

C1202:		LD	B,A
		LD	A,15
		DI
		CALL	RDPSG			; RDPSG
		DJNZ	J1211
		AND	0DFH
		OR	4CH
		JR	J1215

J1211:		AND	0AFH
		OR	03H
J1215:		OUT	(0A1H),A
		CALL	C110C			; read PSG register 14
		EI
		RET

; Subroutine read keyboard row 8
C121C:		DI
		IN	A,(0AAH)
		AND	0F0H
		ADD	A,08H	; 8 
		OUT	(0AAH),A
		IN	A,(0A9H)
		EI
		RET

I1229:		DEFB	000H,005H,001H,000H
		DEFB	003H,004H,002H,003H
		DEFB	007H,006H,008H,007H
		DEFB	000H,005H,001H,000H

I1239:		DEFB	000H,003H,005H,004H
		DEFB	001H,002H,000H,003H
		DEFB	007H,000H,006H,005H
		DEFB	008H,001H,007H,000H

; Subroutine GTTRIG
GTTRIG:
J1249:		DEC	A
		JP	M,J1262
		PUSH	AF
		AND	01H
		CALL	C1202
		POP	BC
		DEC	B
		DEC	B
		LD	B,10H
		JP	M,J125D
		LD	B,20H
J125D:		AND	B
J125E:		SUB	01H
		SBC	A,A
		RET

J1262:		CALL	C121C
		AND	01H
		JR	J125E

; Subroutine GTPDL
GTPDL:
J1269:		INC	A
		AND	A
		RRA
		PUSH	AF
		LD	B,A
		XOR	A
		SCF
J1270:		RLA
		DJNZ	J1270
		LD	B,A
		POP	AF
		LD	C,10H	; 16 
		LD	DE,003AFH
		JR	NC,J1281
		LD	C,20H	; " "
		LD	DE,04C9FH
J1281:		LD	A,0FH	; 15 
		DI
		CALL	C110E
		AND	E
		OR	D
		OR	C
		OUT	(0A1H),A
		XOR	C
		OUT	(0A1H),A
		LD	A,0EH	; 14 
		OUT	(0A0H),A
		LD	C,0
J1295:		IN	A,(0A2H)
		AND	B
		JR	Z,J129F
		INC	C
		JP	NZ,J1295
		DEC	C
J129F:		EI
		LD	A,C
		RET

; Subroutine GTPAD
GTPAD:
J12A2:		CP	8
		JR	C,J12AF
		PUSH	IX
		LD	IX,S_NEWPAD
		JP	SUBROM			; new style pads handled by SUBROM

J12AF:		CP	4
		LD	DE,0CECH
		JR	C,J12BB
		LD	DE,03D3H
		SUB	4
J12BB:		DEC	A
		JP	M,J12C8
		DEC	A
		LD	A,(PADX)
		RET	M
		LD	A,(PADY)
		RET	Z
J12C8:		PUSH	AF
		EX	DE,HL
		LD	(FILNAM+0),HL
		SBC	A,A
		CPL
		AND	40H
		LD	C,A
		LD	A,15
		DI
		CALL	RDPSG			; RDPSG
		AND	0BFH
		OR	C
		OUT	(0A1H),A
		POP	AF
		JP	M,J12EB
		CALL	C110C
		EI
		AND	08H
		SUB	01H
		SBC	A,A
		RET

J12EB:		LD	C,0
		CALL	C1335
		CALL	C1335
		JR	C,J131D
		CALL	C1323
		JR	C,J131D
		PUSH	DE
		CALL	C1323
		POP	BC
		JR	C,J131D
		LD	A,B
		SUB	D
		JR	NC,J1307
		CPL
		INC	A
J1307:		CP	05H
		JR	NC,J12EB
		LD	A,C
		SUB	E
		JR	NC,J1311
		CPL
		INC	A
J1311:		CP	05H
		JR	NC,J12EB
		LD	A,D
		LD	(PADX),A
		LD	A,E
		LD	(PADY),A
J131D:		EI
		LD	A,H
		SUB	01H
		SBC	A,A
		RET

C1323:		LD	C,0AH	; 10 
		CALL	C1335
		RET	C
		LD	D,L
		PUSH	DE
		LD	C,0
		CALL	C1335
		POP	DE
		LD	E,L
		XOR	A
		LD	H,A
		RET

C1335:		CALL	C135E
		LD	B,8
		LD	D,C
J133B:		RES	0,D
		RES	2,D
		CALL	C1370
		CALL	C110C
		LD	H,A
		RRA
		RRA
		RRA
		RL	L
		SET	0,D
		SET	2,D
		CALL	C1370
		DJNZ	J133B
		SET	4,D
		SET	5,D
		CALL	C1370
		LD	A,H
		RRA
		RET

C135E:		LD	A,35H	; "5"
		OR	C
		LD	D,A
		CALL	C1370
J1365:		CALL	C110C
		AND	02H
		JR	Z,J1365
		RES	4,D
		RES	5,D

C1370:		PUSH	HL
		PUSH	DE
		LD	HL,(FILNAM+0)
		LD	A,L
		CPL
		AND	D
		LD	D,A
		LD	A,15
		OUT	(0A0H),A
		IN	A,(0A2H)
		AND	L
		OR	D
		OR	H
		OUT	(0A1H),A
		POP	DE
		POP	HL
		RET

; Subroutine STMOTR
STMOTR:
J1387:		AND	A
		JP	M,J1395
J138B:		JR	NZ,J1390
		LD	A,09H
		DEFB	0C2H
J1390:		LD	A,08H
		OUT	(0ABH),A
		RET
J1395:		IN	A,(0AAH)
		AND	10H
		JR	J138B

; Subroutine NMI (NonMaskable Interrupt routine Z80)
NMI:
J139B:		SYSHOOK	H_NMI
		RETN

; Subroutine INIFNK
INIFNK:
J13A0:		LD	HL,FNKSTR
		LD	B,9FH
		PUSH	HL
		XOR	A
J13A7:		LD	(HL),A
		INC	HL
		DJNZ	J13A7
		POP	HL
		LD	B,10			; 10 functionkeys
		LD	DE,I13C3
J13B1:		LD	C,16
J13B3:		LD	A,(DE)
		INC	DE
		LD	(HL),A
		INC	HL
		DEC	C
		OR	A
		JR	NZ,J13B3
		PUSH	BC
		LD	B,0
		ADD	HL,BC
		POP	BC
		DJNZ	J13B1
		RET

I13C3:		DEFB	"color ",0
		DEFB	"auto ",0
		DEFB	"goto ",0
		DEFB	"list ",0
		DEFB	"run",13,0
	IF BASVER = 0
		DEFB	"color 15,4,7",13,0
	ELSE
		DEFB	"color 15,4,4",13,0
	ENDIF
		DEFB	"cload\"",0
		DEFB	"cont",13,0
		DEFB	"list.",13,30,30,0
		DEFB	12,"run",13,0

; Subroutine RDVDP
RDVDP:
J140B:		IN	A,(99H)
		RET

; Subroutine RSLREG
RSLREG:
J140E:		IN	A,(0A8H)
		RET

; Subroutine WSLREG
WSLREG:
J1411:		OUT	(0A8H),A
		RET

; Subroutine PHYDIO
PHYDIO:
J1414:		SYSHOOK	H_PHYD
		RET

; Subroutine FORMAT
FORMAT:
J1418:		SYSHOOK	H_FORM
		RET


; MSX2 has unused space at 141CH-1479H

		DEFS	1479H-$,0

; Subroutine patch handle lightpen hardware
C1479:		IN	A,(0BAH)
		AND	10H
		JR	NZ,J148C
		LD	HL,XSAVE+1
		SET	7,(HL)
		LD	A,20H
		OUT	(0BBH),A
		OR	08H
		OUT	(0BBH),A
J148C:		IN	A,(99H)
		AND	A
		RET
