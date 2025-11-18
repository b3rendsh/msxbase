; ------------------------------------------------------------------------------
; bios.asm
; MSX 1 BIOS version 1.0
; MSX 2 BIOS version 2.0
;
; Modules: BIOHDR,SLOT,MSXIO,QUEUETL,MSXGRP,CASET,BIO,MSXCHR,MSXINL
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
; Merged MSX 1 and MSX 2 BIOS common modules
; ------------------------------------------------------------------------------

DEFINE	_BIOS_	; avoid duplicate label definitions

		INCLUDE "base.inc"
		INCLUDE "msx.inc"

; Symbols defined in BASIC module
INIENT		EQU	02680H
SYNENT		EQU	02683H
CHRENT		EQU	02686H
GETENT		EQU	02689H
READYR		EQU	0409BH
CSTOP		EQU	063E6H
FILOUT		EQU	06C48H
DIOERR		EQU	073B2H
	IF MSX2 || SLOTFIX
C7FBE		EQU	07FBEH			; helper routine RDSLT
C7FC4		EQU	07FC4H			; helper routine WRSLT
	ENDIF
	IFDEF MSX2
C7D75		EQU	07D75H           	; handle extension ROM
J7B61		EQU	07B61H			; hardware intialization, part 2
PTCPAL		EQU	07BD2H			; if INTHZ=50
RDWEXP		EQU	07E1AH			; read word from slot
	
; Symbols defined in SUBROM module
S_CHGMOD        EQU     00D1h
S_INITXT        EQU     00D5h
S_INIT32        EQU     00D9h
S_INIGRP        EQU     00DDh
S_INIMLT        EQU     00E1h
S_SETTXT        EQU     00E5h
S_SETT32        EQU     00E9h
S_SETGRP        EQU     00EDh
S_SETMLT        EQU     00F1h
S_CLRSPR        EQU     00F5h
S_CLS           EQU     0115h
S_CLRTXT        EQU     0119h
S_DSPFNK        EQU     011Dh
S_DELLNO        EQU     0121h
S_INSLNO        EQU     0125h
S_WRTVDP        EQU     012Dh
S_PUTCHR        EQU     0139h
S_BEEP          EQU     017Dh
S_NEWPAD        EQU     01ADh
S_CHGMDP        EQU     01B5h
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
; BIOHDR.MAC
; BIOS jump table
; ------------------------------------------------------------------------------

		ORG	0000H

		di
		jp	CHKRAM

		defw	CGTABL
VDP_DR: 	defb	098H
VDP_DW: 	defb	098H

		jp	SYNENT
		defs	0000CH-$,0
		jp	RDSLT
		defs	00010H-$,0
		jp	CHRENT
		defs	00014H-$,0
		jp	WRSLT
		defs	00018H-$,0
		jp	OUTDO
		defs	0001CH-$,0
		jp	CALSLT
		defs	00020H-$,0
		jp	DCOMPR
		defs	00024H-$,0
		jp	ENASLT
		defs	00028H-$,0
		jp	GETENT

IDBYT0:
	IF INTHZ = 60
		DEFB	CHRGEN+16*DATFMT
	ELSE
		DEFB	CHRGEN+16*DATFMT+128
	ENDIF
IDBYT1:		DEFB	KEYTYP+16*BASVER
IDBYT2:		DEFB	MSXVER			; MSX version 0 = MSX1
		DEFB	0

		DEFS	00030H-$
		jp	CALLF

		DEFS	00034H-$

; The next bytes are used by the diskrom, to initialize the double byte header char
; table (0F30FH). I have not seen a MSX with anything other than four zero's, meaning
; no double byte chars.
CHAR_16:	defb	0,0
		defb	0,0

		jp	KEYINT
		jp	INITIO
		jp	INIFNK
		jp	DISSCR
		jp	ENASCR
		jp	WRTVDP
		jp	RDVRM
		jp	WRTVRM
		jp	SETRD
		jp	SETWRT
		jp	FILVRM
		jp	LDIRMV
		jp	LDIRVM
		jp	CHGMOD
		jp	CHGCLR

		defs	00066H-$,0		; align to Z80 NMI entry at 0066H

		jp	NMI
		jp	CLRSPR
		jp	INITXT
		jp	INIT32
		jp	INIGRP
		jp	INIMLT
		jp	SETTXT
		jp	SETT32
		jp	SETGRP
		jp	SETMLT
		jp	CALPAT
		jp	CALATR
		jp	GSPSIZ
		jp	GRPPRT
		jp	GICINI
		jp	WRTPSG
		jp	RDPSG
		jp	STRTMS
		jp	CHSNS
		jp	CHGET
		jp	CHPUT
		jp	LPTOUT
		jp	LPTSTT
		jp	CNVCHR
		jp	PINLIN
		jp	INLIN
		jp	QINLIN
		jp	BREAKX
		jp	ISCNTC
		jp	CKCNTC
		jp	BEEP
		jp	CLS
		jp	POSIT
		jp	FNKSB
		jp	ERAFNK
		jp	DSPFNK
		jp	TOTEXT
		jp	GTSTCK
		jp	GTTRIG
		jp	GTPAD
		jp	GTPDL
		jp	TAPION
		jp	TAPIN
		jp	TAPIOF
		jp	TAPOON
		jp	TAPOUT
		jp	TAPOOF
		jp	STMOTR
		jp	LFTQ
		jp	PUTQ
		jp	RIGHTC
		jp	LEFTC
		jp	UPC
		jp	TUPC
		jp	DOWNC
		jp	TDOWNC
		jp	SCALXY
		jp	MAPXYC
		jp	FETCHC
		jp	STOREC
		jp	SETATR
		jp	READC
		jp	SETC
		jp	NSETCX
		jp	GTASPC
		jp	PNTINI
		jp	SCANR
		jp	SCANL
		JP	CHGCAP
		JP	CHGSND
		jp	RSLREG
		jp	WSLREG
		jp	RDVDP
		jp	SNSMAT
		jp	PHYDIO
		jp	FORMAT
		jp	ISFLIO
		jp	OUTDLP
		jp	GETVCP
		jp	GETVC2
		jp	KILBUF
		jp	CALBAS
	IFDEF MSX2
		jp	SUBROM
		jp	EXTROM
		jp	CHKSLZ
		jp	CHKNEW
		jp	EOL
		jp	BIGFIL
		jp	NSETRD
		jp	NSTWRT
		jp	NRDVRM
		jp	NWRVRM
	ENDIF

; ------------------------------------------------------------------------------
; SLOT.MAC
; BIOS slot functions
; ------------------------------------------------------------------------------

	IFDEF MSX1
		INCLUDE "100/slot.asm"
	ELSE
		INCLUDE	"200/slot.asm"
	ENDIF

; ------------------------------------------------------------------------------
; MSXIO.MAC
; BIOS MSX I/O functions
; ------------------------------------------------------------------------------

	IFDEF MSX1
		INCLUDE	"100/msxio.asm"
	ELSE
		INCLUDE	"200/msxio.asm"
	ENDIF

; ------------------------------------------------------------------------------
; QUEUETL.MAC
; BIOS queue functions
;
; Note: Generic queue functions, only used by PLAY statement
;	 The BAKQ function was left out (unused), but info block does have a backup byte flag
;	 used in code. There is also a backup byte location, also used in code.
;	 But since there is no putback code, both are not actually used
;	 The NUMQ function was also left out (unused)
;
; queue info block
;	 +0	PUT offset
;	 +1	GET offset
;	 +2	0 = no backup byte, >0 = backup location in QUEBAK
;	 +3	queue size (must be a power of 2 -1)
;	 +4,2	pointer to queue

	IFDEF MSX1
		ALIGN	1492H
	ELSE
		ALIGN	1490H
	ENDIF

; Subroutine PUTQ
; Input:  A = queue, E = byte
; Output: Zx set if queue is full
PUTQ:
; MSX2: A1490
A1492:		call	A14FA			; get queue info
		ld	a,b			; PUT offset
		inc	a
		inc	hl
		and	(hl)			; warp around queue size
		cp	c			; space left in queue ?
		ret	z			; nope, quit
		push	hl			; store pointer to queue size
		dec	hl
		dec	hl
		dec	hl
		ex	(sp),hl			; store pointer to queue info block, restore pointer to queue size
		inc	hl
		ld	c,a
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a			; pointer to queue
		ld	b,0
		add	hl,bc
		ld	(hl),e			; store byte in queue
		pop	hl			; restore pointer to queue info block
		ld	(hl),c			; update PUT offset
		ret

; Subroutine GETQ
; Input:  A = queue
; Output: Zx set if empty queue
;         A = byte (only when Zx reset)
GETQ:
;MSX2: A14AB
A14AD:		call	A14FA			; get queue info
		ld	(hl),0			; reset backup byte flag
		jr	nz,A14D1		; backup byte, return backup byte
		ld	a,c			; GET offset
		cp	b
		ret	z			; queue empty, quit
		inc	hl
		inc	a			; update GET offset
		and	(hl)			; warp around queue size
		dec	hl
		dec	hl
		push	hl			; store pointer to GET offset
		inc	hl
		inc	hl
		inc	hl
		ld	c,a
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a			; pointer to queue
		ld	b,0
		add	hl,bc
		ld	a,(hl)			; get byte from queue
		pop	hl			; restore pointer to GET offset
		ld	(hl),c			; update GET offset
		or	a
		ret	nz
		inc	a
		ld	a,0			; Zx reset
		ret

; MSX2: A14CF
A14D1:		ld	c,a			; backup offset
		ld	b,0
		ld	hl,QUEBAK-1
		add	hl,bc
		ld	a,(hl)			; return backup byte
		ret

; Subroutine INITQ
; Input:  A  = queue
;         B  = queue size (must be a power of 2 -1)
;         DE = pointer to queue
INITQ:
; MSX2: A14D8
A14DA:		push	bc
		call	A1504			; get queue info block
		ld	(hl),b			; PUT offset = 0
		inc	hl
		ld	(hl),b			; GET offset = 0
		inc	hl
		ld	(hl),b			; reset backup byte flag
		inc	hl
		pop	af
		ld	(hl),a			; queue size
		inc	hl
		ld	(hl),e
		inc	hl
		ld	(hl),d			; pointer to queue
		ret

	  IF 0

; Subroutine BAKQ
; Input:  A = queue
;         E = backup byte
; Remark: Unused Code with SVI, left out with MSX
BAKQ:		push	af
		call	A14FA			; get queue info
		pop	af
		inc	a
		ld	(hl),a
		ld	c,a
		ld	b,0
		ld	hl,QUEBAK-1
		add	hl,bc
		ld	(hl),e
		ret

; Subroutine NUMQ
; Input:  A  = queue
; Output: HL = bytes in queue
; Remark: Unused Code with SVI, left out with MSX
NUMQ:		call	A14FA			; get queue info
		sub	1
		sbc	a,a
		inc	a
		ld	e,a			; extra byte if backup byte
		ld	a,b
		sub	c
		inc	hl
		and	(hl)			; warp around queue size
		ld	l,a
		ld	h,0			; bytes in queue
		ld	d,h
		add	hl,de			; + backup byte
		ret

	  ENDIF


; Subroutine LFTQ
; Input:  A = queue
; Output: A = HL = bytes left in queue
LFTQ:
; MSX2: A14E9
A14EB:		call	A14FA			; get queue info
		ld	a,b			; GET offset
		inc	a			; update
		inc	hl
		and	(hl)			; warp around queue size
		ld	b,a
		ld	a,c			; PUT offset
		sub	b
		and	(hl)
		ld	l,a
		ld	h,0
		ret

; Subroutine get queue info
; Input:  A  = queue
; Output: B  = PUT offset
;		C  = GET offset
;		A  = backup byte flag
;		HL = pointer to backup flag queue info block
; MSX2: A14F8
A14FA:		call	A1504			; get queue info block
		ld	b,(hl)			; PUT offset
		inc	hl
		ld	c,(hl)			; GET offset
		inc	hl
		ld	a,(hl)
		or	a			; backup byte ?
		ret

; Subroutine get queue info block
; Input:  A = queue
; Output: HL = pointer to queue info block
; MSX2: A1502
A1504:		rlca
		ld	b,a			; *2
		rlca				; *4
		add	a,b			; *6
		ld	c,a
		ld	b,0
		ld	hl,(QUEUES)		; base of queue info blocks
		add	hl,bc
		ret

; ------------------------------------------------------------------------------
; MSXGRP.MAC
; BIOS graphic functions
; ------------------------------------------------------------------------------

	IFDEF MSX1
		INCLUDE "100/msxgrp.asm"
	ELSE
		INCLUDE	"200/msxgrp.asm"
	ENDIF

; ------------------------------------------------------------------------------
; CASET.MAC
; MSX BIOS CASSETTE functions
; ------------------------------------------------------------------------------

		ALIGN	19DDH

; Subroutine TAPOOF
TAPOOF:
A19DD:		push	bc
		push	af
		ld	bc,0
A19E2:		dec	bc
		ld	a,b
		or	c
		jr	nz,A19E2		; wait 550 msec
		pop	af
		pop	bc			; cassettemotor off

; Subroutine TAPIOF
TAPIOF:
A19E9:		push	af
		ld	a,009H
		out	(0ABH),a
		pop	af
		ei
		ret

; Subroutine TAPOON
TAPOON:
A19F1:		or	a
		push	af			; flag for headerlength
		ld	a,008H
		out	(0ABH),a		; cassettemotor on
		ld	hl,0
A19FA:		dec	hl
		ld	a,h
		or	l
		jr	nz,A19FA		; wait 550 msec
		pop	af
		ld	a,(HEADER)		; headerlength
		jr	z,A1A07			; short header
		add	a,a
		add	a,a			; *4 for long header
A1A07:		ld	b,a
		ld	c,0			; *256 = # of cycli
		di
A1A0B:		call	A1A4D			; high cycle
		call	A1A3F			; wait
		dec	bc
		ld	a,b
		or	c
		jr	nz,A1A0B		; next cyclus
		jp	BREAKX			; quit with BREAKX

; Subroutine TAPOUT
TAPOUT:
A1A19:		ld	hl,(LOW_)		; size of low cycle
		push	af
		ld	a,l
		sub	00EH
		ld	l,a			; 14 units shorter
		call	A1A50			; low cycle (start bit)
		pop	af
		ld	b,8			; 8 bits data
A1A27:		rrca
		call	c,A1A40			; 1, write mark
		call	nc,A1A39		; 0, write space
		djnz	A1A27			; next bit
		call	A1A40
		call	A1A40			; write 2 marks (stopbits)
		jp	BREAKX			; quit with BREAKX
;
A1A39:		ld	hl,(LOW_)		; low cycle size
		call	A1A50			; write low cycle
A1A3F:		ret
;
A1A40:		call	A1A4D			; write high cycle
		ex	(sp),hl
		ex	(sp),hl
		nop
		nop
		nop
		nop				; wait
		call	A1A4D			; write high cycle
		ret
;
A1A4D:		ld	hl,(HIGH_)		; length of high cycle
A1A50:		push	af
A1A51:		dec	l
		jp	nz,A1A51		; wait low part
		ld	a,00BH
		out	(0ABH),a		; high
A1A59:		dec	h
		jp	nz,A1A59		; wait high part
		ld	a,00AH
		out	(0ABH),a		; low
		pop	af
		ret

; Subroutine TAPION
TAPION:
A1A63:		ld	a,008H
		out	(0ABH),a		; cassettemotor on
		di
		ld	a,00EH
		out	(0A0H),a		; select register 14 of PSG
A1A6C:		ld	hl,1111
A1A6F:		ld	d,c
		call	A1B34			; count length of cycle
		ret	c			; break, quit
		ld	a,c
		cp	222
		jr	nc,A1A6C		; illegal, start again
		cp	4+1
		jr	c,A1A6C			; illegal, start again
		sub	d			; compare with previous cycle
		jr	nc,A1A82
		cpl
		inc	a
A1A82:		cp	4			; difer less then 35  sec ?
		jr	nc,A1A6C		; nop, start again
		dec	hl
		ld	a,h
		or	l			; all 1111 cycles tollerant ?
		jr	nz,A1A6F		; nop, cont
		ld	hl,0
		ld	b,l
		ld	d,l			; 256
A1A90:		call	A1B34			; count length of cycle
		ret	c			; break, quit
		add	hl,bc			; add to total
		dec	d
		jp	nz,A1A90		; next cycle
		ld	bc,1710
		add	hl,bc			; add extra for calc
		ld	a,h
		rra
		and	07FH
		ld	d,a			; /2
		add	hl,hl			; *2
		ld	a,h
		sub	d			; = 1.5*
		ld	d,a
		sub	6			; - extra
		ld	(LOWLIM),a		; min. size of startbit
		ld	a,d
		add	a,a
		ld	b,0
A1AAF:		sub	3
		inc	b
		jr	nc,A1AAF
		ld	a,b
		sub	3
		ld	(WINWID),a
		or	a
		ret

; Subroutine TAPIN
TAPIN:
A1ABC:		ld	a,(LOWLIM)
		ld	d,a			; min. size of startbit
A1AC0:		call	BREAKX			; BREAKX
		ret	c			; break, quit
		in	a,(0A2H)
		rlca
		jr	nc,A1AC0		; wait for high
A1AC9:		call	BREAKX			; BREAKX
		ret	c			; break, quit
		in	a,(0A2H)
		rlca
		jr	c,A1AC9			; wait for low
		ld	e,0
		call	A1B1F			; count length of cycle
A1AD7:		ld	b,c
		call	A1B1F			; count length of cycle
		ret	c			; break, quit
		ld	a,b
		add	a,c			; size of two cycles
		jp	c,A1AD7			; illegal, try again
		cp	d			; > as LOWLIM ?
		jr	c,A1AD7			; nop, try again
		ld	l,8			; 8 bits
A1AE6:		call	A1B03			; count transitions
		cp	4
		ccf
		ret	c			; illegal, quit
		cp	2
		ccf				; databit
		rr	d
		ld	a,c
		rrca				; even transitions ?
		call	nc,A1B23		; yep, skip 1 transitions
		call	A1B1F			; skip 1 transitions
		dec	l
		jp	nz,A1AE6		; next bit
		call	BREAKX			; BREAKX
		ld	a,d
		ret
;
A1B03:		ld	a,(WINWID)
		ld	b,a
		ld	c,0
A1B09:		in	a,(0A2H)
		xor	e
		jp	p,A1B17
		ld	a,e
		cpl
		ld	e,a
		inc	c
		djnz	A1B09
		ld	a,c
		ret
;
A1B17:		nop
		nop
		nop
		nop
A1B1B:		djnz	A1B09
		ld	a,c
		ret
;
A1B1F:		call	BREAKX			; BREAKX
		ret	c
A1B23:		ld	c,0
A1B25:		inc	c
		jr	z,A1B32
		in	a,(0A2H)
		xor	e
		jp	p,A1B25
		ld	a,e
		cpl
		ld	e,a
		ret
;
A1B32:		dec	c
		ret
;
A1B34:		call	BREAKX			; BREAKX
		ret	c			; break, quit
		in	a,(0A2H)
		rlca
		jr	c,A1B34			; wait until casinput = 0
		ld	e,0
		call	A1B23			; count to high input
		jp	A1B25			; add count to low input

; ------------------------------------------------------------------------------
; BIO.MAC
; BASIC I/O functions (part in page 0)
; ------------------------------------------------------------------------------

		ALIGN	1B45H

; Subroutine OUTDO
OUTDO:
A1B45:		push	af
		SYSHOOK	H_OUTD
		call	ISFLIO			; is BASIC interpreter I/O redirected to I/O channel ?
		jr	z,A1B56			; nope,
		pop	af
		ld	ix,FILOUT		; seq. output
		jp	CALBAS			; CALBAS
;
A1B56:		ld	a,(PRTFLG)
		or	a			; BASIC interpreter output to screen ?
		jr	z,A1BBB			; yep, output to screen
		ld	a,(RAWPRT)
		and	a			; data raw to printer ?
		jr	nz,A1BAB		; yep, to printer
		pop	af

; Subroutine OUTDLP (OUTDO line printer)
OUTDLP:
A1B63:		push	af
		cp	9			; tab ?
		jr	nz,A1B76		; nope,
A1B68:		ld	a,' '
		call	OUTDLP			; OUTDLP
		ld	a,(LPTPOS)
		and	007H
		jr	nz,A1B68		; print spaces
		pop	af
		ret
;
A1B76:		sub	13
		jr	z,A1B84			; cr, LPTPOS = 0
		jr	c,A1B87			; control code, keep position
		cp	20H-13
		jr	c,A1B87			; control code, keep position
		ld	a,(LPTPOS)
		inc	a			; increase LPTPOS
A1B84:		ld	(LPTPOS),a
A1B87:		ld	a,(NTMSXP)
		and	a			; MSX printer ?
		jr	z,A1BAB			; yep, to printer
		pop	af
		call	CNVCHR			; CNVCHR (is grafic header ?)
		ret	nc			; header, quit
		jr	nz,J1BB7		; grafic char, print space

; --------------------------------------
	IF KEYTYP = 0
		AND	A
		JP	P,J1BAC			; plain ascii, print
		CP	86H
		JR	C,J1BB7			; 80H-85H, print space
		CP	0A0H			; 86H-9FH ?
		JR	NC,J1BA4		; nope,
		ADD	A,20H
		JR	J1BAC			; adjust to A6H-BFH and print

J1BA4:		CP	0E0H			; E0H-FFH ?
		JR	C,J1BAC			; nope, print
		SUB	20H			; adjust to C0H-DFH
		DEFB	038H			; JR C,xx a trick to skip the next line (because Cx is always reset at this point)
	  ELSE
		JR	J1BAC
		INCLUDE "local/keyint2.asm"
	  ENDIF
; --------------------------------------

		ALIGN	01BABH

A1BAB:		pop	af
J1BAC:		call	LPTOUT			; LPTOUT
		ret	nc			; no break, quit
		ld	ix,DIOERR		; Device I/O error
		jp	CALBAS			; CALBAS
;
J1BB7:		ld	a,020H
		jr	J1BAC
;
A1BBB:		pop	af
		jp	CHPUT			; CHPUT

; ------------------------------------------------------------------------------
; MSXCHR.MAC
; BIOS character set
; ------------------------------------------------------------------------------

		ALIGN	1BBFH

CGTABL:
T1BBF:		
		INCLUDE "local/chrint.asm"

; ------------------------------------------------------------------------------
; MSXINL.MAC
; BIOS line input functions
; ------------------------------------------------------------------------------

		ALIGN	23BFH

; Subroutine PINLIN
PINLIN:
A23BF:		SYSHOOK	H_PINL
		ld	a,(AUTFLG)
		and	a
		jr	nz,A23D5
		ld	l,0
		jr	A23E0

; Subroutine QINLIN
QINLIN:
A23CC:		SYSHOOK	H_QINL
		ld	a,'?'
		RST	R_OUTDO
		ld	a,' '
		RST	R_OUTDO

; Subroutine INLIN
INLIN:
A23D5:		SYSHOOK	H_INLI
		ld	hl,(CSRY)
		dec	l
		call	nz,TERMIN		; no extend on next line
		inc	l
A23E0:		ld	(FSTPOS),hl
		xor	a
		ld	(INTFLG),a
A23E7:		call	CHGET			; CHGET
		ld	hl,T2439-2
		ld	c,11
		call	INDJMP
		push	af
		call	nz,A23FF
		pop	af
		jr	nc,A23E7
		ld	hl,BUFMIN
		ret	z
		ccf
A23FE:		ret
;
A23FF:		push	af
		cp	009H
		jr	nz,A2413
		pop	af
A2405:		ld	a,020H
		call	A23FF
		ld	a,(CSRX)
		dec	a
		and	007H
		jr	nz,A2405
		ret
;
A2413:		pop	af
		ld	hl,INSFLG
		cp	001H
		jr	z,A2426
		cp	020H
		jr	c,A2428
		push	af
		ld	a,(hl)
		and	a
		call	nz,A24F2
		pop	af
A2426:		RST	R_OUTDO
		ret
;
A2428:		ld	(hl),0
		RST	R_OUTDO
		defb	03EH			; LD A,xx: a trick to skip the next line
A242C:		defb	03EH			; LD A,xx: a trick to skip the next line
A242D:		xor	a
		push	af
		call	CKERCS			; cursor off
		pop	af
		ld	(CSTYLE),a
		jp	CKDPCS			; cursor on
;
T2439:		defb	008H
		defw	A2561
		defb	012H
		defw	A24E5
		defb	01BH
		defw	A23FE
		defb	002H
		defw	A260E
		defb	006H
		defw	A25F8
		defb	00EH
		defw	A25D7
		defb	005H
		defw	A25B9
		defb	003H
		defw	A24C5
		defb	00DH
		defw	A245A
		defb	015H
		defw	A25AE
		defb	07FH
		defw	A2550

A245A:		call	A266C
		ld	a,(AUTFLG)
		and	a
		jr	z,A2465
		ld	h,1
A2465:		push	hl
		call	CKERCS			; cursor off
		pop	hl
		ld	de,BUF
		ld	b,254
		dec	l
A2470:		inc	l
A2471:		push	de
		push	bc
		call	GETVRM			; read char from VRAM
		pop	bc
		pop	de
		and	a
		jr	z,A248F
		cp	020H
		jr	nc,A248A
		dec	b
		jr	z,A249F
		ld	c,a
		ld	a,1
		ld	(de),a
		inc	de
		ld	a,c
		add	a,040H
A248A:		ld	(de),a
		inc	de
		dec	b
		jr	z,A249F
A248F:		inc	h
		ld	a,(LINLEN)
		cp	h
		jr	nc,A2471
		push	de
		call	GETTRM			; extend at next line ?
		pop	de
		ld	h,1
		jr	z,A2470
A249F:		dec	de
		ld	a,(de)
		cp	020H
		jr	z,A249F
		push	hl
		push	de
		call	CKDPCS			; cursor on
		pop	de
		pop	hl
		inc	de
		xor	a
		ld	(de),a
A24AF:		ld	a,00DH
		and	a
A24B2:		push	af
		call	TERMIN			; no extend on next line
		call	POSIT			; POSIT
		ld	a,00AH
		RST	R_OUTDO
		xor	a
		ld	(INSFLG),a
		pop	af
		scf
		pop	hl
		ret
;
A24C4:		inc	l
A24C5:		call	GETTRM			; extends in next line ?
		jr	z,A24C4
		call	A242D
		xor	a
		ld	(BUF),a
		ld	h,1
		push	hl
		call	GICINI			; GICINI
		call	CKSTTP
		pop	hl
		jr	c,A24AF
		ld	a,(BASROM)
		and	a
		jr	nz,A24AF
		jr	A24B2

A24E5:		ld	hl,INSFLG
		ld	a,(hl)
		xor	0FFH
		ld	(hl),a
		jp	z,A242D
		jp	A242C
;
A24F2:		call	CKERCS			; cursor off
		ld	hl,(CSRY)
		ld	c,020H
A24FA:		push	hl
A24FB:		push	bc
		call	GETVRM			; read char from VRAM
		pop	de
		push	bc
		ld	c,e
		call	PUTVRM			; calc VRAM adres
		pop	bc
		ld	a,(LINLEN)
		inc	h
		cp	h
		ld	a,d
		jr	nc,A24FB
		pop	hl
		call	GETTRM			; extend in next line ?
		jr	z,A254B
		ld	a,c
		cp	020H
		push	af
		jr	nz,A2524
		ld	a,(LINLEN)
		cp	h
		jr	z,A2524
		pop	af
		jp	CKDPCS			; cursor on
;
A2524:		call	UNTERM			; extend on next line
		inc	l
		push	bc
		push	hl
		call	GETLEN			; get max. line number
		cp	l
		jr	c,A2535
		call	INSLN0
		jr	A2544
;
A2535:		ld	hl,CSRY
		dec	(hl)
		jr	nz,A253C
		inc	(hl)
A253C:		ld	l,1
		call	DELLN0
		pop	hl
		dec	l
		push	hl
A2544:		pop	hl
		pop	bc
		pop	af
		jp	z,CKDPCS		; cursor on
		dec	l
A254B:		inc	l
		ld	h,1
		jr	A24FA

A2550:		ld	a,(LINLEN)
		cp	h
		jr	nz,A255B
		call	GETTRM			; extend in next line ?
		jr	nz,A2595
A255B:		ld	a,01CH
		RST	R_OUTDO
		ld	hl,(CSRY)
A2561:		push	hl
		call	CKERCS			; cursor off
		pop	hl
		dec	h
		jp	nz,A257A
		inc	h
		push	hl
		dec	l
		jr	z,A2579
		ld	a,(LINLEN)
		ld	h,a
		call	GETTRM			; extend on next line ?
		jr	nz,A2579
		ex	(sp),hl
A2579:		pop	hl
A257A:		ld	(CSRY),hl
A257D:		ld	a,(LINLEN)
		cp	h
		jr	z,A2595
		inc	h
A2584:		call	GETVRM			; read char from VRAM
		dec	h
		call	PUTVRM			; calc VRAM adres
		inc	h
		inc	h
		ld	a,(LINLEN)
		inc	a
		cp	h
		jr	nz,A2584
		dec	h
A2595:		ld	c,020H
		call	PUTVRM			; calc VRAM adres
		call	GETTRM			; extend on next line ?
		jp	nz,CKDPCS		; cursor on
		push	hl
		inc	l
		ld	h,1
		call	GETVRM			; read char from VRAM
		ex	(sp),hl
		call	PUTVRM			; calc VRAM adres
		pop	hl
		jr	A257D

A25AE:		call	CKERCS			; cursor off
		call	A266C
		ld	(CSRY),hl
		jr	A25BE

A25B9:		push	hl
		call	CKERCS			; cursor off
		pop	hl
A25BE:		call	GETTRM			; extend on next line ?
		push	af
		call	EOL			; clear to end of line
		pop	af
		jr	nz,A25CD
		ld	h,1
		inc	l
		jr	A25BE
;
A25CD:		call	CKDPCS			; cursor on
		xor	a
		ld	(INSFLG),a
		jp	A242D

A25D7:		call	CKERCS			; cursor off
		ld	hl,(CSRY)
		dec	l
A25DE:		inc	l
		call	GETTRM			; extend on next line ?
		jr	z,A25DE
		ld	a,(LINLEN)
		ld	h,a
		inc	h
A25E9:		dec	h
		jr	z,A25F3
		call	GETVRM			; read char from VRAM
		cp	020H
		jr	z,A25E9
A25F3:		call	ADVCUR			; cursor right
		jr	A25CD

A25F8:		call	CKERCS			; cursor off
		call	A2634
A25FE:		call	A2624
		jr	z,A25CD
		jr	c,A25FE
A2605:		call	A2624
		jr	z,A25CD
		jr	nc,A2605
		jr	A25CD

A260E:		call	CKERCS			; cursor off
A2611:		call	A2634
		jr	z,A25CD
		jr	nc,A2611
A2618:		call	A2634
		jr	z,A25CD
		jr	c,A2618
		call	ADVCUR			; cursor right
		jr	A25CD
;
A2624:		ld	hl,(CSRY)
		call	ADVCUR			; cursor right
		call	GETLEN			; get max. line number
		ld	e,a
		ld	a,(LINLEN)
		ld	d,a
		jr	A263D
;
A2634:		ld	hl,(CSRY)
	IFDEF MSX1
		call	A0A4C			; cursor left with warp (BS)
	ELSE
		call	C0AA9			; cursor left with warp (BS)
	ENDIF
		ld	de,1*256+1
A263D:		ld	hl,(CSRY)
		RST	R_DCOMPR
		ret	z
		ld	de,A2668
		push	de
		call	GETVRM			; read char from VRAM
		cp	'0'
		ccf
		ret	nc
		cp	'9'+1
		ret	c
		cp	'A'
		ccf
		ret	nc
		cp	'Z'+1
		ret	c
		cp	'a'
		ccf
		ret	nc
		cp	'z'+1
		ret	c
		cp	086H
		ccf
		ret	nc
		cp	0A0H
		ret	c
		cp	0A6H
		ccf
A2668:		ld	a,0
		inc	a
		ret

A266C:		dec	l
		jr	z,A2674
		call	GETTRM			; extend on next line ?
		jr	z,A266C
A2674:		inc	l
		ld	a,(FSTPOS)
		cp	l
		ld	h,1
		ret	nz
		ld	hl,(FSTPOS)
		ret

		ALIGN	2680H			; start of BASIC
