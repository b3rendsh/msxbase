; ------------------------------------------------------------------------------
; msxio.asm
; BIOS MSX I/O function, MSX 1 version (version 1.0)
; ------------------------------------------------------------------------------

		ALIGN	02D7H

CHKRAM:
A02D7:		ld	a,082H
		out	(0ABH),a		; initialize PPI (active, group A mode 0, group A output, upper port C output, group B mode 0, group B input, lower port C output)
		xor	a
		out	(0A8H),a		; select primary slot 0 on all pages
		ld	a,050H
		out	(0AAH),a		; CAPS off, motor off, keyboard row 0
		ld	de,0FFFFH		; initialize lowest RAM address found (page 2)
		xor	a			; start with primary slot 0
		ld	c,a			; initialize expanded slot flags
A02E7:		out	(0A8H),a		; select primary slot on page 2 and 3
		sla	c			; shift expanded slot flags
		ld	b,0			; assume not expanded slot
		ld	hl,0FFFFH
		ld	(hl),0F0H
		ld	a,(hl)
		sub	00FH			; is slot expanded (writen value readback inverted) ?
		jr	nz,A0302		; nope,
		ld	(hl),a
		ld	a,(hl)
		inc	a			; is slot expanded (writen value readback inverted) ?
		jr	nz,A0302		; nope,
		inc	b
		set	0,c			; flag slot expanded
A02FF:		ld	(D_FFFF),a
A0302:		ld	hl,0C000H-256		; page 2
A0305:		ld	a,(hl)
		cpl
		ld	(hl),a
		cp	(hl)			; RAM found (write inverted correctly read back) ?
		cpl
		ld	(hl),a			; restore orginal value
		jr	nz,A0314		; no RAM
		inc	l
		jr	nz,A0305
		dec	h
		jp	m,A0305
A0314:		ld	l,0
		inc	h			; round up 256 bytes boundary
		ld	a,l
		sub	e
		ld	a,h
		sbc	a,d			; page 2 with more RAM ?
		jr	nc,A0327		; nope,
		ex	de,hl			; new lowest RAM address found
		ld	a,(D_FFFF)
		cpl
		ld	l,a
		in	a,(0A8H)
		ld	h,a
		ld	sp,hl			; store primary and secondary slotregister of lowest RAM address found (page 2)
A0327:		ld	a,b
		and	a			; slot expanded ?
		jr	z,A0335			; nope, next primary slot
		ld	a,(D_FFFF)
		cpl
		add	a,010H			; next secondary slot page 2
		cp	040H			; all secondary slots done ?
		jr	c,A02FF			; nope, next
A0335:		in	a,(0A8H)
		add	a,050H			; next primary slot page 2 and 3
		jr	nc,A02E7		; all primary slots done ? nope, next
		ld	hl,0
		add	hl,sp
		ld	a,h
		out	(0A8H),a
		ld	a,l
		ld	(D_FFFF),a		; select slot with lowest RAM address found (page 2)
		ld	a,c
		rlca
		rlca
		rlca
		rlca
		ld	c,a			; expanded slot flags in b7-b4
		ld	de,0FFFFH		; initialize lowest RAM address found (page 3)
		in	a,(0A8H)
		and	03FH			; primary slot 0 in page 3
A0353:		out	(0A8H),a		; select new primary slot in page 3
		ld	b,0
		rlc	c			; slot expanded ?
		jr	nc,A0365		; nope,
		inc	b			; flag slot expanded
		ld	a,(D_FFFF)
		cpl
		and	03FH			; secondary slot 0 in page 3
A0362:		ld	(D_FFFF),a		; select new secondary slot in page 3
A0365:		ld	hl,0FF00H-256		; page 3 (leave upper 256 bytes out for secondary slot register)
A0368:		ld	a,(hl)
		cpl
		ld	(hl),a
		cp	(hl)			; RAM found (write inverted correctly read back) ?
		cpl
		ld	(hl),a			; restore orginal value
		jr	nz,A0379		; no RAM
		inc	l
		jr	nz,A0368
		dec	h
		ld	a,h
		cp	0C0H
		jr	nc,A0368
A0379:		ld	l,0
		inc	h			; round up 256 bytes boundary
		ld	a,l
		sub	e
		ld	a,h
		sbc	a,d			; page 3 with more RAM ?
		jr	nc,A038C		; nope,
		ex	de,hl			; new lowest RAM address found
		ld	a,(D_FFFF)
		cpl
		ld	l,a
		in	a,(0A8H)
		ld	h,a
		ld	sp,hl			; store primary and secondary slotregister of lowest RAM address found (page 3)
A038C:		ld	a,b
		and	a			; slot expanded ?
		jr	z,A0398			; nope,
		ld	a,(D_FFFF)
		cpl
		add	a,040H			; next secondary slot page 3
		jr	nc,A0362		; all secondary slots done ? nope, next
A0398:		in	a,(0A8H)
		add	a,040H			; next primary slot page 3
		jr	nc,A0353		; all primary slots done ? nope, next
		ld	hl,0
		add	hl,sp
		ld	a,h
		out	(0A8H),a
		ld	a,l
		ld	(D_FFFF),a		; select slot with lowest RAM address found (page 3)
		ld	a,c
		ld	bc,0C49H
		ld	de,VARWRK+1
		ld	hl,VARWRK
		ld	(hl),0
		ldir				; clear system variable area
		ld	c,a
		ld	b,4
		ld	hl,EXPTBL+3
A03BD:		rr	c
		sbc	a,a
		and	080H
		ld	(hl),a			; initialize slot expanded flag
		dec	hl
		djnz	A03BD
		in	a,(0A8H)
		ld	c,a			; store current primary slot register
		xor	a
		out	(0A8H),a		; select primary slot 0 in page 0,1,2,3
		ld	a,(D_FFFF)
		cpl
		ld	l,a			; store current secondary slot register slot 0
		ld	a,040H
		out	(0A8H),a		; select primary slot 0 in page 0,1,2 slot 1 in page 3
		ld	a,(D_FFFF)
		cpl
		ld	h,a			; store current secondary slot register slot 1
		ld	a,080H
		out	(0A8H),a		; select primary slot 0 in page 0,1,2 slot 2 in page 3
		ld	a,(D_FFFF)
		cpl
		ld	e,a			; store current secondary slot register slot 2
		ld	a,0C0H
		out	(0A8H),a		; select primary slot 0 in page 0,1,2 slot 3 in page 3
		ld	a,(D_FFFF)
		cpl
		ld	d,a			; store current secondary slot register slot 3
		ld	a,c
		out	(0A8H),a		; restore primary slot register
		ld	(SLTTBL+0),hl
		ex	de,hl
		ld	(SLTTBL+2),hl		; current secondary slot registers saved in SLTTBL
		im	1			; Z80 interrupt mode 1 (RST 38H on INT)
		jp	INIENT			; start BASIC interpreter

; Subroutine ISCNTC (handle CTRL/STOP or STOP pressed)
; Input:  HL = BASIC pointer
ISCNTC:
A03FB:		ld	a,(BASROM)
		and	a			; executing BASIC program in ROM ?
		ret	nz			; yep, quit
		push	hl			; store BASIC pointer
		ld	hl,INTFLG
		di				  ; disable maskable interrupts (make sure INTFLG is not changed)
		ld	a,(hl)
		ld	(hl),0			; reset
		pop	hl			; restore BASIC pointer
		ei				  ; enable maskable interrupts
		and	a			; STOP or CTRL/STOP pressed ?
		ret	z			; nope, quit
		cp	3			; CTRL/STOP pressed ?
		jr	z,A042C			; yep,
		push	hl			; store BASIC pointer
		push	de
		push	bc
		call	A09DA			; cursor on
		ld	hl,INTFLG
A0419:		di				  ; disable maskable interrupts (make sure INTFLG is not changed)
		ld	a,(hl)
		ld	(hl),0			; reset
		ei
		and	a			; STOP or CTRL/STOP pressed ?
		jr	z,A0419			; nope, try again
		push	af			; store
		call	A0A27			; cursor off
		pop	af			; restore
		pop	bc
		pop	de
		pop	hl			; restore BASIC pointer
		cp	3			; CTRL/STOP pressed ?
		ret	nz			; nope, just quit
A042C:		push	hl			; store BASIC pointer
		call	KILBUF			; clear keyboard buffer
		call	A0454			; STOP trap valid ?
		jr	nc,A043F		; nope, terminate BASIC program
		ld	hl,TRPTBL+10*3
		di				  ; disable maskable interrupts (make sure trap flags are not changed)
		call	REQTRP			; raise CTRL-STOP trap
		ei				  ; enable maskable interrupts
		pop	hl			; restore BASIC pointer
		ret

; terminate BASIC program
A043F:		call	TOTEXT			; force text screenmode
		ld	a,(EXPTBL+0)
		ld	h,040H
		call	ENASLT			; ENASLT (enable BASIC ROM in page 1)
		pop	hl			; restore BASIC pointer
		xor	a			; end of statement
		ld	sp,(SAVSTK)		; restore stack pointer
		push	bc			; dummy on stack (removed by STOP)
		jp	CSTOP

; Subroutine STOP trap valid ?
CKSTTP:
A0454:		ld	a,(TRPTBL+10*3+0)
		rrca				; CTRL-STOP trap enabled ?
		ret	nc			; nope, quit
		ld	hl,(TRPTBL+10*3+1)
		ld	a,h
		or	l			; CTRL-STOP trap handler defined ?
		ret	z			; nope, quit
		ld	hl,(CURLIN)
		inc	hl
		ld	a,h
		or	l			; in direct mode ?
		ret	z			; yep, quit
		scf
		ret

; Subroutine KILBUF (clear keyboard buffer)
KILBUF:
A0468:		ld	hl,(PUTPNT)
		ld	(GETPNT),hl
		ret

; Subroutine BREAKX (CTRL-STOP pressed ?)
; Output: Cx set if CTRL-STOP pressed
BREAKX:
A046F:		in	a,(0AAH)
		and	0F0H
		or	007H
		out	(0AAH),a		; select keyboard row 7
		in	a,(0A9H)		; read keyboard row
		and	00010000B		; STOP key pressed ?
		ret	nz			; nope, quit
		in	a,(0AAH)
		dec	a
		out	(0AAH),a		; select keyboard row 6
		in	a,(0A9H)		; read keyboard row
		and	00000010B		; CTRL key pressed ?
		ret	nz			; nope, quit
		push	hl
		ld	hl,(PUTPNT)
		ld	(GETPNT),hl		; clear keyboard buffer
		pop	hl
		ld	a,(OLDKEY+7)
		and	0EFH
		ld	(OLDKEY+7),a		; STOP key was pressed
		LD	A,13
		ld	(REPCNT),a		; reset repeat count
		scf				; CTRL-STOP was pressed
		ret

; Subroutine INITIO (initialize I/O)
INITIO:
A049D:		ld	a,7			; PSG register = 7
		ld	e,10000000B		; IOB = input, IOA = output, noise = 0, tone = 0
		call	WRTPSG			; write PSG register
		ld	a,15			; register = 15 (IOB port)
		ld	e,11001111B
		call	WRTPSG			; write PSG register
		ld	a,11			; register = 11
		ld	e,a
		call	WRTPSG			; write PSG register
		call	INGI			; read PSG IOA port
		and	01000000B
		ld	(KANAMD),a		; store japanese keyboard layout
		ld	a,0FFH
		out	(090H),a		; LPT strobe off

; Subroutine GICINI (initialize General Instruments PSG)
GICINI:
A04BD:		push	hl
		push	de
		push	bc
		push	af
		ld	hl,MUSICF
		ld	b,1+1+3*37
		xor	a
A04C7:		ld	(hl),a
		inc	hl
		djnz	A04C7			; initialize MUSICF, PLYCNT, VCBA, VCBB and VCBC
		ld	de,VOICAQ
		ld	b,128-1			; queue size
		ld	hl,128
A04D3:		push	hl
		push	de
		push	bc
		push	af			; store voice
		call	INITQ			; initialize queue
		pop	af			; restore voice
		add	a,8			; PSG register = volume of voice
		ld	e,0			; volume = 0 (silence)
		call	WRTPSG			; write PSG register
		sub	8
		push	af			; store voice
		ld	l,15			; offset = octave
		call	A1477			; get pointer in voice buffer
		ex	de,hl
		ld	hl,T0508
		ld	bc,6
		ldir				; initialize octave, tone length, tempo, volume and envelope period
		pop	af			; restore voice
		pop	bc
		pop	hl
		pop	de
		add	hl,de
		ex	de,hl
		inc	a
		cp	2+1			; all voice done ?
		jr	c,A04D3			; nope, next voice
		ld	a,7			; register = 7
		ld	e,10111000B		; IOB = input, IOA = output, noise = 7, tone = 0
		call	WRTPSG			; write PSG register
		jp	POPALL			; quit

T0508:		defb	4			; default octave = 4
		defb	4			; default tone length = 4
		defb	120			; default tempo = 120
		defb	8+128			; default volume = 8
		defw	255			; default envelope period = 255

; Subroutine INITXT (initialize screen mode 0)
INITXT:
A050E:		call	DISSCR			; disable screen
		xor	a
		ld	(SCRMOD),a		; screen mode = 0
		ld	(OLDSCR),a		; last text screen mode = 0
		ld	a,(LINL40)
		ld	(LINLEN),a		; update number of columns on row
		ld	hl,(TXTNAM)
		ld	(NAMBAS),hl		; update name table address
		ld	hl,(TXTCGP)
		ld	(CGPBAS),hl		; update pattern table address
		call	CHGCLR			; update colors
		call	A077E			; clear text screen
		call	A071E			; initialize pattern table
		call	SETTXT			; setup VDP for screen mode 0
		jr	ENASCR			; enable screen

; Subroutine INIT32 (initialize screen mode 1)
INIT32:
A0538:		call	DISSCR			; disable screen
		ld	a,1
		ld	(SCRMOD),a		; screen mode = 1
		ld	(OLDSCR),a		; last text screen mode = 1
		ld	a,(LINL32)
		ld	(LINLEN),a		; update number of columns on row
		ld	hl,(T32NAM)
		ld	(NAMBAS),hl		; update name table address
		ld	hl,(T32CGP)
		ld	(CGPBAS),hl		; update pattern table address
		ld	hl,(T32PAT)
		ld	(PATBAS),hl		; update sprite pattern table address
		ld	hl,(T32ATR)
		ld	(ATRBAS),hl		; update sprite attribute table address
		call	CHGCLR			; update colors
		call	A077E			; clear text screen
		call	A071E			; initialize pattern table
		call	A06BB			; move sprites off screen
		call	SETT32			; setup VDP for screen mode 1

; Subroutine ENASCR (enable screen)
ENASCR:
A0570:		ld	a,(RG1SAV)
		or	01000000B		; set enable screen bit
		jr	A057C

;  Subroutine DISSCR (disable screen)
DISSCR:
A0577:		ld	a,(RG1SAV)
		and	10111111B		; clear enable screen bit
A057C:		ld	b,a
		ld	c,1			; register = 1

; Subroutine WRTVDP (write VDP register)
; Input:  B = data
;		C = register
WRTVDP:
A057F:		ld	a,b			; store data
		di				; disable maskable interrupts (make sure vdp command is complete)
		out	(099H),a		; write VDP register data
		ld	a,c			; VDP register
		or	10000000B		; VDP command = write VDP register
		out	(099H),a		; execute VDP command
	IF OPTM = 0
		ei				; enable maskable interrupts (bugfix: interrupts should be enabled after RGSAV update!)
	ENDIF
		push	hl
		ld	a,b
		ld	b,0
		ld	hl,RG0SAV
		add	hl,bc
		ld	(hl),a			; update VDP register value in RGSAV (because VDP register can not be read)
		pop	hl
	IF OPTM != 0
		ei				; enable maskable interrupts
	ENDIF
		ret

; Subroutine SETTXT (setup VDP for screen mode 0)
SETTXT:
A0594:		ld	a,(RG0SAV)
		and	00000001B
		ld	b,a
		ld	c,0			; register = 0
		call	WRTVDP			; write VDP register
		ld	a,(RG1SAV)
		and	11100111B
		or	00010000B
		ld	b,a
		inc	c			; register = 1
		call	WRTVDP			; write VDP register
		ld	hl,TXTNAM		; screen mode 0 address table
		ld	de,0*256+0		; register 3 OR mask = 0, register 4 OR mask = 0
		jp	A0677			; write VDP table addresses from table to registers and quit

; Subroutine SETT32 (setup VDP for screen mode 1)
SETT32:
A05B4:		ld	a,(RG0SAV)
		and	00000001B
		ld	b,a
		ld	c,0			; register = 0
		call	WRTVDP			; write VDP register
		ld	a,(RG1SAV)
		and	11100111B
		ld	b,a
		inc	c			; register = 1
		call	WRTVDP			; write VDP register
		ld	hl,T32NAM		; screen mode 1 address table
		ld	de,0*256+0		; register 3 OR mask = 0, register 4 OR mask = 0
		jp	A0677			; write VDP table addresses from table to registers and quit

; Subroutine INIGRP (initialize screen mode 2)
INIGRP:
A05D2:		call	DISSCR			; disable screen
		ld	a,2
		ld	(SCRMOD),a		; screen mode = 2
		ld	hl,(GRPPAT)
		ld	(PATBAS),hl		; update sprite pattern table address
		ld	hl,(GRPATR)
		ld	(ATRBAS),hl		; update sprite attribute table address
		ld	hl,(GRPNAM)
		call	SETWRT			; setup VDP for VRAM write
		xor	a
		ld	b,3
A05EF:		out	(098H),a
		inc	a
		jr	nz,A05EF
		djnz	A05EF			; write default patterns to VRAM
		call	A07A1			; clear screen mode 2
		call	A06BB			; move sprites off screen
		call	SETGRP			; setup VDP for screen mode 2
		jp	ENASCR			; enable screen

; Subroutine SETGRP (setup VDP for screen mode 2)
SETGRP:
A0602:		ld	a,(RG0SAV)
		or	00000010B
		ld	b,a
		ld	c,0			; register = 0
		call	WRTVDP			; write VDP register
		ld	a,(RG1SAV)
		and	11100111B
		ld	b,a
		inc	c			; register = 1
		call	WRTVDP			; write VDP register
		ld	hl,GRPNAM		; screen mode 2 address table
		ld	de,07FH*256+3		; register 3 OR mask = 01111111B, register 4 OR mask = 00000011B
		jr	A0677			; write VDP table addresses from table to registers

; Subroutine INIMLT (initialize screen mode 3)
INIMLT:
A061F:		call	DISSCR			; disable screen
		ld	a,3
		ld	(SCRMOD),a		; screen mode = 3
		ld	hl,(MLTPAT)
		ld	(PATBAS),hl		; update sprite pattern table address
		ld	hl,(MLTATR)
		ld	(ATRBAS),hl		; update sprite attribute table address
		ld	hl,(MLTNAM)
		call	SETWRT			; setup VDP for VRAM write
		ld	de,0*256+6
A063C:		ld	c,4
A063E:		ld	a,d
		ld	b,32
A0641:		out	(098H),a
		inc	a
		djnz	A0641
		dec	c
		jr	nz,A063E
		ld	d,a
		dec	e
		jr	nz,A063C		; write default patterns to VRAM
		call	A07B9			; clear screen mode 3
		call	A06BB			; move sprites off screen
		call	SETMLT			; setup VDP for screen mode 3
		jp	ENASCR			; enable screen

; Subroutine SETMLT (setup VDP for screen mode 3)
SETMLT:
A0659:		ld	a,(RG0SAV)
		and	00000001B
		ld	b,a
		ld	c,0			; register = 0
		call	WRTVDP			; write VDP register
		ld	a,(RG1SAV)
		and	11100111B
		or	00001000B
		ld	b,a
		ld	c,1			; register = 1
		call	WRTVDP			; write VDP register
		ld	hl,MLTNAM		; screen mode 3 address table
		ld	de,0*256+0		; register 3 OR mask = 0, register 4 OR mask = 0

; Subroutine write VDP table addresses from table to registers
; Input:  HL = pointer to address table
;		D  = register 3 OR mask
;		E  = register 4 OR mask
; Output: HL = updated pointer
;		C  = 7
A0677:		ld	bc,6*256+2		; address shift count = 6, register = 2
		call	A0690			; write address from table to register with OR mask = 0, next register and table entry
		ld	b,10			; address shift count = 10
		ld	a,d			; OR mask register 3
		call	A0691			; write address from table to register with OR mask, next register and table entry
		ld	b,5			; address shift count = 5
		ld	a,e			; OR mask register 4
		call	A0691			; write address from table to register with OR mask, next register and table entry
		ld	b,9			; address shift count = 9
		call	A0690			; write address from table to register with OR mask = 0, next register and table entry
		ld	b,5			; address shift count = 5

; Subroutine write address from table to register with OR mask = 0, next register and table entry
; Input:  HL = pointer to address table
;		C  = register
; Output: HL = updated pointer
;		C  = updated register
;		B  = address right shift count
A0690:		xor	a			; OR mask = 0

; Subroutine write address from table to register with OR mask, next register and table entry
; Input:  A  = OR mask
;		HL = pointer to address table
;		C  = register
;		B  = address right shift count
; Output: HL = updated pointer
;		C  = updated register
A0691:		push	hl			; store pointer
		push	af			; store OR mask
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a			; get address
		xor	a
A0698:		add	hl,hl
		adc	a,a
		djnz	A0698
		ld	l,a
		pop	af			; restore OR mask
		or	l
		ld	b,a
		call	WRTVDP			; write VDP register
		pop	hl			; restore pointer
		inc	hl
		inc	hl			; update pointer to next entry
		inc	c			; next register
		ret

; Subroutine CLRSPR (clear sprites)
CLRSPR:
A06A8:		ld	a,(RG1SAV)
		ld	b,a
		ld	c,1			; register = 1
		call	WRTVDP			; write VDP register
		ld	hl,(PATBAS)		; sprite pattern table address
		ld	bc,8*256
		xor	a			; fill byte
		call	FILVRM			; fill VRAM with byte

; Subroutine move sprites off screen
A06BB:		ld	a,(FORCLR)
		ld	e,a			; store foreground color
		ld	hl,(ATRBAS)		; sprite attribute table address
		ld	bc,32*256+0		; 32 sprites, pattern = 0
A06C5:		ld	a,209			; Y = 209 (off screen)
		call	WRTVRM			; write byte to VRAM
		inc	hl
		inc	hl
		ld	a,c			; sprite pattern number
		call	WRTVRM			; write byte to VRAM
		inc	hl
		inc	c			; update sprite pattern number
		ld	a,(RG1SAV)
		rrca
		rrca				; 16x16 sprites ?
		jr	nc,A06DC		; nope,
		inc	c
		inc	c
		inc	c			; update sprite pattern number
A06DC:		ld	a,e			; restore foreground color
		call	WRTVRM			; write byte to VRAM
		inc	hl
		djnz	A06C5			; next sprite
		ret

; Subroutine CALPAT (calculate sprite pattern address)
; Input:  A  = sprite pattern number
; Output: HL = VRAM address
CALPAT:
A06E4:		ld	l,a
		ld	h,0
		add	hl,hl
		add	hl,hl
		add	hl,hl			; *8
		call	GSPSIZ			; get sprite size
		cp	8			; 8x8 sprites ?
		jr	z,A06F3			; yep,
		add	hl,hl
		add	hl,hl			; *32
A06F3:		ex	de,hl
		ld	hl,(PATBAS)		; sprite pattern table address
		add	hl,de
		ret

; Subroutine CALATR (calculate sprite attribute address)
; Input:  A  = sprite number
; Output: HL = VRAM address
CALATR:
A06F9:		ld	l,a
		ld	h,0
		add	hl,hl
		add	hl,hl
		ex	de,hl
		ld	hl,(ATRBAS)		; sprite attribute table address
		add	hl,de
		ret

; Subroutine GSPSIZ (get sprite size)
; Output: A = sprite size
GSPSIZ:
A0704:		ld	a,(RG1SAV)
		rrca
		rrca
		ld	a,8
		ret	nc
		ld	a,32
		ret

; Subroutine LDIRMV (copy data from VRAM to memory)
; Input:  HL = VRAM address
;		DE = memory address
;		BC = number of bytes

LDIRMV:
A070F:		call	SETRD			; setup VDP for VRAM read
		ex	(sp),hl
		ex	(sp),hl
A0714:		in	a,(098H)		; read byte from VRAM
		ld	(de),a			; write byte to memory
		inc	de
		dec	bc
		ld	a,c
		or	b			; done all bytes ?
		jr	nz,A0714		; nope, next
		ret

; Subroutine initialize pattern table
A071E:		SYSHOOK	H_INIP			; hook initialize pattern table
		ld	hl,(CGPBAS)		; pattern table address
		call	SETWRT			; setup VDP for VRAM write
		ld	a,(CGPNT+0)		; slotid system character set
		ld	hl,(CGPNT+1)		; address system character set
		ld	bc,8*256		; number of bytes
		push	af			; store slotid
A0731:		pop	af			; restore slotid
		push	af			; store slotid
		push	bc			; store number of bytes left
	IF OPTM = 0
		di				; ?? (RDSLT already disables interrupts)
	ENDIF
		call	RDSLT			; read byte from slot (system character set)
		ei				  ; enable maskable interrupts (RDSLT disables maskable interrupts)
		pop	bc			; restore number of bytes left
		out	(098H),a		; write to VRAM
		inc	hl
		dec	bc
		ld	a,c
		or	b			; done all bytes ?
		jr	nz,A0731		; nope, next
		pop	af			; restore slotid
		ret

; Subroutine LDIRVM (copy data from memory to VRAM)
; Input:  HL = memory address
;		DE = VRAM address
;		BC = number of bytes
LDIRVM:
A0744:		ex	de,hl
		call	SETWRT			; setup VDP for VRAM write
A0748:		ld	a,(de)			; read byte from memory
		out	(098H),a		; write byte to VRAM
		inc	de
		dec	bc
		ld	a,c
		or	b			; all bytes done ?
		jr	nz,A0748		; nope, next
		ret

; Subroutine GETPAT (get pattern)
; Input:  A = pattern number
; Output: PATWRK = pattern

GETPAT:
A0752:		ld	h,0
		ld	l,a
		add	hl,hl
		add	hl,hl
		add	hl,hl			; *8
		ex	de,hl
		ld	hl,(CGPNT+1)		; address system character set
		add	hl,de
		ld	de,PATWRK
		ld	b,8
		ld	a,(CGPNT+0)		; slotid system character set
A0765:		push	af			; store slotid
		push	hl			; store pointer in pattern
		push	de			; store pointer in PATWRK
		push	bc			; store counter
		call	RDSLT			; read byte from slot
		ei				; enable interrups (RDSLT disables interrupts)
		pop	bc			; restore counter
		pop	de			; restore pointer in PATWRK
		pop	hl			; restore pointer in pattern
		ld	(de),a			; write byte in PATWRK
		inc	de
		inc	hl
		pop	af			; restore slotid
		djnz	A0765			; next byte
		ret

; Subroutine clear screen
A0777:		call	A0B9F			; graphic screen mode ?
		jr	z,A07A1			; screen mode 2, clear screen mode 2
		jr	nc,A07B9		; screen mode 3, clear screen mode 3

; Subroutine clear text screen
A077E:		ld	a,(SCRMOD)
		and	a			; in screen mode 0 ?
		ld	hl,(NAMBAS)		; name table address
		ld	bc,24*40
		jr	z,A078D			; yep,
		ld	bc,24*32
A078D:		ld	a,' '
		call	FILVRM			; fill VRAM with byte
		call	A0A7F			; cursor home
		ld	hl,LINTTB
		ld	b,24
A079A:		ld	(hl),b
		inc	hl
		djnz	A079A			; all logical lines are not expanded
		jp	FNKSB			; add function key display if enabled

; Subroutine clear screen mode 2
A07A1:		call	A0832			; update color register with border color
		ld	bc,256/8*192
		push	bc			; store number of bytes
		ld	hl,(GRPCOL)		; address color table
		ld	a,(BAKCLR)		; background color
		call	FILVRM			; fill VRAM with byte
		ld	hl,(GRPCGP)		; address pattern table
		pop	bc			; restore number of bytes
		xor	a
A07B6:		jp	FILVRM			; fill VRAM with byte and quit

; Subroutine clear screen mode 3
A07B9:		call	A0832			; update color register with border color
		ld	hl,BAKCLR
		ld	a,(hl)			; background color
		add	a,a
		add	a,a
		add	a,a
		add	a,a			; in high nibble
		or	(hl)			; with background color in low nibble
		ld	hl,(MLTCGP)		; address pattern table
		ld	bc,00600H
		jr	A07B6			; fill VRAM with byte

; Subroutine WRTVRM (write byte to VRAM)
; Input:  HL = VRAM address
;		A  = byte
WRTVRM:
A07CD:		push	af			; store byte
		call	SETWRT			; setup VDP for VRAM write
		ex	(sp),hl
		ex	(sp),hl
		pop	af			; restore byte
		out	(098H),a		; write byte to VRAM
		ret

; Subroutine RDVRM (read byte from VRAM)
; Input: HL = VRAM address
; Output: A = byte
RDVRM:
A07D7:		call	SETRD			; setup VDP for VRAM read
		ex	(sp),hl
		ex	(sp),hl
		in	a,(098H)		; read byte from VRAM
		ret

; Subroutine SETWRT (setup VDP for VRAM write)
; Input:  HL = VRAM address
SETWRT:
A07DF:		ld	a,l
		di				; disable maskable interrupts (make sure VDP VRAM latch setup is not interrupted)
		out	(099H),a
		ld	a,h
		and	00111111B		; ignore b15,b14
		or	01000000B		; VDP command = setup VRAM latch for write
		out	(099H),a
		ei				; enable maskable interrupts
		ret

; Subroutine SETRD (setup VDP for VRAM read)
SETRD:
A07EC:		ld	a,l
		di				; make sure VDP setup is not interrupted
		out	(099H),a
		ld	a,h
		and	00111111B		; ignore b15,b14, VDP command = setup VRAM latch for read
		out	(099H),a
		ei				; enable maskable interrupts
		ret

; Subroutine CHGCLR (update colors)
; Input:  FORCLR = foreground color
;		BAKCLR = background color
;		BDRCLR = border color
CHGCLR:
A07F7:		ld	a,(SCRMOD)
		dec	a			; in screen mode 0 ?
		jp	m,A0824			; yep, update colors for screen mode 0
		push	af			; store screen mode flags
		call	A0832			; update color register with border color
		pop	af			; restore screen mode flags
		ret	nz			; not in screen mode 1, quit
		ld	a,(FORCLR)
		add	a,a
		add	a,a
		add	a,a
		add	a,a			; foreground color in b7-b4
		ld	hl,BAKCLR
		or	(hl)			; background color in b3-b0
		ld	hl,(T32COL)		; address color table
		ld	bc,32

; Subroutine FILVRM (fill VRAM with byte)
; Input:  HL = VRAM address
;		BC = number of bytes
;		A  = byte
FILVRM:
A0815:		push	af			; store byte
		call	SETWRT			; setup VDP for VRAM write
A0819:		pop	af			; restore byte
		out	(098H),a		; write byte to VRAM
		push	af			; store byte
		dec	bc
		ld	a,c
		or	b			; all bytes done ?
		jr	nz,A0819		; nope, next byte
		pop	af			; restore byte
		ret

; update colors for screen mode 0
A0824:		ld	a,(FORCLR)
		add	a,a
		add	a,a
		add	a,a
		add	a,a			; foreground color in b7-b4
		ld	hl,BAKCLR
		or	(hl)
		ld	b,a			; background color in b3-b0
		jr	A0835			; update color register

; Subroutine update color register with border color
A0832:		ld	a,(BDRCLR)

; Subroutine update color register
A0835:		ld	b,a
		ld	c,7			; register = 7
		jp	WRTVDP			; write VDP register

; Subroutine TOTEXT (force text screen mode)
TOTEXT:
A083B:		call	A0B9F			; graphic screen mode ?
		ret	c			; nope, quit
		ld	a,(OLDSCR)		; last text screen mode
		SYSHOOK	H_TOTE			; hook switch to text screen mode
		jp	CHGMOD			; change screen mode

; Subroutine CLS (CLS statement)
CLS:
A0848:		ret	nz			; not end of statement, quit
		push	hl			; store BASIC pointer
		call	A0777			; clear screen
		pop	hl			; restore BASIC pointer
		ret

; Subroutine CHGMOD (change screen mode)
CHGMOD:
A084F:		dec	a			; screen mode 0 ?
		jp	m,INITXT		; yep, initialize screen mode 0
		jp	z,INIT32		; screen mode 1, initialize screen mode 1
		dec	a			; screen mode 2 ?
		jp	z,INIGRP		; yep, initialize screen mode 2
		jp	INIMLT			; initialize screen mode 3

; Subroutine LPTOUT (write byte to printer when ready)
; Input:  A = byte
LPTOUT:
A085D:		SYSHOOK	H_LPTO			; hook write byte to printer
		push	af			; store byte
A0861:		call	A046F			; CTRL-STOP pressed ?
		jr	c,A0878			; yep,
		call	LPTSTT			; get printer status
		jr	z,A0861			; printer is busy, wait
		pop	af			; restore byte

; Subroutine write byte to printer
; Input:  A = byte
A086C:		push	af			; store byte
		out	(091H),a		; write data latch printer
		xor	a
		out	(090H),a		; strobe on
		dec	a
		out	(090H),a		; strobe off
		pop	af			; restore byte
		and	a			; clear Cx (ok)
		ret

A0878:		xor	a
		ld	(LPTPOS),a		; printer position = 0
		ld	a,13			; CR
		call	A086C			; write byte to printer
		pop	af			; restore byte
		scf				; set Cx (aborted)
		ret

; Subroutine LPTSTT (get printer status)
; Output: Zx set if printer is busy
LPTSTT:
A0884:		SYSHOOK	H_LPTS			; hook get printer status
		in	a,(090H)
		rrca
		rrca				; busy bit in Cx
		ccf
		sbc	a,a
		ret

; Subroutine POSIT (change cursor location)
; Input:  H = row, L = column
POSIT:
A088E:		ld	a,27			; ESC
		RST	R_OUTDO
		ld	a,'Y'			; locate cursor
		RST	R_OUTDO
		ld	a,l
		add	a,32-1
		RST	R_OUTDO
		ld	a,h
		add	a,32-1
		RST	R_OUTDO
		ret

; Subroutine CNVCHR (convert graphic character)
CNVCHR:
A089D:		push	hl
		push	af			; store character
		ld	hl,GRPHED
		xor	a
		cp	(hl)			; graphic header flag set ?
		ld	(hl),a			; clear graphic header flag
		jr	z,A08B4			; nope,
		pop	af			; restore character
		sub	040H
		cp	020H			; valid graphic character ?
		jr	c,A08B2			; yep, return graphic character (Zx is reset, Cx is set)
		add	a,040H
A08B0:		cp	a			; set Zx
		scf				; set Cx
A08B2:		pop	hl
		ret

A08B4:		pop	af			; restore character
		cp	1			; graphic header character ?
		jr	nz,A08B0		; nope,
		ld	(hl),a			; set graphic header flag
		pop	hl
		ret				; return (Zx is set, Cx is reset)

; Subroutine CHPUT (character to display)
CHPUT:
A08BC:		push	hl
		push	de
		push	bc
		push	af			; store character
		SYSHOOK	H_CHPU			; hook character to display
		call	A0B9F			; graphic screen mode ?
		jr	nc,POPALL		; yep, quit
		call	CKERCS			; remove cursor when visible
		pop	af			; restore character
		push	af			; store character
		call	A08DF			; display character (with control character handling)
		call	CKDPCS			; display cursor when visible
		ld	a,(CSRX)
		dec	a
		ld	(TTYPOS),a		; update TTYPOS
POPALL:
A08DA:		pop	af
A08DB:		pop	bc
		pop	de
		pop	hl
		ret

; Subroutine display character (with control character handling)
CHPUT1:
A08DF:		call	CNVCHR			; convert graphic character
		ret	nc			; graphic header character, quit
		ld	c,a
		jr	nz,A08F3		; graphic character
		ld	hl,ESCCNT
		ld	a,(hl)
		and	a			; in ESC sequence ?
		jp	nz,A098F		; yep, handle
		ld	a,c
		cp	020H			; control character ?
		jr	c,A0914			; yep, handle
A08F3:		ld	hl,(CSRY)		; cursor position
		cp	07FH			; DEL ?
		jp	z,A0AE3			; yep, remove character from location and quit
		call	PUTVRM			; write character to VRAM
		call	A0A44			; cursor right if possible
		ret	nz			; not at end, quit
	IF OPTM = 0
		xor	a
		call	A0C2B			; expand logical line
	ELSE
		call	UNTERM			; expand logical line
	ENDIF
		ld	h,1			; at begin of line

A0908:		call	A0A61			; cursor down if possible
		ret	nz			; no scroll needed, quit
		call	A0A69			; update cursor position
		ld	l,1			; row = 1
		jp	A0A88			; clear line

A0914:		ld	hl,T092F-2		; control code function table
		ld	c,12			; number of entries control code function table

; Subroutine execute function
INDJMP:
A0919:		inc	hl
		inc	hl
		and	a
		dec	c
		ret	m
		cp	(hl)
		inc	hl
		jr	nz,A0919
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
		ld	hl,(CSRY)		; cursor position
		call	A092D			; execute function
		xor	a
		ret

JMPBC:
A092D:		push	bc
		ret

T092F:		defb	007H
		defw	BEEP			; BEEP
		defb	008H
		defw	A0A4C			; BS, cursor left with warp
		defb	009H
		defw	A0A71			; TAB, to next tab
		defb	00AH
		defw	A0908			; LF, new line
		defb	00BH
		defw	A0A7F			; HOME, cursor home
		defb	00CH
		defw	A077E			; CLS, clear text screen
		defb	00DH
		defw	A0A81			; CR, cursor to start of line
		defb	01BH
		defw	A0989			; ESC, start ESC sequence
		defb	01CH
		defw	A0A5B			; CURSOR RIGHT, cursor right
		defb	01DH
		defw	A0A4C			; CURSOR LEFT, cursor left with warp
		defb	01EH
		defw	A0A57			; CURSOR UP, cursor up if possible
		defb	01FH
		defw	A0A61			; CURSOR DOWN, cursor down if possible

T0953:		defb	'j'
		defw	A077E			; clear text screen
		defb	'E'
		defw	A077E			; clear text screen
		defb	'K'
		defw	A0AEE			; clear to end of line
		defb	'J'
		defw	A0B05			; clear to end of screen
		defb	'l'
		defw	A0AEC			; clear line
		defb	'L'
		defw	A0AB4			; insert line
		defb	'M'
		defw	A0A85			; delete line
		defb	'Y'
		defw	A0986			; locate cursor
		defb	'A'
		defw	A0A57			; cursor up if possible
		defb	'B'
		defw	A0A61			; cursor down if possible
		defb	'C'
		defw	A0A44			; cursor right if possible
		defb	'D'
		defw	A0A55			; cursor left if possible
		defb	'H'
		defw	A0A7F			; cursor home
		defb	'x'
		defw	A0980
		defb	'y'
		defw	A0983

; ESC x sequence
A0980:		ld	a,1			; ESC flag = ESC x
		defb	001H			; LD BC,xxxx (trick to skip the next instruction)

; ESC y sequence
A0983:		ld	a,2			; ESC flag = ESC y
		defb	001H			; LD BC,xxxx (trick to skip the next instruction)

; ESC Y sequence
A0986:		ld	a,4			; ESC flag = ESC Y (first parameter)
		defb	001H			; LD BC,xxxx (trick to skip the next instruction)

; start of ESC sequence
A0989:		ld	a,0FFH			; ESC flag = start of ESC sequence
		ld	(ESCCNT),a
		ret

; in ESC sequence
A098F:		jp	p,A099D			; handle ESC sequences with parameter(s)
		ld	(hl),0			; clear ESC flag
		ld	a,c			; restore character
		ld	hl,T0953-2		; ESC function table
		ld	c,15			; number of entries in ESC function table
		jp	INDJMP			; execute function

A099D:		dec	a			; ESC x ?
		jr	z,A09BE			; yep,
		dec	a			; ESC y ?
		jr	z,A09C8			; yep,
		dec	a			; ESC Y (second parameter) ?
		ld	(hl),a			; update ESC flag
		ld	a,(LINLEN)		; number of columns on row
		ld	de,CSRX
		jr	z,A09B3			; yep, validate and update location row

		; ESC Y (first parameter)
		ld	(hl),3			; ESC flag = ESC Y (second parameter)
		call	GETLEN			; get number of usable screen lines
		dec	de			; CSRY

; validate and update location
A09B3:		ld	b,a			; store maximum
		ld	a,c			; restore parameter character
		sub	32			; 0 based
		cp	b			; valid parameter ?
		inc	a			; 1 based
		ld	(de),a			; update location
		ret	c			; yep, quit
		ld	a,b
		ld	(de),a			; update location with maximum
		ret

; ESC x
A09BE:		ld	(hl),a			; clear ESC flag
		ld	a,c
		sub	'4'			; ESC x4 ?
		jr	z,A09CF			; yep, cursor = block cursor
		dec	a			; ESC x5 ?
		jr	z,A09D6			; yep, cursor = invisible
		ret				; nope, quit (ignored)

; ESC y
A09C8:		ld	(hl),a			; clear ESC flag
		ld	a,c			; restore character
		sub	'4'			; ESC y4 ?
		jr	nz,A09D3		; nope,
		inc	a			; cursor style = stripe cursor
A09CF:		ld	(CSTYLE),a
		ret

A09D3:		dec	a			; ESC y5 ?
		ret	nz			; nope, quit (ignored)
		inc	a			; yep, cursor = visible
A09D6:		ld	(CSRSR),a
		ret

; Subroutine cursor on
A09DA:		ld	a,(CSRSR)
		and	a			; cursor already visible ?
		ret	nz			; yep, quit
		jr	A09E6			; display cursor

; Subroutine display cursor when visible
CKDPCS:
A09E1:		ld	a,(CSRSR)
		and	a			; cursor visible ?
		ret	z			; nope, quit

; Subroutine display cursor
A09E6:		SYSHOOK	H_DSPC			; hook display cursor
		call	A0B9F			; graphic screen mode ?
		ret	nc			; yep, quit
		ld	hl,(CSRY)
		push	hl			; store location
		call	GETVRM			; read character from VRAM
		ld	(CURSAV),a		; store orginal character
		ld	l,a
		ld	h,0
		add	hl,hl
		add	hl,hl
		add	hl,hl
		ex	de,hl
		ld	hl,(CGPBAS)		; pattern table address
		push	hl			; store pattern table address
		add	hl,de
		call	A0BA5			; copy pattern from VRAM in LINWRK
		ld	hl,LINWRK+7
		ld	b,8			; 8 lines
		ld	a,(CSTYLE)
		and	a			; block cursor ?
		jr	z,A0A13			; yep,
		ld	b,3			; 3 lines
A0A13:		ld	a,(hl)
		cpl
		ld	(hl),a			; invert line
		dec	hl
		djnz	A0A13			; next line
		pop	hl			; restore pattern table address
		ld	bc,255*8
		add	hl,bc			; address cursor pattern
		call	A0BBE			; copy pattern in LINWRK to VRAM
		pop	hl			; restore location
		ld	c,0FFH			; cursor character
		jp	PUTVRM			; write character to VRAM

; Subroutine cursor off
A0A27:		ld	a,(CSRSR)
		and	a			; cursor visible ?
		ret	nz			; yep, quit
		jr	A0A33			; remove cursor

; Subroutine remove cursor when visible
CKERCS:
A0A2E:		ld	a,(CSRSR)
		and	a			; cursor visible ?
		ret	z			; nope, quit

; Subroutine remove cursor
A0A33:		SYSHOOK	H_ERAC			; hook erase cursor
		call	A0B9F			; graphic screen mode ?
		ret	nc			; yep, quit
		ld	hl,(CSRY)
		ld	a,(CURSAV)
		ld	c,a			; restore orginal character
		jp	PUTVRM			; write character to VRAM

; Subroutine cursor right if possible
A0A44:		ld	a,(LINLEN)		; number of columns on row
		cp	h			; at end of row ?
		ret	z			; yep, quit
		inc	h
		jr	A0A69			; update cursor position and quit

; Subroutine cursor left with warp
; BS (note: duplicate label)
A0A4C:		call	A0A55			; cursor left if possible
		ret	nz
		ld	a,(LINLEN)
		ld	h,a			; column = number of columns on row
		defb	011H			; LD DE,xx: a trick to skip the next 2 lines
A0A55:		dec	h
		defb	03EH			; LD A,xx: a trick to skip the next line

; Subroutine cursor up if possible
A0A57:		dec	l
		ret	z
		jr	A0A69			; update cursor position and quit

; Subroutine cursor right
ADVCUR:
A0A5B:		call	A0A44			; cursor right if possible
		ret	nz
		ld	h,1

; Subroutine cursor down if possible
A0A61:		call	GETLEN			; get number of usable screen lines
		cp	l			; last line number ?
		ret	z			; yep, quit
		jr	c,A0A6D
		inc	l
A0A69:		ld	(CSRY),hl
		ret

A0A6D:		dec	l
		xor	a
		jr	A0A69			; update cursor position and quit

; Subroutine to next tab
A0A71:		ld	a,' '			; space
		call	A08DF			; display character (with control character handling)
		ld	a,(CSRX)
		dec	a
		and	007H			; reached tab ?
		jr	nz,A0A71		; nope, continue
		ret

; Subroutine cursor home
A0A7F:		ld	l,1			; row = 1

; Subroutine cursor to start of line
; Input:  L = row
A0A81:		ld	h,1			; column = 1
		jr	A0A69			; update cursor position and quit

; Subroutine delete line
; Input:  L = row
A0A85:		call	A0A81			; cursor to start of line

; Subroutine delete line from location
; Input:  H = column, L = row
DELLN0:
A0A88:		call	GETLEN			; get number of usable screen lines
		sub	l			; valid line number ?
		ret	c			; nope, quit
		jp	z,A0AEC			; clear line and quit
		push	hl			; store location
		push	af
		ld	c,a
		ld	b,0
		call	GETTRM			; get pointer to logical line expand flag
		ld	l,e
		ld	h,d
		inc	hl
		ldir
		ld	hl,FSTPOS
		dec	(hl)
		pop	af
		pop	hl
A0AA3:		push	af
		inc	l
		call	A0BAA			; copy row from VRAM in LINWRK
		dec	l
		call	A0BC3			; copy row in LINWRK to VRAM
		inc	l
		pop	af
		dec	a
		jr	nz,A0AA3
		jp	A0AEC			; clear line

; Subroutine insert line
; Input:  L = row
A0AB4:		call	A0A81			; cursor to start of line

; Subroutine insert line from location
; Input:  L = row
INSLN0:
A0AB7:		call	GETLEN			; get number of usable screen lines
		ld	h,a			; store number of usable screen lines
		sub	l			; valid line number ?
		ret	c			; nope, quit
		jp	z,A0AEC			; last line, clear line and quit
		ld	l,h			; restore number of usable screen lines
		push	hl
		push	af
		ld	c,a
		ld	b,0
		call	GETTRM			; get pointer to logical line expand flag
		ld	l,e
		ld	h,d
		push	hl
		dec	hl
		lddr
		pop	hl
		ld	(hl),h
		pop	af
		pop	hl
A0AD3:		push	af
		dec	l
		call	A0BAA			; copy row from VRAM in LINWRK
		inc	l
		call	A0BC3			; copy row in LINWRK to VRAM
		dec	l
		pop	af
		dec	a
		jr	nz,A0AD3
		jr	A0AEC			; clear line

; Subroutine remove character from location
A0AE3:		call	A0A4C			; cursor left with warp
		ret	z
		ld	c,' '			; space
		jp	PUTVRM			; write character to VRAM

; Subroutine clear line
; Input:  L = row
A0AEC:		ld	h,1			; column = 1

; Subroutine clear to end of line
; Input:  H = column
;		L = row
EOL:
A0AEE:		call	TERMIN			; terminate logical line
		push	hl			; store location
		call	VADDR			; convert location to VRAM address
		call	SETWRT			; setup VDP for VRAM write
		pop	hl			; restore location
A0AF9:		ld	a,' '
		out	(098H),a
		inc	h			; next column
		ld	a,(LINLEN)
		cp	h			; end of row ?
		jr	nc,A0AF9		; nope, next
		ret

; Subroutine clear to end of screen
; Input:  H = column, L = row
A0B05:		push	hl			; store location
		call	A0AEE			; clear line from location
		pop	hl			; restore location
		call	GETLEN			; number of usable screen lines
		cp	l			; valid line number ?
		ret	c			; nope, quit
		ret	z			; last line, quit
		ld	h,1			; column = 1
		inc	l			; next line
		jr	A0B05			; next

; Subroutine ERAFNK (remove function key display)
ERAFNK:
A0B15:		SYSHOOK	H_ERAF			; hook remove function key display
		xor	a			; function key display = off
		call	A0B9C			; update function key display flag, graphic screen mode ?
		ret	nc			; yep, quit
		push	hl
		ld	hl,(CRTCNT)		; last line of screen
		call	A0AEC			; clear line
		pop	hl
		ret

; Subroutine FNKSB (add function key display if enabled)
FNKSB:
A0B26:		ld	a,(CNSDFG)
		and	a			; function key display enabled ?
		ret	z			; nope, quit

; Subroutine DSPFNK (add function key display)
DSPFNK:
A0B2B:		SYSHOOK	H_DSPF			; hook add function key display
		ld	a,0FFH			; function key display = on
		call	A0B9C			; update function key display flag, graphic screen mode ?
		ret	nc			; yep, quit
		push	hl
		ld	a,(CSRY)		; current row
		ld	hl,CRTCNT
		cp	(hl)			; at last row of screen ?
		ld	a,10			; LF
		jr	nz,A0B41		; nope,
		RST	R_OUTDO
A0B41:		ld	a,(NEWKEY+6)
		rrca				; SHIFT key status
		ld	hl,FNKSTR+0*16
		ld	a,1			; normal function key display
		jr	c,A0B50			; SHIFT not pressed,
		ld	hl,FNKSTR+5*16
		xor	a			; SHIFT function key display
A0B50:		ld	(FNKSWI),a		; update function key switch flag
		ld	de,LINWRK
		push	de			; store pointer to LINWRK
		ld	b,40
		ld	a,' '
A0B5B:		ld	(de),a
		inc	de
		djnz	A0B5B			; empty function key row
		pop	de			; restore pointer to LINWRK
		ld	c,5			; 5 function keys
		ld	a,(LINLEN)		; number of columns on row
		sub	4			; less then 4 columns ?
		jr	c,A0B94			; empty function key display
		ld	b,-1
A0B6B:		inc	b
		sub	5
		jr	nc,A0B6B		; calculate number of characters per function key to display
		ld	a,b
		and	a			; no characters to display ?
		jr	z,A0B94			; yep, empty function key display
		defb	03EH			; LD A,xx: a trick to skip the next line
A0B75:		inc	de
		push	bc			; store number of characters, number of function keys
		ld	c,0			; number of characters = 0
A0B79:		ld	a,(hl)
		inc	hl
		inc	c
		call	CNVCHR			; convert graphic character
		jr	nc,A0B79		; graphic header character, next character
		jr	nz,A0B87		; graphic character,
		cp	020H			; valid character ?
		jr	c,A0B88			; nope, skip character
A0B87:		ld	(de),a
A0B88:		inc	de
		djnz	A0B79			; next character
		ld	a,16
		sub	c
		ld	c,a
		add	hl,bc			; update pointer to next function key definition
		pop	bc			; restore number of characters, number of function keys
		dec	c
		jr	nz,A0B75		; next function key
A0B94:		ld	hl,(CRTCNT)		; row = last row of screen
		call	A0BC3			; copy row in LINWRK to VRAM
		pop	hl
		ret

; Subroutine update function key display flag, graphic screen mode ?
; Output: Cx set if text screen mode
SETCHK:
A0B9C:		ld	(CNSDFG),a		; update function key display flag

; Subroutine graphic screen mode ?
; Output: Cx set if text screen mode
CHKSCR:
A0B9F:		ld	a,(SCRMOD)
		cp	2			; in graphic screen mode ?
		ret

; Subroutine copy pattern from VRAM in LINWRK
A0BA5:		push	hl
		ld	c,8			; number of columns = 8
		jr	A0BB4

; Subroutine copy row from VRAM in LINWRK
A0BAA:		push	hl			; store location
		ld	h,1			; column = 1
		call	VADDR			; convert location to VRAM address
		ld	a,(LINLEN)
		ld	c,a			; number of columns = number of columns on row
A0BB4:		ld	b,0
		ld	de,LINWRK
		call	LDIRMV			; copy data from VRAM to memory
		pop	hl			; restore location
		ret

; Subroutine copy pattern in LINWRK to VRAM
A0BBE:		push	hl
		ld	c,8
		jr	A0BCD

; Subroutine copy row in LINWRK to VRAM
A0BC3:		push	hl			; store location
		ld	h,1			; column = 1
		call	VADDR			; convert location to VRAM address
		ld	a,(LINLEN)
		ld	c,a			; number of columns = number of columns on row
A0BCD:		ld	b,0
		ex	de,hl
		ld	hl,LINWRK
		call	LDIRVM			; copy data from memory to VRAM
		pop	hl			; restore location
		ret

; Subroutine read character from VRAM
; Input:  H = column
;		L = row
; Output: C = character
GETVRM:
A0BD8:		push	hl			; store location
		call	VADDR			; convert location to VRAM address
		call	SETRD			; setup VDP for VRAM read
		ex	(sp),hl
		ex	(sp),hl
		in	a,(098H)		; read byte from VRAM
		ld	c,a
		pop	hl			; restore location
		ret

; Subroutine write character to VRAM
; Input:  C = character, H = column, L = row
PUTVRM:
A0BE6:		push	hl			; store location
		call	VADDR			; convert location to VRAM address
		call	SETWRT			; setup VDP for VRAM write
		ld	a,c
		out	(098H),a		; write byte to VRAM
		pop	hl			; restore location
		ret

; Subroutine convert location to VRAM address
; Input:  H  = column (1 based)
;		L  = row (1 based)
; Output: HL = VRAM address
VADDR:
A0BF2:		push	bc
		ld	e,h
		ld	h,0
		ld	d,h
		dec	l
		add	hl,hl
		add	hl,hl
		add	hl,hl			; *8
		ld	c,l
		ld	b,h
		add	hl,hl
		add	hl,hl			; *32
		add	hl,de
		ld	a,(SCRMOD)
		and	a			; in screen mode 0 ?
		ld	a,(LINLEN)		; number of columns on row
		jr	z,A0C0D			; yep,
		sub	32+1+1
		jr	A0C10

A0C0D:		add	hl,bc
		sub	40+1+1
A0C10:		cpl
		and	a			; clear Cx
		rra				; /2
		ld	e,a
		add	hl,de
		ex	de,hl
		ld	hl,(NAMBAS)		; name table address
		add	hl,de
		dec	hl
		pop	bc
		ret

; Subroutine get pointer to logical line expand flag
GETTRM:
A0C1D:		push	hl
		ld	de,LINTTB-1
		ld	h,0
		add	hl,de
		ld	a,(hl)
		ex	de,hl
		pop	hl
		and	a			; is logical line expanded ?
		ret

; Subroutine terminate logical line
TERMIN:
A0C29:		defb	03EH			; LD A,xx: a trick to skip the next line

; Subroutine expand logical line
UNTERM:
A0C2A:		xor	a			; flag = expanded
A0C2B:		push	af
		call	GETTRM			; get pointer to logical line expand flag
		pop	af
		ld	(de),a			; update logical line expand flag
		ret

; Subroutine get number of usable screen lines
GETLEN:
A0C32:		ld	a,(CNSDFG)		; function key display flag
		push	hl
		ld	hl,CRTCNT
		add	a,(hl)			; number of screen rows, 1 less if function key display is on
		pop	hl
		ret

; Subroutine KEYINT (interrupt handler)
KEYINT:
A0C3C:		push	hl
		push	de
		push	bc
		push	af
		exx
		ex	af,af'
		push	hl
		push	de
		push	bc
		push	af
		push	iy
		push	ix
		call	H_KEYI			; hook maskable interrupt
		in	a,(099H)		; read VDP status register (clears pending VDP interrupt flag)
		and	a			; VDP interrupt ?
		jp	p,A0D02			; nope, quit KEYINT
		call	H_TIMI			; hook VDP maskable interrupt
		ei
		ld	(STATFL),a		; store VDP status register
		and	00100000B		; sprite collision flag set ?
		ld	hl,TRPTBL+11*3
		call	nz,REQTRP		; yep, raise SPRITE trap
		ld	hl,(INTCNT)
		dec	hl
		ld	a,h
		or	l			; interval couter finished ?
		jr	nz,A0C73		; nope, skip
		ld	hl,TRPTBL+17*3
		call	REQTRP			; raise INTERVAL trap
		ld	hl,(INTVAL)		; reload INTCNT
A0C73:		ld	(INTCNT),hl
		ld	hl,(JIFFY)
		inc	hl
		ld	(JIFFY),hl		; update time counter
		ld	a,(MUSICF)
		ld	c,a			; voice active flags
		xor	a			; voice = 0
A0C82:		rr	c			; voice active ?
		push	af
		push	bc
		call	c,A113B			; yep, timer countdown, decode new packet from queue if needed
		pop	bc
		pop	af
		inc	a
		cp	2+1			; done all voices ?
		jr	c,A0C82			; nope, next voice
		ld	hl,SCNCNT
		dec	(hl)			; do i have to scan keyboard ?
		jr	nz,A0D02		; nope, quit KEYINT
	IF KEYBFIX = 1
		ld	(hl),1			; next keyboard scan after 1 ints
	ELSE
		ld	(hl),3			; next keyboard scan after 3 ints
	ENDIF
		xor	a			; joystick = 0
		call	A120C			; read joystick port
		and	00110000B		; only firebuttons
		push	af
		ld	a,1			; joystick = 1
		call	A120C			; read joystick port
		and	00110000B		; only firebuttons
		rlca
		rlca				; to b7,b6
		pop	bc
		or	b
		push	af
		call	A1226			; read keyboard row 8
		and	00000001B		; only spacebar
		pop	bc
		or	b
		ld	c,a
		ld	hl,TRGFLG
		xor	(hl)
		and	(hl)
		ld	(hl),c			; store triggerflag
		ld	c,a
		rrca				; spacebar being pressed ?
		ld	hl,TRPTBL+12*3
		call	c,REQTRP		; yep, raise STRIG0 trap
		rl	c			; firebutton 2 B being pressed ?
		ld	hl,TRPTBL+16*3
		call	c,REQTRP		; yep, raise STRIG4 trap
		rl	c
		ld	hl,TRPTBL+14*3
		call	c,REQTRP		; yep, raise STRIG2 trap
		rl	c
		ld	hl,TRPTBL+15*3
		call	c,REQTRP		; yep, raise STRIG3 trap
		rl	c
		ld	hl,TRPTBL+13*3
		call	c,REQTRP		; yep, raise STRIG1 trap
		xor	a
		ld	(CLIKFL),a		; reset key click flag
		call	A0D12			; scan keyboard
		jr	nz,A0D02		; buffer not empty, quit KEYINT
		ld	hl,REPCNT
		dec	(hl)			; time for a key repeat ?
		jr	nz,A0D02		; nope, quit
	IF KEYBFIX = 1
		ld	(hl),3
	ELSE
		ld	(hl),1
	ENDIF
		ld	hl,OLDKEY
		ld	de,OLDKEY+1
		ld	bc,11-1
		ld	(hl),0FFH
		ldir				; create a being pressed
		call	A0D4E			; handle pressed keys
A0D02:		pop	ix
		pop	iy
		pop	af
		pop	bc
		pop	de
		pop	hl
		ex	af,af'
		exx
		pop	af
		pop	bc
		pop	de
		pop	hl
		ei
		ret

; Subroutine scan keyboard
KEYCHK:
A0D12:		in	a,(0AAH)
		and	0F0H
		ld	c,a
		ld	b,11
		ld	hl,NEWKEY
A0D1C:		ld	a,c
		out	(0AAH),a
		in	a,(0A9H)
		ld	(hl),a
		inc	c
		inc	hl
		djnz	A0D1C			; scan keyboard & put in NEWKEY
		ld	a,(ENSTOP)
		and	a			; Hot break possible ?
		jr	z,A0D3A			; nope, quit
		ld	a,(NEWKEY+6)
		cp	11101000b		; CODE+GRAPH+CTRL+SHIFT pressed ?
		jr	nz,A0D3A		; nope, quit
		ld	ix,READYR		; start headloop
		jp	CALBAS			; call BASIC routine

NOSTOP:
A0D3A:		ld	de,OLDKEY+11
		ld	b,11			; 11 keyboard rows
A0D3F:		dec	de
		dec	hl
		ld	a,(de)
		cp	(hl)			; changed ?
		jr	nz,A0D49		; yep, reset repeat count
		djnz	A0D3F
		jr	A0D4E			; skip reset repeat count

A0D49:
	IF KEYBFIX = 1
		ld	a,39
	ELSE
		ld	a,13
	ENDIF
		ld	(REPCNT),a		; reset repeat count


; Subroutine handle pressed keys
KEYCK4:
A0D4E:		ld	b,11			; 11 keyboard rows
		ld	hl,OLDKEY
		ld	de,NEWKEY
A0D56:		ld	a,(de)
		ld	c,a
		xor	(hl)
		and	(hl)			; key being pressed ?
		ld	(hl),c			; OLDKEY renewed
		call	nz,A0D89		; yep, handle key pressed
		inc	de
		inc	hl
		djnz	A0D56			; next row
A0D62:		ld	hl,(GETPNT)
		ld	a,(PUTPNT)
		sub	l			; keyboard buffer empty ?
		ret

; Subroutine CHSNS (check if key)
CHSNS:
A0D6A:		ei
		push	hl
		push	de
		push	bc
		call	A0B9F			; graphic screen mode ?
		jr	nc,A0D82		; yep, skip functionkeys
		ld	a,(FNKSWI)		; last function key display flag
		ld	hl,NEWKEY+6
		xor	(hl)			; SHIFT changed ?
		ld	hl,CNSDFG
		and	(hl)			; and display functionkeys ?
		rrca
		call	c,DSPFNK		; yep, add function key display
A0D82:		call	A0D62			; keyboard buffer empty ?
		pop	bc
		pop	de
		pop	hl
		ret

; Subroutine handle key pressed
; Input:  A = key row data
KEYANY:
A0D89:		push	hl
		push	de
		push	bc
		push	af			; store key row data
		ld	a,11
		sub	b			; row
		add	a,a
		add	a,a
		add	a,a			; *8
		ld	c,a			; base key code
		ld	b,8			; 8 bits
		pop	af			; restore key row data
A0D97:		rra				; this key being pressed ?
		push	bc			; store counter, key code
		push	af			; store key row data
		call	c,KEYCOD		; yep, handle key
		pop	af			; restore key row data
		pop	bc			; restore counter, key code
		inc	c
		djnz	A0D97			; next key in row
		jp	A08DB			; quit

		INCLUDE "local/keyint1.asm"

		ALIGN	010C2H

; Subroutine increase pointer in keyboard buffer
UPDATE:
C10C2:		inc	hl
		ld	a,l
KEYLOW		EQU	(KEYBUF + 40) % 256
		cp	KEYLOW
		ret	nz
		ld	hl,KEYBUF
		ret

; Subroutine CHGET (get key)
CHGET:
A10CB:		push	hl
		push	de
		push	bc
		SYSHOOK	H_CHGE			; hook get key
		call	CHSNS			; check if key
		jr	nz,A10E1		; yep,
		call	A09DA			; cursor on
A10D9:		call	CHSNS			; check if key
		jr	z,A10D9			; nope, wait
		call	A0A27			; cursor off
A10E1:		ld	hl,INTFLG
		ld	a,(hl)
		cp	4			; STOP key pressed ?
		jr	nz,A10EB		; nope,
		ld	(hl),0			; reset
A10EB:		ld	hl,(GETPNT)
		ld	c,(hl)			; get key from keyboard buffer
		call	C10C2			; increase pointer in keyboard buffer
		ld	(GETPNT),hl		; update keyboard get pointer
		ld	a,c			; key
		jp	A08DB			; quit

; Subroutine CKCNTC (handle CTRL/STOP or STOP pressed, no resume)
CKCNTC:
A10F9:		push	hl			; store BASIC pointer
		ld	hl,0			; BASIC pointer = 0 (no resume when BASIC program is aborted)
		call	ISCNTC			; handle CTRL/STOP or STOP pressed
		pop	hl			; restore BASIC pointer
		ret

; Subroutine WRTPSG (write PSG register)
WRTPSG:
A1102:		di				; disable maskable interrupts (make sure PSG register does not change)
		out	(0A0H),a
		push	af
		ld	a,e
		out	(0A1H),a
		ei				; enable maskable interrupts
		pop	af
		ret

; Subroutine read PSG IOA port
INGI:
A110C:		ld	a,14

; Subroutine RDPSG (read PSG register)
RDPSG:
A110E:		out	(0A0H),a
		in	a,(0A2H)
		ret

; Subroutine BEEP
BEEP:
A1113:		xor	a			; register = 0
		ld	e,055H
		call	WRTPSG			; write PSG register
		ld	e,a
		inc	a			; register = 1
		call	WRTPSG			; write PSG register
		ld	e,0BEH
		ld	a,7			; register = 7
		call	WRTPSG			; write PSG register
		ld	e,a
		inc	a			; register = 8
		call	WRTPSG			; write PSG register
		ld	bc,2000
		call	A1133			; wait
		jp	A04BD			; GICINI

; Subroutine wait
CSDLY1:
A1133:		dec	bc
		ex	(sp),hl
		ex	(sp),hl
		ld	a,b
		or	c
		jr	nz,A1133
		ret

; Subroutine timer countdown, decode new packet from queue if needed
ACTION:
A113B:		ld	b,a			; store voice
		call	GETVCP			; get pointer to macro string info voice buffer
		dec	hl
		ld	d,(hl)
		dec	hl
		ld	e,(hl)			; timer
		dec	de
		ld	(hl),e
		inc	hl
		ld	(hl),d			; decrease timer
		ld	a,d
		or	e			; timer expires ?
		ret	nz			; nope, quit
		ld	a,b			; restore voice
		ld	(QUEUEN),a		; current queue = voice
		call	A11E2			; get byte from current queue
		cp	0FFH			; end of queue mark ?
		jr	z,A11B0			; yep,
		ld	d,a			; store queue byte
		and	0E0H
		rlca
		rlca
		rlca
		ld	c,a			; size of packet
		ld	a,d			; restore queue byte
		and	01FH
		ld	(hl),a			; MSB of duration in b4-b0
		call	A11E2			; get byte from current queue
		dec	hl
		ld	(hl),a			; LSB of duration
		inc	c
A1166:		dec	c			; packet ends ?
		ret	z			; yep, quit
		call	A11E2			; get byte from current queue
		ld	d,a			; store packet byte
		and	0C0H			; packet flags in b7-b6
		jr	nz,A1181		; not a frequency packet
		call	A11E2			; get byte from current queue
		ld	e,a			; low byte frequency
		ld	a,b			; restore voice
		rlca				; frequency register
		call	WRTPSG			; write PSG register
		inc	a
		ld	e,d			; high byte frequency
		call	WRTPSG			; write PSG register
		dec	c			; update packet size left
		jr	A1166			; next

A1181:		ld	h,a			; store packet flags
		and	080H			; volume/shape flag ?
		jr	z,A1195			; nope, skip volume
		ld	e,d			; restore packet byte
		ld	a,b			; restore voice
		add	a,8			; amplitude register
		call	WRTPSG			; write PSG register
		ld	a,e
		and	010H			; shape ?
		ld	a,13
		call	nz,WRTPSG		; yep, write PSG register
A1195:		ld	a,h			; restore packet flags
		and	040H			; envelope flag ?
		jr	z,A1166			; nope, skip envelope
		call	A11E2			; get byte from current queue
		ld	d,a
		call	A11E2			; get byte from current queue
		ld	e,a			; low byte envelope period
		ld	a,11
		call	WRTPSG			; write PSG register
		inc	a
		ld	e,d			; high byte envelope period
		call	WRTPSG			; write PSG register
		dec	c
		dec	c			; update packet size left
		jr	A1166			; next

A11B0:		ld	a,b			; restore voice
		add	a,8			; amplitude register
		ld	e,0			; volume = 0 (silence)
		call	WRTPSG			; write PSG register
		inc	b
		ld	hl,MUSICF
		xor	a
		scf
A11BE:		rla
		djnz	A11BE			; calculate voice mask
		and	(hl)
		xor	(hl)
		ld	(hl),a			; voice not active

; Subroutine STRTMS (start music dequeuing)
STRTMS:
A11C4:		ld	a,(MUSICF)
		or	a			; any voice active ?
		ret	nz			; yep, quit
		ld	hl,PLYCNT
		ld	a,(hl)
		or	a			; play sequences left ?
		ret	z			; nope, quit
		dec	(hl)
		ld	hl,1
		ld	(VCBA+0),hl		; timer VCB voice 0 = 1 (fetches packet in queue)
		ld	(VCBB+0),hl		; timer VCB voice 1 = 1 (fetches packet in queue)
		ld	(VCBC+0),hl		; timer VCB voice 2 = 1 (fetches packet in queue)
		ld	a,111B
		ld	(MUSICF),a		; all voices active
		ret

; Subroutine get byte from current queue
XGETQ:
A11E2:		ld	a,(QUEUEN)		; current queue
		push	hl
		push	de
		push	bc
		call	GETQ			; get byte from queue
		jp	A08DB			; quit

; Subroutine GTSTCK (read joystick)
GTSTCK:
A11EE:		dec	a			; keyboard ?
		jp	m,A1200			; yep,
		call	A120C			; read joystick port
		ld	hl,T1233		; joystick direction translation table
A11F8:		and	00001111B		; only direction bits
		ld	e,a
		ld	d,0
		add	hl,de			; to translation entry
		ld	a,(hl)			; get direction code
		ret

; GTSTCK for keyboard
A1200:		call	A1226			; read keyboard row 8
		rrca
		rrca
		rrca
		rrca				; cursor keys to b3-b0
		ld	hl,T1243		; keyboard direction translation table
		jr	A11F8			; return direction code

; Subroutine read joystick port
; Input:  A = joystick port (0 or 1)
SLSTCK:
A120C:		ld	b,a			; store joystick port
		ld	a,15			; register = IOB
		di				; disable maskable interrupts (make sure no other PSG register is selected)
		call	RDPSG			; read PSG register
		djnz	A121B			; joystick port 1,
		and	11011111B
		or	01001100B
		jr	A121F

A121B:		and	10101111B
		or	00000011B
A121F:		out	(0A1H),a		; write PSG register
		call	INGI			; read PSG IOA port
		ei				; enable maskable interrupts
		ret

; Subroutine read keyboard row 8
GTROW8:
A1226:		di				; disable maskable interrupts (make sure PPI port C is not changed)
		in	a,(0AAH)
		and	11110000B		; clear keyboard bits
		add	a,8			; keyboard row 8
		out	(0AAH),a		; select keyboard row
		in	a,(0A9H)		; read keyboard row
		ei				; enable maskable interrupts
		ret

; joystick direction translation table
T1233:		defb	0,5,1,0,3,4,2,3,7,6,8,7,0,5,1,0

; keyboard direction translation table
T1243:		defb	0,3,5,4,1,2,0,3,7,0,6,5,8,1,7,0

; Subroutine GTTRIG (read trigger)
GTTRIG:
A1253:		dec	a			; keyboard ?
		jp	m,A126C			; yep,
		push	af			; store trigger number
		and	00000001B		; joystick port in b0
		call	A120C			; read joystick port
		pop	bc			; restore trigger number
		dec	b
		dec	b			; trigger A ?
		ld	b,00010000B		; mask = trigger A
		jp	m,A1267			; yep,
		ld	b,00100000B		; mask = trigger B
A1267:		and	b			; mask trigger
A1268:		sub	1
		sbc	a,a			; return 255 if pressed, 0 if not
		ret

; GTTRIG for keyboard
A126C:		call	A1226			; read keyboard row 8
		and	00000001B		; spacebar
		jr	A1268			; return status

; Subroutine GTPDL (read paddle)
GTPDL:
A1273:		inc	a			; paddle number 1 based
		and	a			; clear Cx
		rra				; port in Cx
		push	af			; store port
		ld	b,a
		xor	a
		scf
A127A:		rla
		djnz	A127A			; calculate input bit mask
		ld	b,a			; store input bit mask
		pop	af			; restore port
		ld	c,00010000B		; pulse bit OR mask port 0
		ld	de,00000011B*256 + 10101111B	; OR mask port and pin6/7 bits, AND mask port and pulse bit
		jr	nc,A128B		; port 0,
		ld	c,00100000B		; pulse bit OR mask port 1
		ld	de,01001100B*256 + 10011111B	; OR mask port and pin6/7 bits, AND mask port and pulse bit
A128B:		ld	a,15			; register = IOB
		di				; disable maskable interrupts (because software timing is being used)
		call	RDPSG			; read PSG register
		and	e			; clear port and pulse bit
		or	d			; update port and pin 6/7 bits
		or	c			; update pulse bit
		out	(0A1H),a		; write PSG register
		xor	c
		out	(0A1H),a		; write PSG register (pulse trigger)
		ld	a,14			; register = IOA
		out	(0A0H),a		; select PSG register
		ld	c,0			; pulse length = 0
A129F:		in	a,(0A2H)		; read PSG register
		and	b			; mask off bit
		jr	z,A12A9			; pulse ends, return pulse length
		inc	c			; increase pulse length
		jp	nz,A129F		; no overflow, continue
		dec	c			; pulse length = 255
A12A9:		ei				; enable maskable interrupts
		ld	a,c			; return pulse length
		ret

; Subroutine GTPAD (read touchpad)
GTPAD:
A12AC:		cp	4			; touchpad function on port 0 ?
		ld	de,00001100B*256 + 11101100B	; pin 6/7 bits OR mask other port, clear pulse and pin 6/7 bits AND mask
		jr	c,A12B8			; yep,
		ld	de,00000011B*256 + 11010011B	; pin 6/7 bits OR mask other port, clear pulse and pin 6/7 bits AND mask
		sub	4			; to touchpad function without port
A12B8:		dec	a			; touchpad function = fetch data ?
		jp	m,A12C5			; yep,
		dec	a			; touchpad function = read X position ?
		ld	a,(PADX)
		ret	m			; yep, return touchpad X position
		ld	a,(PADY)
		ret	z			; touchpad function = read Y position, return pad Y position
A12C5:		push	af			; store touchpad function
		ex	de,hl
		ld	(FILNAM+0),hl		; store touchpad masks
		sbc	a,a			; port 0 -> 0FFH, port 1 -> 00H
		cpl				; port 0 -> 00H, port 1 -> 0FFH
		and	01000000B
		ld	c,a			; port select OR mask
		ld	a,15			; register = IOB
		di				; disable maskable interrupts (because software timing is being used)
		call	RDPSG			; read PSG register
		and	10111111B		; clear port bit
		or	c
		out	(0A1H),a		; select port
		pop	af			; restore touchpad function
		jp	m,A12E8			; fetch data, handle

		; read touchpad status
		call	INGI			; read PSG IOA port
		ei				; enable maskable interrupts
		and	00001000B		; trigger A
		sub	1
		sbc	a,a			; return 0FFH when trigger, otherwise 00H
		ret

; fetch touchpad data
A12E8:		ld	c,00000000B		; serial data = 0, both d0,d1 -> channel 0
		call	A1332			; read touchpad data (dummy read)
		call	A1332			; read touchpad data (dummy read)
		jr	c,A131A			; no sense, quit
		call	A1320			; read X and Y coordinates from touchpad
		jr	c,A131A			; no sense, quit
		push	de			; store first coordinates
		call	A1320			; read X and Y coordinates from touchpad
		pop	bc			; restore first coordinates
		jr	c,A131A			; no sense, quit
		ld	a,b
		sub	d			; first X >= second X ?
		jr	nc,A1304		; yep, delta already postive
		cpl
		inc	a			; complement delta
A1304:		cp	4+1			; difference >4 ?
		jr	nc,A12E8		; yep, try again
		ld	a,c
		sub	e			; first Y >= second Y ?
		jr	nc,A130E		; yep, delta already positive
		cpl
		inc	a			; complement delta
A130E:		cp	4+1			; difference >4 ?
		jr	nc,A12E8		; yep, try again
		ld	a,d
		ld	(PADX),a		; store X coordinate
		ld	a,e
		ld	(PADY),a		; store Y coordinate
A131A:		ei
		ld	a,h
		sub	1
		sbc	a,a			; return 0FFH when data fetched, otherwise 00H
		ret

; Subroutine read X and Y coordinates from touchpad
; Output: D  = X coordinate
;		E  = Y coordinate
;		Cx = set when no sense

REDCOD:
A1320:		ld	c,00001010B		; serial data = 1, both d0,d1 -> channel 3
		call	A1332			; read touchpad data
		ret	c			; no sense, quit (H <> 0)
		ld	d,l			; store X coordinate
		push	de			; store X coordinate
		ld	c,00000000B		; serial data = 0, both d0,d1 -> channel 0
		call	A1332			; read touchpad data
		pop	de			; restore X coordinate
		ld	e,l			; store Y coordinate
		xor	a			; clear Cx
		ld	h,a			; flag data fetched
		ret

; Subroutine read touchpad data
; Input:  C  = serial data in b3,b1 (channel)
; Output: Cx = no sense
;		H  = IOA port value
REDPAD:
A1332:		call	A135B			; wait for EOC, select chip
		ld	b,8			; 8 bits
		ld	d,c			; OR mask = pin 7 ports OR mask
A1338:		res	0,d			; pin 6 port 0 = 0 (serial clock high)
		res	2,d			; pin 6 port 1 = 0 (serial clock high)
		call	OUTGI			; update chip select, serial data and serial clock output bits of PSG IOB
		call	INGI			; read PSG IOA port
		ld	h,a			; store value
		rra
		rra
		rra				; b2 (SO) in Cx
		rl	l			; shift left, Cx in b0
		set	0,d			; pin 6 port 0 = 1 (serial clock low)
		set	2,d			; pin 6 port 1 = 1 (serial clock low)
		call	OUTGI			; update chip select, serial data and serial clock output bits of PSG IOB
		djnz	A1338
		set	4,d			; pulse port 0 = 1 (deselect chip)
		set	5,d			; pulse port 1 = 1 (deselect chip)
		call	OUTGI			; update chip select, serial data and serial clock output bits of PSG IOB
		ld	a,h			; restore value
		rra				; -SENSE in Cx
		ret

; Subroutine wait for EOC, select chip
; Input:  C = serial data in b3,b1
CHKEOC:
A135B:		ld	a,00110101B		; pulse ports = 1 (deselect chip), pin 6 ports = 1 (serial clock low)
		or	c			; OR mask pin 7 bit ports
		ld	d,a			; set bit OR mask pulse and pin 6/7 bits
		call	OUTGI			; update chip select, serial data and serial clock output bits of PSG IOB
A1362:		call	INGI			; read PSG IOA port
		and	00000010B		; EOC ?
		jr	z,A1362			; nope, wait
		res	4,d			; pulse port 0 = 0 (select chip)
		res	5,d			; pulse port 1 = 0 (select chip)

; Subroutine update chip select, serial data and serial clock output bits of PSG IOB
; Input:  D = chip select (b5/b4), serial data (b3,b1) and serial clock output bits (b2,b0)
OUTGI:
A136D:		push	hl
		push	de			; store set bit OR mask
		ld	hl,(FILNAM+0)		; restore touchpad masks
		ld	a,l			; clear pulse and pin 6/7 bits AND mask
		cpl				; pulse and pin 6/7 bits AND mask
		and	d			; clear bits
		ld	d,a			; OR mask
		ld	a,15			; register = IOB
		out	(0A0H),a
		in	a,(0A2H)		; read PSG register
		and	l			; clear pulse and pin 6/7 bits of port
		or	d			; set pulse and pin 6/7 bits of port
		or	h			; set pin 6/7 bits of other port to 1
		out	(0A1H),a		; write PSG register
		pop	de			; restore set bit OR mask
		pop	hl
		ret

; Subroutine STMOTR (change cassette motor status)
; Input:  A = 00H (motor on), 01H (motor off), 0FFH (flip motor status)
STMOTR:
A1384:		and	a			; A = 0FFH (flip) ?
		jp	m,A1392			; yep, flip cassette motor status
A1388:		jr	nz,A138D		; A = 01H, casette motor off
		ld	a,00001001B		; set b4 (cassette motor on)
		defb	0C2H			; JP NZ,xx: a trick to skip the next line
A138D:		ld	a,00001000B		; reset b4 (cassette motor off)
		out	(0ABH),a
		ret

A1392:		in	a,(0AAH)
		and	00010000B		; cassette motor status
		jr	A1388

; Subroutine NMI (non maskable interrupt handler)
NMI:
A1398:		SYSHOOK	H_NMI			; hook non maskable interrupt
		retn				; acknowledge NMI

; Subroutine INIFNK (initialize function key definition)
INIFNK:
A139D:		ld	bc,10*16
		ld	de,FNKSTR
		ld	hl,T13A9
		ldir
		ret

T13A9:		defb	"color ",0
		defs	16-7,0
		defb	"auto ",0
		defs	16-6,0
		defb	"goto ",0
		defs	16-6,0
		defb	"list ",0
		defs	16-6,0
		defb	"run",13,0
		defs	16-5,0

	IF BASVER = 0
		defb	"color 15,4,7",13,0
		defs	16-14,0
	ELSE
		defb	"color 15,4,4",13,0
		defs	16-14,0
	ENDIF
		defb	"cload\"",0
		defs	16-7,0
		defb	"cont",13,0
		defs	16-6,0
		defb	"list.",13,30,30,0
		defs	16-9,0
		defb	12,"run",13,0
		defs	16-6,0

; Subroutine RDVDP (read VDP status register)
RDVDP:
A1449:		in	a,(099H)		; read VDP status register
		ret

; Subroutine RSLREG (read primary slot register)
; Output: A = value
RSLREG:
A144C:		in	a,(0A8H)		; read primary slot register (PPI port A)
		ret

; Subroutine WSLREG (write primary slot register)
; Input:  A = value
WSLREG:
A144F:		out	(0A8H),a		; write primary slot register (PPI port A)
		ret

; Subroutine SNSMAT (read keyboard row)
; Input:  A = keyboard row
SNSMAT:
A1452:		ld	c,a			; store keyboard row
		di				; disable maskable interrupts (make sure PPI port C is not changed)
		in	a,(0AAH)
		and	11110000B		; clear keyboard row bits
		add	a,c			; update keyboard row bits
		out	(0AAH),a		; select keyboard row
		in	a,(0A9H)		; read keyboard row
		ei				; enable maskable interrupts
		ret

; Subroutine ISFLIO (is BASIC interpreter I/O redirected to I/O channel ?)
ISFLIO:
A145F:		SYSHOOK	H_ISFL			; hook
		push	hl
		ld	hl,(PTRFIL)		; BASIC interpreter I/O channel
		ld	a,l
		or	h			; I/O channel = none (no redirect) ?
		pop	hl
		ret

; Subroutine DCOMPR (compare HL with DE)
DCOMPR:
A146A:		ld	a,h
		sub	d
		ret	nz
		ld	a,l
		sub	e
		ret

; Subroutine GETVCP (get pointer to macro string info voice buffer)
; Input:  A = voice
GETVCP:
A1470:		ld	l,2			; offset = macro string info
		jr	A1477

; Subroutine GETVC2 (get pointer in current voice buffer)
; Input:  L  = offset in voice buffer
; Output: HL = pointer in voice buffer
GETVC2:
A1474:		ld	a,(VOICEN)		; current voice
A1477:		push	de
	IF OPTM = 0
		ld	de,VCBA
		ld	h,0
		add	hl,de
		or	a
		jr	z,A1488
	ELSE
		ld	de,VCBA-37
		ld	h,0
		add	hl,de
		inc	a
	ENDIF
	  ld	de,37
A1484:		add	hl,de
		dec	a
		jr	nz,A1484
A1488:		pop	de
		ret

; Subroutine PHYDIO (physical disk I/O)
PHYDIO:
A148A:		SYSHOOK	H_PHYD			; hook physical disk I/O
		ret

; Subroutine FORMAT (format disk)
FORMAT:
A148E:		SYSHOOK	H_FORM			; hook format disk
		ret
