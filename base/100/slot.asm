; ------------------------------------------------------------------------------
; slot.asm
; BIOS slot functions, MSX 1 version (version 1.0)
; ------------------------------------------------------------------------------

		ALIGN	015CH

	IF SLOTFIX = 0
		defs	001B6H-$,0
	ELSE
		defs	0016FH-$,0

; RDSLT, handle expanded slot (with fix)
C016F:		CALL	C01AD			; slot = primary slot 0, page = 0 ?
		JR	NZ,A01C6		; nope, use normal method
		PUSH	HL			; store address
		CALL	C0199			; enable BASIC ROM in page 1
		EX	(SP),HL			; store orginal primary slot register, orginal secondary slot register, restore address
		CALL	C7FBE			; read from primary slot 0
		JR	J018D			; restore slot registers

; WRSLT, handle expanded slot (with fix)
C017E:		CALL	C01AD			; slot = primary slot 0, page = 0 ?
		JP	NZ,A01E1		; nope, use normal method
		POP	DE			; restore byte
		PUSH	HL			; store address
		CALL	C0199			; enable BASIC ROM in page 1
		EX	(SP),HL			; store orginal primary slot register, orginal secondary slot register, restore address
		CALL	C7FC4			; write to primary slot 0
J018D:		EX	(SP),HL			; store address, restore orginal primary slot register, orginal secondary slot register
		PUSH	AF			; store byte
		LD	A,L
		OUT	(0A8H),A		; restore primary slot register
		LD	A,H
		LD	(D_FFFF),A		; restore secondary slot register
		POP	AF			; restore byte
		POP	HL			; restore address
		RET

; Subroutine enable BASIC ROM in page 1
C0199:		PUSH	AF
		LD	A,(D_FFFF)
		CPL
		LD	H,A			; store current secondary slot register
		AND	0F3H			; page 1 = secondary slot 0
		LD	(D_FFFF),A
		IN	A,(0A8H)
		LD	L,A			; store current primary slot register
		AND	0F3H			; page 1 = primary slot 0
		OUT	(0A8H),A
		POP	AF
		RET

; Subroutine slot = primary slot 0, page = 0 ?
C01AD:		INC	D
		DEC	D			; update primary slot mask = primary slot 0 ?
		RET	NZ			; nope, quit
		LD	B,A			; store slot id
		LD	A,E
		CP	00000011b		; select page mask = page 0 ?
		LD	A,B			; restore slot id
		RET

	ENDIF ; SLOTFIX

; Subroutine RDSLT
RDSLT:
A01B6:		call	A027E			; calculate slot masks
	IF SLOTFIX = 1
		jp	m,C016F			; expanded slot, handle (with fix)
	ELSE
		jp	m,A01C6			; expanded slot, handle
	ENDIF
		in	a,(0A8H)
		ld	d,a			; store primary slot register
		and	c			; clear slot of page
		or	b			; update slot of page
		call	RDPRIM			; read byte
		ld	a,e			; byte
		ret

A01C6:		push	hl			; store address
		call	A02A3			; store and change secondary slot register
		ex	(sp),hl			; store orginal secondary slot register, restore address
		push	bc			; store primary slot register
		call	A01B6			; do a RDSLT for primary slot
		jr	A01EC			; restore secondary slot register

; Subroutine WRSLT
WRSLT:
A01D1:		push	de			; store byte
		call	A027E			; calculate slot masks
	IF SLOTFIX = 1
		jp	m,C017E			; expanded slot, handle (with fix)
	ELSE
		jp	m,A01E1			; expanded slot, handle
	ENDIF
		pop	de			; restore byte
		in	a,(0A8H)
		ld	d,a			; store primary slot register
		and	c			; clear slot of page
		or	b			; update slot of page
		jp	WRPRIM			; write byte

A01E1:		ex	(sp),hl			; store address, restore byte
		push	hl			; store byte
		call	A02A3			; store and change secondary slot register
		pop	de			; restore byte
		ex	(sp),hl			; store orginal secondary slot register, restore address
		push	bc			; store primary slot register
		call	A01D1			; do a WRSLT for primary slot
A01EC:		pop	bc			; restore primary slot register
		ex	(sp),hl			; store address, restore orginal secondary slot register
		push	af			; store byte
		ld	a,b			; primary slot register
		and	00111111b		; clear slot of page 3
		or	c			; update slot of page 3
		out	(0A8H),a		;
		ld	a,l			; orginal secondary slot register
		ld	(D_FFFF),a		; update secondary slot register
		ld	a,b			; orginal primary slot register
		out	(0A8H),a		; update primary slot register
		pop	af			; restore byte
		pop	hl			; restore address
		ret

; Subroutine CALBAS
CALBAS:
A01FF:		ld	iy,(EXPTBL+0-1)		; slot id BIOS ROM
		jr	A0217			; CALSLT

; Subroutine CALLF
CALLF:
A0205:		ex	(sp),hl			; store HL, restore return address
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

; Subroutine CALSLT
; Input: IX  = address subroutine
;	  IYH = slot id
CALSLT:
A0217:		exx
		ex	af,af'			; alternative set
		push	iy			; store slot id
		pop	af			; restore slot id
		push	ix			; store address subroutine
		pop	hl			; restore address subroutine
		call	A027E			; calculate slot masks
		jp	m,A022E			; expanded slot, handle
		in	a,(0A8H)
		push	af			; store primary slot register
		and	c			; clear slot of page
		or	b			; update slot of page
		exx
		jp	CLPRIM			; call subroutine in slot

A022E:		call	A02A3			; store and change secondary slot register
		push	af			; store slot id
		pop	iy			; restore slot id
		push	hl			; store orginal secondary slot register
		push	bc			; store orginal primary slot register
		ld	c,a
		ld	b,0			; primary slot
		ld	a,l			; orginal secondary slot
		and	h			; clear slot of page
		or	d			; update slot of page
		ld	hl,SLTTBL
		add	hl,bc
		ld	(hl),a			; update SLTTBL
		push	hl			; store pointer in SLTTBL
		ex	af,af'
		exx
		call	A0217			; do a CALSLT with primary slot
		exx
		ex	af,af'
		pop	hl			; restore pointer in SLTTBL
		pop	bc			; restore orginal primary slot register
		pop	de			; restore orginal secondary slot register
		ld	a,b			; orginal primary slot register
		and	03FH			; clear slot of page 3
		or	c			; update slot of page 3
		di
		out	(0A8H),a
		ld	a,e			; orginal secondary slot register
		ld	(D_FFFF),a		; update secondary slot register
		ld	a,b			; orginal primary slot register
		out	(0A8H),a		; update primary slot register
		ld	(hl),e			; update SLTTBL
		ex	af,af'
		exx
		ret

; Subroutine ENASLT
ENASLT:
A025E:		call	A027E			; calculate slot masks
		jp	m,A026B			; expanded slot, handle
		in	a,(0A8H)		; primary slot register
		and	c			; clear slot of page
		or	b			; update slot of page
		out	(0A8H),a		; update primary slot register
		ret

A026B:		push	hl			; store address
		call	A02A3			; store and change secondary slot register
		ld	c,a
		ld	b,0			; primary slot
		ld	a,l			; orginal secondary slot register
		and	h			; clear slot of page
		or	d			; update slot of page
		ld	hl,SLTTBL
		add	hl,bc
		ld	(hl),a			; update SLTTBL
		pop	hl			; restore address
		ld	a,c			; restore slot id
		jr	A025E			; ENASLT for primary slot

; Subroutine calculate slot masks
; Input:  A  = slotid
;		HL = address
; Output: Sx set if expanded slot
;		A  = slot id
;		D  = update primary slot mask
;		E  = select page mask
;		B  = update primary slot of page mask
;		C  = clear page mask
A027E:		di
		push	af			; store slot id
		ld	a,h
		rlca
		rlca
		and	00000011b		; page number
		ld	e,a
		ld	a,11000000b
A0288:		rlca
		rlca
		dec	e
		jp	p,A0288
		ld	e,a			; e = page select AND mask
		cpl
		ld	c,a			; c = page clear AND mask
		pop	af			; restore slot id
		push	af			; store slot id
		and	00000011b		; primary slot
		inc	a
		ld	b,a			; primary slot counter
		ld	a,0-01010101b
A0299:		add	a,01010101b
		djnz	A0299			; calculate update primary slot OR mask
		ld	d,a			; store update primary slot
		and	e			; select page mask
		ld	b,a			; store update primary slot of page
		pop	af			; restore slot id
		and	a			; flag expanded slot
		ret

; Subroutine store and change secondary slot register
; Input:  A = slot id
;		D = update primary slot mask
;		E = select page mask
; Output: A = slot id primary slot
;		L = orginal secondary slot register
;		B = primary slot register
A02A3:		push	af			; store slot id
		ld	a,d			; update primary slot mask
		and	11000000b		; page 3
		ld	c,a			; update primary slot mask page 3
		pop	af			; restore slot id
		push	af			; store slot id
		ld	d,a			; store slotid
		in	a,(0A8H)
		ld	b,a			; store primary slot register
		and	00111111b		; clear slot page 3
		or	c			; update slot page 3
		out	(0A8H),a		; make secondary slot register accessable
		ld	a,d
		rrca
		rrca
		and	003H			; get secondary slot from slotid
		ld	d,a
		ld	a,0-01010101b
A02BB:		add	a,01010101b
		dec	d
		jp	p,A02BB			; calculate update secondary slot mask
		and	e			; select page mask
		ld	d,a			; update secondary slot mask page
		ld	a,e			; select page mask
		cpl
		ld	h,a			; page clear mask
		ld	a,(D_FFFF)
		cpl
		ld	l,a			; store currrent secondary slot register
		and	h			; clear page
		or	d			; set new slot
		ld	(D_FFFF),a		; update secondary slot register
		ld	a,b
		out	(0A8H),a		; restore primary slot register (switch back page 3)
		pop	af			; restore slot id
		and	00000011b		; primary slot
		ret
