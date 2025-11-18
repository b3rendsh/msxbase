; ------------------------------------------------------------------------------
; msxgrp.asm
; BIOS graphic functions, MSX 1 version (version 1.0)
; ------------------------------------------------------------------------------

		ALIGN	1510H

; Subroutine GRPPRT
; Input:  A = character
GRPPRT:
A1510:		push	hl
		push	de
		push	bc
		push	af
		call	CNVCHR			; is grafic header character ?
		jr	nc,A157B		; yep, quit
		jr	nz,A1523		; graphic character,
		cp	00DH			; CR ?
		jr	z,A157E			; yep, handle new line
		cp	020H			; other control character ?
		jr	c,A157B			; yep, ignore and quit
A1523:		call	GETPAT			; get character pattern
		ld	a,(FORCLR)
		ld	(ATRBYT),a		; color = foreground color
		ld	hl,(GRPACY)
		ex	de,hl
		ld	bc,(GRPACX)		; graphic accumulator
		call	SCALXY			; scale coordinates
		jr	nc,A157B		; out of range, quit
		call	MAPXYC			; map coordinates and update current pixel position
		ld	de,PATWRK
		ld	c,8			; 8 lines
A1541:		ld	b,8			; 8 pixels
		call	FETCHC			; fetch current pixel position
		push	hl			; store VRAM address
		push	af			; store mask
		ld	a,(de)
A1549:		add	a,a			; pixel on ?
		push	af			; store pattern
		call	c,SETC			; yep, set color current pixel position
		call	A16AC			; move pixel position right, if possible
		pop	hl			; restore pattern
		jr	c,A1558			; out of range, next line
		push	hl			; store pattern
		pop	af			; restore pattern
		djnz	A1549			; next pixel
A1558:		pop	af			; restore mask
		pop	hl			; restore VRAM address
		call	STOREC			; store current pixel position
		call	TDOWNC			; pixel position down, if possible
		jr	c,A1566			; out of range,
		inc	de
		dec	c			; lines finished ?
		jr	nz,A1541		; nope, next line
A1566:		call	A15D9			; in graphic mode 2 ?
		ld	a,(GRPACX)
		jr	z,A1574			; yep,
		add	a,4*8
		jr	c,A157E
		jr	A1578

A1574:		add	a,8
		jr	c,A157E
A1578:		ld	(GRPACX),a
A157B:		jp	POPALL			; quit

A157E:		xor	a
		ld	(GRPACX),a
		call	A15D9			; in graphic mode 2 ?
		ld	a,(GRPACY)
		jr	z,A158D			; yep,
		add	a,020H
		defb	001H			; LD BC,xxxx: trick to skip next instruction
A158D:		add	a,008H
		cp	0C0H
		jr	c,A1594
		xor	a
A1594:		ld	(GRPACY),a
		jr	A157B			; quit

; Subroutine SCALXY (scale coordinates)
; Input:  BC = X
;		DE = Y
; Output: BC = scaled X
;		DE = scaled Y
SCALXY:
A1599:		push	hl			; store
		push	bc			; store X
		ld	b,1			; assume in range
		ex	de,hl
		ld	a,h
		add	a,a			; Y above top ?
		jr	nc,A15A7		; nope,
		ld	hl,0			; Y = 0
		jr	A15AF

A15A7:		ld	de,192
		RST	R_DCOMPR		; Y below bottom ?
		jr	c,A15B1			; nope,
		ex	de,hl
		dec	hl			; Y = 191
A15AF:		ld	b,0			; out of range
A15B1:		ex	(sp),hl			; store Y, restore X
		ld	a,h
		add	a,a			; X left of leftmost ?
		jr	nc,A15BB		; nope,
		ld	hl,0			; X = 0
		jr	A15C3
;
A15BB:		ld	de,256
		RST	R_DCOMPR		; X right of rightmost ?
		jr	c,A15C5			; nope,
		ex	de,hl
		dec	hl			; X = 255
A15C3:		ld	b,0			; out of range
A15C5:		pop	de
		call	A15D9			; in graphic mode 2 ?
		jr	z,A15D3			; yep,
		srl	l
		srl	l			; X = X/4
		srl	e
		srl	e			; Y = Y/4
A15D3:		ld	a,b
		rrca				; out of range in Cx
		ld	b,h
		ld	c,l			; X
		pop	hl			; restore
		ret

; Subroutine in graphic mode 2 ?
A15D9:		ld	a,(SCRMOD)
		sub	2
		ret

; Subroutine MAPXYC (map coordinates and update current pixel position)
; Input:  BC = X
;		DE = Y
MAPXYC:
A15DF:		push	bc			; store X
		call	A15D9			; in graphic mode 2 ?
		jr	nz,A1613		; nope, handle mode 3

		; MAPXYC mode 2
		ld	d,c			; X
		ld	a,c			; X
		and	007H
		ld	c,a
		ld	hl,T160B
		add	hl,bc
		ld	a,(hl)			; calculate mask
		ld	(CMASK),a		; update mask current pixel position
		ld	a,e			; Y
		rrca
		rrca
		rrca
		and	01FH
		ld	b,a
		ld	a,d
		and	0F8H
		ld	c,a
		ld	a,e
		and	007H
		or	c
		ld	c,a
		ld	hl,(GRPCGP)
		add	hl,bc
		ld	(CLOC),hl		; update VRAM address current pixel position
		pop	bc			; restore X
		ret

; pixel mask table
T160B:		defb	10000000b
		defb	01000000b
		defb	00100000b
		defb	00010000b
		defb	00001000b
		defb	00000100b
		defb	00000010b
		defb	00000001b

; MAPXYC mode 3
A1613:		ld	a,c			; X
		rrca				; left pixel of byte ?
		ld	a,0F0H
		jr	nc,A161B		; yep, use left pixel mask
		ld	a,00FH			; right pixel mask
A161B:		ld	(CMASK),a		; update mask current pixel position
		ld	a,c			; X
		add	a,a
		add	a,a
		and	0F8H
		ld	c,a
		ld	a,e
		and	007H
		or	c
		ld	c,a
		ld	a,e
		rrca
		rrca
		rrca
		and	007H
		ld	b,a
		ld	hl,(MLTCGP)
		add	hl,bc
		ld	(CLOC),hl		; update VRAM address current pixel position
		pop	bc
		ret

; Subroutine FETCHC (fetch current pixel position)
; Output: HL = VRAM address
;		A  = mask
FETCHC:
A1639:		ld	a,(CMASK)		; mask current pixel position
		ld	hl,(CLOC)		; VRAM address current pixel position
		ret

; Subroutine STOREC (store current pixel position)
; Input:  HL = VRAM address, A = mask
STOREC:
A1640:		ld	(CMASK),a		; update mask current pixel position
		ld	(CLOC),hl		; update VRAM address current pixel position
		ret

; Subroutine READC (get color current pixel position)
; Output: A = color
READC:
A1647:		push	bc
		push	hl
		call	FETCHC			; fetch current pixel position
		ld	b,a			; store mask
		call	A15D9			; in graphic mode 2 ?
		jr	nz,A166C		; nope, handle mode 3
		call	RDVRM			; RDVRM
		and	b
		push	af
		ld	bc,02000H
		add	hl,bc
		call	RDVRM			; RDVRM
		ld	b,a
		pop	af
		ld	a,b
		jr	z,A1667
A1663:		rrca
		rrca
		rrca
		rrca
A1667:		and	00FH
		pop	hl
		pop	bc
		ret
;
A166C:		call	RDVRM			; RDVRM
		inc	b
		dec	b
		jp	p,A1667
		jr	A1663

; Subroutine SETATR
SETATR:
A1676:		cp	010H
		ccf
		ret	c
		ld	(ATRBYT),a
		ret

; Subroutine SETC (set color current pixel position)
SETC:
A167E:		push	hl			; store
		push	bc			; store
		call	A15D9			; in graphic mode 2 ?
		call	FETCHC			; fetch current pixel position
		jr	nz,A1690		; nope, handle mode 3

		; SETC mode 2
		push	de			; store
		call	A186C			; update color
		pop	de			; restore
		pop	bc			; restore
		pop	hl			; restore
		ret

; SETC mode 3
A1690:		ld	b,a			; store mask
		call	RDVRM			; RDVRM
		ld	c,a			; VRAM byte
		ld	a,b
		cpl
		and	c			; mask off pixel
		ld	c,a
		ld	a,(ATRBYT)		; color
		inc	b
		dec	b			; left pixel ?
		jp	p,A16A5			; nope, right pixel
		add	a,a
		add	a,a
		add	a,a
		add	a,a			; color to high nibble
A16A5:		or	c
		call	WRTVRM			; WRTVRM
		pop	bc			; restore
		pop	hl			; restore
		ret

; Subroutine move pixel position right, if possible
A16AC:		push	hl
		call	A15D9			; in graphic mode 2 ?
		jp	nz,A1779		; nope, handle mode 3

		; move pixel position right, if possible mode 2
		call	FETCHC			; fetch current pixel position
		rrca				; shift mask right
		jr	nc,A1704		; same VRAM address, update mask and quit with in range
		ld	a,l
		and	0F8H			; ignore line bits
		cp	0F8H			; already at right of range ?
		ld	a,080H
		jr	nz,A16D2		; nope, update VRAM address with +8, update mask with pixel at most left
		jp	A175A			; quit with out of range

; Subroutine RIGHTC (move pixel position right)
RIGHTC:
A16C5:		push	hl
		call	A15D9			; in graphic mode 2 ?
		jp	nz,A178B		; nope, handle mode 3

		; move pixel position right mode 2
		call	FETCHC			; fetch current pixel position
		rrca				; shift mask right
		jr	nc,A1704		; same VRAM address, update mask and quit with in range

; update VRAM address with +8, update mask with pixel at most left, quit with in range
A16D2:		push	de
		ld	de,8
		jr	A16FF

; Subroutine move pixel position left, if possible
A16D8:		push	hl
		call	A15D9			; in graphic mode 2 ?
		jp	nz,A179C		; nope, handle mode 3

		; move pixel position left, if possible mode 2
		call	FETCHC			; fetch current pixel position
		rlca				; shift mask left
		jr	nc,A1704		; same VRAM address, update mask and quit
		ld	a,l
		and	0F8H			; ignore line bits
		ld	a,1
		jr	nz,A16FB		; update VRAM address with -8, update mask with pixel at most right, quit with in range
		jr	A175A			; quit with out of range

; Subroutine LEFTC (move pixel position left)
LEFTC:
A16EE:		push	hl
		call	A15D9			; in graphic mode 2 ?
		jp	nz,A17AC		; nope, handle mode 3

		; move pixel position left mode 2
		call	FETCHC			; fetch current pixel position
		rlca				; shift mask left
		jr	nc,A1704		; same VRAM address, update mask and quit

; update VRAM address with -8, update mask with pixel at most right, quit with in range
A16FB:		push	de
		ld	de,-8
A16FF:		add	hl,de
		ld	(CLOC),hl		; update VRAM address current pixel position
		pop	de
A1704:		ld	(CMASK),a		; update mask current pixel position
		and	a			; pixel in range
		pop	hl
		ret

; Subroutine TDOWNC (pixel position down, if possible)
TDOWNC:
A170A:		push	hl
		push	de
		ld	hl,(CLOC)		; VRAM address current pixel position
		call	A15D9			; in graphic mode 2 ?
		jp	nz,A17C6		; nope, handle mode 3

		; pixel position down, if possible mode 2
		push	hl			; store VRAM address
		ld	hl,(GRPCGP)
		ld	de,256*24-256
		add	hl,de
		ex	de,hl
		pop	hl			; restore VRAM address
		RST	R_DCOMPR
		jr	c,A1735
		ld	a,l
		inc	a
		and	007H
		jr	nz,A1735
		jr	A1759			; quit with out of range

; Subroutine DOWNC (move pixel position down)
DOWNC:
A172A:		push	hl
		push	de
		ld	hl,(CLOC)		; VRAM address current pixel position
		call	A15D9			; in graphic mode 2 ?
		jp	nz,A17DC		; nope, handle mode 3

; DOWNC mode 2
A1735:		inc	hl
		ld	a,l
		ld	de,248
		jr	A176D

; Subroutine TUPC (pixel position up, if possible)
TUPC:
A173C:		push	hl
		push	de
		ld	hl,(CLOC)		; VRAM address current pixel position
		call	A15D9			; in graphic mode 2 ?
		jp	nz,A17E3		; nope, handle mode 3

		; TUPC mode 2
		push	hl
		ld	hl,(GRPCGP)
		ld	de,256
		add	hl,de
		ex	de,hl
		pop	hl
		RST	R_DCOMPR
		jr	nc,A1768
		ld	a,l
		and	007H
		jr	nz,A1768
A1759:		pop	de			; restore
A175A:		scf				; out of range
		pop	hl			; restore
		ret

; Subroutine UPC (pixel position up)
UPC:
A175D:		push	hl
		push	de
		ld	hl,(CLOC)		; VRAM address current pixel position
		call	A15D9			; in graphic mode 2 ?
		jp	nz,A17F8		; nope, handle mode 3

; pixel position up mode 2
A1768:		ld	a,l
		dec	hl
		ld	de,-248
A176D:		and	007H
		jr	nz,A1772
		add	hl,de
A1772:		ld	(CLOC),hl		; update VRAM address current pixel position
		and	a
		pop	de
		pop	hl
		ret

; move pixel position right, if possible mode 3
A1779:		call	FETCHC			; fetch current pixel position
		and	a			; mask at completely left ?
		ld	a,00FH
		jp	m,A17C0			; yep,
		ld	a,l
		and	0F8H
		cp	0F8H
		jr	nz,A1794
		jr	A175A			; quit with out of range

; move pixel position right mode 3
A178B:		call	FETCHC			; fetch current pixel position
		and	a
		ld	a,00FH
		jp	m,A17C0
A1794:		push	de
		ld	de,8
		ld	a,0F0H
		jr	A17BB

; move pixel position left, if possible mode 3
A179C:		call	FETCHC			; fetch current pixel position
		and	a
		ld	a,0F0H
		jp	p,A17C0
		ld	a,l
		and	0F8H
		jr	nz,A17B5
		jr	A175A			; quit with out of range

; move pixel position left mode 3
A17AC:		call	FETCHC			; fetch current pixel position
		and	a
		ld	a,0F0H
		jp	p,A17C0
A17B5:		push	de
		ld	de,-8
		ld	a,00FH
A17BB:		add	hl,de
		ld	(CLOC),hl		; update VRAM address current pixel position
		pop	de
A17C0:		ld	(CMASK),a		; update mask current pixel position
		and	a
		pop	hl
		ret

; pixel position down, if possible mode 3
A17C6:		push	hl
		ld	hl,(MLTCGP)
		ld	de,64*24-256
		add	hl,de
		pop	hl
		RST	R_DCOMPR
		jr	c,A17DC
		ld	a,l
		inc	a
		and	007H
		jr	nz,A17DC
		scf
		pop	de
		pop	hl
		ret

; DOWNC mode 3
A17DC:		inc	hl
		ld	a,l
		ld	de,248
		jr	A17FD

; TUPC mode 3
A17E3:		push	hl
		ld	hl,(MLTCGP)
		ld	de,256
		add	hl,de
		pop	hl
		RST	R_DCOMPR
		jr	nc,A17F8
		ld	a,l
		and	007H
		jr	nz,A17F8
		scf
		pop	de
		pop	hl
		ret

; pixel position up mode 3
A17F8:		ld	a,l
		dec	hl
		ld	de,-248
A17FD:		and	007H
		jr	nz,A1802
		add	hl,de
A1802:		ld	(CLOC),hl		; update VRAM address current pixel position
		and	a
		pop	de
		pop	hl
		ret

; Subroutine NSETCX
; Input:  HL = number of pixels
NSETCX:
A1809:		call	A15D9			; in graphic mode 2 ?
		jp	nz,A18BB		; nope, handle mode 3

		; NSETCX mode 2
		push	hl			; store number of pixels
		call	FETCHC			; fetch current pixel position
		ex	(sp),hl			; store VRAM address, restore number of pixels
		add	a,a			; leftmost pixel of byte ?
		jr	c,A182F			; yep,
		push	af			; store mask
		ld	bc,-1
		rrca
A181C:		add	hl,bc			; decrease number of pixels
		jr	nc,A1864		; finished,
		rrca				; shift mask right, add
		jr	nc,A181C
		pop	af			; restore mask
		dec	a
		ex	(sp),hl			; store number of pixels, restore VRAM address
		push	hl			; store VRAM address
		call	A186C			; update color
		pop	hl			; restore VRAM address
		ld	de,8
		add	hl,de			;
		ex	(sp),hl			; restore number of pixels
A182F:		ld	a,l
		and	007H
		ld	c,a
		ld	a,h
		rrca
		ld	a,l
		rra
		rrca
		rrca
		and	03FH
		pop	hl
		ld	b,a
		jr	z,A1853
A183F:		xor	a
		call	WRTVRM			; WRTVRM
		ld	de,02000H
		add	hl,de
		ld	a,(ATRBYT)
		call	WRTVRM			; WRTVRM
		ld	de,02000H+8
		add	hl,de
		djnz	A183F
A1853:		dec	c
		ret	m
		push	hl
		ld	hl,T185D
		add	hl,bc
		ld	a,(hl)
		jr	A186B

T185D:		defb	10000000b
		defb	11000000b
		defb	11100000b
		defb	11110000b
		defb	11111000b
		defb	11111100b
		defb	11111110b

; Subroutine 
A1864:		add	a,a
		dec	a
		cpl
		ld	b,a			; AND mask
		pop	af			; restore mask
		dec	a
		and	b
A186B:		pop	hl

; Subroutine update color
; Input:  A = color
A186C:		ld	b,a
		call	RDVRM			; RDVRM
		ld	c,a			; VRAM pattern byte
		ld	de,02000H
		add	hl,de
		call	RDVRM			; RDVRM
		push	af			; store VRAM color byte
		and	00FH
		ld	e,a
		pop	af			; restore VRAM color byte
		sub	e
		ld	d,a
		ld	a,(ATRBYT)
		cp	e
		jr	z,A189E
		add	a,a
		add	a,a
		add	a,a
		add	a,a
		cp	d
		jr	z,A18A2
		push	af
		ld	a,b
		or	c
		cp	0FFH
		jr	z,A18AA
		push	hl
		push	de
		call	A18A2
		pop	de
		pop	hl
		pop	af
		or	e
		jr	A18B8

A189E:		ld	a,b
		cpl
		and	c
		defb	011H			; LD DE,xxxx: trick to skip next 2 instructions
A18A2:		ld	a,b
		or	c
A18A4:		ld	de,02000H
		add	hl,de
		jr	A18B8
;
A18AA:		pop	af
		ld	a,b
		cpl
		push	hl
		push	de
		call	A18A4
		pop	de
		pop	hl
		ld	a,(ATRBYT)
		or	d
A18B8:		jp	WRTVRM			; WRTVRM

; NSETCX mode 3
A18BB:		push	hl
		call	SETC			; set color current pixel position
		call	RIGHTC			; RIGHTC
		pop	hl
		dec	l
		jr	nz,A18BB
		ret

; Subroutine GTASPC
GTASPC:
A18C7:		ld	hl,(ASPCT1)
		ex	de,hl
		ld	hl,(ASPCT2)
		ret

; Subroutine PNTINI (initialize for paint)
; Input:  A = border color
PNTINI:
A18CF:		push	af			; store color
		call	A15D9			; in graphic mode 2 ?
		jr	z,A18DB			; yep,
		pop	af			; restore color
		cp	15+1			; valid color ?
		ccf
		jr	A18E0
;
A18DB:		pop	af			; restore color
		ld	a,(ATRBYT)		; current color
		and	a			; color is valid
A18E0:		ld	(BRDATR),a		; update paint border color
		ret

; Subroutine SCANR
SCANR:
A18E4:		ld	hl,0
		ld	c,l
		call	A15D9			; in graphic mode 2 ?
		jr	nz,A1951		; nope, handle mode 3

		; SCANR mode 2
		ld	a,b
		ld	(FILNAM+0),a
		xor	a
		ld	(FILNAM+3),a
		ld	a,(BRDATR)
		ld	b,a
A18F9:		call	READC			; get color current pixel position
		cp	b			; is border color ?
		jr	nz,A190C		; nop,
		dec	de
		ld	a,d
		or	e			; skipped all ?
		ret	z			; yep, quit
		call	A16AC			; move pixel position right, if possible
		jr	nc,A18F9		; not out of range, cont
		ld	de,0
		ret
;
A190C:		call	A19AE
		push	de
		call	FETCHC			; fetch current pixel position
		ld	(CSAVEA),hl		; store VRAM address
		ld	(CSAVEM),a		; store mask
		ld	de,0
A191C:		inc	de
		call	A16AC			; move pixel position right, if possible
		jr	c,A192D			; out of range,
		call	READC			; get color current pixel position
		cp	b			; is border color ?
		jr	z,A192D			; yep,
		call	A19AE
		jr	A191C
;
A192D:		push	de
		call	FETCHC			; fetch current pixel position
		push	hl			; store VRAM address
		push	af			; store mask
		ld	hl,(CSAVEA)		; VRAM address
		ld	a,(CSAVEM)		; mask
		call	STOREC			; store current pixel position
		ex	de,hl
		ld	(FILNAM+1),hl
		ld	a,(FILNAM+0)
		and	a
		call	nz,NSETCX		; NSETCX
		pop	af			; restore mask
		pop	hl			; restore VRAM address
		call	STOREC			; store current pixel position
		pop	hl
		pop	de
		jp	A19A9

; SCANR mode 3
A1951:		call	A19C7
		jr	nc,A1963		; not at the border,
		dec	de
		ld	a,d
		or	e
		ret	z
		call	A16AC			; move pixel position right, if possible
		jr	nc,A1951		; not out of range, continue
		ld	de,0
		ret

A1963:		call	FETCHC			; fetch current pixel position
		ld	(CSAVEA),hl		; store VRAM address
		ld	(CSAVEM),a		; store mask
		ld	hl,0
A196F:		inc	hl
		call	A16AC			; move pixel position right, if possible
		ret	c			; out of range, quit
		call	A19C7
		jr	nc,A196F		; not at the border, next
		ret

; Subroutine SCANL
SCANL:
A197A:		ld	hl,0
		ld	c,l
		call	A15D9			; in graphic mode 2 ?
		jr	nz,A19BA		; nope, handle mode 3

		; SCANL mode 2
		xor	a
		ld	(FILNAM+3),a
		ld	a,(BRDATR)
		ld	b,a			; border color
A198B:		call	A16D8
		jr	c,A199F
		call	READC			; get color current pixel position
		cp	b			; at the border ?
		jr	z,A199C			; yep,
		call	A19AE
		inc	hl
		jr	A198B
;
A199C:		call	RIGHTC			; RIGHTC
A199F:		push	hl
		ld	de,(FILNAM+1)
		add	hl,de
		call	NSETCX			; NSETCX
		pop	hl
A19A9:		ld	a,(FILNAM+3)
		ld	c,a
		ret
;
A19AE:		push	hl
		ld	hl,ATRBYT
		cp	(hl)
		pop	hl
		ret	z
		inc	a
		ld	(FILNAM+3),a
		ret

; SCANL mode 3
A19BA:		call	A16D8
		ret	c
		call	A19C7
		jp	c,RIGHTC		; at the border, RIGHTC
		inc	hl
		jr	A19BA

; Subroutine 
A19C7:		call	READC			; get color current pixel position
		ld	b,a			; color at position
		ld	a,(BRDATR)
		sub	b			; at the border ?
		scf
		ret	z			; yep, quit
		ld	a,(ATRBYT)
		cp	b			; already the paint color ?
		ret	z			; yep, quit
		call	SETC			; set color current pixel position
		ld	c,1
		and	a			; clear Cx
		ret
