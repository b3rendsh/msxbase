; ------------------------------------------------------------------------------
; disk.asm
; HBDOS kernel functions
;
; (C) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		INCLUDE "base.inc"
		INCLUDE "hbmsx.inc"
		INCLUDE	"hbdos/hbdos.inc"

		SECTION	DISK

		PUBLIC	DiskCode
		PUBLIC	DiskSize
		PUBLIC	IplCode
		PUBLIC	IplSize
		PUBLIC	StartBasic
		PUBLIC	StartDos
		PUBLIC	DiskBasHandler
		PUBLIC	BasBsave
		PUBLIC	BasBload
		PUBLIC	DOS_CPMVER
		PUBLIC	DOS_GETDAT
		PUBLIC	DOS_SETDAT
		PUBLIC	DOS_GETTIM
		PUBLIC	DOS_SETTIM
		PUBLIC	DirDateTime
		
		; Defined in sys driver
		EXTERN	DSKIO
		EXTERN	DSKCHG
		EXTERN	GETDPB
		EXTERN	CHOICE
		EXTERN	DSKFMT
		EXTERN	MTOFF
		EXTERN	OEMSTA
		
		; Defined in kernel / sys
		EXTERN	DosMultiply
		EXTERN	MultiplyHigh
		EXTERN	DOS_OPEN
		EXTERN	DOS_CLOSE
		EXTERN	DOS_RENAME
		EXTERN	DOS_DELETE
		EXTERN	DOS_CREATE
		EXTERN	DOS_BLKRD
		EXTERN	DOS_BLKWRT
		EXTERN	DOS_SRCHFR
		EXTERN	DOS_SRCHNX
		EXTERN	DOS_GETEFA
		EXTERN	K_BDOS0
		EXTERN	SysBoot
		EXTERN	DosDiskIO
		
		; Defined in main
		EXTERN	HB_CALBAS
		EXTERN	HB_ENASYS
		EXTERN	HB_ENARAM
		EXTERN	HB_ENADISK
		
DiskCode:
		PHASE	DISKBASE		; 0x4000

; ------------------------------------------------------------------------------
; *** Disk system entry points ***
; ------------------------------------------------------------------------------

		db	"AB"			; ROM header not used
		dw	$0000			; init handler: see StartIpl in main module
		dw	BasCallHandler		; call statement handler
		dw	0
		dw	0
		defs	6,0			; reserved
		
; disk hardware driver entries
A4010:		jp	DSKIO
A4013:		jp	DSKCHG
A4016:		jp	GETDPB
A4019:		jp	CHOICE
A401C:		jp	DSKFMT
A401F:		jp	MTOFF

; kernel entries
; HBIOS: standard COMMAND.COM enables DOS (SDOSON) and calls BASIC and FORMAT entry points
; from page 3, custom command shells won't work if they do the call from page 0.
A4022:		jp	IplBasic		; jp A5B3A start Disk BASIC
A4025:		ret				; format disk not implemented
		defs	3,0
A4029:		ret				; stop all disks not implemented
		db	0
A402D:		ld	a,2			; master disk system is in slot 2
		ret
A4030:		ld	hl,(DOSHIM)		; MSX-DOS system bottom / lowest address used by the base MSX-DOS system
		ret

; Load the IPL code in page 2 and start BASIC or restart DOS

IplBasic:	ld	hl,BaseCode
		ld	de,IPLBASE
		ld	bc,IplSize
		ldir
		jp	StartBasic

IplDos:		ld	hl,BaseCode
		ld	de,IPLBASE
		ld	bc,IplSize
		ldir
		jp	RestartDos

; ------------------------------------------------------------------------------
; *** DOS kernel functions ***
; ------------------------------------------------------------------------------

; ---------------------------------------------------------
; Function $0C CPMVER
; ---------------------------------------------------------
DOS_CPMVER:	ld	b,$00			; machinetype 8080, plain CP/M
		ld	a,$22			; CP/M version 2.2
		ret

; ---------------------------------------------------------
; Function $2A GDATE
; Get date
; Output: hl = year 1980...2079
;         d  = month (1=Jan...12=Dec)
;         e  = date (1...31)
;         c  = day of week (0=Sun...6=Sat)
; ---------------------------------------------------------
DOS_GETDAT:	xor	a
		ld	(CPMCAL),a		; no CP/M call
		call	GetTimeStamp		; get time and date values
		ld	hl,(YEAR)
		ld	de,1980
		add	hl,de
		ld	de,(DAY)		; current day and month
		ld	a,(WEEKDA)
		ret

; ---------------------------------------------------------
; Function $2B SDATE
; Set date
; Input:  hl = year 1980...2079
;         d  = month (1=Jan...12=Dec)
;         e  = date (1...31)
; Output: a  = $00: valid date $be: invalid date
;         c  = $00: valid date $ff: invalid date
DOS_SETDAT:	ld	bc,-1980
		add	hl,bc
		jr	nc,_setdat3		; year <1980, error
		ld	a,h
		or	a
		jr	nz,_setdat3		; yearoffset not in 1 byte, error
		ld	a,l
		cp	120
		jr	nc,_setdat3		; year >2099, error
		call	SetFebDays		; setup february days
		inc	e
		dec	e
		jr	z,_setdat3		; day 0, error
		ld	a,d
		or	a
		jr	z,_setdat3		; month 0, error
		cp	12+1
		jr	nc,_setdat3		; month >12, error
		push	hl
		ld	hl,MONTAB-1
		add	a,l
		ld	l,a
		jr	nc,_setdat1
		inc	h
_setdat1:	ld	a,(hl)			; days in month
		pop	hl
		cp	e
		jr	c,_setdat3		; invalid day, error
		ld	(DAY),de		; current day and month
		call	_setdat4		; calculate days since 1-1-1980
		call	StoreDate		; store date (clockchip or otherwise)
_setdat2:	ld	bc,(DAYCNT)		; days since 1-1-1980
		ld	de,7
		inc	bc
		inc	bc
		call	DIV16			; divide
		ld	a,l
		ld	(WEEKDA),a
		xor	a
		ret

_setdat3:	ld	a,$ff
		ret

_setdat4:	ld	a,l			; year (offset)
		call	SaveYearFeb		; save year and setup days in february
		ld	c,l
		srl	c
		srl	c			; /4
		ld	b,0
		ld	de,4*365+1
		call	DosMultiply		; multiply
		ld	l,c
		ld	h,b
		ld	a,(YEAR)		; year (offset)
		and	$03
		add	a,a
		ld	de,TimeTab
		ld	b,0
		inc	a
		call	_setdat5
		ld	de,MONTAB
		ld	a,(MONTH)		; current month
		call	_setdat5
		ld	a,(DAY)			; current day
		dec	a
		ld	c,a
		add	hl,bc
		ld	(DAYCNT),hl		; days since 1-1-1980
		ret

_setdat5:	dec	a
		ret	z
		ex	de,hl
		ld	c,(hl)
		inc	hl
		ex	de,hl
		add	hl,bc
		jr	_setdat5

; ---------------------------------------------------------
; Function $2C GTIME
; Get time
; Output: h   = hours (0...23)
;         c,l = minutes (0...59)
;         d   = seconds (0...59)
;         e   = centiseconds (always zero)
; ---------------------------------------------------------
DOS_GETTIM:	xor	a
		ld	(CPMCAL),a		; no CP/M call
		call	GetTimeStamp		; get time and date values
		ld	h,b
		ld	l,c
		xor	a
		ret

; ---------------------------------------------------------
; Function $2D STIME
; Set time
; Input:  h = hours (0...23)
;         l = minutes (0...59)
;         d = seconds (0...59)
;         e = centiseconds (ignored)
; Output: a = $00: valid time $bd: invalid time
;         c = $00: valid time $ff: invalid time
; ---------------------------------------------------------
DOS_SETTIM:	ld	b,h
		ld	c,l
		ld	a,b
		cp	24
		jr	nc,_setdat3
		ld	a,59
		cp	c
		jr	c,_setdat3
		cp	d
		jr	c,_setdat3
		ld	a,e
		cp	100
		jr	nc,_setdat3
		call	StoreTime		; store time (clockchip or otherwise)
		xor	a
		ret

; ------------------------------------------------------------------------------
; DOS kernel subroutines
; ------------------------------------------------------------------------------

; ---------------------------------------------------------
; Use HBIOS low level RTC subroutines
; TIMEBUF is BCD year,month,day,hours,minutes,seconds
; Note1: HBIOS time functions set/get both date and time
; Note2: the MSX clock uses base year 1980
; ---------------------------------------------------------

; Subroutine store date
StoreDate:	ld	(CURDAT),hl
		call	hbGetTime		; get current date+time
		ld	a,(YEAR)
		call	SetTimePos		; set year
		ld	a,(MONTH)
		call	SetTimePos		; set month
		ld	a,(DAY)
		call	SetTimePos		; set day
		call	hbSetTime		; set date and time

; Subroutine store time
; Input: b=hours,c=minutes,d=seconds
StoreTime:	push	bc
		push	de
		call	hbGetTime		; get current date+time
		pop	de
		pop	bc
		inc	hl			; skip year
		inc	hl			; skip month
		inc	hl			; skip day
		ld	a,b
		call	SetTimePos		; set hours
		ld	a,c
		call	SetTimePos		; set minutes
		ld	a,d
		call	SetTimePos		; set seconds
		; fall through to set date and time

; Set RTC date+time
hbSetTime:	ld	b,BF_RTCSETTIM
		ld	hl,TIMEBUF
		; Adjust for base year 1980
		ld	a,(hl)
		sub	$20			; offset
		daa				; BCD
		ld	(hl),a
		push	hl
		call	MSX_HBINVOKE
		pop	hl
		ret

; Get RTC date+time
hbGetTime:	ld	b,BF_RTCGETTIM
		ld	hl,TIMEBUF
		push	hl
		call	MSX_HBINVOKE
		pop	hl
		; Adjust for base year 1980
		ld	a,(hl)
		add	a,$20			; offset
		daa				; BCD
		ld	(hl),a
		ret

; Get TIMEBUF entry in binary format
GetTimePos:	ld	a,(hl)			; get value in current buffer position
		inc	hl			; increase buffer position
		push	bc
		ld	b,a			; save bcd value
		and	$f0			; upper nibble
		rrca
		ld	c,a			; c = upper nibble * 8
		rrca
		rrca				; a = upper nibble * 2
		add	a,c
		ld	c,a 			; c = upper nibble * 10
		ld	a,b			; load bcd value
		and 	$0f			; lower nibble
		add	a,c			; add to binary upper nibble
		pop	bc
		ret

; Set TIMEBUF entry in binary format
SetTimePos:	push	bc
		ld	bc,$000a		; b=base-10 counter, c=base-10 divisor
@r01:		sub	c			; a = a-10
		jr	c,@r02			; if cx then remainder
		inc	b			; increment base-10 counter
		jr	@r01
@r02:		add	a,c			; a = a+10
		push	af			; save remainder
		ld	a,b			; base-10 counter
		rlca				; shift to upper nibble
		rlca
		rlca
		rlca
		ld	b,a			; b = tens digit
		pop	af
		or	b			; a = tens + remainder
		pop	bc
		ld	(hl),a			; save in current buffer position
		inc	hl			; increase buffer position
		ret

; Subroutine get date and time
; Output: cx set if from clockchip
GetDateTime:	call	hbGetTime		; get RTC date+time
		call	GetTimePos		; get year
		call	SaveYearFeb		; save year and setup days in february
		call	GetTimePos		; get month
		ld	(MONTH),a
		call	GetTimePos		; get day
		ld	(DAY),a
		call	GetTimePos		; get hours
		ld	b,a
		call	GetTimePos		; get minutes
		ld	c,a
		call	GetTimePos		; get seconds
		ld	d,a
		ld	e,0			; centiseconds always 0
		scf
		ret

; ---------------------------------------------------------
; Get date and time in directory format
; ---------------------------------------------------------
DirDateTime:	call	GetTimeStamp		; get time and date values
		ld	a,c
		add	a,a
		add	a,a
		add	a,a
		rl	b
		add	a,a
		rl	b
		add	a,a
		rl	b
		srl	d
		or	d
		ld	e,a
		ld	d,b
		ld	bc,(MONTH)
		ld	a,c
		add	a,a
		add	a,a
		add	a,a
		add	a,a
		add	a,a
		rl	b
		ld	c,a
		ld	a,(DAY)			; current day
		or	c
		ld	c,a
		ret

GetTimeStamp:	call	GetDateTime		; get date and time from HBIOS
		push	bc
		push	de
		ld	hl,(YEAR)
		call	_setdat4		; calculate days since 1-1-1980
		call	_setdat2
		pop	de
		pop	bc
		ret

SaveYearFeb:	ld	(YEAR),a		; year (offset)
SetFebDays:	and	$03
		ld	a,28
		jr	nz,@r01
		inc	a
@r01:		ld	(MONTAB+1),a
		ret

TimeTab:	db	200,166,200,165,200,165,200,165

; ------------------------------------------------------------------------------
; *** Disk BASIC ***
; ------------------------------------------------------------------------------

DiskBasHandler:	ex	(sp),hl
		ld	(basret+1),hl		; get caller return address
		ex	(sp),hl
		ld	(hlsav),hl		; save hl
		ld	hl,HB_ENASYS		; first enable main rom then return to caller
		push	hl
		push	af
		push	de
basret:		ld	de,$0000		; return address

		; dispatcher
		ld	hl,HookTab
@r01:		ld	a,(hl)
		or	a			; end of table?
		jr	z,@r04			; z=yes
		inc	hl
		cp	e			; caller address low byte?
		jr	nz,@r02			; nz=no, next address
		ld	a,(hl)
		cp	d			; caller address high byte?
		jr	z,@r03			; z=yes, address found
@r02:		inc	hl
		inc	hl
		inc	hl
		jr	@r01			; next

		; caller found, jump to subroutine
@r03:		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)			; de = subroutine address
		ex	de,hl
		pop	de
		pop	af
		push	hl			; put subroutine on stack
		ld	hl,(hlsav)		; restore hl
		ret				; execute subroutine

		; internal error, return to caller
@r04:		pop	de
		pop	af
		ld	hl,(hlsav)
		scf				; set error flag
		ret

hlsav:		dw	$0000

HookTab:	dw	M_NAME+3,BAS_NAME	; rename file
		dw	M_KILL+3,BAS_KILL	; delete file
		dw	M_COPY+3,BAS_COPY	; copy file
		dw	M_DSKF+3,BAS_DSKF	; free disk space (clusters)
		dw	M_LSET+3,BAS_LSET	; string function for file record handling
		dw	M_RSET+3,BAS_RSET	; "
		dw	M_FIEL+3,BAS_FIEL	; "
		dw	M_MKIS+3,BAS_MKIS	; "
		dw	M_MKSS+3,BAS_MKSS	; "
		dw	M_MKDS+3,BAS_MKDS	; "
		dw	M_CVI+3,BAS_CVI		; "
		dw	M_CVS+3,BAS_CVS		; "
		dw	M_CVD+3,BAS_CVD		; "
		dw	M_GETP+3,BAS_GETP	; get file pointer
		dw	M_NOFO+3,BAS_NOFO	; no for clause
		dw	M_NULO+3,BAS_NULO	; null file open
		dw	M_NTFL+3,BAS_NTFL	; not file number 0
		dw	M_BINS+3,BAS_BINS	; binary save file
		dw	M_BINL+3,BAS_BINL	; binary load file
		dw	M_FILE+3,BAS_FILE	; list files in directory
		dw	M_DGET+3,BAS_DGET	; read record into buffer
		dw	M_FILO+3,BAS_FILO	; file output
		dw	M_INDS+3,BAS_INDS	; disk attribute input
		dw	M_LOC+3,BAS_LOC		; record number
		dw	M_LOF+3,BAS_LOF		; file size
		dw	M_EOF+3,BAS_EOF		; end of file
		dw	M_BAKU+3,BAS_BAKU	; backup character internal command
		dw	M_PARD+3,BAS_PARD	; device name parser
		dw	M_NODE+3,BAS_NODE	; no device
		dw	M_ERRP+3,BAS_ERRP	; error pointer
		dw	M_PHYD+3,DosDiskIO	; disk i/o
		db	0
		
; ------------------------------------------------------------------------------
; Extension ROM call handler
; HBIOS: if the call handler is called it means that Disk BASIC is active
; ------------------------------------------------------------------------------
BasCallHandler:	ei
		push	hl
		ld	hl,StatementTab
@r01:		ld	de,PROCNM
@r02:		ld	a,(de)
		cp	(hl)
		jr	nz,@r03
		inc	de
		inc	hl
		and	a
		jr	nz,@r02
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		ex	(sp),hl
		call	GetChar
		scf
		ccf
		ret

@r03:		ld	a,(hl)
		and	a
		inc	hl
		jr	nz,@r03
		inc	hl
		inc	hl
		ld	a,(hl)
		and	a
		jr	nz,@r01
@r04:		pop	hl
		jp	OEMSTA			; custom disk driver call statements (not used)

StatementTab:	db	"SYSTEM",0		; go to DOS mode / command prompt
		dw	CALL_SYSTEM
		db	"REBOOT",0		; reboot RomWBW HBIOS
		dw	CALL_REBOOT
		db	0

CALL_SYSTEM:	ret	nz
		ld	a,(DOSFLG)
		and	a
		jp	z,Error11
		ld	ix,CLSALL		; close all i/o channels
		call	CallBasic
		ld	ix,TOTEXT		; set text mode
		call	CallBasic
		ld	ix,ERAFNK		; erase function key display
		call	CallBasic
		jp	IplDos			; start DOS

; ------------------------------------------------------------------------------
; HBIOS custom call statements
; ------------------------------------------------------------------------------

; Cold boot RomWBW HBIOS (warm boot won't work)
; Copy of custom IPL routine in main BASIC
CALL_REBOOT:	di
		ld	b,BF_SYSRESET
		ld	c,BF_SYSRES_COLD
		call	HB_INVOKE		; never return
		jr	$			

; ------------------------------------------------------------------------------
; Disk BASIC subroutines
; ------------------------------------------------------------------------------

SetDrive:	ld	a,d
SetDrive1:	dec	a
		ret	p
		ld	a,(CURDRV)
		ret

CheckOpen:	push	hl
		push	de
		push	bc
		call	SetDrive		; convert to driveid
		ld	c,a
		ld	hl,(FILTAB)		; I/O channel pointer table
		ld	a,(MAXFIL)		; number of I/O channels
NextChannel:	push	af
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		push	hl
		ex	de,hl			; pointer to I/O channel
		ld	a,(hl)
		and	a			; channel in use ?
		jr	z,CheckNext		; nope, check next
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		ld	a,(hl)
		cp	9			; channel in use by a disk device ?
		jr	nc,CheckNext		; nope, check next
		dec	hl
		dec	hl
		ld	d,(hl)
		dec	hl
		ld	e,(hl)			; pointer to FCB
		ld	a,(de)			; DR byte of FCB
		call	SetDrive1		; convert to driveid
		cp	c			; same as the requested one ?
		jr	nz,CheckNext		; nope, check next
		inc	de
		ex	de,hl
		ld	de,FILNAM
		ld	b,11
@r01:		ld	a,(de)
		cp	'?'
		jr	z,@r02			; wildcard, treat as equal
		cp	(hl)
		jr	nz,CheckNext		; filename not equal, check next
@r02:		inc	de
		inc	hl
		djnz	@r01			; next char
		pop	hl
		pop	hl
		jr	Reg3Return		; quit with Zx set (file open)

CheckNext:	pop	hl
		pop	af
		dec	a
		jp	p,NextChannel
		jr	Reg3Return		; quit with Zx reset (file not open)

SearchFile:	push	hl
		push	de
		push	bc
		ld	a,d
		ld	(TMPBUF+0),a
		call	CopyFileName		; copy FILNAM to FCB
		call	SearchFile1		; search file
		jr	Reg3Return

SearchFile1:	ld	hl,BUF+84
		ld	(DMAADD),hl		; transferaddress in BUF
		ld	de,TMPBUF
		xor	a
		ld	(TMPBUF+12),a		; clear EX byte of FCB
		call	DOS_SRCHFR		; search for first
		inc	a
		ret

CopyFileName:	ld	de,TMPBUF+1
CopyFileName1:	ld	hl,FILNAM
		ld	bc,11
		ldir
		ret

; Subroutine take control from caller (move parameters on stack)
; Input:  IX  = return address replacement
;         IYH = number of bytes to move
;
; HBIOS: custom stack setup at entry:
; +0	return address StackControl caller
; +2	address HB_ENASYS subroutine
; +4	return address Disk BASIC caller

StackControl:	ei
		push	hl
		push	de
		push	bc
		push	af
		ld	hl,12			; Stack position (16+2)
		add	hl,sp
		push	ix
		pop	bc
		ld	(hl),c
		inc	hl
		ld	(hl),b
		ld	hl,10			; Stack position offset
		add	hl,sp
		ex	de,hl
		jr	@r03

@r01:		push	iy
		pop	bc
@r02:		ld	c,(hl)
		ld	a,(de)
		ld	(hl),a
		ld	a,c
		ld	(de),a
		inc	hl
		inc	de
		djnz	@r02
@r03:		ld	hl,14			; Stack position (18+2)
		add	hl,sp
		ld	a,e
		sub	l
		ld	a,d
		sbc	a,h
		jr	c,@r01
Reg4Return:	pop	af
Reg3Return:	pop	bc
		pop	de
		pop	hl
		ret

; ---------------------------------------------------------
; Hook GETP: get pointer to i/o channel
; ---------------------------------------------------------
BAS_GETP:	ld	ix,RETRTN
		ld	iy,$0200
		call	StackControl
		pop	hl			; restore pointer to i/o channel
		ld	a,(hl)			; i/o channel mode
		and	a			; zx = i/o channel open
		ret

; ---------------------------------------------------------
; Hook NOFO: open without for
; ---------------------------------------------------------
BAS_NOFO:	ei
		ld	bc,256
		ld	(RECSIZE),bc		; default recordsize = 256
		call	GetChar			; get BASIC character
		ld	a,e
		ret	z
		push	af
		push	hl			; store BASIC pointer
		ld	hl,8			; Stack position (12+2)
		add	hl,sp
		ld	a,(hl)
		cp	4			; i/o channel mode = random i/o ?
		jp	nz,Error02		; nz=no, syntax error
		inc	hl
		ld	a,(hl)			; device code
		cp	$09			; disk drive ?
		jp	nc,Error02		; nc=no, syntax error
		pop	hl			; restore BASIC pointer
		call	CheckChar
		db	$ff			; function token
		call	CheckChar
		db	$92			; LEN token
		call	CheckChar
		db	$ef			; = token
		ld	ix,INTID2
		call	CallBasic		; evaluate word operand and check for 0-32767 range
		dec	de
		inc	d
		dec	d			; 0 or > 256 ?
		jp	nz,Error11		; nz=yes, illegal function call error
		inc	de
		ld	(RECSIZE),de		; store record size
		pop	af
		ret

; ---------------------------------------------------------
; Hook NULO: open i/o channel
; Input:  HL = i/o channel pointer
; ---------------------------------------------------------
BAS_NULO:	ei
		ret	nc			; not for a disk device, return control
		ld	ix,RETRTN
		ld	iy,$0400
		call	StackControl
		call	ValFile2		; validate file name (wildcard not allowed)
		call	CheckOpen		; is file already open in one of the I/O channels ?
		jp	z,Error54		; z=yes, file already open error
		ld	(PTRFIL),hl		; interpreter input/output device = i/o channel pointer
		ld	a,e
		cp	4			; file mode = random i/o ?
		jr	z,@r01			; z=yes, recordsize already set
		ld	bc,1
		ld	(RECSIZE),bc		; all others use recordsize 1
@r01:		pop	af
		push	af			; I/O channel number
		push	hl
		push	de
		ld	hl,FILMAX
		cp	(hl)			; do I have a FCB for this i/o channel ?
		jp	nc,Error52		; nope, bad file number error
		ld	bc,37
		ld	e,a
		ld	d,b
		ld	hl,(FCBBAS)		; base of the Disk BASIC i/o channel FCBs
		call	MultiplyHigh		; base + 37 * channelnumber
		xor	a
		ld	hl,12
		add	hl,bc
		ld	(hl),a			; reset EX byte of FCB
		pop	de
		pop	hl
		inc	hl
		ld	(hl),c
		inc	hl
		ld	(hl),b			; pointer to FCB
		inc	hl
		ld	(hl),a			; no backup char
		inc	hl
		ld	(hl),d			; device number
		inc	hl
		inc	hl
		ld	(hl),a			; current bufferoffset = 0
		call	SearchFile		; search file
		push	bc
		push	de
		ld	a,d
		ld	(bc),a			; device number
		ld	e,c
		ld	d,b
		inc	de
		call	CopyFileName1		; copy FILNAM to FCB
		pop	de
		pop	bc
		ld	a,e
		jr	nz,@r04			; file exists,
		and	$86			; file not exists and not binsav, random or output mode ?
		jp	z,Error53		; z=yes, file not found error
@r02:		push	de
		push	bc
		ld	e,c
		ld	d,b
		call	DOS_CREATE		; create file
		and	a
		jp	nz,Error67		; failed, error
		pop	hl
		call	SetupFCB		; setup FCB fields
		pop	de
		ld	hl,(PTRFIL)		; i/o channel pointer
		ld	(hl),e			; filemode, I/O channel open
@r03:		pop	af
		pop	hl
		ret

@r04:		cp	8
		jr	z,@r06			; append mode,
		cp	2
		jr	z,@r02			; output mode, create file (overwrites!)
		cp	$80
		jr	z,@r02			; binsav mode, create file (overwrites!)
		push	de
		push	bc
		ld	e,c
		ld	d,b
		call	DOS_OPEN		; open fcb
		pop	hl
		call	SetupFCB		; setup FCB fields
		pop	de
		ld	hl,(PTRFIL)		; i/o channel pointer
		ld	(hl),e			; filemode, I/O channel open
		ld	a,e
		cp	4			; random mode ?
		jr	z,@r03			; yep, quit
		push	hl
		ld	hl,FLBMEM
		xor	a
		cp	(hl)			; in raw mode ?
		ld	(hl),a
		pop	hl
		jr	nz,@r03			; nz=yes, quit
		ld	bc,6
		add	hl,bc
		push	hl
		ld	(hl),$ff		; position 255, so next get char fills buffer
		ld	hl,(PTRFIL)		; i/o channel pointer
		call	ReadCharIO		; get char from I/O channel
		pop	hl
		dec	hl
		dec	hl
		dec	hl
		ld	(hl),a
		cp	$ff
		jr	nz,@r03			; nz=quit
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		ld	(hl),$80
@r05:		jr	@r03			; quit

@r06:		push	bc
		ld	e,c
		ld	d,b
		call	DOS_OPEN		; open fcb
		pop	hl
		push	hl
		call	SetupFCB		; setup FCB fields
		ld	hl,(PTRFIL)		; i/o channel pointer
		ld	(hl),1			; first in sequential input mode
		ld	bc,6
		add	hl,bc
		ld	(hl),$ff		; position 255, so next get char fills buffer
		ld	hl,(PTRFIL)		; i/o channel pointer
@r07:		push	hl
		call	ReadCharIO		; get char from I/O channel
		pop	hl
		jr	nc,@r07			; not at the end of file, continue
		ld	(hl),2			; continue in sequential output mode
		pop	hl
		ld	bc,$0021
		add	hl,bc			; to the Rx field
		ld	c,$04
		push	hl
		scf
@r08:		ld	a,(hl)
		sbc	a,b
		ld	(hl),a
		inc	hl
		dec	c
		jr	nz,@r08			; decrease by 1
		pop	hl
		inc	c
		jr	nc,@r09			; Make Rx a multiply of 256
		ld	c,$04			; Rx = 0
@r09:		call	SetupFCB1
		jr	@r05			; quit

SetupFCB:	ld	bc,12
		add	hl,bc
		ld	(hl),b			; clear EX byte
		inc	hl
		ld	(hl),b			; clear S1 byte
		inc	hl
		ld	bc,(RECSIZE)
		ld	(hl),c
		inc	hl
		ld	(hl),b			; user recordsize = recordsize
		ld	bc,17
		add	hl,bc
		ld	c,5
SetupFCB1:	ld	(hl),b
		inc	hl
		dec	c
		jr	nz,SetupFCB1		; clear CR byte and Rx bytes
		ret

; ---------------------------------------------------------
; Hook INDS: read character from i/o channel
; ---------------------------------------------------------
BAS_INDS:	ld	ix,RETRTN
		ld	iy,$0600
		call	StackControl
		call	ReadCharIO		; read character from i/o channel
		jp	Reg3Return		; restore registers and quit

; Subroutine read character from i/o channel
ReadCharIO:	push	hl
		ld	a,(hl)
		cp	1			; input mode ?
		jp	nz,Error61		; nope, bad file mode error
		ld	e,l
		ld	d,h
		inc	hl
		inc	hl
		inc	hl
		ld	a,(hl)
		and	a			; backup char ?
		jr	nz,@r02			; nz=yes, use that
		inc	hl
		inc	hl
		inc	hl
		inc	(hl)			; update counter
		ld	a,(hl)
		inc	hl
		inc	hl
		inc	hl			; to the buffer
		jr	nz,@r01			; still characters left, use them
		push	hl
		ld	(DMAADD),hl		; transferaddress is I/O channel buffer
		ex	de,hl
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)			; pointer to FCB
		ld	hl,256
		call	DOS_BLKRD		; random block read
		ld	e,l
		ld	d,h
		dec	h
		ld	a,l
		or	h			; have read 256 records ?
		pop	hl
		jr	z,@r01			; z=yes, then not at end of file
		push	hl
		add	hl,de
		ld	(hl),$1a		; put a CTRL-Z at the end
		pop	hl
		xor	a
@r01:		ld	c,a
		ld	b,0
		add	hl,bc
		ld	a,(hl)			; get char
@r02:		ld	b,a
		sub	$1a
		sub	$01
		ld	a,b
		pop	hl
		inc	hl
		inc	hl
		inc	hl
		ld	(hl),0			; no backup char
		ret	nc			; no CTRL-Z, quit
		ld	(hl),a			; CTRL-Z as backup char, so it is always read again
		ret

; ---------------------------------------------------------
; Hook BAKU: backup character to i/o channel
; ---------------------------------------------------------
BAS_BAKU:	ei
		push	hl
		ld	hl,4			; Stack position (8+2)
		add	hl,sp
		ld	(hl),NOSKCR % 256
		inc	hl
		ld	(hl),NOSKCR / 256	; resume character putback
		pop	hl
		inc	hl
		inc	hl
		inc	hl
		ld	(hl),c
		ret

; ---------------------------------------------------------
; Hook FILO: write character to i/o channel
; ---------------------------------------------------------
BAS_FILO:	ld	ix,RETRTN
		ld	iy,$0800
		call	StackControl
		ld	a,(hl)
		cp	2			; i/o channel mode = sequential output ?
		jp	nz,Error61		; nz=no, bad file mode error
		pop	af
		push	af
		call	WriteCharIO		; write char to I/O channel
		jp	Reg4Return		; quit

; Subroutine write character to i/o channel
WriteCharIO:	push	hl
		ld	bc,6
		add	hl,bc			; +6
		ld	c,(hl)			; position in buffer
		inc	(hl)			; update
		inc	hl
		inc	hl
		inc	hl			; +9
		add	hl,bc			; pointer in buffer
		ld	(hl),a			; put char in buffer
		pop	hl
		ret	nz			; buffer not full, quit
		
; Subroutine flush i/o channel buffer
FlushIO:	push	hl
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)			; pointer to FCB
		ld	bc,4
		add	hl,bc
		ld	a,(hl)			; position
		inc	hl
		inc	hl
		inc	hl
		ld	(DMAADD),hl		; transferaddress
		and	a
		ld	l,a
		ld	h,b
		jr	nz,@r01			; not a complete buffer, only the used part
		inc	h			; 256
@r01:		call	RndBlockWrite1		; random block write
		pop	hl
		ret

; ---------------------------------------------------------
; Hook NTFL: close i/o channel for disk devices
; ---------------------------------------------------------
BAS_NTFL:	ld	ix,RETRTN
		ld	iy,$0400
		call	StackControl
		pop	hl
		ld	a,(hl)
		sub	2			; i/o channel mode = sequential output ?
		jr	nz,@r01			; nz=no, skip EOF
		push	hl
		ld	hl,FLBMEM
		cp	(hl)			; i/o channel in raw mode ?
		ld	(hl),a			; i/o channel mode = ascii
		pop	hl
		jr	nz,@r01			; nz=yes, skip CTRL-Z
		ld	(hl),4			; switch to random mode
		ld	a,$1a			; CTRL-Z
		call	WriteCharIO		; write char to I/O channel
		call	nz,FlushIO		; buffer not full/empty, write remaining I/O channel buffer
@r01:		push	hl
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)			; pointer to FCB
		call	DOS_CLOSE		; close fcb
		pop	hl
		push	hl
		ld	de,7
		add	hl,de
		ld	(hl),d			; clear i/o channel flags
		ld	l,d
		ld	h,d
		ld	(PTRFIL),hl		; interpreter input/output device = keyboard/screen
		pop	hl
		inc	(hl)
		dec	(hl)
		ld	(hl),d
		pop	hl
		ret

; ---------------------------------------------------------
; Hook BINS: binary save
; ---------------------------------------------------------
BAS_BINS:	call	TakeControl		; take control from caller
		push	hl
		ld	ix,SCCPTR
		call	CallBasic		; convert linepointers to linenumbers
		ld	a,$ff
		call	WriteByteFCB		; write byte to FCB (binary basicfile id)
		ld	hl,(TXTTAB)
		ld	(DMAADD),hl		; transfer address
		ex	de,hl
		ld	hl,(VARTAB)
		and	a
		sbc	hl,de			; size of BASIC program
		call	RndBlockWrite		; random block write to FCB 0
		ld	(NLONLY),a		; not loading basic program, close i/o channels when requested
		pop	hl
		ld	ix,CLSFIL
		jp	CallBasic		; close i/o channel

; ---------------------------------------------------------
; Hook BINL: binary load
; ---------------------------------------------------------
BAS_BINL:	ld	ix,M739A		; quit loading & start (headloop/executing)
		ld	iy,$0200
		call	StackControl
		pop	af			; MERGE statement ?
		jp	z,Error61		; yep, bad file mode error
		ld	ix,CLSALL
		call	CallBasic		; close all i/o channels
		ld	hl,(FCBBAS)		; FCB for i/o channel 0
		push	hl
		call	SetupFCB		; setup FCB fields
		pop	hl
		push	hl
		ld	bc,16+3
		add	hl,bc
		ld	a,(hl)
		and	a			; file size >= 167772156 bytes ?
		jp	nz,Error07		; yep, out of memory error
		dec	hl
		or	(hl)			; file size >= 65536 bytes ?
		jp	nz,Error07		; yep, out of memory error
		dec	hl
		ld	d,(hl)
		dec	hl
		ld	e,(hl)			; file size
		ld	hl,(TXTTAB)
		add	hl,de			; does in fit in BASIC program area ?
		jp	c,Error07		; nope, out of memory error
		ld	bc,$0093
		add	hl,bc			; does it leave room for stack space ?
		jp	c,Error07		; nope, out of memory error
		sbc	hl,sp			; does it fit in memory ?
		jp	nc,Error07		; nope, out of memory error
		ex	de,hl
		ex	(sp),hl
		ex	de,hl			; save file size, get FCB for i/o channel 0
		call	ReadByteFCB		; read byte from FCB
		ld	hl,(TXTTAB)
		ld	(DMAADD),hl		; transferaddresss
		pop	hl
		dec	hl			; file size -1
		call	DOS_BLKRD		; random block read
		ld	de,(TXTTAB)
		add	hl,de
		ld	(VARTAB),hl		; setup start of BASIC variable area
		ld	ix,LINKER
		call	CallBasic		; recalculate linepointers
		ld	a,(FILNAM+0)
		and	a			; RUN after LOAD ?
		ret	nz			; nope, quit
		ld	(NLONLY),a		; not loading basic program, close i/o channels when requested
		ld	hl,CmdRun
		ld	de,TMPBUF
		ld	bc,5
		push	de
		ldir
		pop	hl			; basicpointer
		ld	ix,NEWSTT
		jp	CallBasic		; continue in the execution loop

CmdRun:		db	':',$92,0,0,0		; :RUN
		db	0			; end of line
		dw	0			; end of program

WriteWordFCB:	push	hl
		ld	a,l
		call	writebyte		; write byte to FCB
		pop	hl
		ld	a,h
		jr	writebyte		; write byte to FCB

WriteByteFCB:	ld	de,(FCBBAS)		; FCB for i/o channel 0
writebyte:	ld	bc,DOS_BLKWRT		; random block write
		jr	readbyte

ReadByteFCB:	ld	bc,DOS_BLKRD		; random block read
readbyte:	push	af
		ld	hl,1
		add	hl,sp
		ld	(DMAADD),hl		; transferaddress
		push	de
		call	jp_bc
		pop	de
		pop	af
		ret

jp_bc:		ld	hl,$0001
		push	bc
		ret

; ---------------------------------------------------------
; *** Command BSAVE ***
; ---------------------------------------------------------
BasBsave:	push	de
		call	LoadSave2
		ld	(SAVENT),de
		push	de
		call	LoadSave2
		ld	(SAVEND),de
		ex	(sp),hl
		ex	de,hl
		call	DCOMPR
		jp	c,Error11
		ex	de,hl
		ex	(sp),hl
		call	GetChar			; at end of statement ?
		scf
		jr	z,@r02			; yep,
		call	CheckChar
		db	','			; check for ','
		cp	'S'
		jr	nz,@r01
		call	GetNextChar		; get basic character
		and	a
		jr	@r02

@r01:		call	LoadSave3
		ld	(SAVENT),de
		scf
@r02:		pop	bc
		jr	nc,@r03
		inc	b
		dec	b
		jp	p,Error11
@r03:		pop	de
		push	hl
		push	bc
		push	af
		xor	a			; i/o channel 0
		ld	e,2			; output mode
		ld	ix,OPNFIL
		call	CallBasic		; open i/o channel
		ld	a,0FEH
		call	WriteByteFCB		; write byte to FCB (bsave file id)
		pop	af
		pop	hl
		push	hl
		push	af
		call	WriteWordFCB
		ld	hl,(SAVEND)
		call	WriteWordFCB
		ld	hl,(SAVENT)
		call	WriteWordFCB
		pop	af
		pop	bc
		push	af
		ld	(DMAADD),bc		; transfer address
		ld	hl,(SAVEND)
		and	a
		sbc	hl,bc
		inc	hl
		pop	af
		jr	nc,@r05
		call	RndBlockWrite		; random block write to FCB 0
@r04:		ld	a,0FFH
		ld	(FLBMEM),a		; raw mode
		xor	a
		ld	ix,CLSFIL
		call	CallBasic		; close i/o channel
		jp	FinPrt			; output back to screen and quit

@r05:		call	Bloadsave
@r06:		push	hl
		ld	de,(SAVENT)
		call	DCOMPR
		push	af
		ld	c,l
		ld	b,h
		ld	hl,(SAVEND)
		push	hl
		add	hl,bc
		ld	(SAVEND),hl
		pop	hl
		ld	de,(DMAADD)		; transfer address
		call	LDIRMV
		pop	af
		jr	nc,@r07
		pop	hl
		push	hl
		call	RndBlockWrite		; random block write to FCB 0
		ld	hl,(SAVENT)
		pop	de
		and	a
		sbc	hl,de
		ld	(SAVENT),hl
		ex	de,hl
		jr	@r06

@r07:		pop	hl
		ld	hl,(SAVENT)
		call	RndBlockWrite		; random block write to FCB 0
		jr	@r04

; ---------------------------------------------------------
; *** Command BLOAD ***
; ---------------------------------------------------------
BasBload:	push	de
		xor	a
		ld	(RUNBNF),a		; assume no autostart, no vram
		ld	c,a
		ld	b,a			; assume offset 0
		call	GetChar			; at end of statement ?
		jr	z,@r03			; yep,
		call	CheckChar
		db	','			; check for ','
		cp	'R'			; autorun specified ?
		jr	z,@r01			; yep, set autorun
		cp	'S'			; vram specified ?
		jr	nz,@r02			; nope, then it must be a offset
@r01:		ld	(RUNBNF),a
		call	GetNextChar		; get basic character
		jr	z,@r03			; end of statement, skip offset
		call	CheckChar
		db	','			; check for ','
@r02:		call	LoadSave3
		ld	b,d
		ld	c,e			; offset
@r03:		pop	de
		push	hl
		push	bc
		ld	a,0FFH
		ld	(FLBMEM),a		; raw mode
		xor	a			; i/o channel 0
		ld	e,1			; input mode
		ld	ix,OPNFIL
		call	CallBasic		; open i/o channel
		ld	de,(FCBBAS)		; FCB for i/o channel 0
		call	ReadByteFCB		; read byte from FCB
		cp	0FEH
		jp	nz,Error61		; nope, bad file mode error
		pop	bc
		call	LoadSave1		; read word from FCB and add offset
		push	hl
		call	LoadSave1		; read word from FCB and add offset
		push	hl
		call	LoadSave1		; read word from FCB and add offset
		ld	(SAVENT),hl
		pop	hl
		pop	bc
		and	a
		sbc	hl,bc			; end address - start address
		inc	hl
		ld	(DMAADD),bc		; transferaddress
		ld	a,(RUNBNF)
		cp	'S'			; vram load ?
		jr	z,@r05			; yep,
		call	DOS_BLKRD		; random block read
@r04:		ld	ix,FINPRT
		call	CallBasic		; output back to screen
		pop	hl
		ret

@r05:		call	Bloadsave
@r06:		push	hl
		ld	de,(SAVENT)
		call	DCOMPR
		push	af
		ld	de,(FCBBAS)		; FCB for i/o channel 0
		call	DOS_BLKRD		; random block read
		pop	af
		pop	bc
		push	bc
		push	af
		ld	hl,(SAVEND)
		push	hl
		add	hl,bc
		ld	(SAVEND),hl
		pop	de
		ld	hl,(DMAADD)		; transferaddress
		pop	af
		jr	nc,@r07
		call	LDIRVM
		ld	hl,(SAVENT)
		pop	de
		and	a
		sbc	hl,de
		ld	(SAVENT),hl
		ex	de,hl
		jr	@r06

@r07:		pop	bc
		ld	bc,(SAVENT)
		call	LDIRVM
		xor	a
		ld	(RUNBNF),a
		jr	@r04

; Subroutine read word from file handle, adjust with offset
LoadSave1:	push	bc
		call	ReadByteFCB		; read byte from FCB
		push	af
		call	ReadByteFCB		; read byte from FCB
		ld	h,a
		pop	af
		ld	l,a
		pop	bc
		add	hl,bc
		ret

; Subroutine check for ',' and evaluate address operand
LoadSave2:	call	CheckChar
		db	','			; check for ','

; Subroutine evaluate address operand
LoadSave3:	ld	ix,M6F0B
		jp	CallBasic		; evaluate address operand (BLOAD/SAVE)


; ---------------------------------------------------------
; Hook DGET: GET/PUT statement
; ---------------------------------------------------------
BAS_DGET:	ld	ix,RETRTN
		ld	iy,$0400
		call	StackControl
		ld	a,(hl)
		cp	4			; random mode ?
		jp	nz,Error61		; nope, bad file mode error
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)			; pointer to FCB
		ld	bc,9-2
		add	hl,bc			; i/o channel buffer
		ex	(sp),hl
		call	GetChar			; at end of statement ?
		jr	z,@r02			; z=yes
		push	de
		call	CheckChar
		db	','			; check for ','
		ld	ix,FRMEVL
		call	CallBasic		; evaluate expression
		push	hl
		call	dget_sub
		ld	a,c
		or	b
		or	l
		or	h
		jp	z,Error11
		ld	a,c
		or	b
		dec	bc
		jr	nz,@r01
		dec	hl
@r01:		ex	de,hl
		pop	hl
		ex	(sp),hl
		push	hl
		push	de
		ld	de,$0021
		add	hl,de
		pop	de
		ld	(hl),c
		inc	hl
		ld	(hl),b
		inc	hl
		ld	(hl),e
		inc	hl
		ld	(hl),d
		pop	de
		pop	hl
@r02:		ex	(sp),hl
		ld	(DMAADD),hl		; transferaddress
		pop	hl
		pop	af
		push	hl
		and	a
		ld	hl,Error55
		ld	bc,DOS_BLKRD		; random block read
		jr	z,@r03
		ld	hl,Error66
		ld	bc,DOS_BLKWRT		; random block write
@r03:		push	hl
		call	jp_bc
		and	a
		ret	nz
		pop	hl
		jp	FinPrt			; output back to screen and quit

; ---------------------------------------------------------
; Hook FIEL: FIELD statement
; ---------------------------------------------------------
BAS_FIEL:	call	TakeControl		; take control from caller
		cp	'#'
		call	z,GetNextChar		; get basic character
		ld	ix,GETBYT
		call	CallBasic		; evaluate byte operand
		jp	z,Error02		; zero, syntax error
		push	hl			; store BASIC pointer
		ld	ix,FILIDX
		call	CallBasic		; get i/o channel pointer
		ld	e,l
		ld	d,h
		jp	z,Error59		; i/o channel closed, file not open error
		jp	c,Error11		; device is not a diskdrive, illegal function call error
		ld	a,(hl)			; i/o channel mode
		cp	4			; random i/o ?
		jp	nz,Error61		; nz=no, bad file mode error
		inc	hl			; +1
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a			; pointer to FCB
		ld	bc,14
		add	hl,bc
		ld	c,(hl)
		inc	hl
		ld	b,(hl)			; record size
		ld	(TMPBUF+0),bc		; store record size
		ld	hl,0
		ld	(TMPBUF+2),hl		; total field size = 0
		ld	bc,9			; offset to i/o channel buffer
		pop	hl			; restore BASIC pointer
@loop:		ex	de,hl
		add	hl,bc
		ex	de,hl			; update pointer in i/o channel buffer
		ld	a,(hl)
		cp	','			; field variable seperator ?
		ret	nz			; nope, quit
		push	de			; store pointer in i/o channel buffer
		ld	ix,GTBYTC
		call	CallBasic		; skip basic char and evaluate byte operand
		push	af			; store field size
		call	CheckChar
		db	'A'
		call	CheckChar
		db	'S'			; check for "AS"
		ld	ix,PTRGET
		call	CallBasic		; get address of variable
		ld	ix,GETYPR
		call	CallBasic		; GETYPR
		jp	nz,Error13		; not a string,
		pop	af			; restore field size
		ex	(sp),hl			; store BASIC pointer, restore pointer in i/o channel buffer
		push	de			; store pointer to string descriptor
		push	hl			; store pointer in i/o channel buffer
		ld	hl,(TMPBUF+2)
		ld	c,a
		ld	b,0			; field size
		add	hl,bc
		ld	(TMPBUF+2),hl		; update total field size
		ex	de,hl
		ld	hl,(TMPBUF+0)		; record size
		call	DCOMPR
		jp	c,Error50
		pop	de			; restore pointer in i/o channel buffer
		pop	hl			; restore pointer to string decriptor
		ld	(hl),c			; update string size
		inc	hl
		ld	(hl),e
		inc	hl
		ld	(hl),d			; update pointer to string
		ld	b,0
		pop	hl			; restore BASIC pointer
		jr	@loop			; next

; ---------------------------------------------------------
; Hook RSET: right set
; ---------------------------------------------------------
BAS_RSET:	db	$f6			; OR xx: skip next instrunction

; ---------------------------------------------------------
; Hook LSET: left set
; ---------------------------------------------------------
BAS_LSET:	scf
		call	TakeControl		; take control from caller
		push	af
		ld	ix,PTRGET
		call	CallBasic		; get address of variable
		ld	ix,GETYPR
		call	CallBasic		; GETYPR
		jp	nz,Error13
		push	de
		ld	ix,FRMEQL
		call	CallBasic		; evaluate =expression
		pop	bc
		ex	(sp),hl
		push	hl
		push	bc
		ld	ix,FRESTR
		call	CallBasic		; free temporary string
		ld	b,(hl)
		ex	(sp),hl
		ld	a,(hl)
		ld	c,a
		push	bc
		push	hl
		push	af
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		or	a
		jr	z,lrset5
		ld	hl,(NULBUF)
		dec	hl
		call	DCOMPR
		jr	c,lrset2
		ld	hl,(VARTAB)
		call	DCOMPR
		jr	c,lrset2
		ld	e,c
		ld	d,000H
		ld	hl,(STKTOP)
		add	hl,de
		ex	de,hl
		ld	hl,(FRETOP)
		call	DCOMPR
		jr	c,lrset7
		pop	af
lrset1:		ld	a,c
		ld	ix,GETSPA
		call	CallBasic		; allocate stringspace
		pop	hl
		pop	bc
		ex	(sp),hl
		push	de
		push	bc
		ld	ix,FRESTR
		call	CallBasic		; free temporary string
		pop	bc
		pop	de
		ex	(sp),hl
		push	bc
		push	hl
		inc	hl
		push	af
		ld	(hl),e
		inc	hl
		ld	(hl),d
lrset2:		pop	af
		pop	hl
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		pop	bc
		pop	hl
		inc	hl
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		ld	a,c
		cp	b
		jr	nc,lrset3
		ld	b,a
lrset3:		sub	b
		ld	c,a
		pop	af
		call	nc,AddSpaces
		inc	b
lrset4:		dec	b
		jr	z,lrset6
		ld	a,(hl)
		ld	(de),a
		inc	hl
		inc	de
		jr	lrset4

lrset5:		pop	bc
		pop	bc
		pop	bc
		pop	bc
		pop	bc
lrset6:		call	c,AddSpaces
		pop	hl
		ret

; Add spaces
AddSpaces:	ld	a,$20
		inc	c
@r01:		dec	c
		ret	z
		ld	(de),a
		inc	de
		jr	@r01

lrset7:		pop	af
		pop	hl
		pop	bc
		ex	(sp),hl
		ex	de,hl
		jr	nz,lrset8
		push	bc
		ld	a,b
		ld	ix,STRINI
		call	CallBasic		; allocate temporary string
		call	mkds1
		pop	bc
lrset8:		ex	(sp),hl
		push	bc
		push	hl
		jp	lrset1

; ---------------------------------------------------------
; Hook MKI: make integer
; ---------------------------------------------------------
BAS_MKIS:	ld	a,2
		db	$01

; ---------------------------------------------------------
; Hook MKS: make single float
; ---------------------------------------------------------
BAS_MKSS:	ld	a,4
		db	$01

; ---------------------------------------------------------
; Hook MKD: make double float
; ---------------------------------------------------------
BAS_MKDS:	ld	a,8
		call	TakeControl		; take control from caller
		push	af
		ld	ix,DOCNVF
		call	CallBasic		; convert DAC
		pop	af
		ld	ix,STRINI
		call	CallBasic		; allocate temporary string
		ld	hl,(DSCTMP+1)
		call	VMOVMF			; copy variable content from DAC
		
; subroutine
mkds1:		ld	de,TEMPST+30
		ld	hl,(TEMPPT)
		ld	(DAC+2),hl
		ld	a,3
		ld	(VALTYP),a
		call	VMOVE			; copy stringdescriptor
		ld	de,TEMPST+30+3
		call	DCOMPR
		ld	(TEMPPT),hl
		jp	z,Error16		; string formula too complex error
		ret
		
; ---------------------------------------------------------
; Hook CVI: convert to integer
; ---------------------------------------------------------
BAS_CVI:	ld	a,2-1
		db	$01

; ---------------------------------------------------------
; Hook CVS: convert to single float
; ---------------------------------------------------------
BAS_CVS:	ld	a,4-1
		db	$01

; ---------------------------------------------------------
; Hook CVD: convert to double float
; ---------------------------------------------------------
BAS_CVD:	ld	a,8-1
		call	TakeControl		; take control from caller
		push	af
		ld	ix,FRESTR
		call	CallBasic		; free temporary string
		pop	af
		cp	(hl)
		jp	nc,Error11
		inc	a
		inc	hl
		ld	c,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,c
		ld	(VALTYP),a
		jp	VMOVFM			; copy variable content to DAC

;subroutine (only called by DGET)
dget_sub:	ld	ix,GETYPR
		call	CallBasic
		ld	bc,(DAC+2)
		ld	hl,0
		ret	m
		jp	z,Error13
		ld	hl,DAC
		ld	de,BUF+158
		ld	bc,8
		ldir
		ld	hl,T6E68
		ld	de,ARG
		ld	c,8
		ldir
		call	DECDIV			; dbl divide
		and	a			; DAC type = double float
		call	INT			; integer part of float
		ld	ix,GETUI
		call	CallBasic		; convert address to integer
		push	de
		ex	de,hl
		ld	ix,FLTLIN
		call	CallBasic		; convert integer to sgn in DAC
		call	CONDS			; convert DAC from sgn to dbl
		ld	bc,$6545
		ld	de,$6053
		call	SGNMUL			; sgn multiply
		ld	hl,DAC
		ld	de,ARG
		ld	bc,8
		ldir
		ld	hl,BUF+158
		ld	de,DAC
		ld	c,8
		ldir
		call	DECSUB			; dbl subtract
		ld	ix,GETUI
		call	CallBasic		; convert address to integer
		ld	c,e
		ld	b,d
		pop	hl
		ret

T6E68:		db	$45,$65,$53,$60,$00,$00,$00,$00

; ---------------------------------------------------------
; Hook EOF: end of file
; ---------------------------------------------------------
BAS_EOF:	call	TakeControl		; take control from caller
		push	hl
		call	ReadCharIO		; get char from I/O channel
		ld	hl,0
		jr	nc,@r01
		dec	hl
@r01:		push	af
		call	MAKINT			; integer to DAC
		pop	af
		pop	hl
		inc	hl
		inc	hl
		inc	hl
		ld	(hl),a
		ret

; ---------------------------------------------------------
; Hook FILE: list FILES/LFILES statement
; ---------------------------------------------------------
BAS_FILE:	call	TakeControl		; take control from caller
		ld	d,$00
		jr	z,@r01			; end of statement,
		call	ValFile			; evaluate disk file specification, default file name = *.*
		push	hl			; store BASIC pointer
		jr	@r02

@r01:		push	hl			; store BASIC pointer
		call	ValWild2		; file name = *.*
@r02:		call	A6F63
		ld	a,(PRTFLG)
		and	a			; output to printer ?
		push	af
		call	SearchFile1		; search file
		jp	z,Error53		; not found, file not found error
		ld	ix,CRDONZ
		call	CallBasic		; newline to OUTDO if not at start of line
@r03:		ld	hl,BUF+85
		ld	b,11
@r04:		ld	a,(hl)
		inc	hl
		ld	ix,R_OUTDO
		call	CallBasic
		ld	a,b
		cp	$04
		jr	nz,@r06
		ld	a,(hl)
		cp	' '
		jr	z,@r05
		ld	a,'.'
@r05:		ld	ix,R_OUTDO
		call	CallBasic
@r06:		djnz	@r04
		ld	ix,CKCNTC
		call	CallBasic
		pop	af			; output to printer ?
		push	af
		ld	a,(LINLEN)
		ld	b,a			; screenwidth
		ld	a,(TTYPOS)		; screenpos
		jr	z,@r07			; nope, use screen
		ld	b,80			; printerwidth
		ld	a,(LPTPOS)		; printerpos
@r07:		and	a			; at start of line ?
		jr	z,@r09			; yep, no newline
		add	a,$0c
		cp	b
		jr	nc,@r08
		ld	a,' '
		ld	ix,R_OUTDO
		call	CallBasic
@r08:		ld	ix,CRDO
		call	nc,CallBasic		; yep, newline to OUTDO
@r09:		ld	de,TMPBUF
		xor	a
		ld	(TMPBUF+12),a
		call	DOS_SRCHNX		; search for next
		inc	a
		jr	nz,@r03
		pop	af
FinPrt:		pop	hl
		ld	ix,FINPRT
		jp	CallBasic		; output back to screen and quit

; ---------------------------------------------------------
; Hook KILL: remove file
; ---------------------------------------------------------
BAS_KILL:	call	TakeControl		; take control from caller
		call	ValFile1		; evaluate disk file specification with * wildcard support
		call	GetChar			; at end of statement ?
		ret	nz			; nope, quit
		call	CheckOpen		; is file already open in one of the I/O channels ?
		jp	z,Error64
		call	A6F63
		push	hl
		ld	de,TMPBUF
		call	DOS_DELETE		; delete file
		and	a			; error ?
		jp	nz,Error53		; yep, file not found error
		pop	hl
		ret

; ---------------------------------------------------------
; Hook NAME: rename file
; ---------------------------------------------------------
BAS_NAME:	call	TakeControl		; take control from caller
		call	ValFile1		; evaluate disk file specification with * wildcard support
		call	CheckOpen		; is file already open in one of the I/O channels ?
		jp	z,Error64
		call	A6F63
		push	hl
		call	SearchFile1		; search file
		jp	z,Error53		; not found, file not found error
		pop	hl
		call	CheckChar
		db	'A'
		call	CheckChar
		db	'S'			; check for "AS"
		call	ValFile1		; evaluate disk file specification with * wildcard support
		ld	a,d
		ld	(TMPBUF+16),a
		push	hl
		ld	hl,(TMPBUF+0)
		and	a
		jr	z,@r01
		cp	l
		jp	nz,Error71
@r01:		ld	de,TMPBUF+17
		call	CopyFileName1
		ld	de,TMPBUF
		call	DOS_RENAME		; rename file
		and	a
		jp	nz,Error65
		pop	hl
		ret

A6F63:		call	SetDrive
		inc	a
		ld	(TMPBUF+0),a
		push	hl
		push	de
		call	CopyFileName
		pop	de
		pop	hl
		ret

; Subroutine evaluate disk file specification
; Input:  HL = BASIC pointer
ValFileStr:	ld	ix,FILEVL
		call	CallBasic		; evaluate file specification
		ld	a,d
		cp	$09			; device = disk drive ?
		ret	c			; c=yes, quit
		jp	Error62			; bad drive name error

; Subroutine evaluate disk file specification, default file name = *.*
; Input:  HL = BASIC pointer
ValFile:	call	ValFileStr		; evaluate disk file specification
		push	hl			; store BASIC pointer
		ld	hl,FILNAM
		ld	b,8+3
valfile01:	ld	a,(hl)
		inc	hl
		cp	' '			; space ?
		jr	nz,valfile02		; nz=no
		djnz	valfile01
		call	ValWild2		; file name = *.*
		jr	valfile02

; Subroutine evaluate disk file specification with * wildcard support
; Input:  HL = BASIC pointer
ValFile1:	call	ValFileStr		; evaluate disk file specification
		push	hl			; store BASIC pointer
valfile02:	ld	hl,FILNAM
		ld	b,8
		call	ValWild1		; expand * wildcard
		ld	b,3
		call	ValWild1		; expand * wildcard
		pop	hl			; restore BASIC pointer
		db	$f6			; OR xx: skip next instrunction, clear Cx: ? character allowed

; Subroutine validate file name (wildcard not allowed)
ValFile2:	scf				; set Cx: ? character not allowed
		push	de			; store DE
		push	hl			; store HL
		ld	de,FILNAM
		push	de			; store FILNAM
		ld	b,8+3			; file name counter
valfile03:	push	bc			; store file name counter
		ld	a,(de)
		ld	hl,IllegalChar
		ld	bc,IllegalSize
		jr	c,valfile04
		dec	bc
valfile04:	cpir				; search illegal character
		jr	z,ValError		; found,
		pop	bc			; restore file name counter
		inc	de
		djnz	valfile03		; next
		pop	hl			; restore FILNAM
		call	ValRestore
		pop	hl			; restore HL
		pop	de			; restore DE
		ret

IllegalChar:	db	".\"/\\[]:+=;,*?"

IllegalSize	equ	ValRestore-IllegalChar

ValRestore:	ld	a,' '
		cp	(hl)			; file name starts with a space ?
		jr	z,ValError		; yep,
		ld	b,8-1
		call	valrestore01
		ld	b,3
valrestore01:	inc	hl
		cp	(hl)
		jr	z,valrestore02
		djnz	valrestore01
		ret

valrestore02:	dec	b
		ret	z
		inc	hl
		cp	(hl)
		jr	z,valrestore02
ValError:	jp	Error56

ValWild1:	ld	a,(hl)
		cp	'*'
		jr	z,ValWild3
		inc	hl
		djnz	ValWild1
		ret

ValWild2:	ld	hl,FILNAM
		ld	b,8+3
ValWild3:	ld	(hl),'?'
		inc	hl
		djnz	ValWild3
		ret

; ---------------------------------------------------------
; Hook LOF:
; ---------------------------------------------------------
BAS_LOF:	ld	bc,$0010
		db	$11

; ---------------------------------------------------------
; Hook LOC:
; ---------------------------------------------------------
BAS_LOC:	ld	bc,$0021
		call	TakeControl		; take control from caller
		push	bc
		ld	ix,CONINT
		call	CallBasic		; convert to byte
		ld	ix,FILIDX
		call	CallBasic		; get i/o channel pointer
		jp	c,Error11
		jp	z,Error59
		pop	bc
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		ex	de,hl
		add	hl,bc
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		ex	de,hl
		push	bc
		ld	ix,FLTLIN
		call	CallBasic		; convert integer to sgn in DAC
		ld	bc,06545H
		ld	de,06053H
		call	SGNMUL			; sgn multiply
		ld	hl,DAC
		ld	de,ARG
		ld	bc,8
		ldir
		pop	hl
		ld	ix,FLTLIN
		call	CallBasic		; convert integer to sgn in DAC
		call	CONDS			; convert DAC from sgn to dbl
		jp	DECADD			; dbl add

; ---------------------------------------------------------
; Hook DSKF: disk free
; ---------------------------------------------------------
BAS_DSKF:	call	TakeControl		; take control from caller
		ld	ix,CONINT
		call	CallBasic		; convert to byte
		ld	hl,SNUMDR
		cp	(hl)
		jr	z,A7074
		jp	nc,Error62
A7074:		ld	e,a
		call	DOS_GETEFA
		jp	MAKINT			; integer to DAC

; ---------------------------------------------------------
; Hook COPY: copy file
; COPY "source_file" TO "target_file"
; ---------------------------------------------------------
BAS_COPY:	call	TakeControl		; take control from caller
		call	ValFile			; evaluate disk file specification, default file name = *.*
		call	CheckOpen		; is file already open in one of the I/O channels ?
		jp	z,Error64
		call	A6F63
		push	hl
		ld	a,(CURDRV)
		inc	a
		ld	(BUF+47),a
		ld	hl,TMPBUF+1
		ld	de,BUF+48
		ld	bc,00024H
		ldir
		pop	hl
		call	GetChar			; at end of statement ?
		jr	z,@r01			; yep,
		call	CheckChar
		db	0D9H			; check for TO token
		call	ValFile			; evaluate disk file specification, default file name = *.*
		call	CheckOpen		; is file already open in one of the I/O channels ?
		jp	z,Error64
		call	SetDrive
		inc	a
		ld	(BUF+47),a
		push	hl
		ld	de,BUF+48
		call	CopyFileName1
		pop	hl
		call	GetChar			; at end of statement ?
		ret	nz			; nope, quit
@r01:		push	hl
		call	SearchFile1		; search file
		jp	z,Error53		; not found, file not found error
CopyNext:	call	CKCNTC
		ld	bc,BUF+47
		ld	de,BUF+121
		ld	hl,BUF+84
		ld	a,00CH
@r02:		push	af
		ld	a,(bc)
		cp	'?'
		jr	nz,@r03
		ld	a,(hl)
@r03:		ld	(de),a
		inc	bc
		inc	de
		inc	hl
		pop	af
		dec	a
		jr	nz,@r02
		ld	hl,BUF+84
		ld	de,BUF+121
		ld	b,00CH
@r04:		ld	a,(de)
		cp	(hl)
		jr	nz,@r05
		inc	hl
		inc	de
		djnz	@r04
		jp	Error11

@r05:		call	Bloadsave1
		push	hl
		xor	a
		ld	(BUF+96),a
		ld	de,BUF+84
		call	DOS_OPEN		; open fcb
		ld	de,BUF+121
		call	DOS_CREATE		; create file
		and	a
		jp	nz,Error67
		ld	l,a
		ld	h,a
		ld	(BUF+117),hl
		ld	(BUF+119),hl
		ld	(BUF+154),hl
		ld	(BUF+156),hl
		inc	hl
		ld	(BUF+98),hl
		ld	(BUF+135),hl
		pop	hl
@r06:		push	hl
		ld	de,BUF+84
		call	DOS_BLKRD		; random block read
		ld	a,l
		or	h
		jr	z,@r07
		ld	de,BUF+121
		call	RndBlockWrite1		; random block write
		pop	hl
		jr	@r06

@r07:		pop	hl
		ld	hl,(BUF+104)
		ld	(BUF+141),hl
		ld	hl,(BUF+106)
		ld	(BUF+143),hl
		ld	de,BUF+121
		call	DOS_CLOSE		; close fcb
		ld	hl,BUF+84
		ld	(DMAADD),hl		; transferaddress
		ld	de,TMPBUF
		xor	a
		ld	(TMPBUF+12),a
		call	DOS_SRCHNX		; search for next
		inc	a
		jp	nz,CopyNext
		pop	hl
		ret

; subroutine
Bloadsave:	ld	(SAVENT),hl
		ld	(SAVEND),bc
Bloadsave1:	ld	hl,0FE00H
		add	hl,sp
		jr	nc,@r01
		ld	de,(STREND)
		and	a
		sbc	hl,de
		jr	c,@r01
		ld	a,h
		and	a
		jr	nz,@r02
@r01:		ld	de,(NULBUF)
		ld	hl,256
@r02:		ld	(DMAADD),de		; transferaddress
		ret

; subroutine
RndBlockWrite:	ld	de,(FCBBAS)		; FCB for i/o channel 0
RndBlockWrite1:	call	DOS_BLKWRT		; random block write
		and	a
		ret	z
		jr	Error66

Error60:	ld	e,$3c			; bad fat
		db	$01
Error61:	ld	e,$3d			; bad file mode
		db	$01
Error62:	ld	e,$3e			; bad drive name
		db	$01
Error64:	ld	e,$40			; file still open
		db	$01
Error65:	ld	e,$41			; file already exists
		db	$01
Error66:	ld	e,$42			; disk full
		db	$01
Error67:	ld	e,$43			; too many files
		db	$01
Error68:	ld	e,$44			; disk write protected
		db	$01
Error69:	ld	e,$45			; disk i/o error
		db	$01
Error70:	ld	e,$46			; disk offline
		db	$01
Error71:	ld	e,$47			; rename across disk
		ld	bc,$0000
		xor	a
		ld	(NLONLY),a		; not loading basic program, close i/o channels when requested
		ld	(FLBMEM),a		; ascii mode
		push	de
		ld	ix,CLSFIL
		call	CallBasic		; close i/o channel
		pop	de
		ld	ix,ERROR		; BASIC error
		jp	CallBasic

; Subroutine expand errormessages (H_ERRP)
BAS_ERRP:	ei
		ld	a,e
		sub	$3c
		ret	c
		cp	0ch
		ret	nc
		inc	a
		ld	b,a			; 1 based offset
		ld	hl,ErrorTxtTab
errp1:		ld	a,(hl)
		and	a
		inc	hl
		jr	nz,errp1
		djnz	errp1			; next errormessage
		dec	hl			; include trailing zero
		ld	de,BUF+166
		push	de
		ld	bc,22
		ldir				; copy errormessage to temporary place
		ld	e,1			; erroroffset 1
		pop	hl
		ret

ErrorTxtTab:	db	0
		db	"Bad FAT",0			; 60
		db	"Bad file mode",0		; 61
		db	"Bad drive name",0		; 62
		db	"Bad sector number",0		; 63
		db	"File still open",0		; 64
		db	"File already exists",0		; 65
		db	"Disk full",0			; 66
		db	"Too many files",0		; 67
		db	"Disk write protected",0	; 68
		db	"Disk I/O error",0		; 69
		db	"Disk offline",0		; 70
		db	"Rename across disk",0		; 71


DiskErrVec:	dw	DiskErrHandler		; pointer to the default diskerror handler for DiskBASIC

; Subroutine diskerror handler for DiskBASIC
DiskErrHandler:	bit	7,c			; FAT error ?
		jp	nz,Error60		; yep, bad fat error
		res	0,c
		ld	b,0
		ld	hl,ErrorTab
		add	hl,bc
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		jp	(hl)

ErrorTab:	dw	Error68		; Write Protect error, disk write protect error
		dw	Error70		; Not Ready error, disk offline error
		dw	Error69		; Data/CRC error, disk i/o error
		dw	Error69		; Seek error, disk i/o error
		dw	Error69		; Record not found error, disk i/o error
		dw	Error69		; Write fault error, disk i/o error
		dw	Error69		; Other error, disk i/o error

; Subroutine check for BASIC character
; Input:  HL = BASIC pointer
CheckChar:	call	GetChar
		ex	(sp),hl
		cp	(hl)
		jr	nz,Error02		; syntax error
		inc	hl
		ex	(sp),hl
		inc	hl
GetChar:	dec	hl
GetNextChar:	ld	ix,CHRGTR
		jr	CallBasic		; continue in CHRGTR

Error16:	ld	e,$10			; string formula too complex
		db	$01
Error13:	ld	e,$0d			; type mismatch
		db	$01
Error07:	ld	e,$07			; out of memory
		db	$01
Error59:	ld	e,$3b			; file not open
		db	$01
Error55:	ld	e,$37			; input past end
		db	$01
Error50:	ld	e,$32			; field overflow
		db	$01
Error54:	ld	e,$36			; file already open
		db	$01
Error52:	ld	e,$34			; bad file number
		db	$01
Error56:	ld	e,$38			; bad file name
		db	$01
Error53:	ld	e,$35			; file not found
		db	$01
Error11:	ld	e,$05			; illegal function call
		db	$01
Error02:	ld	e,$02			; syntax error
		ld	ix,ERROR		; BASIC error
		
CallBasic:	call	HB_CALBAS
		ei
		ret

; ---------------------------------------------------------
; Hook PARD: parse device
; ---------------------------------------------------------
BAS_PARD:	ei
		push	hl
		push	de
		ld	hl,12			; Stack position (BLOAD/BSAVE calls FILEVL calls H.PARDEV)
		add	hl,sp
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		push	hl
		ld	hl,BSAVE+3
		call	DCOMPR			; called from BSAVE ?
		pop	hl
		jr	z,@r01			; yep, adjust returnaddress for DiskBASIC BSAVE
		push	hl
		ld	hl,BLOAD+3
		call	DCOMPR			; called from BLOAD ?
		pop	hl
		jr	nz,@r03			; nope, do not adjust returnaddress
		ld	de,BLDCHK1
		jr	@r02			; yep, adjust returnaddress for DiskBASIC BLOAD
@r01:		ld	de,BSVCHK1
@r02:		ld	(hl),d
		dec	hl
		ld	(hl),e
@r03:		pop	de
		pop	hl
		ld	a,e
		cp	2			; filespecification length <2 ?
		ret	c			; yep, no device specified
		ld	a,(hl)
		cp	':'			; first character a ':' ?
		jr	z,Error56		; yep,
		inc	hl
		ld	a,(hl)
		cp	':'			; second character a ':' ?
		dec	hl
		ret	nz			; nope, no diskdevice specifier
		call	TakeControl		; take control from caller
		ld	a,(hl)
		and	$df			; upcase
		sub	$40			; to drivenumber/deviceid for disk (1...) 
		push	hl
		ld	hl,SNUMDR
		cp	(hl)			; valid drive ?
		pop	hl
		jr	z,@r04
		jp	nc,Error62		; nope,
@r04:		inc	hl
		inc	hl
		dec	e
		dec	e
		push	de
		inc	e			; flag Zx reset
		pop	de
		ret

; ---------------------------------------------------------
; Hook NODE: no device
; ---------------------------------------------------------
BAS_NODE:	ei
		ld	a,0			; deviceid for default drive
		ret

; ------------------------------------------------------------------------------
; Subroutines
; ------------------------------------------------------------------------------

; Subroutine take control from caller
;
; HBIOS: custom stack setup
; +0	save hl
; +2	return address caller of this subroutine
; +4	address HB_ENASYS subroutine
; +6	return address Disk BASIC caller

TakeControl:	ei
		push	hl			; save hl
		ld	hl,6			; return address Disk BASIC caller
		add	hl,sp
		ld	(hl),RETRTN % 256	; address contains simple RET instruction
		inc	hl
		ld	(hl),RETRTN / 256
		pop	hl
		ret

; Subroutine DCOMPR (compare HL with DE)
; Copy of routine in BIOS
DCOMPR:		ld	a,h
		sub	d
		ret	nz
		ld	a,l
		sub	e
		ret

; Subroutine divide
; Copy of routine in kernel
DIV16:		ld	hl,0

; Subroutine divide
Divide:		ld	a,b
		ld	b,$10
		rl	c
		rla
_divide1:	rl	l
		rl	h
		jr	c,_divide4
		sbc	hl,de
		jr	nc,_divide2
		add	hl,de
_divide2:	ccf
_divide3:	rl	c
		rla
		djnz	_divide1
		ld	b,a
		ret

_divide4:	or	a
		sbc	hl,de
		jr	_divide3
		
; ------------------------------------------------------------------------------
; *** Disk driver functions ***
; ------------------------------------------------------------------------------

		; Mandatory symbols defined by the disk hardware interface driver
		PUBLIC	DSKIO		; Disk I/O routine
		PUBLIC	DSKCHG		; Disk change routine
		PUBLIC	GETDPB		; Get disk parameter block (DPB)
		PUBLIC	READSEC		; Read sector (MBR / bootsector) routine
		PUBLIC	CHOICE
		PUBLIC	DSKFMT
		PUBLIC	MTOFF
		PUBLIC	OEMSTA


; Structure of DOS driver work area:
; $00	First absolute sector of partition #1
; $04	Partition type of partition #1
; ...
; $23	First absolute sector of partition #8
; $27	Partition type of partition #8
; 
; $28 - $2B work variables:
W_CURDRV	equ	$28	; Current drive
W_BOOTDRV	equ	$29	; Boot drive (partition)
W_DRIVES	equ	$2a	; Number of drives (partitions) on disk
W_RWFLAG	equ	$2b	; Read/Write flag
W_HBDISK	equ	$2c	; HBIOS disk number

; ------------------------------------------
; GETDPB - Set DPB using sector 0 / bootsector of partition
; Called by DOS 1 only, not used by DOS 2.2
; Input:
;   A  = Drive number
;   B  = First byte of FAT
;   C  = Media descriptor
;   HL = Base address of DPB
; Output:
;   [HL+1]  .. [HL+18] = DPB fo the specified drive
;   [HL+19] .. [HL+20] = unchanged (FAT pointer)
;   [HL+21]..[HL+22]   = 16-bit value of max. directory entries
; ------------------------------------------
GETDPB:		ei
		push	hl
		ld	de,0			; first logical sector
		ld	hl,(SSECBUF)		; transfer address
		ld	b,1			; number of sectors is 1
		or	a			; carry flag cleared ==> read sector
		call	DSKIO
		pop	iy
		ret	c
		ld	ix,(SSECBUF)
		ld	a,(ix+$15)		; Media ID
		ld	(iy+$01),a
		ld	(iy+$02),$00		; Sector size is 0200h
		ld	(iy+$03),$02
		ld	(iy+$04),$0f		; Directory mask 00fh: 512/32-1
		ld	(iy+$05),$04		; Directory shift 004h
		ld	a,(ix+$0d)		; Cluster size (in sectors)
		dec	a
		ld	(iy+$06),a		; Cluster mask
		ld	c,$00
r601:		inc	c
		rra
		jr	c,r601
		ld	(iy+$07),c		; Cluster shift
		ld	l,(ix+$0e)		; Number of unused sectors
		ld	h,(ix+$0f)
		ld	(iy+$08),l		; FIRFAT - first FAT sector
		ld	(iy+$09),h
		ld	e,(ix+$16)		; Size of FAT (in sectors)
		ld	(iy+$10),e		; FATSIZ - Sectors per FAT
		ld	d,$00
		ld	b,(ix+$10)		; Number of FATs
		ld	(iy+$0A),b
r602:		add	hl,de
		djnz	r602
		ld	(iy+$11),l		; FIRDIR - First directory sector
		ld	(iy+$12),h
		ld	a,(ix+$12)		; Number of directory entries (high byte)
		ex	de,hl
		ld	h,a
		ld	l,(ix+$11)		; Number of directory entries (low byte)
		ld	(iy+$15),l		; MAXENT16 (low byte)
		and	$0F			; cutoff MAXENT at 0x0FFF (4095) because there are max 256 directory sectors
		ld	(iy+$16),a		; MAXENT16 (high byte)
		ld	(iy+$0b),$00		; if MAXENT=0 then use MAXENT16
		ld	bc,$000f
		add	hl,bc
		add	hl,hl
		add	hl,hl
		add	hl,hl
		add	hl,hl			; 16 directory entries per sector
		ld	l,h
		ld	h,$00
		ex	de,hl
		add	hl,de
		ld	(iy+$0c),l		; FIRREC - first data sector
		ld	(iy+$0d),h
		ex	de,hl
		ld	l,(ix+$13)		; Total number of sectors
		ld	h,(ix+$14)
		ld	bc,$0000
		ld	a,l
		or	h
		jr	nz,r605
		ld	l,(ix+$20)
		ld	h,(ix+$21)
		ld	c,(ix+$22)
		ld	b,(ix+$23)
r605: 		or	a
		sbc	hl,de
		jr	nc,r606
		dec	bc
r606:		ld	a,(iy+$07)
r607:	  	dec	a
		jr	z,r608
		srl	b
		rr	c
		rr	h
		rr	l
		jr	r607
r608:		inc	hl
		ld	(iy+$0e),l		; MAXCLUS - number of clusters + 1
		ld 	(iy+$0f),h
		xor	a
		ret

; ------------------------------------------
; DSKIO - IDE Hard Disk Read/Write
; Input:
;   Carry flag = clear ==> read, set ==> write
;   A  = drive number
;   B  = number of sectors to transfer
;   C  = if bit7 is set then media descriptor byte
;        else first logical sector number bit 16..22
;   DE = first logical sector number (bit 0..15)
;   HL = transfer address
; Output:
;   Carry flag = clear ==> successful, set ==> error
;   If error then 
;     A = error code
;     B = remaining sectors
; May corrupt: AF,BC,DE,HL,IX,IY
; ------------------------------------------
DSKIO:		ei
		push	hl
		push	de
		push	bc
		push	af
		cp	$08			; Max 8 drives (partitions) supported
		jr	nc,r404
		call	GETWRK			; base address of workarea in hl and ix
		pop	af
		jr	c,write_flag
		ld	(ix+W_RWFLAG),$00
		jr	r400
write_flag:	ld	(ix+W_RWFLAG),$01
r400:		ld	e,a
		add	a,a
		add	a,a
		add	a,e
		ld	e,a			; a * 5
		ld	d,$00
		add	hl,de
		push	hl
		pop	iy
		pop	bc
		pop	de
		pop	hl
		xor	a
		or	(iy+$04)		; Test if partition exists (must have nonzero partition type)
		jr	z,r405

		; translate logical to physical sector number
		push	bc			; save sector counter
		push 	hl			; save transfer address
		ex	de,hl
		bit	7,c			; if bit 7 of media descriptor is 0 then use 23-bit sector number
		jr	nz,r401			; nz if 16-bit sector number
		ld	e,c			; bit 16-22 of sector number
		ld	d,$00
		jr	r402
r401:		ld	de,$0000
r402:		ld	c,(iy+$00)
		ld	b,(iy+$01)
		add	hl,bc
		ex	de,hl			; LBA address: de=00..15
		ld	c,(iy+$02)
		ld	b,(iy+$03)
		adc	hl,bc
		ld	c,l			; LBA address: c=16..23
		pop	hl			; restore transfer address
		pop	af			; restore sector counter
		ld	b,a			; "

		; IDE read/write sector command with more than 1 sector is not supported by all disks
rw_loop:	call	hbDiskSeek
		jr	c,r405
		push	bc
		push	de
		call	rw_sector
		pop	de
		pop	bc
		jr 	c,r405
		inc	e
		jr	nz,r403
		inc	d
		jr	nz,r403
		inc	c
r403:		djnz	rw_loop
		xor	a
		ret

		; Disk i/o error
r404:		pop	af
		pop	bc
		pop	de
		pop	hl
r405:		ld	a,$04			; Error 4 = Data (CRC) error (abort,retry,ignore message)
		scf
		ret

rw_sector:	ld	a,(ix+W_RWFLAG)		; get read/write flag
		or	a
		jr	nz,dosWriteSector

; ------------------------------------------
dosReadSector:	bit	7,h			; store data in ram page 2 or 3?
		push	af
		push	hl
		jr	nz,rd01			; nz=yes, directly store data in destination
		ld	hl,(SSECBUF)		; init temporary buffer pointer
rd01:		call	hbDiskRead
		pop	de
		pop	af
		jr	nz,rd03
		ld	hl,(SSECBUF)
		ld	bc,$0200		; sector size
		call	XFER
		ex	de,hl
rd03:		xor	a			; reset c-flag: no error
		ret

; ------------------------------------------
dosWriteSector:	push	hl
		bit	7,h			; read data from ram page 2 or 3?
		jr	nz,wr01			; nz=yes, directly read data from source
		ld	de,(SSECBUF)		; copy source to temporary buffer
		ld	bc,$0200		; sector size
		call	XFER
		ld	hl,(SSECBUF)		; init buffer pointer
wr01:		call	hbDiskWrite
		pop	hl
		inc	h
		inc	h
		xor	a			; reset c-flag: no error
		ret

; ------------------------------------------
; DSKCHG - Disk change 
; Input:
;   A  = Drive number
;   B  = 0
;   C  = Media descriptor
;   HL = Base address of DPB
; Output:
;   If successful then
;     Carry flag reset
;     B = Disk change status
;         1= Disk unchanged, 0= Unknown, -1=Disk changed
;   else
;     Carry flag set
;     Error code in A
; May corrupt: AF,BC,DE,HL,IX,IY
; ------------------------------------------
DSKCHG:		push	af
		call	GETWRK
		pop	af
		cp	(ix+W_CURDRV)		; current drive
		ld	(ix+W_CURDRV),a
		jr	nz,r501
		ld	b,$01			; unchanged
		xor	a
		ret

r501:		ld	b,$FF			; changed
		xor	a
		ret

; ------------------------------------------
; READSEC - Read (boot) sector
; Input:  C,DE = sector number
;         HL   = transfer address
; Output: Carry flag = clear ==> successful, set ==> error
; May corrupt: AF,BC,DE
; ------------------------------------------
READSEC:	call	hbDiskSeek
		ret	c
		jp	dosReadSector

; ------------------------------------------
; DOS error code handler
; Input: A = IDE error
; ------------------------------------------
dosError:	ld	l,a
		ld	a,$08			; error $08: sector not found
		rr	l
		ret	c			; bit 0: address mark not found --> error $08
		rr	l
		ret	c			; bit 1: track 0 not found --> error $08
		ld	a,$0c			; error $0c: disk error
		rr	l
		ret	c			; bit 2: abort, wrong command --> error $0c
		rr	l
		ld	a,$08
		rr	l
		ret	c			; bit 4: sector id not found --> error $08
		rr	l
		ld	a,$04			; error $04: CRC error
		rr	l
		ret	c			; bit 6: uncorrectable data error --> error $04
		ld	a,$02			; error $02: not ready
		scf
		ret

; ------------------------------------------
; CHOICE - Choice for FORMAT 
; Input : None
; Output: HL = 0 (no choice)
; ------------------------------------------
CHOICE:		xor	a
		ld	l,a
		ld	h,a
		ret

; ------------------------------------------
; DSKFMT  - Format not implemented
; MTOFF   - Motors off not implemented
; ------------------------------------------
DSKFMT:		ld	a,$0c		; Error $0c = Bad parameter
		scf
MTOFF:		ret

; -------------------------------------------
; OEMSTATEMENT - BASIC System statement expansion
; -------------------------------------------
OEMSTA:		scf
		ret
		
; ------------------------------------------------------------------------------
; *** HBIOS driver interface routines ***
; ------------------------------------------------------------------------------

; ----------------------------------------------------------
; Seek disk sector
; Input:  C,D,E = 24-bit sector address
; Output: Cx = set if error
; ----------------------------------------------------------
hbDiskSeek:	push	bc
		push	de
		push	hl
		
		; convert sector address / use LBA mode
		ex	de,hl
		ld	e,c
		ld	d,$80
		
		; call HBIOS
		ld	b,BF_DIOSEEK
		ld	c,(ix+W_HBDISK)
		call	MSX_HBINVOKE
		
		pop	hl
		pop	de
		pop	bc
		xor	a
		ret

; ----------------------------------------------------------
; Read disk sector
; Input:  HL = sector buffer (must be in upper memory)
; ----------------------------------------------------------
hbDiskRead:	ld	a,(MSX_BANKID)
		inc	a
		ld	d,a		; set HBIOS bank
		ld	e,1 		; read 1 sector
	
		; call HBIOS
		ld	b,BF_DIOREAD
		ld	c,(ix+W_HBDISK)
		call	MSX_HBINVOKE
		
		xor	a
		ret

; ----------------------------------------------------------
; Write disk sector
; Input:  HL = sector buffer
; ----------------------------------------------------------
hbDiskWrite:	ld	a,(MSX_BANKID)
		inc	a
		ld	d,a		; set HBIOS bank
		ld	e,1 		; read 1 sector
	
		; call HBIOS
		ld	b,BF_DIOWRITE
		ld	c,(ix+W_HBDISK)
		call	MSX_HBINVOKE
		
		xor	a
		ret

; ------------------------------------------------------------------------------

; Subroutine: get my disk hardware driver workarea
; Output: HL = IX = pointer to workarea
; Remark: used by the disk interface handler and driver
GETWRK:		ld	hl,(MYWORK)
		push	hl
		pop	ix
		ret

BaseCode:
		DEPHASE

; ------------------------------------------------------------------------------
IplCode:
		PHASE	IPLBASE

AutoBas:	db	0,"AUTOEXECBAS",0
RunAutoBas:	db	"RUN\"AUTOEXEC.BAS",0
BasErrVec:	dw	StartBasDirect		; start DiskBASIC in direct mode

; DOS entrypoint to start DiskBASIC
StartBasic:	call	HB_ENARAM		; enable TPA RAM first
		ld	hl,RunAutoBas
		ld	de,TMPBUF
		ld	bc,$0011
		ldir				; copy RUN"AUTOEXEC.BAS in BUF
		ld	hl,NOTFIR
		ld	a,(hl)
		and	a			; is this a warm boot ?
		ld	(hl),h			; next boot is a warm boot
		jr	nz,@r01			; yep, no autoexec.bas but a parameter from MSXDOS ?

		; try to start AUTOEXEC.BAS
		ld	(DOSFLG),a		; flag CALL SYSTEM invalid
		ld	hl,BasErrVec
		ld	(DISKVE),hl		; setup disk errorhandler
		ld	hl,AutoBas
		ld	de,TMPBUF+$11
		ld	bc,37
		push	de
		ldir				; setup FCB for autoexec.bas
		pop	de
		call	DOS_OPEN		; open FCB
		inc	a
		jr	z,StartBasDirect	; z=error, start DiskBASIC in direct mode
		jr	StartBasFile		; no error, start DiskBASIC and run AUTOEXEC.BAS

@r01:		ld	a,(WBOOT)
		cp	$c3			; start from DOS?
		jr	nz,StartBasDirect	; nz=no, start DiskBASIC in direct mode
		ld	hl,$0080
		ld	b,(hl)
		inc	b
		dec	b
		jr	z,StartBasDirect	; no parameter specified (after the BASIC command), start DiskBASIC in direct mode
@r02:		inc	hl
		ld	a,(hl)
		cp	' '
		jr	nz,@r03
		djnz	@r02			; remove spaces in front
		jr	StartBasDirect		; no parameter specified, just start diskbasic

@r03:		xor	a
		ld	c,b
		ld	b,a
		ld	de,TMPBUF+4
		ldir				; copy file name after the RUN" at TMPBUF
		ld	(de),a			; and a end of line marker
		jr	StartBasFile		; start DiskBASIC and run specified basic file

; Subroutine start DiskBASIC in direct mode
StartBasDirect:	xor	a
		ld	(TMPBUF+3),a		; make it a ordinary RUN at TMPBUF

; Subroutine start DiskBASIC
StartBasFile:	ld	sp,TMPSTK		; switch to a temporary stack
		call	HB_ENASYS		; enable main ROM
		ld	hl,(BOTTOM)
		xor	a
		ld	(hl),a			; before the program always a end of line marker
		inc	hl
		ld	(TXTTAB),hl		; start of basictext space
		ld	(hl),a
		inc	hl
		ld	(hl),a			; end of program marker
		inc	hl
		ld	(VARTAB),hl		; start of the variablespace
		ld	hl,$ffff
		ld	(CURLIN),hl		; interpreter in direct mode
		call	InitDiskBas		; initialize diskbasic
		ld	sp,(STKTOP)
		ld	a,$ff
		ld	(CNSDFG),a		; enable function keys
		ld	a,FF
		rst	R_OUTDO			; clear screen
		ld	ix,M7D31
		call	CALBAS			; display BASIC startscreen
		ld	hl,TextDiskBas
		call	DspString
		ld	hl,NTSTOP
		push	hl			; execute RUN command
		ld	hl,TMPBUF-1
		push	hl			; basicpointer
		ld	hl,BUF+64
		push	hl
		ld	(hl),$e1
		inc	hl
		ld	(hl),$c9		; pop the basicpointer when returning
		ret

DspString:  	ld	a,(hl)
		inc	hl
		and	a
		ret	z
		rst	R_OUTDO			; print character
		jr	DspString

TextDiskBas:	db	CR,LF, "HBDOS Disk BASIC version 1.0",CR,LF,0
		
; ------------------------------------------------------------------------------		
		
; Subroutine initialize DiskBASIC environment

InitDiskBas:	ld	hl,DiskErrVec
		ld	(DISKVE),hl		; setup diskerror handler
		ld	hl,BreakVec
		ld	(BREAKV),hl		; setup abort handler
		ld	hl,(HIMSAV)
		ld	(HIMEM),hl

		; Store Disk BASIC i/o channel FCB's in disk bank
		ld	hl,BUF_FCBBAS
		ld	(FCBBAS),hl

		; BLOAD/BSAVE helper routines are initialized in main module

		; adjust memory pointers for i/o channels
		ld	a,1
		ld	(MAXFIL),a
		ld	hl,(HIMEM)
		ld	de,-(2*256+2*9+2*2)
		add	hl,de
		ld	(FILTAB),hl
		ld	e,l
		ld	d,h
		dec	hl
		dec	hl
		ld	(MEMSIZ),hl
		ld	bc,200
		and	a
		sbc	hl,bc
		push	hl
		ld	hl,2*2+9
		add	hl,de
		ld	(NULBUF),hl
		pop	hl
		ld	(STKTOP),hl
		dec	hl
		dec	hl
		ld	(SAVSTK),hl
		
		; init i/o channels
		ld	l,e
		ld	h,d
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		ld	a,2			; 2 i/o channels (1 user, 1 system)
@r01:		ex	de,hl
		ld	(hl),e
		inc	hl
		ld	(hl),d
		inc	hl
		ex	de,hl
		ld	bc,7
		ld	(hl),b			; i/o channel not open
		add	hl,bc
		ld	(hl),b			; clear i/o channel flags
		ld	bc,256+9-7
		add	hl,bc
		dec	a
		jr	nz,@r01
		ret

BreakVec:	dw	K_BDOS0			; restart basic

; ------------------------------------------------------------------------------

		; enable TPA RAM first
RestartDos:	call	HB_ENARAM		

StartDos:	; Mark FAT buffer of boot drive 0 invalid
		ld	hl,SDPBLI
		ld	e,(hl)
		inc	hl
		ld	d,(hl)			; pointer to DPB
		ld	hl,$0013
		add	hl,de
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		dec	de
		ex	de,hl
		ld	(hl),$ff
		
		ld	a,$EB
		ld	(DOSFLG),a		; flag bootable disk
		
		; update memory pointers
		ld	hl,SYSBASE
		ld	(HIMEM),hl
		ld	(HIMSAV),hl
		ld	(DOSHIM),hl

StartSys:	; display signon message
		ld	de,Signon
		call	SPRTBUF

		; set error vectors
		ld	hl,SYSBASE+$09
		ld	(DISKVE),hl
		ld	hl,SYSBASE+$11
		ld	(BREAKV),hl

		; set cold boot flag
		ld	hl,NOTFIR
		ld	a,(hl)
		ld	(hl),h			; next time not a cold boot
		ld	(SYSBASE+$1a),a		; initialize cold boot flag

		; boot HBDOS
		jp	SysBoot

; HBDOS information
Signon:		db	CR,LF,"HBDOS version 1.0",CR,LF,"$"

; ------------------------------------------------------------------------------

		DEPHASE

IplSize:	equ	$-IplCode
DiskSize	equ	$-DiskCode


; Use free area of disk bank for disk buffers

BUF_FCBBAS	equ	DISKBASE+$2000		; 512 byte buffer for Disk Basic I/O channels (default 7 * 37 bytes used)

