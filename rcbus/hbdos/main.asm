; ------------------------------------------------------------------------------
; main.asm
; HBDOS main entry point
;
; (C) 2025 All rights reserved.
; ------------------------------------------------------------------------------
; MSX RAM loadable HBIOS DOS created by H.J. Berends.
; HBDOS is compatible with the kernel functions of MSX-DOS 1.
; See also CXDOS 1.
; ------------------------------------------------------------------------------

		INCLUDE "base.inc"
		INCLUDE "hbmsx.inc"
		INCLUDE	"hbdos/hbdos.inc"

		SECTION	MAIN			; Main entry point / initial program load
		ORG	MSX_DOS			; 0x8000
		SECTION	DISK			; Disk ROM kernel and driver functions
		SECTION	SYS			; Disk RAM resident kernel and driver functions

; ------------------------------------------------------------------------------

		SECTION MAIN

		; HBIOS bank switching helper routines
		PUBLIC	HB_CALBAS		; Call main rom from disk BASIC
		PUBLIC	HB_ENASYS		; Enable main BIOS/BASIC ROM
		PUBLIC	HB_ENARAM		; Enable TPA RAM
		PUBLIC	HB_ENADISK		; Enable extension / disk ROM
		PUBLIC	JP_BLOAD		; Disk BASIC BLOAD
		PUBLIC	JP_BSAVE		; Disk BASIC BSAVE
		
		; Defined by kernel
		EXTERN	DosValFCB
				
		; Defined by driver
		EXTERN	DRIVES			; Return number of drives in system
		EXTERN	INIENV			; Initialize work area
		EXTERN	DEFDPB			; Base address of a 21 byte "default" DPB for this driver.
		EXTERN	MYSIZE			; Size of the page-3 RAM work area required by the driver in bytes.
		EXTERN	SECLEN			; Maximum sector size for media supported by this driver (512).

		; Defined by sys
		EXTERN	PH_SIRQ
		EXTERN	PH_XFER
		EXTERN	PH_RDSLT
		EXTERN	PH_WRSLT
		EXTERN	PH_CALLF
		EXTERN	PH_CALSLT
		EXTERN	PH_ENASLT
		EXTERN	PH_STROUT
		EXTERN	SysCode
		EXTERN	SysSize
		EXTERN	SysBoot
		
		; Defined by disk
		EXTERN	DiskCode
		EXTERN	DiskSize
		EXTERN	IplCode
		EXTERN	IplSize
		EXTERN	StartBasic
		EXTERN	StartDos
		EXTERN	DiskBasHandler
		EXTERN	BasBload
		EXTERN	BasBsave

; ------------------------------------------------------------------------------
; *** Init 1: check requirements ***
; ------------------------------------------------------------------------------

; Initial program loader, HBDOS is the one (master) disk system
; Entry: MSX BIOS/BASIC is initalized up to the point where extension roms are called
; Returning from ipl will continue MSX BASIC init at MSX-DOS entry point.

		defs	3,0			; BASIC sets the first 3 bytes to 0

StartIpl:	di

		; check memory requirements
		ld	bc,BC_SYSGET_MEMINFO
		call	MSX_HBINVOKE
		ld	a,e			; RAM banks in system
		cp	$06			; meets requirement?
		ret	c			; nc=no, return to BASIC
		
; ------------------------------------------------------------------------------
; *** Init 2: load disk system ***
; ------------------------------------------------------------------------------

		; initialize screen
		;xor	a
		;ld	(CSRSW),a		; cursor off
		;call	ERAFNK			; erase function key display
		call	INITXT			; init text mode 40x24

		ld	sp,TMPSTK		; set stack below msxdos area in page 3
		ld	hl,VARWRK
		ld	(HIMEM),hl		; set DOS himem to start of BIOS/BASIC workarea
		
		ld	hl,VARWRK+MYSIZE
		ld	de,DATABA
		and	a
		sbc	hl,de			; bytes needed for static workarea+workarea driver
		call	AllocMemHL		; allocate memory
		push	hl			; save base for disk driver workarea data

		; clear page 3 DOS workarea from sys to himem
		ld	hl,SYSBASE
		ld	bc,VARWRK-SYSBASE
		call	ClearArea

		; copy workarea static data
		ld	hl,DosData
		ld	de,DATABA
		ld	bc,DataSize
		ldir

		; set driver workarea
		pop	hl
		ld	(MYWORK),hl

		; set master disk slot
		ld	a,SLOT_DISK
		ld	(MASTER),a

		; copy system code to page 3 
		ld	hl,SysCode
		ld	de,SYSBASE
		ld	bc,SysSize
		ldir		

		; copy DISK/BASIC bank switching helper routines
		ld	hl,DiskBasCode
		ld	de,DISKBAS
		ld	bc,DiskBasSize
		ldir

		; initialize DOS kernel bank
		call	HB_ENADISK
		call	InitPage		; init page 0
		ld	hl,DiskCode		; init page 1
		ld	de,DISKBASE
		ld	bc,DiskSize
		ldir
		; copy disk DOS/BASIC IPL code to page 2
		ld	hl,IplCode
		ld	de,IPLBASE
		ld	bc,IplSize
		ldir
		call	HB_ENASYS

		; initialize DOS jump table
		ld	hl,JumpTable
		ld	de,SDOSON
		ld	bc,8*3
		ldir

		; patch BIOS jump table entries for CALLF,CALSLT and ENASLT 
		ld	hl,PH_CALLF
		ld	(CALLF+1),hl
		ld	hl,PH_CALSLT
		ld	(CALSLT+1),hl
		ld	hl,PH_ENASLT
		ld	(ENASLT+1),hl

; ------------------------------------------------------------------------------
; *** Init 3: setup disk system ***
; ------------------------------------------------------------------------------

		; clear DRVTBL, HOOKSA
		ld	hl,DRVTBL
		ld	bc,$1400		; b=4*2+4*3 c=clear
		call	FillTable
		
		; Initialize interface / determine available drives.
		; Leave DRVTBL empty in case an application wants to call driver routines in a disk rom
		; at address 0x401X then it won't detect this interface with the driver routines in RAM.
		xor	a			; default no phantom drives
		call	DRIVES			; query no. of drives (this routine will enable interrupts)
		add	a,l
		cp	8+1			; more than 8 drives?
		jr	c,r002			; c=no
		ld	a,8			; limit to 8
r002:		ld	(SNUMDR),a		; drives in system (only 1 interface supported)
		ld	hl,SDPBLI
		push	hl
		push	af
		ld	b,0
		ld	c,a
		ld	de,23
		call	Multiply		; bc*de = drives * size of DPB
		call	AllocMemBC		; reserve number of bytes for the DPBs (max 8 * 23 = 184)
		ex	de,hl
		pop	af
		pop	hl
r003:	  	ld	(hl),e
		inc	hl
		ld	(hl),d			; save in DPBTBL
		inc	hl
		push	hl
		ld	hl,DEFDPB
		ld	bc,23
		ldir				; initialize DPB
		pop	hl
		dec	a
		jr	nz,r003			; next drive
		call	INIENV			; initialize hardware driver workarea

		; initialize double byte header char table
		ld	hl,CHAR_16
		ld	de,KANJTA
		ld	bc,4
		ldir

		; localization and console initialization
		ld	a,(IDBYT0)
		rrca
		rrca
		rrca
		rrca
		and	$07
		ld	(COUNTR),a		; date format
		ld	a,$ff
		ld	(BUFDRN),a		; invalid datasector buffer
		ld	(DIRBFD),a		; invalid directorysector buffer
		ld      (DAYCNT+1),a		; invalid days since 1-1-1980
		ld	a,$0d
		ld	(YCONBF+130),a		; ?? end marker con buffer
		ld	a,7
		ld	(FILMAX),a		; max number of FCBs is 7
		ld	hl,$4035		; number of days between 1-1-1980 and 1-1-2025
		ld	(CURDAT),hl		; set default date when no clockchip

		; initialize character i/o
		ld	hl,AUXBOD
		ld	(hl),$3e		; ld a,"EOF" for AUX input
		inc	hl
		ld	(hl),$1a
		ld	b,2*5-2
r006:	  	inc	hl
		ld	(hl),$c9
		djnz	r006


		; todo: use page 1 for disk buffers

		; initialize buffers
		ld	bc,SECLEN		; fixed sector size (512 bytes)
		call	AllocMemBC
		ld	(SSECBUF),hl		; allocate sectorbuffer
		call	AllocMemBC
		ld	(SBUFFE),hl		; allocate datasector buffer

		; Allocate only 1.5K memory space for a 3 sector FAT buffer, for all drives.
		; If the FAT is bigger than 3 sectors, the needed sector within the FAT will be swapped in from disk.
		call	AllocMemBC
		ld	(SDIRBU),hl		; allocate dirsector buffer
		ld	h,b			; hl,bc = size of biggest sector
		ld	l,c
		add	hl,hl
		add	hl,bc
		inc	hl			; (size of biggest sector: 512) * 3 + 1
		call	AllocMemHL
		inc 	hl
		ld	(FATSWAP4),hl		; FAT common buffer pointer

		; initialize DPB for each drive
		ld	a,(SNUMDR)		; drives in system
		ld	b,a			; b=number of drives
		ld	c,0			; c=drive 0 (A:)
		ld	hl,SDPBLI
r005:	  	ld	e,(hl)
		inc	hl
		ld	d,(hl)			; DPB of drive
		inc	hl
		push	hl
		push	de
		pop	ix
		ld	(ix+0),c		; set drivenumber in DPB
		ld	hl,(FATSWAP4)		; FAT common buffer
		ld	(ix+19),l
		ld	(ix+20),h
		inc	c
		pop	hl
		djnz	r005

		; Use HBIOS clock functions
		; Note: it works the same as MSX if there is no RTC
		ld	a,$ff
		ld	(TIMFLG),a		; flag use clockchip

		; Enable main rom bank and patch BASIC reverse disk hooks
		call	HB_ENASYS
		call	PatchBasic

		; if quit anywhere after this point then start Disk BASIC
		ld	hl,StartBasic
		push	hl

		; set TPA RAMAD variables to fixed values for compatiblity
		xor	a			; SLOT 0
		ld	(RAMAD3),a
		ld	(RAMAD2),a
		inc	a			; SLOT 1
		ld	(RAMAD1),a
		ld	(RAMAD0),a

		; init DOS interrupt routine
		ld	hl,160			; DOS stack size
		call	AllocMemHL
		ld	(IRQ_ST),hl
		ld	a,$c3
		ld	hl,PH_SIRQ
		ld	(MSX_HKEYINT),a
		ld	(MSX_HKEYINT+1),hl	; KEYINT hook
		
		; Enable TPA RAM bank and init scratch area
		call	HB_ENARAM
		call	InitPage

		jp	StartDos

; ------------------------------------------------------------------------------
; *** Subroutines ***
; ------------------------------------------------------------------------------

; Subroutine allocate RAM memory (halt when error)
; Input: BC,HL = number of bytes to allocate
AllocMemHL:	ld	b,h
		ld	c,l
AllocMemBC:	ld	hl,(HIMEM)
		and	a
		sbc	hl,bc
		ld	(HIMEM),hl		; new top of MSXDOS
		jr	c,HaltSystem		; c=below zero, halt system
		ld	a,h
		cp	DOSFREE / 256		; below bottom of free variable memory for DOS?
		ret	nc

HaltSystem:  	call	DspMsg
		db	CR,LF
		db	"Kernel panic, system halted",0
		di
		halt

; ------------------------------------------------------------------------------

; Initialize RAM page 0
InitPage:
		; clear scratch area: 0x0000-0x00FF
		xor	a
		ld	l,a
		ld	h,a
@r001:	  	ld	(hl),a
		inc	l
		jr	nz,@r001

		; set MSX jump routines in scratch area
		ld	a,$c3
		;
		ld	hl,PH_RDSLT
		ld	(RDSLT),a
		ld	(RDSLT+1),hl
		;
		ld	hl,PH_WRSLT
		ld	(WRSLT),a
		ld	(WRSLT+1),hl
		;
		ld	hl,PH_CALLF
		ld	(CALLF),a
		ld	(CALLF+1),hl
		;
		ld	hl,PH_CALSLT
		ld	(CALSLT),a
		ld	(CALSLT+1),hl
		;
		ld	hl,PH_ENASLT
		ld	(ENASLT),a
		ld	(ENASLT+1),hl

		; set HBIOS jump routine
		ld	hl,HB_INVOKE
		ld	(HBCALL),a
		ld	(HBCALL+1),hl
		
		; set interrupt routine
		ld	hl,MSX_INT
		ld	(KEYINT+0),a
		ld	(KEYINT+1),hl		; HBIOS/MSX interrupt handler

		ret

; ------------------------------------------------------------------------------

;Patch BASIC reverse hooks
PatchBasic:
		; Patch BASIC hook calls
		; HBIOS limitation: H.DSKO, HD.DSKI and BDOSBO disk functions are not supported, i.o.w.
		; BASIC (embedded assembler) programs that call DOS functions or do sector read/write won't work.
		ld	hl,PatchTab
		ld	de,DISKBAS
		ld	b,31			; number of hooks in patch table
@r01:		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	a,$cd
		ld	(de),a
		inc	de
		ld	a,DISKBAS%256
		ld	(de),a
		inc	de
		ld	a,DISKBAS/256
		ld	(de),a
		djnz	@r01

		; Set flag for BASIC extension ROM handler
		; Offset: 16*SLOT + 4*EXTENDED_SLOT + PAGE
		; Disk ROM slot is 2, no extended slot, page 1
		ld	hl,SLTATR+16*2+4*0+1			
		ld	(hl),$20		; bit 5 = call statement flag
		
		ret

PatchTab:	dw	M_NAME,M_KILL,M_COPY,M_DSKF,M_LSET,M_RSET,M_FIEL,M_MKIS,M_MKSS,M_MKDS
		dw	M_CVI,M_CVS,M_CVD,M_GETP,M_NOFO,M_NULO,M_NTFL,M_BINS,M_BINL,M_FILE,M_DGET
		dw	M_FILO,M_INDS,M_LOC,M_LOF,M_EOF,M_BAKU,M_PARD,M_NODE,M_ERRP,M_PHYD

; ------------------------------------------------------------------------------

; The DOSHOOKS variable area is re-used for disk ROM paging helper routines
; the paging routines are optimized for fast HBIOS bank switching

DiskBasCode:

		PHASE	DISKBAS
		
; call disk BASIC from main BASIC
DiskBasCall:	call	HB_ENADISK
		jp	DiskBasHandler

; call main rom from disk BASIC
HB_CALBAS:	call	HB_ENASYS
		ei
		call	CLPRM1			; call (ix) subroutine
		; fall through

; enable disk rom 
HB_ENADISK:	di
		push	af
		ld	a,(MSX_BANKID)
		sub	SLOT_DISK		; SLOT 2: DSK ROM
		call	HB_BNKSEL
		pop	af
		ret

; enable TP ram
; enable main rom
HB_ENARAM:	di
		push	af
		ld	a,(MSX_BANKID)
		sub	SLOT_RAM		; SLOT 1: TPA RAM
		call	HB_BNKSEL
		pop	af
		ret

; enable main rom
HB_ENASYS:	di
		push	af
		ld	a,(MSX_BANKID)		; SLOT 0: MAIN ROM
		call	HB_BNKSEL
		pop	af
		ret

; BLOAD helper routine
JP_BLOAD:	ld	a,d
		cp	9			; diskdevice ?
		jp	nc,BLOAD+3		; nope, resume BLOAD
		call	HB_ENADISK
		call	BasBload
		call	HB_ENASYS
		push	hl
		jp	BLDFIN			; finish BLOAD

; BSAVE helper routine
JP_BSAVE:	ld	a,d
		cp	9			; diskdevice ?
		jp	nc,BSAVE+3		; nope, resume BSAVE
		call	HB_ENADISK
		call	BasBsave
		jp	HB_ENASYS

		DEPHASE

DiskBasSize	equ	$-DiskBasCode

; ------------------------------------------------------------------------------

; Subroutine: clear area / fill area
; Input:  hl = start of area
;	  bc = number of bytes
;	  a  = filler (fill area)
ClearArea:	xor	a
FillArea:	ld	(hl),a
		ld	d,h
		ld	e,l
		inc	de
		dec	bc
		ldir
		ret

; Subroutine: fill table
; Input: hl = start of area
;	  b  = number of bytes
;	  c  = filler
FillTable:	ld	(hl),c
		inc	hl
		djnz	FillTable
		ret

; Subroutine compare hl with de (DCOMPR)
; Input:  hl,de = values to compare
; Output: zx set if hl=de, cx set if hl<de
Compare:	ld	a,h
		sub	d
		ret	nz
		ld	a,l
		sub	e
		ret

; Subroutine multiply (duplicate in kernel)
Multiply:	ld	hl,0

; Subroutine multiply high word
MultiplyHigh:	ld	a,b
		ld	b,$11
		jr	_multiply3

_multiply1:	jr	nc,_multiply2
		add	hl,de
_multiply2:	rr	h
		rr	l
_multiply3:	rra
		rr	c
		djnz	_multiply1
		ld	b,a
		ret

; ------------------------------------------------------------------------------
; *** DOS page 3 jump table ***
; ------------------------------------------------------------------------------

JumpTable:	jp	HB_ENADISK		; DOSON
		jp	HB_ENARAM		; DOSOF
		jp	PH_XFER
		jp	AUXBOD+0
		jp	AUXBOD+5
		jp	JP_BLOAD		; BLDCHK1
		jp	JP_BSAVE		; BSVCHK1
		jp	BDOSBO

; ------------------------------------------------------------------------------
; *** HBDOS static data ***
; ------------------------------------------------------------------------------
; HBDOS data, DOS 1 routines/variables are retained for compatibility.
; No DOSOF/DOSON required, the free space is re-used for variables.

DosData:
		PHASE	DATABA

; Function $09 STROUT
; String output
; Input:  de = address of string
AF1C9:	 	jp	PH_STROUT

AF1CC:		dw	0			; LASTEN16
AF1CE:		dw	0			; ENTFRE16
AF1D0:		dw	0			; SRCHLO16
AF1D2:		db	0			; reserved for variable
AF1D3:		dw	0			; MYWORK
AF1D5:		dw	0			; SP_IRQ
AF1D7:		dw	0			; IRQ_ST

; Subroutine move block, same as XFER for driver (obsolete).
AF1D9:		ldir
		ret

AF1DC:		db	0			; SS_TEMP
AF1DD:		dw	0			; F16LOSEC
AF1DF:		db	0			; F16HISEC
AF1E0:		dw	0			; reserved for variables

; Subroutine Warm Boot
AF1E2:	 	call	HB_ENARAM		; ENDJMP
		jp	WBOOT

; Subroutine start handler in DOS memory
; Input:  HL = address of pointer
AF1E8:   	ld	e,(hl)			; JPHL
		inc	hl
		ld	d,(hl)
		ex	de,hl
		jp	(hl)
		
AF1ED:		dw	0			; FATSWAP1
AF1EF:		db	$ff			; FATSWAP2 (initial value $ff)
AF1F0:		db	0			; FATSWAP3
AF1F1:		dw	0			; FATSWAP4
AF1F3:		db	0			; reserved for variables

; Subroutine validate FCB filename
; Input:  HL = address of pointer
AF1F4:		jp	DosValFCB		; LODNAM

; Data table with reserved filenames (devicenames)
; Remark: is copied to 0F1F7H
AF1F7:	 	db	"PRN "			; IONAME
		db	"LST "
		db	"NUL "
		db	"AUX "
		db	"CON "

; Data: fake direntry for devices
AF20B: 		defs	11,SPACE		; DEVDIR
		db	10000000b
		defs	10
		dw	0
		dw	0
		dw	0
		dw	0,0

; Month table (MONTAB)
AF22B:		db	31,28,31,30,31,30,31,31,30,31,30,31

		DEPHASE

DataSize	EQU	$-DosData

; ------------------------------------------------------------------------------
; *** Disk driver initialization ***
; ------------------------------------------------------------------------------

		; Mandatory symbols defined by the DOS driver
		PUBLIC	DRIVES
		PUBLIC	INIENV
		PUBLIC	DEFDPB
		PUBLIC	MYSIZE
		PUBLIC	SECLEN

		; Symbols defined in disk module
		EXTERN	READSEC

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

SECLEN		equ	512	; Sector length (fixed)
MYSIZE		equ	$2d	; Driver workarea size

; ------------------------------------------
; DRIVES - Get number of drives connected
; A maximum of 8 partitions (drives) is supported.
; Input:
;   F = The zero flag is reset if one physical drive must act as two logical drives.
; Output:
;   L = Number of drives connected. A value of 0 is not allowed for DOS 1.
; May corrupt: F,HL,IX,IY
;
; The DRIVES routine will also initialize the work environment
; ------------------------------------------

; Temporary RAM variables:
PART_BUF	equ	TMPSTK		; Copy of disk info / Master Boot Record
PART_BUFX	equ	PART_BUF+$200	; Copy of extended partition boot record
PART_NEXT	equ	PART_BUF+$400	; Pointer to next partition in work area
PART_EXTLO	equ	PART_BUF+$402	; First extended partition offset low word
PART_EXTHI	equ	PART_BUF+$404	; First extended partition offset high word
PART_EBRLO	equ	PART_BUF+$406	; Extended partition boot record offset low word
PART_EBRHI	equ	PART_BUF+$408	; Extended partition boot record offset high word

DRIVES:		push	af
		push	bc
		push	de

		; initialize work buffer
		call	GETWRK			; HL and IX point to work buffer
		push	hl
		ld	d,h
		ld	e,l
		inc	de
		ld	(hl),$00
		ld	bc,MYSIZE-1
		ldir

		; probe hardware, display info and validate MBR
		call	DRVINIT
		jr	nz,r207			; nz=interface not detected / canceled
		jr	c,r207			; c=interface time-out
		ld	hl,PART_BUF		; Buffer address
		xor	a
		ld	e,a
		ld	d,a
		ld	c,a			; MBR sector address = 0 (24 bits)
		call	HB_ENADISK
		call	READSEC			; read master boot record
		call	HB_ENASYS
		jr	c,r207
		ld	a,(PART_BUF+$01fe)	; Validate boot signature (AA 55)
		cp	$55
		jr	nz,r207
		ld	a,(PART_BUF+$01ff)
		cp	$aa
		jr	nz,r207

		pop	de			; Pointer to work bufer, initialided with zeros
		ld	hl,PART_BUF+$01be	; Start of partition table
		ld	b,$04			; max 4 primary partitions
r201:		ld	c,(hl)			; Save status byte (active/inactive)
		push	bc
		inc	hl			; Skip CHS address information of first sector
		inc	hl
		inc	hl
		inc	hl
		ld	a,(hl)			; Load partition type
		call	PartitionType
		jr	z,r202
		call	PartitionExt
		call	z,xpart
		ld	bc,$000c
		add	hl,bc			; Move to next partition
		pop	bc
		jr	r204
r202:		inc	hl			; Skip CHS address information of last sector
		inc	hl
		inc	hl
		inc	hl
		ldi				; Get LBA - first absolute sector in partition (4 bytes)
		ldi
		ldi
		ldi
		ld	(de),a			; Partition type
		inc	de
		inc	hl			; Skip LBA number of sectors (4 bytes) / move to next partition
		inc	hl
		inc	hl
		inc	hl
		pop	bc
		ld	a,c			; Restore status byte
		cp	$80			; Is active partition (and valid type)?
		jr	nz,r203
		ld	a,(ix+W_DRIVES)
		ld	(ix+W_BOOTDRV),a	; Update boot drive
r203:		inc	(ix+W_DRIVES)		; Increase partition count
r204:		ld	a,(ix+W_DRIVES)
		cp	$08			; Maximum partitions processed?
		jr	nc,r205
		djnz	r201			; process next primary partition
r205:		ld	a,(ix+W_DRIVES)
		or	a
		jr	nz,r206
		inc	a			; Return value of 0 drives is not allowed in DOS 1
r206:
		ld	l,a			; set number of drives
		pop	de
		pop	bc
		pop	af
		ret

; Hardware not detected or MBR not valid
r207:		pop	hl
		jr	r205

; Process extended partitions
xpart:		ld	(PART_NEXT),de		; save pointer to next partition in workarea
		push	af
		push	hl
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
		ld	(PART_EXTLO),de		; save first extended partition offset
		ld	(PART_EXTHI),bc		; "
		call	xpart1
		ld	de,(PART_NEXT)		; Load pointer to next partition in workarea
		pop	hl
		pop	af
		ret

xpart1:		ld	(PART_EBRLO),de		; save extended partition boot record offset
		ld	(PART_EBRHI),bc		; "
		ld	hl,PART_BUFX		; set extended partition boot record buffer
		call	HB_ENADISK
		call	READSEC			; read boot sector
		call	HB_ENASYS
		ret	c
		ld	a,(PART_BUFX+$01fe)	; Validate boot signature (AA 55)
		cp	$55
		ret	nz
		ld	a,(PART_BUFX+$01ff)
		cp	$aa
		ret	nz
		ld	a,(PART_BUFX+$01c2)	; Partition type of first entry
		call	PartitionType
		jr	nz,r222
r221:		ld	hl,PART_BUFX+$01c6	; First sector in partition
		ld	de,(PART_NEXT)		; Load pointer to next partition in workarea
		ld	a,(PART_EBRLO)
		add	a,(hl)
		ld	(de),a
		inc	hl
		inc	de
		ld	a,(PART_EBRLO+1)
		adc	a,(hl)
		ld	(de),a
		inc	hl
		inc	de
		ld	a,(PART_EBRHI)
		adc	a,(hl)
		ld	(de),a
		inc	hl
		inc	de
		ld	a,(PART_EBRHI+1)
		adc	a,(hl)
		ld	(de),a
		inc	de
		ld	a,(PART_BUFX+$01c2)	; load partition type
		ld	(de),a		
		inc	de
		ld	(PART_NEXT),de		; Save pointer to next partition in workarea
		inc	(ix+W_DRIVES)		; Increase partition counter
		ld	a,(ix+W_DRIVES)
		cp	$08			; Maximum partitions processed?
		ret	z			; z=yes
r222:		ld	a,(PART_BUFX+$01d2)	; Partition type of 2nd entry
		call	PartitionExt
		ret	nz			; End of chain
		ld	hl,PART_BUFX+$01d6	; pointer to sector number of next extended partition
		ld	de,(PART_EXTLO)		; value is relative to first extended partition sector
		ld	bc,(PART_EXTHI)
		ld	a,(hl)
		add	a,e
		ld	e,a
		inc	hl
		ld	a,(hl)
		adc	a,d
		ld	d,a
		inc	hl
		ld	a,(hl)
		adc	a,c
		ld	c,a
		inc	hl
		ld	a,(hl)
		adc	a,b
		ld	b,a
		jr	xpart1

; Validate partition type
PartitionType:	cp	$01			; FAT12
		ret	z
		cp	$04			; FAT16 (<=32MB)
		ret	z
		cp	$06			; FAT16B (>32MB)
		ret	z
		cp	$0e			; FAT16B with LBA
		ret
r223:		pop	af
		ret


; Validate extended partition type
PartitionExt:	cp	$05			; Extended partition (CHS,LBA)
		ret	z
		cp	$0f			; Extended partition (LBA)
		ret

; ------------------------------------------
; INIENV - Initialize the work area (environment)
; Input : None
; Output: None
; May corrupt: AF,BC,DE,HL,IX,IY
; ------------------------------------------
INIENV:		call	GETWRK			; HL and IX point to work buffer
		xor	a
		or	(ix+W_DRIVES)		; number of drives 0?
		ret	z
		ld	(ix+W_CURDRV),$ff	; Init current drive
		call	DspMsg
		db	"Drives: ",0
		ld	a,(ix+W_DRIVES)
		add	a,'0'
		rst	R_OUTDO
		call	DspCRLF
		ret

; ------------------------------------------
; Default DPB pattern (DOS 1)
; ------------------------------------------
DEFDPB:		db	$00		; +00 DRIVE	Drive number
		db	$f9		; +01 MEDIA	Media type
		dw	$0200		; +02 SECSIZ	Sector size
		db	$0f		; +04 DIRMSK	Directory mask
		db	$04		; +05 DIRSHFT	Directory shift
		db	$03		; +06 CLUSMSK	Cluster mask
		db	$03		; +07 CLUSSFT	Cluster shift
		dw	$0001		; +08 FIRFAT	First FAT sector
		db	$02		; +0A FATCNT	Number of FATs
		db	$70		; +0B MAXENT	Number of directory entries
		dw	$000e		; +0C FIRREC	First data sector
		dw	$02ca		; +0E MAXCLUS	Number of clusters+1
		db	$03		; +10 FATSIZ	Sectors per FAT
		dw	$0007		; +11 FIRDIR	First directory sector
		dw	$0000		; +12 FATPTR	FAT pointer
		dw	$0070		; +14 MAXENT16	16-bit value of max. directory entries

; ------------------------------------------
; DRVINIT - Initialize hardware interface driver
; Input:  None
; Output:
;   Zero flag  = Z: interface detected/active	NZ: not active
;   Carry flag = NC: successfully initialized   C: interface error / time-out
; May corrupt: AF,BC,DE,HL
; ------------------------------------------
DRVINIT:	
	IF DISKID = 0
		; Find first large disk
		ld	bc,BC_SYSGET_DIOCNT
		call	MSX_HBINVOKE		; e = number of disks
		ld	c,$ff
find_disk:	inc	c			; first/next disk unit
		dec	e
		ret	c			; no large disk
		push	bc
		push	de
		ld	b,BF_DIODEVICE
		call	MSX_HBINVOKE
		bit	5,c			; large disk? (>8MB)
		pop	de
		pop	bc
		jr	z,find_disk		; z=no, try next disk
		ld	a,c			; yes, save it
	ELSE
		ld	a,DISKID
	ENDIF
		ld	(ix+W_HBDISK),a
		add	a,'0'
		ld	(t_disk),a
		call	DspMsg
		db	12
		db	"HBDOS : disk unit #"
t_disk:		db	$00,13,10
		db	"Rev.  : "
		INCLUDE	"rdate.inc"		; Revision date
		db	13,10,0
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
		
; Subroutines display message 

DspMsg:		ex	(sp),hl
		call	DspString
		ex	(sp),hl
		ret

DspString:  	ld	a,(hl)
		inc	hl
		and	a
		ret	z
		rst	R_OUTDO			; print character
		jr	DspString
		
DspCRLF:	ld	a,CR
		rst	R_OUTDO
		ld	a,LF
		rst	R_OUTDO
		ret
