; hbmsx.inc
; RomWBW HBIOS / MSX definitions

BF_CIOIN		equ	$00		; character input
BF_CIOOUT		equ	$01		; character output
BF_CIOIST		equ	$02		; character input status
CIO_CONSOLE		equ	$80		; CIO unit number for current console

BF_DIOSTATUS		equ	$10		; disk status
BF_DIORESET		equ	$11		; disk reset
BF_DIOSEEK		equ	$12		; disk seek
BF_DIOREAD		equ	$13		; disk read sectors
BF_DIOWRITE		equ	$14		; disk write sectors
BF_DIODEVICE		equ	$17		; disk device info report
BF_DIOMEDIA		equ	$18		; disk media report
BF_DIOCAP		equ	$1a		; disk capacity report
BF_DIOGEOM		equ	$1b		; disk geometry report

BF_RTCGETTIM		equ	$20		; get time
BF_RTCSETTIM		equ	$21		; set time

BF_SYSRES_COLD		equ	$02		; cold start

BF_SYSRESET		equ	$f0		; soft reset HBIOS
BF_SYSSETBNK		equ	$f2		; set current bank
BF_SYSPEEK		equ	$fa		; get a byte value from alt bank
BF_SYSPOKE		equ	$fb		; set a byte value in alt bank

BC_SYSGET_DIOCNT	equ	$f810		; get disk unit count
BC_SYSGET_TIMER		equ	$f8d0		; get current timer value
BC_SYSGET_MEMINFO	equ	$f8f1		; get memory capacity info
BC_SYSGET_BNKINFO	equ	$f8f2		; get bank assignment info
BC_SYSINT_INFO		equ	$fc00		; get interrupt system info

HBX_IVT			equ	$ff00		; IM 2 interrupt vector table
HBX_INT			equ	$ff60		; interrupt handler
HB_INVOKE		equ	$fff0		; invoke HBIOS function
HB_BNKSEL		equ	$fff3		; select low memory bank id
HB_BNKCALL		equ	$fff9		; interbank function call
HB_IDENT		equ	$fffc		; pointer to HBIOS ident data block

MSX_KEYINT		equ	$0c3c		; MSX BIOS interrupt handler
MSX_DOS			equ	$8000		; MSX HBDOS loader
MSX_HB			equ	$fda4		; MSX HBIOS signature "HB"
MSX_INT			equ	$fda6		; HBIOS/MSX interrupt caller
MSX_HBINVOKE		equ	$fde0		; MSX invoke HBIOS with mutex
MSX_HKEYINT		equ	$fdfb		; MSX interrupt hook
MSX_MUTEX		equ	$fdfe		; Mutex to avoid HBIOS re-entry by interrupt handler
MSX_BANKID		equ	$fdff		; MSX main bank-id (BID_USR)

SLOT_SYS		equ	$00		; slot for main system BIOS/BASIC ROM and common RAM
SLOT_RAM		equ	$01		; slot for DOS TPA RAM
SLOT_DISK		equ	$02		; slot for system extension / disk ROM
SLOT_CART		equ	$03		; slot for user cartridge ROM
