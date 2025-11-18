; ------------------------------------------------------------------------------
; hbdos.inc
; HBDOS / MSX constants and definitions
;
; (C) 2025 All rights reserved.
; ------------------------------------------------------------------------------

		DEFINE	FAT16DOS1	; include FAT16
		DEFINE	IDEDOS1		; include DOS 1 enhancements

FASTALLOC	equ	$01		; Cutoff free disk space count at 2048 free clusters

SYSMAX		equ	$1A00		; Maximum size of RAM resident DOS code
DOSFREE		equ	$E400		; Bottom of free variable memory for DOS (256-byte boundary)
SYSBASE		equ	DOSFREE-SYSMAX	; Ram resident DOS code base address (256-byte page boundary, above TMPSTK)
DISKBASE	equ	$4000		; Disk ROM kernel base address
DISKBUF		equ	$7000		; Disk buffer area (FAT and Directory sectors)
IPLBASE		equ	$A000		; IPL BASIC/DOS base address

TBASE		equ	$0100		; Start of TPA
BOT32K		equ	$8000		; Bottom of 32 KB RAM
BOT16K		equ	$C000		; Bottom of 16 KB RAM
TMPSTK		equ	BOT16K+256	; Temporary stack

; Custom DOS variables
LASTEN16	equ	$F1CC		; 16-bit current direntry number (LASTEN=0xFF if invalid)
ENTFRE16	equ	$F1CE		; 16-bit first free direntry (ENTFRE=0xFF if none found)
SRCHLO16	equ	$F1D0		; 16-bit saved current direntry number search first/next (SRCHLO=0xFF if invalid)
MYWORK		equ	$F1D3		; pointer to driver workarea
SP_IRQ		equ	$F1D5		; temporary save stackpointer (KEYINT)
IRQ_ST		equ	$F1D7		; pointer to temporary stack (KEYINT)
SS_TEMP		equ	$F1DC		; Temporary secondary slot value
F16LOSEC	equ	$F1DD		; FAT16 32-bit sector number bit 0..15 (old: 0xFD0D)
F16HISEC	equ	$F1DF		; FAT16 32-bit sector number bit 16..31 (old: 0xFD0F)
FATSWAP1	equ	$F1ED		; FAT swap value (old: 0xFD09)
FATSWAP2	equ	$F1EF		; FAT swap current drive flag (old: 0xFFD9)
FATSWAP3	equ	$F1F0		; FAT swap block number in buffer (old: 0xFFDA)
FATSWAP4	equ	$F1F1		; FAT swap common buffer indicator (old: 0xFFDB)
DISKBAS		equ	$F24F		; DISK/BASIC HBIOS bank switching routines
TIMEBUF		equ	$F2B2		; HBIOS date/time buffer (BCD 6 bytes: year,month,day,hours,minutes,seconds)

; ----------------------------------------------
; constants
BELL		equ	$07
BS		equ	$08
TAB		equ	$09
LF		equ	$0A
HOME		equ	$0B
FF		equ	$0C
CR		equ	$0D
INS		equ	$12
CTRL_Z		equ	$1A
ESC		equ	$1B
RIGHT		equ	$1C
LEFT		equ	$1D
SPACE		equ	$20
DEL		equ	$7F

; RST labels
R_SYNCHR	equ     $08
R_CHRGTR	equ     $10
R_OUTDO		equ     $18
R_DCOMPR	equ     $20
R_GETYPR	equ     $28
R_CALLF		equ     $30

; ------------------------------------------------------------------------------
; MAIN ROM / HBDOS scratch area

WBOOT		equ     $0000		; CP/M warm boot
KBDOS		equ     $0005		; CP/M BDOS call
HBCALL		equ	$0008		; Invoke HBIOS function (RST 08)
RDSLT		equ     $000C		; Reads an address value in another slot
WRSLT		equ	$0014		; Writes an address value in another slot
CALSLT		equ     $001C		; Call a routine in another slot
ENASLT		equ     $0024		; Switch to slot in specified 16K page
IDBYT0		equ     $002B		; ROM Version
CALLF		equ	$0030		; Interslot call (slot:routine)
CHAR_16		equ     $0034		; Default Kanji character range
KEYINT		equ	$0038		; IM1 interrupt routine
LDIRMV		equ	$0059
LDIRVM		equ	$005C
INITXT		equ	$006C		; init text mode 40x24
CHSNS		equ     $009C		; Check status of the keyboard buffer
CHGET		equ     $009F		; Console input character (wait)
CHPUT		equ     $00A2		; Console output character
LPTOUT		equ     $00A5		; Printer output character
BREAKX		equ     $00B7		; Check status of CTRL+STOP (MSX break program keys)
CKCNTC		equ	$00BD
ERAFNK		equ	$00CC		; Hide bottom function key line
TOTEXT		equ	$00D2
SNSMAT	 	equ     $0141		; Get specified line of the keyboard matrix (machine/region dependent)
CALBAS		equ     $0159		; Call ROM BASIC routine

; ------------------------------------------------------------------------------
; MSX BASIC reverse hooks

M_NAME		equ	$7C20		; rename file
M_KILL		equ	$7C25		; delete file
M_COPY		equ	$7C2F		; copy file
M_DSKF		equ	$7C39		; free disk space (clusters)
M_LSET		equ	$7C48		; string function for file record handling
M_RSET		equ	$7C4D		; "
M_FIEL		equ	$7C52		; "
M_MKIS		equ	$7C57		; "
M_MKSS		equ	$7C5C		; "
M_MKDS		equ	$7C61		; "
M_CVI		equ	$7C66		; "
M_CVS		equ	$7C6B		; "
M_CVD		equ	$7C70		; "
M_GETP		equ	$6A93		; get file pointer
M_NOFO		equ	$6AF6		; no for clause
M_NULO		equ	$6B0F		; null file open
M_NTFL		equ	$6B3B		; not file number 0
M_BINS		equ	$6BCE		; binary save file
M_BINL		equ	$6BD4		; binary load file
M_FILE		equ	$6C2F		; list files in directory
M_DGET		equ	$6C3B		; read record into buffer
M_FILO		equ	$6C51		; file output
M_INDS		equ	$6C79		; disk attribute input
M_LOC		equ	$6D0F		; record number
M_LOF		equ	$6D20		; file size
M_EOF		equ	$6D25		; end of file
M_BAKU		equ	$6E36		; backup character internal command
M_PARD		equ	$6F15		; device name parser
M_NODE		equ	$6F33		; no device
M_ERRP		equ	$40DC		; error pointer
M_PHYD		equ	$148A		; disk i/o
;
DECSUB		equ	$268C
DECADD		equ	$269A
DECDIV		equ	$289F
VMOVE		equ	$2EF3
VMOVFM		equ	$2F08
VMOVMF		equ	$2F10
MAKINT		equ	$2F99
CONDS		equ	$3042
INT		equ	$30D1		; integer part of float
SGNMUL		equ	$325C
ERROR		equ	$406F		; BASIC error
READYR		equ	$409B		; restart BASIC
NTSTOP		equ	$4173		; execute BASIC statement
LINKER		equ	$4253		; recalculate linepointers
NEWSTT		equ	$4601		; execution loop
CHRGTR		equ	$4666		;
FLTLIN		equ	$46FF		; convert to SNG
INTID2		equ	$4756		; evaluate word operand and check for 0-32767 range
FINPRT		equ	$4AFF		; output back to screen
FRMEQL		equ	$4C5F		; evaluate =expression
FRMEVL		equ	$4C64		; evaluate expression
DOCNVF		equ	$517A		; convert DAC to other type
GTBYTC		equ	$521B		; evaluate next byte operand
GETBYT		equ	$521C		; evaluate byte operand
CONINT		equ	$521F		; convert to byte
GETUI		equ	$5432		; convert address to integer
SCCPTR		equ	$54F7		; convert pointers to linenumbers
GETYPR		equ	$5597
PTRGET		equ	$5EA4		; get address of variable
STRINI		equ	$6627		; allocate temp string
GETSPA		equ	$668E		; allocate string space
FRESTR		equ	$67D0		; free temporary string
FILEVL		equ	$6A0E		; evaluate filespecification
FILIDX		equ	$6A6D		; get i/o channel pointer
OPNFIL		equ	$6AFA		; open i/o channel
CLSFIL		equ	$6B24		; close i/o channel
CLSALL		equ	$6C1C		; close all i/o channels
NOSKCR		equ	$6E41		; resume character putback routine
BSAVE		equ	$6E92		; start of BSAVE routine
BLOAD		equ	$6EC6		; start of BLOAD routine
BLDFIN		equ	$6EF4		; finish BLOAD
M6F0B		equ	$6F0B		;
CRDONZ		equ	$7323		; newline to OUTDO if not at start of line
CRDO		equ	$7328		; newline to OUTDO
M739A		equ	$739A		;
M7D31		equ	$7D31		; BASIC ROM entry point for DOS

; ------------------------------------------------------------------------------
; DOS static data area (F1C9-F37F)

DATABA		equ	$F1C9		; Start of disk system static work area
SPRTBUF		equ	$F1C9		; BDOS function $09 STROUT
BLKMOV		equ	$F1D9		; move block
JPHL		equ	$F1E8		; start handler in DOS memory
LODNAM		equ	$F1F4		; validate FCB filename
ENDJMP		equ	$F1E2		; warm boot
IONAME		equ	$F1F7		; device name table
DEVDIR		equ	$F20B		; directory entry structure for devices
MONTAB		equ	$F22B		; month table
CARPOS		equ	$F237		; BDOS console output columnpos
STARTP		equ	$F238		; console columnpos at start of lineinput
INSERT		equ	$F239		; lineinput insert flag
PFLAG 		equ	$F23B		; console output to printer flag
DIRTYD		equ	$F23C		; directory buffer dirty flag
DMAADD		equ	$F23D		; transfer address
BUFSEC		equ	$F23F		; sectornumber in data buffer
BUFDRN		equ	$F241		; driveid of sector in data buffer
DIRTYB		equ	$F242		; data buffer dirty flag
BUFDRI		equ	$F243		; DPB pointer current operation
DIRBFI		equ	$F245		; sectornumber (offset) in directory buffer
DIRBFD		equ	$F246		; driveid of sector in directory buffer
CURDRV		equ	$F247		; default driveid
DAY		equ	$F248		; current day (1..31)
MONTH		equ	$F249		; current month (1..12)
YEAR		equ	$F24A		; current year (offset to 1980)
DAYCNT		equ	$F24C		; current days since 1-1-1980
WEEKDA		equ	$F24E		; current day of week (0=sunday)
; DOSHOOKS area re-used (105 bytes)
LASTEN		equ	$F2B8		; current direntry number
NAME1		equ	$F2B9		; filename1
ATTRIB		equ	$F2C4		; original DR byte FCB
NAME2		equ	$F2C5		; filename2 (rename)
NAME3		equ	$F2D0		; temporary save for NAME1 and ATTRIB (rename)
CREATI		equ	$F2DC		; flag ignore fileattributes
SECCLU		equ	$F2DD		; current relative sector in cluster
DSKERR		equ	$F2DE		; result of record operation
TRANS		equ	$F2DF		; flag increase current relative sector in cluster (0 means not)
PREREA		equ	$F2E0		; flag flake read (0 means real read)
THISDR		equ	$F2E1		; current driveid
NEXTAD		equ	$F2E2		; transfer address for record operation
RECPOS		equ	$F2E4		; start record (32 bit) for record operation
RECCNT		equ	$F2E8		; number of records for recordoperation
LASTPO		equ	$F2EA		; current relative cluster of file
CLUSNU		equ	$F2EC		; current cluster of file
SECPOS		equ	$F2EE		; start relative sector for recordoperation
VALSEC		equ	$F2F0		; relative cluster after fileend for write recordoperation
BYTSEC		equ	$F2F2		; start offset in sector for recordoperation
BYTPOS		equ	$F2F4		; start fileposition (32 bit) for recordoperation
BYTCT1		equ	$F2F8		; partical sector transfer at start
BYTCT2		equ	$F2FA		; partical sector transfer at end
SECCNT		equ	$F2FC		; number of complete sectors to transfer
ENTFRE		equ	$F2FE		; first free direntry (0FFH if none found)
READOP		equ	$F2FF		; flag diskoperation (0 if read, 1 if write)
CONTPO		equ	$F300		; pointer to remaining lineinput from CON read record operation
MAXCLS		equ	$F302		; temporary save for maximium cluster
SPSAVE		equ	$F304		; MSX-DOS save stackpointer
CPMCAL		equ	$F306		; CP/M function
SRCHFC		equ	$F307		; saved pointer to FCB search first, used for search next
SRCHIX		equ	$F309		; saved pointer to DPB search first, used for search next
SRCHLO		equ	$F30B		; saved current direntry number search first/next (0xFF means invalid)
FCBEXT		equ	$F30C		; original EX byte FCB
RAWFLG		equ	$F30D		; read after write (verify) flag
COUNTR		equ	$F30E		;
KANJTA		equ	$F30F		; double byte header table
DISKVE		equ	$F323		; pointer to disk error handler
BREAKV		equ	$F325		; pointer to break handler
AUXBOD		equ	$F327		;
BDOSBO		equ	$F331		; BDOS hook (MSXHOOK style)
RECSIZE		equ	$F33D		; recordsize GET/PUT recordoperations
KEYVLD		equ	$F336		; flag saved input available (0 = none available)
KEYCHR		equ	$F337		; save input
TIMFLG		equ	$F338		;
CURDAT		equ	$F33B		; days since 1-1-1980, used when no clockchip
TARGET		equ	$F33F		;
NOTFIR		equ	$F340		; cold boot flag
RAMAD0		equ	$F341		; slotid DOS ram page 0
RAMAD1		equ	$F342		; slotid DOS ram page 1
RAMAD2		equ	$F343		; slotid DOS ram page 2
RAMAD3		equ	$F344		; slotid DOS ram page 3
FILMAX		equ	$F345		; maximum number of diskbasic FCB's
DOSFLG		equ	$F346		; MSXDOS started flag
SNUMDR		equ	$F347		; number of drives
MASTER		equ	$F348		; slotid disksystem ROM
HIMSAV		equ	$F349		;
DOSHIM		equ	$F34B		;
SSECBUF		equ	$F34D		;
SBUFFE		equ	$F34F		; pointer to data sector buffer
SDIRBU		equ	$F351		; pointer to directory sector buffer
FCBBAS		equ	$F353		; pointer to the diskbasic FCB's
SDPBLI		equ	$F355		;
RSLREG		equ	$F365		;
SDOSON		equ	$F368		; enable disk system ROM on page 1
SDOSOF		equ	$F36B		; enable TPA RAM on page 1
XFER		equ	$F36E		; transfer to/from TPA RAM on page 1
SAUXIN		equ	$F371		;
SAUXOU		equ	$F374		; auxiliary output routine
BLDCHK1		equ	$F377		; routine diskbasic BLOAD
BSVCHK1		equ	$F37A		; routine diskbasic BSAVE
BDOS		equ	$F37D	  	;

; ------------------------------------------------------------------------------
; BIOS/BASIC work area (F380-FFFF)

VARWRK		equ	$F380		; Start of variable workarea for MSX BASIC / BIOS
RDPRIM		equ	$F380
WRPRIM		equ	$F385
RETRTN		equ	$F38B		; on this adres is a simple RET instruction
CLPRIM		equ	$F38C
CLPRM1		equ	$F398		; "jp (ix)" also used as "call CLPRM1+1" = "call (hl)"
LINLEN		equ	$F3B0
CNSDFG		equ	$F3DE
LPTPOS		equ	$F415
PRTFLG		equ	$F416
CURLIN		equ	$F41C
KBUF  		equ	$F41F
YCONTP  	equ     KBUF+58		; KBUF is reused for temporary buffer buffered input, size 128
YCONBF  	equ     KBUF+186	; KBUF is reused for read CON device records, size 127+2
BUF		equ	$F55E
TMPBUF		equ	BUF+10		; temporary buffer
TTYPOS		equ	$F661
VALTYP		equ	$F663
MEMSIZ		equ	$F672
STKTOP		equ	$F674
TXTTAB		equ	$F676
TEMPPT		equ	$F678
TEMPST		equ	$F67A
DSCTMP		equ	$F698
FRETOP		equ	$F69B
SAVSTK		equ	$F6B1
VARTAB		equ	$F6C2
STREND		equ	$F6C6
DAC		equ	$F7F6
ARG		equ	$F847
MAXFIL		equ	$F85F
FILTAB		equ	$F860
NULBUF		equ	$F862
PTRFIL		equ	$F864
FILNAM		equ	$F866
NLONLY		equ	$F87C
SAVEND		equ	$F87D
DRVTBL		equ	$FB21		; disk driver table
BOTTOM		equ	$FC48
HIMEM		equ	$FC4A
CSRSW		equ	$FCA9		; Cursor on/off flag
FLBMEM		equ	$FCAE
RUNBNF		equ	$FCBE
SAVENT		equ	$FCBF
EXPTBL		equ	$FCC1
SLTTBL		equ	$FCC5
SLTATR		equ	$FCC9
PROCNM		equ	$FD89
H_KEYI		equ	$FD9A
H_TIMI  	equ     $FD9F
AFFFF		equ	$FFFF

