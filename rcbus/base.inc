; base.inc
; MSX BIOS/BASIC build options


; Target build options
; --------------------
; MSX100	Original MSX
; HBMSX		HBIOS MSX standard
; RCMSX		RCBUS customized options

; Customization options
; ---------------------
; HBIOS		Use HBIOS proxy

; HBIOS customization options
; MSXKEY	Keyboard input:  0=con   1=msx keyboard
; MSXVDP	Screen output:   0=con   1=tms9918/v9938  2=con and tms9918/v9938
; MSXPSG	Sound/music:     0=off   1=ay-3-8910
; MSXBOOT	Boot option:     0=basic 1=dos
; DISKID	Disk unit        0=find first large disk

IFDEF MSX100
	DEFINE	MSX1
	VDP0	EQU	$98
	VDP1	EQU	$99
	PSG0	EQU	$A0
	PSG1	EQU	$A1
	PSG2	EQU	$A2
	INTHZ	EQU	50
ENDIF

; Standard settings
IFDEF HBMSX
	DEFINE	MSX1
	DEFINE	HBIOS
	MSXKEY	EQU	1
	MSXVDP	EQU	1
	VDP0	EQU	$98
	VDP1	EQU	$99
	MSXPSG	EQU	1
	PSG0	EQU	$A0
	PSG1	EQU	$A1
	PSG2	EQU	$A2
	MSXBOOT	EQU	1
	DISKID	EQU	0
	INTHZ	EQU	50
ENDIF

; Customized settings
IFDEF RCMSX
	DEFINE	MSX1
	DEFINE	HBIOS
	MSXKEY	EQU	0
	MSXVDP	EQU	2	; must be 1 or 2
	VDP0	EQU	$98	; $40
	VDP1	EQU	$99	; $41
	MSXPSG	EQU	1
	PSG0	EQU	$A0
	PSG1	EQU	$A1
	PSG2	EQU	$A2
	MSXBOOT	EQU	1
	DISKID	EQU	0
	INTHZ	EQU	60	; set equal to timer tick frequency: Z180=50 / TMS=60
ENDIF

; BIOS/BASIC compile options
; --------------------------
; MSXVER	MSX version
; OPTM		Optimization                    0 = original code  1 = optimized code
; BTSCNT	Boot screen wait counter 	6 = normal         2 = national MSX 1
; NDEVFIX	Fix device bug                  0 = not fixed      1 = fixed
; SLOTFIX	Fix slot bug                    0 = not fixed      1 = fixed
; KEYBFIX       Fix keyboard speed              0 = not fixed      1 = fixed
; CHIBFIX       Fix LOAD bug                    0 = not fixed      1 = fixed
; SYSHOOKS	Call system hooks

IFDEF MSX1
	MSXVER	EQU	0
ELSE
	MSXVER	EQU	1
ENDIF
	OPTM	EQU	0	; must be 0 in HBIOS build
	BTSCNT	EQU	6
	NDEVFIX	EQU	1
	SLOTFIX	EQU	0	; not used in HBIOS build
	KEYBFIX	EQU	1	; must be 1 in HBIOS build
	CHIFIX	EQU	1
IFNDEF HBIOS
	DEFINE SYSHOOKS
ENDIF

; Localization
; ------------
; Remark: only country 2 is supported in this source
; CNTRY   Country
;        0 = Japan
;        1 = USA
;        2 = International
;        3 = UK
;        4 = France
;        5 = Germany
;        6 = Italy
;        7 = Spain
;        8 = Arabic
;        9 = Korea
;       10 = Russia

CNTRY	EQU	2


; Symbols used to produce alternate code
; --------------------------------------
; INTHZ   interrupt frequency
; CHRGEN  character generator
;         0 = japanese
;         1 = international
;         2 = USSR ??
; DATFMT  date format
;         0 = Y-M-D
;         1 = M-D-Y
;         2 = D-M-Y
; KEYTYP  keyboard layout
;         0 = Japanese 
;         1 = International (QWERTY/other) 
;         2 = French (AZERTY) 
;         3 = English 
;         4 = German (DIN)
;         5 = USSR
;         6 = Spanish
;         7 = Swedish ??
; BASVER  0 = Japanese
;         1 = International
; Remark: Default is international MSX settings

CHRGEN	EQU	1
DATFMT	EQU	1
KEYTYP	EQU	1
BASVER	EQU	1
CHRCUR	EQU	'$'
CHRFLN	EQU	'\\'
CHRVLN	EQU	'&'

