; base.inc
; MSX BIOS/BASIC build options


; Target build options
; --------------------
; MSX1		MSX1 BIOS/BASIC
; MSX2		MSX2 BIOS/BASIC/SUBROM


; BIOS/BASIC compile options
; --------------------------
; MSXVER	MSX version
; OPTM		Optmization                     0 = original code  1 = optimized code
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
OPTM	EQU	0
BTSCNT	EQU	6
NDEVFIX	EQU	1
SLOTFIX	EQU	0
KEYBFIX	EQU	0
CHIFIX	EQU	1
DEFINE	SYSHOOKS

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

INTHZ	EQU	50
CHRGEN	EQU	1
DATFMT	EQU	1
KEYTYP	EQU	1
BASVER	EQU	1
CHRCUR	EQU	'$'
CHRFLN	EQU	'\\'
CHRVLN	EQU	'&'
