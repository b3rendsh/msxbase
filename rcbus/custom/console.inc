; ------------------------------------------------------------------------------
; console.inc
; Custom console definitions
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; Internal keycode definitions which can be invoked using CTRL+[KEY]
; CTRL+[V] emulates CTRL+[STOP] and CTRL+[O] is the [STOP] key.
; Function keys and cursors keys are sent as escape-sequences.
; See also:
;   https://vt100.net/docs/vt100-ug/chapter3.html#T3-5
;   https://www.msx.org/wiki/MSX_Characters_and_Control_Codes
; ------------------------------------------------------------------------------

; Common control keys / characters
KNUL		EQU	$00		; Ctrl-[SPACE] Ignored on input
KBELL		EQU	$07		; Ctrl-G Bell
KBS		EQU	$08		; Ctrl-H Backspace (cursor left)
KTAB		EQU	$09		; Ctrl-I Tab
KLF		EQU	$0A		; Ctrl-J Linefeed (cursor down)
KCR		EQU	$0D		; Ctrl-M Enter (cursor home)
KESC		EQU	$1B		; Ctrl-[ Escape

; Standard MSX control keys
KPREV		EQU	$02		; Ctrl-B Move to previous word (backward)
KERA_END	EQU	$05		; Ctrl-E Erase cursor to end
KFWD		EQU	$06		; Ctrl-F Move to next word (forward)
KHOME		EQU	$0B		; Ctrl-K Home
KCLS		EQU	$0C		; Ctrl-L Clear screen
KEND		EQU	$0E		; Ctrl-N End
KINS		EQU	$12		; Ctrl-R Insert/overwrite mode
KERA_HOME	EQU	$15		; Ctrl-U Erase to home
KDEL		EQU	$7F		; Delete character

	IF MSXKEY = 1
; Standard MSX cursor control keys
KGRAPH		EQU	$01		; Ctrl-A Graph [64-95]
KSEL		EQU	$18		; Ctrl-X [SELECT] key
KCUF		EQU	$1C		; Ctrl-\ Cursor right (forward)
KCUB		EQU	$1D		; Ctrl-] Cursor left (backward)
KCUU		EQU	$1E		; Ctrl-~ Cursor up
KCUD		EQU	$1F		; Ctrl-? Cursor down
	ELSE
; Modified MSX cursor control keys
KGRAPH		EQU	$1C		; Ctrl-\ Graph [64-95]
KSEL		EQU	$1D		; Ctrl-] [SELECT] key
KCUF		EQU	$04		; Ctrl-D Cursor right (forward)
KCUB		EQU	$01		; Ctrl-A Cursor left (backward)
KCUU		EQU	$17		; Ctrl-W Cursor up
KCUD		EQU	$18		; Ctrl-X Cursor down
	ENDIF

; Custom MSX keyboard keys
KSTOP		EQU	$0F 		; Ctrl-O [STOP] key
KDEAD		EQU	$14		; Ctrl-T [DEAD] key
KCTL_STOP	EQU	$16		; Ctrl-V CONTROL+[STOP]
KCODE		EQU	$19 		; Ctrl-Y [CODE] key

; CP/M and MSX-DOS 
KCTLC		EQU	$03		; Ctrl-C Cancel process (warm boot)
KPECHO		EQU	$10		; Ctrl-P Toggle echo to printer
KCONT		EQU	$11		; Ctrl-Q Continue scroll
KSCROLL		EQU	$13		; Ctrl-S Stop scroll
KCTLZ		EQU	$1A		; Ctrl-Z Terminate input

