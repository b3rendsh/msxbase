; keyint2.inc
; International keyboard handler part 2 of 2

; Scancode table
; Remark: last scancode+1,low byte execution address
I1B96:		DEFB	030H,C0F83 % 256	; scancodes 000H-02FH
		DEFB	033H,C0F10 % 256	; SHIFT,CTRL,GRAPH
		DEFB	034H,C0F36 % 256	; CAPS
		DEFB	035H,C0F10 % 256	; CODE
		DEFB	03AH,C0FC3 % 256	; F1,F2,F3,F4,F5
		DEFB	03CH,C0F10 % 256	; ESC,TAB
		DEFB	03DH,C0F46 % 256	; STOP
		DEFB	041H,C0F10 % 256	; BS,SELECT,RETURN,SPACE
		DEFB	042H,C0F06 % 256	; HOME
		DEFB	0FFH,C0F10 % 256	; ins,del,left,up,down,right, numeric pad
