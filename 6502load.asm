;
; Keyboard Component Bootloader for 6502 code
; Frank Palazzolo - 2003
;
; Derived from MAZE DEMO
; Copyright 1999-2000, Joseph Zbiciak
;
; ROM header and function stubs adapted from SHELL.SRC by Carl Mueller, Jr.
; This version has been modified to work with as1600.
;

        ORG     $5000
        ROMW    10                      ; rom is 10-bits wide

;       System-wide  equates
VIDEN   EQU     $0020           ; Video enable handshake
RNDLO   EQU     $035E ;..$35F   ; Where we store random numbers.

; EXEC-ROM HEADER

ROMHDR:
        WORD    MODATA
        WORD    RTAB
        WORD    START
        WORD    BKGDATA
        WORD    CARDTAB
        WORD    TITLE

        BYTE    $9F             ; run code after title, clicks on
        BYTE    $00             ; -> to STIC $32
        BYTE    $00             ; 0 = color stack, 1 = f/b mode
        BYTE    9, 9, 9, 9      ; color stack elements 1 - 4
        BYTE    $09             ; border color

        ; Moving Object offsets = last 16 pictures in GRAM
        ; [What is this for?  Do I need it for this program? --JZ]
        DECLE   $180, $190, $1A0, $1B0, $1C0, $1D0, $1E0, $1F0
CARDTAB DECLE   $1, $0
RTAB:   DECLE   $0, $0

; TITLE
;
; Copyright date, title, and code to patch title.

TITLE:  PROC
        BYTE    77, 'Apple II Monitor', 0
        BEGIN

        ; Cyan screen
        MVII    #$09,   R0      ; Color 9 is Cyan (pastels.)
        MVO     R0,     $28     ; Color stack 0 = Cyan
        MVO     R0,     $29     ; Color stack 1 = Cyan
        MVO     R0,     $2A     ; Color stack 2 = Cyan
        MVO     R0,     $2B     ; Color stack 3 = Cyan
        MVO     R0,     $2C     ; Border color = Cyan

        ; Patch the title string to say '=JRMZ=' instead of Mattel.
        CLRR    R1              ; Black
        MVII    #$23D,  R4      ; First 'Mattel' in top-left
        CALL    DRAWSTRING2     ; Write string (ptr in R5)
        STRING  ' Frank Palazzolo  '   ; Guess who?  :-)
        BYTE    0

        CLRR    R1              ; Black
        MVII    #$2D5,  R4      ; Second 'Mattel' in lower-right
        CALL    DRAWSTRING2     ; Write string (ptr in R5)
        STRING  'WOZ&Co'   ; Guess who?  :-)
        BYTE    0

        ; Make all the rest of the text black to match
        MVII    #$243,  R3      ; Start after first JRMZ
        MVII    #7,     R0      ; Mask = 0xFFF8.  That's the 1s complement...
        COMR    R0              ; ...of 7.  (Saves an SDBD.)
        MVII    #146,   R2      ; We only need to touch 146 words.

@@titlelp:
        MVI@    R3,     R1      ; Read a word from the display
        ANDR    R0,     R1      ; Mask the foreground color
        MVO@    R1,     R3      ; Write the word back
        INCR    R3              ; Point to next word
        DECR    R2              ; Decrement our loop count
        BNEQ    @@titlelp       ; Iterate.

        ; Done.
        RETURN                  ; Return to caller

        ENDP

; HANDTAB
; Vector table for handling the controllers.
HANDTAB WORD    0            ; Disc handler
        WORD    0            ; Keypad handler
        WORD    0            ; Upper action btn
        WORD    0            ; Lower Right act btn
        WORD    0            ; Lower Left act btn

; Moving Objects bitmaps
MODATA: DCW     $0

; Background Cards bitmaps

BKGDATA: DCW     $0



;===========================================================================
; START:  Code which runs after title screen.        
START:  PROC
        BEGIN                   ; Save return address on stack

        ; "Local" variables
@@xc    EQU     $300            ; X card position
@@xp    EQU     $301            ; X pixel position
@@yc    EQU     $302            ; Y card position
@@yp    EQU     $303            ; Y pixel position
@@dir   EQU     $304            ; Direction 
@@tdir  EQU     $305            ; Directions we've tried at this intersection
@@move  EQU     $305            ; Updated pixel coordinate
@@axis  EQU     $306            ; Pointer to axis that was updated

        DIS   ; This demo has no interaction and no need for interrupts.

        CLRR    R0
        MVO     R0,     $2C     ; Black border.
        MVO     R0,     $28     ; 

        MVO     R0,     $30     ; Horizontal delay = 0
        MVO     R0,     $31     ; Vertical delay = 0

	;relocate 6502 code (1000 decles from 6000 to B000) 
	
	MVII   #$6000, R4	; Source
	MVII   #$B000, R5       ; Destination
	MVII   #$1000, R0       ; Count
@@l1	MVI@   R4, R1
	MVO@   R1, R5
	DECR   R0
	BNEQ    @@l1	
	
	;jump to 6502 entry point (3000)
	
	CALL   $7571
	DECLE    $09	; jsr RPC command 
	BIDECLE  PBLK 
	
PBLK:
        BIDECLE  $3000
        BYTE   $00
	
@@forever:
        B       @@forever
        ENDP

;
; DRAWSTRING
; Puts an ASCIIZ string pointed to by R0 onscreen.
;
; DRAWSTRING2
; Puts an ASCIIZ string after a JSR R5 onscreen.
;
; INPUTS:
; R0 -- String pointer (if DRAWSTRING)
; R1 -- Screen format word
; R4 -- Output pointer
; R5 -- Return address (also, string if DRAWSTRING2).
;
; OUTPUTS:
; R0 -- Zero if DRAWSTRING2, one if DRAWSTRING
; R1 -- Untouched EXCEPT bit 15 is cleared.
; R4 -- Points just after displayed string.
; R5 -- Points just past end of string.
; R2 and R3 are not modified.
;
DRAWSTRING      PROC
        PSHR    R5
        MOVR    R0,     R5
        SETC
        INCR    R7
DRAWSTRING2:
        CLRC
        SLL     R1,     1
        RRC     R1,     1
        MVI@    R5,     R0      ; Get first char of string
@@tloop:
        SUBI    #32,    R0      ; Shift ASCII range to charset
        SLL     R0,     2       ; Move it to position for BTAB word
        SLL     R0,     1
        XORR    R1,     R0      ; Merge with color info
        MVO@    R0,     R4      ; Write to display
        MVI@    R5,     R0      ; Get next character
        TSTR    R0              ; Is it NUL?
        BNEQ    @@tloop         ; --> No, keep copying then

        SLLC    R1,     1
        ADCR    R0
        SLR     R1,     1
        ADDR    R0,     R7
        JR      R5
        PULR    R7
        ENDP
