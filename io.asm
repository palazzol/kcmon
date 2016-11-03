	 	 
	 .export   CRLFOUT, FPINIT
	 
	 .importzp LOC0, LOC1, KSWL, CSWL
	 .import   COUT, RESET

START:   SEI
         JMP   RESET
         
;SETUP INC AND OUTC - TBD

TEXTEN   =   $4043  ; Bit 0 enables overlay text

FPINIT:  LDX   #KSWL
         LDY   #<HINC
         LDA   #>HINC
         STY   LOC0,X
	 STA   LOC1,X
         LDX   #CSWL
         LDY   #<HOUTC
         LDA   #>HOUTC
         STY   LOC0,X
	 STA   LOC1,X
	 LDX   #$90
	 LDA   #$01	; enable overlay text
	 STA   TEXTEN 
	 RTS

; HANDLE OUTPUT - CONVERT APPLE ASCII TO STD ASCII

OUTCBUF  =   $90
OUTC     =   $C6CF

HOUTC:   STA   OUTCBUF
         PHA
         TXA
         PHA
         TYA
         PHA
         LDA   #$00
         STA   $91
         STA   $A8
         LDA   OUTCBUF
	 AND   #$E0
	 CMP   #$80
	 BEQ   DOCTRL
	 LDA   OUTCBUF
	 AND   #$20
	 BEQ   DOC1
	 LDA   OUTCBUF
	 AND   #$3F
	 ORA   #$20
	 JMP   SCHAR
DOC1:    LDA   OUTCBUF
         AND   #$3F
         ORA   #$40
         JMP   SCHAR
DOCTRL:  LDA   OUTCBUF
	 AND   #$1F
SCHAR:	 STA   OUTCBUF
         JSR   OUTC
         PLA
         TAY
         PLA
         TAX
         PLA
         RTS

; HANDLE INPUT - WAIT FOR KEYPRESS - RETURN APPLE ASCII

SCANROW  =   $C5F3
         
HINC:    TXA
         PHA
         TYA
         PHA
HINC0:   JSR   CHECKEY
         BEQ   GOTNONE
         BNE   HINC0
GOTNONE: JSR   CHECKEY
         BEQ   GOTNONE
         STA   $91
         PLA
         TAY
         PLA
         TAX
         LDA   $91
         RTS
         
CHECKEY: LDX   #$00
	 TXA
	 JSR   SCANROW
         BEQ   HINC1
         ASL   A
         ASL   A
         AND   #$0C
         STA   $90
HINC1:   LDX   #$01
         TXA
         JSR   SCANROW
         BEQ   HINC2
         AND   #$03
         STA   $90
HINC2:   LDX   #$02
AGAIN:   TXA
         JSR   SCANROW
         BEQ   NEXTROW
         ; now we have the bit set in A, and the byte in X
         JSR   LOOKUP
         RTS
NEXTROW: INX
	 CPX  #$0A
	 BNE  AGAIN
	 LDA  #$00	 
         RTS

LOOKUP:  LDY  #$00
SHIFT:   ASL
         BCS  GOTIT
         INY
         BCC  SHIFT
         
GOTIT:   DEX
         DEX
         TXA
         ASL
         ASL
         ASL
         STA  $91
         TYA
         CLC
         ADC  $91
         ; now a has offset
         TAY
         LDA  TABLE,Y
         RTS
         
TABLE:   .byte   $00, $AF, $BC, $CE, $D6, $D8, $00, $A0
         .byte   $9D, $AE, $CD, $C2, $C3, $DA, $00, $00
         .byte   $8A, $BA, $CB, $C8, $C6, $D3, $00, $89
         .byte   $DD, $D0, $C9, $D9, $D2, $D7, $00, $D1
         .byte   $9C, $AD, $B9, $B7, $B5, $B3, $00, $B1
         .byte   $AB, $B0, $B8, $B6, $B4, $B2, $00, $DB
         .byte   $8D, $88, $CF, $D5, $D4, $C5, $00, $9B
         .byte   $88, $A7, $CC, $CA, $C7, $C4, $00, $C1
         
CRLFOUT: LDA   #$8D
         JSR   COUT
         LDA   #$8A
         JMP   COUT