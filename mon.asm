***************************
*                         *
*        APPLE II         *
*     SYSTEM MONITOR      *
*                         *
*    COPYRIGHT 1977 BY    *
*   APPLE COMPUTER, INC.  *
*                         *
*   ALL RIGHTS RESERVED   *
*                         *
*       S. WOZNIAK        *
*        A. BAUM          *
*                         *
***************************
                          ; TITLE "APPLE II SYSTEM MONITOR"
LOC0     EQU   $00
LOC1     EQU   $01
WNDLFT   EQU   $20
WNDWDTH  EQU   $21
WNDTOP   EQU   $22
WNDBTM   EQU   $23
CH       EQU   $24
CV       EQU   $25
GBASL    EQU   $26
GBASH    EQU   $27
BASL     EQU   $28
BASH     EQU   $29
BAS2L    EQU   $2A
BAS2H    EQU   $2B
H2       EQU   $2C
LMNEM    EQU   $2C
RTNL     EQU   $2C
V2       EQU   $2D
RMNEM    EQU   $2D
RTNH     EQU   $2D
MASK     EQU   $2E
CHKSUM   EQU   $2E
FORMAT   EQU   $2E
LASTIN   EQU   $2F
LENGTH   EQU   $2F
SIGN     EQU   $2F
COLOR    EQU   $30
MODE     EQU   $31
INVFLG   EQU   $32
PROMPT   EQU   $33
YSAV     EQU   $34
YSAV1    EQU   $35
CSWL     EQU   $36
CSWH     EQU   $37
KSWL     EQU   $38
KSWH     EQU   $39
PCL      EQU   $3A
PCH      EQU   $3B
XQT      EQU   $3C
A1L      EQU   $3C
A1H      EQU   $3D
A2L      EQU   $3E
A2H      EQU   $3F
A3L      EQU   $40
A3H      EQU   $41
A4L      EQU   $42
A4H      EQU   $43
A5L      EQU   $44
A5H      EQU   $45
ACC      EQU   $45
XREG     EQU   $46
YREG     EQU   $47
STATUS   EQU   $48
SPNT     EQU   $49
RNDL     EQU   $4E
RNDH     EQU   $4F
ACL      EQU   $50
ACH      EQU   $51
XTNDL    EQU   $52
XTNDH    EQU   $53
AUXL     EQU   $54
AUXH     EQU   $55
PICK     EQU   $95
IN       EQU   $0200
USRADR   EQU   $03F8
NMI      EQU   $03FB
IRQLOC   EQU   $03FE
IOADR    EQU   $C000
KBD      EQU   $C000
KBDSTRB  EQU   $C010
TAPEOUT  EQU   $C020
SPKR     EQU   $C030
TXTCLR   EQU   $C050
TXTSET   EQU   $C051
MIXCLR   EQU   $C052
MIXSET   EQU   $C053
LOWSCR   EQU   $C054
HISCR    EQU   $C055
LORES    EQU   $C056
HIRES    EQU   $C057
TAPEIN   EQU   $C060
PADDL0   EQU   $C064
PTRIG    EQU   $C070
BASIC    EQU   $E000
BASIC2   EQU   $E003
         ORG   $F800      ;ROM START ADDRESS
PLOT     LSR              ;Y-COORD/2
         PHP              ;SAVE LSB IN CARRY
         JSR   GBASCALC   ;CALC BASE ADR IN GBASL,H
         PLP              ;RESTORE LSB FROM CARRY
         LDA   #$0F       ;MASK $0F IF EVEN
         BCC   RTMASK
         ADC   #$E0       ;MASK $F0 IF ODD
RTMASK   STA   MASK
PLOT1    LDA   (GBASL),Y  ;DATA
         EOR   COLOR      ; EOR COLOR
         AND   MASK       ;  AND MASK
         EOR   (GBASL),Y  ;   EOR DATA
         STA   (GBASL),Y  ;    TO DATA
         RTS
HLINE    JSR   PLOT       ;PLOT SQUARE
HLINE1   CPY   H2         ;DONE?
         BCS   RTS1       ; YES, RETURN
         INY              ; NO, INC INDEX (X-COORD)
         JSR   PLOT1      ;PLOT NEXT SQUARE
         BCC   HLINE1     ;ALWAYS TAKEN
VLINEZ   ADC   #$01       ;NEXT Y-COORD
VLINE    PHA              ; SAVE ON STACK
         JSR   PLOT       ; PLOT SQUARE
         PLA
         CMP   V2         ;DONE?
         BCC   VLINEZ     ; NO, LOOP
RTS1     RTS
CLRSCR   LDY   #$2F       ;MAX Y, FULL SCRN CLR
         BNE   CLRSC2     ;ALWAYS TAKEN
CLRTOP   LDY   #$27       ;MAX Y, TOP SCREEN CLR
CLRSC2   STY   V2         ;STORE AS BOTTOM COORD
                          ; FOR VLINE CALLS
         LDY   #$27       ;RIGHTMOST X-COORD (COLUMN)
CLRSC3   LDA   #$00       ;TOP COORD FOR VLINE CALLS
         STA   COLOR      ;CLEAR COLOR (BLACK)
         JSR   VLINE      ;DRAW VLINE
         DEY              ;NEXT LEFTMOST X-COORD
         BPL   CLRSC3     ;LOOP UNTIL DONE
         RTS
GBASCALC PHA              ;FOR INPUT 000DEFGH
         LSR
         AND   #$03
         ORA   #$04       ;  GENERATE GBASH=000001FG
         STA   GBASH
         PLA              ;  AND GBASL=HDEDE000
         AND   #$18
         BCC   GBCALC
         ADC   #$7F
GBCALC   STA   GBASL
         ASL
         ASL
         ORA   GBASL
         STA   GBASL
         RTS
NXTCOL   LDA   COLOR      ;INCREMENT COLOR BY 3
         CLC
         ADC   #$03
SETCOL   AND   #$0F       ;SETS COLOR=17*A MOD 16
         STA   COLOR
         ASL              ;BOTH HALF BYTES OF COLOR EQUAL
         ASL
         ASL
         ASL
         ORA   COLOR
         STA   COLOR
         RTS
SCRN     LSR              ;READ SCREEN Y-COORD/2
         PHP              ;SAVE LSB (CARRY)
         JSR   GBASCALC   ;CALC BASE ADDRESS
         LDA   (GBASL),Y  ;GET BYTE
         PLP              ;RESTORE LSB FROM CARRY
SCRN2    BCC   RTMSKZ     ;IF EVEN, USE LO H
         LSR
         LSR
         LSR              ;SHIFT HIGH HALF BYTE DOWN
         LSR
RTMSKZ   AND   #$0F       ;MASK 4-BITS
         RTS
INSDS1   LDX   PCL        ;PRINT PCL,H
         LDY   PCH
         JSR   PRYX2
         JSR   PRBLNK     ;FOLLOWED BY A BLANK
         LDA   (PCL,X)    ;GET OP CODE
INSDS2   TAY
         LSR              ;EVEN/ODD TEST
         BCC   IEVEN
         ROR              ;BIT 1 TEST
         BCS   ERR        ;XXXXXX11 INVALID OP
         CMP   #$A2
         BEQ   ERR        ;OPCODE $89 INVALID
         AND   #$87       ;MASK BITS
IEVEN    LSR              ;LSB INTO CARRY FOR L/R TEST
         TAX
         LDA   FMT1,X     ;GET FORMAT INDEX BYTE
         JSR   SCRN2      ;R/L H-BYTE ON CARRY
         BNE   GETFMT
ERR      LDY   #$80       ;SUBSTITUTE $80 FOR INVALID OPS
         LDA   #$00       ;SET PRINT FORMAT INDEX TO 0
GETFMT   TAX
         LDA   FMT2,X     ;INDEX INTO PRINT FORMAT TABLE
         STA   FORMAT     ;SAVE FOR ADR FIELD FORMATTING
         AND   #$03       ;MASK FOR 2-BIT LENGTH
                          ; (P=1 BYTE, 1=2 BYTE, 2=3 BYTE)
         STA   LENGTH
         TYA              ;OPCODE
         AND   #$8F       ;MASK FOR 1XXX1010 TEST
         TAX              ; SAVE IT
         TYA              ;OPCODE TO A AGAIN
         LDY   #$03
         CPX   #$8A
         BEQ   MNNDX3
MNNDX1   LSR
         BCC   MNNDX3     ;FORM INDEX INTO MNEMONIC TABLE
         LSR
MNNDX2   LSR              ;1) 1XXX1010->00101XXX
         ORA   #$20       ;2) XXXYYY01->00111XXX
         DEY              ;3) XXXYYY10->00110XXX
         BNE   MNNDX2     ;4) XXXYY100->00100XXX
         INY              ;5) XXXXX000->000XXXXX
MNNDX3   DEY
         BNE   MNNDX1
         RTS
         DFB   $FF,$FF,$FF
INSTDSP  JSR   INSDS1     ;GEN FMT, LEN BYTES
         PHA              ;SAVE MNEMONIC TABLE INDEX
PRNTOP   LDA   (PCL),Y
         JSR   PRBYTE
         LDX   #$01       ;PRINT 2 BLANKS
PRNTBL   JSR   PRBL2
         CPY   LENGTH     ;PRINT INST (1-3 BYTES)
         INY              ;IN A 12 CHR FIELD
         BCC   PRNTOP
         LDX   #$03       ;CHAR COUNT FOR MNEMONIC PRINT
         CPY   #$04
         BCC   PRNTBL
         PLA              ;RECOVER MNEMONIC INDEX
         TAY
         LDA   MNEML,Y
         STA   LMNEM      ;FETCH 3-CHAR MNEMONIC
         LDA   MNEMR,Y    ;  (PACKED IN 2-BYTES)
         STA   RMNEM
PRMN1    LDA   #$00
         LDY   #$05
PRMN2    ASL   RMNEM      ;SHIFT 5 BITS OF
         ROL   LMNEM      ;  CHARACTER INTO A
         ROL              ;    (CLEARS CARRY)
         DEY
         BNE   PRMN2
         ADC   #$BF       ;ADD "?" OFFSET
         JSR   COUT       ;OUTPUT A CHAR OF MNEM
         DEX
         BNE   PRMN1
         JSR   PRBLNK     ;OUTPUT 3 BLANKS
         LDY   LENGTH
         LDX   #$06       ;CNT FOR 6 FORMAT BITS
PRADR1   CPX   #$03
         BEQ   PRADR5     ;IF X=3 THEN ADDR.
PRADR2   ASL   FORMAT
         BCC   PRADR3
         LDA   CHAR1-1,X
         JSR   COUT
         LDA   CHAR2-1,X
         BEQ   PRADR3
         JSR   COUT
PRADR3   DEX
         BNE   PRADR1
         RTS
PRADR4   DEY
         BMI   PRADR2
         JSR   PRBYTE
PRADR5   LDA   FORMAT
         CMP   #$E8       ;HANDLE REL ADR MODE
         LDA   (PCL),Y    ;SPECIAL (PRINT TARGET,
         BCC   PRADR4     ;  NOT OFFSET)
RELADR   JSR   PCADJ3
         TAX              ;PCL,PCH+OFFSET+1 TO A,Y
         INX
         BNE   PRNTYX     ;+1 TO Y,X
         INY
PRNTYX   TYA
PRNTAX   JSR   PRBYTE     ;OUTPUT TARGET ADR
PRNTX    TXA              ;  OF BRANCH AND RETURN
         JMP   PRBYTE
PRBLNK   LDX   #$03       ;BLANK COUNT
PRBL2    LDA   #$A0       ;LOAD A SPACE
PRBL3    JSR   COUT       ;OUTPUT A BLANK
         DEX
         BNE   PRBL2      ;LOOP UNTIL COUNT=0
         RTS
PCADJ    SEC              ;0=1-BYTE, 1=2-BYTE
PCADJ2   LDA   LENGTH     ;  2=3-BYTE
PCADJ3   LDY   PCH
         TAX              ;TEST DISPLACEMENT SIGN
         BPL   PCADJ4     ;  (FOR REL BRANCH)
         DEY              ;EXTEND NEG BY DEC PCH
PCADJ4   ADC   PCL
         BCC   RTS2       ;PCL+LENGTH(OR DISPL)+1 TO A
         INY              ;  CARRY INTO Y (PCH)
RTS2     RTS
* FMT1 BYTES:    XXXXXXY0 INSTRS
* IF Y=0         THEN LEFT HALF BYTE
* IF Y=1         THEN RIGHT HALF BYTE
*                   (X=INDEX)
FMT1     DFB   $04,$20,$54,$30,$0D

         DFB   $80,$04,$90,$03,$22

         DFB   $54,$33,$0D,$80,$04

         DFB   $90,$04,$20,$54,$33

         DFB   $0D,$80,$04,$90,$04

         DFB   $20,$54,$3B,$0D,$80

         DFB   $04,$90,$00,$22,$44

         DFB   $33,$0D,$C8,$44,$00

         DFB   $11,$22,$44,$33,$0D

         DFB   $C8,$44,$A9,$01,$22

         DFB   $44,$33,$0D,$80,$04

         DFB   $90,$01,$22,$44,$33

         DFB   $0D,$80,$04,$90

         DFB   $26,$31,$87,$9A ;$ZZXXXY01 INSTR'S

FMT2     DFB   $00        ;ERR
         DFB   $21        ;IMM
         DFB   $81        ;Z-PAGE
         DFB   $82        ;ABS
         DFB   $00        ;IMPLIED
         DFB   $00        ;ACCUMULATOR
         DFB   $59        ;(ZPAG,X)
         DFB   $4D        ;(ZPAG),Y
         DFB   $91        ;ZPAG,X
         DFB   $92        ;ABS,X
         DFB   $86        ;ABS,Y
         DFB   $4A        ;(ABS)
         DFB   $85        ;ZPAG,Y
         DFB   $9D        ;RELATIVE
CHAR1    ASC   ",),#($"

CHAR2    DFB   $D9,$00,$D8,$A4,$A4,$00

*CHAR2: "Y",0,"X$$",0
* MNEML IS OF FORM:
*  (A) XXXXX000
*  (B) XXXYY100
*  (C) 1XXX1010
*  (D) XXXYYY10
*  (E) XXXYYY01
*      (X=INDEX)
MNEML    DFB   $1C,$8A,$1C,$23,$5D,$8B

         DFB   $1B,$A1,$9D,$8A,$1D,$23

         DFB   $9D,$8B,$1D,$A1,$00,$29

         DFB   $19,$AE,$69,$A8,$19,$23

         DFB   $24,$53,$1B,$23,$24,$53

         DFB   $19,$A1    ;(A) FORMAT ABOVE
         DFB   $00,$1A,$5B,$5B,$A5,$69

         DFB   $24,$24    ;(B) FORMAT
         DFB   $AE,$AE,$A8,$AD,$29,$00

         DFB   $7C,$00    ;(C) FORMAT
         DFB   $15,$9C,$6D,$9C,$A5,$69

         DFB   $29,$53    ;(D) FORMAT
         DFB   $84,$13,$34,$11,$A5,$69

         DFB   $23,$A0    ;(E) FORMAT
MNEMR    DFB   $D8,$62,$5A,$48,$26,$62

         DFB   $94,$88,$54,$44,$C8,$54

         DFB   $68,$44,$E8,$94,$00,$B4

         DFB   $08,$84,$74,$B4,$28,$6E

         DFB   $74,$F4,$CC,$4A,$72,$F2

         DFB   $A4,$8A    ;(A) FORMAT
         DFB   $00,$AA,$A2,$A2,$74,$74

         DFB   $74,$72    ;(B) FORMAT
         DFB   $44,$68,$B2,$32,$B2,$00

         DFB   $22,$00    ;(C) FORMAT
         DFB   $1A,$1A,$26,$26,$72,$72

         DFB   $88,$C8    ;(D) FORMAT
         DFB   $C4,$CA,$26,$48,$44,$44

         DFB   $A2,$C8    ;(E) FORMAT
         DFB   $FF,$FF,$FF
STEP     JSR   INSTDSP    ;DISASSEMBLE ONE INST
         PLA              ;  AT (PCL,H)
         STA   RTNL       ;ADJUST TO USER
         PLA              ;  STACK. SAVE
         STA   RTNH       ;  RTN ADR.
         LDX   #$08
XQINIT   LDA   INITBL-1,X ;INIT XEQ AREA
         STA   XQT,X
         DEX
         BNE   XQINIT
         LDA   (PCL,X)    ;USER OPCODE BYTE
         BEQ   XBRK       ;SPECIAL IF BREAK
         LDY   LENGTH     ;LEN FROM DISASSEMBLY
         CMP   #$20
         BEQ   XJSR       ;HANDLE JSR, RTS, JMP,
         CMP   #$60       ;  JMP (), RTI SPECIAL
         BEQ   XRTS
         CMP   #$4C
         BEQ   XJMP
         CMP   #$6C
         BEQ   XJMPAT
         CMP   #$40
         BEQ   XRTI
         AND   #$1F
         EOR   #$14
         CMP   #$04       ;COPY USER INST TO XEQ AREA
         BEQ   XQ2        ;  WITH TRAILING NOPS
XQ1      LDA   (PCL),Y    ;CHANGE REL BRANCH
XQ2      STA   XQT,Y      ;  DISP TO 4 FOR
         DEY              ;  JMP TO BRANCH OR
         BPL   XQ1        ;  NBRANCH FROM XEQ.
         JSR   RESTORE    ;RESTORE USER REG CONTENTS.
         JMP   XQT        ;XEQ USER OP FROM RAM
IRQ      STA   ACC        ;  (RETURN TO NBRANCH)
         PLA
         PHA              ;**IRQ HANDLER
         ASL
         ASL
         ASL
         BMI   BREAK      ;TEST FOR BREAK
         JMP   (IRQLOC)   ;USER ROUTINE VECTOR IN RAM
BREAK    PLP
         JSR   SAV1       ;SAVE REG'S ON BREAK
         PLA              ;  INCLUDING PC
         STA   PCL
         PLA
         STA   PCH
XBRK     JSR   INSDS1     ;PRINT USER PC.
         JSR   RGDSP1     ;  AND REG'S
         JMP   MON        ;GO TO MONITOR
XRTI     CLC
         PLA              ;SIMULATE RTI BY EXPECTING
         STA   STATUS     ;  STATUS FROM STACK, THEN RTS
XRTS     PLA              ;RTS SIMULATION
         STA   PCL        ;  EXTRACT PC FROM STACK
         PLA              ;  AND UPDATE PC BY 1 (LEN=0)
PCINC2   STA   PCH
PCINC3   LDA   LENGTH     ;UPDATE PC BY LEN
         JSR   PCADJ3
         STY   PCH
         CLC
         BCC   NEWPCL
XJSR     CLC
         JSR   PCADJ2     ;UPDATE PC AND PUSH
         TAX              ;  ONTO STACH FOR
         TYA              ;  JSR SIMULATE
         PHA
         TXA
         PHA
         LDY   #$02
XJMP     CLC
XJMPAT   LDA   (PCL),Y
         TAX              ;LOAD PC FOR JMP,
         DEY              ;  (JMP) SIMULATE.
         LDA   (PCL),Y
         STX   PCH
NEWPCL   STA   PCL
         BCS   XJMP
RTNJMP   LDA   RTNH
         PHA
         LDA   RTNL
         PHA
REGDSP   JSR   CROUT      ;DISPLAY USER REG
RGDSP1   LDA   #ACC       ;  CONTENTS WITH
         STA   A3L        ;  LABELS
         LDA   #ACC/256
         STA   A3H
         LDX   #$FB
RDSP1    LDA   #$A0
         JSR   COUT
         LDA   RTBL-$FB,X
         JSR   COUT
         LDA   #$BD
         JSR   COUT
         LDA   ACC+5,X
         JSR   PRBYTE
         INX
         BMI   RDSP1
         RTS
BRANCH   CLC              ;BRANCH TAKEN,
         LDY   #$01       ;  ADD LEN+2 TO PC
         LDA   (PCL),Y
         JSR   PCADJ3
         STA   PCL
         TYA
         SEC
         BCS   PCINC2
NBRNCH   JSR   SAVE       ;NORMAL RETURN AFTER
         SEC              ;  XEQ USER OF
         BCS   PCINC3     ;GO UPDATE PC
INITBL   NOP
         NOP              ;DUMMY FILL FOR
         JMP   NBRNCH     ;  XEQ AREA
         JMP   BRANCH
RTBL     DFB   $C1
         DFB   $D8
         DFB   $D9
         DFB   $D0
         DFB   $D3
PREAD    LDA   PTRIG      ;TRIGGER PADDLES
         LDY   #$00       ;INIT COUNT
         NOP              ;COMPENSATE FOR 1ST COUNT
         NOP
PREAD2   LDA   PADDL0,X   ;COUNT Y-REG EVERY
         BPL   RTS2D      ;  12 USEC
         INY
         BNE   PREAD2     ;  EXIT AT 255 MAX
         DEY
RTS2D    RTS
INIT     LDA   #$00       ;CLR STATUS FOR DEBUG
         STA   STATUS     ;  SOFTWARE
         LDA   LORES
         LDA   LOWSCR     ;INIT VIDEO MODE
SETTXT   LDA   TXTSET     ;SET FOR TEXT MODE
         LDA   #$00       ;  FULL SCREEN WINDOW
         BEQ   SETWND
SETGR    LDA   TXTCLR     ;SET FOR GRAPHICS MODE
         LDA   MIXSET     ;  LOWER 4 LINES AS
         JSR   CLRTOP     ;  TEXT WINDOW
         LDA   #$14
SETWND   STA   WNDTOP     ;SET FOR 40 COL WINDOW
         LDA   #$00       ;  TOP IN A-REG,
         STA   WNDLFT     ;  BTTM AT LINE 24
         LDA   #$28
         STA   WNDWDTH
         LDA   #$18
         STA   WNDBTM     ;  VTAB TO ROW 23
         LDA   #$17
TABV     STA   CV         ;VTABS TO ROW IN A-REG
         JMP   VTAB
MULPM    JSR   MD1        ;ABS VAL OF AC AUX
MUL      LDY   #$10       ;INDEX FOR 16 BITS
MUL2     LDA   ACL        ;ACX * AUX + XTND
         LSR              ; TO AC, XTND
         BCC   MUL4       ;IF NO CARRY,
         CLC              ; NO PARTIAL PROD.
         LDX   #$FE
MUL3     LDA   XTNDL+2,X  ;ADD MPLCND (AUX)
         ADC   AUXL+2,X   ; TO PARTIAL PROD
         STA   XTNDL+2,X  ; (XTND)
         INX
         BNE   MUL3
MUL4     LDX   #$03
MUL5     DFB   $76
         DFB   $50
         DEX
         BPL   MUL5
         DEY
         BNE   MUL2
         RTS
DIVPM    JSR   MD1        ;ABS VAL OF AC, AUX.
DIV      LDY   #$10       ;INDEX FOR 16 BITS
DIV2     ASL   ACL
         ROL   ACH
         ROL   XTNDL      ;XTND/AUX
         ROL   XTNDH      ;  TO AC.
         SEC
         LDA   XTNDL
         SBC   AUXL       ;MOD TO XTND.
         TAX
         LDA   XTNDH
         SBC   AUXH
         BCC   DIV3
         STX   XTNDL
         STA   XTNDH
         INC   ACL
DIV3     DEY
         BNE   DIV2
         RTS
MD1      LDY   #$00       ;ABS VAL OF AC, AUX
         STY   SIGN       ;  WITH RESULT SIGN
         LDX   #AUXL      ;  IN LSB OF SIGN.
         JSR   MD3
         LDX   #ACL
MD3      LDA   LOC1,X     ;X SPECIFIES AC OR AUX
         BPL   MDRTS
         SEC
         TYA
         SBC   LOC0,X     ;COMPL SPECIFIED REG
         STA   LOC0,X     ;  IF NEG.
         TYA
         SBC   LOC1,X
         STA   LOC1,X
         INC   SIGN
MDRTS    RTS
BASCALC  PHA              ;CALC BASE ADR IN BASL,H
         LSR              ;  FOR GIVEN LINE NO
         AND   #$03       ;  0<=LINE NO.<=$17
         ORA   #$04       ;ARG=000ABCDE, GENERATE
         STA   BASH       ;  BASH=000001CD
         PLA              ;  AND
         AND   #$18       ;  BASL=EABAB000
         BCC   BSCLC2
         ADC   #$7F
BSCLC2   STA   BASL
         ASL
         ASL
         ORA   BASL
         STA   BASL
         RTS
BELL1    CMP   #$87       ;BELL CHAR? (CNTRL-G)
         BNE   RTS2B      ;  NO, RETURN
         LDA   #$40       ;DELAY .01 SECONDS
         JSR   WAIT
         LDY   #$C0
BELL2    LDA   #$0C       ;TOGGLE SPEAKER AT
         JSR   WAIT       ;  1 KHZ FOR .1 SEC.
         LDA   SPKR
         DEY
         BNE   BELL2
RTS2B    RTS
STOADV   LDY   CH         ;CURSOR H INDEX TO Y-REG
         STA   (BASL),Y   ;STORE CHAR IN LINE
ADVANCE  INC   CH         ;INCREMENT CURSOR H INDEX
         LDA   CH         ;  (MOVE RIGHT)
         CMP   WNDWDTH    ;BEYOND WINDOW WIDTH?
         BCS   CR         ;  YES CR TO NEXT LINE
RTS3     RTS              ;  NO,RETURN
VIDOUT   CMP   #$A0       ;CONTROL CHAR?
         BCS   STOADV     ;  NO,OUTPUT IT.
         TAY              ;INVERSE VIDEO?
         BPL   STOADV     ;  YES, OUTPUT IT.
         CMP   #$8D       ;CR?
         BEQ   CR         ;  YES.
         CMP   #$8A       ;LINE FEED?
         BEQ   LF         ;  IF SO, DO IT.
         CMP   #$88       ;BACK SPACE? (CNTRL-H)
         BNE   BELL1      ;  NO, CHECK FOR BELL.
BS       DEC   CH         ;DECREMENT CURSOR H INDEX
         BPL   RTS3       ;IF POS, OK. ELSE MOVE UP
         LDA   WNDWDTH    ;SET CH TO WNDWDTH-1
         STA   CH
         DEC   CH         ;(RIGHTMOST SCREEN POS)
UP       LDA   WNDTOP     ;CURSOR V INDEX
         CMP   CV
         BCS   RTS4       ;IF TOP LINE THEN RETURN
         DEC   CV         ;DEC CURSOR V-INDEX
VTAB     LDA   CV         ;GET CURSOR V-INDEX
VTABZ    JSR   BASCALC    ;GENERATE BASE ADR
         ADC   WNDLFT     ;ADD WINDOW LEFT INDEX
         STA   BASL       ;TO BASL
RTS4     RTS
ESC1     EOR   #$C0       ;ESC?
         BEQ   HOME       ;  IF SO, DO HOME AND CLEAR
         ADC   #$FD       ;ESC-A OR B CHECK
         BCC   ADVANCE    ;  A, ADVANCE
         BEQ   BS         ;  B, BACKSPACE
         ADC   #$FD       ;ESC-C OR D CHECK
         BCC   LF         ;  C, DOWN
         BEQ   UP         ;  D, GO UP
         ADC   #$FD       ;ESC-E OR F CHECK
         BCC   CLREOL     ;  E, CLEAR TO END OF LINE
         BNE   RTS4       ;  NOT F, RETURN
CLREOP   LDY   CH         ;CURSOR H TO Y INDEX
         LDA   CV         ;CURSOR V TO A-REGISTER
CLEOP1   PHA              ;SAVE CURRENT LINE ON STK
         JSR   VTABZ      ;CALC BASE ADDRESS
         JSR   CLEOLZ     ;CLEAR TO EOL, SET CARRY
         LDY   #$00       ;CLEAR FROM H INDEX=0 FOR REST
         PLA              ;INCREMENT CURRENT LINE
         ADC   #$00       ;(CARRY IS SET)
         CMP   WNDBTM     ;DONE TO BOTTOM OF WINDOW?
         BCC   CLEOP1     ;  NO, KEEP CLEARING LINES
         BCS   VTAB       ;  YES, TAB TO CURRENT LINE
HOME     LDA   WNDTOP     ;INIT CURSOR V
         STA   CV         ;  AND H-INDICES
         LDY   #$00
         STY   CH         ;THEN CLEAR TO END OF PAGE
         BEQ   CLEOP1
CR       LDA   #$00       ;CURSOR TO LEFT OF INDEX
         STA   CH         ;(RET CURSOR H=0)
LF       INC   CV         ;INCR CURSOR V(DOWN 1 LINE)
         LDA   CV
         CMP   WNDBTM     ;OFF SCREEN?
         BCC   VTABZ      ;  NO, SET BASE ADDR
         DEC   CV         ;DECR CURSOR V (BACK TO BOTTOM)
SCROLL   LDA   WNDTOP     ;START AT TOP OF SCRL WNDW
         PHA
         JSR   VTABZ      ;GENERATE BASE ADR
SCRL1    LDA   BASL       ;COPY BASL,H
         STA   BAS2L      ;  TO BAS2L,H
         LDA   BASH
         STA   BAS2H
         LDY   WNDWDTH    ;INIT Y TO RIGHTMOST INDEX
         DEY              ;  OF SCROLLING WINDOW
         PLA
         ADC   #$01       ;INCR LINE NUMBER
         CMP   WNDBTM     ;DONE?
         BCS   SCRL3      ;  YES, FINISH
         PHA
         JSR   VTABZ      ;FORM BASL,H (BASE ADDR)
SCRL2    LDA   (BASL),Y   ;MOVE A CHR UP ON LINE
         STA   (BAS2L),Y
         DEY              ;NEXT CHAR OF LINE
         BPL   SCRL2
         BMI   SCRL1      ;NEXT LINE (ALWAYS TAKEN)
SCRL3    LDY   #$00       ;CLEAR BOTTOM LINE
         JSR   CLEOLZ     ;GET BASE ADDR FOR BOTTOM LINE
         BCS   VTAB       ;CARRY IS SET
CLREOL   LDY   CH         ;CURSOR H INDEX
CLEOLZ   LDA   #$A0
CLEOL2   STA   (BASL),Y   ;STORE BLANKS FROM 'HERE'
         INY              ;  TO END OF LINES (WNDWDTH)
         CPY   WNDWDTH
         BCC   CLEOL2
         RTS
WAIT     SEC
WAIT2    PHA
WAIT3    SBC   #$01
         BNE   WAIT3      ;1.0204 USEC
         PLA              ;(13+27/2*A+5/2*A*A)
         SBC   #$01
         BNE   WAIT2
         RTS
NXTA4    INC   A4L        ;INCR 2-BYTE A4
         BNE   NXTA1      ;  AND A1
         INC   A4H
NXTA1    LDA   A1L        ;INCR 2-BYTE A1.
         CMP   A2L
         LDA   A1H        ;  AND COMPARE TO A2
         SBC   A2H
         INC   A1L        ;  (CARRY SET IF >=)
         BNE   RTS4B
         INC   A1H
RTS4B    RTS
HEADR    LDY   #$4B       ;WRITE A*256 'LONG 1'
         JSR   ZERDLY     ;  HALF CYCLES
         BNE   HEADR      ;  (650 USEC EACH)
         ADC   #$FE
         BCS   HEADR      ;THEN A 'SHORT 0'
         LDY   #$21       ;  (400 USEC)
WRBIT    JSR   ZERDLY     ;WRITE TWO HALF CYCLES
         INY              ;  OF 250 USEC ('0')
         INY              ;  OR 500 USEC ('0')
ZERDLY   DEY
         BNE   ZERDLY
         BCC   WRTAPE     ;Y IS COUNT FOR
         LDY   #$32       ;  TIMING LOOP
ONEDLY   DEY
         BNE   ONEDLY
WRTAPE   LDY   TAPEOUT
         LDY   #$2C
         DEX
         RTS
RDBYTE   LDX   #$08       ;8 BITS TO READ
RDBYT2   PHA              ;READ TWO TRANSITIONS
         JSR   RD2BIT     ;  (FIND EDGE)
         PLA
         ROL              ;NEXT BIT
         LDY   #$3A       ;COUNT FOR SAMPLES
         DEX
         BNE   RDBYT2
         RTS
RD2BIT   JSR   RDBIT
RDBIT    DEY              ;DECR Y UNTIL
         LDA   TAPEIN     ; TAPE TRANSITION
         EOR   LASTIN
         BPL   RDBIT
         EOR   LASTIN
         STA   LASTIN
         CPY   #$80       ;SET CARRY ON Y
         RTS
RDKEY    LDY   CH
         LDA   (BASL),Y   ;SET SCREEN TO FLASH
         PHA
         AND   #$3F
         ORA   #$40
         STA   (BASL),Y
         PLA
         JMP   (KSWL)     ;GO TO USER KEY-IN
KEYIN    INC   RNDL
         BNE   KEYIN2     ;INCR RND NUMBER
         INC   RNDH
KEYIN2   BIT   KBD        ;KEY DOWN?
         BPL   KEYIN      ;  LOOP
         STA   (BASL),Y   ;REPLACE FLASHING SCREEN
         LDA   KBD        ;GET KEYCODE
         BIT   KBDSTRB    ;CLR KEY STROBE
         RTS
ESC      JSR   RDKEY      ;GET KEYCODE
         JSR   ESC1       ;  HANDLE ESC FUNC.
RDCHAR   JSR   RDKEY      ;READ KEY
         CMP   #$9B       ;ESC?
         BEQ   ESC        ;  YES, DON'T RETURN
         RTS
NOTCR    LDA   INVFLG
         PHA
         LDA   #$FF
         STA   INVFLG     ;ECHO USER LINE
         LDA   IN,X       ;  NON INVERSE
         JSR   COUT
         PLA
         STA   INVFLG
         LDA   IN,X
         CMP   #$88       ;CHECK FOR EDIT KEYS
         BEQ   BCKSPC     ;  BS, CTRL-X
         CMP   #$98
         BEQ   CANCEL
         CPX   #$F8       ;MARGIN?
         BCC   NOTCR1
         JSR   BELL       ;  YES, SOUND BELL
NOTCR1   INX              ;ADVANCE INPUT INDEX
         BNE   NXTCHAR
CANCEL   LDA   #$DC       ;BACKSLASH AFTER CANCELLED LINE
         JSR   COUT
GETLNZ   JSR   CROUT      ;OUTPUT CR
GETLN    LDA   PROMPT
         JSR   COUT       ;OUTPUT PROMPT CHAR
         LDX   #$01       ;INIT INPUT INDEX
BCKSPC   TXA              ;  WILL BACKSPACE TO 0
         BEQ   GETLNZ
         DEX
NXTCHAR  JSR   RDCHAR
         CMP   #PICK      ;USE SCREEN CHAR
         BNE   CAPTST     ;  FOR CTRL-U
         LDA   (BASL),Y
CAPTST   CMP   #$E0
         BCC   ADDINP     ;CONVERT TO CAPS
         AND   #$DF
ADDINP   STA   IN,X       ;ADD TO INPUT BUF
         CMP   #$8D
         BNE   NOTCR
         JSR   CLREOL     ;CLR TO EOL IF CR
CROUT    LDA   #$8D
         BNE   COUT
PRA1     LDY   A1H        ;PRINT CR,A1 IN HEX
         LDX   A1L
PRYX2    JSR   CROUT
         JSR   PRNTYX
         LDY   #$00
         LDA   #$AD       ;PRINT '-'
         JMP   COUT
XAM8     LDA   A1L
         ORA   #$07       ;SET TO FINISH AT
         STA   A2L        ;  MOD 8=7
         LDA   A1H
         STA   A2H
MODSCHK  LDA   A1L
         AND   #$07
         BNE   DATAOUT
XAM      JSR   PRA1
DATAOUT  LDA   #$A0
         JSR   COUT       ;OUTPUT BLANK
         LDA   (A1L),Y
         JSR   PRBYTE     ;OUTPUT BYTE IN HEX
         JSR   NXTA1
         BCC   MODSCHK    ;CHECK IF TIME TO,
RTS4C    RTS              ;  PRINT ADDR
XAMPM    LSR              ;DETERMINE IF MON
         BCC   XAM        ;  MODE IS XAM
         LSR              ;  ADD, OR SUB
         LSR
         LDA   A2L
         BCC   ADD
         EOR   #$FF       ;SUB: FORM 2'S COMPLEMENT
ADD      ADC   A1L
         PHA
         LDA   #$BD
         JSR   COUT       ;PRINT '=', THEN RESULT
         PLA
PRBYTE   PHA              ;PRINT BYTE AS 2 HEX
         LSR              ;  DIGITS, DESTROYS A-REG
         LSR
         LSR
         LSR
         JSR   PRHEXZ
         PLA
PRHEX    AND   #$0F       ;PRINT HEX DIG IN A-REG
PRHEXZ   ORA   #$B0       ;  LSB'S
         CMP   #$BA
         BCC   COUT
         ADC   #$06
COUT     JMP   (CSWL)     ;VECTOR TO USER OUTPUT ROUTINE
COUT1    CMP   #$A0
         BCC   COUTZ      ;DON'T OUTPUT CTRL'S INVERSE
         AND   INVFLG     ;MASK WITH INVERSE FLAG
COUTZ    STY   YSAV1      ;SAV Y-REG
         PHA              ;SAV A-REG
         JSR   VIDOUT     ;OUTPUT A-REG AS ASCII
         PLA              ;RESTORE A-REG
         LDY   YSAV1      ;  AND Y-REG
         RTS              ;  THEN RETURN
BL1      DEC   YSAV
         BEQ   XAM8
BLANK    DEX              ;BLANK TO MON
         BNE   SETMDZ     ;AFTER BLANK
         CMP   #$BA       ;DATA STORE MODE?
         BNE   XAMPM      ;  NO, XAM, ADD, OR SUB
STOR     STA   MODE       ;KEEP IN STORE MODE
         LDA   A2L
         STA   (A3L),Y    ;STORE AS LOW BYTE AS (A3)
         INC   A3L
         BNE   RTS5       ;INCR A3, RETURN
         INC   A3H
RTS5     RTS
SETMODE  LDY   YSAV       ;SAVE CONVERTED ':', '+',
         LDA   IN-1,Y     ;  '-', '.' AS MODE.
SETMDZ   STA   MODE
         RTS
LT       LDX   #$01
LT2      LDA   A2L,X      ;COPY A2 (2 BYTES) TO
         STA   A4L,X      ;  A4 AND A5
         STA   A5L,X
         DEX
         BPL   LT2
         RTS
MOVE     LDA   (A1L),Y    ;MOVE (A1 TO A2) TO
         STA   (A4L),Y    ;  (A4)
         JSR   NXTA4
         BCC   MOVE
         RTS
VFY      LDA   (A1L),Y    ;VERIFY (A1 TO A2) WITH
         CMP   (A4L),Y    ;  (A4)
         BEQ   VFYOK
         JSR   PRA1
         LDA   (A1L),Y
         JSR   PRBYTE
         LDA   #$A0
         JSR   COUT
         LDA   #$A8
         JSR   COUT
         LDA   (A4L),Y
         JSR   PRBYTE
         LDA   #$A9
         JSR   COUT
VFYOK    JSR   NXTA4
         BCC   VFY
         RTS
LIST     JSR   A1PC       ;MOVE A1 (2 BYTES) TO
         LDA   #$14       ;  PC IF SPEC'D AND
LIST2    PHA              ;  DISEMBLE 20 INSTRS
         JSR   INSTDSP
         JSR   PCADJ      ;ADJUST PC EACH INSTR
         STA   PCL
         STY   PCH
         PLA
         SEC
         SBC   #$01       ;NEXT OF 20 INSTRS
         BNE   LIST2
         RTS
A1PC     TXA              ;IF USER SPEC'D ADR
         BEQ   A1PCRTS    ;  COPY FROM A1 TO PC
A1PCLP   LDA   A1L,X
         STA   PCL,X
         DEX
         BPL   A1PCLP
A1PCRTS  RTS
SETINV   LDY   #$3F       ;SET FOR INVERSE VID
         BNE   SETIFLG    ; VIA COUT1
SETNORM  LDY   #$FF       ;SET FOR NORMAL VID
SETIFLG  STY   INVFLG
         RTS
SETKBD   LDA   #$00       ;SIMULATE PORT #0 INPUT
INPORT   STA   A2L        ;  SPECIFIED (KEYIN ROUTINE)
INPRT    LDX   #KSWL
         LDY   #KEYIN
         BNE   IOPRT
SETVID   LDA   #$00       ;SIMULATE PORT #0 OUTPUT
OUTPORT  STA   A2L        ;  SPECIFIED (COUT1 ROUTINE)
OUTPRT   LDX   #CSWL
         LDY   #COUT1
IOPRT    LDA   A2L        ;SET RAM IN/OUT VECTORS
         AND   #$0F
         BEQ   IOPRT1
         ORA   #IOADR/256
         LDY   #$00
         BEQ   IOPRT2
IOPRT1   LDA   #COUT1/256
IOPRT2   STY   LOC0,X
         STA   LOC1,X
         RTS
         NOP
         NOP
XBASIC   JMP   BASIC      ;TO BASIC WITH SCRATCH
BASCONT  JMP   BASIC2     ;CONTINUE BASIC
GO       JSR   A1PC       ;ADR TO PC IF SPEC'D
         JSR   RESTORE    ;RESTORE META REGS
         JMP   (PCL)      ;GO TO USER SUBR
REGZ     JMP   REGDSP     ;TO REG DISPLAY
TRACE    DEC   YSAV
STEPZ    JSR   A1PC       ;ADR TO PC IF SPEC'D
         JMP   STEP       ;TAKE ONE STEP
USR      JMP   USRADR     ;TO USR SUBR AT USRADR
WRITE    LDA   #$40
         JSR   HEADR      ;WRITE 10-SEC HEADER
         LDY   #$27
WR1      LDX   #$00
         EOR   (A1L,X)
         PHA
         LDA   (A1L,X)
         JSR   WRBYTE
         JSR   NXTA1
         LDY   #$1D
         PLA
         BCC   WR1
         LDY   #$22
         JSR   WRBYTE
         BEQ   BELL
WRBYTE   LDX   #$10
WRBYT2   ASL
         JSR   WRBIT
         BNE   WRBYT2
         RTS
CRMON    JSR   BL1        ;HANDLE A CR AS BLANK
         PLA              ;  THEN POP STACK
         PLA              ;  AND RTN TO MON
         BNE   MONZ
READ     JSR   RD2BIT     ;FIND TAPEIN EDGE
         LDA   #$16
         JSR   HEADR      ;DELAY 3.5 SECONDS
         STA   CHKSUM     ;INIT CHKSUM=$FF
         JSR   RD2BIT     ;FIND TAPEIN EDGE
RD2      LDY   #$24       ;LOOK FOR SYNC BIT
         JSR   RDBIT      ;  (SHORT 0)
         BCS   RD2        ;  LOOP UNTIL FOUND
         JSR   RDBIT      ;SKIP SECOND SYNC H-CYCLE
         LDY   #$3B       ;INDEX FOR 0/1 TEST
RD3      JSR   RDBYTE     ;READ A BYTE
         STA   (A1L,X)    ;STORE AT (A1)
         EOR   CHKSUM
         STA   CHKSUM     ;UPDATE RUNNING CHKSUM
         JSR   NXTA1      ;INC A1, COMPARE TO A2
         LDY   #$35       ;COMPENSATE 0/1 INDEX
         BCC   RD3        ;LOOP UNTIL DONE
         JSR   RDBYTE     ;READ CHKSUM BYTE
         CMP   CHKSUM
         BEQ   BELL       ;GOOD, SOUND BELL AND RETURN
PRERR    LDA   #$C5
         JSR   COUT       ;PRINT "ERR", THEN BELL
         LDA   #$D2
         JSR   COUT
         JSR   COUT
BELL     LDA   #$87       ;OUTPUT BELL AND RETURN
         JMP   COUT
RESTORE  LDA   STATUS     ;RESTORE 6502 REG CONTENTS
         PHA              ;  USED BY DEBUG SOFTWARE
         LDA   ACC
RESTR1   LDX   XREG
         LDY   YREG
         PLP
         RTS
SAVE     STA   ACC        ;SAVE 6502 REG CONTENTS
SAV1     STX   XREG
         STY   YREG
         PHP
         PLA
         STA   STATUS
         TSX
         STX   SPNT
         CLD
         RTS
RESET    JSR   SETNORM    ;SET SCREEN MODE
         JSR   INIT       ;  AND INIT KBD/SCREEN
         JSR   SETVID     ;  AS I/O DEV'S
         JSR   SETKBD
MON      CLD              ;MUST SET HEX MODE!
         JSR   BELL
MONZ     LDA   #$AA       ;'*' PROMPT FOR MON
         STA   PROMPT
         JSR   GETLNZ     ;READ A LINE
         JSR   ZMODE      ;CLEAR MON MODE, SCAN IDX
NXTITM   JSR   GETNUM     ;GET ITEM, NON-HEX
         STY   YSAV       ;  CHAR IN A-REG
         LDY   #$17       ;  X-REG=0 IF NO HEX INPUT
CHRSRCH  DEY
         BMI   MON        ;NOT FOUND, GO TO MON
         CMP   CHRTBL,Y   ;FIND CMND CHAR IN TEL
         BNE   CHRSRCH
         JSR   TOSUB      ;FOUND, CALL CORRESPONDING
         LDY   YSAV       ;  SUBROUTINE
         JMP   NXTITM
DIG      LDX   #$03
         ASL
         ASL              ;GOT HEX DIG,
         ASL              ;  SHIFT INTO A2
         ASL
NXTBIT   ASL
         ROL   A2L
         ROL   A2H
         DEX              ;LEAVE X=$FF IF DIG
         BPL   NXTBIT
NXTBAS   LDA   MODE
         BNE   NXTBS2     ;IF MODE IS ZERO
         LDA   A2H,X      ; THEN COPY A2 TO
         STA   A1H,X      ; A1 AND A3
         STA   A3H,X
NXTBS2   INX
         BEQ   NXTBAS
         BNE   NXTCHR
GETNUM   LDX   #$00       ;CLEAR A2
         STX   A2L
         STX   A2H
NXTCHR   LDA   IN,Y       ;GET CHAR
         INY
         EOR   #$B0
         CMP   #$0A
         BCC   DIG        ;IF HEX DIG, THEN
         ADC   #$88
         CMP   #$FA
         BCS   DIG
         RTS
TOSUB    LDA   #GO/256    ;PUSH HIGH-ORDER
         PHA              ;  SUBR ADR ON STK
         LDA   SUBTBL,Y   ;PUSH LOW-ORDER
         PHA              ;  SUBR ADR ON STK
         LDA   MODE
ZMODE    LDY   #$00       ;CLR MODE, OLD MODE
         STY   MODE       ;  TO A-REG
         RTS              ; GO TO SUBR VIA RTS
CHRTBL   DFB   $BC        ;F("CTRL-C")
         DFB   $B2        ;F("CTRL-Y")
         DFB   $BE        ;F("CTRL-E")
         DFB   $ED        ;F("T")
         DFB   $EF        ;F("V")
         DFB   $C4        ;F("CTRL-K")
         DFB   $EC        ;F("S")
         DFB   $A9        ;F("CTRL-P")
         DFB   $BB        ;F("CTRL-B")
         DFB   $A6        ;F("-")
         DFB   $A4        ;F("+")
         DFB   $06        ;F("M") (F=EX-OR $B0+$89)
         DFB   $95        ;F("<")
         DFB   $07        ;F("N")
         DFB   $02        ;F("I")
         DFB   $05        ;F("L")
         DFB   $F0        ;F("W")
         DFB   $00        ;F("G")
         DFB   $EB        ;F("R")
         DFB   $93        ;F(":")
         DFB   $A7        ;F(".")
         DFB   $C6        ;F("CR")
         DFB   $99        ;F(BLANK)
SUBTBL   DFB   BASCONT-1
         DFB   USR-1
         DFB   REGZ-1
         DFB   TRACE-1
         DFB   VFY-1
         DFB   INPRT-1
         DFB   STEPZ-1
         DFB   OUTPRT-1
         DFB   XBASIC-1
         DFB   SETMODE-1
         DFB   SETMODE-1
         DFB   MOVE-1
         DFB   LT-1
         DFB   SETNORM-1
         DFB   SETINV-1
         DFB   LIST-1
         DFB   WRITE-1
         DFB   GO-1
         DFB   READ-1
         DFB   SETMODE-1
         DFB   SETMODE-1
         DFB   CRMON-1
         DFB   BLANK-1
         DFB   NMI        ;NMI VECTOR
         DFB   NMI/256
         DFB   RESET      ;RESET VECTOR
         DFB   RESET/256
         DFB   IRQ        ;IRQ VECTOR
         DFB   IRQ/256
XQTNZ    EQU   $3C