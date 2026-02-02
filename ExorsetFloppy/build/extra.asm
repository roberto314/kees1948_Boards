
;CURDRV  EQU     $0000
;STRSCTH EQU     $0001
;STRSCTL EQU     $0002
;NUMSCTH EQU     $0003
;NUMSCTL EQU     $0004
;LSCTLN  EQU     $0005
CURADRH EQU     $0006
CURADRL EQU     $0007
FDSTAT  EQU     $0008
;CWRDCNT EQU     $0009
;SofTRK  EQU     $000A
;SECTCNT EQU     $000B
;STATSAV EQU     $000D
FUNCSAV EQU     $000E
;NMIVECSAV EQU     $000F
;M0012   EQU     $0012 ; $11 and $12 are current Track of Drive 0,1
;TRACKSAV EQU    $0013
ADDRMRK EQU     $0014
;DWRDCNT EQU     $0015
;STACKSAV EQU     $0016
;DRDCNT  EQU     $0018
CLKFREQ EQU     $0019
LDADDR  EQU     $0020
EXADDR     EQU     $0022
ONECON     EQU     $0024

; The PIA has A0 and A1 switched!
; 00 with CRA2 Set is Peripheral Register A 
; 00 with CRA2 cleared is Data Direction Register A 
; 01 with CRB2 Set is Peripheral Register B 
; 01 with CRB2 cleared is Data Direction Register B 
; 02 is Control Register A
; 03 is Control Register B
PIAREGA EQU     $EC00
PIAREGB EQU     $EC01
PIACTRL EQU     $EC02
SSDA_0  EQU     $EC04
SSDA_1  EQU     $EC05

LPTPIA0 EQU     $EC10
LPTPIA1 EQU     $EC11
LPTPIA2 EQU     $EC12
LPTPIA3 EQU     $EC13

;DSLOAD  EQU  $E800 ; BOOTSTRAP THE OPERATING SYSTEM
FDINIT  EQU  $E822 ; INITIALIZE THE DISK'S PIA AND SSD
;CHKERR  EQU  $E853 ; CHECK AND PRINT ERROR FROM FDSTAT
;PRNTER  EQU  $E85A ; PRINT ERROR FROM FDSTAT
;READSC  EQU  $E869 ; READ SECTOR(S)
;READPS  EQU  $E86D ; READ PARTIAL SECTOR
;RDCRC   EQU  $E86F ; READ AND CHECK FOR CRC
;RWTEST  EQU  $E872 ; WRITE/READ TEST
RESTOR  EQU  $E875 ; MOVE HEAD TO TRACK 0
;SEEK    EQU  $E878 ; POSITION HEAD TO TRACK OF "STRSCT
;WRTEST  EQU  $E87B ; WRITE TEST
;WRDDAM  EQU  $E87E ; WRITE DELETED DATA MARK
;WRVERF  EQU  $E881 ; WRITE AND VERIFY CRC
;WRITSC  EQU  $E884 ; WRITE SECTDR(S)
SETERR   EQU $E954
NXTSEC   EQU $E9C8
PRNTE2  EQU  $E855 ; Jump Back into old EPROM

; ################################################################
; Jump Table
; ################################################################
		ORG $ED00
ED00	jmp WRITINI
ED03    jmp TOP
ED06    jmp LPINIT
ED09    jmp LIST
ED0C    jmp LDATA
ED0F    jmp LDATA1
ED12    jmp EXIT
ED15    jmp EXIT
ED18    jmp EXIT
EXIT    rts

;------------------------------------------------
; Disk Mini Diagnostic Routine (CLRTOP is in original ROM)
;------------------------------------------------
TOP             JSR     FDINIT                   ; EB98: BD E8 22 ; Diagnostic without clear
                JSR     RESTOR                   ; EB9B: BD E8 75 ; 
ZEB9E           LDX     LDADDR                   ; EB9E: DE 20    ; 
                STX     CURADRH                  ; EBA0: DF 06    ; 
                LDX     EXADDR                   ; EBA2: DE 22    ; must contain addres of function (READSC,...)
                JSR     ,X                       ; EBA4: AD 00    ; 
                LDAA    ONECON                   ; EBA6: 96 24    ; 
                BNE     ZEBB9                    ; EBA8: 26 0F    ; 
                STAA    CURADRL                  ; EBAA: 97 07    ; 
                ASL     FDSTAT                   ; EBAC: 78 00 08 ; 
                LDX     CURADRL                  ; EBAF: DE 07    ; 
                INC     $01,X                    ; EBB1: 6C 01    ; 
                BNE     ZEB9E                    ; EBB3: 26 E9    ; 
                INC     ,X                       ; EBB5: 6C 00    ; 
                BNE     ZEB9E                    ; EBB7: 26 E5    ; 
ZEBB9           JMP     PRNTE2                  ; EBB9: 7E E8 55 ; Print Error Code

;------------------------------------------------
; Line Printer Routines
;------------------------------------------------
LPINIT          LDX     #$FF2E                   ; EBC0: CE FF 2E ; Line Printer Init
                STX     LPTPIA0                  ; EBC3: FF EC 10 ; <------------------------------------ no RAM here!
                LDAA    #$3C                     ; EBC6: 86 3C    ; 
                STAA    LPTPIA3                  ; EBC8: B7 EC 13 ; <------------------------------------ no RAM here!
                RTS                              ; EBCB: 39       ; 
;------------------------------------------------
LIST            STAA    LPTPIA0                  ; EBCC: B7 EC 10 ; Print contents of A
                LDAA    LPTPIA0                  ; EBCF: B6 EC 10 ; <------------------------------------ no RAM here!
ZEBD2           PSHB                             ; EBD2: 37       ; 
                LDAB    LPTPIA2                  ; EBD3: F6 EC 12 ; <------------------------------------ no RAM here!
                ANDB    #$03                     ; EBD6: C4 03    ; 
                DECB                             ; EBD8: 5A       ; 
                PULB                             ; EBD9: 33       ; 
                BNE     ZEBE2                    ; EBDA: 26 06    ; 
                TST     LPTPIA1                  ; EBDC: 7D EC 11 ; <------------------------------------ no RAM here!
                BPL     ZEBD2                    ; EBDF: 2A F1    ; 
                RTS                              ; EBE1: 39       ; 
;------------------------------------------------
ZEBE2           SEC                              ; EBE2: 0D       ; 
                RTS                              ; EBE3: 39       ; 
;------------------------------------------------
LDATA           LDAA    #$0D                     ; EBE4: 86 0D    ; Send String to Printer
ZEBE6           BSR     LIST                     ; EBE6: 8D E4    ; 
                BCS     ZEBE6                    ; EBE8: 25 FC    ; 
                LDAA    #$0A                     ; EBEA: 86 0A    ; 
                DEX                              ; EBEC: 09       ; 
ZEBED           BSR     LIST                     ; EBED: 8D DD    ; 
                BCS     ZEBED                    ; EBEF: 25 FC    ; 
                INX                              ; EBF1: 08       ; 
LDATA1          LDAA    ,X                       ; EBF2: A6 00    ; 
                CMPA    #$04                     ; EBF4: 81 04    ; 
                BNE     ZEBED                    ; EBF6: 26 F5    ; 
                RTS                              ; EBF8: 39       ; 
;------------------------------------------------
SERR2           LDAB    #$32                     ; EA95: C6 32     ; Set Error '2' (DISK WRITE PROTECTED)
                JMP     SETERR     ; -->         ; EA9A: 7E E9 40  ; 
;------------------------------------------------
WRITINI         LDX     #$C0DA                   ; EADD: CE C0 DA  ; 
                STX     SSDA_0                   ; EAE0: FF EC 04  ; 
                LDX     #$C1AA                   ; EAE3: CE C1 AA  ; 
                STX     SSDA_0                   ; EAE6: FF EC 04  ; 
                LDX     #$C270                   ; EAE9: CE C2 70  ; 
                STX     SSDA_0                   ; EAEC: FF EC 04  ; 
                INC     PIAREGB  ;write PIAREGB  ; EAEF: 7C EC 01  ; Reset inactive (PB0 high)
                LDAA    #$82                     ; EAF2: 86 82     ; Bit 1,7
                STAA    SSDA_0                   ; EAF4: B7 EC 04  ; RXRs, Sel. CR3
                LDAA    PIAREGB  ; changed   ;+4  but DEX is gone (-4) so it is a wash     
                BITA    #$10     ; changed   ;+2                   ; Check Writeprot?
                BNE     SERR2        ;changed ;+4  ; EB00: 26 93     ; If high set Error (otherwise it conflicts with format.sy)
                ANDA    #$60     ; changed   ;+2                   ; Clear all but Bit 5,6 (DS2, DS3)
                TAB                          ;+2
                ORAA    #$02     ; changed   ;+2                   ; Set Bit 1 (WG high)
                STAA    PIAREGB  ;write PIAREGB ;+5 ; EAF8: B7 EC 01  ; WG off, PB7 is an Input 
                                ; +6 cycles slower
                LDAA    CLKFREQ              ;+3 ; EB02: 96 19     ; Waitloop A is 3 on 1MHz
                SUBA    #$03                 ;+2 ; EB04: 80 03     ; |
                ASLA                         ;+2 ; EB06: 48        ; |
IEB07           DECA                         ;+2 ; EB07: 4A        ; |
                BPL     IEB07                ;+4 ; EB08: 2A FD     ; | A is FF
                STAB    PIAREGB  ;changed   ;+5                  ; Bit 5,6 (DS2, DS3 unaffected)
                LDB     FUNCSAV             ;+3
                RORB                        ;+2  ; EB0D: 56        ; ROR FUNCSAV (check Bit 0)
                BCC     IEB11               ;+4  ; EB0E: 24 01     ; useless or timing? 
;                BITA    #$56                     ; EB10: 85 56     ; A is now $56, doesn't effect carry
                FCB     $85                
IEB11           RORB                             ; EB11: 56        ; ROR FUNCSAV (check Bit 1) set carry
                                 ; 8 cycles slower
                LDX     #$0005                   ; EB12: CE 00 05  ; 
                JSR     WAIT3                    ; EB15: BD E9 53  ; destroys B
                LDAB    #$40                     ; EB18: C6 40     ; $40 words
                LDAA    ADDRMRK                  ; EB1A: 96 14     ; 
                LDX     #$83F5                   ; EB1C: CE 83 F5  ; 
                STX     SSDA_0                   ; EB1F: FF EC 04  ; 
                LDX     CURADRH                  ; EB22: DE 06     ; 
                STAA    SSDA_1                   ; EB24: B7 EC 05  ; 
                JMP     WRITSEC              ;3  ; EB27: 7E EB 31  ; 

IEB2A           LDAA    #$40                     ; EB2A: 86 40     ; 
IEB2C           BITA    SSDA_0                   ; EB2C: B5 EC 04  ; Transmit Datareg. empty?
                BEQ     IEB2C                    ; EB2F: 27 FB     ; If no, wait
WRITSEC         CLRA           ;changed     ;+2                       
                LDAA    ,X                       ; EB35: A6 00     ; Load Data from RAM
                STAA    SSDA_1                   ; EB37: B7 EC 05  ; Write to disk
                LDAA    $01,X                    ; EB3A: A6 01     ; Load next Byte
                STAA    SSDA_1                   ; EB3C: B7 EC 05  ; Write to disk
                BCS     IEB43                    ; EB3F: 25 02     ; if carry is set, don't INX (RWTEST or WRTEST or WRDAM)
                INX                              ; EB41: 08        ; if not, increase pointer
                INX                              ; EB42: 08        ; |
IEB43           DECB                             ; EB43: 5A        ; decrease counter
                BNE     IEB2A                    ; EB44: 26 E4     ; check if underflow, if not, continue from top
                STX     CURADRH                  ; EB46: DF 06     ; done, store current address  ####### THIS IS WRONG ##########
                LDX     #PIAREGA                 ; EB48: CE EC 00  ;  #### according to the M68SFDC3 Manual it should NOT change CURADR
                LDAA    #$40                     ; EB4B: 86 40     ;  
IEB4D           BITA    $04,X                    ; EB4D: A5 04     ; Check SSDA Status Reg.
                BEQ     IEB4D                    ; EB4F: 27 FC     ; Wait for Data
                STAB    $05,X                    ; EB51: E7 05     ; Store B to Data Register
                LDAB    PIAREGB ;read PIAREGB ;+4  ; EB59: E7 01   ;
IEB53           BITA    $04,X                    ; EB53: A5 04     ; Check SSDA Status Reg.
                BEQ     IEB53                    ; EB55: 27 FC     ; Wait for Data
                ORAB    #$08                ;+2
                STAB    PIAREGB             ;+5
                LDAB    #$08                     ; EB57: C6 08     ; |
                STAB    $05,X                    ; EB5B: E7 05     ; Store $8 to SSDA Data Reg.
IEB5D           BITA    $04,X                    ; EB5D: A5 04     ; Check SSDA Status Reg.
                BEQ     IEB5D                    ; EB5F: 27 FC     ; Wait for Data
                LDAB    #$FF                     ; EB61: C6 FF     ; |
                STAB    $05,X                    ; EB63: E7 05     ; |
                STAB    $05,X                    ; EB65: E7 05     ; Store $FF to SSDA Data Reg.
                LDAB    PIAREGB ;read PIAREGB ;+4 
IEB67           BITA    $04,X                    ; EB67: A5 04     ; Check SSDA Status Reg.
                BEQ     IEB67                    ; EB69: 27 FC     ; Wait for Data
                ANDB    #$60     ; changed ;+2
                STAB    PIAREGB            ;+5
                INC     PIAREGB            ;+6
                INC     PIAREGB            ;+6
                JMP     NXTSEC    ; -->          ; EB71: 7E E9 D3  ; next sector  




;WAIT4	ldx 	#$800
WAIT3    LDAB    CLKFREQ     ; is 3 for 1MHz
WAIT1    DECB                ; 
         BNE     WAIT1       ; 
         DEX                 ; 
         BNE     WAIT3       ; 
         RTS                 ; 

	END