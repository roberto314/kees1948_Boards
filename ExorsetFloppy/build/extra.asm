
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
;FUNCSAV EQU     $000E
;NMIVECSAV EQU     $000F
;M0012   EQU     $0012 ; $11 and $12 are current Track of Drive 0,1
;TRACKSAV EQU    $0013
;ADDRMRK EQU     $0014
;DWRDCNT EQU     $0015
;STACKSAV EQU     $0016
;DRDCNT  EQU     $0018
;CLKFREQ EQU     $0019
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
;PIAREGA EQU     $EC00
;PIAREGB EQU     $EC01
;PIACTRL EQU     $EC02
;SSDA_0  EQU     $EC04
;SSDA_1  EQU     $EC05

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

PRNTE2  EQU  $E855 ; Jump Back into old EPROM

; ################################################################
; Jump Table
; ################################################################
		ORG $ED00
ED00	jmp EXIT
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







;WAIT4	ldx 	#$800
;WAIT3    LDAB    CLKFREQ     ; is 3 for 1MHz
;WAIT1    DECB                ; 
;         BNE     WAIT1       ; 
;         DEX                 ; 
;         BNE     WAIT3       ; 
;         RTS                 ; 

	END