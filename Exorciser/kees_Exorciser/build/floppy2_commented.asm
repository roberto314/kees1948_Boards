; Ready to assemble with asl.
; Modified Version for use with extra ROM @ ED00
; Modifications are:
; - Runs on kees1948 CPUXXCMI Board with Exordisk Replica Board
; - Since there is literally no space left and the position of some labels is fixed
;   we put in JMPs to extra ROM @ ED00 and then JMP back to the appropriate place.
;****************************************************
; Used Labels
;****************************************************

CURDRV  EQU     $0000
STRSCTH EQU     $0001
STRSCTL EQU     $0002
NUMSCTH EQU     $0003
NUMSCTL EQU     $0004
LSCTLN  EQU     $0005
CURADRH EQU     $0006
CURADRL EQU     $0007
FDSTAT  EQU     $0008
CWRDCNT EQU     $0009
SofTRK  EQU     $000A
SECTCNT EQU     $000B
STATSAV EQU     $000D
FUNCSAV EQU     $000E
NMIVECSAV EQU     $000F
M0012   EQU     $0012 ; was: $11 and $12 are current Track of Drive 0,1, Reused always zero
TRACKSAV EQU    $0013
ADDRMRK EQU     $0014
DWRDCNT EQU     $0015
STACKSAV EQU     $0016
DRDCNT  EQU     $0018
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

XOUTCH  EQU     $F018
REENT2  EQU     $F564
PROM_0  EQU     $FCFC
XSTAKs  EQU     $FF8A
NMIsVC  EQU     $FFFC

; Printer
MEC10   EQU     $EC10
MEC11   EQU     $EC11
MEC12   EQU     $EC12
MEC13   EQU     $EC13

;****************************************************
; Program's Code Areas
;****************************************************


                ORG     $E800

OSLOAD          LDS     #XSTAKs                  ; E800: 8E FF 8A  ; Set Stackpointer     
                CLRA                             ; E803: 4F        ; |    
                STAA    CURDRV                   ; E804: 97 00     ; Set Drive to zero   
                BSR     FDINIT2                  ; E806: 8D 48     ; Init PIA and SSDA
                BSR     RESTOR                   ; E808: 8D 6B     ; 
                BSR     CHKERR                   ; E80A: 8D 47     ; 
                LDX     #$0017                   ; E80C: CE 00 17  ; Start at Sector $17 (Bootblock) (0xB80)
                STX     STRSCTH                  ; E80F: DF 01     ; |
                LDX     #$0002                   ; E811: CE 00 02  ; Get 2 sectors
                STX     NUMSCTH                  ; E814: DF 03     ; |
                LDX     #LDADDR                  ; E816: CE 00 20  ; Load @ 0x20
                STX     CURADRH                  ; E819: DF 06     ; |
                BSR     READSC                   ; E81B: 8D 4C     ; Read the Sector
                BSR     CHKERR                   ; E81D: 8D 34     ; Check for Error
                JMP     LDADDR                   ; E81F: 7E 00 20  ; Execute loaded code....
;------------------------------------------------
FDINIT          JMP     FDINITEXT
                ;LDX     #$0000                   ; E822: CE 00 00  ; |
FDINTBACK       STX     PIACTRL                  ; E825: FF EC 02  ; clr both control Reg.
                STX     PIAREGA                  ; E828: FF EC 00  ; Set A and B to Input
                LDX     #$D0DA                   ; E82B: CE D0 DA  ; 
                STX     SSDA_0                   ; E82E: FF EC 04  ; 
                LDX     #$0404                   ; E831: CE 04 04  ; |
                STX     PIACTRL                  ; E834: FF EC 02  ; Set A and B to Output Reg.
                LDX     #$1B62     ;changed      ; E837: CE 1B 02  ; |
                STX     PIAREGA                  ; E83A: FF EC 00  ; Set DS0, DS1, DIRQ, HLD, WG, DS2, DS3(SIDE) high
                LDX     #$0000                   ; E83D: CE 00 00  ; |
                STX     PIACTRL                  ; E840: FF EC 02  ; Set both to Data direction Reg.
                LDX     #$1F6F       ;changed    ; E843: CE 1F 0F  ; |
                STX     PIAREGA                  ; E846: FF EC 00  ; PA0-PA4, PB0-PB3, PB5..6 Output
                LDX     #$3C3E                   ; E849: CE 3C 3E  ; |
                STX     PIACTRL                  ; E84C: FF EC 02  ; Select both Output Reg., Set CA2 (STEP), Set CB2 (NMI Timer), IRQB (IRQ) Set by LH Transition of CB1 (IDX)
ZE84F           RTS                              ; E84F: 39        ; Both Output Regsters Selected
;------------------------------------------------
FDINIT2         JSR     FDINIT3                  ; E850: BD EB A6  ; 
;------------------------------------------------
CHKERR          BCC     ZE84F                    ; E853: 24 FA     ; no carry - no error
PRNTE2          BSR     PRNTER                   ; E855: 8D 03     ; Print for ex.: 'E2 ' for Error 0x32
                JMP     REENT2                   ; E857: 7E F5 64  ; 
;------------------------------------------------
PRNTER          LDAA    #$45                     ; E85A: 86 45      ; Load 'E'
                BSR     ZE866                    ; E85C: 8D 08      ; 
                LDAA    FDSTAT                   ; E85E: 96 08      ; 
                BSR     ZE866                    ; E860: 8D 04      ; 
                LDAA    #$20                     ; E862: 86 20      ; 
                BSR     ZE866                    ; E864: 8D 00      ; 
ZE866           JMP     XOUTCH                   ; E866: 7E F0 18   ; Print Accu A to Console
;------------------------------------------------
;READSC          LDAB    #$80                     ; E869: C6 80      ; 
;                STAB    LSCTLN                   ; E86B: D7 05      ; 
;READPS          CLRB                             ; E86D: 5F         ; 
;                CPX     #MC640                   ; E86E: 8C C6 40   ; These CPX are just a method to deactivate the code after the LDAB
;                CPX     #MC6C2                   ; E871: 8C C6 C2   ; These CPX are just a method to deactivate the code after the LDAB
;                CPX     #MC608                   ; E874: 8C C6 08   ; These CPX are just a method to deactivate the code after the LDAB
;                CPX     #MC610                   ; E877: 8C C6 10   ; These CPX are just a method to deactivate the code after the LDAB
;                CPX     #MC682                   ; E87A: 8C C6 82   ; These CPX are just a method to deactivate the code after the LDAB
;                CPX     #MC681                   ; E87D: 8C C6 81   ; These CPX are just a method to deactivate the code after the LDAB
;                CPX     #MC6C0                   ; E880: 8C C6 C0   ; These CPX are just a method to deactivate the code after the LDAB
;                CPX     #MC680                   ; E883: 8C C6 80   ; These CPX are just a method to deactivate the code after the LDAB
;                CPX     #MC604                   ; E886: 8C C6 04   ; These CPX are just a method to deactivate the code after the LDAB
;*************************************************
; Meaning of the Bits in FUNCSAV:
;
; 7  6  5  4  3  2  1  0 
; |  |  |  |  |  |  |  |-- 0: Write Deleted Address Mark (with bit 7 set)
; |  |  |  |  |  |  |----- 1: test data from disk (with bit 7 set)
; |  |  |  |  |  |-------- 2: measure the CPU Frequency via drive speed (these Bits are set alone)
; |  |  |  |  |----------- 3: do a RESTOR (these Bits are set alone)
; |  |  |  |-------------- 4: do a SEEK (these Bits are set alone)
; |  |  |----------------- 5: not used
; |  |-------------------- 6: do CRC Verify or read (can be set alone)
; |----------------------- 7: write function (can be set alone)
;
;*************************************************
READSC          LDAB    #$80                     ; E869: C6 80    ; NO Bit set    ; causes the number of sectors contained in NUMSCT beginning with STRSCT from CURDRV to be read into memory starting at the address contained in CURADR
                STAB    LSCTLN                   ; E86B: D7 05 
READPS          CLRB                             ; E86D: 5F       ; NO Bit set    ; similar to READSC with the exception that the last sector is only partially read according to the contents of LSCTLN
                FCB     $8C
RDCRC           LDAB    #$40                     ; E86F: C6 40    ; Bit 6 set     ; causes the number of sectors contained in NUMSCT beginning with STRSCT from CURDRV to be read to check their CRCs
                FCB     $8C
RWTEST          LDAB    #$C2                     ; E872: C6 C2    ; Bit 7,6,1 set ; same as WRTEST + readback and check CRC
                FCB     $8C
RESTOR          LDAB    #$08                     ; E875: C6 08    ; Bit 3 set     ; causes the read/write head on CURDRV to be positioned to cylinder zero. The only parameter required is CURDRV.
                FCB     $8C
SEEK            LDAB    #$10                     ; E878: C6 10    ; Bit 4 set     ; causes the read/write head of CURDRV to be positioned to the cylinder containing STRSCT
                FCB     $8C
WRTEST          LDAB    #$82                     ; E87B: C6 82    ; Bit 7,1 set   ; causes the two bytes of data pointed to by the address in CURADR and the address+ 1 to be written to alternating bytes, respectively
                FCB     $8C
WRDDAM          LDAB    #$81                     ; E87E: C6 81    ; Bit 7,0 set   ; causes a deleted data address mark to be written
                FCB     $8C
WRVERF          LDAB    #$C0                     ; E881: C6 C0    ; Bit 7,6 set   ; same as WRITSC + CRC Verify
                FCB     $8C
WRITSC          LDAB    #$80                     ; E884: C6 80    ; Bit 7 set     ; causes NUMSCT sectors beginning with STRSCT of CURDRV to be written from memory beginning at CURADR
                FCB     $8C
CLOCK           LDAB    #$04                     ; E887: C6 04    ; Bit 2 set     ; calculate clock frequency of CPU

                TPA                              ; E889: 07         ; Transfer Status Reg to A
                SEI                              ; E88A: 0F        ; 
                STAA    STATSAV                  ; E88B: 97 0D     ; Save Status Reg.
                STAB    FUNCSAV                  ; E88D: D7 0E     ; Save Function
                LDAA    #$30                     ; E88F: 86 30     ; 
                STAA    FDSTAT                   ; E891: 97 08     ; Clear Errors ('0')
                STS     STACKSAV                 ; E893: 9F 16     ; Set Stackpointer
                LDX     NMIsVC                   ; E895: FE FF FC  ; |
                STX     NMIVECSAV                ; E898: DF 0F     ; Save old NMIISR
                LDX     #NMIISR                  ; E89A: CE E9 39  ; |
                STX     NMIsVC                   ; E89D: FF FF FC  ; New NMIISR
;------------------------------------------------
; from here new code
;------------------------------------------------
                 LDAA    CURDRV
                 CMPA    #$04                     ; is CURDRV < 4
                 BCC     SERR3                    ; if CURDRV > 3 Set Error 3
                 ANDA    #$03                     ; | mask dries > 3
                 STAA    $FE05                    ; in $FE04,5 is a pointer to the variable for the current track / drive  
                 STAA    CURDRV 
                 LDAA    PIAREGA                  ; Get PA0..7   
                 ANDA    #$EE                     ; Mask DS0 and HLD (DEBUG Reset HLD while in floppy firmware and set it on exit)
                 LDAB    CURDRV                   ; Whats is the current drive
                 ANDB    #$01                     ; Mask Bit 0 for Drive 0 or 2
                 BEQ     DRVZRO                   ; Is it zero?
                 ORA     #$01                     ; If not Set PA0
                 ;LDAA    #$03                     ; If drive is NOT zero set PA0,1 high
DRVZRO           STAA    PIAREGA                  ; Write TG43, DIRQ, HLD low, Set DS0, DS1 according to CURDRV (DS1 is NOT used)
                 LDX     $FE04                    ; Get pointer
                 LDAA    ,X                       ; Get the Track of Drive
                 STAA    TRACKSAV                 ; Store it
                 
                 LDAA    PIAREGB                  ;
                 ORAA    #$20                     ; Set WG and PB0(RESET) and DS2
                 ;LDAA    #$62                     ; Set DS3(side) and DS2 high
                 LDAB    CURDRV                   ;
                 ANDB    #$02                     ; Check if Drive 2 or 3 is set
                 BEQ     STORE                    ; If Drive 0 or 1 is active, Leave DS2(PB5) High
                 ANDA    #$DF                     ; Set DS2 low
STORE            STAA    PIAREGB

                LDAA    #$40                     ; E8B4: 86 40     ; PA6 (RDY)
CHECKAGAIN      BITA    PIAREGA                  ; E8B6: B5 EC 00  ; Check Drive Ready
                BNE     CHECKAGAIN  ; changed
                LDAB    FUNCSAV     ; important!
                BRA     DRVRDY
;------------------------------------------------
; from here original code
;------------------------------------------------
;                BEQ     DRVRDY                   ; E8B9: 27 07     ; 
SERR3           LDAB    #$33                     ; E8BB: C6 33     ; Error '3' (DISK NOT READY)
;                CPX     #MC636                   ; E8BD: 8C C6 36  ; 
                FCB     $8C
SERR6           LDAB    #$36                                       ; Set Error '6' (INVALID DISK ADDRESS)
                JMP     SETERR                   ; E8C0: 20 7E     ; 
;                BRA     SETERR                   ; E8C0: 20 7E     ; 
;------------------------------------------------
DRVRDY          BITB    #$08                     ; E8C2: C5 08     ; Get Bit3 from FUNCSAV (RESTOR)
                BEQ     RESTORN                  ; E8C4: 27 05     ; Not RESTOR
                CLRB                             ; E8C6: 5F        ; 
                LDAA    #$03                     ; E8C7: 86 03     ; Go to Track 3
                BRA     RESTORY                  ; E8C9: 20 3F          
;------------------------------------------------
RESTORN         BITB    #$04                     ; E8CB: C5 04     ; Get Bit2 from FUNCSAV (CLOCK)
                BEQ     CLOCKN                   ; E8CD: 27 03     ; No Clock
                JMP     CLKDMD                   ; E8CF: 7E EB 85  ; Calculate CPU Freq., goes to CLKDMD, EXITHNDLR and RST
;------------------------------------------------
CLOCKN          LDAB    STRSCTL                  ; E8D2: D6 02     ; normal code flow (no CLOCK, no RESTOR)
                LDAA    STRSCTH                  ; E8D4: 96 01     ; Get Startsector
                ADDB    NUMSCTL                  ; E8D6: DB 04     ; |
                ADCA    NUMSCTH                  ; E8D8: 99 03     ; Add Number of sectors
                BCS     SERR6                    ; E8DA: 25 E2     ; Set Error '6': INVALID DISK ADDRESS
               ; CMPB    #$D3                     ; E8DC: C1 D3     ; |
               ; SBCA    #$07                     ; E8DE: 82 07     ; 0x7D3 = 2003 (sector too big)
                CMPB    #$A4     ;changed        ; E8DC: C1 D3     ; |
                SBCA    #$0F     ;changed        ; E8DE: 82 07     ; 0xFA4 = 4004 (sector too big)
                BCC     SERR6                    ; E8E0: 24 DC     ; Set Error '6': INVALID DISK ADDRESS
;------------------------------
; above: check for number of sector + startsector < 4004
;------------------------------
                LDAA    #$FF                     ; E8E2: 86 FF     ; 
                STAA    SofTRK                   ; E8E4: 97 0A     ; 
                LDAA    STRSCTH                  ; E8E6: 96 01     ; Startsector is usually at $17 at boot
                LDAB    STRSCTL                  ; E8E8: D6 02     ; |
IE8EA           INC     SofTRK                   ; E8EA: 7C 00 0A  ; is now at 0
                SUBB    #$D0                     ; E8ED: C0 D0     ; 0xd0 = 208 (8 tracks exactly)
                SBCA    #$00                     ; E8EF: 82 00     ; 
                BCC     IE8EA                    ; E8F1: 24 F7     ; $17-$D0 is negative, carry is set
                ADDB    #$D0                     ; E8F3: CB D0     ; add $d0 again
                LDAA    SofTRK                   ; E8F5: 96 0A     ; is 0
                ASLA                             ; E8F7: 48        ; 
                ASLA                             ; E8F8: 48        ; 
                ASLA                             ; E8F9: 48        ; Sector * 8
                DECA                             ; E8FA: 4A        ; 
IE8FB           INCA                             ; E8FB: 4C        ; A is still 0
                SUBB    #$1A                     ; E8FC: C0 1A     ; $1a = 26, is STRTSCT > 26
                BCC     IE8FB                    ; E8FE: 24 FB     ; 
                ADDB    #$1A                     ; E900: CB 1A     ; $1a = 26, if no add it again
                STAB    SofTRK                   ; E902: D7 0A     ; store it in Sector of Track
                LDX     NUMSCTH                  ; E904: DE 03     ; 
                STX     SECTCNT                  ; E906: DF 0B     ; 
                LDAB    TRACKSAV                 ; E908: D6 13     ; 
RESTORY         STAA    TRACKSAV                 ; E90A: 97 13     ; contains track (3 if RESTOR)
                SBA                              ; E90C: 10        ; B cont. 0 if RESTOR
                LDAB    PIAREGA                  ; E90D: F6 EC 00  ; 
                ORAB    #$08                     ; E910: CA 08     ; Set Bit 3 (DIRQ) | Check direction to STEP
                BCC     IE917                    ; E912: 24 03     ;                      | Check direction to STEP
                ANDB    #$F7                     ; E914: C4 F7     ; Clear Bit 3 (DIRQ) | Check direction to STEP
                NEGA                             ; E916: 40        ; 
IE917           ;ANDB    #$EF    ; changed       ; E917: C4 EF     ; Isolate PA4 (HLD)
                CMPA    #$04                     ; E919: 81 04     ; Compare A with Track 4
                BLS     IE91F                    ; E91B: 23 02     ; 
                ;ORAB    #$10     ; changed      ; E91D: CA 10     ; Set Bit 4 (HLD)
IE91F           STAB    PIAREGA                  ; E91F: F7 EC 00  ; Write to Port
                DECA                             ; E922: 4A        ; 
                BMI     ZE96A     ; -->          ; E923: 2B 45     ; 
                BSR     STEP                     ; E925: 8D 1F     ; 
                LDAB    PIAREGA                  ; E927: F6 EC 00  ; 
                BPL     IE917                    ; E92A: 2A EB     ; Bit 7 clear? (TRK0)
                ;BMI     IE917                    ; E92A: 2A EB     ; Bit 7 set? (TRK0)
                TSTA                             ; E92C: 4D        ; 
                BEQ     ZE96A     ; -->          ; E92D: 27 3B     ; 
                LDAA    FUNCSAV                  ; E92F: 96 0E     ; 
                BITA    #$08                     ; E931: 85 08     ; Get Bit3 from FUNCSAV (RESTOR)
                BEQ     SERR7                    ; E933: 27 09     ; Function is NOT RESTOR, Set Error
                BSR     WAIT2                    ; E935: 8D 25     ; Wait
                BRA     EXITHNDLR ; -->          ; E937: 20 58     ; 
;------------------------------------------------
;
;------------------------------------------------
SERR7           JMP     xSERR7
;NMIISR          LDS     STACKSAV                 ; E939: 9E 16     ; 
;                LDAB    #$35                     ; E93B: C6 35     ; Set Error '5' (TIMEOUT)
;;                CPX     #MC637                   ; E93D: 8C C6 37  ; 
;                FCB     $8C
;SERR7           LDAB    #$37                                       ; Set Error '7' (SEEK ERROR)
;SETERR          STAB    FDSTAT                   ; E940: D7 08     ; 
;                BSR     EXITHNDLR                ; E942: 8D 4D     ; 
;                SEC                              ; E944: 0D        ; 
;                RTS                              ; E945: 39        ; 
;------------------------------------------------
;
;------------------------------------------------
STEP            LDAB    #$34                     ; E946: C6 34     ; Bit 5,4,2 set
                STAB    PIACTRL                  ; E948: F7 EC 02  ; CA2 low (STEP)
                LDAB    #$3C                     ; E94B: C6 3C     ; Bit 5,4,3,2 set
                STAB    PIACTRL                  ; E94D: F7 EC 02  ; CA2 high (STEP)
                LDX     #$00FE                   ; E950: CE 00 FE  ; 
WAIT3           LDAB    CLKFREQ                  ; E953: D6 19     ; is 3 for 1MHz
WAIT1           DECB                             ; E955: 5A        ; 
                BNE     WAIT1                    ; E956: 26 FD     ; 
                DEX                              ; E958: 09        ; 
                BNE     WAIT3                    ; E959: 26 F8     ; 
                RTS                              ; E95B: 39        ; 
;------------------------------------------------
WAIT2           LDX     #$0187                   ; E95C: CE 01 87  ; 
                BRA     WAIT3                    ; E95F: 20 F2     ; 
;------------------------------------------------
IE961           LDAA    TRACKSAV                 ; E961: 96 13     ; Function = RESTOR
                BEQ     SERR7                    ; E963: 27 D9     ; Is Track 0? If yes Set Error '7' (SEEK ERROR)
                CLRA                             ; E965: 4F        ; 
                LDAB    #$56                     ; E966: C6 56     ; = 86 <---- What is this??????
                BRA     RESTORY   ; -->          ; E968: 20 A0     ; 
;------------------------------------------------
; Comes from RESTORY
;------------------------------------------------
ZE96A           LDX     #$02C0                   ; E96A: CE 02 C0  ; |
                BSR     WAIT3                    ; E96D: 8D E4     ; Wait
                LDAA    FUNCSAV                  ; E96F: 96 0E     ; What was the function?
                BITA    #$08                     ; E971: 85 08     ; Is Function = RESTOR ?
                BNE     IE961                    ; E973: 26 EC     ; if yes, branch
                BITA    #$10                     ; E975: 85 10     ; Is FUNCTION SEEK?
                BNE     EXITHNDLR                ; E977: 26 18     ; If yes - Done
                LDAB    #$6F                     ; E979: C6 6F     ; Write Sync data address mark
                RORA                             ; E97B: 46        ; Get Bit 0 into carry
                BCC     IE980                    ; E97C: 24 02     ; Is Bit 0 set?
                LDAB    #$6A                     ; E97E: C6 6A     ; yes, write DELETED DATA MARK (WRDDAM)
IE980           STAB    ADDRMRK                  ; E980: D7 14     ; 
                BRA     NXTSEC                   ; E982: 20 4F     ; next sector
;------------------------------------------------
; Comes from NXTSEC
;------------------------------------------------
SECRDDONE       LDAA    FUNCSAV                  ; E984: 96 0E     ; Get Function
                BPL     EXITHNDLR                ; E986: 2A 09     ; Bit 7 not Set - done (READPS, RDCRC, RESTOR, SEEK, CLOCK)
                ANDA    #$40                     ; E988: 84 40     ; Isolate Bit 6
                STAA    FUNCSAV                  ; E98A: 97 0E     ; <------------- WHY??
                BEQ     EXITHNDLR                ; E98C: 27 03     ; Bit 6 NOT set - done (READSC, WRTEST, WRDDAM, WRITSC)
                JMP     CLOCKN                   ; E98E: 7E E8 D2  ; Bit 6 Set (normal codeflow) (RWTEST, WRVERF)
;------------------------------------------------
;
;------------------------------------------------
EXITHNDLR       LDAA    PIAREGA                  ; 
                ORAA    #$10                     ; Set Bit 4 (HLD)
                STAA    PIAREGA                  ; 
                LDX     #$433C     ;changed 
                LDAA    PIAREGB
                BITA    #$20                     ; check for drive 2 or 3
                BEQ     DRIVETHREE               ; if zero we are on drive >1
                LDX     #$633C    ;changed      ; E991: CE 03 3C  ; |
DRIVETHREE      STX     PIAREGB   ;write PIAREGB, changed   ; E994: FF EC 01  ; Set PB0,1,5,6 (RESET, DS2, DS3, WG) and Select A Output Reg., Set CA2 (STEP)
                LDX     NMIVECSAV                ; E997: DE 0F     ; |
                STX     NMIsVC                   ; E999: FF FF FC  ; Restore old NMIISR
;------------------------------------------------
; from here new code
;------------------------------------------------                
                LDAA    CURDRV                  ; 
                STAA    $FE05                   ; 
                LDX     $FE04                   ; 
                LDAA    TRACKSAV                ; 
                STAA    ,X                      ; 
;------------------------------------------------
; from here original code
;------------------------------------------------
                LDAA    STATSAV
                TAP                              ; E9A9: 06        ; Transfer A to Status Register
                CLC                              ; E9AA: 0C        ; 
                RTS                              ; E9AB: 39        ; 
;------------------------------------------------
; NXTSEC:
; if sector >= 27 - steps to next Track,
; updates SECTCNT, SofTRK, CWRDCNT, DWRDCNT
;------------------------------------------------
;                 
INCTRK          STAA    SofTRK                   ; E9CA: 97 0A     ; store new Sector of Track
                JSR     STEP                     ; E9CC: BD E9 46  ; move to next Track (X = 0)
                BSR     WAIT2                    ; E9CF: 8D 8B     ; wait some more (X = 0)
                INC     $13,X                    ; E9D1: 6C 13     ; increment TRACKSAV
NXTSEC          INC     SofTRK                   ; E9D3: 7C 00 0A  ; increment Sector of Track
                LDAA    SofTRK                   ; E9D6: 96 0A     ; Load it to A
                LDX     SECTCNT                  ; E9D8: DE 0B     ; Load Sector count
                BEQ     SECRDDONE   ; -->        ; E9DA: 27 A8     ; done?
                SUBA    #27                     ; E9DC: 80 1B     ; Sector == 27?
                BCC     INCTRK                   ; E9DE: 24 EA     ; increase Track
                LDAA    #$05                     ; E9E0: 86 05     ; 
                STAA    DRDCNT                   ; E9E2: 97 18     ; Data Read Counter
                DEX                              ; E9E4: 09        ; decrement sectorcount
START           LDAA    #$40                     ; E9E5: 86 40     ; One Sector is $40 words
                STX     SECTCNT                  ; E9E7: DF 0B     ; store updated sectorcount
                BNE     IE9F2                    ; E9E9: 26 07     ; not done, jump
                LDAA    LSCTLN                   ; E9EB: 96 05     ; LAST SofTRK LENGTH (1-128)
                ADDA    #$07                     ; E9ED: 8B 07     ; isolate Bit 0,1,2
                LSRA                             ; E9EF: 44        ; convert to words
                ANDA    #$FC                     ; E9F0: 84 FC     ; isolate Bit 0,1
IE9F2           STAA    DWRDCNT                  ; E9F2: 97 15     ; is $40 for every full sector
                NEGA                             ; E9F4: 40        ; 
                LDAB    FUNCSAV                  ; E9F5: D6 0E     ; 
                ASLB                             ; E9F7: 58        ; Check Bit 6 (RDCRC, RWTEST, WRVERF)
                BPL     NOCRC                    ; E9F8: 2A 01     ; if not set, jump
                CLRA                             ; E9FA: 4F        ; 
NOCRC           ADDA    #$40                     ; E9FB: 8B 40     ; 
                STAA    CWRDCNT                  ; E9FD: 97 09     ; 
                JSR     RETRG                    ; E9FF: BD EB 74  ; Retrigger NMI Timer X <- EC00
                ;LDAA    TRACKSAV ;changed       ; EA02: 96 13     ; 
                ;ORAB    #$0C     ;changed       ; EA04: CA 0C     ; In B is FUNCSAV<<1, isol. Bit 2,3 (RWTEST, WRTEST, CLOCK)
                ;CMPA    #$2B     ;changed       ; EA06: 81 2B     ; check for Track > 43
                ;BLS     IEA0C    ;changed       ; EA08: 23 02     ; No
                ;ANDB    #$FB     ;changed       ; EA0A: C4 FB     ; Clr Bit 2 (CLOCK, and TG43 on PA2)
IEA0C           ;STAB    ,X       ;changed       ; EA0C: E7 00     ; Set PortA
                LDX     #$D270                   ; EA0E: CE D2 70  ; Inhibit: Sync,Tx,Rx,Select CR3 and 1 Sync Character,Internal Sync,Clear: TUF,CTS
                STX     SSDA_0                   ; EA11: FF EC 04  ; |
                LDX     #$D1F5                   ; EA14: CE D1 F5  ; Select Sync Code Register and Set Sync Code to hex F5
                STX     SSDA_0                   ; EA17: FF EC 04  ; |
;*************************************************
; Looking for ID Addr. Mark, correct Track and Sector
;*************************************************
IEA1A           JSR     READINIT  ;changed ; EA1A: 8D 90     ; X is on PIAREGA (EC00), Toggle RESET
IEA1C           LDAA    $04,X                    ; EA1C: A6 04     ; Read SSDA Status Reg.
                BPL     IEA1C                    ; EA1E: 2A FC     ; Wait for Data
                LDAA    $05,X                    ; EA20: A6 05     ; Read SSDA Data Reg
                CMPA    #$7E                     ; EA22: 81 7E     ; ID address mark ?
                BNE     IEA1A                    ; EA24: 26 F4     ; No ?, Try again.
IEA26           LDAA    $04,X                    ; EA26: A6 04     ; *********** Found Id Addr. Mark ***********
                BPL     IEA26                    ; EA28: 2A FC     ; Wait for Data
                LDAA    $05,X                    ; EA2A: A6 05     ; Read SSDA Data Reg into A
                LDAB    $05,X                    ; EA2C: E6 05     ; Read SSDA Data Reg into B
                CMPA    TRACKSAV                 ; EA2E: 91 13     ; Compare with Track
                BNE     IEA1A                    ; EA30: 26 E8     ; Try again
IEA32           LDAA    $04,X                    ; EA32: A6 04     ; We are on the right Track, Read SSDA Status Reg.
                BPL     IEA32                    ; EA34: 2A FC     ; Wait for Data
                LDAA    $05,X                    ; EA36: A6 05     ; Read SSDA Data Reg into A
                LDAB    $05,X                    ; EA38: E6 05     ; Read SSDA Data Reg into B
                CMPA    SofTRK                   ; EA3A: 91 0A     ; Compare with sector
                BNE     IEA1A                    ; EA3C: 26 DC     ; Try again
IEA3E           LDAA    $04,X                    ; EA3E: A6 04     ; Found Sector, Read SSDA Status Reg.
                BPL     IEA3E                    ; EA40: 2A FC     ; Wait for Data
                TST     $05,X                    ; EA42: 6D 05     ; SSDA Data Reg
                LDAA    CLKFREQ                  ; EA44: 96 19     ; 
IEA46           SUBA    #$03                     ; EA46: 80 03     ; 
                BHI     IEA46                    ; EA48: 22 FC     ; 
                LDAA    $01,X                    ; EA4A: A6 01     ; Read PIAREGB
                BMI     SERR9                    ; EA4C: 2B 3B     ; PB7 High ? (CRC-0) - ERROR
;*************************************************
; Found corr. Track and Sector, now look for Data Addr. Mark
;*************************************************
                LDAA    $05,X                    ; EA4E: A6 05     ; 
                LDAA    #$04                     ; EA50: 86 04     ; Try four times
IEA52           TST     $04,X                    ; EA52: 6D 04     ; Read SSDA Status Reg.
                BPL     IEA52                    ; EA54: 2A FC     ; Wait for Data
                CMPA    $05,X                    ; EA56: A1 05     ; Compare SSDA Data Reg to A (04)
                CMPA    $05,X                    ; EA58: A1 05     ; Compare SSDA Data Reg to A (04)
                DECA                             ; EA5A: 4A        ; |
                BNE     IEA52                    ; EA5B: 26 F5     ; try again
                LDAB    FUNCSAV                  ; EA5D: D6 0E     ; 
                BMI     WRITINI2   ; -->         ; EA5F: 2B 7C     ; Bit 7 set - Write Function
                LDAB    CLKFREQ                  ; EA61: D6 19     ; Waitloop
                ASLB                             ; EA63: 58        ; |
IEA64           INX                              ; EA64: 08        ; |
                DEX                              ; EA65: 09        ; |
                DECB                             ; EA66: 5A        ; |
                BNE     IEA64                    ; EA67: 26 FB     ; |
                LDAB    #$04                     ; EA69: C6 04     ; Try four times
IEA6B           JSR     READINIT                 ; EA6B: BD E9 AC  ; Initialize SSDA for Read, Toggle RESET
                LDX     CURADRH                  ; EA6E: DE 06     ; Load Start Address
IEA70           LDAA    SSDA_0                   ; EA70: B6 EC 04  ; Read SSDA Status Reg.
                BPL     IEA70                    ; EA73: 2A FB     ; Wait and Test RDA for 2 Bytes Ready
                LDAA    SSDA_1                   ; EA75: B6 EC 05  ; Read Data
                CMPA    #$6F                     ; EA78: 81 6F     ; Sync data address mark
                BEQ     READSECT  ; -->          ; EA7A: 27 21     ; ********** Found data address mark *************
                CMPA    #$6A                     ; EA7C: 81 6A     ; Read DELETED DATA MARK (set Error '4')
                BEQ     SERR4                    ; EA7E: 27 18     ; |
                DECB                             ; EA80: 5A        ; |
                BNE     IEA6B                    ; EA81: 26 E8     ; again
                LDAB    #$38                     ; EA83: C6 38     ; (Set Error '8') (DATA MARK ERROR)
;                CPX     #MC631                   ; EA85: 8C C6 31  ; 
                FCB     $8C
SERR1           LDAB    #$31                                       ; (Set Error '1') (DATA CRC ERROR)
;                CPX     #MC639                   ; EA88: 8C C6 39  ; 
                FCB     $8C
SERR9           LDAB    #$39                                        ; (Set Error '9') (ADDRESS MARK CRC ERROR)
                DEC     DRDCNT                   ; EA8B: 7A 00 18  ; decrement counter (was 5)
                BEQ     IEA9A                    ; EA8E: 27 0A     ; if 0, set error
                LDX     SECTCNT                  ; EA90: DE 0B     ; if not, try again
                JMP     START      ; -->         ; EA92: 7E E9 E5  ; 
;------------------------------------------------
SERR2           LDAB    #$32                     ; EA95: C6 32     ; Set Error '2' (DISK WRITE PROTECTED)
;                CPX     #MC634                   ; EA97: 8C C6 34  ; 
                FCB     $8C
SERR4           LDAB    #$34                                        ; (Set Error '4') (READ DELETED DATA MARK)
IEA9A           JMP     SETERR     ; -->         ; EA9A: 7E E9 40  ; 
;------------------------------------------------
READSECT        LDAB    FUNCSAV                  ; EA9D: D6 0E     ; data address mark found
                ASLB                             ; EA9F: 58        ; FUNCSAV<<1
                BMI     CRCONLY                  ; EAA0: 2B 19     ; Check for READCRC ($40, Bit 6)
                LDAB    DWRDCNT                  ; EAA2: D6 15     ; Data Wordcount
IEAA4           LDAA    SSDA_0                   ; EAA4: B6 EC 04  ; Read SSDA Status Reg.
                BPL     IEAA4                    ; EAA7: 2A FB     ; Wait and Test RDA for 2 Bytes Ready
                LDAA    SSDA_1                   ; EAA9: B6 EC 05  ; Read Data to Buffer
                CPX     PROM_0                   ; EAAC: BC FC FC  ; X is on $20, in PROM_0 is zero *** This does nothing ! ***
                STAA    ,X                       ; EAAF: A7 00     ; |
                LDAA    SSDA_1                   ; EAB1: B6 EC 05  ; |
                STAA    $01,X                    ; EAB4: A7 01     ; |
                INX                              ; EAB6: 08        ; |
                INX                              ; EAB7: 08        ; |
                DECB                             ; EAB8: 5A        ; |
                BNE     IEAA4                    ; EAB9: 26 E9     ; 
CRCONLY         LDAB    CWRDCNT                  ; EABB: D6 09     ; CRC Wordcount
READCRC         LDAA    SSDA_0                   ; EABD: B6 EC 04  ; Read SSDA Status Reg.
                BPL     READCRC                  ; EAC0: 2A FB     ; Wait and Test RDA for 2 Bytes Ready
                DECB                             ; EAC2: 5A        ; dec. counter
                BMI     CRCOK                    ; EAC3: 2B 08     ; finished ?
                LDAA    SSDA_1                   ; EAC5: B6 EC 05  ; Read SSDA Data to A
                LDAA    SSDA_1                   ; EAC8: B6 EC 05  ; Read SSDA Data to A
                BRA     READCRC                  ; EACB: 20 F0     ; continue

CRCOK           LDAA    CLKFREQ                  ; EACD: 96 19     ; 
IEACF           SUBA    #$03                     ; EACF: 80 03     ; 
                BHI     IEACF                    ; EAD1: 22 FC     ; 
                LDAA    PIAREGB                  ; EAD3: B6 EC 01  ; |
                BMI     SERR1                    ; EAD6: 2B AE     ; PB7 High ? (CRC-0) Error
                STX     CURADRH                  ; EAD8: DF 06     ; CRC ok
                JMP     NXTSEC     ; -->         ; EADA: 7E E9 D3  ; next sector
;------------------------------------------------
; from here new code
;------------------------------------------------                
WRITINI2        LDX     #$C0DA                   ; EADD: CE C0 DA  ; 
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
                BNE     SERR2    ;changed    ;+4 ; EB00: 26 93     ; If high set Error (otherwise it conflicts with format.sy)
                ANDA    #$60     ; changed   ;+2                   ; Clear all but Bit 5,6 (DS2, DS3)
                TAB                          ;+2
                ORAA    #$02     ; changed   ;+2                   ; Set Bit 1 (WG high)
                STAA    PIAREGB  ;write PIAREGB ;+5 ; EAF8: B7 EC 01  
                                ; +6 cycles slower
                LDAA    CLKFREQ              ;+3 ; EB02: 96 19     ; Waitloop A is 3 on 1MHz
                SUBA    #$03                 ;+2 ; EB04: 80 03     ; |
                ASLA                         ;+2 ; EB06: 48        ; |
IEB07           DECA                         ;+2 ; EB07: 4A        ; |
                BPL     IEB07                ;+4 ; EB08: 2A FD     ; | A is FF
                STAB    PIAREGB  ;changed   ;+5                    ; Bit 5,6 (DS2, DS3 unaffected), WG low
                LDAB    FUNCSAV             ;+3
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
WRITSEC         LDAA    $12      ;changed     ;+2                  ; $12 is alway zero    
                LDAA    ,X                       ; EB35: A6 00     ; Load Data from RAM
                STAA    SSDA_1                   ; EB37: B7 EC 05  ; Write to disk
                LDAA    $01,X                    ; EB3A: A6 01     ; Load next Byte
                STAA    SSDA_1                   ; EB3C: B7 EC 05  ; Write to disk
                BCS     IEB43                    ; EB3F: 25 02     ; if carry is set, don't INX (RWTEST or WRTEST or WRDAM)
                INX                              ; EB41: 08        ; if not, increase pointer
                INX                              ; EB42: 08        ; |
IEB43           DECB                             ; EB43: 5A        ; decrease counter
                BNE     IEB2A                    ; EB44: 26 E4     ; check if underflow, if not, continue from top
                STX     CURADRH                  ; EB46: DF 06     ; done, store current address
                LDX     #PIAREGA                 ; EB48: CE EC 00  ; 
                LDAA    #$40                     ; EB4B: 86 40     ;  
IEB4D           BITA    $04,X                    ; EB4D: A5 04     ; Check SSDA Status Reg.
                BEQ     IEB4D                    ; EB4F: 27 FC     ; Wait for Data
                STAB    $05,X                    ; EB51: E7 05     ; Store B to Data Register
                LDAB    PIAREGB ;read PIAREGB ;+4  ; EB59: E7 01   ;
                ANDB    #$60                                       ; Clear all but Side and DS2
                ORAB    #$08                ;+2                    ; Set SHIFT_CRC
IEB53           BITA    $04,X                    ; EB53: A5 04     ; Check SSDA Status Reg.
                BEQ     IEB53                    ; EB55: 27 FC     ; Wait for Data
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
                ANDB    #$60     ; changed ;+2                     ; 
                STAB    PIAREGB            ;+5                     ; Clear all but Side and DS2
                INC     PIAREGB            ;+6                     ; Set PB0 high
                INC     PIAREGB            ;+6                     ; Set PB0 low (toggle RESET), Set WG high
                JMP     NXTSEC    ; -->          ; EB71: 7E E9 D3  ; next sector  

; ###########  about 6 Bytes free
                ORG     $EB90

CLRTOP          LDX     #$0014                   ; EB90: CE 00 14 ; Diagnostic with clear
ZEB93           CLR     $5F,X                    ; EB93: 6F 5F    ; Clear $60-$73
                DEX                              ; EB95: 09       ; |
                BNE     ZEB93                    ; EB96: 26 FB    ; |
TOP             JMP     TOPEXT                                    ; Jump to external ROM

READINIT        LDX     #$D0D8                   ; E9AC: CE D0 D8  ; Select CR2 and Inhibit SM, 2-Byte RDA, 8-Bit Word
                STX     SSDA_0                   ; E9AF: FF EC 04  ; |
                LDX     #PIAREGA                 ; E9B2: CE EC 00  ; 
                LDAA    #$50                     ; E9B5: 86 50     ; 
                STAA    $04,X              ;6    ; E9B7: A7 04     ; Write $50 to SSDA_0 (Enable Read)
                LDAA    PIAREGB            ;+4                     ; Read PIAREGB
                ANDA    #$60               ;+2                     ; clear all but Bit 5,6
                ORAA    #$07               ;+2                     ; same as before but leave PB5,6
                STAA    $01,X   ;write PIAREGB ;6 ; E9BB: A7 01    ; Set Bit0,1,2 in PIAREGB (Set to Read, Reset active., WG inact.)
                DEC     $01,X   ;write PIAREGB ;7  ; E9BD: 6A 01   ; Reset inact.
                DECA                      ;+2                    ; timing
                ;INX                        ;+4 didnt help
                ;DEX                        ;+4 didnt help
                LDAA    #$40                     ; E9C1: 86 40     ; Write $40 to SSDA_0 (Enable Sync, Select CR2)
                STAA    $04,X                    ; E9C3: A7 04     ; 
                LDAA    #$98                     ; E9C5: 86 98     ; Write $98 to SSDA_1 (Enable SM Output)
                STAA    $05,X                    ; E9C7: A7 05     ; 
                RTS                              ; E9C9: 39        ; 

;------------------------------------------------
; ########### 3 Bytes free
                ORG     $EBC0
LPINIT          JMP     LPINITEXT
;------------------------------------------------
; ########### 9 Bytes free
                ORG     $EBCC
LIST            JMP     LPLISTEXT
;------------------------------------------------
FDINITEXT       LDAA    #$FE                ; | $ FE00-03  is a pointer to the Position of Track / Drive
                STAA    $FE04               ; | $FE04 <- $FE 
                LDX     #$0000
                CLR     $12
                JMP     FDINTBACK
;------------------------------------------------
; ########### 7 Bytes free
                ORG     $EBE4
LDATA           JMP     LPDATEXT 
;------------------------------------------------
FDINIT3         JSR     FDINIT              ; EBA6: BD E8 22  ; 
                JMP     CLOCK               ; EBA9: 7E E8 87  ; initializes and starts drive, then jumps to CLKDMD
;------------------------------------------------
; ########### 5 Bytes free
                ORG     $EBF2
LDATA1          JMP     LPDAT0EXT
;------------------------------------------------
                ORG     $EBFE
                FDB     $0750                    ; EBFE: 07 50          '.P'

                REPT    $100                      ; Filler for IO Area
                FCB     $10
                ENDM
                

; ################################################################
; Extended ROM
; ################################################################
                  ORG $ED00

CLKDMD          JSR     RETRG                    ; EB85: 8D ED     ; Retrigger NMI Timer
                LDAB    $01,X                    ; EB87: E6 01     ; PIAREGB Clear Interrupt Flag
                CLRA                             ; EB89: 4F        ;  
CONT01          LDAB    $03,X                    ; EB8A: E6 03     ; PIACTRLB
                BPL     CONT01                   ; EB8C: 2A FC     ; Wait for IRQB on CB1 (IDX)
                LDAB    $01,X                    ; EB8E: E6 01     ; PIAREGB Clear Interrupt Flag
IDXHER          CLRB                             ; EB90: 5F        ;  
IEB91           DECB                             ; EB91: 5A        ; Wait
                BNE     IEB91                    ; EB92: 26 FD     ; |
                INCA                             ; EB94: 4C        ; In A is the ammount of 256 ticks
                TST     $03,X                    ; EB95: 6D 03     ; PIACTRLB
                BPL     IDXHER                   ; EB97: 2A F7     ; Wait for IRQB on CB1 (IDX) 166ms later
                INCB                             ; EB99: 5C        ; B is 1 now
                SUBA    #$4B                     ; EB9A: 80 4B     ;  
IEB9C           INCB                             ; EB9C: 5C        ;  
CONT02          SUBA    #$16                     ; EB9D: 80 16     ;  
                BCC     IEB9C                    ; EB9F: 24 FB     ;  
                STAB    CLKFREQ                  ; EBA1: D7 19     ; B is 3 for 1MHz
                JMP     EXITHNDLR  ; -->         ; EBA3: 7E E9 91  ; EXITHNDLR has RTS

;------------------------------------------------
; from here new code
;------------------------------------------------                
RETRG           LDX     #PIAREGA                 ; EB74: CE EC 00  ; 
                LDAA    #$36                     ; EB77: 86 36     ; |
                STAA    $03,X                    ; EB79: A7 03     ; $36 -> PIACTRLB (CB2 Low - Trigger NMI Timer, IRQB Flag set by HL on CB1 - IDX, Select Output Reg. B)
                LDAA    #$3E                     ; EB7B: 86 3E     ; |
                STAA    $03,X                    ; EB7D: A7 03     ; $3E -> PIACTRLB (CB2 high)
                LDAB    ,X                       ; EB7F: E6 00     ; Get Port A
                DECA                             ; EB81: 4A        ; 
                STAA    $02,X                    ; EB82: A7 02     ; $3D -> PIACTRLA (En. IRQA interrupt by CA1 - NMI Timer, Sel. Output Reg. A, CA2 high - STEP)
                RTS                              ; EB84: 39        ; 
;------------------------------------------------
;
;------------------------------------------------
NMIISR          LDS     STACKSAV                 ; E939: 9E 16     ; 
                LDAB    #$35                     ; E93B: C6 35     ; Set Error '5' (TIMEOUT)
;                CPX     #MC637                   ; E93D: 8C C6 37  ; 
                FCB     $8C
xSERR7          LDAB    #$37                                       ; Set Error '7' (SEEK ERROR)
SETERR         STAB    FDSTAT                   ; E940: D7 08     ; 
                JSR     EXITHNDLR                ; E942: 8D 4D     ; 
                SEC                              ; E944: 0D        ; 
                RTS                              ; E945: 39        ; 


;ED00     jmp WRITINI
;ED03    jmp TOP
;ED06    jmp LPINIT
;ED09    jmp LIST
;ED0C    jmp LDATA
;ED0F    jmp LDATA1
;ED12    jmp EXIT
;ED15    jmp EXIT
;ED18    jmp EXIT
;EXIT    rts

;------------------------------------------------
; Disk Mini Diagnostic Routine (CLRTOP is in original ROM)
;------------------------------------------------
TOPEXT          JSR     FDINIT                   ; EB98: BD E8 22 ; Diagnostic without clear
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
ZEBB9           JMP     PRNTE2                   ; EBB9: 7E E8 55 ; Print Error Code

;------------------------------------------------
; Line Printer Routines
;------------------------------------------------
LPINITEXT       LDX     #$FF2E                   ; EBC0: CE FF 2E ; Line Printer Init
                STX     LPTPIA0                  ; EBC3: FF EC 10 ; <------- PIA here ?
                LDAA    #$3C                     ; EBC6: 86 3C    ; 
                STAA    LPTPIA3                  ; EBC8: B7 EC 13 ; <------- PIA here ?
                RTS                              ; EBCB: 39       ; 
;------------------------------------------------
LPLISTEXT       STAA    LPTPIA0                  ; EBCC: B7 EC 10 ; Print contents of A
                LDAA    LPTPIA0                  ; EBCF: B6 EC 10 ; <------- PIA here ?
ZEBD2           PSHB                             ; EBD2: 37       ; 
                LDAB    LPTPIA2                  ; EBD3: F6 EC 12 ; <------- PIA here ?
                ANDB    #$03                     ; EBD6: C4 03    ; 
                DECB                             ; EBD8: 5A       ; 
                PULB                             ; EBD9: 33       ; 
                BNE     ZEBE2                    ; EBDA: 26 06    ; 
                TST     LPTPIA1                  ; EBDC: 7D EC 11 ; <------- PIA here ?
                BPL     ZEBD2                    ; EBDF: 2A F1    ; 
                RTS                              ; EBE1: 39       ; 
;------------------------------------------------
ZEBE2           SEC                              ; EBE2: 0D       ; 
                RTS                              ; EBE3: 39       ; 
;------------------------------------------------
LPDATEXT        LDAA    #$0D                     ; EBE4: 86 0D    ; Send String to Printer
ZEBE6           BSR     LPLISTEXT                ; EBE6: 8D E4    ; 
                BCS     ZEBE6                    ; EBE8: 25 FC    ; 
                LDAA    #$0A                     ; EBEA: 86 0A    ; 
                DEX                              ; EBEC: 09       ; 
ZEBED           BSR     LPLISTEXT                ; EBED: 8D DD    ; 
                BCS     ZEBED                    ; EBEF: 25 FC    ; 
                INX                              ; EBF1: 08       ; 
LPDAT0EXT       LDAA    ,X                       ; EBF2: A6 00    ; 
                CMPA    #$04                     ; EBF4: 81 04    ; 
                BNE     ZEBED                    ; EBF6: 26 F5    ; 
                RTS                              ; EBF8: 39       ; 
;------------------------------------------------




                ORG     $EFFE          ; Just to make sure the Area from End to EFFF is filled
                FDB     $0750
