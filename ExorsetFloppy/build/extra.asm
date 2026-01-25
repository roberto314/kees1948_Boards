
CURDRV    EQU     $0000

CURADRH   EQU     $0006
FDSTAT    EQU     $0008

STATSAV EQU     $000D
TRACKSAV  EQU     $0013
ADDRMRK   EQU     $0014

CLKFREQ   EQU     $0019

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

DRVRDY   EQU    $E8C2
SETERR	EQU    $E940
ERRHNDLR	EQU    $E991
STORTRACKS EQU  $EE40
;##########################  ATTENTION from approx E9C0 difference in 
RETRG    EQU    $EAE0
NXTSEC  EQU     $E9D2
; ################################################################
; Jump Table
; ################################################################
		ORG $ED00
ED00	jmp $ED20
		ORG $ED06
ED06	
	STAA    TRACKSAV     ; contains track (0 if RESTOR or 3)
	SBA
	jmp $E90D

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; DS1 is not used!
;             PA0 PA1 PB5
;             DS0 DS1 DS2
; DRVNUM = 0  low hi  hi
; DRVNUM = 1  hi  low hi
; DRVNUM = 2  low hi  low
; DRVNUM = 3  hi  low low
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
		ORG $ED20
	ldx     #$0012
	;ldx     #$FE03
	LDAA    #$01
	;LDAB    #$02
	CMPA    CURDRV              ; is 0 at start
         BCS     SERR3   ;changed    ; if CURDRV > 3 Set Error 3
         BEQ     ZE8AD               ; if CURRDRV == 3 leave it @ $FE03
         DEX                         ; else, down to $11
         INCA                        ; A is 2 now

ZE8AD    STAA    PIAREGA             ; Write DS0, TG43, DIRQ, HLD low, Set DS1
         LDAA    ,X                  ; X @ $11 for Drive 0 or $12 for Drive 1
         STAA    TRACKSAV            ; 
         
         PSHB
         bsr 	WAIT4
         PULB
         LDAA    #$40        ; PA6 (RDY)
CHECKAGAIN   BITA    PIAREGA     ; Check Drive Ready
         ;bne 	CHECKAGAIN
         JMP     DRVRDY     ; 

SERR3    LDAB    #$33        ; Error '3' (DISK NOT READY)
         jmp     SETERR      ; 
;------------------------------------------------
;DRVRDYJ  jmp     DRVRDY
;	jmp $E8A3
;------------------------------------------------
WAIT4	ldx 	#$800
WAIT3    LDAB    CLKFREQ     ; is 3 for 1MHz
WAIT1    DECB                ; 
         BNE     WAIT1       ; 
         DEX                 ; 
         BNE     WAIT3       ; 
         RTS                 ; 

;------------------------------------------------
;
;------------------------------------------------
		ORG $ED60
READINIT        LDX     #$D0D8                   ; E9AC: CE D0 D8  ; Select CR2 and Inhibit SM, 2-Byte RDA, 8-Bit Word
                STX     SSDA_0                   ; E9AF: FF EC 04  ; |
                LDX     #PIAREGA                 ; E9B2: CE EC 00  ; 
                LDAA    #$50                     ; E9B5: 86 50     ; 
                STAA    $04,X              ;6    ; E9B7: A7 04     ; Write $50 to SSDA_0 (Enable Read)
                ;STAA    SSDA_0            ;5    ; E9B7: A7 04     ; Write $50 to SSDA_0 (Enable Read)
                ;LDAA    #$07    ;changed  ;-2   ; E9B9: 86 07     ; 
                ;LDAA    $01,X             ;+5                      ; Read PIAREGB
                LDAA    PIAREGB            ;+4                      ; Read PIAREGB
                ANDA    #$60               ;+2                      ; clear all but Bit 6,7
                ORAA    #$07               ;+2                      ; same as before but leave PB6,7
                STAA    $01,X   ;write PIAREGB ;6 ; E9BB: A7 01     ; Set Bit0,1,2 in PIAREGB (Set to Read, Reset active., WG inact.)
                ;STAA    PIAREGB ;write PIAREGB ;5  ; E9BB: A7 01     ; Set Bit0,1,2 in PIAREGB (Set to Read, Reset active., WG inact.)
                DEC     $01,X   ;write PIAREGB ;7  ; E9BD: 6A 01     ; Reset inact.
                ;DEC     PIAREGB ;write PIAREGB ;6  ; E9BD: 6A 01     ; Reset inact.
               ; INX                       ;-4    ; E9BF: 08        ; useless or timing?
               ; DEX                       ;-4    ; E9C0: 09        ; useless or timing?
               DECA                        ;+2                     ; timing
                LDAA    #$40                     ; E9C1: 86 40     ; Write $40 to SSDA_0 (Enable Sync, Select CR2)
                STAA    $04,X                    ; E9C3: A7 04     ; 
                LDAA    #$98                     ; E9C5: 86 98     ; Write $98 to SSDA_1 (Enable SM Output)
                STAA    $05,X                    ; E9C7: A7 05     ; 
                RTS                              ; E9C9: 39        ; 

;------------------------------------------------
;
;------------------------------------------------
		ORG $ED90
SERR2           LDAB  #$32                                        ;Set Error '2' (DISK WRITE PROTECTED) 
                JMP SETERR

WRITINI2        LDX     #$C0DA                   ; EADD: CE C0 DA  ; 
                STX     SSDA_0                   ; EAE0: FF EC 04  ; 
                LDX     #$C1AA                   ; EAE3: CE C1 AA  ; 
                STX     SSDA_0                   ; EAE6: FF EC 04  ; 
                LDX     #$C270                   ; EAE9: CE C2 70  ; 
                STX     SSDA_0                   ; EAEC: FF EC 04  ; 
                INC     PIAREGB  ;write PIAREGB  ; EAEF: 7C EC 01  ; Reset inactive (PB0 high)
                LDAA    #$82                     ; EAF2: 86 82     ; Bit 1,7
                STAA    SSDA_0                   ; EAF4: B7 EC 04  ; 
               ; DEX                         ;-4     ; EAF7: 09        ; useless ?
                LDAA    PIAREGB  ; changed   ;+4                        
                BITA    #$10     ; changed   ;-2                   ; Check Writeprot?
                BNE     SERR2        ;changed    ; EB00: 26 93     ; If high set Error (otherwise it conflicts with format.sy)
                ANDA    #$60     ; changed   ;+2                   ; Clear all but Bit 6,7 (DS2, DS3)
                TAB                         ;+2
                ORAA    #$02     ; changed   ;+2                   ; Set Bit 1 (WG high)
                STAA    PIAREGB  ;write PIAREGB ;+5 ; EAF8: B7 EC 01  ; WG off, PB7 is an Input 
                ;LDAA    #$10               ;-2   ; EAFB: 86 10     ; Bit 4
                ;BITA    PIAREGB                  ; EAFD: B5 EC 01  ; Check Writeprot?
                ;BEQ     SERR2                    ; EB00: 27 93     ; If low set Error
                LDAA    CLKFREQ                  ; EB02: 96 19     ; Waitloop
                SUBA    #$03                     ; EB04: 80 03     ; |
                ASLA                             ; EB06: 48        ; |
IEB07           DECA                             ; EB07: 4A        ; |
                BPL     IEB07                    ; EB08: 2A FD     ; |
 ;               CLR     PIAREGB  ;write PIAREGB ;-6 ; EB0A: 7F EC 01  ; Clear all PB (Set to Write, Reset active, ShiftCRC off, WG on)
                STAB    PIAREGB  ;changed   ;+5                  ; Bit 5,6 (DS2, DS3 unaffected)
                ;RORB                        ;-2     ; EB0D: 56        ; useless ?
;                BCC     IEB11                    ; EB0E: 24 01     ; 
;                BITA    #$56                ;-2  ; EB10: 85 56     ; 
;                FCB     $85                
;IEB11           RORB                        ;-2     ;                 ; useless ?
                LDX     #$0005                   ; EB12: CE 00 05  ; 
                JSR     WAIT3                    ; EB15: BD E9 53  ; 
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
WRITSEC         ;LDAA    PROM_0             ;-4   ; EB31: B6 FC FC  ; in PROM_0 is zero
                ;NOP                        ;-2   ; EB34: 01        ;  
                CLRA           ;changed     ;+2                       
                LDAA    ,X                       ; EB35: A6 00     ; Load Data from RAM
                STAA    SSDA_1                   ; EB37: B7 EC 05  ; Write to disk
                LDAA    $01,X                    ; EB3A: A6 01     ; Load next Byte
                STAA    SSDA_1                   ; EB3C: B7 EC 05  ; Write to disk
                BCS     IEB43                    ; EB3F: 25 02     ; all done?
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
IEB53           BITA    $04,X                    ; EB53: A5 04     ; Check SSDA Status Reg.
                BEQ     IEB53                    ; EB55: 27 FC     ; Wait for Data
                ;STAB    $01,X   ;write PIAREGB ;-6  ; EB59: E7 01     ; Store $8 to PIAREGB (SHIFT-CRC high)
                ORAB    #$08                ;+2
                ;STAB    $01,X               ;+6
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
                ;CLR     $01,X   ;write PIAREGB ;7  ; EB6B: 6F 01     ; Clear PIAREGB           - toggle Reset and WG high
                ;INC     $01,X   ;write PIAREGB ;7  ; EB6D: 6C 01     ; |                       - toggle Reset and WG high
                ;INC     $01,X   ;write PIAREGB ;7  ; EB6F: 6C 01     ; should be $2 in PIAREGB - toggle Reset and WG high
                JMP     NXTSEC    ; -->          ; EB71: 7E E9 D3  ; next sector

		ORG $EE40
;STORTRACKS
                LDAA    TRACKSAV                 ; E99C: 96 13     ; 
                LDAB    CURDRV                   ; E99E: D6 00     ; 
                BEQ     IE9A5                    ; E9A0: 27 03     ; 
                STAA    $12                      ; E9A2: 97 12     ; 
;                CPX     #$9711                   ; E9A4: 8C 97 11  ; 
                FCB     $8C
IE9A5           STAA    $11                                       ;
                LDAA    STATSAV                  ; E9A7: 96 0D     ; 
                RTS                              ; E9AB: 39        ; 

		ORG $EFDE
;------------------------------------------------
; Calculates CPU Frequency with Disk Rotation Period
;------------------------------------------------
CLKDMD          JSR     RETRG                    ; EB85: 8D ED     ; Retrigger NMI Timer
                LDAB    $01,X                    ; EB87: E6 01     ; PIAREGB Clear Interrupt Flag
                CLRA                             ; EB89: 4F        ; 
ZEB8A           LDAB    $03,X                    ; EB8A: E6 03     ; PIACTRLB
                BPL     ZEB8A                    ; EB8C: 2A FC     ; Wait for IRQB on CB1 (IDX)
                LDAB    $01,X                    ; EB8E: E6 01     ; PIAREGB Clear Interrupt Flag
ZEB90           CLRB                             ; EB90: 5F        ; 
ZEB91           DECB                             ; EB91: 5A        ; Wait
                BNE     ZEB91                    ; EB92: 26 FD     ; |
                INCA                             ; EB94: 4C        ; In A is the ammount of 256 ticks
                TST     $03,X                    ; EB95: 6D 03     ; PIACTRLB
                BPL     ZEB90                    ; EB97: 2A F7     ; Wait for IRQB on CB1 (IDX) 166ms later
                INCB                             ; EB99: 5C        ; B is 1 now
                SUBA    #$4B                     ; EB9A: 80 4B     ; 
ZEB9C           INCB                             ; EB9C: 5C        ; 
                SUBA    #$16                     ; EB9D: 80 16     ; 
                BCC     ZEB9C                    ; EB9F: 24 FB     ; 
                STAB    CLKFREQ                  ; EBA1: D7 19     ; B is 3 for 1MHz
                JMP     ERRHNDLR                 ; EBA3: 7E E9 91  ; 
;------------------------------------------------
	END