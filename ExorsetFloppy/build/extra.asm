
CURDRV    EQU     $0000
STRSCTH   EQU     $0001
STRSCTL   EQU     $0002
NUMSCTH   EQU     $0003
NUMSCTL   EQU     $0004
LSCTLN    EQU     $0005
CURADRH   EQU     $0006
FDSTAT    EQU     $0008
M0009     EQU     $0009
SECTOR    EQU     $000A
SECTCNT   EQU     $000B
STATSAV   EQU     $000D
FUNCSAV   EQU     $000E
NMIVECSAV EQU     $000F
M0012     EQU     $0012
TRACKSAV  EQU     $0013
M0014     EQU     $0014
WRDCNT    EQU     $0015
STACKSAV  EQU     $0016
M0018     EQU     $0018
CLKFREQ   EQU     $0019
LDADDR    EQU     $0020
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

SETERR	EQU    $E940
ERRHNDLR	EQU    $E991
RETRG    EQU    $EB74
DRVRDY   EQU    $E8C2

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

; ################################################################
; Code ED00
; ################################################################
		ORG $ED20
	ldx #$0012
	LDAA    #$01
	CMPA    CURDRV      ; is 0 at start
         BCS     SERR3       ; if CURDRV > 1 Set Error 3
         BEQ     ZE8AD       ; if CURRDRV == 1 leave it @ $12
         DEX                 ; else, down to $11
         INCA                ; A is 2 now
ZE8AD    STAA    PIAREGA     ; Write DS0, TG43, DIRQ, HLD low, Set DS1
         LDAA    ,X          ; X @ $11 for Drive 0 or $12 for Drive 1
         STAA    TRACKSAV    ; 
         PSHB
         bsr 	WAIT4
         PULB
         LDAA    #$40        ; PA6 (RDY)
CHECKAGAIN   BITA    PIAREGA     ; Check Drive Ready
         bne 	CHECKAGAIN
         JMP     DRVRDY     ; 
SERR3    LDAB    #$33        ; Error '3' (DISK NOT READY)
         jmp     SETERR      ; 
;------------------------------------------------
;DRVRDYJ  jmp     DRVRDY
;	jmp $E8A3
;------------------------------------------------
WAIT4	ldx 	#$4000
WAIT3    LDAB    CLKFREQ     ; is 3 for 1MHz
WAIT1    DECB                ; 
         BNE     WAIT1       ; 
         DEX                 ; 
         BNE     WAIT3       ; 
         RTS                 ; 

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