; UPDATED 990605 JWD - dispcheck



;///[ DISPLAY2.H ]/////////////////////////////////////////////////////////////////
;                        DISPLAY aansturing
;
; GEMODIFICEERD VOOR 23 ZENDER !!!
;
;  Deze routines zijn voor de aansturing van LCD-display's met 1x16 of 2x16
;  tekens. Als je deze routines wilt gebruiken moet je een van de volgende 
;  regels in je hoofdprogramma plaatsen:
;  #DEFINE SOORTLCD 16   (voor een 1x16 regelig display)
;  of:
;  #DEFINE SOORTLCD 32   (voor een 2x16 regelig display)
;  standaard is een display met een 1x16 regelig display geselecteerd
;
;  Geschreven en getest door Willem en Werner 1994 ... 1996.
;
;  Bevat de volgende routines:
;
;  DISPINIT : initialiseert het LCD-display. Het scherm wordt schoongemaakt
;             en de display wordt in 8 bits mode gezet.
;  
;  DISPUSER : Lees een aantal zelfgedefinieerde tekens in. Het startadres
;             van de tekens moet in DPTR staan en het aantal tekens in  
;             accu staan.
;
;  DISPCOM  : Stuur het byte in de accu naar het commandoregister van het
;             display.
;
;  DISP2CHAR : Stuur het byte in de accu naar het dataregister van het
;             display.
;
;  DISPBYTE : Beeld een hex getal van 0 - 0FFH vanuit de accu naar
;             het display.
;
;  DISPTEXT : Stuur de door de DPTR aangegeven tekst naar het display.
;
;  DISPREAD : Lees een byte uit het dataregister van het display (nog niet
;             geschreven). Byte komt terug in de accu.  
;/////////////////////////////////////////////////////////////////////////

DPWRCOM .EQU    0C000H
DPWRDAT .EQU    0C001H
DPRDCOM .EQU    0C002H
DPRDDAT .EQU    0C003H          ; Het adres waar het display zit.

#IFNDEF SOORTLCD
  #DEFINE SOORTLCD 16
#ENDIF


       
;---------------------------------------------------------------
DISPINIT: 
	;Initialiseert het display.
	; gebruikt geen registers
;---------------------------------------------------------------

	PUSH  ACC
	MOV   A,#38H          ;8 bits mode
	LCALL DISPCOM         ;stuur naar controladres LCD
	MOV   A,#01H          ;clear display
	LCALL DISPCOM
	MOV   A,#0CH          ;enable en cursor off
	LCALL DISPCOM
	MOV   A,#06H          ;shift right
	LCALL DISPCOM
	POP   ACC
	RET


;---------------------------------------------------------------------
DISPUSER:
		;leest een aantal door de gebruiker def. karakters in.
		;pointer naar de karakters in DPTR, aantal in A.
;---------------------------------------------------------------------

	  PUSH  B               ; B even wegzetten

	  CLR   C
	  RLC   A               ;X2
	  RLC   A               ;X4
	  RLC   A               ;X8
				;in de accu staat het aantal in te lezen
				;regels.
	  MOV   B,A             ; B gaat aftellen
	  MOV   A,#040H         ;CG RAM startadres
	  LCALL DISPCOM

USCHM1:   MOV   A,#0
	  MOVC  A,@A+DPTR         ;Lees een byte en
	  PUSH    DPH
	  PUSH    DPL
	  PUSH    ACC             ;zet accu even op de stack
	; WACHT OP BUSYVLAG
	  MOV     DPTR,#DPRDCOM
USCHM2:   MOVX    A,@DPTR         ;lees het commando/statusregister
	  JB      ACC.7,USCHM2     ;wacht tot het 7e bit laag is (busy-vlag)
	; SCHRIJF DATA NAAR DISPLAY
	  MOV     DPTR,#DPWRDAT
	  POP     ACC             ;haal accu terug van stack
	  MOVX    @DPTR,A
	  POP     DPL
	  POP     DPH
	  INC     DPTR            ;volgende byte
	  DJNZ    B,USCHM1
	  MOV     A,#80H
	  LCALL   DISPCOM         ;CURSOR LINKSBOVEN
	  POP     B               ;B terughalen.
	  RET


;------------------------------------------------------------------------
DISPTEXT:
	;schrijft een string, beginnend op het adres aangegeven door DPTR
	;en eindigend met een '\0' naar het display.
	;de registers worden niet aangetast.
;------------------------------------------------------------------------
		PUSH    DPH                
		PUSH    DPL                            
		PUSH    ACC             ;DPTR,ACC opslaan

distxt1:        MOV     A,#0
		MOVC    A,@A+DPTR       ;lees een teken
		JZ      distxt2         ;als het een 0 is is hij KLAAR!
		LCALL   DISP2CHAR        ;schrijf naar display
		INC     DPTR
		SJMP    distxt1

distxt2:        POP     ACC
		POP     DPL
		POP     DPH
		RET                     ; UITGANG KOMPLETE ROUTINE


;------------------------------------------------------------------
DISPCHECK:
;------------------------------------------------------------------

		MOV	A, #040H
		LCALL	DISPCOM	
		MOV     DPTR,#DPRDCOM	
dispchk1:   	MOVX    A,@DPTR         ;lees het commando/statusregister
		JB      ACC.7,dispchk1	;wacht tot het 7e bit laag is (busy-vlag)
		MOV     DPTR,#DPWRDAT
		MOV	A, #055H
		MOVX    @DPTR,A		;write
		;lees byte terug
		MOV	A, #040H
		LCALL	DISPCOM
		MOV     DPTR,#DPRDCOM	
dispchk2:   	MOVX    A,@DPTR         ;lees het commando/statusregister
		JB      ACC.7,dispchk2	;wacht tot het 7e bit laag is (busy-vlag)
		MOV     DPTR,#DPRDDAT
		MOVX    A,@DPTR
		CJNE	A, #055H, dispchk3
		SETB	C
		RET
dispchk3:	CLR	C
		RET


;--------------------------------------------------------------------
DISP2CHAR:
		;schrijft het karakter in de accu naar het display.
		;Leest na het schrijven naar het display de cursorpositie
		;uit en plaatst indien nodig de cursor op de goede plek         
;---------------------------------------------------------------------
		;BEWAAR REGISTERS EVEN
		PUSH    DPH                
		PUSH    DPL
		PUSH    ACC             ;zet accu even op de stack

		; WACHT OP BUSYVLAG
		MOV     DPTR,#DPRDCOM
putchrm1:       MOVX    A,@DPTR         ;lees het commando/statusregister
		JB      ACC.7,putchrm1  ;wacht tot het 7e bit laag is (busy-vlag)

		; SCHRIJF DATA NAAR DISPLAY
		MOV     DPTR,#DPWRDAT
		POP     ACC             ;haal accu terug van stack
		MOVX    @DPTR,A
		PUSH    ACC

;                LCALL   DISPREADCOM     ;lees de cursorpositie          
;                MOV     DPL,A           ;sla de accu even op in DPL
;                ANL     A,#03FH         ;kijk alleen naar de laatste 6 bits
;                CJNE    A,#SOORTLCD/2,putchrm2
;                MOV     A,DPL           ;haal de accu terug
;                ANL     A,#11000000b    ;maak de onderste 6 bits 0
;                XRL     A,#11000000b    ;inverteer bit 6 en bit 7
;                LCALL   DISPCOM         ;zet cursor op de goeie plek

putchrm2:       ;ZET REGISTERS TERUG
		POP     ACC
		POP     DPL
		POP     DPH
		RET



;----------------------------------------------------------------------  
DISPBYTE:
		;zet het getal in de accu in hex in het display.
;----------------------------------------------------------------------

		PUSH    ACC
		PUSH    ACC               ; Zet de accu op de stack
		ANL     A,#0F0H           ; Filter eerste nibbel eruit
		SWAP    A                 ; Switch de nibble's
		ACALL   hexconv
		ACALL   DISP2CHAR          ; Verstuur naar de lcd
		POP     ACC               ; Haal de accu van de stack
		ANL     A,#0FH            ; Filter tweede nibbel eruit
		ACALL   hexconv
		ACALL   DISP2CHAR          ; Verstuur naar de lcd
		POP     ACC
		RET                      ; Terug naar de aanroep

hexconv:        ORL     A,#030H
		CJNE    A,#039H,dispb    ; als het niet 9 is spring
dispc:          RET
dispb:          JC      dispc           ; als het kleiner is dan 9 spring
		ADD     A,#7            ; als het groter is dan 9 tel er 7 bij op
		RET



;------------------------------------------------------------
DISPCOM: ;schrijft naar het commandoregister van het display
;------------------------------------------------------------
		PUSH    DPH                
		PUSH    DPL
		PUSH    ACC             ;zet accu even op de stack
		MOV     DPTR,#DPRDCOM   
putcomm1:       MOVX    A,@DPTR         ;lees het commando/statusregister
		JB      ACC.7,putcomm1  ;wacht tot het 7e bit laag is (busy-vlag)
		MOV     DPTR,#DPWRCOM
		POP     ACC             ;haal accu terug van stack
		MOVX    @DPTR,A         ;en schrijf naar het display
		POP     DPL
		POP     DPH
		RET


;--------------------------------------------------------------
DISPREAD:       
		;lees een byte van het display en zet dit in de accu
;--------------------------------------------------------------------


		;BEWAAR REGISTERS EVEN
		PUSH    DPH                
		PUSH    DPL

		; WACHT OP BUSYVLAG
		MOV     DPTR,#DPRDCOM
rdchrm1:        MOVX    A,@DPTR         ;lees het commando/statusregister
		JB      ACC.7,rdchrm1   ;wacht tot het 7e bit laag is (busyvlag)

		; lees data van display
		MOV     DPTR,#DPRDDAT
		MOVX    A,@DPTR
		PUSH    ACC

		LCALL   DISPREADCOM     ;lees de cursorpositie          
		MOV     DPL,A           ;sla de accu even op in DPL
		ANL     A,#03FH         ;kijk alleen naar de laatste 6 bits
		CJNE    A,#SOORTLCD/2,readchrm2
		MOV     A,DPL           ;haal de accu terug
		ANL     A,#11000000b    ;maak de onderste 6 bits 0
		XRL     A,#11000000b    ;inverteer bit 6 en bit 7
		LCALL   DISPCOM         ;zet cursor op de goeie plek

readchrm2:      ;ZET REGISTERS TERUG
		POP     ACC
		POP     DPL
		POP     DPH
		RET
		

;--------------------------------------------------------------
DISPREADCOM:       
		;lees het adres van het display en zet dit in de accu
;--------------------------------------------------------------------

		;BEWAAR REGISTERS EVEN
		PUSH    DPH                
		PUSH    DPL

		; WACHT OP BUSYVLAG
		MOV     DPTR,#DPRDCOM
drdcom1:        MOVX    A,@DPTR         ;lees het commando/statusregister
		JB      ACC.7,drdcom1   ;wacht tot het 7e bit laag is (busyvlag)

		;in de accu staat nu de cursorpositie (?)
		; ZET REGISTERS TERUG
		POP     DPL
		POP     DPH
		RET



