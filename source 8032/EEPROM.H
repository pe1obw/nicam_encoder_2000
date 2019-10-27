;//////[ EEPROM.H ]//////////////////////////////////////////////////////////////////////
;
;  Routines om EEPROM's uit te lezen en te beschrijven. 
;  Getest met de SDA2526.
;  Werkt niet met de SDA2546.
; 
;  Geschreven door Willem, Werner, Gerbert, Bert enz. enz. enz.
;  Laatste datum : 6-2-1996
;
;  Bij EEPROM.H is ook IIC.H nodig. Bovendien moet in het hoofdprogramma het 
;  slave-adres van de EEPROM worden gegeven, bijvoorbeeld zo:
;  #DEFINE  EEPRWR	10100000b	;adres van de EEPROM (schrijven)
;  #DEFINE  EEPRRD	10100001b	;adres van de EEPROM (lezen)
;  Als geen adres wordt gegeven, wordt 10100000b gebruikt (alle adrespootjes
;  aan massa).
;
;  Om de eeprom van het begin af goed te laten werken moet men eerst een
;  keer iets gelezen hebben.
;  Voordat je wilt schrijven moet men eerst altijd lezen.
;
;  EEPROM.H library bevat de volgende routines:
;
;  READEEPROM:  Lees een byte uit de EEPROM. Het adres dat je wil lezen moet in
;		B staan en de gelezen data komt in A terug.
;
;  WREEPROM:	Schrijf een byte naar de EEPROM. Het adres waar je naar toe wil
;		schrijven moet in B staan, de data die er in moet in A.
;
;////////////////////////////////////////////////////////////////////////////

#IFNDEF EEPRWR
  #DEFINE    EEPRWR      10100000b
#ENDIF

#IFNDEF EEPRRD
  #DEFINE EEPRRD 10100001b
#ENDIF



;----------------------------------------------------------------
READEEPROM:
		;lees een byte uit de eeprom en zet dit in de accu
		;het adres dat gelezen moet worden moet in B staan.
		;B wordt niet aangetast
		;
		;LET OP ! Na het inschakelen moet eerst gelezen worden
		;voordat je mag schrijven!
		;
	       	;Gebruikte registers: accu, B
;-----------------------------------------------------------------

		LCALL   IIC_START
		MOV     A,#EEPRWR
		LCALL   IIC_WRITE	;schrijf slave adres
		MOV     A,B             ;in B geef ik het woordadres mee
		LCALL   IIC_WRITE	;schrijf woord adres

		;klok is hoog, data is laag (acknowledge EEPROM)
		CLR     SCL		;maak klok laag
		LCALL   WACHT		;wacht minimaal 4usec.
		SETB    SCL		;maak klok weer hoog.

		LCALL   IIC_START       ;opnieuw startconditie
		MOV     A,#EEPRRD
		LCALL   IIC_WRITE	;slave adres schrijven
		LCALL   IIC_READ        ;lees eerste byte(lage deel PLL-deeltal) 
		LCALL	IIC_NOACK	;geef een negatieve acknowledge
		LCALL   IIC_STOP
		RET




;-------------------------------------------------------------
WREEPROM:       ;Schrijf het byte in de accu naar de EEPROM 
		;adres moet in B staan
		;De accu wordt aangetast.
;--------------------------------------------------------------
		
		PUSH	ACC		;zet accu, met daarin de te schrijven
					;data, even weg
		LCALL   IIC_START
		MOV     A,#EEPRWR
     		LCALL   IIC_WRITE	;schrijf slave-adres
        	MOV     A,B
		LCALL   IIC_WRITE	;schrijf woord-adres
		POP	ACC		;haal accu met de data weer terug
		LCALL   IIC_WRITE       ;schrijf data naar eeprom
		LCALL   IIC_STOP

		;wacht 40 ms om het schrijven af te maken
		PUSH	B
		MOV	A,#40		;aantal milliseconden (minimaal 30 volgens Snor)
wrepm9:		MOV	B,#249
wrepm8:		NOP			; 1 usec
		NOP			; 1 usec
		DJNZ	B,wrepm8	; 2 usec
		DJNZ	ACC,wrepm9
		POP	B
		RET

;////////////////////////////////////////////////////////////////////////////
;//////// vanaf hier zou moeten werken bij 24C02 en PCF 8582 ////////////////
;////////////////////////////////////////////////////////////////////////////
		;ga kijken of het IC al klaar is met schrijven
		PUSH	B		;bewaar B
		MOV	B,#100		;probeer het 100 keer maximaal
wrepm3:		LCALL	IIC_START
		MOV	A,#EEPRRD
		LCALL	IIC_WRITE	;schrijf slave adres (read mode)
		JNC	wrepm4		;klaar !
		LCALL	IIC_STOP
		DJNZ	B,wrepm3
		POP	B		;als hij hier komt is het IC stuk
		RET			;(meer dan 100x geprobeerd)

wrepm4:		LCALL	IIC_STOP
		POP	B		;B weer terug
		RET			;klaar !

