;////[ IIC.H ]/////////////////////////////////////////////////////////////////
;  Routines om IIC IC's aan te sturen.
;  Geschreven door Willem, Werner, Gerbert, Bert enz. enz. enz.
;  Dit zijn DE routines, betere zijn er niet !
;  Laatste datum: 10-2-1996
;
;  Als je deze routines gebruikt wordt voor SDA en SCL P1.5 en P1.6 genomen.
;  Als je andere pootjes wilt gebruiken moet dat in het hoofdprogramma 
;  aangegeven worden:
;  #DEFINE  SDA  095H    ; P1.5 is SDA
;  #DEFINE  SCL  096H    ; P1.6 is SCL
;
;  IIC.H bevat de volgende routines:
;
;  IIC_START:   Genereert een startconditie op de IIC-bus. Carry is clear 
;               als het goed is en set bij fouten (sda/scl niet hoog,
;               bv door kortsluiting).
;
;  IIC_STOP:    Genereert een stopconditie op de IIC-bus.
;
;  IIC_WRITE:   Schrijft het byte in de accu naar de IIC-bus en controleert
;               de acknowledge. Als de acknowledge klopt is de carry gecleared
;               en bij fouten is ie geset.
;
;  IIC_READ:    Lees een byte in via de IIC-bus, ZONDER een acknowledge te geven
;               Het byte komt terug in de accu.
;
;  IIC_ACK:     Geeft een acknowledge. Aanroepen na een IICREAD opdracht.
;
;  IIC_NOACK:   Geeft een negatieve acknowledge (SDA hoog). Aanroepen na een
;               IICREAD opdracht.
;////////////////////////////////////////////////////////////////////////////


#IFNDEF SDA
  #DEFINE SDA 095H
#ENDIF

#IFNDEF SCL
  #DEFINE SCL 096H
#ENDIF



;----------------------------------------------------------------
IIC_WRITE:      ;schrijf het byte in de accu naar de IIC poort en
		;check op een acknowledge.
		;Gebruikte registers : geen.
		;de routine gaat ervan uit dat SCL hoog is bij de aanroep.
		;SDA maakt niet uit. Bij het verlaten is SCL hoog en SDA 
		;laag (acknowledge van ontvanger).
		;Als de acknowledge fout was is de carry GESET, anders
		;is de carry gecleared.
;----------------------------------------------------------------

		PUSH    B               ;B even op de stack
		MOV     B,#8            ;teller voor aantal bits
wriicm3:        CLR     SCL             ;maak de kloklijn laag
		RLC     A               ;roteer de accu (met daarin de data) door de carry.
		JC      wriicm1         ;spring als het een 1 is.
		CLR     SDA             ;maak het SDA-bit laag (0)
		SJMP    wriicm2
wriicm1:        SETB    SDA             ;maak SDA hoog (1)
wriicm2:        LCALL   WACHT           ;klok moet minimaal 4,7 usec laag zijn
		SETB    SCL             ;klok hoog
		LCALL   WACHT           ;klok moet minimaal 4 usec hoog
		DJNZ    B,wriicm3       ;alle bits al gehad ?
		CLR     SCL             ;klok laag
		SETB    SDA             ;sda hoog
		LCALL   WACHT           ;wacht minimaal 3,7 us tot IC z'n ack. klaar heeft.
		SETB    SCL             ;klok weer hoog         
		CLR     C
		JNB     SDA,noerror     ;spring als er een acknowledge is (ack=0)
		SETB    C               ;foutmelding -> geen acknowledge!
noerror:        LCALL   WACHT           ;klok moet minimaal 4,7 usec hoog zijn!
		POP     B
		RET


;----------------------------------------------------------------
IIC_READ:       ;lees een byte in in de accu en verstuur een acknowledge.
		;Gebruikte registers : R0
		;de routine gaat ervan uit dat SCL hoog is bij de aanroep.
		;SDA maakt niet uit.
		;Bij het verlaten is SCL hoog en SDA laag (acknowledge)
;----------------------------------------------------------------

		PUSH	B				
		MOV     B,#8            ;teller voor aantal bits
		MOV     A,#0
		CLR     SCL             ;altijd eerst SCL laag voor je iets met SDA doet.
		SETB    SDA             ;voor de zekerheid (moest al gebeurd zijn)

rdiicm2:        CLR     SCL             ;maak de kloklijn laag
		LCALL   WACHT           ;geef device gelegenheid data op SDA te zetten (min 4,7 us)
		SETB    SCL             ;klok hoog
		LCALL   WACHT           ;wacht even
		SETB    C
		JB      SDA,rdiicm1     ;spring als SDA hoog is
		CLR     C
rdiicm1:        RLC     A               ;schuif bit in de accu
		DJNZ    B,rdiicm2       ;alle bits al gehad ?

		CLR     SCL             ;klok laag
		NOP
		NOP                     ;even wat tijd rekken om een data-hold tijd van enkele uS te maken
		POP	B
		RET

;------------------------------------------------------------------
IIC_START:      ;genereert een startconditie op de IIC bus
;------------------------------------------------------------------
		JNB     SDA,iicstm1
		JNB     SCL,iicstm1     ;spring als SDA of SCL niet hoog zijn
		; sda en scl zijn beide hoog.
		LCALL   WACHT           ;voor een start moet klok minimaal 4 usec hoog zijn
		CLR     SDA             ;SDA laag terwijl SCL hoog is
					; -> dit is de startconditie
		LCALL   WACHT           ; ik mag nu 4 usec niets doen.
		CLR     C
		RET

iicstm1:        SETB    C               ;fout : SDA/SCL error (kortsluiting?)
		RET


;--------------------------------------------------------------------
IIC_STOP:       ;genereert een stop-conditie. Er wordt uitgegaan van
		;een datalijn die hoog is en een klok die hoog is.
;--------------------------------------------------------------------

		CLR     SCL                             
		CLR     SDA             ;data en klok laag
		LCALL   WACHT
		SETB    SCL             ;klok hoog
		LCALL   WACHT           ;wacht minimaal 4,7 usec
		SETB    SDA             ;data hoog (verandering van data tijdens klok = start/stop)
		LCALL   WACHT           ;wacht minimaal 4 usec
		RET

;----------------------------------------------------------------------
IIC_ACK:        
		;geef een acknowledge. (SDA laag maken)
;-----------------------------------------------------------------------

		SETB    SDA             ;sda hoog (neg. acknowledge, maar moet bij eeprom)
		LCALL   WACHT
		SETB    SCL             ;klok ook hoog
		LCALL   WACHT
		RET



;----------------------------------------------------------------------
IIC_NOACK:      
		;geef een negatieve acknowledge. (SDA hoog maken)
;-----------------------------------------------------------------------

		SETB    SDA             ;sda hoog (neg. acknowledge, maar moet bij eeprom)
		LCALL   WACHT
		SETB    SCL             ;klok ook hoog
		LCALL   WACHT
		RET




;--------------------------------------------------------------------
WACHT:          ;wacht enkele uS.
		;wordt gebruikt door alle IIC routines !
;--------------------------------------------------------------------

;               PUSH    A
;               MOV     A,#5
;wachtm1:       DJNZ    A,wachtm1       ;tel af.;
;               POP     A

		RET


