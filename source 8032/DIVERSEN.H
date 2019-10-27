;////[ DIVERSEN.H ]/////////////////////////////////////////////////////////////////
;  Gezellige routines voor leuke dingesen
;  Geschreven door Willem, Werner, Gerbert, Bert enz. enz. enz.
;  Dit zijn DE routines, betere zijn er niet !
;  Laatste datum: 19-2-1996
;
;  DIVERSEN.H bevat de volgende routines:
;
;  DELAY      : wacht een aantal milliseconden. Het aantal milliseconden moet
;		in de datapointer staan. (bij een xtal van 12 MHz)
;
;////////////////////////////////////////////////////////////////////////////



;-----------------------------------------------------------------------------
DELAY:
	        ;wacht een aantal milliseconden. Het aantal milliseconden moet
		;in de datapointer staan.
		;gebruikte registers : DPTR
;-----------------------------------------------------------------------------

		PUSH	ACC
		INC	DPH
delaym2:	MOV	A,#250
delaym1:	NOP			;1 us
		NOP			;1 us
		DJNZ	ACC,delaym1	;2 us
		DJNZ	DPL,delaym2
		DJNZ	DPH,delaym2
		POP	ACC
		RET
