;------------------------------------------------------------------
; SERIAL4.ASM
;
; Serial handling for nicam AD communications
;
; Update : 980704 Werner, initial version
;
;
; NOTE: The TX buffer ranges from 0x70 to 0x7F
;	The RX buffer ranges from 0x60 to 0x6F
;	Pointers SO_WP, SO_RP, SI_WP and SI_RP must be declared
;	TX_ENA must be declared (high active port pin)
;
; Serial routines for the interface from RS232 to RS485.
;
; All received characters are stored in the receive buffer.
;
;------------------------------------------------------------------

;#define BAUDRATE    256-3                  ; 19200 bd on 11.0592 MHz
#define BAUDRATE    256-6                  ; 9k6 bd on 11.0592 MHz



;==================================================================
; INITSERIAL:
; 
; initialize serial port for baudrate
;==================================================================
INITSERIAL:
		
	MOV	SO_WP, #70H
	MOV	SO_RP, #70H
	MOV	SI_WP, #60H
	MOV	SI_RP, #60H		; clear pointers

	CLR	TX_ENA			; clear txena flag
	MOV     PCON,#080H
	ANL	TMOD, #00001111b	; change to 16-bit timer (timer0 mode1)
	ORL	TMOD, #00100000b
	MOV     SCON,#052H      	; mode 1 , Enable receiver=10H
	MOV     TL1,#BAUDRATE
    	MOV     TH1,#BAUDRATE
    	SETB    TCON.6          	; start counter
	SETB	IEC.4			; enable serial interrupt
	SETB	IPC.4
	SETB	IEC.7			; global interrupt enable
    	RET


;==================================================================
; SER_INT:
; 
; Serial interrupt routine. Transmits characters if any in the buffer
; and receive data.
;==================================================================

SER_INT:
	PUSH	PSW
	PUSH	ACC
	PUSH	0
 
	JNB	TI, ser_txend		; transmit interrupt ? If not jump
	CLR	TI
	MOV	A, SO_RP
	CJNE	A, SO_WP, txb_notemp	; if not equal, tx buffer is not empty
	CLR	TX_ENA			; clear txena bit (transmitter is off now)
	SJMP	ser_txend
txb_notemp:
	SETB	TX_ENA			; turnon txena bit, transmitter is on now
	MOV	R0, SO_RP
	MOV	SBUF, @R0		; SBUF = *so_rp
	MOV	A, SO_RP
	ADD	A, #1
	ORL	A, #070H		; so_rp = (so_rp + 1) | 0x70
	ANL	A, #07FH
	MOV	SO_RP, A

ser_txend:
	JNB	RI,nint_rx
	CLR	RI
	MOV	R0, SI_WP
	MOV	@R0, SBUF		; *si_wp = SBUF
	MOV	A, SI_WP
	ADD	A, #1
	ORL	A, #070H
	ANL	A, #06FH
	MOV	SI_WP, A		; si_wp = ((si_wp + 1) | 0x70 ) & 0x6f

nint_rx:
	POP	0
	POP	ACC
	POP	PSW
	RETI


;==================================================================
; PUTCH
; 
; transmits the character in A.
; Use: A
;==================================================================

PUTCH:
	PUSH	0
	PUSH	ACC
tbuf_wait:
	MOV	A, SO_WP
	ADD	A, #1
	ORL	A, #070H
	CJNE	A, SO_RP, tbuf_free
	SJMP	tbuf_wait		; if txbuf not free wait
tbuf_free:
	MOV	R0, SO_WP
	POP	ACC
	MOV	@R0, A			; *(so_wp+1) = A
	MOV	A, SO_WP
	ADD	A, #1
	ORL	A, #070H
	ANL	A, #07FH
	MOV	SO_WP, A
	POP	0
	JB	TX_ENA, putch_end
	SETB	TI			; if transmitter not active, set interrupt flag
putch_end:
	RET


;==================================================================
; GETCH
; 
; Reads a character from the RS232 port.
; If no character in buffer wait.
; Return the character in A.
; Use: A
;==================================================================

GETCH:
	PUSH	0
rbuf_wait:
	MOV	A, SI_RP
	CJNE	A, SI_WP, rbuf_ok
	SJMP	rbuf_wait		; wait until something in buffer
rbuf_ok:
	MOV	R0, A
	MOV	A, @R0
	PUSH	ACC
	MOV	A, SI_RP
	ADD	A, #1
	ORL	A, #070H
	ANL	A, #06FH
	MOV	SI_RP, A
	POP	ACC
	POP	0
	RET


;==================================================================
; KBHIT
; 
; Checks if a character is in the buffer. If so, the routine
; returns with the carry set.
;
; Use: A,C
;==================================================================

KBHIT:
	MOV	A, SI_RP
	CJNE	A, SI_WP, rbuf_filled
	CLR	C
	RET
rbuf_filled:
	SETB	C
	RET

	.end

