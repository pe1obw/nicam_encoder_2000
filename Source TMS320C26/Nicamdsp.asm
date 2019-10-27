;-----------------------------------------------------------;
; NICAMDSP.ASM 
; 
; Nicam signal conditioning for TMS320C26 DSP kit
;
; Date : 970528 JWD initial version
;        970731 JWD 1.00 tested, seems OK now!
;        970903 JWD Test, preemphasis included.
;        971112 JWD Clipping after preemphasis added
;        980110 JWD Adapted from nicampr2.asm, added:clip indication
;	 980201 JWD Converted to TASM assembler
;	 980222 JWD Peak indicator driver added, J17 modified (+/- 0,05dB!)
;	 980627 JWD Peak indicator bits changed.
;	 981011 JWD Level adjust via #IFDEF GAIN_xxDB added. xx can be M6, 0, 6 or 12.
;-----------------------------------------------------------;



; TASM specific
	.msfirst

#define	set		EQU
#define	ps		ORG
#define	entry		msfirst
#define	conf(var)	.word 0CE3Ch|var




;Variables (in B2, 60-7F)
TEMPX           .set    060h    ; STATUS storage
STAT1           .set    061h    ; STATUS storage
ACCU_lo         .set    062h    ;
ACCU_hi         .set    063h    ;
FLAG1           .set    064h    ; semaphore
LMAX            .set    065h
RMAX            .set    066h
SIP             .set    067h
WHATBUF         .set    068h    ; 0=INBUF1 filled by interrupt, 1=INBUF2
OCNT            .set    069h	; output word counter, used by XINT and INT0 isr
INBUF           .set    06Ah    ; contains a pointer to the last read inbuf
AR6STORE        .set    06Bh
AR7STORE        .set    06Ch
CONST127        .set    06Dh    ; constant value (is faster as immediate)
FCOEF1          .set    06Eh    ; filter coefficient
FCOEF2          .set    06Fh
FCOEF3          .set    070h
Y_PREVL         .set    071h
Y_PREVR         .set    072h
X_PREVL         .set    073h
X_PREVR         .set    074h
CLIPVAL         .set    075h
LPEAK		.set	076h	; peak levels. 4400=clip, 3C00=range 7, 2C00=range 5
RPEAK		.set	077h
TMP		.set	078h	; temporarily storage, used by peak indicator
LVUREG		.set	079h
RVUREG		.set	07Ah	; temporarily storage of vu pattern


;Buffer start points
OUTBUF          .set    0600h   ; (44 bytes)
INBUF1-2        .set    067Eh   ; (1 byte)
INBUF1-1        .set    067Fh   ; (1 byte)
INBUF1          .set    0680h   ; (64 bytes)
INBUF2          .set    06C0h   ; (64 bytes)
SABUF           .set    0740h   ; (64 bytes)
SABUF+1         .set    0741h   ; ptr
MAXBUF          .set    07C0h   ; (64 bytes)
MAXBUF+1        .set    07C1h   ; assembler can't calculate

;  register usage:
;  AR0..5 are for common use by the main program
;  AR6 is output buffer pointer
;  AR7 is input buffer pointer

;----------------------------------------------------------------
;    SECONDARY VECTOR TABLE LOACTED IN B0 PROGRAM RAM
;----------------------------------------------------------------

	.include  "mmregs.asm"  ;     > USERCODE SHOULD NOT OVERWRITE DSKD  <
	.ps     0fa00h          ;     > VECTORS.  ON LOAD, INT2 IS RESTORED <
	B       start           ;RS   > BY DSKD, BUT TRAP IS NOT            <
	B       INT0            ;INT0
	B       start           ;INT1
	B       start           ;INT2  > DSKD LOAD IGNORES INT2 VECTOR
	B       start           ;TINT
	B       RINT            ;RINT  Branch to receive interrupt routine
	B       XINT            ;XINT  Branch to transmit interrupt routine
   

;----------------------------------------------------------------
;    APPLICATION CODE IS LOCATED ABOVE DSKD KERNAL
;----------------------------------------------------------------
	
	.ps     0FB00h          ;
	.entry                  ;
		
;-------------------------------------------------------------------
; Initialisation
;
;-------------------------------------------------------------------

start:  ldpk    0               ; All direct addressing is to 00 to 7F (B3)
	fort    0               ; Serial port : 16 bit
	rtxm                    ;             : ext. FSX
	sfsm                    ;             ; burst mode
	lack    080h            ; not needed ?
	sacl    GREG            ; not needed ?
	conf(2)	                ; B3=DATA 0600-07ff, B0,1=program A000-DFFF
	sovm                    ; catch accu overflows (needed for abs funct.)
	ssxm                    ; sign extended mode!
	spm     0               ; set P register shift mode to 0 (no shift)

	;This section is only for diagnostics
	lalk    0000h
	larp    AR0
	lrlk    AR0,600h
	rptk    255
	sacl    *+
	rptk    255
	sacl    *+              ; clear RAM

	
#IFDEF GAIN_M6DB
	lalk    00000h		; 8000 - 1000 / 8000 - 2000 / 8000 - 4000 / 8000 - 8000
#ENDIF
#IFDEF GAIN_0DB
	lalk    04002h		; 8000 - 1000 / 8000 - 2000 / 8000 - 4000
#ENDIF
#IFDEF GAIN_6DB
	lalk    06002h		; 8000 - 1000 / 8000 - 2000 / 8000 - 4000
#ENDIF
#IFDEF GAIN_12DB
	lalk    07002h		; 8000 - 1000 / 8000 - 2000 / 8000 - 4000
#ENDIF

	sacl    CLIPVAL         ; used for clipping after preemphasis
	lack    127
	sacl    CONST127        ; constant for CALC subroutine
	lalk    -32768
	sacl    FCOEF1
;	lalk    15949
	lalk	30000		; just a try ... 
	sacl    FCOEF2           ; preemphasis filter coefficient
	lalk    6457             ; 
	sacl    FCOEF3
	zac
	;setup for serial in isr
	sacl    WHATBUF         ; start with buffer 0 for the first 64 samples
	lrlk    AR7,INBUF1      ; inbuf pointer points to inbuf1
	lalk    INBUF2
	sacl    INBUF           ; reset inbuf pointer
	;setup for serial out isr
	lark	AR0,44
	sar	AR0,OCNT	; reset outbuf word counter
	lrlk    AR6,OUTBUF	; reset outbuf buffer pointer
	lack    035h            ; Turn on XINT and RINT and INT2,0
	sacl    IMR             ;

		
;-------------------------------------------------------------------
; Main loop start
;
;-------------------------------------------------------------------

mainloop:
	lac     FLAG1
	bz      mainloop        ; wait until a packet is xmitted

	lack    0               ; load accu with 0
	sacl    FLAG1           ; reset flag

	;************ DIAGNOSTIC PULSE ***************
;        lrlk    AR0,0FFFFh      ;
;        larp    AR0
;        rptk    2               ; read junk from address 0xFFFF
;        lac     *,0,AR0         ; to pulse BR


;-------------------------------------------------------------------
;
; Bit-interleaving.
;
; Destination buffer is outbuf (44 bytes)
; Source is inbuf1 or 2 (64 bytes)
;
; Directly after the payload packet has been transmitted a flag is set.
; The routine then starts with calculating the first 16 output bits, then
; the following 16 and so on. During the calculation, transmission
; of the first calculated data starts, but because calculation is faster
; as transmitting, no overlap occurs.
;
; Used registers: Accu, AR0,AR1,AR2,AR3,AR4,AR5
; Used data:      tempx
;
;-------------------------------------------------------------------

	lark    AR0,4           ; to add to input buffer pointer
	lrlk    AR5,OUTBUF      ; reset outbuf pointer
	lar     AR1,INBUF       ; reset input buffer pointer
	lark    AR2,10          ; reset output buffer word counter
	lark    AR4,3           ; reset output buffer word counter (AR5*4+AR2)
	larp    AR1             ; arp = input buffer ptr

intl1:  lark    AR3,7           ; reset output buffer bit counter

intl2:  lac     *,0             ;1 load word from inbuf
	ror                     ;1 shift rightmost bit in carry
	sacl    *0+             ;1 and store word to inbuf,arp=obuf word cntr
	lac     TEMPX           ;1 get output word
	rol                     ;1 shift bit in output word
	sacl    TEMPX           ;1 and store in temporalily register
	lac     *,0             ;1 load word from inbuf
	ror                     ;1 shift rightmost bit in carry
	sacl    *0+,0,AR3       ;1 and store word to inbuf,arp=obuf word cntr
	lac     TEMPX           ;1 get output word
	rol                     ;1 shift bit in output word
	sacl    TEMPX           ;1 and store in temporalily register
	banz    intl2,*-,AR1    ;2 if not all 16 bits filled do next

	larp    AR5             ; arp = output buffer pointer
	sacl    *+,0,AR1        ; and store in output buffer++.arp=inbuf ptr
	sbrk    64              ; inbufptr = inbufptr-64

	larp    AR2             ; arp=outbuf word counter
	banz    intl1,*-,AR1    ; block of 11 words written? if not jump

	lark    AR2,10          ; reset word counter A
	adrk    1               ; increment inbuf pointer
	larp    AR4             ; arp=outbuf word counter B
	banz    intl1,*-,AR1    ; branch if not finished

	;end of bit interleaving !

;-------------------------------------------------------------------
;
; Companding, parity generation and range bit coding
;
; Source/destination is inbuf (64 bytes). The samples in inbuf are
; 16 bits wide, signed. The 2 least significant bits are ignored.
;
; Used registers: Accu, AR0,AR1,AR2,AR3
; Used data:      tempx
;
;-------------------------------------------------------------------
	
	idle                    ; wait until next serial interrupt. If
				; necessary a delay loop is placed before
				; this instruction to make sure there idle
				; always waits for the same interrupt!
				; This is needed because the AD conversion                                
				; is not synchronous with the NICAM clock.

;-------------------------------------------------------------------
; Initiate aquiring 64 new samples and copy maximum values
;-------------------------------------------------------------------

	lack    1
	sub     WHATBUF
	sacl    WHATBUF         ; Whatbuf = 1-whatbuf (1 or 0)
	lrlk    AR0,INBUF2
	bz      ibuf2           ; test-which inbuf is filled ?
	lrlk    AR0,INBUF1      ; it was 1
ibuf2:  sar     AR0,INBUF       ; store ptr to last filled inbuf

;-- calculate receive buffer end
;        larp    AR0
;        adrk    63              ; input buffer's end address
;        larp    AR7
;-- wait until receive buffer (pointed to by AR7) is full (= > to AR0)
;wloop:  cmpr    2               ; check if AR7 > AR0
;        bbz     wloop

	dint                    ; disable interrupts
	lrlk    AR7,INBUF1
	bit     WHATBUF,0       ; which inbuf is selected ? -> WAS WHATBUF, 15!
	bbz     buf1            ; branch if it's buffer 1
	lrlk    AR7,INBUF2      ; INPTR points to the selected inbuf
buf1:   eint                    ; enable interrupts



#IFDEF NO_PREEMP
;-------------------------------------------------------------------
; Do no pre-emphasis filtering
;-------------------------------------------------------------------

	lar     AR1,INBUF       ; AR1 is the sample pointer
	lrlk    AR3,MAXBUF	; AR3 points to maxbuf
	lark    AR5,63          ; nr of samples-1
	larp    AR1
	bnv     pre_loop        ; clear overflow flag for clip indication
pre_loop:                       ; (arp = AR1 now)
	;copy samples as abs values to maxbuf
	lac     *+,0,AR3        ; accu = current left sample
	abs                     ; make absolute
	sacl    *+,0,AR5        ; and store in maxbuf
	banz    pre_loop,*-,AR1	
#ENDIF



#IFNDEF NO_PREEMP
;-------------------------------------------------------------------
; Do pre-emphasis filtering
;-------------------------------------------------------------------

	lar     AR1,INBUF       ; AR1 is the sample pointer
	lar     AR2,INBUF
	larp    AR2
	sbrk    2               ; AR2 points to the previous sample
	
	lac     X_PREVL         ; load last left sample of previous block
	sacl    *+              ; store in inbuf-2
	lac     X_PREVR         ; load last right sample of previous block
	sacl    *-              ; store in inbuf-1

	lrlk    AR3,MAXBUF
	lrlk    AR4,SABUF       ; AR4 points to the result buffer
	lark    AR5,31          ; nr of samples-1
	larp    AR1

	bnv     pre_loop        ; clear overflow flag for clip indication

pre_loop:                       ; (arp = AR1 now)
	;--------[ left channel ]-------------------
	lac     *,15,AR2        ; accu = current left sample * 1
	lt      *+,AR1          ; t = previous left sample
	mpy     FCOEF1          ; p = t * -1
	lta     Y_PREVL         ; t = previous left result, a = a + p
	mpy     FCOEF2          ; p = t * 0.4867337441
	lta     *+,AR4          ; t = current left sample, a = a + p
	sach    Y_PREVL         ; store left filter result
	mpy     FCOEF3          ; x0.16 or so
	apac                    ; a = a + p
	addh    CLIPVAL
	subh    CLIPVAL
	subh    CLIPVAL
	addh    CLIPVAL
#IFDEF GAIN_M6DB
	sach    *+,0,AR3        ; store left result in sample_buf
	abs                     ; make absolute
	sach    *+,0,AR1        ; and store in maxbuf
#ENDIF
#IFDEF GAIN_0DB
	sach    *+,1,AR3        ; store left result in sample_buf
	abs                     ; make absolute
	sach    *+,1,AR1        ; and store in maxbuf
#ENDIF
#IFDEF GAIN_6DB
	sach    *+,2,AR3        ; store left result in sample_buf
	abs                     ; make absolute
	sach    *+,2,AR1        ; and store in maxbuf
#ENDIF
#IFDEF GAIN_12DB
	sach    *+,3,AR3        ; store left result in sample_buf
	abs                     ; make absolute
	sach    *+,3,AR1        ; and store in maxbuf
#ENDIF
	;--------[ right channel ]-------------------
	lac     *,15,AR2        ; accu = current right sample * 1
	lt      *+,AR1          ; t = previous right sample
	mpy     FCOEF1          ; p = t * -1
	lta     Y_PREVR         ; t = previous right result, a = a + p
	mpy     FCOEF2          ; p = t * 0.4867337441
	lta     *+,AR4          ; t = current right sample, a = a + p
	sach    Y_PREVR         ; store right filter result
	mpy     FCOEF3          ; x0.16 or so
	apac                    ; a = a + p
	addh    CLIPVAL
	subh    CLIPVAL
	subh    CLIPVAL
	addh    CLIPVAL
#IFDEF GAIN_M6DB
	sach    *+,0,AR3        ; store left result in sample_buf
	abs                     ; make absolute
	sach    *+,0,AR5        ; and store in maxbuf
#ENDIF
#IFDEF GAIN_0DB
	sach    *+,1,AR3        ; store left result in sample_buf
	abs                     ; make absolute
	sach    *+,1,AR5        ; and store in maxbuf
#ENDIF
#IFDEF GAIN_6DB
	sach    *+,2,AR3        ; store left result in sample_buf
	abs                     ; make absolute
	sach    *+,2,AR5        ; and store in maxbuf
#ENDIF
#IFDEF GAIN_12DB
	sach    *+,3,AR3        ; store left result in sample_buf
	abs                     ; make absolute
	sach    *+,3,AR5        ; and store in maxbuf
#ENDIF
	banz    pre_loop,*-,AR1
	
	;---------[ store last samples ]--------------

	sbrk    2               ; sample counter - 2 (700 or 6c0->6FE,6BE)
	lac     *+              ; reload last left sample
	sacl    X_PREVL
	lac     *
	sacl    X_PREVR

	lalk    SABUF
	sacl    INBUF           ; let inbuf point to sbuf. Here are the filtered samples!
#ENDIF

;-------------------------------------------------------------------
; check for clip
;-------------------------------------------------------------------

	bnv     no_clip         ; if an overflow occurred, signal is clipped!
	lalk	04400h
	sacl	LPEAK
	sacl	RPEAK		; set overflow bits
no_clip:

;-------------------------------------------------------------------
; determine range of left channel (0..7)
;-------------------------------------------------------------------

	;arp = AR1
	lark    AR0,2           ; step size
	lrlk    AR1,MAXBUF      ; pointer to maxbuf
	lalk    01ffh           ; minimum value (7 bits used=range 0/1)
	rptk    31              ; repeat 32 times (all left channel samples)
	or      *0+             ; or with the left channel samples
	sacl    LMAX            ; store low accu

	zalh    LMAX            ; and load in high accu
	lark    AR1,9
drangel:
	sbrk    1
	rol
	bnc     drangel         ; if bit=0 decrement range
	;in AR1 is the range (1..7)
	sar     AR1,LMAX        ; store range in LMAX

;-------------------------------------------------------------------
; determine range of right channel
;-------------------------------------------------------------------

	lark    AR0,2           ; step size
	lrlk    AR1,MAXBUF+1    ; pointer to maxbuf+1 (1st right sample)
	lalk    01ffh           ; minimum value (7 bits used=range 0/1)
	rptk    31              ; repeat 32 times (all right channel samples)
	or      *0+             ; or with the right channel samples
	sacl    RMAX            ; store low accu

	zalh    RMAX
	lark    AR1,9
	larp    AR1
	ork     03ffh,15        ; make 9 LSB's 1 (lower range is not possible)
dranger:
	sbrk    1
	rol
	bnc     dranger         ; if bit=0 decrement range
	;in AR1 is the range (1..7)
	sar     AR1,RMAX        ; store range in RMAX


;-------------------------------------------------------------------------------
; Update clip indicator
;
; Indicator works as following:
; The LMAX and RMAX registers contain the range. This ranges from 0 to 7 (3 bits)
; First, the range bits are loaded in accu as following (left shifted 11 positions):
; 00rr r000 0000 0000
; then bit 10 is set high to 'add +0,5'.
; 00rr r100 0000 0000
; This value is compared to the value in the lpeak register. If larger, the
; lpeak register is overwritten by the new value.
; If the lpeak register is larger, the value in the lpeak register is decremented
; by 15.
; If a clip occurred, a value of 0110 0100 0000 0000 is written to the lpeak
; register to light all the LEDs. (this equals range 8 - 6 dB above maximum)
;
; Then the value of the LPEAK register is read and shifted left 5 bits, the high
; accu looks like this:
; 0000 0000 0000 0rrr
; This value is used to read the LED pattern from a table.
;-------------------------------------------------------------------------------

	;see if lmax is higher as previous lmax
	lac	LMAX,11		; load accu with l range (0..7)
	ork	0400h		; add 0.5 (sorry, can't explain...)
	sub	LPEAK		; subtract current peak value
	bnc	lnoupd		; if carry = 0, lpeak was higher as lmax
	lac	LMAX,11
	ork	0400h		; add 0.5 (sorry, can't explain...)
	b	lcont
lnoupd:	lac	LPEAK
	subk	15
	bc	lcont		; if carry=set, lpeak is < 0
	lack	0
lcont:	sacl	LPEAK

	;see if rmax is higher as previous rpeak
	lac	RMAX,11		; load accu with l range (0..7)
	ork	0400h		; add 0.5 (sorry, can't explain...)
	sub	RPEAK		; subtract current peak value
	bnc	rnoupd		; if carry = 0, lpeak was higher as lmax
	lac	RMAX,11
	ork	0400h		; add 0.5 (sorry, can't explain...)
	b	rcont
rnoupd:	lac	RPEAK
	subk	15
	bc	rcont		; if carry=set, lpeak is < 0
	lack	0
rcont:	sacl	RPEAK

	;update LED register
	lac     LPEAK,5         ; load peak, high accu bit0..2 contain 0..7
	sach	TMP
	lac	TMP
	adlk    LVUTAB          ; add base address of vutable
	tblr    LVUREG          ; store pattern in VU register

	lac     RPEAK,5         ; load peak, high accu bit0..2 contain 0..7
	sach	TMP
	lac	TMP
	adlk    RVUTAB          ; add base address of vutable
	tblr    RVUREG          ; store pattern in VU register

        lrlk    AR0,0FFFFh      ;
        larp    AR0
	lac	LVUREG
	or	RVUREG		
	sacl	*


;-------------------------------------------------------------------
; Build payload block
;-------------------------------------------------------------------


	;--------------------------------------
	; Start with left channel...
	;--------------------------------------
	lalk    1,14            ; accu = 2^14
	lar     AR0,LMAX
	larp    AR0
	sbrk    1               ; ar0 is now 0..6.
	banz    calc1,*-        ; if not 0 jump
	b       calc4           ; if 0, MAX was 1 and Accu stays 2^14
calc1:  banz    calc3,*-
	b       calc4           ; if 0 now, MAX was 2 and accu stays 2^14

calc2:  sfr
calc3:  banz    calc2,*-

calc4:  ; in accu is now 2^14 (range=3) .. 2^10 (if range=7)
	sacl    TEMPX
	lt      TEMPX           ; T register for shifting

	lac     LMAX            ; load scale factor
	adlk    SCALETAB        ; add base address of scaletable
	tblr    LMAX            ; read right factor!

	lark    AR0,6           ; step size
	larp    AR1

	;--------------------------------------
	; samples 0,6,12,18,24,30,36,42,48
	;--------------------------------------
	lac     LMAX,8          ; bit 10=rangebit 2L 

	andk    0400h           ; mask bit 10
	sacl    SIP             ; store in SIP register
	lark    AR2,8
	lar     AR1,INBUF
	call    CALC
	;--------------------------------------
	; samples 2,8,14,20,26,32,38,44,50
	;--------------------------------------
	lac     LMAX,9          ; bit 10=rangebit 1L
	andk    0400h           ; mask bit 10
	sacl    SIP             ; store in SIP register
	lark    AR2,8
	lar     AR1,INBUF
	adrk    2               ; inbuf + 2 for samples 2,8 enz.
	call    CALC
	;--------------------------------------
	; samples 4,10,16,22,28,34,40,46,52
	;--------------------------------------
	lac     LMAX,10         ; bit 10=rangebit 0L
	andk    0400h           ; mask bit 10
	sacl    SIP             ; store in SIP register
	lark    AR2,8
	lar     AR1,INBUF
	adrk    4               ; inbuf + 4 for samples 4,10 enz.
	call    CALC
	;--------------------------------------
	; samples 54,56,58,60,62
	;--------------------------------------
	zac
	sacl    SIP             ; store 0 in SIP register (cb0/1=0)
	lark    AR2,4
	lark    AR0,2           ; step size is 2 now (54+2=56 ...)
	lar     AR1,INBUF
	adrk    54              ; inbuf + 54 for samples 54,56 enz.
	call    CALC
		

	;--------------------------------------
	; Right channel
	;--------------------------------------
	lalk    1,14            ; accu = 2^14
	lar     AR0,RMAX
	larp    AR0
	sbrk    1               ; ar0 is now 0..6.
	banz    calc5,*-        ; if not 0 jump
	b       calc8           ; if 0, MAX was 1 and Accu stays 2^14
calc5:  banz    calc7,*-
	b       calc8           ; if 0 now, MAX was 2 and accu stays 2^14

calc6:  sfr
calc7:  banz    calc6,*-

calc8:  ; in accu is now 2^14 (range=3) .. 2^10 (if range=7)
	sacl    TEMPX
	lt      TEMPX           ; T register for shifting

	lac     RMAX            ; load scale factor
	adlk    SCALETAB        ; add base address of scaletable
	tblr    RMAX            ; read right factor!

	lark    AR0,6           ; step size
	larp    AR1

	;--------------------------------------
	; samples 1,7,13,19,25,31,37,43,49
	;--------------------------------------
	lac     RMAX,8          ; bit 10=rangebit 2R
	andk    0400h           ; mask bit 10
	sacl    SIP             ; store in SIP register
	lark    AR2,8
	lar     AR1,INBUF
	adrk    1               ; inbuf + 1 for samples 1,7 enz
	call    CALC
	;--------------------------------------
	; samples 3,9,15,21,27,33,39,45,51
	;--------------------------------------
	lac     RMAX,9          ; bit 10=rangebit 1L
	andk    0400h           ; mask bit 10
	sacl    SIP             ; store in SIP register
	lark    AR2,8
	lar     AR1,INBUF
	adrk    3               ; inbuf + 3 for samples 3,9 enz.
	call    CALC
	;--------------------------------------
	; samples 5,11,17,23,29,35,41,47,53
	;--------------------------------------
	lac     RMAX,10         ; bit 10=rangebit 0L
	andk    0400h           ; mask bit 10
	sacl    SIP             ; store in SIP register
	lark    AR2,8
	lar     AR1,INBUF
	adrk    5               ; inbuf + 4 for samples 5,11 enz.
	call    CALC
	;--------------------------------------
	; samples 55,57,59,61,63
	;--------------------------------------
	zac
	sacl    SIP             ; store 0 in SIP register (cb0/1=0)
	lark    AR2,4
	lark    AR0,2           ; step size is 2 now
	lar     AR1,INBUF
	adrk    55              ; inbuf + 54 for samples 55,57 enz.
	call    CALC

	;--------[end of calculating]----------------------------------------

;        ;************ DIAGNOSTIC PULSE ***************
;        lrlk    AR0,0FFFFh      ;
;        larp    AR0
;        rptk    2               ; read junk from address 0xFFFF
;        lac     *,0,AR0         ; to pulse BR
;
	b       mainloop



;----------------------------------------------------------------------------
; CALC
; 
; Shift the samples, calculate the parity bit and xor with range bit.
; AR1 is the input buffer pointer
; AR2 counts the number of bytes
; AR0 contains the step size
; T contains the sample multiplier (to shift-optionally volume control!)
; SIP contains the signalling in parity bit
;
; uses A,T,P
;
; cycle time (including call): 14 cycles
;----------------------------------------------------------------------------

CALC:   mpyu    *               ; P=T*(AR1) - in P is now the shifted sample
	sph     *               ; store result
	lac     *,12            ; load shifted sample bit 4..9 in high accu
	sach    *               ; and store in memlocation
	lac     *               ; and reload. In low accu is bit 4..9 of sample
	and     CONST127        ; remove unused bits (if sample was negative)
	adlk    PARTAB          ; add parity table address
	tblr    *               ; store parity bit
	zalh    SIP             ; load signal. in parity bit (bit 10,=0/1)
	addh    *               ; add parity bit
	apac                    ; add sample
	sach    *0+,0,AR2       ; store result and inc inbufptr with AR0
	banz    CALC,*-,AR1     ; all samples done?
	ret                     ; end of subroutine!    


;-----------------------------------------------------------------
; Interrupt 0 routine
;
; uses: AR6
;
; This routine is called when the LCA gives an interrupt (on bitcnt 7).
; Then the output word counter is set to 43 (word 1).
; This synchronises the serial output data stream with the nicam
; clock.
;
;-----------------------------------------------------------------

INT0:   sst1    STAT1           ;Recover ARP from ARB by LST1 last
	sar	AR7,AR7STORE	;save AR7	
	lark    AR7,43          
	sar	AR7,OCNT	;set word counter to word 1.
	lar	AR7,AR7STORE	;restore AR7
	lst1    STAT1           ;reload status and ARP
	eint                    ;enable for next interrupt
	ret                     ;


;-----------------------------------------------------------------
; serial interrupt transmit routine
;
; uses:    AR6             (registers)
;          STAT1,OCNT      (data memory)
;  	   AR7STORE
; updates: DXR, OCNT, AR6
;
; AR6 is for the serial xmit routine only and contains the pointer
; to the output buffer.
; The original value in AR7 is stored, then AR7 serves as a temp.
; storage register and then holds the output word count. After updating
;-----------------------------------------------------------------

XINT:   sst1    STAT1           ;Recover ARP from ARB by LST1 last
	sar	AR7,AR7STORE	;save AR7 value
	larp	AR6
	lar	AR7,*+,AR7	;load new value in AR7, increment AR6 (obufptr), arp=AR7
	sar	AR7,DXR		;write value in AR7 to DX register
	lar	AR7,OCNT	;load word counter in AR7
	banz	noend,*-	;compare with 0 and decrement, jump is not 0
	lark	AR7,44		;reset output word counter
	lrlk	AR6,OUTBUF	;reset outbuf pointer
	sar	AR7,FLAG1	;set flag to indicate end of packet transmit

noend:	sar	AR7,OCNT	;write ar7 back to output word count register
	lar	AR7,AR7STORE	;restore AR7
	lst1    STAT1           ;reload status and ARP
	eint                    ;enable for next interrupt
	ret			;


;-----------------------------------------------------------------
; serial interrupt receive routine
;
;       STAT1, AR6/7STORE      (data memory)  
;
; The routine reads the sample from the serial port and writes it
; to the buffer.
;-----------------------------------------------------------------

RINT:   sst1    STAT1           ; Recover ARP from ARB by LST1 last
	sar     AR6,AR6STORE    ; store AR6
	
	larp    AR7
	lar     AR6,DRR         ; get sample
	sar     AR6,*+          ; and store in buffer

	lar     AR6,AR6STORE    ; reload AR6
	lst1    STAT1           ; reload status and ARP
	eint                    ; enable for next interrupt
	ret                     ;



;******************************************************************

SCALETAB:                       ; don't ask me why, but
	.word   0               ; the xmitted scale factor
	.word   1               ; differs in the calculated
	.word   2               ; scale factor: 3 and 4 are exchanged.
	.word   4               ; this table is used to swap them
	.word   3
	.word   5
	.word   6
	.word   7


;---------------------
; Peak indicator: D0,2,4,6 = 0,-6,-12,-18 dB left channel
;                 D1,3,5,7 = 0,-6,-12,-18 dB right channel

LVUTAB:
	.word	00000000b	; 0
	.word	00000000b
	.word	00000000b
	.word	00000000b
	.word	00000000b
	.word	10000000b	; 5
	.word	10100000b
	.word	10101000b	; 7
	.word	10101010b	; clip
	.word	10101010b	; clip
	.word	10101010b	; clip

RVUTAB:
	.word	00000000b	; 0
	.word	00000000b
	.word	00000000b
	.word	00000000b
	.word	00000000b
	.word	01000000b	; 5
	.word	01010000b
	.word	01010100b	; 7
	.word	01010101b	; clip
	.word	01010101b	; clip
	.word	01010101b	; clip

	.include "nicamtab.bin"

	.word	0

	.end

;******************************************************************


