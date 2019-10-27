;============================================================================
; Project:      Nicam coder
;
; File:         Menu.asm
; 
; Date:         981010 JWD initial version
;               000102 JWD 1khz beep added, version nr increased
;
; Key handlers and menu functions for Nicam coder.
;============================================================================



;----------------------------------------------------------------------------
; KEY_TASK
;
; Handle keys
;----------------------------------------------------------------------------

KEY_TASK:
		JNB     nKEY1, KEY_MENU
		JNB     nKEY2, KEY_DOWN
		JNB     nKEY3, KEY_UP
		JBC     TO_FLAG, store          ; If timeoutflag is set, it's time to store the settings
		JBC     TO_FLAG2, quitmenu
		RET

store:          SETB    LED
		LCALL   WRITE_SETTINGS
		CLR     LED
		RET

quitmenu:       MOV     MENU_NR, #0             ; show the welcome text after 1 minute of no keypresses
		MOV     R0, #0
		LCALL   SHOW_MENU
		RET


;----------------------------------------------------------------------------
; KEY_XX:
;
; Called if a key is pressed.
;----------------------------------------------------------------------------

KEY_MENU:
		MOV     A, MENU_NR              ; check menu item nr
		JNZ     next_item               ; if not in the menu
		MOV     MENU_NR, LAST_NR        ; goto last used item
		SJMP    last_item
next_item:      INC     MENU_NR                 ; else increment itemnr
last_item:      MOV     A, MENU_NR
		CJNE    A, #9, menu1
		MOV     MENU_NR, #1
menu1:          MOV     R0, #0
		LCALL   SHOW_MENU
		MOV     LAST_NR, MENU_NR        ; Store last menu item
		SJMP    debounce

KEY_UP:         MOV     R0, #1                          ; increment value
		LCALL   SHOW_MENU;
		SJMP    debounce

KEY_DOWN:       MOV     R0, #-1                         ; decrement value
		LCALL   SHOW_MENU

debounce:       MOV     DPTR, #25
		LCALL   DELAY
		JNB     nKEY1, debounce
		JNB     nKEY2, debounce
		JNB     nKEY3, debounce
		MOV     TO_CNTLO, #255
		MOV     TO_CNTHI, #4                    ; +- 1 minute timeout until menu quits
;       CJNE    R0, #0, changes                 ; see if a change is made in the settings
;       RET
changes:        MOV     SLOW_TIM, #180                  ; store after 6,5 seconds
		RET


;----------------------------------------------------------------------------
; SHOW_MENU:
;
; Show menu text in display. The value in R0 is added to the selected item.
; If R0 != 0 then the configuration is updated
;----------------------------------------------------------------------------

SHOW_MENU:      MOV     A, MENU_NR
		JNZ     not_m0
		MOV     DPTR, #welc_txt
		LCALL   MTXT
		RET

not_m0:         CJNE    A, #1, not_m1                   ; Item 1 = input
		MOV     A, INPUT
		MOV     R1, #2
		LCALL   ADD_A
		MOV     INPUT, A                
		MOV     DPTR, #input_txt
		LCALL   MTXT                            ; show (new) value
		CJNE    R0, #0, m0c                     ; check if changed
		RET
m0c:            LCALL   SET_IN_OUTPUT                   ; if changed, update configuration
		RET

not_m1:         CJNE    A, #2, not_m2                   ; Item 2 = output
		MOV     A, OUTPUT
		MOV     R1, #3
		LCALL   ADD_A
		MOV     OUTPUT, A
		MOV     DPTR, #output_txt
		LCALL   MTXT                            ; show (new) value
		CJNE    R0, #0, m1c                     ; check if changed
		RET
m1c:            LCALL   SET_IN_OUTPUT                   ; if changed, update configuration
		RET

not_m2:         CJNE    A, #3, not_m3                   ; Item 3 = input level
shw_inlvl:      MOV     A, INPUT_LVL
		MOV     R1, #4
		LCALL   ADD_A
		MOV     INPUT_LVL, A
		MOV     DPTR, #inlvl_txt
		LCALL   MTXT                            ; show (new) value
		CJNE    R0, #0, m2c                     ; check if changed
		RET
m2c:            MOV     DPTR, #wait_txt                 ; show wait-text
		MOV     A, #0
		LCALL   MTXT
		LCALL   SET_DSP                         ; if changed, update configuration
		MOV     A, INPUT_LVL
		MOV     DPTR, #inlvl_txt
		LCALL   MTXT                            ; and show again the input level
		RET

not_m3:         CJNE    A, #4, not_m4                   ; Item 4 = preemphasis on/off
		MOV     A, PREEMP
		MOV     R1, #2
		LCALL   ADD_A
		MOV     PREEMP, A
		MOV     DPTR, #preemp_txt
		LCALL   MTXT                            ; show (new) value
		CJNE    R0, #0, m3c                     ; check if changed
		RET
m3c:            MOV     DPTR, #wait_txt                 ; show wait-text
		MOV     A, #0
		LCALL   MTXT
		LCALL   SET_DSP                         ; if changed, update configuration
		MOV     A, PREEMP
		MOV     DPTR, #preemp_txt
		LCALL   MTXT                            ; and show again the preemp state
		RET

not_m4:         CJNE    A, #5, not_m5                   ; Item 5 = Reserve sound switch (C4 bit)
		MOV     A, C4BIT
		MOV     R1, #2
		LCALL   ADD_A
		MOV     C4BIT, A
		MOV     DPTR, #c4bit_txt
		LCALL   MTXT                            ; show (new) value
		CJNE    R0, #0, m4c                     ; check if changed
		RET
m4c:            LCALL   SET_CBITS                       ; if changed, update configuration
		RET

not_m5:         CJNE    A, #6, not_m6                   ; Item 6 = Nicam mode
		MOV     A, NICMODE
		MOV     R1, #4
		LCALL   ADD_A
		MOV     NICMODE, A
		MOV     DPTR, #nicmode_txt
		LCALL   MTXT                            ; show (new) value
		CJNE    R0, #0, m5c                     ; check if changed
		RET
m5c:            LCALL   SET_CBITS                       ; if changed, update configuration
		RET

not_m6:         CJNE    A, #7, not_m7                   ; Item 7 = Test functions
		MOV     A, NICTEST
		MOV     R1, #4
		LCALL   ADD_A
		MOV     NICTEST, A
		MOV     DPTR, #nictest_txt
		LCALL   MTXT                            ; show (new) value
		CJNE    R0, #0, m6c                     ; check if changed
		RET
m6c:            MOV     DPTR, #wait_txt                 ; show wait-text
		MOV     A, #0
		LCALL   MTXT
		LCALL   SET_DSP                         ; if changed, update configuration
		MOV     A, NICTEST
		MOV     DPTR, #nictest_txt
		LCALL   MTXT                            ; and show again the test state
		RET

not_m7:         CJNE    A, #8, not_valid                ; Item 8 = scrambling off/1/2
		MOV     A, SCRAMBLE
		MOV     R1, #3				; off/1/2 (3 choices)
		LCALL   ADD_A
		MOV     SCRAMBLE, A
		MOV     DPTR, #scramble_txt
		LCALL   MTXT                            ; show (new) value
		CJNE    R0, #0, m7c                     ; check if changed
		RET
m7c:            LCALL   SET_SCRAMBLE                    ; if changed, update configuration
		RET

not_valid:      MOV     MENU_NR, #1                     ; To restore invalid LAST_NR variable
		RET                                     ; ERROR! 


;----------------------------
MTXT:           ; show text. Uses R1,R2. Accu points to the index
		SWAP    A
		MOV     R1, A
		MOV     A, #080H
		LCALL   DISPCOM                         ; cursor home
		MOV     R2, #8
m_sh1:          MOV     A, R1
		MOVC    A, @A+DPTR
		LCALL   DISP2CHAR
		INC     DPTR
		DJNZ    R2, m_sh1
		MOV     A, #0C0H
#IFDEF  TWOROW_DISPLAY
		MOV     A, #088H
#ENDIF
		LCALL   DISPCOM                         ; cursor to position 8
		MOV     R2, #8
m_sh2:          MOV     A, R1
		MOVC    A, @A+DPTR              
		LCALL   DISP2CHAR
		INC     DPTR
		DJNZ    R2, m_sh2
m_end:          RET



;---------------------------
ADD_A:          ; add the value in the accu and check if a is between 0 and R1-1
		ADD     A, R0
		CJNE    A, #-1, notmin1
		MOV     A, #0
		MOV     R0, #0                  ; no changes made!
		RET
notmin1:        CJNE    A, 1, notmax1
		DEC     A
		MOV     R0, #0                  ; no changes made!
notmax1:        RET


wait_txt:       .db     " Please wait... "

welc_txt:       .db     "NICAM coder 1.4s"

input_txt:      .db     "Input: ANALOG   "
		.db     "Input: SPDIF    "

output_txt:     .db     "Output: 5.85 MHz"
		.db     "Output: 6.552MHz"
		.db     "Output: 1.44 MHz"

inlvl_txt:      .db     "Inp.level: -6 dB"
		.db     "Inp.level: +0 dB"
		.db     "Inp.level: +6 dB"
		.db     "Inp.level:+12 dB"

preemp_txt:     .db     "Preemphasis: OFF"
		.db     "Preemphasis: ON "

c4bit_txt:      .db     "Reserve snd: OFF"
		.db     "Reserve snd: ON "


nicmode_txt:    .db     "Mode: STEREO    "
		.db     "Mode: DUAL MONO "
		.db     "Mode: MONO+DATA "
		.db     "Mode: DATA ONLY "

nictest_txt:    .db     "Test: OFF       "
		.db     "Test: 1KHz LEFT "
		.db     "Test: 1KHz RIGHT"
		.db     "Test: 1KHz      "

scramble_txt:   .db     "Scrambler:NORMAL"
		.db     "Scrambler: OFF  "
		.db     "Scrambler:SECRET"


;----------------------------------[ end of file "-----------------------------------------


	
