cnt = 32: REM nr of samples
PI = 3.14159265#

OPEN "beeptabb.bin" FOR OUTPUT AS #1
PRINT #1, "; Beeptab.bin"
PRINT #1, "; 1 kHz beep"
PRINT #1,
PRINT #1, "BEEPTAB:"
FOR n = 0 TO cnt - 1
    PRINT #1, "    .word " + STR$(INT(32767 * SIN(2 * PI * n / cnt) + .5))
    PRINT #1, "    .word " + STR$(INT(32767 * SIN(2 * PI * n / cnt) + .5))
NEXT
PRINT #1,
CLOSE 1

OPEN "beeptabl.bin" FOR OUTPUT AS #1
PRINT #1, "; Beeptab.bin"
PRINT #1, "; 1 kHz beep"
PRINT #1,
PRINT #1, "BEEPTAB:"
FOR n = 0 TO cnt - 1
    PRINT #1, "    .word " + STR$(INT(32767 * SIN(2 * PI * n / cnt) + .5))
    PRINT #1, "    .word 0"
NEXT
PRINT #1,
CLOSE 1

OPEN "beeptabr.bin" FOR OUTPUT AS #1
PRINT #1, "; Beeptab.bin"
PRINT #1, "; 1 kHz beep"
PRINT #1,
PRINT #1, "BEEPTAB:"
FOR n = 0 TO cnt - 1
    PRINT #1, "    .word 0"
    PRINT #1, "    .word " + STR$(INT(32767 * SIN(2 * PI * n / cnt) + .5))
NEXT
PRINT #1,
CLOSE 1

