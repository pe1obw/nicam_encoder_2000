@ECHO OFF

rem ----- build version for 1 row display ... ---
tasm -51 nicam.asm
del nicam1r.hex
ren nicam.obj nicam1r.hex

rem ----- build version for 2 row display ... ---
tasm -51 nicam.asm -dTWOROW_DISPLAY
del nicam2r.hex
ren nicam.obj nicam2r.hex

pause
