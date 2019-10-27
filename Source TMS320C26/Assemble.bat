@echo off

rem Assembles all DSP assembly functinos
rem Date: 20000102 JWD
rem 


rem ----- no preemphasis ------
tasm -3225 -x nicamdsp.asm -b -dNO_PREEMP
del dsp_nopr.obj
ren nicamdsp.obj dsp_nopr.obj
bin2asm DSP_NOPR.obj

rem ----- gain -6 dB ------
tasm -3225 -x nicamdsp.asm -b -dGAIN_M6DB
del dsp_m6dB.obj
ren nicamdsp.obj dsp_m6dB.obj
bin2asm DSP_M6DB.obj

rem ----- gain 0 dB ------
tasm -3225 -x nicamdsp.asm -b -dGAIN_0DB
del dsp_0dB.obj
ren nicamdsp.obj dsp_0dB.obj
bin2asm DSP_0DB.obj

rem ----- gain 6 dB ------
tasm -3225 -x nicamdsp.asm -b -dGAIN_6DB
del dsp_6dB.obj
ren nicamdsp.obj dsp_6dB.obj
bin2asm DSP_6DB.obj

rem ----- gain 12 dB ------
tasm -3225 -x nicamdsp.asm -b -dGAIN_12DB
del dsp_12dB.obj
ren nicamdsp.obj dsp_12dB.obj
bin2asm DSP_12DB.obj

rem ----- beep both channels -----
tasm -3225 -x nicbeep.asm -b -dBOTH_CH
del beepboth.obj
ren nicbeep.obj beepboth.obj
bin2asm BEEPBOTH.obj

rem ----- beep left channel -----
tasm -3225 -x nicbeep.asm -b -dLEFT_CH
del beepleft.obj
ren nicbeep.obj beepleft.obj
bin2asm BEEPLEFT.obj

rem ----- beep right channel -----
tasm -3225 -x nicbeep.asm -b -dRIGHT_CH
del beeprght.obj
ren nicbeep.obj beeprght.obj
bin2asm BEEPRGHT.obj

pause

