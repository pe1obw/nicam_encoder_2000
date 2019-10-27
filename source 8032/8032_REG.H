;-------[ 8032_REG.H ]------------------------------------------
;  8032/8052 registers.
;  Geschreven door Werner, Willem, Gerbert, Bert en dergelijke.
;
;	Update 980607 JWD
;---------------------------------------------------------------

RCAP2L  .EQU    0CAH
RCAP2H  .EQU    0CBH
TL2     .EQU    0CCH
TH2     .EQU    0CDH    ;timer 2 tellers

SP      .EQU    081H    ;Stack pointer
DPL     .EQU    082H
DPH     .EQU    083H
PCON    .EQU    087H
TMOD    .EQU    089H
TL0     .EQU    08AH
TL1     .EQU    08BH
TH0     .EQU    08CH
TH1     .EQU    08DH
SBUF    .EQU    099H
R2      .EQU    002H
R4      .EQU    004H

SCON    .EQU    098H	
SCON.0	.EQU	098H
SCON.1	.EQU	099H
SCON.2	.EQU	09AH
SCON.3	.EQU	09BH
SCON.4	.EQU	09CH
SCON.5	.EQU	09DH
SCON.6	.EQU	09EH
SCON.7	.EQU	09FH

RI	.EQU	098H
TI	.EQU	099H
RB8	.EQU	09AH
TB8	.EQU	09BH
REN	.EQU	09CH
SM2	.EQU	09DH
SM1	.EQU	09EH
SM0	.EQU	09FH

BCC     .EQU    0F0H
BCC.0   .EQU    0F0H
BCC.1   .EQU    0F1H
BCC.2   .EQU    0F2H
BCC.3   .EQU    0F3H
BCC.4   .EQU    0F4H
BCC.5   .EQU    0F5H
BCC.6   .EQU    0F6H
BCC.7   .EQU    0F7H
B       .EQU    0F0H    ;Secondary Accumulator
B.0     .EQU    0F0H
B.1     .EQU    0F1H
B.2     .EQU    0F2H
B.3     .EQU    0F3H
B.4     .EQU    0F4H
B.5     .EQU    0F5H
B.6     .EQU    0F6H
B.7     .EQU    0F7H

PSW     .EQU    0D0H
PSW.0   .EQU    0D0H
PSW.1   .EQU    0D1H
PSW.2   .EQU    0D2H
PSW.3   .EQU    0D3H
PSW.4   .EQU    0D4H
PSW.5   .EQU    0D5H
PSW.6   .EQU    0D6H
PSW.7   .EQU    0D7H

IEC     .EQU    0A8H
IEC.0   .EQU    0A8H    ;int0
IEC.1   .EQU    0A9H    ;timer0
IEC.2   .EQU    0AAH    ;int1
IEC.3   .EQU    0ABH    ;timer1
IEC.4   .EQU    0ACH    ;serieele poort
IEC.5   .EQU    0ADH    ;timer2
IEC.6   .EQU    0AEH
IEC.7   .EQU    0AFH

EX0	.EQU	0A8h
ET0	.EQU	0A9h
EX1	.EQU	0AAh
ET1	.EQU	0ABh
ES	.EQU	0ACh
ET2	.EQU	0ADh
EA	.EQU	0AFh		

IPC     .EQU    0B8H
IPC.0   .EQU    0B8H
IPC.1   .EQU    0B9H
IPC.2   .EQU    0BAH
IPC.3   .EQU    0BBH
IPC.4   .EQU    0BCH
IPC.5   .EQU    0BDH
IPC.6   .EQU    0BEH
IPC.7   .EQU    0BFH
PX0	.EQU	0B8h
PT0	.EQU	0B9h
PX1	.EQU	0BAh
PT1	.EQU	0BBh
PS	.EQU	0BCh
PT2	.EQU	0BDh


TCON    .EQU    088H
TCON.0  .EQU    088H
TCON.1  .EQU    089H
TCON.2  .EQU    08AH
TCON.3  .EQU    08BH
TCON.4  .EQU    08CH
TCON.5  .EQU    08DH
TCON.6  .EQU    08EH
TCON.7  .EQU    08FH

IT0	.EQU	088h
IE0	.EQU	089h
IT1	.EQU	08Ah
IE1	.EQU	08Bh
TR0	.EQU	08Ch
TF0	.EQU	08Dh
TR1	.EQU	08Eh
TF1	.EQU	08Fh

T2CON   .EQU    0C8H    ;timer 2 control
T2CON.0 .EQU    0C8H
T2CON.1 .EQU    0C9H
T2CON.2 .EQU    0CAH
T2CON.3 .EQU    0CBH
T2CON.4 .EQU    0CCH
T2CON.5 .EQU    0CDH
T2CON.6 .EQU    0CEH
T2CON.7 .EQU    0CFH

P0      .EQU    080H    ;Port 0
P0.0	.EQU	080H
P0.1	.EQU	081H
P0.2	.EQU	082H
P0.3	.EQU	083H
P0.4	.EQU	084H
P0.5	.EQU	085H
P0.6	.EQU	086H
P0.7	.EQU	087H

P1      .EQU    090H
P1.0    .EQU    090H
P1.1    .EQU    091H
P1.2    .EQU    092H
P1.3    .EQU    093H
P1.4    .EQU    094H
P1.5    .EQU    095H
P1.6    .EQU    096H
P1.7    .EQU    097H
	   
P2      .EQU    0A0H
P2.0    .EQU    0A0H
P2.1    .EQU    0A1H
P2.2    .EQU    0A2H
P2.3    .EQU    0A3H
P2.4    .EQU    0A4H
P2.5    .EQU    0A5H
P2.6    .EQU    0A6H
P2.7    .EQU    0A7H

P3      .EQU    0B0H
P3.0    .EQU    0B0H
P3.1    .EQU    0B1H
P3.2    .EQU    0B2H
P3.3    .EQU    0B3H
P3.4    .EQU    0B4H
P3.5    .EQU    0B5H
P3.6    .EQU    0B6H
P3.7    .EQU    0B7H

ACC     .EQU    0E0H
ACC.0   .EQU    0E0H
ACC.1   .EQU    0E1H
ACC.2   .EQU    0E2H
ACC.3   .EQU    0E3H
ACC.4   .EQU    0E4H
ACC.5   .EQU    0E5H
ACC.6   .EQU    0E6H
ACC.7   .EQU    0E7H

   
