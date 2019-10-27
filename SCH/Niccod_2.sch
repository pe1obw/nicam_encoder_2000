*P CAP
4 60 00000000
-2900, -2050, 5367, 3796, 25;
*R NICCOD
*SAD1893      
800 850
1,5,100,800,100,50,700,50,700,800,100,800;
0,2,100,750,
0,2,100,700,
0,2,100,650,
0,2,100,600,
0,2,100,500,
0,2,100,450,
0,2,100,400,
0,0,700,750,
0,0,700,700,
0,0,700,650,
0,0,700,600,
0,0,700,500,
0,0,700,450,
0,0,700,400,
0,2,100,300,
0,2,100,100,
0,0,700,300,
0,0,700,250,
0,0,700,200,
0,0,700,150,
0,0,700,100;
;
0 750 0 750 35 4 128 #=3
100 750 0 750 35 2 131 LABEL=DATA_I
0 750 0 750 35 3 5 PINTYPE=IN
0 700 0 700 35 4 128 #=4
100 700 0 700 35 2 131 LABEL=BCLK_I
0 700 0 700 35 3 5 PINTYPE=IN
0 650 0 650 35 4 128 #=5
100 650 0 650 35 2 131 LABEL=WCLK_I
0 650 0 650 35 3 5 PINTYPE=IN
0 600 0 600 35 4 128 #=6
100 600 0 600 35 2 131 LABEL=LR_I
0 600 0 600 35 3 5 PINTYPE=IN
0 500 0 500 35 4 128 #=10
100 500 0 500 35 2 131 LABEL=BKPOL_I
0 500 0 500 35 3 5 PINTYPE=IN
0 450 0 450 35 4 128 #=11
100 450 0 450 35 2 131 LABEL=MODE0_I
0 450 0 450 35 3 5 PINTYPE=IN
0 400 0 400 35 4 128 #=12
100 400 0 400 35 2 131 LABEL=MODE1_I
0 400 0 400 35 3 5 PINTYPE=IN
800 750 800 750 35 4 130 #=23
700 750 800 750 35 2 133 LABEL=DATA_O
800 750 800 750 35 3 3 PINTYPE=OUT
800 700 800 700 35 4 130 #=26
700 700 800 700 35 2 133 LABEL=BCLK_O
800 700 800 700 35 3 3 PINTYPE=IN
800 650 800 650 35 4 130 #=25
700 650 800 650 35 2 133 LABEL=WCLK_O
800 650 800 650 35 3 3 PINTYPE=IN
800 600 800 600 35 4 130 #=24
700 600 800 600 35 2 133 LABEL=LR_O
800 600 800 600 35 3 3 PINTYPE=IN
800 500 800 500 35 4 130 #=19
700 500 800 500 35 2 133 LABEL=BKPOL_O
800 500 800 500 35 3 3 PINTYPE=IN
800 450 800 450 35 4 130 #=18
700 450 800 450 35 2 133 LABEL=MODE0_O
800 450 800 450 35 3 3 PINTYPE=IN
800 400 800 400 35 4 130 #=17
700 400 800 400 35 2 133 LABEL=MODE1_O
800 400 800 400 35 3 3 PINTYPE=IN
0 300 0 300 35 4 128 #=1
100 300 0 300 35 2 131 LABEL=XTAL_O
0 300 0 300 35 3 5 PINTYPE=OUT
0 100 0 100 35 4 128 #=2
100 100 0 100 35 2 131 LABEL=XTAL_I
0 100 0 100 35 3 5 PINTYPE=IN
800 300 800 300 35 4 130 #=13
700 300 800 300 35 2 133 LABEL=^RESET
800 300 800 300 35 3 3 PINTYPE=IN
800 250 800 250 35 4 130 #=16
700 250 800 250 35 2 133 LABEL=MUTE_O
800 250 800 250 35 3 3 PINTYPE=OUT
800 200 800 200 35 4 130 #=15
700 200 800 200 35 2 133 LABEL=MUTE_I
800 200 800 200 35 3 3 PINTYPE=IN
800 150 800 150 35 4 130 #=28
700 150 800 150 35 2 133 LABEL=SETLSW
800 150 800 150 35 3 3 PINTYPE=IN
800 100 800 100 35 4 130 #=27
700 100 800 100 35 2 133 LABEL=PDOWN
800 100 800 100 35 3 3 PINTYPE=IN
150 825 400 425 35 14 132 REFDES=U?
600 825 400 425 35 14 132 VALUE=AD1893
400 875 400 425 35 14 4 DEVICE=AD1893
400 925 400 425 35 12 4 PKG_TYPE=DIP28
125 0 400 425 35 14 0 SIGNAL=GND,8,14,21
175 -50 400 425 35 14 0 SIGNAL=VCC,7,22
;
*SCH          
100 100
1,5,60,100,60,0,1,5,40,100,40,0;
0,0,60,50,
0,2,40,50;
;
270 80 100 50 35 4 2 #=2
150 50 100 50 35 1 3 PINTYPE=PAS
-170 75 0 50 35 4 0 #=1
-50 50 0 50 35 1 5 PINTYPE=PAS
50 175 50 50 35 14 132 REFDES=C?
50 125 50 50 35 14 132 VALUE=?
50 225 50 50 35 14 4 DEVICE=C
30 60 50 50 8 5 132 DOT=*
50 275 50 50 35 12 4 PKG_TYPE=CRM5A
;
*SCINCH       
150 150
;
0,0,100,75,
0,3,75,25;
75,75,50,0,23040,5,0,0,
75,75,25,0,17280,5,0,0,
75,75,25,-5760,5760,5,0,0;
300 100 150 75 35 4 2 #=1
200 75 150 75 35 1 3 PINTYPE=PAS
-100 -25 75 0 35 1 3 PINTYPE=PAS
-50 125 75 75 35 14 132 REFDES=J?
75 250 75 75 35 12 4 PKG_TYPE=CINCH
-75 75 75 75 35 14 132 VALUE=?
125 -100 75 0 35 4 2 #=2
75 200 75 75 35 14 4 DEVICE=CINCH
;
*SCO          
100 100
1,5,0,60,100,60,1,5,0,40,100,40;
0,1,50,60,
0,3,50,40;
;
75 100 50 50 35 14 131 REFDES=C?
75 0 50 50 35 14 131 VALUE=?
50 275 50 50 35 12 4 PKG_TYPE=CERCON2A
50 325 50 50 35 12 4 DEVICE=C
80 220 50 100 35 4 5 #=1
50 150 50 100 35 1 1 PINTYPE=PAS
90 -120 50 0 35 4 5 #=2
50 -50 50 0 35 1 7 PINTYPE=PAS
40 70 50 50 8 5 132 DOT=*
50 350 50 100 35 1 1 RACAL=3204
;
*SCS8412      
800 1100
1,5,100,1050,100,50,1,5,100,1050,700,1050,
700,50,100,50;
0,2,100,1000,
0,2,100,950,
0,2,100,800,
0,0,700,1000,
0,0,700,950,
0,0,700,900,
0,0,700,800,
0,0,700,750,
0,0,700,700,
0,0,700,600,
0,0,700,550,
0,0,700,450,
0,0,700,400,
0,0,700,350,
0,0,700,300,
0,0,700,250,
0,0,700,200,
0,0,700,100,
0,2,100,650,
0,2,100,600,
0,2,100,550,
0,2,100,500,
0,2,100,450,
0,2,100,400,
0,2,100,300,
0,2,100,250,
0,2,100,150,
0,2,100,100;
;
0 1000 0 1000 35 4 128 #=9
100 1000 0 1000 35 2 131 LABEL=RXP
0 1000 0 1000 35 3 5 PINTYPE=IN
0 950 0 950 35 4 128 #=10
100 950 0 950 35 2 131 LABEL=RXN
0 950 0 950 35 3 5 PINTYPE=IN
0 800 0 800 35 4 128 #=20
100 800 0 800 35 2 131 LABEL=FILT
0 800 0 800 35 3 5 PINTYPE=OUT
800 1000 800 1000 35 4 130 #=26
700 1000 800 1000 35 2 133 LABEL=SDATA
800 1000 800 1000 35 3 3 PINTYPE=OUT
800 950 800 950 35 4 130 #=12
700 950 800 950 35 2 133 LABEL=SCK
800 950 800 950 35 3 3 PINTYPE=PAS
800 900 800 900 35 4 130 #=11
700 900 800 900 35 2 133 LABEL=FSYNC
800 900 800 900 35 3 3 PINTYPE=PAS
800 800 800 800 35 4 130 #=1
700 800 800 800 35 2 133 LABEL=C
800 800 800 800 35 3 3 PINTYPE=PAS
800 750 800 750 35 4 130 #=14
700 750 800 750 35 2 133 LABEL=U
800 750 800 750 35 3 3 PINTYPE=OUT
800 700 800 700 35 4 130 #=28
700 700 800 700 35 2 133 LABEL=VERF
800 700 800 700 35 3 3 PINTYPE=OUT
800 600 800 600 35 4 130 #=25
700 600 800 600 35 2 133 LABEL=ERF
800 600 800 600 35 3 3 PINTYPE=OUT
800 550 800 550 35 4 130 #=15
700 550 800 550 35 2 133 LABEL=CBL
800 550 800 550 35 3 3 PINTYPE=OUT
800 450 800 450 35 4 130 #=6
700 450 800 450 35 2 133 LABEL=^CO^/E0
800 450 800 450 35 3 3 PINTYPE=OUT
800 400 800 400 35 4 130 #=5
700 400 800 400 35 2 133 LABEL=Ca/E1
800 400 800 400 35 3 3 PINTYPE=OUT
800 350 800 350 35 4 130 #=4
700 350 800 350 35 2 133 LABEL=Cb/E2
800 350 800 350 35 3 3 PINTYPE=OUT
800 300 800 300 35 4 130 #=3
700 300 800 300 35 2 133 LABEL=Cc/F0
800 300 800 300 35 3 3 PINTYPE=OUT
800 250 800 250 35 4 130 #=2
700 250 800 250 35 2 133 LABEL=Cd/F1
800 250 800 250 35 3 3 PINTYPE=OUT
800 200 800 200 35 4 130 #=27
700 200 800 200 35 2 133 LABEL=Ca/F2
800 200 800 200 35 3 3 PINTYPE=OUT
800 100 800 100 35 4 130 #=19
700 100 800 100 35 2 133 LABEL=MCK
800 100 800 100 35 3 3 PINTYPE=PAS
0 650 0 650 35 4 128 #=13
100 650 0 650 35 2 131 LABEL=CS12/FCK
0 650 0 650 35 3 5 PINTYPE=PAS
0 600 0 600 35 4 128 #=16
100 600 0 600 35 2 131 LABEL=SEL
0 600 0 600 35 3 5 PINTYPE=PAS
0 550 0 550 35 4 128 #=23
100 550 0 550 35 2 131 LABEL=M0
0 550 0 550 35 3 5 PINTYPE=IN
0 500 0 500 35 4 128 #=24
100 500 0 500 35 2 131 LABEL=M1
0 500 0 500 35 3 5 PINTYPE=IN
0 450 0 450 35 4 128 #=18
100 450 0 450 35 2 131 LABEL=M2
0 450 0 450 35 3 5 PINTYPE=IN
0 400 0 400 35 4 128 #=17
100 400 0 400 35 2 131 LABEL=M3
0 400 0 400 35 3 5 PINTYPE=IN
0 300 0 300 35 4 128 #=7
100 300 0 300 35 2 131 LABEL=VD+
0 300 0 300 35 3 5 PINTYPE=PWR
0 250 0 250 35 4 128 #=8
100 250 0 250 35 2 131 LABEL=DGND
0 250 0 250 35 3 5 PINTYPE=PWR
0 150 0 150 35 4 128 #=22
100 150 0 150 35 2 131 LABEL=VA+
0 150 0 150 35 3 5 PINTYPE=PWR
0 100 0 100 35 4 128 #=21
100 100 0 100 35 2 131 LABEL=AGND
0 100 0 100 35 3 5 PINTYPE=PWR
150 1075 400 550 35 14 132 REFDES=U?
600 1075 400 550 35 14 132 VALUE=CS8412
400 1125 400 550 35 14 4 DEVICE=CS8412
400 1175 400 550 35 12 4 PKG_TYPE=DIP28
;
*SGND         
100 100
1,261,14,50,80,50,84,50;
0,1,50,50;
;
50 100 50 100 35 1 1 PINTYPE=PWR
50 25 50 100 35 2 4 WIRELABEL=GND
;
*SLOGO        
3350 850
1,7,1824,0,1824,850,1,7,374,750,974,750,
1,7,374,650,974,650,1,7,374,450,974,450,
1,7,400,850,400,450,1,7,974,450,3350,450,
1,7,1824,150,3350,150,1,7,2274,150,2274,0,
1,7,2724,150,2724,0,1,7,3174,450,3174,150,
1,7,2724,450,2724,150,1,7,374,550,974,550,
1,7,974,850,974,0,1,7,0,850,374,850,
1,7,0,750,374,750,1,7,0,650,350,650,
374,650,1,7,0,550,374,550,1,7,0,450,
374,450,1,7,500,350,500,250,1,7,500,350,
700,400,700,200,500,250,1,7,250,374,250,424,
1,7,250,274,250,324,1,7,250,224,250,174,
1,7,124,300,174,300,1,7,224,300,274,300,
1,7,324,300,374,300,1,7,424,300,474,300,
1,7,524,300,574,300,1,7,600,300,650,300,
1,7,674,300,724,300,1,7,374,850,3350,850,
1,7,0,850,0,0,3350,0,3350,850,1,7,
0,150,974,150;
;
250,300,50,0,23040,7,0,0,
250,300,100,0,23040,7,0,0;
1400 225 1675 425 120 9 132 COMPANY=MUETTA
50 675 1675 425 35 7 128 DRAWN=DRAWN
50 575 1675 425 35 7 128 CHECK=CHECK
50 475 1675 425 35 7 128 DESIGN=DESIGN
575 775 1675 425 35 7 128 t1=DATE
50 775 1675 425 35 7 128 APPROVALS=APPROVALS
1025 750 1675 425 45 7 128 t2=PROJECT
1400 600 1675 425 70 15 132 PROJECT=?
1875 750 1675 425 45 7 128 t4=TITLE
1875 625 1675 425 50 15 128 TITLE1=?
1875 500 1675 425 50 15 128 TITLE2=?
1875 225 1675 425 50 15 128 CUSTOMER=?
1875 350 1675 425 45 7 128 t5=CUSTOMER
2800 350 1675 425 35 7 128 t8=DRAWING
2800 250 1675 425 35 15 128 DRAWING=?
3200 350 1675 425 35 7 128 t7=REV
3250 275 1675 425 50 15 132 REV=?A
2925 50 1675 425 45 15 128 SHEET=?1
3175 50 1675 425 45 15 128 OF=?1
3075 50 1675 425 35 7 128 t6=of
2750 50 1675 425 35 7 64 SHT=
1925 50 1675 425 35 7 192 SIZE=A3
2300 50 1675 425 35 7 128 t9=SCALE
425 675 1675 425 35 15 128 DATE_DRAWN=?
425 575 1675 425 35 15 128 DATE_CHECK=?
425 475 1675 425 35 15 128 DATE_DESIGN=?
25 75 1675 425 35 7 131 F1=FILE:
200 75 1675 425 35 15 131 FILENAME=?
;
*SPORT_IN     
125 50
1,3,74,25,100,9,124,9,124,39,100,39,
74,25;
0,2,75,25;
;
0 25 0 25 35 1 5 PORT=PAS
150 25 0 25 35 14 131 LABEL=?
;
*SPORT_OUT    
125 50
1,3,74,39,74,9,100,9,124,25,100,39,
74,39;
0,2,75,25;
;
0 25 0 25 35 1 5 PORT=PAS
150 25 0 25 35 14 131 LABEL=?
;
*SR5%         
100 250
1,5,24,50,74,50,74,200,24,200,24,50;
0,1,50,200,
0,3,50,50;
;
50 250 50 250 35 4 8 #=1
50 250 50 250 35 1 1 PINTYPE=PAS
50 0 50 0 35 4 2 #=2
50 0 50 0 35 1 7 PINTYPE=PAS
100 150 50 125 35 14 131 REFDES=R?
25 325 50 125 35 12 4 DEVICE=R5%
100 100 50 125 35 14 131 VALUE=?
50 125 50 125 35 5 148 PERCENT=5%
75 375 50 125 35 12 4 RACAL=3204
40 210 50 125 8 5 132 DOT=*
50 450 50 125 35 12 4 PKG_TYPE=RES10
;
*SR5%H        
250 100
1,5,50,74,50,24,200,24,200,74,50,74;
0,0,200,50,
0,2,50,50;
;
375 75 250 50 35 4 2 #=2
250 50 250 50 35 1 3 PINTYPE=PAS
-125 75 0 50 35 4 0 #=1
0 50 0 50 35 1 5 PINTYPE=PAS
125 150 125 50 35 14 132 REFDES=R?
125 100 125 50 35 14 132 VALUE=?
125 200 125 50 35 14 4 DEVICE=R5%
125 50 125 50 35 5 132 PERCENT=5%
125 -50 125 50 35 12 4 RACAL=3204
40 60 125 50 8 5 132 DOT=*
125 250 125 50 35 12 4 PKG_TYPE=RES10
;
*STITLE       
1850 675
1,259,48,149,1800,150,1,259,950,149,950,50,
1,259,1300,149,1300,50,1,259,50,399,1800,400,
1,259,50,499,1800,500,1,259,350,599,350,499,
1,259,1050,599,1050,499,1,259,50,624,50,50,
1800,50,1800,624,1,259,1800,600,50,600;
;
;
75 100 925 337 35 3 131 TXT9=Initial
975 100 925 337 35 3 131 TXT10=Page:
1325 100 925 337 35 3 131 TXT11=of:
75 350 925 337 35 3 131 TXT5=Company:
75 200 925 337 35 3 131 TXT8=Country:
550 100 925 337 35 3 131 Initial_date=?
575 550 925 337 35 3 131 Date=?
375 450 925 337 35 3 131 Project=?
1175 100 925 337 35 3 131 Page=?
1450 100 925 337 35 3 131 Of=?
225 550 925 337 35 3 131 Rev=?
375 350 925 337 35 3 131 Company=?
375 300 925 337 35 3 131 Address=?
375 250 925 337 35 3 131 City=?
375 200 925 337 35 3 131 Country=?
1225 550 925 337 35 3 131 Eng=?
75 250 925 337 35 3 131 TXT7=City
75 450 925 337 35 3 131 TXT4=Project:
75 550 925 337 35 3 131 TXT1=Rev:
375 550 925 337 35 3 131 TXT2=Date:
1075 550 925 337 35 3 131 TXT3=Eng:
75 300 925 337 35 3 131 TXT6=Address:
;
*SVCC         
100 70
1,5,34,50,64,50;
2,3,50,50;
;
50 100 50 0 35 2 132 WIRELABEL=VCC
50 -40 50 0 35 1 7 PINTYPE=PWR
;
*SXTAL        
200 200
1,5,74,150,124,150,124,50,74,50,74,150,
1,261,150,124,150,74,1,261,50,124,50,74;
0,0,150,100,
0,2,50,100;
;
200 100 200 100 35 4 2 #=2
200 100 200 100 35 1 3 PINTYPE=PAS
0 100 0 100 35 4 0 #=1
0 100 0 100 35 1 5 PINTYPE=PAS
100 175 100 100 35 14 132 REFDES=X?
100 225 100 100 35 14 4 DEVICE=XTAL
100 25 100 100 35 14 132 VALUE=?
100 275 100 100 35 12 4 PKG_TYPE=XTAL
;
*C 1500 2000 0 VCC
;
*C 2950 1700 0 PORT_IN
150 25 0 25 35 14 131 LABEL=LRCLK
;
*C 2950 1800 0 PORT_IN
150 25 0 25 35 14 131 LABEL=BCLK
;
*C 2950 1850 0 PORT_OUT
150 25 0 25 35 14 131 LABEL=DOUT
;
*C 1250 1075 0 GND
;
*C 1400 1175 0 CH
50 -100 50 50 35 14 132 VALUE=27P
50 -50 50 50 35 14 132 REFDES=C28
;
*C 1400 1375 0 CH
50 175 50 50 35 14 132 REFDES=C16
50 125 50 50 35 14 132 VALUE=27P
;
*C 2550 1475 3 VCC
50 150 50 0 35 2 148 WIRELABEL=VCC
;
*C 2400 1075 0 GND
;
*C -450 1225 0 VCC
;
*C -650 1225 0 VCC
;
*C -650 875 0 CO
75 100 50 50 35 14 131 REFDES=C11
75 0 50 50 35 14 131 VALUE=100N
;
*C -450 875 0 CO
75 100 50 50 35 14 131 REFDES=C25
75 0 50 50 35 14 131 VALUE=100N
;
*C 1650 1225 1 XTAL
100 250 100 100 35 14 148 VALUE=16MC
150 250 100 100 35 14 148 REFDES=X1
;
*C -600 1625 0 CH
50 -100 50 50 35 14 132 VALUE=47N
50 -50 50 50 35 14 132 REFDES=C15
;
*C -750 1375 0 GND
;
*C -600 1975 0 CH
50 175 50 50 35 14 132 REFDES=C3
50 125 50 50 35 14 132 VALUE=10N
;
*C -600 1775 0 CH
50 175 50 50 35 14 132 REFDES=C2
50 125 50 50 35 14 132 VALUE=10N
;
*C -450 1625 0 R5%H
125 150 125 50 35 14 132 REFDES=R12
125 100 125 50 35 14 132 VALUE=1K
;
*C -250 725 0 GND
;
*C -150 875 0 CS8412
150 1075 400 550 35 14 132 REFDES=U2
;
*C -1125 1775 0 GND
;
*C -1150 1950 0 CINCH
-75 75 75 75 35 14 4 VALUE=?
-50 50 75 75 35 14 132 REFDES=J2
-150 100 125 112 35 14 132 VALUE=SPDIF IN
;
*C -975 1725 0 R5%
100 150 50 125 35 14 131 REFDES=R4
100 100 50 125 35 14 131 VALUE=75E
;
*C -975 1575 0 GND
;
*C -1500 -1000 4 PORT_IN
150 25 0 25 35 14 131 LABEL=GND
;
*C -1500 -750 4 PORT_IN
150 25 0 25 35 14 131 LABEL=VCC
;
*C -1150 -1150 0 GND
;
*C -1150 -650 0 VCC
;
*C -1150 -900 0 CO
75 100 50 50 35 14 131 REFDES=C35
75 0 50 50 35 14 131 VALUE=100N
;
*C 1600 1125 0 AD1893
150 825 400 425 35 14 132 REFDES=U8
;
*C 3100 -1650 0 TITLE
375 200 925 337 35 3 3 Country=?
75 200 925 337 35 3 3 TXT8=Country:
375 250 925 337 35 3 3 City=?
75 250 925 337 35 3 3 TXT7=City
375 300 925 337 35 3 3 Address=?
75 300 925 337 35 3 3 TXT6=Address:
1225 550 925 337 35 3 131 Eng=JWD
375 350 925 337 35 3 131 Company=PE1OBW
225 550 925 337 35 3 131 Rev=A
1450 100 925 337 35 3 131 Of=6
1175 100 925 337 35 3 131 Page=2
375 450 925 337 35 3 131 Project=NICAM 728 ENCODER - SPDIF INPUT CIRCUIT
575 550 925 337 35 3 131 Date=980416
550 100 925 337 35 3 131 Initial_date=980101
;

*LT 1 -975
-1500 -1100 25 0 4;
*LT 1 -725
-1500 -1100 24 0 4;
*LT 1 825
-600 -200 3 0 4;
*LT 1 975
-200 -150 3 0 4;
*LT 1 1025
-400 -150 4 0 4;
*LT 1 1125
-200 -150 3 0 4;
*LT 1 1175
-600 -150 2 0 4;
*LT 1 1225
1300 1400 7 0 4
1500 1600 11 0 4
2400 2450 10 0 4;
*LT 1 1275
-200 -150 3 0 4
2400 2450 10 0 4;
*LT 1 1325
-200 -150 3 0 4;
*LT 1 1375
-200 -150 3 0 4;
*LT 1 1425
-200 -150 3 0 4
1300 1400 7 0 4
1500 1600 13 0 4
2400 2550 12 0 4;
*LT 1 1475
-250 -150 2 0 4;
*LT 1 1525
-200 -150 3 0 4
1550 1600 7 0 4
2400 2450 10 0 4;
*LT 1 1575
1300 1600 7 0 4
2400 2450 10 0 4;
*LT 1 1625
1550 1600 8 0 4
2400 2450 10 0 4;
*LT 1 1675
-700 -600 1 0 4
-500 -450 15 0 4
-200 -150 14 0 4;
*LT 1 1725
1200 1600 6 0 4
2400 2950 16 0 4;
*LT 1 1775
650 1200 6 0 4;
*LT 1 1825
-700 -600 1 0 4
-500 -150 19 0 4
650 1600 18 0 4
2400 2950 17 0 4;
*LT 1 1875
-250 -150 5 0 4
650 1600 21 0 4
2400 2950 20 0 4;
*LT 1 2025
-1000 -600 22 0 4
-500 -250 5 0 4;
*LT 1 -1100
-1050 -900 25 0 5
-800 -650 24 0 5;
*LT 1 -1075
1875 1950 0 0 5;
*LT 1 -925
1675 1725 23 0 5
1975 2025 22 0 5;
*LT 1 -700
1475 1825 1 0 5;
*LT 1 -600
825 875 3 0 5
975 1225 2 0 5;
*LT 1 -400
825 875 3 0 5
975 1225 4 0 5;
*LT 1 -250
1175 1475 2 0 5
1875 2025 5 0 5;
*LT 1 -200
825 1525 3 0 5;
*LT 1 1200
1725 1775 6 0 5;
*LT 1 1300
1175 1575 7 0 5;
*LT 1 1550
1525 1575 7 0 5
1625 2000 8 0 5;
*LT 1 2400
1325 1375 9 0 5;
*LT 1 2450
1175 1625 10 0 5;
*V -1100
-975 25 0 0
-725 24 0 0;
*V -925
2025 24 0 0;
*V -700
1675 130 0 0;
*V -600
1175 131 0 0;
*V -400
825 132 0 0
1025 133 0 0;
*V -250
1175 131 0 0;
*V -200
825 132 0 0
975 132 0 0
1125 132 0 0
1275 132 0 0
1325 132 0 0
1375 132 0 0
1425 132 0 0;
*V 1300
1225 136 0 0
1425 136 0 0;
*V 1550
1225 140 0 0
1425 142 0 0
1575 136 0 0;
*V 2450
1225 139 0 0
1275 139 0 0
1525 139 0 0
1575 139 0 0;
*X 275 2600 50 15 0 1 SPDIF RECEIVER
*X 2000 2600 50 15 0 1 SAMPLERATE CONVERTER

**
