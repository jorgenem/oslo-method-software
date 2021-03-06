100 REM    LGFT_BET.BAS         MARCH 28, 1991         J. KANTELE

110 COLOR 12: CLS

120 PRINT "*****************************************************************"

130 PRINT

140 PRINT "PROGRAM TO CALCULATE Log f AND Log ft VALUES FOR ALLOWED BETA (-)"

150 PRINT "TRANSITIONS. Log f AGREES WITH GOVE AND MARTIN [NDT 10 (1971) "

160 PRINT "206] GENERALLY TO A FEW PER CENT, OR A FEW UNITS IN THE SECOND "

170 PRINT "DECIMAL PLACE, FOR 6 <= Z <= 95 AND 10 keV <= E <= 10 MeV. THE"

180 PRINT "ACCURACY IS 1 PER CENT FOR THE INPUT Z VALUES, Z = 10, 20,...,90."

190 PRINT

200 PRINT "*****************************************************************"

210 PRINT

220 INPUT "ATOMIC NUMBER OF THE DAUGHTER NUCLEUS, Z = ", Z

230 PRINT

240 PRINT "DO YOU WANT TO GIVE THE (TOTAL) HALF-LIFE OF THE MOTHER NUCLEUS IN"

250 PRINT "(1) YEARS,  (2) DAYS,  (3) HOURS,  (4) MINUTES,  (5) SECONDS? "

260 PRINT

270 INPUT "YOUR CHOICE (1 TO 5) = ", N

280 PRINT

290 FOR K = 1 TO 5

300 READ T1(K)

310 NEXT K

320 DATA 3.1557E+7, 86400, 3600, 60, 1

330 INPUT "ENTER HALF-LIFE IN THE UNITS OF YOUR CHOICE, T = ", T

340 PRINT

350 INPUT "ENTER FRACTION P OF BETA BRANCH (IN PER CENT), P = ", P

360 PRINT : IF P > 100 THEN 350

370 INPUT "BETA-RAY END-POINT ENERGY (in keV) E = ", E

380 PRINT

390 T2 = T1(N) * T / P * 100

400 FOR K = 1 TO 4

410     FOR L = 1 TO 4

420         READ T(K, L)

430     NEXT L

440 NEXT K

450 DATA -17.2, 7.9015, -2.54, 0.28482

460 DATA 3.31368, -2.06273, 0.703822, -.075039

470 DATA -.364018, .387961, -.142528, .016

480 DATA .0278071,-.026519, .0098854, -.00113772

490 FOR I = 1 TO 4

500 GOSUB 630

510 NEXT I

520 X = C(1) + C(2) * LOG(E) + C(3) * (LOG(E)) ^ 2 + C(4) * (LOG(E)) ^ 3

530 G = LOG(T2) / LOG(10) + X

540 PRINT USING "FOR ALLOWED TRANSITION, Log f(-) = ##.##"; X

550 PRINT

560 PRINT USING "Log ft = ##.##"; G

570 PRINT : IF G > 6 THEN 580 ELSE 600

580 PRINT "NOTE: IF Log ft IS ~> 6, THEN THE TRANSITION MAY NOT BE ALLOWED"

590 PRINT

600 PRINT "*****************************************************************"

610 COLOR 11: PRINT "HIT CTRL-C TO QUIT AT ANY TIME"

620 COLOR 12: GOTO 200

630 C(I) = T(I, 1) + T(I, 2) * LOG(Z) + T(I, 3) * LOG(Z) ^ 2 + T(I, 4) * LOG(Z) ^ 3

640 RESTORE

650 RETURN

