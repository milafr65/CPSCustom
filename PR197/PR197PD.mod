******* PR197PD 133 <913148219>
      ****************************************************************
      *                    PR197 - PAYROLL CLOSE                     *
      ****************************************************************
      *  PHASE 1: CLOSE PAYROLL AND UPDATE BENEFITS, CREATES         *
      *           PRDISTRIB.                                         *
      *                                                              *
      *  PHASE 2: GL REPORT AND BUILD PRDISTRIB                      *
      *                                                              *
      *  PHASE 3: BENEFITS REPORT                                    *
      *                                                              *
      *  PHASE 4: CLEAN-UP - UPDATE ONE-TIME DEDS, CLOSE TEMP VOIDS, *
      *           RESET RUN FLAGS, CANADA: UPDATE EHT TAX WAGES      *
      *                                                              *
      ****************************************************************
      ****************************************************************
      *                            PR197PD                           *
      ****************************************************************
      *                                                              *
      *  JT#      TAG      DESCRIPTION                               *
      *  ------   ------   ----------------------------------------  *
      *  320354 | J20354 | Moved values from table PAYDEDUCTN into   *
      *         |        | CODA table to retain Canadian Business    *
      *         |        | Group Number.                             *
      *  ------   ------   ----------------------------------------  *
      *  332076 | J32076 | EVALUATE WS-PLN-CODA-CONTRIB USING        *
      *         |        | CORRECT SUBSCRIPT.                        *
      *  ------   ------   ----------------------------------------  *
      *  358525 | J58525 | ADDED 3 COMMITS IN PARA 313- TO SEPARATE  * 
      *         |        | UNIT OF WORK FOR PRDISTRIB AND OBJ-ID.    *
      *  ------ | ------ | ----------------------------------------  *
      *  447059 | J47059 | ADDED VARIABLE POPULATION FOR NEW ROUTINE *
      *         |        | 7850-GET-GLT-TYPE                         *
      *  ------ | ------ | ----------------------------------------  *
      *  547909 | J47909 | GET BUS-NBR-GRP AND QC-ENT-NBR-GRP FROM   *
      *         |        | PAYDEDUCTN WHEN CHECK-TYPE IS J           *
      *  ------   ------   ------------------------------------------*
      *  557679 | J57679 | Changes for Increase of PCD-SEQ-NBR to 8  *
      *  ------   ------   ------------------------------------------*
      *  619710 | J19710 | ADDED ADDITIONAL VALIDATION BEFORE CALL   *
      *         |        | TO 243-MOVE-TBL-TO-GMWORK WHICH CREATES   *
      *         |        | GM3 RECORDS FOR GM REPORT AND GMTRANEFRT  *
      *  ------ | ------ | ----------------------------------------  *
      *  615880 | J15880 | MOVED VALUE FROM WR3-PCD-SEQ-NBR TO NEW   *
      *         |        | VAR WS-DST-PCD-SEQ-NBR THAT WILL BE USE   *
      *         |        | WHEN CALLING 7850-GET-GLT-TYPE            *
      *  ------ | ------ | ----------------------------------------  *
      *  627195 | J27195 | MOVED VALUE FROM WR3-HOURS AMOUNT TO WS-  *
      *         |        | TRD-HOURS AND WR3-UNITS-AMOUNT TO WS-TRD  *
      *         |        | -PAY-UNITS B4 CALL TO 7850-GET-GLT-TYPE   *
      *  ------ | ------ | ----------------------------------------  *
      *  629112 | J29112 | REMOVED CHANGES MADE FOR JT619710.     -  *
      *  ------ | ------ | ----------------------------------------  *
      *  651646 | J51646 | MOVE PRM-GL-DATE TO IFACWS-POST-DATE AND  *
      *         |        | IFACWS-TRAN-DATE FOR ACTIVITY CHECKING    *
      *  ------ | ------ | ----------------------------------------  *
      *  611414 | J11414 | CHANGED FOR INDIA PAYROLL                 *
      *  ------ | ------ | ----------------------------------------  *
      *  668929 | J68929 | MODIFICATION FOR YTD-EXEMPT POPULATION    *
      *  ------ | ------ | ----------------------------------------  *
      *  272349 | J72349 | 409A amounts should be subtracted from    *
      *         |        | 401K plan totals for PR197 report and EE  *
      *         |        | COMPHIST records for 401k plans should be *
      *         |        | updated as well.                          *
      *  ------   ------   ------- --------------------------------  *
      *  716146 | J16146 | REMOVED ESIC-REASON                       *
      *  ------   ------   ------- --------------------------------  *
      *  878009 | J78009 | FIXED FOR YTD CALCULATION IN C6EXEMPT     *
      *  ------ | ------ | ---------------------------------------   *
      *  731827 | J31827 | Interface GHR Absence to print PMTOUT file*
      *  ------ | ------ | ----------------------------------------  *
      *  927044 | J27044 | ADDED CHECK-TYPE "M" & "H" FOR PAYDEDUCTN *
      *         |        | POPULATION OF BUS-NBR-GRP & QC-ENT-NBR-GRP*
      *  ------ | ------ | ----------------------------------------  * 
      *  954027 | J54027 | CORRECT ISSUES ON GM DISTRIBUTION REPORT  *
      *         |        | AND POSTING ON GMTRANEFRT                 *
      *  ------   ------   ----------------------------------------  *
      * 1110029 | 110029 | Removed excess printing of bursting tags  *
      *         |        | for entity company.                       *
      *  ------   ------   ----------------------------------------  *
      * 1189834 | 189834 | PR197 fails with FindDBRec error is       *
      *         |        | Operation illegal while not in transaction*
      *         |        | on COMPHIST in recovery only              *
      *  ------   ------   ----------------------------------------  *
      * 1247726 | 247726 | Enable PR197 to update record type S OTDs *
      *         |        | for mid cycle run.                        *
      *  ------   ------   ----------------------------------------  *
      * 1199021 | J99021 | ADDED LOGIC FOR GHR BENEFIT COMPANIES TO  *
      *         |        | UPDATE COMPHIST FOR PLANS THAT ARE SETUP  *
      *         |        | ON BN12.3.THIS WILL ALLOW MAX COMP LIMIT  *
      *         |        | TO HAVE VALID YTD AMOUNTS IN PR80/PR140   *
      *  ------   ------   ----------------------------------------  *
      * 1287945 | J87945 | ADDED LOGIC TO LOAD COMPANY DEDUCTIONS    *
      *         |        | THAT ARE TIED TO A GHR PLAN ON BN12.3.    *
      *         |        | THIS WILL ALLOW MAX COMP LIMIT VALUE OF R *
      *         |        | FUNCTION PROPERLY.                        *
      ****************************************************************
000100****************************************************************
000200 050-EDIT-PARAMETERS             SECTION.
000300****************************************************************
000400 050-START.
000500
           INITIALIZE PRPRD-PRD-WS.

000600     MOVE PRM-COMPANY            TO DB-COMPANY
000700                                    IFACWS-COMPANY.
000800     MOVE SPACES                 TO DB-PROCESS-LEVEL.
000900     PERFORM 840-FIND-PRSSET1.
001000     IF (PRSYSTEM-NOTFOUND)
001100         MOVE PRM-COMPANY        TO CRT-ERR-VAR1
001200         MOVE 101                TO CRT-ERROR-NBR
001300         PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-END
001400     ELSE
001500         MOVE PRS-NAME           TO WS-CO-NAME
001600         MOVE PRS-GL-UNITS       TO WS-GL-UNITS
P04111                                    PRPRD-GL-UNITS
001700         MOVE PRS-GLI-OPTION     TO WS-GLI-OPTION
001800         MOVE PRS-COMPANY        TO HRPRS-COMPANY
001900         MOVE PRS-TIPS           TO HRPRS-TIPS
002000         MOVE PRS-ACR-DIST-CO    TO HRPRS-CO-ACR-DIST-CO
002100         MOVE PRS-ACR-ACCT-UNIT  TO HRPRS-CO-ACR-ACCT-UNIT
002200         MOVE PRS-ES-DIST-CO     TO HRPRS-ES-DIST-CO
002300         MOVE PRS-ES-ACCT-UNIT   TO HRPRS-ES-ACCT-UNIT
002400         MOVE PRS-ES-ACCOUNT     TO HRPRS-ES-ACCOUNT
002500         MOVE PRS-ES-SUB-ACCT    TO HRPRS-ES-SUB-ACCT
002600         MOVE PRS-PRD-OPTION     TO HRPRS-CO-PRD-OPTION
002700         MOVE PRS-AT-LIMIT-DEDS  TO HRPRS-AT-LIMIT-DEDS
002800         MOVE "N"                TO HRPRS-PRD-OPTION
               MOVE PRS-CURRENCY-CODE  TO PRPRD-CO-CURRENCY-CODE
               MOVE PRS-CURRENCY-FLAG  TO PRPRD-CO-CURRENCY-FLAG
GMDIST         MOVE PRS-EMPLOYER-DED   TO HRPRS-EMPLOYER-DED
002900         MOVE PRS-AP-COMPANY     TO HRPRS-AP-COMPANY
J11414         MOVE PRS-PAYROLL-YEAR   TO WS-IPR-PAYROLL-YEAR
J47977                                    PRPRD-CO-PAYROLL-YEAR
P75509         MOVE PRS-OVR-DED-EXP    TO PRPRD-CO-OVR-DED-EXP.
003000
003100     IF (HRPRS-CO-PRD-OPTION = " ")
003200         MOVE "Y"                TO HRPRS-CO-PRD-OPTION.
003300*
003400     IF  (PRS-INCLUDE-TP135  = "Y")
003500     AND (PRS-TP135-RUN-FLG  = "R")
003600         MOVE 123                TO CRT-ERROR-NBR
003700         PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-END.
003800
003900     IF  (PRS-PR132-RUN-FLG = "R")
           AND (NOT INTERACTIVE)
004000         MOVE 120                TO CRT-ERROR-NBR
004100         PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-END.
004200*
004300     IF  (PRS-PR140-RUN-FLG = "R")
           AND (NOT INTERACTIVE)
004400         MOVE 102                TO CRT-ERROR-NBR
004500         PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-END.
004600*
004700     IF  (PRS-PR160-RUN-FLG = "R")
           AND (NOT INTERACTIVE)
004800         MOVE 103                TO CRT-ERROR-NBR
004900         PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-END.
005000*
P62055     IF  (PRM-UPDATE-OPTION = "1" OR "3")
               SET REPORT-ONLY         TO TRUE
               SET PRPRD-REPORT-ONLY   TO TRUE.

P62055     IF  (PRM-UPDATE-OPTION = "3" OR "4")
P62055         SET MID-CYCLE           TO TRUE.

005100     IF  (PRM-PROC-GROUP    NOT = SPACES)
005200     AND (PRM-PROCESS-LEVEL NOT = SPACES)
005300         MOVE 104                TO CRT-ERROR-NBR
005400         PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-END.
005500
005600     IF (PRM-PROC-GROUP NOT = SPACES)
005700         MOVE PRM-PROC-GROUP     TO DB-PROC-GROUP
005800         MOVE PRPSET1-PROC-GROUP TO WS-DB-BEG-RNG
005900         PERFORM 850-FIND-BEGRNG-PRPSET1
006000         IF (PRPROCGRP-NOTFOUND)
006100             MOVE PRM-PROC-GROUP     TO CRT-ERR-VAR1
006200             MOVE 105                TO CRT-ERROR-NBR
006300             PERFORM 780-PRINT-ERROR-MSG
                   GO TO 050-END.
006400
006500     IF (PRM-PROCESS-LEVEL NOT = SPACES)
               MOVE PRM-COMPANY        TO DB-COMPANY
006600         MOVE PRM-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
006700         PERFORM 840-FIND-PRSSET1
006800         IF (PRSYSTEM-NOTFOUND)
006900             MOVE PRM-PROCESS-LEVEL  TO CRT-ERR-VAR1
007000             MOVE 106                TO CRT-ERROR-NBR
007100             PERFORM 780-PRINT-ERROR-MSG
                   GO TO 050-END
007200         ELSE
007300             MOVE PRS-AP-COMPANY     TO WS-AP-COMPANY
007400             PERFORM 051-CHECK-ALL-RUN-FLGS.
007500*
007600     IF (PRM-PROCESS-LEVEL = SPACES)
               MOVE PRM-COMPANY        TO DB-COMPANY
007700         MOVE PRSSET1-COMPANY    TO WS-DB-BEG-RNG
007800         PERFORM 850-FIND-BEGRNG-PRSSET1
007900         PERFORM 051-CHECK-ALL-RUN-FLGS
008000             UNTIL (PRSYSTEM-NOTFOUND)
P19684             OR    (ERROR-FOUND).
008100
           IF (PARAMETERS-IN-ERROR)
               GO TO 050-END.

GMDIST     IF (PRM-PAY-CLASS NOT = SPACES)
GMDIST         MOVE PRM-COMPANY               TO DB-COMPANY
GMDIST         MOVE PRM-PAY-CLASS             TO DB-PAY-CLASS
GMDIST         PERFORM 840-FIND-PCLSET1
GMDIST         IF (PAYCLASS-NOTFOUND)
GMDIST             MOVE 165                   TO CRT-ERROR-NBR
GMDIST             PERFORM 780-PRINT-ERROR-MSG
GMDIST             GO TO 050-END.

J99021* GHR COMPANY
J99021     MOVE SPACES                     TO PRPRD-VALID-GHR-CO.
J99021
J99021     MOVE PRM-COMPANY                TO DB-COMPANY.
J99021     MOVE HRWS-US-WORK-COUNTRY       TO DB-COUNTRY-CODE.
J99021     MOVE ZEROES                     TO DB-PAYROLL-YEAR.
J99021     MOVE "T"                        TO DB-RECORD-TYPE.
J99021     MOVE SPACES                     TO DB-REPORT-ENTITY
J99021                                        DB-TAX-ID-CODE.
J99021     MOVE ZEROES                     TO DB-EMPLOYEE.
J99021     MOVE "C3"                       TO DB-TOPIC.
J99021     MOVE 9001                       TO DB-FLD-NBR.
J99021     MOVE ZEROES                     TO DB-SEQ-NBR.
J99021     PERFORM 840-FIND-RGPSET1.
J99021     IF  (PRREGPARM-FOUND)
J99021         MOVE RGP-A-VALUE            TO PRPRD-VALID-GHR-CO
J99021     END-IF.
J99021
008200     PERFORM 840-KFIND-BNCSET1.
008300     IF (BNCOMPANY-KFOUND)
008400         MOVE WS-TRUE            TO WS-BENEFITS-USER-SW
008500     ELSE
008600         MOVE WS-FALSE           TO WS-BENEFITS-USER-SW.
J99021   
J99021     IF  (VALID-GHR-COMPANY)
J99021     AND (NOT INTERACTIVE)
J99021         PERFORM 075-LOAD-GHR-PLAN-TBL.
008700*
008800     IF  (BENEFITS-USER)
J99021     AND (INVALID-GHR-COMPANY)
           AND (NOT INTERACTIVE)
008900         PERFORM 065-LOAD-PLAN-TBL.
009000
009100     IF (PRM-GL-DATE             < PRM-PREV-GL-DATE)
009200         MOVE 107                TO CRT-ERROR-NBR
009300         PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-END.
009400*
009500     IF (PRM-PCT-TO-PREV         > 100)
009600         MOVE 108                TO CRT-ERROR-NBR
009700         PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-END.
009800
009900     IF  (PRM-PCT-TO-PREV        NOT = ZEROS)
010000     AND (PRM-PREV-GL-DATE       = ZEROS)
010100         MOVE 109                TO CRT-ERROR-NBR
010200         PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-END.
010300
010400     IF  (PRM-TR-DATE      NOT = ZEROS)
010500     AND (PRM-PREV-GL-DATE = ZEROS)
010600         MOVE 109                TO CRT-ERROR-NBR
010700         PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-END.
010800
010900     IF  (PRM-PCT-TO-PREV NOT = ZEROS)
011000     AND (PRM-TR-DATE     NOT = ZEROS)
011100         MOVE 122                TO CRT-ERROR-NBR
011200         PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-END.

DKR        PERFORM 054-CHECK-PRMONITOR.
           IF  (ERROR-FOUND)
               GO TO 050-END.

P03983*****edit prm gl date against valid entry dates for pr system code
           MOVE PRM-COMPANY        TO IFSCWS-COMPANY.
           MOVE "PR"               TO IFSCWS-SYSTEM.
           MOVE PRM-GL-DATE        TO IFSCWS-POSTING-DATE
J51646                                IFACWS-POST-DATE
J51646                                IFACWS-TRAN-DATE.
           INITIALIZE                 IFSCWS-SKIP-DATE-EDIT
                                      IFSCWS-VALIDATE.
           PERFORM 620-EDIT-SYSTEM-CODE-60.
           IF (ERROR-FOUND)
               PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-END.

P03983*****edit prm prev gl date against valid entry dates for pr system code
           IF  (PRM-PREV-GL-DATE NOT = ZEROES)
               MOVE PRM-COMPANY        TO IFSCWS-COMPANY
               MOVE "PR"               TO IFSCWS-SYSTEM
               MOVE PRM-PREV-GL-DATE   TO IFSCWS-POSTING-DATE
               INITIALIZE                 IFSCWS-SKIP-DATE-EDIT
                                          IFSCWS-VALIDATE
               PERFORM 620-EDIT-SYSTEM-CODE-60
               IF (ERROR-FOUND)
                   PERFORM 780-PRINT-ERROR-MSG
                   GO TO 050-END.
011300
           MOVE UPDATE-OPTION-SW       TO PRPRD-UPDATE-OPTION-SW.
011500     MOVE PRM-TR-DATE            TO PRPRD-TR-DATE.
011600     MOVE PRM-GL-DATE            TO PRPRD-GL-DATE.
011700     MOVE PRM-PREV-GL-DATE       TO PRPRD-PREV-GL-DATE.
011800     MOVE PRM-DIST-OPTION        TO PRPRD-DIST-OPTION.
  1900     MOVE "Y"                    TO PRPRD-PR197.
           MOVE PRM-PROCESS-LEVEL      TO WS-PROCESS-LEVEL.
RETROP     MOVE PRM-RETRO-OPTION       TO PRPRD-RETRO-OPTION.
012000
012100     MOVE 115                    TO CRT-MSG-NBR.
012200     PERFORM 790-GET-MSG.
012300     MOVE CRT-MESSAGE            TO WS-C-MESSAGE.
012400     MOVE 116                    TO CRT-MSG-NBR.
012500     PERFORM 790-GET-MSG.
012600     MOVE CRT-MESSAGE            TO WS-A-MESSAGE.
012700     MOVE 117                    TO CRT-MSG-NBR.
012800     PERFORM 790-GET-MSG.
012900     MOVE CRT-MESSAGE            TO WS-E-MESSAGE.
013000
015500     MOVE "PRMSG"                TO CRT-ERROR-CAT.
015600     MOVE 100                    TO CRT-MSG-NBR.
015700     PERFORM 790-GET-MSG.
015800     MOVE CRT-MESSAGE            TO WS-EMPLOYEE-LIT.
015900
016000     MOVE "PRMSG"                TO CRT-ERROR-CAT.
016100     MOVE 101                    TO CRT-MSG-NBR.
016200     PERFORM 790-GET-MSG.
016300     MOVE CRT-MESSAGE            TO WS-DED-CODE-LIT.
016400
016500     MOVE "PRMSG"                TO CRT-ERROR-CAT.
016600     MOVE 102                    TO CRT-MSG-NBR.
016700     PERFORM 790-GET-MSG.
016800     MOVE CRT-MESSAGE            TO WS-JOB-CODE-LIT.
016900
017000     MOVE "PRMSG"                TO CRT-ERROR-CAT.
017100     MOVE 103                    TO CRT-MSG-NBR.
017200     PERFORM 790-GET-MSG.
017300     MOVE CRT-MESSAGE            TO WS-JOB-CLASS-LIT.
017400
017500     MOVE "PRMSG"                TO CRT-ERROR-CAT.
017600     MOVE 104                    TO CRT-MSG-NBR.
017700     PERFORM 790-GET-MSG.
017800     MOVE CRT-MESSAGE            TO WS-ATTEND-CODE-LIT.
017900
018000     MOVE "PRMSG"                TO CRT-ERROR-CAT.
018100     MOVE 105                    TO CRT-MSG-NBR.
018200     PERFORM 790-GET-MSG.
018300     MOVE CRT-MESSAGE            TO WS-PAY-CODE-LIT.
018400
           MOVE "PRMSG"                TO CRT-ERROR-CAT.
           MOVE 106                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WS-POSITION-LIT.

           MOVE 150                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WS-MESSAGE-150.

018500     INITIALIZE CRT-ERROR-CAT
                      CRT-MESSAGE.
018600
018700 050-END.
018800
018900****************************************************************
019000 051-CHECK-ALL-RUN-FLGS          SECTION.
019100****************************************************************
019200 051-START.
019300*
019400     IF (PRM-PROC-GROUP NOT = SPACES)
019500         MOVE PRS-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
019600         PERFORM 840-KFIND-PRPSET1
019700         IF (PRPROCGRP-KNOTFOUND)
019800             GO TO 051-FIND-NEXT.
019900
P46341     IF  (CKP-RESTART-INFO       = SPACES)
020000     AND (PRM-PROCESS-LEVEL      = SPACES)
020100     AND (PRS-PROCESS-LEVEL      = SPACES)
P19684         IF  (PRS-PR199-RUN-FLG  = "R")
P19684         AND (PRM-PROC-GROUP     = SPACES)
P19684         AND (NOT INTERACTIVE)
P19684             MOVE PRS-COMPANY        TO CRT-ERR-VAR1
P19684             MOVE 164                TO CRT-ERROR-NBR
P19684             PERFORM 780-PRINT-ERROR-MSG
P19684         END-IF
020200         GO TO 051-FIND-NEXT.
020300*
020400     MOVE PRS-COMPANY            TO CRT-COMPANY.
020500     MOVE PRS-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL.
020600     PERFORM 900-PROC-LEV-SECURED.
020700     IF (CRT-PROC-LEV-SECURED = "Y")
020800         MOVE PRS-PROCESS-LEVEL  TO CRT-ERR-VAR1
020900         MOVE 119                TO CRT-MSG-NBR
021000         PERFORM 780-DISPLAY-MSG
021100         GO TO 051-FIND-NEXT.
021200
021300     MOVE PRM-COMPANY            TO DB-COMPANY.
021400
021500     IF (PRM-PROCESS-LEVEL NOT = SPACES)
021600         MOVE PRM-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
021700                                    WS-PYM-PROCESS-LEVEL
021800     ELSE
021900         MOVE PRS-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
022000                                    WS-PYM-PROCESS-LEVEL.
022100*
022200     MOVE WS-FALSE               TO WS-SYSTEM-CHECKS-FLAG
022300                                    WS-CHECKS-FLAG.
022400
022500     MOVE PYMSET5-PROCESS-LEVEL  TO WS-DB-BEG-RNG.
P62055     IF  (MID-CYCLE)
P62055         MOVE SPACES             TO FILTER-STRING
P62055         STRING
P62055         "(PYM-CHECK-TYPE != ?) AND (PYM-CHECK-TYPE != ?) AND "
P62055         "(PYM-CHECK-TYPE != ?) AND (PYM-CHECK-TYPE != ?)"
P62055             DELIMITED BY SIZE
P62055             INTO FILTER-STRING
P62055         PERFORM 890-CREATE-FILTER
P62055         MOVE "S"                TO ALPHANUM-FILTER-VALUE
P62055         PERFORM 890-SET-ALPHANUM-FILTER-VALUE
P62055         MOVE "P"                TO ALPHANUM-FILTER-VALUE
P62055         PERFORM 890-SET-ALPHANUM-FILTER-VALUE
P62055         MOVE "A"                TO ALPHANUM-FILTER-VALUE
P62055         PERFORM 890-SET-ALPHANUM-FILTER-VALUE
P62055         MOVE "J"                TO ALPHANUM-FILTER-VALUE
P62055         PERFORM 890-SET-ALPHANUM-FILTER-VALUE
P62055         PERFORM 850-FILTER-BEGRNG-PYMSET5
P62055         IF  (PAYMASTR-FOUND)
P62055             MOVE WS-TRUE        TO WS-CHECKS-FLAG
P62055         END-IF
P62055     ELSE
022600         PERFORM 850-FIND-BEGRNG-PYMSET5
022700         PERFORM 052-EDIT-FOR-CHECKS
022800             UNTIL (WS-SYSTEM-CHECKS-FOUND)
022900             OR    (PAYMASTR-NOTFOUND).
023000
023100     IF  (WS-CHECKS-FOUND)
023300     AND (NOT INTERACTIVE)
023100         IF  ((PRS-INCLUDE-TP135   = "Y")
023200          AND (PRS-TP135-RUN-FLG   = "R"))
023400         MOVE DB-PROCESS-LEVEL       TO CRT-ERR-VAR1
023500         MOVE 110                    TO CRT-ERROR-NBR
023600         PERFORM 780-PRINT-ERROR-MSG
               GO TO 051-FIND-NEXT.
023700
           IF  (WS-SYSTEM-CHECKS-FOUND)
           AND (REPORT-ONLY)
           AND (PRS-PR140-RUN-FLG     NOT = "*")
             IF (NOT INTERACTIVE)
               MOVE DB-PROCESS-LEVEL   TO CRT-ERR-VAR1
               MOVE 110                TO CRT-ERROR-NBR
               PERFORM 780-PRINT-ERROR-MSG
               GO TO 051-FIND-NEXT
             END-IF
           ELSE
023800     IF  (WS-SYSTEM-CHECKS-FOUND)
           AND (UPDATING)
023900     AND ((PRS-PR140-RUN-FLG     NOT = "*")
024000     OR   (PRS-PR160-RUN-FLG     NOT = "*"))
             IF (NOT INTERACTIVE)
024100         MOVE DB-PROCESS-LEVEL   TO CRT-ERR-VAR1
024200         MOVE 110                TO CRT-ERROR-NBR
024300         PERFORM 780-PRINT-ERROR-MSG
               GO TO 051-FIND-NEXT
             END-IF
024400     ELSE
024500     IF  (WS-CHECKS-FOUND)
024600     AND ((PRS-PR140-RUN-FLG     = "R")
024700     OR   (PRS-PR132-RUN-FLG     = "R")
024800     OR   (PRS-PR160-RUN-FLG     = "R"))
             IF (NOT INTERACTIVE)
024900         MOVE DB-PROCESS-LEVEL   TO CRT-ERR-VAR1
025000         MOVE 110                TO CRT-ERROR-NBR
025100         PERFORM 780-PRINT-ERROR-MSG
               GO TO 051-FIND-NEXT.
025200
025300     IF  (WS-SYSTEM-CHECKS-FOUND)
025400     AND (PRS-PR160-RUN-FLG = "*")
025500         MOVE BFLSET1-COMPANY    TO WS-DB-BEG-RNG
025600         PERFORM 850-FIND-BEGRNG-BFLSET1
025700         PERFORM 053-EDIT-BANK-ACCTS
025800             UNTIL (BANKFILE-NOTFOUND).
025900*
           MOVE PRM-COMPANY            TO DB-COMPANY.
           MOVE PRS-PROCESS-LEVEL      TO DB-PROCESS-LEVEL.
P07089     MOVE SPACES                 TO DB-BANK-CODE.
           PERFORM 840-FIND-PMNSET1.

           IF  (PRMONITOR-FOUND)
           AND (PMN-PR198-RUN-FLG = "R")
           AND (NOT INTERACTIVE)
               MOVE DB-PROCESS-LEVEL   TO CRT-ERR-VAR1
               MOVE 154                TO CRT-ERROR-NBR
               PERFORM 780-PRINT-ERROR-MSG
               GO TO 051-FIND-NEXT.

P41283     IF  (CKP-RESTART-INFO  = SPACES)
P19684     AND (PRS-PR199-RUN-FLG = "R")
P19684     AND (NOT INTERACTIVE)
P19684         MOVE PRS-PROCESS-LEVEL  TO CRT-ERR-VAR1
P19684         MOVE 163                TO CRT-ERROR-NBR
P19684         PERFORM 780-PRINT-ERROR-MSG
P19684         GO TO 051-FIND-NEXT.
P19684
026000 051-FIND-NEXT.
026100*
026200     IF (PRM-PROCESS-LEVEL = SPACES)
026300         PERFORM 860-FIND-NXTRNG-PRSSET1.
026400*
026500 051-END.
026600     EXIT.
026700****************************************************************
026800 052-EDIT-FOR-CHECKS             SECTION.
026900****************************************************************
027000 052-START.
027100*
027200     IF  (PAYMASTR-FOUND)
027300     AND (PYM-COMPANY       = PRM-COMPANY)
027400     AND (PYM-PROCESS-LEVEL = WS-PYM-PROCESS-LEVEL)
027500         IF (PYM-CHECK-TYPE = "S" OR "P" OR "A")
027600             MOVE WS-TRUE        TO WS-SYSTEM-CHECKS-FLAG
027700         ELSE
027800         IF (PYM-CHECK-TYPE NOT = "J")
027900             MOVE WS-TRUE        TO WS-CHECKS-FLAG.
028000*
028100     PERFORM 860-FIND-NXTRNG-PYMSET5.
028200*
028300 052-END.
028400     EXIT.
028500****************************************************************
028600 053-EDIT-BANK-ACCTS             SECTION.
028700****************************************************************
028800 053-START.
028900*
029000     MOVE BFL-BANK-CODE          TO DB-BANK-CODE.
029100     MOVE PRS-PROCESS-LEVEL      TO DB-PROCESS-LEVEL.
029200     MOVE WS-FALSE               TO WS-CHECK-FOUND-SW.
029300     MOVE PYMSET2-PROCESS-LEVEL  TO WS-DB-BEG-RNG.
029400     PERFORM 850-FIND-BEGRNG-PYMSET2.
029500     PERFORM
029600         UNTIL (PAYMASTR-NOTFOUND)
029700         OR    (ERROR-FOUND)
029800         OR    (WS-CHECK-FOUND)
029900
P84522         IF (PYM-CHECK-TYPE = "S" OR "A" OR "P" OR "H")
030100             MOVE PYM-CHECK-ID           TO DB-CHECK-ID
030200             PERFORM 840-KFIND-PCHSET5
030300             IF (PRCHECK-KNOTFOUND)
030400                 MOVE PYM-PROCESS-LEVEL  TO CRT-ERR-VAR1
030500                 MOVE PYM-BANK-CODE      TO CRT-ERR-VAR2
030600                 MOVE 121                TO CRT-ERROR-NBR
030700                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 053-FIND-NEXT
030800             ELSE
030900                 MOVE WS-TRUE            TO WS-CHECK-FOUND-SW
031000             END-IF
031100         ELSE
031200             PERFORM 860-FIND-NXTRNG-PYMSET2
031300         END-IF
031400     END-PERFORM.
031500
       053-FIND-NEXT.

031600     PERFORM 860-FIND-NXTRNG-BFLSET1.
031700*
031800 053-END.
031900     EXIT.
DKR   ****************************************************************
       054-CHECK-PRMONITOR             SECTION.
      ****************************************************************
       054-START.
      *
           INITIALIZE                         DB-PROCESS-LEVEL
                                              DB-PROC-GROUP
                                              WS-DB-BEG-RNG.
           IF  (PRM-PROCESS-LEVEL = SPACES)
           AND (PRM-PROC-GROUP    = SPACES)
               MOVE PMNSET2-PROC-GROUP TO  WS-DB-BEG-RNG
               PERFORM 054-FIND-PRMONITOR
P04413         THRU    054-FIND-END
           ELSE
               IF  (PRM-PROC-GROUP = SPACES)
                   MOVE PRM-PROCESS-LEVEL     TO DB-PROCESS-LEVEL
                   MOVE PMNSET2-PROCESS-LEVEL TO WS-DB-BEG-RNG
                   PERFORM 054-FIND-PRMONITOR
P04413             THRU    054-FIND-END
               ELSE  
                   MOVE PRM-PROC-GROUP     TO DB-PROC-GROUP
                   MOVE PRPSET1-PROC-GROUP TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-PRPSET1
                   PERFORM
                       UNTIL (PRPROCGRP-NOTFOUND)
                       IF  (PRP-PROCESS-LEVEL NOT = SPACES)
                           MOVE PRP-PROCESS-LEVEL    TO DB-PROCESS-LEVEL
                           MOVE PMNSET2-PROCESS-LEVEL TO WS-DB-BEG-RNG
                           PERFORM 054-FIND-PRMONITOR
P04413                     THRU    054-FIND-END
                       END-IF
                       PERFORM 860-FIND-NXTRNG-PRPSET1
                   END-PERFORM 
               END-IF
           END-IF.
           GO TO 054-END.
      *
       054-FIND-PRMONITOR.
           MOVE "2"     TO DB-CYCLE-PROCESS.
           MOVE "PR189" TO DB-PROC-GROUP.
           MOVE SPACES  TO DB-BANK-CODE.
           PERFORM 850-FIND-BEGRNG-PMNSET2.
           PERFORM
               UNTIL (PRMONITOR-NOTFOUND)
               OR    (ERROR-FOUND)

               IF  (PMN-PR160-RUN-FLG NOT = "*")
                   MOVE PMN-BANK-CODE      TO CRT-ERR-VAR1
                   MOVE PMN-PROCESS-LEVEL  TO CRT-ERR-VAR2
                   MOVE 157                TO CRT-ERROR-NBR
                   PERFORM 780-PRINT-ERROR-MSG
               ELSE
                   PERFORM 860-FIND-NXTRNG-PMNSET2
               END-IF
           END-PERFORM.
P04413 054-FIND-END.
      *
       054-END.
           EXIT.
032000******************************************************************
032100 065-LOAD-PLAN-TBL               SECTION   .
032200******************************************************************
032300 065-START.
032400
032500     MOVE WS-FALSE               TO WS-TBL-LIMIT-SW.

           INITIALIZE WS-PLN-TBL-1
                      WS-PLN-TBL-2
                      WS-PLN-TBL-SIZE
                      WS-PLN-DB-DC-SIZE
J72349                WS-409A-AMT-TABLE
                      I1.
032900
033000     MOVE PRM-COMPANY            TO DB-COMPANY.
033100     MOVE PLNSET1-COMPANY        TO WS-DB-BEG-RNG.
033200     PERFORM 850-FIND-BEGRNG-PLNSET1.
033300
033400     PERFORM 070-LOAD-PLANS-INTO-TBL
033500     THRU    070-END
033600         UNTIL (PLAN-NOTFOUND)
033700         OR    (WS-TBL-OVER-LIMIT).
033800
033900     GO TO 065-END.
034000
034100******************************************************************
034200 070-LOAD-PLANS-INTO-TBL.
034300******************************************************************
034400
034500     IF (PLN-WAIVE-FLAG = "Y")
034600         GO TO 070-NEXT-PLAN.
034700
034800     ADD 1                       TO I1.
034900
035000     IF (I1 > WS-TBL-MAX)
035100         MOVE 111                TO CRT-ERROR-NBR
035200         PERFORM 780-PRINT-ERROR-MSG
035300         MOVE WS-TRUE            TO WS-TBL-LIMIT-SW
035400         GO TO 070-END.
035500
035600     IF (PLN-PLAN-TYPE = "DB" OR "DC")
035700         ADD 1                   TO WS-PLN-DB-DC-SIZE.
035800
035900     ADD 1                       TO WS-PLN-TBL-SIZE.
036000
036100     MOVE PLN-PLAN-TYPE          TO WS-PLN-PLAN-TYPE (I1).
036200     MOVE PLN-PLAN-CODE          TO WS-PLN-PLAN-CODE (I1).
036300     MOVE PLN-START-DATE         TO WS-PLN-START-DATE (I1).
036400     MOVE PLN-STOP-DATE          TO WS-PLN-STOP-DATE (I1).
036500     MOVE PLN-VEST-FROM-DATE     TO WS-PLN-FROM-DATE (I1).
036600     MOVE PLN-VEST-DATE          TO WS-PLN-VEST-DATE (I1).
036700     MOVE PLN-VEST-BEF-FLG       TO WS-PLN-VEST-BEF-FLG (I1).
036800     MOVE PLN-COMP-CLASS         TO WS-PLN-COMP-CLASS (I1).
036900     MOVE PLN-VEST-CLASS         TO WS-PLN-VEST-CLASS (I1).
P81387     MOVE PLN-CODA-CONTRIB       TO WS-PLN-CODA-CONTRIB (I1).
037000     MOVE PLN-PRE-DED-CODE-A     TO WS-PLN-DED-CODE (I1 1).
037100     MOVE PLN-AFT-LMT-DED-A      TO WS-PLN-DED-CODE (I1 2).
037200     MOVE PLN-AFT-DED-CODE-A     TO WS-PLN-DED-CODE (I1 3).
037300     MOVE PLN-CMP-DED-CODE-A     TO WS-PLN-DED-CODE (I1 4).
037400     MOVE PLN-PRE-DED-CODE-P     TO WS-PLN-DED-CODE (I1 5).
037500     MOVE PLN-AFT-LMT-DED-P      TO WS-PLN-DED-CODE (I1 6).
037600     MOVE PLN-AFT-DED-CODE-P     TO WS-PLN-DED-CODE (I1 7).
037700     MOVE PLN-CMP-DED-CODE-P     TO WS-PLN-DED-CODE (I1 8).
037800     MOVE PLN-PRE-DED-MTCH-A     TO WS-PLN-DED-CODE (I1 9).
037900     MOVE PLN-AFT-LMT-MTCH-A     TO WS-PLN-DED-CODE (I1 10).
038000     MOVE PLN-AFT-DED-MTCH-A     TO WS-PLN-DED-CODE (I1 11).
038100     MOVE PLN-PRE-DED-MTCH-P     TO WS-PLN-DED-CODE (I1 12).
038200     MOVE PLN-AFT-LMT-MTCH-P     TO WS-PLN-DED-CODE (I1 13).
038300     MOVE PLN-AFT-DED-MTCH-P     TO WS-PLN-DED-CODE (I1 14).
038400     MOVE PLN-FLEX-DED-CODE      TO WS-PLN-DED-CODE (I1 15).
038500
038600 070-NEXT-PLAN.
038700
038800     PERFORM 860-FIND-NXTRNG-PLNSET1.
038900
039000 070-END.
039100
039200******************************************************************
039300 065-END.
J99021******************************************************************
J99021 075-LOAD-GHR-PLAN-TBL               SECTION   .
J99021******************************************************************
J99021 075-START.
J99021
J99021     MOVE WS-FALSE               TO WS-TBL-LIMIT-SW.
J99021
J99021     INITIALIZE WS-PLN-TBL-1
J99021                WS-PLN-TBL-2
J99021                WS-PLN-TBL-SIZE
J99021                WS-PLN-DB-DC-SIZE
J99021                I1.
J99021
J99021     MOVE PRM-COMPANY            TO DB-COMPANY.
J99021     MOVE "BN"                   TO DB-TYPE.
J99021     MOVE "DC"                   TO DB-SUB-TYPE. 
J99021     MOVE GHRSET1-SUB-TYPE       TO WS-DB-BEG-RNG.
J99021     PERFORM 850-FIND-BEGRNG-GHRSET1.
J99021     PERFORM
J99021        UNTIL (GHRHDRDATA-NOTFOUND)
J99021        OR    (WS-TBL-OVER-LIMIT)
J99021           ADD 1 TO I1         
J99021           IF (I1 > WS-TBL-MAX)
J99021               MOVE 111                TO CRT-ERROR-NBR
J99021               PERFORM 780-PRINT-ERROR-MSG
J99021               MOVE WS-TRUE            TO WS-TBL-LIMIT-SW
J99021               GO TO 075-END
J99021           END-IF
J99021           ADD 1  TO WS-PLN-TBL-SIZE
J99021                     WS-PLN-DB-DC-SIZE
J99021           MOVE GHR-SUB-TYPE       TO WS-PLN-PLAN-TYPE (I1)
J99021           MOVE "US"               TO DB-COUNTRY-CODE
J99021           MOVE "PR"               TO DB-TOPIC-SOURCE 
J99021           MOVE "GH"               TO DB-TOPIC
J99021           MOVE GHR-CODE           TO DB-CODE
J99021           MOVE ZEROES             TO DB-FLD-NBR      
J99021           MOVE ZEROES             TO DB-SEQ-NBR
J99021           MOVE GHSSET1-CODE       TO WS-DB-BEG-RNG
J99021           PERFORM 850-FIND-BEGRNG-GHSSET1
J99021           PERFORM
J99021              UNTIL (GHRDTLDATA-NOTFOUND)
J99021                IF (GHS-FLD-NBR = 9001)                      
J99021                    MOVE GHS-A-VALUE TO WS-PLN-DED-CODE (I1 1)
J99021                END-IF
J99021                IF (GHS-FLD-NBR = 9002)                      
J99021                    MOVE GHS-A-VALUE TO WS-PLN-DED-CODE (I1 5)
J99021                END-IF
J99021                IF (GHS-FLD-NBR = 9003)                      
J99021                    MOVE GHS-A-VALUE TO WS-PLN-DED-CODE (I1 3)
J99021                END-IF
J99021                IF (GHS-FLD-NBR = 9004)                      
J99021                    MOVE GHS-A-VALUE TO WS-PLN-DED-CODE (I1 7)
J99021                END-IF
J99021                IF (GHS-FLD-NBR = 9005)                      
J99021                    MOVE GHS-A-VALUE TO WS-PLN-DED-CODE (I1 2)
J99021                END-IF
J99021                IF (GHS-FLD-NBR = 9009)                      
J99021                    MOVE GHS-A-VALUE TO WS-PLN-DED-CODE (I1 6)
J99021                END-IF
J87945                IF (GHS-FLD-NBR = 9010)                      
J87945                    MOVE GHS-A-VALUE TO WS-PLN-DED-CODE (I1 4)
J87945                END-IF
J87945                IF (GHS-FLD-NBR = 9011)                      
J87945                    MOVE GHS-A-VALUE TO WS-PLN-DED-CODE (I1 8)
J87945                END-IF
J87945                IF (GHS-FLD-NBR = 9012)                      
J87945                    MOVE GHS-A-VALUE TO WS-PLN-DED-CODE (I1 9)
J87945                END-IF
J87945                IF (GHS-FLD-NBR = 9016)                      
J87945                    MOVE GHS-A-VALUE TO WS-PLN-DED-CODE (I1 10)
J87945                END-IF
J87945                IF (GHS-FLD-NBR = 9014)                      
J87945                    MOVE GHS-A-VALUE TO WS-PLN-DED-CODE (I1 11)
J87945                END-IF
J87945                IF (GHS-FLD-NBR = 9013)                      
J87945                    MOVE GHS-A-VALUE TO WS-PLN-DED-CODE (I1 12)
J87945                END-IF
J87945                IF (GHS-FLD-NBR = 9017)                      
J87945                    MOVE GHS-A-VALUE TO WS-PLN-DED-CODE (I1 13)
J87945                END-IF
J87945                IF (GHS-FLD-NBR = 9015)                      
J87945                    MOVE GHS-A-VALUE TO WS-PLN-DED-CODE (I1 14)
J87945                END-IF
J99021                IF (GHS-FLD-NBR = 9006)                      
J99021                    MOVE GHS-A-VALUE TO WS-PLN-COMP-CLASS (I1) 
J99021                END-IF
J99021                IF (GHS-FLD-NBR = 9008)                      
J99021                    MOVE GHS-A-VALUE TO WS-PLN-PLAN-CODE (I1)
J99021                END-IF
J99021             PERFORM 860-FIND-NXTRNG-GHSSET1
J99021           END-PERFORM 
J99021         PERFORM 860-FIND-NXTRNG-GHRSET1
J99021     END-PERFORM.
J99021
J99021******************************************************************
J99021 075-END.
039400******************************************************************
039500****************************************************************
039600 100-PROGRAM-CONTROL             SECTION.
039700****************************************************************
039800 100-START.
039900*
      * NEEDS TO CHANGE TO PR197
040000     IF  (PRM-PROCESS-LEVEL = SPACES)
           AND (PRM-PROC-GROUP    = SPACES)
           AND (UPDATING)
040100         PERFORM 910-AUDIT-BEGIN
               MOVE PRM-COMPANY           TO DB-COMPANY
040200         MOVE SPACES                TO DB-PROCESS-LEVEL
040300         PERFORM 840-MODIFY-PRSSET1
040400         MOVE "R"                   TO PRS-PR199-RUN-FLG
040500         PERFORM 820-STORE-PRSYSTEM
040600         PERFORM 925-AUDIT-END.
040700
040800     MOVE 114                    TO CRT-MSG-NBR.
040900     PERFORM 780-DISPLAY-MSG.
041000*
041100     PERFORM 840-FIND-CKPSET1.
041200     IF (CKP-RESTART-INFO = SPACES)
041300         INITIALIZE WS-RESTART-INFO
041400     ELSE
041500         MOVE CKP-RESTART-INFO   TO WS-RESTART-INFO
042100         MOVE WS-RS-AX-REC-COUNT TO WS-AX-REC-COUNT.

           IF (WS-RS-PHASE-COUNTER = 1)
               MOVE WS-TRUE            TO PRPRD-PR197-RS-PHASE-1-SW
           ELSE
               MOVE WS-FALSE           TO PRPRD-PR197-RS-PHASE-1-SW.

042400     IF (WS-RS-PHASE-COUNTER = 2)
042500         MOVE WS-TRUE            TO WS-RESTARTING-PHASE-2-SW
042600     ELSE
042700         MOVE WS-FALSE           TO WS-RESTARTING-PHASE-2-SW.
042800
           IF (WS-RS-PHASE-COUNTER = 3)
               MOVE WS-TRUE            TO WS-RESTARTING-PHASE-3-SW
           ELSE
               MOVE WS-FALSE           TO WS-RESTARTING-PHASE-3-SW.

           IF (WS-RS-PHASE-COUNTER = 4)
               MOVE WS-TRUE            TO WS-RESTARTING-PHASE-4-SW
           ELSE
               MOVE WS-FALSE           TO WS-RESTARTING-PHASE-4-SW.

P23120*    IF  (BENEFITS-USER) 
J99021     IF  ((BENEFITS-USER) 
J99021     OR   (VALID-GHR-COMPANY))
           AND (WS-RS-PHASE-COUNTER = 2 OR 3)
               IF (WS-RS-BENWORK-NAME = SPACES)
                   PERFORM 900-BUILD-TMP-FILE-NAME
                   MOVE WS-TMP-FILE        TO BENWORK-NAME
                                              WS-RS-BENWORK-NAME
               ELSE
                   MOVE WS-RS-BENWORK-NAME TO BENWORK-NAME
                   PERFORM 9810-OPEN-IO-BENWORK.

           IF (WS-RS-PHASE-COUNTER = 2)
                MOVE WS-RS-WORK3-NAME     TO WS-WORK3-FILE-NAME
GMDIST          MOVE WS-RS-GMWORK-NAME    TO WS-GMWORK-FILE-NAME.

P79800     IF (WS-RS-PHASE-COUNTER = 3)
P79800          MOVE WS-RS-GLERR-WORK-NAME TO WS-GLERR-WORK-FILE-NAME.

           INITIALIZE PRPRD-CA-EHT-TABLE.

043400     GO TO 100-PHASE-1
043500           100-PHASE-2
043600           100-PHASE-3
043700           100-PHASE-4           DEPENDING ON WS-RS-PHASE-COUNTER.
043900*
044000     PERFORM 910-AUDIT-BEGIN.
044100
           IF (UPDATING)
044200         PERFORM 110-SET-RUN-FLAGS.
044300*
044400     MOVE WS-SYSTEM-DATE-YMD     TO WS-RS-RUN-DATE.
044600     MOVE HHMMSS                 TO WS-RS-RUN-TIME.
044800     MOVE 1                      TO WS-RS-PHASE-COUNTER.
044900*
045000     PERFORM 840-MODIFY-CKPSET1.
045100     MOVE WS-RESTART-INFO        TO CKP-RESTART-INFO.
045200     PERFORM 820-STORE-CKPOINT.
045300     PERFORM 925-AUDIT-END.
045400*
045500 100-PHASE-1.
045600*
           IF (PRPRD-PR197-RS-PHASE-1)
               MOVE 131                TO CRT-MSG-NBR
           ELSE
               MOVE 130                TO CRT-MSG-NBR.
           PERFORM 780-DISPLAY-MSG.

053200     IF  (BENEFITS-USER)
J99021     OR  (VALID-GHR-COMPANY)
      *** RE-USE THE PREVIOUS RUN'S BENWORK FILENAME ***
               IF (WS-RS-BENWORK-NAME = SPACES)
                   PERFORM 900-BUILD-TMP-FILE-NAME
                   MOVE WS-TMP-FILE        TO BENWORK-NAME
                                              WS-RS-BENWORK-NAME
               ELSE
                   MOVE WS-RS-BENWORK-NAME TO BENWORK-NAME
               END-IF
053900         PERFORM 9800-OPEN-OUTPUT-BENWORK
054000         PERFORM 9900-CLOSE-BENWORK
054100         PERFORM 9810-OPEN-IO-BENWORK.
054200
054300     PERFORM 910-AUDIT-BEGIN.
054400     PERFORM 840-MODIFY-CKPSET1.
054500     MOVE WS-RESTART-INFO        TO CKP-RESTART-INFO.
054600     PERFORM 820-STORE-CKPOINT.
054700     PERFORM 925-AUDIT-END.
054800
           PERFORM 900-BUILD-TMP-FILE-NAME.
           MOVE WS-TMP-FILE            TO WS-WORK3-FILE-NAME.
           MOVE WS-WORK3-FILE-NAME     TO WS-RS-WORK3-NAME.
           OPEN OUTPUT WORK3-FILE.
GMDIST     PERFORM 900-BUILD-TMP-FILE-NAME.
GMDIST     MOVE WS-TMP-FILE            TO WS-GMWORK-FILE-NAME.
GMDIST     MOVE WS-GMWORK-FILE-NAME    TO WS-RS-GMWORK-NAME.
GMDIST     OPEN OUTPUT GMWORK-FILE.

           MOVE ZEROS                  TO WS-WR3-SEQ-NBR.

           MOVE 132                    TO CRT-MSG-NBR.
           PERFORM 780-DISPLAY-MSG.

J85817     MOVE "PayrollAdvice"        TO DB-NOUN.
J85817     MOVE SPACES                 TO DB-VERB.
J85817     MOVE SPACES                 TO DB-DIRECTION.
J85817     PERFORM 840-FIND-WFBSET1.
J85817     IF  (WFBOD-FOUND)
J85817     AND (WFB-STATUS = 1)
J85817         SET PR-ADVICE-ENABLED   TO TRUE.

054900     PERFORM 200-DISTRIB-CONTROL.
055000
           MOVE 133                    TO CRT-MSG-NBR.
           PERFORM 780-DISPLAY-MSG.

           CLOSE WORK3-FILE SAVE.
           CLOSE GMWORK-FILE SAVE.

           PERFORM 910-AUDIT-BEGIN.
           PERFORM 840-MODIFY-CKPSET1.
           MOVE 2                  TO WS-RS-PHASE-COUNTER.
           MOVE WS-RESTART-INFO    TO CKP-RESTART-INFO.
           PERFORM 820-STORE-CKPOINT.
           PERFORM 925-AUDIT-END.

       100-PHASE-2.

           IF (RESTARTING-PHASE-2)
               MOVE 135                TO CRT-MSG-NBR
           ELSE
               MOVE 134                TO CRT-MSG-NBR
           END-IF.
           PERFORM 780-DISPLAY-MSG.

P79800     PERFORM 900-BUILD-TMP-FILE-NAME.
P79800     MOVE WS-TMP-FILE                TO WS-GLERR-WORK-FILE-NAME.
P79800     MOVE WS-GLERR-WORK-FILE-NAME    TO WS-RS-GLERR-WORK-NAME.
P79800     OPEN OUTPUT GLERR-WORK-FILE.

PRBRD      PERFORM 1000-OPEN-WORKFLOW-DB.
PRBRD      PERFORM 600-BEGIN-BROADCAST.
PRBRD      MOVE "Y"                     TO PRBRD-DO-BASE-BROADCAST.
PRBRD      MOVE PRM-PRINT-FILE          TO PRBRD-PRINT-FILE.

           PERFORM 300-GL-REPORT.

P79800     CLOSE GLERR-WORK-FILE SAVE.

GMDIST     PERFORM 350-GM-REPORT.

           PERFORM 400-PROCESS-ERR-ERRORS.

PRBRD      IF  (PRBRD-BROADCAST-ENABLED)
PRBRD          PERFORM 605-END-BROADCAST
PRBRD          PERFORM 620-RELEASE-BROADCAST.

057300     PERFORM 910-AUDIT-BEGIN.
057400     PERFORM 840-MODIFY-CKPSET1.
057500     MOVE 3                      TO WS-RS-PHASE-COUNTER.
057800     MOVE WS-RESTART-INFO        TO CKP-RESTART-INFO.
057900     PERFORM 820-STORE-CKPOINT.
058100     PERFORM 900-SAVE-PRINT-FILES.
058300     PERFORM 925-AUDIT-END.
058400*
           MOVE WS-WORK3-FILE-NAME    TO WS-TMP-FILE.
           PERFORM 901-REMOVE-TMP-FILE.
GMDIST     MOVE WS-GMWORK-FILE-NAME   TO WS-TMP-FILE.
GMDIST     PERFORM 901-REMOVE-TMP-FILE.
P79800     MOVE WS-GLERR-WORK-FILE-NAME    TO WS-TMP-FILE.
P79800     PERFORM 901-REMOVE-TMP-FILE.

058500 100-PHASE-3.
058600*
058700     IF (BENEFITS-USER)
J99021     OR (VALID-GHR-COMPANY)
               IF (RESTARTING-PHASE-3)
                   MOVE 137                TO CRT-MSG-NBR
               ELSE
                   MOVE 136                TO CRT-MSG-NBR
               END-IF
               PERFORM 780-DISPLAY-MSG
               PERFORM 600-PROCESS-BN-REPORT
           ELSE
               MOVE 139                    TO CRT-MSG-NBR
               PERFORM 780-DISPLAY-MSG.

110029     IF (PRBRD-BROADCAST-ENABLED)
110029         MOVE 500                    TO PRBRD-ELEMENT-TYPE
110029         MOVE PRM-COMPANY            TO PRBRD-ELEMENT-VALUE
110029         PERFORM 615-END-ELEMENT-TAG
110029         MOVE WFCHWS-TAG-STRING      TO BTL1-TAG-LINE
110029         MOVE BROADCAST-TAG-LINE-1   TO RPT-GROUP-REQUEST
110029         PERFORM 700-PRINT-RPT-GRP
110029     END-IF.

059500     PERFORM 910-AUDIT-BEGIN.
059600     PERFORM 840-MODIFY-CKPSET1.
059700     MOVE 4                          TO WS-RS-PHASE-COUNTER.
059800     MOVE SPACES                     TO WS-RS-BENWORK-NAME.
059900     MOVE WS-RESTART-INFO            TO CKP-RESTART-INFO.
060000     PERFORM 820-STORE-CKPOINT.
060200     PERFORM 900-SAVE-PRINT-FILES.
060400     PERFORM 925-AUDIT-END.
060500
           IF (BENEFITS-USER)
J99021     OR (VALID-GHR-COMPANY)
               MOVE BENWORK-NAME       TO WS-TMP-FILE
               PERFORM 901-REMOVE-TMP-FILE.

063300 100-PHASE-4.
060700
           MOVE 145                    TO CRT-MSG-NBR.
           PERFORM 780-DISPLAY-MSG.

           IF (REPORT-ONLY)
               GO TO 100-END.

           PERFORM 160-UPDATE-CA-EHT.

P62055     IF  (FULL-CYCLE)
P62055         PERFORM 150-CLOSE-TEMP-VOIDS.

063900     PERFORM 120-RESET-RUN-FLAGS.

      * NEEDS TO CHANGE
064100     IF  (PRM-PROCESS-LEVEL      = SPACES)
           AND (PRM-PROC-GROUP         = SPACES)
064200         PERFORM 910-AUDIT-BEGIN
               MOVE PRM-COMPANY           TO DB-COMPANY
064300         MOVE SPACES                TO DB-PROCESS-LEVEL
064400         PERFORM 840-MODIFY-PRSSET1
064500         MOVE " "                   TO PRS-PR199-RUN-FLG
064600         PERFORM 820-STORE-PRSYSTEM
064700         PERFORM 925-AUDIT-END.
064800
064900 100-END.
065000     EXIT.
065100****************************************************************
065200 110-SET-RUN-FLAGS               SECTION.
065300****************************************************************
065400 110-START.
065500*
      * NEEDS TO CHANGE
065600     IF (PRM-PROCESS-LEVEL NOT = SPACES)
               MOVE PRM-COMPANY        TO DB-COMPANY
065700         MOVE PRM-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
065800         PERFORM 840-MODIFY-PRSSET1
065900         MOVE "R"                TO PRS-PR199-RUN-FLG
066000         PERFORM 820-STORE-PRSYSTEM
066100     ELSE
               MOVE PRM-COMPANY        TO DB-COMPANY
066200         MOVE PRSSET1-COMPANY    TO WS-DB-BEG-RNG
066300         PERFORM 850-MODIFY-BEGRNG-PRSSET1
066400         PERFORM 111-SET-PROC-LEV-RUN-FLAGS
066500             UNTIL (PRSYSTEM-NOTFOUND).
066600*
066700 110-END.
066800     EXIT.
066900****************************************************************
067000 111-SET-PROC-LEV-RUN-FLAGS      SECTION.
067100****************************************************************
067200 111-START.
067300*
067400     IF (PRS-PROCESS-LEVEL = SPACES)
067500         GO TO 111-FIND-NEXT.
067600*
067700     MOVE PRS-PROCESS-LEVEL      TO DB-PROCESS-LEVEL.
067800
067900     IF (PRM-PROC-GROUP NOT = SPACES)
               MOVE PRM-PROC-GROUP TO DB-PROC-GROUP
068000         PERFORM 840-KFIND-PRPSET1
068100         IF (PRPROCGRP-KNOTFOUND)
068200             GO TO 111-FIND-NEXT.
068300
068400     MOVE PRS-COMPANY            TO CRT-COMPANY.
068500     MOVE PRS-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL.
068600     PERFORM 900-PROC-LEV-SECURED.
068700     IF (CRT-PROC-LEV-SECURED = "Y")
068800         GO TO 111-FIND-NEXT.
068900
      * NEEDS TO CHANGE
069000     MOVE PYMSET5-PROCESS-LEVEL  TO WS-DB-BEG-RNG.
P62055     IF  (MID-CYCLE)
P62055         PERFORM 190-SET-MID-CYCLE-FILTER
P62055         PERFORM 850-FILTER-BEGRNG-PYMSET5
P62055         IF  (PAYMASTR-FOUND)
P62055             MOVE "R"                TO PRS-PR199-RUN-FLG
P62055             PERFORM 820-STORE-PRSYSTEM
P62055         END-IF
P62055     ELSE
069100         PERFORM 850-FIND-BEGRNG-PYMSET5
069200         IF  (PAYMASTR-FOUND)
069300             MOVE "R"                TO PRS-PR199-RUN-FLG
069400             PERFORM 820-STORE-PRSYSTEM.
069500*
069600 111-FIND-NEXT.
069700*
069800     PERFORM 860-MODIFY-NXTRNG-PRSSET1.
069900*
070000 111-END.
070100     EXIT.
070200****************************************************************
070300 120-RESET-RUN-FLAGS             SECTION.
070400****************************************************************
070500 120-START.
070600*
070700     PERFORM 910-AUDIT-BEGIN.
070800*
           MOVE PRM-COMPANY            TO DB-COMPANY.
070900     MOVE PRM-PROCESS-LEVEL      TO DB-PROCESS-LEVEL.
071000*
071100     IF (PRM-PROCESS-LEVEL = SPACES)
071200         MOVE PRSSET1-COMPANY    TO WS-DB-BEG-RNG
071300         PERFORM 850-MODIFY-BEGRNG-PRSSET1
071400         PERFORM 121-UPDATE-PR197-FLAGS
071500             UNTIL (PRSYSTEM-NOTFOUND)
071600     ELSE
071700         MOVE PRM-COMPANY        TO DB-COMPANY
071800         MOVE PRM-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
071900         PERFORM 840-MODIFY-PRSSET1
072000         PERFORM 121-UPDATE-PR197-FLAGS.
      *
P62055     IF  (FULL-CYCLE)
               PERFORM 122-DELETE-PRMONITOR.
J31827
J31827     MOVE PRM-COMPANY            TO DB-COMPANY.
J31827     MOVE 1                      TO DB-STATUS.
J31827     MOVE LAB-PLAN               TO DB-PLAN.
J31827     MOVE ZEROES                 TO DB-EMPLOYEE.
J31827     MOVE SPACES                 TO DB-EMPLOYEE-GROUP.
J31827     MOVE SPACES                 TO DB-POSITION.
J31827     MOVE ZEROES                 TO DB-TM-OBJ-ID.
J31827     MOVE ZEROES                 TO DB-TM-DATE-STAMP.
J31827     MOVE ZEROES                 TO DB-TM-TIME-STAMP.
J31827     PERFORM 850-FIND-NLT-LABSET3.
J31827     IF  (HRABSMRCVR-FOUND)
J31827     AND (LAB-COMPANY = PRM-COMPANY)
J31827     AND (LAB-STATUS = 1)
J31827         PERFORM 123-SET-GHR-INTERFACE-FLAG
J31827             UNTIL (HRABSMRCVR-NOTFOUND)
J31827             OR    (LAB-COMPANY NOT = DB-COMPANY)
J31827             OR    (LAB-STATUS  NOT = 1)
J31827     END-IF.
J31827
072200     PERFORM 925-AUDIT-END.
072300*
072400 120-END.
072500     EXIT.
072600****************************************************************
072700 121-UPDATE-PR197-FLAGS          SECTION.
072800****************************************************************
072900 121-START.
073000*
073100     IF  (PRM-PROC-GROUP    NOT = SPACES)
073200     AND (PRS-PROCESS-LEVEL NOT = SPACES)
073300         MOVE PRS-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
073400         PERFORM 840-KFIND-PRPSET1
073500         IF (PRPROCGRP-KNOTFOUND)
073600             GO TO 121-NEXT.
073700
073800     IF (PRS-PROCESS-LEVEL NOT = SPACES)
073900         MOVE PRS-COMPANY        TO CRT-COMPANY
074000         MOVE PRS-PROCESS-LEVEL  TO CRT-PROCESS-LEVEL
074100         PERFORM 900-PROC-LEV-SECURED
074200         IF (CRT-PROC-LEV-SECURED = "Y")
074300             GO TO 121-NEXT.
074400
      * NEEDS TO CHANGE
074500     IF (PRS-PR199-RUN-FLG = "R")
P62055         IF  (FULL-CYCLE)
074600             MOVE SPACES             TO PRS-PR132-RUN-FLG
074700                                        PRS-PR140-RUN-FLG
074800                                        PRS-PR160-RUN-FLG
074900                                        PRS-PR199-RUN-FLG
075000                                        PRS-TP135-RUN-FLG
P62055         ELSE
P62055             MOVE SPACES             TO PRS-PR199-RUN-FLG
P62055         END-IF
075100         PERFORM 820-STORE-PRSYSTEM.
075200*
075300 121-NEXT.
075400
075500     IF (PRM-PROCESS-LEVEL = SPACES)
075600         PERFORM 860-MODIFY-NXTRNG-PRSSET1.
075700*
075800 121-END.
075900     EXIT.
DKR   ****************************************************************
       122-DELETE-PRMONITOR            SECTION.
      ****************************************************************
       122-START.
      *
           INITIALIZE                         DB-PROCESS-LEVEL
                                              DB-PROC-GROUP
                                              WS-DB-BEG-RNG.
           MOVE "2"     TO DB-CYCLE-PROCESS.
           MOVE "PR189" TO DB-PROC-GROUP.
           MOVE SPACES  TO DB-BANK-CODE.
      *
           IF  (PRM-PROCESS-LEVEL = SPACES)
           AND (PRM-PROC-GROUP    = SPACES)
               MOVE PMNSET2-PROC-GROUP TO  WS-DB-BEG-RNG
               PERFORM 830-DELETERNG-PMNSET2
           ELSE
               IF  (PRM-PROC-GROUP = SPACES)
                   MOVE PRM-PROCESS-LEVEL     TO DB-PROCESS-LEVEL
                   MOVE PMNSET2-PROCESS-LEVEL TO WS-DB-BEG-RNG
                   PERFORM 830-DELETERNG-PMNSET2
               ELSE  
                   MOVE PRM-PROC-GROUP     TO DB-PROC-GROUP
                   MOVE PRPSET1-PROC-GROUP TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-PRPSET1
                   PERFORM
                       UNTIL (PRPROCGRP-NOTFOUND)
                       IF  (PRP-PROCESS-LEVEL NOT = SPACES)
                           MOVE "PR189"              TO DB-PROC-GROUP
                           MOVE PRP-PROCESS-LEVEL    TO DB-PROCESS-LEVEL
                           MOVE PMNSET2-PROCESS-LEVEL TO WS-DB-BEG-RNG
                           PERFORM 830-DELETERNG-PMNSET2
                       END-IF
                       PERFORM 860-FIND-NXTRNG-PRPSET1
                   END-PERFORM 
               END-IF
           END-IF.
      *
       122-END.
           EXIT.
J31827****************************************************************
J31827 123-SET-GHR-INTERFACE-FLAG      SECTION.
J31827****************************************************************
J31827 123-START.
J31827
J31827     IF (PRM-PROCESS-LEVEL NOT = SPACES)
J31827         MOVE LAB-COMPANY            TO DB-COMPANY
J31827         MOVE LAB-EMPLOYEE           TO DB-EMPLOYEE
J31827         PERFORM 840-FIND-EMPSET1
J31827         IF  (EMPLOYEE-FOUND)
J31827         AND (EMP-PROCESS-LEVEL NOT = PRM-PROCESS-LEVEL)
J31827             GO TO 123-NEXT
J31827         END-IF
J31827     END-IF.
J31827
J31827     MOVE LAB-COMPANY            TO DB-COMPANY.
J31827     MOVE LAB-STATUS             TO DB-STATUS.
J31827     MOVE LAB-PLAN               TO DB-PLAN.
J31827     MOVE LAB-EMPLOYEE           TO DB-EMPLOYEE.
J31827     MOVE LAB-EMPLOYEE-GROUP     TO DB-EMPLOYEE-GROUP.
J31827     MOVE LAB-POSITION           TO DB-POSITION.
J31827     MOVE LAB-TM-OBJ-ID          TO DB-TM-OBJ-ID.
J31827     MOVE LAB-TM-DATE-STAMP      TO DB-TM-DATE-STAMP.
J31827     MOVE LAB-TM-TIME-STAMP      TO DB-TM-TIME-STAMP.
J31827     PERFORM 840-MODIFY-LABSET3.
J31827     MOVE 9                      TO LAB-STATUS.
J31827     MOVE WS-SYSTEM-DATE-YMD     TO LAB-DATE-STAMP.
J31827     MOVE HHMMSS                 TO LAB-TIME-STAMP.
J31827     MOVE "PR197"                TO LAB-USER-ID.
J31827     PERFORM 820-STORE-HRABSMRCVR.
J31827
J31827 123-NEXT.
J31827
J31827     PERFORM 860-FIND-NEXT-LABSET3.
J31827
J31827 123-END.
J31827     EXIT.
076000******************************************************************
076100 150-CLOSE-TEMP-VOIDS            SECTION.
076200******************************************************************
076300 150-START.
076400*
076500     PERFORM 910-AUDIT-BEGIN.
076600     MOVE 5                      TO DB-STATUS.
076700     MOVE PCHSET4-COMPANY        TO WS-DB-BEG-RNG.
076800     PERFORM 850-FIND-BEGRNG-PCHSET4.
076900     PERFORM 152-UPDATE-VOID-STATUS
077000         UNTIL (PRCHECK-NOTFOUND).
077100
077200     PERFORM 925-AUDIT-END.
077300
077400 150-END.
077500     EXIT.
077600******************************************************************
077700 152-UPDATE-VOID-STATUS          SECTION.
077800******************************************************************
077900 152-START.
078000*
078100     IF  (PRM-PROCESS-LEVEL NOT = SPACES)
078200     AND (PCH-PROCESS-LEVEL NOT = PRM-PROCESS-LEVEL)
078300         GO TO 152-NEXT.
078400
078500     IF (PRM-PROC-GROUP NOT = SPACES)
078600         MOVE PCH-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
078700         PERFORM 840-KFIND-PRPSET1
078800         IF (PRPROCGRP-KNOTFOUND)
078900             GO TO 152-NEXT.
079000
079100     IF (PCH-PROCESS-LEVEL NOT = SPACES)
079200         MOVE PCH-COMPANY        TO CRT-COMPANY
079300         MOVE PCH-PROCESS-LEVEL  TO CRT-PROCESS-LEVEL
079400         PERFORM 900-PROC-LEV-SECURED
079500         IF (CRT-PROC-LEV-SECURED = "Y")
079600             GO TO 152-NEXT.

           MOVE PCH-BNK-ACCT-NBR       TO DB-BNK-ACCT-NBR.
           MOVE PCH-PRINT-TYPE         TO DB-PRINT-TYPE.
           MOVE PCH-CHECK-NBR          TO DB-CHECK-NBR.
           MOVE PCH-CHECK-ID           TO DB-CHECK-ID.
J16146*                                TO WS-CHECK-ID.

           PERFORM 840-MODIFY-PCHSET1.

           MOVE 4                      TO PCH-STATUS.
           MOVE SPACE                  TO PCH-CURRENT-FLAG.
           MOVE PCH-CHECK-DATE         TO PCH-RECON-DATE.

           PERFORM 820-STORE-PRCHECK.

080200 152-NEXT.
080300     PERFORM 860-FIND-NXTRNG-PCHSET4.
080400
080500 152-END.
080600     EXIT.
080700
      ****************************************************************
       160-UPDATE-CA-EHT               SECTION.
      ****************************************************************
       160-START.

           IF (RESTARTING-PHASE-2)
           OR (RESTARTING-PHASE-3)
           OR (RESTARTING-PHASE-4)
               PERFORM 161-UPDATE-CA-EHT-RESTART
               THRU    161-END
           ELSE
           IF (NOT PRPRD-UPDATE-CA-EHT)
               GO TO 160-END.

           PERFORM 910-AUDIT-BEGIN.

           PERFORM
            VARYING PRPRD-I1 FROM 1 BY 1
            UNTIL  (PRPRD-I1 > PRPRD-CA-EHT-TABLE-SIZE)
            OR    ((PRPRD-CPL-REPORT-ENTITY (PRPRD-I1) = SPACES)
             AND   (PRPRD-CPL-TAX-ID-CODE (PRPRD-I1)   = SPACES))

             MOVE PRM-COMPANY                        TO DB-COMPANY
             MOVE "CA"                               TO DB-COUNTRY-CODE
             MOVE "T"                                TO DB-RECORD-TYPE
             MOVE WS-PAYROLL-YEAR                    TO DB-PAYROLL-YEAR
             MOVE PRPRD-CPL-REPORT-ENTITY (PRPRD-I1) TO DB-REPORT-ENTITY
             MOVE PRPRD-CPL-TAX-ID-CODE (PRPRD-I1)   TO DB-TAX-ID-CODE
             MOVE ZEROES                             TO DB-EMPLOYEE
             MOVE "H1"                               TO DB-TOPIC
             MOVE 8002                               TO DB-FLD-NBR
             MOVE ZEROES                             TO DB-SEQ-NBR
             PERFORM 840-MODIFY-RGPSET1
             IF (PRREGPARM-NOTFOUND)
      ******* 8001/8002 RECORDS MUST BE MAINTAINED IN PAIRS *******
                 PERFORM 800-CREATE-PRREGPARM
                 MOVE PRM-COMPANY            TO RGP-COMPANY
                 MOVE "CA"                   TO RGP-COUNTRY-CODE
                 MOVE "T"                    TO RGP-RECORD-TYPE
                 MOVE WS-PAYROLL-YEAR        TO RGP-PAYROLL-YEAR
                 MOVE PRPRD-CPL-REPORT-ENTITY (PRPRD-I1)
                                             TO RGP-REPORT-ENTITY
                 MOVE PRPRD-CPL-TAX-ID-CODE (PRPRD-I1)
                                             TO RGP-TAX-ID-CODE
                 MOVE ZEROES                 TO RGP-EMPLOYEE
                 MOVE "H1"                   TO RGP-TOPIC
                 MOVE 8001                   TO RGP-FLD-NBR
                 MOVE ZEROES                 TO RGP-SEQ-NBR
                                                RGP-S-VALUE
                                                RGP-D-VALUE
                 MOVE SPACES                 TO RGP-A-VALUE
                                                RGP-DESCRIPTION
                 PERFORM 820-STORE-PRREGPARM

                 PERFORM 800-CREATE-PRREGPARM
                 MOVE PRM-COMPANY            TO RGP-COMPANY
                 MOVE "CA"                   TO RGP-COUNTRY-CODE
                 MOVE "T"                    TO RGP-RECORD-TYPE
                 MOVE WS-PAYROLL-YEAR        TO RGP-PAYROLL-YEAR
                 MOVE PRPRD-CPL-REPORT-ENTITY (PRPRD-I1)
                                             TO RGP-REPORT-ENTITY
                 MOVE PRPRD-CPL-TAX-ID-CODE (PRPRD-I1)
                                             TO RGP-TAX-ID-CODE
                 MOVE ZEROES                 TO RGP-EMPLOYEE
                 MOVE "H1"                   TO RGP-TOPIC
                 MOVE 8002                   TO RGP-FLD-NBR
                 MOVE ZEROES                 TO RGP-SEQ-NBR
                                                RGP-S-VALUE
                                                RGP-D-VALUE
                 MOVE SPACES                 TO RGP-A-VALUE
                                                RGP-DESCRIPTION
             END-IF

             ADD  PRPRD-CPL-YTD-WAGES (PRPRD-I1) TO RGP-S-VALUE

             PERFORM 820-STORE-PRREGPARM

           END-PERFORM.

           PERFORM 925-AUDIT-END.

           GO TO 160-END.

      ****************************************************************
       161-UPDATE-CA-EHT-RESTART.
      ****************************************************************

      *** RE-PROCESS TO ACCUMULATE PAYDEDUCTN INFO FOR EHT YEAR TO DATE.
      *** TABLE DATA IS LOST ON RESTART AND NEEDS TO BE REBUILT.

           IF (PRM-PROC-GROUP NOT = SPACES)
               MOVE PRM-PROC-GROUP     TO DB-PROC-GROUP
               MOVE PRPSET1-PROC-GROUP TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PRPSET1
               PERFORM
                   UNTIL (PRPROCGRP-NOTFOUND)

                   MOVE PRP-COMPANY        TO CRT-COMPANY
                   MOVE PRP-PROCESS-LEVEL  TO CRT-PROCESS-LEVEL
                   PERFORM 900-PROC-LEV-SECURED
                   IF (CRT-PROC-LEV-SECURED NOT = "Y")
                       MOVE PRM-COMPANY        TO DB-COMPANY
                       MOVE PRP-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
                       PERFORM 840-FIND-PRSSET1
                       PERFORM 165-PROCESS-PAYMASTR
                       THRU    165-END
                   END-IF
                   PERFORM 860-FIND-NXTRNG-PRPSET1
               END-PERFORM
           ELSE
           IF (PRM-PROCESS-LEVEL = SPACES)
               MOVE PRM-COMPANY            TO DB-COMPANY
               MOVE PRSSET1-COMPANY        TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PRSSET1
               PERFORM 165-PROCESS-PAYMASTR
               THRU    165-END
                  UNTIL (PRSYSTEM-NOTFOUND)
           ELSE
               MOVE PRM-COMPANY            TO DB-COMPANY
               MOVE PRM-PROCESS-LEVEL      TO DB-PROCESS-LEVEL
               PERFORM 840-FIND-PRSSET1
               PERFORM 165-PROCESS-PAYMASTR
               THRU    165-END.

       161-END.

      ****************************************************************
       165-PROCESS-PAYMASTR.
      ****************************************************************

           IF (PRS-PROCESS-LEVEL = SPACES)
               GO TO 165-NEXT.

           MOVE PRS-COMPANY            TO CRT-COMPANY.
           MOVE PRS-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL.
           PERFORM 900-PROC-LEV-SECURED.
           IF (CRT-PROC-LEV-SECURED = "Y")
               GO TO 165-NEXT.

           MOVE PRS-COMPANY        TO DB-COMPANY.
           MOVE WS-RS-RUN-DATE     TO DB-DATE-STAMP.
           MOVE WS-RS-RUN-TIME     TO DB-TIME-STAMP.
           MOVE PRS-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
                                      WS-PYM-PROCESS-LEVEL.

           INITIALIZE DB-EMPLOYEE
                      DB-CHECK-TYPE
                      DB-CHECK-ID.

P62055     IF  (MID-CYCLE)
P62055         PERFORM 190-SET-MID-CYCLE-FILTER
P62055         PERFORM 850-FILTER-NLT-PYMSET6
P62055     ELSE
               PERFORM 850-FIND-NLT-PYMSET6.

           PERFORM
              UNTIL (PAYMASTR-NOTFOUND)
              OR    (PYM-COMPANY       NOT = PRM-COMPANY)
              OR    (PYM-DATE-STAMP    NOT = DB-DATE-STAMP)
              OR    (PYM-TIME-STAMP    NOT = DB-TIME-STAMP)
              OR    (PYM-PROCESS-LEVEL NOT = WS-PYM-PROCESS-LEVEL)

                IF (PYM-COUNTRY-CODE = "CA")

                    MOVE PYM-CHECK-DATE    TO WS-BREAK-DATE
                    MOVE WS-BD-YYYY        TO WS-PAYROLL-YEAR

                    MOVE PYM-EMPLOYEE      TO DB-EMPLOYEE
                    MOVE PYM-CHECK-ID      TO DB-CHECK-ID
                    MOVE PYDSET1-CHECK-ID  TO WS-DB-BEG-RNG

                    PERFORM 850-FIND-BEGRNG-PYDSET1

                    PERFORM
                     UNTIL (PAYDEDUCTN-NOTFOUND)

                      MOVE PYD-DED-CODE         TO DB-DED-CODE
                      PERFORM 840-FIND-DDCSET1
                      IF (DDC-TAX-CATEGORY = 20)
                          MOVE DDC-TAX-ID-CODE  TO DB-TAX-ID-CODE
                          PERFORM 840-FIND-PRXSET1
                          IF  (PRTAXAUTH-FOUND)
                          AND (PRX-TAX-PROVINCE = "ON" OR "MB" OR "NF"
                                               OR "NL")
                             PERFORM 7005-CA-EHT-YTD
                          END-IF
                      END-IF
                      PERFORM 860-FIND-NXTRNG-PYDSET1
                    END-PERFORM
                END-IF
                PERFORM 860-FIND-NEXT-PYMSET6
           END-PERFORM.

       165-NEXT.

           IF  (PRM-PROCESS-LEVEL = SPACES)
           AND (PRM-PROC-GROUP    = SPACES)
               PERFORM 860-FIND-NXTRNG-PRSSET1.

       165-END.

       160-END.

P62055****************************************************************
P62055 190-SET-MID-CYCLE-FILTER        SECTION.
P62055****************************************************************
P62055
P62055     MOVE SPACES                 TO FILTER-STRING.
P62055     STRING
P62055     "(PYM-CHECK-TYPE != ?) AND (PYM-CHECK-TYPE != ?) AND "
P62055     "(PYM-CHECK-TYPE != ?)"
P62055         DELIMITED BY SIZE
P62055         INTO FILTER-STRING.
P62055     PERFORM 890-CREATE-FILTER.
P62055     MOVE "S"                    TO ALPHANUM-FILTER-VALUE.
P62055     PERFORM 890-SET-ALPHANUM-FILTER-VALUE.
P62055     MOVE "P"                    TO ALPHANUM-FILTER-VALUE.
P62055     PERFORM 890-SET-ALPHANUM-FILTER-VALUE.
P62055     MOVE "A"                    TO ALPHANUM-FILTER-VALUE.
P62055     PERFORM 890-SET-ALPHANUM-FILTER-VALUE.
XX
P62055 190-END.
097100****************************************************************
097200 200-DISTRIB-CONTROL             SECTION.
097300****************************************************************
097400 200-START.
102600*
102700     MOVE DIPSET2-COMPANY            TO WS-DB-BEG-RNG.
102800     PERFORM 850-FIND-BEGRNG-DIPSET2.
102900     IF  (EMDISTMAST-FOUND)
103000         MOVE DIP-EMPLOYEE           TO WS-EPD-EMPLOYEE
103100     ELSE
103200         MOVE ZEROS                  TO WS-EPD-EMPLOYEE.
103300
103400     IF (PRM-PCT-TO-PREV = ZEROS)
103500         MOVE ZEROS              TO PRPRD-PCT-TO-PREV
103600         MOVE 1                  TO PRPRD-PCT-TO-CURR
103700     ELSE
103800         COMPUTE PRPRD-PCT-TO-PREV  = PRM-PCT-TO-PREV / 100
103900         COMPUTE PRPRD-PCT-TO-CURR  = (1 - PRPRD-PCT-TO-PREV).
104000
104100     IF (PRM-PROC-GROUP NOT = SPACES)
104200         MOVE PRM-PROC-GROUP     TO DB-PROC-GROUP
104300         MOVE PRPSET1-PROC-GROUP TO WS-DB-BEG-RNG
104400         PERFORM 850-FIND-BEGRNG-PRPSET1
104500         PERFORM
104600             UNTIL (PRPROCGRP-NOTFOUND)
104700
104800             MOVE PRP-COMPANY        TO CRT-COMPANY
104900             MOVE PRP-PROCESS-LEVEL  TO CRT-PROCESS-LEVEL
105000             PERFORM 900-PROC-LEV-SECURED
105100             IF (CRT-PROC-LEV-SECURED NOT = "Y")
                       MOVE PRM-COMPANY        TO DB-COMPANY
105200                 MOVE PRP-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
105300                 PERFORM 840-FIND-PRSSET1
105400                 PERFORM 230-DISTRIBUTE
105500             END-IF
105600             PERFORM 860-FIND-NXTRNG-PRPSET1
105700         END-PERFORM
105800     ELSE
105900     IF (PRM-PROCESS-LEVEL = SPACES)
               MOVE PRM-COMPANY            TO DB-COMPANY
106000         MOVE PRSSET1-COMPANY        TO WS-DB-BEG-RNG
106100         PERFORM 850-FIND-BEGRNG-PRSSET1
106200         PERFORM 230-DISTRIBUTE
106300            UNTIL (PRSYSTEM-NOTFOUND)
106400     ELSE
               MOVE PRM-COMPANY            TO DB-COMPANY
106500         MOVE PRM-PROCESS-LEVEL      TO DB-PROCESS-LEVEL
106600         PERFORM 840-FIND-PRSSET1
106700         PERFORM 230-DISTRIBUTE.
106800*
106900 200-END.
107000     EXIT.
107100****************************************************************
107200 230-DISTRIBUTE                  SECTION.
107300****************************************************************
107400 230-START.
107500*
107600     IF (PRS-PROCESS-LEVEL = SPACES)
107700         GO TO 230-NEXT.
107800
107900     MOVE PRS-COMPANY            TO CRT-COMPANY.
108000     MOVE PRS-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL.
108100     PERFORM 900-PROC-LEV-SECURED.
108200     IF (CRT-PROC-LEV-SECURED = "Y")
108300         GO TO 230-NEXT.
108400
           MOVE "Y"                    TO HRPRS-PRD-OPTION.
108500*     IF (PRS-PRD-OPTION       = " ")
108600*         MOVE HRPRS-CO-PRD-OPTION
      *                                TO HRPRS-PRD-OPTION
108700*     ELSE
108800*         MOVE PRS-PRD-OPTION    TO HRPRS-PRD-OPTION.
108900
109000     MOVE PRS-ACR-DIST-CO        TO HRPRS-ACR-DIST-CO.
109100     MOVE PRS-ACR-ACCT-UNIT      TO HRPRS-ACR-ACCT-UNIT.
109200     MOVE PRS-PIK-DIST-CO        TO HRPRS-PIK-DIST-CO.
109300     MOVE PRS-PIK-ACCT-UNIT      TO HRPRS-PIK-ACCT-UNIT.
109400     MOVE PRS-PIK-ACCOUNT        TO HRPRS-PIK-ACCOUNT.
109500     MOVE PRS-PIK-SUB-ACCT       TO HRPRS-PIK-SUB-ACCT.
109600     MOVE PRS-CL-DIST-CO         TO HRPRS-CL-DIST-CO.
109700     MOVE PRS-CL-ACCT-UNIT       TO HRPRS-CL-ACCT-UNIT.
109800     MOVE PRS-CL-ACCOUNT         TO HRPRS-CL-ACCOUNT.
109900     MOVE PRS-CL-SUB-ACCT        TO HRPRS-CL-SUB-ACCT.
           MOVE PRS-CURRENCY-CODE      TO PRPRD-CURRENCY-CODE.
           MOVE PRS-CURRENCY-FLAG      TO PRPRD-CURRENCY-FLAG.

      *** IF RESTARTING IN PHASE ONE, RE-LOAD THE WORKFILES ***

           IF  (PRPRD-PR197-RS-PHASE-1)
           AND (UPDATING)
               MOVE PRS-COMPANY        TO DB-COMPANY
               MOVE WS-RS-RUN-DATE     TO DB-DATE-STAMP
               MOVE WS-RS-RUN-TIME     TO DB-TIME-STAMP
               MOVE PRS-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
                                          WS-PYM-PROCESS-LEVEL
               INITIALIZE DB-EMPLOYEE
                          DB-CHECK-TYPE
                          DB-CHECK-ID
P62055         IF  (MID-CYCLE)
P62055             PERFORM 190-SET-MID-CYCLE-FILTER
P62055             PERFORM 850-FILTER-NLT-PYMSET6
P62055         ELSE
                   PERFORM 850-FIND-NLT-PYMSET6
P62055         END-IF
      *** THE PRPRD-PYMSET6-SW INDICATES TO SUBSEQUENT LOGIC THAT NO ***
      *** UPDATES SHOULD BE DONE (BECAUSE THE WORKFILES ARE BEING    ***
      *** RE-LOADED) AND TO USE PYMSET6.                             ***
               MOVE WS-TRUE            TO PRPRD-PYMSET6-SW
               IF (PAYMASTR-FOUND)
                   PERFORM 231-GM-POSITION-FLAG
               END-IF
               PERFORM 235-PROCESS-EMPLOYEE
                   UNTIL (PAYMASTR-NOTFOUND)
                   OR    (PYM-COMPANY       NOT = PRM-COMPANY)
                   OR    (PYM-DATE-STAMP    NOT = DB-DATE-STAMP)
                   OR    (PYM-TIME-STAMP    NOT = DB-TIME-STAMP)
                   OR    (PYM-PROCESS-LEVEL NOT = WS-PYM-PROCESS-LEVEL).

110100     MOVE PRS-COMPANY            TO DB-COMPANY.
110200     MOVE PRS-PROCESS-LEVEL      TO DB-PROCESS-LEVEL
110300                                    WS-PYM-PROCESS-LEVEL.
110400
           INITIALIZE DB-EMPLOYEE
110600                DB-CHECK-TYPE
110700                DB-CHECK-ID.
110800
           IF (REPORT-ONLY)
P62055         IF  (MID-CYCLE)
P62055             PERFORM 190-SET-MID-CYCLE-FILTER
P62055             PERFORM 850-FILTER-NLT-PYMSET5
P62055         ELSE
                   PERFORM 850-FIND-NLT-PYMSET5
P62055         END-IF
           ELSE
               IF  (WS-QTD-OBJ-REMAIN < WS-MAX-OBJ-PER-PYM-SIZE)
                   PERFORM 910-AUDIT-BEGIN
                   MOVE "PRQTD"                  TO IFOBIWS-OBJ-TYPE
                   MOVE WS-NBR-OF-OBJ-REQUESTED  TO IFOBIWS-NBR-OBJECTS
                                                    WS-QTD-OBJ-REMAIN
                   PERFORM 7000-ASSIGN-OBJ-ID-70
                   COMPUTE WS-LAST-QTD-OBJ-ID    = IFOBIWS-OBJ-ID
                                                 - IFOBIWS-NBR-OBJECTS
                   PERFORM 925-AUDIT-END
               END-IF

               PERFORM 910-AUDIT-BEGIN
P62055         IF  (MID-CYCLE)
P62055             PERFORM 190-SET-MID-CYCLE-FILTER
P62055             PERFORM 850-MODFILTER-NLT-PYMSET5
P62055         ELSE
                   PERFORM 850-MODIFY-NLT-PYMSET5.

           IF (PAYMASTR-FOUND)
               PERFORM 231-GM-POSITION-FLAG.

           MOVE WS-FALSE               TO PRPRD-PYMSET6-SW.
111200     PERFORM 235-PROCESS-EMPLOYEE
111300         UNTIL (PAYMASTR-NOTFOUND)
111400         OR    (PYM-COMPANY       NOT = PRM-COMPANY)
111500         OR    (PYM-PROCESS-LEVEL NOT = WS-PYM-PROCESS-LEVEL).
111600
056400     PERFORM 260-CLOSE-PARTIAL-ACHS-VOIDED.
           IF (UPDATING)
               PERFORM 925-AUDIT-END.
111800
111900 230-NEXT.
112000     IF  (PRM-PROCESS-LEVEL = SPACES)
112100     AND (PRM-PROC-GROUP    = SPACES)
112200         PERFORM 860-FIND-NXTRNG-PRSSET1.
112300
112400 230-END.
112500     EXIT.
112600****************************************************************
112700 231-GM-POSITION-FLAG            SECTION.
112800****************************************************************
112900 231-START.

           MOVE SPACES                   TO PRPRD-POSITION-USED.

           MOVE PYM-COMPANY              TO DB-COMPANY.
           MOVE PYM-PROCESS-LEVEL        TO DB-PROCESS-LEVEL.
           MOVE PYM-DEPARTMENT           TO DB-DEPARTMENT.
           MOVE PYM-PER-END-DATE         TO DB-EFFECT-DATE.
           PERFORM 850-FIND-NLT-PPRSET2.
           IF  (PAPOSRULE-FOUND)
           AND (PPR-COMPANY         = DB-COMPANY)
           AND (PPR-PROCESS-LEVEL   = DB-PROCESS-LEVEL)
           AND (PPR-DEPARTMENT      = DB-DEPARTMENT)
           AND (PPR-POS-USE         = 1 OR 3) 
               MOVE "Y"                 TO PRPRD-POSITION-USED.

           IF (PRPRD-POSITION-USED = SPACES)
               MOVE PYM-COMPANY              TO DB-COMPANY
               MOVE PYM-PROCESS-LEVEL        TO DB-PROCESS-LEVEL
               INITIALIZE                       DB-DEPARTMENT
               MOVE PYM-PER-END-DATE         TO DB-EFFECT-DATE
               PERFORM 850-FIND-NLT-PPRSET2
               IF  (PAPOSRULE-FOUND)
               AND (PPR-COMPANY         = DB-COMPANY)
               AND (PPR-PROCESS-LEVEL   = DB-PROCESS-LEVEL)
               AND (PPR-DEPARTMENT      = DB-DEPARTMENT)
               AND (PPR-POS-USE         = 1 OR 3) 
                   MOVE "Y"             TO PRPRD-POSITION-USED.

           IF (PRPRD-POSITION-USED = SPACES)
               MOVE PYM-COMPANY              TO DB-COMPANY
               INITIALIZE                       DB-PROCESS-LEVEL
                                                DB-DEPARTMENT
               MOVE PYM-PER-END-DATE         TO DB-EFFECT-DATE
               PERFORM 850-FIND-NLT-PPRSET2
               IF  (PAPOSRULE-FOUND)
               AND (PPR-COMPANY         = DB-COMPANY)
               AND (PPR-PROCESS-LEVEL   = DB-PROCESS-LEVEL)
               AND (PPR-DEPARTMENT      = DB-DEPARTMENT)
               AND (PPR-POS-USE         = 1 OR 3) 
                   MOVE "Y"             TO PRPRD-POSITION-USED.

           IF (PRPRD-POSITION-USED = SPACES)
               MOVE "N"                 TO PRPRD-POSITION-USED.

112400 231-END.
112500     EXIT.
112600****************************************************************
112700 235-PROCESS-EMPLOYEE            SECTION.
112800****************************************************************
112900 235-START.
113000
113100     INITIALIZE HREMP-SCR-FIELDS
                      PRPRD-GRH-EXISTS-SW.

113200     MOVE PYM-COMPANY            TO DB-COMPANY.
113300     MOVE PYM-EMPLOYEE           TO DB-EMPLOYEE.
113400     PERFORM 840-FIND-EMPSET1.

           MOVE EMP-LAST-NAME          TO HRWS-LAST-NAME.
           MOVE EMP-FIRST-NAME         TO HRWS-FIRST-NAME.
           MOVE EMP-MIDDLE-INIT        TO HRWS-MIDDLE-INIT.
           MOVE EMP-NAME-SUFFIX        TO HRWS-NAME-SUFFIX.
           MOVE EMP-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE.
           PERFORM 750-HR-FORMAT-NAME.

           PERFORM 7000-HREMP-DEFAULT.

P62055* PERFORMANCE ENHANCEMENT
P62055     IF  (PYM-EMPLOYEE NOT = WS-PYM-EMPLOYEE)
               PERFORM 1000-BUILD-EMP-OTD-TABLE.

           MOVE 0                      TO WS-TIPPAY-FLAG.
           MOVE PYM-EMPLOYEE           TO WS-PYM-EMPLOYEE.
           IF  (PRPRD-PYMSET6)
           AND (UPDATING)
               PERFORM 240-CREATE-PAYMAST-DIST
                   UNTIL (PAYMASTR-NOTFOUND)
                   OR    (PYM-COMPANY       NOT = PRM-COMPANY)
                   OR    (PYM-DATE-STAMP    NOT = DB-DATE-STAMP)
                   OR    (PYM-TIME-STAMP    NOT = DB-TIME-STAMP)
                   OR    (PYM-PROCESS-LEVEL NOT = WS-PYM-PROCESS-LEVEL)
                   OR    (PYM-EMPLOYEE      NOT = WS-PYM-EMPLOYEE)
           ELSE
      *** IF THE PROGRAM IS PROCESSING IN NORMAL (PYMSET5) MODE AFTER***
      *** A RESTART, THEN THE RS-PHASE-1 SWITCH SHOULD BE TURNED OFF ***
      *** TO PREVENT PYMSET6 FROM BEING TRIED WITH EACH SUBSEQUENT   ***
      *** PROCESS LEVEL (IN 230-DISTRIBUTE).                         ***
               MOVE WS-FALSE           TO PRPRD-PR197-RS-PHASE-1-SW
113900         PERFORM 240-CREATE-PAYMAST-DIST
114000             UNTIL (PAYMASTR-NOTFOUND)
114100             OR    (PYM-COMPANY       NOT = PRM-COMPANY)
114200             OR    (PYM-PROCESS-LEVEL NOT = WS-PYM-PROCESS-LEVEL)
114300             OR    (PYM-EMPLOYEE      NOT = WS-PYM-EMPLOYEE).
114400
114500 235-END.
114600
114700****************************************************************
114800 240-CREATE-PAYMAST-DIST         SECTION.
114900****************************************************************
115000 240-START.
115100
115200     MOVE PYM-COMPANY            TO PRPYM-COMPANY.
115300     MOVE PYM-EMPLOYEE           TO PRPYM-EMPLOYEE.
115400     MOVE PYM-CHECK-ID           TO PRPYM-CHECK-ID.
115500     MOVE PYM-CHECK-TYPE         TO PRPYM-CHECK-TYPE.
115600     MOVE PYM-CHECK-DATE         TO PRPYM-CHECK-DATE.
115700     MOVE PYM-PROCESS-LEVEL      TO PRPYM-PROCESS-LEVEL.
115800     MOVE PYM-GROSS-PAY          TO PRPYM-GROSS-PAY.
NONERN     MOVE PYM-NON-TXBL-REMUN     TO PRPYM-NON-TXBL-REMUN.
115900     MOVE PYM-BANK-CODE          TO PRPYM-BANK-CODE.
116000     MOVE 99999999               TO PRPYM-CHECK-NBR.
GMDIST     MOVE PRM-PAY-CLASS          TO PRPYM-PAY-CLASS.
116100
116200     MOVE ZEROS                  TO WS-PYD-SIZE.
116300     INITIALIZE WS-PYD-TBL.
116400     MOVE ZEROS                  TO WS-PRT-SIZE.
116500     INITIALIZE WS-PRT-TBL.
           MOVE PYM-PER-END-DATE       TO WS-PRD-PER-END-DATE.

116600     MOVE PYM-CHECK-DATE         TO WS-BREAK-DATE.
116700     MOVE WS-BD-YYYY             TO WS-PAYROLL-YEAR.
J47977     IF  (HREMP-IN-WORK-COUNTRY)
J47977*STORE  COMPANY PAYROLL YEAR
J47977         MOVE  PRPRD-CO-PAYROLL-YEAR
J47977                                 TO WS-PAYROLL-YEAR
J47977     END-IF.

116800
116900     IF (WS-BD-MMDD < 0401)
117000         MOVE 1                  TO WS-QTR
117100     ELSE
117200     IF (WS-BD-MMDD < 0701)
117300         MOVE 2                  TO WS-QTR
117400     ELSE
117500     IF (WS-BD-MMDD < 1001)
117600         MOVE 3                  TO WS-QTR
117700     ELSE
117800     IF (WS-BD-MMDD < 1232)
117900         MOVE 4                  TO WS-QTR.
118000
J11414     IF (PRS-WORK-COUNTRY = "IN")
J11414         IF (WS-BD-MMDD < 0401)
J11414             MOVE 4              TO WS-QTR
J11414         ELSE  
J11414         IF (WS-BD-MMDD < 0701)
J11414             MOVE 1              TO WS-QTR
J11414         ELSE
J11414         IF (WS-BD-MMDD < 1001)
J11414             MOVE 2              TO WS-QTR
J11414         ELSE
J11414         IF (WS-BD-MMDD < 1232) 
J11414             MOVE 3              TO WS-QTR
J11414         END-IF
J11414         END-IF
J11414         END-IF
J11414         END-IF          
J11414     END-IF.
118100     MOVE WS-PAYROLL-YEAR        TO DB-PAYROLL-YEAR.
118200     MOVE PYM-EMPLOYEE           TO DB-EMPLOYEE.
118300     MOVE PYM-CHECK-ID           TO DB-CHECK-ID.
118400*
           IF  (NOT PRPRD-PYMSET6)
           AND (UPDATING)
118500         PERFORM 840-MODIFY-PCHSET5
118600         IF (PRCHECK-FOUND)
118700             IF (PCH-STATUS              = 1)
118800                 MOVE PCH-CHECK-NBR      TO PYM-CHECK-NBR
118900                 MOVE SPACE              TO PCH-CURRENT-FLAG
P84522                 IF  (PYM-CHECK-TYPE      = "A" OR "D" OR "H")
119200                 OR  ((PYM-CHECK-TYPE     = "P" OR "B")
119300                 AND  (PCH-CHECK-TYPE     = "S"))
119400                     MOVE PCH-COMPANY      TO DB-EMPLOYEE
119500                     MOVE PCH-EMPLOYEE     TO DB-EMPLOYEE
119600                     MOVE PCH-CHECK-ID     TO DB-CHECK-ID
119700                     MOVE ACDSET1-CHECK-ID TO WS-DB-BEG-RNG
119800                     PERFORM 850-MODIFY-BEGRNG-ACDSET1
119900                     PERFORM
120000                        UNTIL (EMPACHDIST-NOTFOUND)
120100                          MOVE 3               TO ACD-STATUS
120200                          MOVE ACD-DIST-AMOUNT TO ACD-RECON-AMOUNT
120300                          MOVE PCH-CHECK-DATE  TO ACD-RECON-DATE
120400                          MOVE SPACES          TO ACD-CURRENT-FLAG
120500                          PERFORM 820-STORE-EMPACHDIST
120600                          PERFORM 860-MODIFY-NXTRNG-ACDSET1
120700                     END-PERFORM
120800                 END-IF
120900
P84522                 IF (PCH-CHECK-TYPE      = "A" OR "H")
P47966                 OR ((PCH-CHECK-AMOUNT   = ZEROES)
P47966                 AND (EMP-AUTO-DEPOSIT   = "P"))
121100                     MOVE 3               TO PCH-STATUS
P84522                     IF (PCH-CHECK-TYPE NOT  = "A" AND "H")
P47966                         MOVE ZEROES          TO PCH-RECON-AMOUNT
P47966                     ELSE
121200                         MOVE PYM-NET-PAY-AMT TO PCH-RECON-AMOUNT
P47966                     END-IF
121400                     MOVE PCH-CHECK-DATE  TO PCH-RECON-DATE
121500                     PERFORM 820-STORE-PRCHECK
121600                 ELSE
121700                     MOVE 2               TO PCH-STATUS
121800                     PERFORM 820-STORE-PRCHECK
121900                 END-IF
122000             ELSE
122100             IF (PCH-STATUS              = 7)
122200                 MOVE PCH-REVERSAL-ID    TO DB-CHECK-ID
122300                 PERFORM 840-MODIFY-PCHSET5
122400                 IF (PRCHECK-FOUND)
122500                     MOVE SPACE          TO PCH-CURRENT-FLAG
122600                     PERFORM 820-STORE-PRCHECK.
122700*
122800     IF  (PYM-CHECK-TYPE = "R" OR "V")
           AND (UPDATING)
122900         MOVE PYM-COMPANY        TO DB-COMPANY
123000         MOVE PYM-EMPLOYEE       TO DB-EMPLOYEE
123100         MOVE PYM-CHECK-ID       TO DB-CHECK-ID
123200         MOVE PRTSET1-CHECK-ID   TO WS-DB-BEG-RNG
123300         PERFORM 850-FIND-BEGRNG-PRTSET1
123400         PERFORM
123500             UNTIL (PRTIME-NOTFOUND)
123600
                IF (NOT PRPRD-PYMSET6)

                   INITIALIZE DB-PENSION
                   IF  (PYM-PENS-SEQ-NBR NOT = ZEROES)
                       MOVE PYM-COMPANY        TO DB-COMPANY
                       MOVE PYM-EMPLOYEE       TO DB-EMPLOYEE
                       MOVE PYM-CHECK-ID       TO DB-CHECK-ID
                       MOVE PYM-PENS-SEQ-NBR   TO DB-PENS-SEQ-NBR
                       PERFORM 840-FIND-PNHSET1
                       IF  (PRPENHIST-FOUND)
                       AND (PNH-PENS-DIST-TYPE NOT = 11 AND 12 AND 24)
                           MOVE "Y"            TO DB-PENSION
                       END-IF
                   END-IF

123700             MOVE PRT-COMPANY        TO DB-COMPANY
123800             MOVE PRT-EMPLOYEE       TO DB-EMPLOYEE
123900             MOVE WS-PAYROLL-YEAR    TO DB-PAYROLL-YEAR
124000             MOVE WS-QTR             TO DB-QUARTER
                   MOVE PRT-COUNTRY-CODE   TO DB-COUNTRY-CODE
124100             MOVE PRT-PAY-SUM-GRP    TO DB-PAY-SUM-GRP
                   MOVE PRT-REPORT-ENTITY  TO DB-REPORT-ENTITY
                   MOVE PRT-BUS-NBR-GRP    TO DB-BUS-NBR-GRP
                   MOVE PRT-QC-ENT-NBR-GRP TO DB-QC-ENT-NBR-GRP
124200             MOVE PRT-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
124300             MOVE PRT-WORK-STATE     TO DB-WORK-STATE
124400             PERFORM 840-MODIFY-QTWSET1
124500             IF (QUARTWAGE-NOTFOUND)
124600                 PERFORM 800-CREATE-QUARTWAGE
124700             END-IF
124800             MOVE PRT-COMPANY        TO QTW-COMPANY
124900             MOVE PRT-EMPLOYEE       TO QTW-EMPLOYEE
125000             MOVE WS-PAYROLL-YEAR    TO QTW-PAYROLL-YEAR
125100             MOVE WS-QTR             TO QTW-QUARTER
                   MOVE PRT-COUNTRY-CODE   TO QTW-COUNTRY-CODE
125200             MOVE PRT-PAY-SUM-GRP    TO QTW-PAY-SUM-GRP
                   MOVE PRT-REPORT-ENTITY  TO QTW-REPORT-ENTITY
                   MOVE PRT-BUS-NBR-GRP    TO QTW-BUS-NBR-GRP
                   MOVE PRT-QC-ENT-NBR-GRP TO QTW-QC-ENT-NBR-GRP
125300             MOVE PRT-PROCESS-LEVEL  TO QTW-PROCESS-LEVEL
125400             MOVE PRT-WORK-STATE     TO QTW-WORK-STATE
                   MOVE DB-PENSION         TO QTW-PENSION
                   MOVE PYM-CURRENCY-CODE  TO QTW-CURRENCY-CODE
                   MOVE PYM-CURR-ND        TO QTW-CURR-ND

125500             ADD  PRT-HOURS          TO QTW-HOURS
125600             ADD  PRT-WAGE-AMOUNT    TO QTW-WAGE-AMOUNT
UOM                ADD  PRT-PAY-UNITS      TO QTW-PAY-UNITS
UOM                MOVE PRT-UNIT-MEASURE   TO QTW-UNIT-MEASURE
125700             IF (PRT-OT-RECORD = "Y")
125800                 ADD PRT-HOURS       TO QTW-OT-CALC-HOURS
125900             END-IF
126000             MOVE PRT-COMPANY        TO DB-COMPANY
J57679*            MOVE PRT-PCD-SEQ-NBR    TO DB-SEQ-NBR
J57679*            PERFORM 840-FIND-PCDSET5

J57679             IF (PRT-PCD-SEQ-NBR > 9999)
J57679                 MOVE PRT-PCD-SEQ-NBR     TO DB-PCD-SEQ-NBR
J57679                 PERFORM 840-FIND-PCDSET7
J57679             ELSE
J57679                 MOVE PRT-PCD-SEQ-NBR     TO DB-SEQ-NBR
J57679                 PERFORM 840-FIND-PCDSET5
J57679             END-IF 

126300             IF (PCD-CALC-TYPE = "F")
126400                 ADD PRT-WAGE-AMOUNT TO QTW-NON-CASH-AMT
126500             END-IF
                   MOVE PYM-PROCESS-LEVEL  TO QTW-HM-PROC-LEVEL
126600             PERFORM 820-STORE-QUARTWAGE
                END-IF      
126700*         IF  (BENEFITS-USER)
J99021          IF ((BENEFITS-USER)
J99021          OR  (VALID-GHR-COMPANY))
126800          AND (WS-PRT-SIZE < 50)
126900              ADD 1            TO WS-PRT-SIZE
127000              MOVE PRT-PAY-SUM-GRP
127100                               TO WS-PRT-PAY-SUM-GRP (WS-PRT-SIZE)
127200              MOVE PRT-OT-RECORD
127300                               TO WS-PRT-OT-RECORD (WS-PRT-SIZE)
127400              PERFORM
127500                  VARYING I4 FROM 1 BY 1
127600                  UNTIL  (WS-PRT-PAY-SUM-GRP (I4)
127700                                  = PRT-PAY-SUM-GRP)
127800                  AND    (WS-PRT-OT-RECORD (I4)
127900                                  = PRT-OT-RECORD)
128000
128100                  CONTINUE
128200              END-PERFORM
128300              IF (I4 NOT = WS-PRT-SIZE)
128400                  SUBTRACT 1      FROM WS-PRT-SIZE
128500              END-IF
128600              ADD PRT-HOURS       TO WS-PRT-HOURS (I4)
128700              ADD PRT-WAGE-AMOUNT TO WS-PRT-WAGE-AMOUNT (I4)
128800          END-IF
128900          PERFORM 860-FIND-NXTRNG-PRTSET1
129000         END-PERFORM.
129100
           IF  (NOT PRPRD-PYMSET6)
           AND (UPDATING)
129200         MOVE 9                      TO PYM-STATUS
129300         MOVE WS-RS-RUN-DATE         TO PYM-DATE-STAMP
129400         MOVE WS-RS-RUN-TIME         TO PYM-TIME-STAMP
J85817         IF  (PR-ADVICE-ENABLED)
J85817             MOVE 1                  TO PYM-INTERFACE-FLAG
J85817         END-IF
129500*
129600         PERFORM 820-STORE-PAYMASTR
P83736*REPLACED FILTER-UPDRNG WITH MODFILTER API FOR PT183736
P54266         MOVE PYM-COMPANY            TO DB-COMPANY
P54266         MOVE PYM-EMPLOYEE           TO DB-EMPLOYEE
P54266         MOVE PYM-CHECK-ID           TO DB-CHECK-ID
P54266         MOVE SPACES                 TO FILTER-STRING
P54266         MOVE "(PRD-STATUS = ?)"     TO FILTER-STRING
P54266         PERFORM 890-CREATE-FILTER
P54266
P54266         MOVE ZEROS                  TO NUMERIC-FILTER-VALUE
P54266         PERFORM 890-SET-NUMERIC-FILTER-VALUE
P54266         MOVE PRDSET1-CHECK-ID       TO WS-DB-BEG-RNG
P54266*         MOVE "STATUS = '1'"         TO WS-DB-UPDATE-CMD
P54266*         PERFORM 820-FILTER-UPDRNG-PRDSET1
P83736         PERFORM 850-MODFILTER-BEGRNG-PRDSET1
P83736         PERFORM 
P83736             UNTIL (PRDISTRIB-NOTFOUND)
P83736                 MOVE 1              TO PRD-STATUS 
P83736                 PERFORM 820-STORE-PRDISTRIB
P83736                 PERFORM 860-MODIFY-NXTRNG-PRDSET1
P83736         END-PERFORM.
129700
129800     MOVE PYM-EMPLOYEE           TO DB-EMPLOYEE.
129900     MOVE PYM-CHECK-ID           TO DB-CHECK-ID.
130000
130100     MOVE WS-RS-RUN-DATE         TO PRPRD-RUN-DATE.
130200     MOVE WS-RS-RUN-TIME         TO PRPRD-RUN-TIME.
130400     MOVE ZEROS                  TO PTM.
162900     MOVE PYM-EMPLOYEE           TO WS-PYM-EMPLOYEE.
           INITIALIZE                     WS-INV-SUFFIX-IND.
130500     PERFORM 5000-DISTRIBUTE-PAYMASTR.
130600
J11414***PAY PLAN DEFENITIONS IS BEING FETCH FROM PROCESS LEVEL 
J11414     IF  (PRS-WORK-COUNTRY = "IN")
J11414         INITIALIZE                 WS-PLAN-CODE-DEF
J11414                                    WS-C6PRDDTL
J11414                                    WS-C6E-PROC-STATUS
J11414                                    WS-C6H-PROC-STATUS
J11414                                    WS-CEI-PROC-STATUS
J14141                                    WS-C6D-VALID-RECORD
J11414                                    WS-C6-FILES-DATES
J11414                                    WS-IPR-START-DATE
J11414                                    WS-INEMP-REASON-CODE-UPDATE-SW
J11414         MOVE PRS-OT-PLAN-CODE(4) 
J11414                                 TO WS-PLAN-CODE-DEF
J11414         MOVE WS-IPR-PAYROLL-YEAR                   
J11414                                 TO DB-PAYROLL-YEAR
J11414         MOVE PRM-COMPANY        TO DB-COMPANY
J11414         MOVE WS-PLAN-CODE-DEF   TO DB-PAY-PRD-DEF   
J11414         PERFORM  840-FIND-C6PSET1 
J11414         IF  (C6PRDDEF-FOUND)       
J11414             MOVE C6P-COMPANY    TO DB-COMPANY
J11414             MOVE C6P-PAYROLL-YEAR                       
J11414                                 TO DB-PAYROLL-YEAR
J11414             MOVE C6P-PAY-PRD-DEF                            
J11414                                 TO DB-PAY-PRD-DEF
J11414             MOVE C6DSET1-PAY-PRD-DEF
J11414                                 TO WS-DB-BEG-RNG
J11414             INITIALIZE FILTER-STRING           
J11414             STRING "((C6D-START-DATE <= ?) AND"
J11414                    "(C6D-END-DATE >= ?))"
J11414             DELIMITED BY SIZE INTO FILTER-STRING
J11414             PERFORM 890-CREATE-FILTER
J11414             MOVE PRM-GL-DATE    TO DATETIME-FILTER-VALUE
J11414             PERFORM 890-SET-DATETIME-FILTER-VALUE
J11414             MOVE PRM-GL-DATE    TO DATETIME-FILTER-VALUE 
J11414             PERFORM 890-SET-DATETIME-FILTER-VALUE
J11414             PERFORM 850-FILTER-BEGRNG-C6DSET1
J11414             IF  (C6PRDDTL-FOUND)
J11414             AND (C6D-PERIOD-STATUS = "0")
J11414                 MOVE C6PRDDTL   TO   WS-C6PRDDTL
J11414                 MOVE C6D-START-DATE        
J11414                                 TO  WS-IPR-START-DATE
J11414                 SET WS-C6D-VALID-RECORD-FOUND         
J11414                                 TO TRUE
J11414             ELSE
J11414                 SET WS-C6D-VALID-RECORD-NOT-FOUND        
J11414                                 TO TRUE
J11414             END-IF
J11414         END-IF 
J11414     END-IF.
J11414*** STATUS OF INDIA SPECIFIC FILES NAMELY C6EXEMPT,C6EMPINV *****
J11414*** C6HOUSERNT AND C6PREVEMP GETS UPDATED FROM   PENDING    *****
J11414*** TO CLOSED.                                              *****
J11414     IF  (UPDATING)
J11414     AND (PRS-WORK-COUNTRY = "IN")
J11414     AND (WS-C6D-VALID-RECORD-FOUND) 
J11414         PERFORM 425-UPDATE-IPR-FILES-STATUS
J11414     END-IF.
      *** DEFERRED DELETE OF TIMERECORD (FOR EFFICIENCY) ***

           IF   (NOT PRPRD-PYMSET6)
           AND  (UPDATING)
P00684          MOVE PYM-COMPANY        TO DB-COMPANY
                MOVE PYM-EMPLOYEE       TO DB-EMPLOYEE
                MOVE PYM-CHECK-ID       TO DB-CHECK-ID
                MOVE TRDSET8-CHECK-ID   TO WS-DB-BEG-RNG
                PERFORM 830-DELETERNG-TRDSET8
           END-IF.
      *    IF (BENEFITS-USER)
J99021     IF  (BENEFITS-USER)
J99021     AND (INVALID-GHR-COMPANY)
               PERFORM 500-UPDATE-BENEFITS-INFO.
J99021        
J99021     IF  (VALID-GHR-COMPANY)
J99021         PERFORM 570-UPDATE-GHR-BEN-INFO.
130900
131000     INITIALIZE WS-CL-TABLE.
131100
131200     IF (PTM NOT = ZEROS)
131300         PERFORM
131400             VARYING I2 FROM 1 BY 1
131500             UNTIL  (I2 > PTM)
131600
131700             MOVE WS-PTM-TBL-KEY (I2)   TO WS-PTM-KEY
131800             IF (WS-PTM-DIST-HOURS (I2) NOT = ZEROS)
131900             OR (WS-PTM-DIST-AMT (I2)   NOT = ZEROS)
UOM                OR (WS-PTM-DIST-PAY-UNITS (I2)   NOT = ZEROES)
132000                 ADD 1               TO WS-CL-CNT
132100                 MOVE WS-PTM-PROCESS-LEVEL
132200                                     TO WS-CL-PROC-LEV (WS-CL-CNT)
132300                 MOVE WS-PTM-RECORD-TYPE
132400                                     TO WS-CL-TYPE (WS-CL-CNT)
132500                 MOVE WS-PTM-GL-DATE TO WS-CL-DATE (WS-CL-CNT)
132600                 PERFORM
132700                     VARYING I1 FROM 1 BY 1
132800                     UNTIL  (WS-PTM-PROCESS-LEVEL =
132900                                             WS-CL-PROC-LEV (I1))
133000*                     AND    (WS-PTM-RECORD-TYPE = WS-CL-TYPE (I1))
133100                     AND    (WS-PTM-GL-DATE     = WS-CL-DATE (I1))
133200
133300                     CONTINUE
133400                 END-PERFORM
133500                 IF (I1 < WS-CL-CNT)
133600                     SUBTRACT 1              FROM WS-CL-CNT
133700                     SUBTRACT WS-PTM-DIST-AMT (I2)
133800                                             FROM WS-CL-AMT (I1)
133900                 ELSE
134000                     COMPUTE WS-CL-AMT (I1)
134100                                       = 0 - WS-PTM-DIST-AMT (I2)
134200                 END-IF
134300             END-IF
134400         END-PERFORM.
137700
137800     PERFORM
137900         VARYING I1 FROM 1 BY 1
138000         UNTIL  (I1 > WS-CL-CNT)
138100
138200         IF (WS-CL-AMT (I1) NOT = ZEROS)
138300             PERFORM 244-CREATE-CLEARING-DISTRIB
138400         END-IF
138500     END-PERFORM.
138600
138700     IF (PTM NOT = ZEROS)
138800         PERFORM
138900             VARYING I2 FROM 1 BY 1
139000             UNTIL  (I2 > PTM)
139100
139200             MOVE WS-PTM-TBL-KEY (I2)    TO WS-PTM-KEY
139300             MOVE WS-PTM-DIST-AMT (I2)   TO WS-AMOUNT
139400             IF (WS-GL-UNITS = "Y")
UOM                    IF (WS-PTM-DIST-PAY-UNITS (I2)  NOT = ZEROES)
UOM                        MOVE WS-PTM-DIST-PAY-UNITS (I2) TO WS-UNITS
UOM                        MOVE WS-PTM-DIST-HOURS (I2) TO WS-PAY-HOURS
UOM                        MOVE WS-PTM-UNIT-MEASURE (I2) 
UOM                                            TO WS-UNIT-MEASURE
UOM                    ELSE
UOM                        MOVE ZEROES                 TO WS-UNITS
UOM                        MOVE WS-PTM-DIST-HOURS (I2) TO WS-PAY-HOURS
UOM                        MOVE SPACES         TO WS-UNIT-MEASURE
UOM                    END-IF
139600             END-IF
139700             IF (WS-AMOUNT NOT = ZEROS)
139800             OR (WS-UNITS  NOT = ZEROS)
P30089             OR (WS-PAY-HOURS NOT = ZEROES)
139900                 PERFORM 242-MOVE-TBL-TO-WORK3
140000             END-IF
GMDIST             IF  ((WS-PTM-LAB-DIST-FLAG (I2) = "Y")
GMDIST             OR   (WS-PTM-EFFORT-FLAG   (I2) = "Y"))
J29112*            AND ((WS-AMOUNT             NOT = ZEROES)
J29112*            OR   (WS-UNITS              NOT = ZEROES)
J29112*            OR   (WS-PAY-HOURS          NOT = ZEROES))
GMDIST                 PERFORM 243-MOVE-TBL-TO-GMWORK
GMDIST             END-IF
140100         END-PERFORM.

GMDIST     IF (WS-GM-ERR-COUNT   NOT = ZEROS)
GMDIST         PERFORM
GMDIST             VARYING I2 FROM 1 BY 1
GMDIST             UNTIL  (I2 > WS-GM-ERR-COUNT)
GMDIST                 PERFORM 250-MOVE-ERR-TBL-TO-GMWORK
GMDIST         END-PERFORM
GMDIST     END-IF.
140200
142600     IF  (BENEFITS-USER)
J99021     OR  (VALID-GHR-COMPANY)
142700         PERFORM
142800             VARYING I1 FROM 1 BY 1
142900             UNTIL  (I1 > WS-PLN-TBL-SIZE)
143000
143100             IF (WR-HOURS-SERV (I1)  NOT = ZEROS)
143200             OR (WR-VEST-HOURS (I1)  NOT = ZEROS)
143300             OR (WR-COMP-AMOUNT (I1) NOT = ZEROS)
143400             OR (WR-CMP-DED-AMT (I1) NOT = ZEROS)
143500             OR (WR-PRE-DED-AMT (I1) NOT = ZEROS)
143600             OR (WR-AFT-DED-AMT (I1) NOT = ZEROS)
143700             OR (WR-DED-AMT (I1)     NOT = ZEROS)
143800                 MOVE PYM-PROCESS-LEVEL     TO BWK-PROCESS-LEVEL
143900                 MOVE WS-PLN-PLAN-TYPE (I1) TO BWK-PLAN-TYPE
144000                 MOVE WS-PLN-PLAN-CODE (I1) TO BWK-PLAN-CODE
144100                 PERFORM 8400-FIND-BENWORK
144200                 IF (BENWORK-NOTFOUND)
144300                     PERFORM 8000-CREATE-BENWORK
144400                     MOVE PYM-PROCESS-LEVEL TO BWK-PROCESS-LEVEL
144500                     MOVE WS-PLN-PLAN-TYPE (I1) TO BWK-PLAN-TYPE
144600                     MOVE WS-PLN-PLAN-CODE (I1) TO BWK-PLAN-CODE
J72349                     MOVE "N"            TO BWK-401K-PLAN-FLAG
144700                 END-IF
144800                 ADD WR-HOURS-SERV (I1)  TO BWK-HOURS-SERV
144900                 ADD WR-VEST-HOURS (I1)  TO BWK-VEST-HOURS
145000                 ADD WR-COMP-AMOUNT (I1) TO BWK-COMP-AMOUNT
145100                 ADD WR-CMP-DED-AMT (I1) TO BWK-CMP-DED-AMT
145200                 ADD WR-PRE-DED-AMT (I1) TO BWK-PRE-DED-AMT
145300                 ADD WR-AFT-DED-AMT (I1) TO BWK-AFT-DED-AMT
145400                 ADD WR-DED-AMT (I1)     TO BWK-DED-AMT
J72349                 IF (WR-401K-IN-PLAN (I1) = "Y")
J72349                     MOVE "Y" TO BWK-401K-PLAN-FLAG
J72349                     IF  (WS-PLN-PLAN-TYPE (I1) = "DB" OR "DC")
J72349                     AND (WS-EE-409A-AMOUNT NOT = ZEROES)
J72349                     AND (UPDATING)
189834                     AND (NOT PRPRD-PYMSET6)
J72349                        MOVE PYM-COMPANY           TO DB-COMPANY
J72349                        MOVE WS-PLN-PLAN-TYPE (I1) TO DB-PLAN-TYPE
J72349                        MOVE WS-PLN-PLAN-CODE (I1) TO DB-PLAN-CODE
J72349                        MOVE PYM-EMPLOYEE          TO DB-EMPLOYEE
J72349                        MOVE WR-PLAN-YEAR (I1)     TO DB-PLAN-YEAR
J72349                        PERFORM 840-MODIFY-CPHSET1
J72349                        IF (COMPHIST-FOUND)
J72349                            SUBTRACT WS-EE-409A-AMOUNT   
J72349                            FROM     CPH-COMP-AMOUNT
J72349                            PERFORM 820-STORE-COMPHIST
J72349                        END-IF
J72349                     END-IF
J72349                 END-IF
J72349                 IF  (WR-409A-DED-AMT (I1) NOT = ZERO)
J72349                 AND (WS-EE-401K-PLAN-FOUND = "Y")
J72349                     PERFORM 241-UPDATE-409A-TABLE 
J72349                 END-IF
145500                 PERFORM 8200-STORE-BENWORK
145600             END-IF
145700         END-PERFORM.
145800
           IF  (NOT PRPRD-PYMSET6)
           AND (UPDATING)
           AND (PRPRD-GRH-EXISTS)
               MOVE PYM-EMPLOYEE       TO DB-EMPLOYEE
               MOVE PYM-CHECK-ID       TO DB-CHECK-ID
               MOVE GRHSET1-CHECK-ID   TO WS-DB-BEG-RNG
               PERFORM 850-MODIFY-BEGRNG-GRHSET1
               PERFORM
                  UNTIL (PRGARNHIST-NOTFOUND)

                    MOVE 9             TO GRH-STATUS
                    PERFORM 820-STORE-PRGARNHIST

                    PERFORM 860-MODIFY-NXTRNG-GRHSET1
               END-PERFORM.

           IF  (UPDATING)
           AND (NOT PRPRD-PYMSET6)
               PERFORM 1100-UPDATE-EMP-ONETMDEDS.

           IF  (NOT PRPRD-PYMSET6)
           AND (UPDATING)
               PERFORM 925-AUDIT-END.

       240-FIND-NEXT.
           IF (UPDATING)
               IF  (WS-QTD-OBJ-REMAIN < WS-MAX-OBJ-PER-PYM-SIZE)
                   PERFORM 910-AUDIT-BEGIN
                   MOVE "PRQTD"                  TO IFOBIWS-OBJ-TYPE
                   MOVE WS-NBR-OF-OBJ-REQUESTED  TO IFOBIWS-NBR-OBJECTS
                                                    WS-QTD-OBJ-REMAIN
                   PERFORM 7000-ASSIGN-OBJ-ID-70
                   COMPUTE WS-LAST-QTD-OBJ-ID    = IFOBIWS-OBJ-ID
                                                 - IFOBIWS-NBR-OBJECTS
                   PERFORM 925-AUDIT-END
               END-IF
               IF (PRPRD-PYMSET6)
                   PERFORM 860-FIND-NEXT-PYMSET6
               ELSE
                   PERFORM 910-AUDIT-BEGIN
                   PERFORM 860-MODIFY-NEXT-PYMSET5
               END-IF
           ELSE
               PERFORM 860-FIND-NEXT-PYMSET5.
146900
147000 240-END.
147100
J72349****************************************************************
J72349 241-UPDATE-409A-TABLE          SECTION.
J72349****************************************************************
J72349 241-START.
J72349
J72349     PERFORM
J72349         VARYING I9 FROM 1 BY 1
J72349         UNTIL  (I9 > WS-409A-TABLE-SIZE)           
J72349         OR    ((WS-409A-PROC-LEVEL (I9) = BWK-PROCESS-LEVEL)     
J72349          AND   (WS-409A-PLAN-TYPE  (I9) = BWK-PLAN-TYPE))      
J72349         OR     (WS-409A-PROC-LEVEL (I9) = SPACES)
J72349             CONTINUE
J72349     END-PERFORM.
J72349
J72349     IF (I9 > WS-409A-TABLE-SIZE)           
J72349         MOVE 112                TO CRT-ERROR-NBR
J72349         PERFORM 780-PRINT-ERROR-MSG
J72349         GO TO 241-END.
J72349
J72349     IF (WS-409A-PROC-LEVEL (I9) = SPACES)     
J72349         MOVE BWK-PROCESS-LEVEL   TO WS-409A-PROC-LEVEL (I9)
J72349         MOVE BWK-PLAN-TYPE       TO WS-409A-PLAN-TYPE  (I9)
J72349         ADD WR-409A-DED-AMT (I1) TO WS-409A-AMOUNT (I9)
J72349     ELSE
J72349         ADD WR-409A-DED-AMT (I1) TO WS-409A-AMOUNT (I9)
J72349     END-IF.
J72349
J72349 241-END.
J72349     EXIT.
147200****************************************************************
147300 242-MOVE-TBL-TO-WORK3          SECTION.
147400****************************************************************
147500 242-START.
147600
147800     MOVE PRM-COMPANY            TO WR3-COMPANY.
147900     MOVE WS-PTM-GL-DATE         TO WR3-GL-DATE.
148000     MOVE WS-PTM-DIST-COMPANY    TO WR3-TO-COMPANY.
148100     MOVE WS-PTM-DST-ACCT-UNIT   TO WR3-ACCT-UNIT.
148200     MOVE WS-PTM-DST-ACCOUNT     TO WR3-ACCOUNT.
149400     MOVE WS-AMOUNT              TO WR3-TRAN-AMOUNT
149500                                    WR3-DIST-AMT.
149700     MOVE 2                      TO WR3-TRAN-ND.
UOM        MOVE WS-PAY-HOURS           TO WR3-HOURS-AMOUNT.
UOM        MOVE WS-UNIT-MEASURE        TO WR3-UNIT-MEASURE.
UOM        MOVE WS-UNITS               TO WR3-UNITS-AMOUNT.
150200     MOVE WS-PTM-DST-SUB-ACCT    TO WR3-SUB-ACCOUNT.
150400     MOVE WS-PTM-ACTIVITY        TO WR3-ACTIVITY.
150500     MOVE WS-PTM-ACCT-CATEGORY   TO WR3-ACCT-CATEGORY.
J85817     MOVE WS-PTM-EXP-RPT-NBR     TO WR3-EXP-RPT-NBR.
J85817     MOVE WS-PTM-REC-TYPE        TO WR3-REC-TYPE.
150600
151000     IF  (WS-PTM-CLEARING    NOT = "Y")
151100     AND (WS-PTM-RECORD-TYPE = "E")
151200     AND (WS-PTM-DED-CODE    NOT = SPACES)
151400         PERFORM
151500             VARYING I1 FROM 1 BY 1
151600             UNTIL  (I1 > 3)
151700
151800             IF (WS-PD-FIELD-NAME (I1) = WS-EMPLOYEE-LIT)
151900                 MOVE PYM-EMPLOYEE       TO WS-EMPLOYEE-N
152000                 MOVE WS-EMPLOYEE-A      TO WR3-ANALYSIS-FLD (I1)
152100             ELSE
                   IF (WS-PD-FIELD-NAME (I1) = WS-POSITION-LIT)
                       MOVE WS-PTM-POSITION    TO WR3-ANALYSIS-FLD (I1)
                   ELSE
152200             IF (WS-PD-FIELD-NAME (I1) = WS-JOB-CODE-LIT)
152300                 MOVE WS-PTM-JOB-CODE    TO WR3-ANALYSIS-FLD (I1)
152400             ELSE
152500             IF (WS-PD-FIELD-NAME (I1) = WS-JOB-CLASS-LIT)
152600                 MOVE PRM-COMPANY        TO DB-COMPANY
152700                 MOVE WS-PTM-JOB-CODE    TO DB-JOB-CODE
152800                 IF (WS-PTM-JOB-CODE NOT = SPACES)
152900                     PERFORM 840-FIND-JBCSET1
153000                     MOVE JBC-JOB-CLASS  TO WR3-ANALYSIS-FLD (I1)
153100                 END-IF
153200             ELSE
153300             IF (WS-PD-FIELD-NAME (I1) = WS-DED-CODE-LIT)
153400                 MOVE WS-PTM-DED-CODE    TO WR3-ANALYSIS-FLD (I1)
153500             ELSE
153600             IF (WS-PD-FIELD-NAME (I1) = WS-ATTEND-CODE-LIT)
153700                 MOVE WS-PTM-ATTEND-CODE TO WR3-ANALYSIS-FLD (I1)
153800             END-IF
153900         END-PERFORM.
154000
154100     IF  (WS-PTM-CLEARING    NOT = "Y")
154200     AND (WS-PTM-RECORD-TYPE     = "E")
154300     AND (WS-PTM-PCD-SEQ-NBR NOT = ZEROS)
GRANTM     AND (WS-PTM-DED-CODE        = SPACES)
154500         PERFORM
154600             VARYING I1 FROM 1 BY 1
154700             UNTIL  (I1 > 3)
154800
154900             IF (WS-PW-FIELD-NAME (I1) = WS-EMPLOYEE-LIT)
155000                 MOVE PYM-EMPLOYEE       TO WS-EMPLOYEE-N
155100                 MOVE WS-EMPLOYEE-A      TO WR3-ANALYSIS-FLD (I1)
155200             ELSE
                   IF (WS-PW-FIELD-NAME (I1) = WS-POSITION-LIT)
                       MOVE WS-PTM-POSITION    TO WR3-ANALYSIS-FLD (I1)
                   ELSE
155300             IF (WS-PW-FIELD-NAME (I1) = WS-JOB-CODE-LIT)
155400                 MOVE WS-PTM-JOB-CODE    TO WR3-ANALYSIS-FLD (I1)
155500             ELSE
155600             IF (WS-PW-FIELD-NAME (I1) = WS-JOB-CLASS-LIT)
155700                 MOVE PRM-COMPANY        TO DB-COMPANY
155800                 MOVE WS-PTM-JOB-CODE    TO DB-JOB-CODE
155900                 IF (WS-PTM-JOB-CODE NOT = SPACES)
156000                     PERFORM 840-FIND-JBCSET1
156100                     MOVE JBC-JOB-CLASS  TO WR3-ANALYSIS-FLD (I1)
156200                 END-IF
156300             ELSE
156400             IF (WS-PW-FIELD-NAME (I1) = WS-PAY-CODE-LIT)
156500                 MOVE PRM-COMPANY        TO DB-COMPANY
J57679*                MOVE WS-PTM-PCD-SEQ-NBR TO DB-SEQ-NBR
J57679*                PERFORM 840-FIND-PCDSET5

J57679                 IF (WS-PTM-PCD-SEQ-NBR > 9999)
J57679                     MOVE WS-PTM-PCD-SEQ-NBR     TO DB-PCD-SEQ-NBR
J57679                     PERFORM 840-FIND-PCDSET7
J57679                 ELSE
J57679                     MOVE WS-PTM-PCD-SEQ-NBR     TO DB-SEQ-NBR
J57679                     PERFORM 840-FIND-PCDSET5
J57679                 END-IF 

156800                 MOVE PCD-PAY-CODE       TO WR3-ANALYSIS-FLD (I1)
156900             ELSE
157000             IF (WS-PW-FIELD-NAME (I1) = WS-ATTEND-CODE-LIT)
157100                 MOVE WS-PTM-ATTEND-CODE TO WR3-ANALYSIS-FLD (I1)
157200             END-IF
157300         END-PERFORM.
157400
157500     IF  (WS-PTM-CLEARING    NOT = "Y")
157600     AND (WS-PTM-RECORD-TYPE = "C")
157800         PERFORM
157900             VARYING I1 FROM 1 BY 1
158000             UNTIL  (I1 > 3)
158100
158200             IF (WS-PC-FIELD-NAME (I1) = WS-EMPLOYEE-LIT)
158300                 MOVE PYM-EMPLOYEE       TO WS-EMPLOYEE-N
158400                 MOVE WS-EMPLOYEE-A      TO WR3-ANALYSIS-FLD (I1)
158500             ELSE
                   IF (WS-PC-FIELD-NAME (I1) = WS-POSITION-LIT)
                       MOVE WS-PTM-POSITION    TO WR3-ANALYSIS-FLD (I1)
                   ELSE
158600             IF (WS-PC-FIELD-NAME (I1) = WS-JOB-CODE-LIT)
158700                 MOVE WS-PTM-JOB-CODE    TO WR3-ANALYSIS-FLD (I1)
158800             ELSE
158900             IF (WS-PC-FIELD-NAME (I1) = WS-JOB-CLASS-LIT)
159000                 MOVE PRM-COMPANY        TO DB-COMPANY
159100                 MOVE WS-PTM-JOB-CODE    TO DB-JOB-CODE
159200                 IF (WS-PTM-JOB-CODE NOT = SPACES)
159300                     PERFORM 840-FIND-JBCSET1
159400                     MOVE JBC-JOB-CLASS  TO WR3-ANALYSIS-FLD (I1)
159500                 END-IF
159600             END-IF
159700         END-PERFORM.
159800
159900     IF  (WS-PTM-CLEARING    NOT = "Y")
160000     AND (WS-PTM-RECORD-TYPE = "A")
160200         PERFORM
160300             VARYING I1 FROM 1 BY 1
160400             UNTIL  (I1 > 3)
160500
160600             IF (WS-PA-FIELD-NAME (I1) = WS-EMPLOYEE-LIT)
160700                 MOVE PYM-EMPLOYEE       TO WS-EMPLOYEE-N
160800                 MOVE WS-EMPLOYEE-A      TO WR3-ANALYSIS-FLD (I1)
160900             ELSE
                   IF (WS-PA-FIELD-NAME (I1) = WS-POSITION-LIT)
                       MOVE WS-PTM-POSITION    TO WR3-ANALYSIS-FLD (I1)
                   ELSE
161000             IF (WS-PA-FIELD-NAME (I1) = WS-JOB-CODE-LIT)
161100                 MOVE WS-PTM-JOB-CODE    TO WR3-ANALYSIS-FLD (I1)
161200             ELSE
161300             IF (WS-PA-FIELD-NAME (I1) = WS-JOB-CLASS-LIT)
161400                 MOVE PRM-COMPANY        TO DB-COMPANY
161500                 MOVE WS-PTM-JOB-CODE    TO DB-JOB-CODE
161600                 IF (WS-PTM-JOB-CODE NOT = SPACES)
161700                     PERFORM 840-FIND-JBCSET1
161800                     MOVE JBC-JOB-CLASS  TO WR3-ANALYSIS-FLD (I1)
161900                 END-IF
162000             ELSE
162100             IF (WS-PA-FIELD-NAME (I1) = WS-DED-CODE-LIT)
162200                 MOVE WS-PTM-DED-CODE    TO WR3-ANALYSIS-FLD (I1)
162300             END-IF
162400         END-PERFORM.
162500
162800     IF (WS-PTM-CLEARING NOT = "Y")
162900         MOVE PYM-EMPLOYEE       TO WR3-EMPLOYEE
163000         MOVE PYM-CHECK-ID       TO WR3-CHECK-ID
163100         MOVE PYM-CHECK-TYPE     TO WR3-CHECK-TYPE
               MOVE HRWS-FORMAT-NAME   TO WR3-FULL-NAME
               MOVE PYM-CHECK-NBR      TO WR3-CHECK-NBR

163200         IF (WR3-CHECK-TYPE  = "V")
163300             MOVE "R"            TO WR3-CHECK-TYPE
163400         END-IF.
163500
           IF (PRM-REPORT-SEQ = "P")
               MOVE WS-PTM-PROCESS-LEVEL
                                       TO WR3-PROCESS-LEVEL-SRT
               MOVE WS-PTM-DEPARTMENT  TO WR3-DEPARTMENT-SRT
           ELSE
               INITIALIZE                 WR3-PROCESS-LEVEL-SRT
                                          WR3-DEPARTMENT-SRT.

163600     MOVE WS-PTM-CLEARING        TO WR3-CLEARING.
163700     MOVE WS-PTM-PROCESS-LEVEL   TO WR3-PROCESS-LEVEL.
163800     MOVE WS-PTM-RECORD-TYPE     TO WR3-RECORD-TYPE.
163900     MOVE HRPRS-PRD-OPTION       TO WR3-PRD-OPTION.
164000     MOVE WS-PTM-DEPARTMENT      TO WR3-DEPARTMENT.
164100     MOVE WS-PTM-PCD-SEQ-NBR     TO WR3-PCD-SEQ-NBR.
164200     MOVE WS-PTM-DED-CODE        TO WR3-DED-CODE.
164300     MOVE WS-PTM-JOB-CODE        TO WR3-JOB-CODE.
164400     MOVE WS-PTM-OBJ-ID (I2)     TO WR3-OBJ-ID.
           MOVE WS-PTM-PAY-CODE (I2)   TO WR3-PAY-CODE.
GMDIST     MOVE WS-PTM-OT-PLAN-CODE(I2)
GMDIST                                 TO WR3-OT-PLAN-CODE.
164500     MOVE WS-PTM-POSITION        TO WR3-POSITION.
164600     MOVE WS-PTM-ATTEND-CODE     TO WR3-ATTEND-CODE.
164900     MOVE WS-PTM-ATN-OBJ-ID      TO WR3-ATN-OBJ-ID.
           MOVE WS-PTM-GLT-OBJ-ID      TO WR3-GLT-OBJ-ID.
           MOVE WS-PTM-GML-OBJ-ID      TO WR3-GML-OBJ-ID.
           MOVE WS-PTM-CURRENCY-CODE (I2)
                                       TO WR3-CURRENCY-CODE.
           MOVE WS-PTM-CURR-ND (I2)    TO WR3-CURR-ND.
           MOVE WS-PTM-CO-CURR-CD (I2) TO WR3-CO-CURR-CD.
           MOVE WS-PTM-DST-CO-CURR-CD (I2)
                                       TO WR3-DST-CO-CURR-CD .
           MOVE WS-PTM-CO-EXCH-RATE (I2)
                                       TO WR3-CO-EXCH-RATE.
           MOVE WS-PTM-DIST-EXCH-RATE (I2)
                                       TO WR3-DIST-EXCH-RATE.
           INITIALIZE                     WR3-ERROR-FLAG.
           PERFORM VARYING PRD-I4 FROM 1 BY 1
             UNTIL (PRD-I4 > 10)
                MOVE WS-PTM-MSG-NBR (I2,PRD-I4)
                                       TO WR3-MSG-NBR (PRD-I4)
                IF  (WR3-ERROR-FLAG   = SPACES)
                AND (WS-PTM-MSG-NBR (I2,PRD-I4) NOT = ZEROS)
                     MOVE "Y"          TO WR3-ERROR-FLAG
                END-IF
           END-PERFORM.
           MOVE WS-PTM-HM-PROC-LEVEL (I2)
                                       TO WR3-HM-PROC-LEVEL.
165300     MOVE WS-PTM-AC-UPDATED (I2) TO WR3-AC-UPDATED.
           MOVE WS-PTM-GARN-OBJ-ID(I2) TO WR3-GARN-OBJ-ID.

           MOVE WS-PTM-SEGMENT-SEQ(I2) TO WR3-SEGMENT-SEQ.
           MOVE WS-PTM-SEGMENT-BLOCK   TO WR3-SEGMENT-BLOCK.
165400
           MOVE WS-PTM-TR-DATE(I2)        TO WR3-TR-DATE.
GMDIST     MOVE WS-PTM-PER-END-DATE(I2)   TO WR3-PER-END-DATE.

           WRITE WORK3-REC             FROM WR3-WORK3-REC.
165600
170100 242-END.
170200     EXIT.
GMDIST****************************************************************
GMDIST 243-MOVE-TBL-TO-GMWORK         SECTION.
GMDIST****************************************************************
GMDIST 243-START.
GMDIST
GMDIST     INITIALIZE GMWORK-REC.
GMDIST
GMDIST*** FOR SORTING RECORDS TO PRINT FOR THE GM REPORT
GMDIST*** 1 = EARNINGS DISTRIBUTION
GMDIST*** 9 = COMPANY PAID DEDUCTIONS 
GMDIST     IF (WS-PTM-DED-CODE  = SPACES)
GMDIST         MOVE 1                    TO GM3-DEDUCTION-TYPE
GMDIST     ELSE
GMDIST         MOVE 9                    TO GM3-DEDUCTION-TYPE.
GMDIST
GMDIST*** FOR SORTING RECORDS TO PRINT FOR THE GM REPORT
GMDIST*** 1 = RECORDS DISTRIBUTED WITH GMLABDISTX
GMDIST*** 2 = RECORDS PROCESSED WITH NORMAL PR PROCESSSING
GMDIST*** 3 = ERROR RECORDS RELATED TO LABOR DISTRIBUTION
GMDIST     IF  (WS-PTM-SALARY-PCT(I2)     = ZEROS)
GMDIST     AND (WS-PTM-EFFORT-PCT(I2)     = ZEROS)
GMDIST         MOVE 3                    TO GM3-RECORD-TYPE
GMDIST     ELSE
GMDIST     IF  (WS-PTM-SALARY-PCT(I2)      = ZEROS)
GMDIST     AND (WS-PTM-EFFORT-PCT(I2)  NOT = ZEROS)
GMDIST     OR  ((WS-PTM-SALARY-PCT(I2) NOT = ZEROS)
GMDIST     AND  (WS-PTM-EFFORT-PCT(I2)     = ZEROS))
GMDIST         MOVE 2                    TO GM3-RECORD-TYPE
           ELSE
GMDIST         MOVE 1                    TO GM3-RECORD-TYPE.
GMDIST
GMDIST     MOVE PRM-COMPANY              TO GM3-COMPANY.
GMDIST     MOVE PYM-EMPLOYEE             TO GM3-EMPLOYEE.
GMDIST     MOVE HRWS-FORMAT-NAME         TO GM3-FULL-NAME.
GMDIST     MOVE WS-PTM-PROCESS-LEVEL     TO GM3-PROCESS-LEVEL.
GMDIST     MOVE WS-PTM-DEPARTMENT        TO GM3-DEPARTMENT.
GMDIST     MOVE WS-PTM-JOB-CODE          TO GM3-JOB-CODE.
GMDIST     MOVE WS-PTM-DED-CODE          TO GM3-DED-CODE.
GMDIST     MOVE WS-PTM-POSITION          TO GM3-POSITION.
GMDIST     MOVE WS-PTM-DIST-COMPANY      TO GM3-TO-COMPANY.
GMDIST     MOVE WS-PTM-DST-ACCT-UNIT     TO GM3-ACCT-UNIT.
GMDIST     MOVE WS-PTM-DST-ACCOUNT       TO GM3-ACCOUNT.
GMDIST     MOVE WS-PTM-DST-SUB-ACCT      TO GM3-SUB-ACCOUNT.
GMDIST     MOVE WS-PTM-ACTIVITY          TO GM3-ACTIVITY.
GMDIST     MOVE WS-PTM-ACCT-CATEGORY     TO GM3-ACCT-CATEGORY.
GMDIST
GMDIST     MOVE WS-PTM-OT-PLAN-CODE(I2)  TO GM3-OT-PLAN-CODE.
GMDIST     MOVE WS-PTM-PER-END-DATE(I2)  TO GM3-PER-END-DATE.
GMDIST     MOVE WS-PTM-TR-DATE     (I2)  TO GM3-TR-DATE.
GMDIST     MOVE WS-PTM-PAY-CODE    (I2)  TO GM3-PAY-CODE.
GMDIST     MOVE WS-PTM-EFFORT-PCT  (I2)  TO GM3-EFFORT-PCT.
GMDIST     MOVE WS-PTM-SALARY-PCT  (I2)  TO GM3-SALARY-PCT.
GMDIST     MOVE WS-PTM-EFFECT-DATE (I2)  TO GM3-EFFECT-DATE.
GMDIST     MOVE WS-PTM-DIST-HOURS  (I2)  TO GM3-HOURS-AMOUNT.
GMDIST     MOVE WS-PTM-DIST-AMT    (I2)  TO GM3-DIST-AMT.
GMDIST     MOVE WS-PTM-WAGE-AMOUNT (I2)  TO GM3-WAGE-AMOUNT.
           MOVE WS-PTM-GM-LINE-NBR (I2)  TO GM3-GM-LINE-NBR.
GMDIST     MOVE WS-PTM-GM-JOB-CODE (I2)  TO GM3-GM-JOB-CODE.
GMDIST     MOVE WS-PTM-GM-POSITION (I2)  TO GM3-GM-POSITION.
GMDIST     MOVE WS-PTM-GM-PAY-CODE (I2)  TO GM3-GM-PAY-CODE.
           MOVE WS-PTM-UPDATE-EFFORT(I2) TO GM3-UPDATE-EFFORT.
GMDIST     MOVE WS-PTM-TR-JOB-CODE (I2)  TO GM3-TR-JOB-CODE.
GMDIST     MOVE WS-PTM-TR-POSITION (I2)  TO GM3-TR-POSITION.
GMDIST     MOVE WS-PTM-TR-PAY-CODE (I2)  TO GM3-TR-PAY-CODE.
J47059     MOVE WS-PTM-PCD-SEQ-NBR       TO GM3-TR-PCD-SEQ-NBR.
GMDIST     MOVE WS-PTM-EFFORT-FLAG (I2)  TO GM3-EFFORT-FLAG.
GMDIST     MOVE WS-PTM-LAB-DIST-FLAG(I2) TO GM3-LAB-DIST-FLAG.
J53283     MOVE WS-PTM-EFFORT-AMT (I2)   TO GM3-EFFORT-AMT.
GMDIST     MOVE PYM-CHECK-ID             TO GM3-CHECK-ID.
           MOVE WS-PTM-GML-OBJ-ID        TO GM3-GML-OBJ-ID.
GMDIST     MOVE WS-PTM-CURRENCY-CODE(I2) TO GM3-CURRENCY-CODE.
GMDIST     INITIALIZE                       GM3-ERROR-NBR.
GMDIST
GMDIST     WRITE GMWORK-REC          FROM GM3-GMWORK-REC.
GMDIST
GMDIST 243-END.
GMDIST     EXIT.
193300****************************************************************
193400 244-CREATE-CLEARING-DISTRIB     SECTION.
193500****************************************************************
193600 244-START.
193700
           ADD 1                           TO PTM.
      *
           IF (PTM > WS-PTM-TABLE-SIZE)
      *--------Table size exceeded, WS-PTM Table size must be increased
               MOVE "PRPRD"                TO CRT-ERROR-CAT
               MOVE 107                    TO CRT-MSG-NBR
               MOVE WS-PTM-TABLE-SIZE      TO CRT-ERR-VAR1
               PERFORM 780-DISPLAY-MSG
      *--------Following move will cause subscript out of range so that 
      *--------program aborts and restart logic initiates.
               MOVE WS-PTM-KEY             TO WS-PTM-TBL-KEY (PTM)
           END-IF.

194000     MOVE WS-CL-DATE (I1)            TO WS-PTM-GL-DATE.
194100     MOVE HRPRS-CL-DIST-CO           TO WS-PTM-DIST-COMPANY.
194200     MOVE HRPRS-CL-ACCT-UNIT         TO WS-PTM-DST-ACCT-UNIT.
194300     MOVE HRPRS-CL-ACCOUNT           TO WS-PTM-DST-ACCOUNT.
194400     MOVE HRPRS-CL-SUB-ACCT          TO WS-PTM-DST-SUB-ACCT.
P55429     MOVE WS-CL-PROC-LEV (I1)        TO WS-PTM-PROCESS-LEVEL.
P11768     MOVE PYM-PROCESS-LEVEL          TO  
P55429                                     WS-PTM-HM-PROC-LEVEL (PTM). 
194600     MOVE SPACES                     TO WS-PTM-DEPARTMENT.
194700     MOVE ZEROS                      TO WS-PTM-PCD-SEQ-NBR.
194800     MOVE SPACES                     TO WS-PTM-ACTIVITY.
194900     MOVE SPACES                     TO WS-PTM-ACCT-CATEGORY.
195000     MOVE SPACES                     TO WS-PTM-DED-CODE.
195100     MOVE SPACES                     TO WS-PTM-JOB-CODE.
195200     MOVE SPACES                     TO WS-PTM-POSITION.
195300     MOVE SPACES                     TO WS-PTM-ATTEND-CODE.
195400     MOVE WS-CL-TYPE (I1)            TO WS-PTM-RECORD-TYPE.
195500     MOVE "Y"                        TO WS-PTM-CLEARING.
195600     MOVE ZEROS                      TO WS-PTM-ATN-OBJ-ID
                                              WS-PTM-GLT-OBJ-ID
                                              WS-PTM-GARN-ID.
           INITIALIZE                      WS-PTM-SEGMENT-BLOCK.
           INITIALIZE                      WS-PTM-SEGMENT-SEQ (PTM).
           INITIALIZE                      WS-PTM-SEGMENT-FLAG (PTM).
195700     MOVE WS-PTM-KEY                 TO WS-PTM-TBL-KEY (PTM).
195800     MOVE ZEROS                      TO WS-PTM-DIST-HOURS (PTM).
UOM        MOVE ZEROES                     TO 
UOM                                        WS-PTM-DIST-PAY-UNITS(PTM).
UOM        MOVE SPACES                     TO WS-PTM-UNIT-MEASURE (PTM).
195900     MOVE WS-CL-AMT (I1)             TO WS-PTM-DIST-AMT (PTM).
           INITIALIZE                      WS-PTM-PAY-SUM-GRP (PTM)
                                           WS-PTM-DDC-DESC (PTM)
                                           WS-PTM-PAY-CODE (PTM).
196000     MOVE ZEROS                      TO WS-PTM-OBJ-ID (PTM).
           INITIALIZE                      WS-PTM-GARN-OBJ-ID (PTM).

P96085     MOVE PRPRD-CO-CURRENCY-CODE TO WS-PTM-CO-CURR-CD (PTM).
P96085     MOVE PRPRD-CURRENCY-CODE    TO WS-PTM-CURRENCY-CODE (PTM).
P96085     PERFORM 245-GET-EXCH-RATE-FOR-CLEARING.
P96085     MOVE GLM-CURRENCY-CODE      TO WS-PTM-DST-CO-CURR-CD(PTM).
           MOVE 2                      TO WS-PTM-CURR-ND (PTM).
P96085     MOVE WS-CO-EXCH-RATE        TO WS-PTM-CO-EXCH-RATE (PTM).
P96085     MOVE WS-DST-CO-EXCH-RATE    TO WS-PTM-DIST-EXCH-RATE (PTM).

           PERFORM VARYING PRD-I4 FROM 1 BY 1
             UNTIL (PRD-I4 > 10)
                    INITIALIZE             WS-PTM-MSG-NBR (PTM PRD-I4)
           END-PERFORM.

196100     PERFORM
196200         VARYING PTM1 FROM 1 BY 1
196300         UNTIL  (WS-PTM-TBL-KEY (PTM1) = WS-PTM-KEY)
196400
196500         CONTINUE
196600     END-PERFORM.
196700     IF (PTM1 NOT = PTM)
196800         ADD WS-CL-AMT (I1)          TO WS-PTM-DIST-AMT (PTM1)
196900         SUBTRACT 1                  FROM PTM
197000     END-IF.

GMDIST     INITIALIZE WS-PTM-EFFORT-PCT(PTM)
GMDIST                WS-PTM-SALARY-PCT(PTM).

201400 244-END.
201500     EXIT.
080800****************************************************************
080900 245-GET-EXCH-RATE-FOR-CLEARING SECTION.
081000****************************************************************
081100 245-START.
      *determine tran to base exchange rate

           IF (IFGRWS-COMPANY         NOT = PRPYM-COMPANY)
           OR (IFGRWS-FR-CURR-CODE    NOT = PRPRD-CURRENCY-CODE)
           OR (IFGRWS-TO-CURR-CODE    NOT = PRPRD-CO-CURRENCY-CODE)
           OR (IFGRWS-SYSTEM          NOT = "HR")
           OR (IFGRWS-EFFECT-DATE     NOT = PRPRD-GL-DATE)
           OR (IFGRWS-SELL-RATE           = ZEROES)
               MOVE PRPRD-GL-DATE           TO IFGRWS-EFFECT-DATE
               MOVE PRPYM-COMPANY           TO IFGRWS-COMPANY
               MOVE PRPRD-CURRENCY-CODE     TO IFGRWS-FR-CURR-CODE
               MOVE PRPRD-CO-CURRENCY-CODE  TO IFGRWS-TO-CURR-CODE
               MOVE "HR"                    TO IFGRWS-SYSTEM
               MOVE PRPRD-GL-DATE           TO IFGRWS-EFFECT-DATE
               PERFORM 660-GET-CURRENCY-RATE-60.

           MOVE IFGRWS-SELL-RATE        TO WS-CO-EXCH-RATE.

      *determine tran to to-company exchange rate
           MOVE HRPRS-CL-DIST-CO        TO IFGRWS-COMPANY.
           MOVE PRPRD-CURRENCY-CODE     TO IFGRWS-FR-CURR-CODE.
           MOVE HRPRS-CL-DIST-CO        TO DB-COMPANY.
           MOVE HRPRS-CL-ACCT-UNIT      TO DB-ACCT-UNIT.
           MOVE HRPRS-CL-ACCOUNT        TO DB-ACCOUNT.
           MOVE HRPRS-CL-SUB-ACCT       TO DB-SUB-ACCOUNT.
           PERFORM 840-FIND-GLMSET2.
           MOVE GLM-CURRENCY-CODE       TO IFGRWS-TO-CURR-CODE.
           MOVE "HR"                    TO IFGRWS-SYSTEM.
           MOVE PRPRD-GL-DATE           TO IFGRWS-EFFECT-DATE.

           IF (IFGRWS-COMPANY         NOT = HRPRS-CL-DIST-CO)
           OR (IFGRWS-FR-CURR-CODE    NOT = PRPRD-CURRENCY-CODE)
           OR (IFGRWS-TO-CURR-CODE    NOT = GLM-CURRENCY-CODE)
           OR (IFGRWS-SYSTEM          NOT = "HR")
           OR (IFGRWS-EFFECT-DATE     NOT = PRPRD-GL-DATE)
           OR (IFGRWS-SELL-RATE           = ZEROES)
               PERFORM 660-GET-CURRENCY-RATE-60.

           MOVE IFGRWS-SELL-RATE        TO WS-DST-CO-EXCH-RATE.

P06255*----reset db company to "from company" as it may have changed 
P06255*----in 660-get-currency-rate-60. 
P06255     MOVE PRPYM-COMPANY           TO DB-COMPANY.

       245-END.
           EXIT.

GMDIST****************************************************************
GMDIST 250-MOVE-ERR-TBL-TO-GMWORK       SECTION.
GMDIST****************************************************************
GMDIST 250-START.
GMDIST
GMDIST     INITIALIZE GMWORK-REC.
GMDIST
GMDIST*** FOR SORTING RECORDS TO PRINT FOR THE GM REPORT
GMDIST*** 1 = RECORDS DISTRIBUTED WITH GMLABDISTX
GMDIST*** 2 = RECORDS PROCESSED WITH NORMAL PR PROCESSSING
GMDIST*** 3 = ERROR RECORDS RELATED TO LABOR DISTRIBUTION
GMDIST
GMDIST     MOVE 4                         TO GM3-RECORD-TYPE.
GMDIST     MOVE PRM-COMPANY               TO GM3-COMPANY.
GMDIST     MOVE PYM-EMPLOYEE              TO GM3-EMPLOYEE.
GMDIST     MOVE HRWS-FORMAT-NAME          TO GM3-FULL-NAME.
GMDIST     MOVE WS-ERR-PROCESS-LEVEL(I2)  TO GM3-PROCESS-LEVEL.
GMDIST     MOVE WS-ERR-DEPARTMENT   (I2)  TO GM3-DEPARTMENT.
GMDIST     INITIALIZE                        GM3-DED-CODE.
GMDIST     MOVE WS-ERR-TR-JOB-CODE  (I2)  TO GM3-JOB-CODE.
GMDIST     MOVE WS-ERR-TR-POSITION  (I2)  TO GM3-POSITION.
GMDIST     MOVE WS-ERR-TR-PAY-CODE  (I2)  TO GM3-TR-PAY-CODE
                                             GM3-PAY-CODE.
GMDIST     MOVE WS-ERR-PER-END-DATE (I2)  TO GM3-PER-END-DATE.
GMDIST     MOVE WS-ERR-TR-DATE      (I2)  TO GM3-TR-DATE.
GMDIST     MOVE WS-ERR-EFFECT-DATE  (I2)  TO GM3-EFFECT-DATE.
GMDIST     MOVE WS-ERR-GM-JOB-CODE  (I2)  TO GM3-GM-JOB-CODE.
GMDIST     MOVE WS-ERR-GM-POSITION  (I2)  TO GM3-GM-POSITION.
GMDIST     MOVE WS-ERR-GM-PAY-CODE  (I2)  TO GM3-GM-PAY-CODE.
GMDIST     MOVE WS-ERR-GM-LINE-NBR  (I2)  TO GM3-GM-LINE-NBR.
           IF (GM3-GM-LINE-NBR  NOT = ZEROS)
               MOVE 1                     TO GM3-RECORD-TYPE.
GMDIST
GMDIST     MOVE WS-ERR-ERROR-NBR    (I2)  TO GM3-ERROR-NBR.
GMDIST     MOVE WS-ERR-MESSAGE      (I2)  TO GM3-ERR-MESSAGE.
GMDIST
GMDIST     INITIALIZE                       GM3-TO-COMPANY
GMDIST                                      GM3-ACCT-UNIT
GMDIST                                      GM3-ACCOUNT
GMDIST                                      GM3-SUB-ACCOUNT
GMDIST                                      GM3-ACTIVITY
GMDIST                                      GM3-ACCT-CATEGORY
GMDIST                                      GM3-EFFORT-PCT
GMDIST                                      GM3-SALARY-PCT
GMDIST                                      GM3-HOURS-AMOUNT
GMDIST                                      GM3-DIST-AMT
GMDIST                                      GM3-WAGE-AMOUNT
GMDIST                                      GM3-CHECK-ID
GMDIST                                      GM3-CURRENCY-CODE.
GMDIST
GMDIST     WRITE GMWORK-REC          FROM GM3-GMWORK-REC.
GMDIST
GMDIST 250-END.
GMDIST     EXIT.
080800****************************************************************
080900 260-CLOSE-PARTIAL-ACHS-VOIDED   SECTION.
081000****************************************************************
081100 260-START.
081500
081600     MOVE PRM-COMPANY            TO DB-COMPANY.

           IF (UPDATING)
P85397*        CHANGED FROM BEGRNG TO NEXT FOR PERFORMANCE REASONS           
P85397         PERFORM 850-MODIFY-NLT-ACDSET3
P85397         UNTIL (EMPACHDIST-NOTFOUND)
P85397         OR    (ACD-COMPANY NOT = DB-COMPANY)
           ELSE
P85397         PERFORM 850-FIND-NLT-ACDSET3
P85397         UNTIL (EMPACHDIST-NOTFOUND)
P85397         OR    (ACD-COMPANY NOT = DB-COMPANY)
P85397     END-IF.

081900     PERFORM 262-UPDATE-CURRENT
082000         UNTIL (EMPACHDIST-NOTFOUND)
P85397         OR    (ACD-COMPANY NOT = DB-COMPANY).
082100
082500 260-END.
082600     EXIT.
082700
082800****************************************************************
082900 262-UPDATE-CURRENT              SECTION.
083000****************************************************************
083100 262-START.
083200
083300     MOVE ACD-COMPANY            TO DB-COMPANY.
083400     MOVE ACD-EMPLOYEE           TO DB-EMPLOYEE.
083500     MOVE ACD-CHECK-ID           TO DB-CHECK-ID .
083600     PERFORM 840-FIND-PYMSET1.
083800     IF (PRS-PROCESS-LEVEL NOT = PYM-PROCESS-LEVEL)
083900         GO TO 262-NEXT.
084000
084100     MOVE ACD-COMPANY            TO DB-COMPANY.
084200     MOVE ACD-EMPLOYEE           TO DB-EMPLOYEE.
084300     MOVE ACD-REVERSAL-ID        TO DB-CHECK-ID .
084400     MOVE PRDSET1-CHECK-ID       TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PRDSET1.

P10582     MOVE ACD-COMPANY            TO DB-COMPANY.
P10582     MOVE ACD-EMPLOYEE           TO DB-EMPLOYEE.
P10582     PERFORM 840-FIND-EMPSET1.

P27928     IF  (PYM-CHECK-TYPE = "P")
P10582         GO TO 262-BYPASS-DISTRIBS.

084600     PERFORM 265-CREATE-REV-OF-VOIDED-ACHS
084700     UNTIL   (PRDISTRIB-NOTFOUND).

P10582 262-BYPASS-DISTRIBS.
084800
           IF (UPDATING)
084900         MOVE SPACES             TO ACD-CURRENT-FLAG
085000         PERFORM 820-STORE-EMPACHDIST.
085100
085200 262-NEXT.
           IF (UPDATING)
P85397         PERFORM 860-MODIFY-NEXT-ACDSET3
           ELSE
P85397         PERFORM 860-FIND-NEXT-ACDSET3
P85397     END-IF.
085400
085500 262-END.
085600     EXIT.
085700
085800****************************************************************
085900 265-CREATE-REV-OF-VOIDED-ACHS   SECTION.
086000****************************************************************
086100 265-START.
086200
           IF (PRD-HM-PROC-LEVEL NOT = PRS-PROCESS-LEVEL)
               GO TO 265-NEXT.

           IF (PRM-REPORT-SEQ = "P")
               MOVE PRD-PROCESS-LEVEL  TO WR3-PROCESS-LEVEL-SRT
               MOVE PRD-DEPARTMENT     TO WR3-DEPARTMENT-SRT
           ELSE
               INITIALIZE                 WR3-PROCESS-LEVEL-SRT
                                          WR3-DEPARTMENT-SRT.

080200     ADD 1                       TO WS-WR3-SEQ-NBR.
080300     MOVE WS-WR3-SEQ-NBR         TO WR3-SEQ-NBR.
080700     MOVE "R"                    TO WR3-CHECK-TYPE.
081300     MOVE PRD-GL-DATE            TO WR3-GL-DATE.
081500     MOVE PRD-DIST-COMPANY       TO WR3-TO-COMPANY.
081700     MOVE PRD-DST-ACCT-UNIT      TO WR3-ACCT-UNIT.
081900     MOVE PRD-DST-ACCOUNT        TO WR3-ACCOUNT.
082100     MOVE PRD-DST-SUB-ACCT       TO WR3-SUB-ACCOUNT.
082300     MOVE PRD-PROCESS-LEVEL      TO WR3-PROCESS-LEVEL.
082400     MOVE PRD-DEPARTMENT         TO WR3-DEPARTMENT.
082500     MOVE PRD-HOURS              TO WR3-DIST-HOURS.
UOM        MOVE PRD-PAY-UNITS          TO WR3-DIST-PAY-UNITS.
UOM        MOVE PRD-UNIT-MEASURE       TO WR3-UNIT-MEASURE.
082700     MOVE PRD-DIST-AMT           TO WR3-TRAN-AMOUNT
                                          WR3-DIST-AMT.
           MOVE 2                      TO WR3-TRAN-ND.
082900     MOVE PRD-EMPLOYEE           TO WR3-EMPLOYEE.
           MOVE PRD-COMPANY            TO DB-COMPANY.
           MOVE PRD-EMPLOYEE           TO DB-EMPLOYEE.
           PERFORM 840-FIND-EMPSET1.

           MOVE EMP-LAST-NAME          TO HRWS-LAST-NAME.
           MOVE EMP-FIRST-NAME         TO HRWS-FIRST-NAME.
           MOVE EMP-MIDDLE-INIT        TO HRWS-MIDDLE-INIT.
           MOVE EMP-NAME-SUFFIX        TO HRWS-NAME-SUFFIX.
           MOVE EMP-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE.
           PERFORM 750-HR-FORMAT-NAME.
           MOVE HRWS-FORMAT-NAME       TO WR3-FULL-NAME.

083000     IF  ((PRD-PCD-SEQ-NBR    NOT = ZEROS)
GRANTM     AND  (PRD-DED-CODE           = SPACES))
083100         MOVE PRD-COMPANY        TO DB-COMPANY
J57679*        MOVE PRD-PCD-SEQ-NBR    TO DB-SEQ-NBR
J57679*        PERFORM 840-FIND-PCDSET5

J57679         IF (PRD-PCD-SEQ-NBR > 9999)
J57679             MOVE PRD-PCD-SEQ-NBR     TO DB-PCD-SEQ-NBR
J57679             PERFORM 840-FIND-PCDSET7
J57679         ELSE
J57679             MOVE PRD-PCD-SEQ-NBR     TO DB-SEQ-NBR
J57679             PERFORM 840-FIND-PCDSET5
J57679         END-IF 

083400         MOVE PCD-PAY-CODE       TO WR3-PAY-CODE
083500         MOVE PCD-PAY-SUM-GRP    TO WR3-PAY-SUM-GRP
083600         MOVE PRD-PCD-SEQ-NBR    TO WR3-PCD-SEQ-NBR
083700     ELSE
083800         MOVE SPACES             TO WR3-PAY-CODE
083900                                    WR3-PAY-SUM-GRP
GRANTM         MOVE PRD-PCD-SEQ-NBR    TO WR3-PCD-SEQ-NBR.
084100     MOVE PRD-ACTIVITY           TO WR3-ACTIVITY.
084200     MOVE PRD-ACCT-CATEGORY      TO WR3-ACCT-CATEGORY.
084300     MOVE SPACES                 TO WR3-DDC-DESC.
084500
084600     MOVE PRD-DED-CODE           TO WR3-DED-CODE.
084700     MOVE PRD-JOB-CODE           TO WR3-JOB-CODE.
084800     MOVE PRD-POSITION           TO WR3-POSITION.
084900     MOVE "C"                    TO WR3-RECORD-TYPE.
085000     MOVE PYM-CHECK-ID           TO WR3-CHECK-ID.
           MOVE PYM-CHECK-NBR          TO WR3-CHECK-NBR.

           MOVE HRPRS-PRD-OPTION       TO WR3-PRD-OPTION.
           MOVE PRD-ATTEND-CODE        TO WR3-ATTEND-CODE.
           MOVE PRD-ATN-OBJ-ID         TO WR3-ATN-OBJ-ID.
           MOVE PRD-GLT-OBJ-ID         TO WR3-GLT-OBJ-ID.
           MOVE PRD-HM-PROC-LEVEL      TO WR3-HM-PROC-LEVEL.
           MOVE PRD-AC-UPDATED         TO WR3-AC-UPDATED.
           MOVE PRD-GARN-OBJ-ID        TO WR3-GARN-OBJ-ID.
           MOVE PRD-SEGMENT-SEQ        TO WR3-SEGMENT-SEQ.

           MOVE PRD-COMPANY            TO DB-COMPANY.
           MOVE PRD-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE PRD-SEGMENT-SEQ        TO DB-TIME-SEQ.

           INITIALIZE                     WR3-SEGMENT-BLOCK.
           IF (PRD-SEGMENT-SEQ  NOT = ZEROS)
               PERFORM 840-FIND-TSGSET1
               IF (TRSGBLCK-FOUND)
                   MOVE TRS-SEGMENT-BLOCK
                                       TO WR3-SEGMENT-BLOCK.

085100     WRITE WORK3-REC             FROM WR3-WORK3-REC.
085500
086800 265-NEXT.

           PERFORM 860-FIND-NXTRNG-PRDSET1.
087000
087100 265-END.

219900****************************************************************
220000 300-GL-REPORT                   SECTION.
220100****************************************************************
220200 300-START.
220300
           PERFORM 900-BUILD-TMP-FILE-NAME.
           MOVE WS-TMP-FILE            TO WS-ERRORS-FILE-NAME.
           OPEN OUTPUT ERRORS-FILE.

           OPEN OUTPUT GL-DETAIL-FILE.

223400     SORT SORT-FILE
223500         ASCENDING KEY           DSRT-COMPANY
                                       DSRT-PROCESS-LEVEL-SRT
                                       DSRT-DEPARTMENT-SRT
223600                                 DSRT-TO-COMPANY
223700                                 DSRT-GL-DATE
223800                                 DSRT-ACCT-UNIT
223900                                 DSRT-ACCOUNT
224000                                 DSRT-SUB-ACCOUNT
                                       DSRT-EMPLOYEE
224200         USING                   WORK3-FILE
224300         GIVING                  WORK3-FILE SAVE.
224400
224500     SET NO-END-OF-FILE          TO TRUE.
224600
J65254*    OPEN INPUT WORK3-FILE.
J65254     OPEN I-O   WORK3-FILE.
224800
224900     READ WORK3-FILE             INTO WR3-WORK3-REC
225000         AT END
225100             SET END-OF-FILE     TO TRUE
225200             GO TO 300-CLOSE.
225300
           IF (UPDATING)
225400         PERFORM 910-AUDIT-BEGIN.

PRBRD      IF (PRBRD-BROADCAST-ENABLED)
PRBRD          MOVE 500                    TO PRBRD-ELEMENT-TYPE
PRBRD          MOVE PRM-COMPANY            TO PRBRD-ELEMENT-VALUE
PRBRD          PERFORM 610-BEG-ELEMENT-TAG
PRBRD          MOVE WFCHWS-TAG-STRING      TO BTL1-TAG-LINE
PRBRD          MOVE BROADCAST-TAG-LINE-1   TO RPT-GROUP-REQUEST
PRBRD          PERFORM 700-PRINT-RPT-GRP.
225500
225600     ADD 1                       TO WS-RECORD-COUNT.
           PERFORM 306-ACCOUNT-CONTROL
               UNTIL (END-OF-FILE).

225900
226000     CLOSE WORK3-FILE.
226100
           IF (UPDATING)
226200         PERFORM 925-AUDIT-END.
226300
       300-CLOSE.

           CLOSE ERRORS-FILE.
           CLOSE GL-DETAIL-FILE.

           IF (PRM-DETAIL-RPT-OPT = "N")
               MOVE GL-DETAIL-NAME     TO WS-TMP-FILE
               PERFORM 901-REMOVE-TMP-FILE.

227400 300-END.
227500     EXIT.
      *****************************************************************
       306-ACCOUNT-CONTROL             SECTION.
      *****************************************************************
       306-START.

           MOVE WS-RS-RUN-DATE         TO PH-RUN-DATE
                                          R2PH-RUN-DATE.
           MOVE WS-RS-RUN-TIME         TO PH-RUN-TIME
                                          R2PH-RUN-TIME.
228100     MOVE PRM-COMPANY            TO PH-COMPANY
                                          R2PH-COMPANY.
228200     MOVE WS-CO-NAME             TO PH-NAME
                                          R2PH-NAME.

230100     MOVE WR3-COMPANY            TO WS-COMPANY.

           IF (PRM-REPORT-SEQ = "A")
               MOVE PAGE-HEADING       TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
               IF (PRM-DETAIL-RPT-OPT = "Y")
                   MOVE R2-PAGE-HEADING TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP.

           IF (PRM-REPORT-SEQ = "A")
               INITIALIZE                 WS-PROCESS-LEVEL
                                          WS-DEPARTMENT.

           PERFORM 307-COMPANY-CONTROL
               UNTIL (END-OF-FILE)
               OR    (WR3-COMPANY           NOT = WS-COMPANY).

       306-END.
           EXIT.
092800*
092500*****************************************************************
092600 307-COMPANY-CONTROL       SECTION.
092700*****************************************************************
       307-START.
092800*
092900     MOVE ZEROS                  TO WS-PROC-LEV-HOURS
093000                                    WS-PROC-LEV-AMOUNT.
093400
           MOVE PRM-COMPANY            TO DB-COMPANY.
093500     MOVE WR3-PROCESS-LEVEL-SRT  TO DH-PROCESS-LEVEL
                                          R2DH-PROCESS-LEVEL
093600                                    WS-PROCESS-LEVEL
093700                                    DB-PROCESS-LEVEL.
093800*
           IF (PRM-REPORT-SEQ = "P")
093900         PERFORM 840-FIND-PRSSET1
094000         MOVE PRS-NAME           TO DH-PL-NAME
                                          R2DH-PL-NAME
PRBRD          IF (PRBRD-BROADCAST-ENABLED)
PRBRD              MOVE 501                    TO PRBRD-ELEMENT-TYPE
PRBRD              MOVE WS-PROCESS-LEVEL       TO PRBRD-ELEMENT-VALUE
PRBRD              PERFORM 610-BEG-ELEMENT-TAG
PRBRD              MOVE WFCHWS-TAG-STRING      TO BTL1-TAG-LINE
PRBRD              MOVE BROADCAST-TAG-LINE-1   TO RPT-GROUP-REQUEST
PRBRD              PERFORM 700-PRINT-RPT-GRP
PRBRD          END-IF
           ELSE
094000         INITIALIZE                 DH-PL-NAME
                                          R2DH-PL-NAME.
094100
094200     PERFORM 308-PROCESS-LEVEL-CONTROL
094400         UNTIL (END-OF-FILE)
               OR    (WR3-COMPANY           NOT = WS-COMPANY)
094500         OR    (WR3-PROCESS-LEVEL-SRT NOT = WS-PROCESS-LEVEL).

           IF  (PRM-REPORT-SEQ      = "P")
PRBRD      AND (PRBRD-BROADCAST-ENABLED)
PRBRD          MOVE 501                    TO PRBRD-ELEMENT-TYPE
PRBRD          MOVE WS-PROCESS-LEVEL       TO PRBRD-ELEMENT-VALUE
PRBRD          PERFORM 615-END-ELEMENT-TAG
PRBRD          MOVE WFCHWS-TAG-STRING      TO BTL1-TAG-LINE
PRBRD          MOVE BROADCAST-TAG-LINE-1   TO RPT-GROUP-REQUEST
PRBRD          PERFORM 700-PRINT-RPT-GRP.

095300 307-END.
095400******************************************************************
095500 308-PROCESS-LEVEL-CONTROL          SECTION.
095600******************************************************************
       308-START.
095700*
095800     MOVE SPACES                 TO DH-CONTINUED.

095900     MOVE WR3-DEPARTMENT-SRT     TO DH-DEPARTMENT
                                          R2DH-DEPARTMENT
096000                                    WS-DEPARTMENT
096100                                    DB-DEPARTMENT.
096200*
096300     MOVE WR3-PROCESS-LEVEL      TO DB-PROCESS-LEVEL.

           INITIALIZE                     DH-DP-NAME
                                          R2DH-DP-NAME.
096400
           IF (PRM-REPORT-SEQ = "P")
096600         PERFORM 840-FIND-DPTSET1
               IF (DEPTCODE-FOUND)
096700             MOVE DPT-NAME       TO DH-DP-NAME
                                          R2DH-DP-NAME.
097000
097100     MOVE ZEROS                  TO WS-DEPT-HOURS
097200                                    WS-DEPT-AMOUNT.
097300*
           IF (PRM-REPORT-SEQ = "P")
PRBRD          IF (PRBRD-BROADCAST-ENABLED)
PRBRD              MOVE 502                    TO PRBRD-ELEMENT-TYPE
PRBRD              MOVE WS-DEPARTMENT          TO PRBRD-ELEMENT-VALUE
PRBRD              PERFORM 610-BEG-ELEMENT-TAG
PRBRD              MOVE WFCHWS-TAG-STRING      TO BTL1-TAG-LINE
PRBRD              MOVE BROADCAST-TAG-LINE-1   TO RPT-GROUP-REQUEST
PRBRD              PERFORM 700-PRINT-RPT-GRP
PRBRD          END-IF

               MOVE PAGE-HEADING       TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
097700         MOVE DEPARTMENT-HEADING TO RPT-GROUP-REQUEST
097800         PERFORM 700-PRINT-RPT-GRP
               IF (PRM-DETAIL-RPT-OPT = "Y")
                   MOVE R2-PAGE-HEADING    TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
                   MOVE R2-DEPARTMENT-HEADING
                                           TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP.
097900*
098200     PERFORM 310-DEPARTMENT-CONTROL
098400         UNTIL (END-OF-FILE)
               OR    (WR3-COMPANY           NOT = WS-COMPANY)
098500         OR    (WR3-PROCESS-LEVEL-SRT NOT = WS-PROCESS-LEVEL)
098600         OR    (WR3-DEPARTMENT-SRT    NOT = WS-DEPARTMENT).
098700*
           IF  (PRM-REPORT-SEQ      = "P")
PRBRD      AND (PRBRD-BROADCAST-ENABLED)
PRBRD          MOVE 502                    TO PRBRD-ELEMENT-TYPE
PRBRD          MOVE WS-DEPARTMENT          TO PRBRD-ELEMENT-VALUE
PRBRD          PERFORM 615-END-ELEMENT-TAG
PRBRD          MOVE WFCHWS-TAG-STRING      TO BTL1-TAG-LINE
PRBRD          MOVE BROADCAST-TAG-LINE-1   TO RPT-GROUP-REQUEST
PRBRD          PERFORM 700-PRINT-RPT-GRP.

099100 308-END.
227600******************************************************************
227700 310-DEPARTMENT-CONTROL          SECTION.
227800******************************************************************
227900 310-START.
228000*
228700     MOVE WR3-TO-COMPANY         TO DCH-DIST-COMPANY
                                          R2DCH-DIST-COMPANY
228800                                    IFCOWS-COMPANY.
228900     IF (WR3-TO-COMPANY          NOT = PRM-COMPANY)
229000         MOVE "PR"               TO IFCOWS-SYSTEM
229100         PERFORM 615-EDIT-COMPANY-60
229200         MOVE IFCOWS-GLS-NAME    TO DCH-GLS-NAME
                                          R2DCH-GLS-NAME
229300     ELSE
229400         MOVE WS-CO-NAME         TO DCH-GLS-NAME
                                          R2DCH-GLS-NAME.
229500*
229600     MOVE DIST-COMP-HEADING      TO RPT-GROUP-REQUEST.
229700     PERFORM 700-PRINT-RPT-GRP.
           IF (PRM-DETAIL-RPT-OPT = "Y")
               MOVE R2-DIST-COMP-HEADING
                                       TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP.
229800*
229900     MOVE ZEROS                  TO WS-SUB-TOT-AMT
230000                                    WS-SUB-TOT-HOURS
UOM                                       WS-SUB-TOT-PAY-UNITS.
230200     MOVE WR3-TO-COMPANY         TO WS-TO-COMPANY.
230300*
230400     PERFORM 311-DIST-COMPANY
230500         UNTIL (END-OF-FILE)
230600         OR    (WR3-COMPANY           NOT = WS-COMPANY)
               OR    (WR3-PROCESS-LEVEL-SRT NOT = WS-PROCESS-LEVEL)
               OR    (WR3-DEPARTMENT-SRT    NOT = WS-DEPARTMENT)
230700         OR    (WR3-TO-COMPANY        NOT = WS-TO-COMPANY).
230800*
230900 310-END.
231000     EXIT.
231100******************************************************************
231200 311-DIST-COMPANY                SECTION.
231300******************************************************************
231400 311-START.
231500*
231600     MOVE ZEROS                  TO WS-TOT-DEBIT-AMT
231700                                    WS-TOT-CREDIT-AMT
231800                                    WS-TOT-HOURS
UOM                                       WS-TOT-UNITS.
232000
232100     MOVE WR3-GL-DATE            TO WS-GL-DATE
232200                                    PDH-PAYMENT-DATE
                                          R2PDH-PAYMENT-DATE
232300                                    TL-PAYMENT-DATE.
232400*
232500     MOVE PAYMENT-DATE-HEADING   TO RPT-GROUP-REQUEST.
232600     PERFORM 700-PRINT-RPT-GRP.
           IF (PRM-DETAIL-RPT-OPT = "Y")
               MOVE R2-PAYMENT-DATE-HEADING
                                       TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP.

232700*
232800     MOVE ACCOUNT-HEADING        TO RPT-GROUP-REQUEST.
232900     PERFORM 700-PRINT-RPT-GRP.
           IF (PRM-DETAIL-RPT-OPT = "Y")
               MOVE R2-EMPLOYEE-HEADING TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP.
235300
235400     PERFORM 312-DATE-CONTROL
235500         UNTIL (END-OF-FILE)
235600         OR    (WR3-COMPANY           NOT = WS-COMPANY)
               OR    (WR3-PROCESS-LEVEL-SRT NOT = WS-PROCESS-LEVEL)
               OR    (WR3-DEPARTMENT-SRT    NOT = WS-DEPARTMENT)
235700         OR    (WR3-TO-COMPANY        NOT = WS-TO-COMPANY)
235800         OR    (WR3-GL-DATE           NOT = WS-GL-DATE).
235900*
236000     MOVE WS-TOT-DEBIT-AMT       TO TL-DEBIT-AMT.
236100     MOVE WS-TOT-CREDIT-AMT      TO TL-CREDIT-AMT.
236200     MOVE WS-TOT-HOURS           TO TL-HOURS.
           MOVE WS-TOT-UNITS           TO TL-UNITS.
236400*
236500     MOVE TOTAL-LINE             TO RPT-GROUP-REQUEST.
236600     PERFORM 700-PRINT-RPT-GRP.
236700*
236800 311-END.
236900     EXIT.
237000******************************************************************
237100 312-DATE-CONTROL                SECTION.
237200******************************************************************
237300 312-START.
237400*
DKR        MOVE "PR"                   TO IFACWS-SYSTEM.
237500     MOVE WR3-TO-COMPANY         TO WS-TO-COMPANY
                                          R2PL-DIST-COMPANY
                                          IFACWS-COMPANY.

           MOVE SPACES                 TO R2PL-CONTINUED.

237600     MOVE WR3-GL-DATE            TO WS-GL-DATE
                                          R2PL-POSTING-DATE.

237700     MOVE WR3-ACCT-UNIT          TO WS-DST-ACCT-UNIT
                                          R2PL-ACCT-UNIT
                                          IFACWS-ACCT-UNIT.

237800     MOVE WR3-ACCOUNT            TO WS-DST-ACCOUNT
                                          R2PL-ACCOUNT
                                          IFACWS-ACCOUNT.

237900     MOVE WR3-SUB-ACCOUNT        TO WS-DST-SUB-ACCT
                                          R2PL-SUB-ACCT
                                          IFACWS-SUB-ACCOUNT.
J15880
J15880     MOVE WR3-PCD-SEQ-NBR        TO WS-DST-PCD-SEQ-NBR.
J27195     MOVE WR3-HOURS-AMOUNT       TO WS-DST-HOURS-AMOUNT.
J27195     MOVE WR3-UNITS-AMOUNT       TO WS-DST-UNITS-AMOUNT.
238000
238100     MOVE ZEROS                  TO WS-DIST-AMT-TOT
238200                                    WS-DIST-HOURS-TOT
UOM                                       WS-DIST-UNITS-TOT.
UOM        MOVE SPACES                 TO WS-UNIT-MEASURE.
238300
P79800     MOVE ZEROS                  TO WS-ACCOUNT-ERROR-SW.

           IF (PRM-DETAIL-RPT-OPT = "Y")
               MOVE 0                  TO IFACWS-EDIT-TYPE
J47059         MOVE WR3-COMPANY        TO WS-TRD-DIST-COMPANY
J47059         MOVE WR3-PCD-SEQ-NBR    TO WS-TRD-PCD-SEQ-NBR
J27195         MOVE WR3-HOURS-AMOUNT   TO WS-TRD-HOURS
J27195         MOVE WR3-UNITS-AMOUNT   TO WS-TRD-PAY-UNITS
J47059         PERFORM 7850-GET-GLT-TYPE
               PERFORM 635-EDIT-GLMASTER-60
               IF (ERROR-FOUND)
P79800             SET ACCOUNT-ERROR-FOUND TO TRUE
                   MOVE CRT-ERROR-NBR  TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
                   MOVE CRT-MESSAGE    TO R2PL-DESCRIPTION
                   INITIALIZE             CRT-ERROR-NBR
               ELSE
                   MOVE IFACWS-GLM-DESCRIPTION
                                       TO R2PL-DESCRIPTION
               END-IF
               MOVE PRM-COMPANY        TO DB-COMPANY
               MOVE R2-ACCOUNT-LINE    TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP.

238400     PERFORM 313-ACCUM-TRANS-AMT
238500         UNTIL (END-OF-FILE)
238600         OR    (WR3-COMPANY           NOT = WS-COMPANY)
               OR    (WR3-PROCESS-LEVEL-SRT NOT = WS-PROCESS-LEVEL)
               OR    (WR3-DEPARTMENT-SRT    NOT = WS-DEPARTMENT)
238700         OR    (WR3-TO-COMPANY        NOT = WS-TO-COMPANY)
238800         OR    (WR3-GL-DATE           NOT = WS-GL-DATE)
238900         OR    (WR3-ACCT-UNIT         NOT = WS-DST-ACCT-UNIT)
239000         OR    (WR3-ACCOUNT           NOT = WS-DST-ACCOUNT)
239100         OR    (WR3-SUB-ACCOUNT       NOT = WS-DST-SUB-ACCT).
239200
239300     PERFORM 320-PRINT-ACCOUNT.
239400
239500 312-END.
239600     EXIT.
239700****************************************************************
239800 313-ACCUM-TRANS-AMT             SECTION.
239900****************************************************************
240000 313-START.
240100
           IF (WR3-ERROR-FLAG = "Y")
               WRITE ERRORS-REC             FROM WR3-WORK3-REC.

J15880     MOVE "PR"                   TO IFACWS-SYSTEM.
J15880     MOVE WR3-TO-COMPANY         TO IFACWS-COMPANY.  
J15880     MOVE WR3-ACCT-UNIT          TO IFACWS-ACCT-UNIT.             
J15880     MOVE WR3-ACCOUNT            TO IFACWS-ACCOUNT.    
J15880     MOVE WR3-SUB-ACCOUNT        TO IFACWS-SUB-ACCOUNT.
J15880     MOVE 0                      TO IFACWS-EDIT-TYPE.
J15880     MOVE WR3-COMPANY            TO WS-TRD-DIST-COMPANY.
J15880     MOVE WR3-PCD-SEQ-NBR        TO WS-TRD-PCD-SEQ-NBR.
J27195     MOVE WR3-HOURS-AMOUNT       TO WS-TRD-HOURS.
J27195     MOVE WR3-UNITS-AMOUNT       TO WS-TRD-PAY-UNITS.
J15880     PERFORM 7850-GET-GLT-TYPE.
J15880     PERFORM 635-EDIT-GLMASTER-60.
J15880     IF (ERROR-FOUND)
J15880         MOVE CRT-ERROR-NBR          TO CRT-MSG-NBR
J15880         PERFORM 790-GET-MSG
J15880         MOVE CRT-MESSAGE            TO PL-DESCRIPTION
J15880         INITIALIZE                     CRT-ERROR-NBR
J15880         IF  (PRM-DETAIL-RPT-OPT = "N")
J15880             PERFORM 330-WRITE-ERROR-REC
J15880         END-IF
J15880         SET ACCOUNT-ERROR-FOUND     TO TRUE
J15880     END-IF.
J15880
P79800     IF  (ACCOUNT-ERROR-FOUND)
P79800         MOVE WR3-EMPLOYEE           TO WS-WR3-EMPLOYEE
P79800         MOVE WR3-FULL-NAME          TO WS-WR3-FULL-NAME
P79800         MOVE WR3-DED-CODE           TO WS-WR3-DED-CODE
P79800         MOVE WR3-PAY-CODE           TO WS-WR3-PAY-CODE
P79800         MOVE WR3-PCD-SEQ-NBR        TO WS-WR3-PCD-SEQ-NBR
P79800         MOVE WR3-TRAN-AMOUNT        TO WS-WR3-TRAN-AMOUNT
P79800         MOVE WR3-CLEARING           TO WS-WR3-CLEARING
P79800         PERFORM 330-WRITE-ERROR-REC
P79800     END-IF.

240500     IF  (WR3-CLEARING = "Y")
240600     AND (WS-RECORD-COUNT > WS-RS-REC-CNT)
240700         IF (UPDATING)
                   IF  (WS-PRD-OBJ-REMAIN = ZEROES)
                   AND (UPDATING)
                       MOVE WS-RECORD-COUNT    TO WS-RS-REC-CNT
                       PERFORM 840-MODIFY-CKPSET1
                       MOVE WS-RESTART-INFO    TO CKP-RESTART-INFO
                       PERFORM 820-STORE-CKPOINT
J58525                 PERFORM 925-AUDIT-END
J58525                 PERFORM 910-AUDIT-BEGIN
                       MOVE "PRPRD"            TO IFOBIWS-OBJ-TYPE
                       MOVE WS-MAX-OPS-IN-TRAN TO IFOBIWS-NBR-OBJECTS
                                                  WS-PRD-OBJ-REMAIN
                       PERFORM 7000-ASSIGN-OBJ-ID-70
                       COMPUTE WS-LAST-PRD-OBJ-ID  = IFOBIWS-OBJ-ID
                                                   - IFOBIWS-NBR-OBJECTS
                       PERFORM 925-AUDIT-END
                       PERFORM 910-AUDIT-BEGIN
                   END-IF
J65254             IF  (WR3-OBJ-ID = ZEROES)
240800                 PERFORM 800-CREATE-PRDISTRIB
J65254             ELSE
J65254                 MOVE WR3-COMPANY        TO DB-COMPANY
J70358                 INITIALIZE                 DB-EMPLOYEE
J70358                                            DB-CHECK-ID
J65254                 MOVE WR3-OBJ-ID         TO DB-OBJ-ID
J65254                 PERFORM 840-MODIFY-PRDSET1
J65254                 IF  (PRDISTRIB-NOTFOUND)
J65254                     PERFORM 800-CREATE-PRDISTRIB
J65254                 END-IF
J65254             END-IF
               ELSE
                   INITIALIZE PRDISTRIB
               END-IF
241000         MOVE ZEROS                  TO PRD-HOURS
UOM                                           PRD-PAY-UNITS
241100                                        PRD-DIST-AMT
UOM            MOVE SPACES                 TO PRD-UNIT-MEASURE
241200         MOVE WR3-COMPANY            TO PRD-COMPANY
241300         MOVE WR3-PROCESS-LEVEL      TO PRD-PROCESS-LEVEL
241500         MOVE WR3-RECORD-TYPE        TO PRD-RECORD-TYPE
241600         MOVE WR3-TO-COMPANY         TO PRD-DIST-COMPANY
241700         MOVE WR3-ACCT-UNIT          TO PRD-DST-ACCT-UNIT
241800         MOVE WR3-ACCOUNT            TO PRD-DST-ACCOUNT
241900         MOVE WR3-SUB-ACCOUNT        TO PRD-DST-SUB-ACCT
242000         MOVE WR3-GL-DATE            TO PRD-GL-DATE
242200         MOVE WS-RS-RUN-DATE         TO PRD-RUN-DATE
242300         MOVE WS-RS-RUN-TIME         TO PRD-RUN-TIME
J65254         IF  (WR3-OBJ-ID = ZEROES)
J65254         OR  ((WR3-OBJ-ID NOT = ZEROES)
J65254         AND (PRDISTRIB-NOTFOUND))
                   SUBTRACT 1              FROM WS-PRD-OBJ-REMAIN
                   ADD 1                   TO WS-LAST-PRD-OBJ-ID
242500             MOVE WS-LAST-PRD-OBJ-ID TO PRD-OBJ-ID
J65254         END-IF
242700         MOVE WR3-ATN-OBJ-ID         TO PRD-ATN-OBJ-ID
               MOVE WR3-GLT-OBJ-ID         TO PRD-GLT-OBJ-ID
               IF (PRD-RECORD-TYPE = "E")
                   MOVE WR3-GML-OBJ-ID     TO PRD-GML-OBJ-ID
               ELSE
                   INITIALIZE                 PRD-GML-OBJ-ID
               END-IF
               MOVE WR3-CURRENCY-CODE      TO PRD-CURRENCY-CODE
               MOVE WR3-CURR-ND            TO PRD-CURR-ND
               MOVE WR3-CO-CURR-CD         TO PRD-CO-CURR-CD
               MOVE WR3-DST-CO-CURR-CD     TO PRD-DST-CO-CURR-CD
               MOVE WR3-CO-EXCH-RATE       TO PRD-CO-EXCH-RATE
               MOVE WR3-DIST-EXCH-RATE     TO PRD-DIST-EXCH-RATE
               IF (WR3-ERROR-FLAG = "Y")
                   PERFORM VARYING PRD-I4 FROM 1 BY 1
                      UNTIL (PRD-I4 > 10)
                         OR (WR3-MSG-NBR (PRD-I4) = ZEROS)
                             MOVE 1            TO PRD-ERROR-TYPE
                             MOVE WR3-MSG-NBR (PRD-I4)
                                           TO PRD-MSG-NBR (PRD-I4)
                   END-PERFORM
               END-IF
P04079         MOVE WR3-HM-PROC-LEVEL      TO PRD-HM-PROC-LEVEL
               MOVE WR3-ATTEND-CODE        TO PRD-ATTEND-CODE
               MOVE WR3-AC-UPDATED         TO PRD-AC-UPDATED
               MOVE WR3-GARN-OBJ-ID        TO PRD-GARN-OBJ-ID
               MOVE WR3-SEGMENT-SEQ        TO PRD-SEGMENT-SEQ
               MOVE WR3-TR-DATE            TO PRD-TR-DATE
GMDIST         MOVE WR3-PER-END-DATE       TO PRD-PER-END-DATE
GMDIST         MOVE WR3-OT-PLAN-CODE       TO PRD-OT-PLAN-CODE
J85817         MOVE WR3-EXP-RPT-NBR        TO PRD-EXP-RPT-NBR
J85817         MOVE WR3-REC-TYPE           TO PRD-REC-TYPE
P54266         MOVE 1                      TO PRD-STATUS

242800         PERFORM
242900             UNTIL (END-OF-FILE)
243000             OR    (WR3-COMPANY           NOT = WS-COMPANY)
                   OR    (WR3-PROCESS-LEVEL-SRT NOT = WS-PROCESS-LEVEL)
                   OR    (WR3-DEPARTMENT-SRT    NOT = WS-DEPARTMENT)
243100             OR    (WR3-TO-COMPANY        NOT = WS-TO-COMPANY)
243200             OR    (WR3-GL-DATE           NOT = WS-GL-DATE)
243300             OR    (WR3-ACCT-UNIT         NOT = WS-DST-ACCT-UNIT)
243400             OR    (WR3-ACCOUNT           NOT = WS-DST-ACCOUNT)
243500             OR    (WR3-SUB-ACCOUNT       NOT = WS-DST-SUB-ACCT)
P86203             OR    (WR3-CLEARING          NOT = "Y")
243700
243800               ADD WR3-HOURS-AMOUNT  TO WS-DIST-HOURS-TOT
243900                                        WS-SUB-TOT-HOURS
244000                                        PRD-HOURS
UOM                  ADD WR3-UNITS-AMOUNT  TO WS-DIST-UNITS-TOT
UOM                                              WS-SUB-TOT-PAY-UNITS
UOM                                              PRD-PAY-UNITS
UOM                  MOVE WR3-UNIT-MEASURE TO WS-UNIT-MEASURE
244100               ADD WR3-TRAN-AMOUNT   TO WS-DIST-AMT-TOT
244200                                        WS-SUB-TOT-AMT
244300                                        PRD-DIST-AMT
J70358               IF  (UPDATING)
J70358                   MOVE PRD-OBJ-ID   TO WR3-OBJ-ID
J70358                   REWRITE WORK3-REC FROM WR3-WORK3-REC
J70358               END-IF
244400               READ WORK3-FILE     INTO WR3-WORK3-REC
244500                 AT END
244600                     SET END-OF-FILE TO TRUE
244700             END-READ
244800
244900             ADD 1                   TO WS-RECORD-COUNT
245000         END-PERFORM

245100         IF  (UPDATING)
245200             PERFORM 820-STORE-PRDISTRIB
246800         END-IF
246900         GO TO 313-END
J65254     END-IF.
247000
248300     IF  (WR3-PRD-OPTION = "Y")
248400     AND (WS-RECORD-COUNT   > WS-RS-REC-CNT)
248500     AND (WR3-CLEARING       NOT = "Y")
248600     AND (WR3-CHECK-TYPE     NOT = "R" AND "J")
           AND (UPDATING)
                IF (WS-PRD-OBJ-REMAIN = ZEROES)
                    MOVE WS-RECORD-COUNT    TO WS-RS-REC-CNT
                    PERFORM 840-MODIFY-CKPSET1
                    MOVE WS-RESTART-INFO    TO CKP-RESTART-INFO
                    PERFORM 820-STORE-CKPOINT
J58525              PERFORM 925-AUDIT-END
J58525              PERFORM 910-AUDIT-BEGIN
                    MOVE "PRPRD"            TO IFOBIWS-OBJ-TYPE
                    MOVE WS-MAX-OPS-IN-TRAN TO IFOBIWS-NBR-OBJECTS
                                               WS-PRD-OBJ-REMAIN
                    PERFORM 7000-ASSIGN-OBJ-ID-70
                    COMPUTE WS-LAST-PRD-OBJ-ID   = IFOBIWS-OBJ-ID
                                                 - IFOBIWS-NBR-OBJECTS
                    PERFORM 925-AUDIT-END
                    PERFORM 910-AUDIT-BEGIN
                END-IF
J65254          IF  (WR3-OBJ-ID  = ZEROES)
248700              PERFORM 800-CREATE-PRDISTRIB
J65254          ELSE
J65254              MOVE WR3-COMPANY        TO DB-COMPANY
J65254              MOVE WR3-EMPLOYEE       TO DB-EMPLOYEE
J65254              MOVE WR3-CHECK-ID       TO DB-CHECK-ID
J65254              MOVE WR3-OBJ-ID         TO DB-OBJ-ID
J65254              PERFORM 840-MODIFY-PRDSET1
J65254              IF  (PRDISTRIB-NOTFOUND)
J65254                  PERFORM 800-CREATE-PRDISTRIB
J65254              END-IF
J65254          END-IF
248800          MOVE WR3-COMPANY        TO PRD-COMPANY
248900          MOVE WR3-EMPLOYEE       TO PRD-EMPLOYEE
249000          MOVE WR3-CHECK-ID       TO PRD-CHECK-ID
249100          MOVE WR3-PROCESS-LEVEL  TO PRD-PROCESS-LEVEL
249300          MOVE WR3-DEPARTMENT     TO PRD-DEPARTMENT
249400          MOVE WR3-RECORD-TYPE    TO PRD-RECORD-TYPE
249500          MOVE WR3-CHECK-TYPE     TO PRD-CHECK-TYPE
249600          MOVE WR3-TO-COMPANY     TO PRD-DIST-COMPANY
249700          MOVE WR3-ACCT-UNIT      TO PRD-DST-ACCT-UNIT
249800          MOVE WR3-ACCOUNT        TO PRD-DST-ACCOUNT
249900          MOVE WR3-SUB-ACCOUNT    TO PRD-DST-SUB-ACCT
250000          MOVE WR3-PCD-SEQ-NBR    TO PRD-PCD-SEQ-NBR
250100          MOVE WR3-DED-CODE       TO PRD-DED-CODE
UOM             MOVE WR3-HOURS-AMOUNT   TO PRD-HOURS
UOM             MOVE WR3-UNITS-AMOUNT   TO PRD-PAY-UNITS
UOM             MOVE WR3-UNIT-MEASURE   TO PRD-UNIT-MEASURE
250300          MOVE WR3-TRAN-AMOUNT    TO PRD-DIST-AMT
250400          MOVE WR3-ACTIVITY       TO PRD-ACTIVITY
250500          MOVE WR3-ACCT-CATEGORY  TO PRD-ACCT-CATEGORY
250600          MOVE WR3-JOB-CODE       TO PRD-JOB-CODE
250700          MOVE WR3-GL-DATE        TO PRD-GL-DATE
250900          MOVE WS-RS-RUN-DATE     TO PRD-RUN-DATE
251000          MOVE WS-RS-RUN-TIME     TO PRD-RUN-TIME
J65254          IF  (WR3-OBJ-ID = ZEROES)
J65254          OR  ((WR3-OBJ-ID NOT = ZEROES)
J65254          AND (PRDISTRIB-NOTFOUND))
                    SUBTRACT 1          FROM WS-PRD-OBJ-REMAIN
251100              ADD 1               TO WS-LAST-PRD-OBJ-ID
251200              MOVE WS-LAST-PRD-OBJ-ID TO PRD-OBJ-ID
J65254          END-IF
251400          MOVE WR3-ATN-OBJ-ID     TO PRD-ATN-OBJ-ID
                MOVE WR3-GLT-OBJ-ID     TO PRD-GLT-OBJ-ID
                IF (PRD-RECORD-TYPE = "E")
                    MOVE WR3-GML-OBJ-ID TO PRD-GML-OBJ-ID
                ELSE
                    INITIALIZE             PRD-GML-OBJ-ID
                END-IF
251500          MOVE WR3-POSITION       TO PRD-POSITION
                MOVE WR3-CURRENCY-CODE  TO PRD-CURRENCY-CODE
                MOVE WR3-CURR-ND        TO PRD-CURR-ND
                MOVE WR3-CO-CURR-CD     TO PRD-CO-CURR-CD
                MOVE WR3-DST-CO-CURR-CD TO PRD-DST-CO-CURR-CD
                MOVE WR3-CO-EXCH-RATE   TO PRD-CO-EXCH-RATE
                MOVE WR3-DIST-EXCH-RATE TO PRD-DIST-EXCH-RATE
                IF (WR3-ERROR-FLAG = "Y")
                    PERFORM VARYING PRD-I4 FROM 1 BY 1
                       UNTIL (PRD-I4 > 10)
                          OR (WR3-MSG-NBR (PRD-I4) = ZEROS)
                              MOVE 1    TO PRD-ERROR-TYPE
                              MOVE WR3-MSG-NBR (PRD-I4)
                                        TO PRD-MSG-NBR (PRD-I4)
                     END-PERFORM
                END-IF
P04079          MOVE WR3-HM-PROC-LEVEL  TO PRD-HM-PROC-LEVEL
                MOVE WR3-ATTEND-CODE    TO PRD-ATTEND-CODE
                MOVE WR3-AC-UPDATED     TO PRD-AC-UPDATED
                MOVE WR3-GARN-OBJ-ID    TO PRD-GARN-OBJ-ID
                MOVE WR3-SEGMENT-SEQ    TO PRD-SEGMENT-SEQ
                MOVE WR3-TR-DATE        TO PRD-TR-DATE
GMDIST          MOVE WR3-PER-END-DATE   TO PRD-PER-END-DATE
GMDIST          MOVE WR3-OT-PLAN-CODE   TO PRD-OT-PLAN-CODE
J85817          MOVE WR3-EXP-RPT-NBR    TO PRD-EXP-RPT-NBR
J85817          MOVE WR3-REC-TYPE       TO PRD-REC-TYPE
P54266          MOVE 1                  TO PRD-STATUS
251600          PERFORM 820-STORE-PRDISTRIB
J65254          MOVE PRD-OBJ-ID         TO WR3-OBJ-ID
J65254          REWRITE WORK3-REC       FROM WR3-WORK3-REC
J65254     END-IF.
253100
           IF (UPDATING)
               IF (WS-PRD-OBJ-REMAIN = ZEROES)
                   MOVE WS-RECORD-COUNT    TO WS-RS-REC-CNT
                   PERFORM 840-MODIFY-CKPSET1
                   MOVE WS-RESTART-INFO    TO CKP-RESTART-INFO
                   PERFORM 820-STORE-CKPOINT
J58525             PERFORM 925-AUDIT-END
J58525             PERFORM 910-AUDIT-BEGIN
                   MOVE "PRPRD"            TO IFOBIWS-OBJ-TYPE
                   MOVE WS-MAX-OPS-IN-TRAN TO IFOBIWS-NBR-OBJECTS
                                              WS-PRD-OBJ-REMAIN
                   PERFORM 7000-ASSIGN-OBJ-ID-70
                   COMPUTE WS-LAST-PRD-OBJ-ID   = IFOBIWS-OBJ-ID
                                                - IFOBIWS-NBR-OBJECTS
                   PERFORM 925-AUDIT-END
                   PERFORM 910-AUDIT-BEGIN
               END-IF
J58525     END-IF.

UOM        ADD WR3-HOURS-AMOUNT        TO WS-DIST-HOURS-TOT
UOM                                       WS-SUB-TOT-HOURS.
UOM        ADD WR3-UNITS-AMOUNT        TO WS-DIST-UNITS-TOT
UOM                                       WS-SUB-TOT-PAY-UNITS.
UOM        MOVE WR3-UNIT-MEASURE       TO WS-UNIT-MEASURE.
253400     ADD WR3-TRAN-AMOUNT         TO WS-DIST-AMT-TOT
253500                                    WS-SUB-TOT-AMT.
253600
           IF (PRM-DETAIL-RPT-OPT = "Y")
               PERFORM 315-PRINT-GL-DETAIL.

053700     READ WORK3-FILE           INTO WR3-WORK3-REC
253800         AT END
253900             SET END-OF-FILE     TO TRUE.
254000     ADD 1                       TO WS-RECORD-COUNT.
254100
254200 313-END.
254300     EXIT.
254400
      ****************************************************************
       315-PRINT-GL-DETAIL             SECTION.
      ****************************************************************
       315-START.

           MOVE WR3-EMPLOYEE           TO R2EL-EMPLOYEE.

           MOVE WR3-FULL-NAME          TO R2EL-FULL-NAME.

           IF (WR3-CHECK-TYPE          = "T")
               MOVE "S"                TO WR3-CHECK-TYPE
               MOVE "*"                TO R2EL-TEMP-STAR
           ELSE
               MOVE SPACE              TO R2EL-TEMP-STAR.

           MOVE WR3-CHECK-NBR          TO R2EL-CHECK-NBR.

           MOVE WR3-GL-DATE            TO R2EL-GL-DATE.
           MOVE WR3-CHECK-TYPE         TO R2EL-CHECK-TYPE.
           MOVE WR3-PAY-CODE           TO R2EL-PAY-CODE.
           MOVE WR3-DED-CODE           TO R2EL-DED-CODE.
           MOVE WR3-JOB-CODE           TO R2EL-JOB-CODE.
           MOVE WR3-PROCESS-LEVEL      TO R2EL-DIST-PROC-LEV.
           MOVE WR3-DEPARTMENT         TO R2EL-DEPARTMENT.

           IF  (WR3-EMPLOYEE           = ZEROS)
           AND (WR3-RECORD-TYPE        = "E")
               MOVE WS-E-MESSAGE       TO R2EL-FULL-NAME
           ELSE
           IF  (WR3-EMPLOYEE           = ZEROS)
           AND (WR3-RECORD-TYPE        = "A")
               MOVE WS-A-MESSAGE       TO R2EL-FULL-NAME
           ELSE
           IF  (WR3-EMPLOYEE           = ZEROS)
           AND (WR3-RECORD-TYPE        = "C")
               MOVE WS-C-MESSAGE       TO R2EL-FULL-NAME.

UOM        MOVE WR3-UNITS-AMOUNT       TO R2EL-DIST-UNITS.
UOM        MOVE WR3-HOURS-AMOUNT       TO R2EL-DIST-HOURS.
UOM        MOVE WR3-TRAN-AMOUNT        TO R2EL-DIST-AMT.
UOM        MOVE R2-EMPLOYEE-LINE       TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.
           MOVE WS-MESSAGE-150         TO R2PL-CONTINUED.
P89534     IF (WR3-RECORD-TYPE       = "E")
               IF (WR3-ACTIVITY      NOT = SPACES)
               OR (WR3-ACCT-CATEGORY NOT = SPACES)
               OR (WR3-POSITION      NOT = SPACES)
                   PERFORM 316-PRINT-ACTIVITY.

           IF (WR3-SEGMENT-BLOCK NOT = SPACES)
               PERFORM 317-PRINT-SEG-BLOCK
               MOVE WR3-COMPANY          TO DB-COMPANY.

       315-END.
           EXIT.

      ****************************************************************
       316-PRINT-ACTIVITY              SECTION.
      ****************************************************************
       316-START.

P82172     MOVE WR3-ACTIVITY           TO R2AL-ACTIVITY 
P82172                                    IFACWS-ACTIVITY.
           MOVE WR3-ACCT-CATEGORY      TO R2AL-ACCT-CATEGORY 
J26795                                    IFACWS-ACCT-CATEGORY.
           MOVE WR3-POSITION           TO R2AL-POSITION.
           MOVE R2-ACTIVITY-LINE       TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

P82172     IF (WR3-ACTIVITY NOT    = SPACES)
P82172         MOVE 3                      TO IFACWS-EDIT-TYPE
J47059         MOVE WR3-COMPANY        TO WS-TRD-DIST-COMPANY
J47059         MOVE WR3-PCD-SEQ-NBR    TO WS-TRD-PCD-SEQ-NBR
J27195         MOVE WR3-HOURS-AMOUNT       TO WS-TRD-HOURS
J27195         MOVE WR3-UNITS-AMOUNT       TO WS-TRD-PAY-UNITS
J47059         PERFORM 7850-GET-GLT-TYPE
P82172         PERFORM 635-EDIT-GLMASTER-60
P82172         IF (ERROR-FOUND)
P82172             MOVE CRT-ERROR-NBR      TO CRT-MSG-NBR
P82172             PERFORM 790-GET-MSG
P82172             MOVE CRT-MESSAGE        TO R2EML-MESSAGE
P82172             MOVE R2-ERROR-MESSAGE-LINE  TO RPT-GROUP-REQUEST
P82172             PERFORM 700-PRINT-RPT-GRP
P82172             MOVE SPACES             TO IFACWS-ACTIVITY
P82172         END-IF
P82172         MOVE 0                      TO CRT-ERROR-NBR
P82172     END-IF.

       316-END.
           EXIT.

      ****************************************************************
       317-PRINT-SEG-BLOCK             SECTION.
      ****************************************************************
       317-START.

           IF (WR3-TO-COMPANY       NOT = IFSGWS-COMPANY)
               MOVE WR3-TO-COMPANY     TO IFSGWS-COMPANY
               PERFORM 700-RETURN-SEGMENT-GROUP-80
               IF (ERROR-FOUND)
                  INITIALIZE              CRT-ERROR-NBR
                                          IFSGWS-SEGMENT-GROUP.

           IF (IFSGWS-SEGMENT-GROUP   = SPACES)
                GO TO 317-END.

           MOVE WR3-SEGMENT-BLOCK      TO SLSEWS-SEGMENT-BLOCK.
           MOVE WR3-TO-COMPANY         TO SLSEWS-COMPANY.
           MOVE 1                      TO SLSEWS-EDIT-TYPE.
           PERFORM 700-EDIT-SEGMENT-BLOCK-80.
           INITIALIZE CRT-ERROR-NBR.

           MOVE SLSEWS-SEGMENT-VALUE(1) TO R2SL-SEGMENT-VALUE-1.
           MOVE SLSEWS-SEGMENT-VALUE(2) TO R2SL-SEGMENT-VALUE-2.
           MOVE SLSEWS-SEGMENT-VALUE(3) TO R2SL-SEGMENT-VALUE-3.
           MOVE SLSEWS-SEGMENT-VALUE(4) TO R2SL-SEGMENT-VALUE-4.
           MOVE SLSEWS-SEGMENT(1)       TO R2SL-SEGMENT-1.
           MOVE SLSEWS-SEGMENT(2)       TO R2SL-SEGMENT-2.
           MOVE SLSEWS-SEGMENT(3)       TO R2SL-SEGMENT-3.
           MOVE SLSEWS-SEGMENT(4)       TO R2SL-SEGMENT-4.
           MOVE R2-SEG-BLOCK-LINE       TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

       317-END.
           EXIT.

264500****************************************************************
264600 320-PRINT-ACCOUNT               SECTION.
264700****************************************************************
264800 320-START.
264900*
DKR        MOVE "PR"                   TO IFACWS-SYSTEM.
265000     MOVE WS-TO-COMPANY          TO PL-DIST-COMPANY
265100                                    IFACWS-COMPANY.
265200     MOVE WS-DST-ACCT-UNIT       TO PL-ACCT-UNIT
265300                                    IFACWS-ACCT-UNIT.
265400     MOVE WS-DST-ACCOUNT         TO PL-ACCOUNT
265500                                    IFACWS-ACCOUNT.
265600     MOVE WS-DST-SUB-ACCT        TO PL-SUB-ACCT
265700                                    IFACWS-SUB-ACCOUNT.
265800     MOVE WS-GL-DATE             TO PL-POSTING-DATE.
265900*
266000     MOVE 0                      TO IFACWS-EDIT-TYPE.
J47059     MOVE WS-COMPANY             TO WS-TRD-DIST-COMPANY.
J15880*    MOVE WR3-PCD-SEQ-NBR        TO WS-TRD-PCD-SEQ-NBR.
J15880     MOVE WS-DST-PCD-SEQ-NBR     TO WS-TRD-PCD-SEQ-NBR.
J27195     MOVE WS-DST-HOURS-AMOUNT    TO WS-TRD-HOURS.
J27195     MOVE WS-DST-UNITS-AMOUNT    TO WS-TRD-PAY-UNITS.
J47059     PERFORM 7850-GET-GLT-TYPE.
266100     PERFORM 635-EDIT-GLMASTER-60.
           IF (ERROR-FOUND)
               MOVE CRT-ERROR-NBR      TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE        TO PL-DESCRIPTION
               INITIALIZE                 CRT-ERROR-NBR
P79800         IF  (PRM-DETAIL-RPT-OPT = "N")
P79800             PERFORM 330-WRITE-ERROR-REC
P79800         END-IF
           ELSE
               MOVE IFACWS-GLM-DESCRIPTION
                                       TO PL-DESCRIPTION
           END-IF.

266300     MOVE PRM-COMPANY            TO DB-COMPANY.
266400*
266500     MOVE ZEROS                  TO PL-HOURS
266600                                    PL-UNITS
266700                                    PL-DEBIT-AMT
266800                                    PL-CREDIT-AMT.
266900*
267000     IF (WS-DIST-AMT-TOT        > ZEROS)
267100         MOVE WS-DIST-AMT-TOT   TO PL-DEBIT-AMT
267200         ADD  WS-DIST-AMT-TOT   TO WS-TOT-DEBIT-AMT
267300     ELSE
267400         MOVE WS-DIST-AMT-TOT   TO PL-CREDIT-AMT
267500         ADD  WS-DIST-AMT-TOT   TO WS-TOT-CREDIT-AMT.
267600
UOM        MOVE WS-DIST-HOURS-TOT     TO PL-HOURS.
UOM        ADD  WS-DIST-HOURS-TOT     TO WS-TOT-HOURS.
UOM        MOVE WS-DIST-UNITS-TOT     TO PL-UNITS.
UOM        ADD  WS-DIST-UNITS-TOT     TO WS-TOT-UNITS.
268300
268400     IF (WS-DIST-AMT-TOT   NOT = ZEROS)
268500     OR (WS-DIST-HOURS-TOT NOT = ZEROS)
UOM        OR (WS-DIST-UNITS-TOT NOT = ZEROES)
268600         MOVE POSTING-LINE       TO RPT-GROUP-REQUEST
268700         PERFORM 700-PRINT-RPT-GRP
               MOVE WS-MESSAGE-150     TO DH-CONTINUED.
268800*
           IF (PRM-DETAIL-RPT-OPT = "Y")
               MOVE WS-DIST-HOURS-TOT  TO R2TL-DIST-HOURS
               MOVE WS-DIST-UNITS-TOT  TO R2TL-DIST-UNITS
               MOVE WS-DIST-AMT-TOT    TO R2TL-DIST-AMT
               MOVE R2-TOTAL-LINE      TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP.

268900 320-END.
269000     EXIT.

P79800****************************************************************
P79800 330-WRITE-ERROR-REC             SECTION.
P79800****************************************************************
       330-START.

           MOVE PRM-COMPANY                TO GE-COMPANY.
           MOVE CRT-MESSAGE                TO GE-MESSAGE.
           MOVE WS-TO-COMPANY              TO GE-TO-COMPANY.
           MOVE WS-DST-ACCT-UNIT           TO GE-ACCT-UNIT.
           MOVE WS-DST-ACCOUNT             TO GE-ACCOUNT.
           MOVE WS-DST-SUB-ACCT            TO GE-SUB-ACCOUNT.

           IF (PRM-DETAIL-RPT-OPT = "Y")
               MOVE WS-WR3-EMPLOYEE        TO GE-EMPLOYEE
               MOVE WS-WR3-FULL-NAME       TO GE-FULL-NAME
               MOVE WS-WR3-DED-CODE        TO GE-DED-CODE
               MOVE WS-WR3-PAY-CODE        TO GE-PAY-CODE
               MOVE WS-WR3-PCD-SEQ-NBR     TO GE-PCD-SEQ-NBR
               MOVE WS-WR3-CLEARING        TO GE-CLEARING
               MOVE WS-WR3-TRAN-AMOUNT     TO GE-TRAN-AMOUNT.

           WRITE GLERR-WORK-REC          FROM GE-GLERR-WORK-REC.

       330-END.

GMDIST****************************************************************
GMDIST 350-GM-REPORT                   SECTION.
GMDIST****************************************************************
GMDIST 350-START.
GMDIST
GMDIST     OPEN OUTPUT GM-DETAIL-FILE.
GMDIST
GMDIST     SORT GMSORT-FILE
GMDIST         ASCENDING KEY           DSF1-DEDUCTION-TYPE
GMDIST                                 DSF1-COMPANY
GMDIST                                 DSF1-EMPLOYEE
GMDIST                                 DSF1-PROCESS-LEVEL
GMDIST                                 DSF1-DEPARTMENT
GMDIST                                 DSF1-TR-DATE
GMDIST                                 DSF1-JOB-CODE
GMDIST                                 DSF1-POSITION
GMDIST                                 DSF1-PAY-CODE
GMDIST                                 DSF1-DED-CODE
181831*                                DSF1-RECORD-TYPE
                                       DSF1-GM-LINE-NBR
GMDIST                                 DSF1-ERROR-NBR
GMDIST         USING                   GMWORK-FILE
GMDIST         GIVING                  GMWORK-FILE.
GMDIST
GMDIST     SET NO-END-OF-FILE          TO TRUE.
GMDIST
GMDIST     OPEN INPUT GMWORK-FILE.
GMDIST
GMDIST     READ GMWORK-FILE             INTO GM3-GMWORK-REC
GMDIST         AT END
GMDIST             SET END-OF-FILE     TO TRUE
GMDIST             GO TO 350-CLOSE.
GMDIST
           MOVE ZEROS                  TO WS-TOT-GEF-COUNT
                                          WS-GEF-COUNT.

GMDIST     IF (UPDATING)
               IF (NOT RESTARTING-PHASE-4)
GMDIST             MOVE "GMGEF"            TO IFOBIWS-OBJ-TYPE
GMDIST             MOVE WS-MAX-OPS-IN-TRAN TO IFOBIWS-NBR-OBJECTS
GMDIST             PERFORM 7000-ASSIGN-OBJ-ID-70
GMDIST             COMPUTE WS-LAST-GEF-OBJ-ID  = IFOBIWS-OBJ-ID
GMDIST                                         - IFOBIWS-NBR-OBJECTS
GMDIST             PERFORM 910-AUDIT-BEGIN
GMDIST             PERFORM 840-MODIFY-CKPSET1
                   MOVE WS-LAST-GEF-OBJ-ID TO WS-RS-LAST-GEF-OBJ-ID
GMDIST             MOVE WS-RESTART-INFO    TO CKP-RESTART-INFO
GMDIST             PERFORM 820-STORE-CKPOINT
GMDIST             PERFORM 925-AUDIT-END
                ELSE
                   MOVE ZEROS  TO SUB9
                   PERFORM
GMDIST               UNTIL (END-OF-FILE)
                     OR    (SUB9 > WS-RS-TOT-GEF-COUNT)
GMDIST                  READ GMWORK-FILE       INTO GM3-GMWORK-REC
GMDIST                     AT END
GMDIST                        SET END-OF-FILE  TO TRUE
                        ADD 1                  TO SUB9
                   END-PERFORM
                   MOVE WS-RS-LAST-GEF-OBJ-ID  TO WS-LAST-GEF-OBJ-ID
                   MOVE WS-RS-TOT-GEF-COUNT    TO WS-TOT-GEF-COUNT
                END-IF
GMDIST          PERFORM 910-AUDIT-BEGIN.
GMDIST
GMDIST     MOVE WS-RS-RUN-DATE         TO R3PH-RUN-DATE.
GMDIST     MOVE WS-RS-RUN-TIME         TO R3PH-RUN-TIME.
GMDIST     MOVE PRM-COMPANY            TO R3PH-COMPANY.
GMDIST     MOVE WS-CO-NAME             TO R3PH-NAME.
GMDIST     MOVE R3-PAGE-HEADING        TO RPT-GROUP-REQUEST.
GMDIST     PERFORM 700-PRINT-RPT-GRP.
GMDIST
J54027     INITIALIZE WS-GM3-VARIABLES.
J54027
GMDIST     PERFORM 360-PROCESS-EMPLOYEE
GMDIST         UNTIL (END-OF-FILE)
GMDIST         OR    (GM3-DEDUCTION-TYPE NOT = 1).
GMDIST
GMDIST     CLOSE GMWORK-FILE.
GMDIST
GMDIST     IF (UPDATING)
GMDIST         PERFORM 925-AUDIT-END.
GMDIST
GMDIST*** PROCESS EMPLOYEE PAID DEDUCTION REPORT ***
GMDIST*
GMDIST     SORT GMSORT-FILE
GMDIST         ASCENDING KEY           DSF1-DEDUCTION-TYPE
GMDIST                                 DSF1-COMPANY
GMDIST                                 DSF1-EMPLOYEE
GMDIST                                 DSF1-PROCESS-LEVEL
GMDIST                                 DSF1-DEPARTMENT
GMDIST                                 DSF1-DED-CODE
GMDIST                                 DSF1-TR-DATE
GMDIST         USING                   GMWORK-FILE
GMDIST         GIVING                  GMWORK-FILE.
GMDIST
GMDIST     SET NO-END-OF-FILE          TO TRUE.
GMDIST
GMDIST     OPEN INPUT GMWORK-FILE.
GMDIST
GMDIST     READ GMWORK-FILE             INTO GM3-GMWORK-REC
GMDIST         AT END
GMDIST             SET END-OF-FILE         TO TRUE
GMDIST             GO TO 350-CLOSE.
GMDIST
GMDIST     PERFORM
GMDIST         UNTIL (END-OF-FILE)
GMDIST         OR    (GM3-DEDUCTION-TYPE = 9)
GMDIST             READ GMWORK-FILE     INTO GM3-GMWORK-REC
GMDIST                 AT END
GMDIST                 SET END-OF-FILE     TO TRUE
GMDIST                 GO TO 350-CLOSE
GMDIST     END-PERFORM.
GMDIST
GMDIST     MOVE WS-RS-RUN-DATE         TO R3PH2-RUN-DATE.
GMDIST     MOVE WS-RS-RUN-TIME         TO R3PH2-RUN-TIME.
GMDIST     MOVE PRM-COMPANY            TO R3PH2-COMPANY.
GMDIST     MOVE WS-CO-NAME             TO R3PH2-NAME.
GMDIST     MOVE R3-DEDUCTION-HEADING   TO RPT-GROUP-REQUEST.
GMDIST     PERFORM 700-PRINT-RPT-GRP.
GMDIST
J54027     INITIALIZE WS-GM3-VARIABLES.
J54027
GMDIST     PERFORM 390-PROCESS-EMPLOYEE
GMDIST         UNTIL (END-OF-FILE)
GMDIST         OR    (GM3-DEDUCTION-TYPE NOT = 9).
GMDIST
GMDIST     CLOSE GMWORK-FILE.
GMDIST
GMDIST***
GMDIST
GMDIST 350-CLOSE.
GMDIST
GMDIST     CLOSE GM-DETAIL-FILE.
GMDIST
GMDIST     IF (PRM-PRINT-LAB-DIST-RPT = "N")
GMDIST         MOVE GM-DETAIL-NAME     TO WS-TMP-FILE
GMDIST         PERFORM 901-REMOVE-TMP-FILE.
GMDIST
GMDIST 350-END.
GMDIST     EXIT.
GMDIST****************************************************************
GMDIST 360-PROCESS-EMPLOYEE            SECTION.
GMDIST****************************************************************
GMDIST 360-START.
GMDIST
GMDIST     MOVE GM3-LAB-DIST-FLAG      TO WS-SV-LAB-DIST-FLAG.
GMDIST
GMDIST     IF  (WS-SV-LAB-DIST-FLAG     = "Y")
GMDIST         MOVE GM3-COMPANY            TO R3H1-COMPANY
GMDIST         MOVE GM3-EMPLOYEE           TO R3H1-EMPLOYEE
GMDIST         MOVE GM3-FULL-NAME          TO R3H1-FULL-NAME
GMDIST         MOVE GM3-PROCESS-LEVEL      TO R3H1-PROCESS-LEVEL
GMDIST         MOVE GM3-DEPARTMENT         TO R3H1-DEPARTMENT
GMDIST         MOVE R3-DTL-HEADING-1       TO RPT-GROUP-REQUEST
GMDIST         PERFORM 700-PRINT-RPT-GRP.
GMDIST
           IF (PRBRD-BROADCAST-ENABLED)
               IF  (GM3-COMPANY    NOT = WS-SV-COMPANY)
               AND (GM3-DEDUCTION-TYPE = 1)
                   MOVE 500                 TO PRBRD-ELEMENT-TYPE
                   MOVE GM3-COMPANY         TO PRBRD-ELEMENT-VALUE
                   PERFORM 610-BEG-ELEMENT-TAG
                   MOVE WFCHWS-TAG-STRING   TO BTL1-TAG-LINE
                   MOVE BROADCAST-TAG-LINE-1
                                            TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
               END-IF
               IF  (GM3-PROCESS-LEVEL NOT = WS-SV-PROCESS-LEVEL)
               AND (GM3-DEDUCTION-TYPE    = 1)
                   MOVE 501                 TO PRBRD-ELEMENT-TYPE
                   MOVE GM3-PROCESS-LEVEL   TO PRBRD-ELEMENT-VALUE
                   PERFORM 610-BEG-ELEMENT-TAG
                   MOVE WFCHWS-TAG-STRING   TO BTL1-TAG-LINE
                   MOVE BROADCAST-TAG-LINE-1
                                            TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
               END-IF
               IF  (GM3-DEPARTMENT NOT = WS-SV-DEPARTMENT)
               AND (GM3-DEDUCTION-TYPE = 1)
                   MOVE 502                 TO PRBRD-ELEMENT-TYPE
                   MOVE GM3-DEPARTMENT      TO PRBRD-ELEMENT-VALUE
                   PERFORM 610-BEG-ELEMENT-TAG
                   MOVE WFCHWS-TAG-STRING   TO BTL1-TAG-LINE
                   MOVE BROADCAST-TAG-LINE-1
                                            TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
               END-IF
           END-IF.

GMDIST     MOVE GM3-COMPANY            TO WS-SV-COMPANY.
GMDIST     MOVE GM3-EMPLOYEE           TO WS-SV-EMPLOYEE.
GMDIST     MOVE GM3-PROCESS-LEVEL      TO WS-SV-PROCESS-LEVEL.
GMDIST     MOVE GM3-DEPARTMENT         TO WS-SV-DEPARTMENT.
GMDIST     MOVE GM3-TR-DATE            TO WS-SV-TR-DATE.
P86464     MOVE GM3-WAGE-AMOUNT        TO WS-SV-WAGE-AMOUNT.
P86464     INITIALIZE                     WS-SV-EFFORT-PCT.
GMDIST
J54027     INITIALIZE WS-GM3-VARIABLES.
J54027
J54027     PERFORM 376-MOVE-GM3-TO-WSGM3.
J54027
J54027     MOVE GM3-SALARY-PCT         TO WS-GM3-SALARY-PCT.
J54027     MOVE GM3-EFFORT-PCT         TO WS-GM3-EFFORT-PCT.
J54027     MOVE GM3-HOURS-AMOUNT       TO WS-GM3-HOURS-AMOUNT.
J54027     MOVE GM3-WAGE-AMOUNT        TO WS-GM3-WAGE-AMOUNT.
J54027
J54027     INITIALIZE WS-TOT-ACT-DIST-AMT.
J54027
GMDIST     PERFORM 365-PROCESS-PAY-CODE
GMDIST         UNTIL (END-OF-FILE)
GMDIST         OR    (GM3-DEDUCTION-TYPE  NOT = 1)
GMDIST         OR    (GM3-COMPANY         NOT = WS-SV-COMPANY)
GMDIST         OR    (GM3-PROCESS-LEVEL   NOT = WS-SV-PROCESS-LEVEL)
GMDIST         OR    (GM3-DEPARTMENT      NOT = WS-SV-DEPARTMENT)
GMDIST         OR    (GM3-EMPLOYEE        NOT = WS-SV-EMPLOYEE)
GMDIST         OR    (GM3-TR-DATE         NOT = WS-SV-TR-DATE).
GMDIST
           IF (PRBRD-BROADCAST-ENABLED)
               IF (GM3-COMPANY         NOT = WS-SV-COMPANY)
               OR ((GM3-COMPANY            = WS-SV-COMPANY)
               AND (GM3-DEDUCTION-TYPE NOT = 1))
                   MOVE 500                 TO PRBRD-ELEMENT-TYPE
                   MOVE WS-SV-COMPANY       TO PRBRD-ELEMENT-VALUE
                   PERFORM 615-END-ELEMENT-TAG
                   MOVE WFCHWS-TAG-STRING   TO BTL1-TAG-LINE
                   MOVE BROADCAST-TAG-LINE-1
                                            TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
               END-IF
               IF (GM3-PROCESS-LEVEL   NOT = WS-SV-PROCESS-LEVEL)
               OR ((GM3-PROCESS-LEVEL      = WS-SV-PROCESS-LEVEL)
               AND (GM3-DEDUCTION-TYPE NOT = 1))
                   MOVE 501                 TO PRBRD-ELEMENT-TYPE
                   MOVE WS-SV-PROCESS-LEVEL TO PRBRD-ELEMENT-VALUE
                   PERFORM 615-END-ELEMENT-TAG
                   MOVE WFCHWS-TAG-STRING   TO BTL1-TAG-LINE
                   MOVE BROADCAST-TAG-LINE-1
                                            TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
               END-IF
               IF (GM3-DEPARTMENT      NOT = WS-SV-DEPARTMENT)
               OR ((GM3-DEPARTMENT         = WS-SV-DEPARTMENT)
               AND (GM3-DEDUCTION-TYPE NOT = 1))
                   MOVE 502                 TO PRBRD-ELEMENT-TYPE
                   MOVE WS-SV-DEPARTMENT    TO PRBRD-ELEMENT-VALUE
                   PERFORM 615-END-ELEMENT-TAG
                   MOVE WFCHWS-TAG-STRING   TO BTL1-TAG-LINE
                   MOVE BROADCAST-TAG-LINE-1
                                            TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
               END-IF
           END-IF.

GMDIST 360-END.
GMDIST     EXIT.
GMDIST****************************************************************
GMDIST 365-PROCESS-PAY-CODE             SECTION.
GMDIST****************************************************************
GMDIST 365-START.
GMDIST
GMDIST     INITIALIZE WS-TOT-PAY-DIST-AMT.
GMDIST
GMDIST     IF  (WS-SV-LAB-DIST-FLAG     = "Y")
GMDIST         MOVE GM3-TR-DATE            TO R3H2-TR-DATE
GMDIST         MOVE GM3-TR-PAY-CODE        TO R3H2-TR-PAY-CODE
GMDIST         MOVE GM3-TR-JOB-CODE        TO R3H2-TR-JOB-CODE
GMDIST         MOVE GM3-TR-POSITION        TO R3H2-TR-POSITION
GMDIST         MOVE GM3-PER-END-DATE       TO R3H2-PER-END-DATE
GMDIST         MOVE GM3-EFFECT-DATE        TO R3H2-EFFECT-DATE
GMDIST         MOVE GM3-GM-PAY-CODE        TO R3H2-GM-PAY-CODE
GMDIST         MOVE GM3-GM-JOB-CODE        TO R3H2-GM-JOB-CODE
GMDIST         MOVE GM3-GM-POSITION        TO R3H2-GM-POSITION
GMDIST
GMDIST         MOVE R3-DTL-HEADING-2       TO RPT-GROUP-REQUEST
GMDIST         PERFORM 700-PRINT-RPT-GRP.
GMDIST
GMDIST     MOVE GM3-PAY-CODE           TO WS-SV-PAY-CODE.
GMDIST     MOVE GM3-JOB-CODE           TO WS-SV-JOB-CODE.
GMDIST     MOVE GM3-POSITION           TO WS-SV-POSITION.
GMDIST
J54027     MOVE GM3-GM-LINE-NBR        TO WS-GM3-GM-LINE-NBR.
J54027     MOVE GM3-ACTIVITY           TO WS-GM3-ACTIVITY.
J54027     MOVE GM3-ACCT-CATEGORY      TO WS-GM3-ACCT-CATEGORY.
J54027     MOVE GM3-TO-COMPANY         TO WS-GM3-TO-COMPANY.
J54027     MOVE GM3-ACCT-UNIT          TO WS-GM3-ACCT-UNIT.
J54027     MOVE GM3-ACCOUNT            TO WS-GM3-ACCOUNT. 
J54027     MOVE GM3-SUB-ACCOUNT        TO WS-GM3-SUB-ACCOUNT.
J54027
J54027     MOVE GM3-SALARY-PCT         TO WS-GM3-SALARY-PCT.
J54027     MOVE GM3-EFFORT-PCT         TO WS-GM3-EFFORT-PCT.
J54027     MOVE GM3-HOURS-AMOUNT       TO WS-GM3-HOURS-AMOUNT.
J54027     MOVE GM3-WAGE-AMOUNT        TO WS-GM3-WAGE-AMOUNT.
J54027
GMDIST     PERFORM 370-PROCESS-DETAIL
GMDIST         UNTIL (END-OF-FILE)
GMDIST         OR    (GM3-DEDUCTION-TYPE NOT = 1)
GMDIST         OR    (GM3-COMPANY         NOT = WS-SV-COMPANY)
GMDIST         OR    (GM3-PROCESS-LEVEL   NOT = WS-SV-PROCESS-LEVEL)
GMDIST         OR    (GM3-DEPARTMENT      NOT = WS-SV-DEPARTMENT)
GMDIST         OR    (GM3-EMPLOYEE        NOT = WS-SV-EMPLOYEE)
GMDIST         OR    (GM3-TR-DATE         NOT = WS-SV-TR-DATE)
GMDIST         OR    (GM3-JOB-CODE        NOT = WS-SV-JOB-CODE)
GMDIST         OR    (GM3-POSITION        NOT = WS-SV-POSITION)
GMDIST         OR    (GM3-PAY-CODE        NOT = WS-SV-PAY-CODE).
GMDIST
J54027     PERFORM 372-PRINT-GMWORK.    
J54027
J54027     IF (UPDATING)
J54027         IF ((WS-GM3-EFFORT-FLAG     = "Y")
J54027         AND (WS-GM3-ERROR-NBR       = ZEROS))
J54027             PERFORM 380-UPDATE-GM-EFFORT
J54027         END-IF 
J54027     END-IF.
J54027
GMDIST     IF  (WS-SV-LAB-DIST-FLAG     = "Y")
GMDIST         MOVE 161                         TO CRT-MSG-NBR
GMDIST         MOVE WS-SV-PAY-CODE              TO CRT-ERR-VAR1
GMDIST         PERFORM 790-GET-MSG
GMDIST         MOVE CRT-MESSAGE                 TO JSTWS-STR
GMDIST         MOVE 50                          TO JSTWS-STR-LEN
GMDIST         SET JSTWS-RIGHT                  TO TRUE
GMDIST         SET JSTWS-NO-REMOVE-0            TO TRUE
GMDIST         PERFORM 2000-JUSTIFY-OBJECT
GMDIST         MOVE JSTWS-STR-OUT               TO R3T1-TOTAL-LABEL
GMDIST         MOVE WS-TOT-PAY-DIST-AMT    TO R3T1-TOT-DED-DIST-AMT
GMDIST         MOVE R3-TOTAL               TO RPT-GROUP-REQUEST
GMDIST         PERFORM 700-PRINT-RPT-GRP.
GMDIST
J54027     INITIALIZE WS-GM3-VARIABLES.
J54027
J54027     PERFORM 376-MOVE-GM3-TO-WSGM3.
J54027
J54027     INITIALIZE WS-TOT-ACT-DIST-AMT.
J54027
GMDIST 365-END.
GMDIST     EXIT.
GMDIST****************************************************************
GMDIST 370-PROCESS-DETAIL             SECTION.
GMDIST****************************************************************
GMDIST 370-START.
GMDIST
J54027*    IF  (WS-SV-LAB-DIST-FLAG     = "Y")
J54027*        IF (GM3-ERROR-NBR = ZEROS)
J54027*            PERFORM 372-PRINT-GMWORK
J54027*        ELSE
J54027*            PERFORM 374-PRINT-GM-ERROR
J54027*        END-IF
J54027*    END-IF.
GMDIST
GMDIST     ADD GM3-DIST-AMT             TO WS-TOT-PAY-DIST-AMT.
GMDIST
J53283*    IF  (GM3-EFFORT-PCT NOT = ZEROS)
J53283*        COMPUTE WS-SV-EFFORT-AMT ROUNDED = GM3-WAGE-AMOUNT 
J53283*                                  * (GM3-EFFORT-PCT / 100)
J53283*        SUBTRACT WS-SV-EFFORT-AMT FROM WS-SV-WAGE-AMOUNT
J53283*        ADD GM3-EFFORT-PCT        TO WS-SV-EFFORT-PCT
J53283*        
J53283*        IF (WS-SV-EFFORT-PCT >= 100)
J53283*           ADD WS-SV-WAGE-AMOUNT  TO WS-SV-EFFORT-AMT
J53283*           MOVE ZEROS             TO WS-SV-WAGE-AMOUNT
J53283*        END-IF
J53283*    END-IF.

J54027*    IF  (UPDATING)
J54027*        IF  (GM3-EFFORT-FLAG     = "Y")
J54027*        AND (GM3-ERROR-NBR       = ZEROS)
J54027*            PERFORM 380-UPDATE-GM-EFFORT.
GMDIST
J54027     IF  (WS-SV-LAB-DIST-FLAG     = "Y")
J54027         IF (GM3-ERROR-NBR = ZEROS)
J54027             IF ((GM3-GM-LINE-NBR   NOT = WS-GM3-GM-LINE-NBR)
J54027             OR  (GM3-ACTIVITY      NOT = WS-GM3-ACTIVITY)
J54027             OR  (GM3-ACCT-CATEGORY NOT = WS-GM3-ACCT-CATEGORY)
J54027             OR  (GM3-TO-COMPANY    NOT = WS-GM3-TO-COMPANY)
J54027             OR  (GM3-ACCT-UNIT     NOT = WS-GM3-ACCT-UNIT)
J54027             OR  (GM3-ACCOUNT       NOT = WS-GM3-ACCOUNT)
J54027             OR  (GM3-SUB-ACCOUNT   NOT = WS-GM3-SUB-ACCOUNT)
J54027             OR  (END-OF-FILE)) 
J54027                 PERFORM 372-PRINT-GMWORK
J54027
J54027                 IF (UPDATING)
J54027                     IF ((WS-GM3-EFFORT-FLAG     = "Y")
J54027                     AND (WS-GM3-ERROR-NBR       = ZEROS))
J54027                         PERFORM 380-UPDATE-GM-EFFORT
J54027                     END-IF
J54027                 END-IF
J54027
J54027                 INITIALIZE WS-TOT-ACT-DIST-AMT
J54027
J54027                 PERFORM 376-MOVE-GM3-TO-WSGM3
J54027
J54027                 MOVE GM3-SALARY-PCT    TO WS-GM3-SALARY-PCT
J54027                 MOVE GM3-EFFORT-PCT    TO WS-GM3-EFFORT-PCT
J54027                 MOVE GM3-HOURS-AMOUNT  TO WS-GM3-HOURS-AMOUNT
J54027                 MOVE GM3-WAGE-AMOUNT   TO WS-GM3-WAGE-AMOUNT
J54027             ELSE
J54027                 PERFORM 376-MOVE-GM3-TO-WSGM3
J54027                 GO TO 370-NEXT-GMWORK
J54027             END-IF
J54027         ELSE
J54027             PERFORM 374-PRINT-GM-ERROR
J54027         END-IF
J54027     END-IF.
J54027
J54027     IF  (WS-GM3-EFFORT-PCT NOT = ZEROS)
J54027         COMPUTE WS-SV-EFFORT-AMT ROUNDED = WS-GM3-WAGE-AMOUNT
J54027                                   * (WS-GM3-EFFORT-PCT / 100)
J54027         SUBTRACT WS-SV-EFFORT-AMT FROM WS-SV-WAGE-AMOUNT
J54027         ADD WS-GM3-EFFORT-PCT     TO WS-SV-EFFORT-PCT
J54027 
J54027         IF (WS-SV-EFFORT-PCT >= 100)
J54027            ADD WS-SV-WAGE-AMOUNT  TO WS-SV-EFFORT-AMT
J54027            MOVE ZEROS             TO WS-SV-WAGE-AMOUNT
J54027         END-IF
J54027     END-IF.
J54027
GMDIST 370-NEXT-GMWORK.
GMDIST
GMDIST     READ GMWORK-FILE           INTO GM3-GMWORK-REC
GMDIST         AT END
GMDIST             SET END-OF-FILE     TO TRUE.
GMDIST
GMDIST 370-END.
GMDIST     EXIT.
GMDIST****************************************************************
GMDIST 372-PRINT-GMWORK               SECTION.
GMDIST****************************************************************
GMDIST 372-START.
GMDIST
GMDIST     MOVE "PR"                   TO IFACWS-SYSTEM.
J54027     MOVE WS-GM3-TO-COMPANY      TO IFACWS-COMPANY.
J54027     MOVE WS-GM3-ACCT-UNIT       TO IFACWS-ACCT-UNIT.
J54027     MOVE WS-GM3-ACCOUNT         TO IFACWS-ACCOUNT.
J54027     MOVE WS-GM3-SUB-ACCOUNT     TO IFACWS-SUB-ACCOUNT.
GMDIST     MOVE 0                      TO IFACWS-EDIT-TYPE.
J54027     MOVE WS-GM3-COMPANY         TO WS-TRD-DIST-COMPANY.
J54027     MOVE WS-GM3-TR-PCD-SEQ-NBR  TO WS-TRD-PCD-SEQ-NBR.
J47059     PERFORM 7850-GET-GLT-TYPE.
GMDIST     PERFORM 635-EDIT-GLMASTER-60.
GMDIST     IF (ERROR-FOUND)
GMDIST         INITIALIZE             CRT-ERROR-NBR
GMDIST                                CRT-ERROR-CAT
GMDIST                                R3D1-GLM-DESCRIPTION
GMDIST     ELSE
GMDIST         MOVE IFACWS-GLM-DESCRIPTION TO R3D1-GLM-DESCRIPTION.
GMDIST
J54027     MOVE WS-GM3-ACCT-CATEGORY   TO ACACWS-ACCT-CATEGORY.
J54027     MOVE WS-GM3-ACTIVITY        TO ACACWS-ACTIVITY.
GMDIST     MOVE 2                      TO ACACWS-EDIT-CODE.
GMDIST     PERFORM 640-EDIT-ACTIVITY-70.
GMDIST     IF (ERROR-FOUND)
GMDIST         INITIALIZE CRT-ERROR-NBR
GMDIST                    CRT-ERROR-CAT
GMDIST                    R3D1-ACV-DESCRIPTION
GMDIST     ELSE
GMDIST         MOVE ACACWS-ACV-DESCRIPTION TO R3D1-ACV-DESCRIPTION.
GMDIST
J54027     MOVE WS-GM3-GM-LINE-NBR      TO R3D1-GM-LINE-NBR.
J54027     MOVE WS-GM3-ACTIVITY         TO R3D1-ACTIVITY.
J54027     MOVE WS-GM3-ACCT-CATEGORY    TO R3D1-ACCT-CATEGORY.
J54027     MOVE WS-GM3-TO-COMPANY       TO R3D1-TO-COMPANY.
J54027     MOVE WS-GM3-ACCT-UNIT        TO R3D1-ACCT-UNIT.
J54027     MOVE WS-GM3-ACCOUNT          TO R3D1-ACCOUNT.
J54027     MOVE WS-GM3-SUB-ACCOUNT      TO R3D1-SUB-ACCOUNT.
J54027*    MOVE WS-GM3-DIST-AMT         TO R3D1-DIST-AMT.
J54027     MOVE WS-TOT-ACT-DIST-AMT     TO R3D1-DIST-AMT.
J54027     MOVE WS-GM3-SALARY-PCT       TO R3D1-SALARY-PCT.
J54027     MOVE WS-GM3-EFFORT-PCT       TO R3D1-EFFORT-PCT.
GMDIST     MOVE R3-DETAIL               TO RPT-GROUP-REQUEST.
GMDIST     PERFORM 700-PRINT-RPT-GRP.
GMDIST
GMDIST 372-END.
GMDIST     EXIT.
GMDIST****************************************************************
GMDIST 374-PRINT-GM-ERROR         SECTION.
GMDIST****************************************************************
000800 374-START.
GMDIST
GMDIST     MOVE GM3-ERR-MESSAGE        TO R3D2-MESSAGE.
GMDIST     MOVE R3-ERROR               TO RPT-GROUP-REQUEST.
GMDIST     PERFORM 700-PRINT-RPT-GRP.
GMDIST
GMDIST 374-END.
GMDIST     EXIT.
J54027****************************************************************
J54027 376-MOVE-GM3-TO-WSGM3       SECTION.
J54027****************************************************************
J54027 376-START.
J54027
J54027     MOVE GM3-DEDUCTION-TYPE     TO WS-GM3-DEDUCTION-TYPE.
J54027     MOVE GM3-EFFORT-FLAG        TO WS-GM3-EFFORT-FLAG.   
J54027     MOVE GM3-LAB-DIST-FLAG      TO WS-GM3-LAB-DIST-FLAG. 
J54027     MOVE GM3-COMPANY            TO WS-GM3-COMPANY.
J54027     MOVE GM3-EMPLOYEE           TO WS-GM3-EMPLOYEE.
J54027     MOVE GM3-FULL-NAME          TO WS-GM3-FULL-NAME.
J54027     MOVE GM3-PROCESS-LEVEL      TO WS-GM3-PROCESS-LEVEL.
J54027     MOVE GM3-DEPARTMENT         TO WS-GM3-DEPARTMENT.
J54027     MOVE GM3-JOB-CODE           TO WS-GM3-JOB-CODE.
J54027     MOVE GM3-POSITION           TO WS-GM3-POSITION.
J54027     MOVE GM3-PAY-CODE           TO WS-GM3-PAY-CODE.
J54027     MOVE GM3-RECORD-TYPE        TO WS-GM3-RECORD-TYPE.
J54027     MOVE GM3-EFFECT-DATE        TO WS-GM3-EFFECT-DATE.
J54027     MOVE GM3-TO-COMPANY         TO WS-GM3-TO-COMPANY.
J54027     MOVE GM3-ACCT-UNIT          TO WS-GM3-ACCT-UNIT.
J54027     MOVE GM3-ACCOUNT            TO WS-GM3-ACCOUNT.
J54027     MOVE GM3-SUB-ACCOUNT        TO WS-GM3-SUB-ACCOUNT.
J54027     MOVE GM3-ACTIVITY           TO WS-GM3-ACTIVITY.
J54027     MOVE GM3-ACCT-CATEGORY      TO WS-GM3-ACCT-CATEGORY.
J54027     MOVE GM3-PER-END-DATE       TO WS-GM3-PER-END-DATE. 
J54027     MOVE GM3-TR-DATE            TO WS-GM3-TR-DATE.        
J54027     MOVE GM3-DIST-AMT           TO WS-GM3-DIST-AMT.
J54027     MOVE GM3-GM-JOB-CODE        TO WS-GM3-GM-JOB-CODE.
J54027     MOVE GM3-GM-POSITION        TO WS-GM3-GM-POSITION.
J54027     MOVE GM3-GM-PAY-CODE        TO WS-GM3-GM-PAY-CODE.
J54027     MOVE GM3-TR-JOB-CODE        TO WS-GM3-TR-JOB-CODE.
J54027     MOVE GM3-TR-POSITION        TO WS-GM3-TR-POSITION.
J54027     MOVE GM3-TR-PAY-CODE        TO WS-GM3-TR-PAY-CODE.
J54027     MOVE GM3-TR-PCD-SEQ-NBR     TO WS-GM3-TR-PCD-SEQ-NBR.
J54027     MOVE GM3-CHECK-ID           TO WS-GM3-CHECK-ID.     
J54027     MOVE GM3-CURRENCY-CODE      TO WS-GM3-CURRENCY-CODE.
J54027     MOVE GM3-DED-CODE           TO WS-GM3-DED-CODE.    
J54027     MOVE GM3-ERROR-NBR          TO WS-GM3-ERROR-NBR. 
J54027     MOVE GM3-ERR-MESSAGE        TO WS-GM3-ERR-MESSAGE.
J54027     MOVE GM3-GML-OBJ-ID         TO WS-GM3-GML-OBJ-ID. 
J54027     MOVE GM3-OT-PLAN-CODE       TO WS-GM3-OT-PLAN-CODE.
J54027     MOVE GM3-GM-LINE-NBR        TO WS-GM3-GM-LINE-NBR.
J54027     MOVE GM3-UPDATE-EFFORT      TO WS-GM3-UPDATE-EFFORT.
J54027     MOVE GM3-EFFORT-AMT         TO WS-GM3-EFFORT-AMT.
J54027     ADD GM3-DIST-AMT            TO WS-TOT-ACT-DIST-AMT
J54027                                    WS-TOT-TRDED-DIST-AMT. 
J54027
J54027 376-END.
J54027     EXIT.
GMDIST****************************************************************
GMDIST 380-UPDATE-GM-EFFORT           SECTION.
GMDIST****************************************************************
GMDIST 380-START.
GMDIST
J54027     IF  (WS-TOT-ACT-DIST-AMT       = ZEROES)
J54027     AND (WS-GM3-HOURS-AMOUNT       = ZEROES)
J54027     AND (WS-GM3-WAGE-AMOUNT        = ZEROES)
181831          GO TO 380-END.
181831
J54027     IF (WS-GM3-EFFORT-PCT  NOT = ZEROS)
J54027         COMPUTE WS-GEF-EFFORT-AMT  ROUNDED = WS-GM3-WAGE-AMOUNT
J54027                                   * (WS-GM3-EFFORT-PCT / 100)
P84284     ELSE
P84284         MOVE ZEROES             TO WS-GEF-EFFORT-AMT
P84284     END-IF.
P84284
J54027     IF  (WS-TOT-ACT-DIST-AMT       = ZEROES)
J54027     AND (WS-GM3-HOURS-AMOUNT       = ZEROES)
P84284     AND (WS-GEF-EFFORT-AMT         = ZEROES)
P84284          GO TO 380-END.

GMDIST     ADD 1                        TO WS-LAST-GEF-OBJ-ID.
GMDIST
GMDIST     MOVE WS-LAST-GEF-OBJ-ID      TO GMGEFWS-OBJ-ID.
J54027     MOVE WS-GM3-TR-DATE          TO GMGEFWS-TRAN-DATE.
J54027     MOVE WS-GM3-COMPANY          TO GMGEFWS-HR-COMPANY.
J54027     MOVE WS-GM3-EMPLOYEE         TO GMGEFWS-EMPLOYEE.
J54027     MOVE WS-GM3-OT-PLAN-CODE     TO GMGEFWS-OT-PLAN-CODE.
J54027     MOVE WS-GM3-ACTIVITY         TO GMGEFWS-ACTIVITY.
J54027     MOVE WS-GM3-ACCT-CATEGORY    TO GMGEFWS-ACCT-CATEGORY.
J54027     MOVE WS-GM3-TO-COMPANY       TO GMGEFWS-COMPANY.
J54027     MOVE WS-GM3-ACCT-UNIT        TO GMGEFWS-ACCT-UNIT.
J54027     MOVE WS-GM3-ACCOUNT          TO GMGEFWS-ACCOUNT.
J54027     MOVE WS-GM3-SUB-ACCOUNT      TO GMGEFWS-SUB-ACCT.
GMDIST
J54027*    MOVE WS-GM3-DIST-AMT         TO GMGEFWS-SALARY-AMT.
J54027     MOVE WS-TOT-ACT-DIST-AMT     TO GMGEFWS-SALARY-AMT.
J54027     MOVE WS-GM3-WAGE-AMOUNT      TO GMGEFWS-WAGE-AMOUNT.
J54027     MOVE WS-GM3-EFFORT-PCT       TO GMGEFWS-EFFORT-PCT.
J54027     MOVE WS-GM3-SALARY-PCT       TO GMGEFWS-SALARY-PCT.
GMDIST
J54027     MOVE WS-GM3-JOB-CODE         TO GMGEFWS-JOB-CODE.
J54027     MOVE WS-GM3-POSITION         TO GMGEFWS-POSITION.
J54027     MOVE WS-GM3-PAY-CODE         TO GMGEFWS-PAY-CODE.
J54027     MOVE WS-GM3-CHECK-ID         TO GMGEFWS-CHECK-ID.
J54027     MOVE WS-GM3-GML-OBJ-ID       TO GMGEFWS-GML-OBJ-ID.
J54027     MOVE WS-GM3-PER-END-DATE     TO GMGEFWS-PER-END-DATE.
GMDIST
J54027     MOVE WS-GM3-CURRENCY-CODE    TO GMGEFWS-CURRENCY-CODE.
J54027     MOVE WS-GM3-UPDATE-EFFORT    TO GMGEFWS-UPDATE-EFFORT.
J54027     MOVE WS-GEF-EFFORT-AMT       TO GMGEFWS-WAGE-AMOUNT.
J54027*    MOVE WS-GM3-EFFORT-AMT       TO GMGEFWS-WAGE-AMOUNT.
GMDIST  
GMDIST     PERFORM 8200-UPDATE-GM-TRAN-EFFORT.
GMDIST
           ADD 1                        TO WS-TOT-GEF-COUNT
                                           WS-GEF-COUNT.

GMDIST     IF  (WS-GEF-COUNT = WS-MAX-OPS-IN-TRAN)
GMDIST         MOVE "GMGEF"            TO IFOBIWS-OBJ-TYPE
GMDIST         MOVE WS-MAX-OPS-IN-TRAN TO IFOBIWS-NBR-OBJECTS
GMDIST         PERFORM 7000-ASSIGN-OBJ-ID-70
GMDIST         COMPUTE WS-LAST-GEF-OBJ-ID  = IFOBIWS-OBJ-ID
GMDIST                                     - IFOBIWS-NBR-OBJECTS
               MOVE WS-LAST-GEF-OBJ-ID TO WS-RS-LAST-GEF-OBJ-ID
GMDIST         MOVE WS-TOT-GEF-COUNT   TO WS-RS-TOT-GEF-COUNT
GMDIST         PERFORM 840-MODIFY-CKPSET1
GMDIST         MOVE WS-RESTART-INFO    TO CKP-RESTART-INFO
GMDIST         PERFORM 820-STORE-CKPOINT
               MOVE ZEROS              TO WS-GEF-COUNT
GMDIST         PERFORM 925-AUDIT-END
GMDIST         PERFORM 910-AUDIT-BEGIN
GMDIST     END-IF.
GMDIST
GMDIST 380-END.
GMDIST     EXIT.
GMDIST****************************************************************
GMDIST 390-PROCESS-EMPLOYEE            SECTION.
GMDIST****************************************************************
GMDIST 390-START.
GMDIST
           IF (PRBRD-BROADCAST-ENABLED)
               IF  (GM3-COMPANY    NOT = R3DH3-COMPANY)
               AND (GM3-DEDUCTION-TYPE = 9)
                   MOVE 500                 TO PRBRD-ELEMENT-TYPE
                   MOVE GM3-COMPANY         TO PRBRD-ELEMENT-VALUE
                   PERFORM 610-BEG-ELEMENT-TAG
                   MOVE WFCHWS-TAG-STRING   TO BTL1-TAG-LINE
                   MOVE BROADCAST-TAG-LINE-1
                                            TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
               END-IF
               IF  (GM3-PROCESS-LEVEL NOT = R3DH3-PROCESS-LEVEL)
               AND (GM3-DEDUCTION-TYPE    = 9)
                   MOVE 501                 TO PRBRD-ELEMENT-TYPE
                   MOVE GM3-PROCESS-LEVEL   TO PRBRD-ELEMENT-VALUE
                   PERFORM 610-BEG-ELEMENT-TAG
                   MOVE WFCHWS-TAG-STRING   TO BTL1-TAG-LINE
                   MOVE BROADCAST-TAG-LINE-1
                                            TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
               END-IF
               IF  (GM3-DEPARTMENT NOT = R3DH3-DEPARTMENT)
               AND (GM3-DEDUCTION-TYPE = 9)
                   MOVE 502                 TO PRBRD-ELEMENT-TYPE
                   MOVE GM3-DEPARTMENT      TO PRBRD-ELEMENT-VALUE
                   PERFORM 610-BEG-ELEMENT-TAG
                   MOVE WFCHWS-TAG-STRING   TO BTL1-TAG-LINE
                   MOVE BROADCAST-TAG-LINE-1
                                            TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
               END-IF
           END-IF.

GMDIST     MOVE GM3-COMPANY            TO R3DH3-COMPANY.
GMDIST     MOVE GM3-EMPLOYEE           TO R3DH3-EMPLOYEE.
GMDIST     MOVE GM3-FULL-NAME          TO R3DH3-FULL-NAME.
GMDIST     MOVE GM3-PROCESS-LEVEL      TO R3DH3-PROCESS-LEVEL.
GMDIST     MOVE GM3-DEPARTMENT         TO R3DH3-DEPARTMENT.
GMDIST     MOVE R3-DED-DTL-HEADING     TO RPT-GROUP-REQUEST.
GMDIST     PERFORM 700-PRINT-RPT-GRP.
GMDIST
GMDIST     MOVE GM3-EMPLOYEE           TO WS-SV-EMPLOYEE.
GMDIST
J54027     PERFORM 376-MOVE-GM3-TO-WSGM3.
J54027     INITIALIZE WS-TOT-DED-DIST-AMT.
J54027
GMDIST     PERFORM 393-PROCESS-DEDUCTION
GMDIST         UNTIL (END-OF-FILE)
GMDIST         OR    (GM3-DEDUCTION-TYPE  NOT = 9)
GMDIST         OR    (GM3-EMPLOYEE        NOT = WS-SV-EMPLOYEE).
GMDIST
           IF (PRBRD-BROADCAST-ENABLED)
               IF (GM3-COMPANY NOT = R3DH3-COMPANY)
                   MOVE 500                 TO PRBRD-ELEMENT-TYPE
                   MOVE R3DH3-COMPANY       TO PRBRD-ELEMENT-VALUE
                   PERFORM 615-END-ELEMENT-TAG
                   MOVE WFCHWS-TAG-STRING   TO BTL1-TAG-LINE
                   MOVE BROADCAST-TAG-LINE-1
                                            TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
               END-IF
               IF (GM3-PROCESS-LEVEL NOT = R3DH3-PROCESS-LEVEL)
                   MOVE 501                 TO PRBRD-ELEMENT-TYPE
                   MOVE R3DH3-PROCESS-LEVEL TO PRBRD-ELEMENT-VALUE
                   PERFORM 615-END-ELEMENT-TAG
                   MOVE WFCHWS-TAG-STRING   TO BTL1-TAG-LINE
                   MOVE BROADCAST-TAG-LINE-1
                                            TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
               END-IF
               IF (GM3-DEPARTMENT NOT = R3DH3-DEPARTMENT)
                   MOVE 502                 TO PRBRD-ELEMENT-TYPE
                   MOVE R3DH3-DEPARTMENT    TO PRBRD-ELEMENT-VALUE
                   PERFORM 615-END-ELEMENT-TAG
                   MOVE WFCHWS-TAG-STRING   TO BTL1-TAG-LINE
                   MOVE BROADCAST-TAG-LINE-1
                                            TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
               END-IF
           END-IF.

GMDIST 390-END.
GMDIST     EXIT.
GMDIST****************************************************************
GMDIST 393-PROCESS-DEDUCTION         SECTION.
GMDIST****************************************************************
GMDIST 393-START.
GMDIST
GMDIST     INITIALIZE                         WS-TOT-DED-DIST-AMT.
GMDIST
GMDIST     MOVE GM3-DED-CODE               TO WS-SV-DED-CODE.
GMDIST     MOVE GM3-TR-DATE                TO WS-SV-TR-DATE.
GMDIST
J54027     INITIALIZE WS-TOT-TRDED-DIST-AMT.
GMDIST     PERFORM 394-PROCESS-DEDUCTION-DETAIL
GMDIST         UNTIL (END-OF-FILE)
GMDIST         OR    (GM3-DEDUCTION-TYPE  NOT = 9)
GMDIST         OR    (GM3-EMPLOYEE        NOT = WS-SV-EMPLOYEE)
GMDIST         OR    (GM3-DED-CODE        NOT = WS-SV-DED-CODE).
GMDIST
J54027     PERFORM 395-PRINT-DEDUCTION.
J54027
GMDIST     INITIALIZE R3T2-MESSAGE.
GMDIST
GMDIST     IF (WS-SV-TR-DATE  = ZEROS)
GMDIST         MOVE 162                         TO CRT-MSG-NBR
GMDIST         MOVE WS-SV-DED-CODE              TO CRT-ERR-VAR1
GMDIST         PERFORM 790-GET-MSG
GMDIST         MOVE CRT-MESSAGE                 TO R3T2-MESSAGE.
GMDIST
GMDIST     MOVE 160                         TO CRT-MSG-NBR.
GMDIST     MOVE WS-SV-DED-CODE              TO CRT-ERR-VAR1.
GMDIST     PERFORM 790-GET-MSG.
GMDIST     MOVE CRT-MESSAGE                 TO JSTWS-STR.
GMDIST     MOVE 50                          TO JSTWS-STR-LEN.
GMDIST     SET JSTWS-RIGHT                  TO TRUE.
GMDIST     SET JSTWS-NO-REMOVE-0            TO TRUE.
GMDIST     PERFORM 2000-JUSTIFY-OBJECT.
GMDIST     MOVE JSTWS-STR-OUT               TO R3T2-TOTAL-LABEL.
GMDIST     MOVE WS-TOT-DED-DIST-AMT         TO R3T2-TOT-DED-DIST-AMT.
GMDIST     MOVE R3-TOTAL-2                  TO RPT-GROUP-REQUEST.
GMDIST     PERFORM 700-PRINT-RPT-GRP.
GMDIST
J54027     INITIALIZE                     WS-TOT-DED-DIST-AMT.
J54027
J54027     INITIALIZE WS-GM3-VARIABLES.
J54027
GMDIST 393-END.
GMDIST     EXIT.
J54027****************************************************************
J54027 394-PROCESS-DEDUCTION-DETAIL   SECTION.
J54027****************************************************************
J54027 394-START.
J54027
J54027     ADD GM3-DIST-AMT             TO WS-TOT-DED-DIST-AMT.   
J54027
J54027     IF  ((GM3-TR-DATE          NOT = WS-GM3-TR-DATE)
J54027     OR   (GM3-ACTIVITY         NOT = WS-GM3-ACTIVITY)
J54027     OR   (GM3-ACCT-CATEGORY    NOT = WS-GM3-ACCT-CATEGORY)
J54027     OR   (GM3-TO-COMPANY       NOT = WS-GM3-TO-COMPANY)
J54027     OR   (GM3-ACCT-UNIT        NOT = WS-GM3-ACCT-UNIT)
J54027     OR   (GM3-ACCOUNT          NOT = WS-GM3-ACCOUNT)
J54027     OR   (GM3-SUB-ACCOUNT      NOT = WS-GM3-SUB-ACCOUNT)
J54027     OR   (END-OF-FILE))
J54027     AND ((GM3-TR-DATE          NOT = ZEROS) 
J54027     AND  (GM3-ACTIVITY         NOT = SPACES) 
J54027     AND  (GM3-ACCT-CATEGORY    NOT = SPACES)
J54027     AND  (WS-GM3-TR-DATE       NOT = ZEROS)
J54027     AND  (WS-GM3-ACTIVITY      NOT = SPACES)
J54027     AND  (WS-GM3-ACCT-CATEGORY NOT = SPACES))
J54027         PERFORM 395-PRINT-DEDUCTION
J54027
J54027         INITIALIZE WS-TOT-TRDED-DIST-AMT
J54027         PERFORM 376-MOVE-GM3-TO-WSGM3
J54027     ELSE
J54027         PERFORM 376-MOVE-GM3-TO-WSGM3
J54027         GO TO 394-NEXT-GMWORK
J54027     END-IF.
J54027
J54027 394-NEXT-GMWORK.
J54027
J54027     READ GMWORK-FILE           INTO GM3-GMWORK-REC
J54027         AT END
J54027             SET END-OF-FILE     TO TRUE.
J54027
J54027 394-END.
J54027     EXIT.
GMDIST****************************************************************
GMDIST 395-PRINT-DEDUCTION            SECTION.
GMDIST****************************************************************
GMDIST 395-START.
GMDIST
GMDIST     MOVE "PR"                   TO IFACWS-SYSTEM.
J54027     MOVE WS-GM3-TO-COMPANY      TO IFACWS-COMPANY.
J54027     MOVE WS-GM3-ACCT-UNIT       TO IFACWS-ACCT-UNIT.
J54027     MOVE WS-GM3-ACCOUNT         TO IFACWS-ACCOUNT.
J54027     MOVE WS-GM3-SUB-ACCOUNT     TO IFACWS-SUB-ACCOUNT.
GMDIST     MOVE 0                      TO IFACWS-EDIT-TYPE.
J54027     MOVE WS-GM3-COMPANY         TO WS-TRD-DIST-COMPANY.
J54027     MOVE WS-GM3-TR-PCD-SEQ-NBR  TO WS-TRD-PCD-SEQ-NBR.
J47059     PERFORM 7850-GET-GLT-TYPE.
GMDIST     PERFORM 635-EDIT-GLMASTER-60.
GMDIST     IF (ERROR-FOUND)
GMDIST         INITIALIZE             CRT-ERROR-NBR
GMDIST                                CRT-ERROR-CAT
GMDIST                                R3D2-GLM-DESCRIPTION
GMDIST     ELSE
GMDIST         MOVE IFACWS-GLM-DESCRIPTION TO R3D2-GLM-DESCRIPTION.
GMDIST
J54027     MOVE WS-GM3-ACCT-CATEGORY   TO ACACWS-ACCT-CATEGORY.
J54027     MOVE WS-GM3-ACTIVITY        TO ACACWS-ACTIVITY.
GMDIST     MOVE 2                      TO ACACWS-EDIT-CODE.
GMDIST     PERFORM 640-EDIT-ACTIVITY-70.
GMDIST     IF (ERROR-FOUND)
GMDIST         INITIALIZE CRT-ERROR-NBR
GMDIST                    CRT-ERROR-CAT
GMDIST                    R3D2-ACV-DESCRIPTION
GMDIST     ELSE
GMDIST         MOVE ACACWS-ACV-DESCRIPTION TO R3D2-ACV-DESCRIPTION.
GMDIST
J54027     MOVE WS-GM3-TR-DATE          TO R3D2-TR-DATE.
J54027     MOVE WS-GM3-ACTIVITY         TO R3D2-ACTIVITY.
J54027     MOVE WS-GM3-ACCT-CATEGORY    TO R3D2-ACCT-CATEGORY.
J54027     MOVE WS-GM3-TO-COMPANY       TO R3D2-TO-COMPANY.
J54027     MOVE WS-GM3-ACCT-UNIT        TO R3D2-ACCT-UNIT.
J54027     MOVE WS-GM3-ACCOUNT          TO R3D2-ACCOUNT.
J54027     MOVE WS-GM3-SUB-ACCOUNT      TO R3D2-SUB-ACCOUNT.
J54027*    MOVE GM3-DIST-AMT            TO R3D2-DIST-AMT.
J54027     MOVE WS-TOT-TRDED-DIST-AMT   TO R3D2-DIST-AMT.
J54027     MOVE WS-GM3-DED-CODE         TO R3D2-DED-CODE.
GMDIST     MOVE R3-DETAIL-2             TO RPT-GROUP-REQUEST.
GMDIST     PERFORM 700-PRINT-RPT-GRP.
GMDIST
J54027*    ADD GM3-DIST-AMT             TO WS-TOT-DED-DIST-AMT.
J54027*
J54027*    READ GMWORK-FILE           INTO GM3-GMWORK-REC
J54027*        AT END
J54027*            SET END-OF-FILE     TO TRUE.
GMDIST 
GMDIST 395-END.
GMDIST     EXIT.
      ****************************************************************
       400-PROCESS-ERR-ERRORS          SECTION.
      ****************************************************************
       400-START.

           OPEN INPUT ERRORS-FILE.

           INITIALIZE                     WS-END-OF-FILE-SW.

           READ ERRORS-FILE          INTO WR3-WORK3-REC
               AT END
                   SET END-OF-FILE     TO TRUE
                   GO TO 400-CONTINUE.

           ADD 1                       TO WS-RECORD-COUNT.

           PERFORM 405-PRINT-COMPANY
           THRU    405-END
               UNTIL (END-OF-FILE).

       400-CONTINUE.
           CLOSE ERRORS-FILE.

           IF (WS-RECORD-COUNT = ZEROS)
               MOVE WS-ERRORS-FILE-NAME
                                       TO WS-TMP-FILE
               PERFORM 901-REMOVE-TMP-FILE.

P79800     PERFORM 440-ACCOUNT-ERRORS
P79800     THRU    440-END.

           GO TO 400-END.

      ****************************************************************
       405-PRINT-COMPANY.
      ****************************************************************

           MOVE WS-RS-RUN-TIME         TO PEH-RUN-TIME.
           MOVE PRM-COMPANY            TO PEH-COMPANY
                                          WS-COMPANY.
           MOVE WS-CO-NAME             TO PEH-NAME.
           MOVE PRPRD-CO-CURRENCY-CODE TO PEH-CURRENCY-CODE.

           MOVE PAGE-ERR-HEADING       TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

           PERFORM 410-PRINT-HM-PROC-LEVEL
           THRU    410-END
             UNTIL (END-OF-FILE)
               OR  (WR3-COMPANY         NOT = WS-COMPANY).

       405-END.

      ****************************************************************
       410-PRINT-HM-PROC-LEVEL.
      ****************************************************************

           MOVE PRM-COMPANY            TO DB-COMPANY.
           MOVE WR3-HM-PROC-LEVEL      TO WS-HM-PROC-LEVEL
                                          PLEH-PROCESS-LEVEL
                                          DB-PROCESS-LEVEL.

           PERFORM 840-FIND-PRSSET1.
           MOVE PRS-NAME               TO PLEH-PL-NAME.
           MOVE HM-PROC-LEVEL-ERR-HEADING
                                       TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

           MOVE WS-MESSAGE-150         TO DH-CONTINUED.

           PERFORM 415-PRINT-TO-COMPANY
           THRU    415-END
             UNTIL (END-OF-FILE)
               OR  (WR3-COMPANY         NOT = WS-COMPANY)
               OR  (WR3-HM-PROC-LEVEL   NOT = WS-HM-PROC-LEVEL).

       410-END.

      ****************************************************************
       415-PRINT-TO-COMPANY.
      ****************************************************************

           MOVE WR3-TO-COMPANY         TO WS-DIST-COMPANY
                                          DCEH-DIST-COMPANY
                                          IFCOWS-COMPANY.

           IF (WR3-TO-COMPANY   NOT = PRM-COMPANY)
               MOVE "PR"               TO IFCOWS-SYSTEM
               PERFORM 615-EDIT-COMPANY-60
               MOVE IFCOWS-GLS-NAME    TO DCEH-GLS-NAME
               MOVE IFCOWS-CURRENCY-CODE
                                       TO DCEH-CURRENCY-CODE
           ELSE
               MOVE WS-CO-NAME         TO DCEH-GLS-NAME
               INITIALIZE                 DCEH-CURRENCY-CODE.

           MOVE DIST-COMP-ERR-HEADING  TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

           PERFORM 420-PRINT-GL-ACCOUNT
           THRU    420-END
             UNTIL (END-OF-FILE)
               OR  (WR3-COMPANY         NOT = WS-COMPANY)
               OR  (WR3-HM-PROC-LEVEL   NOT = WS-HM-PROC-LEVEL)
               OR  (WR3-TO-COMPANY      NOT = WS-DIST-COMPANY).

       415-END.

      ****************************************************************
       420-PRINT-GL-ACCOUNT.
      ****************************************************************

DKR        MOVE "PR"                   TO IFACWS-SYSTEM.
           MOVE WR3-ACCT-UNIT          TO WS-ACCT-UNIT
                                          AEH-ACCT-UNIT.
           MOVE WR3-ACCOUNT            TO WS-ACCOUNT
                                          AEH-ACCOUNT.
           MOVE WR3-SUB-ACCOUNT        TO WS-SUB-ACCOUNT
                                          AEH-SUB-ACCT.

           MOVE ZEROS                  TO CRT-ERROR-NBR.
           MOVE 0                      TO IFACWS-EDIT-TYPE.
J47059     MOVE WR3-COMPANY            TO WS-TRD-DIST-COMPANY.
J47059     MOVE WR3-PCD-SEQ-NBR        TO WS-TRD-PCD-SEQ-NBR.
J27195     MOVE WR3-HOURS-AMOUNT       TO WS-TRD-HOURS.
J27195     MOVE WR3-UNITS-AMOUNT       TO WS-TRD-PAY-UNITS.
J47059     PERFORM 7850-GET-GLT-TYPE.
           PERFORM 635-EDIT-GLMASTER-60.
           IF (ERROR-FOUND)
               MOVE CRT-ERROR-NBR      TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE        TO AEH-DESCRIPTION
               INITIALIZE                 CRT-ERROR-NBR
           ELSE
               MOVE IFACWS-GLM-DESCRIPTION
                                       TO AEH-DESCRIPTION
           END-IF.

           MOVE ACCOUNT-ERR-HEADING    TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

           PERFORM 425-PRINT-EMPLOYEE
           THRU    425-END
             UNTIL (END-OF-FILE)
               OR  (WR3-COMPANY         NOT = WS-COMPANY)
               OR  (WR3-HM-PROC-LEVEL   NOT = WS-HM-PROC-LEVEL)
               OR  (WR3-TO-COMPANY      NOT = WS-DIST-COMPANY)
               OR  (WR3-ACCT-UNIT       NOT = WS-ACCT-UNIT)
               OR  (WR3-ACCOUNT         NOT = WS-ACCOUNT)
               OR  (WR3-SUB-ACCOUNT     NOT = WS-SUB-ACCOUNT).

       420-END.

      ****************************************************************
       425-PRINT-EMPLOYEE.
      ****************************************************************

           MOVE WR3-GL-DATE            TO WS-GL-DATE
                                          PEL-GL-DATE.
           MOVE WR3-EMPLOYEE           TO WS-EMPLOYEE
                                          PEL-EMPLOYEE.

           MOVE WR3-FULL-NAME          TO PEL-FULL-NAME.

           MOVE WR3-CURRENCY-CODE      TO PEL-TRAN-CURRENCY.
           MOVE WR3-TRAN-AMOUNT        TO PEL-TRAN-AMT.
           MOVE WR3-TRAN-ND            TO PEL-TRAN-AMT-ND.

           MOVE POSTING-ERR-LINE       TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

           PERFORM 430-PRINT-WR3-MESSAGES
           THRU    430-END
              VARYING I1 FROM 1 BY 1
              UNTIL  (I1 > 10)
                 OR  (WR3-MSG-NBR (I1) = ZEROS).

           READ ERRORS-FILE             INTO WR3-WORK3-REC
               AT END
                   SET END-OF-FILE     TO TRUE.

       425-END.

      ****************************************************************
       430-PRINT-WR3-MESSAGES.
      ****************************************************************

           IF (WR3-MSG-NBR (I1) NOT = ZEROS)
               MOVE WR3-MSG-NBR (I1)   TO CRT-MSG-NBR
                                          EML-ERR-NBR
               MOVE "PRGLI"            TO CRT-ERROR-CAT
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE        TO EML-MESSAGE
               MOVE ERROR-MESSAGE-LINE TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP.

       430-END.

P79800****************************************************************
P79800 440-ACCOUNT-ERRORS.
P79800****************************************************************

           MOVE ZEROS                  TO WS-END-OF-FILE-SW.

           SORT GLERR-SORT-FILE
               ASCENDING KEY           DGS-COMPANY
                                       DGS-TO-COMPANY
                                       DGS-ACCT-UNIT
                                       DGS-ACCOUNT
                                       DGS-SUB-ACCOUNT
                                       DGS-EMPLOYEE
               USING                   GLERR-WORK-FILE
               GIVING                  GLERR-WORK-FILE SAVE.
       
           SET NO-END-OF-FILE          TO TRUE.
       
           OPEN INPUT GLERR-WORK-FILE.

           READ GLERR-WORK-FILE       INTO GE-GLERR-WORK-REC
                AT END
                    MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
       
           IF  (END-OF-FILE)
               GO TO 440-END.

           OPEN OUTPUT ACCT-ERROR-FILE.
           MOVE PRM-COMPANY        TO R4PH-COMPANY.
           MOVE PRS-NAME           TO R4PH-NAME.
          
           MOVE R4-PAGE-HEADING                TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

           IF  (PRM-DETAIL-RPT-OPT = "N")
               MOVE R4-ACCOUNT-ERR-RPT-HEADING     TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP.

           PERFORM 445-PRINT-ACCOUNT-ERRORS
           THRU    445-END
               UNTIL (END-OF-FILE).

           CLOSE ACCT-ERROR-FILE.

       440-END.

      ****************************************************************
       445-PRINT-ACCOUNT-ERRORS.
      ****************************************************************

           IF  (PRM-DETAIL-RPT-OPT = "Y")
               MOVE R4-ACCOUNT-ERR-RPT-HEADING     TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP.

           MOVE GE-COMPANY             TO WS-COMPANY.
           MOVE GE-TO-COMPANY          TO WS-TO-COMPANY.
           MOVE GE-ACCT-UNIT           TO WS-DST-ACCT-UNIT.
           MOVE GE-ACCOUNT             TO WS-DST-ACCOUNT.
           MOVE GE-SUB-ACCOUNT         TO WS-DST-SUB-ACCT.

           MOVE GE-TO-COMPANY          TO R4RL-DIST-COMPANY.
           MOVE GE-ACCT-UNIT           TO R4RL-ACCT-UNIT.
           MOVE GE-ACCOUNT             TO R4RL-ACCOUNT.
           MOVE GE-SUB-ACCOUNT         TO R4RL-SUB-ACCOUNT.
           MOVE GE-MESSAGE             TO R4RL-MESSAGE.

           MOVE R4-ERROR-RPT-LINE          TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

           IF  (PRM-DETAIL-RPT-OPT = "Y")
           AND (GE-CLEARING    NOT = "Y")
               MOVE R4-EMPLOYEE-HEADING    TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP.

           PERFORM 450-PRINT-ERRORS-FOR-ACCOUNT
           THRU    450-END
               UNTIL (END-OF-FILE)
               OR    (GE-COMPANY           NOT = WS-COMPANY)
               OR    (GE-TO-COMPANY        NOT = WS-TO-COMPANY)
               OR    (GE-ACCT-UNIT         NOT = WS-DST-ACCT-UNIT)
               OR    (GE-ACCOUNT           NOT = WS-DST-ACCOUNT)
               OR    (GE-SUB-ACCOUNT       NOT = WS-DST-SUB-ACCT).

       445-END.
      ****************************************************************
       450-PRINT-ERRORS-FOR-ACCOUNT.
      ****************************************************************

           IF  (PRM-DETAIL-RPT-OPT = "Y")
           AND (GE-CLEARING    NOT = "Y")
               MOVE GE-EMPLOYEE            TO R4EL-EMPLOYEE
               MOVE GE-FULL-NAME           TO R4EL-FULL-NAME
               MOVE GE-TRAN-AMOUNT         TO R4EL-AMOUNT
               MOVE SPACES                 TO R4EL-DED-CODE
                                              R4EL-PAY-CODE
                                              R4EL-DESCRIPTION
               IF  (GE-DED-CODE NOT = SPACES)
                   MOVE PRM-COMPANY        TO DB-COMPANY
                   MOVE GE-DED-CODE        TO R4EL-DED-CODE
                                              DB-DED-CODE
                   PERFORM 840-FIND-DDCSET1
                   MOVE DDC-DESCRIPTION    TO R4EL-DESCRIPTION
               END-IF
               IF  (GE-PCD-SEQ-NBR NOT = ZEROS)
                   MOVE PRM-COMPANY        TO DB-COMPANY
                   MOVE GE-PAY-CODE        TO R4EL-PAY-CODE
J57679*            MOVE GE-PCD-SEQ-NBR     TO DB-SEQ-NBR
J57679*            PERFORM 840-FIND-PCDSET5

J57679             IF (GE-PCD-SEQ-NBR > 9999)
J57679                 MOVE GE-PCD-SEQ-NBR     TO DB-PCD-SEQ-NBR
J57679                 PERFORM 840-FIND-PCDSET7
J57679             ELSE
J57679                 MOVE GE-PCD-SEQ-NBR     TO DB-SEQ-NBR
J57679                 PERFORM 840-FIND-PCDSET5
J57679             END-IF 

                   MOVE PCD-DESCRIPTION    TO R4EL-DESCRIPTION
               END-IF

               MOVE R4-EMPLOYEE-LINE       TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP.

           READ GLERR-WORK-FILE       INTO GE-GLERR-WORK-REC
                AT END
                    MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
       
       450-END.

       400-END.

322900****************************************************************
323000 500-UPDATE-BENEFITS-INFO        SECTION.
323100****************************************************************
323200 500-START.
323300
323400     INITIALIZE WS-BENWORK-TABLE
323500                WS-BENWORK-TABLE-2
J72349                WS-EE-401K-PLAN-FOUND
J72349                WS-EE-409A-AMOUNT
                      WS-PLN-TBL-2
                      WS-VEST-HOURS
                      WS-HOURS-SERV
                      WS-WAGE-AMOUNT.

325400     IF (WS-PRT-SIZE > 0)
325500         PERFORM 510-ADD-PRTIME
325600         THRU    510-END
325700             VARYING I3 FROM 1 BY 1
325800             UNTIL  (I3 > WS-PRT-SIZE).
325900
326000     MOVE PYM-COMPANY            TO DB-COMPANY.
326100     MOVE PYM-EMPLOYEE           TO DB-EMPLOYEE.
326200     MOVE BENSET4-EMPLOYEE       TO WS-DB-BEG-RNG.

           IF  (PRPRD-PYMSET6)
           OR  (REPORT-ONLY)
               PERFORM 850-FIND-BEGRNG-BENSET4
           ELSE
               PERFORM 850-MODIFY-BEGRNG-BENSET4.

           PERFORM
               UNTIL (BENEFIT-NOTFOUND)
      **THIS ROUNTINE WILL UPDATE CURRENT BENEFITS *********
               IF    (PYM-CHECK-DATE NOT < BEN-START-DATE)
P86230         AND (((PYM-CHECK-DATE NOT > BEN-STOP-DATE)
P86230         AND   (BEN-STOP-DATE  NOT = ZEROES))
P86230         OR    (BEN-STOP-DATE      = ZEROES))
327100             MOVE WS-FALSE       TO WS-PLAN-FOUND-SW
327200             MOVE 1              TO I1
327300             PERFORM
327400                 UNTIL (I1 > WS-PLN-TBL-SIZE)
327500                 OR    (WS-PLAN-FOUND)
327600
327700                 IF  (WS-PLN-PLAN-TYPE (I1) = BEN-PLAN-TYPE)
327800                 AND (WS-PLN-PLAN-CODE (I1) = BEN-PLAN-CODE)
327900                     IF ((WS-PLN-START-DATE (I1) > BEN-STOP-DATE)
328000                     AND (BEN-STOP-DATE          NOT = ZEROS))
328100                     OR  (WS-PLN-START-DATE (I1) > BEN-START-DATE)
328200                     OR ((WS-PLN-STOP-DATE (I1)  < BEN-START-DATE)
328300                     AND (WS-PLN-STOP-DATE (I1)  NOT = ZEROS))
328400                         ADD 1           TO I1
328500                     ELSE
328600                         MOVE WS-TRUE    TO WS-PLAN-FOUND-SW
328700                     END-IF
328800                 ELSE
328900                     ADD 1               TO I1
329000                 END-IF
329100
329200             END-PERFORM
329300             IF (WS-PLAN-FOUND)
329400                 MOVE "Y"        TO WS-BENEFIT-DONE (I1)
329500                 PERFORM 520-READ-PLAN-TBL
329600                 THRU    520-END
329700             END-IF
               ELSE
      **THIS ROUNTINE WILL UPDATE STOPPED BENEFITS FOR ONE TIME DEDS ***
                MOVE WS-FALSE       TO WS-PLAN-FOUND-SW
                MOVE 1              TO I1
                PERFORM
                 UNTIL (I1 > WS-PLN-TBL-SIZE)
                 OR    (WS-PLN-PLAN-TYPE (I1)    = SPACES)
                 OR    (WS-PLAN-FOUND)
      *** DON'T SEARCH THE WS-PLN TABLE IF THERE ARE NO ONE-TIMES ***
P80250           OR    (WS-OTD-DED-CODE (1) = SPACES)

                  IF  (WS-PLN-PLAN-TYPE (I1) = BEN-PLAN-TYPE)
                  AND (WS-PLN-PLAN-CODE (I1) = BEN-PLAN-CODE)
      *** SEARCH OTD TABLE FOR BENEFIT ***
                   PERFORM
                    VARYING I5 FROM 1 BY 1
                    UNTIL  (I5 > WS-EMP-OTD-TABLE-SIZE)
                    OR     (WS-OTD-DED-CODE (I5) = SPACES)
                    OR     (WS-PLAN-FOUND)

                     IF  (WS-OTD-BEN-PLAN-TYPE (I5)  = BEN-PLAN-TYPE)
                     AND (WS-OTD-BEN-PLAN-CODE (I5)  = BEN-PLAN-CODE)
                     AND (WS-OTD-BEN-START-DATE (I5) = BEN-START-DATE)
                          MOVE WS-TRUE TO WS-PLAN-FOUND-SW
                     END-IF

                   END-PERFORM
                   IF (WS-PLAN-FOUND)
                       MOVE "Y"        TO WS-BENEFIT-DONE (I1)
                       PERFORM 520-READ-PLAN-TBL
                       THRU    520-END
                   END-IF
                  END-IF
                  ADD 1             TO I1

                END-PERFORM
               END-IF
P82279         IF  (PYM-CHECK-TYPE = "J")
P82279         AND (NOT WS-PLAN-FOUND)
P82279             MOVE 1          TO I1
P82279             PERFORM UNTIL (I1 > WS-PLN-TBL-SIZE)
P82279                     OR    (WS-PLAN-FOUND)
P82279                     IF  (WS-PLN-PLAN-TYPE (I1) = BEN-PLAN-TYPE)
P82279                     AND (WS-PLN-PLAN-CODE (I1) = BEN-PLAN-CODE)
P82279                         MOVE WS-TRUE    TO WS-PLAN-FOUND-SW
P82279                     ELSE
P82279                         ADD 1           TO I1
P82279                     END-IF
P82279             END-PERFORM
P82279             IF (WS-PLAN-FOUND) 
P82279                 MOVE "Y"        TO WS-BENEFIT-DONE (I1)
P82279                 PERFORM 520-READ-PLAN-TBL
P82279                 THRU    520-END  
P82279             END-IF
P82279         END-IF
               IF (PRPRD-PYMSET6)
               OR (REPORT-ONLY)
                   PERFORM 860-FIND-NXTRNG-BENSET4
               ELSE
                   PERFORM 860-MODIFY-NXTRNG-BENSET4
               END-IF
           END-PERFORM.

330200     PERFORM
330300         VARYING I1 FROM 1 BY 1
330400         UNTIL  (I1 > WS-PLN-TBL-SIZE)
330500
330600         IF (WS-BENEFIT-DONE (I1) NOT = "Y")
330700             PERFORM 520-READ-PLAN-TBL
330800             THRU    520-END
330900         END-IF
331000     END-PERFORM.
331100
331200     GO TO 500-END.
331300
331400******************************************************************
331500 510-ADD-PRTIME.
331600******************************************************************
331700
331800     IF (WS-PRT-OT-RECORD (I3) NOT = "Y")
331900         ADD WS-PRT-HOURS (I3)   TO WS-HOURS-SERV
332000                                    WS-VEST-HOURS.
332100     ADD WS-PRT-WAGE-AMOUNT (I3) TO WS-WAGE-AMOUNT.
332200
332300     PERFORM
332400         VARYING I1 FROM 1 BY 1
332500         UNTIL  (I1 > WS-PLN-DB-DC-SIZE)
332600
332700         IF (WS-PLN-COMP-CLASS (I1) NOT = SPACES)
332800             MOVE PRM-COMPANY             TO DB-COMPANY
332900             MOVE WS-PLN-COMP-CLASS (I1)  TO DB-PAY-CLASS
333000             MOVE WS-PRT-PAY-SUM-GRP (I3) TO DB-PAY-SUM-GRP
333100             PERFORM 840-FIND-PSRSET1
333200             IF (PSGRELATE-FOUND)
333300                 IF (PSR-HOURS-FLAG = "S")
333400                     SUBTRACT WS-PRT-HOURS (I3)
333500                                          FROM WS-BASE-HOURS (I1)
333600                 ELSE
333700                 IF (PSR-HOURS-FLAG NOT = "E")
333800                     ADD WS-PRT-HOURS (I3) TO WS-BASE-HOURS (I1)
333900                 END-IF
334000                 END-IF
334100                 IF (PSR-WAGES-FLAG = "S")
334200                     SUBTRACT WS-PRT-WAGE-AMOUNT (I3)
334300                                         FROM WS-BASE-AMOUNT (I1)
334400                 ELSE
334500                 IF (PSR-WAGES-FLAG NOT = "E")
334600                     ADD WS-PRT-WAGE-AMOUNT (I3)
334700                                         TO WS-BASE-AMOUNT (I1)
334800                 END-IF
334900                 END-IF
335000             END-IF
335100         END-IF
335200         IF (WS-PLN-VEST-CLASS (I1) NOT = SPACES)
335300             MOVE PRM-COMPANY             TO DB-COMPANY
335400             MOVE WS-PLN-VEST-CLASS (I1)  TO DB-PAY-CLASS
335500             MOVE WS-PRT-PAY-SUM-GRP (I3) TO DB-PAY-SUM-GRP
335600             PERFORM 840-FIND-PSRSET1
335700             IF (PSGRELATE-FOUND)
335800                 IF (PSR-HOURS-FLAG = "S")
335900                     SUBTRACT WS-PRT-HOURS (I3)
336000                                     FROM WS-BASE-VEST-HOURS (I1)
336100                 ELSE
336200                 IF (PSR-HOURS-FLAG NOT = "E")
336300                     ADD WS-PRT-HOURS (I3)
336400                                     TO WS-BASE-VEST-HOURS (I1)
336500                 END-IF
336600                 END-IF
336700             END-IF
336800         END-IF
336900     END-PERFORM.
337000
337100 510-END.
337200
337300******************************************************************
337400 520-READ-PLAN-TBL.
337500******************************************************************
337600
337700     MOVE ZEROS                  TO WS-COD-PRE-TAX-YTD
337800                                    WS-COD-AFT-TAX-YTD
337900                                    WS-COD-CMP-CONT-YTD.

           IF (WS-BENEFIT-DONE (I1) = "Y")
               IF (WS-PLN-PLAN-TYPE (I1) = "DB" OR "DC")
                   MOVE WS-TRUE          TO WS-BENEFIT-EXISTS-SW
               ELSE
                   IF (WS-PLN-PLAN-TYPE (I1) = "RS" OR "SB" OR "SP")
                       PERFORM 530-SEARCH-PLN-DED-CODES
                       THRU    530-END
                          VARYING I2 FROM 1 BY 1
                          UNTIL  (I2 > 15)
                       IF  (NOT PRPRD-PYMSET6)
                       AND (UPDATING)
                            PERFORM 820-STORE-BENEFIT
                       END-IF
                   END-IF
                   GO TO 520-END
               END-IF
           ELSE
               IF (WS-PLN-PLAN-TYPE (I1) = "DB" OR "DC")
                   MOVE WS-FALSE         TO WS-BENEFIT-EXISTS-SW
               ELSE
                   GO TO 520-END.
339900
340000     MOVE PYM-CHECK-DATE         TO WS-PER-END-DATE.
P12742     MOVE PYM-PER-END-DATE       TO WS-VES-END-DATE.

340100     MOVE WS-PLN-START-DATE (I1) TO WS-PLAN-BEG-DATE.
340200
340300     IF (WS-PLN-FROM-DATE (I1) = "HI")
340400         MOVE EMP-DATE-HIRED     TO WS-HIRE-DATE
340500     ELSE
340600     IF (WS-PLN-FROM-DATE (I1) = "AJ")
340700         MOVE EMP-ADJ-HIRE-DATE  TO WS-HIRE-DATE
340800     ELSE
340900     IF (WS-PLN-FROM-DATE (I1) = "AN")
341000         MOVE EMP-ANNIVERS-DATE  TO WS-HIRE-DATE
341100     ELSE
341200         MOVE EMP-COMPANY        TO DB-COMPANY
341300         MOVE EMP-EMPLOYEE       TO DB-EMPLOYEE
341400         PERFORM 840-FIND-PEMSET1
341500         IF (WS-PLN-FROM-DATE (I1) = "SN")
341600             MOVE PEM-SENIOR-DATE    TO WS-HIRE-DATE
341700         ELSE
341800         IF (WS-PLN-FROM-DATE (I1) = "B1")
341900             MOVE PEM-BEN-DATE-1     TO WS-HIRE-DATE
342000         ELSE
342100         IF (WS-PLN-FROM-DATE (I1) = "B2")
342200             MOVE PEM-BEN-DATE-2     TO WS-HIRE-DATE
342300         ELSE
342400         IF (WS-PLN-FROM-DATE (I1) = "B3")
342500             MOVE PEM-BEN-DATE-3     TO WS-HIRE-DATE
342600         ELSE
342700         IF (WS-PLN-FROM-DATE (I1) = "B4")
342800             MOVE PEM-BEN-DATE-4     TO WS-HIRE-DATE
342900         ELSE
343000         IF (WS-PLN-FROM-DATE (I1) = "B5")
343100             MOVE PEM-BEN-DATE-5     TO WS-HIRE-DATE.
343200
343300     IF (PYM-CHECK-DATE          < WS-HIRE-DATE)
343400         GO TO 520-END.
343500
P56491*    IF  (WS-VES-END-DATE        < WS-PLAN-BEG-DATE)
P56491*    AND (WS-PLN-VEST-BEF-FLG (I1) NOT = "Y")
P56491*        GO TO 520-END.
343900
344000     IF (WS-PLN-VEST-DATE (I1)   = SPACES)
344100         GO TO 520-CONTINUE
344200     ELSE
344300     IF (WS-PLN-VEST-DATE (I1)   = "PL")
344400         MOVE WS-PLN-START-DATE (I1)
344500                                 TO WS-VEST-BEG-DATE
344600     ELSE
344700     IF (WS-PLN-VEST-DATE (I1)   = "EM")
344800         MOVE WS-HIRE-DATE       TO WS-VEST-BEG-DATE
344900     ELSE
345000         MOVE 1                  TO WS-VEST-BEG-MONTH
345100         MOVE 1                  TO WS-VEST-BEG-DAY.
345200
P12742     MOVE WS-VES-END-YEAR        TO WS-VEST-BEG-YEAR.
345400
P12742     IF (WS-VES-END-DATE         < WS-VEST-BEG-DATE)
P12742         COMPUTE WS-VEST-BEG-YEAR = WS-VES-END-YEAR - 1.
345700
           IF ((WS-PLN-VEST-CLASS  (I1)     = SPACES)
           OR  (WS-BASE-VEST-HOURS (I1) NOT =  ZEROS))
           AND (NOT PRPRD-PYMSET6)
           AND (UPDATING)
345800         MOVE WS-PLN-PLAN-TYPE (I1)  TO DB-PLAN-TYPE
345900         MOVE WS-PLN-PLAN-CODE (I1)  TO DB-PLAN-CODE
346000         MOVE WS-VEST-BEG-YEAR       TO DB-PLAN-YEAR
346100         PERFORM 840-MODIFY-VESSET1
346200         IF (VESTHOURS-FOUND)
346300             IF (WS-BASE-VEST-HOURS (I1) NOT = ZEROS)
346400                 ADD WS-BASE-VEST-HOURS (I1)
346500                                     TO VES-VEST-HOURS
346600             ELSE
346700                 ADD WS-VEST-HOURS   TO VES-VEST-HOURS
346800             END-IF
346900         ELSE
347000             PERFORM 800-CREATE-VESTHOURS
347100             MOVE PYM-COMPANY        TO VES-COMPANY
347200             MOVE PYM-EMPLOYEE       TO VES-EMPLOYEE
347300             MOVE WS-PLN-PLAN-TYPE (I1)
347400                                     TO VES-PLAN-TYPE
347500             MOVE WS-PLN-PLAN-CODE (I1)
347600                                     TO VES-PLAN-CODE
347700             MOVE WS-VEST-BEG-YEAR   TO VES-PLAN-YEAR
347800             IF (WS-BASE-VEST-HOURS (I1) NOT = ZEROS)
347900                 ADD WS-BASE-VEST-HOURS (I1)
348000                                     TO VES-VEST-HOURS
348100             ELSE
348200                 ADD WS-VEST-HOURS   TO VES-VEST-HOURS
                   END-IF
J67329             MOVE WS-SYSTEM-DATE-YMD TO VES-CREATE-DATE
J67329             MOVE HHMMSS             TO VES-CREATE-TIME
J67329             MOVE CRT-USER-NAME      TO VES-CREATE-USER-ID                      
348300         END-IF

J67329         MOVE WS-SYSTEM-DATE-YMD     TO VES-DATE-STAMP
J67329         MOVE HHMMSS                 TO VES-TIME-STAMP
J67329         MOVE CRT-USER-NAME          TO VES-USER-ID        
    
348400         PERFORM 820-STORE-VESTHOURS.
348500
348600     IF (WS-BASE-VEST-HOURS (I1) NOT = ZEROS)
348700         MOVE WS-BASE-VEST-HOURS (I1)  TO WR-VEST-HOURS (I1)
348900     ELSE
           IF (WS-PLN-VEST-CLASS  (I1)     = SPACES)
349000         MOVE WS-VEST-HOURS            TO WR-VEST-HOURS (I1)
           END-IF.
349100
349200 520-CONTINUE.
349300
349400     IF (NO-BENEFIT-EXISTS)
349500     OR (PYM-CHECK-DATE          < WS-PLAN-BEG-DATE)
349600         GO TO 520-END.
349700
349800     MOVE WS-PER-END-YEAR        TO WS-PLAN-BEG-YEAR.
349900
350000     IF (WS-PER-END-DATE         < WS-PLAN-BEG-DATE)
350100         COMPUTE WS-PLAN-BEG-YEAR = WS-PER-END-YEAR - 1.
350200
350300     IF (WS-PLN-COMP-CLASS (I1) = SPACES)
350400     OR (WS-BASE-HOURS (I1)     NOT = ZEROS)
350500     OR (WS-BASE-AMOUNT (I1)    NOT = ZEROS)
350600         MOVE PYM-COMPANY           TO DB-COMPANY
350700         MOVE WS-PLN-PLAN-TYPE (I1) TO DB-PLAN-TYPE
350800         MOVE WS-PLN-PLAN-CODE (I1) TO DB-PLAN-CODE
350900         MOVE PYM-EMPLOYEE          TO DB-EMPLOYEE
351000         MOVE WS-PLAN-BEG-YEAR      TO DB-PLAN-YEAR
               IF (PRPRD-PYMSET6)
               OR (REPORT-ONLY)
                   PERFORM 840-FIND-CPHSET1
               ELSE
                   PERFORM 840-MODIFY-CPHSET1
               END-IF
J14383         IF (((PYM-CHECK-DATE NOT > BEN-STOP-DATE)
J14383         AND  (BEN-STOP-DATE  NOT = ZEROES))
J14383         OR   (BEN-STOP-DATE      = ZEROES))
351200            IF (COMPHIST-FOUND)
351300                PERFORM 521-MOVE-COMPHIST-DATA
351400                THRU    521-END
                      IF  (NOT PRPRD-PYMSET6)
                      AND (UPDATING)
                          PERFORM 820-STORE-COMPHIST
                      END-IF
351600            ELSE
                      IF  (NOT PRPRD-PYMSET6)
                      AND (UPDATING)                  
                          PERFORM 800-CREATE-COMPHIST
J67329                    MOVE WS-SYSTEM-DATE-YMD  
J67329                                         TO CPH-CREATE-DATE
J67329                    MOVE HHMMSS          TO CPH-CREATE-TIME
J67329                    MOVE CRT-USER-NAME   TO CPH-CREATE-USER-ID                               
                      ELSE
                          INITIALIZE COMPHIST
                      END-IF
351800                PERFORM 521-MOVE-COMPHIST-DATA
351900                THRU    521-END
                      IF  (NOT PRPRD-PYMSET6)
                      AND (UPDATING)
                          PERFORM 820-STORE-COMPHIST
J14383                END-IF 
J14383            END-IF
J14383         END-IF
J14383     END-IF.
352100
352200     IF (WS-PLN-PLAN-TYPE (I1)    = "DB" OR "DC")
               MOVE WS-PLN-PLAN-TYPE (I1)  TO DB-PLAN-TYPE
352300         MOVE WS-PLN-PLAN-CODE (I1)  TO DB-PLAN-CODE
352400         MOVE PYM-EMPLOYEE           TO DB-EMPLOYEE
352500         MOVE WS-PLAN-BEG-YEAR       TO DB-PLAN-YEAR
352600         PERFORM 530-SEARCH-PLN-DED-CODES
352700         THRU    530-END
352800             VARYING I2 FROM 1 BY 1
352900             UNTIL  (I2 > 15)
353000         IF  (WS-COD-PRE-TAX-YTD  = ZEROS)
353100         AND (WS-COD-AFT-TAX-YTD  = ZEROS)
353200         AND (WS-COD-CMP-CONT-YTD = ZEROS)
353300             GO TO 520-END.
353400
353500     IF (WS-PLN-PLAN-TYPE (I1)   = "DB" OR "DC")
               MOVE PYM-COMPANY            TO DB-COMPANY
               MOVE PYM-EMPLOYEE           TO DB-EMPLOYEE
               MOVE PYM-CHECK-ID           TO DB-CHECK-ID
               IF (REPORT-ONLY)
               OR (PRPRD-PYMSET6)
                   MOVE TRDSET8-CHECK-ID       TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-TRDSET8
                   IF (TIMERECORD-FOUND)
                       MOVE TRD-BUS-NBR-GRP    TO DB-BUS-NBR-GRP
                       MOVE TRD-QC-ENT-NBR-GRP TO DB-QC-ENT-NBR-GRP
                   ELSE
                       MOVE SPACES             TO DB-BUS-NBR-GRP
                                                  DB-QC-ENT-NBR-GRP
                   END-IF
               ELSE 
                   MOVE PRTSET1-CHECK-ID       TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-PRTSET1
                   IF (PRTIME-FOUND)
                       MOVE PRT-BUS-NBR-GRP    TO DB-BUS-NBR-GRP
                       MOVE PRT-QC-ENT-NBR-GRP TO DB-QC-ENT-NBR-GRP
                   ELSE
J27044                 IF (PYM-CHECK-TYPE = "J" OR "M" OR "H")
J47909                     MOVE PYM-COMPANY    TO DB-COMPANY
J47909                     MOVE PYM-EMPLOYEE   TO DB-EMPLOYEE
J47909                     MOVE PYM-CHECK-ID   TO DB-CHECK-ID
J47909                     MOVE PYDSET1-CHECK-ID 
J47909                                         TO WS-DB-BEG-RNG
J47909                     PERFORM 850-FIND-BEGRNG-PYDSET1
J47909                     IF  (PAYDEDUCTN-FOUND)
J47909                         MOVE PYD-BUS-NBR-GRP
J47909                                         TO DB-BUS-NBR-GRP
J47909                         MOVE PYD-QC-ENT-NBR-GRP
J47909                                         TO DB-QC-ENT-NBR-GRP
J47909                     ELSE
J47909                         MOVE SPACES     TO DB-BUS-NBR-GRP
J47909                                            DB-QC-ENT-NBR-GRP
J47909                     END-IF
J47909                 ELSE
                           MOVE SPACES         TO DB-BUS-NBR-GRP
                                                  DB-QC-ENT-NBR-GRP
J47909                 END-IF
                   END-IF
               END-IF
               MOVE WS-PLN-PLAN-TYPE (I1)  TO DB-PLAN-TYPE
353600         MOVE WS-PLN-PLAN-CODE (I1)  TO DB-PLAN-CODE
353700         MOVE PYM-EMPLOYEE           TO DB-EMPLOYEE
353800         MOVE WS-PLAN-BEG-YEAR       TO DB-PLAN-YEAR
      *         MOVE PYM-PROCESS-LEVEL      TO DB-PROCESS-LEVEL
P67419         IF  (EMP-WORK-COUNTRY = "CA")
P67419             MOVE PYM-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
P67419         ELSE
P67419             MOVE SPACES             TO DB-PROCESS-LEVEL
P67419         END-IF
P81387         IF ((WS-COD-CMP-CONT-YTD NOT = ZEROS)
P81387         AND (WS-PLN-CODA-CONTRIB (I1) = SPACES OR "1" OR "2"))
P81387         OR (((WS-COD-PRE-TAX-YTD NOT = ZEROS)
P81387         OR   (WS-COD-AFT-TAX-YTD NOT = ZEROES))
P81387         AND  (WS-PLN-CODA-CONTRIB (I1) = SPACES OR "1" OR "3"))
                   IF (PRPRD-PYMSET6)
                   OR (REPORT-ONLY)
                       PERFORM 840-FIND-CODSET1
                   ELSE
                       PERFORM 840-MODIFY-CODSET1
                   END-IF
354000             IF (CODA-FOUND)
354100                 PERFORM 522-MOVE-CODA-YTD
354200                 THRU    522-END
                       IF  (NOT PRPRD-PYMSET6)
                       AND (UPDATING)
                           PERFORM 820-STORE-CODA
                       END-IF
354400             ELSE
                       IF  (NOT PRPRD-PYMSET6)
                       AND (UPDATING)
                           PERFORM 800-CREATE-CODA
                       ELSE
                           INITIALIZE CODA
                       END-IF
354600                 PERFORM 522-MOVE-CODA-YTD
354700                 THRU    522-END
                       IF  (NOT PRPRD-PYMSET6)
                       AND (UPDATING)
                           PERFORM 820-STORE-CODA
P81387                 END-IF
P81387             END-IF    
P81387         END-IF.
354900
           IF  (NOT PRPRD-PYMSET6)
           AND (UPDATING)
               PERFORM 820-STORE-BENEFIT.
356400
356500 520-END.
356600******************************************************************
356700 521-MOVE-COMPHIST-DATA.
356800******************************************************************
356900
357000     MOVE PYM-COMPANY            TO CPH-COMPANY.
357100     MOVE PYM-EMPLOYEE           TO CPH-EMPLOYEE.
357200     MOVE WS-PLN-PLAN-TYPE (I1)  TO CPH-PLAN-TYPE.
357300     MOVE WS-PLN-PLAN-CODE (I1)  TO CPH-PLAN-CODE.
357400     MOVE WS-PLAN-BEG-YEAR       TO CPH-PLAN-YEAR.
J67329     MOVE WS-SYSTEM-DATE-YMD     TO CPH-DATE-STAMP.
J67329     MOVE HHMMSS                 TO CPH-TIME-STAMP.
J67329     MOVE CRT-USER-NAME          TO CPH-USER-ID.     
357500
357600     IF (WS-PLN-COMP-CLASS (I1) NOT = SPACES)
357700         ADD WS-BASE-HOURS (I1)  TO CPH-HOURS-SERV
357800                                    WR-HOURS-SERV (I1)
357900         ADD WS-BASE-AMOUNT (I1) TO CPH-COMP-AMOUNT
358000                                    WR-COMP-AMOUNT (I1)
358100     ELSE
358200         ADD WS-HOURS-SERV       TO CPH-HOURS-SERV
358300                                    WR-HOURS-SERV (I1)
358400         ADD WS-WAGE-AMOUNT      TO CPH-COMP-AMOUNT
358500                                    WR-COMP-AMOUNT (I1).
358600
358700 521-END.
358800
358900******************************************************************
359000 522-MOVE-CODA-YTD.
359100******************************************************************
359200
359300     MOVE PYM-COMPANY            TO COD-COMPANY.
359400     MOVE PYM-EMPLOYEE           TO COD-EMPLOYEE.
           MOVE WS-PLN-PLAN-TYPE (I1)  TO COD-PLAN-TYPE.
359500     MOVE WS-PLN-PLAN-CODE (I1)  TO COD-PLAN-CODE.
359600     MOVE WS-PLAN-BEG-YEAR       TO COD-PLAN-YEAR.

P65715     IF  (EMP-WORK-COUNTRY = "CA")
P65715         MOVE PYM-PROCESS-LEVEL  TO COD-PROCESS-LEVEL
J20354
J20354         MOVE PYM-COMPANY        TO DB-COMPANY
J20354         MOVE PYM-EMPLOYEE       TO DB-EMPLOYEE
J20354         MOVE PYM-CHECK-ID       TO DB-CHECK-ID
J20354         MOVE PYDSET1-CHECK-ID   TO WS-DB-BEG-RNG
J20354         PERFORM 850-FIND-BEGRNG-PYDSET1
J20354         IF (PAYDEDUCTN-FOUND)
J20354             MOVE PYD-BUS-NBR-GRP    TO COD-BUS-NBR-GRP
J20354             MOVE PYD-QC-ENT-NBR-GRP TO COD-QC-ENT-NBR-GRP
J20354         END-IF
P65715     ELSE
P65715         MOVE SPACES             TO COD-PROCESS-LEVEL
J20354     END-IF.

           IF (REPORT-ONLY)
               IF (TIMERECORD-FOUND)
                   MOVE TRD-BUS-NBR-GRP    TO COD-BUS-NBR-GRP
                   MOVE TRD-QC-ENT-NBR-GRP TO COD-QC-ENT-NBR-GRP
               END-IF
           ELSE
               IF (PRTIME-FOUND)
                   MOVE PRT-BUS-NBR-GRP    TO COD-BUS-NBR-GRP
                   MOVE PRT-QC-ENT-NBR-GRP TO COD-QC-ENT-NBR-GRP
               END-IF
           END-IF.
           MOVE PYM-CHECK-DATE         TO COD-LAST-UPD-DATE.
359700
J32076     IF  (WS-PLN-CODA-CONTRIB (I1) = SPACES OR "1" OR "3")
359800         ADD WS-COD-PRE-TAX-YTD  TO COD-AMOUNT-YTD (1)
359900         ADD WS-COD-AFT-TAX-YTD  TO COD-AMOUNT-YTD (2)
J32076     END-IF.
J32076     IF  (WS-PLN-CODA-CONTRIB (I1) = SPACES OR "1" OR "2")
360000         ADD WS-COD-CMP-CONT-YTD TO COD-AMOUNT-YTD (4)
J32076     END-IF.
360100
360200 522-END.
J32076     EXIT.
361900
362000******************************************************************
362100 530-SEARCH-PLN-DED-CODES.
362200******************************************************************
362300
362400     IF (WS-PLN-DED-CODE (I1 I2) = SPACES)
362500         GO TO 530-END.

           IF (I2 = 4 OR 8 OR 9 OR 10 OR 12 OR 13)
               MOVE BEN-CMP-SEQ-NBR  TO WS-BEN-EDM-SEQ-NBR
           ELSE
           IF (I2 = 11 OR 14)
               MOVE BEN-CMP-AFT-SEQ  TO WS-BEN-EDM-SEQ-NBR
           ELSE
           IF (I2 = 1 OR 2 OR 5 OR 6)
               MOVE BEN-PRE-SEQ-NBR  TO WS-BEN-EDM-SEQ-NBR
           ELSE
           IF (I2 = 3 OR 7)
               MOVE BEN-AFT-SEQ-NBR  TO WS-BEN-EDM-SEQ-NBR
P98627     ELSE
           IF (I2 = 15)
               MOVE BEN-FLEX-DED-SEQ TO WS-BEN-EDM-SEQ-NBR.

362700     PERFORM
362800         VARYING I3 FROM 1 BY 1
362900         UNTIL  (I3 > WS-PYD-SIZE)
363000         OR    ((WS-PYD-DED-CODE (I3) = WS-PLN-DED-CODE (I1 I2))
                AND   (WS-PYD-EDM-SEQ-NBR (I3) = WS-BEN-EDM-SEQ-NBR))
363100         OR     (WS-PYD-DED-CODE (I3) = SPACES)
363200
363300         CONTINUE
363400     END-PERFORM.
363500
363600     IF (I3 > WS-PYD-SIZE)
363700         GO TO 530-END.
363800
363900     IF (WS-PYD-DED-CODE (I3) = SPACES)
364000         GO TO 530-END.
364100
364200     IF (WS-PLN-PLAN-TYPE (I1)   = "DB" OR "DC")
364300         PERFORM 550-ADD-YTD
364400         THRU    550-END
364500     ELSE
364600     IF (WS-PLN-PLAN-TYPE (I1)   = "RS" OR "SB" OR "SP")
364700         PERFORM 560-UPDATE-BENEFIT
364800         THRU    560-END.
364900
365000 530-END.
365100******************************************************************
365200 550-ADD-YTD.
365300******************************************************************
365400
J72349     MOVE PRM-COMPANY          TO DB-COMPANY.
J72349     MOVE WS-PYD-DED-CODE (I3) TO DB-DED-CODE.   
J72349     PERFORM 840-FIND-DDCSET1.
J72349     IF (DEDCODE-FOUND)
J72349         IF (DDC-TAX-STATUS = "01")
J72349            MOVE "Y" TO WS-EE-401K-PLAN-FOUND
J72349            MOVE "Y" TO WR-401K-IN-PLAN (I1)
J72349            MOVE WS-PLAN-BEG-YEAR TO WR-PLAN-YEAR (I1)
J72349         END-IF
J72349         IF  (DDC-TAX-STATUS = "29" OR "30")
J72349         AND (DDC-ADJUST-PAY = "S")
J72349            ADD WS-PYD-DED-AMT (I3) TO WR-409A-DED-AMT (I1)
J72349            ADD WS-PYD-DED-AMT (I3) TO WS-EE-409A-AMOUNT   
J72349         END-IF
J72349     END-IF.
J72349
365500     IF (I2  = 4 OR 8 OR 9 OR 10 OR 11 OR 12 OR 13 OR 14 OR 15)
365600         ADD WS-PYD-DED-AMT (I3) TO WS-COD-CMP-CONT-YTD
365700                                    WR-CMP-DED-AMT (I1)
365800     ELSE
365900     IF (I2 = 1 OR 5)
366000         ADD WS-PYD-DED-AMT (I3) TO WS-COD-PRE-TAX-YTD
366100                                    WR-PRE-DED-AMT (I1)
366200     ELSE
366300     IF (I2 = 2 OR 3 OR 6 OR 7)
366400         ADD WS-PYD-DED-AMT (I3) TO WS-COD-AFT-TAX-YTD
366500                                    WR-AFT-DED-AMT (I1).
366600
366700 550-END.
366800******************************************************************
366900 560-UPDATE-BENEFIT.
367000******************************************************************
367100
367200     IF  (WS-PLN-PLAN-TYPE (I1) = "RS")
367300     AND (WS-PYD-DED-CODE (I3)  = WS-PLN-DED-CODE (I1 15))
367400         ADD WS-PYD-DED-AMT (I3) TO BEN-YTD-FLEX-CONT.
367500
367600     ADD WS-PYD-DED-AMT (I3)     TO BEN-YTD-CONT
367700                                    WR-DED-AMT (I1).

P63803     IF  (WS-DDC-ADJUST-PAY (I3) = "S")
P63803         ADD WS-PYD-DED-AMT (I3) TO BEN-EMP-YTD-CONT.
367800
367900 560-END.
368000******************************************************************
368100 500-END.
368200******************************************************************
J99021****************************************************************
J99021 570-UPDATE-GHR-BEN-INFO         SECTION.
J99021****************************************************************
J99021 570-START.
J99021
J99021     INITIALIZE WS-PLN-TBL-2    
J99021                WS-BENWORK-TABLE
J99021                WS-BENWORK-TABLE-2
J99021                WS-EE-401K-PLAN-FOUND
J99021                WS-EE-409A-AMOUNT
J99021                WS-HOURS-SERV
J99021                WS-WAGE-AMOUNT.
J99021
J99021     IF (WS-PRT-SIZE > 0)
J99021         PERFORM 575-ADD-PRTIME
J99021         THRU    575-END
J99021             VARYING I3 FROM 1 BY 1
J99021             UNTIL  (I3 > WS-PRT-SIZE).
J99021
J99021     PERFORM
J99021         VARYING I1 FROM 1 BY 1
J99021         UNTIL  (I1 > WS-PLN-TBL-SIZE)
J99021         IF (WS-BENEFIT-DONE (I1) NOT = "Y")
J99021             PERFORM 576-READ-PLAN-TBL
J99021             THRU    576-END
J99021         END-IF
J99021     END-PERFORM.
J99921
J99021     MOVE PYM-CHECK-DATE TO WS-PLAN-BEG-DATE.
J99021
J99021     PERFORM
J99021         VARYING I1 FROM 1 BY 1
J99021         UNTIL  (I1 > WS-PLN-TBL-SIZE)
J99021          IF ((WS-PLN-COMP-CLASS (I1) = SPACES)
J99021          OR  (WS-BASE-HOURS (I1)     NOT = ZEROS)
J99021          OR  (WS-BASE-AMOUNT (I1)    NOT = ZEROS))
J99021          AND (WS-BENEFIT-DONE (I1)   = "Y")
J99021             MOVE PYM-COMPANY           TO DB-COMPANY
J99021             MOVE WS-PLN-PLAN-TYPE (I1) TO DB-PLAN-TYPE
J99021             MOVE WS-PLN-PLAN-CODE (I1) TO DB-PLAN-CODE
J99021             MOVE PYM-EMPLOYEE          TO DB-EMPLOYEE
J99021             MOVE WS-PLAN-BEG-YEAR      TO DB-PLAN-YEAR
J99021             IF (PRPRD-PYMSET6)
J99021             OR (REPORT-ONLY)
J99021                 PERFORM 840-FIND-CPHSET1
J99021             ELSE
J99021                 PERFORM 840-MODIFY-CPHSET1
J99021             END-IF
J99021             IF (COMPHIST-FOUND)
J99021                PERFORM 580-MOVE-COMPHIST-DATA
J99021                THRU    580-END
J99021                IF  (NOT PRPRD-PYMSET6)
J99021                AND (UPDATING)
J99021                    PERFORM 820-STORE-COMPHIST
J99021                END-IF
J99021             ELSE
J99021                IF  (NOT PRPRD-PYMSET6)
J99021                AND (UPDATING)                  
J99021                    PERFORM 800-CREATE-COMPHIST
J99021                    MOVE WS-SYSTEM-DATE-YMD  
J99021                                         TO CPH-CREATE-DATE
J99021                    MOVE HHMMSS          TO CPH-CREATE-TIME
J99021                    MOVE CRT-USER-NAME   TO CPH-CREATE-USER-ID    
J99021                ELSE
J99021                    INITIALIZE COMPHIST
J99021                END-IF
J99021                PERFORM 580-MOVE-COMPHIST-DATA
J99021                THRU    580-END
J99021                IF  (NOT PRPRD-PYMSET6)
J99021                AND (UPDATING)
J99021                    PERFORM 820-STORE-COMPHIST
J99021                END-IF 
J99021             END-IF
J99021         END-IF
J99021     END-PERFORM.
J99021
J99021     GO TO 570-END.
J99021
J99021******************************************************************
J99021 575-ADD-PRTIME.
J99021******************************************************************
J99021
J99021     IF (WS-PRT-OT-RECORD (I3) NOT = "Y")
J99021         ADD WS-PRT-HOURS (I3)   TO WS-HOURS-SERV
J99021                                    WS-VEST-HOURS.
J99021     ADD WS-PRT-WAGE-AMOUNT (I3) TO WS-WAGE-AMOUNT.
J99021
J99021     PERFORM
J99021         VARYING I1 FROM 1 BY 1
J99021         UNTIL  (I1 > WS-PLN-DB-DC-SIZE)
J99021
J99021         IF (WS-PLN-COMP-CLASS (I1) NOT = SPACES)
J99021             MOVE PRM-COMPANY             TO DB-COMPANY
J99021             MOVE WS-PLN-COMP-CLASS (I1)  TO DB-PAY-CLASS
J99021             MOVE WS-PRT-PAY-SUM-GRP (I3) TO DB-PAY-SUM-GRP
J99021             PERFORM 840-FIND-PSRSET1
J99021             IF (PSGRELATE-FOUND)
J99021                 IF (PSR-HOURS-FLAG = "S")
J99021                     SUBTRACT WS-PRT-HOURS (I3)
J99021                                          FROM WS-BASE-HOURS (I1)
J99021                 ELSE
J99021                 IF (PSR-HOURS-FLAG NOT = "E")
J99021                     ADD WS-PRT-HOURS (I3) TO WS-BASE-HOURS (I1)
J99021                 END-IF
J99021                 END-IF
J99021                 IF (PSR-WAGES-FLAG = "S")
J99021                     SUBTRACT WS-PRT-WAGE-AMOUNT (I3)
J99021                                         FROM WS-BASE-AMOUNT (I1)
J99021                 ELSE
J99021                 IF (PSR-WAGES-FLAG NOT = "E")
J99021                     ADD WS-PRT-WAGE-AMOUNT (I3)
J99021                                         TO WS-BASE-AMOUNT (I1)
J99021                 END-IF
J99021                 END-IF
J99021             END-IF
J99021         END-IF
J99021     END-PERFORM.
J99021
J99021 575-END.
J99021
J99021******************************************************************
J99021 576-READ-PLAN-TBL.
J99021******************************************************************
J99021
J99021     IF (WS-PLN-PLAN-TYPE (I1)    = "DB" OR "DC")
J99021*        MOVE WS-PLN-PLAN-TYPE (I1)  TO DB-PLAN-TYPE
J99021*        MOVE WS-PLN-PLAN-CODE (I1)  TO DB-PLAN-CODE
J99021*        MOVE PYM-EMPLOYEE           TO DB-EMPLOYEE
J99021*        MOVE WS-PLAN-BEG-YEAR       TO DB-PLAN-YEAR
J99021         PERFORM 577-SEARCH-PLN-DED-CODES
J99021         THRU    577-END
J99021             VARYING I2 FROM 1 BY 1
J99021             UNTIL  (I2 > 15)
J99021     END-IF.
J99021
J99021 576-END.
J99021
J99021******************************************************************
J99021 577-SEARCH-PLN-DED-CODES.
J99021******************************************************************
J99021
J99021     IF (WS-PLN-DED-CODE (I1 I2) = SPACES)
J99021         GO TO 577-END.
J99021
J99021     PERFORM
J99021         VARYING I3 FROM 1 BY 1
J99021         UNTIL  (I3 > WS-PYD-SIZE)
J99021         OR     (WS-PYD-DED-CODE (I3) = WS-PLN-DED-CODE (I1 I2))
J99021         OR     (WS-PYD-DED-CODE (I3) = SPACES)
J99021           CONTINUE
J99021     END-PERFORM.
J99021
J99021     IF (I3 > WS-PYD-SIZE)
J99021         GO TO 577-END.
J99021
J99021     IF (WS-PYD-DED-CODE (I3) = SPACES)
J99021         GO TO 577-END.
J99021
J99021     IF (WS-PYD-DED-CODE (I3) = WS-PLN-DED-CODE (I1 I2))
J99021         MOVE "Y" TO WS-BENEFIT-DONE (I1).
J99021
J99021     IF (WS-PLN-PLAN-TYPE (I1)   = "DB" OR "DC")
J99021         PERFORM 578-ADD-YTD
J99021         THRU    578-END
J99021     END-IF.
J99021
J99021 577-END.
J99021******************************************************************
J99021 578-ADD-YTD.
J99021******************************************************************
J99021
J99021     MOVE PRM-COMPANY          TO DB-COMPANY.
J99021     MOVE WS-PYD-DED-CODE (I3) TO DB-DED-CODE.   
J99021     PERFORM 840-FIND-DDCSET1.
J99021     IF (DEDCODE-FOUND)
J99021         IF (DDC-TAX-STATUS = "01")
J99021            MOVE "Y" TO WS-EE-401K-PLAN-FOUND
J99021            MOVE "Y" TO WR-401K-IN-PLAN (I1)
J99021            MOVE WS-PLAN-BEG-YEAR TO WR-PLAN-YEAR (I1)
J99021         END-IF
J99021         IF  (DDC-TAX-STATUS = "29" OR "30")
J99021         AND (DDC-ADJUST-PAY = "S")
J99021            ADD WS-PYD-DED-AMT (I3) TO WR-409A-DED-AMT (I1)
J99021            ADD WS-PYD-DED-AMT (I3) TO WS-EE-409A-AMOUNT   
J99021         END-IF
J99021     END-IF.
J99021
J87945     IF (I2  = 4 OR 8 OR 9 OR 10 OR 11 OR 12 OR 13 OR 14)       
J87945         ADD WS-PYD-DED-AMT (I3) TO WR-CMP-DED-AMT (I1)
J87945     ELSE
J99021     IF (I2 = 1 OR 5)
J99021         ADD WS-PYD-DED-AMT (I3) TO WR-PRE-DED-AMT (I1)
J99021     ELSE
J99021     IF (I2 = 2 OR 3 OR 6 OR 7)
J99021         ADD WS-PYD-DED-AMT (I3) TO WR-AFT-DED-AMT (I1). 
J99021
J99021 578-END.
J99021
J99021******************************************************************
J99021 580-MOVE-COMPHIST-DATA.
J99021******************************************************************
J99021
J99021     MOVE PYM-COMPANY            TO CPH-COMPANY.
J99021     MOVE PYM-EMPLOYEE           TO CPH-EMPLOYEE.
J99021     MOVE WS-PLN-PLAN-TYPE (I1)  TO CPH-PLAN-TYPE.
J99021     MOVE WS-PLN-PLAN-CODE (I1)  TO CPH-PLAN-CODE.
J99021     MOVE WS-PLAN-BEG-YEAR       TO CPH-PLAN-YEAR.
J99021     MOVE WS-SYSTEM-DATE-YMD     TO CPH-DATE-STAMP.
J99021     MOVE HHMMSS                 TO CPH-TIME-STAMP.
J99021     MOVE CRT-USER-NAME          TO CPH-USER-ID.     
J99021
J99021     IF (WS-PLN-COMP-CLASS (I1) NOT = SPACES)
J99021         ADD WS-BASE-HOURS (I1)  TO CPH-HOURS-SERV
J99021                                    WR-HOURS-SERV  (I1)
J99021         ADD WS-BASE-AMOUNT (I1) TO CPH-COMP-AMOUNT
J99021                                    WR-COMP-AMOUNT (I1)
J99021     ELSE
J99021         ADD WS-HOURS-SERV       TO CPH-HOURS-SERV
J99021                                    WR-HOURS-SERV (I1)
J99021         ADD WS-WAGE-AMOUNT      TO CPH-COMP-AMOUNT
J99021                                    WR-COMP-AMOUNT (I1).
J99021
J99021 580-END.
J99021
J99021******************************************************************
J99021 570-END.
J99021******************************************************************
368300******************************************************************
368400 600-PROCESS-BN-REPORT           SECTION.
368500******************************************************************
368600 600-START.
368700
368800     MOVE PRM-COMPANY            TO G1-COMPANY.
368900     MOVE WS-CO-NAME             TO G1-PRS-NAME.
369000

369100     INITIALIZE BWK-REC-KEY.
369200     PERFORM 8500-FIND-NLT-BENWORK.
369300
369400     PERFORM 610-PRINT-PROCESS-LEVELS
369500     THRU    610-END
369600         UNTIL (BENWORK-NOTFOUND).
369700

369800     PERFORM 9900-CLOSE-BENWORK.
370100
370200     GO TO 600-END.
370300
370400******************************************************************
370500 610-PRINT-PROCESS-LEVELS.
370600******************************************************************
370700
370800     MOVE BWK-PROCESS-LEVEL      TO WS-PROCESS-LEVEL.
370900
PRBRD      IF (PRBRD-BROADCAST-ENABLED)
PRBRD          MOVE 501                    TO PRBRD-ELEMENT-TYPE
PRBRD          MOVE WS-PROCESS-LEVEL       TO PRBRD-ELEMENT-VALUE
PRBRD          PERFORM 610-BEG-ELEMENT-TAG
PRBRD          MOVE WFCHWS-TAG-STRING      TO BTL1-TAG-LINE
PRBRD          MOVE BROADCAST-TAG-LINE-1   TO RPT-GROUP-REQUEST
PRBRD          PERFORM 700-PRINT-RPT-GRP.

371000     PERFORM 611-PRINT-PLAN-TYPES
371100     THRU    611-END
371200         UNTIL (BENWORK-NOTFOUND)
371300         OR    (BWK-PROCESS-LEVEL NOT = WS-PROCESS-LEVEL).
371400
PRBRD      IF (PRBRD-BROADCAST-ENABLED)
PRBRD          MOVE 501                    TO PRBRD-ELEMENT-TYPE
PRBRD          MOVE WS-PROCESS-LEVEL       TO PRBRD-ELEMENT-VALUE
PRBRD          PERFORM 615-END-ELEMENT-TAG
PRBRD          MOVE WFCHWS-TAG-STRING      TO BTL1-TAG-LINE
PRBRD          MOVE BROADCAST-TAG-LINE-1   TO RPT-GROUP-REQUEST
PRBRD          PERFORM 700-PRINT-RPT-GRP.

371500 610-END.
371600
371700******************************************************************
371800 611-PRINT-PLAN-TYPES.
371900******************************************************************
372000
372100     MOVE BWK-PLAN-TYPE          TO WS-PLAN-TYPE.
372200
372300     MOVE ZEROS                  TO WS-TOT-PRE-DED-AMT
372400                                    WS-TOT-AFT-DED-AMT
372500                                    WS-TOT-CMP-DED-AMT
372600                                    WS-TOT-COMP-AMOUNT
372700                                    WS-TOT-HOURS-SERV
372800                                    WS-TOT-VEST-HOURS
372900                                    WS-TOT-DED-AMT.
373000
373100     PERFORM 614-PRT-COMPANY-GRP
373200     THRU    614-END.
373300
373400     PERFORM 612-PRINT-PLAN-CODES
373500     THRU    612-END
373600         UNTIL (BENWORK-NOTFOUND)
373700         OR    (BWK-PROCESS-LEVEL NOT = WS-PROCESS-LEVEL)
373800         OR    (BWK-PLAN-TYPE     NOT = WS-PLAN-TYPE).
373900
374000     IF (WS-PLAN-TYPE = "DB" OR "DC")
374100         MOVE SPACES             TO G41-PLAN-CODE
374300         MOVE 118                TO CRT-MSG-NBR
374400         PERFORM 790-GET-MSG
374500         MOVE CRT-MESSAGE        TO G41-PLAN-DESC
P86989*        IF (WS-PLAN-TYPE = "DC")
                   MOVE WS-TOT-PRE-DED-AMT TO G41-PRE-DED-AMT
                   MOVE WS-TOT-AFT-DED-AMT TO G41-AFT-DED-AMT
                   MOVE WS-TOT-CMP-DED-AMT TO G41-CMP-DED-AMT
P86989*        ELSE
P86989*            MOVE ZEROES             TO G41-PRE-DED-AMT
P86989*                                       G41-AFT-DED-AMT
P86989*                                       G41-CMP-DED-AMT
P86989*        END-IF
375000         MOVE WS-TOT-COMP-AMOUNT TO G41-COMP-AMOUNT
375200         MOVE WS-TOT-HOURS-SERV  TO G41-HOURS-SERV
375400         MOVE WS-TOT-VEST-HOURS  TO G41-VEST-HOURS
375700         MOVE GN41D-PLAN-CODE    TO RPT-GROUP-REQUEST
375800         PERFORM 700-PRINT-RPT-GRP
376300     ELSE
376400         MOVE SPACES             TO G42-PLAN-CODE
376500         MOVE 118                TO CRT-MSG-NBR
376600         PERFORM 790-GET-MSG
376700         MOVE CRT-MESSAGE        TO G42-PLAN-DESC
376800         MOVE WS-TOT-DED-AMT     TO G42-DED-AMT
376900         MOVE GN42D-PLAN-CODE    TO RPT-GROUP-REQUEST
377000         PERFORM 700-PRINT-RPT-GRP.
377100
377200 611-END.
377300
377400******************************************************************
377500 612-PRINT-PLAN-CODES.
377600******************************************************************
377700
377800     MOVE BWK-PLAN-CODE          TO WS-PLAN-CODE.
377900
378000     PERFORM 615-PRT-PLAN-CODE-GRP
378100     THRU    615-END.
378200
378300     IF (BWK-PLAN-TYPE = "DB" OR "DC")
378400         PERFORM 618-PRT-REPORT1
378500         THRU    618-END
378600     ELSE
378700         PERFORM 620-PRT-REPORT2
378800         THRU    620-END.
378900
379000 612-END.
379100
379200******************************************************************
379300 614-PRT-COMPANY-GRP.
379400******************************************************************
379500
           MOVE PRM-COMPANY            TO DB-COMPANY.
379600     MOVE BWK-PROCESS-LEVEL      TO G2-PROCESS-LEVEL
379700                                    DB-PROCESS-LEVEL.
379800
379900     PERFORM 840-FIND-PRSSET1.
380000     MOVE PRS-NAME               TO G2-PROC-LEV-NAME.
           MOVE BWK-PLAN-TYPE          TO G41-PLAN-TYPE
                                          G42-PLAN-TYPE.
380200
380300     MOVE GN1-COMPANY            TO RPT-GROUP-REQUEST.
380400     PERFORM 700-PRINT-RPT-GRP.
380500     MOVE GN2-PLAN-TYPE          TO RPT-GROUP-REQUEST.
380600     PERFORM 700-PRINT-RPT-GRP.
380700
380800     IF (BWK-PLAN-TYPE = "DB" OR "DC")
380900         MOVE GN41H-PLAN-CODE    TO RPT-GROUP-REQUEST
381000         PERFORM 700-PRINT-RPT-GRP
381100     ELSE
381600         MOVE GN42H-PLAN-CODE    TO RPT-GROUP-REQUEST
381700         PERFORM 700-PRINT-RPT-GRP.
381800
381900 614-END.
382000
382100******************************************************************
382200 615-PRT-PLAN-CODE-GRP.
382300******************************************************************
382400
382500     MOVE BWK-PLAN-TYPE          TO DB-PLAN-TYPE.
382600     MOVE BWK-PLAN-CODE          TO G41-PLAN-CODE
382700                                    G42-PLAN-CODE
382900                                    DB-PLAN-CODE.
383000     PERFORM 840-FIND-PLNSET1.
383100     MOVE PLN-DESC               TO G41-PLAN-DESC
383200                                    G42-PLAN-DESC.
383400
383500 615-END.
383600
383700******************************************************************
383800 618-PRT-REPORT1.
383900******************************************************************
384000     MOVE ZEROS                  TO WS-PRE-DED-AMT
384100                                    WS-AFT-DED-AMT
384200                                    WS-CMP-DED-AMT
384300                                    WS-COMP-AMOUNT
384400                                    WS-HOURS-SERV
384500                                    WS-VEST-HOURS.
384600
J72349     MOVE BWK-401K-PLAN-FLAG     TO WS-401K-PLAN-FLAG.
J72349
384700     PERFORM 619-ADD-AMOUNTS
384800     THRU    619-END
384900         UNTIL (BENWORK-NOTFOUND)
385000         OR    (BWK-PROCESS-LEVEL NOT = WS-PROCESS-LEVEL)
385100         OR    (BWK-PLAN-TYPE     NOT = WS-PLAN-TYPE)
385200         OR    (BWK-PLAN-CODE     NOT = WS-PLAN-CODE).
385300
           IF (WS-PLAN-TYPE = "DC")
P43079     OR (WS-PLAN-TYPE = "DB")
               MOVE WS-PRE-DED-AMT     TO G41-PRE-DED-AMT
               MOVE WS-AFT-DED-AMT     TO G41-AFT-DED-AMT
               MOVE WS-CMP-DED-AMT     TO G41-CMP-DED-AMT.
J72349
J72349     IF  (WS-PLAN-TYPE = "DC" OR "DB")
J72349     AND (WS-401K-PLAN-FLAG = "Y")
J72349     AND (PRM-UPDATE-OPTION = "2" OR "4")
J72349          PERFORM 619-CHK-409A-TABLE
J72349          THRU    619-CHK-END.

385700     MOVE WS-COMP-AMOUNT         TO G41-COMP-AMOUNT.
385900     MOVE WS-HOURS-SERV          TO G41-HOURS-SERV.
386100     MOVE WS-VEST-HOURS          TO G41-VEST-HOURS.
386300
386500     MOVE GN41D-PLAN-CODE        TO RPT-GROUP-REQUEST.
386600     PERFORM 700-PRINT-RPT-GRP.
387000
387100 618-END.
387200
387300******************************************************************
387400 619-ADD-AMOUNTS.
387500******************************************************************
387600     ADD BWK-PRE-DED-AMT         TO WS-PRE-DED-AMT
387700                                    WS-TOT-PRE-DED-AMT.
387800     ADD BWK-AFT-DED-AMT         TO WS-AFT-DED-AMT
387900                                    WS-TOT-AFT-DED-AMT.
388000     ADD BWK-CMP-DED-AMT         TO WS-CMP-DED-AMT
388100                                    WS-TOT-CMP-DED-AMT.
388200     ADD BWK-COMP-AMOUNT         TO WS-COMP-AMOUNT
388300                                    WS-TOT-COMP-AMOUNT.
388400     ADD BWK-HOURS-SERV          TO WS-HOURS-SERV
388500                                    WS-TOT-HOURS-SERV.
388600     ADD BWK-VEST-HOURS          TO WS-VEST-HOURS
388700                                    WS-TOT-VEST-HOURS.
388800
388900     PERFORM 8600-FIND-NEXT-BENWORK.
389000
389100 619-END.
389200
J72349******************************************************************
J72349 619-CHK-409A-TABLE.
J72349******************************************************************
J72349
J72349     PERFORM
J72349         VARYING I9 FROM 1 BY 1
J72349         UNTIL  (I9 > WS-409A-TABLE-SIZE)           
J72349         OR    ((WS-409A-PROC-LEVEL (I9) = WS-PROCESS-LEVEL)     
J72349          AND   (WS-409A-PLAN-TYPE  (I9) = WS-PLAN-TYPE))      
J72349         OR     (WS-409A-PROC-LEVEL (I9) = SPACES)
J72349             CONTINUE
J72349     END-PERFORM.
J72349
J72349     IF (I9 > WS-409A-TABLE-SIZE)           
J72349     OR (WS-409A-PROC-LEVEL (I9) = SPACES)
J72349         GO TO 619-CHK-END.
J72349
J72349     IF  (WS-409A-PROC-LEVEL (I9) = WS-PROCESS-LEVEL)     
J72349     AND (WS-409A-PLAN-TYPE  (I9) = WS-PLAN-TYPE)      
J72349         SUBTRACT WS-409A-AMOUNT (I9) FROM WS-COMP-AMOUNT
J72349                                           WS-TOT-COMP-AMOUNT.
J72349
J72349 619-CHK-END.
J72349
389300******************************************************************
389400 620-PRT-REPORT2.
389500******************************************************************
389600     MOVE ZEROS                  TO WS-DED-AMT.
389700
389800     PERFORM
389900         UNTIL (BENWORK-NOTFOUND)
390000         OR    (BWK-PROCESS-LEVEL NOT = WS-PROCESS-LEVEL)
390100         OR    (BWK-PLAN-TYPE     NOT = WS-PLAN-TYPE)
390200         OR    (BWK-PLAN-CODE     NOT = WS-PLAN-CODE)
390300
390400         ADD BWK-DED-AMT         TO WS-DED-AMT
390500                                    WS-TOT-DED-AMT
390600         PERFORM 8600-FIND-NEXT-BENWORK
390700     END-PERFORM.
390800
390900     MOVE WS-DED-AMT             TO G42-DED-AMT.
391000
391100     MOVE GN42D-PLAN-CODE        TO RPT-GROUP-REQUEST.
391200     PERFORM 700-PRINT-RPT-GRP.
391300
391400 620-END.
391500
391600******************************************************************
391700 600-END.
391800******************************************************************

      ****************************************************************
       1000-BUILD-EMP-OTD-TABLE         SECTION.
      ****************************************************************
       1000-START.

           MOVE 1 TO I5.
           INITIALIZE WS-EMP-OTD-TABLE.

P62055     MOVE 1                      TO WS-PR197-FILTER-LENGTH.
P62055     MOVE SPACES                 TO FILTER-STRING.
P62055     CALL "USEEARLYBINDFILTERS" USING WS-TRUE.
P62055
P62055     STRING "(OTD-STATUS = ?)"
P62055         DELIMITED BY SIZE INTO FILTER-STRING
P62055         POINTER WS-PR197-FILTER-LENGTH.
P62055     MOVE 4                      TO NUMERIC-FILTER-VALUE.
P62055     PERFORM 890-SET-NUMERIC-FILTER-VALUE.
P62055
P62055     IF  (MID-CYCLE)
P62055         STRING " AND ((OTD-RECORD-TYPE = ?) OR "
P62055                " (OTD-RECORD-TYPE = ?) OR (OTD-RECORD-TYPE = ?)"
247726                " OR (OTD-RECORD-TYPE = ?))"
P62055             DELIMITED BY SIZE INTO FILTER-STRING
P62055             POINTER WS-PR197-FILTER-LENGTH
P62055         MOVE "M"                TO ALPHANUM-FILTER-VALUE
P62055         PERFORM 890-SET-ALPHANUM-FILTER-VALUE
P62055         MOVE "X"                TO ALPHANUM-FILTER-VALUE
P62055         PERFORM 890-SET-ALPHANUM-FILTER-VALUE
P62055         MOVE "Z"                TO ALPHANUM-FILTER-VALUE
P62055         PERFORM 890-SET-ALPHANUM-FILTER-VALUE
247726         MOVE "S"                TO ALPHANUM-FILTER-VALUE
247726         PERFORM 890-SET-ALPHANUM-FILTER-VALUE
247726     END-IF.
P62055
P62055     PERFORM 890-CREATE-FILTER.

           MOVE OTDSET1-EMPLOYEE       TO WS-DB-BEG-RNG.
           PERFORM 850-FILTER-BEGRNG-OTDSET1.
           PERFORM 1010-LOAD-EMP-OTD-TABLE
           THRU    1010-END
               UNTIL (ONETMDED-NOTFOUND)
               OR    (I5 > WS-EMP-OTD-TABLE-SIZE).

           IF  (I5 > WS-EMP-OTD-TABLE-SIZE)
           AND (ONETMDED-FOUND)
               MOVE 155                   TO CRT-ERROR-NBR
               MOVE OTD-EMPLOYEE          TO CRT-ERR-VAR1
               PERFORM 780-PRINT-ERROR-MSG
               MOVE 156                   TO CRT-ERROR-NBR
               MOVE WS-EMP-OTD-TABLE-SIZE TO CRT-ERR-VAR1
               PERFORM 780-PRINT-ERROR-MSG
               MOVE SPACES TO WS-OTD-DED-CODE (I5).

           GO TO 1000-END.

      ****************************************************************
       1010-LOAD-EMP-OTD-TABLE.
      ****************************************************************

P62055*    IF  (OTD-STATUS             NOT = 4)
P62055     IF  (OTD-PROCESS-LEVEL      NOT = PRM-PROCESS-LEVEL)
           AND (PRM-PROCESS-LEVEL      NOT = SPACES)
               GO TO 1010-NEXT-ONETMDED.

           IF (PRM-PROC-GROUP NOT = SPACES)
               MOVE OTD-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
               PERFORM 840-KFIND-PRPSET1
               IF (PRPROCGRP-KNOTFOUND)
                   GO TO 1010-NEXT-ONETMDED.

           MOVE OTD-DED-CODE           TO WS-OTD-DED-CODE (I5).
           MOVE OTD-EDM-SEQ-NBR        TO WS-OTD-EDM-SEQ-NBR (I5).
           MOVE OTD-SEQ-NBR            TO WS-OTD-SEQ-NBR (I5).
           MOVE OTD-PROCESS-LEVEL      TO WS-OTD-PROCESS-LEVEL (I5).
           MOVE OTD-DED-AMT            TO WS-OTD-DED-AMT (I5).
           MOVE OTD-EFFECT-DATE        TO WS-OTD-EFFECT-DATE (I5).
           MOVE OTD-RECORD-TYPE        TO WS-OTD-RECORD-TYPE (I5).
           MOVE OTD-BEN-PLAN-TYPE      TO WS-OTD-BEN-PLAN-TYPE (I5).
           MOVE OTD-BEN-PLAN-CODE      TO WS-OTD-BEN-PLAN-CODE (I5).
           MOVE OTD-BEN-START-DATE     TO WS-OTD-BEN-START-DATE (I5).

           ADD 1 TO I5.

       1010-NEXT-ONETMDED.

           PERFORM 860-FIND-NXTRNG-OTDSET1.

       1010-END.

      ****************************************************************
       1000-END.
      ****************************************************************
J11414****************************************************************
J11414 425-UPDATE-IPR-FILES-STATUS            SECTION.
J11414****************************************************************
J11414 425-START.
J11414**** LOGIC TO CLOSE STATUS OF  C6EMPINV,C6HOUSERNT AND C6PREV****
J11414**** STARTS HERE.                                            ****
J11414     PERFORM 430-UPDATE-C6EMPINV-STATUS
J11414        THRU 430-C6EMPINV-END.
J11414     PERFORM 440-UPDATE-C6HOUSERNT-STATUS
J11414        THRU 440-C6HOUSERNT-END.
J11414     PERFORM 450-UPDATE-C6PREVEMP-STATUS
J11414        THRU 450-C6PREVEMP-END.
J11414     PERFORM 460-UPDATE-C6EXEMPT-STATUS
J11414        THRU 460-C6EXEMPT-END.
J11414     PERFORM 470-UPDATE-INEMP-DED-TAX-CAT
J11414        THRU 470-INEMP-END.
J11414****************************************************************
J11414 425-END.
J11414****************************************************************
J11414****************************************************************
J11414 430-UPDATE-C6EMPINV-STATUS.
J11414****************************************************************
J11414     MOVE WS-C6D-COMPANY         TO DB-COMPANY.
J11414     MOVE WS-C6D-PAYROLL-YEAR    TO DB-PAYROLL-YEAR.
J11414     MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
J11414     MOVE WS-HIGH-VALUES         TO DB-PRD-START-DATE.
J11414     INITIALIZE                     DB-INV-CODE 
J11414                                    DB-INV-GROUP.
J11414     PERFORM  850-MODIFY-NLT-CEISET1.
J11414     PERFORM
J11414         UNTIL (C6EMPINV-NOTFOUND)  
J11414            OR (CEI-EMPLOYEE      NOT = EMP-EMPLOYEE)
J11414            OR (CEI-COMPANY       NOT = WS-C6D-COMPANY)
J11414            OR (CEI-PAYROLL-YEAR  NOT = WS-C6D-PAYROLL-YEAR)
J11414               IF  (CEI-PROC-STATUS = "P")
J11414                   IF  (CEI-PRD-START-DATE  >= WS-C6D-START-DATE)
J11414                   AND (CEI-PRD-END-DATE    <= WS-C6D-END-DATE)
J11414                        SET WS-CEI-PROC-STATUS-CLOSED
J11414                                 TO TRUE
J11414                        MOVE WS-CEI-PROC-STATUS 
J11414                                 TO CEI-PROC-STATUS
J11414                        PERFORM 820-STORE-C6EMPINV
J11414                   END-IF
J11414               END-IF 
J11414               PERFORM 860-MODIFY-NEXT-CEISET1
J11414     END-PERFORM.
J11414****************************************************************
J11414 430-C6EMPINV-END.
J11414****************************************************************

J11414****************************************************************
J11414 440-UPDATE-C6HOUSERNT-STATUS.
J11414****************************************************************
J11414     MOVE WS-C6D-COMPANY         TO DB-COMPANY.
J11414     MOVE WS-C6D-PAYROLL-YEAR    TO DB-PAYROLL-YEAR.
J11414     MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
J11414     MOVE WS-HIGH-VALUES         TO DB-PRD-START-DATE.
J11414     PERFORM  850-MODIFY-NLT-C6HSET1.
J11414     PERFORM
J11414         UNTIL (C6HOUSERNT-NOTFOUND)  
J11414            OR (C6H-EMPLOYEE      NOT = EMP-EMPLOYEE)   
J11414            OR (C6H-COMPANY       NOT = WS-C6D-COMPANY)
J11414            OR (C6H-PAYROLL-YEAR  NOT = WS-C6D-PAYROLL-YEAR)
J11414               IF  (C6H-PROC-STATUS = "P")
J11414                   IF  (C6H-PRD-START-DATE   >= WS-C6D-START-DATE)
J11414                   AND (C6H-PRD-END-DATE     <= WS-C6D-END-DATE)
J11414                       SET WS-C6H-PROC-STATUS-CLOSED
J11414                                 TO TRUE
J11414                       MOVE WS-C6H-PROC-STATUS 
J11414                                 TO C6H-PROC-STATUS
J11414                       PERFORM 820-STORE-C6HOUSERNT
J11414                   END-IF
J11414               END-IF
J11414               PERFORM 860-MODIFY-NEXT-C6HSET1
J11414     END-PERFORM.
J11414****************************************************************
J11414 440-C6HOUSERNT-END.            
J11414****************************************************************
J11414****************************************************************
J11414 450-UPDATE-C6PREVEMP-STATUS.
J11414****************************************************************
J11414     MOVE WS-C6D-COMPANY         TO DB-COMPANY.
J11414     MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
J11414     MOVE WS-C6D-PAYROLL-YEAR    TO DB-PAYROLL-YEAR.
J11414     INITIALIZE                     DB-PREV-TYPE
J11414                                    DB-PREV-CODE.
J11414     PERFORM  850-MODIFY-NLT-C6ESET1.
J11414     PERFORM
J11414         UNTIL  (C6PREVEMP-NOTFOUND) 
J11414            OR  (C6E-EMPLOYEE        NOT = EMP-EMPLOYEE)
J11414            OR  (C6E-PAYROLL-YEAR    NOT =  WS-C6D-PAYROLL-YEAR)
J11414            OR  (C6E-COMPANY         NOT = WS-C6D-COMPANY)
J11414                IF (C6E-PROC-STATUS = "P")
J11414                   SET WS-C6E-PROC-STATUS-CLOSED 
J11414                                 TO TRUE
J11414                   MOVE WS-C6E-PROC-STATUS         
J11414                                 TO C6E-PROC-STATUS
J11414                   PERFORM 820-STORE-C6PREVEMP
J11414                END-IF
J11414                PERFORM 860-MODIFY-NEXT-C6ESET1
J11414     END-PERFORM.
J11414****************************************************************
J11414 450-C6PREVEMP-END.                                 
J11414**************************************************************** 
J11414****************************************************************
J11414 460-UPDATE-C6EXEMPT-STATUS.
J11414****************************************************************
J11414     MOVE WS-C6D-COMPANY         TO DB-COMPANY.
J11414     MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
J11414     MOVE WS-C6D-PAYROLL-YEAR    TO DB-PAYROLL-YEAR.
J11414     INITIALIZE                     DB-INV-CODE.
J11414     PERFORM  850-MODIFY-NLT-C6XSET1.
J11414     PERFORM
J11414         UNTIL  (C6EXEMPT-NOTFOUND)
J11414            OR  (C6X-EMPLOYEE         NOT = EMP-EMPLOYEE)
J11414            OR  (C6X-PAYROLL-YEAR     NOT =  WS-C6D-PAYROLL-YEAR)
J11414            OR  (C6X-COMPANY          NOT = WS-C6D-COMPANY)
J11414                IF (C6X-STATUS = 0)
J11414                   SET WS-C6X-STATUS-CLOSED
J11414                                 TO TRUE
J11414                   MOVE WS-C6X-STATUS
J11414                                 TO C6X-STATUS
J68929                   INITIALIZE C6X-YTD-EXEMPT-AMT
J78009                              WS-C6X-EXEMPTED-AMT
J11414                   PERFORM   VARYING I9 FROM 1 BY 1
J11414                     UNTIL (I9 > 12)
J68929                     COMPUTE WS-C6X-EXEMPTED-AMT  =
J68929                     WS-C6X-EXEMPTED-AMT + C6X-EXEMPTED-AMT(I9)
J11414                   END-PERFORM
J68929                   MOVE WS-C6X-EXEMPTED-AMT TO C6X-YTD-EXEMPT-AMT
J11414                   PERFORM 820-STORE-C6EXEMPT
J11414                END-IF
J11414                PERFORM 860-MODIFY-NEXT-C6XSET1
J11414     END-PERFORM.
J11414****************************************************************
J11414 460-C6EXEMPT-END.
J11414****************************************************************
J11414****************************************************************
J11414 470-UPDATE-INEMP-DED-TAX-CAT.
J11414****************************************************************
J11414*** LOGIC FOR UPDATING CEM-ESIC-REASON TO 4 -OUT OF COVERAGE ***
J11414*** STARTS HERE                                              ***
J11414     IF  (WS-IPR-START-DATE-MONTH = 4 OR 10)
J11414         MOVE PRS-COMPANY        TO DB-COMPANY
J11414         MOVE EMP-EMPLOYEE       TO DB-EMPLOYEE
J11414         INITIALIZE                 DB-DED-CODE
J11414                                    DB-SEQ-NBR
J11414         PERFORM 850-FIND-NLT-EDMSET1 
J11414         IF  (EMDEDMASTR-FOUND)
J11414             MOVE EDM-COMPANY    TO DB-COMPANY
J11414             MOVE EDM-DED-CODE   TO DB-DED-CODE
J11414             PERFORM 840-FIND-DDCSET1
J11414             IF  (DEDCODE-FOUND)
J11414                 IF  (DDC-TAX-CATEGORY = 8)
J11414                     SET WS-INEMP-REASON-CODE-UPDATE-ON 
J11414                                 TO TRUE
J11414                 END-IF
J11414             END-IF
J11414             PERFORM             
J11414                 UNTIL (EMDEDMASTR-NOTFOUND)
J11414                    OR (WS-INEMP-REASON-CODE-UPDATE-ON)
J11414                    OR (EMP-EMPLOYEE NOT = EDM-EMPLOYEE)
J11414                       PERFORM 860-FIND-NEXT-EDMSET1
J11414                       MOVE EDM-COMPANY         
J11414                                 TO DB-COMPANY
J11414                       MOVE EDM-DED-CODE     
J11414                                 TO DB-DED-CODE
J11414                       PERFORM 840-FIND-DDCSET1
J11414                       IF  (DEDCODE-FOUND)
J11414                       AND (DDC-TAX-CATEGORY = 8)
J11414                           SET WS-INEMP-REASON-CODE-UPDATE-ON
J11414                                 TO TRUE
J11414                       END-IF
J11414                  END-PERFORM
J11414          END-IF.
J11414
J11414     IF  (WS-INEMP-REASON-CODE-UPDATE-ON)
J11414         MOVE PYM-COMPANY        TO DB-COMPANY
J11414         MOVE PYM-EMPLOYEE       TO DB-EMPLOYEE
J11414         MOVE DDC-DED-CODE       TO DB-DED-CODE
J16146*        MOVE PYDSET1-DED-CODE   TO WS-DB-BEG-RNG
J16146         MOVE PYDSET3-DED-CODE   TO WS-DB-BEG-RNG
J16146*        PERFORM 850-FIND-BEGRNG-PYDSET1
J16146         PERFORM 850-FIND-BEGRNG-PYDSET3
J11414         IF  (PAYDEDUCTN-FOUND)                         
J11414         AND (PYD-DED-AMT = 0)                    
J11414             MOVE PYD-COMPANY    TO DB-COMPANY        
J11414             MOVE PYM-EMPLOYEE   TO DB-EMPLOYEE               
J16146             MOVE DDC-DED-CODE   TO DB-DED-CODE
J16146             MOVE ZERO                 TO DB-SEQ-NBR
J16146             PERFORM 840-FIND-CEDSET1
J16146*            PERFORM 840-MODIFY-CEMSET1 
J16146*            MOVE  4             TO CEM-ESIC-REASON
J16146*            PERFORM 820-STORE-INEMPLOYEE
J16146             PERFORM 840-MODIFY-CEDSET1
J16146             MOVE  1                   TO CED-ESIC-STAT-IND
J16146             MOVE WS-SYSTEM-DATE-YMD   TO CED-DATE-STAMP
J16146             MOVE HHMMSS               TO CED-TIME-STAMP
J16146             MOVE CRT-USER-NAME        TO CED-USER-ID
J16146             PERFORM 820-STORE-C6EMDDMSTR
J11414         END-IF
J11414     END-IF.
J11414****************************************************************
J11414 470-INEMP-END. 
J11414****************************************************************

      ****************************************************************
       1100-UPDATE-EMP-ONETMDEDS       SECTION.
      ****************************************************************
       1100-START.

           PERFORM
             VARYING I5 FROM 1 BY 1
             UNTIL (WS-OTD-DED-CODE (I5) = SPACES)
             OR    (I5 > WS-EMP-OTD-TABLE-SIZE)

P87786         MOVE PRM-COMPANY             TO DB-COMPANY
P87786         MOVE WS-PYM-EMPLOYEE         TO DB-EMPLOYEE
               MOVE WS-OTD-DED-CODE (I5)    TO DB-DED-CODE
               MOVE WS-OTD-EDM-SEQ-NBR (I5) TO DB-EDM-SEQ-NBR
               MOVE WS-OTD-SEQ-NBR (I5)     TO DB-SEQ-NBR

               PERFORM 840-MODIFY-OTDSET1

               IF (WS-OTD-RECORD-TYPE (I5) = "S")
P88767             PERFORM 840-MODIFY-OTDSET1
                   MOVE "A"                 TO OTD-RECORD-TYPE
                   MOVE 2                   TO OTD-STATUS
P54823             MOVE WS-SYSTEM-DATE-YMD  TO OTD-DATE-STAMP
P54823             MOVE HHMMSS              TO OTD-TIME-STAMP
P54823             MOVE CRT-USER-NAME       TO OTD-USER-ID
                   PERFORM 820-STORE-ONETMDED
               ELSE
P88767             MOVE OTDSET1-SEQ-NBR     TO WS-DB-BEG-RNG
P88767             PERFORM 830-DELETERNG-OTDSET1
               END-IF

           END-PERFORM.

       1100-END.

402400******************************************************************
402500 8000-CREATE-BENWORK             SECTION.
402600******************************************************************
402700 8000-START.
402800
402900     INITIALIZE                  BENWORK-REC.
403000
403100     MOVE WS-TRUE                TO BENWORK-CREATED-SW.
403200******************************************************************
403300 8200-STORE-BENWORK              SECTION.
403400******************************************************************
403500 8200-START.
403600
403700     IF (BENWORK-CREATED)
403800         WRITE BENWORK-REC       INVALID KEY
403900             MOVE BWK-PLAN-CODE       TO CRT-ERR-VAR1
404000             MOVE 400               TO CRT-ERROR-NBR.
404100
404200     IF (NOT BENWORK-CREATED)
404300         REWRITE BENWORK-REC     INVALID KEY
404400             MOVE BWK-PLAN-CODE       TO CRT-ERR-VAR1
404500             MOVE 400               TO CRT-ERROR-NBR.
404600
404700     MOVE WS-FALSE               TO BENWORK-CREATED-SW.
404800******************************************************************
404900 8300-DELETE-BENWORK           SECTION.
405000******************************************************************
405100 8300-START.
405200
405300     DELETE BENWORK-FILE RECORD
405400         INVALID KEY
405500             MOVE BWK-PLAN-CODE         TO CRT-ERR-VAR1
405600             MOVE 400                 TO CRT-ERROR-NBR.
405700
405800******************************************************************
405900 8400-FIND-BENWORK               SECTION.
406000******************************************************************
406100 8400-START.
406200
406300     MOVE WS-FALSE               TO BENWORK-SW.
406400
406500     READ BENWORK-FILE           KEY BWK-REC-KEY
406600         INVALID KEY
406700             MOVE WS-TRUE        TO BENWORK-SW.
406800
406900******************************************************************
407000 8500-FIND-NLT-BENWORK           SECTION.
407100******************************************************************
407200 8500-START.
407300
407400     MOVE WS-FALSE               TO BENWORK-SW.
407500
407600     START BENWORK-FILE          KEY NOT < BWK-REC-KEY
407700         INVALID KEY
407800             MOVE WS-TRUE        TO BENWORK-SW.
407900
408000     IF (BENWORK-FOUND)
408100         PERFORM 8600-FIND-NEXT-BENWORK.
408200
408300******************************************************************
408400 8600-FIND-NEXT-BENWORK          SECTION.
408500******************************************************************
408600 8600-START.
408700
408800     MOVE WS-FALSE               TO BENWORK-SW.
408900
409000     READ BENWORK-FILE NEXT RECORD
409100         AT END
409200             MOVE WS-TRUE        TO BENWORK-SW.
409300
409400******************************************************************
409500 9800-OPEN-OUTPUT-BENWORK        SECTION.
409600******************************************************************
409700 9800-START.
409800
409900     OPEN OUTPUT BENWORK-FILE.
410000
410100 9800-END.
410200
410300******************************************************************
410400 9810-OPEN-IO-BENWORK          SECTION.
410500******************************************************************
410600 9810-START.
410700
410800     OPEN I-O BENWORK-FILE.
410900
411000 9810-END.
411100
411200******************************************************************
411300 9820-OPEN-INPUT-BENWORK         SECTION.
411400******************************************************************
411500 9820-START.
411600
411700     OPEN INPUT BENWORK-FILE.
411800
411900 9820-END.
412000
412100******************************************************************
412200 9900-CLOSE-BENWORK              SECTION.
412300******************************************************************
412400 9900-START.
412500
412600     CLOSE BENWORK-FILE.
412700
412800 9900-END.
412900



