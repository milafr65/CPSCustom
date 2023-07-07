******* PR134PD 72 <3973552014>
000100*    ********************************************************
000200*    *                                                      *
000300*    *             AUTO TIME RECORD CREATION                *
000400*    *                                                      *
      ******************************************************************
      *                                                                *
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  313588 | J13588 | Remove LP trigger checks                    *
      *  ------   ------   ------------------------------------------  *
      *  363759 | J63759 | Create Standard TRs controlled by hire date *
      *  ------   ------   ------------------------------------------  *
      *  469732 | J69732 | Skip new hires with hire date mid pay period*
      *  ------   ------   ------------------------------------------  *
      *  415961 | J15961 | ADD VALIDATION TO FILTER BATCH NUMBERS NOT  *
      *         |        | INCLUDED IN THE REPORT.                     *
      *  ------   ------   ------------------------------------------  *
      *  550888 | J50888 | CORRECT TIMERECORD PERIOD BEG DATES         *
      *  ------   ------   ------------------------------------------  *
      *  582010 | J82010 | PR134 not returning pay edits error         *
      *  ------   ------   ------------------------------------------  *
      *  670871 | J70871 | PR134 Goes into needs recovery for mulitple *
      *         |        | employees and LT turned off. This issue is  *
      *         |        | caused by fix on JT-582010.                 *
      *  ------   ------   ------------------------------------------  *
      *  717912 | J17912 | ME PAYROLL PROJECT TO ENABLE CREATION OF    *
      *         |        | DAILY TIME RECORDS                          *
      *  ------   ------   ------------------------------------------  *
      *  777488 | J77488 | USE THE PR30 EFFECT TO INDICATE THE START   *
      *         |        | DATE OF THE TIMERECORD TO BE CREATED.       *
      *  ------   ------   ------------------------------------------  *
      *  757836 | J57836 | UPDATED CODE TO DISPLAY EXACT VALUE OF      *
      *         |        | NOTIONAL AMOUNT                             *
      *  ------   ------   ------------------------------------------  *
      *  811610 | J11610 | Added changes to only allow creation of time*
      *         |        | records within the pay period date. Fixed   *
      *         |        | issue with additional rate not being updated*
      *  ------   ------   ------------------------------------------  *
      *  879159 | J79159 | Initialized PRTRD-ANNUAL-SALARY to fix the  *
      *         |        | issue that prevents PRTRDPD in updating     *
      *         |        | the additional rate flag.                   *
      *  ------   ------   ------------------------------------------  *
      *  529267 | J29267 | Fixed issue when PR134 is reporting already *
      *         |        | existing LP time records.                   *
      * -------   ------   ------------------------------------------  *
      * 1027084 | 027084 | REPLACED VALIDATION FLAG USED TO VALIDATE   *
      *         |        | WHEN TO PROCESS 664-PROCESS-LP-TIMERECS     *
      * -------   ------   ------------------------------------------  *
      * 1035273 | 035273 | ADDED VALIDATION WHEN ENTERING EMPL GRP &   *
      *         |        | EMPLOYEE THAT HAVE THE SAME VALUES          *
      * -------   ------   ------------------------------------------  *
      * 1028335 | 028335 | Changed the variable being used from a      *
      *         |        | working storage that is not being populated *
      *         |        | to the appropriate workfile variable        *
      * -------   ------   ------------------------------------------  *
      * 1039994 | 039994 | Remove usage of variable PRTRD-END-DATE and *
      *         |        | replace with STM-END-DATE in order for the  *
      *         |        | program to consider the PR30 end date when  *
      *         |        | creating daily time records.                *
      * -------   ------   ------------------------------------------  *
      * 1617501 | 617501 | ADDED END-IF ON 231-GENERATE-TIMERECORDS    *
      *         |        | AFTER VALIDATION ON PRTRD-PARTIAL/MID-PERIOD*
      * -------   ------   ------------------------------------------  *
      * 1725370 | 725370 | BYPASS THE CONDITIONS THAT CHECKS THE STNDRD*
      *         |        | TIME RECORD EFFECTIVE AND END DATES WHEN THE*
      *         |        | EE BEING PROCESSED IS FROM ME COUNTRIES     *
      * -------   ------   ------------------------------------------  *
      * 1753953 | 753953 | Changed the condition introduced by JT-1725370
      *         |        | to still allow middle east companies to     *
      *         |        | undergo the STM date range validation but   *
      *         |        | only when creating non-daily time records.  *
      * -------   ------   ------------------------------------------  *
      ******************************************************************
      ******************************************************************
      *               M O D I F I C A T I O N   L O G:                 *
      ******************************************************************
MG0325*  MG0325  03/25/04  ADD CODE FOR SPECIAL RUNS THAT WILL BYPASS  *
MG0325*                    STANDTIME RECORDS BASED ON CHECK GROUPS     *
ACS001*          10/29/10  READDED CUSTOMIZATIONS AFTER 9.0 APPS       *
ACS001*                    UPGRADE.  THE PR134PD, PR134WS AND PR134.SCR*
ACS001*                    WERE CUSTOMIZED.                            *
ACS002*          08/19/11  M. HUNTER REAPPLIED CUSTOM CODE AFTER 9.0.1 *
ACS002*                    APPS UPGRADE.  THE PD, WS AND .SCR WERE     *
ACS002*                    MODIFIED.
MG0325******************************************************************
000500******************************************************************
000600 050-EDIT-PARAMETERS             SECTION.
000700******************************************************************
000800 050-START.
000900
001000     MOVE PRM-COMPANY            TO DB-COMPANY.
001100     MOVE SPACES                 TO DB-PROCESS-LEVEL.
001200
001300     PERFORM 840-FIND-PRSSET1.
001400
001500     IF (PRSYSTEM-NOTFOUND)
001600         MOVE PRM-COMPANY        TO CRT-ERR-VAR1
001700         MOVE 100                TO CRT-ERROR-NBR
001800         PERFORM 780-PRINT-ERROR-MSG
001900         GO TO 050-END.
002000
J69732     MOVE PRM-COMPANY            TO PH-COMPANY
J69732                                    EXH-COMPANY.
J69732     MOVE PRS-NAME               TO PH-NAME
J69732                                    EXH-NAME.
002300
002400     IF (PRM-EMPLOYEE-SEQ = SPACES)
002500         MOVE PRS-EMPLOYEE-SEQ   TO PRM-EMPLOYEE-SEQ.
002600
002700     IF  (PRM-BATCH-NBR    = ZEROS)
002800     AND (PRS-BATCH-OPTION = "N")
002900         MOVE 101                TO CRT-ERROR-NBR
003000         PERFORM 780-PRINT-ERROR-MSG.
003100
003200     IF  (PRM-BATCH-NBR NOT = ZEROS)
003300          MOVE PRM-BATCH-NBR     TO DB-BATCH-NBR
003400          PERFORM 840-FIND-PRBSET1
003500          IF  (PRBATCH-NOTFOUND)
003600          AND (PRS-BATCH-OPTION = "Y")
003700               MOVE 125          TO CRT-ERROR-NBR
003800               PERFORM 780-PRINT-ERROR-MSG.
003900
004000     IF (PRM-CHECK-GRP = "0")
004100         MOVE 102                TO CRT-ERROR-NBR
004200         PERFORM 780-PRINT-ERROR-MSG.
004300
004400     IF (PRM-PC-CHECK-GRP (1) = "0")
004500     OR (PRM-PC-CHECK-GRP (2) = "0")
004600     OR (PRM-PC-CHECK-GRP (3) = "0")
004700     OR (PRM-PC-CHECK-GRP (4) = "0")
004800         MOVE 102                TO CRT-ERROR-NBR
004900         PERFORM 780-PRINT-ERROR-MSG.
005000
J17912     IF  (PRM-SELECTION NOT = "S" AND "T" AND "D" AND "G")
J17912     AND (PRS-WORK-COUNTRY = "SA" OR "AE" OR "QA")
J17912          MOVE 146               TO CRT-ERROR-NBR
J17912          PERFORM 780-PRINT-ERROR-MSG
J17912     END-IF.
J17912
005100     IF  (PRM-SELECTION      = "T" OR "A" OR "G" 
P60416                            OR "D" OR "E")
005200         IF  (PRM-GROUP-NAME (1)  = SPACES)
005300         AND (PRM-GROUP-NAME (2)  = SPACES)
005400         AND (PRM-GROUP-NAME (3)  = SPACES)
005500         AND (PRM-GROUP-NAME (4)  = SPACES)
005600         AND (PRM-GROUP-NAME (5)  = SPACES)
               AND (PRM-GROUP-NAME (6)  = SPACES)
               AND (PRM-GROUP-NAME (7)  = SPACES)
               AND (PRM-GROUP-NAME (8)  = SPACES)
               AND (PRM-GROUP-NAME (9)  = SPACES)
               AND (PRM-GROUP-NAME (10) = SPACES)
005700             MOVE 103            TO CRT-ERROR-NBR
005800             PERFORM 780-PRINT-ERROR-MSG.
005900
           IF (PRM-SELECTION = "T")
               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > 15)
                   OR     (ERROR-FOUND)
                       IF (PRM-EMPLOYEE (I1) NOT = ZEROES)
                           MOVE 132                  TO CRT-ERROR-NBR
                           PERFORM 780-PRINT-ERROR-MSG
                       END-IF
               END-PERFORM
           END-IF.

           SET WS-NO-GROUP-SELECT      TO TRUE.
006000     IF (PRM-GROUP-NAME  (1) NOT = SPACES)
006100     OR (PRM-GROUP-NAME  (2) NOT = SPACES)
006200     OR (PRM-GROUP-NAME  (3) NOT = SPACES)
006300     OR (PRM-GROUP-NAME  (4) NOT = SPACES)
006400     OR (PRM-GROUP-NAME  (5) NOT = SPACES)
           OR (PRM-GROUP-NAME  (6) NOT = SPACES)
           OR (PRM-GROUP-NAME  (7) NOT = SPACES)
           OR (PRM-GROUP-NAME  (8) NOT = SPACES)
           OR (PRM-GROUP-NAME  (9) NOT = SPACES)
           OR (PRM-GROUP-NAME (10) NOT = SPACES)
               SET WS-GROUP-SELECT     TO TRUE
006500         IF (PRM-SELECTION = "S" OR "F" OR "B")
006600             MOVE WS-TRUE        TO WS-STD-TR-WITHIN-TG
006700         ELSE
006800         IF (PRM-SELECTION = "P")
006900             MOVE WS-TRUE        TO WS-PC-ONLY-FOR-TG.
007000
J13588*    MOVE PRM-COMPANY                 TO EDCDWS-COMPANY.
J13588*    MOVE "LP"                        TO EDCDWS-SYSTEM.
J13588*    PERFORM 6000-IS-SYSTEM-TRIGGER-ENABLED.
J13588*
J13588*    IF  (NOT EDCDWS-TRIGGER-ENABLED)
J13588*        IF (PRM-REASON-CODE (1) NOT = SPACES)
J13588*        OR (PRM-REASON-CODE (2) NOT = SPACES)
J13588*        OR (PRM-REASON-CODE (3) NOT = SPACES)
J13588*        OR (PRM-REASON-CODE (4) NOT = SPACES)
J13588*        OR (PRM-REASON-CODE (5) NOT = SPACES)
J13588*           MOVE 135                TO CRT-ERROR-NBR
J13588*           PERFORM 780-PRINT-ERROR-MSG.

007100     IF  (PRM-SELECTION       = "P")
007200     AND (PRM-PAY-CODE (1)    = SPACES)
007300     AND (PRM-PAY-CODE (2)    = SPACES)
007400     AND (PRM-PAY-CODE (3)    = SPACES)
007500     AND (PRM-PAY-CODE (4)    = SPACES)
           AND (PRM-PAY-CODE (5)    = SPACES)
007600     AND (PRM-ATTEND-CODE (1) = SPACES)
007700     AND (PRM-ATTEND-CODE (2) = SPACES)
007800     AND (PRM-ATTEND-CODE (3) = SPACES)
007900     AND (PRM-ATTEND-CODE (4) = SPACES)
           AND (PRM-ATTEND-CODE (5) = SPACES)
LP         AND (PRM-REASON-CODE (1) = SPACES)
LP         AND (PRM-REASON-CODE (2) = SPACES)
LP         AND (PRM-REASON-CODE (3) = SPACES)
LP         AND (PRM-REASON-CODE (4) = SPACES)
           AND (PRM-REASON-CODE (5) = SPACES)
008000         MOVE 104                TO CRT-ERROR-NBR
008100         PERFORM 780-PRINT-ERROR-MSG.
008200
008300     INITIALIZE WS-PR134-ERROR-SW.
008400     PERFORM 052-PAY-CODE-EDIT
008500         VARYING I1 FROM 1 BY 1
008600         UNTIL  (I1 > 5)
P08896         OR     (PR134-ERROR) .
008700
008800     IF (PR134-ERROR)
P08896        IF (WS-ERROR-CAT NOT = SPACES)
                  MOVE WS-ERROR-CAT     TO CRT-ERROR-CAT
                  MOVE WS-ERR-VAR1      TO CRT-ERR-VAR1
                  MOVE WS-ERROR-NBR     TO CRT-ERROR-NBR
              ELSE
                  CONTINUE
              END-IF
009000        PERFORM 780-PRINT-ERROR-MSG.
009100
009200     IF (PRM-PROC-GROUP NOT = SPACES)
009200     AND (PRM-PROCESS-LEVEL NOT = SPACES)
009600             MOVE PRM-PROCESS-LEVEL  TO CRT-ERR-VAR1
009700             MOVE 126                TO CRT-ERROR-NBR
009800             PERFORM 780-PRINT-ERROR-MSG.

009200     IF (PRM-PROC-GROUP NOT = SPACES)
009300         MOVE PRM-PROC-GROUP    TO DB-PROC-GROUP
009300         MOVE PRPSET1-PROC-GROUP TO WS-DB-BEG-RNG
009400         PERFORM 850-FIND-BEGRNG-PRPSET1
               IF (PRPROCGRP-NOTFOUND)
009600             MOVE PRM-PROC-GROUP     TO CRT-ERR-VAR1
009700             MOVE 127                TO CRT-ERROR-NBR
009800             PERFORM 780-PRINT-ERROR-MSG.
009100
009200     IF (PRM-PROCESS-LEVEL NOT = SPACES)
009300         MOVE PRM-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
009400         PERFORM 840-FIND-PRSSET1
009500         IF (PRSYSTEM-NOTFOUND)
009600             MOVE PRM-PROCESS-LEVEL  TO CRT-ERR-VAR1
009700             MOVE 105                TO CRT-ERROR-NBR
009800             PERFORM 780-PRINT-ERROR-MSG.
009900
010000     IF  (PRM-DEPARTMENT    NOT = SPACES)
010100     AND (PRM-PROCESS-LEVEL = SPACES)
010200         MOVE 106                TO CRT-ERROR-NBR
010300         PERFORM 780-PRINT-ERROR-MSG.
010400
010500     IF  (PRM-DEPARTMENT    NOT = SPACES)
010600     AND (PRM-PROCESS-LEVEL NOT = SPACES)
010700         MOVE PRM-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
010800         MOVE PRM-DEPARTMENT     TO DB-DEPARTMENT
010900         PERFORM 840-FIND-DPTSET1
011000         IF (DEPTCODE-NOTFOUND)
011100             MOVE PRM-DEPARTMENT TO CRT-ERR-VAR1
011200             MOVE 107            TO CRT-ERROR-NBR
011300             PERFORM 780-PRINT-ERROR-MSG.
011400
011500     IF  (PRM-SELECTION      = "A" OR "B" OR "F" 
P60416                            OR "E")
011600     AND (PRM-FLEX-DATE = ZEROS)
011700         MOVE PRM-SELECTION      TO CRT-ERR-VAR1
011800         MOVE 122                TO CRT-ERROR-NBR
011900         PERFORM 780-PRINT-ERROR-MSG.
012000
012100     IF  (PRM-SELECTION      = "S" OR "G" OR "T" OR
012200                               "P" 
P60416                            OR "D")
012300     AND (PRM-FLEX-DATE NOT = ZEROS)
012400         MOVE PRM-SELECTION      TO CRT-ERR-VAR1
012500         MOVE 123                TO CRT-ERROR-NBR
012600         PERFORM 780-PRINT-ERROR-MSG.
012700
           SET WS-NO-EMPLOYEE-SELECT TO TRUE.
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 15)
               OR     (ERROR-FOUND)

               IF (PRM-EMPLOYEE (I1) NOT = ZEROES)
035273             ADD 1                          TO I2
035273             MOVE PRM-EMPLOYEE (I1)         
035273                                  TO PR134WS-PRM-EMPLOYEE (I2)
035273             PERFORM 
035273             VARYING I3 FROM 1 BY 1
035273               UNTIL (I3 > I2)
035273                 IF  (PR134WS-PRM-EMPLOYEE (I3)
035273                                   = PRM-EMPLOYEE (I1))
035273                 AND (I3 < I2)
035273*--- PR134#147: CANNOT ENTER AN EMPLOYEE NUMBER MORE THAN ONCE ({0})
035273                     MOVE "PR134"           TO CRT-ERROR-CAT
035273                     MOVE PRM-EMPLOYEE (I1) 
035273                                  TO PR134WS-MSG-EMPLOYEE
035273                     MOVE PR134WS-MSG-EMPLOYEE
035273                                            TO CRT-ERR-VAR1
035273                     MOVE 147               TO CRT-ERROR-NBR
035273                     PERFORM 780-PRINT-ERROR-MSG
035273                     GO TO 050-END
035273                 END-IF
035273             END-PERFORM
035273
                   MOVE PRM-EMPLOYEE (I1)         TO DB-EMPLOYEE
                   PERFORM 840-FIND-EMPSET1

                   IF (EMPLOYEE-NOTFOUND)
                       MOVE 129                   TO CRT-ERROR-NBR
                       MOVE PRM-EMPLOYEE (I1)     TO CRT-ERR-VAR1
                       PERFORM 780-PRINT-ERROR-MSG
                   ELSE
                   IF (EMP-AUTO-TIME-REC NOT = "Y" AND "S")
P33365                AND (PRM-SELECTION NOT = "F")
                       MOVE 131                   TO CRT-ERROR-NBR
                       MOVE PRM-EMPLOYEE (I1)     TO CRT-ERR-VAR1
                       PERFORM 780-PRINT-ERROR-MSG
                   ELSE
                       IF (WS-GROUP-SELECT)
                           MOVE 133               TO CRT-ERROR-NBR
                           PERFORM 780-PRINT-ERROR-MSG
                       ELSE
                           SET WS-EMPLOYEE-SELECT TO TRUE
                       END-IF
                   END-IF
                   END-IF
               END-IF
           END-PERFORM.

           IF (WS-GROUP-SELECT)
               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > 10)
                   OR     (ERROR-FOUND)

                   IF (PRM-GROUP-NAME (I1) NOT = SPACES)
035273                 ADD 1                          TO I2
035273                 MOVE PRM-GROUP-NAME (I1)
035273                                  TO PR134WS-PRM-GROUP-NAME (I2)
035273                 PERFORM
035273                 VARYING I3 FROM 1 BY 1
035273                   UNTIL (I3 > I2)
035273                     IF  (PR134WS-PRM-GROUP-NAME (I3)
035273                                       = PRM-GROUP-NAME (I1))
035273                     AND (I3 < I2)
035273*--- PR134#148: CANNOT ENTER AN EMPLOYEE GROUP MORE THAN ONCE ({0})
035273                         MOVE "PR134"           TO CRT-ERROR-CAT
035273                         MOVE PRM-GROUP-NAME (I1)
035273                                                TO CRT-ERR-VAR1
035273                         MOVE 148               TO CRT-ERROR-NBR
035273                         PERFORM 780-PRINT-ERROR-MSG
035273                         GO TO 050-END
035273                     END-IF
035273                 END-PERFORM
035273
                       MOVE PRM-GROUP-NAME (I1)         TO DB-GROUP-NAME
                       PERFORM 840-FIND-PRGSET1

                       IF (PERSGROUP-NOTFOUND)
                           MOVE 130                     TO CRT-ERROR-NBR
                           MOVE PRM-GROUP-NAME (I1)     TO CRT-ERR-VAR1
                           PERFORM 780-PRINT-ERROR-MSG
                       END-IF
                   END-IF
               END-PERFORM
           END-IF.

P14541     MOVE "PR"                   TO EDSYWS-SYSTEM.
P14541     PERFORM 540-EDIT-SYSTEM-70.
P14541     IF (EDSYWS-ERROR-NBR NOT = ZEROES)
P14541         PERFORM 780-PRINT-ERROR-MSG
P14541         GO TO 050-END.
P14541

J69732     IF  (PRM-MID-PERIOD             = ZEROES) 
J69732     AND (PRM-EXCEPTION-REPORT   NOT = ZEROES)     
J63759         MOVE 117                TO CRT-ERROR-NBR
J63759         PERFORM 780-PRINT-ERROR-MSG 
J63749         GO TO 050-END.

012800 050-END.
012900******************************************************************
013000 052-PAY-CODE-EDIT               SECTION.
013100******************************************************************
013200 052-START.
013300

P08896     MOVE "PR134"                    TO CRT-ERROR-CAT.
P08896     INITIALIZE WS-ERROR-CAT
                      WS-ERROR-NBR
                      WS-ERR-VAR1.
013400 

013400     IF  (PRM-PAY-CODE (I1)    = SPACES)
013500     AND (PRM-ATTEND-CODE (I1) = SPACES)
LP         AND (PRM-REASON-CODE (I1) = SPACES)
013600         GO TO 052-END.
013700
013800     IF (PRM-ATTEND-CODE (I1) NOT = SPACES)
013900         MOVE PRM-ATTEND-CODE (I1)   TO DB-ATTEND-CODE
014000         PERFORM 840-FIND-ATCSET1
014100         IF (ATTENDCODE-NOTFOUND)
014200             MOVE DB-ATTEND-CODE     TO CRT-ERR-VAR1
014300             MOVE 109                TO CRT-ERROR-NBR
P08896             MOVE CRT-ERROR-NBR      TO WS-ERROR-NBR
P08896             MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
                   MOVE CRT-ERR-VAR1       TO WS-ERR-VAR1 
014400             MOVE WS-TRUE            TO WS-PR134-ERROR-SW
014500             PERFORM 780-PRINT-ERROR-MSG
                   GO TO 052-END
014600         ELSE
014700         IF (PRM-PAY-CODE (I1) = SPACES)
014800             MOVE ATC-PAY-CODE       TO PRM-PAY-CODE (I1).
014900
LP         IF  (PRM-REASON-CODE (I1) NOT = SPACES)
               MOVE PRM-COMPANY              TO DB-COMPANY
               MOVE PRM-REASON-CODE (I1)     TO DB-REASON-CODE 
               PERFORM 840-FIND-TRESET1
              IF (TAREASCODE-NOTFOUND)
                 MOVE PRM-REASON-CODE (I1) TO CRT-ERR-VAR1
                 MOVE 136                  TO CRT-ERROR-NBR
                 MOVE CRT-ERROR-NBR        TO WS-ERROR-NBR
                 MOVE CRT-ERROR-CAT        TO WS-ERROR-CAT
                 MOVE CRT-ERR-VAR1         TO WS-ERR-VAR1  
017700           MOVE WS-TRUE              TO WS-PR134-ERROR-SW
017800           PERFORM 780-PRINT-ERROR-MSG
                 GO TO 052-END
              ELSE
              IF (TRE-STATUS = 1)
                 MOVE PRM-REASON-CODE (I1) TO CRT-ERR-VAR1
                 MOVE 140                  TO CRT-ERROR-NBR
                 MOVE CRT-ERROR-NBR        TO WS-ERROR-NBR
                 MOVE CRT-ERROR-CAT        TO WS-ERROR-CAT
                 MOVE CRT-ERR-VAR1         TO WS-ERR-VAR1 
017700           MOVE WS-TRUE              TO WS-PR134-ERROR-SW
017800           PERFORM 780-PRINT-ERROR-MSG
                 GO TO 052-END
              END-IF
              END-IF
           END-IF.

LP         IF  (PRM-REASON-CODE (I1) NOT = SPACES)
           AND (PRM-PAY-CODE    (I1)     = SPACES)
               IF (TRE-SERVICE-CODE NOT = SPACES)
                  MOVE PRM-COMPANY              TO DB-COMPANY
                  MOVE PCDSET1-COMPANY          TO WS-DB-BEG-RNG
                  MOVE "(PCD-SERVICE-CODE=""?"")" TO FILTER-STRING
                  PERFORM 890-CREATE-FILTER
                  MOVE TRE-SERVICE-CODE             
                                                TO ALPHANUM-FILTER-VALUE
                  PERFORM 890-SET-ALPHANUM-FILTER-VALUE
008000            PERFORM 850-FILTER-BEGRNG-PCDSET1
008100            IF  (PRPAYCODE-FOUND)
                      MOVE PCD-PAY-CODE           TO PRM-PAY-CODE (I1)
                  ELSE
                      MOVE 139                  TO CRT-ERROR-NBR
                      MOVE CRT-ERROR-NBR        TO WS-ERROR-NBR
                      MOVE CRT-ERROR-CAT        TO WS-ERROR-CAT
017700                MOVE WS-TRUE              TO WS-PR134-ERROR-SW
017800                PERFORM 780-PRINT-ERROR-MSG
                      GO TO 052-END
                  END-IF
               ELSE
                  MOVE 139                  TO CRT-ERROR-NBR
                  MOVE CRT-ERROR-NBR        TO WS-ERROR-NBR
                  MOVE CRT-ERROR-CAT        TO WS-ERROR-CAT
017700            MOVE WS-TRUE              TO WS-PR134-ERROR-SW
017800            PERFORM 780-PRINT-ERROR-MSG
                  GO TO 052-END
               END-IF
           END-IF.

015000     IF  (PRM-OCCURRENCE (I1)  NOT = SPACES)
015100     AND (PRM-ATTEND-CODE (I1) = SPACES)
LP         AND (PRM-REASON-CODE (I1) = SPACES)
015200         MOVE 110                TO CRT-ERROR-NBR
P08896         MOVE CRT-ERROR-NBR      TO WS-ERROR-NBR
P08896         MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
015300         MOVE WS-TRUE            TO WS-PR134-ERROR-SW
015400         PERFORM 780-PRINT-ERROR-MSG.
015500
015600     IF  (PRM-OCCURRENCE (I1)  = SPACES)
015700     AND ((PRM-ATTEND-CODE (I1) NOT = SPACES)
LP         OR   (PRM-REASON-CODE (I1) NOT = SPACES))
015800          MOVE "Y"                TO PRM-OCCURRENCE (I1).
015900
016000     MOVE PRM-COMPANY            TO DB-COMPANY.
016100     MOVE PRM-PAY-CODE (I1)      TO DB-PAY-CODE.
016200     MOVE PCDSET4-PAY-CODE       TO WS-DB-BEG-RNG.
016300     PERFORM 850-FIND-BEGRNG-PCDSET4.
016400
016500     IF (PRPAYCODE-NOTFOUND)
016600         MOVE DB-PAY-CODE        TO CRT-ERR-VAR1
016700         MOVE 112                TO CRT-ERROR-NBR
P08896         MOVE CRT-ERROR-NBR      TO WS-ERROR-NBR
P08896         MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
               MOVE CRT-ERR-VAR1       TO WS-ERR-VAR1 
016800         MOVE WS-TRUE            TO WS-PR134-ERROR-SW
016900         PERFORM 780-PRINT-ERROR-MSG
017000         GO TO 052-END
017100     ELSE
017200         MOVE PRS-TIPS           TO DB-PAY-CLASS
017300         MOVE PCD-PAY-SUM-GRP    TO DB-PAY-SUM-GRP
017400         PERFORM 840-KFIND-PSRSET1
017500         IF (PSGRELATE-KFOUND)
017600             MOVE 113            TO CRT-ERROR-NBR
P08896             MOVE CRT-ERROR-NBR  TO WS-ERROR-NBR
P08896             MOVE CRT-ERROR-CAT  TO WS-ERROR-CAT
017700             MOVE WS-TRUE        TO WS-PR134-ERROR-SW
017800             PERFORM 780-PRINT-ERROR-MSG
017900         ELSE
018000             MOVE PRS-PAY-IN-KIND    TO DB-PAY-CLASS
018100             PERFORM 840-KFIND-PSRSET1
018200             IF (PSGRELATE-KFOUND)
018300                 MOVE 114            TO CRT-ERROR-NBR
018400                 MOVE WS-TRUE        TO WS-PR134-ERROR-SW
P08896                 MOVE CRT-ERROR-NBR  TO WS-ERROR-NBR
P08896                 MOVE CRT-ERROR-CAT  TO WS-ERROR-CAT
018500                 PERFORM 780-PRINT-ERROR-MSG.
018600
LP          IF  (PRM-REASON-CODE (I1) NOT = SPACES)
            AND (PRM-PAY-CODE    (I1) NOT = SPACES)
            AND (PCD-SERVICE-CODE         = SPACES)    
                MOVE 137            TO CRT-ERROR-NBR
                MOVE WS-TRUE        TO WS-PR134-ERROR-SW
                MOVE CRT-ERROR-NBR  TO WS-ERROR-NBR
                MOVE CRT-ERROR-CAT  TO WS-ERROR-CAT
                PERFORM 780-PRINT-ERROR-MSG
                GO TO 052-END.
                      
LP          IF  (PRM-REASON-CODE (I1) NOT = SPACES)
            AND (PRM-PAY-CODE    (I1) NOT = SPACES)
            AND (PCD-SERVICE-CODE     NOT = SPACES)    
                MOVE PCD-SERVICE-CODE TO DB-SERVICE-CODE
                PERFORM 840-FIND-TSCSET1
                IF (TASERVCODE-NOTFOUND)
                   MOVE PRM-PAY-CODE (I1)    TO CRT-ERR-VAR1
                   MOVE 138                  TO CRT-ERROR-NBR
                   MOVE WS-TRUE              TO WS-PR134-ERROR-SW
                   MOVE CRT-ERROR-NBR        TO WS-ERROR-NBR
                   MOVE CRT-ERROR-CAT        TO WS-ERROR-CAT
                   MOVE CRT-ERR-VAR1         TO WS-ERR-VAR1 
                   PERFORM 780-PRINT-ERROR-MSG
                   GO TO 052-END
                ELSE
                IF (TSC-EVENT = 0)
                   MOVE 137            TO CRT-ERROR-NBR
                   MOVE WS-TRUE        TO WS-PR134-ERROR-SW
                   MOVE CRT-ERROR-NBR  TO WS-ERROR-NBR
                   MOVE CRT-ERROR-CAT  TO WS-ERROR-CAT
                   PERFORM 780-PRINT-ERROR-MSG
                   GO TO 052-END.

018700     IF  (PRM-HOURS (I1) = ZEROS)
018800     AND (PCD-NORM-RATE  = ZEROS)
018900         MOVE 111                TO CRT-ERROR-NBR
P08896         MOVE CRT-ERROR-NBR      TO WS-ERROR-NBR
P08896         MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
019000         MOVE WS-TRUE            TO WS-PR134-ERROR-SW
019100         PERFORM 780-PRINT-ERROR-MSG.
019200
019300     IF (PRM-ACTIVITY (I1)      NOT = SPACES)
019400         MOVE PRM-ACTIVITY (I1)      TO ACACWS-ACTIVITY
019500         MOVE PRM-ACCT-CATEGORY (I1) TO ACACWS-ACCT-CATEGORY
P08896         MOVE 2                      TO ACACWS-EDIT-CODE
               MOVE "N"                    TO ACACWS-CURRENCY-OR
                                              ACACWS-RESOURCE-OR
                                              ACACWS-BUDGET-OR
                                              ACACWS-GL-OR
019800         PERFORM 640-EDIT-ACTIVITY-70
019900         IF (ERROR-FOUND)
020000             MOVE WS-TRUE            TO WS-PR134-ERROR-SW
P08896             MOVE CRT-ERROR-NBR      TO WS-ERROR-NBR
P08896             MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
020100             PERFORM 780-PRINT-ERROR-MSG
020200         ELSE
020300             MOVE ACACWS-ACCT-CATEGORY TO PRM-ACCT-CATEGORY (I1)
020400         END-IF
020500     ELSE
020600         IF (PRM-ACCT-CATEGORY (I1) NOT = SPACES)
020700             MOVE PRM-ACCT-CATEGORY (I1) TO ACCTWS-ACCT-CATEGORY
020800             PERFORM 615-EDIT-ACCT-CAT-70
020900             IF (ERROR-FOUND)
021000                 MOVE WS-TRUE            TO WS-PR134-ERROR-SW
P08896                 MOVE CRT-ERROR-NBR      TO WS-ERROR-NBR
P08896                 MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
021100                 PERFORM 780-PRINT-ERROR-MSG
021200             END-IF
021300         END-IF
021400     END-IF.
021500
021600 052-END.
021700     EXIT.
021800******************************************************************
021900 100-PROGRAM-CONTROL             SECTION.
022000******************************************************************
022100 100-START.
022200
022300     MOVE 116                    TO CRT-MSG-NBR.
022400     PERFORM 780-DISPLAY-MSG.
022500
022600     PERFORM 910-AUDIT-BEGIN.
022700
022800     PERFORM 840-MODIFY-CKPSET1.
022900
023000     IF (CKP-RESTART-INFO NOT = SPACES)
023100         MOVE CKP-RESTART-INFO   TO WS-RESTART-INFO
023200     ELSE
023300         MOVE 1                  TO WS-RESTART-PHASE
023400         MOVE ZEROS              TO WS-RESTART-RECORD-COUNT
023500         MOVE WS-RESTART-INFO    TO CKP-RESTART-INFO.
023600
023700     PERFORM 820-STORE-CKPOINT.
023800
023900     PERFORM 925-AUDIT-END.
024000
024100     PERFORM 900-BUILD-TMP-FILE-NAME.
024200     MOVE WS-TMP-FILE            TO WS-ERRORFILE-NAME.
024300     OPEN OUTPUT ERROR-FILE.

J69732     IF (PRM-EXCEPTION-REPORT    = "1" OR "2")
J69732         OPEN OUTPUT EXCEPTION-FILE
J69732     END-IF.

J13588*    IF (EDCDWS-TRIGGER-ENABLED)
J13588     PERFORM 900-BUILD-TMP-FILE-NAME.
J13588     MOVE WS-TMP-FILE        TO WS-LPBALTRD-FILE-NAME.
J13588     PERFORM 9800-OPEN-OUTPUT-LPBALTRD.
J13588     PERFORM 9900-CLOSE-LPBALTRD.
J13588     PERFORM 9810-OPEN-IO-LPBALTRD.
J13588     PERFORM 605-OPEN-LPBALWORKFILE.
J13588     SET LPBAL-BATCH-PROGRAM TO TRUE.
J13588     INITIALIZE LPBAL-LNK-WORKFILE-DONE-SW.
J13588     INITIALIZE LPBAL-BATCH-FIRST-EMP-SW.
J13588     INITIALIZE LPBAL-SKIP-PRTRD-EDIT-SW
J13588                PRTRD-LP-EDIT-DONE-SW.      

024500     IF (WS-RESTART-PHASE = 1)
024600         GO TO 100-PHASE-1
024700     ELSE
024800     IF (WS-RESTART-PHASE = 2)
024900         MOVE WS-RESTART-WF-NAME TO WS-WORKFILE-NAME
025000         GO TO 100-PHASE-2
025100     ELSE
025200     IF (WS-RESTART-PHASE = 3)
025300         MOVE WS-RESTART-WF-NAME TO WS-WORKFILE-NAME
025400         GO TO 100-PHASE-3.
025500
025600 100-PHASE-1.
025700
025800     PERFORM 900-BUILD-TMP-FILE-NAME.
025900     MOVE WS-TMP-FILE            TO WS-WORKFILE-NAME.
026000     OPEN OUTPUT WORK-FILE.
026100
026200     IF (PRM-SELECTION = "T" OR "A" OR "G" OR "P"
P60416                      OR "D" OR "E")
026400         PERFORM
026500             VARYING I1 FROM 1 BY 1
026600             UNTIL  (I1 > 10)
026700             IF (PRM-GROUP-NAME (I1) NOT = SPACES)
026800                 MOVE PRM-GROUP-NAME (I1)    TO DB-TIME-GROUP
026900                 MOVE STMSET3-TIME-GROUP     TO WS-DB-BEG-RNG
027000                 PERFORM 850-FIND-BEGRNG-STMSET3
027100                 IF (STANDTIME-NOTFOUND)
027200                     MOVE DB-TIME-GROUP      TO CRT-ERR-VAR1
027300                     MOVE 108                TO CRT-MSG-NBR
027400                     PERFORM 790-GET-MSG
027500                     MOVE CRT-MESSAGE        TO ER-MESSAGE
027600                     MOVE ZEROS              TO ER-EMPLOYEE
027700                     WRITE ERROR-REC         FROM ER-ERROR-REC
027800                     MOVE "Y"                TO WS-ERROR-FOUND
027900                 END-IF
028000             END-IF
028100         END-PERFORM.

J69732     IF  (PRM-MID-PERIOD             = 1 OR 2)               
J69732     AND (PRM-EXCEPTION-REPORT   NOT = 0)
J63759         MOVE "D"                TO PH-TYPE
J69732                                    EXH-TYPE
J63759         MOVE EXCEPT-HEADING     TO RPT-GROUP-REQUEST
J63759         PERFORM 700-PRINT-RPT-GRP
J69732         MOVE PRM-MID-PERIOD     TO PHD-TYPE
J63759         MOVE PARTIAL-HEADING    TO RPT-GROUP-REQUEST
J63759         PERFORM 700-PRINT-RPT-GRP
J63759     END-IF.
028200
028300     PERFORM 200-EXTRACT-TIMERECORDS.
028400
028500     IF (WS-EMPLOYEE-NOTFOUND)
028600         MOVE "PR134"            TO CRT-ERROR-CAT
028700         MOVE 119                TO CRT-MSG-NBR
028800         PERFORM 780-PRINT-MSG
028900         CLOSE WORK-FILE PURGE
029000         MOVE WS-WORKFILE-NAME   TO WS-TMP-FILE
029100         PERFORM 901-REMOVE-TMP-FILE
029200         IF (WS-ERROR-FOUND = "Y")
029300             GO TO 100-ERROR
029400         ELSE
029500             GO TO 100-END.
029600
029700     CLOSE WORK-FILE SAVE.
029800     PERFORM 910-AUDIT-BEGIN.
029900
030000     PERFORM 840-MODIFY-CKPSET1.
030100
030200     MOVE 2                      TO WS-RESTART-PHASE.
030300     MOVE WS-WORKFILE-NAME       TO WS-RESTART-WF-NAME.
030400     MOVE WS-RESTART-INFO        TO CKP-RESTART-INFO.
030500
030600     PERFORM 820-STORE-CKPOINT.
030700
030800     PERFORM 925-AUDIT-END.
030900
031000 100-PHASE-2.
031100
031200     IF (PRM-EMPLOYEE-SEQ = "A")
031300         SORT SORT-FILE
031400             ON ASCENDING KEY    DSF-LAST-NAME
                   ON ASCENDING KEY    DSF-FIRST-NAME
                   ON ASCENDING KEY    DSF-MIDDLE-INIT
031700             ON ASCENDING KEY    DSF-EMPLOYEE
031800             ON ASCENDING KEY    DSF-PAY-CODE
J17912             ON DESCENDING KEY   DSF-TR-DATE
031900             USING               WORK-FILE
032000             GIVING              WORK-FILE SAVE
032100     ELSE
032200         SORT SORT-FILE
032300             ON ASCENDING KEY    DSF-EMPLOYEE
032400             ON ASCENDING KEY    DSF-PAY-CODE
J17912             ON DESCENDING KEY   DSF-TR-DATE
032500             USING               WORK-FILE
032600             GIVING              WORK-FILE SAVE.
032700
032800     PERFORM 910-AUDIT-BEGIN.
032900
033000     PERFORM 840-MODIFY-CKPSET1.
033100
033200     MOVE 3                      TO WS-RESTART-PHASE.
033300     MOVE WS-RESTART-INFO        TO CKP-RESTART-INFO.
033400
033500     PERFORM 820-STORE-CKPOINT.
033600
033700     PERFORM 925-AUDIT-END.
033800
033900 100-PHASE-3.
034000
034100     OPEN INPUT WORK-FILE.
034200
034300     MOVE ZEROS                  TO WS-RECORD-COUNT.
034400     PERFORM 300-CREATE-TIMERECORDS.
034500
034600     CLOSE WORK-FILE.
034700     MOVE WS-WORKFILE-NAME       TO WS-TMP-FILE.
034800     PERFORM 901-REMOVE-TMP-FILE.
034900
035000 100-ERROR.
035100     CLOSE ERROR-FILE SAVE.
035200
035300     SORT ESORT-FILE
035400         ASCENDING KEY           DES-EMPLOYEE
035500         USING                   ERROR-FILE
035600         GIVING                  ERROR-FILE.
035700
035800     PERFORM 360-PRINT-ERRORS.
035900
036000     CLOSE ERROR-FILE.
036100     MOVE WS-ERRORFILE-NAME      TO WS-TMP-FILE.
036200     PERFORM 901-REMOVE-TMP-FILE.
036300
J69732     IF (PRM-EXCEPTION-REPORT    = "1" OR "2")
J69732         CLOSE EXCEPTION-FILE
J69732     END-IF.

J13588*    IF (EDCDWS-TRIGGER-ENABLED)
J13588     IF (LPBAL-LNK-WORKFILE-DONE)
J13588         PERFORM 9900-CLOSE-LPLNKPLAN
J13588         PERFORM 9990-REMOVE-LPLNKPLAN
J13588     END-IF.
J13588     PERFORM 9900-CLOSE-LPBALTRD.
J13588     PERFORM 9990-REMOVE-LPBALTRD.
J13588     PERFORM 9900-CLOSE-LPBALWORK.
J13588     PERFORM 9990-REMOVE-LPBALWORK.

036400 100-END.
036500     EXIT.
036600*****************************************************************
036700 200-EXTRACT-TIMERECORDS         SECTION.
036800*****************************************************************
036900 200-START.
037000
037100     MOVE WS-FALSE               TO WS-EMPLOYEE-FLAG.
037200     INITIALIZE                     WS-PAYCODE-DONE-SW
037300                                    WS-STD-EXISTS-SW
037400                                    WS-TG-EXISTS-SW.
037500
037600     MOVE PRM-COMPANY            TO DB-COMPANY.

           IF (PRM-PROC-GROUP          NOT = SPACES)
               MOVE PRM-COMPANY        TO DB-COMPANY
               MOVE PRM-PROC-GROUP     TO DB-PROC-GROUP
               MOVE PRPSET1-PROC-GROUP TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PRPSET1
               PERFORM 202-PROC-GROUP
                  UNTIL (PRPROCGRP-NOTFOUND)
               GO TO 200-END.

           IF  (WS-GROUP-SELECT)
P87754     AND (PRM-SELECTION NOT = "G" AND "A" 
P60416                          AND "D" AND "E")
           AND (PRM-PROCESS-LEVEL      = SPACES)
           AND (PRM-PROC-GROUP         = SPACES)
                MOVE 1                 TO WS-GROUP-ACCESS-SW
                PERFORM 205-GROUP-ACCESS
                VARYING I1 FROM 1 BY 1
                UNTIL (I1 > 10)
                GO TO 200-END.

           IF (WS-EMPLOYEE-SELECT)
               PERFORM 204-EMPLOYEES
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > 15)
                   GO TO 200-END.

      *81010 - NEXT 35 LINES.
           IF  (WS-GROUP-SELECT)
           AND (PRM-SELECTION = "G")
           AND (PRM-PROCESS-LEVEL      = SPACES)
           AND (PRM-PROC-GROUP         = SPACES)
               MOVE "G"  TO WS-SAVE-SELECTION

      * FOR "G" SELECTION PARAMETER, CREATE STANDARD TIME RECORDS.
               MOVE "S" TO PRM-SELECTION
               MOVE PRM-PROCESS-LEVEL          TO DB-PROCESS-LEVEL
               MOVE PRM-DEPARTMENT             TO DB-DEPARTMENT
        
               IF (PRM-DEPARTMENT NOT = SPACES)
                   MOVE EMPSET2-DEPARTMENT     TO WS-DB-BEG-RNG
               ELSE
                   IF (PRM-PROCESS-LEVEL NOT = SPACES) 
                       MOVE EMPSET2-PROCESS-LEVEL  TO WS-DB-BEG-RNG
                   ELSE
                       MOVE EMPSET2-COMPANY        TO WS-DB-BEG-RNG
                   END-IF
               END-IF
        
               PERFORM 850-FIND-BEGRNG-EMPSET2
        
               PERFORM 210-EXTRACT-TIMERECORDS
                   UNTIL (EMPLOYEE-NOTFOUND)
        
      * FOR "G" SELECTION PARAMETER, CREATE TIME GROUP TIME RECORDS.
               MOVE "T"           TO PRM-SELECTION
               MOVE 1             TO WS-GROUP-ACCESS-SW
               PERFORM 205-GROUP-ACCESS
                  VARYING I1 FROM 1 BY 1
                  UNTIL (I1 > 10)
               MOVE "G"           TO PRM-SELECTION
               GO TO 200-END
           END-IF.

037700     MOVE PRM-PROCESS-LEVEL      TO DB-PROCESS-LEVEL.
037800     MOVE PRM-DEPARTMENT         TO DB-DEPARTMENT.
037900
038000     IF (PRM-DEPARTMENT NOT = SPACES)
038100         MOVE EMPSET2-DEPARTMENT     TO WS-DB-BEG-RNG
038200     ELSE
038300     IF (PRM-PROCESS-LEVEL NOT = SPACES)
038400         MOVE EMPSET2-PROCESS-LEVEL  TO WS-DB-BEG-RNG
038500     ELSE
038600         MOVE EMPSET2-COMPANY        TO WS-DB-BEG-RNG.
038700
038800     PERFORM 850-FIND-BEGRNG-EMPSET2.
038900
039000     PERFORM 210-EXTRACT-TIMERECORDS
039100         UNTIL (EMPLOYEE-NOTFOUND).
039200
039300 200-END.
039400     EXIT.
036600*****************************************************************
036700 202-PROC-GROUP                  SECTION.
036800*****************************************************************
036900 202-START.
           MOVE PRP-PROCESS-LEVEL          TO DB-PROCESS-LEVEL.
           MOVE EMPSET2-PROCESS-LEVEL      TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-EMPSET2.
P13951     IF  (WS-EMPLOYEE-SELECT)
P13951        PERFORM 203-SELECT-EMPLOYEES
P13951            UNTIL (EMPLOYEE-NOTFOUND)
P13951     ELSE
              PERFORM 210-EXTRACT-TIMERECORDS
                  UNTIL (EMPLOYEE-NOTFOUND).

           PERFORM 860-FIND-NXTRNG-PRPSET1.
036900 202-END.
P13951*****************************************************************
P13951 203-SELECT-EMPLOYEES            SECTION.
P13951*****************************************************************
P13951 203-START.
P13951     PERFORM 
P13951         VARYING I1 FROM 1 BY 1
P13951         UNTIL (I1  >  15)
P13951         IF (EMP-EMPLOYEE   = PRM-EMPLOYEE (I1))
P13951             PERFORM 210-EXTRACT-TIMERECORDS
P13951         END-IF
P13951     END-PERFORM.
P13951 
P13951     PERFORM 860-FIND-NXTRNG-EMPSET2.
P13951   
P13951 203-END.
      *****************************************************************
       204-EMPLOYEES                   SECTION.
      *****************************************************************
       204-START.
           IF (PRM-EMPLOYEE (I1) NOT = ZEROES)
               MOVE PRM-EMPLOYEE (I1)      TO DB-EMPLOYEE
               PERFORM 840-FIND-EMPSET1
               IF (EMPLOYEE-FOUND)
                   PERFORM 210-EXTRACT-TIMERECORDS
               END-IF.
       204-END.
036600*****************************************************************
036700 205-GROUP-ACCESS                SECTION.
036800*****************************************************************
036900 205-START.
           IF (PRM-GROUP-NAME (I1) = SPACES)
               GO TO 205-END.
           MOVE PRM-GROUP-NAME (I1)        TO DB-GROUP-NAME.
           MOVE PGESET1-GROUP-NAME         TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PGESET1.
           PERFORM
               UNTIL (PGEMPLOYEE-NOTFOUND)
               MOVE PGE-EMPLOYEE           TO DB-EMPLOYEE
               PERFORM 840-FIND-EMPSET1
               IF (EMPLOYEE-FOUND)
                   PERFORM 210-EXTRACT-TIMERECORDS
               END-IF
               PERFORM 860-FIND-NXTRNG-PGESET1
           END-PERFORM.
039300 205-END.
039500******************************************************************
039600 210-EXTRACT-TIMERECORDS         SECTION.
039700******************************************************************
039800 210-START.
039900
040000*** THE EMPLOYEE GROUP(S) ALWAYS NARROWS DOWN WHICH EMPLOYEES *
040100*** WILL GET STANDARD, TIME GROUP, OR FLEX TIME RECORDS.      *
040200
      *81010 - IF CONDITION.
      *    IF (WS-SAVE-SELECTION NOT = "G")
040300     IF (PRM-GROUP-NAME  (1) NOT = SPACES)
040400     OR (PRM-GROUP-NAME  (2) NOT = SPACES)
040500     OR (PRM-GROUP-NAME  (3) NOT = SPACES)
040600     OR (PRM-GROUP-NAME  (4) NOT = SPACES)
040700     OR (PRM-GROUP-NAME  (5) NOT = SPACES)
           OR (PRM-GROUP-NAME  (6) NOT = SPACES)
           OR (PRM-GROUP-NAME  (7) NOT = SPACES)
           OR (PRM-GROUP-NAME  (8) NOT = SPACES)
           OR (PRM-GROUP-NAME  (9) NOT = SPACES)
           OR (PRM-GROUP-NAME (10) NOT = SPACES)
040800         MOVE PRM-COMPANY            TO DB-COMPANY
040900         MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE
041000         MOVE WS-FALSE               TO WS-GROUP-QUALIFY-SW
041100         PERFORM
041200             VARYING I2 FROM 1 BY 1
041300             UNTIL  (I2 > 10)
041400             OR     (GROUP-QUALIFIED)
041500             IF (PRM-GROUP-NAME (I2) NOT = SPACES)
041600                 MOVE PRM-GROUP-NAME (I2) TO DB-GROUP-NAME
041700                 PERFORM 840-KFIND-PGESET1
041800                 IF (PGEMPLOYEE-KFOUND)
041900                     MOVE WS-TRUE         TO WS-GROUP-QUALIFY-SW
042000                 END-IF
042100             END-IF
042200         END-PERFORM
042300         IF  (NOT-GROUP-QUALIFIED)
042400*         AND (PRM-SELECTION NOT = "A")
042500*         AND (PRM-SELECTION NOT = "G")
P39878         AND ((PRM-SELECTION     = "A" OR "G")
042500         OR   (WS-SAVE-SELECTION = "G"))
042600              GO TO 210-NEXT
           END-IF.
042700
042800     MOVE EMP-COMPANY            TO CRT-COMPANY.
042900     MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL.
043000     PERFORM 900-PROC-LEV-SECURED.
043100     IF (CRT-PROC-LEV-SECURED = "Y")
P82282        IF  (NOT-GROUP-ACCESS)
P82282        AND (WS-NO-EMPLOYEE-SELECT)
043200             PERFORM 860-FIND-NXTRNG-EMPSET2
043300                 UNTIL (EMPLOYEE-NOTFOUND)
043400                 OR    (EMP-COMPANY       NOT = PRM-COMPANY)
043500                 OR    (EMP-PROCESS-LEVEL NOT = CRT-PROCESS-LEVEL)
043600             GO TO 210-END.

J69732     IF (PRM-MID-PERIOD NOT = ZEROES)
J69732     AND (PRM-EXCEPTION-REPORT   NOT = 0)
J69732         MOVE PRM-COMPANY        TO DB-COMPANY
J69732         IF  (PRM-MID-PERIOD     = 1)
J69732             MOVE EMP-DATE-HIRED     TO PRTRD-HIRE-DATE
J69732         ELSE
J69732             MOVE EMP-ADJ-HIRE-DATE  TO PRTRD-HIRE-DATE
J69732         END-IF
J69732         MOVE EMP-OT-PLAN-CODE   TO DB-PLAN-CODE
J17912         MOVE SPACES                 TO DB-PROCESS-LEVEL
J17912         PERFORM 840-FIND-PRSSET1
J17912         IF (PRS-WORK-COUNTRY = "SA" OR "AE" OR "QA")
J17912             MOVE PRS-PAYROLL-YEAR     TO DB-PAYROLL-YEAR
J17912             MOVE R1DSET1-PAYROLL-YEAR TO WS-DB-BEG-RNG
J17912             INITIALIZE FILTER-STRING
J17912             STRING "((R1D-PER-START-DATE <= ?) AND "
J17912                    "(R1D-PER-END-DATE >= ?))"
J17912                    DELIMITED BY SIZE INTO FILTER-STRING
J17912             PERFORM 890-CREATE-FILTER
J17912             MOVE PRM-TR-DATE        TO DATETIME-FILTER-VALUE
J17912             PERFORM 890-SET-DATETIME-FILTER-VALUE
J17912             MOVE PRM-TR-DATE        TO DATETIME-FILTER-VALUE
J17912             PERFORM 890-SET-DATETIME-FILTER-VALUE
J17912             PERFORM 850-FILTER-BEGRNG-R1DSET1
039994             IF (R1PLANDTL-FOUND)
J17912                 IF (PRTRD-HIRE-DATE >= R1D-PER-START-DATE)
J17912                     IF (PRM-EXCEPTION-REPORT   = 1)
J17912                         IF (PRTRD-HIRE-DATE > R1D-PER-END-DATE)
J17912                             MOVE 1 TO PD-FUTURE
J17912                         ELSE
J17912                             MOVE 0 TO PD-FUTURE
J17912                         END-IF
J17912                         MOVE EMP-EMPLOYEE     TO PD-EMPLOYEE 
J17912                         MOVE EMP-LAST-NAME    TO HRWS-LAST-NAME
J17912                         MOVE EMP-FIRST-NAME   TO HRWS-FIRST-NAME
J17912                         MOVE EMP-MIDDLE-INIT  TO HRWS-MIDDLE-INIT
J17912                         MOVE EMP-NAME-SUFFIX  TO HRWS-NAME-SUFFIX
J17912                         MOVE EMP-LAST-NAME-PRE
J17912                                         TO HRWS-LAST-NAME-PRE
J17912                         PERFORM 750-HR-FORMAT-NAME
J17912                         MOVE HRWS-FORMAT-NAME(1:40)
J17912                                         TO PD-FULL-NAME
J17912                         MOVE PRTRD-HIRE-DATE  TO PD-HIRE-DATE 
J17912                         MOVE PARTIAL-DETAIL   
J17912                                              TO RPT-GROUP-REQUEST
J17912                         PERFORM 700-PRINT-RPT-GRP 
J17912                         GO TO 210-NEXT
J17912                     ELSE
J17912                         IF (PRTRD-HIRE-DATE <= R1D-PER-END-DATE)
J17912                             MOVE EMP-EMPLOYEE
J17912                                         TO PD-EMPLOYEE 
J17912                             MOVE PRTRD-HIRE-DATE
J17912                                         TO PD-HIRE-DATE 
J17912                             MOVE EMP-LAST-NAME
J17912                                         TO HRWS-LAST-NAME
J17912                             MOVE EMP-FIRST-NAME
J17912                                         TO HRWS-FIRST-NAME
J17912                             MOVE EMP-MIDDLE-INIT
J17912                                         TO HRWS-MIDDLE-INIT
J17912                             MOVE EMP-NAME-SUFFIX
J17912                                         TO HRWS-NAME-SUFFIX
J17912                             MOVE EMP-LAST-NAME-PRE
J17912                                         TO HRWS-LAST-NAME-PRE
J17912                             PERFORM 750-HR-FORMAT-NAME
J17912                             MOVE HRWS-FORMAT-NAME(1:23)
J17912                                         TO PD-FULL-NAME
J17912                             MOVE PARTIAL-DETAIL
J17912                                         TO RPT-GROUP-REQUEST 
J17912                             PERFORM 700-PRINT-RPT-GRP 
J17912                             GO TO 210-NEXT
J17912                         END-IF
J17912                     END-IF
J17912                 END-IF
039994             END-IF
J17912         ELSE
J69732             MOVE ZEROES             TO DB-EFFECT-DATE
J69732                                        DB-PAY-START-DATE
J69732             PERFORM 850-FIND-NLT-PRYSET1
J69732             PERFORM
J69732                UNTIL (PROTPAYPRD-NOTFOUND)
J69732                OR    (PRY-COMPANY        NOT = DB-COMPANY)
J69732                OR    (PRY-PLAN-CODE      NOT = DB-PLAN-CODE)
J69732                OR    (PRY-PAY-START-DATE > PRM-TR-DATE)
J69732                 IF  (PRM-TR-DATE        >= PRY-PAY-START-DATE)
J69732                 AND (PRM-TR-DATE        <= PRY-PAY-END-DATE)
J69732                    IF (PRTRD-HIRE-DATE >= PRY-PAY-START-DATE)
J69732                       IF (PRM-EXCEPTION-REPORT   = 1)
J69732                         IF (PRTRD-HIRE-DATE > PRY-PAY-END-DATE)
J69732                             MOVE 1 TO PD-FUTURE
J69732                         ELSE
J69732                             MOVE 0 TO PD-FUTURE
J69732                         END-IF
J69732                         MOVE EMP-EMPLOYEE     TO PD-EMPLOYEE 
J69732                         MOVE EMP-LAST-NAME    TO HRWS-LAST-NAME
J69732                         MOVE EMP-FIRST-NAME   TO HRWS-FIRST-NAME
J69732                         MOVE EMP-MIDDLE-INIT  TO HRWS-MIDDLE-INIT
J69732                         MOVE EMP-NAME-SUFFIX  TO HRWS-NAME-SUFFIX
J69732                         MOVE EMP-LAST-NAME-PRE
J69732                                           TO HRWS-LAST-NAME-PRE
J69732                         PERFORM 750-HR-FORMAT-NAME
J69732                         MOVE HRWS-FORMAT-NAME(1:40)
J69732                                         TO PD-FULL-NAME
J69732                         MOVE PRTRD-HIRE-DATE  TO PD-HIRE-DATE 
J69732                         MOVE PARTIAL-DETAIL  TO RPT-GROUP-REQUEST
J69732                         PERFORM 700-PRINT-RPT-GRP 
J69732                         GO TO 210-NEXT
J69732                       ELSE
J69732                         IF (PRTRD-HIRE-DATE <= PRY-PAY-END-DATE)
J69732                             MOVE EMP-EMPLOYEE
J69732                                         TO PD-EMPLOYEE 
J69732                             MOVE PRTRD-HIRE-DATE
J69732                                         TO PD-HIRE-DATE 
J69732                             MOVE EMP-LAST-NAME
J69732                                         TO HRWS-LAST-NAME
J69732                             MOVE EMP-FIRST-NAME
J69732                                         TO HRWS-FIRST-NAME
J69732                             MOVE EMP-MIDDLE-INIT
J69732                                         TO HRWS-MIDDLE-INIT
J69732                             MOVE EMP-NAME-SUFFIX
J69732                                         TO HRWS-NAME-SUFFIX
J69732                             MOVE EMP-LAST-NAME-PRE
J69732                                         TO HRWS-LAST-NAME-PRE
J69732                             PERFORM 750-HR-FORMAT-NAME
J69732                             MOVE HRWS-FORMAT-NAME(1:23)
J69732                                         TO PD-FULL-NAME
J69732                             MOVE PARTIAL-DETAIL
J69732                                         TO RPT-GROUP-REQUEST 
J69732                             PERFORM 700-PRINT-RPT-GRP 
J69732                                 GO TO 210-NEXT
J69732                         END-IF
J69732                       END-IF
J69732                    END-IF
J69732                 END-IF
J69732                 PERFORM 860-FIND-NEXT-PRYSET1
J69732             END-PERFORM
J17912         END-IF
           END-IF.
043700
043800     INITIALIZE PRTRD-SCR-FIELDS.
043900
044000     MOVE EMP-COMPANY            TO PRTRD-COMPANY.
044100     MOVE EMP-EMPLOYEE           TO PRTRD-EMPLOYEE.
044200     MOVE "A"                    TO PRTRD-FC.
044300     MOVE "Y"                    TO PRTRD-BATCH-PROGRAM.
044400
044500     IF  (PRM-SELECTION     = "S" OR "B" OR "P" OR
044600                              "A" OR "G" OR "F" 
P60416                           OR "D" OR "E")
044700         MOVE PRM-COMPANY        TO DB-COMPANY
044800         MOVE EMP-EMPLOYEE       TO DB-EMPLOYEE
044900         MOVE STMSET1-EMPLOYEE   TO WS-DB-BEG-RNG
045000         PERFORM 850-FIND-BEGRNG-STMSET1
045100         IF  (STANDTIME-NOTFOUND)
045200         AND (PRM-SELECTION     NOT = "P" AND "F")
045300         AND (EMP-AUTO-TIME-REC = "S" OR "Y")
045400             NEXT SENTENCE
045500         ELSE
045600         IF  (PRM-SELECTION     = "P")
045700         AND (EMP-AUTO-TIME-REC = "S" OR "Y")
045800         AND (PC-ONLY-FOR-STD)
045900             MOVE WS-FALSE       TO WS-STM-QUALIFY-SW
046000             MOVE "N"            TO WS-P-TIME-GRP
046100             PERFORM 244-GET-STM-FOR-P
046200                 UNTIL (STANDTIME-NOTFOUND)
046300                 OR    (STM-QUALIFIED)
046400             IF (STM-QUALIFIED)
046500                 PERFORM 250-EXTRACT-PAYCODE-TR
046600             END-IF
046700         ELSE
046800         IF (PRM-SELECTION = "S" OR "B" OR "A" OR "G" OR "F"
P60416                          OR "D" OR "E")
046900             PERFORM 230-EXTRACT-STANDARD-TR
047000                 UNTIL (STANDTIME-NOTFOUND)
047100             IF  (PAYCODE-NOT-DONE)
047200             AND (STD-EXISTS)
047300                 PERFORM 250-EXTRACT-PAYCODE-TR.
047400
047500     IF  (PRM-SELECTION     = "T" OR "G" OR "P" OR "A"
P60416                           OR "D" OR "E" )
047600     AND (EMP-AUTO-TIME-REC = "T" OR "Y")
047700         PERFORM 240-EXTRACT-PERS-GROUP-TR.
047800
047900 210-NEXT.
048000     IF  (NOT-GROUP-ACCESS)
           AND (WS-NO-EMPLOYEE-SELECT)
048100         PERFORM 860-FIND-NXTRNG-EMPSET2.
048200     INITIALIZE                  WS-PAYCODE-DONE-SW
048300                                 WS-STD-EXISTS-SW
048400                                 WS-TG-EXISTS-SW.
048500
048600 210-END.
048700     EXIT.
048800******************************************************************
048900 230-EXTRACT-STANDARD-TR          SECTION.
049000******************************************************************
049100 230-START.
049200
049300     IF  (PRM-PAY-FREQUENCY NOT = ZEROS)
049400     AND (EMP-PAY-FREQUENCY NOT = PRM-PAY-FREQUENCY)
049500         GO TO 230-NEXT.
049600
049700     IF  (EMP-AUTO-TIME-REC NOT = "S" AND "Y")
049800     AND (STM-FLEX-DOLLARS  = ZEROS)
049900         GO TO 230-NEXT.
050000
050100     IF  (PRM-SELECTION = "F")
050200     AND (STM-FLEX-DOLLARS = ZEROS)
050300         GO TO 230-NEXT.
050400
050500     IF  (PRM-SELECTION = "S")
050600     AND (STM-FLEX-DOLLARS NOT = ZEROS)
050700         GO TO 230-NEXT.
050800
050900     IF  (STM-FLEX-DOLLARS NOT = ZEROS)
051000         IF  (STM-EFFECT-DATE > PRM-FLEX-DATE)
051100         OR  (STM-END-DATE    < PRM-FLEX-DATE)
051200             GO TO 230-NEXT
051300         END-IF
051400     ELSE
753953*        IF  (PRS-WORK-COUNTRY NOT = "SA" AND "AE" AND "QA")
753953         IF  (STM-DAILY-TR-FLG = 0)
051500             IF  (STM-EFFECT-DATE NOT = ZEROS)
051600             AND (STM-EFFECT-DATE > PRM-TR-DATE)
051700                 GO TO 230-NEXT
051800             END-IF
051900             IF  (STM-END-DATE NOT = ZEROS)
052000             AND (STM-END-DATE < PRM-TR-DATE)
052100                 GO TO 230-NEXT  
725370             END-IF
725370          END-IF 
725370     END-IF.
052200
052300     IF (STM-DED-CYCLE (PRM-DED-CYCLE) = SPACE)
052400         GO TO 230-NEXT.
052500
      * CALC-TYPE=C WILL HAVE ZERO HOURS AND RATE.
      * OTHER STD RECORDS SHOULD HAVE RATE OR HOURS, NO REASON
      * TO SKIP.
052600*     IF  (STM-HOURS = ZEROS)
052700*     AND (STM-RATE  = ZEROS)
052800*         GO TO 230-NEXT.

      *--- Fix for PT126743. We don't want to create TimeRecord if the
      *--- rate is zero and StandTime was created thru Flex
052600     IF  (PRM-SELECTION    = "F" OR "B" OR "A"
P60416                         OR "E")
           AND (STM-FLEX-DOLLARS > ZEROES)
052700     AND (STM-RATE         = ZEROS)
      *------- Employee time record not created, rate is zero
               MOVE EMP-EMPLOYEE       TO ER-EMPLOYEE
               MOVE 141                TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE        TO ER-MESSAGE
               WRITE ERROR-REC FROM ER-ERROR-REC
059000         MOVE "Y"                TO WS-ERROR-FOUND
052800         GO TO 230-NEXT.
      *--- End fix for PT126743

           IF  (STM-CURRENCY-CODE NOT = SPACES)
           AND (STM-CURRENCY-CODE NOT = EMP-CURRENCY-CODE)
               MOVE EMP-EMPLOYEE       TO ER-EMPLOYEE
               MOVE 128                TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE        TO ER-MESSAGE
               WRITE ERROR-REC FROM ER-ERROR-REC
059000         MOVE "Y"                TO WS-ERROR-FOUND
052800         GO TO 230-NEXT.

MG0325     IF (PRM-SPECIAL-RUN = "Y")
MG0325         MOVE 0                   TO WS-BYPASS-CG-SW
MG0325         PERFORM
MG0325             VARYING I3 FROM 1 BY 1
MG0325             UNTIL  (I3 > 9) OR
MG0325                    (BYPASS-CG)
MG0325                 IF (PRM-CHECK-GROUP (I3) > SPACES)
MG0325                      IF (STM-CHECK-GRP = PRM-CHECK-GROUP (I3))
MG0325                         MOVE 1   TO WS-BYPASS-CG-SW
MG0325                     END-IF
MG0325                 END-IF
MG0325         END-PERFORM
MG0325         IF (BYPASS-CG)
MG0325             GO TO 230-NEXT
MG0325         END-IF
MG0325     END-IF.
MG0325

J17912     IF (PRS-WORK-COUNTRY = "SA" OR "AE" OR "QA")
J17912         MOVE PRM-COMPANY          TO DB-COMPANY
J17912         MOVE SPACES               TO DB-PROCESS-LEVEL
J17912         PERFORM 840-FIND-PRSSET1
J17912         MOVE STM-EMPLOYEE         TO DB-EMPLOYEE
J17912         PERFORM 840-FIND-EMPSET1
J17912         MOVE EMP-OT-PLAN-CODE     TO PRTRD-OT-PLAN-CODE
J17912         IF  (PRTRD-OT-PLAN-CODE      = SPACES)
J17912         AND (EMP-PAY-FREQUENCY NOT = ZEROS)
J17912             MOVE EMP-PROCESS-LEVEL TO DB-PROCESS-LEVEL
J17912             PERFORM 840-FIND-PRSSET1
J17912             MOVE PRS-OT-PLAN-CODE (EMP-PAY-FREQUENCY)
J17912                                     TO PRTRD-OT-PLAN-CODE
J17912         END-IF
J17912         MOVE STM-EMPLOYEE         TO DB-EMPLOYEE
J17912         MOVE PRTRD-OT-PLAN-CODE   TO DB-PLAN-CODE
J17912         MOVE PRS-PAYROLL-YEAR     TO DB-PAYROLL-YEAR
J17912         MOVE R1DSET1-PAYROLL-YEAR TO WS-DB-BEG-RNG
J17912         INITIALIZE FILTER-STRING
J17912         STRING "((R1D-PER-START-DATE <= ?) AND "
J17912                "(R1D-PER-END-DATE >= ?))"
J17912                DELIMITED BY SIZE INTO FILTER-STRING
J17912         PERFORM 890-CREATE-FILTER
J17912         MOVE PRM-TR-DATE
J17912           TO DATETIME-FILTER-VALUE
J17912         PERFORM 890-SET-DATETIME-FILTER-VALUE
J17912         MOVE PRM-TR-DATE
J17912           TO DATETIME-FILTER-VALUE
J17912         PERFORM 890-SET-DATETIME-FILTER-VALUE
J17912         PERFORM 850-FILTER-BEGRNG-R1DSET1
J17912         IF (R1PLANDTL-NOTFOUND)
J17912              MOVE "PRTRD"    TO CRT-ERROR-CAT
J17912              MOVE 130        TO CRT-ERROR-NBR
J17912              PERFORM 780-PRINT-ERROR-MSG
J17912              GO TO 230-NEXT
J17912         END-IF
J17912     END-IF.

053000
053100     INITIALIZE PRTRD-SCR-FIELDS.

053200
053300     MOVE "A"                    TO PRTRD-FC.
053400     MOVE STM-COMPANY            TO PRTRD-COMPANY.
053500     MOVE PRM-BATCH-NBR          TO PRTRD-BATCH-NBR.
053600     MOVE EMP-EMPLOYEE           TO PRTRD-EMPLOYEE.
           MOVE STM-EMPLOYEE           TO PRTRD-EMPLOYEE.
053700     MOVE STM-POSITION           TO PRTRD-POSITION.
053800     MOVE STM-JOB-CODE           TO PRTRD-JOB-CODE.
053900     MOVE STM-PAY-CODE           TO PRTRD-PAY-CODE.
054000     MOVE STM-HOURS              TO PRTRD-HOURS.
054100     MOVE STM-RATE               TO PRTRD-RATE.
           MOVE STM-CHECK-GRP          TO PRTRD-CHECK-GRP.
           IF  (PRTRD-CHECK-GRP = SPACES)
               MOVE PRM-CHECK-GRP      TO PRTRD-CHECK-GRP.
           MOVE STM-PROCESS-GRP        TO PRTRD-PROCESS-GRP.
           IF  (PRTRD-PROCESS-GRP = SPACES)
               MOVE PRM-PROCESS-GRP    TO PRTRD-PROCESS-GRP.
054400     MOVE PRM-TR-DATE            TO PRTRD-TR-DATE.
054500     MOVE STM-PROCESS-LEVEL      TO PRTRD-DIST-PROC-LEV.
054600     MOVE STM-DEPARTMENT         TO PRTRD-DIST-DEPART.
054700     MOVE STM-DIST-COMPANY       TO PRTRD-DIST-COMPANY.
054800     MOVE STM-DST-ACCT-UNIT      TO PRTRD-DST-ACCT-UNIT.
054900     MOVE STM-DST-ACCOUNT        TO PRTRD-DST-ACCOUNT.
055000     MOVE STM-DST-SUB-ACCT       TO PRTRD-DST-SUB-ACCT.
055100     MOVE STM-ACTIVITY           TO PRTRD-ACTIVITY.
055200     MOVE STM-ACCT-CATEGORY      TO PRTRD-ACCT-CATEGORY.
LP         MOVE STM-REASON-CODE        TO PRTRD-REASON-CODE.
055300     MOVE STM-ATTEND-CODE        TO PRTRD-ATTEND-CODE.
055400     MOVE STM-OCCURRENCE         TO PRTRD-OCCURRENCE.
           MOVE STM-BUS-NBR-GRP        TO PRTRD-BUS-NBR-GRP.
           MOVE STM-QC-ENT-NBR-GRP     TO PRTRD-QC-ENT-NBR-GRP.
           MOVE STM-LOCAT-CODE         TO PRTRD-LOCAT-CODE.
           MOVE STM-TAX-FREQ-OVER      TO PRTRD-TAX-FREQ-OVER.
           MOVE STM-PENS-SEQ-NBR       TO PRTRD-PENS-SEQ-NBR.
J17912     MOVE STM-DAILY-TR-FLG       TO PRTRD-DAILY-TR-FLG.
           IF  (PRTRD-PENS-SEQ-NBR NOT = ZEROES)
               MOVE PRTRD-COMPANY      TO DB-COMPANY
               MOVE PRTRD-EMPLOYEE     TO DB-EMPLOYEE
               MOVE PRTRD-PENS-SEQ-NBR TO DB-PENS-SEQ-NBR
               PERFORM 840-FIND-PNPSET2
               IF  (PRPENPAY-FOUND)
                   MOVE PNP-PENS-DIST-TYPE TO PRTRD-PENS-DIST-TYPE-SW
               END-IF
           END-IF.
055500
055600     IF (STM-FLEX-DOLLARS NOT = ZEROS)
055700         MOVE "F"                TO PRTRD-OT-RECORD.
055800

055900     MOVE "Y"                    TO PRTRD-BATCH-PROGRAM.
056000
056100     INITIALIZE HREMP-SCR-FIELDS.
056200     PERFORM 2000-PRTRD-EMP-EDIT-TRAN.
056300
056400     IF (PRTRD-I3 NOT = ZEROS)
056500         PERFORM
056600             VARYING I2 FROM 1 BY 1
056700             UNTIL  (I2 > PRTRD-I3)
056800
056900              MOVE PRTRD-MESSAGE (I2) TO ER-MESSAGE
057000              MOVE EMP-EMPLOYEE TO ER-EMPLOYEE
057100              WRITE ERROR-REC FROM ER-ERROR-REC
057200              MOVE "Y"           TO WS-ERROR-FOUND
057300         END-PERFORM
057400         GO TO 230-NEXT.
057500
057600     MOVE 1                      TO PRTRD-DST-ACCT-UNIT-FN
057700                                    PRTRD-ACTIVITY-FN
057800                                    PRTRD-ACCT-CATEGORY-FN
                                          PRTRD-DIST-PROC-LEV-FN
                                          PRTRD-DIST-DEPART-FN
                                          PRTRD-BUS-NBR-GRP-FN
                                          PRTRD-QC-ENT-NBR-GRP-FN.

J17912
J17912     IF (PRTRD-DAILY-TR-FLG = 1)
J17912         PERFORM 303-GET-DLY-END-DT
J17912         MOVE PRTRD-COMPANY  TO DB-COMPANY
J17912         MOVE PRTRD-EMPLOYEE TO DB-EMPLOYEE
J17912         PERFORM 840-FIND-EMPSET1
J77488         IF (EMP-DATE-HIRED > STM-EFFECT-DATE)
J17912             MOVE EMP-DATE-HIRED    TO PRTRD-DAILY-TR-DATE
J17912                                       PRTRD-TR-DATE
J17912         ELSE  
J17912             MOVE STM-EFFECT-DATE   TO PRTRD-DAILY-TR-DATE
J17912                                       PRTRD-TR-DATE
J17912         END-IF
J11610         IF (R1D-PER-START-DATE > PRTRD-DAILY-TR-DATE)
J11610             MOVE R1D-PER-START-DATE TO PRTRD-DAILY-TR-DATE
J11610                                        PRTRD-TR-DATE
J11610         END-IF
J17912     ELSE
J17912         MOVE PRTRD-TR-DATE TO PRTRD-DAILY-TR-DATE
J17912                               PRTRD-DAILY-TR-END-DATE
J17912     END-IF.
J17912
J17912     PERFORM 231-GENERATE-TIMERECORDS
J17912     UNTIL   (PRTRD-DAILY-TR-DATE > PRTRD-DAILY-TR-END-DATE).
J17912
059800 230-NEXT.
059900
060000     PERFORM 860-FIND-NXTRNG-STMSET1.
060100
060200 230-END.
060300     EXIT.
J17912******************************************************************
J17912 231-GENERATE-TIMERECORDS         SECTION.
J17912******************************************************************
J17912 231-START.
J17912     IF (PRTRD-DAILY-TR-FLG = 1)
J17912         INITIALIZE PRTRD-HOURS
J17912         MOVE PRTRD-COMPANY       TO DB-COMPANY
J17912         MOVE PRTRD-OT-PLAN-CODE  TO DB-PLAN-CODE
J17912         MOVE PRTRD-PAYROLL-YEAR  TO DB-PAYROLL-YEAR
J17912         PERFORM 840-FIND-R1PSET1
J17912         MOVE PRTRD-TR-DATE       TO WSDR-FR-DATE
J17912         PERFORM 900-DAY-FROM-DATE
J17912         IF (R1P-FIXED-STD-HRS = ZEROES)
J17912             MOVE R1P-STD-HR-PER-DAY (WSDR-WEEKDAY-NBR)
J17912                                  TO PRTRD-HOURS
J17912         ELSE
J17912             MOVE R1P-FIXED-STD-HRS TO PRTRD-HOURS
J17912         END-IF
J17912     END-IF.

057900     PERFORM 2000-PRTRD-EDIT-TRAN.
058000
058100     IF ((PRTRD-I3 NOT = ZEROS)
J69732     OR ((PRM-MID-PERIOD = 1 OR 2)
J63759     AND (PRTRD-PARTIAL)))
058300         PERFORM
058400             VARYING I2 FROM 1 BY 1
058500             UNTIL  (I2 > PRTRD-I3)
058600
058700             MOVE EMP-EMPLOYEE       TO ER-EMPLOYEE
058800             MOVE PRTRD-MESSAGE (I2) TO ER-MESSAGE
058900             WRITE ERROR-REC         FROM ER-ERROR-REC
059000             MOVE "Y"                TO WS-ERROR-FOUND
059100         END-PERFORM
               IF ((PRTRD-ERROR-FOUND)
J69732         OR ((PRM-MID-PERIOD = 1 OR 2)
J63759         AND (PRTRD-PARTIAL)))
                   GO TO 231-CONTINUE
617501         END-IF
617501     END-IF.

J69732     IF (PRM-MID-PERIOD NOT = 0)
J63759     AND (PRTRD-PARTIAL)
J69732     AND (PRM-EXCEPTION-REPORT   NOT = 0)
J63759          MOVE EMP-EMPLOYEE          TO PD-EMPLOYEE 
J63759          MOVE PRTRD-HIRE-DATE       TO PD-HIRE-DATE 
J63759          MOVE PARTIAL-DETAIL        TO RPT-GROUP-REQUEST 
J63759          PERFORM 700-PRINT-RPT-GRP 
J63759          GO TO 231-CONTINUE
J63759     END-IF.
059300
059400     PERFORM 299-EXTRACT-TIMERECORD.
059500     MOVE WS-TRUE                TO WS-EMPLOYEE-FLAG
059600                                    WS-STD-EXISTS-SW.
059700
J11610     MOVE STM-RATE TO PRTRD-RATE.
J79159     INITIALIZE PRTRD-ANNUAL-SALARY.
J11610
J17912 231-CONTINUE.
J17912
J17912     IF (PRTRD-DAILY-TR-FLG = 1)
J17912         MOVE PRTRD-DAILY-TR-DATE TO WSDR-FR-DATE
J17912         PERFORM 900-DATE-TO-JULIAN
J17912         ADD 1 TO WSDR-JULIAN-DAYS
J17912         PERFORM 900-JULIAN-TO-DATE
J17912         MOVE WSDR-FR-DATE TO PRTRD-DAILY-TR-DATE
J17912                              PRTRD-TR-DATE
J17912     ELSE
J17912         MOVE PRTRD-DAILY-TR-END-DATE TO WSDR-FR-DATE
J17912         PERFORM 900-DATE-TO-JULIAN
J17912         ADD 1 TO WSDR-JULIAN-DAYS
J17912         PERFORM 900-JULIAN-TO-DATE
J17912         MOVE WSDR-FR-DATE TO PRTRD-DAILY-TR-DATE
J17912     END-IF.
J17912
J17912 231-END.
J17912     EXIT.
060400******************************************************************
060500 240-EXTRACT-PERS-GROUP-TR        SECTION.
060600******************************************************************
060700 240-START.
060800
060900     IF  (PRM-PAY-FREQUENCY NOT = ZEROS)
061000     AND (EMP-PAY-FREQUENCY NOT = PRM-PAY-FREQUENCY)
061100         GO TO 240-END.
061200
           IF (GROUP-ACCESS)
               PERFORM 241-DO-PERS-GROUPS
           ELSE
061300         PERFORM 241-DO-PERS-GROUPS
061400             VARYING I1 FROM 1 BY 1
061500             UNTIL  (I1 > 10).
061600
061700 240-END.
061800     EXIT.
061900******************************************************************
062000 241-DO-PERS-GROUPS             SECTION.
062100******************************************************************
062200 241-START.
062300
062400     IF (PRM-GROUP-NAME (I1) = SPACES)
062500         GO TO 241-END.
062600
           IF (NOT-GROUP-ACCESS)
062700         MOVE PRM-GROUP-NAME (I1)    TO DB-GROUP-NAME
062800         MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE
062900         PERFORM 840-KFIND-PGESET1
063000         IF (PGEMPLOYEE-KNOTFOUND)
063100             GO TO 241-END.

063300     MOVE PRM-COMPANY            TO DB-COMPANY.
063400     MOVE PRM-GROUP-NAME (I1)    TO DB-TIME-GROUP.
063500     MOVE STMSET3-TIME-GROUP     TO WS-DB-BEG-RNG.
063600     PERFORM 850-FIND-BEGRNG-STMSET3.
063700
063800     IF  (STANDTIME-NOTFOUND)
063900     AND (PRM-SELECTION  NOT = "P")
064000         GO TO 241-END
064100     ELSE
064200     IF  (PRM-SELECTION = "P")
064300     AND (STANDTIME-FOUND)
064400     AND (PC-ONLY-FOR-TG)
064500         MOVE WS-FALSE           TO WS-STM-QUALIFY-SW
064600         MOVE "Y"                TO WS-P-TIME-GRP
064700         PERFORM 244-GET-STM-FOR-P
064800         IF  (STM-QUALIFIED)
064900         AND (PAYCODE-NOT-DONE)
065000             PERFORM 250-EXTRACT-PAYCODE-TR
065100         END-IF
065200     ELSE
065300     IF (PRM-SELECTION = "T" OR "A" OR "G"
P60416                      OR "D" OR "E")
065400         PERFORM 242-EXTRACT-TG-TR
065500             UNTIL (STANDTIME-NOTFOUND)
065600         IF  (PAYCODE-NOT-DONE)
065700         AND (TG-EXISTS)
065800             PERFORM 250-EXTRACT-PAYCODE-TR.
065900
066000 241-END.
066100     EXIT.
066200******************************************************************
066300 242-EXTRACT-TG-TR              SECTION.
066400******************************************************************
066500 242-START.
066600
066700     IF  (PRM-PAY-FREQUENCY NOT = ZEROS)
066800     AND (EMP-PAY-FREQUENCY NOT = PRM-PAY-FREQUENCY)
066900         GO TO 242-NEXT.
067000
753953*    IF  (PRS-WORK-COUNTRY NOT = "SA" AND "AE" AND "QA")
753953     IF  (STM-DAILY-TR-FLG = 0)
067100         IF  (STM-EFFECT-DATE NOT = ZEROS)
067200         AND (STM-EFFECT-DATE > PRM-TR-DATE)
067300             GO TO 242-NEXT 
725370         END-IF
067400
067500         IF  (STM-END-DATE NOT = ZEROS)
067600         AND (STM-END-DATE < PRM-TR-DATE)
067700             GO TO 242-NEXT 
725370         END-IF
725370     END-IF.
067800
067900     IF (STM-DED-CYCLE (PRM-DED-CYCLE) = SPACE)
068000         GO TO 242-NEXT.
068100
      * CALC-TYPE=C WILL HAVE ZERO HOURS AND RATE.
      * OTHER STD RECORDS SHOULD HAVE RATE OR HOURS, NO REASON
      * TO SKIP.
068200*     IF  (STM-HOURS = ZEROS)
068300*     AND (STM-RATE  = ZEROS)
068400*         GO TO 242-NEXT.

           IF  (STM-CURRENCY-CODE NOT = SPACES)
           AND (STM-CURRENCY-CODE NOT = EMP-CURRENCY-CODE)
               MOVE EMP-EMPLOYEE       TO ER-EMPLOYEE
               MOVE 128                TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE        TO ER-MESSAGE
               WRITE ERROR-REC FROM ER-ERROR-REC
052800         GO TO 242-NEXT.
068500
068600     INITIALIZE PRTRD-SCR-FIELDS.
068700     MOVE EMP-COMPANY            TO PRTRD-COMPANY.
068800     MOVE EMP-EMPLOYEE           TO PRTRD-EMPLOYEE.
068900     MOVE "A"                    TO PRTRD-FC.
069000     MOVE "Y"                    TO PRTRD-BATCH-PROGRAM.
069100     INITIALIZE HREMP-SCR-FIELDS.
069200     PERFORM 2000-PRTRD-EMP-EDIT-TRAN.
069300
069400     IF (PRTRD-I3 NOT = ZEROS)
069500         PERFORM
069600             VARYING I2 FROM 1 BY 1
069700             UNTIL  (I2 > PRTRD-I3)
069800
069900             MOVE EMP-EMPLOYEE       TO ER-EMPLOYEE
070000             MOVE PRTRD-MESSAGE (I2) TO ER-MESSAGE
070100             WRITE ERROR-REC         FROM ER-ERROR-REC
070200             MOVE "Y"                TO WS-ERROR-FOUND
070300         END-PERFORM
070400         GO TO 242-NEXT.
070500
070600
070700     INITIALIZE PRTRD-SCR-FIELDS.
070800
070900     MOVE "A"                    TO PRTRD-FC.
071000     MOVE STM-COMPANY            TO PRTRD-COMPANY.
071100     MOVE PRM-BATCH-NBR          TO PRTRD-BATCH-NBR.
071200     MOVE EMP-EMPLOYEE           TO PRTRD-EMPLOYEE.
071300     MOVE STM-POSITION           TO PRTRD-POSITION.
071400     MOVE STM-JOB-CODE           TO PRTRD-JOB-CODE.
071500     MOVE STM-PAY-CODE           TO PRTRD-PAY-CODE.
071600     MOVE STM-HOURS              TO PRTRD-HOURS.
071700     MOVE STM-RATE               TO PRTRD-RATE.
           IF (STM-CHECK-GRP NOT = SPACES)
               MOVE STM-CHECK-GRP      TO PRTRD-CHECK-GRP
           ELSE
               MOVE PRM-CHECK-GRP      TO PRTRD-CHECK-GRP.
P38444     IF (STM-PROCESS-GRP NOT = SPACES)
               MOVE STM-PROCESS-GRP    TO PRTRD-PROCESS-GRP
           ELSE
               MOVE PRM-PROCESS-GRP    TO PRTRD-PROCESS-GRP.
072000     MOVE PRM-TR-DATE            TO PRTRD-TR-DATE.
072100     MOVE STM-PROCESS-LEVEL      TO PRTRD-DIST-PROC-LEV.
072200     MOVE STM-DEPARTMENT         TO PRTRD-DIST-DEPART.
072300     MOVE STM-DIST-COMPANY       TO PRTRD-DIST-COMPANY.
072400     MOVE STM-DST-ACCT-UNIT      TO PRTRD-DST-ACCT-UNIT.
072500     MOVE STM-DST-ACCOUNT        TO PRTRD-DST-ACCOUNT.
072600     MOVE STM-DST-SUB-ACCT       TO PRTRD-DST-SUB-ACCT.
072700     MOVE STM-ACTIVITY           TO PRTRD-ACTIVITY.
072800     MOVE STM-ACCT-CATEGORY      TO PRTRD-ACCT-CATEGORY.
LP         MOVE STM-REASON-CODE        TO PRTRD-REASON-CODE.
072900     MOVE STM-ATTEND-CODE        TO PRTRD-ATTEND-CODE.
073000     MOVE STM-OCCURRENCE         TO PRTRD-OCCURRENCE.
           MOVE STM-LOCAT-CODE         TO PRTRD-LOCAT-CODE.
           MOVE STM-TAX-FREQ-OVER      TO PRTRD-TAX-FREQ-OVER.
           MOVE STM-PENS-SEQ-NBR       TO PRTRD-PENS-SEQ-NBR.
J17912     MOVE STM-DAILY-TR-FLG       TO PRTRD-DAILY-TR-FLG.
           IF  (PRTRD-PENS-SEQ-NBR NOT = ZEROES)
               MOVE PRTRD-COMPANY      TO DB-COMPANY
               MOVE PRTRD-EMPLOYEE     TO DB-EMPLOYEE
               MOVE PRTRD-PENS-SEQ-NBR TO DB-PENS-SEQ-NBR
               PERFORM 840-FIND-PNPSET2
               IF  (PRPENPAY-FOUND)
                   MOVE PNP-PENS-DIST-TYPE TO PRTRD-PENS-DIST-TYPE-SW
               END-IF
           END-IF.
073100
073200     MOVE "Y"                    TO PRTRD-BATCH-PROGRAM.
073300
073400     MOVE 1                      TO PRTRD-DST-ACCT-UNIT-FN
073500                                    PRTRD-ACTIVITY-FN
073600                                    PRTRD-ACCT-CATEGORY-FN
                                          PRTRD-DIST-PROC-LEV-FN
                                          PRTRD-DIST-DEPART-FN
                                          PRTRD-BUS-NBR-GRP-FN
                                          PRTRD-QC-ENT-NBR-GRP-FN.

J17912
J17912     IF (PRTRD-DAILY-TR-FLG = 1)
J17912         PERFORM 303-GET-DLY-END-DT
J17912         MOVE PRTRD-COMPANY  TO DB-COMPANY
J17912         MOVE PRTRD-EMPLOYEE TO DB-EMPLOYEE
J17912         PERFORM 840-FIND-EMPSET1
J77488         IF (EMP-DATE-HIRED > STM-EFFECT-DATE)
J17912             MOVE EMP-DATE-HIRED    TO PRTRD-DAILY-TR-DATE
J17912                                       PRTRD-TR-DATE
J17912         ELSE
J17912             MOVE STM-EFFECT-DATE   TO PRTRD-DAILY-TR-DATE
J17912                                       PRTRD-TR-DATE
J17912         END-IF
J11610         IF (R1D-PER-START-DATE > PRTRD-DAILY-TR-DATE)
J11610             MOVE R1D-PER-START-DATE TO PRTRD-DAILY-TR-DATE
J11610                                        PRTRD-TR-DATE
J11610         END-IF
J17912     ELSE
J17912         MOVE PRTRD-TR-DATE TO PRTRD-DAILY-TR-DATE
J17912                               PRTRD-DAILY-TR-END-DATE
J17912     END-IF.
J17912
J17912     PERFORM 231-GENERATE-TIMERECORDS
J17912     UNTIL   (PRTRD-DAILY-TR-DATE > PRTRD-DAILY-TR-END-DATE).
075500
075600 242-NEXT.
075700
075800     PERFORM 860-FIND-NXTRNG-STMSET3.
075900
076000 242-END.
076100     EXIT.
076200******************************************************************
076300 244-GET-STM-FOR-P               SECTION.
076400******************************************************************
076500 244-START.
076600
076700     IF  (PRM-PAY-FREQUENCY NOT = ZEROS)
076800     AND (EMP-PAY-FREQUENCY NOT = PRM-PAY-FREQUENCY)
076900         GO TO 244-NEXT.
077000
077100     IF  (STM-EFFECT-DATE NOT = ZEROS)
077200     AND (STM-EFFECT-DATE > PRM-TR-DATE)
077300         GO TO 244-NEXT.
077400
077500     IF  (STM-END-DATE NOT = ZEROS)
077600     AND (STM-END-DATE < PRM-TR-DATE)
077700         GO TO 244-NEXT.
077800
077900     IF (STM-DED-CYCLE (PRM-DED-CYCLE) = SPACE)
078000         GO TO 244-NEXT.
078100
078200     IF  (WS-P-TIME-GRP    = "N")
078300     AND (STM-FLEX-DOLLARS NOT = ZEROS)
078400         GO TO 244-NEXT.

           IF  (STM-CURRENCY-CODE NOT = SPACES)
           AND (STM-CURRENCY-CODE NOT = EMP-CURRENCY-CODE)
               MOVE EMP-EMPLOYEE       TO ER-EMPLOYEE
               MOVE 128                TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE        TO ER-MESSAGE
               WRITE ERROR-REC FROM ER-ERROR-REC
052800         GO TO 244-NEXT.
078500
078600     MOVE WS-TRUE                TO WS-STM-QUALIFY-SW.
078700     GO TO 244-END.
078800
078900 244-NEXT.
079000     IF (WS-P-TIME-GRP = "N")
079100         PERFORM 860-FIND-NXTRNG-STMSET1.
079200
079300 244-END.
079400     EXIT.
079500
079600******************************************************************
079700 250-EXTRACT-PAYCODE-TR           SECTION.
079800******************************************************************
079900 250-START.
080000
080100     PERFORM 251-EXTRACT-TIMERECORDS
080200         VARYING I4 FROM 1 BY 1
080300         UNTIL  (I4 > 5).
080400
080500 250-END.
080600     EXIT.
080700******************************************************************
080800 251-EXTRACT-TIMERECORDS          SECTION.
080900******************************************************************
081000 251-START.
081100
081200     IF (PRM-PAY-CODE (I4) = SPACES)
081300         GO TO 251-END.
081400
081500     INITIALIZE PRTRD-SCR-FIELDS.
081600
081700     MOVE "A"                    TO PRTRD-FC.
081800     MOVE EMP-COMPANY            TO PRTRD-COMPANY.
081900     MOVE PRM-BATCH-NBR          TO PRTRD-BATCH-NBR.
082000     MOVE EMP-EMPLOYEE           TO PRTRD-EMPLOYEE.
082100     MOVE PRM-PAY-CODE (I4)      TO PRTRD-PAY-CODE.
082200     MOVE PRM-HOURS (I4)         TO PRTRD-HOURS.
082300     MOVE PRM-PC-CHECK-GRP (I4)  TO PRTRD-CHECK-GRP.
082400     MOVE PRM-PC-PROCESS-GRP(I4) TO PRTRD-PROCESS-GRP.
082500     MOVE PRM-TR-DATE            TO PRTRD-TR-DATE.
082600     MOVE PRM-ACTIVITY (I4)      TO PRTRD-ACTIVITY.
082700     MOVE PRM-ACCT-CATEGORY (I4) TO PRTRD-ACCT-CATEGORY.
LP         MOVE PRM-REASON-CODE (I4)   TO PRTRD-REASON-CODE.
082800     MOVE PRM-ATTEND-CODE (I4)   TO PRTRD-ATTEND-CODE.
082900     MOVE PRM-OCCURRENCE (I4)    TO PRTRD-OCCURRENCE.
083000
083100     MOVE "Y"                    TO PRTRD-BATCH-PROGRAM.

083200
083300     INITIALIZE HREMP-SCR-FIELDS.
083400     PERFORM 2000-PRTRD-EMP-EDIT-TRAN.
083500
083600     IF (PRTRD-I3 NOT = ZEROS)
083700         PERFORM
083800             VARYING I2 FROM 1 BY 1
083900             UNTIL  (I2 > PRTRD-I3)
084000
084100             MOVE PRTRD-MESSAGE (I2) TO ER-MESSAGE
084200             MOVE EMP-EMPLOYEE TO ER-EMPLOYEE
084300             WRITE ERROR-REC FROM ER-ERROR-REC
084400             MOVE "Y"            TO WS-ERROR-FOUND
084500             END-PERFORM
084600             GO TO 251-END.
084700
084800     MOVE 1                      TO PRTRD-DST-ACCT-UNIT-FN
084900                                    PRTRD-ACTIVITY-FN
085000                                    PRTRD-ACCT-CATEGORY-FN
                                          PRTRD-DIST-PROC-LEV-FN
                                          PRTRD-DIST-DEPART-FN
                                          PRTRD-BUS-NBR-GRP-FN
                                          PRTRD-QC-ENT-NBR-GRP-FN.
085100     PERFORM 2000-PRTRD-EDIT-TRAN.
085200
085300     IF ((PRTRD-I3 NOT = ZEROS)
J69732     OR ((PRM-MID-PERIOD = 1 OR 2)
J63759     AND (PRTRD-PARTIAL)))
085500         PERFORM
085600             VARYING I2 FROM 1 BY 1
085700             UNTIL  (I2 > PRTRD-I3)
085800
085900             MOVE EMP-EMPLOYEE       TO ER-EMPLOYEE
086000             MOVE PRTRD-MESSAGE (I2) TO ER-MESSAGE
086100             WRITE ERROR-REC         FROM ER-ERROR-REC
086200             MOVE "Y"                TO WS-ERROR-FOUND
086300         END-PERFORM
               IF ((PRTRD-ERROR-FOUND)
J69732         OR ((PRM-MID-PERIOD = 1 OR 2)
J63759         AND (PRTRD-PARTIAL)))
                   GO TO 251-END.

J69732     IF (PRM-MID-PERIOD = 1 OR 2)
J63759     AND (PRTRD-PARTIAL)
J69732     AND (PRM-EXCEPTION-REPORT   NOT = 0)
J63759          MOVE EMP-EMPLOYEE          TO PD-EMPLOYEE
J63759          MOVE PRTRD-HIRE-DATE       TO PD-HIRE-DATE
J63759          MOVE PARTIAL-DETAIL        TO RPT-GROUP-REQUEST
J63759          PERFORM 700-PRINT-RPT-GRP
J63759          GO TO 251-END
J63759     END-IF.
086500
086600     PERFORM 299-EXTRACT-TIMERECORD.
086700     MOVE WS-TRUE                TO WS-EMPLOYEE-FLAG
086800                                    WS-PAYCODE-DONE-SW.
086900
087000 251-END.
087100     EXIT.
087200******************************************************************
087300 299-EXTRACT-TIMERECORD          SECTION.
087400******************************************************************
087500 299-START.
087600
           MOVE EMP-LAST-NAME          TO WF-LAST-NAME.
           MOVE EMP-FIRST-NAME         TO WF-FIRST-NAME.
           MOVE EMP-MIDDLE-INIT        TO WF-MIDDLE-INIT.
           MOVE EMP-NAME-SUFFIX        TO WF-NAME-SUFFIX.
           MOVE EMP-LAST-NAME-PRE      TO WF-LAST-NAME-PRE.

088000     MOVE EMP-EMPLOYEE           TO WF-EMPLOYEE.
088100     MOVE EMP-PROCESS-LEVEL      TO WF-PROCESS-LEVEL.
088200     MOVE EMP-DEPARTMENT         TO WF-DEPARTMENT.
088300     MOVE EMP-UNION-CODE         TO WF-UNION-CODE.
088400     MOVE PRTRD-TAX-STATE        TO WF-TAX-STATE.
           MOVE PRTRD-TAX-CITY         TO WF-TAX-CITY.
           MOVE PRTRD-TAX-COUNTY       TO WF-TAX-COUNTY.
           MOVE PRTRD-TAX-SCHOOL       TO WF-TAX-SCHOOL.
088500     MOVE PRTRD-PAY-SUM-GRP      TO WF-PAY-SUM-GRP.
088600     MOVE PRTRD-PCD-SEQ-NBR      TO WF-PCD-SEQ-NBR.
088700     MOVE PRTRD-POSITION         TO WF-POSITION.
088800     MOVE PRTRD-JOB-CODE         TO WF-JOB-CODE.
088900     MOVE PRTRD-PAY-CODE         TO WF-PAY-CODE.
089000     MOVE PRTRD-HOURS            TO WF-HOURS.
LP         MOVE PRTRD-REASON-CODE      TO WF-REASON-CODE.
LP         MOVE PRTRD-SERVICE-CODE     TO WF-SERVICE-CODE.
089100     MOVE PRTRD-ATTEND-CODE      TO WF-ATTEND-CODE.
089200     MOVE PRTRD-OCCURRENCE       TO WF-OCCURRENCE.
089300     MOVE PRTRD-RATE             TO WF-RATE.
089400     MOVE PRTRD-CHECK-GRP        TO WF-CHECK-GRP.
089500     MOVE PRTRD-PROCESS-GRP      TO WF-PROCESS-GRP.
           MOVE PRTRD-TAX-FREQ-OVER    TO WF-TAX-FREQ-OVER.
089600     MOVE PRTRD-DIST-PROC-LEV    TO WF-DIST-PROC-LEV.
089700     MOVE PRTRD-DIST-DEPART      TO WF-DIST-DEPART.
089800     MOVE PRTRD-SHIFT            TO WF-SHIFT.
089900     MOVE PRTRD-DIST-COMPANY     TO WF-DIST-COMPANY.
090000     MOVE PRTRD-DST-ACCT-UNIT    TO WF-DST-ACCT-UNIT.
090100     MOVE PRTRD-DST-ACCOUNT      TO WF-DST-ACCOUNT.
090200     MOVE PRTRD-DST-SUB-ACCT     TO WF-DST-SUB-ACCT.
090300     MOVE PRTRD-ACTIVITY         TO WF-ACTIVITY.
090400     MOVE PRTRD-ACCT-CATEGORY    TO WF-ACCT-CATEGORY.
090500     MOVE PRTRD-PCT-DIST-FLAG    TO WF-PCT-DIST-FLAG.
090600     MOVE PRTRD-ANNUAL-SALARY    TO WF-ANNUAL-SALARY.
090700     MOVE PRTRD-SCHEDULE         TO WF-SCHEDULE.
090800     MOVE PRTRD-PAY-STEP         TO WF-PAY-STEP.
090900     MOVE PRTRD-PAY-GRADE        TO WF-PAY-GRADE.
091000     MOVE PRTRD-WC-CLASS         TO WF-WC-CLASS.
091000     MOVE PRTRD-CA-WC-CLASS      TO WF-CA-WC-CLASS.
091100     MOVE PRTRD-TR-DATE          TO WF-TR-DATE.
091200     MOVE PRTRD-WORK-STATE       TO WF-WORK-STATE.
091300     MOVE PRTRD-WC-STATE         TO WF-WC-STATE.
091400     MOVE PRTRD-SUPP-TAX-CODE    TO WF-SUPP-TAX-CODE.
091500     MOVE PRTRD-WAGE-AMOUNT      TO WF-WAGE-AMOUNT.
091600     MOVE PRTRD-OT-PREM-AMT      TO WF-OT-PREM-AMT.
091700     MOVE PRTRD-OT-RATE          TO WF-OT-RATE.
091800     MOVE PRTRD-SHIFT-DIFF       TO WF-SHIFT-DIFF.
091900     MOVE PRTRD-SHIFT-DIFF-AMT   TO WF-SHIFT-DIFF-AMT.
092000     MOVE PRTRD-SH-OBJ-ID        TO WF-SH-OBJ-ID.
J50888     MOVE PRTRD-PER-BEG-DATE     TO WF-PER-BEG-DATE.
092100     MOVE PRTRD-PER-END-DATE     TO WF-PER-END-DATE.
J50888     MOVE PRTRD-WRK-BEG-DATE     TO WF-WRK-BEG-DATE.
092200     MOVE PRTRD-WRK-END-DATE     TO WF-WRK-END-DATE.
092300     MOVE PRTRD-LOCAT-CODE       TO WF-LOCAT-CODE.
092400     MOVE PRTRD-OT-RECORD        TO WF-OT-RECORD.
           MOVE PRTRD-CURRENCY-CODE    TO WF-CURRENCY-CODE.
           MOVE PRTRD-CURR-ND          TO WF-CURR-ND.
           MOVE PRTRD-BUS-NBR-GRP      TO WF-BUS-NBR-GRP.
           MOVE PRTRD-QC-ENT-NBR-GRP   TO WF-QC-ENT-NBR-GRP.
           MOVE PRTRD-PENS-SEQ-NBR     TO WF-PENS-SEQ-NBR.
           MOVE PRTRD-REPORT-ENTITY    TO WF-REPORT-ENTITY.
           MOVE PRTRD-COUNTRY-CODE     TO WF-COUNTRY-CODE.
P59635     MOVE PRTRD-NON-EARNINGS     TO WF-NON-EARNINGS.
P73731     MOVE PRTRD-REMUN-CODE       TO WF-REMUN-CODE.
J17912     MOVE PRTRD-ADDTL-RATE       TO WF-ADDTL-RATE.
J57836     MOVE PRTRD-NOTIONAL         TO WF-NOTIONAL.

P48725     MOVE HREMP-SALARY-CLASS     TO WF-SALARY-CLASS.

      *--- Begin SL changes
           MOVE PRTRD-SEGMENT-FLAG     TO WF-SEGMENT-FLAG.
           MOVE PRTRD-SEGMENT-BLOCK    TO WF-SEGMENT-BLOCK.
      *--- End SL changes
092500
092600     WRITE WORK-REC              FROM WF-WORK-REC.
092700
092800 299-END.
092900     EXIT.
093000******************************************************************
093100 300-CREATE-TIMERECORDS          SECTION.
093200******************************************************************
093300 300-START.
093400
093500     PERFORM 301-UPDATE-BATCH-NUMBER.
093600
093700     INITIALIZE WS-BST-TABLE.
093800
093900     MOVE WS-FALSE               TO WS-END-OF-FILE-SW.
P40205     MOVE ZEROS                  TO WS-EMP-TOT-ERRORS.
094000
094100     READ WORK-FILE INTO WF-WORK-REC
094200         AT END
094300             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
094400
094500     PERFORM 310-TYPE-CONTROL
094600         UNTIL (END-OF-FILE).
094700
094800     MOVE WS-TE-TOT-EMPLOYEES    TO TE-TOT-EMPLOYEES.
094900     MOVE TOTAL-EMPLOYEES        TO RPT-GROUP-REQUEST.
095000     PERFORM 700-PRINT-RPT-GRP.
095100
095200     MOVE WS-GT-TOT-HOURS        TO WS-ACTUAL-HOURS.
095300     MOVE WS-GT-TOT-RATE         TO WS-ACTUAL-AMOUNT.
095400     PERFORM 302-CREATE-BATCH-RECORD.
095500
095600 300-END.
095700     EXIT.
095800******************************************************************
095900 301-UPDATE-BATCH-NUMBER            SECTION.
096000******************************************************************
096100 301-START.
096200
096300     PERFORM 910-AUDIT-BEGIN.
096400
096500     INITIALIZE PRPRB-SCR-FIELDS.
096600     MOVE "A"                    TO PRPRB-FC.
096700     MOVE PRM-COMPANY            TO PRPRB-COMPANY.
096800     MOVE PRM-BATCH-NBR          TO PRPRB-BATCH-NBR.
096900
097000     PERFORM 2000-PRPRB-EDIT-TRAN.
097100
097200     IF (PRPRB-FC = "C")
097300         MOVE WS-TRUE            TO WS-EXISTING-BATCH-SW
097400     ELSE
097500         MOVE WS-FALSE           TO WS-EXISTING-BATCH-SW.
097600
097700     PERFORM 3000-PRPRB-PROCESS-TRAN.
097800
097900     MOVE PRPRB-BATCH-NBR        TO PRM-BATCH-NBR.
098000
098100     PERFORM 925-AUDIT-END.
098200
098300 301-END.
098400     EXIT.
098500******************************************************************
098600 302-CREATE-BATCH-RECORD         SECTION.
098700******************************************************************
098800 302-START.
098900
099000     IF (EXISTING-BATCH)
J13588*        IF  (EDCDWS-TRIGGER-ENABLED)
J13588         IF  (WS-EMP-TOT-ERRORS > 0)
P40205              MOVE 143            TO CRT-MSG-NBR
P40205         ELSE
P40205              MOVE 120            TO CRT-MSG-NBR
P40205         END-IF
099200         PERFORM 780-PRINT-MSG
099300         MOVE PRM-BATCH-NBR       TO GT-BATCH-NBR
099400         MOVE PRPRB-ACTUAL-HOURS  TO GT-TOT-HOURS
099500         MOVE PRPRB-ACTUAL-AMOUNT TO GT-TOT-RATE
099600         MOVE GRAND-TOTAL         TO RPT-GROUP-REQUEST
099700         PERFORM 700-PRINT-RPT-GRP.
099800
J13588*    IF  (EDCDWS-TRIGGER-ENABLED)
J13588     IF  (NOT EXISTING-BATCH)
P45097         IF (WS-LP-BATCH-REC-OK > ZERO)
P45097            IF (WS-LP-BATCH-REC-ERR > 0)
P45097               MOVE 145                 TO CRT-MSG-NBR
P45097               PERFORM 780-PRINT-MSG
P45097            END-IF
P45097         ELSE
P45097         IF (WS-LP-BATCH-REC-OK = ZERO)
P45097            MOVE 144                TO CRT-MSG-NBR
P45097            PERFORM 780-PRINT-MSG.

099900     PERFORM 910-AUDIT-BEGIN.

J13588*    IF  (EDCDWS-TRIGGER-ENABLED)
J13588     IF  (NOT EXISTING-BATCH)
P45097     AND (WS-LP-BATCH-REC-OK = ZERO)
P45097         MOVE "D"                TO PRPRB-FC
P45097     ELSE
P45097         MOVE "C"                TO PRPRB-FC.

100200     MOVE PRM-COMPANY            TO PRPRB-COMPANY.
100300     MOVE PRM-BATCH-NBR          TO PRPRB-BATCH-NBR.
100400
100500     PERFORM 3000-PRPRB-PROCESS-TRAN.
100600
100700     PERFORM 925-AUDIT-END.
100800
100900 302-END.
101000     EXIT.
J17912*****************************************************************
J17912 303-GET-DLY-END-DT              SECTION.
J17912*****************************************************************
J17912 303-START.
J17912
J17912     MOVE PRTRD-COMPANY          TO DB-COMPANY.
J17912     MOVE SPACES                 TO DB-PROCESS-LEVEL.
J17912     PERFORM 840-FIND-PRSSET1.
J17912     MOVE PRS-PAYROLL-YEAR       TO PRTRD-PAYROLL-YEAR.
J17912
J17912     MOVE PRTRD-EMPLOYEE         TO DB-EMPLOYEE.
J17912     PERFORM 840-FIND-EMPSET1.
J17912     MOVE EMP-OT-PLAN-CODE     TO PRTRD-OT-PLAN-CODE.
J17912     IF  (PRTRD-OT-PLAN-CODE      = SPACES)
J17912     AND (EMP-PAY-FREQUENCY NOT = ZEROS)
J17912         MOVE EMP-PROCESS-LEVEL TO DB-PROCESS-LEVEL
J17912         PERFORM 840-FIND-PRSSET1
J17912         MOVE PRS-OT-PLAN-CODE (EMP-PAY-FREQUENCY)
J17912                                 TO PRTRD-OT-PLAN-CODE
J17912     END-IF.
J17912     MOVE PRTRD-OT-PLAN-CODE     TO DB-PLAN-CODE.
J17912     MOVE PRTRD-PAYROLL-YEAR     TO DB-PAYROLL-YEAR.
J17912     MOVE R1DSET1-PAYROLL-YEAR   TO WS-DB-BEG-RNG.
J17912     INITIALIZE FILTER-STRING.
J17912     STRING "((R1D-PER-START-DATE <= ?) AND "
J17912            "(R1D-PER-END-DATE >= ?))"
J17912            DELIMITED BY SIZE INTO FILTER-STRING.
J17912     PERFORM 890-CREATE-FILTER.
J17912     MOVE PRTRD-TR-DATE      TO DATETIME-FILTER-VALUE.
J17912     PERFORM 890-SET-DATETIME-FILTER-VALUE.
J17912     MOVE PRTRD-TR-DATE      TO DATETIME-FILTER-VALUE.
J17912     PERFORM 890-SET-DATETIME-FILTER-VALUE.
J17912     PERFORM 850-FILTER-BEGRNG-R1DSET1.
039994*    IF  (PRTRD-END-DATE < R1D-PER-END-DATE)
039994*    AND (PRTRD-END-DATE NOT = ZEROES)
039994*        MOVE PRTRD-END-DATE TO PRTRD-DAILY-TR-END-DATE
039994     IF (R1PLANDTL-FOUND)
039994         IF  (STM-END-DATE < R1D-PER-END-DATE)
039994         AND (STM-END-DATE NOT = ZEROES)
039994             MOVE STM-END-DATE     TO PRTRD-DAILY-TR-END-DATE
J17912         ELSE
J17912             MOVE R1D-PER-END-DATE TO PRTRD-DAILY-TR-END-DATE
J17912         END-IF
039994     END-IF.
J17912
J17912     IF  (EMP-TERM-DATE < PRTRD-DAILY-TR-END-DATE)
J17912     AND (EMP-TERM-DATE NOT = ZEROES)
J17912         MOVE EMP-TERM-DATE TO PRTRD-DAILY-TR-END-DATE
J17912     END-IF.
J17912
J17912     IF  (EMP-LAST-DAY-PAID NOT = ZEROES)
J17912     AND (EMP-LAST-DAY-PAID < PRTRD-DAILY-TR-END-DATE)
J17912          MOVE EMP-LAST-DAY-PAID TO PRTRD-DAILY-TR-END-DATE
J17912     END-IF.
J17912
J17912     MOVE PRTRD-DAILY-TR-END-DATE TO PRTRD-PER-END-DATE.
J17912
J17912 303-END.
J17912     EXIT.
101100******************************************************************
101200 310-TYPE-CONTROL                SECTION.
101300******************************************************************
101400 310-START.
101500
101600     PERFORM 400-PRINT-PAGE-HEADING.
101700     PERFORM 410-PRINT-BATCH-HEADING.
101800     PERFORM 420-PRINT-DETAIL-HEADING.
101900
102000     MOVE ZEROS                  TO WS-BT-TOT-HOURS
102100                                    WS-BT-TOT-RATE.
102200
102300     PERFORM 320-CREATE-TIMERECORDS
102400         UNTIL (END-OF-FILE).
102500
102600     MOVE WS-TRUE                TO WS-BST-ELEMENT-FLAG.
102700
102800     MOVE PRM-BATCH-NBR          TO BPH-BATCH-NBR.
102900     MOVE BATCH-PC-HEADING       TO RPT-GROUP-REQUEST.
103000     PERFORM 700-PRINT-RPT-GRP.
103100
103200     PERFORM 480-PRINT-BATCH-SUB-TOTAL
103300         VARYING I1 FROM 1 BY 1
103400         UNTIL  (I1 > 200)
103500         OR     (WS-BST-ELEMENT-NOTFOUND).
103600
103700     PERFORM 490-PRINT-BATCH-TOTAL.
103800
103900 310-END.
104000     EXIT.
104100******************************************************************
104200 320-CREATE-TIMERECORDS          SECTION.
104300******************************************************************
104400 320-START.
104500
           MOVE WF-EMPLOYEE            TO TR-EMPLOYEE.

           MOVE WF-LAST-NAME           TO HRWS-LAST-NAME.
           MOVE WF-FIRST-NAME          TO HRWS-FIRST-NAME.
           MOVE WF-MIDDLE-INIT         TO HRWS-MIDDLE-INIT.
           MOVE WF-NAME-SUFFIX         TO HRWS-NAME-SUFFIX.
           MOVE WF-LAST-NAME-PRE       TO HRWS-LAST-NAME-PRE.
           PERFORM 750-HR-FORMAT-NAME.
P53752     MOVE HRWS-FORMAT-NAME(1:23) TO TR-FULL-NAME.

104800     MOVE ZEROS                  TO WS-ET-TOT-HOURS
104900                                    WS-ET-TOT-RATE.
105000
105100     MOVE ZEROS                  TO WS-EMPLOYEE-TR-COUNT.
105200
105300     MOVE WF-EMPLOYEE            TO WS-EMPLOYEE.
P48725     MOVE WF-SALARY-CLASS        TO WS-SALARY-CLASS.
105400
105500     ADD 1                       TO WS-TE-TOT-EMPLOYEES.
105600
J13588*    IF (EDCDWS-TRIGGER-ENABLED)
J13588     MOVE PRM-COMPANY         TO LPBAL-COMPANY.
J13588     MOVE WF-PROCESS-LEVEL    TO LPBAL-HM-PROC-LEVEL.
J13588     MOVE WF-EMPLOYEE         TO LPBAL-EMPLOYEE.
J13588     MOVE PRM-BATCH-NBR       TO LPBAL-PGM-BATCH-NBR.
J13588     PERFORM 750-LPBAL-EXISTING-TRD.

105700     PERFORM 910-AUDIT-BEGIN.
105800
105900     MOVE WS-FALSE               TO WS-TRD-CREATED-SW.
106000
106100     PERFORM 330-CREATE-TIMERECORDS
106200         UNTIL (END-OF-FILE)
106300         OR    (WF-EMPLOYEE NOT = WS-EMPLOYEE).
106400
106500     PERFORM 840-MODIFY-CKPSET1.
106600     MOVE WS-RESTART-INFO        TO CKP-RESTART-INFO.
106700     PERFORM 820-STORE-CKPOINT.
P54387     PERFORM 925-AUDIT-END.
106900
107000     IF (TRD-NOT-CREATED)
107100         GO TO 320-CONTINUE.
107200
107300     MOVE PRM-COMPANY            TO DB-COMPANY.
107400     MOVE WS-EMPLOYEE            TO DB-EMPLOYEE.
107500     PERFORM 840-FIND-EMPSET1.
107600
P48725     IF (WS-SALARY-CLASS = "S")
107800         PERFORM 600-CALCULATE-WAGES-70.
107900
J82010***Moved the codes below to the near end of 320-CREATE-TIMERECORDS
J82010***where the time record is already stored in TRD table.
J82010*     MOVE ZEROS                  TO PRSTD-EMPLOYEE.
J82010*     MOVE PRM-BATCH-NBR          TO PRSTD-BATCH-NBR.
J82010*     PERFORM 600-EDIT-STANDARDS.
108300     MOVE ZEROS                  TO PRBAL-EMPLOYEE.
108400     MOVE PRM-BATCH-NBR          TO PRBAL-BATCH-NBR.
108500     MOVE "Y"                    TO PRBAL-BATCH-PROGRAM.
108600     PERFORM 600-CHECK-TA-BALANCES.
108700
J82010*     PERFORM
J82010*         VARYING I2 FROM 1 BY 1
J82010*P38129         UNTIL  (I2 > 20)
J82010*
J82010*         IF (PRSTD-MESSAGE (I2) NOT = SPACES)
J82010*             MOVE PRSTD-MESSAGE (I2) TO ER-MESSAGE
J82010*             MOVE WS-EMPLOYEE        TO ER-EMPLOYEE
J82010*             WRITE ERROR-REC         FROM ER-ERROR-REC
J82010*             MOVE "Y"                TO WS-ERROR-FOUND
J82010*         END-IF
J82010*     END-PERFORM.

P38129     IF  (PRSTD-SUBSCRIPT-RNG-ERR = "Y")
P38129          MOVE 142                TO CRT-MSG-NBR
P38129          PERFORM 790-GET-MSG
P38129          MOVE CRT-MESSAGE        TO ER-MESSAGE
P38129          MOVE WS-EMPLOYEE        TO ER-EMPLOYEE
P38129          WRITE ERROR-REC         FROM ER-ERROR-REC
P38129          MOVE "Y"                TO WS-ERROR-FOUND.

109900
110000     PERFORM
110100         VARYING I2 FROM 1 BY 1
110200         UNTIL  (I2 > PRBAL-PLAN-TABLE-COUNT)
110300
110400         IF (PRBAL-MESSAGE (I2) NOT = SPACES)
110500             MOVE PRBAL-MESSAGE (I2) TO ER-MESSAGE
110600             MOVE WS-EMPLOYEE        TO ER-EMPLOYEE
110700             WRITE ERROR-REC         FROM ER-ERROR-REC
110800             MOVE "Y"                TO WS-ERROR-FOUND
110900         END-IF
111000     END-PERFORM.
111100
LP         PERFORM
LP             VARYING I1 FROM 1 BY 1
LP             UNTIL  (I1            > LPBAL-MSG-TABLE-SIZE)
LP
LP             IF (LPBAL-MESSAGE (I1) NOT = SPACES)
LP                 MOVE WS-EMPLOYEE        TO ER-EMPLOYEE
LP                 MOVE LPBAL-MESSAGE (I1) TO ER-MESSAGE
LP                 WRITE ERROR-REC         FROM ER-ERROR-REC
LP             END-IF
LP         END-PERFORM.
LP
LP         INITIALIZE LPBAL-EDIT-SW.
LP
J13588*    IF  (EDCDWS-TRIGGER-ENABLED)
J13588     IF  (LPBAL-SEND-ERROR)
P40205         ADD 1 TO WS-EMP-TOT-ERRORS.  

J13588*    IF  (EDCDWS-TRIGGER-ENABLED)
J13588     PERFORM 910-AUDIT-BEGIN.
J13588     IF (WS-SALARY-CLASS = "S")
J13588         PERFORM 660-PROCESS-LP-TIMEREC
J13588         PERFORM 600-CALCULATE-WAGES-70
J13588     ELSE  
J13588         PERFORM 660-PROCESS-LP-TIMEREC
J13588     END-IF.
J13588     PERFORM 925-AUDIT-END.
J13588*    END-IF.

J70871*J82010     MOVE ZEROS                  TO PRSTD-EMPLOYEE.
J70871*J82010     MOVE PRM-BATCH-NBR          TO PRSTD-BATCH-NBR.
J70871*J82010     PERFORM 600-EDIT-STANDARDS.
J70871*J82010
J70871*J82010     PERFORM
J70871*J82010         VARYING I2 FROM 1 BY 1
J70871*J82010         UNTIL  (I2 > 20)
J70871*J82010
J70871*J82010         IF (PRSTD-MESSAGE (I2) NOT = SPACES)
J70871*J82010             MOVE PRSTD-MESSAGE (I2) TO ER-MESSAGE
J70871*J82010             MOVE WS-EMPLOYEE        TO ER-EMPLOYEE
J70871*J82010             WRITE ERROR-REC         FROM ER-ERROR-REC
J70871*J82010             MOVE "Y"                TO WS-ERROR-FOUND
J70871*J82010         END-IF
J70871*J82010     END-PERFORM.
J70871*J82010
J70871
J70871     MOVE ZEROS                  TO PRSTD-EMPLOYEE.
J70871     MOVE PRM-BATCH-NBR          TO PRSTD-BATCH-NBR.
J70871     PERFORM 600-EDIT-STANDARDS.
J70871
J70871     PERFORM
J70871         VARYING I2 FROM 1 BY 1
J70871         UNTIL  (I2 > 20)
J70871
J70871         IF (PRSTD-MESSAGE (I2) NOT = SPACES)
J70871             MOVE PRSTD-MESSAGE (I2) TO ER-MESSAGE
J70871             MOVE WS-EMPLOYEE        TO ER-EMPLOYEE
J70871             WRITE ERROR-REC         FROM ER-ERROR-REC
J70871             MOVE "Y"                TO WS-ERROR-FOUND
J70871         END-IF
J70871     END-PERFORM.
J70871

J13588*    IF (EDCDWS-TRIGGER-ENABLED)
J13588     IF (WS-LP-EMP-REC-ERR = 1)
J13588         MOVE 1 TO WS-LP-BATCH-REC-ERR
J13588     END-IF.
J13588     IF (WS-LP-EMP-REC-OK = 1)
J13588         MOVE 1 TO WS-LP-BATCH-REC-OK
J13588     END-IF.
J13588*    END-IF.


111200 320-CONTINUE.
111300     MOVE WS-ET-TOT-HOURS        TO ET-TOT-HOURS.
111400     MOVE WS-ET-TOT-RATE         TO ET-TOT-RATE.
111500     PERFORM 450-PRINT-EMPLOYEE-TOTAL.
111600
111700     ADD WS-ET-TOT-HOURS         TO WS-GT-TOT-HOURS.
111800     ADD WS-ET-TOT-RATE          TO WS-GT-TOT-RATE.
111900
112000 320-END.
112100     EXIT.
112200******************************************************************
112300 330-CREATE-TIMERECORDS          SECTION.
112400******************************************************************
112500 330-START.
LP    *  CHANGE REPORT LAYOUTS AS PART OF LP CHANGES.  
112600
112700     ADD 1                       TO WS-RECORD-COUNT.
112800
112900     IF (WS-RECORD-COUNT > WS-RESTART-RECORD-COUNT)
113000         PERFORM 340-CREATE-TIMERECORD
113100         MOVE WS-TRUE            TO WS-TRD-CREATED-SW.
113200
J13588*    IF (EDCDWS-TRIGGER-ENABLED)
J13588*       GO TO 330-NEXT.

J13588*    PERFORM 440-PRINT-TIMERECORD.

J13588*    IF (WF-JOB-CODE      NOT = SPACES)
J13588*    OR (WF-POSITION      NOT = SPACES)
J13588*    OR (WF-LOCAT-CODE    NOT = SPACES)
J13588*    OR (WF-DIST-PROC-LEV NOT = SPACES)
J13588*    OR (WF-DIST-DEPART   NOT = SPACES)
J13588*        MOVE WF-JOB-CODE        TO TR2-JOB-CODE
J13588*        MOVE WF-POSITION        TO TR2-POSITION
J13588*        MOVE WF-LOCAT-CODE      TO TR2-LOCAT-CODE
J13588*        MOVE WF-DIST-PROC-LEV   TO TR2-DIST-PROC-LEV
J13588*        MOVE WF-DIST-DEPART     TO TR2-DIST-DEPART
J13588*        MOVE TIME-RECORD2       TO RPT-GROUP-REQUEST
J13588*        PERFORM 700-PRINT-RPT-GRP

J13588*    IF (WF-SCHEDULE      NOT = SPACES)
J13588*    OR (WF-PAY-STEP      NOT = ZEROS)
J13588*    OR (WF-PAY-GRADE     NOT = SPACES)
J13588*    OR (WF-REASON-CODE   NOT = SPACES)
J13588*    OR (WF-ATTEND-CODE   NOT = SPACES)
J13588*    OR (WF-OCCURRENCE    NOT = SPACES)
J13588*        MOVE WF-SCHEDULE        TO TR3-SCHEDULE
J13588*        MOVE WF-PAY-STEP        TO TR3-PAY-STEP
J13588*        MOVE WF-PAY-GRADE       TO TR3-PAY-GRADE
J13588*        MOVE WF-REASON-CODE     TO TR3-REASON-CODE
J13588*        MOVE WF-ATTEND-CODE     TO TR3-ATTEND-CODE
J13588*        MOVE WF-OCCURRENCE      TO TR3-OCCURRENCE
J13588*        MOVE TIME-RECORD3       TO RPT-GROUP-REQUEST
J13588*        PERFORM 700-PRINT-RPT-GRP
      *--- Begin SL changes
J13588*        IF (WF-SEGMENT-FLAG = "Y")
J13588*            PERFORM 331-DO-SEGMENT-BLOCK
J13588*        END-IF
      *--- End SL changes
J13588*    END-IF.
114500
P45097 330-NEXT.
114600     READ WORK-FILE INTO WF-WORK-REC
114700         AT END
114800             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.

P45097 330-END.

      ******************************************************************
P45097 331-DO-SEGMENT-BLOCK            SECTION.
      ******************************************************************
P45097  331-START.
P45097* 331 PARAGRAPH WAS CHANGED TO BE A SECTION.
P45097* IT IS CALLED FROM 330-CREATE-TIMERECORDS AND
P45097* 662-PROCESS-LP-TIMERECS.
      *--- Begin SL changes
           IF (WF-DIST-COMPANY      NOT = IFSGWS-COMPANY)
               MOVE WF-DIST-COMPANY    TO IFSGWS-COMPANY
               PERFORM 700-RETURN-SEGMENT-GROUP-80
               IF (ERROR-FOUND)
                  INITIALIZE              CRT-ERROR-NBR
                                          IFSGWS-SEGMENT-GROUP.

           IF (IFSGWS-SEGMENT-GROUP   = SPACES)
                GO TO 331-END.

           MOVE WF-SEGMENT-BLOCK       TO SLSEWS-SEGMENT-BLOCK.
           MOVE WF-DIST-COMPANY        TO SLSEWS-COMPANY.
           MOVE 1                      TO SLSEWS-EDIT-TYPE.
           PERFORM 700-EDIT-SEGMENT-BLOCK-80.
           INITIALIZE CRT-ERROR-NBR.

           MOVE SLSEWS-SEGMENT-VALUE(1) TO TR3-SEGMENT-VALUE-1.
           MOVE SLSEWS-SEGMENT-VALUE(2) TO TR3-SEGMENT-VALUE-2.
           MOVE SLSEWS-SEGMENT-VALUE(3) TO TR3-SEGMENT-VALUE-3.
           MOVE SLSEWS-SEGMENT-VALUE(4) TO TR3-SEGMENT-VALUE-4.
           MOVE SLSEWS-SEGMENT(1)       TO TR3-SEGMENT-1.
           MOVE SLSEWS-SEGMENT(2)       TO TR3-SEGMENT-2.
           MOVE SLSEWS-SEGMENT(3)       TO TR3-SEGMENT-3.
           MOVE SLSEWS-SEGMENT(4)       TO TR3-SEGMENT-4.
           MOVE TR3-SEGMENT-BLOCK       TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.
      *--- End SL changes

       331-END.

115200******************************************************************
115300 340-CREATE-TIMERECORD           SECTION.
115400******************************************************************
115500 340-START.
115600
115700     INITIALIZE PRTRD-SCR-FIELDS.
115800
115900     MOVE PRM-COMPANY            TO PRTRD-COMPANY.
116000     MOVE PRM-BATCH-NBR          TO PRTRD-BATCH-NBR.
116100     MOVE WF-EMPLOYEE            TO PRTRD-EMPLOYEE.
116200     MOVE "A"                    TO PRTRD-FC.
116300     MOVE WF-POSITION            TO PRTRD-POSITION.
116400     MOVE WF-JOB-CODE            TO PRTRD-JOB-CODE.
116500     MOVE WF-PAY-CODE            TO PRTRD-PAY-CODE.
116600     MOVE WF-RATE                TO PRTRD-RATE.
116700     MOVE WF-HOURS               TO PRTRD-HOURS.
116800     MOVE WF-DST-ACCT-UNIT       TO PRTRD-DST-ACCT-UNIT.
116900     MOVE WF-DST-ACCOUNT         TO PRTRD-DST-ACCOUNT.
117000     MOVE WF-DST-SUB-ACCT        TO PRTRD-DST-SUB-ACCT.
117100     MOVE WF-PCT-DIST-FLAG       TO PRTRD-PCT-DIST-FLAG.
117200     MOVE WF-CHECK-GRP           TO PRTRD-CHECK-GRP.
117300     MOVE WF-PROCESS-GRP         TO PRTRD-PROCESS-GRP.
117400     MOVE WF-SHIFT               TO PRTRD-SHIFT.
117500     MOVE WF-TR-DATE             TO PRTRD-TR-DATE.
LP         MOVE WF-REASON-CODE         TO PRTRD-REASON-CODE.
LP         MOVE WF-SERVICE-CODE        TO PRTRD-SERVICE-CODE.
117600     MOVE WF-ATTEND-CODE         TO PRTRD-ATTEND-CODE.
117700     MOVE WF-OCCURRENCE          TO PRTRD-OCCURRENCE.
117800     MOVE WF-DIST-PROC-LEV       TO PRTRD-DIST-PROC-LEV.
117900     MOVE WF-DIST-DEPART         TO PRTRD-DIST-DEPART.
118000     MOVE WF-SCHEDULE            TO PRTRD-SCHEDULE.
118100     MOVE WF-PAY-STEP            TO PRTRD-PAY-STEP.
118200     MOVE WF-PAY-GRADE           TO PRTRD-PAY-GRADE.
118300     MOVE WF-DIST-COMPANY        TO PRTRD-DIST-COMPANY.
118400     MOVE WF-ACTIVITY            TO PRTRD-ACTIVITY.
118500     MOVE WF-ACCT-CATEGORY       TO PRTRD-ACCT-CATEGORY.
118600     MOVE WF-WORK-STATE          TO PRTRD-WORK-STATE.
118700     MOVE WF-WC-STATE            TO PRTRD-WC-STATE.
118800     MOVE WF-PAY-SUM-GRP         TO PRTRD-PAY-SUM-GRP.
118900     MOVE WF-WC-CLASS            TO PRTRD-WC-CLASS.
118900     MOVE WF-CA-WC-CLASS         TO PRTRD-CA-WC-CLASS.
119000     MOVE WF-PCD-SEQ-NBR         TO PRTRD-PCD-SEQ-NBR.
119100     MOVE WF-ANNUAL-SALARY       TO PRTRD-ANNUAL-SALARY.
119200     MOVE WF-SUPP-TAX-CODE       TO PRTRD-SUPP-TAX-CODE.
119300     MOVE WF-WAGE-AMOUNT         TO PRTRD-WAGE-AMOUNT.
119400     MOVE WF-OT-PREM-AMT         TO PRTRD-OT-PREM-AMT.
119500     MOVE WF-OT-RATE             TO PRTRD-OT-RATE.
119600     MOVE WF-SHIFT-DIFF          TO PRTRD-SHIFT-DIFF.
119700     MOVE WF-SHIFT-DIFF-AMT      TO PRTRD-SHIFT-DIFF-AMT.
119800     MOVE WF-SH-OBJ-ID           TO PRTRD-SH-OBJ-ID.
J50888     MOVE WF-PER-BEG-DATE        TO PRTRD-PER-BEG-DATE.
119900     MOVE WF-PER-END-DATE        TO PRTRD-PER-END-DATE.
J50888     MOVE WF-WRK-BEG-DATE        TO PRTRD-WRK-BEG-DATE.
120000     MOVE WF-WRK-END-DATE        TO PRTRD-WRK-END-DATE.
120100     MOVE WF-UNION-CODE          TO PRTRD-UNION-CODE.
120200     MOVE WF-TAX-STATE           TO PRTRD-TAX-STATE.
           MOVE WF-TAX-CITY            TO PRTRD-TAX-CITY.
           MOVE WF-TAX-COUNTY          TO PRTRD-TAX-COUNTY.
           MOVE WF-TAX-SCHOOL          TO PRTRD-TAX-SCHOOL.
120300     MOVE WF-DEPARTMENT          TO PRTRD-DEPARTMENT.
120400     MOVE WF-PROCESS-LEVEL       TO PRTRD-PROCESS-LEVEL.
120500     MOVE WF-LOCAT-CODE          TO PRTRD-LOCAT-CODE.
120600     MOVE WF-OT-RECORD           TO PRTRD-OT-RECORD.
           MOVE WF-CURRENCY-CODE       TO PRTRD-CURRENCY-CODE.
           MOVE WF-CURR-ND             TO PRTRD-CURR-ND.
           MOVE WF-BUS-NBR-GRP         TO PRTRD-BUS-NBR-GRP.
           MOVE WF-QC-ENT-NBR-GRP      TO PRTRD-QC-ENT-NBR-GRP.
           MOVE WF-TAX-FREQ-OVER       TO PRTRD-TAX-FREQ-OVER.
           MOVE WF-PENS-SEQ-NBR        TO PRTRD-PENS-SEQ-NBR.
           MOVE WF-COUNTRY-CODE        TO PRTRD-COUNTRY-CODE.
P59635     MOVE WF-NON-EARNINGS        TO PRTRD-NON-EARNINGS.
P73731     MOVE WF-REMUN-CODE          TO PRTRD-REMUN-CODE.
J17912     MOVE WF-ADDTL-RATE          TO PRTRD-ADDTL-RATE.
J57836     MOVE WF-NOTIONAL            TO PRTRD-NOTIONAL.
           IF  (PRTRD-PENS-SEQ-NBR NOT = ZEROES)
               MOVE PRTRD-COMPANY      TO DB-COMPANY
               MOVE PRTRD-EMPLOYEE     TO DB-EMPLOYEE
               MOVE PRTRD-PENS-SEQ-NBR TO DB-PENS-SEQ-NBR
               PERFORM 840-FIND-PNPSET2
               IF  (PRPENPAY-FOUND)
                   MOVE PNP-PENS-DIST-TYPE TO PRTRD-PENS-DIST-TYPE-SW
               END-IF
           END-IF.
           MOVE WF-REPORT-ENTITY       TO PRTRD-REPORT-ENTITY.
           MOVE WF-SEGMENT-BLOCK       TO PRTRD-SEGMENT-BLOCK.
           IF  (WF-SEGMENT-BLOCK       NOT = SPACES)
               MOVE "Y"                TO WF-SEGMENT-FLAG
           END-IF.
120700
J13588*    IF (EDCDWS-TRIGGER-ENABLED)
J13588     SET LPBAL-EDIT TO TRUE.

120800     PERFORM 3000-PRTRD-PROCESS-TRAN.
120900
P45097     IF (LPBAL-EDIT)
P45097        ADD 1                    TO WS-RESTART-RECORD-COUNT
P45097        GO TO 340-END.

121000     ADD  WF-HOURS               TO WS-ET-TOT-HOURS.
121100     ADD  WF-RATE                TO WS-ET-TOT-RATE.
121200
121300     ADD 1                       TO WS-RESTART-RECORD-COUNT.
121400     ADD 1                       TO WS-EMPLOYEE-TR-COUNT.
121500
121600     PERFORM 350-BUILD-TOTALS.
121700
121800 340-END.
121900     EXIT.
122000******************************************************************
122100 350-BUILD-TOTALS                SECTION.
122200******************************************************************
122300 350-START.
122400
122500     MOVE WS-FALSE               TO WS-BST-ELEMENT-FLAG.

J13588*    IF (EDCDWS-TRIGGER-ENABLED)
J13588     PERFORM 352-LP-BUILD-TOTALS
J13588         VARYING I1 FROM 1 BY 1
J13588         UNTIL  (I1 > 200)
J13588         OR     (WS-BST-ELEMENT-FOUND).
J13588*     GO TO 350-END. 
J13588      
J13588*    PERFORM 351-BUILD-TOTALS
J13588*        VARYING I1 FROM 1 BY 1
J13588*        UNTIL  (I1 > 200)
J13588*        OR     (WS-BST-ELEMENT-FOUND).
123000
123100 350-END.
123200     EXIT.
J13588******************************************************************
J13588*351-BUILD-TOTALS                SECTION.
J13588******************************************************************
J13588*351-START.

J13588*    IF (WS-BST-PAY-CODE (I1) = SPACES)
J13588*        MOVE WF-PAY-CODE        TO WS-BST-PAY-CODE  (I1)
J13588*        MOVE WF-HOURS           TO WS-BST-TOT-HOURS (I1)
J13588*        MOVE WF-RATE            TO WS-BST-TOT-RATE  (I1)
J13588*        MOVE WS-TRUE            TO WS-BST-ELEMENT-FLAG
J13588*        ADD  WF-HOURS           TO WS-BT-TOT-HOURS
J13588*        ADD  WF-RATE            TO WS-BT-TOT-RATE
J13588*    ELSE
J13588*    IF (WS-BST-PAY-CODE (I1) = WF-PAY-CODE)
J13588*        ADD  WF-HOURS           TO WS-BST-TOT-HOURS (I1)
J13588*        ADD  WF-RATE            TO WS-BST-TOT-RATE  (I1)
J13588*        MOVE WS-TRUE            TO WS-BST-ELEMENT-FLAG
J13588*        ADD  WF-HOURS           TO WS-BT-TOT-HOURS
J13588*        ADD  WF-RATE            TO WS-BT-TOT-RATE.

J13588*351-END.
125400     EXIT.
P45097******************************************************************
P45097 352-LP-BUILD-TOTALS               SECTION.
P45097******************************************************************
P45097 352-START.
P45097
P45097     IF  (WS-BST-PAY-CODE (I1) = SPACES)
J15961     AND (PRM-BATCH-NBR = LT1-TRD-BATCH-NBR)
P45097         MOVE LT1-TRD-PAY-CODE   TO WS-BST-PAY-CODE  (I1)
P45097         MOVE LT1-TRD-HOURS      TO WS-BST-TOT-HOURS (I1)
P45097         MOVE LT1-TRD-RATE       TO WS-BST-TOT-RATE  (I1)
P45097         MOVE WS-TRUE            TO WS-BST-ELEMENT-FLAG
P45097         ADD  LT1-TRD-HOURS      TO WS-BT-TOT-HOURS
P45097         ADD  LT1-TRD-RATE       TO WS-BT-TOT-RATE
P45097     ELSE
P45097     IF (WS-BST-PAY-CODE (I1) = LT1-TRD-PAY-CODE)
J15961     AND (PRM-BATCH-NBR = LT1-TRD-BATCH-NBR)
P45097         ADD  LT1-TRD-HOURS      TO WS-BST-TOT-HOURS (I1)
P45097         ADD  LT1-TRD-RATE       TO WS-BST-TOT-RATE  (I1)
P45097         MOVE WS-TRUE            TO WS-BST-ELEMENT-FLAG
P45097         ADD  LT1-TRD-HOURS      TO WS-BT-TOT-HOURS
P45097         ADD  LT1-TRD-RATE       TO WS-BT-TOT-RATE 
J15961     END-IF
J15961     END-IF.                                   
P45097
P45097 352-END.
P45097     EXIT.
125500******************************************************************
125600 360-PRINT-ERRORS                SECTION.
125700******************************************************************
125800 360-START.
125900
126000     MOVE WS-FALSE               TO WS-END-OF-FILE-SW.
126100
126200     OPEN INPUT ERROR-FILE.
126300
126400     READ ERROR-FILE INTO ER-ERROR-REC
126500         AT END
126600             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
126700
126800     IF (NOT-END-OF-FILE)
126900         MOVE "E"                TO PH-TYPE
127000         MOVE PAGE-HEADING       TO RPT-GROUP-REQUEST
127100         PERFORM 700-PRINT-RPT-GRP
127200         MOVE ERROR-HEADING      TO RPT-GROUP-REQUEST
127300         PERFORM 700-PRINT-RPT-GRP.
127400
127500     PERFORM 361-PRINT-ERRORS
127600         UNTIL (END-OF-FILE).
127700
127800 360-END.
127900     EXIT.
128000******************************************************************
128100 361-PRINT-ERRORS                SECTION.
128200******************************************************************
128300 361-START.
128400
128500     MOVE ER-EMPLOYEE            TO ED-EMPLOYEE.
128600     MOVE ER-MESSAGE             TO ED-MESSAGE.
128700
128800     MOVE ERR-DETAIL             TO RPT-GROUP-REQUEST.
128900     PERFORM 700-PRINT-RPT-GRP.
129000
129100     READ ERROR-FILE INTO ER-ERROR-REC
129200         AT END
129300             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
129400
129500 361-END.
129600     EXIT.
129700******************************************************************
129800 400-PRINT-PAGE-HEADING          SECTION.
129900******************************************************************
130000 400-START.
130100
130200     MOVE PRM-SELECTION          TO PH-TYPE.
130300
130400     MOVE PAGE-HEADING           TO RPT-GROUP-REQUEST.
130500     PERFORM 700-PRINT-RPT-GRP.
130600
130700 400-END.
130800     EXIT.
130900******************************************************************
131000 410-PRINT-BATCH-HEADING         SECTION.
131100******************************************************************
131200 410-START.
131300
131400     MOVE PRM-BATCH-NBR          TO BH-BATCH-NBR.
131500     MOVE PRM-TR-DATE            TO BH-TR-DATE.
131600
131700     MOVE BATCH-HEADING          TO RPT-GROUP-REQUEST.
131800     PERFORM 700-PRINT-RPT-GRP.
131900
132000 410-END.
132100     EXIT.
132200******************************************************************
132300 420-PRINT-DETAIL-HEADING        SECTION.
132400******************************************************************
132500 420-START.
132600
132700     MOVE DETAIL-HEADING         TO RPT-GROUP-REQUEST.
132800     PERFORM 700-PRINT-RPT-GRP.
132900
133000 420-END.
133100     EXIT.

134600******************************************************************
134700 440-PRINT-TIMERECORD            SECTION.
134800******************************************************************
134900 440-START.
135000
J13588*    IF (EDCDWS-TRIGGER-ENABLED)
J13588     MOVE LT1-TRD-PAY-CODE           TO TR-PAY-CODE.
J13588     MOVE LT1-TRD-HOURS              TO TR-HOURS.
J13588     MOVE LT1-TRD-RATE               TO TR-RATE.
J13588     MOVE LT1-TRD-CHECK-GRP          TO TR-CHECK-GRP.
J13588     MOVE LT1-TRD-PROCESS-GRP        TO TR-PROCESS-GRP.
J13588     MOVE LT1-TRD-TAX-FREQ-OVER      TO TR-TAX-FREQ-OVERRIDE.
J13588     MOVE LT1-TRD-DIST-COMPANY       TO TR-DIST-COMPANY.
J13588     MOVE LT1-TRD-DST-ACCT-UNIT      TO TR-DST-ACCT-UNIT.
J13588     MOVE LT1-TRD-DST-ACCOUNT        TO TR-DST-ACCOUNT.
J13588     MOVE LT1-TRD-DST-SUB-ACCT       TO TR-DST-SUB-ACCT.
J13588     MOVE LT1-TRD-ACTIVITY           TO TR-ACTIVITY.
J13588     MOVE LT1-TRD-ACCT-CATEGORY      TO TR-ACCT-CATEGORY.
J13588*    ELSE
J13588*       MOVE WF-PAY-CODE            TO TR-PAY-CODE
J13588*       MOVE WF-HOURS               TO TR-HOURS
J13588*       MOVE WF-RATE                TO TR-RATE
J13588*       MOVE WF-CHECK-GRP           TO TR-CHECK-GRP
J13588*       MOVE WF-PROCESS-GRP         TO TR-PROCESS-GRP
J13588*       MOVE WF-TAX-FREQ-OVER       TO TR-TAX-FREQ-OVERRIDE
J13588*       MOVE WF-DIST-COMPANY        TO TR-DIST-COMPANY
J13588*       MOVE WF-DST-ACCT-UNIT       TO TR-DST-ACCT-UNIT
J13588*       MOVE WF-DST-ACCOUNT         TO TR-DST-ACCOUNT
J13588*       MOVE WF-DST-SUB-ACCT        TO TR-DST-SUB-ACCT
J13588*       MOVE WF-ACTIVITY            TO TR-ACTIVITY
J13588*       MOVE WF-ACCT-CATEGORY       TO TR-ACCT-CATEGORY.
136600
136700     IF (COMMENTS-PRINTED)
136800     OR (EMP-TOTAL-PRINTED)
136900         MOVE WS-FALSE           TO WS-COMMENTS-SW
137000         MOVE WS-FALSE           TO WS-EMP-TOTAL-SW
137100         MOVE BLANK-LINE         TO RPT-GROUP-REQUEST
137200         PERFORM 700-PRINT-RPT-GRP.
137300
137400     MOVE TIME-RECORD            TO RPT-GROUP-REQUEST.
137500     PERFORM 700-PRINT-RPT-GRP.
137600
137700 440-END.
137800     EXIT.
137900******************************************************************
138000 450-PRINT-EMPLOYEE-TOTAL        SECTION.
138100******************************************************************
138200 450-START.
138300
138400     IF (WS-EMPLOYEE-TR-COUNT > 1)
138500         MOVE WS-TRUE                TO WS-EMP-TOTAL-SW
138600         MOVE EMPLOYEE-TOTAL         TO RPT-GROUP-REQUEST
138700         PERFORM 700-PRINT-RPT-GRP.
138800
138900     IF (PRM-PRINT-COMMENTS = "Y")
139000         MOVE 0                  TO DB-EMP-APP
139100         MOVE "TR"               TO DB-CMT-TYPE
139200         MOVE WS-EMPLOYEE        TO DB-EMPLOYEE
139300         MOVE SPACES             TO DB-JOB-CODE
139400                                    DB-ACTION-CODE
139500         MOVE ZEROS              TO DB-LN-NBR
139600         MOVE PACSET1-LN-NBR     TO WS-DB-BEG-RNG
139700         PERFORM 850-FIND-BEGRNG-PACSET1
               IF  (PACOMMENTS-FOUND)
139800             MOVE ZEROS              TO ECH-DATE
139900             MOVE SPACES             TO ECH-CMT-TEXT
J18280             PERFORM 860-FIND-NXTRNG-PACSET1
J18280               UNTIL (PACOMMENTS-NOTFOUND)
J18280                  OR (PAC-PRINT-CODE = "Y")
140000             PERFORM 460-PRINT-EMP-COMMENTS-HEADING
140100             PERFORM 860-FIND-NXTRNG-PACSET1
140200             MOVE SPACES             TO EC-CMT-TEXT
140300             PERFORM 470-PRINT-EMPLOYEE-COMMENTS
140400                 UNTIL (PACOMMENTS-NOTFOUND)
J18280         END-IF
J18280     END-IF.
140500
140600 450-END.
140700     EXIT.
140800******************************************************************
140900 460-PRINT-EMP-COMMENTS-HEADING  SECTION.
141000******************************************************************
141100 460-START.
141200
141300     IF (PACOMMENTS-NOTFOUND)
141400         GO TO 460-END.
141500
141600     MOVE PAC-DATE               TO ECH-DATE.
141700     MOVE PAC-CMT-TEXT           TO ECH-CMT-TEXT.
141800
141900     MOVE EMPLOYEE-COMMENTS-HEADING
142000                                 TO RPT-GROUP-REQUEST.
142100     PERFORM 700-PRINT-RPT-GRP.
142200
142300 460-END.
142400     EXIT.
142500******************************************************************
142600 470-PRINT-EMPLOYEE-COMMENTS     SECTION.
142700******************************************************************
142800 470-START.
142900
J18280     IF  (PAC-PRINT-CODE = "N")
J18280         GO TO 470-NEXT
J18280     END-IF.
J18280
143000     MOVE WS-TRUE                TO WS-COMMENTS-SW.
143100
143200     MOVE PAC-DATE               TO EC-DATE.
143300     MOVE PAC-CMT-TEXT           TO EC-CMT-TEXT.
143400
143500     MOVE EMPLOYEE-COMMENTS      TO RPT-GROUP-REQUEST.
143600     PERFORM 700-PRINT-RPT-GRP.
143700
J18280 470-NEXT.
J18280
143800     PERFORM 860-FIND-NXTRNG-PACSET1.
143900
144000 470-END.
144100     EXIT.
144200******************************************************************
144300 480-PRINT-BATCH-SUB-TOTAL       SECTION.
144400******************************************************************
144500 480-START.
144600
144700     IF (WS-BST-PAY-CODE  (I1) = SPACES)
144800         MOVE WS-FALSE           TO WS-BST-ELEMENT-FLAG
144900         GO TO 480-END.
145000
145100     MOVE WS-BST-PAY-CODE (I1)   TO BP-PAY-CODE.
145200     MOVE WS-BST-TOT-HOURS(I1)   TO BP-TOT-HOURS.
145300     MOVE WS-BST-TOT-RATE (I1)   TO BP-TOT-RATE.
145400
145500     MOVE BATCH-PC-TOTAL         TO RPT-GROUP-REQUEST.
145600     PERFORM 700-PRINT-RPT-GRP.
145700
145800     MOVE ZEROS                  TO BPH-BATCH-NBR.
145900
146000 480-END.
146100     EXIT.
146200******************************************************************
146300 490-PRINT-BATCH-TOTAL           SECTION.
146400******************************************************************
146500 490-START.
146600
146700     MOVE WS-BT-TOT-HOURS        TO BT-TOT-HOURS.
146800     MOVE WS-BT-TOT-RATE         TO BT-TOT-RATE.
146900
147000     MOVE BATCH-TOTAL            TO RPT-GROUP-REQUEST.
147100     PERFORM 700-PRINT-RPT-GRP.
147200
147300 490-END.
147400     EXIT.

      ******************************************************************
       660-PROCESS-LP-TIMEREC          SECTION.
      ******************************************************************
       660-START.
P45097* CHANGES IN THIS SECTION MADE FOR PT 145097. 

           MOVE ZEROS                  TO WS-LP-EMP-REC-ERR
                                          WS-LP-EMP-REC-OK.
P60307     INITIALIZE LPBAL-EMP-LP-TRD-DELETED-SW.

P60307     MOVE 20 TO LPBAL-SQ-MAX.
.          PERFORM 
.          VARYING LPBAL-SQ-I FROM 1 BY 1
.          UNTIL (LPBAL-SQ-I > LPBAL-SQ-MAX)
.                MOVE ZEROS TO LPBAL-TM-SQ     (LPBAL-SQ-I)
.                              LPBAL-LNK-TM-SQ (LPBAL-SQ-I)
P60307     END-PERFORM.

           MOVE WS-EMPLOYEE            TO LT1-TRD-EMPLOYEE.
           MOVE ZEROES                 TO LT1-TRD-TIME-SEQ.

           PERFORM 8500-FIND-NLT-LPBALTRD.

P45097     PERFORM 662-PROCESS-LP-TIMERECS
P45097        UNTIL (LPBALTRD-NOTFOUND)
P45097        OR    (LT1-TRD-EMPLOYEE NOT = WS-EMPLOYEE).

027084     IF (LPBAL-EMP-LP-TRD-DELETED)
.             MOVE WS-EMPLOYEE            TO LT1-TRD-EMPLOYEE
.             MOVE ZEROES                 TO LT1-TRD-TIME-SEQ
.             PERFORM 8500-FIND-NLT-LPBALTRD
.             PERFORM 664-PROCESS-LP-TIMERECS
.               UNTIL (LPBALTRD-NOTFOUND)
P60307          OR    (LT1-TRD-EMPLOYEE NOT = WS-EMPLOYEE).

       660-END.
P45097******************************************************************
P45097 662-PROCESS-LP-TIMERECS         SECTION.
P45097******************************************************************
P45097 662-START.
P45097* SECTION ADDED FOR CHANGES MADE FOR PT 145097. 
P45097
P45097     IF (LT1-EXISTING-TRD NOT = ZEROES)
P45097         GO TO 662-NEXT.
P60307
.          IF (LT1-TRD-TIME-ACC-FLAG = "A")
P60307        GO TO 662-NEXT.
P45097
P45097     INITIALIZE LPBAL-BATCH-ERR-FND-SW.
P45097     IF (LPBAL-SEND-ERROR)
P45097        PERFORM
P45097          VARYING I9 FROM 1 BY 1
P45097          UNTIL (I9 > LPBAL-BATCH-ERR-SIZE)
P45097          OR    (LPBAL-BATCH-ERR-EMP (I9) = ZEROS)
P45097          OR    (LPBAL-BATCH-ERR-FND)
P45097             IF  (LT1-TRD-EMPLOYEE  = 
P45097                  LPBAL-BATCH-ERR-EMP     (I9))  
P45097             AND (LT1-TRD-PAY-CODE  =
P45097                  LPBAL-BATCH-ERR-PAY-CDE (I9)) 
P45097                 SET LPBAL-BATCH-ERR-FND TO TRUE
P45097             END-IF
P45097        END-PERFORM
P45097     END-IF.
P45097
P45097* ERROR RETURNED FROM LPBAL FOR PAY CODE, SKIP RECORD.
P45097     IF (LPBAL-BATCH-ERR-FND)
P45097        MOVE 1 TO WS-LP-EMP-REC-ERR
P45097        GO TO 662-NEXT.
P45097
      *DELETE EXISTING TRD RECORDS FOR EMP THAT WERE CREATED PREVIOUSLY
      *BY LINKED PLANS (OFFSET, TRANSFER, USAGE, RECLASSIFY),
P60307    IF (LPBAL-EMP-LP-TRD-NOT-DELETED)
P60307        MOVE PRM-COMPANY            TO DB-COMPANY
P60307        MOVE LT1-TRD-EMPLOYEE       TO DB-EMPLOYEE
P60307        PERFORM 840-FIND-EMPSET1
P60307        IF (EMPLOYEE-NOTFOUND)
P60307           GO TO 662-NEXT
P60307        END-IF
P60307        MOVE PRM-COMPANY            TO LPBAL-TR-COMPANY
P60307        MOVE EMP-PROCESS-LEVEL      TO LPBAL-TR-PROC-LEVEL
P60307        MOVE LT1-TRD-EMPLOYEE       TO LPBAL-TR-EMPLOYEE
P60307        PERFORM 7550-DELETE-LP-CREATED-TIMERECS.
P60307
P45097     MOVE 1 TO WS-LP-EMP-REC-OK.
P45097
P60307     PERFORM 
.          VARYING LPBAL-SQ-I FROM 1 BY 1
.          UNTIL (LPBAL-SQ-I > LPBAL-SQ-MAX)
.          OR    (LPBAL-TM-SQ (LPBAL-SQ-I) = ZEROS)
.                CONTINUE
.          END-PERFORM.
.          IF  (LPBAL-SQ-I NOT > LPBAL-SQ-MAX)
.          AND (LPBAL-TM-SQ (LPBAL-SQ-I) = ZEROS)
.             MOVE LT1-TRD-TIME-SEQ TO LPBAL-TM-SQ   (LPBAL-SQ-I)
.                                    LPBAL-LNK-TM-SQ (LPBAL-SQ-I)
.          END-IF.
P60307
P45097* FOLLOWING CODE WAS COPIED FROM SECTION 330-CREATE-TIMERECORD.
P45097     PERFORM 670-CREATE-LP-TIMERECORD.
P45097
J29267     IF  (PRM-BATCH-NBR = LT1-TRD-BATCH-NBR)
J29267         ADD  LT1-TRD-HOURS          TO WS-ET-TOT-HOURS 
J29267         ADD  LT1-TRD-RATE           TO WS-ET-TOT-RATE 
J29267
J29267         ADD 1                       TO WS-EMPLOYEE-TR-COUNT 
J29267
J29267         PERFORM 350-BUILD-TOTALS 
J29267     END-IF.
P60307* SET UP NEW TIME SEQ NBR.
.          IF (LPBAL-SQ-I NOT > LPBAL-SQ-MAX)
.             MOVE PRTRD-TIME-SEQ TO LPBAL-TM-SQ (LPBAL-SQ-I).
P60307
P45097     PERFORM 440-PRINT-TIMERECORD.
P45097
P45097     IF (LT1-TRD-JOB-CODE      NOT = SPACES)
P45097     OR (LT1-TRD-POSITION      NOT = SPACES)
P45097     OR (LT1-TRD-LOCAT-CODE    NOT = SPACES)
P45097     OR (LT1-TRD-DIST-PROC-LEV NOT = SPACES)
P45097     OR (LT1-TRD-DIST-DEPART   NOT = SPACES)
P45097         MOVE LT1-TRD-JOB-CODE        TO TR2-JOB-CODE
P45097         MOVE LT1-TRD-POSITION        TO TR2-POSITION
P45097         MOVE LT1-TRD-LOCAT-CODE      TO TR2-LOCAT-CODE
P45097         MOVE LT1-TRD-DIST-PROC-LEV   TO TR2-DIST-PROC-LEV
P45097         MOVE LT1-TRD-DIST-DEPART     TO TR2-DIST-DEPART
P45097         MOVE TIME-RECORD2       TO RPT-GROUP-REQUEST
P45097         PERFORM 700-PRINT-RPT-GRP
P45097
P45097     IF (LT1-TRD-SCHEDULE      NOT = SPACES)
P45097     OR (LT1-TRD-PAY-STEP      NOT = ZEROS)
P45097     OR (LT1-TRD-PAY-GRADE     NOT = SPACES)
P45097     OR (LT1-TRD-REASON-CODE   NOT = SPACES)
P45097     OR (LT1-TRD-ATTEND-CODE   NOT = SPACES)
P45097     OR (LT1-TRD-OCCURRENCE    NOT = SPACES)
P45097         MOVE LT1-TRD-SCHEDULE        TO TR3-SCHEDULE
P45097         MOVE LT1-TRD-PAY-STEP        TO TR3-PAY-STEP
P45097         MOVE LT1-TRD-PAY-GRADE       TO TR3-PAY-GRADE
P45097         MOVE LT1-TRD-REASON-CODE     TO TR3-REASON-CODE
P45097         MOVE LT1-TRD-ATTEND-CODE     TO TR3-ATTEND-CODE
P45097         MOVE LT1-TRD-OCCURRENCE      TO TR3-OCCURRENCE
P45097         MOVE TIME-RECORD3       TO RPT-GROUP-REQUEST
P45097         PERFORM 700-PRINT-RPT-GRP
P45097*--- Begin SL changes
P45097         IF (WF-SEGMENT-FLAG = "Y")
P45097             MOVE LT1-TRD-DIST-COMPANY TO WF-DIST-COMPANY
P45097             MOVE LT1-PRTRD-SEGMENT-BLOCK 
P45097                                       TO WF-SEGMENT-BLOCK
P45097             PERFORM 331-DO-SEGMENT-BLOCK
P45097*            THRU    331-END
P45097         END-IF
P45097*--- End SL changes
P45097     END-IF.
P45097
P45097 662-NEXT.
P45097
P45097     PERFORM 8600-FIND-NEXT-LPBALTRD.
P45097
P45097 662-END.
P60307******************************************************************
.      664-PROCESS-LP-TIMERECS         SECTION.
.     ******************************************************************
.      664-START.
.     
.          IF (LT1-EXISTING-TRD NOT = ZEROES)
.              GO TO 664-NEXT.
.   
027084     IF (LT1-TRD-TIME-ACC-FLAG = "A")
.             GO TO 664-NEXT.
.     
.          INITIALIZE LPBAL-BATCH-ERR-FND-SW.
.          IF (LPBAL-SEND-ERROR)
.             PERFORM
.               VARYING I9 FROM 1 BY 1
.               UNTIL (I9 > LPBAL-BATCH-ERR-SIZE)
.               OR    (LPBAL-BATCH-ERR-EMP (I9) = ZEROS)
.               OR    (LPBAL-BATCH-ERR-FND)
.                  IF  (LT1-TRD-EMPLOYEE  = 
.                       LPBAL-BATCH-ERR-EMP     (I9))  
.                  AND (LT1-ORIG-REC-PAY-CODE  =
.                       LPBAL-BATCH-ERR-PAY-CDE (I9)) 
.                      SET LPBAL-BATCH-ERR-FND TO TRUE
.                  END-IF
.             END-PERFORM
.          END-IF.
.     
.          IF (LPBAL-BATCH-ERR-FND)
.             GO TO 664-NEXT.
.     
.          PERFORM 
.          VARYING LPBAL-SQ-I FROM 1 BY 1
.          UNTIL (LPBAL-SQ-I > LPBAL-SQ-MAX)
.          OR    (LPBAL-TM-SQ (LPBAL-SQ-I) = ZEROS)
.          OR    (LPBAL-LNK-TM-SQ (LPBAL-SQ-I) = 
.                               LT1-TRD-LNK-TIME-SEQ)
.                CONTINUE
.          END-PERFORM.
.          IF (LPBAL-LNK-TM-SQ (LPBAL-SQ-I) = 
.                            LT1-TRD-LNK-TIME-SEQ)
.             MOVE LPBAL-TM-SQ (LPBAL-SQ-I) TO 
.                           LT1-TRD-LNK-TIME-SEQ.
.   
.          PERFORM 670-CREATE-LP-TIMERECORD.
.     
J15961     IF  (PRM-BATCH-NBR NOT = LT1-TRD-BATCH-NBR)
J15961         GO TO 664-NEXT
J15961     END-IF.
J15961

J29267     IF (LPBAL-BYPASS-CTR NOT = ZEROES)
J29267         COMPUTE LPBAL-BYPASS-CTR = LPBAL-BYPASS-CTR - 1
J29267         GO TO 664-NEXT
J29267     END-IF.

J29267     IF  (PRM-BATCH-NBR = LT1-TRD-BATCH-NBR)
J29267         ADD  LT1-TRD-HOURS          TO WS-ET-TOT-HOURS 
J29267         ADD  LT1-TRD-RATE           TO WS-ET-TOT-RATE 
J29267
J29267         ADD 1                       TO WS-EMPLOYEE-TR-COUNT 
J29267
J29267         PERFORM 350-BUILD-TOTALS 
J29267     END-IF.

.          PERFORM 440-PRINT-TIMERECORD.
.     
.          IF (LT1-TRD-JOB-CODE      NOT = SPACES)
.          OR (LT1-TRD-POSITION      NOT = SPACES)
.          OR (LT1-TRD-LOCAT-CODE    NOT = SPACES)
.          OR (LT1-TRD-DIST-PROC-LEV NOT = SPACES)
.          OR (LT1-TRD-DIST-DEPART   NOT = SPACES)
.              MOVE LT1-TRD-JOB-CODE        TO TR2-JOB-CODE
.              MOVE LT1-TRD-POSITION        TO TR2-POSITION
.              MOVE LT1-TRD-LOCAT-CODE      TO TR2-LOCAT-CODE
.              MOVE LT1-TRD-DIST-PROC-LEV   TO TR2-DIST-PROC-LEV
.              MOVE LT1-TRD-DIST-DEPART     TO TR2-DIST-DEPART
.              MOVE TIME-RECORD2       TO RPT-GROUP-REQUEST
.              PERFORM 700-PRINT-RPT-GRP
.     
.          IF (LT1-TRD-SCHEDULE      NOT = SPACES)
.          OR (LT1-TRD-PAY-STEP      NOT = ZEROS)
.          OR (LT1-TRD-PAY-GRADE     NOT = SPACES)
.          OR (LT1-TRD-REASON-CODE   NOT = SPACES)
.          OR (LT1-TRD-ATTEND-CODE   NOT = SPACES)
.          OR (LT1-TRD-OCCURRENCE    NOT = SPACES)
.              MOVE LT1-TRD-SCHEDULE        TO TR3-SCHEDULE
.              MOVE LT1-TRD-PAY-STEP        TO TR3-PAY-STEP
.              MOVE LT1-TRD-PAY-GRADE       TO TR3-PAY-GRADE
.              MOVE LT1-TRD-REASON-CODE     TO TR3-REASON-CODE
.              MOVE LT1-TRD-ATTEND-CODE     TO TR3-ATTEND-CODE
.              MOVE LT1-TRD-OCCURRENCE      TO TR3-OCCURRENCE
.              MOVE TIME-RECORD3       TO RPT-GROUP-REQUEST
.              PERFORM 700-PRINT-RPT-GRP
.     *--- Begin SL changes
.              IF (WF-SEGMENT-FLAG = "Y")
.                  MOVE LT1-TRD-DIST-COMPANY TO WF-DIST-COMPANY
.                  MOVE LT1-PRTRD-SEGMENT-BLOCK 
.                                            TO WF-SEGMENT-BLOCK
.                  PERFORM 331-DO-SEGMENT-BLOCK
.     *            THRU    331-END
.              END-IF
.     *--- End SL changes
.          END-IF.
.     
.      664-NEXT.
.     
.          PERFORM 8600-FIND-NEXT-LPBALTRD.
.     
P60307 664-END.
P45097******************************************************************
P45097 670-CREATE-LP-TIMERECORD        SECTION.
P45097******************************************************************
P45097 670-START.
P45097* SECTION ADDED FOR CHANGES MADE FOR PT 145097. 
P45097
P45097     INITIALIZE PRTRD-SCR-FIELDS.
P45097
P45097     MOVE "A"                    TO PRTRD-FC.
P45097     MOVE PRM-COMPANY            TO PRTRD-COMPANY.
J15961     MOVE LT1-TRD-BATCH-NBR      TO PRTRD-BATCH-NBR.
P45097     MOVE LT1-TRD-EMPLOYEE       TO PRTRD-EMPLOYEE.
P45097     MOVE LT1-TRD-TIME-SEQ       TO PRTRD-TIME-SEQ.
P45097     MOVE LT1-TRD-BATCH-NBR      TO PRTRD-BATCH-NBR.
P45097     MOVE LT1-TRD-ATTEND-CODE    TO PRTRD-ATTEND-CODE.
P45097     MOVE LT1-TRD-OCCURRENCE     TO PRTRD-OCCURRENCE.
P45097     MOVE LT1-TRD-PAY-CODE       TO PRTRD-PAY-CODE.
P45097     MOVE LT1-TRD-SHIFT          TO PRTRD-SHIFT.
P45097     MOVE LT1-TRD-CHECK-GRP      TO PRTRD-CHECK-GRP.
P45097     MOVE LT1-TRD-PROCESS-GRP    TO PRTRD-PROCESS-GRP.
P45097     MOVE LT1-TRD-TAX-FREQ-OVER  TO PRTRD-TAX-FREQ-OVER.
P45097     MOVE LT1-TRD-TR-DATE        TO PRTRD-TR-DATE.
P45097     MOVE LT1-TRD-DIST-COMPANY   TO PRTRD-DIST-COMPANY.
P45097     MOVE LT1-TRD-DST-ACCT-UNIT  TO PRTRD-DST-ACCT-UNIT.
P45097     MOVE LT1-TRD-DST-ACCOUNT    TO PRTRD-DST-ACCOUNT.
P45097     MOVE LT1-TRD-DST-SUB-ACCT   TO PRTRD-DST-SUB-ACCT.
P45097     MOVE LT1-TRD-ACTIVITY       TO PRTRD-ACTIVITY.
P45097     MOVE LT1-TRD-ACCT-CATEGORY  TO PRTRD-ACCT-CATEGORY.
P45097     MOVE LT1-TRD-DIST-PROC-LEV  TO PRTRD-DIST-PROC-LEV.
P45097     MOVE LT1-TRD-DIST-DEPART    TO PRTRD-DIST-DEPART.
P45097     MOVE LT1-TRD-PCT-DIST-FLAG  TO PRTRD-PCT-DIST-FLAG.
P45097     MOVE LT1-TRD-JOB-CODE       TO PRTRD-JOB-CODE.
P45097     MOVE LT1-TRD-HOURS          TO PRTRD-HOURS.
P45097     MOVE LT1-TRD-PAY-UNITS      TO PRTRD-PAY-UNITS.
P45097     MOVE LT1-TRD-UNIT-MEASURE   TO PRTRD-UNIT-MEASURE.
P45097     MOVE LT1-TRD-NON-EARNINGS   TO PRTRD-NON-EARNINGS.
P45097     MOVE LT1-TRD-RATE           TO PRTRD-RATE.
P45097     MOVE LT1-TRD-SCHEDULE       TO PRTRD-SCHEDULE.
P45097     MOVE LT1-TRD-PAY-STEP       TO PRTRD-PAY-STEP.
P45097     MOVE LT1-TRD-PAY-GRADE      TO PRTRD-PAY-GRADE.
P45097     MOVE LT1-TRD-PER-END-DATE   TO PRTRD-PER-END-DATE.
P45097     MOVE LT1-TRD-WRK-END-DATE   TO PRTRD-WRK-END-DATE.
028335     MOVE LT1-TRD-PER-BEG-DATE   TO PRTRD-PER-BEG-DATE.
028335     MOVE LT1-TRD-WRK-BEG-DATE   TO PRTRD-WRK-BEG-DATE.
P45097     MOVE LT1-TRD-WC-STATE       TO PRTRD-WC-STATE.
P45097     MOVE LT1-TRD-LOCAT-CODE     TO PRTRD-LOCAT-CODE.
P45097     MOVE LT1-TRD-PAY-PERIODS    TO PRTRD-PAY-PERIODS.
P45097     MOVE LT1-TRD-ANNUAL-SALARY  TO PRTRD-ANNUAL-SALARY.
P45097     MOVE LT1-TRD-PAY-SUM-GRP    TO PRTRD-PAY-SUM-GRP.
P45097     MOVE LT1-TRD-PCD-SEQ-NBR    TO PRTRD-PCD-SEQ-NBR.
P45097     MOVE LT1-TRD-WC-CLASS       TO PRTRD-WC-CLASS.
P45097     MOVE LT1-TRD-CA-WC-CLASS    TO PRTRD-CA-WC-CLASS.
P45097     MOVE LT1-TRD-SUPP-TAX-CODE  TO PRTRD-SUPP-TAX-CODE.
P45097     MOVE LT1-TRD-WAGE-AMOUNT    TO PRTRD-WAGE-AMOUNT.
P45097     MOVE LT1-TRD-OT-PREM-AMT    TO PRTRD-OT-PREM-AMT.
P45097     MOVE LT1-TRD-OT-RATE        TO PRTRD-OT-RATE.
P45097     MOVE LT1-TRD-SHIFT-DIFF     TO PRTRD-SHIFT-DIFF.
P45097     MOVE LT1-TRD-OT-RECORD      TO PRTRD-OT-RECORD.
P45097     MOVE LT1-TRD-PROCESS-LEVEL  TO PRTRD-PROCESS-LEVEL.
P45097     MOVE LT1-TRD-DEPARTMENT     TO PRTRD-DEPARTMENT.
P45097     MOVE LT1-TRD-WORK-STATE     TO PRTRD-WORK-STATE.
P45097     MOVE LT1-TRD-TAX-STATE      TO PRTRD-TAX-STATE.
P45097     MOVE LT1-TRD-TAX-CITY       TO PRTRD-TAX-CITY.
P45097     MOVE LT1-TRD-TAX-COUNTY     TO PRTRD-TAX-COUNTY.
P45097     MOVE LT1-TRD-TAX-SCHOOL     TO PRTRD-TAX-SCHOOL.
P45097     MOVE LT1-TRD-UNION-CODE     TO PRTRD-UNION-CODE.
P45097     MOVE LT1-TRD-POSITION       TO PRTRD-POSITION.
P45097     MOVE LT1-TRD-TIME-ACC-FLAG  TO PRTRD-TIME-ACC-FLAG.
P45097     MOVE LT1-TRD-SHFT-DIFF-RATE TO PRTRD-SHIFT-DIFF-AMT.
P45097     MOVE LT1-TRD-SH-OBJ-ID      TO PRTRD-SH-OBJ-ID.
P45097     MOVE LT1-TRD-CURRENCY-CODE  TO PRTRD-CURRENCY-CODE.
P45097     MOVE LT1-TRD-CURR-ND        TO PRTRD-CURR-ND.
P45097     MOVE LT1-TRD-COUNTRY-CODE   TO PRTRD-COUNTRY-CODE.
P45097     MOVE LT1-TRD-PAYMENT-TYPE   TO PRTRD-PAYMENT-TYPE.
P45097     MOVE LT1-TRD-REMUN-CODE     TO PRTRD-REMUN-CODE.
P45097     MOVE LT1-TRD-REPORT-ENTITY  TO PRTRD-REPORT-ENTITY.
P45097     MOVE LT1-TRD-BUS-NBR-GRP    TO PRTRD-BUS-NBR-GRP.
P45097     MOVE LT1-TRD-QC-ENT-NBR-GRP TO PRTRD-QC-ENT-NBR-GRP.
P45097     MOVE LT1-TRD-PENS-SEQ-NBR   TO PRTRD-PENS-SEQ-NBR.
P45097     MOVE LT1-TRD-LEAVE-STATUS   TO PRTRD-LEAVE-STATUS.
P45097     MOVE LT1-TRD-RETRO-PENSION  TO PRTRD-RETRO-PENSION.
P45097     MOVE LT1-TRD-REASON-CODE    TO PRTRD-REASON-CODE.
P45097     MOVE LT1-TRD-SERVICE-CODE   TO PRTRD-SERVICE-CODE.
P45097     MOVE LT1-TRD-CHECK-TYPE     TO PRTRD-CHECK-TYPE.
P45097     MOVE LT1-TRD-ERROR-FLAG     TO PRTRD-ERROR-FLAG.
P45097     MOVE LT1-TRD-ATN-OBJ-ID     TO PRTRD-ATN-OBJ-ID.
P45097     MOVE LT1-TRD-AC-UPDATED     TO PRTRD-AC-UPDATED.
P45097     MOVE LT1-TRD-GLT-OBJ-ID     TO PRTRD-GLT-OBJ-ID.
P45097     MOVE LT1-TRD-ORIG-OBJ-ID    TO PRTRD-ORIG-OBJ-ID.
P45097     MOVE LT1-TRD-RECORD-TYPE    TO PRTRD-RECORD-TYPE.
P45097     MOVE LT1-TRD-USER-ID        TO PRTRD-USER-ID.
P45097     MOVE LT1-TRD-DATE-STAMP     TO PRTRD-DATE-STAMP.
P54585     MOVE LT1-TRD-TIME-STAMP     TO PRTRD-TIME-STAMP.
P54585     MOVE LT1-TRD-CREATE-DATE    TO PRTRD-CREATE-DATE.
P54585     MOVE LT1-TRD-CREATE-TIME    TO PRTRD-CREATE-TIME.
P54585     MOVE LT1-TRD-CREATE-USER-ID TO PRTRD-CREATE-USER-ID.
P45097     MOVE LT1-TRD-CHECK-ID       TO PRTRD-CHECK-ID.
J15961     MOVE LT1-PRTRD-STATUS       TO PRTRD-STATUS.
J11610     MOVE LT1-TRD-ADDTL-RATE     TO PRTRD-ADDTL-RATE.
P45097
P45097     MOVE LT1-PRTRD-SEGMENT-BLOCK TO PRTRD-SEGMENT-BLOCK.
P45097
P60307     MOVE LT1-TRD-LNK-TIME-SEQ   TO PRTRD-LNK-TIME-SEQ.
J57836     MOVE LT1-TRD-NOTIONAL       TO PRTRD-NOTIONAL.
P60307
P45097     PERFORM 3000-PRTRD-PROCESS-TRAN.
P45097
J29267*    IF  (PRM-BATCH-NBR = LT1-TRD-BATCH-NBR)
J29267*        ADD  LT1-TRD-HOURS          TO WS-ET-TOT-HOURS 
J29267*        ADD  LT1-TRD-RATE           TO WS-ET-TOT-RATE 
J29267*
J29267*        ADD 1                       TO WS-EMPLOYEE-TR-COUNT 
J29267*
J29267*        PERFORM 350-BUILD-TOTALS 
J29267*    END-IF.
P45097
P45097 670-END.
