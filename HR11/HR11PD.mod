******* HR11PD 97.1.15.1.11 <453118188>
000100******************************************************************
000200*                             HR11PD                             *
000300******************************************************************
      *                                                                *
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------- *
      *  119305 | J19305 | MADE CHANGES TO DEFAULT THE POSITION LEVEL  *
      *         |        | TO 1 IN POSITION LEVEL SCREEN FIELD         *
      *  ------   ------   ------------------------------------------  *
      *  313588 | J13588 | REMOVE LP TRIGGER CHECKS                    *
      *  ------ | ------ | ------------------------------------------- *
      *  327500 | J27500 | DUAL LANGUAGE EMPLOYEE NAME FEATURE         *
      *  ------ | ------ | ------------------------------------------- *
      *  356096 | J56096 | MOVE VALUE TO DB-INDICATOR WHEN LOCATING    *
      *         |        | DESCRIPTION TO PRSAGHEAD TABLE.             *
      *  ------   ------   ------------------------------------------- *
      *  377218 | J77218 | MOVE PRIMARY AND SECONDARY CERTIFIER COMPANY*
      *         |        | VALUES TO RETRIEVE AND DISPLAY CORRECT FULL *
      *         |        | NAME IN GRANT MANAGEMENT (HR11.3)           *
      *  ------   ------   ------------------------------------------- *
      *  304456 | J04456 | NEW MEXICO'S ENHANCEMENT CHANGES FOR ICESA  *
      *         |        | MMREF, CSV AND XML FORMATS; ENABLED INVOKE  *
      *         |        | OF EMPLOYEE LEVEL REPORT FORM (HR18.6) FOR  *
      *         |        | NEW MEXICO.                                 *
      *  ------   ------   ------------------------------------------- *
      *  395772 | J96772 | ALLOW FOR BYPASS OF PERSONNEL ACTION ONLY   *
      *         |        | CHECKS IN HREMPSEC                          *
      *  ------   ------   --------------------------------------------*
      *  453125 | J53125 | ADD NEW SCREEN FOR BOD INQUIRE ACTION ONLY  *
      *  ------   ------   --------------------------------------------*
      *  584780 | J84780 | LIST VIEW FOR HR11.4                        *
      *  ------   ------   --------------------------------------------*
      *  666361 | J66361 | DISTRICT OF COLUMBIA ENHANCEMENT TO ENABLE  *
      *         |        | EMPLOYEE LEVEL REPORT FORM.                 *
      *  ------   ------   --------------------------------------------*
      *  608890 | J08890 | ALLTAX Group Code Descrioption change       *
      *  ------   ------   ------------------------------------------  *
      *  719381 | J19381 | ME PAYROLL ENHANCEMENTS                     *
      *         |        | ADDED HR11.5 - EMPLOYEE ADDITIONAL DETAILS  *
      *  ------   ------   ------------------------------------------  *
      *  741902 | J41902 | ENHANCED FOR ME PAYROLL - PAY PLAN ENHANCEM-*
      *         |        | ENT                                         *
      *  ------   ------   ------------------------------------------  *
      *  775684 | J75684 | ADDED VALIDATION FOR FICA NBR PRIOR TO      *
      *         |        | HREMPPD PROCESSING                          *
      *  ------   ------   --------------------------------------------*
      *  850169 | J50169 | Initialize filter string in HR11.3          *
      *  ------   ------   --------------------------------------------*
      *  860623 | J60623 | FIXED INQUIRE ISSUES ON DIFFERENT COMPANY OR*
      *         |        | EMPLOYEE IN HR11.5                          *
      *  ------   ------   --------------------------------------------*
      *  618922 | J18922 | Changes for transferring to HR11.4 for BoD  *
      *  ------   ------   --------------------------------------------*
      *  964983 | J64983 | REMOVED CHANGES MADE BY JT-140211 ON HR11PD *
      *  ------   ------   --------------------------------------------*
      * 1108049 | 108049 | ENHANCEMENT FOR SOUTH CAROLINA'S HR18.8     *
      *  ------   ------   ------------------------------------------- *
      * 1180940 | 180940 | Added code to call HR18.9 from HR11         *
      *  ------   ------   --------------------------------------------*
      * 1173217 | J73217 | ADD GENDER IDENTITY AND SEXUAL ORIENTATION  *
      *  ------   ------   ------------------------------------------- *
      * 1277456 | 277456 | Added code to call HR23.1 from HR11         *
      *  ------   ------   ------------------------------------------- *
      *         |        |                                             *
ACS001******************************************************************
ACS001*CHANGE LOG                                                      *
ACS001*                                                                *
ACS001* MOD NBR   DATE        PROGRAMMER   DESCRIPTION                 *
ACS001*--------  ----------  ------------  ----------------------------*
ACS001*ACS001    04/22/2009  M. HUNTER     REAPPLIED CUSTOMIZATIONS    *
ACS001*                                    AFTER CTP 58237 WAS APPLIED *
ACS001*                                                                *
ACS002*ACS001    10/27/2010  M. HUNTER     REAPPLIED CUSTOMIZATIONS    *
ACS002*                      ACS           AFTER 9.0 APPS UPGRADE      *
ACS002*ACS001    08/24/2011  M. HUNTER     REAPPLIED CUSTOMIZATIONS    *
ACS002*                      ACS           AFTER 9.0.1 APPS UPGRADE    *
000400******************************************************************
000500 HR11S1-TRANSACTION              SECTION 24.
000600******************************************************************
000700 HR11S1-START.
000800
000900     INITIALIZE                         DB-COMPANY
001000                                        DB-PROCESS-LEVEL
001100                                        DB-DEPARTMENT
001200                                        DB-EFFECT-DATE.

           INITIALIZE PRPXL-TAXES.
P59135     INITIALIZE PRPXL-PARM-EFF-DATE.
           SET PRPXL-NO-WH TO TRUE.

P60986     IF (HR11F1-FC = "A" OR "C" OR "D")
P60986         PERFORM 1000-OPEN-WORKFLOW-DB.
P60986
           IF  (HR11F1-FC NOT = "A" AND "C")
           AND (HR11F1-PT-FROM-PR134 = ZEROES)
               INITIALIZE HR11F1-XMIT-HREMP-BLOCK.

           IF  (HR11F1-FC NOT = "R")
               INITIALIZE HR11F1-XMIT-REQDED.

J73217     IF  (HR11F1-FC NOT = "A" AND "C" AND "D")
J73217         INITIALIZE HR11F1-PEM-GENDER-IDENTITY
J73217                    HR11F1-PEM-SEX-ORIENTATION.

001300     PERFORM 850-FIND-NLT-PPRSET1.
001400     IF (PAPOSRULE-FOUND)
001500         MOVE WS-TRUE                TO HREMP-PAUSER-FLAG-SW.

J08890* Check to see if company is using ALLTAX.
J08890     SET NOT-ALLTAX                  TO TRUE.
J08899     MOVE HR11F1-EMP-COMPANY         TO ATX-COMPANY.
J08899     PERFORM 9900-ATX-DBG-DISPLAY-SETUP.
J08890
J08890     IF (ALLTAX)
J08890         MOVE "ALLTAX"               TO HR11F1-GROUP-TYPE
J08890     ELSE
J08890         MOVE "   BSI"               TO HR11F1-GROUP-TYPE
J08890     END-IF.
001600
001700     IF (HR11F1-FC = "F")
001800         PERFORM 650-DEFAULT
001900         THRU    650-END
002000         GO TO HR11S1-TRANSACTION-END.
002100
002200     IF (HR11F1-FC = "G" OR "J" OR "M" OR "T" OR "W" OR "X" OR
J27500*                    "Y" OR "Z" OR "H" OR "V")
J19381                     "Y" OR "Z" OR "H" OR "V" OR "O")
002300         PERFORM 700-SCREEN-XFER
002400         THRU    700-END
002500         GO TO HR11S1-TRANSACTION-END.
002600
           IF (HR11F1-FC = "I")
               MOVE HR11F1-EMP-COMPANY     TO DB-COMPANY
               MOVE HR11F1-EMP-EMPLOYEE    TO DB-EMPLOYEE
               PERFORM 840-FIND-EMPSET1.

           IF (HR11F1-FC                  = "+" OR "-")
               IF (HR11F1-HREMP-COMPANY  NOT = HR11F1-EMP-COMPANY)
               OR (HR11F1-HREMP-EMPLOYEE NOT = HR11F1-EMP-EMPLOYEE)
P81162*        OR (HR11F1-EMP-EMPLOYEE   NOT = HREMP-EMPLOYEE)
                   MOVE 150                    TO CRT-ERROR-NBR
                   MOVE HR11F1-EMP-EMPLOYEE-FN TO CRT-FIELD-NBR
                   GO TO HR11S1-TRANSACTION-END.

           IF   (HR11F1-EMP-EMPLOYEE    NOT = HR11F1-PT-EMP-EMPLOYEE)
           AND ((HR11F1-FC                 = "+")
           OR   (HR11F1-FC                 = "-"))
                   MOVE "+"                    TO HR11F1-FC
011900             MOVE 1                      TO HR11F1-TOP-REC
011900                                            HR11F1-REC
                                                  HR11F1-I1.

           SET HR11WS-HRPADICT2-NOTOPEN    TO TRUE.

           IF (HR11F1-FC                  NOT = "+" AND "-")
           OR (HR11F1-EMP-EMPLOYEE        NOT = HR11F1-PT-EMP-EMPLOYEE)
002700         PERFORM 200-EDIT-TRAN
002800         THRU    200-END
           ELSE
P09505         MOVE HR11F1-EMP-COMPANY     TO DB-COMPANY
P09505         MOVE HR11F1-EMP-EMPLOYEE    TO DB-EMPLOYEE
P09505         PERFORM 840-FIND-EMPSET1
P09505         IF (EMPLOYEE-NOTFOUND)
P09505************ Page up and down is not available for new employee
P09505             MOVE 145                TO CRT-ERROR-NBR
P09505             MOVE HR11F1-FC-FN       TO CRT-FIELD-NBR
P09505             IF (HR11WS-HRPADICT2-OPEN)
P09505                 SET HR11WS-HRPADICT2-NOTOPEN TO TRUE
P09505                 PERFORM 9900-CLOSE-HRPADICT2
P09505             END-IF
P09505             GO TO HR11S1-TRANSACTION-END
               END-IF.
002900
           IF (ERROR-FOUND)
           OR (HR11F1-GRANT-ERR NOT = ZEROES)
               IF  (HR11WS-HRPADICT2-OPEN)
                   SET HR11WS-HRPADICT2-NOTOPEN TO TRUE
000060             PERFORM 9900-CLOSE-HRPADICT2
               END-IF
               GO TO HR11S1-TRANSACTION-END.

           IF (HR11F1-LAST-USER-FLD        <= HR11WS-NBR-OF-DTL-LN)
011900         MOVE 1                      TO HR11F1-REC
011900                                        HR11F1-TOP-REC.

011800     IF  (HR11F1-FC                  = "+")
           AND (HR11F1-REC                 > HR11F1-LAST-USER-FLD)
003500         MOVE 11                     TO CRT-ERROR-NBR
003600         MOVE HR11F1-FC-FN           TO CRT-FIELD-NBR
002500         GO TO HR11S1-TRANSACTION-END
           ELSE
011800     IF  (HR11F1-FC                  = "-")
           AND (HR11F1-TOP-REC             = 1)
003500         MOVE 11                     TO CRT-ERROR-NBR
003600         MOVE HR11F1-FC-FN           TO CRT-FIELD-NBR
002500         GO TO HR11S1-TRANSACTION-END.

011800     IF (HR11F1-FC                   = "I" OR "N" OR "P")
           OR (HR11F1-EMP-EMPLOYEE         NOT = HR11F1-PT-EMP-EMPLOYEE)
011900         MOVE 1                      TO HR11F1-REC
012000     ELSE
012100     IF (HR11F1-FC                   = "+")
               IF  (HR11F1-LAST-USER-FLD   > HR11WS-NBR-OF-DTL-LN)
               AND (HR11F1-TOP-REC         = 1)
                   COMPUTE HR11F1-REC      = HR11WS-NBR-OF-DTL-LN
                                           + 1
               END-IF
012300     ELSE
               IF (HR11F1-TOP-REC > 1)
                   COMPUTE HR11F1-REC      = HR11F1-TOP-REC
                                           - 1
               ELSE
                   MOVE 1                  TO HR11F1-REC
               END-IF.

001200     IF  (NO-ERROR-FOUND)
               INITIALIZE HR11F1-XMIT-REQDED
               IF  (HR11F1-FC            = "A" OR "R")
               AND (HR11F1-PT-FROM-PR134 = ZEROES)
               AND (REQCODE-FOUND)
                   PERFORM 710-SCREEN-XFER
                   THRU    710-END
               ELSE
                   INITIALIZE HR11F1-PT-FROM-PR134
                   PERFORM 400-PROCESS-TRAN
                   THRU    400-END
                   INITIALIZE HR11F1-XMIT-HREMP-BLOCK
               END-IF
           END-IF.
003300
           IF (HR11WS-HRPADICT2-OPEN)
               SET HR11WS-HRPADICT2-NOTOPEN        TO TRUE

000060         PERFORM 9900-CLOSE-HRPADICT2.

003400     GO TO HR11S1-TRANSACTION-END.
003500
003600******************************************************************
003700 200-EDIT-TRAN.
003800******************************************************************
003900
065500     IF (HR11F1-FC = "A" OR "I" OR "N" OR "P")
           OR (HR11F1-EMP-EMPLOYEE         NOT = HR11F1-PT-EMP-EMPLOYEE)
               CONTINUE
           ELSE
               IF  (HR11F1-WORK-FILE-NAME NOT = SPACES)
                   MOVE HR11F1-WORK-FILE-NAME  TO SYSCMD-FILENAME
                   PERFORM 900-FILEEXISTS
               END-IF

               IF (HR11F1-WORK-FILE-NAME       = SPACES)
               OR (SYSCMD-ERROR            NOT = ZEROES)
                   MOVE HR11F1-EMP-COMPANY TO HRHP2-COMPANY
                   MOVE "UF"               TO HRHP2-TOPIC
                   MOVE "Y"                TO HRHP2-SORT-BY-REQ
                   MOVE HR11F1-EMP-WORK-COUNTRY
                                           TO HRHP2-WORK-COUNTRY
                   MOVE HR11F1-WORK-FILE-NAME
                                           TO WS-WORK-FILE-NAME
                   PERFORM 5000-CREATE-HRPADICT2
                   MOVE HRHP2-NBR-OF-RECS  TO HR11F1-HRHP2-RECS
                   MOVE WS-WORK-FILE-NAME  TO HR11F1-WORK-FILE-NAME
                   MOVE "N"                TO HR11F1-HRU-TABLE-CREATED
                   MOVE HRHP2-WORK-COUNTRY TO HR11F1-WORK-FILE-COUNTRY
               END-IF
               SET HR11WS-HRPADICT2-OPEN   TO TRUE
000060         PERFORM 9820-OPEN-INPUT-HRPADICT2.
P63307
P63307     IF (HR11F1-FC  = "A" OR "I")
P63307         MOVE HR11F1-FC              TO HR11WS-PREV-FC
P63307     END-IF.
P63307
004500     IF  ((HR11F1-FC         = "D")
004600     AND (PA-USER))
004700     OR  (HR11F1-FC          = "C" OR "A" OR "R")
004800         IF (HR11F1-EFFECT-DATE = ZEROES)
                   MOVE WS-SYSTEM-DATE-YMD    TO HR11F1-EFFECT-DATE
                   IF  (HR11F1-FC = "C" OR "A")
                       MOVE 108                        TO CRT-ERROR-NBR
                       MOVE HR11F1-EFFECT-DATE-FN      TO CRT-FIELD-NBR
                       GO TO 200-END
                   END-IF
               END-IF
           END-IF.
005700
P63307     IF  (HR11WS-PREV-FC = "A")
P63307     AND (HR11F1-FC      = "C")
P63307          MOVE 167                          TO CRT-ERROR-NBR
P63307          MOVE HR11F1-FC-FN                 TO CRT-FIELD-NBR
P63307          GO TO 200-END
P63307     END-IF.
P63307
005800     PERFORM 201-MOVE-SCR-TO-WS
005900     THRU    201-END.
006000
006100     IF (HR11F1-FC = "A" OR "C")
006200         INITIALIZE                            HREMP-UPDPEP-DATE
006300         MOVE HR11F1-EMP-COMPANY            TO PADT-COMPANY
006400         MOVE HR11F1-EMP-EMPLOYEE           TO PADT-EMPLOYEE
006500         MOVE HR11F1-EMP-TERM-DATE          TO PADT-END-DATE
006600         MOVE 1                             TO PADT-POS-LEVEL
006700         MOVE HR11F1-EFFECT-DATE            TO PADT-EFFECT-DATE
006800         PERFORM 2300-PADT-DATE-CHECK
006900         IF (ERROR-FOUND)
007000             GO TO 200-END
007100         ELSE
007200             MOVE PADT-UPDPEP-DATE          TO HREMP-UPDPEP-DATE.
007300
           MOVE ZEROES                            TO HREMPWS-GRANT-ERR
                                                     HR11F1-GRANT-ERR.
J75684
J75684     IF (HR11F1-EMP-FICA-NBR NOT = HR11WS-HOLD-FICA-NBR)
J75684         MOVE ZEROES                     TO HREMP-XMIT-SOCNBR-DUP 
J75684         MOVE HR11F1-EMP-FICA-NBR        TO HR11WS-HOLD-FICA-NBR
J75684     END-IF.
J75684
007400     PERFORM 2000-HREMP-EDIT-TRAN.
P63307
P63307     IF  (HR11F1-FC  NOT = "C" AND "D")
P63307     AND (NO-ERROR-FOUND)
P63307          MOVE HR11F1-FC              TO HR11WS-PREV-FC
P63307     END-IF.

007500     IF (HREMP-UPDPEP-DATE = HREMP-EFFECT-DATE)
007600         INITIALIZE                             HREMP-UPDPEP-DATE.
007700
007800     PERFORM 600-MOVE-WS-TO-SCR
007900     THRU    600-END.
008000
           IF (ERROR-FOUND)
           OR (HREMPWS-GRANT-ERR NOT = ZEROES)
               GO TO 200-END.

           IF  (HR11F1-FC           = "A" OR "C")
           AND (HRSEC-WORK-COUNTRY  NOT = HR11F1-WORK-FILE-COUNTRY)
               IF (HR11WS-HRPADICT2-OPEN)
                   SET HR11WS-HRPADICT2-NOTOPEN TO TRUE
                   PERFORM 9900-CLOSE-HRPADICT2
               END-IF
               SET HR11WS-HRPADICT2-OPEN   TO TRUE
               MOVE HREMP-COMPANY          TO HRHP2-COMPANY
               MOVE "UF"                   TO HRHP2-TOPIC
               MOVE "Y"                    TO HRHP2-SORT-BY-REQ
               IF (HR11F1-EMP-WORK-COUNTRY = SPACES)
                   MOVE HRSEC-WORK-COUNTRY TO HRHP2-WORK-COUNTRY
               ELSE
                   MOVE HR11F1-EMP-WORK-COUNTRY
                                           TO HRHP2-WORK-COUNTRY
               END-IF
               MOVE HR11F1-WORK-FILE-NAME  TO WS-WORK-FILE-NAME
               PERFORM 5000-CREATE-HRPADICT2
               MOVE HRHP2-NBR-OF-RECS      TO HR11F1-HRHP2-RECS
               MOVE WS-WORK-FILE-NAME      TO HR11F1-WORK-FILE-NAME
               MOVE "N"                    TO HR11F1-HRU-TABLE-CREATED
               MOVE HRHP2-WORK-COUNTRY     TO HR11F1-WORK-FILE-COUNTRY
               SET HR11WS-HRPADICT2-OPEN   TO TRUE
000060         PERFORM 9820-OPEN-INPUT-HRPADICT2
               IF (HR11F1-HRU-TABLE-CREATED NOT = "Y")
                   MOVE "Y"                TO HR11F1-HRU-TABLE-CREATED
                   MOVE 1                  TO HR11F1-I1
                   MOVE "UF"               TO HP2-TOPIC
011900             INITIALIZE                 HP2-REQ-FLAG
011900                                        HP2-ITEM-NAME
                                              HP2-FLD-NBR
012600             PERFORM 8500-FIND-NLT-HRPADICT2
                   PERFORM
                       UNTIL (HRPADICT2-NOTFOUND)
                       OR    (HR11F1-I1    > HR11WS-MAX-USER-FLD)

                       MOVE HP2-FLD-NBR    TO
                                           HR11F1-WS-FLD-NBR (HR11F1-I1)
                       IF (HP2-REQ-FLAG = SPACES)
                           MOVE "X"        TO
                                          HR11F1-WS-REQ-FLAG (HR11F1-I1)
                       ELSE
                           INITIALIZE HR11F1-WS-REQ-FLAG (HR11F1-I1)
                       END-IF

                       INITIALIZE HR11F1-WS-LINE-FC (HR11F1-I1)
                                  HR11F1-WS-VALUE (HR11F1-I1)
                                  HR11F1-WS-CURR (HR11F1-I1)
                                  HR11F1-WS-BASE-AMT (HR11F1-I1)

                       PERFORM 8600-FIND-NEXT-HRPADICT2

                       ADD 1               TO HR11F1-I1

                   END-PERFORM
                   COMPUTE HR11F1-LAST-USER-FLD = HR11F1-I1
                                            - 1
011900             MOVE 1                  TO HR11F1-REC
011900                                        HR11F1-TOP-REC.

002500     IF (HR11F1-FC                   = "R")
               IF (HREMP-PROCESS-LEVEL NOT = SPACES)
                   MOVE HREMP-COMPANY          TO DB-COMPANY
                   MOVE HREMP-PROCESS-LEVEL    TO DB-PROCESS-LEVEL
                   PERFORM 840-FIND-PRSSET1
                   IF (PRS-TAX-FILTER NOT = ZEROES)
                       MOVE PRS-TAX-FILTER TO HREMP-PRS-TAX-FILTER
                   END-IF
               END-IF
               MOVE HREMP-EMP-STATUS       TO DB-EMP-STATUS
               PERFORM 840-FIND-EMSSET1
               IF  ((HR11F1-EMP-TERM-DATE  NOT = ZEROES)
               OR   ((EMSTATUS-FOUND)
               AND   (EMS-PAY-STATUS (1 : 1) = "N")))
               AND (HR11F1-XMIT-REQDED    = ZEROES)
      ************ Warning, Req Deds will be updated for term emp, Press OK
038200             MOVE 1                  TO HR11F1-XMIT-REQDED
038300             MOVE 132                TO CRT-ERROR-NBR
038400             GO TO 200-END
               END-IF
068300         MOVE HR11F1-EMP-COMPANY     TO PRRQC-COMPANY
068400         MOVE HR11F1-EMP-EMPLOYEE    TO PRRQC-EMPLOYEE
068600         MOVE HR11F1-DFT-MAR-STAT    TO PRRQC-DFT-MAR-STAT
068700                                        PRRQC-ST-DFT-MAR-STAT
068800         MOVE HR11F1-DFT-EXEMPTS     TO PRRQC-DFT-EXEMPTS
068900                                        PRRQC-ST-DFT-EXEMPTS
               MOVE HR11F1-EDM-EFFECT-DATE TO PRRQC-EFFECT-DATE
               MOVE HR11F1-EDM-END-DATE    TO PRRQC-END-DATE
               MOVE "N"                    TO PRRQC-UPDATE-OPTION
               PERFORM 910-AUDIT-BEGIN
069000         PERFORM 500-REQ-DED-CREATION
               PERFORM 920-AUDIT-END
               IF (ERROR-FOUND)
                   INITIALIZE HR11F1-PT-FROM-PR134
                   GO TO 200-END.

002500     IF (HR11F1-FC                   = "A" OR "R")
               MOVE HR11F1-EMP-COMPANY     TO DB-COMPANY
               MOVE RQCSET1-COMPANY        TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-RQCSET1.

           IF (HR11F1-FC                   = "A" OR "C")
               PERFORM 490-MOVE-CUR-PG-TO-WS
               THRU    490-END

               IF (HR11F1-FC               = "C")
                   MOVE HR11F1-TOP-REC     TO HR11F1-SV-TOP-REC
                   MOVE 1                  TO HR11F1-REC
               ELSE
                   MOVE 1                  TO HR11F1-REC
                                              HR11F1-TOP-REC
                                              HR11F1-SV-TOP-REC
               END-IF
               PERFORM
                   UNTIL (HR11F1-REC       > HR11F1-LAST-USER-FLD)
                   OR    (ERROR-FOUND)

                   PERFORM
                       VARYING HR11F1-I1 FROM 1 BY 1
                       UNTIL  (HR11F1-I1   > HR11WS-NBR-OF-DTL-LN)

                       INITIALIZE HR11F1-HRU-FLD-NBR (HR11F1-I1)
                                  HR11F1-HRU-FIELD-TYPE (HR11F1-I1)
                                  HR11F1-HRU-FIELD-NAME (HR11F1-I1)
                                  HR11F1-VALUE (HR11F1-I1)
                                  HR11F1-HRU-DESCRIPTION (HR11F1-I1)
                                  HR11F1-HRU-ACTIVE-FLAG (HR11F1-I1)
                                  HR11F1-HEU-CURRENCY-CODE (HR11F1-I1)
                                  HR11F1-HRU-REQ-FLAG (HR11F1-I1)
                                  HR11F1-HEU-BASE-AMOUNT (HR11F1-I1)
                                  HR11F1-HEU-BASE-CURRENCY (HR11F1-I1)
                                  HR11F1-BASE-FORMS-EXP (HR11F1-I1)
                                  HR11F1-HEU-BASE-ND (HR11F1-I1)
                                  HR11F1-HEU-CURR-ND (HR11F1-I1)
                                  HR11F1-HRU-FIELD-KEY (HR11F1-I1)

                   END-PERFORM

                   PERFORM
                       VARYING HR11F1-I1 FROM 1 BY 1
                       UNTIL  (HR11F1-I1   > HR11WS-NBR-OF-DTL-LN)
                       OR     (HR11F1-WS-FLD-NBR (HR11F1-REC)
                                           = ZEROES)

                       PERFORM 610-MOVE-DTL-TO-SCREEN
                       THRU    610-END

                       IF  ((HR11F1-FC                = "A")
                       AND  (HR11F1-HRU-REQ-FLAG (HR11F1-I1) = "X"))
                       OR  ((HR11F1-FC                = "C")
                       AND  (HR11F1-HRU-REQ-FLAG (HR11F1-I1) = "X")
                       AND  (HR11F1-VALUE (HR11F1-I1)        = SPACES))
                           MOVE "A"       TO HR11F1-LINE-FC (HR11F1-I1)
                       END-IF
                   END-PERFORM

                   MOVE HR11F1-EMP-COMPANY       TO DB-COMPANY
                   MOVE HR11F1-EMP-WORK-COUNTRY  TO DB-COUNTRY-CD-REQ
                   MOVE PASSET4-COUNTRY-CD-REQ   TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-PASSET4
                   IF (PASCRTY-FOUND)
                       SET HR11WS-COUNTRY-REQ    TO TRUE
                   ELSE
                       SET HR11WS-COMPANY-REQ    TO TRUE
                   END-IF
                   PERFORM 260-EDIT-DTL-DATA
                   THRU    260-END
                       VARYING HR11F1-I1 FROM 1 BY 1
                       UNTIL  (HR11F1-I1 > HR11WS-NBR-OF-DTL-LN)
                       OR     (HR11F1-HRU-FLD-NBR (HR11F1-I1) = ZEROES)
                       OR     (ERROR-FOUND)
               END-PERFORM.
               
J64983*    IF  (HR11F1-FC = "C")
J64983*    AND (PAEMPLOYEE-FOUND)
J64983*    AND (PEM-EEO-CLASS NOT = HR11F1-PEM-EEO-CLASS)
J64983*        MOVE "ET"                   TO DB-TYPE
J64983*        MOVE HREMP-WORK-COUNTRY     TO DB-COUNTRY-CODE
J64983*        MOVE "M"                    TO DB-US-ET-BEHAVIOR
J64983*        MOVE "A"                    TO DB-ACTIVE-FLAG
J64983*        MOVE CTCSET3-US-ET-BEHAVIOR TO WS-DB-BEG-RNG
J64983*        PERFORM 850-FIND-BEGRNG-CTCSET3
J64983*         
J64983*        IF  (HRCTRYCODE-FOUND)
J64983*        AND (HREMP-WORK-COUNTRY = "US")
J64983*        AND (PEM-EEO-CLASS = CTC-HRCTRY-CODE)
J64983*            MOVE 182                TO CRT-ERROR-NBR
J64983*            MOVE HR11F1-PEM-EEO-CLASS-FN
J64983*                                    TO CRT-FIELD-NBR
J64983*            GO TO 200-END        
J64983*        END-IF
               
J64983*    END-IF.               
012900
008100 200-END.
008200
008300******************************************************************
008400 201-MOVE-SCR-TO-WS.
008500******************************************************************
008600     INITIALIZE HREMP-SCR-FIELDS.
008700     INITIALIZE HRPEM-SCR-FIELDS.
008800
008900     MOVE HR11F1-FC                    TO HREMP-FC.
009000     MOVE HR11F1-FC-FN                 TO HREMP-FC-FN.
009100     MOVE HR11F1-EMP-COMPANY           TO HREMP-COMPANY.
009200     MOVE HR11F1-EMP-COMPANY-FN        TO HREMP-COMPANY-FN.
009300     MOVE HR11F1-EMP-EMPLOYEE          TO HREMP-EMPLOYEE.
009400     MOVE HR11F1-EMP-EMPLOYEE-FN       TO HREMP-EMPLOYEE-FN.
           MOVE HR11F1-EMP-LAST-NAME-PRE     TO HREMP-LAST-NAME-PRE.
           MOVE HR11F1-EMP-LAST-NAME-PRE-FN  TO HREMP-LAST-NAME-PRE-FN.
009500     MOVE HR11F1-EMP-LAST-NAME         TO HREMP-LAST-NAME.
009600     MOVE HR11F1-EMP-LAST-NAME-FN      TO HREMP-LAST-NAME-FN.
009700     MOVE HR11F1-EMP-FIRST-NAME        TO HREMP-FIRST-NAME.
009800     MOVE HR11F1-EMP-FIRST-NAME-FN     TO HREMP-FIRST-NAME-FN.
009900     MOVE HR11F1-EMP-MIDDLE-NAME       TO HREMP-MIDDLE-NAME.
010000     MOVE HR11F1-EMP-MIDDLE-NAME-FN    TO HREMP-MIDDLE-NAME-FN.
009900     MOVE HR11F1-EMP-MIDDLE-NAME       TO HREMP-MIDDLE-INIT.
010000     MOVE HR11F1-EMP-MIDDLE-NAME-FN    TO HREMP-MIDDLE-INIT-FN.
010100     MOVE HR11F1-EMP-NICK-NAME         TO HREMP-NICK-NAME.
010200     MOVE HR11F1-EMP-NICK-NAME-FN      TO HREMP-NICK-NAME-FN.
           MOVE HR11F1-EMP-NAME-PREFIX       TO HREMP-NAME-PREFIX.
           MOVE HR11F1-EMP-NAME-PREFIX-FN    TO HREMP-NAME-PREFIX-FN.
           MOVE HR11F1-EMP-NAME-SUFFIX       TO HREMP-NAME-SUFFIX.
           MOVE HR11F1-EMP-NAME-SUFFIX-FN    TO HREMP-NAME-SUFFIX-FN.
           MOVE HR11F1-PEM-LANGUAGE-CODE     TO HRPEM-LANGUAGE-CODE.
           MOVE HR11F1-PEM-LANGUAGE-CODE-FN  TO HRPEM-LANGUAGE-CODE-FN.
010300     MOVE HR11F1-EMP-ADDR1             TO HREMP-ADDR1.
010400     MOVE HR11F1-EMP-ADDR1-FN          TO HREMP-ADDR1-FN.
010500     MOVE HR11F1-EMP-ADDR2             TO HREMP-ADDR2.
010600     MOVE HR11F1-EMP-ADDR2-FN          TO HREMP-ADDR2-FN.
010700     MOVE HR11F1-EMP-ADDR3             TO HREMP-ADDR3.
010800     MOVE HR11F1-EMP-ADDR3-FN          TO HREMP-ADDR3-FN.
010900     MOVE HR11F1-EMP-ADDR4             TO HREMP-ADDR4.
011000     MOVE HR11F1-EMP-ADDR4-FN          TO HREMP-ADDR4-FN.
011100     MOVE HR11F1-EMP-CITY              TO HREMP-CITY.
011200     MOVE HR11F1-EMP-CITY-FN           TO HREMP-CITY-FN.
011300     MOVE HR11F1-EMP-STATE             TO HREMP-STATE.
011400     MOVE HR11F1-EMP-STATE-FN          TO HREMP-STATE-FN.
011500     MOVE HR11F1-EMP-ZIP               TO HREMP-ZIP.
011600     MOVE HR11F1-EMP-ZIP-FN            TO HREMP-ZIP-FN.
           MOVE HR11F1-EMP-COUNTY            TO HREMP-COUNTY.
           MOVE HR11F1-EMP-COUNTY-FN         TO HREMP-COUNTY-FN.
011700     MOVE HR11F1-EMP-COUNTRY-CODE      TO HREMP-COUNTRY-CODE.
011800     MOVE HR11F1-EMP-COUNTRY-CODE-FN   TO HREMP-COUNTRY-CODE-FN.
           MOVE HR11F1-EMP-WORK-COUNTRY      TO HREMP-WORK-COUNTRY.
           MOVE HR11F1-EMP-WORK-COUNTRY-FN   TO HREMP-WORK-COUNTRY-FN.
011900     MOVE HR11F1-EMP-EMP-STATUS        TO HREMP-EMP-STATUS.
012000     MOVE HR11F1-EMP-EMP-STATUS-FN     TO HREMP-EMP-STATUS-FN.
012100     MOVE HR11F1-EMP-FICA-NBR          TO HREMP-FICA-NBR.
012200     MOVE HR11F1-EMP-FICA-NBR-FN       TO HREMP-FICA-NBR-FN.
012300     MOVE HR11F1-EMP-EIC-STATUS        TO HREMP-EIC-STATUS.
012400     MOVE HR11F1-EMP-EIC-STATUS-FN     TO HREMP-EIC-STATUS-FN.
           MOVE HR11F1-RAILROAD-CODE         TO HREMP-RAILROAD-CODE.
           MOVE HR11F1-RAILROAD-CODE-FN      TO HREMP-RAILROAD-CODE-FN.
           MOVE HR11F1-EMP-TAX-FILTER        TO HREMP-TAX-FILTER.
           MOVE HR11F1-EMP-TAX-FILTER-FN     TO HREMP-TAX-FILTER-FN.
           MOVE HR11F1-EMP-REMOTE            TO HREMP-REMOTE.
           MOVE HR11F1-EMP-REMOTE-FN         TO HREMP-REMOTE-FN.
           MOVE HR11F1-EMP-PUB-SEC-RETIRE    TO HREMP-PUB-SEC-RETIRE.
           MOVE HR11F1-EMP-PUB-SEC-RETIRE-FN TO HREMP-PUB-SEC-RETIRE-FN.
           MOVE HR11F1-EMP-TAX-CITY          TO HREMP-TAX-CITY.
           MOVE HR11F1-EMP-TAX-CITY-FN       TO HREMP-TAX-CITY-FN.
           MOVE HR11F1-EMP-TAX-COUNTY        TO HREMP-TAX-COUNTY.
           MOVE HR11F1-EMP-TAX-COUNTY-FN     TO HREMP-TAX-COUNTY-FN.
           MOVE HR11F1-EMP-TAX-SCHOOL        TO HREMP-TAX-SCHOOL.
           MOVE HR11F1-EMP-TAX-SCHOOL-FN     TO HREMP-TAX-SCHOOL-FN.
           MOVE HR11F1-EMP-WORK-CITY         TO HREMP-WORK-CITY.
           MOVE HR11F1-EMP-WORK-CITY-FN      TO HREMP-WORK-CITY-FN.
           MOVE HR11F1-EMP-WORK-COUNTY       TO HREMP-WORK-COUNTY.
           MOVE HR11F1-EMP-WORK-COUNTY-FN    TO HREMP-WORK-COUNTY-FN.
           MOVE HR11F1-EMP-WORK-SCHOOL       TO HREMP-WORK-SCHOOL.
           MOVE HR11F1-EMP-WORK-SCHOOL-FN    TO HREMP-WORK-SCHOOL-FN.
012500     MOVE HR11F1-EMP-PROCESS-LEVEL     TO HREMP-PROCESS-LEVEL.
012600     MOVE HR11F1-EMP-PROCESS-LEVEL-FN  TO HREMP-PROCESS-LEVEL-FN.
012700     MOVE HR11F1-EMP-DEPARTMENT        TO HREMP-DEPARTMENT.
012800     MOVE HR11F1-EMP-DEPARTMENT-FN     TO HREMP-DEPARTMENT-FN.
012900     MOVE HR11F1-EMP-USER-LEVEL        TO HREMP-USER-LEVEL.
013000     MOVE HR11F1-EMP-USER-LEVEL-FN     TO HREMP-USER-LEVEL-FN.
013100     MOVE HR11F1-EMP-HM-DIST-CO        TO HREMP-HM-DIST-CO.
013200     MOVE HR11F1-EMP-HM-DIST-CO-FN     TO HREMP-HM-DIST-CO-FN.
013300     MOVE HR11F1-EMP-HM-ACCT-UNIT      TO HREMP-HM-ACCT-UNIT.
013400     MOVE HR11F1-EMP-HM-ACCT-UNIT-FN   TO HREMP-HM-ACCT-UNIT-FN.
013500     MOVE HR11F1-EMP-HM-ACCOUNT        TO HREMP-HM-ACCOUNT.
013600     MOVE HR11F1-EMP-HM-ACCOUNT-FN     TO HREMP-HM-ACCOUNT-FN.
013700     MOVE HR11F1-EMP-HM-SUB-ACCT       TO HREMP-HM-SUB-ACCT.
013800     MOVE HR11F1-EMP-HM-SUB-ACCT-FN    TO HREMP-HM-SUB-ACCT-FN.
013900     MOVE HR11F1-EMP-ACTIVITY          TO HREMP-ACTIVITY.
014000     MOVE HR11F1-EMP-ACTIVITY-FN       TO HREMP-ACTIVITY-FN.
014100     MOVE HR11F1-EMP-ACCT-CATEGORY     TO HREMP-ACCT-CATEGORY.
014200     MOVE HR11F1-EMP-ACCT-CATEGORY-FN  TO HREMP-ACCT-CATEGORY-FN.
014300     MOVE HR11F1-EMP-JOB-CODE          TO HREMP-JOB-CODE.
014400     MOVE HR11F1-EMP-JOB-CODE-FN       TO HREMP-JOB-CODE-FN.
014500     MOVE HR11F1-EMP-UNION-CODE        TO HREMP-UNION-CODE.
014600     MOVE HR11F1-EMP-UNION-CODE-FN     TO HREMP-UNION-CODE-FN.
014700     MOVE HR11F1-EMP-SUPERVISOR        TO HREMP-SUPERVISOR.
014800     MOVE HR11F1-EMP-SUPERVISOR-FN     TO HREMP-SUPERVISOR-FN.
014900     MOVE HR11F1-EMP-SUPER-IND         TO HREMP-SUPERVISOR-IND.
015000     MOVE HR11F1-EMP-SUPER-IND-FN      TO HREMP-SUPERVISOR-IND-FN.
015100     MOVE HR11F1-EMP-DATE-HIRED        TO HREMP-DATE-HIRED.
015200     MOVE HR11F1-EMP-DATE-HIRED-FN     TO HREMP-DATE-HIRED-FN.
015300     MOVE HR11F1-EMP-PAY-FREQUENCY     TO HREMP-PAY-FREQUENCY.
015400     MOVE HR11F1-EMP-PAY-FREQUENCY-FN  TO HREMP-PAY-FREQUENCY-FN.
015500     MOVE HR11F1-EMP-SHIFT             TO HREMP-SHIFT.
015600     MOVE HR11F1-EMP-SHIFT-FN          TO HREMP-SHIFT-FN.
015700     MOVE HR11F1-EMP-SALARY-CLASS      TO HREMP-SALARY-CLASS.
015800     MOVE HR11F1-EMP-SALARY-CLASS-FN   TO HREMP-SALARY-CLASS-FN.
015900     MOVE HR11F1-EMP-EXEMPT-EMP        TO HREMP-EXEMPT-EMP.
016000     MOVE HR11F1-EMP-EXEMPT-EMP-FN     TO HREMP-EXEMPT-EMP-FN.
016100     MOVE HR11F1-EMP-PAY-RATE          TO HREMP-PAY-RATE.
016200     MOVE HR11F1-EMP-PAY-RATE-FN       TO HREMP-PAY-RATE-FN.
           MOVE HR11F1-EMP-CURR-CODE         TO HREMP-CURRENCY-CODE.
           MOVE HR11F1-EMP-CURR-CODE-FN      TO HREMP-CURRENCY-CODE-FN.
016300     MOVE HR11F1-EMP-STAND-HOURS       TO HREMP-STAND-HOURS.
016400     MOVE HR11F1-EMP-STAND-HOURS-FN    TO HREMP-STAND-HOURS-FN.
016500     MOVE HR11F1-EMP-STAND-AMT         TO HREMP-STAND-AMT.
016600     MOVE HR11F1-EMP-STAND-AMT-FN      TO HREMP-STAND-AMT-FN.
           MOVE HR11F1-EMP-STAND-AMT-ND      TO HREMP-STAND-AMT-ND.
016700     MOVE HR11F1-EMP-WARN-FLAG         TO HREMP-WARN-FLAG.
016800     MOVE HR11F1-EMP-WARN-FLAG-FN      TO HREMP-WARN-FLAG-FN.
016900     MOVE HR11F1-EMP-ADD-ALLOW-PER     TO HREMP-ADD-ALLOW-PER.
017000     MOVE HR11F1-EMP-ADD-ALLOW-PER-FN  TO HREMP-ADD-ALLOW-PER-FN.
017100     MOVE HR11F1-EMP-ADD-ALLOW-HRS     TO HREMP-ADD-ALLOW-HRS.
017200     MOVE HR11F1-EMP-ADD-ALLOW-HRS-FN  TO HREMP-ADD-ALLOW-HRS-FN.
017300     MOVE HR11F1-EMP-ADD-ALLOW-AMT     TO HREMP-ADD-ALLOW-AMT.
017400     MOVE HR11F1-EMP-ADD-ALLOW-AMT-FN  TO HREMP-ADD-ALLOW-AMT-FN.
017500     MOVE HR11F1-EMP-RPT-INS-COST      TO HREMP-RPT-INS-COST.
017600     MOVE HR11F1-EMP-RPT-INS-COST-FN   TO HREMP-RPT-INS-COST-FN.
017700     MOVE HR11F1-EMP-AUTO-TIME-REC     TO HREMP-AUTO-TIME-REC.
017800     MOVE HR11F1-EMP-AUTO-TIME-REC-FN  TO HREMP-AUTO-TIME-REC-FN.
017900     MOVE HR11F1-EMP-AUTO-DEPOSIT      TO HREMP-AUTO-DEPOSIT.
018000     MOVE HR11F1-EMP-AUTO-DEPOSIT-FN   TO HREMP-AUTO-DEPOSIT-FN.
018300     MOVE HR11F1-EMP-ADJ-HIRE-DATE     TO HREMP-ADJ-HIRE-DATE.
018400     MOVE HR11F1-EMP-ADJ-HIRE-DATE-FN  TO HREMP-ADJ-HIRE-DATE-FN.
018500     MOVE HR11F1-EMP-ANNIVERS-DATE     TO HREMP-ANNIVERS-DATE.
018600     MOVE HR11F1-EMP-ANNIVERS-DATE-FN  TO HREMP-ANNIVERS-DATE-FN.
           MOVE HR11F1-EMP-FST-DAY-WORKED    TO HREMP-FST-DAY-WORKED.
           MOVE HR11F1-EMP-FST-DAY-WORKED-FN TO HREMP-FST-DAY-WORKED-FN.
           MOVE HR11F1-EMP-LAST-DAY-PAID     TO HREMP-LAST-DAY-PAID.
           MOVE HR11F1-EMP-LAST-DAY-PAID-FN  TO HREMP-LAST-DAY-PAID-FN.
018700     MOVE HR11F1-EMP-TERM-DATE         TO HREMP-TERM-DATE.
018800     MOVE HR11F1-EMP-TERM-DATE-FN      TO HREMP-TERM-DATE-FN.
018900     MOVE HR11F1-EMP-SEC-LVL           TO HREMP-SEC-LVL.
019000     MOVE HR11F1-EMP-SEC-LVL-FN        TO HREMP-SEC-LVL-FN.
019100     MOVE HR11F1-EMP-SEC-LOCATION      TO HREMP-SEC-LOCATION.
019200     MOVE HR11F1-EMP-SEC-LOCATION-FN   TO HREMP-SEC-LOCATION-FN.
019300     MOVE HR11F1-EMP-NBR-FTE           TO HREMP-NBR-FTE.
019400     MOVE HR11F1-EMP-NBR-FTE-FN        TO HREMP-NBR-FTE-FN.
           MOVE HR11F1-EMP-FTE-TOTAL         TO HREMP-FTE-TOTAL.
           MOVE HR11F1-EMP-FTE-TOTAL-FN      TO HREMP-FTE-TOTAL-FN.
019500     MOVE HR11F1-EMP-ANNUAL-HOURS      TO HREMP-ANNUAL-HOURS.
019600     MOVE HR11F1-EMP-ANNUAL-HOURS-FN   TO HREMP-ANNUAL-HOURS-FN.
019700     MOVE HR11F1-EMP-PENSION-PLAN      TO HREMP-PENSION-PLAN.
019800     MOVE HR11F1-EMP-PENSION-PLAN-FN   TO HREMP-PENSION-PLAN-FN.
019900     MOVE HR11F1-EMP-PAY-STEP          TO HREMP-PAY-STEP.
020000     MOVE HR11F1-EMP-PAY-STEP-FN       TO HREMP-PAY-STEP-FN.
020100     MOVE HR11F1-EMP-PAY-GRADE         TO HREMP-PAY-GRADE.
020200     MOVE HR11F1-EMP-PAY-GRADE-FN      TO HREMP-PAY-GRADE-FN.
020300     MOVE HR11F1-EMP-SCHEDULE          TO HREMP-SCHEDULE.
020400     MOVE HR11F1-EMP-SCHEDULE-FN       TO HREMP-SCHEDULE-FN.
020500     MOVE HR11F1-EMP-WORK-SCHED        TO HREMP-WORK-SCHED.
020600     MOVE HR11F1-EMP-WORK-SCHED-FN     TO HREMP-WORK-SCHED-FN.
020700     MOVE HR11F1-EMP-EBE-AMOUNT        TO HREMP-EBE-AMOUNT.
020800     MOVE HR11F1-EMP-EBE-AMOUNT-FN     TO HREMP-EBE-AMOUNT-FN.
020900     MOVE HR11F1-EMP-SICK-PAY          TO HREMP-SICK-PAY.
021000     MOVE HR11F1-EMP-SICK-PAY-FN       TO HREMP-SICK-PAY-FN.
021100     MOVE HR11F1-EMP-MOVING-EXP        TO HREMP-MOVING-EXP.
021200     MOVE HR11F1-EMP-MOVING-EXP-FN     TO HREMP-MOVING-EXP-FN.
021700     MOVE HR11F1-EMP-WC-STATE          TO HREMP-WC-STATE.
021800     MOVE HR11F1-EMP-WC-STATE-FN       TO HREMP-WC-STATE-FN.
021900     MOVE HR11F1-EMP-OT-PLAN-CODE      TO HREMP-OT-PLAN-CODE.
022000     MOVE HR11F1-EMP-OT-PLAN-CODE-FN   TO HREMP-OT-PLAN-CODE-FN.
022100     MOVE HR11F1-EMP-TIPPED            TO HREMP-TIPPED.
022200     MOVE HR11F1-EMP-TIPPED-FN         TO HREMP-TIPPED-FN.
022300     MOVE HR11F1-EMP-DECEASED          TO HREMP-DECEASED.
022400     MOVE HR11F1-EMP-DECEASED-FN       TO HREMP-DECEASED-FN.
           MOVE HR11F1-EMP-DEATH-DATE        TO HREMP-DEATH-DATE.
           MOVE HR11F1-EMP-DEATH-DATE-FN     TO HREMP-DEATH-DATE-FN.
           MOVE HR11F1-PEM-ESTAB-PATIENT     TO HRPEM-ESTAB-PATIENT.
           MOVE HR11F1-PEM-ESTAB-PATIENT-FN  TO HRPEM-ESTAB-PATIENT-FN.
           MOVE HR11F1-PEM-PRIOR-COV-MO      TO HRPEM-PRIOR-COV-MO.
           MOVE HR11F1-PEM-PRIOR-COV-MO-FN   TO HRPEM-PRIOR-COV-MO-FN.
022500     MOVE HR11F1-EMP-BSI-GROUP         TO HREMP-BSI-GROUP.
022600     MOVE HR11F1-EMP-BSI-GROUP-FN      TO HREMP-BSI-GROUP-FN.
           MOVE HR11F1-EMP-TAX-PROVINCE      TO HREMP-TAX-PROVINCE.
           MOVE HR11F1-EMP-TAX-PROVINCE-FN   TO HREMP-TAX-PROVINCE-FN.
           MOVE HR11F1-EMP-BUS-NBR-GRP       TO HREMP-BUS-NBR-GRP.
           MOVE HR11F1-EMP-BUS-NBR-GRP-FN    TO HREMP-BUS-NBR-GRP-FN.
           MOVE HR11F1-EMP-QC-ENT-NBR-GRP    TO HREMP-QC-ENT-NBR-GRP.
           MOVE HR11F1-EMP-QC-ENT-NBR-GRP-FN TO HREMP-QC-ENT-NBR-GRP-FN.
           MOVE HR11F1-EMP-WC-PROVINCE       TO HREMP-WC-PROVINCE.
           MOVE HR11F1-EMP-WC-PROVINCE-FN    TO HREMP-WC-PROVINCE-FN.
023100     MOVE HR11F1-EMP-EMAIL-ADDRESS     TO HREMP-EMAIL-ADDRESS.
023200     MOVE HR11F1-EMP-EMAIL-ADDRESS-FN  TO HREMP-EMAIL-ADDRESS-FN.
023300     MOVE HR11F1-EMP-MAX-LIMIT-OVRD    TO HREMP-MAX-LIMIT-OVRD.
023400     MOVE HR11F1-EMP-MAX-LIMIT-OVRD-FN TO HREMP-MAX-LIMIT-OVRD-FN.
023500     MOVE "N"                          TO HREMP-UPDATE-BENEFIT.
023600     MOVE HR11F1-EMP-POSITION          TO HREMP-POSITION.
023700     MOVE HR11F1-EMP-POSITION-FN       TO HREMP-POSITION-FN.
023800     MOVE HR11F1-PEM-LOCAT-CODE        TO HRPEM-LOCAT-CODE.
023900     MOVE HR11F1-PEM-LOCAT-CODE-FN     TO HRPEM-LOCAT-CODE-FN.
024000     MOVE HR11F1-PEM-BARGAIN-UNIT      TO HRPEM-BARGAIN-UNIT.
024100     MOVE HR11F1-PEM-BARGAIN-UNIT-FN   TO HRPEM-BARGAIN-UNIT-FN.
024200     MOVE HR11F1-PEM-SENIOR-DATE       TO HRPEM-SENIOR-DATE.
024300     MOVE HR11F1-PEM-SENIOR-DATE-FN    TO HRPEM-SENIOR-DATE-FN.
024400     MOVE HR11F1-PEM-TRUE-MAR-STAT     TO HRPEM-TRUE-MAR-STAT.
024500     MOVE HR11F1-PEM-TRUE-MAR-STAT-FN  TO HRPEM-TRUE-MAR-STAT-FN.
           MOVE HR11F1-EMP-NEW-HIRE-DATE     TO HREMP-NEW-HIRE-DATE.
           MOVE HR11F1-EMP-NEW-HIRE-DATE-FN  TO HREMP-NEW-HIRE-DATE-FN.
024600     MOVE HR11F1-PEM-WK-PHONE-CNTRY    TO HRPEM-WK-PHONE-CNTRY.
024700     MOVE HR11F1-PEM-WK-PHONE-CNTRY-FN TO HRPEM-WK-PHONE-CNTRY-FN.
024800     MOVE HR11F1-PEM-WK-PHONE-NBR      TO HRPEM-WK-PHONE-NBR.
024900     MOVE HR11F1-PEM-WK-PHONE-NBR-FN   TO HRPEM-WK-PHONE-NBR-FN.
025000     MOVE HR11F1-PEM-WK-PHONE-EXT      TO HRPEM-WK-PHONE-EXT.
025100     MOVE HR11F1-PEM-WK-PHONE-EXT-FN   TO HRPEM-WK-PHONE-EXT-FN.
025200     MOVE HR11F1-PEM-SECURITY-CODE     TO HRPEM-SECURITY-CODE.
025300     MOVE HR11F1-PEM-SECURITY-CODE-FN  TO HRPEM-SECURITY-CODE-FN.
025400     MOVE HR11F1-PEM-SECURITY-NBR      TO HRPEM-SECURITY-NBR.
025500     MOVE HR11F1-PEM-SECURITY-NBR-FN   TO HRPEM-SECURITY-NBR-FN.
025600     MOVE HR11F1-PEM-MAIL-GROUP        TO HRPEM-MAIL-GROUP.
025700     MOVE HR11F1-PEM-MAIL-GROUP-FN     TO HRPEM-MAIL-GROUP-FN.
025800     MOVE HR11F1-PEM-MB-NBR            TO HRPEM-MB-NBR.
025900     MOVE HR11F1-PEM-MB-NBR-FN         TO HRPEM-MB-NBR-FN.
026000     MOVE HR11F1-PEM-CLOCK-NBR         TO HRPEM-CLOCK-NBR.
026100     MOVE HR11F1-PEM-CLOCK-NBR-FN      TO HRPEM-CLOCK-NBR-FN.
026200     MOVE HR11F1-PEM-HIRE-SOURCE       TO HRPEM-HIRE-SOURCE.
026300     MOVE HR11F1-PEM-HIRE-SOURCE-FN    TO HRPEM-HIRE-SOURCE-FN.
026400     MOVE HR11F1-PEM-COMP-CODE         TO HRPEM-COMP-CODE.
026500     MOVE HR11F1-PEM-COMP-CODE-FN      TO HRPEM-COMP-CODE-FN.
026600     MOVE HR11F1-PEM-COMP-NBR          TO HRPEM-COMP-NBR.
026700     MOVE HR11F1-PEM-COMP-NBR-FN       TO HRPEM-COMP-NBR-FN.
026800     MOVE HR11F1-PEM-BIRTHDATE         TO HRPEM-BIRTHDATE.
026900     MOVE HR11F1-PEM-BIRTHDATE-FN      TO HRPEM-BIRTHDATE-FN.
027000     MOVE HR11F1-PEM-SEX               TO HRPEM-SEX.
027100     MOVE HR11F1-PEM-SEX-FN            TO HRPEM-SEX-FN.
J73217     MOVE HR11F1-PEM-SEX-ORIENTATION  TO HRPEM-SEXUAL-ORIENTATION.
J73217     MOVE HR11F1-PEM-SEX-ORIENTATION-FN
J73217                                   TO HRPEM-SEXUAL-ORIENTATION-FN.
J73217     MOVE HR11F1-PEM-GENDER-IDENTITY   TO HRPEM-GENDER-IDENTITY.
J73217     MOVE HR11F1-PEM-GENDER-IDENTITY-FN
J73217                                   TO HRPEM-GENDER-IDENTITY-FN.
027200     MOVE HR11F1-PEM-EEO-CLASS         TO HRPEM-EEO-CLASS.
027300     MOVE HR11F1-PEM-EEO-CLASS-FN      TO HRPEM-EEO-CLASS-FN.
027400     MOVE HR11F1-PEM-HANDICAP-ID       TO HRPEM-HANDICAP-ID.
027500     MOVE HR11F1-PEM-HANDICAP-ID-FN    TO HRPEM-HANDICAP-ID-FN.
027600     MOVE HR11F1-PEM-VETERAN           TO HRPEM-VETERAN.
027700     MOVE HR11F1-PEM-VETERAN-FN        TO HRPEM-VETERAN-FN.
027600     MOVE HR11F1-PEM-CONSENT           TO HRPEM-CONSENT.
027700     MOVE HR11F1-PEM-CONSENT-FN        TO HRPEM-CONSENT-FN.
           MOVE HR11F1-PEM-VISIBLE-MIN       TO HRPEM-VISIBLE-MIN.
           MOVE HR11F1-PEM-VISIBLE-MIN-FN    TO HRPEM-VISIBLE-MIN-FN.
           MOVE HR11F1-PEM-ABORIGINAL        TO HRPEM-ABORIGINAL.
           MOVE HR11F1-PEM-ABORIGINAL-FN     TO HRPEM-ABORIGINAL-FN.
           MOVE HR11F1-PEM-DISABILITY        TO HRPEM-DISABILITY.
           MOVE HR11F1-PEM-DISABILITY-FN     TO HRPEM-DISABILITY-FN.
           MOVE HR11F1-PEM-RELIGION          TO HRPEM-RELIGION.
           MOVE HR11F1-PEM-RELIGION-FN       TO HRPEM-RELIGION-FN.
027800     MOVE HR11F1-PEM-BIRTH-CITY        TO HRPEM-BIRTH-CITY.
027900     MOVE HR11F1-PEM-BIRTH-CITY-FN     TO HRPEM-BIRTH-CITY-FN.
028000     MOVE HR11F1-PEM-BIRTH-STATE       TO HRPEM-BIRTH-STATE.
028100     MOVE HR11F1-PEM-BIRTH-STATE-FN    TO HRPEM-BIRTH-STATE-FN.
028200     MOVE HR11F1-PEM-BIRTH-CNTRY-CD    TO
028200                                       HRPEM-BIRTH-CNTRY-CD.
028300     MOVE HR11F1-PEM-BIRTH-CNTRY-CD-FN TO HRPEM-BIRTH-CNTRY-CD-FN.
028400     MOVE HR11F1-PEM-MAIDEN-LST-NM     TO HRPEM-MAIDEN-LST-NM.
028500     MOVE HR11F1-PEM-MAIDEN-LST-NM-FN  TO HRPEM-MAIDEN-LST-NM-FN.
028600     MOVE HR11F1-PEM-MAIDEN-FST-NM     TO HRPEM-MAIDEN-FST-NM.
028700     MOVE HR11F1-PEM-MAIDEN-FST-NM-FN  TO HRPEM-MAIDEN-FST-NM-FN.
028800     MOVE HR11F1-PEM-MAIDEN-MI         TO HRPEM-MAIDEN-MI.
028900     MOVE HR11F1-PEM-MAIDEN-MI-FN      TO HRPEM-MAIDEN-MI-FN.
029000     MOVE HR11F1-PEM-FORMER-LST-NM     TO HRPEM-FORMER-LST-NM.
029100     MOVE HR11F1-PEM-FORMER-LST-NM-FN  TO HRPEM-FORMER-LST-NM-FN.
029200     MOVE HR11F1-PEM-FORMER-FST-NM     TO HRPEM-FORMER-FST-NM.
029300     MOVE HR11F1-PEM-FORMER-FST-NM-FN  TO HRPEM-FORMER-FST-NM-FN.
029400     MOVE HR11F1-PEM-FORMER-MI         TO HRPEM-FORMER-MI.
029500     MOVE HR11F1-PEM-FORMER-MI-FN      TO HRPEM-FORMER-MI-FN.
029600     MOVE HR11F1-PEM-FNCTN-GROUP       TO HRPEM-FNCTN-GROUP.
029700     MOVE HR11F1-PEM-FNCTN-GROUP-FN    TO HRPEM-FNCTN-GROUP-FN.
029800     MOVE HR11F1-PEM-EXCLUDE-FLAG      TO HRPEM-EXCLUDE-FLAG.
029900     MOVE HR11F1-PEM-EXCLUDE-FLAG-FN   TO HRPEM-EXCLUDE-FLAG-FN.
030000     MOVE HR11F1-PEM-I9-STATUS         TO HRPEM-I9-STATUS.
030100     MOVE HR11F1-PEM-I9-STATUS-FN      TO HRPEM-I9-STATUS-FN.
030200     MOVE HR11F1-PEM-I9-ALIEN-NBR      TO HRPEM-I9-ALIEN-NBR.
030300     MOVE HR11F1-PEM-I9-ALIEN-NBR-FN   TO HRPEM-I9-ALIEN-NBR-FN.
030400     MOVE HR11F1-PEM-I9-ADMIT-NBR      TO HRPEM-I9-ADMIT-NBR.
030500     MOVE HR11F1-PEM-I9-ADMIT-NBR-FN   TO HRPEM-I9-ADMIT-NBR-FN.
030600     MOVE HR11F1-PEM-I9-STA-EXP-DT     TO HRPEM-I9-STA-EXP-DT.
030700     MOVE HR11F1-PEM-I9-STA-EXP-DT-FN  TO HRPEM-I9-STA-EXP-DT-FN.
030800     MOVE HR11F1-PEM-I9-DOC-NBR1       TO HRPEM-I9-DOC-NBR1.
030900     MOVE HR11F1-PEM-I9-DOC-NBR1-FN    TO HRPEM-I9-DOC-NBR1-FN.
031000     MOVE HR11F1-PEM-I9-DOC-DESCR1     TO HRPEM-I9-DOC-DESCR1.
031100     MOVE HR11F1-PEM-I9-DOC-DESCR1-FN  TO HRPEM-I9-DOC-DESCR1-FN.
031200     MOVE HR11F1-PEM-I9-DOC-TYPE1      TO HRPEM-I9-DOC-TYPE1.
031300     MOVE HR11F1-PEM-I9-DOC-TYPE1-FN   TO HRPEM-I9-DOC-TYPE1-FN.
031400     MOVE HR11F1-PEM-I9-DOC-EXP-DT1    TO HRPEM-I9-DOC-EXP-DT1.
031500     MOVE HR11F1-PEM-I9-DOC-EXP-DT1-FN TO HRPEM-I9-DOC-EXP-DT1-FN.
031600     MOVE HR11F1-PEM-I9-DOC-NBR2       TO HRPEM-I9-DOC-NBR2.
031700     MOVE HR11F1-PEM-I9-DOC-NBR2-FN    TO HRPEM-I9-DOC-NBR2-FN.
031800     MOVE HR11F1-PEM-I9-DOC-DESCR2     TO HRPEM-I9-DOC-DESCR2.
031900     MOVE HR11F1-PEM-I9-DOC-DESCR2-FN  TO HRPEM-I9-DOC-DESCR2-FN.
032000     MOVE HR11F1-PEM-I9-DOC-TYPE2      TO HRPEM-I9-DOC-TYPE2.
032100     MOVE HR11F1-PEM-I9-DOC-TYPE2-FN   TO HRPEM-I9-DOC-TYPE2-FN.
032200     MOVE HR11F1-PEM-I9-DOC-EXP-DT2    TO HRPEM-I9-DOC-EXP-DT2.
032300     MOVE HR11F1-PEM-I9-DOC-EXP-DT2-FN TO HRPEM-I9-DOC-EXP-DT2-FN.
032400     MOVE HR11F1-PEM-I9-AUTHORIZE      TO HRPEM-I9-AUTHORIZE.
032500     MOVE HR11F1-PEM-I9-AUTHORIZE-FN   TO HRPEM-I9-AUTHORIZE-FN.
032600     MOVE HR11F1-PEM-OWNER-FLAG        TO HRPEM-OWNER-FLAG.
032700     MOVE HR11F1-PEM-OWNER-FLAG-FN     TO HRPEM-OWNER-FLAG-FN.
032800     MOVE HR11F1-PEM-OFFICER           TO HRPEM-OFFICER.
032900     MOVE HR11F1-PEM-OFFICER-FN        TO HRPEM-OFFICER-FN.
033000     MOVE HR11F1-PEM-HIGH-COMP         TO HRPEM-HIGH-COMP.
033100     MOVE HR11F1-PEM-HIGH-COMP-FN      TO HRPEM-HIGH-COMP-FN.
033200     MOVE HR11F1-PEM-KEY-EMP-FLAG      TO HRPEM-KEY-EMP-FLAG.
033300     MOVE HR11F1-PEM-KEY-EMP-FLAG-FN   TO HRPEM-KEY-EMP-FLAG-FN.
033400     MOVE HR11F1-PEM-SMOKER            TO HRPEM-SMOKER.
033500     MOVE HR11F1-PEM-SMOKER-FN         TO HRPEM-SMOKER-FN.
033600     MOVE HR11F1-PEM-PRIMARY-CARE      TO HRPEM-PRIMARY-CARE.
033700     MOVE HR11F1-PEM-PRIMARY-CARE-FN   TO HRPEM-PRIMARY-CARE-FN.
HIPAA      MOVE HR11F1-PEM-MEDICARE-IND      TO HRPEM-MEDICARE-IND.
HIPAA      MOVE HR11F1-PEM-MEDICARE-IND-FN   TO HRPEM-MEDICARE-IND-FN.
033800     MOVE HR11F1-PEM-FAMILY-AGG        TO HRPEM-FAMILY-AGG.
033900     MOVE HR11F1-PEM-FAMILY-AGG-FN     TO HRPEM-FAMILY-AGG-FN.
           MOVE HR11F1-PEM-STOCK-TRADE       TO HRPEM-STOCK-TRADE.
           MOVE HR11F1-PEM-STOCK-TRADE-FN    TO HRPEM-STOCK-TRADE-FN.
034000     MOVE HR11F1-PEM-NBR-HL-DEP        TO HRPEM-NBR-HL-DEP.
034100     MOVE HR11F1-PEM-NBR-HL-DEP-FN     TO HRPEM-NBR-HL-DEP-FN.
034200     MOVE HR11F1-PEM-NBR-DN-DEP        TO HRPEM-NBR-DN-DEP.
034300     MOVE HR11F1-PEM-NBR-DN-DEP-FN     TO HRPEM-NBR-DN-DEP-FN.
J12776     MOVE HR11F1-EMP-HICN              TO HREMP-EMP-HICN.
J12776     MOVE HR11F1-EMP-HICN-FN           TO HREMP-EMP-HICN-FN.
           MOVE HR11F1-PEM-RELATED-EMP       TO HRPEM-RELATED-EMP.
           MOVE HR11F1-PEM-RELATED-EMP-FN    TO HRPEM-RELATED-EMP-FN.
034400     MOVE HR11F1-PEM-HL-COV-PROOF      TO HRPEM-HL-COV-PROOF.
034500     MOVE HR11F1-PEM-HL-COV-PROOF-FN   TO HRPEM-HL-COV-PROOF-FN.
034600     MOVE HR11F1-PEM-HL-VERIFY-DT      TO HRPEM-HL-VERIFY-DT.
034700     MOVE HR11F1-PEM-HL-VERIFY-DT-FN   TO HRPEM-HL-VERIFY-DT-FN.
034800     MOVE HR11F1-PEM-DN-COV-PROOF      TO HRPEM-DN-COV-PROOF.
034900     MOVE HR11F1-PEM-DN-COV-PROOF-FN   TO HRPEM-DN-COV-PROOF-FN.
035000     MOVE HR11F1-PEM-DN-VERIFY-DT      TO HRPEM-DN-VERIFY-DT.
035100     MOVE HR11F1-PEM-DN-VERIFY-DT-FN   TO HRPEM-DN-VERIFY-DT-FN.
035200     MOVE HR11F1-PEM-SPOUSE-EMP        TO HRPEM-SPOUSE-EMP.
035300     MOVE HR11F1-PEM-SPOUSE-EMP-FN     TO HRPEM-SPOUSE-EMP-FN.
035400     MOVE HR11F1-PEM-SP-EMP-ADDR1      TO HRPEM-SP-EMP-ADDR1.
035500     MOVE HR11F1-PEM-SP-EMP-ADDR1-FN   TO HRPEM-SP-EMP-ADDR1-FN.
035600     MOVE HR11F1-PEM-SP-EMP-ADDR2      TO HRPEM-SP-EMP-ADDR2.
035700     MOVE HR11F1-PEM-SP-EMP-ADDR2-FN   TO HRPEM-SP-EMP-ADDR2-FN.
035800     MOVE HR11F1-PEM-SP-EMP-ADDR3      TO HRPEM-SP-EMP-ADDR3.
035900     MOVE HR11F1-PEM-SP-EMP-ADDR3-FN   TO HRPEM-SP-EMP-ADDR3-FN.
036000     MOVE HR11F1-PEM-SP-EMP-ADDR4      TO HRPEM-SP-EMP-ADDR4.
036100     MOVE HR11F1-PEM-SP-EMP-ADDR4-FN   TO HRPEM-SP-EMP-ADDR4-FN.
036200     MOVE HR11F1-PEM-SP-EMP-CITY       TO HRPEM-SP-EMP-CITY.
036300     MOVE HR11F1-PEM-SP-EMP-CITY-FN    TO HRPEM-SP-EMP-CITY-FN.
036400     MOVE HR11F1-PEM-SP-EMP-STATE      TO HRPEM-SP-EMP-STATE.
036500     MOVE HR11F1-PEM-SP-EMP-STATE-FN   TO HRPEM-SP-EMP-STATE-FN.
036600     MOVE HR11F1-PEM-SP-EMP-ZIP        TO HRPEM-SP-EMP-ZIP.
036700     MOVE HR11F1-PEM-SP-EMP-ZIP-FN     TO HRPEM-SP-EMP-ZIP-FN.
           MOVE HR11F1-PEM-SP-EMP-COUNTRY    TO HRPEM-SP-EMP-COUNTRY.
           MOVE HR11F1-PEM-SP-EMP-COUNTRY-FN TO
                                             HRPEM-SP-EMP-COUNTRY-FN.
036800     MOVE HR11F1-PEM-SP-EMP-PH-CNTR    TO HRPEM-SP-EMP-PH-CNTR.
036900     MOVE HR11F1-PEM-SP-EMP-PH-CNTR-FN TO HRPEM-SP-EMP-PH-CNTR-FN.
037000     MOVE HR11F1-PEM-SP-EMP-PH-NBR     TO HRPEM-SP-EMP-PH-NBR.
037100     MOVE HR11F1-PEM-SP-EMP-PH-NBR-FN  TO HRPEM-SP-EMP-PH-NBR-FN.
037200     MOVE HR11F1-PEM-HM-PHONE-CNTRY    TO HRPEM-HM-PHONE-CNTRY.
037300     MOVE HR11F1-PEM-HM-PHONE-CNTRY-FN TO HRPEM-HM-PHONE-CNTRY-FN.
037400     MOVE HR11F1-PEM-HM-PHONE-NBR      TO HRPEM-HM-PHONE-NBR.
037500     MOVE HR11F1-PEM-HM-PHONE-NBR-FN   TO HRPEM-HM-PHONE-NBR-FN.
037600     MOVE HR11F1-PEM-SUPP-ADDR1        TO HRPEM-SUPP-ADDR1.
037700     MOVE HR11F1-PEM-SUPP-ADDR1-FN     TO HRPEM-SUPP-ADDR1-FN.
037800     MOVE HR11F1-PEM-SUPP-ADDR2        TO HRPEM-SUPP-ADDR2.
037900     MOVE HR11F1-PEM-SUPP-ADDR2-FN     TO HRPEM-SUPP-ADDR2-FN.
038000     MOVE HR11F1-PEM-SUPP-ADDR3        TO HRPEM-SUPP-ADDR3.
038100     MOVE HR11F1-PEM-SUPP-ADDR3-FN     TO HRPEM-SUPP-ADDR3-FN.
038200     MOVE HR11F1-PEM-SUPP-ADDR4        TO HRPEM-SUPP-ADDR4.
038300     MOVE HR11F1-PEM-SUPP-ADDR4-FN     TO HRPEM-SUPP-ADDR4-FN.
038400     MOVE HR11F1-PEM-SUPP-CITY         TO HRPEM-SUPP-CITY.
038500     MOVE HR11F1-PEM-SUPP-CITY-FN      TO HRPEM-SUPP-CITY-FN.
038600     MOVE HR11F1-PEM-SUPP-STATE        TO HRPEM-SUPP-STATE.
038700     MOVE HR11F1-PEM-SUPP-STATE-FN     TO HRPEM-SUPP-STATE-FN.
038800     MOVE HR11F1-PEM-SUPP-ZIP          TO HRPEM-SUPP-ZIP.
038900     MOVE HR11F1-PEM-SUPP-ZIP-FN       TO HRPEM-SUPP-ZIP-FN.
           MOVE HR11F1-PEM-SUPP-COUNTY       TO HRPEM-SUPP-COUNTY.
           MOVE HR11F1-PEM-SUPP-COUNTY-FN    TO HRPEM-SUPP-COUNTY-FN.
039000     MOVE HR11F1-PEM-SUPP-CNTRY-CD     TO HRPEM-SUPP-CNTRY-CD.
039100     MOVE HR11F1-PEM-SUPP-CNTRY-CD-FN  TO HRPEM-SUPP-CNTRY-CD-FN.
           MOVE HR11F1-PEM-SUPP-PHONE-CNT    TO HRPEM-SUPP-PHONE-CNT.
           MOVE HR11F1-PEM-SUPP-PHONE-CNT-FN TO HRPEM-SUPP-PHONE-CNT-FN.
           MOVE HR11F1-PEM-SUPP-PHONE-NBR    TO HRPEM-SUPP-PHONE-NBR.
           MOVE HR11F1-PEM-SUPP-PHONE-NBR-FN TO HRPEM-SUPP-PHONE-NBR-FN.
039200     MOVE HR11F1-PEM-BEN-SALARY-1      TO HRPEM-BEN-SALARY-1.
039300     MOVE HR11F1-PEM-BEN-SALARY-1-FN   TO HRPEM-BEN-SALARY-1-FN.
           MOVE HR11F1-PEM-BEN-SALARY-1-ND   TO HRPEM-BEN-SALARY-1-ND.
039400     MOVE HR11F1-PEM-BEN-SALARY-2      TO HRPEM-BEN-SALARY-2.
039500     MOVE HR11F1-PEM-BEN-SALARY-2-FN   TO HRPEM-BEN-SALARY-2-FN.
           MOVE HR11F1-PEM-BEN-SALARY-2-ND   TO HRPEM-BEN-SALARY-2-ND.
039600     MOVE HR11F1-PEM-BEN-SALARY-3      TO HRPEM-BEN-SALARY-3.
039700     MOVE HR11F1-PEM-BEN-SALARY-3-FN   TO HRPEM-BEN-SALARY-3-FN.
           MOVE HR11F1-PEM-BEN-SALARY-3-ND   TO HRPEM-BEN-SALARY-3-ND.
039800     MOVE HR11F1-PEM-BEN-SALARY-4      TO HRPEM-BEN-SALARY-4.
039900     MOVE HR11F1-PEM-BEN-SALARY-4-FN   TO HRPEM-BEN-SALARY-4-FN.
           MOVE HR11F1-PEM-BEN-SALARY-4-ND   TO HRPEM-BEN-SALARY-4-ND.
040000     MOVE HR11F1-PEM-BEN-SALARY-5      TO HRPEM-BEN-SALARY-5.
040100     MOVE HR11F1-PEM-BEN-SALARY-5-FN   TO HRPEM-BEN-SALARY-5-FN.
           MOVE HR11F1-PEM-BEN-SALARY-5-ND   TO HRPEM-BEN-SALARY-5-ND.
040200     MOVE HR11F1-PEM-BEN-DATE-1        TO HRPEM-BEN-DATE-1.
040300     MOVE HR11F1-PEM-BEN-DATE-1-FN     TO HRPEM-BEN-DATE-1-FN.
040400     MOVE HR11F1-PEM-BEN-DATE-2        TO HRPEM-BEN-DATE-2.
040500     MOVE HR11F1-PEM-BEN-DATE-2-FN     TO HRPEM-BEN-DATE-2-FN.
040600     MOVE HR11F1-PEM-BEN-DATE-3        TO HRPEM-BEN-DATE-3.
040700     MOVE HR11F1-PEM-BEN-DATE-3-FN     TO HRPEM-BEN-DATE-3-FN.
040800     MOVE HR11F1-PEM-BEN-DATE-4        TO HRPEM-BEN-DATE-4.
040900     MOVE HR11F1-PEM-BEN-DATE-4-FN     TO HRPEM-BEN-DATE-4-FN.
041000     MOVE HR11F1-PEM-BEN-DATE-5        TO HRPEM-BEN-DATE-5.
041100     MOVE HR11F1-PEM-BEN-DATE-5-FN     TO HRPEM-BEN-DATE-5-FN.
041200     MOVE HR11F1-PEM-WORK-ZIP          TO HRPEM-WORK-ZIP.
041300     MOVE HR11F1-PEM-WORK-ZIP-FN       TO HRPEM-WORK-ZIP-FN.
041400     MOVE HR11F1-PEM-LIFE-STYLE-CR     TO HRPEM-LIFE-STYLE-CR.
041500     MOVE HR11F1-PEM-LIFE-STYLE-CR-FN  TO HRPEM-LIFE-STYLE-CR-FN.
           MOVE HR11F1-EMP-LAB-DIST-FLAG     TO HREMP-LAB-DIST-FLAG.
           MOVE HR11F1-EMP-LAB-DIST-FLAG-FN  TO HREMP-LAB-DIST-FLAG-FN.
           MOVE HR11F1-EMP-ENCUMBER-FLAG     TO HREMP-ENCUMBER-FLAG.
           MOVE HR11F1-EMP-ENCUMBER-FLAG-FN  TO HREMP-ENCUMBER-FLAG-FN.
           MOVE HR11F1-EMP-FRNG-RATE         TO HREMP-FRNG-RATE.
           MOVE HR11F1-EMP-FRNG-RATE-FN      TO HREMP-FRNG-RATE-FN.
           MOVE HR11F1-EMP-FRNG-ACCT-CAT     TO HREMP-FRNG-ACCT-CAT.
           MOVE HR11F1-EMP-FRNG-ACCT-CAT-FN  TO HREMP-FRNG-ACCT-CAT-FN.
           MOVE HR11F1-EMP-FRNG-ACCOUNT      TO HREMP-FRNG-ACCOUNT.
           MOVE HR11F1-EMP-FRNG-ACCOUNT-FN   TO HREMP-FRNG-ACCOUNT-FN.
           MOVE HR11F1-EMP-FRNG-SUB-ACCT     TO HREMP-FRNG-SUB-ACCT.
           MOVE HR11F1-EMP-FRNG-SUB-ACCT-FN  TO HREMP-FRNG-SUB-ACCT-FN.
           MOVE HR11F1-EMP-EFFORT-FLAG       TO HREMP-EFFORT-FLAG.
           MOVE HR11F1-EMP-EFFORT-FLAG-FN    TO HREMP-EFFORT-FLAG-FN.
           MOVE HR11F1-EMP-RPT-CURR          TO HREMP-RPT-CURR.
           MOVE HR11F1-EMP-RPT-CURR-FN       TO HREMP-RPT-CURR-FN.
           MOVE HR11F1-EMP-PRMCERT-COMP      TO HREMP-PRMCERT-COMP.
           MOVE HR11F1-EMP-PRMCERT-COMP-FN   TO HREMP-PRMCERT-COMP-FN.
           MOVE HR11F1-EMP-PRMCERT-ID        TO HREMP-PRMCERT-ID.
           MOVE HR11F1-EMP-PRMCERT-ID-FN     TO HREMP-PRMCERT-ID-FN.
           MOVE HR11F1-EMP-SNDCERT-COMP      TO HREMP-SNDCERT-COMP.
           MOVE HR11F1-EMP-SNDCERT-COMP-FN   TO HREMP-SNDCERT-COMP-FN.
           MOVE HR11F1-EMP-SNDCERT-ID        TO HREMP-SNDCERT-ID.
           MOVE HR11F1-EMP-SNDCERT-ID-FN     TO HREMP-SNDCERT-ID-FN.
           MOVE HR11F1-XMIT-HREMP-BLOCK      TO HREMP-XMIT-BLOCK.
           MOVE HR11F1-USER-ID               TO HREMP-USER-ID.
023100     MOVE HR11F1-EMP-EMAIL-PERSONAL    TO HREMP-EMAIL-PERSONAL.
023200     MOVE HR11F1-EMP-EMAIL-PERSONAL-FN TO HREMP-EMAIL-PERSONAL-FN.           
043200
043800     IF (HR11F1-FC = "A")
043900         MOVE WS-SYSTEM-DATE-YMD       TO HREMP-CREATION-DATE
044000         MOVE HR11F1-EFFECT-DATE       TO HREMP-EFFECT-DATE
044100         MOVE HR11F1-EFFECT-DATE-FN    TO HREMP-EFFECT-DATE-FN
044300     ELSE
044400         MOVE HR11F1-EFFECT-DATE       TO HREMP-EFFECT-DATE
044500         MOVE HR11F1-EFFECT-DATE-FN    TO HREMP-EFFECT-DATE-FN.
044600
068600     MOVE HR11F1-DFT-MAR-STAT    TO PRPXL-DFT-MAR-STAT
068700                                    PRPXL-ST-DFT-MAR-STAT.
068800     MOVE HR11F1-DFT-EXEMPTS     TO PRPXL-DFT-EXEMPTS
068900                                    PRPXL-ST-DFT-EXEMPTS.
           MOVE PRS-RECRUIT-FLAG       TO HREMP-RECRUIT-FLAG.
           
J95772     IF (HR11F1-PT-BYPASS-PERS-ACT NOT = ZEROS)
J95772         SET HREMP-BYPASS-PERS-ACT       TO TRUE
J95772     ELSE
J95772         INITIALIZE HREMP-BYPASS-PERS-ACT-SW
J95772     END-IF.           

044700 201-END.
044800
063300******************************************************************
       260-EDIT-DTL-DATA.
063300******************************************************************

           MOVE HR11F1-HRU-FLD-NBR (HR11F1-I1)     TO HR11WS-FLD-NBR-G.
           MOVE HR11WS-FIELD-KEY            TO DB-FIELD-KEY.
           PERFORM 840-FIND-HRUSET1.
           INITIALIZE                          HR11WS-REQ-FLAG.
           IF (HR11WS-COMPANY-REQ)
               MOVE HR11F1-EMP-COMPANY      TO DB-COMPANY
               MOVE "UF"                    TO DB-TOPIC
               INITIALIZE                      DB-COUNTRY-CD-REQ
                                               DB-PROCESS-LEVEL
P95622         MOVE HR11WS-FLD-NBR-N        TO DB-FLD-NBR
               PERFORM 840-FIND-PASSET4
               IF (PASCRTY-FOUND)
                   MOVE PAS-REQ-FLAG        TO HR11WS-REQ-FLAG
               END-IF
           ELSE
               MOVE HR11F1-EMP-COMPANY      TO DB-COMPANY
               MOVE "UF"                    TO DB-TOPIC
               MOVE HR11F1-EMP-WORK-COUNTRY TO DB-COUNTRY-CD-REQ
               MOVE SPACES                  TO DB-PROCESS-LEVEL
P95622         MOVE HR11WS-FLD-NBR-N        TO DB-FLD-NBR
               PERFORM 840-FIND-PASSET4
               IF (PASCRTY-FOUND)
                   MOVE PAS-REQ-FLAG        TO HR11WS-REQ-FLAG
               END-IF
           END-IF.

           INITIALIZE HR11WS-A-VALUE
                      HR11WS-N-VALUE
                      HR11WS-D-VALUE.

           IF  (HR11WS-REQ-FLAG                = "X")
           AND (HR11F1-VALUE (HR11F1-I1)       = SPACES)
      ******** User field is required
               MOVE "A"                    TO HR11F1-LINE-FC (HR11F1-I1)
               MOVE 141                    TO CRT-ERROR-NBR
               MOVE HR11F1-VALUE-FN (HR11F1-I1)    TO CRT-FIELD-NBR
               GO TO 260-END.

           IF (HR11F1-LINE-FC (HR11F1-I1)          = SPACES)
               GO TO 260-END.

           IF  (HR11F1-LINE-FC (HR11F1-I1) = "A" OR "C")
           AND (HR11F1-HRU-FIELD-NAME (HR11F1-I1) = SPACES)
               MOVE 166                                 TO CRT-ERROR-NBR
               MOVE HR11F1-LINE-FC-FN (HR11F1-I1)       TO CRT-FIELD-NBR
               GO TO 260-END.

           IF  (HR11F1-LINE-FC (HR11F1-I1) = "D")
           AND (HR11F1-VALUE (HR11F1-I1)   = SPACES)
      ************ Cannot delete; user field has no value
               MOVE 163                                 TO CRT-ERROR-NBR
               MOVE HR11F1-LINE-FC-FN (HR11F1-I1)       TO CRT-FIELD-NBR
               GO TO 260-END.

           IF (HR11F1-VALUE (HR11F1-I1)            NOT = SPACES)
               IF (HRU-FIELD-TYPE           = "A")
                   MOVE HR11F1-VALUE(HR11F1-I1)    TO HRWS-UP-FIELD
                   PERFORM 760-HR-UPPER-CASE
                   MOVE HRWS-UP-FIELD       TO HR11WS-A-VALUE
                   IF  (HR11WS-A-VAL-EXTRA NOT = SPACES)
                       MOVE 147                         TO CRT-ERROR-NBR
                       MOVE HR11F1-VALUE-FN (HR11F1-I1) TO CRT-FIELD-NBR
                       GO TO 260-END
                   END-IF
               ELSE
               IF (HRU-FIELD-TYPE           = "N")
                   MOVE HR11F1-HRU-FLD-NBR (HR11F1-I1) TO DB-FLD-NBR
                   PERFORM 840-FIND-PADSET1
                   MOVE HR11F1-VALUE (HR11F1-I1)   TO HRWS-VALUE
                   MOVE PAD-SIZE            TO HRWS-SIZE
                   MOVE PAD-DECIMALS        TO HRWS-NBR-DECIMALS
                   PERFORM 770-HR-EDIT-DEC-FIELD
                   IF (ERROR-FOUND)
                       MOVE HR11F1-VALUE-FN (HR11F1-I1) TO CRT-FIELD-NBR
                       GO TO 260-END
                   END-IF
                   MOVE HRWS-DEC-FIELD      TO HR11WS-N-VALUE
                   MOVE HRWS-VALUE          TO HR11F1-VALUE (HR11F1-I1)
               ELSE
               IF (HRU-FIELD-TYPE           = "D")
                   MOVE HR11F1-VALUE (HR11F1-I1)   TO HRWS-VALUE
                   PERFORM 780-HR-REFORMAT-DATE-FIELD
                   IF (ERROR-FOUND)
                       MOVE HR11F1-VALUE-FN (HR11F1-I1) TO CRT-FIELD-NBR
                       GO TO 260-END
                   END-IF
                   MOVE HRWS-DATE-8-FIELD   TO HR11WS-D-VALUE
                   MOVE HRWS-VALUE          TO HR11F1-VALUE (HR11F1-I1).

           PERFORM 520-MOVE-DTL-SCR-TO-WS
           THRU    520-END.

           PERFORM 2000-HRHEU-EDIT-TRAN.

           PERFORM 530-MOVE-WS-TO-DTL-SCR
           THRU    530-END.

           IF (ERROR-FOUND)
               GO TO 260-END.
133100
       260-END.

063300******************************************************************
063400 400-PROCESS-TRAN.
063500******************************************************************
063600
063700     IF (HR11F1-FC = "A" OR "C" OR "D" OR "R")
064500         PERFORM 410-UPDATE
064600         THRU    410-END
065400     ELSE
065500     IF (HR11F1-FC = "I" OR "+" OR "-" OR "N" OR "P")
065600         PERFORM 480-INQUIRE
065700         THRU    480-END.
065800
065900 400-END.
066000******************************************************************
066100 410-UPDATE.
066200******************************************************************
066300
           PERFORM 910-AUDIT-BEGIN.

066400     INITIALIZE PRRQC-STD-DED-COUNT
066500                HREMP-NBR-PLANS
066600                HRWS-NBR-GROUPS.
066700
066800     IF (HR11F1-FC = "D")
               INITIALIZE CMTPAF-TYPE
                          CMTPAF-NAME
                          CMTPAI-TYPE
                          CMTPAI-NAME
                          CMTPAV-TYPE
                          CMTPAV-NAME
                          CMTPTS-TYPE
                          CMTPTS-NAME
P60986         SET HREMP-UPDATE-M3     TO TRUE
066900         PERFORM 3300-HREMP-DELETE
P60986*---------------------------------+
P60986* Release Work Unit               |
P60986*---------------------------------+
P60986         INITIALIZE CRT-MESSAGE
P60986         INITIALIZE WFAPI-INPUT
P60986         MOVE WFAPI-O-WORKUNIT   TO WFAPI-I-WORKUNIT
P60986         INITIALIZE WFAPI-OUTPUT
P60986         PERFORM 1000-WF-RELEASE-SETUP
067000         MOVE CRT-RECS-DELETED   TO CRT-MESSAGE
067100         PERFORM 920-AUDIT-END
067200         MOVE ZEROES             TO HR11F1-EFFECT-DATE
067300         GO TO 410-END.
067400
067500     IF (HR11F1-FC = "A" OR "C")
               INITIALIZE PRPXL-PARM-EFF-DATE
               IF  (HR11F1-FC = "A")
                   MOVE HR11F1-EDM-EFFECT-DATE TO PRPXL-PARM-EFF-DATE
               END-IF
               IF (HR11F1-FC = "A")
                   MOVE "Y" TO HRWS-ADD
               ELSE
                   MOVE "N" TO HRWS-ADD
               END-IF
P60986         SET HREMP-UPDATE-M3           TO TRUE

J40211         IF  (PAEMPLOYEE-FOUND)
J40211         AND (PEM-EEO-CLASS NOT = HR11F1-PEM-EEO-CLASS)
J40211             MOVE HREMP-EMPLOYEE           TO DB-EMPLOYEE
J40211             MOVE EERSET1-EMPLOYEE         TO WS-DB-BEG-RNG
J40211             PERFORM 830-DELETERNG-EERSET1
J40211         END-IF

067600         PERFORM 3000-HREMP-PROCESS-TRAN
P60986*---------------------------------+
P60986* Release Work Unit               |
P60986*---------------------------------+
P60986         INITIALIZE CRT-MESSAGE
P60986         INITIALIZE WFAPI-INPUT
P60986         MOVE WFAPI-O-WORKUNIT   TO WFAPI-I-WORKUNIT
P60986         INITIALIZE WFAPI-OUTPUT
P60986         PERFORM 1000-WF-RELEASE-SETUP
               MOVE HREMP-LAST-NAME          TO HRWS-LAST-NAME
               MOVE HREMP-FIRST-NAME         TO HRWS-FIRST-NAME
               MOVE HREMP-MIDDLE-NAME        TO HRWS-MIDDLE-INIT
               MOVE HREMP-NAME-SUFFIX        TO HRWS-NAME-SUFFIX
               MOVE HREMP-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE
               PERFORM 750-HR-FORMAT-NAME
               MOVE HRWS-FORMAT-NAME       TO HREMP-FULL-NAME
067700         PERFORM 600-MOVE-WS-TO-SCR
067800         THRU    600-END.
068100
           MOVE HRWS-NBR-GROUPS            TO HR11WS-NBR-GROUPS.

           INITIALIZE                         HRWS-NBR-GROUPS.
067500     IF (HR11F1-FC = "A" OR "C")
               PERFORM 415-UPDATE-USER-FIELDS
               THRU    415-END.

           COMPUTE HRWS-NBR-GROUPS =
                            (HRWS-NBR-GROUPS + HR11WS-NBR-GROUPS).

068200     IF (HR11F1-FC = "A" OR "R")
068300         MOVE HR11F1-EMP-COMPANY     TO PRRQC-COMPANY
068400         MOVE HR11F1-EMP-EMPLOYEE    TO PRRQC-EMPLOYEE
068600         MOVE HR11F1-DFT-MAR-STAT    TO PRRQC-DFT-MAR-STAT
068700                                        PRRQC-ST-DFT-MAR-STAT
068800         MOVE HR11F1-DFT-EXEMPTS     TO PRRQC-DFT-EXEMPTS
068900                                        PRRQC-ST-DFT-EXEMPTS
               MOVE HR11F1-EDM-EFFECT-DATE TO PRRQC-EFFECT-DATE
               MOVE HR11F1-EDM-END-DATE    TO PRRQC-END-DATE
               INITIALIZE PRRQC-UPDATE-OPTION
069000         PERFORM 500-REQ-DED-CREATION
               INITIALIZE CRT-ERROR-CAT
                          CRT-ERROR-NBR
               INITIALIZE HR11F1-PT-FROM-PR134.
069100
           IF  (HR11F1-FC = "R")
               IF  (PRRQC-STD-DED-COUNT     NOT = ZEROS)
                   MOVE PRRQC-I1            TO HR11WS-NUMBER-N
                   MOVE HR11WS-NUMBER-A     TO CRT-ERR-VAR1
                   MOVE PRRQC-I2            TO HR11WS-NUMBER-N
                   MOVE HR11WS-NUMBER-A     TO CRT-ERR-VAR2
                   MOVE PRRQC-I3            TO HR11WS-NUMBER-N
                   MOVE HR11WS-NUMBER-A     TO CRT-ERR-VAR3
                   MOVE 125                 TO CRT-MSG-NBR
               ELSE
                   MOVE 124                TO CRT-MSG-NBR
               END-IF
               GO TO 410-CONTINUE
           END-IF.

069200     IF (HR11F1-FC = "A")
069300        PERFORM 5000-HREMP-CREATE-ETM.
069400
           INITIALIZE HRWS-VALUE.

           IF  (PRPXL-TAXES NOT = ZEROES)
               MOVE PRPXL-TAXES        TO HR11WS-NUMBER-N
               MOVE HR11WS-NUMBER-A    TO CRT-ERR-VAR1
               MOVE 151                TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE        TO HRWS-VALUE2
               SET HRWS-APPEND-SPACE TO TRUE
               PERFORM 730-HR-APPEND-STRING
           END-IF.

           IF  (PRRQC-STD-DED-COUNT NOT = ZEROES)
               MOVE PRRQC-STD-DED-COUNT TO HR11WS-NUMBER-N
               MOVE HR11WS-NUMBER-A    TO CRT-ERR-VAR1
               MOVE 152                TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE        TO HRWS-VALUE2
               SET HRWS-APPEND-SPACE TO TRUE
               PERFORM 730-HR-APPEND-STRING
           END-IF.

J13588*    IF  (NOT EDCDWS-TRIGGER-ENABLED)
J13588*    AND (HREMP-NBR-PLANS NOT = ZEROES)
J13588*        MOVE HREMP-NBR-PLANS    TO HR11WS-NUMBER-N
J13588*        MOVE HR11WS-NUMBER-A    TO CRT-ERR-VAR1
J13588*        MOVE 153                TO CRT-MSG-NBR
J13588*        PERFORM 790-GET-MSG
J13588*        MOVE CRT-MESSAGE        TO HRWS-VALUE2
J13588*        SET HRWS-APPEND-SPACE TO TRUE
J13588*        PERFORM 730-HR-APPEND-STRING
J13588*    END-IF.

           IF  (HRWS-NBR-GROUPS NOT = ZEROES)
               MOVE HRWS-NBR-GROUPS    TO HR11WS-NUMBER-N
               MOVE HR11WS-NUMBER-A    TO CRT-ERR-VAR1
               MOVE 154                TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE        TO HRWS-VALUE2
               SET HRWS-APPEND-SPACE TO TRUE
               PERFORM 730-HR-APPEND-STRING
           END-IF.

           IF  (HR11F1-FC = "A")
J13588*    AND (EDCDWS-TRIGGER-ENABLED)
               MOVE HREMP-NBR-PLANS    TO HR11WS-NUMBER-N
               MOVE HR11WS-NUMBER-A    TO CRT-ERR-VAR1
               MOVE 156                TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE        TO HRWS-VALUE2
               SET HRWS-APPEND-SPACE TO TRUE
               PERFORM 730-HR-APPEND-STRING
           END-IF.

           MOVE HRWS-VALUE             TO HRWS-VALUE2.

           IF  (HR11F1-FC = "A")
P63307         INITIALIZE              HR11WS-PREV-FC
               IF (HRWS-GROUP-SIZE-ERROR)
                   MOVE 164            TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
                   MOVE CRT-MESSAGE        TO HRWS-VALUE2
                   SET HRWS-APPEND-SPACE TO TRUE
                   PERFORM 730-HR-APPEND-STRING
               ELSE
               IF  (HRWS-VALUE2 = SPACES)
                   MOVE CRT-ADD-COMPLETE   TO CRT-MESSAGE
               ELSE
                   MOVE 161            TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
                   MOVE CRT-MESSAGE    TO HRWS-VALUE
                   SET HRWS-APPEND-SPACE TO TRUE
                   PERFORM 730-HR-APPEND-STRING
                   MOVE HRWS-VALUE     TO CRT-MESSAGE
               END-IF
               END-IF
           ELSE
               IF (HRWS-GROUP-SIZE-ERROR)
                   MOVE 165            TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
                   MOVE CRT-MESSAGE        TO HRWS-VALUE2
                   SET HRWS-APPEND-SPACE TO TRUE
                   PERFORM 730-HR-APPEND-STRING
               ELSE
               IF  (HRWS-VALUE2 = SPACES)
                   MOVE CRT-CHG-COMPLETE   TO CRT-MESSAGE
               ELSE
                   MOVE 162            TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
                   MOVE CRT-MESSAGE    TO HRWS-VALUE
                   SET HRWS-APPEND-SPACE TO TRUE
                   PERFORM 730-HR-APPEND-STRING
                   MOVE HRWS-VALUE     TO CRT-MESSAGE
               END-IF
               END-IF
           END-IF.

       410-CONTINUE.

083000     PERFORM 920-AUDIT-END.

           INITIALIZE                    HR11F1-EFFECT-DATE.

           IF  (PRPXL-WH)
               MOVE "I"                TO CRT-PASS-FC
               MOVE "C"                TO CRT-DISPLAY-FC
               MOVE CRT-MANUAL-CF      TO CRT-REQUEST
               IF (HREMP-US-WORK-COUNTRY)
                   MOVE "PR132"        TO CRT-SCREEN-CODE
               END-IF
           ELSE
082000     IF (PRRQC-TAX-DEDS-CREATED = "Y")
082100         MOVE "I"                TO CRT-PASS-FC
082200         MOVE "C"                TO CRT-DISPLAY-FC
082300         MOVE CRT-MANUAL-CF      TO CRT-REQUEST
               IF (HREMP-CA-WORK-COUNTRY)
                   MOVE "PR136"        TO CRT-SCREEN-CODE               
               END-IF
082500         MOVE SPACES             TO PRRQC-TAX-DEDS-CREATED
               GO TO 410-END.

087600 410-END.
087700
087800******************************************************************
       415-UPDATE-USER-FIELDS.
087800******************************************************************

      **** UPDATE USER FIELDS SECTION ****

           INITIALIZE HRWS-FIELD-NBR-TABLE
                      HR11F1-I2.

           MOVE 1                      TO HR11F1-REC.
           PERFORM
               UNTIL (HR11F1-REC       > HR11F1-LAST-USER-FLD)

               INITIALIZE HR11F1-DETAIL-GROUP

               IF (HR11F1-REC = ZEROES)
                   MOVE 1                  TO HR11F1-REC
               END-IF
               PERFORM
                   VARYING HR11F1-I1 FROM 1 BY 1
                   UNTIL  (HR11F1-I1          > HR11WS-NBR-OF-DTL-LN)
                   OR     (HR11F1-WS-FLD-NBR (HR11F1-REC) = ZEROES)

                   PERFORM 610-MOVE-DTL-TO-SCREEN
                   THRU    610-END

                   MOVE HR11F1-EMP-COMPANY             TO DB-COMPANY
                   INITIALIZE DB-COUNTRY-CD-REQ
                              DB-PROCESS-LEVEL
                   MOVE HR11F1-WS-FLD-NBR (HR11F1-REC - 1)
                                                       TO DB-FLD-NBR
                   PERFORM 840-FIND-PASSET1
                   IF  (PASCRTY-FOUND)
                   AND (PAS-USED-BY-GROUP = "X")
                   AND (HR11F1-WS-LINE-FC (HR11F1-REC - 1) NOT = SPACES)
                       ADD 1              TO HR11F1-I2
                       MOVE DB-FLD-NBR    TO HRWS-FIELD-NBR (HR11F1-I2)
                   END-IF

                   INITIALIZE HR11F1-WS-LINE-FC (HR11F1-REC - 1)
                              HR11F1-WS-VALUE (HR11F1-REC - 1)
                              HR11F1-WS-CURR (HR11F1-REC - 1)
                              HR11F1-WS-BASE-AMT (HR11F1-REC - 1)

               END-PERFORM

               PERFORM
                   VARYING HR11F1-I1 FROM 1 BY 1
                   UNTIL  (HR11F1-I1          > HR11WS-NBR-OF-DTL-LN)
                   OR     (HR11F1-HRU-FLD-NBR (HR11F1-I1) = ZEROES)

                   IF (HR11F1-LINE-FC (HR11F1-I1) NOT = SPACES)
                       PERFORM 540-MOVE-TO-AND-VALUE
                       THRU    540-END

                       PERFORM 520-MOVE-DTL-SCR-TO-WS
                       THRU    520-END

                       PERFORM 2000-HRHEU-EDIT-TRAN

171400                 PERFORM 3000-HRHEU-PROCESS-TRAN
                   END-IF

                   INITIALIZE HR11F1-LINE-FC (HR11F1-I1)

               END-PERFORM
           END-PERFORM.

           MOVE HR11F1-SV-TOP-REC      TO HR11F1-REC.
           INITIALIZE HR11F1-DETAIL-GROUP.

           PERFORM
               VARYING HR11F1-I1 FROM 1 BY 1
               UNTIL  (HR11F1-I1              > HR11WS-NBR-OF-DTL-LN)
               OR     (HR11F1-WS-FLD-NBR (HR11F1-REC) = ZEROES)

               PERFORM 610-MOVE-DTL-TO-SCREEN
               THRU    610-END

           END-PERFORM.

           IF (HR11F1-I2               NOT = ZEROES)
               MOVE WS-FALSE           TO HRWS-ALL-EMPLOYEES-SW
               MOVE EMP-COMPANY        TO HRWS-COMPANY
               MOVE HR11F1-EFFECT-DATE TO HRWS-EFFECT-DATE
               MOVE EMP-COMPANY        TO DB-COMPANY
               MOVE EMP-EMPLOYEE       TO DB-EMPLOYEE
               PERFORM 840-FIND-PEMSET1
               PERFORM 5000-REBUILD-PERSONNEL-GROUP.

       415-END.

087800******************************************************************
087900 480-INQUIRE.
088000******************************************************************

           MOVE HR11F1-EMP-COMPANY      TO HR11F1-HREMP-COMPANY.
           MOVE HR11F1-EMP-EMPLOYEE     TO HR11F1-HREMP-EMPLOYEE.

           IF (HR11F1-FC               NOT = "+" AND "-")
           OR (HR11F1-EMP-EMPLOYEE     NOT = HR11F1-PT-EMP-EMPLOYEE)
088100         PERFORM 600-MOVE-WS-TO-SCR
088200         THRU    600-END.

           PERFORM 490-MOVE-CUR-PG-TO-WS
           THRU    490-END.

           INITIALIZE                     HR11F1-MORE-MSG.
           IF  (HR11F1-WORK-FILE-NAME NOT = SPACES)
               MOVE HR11F1-WORK-FILE-NAME  TO SYSCMD-FILENAME
               PERFORM 900-FILEEXISTS
           END-IF.

           IF (HR11F1-WORK-FILE-NAME     = SPACES)
           OR (SYSCMD-ERROR       NOT = ZEROES)
           OR (HR11F1-FC          = "I" OR "N" OR "P")
           OR (HR11F1-EMP-EMPLOYEE     NOT = HR11F1-PT-EMP-EMPLOYEE)
               MOVE HR11F1-EMP-COMPANY TO HRHP2-COMPANY
               MOVE "UF"               TO HRHP2-TOPIC
               MOVE "Y"                TO HRHP2-SORT-BY-REQ
               MOVE HR11F1-EMP-WORK-COUNTRY
                                       TO HRHP2-WORK-COUNTRY
               MOVE HR11F1-WORK-FILE-NAME
                                       TO WS-WORK-FILE-NAME
               PERFORM 5000-CREATE-HRPADICT2
               MOVE HRHP2-NBR-OF-RECS  TO HR11F1-HRHP2-RECS
               MOVE WS-WORK-FILE-NAME  TO HR11F1-WORK-FILE-NAME
               MOVE "N"                TO HR11F1-HRU-TABLE-CREATED
               MOVE HRHP2-WORK-COUNTRY TO HR11F1-WORK-FILE-COUNTRY.

166500     IF (HR11F1-HRHP2-RECS > HR11WS-MAX-USER-FLD)
166600         MOVE 144                    TO CRT-MSG-NBR
166700         MOVE "HR11"                 TO CRT-ERROR-CAT
166800         PERFORM 790-GET-MSG
166900         MOVE CRT-MESSAGE            TO HR11F1-USER-FIELDS
               MOVE SPACES                 TO CRT-MESSAGE
          ELSE
               INITIALIZE HR11F1-USER-FIELDS.
167000
           MOVE HR11F1-WORK-FILE-NAME  TO WS-WORK-FILE-NAME.
           SET HR11WS-HRPADICT2-OPEN   TO TRUE.
000060     PERFORM 9820-OPEN-INPUT-HRPADICT2.

           IF (HR11F1-HRU-TABLE-CREATED NOT = "Y")
               MOVE "Y"                 TO HR11F1-HRU-TABLE-CREATED
               MOVE 1                   TO HR11F1-I1
               MOVE "UF"                TO HP2-TOPIC
011900         INITIALIZE HP2-REQ-FLAG
011900                    HP2-ITEM-NAME
                          HP2-FLD-NBR
012600         PERFORM 8500-FIND-NLT-HRPADICT2
               PERFORM
                   UNTIL (HRPADICT2-NOTFOUND)
                   OR    (HR11F1-I1            > HR11WS-MAX-USER-FLD)

                       MOVE HP2-FLD-NBR TO HR11F1-WS-FLD-NBR (HR11F1-I1)
                       IF (HP2-REQ-FLAG = SPACES)
                           MOVE "X"    TO HR11F1-WS-REQ-FLAG (HR11F1-I1)
                       ELSE
                           INITIALIZE HR11F1-WS-REQ-FLAG (HR11F1-I1)
                   END-IF

                   INITIALIZE HR11F1-WS-LINE-FC (HR11F1-I1)
                              HR11F1-WS-VALUE (HR11F1-I1)
                              HR11F1-WS-CURR (HR11F1-I1)
                              HR11F1-WS-BASE-AMT (HR11F1-I1)

                   PERFORM 8600-FIND-NEXT-HRPADICT2

                   ADD 1                TO HR11F1-I1

               END-PERFORM
               COMPUTE HR11F1-LAST-USER-FLD = HR11F1-I1
                                            - 1
011900         MOVE 1                       TO HR11F1-REC
011900                                         HR11F1-TOP-REC.

           INITIALIZE HR11F1-DETAIL-GROUP.

           IF (HR11F1-FC                   = "-")
005600         PERFORM 610-MOVE-DTL-TO-SCREEN
005700         THRU    610-END
                   VARYING HR11F1-I1 FROM HR11WS-NBR-OF-DTL-LN
                                     BY NEGATIVE-ONE
                   UNTIL  (HR11F1-I1              < 1)
                   OR     (HR11F1-REC      < 1)
009000         IF (HR11F1-REC              < 1)
                   MOVE 1                  TO HR11F1-REC
009100             MOVE CRT-INQ-COMPLETE   TO CRT-MESSAGE
009200         ELSE
                   IF (HR11F1-REC < HR11F1-TOP-REC)
                       COMPUTE HR11F1-REC =
                           (HR11F1-TOP-REC + HR11WS-NBR-OF-DTL-LN)
                   END-IF
                   MOVE 146                TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
                   MOVE CRT-MESSAGE        TO HR11F1-MORE-MSG
009300             MOVE CRT-MORE-RECS      TO CRT-MESSAGE
               END-IF
           ELSE
005600         PERFORM 610-MOVE-DTL-TO-SCREEN
005700         THRU    610-END
                   VARYING HR11F1-I1 FROM 1 BY 1
                   UNTIL  (HR11F1-I1       > HR11WS-NBR-OF-DTL-LN)
                   OR     (HR11F1-REC      > HR11F1-LAST-USER-FLD)
009000         IF (HR11F1-REC              > HR11F1-LAST-USER-FLD)
009100             MOVE CRT-INQ-COMPLETE   TO CRT-MESSAGE
009200         ELSE
                   MOVE 146                TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
                   MOVE CRT-MESSAGE        TO HR11F1-MORE-MSG
009300             MOVE CRT-MORE-RECS      TO CRT-MESSAGE.

J27500     INITIALIZE                      HR11F1-ADD-ALTERNATE-NAME
J27500                                     HR11F1-ALTERNATE-FLAG.
J27500     MOVE "/EMPALTNAME" TO HRLAWDWS-DIR-EXT.
J27500     PERFORM 1000-GET-LAWDIR-FILENAME-PD.
J27500     MOVE HRLAWDWS-LAWDIR-EXT    TO SYSCMD-FILENAME.
J27500     PERFORM 900-FILEEXISTS.
J27500     IF (SYSCMD-ERROR  = ZEROES)
J27500         MOVE HR11F1-EMP-COMPANY     TO DB-COMPANY
J27500         MOVE HR11F1-EMP-EMPLOYEE    TO DB-EMPLOYEE
J27500         PERFORM 840-FIND-EANSET1
J27500         IF (EMPALTNAME-NOTFOUND)
J27500            MOVE 183                 TO CRT-MSG-NBR
J27500            MOVE "HR11"              TO CRT-ERROR-CAT
J27500            PERFORM 790-GET-MSG
J27500            MOVE CRT-MESSAGE TO HR11F1-ADD-ALTERNATE-NAME
J27500         ELSE
J27500            MOVE "*"                 TO HR11F1-ALTERNATE-FLAG
J27500            MOVE 184                 TO CRT-MSG-NBR
J27500            MOVE "HR11"              TO CRT-ERROR-CAT
J27500            PERFORM 790-GET-MSG
J27500            MOVE CRT-MESSAGE TO HR11F1-ADD-ALTERNATE-NAME
J27500         END-IF
J27500     END-IF.

           IF (HR11F1-FC                   NOT = "+" AND "-")
               MOVE 155                    TO CRT-MSG-NBR
               MOVE "HR11"                 TO CRT-ERROR-CAT
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE            TO HR11F1-GRANT
               MOVE CRT-INQ-COMPLETE       TO CRT-MESSAGE.
088400
088500 480-END.

186200******************************************************************
       490-MOVE-CUR-PG-TO-WS.
186200******************************************************************

           MOVE HR11F1-TOP-REC             TO I2.
           PERFORM
               VARYING HR11F1-I1 FROM 1 BY 1
               UNTIL  (HR11F1-I1           > HR11WS-NBR-OF-DTL-LN)
               OR     (HR11F1-HRU-FLD-NBR (HR11F1-I1) = ZEROES)

               MOVE HR11F1-LINE-FC (HR11F1-I1) TO HR11F1-WS-LINE-FC (I2)
               MOVE HR11F1-VALUE (HR11F1-I1)   TO HR11F1-WS-VALUE (I2)
               MOVE HR11F1-HEU-CURRENCY-CODE (HR11F1-I1)
                                           TO HR11F1-WS-CURR (I2)
               MOVE HR11F1-HEU-BASE-AMOUNT (HR11F1-I1)
                                           TO HR11F1-WS-BASE-AMT (I2)

               ADD 1                       TO I2
           END-PERFORM.

       490-END.

186200******************************************************************
186300 520-MOVE-DTL-SCR-TO-WS.
186400******************************************************************
186500
           INITIALIZE HRHEU-SCR-FIELDS.

186700     INITIALIZE HRHEU-EMP-APP.
186700     MOVE WS-HIGH-VALUES              TO HRHEU-EMP-APP-FN.

186800     MOVE HR11F1-EMP-COMPANY          TO HRHEU-COMPANY.
186800     MOVE HR11F1-EMP-COMPANY-FN       TO HRHEU-COMPANY-FN.

186900     MOVE HR11F1-EMP-EMPLOYEE         TO HRHEU-EMPLOYEE.
186900     MOVE HR11F1-EMP-EMPLOYEE-FN      TO HRHEU-EMPLOYEE-FN.

187000     MOVE HR11F1-EFFECT-DATE          TO HRHEU-EFFECT-DATE.

           MOVE HR11F1-LINE-FC (HR11F1-I1)         TO HRHEU-FC.
           MOVE HR11F1-LINE-FC-FN (HR11F1-I1)      TO HRHEU-FC-FN.

           MOVE HRU-FIELD-KEY               TO HRHEU-FIELD-KEY.
           MOVE WS-HIGH-VALUES              TO HRHEU-FIELD-KEY-FN.

186600     MOVE HRU-FIELD-TYPE              TO HRHEU-FIELD-TYPE.
186600     MOVE WS-HIGH-VALUES              TO HRHEU-FIELD-TYPE-FN.

           MOVE HRU-FIELD-NAME              TO HRHEU-FIELD-NAME.
           MOVE WS-HIGH-VALUES              TO HRHEU-FIELD-NAME-FN.

           MOVE HR11WS-A-VALUE              TO HRHEU-A-FIELD.
           MOVE HR11WS-N-VALUE              TO HRHEU-N-FIELD.
           MOVE HR11WS-D-VALUE              TO HRHEU-D-FIELD.
           MOVE HR11F1-VALUE-FN (HR11F1-I1)        TO HRHEU-A-FIELD-FN
                                               HRHEU-N-FIELD-FN
                                               HRHEU-D-FIELD-FN.

           MOVE HR11F1-HEU-CURRENCY-CODE (HR11F1-I1)
                                            TO HRHEU-CURRENCY-CODE.
           MOVE HR11F1-HEU-CURRENCY-CODE-FN (HR11F1-I1)
                                            TO HRHEU-CURRENCY-CODE-FN.

           MOVE HR11F1-HEU-BASE-CURRENCY (HR11F1-I1)
                                            TO HRHEU-BASE-CURRENCY.

           MOVE HR11F1-HEU-BASE-AMOUNT (HR11F1-I1)
                                            TO HRHEU-BASE-AMOUNT.
           MOVE HR11F1-HEU-BASE-AMOUNT-FN (HR11F1-I1)
                                            TO HRHEU-BASE-AMOUNT-FN.
           MOVE HR11F1-HEU-BASE-AMOUNT-ND (HR11F1-I1)
                                            TO HRHEU-BASE-AMOUNT-ND.

           MOVE HR11F1-HEU-CURR-ND (HR11F1-I1)     TO HRHEU-CURR-ND.
           MOVE HR11F1-HEU-BASE-ND (HR11F1-I1)     TO HRHEU-BASE-ND.
196600
           MOVE HRU-LOG-FLAG                TO HRHEU-LOG-FLAG.
           MOVE WS-HIGH-VALUES              TO HRHEU-LOG-FLAG-FN.

P72310     IF (HRHEU-WORK-COUNTRY = SPACES)
P72310        MOVE HRSEC-WORK-COUNTRY       TO HRHEU-WORK-COUNTRY.

196700 520-END.
196800
      ******************************************************************
       530-MOVE-WS-TO-DTL-SCR.
      ******************************************************************

           MOVE HRHEU-FC               TO HR11F1-LINE-FC (HR11F1-I1).
           MOVE HRHEU-FC-FN            TO HR11F1-LINE-FC-FN (HR11F1-I1).

           MOVE HRHEU-FIELD-TYPE       TO
                                      HR11F1-HRU-FIELD-TYPE (HR11F1-I1).
           MOVE HRHEU-FIELD-TYPE-FN    TO
                                   HR11F1-HRU-FIELD-TYPE-FN (HR11F1-I1).

           MOVE HRHEU-FIELD-NAME       TO
                                      HR11F1-HRU-FIELD-NAME (HR11F1-I1).
           MOVE HRHEU-FIELD-NAME-FN    TO
                                   HR11F1-HRU-FIELD-NAME-FN (HR11F1-I1).

           IF (HRHEU-FIELD-TYPE        = "A")
               MOVE HRHEU-A-FIELD-FN   TO HR11F1-VALUE-FN (HR11F1-I1)
           ELSE
           IF (HRHEU-FIELD-TYPE        = "D")
               MOVE HRHEU-D-FIELD-FN   TO HR11F1-VALUE-FN (HR11F1-I1)
           ELSE
           IF (HRHEU-FIELD-TYPE        = "N")
               MOVE HRHEU-N-FIELD-FN   TO HR11F1-VALUE-FN (HR11F1-I1).

           MOVE HRHEU-CURRENCY-CODE    TO
                                   HR11F1-HEU-CURRENCY-CODE (HR11F1-I1).
           MOVE HRHEU-CURRENCY-CODE-FN TO
                                HR11F1-HEU-CURRENCY-CODE-FN (HR11F1-I1).

           MOVE HRHEU-BASE-AMOUNT      TO
                                     HR11F1-HEU-BASE-AMOUNT (HR11F1-I1).
           MOVE HRHEU-BASE-AMOUNT-ND   TO
                                   HR11F1-HEU-BASE-AMOUNT-ND(HR11F1-I1).

           MOVE HRHEU-BASE-CURRENCY    TO
                                   HR11F1-HEU-BASE-CURRENCY (HR11F1-I1).
           MOVE HRHEU-CURR-ND         TO HR11F1-HEU-CURR-ND (HR11F1-I1).
           MOVE HRHEU-BASE-ND         TO HR11F1-HEU-BASE-ND (HR11F1-I1).

       530-END.

088600******************************************************************
       540-MOVE-TO-AND-VALUE.
088600******************************************************************

           MOVE HR11F1-HRU-FLD-NBR (HR11F1-I1)     TO HR11WS-FLD-NBR-G.
           MOVE HR11WS-FIELD-KEY            TO DB-FIELD-KEY.
           PERFORM 840-FIND-HRUSET1.

           INITIALIZE HR11WS-A-VALUE
                      HR11WS-N-VALUE
                      HR11WS-D-VALUE.

           IF (HR11F1-VALUE (HR11F1-I1)            NOT = SPACES)
               IF (HRU-FIELD-TYPE           = "A")
                   MOVE HR11F1-VALUE(HR11F1-I1)    TO HRWS-UP-FIELD
                   PERFORM 760-HR-UPPER-CASE
                   MOVE HRWS-UP-FIELD       TO HR11WS-A-VALUE
               ELSE
               IF (HRU-FIELD-TYPE           = "N")
                   MOVE HR11F1-HRU-FLD-NBR (HR11F1-I1) TO DB-FLD-NBR
                   PERFORM 840-FIND-PADSET1
                   MOVE HR11F1-VALUE (HR11F1-I1)   TO HRWS-VALUE
                   MOVE PAD-SIZE            TO HRWS-SIZE
                   MOVE PAD-DECIMALS        TO HRWS-NBR-DECIMALS
                   PERFORM 770-HR-EDIT-DEC-FIELD
                   MOVE HRWS-DEC-FIELD      TO HR11WS-N-VALUE
               ELSE
               IF (HRU-FIELD-TYPE           = "D")
                   MOVE HR11F1-VALUE (HR11F1-I1)   TO HRWS-VALUE
                   PERFORM 780-HR-REFORMAT-DATE-FIELD
                   MOVE HRWS-DATE-8-FIELD   TO HR11WS-D-VALUE.

       540-END.

044900******************************************************************
045000 600-MOVE-WS-TO-SCR.
045100******************************************************************
045200
091000     IF (HR11F1-FC = "I" OR "N" OR "P")
091100         MOVE ZEROES             TO HR11F1-EFFECT-DATE.

142200     MOVE HREMP-COMPANY               TO DB-COMPANY.
142300     MOVE SPACES                      TO DB-PROCESS-LEVEL.
142400     PERFORM 840-FIND-PRSSET1.
142500     MOVE PRS-NAME                    TO HR11F1-PRS-NAME.
142900
J19305     MOVE 1                      TO HR11F1-POS-LEVEL.
           MOVE HREMP-EMPLOYEE         TO HR11F1-EMP-EMPLOYEE.
           MOVE HREMP-FULL-NAME        TO HR11F1-EMP-FULL-NAME-CND.
           MOVE HREMP-LAST-NAME-PRE    TO HR11F1-EMP-LAST-NAME-PRE.
045300     MOVE HREMP-LAST-NAME        TO HR11F1-EMP-LAST-NAME.
045400     MOVE HREMP-FIRST-NAME       TO HR11F1-EMP-FIRST-NAME.
045500     MOVE HREMP-MIDDLE-NAME      TO HR11F1-EMP-MIDDLE-NAME.
045600     MOVE HREMP-NICK-NAME        TO HR11F1-EMP-NICK-NAME.
           MOVE HREMP-NAME-PREFIX      TO HR11F1-EMP-NAME-PREFIX.
           MOVE HREMP-NAME-SUFFIX      TO HR11F1-EMP-NAME-SUFFIX.
           MOVE HRPEM-LANGUAGE-CODE    TO HR11F1-PEM-LANGUAGE-CODE.
045700     MOVE HREMP-ADDR1            TO HR11F1-EMP-ADDR1.
045800     MOVE HREMP-ADDR2            TO HR11F1-EMP-ADDR2.
045900     MOVE HREMP-ADDR3            TO HR11F1-EMP-ADDR3.
046000     MOVE HREMP-ADDR4            TO HR11F1-EMP-ADDR4.
046100     MOVE HREMP-CITY             TO HR11F1-EMP-CITY.
046200     MOVE HREMP-STATE            TO HR11F1-EMP-STATE.
046300     MOVE HREMP-ZIP              TO HR11F1-EMP-ZIP.
           MOVE HREMP-COUNTY           TO HR11F1-EMP-COUNTY.
046400     MOVE HREMP-COUNTRY-CODE     TO HR11F1-EMP-COUNTRY-CODE.
046400     MOVE HREMP-WORK-COUNTRY     TO HR11F1-EMP-WORK-COUNTRY.
046500     MOVE HREMP-EMP-STATUS       TO HR11F1-EMP-EMP-STATUS.
046600     MOVE HREMP-FICA-NBR         TO HR11F1-EMP-FICA-NBR.
046700     MOVE HREMP-EIC-STATUS       TO HR11F1-EMP-EIC-STATUS.
           MOVE HREMP-RAILROAD-CODE    TO HR11F1-RAILROAD-CODE.
           MOVE HREMP-TAX-FILTER       TO HR11F1-EMP-TAX-FILTER.
           MOVE HREMP-REMOTE           TO HR11F1-EMP-REMOTE.
           MOVE HREMP-PUB-SEC-RETIRE   TO HR11F1-EMP-PUB-SEC-RETIRE.
           MOVE HREMP-TAX-CITY         TO HR11F1-EMP-TAX-CITY.
           MOVE HREMP-TAX-COUNTY       TO HR11F1-EMP-TAX-COUNTY.
           MOVE HREMP-TAX-SCHOOL       TO HR11F1-EMP-TAX-SCHOOL.
           MOVE HREMP-WORK-CITY        TO HR11F1-EMP-WORK-CITY.
           MOVE HREMP-WORK-COUNTY      TO HR11F1-EMP-WORK-COUNTY.
           MOVE HREMP-WORK-SCHOOL      TO HR11F1-EMP-WORK-SCHOOL.
046800     MOVE HREMP-PROCESS-LEVEL    TO HR11F1-EMP-PROCESS-LEVEL.
046900     MOVE HREMP-DEPARTMENT       TO HR11F1-EMP-DEPARTMENT.
047000     MOVE HREMP-USER-LEVEL       TO HR11F1-EMP-USER-LEVEL.
047100     MOVE HREMP-HM-DIST-CO       TO HR11F1-EMP-HM-DIST-CO.
047200     MOVE HREMP-HM-ACCT-UNIT     TO HR11F1-EMP-HM-ACCT-UNIT.
047300     MOVE HREMP-HM-ACCOUNT       TO HR11F1-EMP-HM-ACCOUNT.
047400     MOVE HREMP-HM-SUB-ACCT      TO HR11F1-EMP-HM-SUB-ACCT.
047500     MOVE HREMP-ACTIVITY         TO HR11F1-EMP-ACTIVITY.
047600     MOVE HREMP-ACCT-CATEGORY    TO HR11F1-EMP-ACCT-CATEGORY.
047700     MOVE HREMP-JOB-CODE         TO HR11F1-EMP-JOB-CODE.
047800     MOVE HREMP-UNION-CODE       TO HR11F1-EMP-UNION-CODE.
047900     MOVE HREMP-SUPERVISOR       TO HR11F1-EMP-SUPERVISOR.
048000     MOVE HREMP-SUPERVISOR-IND   TO HR11F1-EMP-SUPER-IND.
048100     MOVE HREMP-DATE-HIRED       TO HR11F1-EMP-DATE-HIRED.
048200     MOVE HREMP-PAY-FREQUENCY    TO HR11F1-EMP-PAY-FREQUENCY.
048300     MOVE HREMP-SHIFT            TO HR11F1-EMP-SHIFT.
048400     MOVE HREMP-SALARY-CLASS     TO HR11F1-EMP-SALARY-CLASS.
048500     MOVE HREMP-EXEMPT-EMP       TO HR11F1-EMP-EXEMPT-EMP.
048600     MOVE HREMP-PAY-RATE         TO HR11F1-EMP-PAY-RATE.
           MOVE HREMP-CURRENCY-CODE    TO HR11F1-EMP-CURR-CODE
                                          HR11F1-EMP-CURR-CODE-2
                                          HR11F1-EMP-CURR-CODE-3
                                          HR11F1-EMP-CURR-CODE-4.
048700     MOVE HREMP-STAND-HOURS      TO HR11F1-EMP-STAND-HOURS.
048800     MOVE HREMP-STAND-AMT        TO HR11F1-EMP-STAND-AMT.
048900     MOVE HREMP-WARN-FLAG        TO HR11F1-EMP-WARN-FLAG.
049000     MOVE HREMP-ADD-ALLOW-PER    TO HR11F1-EMP-ADD-ALLOW-PER.
049100     MOVE HREMP-ADD-ALLOW-HRS    TO HR11F1-EMP-ADD-ALLOW-HRS.
049200     MOVE HREMP-ADD-ALLOW-AMT    TO HR11F1-EMP-ADD-ALLOW-AMT.

           IF (HR11F1-FC NOT = "A")
               MOVE ZEROES             TO HR11F1-DFT-MAR-STAT
                                          HR11F1-DFT-EXEMPTS.

049300     MOVE HREMP-RPT-INS-COST     TO HR11F1-EMP-RPT-INS-COST.
049400     MOVE HREMP-AUTO-TIME-REC    TO HR11F1-EMP-AUTO-TIME-REC.
049500     MOVE HREMP-AUTO-DEPOSIT     TO HR11F1-EMP-AUTO-DEPOSIT.
049700     MOVE HREMP-ADJ-HIRE-DATE    TO HR11F1-EMP-ADJ-HIRE-DATE.
049800     MOVE HREMP-ANNIVERS-DATE    TO HR11F1-EMP-ANNIVERS-DATE.
           MOVE HREMP-FST-DAY-WORKED   TO HR11F1-EMP-FST-DAY-WORKED.
           MOVE HREMP-LAST-DAY-PAID    TO HR11F1-EMP-LAST-DAY-PAID.
049900     MOVE HREMP-TERM-DATE        TO HR11F1-EMP-TERM-DATE.
050000     MOVE HREMP-SEC-LVL          TO HR11F1-EMP-SEC-LVL.
050100     MOVE HREMP-SEC-LOCATION     TO HR11F1-EMP-SEC-LOCATION.
050200     MOVE HREMP-NBR-FTE          TO HR11F1-EMP-NBR-FTE.
050300     MOVE HREMP-ANNUAL-HOURS     TO HR11F1-EMP-ANNUAL-HOURS.
050400     MOVE HREMP-PENSION-PLAN     TO HR11F1-EMP-PENSION-PLAN.
050500     MOVE HREMP-PAY-STEP         TO HR11F1-EMP-PAY-STEP.
050600     MOVE HREMP-PAY-GRADE        TO HR11F1-EMP-PAY-GRADE.
050700     MOVE HREMP-SCHEDULE         TO HR11F1-EMP-SCHEDULE.
050800     MOVE HREMP-WORK-SCHED       TO HR11F1-EMP-WORK-SCHED.
050900     MOVE HREMP-EBE-AMOUNT       TO HR11F1-EMP-EBE-AMOUNT.
051000     MOVE HREMP-SICK-PAY         TO HR11F1-EMP-SICK-PAY.
051100     MOVE HREMP-MOVING-EXP       TO HR11F1-EMP-MOVING-EXP.
051400     MOVE HREMP-WC-STATE         TO HR11F1-EMP-WC-STATE.
051500     MOVE HREMP-OT-PLAN-CODE     TO HR11F1-EMP-OT-PLAN-CODE.
051600     MOVE HREMP-TIPPED           TO HR11F1-EMP-TIPPED.
051700     MOVE HREMP-DECEASED         TO HR11F1-EMP-DECEASED.
           MOVE HREMP-DEATH-DATE       TO HR11F1-EMP-DEATH-DATE.
           MOVE HRPEM-ESTAB-PATIENT    TO HR11F1-PEM-ESTAB-PATIENT.
           MOVE HRPEM-PRIOR-COV-MO     TO HR11F1-PEM-PRIOR-COV-MO.
051800     MOVE HREMP-BSI-GROUP        TO HR11F1-EMP-BSI-GROUP.
           MOVE HREMP-TAX-PROVINCE     TO HR11F1-EMP-TAX-PROVINCE.
           MOVE HREMP-BUS-NBR-GRP      TO HR11F1-EMP-BUS-NBR-GRP.
           MOVE HREMP-QC-ENT-NBR-GRP   TO HR11F1-EMP-QC-ENT-NBR-GRP.
           MOVE HREMP-WC-PROVINCE      TO HR11F1-EMP-WC-PROVINCE.

052100     MOVE HREMP-EMAIL-ADDRESS    TO HR11F1-EMP-EMAIL-ADDRESS.
052200     MOVE HREMP-MAX-LIMIT-OVRD   TO HR11F1-EMP-MAX-LIMIT-OVRD.
052300     MOVE HREMP-POSITION         TO HR11F1-EMP-POSITION.
052400     MOVE HRPEM-LOCAT-CODE       TO HR11F1-PEM-LOCAT-CODE.
052500     MOVE HRPEM-BARGAIN-UNIT     TO HR11F1-PEM-BARGAIN-UNIT.
052600     MOVE HRPEM-SENIOR-DATE      TO HR11F1-PEM-SENIOR-DATE.
052700     MOVE HRPEM-TRUE-MAR-STAT    TO HR11F1-PEM-TRUE-MAR-STAT.
           MOVE HREMP-NEW-HIRE-DATE    TO HR11F1-EMP-NEW-HIRE-DATE.
052800     MOVE HRPEM-WK-PHONE-CNTRY   TO HR11F1-PEM-WK-PHONE-CNTRY.
052900     MOVE HRPEM-WK-PHONE-NBR     TO HR11F1-PEM-WK-PHONE-NBR.
053000     MOVE HRPEM-WK-PHONE-EXT     TO HR11F1-PEM-WK-PHONE-EXT.
053100     MOVE HRPEM-SECURITY-CODE    TO HR11F1-PEM-SECURITY-CODE.
053200     MOVE HRPEM-SECURITY-NBR     TO HR11F1-PEM-SECURITY-NBR.
053300     MOVE HRPEM-MAIL-GROUP       TO HR11F1-PEM-MAIL-GROUP.
053400     MOVE HRPEM-MB-NBR           TO HR11F1-PEM-MB-NBR.
053500     MOVE HRPEM-CLOCK-NBR        TO HR11F1-PEM-CLOCK-NBR.
053600     MOVE HRPEM-HIRE-SOURCE      TO HR11F1-PEM-HIRE-SOURCE.
053700     MOVE HRPEM-COMP-CODE        TO HR11F1-PEM-COMP-CODE.
053800     MOVE HRPEM-COMP-NBR         TO HR11F1-PEM-COMP-NBR.
053900     MOVE HRPEM-BIRTHDATE        TO HR11F1-PEM-BIRTHDATE.
P54634     IF  (HRPEM-BIRTHDATE  NOT = ZEROES)
P54634     AND (HREMP-DEATH-DATE     = ZEROES)
               MOVE HRPEM-BIRTHDATE        TO WSDR-FR-DATE
               PERFORM 5000-CALCULATE-AGE
P54634         MOVE WSDR-NBR-DAYS      TO HR11F1-EMP-CUR-AGE
P54634     ELSE
P54634         IF  (HRPEM-BIRTHDATE  NOT = ZEROES)
P54634         AND (HREMP-DECEASED       = "Y")
P54634         AND (HREMP-DEATH-DATE NOT = ZEROES)
P54634         AND (HREMP-DEATH-DATE     > HRPEM-BIRTHDATE)
P54634             MOVE HRPEM-BIRTHDATE    TO WSDR-FR-DATE
P54634             MOVE WS-SYSTEM-DATE-YMD TO HR11WS-SYSTEM-DATE-YMD
P54634             MOVE HREMP-DEATH-DATE   TO WS-SYSTEM-DATE-YMD
P54634             PERFORM 5000-CALCULATE-AGE
P54634             MOVE WSDR-NBR-DAYS      TO HR11F1-EMP-CUR-AGE
P54634             MOVE HR11WS-SYSTEM-DATE-YMD  
P54634                                     TO WS-SYSTEM-DATE-YMD
P54634         ELSE
P54634             INITIALIZE                 HR11F1-EMP-CUR-AGE
P54634         END-IF
P54634     END-IF.
054000     MOVE HRPEM-SEX              TO HR11F1-PEM-SEX.
J73217     MOVE HRPEM-SEXUAL-ORIENTATION
J73217                                 TO HR11F1-PEM-SEX-ORIENTATION.
J73217     MOVE HRPEM-GENDER-IDENTITY  TO HR11F1-PEM-GENDER-IDENTITY.
054100     MOVE HRPEM-EEO-CLASS        TO HR11F1-PEM-EEO-CLASS.
054200     MOVE HRPEM-HANDICAP-ID      TO HR11F1-PEM-HANDICAP-ID.
054300     MOVE HRPEM-VETERAN          TO HR11F1-PEM-VETERAN.
054300     MOVE HRPEM-CONSENT          TO HR11F1-PEM-CONSENT.
           MOVE HRPEM-VISIBLE-MIN      TO HR11F1-PEM-VISIBLE-MIN.
           MOVE HRPEM-ABORIGINAL       TO HR11F1-PEM-ABORIGINAL.
054400     MOVE HRPEM-BIRTH-CITY       TO HR11F1-PEM-BIRTH-CITY.
           MOVE HRPEM-DISABILITY       TO HR11F1-PEM-DISABILITY.
           MOVE HRPEM-RELIGION         TO HR11F1-PEM-RELIGION.
054500     MOVE HRPEM-BIRTH-STATE      TO HR11F1-PEM-BIRTH-STATE.
054600     MOVE HRPEM-BIRTH-CNTRY-CD   TO HR11F1-PEM-BIRTH-CNTRY-CD.
054700     MOVE HRPEM-MAIDEN-LST-NM    TO HR11F1-PEM-MAIDEN-LST-NM.
054800     MOVE HRPEM-MAIDEN-FST-NM    TO HR11F1-PEM-MAIDEN-FST-NM.
054900     MOVE HRPEM-MAIDEN-MI        TO HR11F1-PEM-MAIDEN-MI.
055000     MOVE HRPEM-FORMER-LST-NM    TO HR11F1-PEM-FORMER-LST-NM.
055100     MOVE HRPEM-FORMER-FST-NM    TO HR11F1-PEM-FORMER-FST-NM.
055200     MOVE HRPEM-FORMER-MI        TO HR11F1-PEM-FORMER-MI.
055300     MOVE HRPEM-FNCTN-GROUP      TO HR11F1-PEM-FNCTN-GROUP.
055400     MOVE HRPEM-EXCLUDE-FLAG     TO HR11F1-PEM-EXCLUDE-FLAG.
055500     MOVE HRPEM-I9-STATUS        TO HR11F1-PEM-I9-STATUS.
055600     MOVE HRPEM-I9-ALIEN-NBR     TO HR11F1-PEM-I9-ALIEN-NBR.
055700     MOVE HRPEM-I9-ADMIT-NBR     TO HR11F1-PEM-I9-ADMIT-NBR.
055800     MOVE HRPEM-I9-STA-EXP-DT    TO HR11F1-PEM-I9-STA-EXP-DT.
055900     MOVE HRPEM-I9-DOC-NBR1      TO HR11F1-PEM-I9-DOC-NBR1.
056000     MOVE HRPEM-I9-DOC-DESCR1    TO HR11F1-PEM-I9-DOC-DESCR1.
056100     MOVE HRPEM-I9-DOC-TYPE1     TO HR11F1-PEM-I9-DOC-TYPE1.
056200     MOVE HRPEM-I9-DOC-EXP-DT1   TO HR11F1-PEM-I9-DOC-EXP-DT1.
056300     MOVE HRPEM-I9-DOC-NBR2      TO HR11F1-PEM-I9-DOC-NBR2.
056400     MOVE HRPEM-I9-DOC-DESCR2    TO HR11F1-PEM-I9-DOC-DESCR2.
056500     MOVE HRPEM-I9-DOC-TYPE2     TO HR11F1-PEM-I9-DOC-TYPE2.
056600     MOVE HRPEM-I9-DOC-EXP-DT2   TO HR11F1-PEM-I9-DOC-EXP-DT2.
056700     MOVE HRPEM-I9-AUTHORIZE     TO HR11F1-PEM-I9-AUTHORIZE.
056800     MOVE HRPEM-OWNER-FLAG       TO HR11F1-PEM-OWNER-FLAG.
056900     MOVE HRPEM-OFFICER          TO HR11F1-PEM-OFFICER.
057000     MOVE HRPEM-HIGH-COMP        TO HR11F1-PEM-HIGH-COMP.
057100     MOVE HRPEM-KEY-EMP-FLAG     TO HR11F1-PEM-KEY-EMP-FLAG.
057200     MOVE HRPEM-SMOKER           TO HR11F1-PEM-SMOKER.
057300     MOVE HRPEM-PRIMARY-CARE     TO HR11F1-PEM-PRIMARY-CARE.
HIPAA      MOVE HRPEM-MEDICARE-IND     TO HR11F1-PEM-MEDICARE-IND.
057400     MOVE HRPEM-FAMILY-AGG       TO HR11F1-PEM-FAMILY-AGG.
           MOVE HRPEM-STOCK-TRADE      TO HR11F1-PEM-STOCK-TRADE.
057500     MOVE HRPEM-NBR-HL-DEP       TO HR11F1-PEM-NBR-HL-DEP.
057600     MOVE HRPEM-NBR-DN-DEP       TO HR11F1-PEM-NBR-DN-DEP.
J12776     MOVE HREMP-EMP-HICN         TO HR11F1-EMP-HICN.
           MOVE HRPEM-RELATED-EMP      TO HR11F1-PEM-RELATED-EMP.
057700     MOVE HRPEM-HL-COV-PROOF     TO HR11F1-PEM-HL-COV-PROOF.
057800     MOVE HRPEM-HL-VERIFY-DT     TO HR11F1-PEM-HL-VERIFY-DT.
057900     MOVE HRPEM-DN-COV-PROOF     TO HR11F1-PEM-DN-COV-PROOF.
058000     MOVE HRPEM-DN-VERIFY-DT     TO HR11F1-PEM-DN-VERIFY-DT.
058100     MOVE HRPEM-SPOUSE-EMP       TO HR11F1-PEM-SPOUSE-EMP.
058200     MOVE HRPEM-SP-EMP-ADDR1     TO HR11F1-PEM-SP-EMP-ADDR1.
058300     MOVE HRPEM-SP-EMP-ADDR2     TO HR11F1-PEM-SP-EMP-ADDR2.
058400     MOVE HRPEM-SP-EMP-ADDR3     TO HR11F1-PEM-SP-EMP-ADDR3.
058500     MOVE HRPEM-SP-EMP-ADDR4     TO HR11F1-PEM-SP-EMP-ADDR4.
058600     MOVE HRPEM-SP-EMP-CITY      TO HR11F1-PEM-SP-EMP-CITY.
058700     MOVE HRPEM-SP-EMP-STATE     TO HR11F1-PEM-SP-EMP-STATE.
058800     MOVE HRPEM-SP-EMP-ZIP       TO HR11F1-PEM-SP-EMP-ZIP.
           MOVE HRPEM-SP-EMP-COUNTRY   TO HR11F1-PEM-SP-EMP-COUNTRY.
058900     MOVE HRPEM-SP-EMP-PH-CNTR   TO HR11F1-PEM-SP-EMP-PH-CNTR.
059000     MOVE HRPEM-SP-EMP-PH-NBR    TO HR11F1-PEM-SP-EMP-PH-NBR.
059100     MOVE HRPEM-HM-PHONE-CNTRY   TO HR11F1-PEM-HM-PHONE-CNTRY.
059200     MOVE HRPEM-HM-PHONE-NBR     TO HR11F1-PEM-HM-PHONE-NBR.
059300     MOVE HRPEM-SUPP-ADDR1       TO HR11F1-PEM-SUPP-ADDR1.
059400     MOVE HRPEM-SUPP-ADDR2       TO HR11F1-PEM-SUPP-ADDR2.
059500     MOVE HRPEM-SUPP-ADDR3       TO HR11F1-PEM-SUPP-ADDR3.
059600     MOVE HRPEM-SUPP-ADDR4       TO HR11F1-PEM-SUPP-ADDR4.
059700     MOVE HRPEM-SUPP-CITY        TO HR11F1-PEM-SUPP-CITY.
059800     MOVE HRPEM-SUPP-STATE       TO HR11F1-PEM-SUPP-STATE.
059900     MOVE HRPEM-SUPP-ZIP         TO HR11F1-PEM-SUPP-ZIP.
           MOVE HRPEM-SUPP-COUNTY      TO HR11F1-PEM-SUPP-COUNTY.
060000     MOVE HRPEM-SUPP-CNTRY-CD    TO HR11F1-PEM-SUPP-CNTRY-CD.
           MOVE HRPEM-SUPP-PHONE-CNT   TO HR11F1-PEM-SUPP-PHONE-CNT.
           MOVE HRPEM-SUPP-PHONE-NBR   TO HR11F1-PEM-SUPP-PHONE-NBR.
J81460     MOVE HREMP-EMAIL-PERSONAL   TO HR11F1-EMP-EMAIL-PERSONAL.    
060100     MOVE HRPEM-BEN-SALARY-1     TO HR11F1-PEM-BEN-SALARY-1.
060200     MOVE HRPEM-BEN-SALARY-2     TO HR11F1-PEM-BEN-SALARY-2.
060300     MOVE HRPEM-BEN-SALARY-3     TO HR11F1-PEM-BEN-SALARY-3.
060400     MOVE HRPEM-BEN-SALARY-4     TO HR11F1-PEM-BEN-SALARY-4.
060500     MOVE HRPEM-BEN-SALARY-5     TO HR11F1-PEM-BEN-SALARY-5.
060600     MOVE HRPEM-BEN-DATE-1       TO HR11F1-PEM-BEN-DATE-1.
060700     MOVE HRPEM-BEN-DATE-2       TO HR11F1-PEM-BEN-DATE-2.
060800     MOVE HRPEM-BEN-DATE-3       TO HR11F1-PEM-BEN-DATE-3.
060900     MOVE HRPEM-BEN-DATE-4       TO HR11F1-PEM-BEN-DATE-4.
061000     MOVE HRPEM-BEN-DATE-5       TO HR11F1-PEM-BEN-DATE-5.
061100     MOVE HRPEM-WORK-ZIP         TO HR11F1-PEM-WORK-ZIP.
061200     MOVE HRPEM-LIFE-STYLE-CR    TO HR11F1-PEM-LIFE-STYLE-CR.
           MOVE HREMP-LAB-DIST-FLAG    TO HR11F1-EMP-LAB-DIST-FLAG.
           MOVE HREMP-ENCUMBER-FLAG    TO HR11F1-EMP-ENCUMBER-FLAG.
           MOVE HREMP-FRNG-RATE        TO HR11F1-EMP-FRNG-RATE.
           MOVE HREMP-FRNG-ACCT-CAT    TO HR11F1-EMP-FRNG-ACCT-CAT.
           MOVE HREMP-FRNG-ACCOUNT     TO HR11F1-EMP-FRNG-ACCOUNT.
           MOVE HREMP-FRNG-SUB-ACCT    TO HR11F1-EMP-FRNG-SUB-ACCT.
           MOVE HREMP-EFFORT-FLAG      TO HR11F1-EMP-EFFORT-FLAG.
           MOVE HREMP-RPT-CURR         TO HR11F1-EMP-RPT-CURR.
           MOVE HREMP-PRMCERT-COMP     TO HR11F1-EMP-PRMCERT-COMP.
           MOVE HREMP-PRMCERT-ID       TO HR11F1-EMP-PRMCERT-ID.
           MOVE HREMP-SNDCERT-COMP     TO HR11F1-EMP-SNDCERT-COMP.
           MOVE HREMP-SNDCERT-ID       TO HR11F1-EMP-SNDCERT-ID.
061300
           MOVE HREMP-XMIT-BLOCK       TO HR11F1-XMIT-HREMP-BLOCK.
061600
           MOVE HREMP-FTE-TOTAL        TO HR11F1-EMP-FTE-TOTAL.

           IF  (HREMP-CURR-ND > HR11F1-PEM-BEN-SALARY-1-ND)
               MOVE HREMP-CURR-ND      TO HR11F1-PEM-BEN-SALARY-1-ND.
           IF  (HREMP-CURR-ND > HR11F1-PEM-BEN-SALARY-2-ND)
               MOVE HREMP-CURR-ND      TO HR11F1-PEM-BEN-SALARY-2-ND.
           IF  (HREMP-CURR-ND > HR11F1-PEM-BEN-SALARY-3-ND)
               MOVE HREMP-CURR-ND      TO HR11F1-PEM-BEN-SALARY-3-ND.
           IF  (HREMP-CURR-ND > HR11F1-PEM-BEN-SALARY-4-ND)
               MOVE HREMP-CURR-ND      TO HR11F1-PEM-BEN-SALARY-4-ND.
           IF  (HREMP-CURR-ND > HR11F1-PEM-BEN-SALARY-5-ND)
               MOVE HREMP-CURR-ND      TO HR11F1-PEM-BEN-SALARY-5-ND.
           IF  (HREMP-CURR-ND > HR11F1-EMP-STAND-AMT-ND)
               MOVE HREMP-CURR-ND      TO HR11F1-EMP-STAND-AMT-ND.

           INITIALIZE                          HR11F1-COMMENTS-FLAG.
           MOVE HREMP-COMPANY               TO DB-COMPANY.
           INITIALIZE                          DB-EMP-APP.
           MOVE "GE"                        TO DB-CMT-TYPE.
           MOVE HREMP-EMPLOYEE              TO DB-EMPLOYEE.
           INITIALIZE                          DB-JOB-CODE
                                               DB-ACTION-CODE
                                               DB-LN-NBR
                                               DB-SEQ-NBR.
           PERFORM 850-FIND-NLT-PACSET1.
           IF  (PACOMMENTS-FOUND)
           AND (PAC-COMPANY  = DB-COMPANY)
           AND (PAC-EMP-APP  = DB-EMP-APP)
           AND (PAC-CMT-TYPE = DB-CMT-TYPE)
           AND (PAC-EMPLOYEE = HREMP-EMPLOYEE)
               MOVE "*"                     TO HR11F1-COMMENTS-FLAG.

           INITIALIZE                          HR11F1-POSJOB-FLAG.
           MOVE HREMP-COMPANY               TO DB-COMPANY.
           MOVE HREMP-EMPLOYEE              TO DB-EMPLOYEE.
           MOVE 2                           TO DB-POS-LEVEL.
           MOVE ZEROES                      TO DB-EFFECT-DATE.
           PERFORM 850-FIND-NLT-PEPSET2.
           IF  (PAEMPPOS-FOUND)
           AND (PEP-COMPANY   = DB-COMPANY)
           AND (PEP-EMPLOYEE  = DB-EMPLOYEE)
           AND (PEP-POS-LEVEL > 1)
               MOVE "*"                     TO HR11F1-POSJOB-FLAG.

           INITIALIZE                          HR11F1-ALT-RATES-FLAG.
           MOVE HREMP-COMPANY               TO DB-COMPANY.
           MOVE HREMP-EMPLOYEE              TO DB-EMPLOYEE.
           INITIALIZE                          DB-SEQ-NBR.
           PERFORM 850-FIND-NLT-PRRSET1.
           IF  (PRRATES-FOUND)
           AND (PRR-COMPANY   = DB-COMPANY)
           AND (PRR-EMPLOYEE  = DB-EMPLOYEE)
               MOVE "*"                     TO HR11F1-ALT-RATES-FLAG.

           INITIALIZE                          HR11F1-GRANT-FLAG.
           IF (HR11F1-EMP-LAB-DIST-FLAG = "Y")
           OR (HR11F1-EMP-ENCUMBER-FLAG = "Y")
           OR (HR11F1-EMP-EFFORT-FLAG   = "Y")
               MOVE "*"                     TO HR11F1-GRANT-FLAG.

           IF  (ERROR-FOUND)
               GO TO 600-END.

           INITIALIZE                     HR11F1-COMP-ANALYSIS
                                          HR11F1-SERVICE
J19381                                    HR11F1-ADDITIONAL-DETAIL
                                          HR11F1-POSITIONS
                                          HR11F1-NAME-SUFFIX-DESC
                                          HR11F1-NAME-PREFIX-DESC
                                          HR11F1-EMS-DESCRIPTION
                                          HR11F1-PRS1-NAME
                                          HR11F1-DPT-NAME
                                          HR11F1-EMP-COUNTRY-DESC
                                          HR11F1-WORK-COUNTRY-DESC
                                          HR11F1-PCO-DESCRIPTION
                                          HR11F1-POS-DESCRIPTION
                                          HR11F1-JBC-DESCRIPTION
                                          HR11F1-SUP-NAME
                                          HR11F1-IND-SUP-NAME
                                          HR11F1-PCO1-DESCRIPTION
                                          HR11F1-UNION-CODE-DESC
                                          HR11F1-PCO3-DESCRIPTION
                                          HR11F1-WORK-SCHED-DESC
                                          HR11F1-CURR-CODE-DESC
                                          HR11F1-SCHEDULE-DESC
                                          HR11F1-PRO-DESCRIPTION
                                          HR11F1-GLM-DESCRIPTION
                                          HR11F1-ACV-DESCRIPTION
                                          HR11F1-PCO4-DESCRIPTION
                                          HR11F1-PCO5-DESCRIPTION
                                          HR11F1-PCO6-DESCRIPTION
                                          HR11F1-BIRTH-COUNTRY-DESC
                                          HR11F1-CTC-DESCRIPTION
                                          HR11F1-DISABILITY-DESC
                                          HR11F1-RELIGION-DESC
                                          HR11F1-SUPP-COUNTRY-DESC
                                          HR11F1-PCO-WE-DESC
                                          HR11F1-PSA2-DESCRIPTION
                                          HR11F1-PPV-DESCRIPTION
                                          HR11F1-PPV-WC-DESCRIPTION
                                          HR11F1-PBG-BUS-NBR
                                          HR11F1-PQC-QC-ENT-NBR
                                          HR11F1-CURR-CODE-2-DESC
                                          HR11F1-PCO7-DESCRIPTION
                                          HR11F1-SP-COUNTRY-DESC
                                          HR11F1-PCO-DT-DESC1
                                          HR11F1-PCO-DO-DESC1
                                          HR11F1-PCO-DT-DESC2
                                          HR11F1-PCO-DO-DESC2
                                          HR11F1-FAMILY-AGG-NAME
                                          HR11F1-PRIMARY-NAME
                                          HR11F1-SECONDARY-NAME 
                                          HR11F1-CTC-VETERAN-DESC.

      * EMP-PROCESS-LEVEL is used for lookups, in case it's secured
      * and other fields aren't

           IF (HR11F1-EMP-TAX-PROVINCE NOT = SPACES)
               MOVE HR11F1-EMP-TAX-PROVINCE    TO DB-PROVINCE
               PERFORM 840-FIND-PPVSET1
               IF (PRPROVINCE-FOUND)
                   MOVE PPV-DESCRIPTION TO HR11F1-PPV-DESCRIPTION.

           IF (HR11F1-EMP-BUS-NBR-GRP NOT = SPACES)
               MOVE HR11F1-EMP-BUS-NBR-GRP TO DB-BUS-NBR-GRP
               MOVE EMP-PROCESS-LEVEL      TO DB-PROCESS-LEVEL
               PERFORM 840-FIND-PBGSET1
               IF (PRBUSGRP-FOUND)
                   MOVE PBG-BUS-NBR    TO HR11F1-PBG-BUS-NBR.

           IF (HR11F1-EMP-QC-ENT-NBR-GRP NOT = SPACES)
               MOVE HR11F1-EMP-QC-ENT-NBR-GRP  TO DB-QC-ENT-NBR-GRP
               MOVE EMP-PROCESS-LEVEL          TO DB-PROCESS-LEVEL
               PERFORM 840-FIND-PQCSET1
               IF (PRQCENTGRP-FOUND)
                   MOVE PQC-QC-ENT-NBR TO HR11F1-PQC-QC-ENT-NBR.

           IF (HR11F1-EMP-WC-PROVINCE NOT = SPACES)
               MOVE HR11F1-EMP-WC-PROVINCE TO DB-PROVINCE
               PERFORM 840-FIND-PPVSET1
               IF (PRPROVINCE-FOUND)
                   MOVE PPV-DESCRIPTION TO HR11F1-PPV-WC-DESCRIPTION.

           IF (HR11F1-EMP-NAME-SUFFIX NOT = SPACES)
               MOVE "SU"                    TO DB-TYPE
               MOVE HR11F1-EMP-NAME-SUFFIX  TO DB-HRCTRY-CODE
               INITIALIZE                      DB-COUNTRY-CODE
               PERFORM 850-FIND-NLT-CTCSET2
               IF  (HRCTRYCODE-FOUND)
               AND (CTC-TYPE        = DB-TYPE)
               AND (CTC-HRCTRY-CODE = DB-HRCTRY-CODE)
                   MOVE CTC-DESCRIPTION     TO HR11F1-NAME-SUFFIX-DESC.

           IF (HR11F1-PEM-VETERAN     NOT = SPACES)
               MOVE "VS"                      TO DB-TYPE
               MOVE HR11F1-PEM-VETERAN        TO DB-HRCTRY-CODE
               MOVE HR11F1-EMP-WORK-COUNTRY   TO DB-COUNTRY-CODE
               PERFORM 850-FIND-NLT-CTCSET2
               IF  (HRCTRYCODE-FOUND)
               AND (CTC-TYPE        = DB-TYPE)
               AND (CTC-HRCTRY-CODE = DB-HRCTRY-CODE)
                   MOVE CTC-DESCRIPTION     TO HR11F1-CTC-VETERAN-DESC.

           IF (HR11F1-EMP-NAME-PREFIX NOT = SPACES)
               MOVE "PR"                    TO DB-TYPE
               MOVE HR11F1-EMP-NAME-PREFIX  TO DB-HRCTRY-CODE
               INITIALIZE                      DB-COUNTRY-CODE
               PERFORM 850-FIND-NLT-CTCSET2
               IF  (HRCTRYCODE-FOUND)
               AND (CTC-TYPE        = DB-TYPE)
               AND (CTC-HRCTRY-CODE = DB-HRCTRY-CODE)
                   MOVE CTC-DESCRIPTION     TO HR11F1-NAME-PREFIX-DESC.

143000     IF (HR11F1-EMP-EMP-STATUS NOT = SPACES)
143100         MOVE HR11F1-EMP-EMP-STATUS   TO DB-EMP-STATUS
143200         PERFORM 840-FIND-EMSSET1
               IF  (EMSTATUS-FOUND)
143300             MOVE EMS-DESCRIPTION     TO HR11F1-EMS-DESCRIPTION.
143400
143500     IF (HR11F1-EMP-PROCESS-LEVEL NOT = SPACES)
143600         MOVE HR11F1-EMP-PROCESS-LEVEL   TO DB-PROCESS-LEVEL
143700         PERFORM 840-FIND-PRSSET1
               IF  (PRSYSTEM-FOUND)
143800             MOVE PRS-NAME            TO HR11F1-PRS1-NAME.
143900
144000     IF (HR11F1-EMP-DEPARTMENT NOT = SPACES)
144100         MOVE EMP-PROCESS-LEVEL       TO DB-PROCESS-LEVEL
144200         MOVE HR11F1-EMP-DEPARTMENT   TO DB-DEPARTMENT
144300         PERFORM 840-FIND-DPTSET1
               IF  (DEPTCODE-FOUND)
144400             MOVE DPT-NAME            TO HR11F1-DPT-NAME.
144500
144600     IF (HR11F1-EMP-USER-LEVEL NOT = SPACES)
144700         MOVE "UL"                    TO DB-TYPE
144800         MOVE HR11F1-EMP-USER-LEVEL   TO DB-CODE
144900         PERFORM 840-FIND-PCOSET1
               IF  (PCODES-FOUND)
145000             MOVE PCO-DESCRIPTION     TO HR11F1-PCO-DESCRIPTION.
145100
144600     IF (HR11F1-PEM-RELIGION   NOT = SPACES)
144700         MOVE "RE"                    TO DB-TYPE
144800         MOVE HR11F1-PEM-RELIGION     TO DB-CODE
144900         PERFORM 840-FIND-PCOSET1
               IF  (PCODES-FOUND)
145000             MOVE PCO-DESCRIPTION     TO HR11F1-RELIGION-DESC.
145100
144600     IF (HR11F1-PEM-DISABILITY NOT = SPACES)
144700         MOVE "DI"                    TO DB-TYPE
144800         MOVE HR11F1-PEM-DISABILITY   TO DB-CODE
144900         PERFORM 840-FIND-PCOSET1
               IF  (PCODES-FOUND)
145000             MOVE PCO-DESCRIPTION     TO HR11F1-DISABILITY-DESC.
145100
145200     IF (HR11F1-EMP-JOB-CODE NOT = SPACES)
145300         MOVE HR11F1-EMP-JOB-CODE     TO DB-JOB-CODE
145400         PERFORM 840-FIND-JBCSET1
               IF  (JOBCODE-FOUND)
145500             MOVE JBC-DESCRIPTION     TO HR11F1-JBC-DESCRIPTION.
145800
145900     IF (HR11F1-EMP-SUPERVISOR NOT = SPACES)
146000         MOVE HR11F1-EMP-SUPERVISOR   TO DB-CODE
146100         PERFORM 840-FIND-HSUSET1
146200         IF  (HRSUPER-FOUND)
146300         AND (HSU-EMPLOYEE NOT = ZEROS)
146400             MOVE HSU-EMPLOYEE        TO DB-EMPLOYEE
146500             MOVE PTFSET1-EMPLOYEE    TO WS-DB-BEG-RNG
146600             PERFORM 850-FIND-BEGRNG-PTFSET1
146700             MOVE PTF-LAST-NAME       TO HRWS-LAST-NAME
146800             MOVE PTF-FIRST-NAME      TO HRWS-FIRST-NAME
146900             MOVE PTF-MIDDLE-INIT     TO HRWS-MIDDLE-INIT
147000             PERFORM 750-HR-FORMAT-NAME
147100             MOVE HRWS-FORMAT-NAME    TO HR11F1-SUP-NAME.
147200
147300     IF (HR11F1-EMP-SUPER-IND NOT = SPACES)
147400         MOVE HR11F1-EMP-SUPER-IND    TO DB-CODE
147500         PERFORM 840-FIND-HSUSET1
147600         IF  (HRSUPER-FOUND)
147700         AND (HSU-EMPLOYEE NOT = ZEROS)
147800             MOVE HSU-EMPLOYEE        TO DB-EMPLOYEE
147900             MOVE PTFSET1-EMPLOYEE    TO WS-DB-BEG-RNG
148000             PERFORM 850-FIND-BEGRNG-PTFSET1
148100             MOVE PTF-LAST-NAME       TO HRWS-LAST-NAME
148200             MOVE PTF-FIRST-NAME      TO HRWS-FIRST-NAME
148300             MOVE PTF-MIDDLE-INIT     TO HRWS-MIDDLE-INIT
148400             PERFORM 750-HR-FORMAT-NAME
148500             MOVE HRWS-FORMAT-NAME    TO HR11F1-IND-SUP-NAME.
148600
148700     IF (HR11F1-PEM-LOCAT-CODE NOT = SPACES)
148800         MOVE "LO"                    TO DB-TYPE
148900         MOVE HR11F1-PEM-LOCAT-CODE   TO DB-CODE
149000         PERFORM 840-FIND-PCOSET1
               IF  (PCODES-FOUND)
149100             MOVE PCO-DESCRIPTION     TO HR11F1-PCO1-DESCRIPTION.
149200
           IF  (HR11F1-EMP-SCHEDULE      NOT = SPACES)
J56096     AND (HR11F1-EMP-PAY-GRADE     NOT = SPACES)
J56096         IF (HR11F1-EMP-PAY-STEP   NOT = ZEROES)
J56096             MOVE "S"                 TO DB-INDICATOR
J56096         ELSE
J56096             MOVE "G"                 TO DB-INDICATOR 
J56096         END-IF
               MOVE HR11F1-EMP-COMPANY      TO DB-COMPANY
               MOVE HR11F1-EMP-SCHEDULE     TO DB-SCHEDULE
               MOVE WS-SYSTEM-DATE-YMD      TO DB-EFFECT-DATE
               PERFORM 850-FIND-NLT-SGHSET2
               IF  (PRSAGHEAD-FOUND)
               AND (SGH-COMPANY   = DB-COMPANY)
               AND (SGH-INDICATOR = DB-INDICATOR)
               AND (SGH-SCHEDULE  = DB-SCHEDULE)
                   MOVE SGH-DESCRIPTION     TO HR11F1-SCHEDULE-DESC
J56096     END-IF.
150600
150700     IF  (HR11F1-EMP-POSITION      NOT = SPACES)
150800     AND (HR11F1-POS-DESCRIPTION   = SPACES)
150900         MOVE HR11F1-EMP-COMPANY      TO PAPOS-COMPANY
151000         MOVE HR11F1-EMP-POSITION     TO PAPOS-POSITION
151100         MOVE HR11F1-EFFECT-DATE      TO PAPOS-EFFECT-DATE
151200         IF (HR11F1-EMP-TERM-DATE = ZEROES)
151300             IF (HR11F1-EFFECT-DATE < WS-SYSTEM-DATE-YMD)
151400                 MOVE WS-SYSTEM-DATE-YMD TO PAPOS-END-DATE
151500             ELSE
151600                 MOVE HR11F1-EFFECT-DATE TO PAPOS-END-DATE
151700             END-IF
151800         ELSE
151900             MOVE HR11F1-EMP-TERM-DATE   TO PAPOS-END-DATE
                   IF (HR11F1-EFFECT-DATE > PAPOS-END-DATE)
                       MOVE HR11F1-EFFECT-DATE TO PAPOS-END-DATE
                   ELSE
                       IF  (HR11F1-EFFECT-DATE = ZEROES)
                       AND (HR11F1-FC          = "I" OR "N" OR "P")
                           MOVE HR11F1-EMP-TERM-DATE
                                               TO PAPOS-EFFECT-DATE
                       END-IF
                   END-IF
152000         END-IF
152100         MOVE 1                          TO ERROR-FLAG-SW
               INITIALIZE                         PAPOS-DESCRIPTION
152200         PERFORM 2000-EDIT-POSITION-DATES
152300         INITIALIZE                      CRT-ERROR-NBR
                                               CRT-ERROR-CAT
152400         MOVE PAPOS-DESCRIPTION       TO HR11F1-POS-DESCRIPTION.
152500
153200     MOVE HR11F1-EMP-COMPANY          TO DB-COMPANY.
J41902     MOVE SPACES                      TO DB-PROCESS-LEVEL.
J41902     PERFORM 840-FIND-PRSSET1.
J41902     IF (PRSYSTEM-FOUND)
J41902         MOVE PRS-PAYROLL-YEAR        TO DB-PAYROLL-YEAR
J41902     END-IF.
153300
153400     IF (HR11F1-EMP-OT-PLAN-CODE NOT = SPACES)
J41902         IF ( HREMP-SA-WORK-COUNTRY ) 
J41902         OR ( HREMP-AE-WORK-COUNTRY )
J41902         OR ( HREMP-QA-WORK-COUNTRY )
J41902             MOVE HR11F1-EMP-OT-PLAN-CODE
J41902                                      TO DB-PLAN-CODE
J41902             PERFORM 840-FIND-R1PSET1
J41902             IF (R1PAYPLAN-FOUND)
J41902                 MOVE R1P-DESCRIPTION TO HR11F1-PRO-DESCRIPTION
J41902             END-IF 
J41902         ELSE
153500             MOVE HR11F1-EMP-OT-PLAN-CODE TO DB-PLAN-CODE
153600             MOVE WS-SYSTEM-DATE-YMD      TO DB-EFFECT-DATE
153700             PERFORM 850-FIND-NLT-PROSET2
153800             IF  (PROVERTIME-FOUND)
153900             AND (PRO-COMPANY   = DB-COMPANY)
154000             AND (PRO-PLAN-CODE = DB-PLAN-CODE)
J41902                 MOVE PRO-DESCRIPTION    TO HR11F1-PRO-DESCRIPTION
J41902             END-IF
J41902         END-IF
J41902     END-IF.

154200
           IF (HR11F1-EMP-WORK-SCHED NOT = SPACES)
               MOVE HR11F1-EMP-WORK-SCHED  TO DB-WORK-SCHED
               MOVE ZEROES                 TO DB-CONTRACT-YEAR
               MOVE WS-SYSTEM-DATE-YMD     TO DB-EFFECT-DATE
               PERFORM 850-FIND-NLT-WSCSET1
               IF (HRWRKSCHD-NOTFOUND)
               OR (WSC-COMPANY    NOT = DB-COMPANY)
               OR (WSC-WORK-SCHED NOT = DB-WORK-SCHED)
                   MOVE 9999               TO DB-CONTRACT-YEAR
                   MOVE ZEROES             TO DB-EFFECT-DATE
                   PERFORM 850-FIND-NLT-WSCSET1
               END-IF
               IF (HRWRKSCHD-FOUND)
               OR (WSC-COMPANY    NOT = DB-COMPANY)
               OR (WSC-WORK-SCHED NOT = DB-WORK-SCHED)
                   MOVE WSC-DESCRIPTION    TO HR11F1-WORK-SCHED-DESC.

154300     IF (HR11F1-EMP-UNION-CODE NOT = SPACES)
154400         MOVE "UN"                   TO DB-TYPE
154500         MOVE HR11F1-EMP-UNION-CODE  TO DB-CODE
154600         PERFORM 840-FIND-PCOSET1
               IF  (PCODES-FOUND)
154700             MOVE PCO-DESCRIPTION    TO HR11F1-UNION-CODE-DESC.
154800
155600     IF (HR11F1-EMP-HM-DIST-CO    NOT = ZEROES)
155700         MOVE HR11F1-EMP-HM-DIST-CO  TO IFACWS-COMPANY
155800                                        ACACWS-COMPANY
155900     ELSE
156000         MOVE DPT-DEP-DIST-CO        TO IFACWS-COMPANY
156100                                        ACACWS-COMPANY.
156200     MOVE HR11F1-EMP-HM-ACCT-UNIT    TO IFACWS-ACCT-UNIT
                                              ACACWS-ACCT-UNIT.
156300     MOVE HR11F1-EMP-HM-ACCOUNT      TO IFACWS-ACCOUNT
                                              ACACWS-ACCOUNT.
156400     MOVE HR11F1-EMP-HM-SUB-ACCT     TO IFACWS-SUB-ACCOUNT
                                              ACACWS-SUB-ACCOUNT.
156500     MOVE "PR"                       TO IFACWS-SYSTEM.
156600     MOVE 4                          TO IFACWS-EDIT-TYPE.
156800
156900     IF (HR11F1-EMP-HM-ACCT-UNIT NOT = SPACES)
157000     OR (HR11F1-EMP-HM-ACCOUNT   NOT = ZEROS)
157100     OR (HR11F1-EMP-HM-SUB-ACCT  NOT = ZEROS)
157200         PERFORM 635-EDIT-GLMASTER-60
157300         MOVE IFACWS-GLM-DESCRIPTION TO HR11F1-GLM-DESCRIPTION
               INITIALIZE                     CRT-ERROR-NBR
                                              CRT-ERROR-CAT.
157400
157500     MOVE HR11F1-EMP-ACTIVITY        TO ACACWS-ACTIVITY.
           MOVE HR11F1-EMP-ACCT-CATEGORY   TO ACACWS-ACCT-CATEGORY.
157600     IF (HR11F1-EMP-ACTIVITY NOT = SPACES)
157700         MOVE 2                      TO ACACWS-EDIT-CODE
157800         PERFORM 640-EDIT-ACTIVITY-70
157900         MOVE ACACWS-ACV-DESCRIPTION TO HR11F1-ACV-DESCRIPTION
               INITIALIZE                     CRT-ERROR-NBR
                                              CRT-ERROR-CAT.
158000
159700     IF (HR11F1-EMP-WC-STATE NOT = SPACES)
159800         MOVE HR11F1-EMP-WC-STATE    TO DB-STATE
159900         PERFORM 840-FIND-PSASET1
               IF  (PRSTATE-FOUND)
160000             MOVE PSA-DESCRIPTION    TO HR11F1-PSA2-DESCRIPTION.
160100
161700     IF (HR11F1-PEM-BARGAIN-UNIT NOT = SPACES)
161800         MOVE "BU"                   TO DB-TYPE
161900         MOVE HR11F1-PEM-BARGAIN-UNIT
162000                                     TO DB-CODE
162100         PERFORM 840-FIND-PCOSET1
               IF  (PCODES-FOUND)
162200             MOVE PCO-DESCRIPTION    TO HR11F1-PCO3-DESCRIPTION.
162300
162400     IF (HR11F1-PEM-SECURITY-CODE NOT = SPACES)
162500         MOVE "SC"                   TO DB-TYPE
162600         MOVE HR11F1-PEM-SECURITY-CODE
162700                                     TO DB-CODE
162800         PERFORM 840-FIND-PCOSET1
               IF  (PCODES-FOUND)
162900             MOVE PCO-DESCRIPTION    TO HR11F1-PCO4-DESCRIPTION.
163000
163100     IF (HR11F1-PEM-MAIL-GROUP NOT = SPACES)
163200         MOVE "MG"                   TO DB-TYPE
163300         MOVE HR11F1-PEM-MAIL-GROUP  TO DB-CODE
163400         PERFORM 840-FIND-PCOSET1
               IF  (PCODES-FOUND)
163500             MOVE PCO-DESCRIPTION    TO HR11F1-PCO5-DESCRIPTION.
163600
163700     IF (HR11F1-PEM-HIRE-SOURCE NOT = SPACES)
163800         MOVE "HS"                   TO DB-TYPE
163900         MOVE HR11F1-PEM-HIRE-SOURCE TO DB-CODE
164000         PERFORM 840-FIND-PCOSET1
               IF  (PCODES-FOUND)
164100             MOVE PCO-DESCRIPTION    TO HR11F1-PCO6-DESCRIPTION.
164200
164600     MOVE HR11F1-EMP-COMPANY         TO DB-COMPANY.
164700
           IF (HR11F1-EMP-CURR-CODE NOT = SPACES)
               MOVE HR11F1-EMP-CURR-CODE      TO DB-CURRENCY-CODE
               PERFORM 840-FIND-CUCSET1
               IF (CUCODES-FOUND)
                   MOVE CUC-FORMS-EXP      TO HR11F1-CURR-CODE-DESC
                                              HR11F1-CURR-CODE-2-DESC
               END-IF.

164800     IF (HR11F1-PEM-PRIMARY-CARE NOT = SPACES)
164900         MOVE "PC"                   TO DB-TYPE
165000         MOVE HR11F1-PEM-PRIMARY-CARE    TO DB-CODE
165200         PERFORM 840-FIND-PCOSET1
               IF  (PCODES-FOUND)
165300             MOVE PCO-DESCRIPTION    TO HR11F1-PCO7-DESCRIPTION.
165400
165500     IF (HR11F1-PEM-FAMILY-AGG NOT = ZEROS)
165600         MOVE HR11F1-PEM-FAMILY-AGG  TO DB-EMPLOYEE
165700         MOVE PTFSET1-EMPLOYEE       TO WS-DB-BEG-RNG
165800         PERFORM 850-FIND-BEGRNG-PTFSET1
               IF  (PATHFIND-FOUND)
165900             MOVE PTF-LAST-NAME          TO HRWS-LAST-NAME
166000             MOVE PTF-FIRST-NAME         TO HRWS-FIRST-NAME
166100             MOVE PTF-MIDDLE-INIT        TO HRWS-MIDDLE-INIT
166200             PERFORM 750-HR-FORMAT-NAME
166300             MOVE HRWS-FORMAT-NAME       TO
166300                                         HR11F1-FAMILY-AGG-NAME.
166400
            IF (HR11F1-PEM-BIRTH-CNTRY-CD NOT = SPACES)
                MOVE HR11F1-PEM-BIRTH-CNTRY-CD TO DB-COUNTRY-CODE
                PERFORM 840-FIND-INTSET1
                IF (INSTCTRYCD-FOUND)
                    MOVE INT-COUNTRY-DESC  TO HR11F1-BIRTH-COUNTRY-DESC
                END-IF.

           IF (HR11F1-PEM-SUPP-CNTRY-CD NOT = SPACES)
               MOVE HR11F1-PEM-SUPP-CNTRY-CD    TO DB-COUNTRY-CODE
               PERFORM 840-FIND-INTSET1
               IF (INSTCTRYCD-FOUND)
                   MOVE INT-COUNTRY-DESC   TO HR11F1-SUPP-COUNTRY-DESC
               END-IF.

           IF (HR11F1-EMP-COUNTRY-CODE NOT = SPACES)
               MOVE HR11F1-EMP-COUNTRY-CODE TO DB-COUNTRY-CODE
               PERFORM 840-FIND-INTSET1
               IF (INSTCTRYCD-FOUND)
                   MOVE INT-COUNTRY-DESC   TO HR11F1-EMP-COUNTRY-DESC
               END-IF.

           IF (HR11F1-EMP-WORK-COUNTRY NOT = SPACES)
               MOVE HR11F1-EMP-WORK-COUNTRY TO DB-COUNTRY-CODE
               PERFORM 840-FIND-INTSET1
               IF (INSTCTRYCD-FOUND)
                   MOVE INT-COUNTRY-DESC   TO HR11F1-WORK-COUNTRY-DESC
               END-IF.

           IF (HR11F1-PEM-SP-EMP-COUNTRY NOT = SPACES)
               MOVE HR11F1-PEM-SP-EMP-COUNTRY
                                            TO  DB-COUNTRY-CODE
               PERFORM 840-FIND-INTSET1
               IF (INSTCTRYCD-FOUND)
                   MOVE INT-COUNTRY-DESC    TO HR11F1-SP-COUNTRY-DESC
               END-IF.

           IF (HR11F1-PEM-EEO-CLASS NOT = SPACES)
               MOVE "ET"                   TO DB-TYPE
               MOVE HR11F1-EMP-WORK-COUNTRY
                                           TO DB-COUNTRY-CODE
               MOVE HR11F1-PEM-EEO-CLASS   TO DB-HRCTRY-CODE
               PERFORM 840-FIND-CTCSET1
               IF (HRCTRYCODE-FOUND)
                   MOVE CTC-DESCRIPTION    TO HR11F1-CTC-DESCRIPTION
               END-IF.

J40211     IF (HR11F1-PEM-EEO-CLASS = SPACES)
J40211         MOVE SPACES                 TO HR11F1-ETHNICITY
J40211     ELSE
J40211         MOVE "ETHNICITY"            TO HR11F1-ETHNICITY
J40211     END-IF.               

           IF (HR11F1-PEM-I9-DOC-TYPE1 NOT = SPACES)
               MOVE "DT"                   TO DB-TYPE
               MOVE HR11F1-PEM-I9-DOC-TYPE1 TO DB-CODE
               PERFORM 840-FIND-PCOSET1
               IF  (PCODES-FOUND)
                   MOVE PCO-DESCRIPTION    TO HR11F1-PCO-DT-DESC1.

           IF (HR11F1-PEM-I9-DOC-TYPE2 NOT = SPACES)
               MOVE "DT"                   TO DB-TYPE
               MOVE HR11F1-PEM-I9-DOC-TYPE2 TO DB-CODE
               PERFORM 840-FIND-PCOSET1
               IF  (PCODES-FOUND)
                   MOVE PCO-DESCRIPTION    TO HR11F1-PCO-DT-DESC2.

           IF (HR11F1-PEM-I9-DOC-DESCR1 NOT = SPACES)
               MOVE "DO"                   TO DB-TYPE
               MOVE HR11F1-PEM-I9-DOC-DESCR1 TO DB-CODE
               PERFORM 840-FIND-PCOSET1
               IF  (PCODES-FOUND)
                   MOVE PCO-DESCRIPTION    TO HR11F1-PCO-DO-DESC1.

           IF (HR11F1-PEM-I9-DOC-DESCR2 NOT = SPACES)
               MOVE "DO"                   TO DB-TYPE
               MOVE HR11F1-PEM-I9-DOC-DESCR2 TO DB-CODE
               PERFORM 840-FIND-PCOSET1
               IF  (PCODES-FOUND)
                   MOVE PCO-DESCRIPTION    TO HR11F1-PCO-DO-DESC2.

           IF (HR11F1-PEM-I9-STATUS NOT = SPACES)
               MOVE "WE"                   TO DB-TYPE
               MOVE HR11F1-PEM-I9-STATUS          TO DB-CODE
               PERFORM 840-FIND-PCOSET1
               IF  (PCODES-FOUND)
                   MOVE PCO-DESCRIPTION    TO HR11F1-PCO-WE-DESC.

166500     IF  (HR11F1-EMP-DATE-HIRED NOT = ZEROES)
P60752     AND (PA-USER)
166600         MOVE 148                    TO CRT-MSG-NBR
166700         MOVE "HR11"                 TO CRT-ERROR-CAT
166800         PERFORM 790-GET-MSG
166900         MOVE CRT-MESSAGE            TO HR11F1-SERVICE  
               MOVE SPACES                 TO CRT-MESSAGE.
167000
J19381
J19381     IF  (HR11F1-EMP-WORK-COUNTRY NOT = "US" AND "CA")
J19381*---HR11#186: Additional Details
J19381         MOVE 186                    TO CRT-MSG-NBR
J19381         MOVE "HR11"                 TO CRT-ERROR-CAT
J19381         PERFORM 790-GET-MSG
J19381         MOVE CRT-MESSAGE            TO HR11F1-ADDITIONAL-DETAIL  
J19381         MOVE SPACES                 TO CRT-MESSAGE.
J19381
166500     IF (PA-USER)
166600         MOVE 103                    TO CRT-MSG-NBR
166700         MOVE "HR11"                 TO CRT-ERROR-CAT
166800         PERFORM 790-GET-MSG
166900         MOVE CRT-MESSAGE            TO HR11F1-POSITIONS
               MOVE SPACES                 TO CRT-MESSAGE.
167000
166500     IF (HR11F1-EMP-PAY-RATE NOT = ZEROES)
166600         MOVE 138                    TO CRT-MSG-NBR
166700         MOVE "HR11"                 TO CRT-ERROR-CAT
166800         PERFORM 790-GET-MSG
166900         MOVE CRT-MESSAGE            TO HR11F1-COMP-ANALYSIS
               MOVE SPACES                 TO CRT-MESSAGE.
167000
           IF (HR11F1-EMP-PRMCERT-ID NOT = ZEROES)
J77218         MOVE HR11F1-EMP-PRMCERT-COMP    TO DB-COMPANY
               MOVE HR11F1-EMP-PRMCERT-ID  TO DB-EMPLOYEE
               PERFORM 840-FIND-EMPSET1
               IF (EMPLOYEE-FOUND)
                   MOVE EMP-LAST-NAME      TO HRWS-LAST-NAME
                   MOVE EMP-FIRST-NAME     TO HRWS-FIRST-NAME
                   MOVE EMP-MIDDLE-INIT    TO HRWS-MIDDLE-INIT
                   MOVE EMP-NAME-SUFFIX    TO HRWS-NAME-SUFFIX
                   MOVE EMP-LAST-NAME-PRE  TO HRWS-LAST-NAME-PRE
                   PERFORM 750-HR-FORMAT-NAME
                   MOVE HRWS-FORMAT-NAME   TO HR11F1-PRIMARY-NAME
               END-IF
           END-IF.

           IF (HR11F1-EMP-SNDCERT-ID NOT = ZEROES)
J77218         MOVE HR11F1-EMP-SNDCERT-COMP    TO DB-COMPANY
               MOVE HR11F1-EMP-SNDCERT-ID      TO DB-EMPLOYEE
               PERFORM 840-FIND-EMPSET1
               IF (EMPLOYEE-FOUND)
                   MOVE EMP-LAST-NAME      TO HRWS-LAST-NAME
                   MOVE EMP-FIRST-NAME     TO HRWS-FIRST-NAME
                   MOVE EMP-MIDDLE-INIT    TO HRWS-MIDDLE-INIT
                   MOVE EMP-NAME-SUFFIX    TO HRWS-NAME-SUFFIX
                   MOVE EMP-LAST-NAME-PRE  TO HRWS-LAST-NAME-PRE
                   PERFORM 750-HR-FORMAT-NAME
                   MOVE HRWS-FORMAT-NAME   TO HR11F1-SECONDARY-NAME
               END-IF
           END-IF.

           IF (HR11F1-EMP-PRMCERT-ID NOT = ZEROES)
           OR (HR11F1-EMP-SNDCERT-ID NOT = ZEROES)
               MOVE HR11F1-EMP-COMPANY     TO DB-COMPANY
               MOVE HR11F1-EMP-EMPLOYEE    TO DB-EMPLOYEE
               PERFORM 840-FIND-EMPSET1.

           INITIALIZE                      HR11F1-STATE-BTN-INDICATOR.
           IF (HR11F1-FC NOT = "A")
               MOVE HR11F1-EMP-COMPANY     TO DB-COMPANY
               MOVE HR11F1-EMP-EMPLOYEE    TO DB-EMPLOYEE
               MOVE EMP-WORK-STATE         TO DB-STATE
               PERFORM 840-FIND-PESSET1
               IF (PREMPSTATE-FOUND)
                   MOVE "*"                TO HR11F1-STATE-BTN-INDICATOR
               END-IF
           END-IF.

           MOVE ZEROES TO CRT-ERROR-NBR.

167100 600-END.
167200
167300******************************************************************
005600 610-MOVE-DTL-TO-SCREEN.
167300******************************************************************

           MOVE HR11F1-WS-FLD-NBR (HR11F1-REC) TO HR11WS-FLD-NBR-G.
           MOVE HR11WS-FIELD-KEY               TO DB-FIELD-KEY.
           PERFORM 840-FIND-HRUSET1.
           IF (HRUSERFLDS-NOTFOUND)
               IF (HR11F1-FC                   = "-")
                   SUBTRACT 1                  FROM HR11F1-REC
                   ADD 1                       TO HR11F1-I1
               ELSE
                   ADD 1                       TO HR11F1-REC
                   SUBTRACT 1                  FROM HR11F1-I1
               END-IF
               GO TO 610-END.

           MOVE DB-FIELD-KEY            TO
                                       HR11F1-HRU-FIELD-KEY (HR11F1-I1).
           MOVE HRU-FIELD-TYPE          TO
                                      HR11F1-HRU-FIELD-TYPE (HR11F1-I1).
197400     MOVE HRU-FIELD-NAME          TO
                                      HR11F1-HRU-FIELD-NAME (HR11F1-I1).
           IF (HRU-ACTIVE-FLAG = "I")
               MOVE HRU-ACTIVE-FLAG         TO
                                     HR11F1-HRU-ACTIVE-FLAG (HR11F1-I1)
           ELSE
               MOVE SPACES                  TO
                                     HR11F1-HRU-ACTIVE-FLAG (HR11F1-I1)
           END-IF.
           MOVE HR11F1-WS-FLD-NBR (HR11F1-REC)
                                      TO HR11F1-HRU-FLD-NBR (HR11F1-I1).
           MOVE HR11F1-WS-REQ-FLAG (HR11F1-REC)
                                     TO HR11F1-HRU-REQ-FLAG (HR11F1-I1).

           MOVE 140                     TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE             TO
                                    HR11F1-BASE-CURRENCY-TAB(HR11F1-I1).

           INITIALIZE HR11F1-HEU-BASE-CURRENCY (HR11F1-I1)
                      HR11F1-BASE-FORMS-EXP    (HR11F1-I1)
                      HR11F1-HEU-CURRENCY-CODE (HR11F1-I1)
                      HR11F1-CALC-WINDOW       (HR11F1-I1)
                      HR11F1-HEU-BASE-ND       (HR11F1-I1)
                      HR11F1-HEU-CURR-ND       (HR11F1-I1)
                      HR11F1-HEU-BASE-AMOUNT   (HR11F1-I1).
           IF (HR11F1-HRU-FIELD-TYPE (HR11F1-I1)  = "N")
               MOVE 139                    TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE       TO HR11F1-CALC-WINDOW (HR11F1-I1).

           MOVE HRU-SEC-LEVEL              TO HRWS-SEC-LEVEL.
           PERFORM 730-HR-FIELD-SECURITY.
           IF (HRWS-FLD-SECURED)
               GO TO 610-CONTINUE.

197600     MOVE HR11F1-EMP-COMPANY         TO DB-COMPANY.
           INITIALIZE DB-EMP-APP.
           MOVE HR11F1-EMP-EMPLOYEE        TO DB-EMPLOYEE.
197700     MOVE HRU-FIELD-KEY              TO DB-FIELD-KEY.
197800     PERFORM 840-FIND-HEUSET1.
197900     IF (HREMPUSF-FOUND)
               PERFORM 612-MOVE-HEU-TO-SCREEN
               THRU    612-END
               INITIALIZE HR11F1-HRU-DESCRIPTION (HR11F1-I1)
               MOVE HRU-FIELD-KEY          TO DB-TYPE
               MOVE HR11F1-VALUE (HR11F1-I1)      TO DB-CODE
               PERFORM 840-FIND-PCOSET1
               IF (PCODES-FOUND)
                   MOVE PCO-DESCRIPTION   TO
                                      HR11F1-HRU-DESCRIPTION(HR11F1-I1).

       610-CONTINUE.
           MOVE HR11F1-WS-LINE-FC (HR11F1-REC)   TO
                                             HR11F1-LINE-FC (HR11F1-I1).
           IF (HR11F1-WS-LINE-FC (HR11F1-REC)    NOT = SPACES)
               MOVE HR11F1-WS-VALUE (HR11F1-REC)
                                           TO HR11F1-VALUE (HR11F1-I1)
               MOVE HR11F1-WS-CURR (HR11F1-REC)
                                           TO
                                    HR11F1-HEU-CURRENCY-CODE (HR11F1-I1)
               MOVE HR11F1-WS-BASE-AMT (HR11F1-REC)
                                           TO
                                     HR11F1-HEU-BASE-AMOUNT (HR11F1-I1).

           IF (HR11F1-I1                          = 1)
               MOVE HR11F1-REC             TO HR11F1-TOP-REC.

           IF (HR11F1-FC                   = "-")
               SUBTRACT 1                  FROM HR11F1-REC
           ELSE
               ADD 1                       TO HR11F1-REC.

005700 610-END.

167300******************************************************************
       612-MOVE-HEU-TO-SCREEN.
167300******************************************************************

           IF (HR11F1-HRU-FIELD-TYPE (HR11F1-I1)  = "A")
               MOVE HEU-A-FIELD            TO HR11F1-VALUE (HR11F1-I1)
           ELSE
199200     IF (HR11F1-HRU-FIELD-TYPE (HR11F1-I1)  = "N")
               MOVE HEU-CURRENCY-CODE  TO
                                    HR11F1-HEU-CURRENCY-CODE (HR11F1-I1)
               MOVE HEU-CURR-ND        TO HR11F1-HEU-CURR-ND (HR11F1-I1)
               MOVE HEU-BASE-CURRENCY  TO
                                    HR11F1-HEU-BASE-CURRENCY (HR11F1-I1)
                                          DB-CURRENCY-CODE
               PERFORM 840-FIND-CUCSET1
               IF (CUCODES-FOUND)
                   MOVE CUC-FORMS-EXP  TO
                                       HR11F1-BASE-FORMS-EXP (HR11F1-I1)
               ELSE
                   INITIALIZE HR11F1-BASE-FORMS-EXP (HR11F1-I1)
               END-IF
               MOVE HEU-BASE-ND        TO HR11F1-HEU-BASE-ND (HR11F1-I1)
                                   HR11F1-HEU-BASE-AMOUNT-ND (HR11F1-I1)
               MOVE HEU-BASE-AMOUNT    TO
                                      HR11F1-HEU-BASE-AMOUNT (HR11F1-I1)
               MOVE HEU-N-FIELD        TO HRWS-DEC-FIELD
               IF (HEU-CURRENCY-CODE   = SPACES)
                   MOVE 2              TO HRWS-NBR-DECIMALS
               ELSE
                   MOVE HEU-CURR-ND    TO HRWS-NBR-DECIMALS
               END-IF
               MOVE 11                 TO HRWS-SIZE
               PERFORM 770-HR-FORMAT-DEC-FIELD
               MOVE HRWS-VALUE         TO HR11F1-VALUE (HR11F1-I1)
           ELSE
199600     IF (HR11F1-HRU-FIELD-TYPE (HR11F1-I1) = "D")
               MOVE HEU-D-FIELD        TO HRWS-DATE-8-FIELD
               PERFORM 781-HR-FORMAT-DATE-FIELD
               MOVE HRWS-VALUE         TO HR11F1-VALUE (HR11F1-I1).

       612-END.

167300******************************************************************
167400 650-DEFAULT.
167500******************************************************************
167600
167700     IF (NOT-PA-USER)
167800         MOVE 101                    TO CRT-ERROR-NBR
167900         MOVE HR11F1-FC-FN           TO CRT-FIELD-NBR
168000         GO TO 650-END.
168100
168200     IF (HR11F1-EFFECT-DATE = ZEROES)
168300         MOVE 122                    TO CRT-ERROR-NBR
168400         MOVE HR11F1-EFFECT-DATE-FN  TO CRT-FIELD-NBR
168500         GO TO 650-END
168600     ELSE
168700     IF (HR11F1-EMP-POSITION = SPACES)
168800         MOVE 104                    TO CRT-ERROR-NBR
168900         MOVE HR11F1-EMP-POSITION-FN
169000                                     TO CRT-FIELD-NBR
169100         GO TO 650-END.
169200
169300     PERFORM 725-DEFAULT-ACTION
169400     THRU    725-END.
169500
169600 650-END.
169700
169800******************************************************************
169900 700-SCREEN-XFER.
170000******************************************************************
170100
           IF (HR11F1-HREMP-COMPANY  NOT = HR11F1-EMP-COMPANY)
           OR (HR11F1-HREMP-EMPLOYEE NOT = HR11F1-EMP-EMPLOYEE)
               MOVE 137                    TO CRT-ERROR-NBR
               MOVE HR11F1-EMP-EMPLOYEE-FN TO CRT-FIELD-NBR
               GO TO 700-END.
      
           IF (HRWS-EMP-SECURED)
               MOVE HR11F1-EMP-EMPLOYEE-FN TO CRT-FIELD-NBR
               MOVE 143                    TO CRT-ERROR-NBR
               GO TO 700-END.

171000     IF (HR11F1-FC = "W")
171100         MOVE "C"                TO CRT-DISPLAY-FC
171200         MOVE "I"                TO CRT-PASS-FC
171300         MOVE CRT-MANUAL-CF      TO CRT-REQUEST
171400         MOVE SPACES             TO CRT-MESSAGE
171500         MOVE HR11F1-COMMENTS-FN TO CRT-FIELD-NBR
171600         MOVE "HR901"            TO CRT-SCREEN-CODE
               INITIALIZE CRT-MSG-NBR
                          CRT-FIELD-NBR
171700         GO TO 700-END.
171800
171900     IF (HR11F1-FC = "T")
172000         MOVE "C"                TO CRT-DISPLAY-FC
172100         MOVE "I"                TO CRT-PASS-FC
172200         MOVE CRT-MANUAL-CF      TO CRT-REQUEST
172300         MOVE SPACES             TO CRT-MESSAGE
172400         MOVE HR11F1-POSITIONS-FN
172500                                 TO CRT-FIELD-NBR
172600         MOVE "PA131"            TO CRT-SCREEN-CODE
               INITIALIZE CRT-MSG-NBR
                          CRT-FIELD-NBR
172700         GO TO 700-END.
172800
171900     IF (HR11F1-FC = "G")
172000         MOVE "C"                   TO CRT-DISPLAY-FC
172100         MOVE "I"                   TO CRT-PASS-FC
172200         MOVE CRT-MANUAL-CF         TO CRT-REQUEST
172300         MOVE SPACES                TO CRT-MESSAGE
172400         MOVE HR11F1-COMP-ANALYSIS-FN 
172500                                    TO CRT-FIELD-NBR 
172600         MOVE "HR112"               TO CRT-SCREEN-CODE
               INITIALIZE CRT-MSG-NBR
                          CRT-FIELD-NBR
172700         GO TO 700-END.
172800
171900     IF (HR11F1-FC = "H")
172000*        MOVE "I"                   TO CRT-DISPLAY-FC
172100         MOVE "I"                   TO CRT-PASS-FC
172200         MOVE CRT-MANUAL-CF         TO CRT-REQUEST
172300         MOVE SPACES                TO CRT-MESSAGE
172400         MOVE HR11F1-SERVICE-FN         
172500                                    TO CRT-FIELD-NBR 
172600         MOVE "PA101"               TO CRT-SCREEN-CODE
               INITIALIZE CRT-MSG-NBR
                          CRT-FIELD-NBR
172700         GO TO 700-END.
172800
172900     IF (HR11F1-FC = "X")
173000         MOVE "C"                   TO CRT-DISPLAY-FC
173100         MOVE "I"                   TO CRT-PASS-FC
173200         MOVE CRT-MANUAL-CF         TO CRT-REQUEST
173300         MOVE SPACES                TO CRT-MESSAGE
               MOVE HR11F1-EMP-EMPLOYEE   TO HR11F1-EMPLOYEE-APP-NBR
173400         MOVE HR11F1-USER-FIELDS-FN TO CRT-FIELD-NBR
173500         MOVE "HR151"               TO CRT-SCREEN-CODE
               INITIALIZE CRT-MSG-NBR
                          CRT-FIELD-NBR
173600         GO TO 700-END.
173700
173800     IF (HR11F1-FC = "Y")
173900         MOVE "C"                     TO CRT-DISPLAY-FC
174000         MOVE "I"                     TO CRT-PASS-FC
174100         MOVE CRT-MANUAL-CF           TO CRT-REQUEST
174200         MOVE SPACES                  TO CRT-MESSAGE
174300         MOVE HR11F1-ALT-PAY-RATES-FN TO CRT-FIELD-NBR
174400         MOVE "HR141"                 TO CRT-SCREEN-CODE
               INITIALIZE CRT-MSG-NBR
                          CRT-FIELD-NBR
174500         GO TO 700-END.
174600
171900     IF (HR11F1-FC = "M")
172000         MOVE "C"                TO CRT-DISPLAY-FC
172100         MOVE "I"                TO CRT-PASS-FC
172200         MOVE CRT-MANUAL-CF      TO CRT-REQUEST
172300         MOVE SPACES             TO CRT-MESSAGE
172400         MOVE HR11F1-MORE-FN
172500                                 TO CRT-FIELD-NBR
172600         MOVE "HR201"            TO CRT-SCREEN-CODE
               INITIALIZE CRT-MSG-NBR
                          CRT-FIELD-NBR
172700         GO TO 700-END.
172800

J27500     IF (HR11F1-FC = "V")
J27500         MOVE HR11F1-EMP-COMPANY     TO DB-COMPANY
J27500         MOVE HR11F1-EMP-EMPLOYEE    TO DB-EMPLOYEE
J27500         PERFORM 840-FIND-EANSET1
J27500         IF (EMPALTNAME-NOTFOUND)
J27500             MOVE "A"                TO CRT-DISPLAY-FC
J27500             MOVE "I"                TO CRT-PASS-FC
J27500             MOVE CRT-MANUAL-CF      TO CRT-REQUEST
J27500             MOVE SPACES             TO CRT-MESSAGE
J27500             MOVE "HR402"            TO CRT-SCREEN-CODE
J27500             INITIALIZE CRT-MSG-NBR
J27500                        CRT-FIELD-NBR
J27500             GO TO 700-END
J27500         ELSE
J27500             MOVE "C"                TO CRT-DISPLAY-FC
J27500             MOVE "I"                TO CRT-PASS-FC
J27500             MOVE CRT-MANUAL-CF      TO CRT-REQUEST
J27500             MOVE SPACES             TO CRT-MESSAGE
J27500             MOVE "HR402"            TO CRT-SCREEN-CODE
J27500             INITIALIZE CRT-MSG-NBR
J27500                        CRT-FIELD-NBR
J27500             GO TO 700-END
J27500         END-IF
J27500     END-IF.
J19381
J19381     IF (HR11F1-FC = "O")
J19381        MOVE "I"                    TO CRT-DISPLAY-FC
J19381        MOVE "I"                    TO CRT-PASS-FC
J19381        MOVE CRT-MANUAL-CF          TO CRT-REQUEST
J19381        MOVE SPACES                 TO CRT-MESSAGE
J19381        MOVE HR11F1-FC-FN           TO CRT-FIELD-NBR
J19381        MOVE "HR115"                TO CRT-SCREEN-CODE
J19381        GO TO 700-END
J19381     END-IF.
J19381
           IF  (HR11F1-FC NOT = "Z")
               GO TO 700-END-STATE.

           MOVE HR11F1-EMP-COMPANY     TO DB-COMPANY.
           MOVE HR11F1-EMP-EMPLOYEE    TO DB-EMPLOYEE.
           PERFORM 840-FIND-EMPSET1.
           PERFORM 840-FIND-PEMSET1.

           IF  (EMPLOYEE-NOTFOUND)
           OR  (PAEMPLOYEE-NOTFOUND)
           OR  (HR11F1-PEM-LOCAT-CODE NOT = PEM-LOCAT-CODE)
           OR  (HR11F1-EMP-STATE      NOT = EMP-STATE)
           OR  (HR11F1-PEM-SUPP-STATE NOT = PEM-SUPP-STATE)
               MOVE 109                                TO CRT-ERROR-NBR
               MOVE HR11F1-FC-FN                       TO CRT-FIELD-NBR
               GO TO 700-END.

           MOVE EMP-WORK-STATE         TO HR11F1-EMP-WORK-STATE.
           MOVE EMP-WORK-STATE         TO DB-STATE.
           PERFORM 840-FIND-PESSET1.

           IF (PREMPSTATE-FOUND)
               MOVE "C"                TO CRT-DISPLAY-FC
           ELSE
               MOVE "A"                TO CRT-DISPLAY-FC.

           MOVE "I"                    TO CRT-PASS-FC.
           MOVE CRT-MANUAL-CF          TO CRT-REQUEST.
           MOVE SPACES                 TO CRT-MESSAGE.
           MOVE HR11F1-STATE-BUTTON-FN TO CRT-FIELD-NBR.

           IF (DB-STATE = "AK")
               MOVE "HR162"            TO CRT-SCREEN-CODE
               GO TO 700-END.

           IF (DB-STATE = "AZ")
               MOVE "HR163"            TO CRT-SCREEN-CODE
               GO TO 700-END.

           IF (DB-STATE = "AR")
               MOVE "HR164"            TO CRT-SCREEN-CODE
               GO TO 700-END.

           IF (DB-STATE = "CA")
               MOVE "HR165"            TO CRT-SCREEN-CODE
               GO TO 700-END.

           IF (DB-STATE = "CO" OR "ME")
               MOVE "HR166"            TO CRT-SCREEN-CODE
               GO TO 700-END.

           IF (DB-STATE = "FL")
               MOVE "HR167"            TO CRT-SCREEN-CODE
               GO TO 700-END.

           IF (DB-STATE = "IL")
               MOVE "HR168"            TO CRT-SCREEN-CODE
               GO TO 700-END.

           IF (DB-STATE = "IN")
               MOVE "HR171"            TO CRT-SCREEN-CODE
               GO TO 700-END.

           IF (DB-STATE = "IA")
               MOVE "HR172"            TO CRT-SCREEN-CODE
               GO TO 700-END.

      *  MA ENTITY CODE ENTRY CHANGED TO PR06
      *     IF (DB-STATE = "MA")
      *         MOVE "HR173"            TO CRT-SCREEN-CODE
      *         GO TO 700-END.

           IF (DB-STATE = "MN")
               MOVE "HR174"            TO CRT-SCREEN-CODE
               GO TO 700-END.

           IF (DB-STATE = "MO")
               MOVE "HR175"            TO CRT-SCREEN-CODE
               GO TO 700-END.

           IF (DB-STATE = "NC")
               MOVE "HR176"            TO CRT-SCREEN-CODE
               GO TO 700-END.

           IF (DB-STATE = "NJ")
               MOVE "HR177"            TO CRT-SCREEN-CODE
               GO TO 700-END.

           IF (DB-STATE = "OH")
               MOVE "HR178"            TO CRT-SCREEN-CODE
               GO TO 700-END.

           IF (DB-STATE = "OR")
               MOVE "HR179"            TO CRT-SCREEN-CODE
               GO TO 700-END.

           IF (DB-STATE = "PA")
               MOVE "HR181"            TO CRT-SCREEN-CODE
               GO TO 700-END.

           IF (DB-STATE = "TX")
               MOVE "HR182"            TO CRT-SCREEN-CODE
               GO TO 700-END.

           IF (DB-STATE = "WY")
               MOVE "HR183"            TO CRT-SCREEN-CODE
               GO TO 700-END.

           IF (DB-STATE = "MI")
               MOVE "HR184"            TO CRT-SCREEN-CODE
               GO TO 700-END.

J26176     IF (DB-STATE = "PR")
J26176         MOVE "HR185"            TO CRT-SCREEN-CODE
J26176         GO TO 700-END
J26176     END-IF.

J04456     IF (DB-STATE = "NM")
J04456         MOVE "HR186"            TO CRT-SCREEN-CODE
J04456         GO TO 700-END
J04456     END-IF.

J66361     IF (DB-STATE = "DC")
J66361         MOVE "HR187"            TO CRT-SCREEN-CODE
J66361         GO TO 700-END
J66361     END-IF.

108049     IF (DB-STATE = "SC")
108049         MOVE "HR188"            TO CRT-SCREEN-CODE
108049         GO TO 700-END
108049     END-IF.

180940     IF (DB-STATE = "MD")
180940         MOVE "HR189"            TO CRT-SCREEN-CODE
180940         GO TO 700-END
180940     END-IF.

277456     IF (DB-STATE = "WV")
277456         MOVE "HR231"            TO CRT-SCREEN-CODE
277456         GO TO 700-END
277456     END-IF.

           MOVE DB-STATE                   TO CRT-ERR-VAR1.
           MOVE 107                        TO CRT-ERROR-NBR.
           MOVE SPACES                     TO CRT-DISPLAY-FC.
           MOVE SPACES                     TO CRT-PASS-FC.
           MOVE 1                          TO CRT-REQUEST.
           GO TO 700-END.

       700-END-STATE.

185200 700-END.

090900******************************************************************
091000 710-SCREEN-XFER.
091100******************************************************************
091200
           MOVE HR11F1-EFFECT-DATE     TO HR11F1-EDM-EFFECT-DATE.

092300     MOVE "I"                    TO CRT-PASS-FC.
091900     MOVE HR11F1-FC              TO CRT-DISPLAY-FC.
092400     MOVE CRT-MANUAL-CF          TO CRT-REQUEST.
092500     INITIALIZE CRT-MESSAGE.
092600     MOVE HR11F1-FC-FN           TO CRT-FIELD-NBR.

           IF (HR11F1-FC               = "A")
092900         MOVE "PR133"            TO CRT-SCREEN-CODE
           ELSE
092900         MOVE "PR134"            TO CRT-SCREEN-CODE.
093100
101100 710-END.
101200
185300******************************************************************
185400 725-DEFAULT-ACTION.
185500******************************************************************
185600
           SET HR11WS-USE-COMPANY          TO TRUE.
           IF (HR11F1-EMP-POSITION NOT = SPACES)
               MOVE HR11F1-EMP-COMPANY     TO DB-COMPANY
               MOVE HR11F1-EMP-POSITION    TO DB-POSITION
               MOVE HR11F1-EFFECT-DATE     TO DB-EFFECT-DATE
               PERFORM 850-FIND-NLT-POSSET2
               IF  (PAPOSITION-FOUND)
               AND (POS-COMPANY           = DB-COMPANY)
               AND (POS-POSITION          = DB-POSITION)
               AND (POS-PROCESS-LEVEL NOT = SPACES)
                   MOVE "PO"               TO DB-TOPIC
                   MOVE SPACES             TO DB-COUNTRY-CD-REQ
                   MOVE POS-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
                   MOVE PASSET2-PROCESS-LEVEL
                                           TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-PASSET2
                   IF (PASCRTY-FOUND)
                       SET HR11WS-USE-PROCESS-LEVEL
                                           TO TRUE.

186100     PERFORM 750-CHECK-SECURITY
186200     THRU    750-END.
186700
186800     IF (ERROR-FOUND)
186900         GO TO 725-END.
187000
187100     PERFORM 775-LOAD-PADFP-FIELDS
187200     THRU    775-END.
187300
187400     PERFORM 2000-EDIT-DEFAULTS.
187500     IF (ERROR-FOUND)
187600         GO TO 725-END.
187700
187800     PERFORM 800-MOVE-WS-TO-SCR
187900     THRU    800-END.
188000
188100     MOVE 114                        TO CRT-ERROR-NBR.
188200     MOVE HR11F1-FC-FN               TO CRT-FIELD-NBR.
188300
188400 725-END.
      ******************************************************************
       750-CHECK-SECURITY.
      ******************************************************************

           MOVE HR11F1-EMP-COMPANY         TO DB-COMPANY.
           MOVE "PO"                       TO DB-TOPIC.
           PERFORM
               VARYING I9 FROM 1 BY 1
               UNTIL  (I9 > PAPOSEMP-TABLE-SIZE)
               OR     (ERROR-FOUND)

               MOVE PAPOSEMP-POS-FLD (I9)  TO DB-FLD-NBR
               IF (HR11WS-USE-PROCESS-LEVEL)
                   MOVE POS-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
               ELSE
                   MOVE SPACES             TO DB-PROCESS-LEVEL
               END-IF
               PERFORM 840-FIND-PASSET2
               IF  (PASCRTY-FOUND)
               AND (PAS-DEFAULT-FLG > 1)
                   MOVE PAPOSEMP-EMP-FLD (I9)  TO DB-FLD-NBR
                   MOVE SPACES                 TO DB-PROCESS-LEVEL
                   PERFORM 840-FIND-PASSET1
                   IF  (PASCRTY-FOUND)
                       MOVE PAS-SEC-LEVEL          TO HRWS-SEC-LEVEL
                       PERFORM 730-HR-FIELD-SECURITY
                       IF  (HRWS-FLD-SECURED)
                           MOVE 121                    TO CRT-ERROR-NBR
                           MOVE HR11F1-FC-FN           TO CRT-FIELD-NBR
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

       750-END.
189800******************************************************************
189900 775-LOAD-PADFP-FIELDS.
190000******************************************************************
190100
           MOVE HR11F1-EMP-COMPANY         TO PADT-COMPANY.
           MOVE HR11F1-EMP-EMPLOYEE        TO PADT-EMPLOYEE.
           MOVE 1                          TO PADT-POS-LEVEL.
           MOVE HR11F1-EFFECT-DATE         TO PADT-EFFECT-DATE.
           PERFORM 2300-PADT-DATE-CHECK.

190200     MOVE HR11F1-FC-FN               TO PADFP-FC-FN.
190300     MOVE HR11F1-EMP-COMPANY         TO PADFP-COMPANY.
190400     MOVE HR11F1-EFFECT-DATE         TO PADFP-EFFECT-DATE.
           IF  (HR11F1-EFFECT-DATE  < PADT-UPDPEP-DATE)
               IF  (HR11F1-EMP-POSITION   = PEP-POSITION)
               AND (PEP-POS-EFF-DT    NOT = ZEROES)
                   MOVE  PEP-POS-EFF-DT    TO PADFP-EFFECT-DATE.

191200     MOVE HR11F1-EMP-TERM-DATE       TO PADFP-END-DATE.
           IF  (PADFP-EFFECT-DATE  > PADFP-END-DATE)
           AND (PADFP-END-DATE     NOT = ZEROES)
               MOVE PADFP-EFFECT-DATE      TO PADFP-END-DATE.
191300     MOVE HR11F1-EMP-POSITION        TO PADFP-POSITION.
191400     MOVE HR11F1-EMP-POSITION-FN     TO PADFP-POSITION-FN.
191500     MOVE HR11F1-EMP-JOB-CODE        TO PADFP-JOB-CODE.
191600     MOVE HR11F1-EMP-JOB-CODE-FN     TO PADFP-JOB-CODE-FN.
191700     MOVE HR11F1-EMP-PROCESS-LEVEL   TO PADFP-PROCESS-LEVEL.
191800     MOVE HR11F1-EMP-PROCESS-LEVEL-FN
191900                                     TO PADFP-PROCESS-LEVEL-FN.
           MOVE HR11F1-EMP-CURR-CODE       TO PADFP-CURRENCY-CODE.
192000     MOVE HR11F1-EMP-DEPARTMENT      TO PADFP-DEPARTMENT.
192100     MOVE HR11F1-EMP-DEPARTMENT-FN   TO PADFP-DEPARTMENT-FN.
192200     MOVE HR11F1-EMP-USER-LEVEL      TO PADFP-USER-LEVEL.
192300     MOVE HR11F1-EMP-USER-LEVEL-FN   TO PADFP-USER-LEVEL-FN.
192400     MOVE HR11F1-EMP-SEC-LVL         TO PADFP-SEC-LVL.
192500     MOVE HR11F1-EMP-SEC-LVL-FN      TO PADFP-SEC-LVL-FN.
192600     MOVE HR11F1-EMP-SEC-LOCATION    TO PADFP-SEC-LOCATION.
192700     MOVE HR11F1-EMP-SEC-LOCATION-FN TO PADFP-SEC-LOCATION-FN.
192800     MOVE HR11F1-EMP-SALARY-CLASS    TO PADFP-SALARY-CLASS.
192900     MOVE HR11F1-EMP-SALARY-CLASS-FN TO PADFP-SALARY-CLASS-FN.
193000     MOVE HR11F1-EMP-SCHEDULE        TO PADFP-SCHEDULE.
193100     MOVE HR11F1-EMP-SCHEDULE-FN     TO PADFP-SCHEDULE-FN.
193200     MOVE HR11F1-EMP-PAY-GRADE       TO PADFP-PAY-GRADE.
193300     MOVE HR11F1-EMP-PAY-GRADE-FN    TO PADFP-PAY-GRADE-FN.
193400     MOVE HR11F1-EMP-PAY-STEP        TO PADFP-PAY-STEP.
193500     MOVE HR11F1-EMP-PAY-STEP-FN     TO PADFP-PAY-STEP-FN.
193600     MOVE HR11F1-EMP-WORK-SCHED      TO PADFP-WORK-SCHED.
193700     MOVE HR11F1-EMP-WORK-SCHED-FN   TO PADFP-WORK-SCHED-FN.
193800     MOVE HR11F1-EMP-ANNUAL-HOURS    TO PADFP-ANNUAL-HOURS.
193900     MOVE HR11F1-EMP-ANNUAL-HOURS-FN TO PADFP-ANNUAL-HOURS-FN.
194000     MOVE HR11F1-EMP-SUPERVISOR      TO PADFP-SUPERVISOR.
194100     MOVE HR11F1-EMP-SUPERVISOR-FN   TO PADFP-SUPERVISOR-FN.
194200     MOVE HR11F1-EMP-SUPER-IND       TO PADFP-SUPERVISOR-IND.
194300     MOVE HR11F1-EMP-SUPER-IND-FN    TO PADFP-SUPERVISOR-IND-FN.
194400     MOVE HR11F1-EMP-SHIFT           TO PADFP-SHIFT.
194500     MOVE HR11F1-EMP-SHIFT-FN        TO PADFP-SHIFT-FN.
194600     MOVE HR11F1-PEM-LOCAT-CODE      TO PADFP-LOCAT-CODE.
194700     MOVE HR11F1-PEM-LOCAT-CODE-FN   TO PADFP-LOCAT-CODE-FN.
194600     MOVE HR11F1-EMP-UNION-CODE      TO PADFP-UNION-CODE.
194700     MOVE HR11F1-EMP-UNION-CODE-FN   TO PADFP-UNION-CODE-FN.
194600     MOVE HR11F1-PEM-BARGAIN-UNIT    TO PADFP-BARGAIN-UNIT.
194700     MOVE HR11F1-PEM-BARGAIN-UNIT-FN TO PADFP-BARGAIN-UNIT-FN.
194800     MOVE HR11F1-EMP-EXEMPT-EMP      TO PADFP-EXEMPT-EMP.
194900     MOVE HR11F1-EMP-EXEMPT-EMP-FN   TO PADFP-EXEMPT-EMP-FN.
195000     MOVE HR11F1-EMP-HM-DIST-CO      TO PADFP-EXP-COMPANY.
195100     MOVE HR11F1-EMP-HM-DIST-CO-FN   TO PADFP-EXP-COMPANY-FN.
195200     MOVE HR11F1-EMP-HM-ACCT-UNIT    TO PADFP-EXP-ACCT-UNIT.
195300     MOVE HR11F1-EMP-HM-ACCT-UNIT-FN TO PADFP-EXP-ACCT-UNIT-FN.
195400     MOVE HR11F1-EMP-HM-ACCOUNT      TO PADFP-EXP-ACCOUNT.
195500     MOVE HR11F1-EMP-HM-ACCOUNT-FN   TO PADFP-EXP-ACCOUNT-FN.
195600     MOVE HR11F1-EMP-HM-SUB-ACCT     TO PADFP-EXP-SUB-ACCT.
195700     MOVE HR11F1-EMP-HM-SUB-ACCT-FN  TO PADFP-EXP-SUB-ACCT-FN.
195800     MOVE HR11F1-EMP-PAY-RATE        TO PADFP-PAY-RATE.
195900     MOVE HR11F1-EMP-PAY-RATE-FN     TO PADFP-PAY-RATE-FN.
           MOVE HR11F1-EMP-NBR-FTE         TO PADFP-FTE.
196000     MOVE HR11F1-EMP-PAY-FREQUENCY   TO PADFP-PAY-FREQUENCY.
196100     MOVE HR11F1-EMP-PAY-FREQUENCY-FN
196200                                     TO PADFP-PAY-FREQUENCY-FN.
196300     MOVE HR11F1-EMP-ACTIVITY        TO PADFP-ACTIVITY.
196400     MOVE HR11F1-EMP-ACTIVITY-FN     TO PADFP-ACTIVITY-FN.
196500     MOVE HR11F1-EMP-ACCT-CATEGORY   TO PADFP-ACCT-CATEGORY.
196600     MOVE HR11F1-EMP-ACCT-CATEGORY-FN
196700                                     TO PADFP-ACCT-CATEGORY-FN.
196800     MOVE HR11F1-EMP-OT-PLAN-CODE    TO PADFP-OT-PLAN-CODE.
196900     MOVE HR11F1-EMP-OT-PLAN-CODE-FN TO PADFP-OT-PLAN-CODE-FN.
197000
197100 775-END.
197200******************************************************************
197300 800-MOVE-WS-TO-SCR.
197400******************************************************************
197500
197600     IF (HR11F1-EMP-POSITION NOT = SPACES)
197700         MOVE PAPOS-DESCRIPTION      TO HR11F1-POS-DESCRIPTION.
197800
197900     MOVE PADFP-JOB-CODE             TO HR11F1-EMP-JOB-CODE.
198000     INITIALIZE                         HR11F1-JBC-DESCRIPTION.
198100     IF (HR11F1-EMP-JOB-CODE NOT = SPACES)
198200         MOVE HR11F1-EMP-COMPANY     TO DB-COMPANY
198300         MOVE HR11F1-EMP-JOB-CODE    TO DB-JOB-CODE
198400         PERFORM 840-FIND-JBCSET1
198500         IF (JOBCODE-FOUND)
198600             MOVE JBC-DESCRIPTION    TO HR11F1-JBC-DESCRIPTION.
198700
198800     MOVE PADFP-PROCESS-LEVEL        TO HR11F1-EMP-PROCESS-LEVEL.
198900     INITIALIZE                         HR11F1-PRS1-NAME.
199000     IF (HR11F1-EMP-PROCESS-LEVEL NOT = SPACES)
199100         MOVE HR11F1-EMP-PROCESS-LEVEL
199200                                     TO DB-PROCESS-LEVEL
199300         PERFORM 840-FIND-PRSSET1
199400         IF (PRSYSTEM-FOUND)
199500             MOVE PRS-NAME           TO HR11F1-PRS1-NAME.
199600
199700     MOVE PADFP-DEPARTMENT           TO HR11F1-EMP-DEPARTMENT.
199800     INITIALIZE                         HR11F1-DPT-NAME.
199900     IF (HR11F1-EMP-DEPARTMENT NOT = SPACES)
200000         MOVE HR11F1-EMP-COMPANY     TO DB-COMPANY
200100         MOVE HR11F1-EMP-PROCESS-LEVEL
200200                                     TO DB-PROCESS-LEVEL
200300         MOVE HR11F1-EMP-DEPARTMENT  TO DB-DEPARTMENT
200400         PERFORM 840-FIND-DPTSET1
200500         IF (DEPTCODE-FOUND)
200600             MOVE DPT-NAME           TO HR11F1-DPT-NAME.
200700
200800     MOVE PADFP-USER-LEVEL           TO HR11F1-EMP-USER-LEVEL.
200900     INITIALIZE                         HR11F1-PCO-DESCRIPTION.
201000     IF (HR11F1-EMP-USER-LEVEL NOT = SPACES)
201100         MOVE "UL"                   TO DB-TYPE
201200         MOVE HR11F1-EMP-USER-LEVEL  TO DB-CODE
201300         PERFORM 840-FIND-PCOSET1
201400         IF (PCODES-FOUND)
201500             MOVE PCO-DESCRIPTION    TO HR11F1-PCO-DESCRIPTION.
201600
201700     MOVE PADFP-SALARY-CLASS         TO HR11F1-EMP-SALARY-CLASS.
201800
201900     MOVE PADFP-SCHEDULE             TO HR11F1-EMP-SCHEDULE.
202000     MOVE PADFP-PAY-GRADE            TO HR11F1-EMP-PAY-GRADE.
202100     MOVE PADFP-PAY-STEP             TO HR11F1-EMP-PAY-STEP.
202200     MOVE PADFP-SUPERVISOR           TO HR11F1-EMP-SUPERVISOR.
202300
202400     INITIALIZE                         HR11F1-SUP-NAME.
202500     IF (HR11F1-EMP-SUPERVISOR NOT = SPACES)
202600         MOVE HR11F1-EMP-COMPANY     TO DB-COMPANY
202700         MOVE HR11F1-EMP-SUPERVISOR  TO DB-CODE
202800         PERFORM 840-FIND-HSUSET1
202900         IF  (HRSUPER-FOUND)
203000         AND (HSU-EMPLOYEE NOT = ZEROS)
203100             MOVE HSU-EMPLOYEE       TO DB-EMPLOYEE
203200             MOVE ZEROS              TO DB-FLD-NBR
203300             PERFORM 850-FIND-NLT-PTFSET1
203400             MOVE PTF-LAST-NAME      TO HRWS-LAST-NAME
203500             MOVE PTF-FIRST-NAME     TO HRWS-FIRST-NAME
203600             MOVE PTF-MIDDLE-INIT    TO HRWS-MIDDLE-INIT
203700             PERFORM 750-HR-FORMAT-NAME
203800             MOVE HRWS-FORMAT-NAME   TO HR11F1-SUP-NAME.
203900
204000     MOVE PADFP-SUPERVISOR-IND       TO HR11F1-EMP-SUPER-IND.
204100
204200     INITIALIZE                         HR11F1-IND-SUP-NAME.
204300     IF (HR11F1-EMP-SUPERVISOR NOT = SPACES)
204400         MOVE HR11F1-EMP-COMPANY     TO DB-COMPANY
204500         MOVE HR11F1-EMP-SUPER-IND   TO DB-CODE
204600         PERFORM 840-FIND-HSUSET1
204700         IF  (HRSUPER-FOUND)
204800         AND (HSU-EMPLOYEE NOT = ZEROS)
204900             MOVE HSU-EMPLOYEE       TO DB-EMPLOYEE
205000             MOVE ZEROS              TO DB-FLD-NBR
205100             PERFORM 850-FIND-NLT-PTFSET1
205200             MOVE PTF-LAST-NAME      TO HRWS-LAST-NAME
205300             MOVE PTF-FIRST-NAME     TO HRWS-FIRST-NAME
205400             MOVE PTF-MIDDLE-INIT    TO HRWS-MIDDLE-INIT
205500             PERFORM 750-HR-FORMAT-NAME
205600             MOVE HRWS-FORMAT-NAME   TO HR11F1-IND-SUP-NAME.
205700
205800     MOVE PADFP-SHIFT                TO HR11F1-EMP-SHIFT.
205900
206000     MOVE PADFP-LOCAT-CODE           TO HR11F1-PEM-LOCAT-CODE.
206100     INITIALIZE                         HR11F1-PCO1-DESCRIPTION.
206200     IF (HR11F1-PEM-LOCAT-CODE NOT = SPACES)
206300         MOVE "LO"                   TO DB-TYPE
206400         MOVE HR11F1-PEM-LOCAT-CODE  TO DB-CODE
206500         PERFORM 840-FIND-PCOSET1
206600             IF (PCODES-FOUND)
206700                 MOVE PCO-DESCRIPTION
                                           TO HR11F1-PCO1-DESCRIPTION.
206800
206000     MOVE PADFP-UNION-CODE           TO HR11F1-EMP-UNION-CODE.
P83060     INITIALIZE                         HR11F1-UNION-CODE-DESC.
P83060*    INITIALIZE                         HR11F1-PCO1-DESCRIPTION.
206200     IF (HR11F1-EMP-UNION-CODE NOT = SPACES)
206300         MOVE "UN"                   TO DB-TYPE
206400         MOVE HR11F1-EMP-UNION-CODE  TO DB-CODE
206500         PERFORM 840-FIND-PCOSET1
206600             IF (PCODES-FOUND)
206700                 MOVE PCO-DESCRIPTION
P83060                                     TO HR11F1-UNION-CODE-DESC.
P83060*                                    TO HR11F1-PCO1-DESCRIPTION.
206800
206000     MOVE PADFP-BARGAIN-UNIT         TO HR11F1-PEM-BARGAIN-UNIT.
P83060     INITIALIZE                         HR11F1-PCO3-DESCRIPTION.
P83060*    INITIALIZE                         HR11F1-PCO1-DESCRIPTION.
206200     IF (HR11F1-PEM-BARGAIN-UNIT NOT = SPACES)
206300         MOVE "BU"                   TO DB-TYPE
206400         MOVE HR11F1-PEM-BARGAIN-UNIT
206400                                     TO DB-CODE
206500         PERFORM 840-FIND-PCOSET1
206600             IF (PCODES-FOUND)
206700                 MOVE PCO-DESCRIPTION
P83060                                     TO HR11F1-PCO3-DESCRIPTION.
P83060*                                    TO HR11F1-PCO1-DESCRIPTION.
206800
206900     MOVE PADFP-WORK-SCHED           TO HR11F1-EMP-WORK-SCHED.
207000     MOVE PADFP-EXEMPT-EMP           TO HR11F1-EMP-EXEMPT-EMP.
207100     MOVE PADFP-EXP-COMPANY          TO HR11F1-EMP-HM-DIST-CO.
207200     MOVE PADFP-EXP-ACCT-UNIT        TO HR11F1-EMP-HM-ACCT-UNIT.
207300     MOVE PADFP-EXP-ACCOUNT          TO HR11F1-EMP-HM-ACCOUNT.
207400     MOVE PADFP-EXP-SUB-ACCT         TO HR11F1-EMP-HM-SUB-ACCT.
207500     MOVE PADFP-PAY-RATE             TO HR11F1-EMP-PAY-RATE.
207600     MOVE PADFP-PAY-FREQUENCY        TO HR11F1-EMP-PAY-FREQUENCY.
207700     MOVE PADFP-ACTIVITY             TO HR11F1-EMP-ACTIVITY.
207800     MOVE PADFP-ACCT-CATEGORY        TO HR11F1-EMP-ACCT-CATEGORY.
207900     MOVE PADFP-OT-PLAN-CODE         TO HR11F1-EMP-OT-PLAN-CODE.
           IF (HR11F1-EMP-NBR-FTE = ZEROES)
208000         MOVE PADFP-FTE              TO HR11F1-EMP-NBR-FTE.
208100     MOVE PADFP-SEC-LVL              TO HR11F1-EMP-SEC-LVL.
208200     MOVE PADFP-SEC-LOCATION         TO HR11F1-EMP-SEC-LOCATION.
208300     MOVE PADFP-ANNUAL-HOURS         TO HR11F1-EMP-ANNUAL-HOURS.
           MOVE PADFP-CURRENCY-CODE        TO HR11F1-EMP-CURR-CODE
                                              HR11F1-EMP-CURR-CODE-2
                                              HR11F1-EMP-CURR-CODE-3
                                              HR11F1-EMP-CURR-CODE-4.
208400
208500 800-END.
208600
208700******************************************************************
208800 HR11S1-TRANSACTION-END.
208900******************************************************************
ACS001
ACS001     PERFORM 1000-OPEN-WORKFLOW-DB.
ACS001     
ACS001     INITIALIZE WFAPI-I-SERVICE.
ACS001
ACS001     MOVE HR11F1-EMP-COMPANY        TO WFAPI-CRITERION-1.
ACS001     MOVE "EMPLOYEE"                TO WFAPI-I-VARIABLE-NAME (1).
ACS001     MOVE HR11F1-EMP-EMPLOYEE       TO WFAPI-I-VARIABLE-VAL  (1).
ACS001     MOVE "COMPANY"                 TO WFAPI-I-VARIABLE-NAME (2).
ACS001     MOVE HR11F1-EMP-COMPANY        TO WFAPI-I-VARIABLE-VAL  (2).
ACS001     MOVE "PROCESS_LEVEL"           TO WFAPI-I-VARIABLE-NAME (3).
ACS001     MOVE HR11F1-EMP-PROCESS-LEVEL  TO WFAPI-I-VARIABLE-VAL  (3).
ACS001     MOVE "DEPARTMENT"              TO WFAPI-I-VARIABLE-NAME (4).
ACS001     MOVE HR11F1-EMP-DEPARTMENT     TO WFAPI-I-VARIABLE-VAL  (4).
ACS001
ACS001     PERFORM 1000-PROCESS-FLOW.
ACS001
000400******************************************************************
000500 HR11S2-TRANSACTION              SECTION 24.
000600******************************************************************
000700 HR11S2-START.

002700     PERFORM 200-EDIT-TRAN
002800     THRU    200-END.
002900
001200     IF (NO-ERROR-FOUND)
               IF (HR11F2-PASSED-FROM-HRPA = "HR")
                   PERFORM 400-PROCESS-TRAN
                   THRU    400-END
               ELSE
                   PERFORM 500-PROCESS-TRAN
                   THRU    500-END
               END-IF.

           MOVE CRT-RETURN-KNS       TO CRT-REQUEST.
             
           GO TO HR11S2-TRANSACTION-END.

003600******************************************************************
003700 200-EDIT-TRAN.
003800******************************************************************
003900
           IF (HR11F2-EMP-COMPANY = ZEROES)
               MOVE 134                    TO CRT-ERROR-NBR
               MOVE HR11F2-EMP-COMPANY     TO CRT-FIELD-NBR
               GO TO 200-END
           ELSE
               IF (PRSYSTEM-NOTFOUND)
               OR (PRS-COMPANY       NOT = EMP-COMPANY)
               OR (PRS-PROCESS-LEVEL NOT = SPACES)
                   MOVE EMP-COMPANY        TO DB-COMPANY
                   INITIALIZE                 DB-PROCESS-LEVEL
                   PERFORM 840-FIND-PRSSET1.

           IF (HR11F2-EMP-EMPLOYEE = ZEROES)
               MOVE 135                    TO CRT-ERROR-NBR
               MOVE HR11F2-EMP-EMPLOYEE    TO CRT-FIELD-NBR
               GO TO 200-END.

           IF (HR11F2-PASSED-FROM-HRPA = "HR")
               MOVE HR11F2-EMP-COMPANY     TO HREMP-COMPANY
               MOVE HR11F2-EMP-EMPLOYEE    TO HREMP-EMPLOYEE
               MOVE "I"                    TO HREMP-FC
               PERFORM 2000-HREMP-EDIT-TRAN
               INITIALIZE                     CRT-MESSAGE
           ELSE
               MOVE HR11F2-EMP-COMPANY     TO PAPEP-COMPANY
               MOVE HR11F2-EMP-EMPLOYEE    TO PAPEP-EMPLOYEE
               MOVE "I"                    TO PAPEP-FC
               MOVE HR11F2-POS-LEVEL       TO PAPEP-POS-LEVEL
               MOVE HR11F2-POS-EFFECT-DATE TO PAPEP-EFFECT-DATE
               PERFORM 2000-PAPEP-EDIT-TRAN
               INITIALIZE                     CRT-MESSAGE
           END-IF.    

008100 200-END.
008200
063300******************************************************************
063400 400-PROCESS-TRAN.
063500******************************************************************
063600
           MOVE HREMP-EMPLOYEE             TO HR11F2-EMP-EMPLOYEE.
           MOVE HREMP-FULL-NAME            TO HR11F2-EMP-SHORT-NAME.
           IF (HREMP-JOB-CODE NOT = SPACES)
               MOVE HREMP-COMPANY             TO DB-COMPANY
               MOVE HREMP-JOB-CODE            TO DB-JOB-CODE
               PERFORM 840-FIND-JBCSET1.
           MOVE WS-SYSTEM-DATE-YMD         TO HR11F2-EFFECT-DATE.
           MOVE 1                          TO HR11F2-EMP-POS-LEVEL.
           MOVE HREMP-SALARY-CLASS         TO HR11F2-EMP-SALARY-CLASS.
           MOVE HREMP-PAY-RATE             TO HR11F2-EMP-PAY-RATE
                                              HR11F2-BASE-PAY-RATE.
           MOVE HREMP-SCHEDULE             TO HR11F2-EMP-SCHEDULE.
           MOVE HREMP-PAY-GRADE            TO HR11F2-EMP-PAY-GRADE.
           MOVE HREMP-PAY-STEP             TO HR11F2-EMP-PAY-STEP.
           IF (HREMP-PAY-STEP NOT = ZEROS)
               MOVE EMP-SCHEDULE           TO DB-SCHEDULE
               MOVE EMP-PAY-GRADE          TO DB-PAY-GRADE
               MOVE HREMP-PAY-STEP         TO DB-PAY-STEP
               MOVE WS-SYSTEM-DATE-YMD     TO DB-EFFECT-DATE
               PERFORM 850-FIND-NLT-SGDSET3
               IF  (PRSAGDTL-FOUND)
               AND (SGD-COMPANY   = DB-COMPANY)
               AND (SGD-SCHEDULE  = DB-SCHEDULE)
               AND (SGD-PAY-GRADE = DB-PAY-GRADE)
               AND (SGD-PAY-STEP  = DB-PAY-STEP)
                   MOVE SGD-PAY-RATE       TO HR11F2-EMP-PAY-RATE
                                              HR11F2-BASE-PAY-RATE
               END-IF
           ELSE
           IF  (HREMP-JOB-CODE NOT = SPACES)
           AND (JOBCODE-FOUND)
               IF (HREMP-SCHEDULE = SPACES)
                   MOVE JBC-SCHEDULE        TO HR11F2-EMP-SCHEDULE
               END-IF
           END-IF
           IF  (HREMP-JOB-CODE NOT = SPACES)
           AND (JOBCODE-FOUND)
               IF (HREMP-PAY-GRADE = SPACES)
                   MOVE JBC-PAY-GRADE       TO HR11F2-EMP-PAY-GRADE
               END-IF
           END-IF.

           MOVE HREMP-CURRENCY-CODE         TO HR11F2-EMP-CURR-CODE
                                               HR11F2-BASE-CURRENCY.
           IF (HREMP-CURRENCY-CODE NOT = SPACES)
               MOVE HREMP-CURRENCY-CODE     TO DB-CURRENCY-CODE
               PERFORM 840-FIND-CUCSET1
               IF (CUCODES-FOUND)
                   MOVE CUC-FORMS-EXP      TO HR11F2-CURR-CODE-DESC
                                              HR11F2-PRO-RATE-CURR-DESC
                                              HR11F2-TPRO-RATE-CURR-DESC
               ELSE
                   INITIALIZE                HR11F2-CURR-CODE-DESC
                                             HR11F2-PRO-RATE-CURR-DESC
                                             HR11F2-TPRO-RATE-CURR-DESC.

           MOVE HREMP-ANNUAL-HOURS          TO HR11F2-EMP-ANNUAL-HOURS.
           MOVE ZEROES                      TO HR11F2-DAILY-SCHED-HOURS.
           IF (HREMP-WORK-SCHED NOT = SPACES)
               MOVE HREMP-COMPANY           TO DB-COMPANY
               MOVE HREMP-WORK-SCHED        TO DB-WORK-SCHED
               MOVE ZEROES                  TO DB-CONTRACT-YEAR
               MOVE WS-SYSTEM-DATE-YMD      TO DB-EFFECT-DATE
               PERFORM 850-FIND-NLT-WSCSET1
               IF (HRWRKSCHD-NOTFOUND)
               OR (WSC-COMPANY    NOT = DB-COMPANY)
               OR (WSC-WORK-SCHED NOT = DB-WORK-SCHED)
                   MOVE 9999               TO DB-CONTRACT-YEAR
                   MOVE ZEROES             TO DB-EFFECT-DATE
                   PERFORM 850-FIND-NLT-WSCSET1
               END-IF
               IF  (HRWRKSCHD-FOUND)
               AND (WSC-COMPANY    = DB-COMPANY)
               AND (WSC-WORK-SCHED = DB-WORK-SCHED)
               AND (WSC-TOT-DAYS   NOT = ZEROES)
                   COMPUTE HR11F2-DAILY-SCHED-HOURS = 
                      (HREMP-ANNUAL-HOURS / WSC-TOT-DAYS)
               END-IF
           END-IF.
           MOVE HREMP-NBR-FTE               TO HR11F2-EMP-NBR-FTE.
           MOVE HREMP-JOB-CODE              TO HR11F2-EMP-JOB-CODE.
           MOVE HREMP-CURR-ND          TO HR11F2-EMP-PRO-RATE-A-SAL-ND.
           MOVE HREMP-PRO-RATE-A-SAL   TO HR11F2-EMP-PRO-RATE-A-SAL.
           MOVE HREMP-CURR-ND          TO HR11F2-EMP-PRO-RATE-TOTAL-ND.
           MOVE HREMP-PRO-RATE-TOTAL   TO HR11F2-EMP-PRO-RATE-TOTAL.
           MOVE "N"                         TO HR11F2-CHK-DECIMALS.
           MOVE "Y"                         TO HR11F2-PR-FLAG.
           IF  (HREMP-JOB-CODE NOT = SPACES)
           AND (JOBCODE-FOUND)
               MOVE JBC-DESCRIPTION         TO HR11F2-JBC-DESCRIPTION
               MOVE JBC-CURR-ND        TO HR11F2-JBC-MARKET-SALARY-ND
               MOVE JBC-MARKET-SALARY       TO HR11F2-JBC-MARKET-SALARY
               MOVE JBC-POINTS              TO HR11F2-JBC-POINTS
               INITIALIZE                      HR11F2-JBC-CURRENCY-CODE
                                               HR11F2-MKT-SAL-CURR-DESC
               IF  (JBC-MARKET-SALARY NOT = ZEROES)
               AND (JBC-CURRENCY-CODE NOT = SPACES)
                   MOVE JBC-CURRENCY-CODE   TO HR11F2-JBC-CURRENCY-CODE
                                               DB-CURRENCY-CODE
                   PERFORM 840-FIND-CUCSET1
                   IF (CUCODES-FOUND)
                       MOVE CUC-FORMS-EXP   TO HR11F2-MKT-SAL-CURR-DESC
                   ELSE
                       INITIALIZE              HR11F2-MKT-SAL-CURR-DESC
                   END-IF
               END-IF
           ELSE
               INITIALIZE                      HR11F2-JBC-DESCRIPTION
                                               HR11F2-JBC-MARKET-SALARY
                                               HR11F2-JBC-CURRENCY-CODE
                                               HR11F2-MKT-SAL-CURR-DESC.

           IF (HR11F2-EMP-PAY-STEP  NOT = ZEROES)
               GO TO 400-END.

           INITIALIZE                          HR11F2-MINIMUM
                                               HR11F2-MIDPOINT
                                               HR11F2-MAXIMUM
                                               HR11F2-MINIMUM-CURRENCY
                                               HR11F2-COMP-RATIO
                                               HR11F2-PERCENT-OF-RANGE.

089800     MOVE HR11F2-EMP-COMPANY          TO HRCALC-EMP-COMPANY.
089900     MOVE HR11F2-EMP-SCHEDULE         TO HRCALC-EMP-SCHEDULE.
090000     MOVE HR11F2-EMP-PAY-GRADE        TO HRCALC-EMP-PAY-GRADE.
           MOVE HR11F2-EFFECT-DATE          TO HRCALC-EMP-EFFECT-DATE.
           MOVE HR11F2-EMP-SALARY-CLASS     TO HRCALC-EMP-SALARY-CLASS.
           MOVE HR11F2-EMP-PAY-STEP         TO HRCALC-EMP-PAY-STEP.
           MOVE HR11F2-EMP-JOB-CODE         TO HRCALC-EMP-JOB-CODE.
           MOVE HR11F2-EMP-PAY-RATE         TO HRCALC-EMP-PAY-RATE.
           MOVE HR11F2-EMP-ANNUAL-HOURS     TO HRCALC-EMP-ANNUAL-HOURS.
           MOVE HR11F2-EMP-CURR-CODE        TO HRCALC-EMP-CURRENCY-CODE.
           MOVE HR11F2-EMP-NBR-FTE          TO HRCALC-EMP-FTE.
           PERFORM 5000-CALC-COMPA-AND-RANGE-73.
           IF (NO-ERROR-FOUND)
155400         MOVE HRCALC-CLC-BEG-SAL-RANGE
155400                                      TO HR11F2-MINIMUM
155400         MOVE HRCALC-CLC-MID-SAL-RANGE
155400                                      TO HR11F2-MIDPOINT
               MOVE HRCALC-CLC-END-SAL-RANGE
                                            TO HR11F2-MAXIMUM
               MOVE HRCALC-CLC-CURRENCY-CODE
                                            TO HR11F2-MINIMUM-CURRENCY
               MOVE HRCALC-CLC-COMPA-RATIO  TO HR11F2-COMP-RATIO
               MOVE HRCALC-CLC-PCT-OF-RANGE TO HR11F2-PERCENT-OF-RANGE
               MOVE HRCALC-CLC-ANNUAL-HOURS TO HR11F2-EMP-ANNUAL-HOURS
           END-IF.

           IF (HR11F2-MINIMUM-CURRENCY NOT = SPACES)
               MOVE HR11F2-MINIMUM-CURRENCY TO DB-CURRENCY-CODE
               PERFORM 840-FIND-CUCSET1
               IF (CUCODES-FOUND)
                   MOVE CUC-FORMS-EXP       TO HR11F2-MINIMUM-CURR-DESC
               ELSE
                   INITIALIZE                  HR11F2-MINIMUM-CURR-DESC.

065900 400-END.

063300******************************************************************
063400 500-PROCESS-TRAN.
063500******************************************************************
063600
           MOVE HREMP-EMPLOYEE             TO HR11F2-EMP-EMPLOYEE.
           MOVE HREMP-FULL-NAME            TO HR11F2-EMP-SHORT-NAME.
           IF (PAPEP-JOB-CODE NOT = SPACES)
               MOVE HREMP-COMPANY          TO DB-COMPANY
               MOVE PAPEP-JOB-CODE         TO DB-JOB-CODE
               PERFORM 840-FIND-JBCSET1.
           MOVE WS-SYSTEM-DATE-YMD         TO HR11F2-EFFECT-DATE.
           MOVE PAPEP-POS-LEVEL            TO HR11F2-EMP-POS-LEVEL.
           MOVE HREMP-SALARY-CLASS         TO HR11F2-EMP-SALARY-CLASS.
           MOVE PAPEP-PAY-RATE             TO HR11F2-EMP-PAY-RATE
                                              HR11F2-BASE-PAY-RATE.
           MOVE PAPEP-BASE-PAY-RATE        TO HR11F2-BASE-PAY-RATE.
           MOVE PAPEP-SCHEDULE             TO HR11F2-EMP-SCHEDULE.
           MOVE PAPEP-PAY-GRADE            TO HR11F2-EMP-PAY-GRADE.
           MOVE PAPEP-PAY-STEP             TO HR11F2-EMP-PAY-STEP.
           IF  (PAPEP-JOB-CODE NOT = SPACES)
           AND (JOBCODE-FOUND)
               IF (PAPEP-SCHEDULE = SPACES)
                   MOVE JBC-SCHEDULE       TO HR11F2-EMP-SCHEDULE
               END-IF
           END-IF.
           IF  (PAPEP-JOB-CODE NOT = SPACES)
           AND (JOBCODE-FOUND)
               IF (PAPEP-PAY-GRADE = SPACES)
                   MOVE JBC-PAY-GRADE      TO HR11F2-EMP-PAY-GRADE
               END-IF
           END-IF.

           MOVE PAPEP-CURRENCY-CODE        TO HR11F2-EMP-CURR-CODE.
           MOVE PAPEP-BASE-CURRENCY        TO HR11F2-BASE-CURRENCY.
           IF (PAPEP-CURRENCY-CODE NOT = SPACES)
               MOVE PAPEP-CURRENCY-CODE    TO DB-CURRENCY-CODE
               PERFORM 840-FIND-CUCSET1
               IF (CUCODES-FOUND)
                   MOVE CUC-FORMS-EXP      TO HR11F2-CURR-CODE-DESC
                                              HR11F2-PRO-RATE-CURR-DESC
               ELSE
                   INITIALIZE                 HR11F2-CURR-CODE-DESC
                                              HR11F2-PRO-RATE-CURR-DESC
               END-IF.

           IF (PAPEP-CURRENCY-CODE = PAPEP-BASE-CURRENCY)
               MOVE CUC-FORMS-EXP          TO HR11F2-TPRO-RATE-CURR-DESC
           ELSE
               MOVE PAPEP-BASE-CURRENCY    TO DB-CURRENCY-CODE
               PERFORM 840-FIND-CUCSET1
               IF (CUCODES-FOUND)
                   MOVE CUC-FORMS-EXP      TO HR11F2-TPRO-RATE-CURR-DESC
               ELSE
                   INITIALIZE                 HR11F2-TPRO-RATE-CURR-DESC
               END-IF
           END-IF.

           MOVE PAPEP-ANNUAL-HOURS         TO HR11F2-EMP-ANNUAL-HOURS.
           MOVE PAPEP-NBR-FTE              TO HR11F2-EMP-NBR-FTE.
           MOVE PAPEP-JOB-CODE             TO HR11F2-EMP-JOB-CODE.
           MOVE PAPEP-CURR-ND          TO HR11F2-EMP-PRO-RATE-A-SAL-ND.
           MOVE PAPEP-PRO-RATE-A-SAL   TO HR11F2-EMP-PRO-RATE-A-SAL.
           MOVE HREMP-CURR-ND          TO HR11F2-EMP-PRO-RATE-TOTAL-ND.
           MOVE HREMP-PRO-RATE-TOTAL   TO HR11F2-EMP-PRO-RATE-TOTAL.
           MOVE "N"                        TO HR11F2-CHK-DECIMALS.
           MOVE "Y"                        TO HR11F2-PR-FLAG.
           IF  (PAPEP-JOB-CODE NOT = SPACES)
           AND (JOBCODE-FOUND)
               MOVE JBC-DESCRIPTION        TO HR11F2-JBC-DESCRIPTION
               MOVE JBC-CURR-ND        TO HR11F2-JBC-MARKET-SALARY-ND
               MOVE JBC-MARKET-SALARY      TO HR11F2-JBC-MARKET-SALARY
               MOVE JBC-POINTS             TO HR11F2-JBC-POINTS
               INITIALIZE                     HR11F2-JBC-CURRENCY-CODE
                                              HR11F2-MKT-SAL-CURR-DESC
               IF  (JBC-MARKET-SALARY NOT = ZEROES)
               AND (JBC-CURRENCY-CODE NOT = SPACES)
                   MOVE JBC-CURRENCY-CODE  TO HR11F2-JBC-CURRENCY-CODE
                                              DB-CURRENCY-CODE
                   PERFORM 840-FIND-CUCSET1
                   IF (CUCODES-FOUND)
                       MOVE CUC-FORMS-EXP  TO HR11F2-MKT-SAL-CURR-DESC
                   ELSE
                       INITIALIZE             HR11F2-MKT-SAL-CURR-DESC
                   END-IF
               END-IF
           ELSE
               INITIALIZE                     HR11F2-JBC-DESCRIPTION
                                              HR11F2-JBC-MARKET-SALARY
                                              HR11F2-JBC-CURRENCY-CODE
                                              HR11F2-MKT-SAL-CURR-DESC.

           IF (HR11F2-EMP-PAY-STEP  NOT = ZEROES)
               GO TO 500-END.

           INITIALIZE                         HR11F2-MINIMUM
                                              HR11F2-MIDPOINT
                                              HR11F2-MAXIMUM
                                              HR11F2-MINIMUM-CURRENCY
                                              HR11F2-COMP-RATIO
                                              HR11F2-PERCENT-OF-RANGE.

089800     MOVE HR11F2-EMP-COMPANY         TO HRCALC-EMP-COMPANY.
089900     MOVE HR11F2-EMP-SCHEDULE        TO HRCALC-EMP-SCHEDULE.
090000     MOVE HR11F2-EMP-PAY-GRADE       TO HRCALC-EMP-PAY-GRADE.
           MOVE HR11F2-EFFECT-DATE         TO HRCALC-EMP-EFFECT-DATE.
           MOVE HR11F2-EMP-SALARY-CLASS    TO HRCALC-EMP-SALARY-CLASS.
           MOVE HR11F2-EMP-PAY-STEP        TO HRCALC-EMP-PAY-STEP.
           MOVE HR11F2-EMP-JOB-CODE        TO HRCALC-EMP-JOB-CODE.
           MOVE HR11F2-EMP-PAY-RATE        TO HRCALC-EMP-PAY-RATE.
           MOVE HR11F2-EMP-ANNUAL-HOURS    TO HRCALC-EMP-ANNUAL-HOURS.
           MOVE HR11F2-EMP-CURR-CODE       TO HRCALC-EMP-CURRENCY-CODE.
           MOVE HR11F2-EMP-NBR-FTE         TO HRCALC-EMP-FTE.
           PERFORM 5000-CALC-COMPA-AND-RANGE-73.
           IF (NO-ERROR-FOUND)
155400         MOVE HRCALC-CLC-BEG-SAL-RANGE
155400                                     TO HR11F2-MINIMUM
155400         MOVE HRCALC-CLC-MID-SAL-RANGE
155400                                     TO HR11F2-MIDPOINT
               MOVE HRCALC-CLC-END-SAL-RANGE
                                           TO HR11F2-MAXIMUM
               MOVE HRCALC-CLC-CURRENCY-CODE
                                           TO HR11F2-MINIMUM-CURRENCY
               MOVE HRCALC-CLC-COMPA-RATIO  TO HR11F2-COMP-RATIO
               MOVE HRCALC-CLC-PCT-OF-RANGE TO HR11F2-PERCENT-OF-RANGE
               MOVE HRCALC-CLC-ANNUAL-HOURS TO HR11F2-EMP-ANNUAL-HOURS
           END-IF.

           IF (HR11F2-MINIMUM-CURRENCY NOT = SPACES)
               MOVE HR11F2-MINIMUM-CURRENCY TO DB-CURRENCY-CODE
               PERFORM 840-FIND-CUCSET1
               IF (CUCODES-FOUND)
                   MOVE CUC-FORMS-EXP       TO HR11F2-MINIMUM-CURR-DESC
               ELSE
                   INITIALIZE                  HR11F2-MINIMUM-CURR-DESC.

065900 500-END.

208700******************************************************************
208800 HR11S2-TRANSACTION-END.
208900******************************************************************
000400******************************************************************
000500 HR11S3-TRANSACTION              SECTION 10.
000600******************************************************************
000700 HR11S3-START.
000800
           PERFORM 200-EDIT-TRAN
           THRU    200-END.

001200     IF (NO-ERROR-FOUND)
001300         PERFORM 400-PROCESS-TRAN
001400         THRU    400-END.
001500
001600     GO TO HR11S3-TRANSACTION-END.
001700
      ******************************************************************
       200-EDIT-TRAN.
      ******************************************************************
      
           IF  (HR11F3-EMP-LAB-DIST-FLAG = "N")
           AND (HR11F3-EMP-EFFORT-FLAG   = "Y")
               MOVE 111                         TO CRT-ERROR-NBR
               MOVE HR11F3-EMP-LAB-DIST-FLAG-FN TO CRT-FIELD-NBR
               GO TO 200-END.

J54954     INITIALIZE CRT-ERROR-NBR.
J54954     INITIALIZE CRT-FIELD-NBR.
J54954
J54954     IF (HR11F3-EMP-ENCUMBER-FLAG = "N")
J54954         INITIALIZE HR11F3-EMP-FRNG-RATE               
J54954                    HR11F3-EMP-FRNG-ACCT-CAT
J54954                    HR11F3-EMP-FRNG-ACCOUNT
J54954                    HR11F3-EMP-FRNG-SUB-ACCT
J54954     END-IF.
J54954     IF (HR11F3-EMP-EFFORT-FLAG   = "N")
J54954         INITIALIZE HR11F3-EMP-PRMCERT-COMP
J54954                    HR11F3-EMP-PRMCERT-ID
J54954                    HR11F3-EMP-SNDCERT-COMP
J54954                    HR11F3-EMP-SNDCERT-ID
J54954                    HR11F3-PRIMARY-NAME
J54954                    HR11F3-SECONDARY-NAME
J54954     END-IF.
J54954
J54954*    Fringe Account Category validation with respect to file
J54954*    ACACCTCAT values
J54954
J54954     IF  (HR11F3-EMP-FRNG-ACCT-CAT NOT = SPACES)
J54954         MOVE HR11F3-EMP-FRNG-ACCT-CAT    TO DB-ACCT-CATEGORY
J54954         PERFORM 840-KFIND-AAXSET1
J54954         IF (ACACCTCAT-KNOTFOUND)
J54954*************Fringe Account Category is not valid
J54954             MOVE 169                             TO CRT-ERROR-NBR
J54954             MOVE HR11F3-EMP-FRNG-ACCT-CAT-FN     TO CRT-FIELD-NBR
J54954             GO TO 200-END
J54954         END-IF
J54954     END-IF.
J54954
J54954*  Fringe Account validation with respect to file
J54954*  GLCHARTDTL values
J54954
J54954     IF (HR11F3-EMP-FRNG-ACCOUNT  NOT = ZEROES)
J54954         SET HR11WS-INVALID-ACCT         TO TRUE
J54954         MOVE HR11F3-EMP-FRNG-ACCOUNT    TO DB-ACCOUNT
J54954         MOVE ZEROES                     TO DB-SUB-ACCOUNT
J54954         MOVE SPACES                     TO DB-CHART-NAME
J54954         PERFORM 850-FIND-NLT-GDTSET2
J54954         PERFORM
J54954         UNTIL (GLCHARTDTL-NOTFOUND)
J54954         OR    (HR11WS-VALID-ACCT)
J54954             IF  (HR11F3-EMP-FRNG-ACCOUNT   = GDT-ACCOUNT)
J54954             AND (HR11F3-EMP-FRNG-SUB-ACCT  = GDT-SUB-ACCOUNT)
J54954                 SET HR11WS-VALID-ACCT      TO TRUE
J54954             END-IF
J54954             PERFORM 860-FIND-NEXT-GDTSET2
J54954         END-PERFORM
J54954         IF (HR11WS-INVALID-ACCT)
J54954*************Fringe Account is not valid
J54954             MOVE 170                        TO CRT-ERROR-NBR
J54954             MOVE HR11F3-EMP-FRNG-ACCOUNT-FN TO CRT-FIELD-NBR
J54954             GO TO 200-END
J54954         END-IF
J54954     END-IF.
J54954
J54954     IF (HR11F3-EMP-FRNG-RATE < ZEROES)
J54954*********Fringe rate must be positive
J54954         MOVE 168                            TO CRT-ERROR-NBR
J54954         MOVE HR11F3-EMP-FRNG-RATE-FN        TO CRT-FIELD-NBR
J54954         GO TO 200-END
J54954     END-IF.
J54954
J54954     IF (HR11F3-EMP-FRNG-RATE     > ZEROES)
J54954         IF (HR11F3-EMP-FRNG-ACCT-CAT = SPACES)
J54954         OR (HR11F3-EMP-FRNG-ACCOUNT  = ZEROES)
J54954             IF (HR11F3-EMP-FRNG-ACCT-CAT = SPACES)
J54954*****Fringe Rate entered; fringe encumbrance account cat required
J54954                 MOVE 171                    TO CRT-ERROR-NBR
J54954                 MOVE HR11F3-EMP-FRNG-ACCT-CAT-FN
J54954                                             TO CRT-FIELD-NBR
J54954             ELSE
J54954*****Fringe Rate entered; fringe encumbrance account required
J54954                 MOVE 172                    TO CRT-ERROR-NBR
J54954                 MOVE HR11F3-EMP-FRNG-ACCOUNT-FN
J54954                                             TO CRT-FIELD-NBR
J54954             END-IF
J54954             GO TO 200-END
J54954         END-IF
J54954     END-IF.
J54954
J54954     IF (HR11F3-EMP-RPT-CURR NOT = SPACES)
J54954         MOVE HR11F3-EMP-RPT-CURR            TO DB-CURRENCY-CODE
J54954         PERFORM 840-KFIND-CUCSET1
J54954         IF (CUCODES-KNOTFOUND)
J54954*************Effort currency is not valid
J54954             MOVE 173                        TO CRT-ERROR-NBR
J54954             MOVE HR11F3-EMP-RPT-CURR-FN     TO CRT-FIELD-NBR
J54954             GO TO 200-END
J54954         END-IF
J54954     END-IF.
J54954
J54954     IF  (HR11F3-EMP-PRMCERT-COMP  NOT = ZEROES)
J54954     AND (HR11F3-EMP-PRMCERT-ID    NOT = ZEROES)
J54954         MOVE HR11F3-EMP-PRMCERT-COMP        TO DB-COMPANY
J54954         MOVE HR11F3-EMP-PRMCERT-ID          TO DB-EMPLOYEE
J54954         MOVE PTFSET1-EMPLOYEE               TO WS-DB-BEG-RNG
J54954         PERFORM 850-KFIND-BEGRNG-PTFSET1
J54954         IF (PATHFIND-KNOTFOUND)
J54954*************Primary Certifier is not a valid employee
J54954             MOVE 174                        TO CRT-ERROR-NBR
J54954             MOVE HR11F3-EMP-PRMCERT-ID-FN   TO CRT-FIELD-NBR
J54954             GO TO 200-END
J54954         END-IF
J54954     END-IF.
J54954
J54954     IF  (HR11F3-EMP-SNDCERT-COMP  NOT = ZEROES)
J54954     AND (HR11F3-EMP-SNDCERT-ID    NOT = ZEROES)
J54954         MOVE HR11F3-EMP-SNDCERT-COMP        TO DB-COMPANY
J54954         MOVE HR11F3-EMP-SNDCERT-ID          TO DB-EMPLOYEE
J54954         MOVE PTFSET1-EMPLOYEE               TO WS-DB-BEG-RNG
J54954         PERFORM 850-KFIND-BEGRNG-PTFSET1
J54954         IF (PATHFIND-KNOTFOUND)
J54954*************Secondary Certifier is not a valid employee
J54954             MOVE 175                        TO CRT-ERROR-NBR
J54954             MOVE HR11F3-EMP-SNDCERT-ID-FN TO CRT-FIELD-NBR
J54954             GO TO 200-END
J54954         END-IF
J54954     END-IF.
J54954
J54954     IF  (HR11F3-EMP-PRMCERT-COMP     = ZEROES)
J54954     AND (HR11F3-EMP-PRMCERT-ID   NOT = ZEROES)
J54954*********HR company required for effort certifier
J54954         MOVE 176                   TO CRT-ERROR-NBR
J54954         MOVE HR11F3-EMP-PRMCERT-COMP-FN TO CRT-FIELD-NBR
J54954         GO TO 200-END
J54954     END-IF.
J54954
J54954     IF  (HR11F3-EMP-SNDCERT-COMP     = ZEROES)
J54954     AND (HR11F3-EMP-SNDCERT-ID   NOT = ZEROES)
J54954*********HR company required for effort certifier
J54954         MOVE 176                        TO CRT-ERROR-NBR
J54954         MOVE HR11F3-EMP-SNDCERT-COMP-FN TO CRT-FIELD-NBR
J54954         GO TO 200-END
J54954     END-IF.
J54954
J54954     IF (HR11F3-EMP-PRMCERT-COMP  NOT = ZEROES)
J54954         MOVE HR11F3-EMP-PRMCERT-COMP     TO DB-COMPANY
J54954         MOVE PTFSET1-COMPANY             TO WS-DB-BEG-RNG
J54954         PERFORM 850-KFIND-BEGRNG-PTFSET1
J54954         IF (PATHFIND-KNOTFOUND)
J54954*************Company for primary certifier is invalid
J54954             MOVE 178                        TO CRT-ERROR-NBR
J54954             MOVE HR11F3-EMP-PRMCERT-COMP-FN      TO CRT-FIELD-NBR
J54954             GO TO 200-END
J54954         END-IF
J54954     END-IF.
J54954
J54954     IF (HR11F3-EMP-SNDCERT-COMP  NOT = ZEROES)
J54954         MOVE HR11F3-EMP-SNDCERT-COMP     TO DB-COMPANY
J54954         MOVE PTFSET1-COMPANY        TO WS-DB-BEG-RNG
J54954         PERFORM 850-KFIND-BEGRNG-PTFSET1
J54954         IF (PATHFIND-KNOTFOUND)
J54954*************Company for secondary certifier is invalid  
J54954             MOVE 179                        TO CRT-ERROR-NBR
J54954             MOVE HR11F3-EMP-SNDCERT-COMP-FN      TO CRT-FIELD-NBR
J54954             GO TO 200-END
J54954         END-IF
J54954     END-IF.
J54954
J54954     IF (HR11F3-EMP-EFFORT-FLAG = "Y")
J50169         INITIALIZE FILTER-STRING
J54954         STRING "((EFH-HR-COMPANY=""?"")"    DELIMITED BY SIZE
J54954         "AND"                               DELIMITED BY SIZE
J54954         "(EFH-EMPLOYEE=""?"")"              DELIMITED BY SIZE
J54954         "AND" DELIMITED BY SIZE
J54954         "(EFH-STATUS != ""?""))"            DELIMITED BY SIZE
J54954                                             INTO FILTER-STRING
J54954         PERFORM 890-CREATE-FILTER
J54954         MOVE HR11F3-EMP-COMPANY         TO ALPHANUM-FILTER-VALUE
J54954         PERFORM 890-SET-ALPHANUM-FILTER-VALUE
J54954         MOVE HR11F3-EMP-EMPLOYEE        TO ALPHANUM-FILTER-VALUE
J54954         PERFORM 890-SET-ALPHANUM-FILTER-VALUE
J54954         MOVE "4"                        TO ALPHANUM-FILTER-VALUE
J54954         PERFORM 890-SET-ALPHANUM-FILTER-VALUE
J54954         PERFORM 850-FILTER-NLT-EFHSET1
J54954         IF (GMEFFORT-FOUND)
J54954*********Effort Reporting is required; non certified records exist
J54954             MOVE 180                         TO CRT-ERROR-NBR
J54954             MOVE HR11F3-EMP-EFFORT-FLAG-FN   TO CRT-FIELD-NBR
J54954             GO TO 200-END
J54954         END-IF
J54954     END-IF.
J54954
J54954     IF (HR11F3-EMP-EFFORT-FLAG = "Y")
J54954         IF (HR11F3-EMP-PRMCERT-COMP = ZEROES)
J54954         OR (HR11F3-EMP-PRMCERT-ID   = ZEROES)
J54954************ Effort Reporting requires a Primary Certifier
J54954             MOVE 181                             TO CRT-ERROR-NBR
J54954             IF (HR11F3-EMP-PRMCERT-COMP = ZEROES)
J54954                 MOVE HR11F3-EMP-PRMCERT-COMP-FN  TO CRT-FIELD-NBR
J54954             ELSE
J54954                 MOVE HR11F3-EMP-PRMCERT-ID-FN    TO CRT-FIELD-NBR
J54954             END-IF
J54954             GO TO 200-END
J54954         ELSE
J54954        IF (HR11F3-EMP-RPT-CURR  = SPACES)
J54954************ Effort Reporting requires an Effort Currency
J54954             MOVE 177                             TO CRT-ERROR-NBR
J54954             MOVE HR11F3-EMP-RPT-CURR-FN          TO CRT-FIELD-NBR
J54954             GO TO 200-END                   
J54954         END-IF
J54954     END-IF.
 
       200-END.
030100******************************************************************
030200 400-PROCESS-TRAN.
030300******************************************************************
030400
           MOVE CRT-WINDOW-CHANGED     TO CRT-MESSAGE.
           MOVE CRT-RETURN-KNS         TO CRT-REQUEST.
032000
032100 400-END.
032200
079500******************************************************************
079600 HR11S3-TRANSACTION-END.
079700******************************************************************
079800
J53125******************************************************************
J53125 HR11S4-TRANSACTION              SECTION 24.
J53125******************************************************************
J53125 HR11S4-START.
J53125
J53125     INITIALIZE                         DB-COMPANY
J53125                                        DB-PROCESS-LEVEL
J53125                                        DB-DEPARTMENT
J53125                                        DB-EFFECT-DATE.
J18922
J18922     IF (HR11F4-EMP-COMPANY = ZEROES)
J18922         GO TO HR11S4-TRANSACTION-END.
J53125
J53125     INITIALIZE PRPXL-TAXES.
J53125     INITIALIZE PRPXL-PARM-EFF-DATE.
J53125     SET PRPXL-NO-WH TO TRUE.
J53125
J53125     PERFORM 850-FIND-NLT-PPRSET1.
J53125     IF (PAPOSRULE-FOUND)
J53125         MOVE WS-TRUE                TO HREMP-PAUSER-FLAG-SW.
J53125
J53125     IF (HR11F4-FC = "I")
J53125         MOVE HR11F4-EMP-COMPANY     TO DB-COMPANY
J53125         MOVE HR11F4-EMP-EMPLOYEE    TO DB-EMPLOYEE
J53125         PERFORM 840-FIND-EMPSET1
J84780         MOVE EMP-EMPLOYEE           TO HR11F4-TSF-EMPLOYEE.
J53125
J53125     SET HR11WS-HRPADICT2-NOTOPEN    TO TRUE.
J53125
J53125     IF (HR11F4-FC                  NOT = "+" AND "-")
J53125     OR (HR11F4-EMP-EMPLOYEE        NOT = HR11F4-PT-EMP-EMPLOYEE)
J53125         PERFORM 200-EDIT-TRAN
J53125         THRU    200-END
J53125     ELSE
J53125         MOVE HR11F4-EMP-COMPANY     TO DB-COMPANY
J53125         MOVE HR11F4-EMP-EMPLOYEE    TO DB-EMPLOYEE
J53125         PERFORM 840-FIND-EMPSET1
J53125         IF (EMPLOYEE-NOTFOUND)
J53125************ Page up and down is not available for new employee
J53125             MOVE 145                TO CRT-ERROR-NBR
J53125             MOVE HR11F4-FC-FN       TO CRT-FIELD-NBR
J53125             IF (HR11WS-HRPADICT2-OPEN)
J53125                 SET HR11WS-HRPADICT2-NOTOPEN TO TRUE
J53125                 PERFORM 9900-CLOSE-HRPADICT2
J53125             END-IF
J53125             GO TO HR11S4-TRANSACTION-END
J53125         END-IF.
J53125
J53125     GO TO HR11S4-TRANSACTION-END.
J53125
J53125******************************************************************
J53125 200-EDIT-TRAN.
J53125******************************************************************
J53125
J53125     IF (HR11F4-FC  = "A" OR "I")
J53125         MOVE HR11F4-FC              TO HR11WS-PREV-FC
J53125     END-IF.
J53125
J53125     PERFORM 201-MOVE-SCR-TO-WS
J53125     THRU    201-END.
J53125
J53125     PERFORM 2000-HREMP-EDIT-TRAN.
J53125
J53125     IF  (HR11F4-FC  NOT = "C" AND "D")
J53125     AND (NO-ERROR-FOUND)
J53125          MOVE HR11F4-FC              TO HR11WS-PREV-FC
J53125     END-IF.
J53125
J53125     IF (HREMP-UPDPEP-DATE = HREMP-EFFECT-DATE)
J53125         INITIALIZE                             HREMP-UPDPEP-DATE.
J53125
J53125     PERFORM 600-MOVE-WS-TO-SCR
J53125     THRU    600-END.
J53125
J53125     IF (ERROR-FOUND)
J53125     OR (HREMPWS-GRANT-ERR NOT = ZEROES)
J53125         GO TO 200-END.
J53125
J53125 200-END.
J53125
J53125******************************************************************
J53125 201-MOVE-SCR-TO-WS.
J53125******************************************************************
J53125     INITIALIZE HREMP-SCR-FIELDS.
J53125     INITIALIZE HRPEM-SCR-FIELDS.
J53125
J53125     MOVE HR11F4-FC                    TO HREMP-FC.
J53125     MOVE HR11F4-FC-FN                 TO HREMP-FC-FN.
J53125     MOVE HR11F4-EMP-COMPANY           TO HREMP-COMPANY.
J53125     MOVE HR11F4-EMP-COMPANY-FN        TO HREMP-COMPANY-FN.
J53125     MOVE HR11F4-EMP-EMPLOYEE          TO HREMP-EMPLOYEE.
J53125     MOVE HR11F4-EMP-EMPLOYEE-FN       TO HREMP-EMPLOYEE-FN.
J53125     MOVE HR11F4-EMP-LAST-NAME-PRE     TO HREMP-LAST-NAME-PRE.
J53125     MOVE HR11F4-EMP-LAST-NAME-PRE-FN  TO HREMP-LAST-NAME-PRE-FN.
J53125     MOVE HR11F4-EMP-LAST-NAME         TO HREMP-LAST-NAME.
J53125     MOVE HR11F4-EMP-LAST-NAME-FN      TO HREMP-LAST-NAME-FN.
J53125     MOVE HR11F4-EMP-FIRST-NAME        TO HREMP-FIRST-NAME.
J53125     MOVE HR11F4-EMP-FIRST-NAME-FN     TO HREMP-FIRST-NAME-FN.
J53125     MOVE HR11F4-EMP-MIDDLE-NAME       TO HREMP-MIDDLE-NAME.
J53125     MOVE HR11F4-EMP-MIDDLE-NAME-FN    TO HREMP-MIDDLE-NAME-FN.
J53125     MOVE HR11F4-EMP-MIDDLE-NAME       TO HREMP-MIDDLE-INIT.
J53125     MOVE HR11F4-EMP-MIDDLE-NAME-FN    TO HREMP-MIDDLE-INIT-FN.
J53125     MOVE HR11F4-EMP-NICK-NAME         TO HREMP-NICK-NAME.
J53125     MOVE HR11F4-EMP-NICK-NAME-FN      TO HREMP-NICK-NAME-FN.
J53125     MOVE HR11F4-EMP-NAME-PREFIX       TO HREMP-NAME-PREFIX.
J53125     MOVE HR11F4-EMP-NAME-PREFIX-FN    TO HREMP-NAME-PREFIX-FN.
J53125     MOVE HR11F4-EMP-NAME-SUFFIX       TO HREMP-NAME-SUFFIX.
J53125     MOVE HR11F4-EMP-NAME-SUFFIX-FN    TO HREMP-NAME-SUFFIX-FN.
J53125     MOVE HR11F4-EMP-WORK-COUNTRY      TO HREMP-WORK-COUNTRY.
J53125     MOVE HR11F4-EMP-WORK-COUNTRY-FN   TO HREMP-WORK-COUNTRY-FN.
J53125     MOVE HR11F4-EMP-EMP-STATUS        TO HREMP-EMP-STATUS.
J53125     MOVE HR11F4-EMP-EMP-STATUS-FN     TO HREMP-EMP-STATUS-FN.
J53125     MOVE HR11F4-EMP-FICA-NBR          TO HREMP-FICA-NBR.
J53125     MOVE HR11F4-EMP-FICA-NBR-FN       TO HREMP-FICA-NBR-FN.
J53125     MOVE HR11F4-EMP-PROCESS-LEVEL     TO HREMP-PROCESS-LEVEL.
J53125     MOVE HR11F4-EMP-PROCESS-LEVEL-FN  TO HREMP-PROCESS-LEVEL-FN.
J53125     MOVE HR11F4-EMP-DEPARTMENT        TO HREMP-DEPARTMENT.
J53125     MOVE HR11F4-EMP-DEPARTMENT-FN     TO HREMP-DEPARTMENT-FN.
J53125     MOVE HR11F4-EMP-USER-LEVEL        TO HREMP-USER-LEVEL.
J53125     MOVE HR11F4-EMP-USER-LEVEL-FN     TO HREMP-USER-LEVEL-FN.
J53125     MOVE HR11F4-EMP-JOB-CODE          TO HREMP-JOB-CODE.
J53125     MOVE HR11F4-EMP-JOB-CODE-FN       TO HREMP-JOB-CODE-FN.
J53125     MOVE HR11F4-EMP-UNION-CODE        TO HREMP-UNION-CODE.
J53125     MOVE HR11F4-EMP-UNION-CODE-FN     TO HREMP-UNION-CODE-FN.
J53125     MOVE HR11F4-EMP-SUPERVISOR        TO HREMP-SUPERVISOR.
J53125     MOVE HR11F4-EMP-SUPERVISOR-FN     TO HREMP-SUPERVISOR-FN.
J53125     MOVE HR11F4-EMP-SUPER-IND         TO HREMP-SUPERVISOR-IND.
J53125     MOVE HR11F4-EMP-SUPER-IND-FN      TO HREMP-SUPERVISOR-IND-FN.
J53125     MOVE HR11F4-EMP-DATE-HIRED        TO HREMP-DATE-HIRED.
J53125     MOVE HR11F4-EMP-DATE-HIRED-FN     TO HREMP-DATE-HIRED-FN.
J53125     MOVE HR11F4-EMP-ADJ-HIRE-DATE     TO HREMP-ADJ-HIRE-DATE.
J53125     MOVE HR11F4-EMP-ADJ-HIRE-DATE-FN  TO HREMP-ADJ-HIRE-DATE-FN.
J53125     MOVE HR11F4-EMP-ANNIVERS-DATE     TO HREMP-ANNIVERS-DATE.
J53125     MOVE HR11F4-EMP-ANNIVERS-DATE-FN  TO HREMP-ANNIVERS-DATE-FN.
J53125     MOVE HR11F4-EMP-FST-DAY-WORKED    TO HREMP-FST-DAY-WORKED.
J53125     MOVE HR11F4-EMP-FST-DAY-WORKED-FN TO HREMP-FST-DAY-WORKED-FN.
J53125     MOVE HR11F4-EMP-LAST-DAY-PAID     TO HREMP-LAST-DAY-PAID.
J53125     MOVE HR11F4-EMP-LAST-DAY-PAID-FN  TO HREMP-LAST-DAY-PAID-FN.
J53125     MOVE HR11F4-EMP-TERM-DATE         TO HREMP-TERM-DATE.
J53125     MOVE HR11F4-EMP-TERM-DATE-FN      TO HREMP-TERM-DATE-FN.
J53125     MOVE HR11F4-EMP-WORK-SCHED        TO HREMP-WORK-SCHED.
J53125     MOVE HR11F4-EMP-WORK-SCHED-FN     TO HREMP-WORK-SCHED-FN.
J53125     MOVE HR11F4-EMP-POSITION          TO HREMP-POSITION.
J53125     MOVE HR11F4-EMP-POSITION-FN       TO HREMP-POSITION-FN.
J53125     MOVE HR11F4-PEM-LOCAT-CODE        TO HRPEM-LOCAT-CODE.
J53125     MOVE HR11F4-PEM-LOCAT-CODE-FN     TO HRPEM-LOCAT-CODE-FN.
J53125     MOVE HR11F4-PEM-BARGAIN-UNIT      TO HRPEM-BARGAIN-UNIT.
J53125     MOVE HR11F4-PEM-BARGAIN-UNIT-FN   TO HRPEM-BARGAIN-UNIT-FN.
J53125     MOVE HR11F4-PEM-SENIOR-DATE       TO HRPEM-SENIOR-DATE.
J53125     MOVE HR11F4-PEM-SENIOR-DATE-FN    TO HRPEM-SENIOR-DATE-FN.
J53125     MOVE HR11F4-EMP-NEW-HIRE-DATE     TO HREMP-NEW-HIRE-DATE.
J53125     MOVE HR11F4-EMP-NEW-HIRE-DATE-FN  TO HREMP-NEW-HIRE-DATE-FN.
J53125     MOVE HR11F4-USER-ID               TO HREMP-USER-ID.
J53125
J53125 201-END.
J53125
J53125******************************************************************
J53125 600-MOVE-WS-TO-SCR.
J53125******************************************************************
J53125
J53125     MOVE HREMP-COMPANY               TO DB-COMPANY.
J53125     MOVE SPACES                      TO DB-PROCESS-LEVEL.
J53125     PERFORM 840-FIND-PRSSET1.
J53125     MOVE PRS-NAME                    TO HR11F4-PRS-NAME.
J53125
J53125     MOVE 1                      TO HR11F4-POS-LEVEL.
J53125     MOVE HREMP-EMPLOYEE         TO HR11F4-EMP-EMPLOYEE.
J53125     MOVE HREMP-FULL-NAME        TO HR11F4-EMP-FULL-NAME-CND.
J53125     MOVE HREMP-LAST-NAME-PRE    TO HR11F4-EMP-LAST-NAME-PRE.
J53125     MOVE HREMP-LAST-NAME        TO HR11F4-EMP-LAST-NAME.
J53125     MOVE HREMP-FIRST-NAME       TO HR11F4-EMP-FIRST-NAME.
J53125     MOVE HREMP-MIDDLE-NAME      TO HR11F4-EMP-MIDDLE-NAME.
J53125     MOVE HREMP-NICK-NAME        TO HR11F4-EMP-NICK-NAME.
J53125     MOVE HREMP-NAME-PREFIX      TO HR11F4-EMP-NAME-PREFIX.
J53125     MOVE HREMP-NAME-SUFFIX      TO HR11F4-EMP-NAME-SUFFIX.
J53125     MOVE HREMP-WORK-COUNTRY     TO HR11F4-EMP-WORK-COUNTRY.
J53125     MOVE HREMP-EMP-STATUS       TO HR11F4-EMP-EMP-STATUS.
J53125     MOVE HREMP-FICA-NBR         TO HR11F4-EMP-FICA-NBR.
J53125*    MASK SOCIAL NUMBER
J53125     MOVE HREMP-FICA-NBR         TO WS-FICA-MASK.
J53125     MOVE 1                      TO I3.
J53125     PERFORM
J53125         VARYING I2 FROM 20 BY -1
J53125         UNTIL (I2 < 1)
J53125         IF  (WS-HOLD-FICA-IN (I2)  = SPACES OR "-")
J53125             MOVE WS-HOLD-FICA-IN (I2)  
J53125                                 TO WS-HOLD-FICA-OUT (I2)
J53125         ELSE
J53125             IF  (I3 > 4)
J53125                 MOVE "*"        TO WS-HOLD-FICA-OUT (I2)
J53125             ELSE
J53125                 MOVE WS-HOLD-FICA-IN (I2) 
J53125                                 TO WS-HOLD-FICA-OUT (I2)
J53125                 ADD 1           TO I3
J53125             END-IF
J53125         END-IF
J53125     END-PERFORM.
J53125     MOVE WS-FICA-MASKED         TO HR11F4-EMP-FICA-NBR.
J53125     MOVE HREMP-PROCESS-LEVEL    TO HR11F4-EMP-PROCESS-LEVEL.
J53125     MOVE HREMP-DEPARTMENT       TO HR11F4-EMP-DEPARTMENT.
J53125     MOVE HREMP-USER-LEVEL       TO HR11F4-EMP-USER-LEVEL.
J53125     MOVE HREMP-JOB-CODE         TO HR11F4-EMP-JOB-CODE.
J53125     MOVE HREMP-UNION-CODE       TO HR11F4-EMP-UNION-CODE.
J53125     MOVE HREMP-SUPERVISOR       TO HR11F4-EMP-SUPERVISOR.
J53125     MOVE HREMP-SUPERVISOR-IND   TO HR11F4-EMP-SUPER-IND.
J53125     MOVE HREMP-DATE-HIRED       TO HR11F4-EMP-DATE-HIRED.
J53125
J53125     MOVE HREMP-ADJ-HIRE-DATE    TO HR11F4-EMP-ADJ-HIRE-DATE.
J53125     MOVE HREMP-ANNIVERS-DATE    TO HR11F4-EMP-ANNIVERS-DATE.
J53125     MOVE HREMP-FST-DAY-WORKED   TO HR11F4-EMP-FST-DAY-WORKED.
J53125     MOVE HREMP-LAST-DAY-PAID    TO HR11F4-EMP-LAST-DAY-PAID.
J53125     MOVE HREMP-TERM-DATE        TO HR11F4-EMP-TERM-DATE.
J53125     MOVE HREMP-WORK-SCHED       TO HR11F4-EMP-WORK-SCHED.
J53125
J53125     MOVE HREMP-POSITION         TO HR11F4-EMP-POSITION.
J53125     MOVE HRPEM-LOCAT-CODE       TO HR11F4-PEM-LOCAT-CODE.
J53125     MOVE HRPEM-BARGAIN-UNIT     TO HR11F4-PEM-BARGAIN-UNIT.
J53125     MOVE HRPEM-SENIOR-DATE      TO HR11F4-PEM-SENIOR-DATE.
J53125     MOVE HREMP-NEW-HIRE-DATE    TO HR11F4-EMP-NEW-HIRE-DATE.
J53125
J53125     IF  (ERROR-FOUND)
J53125         GO TO 600-END.
J53125
J53125     INITIALIZE                     HR11F4-NAME-SUFFIX-DESC
J53125                                    HR11F4-NAME-PREFIX-DESC
J53125                                    HR11F4-EMS-DESCRIPTION
J53125                                    HR11F4-PRS1-NAME
J53125                                    HR11F4-DPT-NAME
J53125                                    HR11F4-WORK-COUNTRY-DESC
J53125                                    HR11F4-PCO-DESCRIPTION
J53125                                    HR11F4-POS-DESCRIPTION
J53125                                    HR11F4-JBC-DESCRIPTION
J53125                                    HR11F4-SUP-NAME
J53125                                    HR11F4-IND-SUP-NAME
J53125                                    HR11F4-PCO1-DESCRIPTION
J53125                                    HR11F4-UNION-CODE-DESC
J53125                                    HR11F4-PCO3-DESCRIPTION
J53125                                    HR11F4-WORK-SCHED-DESC.
J53125
J53125* EMP-PROCESS-LEVEL is used for lookups, in case it's secured
J53125* and other fields aren't
J53125
J53125     IF (HR11F4-EMP-NAME-SUFFIX NOT = SPACES)
J53125         MOVE "SU"                    TO DB-TYPE
J53125         MOVE HR11F4-EMP-NAME-SUFFIX  TO DB-HRCTRY-CODE
J53125         INITIALIZE                      DB-COUNTRY-CODE
J53125         PERFORM 850-FIND-NLT-CTCSET2
J53125         IF  (HRCTRYCODE-FOUND)
J53125         AND (CTC-TYPE        = DB-TYPE)
J53125         AND (CTC-HRCTRY-CODE = DB-HRCTRY-CODE)
J53125             MOVE CTC-DESCRIPTION     TO HR11F4-NAME-SUFFIX-DESC.
J53125
J53125     IF (HR11F4-EMP-NAME-PREFIX NOT = SPACES)
J53125         MOVE "PR"                    TO DB-TYPE
J53125         MOVE HR11F4-EMP-NAME-PREFIX  TO DB-HRCTRY-CODE
J53125         INITIALIZE                      DB-COUNTRY-CODE
J53125         PERFORM 850-FIND-NLT-CTCSET2
J53125         IF  (HRCTRYCODE-FOUND)
J53125         AND (CTC-TYPE        = DB-TYPE)
J53125         AND (CTC-HRCTRY-CODE = DB-HRCTRY-CODE)
J53125             MOVE CTC-DESCRIPTION     TO HR11F4-NAME-PREFIX-DESC.
J53125
J53125     IF (HR11F4-EMP-EMP-STATUS NOT = SPACES)
J53125         MOVE HR11F4-EMP-EMP-STATUS   TO DB-EMP-STATUS
J53125         PERFORM 840-FIND-EMSSET1
J53125         IF  (EMSTATUS-FOUND)
J53125             MOVE EMS-DESCRIPTION     TO HR11F4-EMS-DESCRIPTION.
J53125
J53125     IF (HR11F4-EMP-PROCESS-LEVEL NOT = SPACES)
J53125         MOVE HR11F4-EMP-PROCESS-LEVEL   TO DB-PROCESS-LEVEL
J53125         PERFORM 840-FIND-PRSSET1
J53125         IF  (PRSYSTEM-FOUND)
J53125             MOVE PRS-NAME            TO HR11F4-PRS1-NAME.
J53125
J53125     IF (HR11F4-EMP-DEPARTMENT NOT = SPACES)
J53125         MOVE EMP-PROCESS-LEVEL       TO DB-PROCESS-LEVEL
J53125         MOVE HR11F4-EMP-DEPARTMENT   TO DB-DEPARTMENT
J53125         PERFORM 840-FIND-DPTSET1
J53125         IF  (DEPTCODE-FOUND)
J53125             MOVE DPT-NAME            TO HR11F4-DPT-NAME.
J53125
J53125     IF (HR11F4-EMP-USER-LEVEL NOT = SPACES)
J53125         MOVE "UL"                    TO DB-TYPE
J53125         MOVE HR11F4-EMP-USER-LEVEL   TO DB-CODE
J53125         PERFORM 840-FIND-PCOSET1
J53125         IF  (PCODES-FOUND)
J53125             MOVE PCO-DESCRIPTION     TO HR11F4-PCO-DESCRIPTION.
J53125
J53125     IF (HR11F4-EMP-JOB-CODE NOT = SPACES)
J53125         MOVE HR11F4-EMP-JOB-CODE     TO DB-JOB-CODE
J53125         PERFORM 840-FIND-JBCSET1
J53125         IF  (JOBCODE-FOUND)
J53125             MOVE JBC-DESCRIPTION     TO HR11F4-JBC-DESCRIPTION.
J53125
J53125     IF (HR11F4-EMP-SUPERVISOR NOT = SPACES)
J53125         MOVE HR11F4-EMP-SUPERVISOR   TO DB-CODE
J53125         PERFORM 840-FIND-HSUSET1
J53125         IF  (HRSUPER-FOUND)
J53125         AND (HSU-EMPLOYEE NOT = ZEROS)
J53125             MOVE HSU-EMPLOYEE        TO DB-EMPLOYEE
J53125             MOVE PTFSET1-EMPLOYEE    TO WS-DB-BEG-RNG
J53125             PERFORM 850-FIND-BEGRNG-PTFSET1
J53125             MOVE PTF-LAST-NAME       TO HRWS-LAST-NAME
J53125             MOVE PTF-FIRST-NAME      TO HRWS-FIRST-NAME
J53125             MOVE PTF-MIDDLE-INIT     TO HRWS-MIDDLE-INIT
J53125             PERFORM 750-HR-FORMAT-NAME
J53125             MOVE HRWS-FORMAT-NAME    TO HR11F4-SUP-NAME.
J53125
J53125     IF (HR11F4-EMP-SUPER-IND NOT = SPACES)
J53125         MOVE HR11F4-EMP-SUPER-IND    TO DB-CODE
J53125         PERFORM 840-FIND-HSUSET1
J53125         IF  (HRSUPER-FOUND)
J53125         AND (HSU-EMPLOYEE NOT = ZEROS)
J53125             MOVE HSU-EMPLOYEE        TO DB-EMPLOYEE
J53125             MOVE PTFSET1-EMPLOYEE    TO WS-DB-BEG-RNG
J53125             PERFORM 850-FIND-BEGRNG-PTFSET1
J53125             MOVE PTF-LAST-NAME       TO HRWS-LAST-NAME
J53125             MOVE PTF-FIRST-NAME      TO HRWS-FIRST-NAME
J53125             MOVE PTF-MIDDLE-INIT     TO HRWS-MIDDLE-INIT
J53125             PERFORM 750-HR-FORMAT-NAME
J53125             MOVE HRWS-FORMAT-NAME    TO HR11F4-IND-SUP-NAME.
J53125
J53125     IF (HR11F4-PEM-LOCAT-CODE NOT = SPACES)
J53125         MOVE "LO"                    TO DB-TYPE
J53125         MOVE HR11F4-PEM-LOCAT-CODE   TO DB-CODE
J53125         PERFORM 840-FIND-PCOSET1
J53125         IF  (PCODES-FOUND)
J53125             MOVE PCO-DESCRIPTION     TO HR11F4-PCO1-DESCRIPTION.
J53125
J53125     IF  (HR11F4-EMP-POSITION      NOT = SPACES)
J53125     AND (HR11F4-POS-DESCRIPTION   = SPACES)
J53125         MOVE HR11F4-EMP-COMPANY      TO PAPOS-COMPANY
J53125         MOVE HR11F4-EMP-POSITION     TO PAPOS-POSITION
J53125         MOVE WS-SYSTEM-DATE-YMD      TO PAPOS-EFFECT-DATE
J53125         IF (HR11F4-EMP-TERM-DATE = ZEROES)
J53125             MOVE WS-SYSTEM-DATE-YMD TO PAPOS-END-DATE
J53125         ELSE
J53125             MOVE HR11F4-EMP-TERM-DATE   TO PAPOS-END-DATE
J53125         END-IF
J53125         MOVE 1                          TO ERROR-FLAG-SW
J53125         INITIALIZE                         PAPOS-DESCRIPTION
J53125         PERFORM 2000-EDIT-POSITION-DATES
J53125         INITIALIZE                      CRT-ERROR-NBR
J53125                                         CRT-ERROR-CAT
J53125         MOVE PAPOS-DESCRIPTION       TO HR11F4-POS-DESCRIPTION.
J53125
J53125     MOVE HR11F4-EMP-COMPANY          TO DB-COMPANY.
J53125
J53125     IF (HR11F4-EMP-WORK-SCHED NOT = SPACES)
J53125         MOVE HR11F4-EMP-WORK-SCHED  TO DB-WORK-SCHED
J53125         MOVE ZEROES                 TO DB-CONTRACT-YEAR
J53125         MOVE WS-SYSTEM-DATE-YMD     TO DB-EFFECT-DATE
J53125         PERFORM 850-FIND-NLT-WSCSET1
J53125         IF (HRWRKSCHD-NOTFOUND)
J53125         OR (WSC-COMPANY    NOT = DB-COMPANY)
J53125         OR (WSC-WORK-SCHED NOT = DB-WORK-SCHED)
J53125             MOVE 9999               TO DB-CONTRACT-YEAR
J53125             MOVE ZEROES             TO DB-EFFECT-DATE
J53125             PERFORM 850-FIND-NLT-WSCSET1
J53125         END-IF
J53125         IF (HRWRKSCHD-FOUND)
J53125         OR (WSC-COMPANY    NOT = DB-COMPANY)
J53125         OR (WSC-WORK-SCHED NOT = DB-WORK-SCHED)
J53125             MOVE WSC-DESCRIPTION    TO HR11F4-WORK-SCHED-DESC.
J53125
J53125     IF (HR11F4-EMP-UNION-CODE NOT = SPACES)
J53125         MOVE "UN"                   TO DB-TYPE
J53125         MOVE HR11F4-EMP-UNION-CODE  TO DB-CODE
J53125         PERFORM 840-FIND-PCOSET1
J53125         IF  (PCODES-FOUND)
J53125             MOVE PCO-DESCRIPTION    TO HR11F4-UNION-CODE-DESC.
J53125
J53125     IF (HR11F4-PEM-BARGAIN-UNIT NOT = SPACES)
J53125         MOVE "BU"                   TO DB-TYPE
J53125         MOVE HR11F4-PEM-BARGAIN-UNIT
J53125                                     TO DB-CODE
J53125         PERFORM 840-FIND-PCOSET1
J53125         IF  (PCODES-FOUND)
J53125             MOVE PCO-DESCRIPTION    TO HR11F4-PCO3-DESCRIPTION.
J53125
J53125     MOVE HR11F4-EMP-COMPANY         TO DB-COMPANY.
J53125
J53125     IF (HR11F4-EMP-WORK-COUNTRY NOT = SPACES)
J53125         MOVE HR11F4-EMP-WORK-COUNTRY TO DB-COUNTRY-CODE
J53125         PERFORM 840-FIND-INTSET1
J53125         IF (INSTCTRYCD-FOUND)
J53125             MOVE INT-COUNTRY-DESC   TO HR11F4-WORK-COUNTRY-DESC
J53125         END-IF.
J53125
J53125     MOVE ZEROES TO CRT-ERROR-NBR.
J18922     MOVE CRT-INQ-COMPLETE           TO CRT-MESSAGE.
J53125
J53125 600-END.
J53125
J53125******************************************************************
J53125 HR11S4-TRANSACTION-END.
J53125******************************************************************
J19381
J19381******************************************************************
J19381 HR11S5-TRANSACTION              SECTION 24.
J19381******************************************************************
J19381 HR11S5-START.
J19381
J19381     IF (HR11F5-FC           NOT = "A" AND "C")
J19381         INITIALIZE          HR11F5-XMIT-EFFECT-DATE
J19381     END-IF.
J19381
J19381     PERFORM 200-EDIT-TRAN
J19381     THRU    200-END.
J19381
J19381     IF (NO-ERROR-FOUND)
J19381         PERFORM 400-PROCESS-TRAN
J19381         THRU    400-END
J19381         INITIALIZE          HR11F5-XMIT-EFFECT-DATE
J19381     END-IF.
J19381
J19381     GO TO HR11S5-TRANSACTION-END.
J19381
J19381******************************************************************
J19381 200-EDIT-TRAN.
J19381******************************************************************
J19381
J19381     PERFORM 210-EDIT-ACCESS
J19381     THRU    210-END.
J19381
J19381     IF (ERROR-FOUND)
J19381         GO TO 200-END
J19381     END-IF.
J19381
J19381     IF (HR11F5-FC = "A" OR "C")
J19381         PERFORM 230-EDIT-DATA
J19381         THRU    230-END
J19381     END-IF.
J19381
J19381 200-END.
J19381     EXIT.
J19381
J19381******************************************************************
J19381 210-EDIT-ACCESS.
J19381******************************************************************
J19381
J19381     MOVE HR11F5-PRS-COMPANY         TO DB-COMPANY.
J19381     MOVE SPACES                     TO DB-PROCESS-LEVEL.
J19381     PERFORM 840-FIND-PRSSET1.
J19381     IF (PRSYSTEM-NOTFOUND)
J19381*---HR11#185: Company does not exist
J19381         MOVE "HR11"                 TO CRT-ERROR-CAT
J19381         MOVE 185                    TO CRT-ERROR-NBR
J19381         MOVE HR11F5-PRS-COMPANY-FN  TO CRT-FIELD-NBR
J19381         GO TO 210-END
J19381     END-IF.
J19381
J19381     MOVE PRS-NAME                   TO HR11F5-PRS-NAME.
J19381
J19381     IF  (HR11F5-FC           NOT = "I" AND "+" AND "-")
J19381         INITIALIZE HR11F5-PT-GED-FLD-NBR
J19381     END-IF.
J19381
J60623     IF  (HR11F5-FC = "I")
J60623     AND ((HR11F5-PRS-COMPANY NOT = HR11WS-SAVE-COMPANY)
J60623     OR   (HR11F5-EMPLOYEE    NOT = HR11WS-SAVE-EMPLOYEE))
J60623         INITIALIZE HR11F5-PT-GED-FLD-NBR
J60623     END-IF.
J60623
J19381     MOVE HR11F5-PRS-COMPANY         TO DB-COMPANY
J60623                                        HR11WS-SAVE-COMPANY.
J19381     MOVE HR11F5-EMPLOYEE            TO DB-EMPLOYEE
J60623                                        HR11WS-SAVE-EMPLOYEE.
J19381
J19381     PERFORM 840-FIND-EMPSET1.
J19381
J19381     IF  (HR11F5-FC = "P")
J19381        PERFORM 850-FIND-NLT-EMPSET1
J19381        PERFORM 870-FIND-PREV-EMPSET1
J19381     END-IF.
J19381
J19381     IF  (HR11F5-FC = "N")
J19381        IF  (EMPLOYEE-FOUND)
J19381        PERFORM 850-FIND-NLT-EMPSET1
J19381        PERFORM 860-FIND-NEXT-EMPSET1
J19381        ELSE
J19381        PERFORM 850-FIND-NLT-EMPSET1
J19381       END-IF
J19381     END-IF.
J19381
J19381     IF  (HR11F5-FC = "N" OR "P")
J19381         IF (EMPLOYEE-NOTFOUND)
J19381         OR (EMP-COMPANY NOT = DB-COMPANY)
J19381*---HR11#187 : No (More) additional details exist for company
J19381             MOVE 187                TO CRT-ERROR-NBR
J19381             MOVE SPACES             TO CRT-ERROR-CAT
J19381             MOVE HR11F5-FC-FN       TO CRT-FIELD-NBR
J19381             GO TO 210-END
J19381         ELSE
J19381             MOVE EMP-EMPLOYEE       TO HR11F5-EMPLOYEE
J19381                                        DB-EMPLOYEE
J19381         END-IF
J19381     END-IF.
J19381
J19381     IF (EMPLOYEE-NOTFOUND)
J19381*---HR11#135: Employee does not exist
J19381         MOVE "HR11"                 TO CRT-ERROR-CAT
J19381         MOVE 135                    TO CRT-ERROR-NBR
J19381         MOVE HR11F5-EMPLOYEE-FN     TO CRT-FIELD-NBR
J19381         GO TO 210-END
J19381     END-IF.
J19381
J19381     MOVE EMP-LAST-NAME          TO HRWS-LAST-NAME.
J19381     MOVE EMP-FIRST-NAME         TO HRWS-FIRST-NAME.
J19381     MOVE EMP-MIDDLE-NAME        TO HRWS-MIDDLE-INIT.
J19381     MOVE EMP-NAME-SUFFIX        TO HRWS-NAME-SUFFIX.
J19381     MOVE EMP-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE.
J19381     PERFORM 750-HR-FORMAT-NAME.
J19381     MOVE HRWS-FORMAT-NAME       TO HR11F5-EMP-NAME.


J19381     MOVE EMP-WORK-COUNTRY       TO HR11F5-COUNTRY-CODE.
J19381     IF (EMP-WORK-COUNTRY = "IN")
J19381         MOVE "I1"               TO HR11F5-GED-TOPIC
J19381     ELSE
J19381         MOVE "G1"               TO HR11F5-GED-TOPIC
J19381     END-IF.
J19381
J19381     IF  (HR11F5-FC NOT = "I" AND "+" AND "-")
J19381         GO TO 210-END
J19381     END-IF.
J19381
J19381     MOVE HR11F5-COUNTRY-CODE        TO DB-COUNTRY-CODE.
J19381     MOVE HR11F5-GED-TOPIC           TO DB-TOPIC.
J19381     IF  (HR11F5-FC     = "I" OR "+")
J19381         MOVE HR11F5-PT-GED-FLD-NBR  TO DB-FLD-NBR
J19381     ELSE
J19381         MOVE HR11F5-GED-FLD-NBR (1) TO DB-FLD-NBR
J19381     END-IF.
J19381
J19381     IF  (HR11F5-FC      = "-")
J19381     AND (DB-FLD-NBR     = ZEROES)
J19381*---<   >#11: No More Records For Given Key
J19381         MOVE 11                         TO CRT-ERROR-NBR
J19381         MOVE SPACES                     TO CRT-ERROR-CAT
J19381         MOVE HR11F5-EMPLOYEE-FN         TO CRT-FIELD-NBR
J19381     END-IF.
J19381
J19381     PERFORM 850-FIND-NLT-GEDSET1.
J19381
J19381     IF  (HR11F5-FC = "-")
J19381         PERFORM 870-FIND-PREV-GEDSET1
J19381     END-IF.
J19381
J19381     IF  (GEMPDICT-NOTFOUND)
J19381     OR  (GED-COUNTRY-CODE   NOT = DB-COUNTRY-CODE)
J19381     OR  (GED-TOPIC          NOT = DB-TOPIC)
J19381         IF  (HR11F5-FC      = "I")
J19381             CONTINUE
J19381         ELSE
J19381*---<   >#11: No More Records For Given Key
J19381             MOVE 11                     TO CRT-ERROR-NBR
J19381             MOVE SPACES                 TO CRT-ERROR-CAT
J19381             MOVE HR11F5-EMPLOYEE-FN     TO CRT-FIELD-NBR
J19381             GO TO 210-END
J19381         END-IF
J19381     ELSE
J19381         IF  (GED-FLD-NBR = HR11F5-GED-FLD-NBR (1))
J19381             IF  (HR11F5-FC      = "I")
J19381                 CONTINUE
J19381             ELSE
J19381*---<   >#11: No More Records For Given Key
J19381                 MOVE 11                     TO CRT-ERROR-NBR
J19381                 MOVE SPACES                 TO CRT-ERROR-CAT
J19381                 MOVE HR11F5-EMPLOYEE-FN     TO CRT-FIELD-NBR
J19381                 GO TO 210-END
J19381             END-IF
J19381         END-IF
J19381     END-IF.
J19381
J19381 210-END.
J19381
J19381******************************************************************
J19381 230-EDIT-DATA.
J19381******************************************************************
J19381
J19381     IF  (HR11F5-EFFECT-DATE = ZEROES)
J19381     AND (HR11F5-XMIT-EFFECT-DATE     = 0)
J19381*---HR11#108: Verify effective date of action
J19381         MOVE WS-SYSTEM-DATE-YMD        TO HR11F5-EFFECT-DATE
J19381         MOVE 1                         TO HR11F5-XMIT-EFFECT-DATE
J19381         MOVE "HR11"                    TO CRT-ERROR-CAT
J19381         MOVE 108                       TO CRT-ERROR-NBR
J19381         MOVE HR11F5-EFFECT-DATE-FN     TO CRT-FIELD-NBR
J19381         GO TO 230-END
J19381     END-IF.
J19381
J19381     PERFORM 260-EDIT-DTL-TRAN
J19381     THRU    260-END
J19381         VARYING I1 FROM 1 BY 1
J19381         UNTIL  (I1 > 15)
J19381         OR     (ERROR-FOUND).
J19381
J19381     IF (ERROR-FOUND)
J19381         GO TO 230-END
J19381     END-IF.
J19381
J19381 230-END.
J19381
J19381******************************************************************
J19381 260-EDIT-DTL-TRAN.
J19381******************************************************************
J19381
J19381     IF (HR11F5-LINE-FC     (I1) = SPACES)
J19381     OR (HR11F5-GED-FLD-NBR (I1) = ZEROES)
J19381         GO TO 260-END
J19381     END-IF.
J19381
J19381     PERFORM 700-MOVE-SCR-TO-HRGEP
J19381     THRU    700-END.
J19381
J19381     PERFORM 2000-HRGEP-EDITS.
J19381
J19381 260-END.
J19381     EXIT.
J19381
J19381******************************************************************
J19381 400-PROCESS-TRAN.
J19381******************************************************************
J19381
J19381     IF (HR11F5-FC = "A")
J19381         PERFORM 410-ADD
J19381         THRU    410-END
J19381     ELSE
J19381     IF (HR11F5-FC = "C")
J19381         PERFORM 420-CHANGE
J19381         THRU    420-END
J19381     ELSE
J19381     IF (HR11F5-FC = "I" OR "-" OR "+" OR "N" OR "P")
J19381         PERFORM 480-INQUIRE
J19381         THRU    480-END
J19381     END-IF
J19381     END-IF
J19381     END-IF.
J19381
J19381 400-END.
J19381     EXIT.
J19381
J19381******************************************************************
J19381 410-ADD.
J19381******************************************************************
J19381
J19381     PERFORM 910-AUDIT-BEGIN.
J19381     IF (DMS-ABORTED)
J19381         GO TO 410-END
J19381     END-IF.
J19381
J19381     PERFORM 430-PROCESS-DETAIL
J19381     THRU    430-END
J19381         VARYING I1 FROM 1 BY 1
J19381         UNTIL  (I1 > 15)
J19381         OR     (ERROR-FOUND).
J19381
J19381     IF  (ERROR-FOUND)
J19381         GO TO 410-END
J19381     END-IF.
J19381
J19381     PERFORM 920-AUDIT-END.
J19381
J19381     MOVE "I"                        TO HR11F5-FC.
J19381     PERFORM 600-MOVE-TO-SCREEN
J19381     THRU    600-END.
J19381
J19381     MOVE CRT-ADD-COMPLETE           TO CRT-MESSAGE.
J19381
J19381 410-END.
J19381     EXIT.
J19381
J19381******************************************************************
J19381 420-CHANGE.
J19381******************************************************************
J19381
J19381     PERFORM 910-AUDIT-BEGIN.
J19381     IF (DMS-ABORTED)
J19381         GO TO 420-END
J19381     END-IF.
J19381
J19381     PERFORM 430-PROCESS-DETAIL
J19381     THRU    430-END
J19381         VARYING I1 FROM 1 BY 1
J19381         UNTIL  (I1 > 15)
J19381         OR     (ERROR-FOUND).
J19381
J19381     IF  (ERROR-FOUND)
J19381         GO TO 420-END
J19381     END-IF.
J19381
J19381     PERFORM 920-AUDIT-END.
J19381
J19381     MOVE "I"                        TO HR11F5-FC.
J19381     PERFORM 600-MOVE-TO-SCREEN
J19381     THRU    600-END.
J19381
J19381     MOVE CRT-CHG-COMPLETE           TO CRT-MESSAGE.
J19381
J19381 420-END.
J19381     EXIT.
J19381
J19381******************************************************************
J19381 430-PROCESS-DETAIL.
J19381******************************************************************
J19381
J19381     IF (HR11F5-LINE-FC     (I1) = SPACES)
J19381     OR (HR11F5-GED-FLD-NBR (I1) = ZEROES)
J19381         GO TO 430-END
J19381     END-IF.
J19381
J19381     PERFORM 700-MOVE-SCR-TO-HRGEP
J19381     THRU    700-END.
J19381
J19381     PERFORM 2000-HRGEP-EDITS.
J19381     IF (NO-ERROR-FOUND)
J19381         PERFORM 4000-HRGEP-PROCESS-TRAN
J19381     END-IF.
J19381
J19381 430-END.
J19381     EXIT.
J19381
J19381******************************************************************
J19381 480-INQUIRE.
J19381******************************************************************
J19381     IF (HR11F5-FC = "I" OR "N" OR "P")
J19381         MOVE ZEROES                 TO HR11F5-EFFECT-DATE
J19381     END-IF.
J19381
J19381     PERFORM 600-MOVE-TO-SCREEN
J19381     THRU    600-END.
J19381
J19381     IF  (HR11WS-NO-MORE-REC)
J19381         MOVE CRT-INQ-COMPLETE       TO CRT-MESSAGE
J19381     ELSE
J19381         MOVE CRT-MORE-RECS          TO CRT-MESSAGE
J19381     END-IF.
J19381
J19381 480-END.
J19381     EXIT.
J19381
J19381******************************************************************
J19381 600-MOVE-TO-SCREEN.
J19381******************************************************************
J19381
J19381     IF (HR11F5-FC = "I")
J19381         MOVE "+"                    TO HR11F5-FC
J19381     END-IF.
J19381
J19381     MOVE HR11F5-PRS-COMPANY         TO DB-COMPANY.
J19381     MOVE HR11F5-EMPLOYEE            TO DB-EMPLOYEE.
J19381     PERFORM 840-FIND-EMPSET1.
J19381
J19381* HEADER FIELDS
J19381     MOVE EMP-COMPANY                TO HR11F5-PRS-COMPANY.
J19381     MOVE EMP-EMPLOYEE               TO HR11F5-EMPLOYEE.
J19381     MOVE ZEROES                     TO HR11F5-EFFECT-DATE.
J19381     MOVE EMP-WORK-COUNTRY           TO HR11F5-COUNTRY-CODE.
J19381     IF (EMP-WORK-COUNTRY = "IN")
J19381         MOVE "I1"                   TO HR11F5-GED-TOPIC
J19381     ELSE
J19381         MOVE "G1"                   TO HR11F5-GED-TOPIC
J19381     END-IF.
J19381
J19381     IF (HR11F5-FC = "-")
J19381         MOVE HR11F5-COUNTRY-CODE        TO DB-COUNTRY-CODE
J19381         MOVE HR11F5-GED-TOPIC           TO DB-TOPIC
J19381         MOVE HR11F5-GED-FLD-NBR (1)     TO DB-FLD-NBR
J19381                                            HR11F5-PT-GED-FLD-NBR
J19381         INITIALIZE HR11WS-NO-MORE-REC-SW
J19381
J19381         PERFORM 850-FIND-NLT-GEDSET1
J19381         PERFORM 870-FIND-PREV-GEDSET1
J19381         IF  (GEMPDICT-NOTFOUND)
J19381         OR  (GED-COUNTRY-CODE NOT = DB-COUNTRY-CODE)
J19381         OR  (GED-TOPIC        NOT = DB-TOPIC)
J19381             SET HR11WS-NO-MORE-REC  TO TRUE
J19381         END-IF
J19381         INITIALIZE HR11F5-DETAIL-DATA
J19381
J19381         PERFORM 610-MOVE-DTL-TO-SCREEN
J19381         THRU    610-END
J19381             VARYING I1 FROM 15 BY NEGATIVE-ONE
J19381             UNTIL  (I1           < 1)
J19381             OR     (GEMPDICT-NOTFOUND)
J19381             OR     (HR11WS-NO-MORE-REC)
J19381
J19381         IF  (I1 NOT < 1)
J19381             MOVE I1                 TO I7
J19381             ADD 1                   TO I7
J19381             MOVE 1                  TO I1
J19381             PERFORM UNTIL (I7 > 15)
J19381                 MOVE HR11F5-DETAIL-LINE (I7)
J19381                                     TO HR11F5-DETAIL-LINE (I1)
J19381                 INITIALIZE HR11F5-DETAIL-LINE (I7)
J19381                 ADD 1               TO I1
J19381                 ADD 1               TO I7
J19381             END-PERFORM
J19381             MOVE "+"                    TO HR11F5-FC
J19381             MOVE HR11F5-COUNTRY-CODE    TO DB-COUNTRY-CODE
J19381             MOVE HR11F5-GED-TOPIC       TO DB-TOPIC
J19381             MOVE HR11F5-PT-GED-FLD-NBR  TO DB-FLD-NBR
J19381             INITIALIZE HR11WS-NO-MORE-REC-SW
J19381
J19381             PERFORM 850-FIND-NLT-GEDSET1
J19381             IF  (GEMPDICT-NOTFOUND)
J19381             OR  (GED-COUNTRY-CODE NOT = DB-COUNTRY-CODE)
J19381             OR  (GED-TOPIC        NOT = DB-TOPIC)
J19381                 SET HR11WS-NO-MORE-REC  TO TRUE
J19381             END-IF
J19381
J19381             PERFORM 610-MOVE-DTL-TO-SCREEN
J19381             THRU    610-END
J19381                 VARYING I1 FROM I1 BY 1
J19381                 UNTIL  (I1           > 15)
J19381                 OR     (GEMPDICT-NOTFOUND)
J19381                 OR     (HR11WS-NO-MORE-REC)
J19381                 OR     (GED-COUNTRY-CODE NOT = DB-COUNTRY-CODE)
J19381             IF  (GEMPDICT-NOTFOUND)
J19381             OR  (HR11WS-NO-MORE-REC)
J19381             OR  (GED-COUNTRY-CODE NOT = DB-COUNTRY-CODE)
J19381                 MOVE ZEROES         TO HR11F5-PT-GED-FLD-NBR
J19381             ELSE
J19381                 MOVE GED-FLD-NBR    TO HR11F5-PT-GED-FLD-NBR
J19381             END-IF
J19381         END-IF
J19381     ELSE
J19381         MOVE HR11F5-COUNTRY-CODE    TO DB-COUNTRY-CODE
J19381         MOVE HR11F5-GED-TOPIC       TO DB-TOPIC
J19381         MOVE HR11F5-PT-GED-FLD-NBR  TO DB-FLD-NBR
J19381         INITIALIZE HR11WS-NO-MORE-REC-SW
J19381
J19381         PERFORM 850-FIND-NLT-GEDSET1
J19381         IF  (GEMPDICT-NOTFOUND)
J19381         OR  (GED-COUNTRY-CODE NOT = DB-COUNTRY-CODE)
J19381         OR  (GED-TOPIC        NOT = DB-TOPIC)
J19381             SET HR11WS-NO-MORE-REC  TO TRUE
J19381         END-IF
J19381
J19381         INITIALIZE HR11F5-DETAIL-DATA
J19381
J19381         PERFORM 610-MOVE-DTL-TO-SCREEN
J19381         THRU    610-END
J19381             VARYING I1 FROM 1 BY 1
J19381             UNTIL  (I1           > 15)
J19381             OR     (GEMPDICT-NOTFOUND)
J19381             OR     (HR11WS-NO-MORE-REC)
J19381             OR     (GED-COUNTRY-CODE NOT = DB-COUNTRY-CODE)
J19381
J19381         IF  (GEMPDICT-NOTFOUND)
J19381         OR  (HR11WS-NO-MORE-REC)
J19381         OR  (GED-COUNTRY-CODE NOT = DB-COUNTRY-CODE)
J19381             IF  (HR11F5-GED-FLD-NBR (1) NOT = ZEROES)
J19381                 MOVE HR11F5-GED-FLD-NBR (1)
J19381                                     TO HR11F5-PT-GED-FLD-NBR
J19381             ELSE
J19381                 MOVE ZEROES         TO HR11F5-PT-GED-FLD-NBR
J19381             END-IF
J19381             SET HR11WS-NO-MORE-REC  TO TRUE
J19381         ELSE
J19381             MOVE GED-FLD-NBR        TO HR11F5-PT-GED-FLD-NBR
J19381         END-IF
J19381     END-IF.
J19381
J19381 600-END.
J19381     EXIT.
J19381
J19381******************************************************************
J19381 610-MOVE-DTL-TO-SCREEN.
J19381******************************************************************
J19381                                 
J19381* LINE DETAIL
J19381     MOVE GED-NAME                   TO HR11F5-GED-FLD-NAME (I1).
J19381     MOVE GED-FLD-NBR                TO HR11F5-GED-FLD-NBR  (I1).
J19381     MOVE SPACES                     TO HR11F5-GEP-DATA     (I1).
J19381     MOVE "V"                        TO HR11F5-SELECT-BTN   (I1).
J19381
J19381     MOVE HR11F5-PRS-COMPANY         TO DB-COMPANY.
J19381     MOVE HR11F5-EMPLOYEE            TO DB-EMPLOYEE.
J19381     MOVE HR11F5-GED-TOPIC           TO DB-TOPIC.
J19381     MOVE HR11F5-GED-FLD-NBR (I1)    TO DB-FLD-NBR.
J19381     MOVE ZEROES                     TO DB-SEQ-NBR.
J19381     PERFORM 840-FIND-GEPSET1.
J19381     IF  (GEMPPARM-FOUND)
J19381         IF  (GEP-FIELD-TYPE = "A")
J19381             MOVE GEP-A-VALUE        TO HR11F5-GEP-DATA     (I1)
J19381         END-IF
J19381         IF  (GEP-FIELD-TYPE = "N")
J19381             MOVE GEP-S-VALUE        TO PRWS-DEC-FIELD
J19381             MOVE GED-SIZE           TO PRWS-SIZE
J19381             MOVE GED-DECIMALS       TO PRWS-NBR-DECIMALS
J19381             PERFORM 770-YE-FORMAT-DEC-FIELD
J19381             MOVE PRWS-VALUE         TO HR11F5-GEP-DATA     (I1)
J19381         END-IF
J19381         IF  (GEP-FIELD-TYPE = "D")
J19381             MOVE GEP-D-VALUE        TO PRWS-DATE-8-FIELD
J19381             PERFORM 781-YE-FORMAT-DATE-FIELD
J19381             MOVE PRWS-VALUE         TO HR11F5-GEP-DATA     (I1)
J19381         END-IF
J19381     END-IF.
J19381
J19381     IF (HR11F5-FC = "-")
J19381         PERFORM 870-FIND-PREV-GEDSET1
J19381     ELSE
J19381         PERFORM 860-FIND-NEXT-GEDSET1
J19381     END-IF.
J19381
J19381 610-END.
J19381     EXIT.
J19381
J19381******************************************************************
J19381 700-MOVE-SCR-TO-HRGEP.
J19381******************************************************************
J19381
J19381* HEADER FIELDS                                    
J19381     MOVE HR11F5-PRS-COMPANY         TO HRGEP-COMPANY.
J19381     MOVE HR11F5-PRS-COMPANY-FN      TO HRGEP-COMPANY-FN.
J19381     MOVE HR11F5-EMPLOYEE            TO HRGEP-EMPLOYEE.
J19381     MOVE HR11F5-EMPLOYEE-FN         TO HRGEP-EMPLOYEE-FN.
J19381     MOVE HR11F5-EFFECT-DATE         TO HRGEP-EFFECT-DATE.
J19381     MOVE HR11F5-EFFECT-DATE-FN      TO HRGEP-EFFECT-DATE-FN.
J19381
J19381* DETAIL RECORD
J19381     MOVE HR11F5-LINE-FC (I1)        TO HRGEP-FC.
J19381     MOVE HR11F5-LINE-FC-FN (I1)     TO HRGEP-FC-FN.
J19381     MOVE HR11F5-GED-FLD-NBR (I1)    TO HRGEP-FLD-NBR.
J19381     MOVE HR11F5-GED-FLD-NBR-FN (I1) TO HRGEP-FLD-NBR-FN.
J19381     MOVE HR11F5-GEP-DATA (I1)       TO HRGEP-DATA.
J19381     MOVE HR11F5-GEP-DATA-FN (I1)    TO HRGEP-DATA-FN.
J19381
J19381 700-END.
J19381     EXIT.
J19381
J19381******************************************************************
J19381 HR11S5-TRANSACTION-END.
J19381******************************************************************
