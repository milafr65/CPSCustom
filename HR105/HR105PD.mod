******* HR105PD 9 <3500162063>
      ******************************************************************
      *                            HR105PD                             *
      ******************************************************************
      *                                                                *
      *  JT#       TAG     DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  654204 | J54204 | FIXED ISSUE ON EMPLOYEE SEARCH USING        *
      *         |        | PROCESS LEVEL AND DEPARTMENT IN SCREEN.     *
      *  ------   ------   ------------------------------------------  *
      *  707198 | J07198 | CORRECTED LOGIC FOR USE OF SCREEN FIELD     *
      *         |        | END-DATE.                                   *
      *  ------   ------   ------------------------------------------  *
      ******************************************************************
      *               M O D I F I C A T I O N   L O G:                 *
      ******************************************************************
      *  Modified by MARK GLISSEN - MG1103                             *
      ******************************************************************
      *  11/03/03  - MODIFIED REPORT TO CONTAIN TIME STAMP.  MODIFIED  *
      *              SORTING ORDER TO CONTAIN DATE/TIME STAMP FIRST.   *
      *  10/29/10  - READDED ABOVE CUSTOMIZATIONS AFTER 9.0 APPS       *
      *              UPGRADE.  HR105PD AND HR105.RPT WERE MODIFIED     *
      *  08/25/11  - READDED ABOVE CUSTOMIZATIONS AFTER 9.0.1 APPS     *
      *              UPGRADE.  HR105PD AND HR105.RPT WERE MODIFIED     *
      *              ALSO ADDED TIME-STAMP FIELD TO SORT FILE IN       *
      *              WORKDEF.                                          *
      ******************************************************************
000100******************************************************************
000200 050-EDIT-PARAMETERS SECTION 10.
000300******************************************************************
000400 050-START.
000500
000600     MOVE PRM-COMPANY            TO DB-COMPANY.
000700     MOVE SPACES                 TO DB-PROCESS-LEVEL.
000800     PERFORM 840-FIND-PRSSET1.
000900
001000     IF (PRSYSTEM-NOTFOUND)
001100         MOVE PRM-COMPANY    TO CRT-ERR-VAR1
001200         MOVE 100            TO CRT-ERROR-NBR
001300         PERFORM 780-PRINT-ERROR-MSG
001400         GO TO 050-END
001500     ELSE
001600         MOVE PRS-COMPANY              TO G1-LOG-COMPANY
001700         MOVE PRS-NAME                 TO G1-PRS-NAME.

J03356* There cannot be duplicate employee numbers on the screen
J03356     SET NOT-DUPLICATE-EMPLOYEE TO TRUE.
J03356     PERFORM
J03356         VARYING I1 FROM 1 BY 1
J03356         UNTIL  (I1 > 5)
J03356         OR     (DUPLICATE-EMPLOYEE)
J03356            PERFORM
J03356             VARYING I2 FROM 1 BY 1
J03356              UNTIL  (I2 > 5)
J03356              OR     (DUPLICATE-EMPLOYEE)
J03356                 IF (I1 NOT = I2)
J03356                 AND (PRM-EMPLOYEE (I1) = PRM-EMPLOYEE (I2))
J03356                      IF (PRM-EMPLOYEE (I1) NOT = ZEROES)
J03356                      OR (PRM-EMPLOYEE (I2) NOT = ZEROES)
J03356                          SET DUPLICATE-EMPLOYEE TO TRUE
J03356                          MOVE 120 TO CRT-ERROR-NBR
J03356                          PERFORM 780-PRINT-ERROR-MSG
J03356                          GO TO 050-END
J03356                      END-IF
J03356                 END-IF
J03356            END-PERFORM
J03356     END-PERFORM.
001800
001900     IF  (PRM-RUN-OPTION = 1)
002000     AND (PRM-EMPLOYEE (1) = ZEROS)
002100     AND (PRM-EMPLOYEE (2) = ZEROS)
002200     AND (PRM-EMPLOYEE (3) = ZEROS)
002300     AND (PRM-EMPLOYEE (4) = ZEROS)
002400     AND (PRM-EMPLOYEE (5) = ZEROS)
002500         MOVE 101            TO CRT-ERROR-NBR
002600         PERFORM 780-PRINT-ERROR-MSG.
002700
002800     IF  (PRM-RUN-OPTION = 1)
002900     AND ((PRM-EMPLOYEE (1) NOT = ZEROS)
003000     OR  (PRM-EMPLOYEE (2) NOT = ZEROS)
003100     OR  (PRM-EMPLOYEE (3) NOT = ZEROS)
003200     OR  (PRM-EMPLOYEE (4) NOT = ZEROS)
003300     OR  (PRM-EMPLOYEE (5) NOT = ZEROS))
003400         PERFORM 055-EDIT-EMPLOYEES
003500             VARYING I1 FROM 1 BY 1
003600             UNTIL   (I1 > 5).
003700
003800     IF  (PRM-END-DATE NOT = ZEROS)
003900     AND (PRM-BEG-DATE > PRM-END-DATE)
004000         MOVE 102            TO CRT-ERROR-NBR
004100         PERFORM 780-PRINT-ERROR-MSG 
J03356         GO TO 050-END.
004200
004300     IF (PRM-END-DATE = ZEROS)
J07198         MOVE WS-HIGH-VALUES TO PRM-END-DATE
J07198     END-IF.
J07198*        IF (PRM-BEG-DATE NOT = ZEROS)
J07198*            MOVE PRM-BEG-DATE   TO PRM-END-DATE
J07198*        ELSE
J07198*            MOVE WS-HIGH-VALUES TO PRM-END-DATE.
J03356           
J03356     IF (PRM-RUN-OPTION = 3)
J03356     AND (PRM-GROUP-NAME = SPACES)
J03356         MOVE 106            TO CRT-ERROR-NBR
J03356         PERFORM 780-PRINT-ERROR-MSG
J03356         GO TO 050-END.
J03356
J03356     IF (PRM-RUN-OPTION = 4)
J03356     AND (PRM-PROCESS-LEVEL = SPACES)
J03356         MOVE 107            TO CRT-ERROR-NBR
J03356         PERFORM 780-PRINT-ERROR-MSG
J03356         GO TO 050-END.
J03356
J03356     IF (PRM-GROUP-NAME NOT = SPACES)
J03356         MOVE PRM-COMPANY           TO DB-COMPANY
J03356         MOVE PRM-GROUP-NAME        TO DB-GROUP-NAME
J03356         PERFORM 840-FIND-PRGSET1
J03356         IF (PERSGROUP-NOTFOUND)
J03356             MOVE 109               TO CRT-ERROR-NBR
J03356             PERFORM 780-PRINT-ERROR-MSG
J03356             GO TO 050-END 
J03356         END-IF
J03356     END-IF.
J03356
J03356     IF (PRM-PROCESS-LEVEL    NOT = SPACES)
J03356         MOVE PRM-COMPANY             TO DB-COMPANY
J03356         MOVE PRM-PROCESS-LEVEL       TO DB-PROCESS-LEVEL
J03356         PERFORM 840-FIND-PRSSET1
J03356         IF (PRSYSTEM-NOTFOUND)
J03356             MOVE 110                 TO CRT-ERROR-NBR
J03356             PERFORM 780-PRINT-ERROR-MSG
J03356             GO TO 050-END 
J03356         END-IF 
J03356     END-IF.
J03356
J03356     IF (PRM-PROCESS-LEVEL = SPACES)
J03356     AND (PRM-DEPARTMENT NOT = SPACES)
J03356          MOVE 111                 TO CRT-ERROR-NBR
J03356          PERFORM 780-PRINT-ERROR-MSG
J03356          GO TO 050-END
J03356     END-IF.
J03356
J03356     IF (PRM-DEPARTMENT    NOT = SPACES)
J03356         MOVE PRM-COMPANY             TO DB-COMPANY
J03356         MOVE PRM-PROCESS-LEVEL       TO DB-PROCESS-LEVEL
J03356         MOVE PRM-DEPARTMENT          TO DB-DEPARTMENT
J03356         PERFORM 840-FIND-DPTSET1
J03356         IF (DEPTCODE-NOTFOUND)
J03356             MOVE 112                 TO CRT-ERROR-NBR
J03356             PERFORM 780-PRINT-ERROR-MSG
J03356             GO TO 050-END 
J03356         END-IF
J03356     END-IF. 
J03356
J03356* There cannot be duplicate Field Numbers on the screen
J03356     SET NOT-DUPLICATE-FLD-NBR TO TRUE.     
J03356     PERFORM
J03356         VARYING I1 FROM 1 BY 1
J03356         UNTIL  (I1 > 8)
J03356         OR     (DUPLICATE-FLD-NBR) 
J03356               PERFORM                 
J03356                  VARYING I2 FROM 1 BY 1
J03356                      UNTIL  (I2 > 8)
J03356                      OR     (DUPLICATE-FLD-NBR)
J03356                         IF (I1 NOT = I2)
J03356                         AND (PRM-FLD-NBR (I1) = PRM-FLD-NBR (I2))
J03356                             IF (PRM-FLD-NBR (I1) NOT = ZEROES)
J03356                             OR (PRM-FLD-NBR (I2) NOT = ZEROES)
J03356                                 SET DUPLICATE-FLD-NBR TO TRUE    
J03356                                 MOVE 113 TO CRT-ERROR-NBR
J03356                                 PERFORM 780-PRINT-ERROR-MSG
J03356                                 GO TO 050-END
J03356                             END-IF
J03356                          END-IF
J03356               END-PERFORM
J03356     END-PERFORM.
J03356
J03356     IF ((PRM-EMPLOYEE (1) NOT = ZEROES)
J03356     OR  (PRM-EMPLOYEE (2) NOT = ZEROES)
J03356     OR  (PRM-EMPLOYEE (3) NOT = ZEROES)
J03356     OR  (PRM-EMPLOYEE (4) NOT = ZEROES)
J03356     OR  (PRM-EMPLOYEE (5) NOT = ZEROES))
J03356         IF (PRM-RUN-OPTION NOT = 1)
J03356              MOVE 114                 TO CRT-ERROR-NBR
J03356              PERFORM 780-PRINT-ERROR-MSG
J03356              GO TO 050-END
J03356         END-IF
J03356     END-IF.
J03356
J03356*    IF (PRM-RUN-OPTION NOT = 3) 
J03356*    AND (PRM-GROUP-NAME NOT = SPACES)
J03356*         MOVE 115                 TO CRT-ERROR-NBR
J03356*         PERFORM 780-PRINT-ERROR-MSG
J03356*         GO TO 050-END
J03356*    END-IF.
J03356
J03356     IF (PRM-RUN-OPTION NOT = 4 AND 5)
J03356     AND (PRM-PROCESS-LEVEL NOT = SPACES)
J03356          MOVE 116                 TO CRT-ERROR-NBR
J03356          PERFORM 780-PRINT-ERROR-MSG
J03356          GO TO 050-END
J03356     END-IF.
J03356
004900 050-END.
005000
005100******************************************************************
005200 055-EDIT-EMPLOYEES  SECTION 10.
005300******************************************************************
005400 055-START.
005500
005600     IF (PRM-EMPLOYEE (I1)       = ZEROS)
005700         GO TO 055-END.
005800
005900     MOVE PRM-EMPLOYEE (I1)      TO DB-EMPLOYEE.
006000     PERFORM 840-FIND-EMPSET1.
006100     IF (EMPLOYEE-NOTFOUND)
006200         MOVE PRM-EMPLOYEE (I1)  TO CRT-ERR-VAR1
006300         MOVE 103                TO CRT-ERROR-NBR
006400         PERFORM 780-PRINT-ERROR-MSG.
006500
006600 055-END.
006700
006800******************************************************************
006900 100-PROGRAM-CONTROL SECTION 10.
007000******************************************************************
007100 100-START.
007200
007300     MOVE 104                    TO CRT-MSG-NBR.
007400     PERFORM 780-DISPLAY-MSG.
           MOVE 200                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WS-HER-PRE-VALUE.
007500
007600     SORT SORT-FILE
MG1103         ASCENDING KEY    DSR-DATE-STAMP
MG1103                          DSR-TIME-STAMP
MG1103                          DSR-EMPLOYEE
ACS001*        ASCENDING KEY    DSR-EMPLOYEE
007800                          DSR-EFFECT-DATE
007900                          DSR-POS-LEVEL
008000                          DSR-ITEM-NAME
008100                          DSR-SEQ-NBR
008200         INPUT  PROCEDURE 1000-EXTRACT-SECTION
008300         OUTPUT PROCEDURE 2000-PRINT-SECTION.
008400
008500 100-END.
008600
008700******************************************************************
008800 1000-EXTRACT-SECTION SECTION 50.
008900******************************************************************
009000 1000-START.
009100
009200     IF (PRM-RUN-OPTION = 1)
               IF (PRM-HISTORY-ERRORS = "1" OR "2")
009300             PERFORM 1050-LOAD-EMPLOYEES
009400             THRU    1050-END
009500                 VARYING I2 FROM 1 BY 1
009600                 UNTIL   (I2 > 5)
               END-IF
               IF (PRM-HISTORY-ERRORS = "2" OR "3")
009300             PERFORM 1075-LOAD-HER-EMPLOYEES
009400             THRU    1075-END
009500                 VARYING I2 FROM 1 BY 1
009600                 UNTIL   (I2 > 5)
               END-IF.
009700
J00356     IF (PRM-RUN-OPTION = 2)
               IF (PRM-HISTORY-ERRORS = "1" OR "2")
009900             MOVE HRHSET1-COMPANY    TO WS-DB-BEG-RNG
010000             PERFORM 850-FIND-BEGRNG-HRHSET1
010100             PERFORM 1100-LOAD-SORT-FILE
010200             THRU    1100-END
010300                 UNTIL (HRHISTORY-NOTFOUND)
010400             MOVE HREMP-PAY-RATE-DN  TO DB-FLD-NBR
010500             PERFORM 840-FIND-PADSET1
010600             MOVE PRHSET1-COMPANY    TO WS-DB-BEG-RNG
010700             PERFORM 850-FIND-BEGRNG-PRHSET1
010800             PERFORM 1200-LOAD-RATE-CHANGES
010900             THRU    1200-END
011000                 UNTIL (PRRATEHIST-NOTFOUND)
               END-IF
               IF (PRM-HISTORY-ERRORS = "2" OR "3")
009900             MOVE HERSET1-COMPANY    TO WS-DB-BEG-RNG
010000             PERFORM 850-FIND-BEGRNG-HERSET1
010100             PERFORM 1150-LOAD-HER-SORT-FILE
010200             THRU    1150-END
010300                 UNTIL (HISTERR-NOTFOUND)
               END-IF.

J03356* Run by Employee Group.
J03356     IF (PRM-RUN-OPTION = 3)
J03356     AND (PRM-GROUP-NAME NOT = SPACES)
J03356          MOVE PRM-GROUP-NAME           TO DB-GROUP-NAME
J03356          MOVE PGESET1-GROUP-NAME       TO WS-DB-BEG-RNG
J03356          IF (PRM-HISTORY-ERRORS = "1" OR "2")
J03356             PERFORM 850-FIND-BEGRNG-PGESET1
J03356             IF (PGEMPLOYEE-FOUND)
JJ3356                  PERFORM 1060-LOAD-EMPLOYEE-GRP
J03356                  THRU    1060-END
J03356                   UNTIL  (PGEMPLOYEE-NOTFOUND)
J03356              END-IF
J03356          END-IF
J03356          IF (PRM-HISTORY-ERRORS = "2" OR "3")
J03356              PERFORM 850-FIND-BEGRNG-PGESET1
J03356              IF (PGEMPLOYEE-FOUND)
J03356                 PERFORM 1085-LOAD-HER-EMPLOYEE-GRP
J03356                 THRU    1085-END
J03356                   UNTIL (PGEMPLOYEE-NOTFOUND)
J03356              END-IF 
J03356          END-IF
J03356     END-IF.
J03356
J03356* Run by Process Level alone or Process Level and Department.
J03356    IF (PRM-RUN-OPTION = 4)
J03356    AND (PRM-PROCESS-LEVEL NOT = SPACES)
J03356         MOVE PRM-PROCESS-LEVEL      TO DB-PROCESS-LEVEL
J54204         IF (PRM-DEPARTMENT      NOT = SPACES)
J03356             MOVE PRM-DEPARTMENT     TO DB-DEPARTMENT
J03356             MOVE EMPSET2-DEPARTMENT TO WS-DB-BEG-RNG
J54204         ELSE
J54204             MOVE EMPSET2-PROCESS-LEVEL
J54204                                     TO WS-DB-BEG-RNG
J54204         END-IF
J03356         IF (PRM-HISTORY-ERRORS = "1" OR "2")
J03356             PERFORM 850-FIND-BEGRNG-EMPSET2
J03356             IF (EMPLOYEE-FOUND)
JJ3356                 PERFORM 1070-LOAD-PROC-LEVEL
J03356                 THRU    1070-END
J03356                  UNTIL  (EMPLOYEE-NOTFOUND)
J03356             END-IF
J03356         END-IF 
J03356         IF (PRM-HISTORY-ERRORS = "2" OR "3")
J03356             PERFORM 850-FIND-BEGRNG-EMPSET2
J03356             IF (EMPLOYEE-FOUND)
J03356                 PERFORM 1095-LOAD-HER-PROC-LEVEL
J03356                 THRU    1095-END
J03356                   UNTIL (EMPLOYEE-NOTFOUND)
J03356             END-IF
J03356         END-IF
J03356     END-IF.
011100
011200     GO TO 1000-END.
011300
011400******************************************************************
011500 1050-LOAD-EMPLOYEES.
011600******************************************************************
011700
011800     IF (PRM-EMPLOYEE (I2) = ZEROS)
011900         GO TO 1050-END.
012000
012100     MOVE PRM-EMPLOYEE (I2)      TO DB-EMPLOYEE.
012200     MOVE ZEROS                  TO DB-OBJ-ID.
012300     MOVE HRHSET1-OBJ-ID         TO WS-DB-BEG-RNG.
012400     PERFORM 850-FIND-BEGRNG-HRHSET1.
012500     PERFORM 1100-LOAD-SORT-FILE
012600     THRU    1100-END
012700         UNTIL (HRHISTORY-NOTFOUND).
012800
012900     MOVE HREMP-PAY-RATE-DN      TO DB-FLD-NBR.
013000     PERFORM 840-FIND-PADSET1.
013100     MOVE PRM-EMPLOYEE (I2)      TO DB-EMPLOYEE.
013200     MOVE PRHSET1-EMPLOYEE       TO WS-DB-BEG-RNG.
013300     PERFORM 850-FIND-BEGRNG-PRHSET1.
013400     PERFORM 1200-LOAD-RATE-CHANGES
013500     THRU    1200-END
013600         UNTIL (PRRATEHIST-NOTFOUND).
013700
013800 1050-END.
013900
011400******************************************************************
011500 1075-LOAD-HER-EMPLOYEES.
011600******************************************************************
011700
011800     IF (PRM-EMPLOYEE (I2) = ZEROS)
011900         GO TO 1075-END.
012000
012100     MOVE PRM-EMPLOYEE (I2)      TO DB-EMPLOYEE.
012200     MOVE ZEROS                  TO DB-OBJ-ID.
012300     MOVE HERSET1-OBJ-ID         TO WS-DB-BEG-RNG.
012400     PERFORM 850-FIND-BEGRNG-HERSET1.
012500     PERFORM 1150-LOAD-HER-SORT-FILE
012600     THRU    1150-END
012700         UNTIL (HISTERR-NOTFOUND).
012800
013800 1075-END.
013900
J03356******************************************************************
J03356 1060-LOAD-EMPLOYEE-GRP.
J03356******************************************************************
J03356
J03356     MOVE PGE-EMPLOYEE           TO DB-EMPLOYEE.
J03356     MOVE ZEROS                  TO DB-OBJ-ID.
J03356     MOVE HRHSET1-OBJ-ID         TO WS-DB-BEG-RNG.
J03356     PERFORM 850-FIND-BEGRNG-HRHSET1.
J03336     PERFORM 1100-LOAD-SORT-FILE
J03356     THRU    1100-END
J03356         UNTIL (HRHISTORY-NOTFOUND).
J03356
J03356     MOVE HREMP-PAY-RATE-DN      TO DB-FLD-NBR.
J03356     PERFORM 840-FIND-PADSET1.
J03356     MOVE PGE-EMPLOYEE           TO DB-EMPLOYEE.
J03356     MOVE PRHSET1-EMPLOYEE       TO WS-DB-BEG-RNG.
J03356     PERFORM 850-FIND-BEGRNG-PRHSET1.
J03356     PERFORM 1200-LOAD-RATE-CHANGES
J03356     THRU    1200-END
J03356         UNTIL (PRRATEHIST-NOTFOUND).
J03356
J03356     PERFORM 860-FIND-NXTRNG-PGESET1.
J03356
J03356 1060-END.
J03356
J03356******************************************************************
J03356 1085-LOAD-HER-EMPLOYEE-GRP. 
J03356******************************************************************
J03356
J03356     MOVE PGE-EMPLOYEE           TO DB-EMPLOYEE.
J03356     MOVE ZEROS                  TO DB-OBJ-ID.
J03356     MOVE HERSET1-OBJ-ID         TO WS-DB-BEG-RNG.
J03356     PERFORM 850-FIND-BEGRNG-HERSET1.
J03356     PERFORM 1150-LOAD-HER-SORT-FILE
J03356     THRU    1150-END
J03356         UNTIL (HISTERR-NOTFOUND).
J03356
J03356     PERFORM 860-FIND-NXTRNG-PGESET1.
J03356
J03356 1085-END.
J03356
J03356******************************************************************
J03356 1070-LOAD-PROC-LEVEL.
J03356******************************************************************
J03356
J03356     MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
J03356     MOVE ZEROS                  TO DB-OBJ-ID.
J03356     MOVE HRHSET1-OBJ-ID         TO WS-DB-BEG-RNG.
J03356     PERFORM 850-FIND-BEGRNG-HRHSET1.
J03336     PERFORM 1100-LOAD-SORT-FILE
J03356     THRU    1100-END
J03356         UNTIL (HRHISTORY-NOTFOUND).
J03356
J03356     MOVE HREMP-PAY-RATE-DN      TO DB-FLD-NBR.
J03356     PERFORM 840-FIND-PADSET1.
J03356     MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
J03356     MOVE PRHSET1-EMPLOYEE       TO WS-DB-BEG-RNG.
J03356     PERFORM 850-FIND-BEGRNG-PRHSET1.
J03356     PERFORM 1200-LOAD-RATE-CHANGES
J03356     THRU    1200-END
J03356         UNTIL (PRRATEHIST-NOTFOUND).
J03356
J03356     PERFORM 860-FIND-NXTRNG-EMPSET2.
J03356
J03356 1070-END.
J03356
J03356******************************************************************
J03356 1095-LOAD-HER-PROC-LEVEL.
J03356******************************************************************
J03356
J03356     MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
J03356     MOVE ZEROS                  TO DB-OBJ-ID.
J03356     MOVE HERSET1-OBJ-ID         TO WS-DB-BEG-RNG.
J03356     PERFORM 850-FIND-BEGRNG-HERSET1.
J03356     PERFORM 1150-LOAD-HER-SORT-FILE
J03356     THRU    1150-END
J03356         UNTIL (HISTERR-NOTFOUND).
J03356
J03356     PERFORM 860-FIND-NXTRNG-EMPSET2.
J03356
J03356 1095-END.
J03356
014000******************************************************************
014100 1100-LOAD-SORT-FILE.
014200******************************************************************
014300
014400     IF (HRH-DATA-TYPE NOT = "E")
014500         GO TO 1100-FIND-NEXT.
014600
           IF  ((PRM-DATE-TYPE  = "1")
014700     AND  (HRH-BEG-DATE   < PRM-BEG-DATE))
           OR  ((PRM-DATE-TYPE  = "2")
           AND  (HRH-DATE-STAMP < PRM-BEG-DATE))
014800         GO TO 1100-FIND-NEXT.
014900
           IF  ((PRM-DATE-TYPE  = "1")
015000     AND  (HRH-BEG-DATE   > PRM-END-DATE))
           OR  ((PRM-DATE-TYPE  = "2")
           AND  (HRH-DATE-STAMP > PRM-END-DATE))
015100         GO TO 1100-FIND-NEXT.

J03356     SET FLD-NBR-NOT-FOUND TO TRUE.
J03356     SET FLD-NBR-NOT-MATCH TO TRUE.
J03356     PERFORM
J03356       VARYING I4 FROM 1 BY 1
J03356       UNTIL (I4 > 8)
J03356       OR    (FLD-NBR-MATCH)
J03356          IF (PRM-FLD-NBR (I4) NOT = ZEROES)
J03356              SET FLD-NBR-FOUND TO TRUE
J03356              IF (HRH-FLD-NBR = PRM-FLD-NBR (I4))
J03356                  SET FLD-NBR-MATCH TO TRUE
J03356              END-IF
J03356          END-IF
J03356     END-PERFORM.
J03356     IF (FLD-NBR-FOUND)
J03356     AND (FLD-NBR-NOT-MATCH)
J03356         GO TO 1100-FIND-NEXT
J03356     END-IF.
J03356
J03356     IF (PRM-USER-ID NOT = SPACES)
J03356     AND (HRH-USER-ID NOT = PRM-USER-ID)
J03356         GO TO 1100-FIND-NEXT
J03356     END-IF.
015200
015800     MOVE HRH-BEG-DATE           TO SR-EFFECT-DATE.
015900     MOVE HRH-EMPLOYEE           TO SR-EMPLOYEE.
016000     MOVE HRH-POS-LEVEL          TO SR-POS-LEVEL.
016100     MOVE HRH-USER-ID            TO SR-USER-ID.
016200     MOVE HRH-SEQ-NBR            TO SR-SEQ-NBR.
016300     MOVE HRH-FLD-NBR            TO SR-FLD-NBR.
MG1103     MOVE HRH-TIME-STAMP         TO SR-TIME-STAMP.
016400     MOVE HRH-DATE-STAMP         TO SR-DATE-STAMP.
           INITIALIZE                     SR-HIST-ERR.
016500
016600     MOVE HRH-FLD-NBR            TO DB-FLD-NBR.
016700     PERFORM 840-FIND-PADSET1.
016800
           MOVE PAD-ITEM-NAME          TO CRT-PHRASE.
           MOVE WS-PHRASE-SIZE         TO CRT-PHRASE-SIZE.
           MOVE "N"                    TO CRT-PUT-DOTS.
           PERFORM 900-GET-PHRASE-XLT.
           MOVE CRT-PHRASE-XLT         TO SR-ITEM-NAME.
017300
017400     IF (PAD-DATA-TYPE = "A")
017500         MOVE HRH-A-VALUE        TO SR-NEW-VALUE
               MOVE SPACES             TO SR-CURRENCY-CODE
017600     ELSE
017700     IF (PAD-DATA-TYPE = "D")
017800         MOVE HRH-D-VALUE        TO HRWS-DATE-8-FIELD
017900         INITIALIZE HRWS-DATE-FIELD
018000         PERFORM 781-HR-FORMAT-DATE-FIELD
018100         MOVE HRWS-VALUE         TO SR-NEW-VALUE
               MOVE SPACES             TO SR-CURRENCY-CODE
018200     ELSE
018300         MOVE HRH-N-VALUE        TO HRWS-DEC-FIELD
               IF  (HRH-CURRENCY-CODE  NOT = SPACES)
               AND (PAD-CURRENCY-FLAG      = "Y")
                   MOVE HRH-CURR-ND    TO HRWS-NBR-DECIMALS
                   MOVE HRH-CURRENCY-CODE
                                       TO SR-CURRENCY-CODE
               ELSE
018400             MOVE PAD-DECIMALS   TO HRWS-NBR-DECIMALS
                   MOVE SPACES         TO SR-CURRENCY-CODE
               END-IF
018500         PERFORM 770-HR-FORMAT-DEC-FIELD
018600         MOVE HRWS-VALUE         TO SR-NEW-VALUE.
018700
018800     MOVE PAD-DATA-TYPE          TO SR-DATA-TYPE.
018900     MOVE PAD-DECIMALS           TO SR-DECIMALS.
           MOVE HRH-CURR-ND            TO SR-CURR-ND.
           MOVE PAD-CURRENCY-FLAG      TO SR-CURRENCY-FLAG.
019000     RELEASE SORT-REC FROM SR-SORT-REC.
019100
019200 1100-FIND-NEXT.
019300
019400     PERFORM 860-FIND-NXTRNG-HRHSET1.
019500
019600 1100-END.
019700
014000******************************************************************
014100 1150-LOAD-HER-SORT-FILE.
014200******************************************************************
014300
014400     IF (HER-DATA-TYPE NOT = "E")
014500         GO TO 1150-FIND-NEXT.
014600
           IF  ((PRM-DATE-TYPE  = "1")
014700     AND  (HER-EFFECT-DATE < PRM-BEG-DATE))
           OR  ((PRM-DATE-TYPE   = "2")
           AND  (HER-DATE-STAMP2 < PRM-BEG-DATE))
014800         GO TO 1150-FIND-NEXT.
014900
           IF  ((PRM-DATE-TYPE   = "1")
015000     AND  (HER-EFFECT-DATE > PRM-END-DATE))
           OR  ((PRM-DATE-TYPE   = "2")
           AND  (HER-DATE-STAMP2 > PRM-END-DATE))
015100         GO TO 1150-FIND-NEXT.
015200
J03356     SET FLD-NBR-NOT-FOUND TO TRUE.
J03356     SET FLD-NBR-NOT-MATCH TO TRUE.
J03356     PERFORM
J03356       VARYING I4 FROM 1 BY 1
J03356       UNTIL (I4 > 8)
J03356       OR    (FLD-NBR-MATCH)
J03356          IF (PRM-FLD-NBR (I4) NOT = ZEROES)
J03356              SET FLD-NBR-FOUND TO TRUE
J03356              IF (HER-FLD-NBR = PRM-FLD-NBR (I4))
J03356                  SET FLD-NBR-MATCH TO TRUE
J03356              END-IF
J03356          END-IF
J03356     END-PERFORM.
J03356     IF (FLD-NBR-FOUND)
J03356     AND (FLD-NBR-NOT-MATCH)
J03356         GO TO 1150-FIND-NEXT
J03356     END-IF.
J03356
J03356     IF (PRM-USER-ID NOT = SPACES)
J03356     AND (HRH-USER-ID NOT = PRM-USER-ID)
J03356         GO TO 1150-FIND-NEXT
J03356     END-IF.
J03356
015800     MOVE HER-EFFECT-DATE        TO SR-EFFECT-DATE.
015900     MOVE HER-EMPLOYEE           TO SR-EMPLOYEE.
016000     MOVE HER-POS-LEVEL          TO SR-POS-LEVEL.
016100     MOVE HER-USER-ID            TO SR-USER-ID.
016200     MOVE HER-SEQ-NBR            TO SR-SEQ-NBR.
016300     MOVE HER-FLD-NBR            TO SR-FLD-NBR.
016400     MOVE HER-DATE-STAMP2        TO SR-DATE-STAMP.
MG1103     MOVE HER-TIME-STAMP         TO SR-TIME-STAMP.
           MOVE "Y"                    TO SR-HIST-ERR.
016500
016600     MOVE HER-FLD-NBR            TO DB-FLD-NBR.
016700     PERFORM 840-FIND-PADSET1.
016800
           MOVE PAD-ITEM-NAME          TO CRT-PHRASE.
           MOVE WS-PHRASE-SIZE         TO CRT-PHRASE-SIZE.
           MOVE "N"                    TO CRT-PUT-DOTS.
           PERFORM 900-GET-PHRASE-XLT.
           MOVE CRT-PHRASE-XLT         TO SR-ITEM-NAME.
017300
           IF  (HER-CURRENCY-CODE      NOT = SPACES)
           AND (PAD-CURRENCY-FLAG          = "Y")
               MOVE HER-N-VALUE        TO HRWS-DEC-FIELD
               MOVE HER-CURR-ND        TO HRWS-NBR-DECIMALS
               MOVE HER-CURRENCY-CODE  TO SR-CURRENCY-CODE
               PERFORM 770-HR-FORMAT-DEC-FIELD
               MOVE HRWS-VALUE         TO SR-NEW-VALUE
           ELSE
               MOVE SPACES             TO SR-CURRENCY-CODE
017500         MOVE HER-NEW-VALUE      TO SR-NEW-VALUE.
018700
018800     MOVE PAD-DATA-TYPE          TO SR-DATA-TYPE.
018900     MOVE PAD-DECIMALS           TO SR-DECIMALS.
           MOVE HER-CURR-ND            TO SR-CURR-ND.
           MOVE PAD-CURRENCY-FLAG      TO SR-CURRENCY-FLAG.
019000     RELEASE SORT-REC FROM SR-SORT-REC.
019100
019200 1150-FIND-NEXT.
019300
019400     PERFORM 860-FIND-NXTRNG-HERSET1.
019500
019600 1150-END.
019700
019800******************************************************************
019900 1200-LOAD-RATE-CHANGES.
020000******************************************************************
020100
           IF  ((PRM-DATE-TYPE  = "1")
020200     AND  (PRH-BEG-DATE < PRM-BEG-DATE))
           OR  ((PRM-DATE-TYPE  = "2")
           AND  (PRH-DATE-STAMP < PRM-BEG-DATE))
020300         GO TO 1200-FIND-NEXT.
020400
           IF  ((PRM-DATE-TYPE  = "1")
020500     AND  (PRH-BEG-DATE > PRM-END-DATE))
           OR  ((PRM-DATE-TYPE  = "2")
           AND  (PRH-DATE-STAMP > PRM-END-DATE))
020600         GO TO 1200-FIND-NEXT.
020700
020800     IF (PRH-PAY-RATE = ZEROS)
020900         GO TO 1200-FIND-NEXT.

J03356     SET FLD-NBR-NOT-FOUND TO TRUE.
J03356     SET FLD-NBR-NOT-MATCH TO TRUE.
J03356     PERFORM
J03356       VARYING I4 FROM 1 BY 1
J03356       UNTIL (I4 > 8)
J03356       OR    (FLD-NBR-MATCH)
J03356          IF (PRM-FLD-NBR (I4) NOT = ZEROES)
J03356              SET FLD-NBR-FOUND TO TRUE
J03356              IF (HREMP-PAY-RATE-DN = PRM-FLD-NBR (I4))
J03356                  SET FLD-NBR-MATCH TO TRUE
J03356              END-IF
J03356          END-IF
J03356     END-PERFORM.
J03356     IF (FLD-NBR-FOUND)
J03356     AND (FLD-NBR-NOT-MATCH)
J03356         GO TO 1200-FIND-NEXT
J03356     END-IF.
J03356
J03356     IF (PRM-USER-ID NOT = SPACES)
J03356     AND (PRH-USER-ID NOT = PRM-USER-ID)
J03356         GO TO 1200-FIND-NEXT
J03356     END-IF.
021000
021100     MOVE PRH-BEG-DATE           TO SR-EFFECT-DATE.
021200     MOVE PRH-EMPLOYEE           TO SR-EMPLOYEE.
021300     MOVE PRH-POS-LEVEL          TO SR-POS-LEVEL.
021400     MOVE PRH-USER-ID            TO SR-USER-ID.
021500     MOVE PRH-SEQ-NBR            TO SR-SEQ-NBR.
021600     MOVE HREMP-PAY-RATE-DN      TO SR-FLD-NBR.
021700     MOVE PAD-ITEM-NAME          TO SR-ITEM-NAME.
021800     MOVE PRH-DATE-STAMP         TO SR-DATE-STAMP.
MG1103     MOVE PRH-TIME-STAMP         TO SR-TIME-STAMP.
           INITIALIZE                     SR-HIST-ERR.
021900
022000     MOVE PRH-PAY-RATE           TO HRWS-DEC-FIELD.
           IF  (PRH-CURRENCY-CODE      NOT = SPACES)
           AND (PAD-CURRENCY-FLAG          = "Y")
               MOVE PRH-CURR-ND        TO HRWS-NBR-DECIMALS
               MOVE PRH-CURRENCY-CODE  TO SR-CURRENCY-CODE
           ELSE
022100         MOVE PAD-DECIMALS       TO HRWS-NBR-DECIMALS
022100         MOVE PRH-CURRENCY-CODE  TO SR-CURRENCY-CODE.
022200     PERFORM 770-HR-FORMAT-DEC-FIELD.
022300     MOVE HRWS-VALUE             TO SR-NEW-VALUE.
022400
022500     MOVE PAD-DECIMALS           TO SR-DECIMALS.
022600     MOVE PAD-DATA-TYPE          TO SR-DATA-TYPE.
           MOVE PRH-CURR-ND            TO SR-CURR-ND.
           MOVE PAD-CURRENCY-FLAG      TO SR-CURRENCY-FLAG.
022700     RELEASE SORT-REC FROM SR-SORT-REC.
022800
022900 1200-FIND-NEXT.
023000
023100     PERFORM 860-FIND-NXTRNG-PRHSET1.
023200
023300 1200-END.
023400
023500******************************************************************
023600 1000-END.
023700******************************************************************
023800******************************************************************
023900 2000-PRINT-SECTION SECTION 50.
024000******************************************************************
024100 2000-START.
024200
024300     MOVE PRM-BEG-DATE           TO G1-BEG-DATE.
J07198     IF (PRM-END-DATE = 99999999)
J07198         MOVE ZEROES             TO G1-END-DATE               
J07198     ELSE
024400         MOVE PRM-END-DATE       TO G1-END-DATE
J07198     END-IF.
024500
024600     IF (PRM-PGBRK-EMP NOT = "Y") 
024700         MOVE GN1-LOG-COMPANY    TO RPT-GROUP-REQUEST
024800         PERFORM 700-PRINT-RPT-GRP.
024900
025000     MOVE WS-FALSE               TO WS-END-OF-FILE-SW.
025100
025200     RETURN SORT-FILE INTO SR-SORT-REC
025300         AT END
025400             MOVE WS-TRUE        TO WS-END-OF-FILE-SW
025500             MOVE 105            TO CRT-MSG-NBR
025600             PERFORM 780-PRINT-MSG.
025700
025800     PERFORM 2100-PRINT-REPORT
025900     THRU    2100-END
026000         UNTIL (END-OF-FILE).
026100
026200     GO TO 2000-END.
026300
026400******************************************************************
026500 2100-PRINT-REPORT.
026600******************************************************************
026700
026800     MOVE SR-EMPLOYEE            TO G2-LOG-EMPLOYEE
026900                                    DB-EMPLOYEE.
027000     PERFORM 840-FIND-EMPSET1.
027100     IF (EMPLOYEE-FOUND)
027200         MOVE EMP-COMPANY        TO CRT-COMPANY
027300         MOVE EMP-PROCESS-LEVEL  TO CRT-PROCESS-LEVEL
027400         PERFORM 700-HR-EMP-SECURITY
027500         IF (HRWS-EMP-SECURED)
027600             GO TO 2100-CONTINUE
027700         ELSE
027800             MOVE EMP-LAST-NAME     TO HRWS-LAST-NAME
027900             MOVE EMP-FIRST-NAME    TO HRWS-FIRST-NAME
028000             MOVE EMP-MIDDLE-INIT   TO HRWS-MIDDLE-INIT
                   MOVE EMP-LAST-NAME-PRE TO HRWS-LAST-NAME-PRE
                   MOVE EMP-NAME-SUFFIX   TO HRWS-NAME-SUFFIX
028100             PERFORM 750-HR-FORMAT-NAME
028200             MOVE HRWS-FORMAT-NAME  TO G2-EMP-NAME
028300     ELSE
028400         MOVE SPACES                TO G2-EMP-NAME.
028500
028600     IF (PRM-PGBRK-EMP = "Y") 
028700         MOVE GN1-LOG-COMPANY    TO RPT-GROUP-REQUEST
028800         PERFORM 700-PRINT-RPT-GRP.
028900     MOVE GN2-LOG-EMPLOYEE       TO RPT-GROUP-REQUEST.
029000     PERFORM 700-PRINT-RPT-GRP.
029100     MOVE GN3H-LOG-SEQ-NBR       TO RPT-GROUP-REQUEST.
029200     PERFORM 700-PRINT-RPT-GRP.
029300
029400 2100-CONTINUE.
029500     PERFORM 2110-PRINT-LOG-RECORDS
029600     THRU    2110-END
029700         UNTIL (END-OF-FILE)
029800         OR    (SR-EMPLOYEE      NOT = DB-EMPLOYEE).
029900
030000 2100-END.
030100
030200******************************************************************
030300 2110-PRINT-LOG-RECORDS.
030400******************************************************************
030500
030600     IF (HRWS-EMP-SECURED)
030700         GO TO 2110-READ-NEXT.
030800
030900     MOVE SR-EFFECT-DATE         TO G3-LOG-EFFECT-DATE.
031000     MOVE SR-POS-LEVEL           TO G3-LOG-POS-LEVEL.
031100     MOVE SR-ITEM-NAME           TO G3-PAD-ITEM-NAME.
031200     MOVE SR-NEW-VALUE           TO G3-LOG-NEW-VALUE.
           MOVE SR-CURRENCY-CODE       TO G3-LOG-CURRENCY-CODE.
MG1103     MOVE SR-TIME-STAMP          TO G3-LOG-TIME-STAMP.           
031300     MOVE SR-DATE-STAMP          TO G3-LOG-DATE-STAMP.
           MOVE SR-USER-ID             TO WS-USER-DBUIDKEY.
           PERFORM 900-GET-USER-DISPLAY-NAME.
P36862     PERFORM 7000-NAME-CHECK.
P36862     MOVE WS-CHECK-NAME-OUTPUT   TO G3-LOG-USER-ID.
031500     MOVE SPACES                 TO G3-LOG-PRE-VALUE
                                          G3-LOG-PRE-CURR-CODE.
031600
           IF (SR-HIST-ERR = "Y")
               MOVE WS-HER-PRE-VALUE   TO G3-LOG-PRE-VALUE
               MOVE SR-HIST-ERR        TO G3-LOG-ERR
           ELSE
               INITIALIZE                     G3-LOG-ERR
031700         IF (SR-FLD-NBR = HREMP-PAY-RATE-DN)
031800             MOVE PRM-COMPANY        TO DB-COMPANY
031900             MOVE SR-EMPLOYEE        TO DB-EMPLOYEE
032000             MOVE SR-POS-LEVEL       TO DB-POS-LEVEL
032100             MOVE SR-EFFECT-DATE     TO DB-BEG-DATE
032200             MOVE SR-SEQ-NBR         TO DB-SEQ-NBR
032300             PERFORM 850-FIND-NLT-PRHSET2
032400             PERFORM 860-FIND-NEXT-PRHSET2
032500             IF  (PRRATEHIST-FOUND)
032600             AND (PRH-COMPANY   = DB-COMPANY)
032700             AND (PRH-EMPLOYEE  = DB-EMPLOYEE)
032800             AND (PRH-POS-LEVEL = DB-POS-LEVEL)
032900                 MOVE PRH-PAY-RATE   TO HRWS-DEC-FIELD
033000                 MOVE SR-DECIMALS    TO HRWS-NBR-DECIMALS
033100                 PERFORM 770-HR-FORMAT-DEC-FIELD
033200                 MOVE HRWS-VALUE     TO G3-LOG-PRE-VALUE
                       MOVE PRH-CURRENCY-CODE
                                           TO G3-LOG-PRE-CURR-CODE
033300             END-IF
033400         ELSE
033500             MOVE PRM-COMPANY        TO DB-COMPANY
033600             MOVE SR-EMPLOYEE        TO DB-EMPLOYEE
033700             MOVE ZEROS              TO DB-OBJ-ID
033800             MOVE SR-FLD-NBR         TO DB-FLD-NBR
033900             MOVE SR-POS-LEVEL       TO DB-POS-LEVEL
034000             MOVE SR-EFFECT-DATE     TO DB-BEG-DATE
034100             MOVE SR-SEQ-NBR         TO DB-SEQ-NBR
034200             PERFORM 850-FIND-NLT-HRHSET2
034300             PERFORM 860-FIND-NEXT-HRHSET2
034400             IF  (HRHISTORY-FOUND)
034500             AND (HRH-COMPANY   = DB-COMPANY)
034600             AND (HRH-EMPLOYEE  = DB-EMPLOYEE)
034700             AND (HRH-OBJ-ID    = DB-OBJ-ID)
034800             AND (HRH-FLD-NBR   = DB-FLD-NBR)
034900             AND (HRH-POS-LEVEL = DB-POS-LEVEL)
035000                 IF (SR-DATA-TYPE = "A")
035100                     MOVE HRH-A-VALUE    TO G3-LOG-PRE-VALUE
035200                 ELSE
035300                 IF (SR-DATA-TYPE = "D")
035400                     MOVE HRH-D-VALUE    TO HRWS-DATE-8-FIELD
035500                     INITIALIZE HRWS-DATE-FIELD
035600                     PERFORM 781-HR-FORMAT-DATE-FIELD
035700                     MOVE HRWS-VALUE     TO G3-LOG-PRE-VALUE
035800                 ELSE
035900                   MOVE HRH-N-VALUE    TO HRWS-DEC-FIELD
                         MOVE HRH-CURRENCY-CODE
                                               TO G3-LOG-PRE-CURR-CODE
                         IF  (HRH-CURRENCY-CODE NOT = SPACES)
                         AND (SR-CURRENCY-FLAG     = "Y")
                           MOVE HRH-CURR-ND    TO HRWS-NBR-DECIMALS
                         ELSE
036000                     MOVE SR-DECIMALS    TO HRWS-NBR-DECIMALS
                         END-IF
036100                   PERFORM 770-HR-FORMAT-DEC-FIELD
036200                   MOVE HRWS-VALUE       TO G3-LOG-PRE-VALUE
                         END-IF.

           IF  (G3-LOG-NEW-VALUE       = SPACES)
           AND (G3-LOG-PRE-VALUE       = SPACES)
               GO TO 2110-READ-NEXT.
036300
036400     MOVE GN3D-LOG-SEQ-NBR       TO RPT-GROUP-REQUEST.
036500     PERFORM 700-PRINT-RPT-GRP.
036600
036700 2110-READ-NEXT.
036800     RETURN SORT-FILE INTO SR-SORT-REC
036900         AT END
037000             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
037100
037200 2110-END.
037300
037400******************************************************************
037500 2000-END.
037600******************************************************************
P36862******************************************************************
P36862 7000-NAME-CHECK                 SECTION.
P36862******************************************************************
P36862 7000-START.
P36862
P36862     MOVE WS-USER-DISPLAY-NAME   TO WS-CHECK-NAME-INPUT.
P36862     PERFORM
P36862         VARYING I1 FROM 1 BY 1
P36862         UNTIL  (I1 > 30)
P36862         OR     ( WS-CHECK-NAME-IN (I1)= "\")
P36862             CONTINUE
P36862     END-PERFORM.
P36862
P36862     IF (I1 > 30)
P36862         MOVE WS-CHECK-NAME-INPUT TO WS-CHECK-NAME-OUTPUT
P36862     ELSE
P36862         ADD 1 TO I1
P36862         MOVE 1 TO I2
P36862         PERFORM
P36862             VARYING I1 FROM I1 BY 1
P36862             UNTIL  (I1 > 30)
P36862                 MOVE WS-CHECK-NAME-IN (I1)
P36862                                 TO WS-CHECK-NAME-OUT (I2)
P36862                 ADD 1 TO I2
P36862         END-PERFORM
P36862     END-IF.
P36862
P36862******************************************************************
P36862 7000-END.
P36862******************************************************************
037700
037800
