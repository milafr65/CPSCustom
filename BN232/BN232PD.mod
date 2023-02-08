******* BN232PD 15 <1062113936>
      ******************************************************************
      *                              BN232                             *
      ******************************************************************
      *                                                                *
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  307540 | J07540 | DOMESTIC PARTNER                            *
      ******************************************************************

000100******************************************************************
000200 050-EDIT-PARAMETERS             SECTION 10.
000300******************************************************************
000400 050-START.
000500
000600     MOVE PRM-COMPANY            TO DB-COMPANY.
000700     MOVE SPACES                 TO DB-PROCESS-LEVEL.
000800     PERFORM 840-FIND-PRSSET1.
000900     PERFORM 840-FIND-BNCSET1.
001000
001100     IF (PRSYSTEM-NOTFOUND)
001200     OR (BNCOMPANY-NOTFOUND)
001300         MOVE 100                TO CRT-ERROR-NBR
001400         PERFORM 780-PRINT-ERROR-MSG
001500     ELSE
001700         MOVE PRS-NAME           TO G2-PRS-NAME
               MOVE PRS-ANNUAL-HOURS   TO WS-PRS-ANNUAL-HOURS.
001800
001900     IF (PRM-EMPLOYEE-SEQ        = SPACES)
002000         MOVE PRS-EMPLOYEE-SEQ   TO PRM-EMPLOYEE-SEQ.
002100
002200     MOVE PRM-PROCESS-LEVEL      TO BNWS-PROCESS-LEVEL.
002300     MOVE PRM-DEPARTMENT         TO BNWS-DEPARTMENT.
002400     MOVE PRM-USER-LEVEL         TO BNWS-USER-LEVEL.
002500     MOVE PRM-LOCAT-CODE         TO BNWS-LOCAT-CODE.
002600     MOVE PRM-SUPERVISOR         TO BNWS-SUPERVISOR.
002700     MOVE PRM-INCLUDE-FLAG       TO BNWS-INCLUDE-FLAG.
002800     MOVE PRM-GROUP-NAME         TO BNWS-GROUP-NAME.
002900
003000     PERFORM
003100         VARYING I1 FROM 1 BY 1
003200         UNTIL  (I1 > 10)
003300
003400         MOVE PRM-EMP-STATUS (I1) TO BNWS-EMP-STATUS (I1)
003500     END-PERFORM.
003600
003700     PERFORM 600-BN-EDIT-RUN-OPTIONS.
003800
003700*
003800**** SHIFT LEFT & CHECK FOR DUP PLAN TYPES
003900*
004000     PERFORM 052-EDIT-PLAN-TYPE
004100     THRU    052-END.
004200     IF (NO-ERROR-FOUND)
004300******** SHIFT LEFT & CHECK FOR DUP PLAN CODES
004400         PERFORM 054-EDIT-PLAN-CODE
004500         THRU    054-END.
004600
005300     IF  (PRM-BEG-EMPLOYEE       > PRM-END-EMPLOYEE)
005400     AND (PRM-BEG-EMPLOYEE       NOT = ZEROS)
005500     AND (PRM-END-EMPLOYEE       NOT = ZEROS)
005600         MOVE 102                TO CRT-ERROR-NBR
005700         PERFORM 780-PRINT-ERROR-MSG.
005800
005900     IF  (PRM-BEG-EMPLOYEE       NOT = ZEROS)
006000     AND (PRM-END-EMPLOYEE       = ZEROS)
006100         MOVE PRM-BEG-EMPLOYEE   TO DB-EMPLOYEE
006200         PERFORM 840-FIND-EMPSET1
006300         IF (EMPLOYEE-NOTFOUND)
006400             MOVE PRM-BEG-EMPLOYEE   TO CRT-ERR-VAR1
006500             MOVE 103                TO CRT-ERROR-NBR
006600             PERFORM 780-PRINT-ERROR-MSG.
006700
006800     IF  (PRM-BEG-EMPLOYEE       = ZEROS)
006900     AND (PRM-END-EMPLOYEE       NOT = ZEROS)
007000         MOVE 104                TO CRT-ERROR-NBR
007100         PERFORM 780-PRINT-ERROR-MSG.
007200
007300     IF (PRM-FROM-DATE > PRM-TO-DATE)
007400         MOVE 116                TO CRT-ERROR-NBR
007500         PERFORM 780-PRINT-ERROR-MSG.
007600
007700     MOVE PRM-FROM-DATE          TO WS-PLAN-YEAR.
007800     MOVE PRM-TO-DATE            TO WS-END-PLAN-YEAR.
007900
008000     IF  (WS-FROM-MM NOT < 1)
008100     AND (WS-FROM-MM NOT > 3)
008200         MOVE 1                  TO WS-QUARTER
008300         MOVE 0101               TO WS-END-OF-QUARTER-MMDD
008400         MOVE 0331               TO WS-BEG-OF-QUARTER-MMDD
008500         MOVE 2                  TO WS-QUARTER1
008600         MOVE 3                  TO WS-QUARTER2
008700         MOVE 4                  TO WS-QUARTER3
008800         MOVE ZEROS              TO WS-QUARTER4
008900         MOVE ZEROS              TO WS-QUARTER5
009000         MOVE ZEROS              TO WS-QUARTER6
009100     ELSE
009200     IF  (WS-FROM-MM NOT < 4)
009300     AND (WS-FROM-MM NOT > 6)
009400         MOVE 2                  TO WS-QUARTER
009500         MOVE 0401               TO WS-END-OF-QUARTER-MMDD
009600         MOVE 0631               TO WS-BEG-OF-QUARTER-MMDD
009700         MOVE 3                  TO WS-QUARTER1
009800         MOVE 4                  TO WS-QUARTER2
009900         MOVE ZEROS              TO WS-QUARTER3
010000         MOVE 1                  TO WS-QUARTER4
010100         MOVE ZEROS              TO WS-QUARTER5
010200         MOVE ZEROS              TO WS-QUARTER6
010300     ELSE
010400     IF  (WS-FROM-MM NOT < 7)
010500     AND (WS-FROM-MM NOT > 9)
010600         MOVE 3                  TO WS-QUARTER
010700         MOVE 0701               TO WS-END-OF-QUARTER-MMDD
010800         MOVE 0931               TO WS-BEG-OF-QUARTER-MMDD
010900         MOVE 4                  TO WS-QUARTER1
011000         MOVE ZEROS              TO WS-QUARTER2
011100         MOVE ZEROS              TO WS-QUARTER3
011200         MOVE 1                  TO WS-QUARTER4
011300         MOVE 2                  TO WS-QUARTER5
011400         MOVE ZEROS              TO WS-QUARTER6
011500     ELSE
011600     IF  (WS-FROM-MM NOT < 10)
011700     AND (WS-FROM-MM NOT > 12)
011800         MOVE 4                  TO WS-QUARTER
011900         MOVE 1001               TO WS-END-OF-QUARTER-MMDD
012000         MOVE 1231               TO WS-BEG-OF-QUARTER-MMDD
012100         MOVE ZEROS              TO WS-QUARTER1
012200         MOVE ZEROS              TO WS-QUARTER2
012300         MOVE ZEROS              TO WS-QUARTER3
012400         MOVE 1                  TO WS-QUARTER4
012500         MOVE 2                  TO WS-QUARTER5
012600         MOVE 3                  TO WS-QUARTER6.
012700
012800     IF  (WS-TO-MM NOT < 1)
012900     AND (WS-TO-MM NOT > 3)
013000         MOVE 1                  TO WS-TO-QUARTER
013100     ELSE
013200     IF  (WS-TO-MM NOT < 4)
013300     AND (WS-TO-MM NOT > 6)
013400         MOVE 2                  TO WS-TO-QUARTER
013500     ELSE
013600     IF  (WS-TO-MM NOT < 7)
013700     AND (WS-TO-MM NOT > 9)
013800         MOVE 3                  TO WS-TO-QUARTER
013900     ELSE
014000     IF  (WS-TO-MM NOT < 10)
014100     AND (WS-TO-MM NOT > 12)
014200         MOVE 4                  TO WS-TO-QUARTER.
014300
014400     MOVE WS-FROM-YY             TO WS-BEG-OF-QUARTER-YY.
014500*     COMPUTE WS-END-OF-QUARTER-YY = (WS-FROM-YY + 1).
014600     MOVE WS-TO-YY               TO WS-END-OF-QUARTER-YY.
014700
           GO TO 050-END.

007900******************************************************************
008000 052-EDIT-PLAN-TYPE.
008100******************************************************************
008200
008300*
008400**** SHIFT LEFT ALL PLAN TYPES
008500*
008600     PERFORM
008700         VARYING I1 FROM 1 BY 1
008800         UNTIL  (I1 > 11)
008900         OR     (I2 > 11)
009000
009100         PERFORM
009200             VARYING I1 FROM I1 BY 1
009300             UNTIL  (I1 > 11)
009400             OR     (PRM-PLAN-TYPE (I1)  = SPACES)
009500
009600             CONTINUE
009700         END-PERFORM
009800
009900         COMPUTE I2                      = I1
010000                                         + 1
010100         PERFORM
010200             VARYING I2 FROM I1 BY 1
010300             UNTIL  (I2 > 11)
010400             OR     (PRM-PLAN-TYPE (I2)  NOT = SPACES)
010500
010600             CONTINUE
010700         END-PERFORM
010800         IF (I2                          <= 11)
010900             IF (PRM-PLAN-TYPE (I2)      NOT = SPACES)
011000                 MOVE PRM-PLAN-TYPE (I2) TO PRM-PLAN-TYPE (I1)
011100                 INITIALIZE PRM-PLAN-TYPE (I2)
011200             END-IF
011300         END-IF
011400     END-PERFORM.
011500
011600*
011700**** CHECK FOR DUPS
011800*
011900     PERFORM
012000         VARYING I1 FROM 1 BY 1
012100         UNTIL  (I1 > 11)
012200         OR     (PRM-PLAN-TYPE (I1)      = SPACES)
012300         OR     (ERROR-FOUND)
012400
012500         COMPUTE I2                      = I1
012600                                         + 1
012700         PERFORM
012800             VARYING I2 FROM I2 BY 1
012900             UNTIL  (I2 > 11)
013000             OR     (PRM-PLAN-TYPE (I2)  = PRM-PLAN-TYPE (I1))
013100
013200             CONTINUE
013300         END-PERFORM
013400         IF (I2                          <= 11)
013500             IF (PRM-PLAN-TYPE (I2)      = PRM-PLAN-TYPE (I1))
013600**************** Duplicate plan type entered
013700                 MOVE 125                TO CRT-ERROR-NBR
                       MOVE PRM-PLAN-TYPE (I1) TO CRT-ERR-VAR1
013800                 MOVE WS-TRUE            TO WS-PARAMETER-ERROR
013900                 PERFORM 780-PRINT-ERROR-MSG
014000             END-IF
014100         END-IF
014200     END-PERFORM.
014300
014400     IF (I1                               = 1)
014500******** Select at least one type of benefit
014600         MOVE 101                        TO CRT-ERROR-NBR
014700         MOVE WS-TRUE                    TO WS-PARAMETER-ERROR
014800         PERFORM 780-PRINT-ERROR-MSG
014900     ELSE
015000         COMPUTE WS-LAST-PLAN-TYPE        = I1
015100                                          - 1.
015200
015300 052-END.
015400
015500******************************************************************
015600 054-EDIT-PLAN-CODE.
015700******************************************************************
015800
015900*
016000**** SHIFT LEFT ALL PLAN CODES
016100*
           INITIALIZE I2.

016200     PERFORM
016300         VARYING I1 FROM 1 BY 1
016400         UNTIL  (I1 > 11)
016500         OR     (I2 > 11)
016600
016700         PERFORM
016800             VARYING I1 FROM I1 BY 1
016900             UNTIL  (I1 > 11)
017000             OR     (PRM-PLAN-CODE (I1)  = SPACES)
017100
017200             CONTINUE
017300         END-PERFORM
017400
017500         COMPUTE I2                      = I1
017600                                         + 1
017700         PERFORM
017800             VARYING I2 FROM I1 BY 1
017900             UNTIL  (I2 > 11)
018000             OR     (PRM-PLAN-CODE (I2)  NOT = SPACES)
018100
018200             CONTINUE
018300         END-PERFORM
018400         IF (I2                          <= 11)
018500             IF (PRM-PLAN-CODE (I2)      NOT = SPACES)
018600                 MOVE PRM-PLAN-CODE (I2) TO PRM-PLAN-CODE (I1)
018700                 INITIALIZE PRM-PLAN-CODE (I2)
018800             END-IF
018900         END-IF
019000     END-PERFORM.
019100
019200*
019300**** CHECK FOR DUPS
019400*
019500     PERFORM
019600         VARYING I1 FROM 1 BY 1
019700         UNTIL  (I1 > 11)
019800         OR     (PRM-PLAN-CODE (I1)      = SPACES)
019900         OR     (ERROR-FOUND)
020000
020100         COMPUTE I2                      = I1
020200                                         + 1
020300         PERFORM
020400             VARYING I2 FROM I2 BY 1
020500             UNTIL  (I2 > 11)
020600             OR     (PRM-PLAN-CODE (I2)  = PRM-PLAN-CODE (I1))
020700
020800             CONTINUE
020900         END-PERFORM
021000         IF (I2                          <= 11)
021100             IF (PRM-PLAN-CODE (I2)      = PRM-PLAN-CODE (I1))
021200**************** Duplicate plan code entered
021300                 MOVE 126                TO CRT-ERROR-NBR
                       MOVE PRM-PLAN-CODE (I1) TO CRT-ERR-VAR1
021400                 MOVE WS-TRUE            TO WS-PARAMETER-ERROR
021500                 PERFORM 780-PRINT-ERROR-MSG
021600             END-IF
021700         END-IF
021800     END-PERFORM.
021900
022000     IF (ERROR-FOUND)
022100         GO TO 054-END.
022200
022300     COMPUTE WS-LAST-PLAN-CODE            = I1
022400                                          - 1.
022500
022600     IF (WS-LAST-PLAN-CODE                = ZEROES)
022700         GO TO 054-END.
022800
022900     INITIALIZE I9.
023000
023100     MOVE PRM-COMPANY                    TO DB-COMPANY.
023200
023300     PERFORM
023400         VARYING I1 FROM 1 BY 1
023500         UNTIL  (I1 > WS-LAST-PLAN-CODE)
               OR     (ERROR-FOUND)
023600
023700         SET WS-PLAN-NOTFOUND            TO TRUE
023800
023900         PERFORM
024000             VARYING I2 FROM 1 BY 1
024100             UNTIL  (I2 > WS-LAST-PLAN-TYPE)
                   OR     (ERROR-FOUND)
024200
024300             MOVE PRM-PLAN-TYPE (I2)     TO DB-PLAN-TYPE
024400             MOVE PRM-PLAN-CODE (I1)     TO DB-PLAN-CODE
024500             PERFORM 840-FIND-PLNSET1
024600             IF (PLAN-FOUND)
024700                 ADD 1                   TO I9
024800                 SET WS-PLAN-FOUND       TO TRUE
024900                 MOVE PRM-PLAN-TYPE (I2) TO WS-TBL-PLAN-TYPE (I9)
025000                 MOVE PRM-PLAN-CODE (I1) TO WS-TBL-PLAN-CODE (I9)
025100             END-IF
025200         END-PERFORM
025300         IF (WS-PLAN-NOTFOUND)
025400************ Plan {0} does not exist in selected benefit types
025500             MOVE 127                    TO CRT-ERROR-NBR
025600             MOVE PRM-PLAN-CODE (I1)     TO CRT-ERR-VAR1
025700             MOVE WS-TRUE                TO WS-PARAMETER-ERROR
025800             PERFORM 780-PRINT-ERROR-MSG
025900         END-IF
026000     END-PERFORM.
026100
026200     MOVE I9                             TO WS-LAST-PLAN-CODE.
026300
026400 054-END.
026500
014900******************************************************************
014800 050-END.
014900******************************************************************

014900******************************************************************
015000 100-PROGRAM-CONTROL             SECTION 10.
015100******************************************************************
015200 100-START.
015300
015400     MOVE 105                    TO CRT-MSG-NBR.
015500     PERFORM 780-DISPLAY-MSG.
015600
015700     IF (PRM-SORT-OPTION         = 1)
015800         IF (PRM-EMPLOYEE-SEQ    = "N")
015900              SORT SORT-FILE
016000                 ASCENDING KEY           DSF-LOCAT-CODE
016100                                         DSF-EMPLOYEE
016200                                         DSF-PLAN-TYPE-ORDER
016300                                         DSF-PLAN-TYPE
016400                                         DSF-PLAN-CODE
016500                                         DSF-START-DATE
016600                     INPUT  PROCEDURE    1000-SEL-REPORT-SECTION
016700                     OUTPUT PROCEDURE    2000-PRT-REPORT-SECTION
016800         ELSE
016900              SORT SORT-FILE
017000                 ASCENDING KEY           DSF-LOCAT-CODE
017100                                         DSF-LAST-NAME
017200                                         DSF-FIRST-NAME
017300                                         DSF-MIDDLE-INIT
017400                                         DSF-EMPLOYEE
017500                                         DSF-PLAN-TYPE-ORDER
017600                                         DSF-PLAN-TYPE
017700                                         DSF-PLAN-CODE
017800                                         DSF-START-DATE
017900                     INPUT  PROCEDURE    1000-SEL-REPORT-SECTION
018000                     OUTPUT PROCEDURE    2000-PRT-REPORT-SECTION.
018100
018200     IF (PRM-SORT-OPTION         = 2)
018300         IF (PRM-EMPLOYEE-SEQ    = "N")
018400             SORT SORT-FILE
018500                 ASCENDING KEY           DSF-LOCAT-CODE
018600                                         DSF-PROCESS-LEVEL
018700                                         DSF-DEPARTMENT
018800                                         DSF-USER-LEVEL
018900                                         DSF-EMPLOYEE
019000                                         DSF-PLAN-TYPE-ORDER
019100                                         DSF-PLAN-TYPE
019200                                         DSF-PLAN-CODE
019300                                         DSF-START-DATE
019400                     INPUT  PROCEDURE    1000-SEL-REPORT-SECTION
019500                     OUTPUT PROCEDURE    2000-PRT-REPORT-SECTION
019600         ELSE
019700             SORT SORT-FILE
019800                 ASCENDING KEY           DSF-LOCAT-CODE
019900                                         DSF-PROCESS-LEVEL
020000                                         DSF-DEPARTMENT
020100                                         DSF-USER-LEVEL
020200                                         DSF-LAST-NAME
020300                                         DSF-FIRST-NAME
020400                                         DSF-MIDDLE-INIT
020500                                         DSF-EMPLOYEE
020600                                         DSF-PLAN-TYPE-ORDER
020700                                         DSF-PLAN-TYPE
020800                                         DSF-PLAN-CODE
020900                                         DSF-START-DATE
021000                     INPUT  PROCEDURE    1000-SEL-REPORT-SECTION
021100                     OUTPUT PROCEDURE    2000-PRT-REPORT-SECTION.
021200
021300     IF (PRM-SORT-OPTION         = 3)
021400         IF (PRM-EMPLOYEE-SEQ    = "N")
021500             SORT SORT-FILE
021600                 ASCENDING KEY           DSF-LOCAT-CODE
021700                                         DSF-PROCESS-LEVEL
021800                                         DSF-DEPARTMENT
021900                                         DSF-SUPERVISOR
022000                                         DSF-EMPLOYEE
022100                                         DSF-PLAN-TYPE-ORDER
022200                                         DSF-PLAN-TYPE
022300                                         DSF-PLAN-CODE
022400                                         DSF-START-DATE
022500                     INPUT  PROCEDURE    1000-SEL-REPORT-SECTION
022600                     OUTPUT PROCEDURE    2000-PRT-REPORT-SECTION
022700         ELSE
022800             SORT SORT-FILE
022900                 ASCENDING KEY           DSF-LOCAT-CODE
023000                                         DSF-PROCESS-LEVEL
023100                                         DSF-DEPARTMENT
023200                                         DSF-SUPERVISOR
023300                                         DSF-LAST-NAME
023400                                         DSF-FIRST-NAME
023500                                         DSF-MIDDLE-INIT
023600                                         DSF-EMPLOYEE
023700                                         DSF-PLAN-TYPE-ORDER
023800                                         DSF-PLAN-TYPE
023900                                         DSF-PLAN-CODE
024000                                         DSF-START-DATE
024100                     INPUT  PROCEDURE    1000-SEL-REPORT-SECTION
024200                     OUTPUT PROCEDURE    2000-PRT-REPORT-SECTION.
024300
024400     IF (PRM-SORT-OPTION         = 4)
024500         IF (PRM-EMPLOYEE-SEQ    = "N")
024600             SORT SORT-FILE
024700                 ASCENDING KEY           DSF-EMPLOYEE
024800                                         DSF-PLAN-TYPE-ORDER
024900                                         DSF-PLAN-TYPE
025000                                         DSF-PLAN-CODE
025100                                         DSF-START-DATE
025200                     INPUT  PROCEDURE    1000-SEL-REPORT-SECTION
025300                     OUTPUT PROCEDURE    2000-PRT-REPORT-SECTION
025400         ELSE
025500             SORT SORT-FILE
025600                 ASCENDING KEY           DSF-LAST-NAME
025700                                         DSF-FIRST-NAME
025800                                         DSF-MIDDLE-INIT
025900                                         DSF-EMPLOYEE
026000                                         DSF-PLAN-TYPE-ORDER
026100                                         DSF-PLAN-TYPE
026200                                         DSF-PLAN-CODE
026300                                         DSF-START-DATE
026400                     INPUT  PROCEDURE    1000-SEL-REPORT-SECTION
026500                     OUTPUT PROCEDURE    2000-PRT-REPORT-SECTION.
026600
026700 100-END.
026800******************************************************************
026900 1000-SEL-REPORT-SECTION         SECTION 50.
027000******************************************************************
027100 1000-START.
027200
027300     MOVE PRM-COMPANY            TO DB-COMPANY.
027400     MOVE PRM-BEG-EMPLOYEE       TO DB-EMPLOYEE.
027500     MOVE SPACES                 TO DB-PLAN-TYPE
027600                                    DB-PLAN-CODE.
027700     MOVE ZEROS                  TO DB-START-DATE.
027800
027900     PERFORM 850-FIND-NLT-BENSET3.
028000
028100     IF  (PRM-BEG-EMPLOYEE NOT = ZEROS)
028200     AND (PRM-END-EMPLOYEE = ZEROS)
028300         PERFORM 1100-SEL-EMPLOYEES
028400         THRU    1100-END
028500     ELSE
028600         PERFORM 1100-SEL-EMPLOYEES
028700         THRU    1100-END
028800             UNTIL (BENEFIT-NOTFOUND)
028900             OR    (BEN-COMPANY       NOT = DB-COMPANY)
029000             OR    ((BEN-EMPLOYEE         > PRM-END-EMPLOYEE)
029100             AND    (PRM-END-EMPLOYEE NOT = ZEROS)).
029200
029300     GO TO 1000-END.
029400
029500******************************************************************
029600 1100-SEL-EMPLOYEES.
029700******************************************************************
029800
029900     MOVE BEN-EMPLOYEE           TO DB-EMPLOYEE
030000                                    SF-EMPLOYEE.
030100     PERFORM 840-FIND-EMPSET1.
030200     PERFORM 840-FIND-PEMSET1.
030300     IF (EMPLOYEE-NOTFOUND)
030400     OR (PAEMPLOYEE-NOTFOUND)
030500         GO TO 1100-FIND-NEXT-BENEFIT.
030600
030700     MOVE EMP-COMPANY            TO CRT-COMPANY.
030800     MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL.
030900     PERFORM 700-HR-EMP-SECURITY.
031000     IF (HRWS-EMP-SECURED)
031100         GO TO 1100-FIND-NEXT-BENEFIT.
031200
031300     IF  (EMP-TERM-DATE NOT = ZEROS)
031400     AND (EMP-TERM-DATE NOT > PRM-AS-OF-DATE)
031500         GO TO 1100-FIND-NEXT-BENEFIT.
031600
031700     PERFORM 620-BN-QUALIFY-RUN-OPTIONS.
031800
031900     IF (BNWS-NOT-QUALIFIED)
032000         GO TO 1100-FIND-NEXT-BENEFIT.
032100
032200     MOVE EMP-LAST-NAME          TO SF-LAST-NAME.
032300     MOVE EMP-FIRST-NAME         TO SF-FIRST-NAME.
032400     MOVE EMP-MIDDLE-INIT        TO SF-MIDDLE-INIT.
037300     MOVE EMP-LAST-NAME-PRE      TO SF-LAST-NAME-PRE.
037300     MOVE EMP-NAME-SUFFIX        TO SF-NAME-SUFFIX.
032500     MOVE EMP-PROCESS-LEVEL      TO SF-PROCESS-LEVEL.
032600     MOVE EMP-DEPARTMENT         TO SF-DEPARTMENT.
032700     MOVE EMP-USER-LEVEL         TO SF-USER-LEVEL.
032800     MOVE EMP-SUPERVISOR         TO SF-SUPERVISOR.
032900     MOVE PEM-LOCAT-CODE         TO SF-LOCAT-CODE. 
033000
033100     IF (EMP-PAY-STEP NOT = ZEROES)
033200         MOVE EMP-SCHEDULE           TO DB-SCHEDULE
033300         MOVE EMP-PAY-STEP           TO DB-PAY-STEP
033400         MOVE EMP-PAY-GRADE          TO DB-PAY-GRADE
033500         MOVE WS-SYSTEM-DATE-YMD     TO DB-EFFECT-DATE
033600         PERFORM 850-FIND-NLT-SGDSET3
033700         IF (PRSAGDTL-NOTFOUND)
033800         OR (SGD-COMPANY   NOT = DB-COMPANY)
033900         OR (SGD-SCHEDULE  NOT = DB-SCHEDULE)
034000         OR (SGD-PAY-STEP  NOT = DB-PAY-STEP)
034100         OR (SGD-PAY-GRADE NOT = DB-PAY-GRADE)
034200             MOVE ZEROES             TO SF-EMP-PAY-RATE
034300             GO TO 1100-CONTINUE.
034400
034500     IF (EMP-SALARY-CLASS = "S")
034600         IF (EMP-PAY-STEP = ZEROES)
034700             MOVE EMP-PAY-RATE       TO SF-EMP-PAY-RATE
034800         ELSE
034900             MOVE SGD-PAY-RATE       TO SF-EMP-PAY-RATE
               END-IF
               GO TO 1100-CONTINUE
           END-IF.
035000     
           MOVE EMP-ANNUAL-HOURS           TO WS-ANNUAL-HOURS.
           IF (WS-ANNUAL-HOURS             = ZEROES)
035100         MOVE EMP-JOB-CODE           TO DB-JOB-CODE
               IF (DB-JOB-CODE         NOT = SPACES)
035200             PERFORM 840-FIND-JBCSET1
035300             IF (JOBCODE-FOUND)
035400                 MOVE JBC-ANNUAL-HOURS   TO WS-ANNUAL-HOURS.
035500 
           IF  (WS-ANNUAL-HOURS            = ZEROES)
035600         IF (WS-PRS-ANNUAL-HOURS NOT = ZEROS)
035700             MOVE WS-PRS-ANNUAL-HOURS TO WS-ANNUAL-HOURS
035800         ELSE
035900             MOVE 2080               TO WS-ANNUAL-HOURS.
036000
036100     IF (EMP-SALARY-CLASS = "H")
036200         IF (EMP-PAY-STEP = ZEROES)
036300             COMPUTE SF-EMP-PAY-RATE ROUNDED = EMP-NBR-FTE
036400                                             * EMP-PAY-RATE
036500                                             * WS-ANNUAL-HOURS
036600         ELSE
036700             COMPUTE SF-EMP-PAY-RATE ROUNDED = EMP-NBR-FTE
036800                                             * SGD-PAY-RATE
036900                                             * WS-ANNUAL-HOURS.
037000
037100 1100-CONTINUE.
037200
037300     PERFORM 1110-SEL-BENEFITS
037400     THRU    1110-END
037500         UNTIL (BENEFIT-NOTFOUND)
037600         OR    (BEN-COMPANY      NOT = DB-COMPANY)
037700         OR    (BEN-EMPLOYEE     NOT = DB-EMPLOYEE).
037800
037900     GO TO 1100-END.
038000
038100 1100-FIND-NEXT-BENEFIT.
038200
038300     PERFORM 860-FIND-NEXT-BENSET3
038400         UNTIL (BENEFIT-NOTFOUND)
038500         OR    (BEN-COMPANY  NOT = DB-COMPANY)
038600         OR    (BEN-EMPLOYEE NOT = DB-EMPLOYEE).
038700
038800 1100-END.
038900******************************************************************
039000 1110-SEL-BENEFITS.
039100******************************************************************
039200
           IF (PRM-PLAN-CODE (1)              NOT = SPACES)
               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > WS-LAST-PLAN-CODE)
                   OR     ((WS-TBL-PLAN-TYPE (I1) = BEN-PLAN-TYPE)
                   AND     (WS-TBL-PLAN-CODE (I1) = BEN-PLAN-CODE))

                   CONTINUE
               END-PERFORM
               IF (WS-TBL-PLAN-TYPE (I1)      = SPACES)
040500             GO TO 1110-NEXT-BENEFIT
               END-IF
           ELSE
               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > WS-LAST-PLAN-TYPE)
                   OR     (PRM-PLAN-TYPE (I1) = BEN-PLAN-TYPE)

                   CONTINUE
               END-PERFORM
               IF (PRM-PLAN-TYPE (I1)         = SPACES)
040500             GO TO 1110-NEXT-BENEFIT

040700     IF (BEN-START-DATE          > PRM-AS-OF-DATE)
040800         GO TO 1110-NEXT-BENEFIT.
040900
041000     IF  (BEN-STOP-DATE          NOT = ZEROS)
041100     AND (BEN-STOP-DATE          < PRM-AS-OF-DATE)
041200         GO TO 1110-NEXT-BENEFIT.
041300
041400     IF (BEN-PLAN-TYPE = "HL")
041500         MOVE 1                  TO SF-PLAN-TYPE-ORDER
041600     ELSE
041700     IF (BEN-PLAN-TYPE = "DN")
041800         MOVE 2                  TO SF-PLAN-TYPE-ORDER
041900     ELSE
042000     IF (BEN-PLAN-TYPE = "DI")
042100         MOVE 3                  TO SF-PLAN-TYPE-ORDER
042200     ELSE
042300     IF (BEN-PLAN-TYPE = "EL")
042400         MOVE 4                  TO SF-PLAN-TYPE-ORDER
042500     ELSE
042600     IF (BEN-PLAN-TYPE = "DL")
042700         MOVE 5                  TO SF-PLAN-TYPE-ORDER
042800     ELSE
042900     IF (BEN-PLAN-TYPE = "DC")
043000         MOVE 6                  TO SF-PLAN-TYPE-ORDER
043100     ELSE
043200     IF (BEN-PLAN-TYPE = "DB")
043300         MOVE 7                  TO SF-PLAN-TYPE-ORDER
043400     ELSE
043500     IF (BEN-PLAN-TYPE = "VA")
043600         MOVE 8                  TO SF-PLAN-TYPE-ORDER
043700     ELSE
043800     IF (BEN-PLAN-TYPE = "RS")
043900         MOVE 9                  TO SF-PLAN-TYPE-ORDER
044000     ELSE
044100     IF (BEN-PLAN-TYPE = "SB")
044200         MOVE 10                 TO SF-PLAN-TYPE-ORDER
044300     ELSE
044400     IF (BEN-PLAN-TYPE = "SP")
044500         MOVE 11                 TO SF-PLAN-TYPE-ORDER.
044600
044700     MOVE BEN-PLAN-CODE          TO SF-PLAN-CODE.
044800     MOVE BEN-PLAN-TYPE          TO SF-PLAN-TYPE.
044900     MOVE BEN-START-DATE         TO SF-START-DATE.
045000     MOVE BEN-STOP-DATE          TO SF-STOP-DATE.
045100     MOVE BEN-COV-OPTION         TO SF-COV-OPTION.
045200     MOVE BEN-COVER-AMT          TO SF-COVER-AMT.
045300     MOVE BEN-PAY-RATE           TO SF-BEN-PAY-RATE.
045400     MOVE BEN-COV-UPD-DT         TO SF-COV-UPD-DT.
045500     MOVE BEN-NBR-HOURS          TO SF-NBR-HOURS.
045600     MOVE BEN-YTD-CONT           TO SF-YTD-CONT.
045700     MOVE BEN-PCT-AMT-FLAG       TO SF-PCT-AMT-FLAG.
045800     MOVE BEN-MULTIPLE           TO SF-MULTIPLE.
045900     MOVE BEN-COV-GROUP          TO SF-GROUP-NAME.
046000
046100     RELEASE SORT-REC       FROM SF-SORT-REC.
046200
046300 1110-NEXT-BENEFIT.
046400
046500     PERFORM 860-FIND-NEXT-BENSET3.
046600
046700 1110-END.
046800
046900******************************************************************
047000 1000-END.
047100******************************************************************
047200******************************************************************
047300 2000-PRT-REPORT-SECTION          SECTION 50.
047400******************************************************************
047500 2000-START.
047600
047700     IF (PRM-SORT-OPTION = 1)
047800         PERFORM 2100-PRINT-OPTION-1
047900         THRU    2100-END
048000     ELSE
048100     IF (PRM-SORT-OPTION = 2)
048200         PERFORM 2200-PRINT-OPTION-2
048300         THRU    2200-END
048400     ELSE
048500     IF (PRM-SORT-OPTION = 3)
048600         PERFORM 2300-PRINT-OPTION-3
048700         THRU    2300-END
048800     ELSE
048900     IF (PRM-SORT-OPTION = 4)
049000         PERFORM 2400-PRINT-OPTION-4
049100         THRU    2400-END.
049200
049300     GO TO 2000-END.
049400******************************************************************
049500 2100-PRINT-OPTION-1.
049600******************************************************************
049700
049800     MOVE PRM-AS-OF-DATE         TO WS-PRM-DATE.
049900     MOVE WS-FALSE               TO WS-END-OF-FILE-SW.
050000
050100     RETURN SORT-FILE       INTO SF-SORT-REC
050200         AT END
050300             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
050400
050500     PERFORM 2110-DO-PEM-LOCAT-CODE
050600     THRU    2110-END
050700         UNTIL (END-OF-FILE).
050800
050900 2100-END.
051000******************************************************************
051100 2110-DO-PEM-LOCAT-CODE.
051200******************************************************************
051300
051400     MOVE SF-LOCAT-CODE          TO WS-PEM-LOCAT-CODE
051500                                    G17-PEM-LOCAT-CODE.
051600
051700     PERFORM 2130-PRT-COMPANY-GRP
051800     THRU    2130-END.
051900
052000     PERFORM 2120-DO-BEN-EMPLOYEE
052100     THRU    2120-END
052200         UNTIL (END-OF-FILE)
052300         OR    (SF-LOCAT-CODE NOT = WS-PEM-LOCAT-CODE).
052400
052500 2110-END.
052600******************************************************************
052700 2120-DO-BEN-EMPLOYEE.
052800******************************************************************
052900
053000     INITIALIZE WS-SUMM-TABLE.
053100     MOVE 0                      TO RPT-PAGE-COUNT (1).
053200     MOVE SF-EMPLOYEE            TO WS-BEN-EMPLOYEE.
053300     MOVE ZEROS                  TO WS-CPNY-PD-BENEFITS
053400                                    I3.
053500
053600     MOVE SF-LAST-NAME           TO HRWS-LAST-NAME.
053700     MOVE SF-FIRST-NAME          TO HRWS-FIRST-NAME.
053800     MOVE SF-MIDDLE-INIT         TO HRWS-MIDDLE-INIT.
037300     MOVE SF-LAST-NAME-PRE       TO HRWS-LAST-NAME-PRE.
037300     MOVE SF-NAME-SUFFIX         TO HRWS-NAME-SUFFIX.
053900     PERFORM 750-HR-FORMAT-NAME.
054000     MOVE HRWS-FORMAT-NAME       TO G1N-EMP-NAME
054100                                    G1-EMP-NAME.
054200     MOVE SF-EMPLOYEE            TO G1-EMPLOYEE
054300                                    G1N-EMPLOYEE.
054400
054500     IF (PRM-PAGE-BREAK = "N")            
054600         MOVE GN1N-NO-PAGE-LINE      TO RPT-GROUP-REQUEST
054700     ELSE 
054800         MOVE GN1-PAGE-NBR-LINE      TO RPT-GROUP-REQUEST.
054900
055000     PERFORM 700-PRINT-RPT-GRP.
055100
055200     PERFORM 4000-PRT-RPT-COVER-PAGE
055300     THRU    4000-END.
055400
055500     MOVE SF-EMPLOYEE            TO DB-EMPLOYEE.
055600     MOVE PRM-AS-OF-DATE         TO DB-START-DATE.
055700     PERFORM 850-FIND-NLT-EFDSET2.
055800
055900     IF  (EMPFLEXDOL-FOUND)
056000     AND (EFD-COMPANY   = DB-COMPANY)
056100     AND (EFD-EMPLOYEE  = DB-EMPLOYEE)
056200     AND (PRM-AS-OF-DATE NOT > EFD-STOP-DATE)
056300         PERFORM 4100-PRT-FLEX-PLAN
056400         THRU    4100-END.
056500
056600     PERFORM 4201-DO-BEN-PLAN-TYPE
056700     THRU    4201-END
056800         UNTIL (END-OF-FILE)
056900         OR    (SF-LOCAT-CODE NOT = WS-PEM-LOCAT-CODE)
057000         OR    (SF-EMPLOYEE   NOT = WS-BEN-EMPLOYEE).
057100
057200     IF  (EMPFLEXDOL-FOUND)
057300     AND (EFD-COMPANY   = DB-COMPANY)
057400     AND (EFD-EMPLOYEE  = WS-BEN-EMPLOYEE)
057500     AND (PRM-AS-OF-DATE NOT > EFD-STOP-DATE)
057600         GO TO 2120-END
057700     ELSE
057800         MOVE ZEROS              TO WS-TOT-EDM-AMOUNT-YTD
057900         IF (PRM-PAGE-BREAK = "Y")            
058000             MOVE GN1-PAGE-NBR-LINE      TO RPT-GROUP-REQUEST
058100             PERFORM 700-PRINT-RPT-GRP
058200         END-IF
058300         MOVE GN15H-SUMMARY-STMT TO RPT-GROUP-REQUEST
058400         PERFORM 700-PRINT-RPT-GRP
058500         PERFORM 4500-PRT-SUMMARY-STMT
058600         THRU    4500-END
058700             VARYING I2 FROM 1 BY 1
058800             UNTIL   (I2 > WS-CPNY-PD-BENEFITS)
058900         MOVE WS-TOT-EDM-AMOUNT-YTD      TO G15-TOT-CPNY-CONTRIB
059000         MOVE WS-PRM-YYYY                TO G15-REPORT-YEAR-5
059100         MOVE WS-SAVE-PAY-RATE           TO G15-ANNUAL-SALARY
059200         COMPUTE WS-TOT-COMPENSATION     = WS-TOT-EDM-AMOUNT-YTD
059300                                         + WS-SAVE-PAY-RATE
059400         MOVE WS-TOT-COMPENSATION        TO G15-TOTAL-COMPENSATION
059500         MOVE GN15D-2-SUMMARY-STMT       TO RPT-GROUP-REQUEST
059600         PERFORM 700-PRINT-RPT-GRP.
059700
059800 2120-END.
059900******************************************************************
060000 2130-PRT-COMPANY-GRP.
060100******************************************************************
060200
060300     IF (SF-LOCAT-CODE NOT = SPACES)
060400*        MOVE "LO"               TO DB-TYPE
060500         MOVE SF-LOCAT-CODE      TO DB-CODE      
060600         PERFORM 840-FIND-PCOSET1
060700         IF (PCODES-FOUND)
060800             MOVE PCO-DESCRIPTION    TO G17-PEM-LOCAT-CODE-DESC
060900         ELSE
061000             MOVE 106                TO CRT-MSG-NBR
061100             PERFORM 790-GET-MSG
061200             MOVE CRT-MESSAGE        TO G17-PEM-LOCAT-CODE-DESC
061300     ELSE
061400         MOVE SPACES             TO  G17-PEM-LOCAT-CODE-DESC.
061500
061600     MOVE GN17-PEM-LOCAT-CODE    TO RPT-GROUP-REQUEST.
061700     PERFORM 700-PRINT-RPT-GRP.
061800
061900 2130-END.
062000******************************************************************
062100 2200-PRINT-OPTION-2.
062200******************************************************************
062300
062400     MOVE PRM-AS-OF-DATE         TO WS-PRM-DATE.
062500     MOVE WS-FALSE               TO WS-END-OF-FILE-SW.
062600
062700     RETURN SORT-FILE       INTO SF-SORT-REC
062800         AT END
062900             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
063000
063100     PERFORM 2210-DO-PEM-LOCAT-CODE-GRP
063200     THRU    2210-END
063300         UNTIL (END-OF-FILE).
063400
063500 2200-END.
063600******************************************************************
063700 2210-DO-PEM-LOCAT-CODE-GRP.
063800******************************************************************
063900
064000     MOVE SF-LOCAT-CODE          TO WS-PEM-LOCAT-CODE
064100                                    G18-PEM-LOCAT-CODE.
064200     MOVE SF-PROCESS-LEVEL       TO WS-EMP-PROCESS-LEVEL
064300                                    G18-EMP-PROCESS-LEVEL.
064400     MOVE SF-DEPARTMENT          TO WS-EMP-DEPARTMENT
064500                                    G18-EMP-DEPARTMENT.
064600     MOVE SF-USER-LEVEL          TO WS-EMP-USER-LEVEL
064700                                    G18-EMP-USER-LEVEL.
064800
064900     PERFORM 2230-PRT-COMPANY-GRP
065000     THRU    2230-END.
065100
065200     PERFORM 2220-DO-BEN-EMPLOYEE
065300     THRU    2220-END
065400         UNTIL (END-OF-FILE)
065500         OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
065600         OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
065700         OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
065800         OR    (SF-USER-LEVEL    NOT = WS-EMP-USER-LEVEL).
065900
066000 2210-END.
066100******************************************************************
066200 2220-DO-BEN-EMPLOYEE.
066300******************************************************************
066400
066500     INITIALIZE WS-SUMM-TABLE.
066600     MOVE 0                      TO RPT-PAGE-COUNT (1).
066700     MOVE SF-EMPLOYEE            TO WS-BEN-EMPLOYEE.
066800     MOVE ZEROS                  TO WS-CPNY-PD-BENEFITS
066900                                    I3.
067000
067100     INITIALIZE WS-SUMM-TABLE.
067200     MOVE 0                      TO RPT-PAGE-COUNT (1).
067300     MOVE SF-EMPLOYEE            TO WS-BEN-EMPLOYEE.
067400     MOVE ZEROS                  TO WS-CPNY-PD-BENEFITS
067500                                    I3.
067600
067700     MOVE SF-LAST-NAME           TO HRWS-LAST-NAME.
067800     MOVE SF-FIRST-NAME          TO HRWS-FIRST-NAME.
067900     MOVE SF-MIDDLE-INIT         TO HRWS-MIDDLE-INIT.
037300     MOVE SF-LAST-NAME-PRE       TO HRWS-LAST-NAME-PRE.
037300     MOVE SF-NAME-SUFFIX         TO HRWS-NAME-SUFFIX.
068000     PERFORM 750-HR-FORMAT-NAME.
068100     MOVE HRWS-FORMAT-NAME       TO G1N-EMP-NAME
068200                                    G1-EMP-NAME.
068300     MOVE SF-EMPLOYEE            TO G1-EMPLOYEE
068400                                    G1N-EMPLOYEE.
068500
068600     IF (PRM-PAGE-BREAK = "N")            
068700         MOVE GN1N-NO-PAGE-LINE      TO RPT-GROUP-REQUEST
068800     ELSE 
068900         MOVE GN1-PAGE-NBR-LINE      TO RPT-GROUP-REQUEST.
069000
069100     PERFORM 700-PRINT-RPT-GRP.
069200
069300     PERFORM 4000-PRT-RPT-COVER-PAGE
069400     THRU    4000-END.
069500
069600     MOVE SF-EMPLOYEE            TO DB-EMPLOYEE.
069700     MOVE PRM-AS-OF-DATE         TO DB-START-DATE.
069800     PERFORM 850-FIND-NLT-EFDSET2.
069900
070000     IF  (EMPFLEXDOL-FOUND)
070100     AND (EFD-COMPANY   = DB-COMPANY)
070200     AND (EFD-EMPLOYEE  = DB-EMPLOYEE)
070300     AND (PRM-AS-OF-DATE NOT > EFD-STOP-DATE)
070400         PERFORM 4100-PRT-FLEX-PLAN
070500         THRU    4100-END.
070600
070700     PERFORM 4202-DO-BEN-PLAN-TYPE
070800     THRU    4202-END
070900         UNTIL (END-OF-FILE)
071000         OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
071100         OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
071200         OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
071300         OR    (SF-USER-LEVEL    NOT = WS-EMP-USER-LEVEL)
071400         OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE).
071500
071600     IF  (EMPFLEXDOL-FOUND)
071700     AND (EFD-COMPANY   = DB-COMPANY)
071800     AND (EFD-EMPLOYEE  = WS-BEN-EMPLOYEE)
071900     AND (PRM-AS-OF-DATE NOT > EFD-STOP-DATE)
072000         GO TO 2220-END
072100     ELSE
072200         MOVE ZEROS              TO WS-TOT-EDM-AMOUNT-YTD
072300         IF (PRM-PAGE-BREAK = "Y")            
072400             MOVE GN1-PAGE-NBR-LINE      TO RPT-GROUP-REQUEST
072500             PERFORM 700-PRINT-RPT-GRP
072600         END-IF
072700         MOVE GN15H-SUMMARY-STMT TO RPT-GROUP-REQUEST
072800         PERFORM 700-PRINT-RPT-GRP
072900         PERFORM 4500-PRT-SUMMARY-STMT
073000         THRU    4500-END
073100             VARYING I2 FROM 1 BY 1
073200             UNTIL   (I2 > WS-CPNY-PD-BENEFITS)
073300         MOVE WS-TOT-EDM-AMOUNT-YTD      TO G15-TOT-CPNY-CONTRIB
073400         MOVE WS-PRM-YYYY                TO G15-REPORT-YEAR-5
073500         MOVE WS-SAVE-PAY-RATE           TO G15-ANNUAL-SALARY
073600         COMPUTE WS-TOT-COMPENSATION     = WS-TOT-EDM-AMOUNT-YTD
073700                                         + WS-SAVE-PAY-RATE
073800         MOVE WS-TOT-COMPENSATION        TO G15-TOTAL-COMPENSATION
073900         MOVE GN15D-2-SUMMARY-STMT       TO RPT-GROUP-REQUEST
074000         PERFORM 700-PRINT-RPT-GRP.
074100
074200 2220-END.
074300******************************************************************
074400 2230-PRT-COMPANY-GRP.
074500******************************************************************
074600
074700     IF (SF-LOCAT-CODE NOT = SPACES)
074800*        MOVE "LO"               TO DB-TYPE
074900         MOVE SF-LOCAT-CODE      TO DB-CODE       
075000         PERFORM 840-FIND-PCOSET1
075100         IF (PCODES-FOUND)
075200             MOVE PCO-DESCRIPTION    TO G18-PEM-LOCAT-CODE-DESC
075300         ELSE
075400             MOVE 106                TO CRT-MSG-NBR
075500             PERFORM 790-GET-MSG
075600             MOVE CRT-MESSAGE        TO G18-PEM-LOCAT-CODE-DESC
075700     ELSE
075800         MOVE SPACES             TO  G18-PEM-LOCAT-CODE-DESC.
075900
076000     MOVE SF-PROCESS-LEVEL       TO DB-PROCESS-LEVEL.
076100
076200     IF (SF-PROCESS-LEVEL NOT = SPACES)
076300         PERFORM 840-FIND-PRSSET1
076400         IF (PRSYSTEM-FOUND)
076500             MOVE PRS-NAME           TO G18-EMP-PROC-LEV-DESC
076600         ELSE
076700             MOVE 106                TO CRT-MSG-NBR
076800             PERFORM 790-GET-MSG
076900             MOVE CRT-MESSAGE        TO G18-EMP-PROC-LEV-DESC
077000     ELSE
077100         MOVE SPACES             TO G18-EMP-PROC-LEV-DESC.
077200
077300     MOVE SF-DEPARTMENT          TO DB-DEPARTMENT.
077400
077500     IF (SF-DEPARTMENT NOT = SPACES)
077600         PERFORM 840-FIND-DPTSET1
077700         IF (DEPTCODE-FOUND)
077800             MOVE DPT-NAME           TO G18-EMP-DEPARTMENT-DESC
077900         ELSE
078000             MOVE 106                TO CRT-MSG-NBR
078100             PERFORM 790-GET-MSG
078200             MOVE CRT-MESSAGE        TO G18-EMP-DEPARTMENT-DESC
078300     ELSE
078400         MOVE SPACES             TO G18-EMP-DEPARTMENT-DESC.
078500
078600     IF (SF-USER-LEVEL NOT = SPACES)
078700         MOVE "UL"               TO DB-TYPE
078800         MOVE SF-USER-LEVEL      TO DB-CODE
078900         PERFORM 840-FIND-PCOSET1
079000         IF (PCODES-FOUND)
079100             MOVE PCO-DESCRIPTION    TO G18-EMP-USER-LEVEL-DESC
079200         ELSE
079300             MOVE 106                TO CRT-MSG-NBR
079400             PERFORM 790-GET-MSG
079500             MOVE CRT-MESSAGE        TO G18-EMP-USER-LEVEL-DESC
079600     ELSE
079700         MOVE SPACES             TO G18-EMP-USER-LEVEL-DESC.
079800
079900     MOVE GN18-EMP-USER-LEVEL    TO RPT-GROUP-REQUEST.
080000     PERFORM 700-PRINT-RPT-GRP.
080100
080200 2230-END.
080300******************************************************************
080400 2300-PRINT-OPTION-3.
080500******************************************************************
080600
080700     MOVE PRM-AS-OF-DATE         TO WS-PRM-DATE.
080800     MOVE WS-FALSE               TO WS-END-OF-FILE-SW.
080900
081000     RETURN SORT-FILE       INTO SF-SORT-REC
081100         AT END
081200             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
081300
081400     PERFORM 2310-DO-PEM-LOCAT-CODE
081500     THRU    2310-END
081600         UNTIL (END-OF-FILE).
081700
081800 2300-END.
081900******************************************************************
082000 2310-DO-PEM-LOCAT-CODE.
082100******************************************************************
082200
082300     MOVE SF-LOCAT-CODE          TO WS-PEM-LOCAT-CODE
082400                                    G19-PEM-LOCAT-CODE.
082500     MOVE SF-PROCESS-LEVEL       TO WS-EMP-PROCESS-LEVEL
082600                                    G19-EMP-PROCESS-LEVEL.
082700     MOVE SF-DEPARTMENT          TO WS-EMP-DEPARTMENT
082800                                    G19-EMP-DEPARTMENT.
082900     MOVE SF-SUPERVISOR          TO WS-EMP-SUPERVISOR
083000                                    G19-EMP-SUPERVISOR.
083100
083200     PERFORM 2330-PRT-COMPANY-GRP
083300     THRU    2330-END.
083400
083500     PERFORM 2320-DO-BEN-EMPLOYEE
083600     THRU    2320-END
083700         UNTIL (END-OF-FILE)
083800         OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
083900         OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
084000         OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
084100         OR    (SF-SUPERVISOR    NOT = WS-EMP-SUPERVISOR).
084200
084300 2310-END.
084400******************************************************************
084500 2320-DO-BEN-EMPLOYEE.
084600******************************************************************
084700
084800     MOVE 0                      TO RPT-PAGE-COUNT (1).
084900     MOVE SF-EMPLOYEE            TO WS-BEN-EMPLOYEE.
085000     MOVE ZEROS                  TO WS-CPNY-PD-BENEFITS
085100                                    I3.
085200
085300     INITIALIZE WS-SUMM-TABLE.
085400     MOVE 0                      TO RPT-PAGE-COUNT (1).
085500     MOVE SF-EMPLOYEE            TO WS-BEN-EMPLOYEE.
085600     MOVE ZEROS                  TO WS-CPNY-PD-BENEFITS
085700                                    I3.
085800
085900     MOVE SF-LAST-NAME           TO HRWS-LAST-NAME.
086000     MOVE SF-FIRST-NAME          TO HRWS-FIRST-NAME.
086100     MOVE SF-MIDDLE-INIT         TO HRWS-MIDDLE-INIT.
037300     MOVE SF-LAST-NAME-PRE       TO HRWS-LAST-NAME-PRE.
037300     MOVE SF-NAME-SUFFIX         TO HRWS-NAME-SUFFIX.
086200     PERFORM 750-HR-FORMAT-NAME.
086300     MOVE HRWS-FORMAT-NAME       TO G1N-EMP-NAME
086400                                    G1-EMP-NAME.
086500     MOVE SF-EMPLOYEE            TO G1-EMPLOYEE
086600                                    G1N-EMPLOYEE.
086700
086800     IF (PRM-PAGE-BREAK = "N")            
086900         MOVE GN1N-NO-PAGE-LINE      TO RPT-GROUP-REQUEST
087000     ELSE 
087100         MOVE GN1-PAGE-NBR-LINE      TO RPT-GROUP-REQUEST.
087200
087300     PERFORM 700-PRINT-RPT-GRP.
087400
087500     PERFORM 4000-PRT-RPT-COVER-PAGE
087600     THRU    4000-END.
087700
087800     MOVE SF-EMPLOYEE            TO DB-EMPLOYEE.
087900     MOVE PRM-AS-OF-DATE         TO DB-START-DATE.
088000     PERFORM 850-FIND-NLT-EFDSET2.
088100
088200     IF  (EMPFLEXDOL-FOUND)
088300     AND (EFD-COMPANY   = DB-COMPANY)
088400     AND (EFD-EMPLOYEE  = DB-EMPLOYEE)
088500     AND (PRM-AS-OF-DATE NOT > EFD-STOP-DATE)
088600         PERFORM 4100-PRT-FLEX-PLAN
088700         THRU    4100-END.
088800
088900     PERFORM 4203-DO-BEN-PLAN-TYPE
089000     THRU    4203-END
089100         UNTIL (END-OF-FILE)
089200         OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
089300         OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
089400         OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
089500         OR    (SF-SUPERVISOR    NOT = WS-EMP-SUPERVISOR)
089600         OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE).
089700
089800     IF  (EMPFLEXDOL-FOUND)
089900     AND (EFD-COMPANY   = DB-COMPANY)
090000     AND (EFD-EMPLOYEE  = WS-BEN-EMPLOYEE)
090100     AND (PRM-AS-OF-DATE NOT > EFD-STOP-DATE)
090200         GO TO 2320-END
090300     ELSE
090400         MOVE ZEROS              TO WS-TOT-EDM-AMOUNT-YTD
090500         IF (PRM-PAGE-BREAK = "Y")            
090600             MOVE GN1-PAGE-NBR-LINE      TO RPT-GROUP-REQUEST
090700             PERFORM 700-PRINT-RPT-GRP
090800         END-IF
090900         MOVE GN15H-SUMMARY-STMT TO RPT-GROUP-REQUEST
091000         PERFORM 700-PRINT-RPT-GRP
091100         PERFORM 4500-PRT-SUMMARY-STMT
091200         THRU    4500-END
091300             VARYING I2 FROM 1 BY 1
091400             UNTIL   (I2 > WS-CPNY-PD-BENEFITS)
091500         MOVE WS-TOT-EDM-AMOUNT-YTD      TO G15-TOT-CPNY-CONTRIB
091600         MOVE WS-PRM-YYYY                TO G15-REPORT-YEAR-5
091700         MOVE WS-SAVE-PAY-RATE           TO G15-ANNUAL-SALARY
091800         COMPUTE WS-TOT-COMPENSATION     = WS-TOT-EDM-AMOUNT-YTD
091900                                         + WS-SAVE-PAY-RATE
092000         MOVE WS-TOT-COMPENSATION        TO G15-TOTAL-COMPENSATION
092100         MOVE GN15D-2-SUMMARY-STMT       TO RPT-GROUP-REQUEST
092200         PERFORM 700-PRINT-RPT-GRP.
092300
092400 2320-END.
092500******************************************************************
092600 2330-PRT-COMPANY-GRP.
092700******************************************************************
092800
092900     IF (SF-LOCAT-CODE NOT = SPACES)
093000         MOVE "LO"               TO DB-TYPE
093100         MOVE SF-LOCAT-CODE      TO DB-CODE
093200         PERFORM 840-FIND-PCOSET1
093300         IF (PCODES-FOUND)
093400             MOVE PCO-DESCRIPTION    TO G19-PEM-LOCAT-CODE-DESC
093500         ELSE
093600             MOVE 106                TO CRT-MSG-NBR
093700             PERFORM 790-GET-MSG
093800             MOVE CRT-MESSAGE        TO G19-PEM-LOCAT-CODE-DESC
093900     ELSE
094000         MOVE SPACES             TO  G19-PEM-LOCAT-CODE-DESC.
094100
094200     MOVE SF-PROCESS-LEVEL       TO DB-PROCESS-LEVEL.
094300
094400     IF (SF-PROCESS-LEVEL NOT = SPACES)
094500         PERFORM 840-FIND-PRSSET1
094600         IF (PRSYSTEM-FOUND)
094700             MOVE PRS-NAME           TO G19-EMP-PROC-LEV-DESC
094800         ELSE
094900             MOVE 106                TO CRT-MSG-NBR
095000             PERFORM 790-GET-MSG
095100             MOVE CRT-MESSAGE        TO G19-EMP-PROC-LEV-DESC
095200     ELSE
095300         MOVE SPACES             TO G19-EMP-PROC-LEV-DESC.
095400
095500     MOVE SF-DEPARTMENT          TO DB-DEPARTMENT.
095600
095700     IF (SF-DEPARTMENT NOT = SPACES)
095800         PERFORM 840-FIND-DPTSET1
095900         IF (DEPTCODE-FOUND)
096000             MOVE DPT-NAME           TO G19-EMP-DEPARTMENT-DESC
096100         ELSE
096200             MOVE 106                TO CRT-MSG-NBR
096300             PERFORM 790-GET-MSG
096400             MOVE CRT-MESSAGE        TO G19-EMP-DEPARTMENT-DESC
096500     ELSE
096600         MOVE SPACES             TO G19-EMP-DEPARTMENT-DESC.
096700
096800
096900     IF (SF-SUPERVISOR NOT = SPACES)
097000         MOVE SF-SUPERVISOR          TO DB-CODE
097100         PERFORM 840-FIND-HSUSET1
097200         IF (HRSUPER-FOUND)
097300             MOVE HSU-DESCRIPTION    TO G19-EMP-SUPERVISOR-DESC
097400         ELSE
097500             MOVE 106                TO CRT-MSG-NBR
097600             PERFORM 790-GET-MSG
097700             MOVE CRT-MESSAGE        TO G19-EMP-SUPERVISOR-DESC
097800     ELSE
097900         MOVE SPACES             TO G19-EMP-SUPERVISOR-DESC.
098000
098100     MOVE GN19-EMP-SUPERVISOR    TO RPT-GROUP-REQUEST.
098200     PERFORM 700-PRINT-RPT-GRP.
098300
098400 2330-END.
098500******************************************************************
098600 2400-PRINT-OPTION-4.
098700******************************************************************
098800
098900     MOVE PRM-AS-OF-DATE         TO WS-PRM-DATE.
099000     MOVE WS-FALSE               TO WS-END-OF-FILE-SW.
099100
099200     RETURN SORT-FILE       INTO SF-SORT-REC
099300         AT END
099400             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
099500
099600     PERFORM 2410-DO-BEN-EMPLOYEE
099700     THRU    2410-END
099800         UNTIL (END-OF-FILE).
099900
100000 2400-END.
100100******************************************************************
100200 2410-DO-BEN-EMPLOYEE.
100300******************************************************************
100400
100500     INITIALIZE WS-SUMM-TABLE.
100600     MOVE 0                      TO RPT-PAGE-COUNT (1).
100700     MOVE SF-EMPLOYEE            TO WS-BEN-EMPLOYEE.
100800     MOVE ZEROS                  TO WS-CPNY-PD-BENEFITS
100900                                    I3.
101000
101100     INITIALIZE WS-SUMM-TABLE.
101200     MOVE 0                      TO RPT-PAGE-COUNT (1).
101300     MOVE SF-EMPLOYEE            TO WS-BEN-EMPLOYEE.
101400     MOVE ZEROS                  TO WS-CPNY-PD-BENEFITS
101500                                    I3.
101600
101700     MOVE SF-LAST-NAME           TO HRWS-LAST-NAME.
101800     MOVE SF-FIRST-NAME          TO HRWS-FIRST-NAME.
101900     MOVE SF-MIDDLE-INIT         TO HRWS-MIDDLE-INIT.
037300     MOVE SF-LAST-NAME-PRE       TO HRWS-LAST-NAME-PRE.
037300     MOVE SF-NAME-SUFFIX         TO HRWS-NAME-SUFFIX.
102000     PERFORM 750-HR-FORMAT-NAME.
102100     MOVE HRWS-FORMAT-NAME       TO G1N-EMP-NAME
102200                                    G1-EMP-NAME.
102300     MOVE SF-EMPLOYEE            TO G1-EMPLOYEE
102400                                    G1N-EMPLOYEE.
102500
102600     IF (PRM-PAGE-BREAK = "N")            
102700         MOVE GN1N-NO-PAGE-LINE      TO RPT-GROUP-REQUEST
102800     ELSE 
102900         MOVE GN1-PAGE-NBR-LINE      TO RPT-GROUP-REQUEST.
103000
103100     PERFORM 700-PRINT-RPT-GRP.
103200
103300     PERFORM 4000-PRT-RPT-COVER-PAGE
103400     THRU    4000-END.
103500
103600     MOVE SF-EMPLOYEE            TO DB-EMPLOYEE.
103700     MOVE PRM-AS-OF-DATE         TO DB-START-DATE.
103800     PERFORM 850-FIND-NLT-EFDSET2.
103900
104000     IF  (EMPFLEXDOL-FOUND)
104100     AND (EFD-COMPANY   = DB-COMPANY)
104200     AND (EFD-EMPLOYEE  = DB-EMPLOYEE)
104300     AND (PRM-AS-OF-DATE NOT > EFD-STOP-DATE)
104400         PERFORM 4100-PRT-FLEX-PLAN
104500         THRU    4100-END.
104600
104700     PERFORM 4204-DO-BEN-PLAN-TYPE
104800     THRU    4204-END
104900         UNTIL (END-OF-FILE)
105000         OR    (SF-EMPLOYEE NOT = WS-BEN-EMPLOYEE).
105100
105200     IF  (EMPFLEXDOL-FOUND)
105300     AND (EFD-COMPANY   = DB-COMPANY)
105400     AND (EFD-EMPLOYEE  = WS-BEN-EMPLOYEE)
105500     AND (PRM-AS-OF-DATE NOT > EFD-STOP-DATE)
105600         GO TO 2410-END
105700     ELSE
105800         MOVE ZEROS              TO WS-TOT-EDM-AMOUNT-YTD
105900         IF (PRM-PAGE-BREAK = "Y")            
106000             MOVE GN1-PAGE-NBR-LINE      TO RPT-GROUP-REQUEST
106100             PERFORM 700-PRINT-RPT-GRP
106200         END-IF
106300         MOVE GN15H-SUMMARY-STMT TO RPT-GROUP-REQUEST
106400         PERFORM 700-PRINT-RPT-GRP
106500         PERFORM 4500-PRT-SUMMARY-STMT
106600         THRU    4500-END
106700             VARYING I2 FROM 1 BY 1
106800             UNTIL   (I2 > WS-CPNY-PD-BENEFITS)
106900         MOVE WS-TOT-EDM-AMOUNT-YTD      TO G15-TOT-CPNY-CONTRIB
107000         MOVE WS-PRM-YYYY                TO G15-REPORT-YEAR-5
107100         MOVE WS-SAVE-PAY-RATE           TO G15-ANNUAL-SALARY
107200         COMPUTE WS-TOT-COMPENSATION     = WS-TOT-EDM-AMOUNT-YTD
107300                                         + WS-SAVE-PAY-RATE
107400         MOVE WS-TOT-COMPENSATION        TO G15-TOTAL-COMPENSATION
107500         MOVE GN15D-2-SUMMARY-STMT       TO RPT-GROUP-REQUEST
107600         PERFORM 700-PRINT-RPT-GRP.
107700
107800 2410-END.
107900******************************************************************
108000 4000-PRT-RPT-COVER-PAGE.
108100******************************************************************
108200
108300     MOVE PRM-AS-OF-DATE         TO G2-SYSTEM-DATE.
108400
108500     MOVE GN2-RPT-COVER-PAGE     TO RPT-GROUP-REQUEST.
108600     PERFORM 700-PRINT-RPT-GRP.
108700
108800 4000-END.
108900******************************************************************
109000 4100-PRT-FLEX-PLAN.
109100******************************************************************
109200
109300     IF (PRM-PAGE-BREAK = "Y")            
109400         MOVE GN1-PAGE-NBR-LINE      TO RPT-GROUP-REQUEST
109500         PERFORM 700-PRINT-RPT-GRP.
109600
109700     MOVE EFD-FLEX-PLAN          TO DB-FLEX-PLAN.
109800     PERFORM 840-FIND-FLPSET1.
109900
110000     IF (FLEXPLAN-FOUND)
110100         MOVE FLP-DESC               TO G3-FLP-DESC
110200     ELSE
110300         MOVE 106                TO CRT-MSG-NBR
110400         PERFORM 790-GET-MSG
110500         MOVE CRT-MESSAGE        TO G3-FLP-DESC.
110600
110700     MOVE GN3H-FLP-FLEX-PLAN     TO RPT-GROUP-REQUEST.
110800     PERFORM 700-PRINT-RPT-GRP.
110900
111000     MOVE EFD-START-DATE         TO DB-PLAN-START-DT.
111100     MOVE PRM-AS-OF-DATE         TO DB-START-DATE.
111200     PERFORM 850-FIND-NLT-EFRSET2.
111300
111400     IF (EMPFLEXREM-NOTFOUND)
111500     OR (EFR-COMPANY       NOT = DB-COMPANY)
111600     OR (EFR-EMPLOYEE      NOT = DB-EMPLOYEE)
111700     OR (EFR-PLAN-START-DT NOT = DB-PLAN-START-DT)
111800     OR (EFR-START-DATE        > DB-START-DATE)
111900     OR (EFR-STOP-DATE         <  DB-START-DATE)
112000         MOVE ZEROS              TO G3-EFD-EMP-FLEX-AMT
112100         MOVE ZEROS              TO G3-EFD-COMP-FLEX-AMT
112200     ELSE
112300         COMPUTE WS-EFD-COMP-FLEX-AMT    = EFR-CREDITS-AVAIL
112400                                         + EFR-CREDITS-SPENT
112500         MOVE WS-EFD-COMP-FLEX-AMT   TO G3-EFD-COMP-FLEX-AMT
112600         COMPUTE WS-EFD-EMP-FLEX-AMT     = EFR-PRE-TAX-AVAIL
112700                                         + EFR-PRE-TAX-SPENT
112800         MOVE WS-EFD-EMP-FLEX-AMT    TO G3-EFD-EMP-FLEX-AMT.
112900
113000     MOVE EFD-SALARY                 TO G3-EFD-SALARY.
113100     MOVE EFD-FLEX-PLAN              TO DB-FLEX-PLAN.
113200     MOVE EFD-START-DATE             TO DB-START-DATE.
113300     MOVE SPACES                     TO DB-GROUP-NAME.
113400     PERFORM 850-FIND-NLT-FLDSET1.
113500
113600     IF  (FLEXDOLLAR-FOUND)
113700     AND (FLD-COMPANY            = DB-COMPANY)
113800     AND (FLD-FLEX-PLAN          = DB-FLEX-PLAN)
113900     AND (FLD-START-DATE         NOT > DB-START-DATE)
114000     AND (PRM-PRINT-COMMENTS = "Y")
114100         MOVE WS-TRUE                TO WS-PRT-FLEX-PAGE-SW
114200         IF (EFD-SAL-CALC-DATE NOT = ZEROS)
114300             MOVE EFD-SAL-CALC-DATE  TO G3-EFD-SAL-CALC-DATE
114400         ELSE
114500             MOVE EFD-START-DATE     TO G3-EFD-SAL-CALC-DATE
114600         END-IF
114700         IF (EFD-AGE-CALC-DATE NOT = ZEROS)
114800             MOVE EFD-AGE-CALC-DATE  TO G3-EFD-AGE-CALC-DATE
114900         ELSE
115000             MOVE EFD-START-DATE     TO G3-EFD-AGE-CALC-DATE
115100         END-IF
115200         IF (EFD-SERV-CALC-DATE NOT = ZEROS)
115300             MOVE EFD-SERV-CALC-DATE  TO G3-EFD-SERV-CALC-DATE
115400         ELSE
115500             MOVE EFD-START-DATE      TO G3-EFD-SERV-CALC-DATE
115600         END-IF
115700         MOVE "FL"                   TO DB-CMT-TYPE
115800         MOVE ZEROS                  TO DB-EMPLOYEE
115900                                        DB-PARTICIPNT
116000         MOVE SPACES                 TO DB-PLAN-TYPE
116100         MOVE FLD-FLEX-PLAN          TO DB-PLAN-CODE
116200         MOVE FLD-GROUP-NAME         TO DB-GROUP-NAME
116300         MOVE FLD-START-DATE         TO DB-START-DATE
116400         MOVE ZEROS                  TO DB-SEQ-NBR
116500         PERFORM 850-FIND-NLT-BCMSET1
116600         IF  (BNCOMMENTS-FOUND)
116700         AND (BCM-COMPANY     = DB-COMPANY)
116800         AND (BCM-CMT-TYPE    = DB-CMT-TYPE)
116900         AND (BCM-EMPLOYEE    = DB-EMPLOYEE)
117000         AND (BCM-PARTICIPNT  = DB-PARTICIPNT)
117100         AND (BCM-PLAN-TYPE   = DB-PLAN-TYPE)
117200         AND (BCM-PLAN-CODE   = DB-PLAN-CODE)
117300         AND (BCM-GROUP-NAME  = DB-GROUP-NAME)
117400         AND (BCM-START-DATE  = DB-START-DATE)
117500             MOVE SPACES                 TO G20-BCM-CMT-TEXT
117600             MOVE GN20-COMMENTS-LINE     TO RPT-GROUP-REQUEST
117700             PERFORM 700-PRINT-RPT-GRP
117800         END-IF
117900         PERFORM 4410-PRT-COMMENT-LINE
118000         THRU    4410-END
118100             UNTIL (BNCOMMENTS-NOTFOUND)
118200             OR    (BCM-COMPANY    NOT = DB-COMPANY)
118300             OR    (BCM-CMT-TYPE   NOT = DB-CMT-TYPE)
118400             OR    (BCM-EMPLOYEE   NOT = DB-EMPLOYEE)
118500             OR    (BCM-PARTICIPNT NOT = DB-PARTICIPNT)
118600             OR    (BCM-PLAN-TYPE  NOT = DB-PLAN-TYPE)
118700             OR    (BCM-PLAN-CODE  NOT = DB-PLAN-CODE)
118800             OR    (BCM-GROUP-NAME NOT = DB-GROUP-NAME)
118900             OR    (BCM-START-DATE NOT = DB-START-DATE)
119000         MOVE WS-FALSE             TO WS-PRT-FLEX-PAGE-SW
119100     ELSE
119200     IF (EFD-START-DATE NOT = ZEROS)
119300         MOVE EFD-START-DATE     TO G3-EFD-SAL-CALC-DATE
119400                                    G3-EFD-AGE-CALC-DATE
119500                                    G3-EFD-SERV-CALC-DATE
119600     ELSE
119700         MOVE ZEROS              TO G3-EFD-SAL-CALC-DATE
119800                                    G3-EFD-AGE-CALC-DATE
119900                                    G3-EFD-SERV-CALC-DATE.
120000
120100     MOVE GN3D-FLEX-SUMMARY      TO RPT-GROUP-REQUEST.
120200     PERFORM 700-PRINT-RPT-GRP.
120300
120400 4100-END.
120500******************************************************************
120600 4201-DO-BEN-PLAN-TYPE.
120700******************************************************************
120800
120900     MOVE SF-PLAN-TYPE               TO WS-PLAN-TYPE.
           INITIALIZE WS-DED-CODES.
121000
121100     IF (SF-PLAN-TYPE = "HL")
121200         IF (PRM-PAGE-BREAK = "Y")            
121300             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
121400             PERFORM 700-PRINT-RPT-GRP
121500         END-IF
121600         MOVE GN4H-BEN-HEALTH        TO RPT-GROUP-REQUEST
121700         PERFORM 700-PRINT-RPT-GRP
121800         PERFORM 4210-PRT-BEN-HEALTH
121900         THRU    4210-END
122000             UNTIL (END-OF-FILE)
122100             OR    (SF-LOCAT-CODE NOT = WS-PEM-LOCAT-CODE)
122200             OR    (SF-EMPLOYEE   NOT = WS-BEN-EMPLOYEE)
122300             OR    (SF-PLAN-TYPE  NOT = WS-PLAN-TYPE)
122400     ELSE
122500     IF (SF-PLAN-TYPE = "DN")
122600         IF (PRM-PAGE-BREAK = "Y")            
122700             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
122800             PERFORM 700-PRINT-RPT-GRP
122900         END-IF
123000         MOVE GN5H-BEN-DENTAL        TO RPT-GROUP-REQUEST
123100         PERFORM 700-PRINT-RPT-GRP
123200         PERFORM 4220-PRT-BEN-DENTAL
123300         THRU    4220-END
123400             UNTIL (END-OF-FILE)
123500             OR    (SF-LOCAT-CODE NOT = WS-PEM-LOCAT-CODE)
123600             OR    (SF-EMPLOYEE   NOT = WS-BEN-EMPLOYEE)
123700             OR    (SF-PLAN-TYPE  NOT = WS-PLAN-TYPE)
123800     ELSE
123900     IF (SF-PLAN-TYPE = "DI")
124000         IF (PRM-PAGE-BREAK = "Y")            
124100             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
124200             PERFORM 700-PRINT-RPT-GRP
124300         END-IF
124400         MOVE GN6H-BEN-DISABILITY    TO RPT-GROUP-REQUEST
124500         PERFORM 700-PRINT-RPT-GRP
124600         PERFORM 4230-PRT-BEN-DISABILITY
124700         THRU    4230-END
124800             UNTIL (END-OF-FILE)
124900             OR    (SF-LOCAT-CODE NOT = WS-PEM-LOCAT-CODE)
125000             OR    (SF-EMPLOYEE   NOT = WS-BEN-EMPLOYEE)
125100             OR    (SF-PLAN-TYPE  NOT = WS-PLAN-TYPE)
125200     ELSE
125300     IF (SF-PLAN-TYPE = "EL")
125400         IF (PRM-PAGE-BREAK = "Y")            
125500             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
125600             PERFORM 700-PRINT-RPT-GRP
125700         END-IF
125800         MOVE GN7H-BEN-EMPL-LIFE-ADD TO RPT-GROUP-REQUEST
125900         PERFORM 700-PRINT-RPT-GRP
126000         PERFORM 4240-PRT-BEN-EMPL-LIFE-ADD
126100         THRU    4240-END
126200             UNTIL (END-OF-FILE)
126300             OR    (SF-LOCAT-CODE NOT = WS-PEM-LOCAT-CODE)
126400             OR    (SF-EMPLOYEE   NOT = WS-BEN-EMPLOYEE)
126500             OR    (SF-PLAN-TYPE  NOT = WS-PLAN-TYPE)
126600     ELSE
126700     IF (SF-PLAN-TYPE = "DL")
126800         IF (PRM-PAGE-BREAK = "Y")            
126900             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
127000             PERFORM 700-PRINT-RPT-GRP
127100         END-IF
127200         MOVE GN8H-BEN-DEPLIFE       TO RPT-GROUP-REQUEST
127300         PERFORM 700-PRINT-RPT-GRP
127400         PERFORM 4250-PRT-BEN-DEPLIFE
127500         THRU    4250-END
127600             UNTIL (END-OF-FILE)
127700             OR    (SF-LOCAT-CODE NOT = WS-PEM-LOCAT-CODE)
127800             OR    (SF-EMPLOYEE   NOT = WS-BEN-EMPLOYEE)
127900             OR    (SF-PLAN-TYPE  NOT = WS-PLAN-TYPE)
128000     ELSE
128100     IF (SF-PLAN-TYPE = "DC")
128200         IF (PRM-PAGE-BREAK = "Y")            
128300             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
128400             PERFORM 700-PRINT-RPT-GRP
128500         END-IF
128600         MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE
128700         MOVE SF-PLAN-CODE           TO DB-PLAN-CODE
128800         PERFORM 840-FIND-PLNSET1
128900         IF (PLAN-FOUND)
129000             MOVE PLN-PLAN-OPTION    TO G9-PLAN-OPTION
129100         ELSE   
129200             MOVE 8                  TO G9-PLAN-OPTION
129300         END-IF
129400         MOVE GN9H-BEN-DEF-CONTRIB   TO RPT-GROUP-REQUEST
129500         PERFORM 700-PRINT-RPT-GRP
129600         PERFORM 4260-PRT-BEN-DEF-CONTRIB
129700         THRU    4260-END
129800             UNTIL (END-OF-FILE)
129900             OR    (SF-LOCAT-CODE NOT = WS-PEM-LOCAT-CODE)
130000             OR    (SF-EMPLOYEE   NOT = WS-BEN-EMPLOYEE)
130100             OR    (SF-PLAN-TYPE  NOT = WS-PLAN-TYPE)
130200     ELSE
130300     IF (SF-PLAN-TYPE = "DB")
130400         IF (PRM-PAGE-BREAK = "Y")            
130500             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
130600             PERFORM 700-PRINT-RPT-GRP
130700         END-IF
130800         MOVE GN10H-BEN-DEF-BENEFIT  TO RPT-GROUP-REQUEST
130900         PERFORM 700-PRINT-RPT-GRP
131000         PERFORM 4270-PRT-BEN-DEF-BENEFIT
131100         THRU    4270-END
131200             UNTIL (END-OF-FILE)
131300             OR    (SF-LOCAT-CODE NOT = WS-PEM-LOCAT-CODE)
131400             OR    (SF-EMPLOYEE   NOT = WS-BEN-EMPLOYEE)
131500             OR    (SF-PLAN-TYPE  NOT = WS-PLAN-TYPE)
131600     ELSE
131700     IF (SF-PLAN-TYPE = "VA")
131800         IF (PRM-PAGE-BREAK = "Y")            
131900             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
132000             PERFORM 700-PRINT-RPT-GRP
132100         END-IF
132200         MOVE GN11H-BEN-VACATION     TO RPT-GROUP-REQUEST
132300         PERFORM 700-PRINT-RPT-GRP
132400         PERFORM 4280-PRT-BEN-VACATION
132500         THRU    4280-END
132600             UNTIL (END-OF-FILE)
132700             OR    (SF-LOCAT-CODE NOT = WS-PEM-LOCAT-CODE)
132800             OR    (SF-EMPLOYEE   NOT = WS-BEN-EMPLOYEE)
132900             OR    (SF-PLAN-TYPE  NOT = WS-PLAN-TYPE)
133000     ELSE
133100     IF (SF-PLAN-TYPE = "RS")
133200         IF (PRM-PAGE-BREAK = "Y")            
133300             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
133400             PERFORM 700-PRINT-RPT-GRP
133500         END-IF
133600         MOVE GN12H-BEN-RESERVE-ACCT TO RPT-GROUP-REQUEST
133700         PERFORM 700-PRINT-RPT-GRP
133800         PERFORM 4290-PRT-BEN-RESERVE-ACCT
133900         THRU    4290-END
134000             UNTIL (END-OF-FILE)
134100             OR    (SF-LOCAT-CODE NOT = WS-PEM-LOCAT-CODE)
134200             OR    (SF-EMPLOYEE   NOT = WS-BEN-EMPLOYEE)
134300             OR    (SF-PLAN-TYPE  NOT = WS-PLAN-TYPE)
134400     ELSE
134500     IF (SF-PLAN-TYPE = "SB")
134600         IF (PRM-PAGE-BREAK = "Y")            
134700             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
134800             PERFORM 700-PRINT-RPT-GRP
134900         END-IF
135000         MOVE GN13H-BEN-SAVINGS-BOND TO RPT-GROUP-REQUEST
135100         PERFORM 700-PRINT-RPT-GRP
135200         PERFORM 4300-PRT-BEN-SAVINGS-BOND
135300         THRU    4300-END
135400             UNTIL (END-OF-FILE)
135500             OR    (SF-LOCAT-CODE NOT = WS-PEM-LOCAT-CODE)
135600             OR    (SF-EMPLOYEE   NOT = WS-BEN-EMPLOYEE)
135700             OR    (SF-PLAN-TYPE  NOT = WS-PLAN-TYPE)
135800     ELSE
135900     IF (SF-PLAN-TYPE = "SP")
136000         IF (PRM-PAGE-BREAK = "Y")            
136100             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
136200             PERFORM 700-PRINT-RPT-GRP
136300         END-IF
136400         MOVE GN14H-BEN-STOCK-PURCH  TO RPT-GROUP-REQUEST
136500         PERFORM 700-PRINT-RPT-GRP
136600         PERFORM 4310-PRT-BEN-STOCK-PURCH
136700         THRU    4310-END
136800             UNTIL (END-OF-FILE)
136900             OR    (SF-LOCAT-CODE NOT = WS-PEM-LOCAT-CODE)
137000             OR    (SF-EMPLOYEE   NOT = WS-BEN-EMPLOYEE)
137100             OR    (SF-PLAN-TYPE  NOT = WS-PLAN-TYPE).
137200
137300 4201-END.
137400******************************************************************
137500 4202-DO-BEN-PLAN-TYPE.
137600******************************************************************
137700
137800     MOVE SF-PLAN-TYPE               TO WS-PLAN-TYPE.
           INITIALIZE WS-DED-CODES.
137900
138000     IF (SF-PLAN-TYPE = "HL")
138100         IF (PRM-PAGE-BREAK = "Y")            
138200             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
138300             PERFORM 700-PRINT-RPT-GRP
138400         END-IF
138500         MOVE GN4H-BEN-HEALTH        TO RPT-GROUP-REQUEST
138600         PERFORM 700-PRINT-RPT-GRP
138700         PERFORM 4210-PRT-BEN-HEALTH
138800         THRU    4210-END
138900             UNTIL (END-OF-FILE)
139000             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
139100             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
139200             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
139300             OR    (SF-USER-LEVEL    NOT = WS-EMP-USER-LEVEL)
139400             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
139500             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
139600     ELSE
139700     IF (SF-PLAN-TYPE = "DN")
139800         IF (PRM-PAGE-BREAK = "Y")            
139900             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
140000             PERFORM 700-PRINT-RPT-GRP
140100         END-IF
140200         MOVE GN5H-BEN-DENTAL        TO RPT-GROUP-REQUEST
140300         PERFORM 700-PRINT-RPT-GRP
140400         PERFORM 4220-PRT-BEN-DENTAL
140500         THRU    4220-END
140600             UNTIL (END-OF-FILE)
140700             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
140800             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
140900             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
141000             OR    (SF-USER-LEVEL    NOT = WS-EMP-USER-LEVEL)
141100             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
141200             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
141300     ELSE
141400     IF (SF-PLAN-TYPE = "DI")
141500         IF (PRM-PAGE-BREAK = "Y")            
141600             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
141700             PERFORM 700-PRINT-RPT-GRP
141800         END-IF
141900         MOVE GN6H-BEN-DISABILITY    TO RPT-GROUP-REQUEST
142000         PERFORM 700-PRINT-RPT-GRP
142100         PERFORM 4230-PRT-BEN-DISABILITY
142200         THRU    4230-END
142300             UNTIL (END-OF-FILE)
142400             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
142500             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
142600             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
142700             OR    (SF-USER-LEVEL    NOT = WS-EMP-USER-LEVEL)
142800             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
142900             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
143000     ELSE
143100     IF (SF-PLAN-TYPE = "EL")
143200         IF (PRM-PAGE-BREAK = "Y")            
143300             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
143400             PERFORM 700-PRINT-RPT-GRP
143500         END-IF
143600         MOVE GN7H-BEN-EMPL-LIFE-ADD TO RPT-GROUP-REQUEST
143700         PERFORM 700-PRINT-RPT-GRP
143800         PERFORM 4240-PRT-BEN-EMPL-LIFE-ADD
143900         THRU    4240-END
144000             UNTIL (END-OF-FILE)
144100             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
144200             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
144300             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
144400             OR    (SF-USER-LEVEL    NOT = WS-EMP-USER-LEVEL)
144500             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
144600             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
144700     ELSE
144800     IF (SF-PLAN-TYPE = "DL")
144900         IF (PRM-PAGE-BREAK = "Y")            
145000             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
145100             PERFORM 700-PRINT-RPT-GRP
145200         END-IF
145300         MOVE GN8H-BEN-DEPLIFE       TO RPT-GROUP-REQUEST
145400         PERFORM 700-PRINT-RPT-GRP
145500         PERFORM 4250-PRT-BEN-DEPLIFE
145600         THRU    4250-END
145700             UNTIL (END-OF-FILE)
145800             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
145900             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
146000             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
146100             OR    (SF-USER-LEVEL    NOT = WS-EMP-USER-LEVEL)
146200             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
146300             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
146400     ELSE
146500     IF (SF-PLAN-TYPE = "DC")
146600         IF (PRM-PAGE-BREAK = "Y")            
146700             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
146800             PERFORM 700-PRINT-RPT-GRP
146900         END-IF
147000         MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE
147100         MOVE SF-PLAN-CODE           TO DB-PLAN-CODE
147200         PERFORM 840-FIND-PLNSET1
147300         IF (PLAN-FOUND)
147400             MOVE PLN-PLAN-OPTION    TO G9-PLAN-OPTION
147500         ELSE   
147600             MOVE 8                  TO G9-PLAN-OPTION
147700         END-IF
147800         MOVE GN9H-BEN-DEF-CONTRIB   TO RPT-GROUP-REQUEST
147900         PERFORM 700-PRINT-RPT-GRP
148000         PERFORM 4260-PRT-BEN-DEF-CONTRIB
148100         THRU    4260-END
148200             UNTIL (END-OF-FILE)
148300             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
148400             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
148500             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
148600             OR    (SF-USER-LEVEL    NOT = WS-EMP-USER-LEVEL)
148700             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
148800             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
148900     ELSE
149000     IF (SF-PLAN-TYPE = "DB")
149100         IF (PRM-PAGE-BREAK = "Y")            
149200             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
149300             PERFORM 700-PRINT-RPT-GRP
149400         END-IF
149500         MOVE GN10H-BEN-DEF-BENEFIT  TO RPT-GROUP-REQUEST
149600         PERFORM 700-PRINT-RPT-GRP
149700         PERFORM 4270-PRT-BEN-DEF-BENEFIT
149800         THRU    4270-END
149900             UNTIL (END-OF-FILE)
150000             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
150100             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
150200             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
150300             OR    (SF-USER-LEVEL    NOT = WS-EMP-USER-LEVEL)
150400             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
150500             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
150600     ELSE
150700     IF (SF-PLAN-TYPE = "VA")
150800         IF (PRM-PAGE-BREAK = "Y")            
150900             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
151000             PERFORM 700-PRINT-RPT-GRP
151100         END-IF
151200         MOVE GN11H-BEN-VACATION     TO RPT-GROUP-REQUEST
151300         PERFORM 700-PRINT-RPT-GRP
151400         PERFORM 4280-PRT-BEN-VACATION
151500         THRU    4280-END
151600             UNTIL (END-OF-FILE)
151700             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
151800             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
151900             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
152000             OR    (SF-USER-LEVEL    NOT = WS-EMP-USER-LEVEL)
152100             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
152200             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
152300     ELSE
152400     IF (SF-PLAN-TYPE = "RS")
152500         IF (PRM-PAGE-BREAK = "Y")            
152600             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
152700             PERFORM 700-PRINT-RPT-GRP
152800         END-IF
152900         MOVE GN12H-BEN-RESERVE-ACCT TO RPT-GROUP-REQUEST
153000         PERFORM 700-PRINT-RPT-GRP
153100         PERFORM 4290-PRT-BEN-RESERVE-ACCT
153200         THRU    4290-END
153300             UNTIL (END-OF-FILE)
153400             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
153500             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
153600             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
153700             OR    (SF-USER-LEVEL    NOT = WS-EMP-USER-LEVEL)
153800             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
153900             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
154000     ELSE
154100     IF (SF-PLAN-TYPE = "SB")
154200         IF (PRM-PAGE-BREAK = "Y")            
154300             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
154400             PERFORM 700-PRINT-RPT-GRP
154500         END-IF
154600         MOVE GN13H-BEN-SAVINGS-BOND TO RPT-GROUP-REQUEST
154700         PERFORM 700-PRINT-RPT-GRP
154800         PERFORM 4300-PRT-BEN-SAVINGS-BOND
154900         THRU    4300-END
155000             UNTIL (END-OF-FILE)
155100             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
155200             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
155300             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
155400             OR    (SF-USER-LEVEL    NOT = WS-EMP-USER-LEVEL)
155500             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
155600             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
155700     ELSE
155800     IF (SF-PLAN-TYPE = "SP")
155900         IF (PRM-PAGE-BREAK = "Y")            
156000             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
156100             PERFORM 700-PRINT-RPT-GRP
156200         END-IF
156300         MOVE GN14H-BEN-STOCK-PURCH  TO RPT-GROUP-REQUEST
156400         PERFORM 700-PRINT-RPT-GRP
156500         PERFORM 4310-PRT-BEN-STOCK-PURCH
156600         THRU    4310-END
156700             UNTIL (END-OF-FILE)
156800             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
156900             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
157000             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
157100             OR    (SF-USER-LEVEL    NOT = WS-EMP-USER-LEVEL)
157200             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
157300             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE).
157400
157500 4202-END.
157600******************************************************************
157700 4203-DO-BEN-PLAN-TYPE.
157800******************************************************************
157900
158000     MOVE SF-PLAN-TYPE               TO WS-PLAN-TYPE.
           INITIALIZE WS-DED-CODES.
158100
158200     IF (SF-PLAN-TYPE = "HL")
158300         IF (PRM-PAGE-BREAK = "Y")            
158400             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
158500             PERFORM 700-PRINT-RPT-GRP
158600         END-IF
158700         MOVE GN4H-BEN-HEALTH       TO RPT-GROUP-REQUEST
158800         PERFORM 700-PRINT-RPT-GRP
158900         PERFORM 4210-PRT-BEN-HEALTH
159000         THRU    4210-END
159100             UNTIL (END-OF-FILE)
159200             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
159300             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
159400             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
159500             OR    (SF-SUPERVISOR    NOT = WS-EMP-SUPERVISOR)
159600             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
159700             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
159800     ELSE
159900     IF (SF-PLAN-TYPE = "DN")
160000         IF (PRM-PAGE-BREAK = "Y")            
160100             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
160200             PERFORM 700-PRINT-RPT-GRP
160300         END-IF
160400         MOVE GN5H-BEN-DENTAL       TO RPT-GROUP-REQUEST
160500         PERFORM 700-PRINT-RPT-GRP
160600         PERFORM 4220-PRT-BEN-DENTAL
160700         THRU    4220-END
160800             UNTIL (END-OF-FILE)
160900             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
161000             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
161100             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
161200             OR    (SF-SUPERVISOR    NOT = WS-EMP-SUPERVISOR)
161300             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
161400             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
161500     ELSE
161600     IF (SF-PLAN-TYPE = "DI")
161700         IF (PRM-PAGE-BREAK = "Y")            
161800             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
161900             PERFORM 700-PRINT-RPT-GRP
162000         END-IF
162100         MOVE GN6H-BEN-DISABILITY    TO RPT-GROUP-REQUEST
162200         PERFORM 700-PRINT-RPT-GRP
162300         PERFORM 4230-PRT-BEN-DISABILITY
162400         THRU    4230-END
162500             UNTIL (END-OF-FILE)
162600             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
162700             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
162800             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
162900             OR    (SF-SUPERVISOR    NOT = WS-EMP-SUPERVISOR)
163000             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
163100             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
163200     ELSE
163300     IF (SF-PLAN-TYPE = "EL")
163400         IF (PRM-PAGE-BREAK = "Y")            
163500             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
163600             PERFORM 700-PRINT-RPT-GRP
163700         END-IF
163800         MOVE GN7H-BEN-EMPL-LIFE-ADD TO RPT-GROUP-REQUEST
163900         PERFORM 700-PRINT-RPT-GRP
164000         PERFORM 4240-PRT-BEN-EMPL-LIFE-ADD
164100         THRU    4240-END
164200             UNTIL (END-OF-FILE)
164300             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
164400             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
164500             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
164600             OR    (SF-SUPERVISOR    NOT = WS-EMP-SUPERVISOR)
164700             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
164800             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
164900     ELSE
165000     IF (SF-PLAN-TYPE = "DL")
165100         IF (PRM-PAGE-BREAK = "Y")            
165200             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
165300             PERFORM 700-PRINT-RPT-GRP
165400         END-IF
165500         MOVE GN8H-BEN-DEPLIFE       TO RPT-GROUP-REQUEST
165600         PERFORM 700-PRINT-RPT-GRP
165700         PERFORM 4250-PRT-BEN-DEPLIFE
165800         THRU    4250-END
165900             UNTIL (END-OF-FILE)
166000             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
166100             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
166200             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
166300             OR    (SF-SUPERVISOR    NOT = WS-EMP-SUPERVISOR)
166400             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
166500             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
166600     ELSE
166700     IF (SF-PLAN-TYPE = "DC")
166800         IF (PRM-PAGE-BREAK = "Y")            
166900             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
167000             PERFORM 700-PRINT-RPT-GRP
167100         END-IF
167200         MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE
167300         MOVE SF-PLAN-CODE           TO DB-PLAN-CODE
167400         PERFORM 840-FIND-PLNSET1
167500         IF (PLAN-FOUND)
167600             MOVE PLN-PLAN-OPTION    TO G9-PLAN-OPTION
167700         ELSE   
167800             MOVE 8                  TO G9-PLAN-OPTION
167900         END-IF
168000         MOVE GN9H-BEN-DEF-CONTRIB   TO RPT-GROUP-REQUEST
168100         PERFORM 700-PRINT-RPT-GRP
168200         PERFORM 4260-PRT-BEN-DEF-CONTRIB
168300         THRU    4260-END
168400             UNTIL (END-OF-FILE)
168500             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
168600             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
168700             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
168800             OR    (SF-SUPERVISOR    NOT = WS-EMP-SUPERVISOR)
168900             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
169000             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
169100     ELSE
169200     IF (SF-PLAN-TYPE = "DB")
169300         IF (PRM-PAGE-BREAK = "Y")            
169400             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
169500             PERFORM 700-PRINT-RPT-GRP
169600         END-IF
169700         MOVE GN10H-BEN-DEF-BENEFIT  TO RPT-GROUP-REQUEST
169800         PERFORM 700-PRINT-RPT-GRP
169900         PERFORM 4270-PRT-BEN-DEF-BENEFIT
170000         THRU    4270-END
170100             UNTIL (END-OF-FILE)
170200             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
170300             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
170400             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
170500             OR    (SF-SUPERVISOR    NOT = WS-EMP-SUPERVISOR)
170600             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
170700             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
170800     ELSE
170900     IF (SF-PLAN-TYPE = "VA")
171000         IF (PRM-PAGE-BREAK = "Y")            
171100             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
171200             PERFORM 700-PRINT-RPT-GRP
171300         END-IF
171400         MOVE GN11H-BEN-VACATION     TO RPT-GROUP-REQUEST
171500         PERFORM 700-PRINT-RPT-GRP
171600         PERFORM 4280-PRT-BEN-VACATION
171700         THRU    4280-END
171800             UNTIL (END-OF-FILE)
171900             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
172000             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
172100             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
172200             OR    (SF-SUPERVISOR    NOT = WS-EMP-SUPERVISOR)
172300             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
172400             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
172500     ELSE
172600     IF (SF-PLAN-TYPE = "RS")
172700         IF (PRM-PAGE-BREAK = "Y")            
172800             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
172900             PERFORM 700-PRINT-RPT-GRP
173000         END-IF
173100         MOVE GN12H-BEN-RESERVE-ACCT TO RPT-GROUP-REQUEST
173200         PERFORM 700-PRINT-RPT-GRP
173300         PERFORM 4290-PRT-BEN-RESERVE-ACCT
173400         THRU    4290-END
173500             UNTIL (END-OF-FILE)
173600             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
173700             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
173800             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
173900             OR    (SF-SUPERVISOR    NOT = WS-EMP-SUPERVISOR)
174000             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
174100             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
174200     ELSE
174300     IF (SF-PLAN-TYPE = "SB")
174400         IF (PRM-PAGE-BREAK = "Y")            
174500             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
174600             PERFORM 700-PRINT-RPT-GRP
174700         END-IF
174800         MOVE GN13H-BEN-SAVINGS-BOND TO RPT-GROUP-REQUEST
174900         PERFORM 700-PRINT-RPT-GRP
175000         PERFORM 4300-PRT-BEN-SAVINGS-BOND
175100         THRU    4300-END
175200             UNTIL (END-OF-FILE)
175300             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
175400             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
175500             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
175600             OR    (SF-SUPERVISOR    NOT = WS-EMP-SUPERVISOR)
175700             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
175800             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE)
175900     ELSE
176000     IF (SF-PLAN-TYPE = "SP")
176100         IF (PRM-PAGE-BREAK = "Y")            
176200             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
176300             PERFORM 700-PRINT-RPT-GRP
176400         END-IF
176500         MOVE GN14H-BEN-STOCK-PURCH  TO RPT-GROUP-REQUEST
176600         PERFORM 700-PRINT-RPT-GRP
176700         PERFORM 4310-PRT-BEN-STOCK-PURCH
176800         THRU    4310-END
176900             UNTIL (END-OF-FILE)
177000             OR    (SF-LOCAT-CODE    NOT = WS-PEM-LOCAT-CODE)
177100             OR    (SF-PROCESS-LEVEL NOT = WS-EMP-PROCESS-LEVEL)
177200             OR    (SF-DEPARTMENT    NOT = WS-EMP-DEPARTMENT)
177300             OR    (SF-SUPERVISOR    NOT = WS-EMP-SUPERVISOR)
177400             OR    (SF-EMPLOYEE      NOT = WS-BEN-EMPLOYEE)
177500             OR    (SF-PLAN-TYPE     NOT = WS-PLAN-TYPE).
177600
177700 4203-END.
177800******************************************************************
177900 4204-DO-BEN-PLAN-TYPE.
178000******************************************************************
178100
178200     MOVE SF-PLAN-TYPE               TO WS-PLAN-TYPE.
           INITIALIZE WS-DED-CODES.
178300
178400     IF (SF-PLAN-TYPE = "HL")
178500         IF (PRM-PAGE-BREAK = "Y")            
178600             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
178700             PERFORM 700-PRINT-RPT-GRP
178800         END-IF
178900         MOVE GN4H-BEN-HEALTH        TO RPT-GROUP-REQUEST
179000         PERFORM 700-PRINT-RPT-GRP
179100         PERFORM 4210-PRT-BEN-HEALTH
179200         THRU    4210-END
179300             UNTIL (END-OF-FILE)
179400             OR    (SF-EMPLOYEE  NOT = WS-BEN-EMPLOYEE)
179500             OR    (SF-PLAN-TYPE NOT = WS-PLAN-TYPE)
179600     ELSE
179700     IF (SF-PLAN-TYPE = "DN")
179800         IF (PRM-PAGE-BREAK = "Y")            
179900             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
180000             PERFORM 700-PRINT-RPT-GRP
180100         END-IF
180200         MOVE GN5H-BEN-DENTAL        TO RPT-GROUP-REQUEST
180300         PERFORM 700-PRINT-RPT-GRP
180400         PERFORM 4220-PRT-BEN-DENTAL
180500         THRU    4220-END
180600             UNTIL (END-OF-FILE)
180700             OR    (SF-EMPLOYEE  NOT = WS-BEN-EMPLOYEE)
180800             OR    (SF-PLAN-TYPE NOT = WS-PLAN-TYPE)
180900     ELSE
181000     IF (SF-PLAN-TYPE = "DI")
181100         IF (PRM-PAGE-BREAK = "Y")            
181200             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
181300             PERFORM 700-PRINT-RPT-GRP
181400         END-IF
181500         MOVE GN6H-BEN-DISABILITY    TO RPT-GROUP-REQUEST
181600         PERFORM 700-PRINT-RPT-GRP
181700         PERFORM 4230-PRT-BEN-DISABILITY
181800         THRU    4230-END
181900             UNTIL (END-OF-FILE)
182000             OR    (SF-EMPLOYEE  NOT = WS-BEN-EMPLOYEE)
182100             OR    (SF-PLAN-TYPE NOT = WS-PLAN-TYPE)
182200     ELSE
182300     IF (SF-PLAN-TYPE = "EL")
182400         IF (PRM-PAGE-BREAK = "Y")            
182500             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
182600             PERFORM 700-PRINT-RPT-GRP
182700         END-IF
182800         MOVE GN7H-BEN-EMPL-LIFE-ADD TO RPT-GROUP-REQUEST
182900         PERFORM 700-PRINT-RPT-GRP
183000         PERFORM 4240-PRT-BEN-EMPL-LIFE-ADD
183100         THRU    4240-END
183200             UNTIL (END-OF-FILE)
183300             OR    (SF-EMPLOYEE  NOT = WS-BEN-EMPLOYEE)
183400             OR    (SF-PLAN-TYPE NOT = WS-PLAN-TYPE)
183500     ELSE
183600     IF (SF-PLAN-TYPE = "DL")
183700         IF (PRM-PAGE-BREAK = "Y")            
183800             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
183900             PERFORM 700-PRINT-RPT-GRP
184000         END-IF
184100         MOVE GN8H-BEN-DEPLIFE       TO RPT-GROUP-REQUEST
184200         PERFORM 700-PRINT-RPT-GRP
184300         PERFORM 4250-PRT-BEN-DEPLIFE
184400         THRU    4250-END
184500             UNTIL (END-OF-FILE)
184600             OR    (SF-EMPLOYEE  NOT = WS-BEN-EMPLOYEE)
184700             OR    (SF-PLAN-TYPE NOT = WS-PLAN-TYPE)
184800     ELSE
184900     IF (SF-PLAN-TYPE = "DC")
185000         IF (PRM-PAGE-BREAK = "Y")            
185100             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
185200             PERFORM 700-PRINT-RPT-GRP
185300         END-IF
185400         MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE
185500         MOVE SF-PLAN-CODE           TO DB-PLAN-CODE
185600         PERFORM 840-FIND-PLNSET1
185700         IF (PLAN-FOUND)
185800             MOVE PLN-PLAN-OPTION    TO G9-PLAN-OPTION
185900         ELSE   
186000             MOVE 8                  TO G9-PLAN-OPTION
186100         END-IF
186200         MOVE GN9H-BEN-DEF-CONTRIB   TO RPT-GROUP-REQUEST
186300         PERFORM 700-PRINT-RPT-GRP
186400         PERFORM 4260-PRT-BEN-DEF-CONTRIB
186500         THRU    4260-END
186600             UNTIL (END-OF-FILE)
186700             OR    (SF-EMPLOYEE  NOT = WS-BEN-EMPLOYEE)
186800             OR    (SF-PLAN-TYPE NOT = WS-PLAN-TYPE)
186900     ELSE
187000     IF (SF-PLAN-TYPE = "DB")
187100         IF (PRM-PAGE-BREAK = "Y")            
187200             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
187300             PERFORM 700-PRINT-RPT-GRP
187400         END-IF
187500         MOVE GN10H-BEN-DEF-BENEFIT  TO RPT-GROUP-REQUEST
187600         PERFORM 700-PRINT-RPT-GRP
187700         PERFORM 4270-PRT-BEN-DEF-BENEFIT
187800         THRU    4270-END
187900             UNTIL (END-OF-FILE)
188000             OR    (SF-EMPLOYEE  NOT = WS-BEN-EMPLOYEE)
188100             OR    (SF-PLAN-TYPE NOT = WS-PLAN-TYPE)
188200     ELSE
188300     IF (SF-PLAN-TYPE = "VA")
188400         IF (PRM-PAGE-BREAK = "Y")            
188500             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
188600             PERFORM 700-PRINT-RPT-GRP
188700         END-IF
188800         MOVE GN11H-BEN-VACATION     TO RPT-GROUP-REQUEST
188900         PERFORM 700-PRINT-RPT-GRP
189000         PERFORM 4280-PRT-BEN-VACATION
189100         THRU    4280-END
189200             UNTIL (END-OF-FILE)
189300             OR    (SF-EMPLOYEE  NOT = WS-BEN-EMPLOYEE)
189400             OR    (SF-PLAN-TYPE NOT = WS-PLAN-TYPE)
189500     ELSE
189600     IF (SF-PLAN-TYPE = "RS")
189700         IF (PRM-PAGE-BREAK = "Y")            
189800             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
189900             PERFORM 700-PRINT-RPT-GRP
190000         END-IF
190100         MOVE GN12H-BEN-RESERVE-ACCT TO RPT-GROUP-REQUEST
190200         PERFORM 700-PRINT-RPT-GRP
190300         PERFORM 4290-PRT-BEN-RESERVE-ACCT
190400         THRU    4290-END
190500             UNTIL (END-OF-FILE)
190600             OR    (SF-EMPLOYEE  NOT = WS-BEN-EMPLOYEE)
190700             OR    (SF-PLAN-TYPE NOT = WS-PLAN-TYPE)
190800     ELSE
190900     IF (SF-PLAN-TYPE = "SB")
191000         IF (PRM-PAGE-BREAK = "Y")            
191100             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
191200             PERFORM 700-PRINT-RPT-GRP
191300         END-IF
191400         MOVE GN13H-BEN-SAVINGS-BOND TO RPT-GROUP-REQUEST
191500         PERFORM 700-PRINT-RPT-GRP
191600         PERFORM 4300-PRT-BEN-SAVINGS-BOND
191700         THRU    4300-END
191800             UNTIL (END-OF-FILE)
191900             OR    (SF-EMPLOYEE  NOT = WS-BEN-EMPLOYEE)
192000             OR    (SF-PLAN-TYPE NOT = WS-PLAN-TYPE)
192100     ELSE
192200     IF (SF-PLAN-TYPE = "SP")
192300         IF (PRM-PAGE-BREAK = "Y")            
192400             MOVE GN1-PAGE-NBR-LINE  TO RPT-GROUP-REQUEST
192500             PERFORM 700-PRINT-RPT-GRP
192600         END-IF
192700         MOVE GN14H-BEN-STOCK-PURCH  TO RPT-GROUP-REQUEST
192800         PERFORM 700-PRINT-RPT-GRP
192900         PERFORM 4310-PRT-BEN-STOCK-PURCH
193000         THRU    4310-END
193100             UNTIL (END-OF-FILE)
193200             OR    (SF-EMPLOYEE  NOT = WS-BEN-EMPLOYEE)
193300             OR    (SF-PLAN-TYPE NOT = WS-PLAN-TYPE).
193400
193500 4204-END.
193600******************************************************************
193700 4210-PRT-BEN-HEALTH.
193800******************************************************************
193900
194000     MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE.
194100     MOVE SF-PLAN-CODE           TO DB-PLAN-CODE.
194200     PERFORM 840-FIND-PLNSET1.
194300
194400     IF (PLAN-FOUND)
194500         MOVE PLN-DESC               TO G4-PLN-DESC
194600         MOVE PLN-CMP-DED-CODE-A     TO WS-CMP-DED-CODE-A
194700         ADD 1                       TO I3
194800         IF (I3 > WS-MAX-TBL-SIZE)
194900             NEXT SENTENCE
195000         ELSE
195100             PERFORM 4600-GET-DEDUCTIONS
195200             THRU    4600-END
195300     ELSE
195400         MOVE 106                    TO CRT-MSG-NBR
195500         PERFORM 790-GET-MSG
195600         MOVE CRT-MESSAGE            TO G4-PLN-DESC.
195700              
195800     MOVE GN4D-BEN-HEALTH            TO RPT-GROUP-REQUEST.
195900     PERFORM 700-PRINT-RPT-GRP.
196000
196100     IF (PRM-PRINT-COMMENTS = "Y")
196200         PERFORM 4400-PRT-COMMENTS
196300         THRU    4400-END.
196400
196500     IF (PLN-WAIVE-FLAG NOT = "Y")
196600         PERFORM 4375-PRT-BEN-COVERAGE
196700         THRU    4375-END.
196800
196900     MOVE SF-EMP-PAY-RATE            TO WS-SAVE-PAY-RATE.
197000
197100     RETURN SORT-FILE                INTO SF-SORT-REC
197200         AT END
197300             MOVE WS-TRUE            TO WS-END-OF-FILE-SW.
197400
197500 4210-END.
197600******************************************************************
197700 4220-PRT-BEN-DENTAL.
197800******************************************************************
197900
198000     MOVE SF-PLAN-TYPE               TO DB-PLAN-TYPE.
198100     MOVE SF-PLAN-CODE               TO DB-PLAN-CODE.
198200     PERFORM 840-FIND-PLNSET1.
198300
198400     IF (PLAN-FOUND)
198500         MOVE PLN-DESC               TO G5-PLN-DESC
198600         MOVE PLN-CMP-DED-CODE-A     TO WS-CMP-DED-CODE-A
198700         ADD 1                       TO I3
198800         IF (I3 > WS-MAX-TBL-SIZE)
198900             NEXT SENTENCE
199000         ELSE
199100             PERFORM 4600-GET-DEDUCTIONS
199200             THRU    4600-END
199300     ELSE
199400         MOVE 106                    TO CRT-MSG-NBR
199500         PERFORM 790-GET-MSG
199600         MOVE CRT-MESSAGE            TO G5-PLN-DESC.
199700
199800     MOVE GN5D-BEN-DENTAL            TO RPT-GROUP-REQUEST.
199900     PERFORM 700-PRINT-RPT-GRP.
200000
200100     IF (PRM-PRINT-COMMENTS = "Y")
200200         PERFORM 4400-PRT-COMMENTS
200300         THRU    4400-END.
200400
200500     IF (PLN-WAIVE-FLAG NOT = "Y")
200600         PERFORM 4375-PRT-BEN-COVERAGE
200700         THRU    4375-END.
200800
200900     MOVE SF-EMP-PAY-RATE        TO WS-SAVE-PAY-RATE.
201000
201100     RETURN SORT-FILE            INTO SF-SORT-REC
201200         AT END
201300             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
201400
201500 4220-END.
201600
201700******************************************************************
201800 4230-PRT-BEN-DISABILITY.
201900******************************************************************
202000
202100     MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE.
202200     MOVE SF-PLAN-CODE           TO DB-PLAN-CODE.
202300     PERFORM 840-FIND-PLNSET1.
202400
202500     IF (PLAN-FOUND)
202600         MOVE PLN-DESC               TO G6D-PLN-DESC
202700         MOVE PLN-CMP-DED-CODE-A     TO WS-CMP-DED-CODE-A
202800         ADD 1                       TO I3
202900         IF (I3 > WS-MAX-TBL-SIZE)
203000             NEXT SENTENCE
203100         ELSE
203200             PERFORM 4600-GET-DEDUCTIONS
203300             THRU    4600-END
203400     ELSE
203500         MOVE 106            TO CRT-MSG-NBR
203600         PERFORM 790-GET-MSG
203700         MOVE CRT-MESSAGE    TO G6D-PLN-DESC.
203800
203900     MOVE GN6D-BEN-DISABILITY        TO RPT-GROUP-REQUEST.
204000     PERFORM 700-PRINT-RPT-GRP.
204100
204200     IF (PRM-PRINT-COMMENTS = "Y")
204300         PERFORM 4400-PRT-COMMENTS
204400         THRU    4400-END.
204500
204600     IF (PLN-WAIVE-FLAG NOT = "Y")
204700         PERFORM 4375-PRT-BEN-COVERAGE
204800         THRU    4375-END.
204900
205000     MOVE SF-EMPLOYEE            TO DB-EMPLOYEE.
205100     MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE.
205200     MOVE SF-PLAN-CODE           TO DB-PLAN-CODE.
205300     MOVE 1                      TO DB-PRIM-CNTGNT.
205400     MOVE ZEROS                  TO DB-SEQ-NBR.
205500     PERFORM 850-FIND-NLT-BNFSET2.
205600
205700     MOVE 110                    TO CRT-MSG-NBR.
205800     PERFORM 790-GET-MSG.
205900     MOVE CRT-MESSAGE            TO G6-BEN-TYPE-DESC.
206000
206100     MOVE WS-FALSE               TO WS-PRINTED-SW.
206200     IF  (BENEFICRY-FOUND)
206300     AND (BNF-COMPANY     = DB-COMPANY)
206400     AND (BNF-EMPLOYEE    = DB-EMPLOYEE)
206500     AND (BNF-PLAN-TYPE   = DB-PLAN-TYPE)
206600     AND (BNF-PLAN-CODE   = DB-PLAN-CODE)
206700     AND (BNF-PRIM-CNTGNT = DB-PRIM-CNTGNT)
206800         MOVE WS-TRUE            TO WS-PRINTED-SW
206900         MOVE GN6D-3-DI-SUMM     TO RPT-GROUP-REQUEST
207000         PERFORM 700-PRINT-RPT-GRP.
207100
207200     PERFORM 4235-PRT-BENEFICRY
207300     THRU    4235-END
207400         UNTIL (BENEFICRY-NOTFOUND)
207500         OR    (BNF-COMPANY     NOT = DB-COMPANY)
207600         OR    (BNF-EMPLOYEE    NOT = DB-EMPLOYEE)
207700         OR    (BNF-PLAN-TYPE   NOT = DB-PLAN-TYPE)
207800         OR    (BNF-PLAN-CODE   NOT = DB-PLAN-CODE)
207900         OR    (BNF-PRIM-CNTGNT NOT = DB-PRIM-CNTGNT).
208000
208100     MOVE 2                      TO DB-PRIM-CNTGNT.
208200     PERFORM 850-FIND-NLT-BNFSET2.
208300
208400     MOVE 111                    TO CRT-MSG-NBR.
208500     PERFORM 790-GET-MSG.
208600     MOVE CRT-MESSAGE            TO G6-BEN-TYPE-DESC.
208700
208800     IF  (BENEFICRY-FOUND)
208900     AND (WS-NOT-PRINTED)
209000     AND (BNF-COMPANY     = DB-COMPANY)
209100     AND (BNF-EMPLOYEE    = DB-EMPLOYEE)
209200     AND (BNF-PLAN-TYPE   = DB-PLAN-TYPE)
209300     AND (BNF-PLAN-CODE   = DB-PLAN-CODE)
209400     AND (BNF-PRIM-CNTGNT = DB-PRIM-CNTGNT)
209500         MOVE GN6D-3-DI-SUMM         TO RPT-GROUP-REQUEST
209600         PERFORM 700-PRINT-RPT-GRP.
209700
209800     PERFORM 4235-PRT-BENEFICRY
209900     THRU    4235-END
210000         UNTIL (BENEFICRY-NOTFOUND)
210100         OR    (BNF-COMPANY     NOT = DB-COMPANY)
210200         OR    (BNF-EMPLOYEE    NOT = DB-EMPLOYEE)
210300         OR    (BNF-PLAN-TYPE   NOT = DB-PLAN-TYPE)
210400         OR    (BNF-PLAN-CODE   NOT = DB-PLAN-CODE)
210500         OR    (BNF-PRIM-CNTGNT NOT = DB-PRIM-CNTGNT).
210600
210700     MOVE SF-EMP-PAY-RATE        TO WS-SAVE-PAY-RATE.
210800
210900     RETURN SORT-FILE       INTO SF-SORT-REC
211000         AT END
211100             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
211200
211300 4230-END.
211400******************************************************************
211500 4235-PRT-BENEFICRY.
211600******************************************************************
211700
           IF (BNF-BENEF-TYPE = 0)
211800         MOVE BNF-LAST-NAME          TO HRWS-LAST-NAME
211900         MOVE BNF-FIRST-NAME         TO HRWS-FIRST-NAME
212000         MOVE BNF-MIDDLE-INIT        TO HRWS-MIDDLE-INIT
               MOVE BNF-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE
               MOVE BNF-NAME-SUFFIX        TO HRWS-NAME-SUFFIX
               INITIALIZE HRWS-LAST-NAME-PRE
                          HRWS-NAME-SUFFIX
212100         PERFORM 750-HR-FORMAT-NAME
212200         MOVE HRWS-FORMAT-NAME       TO G6-BNF-NAME
           ELSE
               MOVE BNF-TRUST              TO G6-BNF-NAME.
212300
212400     MOVE GN6D-2-DI-SUMM         TO RPT-GROUP-REQUEST.
212500     PERFORM 700-PRINT-RPT-GRP.
212600
212700     MOVE SPACES                 TO G6-BEN-TYPE-DESC.
212800
212900     PERFORM 860-FIND-NEXT-BNFSET2.
213000
213100 4235-END.
213200******************************************************************
213300 4240-PRT-BEN-EMPL-LIFE-ADD.
213400******************************************************************
213500
213600     MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE.
213700     MOVE SF-PLAN-CODE           TO DB-PLAN-CODE.
213800     PERFORM 840-FIND-PLNSET1.
213900
214000     IF (PLAN-FOUND)
214100         MOVE PLN-DESC               TO G7-PLN-DESC
214200         MOVE PLN-CMP-DED-CODE-A     TO WS-CMP-DED-CODE-A
214300         ADD 1                       TO I3
214400         IF (I3 > WS-MAX-TBL-SIZE)
214500             NEXT SENTENCE
214600         ELSE
214700             PERFORM 4600-GET-DEDUCTIONS
214800             THRU    4600-END
214900     ELSE
215000         MOVE 106                    TO CRT-MSG-NBR
215100         PERFORM 790-GET-MSG
215200         MOVE CRT-MESSAGE            TO G7-PLN-DESC.
215300
215400     MOVE GN7D-BEN-EMPL-LIFE-ADD     TO RPT-GROUP-REQUEST.
215500     PERFORM 700-PRINT-RPT-GRP.
215600
215700     IF (PRM-PRINT-COMMENTS = "Y")
215800         PERFORM 4400-PRT-COMMENTS
215900         THRU    4400-END.
216000
216100     IF (PLN-WAIVE-FLAG NOT = "Y")
216200         PERFORM 4375-PRT-BEN-COVERAGE
216300         THRU    4375-END.
216400
216500     MOVE SF-EMPLOYEE            TO DB-EMPLOYEE.
216600     MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE.
216700     MOVE SF-PLAN-CODE           TO DB-PLAN-CODE.
216800     MOVE 1                      TO DB-PRIM-CNTGNT.
216900     MOVE ZEROS                  TO DB-SEQ-NBR.
217000     PERFORM 850-FIND-NLT-BNFSET2.
217100
217200     MOVE 113                    TO CRT-MSG-NBR.
217300     PERFORM 790-GET-MSG.
217400     MOVE CRT-MESSAGE            TO G7-BEN-TYPE-DESC.
217500
217600     MOVE WS-FALSE               TO WS-PRINTED-SW.
217700     IF  (BENEFICRY-FOUND)
217800     AND (BNF-COMPANY     = DB-COMPANY)
217900     AND (BNF-EMPLOYEE    = DB-EMPLOYEE)
218000     AND (BNF-PLAN-TYPE   = DB-PLAN-TYPE)
218100     AND (BNF-PLAN-CODE   = DB-PLAN-CODE)
218200     AND (BNF-PRIM-CNTGNT = DB-PRIM-CNTGNT)
218300         MOVE WS-TRUE            TO WS-PRINTED-SW
218400         MOVE GN7D-3-EL-SUMM     TO RPT-GROUP-REQUEST
218500         PERFORM 700-PRINT-RPT-GRP.
218600
218700     PERFORM 4245-PRT-BENEFICRY
218800     THRU    4245-END
218900         UNTIL (BENEFICRY-NOTFOUND)
219000         OR    (BNF-COMPANY     NOT = DB-COMPANY)
219100         OR    (BNF-EMPLOYEE    NOT = DB-EMPLOYEE)
219200         OR    (BNF-PLAN-TYPE   NOT = DB-PLAN-TYPE)
219300         OR    (BNF-PLAN-CODE   NOT = DB-PLAN-CODE)
219400         OR    (BNF-PRIM-CNTGNT NOT = DB-PRIM-CNTGNT).
219500
219600     MOVE 2                      TO DB-PRIM-CNTGNT.
219700     PERFORM 850-FIND-NLT-BNFSET2.
219800
219900     MOVE 111                    TO CRT-MSG-NBR.
220000     PERFORM 790-GET-MSG.
220100     MOVE CRT-MESSAGE            TO G7-BEN-TYPE-DESC.
220200
220300     IF  (BENEFICRY-FOUND)
220400     AND (WS-NOT-PRINTED)
220500     AND (BNF-COMPANY     = DB-COMPANY)
220600     AND (BNF-EMPLOYEE    = DB-EMPLOYEE)
220700     AND (BNF-PLAN-TYPE   = DB-PLAN-TYPE)
220800     AND (BNF-PLAN-CODE   = DB-PLAN-CODE)
220900     AND (BNF-PRIM-CNTGNT = DB-PRIM-CNTGNT)
221000         MOVE GN7D-3-EL-SUMM         TO RPT-GROUP-REQUEST
221100         PERFORM 700-PRINT-RPT-GRP.
221200
221300     PERFORM 4245-PRT-BENEFICRY
221400     THRU    4245-END
221500         UNTIL (BENEFICRY-NOTFOUND)
221600         OR    (BNF-COMPANY     NOT = DB-COMPANY)
221700         OR    (BNF-EMPLOYEE    NOT = DB-EMPLOYEE)
221800         OR    (BNF-PLAN-TYPE   NOT = DB-PLAN-TYPE)
221900         OR    (BNF-PLAN-CODE   NOT = DB-PLAN-CODE)
222000         OR    (BNF-PRIM-CNTGNT NOT = DB-PRIM-CNTGNT).
222100
222200     MOVE SF-EMP-PAY-RATE        TO WS-SAVE-PAY-RATE.
222300
222400     RETURN SORT-FILE       INTO SF-SORT-REC
222500         AT END
222600             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
222700
222800 4240-END.
222900******************************************************************
223000 4245-PRT-BENEFICRY.
223100******************************************************************
223200
223300     MOVE BNF-LAST-NAME          TO HRWS-LAST-NAME.
223400     MOVE BNF-FIRST-NAME         TO HRWS-FIRST-NAME.
223500     MOVE BNF-MIDDLE-INIT        TO HRWS-MIDDLE-INIT.
           INITIALIZE HRWS-LAST-NAME-PRE
                      HRWS-NAME-SUFFIX.
223600     PERFORM 750-HR-FORMAT-NAME.
223700     MOVE HRWS-FORMAT-NAME       TO G7-BNF-NAME.
223800
223900     MOVE GN7D-2-EL-SUMM         TO RPT-GROUP-REQUEST.
224000     PERFORM 700-PRINT-RPT-GRP.
224100
224200     MOVE SPACES                 TO G7-BEN-TYPE-DESC.
224300
224400     PERFORM 860-FIND-NEXT-BNFSET2.
224500
224600 4245-END.
224700******************************************************************
224800 4250-PRT-BEN-DEPLIFE.
224900******************************************************************
225000
225100     MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE.
225200     MOVE SF-PLAN-CODE           TO DB-PLAN-CODE.
225300     PERFORM 840-FIND-PLNSET1.
225400
225500     IF (PLAN-FOUND)
225600         MOVE PLN-DESC           TO G8D-PLN-DESC
225700         MOVE PLN-CMP-DED-CODE-A     TO WS-CMP-DED-CODE-A
225800         ADD 1                   TO I3
225900         IF (I3 > WS-MAX-TBL-SIZE)
226000             NEXT SENTENCE
226100         ELSE
226200             PERFORM 4600-GET-DEDUCTIONS
226300             THRU    4600-END
226400     ELSE
226500         MOVE 106                TO CRT-MSG-NBR
226600         PERFORM 790-GET-MSG
226700         MOVE CRT-MESSAGE        TO G8D-PLN-DESC.
226800
226900     MOVE GN8D-BEN-DEPLIFE       TO RPT-GROUP-REQUEST.
227000     PERFORM 700-PRINT-RPT-GRP.
227100
227200     IF (PRM-PRINT-COMMENTS = "Y")
227300         PERFORM 4400-PRT-COMMENTS
227400         THRU    4400-END.
227500
227600     IF (PLN-WAIVE-FLAG NOT = "Y")
227700         PERFORM 4375-PRT-BEN-COVERAGE
227800         THRU    4375-END.
227900
228000     MOVE SF-EMP-PAY-RATE        TO WS-SAVE-PAY-RATE.
228100
228200     RETURN SORT-FILE            INTO SF-SORT-REC
228300         AT END
228400             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
228500
228600 4250-END.
228700******************************************************************
228800 4260-PRT-BEN-DEF-CONTRIB.
228900******************************************************************
229000
229100     MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE.
229200     MOVE SF-PLAN-CODE           TO DB-PLAN-CODE.
229300     PERFORM 840-FIND-PLNSET1.
229400
229500     IF (PLAN-FOUND)
229600         PERFORM 4265-PLAN-FOUND
229700         THRU    4265-END
229800     ELSE
229900         MOVE 106                TO CRT-MSG-NBR
230000         PERFORM 790-GET-MSG
230100         MOVE CRT-MESSAGE        TO G9D-PLN-DESC
230200         MOVE 8                  TO G9-PLAN-OPTION.
230300
230400     MOVE GN9D-BEN-DEF-CONTRIB   TO RPT-GROUP-REQUEST.
230500     PERFORM 700-PRINT-RPT-GRP.
230600
230700     IF (PRM-PRINT-COMMENTS = "Y")
230800         PERFORM 4400-PRT-COMMENTS
230900         THRU    4400-END.
231000
231100     IF (PLAN-NOTFOUND)
231200         GO TO 4260-END
231300     ELSE
231400         MOVE PLN-START-DATE     TO WS-PLN-START-DATE
231500         MOVE WS-PRM-YYYY        TO WS-PLN-YYYY
231600         IF (WS-PRM-DATE < WS-PLN-START-DATE)
231700             COMPUTE WS-PLN-YYYY = WS-PLN-YYYY - 1.
231800
231900     IF (PLN-WAIVE-FLAG = "Y")
232000         GO TO 4260-CONTINUE.
232100
L10113     MOVE ZEROES                 TO WS-TOTAL-CONTRIB
L10113                                    BN232WS-AMOUNT-YTD-1
L10113                                    BN232WS-AMOUNT-YTD-2
L10113                                    BN232WS-AMOUNT-YTD-3.
J31490     MOVE ZEROES                 TO BN232WS-AMOUNT-YTD-4
J31490                                    BN232WS-AMOUNT-YTD-5
J31490                                    BN232WS-AMOUNT-YTD-6.
232200     MOVE SF-EMPLOYEE            TO DB-EMPLOYEE.
232300     MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE.
232300     MOVE SF-PLAN-CODE           TO DB-PLAN-CODE.
232400     MOVE WS-PLN-YYYY            TO DB-PLAN-YEAR.
L10113     MOVE CODSET1-PLAN-YEAR      TO WS-DB-BEG-RNG.
232500     PERFORM 850-FIND-BEGRNG-CODSET1.
232600
232700     IF  (CODA-FOUND)
L10113         PERFORM
L10113             UNTIL (CODA-NOTFOUND)
L10113             COMPUTE WS-TOTAL-CONTRIB    = WS-TOTAL-CONTRIB
232800                                         + COD-AMOUNT-YTD (1)
232900                                         + COD-AMOUNT-YTD (2)
233000                                         + COD-AMOUNT-YTD (3)
233100                                         + COD-AMOUNT-YTD (4)
233200                                         + COD-AMOUNT-YTD (5)
233300                                         + COD-AMOUNT-YTD (6)
L10113             ADD COD-AMOUNT-YTD (1)      TO BN232WS-AMOUNT-YTD-1
L10113             ADD COD-AMOUNT-YTD (2)      TO BN232WS-AMOUNT-YTD-2
L10113             ADD COD-AMOUNT-YTD (3)      TO BN232WS-AMOUNT-YTD-3
P79802             ADD COD-AMOUNT-YTD (4)      TO BN232WS-AMOUNT-YTD-4
P79802             ADD COD-AMOUNT-YTD (5)      TO BN232WS-AMOUNT-YTD-5
P79802             ADD COD-AMOUNT-YTD (6)      TO BN232WS-AMOUNT-YTD-6
L10113             PERFORM 860-FIND-NXTRNG-CODSET1
L10113         END-PERFORM
233400         MOVE WS-TOTAL-CONTRIB       TO G9-TOTAL-CONTRIB-YR
L10113         MOVE BN232WS-AMOUNT-YTD-1   TO G9-COD-AMOUNT-YTD-1
L10113         MOVE BN232WS-AMOUNT-YTD-2   TO G9-COD-AMOUNT-YTD-2
L10113         MOVE BN232WS-AMOUNT-YTD-3   TO G9-COD-AMOUNT-YTD-3
P79802         MOVE BN232WS-AMOUNT-YTD-4   TO G9-COD-AMOUNT-YTD-4
P79802         MOVE BN232WS-AMOUNT-YTD-5   TO G9-COD-AMOUNT-YTD-5
P79802         MOVE BN232WS-AMOUNT-YTD-6   TO G9-COD-AMOUNT-YTD-6
233800     ELSE
233900         MOVE ZEROS                  TO G9-COD-AMOUNT-YTD-1
234000                                        G9-COD-AMOUNT-YTD-2
234100                                        G9-COD-AMOUNT-YTD-3
P79802                                        G9-COD-AMOUNT-YTD-4
P79802                                        G9-COD-AMOUNT-YTD-5
P79802                                        G9-COD-AMOUNT-YTD-6
234200                                        G9-TOTAL-CONTRIB-YR.
234300
234400     MOVE WS-PLN-YYYY                TO G9-PLN-YEAR-1
234500                                        G9-PLN-YEAR-2.
234600
234700     IF (G9-COD-AMOUNT-YTD-1 NOT = ZEROS)
234800     OR (G9-COD-AMOUNT-YTD-2 NOT = ZEROS)
234900     OR (G9-COD-AMOUNT-YTD-3 NOT = ZEROS)
235000         MOVE GN9D-1-DC-SUMM         TO RPT-GROUP-REQUEST
235100         PERFORM 700-PRINT-RPT-GRP.
235200
P79802*    IF   (CODA-FOUND)
P79802*    AND ((COD-AMOUNT-YTD (4) NOT = ZEROS)
P79802*    OR   (COD-AMOUNT-YTD (5) NOT = ZEROS)
P79802*    OR   (COD-AMOUNT-YTD (6) NOT = ZEROS))
P79802*        MOVE COD-AMOUNT-YTD (4) TO G9-COD-AMOUNT-YTD-4
P79802*        MOVE COD-AMOUNT-YTD (5) TO G9-COD-AMOUNT-YTD-5
P79802*        MOVE COD-AMOUNT-YTD (6) TO G9-COD-AMOUNT-YTD-6
P79802     IF (G9-COD-AMOUNT-YTD-4 NOT = ZEROS)
P79802     OR (G9-COD-AMOUNT-YTD-5 NOT = ZEROS)
P79802     OR (G9-COD-AMOUNT-YTD-6 NOT = ZEROS)
236000         MOVE GN9D-2-DC-SUMM     TO RPT-GROUP-REQUEST
236100         PERFORM 700-PRINT-RPT-GRP.
236200
236300     IF (G9-TOTAL-CONTRIB-YR NOT = ZEROS)
236400         MOVE GN9D-3-DC-SUMM     TO RPT-GROUP-REQUEST
236500         PERFORM 700-PRINT-RPT-GRP.
236600
L10113     MOVE ZEROES                 TO WS-TOTAL-CONTRIB
L10113                                    WS-EMP-CONT-LTD
L10113                                    WS-COMP-CONT-LTD.
236700     MOVE SF-EMPLOYEE            TO DB-EMPLOYEE.
236800     MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE.
236800     MOVE SF-PLAN-CODE           TO DB-PLAN-CODE.
236900     MOVE ZEROS                  TO DB-PLAN-YEAR.
L10113     MOVE CODSET1-PLAN-YEAR      TO WS-DB-BEG-RNG.
237000     PERFORM 850-FIND-BEGRNG-CODSET1.
237100
237200     IF  (CODA-FOUND)
L10113         PERFORM
L10113             UNTIL (CODA-NOTFOUND)
237300             MOVE COD-LAST-UPD-DATE      TO G9-COD-LAST-UPD-DATE
L10113             COMPUTE WS-TOTAL-CONTRIB    = WS-TOTAL-CONTRIB
237400                                         + COD-AMOUNT-YTD (1)
237500                                         + COD-AMOUNT-YTD (2)
237600                                         + COD-AMOUNT-YTD (3)
237700                                         + COD-AMOUNT-YTD (4)
237800                                         + COD-AMOUNT-YTD (5)
237900                                         + COD-AMOUNT-YTD (6)
L10113             COMPUTE WS-EMP-CONT-LTD     = WS-EMP-CONT-LTD
238100                                         + COD-AMOUNT-YTD (1)
238200                                         + COD-AMOUNT-YTD (2)
238300                                         + COD-AMOUNT-YTD (3)
L10113             COMPUTE WS-COMP-CONT-LTD    = WS-COMP-CONT-LTD
238400                                         + COD-AMOUNT-YTD (4)
238500                                         + COD-AMOUNT-YTD (5)
238600                                         + COD-AMOUNT-YTD (6)
L10113             PERFORM 860-FIND-NXTRNG-CODSET1
L10113         END-PERFORM
238000         MOVE WS-TOTAL-CONTRIB           TO G9-TOTAL-ACCT-BAL
238700         PERFORM 4267-CALC-VESTED-AMT
238800         THRU    4267-END
238900         MOVE GN9D-4-DC-SUMM TO RPT-GROUP-REQUEST
239000         PERFORM 700-PRINT-RPT-GRP.
239100
239200
239300     MOVE SF-EMPLOYEE            TO DB-EMPLOYEE.
239400     MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE.
239500     MOVE SF-PLAN-CODE           TO DB-PLAN-CODE.
239600     MOVE 1                      TO DB-PRIM-CNTGNT.
239700     MOVE ZEROES                 TO DB-SEQ-NBR.
239800     PERFORM 850-FIND-NLT-BNFSET2.
239900
240000     MOVE 110                TO CRT-MSG-NBR.
240100     PERFORM 790-GET-MSG.
240200     MOVE CRT-MESSAGE        TO G9-BEN-TYPE-DESC.
240300
240400     IF  (BENEFICRY-FOUND)
240500     AND (BNF-COMPANY     = DB-COMPANY)
240600     AND (BNF-EMPLOYEE    = DB-EMPLOYEE)
240700     AND (BNF-PLAN-TYPE   = DB-PLAN-TYPE)
240800     AND (BNF-PLAN-CODE   = DB-PLAN-CODE)
240900     AND (BNF-PRIM-CNTGNT = DB-PRIM-CNTGNT)
241000     AND (PLN-WAIVE-FLAG NOT = "Y")
241100         MOVE GN9D-6-DC-SUMM         TO RPT-GROUP-REQUEST
241200         PERFORM 700-PRINT-RPT-GRP.
241300
241400     PERFORM 4262-PRT-BENEFICRY    
241500     THRU    4262-END
241600         UNTIL (BENEFICRY-NOTFOUND)
241700         OR    (BNF-COMPANY      NOT = DB-COMPANY)
241800         OR    (BNF-EMPLOYEE     NOT = DB-EMPLOYEE)
241900         OR    (BNF-PLAN-TYPE    NOT = DB-PLAN-TYPE)
242000         OR    (BNF-PLAN-CODE    NOT = DB-PLAN-CODE)
242100         OR    (BNF-PRIM-CNTGNT  NOT = DB-PRIM-CNTGNT).
242200      MOVE 2                      TO DB-PRIM-CNTGNT.
242300      PERFORM 850-FIND-NLT-BNFSET2.
242400
242500     MOVE 111                TO CRT-MSG-NBR.
242600     PERFORM 790-GET-MSG.
242700     MOVE CRT-MESSAGE        TO G9-BEN-TYPE-DESC.
242800
242900     PERFORM 4262-PRT-BENEFICRY
243000     THRU    4262-END
243100         UNTIL (BENEFICRY-NOTFOUND)
243200         OR    (BNF-COMPANY      NOT = DB-COMPANY)
243300         OR    (BNF-EMPLOYEE     NOT = DB-EMPLOYEE)
243400         OR    (BNF-PLAN-TYPE    NOT = DB-PLAN-TYPE)
243500         OR    (BNF-PLAN-CODE    NOT = DB-PLAN-CODE)
243600         OR    (BNF-PRIM-CNTGNT  NOT = DB-PRIM-CNTGNT).
243700
243800 4260-CONTINUE.
243900
244000     MOVE SF-EMP-PAY-RATE        TO WS-SAVE-PAY-RATE.
244100
244200     RETURN SORT-FILE            INTO SF-SORT-REC
244300         AT END
244400             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
244500
244600 4260-END.
244700******************************************************************
244800 4262-PRT-BENEFICRY.
244900******************************************************************
245000
245100     MOVE BNF-LAST-NAME                   TO HRWS-LAST-NAME.
245200     MOVE BNF-FIRST-NAME                  TO HRWS-FIRST-NAME.
245300     MOVE BNF-MIDDLE-INIT                 TO HRWS-MIDDLE-INIT.
           INITIALIZE HRWS-LAST-NAME-PRE
                      HRWS-NAME-SUFFIX.
245400     PERFORM 750-HR-FORMAT-NAME.
245500     MOVE HRWS-FORMAT-NAME                TO G9-BNF-NAME.
245600
245700     MOVE GN9D-5-DC-BENEFICIARIES       TO RPT-GROUP-REQUEST.
245800     PERFORM 700-PRINT-RPT-GRP.
245900
246000     MOVE SPACES                          TO G9-BEN-TYPE-DESC.
246100
246200     PERFORM 860-FIND-NEXT-BNFSET2.
246300
246400 4262-END.
246500******************************************************************
246600 4265-PLAN-FOUND.
246700******************************************************************
246800
246900     MOVE PLN-PLAN-OPTION        TO G9-PLAN-OPTION.
247000
           MOVE PLN-AFT-DED-MTCH-A     TO WS-AFT-DED-MTCH-A.
           MOVE PLN-AFT-DED-MTCH-P     TO WS-AFT-DED-MTCH-P.
           MOVE PLN-AFT-LMT-MTCH-A     TO WS-AFT-LMT-MTCH-A.
           MOVE PLN-AFT-LMT-MTCH-P     TO WS-AFT-LMT-MTCH-P.
           MOVE PLN-CMP-DED-CODE-A     TO WS-CMP-DED-CODE-A.
           MOVE PLN-CMP-DED-CODE-P     TO WS-CMP-DED-CODE-P.
           MOVE PLN-PRE-DED-MTCH-A     TO WS-PRE-DED-MTCH-A.
           MOVE PLN-PRE-DED-MTCH-P     TO WS-PRE-DED-MTCH-P.
247700
247800     MOVE PLN-DESC               TO G9D-PLN-DESC.
247900     ADD 1                       TO I3.
248000
248100     IF (I3 > WS-MAX-TBL-SIZE)
248200         NEXT SENTENCE
248300     ELSE
248500         PERFORM 4600-GET-DEDUCTIONS
248600         THRU    4600-END.
248700
248800 4265-END.
248900******************************************************************
249000 4267-CALC-VESTED-AMT.
249100******************************************************************
249200
249300     MOVE PRM-COMPANY            TO BNWS-VES-COMPANY.
249400     MOVE SF-EMPLOYEE            TO BNWS-VES-EMPLOYEE.
249500     MOVE "DC"                   TO BNWS-VES-PLAN-TYPE.
249600     MOVE SF-PLAN-CODE           TO BNWS-VES-PLAN-CODE.
249700
249800     PERFORM 700-COMPUTE-VESTING.
249900
250000     COMPUTE WS-AMT-VESTED ROUNDED   =  (WS-EMP-CONT-LTD
250100                                     + ((BNWS-PCT-VESTED / 100)
250200                                     *   WS-COMP-CONT-LTD)).
250300     MOVE WS-AMT-VESTED          TO G9-AMT-VESTED.
250400     MOVE BNWS-PCT-VESTED        TO G9-PERCENT-VESTED.
250500
250600 4267-END.
250700******************************************************************
250800 4270-PRT-BEN-DEF-BENEFIT.
250900******************************************************************
251000
251100     MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE.
251200     MOVE SF-PLAN-CODE           TO DB-PLAN-CODE.
251300     PERFORM 840-FIND-PLNSET1.
251400
251500     IF (PLAN-FOUND)
251600         MOVE PLN-DESC           TO G10D-PLN-DESC
251700     ELSE
251800         MOVE 106                TO CRT-MSG-NBR
251900         PERFORM 790-GET-MSG
252000         MOVE CRT-MESSAGE        TO G10D-PLN-DESC.
252100
252200     MOVE GN10D-BEN-DEF-BENEFIT  TO RPT-GROUP-REQUEST.
252300     PERFORM 700-PRINT-RPT-GRP.
252400
252500     IF (PRM-PRINT-COMMENTS = "Y")
252600         PERFORM 4400-PRT-COMMENTS
252700         THRU    4400-END.
252800
252900     IF (PLN-WAIVE-FLAG = "Y")
253000         GO TO 4270-CONTINUE.
253100
253200     MOVE SF-EMPLOYEE            TO DB-EMPLOYEE.
253300     MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE.
253400     MOVE SF-PLAN-CODE           TO DB-PLAN-CODE.
253500     MOVE 1                      TO DB-PRIM-CNTGNT.
253600     MOVE ZEROES                 TO DB-SEQ-NBR.
253700     PERFORM 850-FIND-NLT-BNFSET2.
253800
253900     MOVE 110                    TO CRT-MSG-NBR.
254000     PERFORM 790-GET-MSG.
254100     MOVE CRT-MESSAGE             TO G10-BEN-TYPE-DESC.   
254200     IF  (BENEFICRY-FOUND)
254300     AND (BNF-COMPANY     = DB-COMPANY)
254400     AND (BNF-EMPLOYEE    = DB-EMPLOYEE)
254500     AND (BNF-PLAN-TYPE   = DB-PLAN-TYPE)
254600     AND (BNF-PLAN-CODE   = DB-PLAN-CODE)
254700     AND (BNF-PRIM-CNTGNT = DB-PRIM-CNTGNT)
254800         MOVE GN10-H-1-BENF-HEADER   TO RPT-GROUP-REQUEST
254900         PERFORM 700-PRINT-RPT-GRP.
255000
255100     PERFORM 4275-PRT-BENEFICRY
255200     THRU    4275-END
255300         UNTIL (BENEFICRY-NOTFOUND)
255400         OR    (BNF-COMPANY      NOT = DB-COMPANY)
255500         OR    (BNF-EMPLOYEE     NOT = DB-EMPLOYEE)
255600         OR    (BNF-PLAN-TYPE    NOT = DB-PLAN-TYPE)
255700         OR    (BNF-PLAN-CODE    NOT = DB-PLAN-CODE)
255800         OR    (BNF-PRIM-CNTGNT  NOT = DB-PRIM-CNTGNT).
255900
256000     MOVE 2                      TO DB-PRIM-CNTGNT.
256100     PERFORM 850-FIND-NLT-BNFSET2.
256200
256300     MOVE 111                    TO CRT-MSG-NBR.
256400     PERFORM 790-GET-MSG.
256500     MOVE CRT-MESSAGE            TO G10-BEN-TYPE-DESC.   
256600
256700     IF  (BENEFICRY-FOUND)
256800     AND (BNF-COMPANY     = DB-COMPANY)
256900     AND (BNF-EMPLOYEE    = DB-EMPLOYEE)
257000     AND (BNF-PLAN-TYPE   = DB-PLAN-TYPE)
257100     AND (BNF-PLAN-CODE   = DB-PLAN-CODE)
257200     AND (BNF-PRIM-CNTGNT = DB-PRIM-CNTGNT)
257300         MOVE GN10-H-1-BENF-HEADER   TO RPT-GROUP-REQUEST
257400         PERFORM 700-PRINT-RPT-GRP.
257500
257600     PERFORM 4275-PRT-BENEFICRY
257700     THRU    4275-END
257800         UNTIL (BENEFICRY-NOTFOUND)
257900         OR    (BNF-COMPANY      NOT = DB-COMPANY)
258000         OR    (BNF-EMPLOYEE     NOT = DB-EMPLOYEE)
258100         OR    (BNF-PLAN-TYPE    NOT = DB-PLAN-TYPE)
258200         OR    (BNF-PLAN-CODE    NOT = DB-PLAN-CODE)
258300         OR    (BNF-PRIM-CNTGNT  NOT = DB-PRIM-CNTGNT).
258400
258500 4270-CONTINUE.
258600     MOVE SF-EMP-PAY-RATE        TO WS-SAVE-PAY-RATE.
258700
258800     RETURN SORT-FILE       INTO SF-SORT-REC
258900         AT END
259000             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
259100
259200 4270-END.
259300******************************************************************
259400 4275-PRT-BENEFICRY.
259500******************************************************************
259600
259700     MOVE BNF-LAST-NAME                   TO HRWS-LAST-NAME.
259800     MOVE BNF-FIRST-NAME                  TO HRWS-FIRST-NAME.
259900     MOVE BNF-MIDDLE-INIT                 TO HRWS-MIDDLE-INIT.
           INITIALIZE HRWS-LAST-NAME-PRE
                      HRWS-NAME-SUFFIX.
260000     PERFORM 750-HR-FORMAT-NAME.
260100     MOVE HRWS-FORMAT-NAME                TO G10-BNF-NAME.
260200
260300     MOVE GN10D-BEN-DEF-BENEFIC           TO RPT-GROUP-REQUEST.
260400     PERFORM 700-PRINT-RPT-GRP.
260500
260600     MOVE SPACES                          TO G10-BEN-TYPE-DESC.
260700
260800     PERFORM 860-FIND-NEXT-BNFSET2.
260900
261000 4275-END.
261100******************************************************************
261200 4279-FIND-SALARY.     
261300******************************************************************
261400
           INITIALIZE BNASWS-ANNUAL-SALARY.
           IF  (CVR-SALARY-TYPE NOT = SPACES)
               MOVE PRM-COMPANY        TO BNASWS-COMPANY
P37826         MOVE SF-EMPLOYEE        TO BNASWS-EMPLOYEE
P37826                                    DB-EMPLOYEE
P37826         PERFORM 840-FIND-PEMSET1
P37826         PERFORM 840-FIND-EMPSET1
               MOVE CVR-SALARY-TYPE    TO BNASWS-SALARY-TYPE
               MOVE "C"                TO BNASWS-DATE-TYPE
               MOVE ZEROES             TO BNASWS-STOP-DATE
               INITIALIZE BNASWS-START-DATE
                          BNASWS-STOP-DATE
                          BNASWS-AS-OF-MMDD
                          BNASWS-AS-OF-YYYY
               PERFORM 5000-DO-ANNUAL-SALARY-71.
           MOVE BNASWS-ANNUAL-SALARY   TO WS-EMP-PAY-RATE.

266800 4279-END.
266900
267000******************************************************************
267100 4280-PRT-BEN-VACATION.
267200******************************************************************
267300
267400     MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE.
267500     MOVE SF-PLAN-CODE           TO DB-PLAN-CODE.
267600     PERFORM 840-FIND-PLNSET1.
267700
267800     IF  (PLAN-FOUND)
267900         IF (PLN-PLAN-OPTION = 14)
268000             MOVE 120                    TO CRT-MSG-NBR
268100             PERFORM 790-GET-MSG
268200             MOVE CRT-MESSAGE            TO G11-PURCH-SOLD
268300             MOVE PLN-DESC               TO G11D-PLN-DESC
268400         ELSE
268500         IF (PLN-PLAN-OPTION = 15)
268600             MOVE 121                    TO CRT-MSG-NBR
268700             PERFORM 790-GET-MSG
268800             MOVE CRT-MESSAGE            TO G11-PURCH-SOLD
268900             MOVE PLN-DESC               TO G11D-PLN-DESC
269000     ELSE
269100         MOVE 106                    TO CRT-MSG-NBR
269200         PERFORM 790-GET-MSG
269300         MOVE CRT-MESSAGE            TO G11D-PLN-DESC
269400         MOVE SPACES                 TO G11-PURCH-SOLD.
269500
269600     MOVE GN11D-BEN-VACATION         TO RPT-GROUP-REQUEST.
269700     PERFORM 700-PRINT-RPT-GRP.
269800
269900     IF (PRM-PRINT-COMMENTS = "Y")
270000         PERFORM 4400-PRT-COMMENTS
270100         THRU    4400-END.
270200
270300     IF (SF-NBR-HOURS NOT = ZEROES)
270400         MOVE SF-NBR-HOURS            TO G11-BEN-NBR-HOURS
270500         MOVE GN11D-VA-SUMM           TO RPT-GROUP-REQUEST
270600         PERFORM 700-PRINT-RPT-GRP.
270700
270800     IF (PLN-WAIVE-FLAG NOT = "Y")
270900         PERFORM 4375-PRT-BEN-COVERAGE
271000         THRU    4375-END.
271100
271200     MOVE SF-EMP-PAY-RATE        TO WS-SAVE-PAY-RATE.
271300
271400     RETURN SORT-FILE            INTO SF-SORT-REC
271500         AT END
271600             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
271700
271800 4280-END.
271900******************************************************************
272000 4290-PRT-BEN-RESERVE-ACCT.
272100******************************************************************
272200
272300     MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE.
272400     MOVE SF-PLAN-CODE           TO DB-PLAN-CODE.
272500     PERFORM 840-FIND-PLNSET1.
272600
272700     IF (PLAN-FOUND)
272800         MOVE PLN-DESC               TO G12D-PLN-DESC
272900     ELSE
273000         MOVE 106                    TO CRT-MSG-NBR
273100         PERFORM 790-GET-MSG
273200         MOVE CRT-MESSAGE            TO G12D-PLN-DESC. 
273300
273400     MOVE GN12D-BEN-RESERVE-ACCT     TO RPT-GROUP-REQUEST.
273500     PERFORM 700-PRINT-RPT-GRP.
273600
273700     IF (PRM-PRINT-COMMENTS = "Y")
273800         PERFORM 4400-PRT-COMMENTS
273900         THRU    4400-END.
274000
274100     MOVE SF-YTD-CONT            TO G12-BEN-RA-YTD-CONT.
274200     MOVE ZEROS                  TO WS-RTR-TRANS-AMT.
274300
274400     MOVE SF-START-DATE          TO DB-START-DATE.
274500     MOVE SF-EMPLOYEE            TO DB-EMPLOYEE.
274600     MOVE SF-PLAN-CODE           TO DB-PLAN-CODE.
274700     MOVE ZEROS                  TO DB-DATE
274800                                    DB-CHECK-NBR.
274900     PERFORM 850-FIND-NLT-RTRSET1.
275000
275100     PERFORM 4295-TOTAL-TRANS-AMT
275200     THRU    4295-END
275300         UNTIL (RESTRANS-NOTFOUND)
275400         OR    (RTR-COMPANY    NOT = DB-COMPANY)
275500         OR    (RTR-START-DATE NOT = DB-START-DATE)
275600         OR    (RTR-EMPLOYEE   NOT = DB-EMPLOYEE)
275700         OR    (RTR-PLAN-CODE  NOT = DB-PLAN-CODE).
275800
275900     MOVE WS-RTR-TRANS-AMT       TO G12-RTR-TRANS-AMT.
276000
276100     IF (PLN-WAIVE-FLAG NOT = "Y")
276200         MOVE GN12D-RS-SUMM      TO RPT-GROUP-REQUEST
276300         PERFORM 700-PRINT-RPT-GRP.
276400
276500     MOVE SF-EMP-PAY-RATE        TO WS-SAVE-PAY-RATE.
276600
276700     RETURN SORT-FILE            INTO SF-SORT-REC
276800         AT END
276900             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
277000
277100 4290-END.
277200******************************************************************
277300 4295-TOTAL-TRANS-AMT.
277400******************************************************************
277500
277600     COMPUTE WS-RTR-TRANS-AMT    = WS-RTR-TRANS-AMT
277700                                 + RTR-TRANS-AMT.
277800
277900     PERFORM 860-FIND-NEXT-RTRSET1.
278000
278100 4295-END.
278200******************************************************************
278300 4300-PRT-BEN-SAVINGS-BOND.
278400******************************************************************
278500
278600     MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE.
278700     MOVE SF-PLAN-CODE           TO DB-PLAN-CODE.
278800     PERFORM 840-FIND-PLNSET1.
278900
279000     IF (PLAN-FOUND)
279100         MOVE PLN-DESC               TO G13D-PLN-DESC
279200         MOVE PLN-BOND-COST          TO G13-PLN-BOND-COST
279300         MOVE PLN-BOND-VALUE         TO G13-PLN-BOND-VALUE
279400     ELSE
279500         MOVE ZEROS                  TO G13-PLN-BOND-COST
279600                                        G13-PLN-BOND-VALUE
279700         MOVE 106                    TO CRT-MSG-NBR
279800         PERFORM 790-GET-MSG
279900         MOVE CRT-MESSAGE            TO G13D-PLN-DESC.
280000
280100     MOVE GN13D-BEN-SAVINGS-BOND     TO RPT-GROUP-REQUEST.
280200     PERFORM 700-PRINT-RPT-GRP.
280300
280400     IF (PRM-PRINT-COMMENTS = "Y")
280500         PERFORM 4400-PRT-COMMENTS
280600         THRU    4400-END.
280700
280800     IF (PLN-WAIVE-FLAG NOT = "Y")
280900         MOVE GN13D-SB-SUMM      TO RPT-GROUP-REQUEST
281000         PERFORM 700-PRINT-RPT-GRP.
281100
281200     MOVE SF-EMP-PAY-RATE        TO WS-SAVE-PAY-RATE.
281300
281400     RETURN SORT-FILE            INTO SF-SORT-REC
281500         AT END
281600             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
281700
281800 4300-END.
281900******************************************************************
282000 4310-PRT-BEN-STOCK-PURCH.
282100******************************************************************
282200
282300     MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE.
282400     MOVE SF-PLAN-CODE           TO DB-PLAN-CODE.
282500     PERFORM 840-FIND-PLNSET1.
282600
282700     IF (PLAN-FOUND)
282800         MOVE PLN-DESC               TO G14D-PLN-DESC
282900     ELSE
283000         MOVE 106                    TO CRT-MSG-NBR
283100         PERFORM 790-GET-MSG
283200         MOVE CRT-MESSAGE            TO G14D-PLN-DESC.
283300
283400     MOVE GN14D-BEN-STOCK-PURCH      TO RPT-GROUP-REQUEST.
283500     PERFORM 700-PRINT-RPT-GRP.
283600
283700     IF (PRM-PRINT-COMMENTS = "Y")
283800         PERFORM 4400-PRT-COMMENTS
283900         THRU    4400-END.
284000
284100     IF (PLN-WAIVE-FLAG = "Y")
284200         GO TO 4310-CONTINUE.
284300
284400     MOVE SF-EMPLOYEE            TO DB-EMPLOYEE.
284500     MOVE SF-PLAN-CODE           TO DB-PLAN-CODE.
284600     MOVE SF-START-DATE          TO DB-START-DATE.
284700     MOVE ZEROS                  TO DB-DATE-PURCH.
284800     PERFORM 850-FIND-NLT-SPHSET1.
284900
285000     MOVE ZEROS                  TO WS-TOT-NBR-SHARES.
285100     IF  (SPHIST-FOUND)
285200     AND (SPH-COMPANY    = DB-COMPANY)
285300     AND (SPH-EMPLOYEE   = DB-EMPLOYEE)
285400     AND (SPH-PLAN-CODE  = DB-PLAN-CODE)
285500     AND (SPH-START-DATE = DB-START-DATE)
285600         MOVE GN14D-1-SP-SUMM        TO RPT-GROUP-REQUEST
285700         PERFORM 700-PRINT-RPT-GRP.
285800
285900     PERFORM 4315-PRT-SPHIST-DTL
286000     THRU    4315-END
286100         UNTIL (SPHIST-NOTFOUND)
286200         OR    (SPH-COMPANY    NOT = DB-COMPANY)
286300         OR    (SPH-EMPLOYEE   NOT = DB-EMPLOYEE)
286400         OR    (SPH-PLAN-CODE  NOT = DB-PLAN-CODE)
286500         OR    (SPH-START-DATE NOT = DB-START-DATE).
286600
286700     MOVE SF-YTD-CONT            TO G14-BEN-SP-YTD-CONT.
286800     MOVE WS-TOT-NBR-SHARES      TO G14-TOT-NBR-SHARES.
286900     MOVE GN14D-3-SP-SUMM        TO RPT-GROUP-REQUEST.
287000     PERFORM 700-PRINT-RPT-GRP.
287100
287200 4310-CONTINUE.
287300
287400     MOVE SF-EMP-PAY-RATE        TO WS-SAVE-PAY-RATE.
287500     RETURN SORT-FILE            INTO SF-SORT-REC
287600         AT END
287700             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
287800
287900 4310-END.
288000******************************************************************
288100 4315-PRT-SPHIST-DTL.
288200******************************************************************
288300
288400     MOVE SPH-DATE-PURCH         TO G14-SPH-DATE-PURCH.
288500     MOVE SPH-NBR-SHARES         TO G14-SPH-NBR-SHARES.
288600     MOVE SPH-SHARE-PRICE        TO G14-SPH-SHARE-PRICE.
288700
288800     MOVE GN14D-2-SP-SUMM        TO RPT-GROUP-REQUEST.
288900     PERFORM 700-PRINT-RPT-GRP.
289000
289100     COMPUTE WS-TOT-NBR-SHARES   = WS-TOT-NBR-SHARES
289200                                 + SPH-NBR-SHARES.
289300
289400     PERFORM 860-FIND-NEXT-SPHSET1.
289500
289600 4315-END.
289700
289800******************************************************************
289900 4375-PRT-BEN-COVERAGE.
290000******************************************************************
290100
290200     IF (PLN-COVERAGE-TYPE = "1 ")
290300         MOVE SF-COV-OPTION          TO DB-COVERAGE-OPT
290400         PERFORM 850-FIND-NLT-COPSET1
290500         IF  (BNCOVOPT-FOUND)
290600         AND (COP-COMPANY   = DB-COMPANY)
290700         AND (COP-PLAN-TYPE = DB-PLAN-TYPE)
290800         AND (COP-PLAN-CODE = DB-PLAN-CODE)
290900             MOVE COP-COV-DESC           TO G4-COP-COV-DESC
291000             MOVE GN4D-HL-SUMM           TO RPT-GROUP-REQUEST
291100             PERFORM 700-PRINT-RPT-GRP
291200         END-IF
291300     ELSE
291400     IF  (PLN-COVERAGE-TYPE = "2 ")
291500         MOVE "E"                        TO DB-COVER-TYPE
291600         MOVE SF-COV-UPD-DT              TO DB-START-DATE
291700         MOVE SF-GROUP-NAME              TO DB-GROUP-NAME
291800         PERFORM 850-FIND-NLT-CVRSET1
291900         IF  (BNCOVERAGE-FOUND)
292000         AND (CVR-COMPANY    = DB-COMPANY)
292100         AND (CVR-PLAN-TYPE  = DB-PLAN-TYPE)
292200         AND (CVR-PLAN-CODE  = DB-PLAN-CODE)
292300         AND (CVR-COVER-TYPE = DB-COVER-TYPE)
292400         AND (CVR-START-DATE = DB-START-DATE)
292500         AND (CVR-GROUP-NAME = DB-GROUP-NAME)
292600             IF (CVR-CALC-TYPE = "P")
292700                 PERFORM 4279-FIND-SALARY
292800                 THRU    4279-END
292900                 MOVE WS-EMP-PAY-RATE    TO G6-BEN-PAY-RATE
293000                 COMPUTE WS-COVER-AMT = SF-COVER-AMT / 12
293100                 MOVE WS-COVER-AMT       TO G6-BEN-COVER-AMT
293200                 MOVE GN6D-1-DI-SUMM     TO RPT-GROUP-REQUEST
293300                 PERFORM 700-PRINT-RPT-GRP
293400             ELSE
293500             IF   (CVR-CALC-TYPE = "F")
293600             AND ((CVR-FLAT-AMT-1 NOT = ZEROES)
293700             AND  (CVR-FLAT-AMT-2 NOT = ZEROES))
293800                 MOVE CVR-FLAT-AMT-1      TO G8-SPOUSE-CVG
293900                 MOVE GN8D-DL-SUMM-S      TO RPT-GROUP-REQUEST
294000                 PERFORM 700-PRINT-RPT-GRP
294100                 MOVE CVR-FLAT-AMT-2      TO G8-DEPEND-CVG
294200                 MOVE GN8D-DL-SUMM-D      TO RPT-GROUP-REQUEST
294300                 PERFORM 700-PRINT-RPT-GRP
294400             ELSE
294500             IF  (CVR-CALC-TYPE = "F" OR "S")
J07540* (E)MPLOYEE, (B)OTH SPOUSE AND DEPS, (C) PARTNER AND DEPS,
J07540* SPOUSE OR PARTNER (A)ND DEPS.
J07540                 IF (CVR-LIFE-ADD-FLG = "E" OR "B" OR "C" OR "A")
                           IF (CVR-FLAT-AMT-1 NOT = ZEROES)
                               MOVE CVR-FLAT-AMT-1  TO G7-BEN-COVER-AMT
                           ELSE
                               MOVE SF-COVER-AMT    TO G7-BEN-COVER-AMT
                           END-IF
                           MOVE GN7D-1-EL-SUMM      TO RPT-GROUP-REQUEST
                           PERFORM 700-PRINT-RPT-GRP
                       ELSE
J07540* (S)POUSE, (P)ARTNER, SPOUSE (O)R PARTNER
J07540                 IF (CVR-LIFE-ADD-FLG = "S" OR "P" OR "O")
                           IF (CVR-FLAT-AMT-1 NOT = ZEROES)
                               MOVE CVR-FLAT-AMT-1  TO G8-SPOUSE-CVG
                           ELSE
                               MOVE SF-COVER-AMT    TO G8-SPOUSE-CVG
                           END-IF
                           MOVE GN8D-DL-SUMM-S      TO RPT-GROUP-REQUEST
                           PERFORM 700-PRINT-RPT-GRP
                       ELSE
J07540* (D)EPENDENTS OR PA(R)TNER DEPS
J07540                 IF (CVR-LIFE-ADD-FLG = "D" OR "R")
                           IF (CVR-FLAT-AMT-1 NOT = ZEROES)
                               MOVE CVR-FLAT-AMT-1  TO G8-DEPEND-CVG
                           ELSE
                               MOVE SF-COVER-AMT    TO G8-DEPEND-CVG
                           END-IF
                           MOVE GN8D-DL-SUMM-D      TO RPT-GROUP-REQUEST
                           PERFORM 700-PRINT-RPT-GRP
                       END-IF
                       END-IF
                       END-IF
294900             ELSE
P60050             IF  (CVR-CALC-TYPE = "M" OR "N")
295100                 MOVE SF-COVER-AMT        TO G7M-BEN-COVER-AMT
P95537*                IF (SF-MULTIPLE = SPACES)
P95537                 IF (SF-MULTIPLE = ZEROES)
295300                     MOVE CVR-DEFAULT-MULT 
295400                                          TO G7M-BEN-MULTIPLE
295500                 ELSE
295600                     MOVE SF-MULTIPLE     TO G7M-BEN-MULTIPLE
295700                 END-IF
295800                 MOVE GN7D-M-EL-SUMM      TO RPT-GROUP-REQUEST
295900                 PERFORM 700-PRINT-RPT-GRP.
296000 4375-END.
296100
296200******************************************************************
296300 4400-PRT-COMMENTS.
296400******************************************************************
296500
296600     MOVE "PL"                   TO DB-CMT-TYPE.
296700     MOVE ZEROS                  TO DB-EMPLOYEE
296800                                    DB-PARTICIPNT.
296900     MOVE SF-PLAN-TYPE           TO DB-PLAN-TYPE.
297000     MOVE SF-PLAN-CODE           TO DB-PLAN-CODE.
297100     MOVE SPACES                 TO DB-GROUP-NAME.
297200     MOVE ZEROS                  TO DB-START-DATE
297300                                    DB-SEQ-NBR.
297400
297500     PERFORM 850-FIND-NLT-BCMSET1.
297600     IF  (BNCOMMENTS-FOUND)
297700     AND (BCM-COMPANY     = DB-COMPANY)
297800     AND (BCM-CMT-TYPE    = DB-CMT-TYPE)
297900     AND (BCM-EMPLOYEE    = DB-EMPLOYEE)
298000     AND (BCM-PARTICIPNT  = DB-PARTICIPNT)
298100     AND (BCM-PLAN-TYPE   = DB-PLAN-TYPE)
298200     AND (BCM-PLAN-CODE   = DB-PLAN-CODE)
298300     AND (BCM-GROUP-NAME  = DB-GROUP-NAME)
298400     AND (BCM-START-DATE  = DB-START-DATE)
298500         MOVE SPACES                 TO G20-BCM-CMT-TEXT
298600         MOVE GN20-COMMENTS-LINE     TO RPT-GROUP-REQUEST
298700         PERFORM 700-PRINT-RPT-GRP.
298800     PERFORM 4410-PRT-COMMENT-LINE
298900     THRU    4410-END
299000         UNTIL (BNCOMMENTS-NOTFOUND)
299100         OR    (BCM-COMPANY    NOT = DB-COMPANY)
299200         OR    (BCM-CMT-TYPE   NOT = DB-CMT-TYPE)
299300         OR    (BCM-EMPLOYEE   NOT = DB-EMPLOYEE)
299400         OR    (BCM-PARTICIPNT NOT = DB-PARTICIPNT)
299500         OR    (BCM-PLAN-TYPE  NOT = DB-PLAN-TYPE)
299600         OR    (BCM-PLAN-CODE  NOT = DB-PLAN-CODE)
299700         OR    (BCM-GROUP-NAME NOT = DB-GROUP-NAME)
299800         OR    (BCM-START-DATE NOT = DB-START-DATE).
299900
300000 4400-END.
300100******************************************************************
300200 4410-PRT-COMMENT-LINE.
300300******************************************************************
300400
300500     MOVE BCM-CMT-TEXT           TO G20-BCM-CMT-TEXT.
300600
300700     MOVE GN20-COMMENTS-LINE     TO RPT-GROUP-REQUEST.
300800     PERFORM 700-PRINT-RPT-GRP.
300900
301000     PERFORM 860-FIND-NEXT-BCMSET1.
301100
301200 4410-END.
301300******************************************************************
301400 4500-PRT-SUMMARY-STMT.
301500******************************************************************
301600
301700     MOVE WS-TBL-PLN-TYPE (I2)       TO G15-BEN-PLAN-TYPE.
301800     MOVE WS-TBL-PLN-DESC (I2)       TO G15-PLN-DESC.
301900     MOVE WS-TBL-EDM-AMOUNT-YTD (I2) TO G15-EDM-AMOUNT-YTD.
302000
302100     IF (I2 NOT = 1)
302200         COMPUTE I4 = I2 - 1
302300         IF (WS-TBL-PLN-TYPE (I2)    = WS-TBL-PLN-TYPE (I4))
302400             MOVE SPACES             TO G15-BEN-PLAN-TYPE.
302500
302600     MOVE GN15D-1-SUMMARY-STMT       TO RPT-GROUP-REQUEST.
302700     PERFORM 700-PRINT-RPT-GRP.
302800
302900     COMPUTE WS-TOT-EDM-AMOUNT-YTD   = WS-TOT-EDM-AMOUNT-YTD
303000                                     + WS-TBL-EDM-AMOUNT-YTD (I2).
303100
303200 4500-END.
303300******************************************************************
303400 4600-GET-DEDUCTIONS.
303500******************************************************************
303600
303700     ADD 1                       TO WS-CPNY-PD-BENEFITS.
303800     MOVE PLN-DESC               TO WS-TBL-PLN-DESC (I3).
303900     MOVE PLN-PLAN-TYPE          TO WS-TBL-PLN-TYPE (I3).
304000
304100     MOVE SF-EMPLOYEE            TO DB-EMPLOYEE.
304200     MOVE WS-FROM-YY             TO DB-PAYROLL-YEAR.
304300     MOVE WS-QUARTER             TO DB-QUARTER.
304400     INITIALIZE DB-DED-CODE.
304500     INITIALIZE DB-PROCESS-LEVEL.
304600     INITIALIZE DB-WORK-STATE.
304700
304800     PERFORM 850-FIND-NLT-QTDSET1.
304900     PERFORM 4610-QTD-RECORDS
305000     THRU    4610-END
305100         UNTIL (QUARTDED-NOTFOUND)
305200         OR    (QTD-COMPANY       NOT = DB-COMPANY)
305300         OR    (QTD-EMPLOYEE      NOT = DB-EMPLOYEE)
305400         OR    (QTD-PAYROLL-YEAR      > WS-TO-YY)
305500         OR    ((QTD-PAYROLL-YEAR     = WS-TO-YY)
305600         AND    (QTD-QUARTER          > WS-TO-QUARTER)).
305700
305800     MOVE PRM-FROM-DATE          TO DB-CHECK-DATE.
305900     MOVE ZEROS                  TO DB-CHECK-NBR.
306000     MOVE ZEROS                  TO DB-CHECK-ID.
306100
306200     PERFORM 850-FIND-NLT-PYMSET4.
306300     PERFORM 4630-PAY-RECORDS
306400     THRU    4630-END
306500         UNTIL (PAYMASTR-NOTFOUND)
306600         OR    (PYM-COMPANY    NOT = DB-COMPANY)
306700         OR    (PYM-EMPLOYEE   NOT = DB-EMPLOYEE)
306800         OR    (PYM-CHECK-DATE > WS-BEG-OF-QUARTER).
306900
307000     MOVE WS-END-OF-QUARTER      TO DB-CHECK-DATE.
307100
307200     PERFORM 850-FIND-NLT-PYMSET4.
307300     PERFORM 4630-PAY-RECORDS
307400     THRU    4630-END
307500         UNTIL (PAYMASTR-NOTFOUND)
307600         OR    (PYM-COMPANY    NOT = DB-COMPANY)
307700         OR    (PYM-EMPLOYEE   NOT = DB-EMPLOYEE)
307800         OR    (PYM-CHECK-DATE > WS-PLAN-YEAR-TO).
307900
308000     IF (WS-TBL-EDM-AMOUNT-YTD (I3) = ZEROS)
308100         SUBTRACT 1          FROM I3
308200         SUBTRACT 1          FROM WS-CPNY-PD-BENEFITS.
308300
308400 4600-END.
308500******************************************************************
308600 4610-QTD-RECORDS.
308700******************************************************************
308800
           IF (QTD-DED-CODE            =  WS-AFT-DED-MTCH-A)
           OR (QTD-DED-CODE            =  WS-AFT-DED-MTCH-P)
           OR (QTD-DED-CODE            =  WS-AFT-LMT-MTCH-A)
           OR (QTD-DED-CODE            =  WS-AFT-LMT-MTCH-P)
           OR (QTD-DED-CODE            =  WS-CMP-DED-CODE-A)
           OR (QTD-DED-CODE            =  WS-CMP-DED-CODE-P)
           OR (QTD-DED-CODE            =  WS-PRE-DED-MTCH-A)
           OR (QTD-DED-CODE            =  WS-PRE-DED-MTCH-P)
309600         ADD QTD-DED-AMT         TO WS-TBL-EDM-AMOUNT-YTD (I3).
309700
309800 4610-NEXT-QUARTDED.
309900     PERFORM 860-FIND-NEXT-QTDSET1.
310000
310100 4610-END.
310200******************************************************************
310300 4630-PAY-RECORDS.
310400******************************************************************
310500
310600     IF (PYM-STATUS NOT = 9)
310700         GO TO 4630-FIND-NEXT-PAYMASTR.
310800
310900     MOVE PYM-CHECK-ID           TO DB-CHECK-ID.
311000     MOVE PLN-PRE-DED-MTCH-A     TO DB-DED-CODE.
311100     MOVE PYM-PROCESS-LEVEL      TO DB-PROCESS-LEVEL.
311200     MOVE SPACES                 TO DB-WORK-STATE.
311300
311400     PERFORM 850-FIND-NLT-PYDSET1.
311500     IF  (PAYDEDUCTN-NOTFOUND)
311600     OR  (PYD-COMPANY       NOT = DB-COMPANY)
311700     OR  (PYD-EMPLOYEE      NOT = DB-EMPLOYEE)
311800     OR  (PYD-CHECK-ID      NOT = DB-CHECK-ID)
311900     OR  (PYD-DED-CODE      NOT = DB-DED-CODE)
312000     OR  (PYD-PROCESS-LEVEL NOT = DB-PROCESS-LEVEL)
312100         GO TO 4630-FIND-NEXT-PAYMASTR.
312200         
312300     PERFORM 4640-GET-PYD
312400     THRU    4640-END     
312500         UNTIL  (PAYDEDUCTN-NOTFOUND)
312600         OR     (PYD-COMPANY       NOT = DB-COMPANY)
312700         OR     (PYD-EMPLOYEE      NOT = DB-EMPLOYEE)
312800         OR     (PYD-CHECK-ID      NOT = DB-CHECK-ID)
312900         OR     (PYD-DED-CODE      NOT = DB-DED-CODE)
313000         OR     (PYD-PROCESS-LEVEL NOT = DB-PROCESS-LEVEL).
313100
313200 4630-FIND-NEXT-PAYMASTR.
313300     PERFORM 860-FIND-NEXT-PYMSET4.
313400 4630-END.
313500******************************************************************
313600 4640-GET-PYD. 
313700******************************************************************
313800
313900     ADD PYD-DED-AMT         TO WS-TBL-EDM-AMOUNT-YTD (I3).
314000
314100 4640-FIND-NEXT-PAYDEDUCTN.
314200     PERFORM 860-FIND-NEXT-PYDSET1.
314300
314400 4640-END.
314500******************************************************************
314600 2000-END.
314700******************************************************************
314800
