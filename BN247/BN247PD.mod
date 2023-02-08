******* BN247PD 12 <3735456357>
      ******************************************************************
      * BN247 - BENEFICIARY LISTING
      ******************************************************************
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  412175 | J12175 | ADD ADDITIONAL SELECTION CRITERIA & AUDIT   *
      *  ------   ------   ------------------------------------------  *
      *  848358 | J48358 | Fixed the issue of report selection 1 does  *
      *                    not display all records                     *
      *  ------   ------   ------------------------------------------  *
      * 1061529 | J61529 | USING INCORRECT USER ID                     *
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
001600         MOVE PRS-COMPANY        TO R1G1-BNF-COMPANY
001700         MOVE PRS-NAME           TO R1G1-PRS-NAME.
001800
J12175     IF (ERROR-FOUND)
J12175         GO TO 050-END.
002500
002600     IF (PRM-EMPLOYEE-SEQ        = SPACES)
002700         MOVE PRS-EMPLOYEE-SEQ    TO PRM-EMPLOYEE-SEQ.
002800
J12175* NO EMPLOYEE SPECIFIED
J12175     IF  (PRM-RUN-OPTION = 1)
J12175     AND (PRM-EMPLOYEE (1) = ZEROS)
J12175     AND (PRM-EMPLOYEE (2) = ZEROS)
J12175     AND (PRM-EMPLOYEE (3) = ZEROS)
J12175     AND (PRM-EMPLOYEE (4) = ZEROS)
J12175     AND (PRM-EMPLOYEE (5) = ZEROS)
J12175         MOVE 117            TO CRT-ERROR-NBR
J12175         PERFORM 780-PRINT-ERROR-MSG 
J12175         GO TO 050-END
J12175     END-IF.

J12175     IF  (PRM-EMPLOYEE (1) NOT = ZEROS)
J12175     OR  (PRM-EMPLOYEE (2) NOT = ZEROS)
J12175     OR  (PRM-EMPLOYEE (3) NOT = ZEROS)
J12175     OR  (PRM-EMPLOYEE (4) NOT = ZEROS)
J12175     OR  (PRM-EMPLOYEE (5) NOT = ZEROS) 
J12175         IF (PRM-RUN-OPTION = 1)
J12175             PERFORM 055-EDIT-EMPLOYEES
                   THRU    055-END
J12175                 VARYING I1 FROM 1 BY 1
J12175                 UNTIL   (I1 > 5) 
J12175             IF (ERROR-FOUND)
J12175                 GO TO 050-END
J12175             END-IF
J12175         ELSE
J12175* EMPLOYEE ONLY VALID WITH REPORT OPTION 1
J12175             MOVE 119        TO CRT-ERROR-NBR 
J12175             PERFORM 780-PRINT-ERROR-MSG
J12175             GO TO 050-END
J12175         END-IF
J12175     END-IF.
J12175
J12175* SELECT AN EMPLOYEE GROUP FOR OPTION 3
J12175     IF  (PRM-RUN-OPTION = 3)
J12175     AND (PRM-GROUP-NAME                 = SPACES)
J12175         MOVE 121                        TO CRT-ERROR-NBR
J12175         PERFORM 780-PRINT-ERROR-MSG
J12175         GO TO 050-END
J12175     END-IF.
J12175
J12175     IF (PRM-GROUP-NAME              NOT = SPACES)
J12175         IF (PRM-RUN-OPTION = 3)
J12175             MOVE PRM-GROUP-NAME         TO DB-GROUP-NAME
J12175             PERFORM 840-KFIND-PRGSET1
J12175             IF (PERSGROUP-KNOTFOUND)
J12175* EMPLOYEE GROUP DOES NOT EXIST
J12175                 MOVE PRM-GROUP-NAME     TO CRT-ERR-VAR1
J12175                 MOVE 122                TO CRT-ERROR-NBR
J12175                 PERFORM 780-PRINT-ERROR-MSG
J12175                 GO TO 050-END 
J12175             END-IF
J12175         ELSE
J12175* EMPLOYEE GROUP ONLY VALID WITH REPORT OPTION 3
J12175             MOVE 123                TO CRT-ERROR-NBR
J12175             PERFORM 780-PRINT-ERROR-MSG
J12175             GO TO 050-END
J12175         END-IF
J12175     END-IF.

J32626*
J32626**** SHIFT LEFT & CHECK FOR DUP PLAN TYPES
J32626*
J32626     PERFORM 052-EDIT-PLAN-TYPE
J32626     THRU    052-END.
J32626     IF (NO-ERROR-FOUND)
J32626*
J32626******** SHIFT LEFT & CHECK FOR DUP PLAN CODES
J32626*
J32626         PERFORM 054-EDIT-PLAN-CODE
J32626         THRU    054-END.
J32626
J32626     GO TO 050-END.
J32626
J32626******************************************************************
J32626 052-EDIT-PLAN-TYPE.
J32626******************************************************************
J32626
J32626*
J32626**** SHIFT LEFT ALL PLAN TYPES
J32626*
J32626     PERFORM
J32626         VARYING I1 FROM 1 BY 1
J32626         UNTIL  (I1 > 8)
J32626         OR     (I2 > 8)
J32626
J32626         PERFORM
J32626             VARYING I1 FROM I1 BY 1
J32626             UNTIL  (I1 > 8)
J32626             OR     (PRM-PLAN-TYPE (I1)  = SPACES)
J32626
J32626             CONTINUE
J32626         END-PERFORM
J32626
J32626         COMPUTE I2                      = I1
J32626                                         + 1
J32626         PERFORM
J32626             VARYING I2 FROM I1 BY 1
J32626             UNTIL  (I2 > 8)
J32626             OR     (PRM-PLAN-TYPE (I2)  NOT = SPACES)
J32626
J32626             CONTINUE
J32626         END-PERFORM
J32626         IF (I2                          <= 8)
J32626             IF (PRM-PLAN-TYPE (I2)      NOT = SPACES)
J32626                 MOVE PRM-PLAN-TYPE (I2) TO PRM-PLAN-TYPE (I1)
J32626                 INITIALIZE PRM-PLAN-TYPE (I2)
J32626             END-IF
J32626         END-IF
J32626     END-PERFORM.
J32626
J32626*
J32626**** CHECK FOR DUPS
J32626*
J32626     PERFORM
J32626         VARYING I1 FROM 1 BY 1
J32626         UNTIL  (I1 > 8)
J32626         OR     (PRM-PLAN-TYPE (I1)      = SPACES)
J32626         OR     (ERROR-FOUND)
J32626
J32626         COMPUTE I2                      = I1
J32626                                         + 1
J32626         PERFORM
J32626             VARYING I2 FROM I2 BY 1
J32626             UNTIL  (I2 > 8)
J32626             OR     (PRM-PLAN-TYPE (I2)  = PRM-PLAN-TYPE (I1))
J32626
J32626             CONTINUE
J32626         END-PERFORM
J32626         IF (I2                          <= 8)
J32626             IF (PRM-PLAN-TYPE (I2)      = PRM-PLAN-TYPE (I1))
J32626**************** DUPLICATE PLAN TYPE
J32626                 MOVE 115                TO CRT-ERROR-NBR
J32626                 PERFORM 780-PRINT-ERROR-MSG
J32626             END-IF
J32626         END-IF
J32626     END-PERFORM.
J32626
J32626     IF (I1                               = 1)
J32626******** MUST ENTER ATLEAST ONE PLAN TYPE
J32626         MOVE 101                        TO CRT-ERROR-NBR
J32626         PERFORM 780-PRINT-ERROR-MSG
J32626     ELSE
J32626         COMPUTE WS-LAST-PLAN-TYPE        = I1
J32626                                          - 1
J32626     END-IF.
J32626
J32626 052-END.
J32626
J32626******************************************************************
J32626 054-EDIT-PLAN-CODE.
J32626******************************************************************
J32626
J32626*
J32626**** SHIFT LEFT ALL PLAN CODES
J32626*
J32626     INITIALIZE I2.
J32626
J32626     PERFORM
J32626         VARYING I1 FROM 1 BY 1
J32626         UNTIL  (I1 > 8)
J32626         OR     (I2 > 8)
J32626
J32626         PERFORM
J32626             VARYING I1 FROM I1 BY 1
J32626             UNTIL  (I1 > 8)
J32626             OR     (PRM-PLAN-CODE (I1)  = SPACES)
J32626
J32626             CONTINUE
J32626         END-PERFORM
J32626
J32626         COMPUTE I2                      = I1
J32626                                         + 1
J32626         PERFORM
J32626             VARYING I2 FROM I1 BY 1
J32626             UNTIL  (I2 > 8)
J32626             OR     (PRM-PLAN-CODE (I2)  NOT = SPACES)
J32626
J32626             CONTINUE
J32626         END-PERFORM
J32626         IF (I2                          <= 8)
J32626             IF (PRM-PLAN-CODE (I2)      NOT = SPACES)
J32626                 MOVE PRM-PLAN-CODE (I2) TO PRM-PLAN-CODE (I1)
J32626                 INITIALIZE PRM-PLAN-CODE (I2)
J32626             END-IF
J32626         END-IF
J32626     END-PERFORM.
J32626
J32626*
J32626**** CHECK FOR DUPS
J32626*
J32626     PERFORM
J32626         VARYING I1 FROM 1 BY 1
J32626         UNTIL  (I1 > 8)
J32626         OR     (PRM-PLAN-CODE (I1)      = SPACES)
J32626         OR     (ERROR-FOUND)
J32626
J32626         COMPUTE I2                      = I1
J32626                                         + 1
J32626         PERFORM
J32626             VARYING I2 FROM I2 BY 1
J32626             UNTIL  (I2 > 8)
J32626             OR     (PRM-PLAN-CODE (I2)  = PRM-PLAN-CODE (I1))
J32626
J32626             CONTINUE
J32626         END-PERFORM
J32626         IF (I2                          <= 8)
J32626             IF (PRM-PLAN-CODE (I2)      = PRM-PLAN-CODE (I1))
J32626**************** DUPLICATE PLAN CODE
J32626                 MOVE 116                TO CRT-ERROR-NBR
J32626                 PERFORM 780-PRINT-ERROR-MSG
J32626             END-IF
J32626         END-IF
J32626     END-PERFORM.
J32626
J32626     IF (ERROR-FOUND)
J32626         GO TO 054-END.
J32626
J32626     COMPUTE WS-LAST-PLAN-CODE            = I1
J32626                                          - 1.
J32626
J32626     IF (WS-LAST-PLAN-CODE                = ZEROES)
J32626         GO TO 054-END.
J32626
J32626     INITIALIZE I9.
J32626
J32626     MOVE PRM-COMPANY                    TO DB-COMPANY.
J32626
J32626     PERFORM
J32626         VARYING I1 FROM 1 BY 1
J32626         UNTIL  (I1 > WS-LAST-PLAN-CODE)
J32626
J32626         SET WS-PLAN-NOTFOUND            TO TRUE
J32626
J32626         PERFORM
J32626             VARYING I2 FROM 1 BY 1
J32626             UNTIL  (I2 > WS-LAST-PLAN-TYPE)
J32626
J32626             MOVE PRM-PLAN-TYPE (I2)     TO DB-PLAN-TYPE
J32626             MOVE PRM-PLAN-CODE (I1)     TO DB-PLAN-CODE
J32626             PERFORM 840-FIND-PLNSET1
J32626             IF (PLAN-FOUND)
J32626                 ADD 1                   TO I9
J32626                 SET WS-PLAN-FOUND       TO TRUE
J32626                 MOVE PRM-PLAN-TYPE (I2) TO WS-TBL-PLAN-TYPE (I9)
J32626                 MOVE PRM-PLAN-CODE (I1) TO WS-TBL-PLAN-CODE (I9)
J32626             END-IF
J32626         END-PERFORM
J32626         IF (WS-PLAN-NOTFOUND)
J32626************ PLAN DOES NOT EXIST IN ANY OF THE PLAN TYPES    
J32626             MOVE 103                    TO CRT-ERROR-NBR
J32626             MOVE PRM-PLAN-CODE (I1)     TO CRT-ERR-VAR1
J32626             PERFORM 780-PRINT-ERROR-MSG
J32626         END-IF
J32626     END-PERFORM.
J32626
J32626     MOVE I9                             TO WS-LAST-PLAN-CODE.
J32626
J32626 054-END.
J32626
J12175******************************************************************
J12175 055-EDIT-EMPLOYEES.                
J12175******************************************************************
J12175
J12175     IF (PRM-EMPLOYEE (I1)       = ZEROS)
J12175         GO TO 055-END.
J12175
J12175* EMPLOYEE DOES NOT EXIST
J12175     MOVE PRM-EMPLOYEE (I1)      TO DB-EMPLOYEE.
J12175     PERFORM 840-FIND-EMPSET1.
J12175     IF (EMPLOYEE-NOTFOUND)
J12175         MOVE PRM-EMPLOYEE (I1)  TO CRT-ERR-VAR1
J12175         MOVE 118                TO CRT-ERROR-NBR
J12175         PERFORM 780-PRINT-ERROR-MSG
J12175     ELSE 
J12175* NOT AUTHORIZED TO ACCESS EMPLOYEE
J12175         MOVE EMP-COMPANY            TO CRT-COMPANY 
J12175         MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL 
J12175         PERFORM 700-HR-EMP-SECURITY 
J12175         IF (HRWS-EMP-SECURED)
J12175             MOVE PRM-EMPLOYEE (I1)  TO CRT-ERR-VAR1 
J12175             MOVE 120                TO CRT-ERROR-NBR
J12175             PERFORM 780-PRINT-ERROR-MSG
J12175         END-IF
J12175     END-IF.
J12175
J12175 055-END.
J12175
002900 050-END.
003000******************************************************************
003100 100-PROGRAM-CONTROL             SECTION 10.
003200******************************************************************
003300 100-START.
003400
003500     MOVE 102                    TO CRT-MSG-NBR.
003600     PERFORM 780-DISPLAY-MSG.
003700
003800     PERFORM 1000-DO-REPORT-1.
003900
004000 100-END.
004100******************************************************************
004200 1000-DO-REPORT-1                SECTION 50.
004300******************************************************************
004400 1000-START.
004500
004600     IF (PRM-EMPLOYEE-SEQ = "N")
004700         SORT SORT-FILE
004800             ASCENDING KEY           DSF1-PLAN-TYPE-NBR
004900                                     DSF1-BNF-PLAN-CODE
005000                                     DSF1-BNF-EMPLOYEE
005100                                     DSF1-BNF-LAST-NAME
005200                                     DSF1-BNF-FIRST-NAME
005300                                     DSF1-BNF-MIDDLE-INIT
005400                                     DSF1-BNF-SEQ-NBR
005500                 INPUT  PROCEDURE    1000-SEL-REPORT
005600                 OUTPUT PROCEDURE    2000-PRT-REPORT
005700     ELSE
005800         SORT SORT-FILE
005900             ASCENDING KEY           DSF1-PLAN-TYPE-NBR
006000                                     DSF1-BNF-PLAN-CODE
006100                                     DSF1-EMP-LAST-NAME
006200                                     DSF1-EMP-FIRST-NAME
006300                                     DSF1-EMP-MIDDLE-INIT
006400                                     DSF1-BNF-EMPLOYEE
006500                                     DSF1-BNF-LAST-NAME
006600                                     DSF1-BNF-FIRST-NAME
006700                                     DSF1-BNF-MIDDLE-INIT
006800                                     DSF1-BNF-SEQ-NBR
006900                 INPUT  PROCEDURE    1000-SEL-REPORT
007000                 OUTPUT PROCEDURE    2000-PRT-REPORT.
007100
007200******************************************************************
007300 1000-SEL-REPORT                 SECTION 50.
007400******************************************************************
007500 1000-START.
007600
007700     MOVE PRM-COMPANY            TO DB-COMPANY.
007800     MOVE ZEROS                  TO DB-EMPLOYEE.
007900     MOVE SPACES                 TO DB-PLAN-CODE.
008000     MOVE ZEROS                  TO DB-SEQ-NBR.
008100
J12175     IF (PRM-RUN-OPTION = 1)
J12175         PERFORM 1010-LOAD-EMPLOYEES
J12175         THRU    1010-END
J12175             VARYING I2 FROM 1 BY 1
J12175             UNTIL  (I2 > 5)
J12175     END-IF.
J12175
J12175     IF (PRM-RUN-OPTION = 2)
008200         PERFORM 850-FIND-NLT-BNFSET1
008300         PERFORM 1020-FIND-EMPLOYEE
008400         THRU    1020-END
008500             UNTIL (BENEFICRY-NOTFOUND)
008600             OR    (BNF-COMPANY  NOT = DB-COMPANY)
J12175     END-IF.
008700
J12175     IF (PRM-RUN-OPTION = 3)
J12175         PERFORM 1010-LOAD-EMPLOYEES
J12175         THRU    1010-END
J12175     END-IF.
J12175
008800     GO TO 1000-SECTION-END.
J12175******************************************************************
J12175 1010-LOAD-EMPLOYEES.
J12175******************************************************************
J12175
J12175     IF (PRM-RUN-OPTION = 1)                    
J12175         MOVE PRM-COMPANY        TO DB-COMPANY
J12175         MOVE PRM-EMPLOYEE (I2)  TO DB-EMPLOYEE
J12175         PERFORM 840-FIND-EMPSET1
J12175         IF (EMPLOYEE-NOTFOUND)
J12175             GO TO 1010-END
J12175         END-IF
J12175         MOVE SPACES             TO DB-PLAN-CODE 
J12175         MOVE ZEROS              TO DB-SEQ-NBR 
J48358         MOVE SPACES             TO DB-PLAN-TYPE  
J12175         PERFORM 850-FIND-NLT-BNFSET1
J12175         PERFORM 1015-FIND-BENEFICRY
J12175         THRU    1015-END
J12175             UNTIL (BENEFICRY-NOTFOUND)
J12175             OR    (BNF-COMPANY  NOT = PRM-COMPANY)
J12175             OR    (BNF-EMPLOYEE NOT = EMP-EMPLOYEE)
J12175*        END-PERFORM 
J12175     END-IF.
J12175     IF (PRM-RUN-OPTION = 3)
J12175         MOVE PRM-COMPANY        TO DB-COMPANY
J12175         MOVE PRM-GROUP-NAME     TO DB-GROUP-NAME
J12175         MOVE PGESET1-GROUP-NAME TO WS-DB-BEG-RNG
J12175         PERFORM 850-FIND-BEGRNG-PGESET1
J12175         PERFORM 1012-FIND-EMP-GROUP
J12175         THRU    1012-END
J12175             UNTIL (PGEMPLOYEE-NOTFOUND)
J12175             OR    (PGE-COMPANY    NOT = PRM-COMPANY)
J12175             OR    (PGE-GROUP-NAME NOT = PRM-GROUP-NAME)
J12175*        END         
J12175     END-IF.
J12175                                  
J12175 1010-END.
J12175******************************************************************
J12175 1012-FIND-EMP-GROUP.
J12175******************************************************************
J12175
J12175     MOVE PRM-COMPANY        TO DB-COMPANY.
J12175     MOVE PGE-EMPLOYEE       TO DB-EMPLOYEE.
J12175     PERFORM 840-FIND-EMPSET1.
J12175     IF (EMPLOYEE-NOTFOUND)
J12175         GO TO 1012-NEXT
J12175     END-IF.
J12175
J12175     MOVE EMP-COMPANY            TO CRT-COMPANY.
J12175     MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL.
J12175     PERFORM 700-HR-EMP-SECURITY.
J12175     IF (HRWS-EMP-SECURED)
J12175         GO TO 1012-NEXT
J12175     END-IF.
J12175
J12175     MOVE SPACES             TO DB-PLAN-TYPE.
J12175     MOVE SPACES             TO DB-PLAN-CODE.
J12175     MOVE ZEROS              TO DB-SEQ-NBR.
J12175     PERFORM 850-FIND-NLT-BNFSET1.
J12175     PERFORM 1015-FIND-BENEFICRY
J12175     THRU    1015-END
J12175         UNTIL (BENEFICRY-NOTFOUND)
J12175         OR    (BNF-COMPANY  NOT = PRM-COMPANY)
J12175         OR    (BNF-EMPLOYEE NOT = EMP-EMPLOYEE).
J12175*    END-PERFORM.
J12175
J12175 1012-NEXT.
J12175     PERFORM 860-FIND-NXTRNG-PGESET1.
J12175
J12175 1012-END.
J12175******************************************************************
J12175 1015-FIND-BENEFICRY.
J12175******************************************************************
J12175
J12175     MOVE EMP-COMPANY            TO CRT-COMPANY.
J12175     MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL.
J12175     PERFORM 700-HR-EMP-SECURITY.
J12175     IF (HRWS-EMP-SECURED)
J12175         GO TO 1015-END 
J12175     END-IF.
J12175     MOVE EMP-LAST-NAME          TO SF1-EMP-LAST-NAME.
J12175     MOVE EMP-FIRST-NAME         TO SF1-EMP-FIRST-NAME.
J12175     MOVE EMP-MIDDLE-INIT        TO SF1-EMP-MIDDLE-INIT.
J12175     MOVE EMP-LAST-NAME-PRE      TO SF1-EMP-LAST-NAME-PRE.
J12175     MOVE EMP-NAME-SUFFIX        TO SF1-EMP-NAME-SUFFIX.
J12175     MOVE EMP-TERM-DATE          TO SF1-TERMINATED.
J12175
J12175     PERFORM 1040-FIND-PLAN-TYPE
J12175     THRU    1040-END.
J12175
J12175 1015-END.
008900******************************************************************
009000 1020-FIND-EMPLOYEE.                                              
009100******************************************************************
009200     MOVE BNF-EMPLOYEE           TO DB-EMPLOYEE.
009300
009400     MOVE PRM-COMPANY            TO DB-COMPANY.
009500     MOVE BNF-EMPLOYEE           TO DB-EMPLOYEE.
               DISPLAY "BNF-EMP-" DB-EMPLOYEE.
009600     PERFORM 840-FIND-EMPSET1.
009700     IF (EMPLOYEE-NOTFOUND)
                   DISPLAY "1020-EMP-" DB-EMPLOYEE
009800         GO TO 1020-NEXT.
009900
010000     MOVE EMP-COMPANY            TO CRT-COMPANY.
010100     MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL.
010200     PERFORM 700-HR-EMP-SECURITY.
010300     IF (HRWS-EMP-SECURED)
                   DISPLAY "SECURED -" EMP-COMPANY
010400         GO TO 1020-NEXT.
010500
010600     MOVE EMP-LAST-NAME          TO SF1-EMP-LAST-NAME.
010700     MOVE EMP-FIRST-NAME         TO SF1-EMP-FIRST-NAME.
010800     MOVE EMP-MIDDLE-INIT        TO SF1-EMP-MIDDLE-INIT.
           MOVE EMP-LAST-NAME-PRE      TO SF1-EMP-LAST-NAME-PRE.
           MOVE EMP-NAME-SUFFIX        TO SF1-EMP-NAME-SUFFIX.
010900     MOVE EMP-TERM-DATE          TO SF1-TERMINATED.
011000
011100     PERFORM 1040-FIND-PLAN-TYPE
011200     THRU    1040-END.
011300
011400     GO TO 1020-END.
011500
011600 1020-NEXT.
011700     PERFORM 860-FIND-NEXT-BNFSET1
011800         UNTIL  (BENEFICRY-NOTFOUND)
011900         OR     (BNF-COMPANY  NOT = DB-COMPANY)
012000         OR     (BNF-EMPLOYEE NOT = DB-EMPLOYEE).
012100
012200 1020-END.
012300******************************************************************
012400 1040-FIND-PLAN-TYPE.
012500******************************************************************
J32626     IF (PRM-PLAN-TYPE (1) NOT = SPACES)
J32626         MOVE PRM-PLAN-TYPE (1)  TO DB-PLAN-TYPE
013600     MOVE SPACES                 TO DB-PLAN-CODE
013700     MOVE ZEROS                  TO DB-SEQ-NBR

012800         PERFORM 850-FIND-NLT-BNFSET1
012900         PERFORM 1060-SEL-BNF-PLAN-CODE
013000         THRU    1060-END
013100             UNTIL (BENEFICRY-NOTFOUND)
013200             OR    (BNF-COMPANY   NOT = DB-COMPANY)
013300             OR    (BNF-EMPLOYEE  NOT = DB-EMPLOYEE)
013400             OR    (BNF-PLAN-TYPE NOT = DB-PLAN-TYPE).
013500
013600     MOVE SPACES                 TO DB-PLAN-CODE.
013700     MOVE ZEROS                  TO DB-SEQ-NBR.
013800
J32626     IF (PRM-PLAN-TYPE (2) NOT = SPACES)
J32626         MOVE PRM-PLAN-TYPE (2)  TO DB-PLAN-TYPE
014100         PERFORM 850-FIND-NLT-BNFSET1
014200         PERFORM 1060-SEL-BNF-PLAN-CODE
014300         THRU    1060-END
014400             UNTIL (BENEFICRY-NOTFOUND)
014500             OR    (BNF-COMPANY   NOT = DB-COMPANY)
014600             OR    (BNF-EMPLOYEE  NOT = DB-EMPLOYEE)
014700             OR    (BNF-PLAN-TYPE NOT = DB-PLAN-TYPE).
014800
014900     MOVE SPACES                 TO DB-PLAN-CODE.
015000     MOVE ZEROS                  TO DB-SEQ-NBR.
015100
J32626     IF (PRM-PLAN-TYPE (3) NOT = SPACES)
J32626         MOVE PRM-PLAN-TYPE (3)  TO DB-PLAN-TYPE
015400         PERFORM 850-FIND-NLT-BNFSET1
015500         PERFORM 1060-SEL-BNF-PLAN-CODE
015600         THRU    1060-END
015700             UNTIL (BENEFICRY-NOTFOUND)
015800             OR    (BNF-COMPANY   NOT = DB-COMPANY)
015900             OR    (BNF-EMPLOYEE  NOT = DB-EMPLOYEE)
016000             OR    (BNF-PLAN-TYPE NOT = DB-PLAN-TYPE).
016100
016200     MOVE SPACES                 TO DB-PLAN-CODE.
016300     MOVE ZEROS                  TO DB-SEQ-NBR.
016400
J32626     IF (PRM-PLAN-TYPE (4) NOT = SPACES)
J32626         MOVE PRM-PLAN-TYPE (4)  TO DB-PLAN-TYPE
016700         PERFORM 850-FIND-NLT-BNFSET1
016800         PERFORM 1060-SEL-BNF-PLAN-CODE
016900         THRU    1060-END
017000             UNTIL (BENEFICRY-NOTFOUND)
017100             OR    (BNF-COMPANY   NOT = DB-COMPANY)
017200             OR    (BNF-EMPLOYEE  NOT = DB-EMPLOYEE)
017300             OR    (BNF-PLAN-TYPE NOT = DB-PLAN-TYPE).
017400
J32626     IF (PRM-PLAN-TYPE (5) NOT = SPACES)
J32626         MOVE PRM-PLAN-TYPE (5)  TO DB-PLAN-TYPE
J32626         PERFORM 850-FIND-NLT-BNFSET1
J32626         PERFORM 1060-SEL-BNF-PLAN-CODE
J32626         THRU    1060-END
J32626             UNTIL (BENEFICRY-NOTFOUND)
J32626             OR    (BNF-COMPANY   NOT = DB-COMPANY)
J32626             OR    (BNF-EMPLOYEE  NOT = DB-EMPLOYEE)
J32626             OR    (BNF-PLAN-TYPE NOT = DB-PLAN-TYPE).
J32626
J32626     IF (PRM-PLAN-TYPE (6) NOT = SPACES)
J32626         MOVE PRM-PLAN-TYPE (6)  TO DB-PLAN-TYPE
J32626         PERFORM 850-FIND-NLT-BNFSET1
J32626         PERFORM 1060-SEL-BNF-PLAN-CODE
J32626         THRU    1060-END
J32626             UNTIL (BENEFICRY-NOTFOUND)
J32626             OR    (BNF-COMPANY   NOT = DB-COMPANY)
J32626             OR    (BNF-EMPLOYEE  NOT = DB-EMPLOYEE)
J32626             OR    (BNF-PLAN-TYPE NOT = DB-PLAN-TYPE).
J32626
J32626     IF (PRM-PLAN-TYPE (7) NOT = SPACES)
J32626         MOVE PRM-PLAN-TYPE (7)  TO DB-PLAN-TYPE
J32626         PERFORM 850-FIND-NLT-BNFSET1
J32626         PERFORM 1060-SEL-BNF-PLAN-CODE
J32626         THRU    1060-END
J32626             UNTIL (BENEFICRY-NOTFOUND)
J32626             OR    (BNF-COMPANY   NOT = DB-COMPANY)
J32626             OR    (BNF-EMPLOYEE  NOT = DB-EMPLOYEE)
J32626             OR    (BNF-PLAN-TYPE NOT = DB-PLAN-TYPE).
J32626
J32626     IF (PRM-PLAN-TYPE (8) NOT = SPACES)
J32626         MOVE PRM-PLAN-TYPE (8)  TO DB-PLAN-TYPE
J32626         PERFORM 850-FIND-NLT-BNFSET1
J32626         PERFORM 1060-SEL-BNF-PLAN-CODE
J32626         THRU    1060-END
J32626             UNTIL (BENEFICRY-NOTFOUND)
J32626             OR    (BNF-COMPANY   NOT = DB-COMPANY)
J32626             OR    (BNF-EMPLOYEE  NOT = DB-EMPLOYEE)
J32626             OR    (BNF-PLAN-TYPE NOT = DB-PLAN-TYPE).
J32626
017500     MOVE "ZZ"                   TO DB-PLAN-TYPE.
017600     PERFORM 850-FIND-NLT-BNFSET1.
017700
017800     IF  (BENEFICRY-FOUND)
017900     AND (BNF-COMPANY = DB-COMPANY)
018000         MOVE BNF-EMPLOYEE       TO DB-EMPLOYEE.
018100
018200 1040-END.
018300******************************************************************
018400 1060-SEL-BNF-PLAN-CODE.
018500******************************************************************
                   DISPLAY "INSIDE 1060-0" BNF-PLAN-CODE.
018600     IF (PRM-PLAN-CODE (1) NOT = SPACES)
018700     OR (PRM-PLAN-CODE (2) NOT = SPACES)
018800     OR (PRM-PLAN-CODE (3) NOT = SPACES)
018900     OR (PRM-PLAN-CODE (4) NOT = SPACES)
019000     OR (PRM-PLAN-CODE (5) NOT = SPACES)
019100     OR (PRM-PLAN-CODE (6) NOT = SPACES)
019200     OR (PRM-PLAN-CODE (7) NOT = SPACES)
019300     OR (PRM-PLAN-CODE (8) NOT = SPACES)
019400         IF  (PRM-PLAN-CODE (1) = BNF-PLAN-CODE)
019500         AND (PRM-PLAN-CODE (1) NOT = SPACES)
                   DISPLAY "NEXT-"
019600             NEXT SENTENCE
019700         ELSE
019800         IF  (PRM-PLAN-CODE (2) = BNF-PLAN-CODE)
019900         AND (PRM-PLAN-CODE (2) NOT = SPACES)
020000             NEXT SENTENCE
020100         ELSE
020200         IF  (PRM-PLAN-CODE (3) = BNF-PLAN-CODE)
020300         AND (PRM-PLAN-CODE (3) NOT = SPACES)
020400             NEXT SENTENCE
020500         ELSE
020600         IF  (PRM-PLAN-CODE (4) = BNF-PLAN-CODE)
020700         AND (PRM-PLAN-CODE (4) NOT = SPACES)
020800             NEXT SENTENCE
020900         ELSE
021000         IF  (PRM-PLAN-CODE (5) = BNF-PLAN-CODE)
021100         AND (PRM-PLAN-CODE (5) NOT = SPACES)
021200             NEXT SENTENCE
021300         ELSE
021400         IF  (PRM-PLAN-CODE (6) = BNF-PLAN-CODE)
021500         AND (PRM-PLAN-CODE (6) NOT = SPACES)
021600             NEXT SENTENCE
021700         ELSE
021800         IF  (PRM-PLAN-CODE (7) = BNF-PLAN-CODE)
021900         AND (PRM-PLAN-CODE (7) NOT = SPACES)
022000             NEXT SENTENCE
022100         ELSE
022200         IF  (PRM-PLAN-CODE (8) = BNF-PLAN-CODE)
022300         AND (PRM-PLAN-CODE (8) NOT = SPACES)
022400             NEXT SENTENCE
022500         ELSE
                   DISPLAY "READ-NEXT-"
022600             GO TO 1060-NEXT-BENEFICRY.
022700
J32626     IF (BNF-PLAN-TYPE = "DB")
022900         MOVE 1                  TO SF1-PLAN-TYPE-NBR
023000     ELSE
J32626     IF (BNF-PLAN-TYPE = "DC")
023200         MOVE 2                  TO SF1-PLAN-TYPE-NBR
023300     ELSE
J32626     IF (BNF-PLAN-TYPE = "DI")
023500         MOVE 3                  TO SF1-PLAN-TYPE-NBR
023600     ELSE
J32626     IF (BNF-PLAN-TYPE = "DL")
023800         MOVE 4                  TO SF1-PLAN-TYPE-NBR
023900     ELSE
J32626     IF (BNF-PLAN-TYPE = "EL")
J32626         MOVE 5                  TO SF1-PLAN-TYPE-NBR
J32626     ELSE
J32626     IF (BNF-PLAN-TYPE = "RS")
J32626         MOVE 6                  TO SF1-PLAN-TYPE-NBR
J32626     ELSE
J32626     IF (BNF-PLAN-TYPE = "SB")
J32626         MOVE 7                  TO SF1-PLAN-TYPE-NBR
J32626     ELSE
J32626     IF (BNF-PLAN-TYPE = "SP")
J32626         MOVE 8                  TO SF1-PLAN-TYPE-NBR
J32626     END-IF.
024100
024200     MOVE BNF-PLAN-TYPE          TO SF1-BNF-PLAN-TYPE.
024300     MOVE BNF-PLAN-CODE          TO SF1-BNF-PLAN-CODE.
024400     MOVE BNF-EMPLOYEE           TO SF1-BNF-EMPLOYEE.
024500     MOVE BNF-SEQ-NBR            TO SF1-BNF-SEQ-NBR.
024600     MOVE BNF-LAST-NAME          TO SF1-BNF-LAST-NAME.
024700     MOVE BNF-FIRST-NAME         TO SF1-BNF-FIRST-NAME.
024800     MOVE BNF-MIDDLE-INIT        TO SF1-BNF-MIDDLE-INIT.
           INITIALIZE SF1-BNF-LAST-NAME-PRE
                      SF1-BNF-NAME-SUFFIX.
024900     MOVE BNF-PMT-AMT            TO SF1-BNF-PMT-AMT.
P83206     MOVE BNF-PRIM-CNTGNT        TO SF1-BNF-PRIM-CNTGNT.
           MOVE BNF-TRUST              TO SF1-BNF-TRUST.
           MOVE BNF-BENEF-TYPE         TO SF1-BNF-BENEF-TYPE.
           MOVE  BNF-LAST-NAME-PRE     TO SF1-BNF-LAST-NAME-PRE.
           MOVE  BNF-NAME-SUFFIX       TO SF1-BNF-NAME-SUFFIX.
           MOVE  BNF-REL-CODE          TO SF1-BNF-REL-CODE.
           MOVE  BNF-FICA-NBR          TO SF1-BNF-FICA-NBR.
           MOVE  BNF-EMP-ADDRESS       TO SF1-BNF-EMP-ADDRESS.
           MOVE  BNF-ADDR1             TO SF1-BNF-ADDR1.
           MOVE  BNF-ADDR2             TO SF1-BNF-ADDR2.
           MOVE  BNF-ADDR3             TO SF1-BNF-ADDR3.
           MOVE  BNF-ADDR4             TO SF1-BNF-ADDR4.
           MOVE  BNF-CITY              TO SF1-BNF-CITY.
           MOVE  BNF-STATE             TO SF1-BNF-STATE.
           MOVE  BNF-ZIP               TO SF1-BNF-ZIP.
           MOVE  BNF-COUNTRY-CODE      TO SF1-BNF-COUNTRY-CODE.
           MOVE  BNF-CMT-TEXT          TO SF1-BNF-CMT-TEXT.
           MOVE  BNF-DATE-STAMP        TO SF1-BNF-DATE-STAMP.
           MOVE  BNF-TIME-STAMP        TO SF1-BNF-TIME-STAMP.
           MOVE  BNF-USER-ID           TO SF1-BNF-USER-ID.
025100     RELEASE SORT-REC            FROM SF1-SORT-REC.
               DISPLAY "WRITING-" BNF-EMPLOYEE.
025200
025300 1060-NEXT-BENEFICRY.
025400     PERFORM 860-FIND-NEXT-BNFSET1.
025500 1060-END.
025600******************************************************************
025700 1000-SECTION-END.
025800******************************************************************
025900******************************************************************
026000 2000-PRT-REPORT                 SECTION 50.
026100******************************************************************
026200 2000-START.
026300
026400     MOVE WS-FALSE               TO WS-END-OF-FILE-SW.
026500
026600     RETURN SORT-FILE            INTO SF1-SORT-REC
026700         AT END
026800             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
026900
027000     IF (END-OF-FILE)
027100         MOVE 110                TO CRT-MSG-NBR
027200         PERFORM 780-PRINT-MSG
027300         GO TO 2000-SECTION-END.
027400
027500     PERFORM 2010-DO-BNF-COMPANY
027600     THRU    2010-END
027700         UNTIL (END-OF-FILE).
027800
027900     GO TO 2000-SECTION-END.
028000******************************************************************
028100 2010-DO-BNF-COMPANY.
028200******************************************************************
028300     PERFORM 2020-DO-BNF-PLAN-TYPE
028400     THRU    2020-END
028500         UNTIL (END-OF-FILE).
028600
028700 2010-END.
028800******************************************************************
028900 2020-DO-BNF-PLAN-TYPE.
029000******************************************************************
029100
029200     MOVE SF1-BNF-PLAN-TYPE      TO WS-BNF-PLAN-TYPE.
029300
029400     PERFORM 2030-DO-BNF-PLAN-CODE
029500     THRU    2030-END
029600         UNTIL (END-OF-FILE)
029700         OR    (SF1-BNF-PLAN-TYPE NOT = WS-BNF-PLAN-TYPE).
029800
029900 2020-END.
030000******************************************************************
030100 2030-DO-BNF-PLAN-CODE.
030200******************************************************************
030300     MOVE SF1-BNF-PLAN-CODE      TO WS-BNF-PLAN-CODE
030400                                    WS-PLAN-CODE.
030500
030600     PERFORM 2230-PRT-BNF-PLAN-CODE-GRP
030700     THRU    2230-END.
030800
030900     PERFORM 2040-DO-BNF-EMPLOYEE
031000     THRU    2040-END
031100         UNTIL (END-OF-FILE)
031200         OR    (SF1-BNF-PLAN-TYPE NOT = WS-BNF-PLAN-TYPE)
031300         OR    (SF1-BNF-PLAN-CODE NOT = WS-BNF-PLAN-CODE).
031400
031500 2030-END.
031600******************************************************************
031700 2040-DO-BNF-EMPLOYEE.
031800******************************************************************
031900     MOVE SF1-BNF-EMPLOYEE       TO WS-BNF-EMPLOYEE.
032000
032100     PERFORM 2240-PRT-BNF-EMPLOYEE-GRP
032200     THRU    2240-END.
032300
032700     MOVE ZEROS  TO I1 I2.
032800     INITIALIZE WS-TABLE.
032900
033000     PERFORM 2050-DO-TABLE
033100     THRU    2050-END
033200         UNTIL (END-OF-FILE)
033300         OR    (SF1-BNF-PLAN-TYPE NOT = WS-BNF-PLAN-TYPE)
033400         OR    (SF1-BNF-PLAN-CODE NOT = WS-BNF-PLAN-CODE)
033500         OR    (SF1-BNF-EMPLOYEE  NOT = WS-BNF-EMPLOYEE).
033600
           IF (I1 > 0)    
032400         MOVE R1GN4H-BNF-SEQ-NBR    TO RPT-GROUP-REQUEST
032500         PERFORM 700-PRINT-RPT-GRP
               MOVE WS-TRUE TO WS-PRIMARY-SW  
033700         PERFORM 2060-DO-BNF-SEQ-NBR
033800         THRU    2060-END
033900             VARYING I3 FROM 1 BY 1
034000             UNTIL  (I3 > I1).
       
           IF (I2 > 0)    
032400         MOVE R1GN4H-BNF-CONTINGENT TO RPT-GROUP-REQUEST
               MOVE WS-FALSE TO WS-PRIMARY-SW  
032500         PERFORM 700-PRINT-RPT-GRP
033700         PERFORM 2060-DO-BNF-SEQ-NBR
033800         THRU    2060-END
033900             VARYING I3 FROM 1 BY 1
034100             UNTIL  (I3 > I2).
034200
034300 2040-END.
034400******************************************************************
034500 2050-DO-TABLE.
034600******************************************************************
034700     IF (SF1-BNF-PRIM-CNTGNT = 1)
               IF (SF1-BNF-BENEF-TYPE = 0)
034800             ADD 1 TO I1
034900             MOVE SF1-BNF-LAST-NAME    
                                        TO WS-TABLE-P-LAST-NAME (I1)
035000             MOVE SF1-BNF-FIRST-NAME   
                                        TO WS-TABLE-P-FIRST-NAME (I1)
035100             MOVE SF1-BNF-MIDDLE-INIT 
                                        TO WS-TABLE-P-MIDDLE-INIT (I1)
                   MOVE SF1-BNF-LAST-NAME-PRE
                                        TO WS-TABLE-P-LAST-NAME-PRE (I1)
                   MOVE SF1-BNF-NAME-SUFFIX
                                        TO WS-TABLE-P-NAME-SUFFIX (I1)
               ELSE
034800             ADD 1 TO I1
                   MOVE SF1-BNF-TRUST   TO WS-TABLE-P-TRUST (I1)
               END-IF
035200         MOVE SF1-BNF-PMT-AMT     TO WS-TABLE-P-PMT-AMT (I1)
               MOVE SF1-BNF-BENEF-TYPE 
                                        TO WS-TABLE-P-BENEF-TYPE (I1)
               MOVE SF1-BNF-REL-CODE 
                                        TO WS-TABLE-P-BNF-REL-CODE (I1)
               MOVE SF1-BNF-FICA-NBR 
                                        TO WS-TABLE-P-BNF-FICA-NBR (I1)
               MOVE SF1-BNF-EMP-ADDRESS 
                                     TO WS-TABLE-P-BNF-EMP-ADDRESS (I1)
               MOVE SF1-BNF-ADDR1
                                        TO WS-TABLE-P-ADDR1 (I1)
               MOVE SF1-BNF-ADDR2
                                        TO WS-TABLE-P-ADDR2 (I1)
               MOVE SF1-BNF-ADDR3
                                        TO WS-TABLE-P-ADDR3 (I1)
               MOVE SF1-BNF-ADDR4
                                        TO WS-TABLE-P-ADDR4 (I1)
               MOVE SF1-BNF-CITY
                                        TO WS-TABLE-P-CITY (I1)
               MOVE SF1-BNF-STATE
                                        TO WS-TABLE-P-STATE (I1)
               MOVE SF1-BNF-ZIP
                                        TO WS-TABLE-P-ZIP (I1)
               MOVE SF1-BNF-COUNTRY-CODE
                                        TO WS-TABLE-P-COUNTRY-CODE (I1)
               MOVE SF1-BNF-CMT-TEXT    TO WS-TABLE-P-CMT-TEXT (I1)
J61529         MOVE SF1-BNF-DATE-STAMP  TO WS-TABLE-P-DATE-STAMP (I1)
J61529         MOVE SF1-BNF-TIME-STAMP  TO WS-TABLE-P-TIME-STAMP (I1)
J61529         MOVE SF1-BNF-USER-ID     TO WS-TABLE-P-USER-ID (I1)
               
035300     ELSE
           IF (SF1-BNF-PRIM-CNTGNT NOT = 1)
               IF (SF1-BNF-BENEF-TYPE = 0)
035400             ADD 1 TO I2
035500             MOVE SF1-BNF-LAST-NAME 
                                        TO WS-TABLE-C-LAST-NAME (I2)
035600             MOVE SF1-BNF-FIRST-NAME  
                                        TO WS-TABLE-C-FIRST-NAME (I2)
035700             MOVE SF1-BNF-MIDDLE-INIT 
                                        TO WS-TABLE-C-MIDDLE-INIT (I2)
                   MOVE SF1-BNF-LAST-NAME-PRE
                                        TO WS-TABLE-C-LAST-NAME-PRE (I2)
                   MOVE SF1-BNF-NAME-SUFFIX
                                        TO WS-TABLE-C-NAME-SUFFIX (I2)
               ELSE
035400             ADD 1 TO I2
                   MOVE SF1-BNF-TRUST   TO WS-TABLE-C-TRUST (I2)
               END-IF
035800         MOVE SF1-BNF-PMT-AMT     TO WS-TABLE-C-PMT-AMT (I2)
               MOVE SF1-BNF-BENEF-TYPE 
                                        TO WS-TABLE-C-BENEF-TYPE (I2)
               MOVE SF1-BNF-REL-CODE 
                                        TO WS-TABLE-C-BNF-REL-CODE (I2)
               MOVE SF1-BNF-FICA-NBR 
                                        TO WS-TABLE-C-BNF-FICA-NBR (I2)
               MOVE SF1-BNF-EMP-ADDRESS 
                                     TO WS-TABLE-C-BNF-EMP-ADDRESS (I2)
               MOVE SF1-BNF-ADDR1
                                        TO WS-TABLE-C-ADDR1 (I2)
               MOVE SF1-BNF-ADDR2
                                        TO WS-TABLE-C-ADDR2 (I2)
               MOVE SF1-BNF-ADDR3
                                        TO WS-TABLE-C-ADDR3 (I2)
               MOVE SF1-BNF-ADDR4
                                        TO WS-TABLE-C-ADDR4 (I2)
               MOVE SF1-BNF-CITY
                                        TO WS-TABLE-C-CITY (I2)
               MOVE SF1-BNF-STATE
                                        TO WS-TABLE-C-STATE (I2)
               MOVE SF1-BNF-ZIP
                                        TO WS-TABLE-C-ZIP (I2)
               MOVE SF1-BNF-COUNTRY-CODE
                                        TO WS-TABLE-C-COUNTRY-CODE (I2)
J61529         MOVE SF1-BNF-DATE-STAMP  TO WS-TABLE-C-DATE-STAMP (I2)
J61529         MOVE SF1-BNF-TIME-STAMP  TO WS-TABLE-C-TIME-STAMP (I2)
J61529         MOVE SF1-BNF-USER-ID     TO WS-TABLE-C-USER-ID (I2)
P33092         MOVE SF1-BNF-CMT-TEXT    TO WS-TABLE-C-CMT-TEXT (I2).
03590
036000 2050-NEXT-BENEFICRY.
036100     RETURN SORT-FILE           INTO SF1-SORT-REC
036200         AT END
036300             MOVE WS-TRUE        TO WS-END-OF-FILE-SW.
036400
036500 2050-END.
036600******************************************************************
036700 2060-DO-BNF-SEQ-NBR.
036800******************************************************************
           IF (WS-PRIMARY)
036900         PERFORM 2250-PRT-BNF-SEQ-NBR-GRP-P
037000         THRU    2250-END
           ELSE
036900         PERFORM 2260-PRT-BNF-SEQ-NBR-GRP-C
037000         THRU    2260-END.
037100
037200 2060-END.
037300******************************************************************
037400 2230-PRT-BNF-PLAN-CODE-GRP.
037500******************************************************************
037600 
037700     MOVE SF1-BNF-PLAN-CODE      TO R1G2-BNF-PLAN-CODE.
037800     MOVE SF1-BNF-PLAN-TYPE      TO R1G2-BNF-PLAN-TYPE.
037900 
038000     MOVE PRM-COMPANY            TO DB-COMPANY.
038100     MOVE SF1-BNF-PLAN-TYPE      TO DB-PLAN-TYPE.
038200     MOVE SF1-BNF-PLAN-CODE      TO DB-PLAN-CODE.
038300     PERFORM 840-FIND-PLNSET1.
038400     IF (PLAN-FOUND)
038500         MOVE PLN-DESC           TO R1G2-PLN-DESC 
038600         MOVE PLN-START-DATE     TO R1G2-PLN-START-DATE 
038700         MOVE PLN-STOP-DATE      TO R1G2-PLN-STOP-DATE 
J12221*    ELSE
J12221*        MOVE 103                TO CRT-MSG-NBR
J12221*        PERFORM 790-GET-MSG
J12221*        MOVE CRT-MESSAGE        TO R1G2-PLN-DESC
J12221*        MOVE ZEROS              TO R1G2-PLN-START-DATE
J12221*                                   R1G2-PLN-STOP-DATE.
J12221     END-IF.
039500     MOVE R1GN1-BNF-COMPANY      TO RPT-GROUP-REQUEST.
039600     PERFORM 700-PRINT-RPT-GRP.
039700     MOVE R1GN2-BNF-PLAN-CODE    TO RPT-GROUP-REQUEST.
039800     PERFORM 700-PRINT-RPT-GRP.
039900 
040000 2230-END.
040100******************************************************************
040200 2240-PRT-BNF-EMPLOYEE-GRP.
040300******************************************************************
040400     MOVE SF1-BNF-EMPLOYEE       TO R1G3-BNF-EMPLOYEE.
040500
040600     MOVE SF1-EMP-LAST-NAME      TO HRWS-LAST-NAME.
040700     MOVE SF1-EMP-FIRST-NAME     TO HRWS-FIRST-NAME.
040800     MOVE SF1-EMP-MIDDLE-INIT    TO HRWS-MIDDLE-INIT.
           MOVE SF1-EMP-LAST-NAME-PRE  TO HRWS-LAST-NAME-PRE.
           MOVE SF1-EMP-NAME-SUFFIX    TO HRWS-NAME-SUFFIX.
040900     PERFORM 750-HR-FORMAT-NAME.
041000     MOVE HRWS-FORMAT-NAME       TO R1G3-EMP-NAME.
041100
041200     IF (SF1-TERMINATED NOT = ZEROS)
041300         MOVE 104                TO CRT-MSG-NBR
041400         PERFORM 790-GET-MSG
041500         MOVE CRT-MESSAGE        TO R1G3-TERMINATED   
041600     ELSE
041700         MOVE SPACES             TO R1G3-TERMINATED.
041800
041900     MOVE R1GN3-BNF-EMPLOYEE     TO RPT-GROUP-REQUEST.
042000     PERFORM 700-PRINT-RPT-GRP.
042100 2240-END.
042200******************************************************************
042300 2250-PRT-BNF-SEQ-NBR-GRP-P.
042400******************************************************************
042500     IF (I3 NOT > I1)
P83206         MOVE 1                             TO
P83206                                            R1G4-BNF-PRIM-CNTGNT
               IF (WS-TABLE-P-BENEF-TYPE (I3) = 0)
042600             MOVE WS-TABLE-P-LAST-NAME   (I3)
                                                  TO HRWS-LAST-NAME
042700             MOVE WS-TABLE-P-FIRST-NAME  (I3)
                                                  TO HRWS-FIRST-NAME
042800             MOVE WS-TABLE-P-MIDDLE-INIT (I3) 
                                                  TO HRWS-MIDDLE-INIT
042800             MOVE WS-TABLE-P-LAST-NAME-PRE (I3)
                                                  TO HRWS-LAST-NAME-PRE
042800             MOVE WS-TABLE-P-NAME-SUFFIX (I3)
                                                  TO HRWS-NAME-SUFFIX
042900             PERFORM 750-HR-FORMAT-NAME
043000             MOVE HRWS-FORMAT-NAME          TO R1G4-BNF-P-NAME
J07551             MOVE 113                       TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
                   MOVE CRT-MESSAGE              
                                             TO R1G5-RELATION-TXT
                   MOVE WS-TABLE-P-BNF-REL-CODE (I3)  
                                                  TO R1G5-BNF-REL-CODE
J07551             MOVE 114                       TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
                   MOVE CRT-MESSAGE              
                                             TO R1G5-FICA-TXT    
                   MOVE WS-TABLE-P-BNF-FICA-NBR (I3)  
                                                  TO R1G5-BNF-FICA-NBR
               ELSE
                   MOVE WS-TABLE-P-TRUST (I3)     TO R1G4-BNF-P-NAME
                   MOVE SPACES                    TO R1G5-FICA-TXT
                   MOVE SPACES                    TO R1G5-RELATION-TXT
               END-IF 
043100         MOVE WS-TABLE-P-PMT-AMT (I3)       TO R1G4-BNF-P-PMT-AMT 
J61529         MOVE WS-TABLE-P-DATE-STAMP (I3)    TO R1G4-BNF-DATE-STAMP
J61529         MOVE WS-TABLE-P-TIME-STAMP (I3)    TO R1G4-BNF-TIME-STAMP
J61529         MOVE WS-TABLE-P-USER-ID (I3)       TO R1G4-BNF-USER-ID
P33092         MOVE WS-TABLE-P-CMT-TEXT (I3)      TO R1G7-BNF-CMT-TEXT
               IF (WS-TABLE-P-BNF-EMP-ADDRESS (I3) = "H") 
P33092             MOVE 105                       TO CRT-MSG-NBR 
P33092             PERFORM 790-GET-MSG 
P33092             MOVE CRT-MESSAGE               TO R1G8-BNF-ADDR1 
                ELSE 
                IF (WS-TABLE-P-BNF-EMP-ADDRESS (I3) = "S") 
P33092             MOVE 106                       TO CRT-MSG-NBR 
                   PERFORM 790-GET-MSG 
P33092             MOVE CRT-MESSAGE               TO R1G8-BNF-ADDR1 
                ELSE
P33092             MOVE WS-TABLE-P-ADDR1 (I3)         TO R1G8-BNF-ADDR1
                   MOVE WS-TABLE-P-ADDR2 (I3)         TO R1G6-BNF-ADDR2
                   MOVE WS-TABLE-P-ADDR3 (I3)         TO R1G6-BNF-ADDR3
                   MOVE WS-TABLE-P-ADDR4 (I3)         TO R1G6-BNF-ADDR4
                   MOVE WS-TABLE-P-CITY (I3)          TO R1G6-BNF-CITY
                   MOVE WS-TABLE-P-STATE (I3)         TO R1G6-BNF-STATE
                   MOVE WS-TABLE-P-ZIP (I3)           TO R1G6-BNF-ZIP
                   MOVE WS-TABLE-P-COUNTRY-CODE (I3) 
                                             TO R1G6-BNF-COUNTRY-CODE
               END-IF 
               END-IF 
043200     ELSE
043300         MOVE SPACES                        TO R1G4-BNF-P-NAME
043400         MOVE ZEROS                         TO R1G4-BNF-P-PMT-AMT.

044700     MOVE R1GN4D-BNF-SEQ-NBR     TO RPT-GROUP-REQUEST.
044800     PERFORM 700-PRINT-RPT-GRP.
001900     IF  (PRM-DETAIL   = "Y")    
P33092         IF  (WS-TABLE-P-BENEF-TYPE (I3) = 0)
P33092             MOVE R1GN5D-BNF-SEQ-NBR TO RPT-GROUP-REQUEST
P33092             PERFORM 700-PRINT-RPT-GRP
P33092         END-IF
P33092         IF  (R1G7-BNF-CMT-TEXT NOT = SPACES)
P33092             MOVE R1GN7D-BNF-COMMENT TO RPT-GROUP-REQUEST
P33092             PERFORM 700-PRINT-RPT-GRP
P33092         END-IF
               IF  (WS-TABLE-P-BNF-EMP-ADDRESS (I3) = SPACES)
               AND (WS-TABLE-P-ADDR1 (I3)   NOT = SPACES)
P33092              MOVE R1GN8D-BNF-ADDR    TO RPT-GROUP-REQUEST
044800              PERFORM 700-PRINT-RPT-GRP
                    MOVE R1GN6D-BNF-SEQ-NBR TO RPT-GROUP-REQUEST
044800              PERFORM 700-PRINT-RPT-GRP
               ELSE
               IF  (WS-TABLE-P-BNF-EMP-ADDRESS (I3) NOT = SPACES)
P33092              MOVE R1GN8D-BNF-ADDR    TO RPT-GROUP-REQUEST
044800              PERFORM 700-PRINT-RPT-GRP.
043500
045000 2250-END.
042200******************************************************************
042300 2260-PRT-BNF-SEQ-NBR-GRP-C.
042400******************************************************************
043600     IF (I3 NOT > I2)
P83206         MOVE 2                             TO
P83206                                            R1G4-BNF-PRIM-CNTGNT
               IF (WS-TABLE-C-BENEF-TYPE (I3) = 0)
043700             MOVE WS-TABLE-C-LAST-NAME  (I3) 
                                                 TO HRWS-LAST-NAME
043800             MOVE WS-TABLE-C-FIRST-NAME (I3)
                                                 TO HRWS-FIRST-NAME
043900             MOVE WS-TABLE-C-MIDDLE-INIT (I3) 
                                                 TO HRWS-MIDDLE-INIT
042800             MOVE WS-TABLE-C-LAST-NAME-PRE (I3)
                                                 TO HRWS-LAST-NAME-PRE
042800             MOVE WS-TABLE-C-NAME-SUFFIX (I3)
                                                 TO HRWS-NAME-SUFFIX
044000             PERFORM 750-HR-FORMAT-NAME
044100             MOVE HRWS-FORMAT-NAME         TO R1G4-BNF-P-NAME
J07551             MOVE 113                       TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
                   MOVE CRT-MESSAGE              
                                             TO R1G5-RELATION-TXT
                   MOVE WS-TABLE-C-BNF-REL-CODE (I3)  
                                                  TO R1G5-BNF-REL-CODE
J07551             MOVE 114                       TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
                   MOVE CRT-MESSAGE              
                                             TO R1G5-FICA-TXT    
                   MOVE WS-TABLE-C-BNF-FICA-NBR (I3)  
                                                  TO R1G5-BNF-FICA-NBR
               ELSE
043100             MOVE WS-TABLE-C-TRUST   (I3)   TO R1G4-BNF-P-NAME   
                   MOVE SPACES                    TO R1G5-FICA-TXT
                   MOVE SPACES                    TO R1G5-RELATION-TXT  
               END-IF
044200         MOVE WS-TABLE-C-PMT-AMT (I3)       TO R1G4-BNF-P-PMT-AMT
043100         MOVE WS-TABLE-C-PMT-AMT (I3)       TO R1G4-BNF-P-PMT-AMT
J61529         MOVE WS-TABLE-C-DATE-STAMP (I3)    TO R1G4-BNF-DATE-STAMP
J61529         MOVE WS-TABLE-C-TIME-STAMP (I3)    TO R1G4-BNF-TIME-STAMP
J61529         MOVE WS-TABLE-C-USER-ID (I3)       TO R1G4-BNF-USER-ID
               MOVE WS-TABLE-C-BNF-REL-CODE (I3)  TO R1G5-BNF-REL-CODE
               MOVE WS-TABLE-C-BNF-FICA-NBR (I3)  TO R1G5-BNF-FICA-NBR
P33092         MOVE WS-TABLE-C-CMT-TEXT (I3)      TO R1G7-BNF-CMT-TEXT
               IF (WS-TABLE-C-BNF-EMP-ADDRESS (I3) = "H")
P33092             MOVE 105                       TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
P33092             MOVE CRT-MESSAGE               TO R1G8-BNF-ADDR1
               ELSE
               IF (WS-TABLE-C-BNF-EMP-ADDRESS (I3) = "S")
P33092             MOVE 106                       TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
P33092             MOVE CRT-MESSAGE               TO R1G8-BNF-ADDR1
               ELSE
P33092             MOVE WS-TABLE-C-ADDR1 (I3)         TO R1G8-BNF-ADDR1
                   MOVE WS-TABLE-C-ADDR2 (I3)         TO R1G6-BNF-ADDR2
                   MOVE WS-TABLE-C-ADDR3 (I3)         TO R1G6-BNF-ADDR3
                   MOVE WS-TABLE-C-ADDR4 (I3)         TO R1G6-BNF-ADDR4
                   MOVE WS-TABLE-C-CITY (I3)          TO R1G6-BNF-CITY
                   MOVE WS-TABLE-C-STATE (I3)         TO R1G6-BNF-STATE
                   MOVE WS-TABLE-C-ZIP (I3)           TO R1G6-BNF-ZIP
                   MOVE WS-TABLE-C-COUNTRY-CODE (I3) 
                                          TO R1G6-BNF-COUNTRY-CODE
               END-IF
               END-IF
044300     ELSE
044400         MOVE SPACES                        TO R1G4-BNF-P-NAME
044500         MOVE ZEROS                         TO R1G4-BNF-P-PMT-AMT.
044600
044700     MOVE R1GN4D-BNF-SEQ-NBR     TO RPT-GROUP-REQUEST.
044800     PERFORM 700-PRINT-RPT-GRP.
001900     IF  (PRM-DETAIL   = "Y")    
P33092         IF  (WS-TABLE-C-BENEF-TYPE (I3) = 0)
P33092             MOVE R1GN5D-BNF-SEQ-NBR TO RPT-GROUP-REQUEST
P33092             PERFORM 700-PRINT-RPT-GRP
P33092         END-IF
P33092         IF  (R1G7-BNF-CMT-TEXT NOT = SPACES)
P33092             MOVE R1GN7D-BNF-COMMENT TO RPT-GROUP-REQUEST
P33092             PERFORM 700-PRINT-RPT-GRP
P33092         END-IF
P33092         IF  (WS-TABLE-C-BNF-EMP-ADDRESS (I3) NOT = SPACES)
P33092              MOVE R1GN8D-BNF-ADDR    TO RPT-GROUP-REQUEST
P33092              PERFORM 700-PRINT-RPT-GRP 
P33092         END-IF
P33092         IF  (WS-TABLE-C-BNF-EMP-ADDRESS (I3) = SPACES)
P33092         AND (WS-TABLE-C-ADDR1 (I3)   NOT = SPACES)
P33092          MOVE R1GN8D-BNF-ADDR    TO RPT-GROUP-REQUEST
044800          PERFORM 700-PRINT-RPT-GRP
                MOVE R1GN6D-BNF-SEQ-NBR TO RPT-GROUP-REQUEST
044800          PERFORM 700-PRINT-RPT-GRP.
044900
045000 2260-END.
045100******************************************************************
045200 2000-SECTION-END.
045300******************************************************************
