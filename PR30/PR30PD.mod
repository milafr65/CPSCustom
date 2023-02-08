******* PR30PD 32.1.9 <2045965204>
000100******************************************************************
000200*                             PR30PD                             *
000300******************************************************************
      ******************************************************************
      *                                                                *
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  313588 | J13588 | Remove LP trigger checks                    *
      *  ------   ------   ------------------------------------------  *
      *  348634 | J48634 |  INITIALIZE HREMP EMPLOYEE FIELDS           *
      *  ------   ------   ------------------------------------------  *
      *  741010 | J41010 | ENHANCEMENT FOR ME PAYROLL -                *
      *         |        | ADDED NEW FIELD DAILY TIME RECORD           *
      *  ------   ------   ------------------------------------------  *
      *  819756 | J19756 | ADDED NEW ERROR MESSAGE                     *
      *  ------   ------   ------------------------------------------  *
      *  841321 | J41321 | ADDED NEW EDIT FOR NON-CASH PAYCODE,        *
      *                    CALC-TYPE = "F"                             *
      *  ------   ------   ------------------------------------------  *
      *  859938 | J59938 | Reverted back the changes done in above JT  *
      *                    841321 in Patch901 and updated PR30.1 to    *
      *                    display an edit message for missing rate for*
      *                    calc-type = "F"                             *
      *  ------   ------   ------------------------------------------  *
      *  885777 | J85777 | ADDED MOVE STATEMENTS FOR DB-COMPANY,       *
      *                    DB-EMPLOYEE, AND DB-TIME-GROUP.             *
      *  ------   ------   ------------------------------------------  *
      * 1063408 | J63408 | EDIT AGAINST DELETE OF RECORDS ADDED IN LT530
      *  ------   ------   ------------------------------------------  *
ACS001*****************************************************************
ACS001* CHANGE LOG:                                                   *
ACS001*****************************************************************
ACS001* ??/??/??  - THE PR30PD WAS MODIFIED TO CHANGE STMSET1 TO      *
ACS001*             STMSETW1. THE PR30.SCR WAS MODIFIED TO ADD THE    *
ACS001*             FIELDS *PT-STM-EFFECT-DATE AND *PT-STM-END-DATE   *
ACS001*             ALSO THE +STM-BUS-NBR-GRP WAS INCREASED TO 5 BYTES*
ACS001* 10/29/10    ACS - M. HUNTER                                   *
ACS001*             REAPPLIED CUSTOMIZATIONS AFTER 9.0 APP UPGRADE    *
ACS002* 08/19/11    ACS - M. HUNTER                                   *
ACS002*             REAPPLIED CUSTOM CODE AFTER 9.0.1 APP UPGRADE     *
US1877* 12/21/21    MLF                                               *
US1877*             PATCH J85777 COMMENTED OUT IN CUSTOM CODE MERGE   *
US1877*             DUE TO CONCERNS IT WILL AFFECT THE EXISTING       *
US1877*             CUSTOM CODE                                       *
000400******************************************************************
000500 PR30S1-TRANSACTION              SECTION 10.
000600******************************************************************
000700 PR30S1-START.
000800
000900     PERFORM 200-EDIT-TRAN
001000     THRU    200-END.
J60347
J60347     IF  (NO-ERROR-FOUND)
J60347     AND (PR30F1-FC = "H")
J60347         PERFORM 705-SCREEN-XFER
J60347         THRU    705-END
J60347         GO TO PR30S1-TRANSACTION-END.
001100
001200     IF (NO-ERROR-FOUND)
001300         PERFORM 400-PROCESS-TRAN
001400         THRU    400-END.
001500
001600     GO TO PR30S1-TRANSACTION-END.
001700
001800******************************************************************
001900 200-EDIT-TRAN.
002000******************************************************************
002100
002200     PERFORM 210-EDIT-ACCESS
002300     THRU    210-END.
002400
002500     IF (ERROR-FOUND)
002600         GO TO 200-END.
002700
002800     IF (PR30F1-FC = "A" OR "C")
002900         PERFORM 230-EDIT-DATA
003000         THRU    230-END
003100         GO TO 200-END.
003200
003300 200-END.
003400
003500******************************************************************
003600 210-EDIT-ACCESS.
003700******************************************************************
003800
003900     MOVE PR30F1-STM-COMPANY           TO DB-COMPANY.
004000     MOVE SPACES                       TO DB-PROCESS-LEVEL.
004100     PERFORM 570-EDIT-PR-COMPANY.
004200     IF (ERROR-FOUND)
004300         MOVE PR30F1-STM-COMPANY-FN           TO CRT-FIELD-NBR
004400         GO TO 210-END.
J41010
J41010     IF  (PR30F1-STM-EMPLOYEE   NOT = ZEROS)
J41010     AND (PR30F1-STM-TIME-GROUP NOT = SPACES)
J41010*---PR30#106: Cannot select both employee and employee group
J41010         MOVE 106                      TO CRT-ERROR-NBR
J41010         MOVE "PR30"                   TO CRT-ERROR-CAT
J41010         MOVE PR30F1-STM-TIME-GROUP-FN TO CRT-FIELD-NBR
J41010         GO TO 210-END
J41010     END-IF.
J41010
J41010     IF  (PR30F1-STM-EMPLOYEE   = ZEROS)
J41010     AND (PR30F1-STM-TIME-GROUP = SPACES)
J41010*---PR30#112: Select either employee or employee group
J41010         MOVE 112                      TO CRT-ERROR-NBR
J41010         MOVE "PR30"                   TO CRT-ERROR-CAT
J41010         MOVE PR30F1-STM-EMPLOYEE-FN   TO CRT-FIELD-NBR
J41010         GO TO 210-END 
J41010     END-IF.
004500
004600     MOVE PRS-GL-UNITS                 TO PR30WS-GL-UNITS.
004700     MOVE PRS-TIPS                     TO PR30WS-TIPS-CLASS.
004800     MOVE PR30F1-STM-COMPANY           TO DB-COMPANY.
004900     MOVE PR30F1-STM-EMPLOYEE          TO DB-EMPLOYEE.
005000     MOVE PR30F1-STM-TIME-GROUP        TO DB-TIME-GROUP.
005100
005200     IF (PR30F1-FC = "N")
ACS001         MOVE ZEROS                    TO DB-EFFECT-DATE
ACS001         MOVE ZEROS                    TO DB-END-DATE
005300         MOVE HIGH-VALUES              TO DB-PAY-CODE
005400         MOVE WS-HIGH-VALUES           TO DB-SEQ-NBR
ACS001*        PERFORM 850-FIND-NLT-STMSET1.
ACS001         PERFORM 850-FIND-NLT-STMSETW1.
005600
005700     IF (PR30F1-FC = "P")
ACS001         MOVE WS-HIGH-VALUES          TO DB-EFFECT-DATE
ACS001         MOVE WS-HIGH-VALUES          TO DB-END-DATE
005800         MOVE SPACES                   TO DB-PAY-CODE
005900         MOVE ZEROS                    TO DB-SEQ-NBR
ACS001*        PERFORM 850-FIND-NLT-STMSET1
ACS001*        PERFORM 870-FIND-PREV-STMSET1
ACS001         PERFORM 850-FIND-NLT-STMSETW1
ACS001         PERFORM 870-FIND-PREV-STMSETW1
006200         IF  (STANDTIME-FOUND)
006300         AND (STM-COMPANY = DB-COMPANY)
006400             MOVE STM-EMPLOYEE   TO DB-EMPLOYEE
006500             MOVE STM-TIME-GROUP TO DB-TIME-GROUP
ACS001*            PERFORM 850-FIND-NLT-STMSET1.
ACS001             PERFORM 850-FIND-NLT-STMSETW1.
006700
006800     IF (PR30F1-FC = "N" OR "P")
006900         IF (STANDTIME-NOTFOUND)
007000         OR (STM-COMPANY NOT = PR30F1-STM-COMPANY)
007100             MOVE 12                          TO CRT-ERROR-NBR
007200             MOVE PR30F1-FC-FN                TO CRT-FIELD-NBR
007300             GO TO 210-END
007400         ELSE
007500             MOVE STM-EMPLOYEE         TO PR30F1-STM-EMPLOYEE
007600                                          DB-EMPLOYEE
007700             MOVE STM-TIME-GROUP       TO PR30F1-STM-TIME-GROUP
007800                                          DB-TIME-GROUP.
007900
008000     IF (PR30F1-STM-EMPLOYEE NOT = ZEROES)
008100         MOVE PR30F1-STM-COMPANY       TO DB-COMPANY
008200         MOVE PR30F1-STM-EMPLOYEE      TO DB-EMPLOYEE
008300         PERFORM 840-FIND-EMPSET1
008400         IF (EMPLOYEE-NOTFOUND)
008500             MOVE 101                         TO CRT-ERROR-NBR
008600             MOVE PR30F1-STM-EMPLOYEE-FN      TO CRT-FIELD-NBR
008700             GO TO 210-END
008800         ELSE
008900             MOVE EMP-EMP-STATUS       TO DB-EMP-STATUS
009000             PERFORM 840-FIND-EMSSET1
009100             IF (EMSTATUS-FOUND)
009200             AND (EMS-PAY-STATUS = "TB" OR "TN")
009300                 MOVE EMS-PAY-STATUS   TO CRT-ERR-VAR1
009400                 MOVE 125 TO CRT-MSG-NBR
009500                 PERFORM 790-GET-MSG.
009600
009700     IF (PR30F1-STM-TIME-GROUP NOT = SPACES)
009800         MOVE PR30F1-STM-COMPANY       TO DB-COMPANY
009900         MOVE PR30F1-STM-TIME-GROUP    TO DB-GROUP-NAME
010000         PERFORM 840-FIND-PRGSET1
010100         IF (PERSGROUP-NOTFOUND)
010200             MOVE 102                         TO CRT-ERROR-NBR
010300             MOVE PR30F1-STM-TIME-GROUP-FN    TO CRT-FIELD-NBR
010400             GO TO 210-END.
010500
010600     IF (PR30F1-STM-EMPLOYEE NOT = ZEROS)
010700         MOVE ZEROS              TO HRWS-NEXT-COUNT
010800         MOVE EMP-COMPANY        TO CRT-COMPANY
010900         MOVE EMP-PROCESS-LEVEL  TO CRT-PROCESS-LEVEL
011000         PERFORM 700-HR-EMP-SECURITY
011100         IF (HRWS-EMP-SECURED)
011200             IF (PR30F1-FC NOT = "N" AND "P")
011300                 MOVE 114                     TO CRT-ERROR-NBR
011400                 MOVE PR30F1-STM-EMPLOYEE-FN  TO CRT-FIELD-NBR
011500                 GO TO 210-END
011600             ELSE
011700                 IF (PR30F1-FC = "N")
ACS001*                    PERFORM 860-FIND-NEXT-STMSET1
ACS001                     PERFORM 860-FIND-NEXT-STMSETW1
011900                     PERFORM 212-GET-NEXT-EMPLOYEE
012000                     THRU    212-END
012100                         UNTIL (STANDTIME-NOTFOUND)
012200                         OR    (STM-COMPANY     NOT = DB-COMPANY)
012300                         OR    (STM-EMPLOYEE    = ZEROS)
012400                         OR    (HRWS-EMP-NOT-SECURED)
012500                         OR   ((HRWS-NEXT-COUNT > PRS-SEC-SEARCH)
012600                         AND   (PRS-SEC-SEARCH  > ZEROS))
012700                 ELSE
ACS001*                    PERFORM 870-FIND-PREV-STMSET1
ACS001                     PERFORM 870-FIND-PREV-STMSETW1
012900                     PERFORM 212-GET-NEXT-EMPLOYEE
013000                     THRU    212-END
013100                         UNTIL (STANDTIME-NOTFOUND)
013200                         OR    (STM-COMPANY     NOT = DB-COMPANY)
013300                         OR    (STM-EMPLOYEE    = ZEROS)
013400                         OR    (HRWS-EMP-NOT-SECURED)
013500                         OR   ((HRWS-NEXT-COUNT > PRS-SEC-SEARCH)
013600                         AND   (PRS-SEC-SEARCH  > ZEROS)).
013700
013800     IF (PR30F1-STM-EMPLOYEE NOT = ZEROS)
013900         IF  (PR30F1-FC = "N" OR "P")
014000         AND (HRWS-NEXT-COUNT > PRS-SEC-SEARCH)
014100         AND (PRS-SEC-SEARCH  > ZEROS)
014200             MOVE PRS-SEC-SEARCH          TO HRWS-SEARCH-NBR
014300             MOVE HRWS-SEARCH-VAR         TO CRT-ERR-VAR1
014400             MOVE 99                      TO CRT-ERROR-NBR
014500             MOVE PR30F1-FC-FN            TO CRT-FIELD-NBR
014600             GO TO 210-END.
014700
014800     IF (PR30F1-FC = "N" OR "P")
014900         IF (STANDTIME-NOTFOUND)
015000         OR (STM-COMPANY NOT = PR30F1-STM-COMPANY)
015100             MOVE 12                          TO CRT-ERROR-NBR
015200             MOVE PR30F1-FC-FN                TO CRT-FIELD-NBR
015300             GO TO 210-END
015400         ELSE
015500             MOVE STM-EMPLOYEE         TO PR30F1-STM-EMPLOYEE
015600                                          DB-EMPLOYEE
015700             MOVE STM-TIME-GROUP       TO PR30F1-STM-TIME-GROUP
015800                                          DB-TIME-GROUP.
015900
016000     IF (PR30F1-FC NOT = "I" AND "+" AND "-")
016100         GO TO 210-END.
016200
J85777*     MOVE PR30F1-STM-COMPANY          TO PR30WS-SAVE-COMPANY.
J85777*     MOVE PR30F1-STM-EMPLOYEE         TO PR30WS-SAVE-EMPLOYEE.
J85777*     MOVE PR30F1-STM-TIME-GROUP       TO PR30WS-SAVE-TIME-GROUP.

J41010     IF (PR30F1-STM-EMPLOYEE NOT = ZEROS)
J48634         INITIALIZE HREMP-SCR-FIELDS
J48634         PERFORM 7000-HREMP-DEFAULT
J41010     END-IF.

016300     IF (PR30F1-FC = "I")
ACS001         MOVE WS-HIGH-VALUES          TO DB-EFFECT-DATE
ACS001         MOVE WS-HIGH-VALUES          TO DB-END-DATE
016400         MOVE SPACES                  TO DB-PAY-CODE
016500         MOVE ZEROES                  TO DB-SEQ-NBR
016600     ELSE
016700     IF (PR30F1-FC = "+")
ACS001         MOVE PR30F1-PT-STM-EFFECT-DATE TO DB-EFFECT-DATE
ACS001         MOVE PR30F1-PT-STM-END-DATE    TO DB-END-DATE
016800         MOVE PR30F1-PT-STM-PAY-CODE  TO DB-PAY-CODE
016900         MOVE PR30F1-PT-STM-SEQ-NBR   TO DB-SEQ-NBR
017000     ELSE
017100         MOVE PR30F1-STM-PAY-CODE (1) TO DB-PAY-CODE
017200         MOVE PR30F1-STM-SEQ-NBR (1)  TO DB-SEQ-NBR.
017300
ACS001*    PERFORM 850-FIND-NLT-STMSET1.
ACS001     PERFORM 850-FIND-NLT-STMSETW1.
J85777*     MOVE PR30WS-SAVE-COMPANY         TO DB-COMPANY.
J85777*     MOVE PR30WS-SAVE-EMPLOYEE        TO DB-EMPLOYEE.
J85777*    MOVE PR30WS-SAVE-TIME-GROUP      TO DB-TIME-GROUP.

017500     IF (PR30F1-FC = "-")
ACS001*        PERFORM 870-FIND-PREV-STMSET1.
ACS001         PERFORM 870-FIND-PREV-STMSETW1.
017700
017800     IF (STANDTIME-NOTFOUND)
017900     OR (STM-COMPANY    NOT = DB-COMPANY)
018000     OR (STM-EMPLOYEE   NOT = DB-EMPLOYEE)
018100     OR (STM-TIME-GROUP NOT = DB-TIME-GROUP)
018200         MOVE 115                             TO CRT-ERROR-NBR
018300         MOVE PR30F1-FC-FN                    TO CRT-FIELD-NBR
018400         INITIALIZE PR30F1-DETAIL-DATA
018500         GO TO 210-END.
018600
018700 210-END.
018800
018900******************************************************************
019000 212-GET-NEXT-EMPLOYEE.
019100******************************************************************
019200     IF (STM-EMPLOYEE = DB-EMPLOYEE)
019300         GO TO 212-NEXT.
019400
019500     IF (STM-EMPLOYEE = ZEROS)
019600         GO TO 212-END.
019700
019800     ADD 1                       TO HRWS-NEXT-COUNT.
019900     MOVE STM-EMPLOYEE           TO DB-EMPLOYEE.
020000     PERFORM 840-FIND-EMPSET1.
020100     MOVE EMP-COMPANY            TO CRT-COMPANY.
020200     MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL.
020300     PERFORM 700-HR-EMP-SECURITY.
020400     IF (HRWS-EMP-NOT-SECURED)
020500         IF (PR30F1-FC = "N")
020600             GO TO 212-END
020700         ELSE
ACS001*            PERFORM 850-FIND-NLT-STMSET1
ACS001             PERFORM 850-FIND-NLT-STMSETW1
020900             GO TO 212-END.
021000
021100 212-NEXT.
021200     IF (PR30F1-FC = "P")
ACS001*        PERFORM 870-FIND-PREV-STMSET1
ACS001         PERFORM 870-FIND-PREV-STMSETW1
021400     ELSE
ACS001*        PERFORM 860-FIND-NEXT-STMSET1.
ACS001         PERFORM 860-FIND-NEXT-STMSETW1.

021600
021700 212-END.
021800
021900******************************************************************
022000 230-EDIT-DATA.
022100******************************************************************
022200
J13588*    MOVE PR30F1-STM-COMPANY          TO EDCDWS-COMPANY.
J13588*    MOVE "LP"                        TO EDCDWS-SYSTEM.
J13588*    PERFORM 6000-IS-SYSTEM-TRIGGER-ENABLED.

022300     IF  (PR30F1-STM-EMPLOYEE   NOT = ZEROS)
022400     AND (PR30F1-STM-TIME-GROUP NOT = SPACES)
022500         MOVE 106                             TO CRT-ERROR-NBR
022600         MOVE PR30F1-STM-TIME-GROUP-FN        TO CRT-FIELD-NBR
022700         GO TO 230-END.
022800
022900     IF  (PR30F1-STM-EMPLOYEE   = ZEROS)
023000     AND (PR30F1-STM-TIME-GROUP = SPACES)
023100         MOVE 112                             TO CRT-ERROR-NBR
023200         MOVE PR30F1-STM-EMPLOYEE-FN          TO CRT-FIELD-NBR
023300         GO TO 230-END.
023400
023500     IF (PR30F1-STM-EMPLOYEE NOT = ZEROS)
023600         MOVE EMP-EMP-STATUS     TO DB-EMP-STATUS
023700         PERFORM 840-FIND-EMSSET1
023800         IF (EMSTATUS-NOTFOUND)
023900             MOVE 126                         TO CRT-ERROR-NBR
024000             MOVE PR30F1-STM-EMPLOYEE-FN      TO CRT-FIELD-NBR
024100             GO TO 230-END
ACS001         ELSE
ACS001         IF (EMS-PAY-STATUS = "NN")
ACS001             MOVE 127                         TO CRT-ERROR-NBR
ACS001             MOVE PR30F1-STM-EMPLOYEE-FN      TO CRT-FIELD-NBR
ACS001             GO TO 230-END 
P53786         END-IF
P53786      END-IF.
024700*
024800     PERFORM 260-EDIT-DTL-TRAN
024900     THRU    260-END
025000         VARYING I1 FROM 1 BY 1
025100         UNTIL  (I1 > PR30F1-DETAIL-COUNT)
025200         OR     (ERROR-FOUND).
025300
025400 230-END.
025500
025600******************************************************************
025700 260-EDIT-DTL-TRAN.
025800******************************************************************
025900
026000     IF (PR30F1-LINE-FC (I1) = SPACES)
026100         GO TO 260-END.
026200
026300     IF (PR30F1-LINE-FC (I1) = "A")
026400         MOVE ZEROS               TO PR30F1-STM-SEQ-NBR (I1)
                                           PR30F1-STM-PENS-SEQ-NBR (I1).
026500
026600     MOVE PR30F1-STM-COMPANY           TO DB-COMPANY.
026700     MOVE PR30F1-STM-EMPLOYEE          TO DB-EMPLOYEE.
026800     MOVE PR30F1-STM-TIME-GROUP        TO DB-TIME-GROUP.
026900     MOVE PR30F1-STM-PAY-CODE (I1)     TO DB-PAY-CODE.
027000     MOVE PR30F1-STM-SEQ-NBR (I1)      TO DB-SEQ-NBR.
027100     PERFORM 840-FIND-STMSET1.
027200
027300     IF  (STANDTIME-NOTFOUND)
027400     AND (PR30F1-LINE-FC (I1) = "C" OR "D")
027500         MOVE 105                             TO CRT-ERROR-NBR
027600         MOVE PR30F1-LINE-FC-FN (I1)          TO CRT-FIELD-NBR
027700         GO TO 260-END.
027800
027900     IF (PR30F1-LINE-FC (I1) = "A" OR "C")
028000         PERFORM 264-EDIT-DTL-DATA
028100         THRU    264-END.
028200
028300     IF (PR30F1-LINE-FC (I1) = "D")
028400         PERFORM 268-EDIT-DTL-DELETE
028500         THRU    268-END.
028600
028700 260-END.
028800
028900******************************************************************
029000 264-EDIT-DTL-DATA.
029100******************************************************************
029200
J28323     IF (EMS-PAY-STATUS              = "NN" OR "NB")
J28323         IF  (PR30F1-LINE-FC (I1)    = "A")
J28323         AND (PR30F1-PT-XMIT-NBR-2   = ZEROES)
J28323             MOVE 1                      TO PR30F1-PT-XMIT-NBR-2
P53786             MOVE 127                    TO CRT-ERROR-NBR
P53786             MOVE PR30F1-STM-EMPLOYEE-FN TO CRT-FIELD-NBR
P53786             GO TO 264-END
P53786         ELSE
J28323         IF  ((PR30F1-STM-PAY-CODE (I1) NOT = STM-PAY-CODE)
P53786         OR   (PR30F1-STM-HOURS    (I1) NOT = STM-HOURS)
P53786         OR   (PR30F1-STM-JOB-CODE (I1) NOT = STM-JOB-CODE)
P53786         OR   (PR30F1-STM-RATE     (I1) NOT = STM-RATE)
P53786         OR   (PR30F1-STM-EFFECT-DATE (I1) NOT =       
P53786                                            STM-EFFECT-DATE)
P53786         OR   (PR30F1-STM-CURRENCY-CODE (I1) NOT = 
J28323                                            STM-CURRENCY-CODE))
J28323         AND  (PR30F1-PT-XMIT-NBR-2 = ZEROES)
J28323             MOVE 1                      TO PR30F1-PT-XMIT-NBR-2
P53786             MOVE 127                    TO CRT-ERROR-NBR
P53786             MOVE PR30F1-STM-EMPLOYEE-FN TO CRT-FIELD-NBR
P53786             GO TO 264-END
P53786         END-IF
P53786     END-IF.
029300     IF (PR30F1-STM-ATTEND-CODE (I1) NOT = SPACES)
029400         MOVE PR30F1-STM-COMPANY          TO DB-COMPANY
029500         MOVE PR30F1-STM-ATTEND-CODE (I1) TO DB-ATTEND-CODE
029600         PERFORM 840-FIND-ATCSET1
029700         IF (ATTENDCODE-NOTFOUND)
029800             MOVE 108                            TO CRT-ERROR-NBR
029900             MOVE PR30F1-STM-ATTEND-CODE-FN (I1) TO CRT-FIELD-NBR
030000             GO TO 264-END
030100         ELSE
030200         IF (PR30F1-STM-PAY-CODE (I1) = SPACES)
030300             MOVE ATC-PAY-CODE   TO PR30F1-STM-PAY-CODE (I1).
030400
030500     IF (PR30F1-STM-PAY-CODE (I1) = SPACES)
030600         MOVE 124                             TO CRT-ERROR-NBR
030700         MOVE PR30F1-STM-PAY-CODE-FN (I1)     TO CRT-FIELD-NBR
030800         GO TO 264-END.
030900
           MOVE PR30F1-STM-COMPANY             TO DB-COMPANY.
           MOVE PR30F1-STM-PAY-CODE (I1)       TO DB-PAY-CODE.
           MOVE PR30F1-STM-PROCESS-LEVEL (I1)  TO DB-PROCESS-LEVEL.
           MOVE PR30F1-STM-JOB-CODE (I1)       TO DB-JOB-CODE.
           PERFORM 840-FIND-PCDSET1.
           IF  (PRPAYCODE-NOTFOUND)
               INITIALIZE DB-PROCESS-LEVEL
               PERFORM 840-FIND-PCDSET1.
           IF  (PRPAYCODE-NOTFOUND)
               MOVE PR30F1-STM-PROCESS-LEVEL (I1)  TO DB-PROCESS-LEVEL
               INITIALIZE DB-JOB-CODE
               PERFORM 840-FIND-PCDSET1.
           IF  (PRPAYCODE-NOTFOUND)
               INITIALIZE DB-PROCESS-LEVEL
               PERFORM 840-FIND-PCDSET1.
031400     IF (PRPAYCODE-NOTFOUND)
031500         MOVE 104                                 TO CRT-ERROR-NBR
031600         MOVE PR30F1-STM-PAY-CODE-FN (I1)         TO CRT-FIELD-NBR
031700         GO TO 264-END.
031800
P80643     IF  (PRPAYCODE-FOUND)
P80643     AND (PCD-ACTIVE-FLAG = "I")
J19756*---PR30#148: Pay code {0} does not exist or is inactive
J19756         MOVE "PR30"                              TO CRT-ERROR-CAT
J19756         MOVE PR30F1-STM-PAY-CODE (I1)            TO CRT-ERR-VAR1
J19756         MOVE 148                                 TO CRT-ERROR-NBR
P80643         MOVE PR30F1-STM-PAY-CODE-FN (I1)         TO CRT-FIELD-NBR
P80643         GO TO 264-END
P80643     END-IF.
J41010
J41010     MOVE PRS-WORK-COUNTRY    TO PR30WS-WORK-COUNTRY.
J41010     IF  (NOT PR30WS-WORK-CTRY-R1)
J41010     AND (PR30F1-STM-DAILY-TR-FLG (I1) = "Y")
J41010*---PR30#213: Daily Timerecord not valid for work country {0}
J41010         MOVE 213                      TO CRT-ERROR-NBR
J41010         MOVE "PR30"                   TO CRT-ERROR-CAT
J41010         MOVE PR30F1-STM-DAILY-TR-FLG-FN (I1)
J41010                                       TO CRT-FIELD-NBR
J41010         MOVE PRS-WORK-COUNTRY         TO CRT-ERR-VAR1
J41010         GO TO 264-END
J41010     END-IF.
J41010
J41010     IF (PR30F1-STM-EMPLOYEE NOT = ZEROES)
J41010         MOVE PR30F1-STM-COMPANY       TO DB-COMPANY
J41010         MOVE PR30F1-STM-EMPLOYEE      TO DB-EMPLOYEE
J41010         PERFORM 840-FIND-EMPSET1
J41010         IF (EMPLOYEE-FOUND)
J41010         AND (PR30F1-STM-DAILY-TR-FLG (I1) = "Y")
J41010         AND ((EMP-SALARY-CLASS NOT = "S" ) 
J41010          OR  (EMP-EXEMPT-EMP NOT = "Y"))
J41010*---PR30#216: Daily time records only valid for Salaried, Exempt 
J41010*             from OT
J41010             MOVE 216                      TO CRT-ERROR-NBR
J41010             MOVE "PR30"                   TO CRT-ERROR-CAT
J41010             MOVE PR30F1-STM-EMPLOYEE-FN   TO CRT-FIELD-NBR
J41010             GO TO 264-END
J41010         END-IF
J41010     END-IF.
J41010
J41010     IF  (PCD-CALC-TYPE NOT = "L" AND "R" AND "A" AND "F" AND     
J41010                              "P" AND "D" AND "S" AND "Q" AND
J41010                              "B" AND "N" AND "M" )
J41010     AND (PR30F1-STM-DAILY-TR-FLG (I1) = "Y")
J41010*---PR30#214: Daily Timerecord not valid for Pay Code Calc Type
J41010*             {0}
J41010         MOVE 214                      TO CRT-ERROR-NBR
J41010         MOVE "PR30"                   TO CRT-ERROR-CAT
J41010         MOVE PR30F1-STM-DAILY-TR-FLG-FN(I1)
J41010                                       TO CRT-FIELD-NBR
J41010         MOVE PCD-CALC-TYPE            TO CRT-ERR-VAR1
J41010         GO TO 264-END
J41010     END-IF.
J41010
J41010     IF  (PCD-CALC-TYPE = "L" OR "R" OR "A" OR "F" OR "P" OR 
J41010                          "D" OR "S" OR "Q" OR "B" OR "N" OR "M" )
J41010     AND (PR30F1-STM-DAILY-TR-FLG (I1) = "Y")
J41010     AND (PR30F1-STM-HOURS (I1) NOT = ZEROES)
J41010*---PR30#215: Daily Timerecord = Y; hours must be blank
J41010         MOVE 215                      TO CRT-ERROR-NBR
J41010         MOVE "PR30"                   TO CRT-ERROR-CAT
J41010         MOVE PR30F1-STM-HOURS-FN (I1) TO CRT-FIELD-NBR
J41010         GO TO 264-END
J41010     END-IF.
J41010
           IF  (PCD-CALC-TYPE = "C")
               IF  (PR30F1-STM-HOURS (I1) NOT = ZEROES)
                   MOVE 208                            TO CRT-ERROR-NBR
                   MOVE PR30F1-STM-HOURS-FN (I1)       TO CRT-FIELD-NBR
                   GO TO 264-END
               END-IF
               IF  (PR30F1-STM-RATE (I1)  NOT = ZEROES)
                   MOVE 210                            TO CRT-ERROR-NBR
                   MOVE PR30F1-STM-RATE-FN (I1)        TO CRT-FIELD-NBR
                   GO TO 264-END
               END-IF
               GO TO 264-CONTINUE-CALC-TYPE
           END-IF.

031900     IF  (PR30F1-STM-HOURS (I1) = ZEROS)
032000     AND (PR30F1-STM-RATE (I1)  = ZEROS)
J59938*    AND (PR30F1-LINE-FC (I1)   = "A")
J59938     AND (PR30F1-LINE-FC (I1)   = "A" OR "C")
J41010     AND (NOT PR30WS-WORK-CTRY-R1)
J59938*    AND (PCD-CALC-TYPE     NOT = "F")
J59938         IF  ((PCD-CALC-TYPE    = "F")
J59938         AND  (PCD-NORM-RATE    = ZEROS))
J59938         OR  (PCD-CALC-TYPE NOT = "F")
J41321*---PR30#113: Type either hours or amount              
J41321             MOVE "PR30"                     TO CRT-ERROR-CAT
032200             MOVE 113                        TO CRT-ERROR-NBR
032300             MOVE PR30F1-STM-HOURS-FN (I1)   TO CRT-FIELD-NBR
032400             GO TO 264-END 
J59938         ELSE
J59938         IF  (PCD-CALC-TYPE = "F")
J59938         AND (PCD-NORM-RATE NOT = ZEROS)
J59938             MOVE PCD-NORM-RATE          TO PR30F1-STM-RATE(I1)
J59938         END-IF
J59938         END-IF
J41321     END-IF.
J59938
032600     IF  (PR30F1-STM-EMPLOYEE NOT = ZEROS)
032700     AND ((PR30F1-STM-RATE (I1)   = ZEROS)
032800     OR   (PR30F1-STM-HOURS (I1)  = ZEROS))
032900         MOVE PCD-PAY-SUM-GRP        TO DB-PAY-SUM-GRP
033000         MOVE PR30WS-TIPS-CLASS      TO DB-PAY-CLASS
033100         PERFORM 840-FIND-PSRSET2
033200         IF  (PSGRELATE-FOUND)
033300             IF  (PR30F1-STM-RATE (I1) = ZEROS)
J41321*---PR30#119: Type rate    
J41321                 MOVE "PR30"                 TO CRT-ERROR-CAT
033400                 MOVE 119                    TO CRT-ERROR-NBR
033500                 MOVE PR30F1-STM-RATE-FN(I1) TO CRT-FIELD-NBR
033600                 GO TO 264-END
033700             ELSE
033800             IF  (PR30F1-STM-HOURS (I1) = ZEROS)
J41321*---PR30#120: Type hours for tip pay code
J41321                 MOVE "PR30"                 TO CRT-ERROR-CAT
033900                 MOVE 120                    TO CRT-ERROR-NBR
J41321                 MOVE PR30F1-STM-HOURS-FN(I1)                     
J41321                                             TO CRT-FIELD-NBR
034100                 GO TO 264-END
034200             END-IF
034300             END-IF
034400         ELSE
J59938         IF  (PCD-CALC-TYPE        = "F")
J59938         AND (PR30F1-STM-RATE (I1) = ZEROES)
J59938         AND (((PR30WS-WORK-CTRY-R1) 
J59938         AND   (PR30F1-STM-DAILY-TR-FLG (I1) NOT = "Y"))
J59938         OR   (NOT PR30WS-WORK-CTRY-R1))
J59938             IF  (PCD-NORM-RATE    = ZEROS)
J59938*---PR30#119: Type rate
J59938                 MOVE "PR30"                 TO CRT-ERROR-CAT
J59938                 MOVE 119                    TO CRT-ERROR-NBR
J59938                 MOVE PR30F1-STM-RATE-FN (I1)
J59938                                             TO CRT-FIELD-NBR
J59938                 GO TO 264-END
J59938             ELSE
J59938                 MOVE PCD-NORM-RATE      TO PR30F1-STM-RATE(I1)
J59938             END-IF
J59938         ELSE
J59938         IF  (PCD-CALC-TYPE NOT    = "F")
J59938         AND (PR30F1-STM-RATE (I1) = ZEROS)
J59938         AND (PR30F1-STM-HOURS(I1) = ZEROS)
J59938         AND (PR30WS-WORK-CTRY-R1)
J59938         AND (PR30F1-STM-DAILY-TR-FLG (I1) NOT = "Y")
J59938*---PR30#113: Type either hours or amount
J59938             MOVE "PR30"                     TO CRT-ERROR-CAT
J59938             MOVE 113                        TO CRT-ERROR-NBR
J59938             MOVE PR30F1-STM-RATE-FN (I1)    TO CRT-FIELD-NBR
J59938             GO TO 264-END
J41321         END-IF
J41321         END-IF
J41321         END-IF 
J41321     END-IF.
035300
035400     IF  (PCD-CALC-TYPE         = "H")
035500     AND (PR30F1-STM-RATE (I1)  NOT = ZEROS)
035600         MOVE 121                             TO CRT-ERROR-NBR
035700         MOVE PR30F1-STM-RATE-FN   (I1)       TO CRT-FIELD-NBR
035800         GO TO 264-END.
035900
       264-CONTINUE-CALC-TYPE.

036000     IF (PR30F1-STM-JOB-CODE (I1) NOT = SPACES)
036100         MOVE PR30F1-STM-COMPANY       TO DB-COMPANY
036200         MOVE PR30F1-STM-JOB-CODE (I1) TO DB-JOB-CODE
036300         PERFORM 840-FIND-JBCSET1
036400         IF (JOBCODE-NOTFOUND)
036500             MOVE 107                         TO CRT-ERROR-NBR
036600             MOVE PR30F1-STM-JOB-CODE-FN (I1) TO CRT-FIELD-NBR
036700             GO TO 264-END.
036800
036900     IF (PR30F1-STM-EFFECT-DATE (I1) = ZEROS)
037000         MOVE WS-SYSTEM-DATE-YMD TO PR30F1-STM-EFFECT-DATE (I1).
037100
037200     IF  (PR30F1-STM-END-DATE (I1) NOT = ZEROS)
037300     AND (PR30F1-STM-END-DATE (I1) < PR30F1-STM-EFFECT-DATE (I1))
037400         MOVE 122                                TO CRT-ERROR-NBR
037500         MOVE PR30F1-STM-END-DATE-FN(I1)         TO CRT-FIELD-NBR
037600         GO TO 264-END.
037700
037800     IF (PR30F1-STM-POSITION (I1) NOT = SPACES)
037900         MOVE PR30F1-STM-COMPANY             TO DB-COMPANY
038000         MOVE PPRSET1-COMPANY                TO WS-DB-BEG-RNG
038100         PERFORM 850-KFIND-BEGRNG-PPRSET1
038200         IF  (PAPOSRULE-KNOTFOUND)
038300             MOVE 135                        TO CRT-ERROR-NBR
038400             MOVE PR30F1-STM-POSITION-FN (I1)
038500                                             TO CRT-FIELD-NBR
038600             GO TO 264-END
038700         END-IF
038800         MOVE PR30F1-STM-POSITION (I1)       TO DB-POSITION
038900         MOVE POSSET1-POSITION               TO WS-DB-BEG-RNG
039000         PERFORM 850-FIND-BEGRNG-POSSET1
039100         IF (PAPOSITION-NOTFOUND)
039200             MOVE 133                        TO CRT-ERROR-NBR
039300             MOVE PR30F1-STM-POSITION-FN (I1)
039400                                             TO CRT-FIELD-NBR
039500             GO TO 264-END
039600         ELSE
039700             MOVE PR30F1-STM-COMPANY         TO PAPOS-COMPANY
039800             MOVE PR30F1-STM-POSITION (I1)   TO PAPOS-POSITION
039900             MOVE PR30F1-STM-EFFECT-DATE (I1)
040000                                             TO PAPOS-EFFECT-DATE
040100             MOVE PR30F1-STM-END-DATE (I1)   TO PAPOS-END-DATE
040200             MOVE 1                          TO ERROR-FLAG-SW
040300             PERFORM 2000-EDIT-POSITION-DATES
040400             IF (NO-REC)
P01416                 MOVE "PR30"                 TO CRT-ERROR-CAT
040500                 MOVE 134                    TO CRT-ERROR-NBR
040600                 MOVE PR30F1-STM-POSITION-FN (I1)
040700                                             TO CRT-FIELD-NBR
040800                 GO TO 264-END.
040900
J13588*    IF  (PR30F1-STM-REASON-CODE (I1) NOT = SPACES)
J13588*    AND (NOT EDCDWS-TRIGGER-ENABLED)
J13588*        MOVE 150                             TO CRT-ERROR-NBR
J13588*        MOVE PR30F1-STM-REASON-CODE-FN (I1)  TO CRT-FIELD-NBR
J13588*        GO TO 264-END.
              
LP         IF  (PR30F1-STM-REASON-CODE (I1) NOT = SPACES)
               MOVE PR30F1-STM-COMPANY          TO DB-COMPANY
               MOVE PR30F1-STM-REASON-CODE (I1) TO DB-REASON-CODE 
               PERFORM 840-FIND-TRESET1
              IF (TAREASCODE-NOTFOUND)
                 MOVE 151                            TO CRT-ERROR-NBR
                 MOVE TRE-REASON-CODE                TO CRT-ERR-VAR1
                 MOVE PR30F1-STM-REASON-CODE-FN (I1) TO CRT-FIELD-NBR
                 GO TO 264-END
              ELSE
              IF (TRE-STATUS = 1)
                 MOVE 156                            TO CRT-ERROR-NBR
                 MOVE TRE-REASON-CODE                TO CRT-ERR-VAR1
                 MOVE PR30F1-STM-REASON-CODE-FN (I1) TO CRT-FIELD-NBR
                 GO TO 264-END
              END-IF
              END-IF
           END-IF.

LP         IF (PCD-SERVICE-CODE        NOT = SPACES)
J13588*    AND (EDCDWS-TRIGGER-ENABLED)
              MOVE PR30F1-STM-COMPANY          TO DB-COMPANY
              MOVE PCD-SERVICE-CODE            TO DB-SERVICE-CODE
              PERFORM 840-FIND-TSCSET1
              IF (TASERVCODE-NOTFOUND)
                 MOVE 152                            TO CRT-ERROR-NBR
                 MOVE PCD-SERVICE-CODE               TO CRT-ERR-VAR1
                 MOVE PR30F1-STM-REASON-CODE-FN (I1) TO CRT-FIELD-NBR
                 GO TO 264-END. 

LP         IF  (PR30F1-STM-REASON-CODE (I1) = SPACES)
J13588*    AND (EDCDWS-TRIGGER-ENABLED)
           AND (PCD-SERVICE-CODE        NOT = SPACES)
               IF (TSC-EVENT = 1)
                  MOVE TSC-REASON-CODE TO PR30F1-STM-REASON-CODE (I1)   
               END-IF
           END-IF.

LP         IF  (PR30F1-STM-REASON-CODE (I1) NOT = SPACES)
               IF (PCD-SERVICE-CODE             = SPACES)
                  MOVE 154                            TO CRT-ERROR-NBR
                  MOVE PR30F1-STM-REASON-CODE-FN (I1) TO CRT-FIELD-NBR
                  GO TO 264-END
               ELSE
               IF (TSC-EVENT = 0)
                  MOVE 154                            TO CRT-ERROR-NBR
                  MOVE PR30F1-STM-REASON-CODE-FN (I1) TO CRT-FIELD-NBR
                  GO TO 264-END
               END-IF
               END-IF
           END-IF.

041000     IF  (PR30F1-STM-OCCURRENCE (I1)  NOT = SPACES)
041100     AND (PR30F1-STM-ATTEND-CODE (I1) = SPACES)
LP         AND (PR30F1-STM-REASON-CODE (I1) = SPACES)
041200         MOVE 123                             TO CRT-ERROR-NBR
041300         MOVE PR30F1-STM-OCCURRENCE-FN (I1)   TO CRT-FIELD-NBR
041400         GO TO 264-END.
041500
041600     IF  (PR30F1-STM-OCCURRENCE (I1)  = SPACES)
041700     AND ((PR30F1-STM-ATTEND-CODE (I1) NOT = SPACES)
LP         OR   (PR30F1-STM-REASON-CODE (I1) NOT = SPACES))
041800         MOVE "Y"                TO PR30F1-STM-OCCURRENCE (I1).
041900
           IF  (PR30F1-STM-LOCAT-CODE (I1) NOT =
                SPACES                     AND
                PRPXL-PENSION-LOCAT-CODE1  AND
                PRPXL-PENSION-LOCAT-CODE2)
               MOVE "LO"                           TO DB-TYPE
               MOVE PR30F1-STM-LOCAT-CODE (I1)     TO DB-CODE
               PERFORM 840-FIND-PCOSET1
               IF  (PCODES-NOTFOUND)
                   MOVE 137                            TO CRT-ERROR-NBR
                   MOVE PR30F1-STM-LOCAT-CODE-FN (I1)  TO CRT-FIELD-NBR
                   GO TO 264-END
               END-IF
           END-IF.

042000     IF (PR30F1-LINE-FC (I1) = "C")
042100         IF (STM-FLEX-DOLLARS NOT = ZEROS)
042200             IF (PR30F1-STM-HOURS (I1) NOT = STM-HOURS)
042300                 MOVE 131                        TO CRT-ERROR-NBR
042400                 MOVE PR30F1-STM-HOURS-FN (I1)   TO CRT-FIELD-NBR
042500                 GO TO 264-END
042600             END-IF
042700             IF (PR30F1-STM-EFFECT-DATE (I1)
042800                                       NOT = STM-EFFECT-DATE)
042900                 MOVE 131                        TO CRT-ERROR-NBR
043000                 MOVE PR30F1-STM-EFFECT-DATE-FN (I1)
043100                                                 TO CRT-FIELD-NBR
043200                 GO TO 264-END
043300             END-IF
043400             IF (PR30F1-STM-END-DATE (I1) NOT = STM-END-DATE)
043500                 MOVE 131                        TO CRT-ERROR-NBR
043600                 MOVE PR30F1-STM-END-DATE-FN (I1)
043700                                                 TO CRT-FIELD-NBR
043800                 GO TO 264-END.
043900
044000     IF (PR30F1-LINE-FC (I1) = "A")
044100         IF  (PR30F1-STM-DED-CYCLE1 (I1) = SPACE)
044200         AND (PR30F1-STM-DED-CYCLE2 (I1) = SPACE)
044300         AND (PR30F1-STM-DED-CYCLE3 (I1) = SPACE)
044400         AND (PR30F1-STM-DED-CYCLE4 (I1) = SPACE)
044500         AND (PR30F1-STM-DED-CYCLE5 (I1) = SPACE)
044600         AND (PR30F1-STM-DED-CYCLE6 (I1) = SPACE)
044700         AND (PR30F1-STM-DED-CYCLE7 (I1) = SPACE)
044800         AND (PR30F1-STM-DED-CYCLE8 (I1) = SPACE)
044900         AND (PR30F1-STM-DED-CYCLE9 (I1) = SPACE)
045000             MOVE "X"            TO PR30F1-STM-DED-CYCLE1 (I1)
045100             MOVE "X"            TO PR30F1-STM-DED-CYCLE2 (I1)
045200             MOVE "X"            TO PR30F1-STM-DED-CYCLE3 (I1)
045300             MOVE "X"            TO PR30F1-STM-DED-CYCLE4 (I1)
045400             MOVE "X"            TO PR30F1-STM-DED-CYCLE5 (I1)
045500             MOVE "X"            TO PR30F1-STM-DED-CYCLE6 (I1)
045600             MOVE "X"            TO PR30F1-STM-DED-CYCLE7 (I1)
045700             MOVE "X"            TO PR30F1-STM-DED-CYCLE8 (I1)
045800             MOVE "X"            TO PR30F1-STM-DED-CYCLE9 (I1).
045900
046300     MOVE "PR"                   TO IFACWS-SYSTEM.
      *----ifacws-employee is required for pr editing in ifac.
      *----used to validate if an employee is assigned to an activity.
P06555     MOVE PR30F1-STM-EMPLOYEE    TO IFACWS-EMPLOYEE.
046400
046500     IF (PR30F1-STM-DST-ACCT-UNIT (I1) NOT = SPACES)
P37747     AND (PR30F1-STM-DST-ACCOUNT (I1)   NOT = ZEROES)
046700        IF   (PR30F1-STM-ACTIVITY (I1)  NOT = SPACES)
046800        OR  ((PR30F1-STM-ACTIVITY (I1)      = SPACES)
046900        AND  (PR30F1-STM-ACCT-CATEGORY (I1) = SPACES))
               IF (PR30F1-STM-DIST-COMPANY (I1) NOT = ZEROES)
047000             MOVE PR30F1-STM-DIST-COMPANY (I1)  TO IFACWS-COMPANY
               ELSE
                   MOVE PR30F1-STM-COMPANY        TO IFACWS-COMPANY
               END-IF
047100         MOVE PR30F1-STM-DST-ACCT-UNIT (I1) TO IFACWS-ACCT-UNIT
047200         MOVE PR30F1-STM-DST-ACCOUNT (I1)   TO IFACWS-ACCOUNT
047300         MOVE PR30F1-STM-DST-SUB-ACCT (I1)  TO IFACWS-SUB-ACCOUNT
047400         MOVE PR30F1-STM-ACTIVITY      (I1) TO IFACWS-ACTIVITY
047500         MOVE PR30F1-STM-ACCT-CATEGORY (I1)
047600                                           TO IFACWS-ACCT-CATEGORY
047700         IF (PR30F1-STM-DST-ACCT-UNIT (I1)  NOT = SPACES)
047800             IF (PR30F1-STM-ACTIVITY (I1)       = SPACES)
P06555*------------if adding emp group and activity is filled in, can not do 
P06555*------------resource edit in acac because it is done by ee.
P06555*------------pr134 will catch any ee not assigned to activity.
P06555             OR ((PR30F1-STM-ACTIVITY (I1)  NOT = SPACES)
P06555             AND (PR30F1-STM-TIME-GROUP     NOT = SPACES))
047900                MOVE ZERO                  TO IFACWS-EDIT-TYPE
P08896                MOVE 2                     TO IFACWS-EDIT-CODE
048000             ELSE
048100                MOVE 2                     TO IFACWS-EDIT-TYPE
P08896                MOVE 2                     TO IFACWS-EDIT-CODE
048200             END-IF
048300         ELSE
048400             IF (PR30F1-STM-ACTIVITY (I1)       = SPACES)
P06555             OR ((PR30F1-STM-ACTIVITY (I1)  NOT = SPACES)
P06555             AND (PR30F1-STM-TIME-GROUP     NOT = SPACES))
048500                MOVE 1                     TO IFACWS-EDIT-TYPE
P08896                MOVE 2                     TO IFACWS-EDIT-CODE
048600             ELSE
048700                MOVE 3                     TO IFACWS-EDIT-TYPE
P08896                MOVE 2                     TO IFACWS-EDIT-CODE
                      MOVE "Y"             TO IFACWS-BYPASS-SEG-EDIT
048800             END-IF
048900         END-IF
P43667
P43667         INITIALIZE IFACWS-GLT-TYPE
P43667                    IFACWS-BYPASS-UNITS-EDIT
P43667         IF  (PR30WS-GL-UNITS = "Y")
P43667             IF  (PCD-CALC-TYPE = "H")
P43667                 MOVE "U"            TO IFACWS-GLT-TYPE
P43667             ELSE
P43667                 IF  (PR30F1-STM-HOURS (I1) NOT = ZEROS)
P43667                     MOVE "B"        TO IFACWS-GLT-TYPE
P43667                 ELSE
P43667                     MOVE "A"        TO IFACWS-GLT-TYPE
P43667                 END-IF
P43667             END-IF
P43667         ELSE
P43667             IF  (PR30WS-GL-UNITS = "N")
P43667                 IF  (PCD-CALC-TYPE = "H")
P43667                     MOVE "U"        TO IFACWS-GLT-TYPE
P43667                     MOVE "Y"        TO IFACWS-BYPASS-UNITS-EDIT
P43667                 ELSE
P43667                     MOVE "A"        TO IFACWS-GLT-TYPE
P43667                 END-IF
P43667             END-IF
P43667         END-IF
P43667
               MOVE PR30F1-STM-COMPANY TO IFACWS-FROM-COMPANY
049600         PERFORM 635-EDIT-GLMASTER-60
049700         IF (ERROR-FOUND)
049800             IF (IFACWS-CURSOR-POSITION = "C")
049900                 MOVE PR30F1-STM-DIST-COMPANY-FN (I1)
050000                                                  TO CRT-FIELD-NBR
050100                 GO TO 264-END
050200             ELSE
050300                 IF (IFACWS-CURSOR-POSITION = "A")
050400                   IF (PR30F1-STM-DST-ACCT-UNIT (I1) NOT = SPACES)
050500                       MOVE PR30F1-STM-DST-ACCT-UNIT-FN (I1)
050600                                                  TO CRT-FIELD-NBR
050700                       GO TO 264-END
050800                   ELSE
050900                       MOVE PR30F1-STM-DST-ACCOUNT-FN (I1)
051000                                                  TO CRT-FIELD-NBR
051100                       GO TO 264-END
051200                   END-IF
051300                 ELSE
051400                   IF (IFACWS-CURSOR-POSITION = "X")
051500                       MOVE PR30F1-STM-ACTIVITY-FN (I1)
051600                                                  TO CRT-FIELD-NBR
051700                       GO TO 264-END
051800                   ELSE
051900                       MOVE PR30F1-STM-ACCT-CATEGORY-FN (I1)
052000                                                  TO CRT-FIELD-NBR
052100                       GO TO 264-END
052200                   END-IF
052300                 END-IF
052400             END-IF
052500         ELSE
052600             MOVE IFACWS-ACCT-CATEGORY
052700                                  TO PR30F1-STM-ACCT-CATEGORY (I1)
052800         END-IF
052900        END-IF
053000        IF  (PR30F1-STM-ACTIVITY      (I1)     = SPACES)
053100        AND (PR30F1-STM-ACCT-CATEGORY (I1) NOT = SPACES)
                 IF (PR30F1-STM-DIST-COMPANY (I1) NOT = ZEROES)
                     MOVE PR30F1-STM-DIST-COMPANY (I1) TO IFACWS-COMPANY
                 ELSE
053200               MOVE PR30F1-STM-COMPANY       TO IFACWS-COMPANY
                 END-IF
053300           MOVE PR30F1-STM-DST-ACCT-UNIT (I1) TO IFACWS-ACCT-UNIT
053400           MOVE PR30F1-STM-DST-ACCOUNT  (I1) TO IFACWS-ACCOUNT
053500           MOVE PR30F1-STM-DST-SUB-ACCT (I1) TO IFACWS-SUB-ACCOUNT
053600           MOVE SPACES                   TO IFACWS-ACTIVITY
053700                                            IFACWS-ACCT-CATEGORY
053800            IF  (PR30F1-STM-DST-ACCT-UNIT (I1) NOT = SPACES)
053900            AND (PR30F1-STM-DST-ACCOUNT   (I1) NOT = ZEROES)
054000                MOVE 0                    TO IFACWS-EDIT-TYPE
P08896                MOVE 2                     TO IFACWS-EDIT-CODE
054100            ELSE
054200                MOVE 1                    TO IFACWS-EDIT-TYPE
P08896                MOVE 2                     TO IFACWS-EDIT-CODE
054300            END-IF
                  MOVE PR30F1-STM-COMPANY TO IFACWS-FROM-COMPANY
054400            PERFORM 635-EDIT-GLMASTER-60
054500            IF (ERROR-FOUND)
054600               IF (CRT-ERROR-CAT     = "EDAU")
054700                   MOVE PR30F1-STM-DST-ACCT-UNIT-FN (I1)
054800                                           TO CRT-FIELD-NBR
054900                   GO TO 264-END
055000               ELSE
055100                   MOVE PR30F1-STM-DST-ACCOUNT-FN (I1)
055200                                           TO CRT-FIELD-NBR
055300                   GO TO 264-END
055400               END-IF
055500            ELSE
055600               MOVE PR30F1-STM-ACCT-CATEGORY (I1)
055700                                      TO ACCTWS-ACCT-CATEGORY
055800               PERFORM 615-EDIT-ACCT-CAT-70
055900               IF (ERROR-FOUND)
056000                   MOVE 132           TO CRT-ERROR-NBR
056100                   MOVE PR30F1-STM-ACCT-CATEGORY-FN (I1)
056200                                      TO CRT-FIELD-NBR
056300                   GO TO 264-END
056400               END-IF
056500            END-IF
056600        END-IF
056700     ELSE
056800     IF  (PR30F1-STM-DST-ACCT-UNIT (I1)     = SPACES)
056900     AND (PR30F1-STM-DST-ACCOUNT (I1)       = ZEROES)
057000         IF (PR30F1-STM-ACTIVITY (I1)      NOT = SPACES)
057100             MOVE PR30F1-STM-ACTIVITY (I1) TO ACACWS-ACTIVITY
057200             MOVE PR30F1-STM-ACCT-CATEGORY (I1)
057300                                           TO ACACWS-ACCT-CATEGORY
P08896             MOVE 2                        TO ACACWS-EDIT-CODE
                   MOVE "N"                      TO ACACWS-CURRENCY-OR
                                                    ACACWS-RESOURCE-OR
                                                    ACACWS-BUDGET-OR
                                                    ACACWS-GL-OR
057600             PERFORM 640-EDIT-ACTIVITY-70
057700             IF (ERROR-FOUND)
057800                IF (ACACWS-CURSOR-POSITION = "X")
057900                    MOVE PR30F1-STM-ACTIVITY-FN (I1)
058000                                           TO CRT-FIELD-NBR
058100                    GO TO 264-END
058200                ELSE
058300                    MOVE PR30F1-STM-ACCT-CATEGORY-FN (I1)
058400                                           TO CRT-FIELD-NBR
058500                    GO TO 264-END
058600                END-IF
058700             ELSE
058800                MOVE ACACWS-ACCT-CATEGORY
058900                                  TO PR30F1-STM-ACCT-CATEGORY (I1)
059000             END-IF
059100         ELSE
059200             IF (PR30F1-STM-ACCT-CATEGORY (I1) NOT = SPACES)
059300                MOVE PR30F1-STM-ACCT-CATEGORY (I1)
059400                                       TO ACCTWS-ACCT-CATEGORY
059500                PERFORM 615-EDIT-ACCT-CAT-70
059600                IF (ERROR-FOUND)
059700                    MOVE 132           TO CRT-ERROR-NBR
059800                    MOVE PR30F1-STM-ACCT-CATEGORY-FN (I1)
059900                                       TO CRT-FIELD-NBR
060000                    GO TO 264-END
060100                END-IF
060200             END-IF
060300         END-IF
060400     END-IF
060500     END-IF.
060600
060700     IF  (PR30F1-STM-DIST-COMPANY (I1) NOT = PR30F1-STM-COMPANY)
           AND (PR30F1-STM-DIST-COMPANY (I1) NOT = ZEROES)
060800         MOVE PR30F1-STM-COMPANY             TO IFICWS-COMPANY
060900         MOVE PR30F1-STM-DIST-COMPANY (I1)   TO IFICWS-TO-COMPANY
061000         MOVE "PR"                           TO IFICWS-SYSTEM
061100         PERFORM 600-EDIT-INTERCOMPANY-60
061200         IF (ERROR-FOUND)
061300             MOVE PR30F1-STM-DIST-COMPANY-FN (I1) TO CRT-FIELD-NBR
061400             GO TO 264-END.
061500
061600     MOVE PR30F1-STM-COMPANY     TO DB-COMPANY.
061700
061800     IF (PR30F1-STM-PROCESS-LEVEL (I1) NOT = SPACES)
061900         MOVE PR30F1-STM-COMPANY            TO DB-COMPANY
062000         MOVE PR30F1-STM-PROCESS-LEVEL (I1) TO DB-PROCESS-LEVEL
062100         PERFORM 840-FIND-PRSSET1
062200         IF (PRSYSTEM-NOTFOUND)
062300             MOVE 111                            TO CRT-ERROR-NBR
062400             MOVE PR30F1-STM-PROCESS-LEVEL-FN (I1)
062500                                                 TO CRT-FIELD-NBR
062600             GO TO 264-END
               ELSE
               IF  (PRS-ACTIVE-FLAG = "I")
                   MOVE 146                            TO CRT-ERROR-NBR
                   MOVE PR30F1-STM-PROCESS-LEVEL-FN (I1)
                                                       TO CRT-FIELD-NBR
                   GO TO 264-END.
062700
062800     IF (PR30F1-STM-DEPARTMENT (I1) NOT = SPACES)
062900         MOVE PR30F1-STM-COMPANY            TO DB-COMPANY
063000         MOVE PR30F1-STM-PROCESS-LEVEL (I1) TO DB-PROCESS-LEVEL
063100         MOVE PR30F1-STM-DEPARTMENT (I1)    TO DB-DEPARTMENT
063200         PERFORM 840-FIND-DPTSET1
063300         IF (DEPTCODE-NOTFOUND)
063400             MOVE 103                            TO CRT-ERROR-NBR
063500             MOVE PR30F1-STM-DEPARTMENT-FN (I1)  TO CRT-FIELD-NBR
063600             GO TO 264-END
               ELSE
               IF  (DPT-ACTIVE-FLAG = "I")
                   MOVE 147                            TO CRT-ERROR-NBR
                   MOVE PR30F1-STM-DEPARTMENT-FN (I1)
                                                       TO CRT-FIELD-NBR
                   GO TO 264-END.

           IF (PR30F1-STM-BUS-NBR-GRP (I1) NOT = SPACES)
               IF (PR30F1-STM-EMPLOYEE NOT = ZEROES)
                   MOVE EMP-PROCESS-LEVEL            TO DB-PROCESS-LEVEL
                   MOVE PR30F1-STM-BUS-NBR-GRP (I1)  TO DB-BUS-NBR-GRP
                   PERFORM 840-KFIND-PBGSET1
                   IF (PRBUSGRP-KNOTFOUND)
                       MOVE 138                      TO CRT-ERROR-NBR
                       MOVE PR30F1-STM-BUS-NBR-GRP-FN (I1)
                                                     TO CRT-FIELD-NBR
                       GO TO 264-END
                   END-IF
               ELSE
                   MOVE PR30F1-STM-BUS-NBR-GRP (I1)  TO DB-BUS-NBR-GRP
                   MOVE PBGSET2-BUS-NBR-GRP          TO WS-DB-BEG-RNG
                   PERFORM 850-KFIND-BEGRNG-PBGSET2
                   IF (PRBUSGRP-KNOTFOUND)
                       MOVE 138                      TO CRT-ERROR-NBR
                       MOVE PR30F1-STM-BUS-NBR-GRP-FN (I1)
                                                     TO CRT-FIELD-NBR
                       GO TO 264-END.

           IF (PR30F1-STM-QC-ENT-NBR-GRP (I1) NOT = SPACES)
               IF (PR30F1-STM-EMPLOYEE NOT = ZEROES)
                   MOVE EMP-PROCESS-LEVEL           TO DB-PROCESS-LEVEL
                   MOVE PR30F1-STM-QC-ENT-NBR-GRP (I1)
                                                    TO DB-QC-ENT-NBR-GRP
                   PERFORM 840-KFIND-PQCSET1
                   IF (PRQCENTGRP-KNOTFOUND)
                       MOVE 139                     TO CRT-ERROR-NBR
                       MOVE PR30F1-STM-QC-ENT-NBR-GRP-FN (I1)
                                                    TO CRT-FIELD-NBR
                       GO TO 264-END
                   END-IF
               ELSE
                   MOVE PQCSET1-COMPANY             TO WS-DB-BEG-RNG
                   PERFORM 850-KFIND-BEGRNG-PQCSET1
                   PERFORM 860-KFIND-NXTRNG-PQCSET1
                      UNTIL (PRQCENTGRP-KNOTFOUND)
                      OR    (PQC-QC-ENT-NBR-GRP
                                       = PR30F1-STM-QC-ENT-NBR-GRP (I1))
                   IF (PRQCENTGRP-KNOTFOUND)
                       MOVE 139                     TO CRT-ERROR-NBR
                       MOVE PR30F1-STM-QC-ENT-NBR-GRP-FN (I1)
                                                    TO CRT-FIELD-NBR
                       GO TO 264-END.

      * Amount does not exist, currency cannot be entered
           IF  (PR30F1-STM-CURRENCY-CODE (I1) NOT = SPACES)
           AND (PR30F1-STM-RATE (I1) = ZEROES)
               MOVE PR30F1-STM-CURRENCY-CODE-FN (I1)
                                        TO CRT-FIELD-NBR
               MOVE 143                 TO CRT-ERROR-NBR
               GO TO 264-END.


      * Employee currency must be used
           IF (PR30F1-STM-EMPLOYEE NOT = ZEROES)
           AND (PR30F1-STM-RATE (I1) NOT = ZEROES)
               IF  (PR30F1-STM-CURRENCY-CODE (I1) NOT = SPACES)
               AND (PR30F1-STM-CURRENCY-CODE (I1)
                                             NOT = EMP-CURRENCY-CODE)
                    IF (PR30F1-PT-XMIT-NBR (I1) NOT = 1)
                        MOVE PR30F1-STM-CURRENCY-CODE-FN (I1)
                                         TO CRT-FIELD-NBR
                        MOVE 141         TO CRT-ERROR-NBR
                        MOVE EMP-CURRENCY-CODE
                                         TO CRT-ERR-VAR1
                        MOVE 1
                                         TO PR30F1-PT-XMIT-NBR (I1)
                        GO TO 264-END
                    END-IF
               ELSE
               IF  (PR30F1-STM-CURRENCY-CODE (I1) = SPACES)
               AND (PR30F1-STM-RATE (I1) NOT = ZEROES)
                   MOVE EMP-CURRENCY-CODE
                                     TO PR30F1-STM-CURRENCY-CODE (I1)
                   MOVE EMP-CURR-ND  TO PR30F1-STM-CURR-ND (I1)
               END-IF
           END-IF.

      * Currency must be entered
           IF  (PR30F1-STM-CURRENCY-CODE (I1) = SPACES)
           AND (PR30F1-STM-RATE (I1) NOT = ZEROES)
               IF (PR30F1-FC = "A")
P95823         AND (PR30F1-STM-EMPLOYEE NOT = ZEROES)
                   MOVE EMP-CURRENCY-CODE
                                     TO PR30F1-STM-CURRENCY-CODE (I1)
                   MOVE EMP-CURR-ND  TO PR30F1-STM-CURR-ND (I1)
               ELSE
                   MOVE PR30F1-STM-CURRENCY-CODE-FN (I1)
                                            TO CRT-FIELD-NBR
                   MOVE 142                 TO CRT-ERROR-NBR
                   GO TO 264-END
               END-IF
           END-IF.

      * Currency code does not exist
           IF  (PR30F1-STM-CURRENCY-CODE (I1) NOT = SPACES)
                   MOVE PR30F1-STM-CURRENCY-CODE (I1)
                                            TO IFCCWS-CURRENCY-CODE
                   PERFORM 605-EDIT-CURRENCY-CODE-60
                   IF (ERROR-FOUND)
                       MOVE PR30F1-STM-CURRENCY-CODE-FN (I1)
                                            TO CRT-FIELD-NBR
                       MOVE 140             TO CRT-ERROR-NBR
                       GO TO 264-END
                   ELSE
                       MOVE IFCCWS-CURRENCY-ND
                                            TO PR30F1-STM-CURR-ND (I1).

 LP        IF  (PCD-SERVICE-CODE  NOT = SPACES)
           AND (TSC-CURRENCY-CODE NOT = SPACES)
J13588*    AND (EDCDWS-TRIGGER-ENABLED)
           AND (PR30F1-STM-CURRENCY-CODE (I1) NOT = SPACES)
               IF (TSC-CURRENCY-CODE
                                  NOT = PR30F1-STM-CURRENCY-CODE (I1))
                  MOVE 155                     TO CRT-ERROR-NBR
                  MOVE PR30F1-STM-CURRENCY-CODE-FN (I1)
                                               TO CRT-FIELD-NBR
                  GO TO 264-END.

      * If a tax frequency is entered, process group is required
           IF  (PR30F1-STM-TAX-FREQ-OVER (I1) > ZEROES)
           AND (PR30F1-STM-PROCESS-GRP   (I1) = SPACES)
               MOVE 118                     TO CRT-ERROR-NBR
               MOVE PR30F1-STM-PROCESS-GRP-FN (I1)
                                            TO CRT-FIELD-NBR
               GO TO 264-END.

      * Pension payment must use PR29 to maintain records
           IF  (PR30F1-LINE-FC (I1)          = "C")
           AND (PR30F1-STM-PENS-SEQ-NBR (I1) > ZEROES)
               IF (PR30F1-STM-HOURS (I1)          NOT = STM-HOURS)
               OR (PR30F1-STM-JOB-CODE (I1)       NOT = STM-JOB-CODE)
               OR (PR30F1-STM-RATE (I1)           NOT = STM-RATE)
               OR (PR30F1-STM-EFFECT-DATE (I1)    NOT = STM-EFFECT-DATE)
               OR (PR30F1-STM-END-DATE (I1)       NOT = STM-END-DATE)
               OR (PR30F1-STM-CURRENCY-CODE (I1)  NOT =
                                                      STM-CURRENCY-CODE)
               OR (PR30F1-STM-PROCESS-LEVEL (I1)  NOT =
                                                      STM-PROCESS-LEVEL)
               OR (PR30F1-STM-DEPARTMENT (I1)     NOT = STM-DEPARTMENT)
               OR (PR30F1-STM-BUS-NBR-GRP (I1)    NOT = STM-BUS-NBR-GRP)
               OR (PR30F1-STM-QC-ENT-NBR-GRP (I1) NOT =
                                                     STM-QC-ENT-NBR-GRP)
               OR (PR30F1-STM-LOCAT-CODE (I1)     NOT = STM-LOCAT-CODE)
               OR (PR30F1-STM-DIST-COMPANY (I1)   NOT =
                                                       STM-DIST-COMPANY)
               OR (PR30F1-STM-DST-ACCT-UNIT (I1)  NOT =
                                                      STM-DST-ACCT-UNIT)
               OR (PR30F1-STM-DST-SUB-ACCT (I1)   NOT =
                                                       STM-DST-SUB-ACCT)
               OR (PR30F1-STM-ACTIVITY (I1)       NOT =
                                                       STM-ACTIVITY)
               OR (PR30F1-STM-ACCT-CATEGORY (I1)  NOT =
                                                      STM-ACCT-CATEGORY)
               OR (PR30F1-STM-ATTEND-CODE (I1)    NOT = STM-ATTEND-CODE)
               OR (PR30F1-STM-OCCURRENCE (I1)     NOT = STM-OCCURRENCE)
               OR (PR30F1-STM-POSITION (I1)       NOT = STM-POSITION)
               OR (PR30F1-STM-CHECK-GRP (I1)      NOT = STM-CHECK-GRP)
               OR (PR30F1-STM-PROCESS-GRP (I1)    NOT = STM-PROCESS-GRP)
               OR (PR30F1-STM-TAX-FREQ-OVER (I1)  NOT =
                                                      STM-TAX-FREQ-OVER)
                   MOVE 145                        TO CRT-ERROR-NBR
                   MOVE PR30F1-LINE-FC-FN (I1)     TO CRT-FIELD-NBR
                   GO TO 264-END.

063800 264-END.
063900
064000******************************************************************
064100 268-EDIT-DTL-DELETE.
064200******************************************************************
064300
064400     IF (STM-EMPLOYEE = ZEROS)
064500         GO TO 268-END.
J28323
J28323     IF  (EMS-PAY-STATUS         = "NN" OR "NB")
J28323     AND (PR30F1-PT-XMIT-NBR-2   = ZEROES)
J28323         MOVE 1                          TO PR30F1-PT-XMIT-NBR-2
P53786         MOVE 127                        TO CRT-ERROR-NBR
P53786         MOVE PR30F1-STM-EMPLOYEE-FN     TO CRT-FIELD-NBR
P53786         GO TO 268-END
P53786     END-IF.
064600
064700     IF (STM-FLEX-DOLLARS NOT = ZEROS)
064800         MOVE 130                                 TO CRT-ERROR-NBR
064900         MOVE PR30F1-LINE-FC-FN (I1)              TO CRT-FIELD-NBR
065000         GO TO 268-END.
065100
J63408* VERIFY RECORD WAS NOT CREATED THRU LT530
J63408     PERFORM 269-CHECK-LT530-RCD
J63408     THRU    269-END.
J63408
      * Pension payment must use PR29 to maintain records
           IF (PR30F1-STM-PENS-SEQ-NBR (I1) > ZEROES)
               MOVE 144                        TO CRT-ERROR-NBR
               MOVE PR30F1-LINE-FC-FN (I1)     TO CRT-FIELD-NBR
               GO TO 268-END.

065200 268-END.
065300
J63408******************************************************************
J63408 269-CHECK-LT530-RCD.
J63408******************************************************************
J63408     MOVE PR30F1-STM-COMPANY         TO WS-COMPANY-N.
J63408     MOVE PR30F1-STM-EMPLOYEE        TO WS-EMPLOYEE-N.
J63408
J63408     MOVE "HR"                       TO DB-SYSTEM.
J63408     MOVE 9                          TO DB-RELEASE.
J63408     MOVE 1                          TO DB-REL-LEVEL.
J63408     MOVE "LT530"                    TO DB-KEY1.
J63408     MOVE WS-COMPANY-A               TO DB-KEY2.
J63408     MOVE WS-EMPLOYEE-A              TO DB-KEY3.
J63408     MOVE PR30F1-STM-PAY-CODE (I1)   TO DB-KEY4.
J63408     MOVE SPACES                     TO DB-KEY5
J63408                                        DB-KEY6
J63408                                        DB-KEY7
J63408                                        DB-KEY8.
J63408     MOVE ZEROES                     TO DB-SEQ-NUMBER.
J63408
J63408     MOVE SPACES                     TO FILTER-STRING.
J63408     STRING
J63408           "((HUT-SYSTEM = ?) AND (HUT-RELEASE = ?)"
J63408           " AND (HUT-REL-LEVEL = ?) AND (HUT-KEY1 = ?)"
J63408           " AND (HUT-KEY2 = ?) AND (HUT-KEY3 = ?)"
J63408           " AND (HUT-KEY4 = ?) AND (HUT-SEQ-NUMBER = ?))"
J63408                 DELIMITED BY SIZE INTO FILTER-STRING
J63408     END-STRING.
J63408
J63408     PERFORM 890-CREATE-FILTER.
J63408
J63408     MOVE "HR"                       TO ALPHANUM-FILTER-VALUE.
J63408     PERFORM 890-SET-ALPHANUM-FILTER-VALUE.
J63408     MOVE 9                          TO NUMERIC-FILTER-VALUE.
J63408     PERFORM 890-SET-NUMERIC-FILTER-VALUE.
J63408     MOVE 1                          TO NUMERIC-FILTER-VALUE.
J63408     PERFORM 890-SET-NUMERIC-FILTER-VALUE.
J63408     MOVE "LT530"                    TO ALPHANUM-FILTER-VALUE.
J63408     PERFORM 890-SET-ALPHANUM-FILTER-VALUE.
J63408     MOVE WS-COMPANY-A               TO ALPHANUM-FILTER-VALUE.
J63408     PERFORM 890-SET-ALPHANUM-FILTER-VALUE.
J63408     MOVE WS-EMPLOYEE-A              TO ALPHANUM-FILTER-VALUE.
J63408     PERFORM 890-SET-ALPHANUM-FILTER-VALUE.
J63408     MOVE PR30F1-STM-PAY-CODE (I1)   TO ALPHANUM-FILTER-VALUE.
J63408     PERFORM 890-SET-ALPHANUM-FILTER-VALUE.
J63408     MOVE PR30F1-STM-SEQ-NBR (I1)    TO NUMERIC-FILTER-VALUE.
J63408     PERFORM 890-SET-NUMERIC-FILTER-VALUE.
J63408     PERFORM 850-FILTER-BEGRNG-HUTSET1.
J63408     IF (HRUTILITY-FOUND)
J63408         MOVE HUT-DATA               TO PR30WS-HUT-DATA 
J63408         MOVE PR30WS-HUT-OBJ-ID      TO WS-OBJ-ID-A
J63408         IF (WS-OBJ-ID-N = STM-OBJ-ID)
J63408             MOVE 149                TO CRT-ERROR-NBR
J63408             MOVE PR30F1-LINE-FC-FN (I1) TO CRT-FIELD-NBR
J63408             GO TO 269-END
J63408         END-IF
J63408     END-IF.
J63408
J63408 269-END.
065400******************************************************************
065500 400-PROCESS-TRAN.
065600******************************************************************
065700
065800     IF (PR30F1-FC = "A")
065900         PERFORM 410-ADD
066000         THRU    410-END
066100     ELSE
066200     IF (PR30F1-FC = "C")
066300         PERFORM 420-CHANGE
066400         THRU    420-END
066500     ELSE
066600     IF (PR30F1-FC = "I" OR "N" OR "P" OR "+" OR "-")
066700         PERFORM 480-INQUIRE
066800         THRU    480-END.
066900
067000     IF (PR30F1-FC = "A" OR "C")
067100     AND (EMSTATUS-FOUND)
067200     AND (EMS-PAY-STATUS = "TB" OR "TN")
067300         MOVE EMS-PAY-STATUS    TO CRT-ERR-VAR1
067400         MOVE 125               TO CRT-MSG-NBR.
067500
067600 400-END.
067700
067800******************************************************************
067900 410-ADD.
068000******************************************************************
068100
068200     PERFORM 910-AUDIT-BEGIN.
068300     IF (DMS-ABORTED)
068400         GO TO 410-END.
068500
068600     IF (PR30F1-STM-EMPLOYEE NOT = ZEROS)
068700         MOVE PR30F1-STM-EMPLOYEE   TO DB-EMPLOYEE
068800         PERFORM 840-MODIFY-EMPSET1
068900     ELSE
069000         MOVE PR30F1-STM-TIME-GROUP TO DB-GROUP-NAME
069100         PERFORM 840-MODIFY-PRGSET1.
069200
069300     PERFORM 422-PROCESS-DETAIL
069400     THRU    422-END
069500         VARYING I1 FROM 1 BY 1
069600         UNTIL  (I1 > PR30F1-DETAIL-COUNT).
069700
069800     IF (PR30F1-STM-EMPLOYEE NOT = ZEROS)
069900         PERFORM 820-STORE-EMPLOYEE
070000     ELSE
070100         PERFORM 820-STORE-PERSGROUP.
070200
070300     PERFORM 920-AUDIT-END.
070400
071300     PERFORM 481-MOVE-TO-SCREEN
071400     THRU    481-END.
071500
070500     IF (PR30F1-STM-EMPLOYEE NOT = ZEROS)
070600         IF (EMP-AUTO-TIME-REC NOT = "S" AND "Y")
070700             MOVE 128             TO CRT-MSG-NBR
070800         ELSE
070900            MOVE CRT-ADD-COMPLETE TO CRT-MESSAGE
071000     ELSE
071100         MOVE CRT-ADD-COMPLETE    TO CRT-MESSAGE.
071200
071600 410-END.
071700
071800******************************************************************
071900 420-CHANGE.
072000******************************************************************
072100
072200     PERFORM 910-AUDIT-BEGIN.
072300     IF (DMS-ABORTED)
072400         GO TO 420-END.
072500
072600     PERFORM 422-PROCESS-DETAIL
072700     THRU    422-END
072800         VARYING I1 FROM 1 BY 1
072900         UNTIL  (I1 > PR30F1-DETAIL-COUNT).

           PERFORM 423-CHECK-PENS-CYCLE
           THRU    423-END
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > PR30F1-DETAIL-COUNT).
073000
073100     PERFORM 920-AUDIT-END.
073200
074100     PERFORM 481-MOVE-TO-SCREEN
074200     THRU    481-END.
074300
073300     IF (PR30F1-STM-EMPLOYEE NOT = ZEROS)
073400         IF (EMP-AUTO-TIME-REC NOT = "S" AND "Y")
073500             MOVE 129             TO CRT-MSG-NBR
073600         ELSE
073700            MOVE CRT-CHG-COMPLETE TO CRT-MESSAGE
073800     ELSE
073900         MOVE CRT-CHG-COMPLETE    TO CRT-MESSAGE.
074000
074400 420-END.
074500
074600******************************************************************
074700 422-PROCESS-DETAIL.
074800******************************************************************
074900
075000     IF (PR30F1-LINE-FC (I1) = SPACES)
075100         GO TO 422-END.
075200
075300     MOVE PR30F1-STM-COMPANY       TO DB-COMPANY.
075400     MOVE PR30F1-STM-EMPLOYEE      TO DB-EMPLOYEE.
075500     MOVE PR30F1-STM-TIME-GROUP    TO DB-TIME-GROUP.
075600     MOVE PR30F1-STM-PAY-CODE (I1) TO DB-PAY-CODE.
075700     MOVE PR30F1-STM-SEQ-NBR (I1)  TO DB-SEQ-NBR.
075800
075900     IF (PR30F1-LINE-FC (I1) = "C" OR "D")
076000         PERFORM 840-MODIFY-STMSET1.
076100
076200     IF (PR30F1-LINE-FC (I1) = "A")
076300         MOVE PR30F1-STM-EMPLOYEE    TO DB-EMPLOYEE
076400         MOVE PR30F1-STM-TIME-GROUP  TO DB-TIME-GROUP
076500         MOVE STMSET2-TIME-GROUP     TO WS-DB-BEG-RNG
076600         PERFORM 850-KFIND-BEGRNG-STMSET2
076700         IF  (STANDTIME-KFOUND)
076800             COMPUTE PR30F1-STM-SEQ-NBR (I1)
076900                                 = STM-SEQ-NBR - 1
077000         ELSE
077100             MOVE 9999           TO PR30F1-STM-SEQ-NBR (I1)
077200         END-IF
077300         PERFORM 800-CREATE-STANDTIME
077400         MOVE "HRHST"            TO IFOBIWS-OBJ-TYPE
077500         PERFORM 7000-ASSIGN-OBJ-ID-70
077600         MOVE IFOBIWS-OBJ-ID     TO STM-OBJ-ID.
077700
077800     IF (PR30F1-LINE-FC (I1) = "A" OR "C")
077900         PERFORM 510-MOVE-DTL-DATA
078000         THRU    510-END
078100         PERFORM 820-STORE-STANDTIME
078200         MOVE SPACES             TO PR30F1-LINE-FC (I1).
078300
078400     IF (PR30F1-LINE-FC (I1) = "D")
078500         PERFORM 830-DELETE-STANDTIME
078600         IF (STM-EMPLOYEE NOT = ZEROS)
078700             MOVE STM-COMPANY    TO DB-COMPANY
078800             MOVE STM-EMPLOYEE   TO DB-EMPLOYEE
078900             MOVE STM-OBJ-ID     TO DB-OBJ-ID
079000             MOVE HRHSET1-OBJ-ID TO WS-DB-BEG-RNG
079100             PERFORM 830-DELETERNG-HRHSET1
079200         END-IF
079300         INITIALIZE PR30F1-DETAIL-LINE (I1)
079400         MOVE 117                TO CRT-MSG-NBR
079500         PERFORM 790-GET-MSG
079600         MOVE CRT-MESSAGE        TO PR30F1-GL-TAB (I1)
079700         MOVE 116                TO CRT-MSG-NBR
079800         PERFORM 790-GET-MSG
079900         MOVE CRT-MESSAGE        TO PR30F1-MORE-TAB (I1).
080000
080100 422-END.
080200
074600******************************************************************
074700 423-CHECK-PENS-CYCLE.
074800******************************************************************
074900
075000     IF (PR30F1-STM-PENS-SEQ-NBR (I1) = ZEROES)
           OR (PR30F1-CYCLE-CHANGE (I1)     = ZEROES)
075100         GO TO 423-END.
075200
075300     MOVE PR30F1-STM-COMPANY             TO DB-COMPANY.
075400     MOVE PR30F1-STM-EMPLOYEE            TO DB-EMPLOYEE.
           MOVE PR30F1-STM-PENS-SEQ-NBR (I1)   TO DB-PENS-SEQ-NBR.
           MOVE STMSET6-PENS-SEQ-NBR           TO WS-DB-BEG-RNG.
           PERFORM 850-MODIFY-BEGRNG-STMSET6.
           PERFORM 424-CHECK-STM-PENS-CYCLE
           THRU    424-END
               UNTIL (STANDTIME-NOTFOUND).

           MOVE PR30F1-STM-COMPANY             TO STM-COMPANY.
           MOVE PR30F1-STM-EMPLOYEE            TO STM-EMPLOYEE.
           MOVE PR30F1-STM-TIME-GROUP          TO STM-TIME-GROUP.

080100 423-END.
080200
074600******************************************************************
074700 424-CHECK-STM-PENS-CYCLE.
074800******************************************************************
074900
115800     INITIALIZE PR30WS-OLD-CYCLES
115900                PR30WS-NEW-CYCLES.
116000     MOVE STM-DED-CYCLE (1)          TO PR30WS-OLD-CYCLES (1:1).
116100     MOVE STM-DED-CYCLE (2)          TO PR30WS-OLD-CYCLES (3:1).
116200     MOVE STM-DED-CYCLE (3)          TO PR30WS-OLD-CYCLES (5:1).
116300     MOVE STM-DED-CYCLE (4)          TO PR30WS-OLD-CYCLES (7:1).
116400     MOVE STM-DED-CYCLE (5)          TO PR30WS-OLD-CYCLES (9:1).
116500     MOVE STM-DED-CYCLE (6)          TO PR30WS-OLD-CYCLES (11:1).
116600     MOVE STM-DED-CYCLE (7)          TO PR30WS-OLD-CYCLES (13:1).
116700     MOVE STM-DED-CYCLE (8)          TO PR30WS-OLD-CYCLES (15:1).
116800     MOVE STM-DED-CYCLE (9)          TO PR30WS-OLD-CYCLES (17:1).
116900     MOVE PR30F1-STM-DED-CYCLE1 (I1) TO PR30WS-NEW-CYCLES (1:1).
117100     MOVE PR30F1-STM-DED-CYCLE2 (I1) TO PR30WS-NEW-CYCLES (3:1).
117300     MOVE PR30F1-STM-DED-CYCLE3 (I1) TO PR30WS-NEW-CYCLES (5:1).
117500     MOVE PR30F1-STM-DED-CYCLE4 (I1) TO PR30WS-NEW-CYCLES (7:1).
117700     MOVE PR30F1-STM-DED-CYCLE5 (I1) TO PR30WS-NEW-CYCLES (9:1).
117900     MOVE PR30F1-STM-DED-CYCLE6 (I1) TO PR30WS-NEW-CYCLES (11:1).
118100     MOVE PR30F1-STM-DED-CYCLE7 (I1) TO PR30WS-NEW-CYCLES (13:1).
118300     MOVE PR30F1-STM-DED-CYCLE8 (I1) TO PR30WS-NEW-CYCLES (15:1).
118500     MOVE PR30F1-STM-DED-CYCLE9 (I1) TO PR30WS-NEW-CYCLES (17:1).
118700
118800     IF (PR30WS-NEW-CYCLES NOT = PR30WS-OLD-CYCLES)
102100         INITIALIZE HRLOG-SCR-FIELDS
102200         MOVE PR30F1-STM-COMPANY         TO HRLOG-COMPANY
102300         MOVE "T"                        TO HRLOG-DATA-TYPE
102400         MOVE WS-SYSTEM-DATE-YMD         TO HRLOG-EFFECT-DATE
102500         MOVE STM-OBJ-ID                 TO HRLOG-OBJ-ID
102600
118900         MOVE PRSTM-DED-CYCLES-DN    TO HRLOG-FLD-NBR
119000         PERFORM 601-CHECK-FOR-LOG
119100         THRU    601-END
119200         IF (PR30WS-LOG-FLAG = "X")
119300             MOVE PR30WS-NEW-CYCLES  TO HRLOG-NEW-VALUE
119400             PERFORM 6000-CREATE-LOG-RECORD
               END-IF

099700         MOVE PR30F1-STM-DED-CYCLE1 (I1)    TO STM-DED-CYCLE (1)
099800         MOVE PR30F1-STM-DED-CYCLE2 (I1)    TO STM-DED-CYCLE (2)
099900         MOVE PR30F1-STM-DED-CYCLE3 (I1)    TO STM-DED-CYCLE (3)
100000         MOVE PR30F1-STM-DED-CYCLE4 (I1)    TO STM-DED-CYCLE (4)
100100         MOVE PR30F1-STM-DED-CYCLE5 (I1)    TO STM-DED-CYCLE (5)
100200         MOVE PR30F1-STM-DED-CYCLE6 (I1)    TO STM-DED-CYCLE (6)
100300         MOVE PR30F1-STM-DED-CYCLE7 (I1)    TO STM-DED-CYCLE (7)
100400         MOVE PR30F1-STM-DED-CYCLE8 (I1)    TO STM-DED-CYCLE (8)
100500         MOVE PR30F1-STM-DED-CYCLE9 (I1)    TO STM-DED-CYCLE (9)
P54339         MOVE WS-SYSTEM-DATE-YMD            TO STM-DATE-STAMP
P54339         MOVE HHMMSS                        TO STM-TIME-STAMP
P54339         MOVE CRT-USER-NAME                 TO STM-USER-ID
               PERFORM 820-STORE-STANDTIME
               PERFORM
                   VARYING I2 FROM 1 BY 1
                   UNTIL  (I2 > PR30F1-DETAIL-COUNT)
                   IF  (PR30F1-STM-PENS-SEQ-NBR (I2) =
                        PR30F1-STM-PENS-SEQ-NBR (I1))
                       MOVE PR30F1-STM-DED-CYCLE1 (I1)
                                           TO PR30F1-STM-DED-CYCLE1 (I2)
                       MOVE PR30F1-STM-DED-CYCLE2 (I1)
                                           TO PR30F1-STM-DED-CYCLE2 (I2)
                       MOVE PR30F1-STM-DED-CYCLE3 (I1)
                                           TO PR30F1-STM-DED-CYCLE3 (I2)
                       MOVE PR30F1-STM-DED-CYCLE4 (I1)
                                           TO PR30F1-STM-DED-CYCLE4 (I2)
                       MOVE PR30F1-STM-DED-CYCLE5 (I1)
                                           TO PR30F1-STM-DED-CYCLE5 (I2)
                       MOVE PR30F1-STM-DED-CYCLE6 (I1)
                                           TO PR30F1-STM-DED-CYCLE6 (I2)
                       MOVE PR30F1-STM-DED-CYCLE7 (I1)
                                           TO PR30F1-STM-DED-CYCLE7 (I2)
                       MOVE PR30F1-STM-DED-CYCLE8 (I1)
                                           TO PR30F1-STM-DED-CYCLE8 (I2)
                       MOVE PR30F1-STM-DED-CYCLE9 (I1)
                                           TO PR30F1-STM-DED-CYCLE9 (I2)
                   END-IF
               END-PERFORM
           END-IF.

           PERFORM 860-MODIFY-NXTRNG-STMSET6.

080100 424-END.
080200
080300******************************************************************
080400 480-INQUIRE.
080500******************************************************************
080600
080700     IF (PR30F1-FC = "I")
080800         MOVE "+"                    TO PR30F1-FC.
J28323
J28323     MOVE ZEROES                     TO PR30F1-PT-XMIT-NBR-2.
080900
081000     PERFORM 481-MOVE-TO-SCREEN
081100     THRU    481-END.
081200
081300     IF (PR30F1-FC = "-")
081400         MOVE PR30F1-DETAIL-COUNT     TO I1
ACS001         MOVE PR30F1-STM-EFFECT-DATE (1)
ACS001                                      TO PR30F1-PT-STM-EFFECT-DATE
ACS001         MOVE PR30F1-STM-END-DATE (1) TO PR30F1-PT-STM-END-DATE
081500         MOVE PR30F1-STM-PAY-CODE (1) TO PR30F1-PT-STM-PAY-CODE
081600         MOVE PR30F1-STM-SEQ-NBR (1)  TO PR30F1-PT-STM-SEQ-NBR
081700         INITIALIZE PR30F1-DETAIL-DATA
081800         MOVE 116                     TO CRT-MSG-NBR
081900         PERFORM 790-GET-MSG
082000         PERFORM
082100             VARYING I2 FROM 1 BY 1
082200             UNTIL  (I2 > PR30F1-DETAIL-COUNT)
082300             MOVE CRT-MESSAGE         TO PR30F1-MORE-TAB (I2)
082400         END-PERFORM
082500         MOVE 117                     TO CRT-MSG-NBR
082600         PERFORM 790-GET-MSG
082700         PERFORM
082800             VARYING I2 FROM 1 BY 1
082900             UNTIL  (I2 > PR30F1-DETAIL-COUNT)
083000             MOVE CRT-MESSAGE         TO PR30F1-GL-TAB (I2)
083100         END-PERFORM
083200         PERFORM 490-MOVE-DTL-TO-SCREEN
083300         THRU    490-END
083400             UNTIL  (I1           < 1)
083500             OR     (STANDTIME-NOTFOUND)
083600             OR     (STM-COMPANY    NOT = PR30F1-STM-COMPANY)
083700             OR     (STM-EMPLOYEE   NOT = PR30F1-STM-EMPLOYEE)
083800             OR     (STM-TIME-GROUP NOT = PR30F1-STM-TIME-GROUP)
083900
084000         IF (STANDTIME-NOTFOUND)
084100         OR (STM-COMPANY    NOT = PR30F1-STM-COMPANY)
084200         OR (STM-EMPLOYEE   NOT = PR30F1-STM-EMPLOYEE)
084300         OR (STM-TIME-GROUP NOT = PR30F1-STM-TIME-GROUP)
084400             MOVE CRT-INQ-COMPLETE    TO CRT-MESSAGE
084500         ELSE
084600             MOVE CRT-MORE-RECS       TO CRT-MESSAGE
084700     ELSE
084800         MOVE 1                       TO I1
084900         INITIALIZE PR30F1-DETAIL-DATA
085000         MOVE 116                     TO CRT-MSG-NBR
085100         PERFORM 790-GET-MSG
085200         PERFORM
085300             VARYING I2 FROM 1 BY 1
085400             UNTIL  (I2 > PR30F1-DETAIL-COUNT)
085500             MOVE CRT-MESSAGE         TO PR30F1-MORE-TAB (I2)
085600         END-PERFORM
085700         MOVE 117                     TO CRT-MSG-NBR
085800         PERFORM 790-GET-MSG
085900         PERFORM
086000             VARYING I2 FROM 1 BY 1
086100             UNTIL  (I2 > PR30F1-DETAIL-COUNT)
086200             MOVE CRT-MESSAGE         TO PR30F1-GL-TAB (I2)
086300         END-PERFORM
086400         PERFORM 490-MOVE-DTL-TO-SCREEN
086500         THRU    490-END
086600             UNTIL  (I1           > PR30F1-DETAIL-COUNT)
086700             OR     (STANDTIME-NOTFOUND)
086800             OR     (STM-COMPANY    NOT = PR30F1-STM-COMPANY)
086900             OR     (STM-EMPLOYEE   NOT = PR30F1-STM-EMPLOYEE)
087000             OR     (STM-TIME-GROUP NOT = PR30F1-STM-TIME-GROUP)
087100
087200         IF (STANDTIME-NOTFOUND)
087300         OR (STM-COMPANY    NOT = PR30F1-STM-COMPANY)
087400         OR (STM-EMPLOYEE   NOT = PR30F1-STM-EMPLOYEE)
087500         OR (STM-TIME-GROUP NOT = PR30F1-STM-TIME-GROUP)
ACS001             MOVE WS-HIGH-VALUES      TO PR30F1-PT-STM-EFFECT-DATE
ACS001             MOVE WS-HIGH-VALUES      TO PR30F1-PT-STM-END-DATE
087600             MOVE SPACES              TO PR30F1-PT-STM-PAY-CODE
087700             MOVE ZEROES              TO PR30F1-PT-STM-SEQ-NBR
087800             MOVE CRT-INQ-COMPLETE    TO CRT-MESSAGE
087900         ELSE
ACS001             MOVE STM-EFFECT-DATE     TO PR30F1-PT-STM-EFFECT-DATE
ACS001             MOVE STM-END-DATE        TO PR30F1-PT-STM-END-DATE
088000             MOVE STM-PAY-CODE        TO PR30F1-PT-STM-PAY-CODE
088100             MOVE STM-SEQ-NBR         TO PR30F1-PT-STM-SEQ-NBR
088200             MOVE CRT-MORE-RECS       TO CRT-MESSAGE.
088300
088400 480-END.
088500
088600******************************************************************
088700 481-MOVE-TO-SCREEN.
088800******************************************************************
088900
089000     MOVE STM-COMPANY            TO PR30F1-STM-COMPANY.
089100     MOVE STM-EMPLOYEE           TO PR30F1-STM-EMPLOYEE.
089200     MOVE STM-TIME-GROUP         TO PR30F1-STM-TIME-GROUP.
089300
089400     INITIALIZE PR30F1-EMP-FULL-NAME.
089500     INITIALIZE PR30F1-PRG-DESCRIPTION.
089600
089700     IF (PR30F1-STM-TIME-GROUP NOT = SPACES)
089800         MOVE PR30F1-STM-COMPANY       TO DB-COMPANY
089900         MOVE PR30F1-STM-TIME-GROUP    TO DB-GROUP-NAME
090000         PERFORM 840-FIND-PRGSET1
090100         IF (PERSGROUP-FOUND)
090200             MOVE PRG-DESCRIPTION    TO PR30F1-PRG-DESCRIPTION.
090300
090400     MOVE PR30F1-STM-COMPANY               TO DB-COMPANY.
090500     MOVE SPACES                           TO DB-PROCESS-LEVEL.
090600     PERFORM 840-FIND-PRSSET1.
090700     MOVE PRS-NAME                         TO PR30F1-PRS-1-NAME.

J60347     MOVE SPACES                       TO PR30F1-AUTO-TR-DESC.
J60347     MOVE SPACES                       TO PR30F1-AUTO-TIME-REC.
J60347     MOVE SPACES                       TO PR30F1-EMP-AUTO-TIMREC.
090800
090900     IF (PR30F1-STM-EMPLOYEE NOT = ZEROES)
091000         MOVE PR30F1-STM-COMPANY       TO DB-COMPANY
091100         MOVE PR30F1-STM-EMPLOYEE      TO DB-EMPLOYEE
091200         PERFORM 840-FIND-EMPSET1
091300         IF (EMPLOYEE-FOUND)
J60347             MOVE EMP-AUTO-TIME-REC    TO PR30F1-AUTO-TIME-REC
                   MOVE EMP-LAST-NAME        TO HRWS-LAST-NAME
                   MOVE EMP-FIRST-NAME       TO HRWS-FIRST-NAME
                   MOVE EMP-MIDDLE-INIT      TO HRWS-MIDDLE-INIT
                   MOVE EMP-NAME-SUFFIX      TO HRWS-NAME-SUFFIX
                   MOVE EMP-LAST-NAME-PRE    TO HRWS-LAST-NAME-PRE
                   PERFORM 750-HR-FORMAT-NAME
                   MOVE HRWS-FORMAT-NAME     TO PR30F1-EMP-FULL-NAME.
092800
090800     MOVE "PRMSG"                      TO CRT-ERROR-CAT.
090900     MOVE 313                          TO CRT-MSG-NBR.
091000     PERFORM 790-GET-MSG.
091100     MOVE CRT-MESSAGE                  TO PR30F1-COMMENTS.

J60347     IF (PR30F1-STM-EMPLOYEE NOT = ZEROES)
J60347         MOVE 211                      TO CRT-MSG-NBR 
J60347         MOVE "PR30"                   TO CRT-ERROR-CAT 
J60347         PERFORM 790-GET-MSG 
J60347         MOVE CRT-MESSAGE              TO PR30F1-EMP-AUTO-TIMREC
J60347         MOVE 212                      TO CRT-MSG-NBR
J60347         MOVE "PR30"                   TO CRT-ERROR-CAT
J60347         PERFORM 790-GET-MSG
J60347         MOVE CRT-MESSAGE              TO PR30F1-AUTO-TR-DESC    
J60347     END-IF.

           INITIALIZE CRT-MESSAGE.

           INITIALIZE PR30F1-COMMENTS-FLAG.
           MOVE PR30F1-STM-COMPANY     TO DB-COMPANY.
           MOVE PR30F1-PAC-EMP-APP     TO DB-EMP-APP.
           MOVE PR30F1-PAC-CMT-TYPE    TO DB-CMT-TYPE.
           MOVE PR30F1-STM-EMPLOYEE    TO DB-EMPLOYEE.
           MOVE PACSET1-EMPLOYEE       TO WS-DB-BEG-RNG.
           PERFORM 850-KFIND-BEGRNG-PACSET1.
           IF (PACOMMENTS-KFOUND)
               MOVE "*"                TO PR30F1-COMMENTS-FLAG.
115300
092900 481-END.
093000
093100******************************************************************
093200 490-MOVE-DTL-TO-SCREEN.
093300******************************************************************
093400
093500     MOVE STM-PAY-CODE           TO PR30F1-STM-PAY-CODE (I1).
093600     MOVE STM-SEQ-NBR            TO PR30F1-STM-SEQ-NBR (I1).
093700     MOVE STM-HOURS              TO PR30F1-STM-HOURS (I1).
093800     MOVE STM-JOB-CODE           TO PR30F1-STM-JOB-CODE (I1).
093900     MOVE STM-POSITION           TO PR30F1-STM-POSITION (I1).
094000     MOVE STM-RATE               TO PR30F1-STM-RATE (I1).
094100     MOVE STM-EFFECT-DATE        TO PR30F1-STM-EFFECT-DATE (I1).
094200     MOVE STM-END-DATE           TO PR30F1-STM-END-DATE (I1).
LP         MOVE STM-REASON-CODE        TO PR30F1-STM-REASON-CODE (I1).
094300     MOVE STM-ATTEND-CODE        TO PR30F1-STM-ATTEND-CODE (I1).
094400     MOVE STM-OCCURRENCE         TO PR30F1-STM-OCCURRENCE (I1).
           MOVE STM-PROCESS-LEVEL      TO PR30F1-STM-PROCESS-LEVEL (I1).
           MOVE STM-DEPARTMENT         TO PR30F1-STM-DEPARTMENT (I1).
           MOVE STM-BUS-NBR-GRP        TO PR30F1-STM-BUS-NBR-GRP (I1).
           MOVE STM-QC-ENT-NBR-GRP    TO PR30F1-STM-QC-ENT-NBR-GRP (I1).
           MOVE STM-LOCAT-CODE         TO PR30F1-STM-LOCAT-CODE (I1).
094500     MOVE STM-DED-CYCLE (1)      TO PR30F1-STM-DED-CYCLE1 (I1).
094600     MOVE STM-DED-CYCLE (2)      TO PR30F1-STM-DED-CYCLE2 (I1).
094700     MOVE STM-DED-CYCLE (3)      TO PR30F1-STM-DED-CYCLE3 (I1).
094800     MOVE STM-DED-CYCLE (4)      TO PR30F1-STM-DED-CYCLE4 (I1).
094900     MOVE STM-DED-CYCLE (5)      TO PR30F1-STM-DED-CYCLE5 (I1).
095000     MOVE STM-DED-CYCLE (6)      TO PR30F1-STM-DED-CYCLE6 (I1).
095100     MOVE STM-DED-CYCLE (7)      TO PR30F1-STM-DED-CYCLE7 (I1).
095200     MOVE STM-DED-CYCLE (8)      TO PR30F1-STM-DED-CYCLE8 (I1).
095300     MOVE STM-DED-CYCLE (9)      TO PR30F1-STM-DED-CYCLE9 (I1).
095400     MOVE STM-DIST-COMPANY       TO PR30F1-STM-DIST-COMPANY (I1).
095500     MOVE STM-DST-ACCT-UNIT      TO PR30F1-STM-DST-ACCT-UNIT (I1).
095600     MOVE STM-DST-ACCOUNT        TO PR30F1-STM-DST-ACCOUNT (I1).
095700     MOVE STM-DST-SUB-ACCT       TO PR30F1-STM-DST-SUB-ACCT (I1).
095800     MOVE STM-ACTIVITY           TO PR30F1-STM-ACTIVITY (I1).
095900     MOVE STM-ACCT-CATEGORY      TO PR30F1-STM-ACCT-CATEGORY (I1).
           MOVE STM-CURRENCY-CODE      TO PR30F1-STM-CURRENCY-CODE (I1).
           MOVE STM-CURR-ND            TO PR30F1-STM-CURR-ND (I1).
           MOVE STM-PENS-SEQ-NBR       TO PR30F1-STM-PENS-SEQ-NBR (I1).
           MOVE STM-TAX-FREQ-OVER      TO PR30F1-STM-TAX-FREQ-OVER (I1).
           MOVE STM-CHECK-GRP          TO PR30F1-STM-CHECK-GRP (I1).
           MOVE STM-PROCESS-GRP        TO PR30F1-STM-PROCESS-GRP(I1).
J41010
J41010     IF (STM-DAILY-TR-FLG = 1)
J41010         MOVE "Y"                TO PR30F1-STM-DAILY-TR-FLG (I1)
J41010     ELSE
J41010         MOVE "N"                TO PR30F1-STM-DAILY-TR-FLG (I1)
J41010     END-IF.
096200
096300     IF (PR30F1-FC = "-")
096400         SUBTRACT 1              FROM I1
096500     ELSE
096600         ADD 1                   TO I1.
096700
096800 490-NEXT.
096900     IF (PR30F1-FC = "-")
ACS001*        PERFORM 870-FIND-PREV-STMSET1
ACS001         PERFORM 870-FIND-PREV-STMSETW1
097100     ELSE
ACS001*        PERFORM 860-FIND-NEXT-STMSET1.
ACS001         PERFORM 860-FIND-NEXT-STMSETW1.
097300
097400 490-END.
097500
097600******************************************************************
097700 510-MOVE-DTL-DATA.
097800******************************************************************
097900
J41010     IF (PR30F1-STM-DAILY-TR-FLG (I1) = "Y")
J41010         MOVE 1                         TO PR30WS-DAILY-TR-FLG
J41010     ELSE
J41010         MOVE 0                         TO PR30WS-DAILY-TR-FLG
J41010     END-IF.
098000     IF (PR30F1-STM-EMPLOYEE NOT = ZEROS)
098100         PERFORM 600-LOG-CHANGES
098200         THRU    600-END.
098300
098400     MOVE PR30F1-STM-COMPANY            TO STM-COMPANY.
098500     MOVE PR30F1-STM-EMPLOYEE           TO STM-EMPLOYEE.
098600     MOVE PR30F1-STM-TIME-GROUP         TO STM-TIME-GROUP.
098700     MOVE PR30F1-STM-PAY-CODE (I1)      TO STM-PAY-CODE.
098800     MOVE PR30F1-STM-SEQ-NBR (I1)       TO STM-SEQ-NBR.
098900     MOVE PR30F1-STM-HOURS (I1)         TO STM-HOURS.
099000     MOVE PR30F1-STM-JOB-CODE (I1)      TO STM-JOB-CODE.
099100     MOVE PR30F1-STM-POSITION (I1)      TO STM-POSITION.
099200     MOVE PR30F1-STM-RATE (I1)          TO STM-RATE.
099300     MOVE PR30F1-STM-EFFECT-DATE (I1)   TO STM-EFFECT-DATE.
099400     MOVE PR30F1-STM-END-DATE (I1)      TO STM-END-DATE.
LP         MOVE PR30F1-STM-REASON-CODE (I1)   TO STM-REASON-CODE.
099500     MOVE PR30F1-STM-ATTEND-CODE (I1)   TO STM-ATTEND-CODE.
099600     MOVE PR30F1-STM-OCCURRENCE (I1)    TO STM-OCCURRENCE.
           MOVE PR30F1-STM-PROCESS-LEVEL (I1) TO STM-PROCESS-LEVEL.
           MOVE PR30F1-STM-DEPARTMENT (I1)    TO STM-DEPARTMENT.
           MOVE PR30F1-STM-BUS-NBR-GRP (I1)   TO STM-BUS-NBR-GRP.
           MOVE PR30F1-STM-QC-ENT-NBR-GRP (I1) TO STM-QC-ENT-NBR-GRP.
           MOVE PR30F1-STM-LOCAT-CODE (I1)    TO STM-LOCAT-CODE.
099700     MOVE PR30F1-STM-DED-CYCLE1 (I1)    TO STM-DED-CYCLE (1).
099800     MOVE PR30F1-STM-DED-CYCLE2 (I1)    TO STM-DED-CYCLE (2).
099900     MOVE PR30F1-STM-DED-CYCLE3 (I1)    TO STM-DED-CYCLE (3).
100000     MOVE PR30F1-STM-DED-CYCLE4 (I1)    TO STM-DED-CYCLE (4).
100100     MOVE PR30F1-STM-DED-CYCLE5 (I1)    TO STM-DED-CYCLE (5).
100200     MOVE PR30F1-STM-DED-CYCLE6 (I1)    TO STM-DED-CYCLE (6).
100300     MOVE PR30F1-STM-DED-CYCLE7 (I1)    TO STM-DED-CYCLE (7).
100400     MOVE PR30F1-STM-DED-CYCLE8 (I1)    TO STM-DED-CYCLE (8).
100500     MOVE PR30F1-STM-DED-CYCLE9 (I1)    TO STM-DED-CYCLE (9).
100600     MOVE PR30F1-STM-DIST-COMPANY (I1)  TO STM-DIST-COMPANY.
100700     MOVE PR30F1-STM-DST-ACCT-UNIT (I1) TO STM-DST-ACCT-UNIT.
100800     MOVE PR30F1-STM-DST-ACCOUNT (I1)   TO STM-DST-ACCOUNT.
100900     MOVE PR30F1-STM-DST-SUB-ACCT (I1)  TO STM-DST-SUB-ACCT.
101000     MOVE PR30F1-STM-ACTIVITY (I1)      TO STM-ACTIVITY.
101100     MOVE PR30F1-STM-ACCT-CATEGORY (I1) TO STM-ACCT-CATEGORY.
           MOVE PR30F1-STM-PENS-SEQ-NBR (I1)  TO STM-PENS-SEQ-NBR.
           MOVE PR30F1-STM-TAX-FREQ-OVER (I1) TO STM-TAX-FREQ-OVER.
           MOVE PR30F1-STM-CHECK-GRP (I1)     TO STM-CHECK-GRP.
           MOVE PR30F1-STM-PROCESS-GRP (I1)   TO STM-PROCESS-GRP.
J41010     IF (PR30F1-STM-DAILY-TR-FLG (I1) = "Y")
J41010         MOVE 1                         TO STM-DAILY-TR-FLG
J41010     ELSE
J41010         MOVE 0                         TO STM-DAILY-TR-FLG  
J41010     END-IF.
           IF (PR30F1-STM-RATE (I1) = ZEROES)
               MOVE SPACES                    TO STM-CURRENCY-CODE
               MOVE ZEROES                    TO STM-CURR-ND
           ELSE
               MOVE PR30F1-STM-CURRENCY-CODE (I1)
                                              TO STM-CURRENCY-CODE
               MOVE PR30F1-STM-CURR-ND (I1)   TO STM-CURR-ND.

P54339     MOVE WS-SYSTEM-DATE-YMD            TO STM-DATE-STAMP.
P54339     MOVE HHMMSS                        TO STM-TIME-STAMP.
P54339     MOVE CRT-USER-NAME                 TO STM-USER-ID.
P54339     IF (PR30F1-LINE-FC (I1) = "A")   
P54339         MOVE WS-SYSTEM-DATE-YMD        TO STM-CREATE-DATE
P54339         MOVE HHMMSS                    TO STM-CREATE-TIME
P54339         MOVE CRT-USER-NAME             TO STM-CREATE-USER-ID
P54339     END-IF.
101400
101500 510-END.
101600
101700******************************************************************
101800 600-LOG-CHANGES.
101900******************************************************************
102000
102100     INITIALIZE HRLOG-SCR-FIELDS.
102200     MOVE PR30F1-STM-COMPANY         TO HRLOG-COMPANY.
102300     MOVE "T"                        TO HRLOG-DATA-TYPE.
102400     MOVE WS-SYSTEM-DATE-YMD         TO HRLOG-EFFECT-DATE.
102500     MOVE STM-OBJ-ID                 TO HRLOG-OBJ-ID.
102600
102700     IF (PR30F1-STM-PAY-CODE (I1) NOT = STM-PAY-CODE)
102800         MOVE PRSTM-PAY-CODE-DN   TO HRLOG-FLD-NBR
102900         PERFORM 601-CHECK-FOR-LOG
103000         THRU    601-END
103100         IF (PR30WS-LOG-FLAG = "X")
103200             MOVE PR30F1-STM-PAY-CODE (I1) TO HRLOG-NEW-VALUE
103300             PERFORM 6000-CREATE-LOG-RECORD.
103400
103500     IF (PR30F1-STM-HOURS (I1) NOT = STM-HOURS)
103600         MOVE PRSTM-HOURS-DN   TO HRLOG-FLD-NBR
103700         PERFORM 601-CHECK-FOR-LOG
103800         THRU    601-END
103900         IF (PR30WS-LOG-FLAG = "X")
104000             MOVE PR30F1-STM-HOURS (I1) TO HRLOG-NEW-DEC-FIELD
104100             PERFORM 6000-CREATE-LOG-RECORD.
104200
104300     IF (PR30F1-STM-RATE (I1) NOT = STM-RATE)
104400         MOVE PRSTM-RATE-DN   TO HRLOG-FLD-NBR
104500         PERFORM 601-CHECK-FOR-LOG
104600         THRU    601-END
104700         IF (PR30WS-LOG-FLAG = "X")
104800             MOVE PR30F1-STM-RATE (I1) TO HRLOG-NEW-DEC-FIELD
104900             PERFORM 6000-CREATE-LOG-RECORD.
105000
105100     IF (PR30F1-STM-EFFECT-DATE (I1) NOT = STM-EFFECT-DATE)
105200         MOVE PRSTM-EFFECT-DATE-DN     TO HRLOG-FLD-NBR
105300         PERFORM 601-CHECK-FOR-LOG
105400         THRU    601-END
105500         IF (PR30WS-LOG-FLAG = "X")
105600             MOVE PR30F1-STM-EFFECT-DATE (I1)  TO HRLOG-NEW-DATE
105700             PERFORM 6000-CREATE-LOG-RECORD.
105800
105900     IF (PR30F1-STM-END-DATE (I1) NOT = STM-END-DATE)
106000         MOVE PRSTM-END-DATE-DN     TO HRLOG-FLD-NBR
106100         PERFORM 601-CHECK-FOR-LOG
106200         THRU    601-END
106300         IF (PR30WS-LOG-FLAG = "X")
106400             MOVE PR30F1-STM-END-DATE (I1)  TO HRLOG-NEW-DATE
106500             PERFORM 6000-CREATE-LOG-RECORD.
106600
106700     IF (PR30F1-STM-PROCESS-LEVEL (I1) NOT = STM-PROCESS-LEVEL)
106800         MOVE PRSTM-PROCESS-LEVEL-DN   TO HRLOG-FLD-NBR
106900         PERFORM 601-CHECK-FOR-LOG
107000         THRU    601-END
107100         IF (PR30WS-LOG-FLAG = "X")
107200             MOVE PR30F1-STM-PROCESS-LEVEL (I1) TO HRLOG-NEW-VALUE
107300             PERFORM 6000-CREATE-LOG-RECORD.
107400
107500     IF (PR30F1-STM-DST-ACCT-UNIT (I1) NOT = STM-DST-ACCT-UNIT)
107600         MOVE PRSTM-DST-ACCT-UNIT-DN   TO HRLOG-FLD-NBR
107700         PERFORM 601-CHECK-FOR-LOG
107800         THRU    601-END
107900         IF (PR30WS-LOG-FLAG = "X")
108000             MOVE PR30F1-STM-DST-ACCT-UNIT (I1) TO HRLOG-NEW-VALUE
108100             PERFORM 6000-CREATE-LOG-RECORD.
108200
108300     IF (PR30F1-STM-DST-ACCOUNT (I1) NOT = STM-DST-ACCOUNT)
108400         MOVE PRSTM-DST-ACCOUNT-DN   TO HRLOG-FLD-NBR
108500         PERFORM 601-CHECK-FOR-LOG
108600         THRU    601-END
108700         IF (PR30WS-LOG-FLAG = "X")
108800             MOVE PR30F1-STM-DST-ACCOUNT (I1)
108900                                     TO HRLOG-NEW-DEC-FIELD
109000             PERFORM 6000-CREATE-LOG-RECORD.
109100
109200     IF (PR30F1-STM-DST-SUB-ACCT (I1) NOT = STM-DST-SUB-ACCT)
109300         MOVE PRSTM-DST-SUB-ACCT-DN   TO HRLOG-FLD-NBR
109400         PERFORM 601-CHECK-FOR-LOG
109500         THRU    601-END
109600         IF (PR30WS-LOG-FLAG = "X")
109700             MOVE PR30F1-STM-DST-SUB-ACCT (I1)
109800                                      TO HRLOG-NEW-DEC-FIELD
109900             PERFORM 6000-CREATE-LOG-RECORD.
110000
110100     IF (PR30F1-STM-DIST-COMPANY (I1) NOT = STM-DIST-COMPANY)
110200         MOVE PRSTM-DIST-COMPANY-DN   TO HRLOG-FLD-NBR
110300         PERFORM 601-CHECK-FOR-LOG
110400         THRU    601-END
110500         IF (PR30WS-LOG-FLAG = "X")
110600             MOVE PR30F1-STM-DIST-COMPANY (I1)
110700                                      TO HRLOG-NEW-DEC-FIELD
110800             PERFORM 6000-CREATE-LOG-RECORD.
110900
111000     IF (PR30F1-STM-ACTIVITY (I1) NOT = STM-ACTIVITY)
111100         MOVE PRSTM-ACTIVITY-DN       TO HRLOG-FLD-NBR
111200         PERFORM 601-CHECK-FOR-LOG
111300         THRU    601-END
111400         IF (PR30WS-LOG-FLAG = "X")
111500             MOVE PR30F1-STM-ACTIVITY (I1)     TO HRLOG-NEW-VALUE
111600             PERFORM 6000-CREATE-LOG-RECORD.
111700
111800     IF (PR30F1-STM-ACCT-CATEGORY (I1) NOT = STM-ACCT-CATEGORY)
111900         MOVE PRSTM-ACCT-CATEGORY-DN TO HRLOG-FLD-NBR
112000         PERFORM 601-CHECK-FOR-LOG
112100         THRU    601-END
112200         IF (PR30WS-LOG-FLAG = "X")
112300             MOVE PR30F1-STM-ACCT-CATEGORY (I1) TO HRLOG-NEW-VALUE
112400             PERFORM 6000-CREATE-LOG-RECORD.
112500
112600     IF (PR30F1-STM-JOB-CODE (I1) NOT = STM-JOB-CODE)
112700         MOVE PRSTM-JOB-CODE-DN   TO HRLOG-FLD-NBR
112800         PERFORM 601-CHECK-FOR-LOG
112900         THRU    601-END
113000         IF (PR30WS-LOG-FLAG = "X")
113100             MOVE PR30F1-STM-JOB-CODE (I1) TO HRLOG-NEW-VALUE
113200             PERFORM 6000-CREATE-LOG-RECORD.
113300
113400     IF (PR30F1-STM-POSITION (I1) NOT = STM-POSITION)
113500         MOVE PRSTM-POSITION-DN   TO HRLOG-FLD-NBR
113600         PERFORM 601-CHECK-FOR-LOG
113700         THRU    601-END
113800         IF (PR30WS-LOG-FLAG = "X")
113900             MOVE PR30F1-STM-POSITION (I1) TO HRLOG-NEW-VALUE
114000             PERFORM 6000-CREATE-LOG-RECORD.
114100
LP         IF (PR30F1-STM-REASON-CODE (I1) NOT = STM-REASON-CODE)
114300         MOVE PRSTM-REASON-CODE-DN   TO HRLOG-FLD-NBR
114400         PERFORM 601-CHECK-FOR-LOG
114500         THRU    601-END
114600         IF (PR30WS-LOG-FLAG = "X")
114700             MOVE PR30F1-STM-REASON-CODE (I1) TO HRLOG-NEW-VALUE
114800             PERFORM 6000-CREATE-LOG-RECORD.
114900
114200     IF (PR30F1-STM-ATTEND-CODE (I1) NOT = STM-ATTEND-CODE)
114300         MOVE PRSTM-ATTEND-CODE-DN   TO HRLOG-FLD-NBR
114400         PERFORM 601-CHECK-FOR-LOG
114500         THRU    601-END
114600         IF (PR30WS-LOG-FLAG = "X")
114700             MOVE PR30F1-STM-ATTEND-CODE (I1) TO HRLOG-NEW-VALUE
114800             PERFORM 6000-CREATE-LOG-RECORD.
114900
115000     IF (PR30F1-STM-OCCURRENCE (I1) NOT = STM-OCCURRENCE)
115100         MOVE PRSTM-OCCURRENCE-DN   TO HRLOG-FLD-NBR
115200         PERFORM 601-CHECK-FOR-LOG
115300         THRU    601-END
115400         IF (PR30WS-LOG-FLAG = "X")
115500             MOVE PR30F1-STM-OCCURRENCE (I1) TO HRLOG-NEW-VALUE
115600             PERFORM 6000-CREATE-LOG-RECORD.
J41010
J41010    IF (PR30WS-DAILY-TR-FLG-R NOT = STM-DAILY-TR-FLG)
J41010         MOVE PRSTM-DAILY-TR-FLG-DN  TO HRLOG-FLD-NBR
J41010         PERFORM 601-CHECK-FOR-LOG
J41010         THRU    601-END
J41010         IF (PR30WS-LOG-FLAG = "X")
J41010             MOVE PR30WS-DAILY-TR-FLG-R     TO HRLOG-NEW-DEC-FIELD
J41010             PERFORM 6000-CREATE-LOG-RECORD
J41010         END-IF
J41010     END-IF.
115700
115800     INITIALIZE PR30WS-OLD-CYCLES
115900                PR30WS-NEW-CYCLES.
           INITIALIZE PR30F1-CYCLE-CHANGE (I1).
116000     MOVE STM-DED-CYCLE (1)      TO PR30WS-OLD-CYCLES (1:1).
116100     MOVE STM-DED-CYCLE (2)      TO PR30WS-OLD-CYCLES (3:1).
116200     MOVE STM-DED-CYCLE (3)      TO PR30WS-OLD-CYCLES (5:1).
116300     MOVE STM-DED-CYCLE (4)      TO PR30WS-OLD-CYCLES (7:1).
116400     MOVE STM-DED-CYCLE (5)      TO PR30WS-OLD-CYCLES (9:1).
116500     MOVE STM-DED-CYCLE (6)      TO PR30WS-OLD-CYCLES (11:1).
116600     MOVE STM-DED-CYCLE (7)      TO PR30WS-OLD-CYCLES (13:1).
116700     MOVE STM-DED-CYCLE (8)      TO PR30WS-OLD-CYCLES (15:1).
116800     MOVE STM-DED-CYCLE (9)      TO PR30WS-OLD-CYCLES (17:1).
116900     MOVE PR30F1-STM-DED-CYCLE1 (I1)
117000                                 TO PR30WS-NEW-CYCLES (1:1).
117100     MOVE PR30F1-STM-DED-CYCLE2 (I1)
117200                                 TO PR30WS-NEW-CYCLES (3:1).
117300     MOVE PR30F1-STM-DED-CYCLE3 (I1)
117400                                 TO PR30WS-NEW-CYCLES (5:1).
117500     MOVE PR30F1-STM-DED-CYCLE4 (I1)
117600                                 TO PR30WS-NEW-CYCLES (7:1).
117700     MOVE PR30F1-STM-DED-CYCLE5 (I1)
117800                                 TO PR30WS-NEW-CYCLES (9:1).
117900     MOVE PR30F1-STM-DED-CYCLE6 (I1)
118000                                 TO PR30WS-NEW-CYCLES (11:1).
118100     MOVE PR30F1-STM-DED-CYCLE7 (I1)
118200                                 TO PR30WS-NEW-CYCLES (13:1).
118300     MOVE PR30F1-STM-DED-CYCLE8 (I1)
118400                                 TO PR30WS-NEW-CYCLES (15:1).
118500     MOVE PR30F1-STM-DED-CYCLE9 (I1)
118600                                 TO PR30WS-NEW-CYCLES (17:1).
118700
118800     IF (PR30WS-NEW-CYCLES NOT = PR30WS-OLD-CYCLES)
               MOVE 1                  TO PR30F1-CYCLE-CHANGE (I1)
118900         MOVE PRSTM-DED-CYCLES-DN    TO HRLOG-FLD-NBR
119000         PERFORM 601-CHECK-FOR-LOG
119100         THRU    601-END
119200         IF (PR30WS-LOG-FLAG = "X")
119300             MOVE PR30WS-NEW-CYCLES  TO HRLOG-NEW-VALUE
119400             PERFORM 6000-CREATE-LOG-RECORD.
119500
119600 600-END.
119700
119800******************************************************************
119900 601-CHECK-FOR-LOG.
120000******************************************************************
120100
120200     MOVE PR30F1-STM-COMPANY     TO DB-COMPANY.
120300     MOVE HRLOG-FLD-NBR          TO DB-FLD-NBR.
           INITIALIZE                     DB-COUNTRY-CD-REQ
                                          DB-PROCESS-LEVEL. 
120400     PERFORM 840-FIND-PASSET1.
120500     IF (PASCRTY-FOUND)
120600         MOVE PAS-LOG-FLAG       TO PR30WS-LOG-FLAG
120700     ELSE
120800         MOVE SPACES             TO PR30WS-LOG-FLAG.
120900
121000 601-END.
J60347******************************************************************
J60347 705-SCREEN-XFER.
J60347******************************************************************
J60347     IF (PR30F1-FC = "H")
J60347         MOVE "I"                TO CRT-DISPLAY-FC
J60347         MOVE "I"                TO CRT-PASS-FC
J60347         MOVE CRT-MANUAL-CF      TO CRT-REQUEST
J60347         MOVE SPACES             TO CRT-MESSAGE
J60347         MOVE PR30F1-EMP-AUTO-TIMREC-FN
J60347                                 TO CRT-FIELD-NBR
J60347         MOVE "PR302"            TO CRT-SCREEN-CODE
J60347         INITIALIZE CRT-MSG-NBR
J60347                    CRT-FIELD-NBR
J60347         GO TO 705-END.
J60347 705-END.
121200******************************************************************
121300 PR30S1-TRANSACTION-END.
121400******************************************************************
J60347******************************************************************
J60347 PR30S2-TRANSACTION              SECTION 10.
J60347******************************************************************
J60347 PR30S2-START.
J60347
J60347     PERFORM 210-EDIT-ACCESS
J60347     THRU    210-END.
J60347
J60347     IF (PR30F2-FC = "I")
J60347     AND (NO-ERROR-FOUND)
J60347         PERFORM 480-INQUIRE
J60347         THRU    480-END.
J60347
J60347     IF (PR30F2-FC = "C")
J60347     AND (NO-ERROR-FOUND)
J60347         PERFORM 650-CHANGE
J60347         THRU    650-END.
J60347
J60347     GO TO PR30S2-TRANSACTION-END.
J60347
J60347******************************************************************
J60347 210-EDIT-ACCESS.
J60347******************************************************************
J60347
J60347     MOVE PR30F2-STM-COMPANY          TO DB-COMPANY.
J60347     MOVE SPACES                      TO DB-PROCESS-LEVEL.
J60347     PERFORM 840-FIND-PRSSET1.
J60347     IF (PRSYSTEM-NOTFOUND)
J60347         MOVE 100                     TO CRT-ERROR-NBR
J60347         MOVE PR30F2-FC-FN            TO CRT-FIELD-NBR
J60347         GO TO 210-END.
J60347
J60347     MOVE PR30F2-STM-COMPANY          TO DB-COMPANY.
J60347     MOVE PR30F2-STM-EMPLOYEE         TO DB-EMPLOYEE.
J60347     PERFORM 840-FIND-EMPSET1.
J60347     IF (EMPLOYEE-NOTFOUND)
J60347         MOVE 101                     TO CRT-ERROR-NBR
J60347         MOVE PR30F2-FC-FN            TO CRT-FIELD-NBR
J60347         GO TO 210-END.
J60347
J60347     PERFORM 850-FIND-NLT-PPRSET1.
J60347     IF (PAPOSRULE-FOUND)
J60347         MOVE WS-TRUE                 TO HREMP-PAUSER-FLAG-SW.
J60347
J60347     MOVE PR30F2-FC                   TO HREMP-FC.
J60347     MOVE PR30F2-STM-COMPANY          TO HREMP-COMPANY.
J60347     MOVE SPACES                      TO HREMP-PROCESS-LEVEL.
J60347     MOVE PR30F2-STM-EMPLOYEE         TO HREMP-EMPLOYEE.
J60347     MOVE PR30F2-STM-AUTO-TIMEREC     TO HREMP-AUTO-TIME-REC.
J60347     MOVE PR30F2-STM-AUTO-TIMEREC-FN  TO HREMP-AUTO-TIME-REC-FN.
J60347     PERFORM 2000-HREMP-EDIT-TRAN.
J60347
J60347     MOVE PR30F2-STM-COMPANY          TO DB-COMPANY.
J60347     MOVE SPACES                      TO DB-PROCESS-LEVEL.
J60347     PERFORM 840-FIND-PRSSET1.
J60347     IF (PRSYSTEM-FOUND)
J60347         MOVE PRS-NAME                TO PR30F2-PRS-NAME
J60347     ELSE
J60347         MOVE SPACES                  TO PR30F2-PRS-NAME
J60347     END-IF.
J60347
J60347     MOVE PRS-NAME                    TO PR30F2-PRS-NAME.
J60347     MOVE EMP-LAST-NAME               TO HRWS-LAST-NAME.
J60347     MOVE EMP-FIRST-NAME              TO HRWS-FIRST-NAME.
J60347     MOVE EMP-MIDDLE-INIT             TO HRWS-MIDDLE-INIT.
J60347     MOVE EMP-NAME-SUFFIX             TO HRWS-NAME-SUFFIX.
J60347     MOVE EMP-LAST-NAME-PRE           TO HRWS-LAST-NAME-PRE.
J60347     PERFORM 750-HR-FORMAT-NAME.
J60347     MOVE HRWS-FORMAT-NAME            TO PR30F2-EMP-FULL-NAME.
J60347
J60347 210-END.
J60347
J60347******************************************************************
J60347 480-INQUIRE.
J60347******************************************************************
J60347
J60347     MOVE EMP-AUTO-TIME-REC           TO PR30F2-STM-AUTO-TIMEREC.
J60347     MOVE CRT-INQ-COMPLETE            TO CRT-MESSAGE.
J60347                                  
J60347 480-END.
J60347
J60347******************************************************************
J60347 650-CHANGE.
J60347******************************************************************
J60347
J60347     PERFORM 910-AUDIT-BEGIN.
J60347*
J60347     MOVE PR30F2-STM-COMPANY          TO HREMP-COMPANY.
J60347     MOVE PR30F2-STM-EMPLOYEE         TO HREMP-EMPLOYEE.
J60347     MOVE PR30F2-FC                   TO HREMP-FC.
J60347     MOVE SPACES                      TO HREMP-PROCESS-LEVEL.
J60347     MOVE PR30F2-STM-AUTO-TIMEREC     TO HREMP-AUTO-TIME-REC.
J60347     MOVE PR30F2-STM-AUTO-TIMEREC-FN  TO HREMP-AUTO-TIME-REC-FN.
J60347
J60347     PERFORM 850-FIND-NLT-PPRSET1.
J60347     IF (PAPOSRULE-FOUND)
J60347         MOVE WS-TRUE                 TO HREMP-PAUSER-FLAG-SW.
J60347
J60347     PERFORM 3000-HREMP-PROCESS-TRAN.
J60347
J60347     MOVE PR30F2-STM-COMPANY          TO DB-COMPANY.
J60347     MOVE SPACES                      TO DB-PROCESS-LEVEL.
J60347     PERFORM 840-FIND-PRSSET1.
J60347     IF (PRSYSTEM-FOUND)
J60347         MOVE PRS-NAME                TO PR30F2-PRS-NAME
J60347     ELSE
J60347         MOVE SPACES                  TO PR30F2-PRS-NAME
J60347     END-IF.
J60347
J60347     MOVE CRT-CHG-COMPLETE            TO CRT-MESSAGE.
J60347
J60347     PERFORM 920-AUDIT-END.
J60347
J60347 650-END.
J60347******************************************************************
J60347 PR30S2-TRANSACTION-END.
J60347******************************************************************
