******* BNPTBPD 33 <3813475752>
      ******************************************************************
      *                             BNPTBPD                            *
000100******************************************************************
000200*Use Range operators wherever possible                           *
000300*Edits/Defaulting of Retiree Benefit Stop Date                   *
000400******************************************************************
      *                                                                *
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  307540 | J07540 | DOMESTIC PARTNER                            *
      ******************************************************************
      ******************************************************************
      *     CHANGES ADDED FOR THE BILLING LOGIC FOR EMPERIAN
      *
      *  SDB  08/28/2018  MODIFIED THE LOGIC SO THAT THE
      *                   PLN-CONTRIB-TYPE FIELD IS NOT CHECKED FOR THE
      *                   BILLING TRIGGER.  IF THE PLAN STARTS WITH "Y"
      *                   NO TRIGGER IS WRITTEN, ELSE WRITE THE TRIGGER 
      *  SDB 10/8/2018    ADDED LOGIC TO SEE IF THE PLAN IS ON THE 
      *                   IF10 (LUPTBLDTL) BEFORE WRITING THE TRIGGER     
      ******************************************************************
      *  AI0010  03/24/03  MODIFY THE BENEFIT PLAN ELIGIBILITY COMMON  *
      *                    MODULE TO UTILIZE THE EFFECTIVE DATE AND    *
      *                    END DATE FIELDS IN DETERMINATION OF THE     *
      *                    EMPLOYEES ELIGIBILITY FOR THE BENEFIT PLAN. *
      *  ------   ------   ------------------------------------------  *
      * 1014132 | J10377 | REMOVED VALIDATION FOR HDB-PARTICIPANT = 0  *
      *         |        | UNDER 2700-EDIT-DEPENDENTS FOR FC = "D" TO  *
      *         |        | RESTRICT DELETION WHEN THERE ARE EXISTING   *
      *         |        | DEBENDENT BENEFITS                          *
      * -------   ------   ------------------------------------------  *
      * 1122477 | 122477 | ADD MISSING END-IF. ON 5200-CREATE-BNTRANS  *
      * -------   ------   ------------------------------------------  *
049845* 1049845 | 049845 | FIXED PASSING OF SPACES TO THE SMOKER FIELD *
049845*         |        | IN BN71.1 AND BN72.1 THAT CAUSES SMOKER     *
049845*         |        | FIELD TO BE EMPTY ALWAYS EVEN WHEN UPDATED. *
049845*         |        | ADDED LOGIC THAT PASSES THE VALUE OF SMOKER *
049845*         |        | FIELD FROM BN70.1 AND HR11.1 TO THE SMOKER  *
049845*         |        | FIELD OF BN71.1 AND BN72.1 RESPECTIVELY IF  *
049845*         |        | THE SMOKER FIELD OF BN71.1 AND BN72.1 IS    *
049845*         |        | SPACES.                                     *
      ******************************************************************
AI0095****   ANALYSTS INTERNATIONAL   RDAHL    3/24/03             *****
AI0095****   ENHANCEMENT FOR MOD 9.5 RETROACTIVE BILLING           *****
AI0095******************************************************************
      *  TAG                DESCRIPTION                                *
AI0095*  AI0095             ENHANCEMENT FOR MOD 9.5 RETROACTV BILLING  *
      *  RAY AND GW         TAGS FOR OTHER CHANGES                     *

000500******************************************************************
000600 2000-BNPTB-EDIT-TRAN            SECTION 41.
000700******************************************************************
000800 2000-START.  
000900
           MOVE CRT-PROGRAM-CODE           TO BNPTB-HIPAA-DEFAULT-SW.

001000     PERFORM 2100-EDIT-ACCESS
001100     THRU    2100-END.
001200
001300     IF (ERROR-FOUND)
001400         GO TO 2000-BNPTB-SET-CAT.
001500
001600     PERFORM 2300-EDIT-DTL-TRAN
001700     THRU    2300-END
001800         VARYING I1 FROM 1 BY 1
001900         UNTIL  (I1 > BNPTB-NBR-LINES)
002000         OR     (ERROR-FOUND).
002100
002200 2000-BNPTB-SET-CAT.
002300     IF  (ERROR-FOUND)
002400     AND (CRT-ERROR-CAT          = SPACES)
002500         MOVE "BNPTB"            TO CRT-ERROR-CAT.               
002600
002700     GO TO 2000-END.
002800
002900******************************************************************
003000 2100-EDIT-ACCESS.
003100******************************************************************
003200
003300     MOVE BNPTB-COMPANY              TO DB-COMPANY.
003400     INITIALIZE DB-PROCESS-LEVEL.
003500     PERFORM 840-FIND-PRSSET1.
003600     IF (PRSYSTEM-NOTFOUND)
003700******** Company not setup in HR00
003800         MOVE 52                     TO CRT-ERROR-NBR
003900         MOVE BNPTB-COMPANY-FN       TO CRT-FIELD-NBR
004000         GO TO 2100-END.
004100
004200     MOVE BNPTB-COMPANY              TO DB-COMPANY.
004300     PERFORM 840-FIND-BNCSET1.
004400     IF (BNCOMPANY-NOTFOUND)
004500******** Company not setup in BN00
004600         MOVE 53                     TO CRT-ERROR-NBR
004700         MOVE BNPTB-COMPANY-FN       TO CRT-FIELD-NBR
004800         GO TO 2100-END.
004900
005000     IF (BNPTB-PART-ENT)
005100         MOVE BNPTB-COMPANY          TO DB-COMPANY
005200         MOVE BNPTB-PARTICIPNT       TO DB-PARTICIPNT
005300         PERFORM 840-FIND-PARSET1
005400         IF (PARTICIPNT-NOTFOUND)
005500************ COBRA participant does not exist
005600             MOVE 101                    TO CRT-ERROR-NBR
005700             MOVE BNPTB-PARTICIPNT-FN    TO CRT-FIELD-NBR
005800             GO TO 2100-END.
005900
006000     IF  (BNPTB-PART-ENT)
006100     AND (BNC-CBR-PREM-PCT           = ZEROES)
006200******** Setup COBRA premium percent in BN00.2
006300         MOVE 130                    TO CRT-ERROR-NBR
006400         MOVE BNPTB-FC-FN            TO CRT-FIELD-NBR
006500         GO TO 2100-END.
006600
006700     INITIALIZE DB-EMPLOYEE.
006800     IF (BNPTB-EMPLOYEE              NOT = ZEROES)
006900         MOVE BNPTB-EMPLOYEE         TO DB-EMPLOYEE
007000         PERFORM 840-FIND-EMPSET1
007100         IF (EMPLOYEE-NOTFOUND)
007200************ Employee does not exist
007300             MOVE 50                 TO CRT-ERROR-NBR
007400             MOVE BNPTB-EMPLOYEE-FN  TO CRT-FIELD-NBR
007500             GO TO 2100-END
007600         ELSE
007700             PERFORM 840-FIND-PEMSET1.
007800
007900     MOVE WS-FALSE                   TO BNPTB-DEPEND-SW.
008000     INITIALIZE DB-SEQ-NBR.
008100     PERFORM 850-FIND-NLT-EMDSET1.
008200     IF  (EMDEPEND-FOUND)
008300     AND (EMD-COMPANY                = DB-COMPANY)
008400     AND (EMD-EMPLOYEE               = DB-EMPLOYEE)
008500         MOVE WS-TRUE                TO BNPTB-DEPEND-SW.
008600
008700 2100-END.
008800
008900******************************************************************
009000 2300-EDIT-DTL-TRAN.
009100******************************************************************
009200
009300     IF (BNPTB-LINE-FC (I1)          = SPACES)
009400         GO TO 2300-END.
009500
009600     IF (BNPTB-PLAN-INFO (I1)        = SPACES)
009700******** No plan information on detail line
009800         MOVE 141                    TO CRT-ERROR-NBR
009900         MOVE BNPTB-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
010000         GO TO 2300-END.
010100
010200     IF  (BNPTB-LINE-FC (I1)         = "C" OR "D" OR "S")
010300     AND (BNPTB-RECORD-ORIG (I1)     = "P")
010400******** Cannot delete or change or stop on plan line
010500         MOVE 102                    TO CRT-ERROR-NBR
010600         MOVE BNPTB-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
010700         GO TO 2300-END.
010800
010900     IF  (BNPTB-LINE-FC (I1)         = "A")
011000     AND (BNPTB-RECORD-ORIG (I1)     = "B")
011100******** Add a benefit on plan line
011200         MOVE 106                    TO CRT-ERROR-NBR
011300         MOVE BNPTB-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
011400         GO TO 2300-END.
011500
011600     IF (BNPTB-START-DATE (I1)       = ZEROES)
011700         IF (BNPTB-PART-ENT)
011800             MOVE PAR-OCCUR-DATE     TO BNPTB-START-DATE (I1)
011900         ELSE
012000         IF (EMP-TERM-DATE           = ZEROES)
012100             MOVE WS-SYSTEM-DATE-YMD TO BNPTB-START-DATE (I1)
012200         ELSE
012300             MOVE EMP-TERM-DATE      TO BNPTB-START-DATE (I1).
012400
012500     IF  (BNPTB-LINE-FC (I1)        = "A")
012600     AND (BNPTB-PART-ENT)
012700     AND (BNPTB-START-DATE (I1)     < PAR-OCCUR-DATE)
012800         MOVE 203                                 TO CRT-ERROR-NBR
012900         MOVE BNPTB-START-DATE-FN (I1)            TO CRT-FIELD-NBR
013000         GO TO 2300-END.
013100
013200     MOVE BNPTB-COMPANY              TO DB-COMPANY.
013300     MOVE BNPTB-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
013400     MOVE BNPTB-PLAN-CODE (I1)       TO DB-PLAN-CODE.
013500     PERFORM 840-FIND-PLNSET1.
013600     IF (PLAN-NOTFOUND)
013700******** Plan does not exist
013800         MOVE 103                    TO CRT-ERROR-NBR
013900         MOVE BNPTB-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
014000         GO TO 2300-END.
014100
014200     IF (PLN-FLEX-PLAN               NOT = SPACES)
014300         MOVE PLN-COMPANY            TO DB-COMPANY
014400         MOVE PLN-FLEX-PLAN          TO DB-FLEX-PLAN
014500         PERFORM 840-FIND-FLPSET1
014600         IF (FLEXPLAN-NOTFOUND)
014700************ Flex plan does not exist
014800             MOVE 250                    TO CRT-ERROR-NBR
014900             MOVE BNPTB-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
015000             GO TO 2300-END.
015100
015200     MOVE BNPTB-COMPANY              TO DB-COMPANY.
015300     IF  (BNPTB-PART-ENT)
015400         MOVE ZEROES                 TO DB-EMPLOYEE
015500     ELSE
015600         MOVE BNPTB-EMPLOYEE         TO DB-EMPLOYEE.
015700     MOVE BNPTB-PARTICIPNT           TO DB-PARTICIPNT.
015800     MOVE BNPTB-START-DATE (I1)      TO DB-START-DATE.
015900     MOVE BNPTB-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
016000     MOVE BNPTB-PLAN-CODE (I1)       TO DB-PLAN-CODE.
016100
016200     IF  (BNPTB-LINE-FC (I1)         = "A")
016300     AND (BNPTB-RETIREE-ENT)
           AND (CRT-PROGRAM-CODE       NOT = "BN105")
016400         PERFORM 2305-EDIT-PLAN-ELIGIBILITY
016500         THRU    2305-END
016600         IF (ERROR-FOUND)
016700             GO TO 2300-END.
016800
           MOVE BNPTB-COMPANY              TO DB-COMPANY.
           IF  (BNPTB-PART-ENT)
               MOVE ZEROES                 TO DB-EMPLOYEE
           ELSE
               MOVE BNPTB-EMPLOYEE         TO DB-EMPLOYEE.
           MOVE BNPTB-PARTICIPNT           TO DB-PARTICIPNT.
           MOVE BNPTB-START-DATE (I1)      TO DB-START-DATE.
           MOVE BNPTB-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
           MOVE BNPTB-PLAN-CODE (I1)       TO DB-PLAN-CODE.

016900     IF  (BNPTB-PART-ENT)
017000     AND (PLN-CONTRIB-TYPE           = "5")
017100     AND (PAR-EMPLOYEE               = ZEROES)
017200******** Participant must be associated to an employee
017300         MOVE 500                    TO CRT-ERROR-NBR
017400         MOVE BNPTB-PARTICIPNT-FN    TO CRT-FIELD-NBR
017500         GO TO 2300-END.
017600
017700     PERFORM 840-FIND-PTBSET1.
017800     IF  (PARTBEN-NOTFOUND)
017900     AND (BNPTB-LINE-FC (I1)         = "C" OR "D" OR "S")
018000******** Benefit does not exist
018100         MOVE 109                    TO CRT-ERROR-NBR
018200         MOVE BNPTB-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
018300         GO TO 2300-END.
018400
018500     IF  (PARTBEN-FOUND)
018600     AND (BNPTB-LINE-FC (I1)         = "A")
018700******** Benefit already exist
018800         MOVE 110                    TO CRT-ERROR-NBR
018900         MOVE BNPTB-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
019000         GO TO 2300-END.
019100
019200     IF  (BNPTB-LINE-FC (I1)         NOT = "A")
019300     AND (PTB-START-DATE             NOT = BNPTB-START-DATE (I1))
019400******** Cannot change start date
019500         MOVE 210                    TO CRT-ERROR-NBR
019600         MOVE BNPTB-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
019700         GO TO 2300-END.
019800
J93630     IF  (BNPTB-LINE-FC (I1)         = "D")
J93630******** Cannot delete, ACA records exist
J93630         MOVE BNPTB-COMPANY          TO DB-COMPANY
J93630         IF (BNPTB-PART-ENT)
J52049             MOVE "3"                TO HACAF2-TYPE
J93630             MOVE BNPTB-PARTICIPNT   TO DB-PARTICIPNT
J52049             MOVE BNPTB-PARTICIPNT   TO HACAF2-PARTICIPNT
J93630             MOVE ZEROES             TO DB-EMPLOYEE
J52049             MOVE ZEROES             TO HACAF2-EMPLOYEE
J93630             PERFORM 840-FIND-PARSET1
J93630             IF (PARTICIPNT-FOUND)
J93630               IF  (PAR-EMPLOYEE NOT = ZEROES)
J93630               AND (PAR-DEPENDENT = ZEROES)
J93630                 MOVE PAR-EMPLOYEE   TO DB-EMPLOYEE
J93630               END-IF
J93630             END-IF
J93630         END-IF
J93630         IF (BNPTB-RETIREE-ENT)
J52049             MOVE "2"                TO HACAF2-TYPE
J93630             MOVE ZEROES             TO DB-PARTICIPNT
J52049             MOVE ZEROES             TO HACAF2-PARTICIPNT
J93630             MOVE BNPTB-EMPLOYEE     TO DB-EMPLOYEE
J52049             MOVE BNPTB-EMPLOYEE     TO HACAF2-EMPLOYEE
J93630         END-IF
J93630         MOVE ZEROES                 TO BNPTB-SAVE-END-YR
J93630         IF  (BNPTB-STOP-DATE (I1) NOT = ZEROES)
J93630             MOVE BNPTB-STOP-DATE (I1)
J93630                                     TO BNPTB-START-DATE-CALC
J93630             MOVE BNPTB-START-YYYY   TO BNPTB-SAVE-END-YR
J93630         ELSE
J93630             MOVE WS-SYSTEM-DATE-YMD TO BNPTB-START-DATE-CALC
J93630             MOVE BNPTB-START-YYYY   TO BNPTB-SAVE-END-YR
J93630         END-IF
J93630         MOVE BNPTB-START-DATE (I1)  TO BNPTB-START-DATE-CALC
J93630         MOVE BNPTB-START-YYYY       TO BNPTB-SAVE-BEG-YR
J52049         MOVE "I"                    TO HACAF2-FC
J52049         MOVE BNPTB-COMPANY          TO HACAF2-COMPANY
J93630         IF  (BNPTB-SAVE-BEG-YR  >= BNPTB-SAVE-END-YR)
J52049             MOVE BNPTB-SAVE-BEG-YR  TO HACAF2-YEAR
J52049             INVOKE "HACA.2"           
J09473             MOVE ZEROES             TO CRT-ERROR-NBR
J52049             IF (HACAF2-ERROR-NBR NOT = ZEROES)
J93630             AND (BNPTB-FIRST-XMIT NOT = "1")
J93630                 MOVE 231            TO CRT-ERROR-NBR
J93630                 MOVE BNPTB-LINE-FC-FN (I1)
J93630                                     TO CRT-FIELD-NBR
J93630                 MOVE "1"            TO BNPTB-FIRST-XMIT
J93630                 GO TO 2300-END
J09473             ELSE
J09473                 MOVE SPACES         TO BNPTB-FIRST-XMIT
J93630             END-IF 
J93630         ELSE
J93630           PERFORM
J93630             VARYING I9 FROM BNPTB-SAVE-BEG-YR BY 1
J93630             UNTIL (I9 > BNPTB-SAVE-END-YR)
J52049               MOVE I9               TO HACAF2-YEAR
J52049*              INVOKE "HACA.2"
J52049               PERFORM 2301-INVOKE-HACA
J09473               THRU    2301-END 
J52049               IF (HACAF2-ERROR-NBR NOT = ZEROES)
J93630               AND (BNPTB-FIRST-XMIT NOT = "1")
J93630                 MOVE 231            TO CRT-ERROR-NBR
J93630                 MOVE BNPTB-LINE-FC-FN (I1) 
J93630                                     TO CRT-FIELD-NBR
J93630                 MOVE "1"            TO BNPTB-FIRST-XMIT
J93630                 GO TO 2300-END
J93630               END-IF
J93630           END-PERFORM
J09473           MOVE SPACES               TO BNPTB-FIRST-XMIT
J93630         END-IF
J93630     END-IF.
J93630
019900     IF  (PLN-FLEX-PLAN              NOT = SPACES)
020000     AND (PLN-CONTRIB-TYPE           = "5")
020100         PERFORM 2330-GET-FLEX-START-STOP
020200         THRU    2330-END.
020300
020400     IF (BNPTB-LINE-FC (I1)          = "A" OR "C")
020500         PERFORM 2400-EDIT-DTL-DATA
020600         THRU    2400-END.
020700
020800     IF (BNPTB-LINE-FC (I1)          = "D")
020900         PERFORM 2600-EDIT-DTL-DELETE
021000         THRU    2600-END.
021100
021200     IF (BNPTB-LINE-FC (I1)          = "S")
021300         PERFORM 2800-EDIT-DTL-STOP
021400         THRU    2800-END.
021500
021600 2300-END.     
021700
J09473******************************************************************
J52049 2301-INVOKE-HACA.
J09473******************************************************************
J09473
J09473* Needed to create this routine.  When INVOKE was in the loop,
J09473* received compile error "Unknown Node Type "
J09473
J52049     INVOKE "HACA.2".
J09473     MOVE ZEROES                     TO CRT-ERROR-NBR.
J09473
J09473 2301-END.
J09473
021800******************************************************************
021900 2305-EDIT-PLAN-ELIGIBILITY.
022000******************************************************************
022100
022200*
022300**** CALL PLAN ELIGIBILITY SECTION WHICH VALIDATES ONLY
022400**** GROUP & ZIP CODE
022500*
022600     MOVE BNPTB-COMPANY              TO BNPEWS-COMPANY.
022700     MOVE BNPTB-PLAN-TYPE (I1)       TO BNPEWS-PLAN-TYPE.
022800     MOVE BNPTB-PLAN-CODE (I1)       TO BNPEWS-PLAN-CODE.
022900     MOVE BNPTB-EMPLOYEE             TO BNPEWS-EMPLOYEE.
AI0010     MOVE BNPTB-ENROLLMENT-DATE      TO BNPEWS-START-DATE.
023000     PERFORM 5000-EDIT-GROUP-N-ZIP-70.
023100     IF (ERROR-FOUND)
023200         MOVE BNPTB-PARTICIPNT-FN    TO CRT-FIELD-NBR
023300         GO TO 2305-END.
023400
           MOVE BNPTB-COMPANY              TO BNEDWS-COMPANY.
           MOVE BNPTB-PLAN-TYPE (I1)       TO BNEDWS-PLAN-TYPE.
           MOVE BNPTB-PLAN-CODE (I1)       TO BNEDWS-PLAN-CODE.
           MOVE BNPTB-EMPLOYEE             TO BNEDWS-EMPLOYEE.
           IF (BNPTB-START-DATE (I1)       = ZEROES)
               MOVE BNPTB-ENROLLMENT-DATE  TO BNEDWS-AS-OF-DATE
               INITIALIZE BNEDWS-START-DATE
           ELSE
               MOVE BNPTB-START-DATE (I1)  TO BNEDWS-AS-OF-DATE
                                              BNEDWS-START-DATE.
       
           INITIALIZE BNEDWS-FROM-MAGIC-SW.
           PERFORM 5000-ELIGIBILITY-DATE-CALC-70.
           IF (ERROR-FOUND)
               MOVE BNPTB-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
               GO TO 2305-END.

           IF (BNEDWS-START-DATE < BNEDWS-ELIGIBILITY-DATE)
               MOVE 125                       TO CRT-ERROR-NBR
               MOVE BNPTB-START-DATE-FN (I1)  TO CRT-FIELD-NBR
               MOVE BNEDWS-ELIGIBILITY-DATE   TO CRT-ERR-VAR1
               GO TO 2305-END.

           IF (BNPTB-START-DATE (I1)       NOT = ZEROES)
               PERFORM 2310-EDIT-START-DATE
               THRU    2310-END
               IF (ERROR-FOUND)
                   GO TO 2305-END.

       2305-END.

      ******************************************************************
       2310-EDIT-START-DATE.
      ******************************************************************

           IF (BNEDWS-INIT-ENT-POINT   = "1" OR "2")
               PERFORM 2312-USE-PLAN-ENTRY-POINTS
               THRU    2312-END.

       2310-END.

      ******************************************************************
       2312-USE-PLAN-ENTRY-POINTS.
      ******************************************************************

           MOVE BNPTB-START-DATE (I1)      TO BNPTB-START-DATE-CALC.

           PERFORM
               VARYING BNPTB-I1 FROM 1 BY 1
               UNTIL  (BNPTB-I1            > BNEDWS-LAST-EP-INDEX)
               OR     (BNEDWS-ENTRY-MMDD (BNPTB-I1)
                                           = BNPTB-START-DATE-MMDD)

               CONTINUE
           END-PERFORM.

           IF (BNPTB-I1                    > BNEDWS-LAST-EP-INDEX)
      ******** Start date must match entry point
               MOVE 126                       TO CRT-ERROR-NBR
               MOVE BNPTB-START-DATE-FN (I1)  TO CRT-FIELD-NBR
               GO TO 2312-END.

       2312-END.

023700******************************************************************
023800 2330-GET-FLEX-START-STOP.
023900******************************************************************
024000
024100     MOVE BNPTB-START-DATE (I1)      TO BNPTB-START-DATE-CALC.
024200     IF (BNPTB-START-DATE-MMDD       < FLP-ENTRY-MMDD)
024300         MOVE FLP-ENTRY-MMDD         TO BNPTB-START-DATE-MMDD
024400         SUBTRACT 1                  FROM BNPTB-START-YYYY
024500         MOVE BNPTB-START-DATE-CALC  TO BNPTB-EFD-START-DATE
024600         ADD 1                       TO BNPTB-START-YYYY
024700         MOVE BNPTB-START-DATE-CALC  TO WSDR-FR-DATE
024800         PERFORM 900-DATE-TO-JULIAN
024900         COMPUTE WSDR-JULIAN-DAYS    = WSDR-JULIAN-DAYS
025000                                     - 1
025100         PERFORM 900-JULIAN-TO-DATE    
025200         MOVE WSDR-FR-DATE           TO BNPTB-EFD-STOP-DATE
025300     ELSE
025400     IF (BNPTB-START-DATE-MMDD       > FLP-ENTRY-MMDD)
025500         MOVE FLP-ENTRY-MMDD         TO BNPTB-START-DATE-MMDD
025600         MOVE BNPTB-START-DATE-CALC  TO BNPTB-EFD-START-DATE
025700         ADD 1                       TO BNPTB-START-YYYY
025800         MOVE BNPTB-START-DATE-CALC  TO WSDR-FR-DATE
025900         PERFORM 900-DATE-TO-JULIAN
026000         COMPUTE WSDR-JULIAN-DAYS    = WSDR-JULIAN-DAYS
026100                                     - 1
026200         PERFORM 900-JULIAN-TO-DATE    
026300         MOVE WSDR-FR-DATE           TO BNPTB-EFD-STOP-DATE
026400     ELSE
026500     IF (BNPTB-START-DATE-MMDD       = FLP-ENTRY-MMDD)
026600         MOVE BNPTB-START-DATE-CALC  TO BNPTB-EFD-START-DATE
026700         ADD 1                       TO BNPTB-START-YYYY
026800         MOVE BNPTB-START-DATE-CALC  TO WSDR-FR-DATE
026900         PERFORM 900-DATE-TO-JULIAN
027000         COMPUTE WSDR-JULIAN-DAYS    = WSDR-JULIAN-DAYS
027100                                     - 1
027200         PERFORM 900-JULIAN-TO-DATE
027300         MOVE WSDR-FR-DATE           TO BNPTB-EFD-STOP-DATE.
027400
027500 2330-END.
027600
027700******************************************************************
027800 2400-EDIT-DTL-DATA.
027900******************************************************************
028000
028100     IF  (BNPTB-START-DATE (I1)      > BNPTB-STOP-DATE (I1))
028200     AND (BNPTB-STOP-DATE (I1)       NOT = ZEROES)
028300******** Start date cannot be greater than stop date
028400         MOVE 111                        TO CRT-ERROR-NBR
028500         MOVE BNPTB-STOP-DATE-FN (I1)    TO CRT-FIELD-NBR
028600         GO TO 2400-END.
028700
028800*     IF  (BNPTB-LINE-FC  (I1)        = "C")
028900*     AND (BNPTB-STOP-DATE (I1)       NOT = PTB-STOP-DATE)
029000******** Cannot change stop date; Use "S"top line function code
029100*         MOVE 160                    TO CRT-ERROR-NBR
029200*         MOVE BNPTB-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
029300*         GO TO 2400-END.
029400
029500     IF (BNPTB-PART-ENT)
029600         IF (BNPTB-START-DATE (I1)   >= PAR-TERM-DATE)
029700************ Start date must be less than extended coverage term date
029800             MOVE 118                        TO CRT-ERROR-NBR
029900             MOVE BNPTB-START-DATE-FN (I1)   TO CRT-FIELD-NBR
030000             GO TO 2400-END
030100         ELSE
030200         IF (PLN-FLEX-PLAN            = SPACES)
030300             IF (BNPTB-STOP-DATE (I1) > PAR-TERM-DATE)
030400**************** Stop date must not exceed extended coverage term date
030500                 MOVE 119                     TO CRT-ERROR-NBR
030600                 MOVE BNPTB-STOP-DATE-FN (I1) TO CRT-FIELD-NBR
030700                 GO TO 2400-END
030800             ELSE
030900             IF (BNPTB-STOP-DATE (I1) = ZEROES)
031000                 MOVE PAR-TERM-DATE       TO BNPTB-STOP-DATE (I1)
031500             END-IF
031600             END-IF
031700         ELSE
031800         IF (BNPTB-STOP-DATE (I1)        = ZEROES)
031900             IF (BNPTB-EFD-STOP-DATE     < PAR-TERM-DATE)
032000                 MOVE BNPTB-EFD-STOP-DATE  TO BNPTB-STOP-DATE (I1)
032100             ELSE
032200                 MOVE PAR-TERM-DATE       TO BNPTB-STOP-DATE (I1)
032700             END-IF
032800         ELSE
032900         IF (BNPTB-STOP-DATE (I1)        > PAR-TERM-DATE)
033000************ Stop date must not exceed extended coverage term date
033100             MOVE 119                     TO CRT-ERROR-NBR
033200             MOVE BNPTB-STOP-DATE-FN (I1) TO CRT-FIELD-NBR
033300             GO TO 2400-END
033400         ELSE
033500         IF (BNPTB-STOP-DATE (I1)        > BNPTB-EFD-STOP-DATE)
033600************ Stop date cannot be > flex year stop date
033700             MOVE 155                        TO CRT-ERROR-NBR
033800             MOVE BNPTB-STOP-DATE-FN (I1)    TO CRT-FIELD-NBR
033900             GO TO 2400-END.
034000
034100     IF  (BNPTB-START-DATE (I1)      < PLN-START-DATE)
034200     OR ((BNPTB-START-DATE (I1)      > PLN-STOP-DATE)
034300     AND (PLN-STOP-DATE              NOT = ZEROES))
034400******** Start date must be within start and stop date
034500         MOVE 114                        TO CRT-ERROR-NBR
034600         MOVE BNPTB-START-DATE-FN (I1)   TO CRT-FIELD-NBR
034700         GO TO 2400-END.
034800
034900     MOVE BNPTB-START-DATE (I1)      TO DB-START-DATE.
035000
035100     IF (BNPTB-LINE-FC (I1)          = "A")
035200         PERFORM 850-FIND-NLT-PTBSET4
035300         IF  (PARTBEN-FOUND)
035400         AND (PTB-COMPANY            = DB-COMPANY)
035500         AND (PTB-PARTICIPNT         = DB-PARTICIPNT)
035600         AND (PTB-EMPLOYEE           = DB-EMPLOYEE)
035700         AND (PTB-PLAN-TYPE          = DB-PLAN-TYPE)
035800         AND (PTB-PLAN-CODE          = DB-PLAN-CODE)
035900             PERFORM 2410-EDIT-EXISTING-PREV-PTB
036000             THRU    2410-END
036100             IF (ERROR-FOUND)
036200                 GO TO 2400-END.
036300
036400     IF (BNPTB-LINE-FC (I1)          = "A")
036500     OR ((BNPTB-LINE-FC (I1)         = "C")
036600     AND (BNPTB-STOP-DATE (I1)       > PTB-STOP-DATE))
036700     OR ((BNPTB-LINE-FC (I1)         = "C")
036800     AND (BNPTB-STOP-DATE (I1)       NOT = PTB-STOP-DATE)
036900     AND (BNPTB-STOP-DATE (I1)       = ZEROES))
037000         PERFORM 850-FIND-NLT-PTBSET5
037100         IF  (PARTBEN-FOUND)
037200         AND (PTB-COMPANY            = DB-COMPANY)
037300         AND (PTB-PARTICIPNT         = DB-PARTICIPNT)
037400         AND (PTB-EMPLOYEE           = DB-EMPLOYEE)
037500         AND (PTB-PLAN-TYPE          = DB-PLAN-TYPE)
037600         AND (PTB-PLAN-CODE          = DB-PLAN-CODE)
037700             MOVE WS-FALSE           TO BNPTB-STOP-DFLT-SW
037800             PERFORM 2420-EDIT-EXISTING-POST-PTB
037900             THRU    2420-END
038000                 UNTIL  (ERROR-FOUND)
038100                 OR     (BNPTB-STOP-DEFAULTED)
038200                 OR     (PARTBEN-NOTFOUND)
038300                 OR     (PTB-COMPANY          NOT = DB-COMPANY)
038400                 OR     (PTB-PARTICIPNT       NOT = DB-PARTICIPNT)
038500                 OR     (PTB-EMPLOYEE         NOT = DB-EMPLOYEE)
038600                 OR     (PTB-PLAN-TYPE        NOT = DB-PLAN-TYPE)
038700                 OR     (PTB-PLAN-CODE        NOT = DB-PLAN-CODE)
038800                 OR    ((BNPTB-STOP-DATE (I1) NOT = ZEROES)
038900                 AND    (BNPTB-STOP-DATE (I1) < PTB-START-DATE))
039000             IF (ERROR-FOUND)
039100                 GO TO 2400-END.
039200*
039300**** NOTE: THE PROGRAM MAY NO LONGER BE ON THE PARTBEN RECORD FROM
039400**** THE SCREEN AT THIS POINT, BECAUSE 2440 AND 2450 MAY HAVE YOU
039500**** POINTING AT ANOTHER PARTBEN RECORD.
039600*
039700     IF (DB-START-DATE               NOT = BNPTB-START-DATE (I1))
039800         MOVE BNPTB-START-DATE (I1)  TO DB-START-DATE.
039900     PERFORM 840-FIND-PTBSET1.
040000
040100     IF (PLN-COVERAGE-TYPE           NOT = "0")
040200         PERFORM 2440-CALC-COVERAGE
040300         THRU    2440-END
040400         IF (ERROR-FOUND)
040500             GO TO 2400-END.
040600
2018****** IF (PLN-CONTRIB-TYPE            NOT = "0")
SDB        IF (PLN-PLAN-CODE(1:1)          NOT = "Y")
040700         PERFORM 2450-CALC-CONTRIB
040800         THRU    2450-END
040900         IF (ERROR-FOUND)
041000             GO TO 2400-END.
041100
041200     IF  (PLN-CONTRIB-TYPE           = "5")
041300     AND (PLN-FLEX-PLAN              NOT = SPACES)
041400         PERFORM 2460-EDIT-ANN-LIMITS
041500         THRU    2460-END.
041600
041700     IF (BNPTB-PLAN-TYPE (I1)        = "HL" OR "DN" OR "DL")
041800     OR (BNPTB-PLAN-TYPE (I1)        = "EL")
041900         PERFORM 2700-EDIT-DEPENDENTS
042000         THRU    2700-END
042100         IF (ERROR-FOUND)
042200             GO TO 2400-END.
042300
           PERFORM 2850-EDIT-HIPAA
           THRU    2850-END.

042400 2400-END.
042500
042600******************************************************************
042700 2410-EDIT-EXISTING-PREV-PTB.
042800******************************************************************
042900
043000     MOVE WS-FALSE                   TO BNPTB-DEL-PTB-SW
043100                                        BNPTB-CHG-PTB-SW.
043200
043300     PERFORM 2412-EDIT-CHANGING-PTB
043400     THRU    2412-END
043500         VARYING I2 FROM 1 BY 1
043600         UNTIL  (I2 = I1)
043700         OR     (BNPTB-DELETING-PTB)
043800         OR     (BNPTB-CHANGING-PTB).
043900
044000     IF  (BNPTB-NOT-CHANGING-PTB)
044100     AND (PTB-STOP-DATE              NOT = ZEROES)
044200     AND (PTB-STOP-DATE              >= BNPTB-START-DATE (I1))
044300******** Already enrolled for specified time period
044400         MOVE 124                        TO CRT-ERROR-NBR
044500         MOVE BNPTB-STOP-DATE-FN (I1)    TO CRT-FIELD-NBR
044600         GO TO 2410-END.
044700
044800 2410-END.
044900
045000******************************************************************
045100 2412-EDIT-CHANGING-PTB.
045200******************************************************************
045300
045400     IF  (BNPTB-LINE-FC (I2)         = "D")
045500     AND (BNPTB-PLAN-CODE (I2)       = PTB-PLAN-CODE)
045600     AND (BNPTB-START-DATE (I2)      = PTB-START-DATE)
045700         MOVE WS-TRUE                TO BNPTB-DEL-PTB-SW      
045800         GO TO 2412-END.
045900
046000     IF  (BNPTB-LINE-FC (I2)         = "S")
046100     AND (BNPTB-PLAN-CODE (I2)       = PTB-PLAN-CODE)
046200     AND (BNPTB-START-DATE (I2)      = PTB-START-DATE)
046300         IF  (BNPTB-STOP-DATE (I2)   NOT = ZEROES)
046400         AND (BNPTB-STOP-DATE (I2)   >= BNPTB-START-DATE (I1))
046500************ Already enrolled for specified time period
046600             MOVE 124                        TO CRT-ERROR-NBR
046700             MOVE BNPTB-STOP-DATE-FN (I1)    TO CRT-FIELD-NBR
046800             MOVE WS-TRUE                    TO BNPTB-CHG-PTB-SW
046900             GO TO 2412-END
047000         ELSE
047100             MOVE WS-TRUE                    TO BNPTB-CHG-PTB-SW
047200             GO TO 2412-END.
047300
047400 2412-END.
047500
047600******************************************************************
047700 2420-EDIT-EXISTING-POST-PTB.
047800******************************************************************
047900
048000     IF (PTB-START-DATE              = BNPTB-START-DATE (I1))
048100         GO TO 2420-FIND-NEXT-PTBSET5.
048200
048300     MOVE WS-FALSE                   TO BNPTB-DEL-PTB-SW.
048400
048500     PERFORM 2422-EDIT-DELETING-POST-PTB
048600     THRU    2422-END
048700         VARYING I2 FROM 1 BY 1
048800         UNTIL  (BNPTB-DELETING-PTB)
048900         OR     (I2 > BNPTB-NBR-LINES).
049000
049100     IF (BNPTB-DELETING-PTB)
049200         GO TO 2420-FIND-NEXT-PTBSET5.
049300
049400     IF (BNPTB-NOT-DELETING-PTB)
049500         IF (BNPTB-STOP-DATE (I1)    = ZEROES)
049600             MOVE WS-TRUE            TO BNPTB-STOP-DFLT-SW
049700             MOVE PTB-START-DATE     TO WSDR-FR-DATE
049800             PERFORM 900-DATE-TO-JULIAN
049900             SUBTRACT 1              FROM WSDR-JULIAN-DAYS
050000             PERFORM 900-JULIAN-TO-DATE   
050100             MOVE WSDR-FR-DATE       TO BNPTB-STOP-DATE (I1)
050200         ELSE
050300         IF (BNPTB-STOP-DATE (I1)    >= PTB-START-DATE)
050400************ Already enrolled for specified time period
050500             MOVE 124                    TO CRT-ERROR-NBR
050600             MOVE BNPTB-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
050700             GO TO 2420-END.
050800
050900 2420-FIND-NEXT-PTBSET5.
051000     PERFORM 860-FIND-NEXT-PTBSET5.
051100
051200 2420-END.
051300
051400******************************************************************
051500 2422-EDIT-DELETING-POST-PTB.
051600******************************************************************
051700
051800     IF  (BNPTB-LINE-FC (I2)         = "D")
051900     AND (BNPTB-PLAN-CODE (I2)       = PTB-PLAN-CODE)
052000     AND (BNPTB-START-DATE (I2)      = PTB-START-DATE)
052100         MOVE WS-TRUE                TO BNPTB-DEL-PTB-SW.
052200
052300 2422-END.
052400
052500******************************************************************
052600 2440-CALC-COVERAGE.
052700******************************************************************
052800
052900*
053000**** CALCULATE COVERAGE AMOUNT
053100*
053200     MOVE BNPTB-COMPANY              TO BNCVWS-COMPANY.
053300     MOVE BNPTB-PLAN-TYPE       (I1) TO BNCVWS-PLAN-TYPE.
053400     MOVE BNPTB-PLAN-CODE       (I1) TO BNCVWS-PLAN-CODE.
053500     IF (BNPTB-RETIREE-ENT)
053600         MOVE "R"                    TO BNCVWS-COVER-TYPE
053700         MOVE BNPTB-EMPLOYEE         TO BNCVWS-EMPLOYEE
053800     ELSE
053900         MOVE "C"                    TO BNCVWS-COVER-TYPE
054000         MOVE BNPTB-PARTICIPNT       TO BNCVWS-EMPLOYEE.
054100     MOVE BNPTB-START-DATE      (I1) TO BNCVWS-AS-OF-DATE
054200                                        BNCVWS-START-DATE.
054300     MOVE BNPTB-STOP-DATE       (I1) TO BNCVWS-STOP-DATE.
054400     MOVE BNPTB-COVER-OPT       (I1) TO BNCVWS-IN-COVER-OPT.
054500     MOVE BNPTB-COVER-AMT       (I1) TO BNCVWS-IN-COVER-AMT.
054600     MOVE BNPTB-LINE-FC         (I1) TO BNCVWS-FC.
054700     IF (BNPTB-LINE-FC          (I1) = "C")
054800         MOVE PTB-COV-UPD-DT         TO BNCVWS-COV-UPD-DT
054900         MOVE PTB-COV-GROUP          TO BNCVWS-COV-GROUP.
055000     IF (BNPTB-PART-ENT)
055100         MOVE PAR-BIRTHDATE          TO BNCVWS-PAR-BIRTHDATE.
055200
055300     PERFORM 5000-DO-COVERAGE-70.
055400
055500     IF (ERROR-FOUND)    
055600         PERFORM 2442-SET-ERROR-CURSOR
055700         THRU    2442-END
055800         GO TO 2440-END.
055900
056000     MOVE BNCVWS-COVER-OPT           TO BNPTB-COVER-OPT  (I1).
056100     MOVE BNCVWS-COVER-AMT           TO BNPTB-COVER-AMT  (I1).
056200
056300     MOVE BNCVWS-CVR-START-DATE      TO BNPTB-COV-UPD-DT (I1).
056400     MOVE BNCVWS-CVR-GROUP-NAME      TO BNPTB-COV-GROUP  (I1).
056500
056600 2440-END.
056700
056800******************************************************************
056900 2442-SET-ERROR-CURSOR.
057000******************************************************************
057100
057200     IF (BNCVWS-FIELD-NBR                 = 1)
057300         MOVE BNPTB-COVER-OPT-FN    (I1) TO CRT-FIELD-NBR
057400     ELSE
057500     IF (BNCVWS-FIELD-NBR                 = 3)
057600         MOVE BNPTB-COVER-AMT-FN    (I1) TO CRT-FIELD-NBR
057700     ELSE
057800         MOVE BNPTB-LINE-FC-FN      (I1) TO CRT-FIELD-NBR.
057900
058000 2442-END.
058100
058200******************************************************************
058300 2450-CALC-CONTRIB.
058400******************************************************************
058500
058600*
058700**** CALCULATE CONTRIBUTION AMOUNT
058800*
058900     MOVE BNPTB-COMPANY              TO BNCTWS-COMPANY.
059000     MOVE BNPTB-PLAN-TYPE       (I1) TO BNCTWS-PLAN-TYPE.
059100     MOVE BNPTB-PLAN-CODE       (I1) TO BNCTWS-PLAN-CODE.
059200     IF (BNPTB-RETIREE-ENT)
059300         MOVE "R"                    TO BNCTWS-COVER-TYPE
059400         MOVE BNPTB-EMPLOYEE         TO BNCTWS-EMPLOYEE
059500     ELSE
059600         MOVE "C"                    TO BNCTWS-COVER-TYPE
059700         MOVE BNPTB-PARTICIPNT       TO BNCTWS-EMPLOYEE.
059800     MOVE BNPTB-START-DATE      (I1) TO BNCTWS-AS-OF-DATE
059900                                        BNCTWS-START-DATE.
060000     MOVE BNPTB-STOP-DATE       (I1) TO BNCTWS-STOP-DATE.
060100
060200     MOVE BNPTB-COVER-OPT       (I1) TO BNCTWS-CONTRIB-OPT
060300                                        BNCTWS-CYC-REMAIN.
060400     MOVE BNPTB-COVER-AMT       (I1) TO BNCTWS-COVER-AMT.
060500     MOVE BNPTB-EMP-AFT-CONT    (I1) TO BNCTWS-IN-EMP-AT-CONT.
060600     MOVE BNPTB-PAY-PER-AMT     (I1) TO BNCTWS-IN-PAY-PER-AMT.
060700     MOVE BNPTB-ANNUAL-AMT      (I1) TO BNCTWS-IN-ANNUAL-AMT.
060800
060900     MOVE "A"                        TO BNCTWS-IN-PCT-AMT-FLAG
061000                                        BNCTWS-IN-PRE-AFT-FLAG.
061100
061200     MOVE BNPTB-SMOKER-FLAG     (I1) TO BNCTWS-IN-SMOKER-SW.
061300     IF (BNPTB-PART-ENT)
061400         MOVE PAR-BIRTHDATE          TO BNCTWS-PAR-BIRTHDATE.
061500         MOVE PAR-SMOKER             TO BNCTWS-PAR-SMOKER.
061600     MOVE BNPTB-LINE-FC         (I1) TO BNCVWS-FC.
061700     IF (BNPTB-LINE-FC          (I1) = "C")
061800         MOVE PTB-PREM-UPD-DT        TO BNCTWS-PREM-UPD-DT
061900         MOVE PTB-PREM-GROUP         TO BNCTWS-PREM-GROUP.
062000
062100     PERFORM 5000-DO-CONTRIBUTION-70.
062200
062300     IF (ERROR-FOUND)
062400         PERFORM 2452-SET-ERROR-CURSOR
062500         THRU    2452-END
062600         GO TO 2450-END.
062700
062800     MOVE BNCTWS-EMP-AT-CONT         TO BNPTB-EMP-AFT-CONT (I1).
062900     MOVE BNCTWS-COMP-CONT           TO BNPTB-COMP-CONT    (I1).
063000     MOVE BNCTWS-CYC-REMAIN          TO BNPTB-COVER-OPT    (I1).
063100     MOVE BNCTWS-PAY-PER-AMT         TO BNPTB-PAY-PER-AMT  (I1).
063200     MOVE BNCTWS-ANNUAL-AMT          TO BNPTB-ANNUAL-AMT   (I1).
063300
049845     IF (CRT-SCREEN-CODE = "BN711" OR "BN721")  
049845         CONTINUE
049845     ELSE
049845         MOVE BNCTWS-SMOKER-SW       TO BNPTB-SMOKER-FLAG  (I1)
049845     END-IF.
063500
063600     MOVE BNCTWS-PRE-START-DATE      TO BNPTB-PREM-UPD-DT  (I1).
063700     MOVE BNCTWS-PRE-GROUP-NAME      TO BNPTB-PREM-GROUP   (I1).
063800
063900 2450-END.
064000
064100******************************************************************
064200 2452-SET-ERROR-CURSOR.
064300******************************************************************
064400
064500     IF (BNCTWS-FIELD-NBR                 = 1)
064600         MOVE BNPTB-COVER-OPT-FN    (I1) TO CRT-FIELD-NBR
064700     ELSE
064800     IF (BNCTWS-FIELD-NBR                 = 3)
064900         MOVE BNPTB-PAY-PER-AMT-FN  (I1) TO CRT-FIELD-NBR
065000     ELSE
065100     IF (BNCTWS-FIELD-NBR                 = 4)
065200         MOVE BNPTB-ANNUAL-AMT-FN   (I1) TO CRT-FIELD-NBR
065300     ELSE
065400     IF (BNCTWS-FIELD-NBR                 = 5)
065500         MOVE BNPTB-START-DATE-FN   (I1) TO CRT-FIELD-NBR
065600     ELSE
065700     IF (BNCTWS-FIELD-NBR                 = 6)
065800         MOVE BNPTB-STOP-DATE-FN    (I1) TO CRT-FIELD-NBR
065900     ELSE
066000     IF (BNCTWS-FIELD-NBR                 = 11)
066100         MOVE BNPTB-SMOKER-FLAG-FN  (I1) TO CRT-FIELD-NBR
066200     ELSE
066300         MOVE BNPTB-LINE-FC-FN      (I1) TO CRT-FIELD-NBR.
066400
066500 2452-END.
066600
066700******************************************************************
066800 2460-EDIT-ANN-LIMITS.
066900******************************************************************
067000
067100     INITIALIZE BNPTB-C5-ANNUAL-AMT.
067200
067300     MOVE BNPTB-COMPANY              TO DB-COMPANY.
067400     MOVE BNPTB-EMPLOYEE             TO DB-EMPLOYEE
                                              BNPTB-SAVE-EMPLOYEE.
067500     MOVE BNPTB-PARTICIPNT           TO DB-PARTICIPNT
                                              BNPTB-SAVE-PARTICIPNT.
067600     MOVE BNPTB-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
067700     MOVE BNPTB-PLAN-CODE (I1)       TO DB-PLAN-CODE
                                              BNPTB-SAVE-PLAN-CODE.
067800     MOVE BNPTB-EFD-START-DATE       TO DB-START-DATE.
067900     PERFORM 850-FIND-NLT-PTBSET5.
068000     PERFORM 2462-ADD-PTB-ANNUAL-AMT
068100     THRU    2462-END
068200         UNTIL (PARTBEN-NOTFOUND)
068300         OR    (PTB-COMPANY          NOT = DB-COMPANY)
068400         OR    (PTB-EMPLOYEE         NOT = DB-EMPLOYEE)
068500         OR    (PTB-PARTICIPNT       NOT = DB-PARTICIPNT)
068600         OR    (PTB-PLAN-TYPE        NOT = DB-PLAN-TYPE)
068700         OR    (PTB-PLAN-CODE        NOT = DB-PLAN-CODE)
068800         OR    (PTB-START-DATE       > BNPTB-EFD-STOP-DATE).
068900
069000     PERFORM 2464-ADD-FROM-DETAIL-LINE
069100     THRU    2464-END
069200         VARYING BNPTB-I1 FROM 1 BY 1
069300         UNTIL  (BNPTB-I1 > I1).
069400
069500     MOVE BNPTB-COMPANY              TO DB-COMPANY.
069600     MOVE BNPTB-EMPLOYEE             TO DB-EMPLOYEE.
069700     MOVE BNPTB-PARTICIPNT           TO DB-PARTICIPNT.
069800     MOVE BNPTB-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
069900     MOVE BNPTB-PLAN-CODE (I1)       TO DB-PLAN-CODE.
070000     MOVE BNPTB-START-DATE (I1)      TO DB-START-DATE.
070100     PERFORM 840-FIND-PTBSET1.
070200
070300     IF  (PRE-ANN-AMT-MAX            NOT = ZEROES)
070400     AND (BNPTB-C5-ANNUAL-AMT        > BNCTWS-ANN-AMT-MAX)
070500******** Total of benefits for plan year exceeds plan maximum
070600         MOVE 184                    TO CRT-ERROR-NBR
070700         MOVE BNPTB-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
070800         GO TO 2460-END.
070900
071000     IF  (PRE-ANN-AMT-MIN            NOT = ZEROES)
071100     AND (BNPTB-C5-ANNUAL-AMT        < BNCTWS-ANN-AMT-MIN)
071200******** Contribution amount less than annual minimum amount
071300         MOVE 196                    TO BNCTWS-ERROR-NBR
071400         MOVE BNPTB-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
071500         GO TO 2460-END.
071600
071700 2460-END.
071800
071900******************************************************************
072000 2462-ADD-PTB-ANNUAL-AMT.
072100******************************************************************
072200
072300     IF (PTB-START-DATE              < BNPTB-EFD-START-DATE)
072400     OR (PTB-START-DATE              > BNPTB-EFD-STOP-DATE)
072500         GO TO 2462-GET-NEXT-PARTBEN.
072600
072700     ADD PTB-ANNUAL-AMT              TO BNPTB-C5-ANNUAL-AMT.
072800
072900 2462-GET-NEXT-PARTBEN.
073000     PERFORM 860-FIND-NEXT-PTBSET5.
073100
073200 2462-END.
073300
073400******************************************************************
073500 2464-ADD-FROM-DETAIL-LINE.
073600******************************************************************
073700
073800     IF (BNPTB-LINE-FC (BNPTB-I1)         NOT = "A" AND "C")
           OR (BNPTB-EMPLOYEE           NOT = BNPTB-SAVE-EMPLOYEE)
           OR (BNPTB-PARTICIPNT         NOT = BNPTB-SAVE-PARTICIPNT)
073900         GO TO 2464-END.
074000
074100     IF (BNPTB-PLAN-TYPE (BNPTB-I1)       NOT = "RS")
074200         GO TO 2464-END.
074300
074400     MOVE BNPTB-COMPANY                   TO DB-COMPANY.
074500     MOVE BNPTB-EMPLOYEE                  TO DB-EMPLOYEE.
074600     MOVE BNPTB-PARTICIPNT                TO DB-PARTICIPNT.
           IF (DB-PARTICIPNT                    NOT = ZEROES)
               INITIALIZE DB-EMPLOYEE.
074700     MOVE BNPTB-PLAN-TYPE (BNPTB-I1)      TO DB-PLAN-TYPE.
074800     MOVE BNPTB-PLAN-CODE (BNPTB-I1)      TO DB-PLAN-CODE.
074900     MOVE BNPTB-START-DATE (BNPTB-I1)     TO DB-START-DATE.
075000     PERFORM 840-FIND-PLNSET1.
075100
075200     IF  (PLN-CONTRIB-TYPE                = "5")
075300     AND (PLN-FLEX-PLAN                   NOT = SPACES)
           AND (PLN-PLAN-CODE                   = BNPTB-SAVE-PLAN-CODE)
075400         CONTINUE
075500     ELSE
075600         GO TO 2464-END.
075700
075800     IF (BNPTB-LINE-FC (BNPTB-I1)         = "A")
075900         ADD BNPTB-ANNUAL-AMT (BNPTB-I1)  TO BNPTB-C5-ANNUAL-AMT.
076000
076100     IF (BNPTB-LINE-FC (BNPTB-I1)         = "C")
076200         PERFORM 840-FIND-PTBSET1
076300         SUBTRACT PTB-ANNUAL-AMT          FROM BNPTB-C5-ANNUAL-AMT
076400         ADD  BNPTB-ANNUAL-AMT (BNPTB-I1) TO BNPTB-C5-ANNUAL-AMT.
076500
076600 2464-END.
076700
076800******************************************************************
076900 2600-EDIT-DTL-DELETE.
077000******************************************************************
077100
077200     PERFORM 2610-EDIT-BNINVDETL
077300     THRU    2610-END.
077400     IF (ERROR-FOUND)
077500         GO TO 2600-END.
077600
077700     IF (BNPTB-PLAN-TYPE (I1)        = "RS")
077800         PERFORM 2620-EDIT-RESTRANS
077900         THRU    2620-END
078000         IF (ERROR-FOUND)
078100             GO TO 2600-END.
078200
078300     IF (BNPTB-PLAN-TYPE (I1)        = "HL" OR "DN" OR "DL")
078400     OR (BNPTB-PLAN-TYPE (I1)        = "EL")
078500         PERFORM 2700-EDIT-DEPENDENTS
078600         THRU    2700-END
078700         IF (ERROR-FOUND)
078800             GO TO 2600-END.
078900
           PERFORM 2850-EDIT-HIPAA
           THRU    2850-END.

079000 2600-END.
079100
079200******************************************************************
079300 2610-EDIT-BNINVDETL.
079400******************************************************************
079500
079600     MOVE BNPTB-COMPANY              TO DB-COMPANY.
079700     MOVE BNPTB-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
079800     MOVE BNPTB-PLAN-CODE (I1)       TO DB-PLAN-CODE.
079900     MOVE BNPTB-START-DATE (I1)      TO DB-START-DATE.
080000     IF  (BNPTB-PARTICIPNT = ZEROES)
080100         MOVE BNPTB-EMPLOYEE         TO DB-EMPLOYEE
080200     ELSE
080300         MOVE ZEROES                 TO DB-EMPLOYEE.
080400     MOVE BNPTB-PARTICIPNT           TO DB-PARTICIPNT.
080500     INITIALIZE DB-INV-NUMBER.
080600     PERFORM 850-FIND-NLT-BIDSET1.
080700     IF  (BNINVDETL-FOUND)
080800     AND (BID-COMPANY                = DB-COMPANY)
080900     AND (BID-PLAN-TYPE              = DB-PLAN-TYPE)
081000     AND (BID-PLAN-CODE              = DB-PLAN-CODE)
081100     AND (BID-START-DATE             = DB-START-DATE)
081200     AND (BID-EMPLOYEE               = DB-EMPLOYEE)
081300     AND (BID-PARTICIPNT             = DB-PARTICIPNT)
081400******** Cannot delete invoice exists
081500         MOVE 115                    TO CRT-ERROR-NBR
081600         MOVE BNPTB-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
081700         GO TO 2610-END.
081800
081900 2610-END.
082000
082100******************************************************************
082200 2620-EDIT-RESTRANS.
082300******************************************************************
082400
082500     MOVE BNPTB-COMPANY              TO DB-COMPANY.
082600     MOVE BNPTB-EMPLOYEE             TO DB-EMPLOYEE.
082700     MOVE BNPTB-PLAN-CODE  (I1)      TO DB-PLAN-CODE.
082800     MOVE BNPTB-START-DATE (I1)      TO DB-START-DATE.
082900     INITIALIZE DB-DATE
083000                DB-CHECK-NBR.
083100     PERFORM 850-FIND-NLT-RTRSET2.
083200     IF  (RESTRANS-FOUND)
083300     AND (RTR-COMPANY                = DB-COMPANY)
083400     AND (RTR-EMPLOYEE               = DB-EMPLOYEE)
083500     AND (RTR-PLAN-CODE              = DB-PLAN-CODE)
083600     AND (RTR-START-DATE             = DB-START-DATE)
083700******** Cannot delete; spending account transactions exist
083800         MOVE 139                    TO CRT-ERROR-NBR
083900         MOVE BNPTB-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
084000         GO TO 2620-END.
084100
084200 2620-END.
084300
084400******************************************************************
084500 2700-EDIT-DEPENDENTS.
084600******************************************************************
084700
084800     IF  (BNPTB-LINE-FC (I1)         = "C")
084900     AND (BNPTB-COVER-OPT (I1)       NOT = PTB-COV-OPTION)
085000         MOVE BNPTB-COVER-OPT (I1)   TO DB-COVERAGE-OPT
085100         PERFORM 840-FIND-COPSET1.
085200
005000     IF  (BNPTB-PART-ENT)
           AND (PAR-DEPENDENT              NOT = ZEROES)
                PERFORM 2710-EDIT-PAR-DEP
                THRU    2710-END
                GO TO 2700-END.

085300     MOVE BNPTB-COMPANY              TO DB-COMPANY.
085400     MOVE BNPTB-EMPLOYEE             TO DB-EMPLOYEE.
085500     MOVE BNPTB-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
085600     MOVE BNPTB-PLAN-CODE  (I1)      TO DB-PLAN-CODE.
085700     MOVE BNPTB-START-DATE (I1)      TO DB-EMP-START.
085800     INITIALIZE DB-DEPENDENT
085900                DB-START-DATE.
086000     PERFORM 850-FIND-NLT-HDBSET3.
086100     PERFORM 860-FIND-NEXT-HDBSET3
086200         UNTIL (HRDEPBEN-NOTFOUND)
086300         OR    (HDB-COMPANY          NOT = DB-COMPANY)
086400         OR    (HDB-EMPLOYEE         NOT = DB-EMPLOYEE)
086500         OR    (HDB-PLAN-TYPE        NOT = DB-PLAN-TYPE)
086600         OR    (HDB-PLAN-CODE        NOT = DB-PLAN-CODE)
086700         OR    (HDB-EMP-START        NOT = DB-EMP-START)
J10377         OR    ((HDB-PARTICIPNT          = BNPTB-PARTICIPNT)
086800         AND   ((HDB-START-DATE      NOT < BNPTB-START-DATE (I1))
086900         AND  (((HDB-START-DATE      NOT > BNPTB-STOP-DATE (I1)) 
087000         AND    (BNPTB-STOP-DATE (I1) NOT = ZEROES)) 
087100         OR     (BNPTB-STOP-DATE (I1) = ZEROES)))).
087200
087300     IF (BNPTB-LINE-FC (I1)          = "C")
087400         IF  ((HRDEPBEN-FOUND)
087500         AND  (HDB-COMPANY           = DB-COMPANY)
087600         AND  (HDB-EMPLOYEE          = DB-EMPLOYEE)
J10377         AND  (HDB-PARTICIPNT        = 0)
087700         AND  (HDB-PLAN-TYPE         = DB-PLAN-TYPE)
087800         AND  (HDB-PLAN-CODE         = DB-PLAN-CODE)
087900         AND  (HDB-EMP-START         = DB-EMP-START)
J10377         AND  (HDB-PARTICIPNT        = BNPTB-PARTICIPNT)
088000         AND  (HDB-START-DATE        >= BNPTB-START-DATE (I1))
088100         AND (((HDB-START-DATE       <= BNPTB-STOP-DATE  (I1))
088200         AND   (BNPTB-STOP-DATE (I1) NOT = ZEROES))
088300         OR    (BNPTB-STOP-DATE (I1) = ZEROES))
J07540* NOT (S)POUSE, (D)EPENDENTS, (B)OTH SPOUSE AND DEPS, (P)ARTNER, 
J07540*     SPOUSE (O)R PARTNER, PA(R)TNER DEPS, (C) PARTNER AND DEPS, 
J07540*     SPOUSE OR PARTNER (A)ND DEPS.
J07540         AND (COP-COV-DEPENDENTS     NOT = "S" AND "D" AND "B" AND
J07540              "P" AND "O" AND "R" AND "C" AND "A"))
088500************ Cannot select single coverage; dependents exist
088600             MOVE 202                      TO CRT-ERROR-NBR
088700             MOVE BNPTB-COVER-OPT-FN (I1)  TO CRT-FIELD-NBR
088800             GO TO 2700-END.
088900
089000     IF (BNPTB-LINE-FC (I1)          = "D")
089100         IF   (HRDEPBEN-FOUND)
089200         AND  (HDB-COMPANY           = DB-COMPANY)
089300         AND  (HDB-EMPLOYEE          = DB-EMPLOYEE)
J14132*J10377         AND  (HDB-PARTICIPNT        = 0)
089400         AND  (HDB-PLAN-TYPE         = DB-PLAN-TYPE)
089500         AND  (HDB-PLAN-CODE         = DB-PLAN-CODE)
089600         AND  (HDB-EMP-START         = DB-EMP-START)
J10377         AND  (HDB-PARTICIPNT        = BNPTB-PARTICIPNT)
089700         AND  (HDB-START-DATE        >= BNPTB-START-DATE (I1))
089800         AND (((HDB-START-DATE       <= BNPTB-STOP-DATE  (I1))
089900         AND   (BNPTB-STOP-DATE (I1) NOT = ZEROES))
090000         OR    (BNPTB-STOP-DATE (I1) = ZEROES))
090100************ Cannot delete dependent benefits exist
090200             MOVE 201                      TO CRT-ERROR-NBR
090300             MOVE BNPTB-LINE-FC-FN (I1)    TO CRT-FIELD-NBR
090400             GO TO 2700-END.
090500
090600 2700-END.
090700
090800******************************************************************
       2710-EDIT-PAR-DEP.
090800******************************************************************

085300     MOVE BNPTB-COMPANY              TO DB-COMPANY.
085400     MOVE BNPTB-EMPLOYEE             TO DB-EMPLOYEE.
085500     MOVE BNPTB-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
085600     MOVE BNPTB-PLAN-CODE  (I1)      TO DB-PLAN-CODE.
085700     MOVE BNPTB-START-DATE (I1)      TO DB-EMP-START.
085800     MOVE PAR-DEPENDENT              TO DB-DEPENDENT.
085900     INITIALIZE DB-START-DATE.
086000     PERFORM 850-FIND-NLT-HDBSET3.
086100     PERFORM 860-FIND-NEXT-HDBSET3
086200         UNTIL (HRDEPBEN-NOTFOUND)
086300         OR    (HDB-COMPANY          NOT = DB-COMPANY)
086400         OR    (HDB-EMPLOYEE         NOT = DB-EMPLOYEE)
086500         OR    (HDB-PLAN-TYPE        NOT = DB-PLAN-TYPE)
086600         OR    (HDB-PLAN-CODE        NOT = DB-PLAN-CODE)
086700         OR    (HDB-EMP-START        NOT = DB-EMP-START)
086700         OR    (HDB-DEPENDENT        NOT = DB-DEPENDENT)
086800         OR    ((HDB-START-DATE      NOT < BNPTB-START-DATE (I1))
086900         AND  (((HDB-START-DATE      NOT > BNPTB-STOP-DATE (I1)) 
087000         AND    (BNPTB-STOP-DATE (I1) NOT = ZEROES)) 
087100         OR     (BNPTB-STOP-DATE (I1) = ZEROES))).
087200
087300     IF (BNPTB-LINE-FC (I1)          = "C")
087400         IF  ((HRDEPBEN-FOUND)
087500         AND  (HDB-COMPANY           = DB-COMPANY)
087600         AND  (HDB-EMPLOYEE          = DB-EMPLOYEE)
087700         AND  (HDB-PLAN-TYPE         = DB-PLAN-TYPE)
087800         AND  (HDB-PLAN-CODE         = DB-PLAN-CODE)
087900         AND  (HDB-EMP-START         = DB-EMP-START)
086700*         AND  (HDB-DEPENDENT         = DB-DEPENDENT)
J10377         AND  (HDB-PARTICIPNT        = BNPTB-PARTICIPNT)
088000         AND  (HDB-START-DATE        >= BNPTB-START-DATE (I1))
088100         AND (((HDB-START-DATE       <= BNPTB-STOP-DATE  (I1))
088200         AND   (BNPTB-STOP-DATE (I1) NOT = ZEROES))
088300         OR    (BNPTB-STOP-DATE (I1) = ZEROES))
J07540* NOT (S)POUSE, (D)EPENDENTS, (B)OTH SPOUSE AND DEPS, (P)ARTNER,
J07540*     SPOUSE (O)R PARTNER, PA(R)TNER DEPS, (C) PARTNER AND DEPS,
J07540*     SPOUSE OR PARTNER (A)ND DEPS.
J07540         AND (COP-COV-DEPENDENTS     NOT = "S" AND "D" AND "B" AND
J07540              "P" AND "O" AND "R" AND "C" AND "A"))
088500************ Cannot select single coverage; dependents exist
088600             MOVE 202                      TO CRT-ERROR-NBR
088700             MOVE BNPTB-COVER-OPT-FN (I1)  TO CRT-FIELD-NBR
P95530*            GO TO 2700-END.
P95530             GO TO 2710-END.
088900
089000     IF (BNPTB-LINE-FC (I1)          = "D")
089100         IF   (HRDEPBEN-FOUND)
089200         AND  (HDB-COMPANY           = DB-COMPANY)
089300         AND  (HDB-EMPLOYEE          = DB-EMPLOYEE)
089400         AND  (HDB-PLAN-TYPE         = DB-PLAN-TYPE)
089500         AND  (HDB-PLAN-CODE         = DB-PLAN-CODE)
089600         AND  (HDB-EMP-START         = DB-EMP-START)
J10377         AND  (HDB-PARTICIPNT        = BNPTB-PARTICIPNT)
086700*         AND  (HDB-DEPENDENT         = DB-DEPENDENT)
089700         AND  (HDB-START-DATE        >= BNPTB-START-DATE (I1))
089800         AND (((HDB-START-DATE       <= BNPTB-STOP-DATE  (I1))
089900         AND   (BNPTB-STOP-DATE (I1) NOT = ZEROES))
090000         OR    (BNPTB-STOP-DATE (I1) = ZEROES))
090100************ Cannot delete dependent benefits exist
090200             MOVE 201                      TO CRT-ERROR-NBR
090300             MOVE BNPTB-LINE-FC-FN (I1)    TO CRT-FIELD-NBR
P95530*            GO TO 2700-END.
P95530             GO TO 2710-END.
090500
       2710-END.

090800******************************************************************
090900 2800-EDIT-DTL-STOP.
091000******************************************************************
091100
091200     IF (BNPTB-STOP-DATE (I1)        = PTB-STOP-DATE)
091300******** Must change stop date with stop function code
091400         MOVE 170                        TO CRT-ERROR-NBR
091500         MOVE BNPTB-STOP-DATE-FN (I1)    TO CRT-FIELD-NBR
091600         GO TO 2800-END.
091700
091800     IF  (BNPTB-START-DATE (I1)      > BNPTB-STOP-DATE (I1))
091900     AND (BNPTB-STOP-DATE (I1)       NOT = ZEROES)
092000******** Stop date cannot be less than start date
092100         MOVE 171                        TO CRT-ERROR-NBR
092200         MOVE BNPTB-STOP-DATE-FN (I1)    TO CRT-FIELD-NBR
092300         GO TO 2800-END.
092400
092500     IF (BNPTB-PART-ENT)
092600         IF (PLN-FLEX-PLAN            = SPACES)
092700             IF (BNPTB-STOP-DATE (I1) > PAR-TERM-DATE)
092800**************** Stop date must not exceed extended coverage term date
092900                 MOVE 119                     TO CRT-ERROR-NBR
093000                 MOVE BNPTB-STOP-DATE-FN (I1) TO CRT-FIELD-NBR
093100                 GO TO 2800-END
093200             ELSE
093300             IF (BNPTB-STOP-DATE (I1) = ZEROES)
093400                 MOVE PAR-TERM-DATE       TO BNPTB-STOP-DATE (I1)
093900             END-IF
094000             END-IF
094100         ELSE
094200         IF (BNPTB-STOP-DATE (I1)        = ZEROES)
094300             IF (BNPTB-EFD-STOP-DATE     < PAR-TERM-DATE)
094400                 MOVE BNPTB-EFD-STOP-DATE  TO BNPTB-STOP-DATE (I1)
094500             ELSE
094600                 MOVE PAR-TERM-DATE       TO BNPTB-STOP-DATE (I1)
095100             END-IF
095200         ELSE
095300         IF (BNPTB-STOP-DATE (I1)        > PAR-TERM-DATE)
095400************ Stop date must not exceed extended coverage term date
095500             MOVE 119                     TO CRT-ERROR-NBR
095600             MOVE BNPTB-STOP-DATE-FN (I1) TO CRT-FIELD-NBR
095700             GO TO 2800-END
095800         ELSE
095900         IF (BNPTB-STOP-DATE (I1)        > BNPTB-EFD-STOP-DATE)
096000************ Stop date cannot be > flex year stop date
096100             MOVE 155                        TO CRT-ERROR-NBR
096200             MOVE BNPTB-STOP-DATE-FN (I1)    TO CRT-FIELD-NBR
096300             GO TO 2800-END.
096400
096500     IF (BNPTB-START-DATE (I1)       NOT = PTB-START-DATE)
096600     OR (BNPTB-COVER-OPT (I1)        NOT = PTB-COV-OPTION)
096700     OR (BNPTB-COVER-AMT (I1)        NOT = PTB-COVER-AMT)
096800     OR (BNPTB-PAY-PER-AMT (I1)      NOT = PTB-PAY-PER-AMT)
096900     OR (BNPTB-ANNUAL-AMT (I1)       NOT = PTB-ANNUAL-AMT)
097000     OR (BNPTB-SMOKER-FLAG (I1)      NOT = PTB-SMOKER)
J55910     OR (BNPTB-PROC-LEVEL (I1)       NOT = PTB-PROCESS-LEVEL)
097100******** Cannot change with stop line function code
097200         MOVE 161                    TO CRT-ERROR-NBR
097300         MOVE BNPTB-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
097400         GO TO 2800-END.
097500
097600     IF  (BNPTB-STOP-DATE (I1)       > PTB-STOP-DATE) 
097700     OR ((BNPTB-STOP-DATE (I1)       NOT = PTB-STOP-DATE)
097800     AND (BNPTB-STOP-DATE (I1)       = ZEROES))
097900         PERFORM 850-FIND-NLT-PTBSET5
098000         IF  (PARTBEN-FOUND)
098100         AND (PTB-COMPANY            = DB-COMPANY)
098200         AND (PTB-PARTICIPNT         = DB-PARTICIPNT)
098300         AND (PTB-EMPLOYEE           = DB-EMPLOYEE)
098400         AND (PTB-PLAN-TYPE          = DB-PLAN-TYPE)
098500         AND (PTB-PLAN-CODE          = DB-PLAN-CODE)
098600             MOVE WS-FALSE           TO BNPTB-STOP-DFLT-SW
098700             PERFORM 2420-EDIT-EXISTING-POST-PTB
098800             THRU    2420-END
098900                 UNTIL  (ERROR-FOUND)
099000                 OR     (BNPTB-STOP-DEFAULTED)
099100                 OR     (PARTBEN-NOTFOUND)
099200                 OR     (PTB-COMPANY          NOT = DB-COMPANY)
099300                 OR     (PTB-PARTICIPNT       NOT = DB-PARTICIPNT)
099400                 OR     (PTB-EMPLOYEE         NOT = DB-EMPLOYEE)
099500                 OR     (PTB-PLAN-TYPE        NOT = DB-PLAN-TYPE)
099600                 OR     (PTB-PLAN-CODE        NOT = DB-PLAN-CODE)
099700                 OR    ((BNPTB-STOP-DATE (I1) NOT = ZEROES)
099800                 AND    (BNPTB-STOP-DATE (I1) < PTB-START-DATE))
099900             IF (ERROR-FOUND)
100000                 GO TO 2800-END.
100100*
100200**** NOTE : THE PROGRAM MAY NO LONGER BE ON THE PARTBEN RECORD FROM
100300**** THE SCREEN AT THIS POINT, BECAUSE 2440 AND 2450 MAY HAVE YOU
100400**** POINTING AT ANOTHER PARTBEN RECORD.
100500*
100600     IF (DB-START-DATE               NOT = BNPTB-START-DATE (I1))
100700         MOVE BNPTB-START-DATE (I1)  TO DB-START-DATE.
100800     PERFORM 840-FIND-PTBSET1.
100900
101000     MOVE WS-TRUE                TO BNPTB-CHANGE-PREMIUM-SW (I1).
101100
           PERFORM 2850-EDIT-HIPAA
           THRU    2850-END.

101200 2800-END.
101300
067500******************************************************************
       2850-EDIT-HIPAA.
067500******************************************************************

           IF  (BNPTB-CREATE-TRANS (I1)    = SPACES)
           OR  ((BNPTB-CREATE-TRANS (I1)   = "Y")
           AND  (BNPTB-HIPAA-DEFAULT))
               MOVE PLN-CREATE-TRANS       TO BNPTB-CREATE-TRANS (I1).

           IF  (BNPTB-CREATE-TRANS (I1)    = "Y")
           AND (BNPTB-HIPAA-DEFAULT)
               MOVE BNPTB-HIPAA-REASON     TO BNPTB-REASON (I1).

           IF  (PLN-CREATE-TRANS           = "N")
           AND (BNPTB-CREATE-TRANS (I1)    = "Y")
      ******** Plan not defined for HIPAA transactions
               MOVE 230                                 TO CRT-ERROR-NBR
               MOVE BNPTB-CREATE-TRANS-FN (I1)          TO CRT-FIELD-NBR
               GO TO 2850-END.

           IF (BNPTB-CREATE-TRANS (I1)     = "N")
               IF (BNPTB-REASON (I1)       NOT = SPACES)
      ************ Do not enter reason if create transaction = "N"
                   MOVE 224                             TO CRT-ERROR-NBR
                   MOVE BNPTB-REASON-FN (I1)            TO CRT-FIELD-NBR
                   GO TO 2850-END
               END-IF
               IF (BNPTB-MEMBER-ID (I1)    NOT = ZEROES)
      ************ Do not specify member id if create transaction = "N"
                   MOVE 225                             TO CRT-ERROR-NBR
                   MOVE BNPTB-MEMBER-ID-FN (I1)         TO CRT-FIELD-NBR
                   GO TO 2850-END.

           IF (BNPTB-CREATE-TRANS (I1)     = "Y")
               IF (BNPTB-REASON (I1)       = SPACES)
      ************ Must enter reason if create transaction = "Y"
                   MOVE 226                             TO CRT-ERROR-NBR
                   MOVE BNPTB-REASON-FN (I1)            TO CRT-FIELD-NBR
                   GO TO 2850-END
               END-IF
               IF (BNPTB-MEMBER-ID (I1)    = ZEROES)
                   MOVE PLN-MEMBER-ID      TO BNPTB-MEMBER-ID (I1).

           IF (BNPTB-REASON (I1)           NOT = SPACES)
               MOVE BNPTB-BT               TO DB-TYPE
               MOVE BNPTB-REASON (I1)      TO DB-CODE
               PERFORM 840-FIND-PCOSET1
               IF (PCODES-NOTFOUND)
      ************ Reason code does not exist
                   MOVE 228                             TO CRT-ERROR-NBR
                   MOVE BNPTB-REASON-FN (I1)            TO CRT-FIELD-NBR
                   GO TO 2850-END.

           IF  (BNPTB-MEMBER-ID (I1)       NOT = ZEROES)
           AND (BNPTB-MEMBER-ID (I1)       NOT = 1 AND 2)
      ******** Invalid member id
               MOVE 229                                 TO CRT-ERROR-NBR
               MOVE BNPTB-MEMBER-ID-FN (I1)             TO CRT-FIELD-NBR
               GO TO 2850-END.

       2850-END.

101400******************************************************************
101500 2000-END.
101600******************************************************************
101700
101800******************************************************************
101900 4000-BNPTB-PROCESS-TRAN         SECTION.
102000******************************************************************
102100 4000-START.
102200 
           MOVE CRT-PROGRAM-CODE           TO BNPTB-HIPAA-DEFAULT-SW.

102300     PERFORM 4200-CHANGE
102400     THRU    4200-END.
102500
102600     GO TO 4000-END. 
102700
102800******************************************************************
102900 4200-CHANGE.
103000******************************************************************
103100
GW         MOVE PTB-STOP-DATE             TO WS-BNPTB-STOP-DATE.
103200     PERFORM 4220-PROCESS-DETAIL
103300     THRU    4220-END
103400         VARYING I1 FROM 1 BY 1
103500         UNTIL  (I1 > BNPTB-NBR-LINES).
103600 
103700 4200-END.
103800
103900******************************************************************
104000 4220-PROCESS-DETAIL.
104100******************************************************************
104200
104300     IF (BNPTB-LINE-FC (I1)          = SPACES)
104400         GO TO 4220-END.
104500
104600     MOVE BNPTB-COMPANY              TO DB-COMPANY.
104700     MOVE BNPTB-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
104800     MOVE BNPTB-PLAN-CODE (I1)       TO DB-PLAN-CODE.
104900     PERFORM 840-FIND-PLNSET1.
105000
105100     MOVE BNPTB-COMPANY              TO DB-COMPANY.
105200     MOVE BNPTB-PARTICIPNT           TO DB-PARTICIPNT.
105300     IF  (BNPTB-PART-ENT)
105400          MOVE ZEROES                TO DB-EMPLOYEE
105500     ELSE
105600          MOVE BNPTB-EMPLOYEE        TO DB-EMPLOYEE.
105700     MOVE BNPTB-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
105800     MOVE BNPTB-PLAN-CODE (I1)       TO DB-PLAN-CODE.
105900     MOVE BNPTB-START-DATE (I1)      TO DB-START-DATE.
106000
106100     IF (BNPTB-LINE-FC (I1)          = "A")
106200         PERFORM 4230-ADD-PTB
106300         THRU    4230-END
106400     ELSE
106500     IF (BNPTB-LINE-FC (I1)          = "C")
106600         PERFORM 4240-CHANGE-PTB
106700         THRU    4240-END
106800     ELSE
106900     IF (BNPTB-LINE-FC (I1)          = "D")
107000         PERFORM 4250-DELETE-PTB
107100         THRU    4250-END
107200     ELSE
107300     IF (BNPTB-LINE-FC (I1)          = "S")
107400         PERFORM 4260-STOP-PTB
107500         THRU    4260-END.
107600
           IF  (BNPTB-CREATE-TRANS (I1)    = SPACES)
           OR  ((BNPTB-CREATE-TRANS (I1)   = "Y")
           AND  (BNPTB-HIPAA-DEFAULT))
               PERFORM 5300-DEFAULT-HIPAA-FIELDS
               THRU    5300-END.

           IF (BNPTB-CREATE-TRANS (I1)     = "Y")
               PERFORM 5200-CREATE-BNTRANS
               THRU    5200-END.

110100     INITIALIZE BNPTB-LINE-FC (I1).
110200
106900     IF (BNPTB-LINE-FC (I1)          = "D")
114500         INITIALIZE BNPTB-PARTBEN-DETAIL (I1).
114600
107700 4220-END.
107800
107900******************************************************************
108000 4230-ADD-PTB.
108100******************************************************************
108200
108300     PERFORM 850-MODIFY-NLT-PTBSET4.
108400     IF  (PARTBEN-FOUND)
108500     AND (PTB-COMPANY                = DB-COMPANY)
108600     AND (PTB-PARTICIPNT             = DB-PARTICIPNT)
108700     AND (PTB-EMPLOYEE               = DB-EMPLOYEE)
108800     AND (PTB-PLAN-TYPE              = DB-PLAN-TYPE)
108900     AND (PTB-PLAN-CODE              = DB-PLAN-CODE)
109000     AND (PTB-STOP-DATE              = ZEROES)
109100         PERFORM 4232-STOP-EXISTING-PARTBEN
109200         THRU    4232-END.
109300
109400     PERFORM 800-CREATE-PARTBEN.
109500
109600     PERFORM 5100-MOVE-DETAIL-DATA
109700     THRU    5100-END.
AI0095******************************************************************
AI0095****   ANALYSTS INTERNATIONAL   RDAHL    3/24/03             *****
AI0095****   ENHANCEMENT FOR MOD 9.5 RETROACTIVE BILLING           *****
AI0095******************************************************************
RAY        PERFORM 4400-RETRO-BILL-TRIGGER.
109800
109900     PERFORM 820-STORE-PARTBEN.
110000
110300 4230-END.
110400
110500******************************************************************
110600 4232-STOP-EXISTING-PARTBEN.
110700******************************************************************
110800
110900     MOVE BNPTB-START-DATE (I1)      TO WSDR-FR-DATE.
111000     PERFORM 900-DATE-TO-JULIAN.
111100     SUBTRACT 1                      FROM WSDR-JULIAN-DAYS.
111200     PERFORM 900-JULIAN-TO-DATE.
111300     MOVE WSDR-FR-DATE               TO PTB-STOP-DATE.
AI0095******************************************************************
AI0095****   ANALYSTS INTERNATIONAL   RDAHL    3/24/03             *****
AI0095****   ENHANCEMENT FOR MOD 9.5 RETROACTIVE BILLING           *****
AI0095******************************************************************
RAY
RAY        MOVE "S"                        TO BNPTB-LINE-FC (I1).
RAY        PERFORM 4400-RETRO-BILL-TRIGGER.
RAY        MOVE "A"                        TO BNPTB-LINE-FC (I1).

J67329     IF (BNPTB-LINE-FC (I1) = "A")
J67329         MOVE WS-SYSTEM-DATE-YMD     TO PTB-UPD-DATE
J67329         MOVE HHMMSS                 TO PTB-TIME-STAMP
J67329         MOVE CRT-USER-NAME          TO PTB-USER-ID
J67329     END-IF.
111400
111500     PERFORM 820-STORE-PARTBEN.
111600
111700 4232-END.
111800
111900******************************************************************
112000 4240-CHANGE-PTB.
112100******************************************************************
112200
112300     PERFORM 840-MODIFY-PTBSET1.
112400
120400     IF  (BNPTB-STOP-DATE (I1)       NOT = PTB-STOP-DATE)
120500     AND (BNPTB-PLAN-TYPE (I1)       NOT = "RS")
120600         MOVE PTB-STOP-DATE          TO BNPTB-DEP-STOP-DATE
120700         MOVE BNPTB-STOP-DATE (I1)   TO PTB-STOP-DATE
120800         PERFORM 4262-STOP-HRDEPBEN
120900         THRU    4262-END.
121000
112500     PERFORM 5100-MOVE-DETAIL-DATA
112600     THRU    5100-END.
112700
112800     PERFORM 820-STORE-PARTBEN.
RAY
AI0095******************************************************************
AI0095****   ANALYSTS INTERNATIONAL   RDAHL    3/24/03             *****
AI0095****   ENHANCEMENT FOR MOD 9.5 RETROACTIVE BILLING           *****
AI0095******************************************************************
RAY   *--- THE RETRO ROUTINE WILL BE PERFORMED FOR BOTH THE STOP OLD
RAY   *--- AND THE ADD NEW. THE STOP OLD WILL CREATE A TRIGGER, THE
RAY   *--- ADD WILL NOT BECAUSE THE DATE WILL BE THE SAME

RAY        PERFORM 4400-RETRO-BILL-TRIGGER.

RAY   *----------------------------------------------------------------
112900
113200 4240-END.
113300
113400******************************************************************
113500 4250-DELETE-PTB.
113600******************************************************************
113700
113800     PERFORM 840-MODIFY-PTBSET1.
AI0095******************************************************************
AI0095****   ANALYSTS INTERNATIONAL   RDAHL    3/24/03             *****
AI0095****   ENHANCEMENT FOR MOD 9.5 RETROACTIVE BILLING           *****
AI0095******************************************************************
            
RAY        PERFORM 4400-RETRO-BILL-TRIGGER.
113900
114000     PERFORM 830-DELETE-PARTBEN.
114100
114200     PERFORM 4252-DELETE-BNCOMMENTS
114300     THRU    4252-END.
114400
114700 4250-END.
114800
114900******************************************************************
115000 4252-DELETE-BNCOMMENTS.
115100******************************************************************
115200
115300     MOVE BNPTB-COMPANY              TO DB-COMPANY.
115400     MOVE BNPTB-PARTICIPNT           TO DB-PARTICIPNT.
115500     IF  (BNPTB-PART-ENT)
115600          MOVE ZEROES                TO DB-EMPLOYEE
115700     ELSE
115800          MOVE BNPTB-EMPLOYEE        TO DB-EMPLOYEE.
115900     MOVE BNPTB-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
116000     INITIALIZE DB-PLAN-CODE
116100                DB-START-DATE.
116200     PERFORM 850-FIND-NLT-PTBSET5.
116300     IF (PARTBEN-NOTFOUND)
116400     OR (PTB-COMPANY                 NOT = DB-COMPANY)
116500     OR (PTB-PARTICIPNT              NOT = DB-PARTICIPNT)
116600     OR (PTB-EMPLOYEE                NOT = DB-EMPLOYEE)
116700     OR (PTB-PLAN-TYPE               NOT = DB-PLAN-TYPE)
116800         MOVE BNPTB-COMPANY          TO DB-COMPANY
116900         MOVE "EM"                   TO DB-CMT-TYPE
117000         MOVE BNPTB-EMPLOYEE         TO DB-EMPLOYEE
117100         INITIALIZE DB-PARTICIPNT
117200         MOVE BNPTB-PLAN-TYPE (I1)   TO DB-PLAN-TYPE
117300         INITIALIZE DB-PLAN-CODE
117400                    DB-GROUP-NAME
117500                    DB-START-DATE
117600                    DB-SEQ-NBR
117700         PERFORM 850-MODIFY-NLT-BCMSET1
117800         PERFORM 
117900             UNTIL (BNCOMMENTS-NOTFOUND)
118000             OR    (BCM-COMPANY      NOT = DB-COMPANY)
118100             OR    (BCM-CMT-TYPE     NOT = DB-CMT-TYPE)
118200             OR    (BCM-EMPLOYEE     NOT = DB-EMPLOYEE)
118300             OR    (BCM-PARTICIPNT   NOT = DB-PARTICIPNT)
118400             OR    (BCM-PLAN-TYPE    NOT = DB-PLAN-TYPE)
118500
118600             PERFORM 830-DELETE-BNCOMMENTS
118700             PERFORM 860-MODIFY-NEXT-BCMSET1
118800         END-PERFORM.
118900
119000 4252-END.
119100
119200******************************************************************
119300 4260-STOP-PTB.
119400******************************************************************
119500
119600     PERFORM 840-MODIFY-PTBSET1.
119700
120300     IF  (BNPTB-LINE-FC (I1)         = "S")
120400     AND (BNPTB-STOP-DATE (I1)       NOT = PTB-STOP-DATE)
120500     AND (BNPTB-PLAN-TYPE (I1)       NOT = "RS")
120600         MOVE PTB-STOP-DATE          TO BNPTB-DEP-STOP-DATE
120700         MOVE BNPTB-STOP-DATE (I1)   TO PTB-STOP-DATE
120800         PERFORM 4262-STOP-HRDEPBEN
120900         THRU    4262-END.
121000
119800     MOVE BNPTB-STOP-DATE (I1)       TO PTB-STOP-DATE.
J67329     IF (BNPTB-LINE-FC (I1) = "C" OR "S")
119900         MOVE WS-SYSTEM-DATE-YMD     TO PTB-UPD-DATE
P56935         MOVE HHMMSS                 TO PTB-TIME-STAMP
P56935         MOVE CRT-USER-NAME          TO PTB-USER-ID
J67329     END-IF.
110000
AI0095******************************************************************
AI0095****   ANALYSTS INTERNATIONAL   RDAHL    3/24/03             *****
AI0095****   ENHANCEMENT FOR MOD 9.5 RETROACTIVE BILLING           *****
AI0095******************************************************************
           
RAY        PERFORM 4400-RETRO-BILL-TRIGGER.
120000
120100     PERFORM 820-STORE-PARTBEN.
120200
********* GW 7/07 BELOW INITIALIZATION WAS IN PROD, NOT VNLA **********
121100     INITIALIZE BNPTB-LINE-FC (I1).
121300 4260-END.
121400
121500******************************************************************
121600 4262-STOP-HRDEPBEN.
121700******************************************************************
121800
121900     MOVE BNPTB-COMPANY              TO HRHDB-COMPANY.
122000     MOVE 1                          TO HRHDB-COMPANY-FN.
122100     IF (BNPTB-PART-ENT)
122200         MOVE PAR-PARTICIPNT         TO HRHDB-PARTICIPNT
122200         MOVE PAR-EMPLOYEE           TO HRHDB-EMPLOYEE
122300     ELSE
122400         MOVE BNPTB-EMPLOYEE         TO HRHDB-EMPLOYEE.
122500
122600     MOVE 1                          TO HRHDB-EMPLOYEE-FN.
122700     INITIALIZE HRHDB-DEPENDENT.
122800     MOVE 1                          TO HRHDB-DEPENDENT-FN.
122900     MOVE BNPTB-PLAN-TYPE (I1)       TO HRHDB-PLAN-TYPE.
123000     MOVE 1                          TO HRHDB-PLAN-TYPE-FN.
123100     MOVE BNPTB-PLAN-CODE (I1)       TO HRHDB-PLAN-CODE.
123200     MOVE 1                          TO HRHDB-PLAN-CODE-FN.
123300     MOVE BNPTB-START-DATE (I1)      TO HRHDB-BEN-START-DATE. 
123400     MOVE 1                          TO HRHDB-BEN-START-DATE-FN. 
123500     INITIALIZE HRHDB-START-DATE.
123600     MOVE 1                          TO HRHDB-START-DATE-FN.
123700     MOVE "S"                        TO HRHDB-FC.
123800     MOVE 1                          TO HRHDB-FC-FN.
123900     MOVE BNPTB-STOP-DATE (I1)       TO HRHDB-BEN-STOP-DATE.
124000     MOVE 1                          TO HRHDB-BEN-STOP-DATE-FN.
124100     IF (BNPTB-PART-ENT)
124200         MOVE "C"                    TO HRHDB-COVER-TYPE
124300     ELSE
124400         MOVE "R"                    TO HRHDB-COVER-TYPE.
           MOVE BNPTB-BATCH-FC (I1)        TO HRHDBWS-BATCH-FC.
124500
           MOVE BNPTB-CREATE-TRANS (I1)    TO HRHDB-CREATE-TRANS.
           MOVE BNPTB-REASON (I1)          TO HRHDB-REASON.
           MOVE BNPTB-MEMBER-ID (I1)       TO HRHDB-MEMBER-ID.

124600     PERFORM 3000-HRHDB-PROCESS-TRAN.
124700
           IF  (CRT-PROGRAM-CODE = "BN105" OR "BN102" OR "BN100")
           AND (HRHDB-SV-SP-END-DT (1)  NOT = ZEROES)
               INITIALIZE BNPTB-SV-SP-TBL (I1)
               MOVE HRHDB-SV-SP-TBL    TO BNPTB-SV-SP-TBL (I1)
               INITIALIZE HRHDB-SV-SP-TBL
               INITIALIZE I4.

124800 4262-END.
124900
RAY    4400-RETRO-BILL-TRIGGER.
AI0095******************************************************************
AI0095****   ANALYSTS INTERNATIONAL   RDAHL    3/24/03             *****
AI0095****   ENHANCEMENT FOR MOD 9.5 RETROACTIVE BILLING           *****
RAY   ****   TRIGGER REWRITE - HERWECK 5/5/04                      *****
AI0095******************************************************************
RAY        MOVE PTB-COMPANY                TO DB-COMPANY.
RAY        MOVE PTB-PLAN-TYPE              TO DB-PLAN-TYPE.
RAY        MOVE PTB-PLAN-CODE              TO DB-PLAN-CODE.
RAY        PERFORM 840-FIND-PLNSET1.

AI0095     IF (BNPTB-LINE-FC (I1) = "A" OR "C" OR "S")
RAY******2018  (PLN-CONTRIB-TYPE > ZEROES)
SDB******* AND (PLN-PLAN-CODE(1:1) NOT = "Y")
AI0095         MOVE PTB-COMPANY            TO DB-COMPANY
AI0095         MOVE PTB-PLAN-TYPE          TO DB-PLAN-TYPE
AI0095         MOVE PTB-PLAN-CODE          TO DB-PLAN-CODE
AI0095         MOVE PTB-EMPLOYEE           TO DB-EMPLOYEE
RAY            MOVE PTB-PARTICIPNT         TO DB-PARTICIPNT
GW             MOVE PTB-START-DATE         TO DB-START-DATE
RAY            PERFORM 840-FIND-WBPSET1
RAY            IF (WBPBENEFIT-NOTFOUND)
RAY                MOVE ZEROES             TO WS-BNPTB-START-DATE
RAY                MOVE ZEROES             TO WS-BNPTB-STOP-DATE
RAY            ELSE
RAY                MOVE WBP-RETRO-START-DT TO WS-BNPTB-START-DATE
RAY            END-IF
RAY            IF (PTB-PARTICIPNT = ZEROES)
RAY                PERFORM 4420-GET-LAST-BILLED-DATE
RAY            ELSE
RAY                PERFORM 4425-GET-LAST-BILLED-PTB
RAY            END-IF
AI0095         IF  (BNPTB-PROCESSED-DATE = ZERO)
AI0095         OR  (PTB-START-DATE < BNPTB-PROCESSED-DATE)
RAY                PERFORM 4460-WBP-RETRO-START-DATE
RAY                PERFORM 4490-WRITE-RETRO-BILL-TRIGGER
RAY                THRU    4490-END
RAY            END-IF
RAY        END-IF. 
RAY
RAY        IF (BNPTB-LINE-FC (I1) = "D")
RAY******2018  (PLN-CONTRIB-TYPE > ZEROES)
SDB******* AND (PLN-PLAN-CODE(1:1) NOT = "Y")
RAY            IF (PTB-PARTICIPNT = ZEROES)
RAY                PERFORM 4420-GET-LAST-BILLED-DATE
RAY            ELSE
RAY                PERFORM 4425-GET-LAST-BILLED-PTB
RAY            END-IF
RAY            MOVE PTB-COMPANY            TO DB-COMPANY
RAY            MOVE PTB-PLAN-TYPE          TO DB-PLAN-TYPE
RAY            MOVE PTB-PLAN-CODE          TO DB-PLAN-CODE
RAY            MOVE PTB-EMPLOYEE           TO DB-EMPLOYEE
RAY            MOVE PTB-PARTICIPNT         TO DB-PARTICIPNT
RAY            MOVE PTB-START-DATE         TO DB-START-DATE
RAY            PERFORM 840-FIND-WBPSET1
RAY            IF  (BNPTB-PROCESSED-DATE = ZERO)
RAY            OR  (PTB-START-DATE < BNPTB-PROCESSED-DATE)
RAY                MOVE PTB-START-DATE     TO WS-BNPTB-START-DATE
RAY                PERFORM 4490-WRITE-RETRO-BILL-TRIGGER
RAY                THRU    4490-END
RAY            END-IF
RAY        END-IF.
RAY
RAY   ******************************************************************
RAY    4420-GET-LAST-BILLED-DATE.
RAY   ******************************************************************
RAY
AI0095     MOVE ZERO                       TO BNPTB-PROCESSED-DATE.
AI0095     MOVE PTB-COMPANY                TO DB-COMPANY.
AI0095     MOVE PTB-EMPLOYEE               TO DB-EMPLOYEE.
RAY        MOVE PTB-PLAN-TYPE              TO DB-PLAN-TYPE.
RAY        MOVE PTB-PLAN-CODE              TO DB-PLAN-CODE.
AI0095     MOVE ZB3SETW1-PLAN-CODE          TO WS-DB-BEG-RNG.
AI0095     PERFORM 850-FIND-BEGRNG-ZB3SETW1.
AI0095     PERFORM
AI0095     UNTIL (PBHSTDTL-NOTFOUND)
AI0095         MOVE ZB3-COMPANY            TO DB-COMPANY
AI0095         MOVE ZB3-PROCESS-LEVEL      TO DB-PROCESS-LEVEL
AI0095         MOVE ZB3-INVOICE            TO DB-INVOICE
AI0095         PERFORM 840-FIND-ZB4SET1
AI0095         IF  (PBHSTHDR-FOUND)
AI0095         AND (ZB4-PER-END-DATE > BNPTB-PROCESSED-DATE)
AI0095             MOVE ZB4-PER-END-DATE   TO BNPTB-PROCESSED-DATE
AI0095         END-IF
AI0095         PERFORM 860-FIND-NXTRNG-ZB3SETW1
AI0095     END-PERFORM.
RAY
RAY   *    PERFORM 4430-GET-LAST-BILL-PERIOD.
RAY    
RAY   ******************************************************************
RAY    4425-GET-LAST-BILLED-PTB.
RAY   ******************************************************************
RAY
AI0095         MOVE ZERO                   TO BNPTB-PROCESSED-DATE.
RAY
RAY            MOVE PTB-COMPANY            TO DB-COMPANY.
RAY            MOVE PTB-PARTICIPNT         TO DB-CUSTOMER.
RAY            MOVE "D"                    TO DB-CUSTOMER(1:1).
RAY            INITIALIZE                     DB-PER-END-DATE.
RAY            INITIALIZE                     DB-INVOICE.
RAY            MOVE ZB4SETW1-CUSTOMER TO WS-DB-BEG-RNG.
RAY            PERFORM 850-FIND-BEGRNG-ZB4SETW1.
RAY            PERFORM
RAY            UNTIL (PBHSTHDR-NOTFOUND)
RAY            OR (BNPTB-PROCESSED-DATE > ZEROES)
RAY                MOVE ZB4-COMPANY        TO DB-COMPANY
RAY                MOVE ZB4-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
RAY                MOVE ZB4-INVOICE        TO DB-INVOICE
RAY                MOVE ZB3SET1-INVOICE    TO WS-DB-BEG-RNG
RAY                PERFORM 850-FIND-BEGRNG-ZB3SET1
RAY                PERFORM 
RAY                UNTIL (PBHSTDTL-NOTFOUND)
RAY                OR (BNPTB-PROCESSED-DATE > ZEROES)
RAY                    IF  (PBHSTDTL-FOUND)
RAY                    AND (ZB3-PLAN-TYPE = PTB-PLAN-TYPE)
RAY                    AND (ZB3-PLAN-CODE = PTB-PLAN-CODE)
RAY                        MOVE ZB4-PER-END-DATE TO BNPTB-PROCESSED-DATE
RAY                    ELSE   
RAY                        PERFORM 860-FIND-NXTRNG-ZB3SET1
RAY                    END-IF
RAY                END-PERFORM
RAY                PERFORM 860-FIND-NXTRNG-ZB4SETW1
RAY            END-PERFORM.
RAY
RAY   *    PERFORM 4430-GET-LAST-BILL-PERIOD.
RAY 
RAY   ******************************************************************
RAY   *4430-GET-LAST-BILL-PERIOD.
RAY   ******************************************************************
RAY   *    MOVE EMP-COMPANY                TO DB-COMPANY.
RAY   *    MOVE EMP-PROCESS-LEVEL          TO DB-PROCESS-LEVEL.
RAY   *    MOVE PTB-PLAN-TYPE              TO DB-PLAN-TYPE.
RAY   *    MOVE PTB-PLAN-CODE              TO DB-PLAN-CODE.
RAY   *    PERFORM 840-FIND-PBLSET1.
RAY   *--- USED DURING THE WRITE TO SEE IF ITS A FUTURE BILLING PERIOD
RAY   *    REMOVED 7/2004 - PBL ISN'T THERE IF THIS PLAN DIDN'T HAVE
RAY   *            ANYONE TO BILL FOR THE PAST MONTH. IT WOULD WORK IF 
RAY   *            YOU COULD JUST LOOK FOR THE HIGHEST DATE FOR THE
RAY   *            PROCESS LEVEL, REGARDLESS OF PLAN TYPE/CODE.
RAY
RAY   ******************************************************************
RAY    4460-WBP-RETRO-START-DATE.
RAY   ******************************************************************
RAY      
RAY        MOVE PTB-START-DATE         TO WS-BNPTB-START-DATE.
RAY   *    -------------------------------------------------------------
RAY   *    | IF THIS IS A NEW STOP DATE, CHANGE RETRO START DATE       |
RAY   *    | TO 1 DAY AFTER THE NEW STOP DATE.                         |
RAY   *    | NO NEED TO RECALC MONTHS THAT WERE BILLED CORRECTLY.      |
RAY   *    -------------------------------------------------------------
RAY        IF (BNPTB-LINE-FC (I1) NOT = "A")
GW         AND ((WS-BNPTB-STOP-DATE  = ZERO)
GW         OR  (WS-BNPTB-STOP-DATE  > PTB-STOP-DATE))
RAY        AND (PTB-STOP-DATE > ZERO)
GW             MOVE PTB-STOP-DATE  TO WSDR-FR-DATE
GW             PERFORM 900-DATE-TO-JULIAN
GW             ADD 1 TO WSDR-JULIAN-DAYS
GW             PERFORM 900-JULIAN-TO-DATE
RAY   *---     DON'T REPLACE AN EARLIER RETRO START DATE
RAY            IF (WSDR-FR-DATE < WS-BNPTB-START-DATE)
RAY            OR (WS-BNPTB-STOP-DATE = ZERO)
GW                 MOVE WSDR-FR-DATE   TO WS-BNPTB-START-DATE.
RAY
RAY   ******************************************************************
RAY    4490-WRITE-RETRO-BILL-TRIGGER.
RAY   ******************************************************************
RAY   *--- SKIP WRITE IF THE RETRO START DATE ALREADY EXISTS FOR THE 
RAY   *    SAME PLAN TYPE/PLAN CODE WITH A DIFFERENT START DATE
RAY        MOVE PTB-COMPANY            TO DB-COMPANY.
RAY        MOVE ZEROES                 TO DB-STATUS.
RAY        MOVE PTB-PLAN-TYPE          TO DB-PLAN-TYPE.
RAY        MOVE PTB-PLAN-CODE          TO DB-PLAN-CODE.
RAY        MOVE PTB-EMPLOYEE           TO DB-EMPLOYEE.
RAY        MOVE PTB-PARTICIPNT         TO DB-PARTICIPNT.
RAY        INITIALIZE                     DB-START-DATE.
RAY        MOVE WBPSET4-PARTICIPNT     TO WS-DB-BEG-RNG.
RAY        PERFORM 850-FIND-BEGRNG-WBPSET4.
RAY        PERFORM 
RAY        UNTIL (WBPBENEFIT-NOTFOUND)
RAY            IF (WBP-RETRO-START-DT <= WS-BNPTB-START-DATE)
RAY                GO TO 4490-END
RAY            END-IF
RAY            PERFORM 860-FIND-NXTRNG-WBPSET4
RAY        END-PERFORM.
RAY
RAY   *--- SKIP WRITE IF FUTURE DATED RETRO
RAY   *    IF (PBBNPLAN-FOUND)
RAY   *    AND (WS-BNPTB-START-DATE > PBL-LAST-BILLED-DT)
RAY   *        GO TO 4490-END.
RAY
RAY        IF  ((PTB-STOP-DATE        > ZERO)
RAY        AND  (BNPTB-PROCESSED-DATE > ZERO)
RAY        AND  (PTB-STOP-DATE        > BNPTB-PROCESSED-DATE))
RAY        OR  ((BNPTB-PROCESSED-DATE > ZEROES)
RAY        AND (WS-BNPTB-START-DATE   >= BNPTB-PROCESSED-DATE))
RAY            GO TO 4490-END.
RAY              
           STRING PTB-PLAN-TYPE        DELIMITED BY SIZE
                  PTB-PLAN-CODE        DELIMITED BY SIZE
                                       INTO DB-DTL-KEY.
           MOVE "TRIGGER"             TO DB-LVL-1-KEY.
           PERFORM 840-FIND-IF1SET1.
           IF (LUPTBLDTL-NOTFOUND)
             GO TO 4490-END
           END-IF.  

RAY        MOVE PTB-COMPANY            TO DB-COMPANY.
RAY        MOVE PTB-PLAN-TYPE          TO DB-PLAN-TYPE.
RAY        MOVE PTB-PLAN-CODE          TO DB-PLAN-CODE.
RAY        MOVE PTB-EMPLOYEE           TO DB-EMPLOYEE.
RAY        MOVE PTB-PARTICIPNT         TO DB-PARTICIPNT.
RAY        INITIALIZE                     DB-START-DATE.
           MOVE WBPSET1-PARTICIPNT     TO WS-DB-BEG-RNG.
RAY        PERFORM 850-MODIFY-BEGRNG-WBPSET1.
RAY        IF (WBPBENEFIT-FOUND)
RAY            PERFORM 830-DELETE-WBPBENEFIT
RAY        END-IF.                      
RAY        PERFORM 800-CREATE-WBPBENEFIT.
RAY        MOVE PTB-COMPANY            TO WBP-COMPANY.
RAY        MOVE PTB-PLAN-TYPE          TO WBP-PLAN-TYPE.
RAY        MOVE PTB-PLAN-CODE          TO WBP-PLAN-CODE.
RAY        MOVE PTB-EMPLOYEE           TO WBP-EMPLOYEE.
RAY        MOVE PTB-PARTICIPNT         TO WBP-PARTICIPNT.
RAY        MOVE PTB-START-DATE         TO WBP-START-DATE.
RAY
RAY        PERFORM 4495-WBPBENEFIT-COMMON-FLDS.
RAY        MOVE WS-BNPTB-START-DATE    TO WBP-RETRO-START-DT.
RAY        PERFORM 820-STORE-WBPBENEFIT.
RAY    
RAY    4490-END.
RAY
RAY   ******************************************************************
RAY    4495-WBPBENEFIT-COMMON-FLDS.
RAY   ******************************************************************
RAY
AI0095         MOVE HHMMSS                 TO WBP-TIME-STAMP.
AI0095         MOVE CRT-USER-NAME          TO WBP-USER-ID.
AI0095         MOVE WS-SYSTEM-DATE-YMD     TO WBP-UPD-DATE.
AI0095         MOVE CRT-PROGRAM-CODE       TO WBP-PROGRAM-CODE.
RAY
AI0095         IF (CRT-PROGRAM-CODE = "BN100")
AI0095         OR (CRT-PROGRAM-CODE = "BN101")  
AI0095         OR (CRT-PROGRAM-CODE = "BN102")
AI0095         OR (CRT-PROGRAM-CODE = "BN103")
AI0095         OR (CRT-PROGRAM-CODE = "BN104")
AI0095         OR (CRT-PROGRAM-CODE = "BN105")
AI0095             MOVE CRT-PROGRAM-CODE   TO WBP-USER-ID.
RAY
GW             MOVE ZERO                   TO WBP-STATUS.
GW             MOVE ZERO                   TO WBP-PROCESS-DATE.
GW             MOVE ZERO                   TO WBP-PROCESS-TIME.
RAY
125000******************************************************************
125100 5100-MOVE-DETAIL-DATA.
125200******************************************************************
125300
125400     MOVE BNPTB-COMPANY              TO PTB-COMPANY.
125500     MOVE BNPTB-PARTICIPNT           TO PTB-PARTICIPNT.
125600     IF  (BNPTB-PART-ENT)
125700          MOVE ZEROES                TO PTB-EMPLOYEE
125800     ELSE
125900          MOVE BNPTB-EMPLOYEE        TO PTB-EMPLOYEE.
126000
126100     MOVE BNPTB-PLAN-TYPE       (I1) TO PTB-PLAN-TYPE.
126200     MOVE BNPTB-PLAN-CODE       (I1) TO PTB-PLAN-CODE.
126300
126400     MOVE BNPTB-START-DATE      (I1) TO PTB-START-DATE.
126500     MOVE BNPTB-STOP-DATE       (I1) TO PTB-STOP-DATE.
126600
126700     MOVE BNPTB-COVER-OPT       (I1) TO PTB-COV-OPTION.
126800     MOVE BNPTB-COVER-AMT       (I1) TO PTB-COVER-AMT.
126900     MOVE BNPTB-EMP-AFT-CONT    (I1) TO PTB-EMP-AFT-CONT.
127000     MOVE BNPTB-COMP-CONT       (I1) TO PTB-COMP-CONT.
127100     MOVE BNPTB-PAY-PER-AMT     (I1) TO PTB-PAY-PER-AMT.
127200     MOVE BNPTB-ANNUAL-AMT      (I1) TO PTB-ANNUAL-AMT.
127300
127400     IF  (BNPTB-PART-ENT)
127500     AND (BNC-CBR-PREM-PCT           > 100)
127600         COMPUTE BNPTB-EXCESS-PREM   = BNC-CBR-PREM-PCT
127700                                     - 100
127800         COMPUTE BNPTB-COST-PER-PCT ROUNDED
127900                                     = BNPTB-EMP-AFT-CONT (I1)
128000                                     / BNC-CBR-PREM-PCT
128100         COMPUTE PTB-EXC-PREM-AMT ROUNDED
128200                                     = BNPTB-COST-PER-PCT
128300                                     * BNPTB-EXCESS-PREM.
128400
128500     MOVE BNPTB-COV-UPD-DT      (I1) TO PTB-COV-UPD-DT.
128600     MOVE BNPTB-COV-GROUP       (I1) TO PTB-COV-GROUP.
128700     MOVE BNPTB-PREM-UPD-DT     (I1) TO PTB-PREM-UPD-DT.
128800     MOVE BNPTB-PREM-GROUP      (I1) TO PTB-PREM-GROUP.
128900
049845     IF (BNPTB-SMOKER-FLAG      (I1) = SPACES)
049845         IF (CRT-SCREEN-CODE = "BN711")
049845             MOVE BNPTB-COMPANY      TO DB-COMPANY
049845             MOVE BNPTB-PARTICIPNT   TO DB-PARTICIPNT
049845             PERFORM 840-FIND-PARSET1
049845             IF (PARTICIPNT-FOUND)
049845                 MOVE PAR-SMOKER     TO BNPTB-SMOKER-FLAG (I1)
049845             END-IF
049845         END-IF
049845         
049845         IF (CRT-SCREEN-CODE = "BN721")
049845             MOVE PEM-SMOKER         TO BNPTB-SMOKER-FLAG (I1)
049845         END-IF
049845     END-IF.
049845
129000     MOVE BNPTB-SMOKER-FLAG     (I1) TO PTB-SMOKER.
J55910     MOVE BNPTB-PROC-LEVEL      (I1) TO PTB-PROCESS-LEVEL.
129100
129200     IF (BNPTB-LINE-FC          (I1) = "A")
129300         MOVE WS-SYSTEM-DATE-YMD     TO PTB-CREATION-DATE
J67329         MOVE HHMMSS                 TO PTB-CREATE-TIME
J67329         MOVE CRT-USER-NAME          TO PTB-CREATE-USER-ID
           END-IF.
129400
J67329     IF (BNPTB-LINE-FC (I1) = "A" OR "C" OR "S")
129500         MOVE WS-SYSTEM-DATE-YMD     TO PTB-UPD-DATE
J67329         MOVE HHMMSS                 TO PTB-TIME-STAMP
J67329         MOVE CRT-USER-NAME          TO PTB-USER-ID
           END-IF.


129700 5100-END.
129800
194000******************************************************************
       5200-CREATE-BNTRANS.
194000******************************************************************

027800     MOVE BNPTB-COMPANY              TO BNSNWS-COMPANY.
027800     MOVE BNPTB-PLAN-TYPE (I1)       TO BNSNWS-PLAN-TYPE.
027800     MOVE BNPTB-PLAN-CODE (I1)       TO BNSNWS-PLAN-CODE.
           INITIALIZE BNSNWS-EMPLOYEE
                      BNSNWS-PARTICIPNT.
           IF (BNPTB-PART-ENT)
               MOVE BNPTB-PARTICIPNT       TO BNSNWS-PARTICIPNT
           ELSE
027800         MOVE BNPTB-EMPLOYEE         TO BNSNWS-EMPLOYEE.
027800     INITIALIZE BNSNWS-DEPENDENT.
027800     MOVE BNPTB-START-DATE (I1)      TO BNSNWS-START-DATE.
           IF (BNPTB-LINE-FC (I1)          = "A" OR "C" OR "D")
               MOVE BNPTB-START-DATE (I1)  TO BNSNWS-EFFECT-DATE
           ELSE
           IF (BNPTB-LINE-FC (I1)          = "S")
               MOVE BNPTB-STOP-DATE (I1)   TO BNSNWS-EFFECT-DATE.
           PERFORM 5000-GET-BNT-TRAN-SEQ-NBR.
           MOVE BNSNWS-TRAN-SEQ-NBR        TO BNPTB-TRAN-SEQ-NBR.

           PERFORM 800-CREATE-BNTRANS.

           MOVE BNPTB-COMPANY              TO BNT-COMPANY.
           MOVE BNPTB-PLAN-TYPE (I1)       TO BNT-PLAN-TYPE.
           MOVE BNPTB-PLAN-CODE (I1)       TO BNT-PLAN-CODE.

           INITIALIZE BNT-EMPLOYEE
                      BNT-PARTICIPNT.
124100     IF (BNPTB-PART-ENT)
               MOVE BNPTB-PARTICIPNT       TO BNT-PARTICIPNT
           ELSE
               MOVE BNPTB-EMPLOYEE         TO BNT-EMPLOYEE.

           INITIALIZE BNT-DEPENDENT.

           MOVE BNPTB-START-DATE (I1)      TO BNT-START-DATE.
           MOVE BNPTB-TRAN-SEQ-NBR         TO BNT-TRAN-SEQ-NBR.

           INITIALIZE BNT-EMP-START.

           IF (BNPTB-PART-ENT)
               MOVE PAR-COMPANY            TO DB-COMPANY
               MOVE PAR-OCCUR-TYPE         TO DB-OCCUR-TYPE
               PERFORM 840-FIND-OCCSET1
               IF (OCCURTYPE-FOUND)
                   MOVE OCC-EVENT-CODE     TO BNT-EVENT-CODE
               END-IF

               MOVE "C"                    TO BNT-COVER-TYPE
           ELSE
               INITIALIZE BNT-EVENT-CODE
               MOVE "R"                    TO BNT-COVER-TYPE.

           MOVE 1                          TO BNT-TRAN-STATUS.
           MOVE BNPTB-LINE-FC (I1)         TO BNT-TRAN-ACTION.
           MOVE BNPTB-REASON (I1)          TO BNT-TRAN-REASON.
           MOVE BNPTB-MEMBER-ID (I1)       TO BNT-MEMBER-ID.

           IF (BNPTB-LINE-FC (I1)          = "A" OR "C" OR "D")
               MOVE BNPTB-START-DATE (I1)  TO BNT-EFFECT-DATE
           ELSE
           IF (BNPTB-LINE-FC (I1)          = "S")
               MOVE BNPTB-STOP-DATE (I1)   TO BNT-EFFECT-DATE.

           MOVE BNPTB-STOP-DATE (I1)       TO BNT-STOP-DATE.

           INITIALIZE BNT-HIPAA-FILE-NBR
                      BNT-FILE-DATE.

           MOVE WS-SYSTEM-DATE-YMD         TO BNT-DATE-STAMP.
           MOVE HHMMSS                     TO BNT-TIME-STAMP.

           IF (CRT-PROGRAM-CODE            = "BN100")
           OR (CRT-PROGRAM-CODE            = "BN101")
           OR (CRT-PROGRAM-CODE            = "BN102")
           OR (CRT-PROGRAM-CODE            = "BN103")
           OR (CRT-PROGRAM-CODE            = "BN104")
           OR (CRT-PROGRAM-CODE            = "BN105")
               MOVE CRT-PROGRAM-CODE       TO BNT-USER-ID
           ELSE
               MOVE CRT-USER-NAME          TO BNT-USER-ID
122477     END-IF.

           PERFORM 820-STORE-BNTRANS.

       5200-END.

194000******************************************************************
       5300-DEFAULT-HIPAA-FIELDS.
194000******************************************************************

           MOVE PLN-CREATE-TRANS           TO BNPTB-CREATE-TRANS (I1).

           IF (BNPTB-CREATE-TRANS (I1)     = "Y")
               MOVE BNPTB-HIPAA-REASON     TO BNPTB-REASON (I1)
               MOVE PLN-MEMBER-ID          TO BNPTB-MEMBER-ID (I1).

       5300-END.

129900******************************************************************
130000 4000-END.
130100******************************************************************

322100******************************************************************
322200 5000-CALC-HDB-STOP-DATE             SECTION.
322300******************************************************************
       5000-START.
322400
           PERFORM
               VARYING I4 FROM 1 BY 1
               UNTIL  (I4 > 25)
               OR    ((BNPTB-SV-SP-SPOUSE (1 I4) = HDB-DEPENDENT)
               AND    (BNPTB-SV-SP-PLN-TP (1 I4) = HDB-PLAN-TYPE)
               AND    (BNPTB-SV-SP-PLN-CD (1 I4) = HDB-PLAN-CODE))

               CONTINUE

           END-PERFORM.

           IF   (BNPTB-FUNCTION-CODE       = "CSCA")
           AND  (BNPTB-LINE-FC (2)         = "A")
           AND  (I4 NOT > 25)
           AND  (BNPTB-SV-SP-END-DT (1 I4) NOT = ZEROES)
           AND ((BNPTB-SV-SP-END-DT (1 I4) < PTB-STOP-DATE)
           OR   (PTB-STOP-DATE             = ZEROES))
               MOVE BNPTB-SV-SP-END-DT (1 I4) TO HDB-STOP-DATE
341100         GO TO 5000-END
           ELSE
341000         MOVE PTB-STOP-DATE          TO HDB-STOP-DATE
               IF (PTB-STOP-DATE           NOT = ZEROES)
               OR (EMD-DEP-TYPE            = "S" OR "P")
341100             GO TO 5000-END.
341200
           INITIALIZE HRDEP-CALC-DEP-N-STUD.
           PERFORM 5200-DEP-AGE-DATE-CALC.

344200     IF (HRDEP-DEP-END-DATE          < PTB-START-DATE)
344300         INITIALIZE HDB-START-DATE
344400                    HDB-STOP-DATE
344500     ELSE
344600     IF (HRDEP-DEP-END-DATE          >= PTB-START-DATE)
344700         IF (HRDEP-DEP-END-DATE      < PTB-STOP-DATE)
344800             MOVE HRDEP-DEP-END-DATE TO HDB-STOP-DATE
344900         ELSE
345000         IF (PTB-STOP-DATE           NOT = ZEROES)
345100             MOVE PTB-STOP-DATE      TO HDB-STOP-DATE
345200         ELSE
345300             MOVE HRDEP-DEP-END-DATE TO HDB-STOP-DATE.
330000
330100     IF  (EMD-DEP-TYPE               = "D")
330200     AND (EMD-DISABLED               = "Y")
330300         MOVE PTB-STOP-DATE          TO HDB-STOP-DATE.
330400
330500 5000-END.
330600
