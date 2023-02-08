******* BS12PD 8.1.18 <1578039472>
000100******************************************************************
      ******************************************************************
      * SDB 09/2015 - MOD TO READ BNANNENR TABLE IS NO ENROLLMENT DATE
      *               IS ENTERED 
      *               MOD TO MOVE NEW-DATE TO BNPEWS-START-DATE
      *  SEARCH ON ESS915 
000400******************************************************************
000200*                             BS12PD                             *
000300******************************************************************
      ******************************************************************
      *                                                                *
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  614959 | J14959 | EXPAND PERCENT CONTRIBUTIONS TO 4 DECIMALS  *
000400******************************************************************
000500 BS12S1-TRANSACTION              SECTION 10.
000600******************************************************************
000700 BS12S1-START.
000800
000900     PERFORM 210-EDIT-ACCESS
001000     THRU    210-END.
001100
001200     IF (NO-ERROR-FOUND)
001300         PERFORM 480-INQUIRE
001400         THRU    480-END.
001500
001600     GO TO BS12S1-TRANSACTION-END.
001700
001800******************************************************************
001900 210-EDIT-ACCESS.
002000******************************************************************
002100
004700     MOVE BS12F1-PLN-COMPANY           TO DB-COMPANY.
004800     INITIALIZE DB-PROCESS-LEVEL.
004900     PERFORM 840-FIND-PRSSET1.
005000     IF (PRSYSTEM-NOTFOUND)
      ******** Company does not exist
005100         MOVE 100                             TO CRT-ERROR-NBR
005200         MOVE BS12F1-PLN-COMPANY-FN           TO CRT-FIELD-NBR
005300         GO TO 210-END.
005400
004700     MOVE BS12F1-PLN-COMPANY           TO DB-COMPANY.
004900     PERFORM 840-FIND-BNCSET1.
005000     IF (BNCOMPANY-NOTFOUND)
      ******** Company not defined in benefit system
005100         MOVE 101                             TO CRT-ERROR-NBR
005200         MOVE BS12F1-PLN-COMPANY-FN           TO CRT-FIELD-NBR
005300         GO TO 210-END.
005400
004700     MOVE BS12F1-PLN-COMPANY           TO DB-COMPANY.
004800     MOVE BS12F1-EMP-EMPLOYEE          TO DB-EMPLOYEE.
004900     PERFORM 840-FIND-EMPSET1.
005000     IF (EMPLOYEE-NOTFOUND)
      ******** Employee does not exist
005100         MOVE 102                             TO CRT-ERROR-NBR
005200         MOVE BS12F1-EMP-EMPLOYEE-FN          TO CRT-FIELD-NBR
005300         GO TO 210-END.
005400
           MOVE EMP-COMPANY                  TO CRT-COMPANY.
           MOVE EMP-PROCESS-LEVEL            TO CRT-PROCESS-LEVEL.
006900     PERFORM 700-HR-EMP-SECURITY.
007000     IF (HRWS-EMP-SECURED)
      ******** Not authorised to access employee information
007100         MOVE 103                             TO CRT-ERROR-NBR
007200         MOVE BS12F1-EMP-EMPLOYEE-FN          TO CRT-FIELD-NBR
007300         GO TO 210-END.
007400
           PERFORM 840-FIND-PEMSET1.

           IF (BS12F1-BAE-RULE-TYPE          = SPACES)
      ******** Rule type is required
005100         MOVE 105                             TO CRT-ERROR-NBR
005200         MOVE BS12F1-BAE-RULE-TYPE-FN         TO CRT-FIELD-NBR
005300         GO TO 210-END.

           IF (BS12F1-BAE-NEW-DATE          = ZEROES)
      ******** New date is required
ESS915          MOVE BS12F1-PLN-COMPANY    TO DB-COMPANY
ESS915          INITIALIZE                    DB-PROCESS-LEVEL
ESS915          MOVE BS12F1-BAE-RULE-TYPE  TO DB-RULE-TYPE
ESS915          INITIALIZE                    DB-GROUP-NAME
ESS915          PERFORM  840-FIND-BAESET1
ESS915          IF (BNANNENR-FOUND)
ESS915            MOVE BAE-NEW-DATE         TO BS12F1-BAE-NEW-DATE                
                ELSE
ESS915            MOVE 106                         TO CRT-ERROR-NBR
ESS915            MOVE BS12F1-BAE-NEW-DATE-FN      TO CRT-FIELD-NBR
ESS915            GO TO 210-END 
                END-IF
          END-IF.
           IF  (BS12F1-BAE-RULE-TYPE         = "F")
           AND (BS12F1-BFS-FAMILY-STATUS     = SPACES)
      ******** Must enter family status code for rule type = "F"
005100         MOVE 104                             TO CRT-ERROR-NBR
005200         MOVE BS12F1-BFS-FAMILY-STATUS-FN     TO CRT-FIELD-NBR
005300         GO TO 210-END.

           IF (BS12F1-FC                 = "I")
P51215     OR (BS12F1-PT-COMPANY     = ZEROES)
               MOVE ZERO                 TO BS12F1-REC-FLAG
P51215         MOVE BS12F1-PLN-COMPANY   TO BS12F1-PT-COMPANY
P51215         MOVE BS12F1-EMP-EMPLOYEE  TO BS12F1-PT-EMPLOYEE
               MOVE BS12F1-BFS-FAMILY-STATUS
                                         TO BS12F1-PT-BFS-FAMILY-STATUS
               MOVE BS12F1-BAE-RULE-TYPE TO BS12F1-PT-BAE-RULE-TYPE
               PERFORM 212-GET-FIRST-RECORD
               THRU    212-END
               IF (ERROR-FOUND)
                   GO TO 210-END
               END-IF

P85400*        PERFORM 630-EDIT-PAY-PLAN
P85400*        THRU    630-END
P85400*        IF (ERROR-FOUND)
P85400*            GO TO 210-END
P85400*        END-IF
           ELSE
           IF (BS12F1-FC                    = "+")
               MOVE BS12F1-PLN-COMPANY      TO DB-COMPANY
               MOVE BS12F1-PT-PROCESS-LEVEL TO DB-PROCESS-LEVEL
               MOVE BS12F1-BFS-FAMILY-STATUS TO DB-FAMILY-STATUS
               MOVE BS12F1-PT-GROUP-NAME    TO DB-GROUP-NAME
               MOVE BS12F1-PT-PROCESS-ORDER TO DB-PROCESS-ORDER
               MOVE BS12F1-PT-PLAN-TYPE     TO DB-PLAN-TYPE
               MOVE BS12F1-PT-PLAN-CODE     TO DB-PLAN-CODE
               IF (BS12F1-BAE-RULE-TYPE     = "F")
                   MOVE BFSSET2-GROUP-NAME  TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-BFSSET2
                   IF (BS12F1-REC-FLAG = 1)
                       MOVE ZEROES             TO BS12F1-REC-FLAG
                   ELSE
                       PERFORM 860-FIND-NXTRNG-BFSSET2
                           UNTIL (BNFAMSTS-NOTFOUND)
                           OR   ((BFS-COMPANY       = DB-COMPANY)
                           AND   (BFS-PROCESS-LEVEL = DB-PROCESS-LEVEL)
                           AND   (BFS-FAMILY-STATUS = DB-FAMILY-STATUS)
                           AND   (BFS-GROUP-NAME    = DB-GROUP-NAME)
                           AND   (BFS-PROCESS-ORDER = DB-PROCESS-ORDER)
                           AND   (BFS-PLAN-TYPE     = DB-PLAN-TYPE)
                           AND   (BFS-PLAN-CODE     = DB-PLAN-CODE))
                       IF (BNFAMSTS-NOTFOUND)
                           MOVE BFSSET2-GROUP-NAME TO WS-DB-BEG-RNG
                           PERFORM 850-FIND-BEGRNG-BFSSET2
                       END-IF
                   END-IF
               ELSE
               IF (BS12F1-BAE-RULE-TYPE     = "N")
                   MOVE BNNSET2-GROUP-NAME TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-BNNSET2
                   IF (BS12F1-REC-FLAG = 1)
                       MOVE ZEROES             TO BS12F1-REC-FLAG
                   ELSE
                       PERFORM 860-FIND-NXTRNG-BNNSET2
                           UNTIL (BNNEWHIRE-NOTFOUND)
                           OR   ((BNN-COMPANY       = DB-COMPANY)
                           AND   (BNN-PROCESS-LEVEL = DB-PROCESS-LEVEL)
                           AND   (BNN-GROUP-NAME    = DB-GROUP-NAME)
                           AND   (BNN-PROCESS-ORDER = DB-PROCESS-ORDER)
                           AND   (BNN-PLAN-TYPE     = DB-PLAN-TYPE)
                           AND   (BNN-PLAN-CODE     = DB-PLAN-CODE))
                       IF (BNNEWHIRE-NOTFOUND)
                           MOVE BNNSET2-GROUP-NAME TO WS-DB-BEG-RNG
                           PERFORM 850-FIND-BEGRNG-BNNSET2
                       END-IF
                   END-IF
               ELSE
                   MOVE BPESET2-GROUP-NAME TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-BPESET2
                   IF (BS12F1-REC-FLAG = 1)
                       MOVE ZEROES             TO BS12F1-REC-FLAG
                   ELSE
                       PERFORM 860-FIND-NXTRNG-BPESET2
                           UNTIL (BNPLNENR-NOTFOUND)
                           OR   ((BPE-COMPANY       = DB-COMPANY)
                           AND   (BPE-PROCESS-LEVEL = DB-PROCESS-LEVEL)
                           AND   (BPE-GROUP-NAME    = DB-GROUP-NAME)
                           AND   (BPE-PROCESS-ORDER = DB-PROCESS-ORDER)
                           AND   (BPE-PLAN-TYPE     = DB-PLAN-TYPE)
                           AND   (BPE-PLAN-CODE     = DB-PLAN-CODE))
                       IF (BNPLNENR-NOTFOUND)
                           MOVE BPESET2-GROUP-NAME TO WS-DB-BEG-RNG
                           PERFORM 850-FIND-BEGRNG-BPESET2
                       END-IF
                   END-IF.

008100 210-END.
008200
008300******************************************************************
       212-GET-FIRST-RECORD.
008300******************************************************************

           IF (BS12F1-BAE-RULE-TYPE          = "F")
               MOVE BS12F1-PLN-COMPANY       TO BNREWS-COMPANY
               INITIALIZE BNREWS-PROC-LEVEL
               MOVE BS12F1-BFS-FAMILY-STATUS TO BNREWS-FAMILY-STATUS
               MOVE BS12F1-EMP-EMPLOYEE      TO BNREWS-EMPLOYEE
               MOVE "BFS"                    TO BNREWS-FILE-PREFIX
               PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70
               IF (ERROR-FOUND)
                   GO TO 212-END
               END-IF
           ELSE
           IF (BS12F1-BAE-RULE-TYPE          = "N")
               MOVE BS12F1-PLN-COMPANY       TO BNREWS-COMPANY
               INITIALIZE BNREWS-PROC-LEVEL
               MOVE BS12F1-EMP-EMPLOYEE      TO BNREWS-EMPLOYEE
               MOVE "BNN"                    TO BNREWS-FILE-PREFIX
               PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70
               IF (ERROR-FOUND)
                   GO TO 212-END
               END-IF
           ELSE
               MOVE BS12F1-PLN-COMPANY       TO BNREWS-COMPANY
               INITIALIZE BNREWS-PROC-LEVEL
               MOVE BS12F1-EMP-EMPLOYEE      TO BNREWS-EMPLOYEE
               MOVE "BPE"                    TO BNREWS-FILE-PREFIX
               PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70
               IF (ERROR-FOUND)
                   GO TO 212-END.

           IF (BS12F1-BAE-RULE-TYPE    = "F")
               MOVE BFS-COMPANY        TO DB-COMPANY
                                          BS12WS-COMPANY
               MOVE BFS-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
                                          BS12WS-PROCESS-LEVEL
               MOVE BFS-FAMILY-STATUS  TO DB-FAMILY-STATUS
                                          BS12WS-FAMILY-STATUS
               MOVE BFS-GROUP-NAME     TO DB-GROUP-NAME
                                          BS12WS-GROUP-NAME
               MOVE BFSSET2-GROUP-NAME TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-BFSSET2
           ELSE
           IF (BS12F1-BAE-RULE-TYPE    = "N")
               MOVE BNN-COMPANY        TO DB-COMPANY
                                          BS12WS-COMPANY
               MOVE BNN-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
                                          BS12WS-PROCESS-LEVEL
               MOVE BNN-GROUP-NAME     TO DB-GROUP-NAME
                                          BS12WS-GROUP-NAME
               MOVE BNNSET2-GROUP-NAME TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-BNNSET2
           ELSE
               MOVE BPE-COMPANY        TO DB-COMPANY
                                          BS12WS-COMPANY
               MOVE BPE-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
                                          BS12WS-PROCESS-LEVEL
               MOVE BPE-GROUP-NAME     TO DB-GROUP-NAME
                                          BS12WS-GROUP-NAME
               MOVE BPESET2-GROUP-NAME TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-BPESET2.
015400
009000     PERFORM 600-MOVE-TO-SCREEN
009100     THRU    600-END.
009200
       212-END.

008300******************************************************************
008400 480-INQUIRE.
008500******************************************************************
008600
031200     IF (BS12F1-FC               = "I")
031300         MOVE "+"                TO BS12F1-FC.
031400
014700     INITIALIZE BS12F1-DETAIL-GROUP.

           MOVE 1                      TO I1.
           IF (BS12F1-BAE-RULE-TYPE    = "F")
014800         PERFORM 610-MOVE-DTL-TO-SCREEN
014900         THRU    610-END
                   UNTIL (BNFAMSTS-NOTFOUND)
015100             OR    (I1           > BS12WS-DETAIL-LINES)
                   OR    (ERROR-FOUND)
               IF (BNFAMSTS-NOTFOUND)
                   MOVE "Y"                TO BS12F1-PT-INQUIRY-COMPLETE
083800             MOVE CRT-INQ-COMPLETE   TO CRT-MESSAGE
                   MOVE 1                  TO BS12F1-REC-FLAG
P51215             MOVE BS12F1-PT-COMPANY  TO DB-COMPANY
                   MOVE BS12F1-PT-PROCESS-LEVEL TO DB-PROCESS-LEVEL
                   MOVE BS12F1-PT-BFS-FAMILY-STATUS TO DB-FAMILY-STATUS
                   MOVE BS12F1-PT-GROUP-NAME    TO DB-GROUP-NAME
                   MOVE BFSSET2-GROUP-NAME TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-BFSSET2
083900         ELSE
                   MOVE "N"                TO BS12F1-PT-INQUIRY-COMPLETE
084000             MOVE CRT-MORE-RECS      TO CRT-MESSAGE
079700             MOVE 110                TO CRT-MSG-NBR
                   MOVE BFS-PROCESS-LEVEL  TO BS12F1-PT-PROCESS-LEVEL
                   MOVE BFS-GROUP-NAME     TO BS12F1-PT-GROUP-NAME
                   MOVE BFS-PROCESS-ORDER  TO BS12F1-PT-PROCESS-ORDER
                   MOVE BFS-PLAN-TYPE      TO BS12F1-PT-PLAN-TYPE
                   MOVE BFS-PLAN-CODE      TO BS12F1-PT-PLAN-CODE
084100         END-IF
           ELSE
           IF (BS12F1-BAE-RULE-TYPE    = "N")
014800         PERFORM 610-MOVE-DTL-TO-SCREEN
014900         THRU    610-END
                   UNTIL (BNNEWHIRE-NOTFOUND)
015100             OR    (I1           > BS12WS-DETAIL-LINES)
                   OR    (ERROR-FOUND)
               IF (BNNEWHIRE-NOTFOUND)
                   MOVE "Y"                TO BS12F1-PT-INQUIRY-COMPLETE
083800             MOVE CRT-INQ-COMPLETE   TO CRT-MESSAGE
                   MOVE 1                  TO BS12F1-REC-FLAG
P51215             MOVE BS12F1-PT-COMPANY  TO DB-COMPANY
                   MOVE BS12F1-PT-PROCESS-LEVEL TO DB-PROCESS-LEVEL
                   MOVE BS12F1-PT-GROUP-NAME    TO DB-GROUP-NAME
                   MOVE BNNSET2-GROUP-NAME TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-BNNSET2
083900         ELSE
                   MOVE "N"                TO BS12F1-PT-INQUIRY-COMPLETE
084000             MOVE CRT-MORE-RECS      TO CRT-MESSAGE
079700             MOVE 110                TO CRT-MSG-NBR
                   MOVE BNN-PROCESS-LEVEL  TO BS12F1-PT-PROCESS-LEVEL
                   MOVE BNN-GROUP-NAME     TO BS12F1-PT-GROUP-NAME
                   MOVE BNN-PROCESS-ORDER  TO BS12F1-PT-PROCESS-ORDER
                   MOVE BNN-PLAN-TYPE      TO BS12F1-PT-PLAN-TYPE
                   MOVE BNN-PLAN-CODE      TO BS12F1-PT-PLAN-CODE
084100         END-IF
           ELSE
014800         PERFORM 610-MOVE-DTL-TO-SCREEN
014900         THRU    610-END
                   UNTIL (BNPLNENR-NOTFOUND)
015100             OR    (I1           > BS12WS-DETAIL-LINES)
                   OR    (ERROR-FOUND)
               IF (BNPLNENR-NOTFOUND)
                   MOVE "Y"                TO BS12F1-PT-INQUIRY-COMPLETE
083800             MOVE CRT-INQ-COMPLETE   TO CRT-MESSAGE
                   MOVE 1                  TO BS12F1-REC-FLAG
P51215             MOVE BS12F1-PT-COMPANY  TO DB-COMPANY
                   MOVE BS12F1-PT-PROCESS-LEVEL TO DB-PROCESS-LEVEL
                   MOVE BS12F1-PT-GROUP-NAME    TO DB-GROUP-NAME
                   MOVE BPESET2-GROUP-NAME TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-BPESET2
083900         ELSE
                   MOVE "N"                TO BS12F1-PT-INQUIRY-COMPLETE
084000             MOVE CRT-MORE-RECS      TO CRT-MESSAGE
079700             MOVE 110                TO CRT-MSG-NBR
                   MOVE BPE-PROCESS-LEVEL  TO BS12F1-PT-PROCESS-LEVEL
                   MOVE BPE-GROUP-NAME     TO BS12F1-PT-GROUP-NAME
                   MOVE BPE-PROCESS-ORDER  TO BS12F1-PT-PROCESS-ORDER
                   MOVE BPE-PLAN-TYPE      TO BS12F1-PT-PLAN-TYPE
                   MOVE BPE-PLAN-CODE      TO BS12F1-PT-PLAN-CODE.
015400
016500 480-END.
016600
016700******************************************************************
016800 600-MOVE-TO-SCREEN.
016900******************************************************************
017000
145800     MOVE PRS-NAME               TO BS12F1-PRS-NAME.
145900
154100     MOVE EMP-LAST-NAME          TO HRWS-LAST-NAME.
154300     MOVE EMP-FIRST-NAME         TO HRWS-FIRST-NAME.
154500     MOVE EMP-MIDDLE-INIT        TO HRWS-MIDDLE-INIT.
154700     PERFORM 750-HR-FORMAT-NAME.
154800     MOVE HRWS-FORMAT-NAME       TO BS12F1-EMP-SHORT-NAME.
017200
017300 600-END.
017400
017500******************************************************************
017600 610-MOVE-DTL-TO-SCREEN.
017700******************************************************************
017800
           IF (BS12F1-BAE-RULE-TYPE    = "F")
               MOVE BFS-COMPANY        TO DB-COMPANY
               MOVE BFS-PLAN-TYPE      TO DB-PLAN-TYPE
               MOVE BFS-PLAN-CODE      TO DB-PLAN-CODE
           ELSE
           IF (BS12F1-BAE-RULE-TYPE    = "N")
               MOVE BNN-COMPANY        TO DB-COMPANY
               MOVE BNN-PLAN-TYPE      TO DB-PLAN-TYPE
               MOVE BNN-PLAN-CODE      TO DB-PLAN-CODE
           ELSE
               MOVE BPE-COMPANY        TO DB-COMPANY
               MOVE BPE-PLAN-TYPE      TO DB-PLAN-TYPE
               MOVE BPE-PLAN-CODE      TO DB-PLAN-CODE.

           PERFORM 840-FIND-PLNSET1.
018400
           IF (PLN-FLEX-PLAN           NOT = SPACES)
               MOVE PLN-FLEX-PLAN      TO DB-FLEX-PLAN
               PERFORM 840-FIND-FLPSET1
               MOVE FLP-SPEND-ONLY     TO BS12F1-FLP-SPEND-ONLY (I1)
               IF (FLP-SPEND-ONLY      = "N")
                   MOVE FLP-FLEX-PLAN  TO BS12F1-FLP-FLEX-PLAN (I1).

           MOVE PLN-CREATE-TRANS       TO BS12F1-BNT-CREATE-TRANS (I1).

           MOVE BS12F1-PLN-COMPANY     TO BNPEWS-COMPANY.
           MOVE PLN-PLAN-TYPE          TO BNPEWS-PLAN-TYPE.
           MOVE PLN-PLAN-CODE          TO BNPEWS-PLAN-CODE.
           MOVE BS12F1-EMP-EMPLOYEE    TO BNPEWS-EMPLOYEE.
           MOVE "E"                    TO BNPEWS-COVER-TYPE.
ESS915     MOVE BS12F1-BAE-NEW-DATE    TO BNPEWS-START-DATE.
           PERFORM 5000-EDIT-GROUP-N-ZIP-70.
           IF (ERROR-FOUND)
               INITIALIZE CRT-ERROR-NBR
                          CRT-ERROR-CAT
                          CRT-ERR-VAR1
                          CRT-ERR-VAR2
                          CRT-ERR-VAR3
                          CRT-ERR-VAR4
                          CRT-ERR-VAR5
               GO TO 610-NEXT-BNPLNENR.
           MOVE BS12F1-PLN-COMPANY     TO BNEDWS-COMPANY.
           MOVE PLN-PLAN-TYPE          TO BNEDWS-PLAN-TYPE.
           MOVE PLN-PLAN-CODE          TO BNEDWS-PLAN-CODE.
           MOVE BS12F1-EMP-EMPLOYEE    TO BNEDWS-EMPLOYEE.
           MOVE BS12F1-BAE-NEW-DATE    TO BNEDWS-AS-OF-DATE.
           PERFORM 5000-ELIGIBILITY-DATE-CALC-70.
           IF (ERROR-FOUND)
               INITIALIZE CRT-ERROR-NBR
                          CRT-ERROR-CAT
                          CRT-ERR-VAR1
                          CRT-ERR-VAR2
                          CRT-ERR-VAR3
                          CRT-ERR-VAR4
                          CRT-ERR-VAR5
               GO TO 610-NEXT-BNPLNENR.

           IF  (BS12F1-BAE-RULE-TYPE    NOT = "N")
           AND (BNEDWS-ELIGIBILITY-DATE > BS12F1-BAE-NEW-DATE)
               GO TO 610-NEXT-BNPLNENR.

           MOVE BS12F1-PLN-COMPANY     TO BNPEWS-COMPANY.
           MOVE PLN-PLAN-TYPE          TO BNPEWS-PLAN-TYPE.
           MOVE PLN-PLAN-CODE          TO BNPEWS-PLAN-CODE.
           MOVE BS12F1-EMP-EMPLOYEE    TO BNPEWS-EMPLOYEE.
           INITIALIZE BNPEWS-START-DATE.
           IF (BS12F1-BAE-RULE-TYPE NOT = "N")
               MOVE BS12F1-BAE-NEW-DATE
                                       TO BNPEWS-START-DATE
           END-IF.
           MOVE "E"                    TO BNPEWS-COVER-TYPE.
           PERFORM 5000-EDIT-PLAN-START-DATE-70.
           IF (ERROR-FOUND)
               INITIALIZE CRT-ERROR-NBR
                          CRT-ERROR-CAT
                          CRT-ERR-VAR1
                          CRT-ERR-VAR2
                          CRT-ERR-VAR3
                          CRT-ERR-VAR4
                          CRT-ERR-VAR5
               IF  (PLN-STOP-DATE      NOT = ZEROES)
               AND (BNPEWS-START-DATE  > PLN-STOP-DATE)
                    GO TO 610-NEXT-BNPLNENR.

017900     MOVE PLN-PLAN-TYPE          TO BS12F1-PLN-PLAN-TYPE (I1).
018000     MOVE PLN-PLAN-CODE          TO BS12F1-PLN-PLAN-CODE (I1).
018100     MOVE PLN-DISPLAY-DESC       TO BS12F1-PLN-DISPLAY-DESC (I1).
018100     MOVE PLN-DESC               TO BS12F1-PLN-DESC (I1).
018200     MOVE PLN-PLAN-OPTION        TO BS12F1-PLN-PLAN-OPTION (I1).
           MOVE BNEDWS-ELIGIBILITY-DATE TO BS12F1-PLN-ELIG-DATE (I1).

           IF (PLN-COVERAGE-TYPE       = "2")
               MOVE BS12F1-PLN-COMPANY TO BNREWS-COMPANY
               MOVE PLN-PLAN-TYPE      TO BNREWS-PLAN-TYPE
               MOVE PLN-PLAN-CODE      TO BNREWS-PLAN-CODE
               MOVE "E"                TO BNREWS-COVER-TYPE
               MOVE BS12F1-EMP-EMPLOYEE TO BNREWS-EMPLOYEE
               IF (BS12F1-BAE-RULE-TYPE NOT = "N")
                   MOVE BS12F1-BAE-NEW-DATE
                                       TO BNREWS-AS-OF-DATE
               END-IF
               MOVE "CVR"              TO BNREWS-FILE-PREFIX
               PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70
               IF (ERROR-FOUND)
                   INITIALIZE CRT-ERROR-NBR
                              CRT-ERROR-CAT
                              CRT-ERR-VAR1
                              CRT-ERR-VAR2
                              CRT-ERR-VAR3
                              CRT-ERR-VAR4
                              CRT-ERR-VAR5
                   GO TO 610-NEXT-BNPLNENR
               END-IF
               MOVE CVR-BN-PLAN-TYPE   TO BS12F1-CVR-BN-PLAN-TYPE (I1)
               MOVE CVR-BN-PLAN-CODE   TO BS12F1-CVR-BN-PLAN-CODE (I1).

           IF (PLN-COVERAGE-TYPE       = "1")
               MOVE 1                  TO BS12F1-RECORD-TYPE (I1)
           ELSE
           IF  (PLN-COVERAGE-TYPE      = "2")
           AND (CVR-CALC-TYPE          = "F")
               MOVE 2                  TO BS12F1-RECORD-TYPE (I1)
           ELSE
           IF  (PLN-COVERAGE-TYPE      = "2")
           AND (CVR-CALC-TYPE          = "M")
               MOVE 3                  TO BS12F1-RECORD-TYPE (I1)
           ELSE
           IF  (PLN-COVERAGE-TYPE      = "2")
           AND (CVR-CALC-TYPE          = "S")
               MOVE 4                  TO BS12F1-RECORD-TYPE (I1)
           ELSE
           IF  (PLN-COVERAGE-TYPE      = "2")
           AND (CVR-CALC-TYPE          = "P")
               MOVE 5                  TO BS12F1-RECORD-TYPE (I1)
           ELSE
P60050     IF  (PLN-COVERAGE-TYPE      = "2")   
P60050     AND (CVR-CALC-TYPE          = "N")
P60050          MOVE 13                TO BS12F1-RECORD-TYPE (I1)   
P60050     ELSE  
           IF  (PLN-CONTRIB-TYPE       = "5")
           AND (PLN-PLAN-TYPE          NOT = "VA")
               MOVE 6                  TO BS12F1-RECORD-TYPE (I1)
           ELSE
           IF  (PLN-CONTRIB-TYPE       = "5")
           AND (PLN-PLAN-TYPE          = "VA")
               MOVE 7                  TO BS12F1-RECORD-TYPE (I1)
           ELSE
           IF (PLN-CONTRIB-TYPE        = "6")
               MOVE 8                  TO BS12F1-RECORD-TYPE (I1)
           ELSE
           IF (PLN-CONTRIB-TYPE        = "7")
               MOVE 9                  TO BS12F1-RECORD-TYPE (I1)
           ELSE
           IF  (PLN-COVERAGE-TYPE      = "0")
           AND (PLN-CONTRIB-TYPE       = "3")
               MOVE 10                 TO BS12F1-RECORD-TYPE (I1)
           ELSE
           IF  (PLN-COVERAGE-TYPE      = "0")
           AND (PLN-CONTRIB-TYPE       = "0")
               MOVE 12                 TO BS12F1-RECORD-TYPE (I1).

           MOVE PLN-WAIVE-FLAG         TO BS12F1-PLN-WAIVE-FLAG (I1).

J14959     MOVE PLN-COVERAGE-TYPE      TO BS12F1-PLN-COVERAGE-TYPE (I1).
J14959     MOVE PLN-CONTRIB-TYPE       TO BS12F1-PLN-CONTRIB-TYPE (I1).
J14959     MOVE SPACES                 TO BS12F1-PRE-CALC-TYPE (I1).
J14959     IF (PLN-CONTRIB-TYPE        NOT = "0" AND "X")
J14959         PERFORM 670-DO-CONTRIBUTION
J14959         THRU    670-END
J14959         IF (ERROR-FOUND)
J14959             INITIALIZE             CRT-ERROR-NBR        
J14959         ELSE
J14959             MOVE PRE-CALC-TYPE  TO BS12F1-PRE-CALC-TYPE (I1)
J14959         END-IF
J14959     END-IF.

           IF (BS12F1-BAE-RULE-TYPE    = "F")
               MOVE BFS-PLAN-GROUP     TO BS12F1-BPE-PLAN-GROUP (I1)
               MOVE BFS-PROCESS-ORDER  TO BS12F1-BPE-PROCESS-ORDER (I1)
               MOVE BFS-EMPLOYEE-COST  TO BS12F1-BPE-EMPLOYEE-COST (I1)
               MOVE BFS-COMPANY-COST   TO BS12F1-BPE-COMPANY-COST (I1)
      *        MOVE BFS-CO-PROVIDED    TO BS12F1-BPE-CO-PROVIDED (I1)
               MOVE BFS-DEP-BENEFITS   TO BS12F1-BPE-DEP-BENEFITS (I1)
      *        MOVE BFS-BENEFICIARY    TO BS12F1-BPE-BENEFICIARY (I1)
               MOVE BFS-ADD-ALLOWED    TO BS12F1-BFS-ADD-ALLOWED (I1)
               MOVE BFS-CHANGE-ALLOWED TO BS12F1-BFS-CHANGE-ALLOWED (I1)
               MOVE BFS-STOP-ALLOWED   TO BS12F1-BFS-STOP-ALLOWED (I1)
J90521         MOVE BFS-LINK-PLAN-CODE TO BS12F1-LINK-PLAN-CODE (I1)
J90521         MOVE BFS-LINK-PLAN-TYPE TO BS12F1-LINK-PLAN-TYPE (I1)
               IF (BFS-ADD-DATE        NOT = SPACES)
                   PERFORM 612-COMP-ADD-DATE
                   THRU    612-END
               END-IF
               IF (BFS-CHANGE-DATE     NOT = SPACES)
                   PERFORM 614-COMP-CHANGE-DATE
                   THRU    614-END
               END-IF
               IF (BFS-STOPPED-DATE    NOT = SPACES)
                   PERFORM 616-COMP-STOPPED-DATE
                   THRU    616-END
               END-IF
      *        MOVE BFS-INQUIRE-ONLY   TO BS12F1-BFS-INQUIRE-ONLY (I1)
               MOVE BFS-EOI-DAYS       TO BS12F1-BFS-EOI-DAYS (I1)
               IF (BFS-EOI-DAYS        = ZEROES)
                   INITIALIZE BS12F1-LAST-CHG-DATE (I1)
               ELSE
                   MOVE BS12F1-BAE-NEW-DATE TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   ADD BFS-EOI-DAYS         TO WSDR-JULIAN-DAYS
                   PERFORM 900-JULIAN-TO-DATE
                   MOVE WSDR-FR-DATE        TO BS12F1-LAST-CHG-DATE (I1)
               END-IF
               MOVE BFS-PLAN-GROUP     TO DB-PLAN-GROUP
               MOVE BFS-CREATE-TRANS   TO BS12F1-BNT-CREATE-TRANS (I1)
               MOVE BFS-DEFAULT-REASON (1) TO BS12F1-BNT-REASON (I1)
               IF (BS12F1-BNT-CREATE-TRANS (I1) = "Y")
                   MOVE PLN-MEMBER-ID  TO BS12F1-BNT-MEMBER-ID (I1)
               END-IF
           ELSE
           IF (BS12F1-BAE-RULE-TYPE    = "N")
               MOVE BNN-PLAN-GROUP     TO BS12F1-BPE-PLAN-GROUP (I1)
               MOVE BNN-PROCESS-ORDER  TO BS12F1-BPE-PROCESS-ORDER (I1)
               MOVE BNN-EMPLOYEE-COST  TO BS12F1-BPE-EMPLOYEE-COST (I1)
               MOVE BNN-COMPANY-COST   TO BS12F1-BPE-COMPANY-COST (I1)
      *        MOVE BNN-CO-PROVIDED    TO BS12F1-BPE-CO-PROVIDED (I1)
               MOVE BNN-DEP-BENEFITS   TO BS12F1-BPE-DEP-BENEFITS (I1)
      *        MOVE BNN-BENEFICIARY    TO BS12F1-BPE-BENEFICIARY (I1)
               MOVE BNN-PLAN-GROUP     TO DB-PLAN-GROUP
               MOVE BNN-EOI-DAYS       TO BS12F1-BFS-EOI-DAYS (I1)
J90521         MOVE BNN-LINK-PLAN-CODE TO BS12F1-LINK-PLAN-CODE (I1)
J90521         MOVE BNN-LINK-PLAN-TYPE TO BS12F1-LINK-PLAN-TYPE (I1)
               IF (BNN-EOI-DAYS        = ZEROES)
                   INITIALIZE BS12F1-LAST-CHG-DATE (I1)
               ELSE
                   MOVE BS12F1-BAE-NEW-DATE TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   ADD BNN-EOI-DAYS         TO WSDR-JULIAN-DAYS
                   PERFORM 900-JULIAN-TO-DATE
                   MOVE WSDR-FR-DATE        TO BS12F1-LAST-CHG-DATE (I1)
               END-IF
               MOVE BNN-CREATE-TRANS   TO BS12F1-BNT-CREATE-TRANS (I1)
               MOVE BNN-DEFAULT-REASON (1) TO BS12F1-BNT-REASON (I1)
               IF (BS12F1-BNT-CREATE-TRANS (I1) = "Y")
                   MOVE PLN-MEMBER-ID  TO BS12F1-BNT-MEMBER-ID (I1)
               END-IF
               
J47712         MOVE BNEDWS-ELIGIBILITY-DATE
J47712                                 TO BS12F1-BNN-EFFECTIVE-DATE (I1)               
J47712         IF (BNN-EFFECTIVE-CODE     = "CD")
J47712*        Use Current Date rather than eligiblity date for Effective Dt
J47712             IF (WS-SYSTEM-DATE-YMD >= BNEDWS-ELIGIBILITY-DATE)
J47712                 MOVE WS-SYSTEM-DATE-YMD
J47712                                 TO BS12F1-BNN-EFFECTIVE-DATE (I1)
J47712             END-IF
J47712         END-IF 

           ELSE
               MOVE BPE-PLAN-GROUP     TO BS12F1-BPE-PLAN-GROUP (I1)
               MOVE BPE-PROCESS-ORDER  TO BS12F1-BPE-PROCESS-ORDER (I1)
               MOVE BPE-EMPLOYEE-COST  TO BS12F1-BPE-EMPLOYEE-COST (I1)
               MOVE BPE-COMPANY-COST   TO BS12F1-BPE-COMPANY-COST (I1)
      *        MOVE BPE-CO-PROVIDED    TO BS12F1-BPE-CO-PROVIDED (I1)
               MOVE BPE-DEP-BENEFITS   TO BS12F1-BPE-DEP-BENEFITS (I1)
      *        MOVE BPE-BENEFICIARY    TO BS12F1-BPE-BENEFICIARY (I1)
               MOVE BPE-PLAN-GROUP     TO DB-PLAN-GROUP
               MOVE BPE-CREATE-TRANS   TO BS12F1-BNT-CREATE-TRANS (I1)
               MOVE BPE-DEFAULT-REASON (1) TO BS12F1-BNT-REASON (I1)
J90521         MOVE BPE-LINK-PLAN-CODE TO BS12F1-LINK-PLAN-CODE (I1)
J90521         MOVE BPE-LINK-PLAN-TYPE TO BS12F1-LINK-PLAN-TYPE (I1)
               IF (BS12F1-BNT-CREATE-TRANS (I1) = "Y")
                   MOVE PLN-MEMBER-ID  TO BS12F1-BNT-MEMBER-ID (I1)
               END-IF
           END-IF.

           PERFORM 840-FIND-BPGSET1.
           IF  (BNPLNGROUP-NOTFOUND)
               INITIALIZE BPG-NBR-PLANS.
           MOVE BPG-NBR-PLANS          TO BS12F1-BPG-NBR-PLANS (I1).

           IF (BS12F1-FLP-FLEX-PLAN (I1)    NOT = SPACES)
               IF (BS12F1-BAE-RULE-TYPE     = "A")
                   MOVE BS12F1-PLN-COMPANY  TO DB-COMPANY
                   MOVE BS12F1-EMP-EMPLOYEE TO DB-EMPLOYEE
                   MOVE BS12F1-BAE-NEW-DATE TO DB-START-DATE
                   PERFORM 850-FIND-NLT-EFRSET3
                   PERFORM 860-FIND-NEXT-EFRSET3
                       UNTIL (EMPFLEXREM-NOTFOUND)
                       OR    (EFR-COMPANY    NOT = DB-COMPANY)
                       OR    (EFR-EMPLOYEE   NOT = DB-EMPLOYEE)
                       OR    (EFR-START-DATE > DB-START-DATE)
                   IF  (EMPFLEXREM-FOUND)
                   AND (EFR-COMPANY          = DB-COMPANY)
                   AND (EFR-EMPLOYEE         = DB-EMPLOYEE)
                   AND (EFR-START-DATE       > DB-START-DATE)
      **************** Future dated flex periods exist
                       INITIALIZE BS12F1-DETAIL-GROUP
                       MOVE 120                    TO CRT-ERROR-NBR
                       MOVE BS12F1-BAE-NEW-DATE-FN TO CRT-FIELD-NBR
                       GO TO 610-END
                   END-IF
               ELSE
               IF (BS12F1-BAE-RULE-TYPE   = "N")
                   MOVE BS12F1-PLN-COMPANY  TO DB-COMPANY
                   MOVE BS12F1-EMP-EMPLOYEE TO DB-EMPLOYEE
                   MOVE BS12F1-BAE-NEW-DATE TO DB-START-DATE
                   PERFORM 850-FIND-NLT-EFRSET3
                   PERFORM 860-FIND-NEXT-EFRSET3
                       UNTIL (EMPFLEXREM-NOTFOUND)
                       OR    (EFR-COMPANY    NOT = DB-COMPANY)
                       OR    (EFR-EMPLOYEE   NOT = DB-EMPLOYEE)
                       OR    (EFR-START-DATE NOT = EFR-PLAN-START-DT)
                   IF  (EMPFLEXREM-FOUND)
                   AND (EFR-COMPANY          = DB-COMPANY)
                   AND (EFR-EMPLOYEE         = DB-EMPLOYEE)
                   AND (EFR-START-DATE       >= DB-START-DATE)
                   AND (EFR-START-DATE       NOT = EFR-PLAN-START-DT)
      **************** Future dated flex periods exist
                       INITIALIZE BS12F1-DETAIL-GROUP
                       MOVE 120                    TO CRT-ERROR-NBR
                       MOVE BS12F1-BAE-NEW-DATE-FN TO CRT-FIELD-NBR
                       GO TO 610-END
                   END-IF
               ELSE
               IF (BS12F1-BAE-RULE-TYPE   = "F")
                   INITIALIZE BS12WS-FS-WRK-DATE
                   IF  (BS12F1-BFS-ADD-DATE (I1) NOT = ZEROES)
                       MOVE BS12F1-BFS-ADD-DATE (I1) 
                                                 TO BS12WS-FS-WRK-DATE
                   END-IF
                   IF   (BS12WS-FS-WRK-DATE = ZEROES)
                   OR  ((BS12F1-BFS-CHANGE-DATE (I1) NOT = ZEROES)
                   AND  (BS12F1-BFS-CHANGE-DATE (I1)
                                            < BS12WS-FS-WRK-DATE))
                       MOVE BS12F1-BFS-CHANGE-DATE (I1)
                                                 TO BS12WS-FS-WRK-DATE
                   END-IF
                   IF   (BS12WS-FS-WRK-DATE = ZEROES)
                   OR  ((BS12F1-BFS-STOPPED-DATE (I1) NOT = ZEROES)
                   AND  (BS12F1-BFS-STOPPED-DATE (I1)
                                            < BS12WS-FS-WRK-DATE))
                       MOVE BS12F1-BFS-STOPPED-DATE (I1)
                                                 TO BS12WS-FS-WRK-DATE
                   END-IF
                   MOVE BS12F1-PLN-COMPANY  TO DB-COMPANY
                   MOVE BS12F1-EMP-EMPLOYEE TO DB-EMPLOYEE
                   MOVE BS12WS-FS-WRK-DATE  TO DB-START-DATE
                   PERFORM 850-FIND-NLT-EFRSET3
                   PERFORM 860-FIND-NEXT-EFRSET3
                       UNTIL (EMPFLEXREM-NOTFOUND)
                       OR    (EFR-COMPANY    NOT = DB-COMPANY)
                       OR    (EFR-EMPLOYEE   NOT = DB-EMPLOYEE)
                       OR    (EFR-START-DATE     > DB-START-DATE)
                   IF  (EMPFLEXREM-FOUND)
                   AND (EFR-COMPANY          = DB-COMPANY)
                   AND (EFR-EMPLOYEE         = DB-EMPLOYEE)
                   AND (EFR-START-DATE       > DB-START-DATE)
      **************** Future dated flex periods exist
                       INITIALIZE BS12F1-DETAIL-GROUP
                       MOVE 120                    TO CRT-ERROR-NBR
                       MOVE BS12F1-BAE-NEW-DATE-FN TO CRT-FIELD-NBR
                       GO TO 610-END.

           ADD 1                       TO I1.
P63451     IF  (PLN-COVERAGE-TYPE      = "0")
P63451     AND (PLN-CONTRIB-TYPE       = "X")
P63451         MOVE 121                TO CRT-ERROR-NBR
P63451         GO TO 610-NEXT-BNPLNENR.

       610-NEXT-BNPLNENR.
           IF (BS12F1-BAE-RULE-TYPE    = "F")
018800         PERFORM 860-FIND-NXTRNG-BFSSET2
           ELSE
           IF (BS12F1-BAE-RULE-TYPE    = "N")
018800         PERFORM 860-FIND-NXTRNG-BNNSET2
           ELSE
018800         PERFORM 860-FIND-NXTRNG-BPESET2.
018900
019000 610-END.
019100
019200******************************************************************
       612-COMP-ADD-DATE.
019200******************************************************************

           MOVE BS12F1-BAE-NEW-DATE    TO BS12WS-DATE.

           IF (BFS-ADD-DATE            = "FS")
               MOVE BS12WS-DATE        TO BS12F1-BFS-ADD-DATE (I1)
           ELSE
           IF (BFS-ADD-DATE            = "CM")
               PERFORM 618-CALC-1ST-OF-MNTH
               THRU    618-END
           ELSE
           IF (BFS-ADD-DATE            = "NM")
               PERFORM 620-CALC-1ST-OF-NXT-MNTH
               THRU    620-END
           ELSE
           IF (BFS-ADD-DATE            = "CP")
               PERFORM 622-CALC-CURR-PAY-PER
               THRU    622-END
           ELSE
           IF (BFS-ADD-DATE            = "NP")
               PERFORM 624-CALC-NEXT-PAY-PER
               THRU    624-END
           ELSE
           IF (BFS-ADD-DATE            = "CW")
               PERFORM 626-CALC-CURR-WORK-PER
               THRU    626-END
           ELSE
           IF (BFS-ADD-DATE            = "NW")
               PERFORM 628-CALC-NEXT-WORK-PER
               THRU    628-END
J47712     ELSE 
J47712     IF (BFS-ADD-DATE            = "CD")
J47712         PERFORM 629-CALC-FOR-CURRENT-DATE
J47712         THRU    629-END.

           MOVE BS12WS-DATE            TO BS12F1-BFS-ADD-DATE (I1).

       612-END.

019200******************************************************************
       614-COMP-CHANGE-DATE.
019200******************************************************************

           MOVE BS12F1-BAE-NEW-DATE    TO BS12WS-DATE.

           IF (BFS-CHANGE-DATE         = "FS")
               MOVE BS12WS-DATE        TO BS12F1-BFS-CHANGE-DATE (I1)
           ELSE
           IF (BFS-CHANGE-DATE         = "CM")
               PERFORM 618-CALC-1ST-OF-MNTH
               THRU    618-END
           ELSE
           IF (BFS-CHANGE-DATE         = "NM")
               PERFORM 620-CALC-1ST-OF-NXT-MNTH
               THRU    620-END
           ELSE
           IF (BFS-CHANGE-DATE         = "CP")
               PERFORM 622-CALC-CURR-PAY-PER
               THRU    622-END
           ELSE
           IF (BFS-CHANGE-DATE         = "NP")
               PERFORM 624-CALC-NEXT-PAY-PER
               THRU    624-END
           ELSE
           IF (BFS-CHANGE-DATE         = "CW")
               PERFORM 626-CALC-CURR-WORK-PER
               THRU    626-END
           ELSE
           IF (BFS-CHANGE-DATE         = "NW")
               PERFORM 628-CALC-NEXT-WORK-PER
               THRU    628-END
J47712     ELSE 
J47712     IF (BFS-CHANGE-DATE         = "CD")
J47712         PERFORM 629-CALC-FOR-CURRENT-DATE
J47712         THRU    629-END.

           MOVE BS12WS-DATE            TO BS12F1-BFS-CHANGE-DATE (I1).

       614-END.

019200******************************************************************
       616-COMP-STOPPED-DATE.
019200******************************************************************

           MOVE BS12F1-BAE-NEW-DATE    TO BS12WS-DATE.

           IF (BFS-STOPPED-DATE        = "FS")
               MOVE BS12WS-DATE        TO BS12F1-BFS-STOPPED-DATE (I1)
           ELSE
           IF (BFS-STOPPED-DATE        = "CM")
               PERFORM 618-CALC-1ST-OF-MNTH
               THRU    618-END
           ELSE
           IF (BFS-STOPPED-DATE        = "NM")
               PERFORM 620-CALC-1ST-OF-NXT-MNTH
               THRU    620-END
           ELSE
           IF (BFS-STOPPED-DATE        = "CP")
               PERFORM 622-CALC-CURR-PAY-PER
               THRU    622-END
           ELSE
           IF (BFS-STOPPED-DATE        = "NP")
               PERFORM 624-CALC-NEXT-PAY-PER
               THRU    624-END
           ELSE
           IF (BFS-STOPPED-DATE        = "CW")
               PERFORM 626-CALC-CURR-WORK-PER
               THRU    626-END
           ELSE
           IF (BFS-STOPPED-DATE        = "NW")
               PERFORM 628-CALC-NEXT-WORK-PER
               THRU    628-END
J47712     ELSE 
J47712     IF (BFS-STOPPED-DATE        = "CD")
J47712         PERFORM 629-CALC-FOR-CURRENT-DATE
J47712         THRU    629-END.

           MOVE BS12WS-DATE            TO BS12F1-BFS-STOPPED-DATE (I1).

       616-END.

011700******************************************************************
       618-CALC-1ST-OF-MNTH.
011700******************************************************************

           MOVE 1                      TO BS12WS-DAY.

       618-END.

011700******************************************************************
       620-CALC-1ST-OF-NXT-MNTH.
011700******************************************************************

           MOVE 1                      TO BS12WS-DAY.

           ADD 1                       TO BS12WS-MONTH.

           IF (BS12WS-MONTH            > 12)
               MOVE 1                  TO BS12WS-MONTH
               ADD 1                   TO BS12WS-YEAR.

       620-END.

019200******************************************************************
       622-CALC-CURR-PAY-PER.
019200******************************************************************

           PERFORM 630-EDIT-PAY-PLAN
           THRU    630-END.
           IF (ERROR-FOUND)
               GO TO 622-END.

           MOVE BS12F1-PLN-COMPANY     TO DB-COMPANY.
           MOVE EMP-OT-PLAN-CODE       TO DB-PLAN-CODE.
           MOVE PRO-EFFECT-DATE        TO DB-EFFECT-DATE.
           MOVE PRYSET1-EFFECT-DATE    TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PRYSET1.

           PERFORM
               UNTIL (PROTPAYPRD-NOTFOUND)
               OR    (PRY-PAY-END-DATE >= BS12F1-BAE-NEW-DATE)

               PERFORM 860-FIND-NXTRNG-PRYSET1
           END-PERFORM.

           IF (PROTPAYPRD-NOTFOUND)
      ******** Pay period do not exist for employee
000500         MOVE 107                TO CRT-ERROR-NBR
               MOVE EMP-EMPLOYEE       TO BS12WS-BWZ-EMP-NBR
000490         MOVE BS12WS-BWZ-EMP-NBR TO CRT-ERR-VAR1
000510         GO TO 622-END.

           MOVE PRY-PAY-START-DATE     TO BS12WS-DATE.

       622-END.

019200******************************************************************
       624-CALC-NEXT-PAY-PER.
019200******************************************************************

           PERFORM 630-EDIT-PAY-PLAN
           THRU    630-END.
           IF (ERROR-FOUND)
               GO TO 624-END.

           MOVE BS12F1-PLN-COMPANY     TO DB-COMPANY.
           MOVE EMP-OT-PLAN-CODE       TO DB-PLAN-CODE.
           MOVE PRO-EFFECT-DATE        TO DB-EFFECT-DATE.
           MOVE PRYSET1-EFFECT-DATE    TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PRYSET1.

           PERFORM
               UNTIL (PROTPAYPRD-NOTFOUND)
               OR    (PRY-PAY-START-DATE > BS12F1-BAE-NEW-DATE)

               PERFORM 860-FIND-NXTRNG-PRYSET1
           END-PERFORM.

           IF (PROTPAYPRD-NOTFOUND)
      ******** Pay period do not exist for employee
000500         MOVE 107                TO CRT-ERROR-NBR
               MOVE EMP-EMPLOYEE       TO BS12WS-BWZ-EMP-NBR
000490         MOVE BS12WS-BWZ-EMP-NBR TO CRT-ERR-VAR1
000510         GO TO 624-END.

           MOVE PRY-PAY-START-DATE     TO BS12WS-DATE.

       624-END.

019200******************************************************************
       626-CALC-CURR-WORK-PER.
019200******************************************************************

           PERFORM 630-EDIT-PAY-PLAN
           THRU    630-END.
           IF (ERROR-FOUND)
               GO TO 626-END.

           MOVE BS12F1-PLN-COMPANY     TO DB-COMPANY.
           MOVE EMP-OT-PLAN-CODE       TO DB-PLAN-CODE.
           MOVE PRO-EFFECT-DATE        TO DB-EFFECT-DATE.
           MOVE PRWSET1-EFFECT-DATE    TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PRWSET1.

           PERFORM
               UNTIL (PROTWRKPRD-NOTFOUND)
               OR    (PRW-WRK-END-DATE >= BS12F1-BAE-NEW-DATE)

               PERFORM 860-FIND-NXTRNG-PRWSET1
           END-PERFORM.

           IF (PROTWRKPRD-NOTFOUND)
      ******** Work period do not exist for employee
000500         MOVE 108                TO CRT-ERROR-NBR
               MOVE EMP-EMPLOYEE       TO BS12WS-BWZ-EMP-NBR
000490         MOVE BS12WS-BWZ-EMP-NBR TO CRT-ERR-VAR1
000510         GO TO 626-END.

           MOVE PRW-WRK-START-DATE     TO BS12WS-DATE.

       626-END.

019200******************************************************************
       628-CALC-NEXT-WORK-PER.
019200******************************************************************

           PERFORM 630-EDIT-PAY-PLAN
           THRU    630-END.
           IF (ERROR-FOUND)
               GO TO 628-END.

           MOVE BS12F1-PLN-COMPANY     TO DB-COMPANY.
           MOVE EMP-OT-PLAN-CODE       TO DB-PLAN-CODE.
           MOVE PRO-EFFECT-DATE        TO DB-EFFECT-DATE.
           MOVE PRWSET1-EFFECT-DATE    TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PRWSET1.

           PERFORM
               UNTIL (PROTWRKPRD-NOTFOUND)
               OR    (PRW-WRK-START-DATE > BS12F1-BAE-NEW-DATE)

               PERFORM 860-FIND-NXTRNG-PRWSET1
           END-PERFORM.

           IF (PROTWRKPRD-NOTFOUND)
      ******** Work period do not exist for employee
000500         MOVE 108                TO CRT-ERROR-NBR
               MOVE EMP-EMPLOYEE       TO BS12WS-BWZ-EMP-NBR
000490         MOVE BS12WS-BWZ-EMP-NBR TO CRT-ERR-VAR1
000510         GO TO 628-END.

           MOVE PRW-WRK-START-DATE     TO BS12WS-DATE.

       628-END.
              
J47712******************************************************************
J47712 629-CALC-FOR-CURRENT-DATE.
J47712******************************************************************       

J47712     MOVE BNEDWS-ELIGIBILITY-DATE TO BS12WS-DATE.   
           
J47712*    Use Current Date rather than eligiblity date for Effective Dt
J47712     IF (WS-SYSTEM-DATE-YMD >= BNEDWS-ELIGIBILITY-DATE)
J47712         MOVE WS-SYSTEM-DATE-YMD  TO BS12WS-DATE
J47712     END-IF.

J47712 629-END.

000410******************************************************************
       630-EDIT-PAY-PLAN.
000410******************************************************************

           IF (EMP-OT-PLAN-CODE            = SPACES)
               MOVE BS12F1-PLN-COMPANY     TO DB-COMPANY 
009821         MOVE EMP-PROCESS-LEVEL      TO DB-PROCESS-LEVEL
009822         PERFORM 840-FIND-PRSSET1
               IF (PRS-OT-PLAN-CODE (EMP-PAY-FREQUENCY) NOT = SPACES)
009823             MOVE PRS-OT-PLAN-CODE (EMP-PAY-FREQUENCY)
009824                                     TO DB-PLAN-CODE
009825         ELSE                             
009821             INITIALIZE DB-PROCESS-LEVEL
009822             PERFORM 840-FIND-PRSSET1
                   IF (PRS-OT-PLAN-CODE (EMP-PAY-FREQUENCY) = SPACES)
      **************** Pay plan does not exist for employee \
000500                 MOVE 109            TO CRT-ERROR-NBR
                       MOVE EMP-EMPLOYEE   TO BS12WS-BWZ-EMP-NBR
000490                 MOVE BS12WS-BWZ-EMP-NBR TO CRT-ERR-VAR1
000510                 GO TO 630-END
000510             ELSE
                       MOVE PRS-OT-PLAN-CODE (EMP-PAY-FREQUENCY) 
                                           TO DB-PLAN-CODE
                   END-IF
               END-IF
           ELSE                                      
               MOVE EMP-OT-PLAN-CODE       TO DB-PLAN-CODE.

           MOVE BS12F1-PLN-COMPANY         TO DB-COMPANY.
           MOVE PROSET2-PLAN-CODE          TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PROSET2.
           PERFORM
               UNTIL (PROVERTIME-NOTFOUND)
               OR    (PRO-EFFECT-DATE      <= BS12F1-BAE-NEW-DATE)

               PERFORM 860-FIND-NXTRNG-PROSET2
           END-PERFORM.

       630-END.

J14959******************************************************************
J14959 670-DO-CONTRIBUTION.
J14959******************************************************************

           MOVE BS12F1-PLN-COMPANY           TO BNCTWS-COMPANY.
           MOVE PLN-PLAN-TYPE                TO BNCTWS-PLAN-TYPE.
           MOVE PLN-PLAN-CODE                TO BNCTWS-PLAN-CODE.
           MOVE "E"                          TO BNCTWS-COVER-TYPE.
           MOVE BS12F1-EMP-EMPLOYEE          TO BNCTWS-EMPLOYEE.
           MOVE BS12F1-BAE-NEW-DATE          TO BNCTWS-AS-OF-DATE
                                                BNCTWS-START-DATE.
      *    IF (PLN-CONTRIB-TYPE              = "4")
      *        MOVE BNCVWS-COVER-AMT         TO BNCTWS-COVER-AMT
      *    ELSE
               INITIALIZE BNCTWS-COVER-AMT.
           INITIALIZE BNCTWS-IN-SMOKER-SW
                      BNCTWS-IN-EMP-PT-CONT
                      BNCTWS-IN-EMP-AT-CONT
                      BNCTWS-CONTRIB-OPT
                      BNCTWS-IN-PAY-PER-AMT
                      BNCTWS-YTD-CONT
                      BNCTWS-ANNUAL-SALARY
                      BNCTWS-IN-ANNUAL-AMT
                      BNCTWS-IN-PCT-AMT-FLAG
                      BNCTWS-IN-PRE-AFT-FLAG
                      BNCTWS-CYC-REMAIN
                      BNCTWS-STOP-DATE
                      BNCTWS-FC
                      BNCTWS-PREM-UPD-DT
                      BNCTWS-PREM-GROUP
                      BNCTWS-NBR-OF-HOURS.

      **** THE FOLLOWING ROUTINE IS CALLED TO SET PRE-AFT-FLAG IF ****
      **** DEFAULT IS NOT SET AND BOTH TAX TYPES ARE ALLOWED      ****

019000     MOVE BNCTWS-COMPANY               TO BNREWS-COMPANY.
019100     MOVE BNCTWS-PLAN-TYPE             TO BNREWS-PLAN-TYPE.
019200     MOVE BNCTWS-PLAN-CODE             TO BNREWS-PLAN-CODE.
019300     MOVE BNCTWS-COVER-TYPE            TO BNREWS-COVER-TYPE.
019400     MOVE BNCTWS-EMPLOYEE              TO BNREWS-EMPLOYEE.
019500     MOVE BNCTWS-AS-OF-DATE            TO BNREWS-AS-OF-DATE.
019600     MOVE "PRE"                        TO BNREWS-FILE-PREFIX.
019700     PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70.
019800     IF (ERROR-FOUND)
019900         GO TO 670-END.
020000
           IF  (PRE-CONT-TAX-STS             = "B")
           AND (PRE-DEFAUL-TAX-TYP           = SPACES)
               MOVE "P"                      TO BNCTWS-IN-PRE-AFT-FLAG.

           PERFORM 5000-DO-CONTRIBUTION-70.
           IF (ERROR-FOUND)
               GO TO 670-END.

           IF (PLN-CONTRIB-TYPE              NOT = "4")
               COMPUTE BNCTWS-EMP-CONT       = BNCTWS-EMP-CONT
                                             + BNCTWS-FLEX-CONT
           ELSE
               MOVE RTD-EMP-RATE             TO BNCTWS-EMP-CONT
               IF  (BNCTWS-EMP-CONT          NOT = ZEROES)
053900             IF (RTH-PCT-RATE-FLG      = "A")
055300                 INITIALIZE BNMRWS-ROUND-ONLY-SW
055400                 MOVE BNCTWS-EMP-CONT  TO BNMRWS-AMOUNT
055500                 INITIALIZE BNMRWS-BEF-AFT-FLAG
055600                 MOVE "R"              TO BNMRWS-ROUND-METH
055700                 MOVE .0001            TO BNMRWS-ROUND-TO
055800                 MOVE BNCTWS-UNITS     TO BNMRWS-MULT-OF-SALARY
055900                 INITIALIZE BNMRWS-SALARY-PCT
056000                 PERFORM 5000-DO-MULTIPLY-AND-ROUND-70
056100                 MOVE BNMRWS-AMT-ROUNDED TO BNCTWS-EMP-CONT
                   ELSE
052200                 COMPUTE BNCTWS-EMP-CONT ROUNDED
052300                                       = BNCTWS-CONT-BASIS-AMT
052400                                       * BNCTWS-EMP-CONT
052500                                       / 100
058100                 MOVE "Y"              TO BNMRWS-ROUND-ONLY-SW
058200                 MOVE BNCTWS-EMP-CONT TO BNMRWS-AMOUNT
058300                 INITIALIZE BNMRWS-BEF-AFT-FLAG
058400                 MOVE "R"              TO BNMRWS-ROUND-METH
058500                 MOVE .0001            TO BNMRWS-ROUND-TO
058600                 INITIALIZE BNMRWS-MULT-OF-SALARY
058700                            BNMRWS-SALARY-PCT
058800                 PERFORM 5000-DO-MULTIPLY-AND-ROUND-70
058900                 MOVE BNMRWS-AMT-ROUNDED TO BNCTWS-EMP-CONT
                   END-IF
               END-IF
055200         IF (BNCTWS-FLEX-CONT          NOT = ZEROES)
053900             IF (RTH-PCT-RATE-FLG      = "A")
055300                 INITIALIZE BNMRWS-ROUND-ONLY-SW
055400                 MOVE BNCTWS-FLEX-CONT TO BNMRWS-AMOUNT
055500                 INITIALIZE BNMRWS-BEF-AFT-FLAG
055600                 MOVE "R"              TO BNMRWS-ROUND-METH
055700                 MOVE .0001            TO BNMRWS-ROUND-TO
055800                 MOVE BNCTWS-UNITS     TO BNMRWS-MULT-OF-SALARY
055900                 INITIALIZE BNMRWS-SALARY-PCT
056000                 PERFORM 5000-DO-MULTIPLY-AND-ROUND-70
056100                 MOVE BNMRWS-AMT-ROUNDED TO BNCTWS-FLEX-CONT
                   ELSE
052200                 COMPUTE BNCTWS-FLEX-CONT ROUNDED
052300                                       = BNCTWS-CONT-BASIS-AMT
052400                                       * BNCTWS-FLEX-CONT
052500                                       / 100
058100                 MOVE "Y"              TO BNMRWS-ROUND-ONLY-SW
058200                 MOVE BNCTWS-FLEX-CONT TO BNMRWS-AMOUNT
058300                 INITIALIZE BNMRWS-BEF-AFT-FLAG
058400                 MOVE "R"              TO BNMRWS-ROUND-METH
058500                 MOVE .0001            TO BNMRWS-ROUND-TO
058600                 INITIALIZE BNMRWS-MULT-OF-SALARY
058700                            BNMRWS-SALARY-PCT
058800                 PERFORM 5000-DO-MULTIPLY-AND-ROUND-70
058900                 MOVE BNMRWS-AMT-ROUNDED TO BNCTWS-FLEX-CONT.

P60050     IF  (CVR-CALC-TYPE = "M" OR "N")
           AND (CVR-BEF-AFT-FLAG = "A")
           AND (PRE-CONTRIB-BASIS   = "C")
           AND  ((PRE-CONTRIB-TYPE = 4)
           OR   ((PRE-CONTRIB-TYPE = 3)
           AND   (PRE-CALC-TYPE    = 4)))
               MOVE RTD-EMP-RATE       TO BNCTWS-EMP-CONT
               MOVE RTD-FLEX-RATE      TO BNCTWS-FLEX-CONT
               MOVE RTD-COMP-RATE      TO BNCTWS-COMP-CONT.

J14959 670-END.

019200******************************************************************
019300 BS12S1-TRANSACTION-END.
019400******************************************************************
019500
