******* BN47PD 2.1.15 <2740661848>
      ******************************************************************
      *               M O D I F I C A T I O N   L O G:                 *
      ******************************************************************
      *  Modified by Analysts International,MN                         *
      ******************************************************************
      *  AI0030  03/24/03  MODIFY TO ALL PLAN TYPE OF HL AND DL.       *
      *  SEN999  04/04/03  Fix std lawson bug                          *
      *  CR0305  07/24/03  ADD LOGIC FOR RETIREE BENEFICIARY           *
      *          11/01/10  M. HUNTER - ACS REAPPLIED ABOVE MODS AFTER  *
      *                                9.0 APPS UPGRADE. I LEFT OUT THE*
      *                                SEN999 MOD SINCE IT APPEARS TO  *
      *                                HAVE BEEN RESOLVED IN 9.0 APPS  *
      *          08/25/11  M. HUNTER - ACS REAPPLIED ABOVE MODS AFTER  *
      *                                9.0.1 APPS UPGRADE.             *
      ******************************************************************

000100******************************************************************
000200 BN47S1-TRANSACTION              SECTION 53.
000300******************************************************************
000400 BN47S1-START.
000500
P59785     INITIALIZE BN47F1-WEB-UPDATE-FL.
P59785
000600     PERFORM 200-EDIT-TRAN
000700     THRU    200-END.
000800
000900     IF (NO-ERROR-FOUND)
001000         PERFORM 400-PROCESS-TRAN
001100         THRU    400-END.
001200
001300     GO TO BN47S1-TRANSACTION-END.
001400******************************************************************
001500 200-EDIT-TRAN.
001600******************************************************************
001700     PERFORM 210-EDIT-ACCESS
001800     THRU    210-END.
001900
002000     IF (ERROR-FOUND)
002100         GO TO 200-END.
002200
002300     IF (BN47F1-FC = "A" OR "C")
002400         PERFORM 230-EDIT-DATA
002500         THRU    230-END
002600         GO TO 200-END.
002700
002800 200-END.
002900******************************************************************
003000 210-EDIT-ACCESS.
003100******************************************************************
003200     MOVE BN47F1-BEN-COMPANY          TO DB-COMPANY.
003300     MOVE BN47F1-BEN-EMPLOYEE         TO DB-EMPLOYEE.
003400     MOVE BN47F1-BEN-PLAN-TYPE        TO DB-PLAN-TYPE.
003500     MOVE BN47F1-BEN-PLAN-CODE        TO DB-PLAN-CODE.
003600     MOVE 1                           TO DB-PRIM-CNTGNT.
003700     MOVE SPACES                      TO DB-PROCESS-LEVEL
003800                                          BN47WS-PCT-AMT-FLG.
003900     MOVE ZEROS                       TO DB-START-DATE
004000                                         DB-SEQ-NBR
004100                                         BN47WS-PMT-AMT-P
004200                                         BN47WS-PMT-AMT-C.
004300
004400     PERFORM 840-FIND-BNCSET1.
004500     IF (BNCOMPANY-NOTFOUND)
004600         MOVE 52                              TO CRT-ERROR-NBR
004700         MOVE BN47F1-BEN-COMPANY-FN           TO CRT-FIELD-NBR
004800         GO TO 210-END.
004900
005000     PERFORM 840-FIND-PRSSET1.
005100     IF (PRSYSTEM-NOTFOUND)
005200         MOVE 53                              TO CRT-ERROR-NBR
005300         MOVE BN47F1-BEN-COMPANY-FN           TO CRT-FIELD-NBR
005400         GO TO 210-END.
005500
005600     IF  (BN47F1-FC = "P")
005700         IF  (BN47F1-BEN-EMPLOYEE  = ZEROS)
005800         AND (BN47F1-BEN-PLAN-TYPE = SPACES)
005900         AND (BN47F1-BEN-PLAN-CODE = SPACES)
006000             MOVE 11                          TO CRT-ERROR-NBR
006100             MOVE BN47F1-FC-FN                TO CRT-FIELD-NBR
006200             PERFORM
006300                 VARYING I1 FROM 1 BY 1
006400                 UNTIL  (I1 > 3)
006500                     INITIALIZE BN47F1-DETAIL-LINE (I1)
006600             END-PERFORM
006700             GO TO 210-END.
006800
006900     PERFORM 850-FIND-NLT-BENSET4.
007000
008100     IF (BN47F1-FC = "N")
008200         PERFORM 860-FIND-NEXT-BENSET4
008300             UNTIL (BENEFIT-NOTFOUND)
008400             OR   (BEN-COMPANY   NOT = DB-COMPANY)
008500             OR  ((BEN-EMPLOYEE  NOT = DB-EMPLOYEE)
008600             AND  (BEN-PLAN-TYPE = "EL" OR "DI" OR "DC" OR "DB" OR
AI0030*                                  "DL" OR "RS" OR "SB" OR "SP"))
AI0030                           "HL" OR "DL" OR "RS" OR "SB" OR "SP"))
008700             OR  ((BEN-PLAN-TYPE NOT = DB-PLAN-TYPE)
008800             AND  (BEN-PLAN-TYPE = "EL" OR "DI" OR "DC" OR "DB" OR
AI0030*                                  "DL" OR "RS" OR "SB" OR "SP"))
AI0030                           "HL" OR "DL" OR "RS" OR "SB" OR "SP"))
008900             OR  ((BEN-PLAN-CODE NOT = DB-PLAN-CODE)
009000             AND  (BEN-PLAN-TYPE = "EL" OR "DI" OR "DC" OR "DB" OR
AI0030*                                  "DL" OR "RS" OR "SB" OR "SP")).
AI0030                           "HL" OR "DL" OR "RS" OR "SB" OR "SP")).
009100
009200     IF (BN47F1-FC = "P")
009300         PERFORM 870-FIND-PREV-BENSET4
009400             UNTIL (BENEFIT-NOTFOUND)
009500             OR   (BEN-COMPANY   NOT = DB-COMPANY)
009600             OR  ((BEN-EMPLOYEE  NOT = DB-EMPLOYEE)
009700             AND  (BEN-PLAN-TYPE = "EL" OR "DI" OR "DC" OR "DB" OR
AI0030*                                  "DL" OR "RS" OR "SB" OR "SP"))
AI0030                           "HL" OR "DL" OR "RS" OR "SB" OR "SP"))
009800             OR  ((BEN-PLAN-TYPE NOT = DB-PLAN-TYPE)
009900             AND  (BEN-PLAN-TYPE = "EL" OR "DI" OR "DC" OR "DB" OR
AI0030*                                  "DL" OR "RS" OR "SB" OR "SP"))
AI0030                           "HL" OR "DL" OR "RS" OR "SB" OR "SP"))
010000             OR  ((BEN-PLAN-CODE NOT = DB-PLAN-CODE)
010100             AND  (BEN-PLAN-TYPE = "EL" OR "DI" OR "DC" OR "DB" OR
AI0030*                                  "DL" OR "RS" OR "SB" OR "SP")).
AI0030                           "HL" OR "DL" OR "RS" OR "SB" OR "SP")).
010200
010300     IF (BN47F1-FC = "N" OR "P")
010400         IF (BENEFIT-NOTFOUND)
010500         OR (BEN-COMPANY  NOT = BN47F1-BEN-COMPANY)
010600             MOVE 11                          TO CRT-ERROR-NBR
010700             MOVE BN47F1-FC-FN                TO CRT-FIELD-NBR
010800             PERFORM
010900                 VARYING I1 FROM 1 BY 1
011000                 UNTIL  (I1 > 3)
011100                     INITIALIZE BN47F1-DETAIL-LINE (I1)
011200             END-PERFORM
011300             GO TO 210-END
011400         ELSE
011500             MOVE BEN-EMPLOYEE        TO DB-EMPLOYEE
011600                                         BN47F1-BEN-EMPLOYEE
011700             MOVE BEN-PLAN-TYPE       TO DB-PLAN-TYPE
011800                                         BN47F1-BEN-PLAN-TYPE
011900             MOVE BEN-PLAN-CODE       TO DB-PLAN-CODE
012000                                         BN47F1-BEN-PLAN-CODE.
012100
012200     PERFORM 840-FIND-PEMSET1.
012300     IF (PAEMPLOYEE-NOTFOUND)
012400         MOVE 102                             TO CRT-ERROR-NBR
012500         MOVE BN47F1-BEN-EMPLOYEE-FN          TO CRT-FIELD-NBR
012600         GO TO 210-END.
012700
012800     PERFORM 840-FIND-EMPSET1.
012900     IF (EMPLOYEE-NOTFOUND)
013000         MOVE 102                             TO CRT-ERROR-NBR
013100         MOVE BN47F1-BEN-EMPLOYEE-FN          TO CRT-FIELD-NBR
013200         GO TO 210-END.
013300
007100     IF (BN47F1-FC NOT = "N" AND "P")
007200         IF (BENEFIT-NOTFOUND)
007300         OR (BEN-COMPANY   NOT = DB-COMPANY)
007400         OR (BEN-EMPLOYEE  NOT = DB-EMPLOYEE)
007500         OR (BEN-PLAN-TYPE NOT = DB-PLAN-TYPE)
007600         OR (BEN-PLAN-CODE NOT = DB-PLAN-CODE)
CR0305             SET BENEFIT-NOTFOUND             TO TRUE  
CR0305             MOVE ZERO                        TO DB-PARTICIPNT
CR0305             MOVE 99999999                    TO DB-START-DATE
CR0305             PERFORM 850-FIND-NLT-PTBSET4
CR0305             IF (PARTBEN-NOTFOUND)
CR0305             OR (PTB-COMPANY    NOT = DB-COMPANY)
CR0305             OR (PTB-EMPLOYEE   NOT = DB-EMPLOYEE)
CR0305             OR (PTB-PLAN-TYPE  NOT = DB-PLAN-TYPE)
CR0305             OR (PTB-PLAN-CODE  NOT = DB-PLAN-CODE)
CR0305             OR (PTB-PARTICIPNT NOT = ZERO)
007700             MOVE 106                         TO CRT-ERROR-NBR
007800             MOVE BN47F1-BEN-PLAN-CODE-FN     TO CRT-FIELD-NBR
007900             GO TO 210-END.
008000
013400     IF  (BN47F1-FC NOT = "N" AND "P")
013500         MOVE EMP-COMPANY        TO CRT-COMPANY
013600         MOVE EMP-PROCESS-LEVEL  TO CRT-PROCESS-LEVEL
013700         PERFORM 700-HR-EMP-SECURITY
013800         IF (HRWS-EMP-SECURED)
013900             MOVE 61                              TO CRT-ERROR-NBR
014000             MOVE BN47F1-BEN-EMPLOYEE-FN          TO CRT-FIELD-NBR
014100             GO TO 210-END.
014200
014300     IF (BN47F1-FC = "N" OR "P")
014400         MOVE EMP-COMPANY        TO CRT-COMPANY
014500         MOVE EMP-PROCESS-LEVEL  TO CRT-PROCESS-LEVEL
014600         PERFORM 700-HR-EMP-SECURITY
014700         IF (HRWS-EMP-SECURED)
014800             PERFORM 212-GET-NEXT-EMPLOYEE
014900             THRU    212-END
015000                 UNTIL (BENEFIT-NOTFOUND)
015100                 OR    (BEN-COMPANY NOT = DB-COMPANY)
015200                 OR    (HRWS-EMP-NOT-SECURED).
015300
015400     IF   (BN47F1-FC   = "N" OR "P")
015500     AND ((BENEFIT-NOTFOUND)
015600     OR   (BEN-COMPANY NOT = DB-COMPANY))
015700         MOVE 11                              TO CRT-ERROR-NBR
015800         MOVE BN47F1-FC-FN                    TO CRT-FIELD-NBR
015900         PERFORM
016000             VARYING I1 FROM 1 BY 1
016100             UNTIL  (I1 > 3)
016200                 INITIALIZE BN47F1-DETAIL-LINE (I1)
016300         END-PERFORM
016400         GO TO 210-END.
016500
016600     PERFORM 840-FIND-PLNSET1.
016700     IF (PLAN-NOTFOUND)
016800         MOVE 103                             TO CRT-ERROR-NBR
016900         MOVE BN47F1-BEN-PLAN-CODE-FN         TO CRT-FIELD-NBR
017000         GO TO 210-END.
017100
017200 210-END.
017300******************************************************************
017400 212-GET-NEXT-EMPLOYEE.
017500******************************************************************
017600     IF (BN47F1-FC = "N")
017700         PERFORM 860-FIND-NEXT-BENSET4
017800            UNTIL (BENEFIT-NOTFOUND)
017900            OR    (BEN-COMPANY   NOT = DB-COMPANY)
018000            OR   ((BEN-EMPLOYEE  NOT = DB-EMPLOYEE)
018100            AND   (BEN-PLAN-TYPE = "EL" OR "DI" OR "DC" OR "DB" OR
AI0030*                                  "DL" OR "RS" OR "SB" OR "SP"))
AI0030                           "HL" OR "DL" OR "RS" OR "SB" OR "SP"))
018200     ELSE
018300         PERFORM 870-FIND-PREV-BENSET4
018400            UNTIL (BENEFIT-NOTFOUND)
018500            OR    (BEN-COMPANY   NOT = DB-COMPANY)
018600            OR   ((BEN-EMPLOYEE  NOT = DB-EMPLOYEE)
018700            AND   (BEN-PLAN-TYPE = "EL" OR "DI" OR "DC" OR "DB" OR
AI0030*                                  "DL" OR "RS" OR "SB" OR "SP")).
AI0030                           "HL" OR "DL" OR "RS" OR "SB" OR "SP")).
018800
018900     IF  (BENEFIT-FOUND)
019000     AND (BEN-COMPANY = DB-COMPANY)
019100         MOVE BEN-EMPLOYEE       TO DB-EMPLOYEE
019200                                    BN47F1-BEN-EMPLOYEE
019300         MOVE BEN-PLAN-TYPE      TO DB-PLAN-TYPE
019400         MOVE BEN-START-DATE     TO DB-START-DATE
019500         MOVE BEN-PLAN-CODE      TO DB-PLAN-CODE
019600         PERFORM 840-FIND-EMPSET1
019700         MOVE EMP-COMPANY        TO CRT-COMPANY
019800         MOVE EMP-PROCESS-LEVEL  TO CRT-PROCESS-LEVEL
019900         PERFORM 700-HR-EMP-SECURITY.
020000
020100 212-END.
020200******************************************************************
020300 230-EDIT-DATA.
020400******************************************************************
020500     PERFORM 850-FIND-NLT-BNFSET1.
020600     IF  (BENEFICRY-FOUND)
020700     AND (BNF-COMPANY   = DB-COMPANY)
020800     AND (BNF-EMPLOYEE  = DB-EMPLOYEE)
020900     AND (BNF-PLAN-TYPE = DB-PLAN-TYPE)
021000     AND (BNF-PLAN-CODE = DB-PLAN-CODE)
021100         IF (BNF-PCT-AMT-FLAG = "P")
021200             MOVE "P"            TO BN47WS-PCT-AMT-FLG
021300             PERFORM 235-ADD-PERCENT
021400             THRU    235-END
021500                 UNTIL (BENEFICRY-NOTFOUND)
021600                 OR    (BNF-COMPANY   NOT = DB-COMPANY)
021700                 OR    (BNF-EMPLOYEE  NOT = DB-EMPLOYEE)
021800                 OR    (BNF-PLAN-TYPE NOT = DB-PLAN-TYPE)
021900                 OR    (BNF-PLAN-CODE NOT = DB-PLAN-CODE)
022000         ELSE
022100             IF (BNF-PCT-AMT-FLAG = "A")
022200                 MOVE "A"    TO BN47WS-PCT-AMT-FLG.
022300
022400     PERFORM 260-EDIT-DTL-TRAN
022500     THRU    260-END
022600         VARYING I1 FROM 1 BY 1
022700         UNTIL  (I1 > 3)
022800         OR     (ERROR-FOUND).
022900
023000 230-END.
023100******************************************************************
023200 235-ADD-PERCENT.
023300******************************************************************
023400     IF (BNF-PRIM-CNTGNT = 2)
023500         ADD BNF-PMT-AMT        TO BN47WS-PMT-AMT-C
023600     ELSE
023700     IF (BNF-PRIM-CNTGNT = 1)
023800         ADD BNF-PMT-AMT        TO BN47WS-PMT-AMT-P.
023900
024000     PERFORM 860-FIND-NEXT-BNFSET1.
024100
024200 235-END.
024300******************************************************************
024400 260-EDIT-DTL-TRAN.
024500******************************************************************
024600     IF (BN47F1-LINE-FC (I1) = SPACES)
024700         GO TO 260-END.
024800
024900     IF  (BN47F1-BNF-PCT-AMT-FLAG (1) NOT = SPACES)
025000     AND (BN47WS-PCT-AMT-FLG           = SPACES)
025100         MOVE BN47F1-BNF-PCT-AMT-FLAG (1)  TO BN47WS-PCT-AMT-FLG.
025200
025300     IF (BN47F1-BNF-PCT-AMT-FLAG (I1) = SPACES)
025400         IF (BN47WS-PCT-AMT-FLG = "P")
025500             MOVE "P"                  TO
025600                                BN47F1-BNF-PCT-AMT-FLAG (I1)
025700         ELSE
025800         IF (BN47WS-PCT-AMT-FLG = "A")
025900             MOVE "A"                  TO
026000                                BN47F1-BNF-PCT-AMT-FLAG (I1).
           MOVE BN47F1-BEN-COMPANY        TO DB-COMPANY.
           MOVE BN47F1-BEN-EMPLOYEE       TO DB-EMPLOYEE.
           MOVE BN47F1-BEN-PLAN-TYPE      TO DB-PLAN-TYPE.
           MOVE BN47F1-BEN-PLAN-CODE      TO DB-PLAN-CODE.
           MOVE BN47F1-BNF-SEQ-NBR (I1)   TO DB-SEQ-NBR.
           PERFORM 840-FIND-BNFSET1.

027600     IF (BN47F1-LINE-FC (I1) = "D")
027700         PERFORM 262-EDIT-DTL-DELETE
027800         THRU    262-END.
027900
           PERFORM 600-MOVE-SCR-TO-WS
           THRU    600-END.
           PERFORM 2000-BNBNF-EDIT-TRAN.
           PERFORM 630-MOVE-WS-TO-SCR
           THRU    630-END.
027500
028300
028400 260-END.
028500******************************************************************
028600 262-EDIT-DTL-DELETE.
028700******************************************************************
028800     IF (BNF-PCT-AMT-FLAG = "P")
028900         IF (BN47F1-BNF-PRIM-CNTGNT (I1) = "2")
029000             SUBTRACT BN47F1-BNF-PMT-AMT (I1)
029100                                     FROM BN47WS-PMT-AMT-C
029200         ELSE
029300         IF (BN47F1-BNF-PRIM-CNTGNT (I1) = "1")
029400             SUBTRACT BN47F1-BNF-PMT-AMT (I1)
029500                                     FROM BN47WS-PMT-AMT-P.
029600
029700 262-END.
035900******************************************************************
036000 400-PROCESS-TRAN.
036100******************************************************************
036200     IF (BN47F1-FC = "A")
036300         PERFORM 410-ADD
036400         THRU    410-END
036500     ELSE
036600     IF (BN47F1-FC = "C")
036700         PERFORM 420-CHANGE
036800         THRU    420-END
036900     ELSE
037000     IF (BN47F1-FC = "I" OR "+" OR "-" OR "N" OR "P")
037100         PERFORM 480-INQUIRE
037200         THRU    480-END.
037300
037400 400-END.
037500******************************************************************
037600 410-ADD.
037700******************************************************************
037800     PERFORM 910-AUDIT-BEGIN.
037900     IF (DMS-ABORTED)
038000         GO TO 410-END.
038100
038300
038400     PERFORM 422-PROCESS-DETAIL
038500     THRU    422-END
038600         VARYING I1 FROM 1 BY 1
038700         UNTIL  (I1 > 3).
038800
039000
039100     MOVE CRT-ADD-COMPLETE                    TO CRT-MESSAGE.
039200     PERFORM 920-AUDIT-END.
039300
039400     PERFORM 481-MOVE-TO-SCREEN
039500     THRU    481-END.
039600
039700 410-END.
039800******************************************************************
039900 420-CHANGE.
040000******************************************************************
040100     PERFORM 910-AUDIT-BEGIN.
040200     IF (DMS-ABORTED)
040300         GO TO 420-END.
040400
040600
040700     PERFORM 422-PROCESS-DETAIL
040800     THRU    422-END
040900         VARYING I1 FROM 1 BY 1
041000         UNTIL  (I1 > 3).
041100
041300
041400     MOVE CRT-CHG-COMPLETE                    TO CRT-MESSAGE.
041500     PERFORM 920-AUDIT-END.
041600
041700     PERFORM 481-MOVE-TO-SCREEN
041800     THRU    481-END.
041900
042000 420-END.
042100******************************************************************
042200 422-PROCESS-DETAIL.
042300******************************************************************
042400     IF (BN47F1-LINE-FC (I1) = SPACES)
042500         GO TO 422-END.
042600
           PERFORM 600-MOVE-SCR-TO-WS
             THRU 600-END.
           PERFORM 3000-BNBNF-PROCESS-TRAN.
043800

033300     IF (BN47F1-LINE-FC (I1) = "D")
033500         INITIALIZE BN47F1-DETAIL-LINE (I1)
033600     ELSE
           IF (BN47F1-LINE-FC (I1) = "A")
               MOVE BNF-SEQ-NBR        TO BN47F1-BNF-SEQ-NBR (I1)
               INITIALIZE BN47F1-LINE-FC (I1)
           ELSE
               INITIALIZE BN47F1-LINE-FC (I1).
044500
044900
045000 422-END.
045100******************************************************************
045200 480-INQUIRE.
045300******************************************************************
045400     PERFORM 481-MOVE-TO-SCREEN
045500     THRU    481-END.
045600
045700     IF (BN47F1-FC            = "I")
045800     OR (BN47F1-BEN-COMPANY   NOT = BN47F1-PT-BEN-COMPANY)
045900     OR (BN47F1-BEN-EMPLOYEE  NOT = BN47F1-PT-BEN-EMPLOYEE)
046000     OR (BN47F1-BEN-PLAN-TYPE NOT = BN47F1-PT-BEN-PLAN-TYPE)
046100     OR (BN47F1-BEN-PLAN-CODE NOT = BN47F1-PT-BEN-PLAN-CODE)
046200         MOVE ZEROS                   TO DB-SEQ-NBR
046300     ELSE
046400     IF (BN47F1-FC = "+")
046500         MOVE BN47F1-PT-BNF-SEQ-NBR       TO DB-SEQ-NBR
046700         MOVE BN47F1-PT-BNF-PRIM-CNTGNT   TO DB-PRIM-CNTGNT
046800     ELSE
046900         MOVE BN47F1-BNF-SEQ-NBR (1)  TO DB-SEQ-NBR
047000         MOVE BN47F1-BNF-PRIM-CNTGNT (1)  TO DB-PRIM-CNTGNT.
047400
047500
047600
047700     IF (BN47F1-FC = "I")
               IF (BN47F1-WEB-UPDATE-FL NOT = SPACES)
                   MOVE BN47F1-WEB-SEQ-NBR     TO DB-SEQ-NBR
CR0305*            PERFORM 840-FIND-BNFSET1
CR0305             PERFORM 840-FIND-BNFSET1.
CR0305****     ELSE
CR0305****          MOVE "+"                   TO BN47F1-FC.
047900
           IF (BN47F1-WEB-UPDATE-FL = SPACES)
048000         PERFORM 850-FIND-NLT-BNFSET2
048100         IF (BN47F1-FC = "-")
048200             PERFORM 870-FIND-PREV-BNFSET2.
048300
048400     IF (BN47F1-FC = "-" OR "+" OR "I")
048500         IF  (BENEFICRY-FOUND)
048600         AND (BNF-COMPANY   = DB-COMPANY)
048700         AND (BNF-EMPLOYEE  = DB-EMPLOYEE)
048800         AND (BNF-PLAN-TYPE = DB-PLAN-TYPE)
048900         AND (BNF-PLAN-CODE = DB-PLAN-CODE)
049400                 NEXT SENTENCE
049500         ELSE
P08732             MOVE 11                          TO CRT-ERROR-NBR
P08732             MOVE BN47F1-FC-FN                TO CRT-FIELD-NBR
050200             GO TO 480-END.
050300
047700     IF (BN47F1-FC = "I")
047800         MOVE "+"                     TO BN47F1-FC.
047900
050400     IF (BN47F1-FC = "-")
050600         INITIALIZE BN47F1-DETAIL-GROUP
050700         PERFORM 482-MOVE-DTL-TO-SCREEN
050800         THRU    482-END
050900             VARYING I1 FROM 3 BY NEGATIVE-ONE
051000             UNTIL  (I1            < 1)
051100             OR     (BENEFICRY-NOTFOUND)
051200             OR     (BNF-COMPANY   NOT = DB-COMPANY)
051300             OR     (BNF-EMPLOYEE  NOT = DB-EMPLOYEE)
051400             OR     (BNF-PLAN-TYPE NOT = DB-PLAN-TYPE)
051500             OR     (BNF-PLAN-CODE NOT = DB-PLAN-CODE)
051600
051700         IF (BENEFICRY-NOTFOUND)
051800         OR (BNF-COMPANY   NOT = DB-COMPANY)
051900         OR (BNF-EMPLOYEE  NOT = DB-EMPLOYEE)
052000         OR (BNF-PLAN-TYPE NOT = DB-PLAN-TYPE)
052100         OR (BNF-PLAN-CODE NOT = DB-PLAN-CODE)
052200             MOVE CRT-INQ-COMPLETE            TO CRT-MESSAGE
052300         ELSE
052400             MOVE CRT-MORE-RECS               TO CRT-MESSAGE
052500     ELSE
052600         INITIALIZE BN47F1-DETAIL-GROUP
               IF (BN47F1-WEB-UPDATE-FL = SPACES)
052700             PERFORM 482-MOVE-DTL-TO-SCREEN
052800             THRU    482-END
052900                 VARYING I1 FROM 1 BY 1
053000                 UNTIL  (I1            > 3)
053100                 OR     (BENEFICRY-NOTFOUND)
053200                 OR     (BNF-COMPANY   NOT = DB-COMPANY)
053300                 OR     (BNF-EMPLOYEE  NOT = DB-EMPLOYEE)
053400                 OR     (BNF-PLAN-TYPE NOT = DB-PLAN-TYPE)
053500                 OR     (BNF-PLAN-CODE NOT = DB-PLAN-CODE)
               ELSE
                   MOVE 1                  TO I1
052700             PERFORM 482-MOVE-DTL-TO-SCREEN
052800             THRU    482-END
               END-IF
053600
053700         IF (BENEFICRY-NOTFOUND)
053800         OR (BNF-COMPANY   NOT = DB-COMPANY)
053900         OR (BNF-EMPLOYEE  NOT = DB-EMPLOYEE)
054000         OR (BNF-PLAN-TYPE NOT = DB-PLAN-TYPE)
054100         OR (BNF-PLAN-CODE NOT = DB-PLAN-CODE)
               OR (BN47F1-WEB-UPDATE-FL NOT = SPACES)
054200             MOVE ZEROES              TO BN47F1-PT-BNF-SEQ-NBR
054300             MOVE 1                   TO BN47F1-PT-BNF-PRIM-CNTGNT
054400             MOVE CRT-INQ-COMPLETE    TO CRT-MESSAGE
054500         ELSE
054700             MOVE BNF-PRIM-CNTGNT TO BN47F1-PT-BNF-PRIM-CNTGNT
054800             MOVE BNF-SEQ-NBR     TO BN47F1-PT-BNF-SEQ-NBR
054900             MOVE CRT-MORE-RECS   TO CRT-MESSAGE.
055000
055100 480-END.
055200******************************************************************
055300 481-MOVE-TO-SCREEN.
055400******************************************************************
CR0305     IF (BENEFIT-FOUND)                              
CR0305     OR (BENEFIT-KFOUND)                             
055500         MOVE BEN-COMPANY        TO BN47F1-BEN-COMPANY
055600         MOVE BEN-EMPLOYEE       TO BN47F1-BEN-EMPLOYEE
055700         MOVE BEN-PLAN-TYPE      TO BN47F1-BEN-PLAN-TYPE
055800         MOVE BEN-PLAN-CODE      TO BN47F1-BEN-PLAN-CODE
CR0305     ELSE
CR0305         MOVE PTB-COMPANY        TO BN47F1-BEN-COMPANY
CR0305         MOVE PTB-EMPLOYEE       TO BN47F1-BEN-EMPLOYEE
CR0305         MOVE PTB-PLAN-TYPE      TO BN47F1-BEN-PLAN-TYPE
CR0305         MOVE PTB-PLAN-CODE      TO BN47F1-BEN-PLAN-CODE.
055900     MOVE PRS-NAME               TO BN47F1-PRS-NAME.
056000     MOVE EMP-LAST-NAME          TO HRWS-LAST-NAME.
056100     MOVE EMP-FIRST-NAME         TO HRWS-FIRST-NAME.
056200     MOVE EMP-MIDDLE-INIT        TO HRWS-MIDDLE-INIT.
           MOVE EMP-NAME-SUFFIX        TO HRWS-NAME-SUFFIX.
           MOVE EMP-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE.
056300
056400     PERFORM 750-HR-FORMAT-NAME.
056500     MOVE HRWS-FORMAT-NAME       TO BN47F1-EMP-NAME.
056600
056700     MOVE PLN-DESC               TO BN47F1-PLN-DESC.
056800
056900 481-END.
057000******************************************************************
057100 482-MOVE-DTL-TO-SCREEN.
057200******************************************************************
057300     MOVE BNF-SEQ-NBR           TO BN47F1-BNF-SEQ-NBR (I1).
           MOVE BNF-BENEF-TYPE        TO BN47F1-BNF-TYPE (I1).
057400     MOVE BNF-LAST-NAME         TO BN47F1-BNF-LAST-NAME (I1).
057500     MOVE BNF-FIRST-NAME        TO BN47F1-BNF-FIRST-NAME (I1).
057600     MOVE BNF-MIDDLE-INIT       TO BN47F1-BNF-MIDDLE-INIT (I1).
           MOVE BNF-LAST-NAME-PRE     TO BN47F1-BNF-LAST-NAME-PRE (I1).
           MOVE BNF-NAME-SUFFIX       TO BN47F1-BNF-NAME-SUFFIX (I1).
           MOVE BNF-REL-CODE          TO BN47F1-BNF-REL-CODE (I1).
057700     MOVE BNF-PCT-AMT-FLAG      TO BN47F1-BNF-PCT-AMT-FLAG (I1).
057800     MOVE BNF-PMT-AMT           TO BN47F1-BNF-PMT-AMT (I1).
058200     MOVE BNF-PRIM-CNTGNT       TO BN47F1-BNF-PRIM-CNTGNT (I1).
           MOVE BNF-FICA-NBR          TO BN47F1-BNF-FICA-NBR (I1).
           MOVE BNF-TRUST             TO BN47F1-BNF-TRUST (I1).     
           MOVE BNF-CMT-TEXT          TO BN47F1-BNF-CMT-TEXT (I1).  
           MOVE BNF-EMP-ADDRESS       TO BN47F1-EMP-ADDRESS (I1).
           MOVE BNF-ADDR1             TO BN47F1-BNF-ADDR1 (I1).     
           MOVE BNF-ADDR2             TO BN47F1-BNF-ADDR2 (I1).    
           MOVE BNF-ADDR3             TO BN47F1-BNF-ADDR3 (I1).   
           MOVE BNF-ADDR4             TO BN47F1-BNF-ADDR4 (I1).  
           MOVE BNF-CITY              TO BN47F1-BNF-CITY (I1). 
           MOVE BNF-STATE             TO BN47F1-BNF-STATE (I1).
           MOVE BNF-ZIP               TO BN47F1-BNF-ZIP (I1). 
           MOVE BNF-COUNTRY-CODE      TO BN47F1-BNF-COUNTRY-CODE (I1).
058300
058400     IF (BN47F1-LINE-FC (I1) NOT = SPACES)
058500         MOVE SPACES            TO BN47F1-LINE-FC (I1)
058600     ELSE
058700     IF (BN47F1-FC = "-")
058800         PERFORM 870-FIND-PREV-BNFSET2
058900     ELSE
059000         PERFORM 860-FIND-NEXT-BNFSET2.
           MOVE 112                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO BN47F1-MORE-TAB (I1).
           MOVE 113                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO BN47F1-ADDRESS-TAB (I1).
059100
059200 482-END.
061500******************************************************************
       600-MOVE-SCR-TO-WS.
061500******************************************************************
           INITIALIZE BNBNFWS.
           MOVE I1                            TO BNBNFWS-LINE-NBR.
060000     MOVE BN47F1-BEN-COMPANY            TO BNBNF-BEN-COMPANY.
060000     MOVE BN47F1-BEN-COMPANY-FN         TO BNBNF-BEN-COMPANY-FN.
           MOVE BN47F1-LINE-FC (I1)           TO BNBNF-FC.
060100     MOVE BN47F1-BEN-EMPLOYEE           TO BNBNF-BEN-EMPLOYEE.
060200     MOVE BN47F1-BEN-PLAN-TYPE          TO BNBNF-BEN-PLAN-TYPE.
060300     MOVE BN47F1-BEN-PLAN-CODE          TO BNBNF-BEN-PLAN-CODE.
057300     MOVE BN47F1-BNF-SEQ-NBR (I1)       TO BNBNF-BNF-SEQ-NBR.
057300     MOVE BN47F1-BNF-SEQ-NBR-FN (I1)    TO BNBNF-BNF-SEQ-NBR-FN.
           MOVE BN47WS-PCT-AMT-FLG            TO BNBNFWS-PCT-AMT-FLG.
           MOVE BN47WS-PMT-AMT-P              TO BNBNFWS-PMT-AMT-P.
           MOVE BN47WS-PMT-AMT-C              TO BNBNFWS-PMT-AMT-C. 
           IF (BN47F1-BNF-TYPE (I1) = "0")
               MOVE 0                         TO BNBNF-BNF-TYPE
           ELSE
           IF (BN47F1-BNF-TYPE (I1) = "1")
               MOVE 1                         TO BNBNF-BNF-TYPE.
           MOVE BN47F1-BNF-TYPE-FN (I1)       TO BNBNF-BNF-TYPE-FN.
060400     MOVE BN47F1-BNF-LAST-NAME (I1)     TO BNBNF-BNF-LAST-NAME.
060400     MOVE BN47F1-BNF-LAST-NAME-FN (I1)  TO BNBNF-BNF-LAST-NAME-FN.
060500     MOVE BN47F1-BNF-FIRST-NAME (I1)    TO BNBNF-BNF-FIRST-NAME.
060500     MOVE BN47F1-BNF-FIRST-NAME-FN (I1)
                                           TO BNBNF-BNF-FIRST-NAME-FN.      
           MOVE BN47F1-BNF-LAST-NAME-PRE (I1) 
                                           TO BNBNF-BNF-LAST-NAME-PRE.
           MOVE BN47F1-BNF-LAST-NAME-PRE-FN (I1)
                                         TO BNBNF-BNF-LAST-NAME-PRE-FN.
           MOVE BN47F1-BNF-NAME-SUFFIX (I1)   TO BNBNF-BNF-NAME-SUFFIX.
           MOVE BN47F1-BNF-NAME-SUFFIX-FN (I1)
                                           TO BNBNF-BNF-NAME-SUFFIX-FN.
           MOVE BN47F1-BNF-REL-CODE (I1)      TO BNBNF-BNF-REL-CODE.
           MOVE BN47F1-BNF-REL-CODE-FN (I1)   TO BNBNF-BNF-REL-CODE-FN.
060600     MOVE BN47F1-BNF-MIDDLE-INIT (I1)   TO BNBNF-BNF-MIDDLE-INIT.
060600     MOVE BN47F1-BNF-MIDDLE-INIT-FN (I1)
                                          TO BNBNF-BNF-MIDDLE-INIT-FN.
060700     MOVE BN47F1-BNF-PCT-AMT-FLAG (I1)  TO BNBNF-BNF-PCT-AMT-FLAG.       
060700     MOVE BN47F1-BNF-PCT-AMT-FLAG-FN (I1)
                                          TO BNBNF-BNF-PCT-AMT-FLAG-FN.       
060800     MOVE BN47F1-BNF-PMT-AMT (I1)       TO BNBNF-BNF-PMT-AMT.
060800     MOVE BN47F1-BNF-PMT-AMT-FN (I1)    TO BNBNF-BNF-PMT-AMT-FN.
058200     MOVE BN47F1-BNF-PRIM-CNTGNT (I1)   TO BNBNF-BNF-PRIM-CNTGNT.
058200     MOVE BN47F1-BNF-PRIM-CNTGNT-FN (I1)
                                          TO BNBNF-BNF-PRIM-CNTGNT-FN.
           MOVE BN47F1-BNF-FICA-NBR (I1)      TO BNBNF-BNF-FICA-NBR.
           MOVE BN47F1-BNF-FICA-NBR-FN (I1)   TO BNBNF-BNF-FICA-NBR-FN.
           MOVE BN47F1-BNF-TRUST (I1)         TO BNBNF-BNF-TRUST.
           MOVE BN47F1-BNF-TRUST-FN (I1)      TO BNBNF-BNF-TRUST-FN.
           MOVE BN47F1-BNF-CMT-TEXT (I1)      TO BNBNF-BNF-CMT-TEXT.
           MOVE BN47F1-BNF-CMT-TEXT-FN (I1)   TO BNBNF-BNF-CMT-TEXT-FN.
           MOVE BN47F1-EMP-ADDRESS (I1)       TO BNBNF-EMP-ADDRESS.
           MOVE BN47F1-EMP-ADDRESS-FN (I1)    TO BNBNF-EMP-ADDRESS-FN.
           MOVE BN47F1-BNF-ADDR1 (I1)         TO BNBNF-BNF-ADDR1.
           MOVE BN47F1-BNF-ADDR1-FN (I1)      TO BNBNF-BNF-ADDR1-FN.
           MOVE BN47F1-BNF-ADDR2 (I1)         TO BNBNF-BNF-ADDR2.
           MOVE BN47F1-BNF-ADDR2-FN (I1)      TO BNBNF-BNF-ADDR2-FN.
           MOVE BN47F1-BNF-ADDR3 (I1)         TO BNBNF-BNF-ADDR3.
           MOVE BN47F1-BNF-ADDR3-FN (I1)      TO BNBNF-BNF-ADDR3-FN.
           MOVE BN47F1-BNF-ADDR4 (I1)         TO BNBNF-BNF-ADDR4.
           MOVE BN47F1-BNF-ADDR4-FN (I1)      TO BNBNF-BNF-ADDR4-FN.
           MOVE BN47F1-BNF-CITY (I1)          TO BNBNF-BNF-CITY.
           MOVE BN47F1-BNF-CITY-FN (I1)       TO BNBNF-BNF-CITY-FN.
           MOVE BN47F1-BNF-STATE (I1)         TO BNBNF-BNF-STATE.
           MOVE BN47F1-BNF-STATE-FN (I1)      TO BNBNF-BNF-STATE-FN.
           MOVE BN47F1-BNF-ZIP (I1)           TO BNBNF-BNF-ZIP.
           MOVE BN47F1-BNF-ZIP-FN (I1)        TO BNBNF-BNF-ZIP-FN.
           MOVE BN47F1-BNF-COUNTRY-CODE (I1)  TO BNBNF-BNF-COUNTRY-CODE.
           MOVE BN47F1-BNF-COUNTRY-CODE-FN (I1)
                                         TO BNBNF-BNF-COUNTRY-CODE-FN.
           MOVE BN47F1-USER-ID (I1)           TO BNBNF-BNF-USER-ID.
           MOVE EMP-ADDR1                     TO BNBNF-EMP-ADDR1.
           MOVE EMP-ADDR2                     TO BNBNF-EMP-ADDR2.
           MOVE PEM-SUPP-ADDR1                TO BNBNF-PEM-SUPP-ADDR1.
           MOVE PEM-SUPP-ADDR2                TO BNBNF-PEM-SUPP-ADDR2.
061400 600-END.
061500******************************************************************
       630-MOVE-WS-TO-SCR.
061500******************************************************************
057300     MOVE BNBNF-BNF-SEQ-NBR        TO BN47F1-BNF-SEQ-NBR (I1).
           IF (BNBNF-BNF-TYPE = 1)
               MOVE "1"                  TO BN47F1-BNF-TYPE (I1)
           ELSE
           IF (BNBNF-BNF-TYPE = 0)
           AND (BNBNF-BNF-PRIM-CNTGNT NOT = 0) 
               MOVE "0"                  TO BN47F1-BNF-TYPE (I1).
057400     MOVE BNBNF-BNF-LAST-NAME      TO BN47F1-BNF-LAST-NAME (I1).
057500     MOVE BNBNF-BNF-FIRST-NAME     TO BN47F1-BNF-FIRST-NAME (I1).
057600     MOVE BNBNF-BNF-MIDDLE-INIT    TO BN47F1-BNF-MIDDLE-INIT (I1).
           MOVE BNBNF-BNF-LAST-NAME-PRE 
                                      TO BN47F1-BNF-LAST-NAME-PRE (I1).
           MOVE BNBNF-BNF-NAME-SUFFIX   
                                      TO BN47F1-BNF-NAME-SUFFIX (I1).
           MOVE BNBNF-BNF-REL-CODE       TO BN47F1-BNF-REL-CODE (I1).
057700     MOVE BNBNF-BNF-PCT-AMT-FLAG 
                                      TO BN47F1-BNF-PCT-AMT-FLAG (I1).
057800     MOVE BNBNF-BNF-PMT-AMT        TO BN47F1-BNF-PMT-AMT (I1).
058200     MOVE BNBNF-BNF-PRIM-CNTGNT TO  BN47F1-BNF-PRIM-CNTGNT (I1).
           MOVE BNBNF-BNF-FICA-NBR       TO BN47F1-BNF-FICA-NBR (I1).
           MOVE BNBNF-BNF-TRUST          TO BN47F1-BNF-TRUST (I1).     
           MOVE BNBNF-BNF-CMT-TEXT       TO BN47F1-BNF-CMT-TEXT (I1).  
           MOVE BNBNF-EMP-ADDRESS        TO BN47F1-EMP-ADDRESS (I1).
           MOVE BNBNF-BNF-ADDR1          TO BN47F1-BNF-ADDR1 (I1).     
           MOVE BNBNF-BNF-ADDR2          TO BN47F1-BNF-ADDR2 (I1).    
           MOVE BNBNF-BNF-ADDR3          TO BN47F1-BNF-ADDR3 (I1).   
           MOVE BNBNF-BNF-ADDR4          TO BN47F1-BNF-ADDR4 (I1).  
           MOVE BNBNF-BNF-CITY           TO BN47F1-BNF-CITY (I1). 
           MOVE BNBNF-BNF-STATE          TO BN47F1-BNF-STATE (I1).
           MOVE BNBNF-BNF-ZIP            TO BN47F1-BNF-ZIP (I1). 
           MOVE BNBNF-BNF-COUNTRY-CODE  
                                      TO BN47F1-BNF-COUNTRY-CODE (I1).
           MOVE BNBNFWS-PMT-AMT-P        TO BN47WS-PMT-AMT-P.
           MOVE BNBNFWS-PMT-AMT-C        TO BN47WS-PMT-AMT-C.
       630-END.
061500******************************************************************       
061600 BN47S1-TRANSACTION-END.
061700******************************************************************
