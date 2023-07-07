******* HR04PD 26 <1266117667>
000100******************************************************************
000200*                             HR04PD                             *
000300******************************************************************
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  833061 | J33061 | ADD NEW FUNCTIONALITY FOR KEY FIELD CHANGE  *
      *  ------   ------   ------------------------------------------  *
      *  953995 | J53995 | PROPERLY POPULATED STATE/PROV DESCRIPTION   *
      *         |        | IN MAILING ADDRESS TAB OF HR04.9.           *
      ******************************************************************
      *               M O D I F I C A T I O N   L O G:                 *
      ******************************************************************
      *  Modified by MARK GLISSEN - MG0706                             *
      ******************************************************************
      *  07/06/04  - MODIFIED SCREEN TO ALLOWING USERS POSITIONING     *
      *              TO RECORDS NEEDED INSTEAD OF SCROLLING            *
      *  10/27/10  - M. HUNTER REAPPLIED ABOVE CUSTOMIZATIONS AFTER    *
      *              ACS       9.0 APPS UPGRADE.                       *
      *  08/24/11  - M. HUNTER REAPPLIED ABOVE CUSTOM CODE AFTER 9.0.1 *
      *             ACS        APPS UPGRADE.                           *
000400******************************************************************
000500 HR04S1-TRANSACTION              SECTION 10.
000600******************************************************************
000700 HR04S1-START.
000800
000900******************************************************************
001000 HR04S1-TRANSACTION-END.
001100******************************************************************
001200******************************************************************
001300 HR04S2-TRANSACTION              SECTION 10.
001400******************************************************************
001500 HR04S2-START.
001600
           IF  (HR04F2-PCO-TYPE NOT = SPACES)
002700     AND (HR04F2-FC       NOT = "N" AND "P")
                MOVE HR04F2-PCO-TYPE       TO DB-TYPE
                PERFORM 840-FIND-PCSSET1
                IF  (PCODESTYPE-FOUND)
                AND (PCS-DEFN-PROGRAM NOT = SPACES)
001900             MOVE PCS-DEFN-PROGRAM   TO CRT-SCREEN-CODE
002000             MOVE HR04F2-FC          TO CRT-DISPLAY-FC
002100             MOVE "I"                TO CRT-PASS-FC
002200             MOVE CRT-INQ-COMPLETE   TO CRT-MESSAGE
002300             MOVE CRT-MANUAL-CF      TO CRT-REQUEST
002400             GO TO HR04S2-TRANSACTION-END.
                    

003500     PERFORM 200-EDIT-TRAN
003600     THRU    200-END.
003700
003800     IF (NO-ERROR-FOUND)
003900         PERFORM 400-PROCESS-TRAN
004000         THRU    400-END.
004100
004200     GO TO HR04S2-TRANSACTION-END.
004300
004400******************************************************************
004500 200-EDIT-TRAN.
004600******************************************************************
004700
004800     PERFORM 210-EDIT-ACCESS
004900     THRU    210-END.
005000
005100     IF (ERROR-FOUND)
005200         GO TO 200-END.
005300
005400     IF (HR04F2-FC = "A" OR "C")
005500         PERFORM 230-EDIT-DATA
005600         THRU    230-END
005700         GO TO 200-END.
005800
005900 200-END.
006000
006100******************************************************************
006200 210-EDIT-ACCESS.
006300******************************************************************
006400
006500*    IF  (HR04F2-PCO-TYPE NOT < "01")
006600*    AND (HR04F2-PCO-TYPE NOT > "99")
006700*        MOVE SPACES             TO HR04F2-PCO-TYPE.
006800
006900*    IF (HR04F2-PCO-TYPE = SPACES)
007000*        IF (HR04F2-FC   = "N")
007100*            MOVE "A "            TO HR04F2-PCO-TYPE
007200*        ELSE
007300*            MOVE 100                         TO CRT-ERROR-NBR
007400*            MOVE HR04F2-FC-FN                TO CRT-FIELD-NBR
007500*            GO TO 210-END.
00760*
007700*    IF  (HR04F2-PCO-TYPE = "AR")
007800*    AND (HR04F2-FC       = "P")
007900*        MOVE SPACES              TO HR04F2-PCO-TYPE.

           MOVE HR04F2-PCO-TYPE    TO DB-TYPE.
           PERFORM 840-FIND-PCSSET1.
                
008300     IF  (HR04F2-FC NOT = "N")
           AND (HR04F2-FC NOT = "P")
               IF (PCODESTYPE-NOTFOUND)
               OR (PCS-SETUP-FLAG NOT = "Y")
                   MOVE 135             TO CRT-ERROR-NBR
022400             GO TO 210-END
               END-IF.

008200
008300     IF (HR04F2-FC = "N")
               IF (PCODESTYPE-FOUND) 
                   MOVE PCS-DESCRIPTION     TO DB-DESCRIPTION
008400             MOVE HIGH-VALUES         TO DB-TYPE
               END-IF
008500         PERFORM 850-FIND-NLT-PCSSET2.
008600
008700     IF (HR04F2-FC = "P")
               MOVE PCS-DESCRIPTION     TO DB-DESCRIPTION
008800         MOVE SPACES              TO DB-TYPE
008900         PERFORM 850-FIND-NLT-PCSSET2
009000         PERFORM 870-FIND-PREV-PCSSET2
009100         IF (PCODESTYPE-FOUND)
009200             MOVE PCS-DESCRIPTION TO DB-DESCRIPTION
009300             PERFORM 850-FIND-NLT-PCSSET2.
009400
009500     IF (HR04F2-FC = "N" OR "P")
009600         IF (PCODESTYPE-NOTFOUND)
009700             MOVE 12                          TO CRT-ERROR-NBR
009800             MOVE HR04F2-FC-FN                TO CRT-FIELD-NBR
009900             GO TO 210-END
010000         ELSE
010100             MOVE PCS-TYPE        TO DB-TYPE
010200                                     HR04F2-PCO-TYPE
                   MOVE PCS-DESCRIPTION TO HR04F2-PCS-DESCRIPTION.
010300
010400     IF (HR04F2-FC NOT = "I" AND "+" AND "-")
010500         GO TO 210-END.
010600
010700     IF (HR04F2-FC          = "I")
010800     OR (HR04F2-PT-PCO-TYPE NOT = HR04F2-PCO-TYPE)
010900         MOVE SPACES              TO DB-CODE
011000     ELSE
011100     IF (HR04F2-FC = "+")
011200         MOVE HR04F2-PT-PCO-CODE  TO DB-CODE
011300     ELSE
011400         MOVE HR04F2-PCO-CODE (1) TO DB-CODE.
011500
011600     PERFORM 850-FIND-NLT-PCOSET1.
011700     IF (HR04F2-FC = "-")
011800         PERFORM 870-FIND-PREV-PCOSET1.
011900
012000     IF (PCODES-NOTFOUND)
012100     OR (PCO-TYPE NOT = DB-TYPE)
012200         INITIALIZE HR04F2-DETAIL-GROUP
012300         MOVE 11                  TO CRT-ERROR-NBR
012400         INITIALIZE                  HR04F2-PT-PCO-CODE
012500         MOVE HR04F2-PCO-TYPE-FN  TO CRT-FIELD-NBR
012600         GO TO 210-END.
012700
012800 210-END.
012900
013000******************************************************************
013100 230-EDIT-DATA.
013200******************************************************************
013300
013400     PERFORM 260-EDIT-DTL-TRAN
013500     THRU    260-END
013600         VARYING I1 FROM 1 BY 1
013700         UNTIL  (I1 > 10)
013800         OR     (ERROR-FOUND).
013900
014000     IF (ERROR-FOUND)
014100         GO TO 230-END.
014200
014300 230-END.
014400
014500******************************************************************
014600 260-EDIT-DTL-TRAN.
014700******************************************************************
014800
014900     IF (HR04F2-LINE-FC (I1) = SPACES)
015000         GO TO 260-END.
015100
015200     PERFORM 520-MOVE-DTL-SCR-TO-WS
015300     THRU    520-END.
015400
015500     PERFORM 2000-HRPCO-EDIT-TRAN.
015600
015700     MOVE HRPCO-DESCRIPTION      TO HR04F2-PCO-DESCRIPTION (I1).
015800     IF (ERROR-FOUND)
015900         GO TO 260-END.
016000
016100 260-END.
016200******************************************************************
016300 400-PROCESS-TRAN.
016400******************************************************************
016500
016600     IF (HR04F2-FC = "A" OR "C")
016700         PERFORM 910-AUDIT-BEGIN
016800
016900         IF (DMS-ABORTED)
017000             GO TO 400-END
017100         END-IF
017200
017300         PERFORM 420-PROCESS-DETAIL
017400         THRU    420-END
017500             VARYING I1 FROM 1 BY 1
017600             UNTIL  (I1 > 10)
017700
017800         PERFORM 920-AUDIT-END
017900         IF (HR04F2-FC = "A")
018000             MOVE CRT-ADD-COMPLETE   TO CRT-MESSAGE
018100         END-IF
018200         IF (HR04F2-FC = "C")
018300             MOVE CRT-CHG-COMPLETE   TO CRT-MESSAGE
018400         END-IF
018500     ELSE
018600     IF (HR04F2-FC = "I" OR "-" OR "+" OR "N" OR "P")
018700         PERFORM 480-INQUIRE
018800         THRU    480-END.
018900
019000 400-END.
019100
019200******************************************************************
019300 420-PROCESS-DETAIL.
019400******************************************************************
019500
019600     IF (HR04F2-LINE-FC (I1) = SPACES)
019700         GO TO 420-END.
019800
019900     PERFORM 520-MOVE-DTL-SCR-TO-WS
020000     THRU    520-END.
020100
020200     PERFORM 3000-HRPCO-PROCESS-TRAN.
020300
020400     IF (HR04F2-LINE-FC (I1) = "A" OR "C")
020500         PERFORM 610-MOVE-DTL-TO-SCREEN
020600         THRU    610-END
020700         INITIALIZE HR04F2-LINE-FC (I1)
020800     ELSE
020900         INITIALIZE HR04F2-DETAIL-LINE (I1).
021000
021100 420-END.
021200
021300******************************************************************
021400 480-INQUIRE.
021500******************************************************************
021600
002700     IF  (HR04F2-FC       NOT = "N" AND "P")
           AND (PCS-DEFN-PROGRAM NOT = SPACES)
           AND (HR04F2-PCO-TYPE NOT = SPACES)
001900          MOVE PCS-DEFN-PROGRAM   TO CRT-SCREEN-CODE
002000          MOVE HR04F2-FC          TO CRT-DISPLAY-FC
022000          MOVE "I"                TO CRT-DISPLAY-FC
002100          MOVE "I"                TO CRT-PASS-FC
002200          MOVE CRT-INQ-COMPLETE   TO CRT-MESSAGE
002300          MOVE CRT-MANUAL-CF      TO CRT-REQUEST
022400          GO TO 480-END.

002700     IF  (HR04F2-FC           = "N" OR  "P")
                MOVE PCS-TYPE            TO DB-TYPE
                PERFORM  850-FIND-NLT-PCOSET1.

123460     IF  (HR04F2-FC            = "N" OR "P")
123460     AND (HR04F2-PCO-TYPE  NOT = SPACES) 
123460     AND (PCS-DEFN-PROGRAM NOT = SPACES)
123460         INITIALIZE HR04F2-DETAIL-GROUP 
123460         MOVE 139                TO CRT-MSG-NBR
123460         PERFORM 790-GET-MSG
123460         GO TO 480-END.
123460
022600     IF (HR04F2-FC = "I")
022700         MOVE "+"                  TO HR04F2-FC.
022800
022900     IF (HR04F2-FC = "-")
023000         MOVE HR04F2-PCO-CODE (1)  TO HR04F2-PT-PCO-CODE
023100         INITIALIZE HR04F2-DETAIL-GROUP
023200         PERFORM 610-MOVE-DTL-TO-SCREEN
023300         THRU    610-END
023400             VARYING I1 FROM 10 BY NEGATIVE-ONE
023500             UNTIL  (I1       < 1)
023600             OR     (PCODES-NOTFOUND)
023700             OR     (PCO-TYPE NOT = HR04F2-PCO-TYPE)
023800
023900         IF (I1 NOT < 1)
024000             COMPUTE I9              = I1 + 1
024100             MOVE 1                  TO I1
024200             PERFORM
024300                 UNTIL (I9 > 10)
024400                     MOVE HR04F2-DETAIL-LINE (I9)
024500                                     TO HR04F2-DETAIL-LINE (I1)
024600                     INITIALIZE         HR04F2-DETAIL-LINE (I9)
024700                     ADD 1           TO I1
024800                                        I9
024900             END-PERFORM
025000             MOVE "+"                TO HR04F2-FC
025100             MOVE HR04F2-PT-PCO-CODE TO DB-CODE
025200             PERFORM 850-FIND-NLT-PCOSET1
025300             PERFORM 610-MOVE-DTL-TO-SCREEN
025400             THRU    610-END
025500                 VARYING I1 FROM I1 BY 1
025600                 UNTIL  (I1 > 10)
025700                 OR     (PCODES-NOTFOUND)
025800                 OR     (PCO-TYPE NOT = HR04F2-PCO-TYPE)
025900
026000             IF (PCODES-NOTFOUND)
026100             OR (PCO-TYPE NOT = HR04F2-PCO-TYPE)
026200                 MOVE SPACES         TO HR04F2-PT-PCO-CODE
026300             ELSE
026400                 MOVE PCO-CODE       TO HR04F2-PT-PCO-CODE
026500             END-IF
026600         END-IF
026700
026800         IF (PCODES-NOTFOUND)
026900         OR (PCO-TYPE NOT = HR04F2-PCO-TYPE)
027000             MOVE CRT-INQ-COMPLETE TO CRT-MESSAGE
027100         ELSE
027200             MOVE CRT-MORE-RECS    TO CRT-MESSAGE
027300     ELSE
027400         INITIALIZE HR04F2-DETAIL-GROUP
027500         PERFORM 610-MOVE-DTL-TO-SCREEN
027600         THRU    610-END
027700             VARYING I1 FROM 1 BY 1
027800             UNTIL  (I1       > 10)
027900             OR     (PCODES-NOTFOUND)
028000             OR     (PCO-TYPE NOT = HR04F2-PCO-TYPE)
028100
028200         IF (PCODES-NOTFOUND)
028300         OR (PCO-TYPE NOT = HR04F2-PCO-TYPE)
028400             MOVE SPACES           TO HR04F2-PT-PCO-CODE
028500             MOVE CRT-INQ-COMPLETE TO CRT-MESSAGE
028600         ELSE
028700             MOVE PCO-CODE         TO HR04F2-PT-PCO-CODE
028800             MOVE CRT-MORE-RECS    TO CRT-MESSAGE.
028900
029000 480-END.
029100
029200******************************************************************
029300 520-MOVE-DTL-SCR-TO-WS.
029400******************************************************************
029500
029600     INITIALIZE HRPCO-SCR-FIELDS.
029700
029800     MOVE HR04F2-PCO-TYPE                TO HRPCO-TYPE.
029900     MOVE HR04F2-PCO-TYPE-FN             TO HRPCO-TYPE-FN.
030000
030100     MOVE HR04F2-LINE-FC (I1)            TO HRPCO-FC.
030200     MOVE HR04F2-LINE-FC-FN (I1)         TO HRPCO-FC-FN.
030300     MOVE HR04F2-PCO-CODE (I1)           TO HRPCO-CODE.
030400     MOVE HR04F2-PCO-CODE-FN (I1)        TO HRPCO-CODE-FN.
030500     MOVE HR04F2-PCO-DESCRIPTION (I1)    TO HRPCO-DESCRIPTION.
030600     MOVE HR04F2-PCO-DESCRIPTION-FN (I1) TO HRPCO-DESCRIPTION-FN.
030700     MOVE HR04F2-PCO-ACTIVE-FLAG (I1)    TO HRPCO-ACTIVE-FLAG.
030800     MOVE HR04F2-PCO-ACTIVE-FLAG-FN (I1) TO HRPCO-ACTIVE-FLAG-FN.
030900
031000 520-END.
031100
031200******************************************************************
031300 610-MOVE-DTL-TO-SCREEN.
031400******************************************************************
031500
031600     MOVE PCO-CODE               TO HR04F2-PCO-CODE (I1).
031700     MOVE PCO-DESCRIPTION        TO HR04F2-PCO-DESCRIPTION (I1).
031800     MOVE PCO-ACTIVE-FLAG        TO HR04F2-PCO-ACTIVE-FLAG (I1).
031900
032000     IF (HR04F2-LINE-FC (I1) NOT = SPACES)
032100         GO TO 610-END.
032200
032300     IF (HR04F2-FC = "-")
032400         PERFORM 870-FIND-PREV-PCOSET1
032500     ELSE
032600         PERFORM 860-FIND-NEXT-PCOSET1.
032700
032800 610-END.
032900
033000******************************************************************
033100 HR04S2-TRANSACTION-END.
033200******************************************************************
033300******************************************************************
033400 HR04S3-TRANSACTION              SECTION 10.
033500******************************************************************
033600 HR04S3-START.
033700
033800     PERFORM 200-EDIT-TRAN
033900     THRU    200-END.
034000
034100     IF (NO-ERROR-FOUND)
034200         PERFORM 400-PROCESS-TRAN
034300         THRU    400-END.
034400
034500     GO TO HR04S3-TRANSACTION-END.
034600
034700******************************************************************
034800 200-EDIT-TRAN.
034900******************************************************************
035000
035100     PERFORM 210-EDIT-ACCESS
035200     THRU    210-END.
035300
035400     IF (ERROR-FOUND)
035500         GO TO 200-END.
035600
035700     IF (HR04F3-FC = "A" OR "C")
035800         PERFORM 230-EDIT-DATA
035900         THRU    230-END
036000         GO TO 200-END.
036100
036200 200-END.
036300
036400******************************************************************
036500 210-EDIT-ACCESS.
036600******************************************************************
036700
036800     MOVE HR04F3-PCO-TYPE         TO DB-TYPE.
036900
037000     IF (HR04F3-FC NOT = "I" AND "+" AND "-")
037100         GO TO 210-END.
037200
037300     IF (HR04F3-FC          = "I")
037400     OR (HR04F3-PT-PCO-TYPE NOT = HR04F3-PCO-TYPE)
037500         MOVE SPACES              TO DB-CODE
037600     ELSE
037700     IF (HR04F3-FC = "+")
037800         MOVE HR04F3-PT-PCO-CODE  TO DB-CODE
037900     ELSE
038000         MOVE HR04F3-PCO-CODE (1) TO DB-CODE.
038100
038200     PERFORM 850-FIND-NLT-PCOSET1.
038300     IF (HR04F3-FC = "-")
038400         PERFORM 870-FIND-PREV-PCOSET1.
038500
038600     IF (PCODES-NOTFOUND)
038700     OR (PCO-TYPE NOT = DB-TYPE)
038800         INITIALIZE HR04F3-DETAIL-GROUP
038900         MOVE 11                     TO CRT-ERROR-NBR
039000         MOVE HR04F3-PCO-TYPE-FN     TO CRT-FIELD-NBR
039100         INITIALIZE                     HR04F3-PT-PCO-CODE
039200         GO TO 210-END.
039300
039400 210-END.
039500
039600******************************************************************
039700 230-EDIT-DATA.
039800******************************************************************
039900
040000     PERFORM 260-EDIT-DTL-TRAN
040100     THRU    260-END
040200         VARYING I1 FROM 1 BY 1
040300         UNTIL  (I1 > 10)
040400         OR     (ERROR-FOUND).
040500
040600     IF (ERROR-FOUND)
040700         GO TO 230-END.
040800
040900 230-END.
041000
041100******************************************************************
041200 260-EDIT-DTL-TRAN.
041300******************************************************************
041400
041500     IF (HR04F3-LINE-FC (I1) = SPACES)
041600         GO TO 260-END.
041700
           IF (HR04F3-LINE-FC (I1) = "A" OR "C")
           AND (HR04F3-PCO-TYPE = "PR")
              PERFORM
                  VARYING I2 FROM 1 BY 1
                      UNTIL (I2 = I1)
                      OR    (ERROR-FOUND)
                        IF  (HR04F3-PCO-CODE (I1) NOT = SPACES)
                        AND (HR04F3-PCO-CODE (I1) NOT =
                                             HR04F3-PCO-CODE (I2))
                        AND (HR04F3-PCO-EDUC-LEVEL (I1) =
                                             HR04F3-PCO-EDUC-LEVEL (I2))
                        AND (HR04F3-PCO-EDUC-LEVEL (I1) NOT = ZEROES)
                            MOVE HR04F3-PCO-EDUC-LEVEL-FN (I1)
                                             TO CRT-FIELD-NBR
                            MOVE 115         TO CRT-ERROR-NBR
                            MOVE HR04F3-PCO-CODE (I2)
                                             TO CRT-ERR-VAR1
                        END-IF
                END-PERFORM.

           IF (ERROR-FOUND)
               GO TO 260-END.

041800     PERFORM 520-MOVE-DTL-SCR-TO-WS
041900     THRU    520-END.
042000
042100     PERFORM 2000-HRPCO-EDIT-TRAN.
042200
042300     IF (ERROR-FOUND)
042400         GO TO 260-END.
042500
           PERFORM 530-MOVE-WS-TO-SCR
           THRU 530-END.

042600 260-END.
042700
042800******************************************************************
042900 400-PROCESS-TRAN.
043000******************************************************************
043100
043200     IF (HR04F3-FC = "A" OR "C")
043300         PERFORM 910-AUDIT-BEGIN
043400
043500         IF (DMS-ABORTED)
043600             GO TO 400-END
043700         END-IF
043800
043900         PERFORM 420-PROCESS-DETAIL
044000         THRU    420-END
044100             VARYING I1 FROM 1 BY 1
044200             UNTIL  (I1 > 10)
044300
044400         PERFORM 920-AUDIT-END
044500         IF (HR04F3-FC = "A")
044600             MOVE CRT-ADD-COMPLETE   TO CRT-MESSAGE
044700         END-IF
044800         IF (HR04F3-FC = "C")
044900             MOVE CRT-CHG-COMPLETE   TO CRT-MESSAGE
045000         END-IF
045100     ELSE
045200     IF (HR04F3-FC = "I" OR "-" OR "+")
045300         PERFORM 480-INQUIRE
045400         THRU    480-END.
045500
045600 400-END.
045700
045800******************************************************************
045900 420-PROCESS-DETAIL.
046000******************************************************************
046100
046200     IF (HR04F3-LINE-FC (I1) = SPACES)
046300         GO TO 420-END.
046400
046500     PERFORM 520-MOVE-DTL-SCR-TO-WS
046600     THRU    520-END.
046700
046800     PERFORM 3000-HRPCO-PROCESS-TRAN.
046900
047000     IF (HR04F3-LINE-FC (I1) = "A" OR "C")
047100         PERFORM 610-MOVE-DTL-TO-SCREEN
047200         THRU    610-END
047300         INITIALIZE HR04F3-LINE-FC (I1)
047400     ELSE
047500         INITIALIZE HR04F3-DETAIL-LINE (I1).
047600
047700 420-END.
047800
047900******************************************************************
048000 480-INQUIRE.
048100******************************************************************
048200
048300     IF (HR04F3-FC = "I")
048400         MOVE "+"                  TO HR04F3-FC.
048500
048600     IF (HR04F3-FC = "-")
048700         MOVE HR04F3-PCO-CODE (1)  TO HR04F3-PT-PCO-CODE
048800         INITIALIZE HR04F3-DETAIL-GROUP
048900         PERFORM 610-MOVE-DTL-TO-SCREEN
049000         THRU    610-END
049100             VARYING I1 FROM 10 BY NEGATIVE-ONE
049200             UNTIL  (I1       < 1)
049300             OR     (PCODES-NOTFOUND)
049400             OR     (PCO-TYPE NOT = HR04F3-PCO-TYPE)
049500
049600         IF (I1 NOT < 1)
049700             COMPUTE I9              = I1 + 1
049800             MOVE 1                  TO I1
049900             PERFORM
050000                 UNTIL (I9 > 10)
050100                     MOVE HR04F3-DETAIL-LINE (I9)
050200                                     TO HR04F3-DETAIL-LINE (I1)
050300                     INITIALIZE         HR04F3-DETAIL-LINE (I9)
050400                     ADD 1           TO I1
050500                                        I9
050600             END-PERFORM
050700             MOVE "+"                TO HR04F3-FC
050800             MOVE HR04F3-PT-PCO-CODE TO DB-CODE
050900             PERFORM 850-FIND-NLT-PCOSET1
051000             PERFORM 610-MOVE-DTL-TO-SCREEN
051100             THRU    610-END
051200                 VARYING I1 FROM I1 BY 1
051300                 UNTIL  (I1 > 10)
051400                 OR     (PCODES-NOTFOUND)
051500                 OR     (PCO-TYPE NOT = HR04F3-PCO-TYPE)
051600
051700             IF (PCODES-NOTFOUND)
051800             OR (PCO-TYPE NOT = HR04F3-PCO-TYPE)
051900                 MOVE SPACES         TO HR04F3-PT-PCO-CODE
052000             ELSE
052100                 MOVE PCO-CODE       TO HR04F3-PT-PCO-CODE
052200             END-IF
052300         END-IF
052400
052500         IF (PCODES-NOTFOUND)
052600         OR (PCO-TYPE NOT = HR04F3-PCO-TYPE)
052700             MOVE CRT-INQ-COMPLETE TO CRT-MESSAGE
052800         ELSE
052900             MOVE CRT-MORE-RECS    TO CRT-MESSAGE
053000     ELSE
053100         INITIALIZE HR04F3-DETAIL-GROUP
053200         PERFORM 610-MOVE-DTL-TO-SCREEN
053300         THRU    610-END
053400             VARYING I1 FROM 1 BY 1
053500             UNTIL  (I1       > 10)
053600             OR     (PCODES-NOTFOUND)
053700             OR     (PCO-TYPE NOT = HR04F3-PCO-TYPE)
053800
053900         IF (PCODES-NOTFOUND)
054000         OR (PCO-TYPE NOT = HR04F3-PCO-TYPE)
054100             MOVE SPACES           TO HR04F3-PT-PCO-CODE
054200             MOVE CRT-INQ-COMPLETE TO CRT-MESSAGE
054300         ELSE
054400             MOVE PCO-CODE         TO HR04F3-PT-PCO-CODE
054500             MOVE CRT-MORE-RECS    TO CRT-MESSAGE.
054600
054700 480-END.
054800
054900******************************************************************
055000 520-MOVE-DTL-SCR-TO-WS.
055100******************************************************************
055200
055300     INITIALIZE HRPCO-SCR-FIELDS.
055400
055500     MOVE HR04F3-PCO-TYPE                TO HRPCO-TYPE.
055600     MOVE HR04F3-PCO-TYPE-FN             TO HRPCO-TYPE-FN.
055700
055800     MOVE HR04F3-LINE-FC (I1)            TO HRPCO-FC.
055900     MOVE HR04F3-LINE-FC-FN (I1)         TO HRPCO-FC-FN.
056000     MOVE HR04F3-PCO-CODE (I1)           TO HRPCO-CODE.
056100     MOVE HR04F3-PCO-CODE-FN (I1)        TO HRPCO-CODE-FN.
056200     MOVE HR04F3-PCO-DESCRIPTION (I1)    TO HRPCO-DESCRIPTION.
056300     MOVE HR04F3-PCO-DESCRIPTION-FN (I1) TO HRPCO-DESCRIPTION-FN.
056400     MOVE HR04F3-PCO-ACTIVE-FLAG (I1)    TO HRPCO-ACTIVE-FLAG.
056500     MOVE HR04F3-PCO-ACTIVE-FLAG-FN (I1) TO HRPCO-ACTIVE-FLAG-FN.
056600     MOVE HR04F3-PCO-EDUC-LEVEL (I1)     TO HRPCO-EDUC-LEVEL.
056700     MOVE HR04F3-PCO-EDUC-LEVEL-FN (I1)  TO HRPCO-EDUC-LEVEL-FN.
056800
056900 520-END.
057000
054900******************************************************************
055000 530-MOVE-WS-TO-SCR.    
055100******************************************************************
          
           MOVE HRPCO-CODE            TO HR04F3-PCO-CODE (I1).
           MOVE HRPCO-DESCRIPTION     TO HR04F3-PCO-DESCRIPTION (I1).
           MOVE HRPCO-EDUC-LEVEL      TO HR04F3-PCO-EDUC-LEVEL (I1).
           MOVE HRPCO-ACTIVE-FLAG     TO HR04F3-PCO-ACTIVE-FLAG (I1).
056900 530-END.
057100******************************************************************
057200 610-MOVE-DTL-TO-SCREEN.
057300******************************************************************
057400
057500     MOVE PCO-CODE               TO HR04F3-PCO-CODE (I1).
057600     MOVE PCO-DESCRIPTION        TO HR04F3-PCO-DESCRIPTION (I1).
057700     MOVE PCO-EDUC-LEVEL         TO HR04F3-PCO-EDUC-LEVEL (I1).
057800     MOVE PCO-ACTIVE-FLAG        TO HR04F3-PCO-ACTIVE-FLAG (I1).
057900
058000     IF (HR04F3-LINE-FC (I1) NOT = SPACES)
058100         GO TO 610-END.
058200
058300     IF (HR04F3-FC = "-")
058400         PERFORM 870-FIND-PREV-PCOSET1
058500     ELSE
058600         PERFORM 860-FIND-NEXT-PCOSET1.
058700
058800 610-END.
058900
059000******************************************************************
059100 HR04S3-TRANSACTION-END.
059200******************************************************************
059300******************************************************************
059400 HR04S4-TRANSACTION              SECTION 10.
059500******************************************************************
059600 HR04S4-START.
059700
001800     IF (HR04F4-FC       = "R")
001900         MOVE "HR104"            TO CRT-SCREEN-CODE
002000         MOVE "I"                TO CRT-DISPLAY-FC
002100         MOVE "I"                TO CRT-PASS-FC
002200         MOVE SPACES             TO CRT-MESSAGE
002300         MOVE CRT-MANUAL-CF      TO CRT-REQUEST
002400         GO TO HR04S4-TRANSACTION-END.

059800     PERFORM 200-EDIT-TRAN
059900     THRU    200-END.
060000
060100     IF (NO-ERROR-FOUND)
060200         PERFORM 400-PROCESS-TRAN
060300         THRU    400-END.
060400
060500     GO TO HR04S4-TRANSACTION-END.
060600
060700******************************************************************
060800 200-EDIT-TRAN.
060900******************************************************************
061000
061100     PERFORM 210-EDIT-ACCESS
061200     THRU    210-END.
061300
061400     IF (ERROR-FOUND)
061500         GO TO 200-END.
061600
061700     IF (HR04F4-FC = "A" OR "C")
061800         PERFORM 230-EDIT-DATA
061900         THRU    230-END
062000         GO TO 200-END.
062100
062200 200-END.
062300
062400******************************************************************
062500 210-EDIT-ACCESS.
062600******************************************************************
062700
062800     MOVE HR04F4-HRU-FIELD-TYPE         TO DB-FIELD-TYPE.
062900
063000     IF (HR04F4-FC NOT = "I" AND "+" AND "-")
063100         GO TO 210-END.
063200
063300     IF (HR04F4-FC = "I")
063400         MOVE "+"                       TO HR04F4-FC.
063500
063600     IF (HR04F4-FC = "+")
063700         MOVE HR04F4-PT-HRU-FIELD-NAME  TO DB-FIELD-NAME
063800     ELSE
063900         MOVE HR04F4-HRU-FIELD-NAME (1) TO DB-FIELD-NAME.
064000
064100     PERFORM 850-FIND-NLT-HRUSET5.
064200     IF (HR04F4-FC = "-")
064300         PERFORM 870-FIND-PREV-HRUSET5.
064400
064500     IF (HRUSERFLDS-NOTFOUND)
064600     OR (HRU-FIELD-TYPE NOT = DB-FIELD-TYPE)
064700         INITIALIZE HR04F4-DETAIL-GROUP
064800         MOVE 11                     TO CRT-ERROR-NBR
064900         MOVE HR04F4-FC-FN           TO CRT-FIELD-NBR
065000         INITIALIZE                     HR04F4-PT-HRU-FIELD-NAME
065100         GO TO 210-END.
065200
           IF (HR04F4-FC = "C")
               PERFORM
                  VARYING I1 FROM 1 BY 1
                  UNTIL  (I1 > 12)
                  OR     (HR04F4-LINE-FC (I1) NOT = SPACES)
                      CONTINUE
              END-PERFORM
              IF (I1 > 12)
                  MOVE 121                  TO CRT-ERROR-NBR
                  MOVE HR04F4-FC-FN         TO CRT-FIELD-NBR
                  GO TO 210-END
              END-IF.

065300 210-END.
065400
065500******************************************************************
065600 230-EDIT-DATA.
065700******************************************************************
065800
065900     PERFORM 260-EDIT-DTL-TRAN
066000     THRU    260-END
066100         VARYING I1 FROM 1 BY 1
066200         UNTIL  (I1 > 12)
066300         OR     (ERROR-FOUND).
066400
066500     IF (ERROR-FOUND)
066600         GO TO 230-END.
066700
066800 230-END.
066900
067000******************************************************************
067100 260-EDIT-DTL-TRAN.
067200******************************************************************
067300
067400     IF (HR04F4-LINE-FC (I1) = SPACES)
067500         GO TO 260-END.
067600
067700     PERFORM 520-MOVE-DTL-SCR-TO-WS
067800     THRU    520-END.
067900
068000     PERFORM 2000-HRHRU-EDIT-TRAN.
068100
068200     IF (ERROR-FOUND)
068300         GO TO 260-END.
068400
068500 260-END.
068600******************************************************************
068700 400-PROCESS-TRAN.
068800******************************************************************
068900
069000     MOVE WS-FALSE               TO HRHRU-NOT-ADDED-SW.
069100
069200     IF (HR04F4-FC = "A" OR "C")
069300         PERFORM 910-AUDIT-BEGIN
069400
069500         IF (DMS-ABORTED)
069600             GO TO 400-END
069700         END-IF
069800
069900         PERFORM 420-PROCESS-DETAIL
070000         THRU    420-END
070100             VARYING I1 FROM 1 BY 1
070200             UNTIL  (I1 > 12)
070300
070400         PERFORM 920-AUDIT-END
070500         IF (HRHRU-NOT-ADDED)
070600             MOVE 119                    TO CRT-MSG-NBR
070700         ELSE
070800             IF (HR04F4-FC = "A")
070900                 MOVE CRT-ADD-COMPLETE   TO CRT-MESSAGE
071000             END-IF
071100             IF (HR04F4-FC = "C")
071200                 MOVE CRT-CHG-COMPLETE   TO CRT-MESSAGE
071300             END-IF
071400         END-IF
071500     ELSE
071600     IF (HR04F4-FC = "I" OR "-" OR "+")
071700         PERFORM 480-INQUIRE
071800         THRU    480-END.
071900
072000     IF (CRT-USER-XFER = "N" OR "P")
072100         MOVE "I"                    TO CRT-PASS-FC
072200         MOVE "I"                    TO CRT-DISPLAY-FC
072300         MOVE SPACES                 TO CRT-MESSAGE
072400         MOVE CRT-RTN-KNS-AND-MAN-CF TO CRT-REQUEST
072500         IF (CRT-USER-XFER = "N")
072600             MOVE "HR045"            TO CRT-SCREEN-CODE
072700         ELSE
072800             MOVE "HR046"            TO CRT-SCREEN-CODE.
072900
073000 400-END.
073100
073200******************************************************************
073300 420-PROCESS-DETAIL.
073400******************************************************************
073500
073600     IF (HR04F4-LINE-FC (I1) = SPACES)
073700         GO TO 420-END.
073800
073900     PERFORM 520-MOVE-DTL-SCR-TO-WS
074000     THRU    520-END.
074100
074200     PERFORM 3000-HRHRU-PROCESS-TRAN.
074300
           IF (HRHRU-NOT-ADDED)
               GO TO 420-END.

074400     IF (HR04F4-LINE-FC (I1) = "A" OR "C")
074500         PERFORM 610-MOVE-DTL-TO-SCREEN
074600         THRU    610-END
074700         INITIALIZE HR04F4-LINE-FC (I1)
074800     ELSE
074900         INITIALIZE HR04F4-DETAIL-LINE (I1).
075000
075100 420-END.
075200
075300******************************************************************
075400 480-INQUIRE.
075500******************************************************************
075600
032300     IF (HR04F4-FC = "I")
032400         MOVE "+"                      TO HR04F4-FC.
032500
076000     INITIALIZE HR04F4-PT-HRU-FIELD-NAME.
076100
076200     IF (HR04F4-FC = "-")
076300         MOVE HR04F4-HRU-FIELD-NAME(1) TO HR04F4-PT-HRU-FIELD-NAME
076400         INITIALIZE HR04F4-DETAIL-GROUP
076500         PERFORM 610-MOVE-DTL-TO-SCREEN
076600         THRU    610-END
076700             VARYING I1 FROM 12 BY NEGATIVE-ONE
076800             UNTIL  (I1 < 1)
076900             OR     (HRUSERFLDS-NOTFOUND)
077000             OR     (HRU-FIELD-TYPE NOT = HR04F4-HRU-FIELD-TYPE)
077100
077200         IF (I1 NOT < 1)
077300             COMPUTE I9              = I1 + 1
077400             MOVE 1                  TO I1
077500             PERFORM
077600                 UNTIL (I9 > 12)
077700                     MOVE HR04F4-DETAIL-LINE (I9)
077800                                     TO HR04F4-DETAIL-LINE (I1)
077900                     INITIALIZE         HR04F4-DETAIL-LINE (I9)
078000                     ADD 1           TO I1
078100                                        I9
078200             END-PERFORM
078300             MOVE "+"                TO HR04F4-FC
078400             MOVE HR04F4-PT-HRU-FIELD-NAME
078500                                     TO DB-FIELD-NAME
078600             PERFORM 850-FIND-NLT-HRUSET5
078700             PERFORM 610-MOVE-DTL-TO-SCREEN
078800             THRU    610-END
078900                 VARYING I1 FROM I1 BY 1
079000                 UNTIL  (I1 > 12)
079100                 OR     (HRUSERFLDS-NOTFOUND)
079200                 OR     (HRU-FIELD-TYPE
079300                                     NOT = HR04F4-HRU-FIELD-TYPE)
079400
079500             IF (HRUSERFLDS-NOTFOUND)
079600             OR (HRU-FIELD-TYPE NOT = HR04F4-HRU-FIELD-TYPE)
079700                 MOVE SPACES         TO HR04F4-PT-HRU-FIELD-NAME
079800             ELSE
079900                 MOVE HRU-FIELD-NAME TO HR04F4-PT-HRU-FIELD-NAME
080000             END-IF
080100         END-IF
080200
080300         IF (HRUSERFLDS-NOTFOUND)
080400         OR (HRU-FIELD-TYPE NOT = HR04F4-HRU-FIELD-TYPE)
080500             MOVE CRT-INQ-COMPLETE     TO CRT-MESSAGE
080600         ELSE
080700             MOVE CRT-MORE-RECS        TO CRT-MESSAGE
080800     ELSE
080900         INITIALIZE HR04F4-DETAIL-GROUP
081000         PERFORM 610-MOVE-DTL-TO-SCREEN
081100         THRU    610-END
081200             VARYING I1 FROM 1 BY 1
081300             UNTIL  (I1 > 12)
081400             OR     (HRUSERFLDS-NOTFOUND)
081500             OR     (HRU-FIELD-TYPE NOT = HR04F4-HRU-FIELD-TYPE)
081600
081700         IF (HRUSERFLDS-NOTFOUND)
081800         OR (HRU-FIELD-TYPE NOT = HR04F4-HRU-FIELD-TYPE)
081900             INITIALIZE HR04F4-PT-HRU-FIELD-NAME
082000             MOVE CRT-INQ-COMPLETE     TO CRT-MESSAGE
082100         ELSE
082200             MOVE HRU-FIELD-NAME       TO HR04F4-PT-HRU-FIELD-NAME
082300             MOVE CRT-MORE-RECS        TO CRT-MESSAGE.
082400
082500 480-END.
082600
082700******************************************************************
082800 520-MOVE-DTL-SCR-TO-WS.
082900******************************************************************
083000
083100     INITIALIZE HRHRU-SCR-FIELDS.
083200
083300     MOVE HR04F4-HRU-FIELD-TYPE          TO HRHRU-FIELD-TYPE.
083400     MOVE HR04F4-HRU-FIELD-TYPE-FN       TO HRHRU-FIELD-TYPE-FN.
083500
083600     MOVE HR04F4-LINE-FC (I1)            TO HRHRU-FC.
083700     MOVE HR04F4-LINE-FC-FN (I1)         TO HRHRU-FC-FN.
083800     MOVE HR04F4-HRU-FIELD-NAME (I1)     TO HRHRU-FIELD-NAME.
083900     MOVE HR04F4-HRU-FIELD-NAME-FN (I1)  TO HRHRU-FIELD-NAME-FN.
084000     MOVE HR04F4-HRU-INDICATOR (I1)      TO HRHRU-INDICATOR.
084100     MOVE HR04F4-HRU-INDICATOR-FN (I1)   TO HRHRU-INDICATOR-FN.
084200     MOVE HR04F4-HRU-REQ-VALUE (I1)      TO HRHRU-REQ-VALUE.
084300     MOVE HR04F4-HRU-REQ-VALUE-FN (I1)   TO HRHRU-REQ-VALUE-FN.
084400     MOVE HR04F4-HRU-PERS-ACTION (I1)    TO HRHRU-PERS-ACTION.
084500     MOVE HR04F4-HRU-PERS-ACTION-FN (I1) TO HRHRU-PERS-ACTION-FN.
084800     MOVE HR04F4-HRU-LOG-FLAG (I1)       TO HRHRU-LOG-FLAG.
084900     MOVE HR04F4-HRU-LOG-FLAG-FN (I1)    TO HRHRU-LOG-FLAG-FN.
085000     MOVE HR04F4-HRU-SEC-LEVEL (I1)      TO HRHRU-SEC-LEVEL.
085100     MOVE HR04F4-HRU-SEC-LEVEL-FN (I1)   TO HRHRU-SEC-LEVEL-FN.
085200     MOVE HR04F4-HRU-FIELD-KEY (I1)      TO HRHRU-FIELD-KEY.
085300     MOVE HR04F4-HRU-FIELD-KEY-FN (I1)   TO HRHRU-FIELD-KEY-FN.
           MOVE HR04F4-HRU-ACTIVE-FLAG (I1)    TO HRHRU-ACTIVE-FLAG.
           MOVE HR04F4-HRU-ACTIVE-FLAG-FN (I1) TO HRHRU-ACTIVE-FLAG-FN.
           MOVE "N"                            TO HRHRU-CURRENCY-FLAG.
           SET HRHRU-NO-REQ-UPD                TO TRUE.
085400
085500 520-END.
085600
085700******************************************************************
085800 610-MOVE-DTL-TO-SCREEN.
085900******************************************************************
086000
086100     MOVE HRU-FIELD-KEY          TO HR04F4-HRU-FIELD-KEY (I1).
086200     MOVE HRU-FIELD-NAME         TO HR04F4-HRU-FIELD-NAME (I1).
086300     MOVE HRU-INDICATOR          TO HR04F4-HRU-INDICATOR (I1).
086400     MOVE HRU-REQ-VALUE          TO HR04F4-HRU-REQ-VALUE (I1).
086500     MOVE HRU-PERS-ACTION        TO HR04F4-HRU-PERS-ACTION (I1).
086700     MOVE HRU-LOG-FLAG           TO HR04F4-HRU-LOG-FLAG (I1).
086800     MOVE HRU-SEC-LEVEL          TO HR04F4-HRU-SEC-LEVEL (I1).
           MOVE HRU-ACTIVE-FLAG        TO HR04F4-HRU-ACTIVE-FLAG (I1).
           INITIALIZE                     HR04F4-REQ-FLAG (I1).

           IF (HRU-REQ-FLAG = "X")
               MOVE "*"                TO HR04F4-REQ-FLAG (I1)
           ELSE
               MOVE HRU-FIELD-KEY      TO HR04WS-FIELD-KEY-A
               MOVE 2000               TO HR04WS-FIELD-NBR
               ADD HR04WS-FIELD-KEY-N  TO HR04WS-FIELD-NBR
               MOVE HR04WS-FIELD-NBR   TO DB-FLD-NBR
               MOVE PASSET3-FLD-NBR    TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PASSET3
               IF (PASCRTY-FOUND)
                   PERFORM 860-FIND-NXTRNG-PASSET3
                       UNTIL (PASCRTY-NOTFOUND)
                       OR    (PAS-REQ-FLAG = "X")
                   IF (PASCRTY-FOUND)
                       MOVE "*"        TO HR04F4-REQ-FLAG (I1)
                   END-IF
               END-IF
           END-IF.
086900
087000     IF (HRU-REQ-VALUE         = "Y")
087100         MOVE 114                TO CRT-MSG-NBR
087200         PERFORM 790-GET-MSG
087300         MOVE CRT-MESSAGE        TO HR04F4-VALUE-LIST (I1).
087400
087500     IF (HR04F4-LINE-FC (I1) NOT = SPACES)
087600         GO TO 610-END.
087700
087800     IF (HR04F4-FC = "-")
087900         PERFORM 870-FIND-PREV-HRUSET5
088000     ELSE
088100         PERFORM 860-FIND-NEXT-HRUSET5.
088200
088300 610-END.
088400
088500******************************************************************
088600 HR04S4-TRANSACTION-END.
088700******************************************************************
088800******************************************************************
088900 HR04S5-TRANSACTION              SECTION 10.
089000******************************************************************
089100 HR04S5-START.
089200
001800     IF (HR04F5-FC       = "R")
001900         MOVE "HR104"            TO CRT-SCREEN-CODE
002000         MOVE "I"                TO CRT-DISPLAY-FC
002100         MOVE "I"                TO CRT-PASS-FC
002200         MOVE SPACES             TO CRT-MESSAGE
002300         MOVE CRT-MANUAL-CF      TO CRT-REQUEST
002400         GO TO HR04S5-TRANSACTION-END.

089300     PERFORM 200-EDIT-TRAN
089400     THRU    200-END.
089500
089600     IF (NO-ERROR-FOUND)
089700         PERFORM 400-PROCESS-TRAN
089800         THRU    400-END.
089900
090000     GO TO HR04S5-TRANSACTION-END.
090100
090200******************************************************************
090300 200-EDIT-TRAN.
090400******************************************************************
090500
090600     PERFORM 210-EDIT-ACCESS
090700     THRU    210-END.
090800
090900     IF (ERROR-FOUND)
091000         GO TO 200-END.
091100
091200     IF (HR04F5-FC = "A" OR "C")
091300         PERFORM 230-EDIT-DATA
091400         THRU    230-END
091500         GO TO 200-END.
091600
091700 200-END.
091800
091900******************************************************************
092000 210-EDIT-ACCESS.
092100******************************************************************
092200
092300     MOVE HR04F5-HRU-FIELD-TYPE         TO DB-FIELD-TYPE.
092700
092800     IF (HR04F5-FC NOT = "I" AND "+" AND "-")
092900         GO TO 210-END.
093000
093100     IF (HR04F5-FC = "I")
093200         MOVE "+"                       TO HR04F5-FC.
093300
093400     IF (HR04F5-FC = "+")
093500         MOVE HR04F5-PT-HRU-FIELD-NAME  TO DB-FIELD-NAME
093600     ELSE
093700         MOVE HR04F5-HRU-FIELD-NAME (1) TO DB-FIELD-NAME.
093800
093900     PERFORM 850-FIND-NLT-HRUSET5.
094000     IF (HR04F5-FC = "-")
094100         PERFORM 870-FIND-PREV-HRUSET5.
094200
094300     IF (HRUSERFLDS-NOTFOUND)
094400     OR (HRU-FIELD-TYPE NOT = DB-FIELD-TYPE)
094500         INITIALIZE HR04F5-DETAIL-GROUP
094600         MOVE 11                     TO CRT-ERROR-NBR
094700         MOVE HR04F5-FC-FN           TO CRT-FIELD-NBR
094800         INITIALIZE                     HR04F5-PT-HRU-FIELD-NAME
094900         GO TO 210-END.
095000
           IF (HR04F5-FC = "C")
               PERFORM
                  VARYING I1 FROM 1 BY 1
                  UNTIL  (I1 > 12)
                  OR     (HR04F5-LINE-FC (I1) NOT = SPACES)
                      CONTINUE
              END-PERFORM
              IF (I1 > 12)
                  MOVE 121                  TO CRT-ERROR-NBR
                  MOVE HR04F5-FC-FN         TO CRT-FIELD-NBR
                  GO TO 210-END
              END-IF.

095100 210-END.
095200
095300******************************************************************
095400 230-EDIT-DATA.
095500******************************************************************
095600
095700     PERFORM 260-EDIT-DTL-TRAN
095800     THRU    260-END
095900         VARYING I1 FROM 1 BY 1
096000         UNTIL  (I1 > 12)
096100         OR     (ERROR-FOUND).
096200
096300     IF (ERROR-FOUND)
096400         GO TO 230-END.
096500
096600 230-END.
096700
096800******************************************************************
096900 260-EDIT-DTL-TRAN.
097000******************************************************************
097100
097200     IF (HR04F5-LINE-FC (I1) = SPACES)
097300         GO TO 260-END.
097400
097500     PERFORM 520-MOVE-DTL-SCR-TO-WS
097600     THRU    520-END.
097700
097800
097900     PERFORM 2000-HRHRU-EDIT-TRAN.
098000
098100     IF (ERROR-FOUND)
098200         GO TO 260-END.
098300
098400 260-END.
098500******************************************************************
098600 400-PROCESS-TRAN.
098700******************************************************************
098800
098900     MOVE WS-FALSE               TO HRHRU-NOT-ADDED-SW.
099000
099100     IF (HR04F5-FC = "A" OR "C")
099200         PERFORM 910-AUDIT-BEGIN
099300
099400         IF (DMS-ABORTED)
099500             GO TO 400-END
099600         END-IF
099700
099800         PERFORM 420-PROCESS-DETAIL
099900         THRU    420-END
100000             VARYING I1 FROM 1 BY 1
100100             UNTIL  (I1 > 12)
100200
100300         PERFORM 920-AUDIT-END
100400         IF (HRHRU-NOT-ADDED)
100500             MOVE 119                    TO CRT-MSG-NBR
100600         ELSE
100700             IF (HR04F5-FC = "A")
100800                 MOVE CRT-ADD-COMPLETE   TO CRT-MESSAGE
100900             END-IF
101000             IF (HR04F5-FC = "C")
101100                 MOVE CRT-CHG-COMPLETE   TO CRT-MESSAGE
101200             END-IF
101300         END-IF
101400     ELSE
101500     IF (HR04F5-FC = "I" OR "-" OR "+")
101600         PERFORM 480-INQUIRE
101700         THRU    480-END.
101800
101900     IF (CRT-USER-XFER = "N" OR "P")
102000         MOVE "I"                    TO CRT-PASS-FC
102100         MOVE "I"                    TO CRT-DISPLAY-FC
102200         MOVE SPACES                 TO CRT-MESSAGE
102300         MOVE CRT-RTN-KNS-AND-MAN-CF TO CRT-REQUEST
102400         IF (CRT-USER-XFER = "N")
102500             MOVE "HR046"            TO CRT-SCREEN-CODE
102600         ELSE
102700             MOVE "HR044"            TO CRT-SCREEN-CODE.
102800
102900 400-END.
103000
103100******************************************************************
103200 420-PROCESS-DETAIL.
103300******************************************************************
103400
103500     IF (HR04F5-LINE-FC (I1) = SPACES)
103600         GO TO 420-END.
103700
103800     PERFORM 520-MOVE-DTL-SCR-TO-WS
103900     THRU    520-END.
104000
104100     PERFORM 3000-HRHRU-PROCESS-TRAN.
104200
           IF (HRHRU-NOT-ADDED)
               GO TO 420-END.

104300     IF (HR04F5-LINE-FC (I1) = "A" OR "C")
104400         PERFORM 610-MOVE-DTL-TO-SCREEN
104500         THRU    610-END
104600         INITIALIZE HR04F5-LINE-FC (I1)
104700     ELSE
104800         INITIALIZE HR04F5-DETAIL-LINE (I1).
104900
105000 420-END.
105100
105200******************************************************************
105300 480-INQUIRE.
105400******************************************************************
105500
105600     IF (HR04F5-FC = "I")
105700         MOVE "+"                      TO HR04F5-FC.
105800
105900     INITIALIZE HR04F5-PT-HRU-FIELD-NAME.
106000
106100     IF (HR04F5-FC = "-")
106200         MOVE HR04F5-HRU-FIELD-NAME(1) TO HR04F5-PT-HRU-FIELD-NAME
106300         INITIALIZE HR04F5-DETAIL-GROUP
106400         PERFORM 610-MOVE-DTL-TO-SCREEN
106500         THRU    610-END
106600             VARYING I1 FROM 12 BY NEGATIVE-ONE
106700             UNTIL  (I1 < 1)
106800             OR     (HRUSERFLDS-NOTFOUND)
106900             OR     (HRU-FIELD-TYPE NOT = HR04F5-HRU-FIELD-TYPE)
107000
107100         IF (I1 NOT < 1)
107200             COMPUTE I9              = I1 + 1
107300             MOVE 1                  TO I1
107400             PERFORM
107500                 UNTIL (I9 > 12)
107600                     MOVE HR04F5-DETAIL-LINE (I9)
107700                                     TO HR04F5-DETAIL-LINE (I1)
107800                     INITIALIZE         HR04F5-DETAIL-LINE (I9)
107900                     ADD 1           TO I1
108000                                        I9
108100             END-PERFORM
108200             MOVE "+"                TO HR04F5-FC
108300             MOVE HR04F5-PT-HRU-FIELD-NAME
108400                                     TO DB-FIELD-NAME
108500             PERFORM 850-FIND-NLT-HRUSET5
108600             PERFORM 610-MOVE-DTL-TO-SCREEN
108700             THRU    610-END
108800                 VARYING I1 FROM I1 BY 1
108900                 UNTIL  (I1 > 12)
109000                 OR     (HRUSERFLDS-NOTFOUND)
109100                 OR     (HRU-FIELD-TYPE
109200                                     NOT = HR04F5-HRU-FIELD-TYPE)
109300
109400             IF (HRUSERFLDS-NOTFOUND)
109500             OR (HRU-FIELD-TYPE NOT = HR04F5-HRU-FIELD-TYPE)
109600                 MOVE SPACES         TO HR04F5-PT-HRU-FIELD-NAME
109700             ELSE
109800                 MOVE HRU-FIELD-NAME TO HR04F5-PT-HRU-FIELD-NAME
109900             END-IF
110000         END-IF
110100
110200         IF (HRUSERFLDS-NOTFOUND)
110300         OR (HRU-FIELD-TYPE NOT = HR04F5-HRU-FIELD-TYPE)
110400             MOVE CRT-INQ-COMPLETE     TO CRT-MESSAGE
110500         ELSE
110600             MOVE CRT-MORE-RECS        TO CRT-MESSAGE
110700     ELSE
110800         INITIALIZE HR04F5-DETAIL-GROUP
110900         PERFORM 610-MOVE-DTL-TO-SCREEN
111000         THRU    610-END
111100             VARYING I1 FROM 1 BY 1
111200             UNTIL  (I1 > 12)
111300             OR     (HRUSERFLDS-NOTFOUND)
111400             OR     (HRU-FIELD-TYPE NOT = HR04F5-HRU-FIELD-TYPE)
111500
111600         IF (HRUSERFLDS-NOTFOUND)
111700         OR (HRU-FIELD-TYPE NOT = HR04F5-HRU-FIELD-TYPE)
111800             INITIALIZE HR04F5-PT-HRU-FIELD-NAME
111900             MOVE CRT-INQ-COMPLETE     TO CRT-MESSAGE
112000         ELSE
112100             MOVE HRU-FIELD-NAME       TO HR04F5-PT-HRU-FIELD-NAME
112200             MOVE CRT-MORE-RECS        TO CRT-MESSAGE.
112300
112400 480-END.
112500
112600******************************************************************
112700 520-MOVE-DTL-SCR-TO-WS.
112800******************************************************************
112900
113000     INITIALIZE HRHRU-SCR-FIELDS.
113100
113200     MOVE HR04F5-HRU-FIELD-TYPE          TO HRHRU-FIELD-TYPE.
113300     MOVE HR04F5-HRU-FIELD-TYPE-FN       TO HRHRU-FIELD-TYPE-FN.
113400
113500     MOVE HR04F5-LINE-FC (I1)            TO HRHRU-FC.
113600     MOVE HR04F5-LINE-FC-FN (I1)         TO HRHRU-FC-FN.
113700     MOVE HR04F5-HRU-FIELD-NAME (I1)     TO HRHRU-FIELD-NAME.
113800     MOVE HR04F5-HRU-FIELD-NAME-FN (I1)  TO HRHRU-FIELD-NAME-FN.
113900     MOVE HR04F5-HRU-INDICATOR (I1)      TO HRHRU-INDICATOR.
114000     MOVE HR04F5-HRU-INDICATOR-FN (I1)   TO HRHRU-INDICATOR-FN.
114100     MOVE HR04F5-HRU-BEG-NUMBER (I1)     TO HRHRU-BEG-NUMBER.
114200     MOVE HR04F5-HRU-BEG-NUMBER-FN (I1)  TO HRHRU-BEG-NUMBER-FN.
114300     MOVE HR04F5-HRU-END-NUMBER (I1)     TO HRHRU-END-NUMBER.
114400     MOVE HR04F5-HRU-END-NUMBER-FN (I1)  TO HRHRU-END-NUMBER-FN.
           MOVE HR04F5-HRU-CURRENCY-FLAG (I1)  TO HRHRU-CURRENCY-FLAG.
           MOVE HR04F5-HRU-CURRENCY-FLAG-FN (I1)
                                               TO
                                               HRHRU-CURRENCY-FLAG-FN.
114500     MOVE HR04F5-HRU-PERS-ACTION (I1)    TO HRHRU-PERS-ACTION.
114600     MOVE HR04F5-HRU-PERS-ACTION-FN (I1) TO HRHRU-PERS-ACTION-FN.
114900     MOVE HR04F5-HRU-LOG-FLAG (I1)       TO HRHRU-LOG-FLAG.
115000     MOVE HR04F5-HRU-LOG-FLAG-FN (I1)    TO HRHRU-LOG-FLAG-FN.
115100     MOVE HR04F5-HRU-SEC-LEVEL (I1)      TO HRHRU-SEC-LEVEL.
115200     MOVE HR04F5-HRU-SEC-LEVEL-FN (I1)   TO HRHRU-SEC-LEVEL-FN.
115300     MOVE HR04F5-HRU-FIELD-KEY (I1)      TO HRHRU-FIELD-KEY.
115400     MOVE HR04F5-HRU-FIELD-KEY-FN (I1)   TO HRHRU-FIELD-KEY-FN.
           MOVE HR04F5-HRU-ACTIVE-FLAG (I1)    TO HRHRU-ACTIVE-FLAG.
           MOVE HR04F5-HRU-ACTIVE-FLAG-FN (I1) TO HRHRU-ACTIVE-FLAG-FN.
           SET HRHRU-NO-REQ-UPD                TO TRUE.
115500
115600
115700 520-END.
115800
115900******************************************************************
116000 610-MOVE-DTL-TO-SCREEN.
116100******************************************************************
116200
116300     MOVE HRU-FIELD-KEY          TO HR04F5-HRU-FIELD-KEY (I1).
116400     MOVE HRU-FIELD-NAME         TO HR04F5-HRU-FIELD-NAME (I1).
116500     MOVE HRU-INDICATOR          TO HR04F5-HRU-INDICATOR (I1).
116600     MOVE HRU-BEG-NUMBER         TO HR04F5-HRU-BEG-NUMBER (I1).
116700     MOVE HRU-END-NUMBER         TO HR04F5-HRU-END-NUMBER (I1).
           MOVE HRU-CURRENCY-FLAG      TO HR04F5-HRU-CURRENCY-FLAG (I1).
116800     MOVE HRU-PERS-ACTION        TO HR04F5-HRU-PERS-ACTION (I1).
117000     MOVE HRU-LOG-FLAG           TO HR04F5-HRU-LOG-FLAG (I1).
117100     MOVE HRU-SEC-LEVEL          TO HR04F5-HRU-SEC-LEVEL (I1).
           INITIALIZE                     HR04F5-REQ-FLAG (I1).
           MOVE HRU-ACTIVE-FLAG        TO HR04F5-HRU-ACTIVE-FLAG (I1).

           IF (HRU-REQ-FLAG = "X")
               MOVE "*"                TO HR04F5-REQ-FLAG (I1)
           ELSE
               MOVE HRU-FIELD-KEY      TO HR04WS-FIELD-KEY-A
               MOVE 2000               TO HR04WS-FIELD-NBR
               ADD HR04WS-FIELD-KEY-N  TO HR04WS-FIELD-NBR
               MOVE HR04WS-FIELD-NBR   TO DB-FLD-NBR
               MOVE PASSET3-FLD-NBR    TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PASSET3
               IF (PASCRTY-FOUND)
                   PERFORM 860-FIND-NXTRNG-PASSET3
                       UNTIL (PASCRTY-NOTFOUND)
                       OR    (PAS-REQ-FLAG = "X")
                   IF (PASCRTY-FOUND)
                       MOVE "*"            TO HR04F5-REQ-FLAG (I1)
                   END-IF
               END-IF
           END-IF.

117300     IF (HR04F5-LINE-FC (I1) NOT = SPACES)
117400         GO TO 610-END.
117500
117600     IF (HR04F5-FC = "-")
117700         PERFORM 870-FIND-PREV-HRUSET5
117800     ELSE
117900         PERFORM 860-FIND-NEXT-HRUSET5.
118000
118100 610-END.
118200
118300******************************************************************
118400 HR04S5-TRANSACTION-END.
118500******************************************************************
118600******************************************************************
118700 HR04S6-TRANSACTION              SECTION 10.
118800******************************************************************
118900 HR04S6-START.
119000
001800     IF (HR04F6-FC       = "R")
001900         MOVE "HR104"            TO CRT-SCREEN-CODE
002000         MOVE "I"                TO CRT-DISPLAY-FC
002100         MOVE "I"                TO CRT-PASS-FC
002200         MOVE SPACES             TO CRT-MESSAGE
002300         MOVE CRT-MANUAL-CF      TO CRT-REQUEST
002400         GO TO HR04S6-TRANSACTION-END.

119100     PERFORM 200-EDIT-TRAN
119200     THRU    200-END.
119300
119400     IF (NO-ERROR-FOUND)
119500         PERFORM 400-PROCESS-TRAN
119600         THRU    400-END.
119700
119800     GO TO HR04S6-TRANSACTION-END.
119900
120000******************************************************************
120100 200-EDIT-TRAN.
120200******************************************************************
120300
120400     PERFORM 210-EDIT-ACCESS
120500     THRU    210-END.
120600
120700     IF (ERROR-FOUND)
120800         GO TO 200-END.
120900
121000     IF (HR04F6-FC = "A" OR "C")
121100         PERFORM 230-EDIT-DATA
121200         THRU    230-END
121300         GO TO 200-END.
121400
121500 200-END.
121600
121700******************************************************************
121800 210-EDIT-ACCESS.
121900******************************************************************
122000
122100     MOVE HR04F6-HRU-FIELD-TYPE         TO DB-FIELD-TYPE.
122200
122300     IF (HR04F6-FC NOT = "I" AND "+" AND "-")
122400         GO TO 210-END.
122500
122600     IF (HR04F6-FC = "I")
122700         MOVE "+"                       TO HR04F6-FC.
122800
122900     IF (HR04F6-FC = "+")
123000         MOVE HR04F6-PT-HRU-FIELD-NAME  TO DB-FIELD-NAME
123100     ELSE
123200         MOVE HR04F6-HRU-FIELD-NAME (1) TO DB-FIELD-NAME.
123300
123400     PERFORM 850-FIND-NLT-HRUSET5.
123500     IF (HR04F6-FC = "-")
123600         PERFORM 870-FIND-PREV-HRUSET5.
123700
123800     IF (HRUSERFLDS-NOTFOUND)
123900     OR (HRU-FIELD-TYPE NOT = DB-FIELD-TYPE)
124000         INITIALIZE HR04F6-DETAIL-GROUP
124100         MOVE 11                     TO CRT-ERROR-NBR
124200         MOVE HR04F6-FC-FN           TO CRT-FIELD-NBR
124300         INITIALIZE                     HR04F6-PT-HRU-FIELD-NAME
124400         GO TO 210-END.
124500
           IF (HR04F6-FC = "C")
               PERFORM
                  VARYING I1 FROM 1 BY 1
                  UNTIL  (I1 > 12)
                  OR     (HR04F6-LINE-FC (I1) NOT = SPACES)
                      CONTINUE
              END-PERFORM
              IF (I1 > 12)
                  MOVE 121                  TO CRT-ERROR-NBR
                  MOVE HR04F6-FC-FN         TO CRT-FIELD-NBR
                  GO TO 210-END
              END-IF.

124600 210-END.
124700
124800******************************************************************
124900 230-EDIT-DATA.
125000******************************************************************
125100
125200     PERFORM 260-EDIT-DTL-TRAN
125300     THRU    260-END
125400         VARYING I1 FROM 1 BY 1
125500         UNTIL  (I1 > 12)
125600         OR     (ERROR-FOUND).
125700
125800     IF (ERROR-FOUND)
125900         GO TO 230-END.
126000
126100 230-END.
126200
126300******************************************************************
126400 260-EDIT-DTL-TRAN.
126500******************************************************************
126600
126700     IF (HR04F6-LINE-FC (I1) = SPACES)
126800         GO TO 260-END.
126900
127000     PERFORM 520-MOVE-DTL-SCR-TO-WS
127100     THRU    520-END.
127200
127300     PERFORM 2000-HRHRU-EDIT-TRAN.
127400
127500     IF (ERROR-FOUND)
127600         GO TO 260-END.
127700
127800 260-END.
127900******************************************************************
128000 400-PROCESS-TRAN.
128100******************************************************************
128200
128300     MOVE WS-FALSE               TO HRHRU-NOT-ADDED-SW.
128400
128500     IF (HR04F6-FC = "A" OR "C")
128600         PERFORM 910-AUDIT-BEGIN
128700
128800         IF (DMS-ABORTED)
128900             GO TO 400-END
129000         END-IF
129100
129200         PERFORM 420-PROCESS-DETAIL
129300         THRU    420-END
129400             VARYING I1 FROM 1 BY 1
129500             UNTIL  (I1 > 12)
129600
129700         PERFORM 920-AUDIT-END
129800         IF (HRHRU-NOT-ADDED)
129900             MOVE 119                    TO CRT-MSG-NBR
130000         ELSE
130100             IF (HR04F6-FC = "A")
130200                 MOVE CRT-ADD-COMPLETE   TO CRT-MESSAGE
130300             END-IF
130400             IF (HR04F6-FC = "C")
130500                 MOVE CRT-CHG-COMPLETE   TO CRT-MESSAGE
130600             END-IF
130700         END-IF
130800     ELSE
130900     IF (HR04F6-FC = "I" OR "-" OR "+")
131000         PERFORM 480-INQUIRE
131100         THRU    480-END.
131200
131300     IF (CRT-USER-XFER = "N" OR "P")
131400         MOVE "I"                    TO CRT-PASS-FC
131500         MOVE "I"                    TO CRT-DISPLAY-FC
131600         MOVE SPACES                 TO CRT-MESSAGE
131700         MOVE CRT-RTN-KNS-AND-MAN-CF TO CRT-REQUEST
131800         IF (CRT-USER-XFER = "N")
131900             MOVE "HR044"            TO CRT-SCREEN-CODE
132000         ELSE
132100             MOVE "HR045"            TO CRT-SCREEN-CODE.
132200
132300 400-END.
132400
132500******************************************************************
132600 420-PROCESS-DETAIL.
132700******************************************************************
132800
132900     IF (HR04F6-LINE-FC (I1) = SPACES)
133000         GO TO 420-END.
133100
133200     PERFORM 520-MOVE-DTL-SCR-TO-WS
133300     THRU    520-END.
133400
133500     PERFORM 3000-HRHRU-PROCESS-TRAN.
133600
           IF (HRHRU-NOT-ADDED)
               GO TO 420-END.

133700     IF (HR04F6-LINE-FC (I1) = "A" OR "C")
133800         PERFORM 610-MOVE-DTL-TO-SCREEN
133900         THRU    610-END
134000         INITIALIZE HR04F6-LINE-FC (I1)
134100     ELSE
134200         INITIALIZE HR04F6-DETAIL-LINE (I1).
134300
134400 420-END.
134500
134600******************************************************************
134700 480-INQUIRE.
134800******************************************************************
134900
135000     IF (HR04F6-FC = "I")
135100         MOVE "+"                      TO HR04F6-FC.
135200
135300     INITIALIZE HR04F6-PT-HRU-FIELD-NAME.
135400
135500     IF (HR04F6-FC = "-")
135600         MOVE HR04F6-HRU-FIELD-NAME(1) TO HR04F6-PT-HRU-FIELD-NAME
135700         INITIALIZE HR04F6-DETAIL-GROUP
135800         PERFORM 610-MOVE-DTL-TO-SCREEN
135900         THRU    610-END
136000             VARYING I1 FROM 12 BY NEGATIVE-ONE
136100             UNTIL  (I1 < 1)
136200             OR     (HRUSERFLDS-NOTFOUND)
136300             OR     (HRU-FIELD-TYPE NOT = HR04F6-HRU-FIELD-TYPE)
136400
136500         IF (I1 NOT < 1)
136600             COMPUTE I9              = I1 + 1
136700             MOVE 1                  TO I1
136800             PERFORM
136900                 UNTIL (I9 > 12)
137000                     MOVE HR04F6-DETAIL-LINE (I9)
137100                                     TO HR04F6-DETAIL-LINE (I1)
137200                     INITIALIZE         HR04F6-DETAIL-LINE (I9)
137300                     ADD 1           TO I1
137400                                        I9
137500             END-PERFORM
137600             MOVE "+"                TO HR04F6-FC
137700             MOVE HR04F6-PT-HRU-FIELD-NAME
137800                                     TO DB-FIELD-NAME
137900             PERFORM 850-FIND-NLT-HRUSET5
138000             PERFORM 610-MOVE-DTL-TO-SCREEN
138100             THRU    610-END
138200                 VARYING I1 FROM I1 BY 1
138300                 UNTIL  (I1 > 12)
138400                 OR     (HRUSERFLDS-NOTFOUND)
138500                 OR     (HRU-FIELD-TYPE
138600                                     NOT = HR04F6-HRU-FIELD-TYPE)
138700
138800             IF (HRUSERFLDS-NOTFOUND)
138900             OR (HRU-FIELD-TYPE NOT = HR04F6-HRU-FIELD-TYPE)
139000                 MOVE SPACES         TO HR04F6-PT-HRU-FIELD-NAME
139100             ELSE
139200                 MOVE HRU-FIELD-NAME TO HR04F6-PT-HRU-FIELD-NAME
139300             END-IF
139400         END-IF
139500
139600         IF (HRUSERFLDS-NOTFOUND)
139700         OR (HRU-FIELD-TYPE NOT = HR04F6-HRU-FIELD-TYPE)
139800             MOVE CRT-INQ-COMPLETE     TO CRT-MESSAGE
139900         ELSE
140000             MOVE CRT-MORE-RECS        TO CRT-MESSAGE
140100     ELSE
140200         INITIALIZE HR04F6-DETAIL-GROUP
140300         PERFORM 610-MOVE-DTL-TO-SCREEN
140400         THRU    610-END
140500             VARYING I1 FROM 1 BY 1
140600             UNTIL  (I1 > 12)
140700             OR     (HRUSERFLDS-NOTFOUND)
140800             OR     (HRU-FIELD-TYPE NOT = HR04F6-HRU-FIELD-TYPE)
140900
141000         IF (HRUSERFLDS-NOTFOUND)
141100         OR (HRU-FIELD-TYPE NOT = HR04F6-HRU-FIELD-TYPE)
141200             INITIALIZE HR04F6-PT-HRU-FIELD-NAME
141300             MOVE CRT-INQ-COMPLETE     TO CRT-MESSAGE
141400         ELSE
141500             MOVE HRU-FIELD-NAME       TO HR04F6-PT-HRU-FIELD-NAME
141600             MOVE CRT-MORE-RECS        TO CRT-MESSAGE.
141700
141800 480-END.
141900
142000******************************************************************
142100 520-MOVE-DTL-SCR-TO-WS.
142200******************************************************************
142300
142400     INITIALIZE HRHRU-SCR-FIELDS.
142500
142600     MOVE HR04F6-HRU-FIELD-TYPE          TO HRHRU-FIELD-TYPE.
142700     MOVE HR04F6-HRU-FIELD-TYPE-FN       TO HRHRU-FIELD-TYPE-FN.
142800
142900     MOVE HR04F6-LINE-FC (I1)            TO HRHRU-FC.
143000     MOVE HR04F6-LINE-FC-FN (I1)         TO HRHRU-FC-FN.
143100     MOVE HR04F6-HRU-FIELD-NAME (I1)     TO HRHRU-FIELD-NAME.
143200     MOVE HR04F6-HRU-FIELD-NAME-FN (I1)  TO HRHRU-FIELD-NAME-FN.
143300     MOVE HR04F6-HRU-INDICATOR (I1)      TO HRHRU-INDICATOR.
143400     MOVE HR04F6-HRU-INDICATOR-FN (I1)   TO HRHRU-INDICATOR-FN.
143500     MOVE HR04F6-HRU-BEG-DATE (I1)       TO HRHRU-BEG-DATE.
143600     MOVE HR04F6-HRU-BEG-DATE-FN (I1)    TO HRHRU-BEG-DATE-FN.
143700     MOVE HR04F6-HRU-END-DATE (I1)       TO HRHRU-END-DATE.
143800     MOVE HR04F6-HRU-END-DATE-FN (I1)    TO HRHRU-END-DATE-FN.
143900     MOVE HR04F6-HRU-PERS-ACTION (I1)    TO HRHRU-PERS-ACTION.
144000     MOVE HR04F6-HRU-PERS-ACTION-FN (I1) TO HRHRU-PERS-ACTION-FN.
144300     MOVE HR04F6-HRU-LOG-FLAG (I1)       TO HRHRU-LOG-FLAG.
144400     MOVE HR04F6-HRU-LOG-FLAG-FN (I1)    TO HRHRU-LOG-FLAG-FN.
144500     MOVE HR04F6-HRU-SEC-LEVEL (I1)      TO HRHRU-SEC-LEVEL.
144600     MOVE HR04F6-HRU-SEC-LEVEL-FN (I1)   TO HRHRU-SEC-LEVEL-FN.
144700     MOVE HR04F6-HRU-FIELD-KEY (I1)      TO HRHRU-FIELD-KEY.
144800     MOVE HR04F6-HRU-FIELD-KEY-FN (I1)   TO HRHRU-FIELD-KEY-FN.
           MOVE "N"                            TO HRHRU-CURRENCY-FLAG.
           MOVE HR04F6-HRU-ACTIVE-FLAG (I1)    TO HRHRU-ACTIVE-FLAG.
           MOVE HR04F6-HRU-ACTIVE-FLAG-FN (I1) TO HRHRU-ACTIVE-FLAG-FN.
           SET HRHRU-NO-REQ-UPD                TO TRUE.
144900
145000 520-END.
145100
145200******************************************************************
145300 610-MOVE-DTL-TO-SCREEN.
145400******************************************************************
145500
145600     MOVE HRU-FIELD-KEY          TO HR04F6-HRU-FIELD-KEY (I1).
145700     MOVE HRU-FIELD-NAME         TO HR04F6-HRU-FIELD-NAME (I1).
145800     MOVE HRU-INDICATOR          TO HR04F6-HRU-INDICATOR (I1).
145900     MOVE HRU-BEG-DATE           TO HR04F6-HRU-BEG-DATE (I1).
146000     MOVE HRU-END-DATE           TO HR04F6-HRU-END-DATE (I1).
146100     MOVE HRU-PERS-ACTION        TO HR04F6-HRU-PERS-ACTION (I1).
146300     MOVE HRU-LOG-FLAG           TO HR04F6-HRU-LOG-FLAG (I1).
146400     MOVE HRU-SEC-LEVEL          TO HR04F6-HRU-SEC-LEVEL (I1).
           INITIALIZE                     HR04F6-REQ-FLAG (I1).
           MOVE HRU-ACTIVE-FLAG        TO HR04F6-HRU-ACTIVE-FLAG (I1).

           IF (HRU-REQ-FLAG = "X")
               MOVE "*"                TO HR04F6-REQ-FLAG (I1)
           ELSE
               MOVE HRU-FIELD-KEY      TO HR04WS-FIELD-KEY-A
               MOVE 2000               TO HR04WS-FIELD-NBR
               ADD HR04WS-FIELD-KEY-N  TO HR04WS-FIELD-NBR
               MOVE HR04WS-FIELD-NBR   TO DB-FLD-NBR
               MOVE PASSET3-FLD-NBR    TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PASSET3
               IF (PASCRTY-FOUND)
                   PERFORM 860-FIND-NXTRNG-PASSET3
                       UNTIL (PASCRTY-NOTFOUND)
                       OR    (PAS-REQ-FLAG = "X")
                   IF (PASCRTY-FOUND)
                       MOVE "*"            TO HR04F6-REQ-FLAG (I1)
                   END-IF
               END-IF
           END-IF.

146600     IF (HR04F6-LINE-FC (I1) NOT = SPACES)
146700         GO TO 610-END.
146800
146900     IF (HR04F6-FC = "-")
147000         PERFORM 870-FIND-PREV-HRUSET5
147100     ELSE
147200         PERFORM 860-FIND-NEXT-HRUSET5.
147300
147400 610-END.
147500
147600******************************************************************
147700 HR04S6-TRANSACTION-END.
147800******************************************************************
147900******************************************************************
148000 HR04S7-TRANSACTION              SECTION 10.
148100******************************************************************
148200 HR04S7-START.
148300
148400     PERFORM 200-EDIT-TRAN
148500     THRU    200-END.
148600
148700     IF (NO-ERROR-FOUND)
148800         PERFORM 400-PROCESS-TRAN
148900         THRU    400-END.
149000
149100     GO TO HR04S7-TRANSACTION-END.
149200
149300******************************************************************
149400 200-EDIT-TRAN.
149500******************************************************************
149600
149700     PERFORM 210-EDIT-ACCESS
149800     THRU    210-END.
149900
150000     IF (ERROR-FOUND)
150100         GO TO 200-END.
150200
150300     IF (HR04F7-FC = "A" OR "C")
150400         PERFORM 230-EDIT-DATA
150500         THRU    230-END
150600         GO TO 200-END.
150700
150800 200-END.
150900
151000******************************************************************
151100 210-EDIT-ACCESS.
151200******************************************************************
151300
151400     IF (HR04F7-PCO-TYPE = SPACES)
151500         MOVE 100                             TO CRT-ERROR-NBR
151600         MOVE HR04F7-FC-FN                    TO CRT-FIELD-NBR
151700         GO TO 210-END.
151800
151900     MOVE HR04F7-PCO-TYPE        TO DB-TYPE.
152000     MOVE HR04F7-PCO-TYPE        TO DB-FIELD-KEY.
152100     PERFORM 840-FIND-HRUSET1.
152200     IF (HRUSERFLDS-NOTFOUND)
152300         MOVE 103                             TO CRT-ERROR-NBR
152400         MOVE HR04F7-FC-FN                    TO CRT-FIELD-NBR
152500         GO TO 210-END.
152600
152700     MOVE HRU-FIELD-NAME         TO HR04F7-HRU-FIELD-NAME.
152800
152900     IF (HR04F7-FC NOT = "I" AND "+" AND "-")
153000         GO TO 210-END.
MG0706
MG0706     IF (HR04F7-FC = "I")
MG0706     OR (HR04F7-PT-PCO-TYPE NOT = HR04F7-PCO-TYPE)
MG0706         IF (HR04F7-PS-PCO-CODE = SPACES)
MG0706             MOVE SPACES              TO DB-CODE
MG0706         ELSE
MG0706             MOVE HR04F7-PS-PCO-CODE  TO DB-CODE
MG0706             MOVE SPACES              TO HR04F7-PS-PCO-CODE
MG0706         END-IF
MG0706     ELSE
MG0706         IF (HR04F7-FC = "+")
MG0706             MOVE HR04F7-PT-PCO-CODE  TO DB-CODE
MG0706         ELSE
MG0706             MOVE HR04F7-PCO-CODE (1) TO DB-CODE
MG0706         END-IF
MG0706     END-IF.
MG0706
153100
MG0706*153200     IF (HR04F7-FC          = "I")
MG0706*153300     OR (HR04F7-PT-PCO-TYPE NOT = HR04F7-PCO-TYPE)
MG0706*153400         MOVE SPACES              TO DB-CODE
MG0706*153500     ELSE
MG0706*153600     IF (HR04F7-FC = "+")
MG0706*153700         MOVE HR04F7-PT-PCO-CODE  TO DB-CODE
MG0706*153800     ELSE
MG0706*153900         MOVE HR04F7-PCO-CODE (1) TO DB-CODE.
154000
154100     PERFORM 850-FIND-NLT-PCOSET1.
154200     IF (HR04F7-FC = "-")
154300         PERFORM 870-FIND-PREV-PCOSET1.
154400
154500     IF (PCODES-NOTFOUND)
154600     OR (PCO-TYPE NOT = DB-TYPE)
154700         INITIALIZE HR04F7-DETAIL-GROUP
154800         MOVE 11                     TO CRT-ERROR-NBR
154900         MOVE HR04F7-PCO-TYPE-FN     TO CRT-FIELD-NBR
155000         INITIALIZE                     HR04F7-PT-PCO-CODE
155100         GO TO 210-END.
155200
155300 210-END.
155400
155500******************************************************************
155600 230-EDIT-DATA.
155700******************************************************************
155800
155900     PERFORM 260-EDIT-DTL-TRAN
156000     THRU    260-END
156100         VARYING I1 FROM 1 BY 1
156200         UNTIL  (I1 > 10)
156300         OR     (ERROR-FOUND).
156400
156500 230-END.
156600
156700******************************************************************
156800 260-EDIT-DTL-TRAN.
156900******************************************************************
157000
157100     IF (HR04F7-LINE-FC (I1) = SPACES)
157200         GO TO 260-END.
157300
157400     PERFORM 520-MOVE-DTL-SCR-TO-WS
157500     THRU    520-END.
157600
157700     PERFORM 2000-HRPCO-EDIT-TRAN.
157800
157900     IF (ERROR-FOUND)
158000         GO TO 260-END.
158100
158200 260-END.
158300
158400******************************************************************
158500 400-PROCESS-TRAN.
158600******************************************************************
158700
158800     IF (HR04F7-FC = "A" OR "C")
158900         PERFORM 910-AUDIT-BEGIN
159000
159100         IF (DMS-ABORTED)
159200             GO TO 400-END
159300         END-IF
159400
159500         PERFORM 420-PROCESS-DETAIL
159600         THRU    420-END
159700             VARYING I1 FROM 1 BY 1
159800             UNTIL  (I1 > 10)
159900
160000         PERFORM 920-AUDIT-END
160100         IF (HR04F7-FC = "A")
160200             MOVE CRT-ADD-COMPLETE   TO CRT-MESSAGE
160300         END-IF
160400         IF (HR04F7-FC = "C")
160500             MOVE CRT-CHG-COMPLETE   TO CRT-MESSAGE
160600         END-IF
160700     ELSE
160800     IF (HR04F7-FC = "I" OR "-" OR "+")
160900         PERFORM 480-INQUIRE
161000         THRU    480-END.
161100
161200 400-END.
161300
161400******************************************************************
161500 420-PROCESS-DETAIL.
161600******************************************************************
161700
161800     IF (HR04F7-LINE-FC (I1) = SPACES)
161900         GO TO 420-END.
162000
162100     PERFORM 520-MOVE-DTL-SCR-TO-WS
162200     THRU    520-END.
162300
162400     PERFORM 3000-HRPCO-PROCESS-TRAN.
162500
162600     IF (HR04F7-LINE-FC (I1) = "A" OR "C")
162700         PERFORM 610-MOVE-DTL-TO-SCREEN
162800         THRU    610-END
162900         INITIALIZE HR04F7-LINE-FC (I1)
163000     ELSE
163100         INITIALIZE HR04F7-DETAIL-LINE (I1).
163200
163300 420-END.
163400
163500******************************************************************
163600 480-INQUIRE.
163700******************************************************************
163800
163900     IF (HR04F7-FC = "I")
164000         MOVE "+"                  TO HR04F7-FC.
164100
164200     IF (HR04F7-FC = "-")
164300         MOVE HR04F7-PCO-CODE (1)  TO HR04F7-PT-PCO-CODE
164400         INITIALIZE HR04F7-DETAIL-GROUP
164500         PERFORM 610-MOVE-DTL-TO-SCREEN
164600         THRU    610-END
164700             VARYING I1 FROM 10 BY NEGATIVE-ONE
164800             UNTIL  (I1       < 1)
164900             OR     (PCODES-NOTFOUND)
165000             OR     (PCO-TYPE NOT = HR04F7-PCO-TYPE)
165100
165200         IF (I1 NOT < 1)
165300             COMPUTE I9              = I1 + 1
165400             MOVE 1                  TO I1
165500             PERFORM
165600                 UNTIL (I9 > 10)
165700                     MOVE HR04F7-DETAIL-LINE (I9)
165800                                     TO HR04F7-DETAIL-LINE (I1)
165900                     INITIALIZE         HR04F7-DETAIL-LINE (I9)
166000                     ADD 1           TO I1
166100                                        I9
166200             END-PERFORM
166300             MOVE "+"                TO HR04F7-FC
166400             MOVE HR04F7-PT-PCO-CODE TO DB-CODE
166500             PERFORM 850-FIND-NLT-PCOSET1
166600             PERFORM 610-MOVE-DTL-TO-SCREEN
166700             THRU    610-END
166800                 VARYING I1 FROM I1 BY 1
166900                 UNTIL  (I1 > 10)
167000                 OR     (PCODES-NOTFOUND)
167100                 OR     (PCO-TYPE NOT = HR04F7-PCO-TYPE)
167200
167300             IF (PCODES-NOTFOUND)
167400             OR (PCO-TYPE NOT = HR04F7-PCO-TYPE)
167500                 MOVE SPACES         TO HR04F7-PT-PCO-CODE
167600             ELSE
167700                 MOVE PCO-CODE       TO HR04F7-PT-PCO-CODE
167800             END-IF
167900         END-IF
168000
168100         IF (PCODES-NOTFOUND)
168200         OR (PCO-TYPE NOT = HR04F7-PCO-TYPE)
168300             MOVE CRT-INQ-COMPLETE TO CRT-MESSAGE
168400         ELSE
168500             MOVE CRT-MORE-RECS    TO CRT-MESSAGE
168600     ELSE
168700         INITIALIZE HR04F7-DETAIL-GROUP
168800         PERFORM 610-MOVE-DTL-TO-SCREEN
168900         THRU    610-END
169000             VARYING I1 FROM 1 BY 1
169100             UNTIL  (I1       > 10)
169200             OR     (PCODES-NOTFOUND)
169300             OR     (PCO-TYPE NOT = HR04F7-PCO-TYPE)
169400
169500         IF (PCODES-NOTFOUND)
169600         OR (PCO-TYPE NOT = HR04F7-PCO-TYPE)
169700             MOVE SPACES           TO HR04F7-PT-PCO-CODE
169800             MOVE CRT-INQ-COMPLETE TO CRT-MESSAGE
169900         ELSE
170000             MOVE PCO-CODE         TO HR04F7-PT-PCO-CODE
170100             MOVE CRT-MORE-RECS    TO CRT-MESSAGE.
170200
170300 480-END.
170400
170500******************************************************************
170600 520-MOVE-DTL-SCR-TO-WS.
170700******************************************************************
170800
170900     INITIALIZE HRPCO-SCR-FIELDS.
171000
171100     MOVE HR04F7-PCO-TYPE                TO HRPCO-TYPE.
171200     MOVE HR04F7-PCO-TYPE-FN             TO HRPCO-TYPE-FN.
171300
171400     MOVE HR04F7-LINE-FC (I1)            TO HRPCO-FC.
171500     MOVE HR04F7-LINE-FC-FN (I1)         TO HRPCO-FC-FN.
171600     MOVE HR04F7-PCO-CODE (I1)           TO HRPCO-CODE.
171700     MOVE HR04F7-PCO-CODE-FN (I1)        TO HRPCO-CODE-FN.
171800     MOVE HR04F7-PCO-DESCRIPTION (I1)    TO HRPCO-DESCRIPTION.
171900     MOVE HR04F7-PCO-DESCRIPTION-FN (I1) TO HRPCO-DESCRIPTION-FN.
172000     MOVE HR04F7-PCO-ACTIVE-FLAG (I1)    TO HRPCO-ACTIVE-FLAG.
172100     MOVE HR04F7-PCO-ACTIVE-FLAG-FN (I1) TO HRPCO-ACTIVE-FLAG-FN.
172200
172300 520-END.
172400
172500******************************************************************
172600 610-MOVE-DTL-TO-SCREEN.
172700******************************************************************
172800
172900     MOVE PCO-CODE               TO HR04F7-PCO-CODE (I1).
173000     MOVE PCO-DESCRIPTION        TO HR04F7-PCO-DESCRIPTION (I1).
173100     MOVE PCO-ACTIVE-FLAG        TO HR04F7-PCO-ACTIVE-FLAG (I1).
173200
173300     IF (HR04F7-LINE-FC (I1) NOT = SPACES)
173400         GO TO 610-END.
173500
173600     IF (HR04F7-FC = "-")
173700         PERFORM 870-FIND-PREV-PCOSET1
173800     ELSE
173900         PERFORM 860-FIND-NEXT-PCOSET1.
174000
174100 610-END.
174200
174300******************************************************************
174400 HR04S7-TRANSACTION-END.
174500******************************************************************
174600******************************************************************
174700 HR04S8-TRANSACTION              SECTION 10.
174800******************************************************************
174900 HR04S8-START.
175000
      *  This form will no longer be used to define Requisition Status
      *  code (HR83.8 will be used instead).  This form number is
      *  available for a different use now.

175100     PERFORM 200-EDIT-TRAN
175200     THRU    200-END.
175300
175400     IF (NO-ERROR-FOUND)
175500         PERFORM 400-PROCESS-TRAN
175600         THRU    400-END.
175700
175800     GO TO HR04S8-TRANSACTION-END.
175900
176000******************************************************************
176100 200-EDIT-TRAN.
176200******************************************************************
176300
176400     PERFORM 210-EDIT-ACCESS
176500     THRU    210-END.
176600
176700     IF (ERROR-FOUND)
176800         GO TO 200-END.
176900
177000     IF (HR04F8-FC = "A" OR "C" OR "D")
177100         PERFORM 230-EDIT-DATA
177200         THRU    230-END
177300         GO TO 200-END.
177400
177500 200-END.
177600
177700******************************************************************
177800 210-EDIT-ACCESS.
177900******************************************************************
178000
178100     MOVE HR04F8-PCO-TYPE         TO DB-TYPE.
178200
178300     IF (HR04F8-FC NOT = "I" AND "+" AND "-")
178400         GO TO 210-END.
178500
178600     IF (HR04F8-FC          = "I")
178700     OR (HR04F8-PT-PCO-TYPE NOT = HR04F8-PCO-TYPE)
178800         MOVE SPACES              TO DB-CODE
178900     ELSE
179000     IF (HR04F8-FC = "+")
179100         MOVE HR04F8-PT-PCO-CODE  TO DB-CODE
179200     ELSE
179300         MOVE HR04F8-PCO-CODE (1) TO DB-CODE.
179400
179500     PERFORM 850-FIND-NLT-PCOSET1.
179600     IF (HR04F8-FC = "-")
179700         PERFORM 870-FIND-PREV-PCOSET1.
179800
179900     IF (PCODES-NOTFOUND)
180000     OR (PCO-TYPE NOT = DB-TYPE)
180100         INITIALIZE HR04F8-DETAIL-GROUP
180200         MOVE 11                  TO CRT-ERROR-NBR
180300         MOVE HR04F8-FC-FN        TO CRT-FIELD-NBR
180400         INITIALIZE                  HR04F8-PT-PCO-CODE
180500         GO TO 210-END.
180600
180700 210-END.
180800
180900******************************************************************
181000 230-EDIT-DATA.
181100******************************************************************
181200
181300     PERFORM 260-EDIT-DTL-TRAN
181400     THRU    260-END
181500         VARYING I1 FROM 1 BY 1
181600         UNTIL  (I1 > 10)
181700         OR     (ERROR-FOUND).
181800
181900     IF (ERROR-FOUND)
182000         GO TO 230-END.
182100
182200 230-END.
182300
182400******************************************************************
182500 260-EDIT-DTL-TRAN.
182600******************************************************************
182700
182800     IF (HR04F8-LINE-FC (I1) = SPACES)
182900         GO TO 260-END.
183000
183100     PERFORM 520-MOVE-DTL-SCR-TO-WS
183200     THRU    520-END.
183300
183400     PERFORM 2000-HRPCO-EDIT-TRAN.
183500
183600     IF (ERROR-FOUND)
183700         GO TO 260-END.
183800
183900 260-END.
184000
184100******************************************************************
184200 400-PROCESS-TRAN.
184300******************************************************************
184400
184500     IF (HR04F8-FC = "A" OR "C")
184600         PERFORM 910-AUDIT-BEGIN
184700
184800         IF (DMS-ABORTED)
184900             GO TO 400-END
185000         END-IF
185100
185200         PERFORM 420-PROCESS-DETAIL
185300         THRU    420-END
185400             VARYING I1 FROM 1 BY 1
185500             UNTIL  (I1 > 10)
185600
185700         PERFORM 920-AUDIT-END
185800         IF (HR04F8-FC = "A")
185900             MOVE CRT-ADD-COMPLETE   TO CRT-MESSAGE
186000         END-IF
186100         IF (HR04F8-FC = "C")
186200             MOVE CRT-CHG-COMPLETE   TO CRT-MESSAGE
186300         END-IF
186400     ELSE
186500     IF (HR04F8-FC = "I" OR "-" OR "+")
186600         PERFORM 480-INQUIRE
186700         THRU    480-END.
186800
186900 400-END.
187000
187100******************************************************************
187200 420-PROCESS-DETAIL.
187300******************************************************************
187400
187500     IF (HR04F8-LINE-FC (I1) = SPACES)
187600         GO TO 420-END.
187700
187800     PERFORM 520-MOVE-DTL-SCR-TO-WS
187900     THRU    520-END.
188000
188100     PERFORM 3000-HRPCO-PROCESS-TRAN.
188200
188300     IF (HR04F8-LINE-FC (I1) = "A" OR "C")
188400         PERFORM 610-MOVE-DTL-TO-SCREEN
188500         THRU    610-END
188600         INITIALIZE HR04F8-LINE-FC     (I1)
188700     ELSE
188800         INITIALIZE HR04F8-DETAIL-LINE (I1).
188900
189000 420-END.
189100
189200******************************************************************
189300 480-INQUIRE.
189400******************************************************************
189500
189600     IF (HR04F8-FC = "I")
189700         MOVE "+"                  TO HR04F8-FC.
189800
189900     IF (HR04F8-FC = "-")
190000         MOVE HR04F8-PCO-CODE (1)  TO HR04F8-PT-PCO-CODE
190100         INITIALIZE HR04F8-DETAIL-GROUP
190200         PERFORM 610-MOVE-DTL-TO-SCREEN
190300         THRU    610-END
190400             VARYING I1 FROM 10 BY NEGATIVE-ONE
190500             UNTIL  (I1       < 1)
190600             OR     (PCODES-NOTFOUND)
190700             OR     (PCO-TYPE NOT = HR04F8-PCO-TYPE)
190800
190900         IF (I1 NOT < 1)
191000             COMPUTE I9              = I1 + 1
191100             MOVE 1                  TO I1
191200             PERFORM
191300                 UNTIL (I9 > 10)
191400                     MOVE HR04F8-DETAIL-LINE (I9)
191500                                     TO HR04F8-DETAIL-LINE (I1)
191600                     INITIALIZE         HR04F8-DETAIL-LINE (I9)
191700                     ADD 1           TO I1
191800                                        I9
191900             END-PERFORM
192000             MOVE "+"                TO HR04F8-FC
192100             MOVE HR04F8-PT-PCO-CODE TO DB-CODE
192200             PERFORM 850-FIND-NLT-PCOSET1
192300             PERFORM 610-MOVE-DTL-TO-SCREEN
192400             THRU    610-END
192500                 VARYING I1 FROM I1 BY 1
192600                 UNTIL  (I1 > 10)
192700                 OR     (PCODES-NOTFOUND)
192800                 OR     (PCO-TYPE NOT = HR04F8-PCO-TYPE)
192900
193000             IF (PCODES-NOTFOUND)
193100             OR (PCO-TYPE NOT = HR04F8-PCO-TYPE)
193200                 MOVE SPACES         TO HR04F8-PT-PCO-CODE
193300             ELSE
193400                 MOVE PCO-CODE       TO HR04F8-PT-PCO-CODE
193500             END-IF
193600         END-IF
193700
193800         IF (PCODES-NOTFOUND)
193900         OR (PCO-TYPE NOT = HR04F8-PCO-TYPE)
194000             MOVE CRT-INQ-COMPLETE TO CRT-MESSAGE
194100         ELSE
194200             MOVE CRT-MORE-RECS    TO CRT-MESSAGE
194300     ELSE
194400         INITIALIZE HR04F8-DETAIL-GROUP
194500         PERFORM 610-MOVE-DTL-TO-SCREEN
194600         THRU    610-END
194700             VARYING I1 FROM 1 BY 1
194800             UNTIL  (I1       > 10)
194900             OR     (PCODES-NOTFOUND)
195000             OR     (PCO-TYPE NOT = HR04F8-PCO-TYPE)
195100
195200         IF (PCODES-NOTFOUND)
195300         OR (PCO-TYPE NOT = HR04F8-PCO-TYPE)
195400             MOVE SPACES           TO HR04F8-PT-PCO-CODE
195500             MOVE CRT-INQ-COMPLETE TO CRT-MESSAGE
195600         ELSE
195700             MOVE PCO-CODE         TO HR04F8-PT-PCO-CODE
195800             MOVE CRT-MORE-RECS    TO CRT-MESSAGE.
195900
196000 480-END.
196100
196200******************************************************************
196300 520-MOVE-DTL-SCR-TO-WS.
196400******************************************************************
196500
196600     INITIALIZE HRPCO-SCR-FIELDS.
196700
196800     MOVE HR04F8-PCO-TYPE                TO HRPCO-TYPE.
196900     MOVE HR04F8-PCO-TYPE-FN             TO HRPCO-TYPE-FN.
197000
197100     MOVE HR04F8-LINE-FC (I1)            TO HRPCO-FC.
197200     MOVE HR04F8-LINE-FC-FN (I1)         TO HRPCO-FC-FN.
197300     MOVE HR04F8-PCO-CODE (I1)           TO HRPCO-CODE.
197400     MOVE HR04F8-PCO-CODE-FN (I1)        TO HRPCO-CODE-FN.
197500     MOVE HR04F8-PCO-DESCRIPTION (I1)    TO HRPCO-DESCRIPTION.
197600     MOVE HR04F8-PCO-DESCRIPTION-FN (I1) TO HRPCO-DESCRIPTION-FN.
197700     MOVE HR04F8-PCO-ACTIVE-FLAG (I1)    TO HRPCO-ACTIVE-FLAG.
197800     MOVE HR04F8-PCO-ACTIVE-FLAG-FN (I1) TO HRPCO-ACTIVE-FLAG-FN.
197900     MOVE HR04F8-PCO-COUNT (I1)          TO HRPCO-COUNT.
198000     MOVE HR04F8-PCO-COUNT-FN (I1)       TO HRPCO-COUNT-FN.
198100
198200 520-END.
198300
198400******************************************************************
198500 610-MOVE-DTL-TO-SCREEN.
198600******************************************************************
198700
198800     MOVE PCO-CODE               TO HR04F8-PCO-CODE (I1).
198900     MOVE PCO-DESCRIPTION        TO HR04F8-PCO-DESCRIPTION (I1).
199000     MOVE PCO-COUNT              TO HR04F8-PCO-COUNT (I1).
199100     MOVE PCO-ACTIVE-FLAG        TO HR04F8-PCO-ACTIVE-FLAG (I1).
199200
199300     IF (HR04F8-LINE-FC (I1) NOT = SPACES)
199400         GO TO 610-END.
199500
199600     IF (HR04F8-FC = "-")
199700         PERFORM 870-FIND-PREV-PCOSET1
199800     ELSE
199900         PERFORM 860-FIND-NEXT-PCOSET1.
200000
200100 610-END.
200200
200300******************************************************************
200400 HR04S8-TRANSACTION-END.
200500******************************************************************
174600******************************************************************
174700 HR04S9-TRANSACTION              SECTION 10.
174800******************************************************************
174900 HR04S9-START.
175000
           PERFORM 5000-HRDICT-INCLUDE.

175100     PERFORM 200-EDIT-TRAN
175200     THRU    200-END.
175300
175400     IF (NO-ERROR-FOUND)
175500         PERFORM 400-PROCESS-TRAN
175600         THRU    400-END.
175700
175800     GO TO HR04S9-TRANSACTION-END.
175900
176000******************************************************************
176100 200-EDIT-TRAN.
176200******************************************************************
176300
176400     PERFORM 210-EDIT-ACCESS
176500     THRU    210-END.
176600
176700     IF (ERROR-FOUND)
176800         GO TO 200-END.
176900
           IF  (HR04F9-FC = "A" OR "C")
177100         PERFORM 230-EDIT-DATA
177200         THRU    230-END.

           IF  (HR04F9-FC = "D")
               PERFORM 240-EDIT-DELETE
               THRU    240-END.
177400
177500 200-END.
177600
177700******************************************************************
177800 210-EDIT-ACCESS.
177900******************************************************************
178000
178100     MOVE HR04F9-PCO-TYPE         TO DB-TYPE.
           MOVE HR04F9-PCO-CODE         TO DB-CODE.
178200
           IF (HR04F9-FC = "N")
               PERFORM 850-FIND-NLT-PCOSET1
               IF  (PCODES-FOUND)
               AND (PCO-TYPE        = HR04F9-PCO-TYPE)
               AND (PCO-CODE        = HR04F9-PCO-CODE)
                   PERFORM 860-FIND-NEXT-PCOSET1
               END-IF
           ELSE
           IF (HR04F9-FC = "P")
               PERFORM 850-FIND-NLT-PCOSET1
               PERFORM 870-FIND-PREV-PCOSET1.

           IF (HR04F9-FC = "N" OR "P")
               IF (PCODES-NOTFOUND)
               OR (PCO-TYPE NOT = HR04F9-PCO-TYPE)
                   MOVE 12                  TO CRT-ERROR-NBR
                   MOVE HR04F9-FC-FN        TO CRT-FIELD-NBR
                   GO TO 210-END
               ELSE
                   MOVE PCO-CODE            TO HR04F9-PCO-CODE
                                               DB-CODE
                   PERFORM 840-FIND-PDDSET1
                   GO TO 210-END.

           PERFORM 840-FIND-PCOSET1.
           PERFORM 840-FIND-PDDSET1.

           IF  (HR04F9-FC = "D" OR "I")
           AND (PCODES-NOTFOUND)
180200         MOVE 10                                 TO CRT-ERROR-NBR
180300         MOVE HR04F9-PCO-CODE-FN                 TO CRT-FIELD-NBR
180400         INITIALIZE                  HR04F9-PT-PCO-CODE
                                           HR04F9-PDD-CONTACT-LAST
                                           HR04F9-PDD-CONTACT-FIRST
                                           HR04F9-PDD-CONTACT-MI
                                           HR04F9-PDD-ADDR1
                                           HR04F9-PDD-ADDR2
                                           HR04F9-PDD-ADDR3
                                           HR04F9-PDD-ADDR4
                                           HR04F9-PDD-CITY
                                           HR04F9-PDD-STATE
                                           HR04F9-PDD-ZIP
                                           HR04F9-PDD-COUNTY
                                           HR04F9-PDD-COUNTRY-CODE
                                           HR04F9-PDD-PHONE-COUNTRY
                                           HR04F9-PDD-PHONE
                                           HR04F9-PDD-PHONE-EXT
                                           HR04F9-PDD-FAX-COUNTRY
                                           HR04F9-PDD-FAX-NUMBER
                                           HR04F9-PDD-FAX-EXT
                                           HR04F9-PDD-EMAIL-ADDRESS
                                           HR04F9-PDD-USER1
                                           HR04F9-PDD-USER2
                                           HR04F9-PDD-NUMERIC1
                                           HR04F9-PDD-NUMERIC2
                                           HR04F9-PDD-CMA
                                           HR04F9-PDD-DESC
                                           HR04F9-PDD-SUPP-ADDR1
                                           HR04F9-PDD-SUPP-ADDR2
                                           HR04F9-PDD-SUPP-ADDR3
                                           HR04F9-PDD-SUPP-ADDR4
                                           HR04F9-PDD-SUPP-CITY
                                           HR04F9-PDD-SUPP-STATE
                                           HR04F9-PDD-SUPP-ZIP
                                           HR04F9-PDD-SUPP-CNTRY-CODE 
180500         GO TO 210-END.

           IF  (HR04F9-FC = "C")
           AND (PCODES-NOTFOUND)
180200         MOVE 118                                TO CRT-ERROR-NBR
180300         MOVE HR04F9-FC-FN                       TO CRT-FIELD-NBR
180500         GO TO 210-END.

           IF (HR04F9-FC = "A")
               IF (PCODES-FOUND)
180200             MOVE 117                 TO CRT-ERROR-NBR
180300             MOVE HR04F9-FC-FN        TO CRT-FIELD-NBR
180500             GO TO 210-END.
180600
180700 210-END.
180800
180900******************************************************************
181000 230-EDIT-DATA.
181100******************************************************************
181200
181300     IF (HR04F9-PDD-COUNTRY-CODE NOT = SPACES)
               MOVE HR04F9-PDD-COUNTRY-CODE        TO DB-COUNTRY-CODE
               PERFORM 840-FIND-INTSET1
               IF (INSTCTRYCD-NOTFOUND)
                   MOVE 116                        TO CRT-ERROR-NBR
                   MOVE HR04F9-PDD-COUNTRY-CODE-FN TO CRT-FIELD-NBR
                   GO TO 230-END.
181800
           IF (HR04F9-PDD-CMA          NOT = SPACES)
      * CMA only valid when Type = LO
               IF (HR04F9-PCO-TYPE     NOT = "LO")
                   MOVE 124                TO CRT-ERROR-NBR
                   MOVE HR04F9-PDD-CMA-FN  TO CRT-FIELD-NBR
                   GO TO 230-END
               ELSE
      * Census Metropolitan Area does not exist
                   MOVE "CA"                   TO DB-TYPE
                   MOVE HR04F9-PDD-CMA         TO DB-CODE
                   PERFORM 840-FIND-PCOSET1
                   IF (PCODES-NOTFOUND)
                       MOVE 122                TO CRT-ERROR-NBR
                       MOVE HR04F9-PDD-CMA-FN  TO CRT-FIELD-NBR
                       GO TO 230-END
                   ELSE
      * CMA code is inactive
                   IF (PCO-ACTIVE-FLAG     = "I")
                       MOVE 123                TO CRT-ERROR-NBR
                       MOVE HR04F9-PDD-CMA-FN  TO CRT-FIELD-NBR
                       GO TO 230-END.

           IF (HR04F9-PDD-REPORT-PROV      NOT = SPACES)
      * Report Province only valid when Type = LO
               IF (HR04F9-PCO-TYPE     NOT = "LO")
                   MOVE 132                TO CRT-ERROR-NBR
                   MOVE HR04F9-PDD-REPORT-PROV-FN
                                           TO CRT-FIELD-NBR
                   GO TO 230-END
               ELSE
      * Report Province does not exist
                   MOVE "RP"                   TO DB-TYPE
                   MOVE HR04F9-PDD-REPORT-PROV TO DB-CODE
                   PERFORM 840-FIND-PCOSET1
                   IF (PCODES-NOTFOUND)
                       MOVE 134                TO CRT-ERROR-NBR
                       MOVE HR04F9-PDD-REPORT-PROV-FN
                                               TO CRT-FIELD-NBR
                       GO TO 230-END
                   ELSE
      * Report Province is inactive
                   IF (PCO-ACTIVE-FLAG     NOT = "A")
                       MOVE "LO"                   TO DB-TYPE
                       MOVE HR04F9-PCO-CODE        TO DB-CODE
                       PERFORM 840-FIND-PCOSET1
                       IF  (PCODES-FOUND)
                       AND (PCO-ACTIVE-FLAG = "A")
                           MOVE 133            TO CRT-ERROR-NBR
                           MOVE HR04F9-PDD-REPORT-PROV-FN
                                               TO CRT-FIELD-NBR
                           GO TO 230-END
                       END-IF
                   END-IF.

           SET NO-PR113 TO TRUE.
           IF  (HR04F9-PCO-TYPE = "LO")
               IF  (HR04F9-PDD-COUNTRY-CODE = SPACES)
                   MOVE 125                            TO CRT-ERROR-NBR
                   MOVE HR04F9-PDD-COUNTRY-CODE-FN     TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF
               IF  (HR04F9-PDD-COUNTRY-CODE = HRWS-US-WORK-COUNTRY)
                   IF  (HR04F9-PDD-ADDR1 = SPACES)
                       MOVE 126                        TO CRT-ERROR-NBR
                       MOVE HR04F9-PDD-ADDR1-FN        TO CRT-FIELD-NBR
                       GO TO 230-END
                   END-IF
                   IF  (HR04F9-PDD-CITY = SPACES)
                       MOVE 126                        TO CRT-ERROR-NBR
                       MOVE HR04F9-PDD-CITY-FN         TO CRT-FIELD-NBR
                       GO TO 230-END
                   END-IF
                   IF  (HR04F9-PDD-STATE = SPACES)
                       MOVE 126                        TO CRT-ERROR-NBR
                       MOVE HR04F9-PDD-STATE-FN        TO CRT-FIELD-NBR
                       GO TO 230-END
                   END-IF
                   IF  (HR04F9-PDD-ZIP = SPACES)
                       MOVE 126                        TO CRT-ERROR-NBR
                       MOVE HR04F9-PDD-ZIP-FN          TO CRT-FIELD-NBR
                       GO TO 230-END
                   END-IF
                   IF  (HR04F9-PDD-COUNTY = SPACES)
                       MOVE 126                        TO CRT-ERROR-NBR
                       MOVE HR04F9-PDD-COUNTY-FN       TO CRT-FIELD-NBR
                       GO TO 230-END
                   END-IF
                   IF  (HR04F9-FC = "C")
                       IF  (PCODESDTL-NOTFOUND)
                       OR  (HR04F9-PDD-ADDR1  NOT = PDD-ADDR1)
                       OR  (HR04F9-PDD-ADDR2  NOT = PDD-ADDR2)
                       OR  (HR04F9-PDD-CITY   NOT = PDD-CITY)
                       OR  (HR04F9-PDD-STATE  NOT = PDD-STATE)
                       OR  (HR04F9-PDD-ZIP    NOT = PDD-ZIP)
                       OR  (HR04F9-PDD-COUNTY NOT = PDD-COUNTY)
                       OR  (HR04F9-PDD-COUNTRY-CODE
                                              NOT = PDD-COUNTRY-CODE)
                           MOVE HRPEM-LOCAT-CODE-DN    TO DB-FLD-NBR
                           MOVE HR04F9-PCO-CODE        TO DB-FLD-VALUE
                           MOVE PTFSET4-FLD-VALUE      TO WS-DB-BEG-RNG
                           PERFORM 850-FIND-BEGRNG-PTFSET4
                           IF  (PATHFIND-FOUND)
                               SET PR113-NEEDED TO TRUE
                           END-IF
                       END-IF
                   END-IF
               END-IF
P46804         IF  (HR04F9-PCO-ACTIVE-FLAG = "I")
P46804             MOVE "(TRD-LOCAT-CODE = ?)" TO FILTER-STRING
P46804             PERFORM 890-CREATE-FILTER
P46804             MOVE HR04F9-PCO-CODE        TO ALPHANUM-FILTER-VALUE 
P46804             PERFORM 890-SET-ALPHANUM-FILTER-VALUE
P46804
P46804             PERFORM 850-FILTER-NLT-TRDSET1
P46804
P46804             PERFORM
P46804               UNTIL (TIMERECORD-NOTFOUND)
P46804                 OR (TRD-LOCAT-CODE = HR04F9-PCO-CODE)
P46804                PERFORM 860-FIND-NEXT-TRDSET1
P46804             END-PERFORM
P46804
P46804             IF  (TRD-LOCAT-CODE = HR04F9-PCO-CODE)
P46804                 MOVE 141                TO CRT-ERROR-NBR
P46804                 MOVE HR04F9-FC-FN       TO CRT-FIELD-NBR
P46804                 GO TO 230-END
P46804             END-IF
P46804         END-IF
           END-IF.

           MOVE SPACES TO HR04F9-PDD-DESCRIPTION1
                          HR04F9-PDD-DESCRIPTION2.

           IF    (HR04F9-PDD-COUNTRY-CODE  NOT = SPACES)
           AND  ((HR04F9-PCO-TYPE          = "LO")
           OR    ((HR04F9-PCO-TYPE         NOT = "LO")
           AND    (HR04F9-PDD-STATE        NOT = SPACES)))
               IF (HR04F9-PDD-COUNTRY-CODE = HRWS-US-WORK-COUNTRY)
                   MOVE HR04F9-PDD-STATE   TO DB-STATE
                   PERFORM 840-FIND-PSASET1
                   IF  (PRSTATE-NOTFOUND)
                       MOVE 130                        TO CRT-ERROR-NBR
                       MOVE HR04F9-PDD-STATE-FN        TO CRT-FIELD-NBR
                       GO TO 230-END
                   ELSE
                       MOVE PSA-DESCRIPTION            TO
                                                HR04F9-PDD-DESCRIPTION1
                   END-IF
               ELSE
               IF (HR04F9-PDD-COUNTRY-CODE = HRWS-CA-WORK-COUNTRY)
                   MOVE HR04F9-PDD-STATE TO DB-PROVINCE
                   PERFORM 840-FIND-PPVSET1
                   IF (PRPROVINCE-NOTFOUND)
                       MOVE 140                         TO CRT-ERROR-NBR
                       MOVE HR04F9-PDD-STATE-FN         TO CRT-FIELD-NBR
                       GO TO 230-END
                   ELSE
                       MOVE PPV-DESCRIPTION            TO
                                                HR04F9-PDD-DESCRIPTION1
                   END-IF
               END-IF.
128800
           IF (HR04F9-PDD-SUPP-CNTRY-CODE NOT = SPACES)
               MOVE HR04F9-PDD-SUPP-CNTRY-CODE      
                                              TO DB-COUNTRY-CODE
               PERFORM 840-FIND-INTSET1
               IF (INSTCTRYCD-NOTFOUND)
                   MOVE 131                   TO CRT-ERROR-NBR
                   MOVE HR04F9-PDD-SUPP-CNTRY-CODE-FN TO CRT-FIELD-NBR
                       GO TO 230-END.

           IF    (HR04F9-PDD-SUPP-CNTRY-CODE  NOT = SPACES)
           AND  ((HR04F9-PCO-TYPE             = "LO")
           OR    ((HR04F9-PCO-TYPE            NOT = "LO")
           AND    (HR04F9-PDD-STATE           NOT = SPACES)))
               IF (HR04F9-PDD-SUPP-CNTRY-CODE = HRWS-US-WORK-COUNTRY)
                   MOVE HR04F9-PDD-SUPP-STATE   TO DB-STATE
                   PERFORM 840-FIND-PSASET1
                   IF  (PRSTATE-NOTFOUND)
                       MOVE 130                        TO CRT-ERROR-NBR
                       MOVE HR04F9-PDD-SUPP-STATE-FN   TO CRT-FIELD-NBR
                       GO TO 230-END
                   ELSE
                       MOVE PSA-DESCRIPTION            TO
                                                HR04F9-PDD-DESCRIPTION2
                   END-IF
               ELSE
               IF (HR04F9-PDD-SUPP-CNTRY-CODE = HRWS-CA-WORK-COUNTRY)
                   MOVE HR04F9-PDD-SUPP-STATE TO DB-PROVINCE
                   PERFORM 840-FIND-PPVSET1
                   IF (PRPROVINCE-NOTFOUND)
                       MOVE 140                        TO CRT-ERROR-NBR
                       MOVE HR04F9-PDD-SUPP-STATE-FN   TO CRT-FIELD-NBR
                       GO TO 230-END
                   ELSE
                       MOVE PPV-DESCRIPTION            TO
                                                HR04F9-PDD-DESCRIPTION2
                   END-IF
               END-IF.
128800
182200 230-END.
182300
180900******************************************************************
181000 240-EDIT-DELETE.
181100******************************************************************
181200
           IF (HR04F9-PCO-TYPE = "LO")
               MOVE HRPEM-LOCAT-CODE-DN        TO DB-FLD-NBR
               MOVE HR04F9-PCO-CODE            TO DB-FLD-VALUE
               MOVE PTFSET4-FLD-VALUE          TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PTFSET4
               IF  (PATHFIND-FOUND)
                   MOVE 128                            TO CRT-ERROR-NBR
                   MOVE HR04F9-FC-FN                   TO CRT-FIELD-NBR
                   GO TO 240-END
               END-IF
               INITIALIZE DB-COMPANY
                          DB-PROCESS-LEVEL
               PERFORM 850-FIND-NLT-PRSSET1
               PERFORM 241-EDIT-DELETE-PRS
               THRU    241-END
                   UNTIL (PRSYSTEM-NOTFOUND)
                   OR    (ERROR-FOUND).

           IF (HR04F9-PCO-TYPE = "UN")
               MOVE HREMP-UNION-CODE-DN        TO DB-FLD-NBR
               MOVE HR04F9-PCO-CODE            TO DB-FLD-VALUE
               MOVE PTFSET4-FLD-VALUE          TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PTFSET4
               IF  (PATHFIND-FOUND)
                   MOVE 137                        TO CRT-ERROR-NBR
                   MOVE HR04F9-FC-FN                   TO CRT-FIELD-NBR
                   GO TO 240-END
               END-IF.

182200 240-END.
182300
180900******************************************************************
181000 241-EDIT-DELETE-PRS.
181100******************************************************************
181200
           IF  (PRS-PROCESS-LEVEL NOT = SPACES)
               GO TO 241-NEXT.

           MOVE PRS-COMPANY                TO DB-COMPANY.
           MOVE HR04F9-PCO-CODE            TO DB-LOCAT-CODE.
           MOVE PXLSET2-LOCAT-CODE         TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PXLSET2.
           IF  (PRTAXLOC-FOUND)
               MOVE 129                                TO CRT-ERROR-NBR
               MOVE HR04F9-FC-FN                       TO CRT-FIELD-NBR
               GO TO 241-END.

       241-NEXT.
           PERFORM 860-FIND-NEXT-PRSSET1.

182200 241-END.
182300
184100******************************************************************
184200 400-PROCESS-TRAN.
184300******************************************************************
184400
184500     IF (HR04F9-FC = "A")
184600         PERFORM 410-ADD
184600         THRU    410-END
184600     ELSE
           IF (HR04F9-FC = "C")
               PERFORM 420-CHANGE
               THRU    420-END
           ELSE
           IF (HR04F9-FC = "D")
               PERFORM 430-DELETE
               THRU    430-END
           ELSE
               PERFORM 480-INQUIRE
               THRU    480-END.
186800
186900 400-END.
187000
187100******************************************************************
187200 410-ADD.
187300******************************************************************
187400
187500     PERFORM 910-AUDIT-BEGIN.
187600     IF (DMS-ABORTED)
187600         GO TO 410-END
           END-IF.
187700
           PERFORM 800-CREATE-PCODES.

           MOVE HR04F9-PCO-TYPE            TO DB-TYPE.
           MOVE HR04F9-PCO-CODE            TO DB-CODE.

           PERFORM 510-MOVE-PCO-DATA
           THRU    510-END.
           PERFORM 820-STORE-PCODES.

           SET HR04WS-PCODESDTL-NOCHG   TO TRUE.
           PERFORM 440-CHECK-FOR-CHANGE
           THRU    440-END.
           IF (HR04WS-PCODESDTL-CHG)
               PERFORM 415-ADD-PCODESDTL
               THRU    415-END.

           PERFORM 920-AUDIT-END.

           PERFORM 840-FIND-PDDSET1.
           PERFORM 600-MOVE-TO-SCREEN
           THRU    600-END.

           MOVE CRT-ADD-COMPLETE       TO CRT-MESSAGE.

189000 410-END.
189100
187100******************************************************************
187200 415-ADD-PCODESDTL.
187300******************************************************************
187400
187800     PERFORM 800-CREATE-PCODESDTL.
188000     PERFORM 500-MOVE-DATA
           THRU    500-END.
           PERFORM 820-STORE-PCODESDTL.

189000 415-END.
189100
187100******************************************************************
187200 420-CHANGE.
187300******************************************************************
187400
187500     PERFORM 910-AUDIT-BEGIN.
187600     IF (DMS-ABORTED)
187600         GO TO 420-END
           END-IF.
187700
           MOVE HR04F9-PCO-TYPE        TO DB-TYPE.
           MOVE HR04F9-PCO-CODE        TO DB-CODE.

           IF (HR04F9-PCO-DESCRIPTION NOT = PCO-DESCRIPTION)
           OR (HR04F9-PCO-ACTIVE-FLAG NOT = PCO-ACTIVE-FLAG)
               PERFORM 840-MODIFY-PCOSET1
               PERFORM 510-MOVE-PCO-DATA
               THRU    510-END
               PERFORM 820-STORE-PCODES.

           SET HR04WS-PCODESDTL-NOCHG   TO TRUE.
           PERFORM 440-CHECK-FOR-CHANGE
           THRU    440-END.
           IF (HR04WS-PCODESDTL-CHG)
           OR (HR04WS-PCODESDTL-NOVAL)
               PERFORM 425-CHANGE-PCODESDTL
               THRU    425-END
J33061     ELSE
J33061         IF (HR04WS-PCODESDTL-KCHG)
J33061             PERFORM 426-KCHANGE-PCODESDTL
J33061             THRU    426-END 
J33061         END-IF
J33061     END-IF.

           PERFORM 920-AUDIT-END.

           PERFORM 840-FIND-PDDSET1.
           PERFORM 600-MOVE-TO-SCREEN
           THRU    600-END.

           MOVE CRT-CHG-COMPLETE       TO CRT-MESSAGE.
           IF  (PR113-NEEDED)
               MOVE HR04F9-PCO-CODE    TO CRT-ERR-VAR1
               MOVE 127                TO CRT-MSG-NBR
               PERFORM 790-GET-MSG.

189000 420-END.
189100
187100******************************************************************
187200 425-CHANGE-PCODESDTL.
187300******************************************************************
187400
           IF  (HR04WS-PCODESDTL-CHG)
           AND (PCODESDTL-NOTFOUND)
               PERFORM 800-CREATE-PCODESDTL
           ELSE
           IF (PCODESDTL-FOUND)
               PERFORM 840-MODIFY-PDDSET1.

           IF (HR04WS-PCODESDTL-CHG)
               PERFORM 500-MOVE-DATA
               THRU    500-END.

           IF (PCODESDTL-FOUND)
               IF (HR04WS-PCODESDTL-NOVAL)
                   PERFORM 830-DELETE-PCODESDTL
               ELSE
                   PERFORM 820-STORE-PCODESDTL
               END-IF
           ELSE
               IF (HR04WS-PCODESDTL-NOVAL)
                   GO TO 425-END
               ELSE
                   PERFORM 820-STORE-PCODESDTL.

189000 425-END.
189100
J33061******************************************************************
J33061 426-KCHANGE-PCODESDTL.
J33061******************************************************************
J33061
J33061     MOVE HR04F9-PCO-TYPE        TO DB-TYPE.
J33061     MOVE HR04F9-PCO-CODE        TO DB-CODE.
J33061     PERFORM 840-MODIFY-PDDSET1.
J33061     PERFORM 830-DELETE-PCODESDTL.
J33061
J33061     PERFORM 800-CREATE-PCODESDTL.
J33061     PERFORM 500-MOVE-DATA
J33061     THRU    500-END.
J33061
J33061     PERFORM 820-STORE-PCODESDTL.
J33061
J33061 426-END.
J33061
187100******************************************************************
187200 430-DELETE.
187300******************************************************************
187400
187500     PERFORM 910-AUDIT-BEGIN.
187600     IF (DMS-ABORTED)
187600         GO TO 430-END
           END-IF.
187700
           MOVE HR04F9-PCO-TYPE        TO DB-TYPE.
           MOVE HR04F9-PCO-CODE        TO DB-CODE.

           PERFORM 840-MODIFY-PCOSET1.
           PERFORM 840-MODIFY-PDDSET1.

           PERFORM 830-DELETE-PCODES.
           IF (PCODESDTL-FOUND)
               PERFORM 830-DELETE-PCODESDTL.

           PERFORM 920-AUDIT-END.

           MOVE CRT-RECS-DELETED       TO CRT-MESSAGE.

189000 430-END.
189100
189200******************************************************************
189300 440-CHECK-FOR-CHANGE.
189400******************************************************************

197000     IF  (HR04F9-PDD-CONTACT-LAST      = SPACES)
197000     AND (HR04F9-PDD-CONTACT-FIRST     = SPACES)
197000     AND (HR04F9-PDD-CONTACT-MI        = SPACES)
197100     AND (HR04F9-PDD-ADDR1             = SPACES)
197100     AND (HR04F9-PDD-ADDR2             = SPACES)
197100     AND (HR04F9-PDD-ADDR3             = SPACES)
197100     AND (HR04F9-PDD-ADDR4             = SPACES)
           AND (HR04F9-PDD-CITY              = SPACES)
           AND (HR04F9-PDD-STATE             = SPACES)
           AND (HR04F9-PDD-ZIP               = SPACES)
           AND (HR04F9-PDD-COUNTY            = SPACES)
           AND (HR04F9-PDD-COUNTRY-CODE      = SPACES)
197200     AND (HR04F9-PDD-PHONE-COUNTRY     = SPACES)
197200     AND (HR04F9-PDD-PHONE             = SPACES)
197200     AND (HR04F9-PDD-PHONE-EXT         = SPACES)
197300     AND (HR04F9-PDD-FAX-COUNTRY       = SPACES)
197300     AND (HR04F9-PDD-FAX-NUMBER        = SPACES)
197300     AND (HR04F9-PDD-FAX-EXT           = SPACES)
           AND (HR04F9-PDD-EMAIL-ADDRESS     = SPACES)
197400     AND (HR04F9-PDD-USER1             = SPACES)
197400     AND (HR04F9-PDD-USER2             = SPACES)
           AND (HR04F9-PDD-NUMERIC1          = ZEROES)
           AND (HR04F9-PDD-NUMERIC2          = ZEROES)
           AND (HR04F9-PDD-CMA               = SPACES)
197100     AND (HR04F9-PDD-SUPP-ADDR1        = SPACES)
197100     AND (HR04F9-PDD-SUPP-ADDR2        = SPACES)
197100     AND (HR04F9-PDD-SUPP-ADDR3        = SPACES)
197100     AND (HR04F9-PDD-SUPP-ADDR4        = SPACES)
           AND (HR04F9-PDD-SUPP-CITY         = SPACES)
           AND (HR04F9-PDD-SUPP-STATE        = SPACES)
           AND (HR04F9-PDD-SUPP-ZIP          = SPACES)
           AND (HR04F9-PDD-SUPP-CNTRY-CODE   = SPACES)
           AND (HR04F9-PDD-REPORT-PROV       = SPACES)
               SET HR04WS-PCODESDTL-NOVAL    TO TRUE
               GO TO 440-END.

           IF (PCODESDTL-FOUND)
197000         IF (HR04F9-PDD-CONTACT-LAST     NOT = PDD-CONTACT-LAST)
197000         OR (HR04F9-PDD-CONTACT-FIRST    NOT = PDD-CONTACT-FIRST)
197000         OR (HR04F9-PDD-CONTACT-MI       NOT = PDD-CONTACT-MI)
197100         OR (HR04F9-PDD-ADDR1            NOT = PDD-ADDR1)
197100         OR (HR04F9-PDD-ADDR2            NOT = PDD-ADDR2)
197100         OR (HR04F9-PDD-ADDR3            NOT = PDD-ADDR3)
197100         OR (HR04F9-PDD-ADDR4            NOT = PDD-ADDR4)
               OR (HR04F9-PDD-CITY             NOT = PDD-CITY)
               OR (HR04F9-PDD-STATE            NOT = PDD-STATE)
               OR (HR04F9-PDD-ZIP              NOT = PDD-ZIP)
               OR (HR04F9-PDD-COUNTY           NOT = PDD-COUNTY)
               OR (HR04F9-PDD-COUNTRY-CODE     NOT = PDD-COUNTRY-CODE)
197200         OR (HR04F9-PDD-PHONE-COUNTRY    NOT = PDD-PHONE-COUNTRY)
197200         OR (HR04F9-PDD-PHONE            NOT = PDD-PHONE)
197200         OR (HR04F9-PDD-PHONE-EXT        NOT = PDD-PHONE-EXT)
197300         OR (HR04F9-PDD-FAX-COUNTRY      NOT = PDD-FAX-COUNTRY)
197300         OR (HR04F9-PDD-FAX-NUMBER       NOT = PDD-FAX-NUMBER)
197300         OR (HR04F9-PDD-FAX-EXT          NOT = PDD-FAX-EXT)
               OR (HR04F9-PDD-EMAIL-ADDRESS    NOT = PDD-EMAIL-ADDRESS)
197400         OR (HR04F9-PDD-USER1            NOT = PDD-USER1)
197400         OR (HR04F9-PDD-USER2            NOT = PDD-USER2)
               OR (HR04F9-PDD-NUMERIC1         NOT = PDD-NUMERIC1)
               OR (HR04F9-PDD-NUMERIC2         NOT = PDD-NUMERIC2)
               OR (HR04F9-PDD-CMA              NOT = PDD-CMA-CODE)
197100         OR (HR04F9-PDD-SUPP-ADDR1       NOT = PDD-SUPP-ADDR1)
197100         OR (HR04F9-PDD-SUPP-ADDR2       NOT = PDD-SUPP-ADDR2)
197100         OR (HR04F9-PDD-SUPP-ADDR3       NOT = PDD-SUPP-ADDR3)
197100         OR (HR04F9-PDD-SUPP-ADDR4       NOT = PDD-SUPP-ADDR4)
               OR (HR04F9-PDD-SUPP-CITY        NOT = PDD-SUPP-CITY)
               OR (HR04F9-PDD-SUPP-STATE       NOT = PDD-SUPP-STATE)
               OR (HR04F9-PDD-SUPP-ZIP         NOT = PDD-SUPP-ZIP)
               OR (HR04F9-PDD-SUPP-CNTRY-CODE 
                                          NOT = PDD-SUPP-CNTRY-CD)      
               OR (HR04F9-PDD-REPORT-PROV      NOT = PDD-REPORT-PROV)
                   SET HR04WS-PCODESDTL-CHG    TO TRUE
J33061             IF ((HR04F9-PDD-STATE        NOT = PDD-STATE)
J33061             OR  (HR04F9-PDD-COUNTRY-CODE NOT = PDD-COUNTRY-CODE))
J33061             AND (HR04F9-FC = "C")
J33061                 SET HR04WS-PCODESDTL-KCHG TO TRUE
J33061             END-IF
               END-IF
           ELSE
197000         IF (HR04F9-PDD-CONTACT-LAST     NOT = SPACES)
197000         OR (HR04F9-PDD-CONTACT-FIRST    NOT = SPACES)
197000         OR (HR04F9-PDD-CONTACT-MI       NOT = SPACES)
197100         OR (HR04F9-PDD-ADDR1            NOT = SPACES)
197100         OR (HR04F9-PDD-ADDR2            NOT = SPACES)
197100         OR (HR04F9-PDD-ADDR3            NOT = SPACES)
197100         OR (HR04F9-PDD-ADDR4            NOT = SPACES)
               OR (HR04F9-PDD-CITY             NOT = SPACES)
               OR (HR04F9-PDD-STATE            NOT = SPACES)
               OR (HR04F9-PDD-ZIP              NOT = SPACES)
               OR (HR04F9-PDD-COUNTY           NOT = SPACES)
               OR (HR04F9-PDD-COUNTRY-CODE     NOT = SPACES)
197200         OR (HR04F9-PDD-PHONE-COUNTRY    NOT = SPACES)
197200         OR (HR04F9-PDD-PHONE            NOT = SPACES)
197200         OR (HR04F9-PDD-PHONE-EXT        NOT = SPACES)
197300         OR (HR04F9-PDD-FAX-COUNTRY      NOT = SPACES)
197300         OR (HR04F9-PDD-FAX-NUMBER       NOT = SPACES)
197300         OR (HR04F9-PDD-FAX-EXT          NOT = SPACES)
               OR (HR04F9-PDD-EMAIL-ADDRESS    NOT = SPACES)
197400         OR (HR04F9-PDD-USER1            NOT = SPACES)
197400         OR (HR04F9-PDD-USER2            NOT = SPACES)
               OR (HR04F9-PDD-NUMERIC1         NOT = ZEROES)
               OR (HR04F9-PDD-NUMERIC2         NOT = ZEROES)
               OR (HR04F9-PDD-CMA              NOT = SPACES)
197100         OR (HR04F9-PDD-SUPP-ADDR1       NOT = SPACES)
197100         OR (HR04F9-PDD-SUPP-ADDR2       NOT = SPACES)
197100         OR (HR04F9-PDD-SUPP-ADDR3       NOT = SPACES)
197100         OR (HR04F9-PDD-SUPP-ADDR4       NOT = SPACES)
               OR (HR04F9-PDD-SUPP-CITY        NOT = SPACES)
               OR (HR04F9-PDD-SUPP-STATE       NOT = SPACES)
               OR (HR04F9-PDD-SUPP-ZIP         NOT = SPACES)
               OR (HR04F9-PDD-SUPP-CNTRY-CODE  NOT = SPACES)
               OR (HR04F9-PDD-REPORT-PROV      NOT = SPACES)
                   SET HR04WS-PCODESDTL-CHG    TO TRUE
J33061         END-IF
J33061     END-IF.

           IF  (PCODESDTL-FOUND)
           AND (HR04F9-FC = "C")
197100         IF  (HR04F9-PDD-ADDR1            NOT = PDD-ADDR1)
197100         OR  (HR04F9-PDD-ADDR2            NOT = PDD-ADDR2)
197100         OR  (HR04F9-PDD-ADDR3            NOT = PDD-ADDR3)
197100         OR  (HR04F9-PDD-ADDR4            NOT = PDD-ADDR4)
               OR  (HR04F9-PDD-CITY             NOT = PDD-CITY)
               OR  (HR04F9-PDD-STATE            NOT = PDD-STATE)
               OR  (HR04F9-PDD-ZIP              NOT = PDD-ZIP)
               OR  (HR04F9-PDD-COUNTRY-CODE     NOT = PDD-COUNTRY-CODE)
197100         AND ((HR04F9-PDD-SUPP-ADDR1      = PDD-SUPP-ADDR1)
197100         AND (HR04F9-PDD-SUPP-ADDR2       = PDD-SUPP-ADDR2)
197100         AND (HR04F9-PDD-SUPP-ADDR3       = PDD-SUPP-ADDR3)
197100         AND (HR04F9-PDD-SUPP-ADDR4       = PDD-SUPP-ADDR4)
               AND (HR04F9-PDD-SUPP-CITY        = PDD-SUPP-CITY)
               AND (HR04F9-PDD-SUPP-STATE       = PDD-SUPP-STATE)
               AND (HR04F9-PDD-SUPP-ZIP         = PDD-SUPP-ZIP)
               AND (HR04F9-PDD-SUPP-CNTRY-CODE  = PDD-SUPP-CNTRY-CD)
               AND (HR04F9-PDD-REPORT-PROV      = PDD-REPORT-PROV))
197100             IF  (HR04F9-PDD-SUPP-ADDR1   = PDD-ADDR1)
197100             AND (HR04F9-PDD-SUPP-ADDR2   = PDD-ADDR2)
197100             AND (HR04F9-PDD-SUPP-ADDR3   = PDD-ADDR3)
197100             AND (HR04F9-PDD-SUPP-ADDR4   = PDD-ADDR4)
                   AND (HR04F9-PDD-SUPP-CITY    = PDD-CITY)
                   AND (HR04F9-PDD-SUPP-STATE   = PDD-STATE)
                   AND (HR04F9-PDD-SUPP-ZIP     = PDD-ZIP)
                   AND (HR04F9-PDD-SUPP-CNTRY-CODE = PDD-COUNTRY-CODE)
                       MOVE HR04F9-PDD-ADDR1    TO HR04F9-PDD-SUPP-ADDR1
197100                 MOVE HR04F9-PDD-ADDR2    TO HR04F9-PDD-SUPP-ADDR2
197100                 MOVE HR04F9-PDD-ADDR3    TO HR04F9-PDD-SUPP-ADDR3
197100                 MOVE HR04F9-PDD-ADDR4    TO HR04F9-PDD-SUPP-ADDR4
                       MOVE HR04F9-PDD-CITY     TO HR04F9-PDD-SUPP-CITY
                       MOVE HR04F9-PDD-STATE    TO HR04F9-PDD-SUPP-STATE
                       MOVE HR04F9-PDD-ZIP      TO HR04F9-PDD-SUPP-ZIP
                       MOVE HR04F9-PDD-COUNTRY-CODE 
                                          TO HR04F9-PDD-SUPP-CNTRY-CODE
J33061                 IF (NOT HR04WS-PCODESDTL-KCHG)
                           SET HR04WS-PCODESDTL-CHG    TO TRUE
J33061                 END-IF
J33061             END-IF
J33061         END-IF
J33061     END-IF.
                 
197100         IF  (HR04F9-PDD-SUPP-ADDR1      = SPACES)
197100         AND (HR04F9-PDD-SUPP-ADDR2      = SPACES)
197100         AND (HR04F9-PDD-SUPP-ADDR3      = SPACES)
197100         AND (HR04F9-PDD-SUPP-ADDR4      = SPACES)
               AND (HR04F9-PDD-SUPP-CITY       = SPACES)
               AND (HR04F9-PDD-SUPP-STATE      = SPACES)
               AND (HR04F9-PDD-SUPP-ZIP        = SPACES)
               AND (HR04F9-PDD-SUPP-CNTRY-CODE = SPACES)
197100         AND ((HR04F9-PDD-ADDR1          NOT = SPACES)
197100         OR  (HR04F9-PDD-ADDR2           NOT = SPACES)
197100         OR  (HR04F9-PDD-ADDR3           NOT = SPACES)
197100         OR  (HR04F9-PDD-ADDR4           NOT = SPACES)
               OR  (HR04F9-PDD-CITY            NOT = SPACES)
               OR  (HR04F9-PDD-STATE           NOT = SPACES)
               OR  (HR04F9-PDD-ZIP             NOT = SPACES)
               OR  (HR04F9-PDD-COUNTRY-CODE    NOT = SPACES))
                   MOVE HR04F9-PDD-ADDR1       TO HR04F9-PDD-SUPP-ADDR1
197100             MOVE HR04F9-PDD-ADDR2       TO HR04F9-PDD-SUPP-ADDR2
197100             MOVE HR04F9-PDD-ADDR3       TO HR04F9-PDD-SUPP-ADDR3
197100             MOVE HR04F9-PDD-ADDR4       TO HR04F9-PDD-SUPP-ADDR4
                   MOVE HR04F9-PDD-CITY        TO HR04F9-PDD-SUPP-CITY
                   MOVE HR04F9-PDD-STATE       TO HR04F9-PDD-SUPP-STATE
                   MOVE HR04F9-PDD-ZIP         TO HR04F9-PDD-SUPP-ZIP
                   MOVE HR04F9-PDD-COUNTRY-CODE 
                                          TO HR04F9-PDD-SUPP-CNTRY-CODE
J33061             IF (NOT HR04WS-PCODESDTL-KCHG)
                       SET HR04WS-PCODESDTL-CHG    TO TRUE
J33061             END-IF
J33061         END-IF.

       440-END.

189200******************************************************************
189300 480-INQUIRE.
189400******************************************************************
189500
           MOVE HR04F9-PCO-TYPE        TO DB-TYPE.
           MOVE HR04F9-PCO-CODE        TO DB-CODE.
           PERFORM 840-FIND-PCOSET1.

190900     PERFORM 600-MOVE-TO-SCREEN
191000     THRU    600-END.
191100
191200     MOVE CRT-INQ-COMPLETE           TO CRT-MESSAGE.
195900
196000 480-END.
196100
196200******************************************************************
196300 500-MOVE-DATA.
196400******************************************************************
196500
196800     MOVE HR04F9-PCO-TYPE                TO PDD-TYPE.
196900     MOVE HR04F9-PCO-CODE                TO PDD-CODE.
197000     MOVE HR04F9-PDD-CONTACT-LAST        TO PDD-CONTACT-LAST.
197000     MOVE HR04F9-PDD-CONTACT-FIRST       TO PDD-CONTACT-FIRST.
197000     MOVE HR04F9-PDD-CONTACT-MI          TO PDD-CONTACT-MI.
197100     MOVE HR04F9-PDD-ADDR1               TO PDD-ADDR1.
197100     MOVE HR04F9-PDD-ADDR2               TO PDD-ADDR2.
197100     MOVE HR04F9-PDD-ADDR3               TO PDD-ADDR3.
197100     MOVE HR04F9-PDD-ADDR4               TO PDD-ADDR4.
           MOVE HR04F9-PDD-CITY                TO PDD-CITY.
           MOVE HR04F9-PDD-STATE               TO PDD-STATE.
           MOVE HR04F9-PDD-ZIP                 TO PDD-ZIP.
           MOVE HR04F9-PDD-COUNTY              TO PDD-COUNTY.
           MOVE HR04F9-PDD-COUNTRY-CODE        TO PDD-COUNTRY-CODE.
197200     MOVE HR04F9-PDD-PHONE-COUNTRY       TO PDD-PHONE-COUNTRY.
197200     MOVE HR04F9-PDD-PHONE               TO PDD-PHONE.
197200     MOVE HR04F9-PDD-PHONE-EXT           TO PDD-PHONE-EXT.
197300     MOVE HR04F9-PDD-FAX-COUNTRY         TO PDD-FAX-COUNTRY.
197300     MOVE HR04F9-PDD-FAX-NUMBER          TO PDD-FAX-NUMBER.
197300     MOVE HR04F9-PDD-FAX-EXT             TO PDD-FAX-EXT.
           MOVE HR04F9-PDD-EMAIL-ADDRESS       TO PDD-EMAIL-ADDRESS.
197400     MOVE HR04F9-PDD-USER1               TO PDD-USER1.
197400     MOVE HR04F9-PDD-USER2               TO PDD-USER2.
           MOVE HR04F9-PDD-NUMERIC1            TO PDD-NUMERIC1.
           MOVE HR04F9-PDD-NUMERIC2            TO PDD-NUMERIC2.
           MOVE WS-SYSTEM-DATE-YMD             TO PDD-DATE-STAMP.
           MOVE HHMMSS                         TO PDD-TIME-STAMP.
           MOVE CRT-USER-NAME                  TO PDD-USER-ID.
           MOVE HR04F9-PDD-CMA                 TO PDD-CMA-CODE.
197100     MOVE HR04F9-PDD-SUPP-ADDR1          TO PDD-SUPP-ADDR1.
197100     MOVE HR04F9-PDD-SUPP-ADDR2          TO PDD-SUPP-ADDR2.
197100     MOVE HR04F9-PDD-SUPP-ADDR3          TO PDD-SUPP-ADDR3.
197100     MOVE HR04F9-PDD-SUPP-ADDR4          TO PDD-SUPP-ADDR4.
           MOVE HR04F9-PDD-SUPP-CITY           TO PDD-SUPP-CITY.
           MOVE HR04F9-PDD-SUPP-STATE          TO PDD-SUPP-STATE.
           MOVE HR04F9-PDD-SUPP-ZIP            TO PDD-SUPP-ZIP.
           MOVE HR04F9-PDD-SUPP-CNTRY-CODE     TO PDD-SUPP-CNTRY-CD.
           MOVE HR04F9-PDD-REPORT-PROV         TO PDD-REPORT-PROV.
198100
198200 500-END.
198300
196200******************************************************************
196300 510-MOVE-PCO-DATA.
196400******************************************************************
196500
196800     MOVE HR04F9-PCO-TYPE                TO PCO-TYPE.
196900     MOVE HR04F9-PCO-CODE                TO PCO-CODE.
197000     MOVE HR04F9-PCO-DESCRIPTION         TO PCO-DESCRIPTION.
           MOVE HR04F9-PCO-ACTIVE-FLAG         TO PCO-ACTIVE-FLAG.
           INITIALIZE                             PCO-EDUC-LEVEL
                                                  PCO-COUNT.

198200 510-END.
198300
198400******************************************************************
198500 600-MOVE-TO-SCREEN.
198600******************************************************************
198700
198800     MOVE PCO-CODE               TO HR04F9-PCO-CODE.
198900     MOVE PCO-DESCRIPTION        TO HR04F9-PCO-DESCRIPTION.
199100     MOVE PCO-ACTIVE-FLAG        TO HR04F9-PCO-ACTIVE-FLAG.
199200
           MOVE HR04F9-PCO-TYPE        TO DB-TYPE.
           MOVE HR04F9-PCO-CODE        TO DB-CODE.
           PERFORM 840-FIND-PDDSET1.

           IF (PCODESDTL-NOTFOUND)
               INITIALIZE                 HR04F9-PDD-CONTACT-LAST
                                          HR04F9-PDD-CONTACT-FIRST
                                          HR04F9-PDD-CONTACT-MI
                                          HR04F9-PDD-ADDR1
                                          HR04F9-PDD-ADDR2
                                          HR04F9-PDD-ADDR3
                                          HR04F9-PDD-ADDR4
                                          HR04F9-PDD-CITY
                                          HR04F9-PDD-STATE
                                          HR04F9-PDD-ZIP
                                          HR04F9-PDD-COUNTY
                                          HR04F9-PDD-COUNTRY-CODE
                                          HR04F9-PDD-PHONE-COUNTRY
                                          HR04F9-PDD-PHONE
                                          HR04F9-PDD-PHONE-EXT
                                          HR04F9-PDD-FAX-COUNTRY
                                          HR04F9-PDD-FAX-NUMBER
                                          HR04F9-PDD-FAX-EXT
                                          HR04F9-PDD-EMAIL-ADDRESS
                                          HR04F9-PDD-USER1
                                          HR04F9-PDD-USER2
                                          HR04F9-PDD-NUMERIC1
                                          HR04F9-PDD-NUMERIC2
                                          HR04F9-PDD-CMA
                                          HR04F9-PDD-DESC
                                          HR04F9-PDD-SUPP-ADDR1
                                          HR04F9-PDD-SUPP-ADDR2
                                          HR04F9-PDD-SUPP-ADDR3
                                          HR04F9-PDD-SUPP-ADDR4
                                          HR04F9-PDD-SUPP-CITY
                                          HR04F9-PDD-SUPP-STATE
                                          HR04F9-PDD-SUPP-ZIP
                                          HR04F9-PDD-SUPP-CNTRY-CODE
                                          HR04F9-PDD-REPORT-PROV
                                          HR04F9-RPT-PROV-DESC
                                                  
               GO TO 600-END.

199700     MOVE PDD-CONTACT-LAST       TO HR04F9-PDD-CONTACT-LAST.
           MOVE PDD-CONTACT-FIRST      TO HR04F9-PDD-CONTACT-FIRST.
           MOVE PDD-CONTACT-MI         TO HR04F9-PDD-CONTACT-MI.
           MOVE PDD-ADDR1              TO HR04F9-PDD-ADDR1.
           MOVE PDD-ADDR2              TO HR04F9-PDD-ADDR2.
           MOVE PDD-ADDR3              TO HR04F9-PDD-ADDR3.
           MOVE PDD-ADDR4              TO HR04F9-PDD-ADDR4.
           MOVE PDD-CITY               TO HR04F9-PDD-CITY.
           MOVE PDD-STATE              TO HR04F9-PDD-STATE.
           MOVE PDD-ZIP                TO HR04F9-PDD-ZIP.
           MOVE PDD-COUNTY             TO HR04F9-PDD-COUNTY.
           MOVE PDD-COUNTRY-CODE       TO HR04F9-PDD-COUNTRY-CODE.
           MOVE PDD-PHONE-COUNTRY      TO HR04F9-PDD-PHONE-COUNTRY.
           MOVE PDD-PHONE              TO HR04F9-PDD-PHONE.
           MOVE PDD-PHONE-EXT          TO HR04F9-PDD-PHONE-EXT.
           MOVE PDD-FAX-COUNTRY        TO HR04F9-PDD-FAX-COUNTRY.
           MOVE PDD-FAX-NUMBER         TO HR04F9-PDD-FAX-NUMBER.
           MOVE PDD-FAX-EXT            TO HR04F9-PDD-FAX-EXT.
           MOVE PDD-EMAIL-ADDRESS      TO HR04F9-PDD-EMAIL-ADDRESS.
           MOVE PDD-USER1              TO HR04F9-PDD-USER1.
           MOVE PDD-USER2              TO HR04F9-PDD-USER2.
           MOVE PDD-NUMERIC1           TO HR04F9-PDD-NUMERIC1.
           MOVE PDD-NUMERIC2           TO HR04F9-PDD-NUMERIC2.
           MOVE PDD-CMA-CODE           TO HR04F9-PDD-CMA.
           MOVE PDD-SUPP-ADDR1         TO HR04F9-PDD-SUPP-ADDR1.
           MOVE PDD-SUPP-ADDR2         TO HR04F9-PDD-SUPP-ADDR2.
           MOVE PDD-SUPP-ADDR3         TO HR04F9-PDD-SUPP-ADDR3.
           MOVE PDD-SUPP-ADDR4         TO HR04F9-PDD-SUPP-ADDR4.
           MOVE PDD-SUPP-CITY          TO HR04F9-PDD-SUPP-CITY.
           MOVE PDD-SUPP-STATE         TO HR04F9-PDD-SUPP-STATE.
           MOVE PDD-SUPP-ZIP           TO HR04F9-PDD-SUPP-ZIP.
           MOVE PDD-SUPP-CNTRY-CD      TO HR04F9-PDD-SUPP-CNTRY-CODE.
           MOVE PDD-REPORT-PROV        TO HR04F9-PDD-REPORT-PROV.

           INITIALIZE                             HR04F9-PDD-DESC.
           IF (HR04F9-PDD-CMA NOT = SPACES)
               MOVE HR04F9-PDD-CMA             TO DB-CODE
               MOVE "CA"                       TO DB-TYPE
               PERFORM 840-FIND-PCOSET1
               IF (PCODES-FOUND)
                   MOVE PCO-DESCRIPTION        TO HR04F9-PDD-DESC.

           INITIALIZE                             HR04F9-RPT-PROV-DESC.
           IF (HR04F9-PDD-REPORT-PROV NOT = SPACES)
               MOVE HR04F9-PDD-REPORT-PROV     TO DB-CODE
               MOVE "RP"                       TO DB-TYPE
               PERFORM 840-FIND-PCOSET1
               IF (PCODES-FOUND)
                   MOVE PCO-DESCRIPTION        TO HR04F9-RPT-PROV-DESC.

           INITIALIZE                     HR04F9-INT-COUNTRY-DESC.
           IF (HR04F9-PDD-COUNTRY-CODE NOT = SPACES)
               MOVE HR04F9-PDD-COUNTRY-CODE
                                       TO DB-COUNTRY-CODE
               PERFORM 840-FIND-INTSET1
               IF (INSTCTRYCD-FOUND)
                   MOVE INT-COUNTRY-DESC
                                       TO HR04F9-INT-COUNTRY-DESC
               END-IF.
           INITIALIZE                     HR04F9-INT-SUPP-CNTRY-DESC.
           IF (HR04F9-PDD-SUPP-CNTRY-CODE NOT = SPACES)
               MOVE HR04F9-PDD-SUPP-CNTRY-CODE
                                       TO DB-COUNTRY-CODE
               PERFORM 840-FIND-INTSET1
               IF (INSTCTRYCD-FOUND)
                   MOVE INT-COUNTRY-DESC
                                       TO HR04F9-INT-SUPP-CNTRY-DESC
               END-IF.

           INITIALIZE                     HR04F9-PDD-DESCRIPTION1.
           IF  (HR04F9-PDD-COUNTRY-CODE NOT = SPACES)
           AND (HR04F9-PDD-STATE        NOT = SPACES)
               IF (HR04F9-PDD-COUNTRY-CODE = HRWS-US-WORK-COUNTRY)
                   MOVE HR04F9-PDD-STATE       TO DB-STATE
                   PERFORM 840-FIND-PSASET1
                   IF  (PRSTATE-FOUND)
                       MOVE PSA-DESCRIPTION    TO
                                               HR04F9-PDD-DESCRIPTION1
                   END-IF
               ELSE
               IF (HR04F9-PDD-COUNTRY-CODE = HRWS-CA-WORK-COUNTRY)
                   MOVE HR04F9-PDD-STATE       TO DB-PROVINCE
                   PERFORM 840-FIND-PPVSET1
                   IF (PRPROVINCE-FOUND)
                       MOVE PPV-DESCRIPTION    TO
                                               HR04F9-PDD-DESCRIPTION1
                   END-IF
               END-IF.

           INITIALIZE                     HR04F9-PDD-DESCRIPTION2.
           IF  (HR04F9-PDD-SUPP-CNTRY-CODE   NOT = SPACES)
           AND (HR04F9-PDD-SUPP-STATE        NOT = SPACES)
               IF (HR04F9-PDD-SUPP-CNTRY-CODE = HRWS-US-WORK-COUNTRY)
J53995*            MOVE HR04F9-PDD-STATE       TO DB-STATE
J53995             MOVE HR04F9-PDD-SUPP-STATE  TO DB-STATE
                    PERFORM 840-FIND-PSASET1
                   IF  (PRSTATE-FOUND)
                       MOVE PSA-DESCRIPTION    TO
                                               HR04F9-PDD-DESCRIPTION2
                   END-IF
               ELSE
               IF (HR04F9-PDD-COUNTRY-CODE = HRWS-CA-WORK-COUNTRY)
J53995*            MOVE HR04F9-PDD-STATE       TO DB-PROVINCE
J53995             MOVE HR04F9-PDD-SUPP-STATE  TO DB-PROVINCE
                   PERFORM 840-FIND-PPVSET1
                   IF (PRPROVINCE-FOUND)
                       MOVE PPV-DESCRIPTION    TO
                                               HR04F9-PDD-DESCRIPTION2
                   END-IF
               END-IF.

200100 600-END.
200200
200300******************************************************************
200400 HR04S9-TRANSACTION-END.
200500******************************************************************
200600
