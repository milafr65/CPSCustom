******* BN101PD 40 <3645030978>
      ******************************************************************
      *                            BN101PD                             *
      ******************************************************************
      *                                                                *
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  387287 | J87287 | INCLUDED PLAN TYPE AND CODE FOR CHECKING OF *
      *         |        | DUPLICATE BENEFITS UNDER PLAN CATEGORY.     *
      *  ------   ------   ------------------------------------------  *
      *  491568 | J91568 | REMOVED THE PLAN-CODE FIELD WHEN DEFINING   *
      *         |        | BEGRNG TO ELIMINATE DUPLICATE BENEFIT ROWS. *
      *  ------   ------   ------------------------------------------  *
      *  717193 | J17193 | ADDED LOGIC TO PASS DATA OF PRM-UPDATE-ACA  *
      *         |        | AND WEB-UPDATE TO THE BNBEN WS FIELDS.      *
      *  ------   ------   ------------------------------------------- *
      *         |        |                                             *
000100******************************************************************
000200*INITIALIZE HDB-START-DATE HDB-STOP-DATE                         *
000300*Proc 5,6,7 type for Add a new benefit                           *
000400* Create benefit with zeroes in PayPer, AnnAmt and PreAft Conts  *
000500******************************************************************
      *                                                                *
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  307540 | J07540 | DOMESTIC PARTNER                            *
      *  ------   ------   ------------------------------------------  *
      *  267795 | J67795 | ALLOW 4 DECIMALS FOR BN PENSION PERCENT PLAN*
      ******************************************************************
      *               M O D I F I C A T I O N   L O G:                 *
      ******************************************************************
      *  Modified by Analysts International,MN                         *
      ******************************************************************
      *  AI0010  03/24/03  MODIFY THE BENEFIT PLAN ELIGIBILITY COMMON  *
      *                    MODULE TO UTILIZE THE EFFECTIVE DATE AND    *
      *                    END DATE FIELDS IN DETERMINATION OF THE     *
      *                    EMPLOYEES ELIGIBILITY FOR THE BENEFIT PLAN. *
      *          10/27/10  REAPPLIED ABOVE CUSTOMIZATIONS AFTER THE    *
      *                    9.0 APPS UPGRADE.  M. HUNTER - ACS          *
      *          08/23/11  REAPPLIED ABOVE CUSTOMIZATIONS AFTER THE    *
      *                    9.0.1 APPS UPGRADE.  M. HUNTER - ACS        *
000600******************************************************************
000700 050-EDIT-PARAMETERS             SECTION 10.
000800******************************************************************
000900 050-START.
001000
001100     MOVE PRM-COMPANY            TO DB-COMPANY.
001200     INITIALIZE DB-PROCESS-LEVEL.
001300     PERFORM 840-FIND-PRSSET1.
001400     IF (PRSYSTEM-NOTFOUND)
001500******** Company does not exist
001600         MOVE 100                TO CRT-ERROR-NBR
001700         MOVE PRM-COMPANY        TO CRT-ERR-VAR1
001800         MOVE WS-TRUE            TO WS-PARAMETER-ERROR
001900         PERFORM 780-PRINT-ERROR-MSG
002000         GO TO 050-END.
002100
002200     MOVE PRM-COMPANY            TO DB-COMPANY.
002300     PERFORM 840-FIND-BNCSET1.
002400     IF (BNCOMPANY-NOTFOUND)
002500******** Company is not setup in benefit system
002600         MOVE 101                TO CRT-ERROR-NBR
002700         MOVE PRM-COMPANY        TO CRT-ERR-VAR1
002800         MOVE WS-TRUE            TO WS-PARAMETER-ERROR
002900         PERFORM 780-PRINT-ERROR-MSG
003000         GO TO 050-END.
003100
003200     IF (PRM-EMPLOYEE-SEQ        = SPACES)
003300         MOVE PRS-EMPLOYEE-SEQ   TO PRM-EMPLOYEE-SEQ.
003400
003500*
003600**** SHIFT LEFT & CHECK FOR DUP PLAN TYPES
003700*
003800     PERFORM 052-EDIT-PLAN-TYPE
003900     THRU    052-END.
004000     IF (NO-ERROR-FOUND)
004100*
004200******** SHIFT LEFT & CHECK FOR DUP PLAN CODES
004300*
004400         PERFORM 054-EDIT-PLAN-CODE
004500         THRU    054-END.
004600
004700     IF  (PRM-PROC-LEVEL         NOT = SPACES)
004800     AND (PRM-PROC-GROUP         NOT = SPACES)
004900******** Cannot enter process level and processing group
005000         MOVE 107                TO CRT-ERROR-NBR
005100         MOVE PRM-PROC-LEVEL     TO CRT-ERR-VAR1
005200         MOVE WS-TRUE            TO WS-PARAMETER-ERROR
005300         PERFORM 780-PRINT-ERROR-MSG.
005400
005500     IF  (PRM-PROC-LEVEL         NOT = SPACES)
005600     AND (PRM-GROUP-NAME         NOT = SPACES)
005700******** Cannot enter process level and employee group
005800         MOVE 123                TO CRT-ERROR-NBR
005900         MOVE PRM-PROC-LEVEL     TO CRT-ERR-VAR1
006000         MOVE WS-TRUE            TO WS-PARAMETER-ERROR
006100         PERFORM 780-PRINT-ERROR-MSG.
006200
006300     IF  (PRM-PROC-GROUP         NOT = SPACES)
006400     AND (PRM-GROUP-NAME         NOT = SPACES)
006500******** Cannot enter processing group and employee group
006600         MOVE 124                TO CRT-ERROR-NBR
006700         MOVE PRM-PROC-GROUP     TO CRT-ERR-VAR1
006800         MOVE WS-TRUE            TO WS-PARAMETER-ERROR
006900         PERFORM 780-PRINT-ERROR-MSG.
007000
007100     IF (PRM-PROC-LEVEL          NOT = SPACES)
007200         MOVE PRM-COMPANY        TO DB-COMPANY
007300         MOVE PRM-PROC-LEVEL     TO DB-PROCESS-LEVEL
007400         PERFORM 840-FIND-PRSSET1
007500         IF (PRSYSTEM-NOTFOUND)
007600************ Process level \ does not exist
007700             MOVE 106            TO CRT-ERROR-NBR
007800             MOVE PRM-PROC-LEVEL TO CRT-ERR-VAR1
007900             MOVE WS-TRUE        TO WS-PARAMETER-ERROR
008000             PERFORM 780-PRINT-ERROR-MSG
               ELSE
      ************ Process level is inactive
                   IF (PRS-ACTIVE-FLAG = "I")
                       MOVE 131            TO CRT-ERROR-NBR
                       MOVE PRM-PROC-LEVEL TO CRT-ERR-VAR1
                       MOVE WS-TRUE        TO WS-PARAMETER-ERROR
                       PERFORM 780-PRINT-ERROR-MSG
                   END-IF.
008100
008200     IF (PRM-PROC-GROUP          NOT = SPACES)
008300         MOVE PRM-COMPANY        TO DB-COMPANY
008400         MOVE PRM-PROC-GROUP     TO DB-PROC-GROUP
008500         MOVE PRPSET1-PROC-GROUP TO WS-DB-BEG-RNG
008600         PERFORM 850-FIND-BEGRNG-PRPSET1
008700         IF (PRPROCGRP-NOTFOUND)
008800************ Processing group \ does not exist
008900             MOVE 108            TO CRT-ERROR-NBR
009000             MOVE PRM-PROC-GROUP TO CRT-ERR-VAR1
009100             MOVE WS-TRUE        TO WS-PARAMETER-ERROR
009200             PERFORM 780-PRINT-ERROR-MSG.
009300
009400     IF (PRM-GROUP-NAME          NOT = SPACES)
009500         MOVE PRM-COMPANY        TO DB-COMPANY
009600         MOVE PRM-GROUP-NAME     TO DB-GROUP-NAME
009700         PERFORM 840-FIND-PRGSET1
009800         IF (PERSGROUP-NOTFOUND)
009900************ Employee group \ does not exist
010000             MOVE 109            TO CRT-ERROR-NBR
010100             MOVE PRM-GROUP-NAME TO CRT-ERR-VAR1
010200             MOVE WS-TRUE        TO WS-PARAMETER-ERROR
010300             PERFORM 780-PRINT-ERROR-MSG.
010400
010500     IF (PRM-TO-DATE             = ZEROES)
010600         MOVE WS-HIGH-VALUES     TO PRM-TO-DATE.
010700
010800     IF (PRM-FROM-DATE           > PRM-TO-DATE)
010900******* From date cannot be greater than to date
011000         MOVE 125                TO CRT-ERROR-NBR
011100         MOVE WS-TRUE            TO WS-PARAMETER-ERROR
011200         PERFORM 780-PRINT-ERROR-MSG.
011300
           IF  (PRM-CREATE-TRANS       = "Y")
           AND (PRM-HIPAA-REASON       = SPACES)
      ******** Create transaticon = 'Y'; Must enter hipaa reason code
011000         MOVE 128                TO CRT-ERROR-NBR
011100         MOVE WS-TRUE            TO WS-PARAMETER-ERROR
011200         PERFORM 780-PRINT-ERROR-MSG.

           IF  (PRM-CREATE-TRANS       = "N")
           AND (PRM-HIPAA-REASON       NOT = SPACES)
      ******** Cannot enter hipaa reason when create transaction = 'N'
011000         MOVE 129                TO CRT-ERROR-NBR
011100         MOVE WS-TRUE            TO WS-PARAMETER-ERROR
011200         PERFORM 780-PRINT-ERROR-MSG.

           IF (PRM-HIPAA-REASON        NOT = SPACES)
               MOVE BNBEN-BT           TO DB-TYPE
               MOVE PRM-HIPAA-REASON   TO DB-CODE
               PERFORM 840-FIND-PCOSET1
               IF (PCODES-NOTFOUND)
      ************ HIPAA reason code {0} does not exist
011000             MOVE 130            TO CRT-ERROR-NBR
011100             MOVE WS-TRUE        TO WS-PARAMETER-ERROR
011200             PERFORM 780-PRINT-ERROR-MSG
               ELSE
                   MOVE PRM-HIPAA-REASON   TO BNBEN-HIPAA-REASON
                                              HRHDB-HIPAA-REASON.

011400     GO TO 050-END.
011500
011600******************************************************************
011700 052-EDIT-PLAN-TYPE.
011800******************************************************************
011900
012000*
012100**** SHIFT LEFT ALL PLAN TYPES
012200*
012300     PERFORM
012400         VARYING I1 FROM 1 BY 1
012500         UNTIL  (I1 > 11)
012600         OR     (I2 > 11)
012700
012800         PERFORM
012900             VARYING I1 FROM I1 BY 1
013000             UNTIL  (I1 > 11)
013100             OR     (PRM-PLAN-TYPE (I1)  = SPACES)
013200
013300             CONTINUE
013400         END-PERFORM
013500
013600         COMPUTE I2                      = I1
013700                                         + 1
013800         PERFORM
013900             VARYING I2 FROM I1 BY 1
014000             UNTIL  (I2 > 11)
014100             OR     (PRM-PLAN-TYPE (I2)  NOT = SPACES)
014200
014300             CONTINUE
014400         END-PERFORM
014500         IF (I2                          <= 11)
014600             IF (PRM-PLAN-TYPE (I2)      NOT = SPACES)
014700                 MOVE PRM-PLAN-TYPE (I2) TO PRM-PLAN-TYPE (I1)
014800                 INITIALIZE PRM-PLAN-TYPE (I2)
014900             END-IF
015000         END-IF
015100     END-PERFORM.
015200
015300*
015400**** CHECK FOR DUPS
015500*
015600     PERFORM
015700         VARYING I1 FROM 1 BY 1
015800         UNTIL  (I1 > 11)
015900         OR     (PRM-PLAN-TYPE (I1)      = SPACES)
016000         OR     (ERROR-FOUND)
016100
016200         COMPUTE I2                      = I1
016300                                         + 1
016400         PERFORM
016500             VARYING I2 FROM I2 BY 1
016600             UNTIL  (I2 > 11)
016700             OR     (PRM-PLAN-TYPE (I2)  = PRM-PLAN-TYPE (I1))
016800
016900             CONTINUE
017000         END-PERFORM
017100         IF (I2                          <= 11)
017200             IF (PRM-PLAN-TYPE (I2)      = PRM-PLAN-TYPE (I1))
017300**************** Duplicate plan type
017400                 MOVE 102                TO CRT-ERROR-NBR
017500                 MOVE WS-TRUE            TO WS-PARAMETER-ERROR
017600                 PERFORM 780-PRINT-ERROR-MSG
017700             END-IF
017800         END-IF
017900     END-PERFORM.
018000
018100     IF (I1                               = 1)
018200******** Must enter atleast one plan type
018300         MOVE 110                        TO CRT-ERROR-NBR
018400         MOVE WS-TRUE                    TO WS-PARAMETER-ERROR
018500         PERFORM 780-PRINT-ERROR-MSG
018600     ELSE
018700         COMPUTE WS-LAST-PLAN-TYPE        = I1
018800                                          - 1.
018900
019000 052-END.
019100
019200******************************************************************
019300 054-EDIT-PLAN-CODE.
019400******************************************************************
019500
019600*
019700**** SHIFT LEFT ALL PLAN CODES
019800*
           INITIALIZE I2.

019900     PERFORM
020000         VARYING I1 FROM 1 BY 1
020100         UNTIL  (I1 > 11)
020200         OR     (I2 > 11)
020300
020400         PERFORM
020500             VARYING I1 FROM I1 BY 1
020600             UNTIL  (I1 > 11)
020700             OR     (PRM-PLAN-CODE (I1)  = SPACES)
020800
020900             CONTINUE
021000         END-PERFORM
021100
021200         COMPUTE I2                      = I1
021300                                         + 1
021400         PERFORM
021500             VARYING I2 FROM I1 BY 1
021600             UNTIL  (I2 > 11)
021700             OR     (PRM-PLAN-CODE (I2)  NOT = SPACES)
021800
021900             CONTINUE
022000         END-PERFORM
022100         IF (I2                          <= 11)
022200             IF (PRM-PLAN-CODE (I2)      NOT = SPACES)
022300                 MOVE PRM-PLAN-CODE (I2) TO PRM-PLAN-CODE (I1)
022400                 INITIALIZE PRM-PLAN-CODE (I2)
022500             END-IF
022600         END-IF
022700     END-PERFORM.
022800
022900*
023000**** CHECK FOR DUPS
023100*
023200     PERFORM
023300         VARYING I1 FROM 1 BY 1
023400         UNTIL  (I1 > 11)
023500         OR     (PRM-PLAN-CODE (I1)      = SPACES)
023600         OR     (ERROR-FOUND)
023700
023800         COMPUTE I2                      = I1
023900                                         + 1
024000         PERFORM
024100             VARYING I2 FROM I2 BY 1
024200             UNTIL  (I2 > 11)
024300             OR     (PRM-PLAN-CODE (I2)  = PRM-PLAN-CODE (I1))
024400
024500             CONTINUE
024600         END-PERFORM
024700         IF (I2                          <= 11)
024800             IF (PRM-PLAN-CODE (I2)      = PRM-PLAN-CODE (I1))
024900**************** Duplicate plan code
025000                 MOVE 103                TO CRT-ERROR-NBR
025100                 MOVE WS-TRUE            TO WS-PARAMETER-ERROR
025200                 PERFORM 780-PRINT-ERROR-MSG
025300             END-IF
025400         END-IF
025500     END-PERFORM.
025600
025700     IF (ERROR-FOUND)
025800         GO TO 054-END.
025900
026000     COMPUTE WS-LAST-PLAN-CODE            = I1
026100                                          - 1.
026200
026300     IF (WS-LAST-PLAN-CODE                = ZEROES)
026400         GO TO 054-END.
026500
026600     INITIALIZE I9.
026700
026800     MOVE PRM-COMPANY                    TO DB-COMPANY.
026900
027000     PERFORM
027100         VARYING I1 FROM 1 BY 1
027200         UNTIL  (I1 > WS-LAST-PLAN-CODE)
027300
027400         SET WS-PLAN-NOTFOUND            TO TRUE
027500
027600         PERFORM
027700             VARYING I2 FROM 1 BY 1
027800             UNTIL  (I2 > WS-LAST-PLAN-TYPE)
027900
028000             MOVE PRM-PLAN-TYPE (I2)     TO DB-PLAN-TYPE
028100             MOVE PRM-PLAN-CODE (I1)     TO DB-PLAN-CODE
028200             PERFORM 840-FIND-PLNSET1
028300             IF (PLAN-FOUND)
028400                 ADD 1                   TO I9
028500                 SET WS-PLAN-FOUND       TO TRUE
028600                 MOVE PRM-PLAN-TYPE (I2) TO WS-TBL-PLAN-TYPE (I9)
028700                 MOVE PRM-PLAN-CODE (I1) TO WS-TBL-PLAN-CODE (I9)
028800             END-IF
028900         END-PERFORM
029000         IF (WS-PLAN-NOTFOUND)
029100************ Plan does not exist in any of the plan types
029200             MOVE 104                    TO CRT-ERROR-NBR
029300             MOVE PRM-PLAN-CODE (I1)     TO CRT-ERR-VAR1
029400             MOVE WS-TRUE                TO WS-PARAMETER-ERROR
029500             PERFORM 780-PRINT-ERROR-MSG
029600         END-IF
029700     END-PERFORM.
029800
029900     MOVE I9                             TO WS-LAST-PLAN-CODE.
030000
030100 054-END.
030200
030300******************************************************************
030400 050-END.
030500******************************************************************
030600
030700******************************************************************
030800 100-PROGRAM-CONTROL             SECTION 10.
030900******************************************************************
031000 100-START.
031100
031200**** Processing BN101 - Mass Benefit Add
031300     MOVE 050                    TO CRT-MSG-NBR.
031400     PERFORM 780-DISPLAY-MSG.
031500
031600     PERFORM 840-FIND-CKPSET1.
031700     IF  (CKPOINT-FOUND)
031800     AND (CKP-RESTART-INFO           NOT = SPACES)
031900         MOVE CKP-RESTART-INFO       TO WS-REST-DATA
032000         MOVE WS-REST-WORK-FILE      TO WS-WORK-FILE
032100         MOVE WS-REST-ERROR-FILE     TO WS-ERROR-FILE
032200         MOVE WS-REST-ELIG-FILE      TO WS-ELIG-FILE
032300         MOVE WS-REST-COUNT          TO WS-RECORD-COUNT
032400         MOVE WS-REST-NAV-POINTER    TO WS-NAV-POINTER
032500         MOVE WS-TRUE                TO PROGRAM-RESTARTING
032600         IF (CREATE-WORK)
032700             GO TO 100-CREATE-WORK
032800         ELSE
032900         IF (PROCESS-BN101BENA)
033000             GO TO 100-PROCESS-BN101BENA
033100         ELSE
033200         IF (PRINT-REPORT)
033300             GO TO 100-PRINT-REPORT
033400         ELSE
033500             GO TO 100-END.
033600
033700     PERFORM 910-AUDIT-BEGIN.
033800
033900     PERFORM 840-MODIFY-CKPSET1.
034000     SET CREATE-WORK             TO TRUE.
034100     INITIALIZE WS-REST-WORK-FILE
034200                WS-REST-ERROR-FILE
034300                WS-REST-ELIG-FILE
034400                WS-REST-COUNT.
034500     MOVE WS-REST-DATA           TO CKP-RESTART-INFO.
034600     PERFORM 820-STORE-CKPOINT.
034700
034800     PERFORM 925-AUDIT-END.
034900
035000******************************************************************
035100* CREATE BN101BENA, BN101ERROR, BN101ELIG                        *
035200******************************************************************
035300 100-CREATE-WORK.
035400******************************************************************
035500
035600     IF (WS-WORK-FILE            = SPACES)
035700         PERFORM 900-BUILD-TMP-FILE-NAME
035800         MOVE WS-TMP-FILE        TO WS-WORK-FILE
035900                                    WS-REST-WORK-FILE
036000         OPEN OUTPUT BN101BENA-FILE
036100         CLOSE BN101BENA-FILE.
036200
036300     IF (WS-ERROR-FILE           = SPACES)
036400         PERFORM 900-BUILD-TMP-FILE-NAME
036500         MOVE WS-TMP-FILE        TO WS-ERROR-FILE
036600                                    WS-REST-ERROR-FILE.
036700
036800     IF (WS-ELIG-FILE            = SPACES)
036900         PERFORM 900-BUILD-TMP-FILE-NAME
037000         MOVE WS-TMP-FILE        TO WS-ELIG-FILE
037100                                    WS-REST-ELIG-FILE.
037200
037300     OPEN I-O BN101BENA-FILE.
037400
037500     OPEN OUTPUT BN101ERROR-FILE.
037600
037700     OPEN OUTPUT BN101ELIG-FILE.
037800
037900     PERFORM 1000-CREATE-WORK.
038000
038100     CLOSE BN101BENA-FILE SAVE.
038200
038300     CLOSE BN101ERROR-FILE SAVE.
038400
038500     CLOSE BN101ELIG-FILE SAVE.
038600
038700     IF (NO-BEN-DATA)
038800******** No data to print in Mass Benefit Add
038900         MOVE 051                TO CRT-MSG-NBR
039000         PERFORM 780-PRINT-MSG.
039100
039200     IF (NO-ERR-DATA)
039300******** No data to print in Error Report
039400         MOVE 052                TO CRT-MSG-NBR
039500         PERFORM 780-PRINT-MSG.
039600
039700     IF (NO-ELIG-DATA)
039800******** No data to print in Eligibility Report
039900         MOVE 053                TO CRT-MSG-NBR
040000         PERFORM 780-PRINT-MSG.
040100
040200     IF  (NO-BEN-DATA)
040300     AND (NO-ERR-DATA)
040400     AND (NO-ELIG-DATA)
040500         MOVE WS-WORK-FILE       TO WS-TMP-FILE
040600         PERFORM 901-REMOVE-TMP-FILE
040700
040800         MOVE WS-ERROR-FILE      TO WS-TMP-FILE
040900         PERFORM 901-REMOVE-TMP-FILE
041000
041100         MOVE WS-ELIG-FILE       TO WS-TMP-FILE
041200         PERFORM 901-REMOVE-TMP-FILE
041300
041400         GO TO 100-END.
041500
041600     PERFORM 910-AUDIT-BEGIN.
041700
041800     PERFORM 840-MODIFY-CKPSET1.
041900     IF (PRM-UPDATE-OPTION       = "U")
042000         SET PROCESS-BN101BENA   TO TRUE
042100     ELSE
042200         SET PRINT-REPORT        TO TRUE.
042300     MOVE WS-REST-DATA           TO CKP-RESTART-INFO.
042400     PERFORM 820-STORE-CKPOINT.
042500
042600     PERFORM 925-AUDIT-END.
042700
042800     IF (PRINT-REPORT)
042900         GO TO 100-PRINT-REPORT.
043000
043100******************************************************************
043200* ADD BENEFITS USING BN101BENA                                   *
043300******************************************************************
043400 100-PROCESS-BN101BENA.
043500******************************************************************
043600
043700     OPEN INPUT BN101BENA-FILE.
043800
043900     PERFORM 910-AUDIT-BEGIN.
044000
044100     PERFORM 2000-PROCESS-BN101BENA.
044200
044300     PERFORM 840-MODIFY-CKPSET1.
044400     SET PRINT-REPORT            TO TRUE.
044500     MOVE WS-REST-DATA           TO CKP-RESTART-INFO.
044600     PERFORM 820-STORE-CKPOINT.
044700
044800     PERFORM 925-AUDIT-END.
044900
045000     CLOSE BN101BENA-FILE SAVE.
045100
045200******************************************************************
045300* PRINT REPORT USING BN101BENA, BN101ERROR, BN101ELIG            *
045400******************************************************************
045500 100-PRINT-REPORT.
045600******************************************************************
045700
045800     IF (WS-BENAS-FILE           = SPACES)
045900         PERFORM 900-BUILD-TMP-FILE-NAME
046000         MOVE WS-TMP-FILE        TO WS-BENAS-FILE.
046100
046200     IF (PRM-EMPLOYEE-SEQ        = "N")
046300         SORT BN101SORT-FILE
046400             ASCENDING KEY       DSF-COMPANY
046500                                 DSF-PLAN-TYPE
046600                                 DSF-PLAN-CODE
046700                                 DSF-PROC-LEVEL
046800                                 DSF-DEPARTMENT
046900                                 DSF-EMPLOYEE
047000                                 DSF-START-DATE
047100                           USING BN101BENA-FILE
047200                          GIVING BN101BENAS-FILE SAVE
047300     ELSE
047400         SORT BN101SORT-FILE
047500             ASCENDING KEY       DSF-COMPANY
047600                                 DSF-PLAN-TYPE
047700                                 DSF-PLAN-CODE
047800                                 DSF-PROC-LEVEL
047900                                 DSF-DEPARTMENT
048000                                 DSF-LAST-NAME
048100                                 DSF-FIRST-NAME
048200                                 DSF-MIDDLE-INIT
048300                                 DSF-START-DATE
048400                           USING BN101BENA-FILE
048500                          GIVING BN101BENAS-FILE SAVE.
048600
048700     OPEN INPUT BN101BENAS-FILE.
048800
048900     PERFORM 3000-PRINT-REPORT.
049000
049100     CLOSE BN101BENAS-FILE SAVE.
049200
049300     IF (PRM-EMPLOYEE-SEQ        = "N")
049400         SORT BN101ESORT-FILE
049500             ASCENDING KEY       DESF-COMPANY
049600                                 DESF-PLAN-TYPE
049700                                 DESF-PLAN-CODE
049800                                 DESF-PROC-LEVEL
049900                                 DESF-DEPARTMENT
050000                                 DESF-EMPLOYEE
050100                           USING BN101ERROR-FILE
050200                          GIVING BN101ERROR-FILE SAVE
050300     ELSE
050400         SORT BN101ESORT-FILE
050500             ASCENDING KEY       DESF-COMPANY
050600                                 DESF-PLAN-TYPE
050700                                 DESF-PLAN-CODE
050800                                 DESF-PROC-LEVEL
050900                                 DESF-DEPARTMENT
051000                                 DESF-LAST-NAME
051100                                 DESF-FIRST-NAME
051200                                 DESF-MIDDLE-INIT
051300                           USING BN101ERROR-FILE
051400                          GIVING BN101ERROR-FILE SAVE.
051500
051600     OPEN INPUT BN101ERROR-FILE.
051700
051800     PERFORM 4000-PRINT-ERROR-REPORT.
051900
052000     CLOSE BN101ERROR-FILE SAVE.
052100
052200     IF (PRM-EMPLOYEE-SEQ        = "N")
052300         SORT BN101ELIGS-FILE
052400             ASCENDING KEY       DSF1-COMPANY
052500                                 DSF1-PLAN-TYPE
052600                                 DSF1-PLAN-CODE
052700                                 DSF1-PROC-LEVEL
052800                                 DSF1-DEPARTMENT
052900                                 DSF1-EMPLOYEE
053000                                 DSF1-ELIG-DATE
053100                           USING BN101ELIG-FILE
053200                          GIVING BN101ELIG-FILE SAVE
053300     ELSE
053400         SORT BN101ELIGS-FILE
053500             ASCENDING KEY       DSF1-COMPANY
053600                                 DSF1-PLAN-TYPE
053700                                 DSF1-PLAN-CODE
053800                                 DSF1-PROC-LEVEL
053900                                 DSF1-DEPARTMENT
054000                                 DSF1-LAST-NAME
054100                                 DSF1-FIRST-NAME
054200                                 DSF1-MIDDLE-INIT
054300                                 DSF1-ELIG-DATE
054400                           USING BN101ELIG-FILE
054500                          GIVING BN101ELIG-FILE SAVE.
054600
054700     OPEN INPUT BN101ELIG-FILE.
054800
054900     PERFORM 5000-PRINT-ELIG-REPORT.
055000
055100     CLOSE BN101ELIG-FILE SAVE.
055200
055300     MOVE WS-WORK-FILE           TO WS-TMP-FILE.
055400     PERFORM 901-REMOVE-TMP-FILE.
055500
055600     MOVE WS-ERROR-FILE          TO WS-TMP-FILE.
055700     PERFORM 901-REMOVE-TMP-FILE.
055800
055900     MOVE WS-ELIG-FILE           TO WS-TMP-FILE.
056000     PERFORM 901-REMOVE-TMP-FILE.
056100
056200     MOVE WS-BENAS-FILE          TO WS-TMP-FILE.
056300     PERFORM 901-REMOVE-TMP-FILE.
056400
056500     GO TO 100-END.
056600
056700 100-END.
056800
056900******************************************************************
057000 1000-CREATE-WORK                SECTION 50.
057100******************************************************************
057200 1000-START.
057300
057400**** Processing BN101 - Creating workfiles
057500     MOVE 054                        TO CRT-MSG-NBR.
057600     PERFORM 780-DISPLAY-MSG.
057700
057800     MOVE PRM-COMPANY                    TO DB-COMPANY.
057900     IF (PRM-PROC-LEVEL                  NOT = SPACES)
058000         MOVE PRM-PROC-LEVEL             TO DB-PROCESS-LEVEL
058100         MOVE EMPSET7-PROCESS-LEVEL      TO WS-DB-BEG-RNG
058200         PERFORM 850-FIND-BEGRNG-EMPSET7
058300         PERFORM 1100-SEL-EMPLOYEE
058400         THRU    1100-END
058500             UNTIL (EMPLOYEE-NOTFOUND)
058600     ELSE
058700     IF (PRM-PROC-GROUP                  NOT = SPACES)
058800         MOVE PRM-PROC-GROUP             TO DB-PROC-GROUP
058900         MOVE PRPSET1-PROC-GROUP         TO WS-DB-BEG-RNG
059000         PERFORM 850-FIND-BEGRNG-PRPSET1
059100         PERFORM
059200             UNTIL (PRPROCGRP-NOTFOUND)
059300
059400             MOVE PRP-PROCESS-LEVEL      TO DB-PROCESS-LEVEL
059500             MOVE EMPSET7-PROCESS-LEVEL  TO WS-DB-BEG-RNG
059600             PERFORM 850-FIND-BEGRNG-EMPSET7
059700             PERFORM 1100-SEL-EMPLOYEE
059800             THRU    1100-END
059900                 UNTIL (EMPLOYEE-NOTFOUND)
060000
060100             PERFORM 860-FIND-NXTRNG-PRPSET1
060200         END-PERFORM
060300     ELSE
060400     IF (PRM-GROUP-NAME                  NOT = SPACES)
060500         MOVE PRM-GROUP-NAME             TO DB-GROUP-NAME
P74008*        INITIALIZE DB-EMPLOYEE
P74008*        PERFORM 850-FIND-NLT-PGESET1
P74008*        PERFORM
P74008*            UNTIL (PGEMPLOYEE-NOTFOUND)
P74008*            OR    (PGE-COMPANY          NOT = PRM-COMPANY)
P74008*            OR    (PGE-GROUP-NAME       NOT = PRM-GROUP-NAME)
P74008
P74008         MOVE PGESET1-GROUP-NAME         TO WS-DB-BEG-RNG
P74008         PERFORM 850-FIND-BEGRNG-PGESET1
P74008         PERFORM 
P74008             UNTIL (PGEMPLOYEE-NOTFOUND)
061200
061300             MOVE PGE-EMPLOYEE           TO DB-EMPLOYEE
061400             PERFORM 840-FIND-EMPSET1
061500             PERFORM 1100-SEL-EMPLOYEE
061600             THRU    1100-END
061700
P74008*            MOVE PRM-GROUP-NAME         TO DB-GROUP-NAME
P74008*            MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE
P74008*            PERFORM 850-FIND-NLT-PGESET1
062100 
P74008*            PERFORM 860-FIND-NEXT-PGESET1
P74008             PERFORM 860-FIND-NXTRNG-PGESET1
062300         END-PERFORM
062400     ELSE
062500         MOVE EMPSET1-COMPANY            TO WS-DB-BEG-RNG
062600         PERFORM 850-FIND-BEGRNG-EMPSET1
062700         PERFORM
062800             UNTIL (EMPLOYEE-NOTFOUND)
062900
063000             PERFORM 1100-SEL-EMPLOYEE
063100             THRU    1100-END
063200                 UNTIL (EMPLOYEE-NOTFOUND)
063300         END-PERFORM.
063400
063500     GO TO 1000-END.
063600
063700******************************************************************
063800 1100-SEL-EMPLOYEE.
063900******************************************************************
064000
064100     MOVE EMP-COMPANY            TO CRT-COMPANY.
064200     MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL.
064300     PERFORM 700-HR-EMP-SECURITY.
064400     IF (HRWS-EMP-SECURED)
064500         GO TO 1100-NEXT-EMPLOYEE.
064600
064700     MOVE PRM-COMPANY            TO DB-COMPANY.
064800     MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
064900     PERFORM 840-FIND-PEMSET1.
065000
065100     INITIALIZE WS-EFR-TABLE
065200                WS-EFR-TABLE-1
065300                WS-TOTAL-AVAIL.
065400
065500     SET PROC-EFD                TO TRUE.
065600
065700     PERFORM 1110-FIND-EMPFLEXDOL
065800     THRU    1110-END.
065900     IF (ERROR-FOUND)
066000         PERFORM 1920-CREATE-BN101ERROR
066100         THRU    1920-END
066200         GO TO 1100-NEXT-EMPLOYEE.
066300
066400     PERFORM 1120-CREATE-EFR-TABLE
066500     THRU    1120-END.
066600     IF (ERROR-FOUND)
066700         PERFORM 1920-CREATE-BN101ERROR
066800         THRU    1920-END
066900         GO TO 1100-NEXT-EMPLOYEE.
067000
067100     SET PROC-EFD-OVER           TO TRUE.
067200
067300     IF (WS-LAST-PLAN-CODE               > ZEROES)
067400         PERFORM
067500             VARYING WS-I8 FROM 1 BY 1
067600             UNTIL  (WS-I8               > WS-LAST-PLAN-CODE)
067700
067800             MOVE PRM-COMPANY               TO DB-COMPANY
067900             MOVE WS-TBL-PLAN-TYPE (WS-I8)  TO DB-PLAN-TYPE
068000             MOVE WS-TBL-PLAN-CODE (WS-I8)  TO DB-PLAN-CODE
068100             PERFORM 840-FIND-PLNSET1
068200             PERFORM 1200-SEL-PLAN-CODES
068300             THRU    1200-END
068400         END-PERFORM
068500     ELSE
068600         PERFORM
068700             VARYING WS-I8 FROM 1 BY 1
068800             UNTIL  (WS-I8               > WS-LAST-PLAN-TYPE)
068900
069000             MOVE PRM-COMPANY            TO DB-COMPANY
069100             MOVE PRM-PLAN-TYPE (WS-I8)  TO DB-PLAN-TYPE
069200             MOVE PLNSET1-PLAN-TYPE      TO WS-DB-BEG-RNG
069300             PERFORM 850-FIND-BEGRNG-PLNSET1
069400             PERFORM
069500                 UNTIL (PLAN-NOTFOUND)
069600
069700                 PERFORM 1200-SEL-PLAN-CODES
069800                 THRU    1200-END
069900
070000                 PERFORM 860-FIND-NXTRNG-PLNSET1
070100             END-PERFORM
070200         END-PERFORM.
070300
070400 1100-NEXT-EMPLOYEE.
070500     IF (PRM-PROC-LEVEL                  NOT = SPACES)
070600     OR (PRM-PROC-GROUP                  NOT = SPACES)
070700         PERFORM 860-FIND-NXTRNG-EMPSET7
070800     ELSE
070900     IF (PRM-GROUP-NAME                  = SPACES)
071000         PERFORM 860-FIND-NXTRNG-EMPSET1.
071100
071200 1100-END.
071300
071400******************************************************************
071500 1110-FIND-EMPFLEXDOL.
071600******************************************************************
071700
071800*    INITIALIZE WS-EFD-START-DATE
071900*               WS-EFD-STOP-DATE.
072000
072100*    MOVE PRM-COMPANY                TO DB-COMPANY.
072200*    MOVE EMP-EMPLOYEE               TO DB-EMPLOYEE.
072300*    MOVE PRM-CALC-DATE              TO DB-START-DATE.
072400*    PERFORM 850-FIND-NLT-EFDSET2.
072500*    IF  (EMPFLEXDOL-FOUND)
072600*    AND (EFD-COMPANY                = DB-COMPANY)
072700*    AND (EFD-EMPLOYEE               = DB-EMPLOYEE)
072800*    AND (EFD-STOP-DATE              >= DB-START-DATE)
072900*        MOVE EFD-START-DATE         TO WS-EFD-START-DATE
073000*        MOVE EFD-STOP-DATE          TO WS-EFD-STOP-DATE
073100*    ELSE
073200******** Flex Dollar record does not exist
073300*        MOVE 126                    TO CRT-ERROR-NBR.
073400
073500 1110-END.
073600
073700******************************************************************
073800 1120-CREATE-EFR-TABLE.
073900******************************************************************
074000
074100     PERFORM 1122-FIND-EFD-IN-TABLE
074200     THRU    1122-END.
074300
074400*
074500**** 1122-FIND-EFD-IN-TABLE WILL CHECK IF WE HAVE ALREADY CREATED
074600**** EFR TABLE FOR EMPLOYEE AND EMPFLEXDOL, IF NOT WE WILL CREATE
074700**** IT, THIS SETS THE VALUE OF I4 WHICH IS POINTING TO THE RIGHT EFD
074800*
074900*    IF (ERROR-FOUND)
075000*        GO TO 1120-END.
075100
075200*    MOVE EMP-EMPLOYEE           TO WS-EFD-EMPLOYEE (I4).
075300*    MOVE WS-EFD-START-DATE      TO WS-EFD-START-DT (I4).
075400
075500*    INITIALIZE I5.
075600
075700*    MOVE PRM-COMPANY            TO DB-COMPANY.
075800*    MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
075900*    MOVE WS-EFD-START-DATE      TO DB-PLAN-START-DT.
076000*    MOVE EFRSET1-PLAN-START-DT  TO WS-DB-BEG-RNG.
076100*    PERFORM 850-FIND-BEGRNG-EFRSET1.
076200*    PERFORM
076300*        UNTIL (EMPFLEXREM-NOTFOUND)
076400*        OR    (I5               > WS-MAX-PERIODS)
076500
076600*        ADD 1                   TO I5
076700*        MOVE I5                 TO I7
076800
076900*        PERFORM 795-EDIT-AVAIL-DOLLARS
077000
077100*        MOVE EFR-START-DATE     TO WS-START-DT      (I4 I5)
077200*        MOVE EFR-STOP-DATE      TO WS-STOP-DT       (I4 I5)
077300*        MOVE EFR-CREDITS-AVAIL  TO WS-CREDITS-AVAIL (I4 I5)
077400*        MOVE EFR-PRE-TAX-AVAIL  TO WS-PRE-TAX-AVAIL (I4 I5)
077500
077600*        COMPUTE WS-TOTAL-AVAIL  = WS-CREDITS-AVAIL  (I4 I5)
077700*                                + WS-PRE-TAX-AVAIL  (I4 I5)
077800
077900*        PERFORM 860-FIND-NXTRNG-EFRSET1
078000*    END-PERFORM.
078100*    
078200*    IF (I5                      > WS-MAX-PERIODS)
078300******** More than 24 flex periods exist; Cannot update
078400*        MOVE 124                TO CRT-ERROR-NBR
078500*        GO TO 1120-END.
078600
078700 1120-END.
078800
078900******************************************************************
079000 1122-FIND-EFD-IN-TABLE.
079100******************************************************************
079200
079300*    PERFORM
079400*        VARYING I4 FROM 1 BY 1
079500*        UNTIL  (I4                    > 24)
079600*        OR     ((WS-EFD-EMPLOYEE (I4) = EMP-EMPLOYEE)
079700*        AND     (WS-EFD-START-DT (I4) = WS-EFD-START-DATE))
079800*        OR     (WS-EFD-EMPLOYEE  (I4) = ZEROES)
079900
080000*        CONTINUE
080100*    END-PERFORM.
080200
080300*    IF (I4                      > WS-MAX-EFDS)
080400******** More than 24 EFDs exist for employee; Cannot update
080500*        MOVE 119                TO CRT-ERROR-NBR
080600*        GO TO 1122-END.
080700
080800 1122-END.
080900
081000******************************************************************
081100 1200-SEL-PLAN-CODES.
081200******************************************************************
081300
081400     INITIALIZE WS-START-DATE
081500                WS-STOP-DATE
081600                WS-FLEX-FLAG.
081700
081800     IF  (PLN-STOP-DATE              NOT = ZEROES)
081900     AND (PLN-STOP-DATE              < PRM-CALC-DATE)
082000         GO TO 1200-END.
082100
082200     IF (PLN-FLEX-PLAN               NOT = SPACES)
082300         MOVE PRM-COMPANY            TO DB-COMPANY
082400         MOVE PLN-FLEX-PLAN          TO DB-FLEX-PLAN
082500         PERFORM 840-FIND-FLPSET1
082600         IF (FLP-SPEND-ONLY          = "N")
082700             MOVE WS-EFD-STOP-DATE   TO WS-STOP-DATE
082800             MOVE "Y"                TO WS-FLEX-FLAG.
082900
083000*
083100**** CALL PLAN ELIGIBILITY SECTION WHICH VALIDATES ONLY
083200**** GROUP & ZIP CODE
083300*
083400     MOVE PRM-COMPANY                TO BNPEWS-COMPANY.
083500     MOVE PLN-PLAN-TYPE              TO BNPEWS-PLAN-TYPE.
083600     MOVE PLN-PLAN-CODE              TO BNPEWS-PLAN-CODE.
083700     MOVE EMP-EMPLOYEE               TO BNPEWS-EMPLOYEE.
083800     MOVE "E"                        TO BNPEWS-COVER-TYPE.
AI0010     MOVE PRM-CALC-DATE              TO BNPEWS-START-DATE.
083900     PERFORM 5000-EDIT-GROUP-N-ZIP-70.
084000     IF (ERROR-FOUND)
P62733         IF (CRT-ERROR-NBR = 102 OR 104 OR 105)
                   INITIALIZE                 CRT-ERROR-NBR
                                              CRT-ERROR-CAT
                                              CRT-ERR-VAR1
                                              CRT-ERR-VAR2
                                              CRT-ERR-VAR3
                   GO TO 1200-END
               ELSE
                   PERFORM 1920-CREATE-BN101ERROR
                   THRU    1920-END
                   GO TO 1200-END.
084400
084500*
084600**** CALCULATE ELIGIBILITY DATE AND SET START DATE
084700*
084800     MOVE PRM-COMPANY                TO BNEDWS-COMPANY.
084900     MOVE PLN-PLAN-TYPE              TO BNEDWS-PLAN-TYPE.
085000     MOVE PLN-PLAN-CODE              TO BNEDWS-PLAN-CODE.
085100     MOVE EMP-EMPLOYEE               TO BNEDWS-EMPLOYEE.
085200     MOVE PRM-CALC-DATE              TO BNEDWS-AS-OF-DATE.
           INITIALIZE BNEDWS-START-DATE.
085300     PERFORM 5000-ELIGIBILITY-DATE-CALC-70.
085400     IF (ERROR-FOUND)
085500         PERFORM 1920-CREATE-BN101ERROR
085600         THRU    1920-END
085700         GO TO 1200-END.
085800
P68417*    IF (PRM-CALC-DATE                  < BNEDWS-ELIGIBILITY-DATE)
P68417*        GO TO 1200-END.
P51586
085900     IF (PRM-USE-ELIG-DATE              = "Y")
086000         MOVE BNEDWS-ELIGIBILITY-DATE   TO WS-START-DATE
086100     ELSE
086200     IF (PRM-CALC-DATE                  < PLN-START-DATE)
086300         MOVE PLN-START-DATE            TO WS-START-DATE
086400     ELSE
086500     IF (PRM-CALC-DATE                  > BNEDWS-ELIGIBILITY-DATE)
086600         MOVE PRM-CALC-DATE             TO WS-START-DATE
086700     ELSE
086800         MOVE BNEDWS-ELIGIBILITY-DATE   TO WS-START-DATE.
086900
087000     IF (WS-START-DATE                  < PRM-FROM-DATE)
087100     OR (WS-START-DATE                  > PRM-TO-DATE)
087200         GO TO 1200-END.
087300
087400     PERFORM 1210-EDIT-START-DATE
087500     THRU    1210-END.
087600     IF (ERROR-FOUND)
087700         PERFORM 1920-CREATE-BN101ERROR
087800         THRU    1920-END
087900         GO TO 1200-END.
088000
088100     MOVE BEN-PLAN-CODE              TO DB-PLAN-CODE.
088200
088300*
088400**** CHECK FOR DUPLICATE BENEFIT WITH DEFAULT RULES
088500*
088600     PERFORM 1220-CHECK-DUP-N-DEFAULT
088700     THRU    1220-END.
088800     IF (DUP-BEN)
088900         GO TO 1200-END.
089000
089100*
089200**** PERFORM BNENT EDITS ONLY ON DEFAULT PLANS, BECAUSE THESE ARE THE
089300**** ONLY PLANS WE CAN ADD FROM THIS PROGRAM. IF EMPLOYEE BECOMES
089400**** ELIGIBLE FOR NON-DEFAULT PLAN, WE JUST LIST THEM ON THE REPORT
089500**** WE NOW PROCESS 5, 6 AND 7 TYPE PLANES
089600*
089700     IF (PLN-DEFAULT                 NOT = "N")
089800         PERFORM 1230-CALL-BNENT
089900         THRU    1230-END
090000         IF (ERROR-FOUND)
090100             PERFORM 1920-CREATE-BN101ERROR
090200             THRU    1920-END
090300             GO TO 1200-END
090400         END-IF
090500     ELSE
090600         PERFORM 1930-CREATE-BN101ELIG
090700         THRU    1930-END
090800         GO TO 1200-END.
090900
091000*
091100**** IF FLEX CHECK AVAILABLE DOLLARS
091200*
091300     IF (WS-FLEX-FLAG                = "Y")
091400         PERFORM 1240-EDIT-AVAIL-DOL
091500         THRU    1240-END
091600         IF (ERROR-FOUND)
091700             PERFORM 1920-CREATE-BN101ERROR
091800             THRU    1920-END
091900             GO TO 1200-END.
092000
092100     PERFORM 1900-CREATE-BN101BENA
092200     THRU    1900-END
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > BNBEN-NBR-LINES).
092300
092400 1200-END.
092500
092600******************************************************************
092700 1210-EDIT-START-DATE.
092800******************************************************************
092900
093000     IF (BNEDWS-INIT-ENT-POINT       = "1" OR "2")
093100         PERFORM 1212-USE-PLAN-ENTRY-POINTS
093200         THRU    1212-END
093300     ELSE
093400     IF (BNEDWS-INIT-ENT-POINT       = "3" OR "4")
093500         PERFORM 1214-USE-PAY-PERIOD-DATES
093600         THRU    1214-END
093700     ELSE
093800     IF (BNEDWS-INIT-ENT-POINT       = "5" OR "6")
093900         PERFORM 1216-USE-WORK-PERIOD-DATES
094000         THRU    1216-END.
094100
094200 1210-END.
094300
094400******************************************************************
094500 1212-USE-PLAN-ENTRY-POINTS.
094600******************************************************************
094700
094800     MOVE WS-START-DATE              TO WS-START-DATE-CALC.
094900
095000     PERFORM
095100         VARYING I7 FROM 1 BY 1
095200         UNTIL  (I7                  > BNEDWS-LAST-EP-INDEX)
095300         OR     (BNEDWS-ENTRY-MMDD (I7)
095400                                     = WS-START-DATE-MMDD)
095500
095600         CONTINUE
095700     END-PERFORM.
095800
095900     IF (I7                          > BNEDWS-LAST-EP-INDEX)
096000******** Start date must match entry point
096100         MOVE 115                    TO CRT-ERROR-NBR
096200         GO TO 1212-END.
096300
096400 1212-END.
096500
096600******************************************************************
096700 1214-USE-PAY-PERIOD-DATES.
096800******************************************************************
096900
097000     PERFORM 1218-EDIT-PAY-PLAN
097100     THRU    1218-END.
097200     IF (ERROR-FOUND)
097300         GO TO 1214-END.
097400
097500     MOVE PRM-COMPANY                TO DB-COMPANY.
097600     MOVE PRO-PLAN-CODE              TO DB-PLAN-CODE.
097700     MOVE PRO-EFFECT-DATE            TO DB-EFFECT-DATE.
097800     MOVE PRYSET1-EFFECT-DATE        TO WS-DB-BEG-RNG.
097900     PERFORM 850-FIND-BEGRNG-PRYSET1.
098000
098100     PERFORM
098200         UNTIL (PROTPAYPRD-NOTFOUND)
098300         OR    (PRY-PAY-START-DATE   = WS-START-DATE)
098400
098500         PERFORM 860-FIND-NXTRNG-PRYSET1
098600     END-PERFORM.
098700
098800     IF (PROTPAYPRD-NOTFOUND)
098900******** Start date must match pay period start date
099000         MOVE 116                    TO CRT-ERROR-NBR
099100         GO TO 1214-END.
099200
099300 1214-END.
099400
099500******************************************************************
099600 1216-USE-WORK-PERIOD-DATES.
099700******************************************************************
099800
099900     PERFORM 1218-EDIT-PAY-PLAN
100000     THRU    1218-END.
100100     IF (ERROR-FOUND)
100200         GO TO 1216-END.
100300
100400     MOVE PRM-COMPANY                TO DB-COMPANY.
100500     MOVE PRO-PLAN-CODE              TO DB-PLAN-CODE.
100600     MOVE PRO-EFFECT-DATE            TO DB-EFFECT-DATE.
100700     MOVE PRWSET1-EFFECT-DATE        TO WS-DB-BEG-RNG.
100800     PERFORM 850-FIND-BEGRNG-PRWSET1.
100900
101000     PERFORM
101100         UNTIL (PROTWRKPRD-NOTFOUND)
101200         OR    (PRW-WRK-START-DATE   = WS-START-DATE)
101300
101400         PERFORM 860-FIND-NXTRNG-PRWSET1
101500     END-PERFORM.
101600
101700     IF (PROTWRKPRD-NOTFOUND)
101800******** Start date must match work period start date
101900         MOVE 117                    TO CRT-ERROR-NBR
102000         GO TO 1216-END.
102100
102200 1216-END.
102300
102400******************************************************************
102500 1218-EDIT-PAY-PLAN.
102600******************************************************************
102700
102800     IF (EMP-OT-PLAN-CODE            = SPACES)
102900         MOVE PRM-COMPANY            TO DB-COMPANY 
103000         MOVE EMP-PROCESS-LEVEL      TO DB-PROCESS-LEVEL
103100         PERFORM 840-FIND-PRSSET1
103200         IF (PRS-OT-PLAN-CODE (EMP-PAY-FREQUENCY)
103300                                     NOT = SPACES)
103400             MOVE PRS-OT-PLAN-CODE (EMP-PAY-FREQUENCY)
103500                                     TO DB-PLAN-CODE
103600         ELSE                             
103700             INITIALIZE DB-PROCESS-LEVEL
103800             PERFORM 840-FIND-PRSSET1
103900             IF (PRS-OT-PLAN-CODE (EMP-PAY-FREQUENCY)
104000                                     = SPACES)
104100                 MOVE 122            TO CRT-ERROR-NBR
104200                 GO TO 1218-END
104300             ELSE
104400                 MOVE PRS-OT-PLAN-CODE (EMP-PAY-FREQUENCY) 
104500                                     TO DB-PLAN-CODE
104600             END-IF
104700         END-IF
104800     ELSE                                      
104900         MOVE EMP-OT-PLAN-CODE       TO DB-PLAN-CODE.
105000
105100     MOVE PRM-COMPANY                TO DB-COMPANY.
105200     MOVE PROSET2-PLAN-CODE          TO WS-DB-BEG-RNG.
105300     PERFORM 850-FIND-BEGRNG-PROSET2.
105400     PERFORM
105500         UNTIL (PROVERTIME-NOTFOUND)
105600         OR    (PRO-EFFECT-DATE      <= WS-START-DATE)
105700
105800         PERFORM 860-FIND-NXTRNG-PROSET2
105900     END-PERFORM.
106000
106100 1218-END.
106200
106300******************************************************************
106400 1220-CHECK-DUP-N-DEFAULT.
106500******************************************************************
106600
106700     PERFORM 1221-CHECK-FOR-FUTURE
106800     THRU    1221-END.
106900     IF (ERROR-FOUND)
               IF (BEN-START-DATE > WS-START-DATE)
                   PERFORM 1920-CREATE-BN101ERROR
                   THRU    1920-END
                   GO TO 1220-END
               ELSE
                   INITIALIZE          CRT-ERROR-NBR
                                       CRT-ERROR-CAT
                                       CRT-ERR-VAR1
                                       CRT-ERR-VAR2
                                       CRT-ERR-VAR3
                   GO TO 1220-END.
107300
107300
107400     IF (PLN-DEFAULT             = "T")
107500         PERFORM 1222-EDIT-BY-TYPE
107600         THRU    1222-END
107700         IF (DUP-BEN)
107800             GO TO 1220-END
107900         END-IF
108000     ELSE
108100     IF (PLN-DEFAULT             = "O")
108200         PERFORM 1224-EDIT-BY-OPTION
108300         THRU    1224-END
108400         IF (DUP-BEN)
108500             GO TO 1220-END.
108600
108700     PERFORM 1226-EDIT-BY-CODE
108800     THRU    1226-END.
108900
109000 1220-END.
109100
109200******************************************************************
109300 1221-CHECK-FOR-FUTURE.
109400******************************************************************
109500
109600*
109700**** CHECK FOR FUTURE BENEFITS
109800*
109900     SET NO-DUP-BEN              TO TRUE.
110000
110100     MOVE PRM-COMPANY            TO DB-COMPANY.
110200     MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
110300     MOVE PLN-PLAN-TYPE          TO DB-PLAN-TYPE.
110400     MOVE PLN-PLAN-CODE          TO DB-PLAN-CODE.
110500     MOVE WS-START-DATE          TO DB-START-DATE.
110600     PERFORM 850-FIND-NLT-BENSET4.
110700     IF  (BENEFIT-FOUND)
110800     AND (BEN-COMPANY            = DB-COMPANY)
110900     AND (BEN-EMPLOYEE           = DB-EMPLOYEE)
111000     AND (BEN-PLAN-TYPE          = DB-PLAN-TYPE)
111100     AND (BEN-PLAN-CODE          = DB-PLAN-CODE)
      **** Note, the following was added to verify that the benefit
      **** found is not a previous benefit enrollment with a stop
      **** date prior to the proposed enrollment date before setting the
      **** error condition.
               IF  (BEN-START-DATE     >= WS-START-DATE)
               OR ((BEN-START-DATE     <  WS-START-DATE)
               AND (BEN-STOP-DATE      >= WS-START-DATE))    
               OR ((BEN-START-DATE     <  WS-START-DATE)
               AND (BEN-STOP-DATE      =  ZEROES))
                   SET DUP-BEN             TO TRUE

                   MOVE 127                TO CRT-ERROR-NBR
                   MOVE WS-START-DATE      TO HRWS-DATE-FIELD
                   INITIALIZE HRWS-DATE-8-FIELD
                   PERFORM 781-HR-FORMAT-DATE-FIELD
                   MOVE HRWS-VALUE         TO CRT-ERR-VAR1
                   MOVE BEN-START-DATE     TO HRWS-DATE-FIELD
                   PERFORM 781-HR-FORMAT-DATE-FIELD
                   MOVE HRWS-VALUE         TO CRT-ERR-VAR2.
111800
111900 1221-END.
112000
112100******************************************************************
112200 1222-EDIT-BY-TYPE.
112300******************************************************************
112400
112500*
112600**** CHECK DUPS IN BENEFIT FILE
112700*
112800     SET NO-DUP-BEN              TO TRUE.
112900
113000     MOVE PRM-COMPANY            TO DB-COMPANY.
113100     MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
113200     MOVE PLN-PLAN-TYPE          TO DB-PLAN-TYPE.
113300     MOVE BENSET4-PLAN-TYPE      TO WS-DB-BEG-RNG.
113400     PERFORM 850-FIND-BEGRNG-BENSET4.
113500     PERFORM
113600         UNTIL (BENEFIT-NOTFOUND)
113700         OR    (DUP-BEN)
113800
113900         PERFORM 1228-CHECK-FOR-DUP-BEN
114000         THRU    1228-END
114100
114200         IF (NO-DUP-BEN)
114300             PERFORM 860-FIND-NXTRNG-BENSET4
114400         END-IF
114500     END-PERFORM.
114600
114700     IF (DUP-BEN)
114800         GO TO 1222-END.
114900
115000*
115100**** CHECK DUPS IN WORK FILE
115200*
115300     MOVE PRM-COMPANY            TO IDX-COMPANY.
115400     MOVE EMP-EMPLOYEE           TO IDX-EMPLOYEE.
115500     MOVE PLN-PLAN-TYPE          TO IDX-PLAN-TYPE.
115600     INITIALIZE IDX-PLAN-CODE
115700                IDX-START-DATE.
115800     PERFORM 8500-FIND-NLT-BN101BENA.
115900     PERFORM
116000         UNTIL (BN101BENA-NOTFOUND)
116100         OR    (IDX-COMPANY      NOT = PRM-COMPANY)
116200         OR    (IDX-EMPLOYEE     NOT = EMP-EMPLOYEE)
116300         OR    (IDX-PLAN-TYPE    NOT = PLN-PLAN-TYPE)
116400         OR    (DUP-BEN)
116500
116600         PERFORM 1229-CHECK-FOR-DUP-IDX
116700         THRU    1229-END
116800
116900         IF (NO-DUP-BEN)
117000             PERFORM 8600-FIND-NEXT-BN101BENA
117100         END-IF
117200     END-PERFORM.
117300
117400 1222-END.
117500
117600******************************************************************
117700 1224-EDIT-BY-OPTION.
117800******************************************************************
117900
118000*
118100**** CHECK DUPS IN BENEFIT FILE
118200*
118300     SET NO-DUP-BEN              TO TRUE.
118400
118500     MOVE PRM-COMPANY            TO DB-COMPANY.
118600     MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
118700     MOVE PLN-PLAN-OPTION        TO DB-PLAN-OPTION.
J87287     MOVE PLN-PLAN-TYPE          TO DB-PLAN-TYPE.
J91568*J87287 MOVE PLN-PLAN-CODE          TO DB-PLAN-CODE. 
J91568*J87287 MOVE BENSET8-PLAN-CODE      TO WS-DB-BEG-RNG.
J91568     MOVE BENSET8-PLAN-TYPE      TO WS-DB-BEG-RNG.
118900     PERFORM 850-FIND-BEGRNG-BENSET8.
119000     PERFORM
119100         UNTIL (BENEFIT-NOTFOUND)
119200         OR    (DUP-BEN)
119300
119400         PERFORM 1228-CHECK-FOR-DUP-BEN
119500         THRU    1228-END
119600
119700         IF (NO-DUP-BEN)
119800             PERFORM 860-FIND-NXTRNG-BENSET8
119900         END-IF
120000     END-PERFORM.
120100
120200     IF (DUP-BEN)
120300         GO TO 1224-END.
120400
120500     MOVE PLN-PLAN-TYPE          TO WS-SAVE-PLAN-TYPE.
120600     MOVE PLN-PLAN-CODE          TO WS-SAVE-PLAN-CODE.
120700     MOVE PLN-PLAN-OPTION        TO WS-SAVE-PLAN-OPTION.
120800
120900*
121000**** CHECK DUPS IN WORK FILE
121100*
121200     MOVE PRM-COMPANY            TO IDX-COMPANY.
121300     MOVE EMP-EMPLOYEE           TO IDX-EMPLOYEE.
P74853     MOVE DB-PLAN-TYPE           TO IDX-PLAN-TYPE.
P74853     MOVE DB-PLAN-CODE           TO IDX-PLAN-CODE.
P74853     MOVE DB-START-DATE          TO IDX-START-DATE.
P74853
P74853*    INITIALIZE IDX-PLAN-TYPE
P74853*               IDX-PLAN-CODE
P74853*               IDX-START-DATE.
P74853
121700     PERFORM 8500-FIND-NLT-BN101BENA.
121800     PERFORM
121900         UNTIL (BN101BENA-NOTFOUND)
122000         OR    (IDX-COMPANY      NOT = PRM-COMPANY)
122100         OR    (IDX-EMPLOYEE     NOT = EMP-EMPLOYEE)
122200         OR    (DUP-BEN)
122300
122400         MOVE IDX-PLAN-TYPE      TO DB-PLAN-TYPE
122500         MOVE IDX-PLAN-CODE      TO DB-PLAN-CODE
122600         PERFORM 840-FIND-PLNSET1
122700
122800         IF  (PLAN-FOUND)
122900         AND (PLN-PLAN-OPTION    = WS-SAVE-PLAN-OPTION)
123000             PERFORM 1229-CHECK-FOR-DUP-IDX
123100             THRU    1229-END
123200         END-IF
123300
123400         IF (NO-DUP-BEN)
123500             PERFORM 8600-FIND-NEXT-BN101BENA
123600         END-IF
123700     END-PERFORM.
123800
123900     MOVE PRM-COMPANY            TO DB-COMPANY.
124000     MOVE WS-SAVE-PLAN-TYPE      TO DB-PLAN-TYPE.
124100     MOVE WS-SAVE-PLAN-CODE      TO DB-PLAN-CODE.
124200     PERFORM 840-FIND-PLNSET1.
124300
124400 1224-END.
124500
124600******************************************************************
124700 1226-EDIT-BY-CODE.
124800******************************************************************
124900
125000*
125100**** CHECK DUPS IN BENEFIT FILE
125200*
125300     SET NO-DUP-BEN              TO TRUE.
125400
125500     MOVE PRM-COMPANY            TO DB-COMPANY.
125600     MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
125700     MOVE PLN-PLAN-TYPE          TO DB-PLAN-TYPE.
125800     MOVE PLN-PLAN-CODE          TO DB-PLAN-CODE.
125900     MOVE BENSET4-PLAN-CODE      TO WS-DB-BEG-RNG.
126000     PERFORM 850-FIND-BEGRNG-BENSET4.
126100     PERFORM
126200         UNTIL (BENEFIT-NOTFOUND)
126300         OR    (DUP-BEN)
126400
126500         PERFORM 1228-CHECK-FOR-DUP-BEN
126600         THRU    1228-END
126700
126800         IF (NO-DUP-BEN)
126900             PERFORM 860-FIND-NXTRNG-BENSET4
127000         END-IF
127100     END-PERFORM.
127200
127300     IF (DUP-BEN)
127400         GO TO 1226-END.
127500
127600*
127700**** CHECK DUPS IN WORK FILE
127800*
127900     MOVE PRM-COMPANY            TO IDX-COMPANY.
128000     MOVE EMP-EMPLOYEE           TO IDX-EMPLOYEE.
128100     MOVE PLN-PLAN-TYPE          TO IDX-PLAN-TYPE.
128200     MOVE PLN-PLAN-CODE          TO IDX-PLAN-CODE.
128300     INITIALIZE IDX-START-DATE.
128400     PERFORM 8500-FIND-NLT-BN101BENA.
128500     PERFORM
128600         UNTIL (BN101BENA-NOTFOUND)
128700         OR    (IDX-COMPANY      NOT = PRM-COMPANY)
128800         OR    (IDX-EMPLOYEE     NOT = EMP-EMPLOYEE)
128900         OR    (IDX-PLAN-TYPE    NOT = PLN-PLAN-TYPE)
129000         OR    (IDX-PLAN-CODE    NOT = PLN-PLAN-CODE)
129100         OR    (DUP-BEN)
129200
129300         PERFORM 1229-CHECK-FOR-DUP-IDX
129400         THRU    1229-END
129500
129600         IF (NO-DUP-BEN)
129700             PERFORM 8600-FIND-NEXT-BN101BENA
129800         END-IF
129900     END-PERFORM.
130000
130100 1226-END.
130200
130300******************************************************************
130400 1228-CHECK-FOR-DUP-BEN.
130500******************************************************************
130600
130700     IF (BEN-FLEX-PLAN           NOT = SPACES)
130800         IF  ((BEN-START-DATE    <= WS-START-DATE)
130900         AND  (BEN-STOP-DATE     >= WS-START-DATE))
131000         OR  ((BEN-START-DATE    <= WS-STOP-DATE)
131100         AND  (BEN-STOP-DATE     >= WS-STOP-DATE))
131200         OR  ((BEN-START-DATE    >= WS-START-DATE)
131300         AND  (BEN-STOP-DATE     <= WS-STOP-DATE))
131400             SET DUP-BEN         TO TRUE
131500         END-IF
131600     ELSE
131700     IF (BEN-STOP-DATE           NOT = ZEROES)
131800         IF  (BEN-START-DATE     <= WS-START-DATE)
131900         AND (BEN-STOP-DATE      >= WS-START-DATE)
132000             SET DUP-BEN         TO TRUE
132100         END-IF
132200     ELSE
132300     IF (BEN-START-DATE          <= WS-START-DATE)
P64693         SET DUP-BEN             TO TRUE
P64693     ELSE
P64693     IF  (WS-STOP-DATE           = ZEROES)
P64693     AND (WS-START-DATE         <= BEN-START-DATE)        
P64693         SET DUP-BEN             TO TRUE
P64693     END-IF.
132500
132600 1228-END.
132700
132800******************************************************************
132900 1229-CHECK-FOR-DUP-IDX.
133000******************************************************************
133100
133200     IF (PLN-FLEX-PLAN           NOT = SPACES)
133300         IF  ((IDX-START-DATE    <= WS-START-DATE)
133400         AND  (IDX-STOP-DATE     >= WS-START-DATE))
133500         OR  ((IDX-START-DATE    <= WS-STOP-DATE)
133600         AND  (IDX-STOP-DATE     >= WS-STOP-DATE))
133700         OR  ((IDX-START-DATE    >= WS-START-DATE)
133800         AND  (IDX-STOP-DATE     <= WS-STOP-DATE))
133900             SET DUP-BEN         TO TRUE
134000         END-IF
134100     ELSE
134200     IF (IDX-STOP-DATE           NOT = ZEROES)
134300         IF  (IDX-START-DATE     <= WS-START-DATE)
134400         AND (IDX-STOP-DATE      >= WS-START-DATE)
134500             SET DUP-BEN         TO TRUE
134600         END-IF
134700     ELSE
134800     IF (IDX-START-DATE          <= WS-START-DATE)
134900         SET DUP-BEN             TO TRUE.
135000
135100 1229-END.
135200
135300******************************************************************
135400 1230-CALL-BNENT.
135500******************************************************************
135600
135700     PERFORM 1232-MOVE-TO-BNBEN
135800     THRU    1232-END.
135900
136000     PERFORM 2000-BNBEN-EDIT-TRAN.
136100
136200 1230-END.
136300
136400******************************************************************
136500 1232-MOVE-TO-BNBEN.
136600******************************************************************
136700
136800     INITIALIZE BNBEN-DETAIL-GROUP
136900                BNBEN-USE-NAVIGATE-SW.
137000
137100     MOVE PRM-COMPANY                TO BNBEN-COMPANY.
137200     MOVE EMP-EMPLOYEE               TO BNBEN-EMPLOYEE (1).
137300     MOVE PLN-PLAN-TYPE              TO BNBEN-PLAN-TYPE (1).
137400     MOVE PLN-PLAN-CODE              TO BNBEN-PLAN-CODE (1).
137500     MOVE WS-START-DATE              TO BNBEN-START-DATE (1).
137600     MOVE WS-STOP-DATE               TO BNBEN-STOP-DATE (1).
           MOVE PRM-CREATE-TRANS           TO BNBEN-CREATE-TRANS (1).
J17193     MOVE PRM-UPDATE-ACA             TO BNBEN-UPDATE-ACA.
J17193     MOVE SPACES                     TO BNBEN-WEB-UPDATE.
137700
137800     MOVE "C"                        TO BNBEN-FC.
137900     MOVE "A"                        TO BNBEN-LINE-FC (1).
138000     MOVE 1                          TO BNBEN-NBR-LINES.
138100
138200 1232-END.
138300
138400******************************************************************
138500 1240-EDIT-AVAIL-DOL.
138600******************************************************************
138700
138800     PERFORM 1242-UPDATE-PERIODS
138900     THRU    1242-END.
139000
139100     IF (BNBEN-POS-PREMIUM-SW (1)    = WS-TRUE)
139200         PERFORM 1244-UPDATE-AVAILABLE
139300         THRU    1244-END.
139400
139500 1240-END.
139600
139700******************************************************************
139800 1242-UPDATE-PERIODS.
139900******************************************************************
140000
140100*     IF  (BNBEN-NEG-PREMIUM-SW (1)   = WS-TRUE)
140200*     AND (PLN-FLEX-PAY-CODE          NOT = SPACES)
140300*         GO TO 1242-END.
140400
140500*     IF  (BNBEN-START-DATE (1)       > WS-START-DT (I4 I5))
140600*     AND (BNBEN-STOP-DATE (1)        < WS-STOP-DT (I4 I5))
140700*         MOVE WS-STOP-DT (I4 I5)     TO BNWS-SAVE-STOP-DATE
140800*         MOVE BNBEN-START-DATE (1)   TO WSDR-FR-DATE
140900*         PERFORM 900-DATE-TO-JULIAN
141000*         SUBTRACT 1                  FROM WSDR-JULIAN-DAYS
141100*         PERFORM 900-JULIAN-TO-DATE   
141200*         MOVE WSDR-FR-DATE           TO WS-STOP-DT (I4 I5)
141300*         MOVE I5                     TO I8
141400*         PERFORM 
141500*             VARYING I5 FROM 1 BY 1
141600*             UNTIL  (WS-START-DT (I4 I5) = ZEROES)
141700
141800*             CONTINUE
141900*         END-PERFORM
142000*         MOVE BNBEN-STOP-DATE (1)    TO WSDR-FR-DATE
142100*         PERFORM 900-DATE-TO-JULIAN
142200*         ADD 1                       TO WSDR-JULIAN-DAYS
142300*         PERFORM 900-JULIAN-TO-DATE   
142400*         MOVE WSDR-FR-DATE           TO WS-START-DT (I4 I5)
142500*         MOVE BNWS-SAVE-STOP-DATE    TO WS-STOP-DT (I4 I5)
142600*         MOVE WS-CREDITS-AVAIL (I4 I8)
142700*                                     TO WS-CREDITS-AVAIL (I4 I5)
142800*         MOVE WS-PRE-TAX-AVAIL (I4 I8)
142900*                                     TO WS-PRE-TAX-AVAIL (I4 I5)
143000*         ADD 1                       TO I5
143100*         MOVE BNBEN-START-DATE (1)   TO WS-START-DT (I4 I5)
143200*         MOVE BNBEN-STOP-DATE  (1)   TO WS-STOP-DT (I4 I5)
143300*         MOVE WS-CREDITS-AVAIL (I4 I8)
143400*                                     TO WS-CREDITS-AVAIL (I4 I5)
143500*         MOVE WS-PRE-TAX-AVAIL (I4 I8)
143600*                                     TO WS-PRE-TAX-AVAIL (I4 I5)
143700*     ELSE  
143800*     IF (BNBEN-START-DATE (1)        > WS-START-DT (I4 I5))
143900*         MOVE WS-STOP-DT (I4 I5)     TO BNWS-SAVE-STOP-DATE
144000*         MOVE BNBEN-START-DATE (1)   TO WSDR-FR-DATE
144100*         PERFORM 900-DATE-TO-JULIAN
144200*         SUBTRACT 1                  FROM WSDR-JULIAN-DAYS
144300*         PERFORM 900-JULIAN-TO-DATE    
144400*         MOVE WSDR-FR-DATE           TO WS-STOP-DT (I4 I5)
144500*         MOVE I5                     TO I8
144600*         PERFORM 
144700*             VARYING I5 FROM 1 BY 1
144800*             UNTIL  (WS-START-DT (I4 I5) = ZEROES)
144900*
145000*             CONTINUE
145100*         END-PERFORM
145200*         MOVE BNBEN-START-DATE (1)   TO WS-START-DT (I4 I5)
145300*         MOVE BNWS-SAVE-STOP-DATE    TO WS-STOP-DT (I4 I5)
145400*         MOVE WS-CREDITS-AVAIL (I4 I8)
145500*                                     TO WS-CREDITS-AVAIL (I4 I5)
145600*         MOVE WS-PRE-TAX-AVAIL (I4 I8)
145700*                                     TO WS-PRE-TAX-AVAIL (I4 I5)
145800*     END-IF
145900*     IF (BNBEN-STOP-DATE (1)         < WS-STOP-DT (I4 I5))
146000*         MOVE WS-STOP-DT (I4 I5)     TO BNWS-SAVE-STOP-DATE
146100*         MOVE BNBEN-STOP-DATE (1)    TO WS-STOP-DT (I4 I5)
146200*         MOVE I5                     TO I8
146300*         PERFORM 
146400*             VARYING I5 FROM 1 BY 1
146500*             UNTIL  (WS-START-DT (I4 I5) = ZEROES)
146600
146700*             CONTINUE
146800*         END-PERFORM
146900*         MOVE BNBEN-STOP-DATE (1)    TO WSDR-FR-DATE
147000*         PERFORM 900-DATE-TO-JULIAN
147100*         ADD 1                       TO WSDR-JULIAN-DAYS
147200*         PERFORM 900-JULIAN-TO-DATE    
147300*         MOVE WSDR-FR-DATE           TO WS-START-DT (I4 I5)
147400*         MOVE BNWS-SAVE-STOP-DATE    TO WS-STOP-DT (I4 I5)
147500*         MOVE WS-CREDITS-AVAIL (I4 I8)
147600*                                     TO WS-CREDITS-AVAIL (I4 I5)
147700*         MOVE WS-PRE-TAX-AVAIL (I4 I8)
147800*                                     TO WS-PRE-TAX-AVAIL (I4 I5).
147900
148000 1242-END.
148100
148200******************************************************************
148300 1244-UPDATE-AVAILABLE.
148400******************************************************************
148500
148600*     IF (PLN-FLEX-DED-CODE           NOT = SPACES)
148700*         IF (BNBEN-EMP-PRE-CONT (1)  <= WS-CREDITS-AVAIL (I4 I6))
148800*             MOVE BNBEN-EMP-PRE-CONT (1) 
148900*                                     TO BNBEN-CMP-FLX-CONT (1) 
149000*             COMPUTE WS-CREDITS-AVAIL (I4 I6)
149100*                                     = WS-CREDITS-AVAIL (I4 I6)
149200*                                     - BNBEN-EMP-PRE-CONT (1) 
149300*             COMPUTE WS-TOTAL-AVAIL  = WS-CREDITS-AVAIL (I4 I6)
149400*                                     + WS-PRE-TAX-AVAIL (I4 I6)
149500*         ELSE
149600*             MOVE WS-CREDITS-AVAIL (I4 I6)
149700*                                     TO BNBEN-CMP-FLX-CONT (1) 
149800*             COMPUTE WS-PRE-TAX-AVAIL (I4 I6)
149900*                                     = WS-PRE-TAX-AVAIL (I4 I6)
150000*                                     - (BNBEN-EMP-PRE-CONT (1) 
150100*                                     -  WS-CREDITS-AVAIL (I4 I6))
150200*             INITIALIZE WS-CREDITS-AVAIL (I4 I6) 
150300*             COMPUTE WS-TOTAL-AVAIL  = WS-CREDITS-AVAIL (I4 I6)
150400*                                     + WS-PRE-TAX-AVAIL (I4 I6)
150500*             IF (PLN-PRE-DED-CODE-A  = SPACES)
150600**************** Spending pre-tax dollars;assign pre-tax ded
150700*                 MOVE 120            TO CRT-ERROR-NBR
150800*                 GO TO 1244-END
150900*             END-IF
151000*         END-IF
151100*     ELSE
151200*     IF (BNBEN-EMP-PRE-CONT (1)      <= WS-PRE-TAX-AVAIL (I4 I6))
151300*         INITIALIZE BNBEN-CMP-FLX-CONT (1) 
151400*         COMPUTE WS-PRE-TAX-AVAIL (I4 I6)
151500*                                     = WS-PRE-TAX-AVAIL (I4 I6)
151600*                                     - BNBEN-EMP-PRE-CONT (1) 
151700*         MOVE WS-PRE-TAX-AVAIL (I4 I6) TO WS-TOTAL-AVAIL
151800*     ELSE
151900*         INITIALIZE BNBEN-CMP-FLX-CONT (1) 
152000*         COMPUTE WS-TOTAL-AVAIL      = WS-PRE-TAX-AVAIL (I4 I6)
152100*                                     - BNBEN-EMP-PRE-CONT (1) .
152200
152300*     IF  (WS-TOTAL-AVAIL             < ZEROES)
152400*     AND (BNBEN-POS-PREMIUM-SW (1)   = WS-TRUE)
152500******** Amount exceeds available dollars; Cannot add
152600*         MOVE 121                    TO CRT-ERROR-NBR
152700*         MOVE WS-TOTAL-AVAIL         TO WS-TOTAL-AVAIL-A
      *         MOVE WS-TOTAL-AVAIL-A       TO HRWS-VALUE
      *         PERFORM 772-HR-INTL-FORMAT-NBR
      *         MOVE HRWS-VALUE             TO CRT-ERR-VAR1
152900*         GO TO 1244-END.
153000
153100 1244-END.
153200
153300******************************************************************
153400 1900-CREATE-BN101BENA.
153500******************************************************************
153600
           IF (BNBEN-LINE-FC (I1)          NOT = "A")
           OR (BNBEN-FUTURE-ERROR-SW (I1)  = "Y")
               GO TO 1900-END.

153700     PERFORM 8000-CREATE-BN101BENA.
153800
153900     PERFORM 1902-MOVE-TO-IDX
154000     THRU    1902-END.
154100
154200     PERFORM 8200-STORE-BN101BENA.
154300
154400 1900-END.
154500
154600******************************************************************
154700 1902-MOVE-TO-IDX.
154800******************************************************************
154900
155000     MOVE PRM-COMPANY                TO IDX-COMPANY.
155100
155200     MOVE EMP-EMPLOYEE               TO IDX-EMPLOYEE.
155300
155400     MOVE PLN-PLAN-TYPE              TO IDX-PLAN-TYPE.
155500     MOVE PLN-PLAN-CODE              TO IDX-PLAN-CODE.
155600
155700     MOVE BNBEN-START-DATE (I1)      TO IDX-START-DATE.
155800
155900     IF (PRM-REPORT-SEQ              = "2")
156000         MOVE EMP-PROCESS-LEVEL      TO IDX-PROC-LEVEL
156100         MOVE EMP-DEPARTMENT         TO IDX-DEPARTMENT.
156200
156300     MOVE EMP-LAST-NAME              TO IDX-LAST-NAME.
156400     MOVE EMP-FIRST-NAME             TO IDX-FIRST-NAME.
156500     MOVE EMP-MIDDLE-INIT            TO IDX-MIDDLE-INIT.
156600
156700     MOVE BNBEN-STOP-DATE (I1)       TO IDX-STOP-DATE.
156800
156900     MOVE BNBEN-COVER-OPT (I1)       TO IDX-COV-OPTION.
157000     MOVE BNBEN-MULT-SALARY (I1)     TO IDX-MULTIPLE.
157100     MOVE BNBEN-COVER-AMT (I1)       TO IDX-COVER-AMT.
157200     MOVE BNBEN-DEP-COVER-AMT (I1)   TO IDX-DEP-COVER-AMT.
157300     MOVE BNBEN-PAY-RATE (I1)        TO IDX-PAY-RATE.
J67795*    MOVE BNBEN-CMP-FLX-CONT (I1)    TO IDX-CMP-FLX-CONT.
J67795     COMPUTE IDX-CMP-FLX-CONT ROUNDED =
J67795          BNBEN-CMP-FLX-CONT (I1) * 1.
J67795*    MOVE BNBEN-EMP-PRE-CONT (I1)    TO IDX-EMP-PRE-CONT.
J67795     COMPUTE IDX-EMP-PRE-CONT ROUNDED = 
J67795          BNBEN-EMP-PRE-CONT (I1) * 1.
J67795*    MOVE BNBEN-EMP-AFT-CONT (I1)    TO IDX-EMP-AFT-CONT.
J67795     COMPUTE IDX-EMP-AFT-CONT ROUNDED = 
J67795          BNBEN-EMP-AFT-CONT (I1) * 1.
J67795*    MOVE BNBEN-COMP-CONT (I1)       TO IDX-COMP-CONT.
J67795     COMPUTE IDX-COMP-CONT ROUNDED = 
J67795          BNBEN-COMP-CONT (I1) * 1.
157800     MOVE BNBEN-PCT-AMT-FLAG (I1)    TO IDX-PCT-AMT-FLAG.
157900     MOVE BNBEN-PRE-AFT-FLAG (I1)    TO IDX-PRE-AFT-FLAG.
158000     MOVE BNBEN-SMOKER-FLAG (I1)     TO IDX-SMOKER.
158100     MOVE BNBEN-COV-OVER-FLG (I1)    TO IDX-COV-OVER-FLG.
158200     MOVE BNBEN-ZERO-PREMIUM-SW (I1) TO IDX-ZERO-PREMIUM-SW.
158300     MOVE BNBEN-NEG-PREMIUM-SW (I1)  TO IDX-NEG-PREMIUM-SW.
158400     MOVE BNBEN-POS-PREMIUM-SW (I1)  TO IDX-POS-PREMIUM-SW.
158500     MOVE BNBEN-SPEND-ONLY (I1)      TO IDX-SPEND-ONLY.
158600     MOVE BNBEN-FLEX-FLAG (I1)       TO IDX-FLEX-FLAG.
158700     MOVE BNBEN-ELIG-UPD-DT (I1)     TO IDX-ELIG-UPD-DT.
158800     MOVE BNBEN-ELIG-GROUP (I1)      TO IDX-ELIG-GROUP.
158900     MOVE BNBEN-COV-UPD-DT (I1)      TO IDX-COV-UPD-DT.
159000     MOVE BNBEN-COV-GROUP (I1)       TO IDX-COV-GROUP.
159100     MOVE BNBEN-PREM-UPD-DT (I1)     TO IDX-PREM-UPD-DT.
159200     MOVE BNBEN-PREM-GROUP (I1)      TO IDX-PREM-GROUP.
159300     MOVE BNBEN-BNA-START-DATE (I1)  TO IDX-GL-UPD-DT.
159400     MOVE BNBEN-BNA-GROUP-NAME (I1)  TO IDX-GL-GROUP.
159500     MOVE BNBEN-EFD-START-DT (I1)    TO IDX-EFD-START-DATE.
159600     MOVE BNBEN-EFD-GROUP-NM (I1)    TO IDX-EFD-GROUP-NAME.
159700     MOVE BNBEN-COV-SAL-DATE (I1)    TO IDX-COV-SAL-DATE.
P38378     MOVE BNBEN-COV-AGE-DATE (I1)    TO IDX-COV-AGE-DATE.
159800     MOVE BNBEN-CNT-SAL-DATE (I1)    TO IDX-CNT-SAL-DATE.
159900     MOVE BNBEN-CNT-AGE-DATE (I1)    TO IDX-CNT-AGE-DATE.
160000     MOVE BNBEN-CNT-SERV-DATE (I1)   TO IDX-CNT-SERV-DATE.
160000     MOVE BNBEN-TRIGGER-ENABLED-SW (I1)
                                           TO IDX-TRIGGER-ENABLED.
160100
160200     MOVE PLN-DEFAULT                TO IDX-DEFAULT.
160300
160400     SET BEN-DATA                    TO TRUE.
160500
160600 1902-END.
160700
160800******************************************************************
160900 1920-CREATE-BN101ERROR.
161000******************************************************************
161100
161200     INITIALIZE ERF-BN101ERROR-REC.
161300
161400     MOVE PRM-COMPANY                TO ERF-COMPANY.
161500
161600     IF (PROC-EFD)
161700         MOVE "AA"                   TO ERF-PLAN-TYPE
161800         IF (EMPFLEXDOL-FOUND)
161900             MOVE EFD-FLEX-PLAN      TO ERF-PLAN-CODE
162000         END-IF
162100     ELSE
162200         MOVE PLN-PLAN-TYPE          TO ERF-PLAN-TYPE
162300         MOVE PLN-PLAN-CODE          TO ERF-PLAN-CODE.
162400
162500     MOVE EMP-EMPLOYEE               TO ERF-EMPLOYEE.
162600
162700     IF (PRM-REPORT-SEQ              = "2")
162800         MOVE EMP-PROCESS-LEVEL      TO ERF-PROC-LEVEL
162900         MOVE EMP-DEPARTMENT         TO ERF-DEPARTMENT.
163000
163100     MOVE EMP-LAST-NAME              TO ERF-LAST-NAME.
163200     MOVE EMP-FIRST-NAME             TO ERF-FIRST-NAME.
163300     MOVE EMP-MIDDLE-INIT            TO ERF-MIDDLE-INIT.
163400
163500     MOVE CRT-ERROR-NBR              TO ERF-ERROR-NBR.
163600     MOVE CRT-ERROR-CAT              TO ERF-ERROR-CAT.
163700     MOVE CRT-ERR-VAR1               TO ERF-ERR-VAR1.
163800     MOVE CRT-ERR-VAR2               TO ERF-ERR-VAR2.
163900     MOVE CRT-ERR-VAR3               TO ERF-ERR-VAR3.
164000
164100     WRITE BN101ERROR-REC            FROM ERF-BN101ERROR-REC.
164200
164300     SET ERR-DATA                    TO TRUE.
164400
164500     INITIALIZE CRT-ERROR-NBR
164600                CRT-ERROR-CAT
164700                CRT-ERR-VAR1
164800                CRT-ERR-VAR2
164900                CRT-ERR-VAR3.
165000
165100 1920-END.
165200
165300******************************************************************
165400 1930-CREATE-BN101ELIG.
165500******************************************************************
165600
165700     INITIALIZE ELG-BN101ELIG-REC.
165800
165900     MOVE PLN-COMPANY                TO ELG-COMPANY.
166000     MOVE PLN-PLAN-TYPE              TO ELG-PLAN-TYPE.
166100     MOVE PLN-PLAN-CODE              TO ELG-PLAN-CODE.
166200
166300     MOVE EMP-EMPLOYEE               TO ELG-EMPLOYEE.
166400
166500     MOVE BNEDWS-ELIGIBILITY-DATE    TO ELG-ELIG-DATE.
166600
166700     IF (PRM-REPORT-SEQ              = "2")
166800         MOVE EMP-PROCESS-LEVEL      TO ELG-PROC-LEVEL
166900         MOVE EMP-DEPARTMENT         TO ELG-DEPARTMENT.
167000
167100     MOVE EMP-LAST-NAME              TO ELG-LAST-NAME.
167200     MOVE EMP-FIRST-NAME             TO ELG-FIRST-NAME.
167300     MOVE EMP-MIDDLE-INIT            TO ELG-MIDDLE-INIT.
167400
167500     WRITE BN101ELIG-REC             FROM ELG-BN101ELIG-REC.
167600
167700     SET ELIG-DATA                   TO TRUE.
167800
167900 1930-END.
168000
168100******************************************************************
168200 1000-END.
168300******************************************************************
168400
168500******************************************************************
168600 2000-PROCESS-BN101BENA          SECTION 50.
168700******************************************************************
168800 2000-START.
168900
169000**** Processing BN101 - Updating benefits
169100     MOVE 055                        TO CRT-MSG-NBR.
169200     PERFORM 780-DISPLAY-MSG.
169300
169400     INITIALIZE IDX-BN101BENA-KEY.
169500     PERFORM 8500-FIND-NLT-BN101BENA.
169600
169700     IF (WS-RECORD-COUNT             NOT = ZEROES)
169800         PERFORM 2050-READ-BN101BENA
169900         THRU    2050-END
170000             WS-RECORD-COUNT TIMES.
170100
170200     MOVE IDX-COMPANY                TO WS-BEN-COMPANY.
170300
170400     PERFORM 2100-UPD-IDX-COMPANY
170500     THRU    2100-END
170600         UNTIL (BN101BENA-NOTFOUND)
170700         OR    (IDX-COMPANY          NOT = WS-BEN-COMPANY).
170800
170900     GO TO 2000-END.
171000
171100******************************************************************
171200 2050-READ-BN101BENA.
171300******************************************************************
171400
171500     PERFORM 8600-FIND-NEXT-BN101BENA.
171600
171700 2050-END.
171800
171900******************************************************************
172000 2100-UPD-IDX-COMPANY.
172100******************************************************************
172200
172300     INITIALIZE WS-NAV-POINTER.
172400
172500     MOVE IDX-EMPLOYEE               TO WS-BEN-EMPLOYEE
172600                                        DB-EMPLOYEE.
172700     PERFORM 840-FIND-EMPSET1.
172800
172900     PERFORM 2110-FIND-NAVIGATE
173000     THRU    2110-END.
173100
173200     IF (USE-NAV-SEQ)
173300         PERFORM 2200-UPD-IDX-PLAN-TYPE-NAV
173400         THRU    2200-END
173500
173600         PERFORM 2105-FIND-NEXT-EMP-IN-BENA
173700         THRU    2105-END
173800     ELSE
173900     IF (PRM-REPORT-SEQ              = 1)
174000         PERFORM 2300-UPD-IDX-PLAN-TYPE
174100         THRU    2300-END
174200             UNTIL (BN101BENA-NOTFOUND)
174300             OR    (IDX-COMPANY      NOT = WS-BEN-COMPANY)
174400             OR    (IDX-EMPLOYEE     NOT = WS-BEN-EMPLOYEE)
174500     ELSE
174600         PERFORM 2300-UPD-IDX-PLAN-TYPE
174700         THRU    2300-END
174800             UNTIL (BN101BENA-NOTFOUND)
174900             OR    (IDX-COMPANY      NOT = WS-BEN-COMPANY)
175000             OR    (IDX-PROC-LEVEL   NOT = EMP-PROCESS-LEVEL)
175100             OR    (IDX-DEPARTMENT   NOT = EMP-DEPARTMENT)
175200             OR    (IDX-EMPLOYEE     NOT = WS-BEN-EMPLOYEE).
175300
175400     PERFORM 840-MODIFY-CKPSET1.
175500     MOVE WS-RECORD-COUNT            TO WS-REST-COUNT.
175600     INITIALIZE WS-REST-NAV-POINTER.
175700     MOVE WS-REST-DATA               TO CKP-RESTART-INFO.
175800     PERFORM 820-STORE-CKPOINT.
175900     PERFORM 925-AUDIT-END.
176000     PERFORM 910-AUDIT-BEGIN.
176100
176200 2100-END.
176300
176400******************************************************************
176500 2105-FIND-NEXT-EMP-IN-BENA.
176600******************************************************************
176700
176800     MOVE WS-BEN-COMPANY             TO IDX-COMPANY.
176900     MOVE WS-BEN-EMPLOYEE            TO IDX-EMPLOYEE.
177000     MOVE EMP-PROCESS-LEVEL          TO IDX-PROC-LEVEL.
177100     MOVE EMP-DEPARTMENT             TO IDX-DEPARTMENT.
177200     MOVE HIGH-VALUES                TO IDX-PLAN-TYPE
177300                                        IDX-PLAN-CODE.
177400     MOVE WS-HIGH-VALUES             TO IDX-START-DATE.
177500     PERFORM 8500-FIND-NLT-BN101BENA.
177600
177700 2105-END.
177800
177900******************************************************************
178000 2110-FIND-NAVIGATE.
178100******************************************************************
178200
178300     SET NO-NAV-SEQ                  TO TRUE.
178400
178500     MOVE WS-BEN-COMPANY             TO DB-COMPANY.
178600     INITIALIZE DB-PROCESS-LEVEL.
178700     MOVE WS-BEN-EMPLOYEE            TO DB-EMPLOYEE.
178800     PERFORM 840-FIND-NAVSET1.
178900     IF (NAVIGATE-FOUND)
179000         SET USE-NAV-SEQ             TO TRUE
179100         GO TO 2110-END.
179200
179300     MOVE WS-BEN-COMPANY             TO DB-COMPANY.
179400     MOVE EMP-PROCESS-LEVEL          TO DB-PROCESS-LEVEL.
179500     INITIALIZE DB-EMPLOYEE.
179600     PERFORM 850-FIND-NLT-NAVSET1.
179700     IF  (NAVIGATE-FOUND)
179800     AND (NAV-PROCESS-LEVEL          = EMP-PROCESS-LEVEL)
179900         SET USE-NAV-SEQ             TO TRUE
180000         GO TO 2110-END.
180100
180200     MOVE WS-BEN-COMPANY             TO DB-COMPANY.
180300     INITIALIZE DB-PROCESS-LEVEL
180400                                        DB-EMPLOYEE.
180500     PERFORM 850-FIND-NLT-NAVSET1.
180600     IF  (NAVIGATE-FOUND)
180700     AND (NAV-COMPANY                = PRM-COMPANY)
180800         SET USE-NAV-SEQ             TO TRUE
180900         GO TO 2110-END.
181000
181100 2110-END.
181200
181300******************************************************************
181400 2200-UPD-IDX-PLAN-TYPE-NAV.
181500******************************************************************
181600
181700     IF (WS-NAV-POINTER              = ZEROES)
181800         MOVE 1                      TO WS-NAV-POINTER.
181900
182000     PERFORM
182100         VARYING I9                  FROM WS-NAV-POINTER BY 1
182200         UNTIL  (I9                  > 11)
182300
182400         IF (PRM-REPORT-SEQ         = 1)
182500             INITIALIZE IDX-PROC-LEVEL
182600                        IDX-DEPARTMENT
182700         ELSE
182800             MOVE EMP-PROCESS-LEVEL  TO IDX-PROC-LEVEL
182900             MOVE EMP-DEPARTMENT     TO IDX-DEPARTMENT
183000         END-IF
183100         MOVE WS-BEN-EMPLOYEE        TO IDX-EMPLOYEE
183200         MOVE NAV-PLAN-TYPE (I9)     TO IDX-PLAN-TYPE
183300         INITIALIZE IDX-PLAN-CODE
183400                    IDX-START-DATE
183500         PERFORM 8500-FIND-NLT-BN101BENA
183600
183700         IF (PRM-REPORT-SEQ              = 1)
183800             PERFORM 2300-UPD-IDX-PLAN-TYPE
183900             THRU    2300-END
184000                 UNTIL (BN101BENA-NOTFOUND)
184100                 OR    (IDX-COMPANY      NOT = WS-BEN-COMPANY)
184200                 OR    (IDX-EMPLOYEE     NOT = WS-BEN-EMPLOYEE)
184300                 OR    (IDX-PLAN-TYPE    NOT = NAV-PLAN-TYPE (I9))
184400         ELSE
184500             PERFORM 2300-UPD-IDX-PLAN-TYPE
184600             THRU    2300-END
184700                 UNTIL (BN101BENA-NOTFOUND)
184800                 OR    (IDX-COMPANY      NOT = WS-BEN-COMPANY)
184900                 OR    (IDX-PROC-LEVEL   NOT = EMP-PROCESS-LEVEL)
185000                 OR    (IDX-DEPARTMENT   NOT = EMP-DEPARTMENT)
185100                 OR    (IDX-EMPLOYEE     NOT = WS-BEN-EMPLOYEE)
185200                 OR    (IDX-PLAN-TYPE    NOT = NAV-PLAN-TYPE (I9))
185300         END-IF
185400     END-PERFORM.
185500
185600 2200-END.
185700
185800******************************************************************
185900 2300-UPD-IDX-PLAN-TYPE.
186000******************************************************************
186100
186200     ADD 1                           TO WS-RECORD-COUNT.
186300
186400     IF (IDX-DEFAULT                 = "N")
186500         GO TO 2300-NEXT-BN101BENA.
186600
186700     PERFORM 2310-CALL-BNENT
186800     THRU    2310-END.
186900
187000     IF (PRM-UPDATE-INVEST           = "Y")
187100         PERFORM 2320-CREATE-INVESTMENT
187200         THRU    2320-END.
187300
187400     IF  (PRM-UPDATE-DEPEND          = "Y")
187500     AND (PLN-COVERAGE-TYPE          NOT = "0")
187600     AND (((PLN-PLAN-TYPE            = "HL")
187700     AND   (BNC-DEP-HEALTH           = "Y"))
187800     OR   ((PLN-PLAN-TYPE            = "DN")
187900     AND   (BNC-DEP-DENTAL           = "Y"))
188000     OR   ((PLN-PLAN-TYPE            = "DL")
188100     AND   (BNC-DEP-DEP-LIFE         = "Y")))
188200         PERFORM 2330-CREATE-DEPENDENT
188300         THRU    2330-END.
188400
188500     ADD 5                           TO WS-UPDATE-COUNT.
188600
188700     IF (WS-UPDATE-COUNT             > WS-MAX-OPS-IN-TRAN)
188800         INITIALIZE WS-UPDATE-COUNT
188900         PERFORM 840-MODIFY-CKPSET1
189000         MOVE WS-RECORD-COUNT        TO WS-REST-COUNT
189100         MOVE I9                     TO WS-REST-NAV-POINTER
189200         MOVE WS-REST-DATA           TO CKP-RESTART-INFO
189300         PERFORM 820-STORE-CKPOINT
189400         PERFORM 925-AUDIT-END
189500         PERFORM 910-AUDIT-BEGIN.
189600
189700 2300-NEXT-BN101BENA.
189800     PERFORM 8600-FIND-NEXT-BN101BENA.
189900
190000 2300-END.
190100
190200******************************************************************
190300 2310-CALL-BNENT.
190400******************************************************************
190500
190600     PERFORM 2312-MOVE-TO-BNBEN
190700     THRU    2312-END.
190800
190900     PERFORM 4000-BNBEN-PROCESS-TRAN.
191000
191100 2310-END.
191200
191300******************************************************************
191400 2312-MOVE-TO-BNBEN.
191500******************************************************************
191600
191700     INITIALIZE BNBEN-DETAIL-GROUP
191800                BNBEN-USE-NAVIGATE-SW.
191900
192000     MOVE IDX-COMPANY                TO BNBEN-COMPANY.
192100     MOVE IDX-EMPLOYEE               TO BNBEN-EMPLOYEE (1).
192200     MOVE IDX-PLAN-TYPE              TO BNBEN-PLAN-TYPE (1).
192300     MOVE IDX-PLAN-CODE              TO BNBEN-PLAN-CODE (1).
192400     MOVE IDX-START-DATE             TO BNBEN-START-DATE (1).
192500     MOVE IDX-STOP-DATE              TO BNBEN-STOP-DATE (1).
192600     MOVE IDX-COV-OPTION             TO BNBEN-COVER-OPT (1).
192700     MOVE IDX-MULTIPLE               TO BNBEN-MULT-SALARY (1).
192800     MOVE IDX-COVER-AMT              TO BNBEN-COVER-AMT (1).
192900     MOVE IDX-DEP-COVER-AMT          TO BNBEN-DEP-COVER-AMT (1).
193000     MOVE IDX-PAY-RATE               TO BNBEN-PAY-RATE (1).
193100     MOVE IDX-CMP-FLX-CONT           TO BNBEN-CMP-FLX-CONT (1).
193200     MOVE IDX-EMP-PRE-CONT           TO BNBEN-EMP-PRE-CONT (1).
193300     MOVE IDX-EMP-AFT-CONT           TO BNBEN-EMP-AFT-CONT (1).
193400     MOVE IDX-COMP-CONT              TO BNBEN-COMP-CONT (1).
193500     MOVE IDX-PCT-AMT-FLAG           TO BNBEN-PCT-AMT-FLAG (1).
193600     MOVE IDX-PRE-AFT-FLAG           TO BNBEN-PRE-AFT-FLAG (1).
193700     MOVE IDX-SMOKER                 TO BNBEN-SMOKER-FLAG (1).
193800     MOVE IDX-COV-OVER-FLG           TO BNBEN-COV-OVER-FLG (1).
193900     MOVE IDX-ZERO-PREMIUM-SW        TO BNBEN-ZERO-PREMIUM-SW (1).
194000     MOVE IDX-NEG-PREMIUM-SW         TO BNBEN-NEG-PREMIUM-SW (1).
194100     MOVE IDX-POS-PREMIUM-SW         TO BNBEN-POS-PREMIUM-SW (1).
194200     MOVE IDX-SPEND-ONLY             TO BNBEN-SPEND-ONLY (1).
194300     MOVE IDX-FLEX-FLAG              TO BNBEN-FLEX-FLAG (1).
194400     MOVE IDX-ELIG-UPD-DT            TO BNBEN-ELIG-UPD-DT (1).
194500     MOVE IDX-ELIG-GROUP             TO BNBEN-ELIG-GROUP (1).
194600     MOVE IDX-COV-UPD-DT             TO BNBEN-COV-UPD-DT (1).
194700     MOVE IDX-COV-GROUP              TO BNBEN-COV-GROUP (1).
194800     MOVE IDX-PREM-UPD-DT            TO BNBEN-PREM-UPD-DT (1).
194900     MOVE IDX-PREM-GROUP             TO BNBEN-PREM-GROUP (1).
195000     MOVE IDX-GL-UPD-DT              TO BNBEN-BNA-START-DATE (1).
195100     MOVE IDX-GL-GROUP               TO BNBEN-BNA-GROUP-NAME (1).
195200     MOVE IDX-EFD-START-DATE         TO BNBEN-EFD-START-DT (1).
195300     MOVE IDX-EFD-GROUP-NAME         TO BNBEN-EFD-GROUP-NM (1).
195400     MOVE IDX-COV-SAL-DATE           TO BNBEN-COV-SAL-DATE (1).
P38378     MOVE IDX-COV-AGE-DATE           TO BNBEN-COV-AGE-DATE (1).
195500     MOVE IDX-CNT-SAL-DATE           TO BNBEN-CNT-SAL-DATE (1).
195600     MOVE IDX-CNT-AGE-DATE           TO BNBEN-CNT-AGE-DATE (1).
195700     MOVE IDX-CNT-SERV-DATE          TO BNBEN-CNT-SERV-DATE (1).
160000     MOVE IDX-TRIGGER-ENABLED        TO
                                           BNBEN-TRIGGER-ENABLED-SW (1).
           MOVE PRM-CREATE-TRANS           TO BNBEN-CREATE-TRANS (1).
195800
195900     MOVE "C"                        TO BNBEN-FC.
196000     MOVE "A"                        TO BNBEN-LINE-FC (1).
196100     MOVE 1                          TO BNBEN-NBR-LINES.
196200
196300 2312-END.
196400
196500******************************************************************
196600 2320-CREATE-INVESTMENT.
196700******************************************************************
196800
196900     MOVE IDX-COMPANY            TO DB-COMPANY.
197000     MOVE IDX-PLAN-CODE          TO DB-PLAN-CODE.
197100     INITIALIZE DB-SEQ-NBR.
197200     PERFORM 850-FIND-NLT-BIVSET2.
197300     IF  (BNINVEST-FOUND)
197400     AND (BIV-COMPANY            = DB-COMPANY)
197500     AND (BIV-PLAN-CODE          = DB-PLAN-CODE)
197600         PERFORM 800-CREATE-EMPINVEST
197700
197800         PERFORM 2322-MOVE-TO-EMPINVEST
197900         THRU    2322-END
198000
198100         PERFORM 820-STORE-EMPINVEST.
198200
198300 2320-END.
198400
198500******************************************************************
198600 2322-MOVE-TO-EMPINVEST.
198700******************************************************************
198800
198900     MOVE IDX-COMPANY            TO EMI-COMPANY.
199000     MOVE IDX-EMPLOYEE           TO EMI-EMPLOYEE.
199100     MOVE IDX-PLAN-CODE          TO EMI-PLAN-CODE.
199200     MOVE IDX-START-DATE         TO EMI-START-DATE.
199300
199400     MOVE BIV-SEQ-NBR            TO EMI-SEQ-NBR.
199500
199600     MOVE IDX-START-DATE         TO EMI-DIST-ST-DATE.
199700     MOVE IDX-STOP-DATE          TO EMI-STOP-DATE.
199800
199900     MOVE 100                    TO EMI-DIST-PCT.
200000
200100 2322-END.
200200
200300******************************************************************
200400 2330-CREATE-DEPENDENT.
200500******************************************************************
200600
200700     MOVE EMDSET1-EMPLOYEE           TO WS-DB-BEG-RNG.
200800     PERFORM 850-FIND-BEGRNG-EMDSET1.
200900     IF (EMDEPEND-FOUND)
               MOVE "E"                    TO HRDEP-COVER-TYPE
               INITIALIZE HRDEP-CALC-AGE-DATE
               PERFORM 5110-FIND-DEP-N-STUD-AGE
J07540* (S)POUSE, (D)EPENDENTS, (B)OTH SPOUSE AND DEPS, (P)ARTNER, 
J07540* SPOUSE (O)R PARTNER, PA(R)TNER DEPS, (C) PARTNER AND DEPS, 
J07540* SPOUSE OR PARTNER (A)ND DEPS.
202200         IF  ((PLN-COVERAGE-TYPE     = "1")
J07540         AND  (COP-COV-DEPENDENTS    = "S" OR "D" OR "B" OR
J07540               "P" OR "O" OR "R" OR "C" OR "A"))
202400         OR  ((PLN-COVERAGE-TYPE     = "2")
J07540         AND  (CVR-LIFE-ADD-FLG      = "S" OR "D" OR "B" OR
J07540               "P" OR "O" OR "R" OR "C" OR "A"))
202600             CONTINUE
202700         ELSE
202800             GO TO 2330-END.
202900
203000     MOVE IDX-COMPANY                TO DB-COMPANY.
203100     MOVE IDX-EMPLOYEE               TO DB-EMPLOYEE.
203200     MOVE EMDSET1-EMPLOYEE           TO WS-DB-BEG-RNG.
203300     PERFORM 850-FIND-BEGRNG-EMDSET1.
203400     PERFORM
203500         UNTIL (EMDEPEND-NOTFOUND)
203600
               IF (EMD-ACTIVE-FLAG         = "A")
203700             PERFORM 2332-CREATE-HRDEPBEN
203800             THRU    2332-END
               END-IF
203900
204000         PERFORM 860-FIND-NXTRNG-EMDSET1
204100     END-PERFORM.
204200
204300 2330-END.
204400
204500******************************************************************
204600 2332-CREATE-HRDEPBEN.
204700******************************************************************
204800
J07540* (E)MPLOYEE, (N)ONE, (S)POUSE, (P)ARTNER, SPOUSE (O)R PARTNER
J07540     IF  (EMD-DEP-TYPE               = "D")
J07540     AND (HRDEP-COV-DEPS             = "E" OR "N" OR "S" OR
J07540          "P" OR "O")
J07540         GO TO 2332-END.
J07540* (E)MPLOYEE, (N)ONE, (D)EPENDENTS, PA(R)TNER DEPS
J07540     IF  (EMD-DEP-TYPE               = "S" OR "P")
J07540     AND (HRDEP-COV-DEPS             = "E" OR "N" OR "D" OR "R")
J07540         GO TO 2332-END.
J07540* (P)ARTNER, (C)PARTNER AND DEPS
J07540     IF  (EMD-DEP-TYPE               = "S")
J07540     AND (HRDEP-COV-DEPS             = "P" OR "C")
J07540         GO TO 2332-END.
J07540* (S)POUSE, (B)OTH SPOUSE AND DEPS
J07540     IF  (EMD-DEP-TYPE               = "P")
J07540     AND (HRDEP-COV-DEPS             = "S" OR "B")
J07540         GO TO 2332-END.

           INITIALIZE WS-HDB-STOP-DATE.

206800     MOVE IDX-START-DATE         TO HDB-EMP-START
206900                                    HDB-START-DATE.

207100     PERFORM 2336-CALC-STOP-DATE
207200     THRU    2336-END.

           IF (HDB-START-DATE          NOT = ZEROES)
               MOVE HDB-STOP-DATE      TO WS-HDB-STOP-DATE

204900         PERFORM 800-CREATE-HRDEPBEN

205100         PERFORM 2334-MOVE-TO-HRDEPBEN
205200         THRU    2334-END

205400         PERFORM 820-STORE-HRDEPBEN.
205500
           IF  (PRM-CREATE-TRANS       = "Y")
           AND (BNBEN-CREATE-TRANS (1) = "Y")
               PERFORM 2350-CREATE-BNTRANS
               THRU    2350-END.

205600 2332-END.
205700
205800******************************************************************
205900 2334-MOVE-TO-HRDEPBEN.
206000******************************************************************
206100
206200     MOVE EMD-COMPANY            TO HDB-COMPANY.
206300     MOVE EMD-EMPLOYEE           TO HDB-EMPLOYEE.
206400     MOVE EMD-SEQ-NBR            TO HDB-DEPENDENT.
206500
206600     MOVE IDX-PLAN-TYPE          TO HDB-PLAN-TYPE.
206700     MOVE IDX-PLAN-CODE          TO HDB-PLAN-CODE.
206800     MOVE IDX-START-DATE         TO HDB-EMP-START
206900                                    HDB-START-DATE.
207000
           MOVE WS-HDB-STOP-DATE       TO HDB-STOP-DATE.
207300
           MOVE WS-SYSTEM-DATE-YMD     TO HDB-CREATION-DATE
                                          HDB-UPD-DATE.
           MOVE HHMMSS                 TO HDB-TIME-STAMP
J67329                                    HDB-CREATE-TIME.
           MOVE CRT-USER-NAME          TO HDB-USER-ID
J67329                                    HDB-CREATE-USER-ID.

207400 2334-END.
207500
207600******************************************************************
207700 2336-CALC-STOP-DATE.
207800******************************************************************
207900
J07540     IF (EMD-DEP-TYPE                = "S" OR "P")
208100         MOVE IDX-STOP-DATE          TO HDB-STOP-DATE
208200         GO TO 2336-END.
208300
           INITIALIZE HRDEP-CALC-DEP-N-STUD.
           PERFORM 5200-DEP-AGE-DATE-CALC.

211300     IF (HRDEP-DEP-END-DATE          < BEN-START-DATE)
211400         INITIALIZE HDB-START-DATE
211500                    HDB-STOP-DATE
211600     ELSE
211700     IF (HRDEP-DEP-END-DATE          >= BEN-START-DATE)
211800         IF (HRDEP-DEP-END-DATE      < BEN-STOP-DATE)
211900             MOVE HRDEP-DEP-END-DATE TO HDB-STOP-DATE
212000         ELSE
212100         IF (BEN-STOP-DATE           NOT = ZEROES)
212200             MOVE BEN-STOP-DATE      TO HDB-STOP-DATE
212300         ELSE
212400             MOVE HRDEP-DEP-END-DATE TO HDB-STOP-DATE.
215000
215100     IF  (EMD-DEP-TYPE               = "D")
215200     AND (EMD-DISABLED               = "Y")
215300         MOVE BEN-STOP-DATE          TO HDB-STOP-DATE.
215400
215500 2336-END.
215600
011700******************************************************************
       2350-CREATE-BNTRANS.
011700******************************************************************

           INITIALIZE HRHDB-TRAN-SEQ-NBR.

           MOVE HDB-COMPANY                TO DB-COMPANY.
           MOVE HDB-PLAN-TYPE              TO DB-PLAN-TYPE.
           MOVE HDB-PLAN-CODE              TO DB-PLAN-CODE.
           MOVE HDB-EMPLOYEE               TO DB-EMPLOYEE.
           INITIALIZE DB-PARTICIPNT.
           MOVE HDB-DEPENDENT              TO DB-DEPENDENT.
           MOVE HDB-START-DATE             TO DB-START-DATE.
           MOVE BNTSET1-START-DATE         TO WS-DB-BEG-RNG.
           PERFORM 850-KFIND-BEGRNG-BNTSET1.
           PERFORM
               UNTIL (BNTRANS-KNOTFOUND)

               MOVE BNT-TRAN-SEQ-NBR       TO HRHDB-TRAN-SEQ-NBR

               PERFORM 860-KFIND-NXTRNG-BNTSET1
           END-PERFORM.

           ADD 1                           TO HRHDB-TRAN-SEQ-NBR.

           PERFORM 800-CREATE-BNTRANS.

           MOVE HDB-COMPANY                TO BNT-COMPANY.
           MOVE HDB-PLAN-TYPE              TO BNT-PLAN-TYPE.
           MOVE HDB-PLAN-CODE              TO BNT-PLAN-CODE.
           MOVE HDB-EMPLOYEE               TO BNT-EMPLOYEE.
           MOVE HDB-DEPENDENT              TO BNT-DEPENDENT.
           MOVE HDB-START-DATE             TO BNT-START-DATE.

           MOVE HRHDB-TRAN-SEQ-NBR         TO BNT-TRAN-SEQ-NBR.

           MOVE HDB-EMP-START              TO BNT-EMP-START.

           INITIALIZE BNT-EVENT-CODE.

           MOVE "E"                        TO BNT-COVER-TYPE.
           MOVE 1                          TO BNT-TRAN-STATUS.
           MOVE "A"                        TO BNT-TRAN-ACTION.

           MOVE HRHDB-HIPAA-REASON         TO BNT-TRAN-REASON.

           MOVE PLN-MEMBER-ID              TO BNT-MEMBER-ID.

           MOVE HDB-START-DATE             TO BNT-EFFECT-DATE.

           INITIALIZE BNT-HIPAA-FILE-NBR
                      BNT-FILE-DATE.

           MOVE WS-SYSTEM-DATE-YMD         TO BNT-DATE-STAMP.
           MOVE HHMMSS                     TO BNT-TIME-STAMP.

           MOVE CRT-PROGRAM-CODE           TO BNT-USER-ID.

           PERFORM 820-STORE-BNTRANS.

       2350-END.

215700******************************************************************
215800 2000-END.
215900******************************************************************
216000
216100******************************************************************
216200 3000-PRINT-REPORT               SECTION 50.
216300******************************************************************
216400 3000-START.
216500
216600     INITIALIZE RPT-PAGE-COUNT (BN101-R1).
216700
216800     SET NO-END-OF-FILE          TO TRUE.
216900     READ BN101BENAS-FILE        INTO SRT-BN101BENAS-REC
217000         AT END
217100             SET END-OF-FILE     TO TRUE
217200             GO TO 3000-END.
217300
217400**** Processing BN101 - Printing report
217500     MOVE 056                        TO CRT-MSG-NBR.
217600     PERFORM 780-DISPLAY-MSG.
217700
217800     PERFORM 3010-DO-BEN-COMPANY
217900     THRU    3010-END
218000         UNTIL (END-OF-FILE).
218100
218200     GO TO 3000-END.
218300
218400******************************************************************
218500 3010-DO-BEN-COMPANY.
218600******************************************************************
218700
218800     MOVE PRM-COMPANY            TO G1-PRM-COMPANY
218900                                    WS-BEN-COMPANY.
219000
219100     MOVE SRT-COMPANY            TO DB-COMPANY.
219200     INITIALIZE DB-PROCESS-LEVEL.
219300     PERFORM 840-FIND-PRSSET1.
219400     MOVE PRS-NAME               TO G1-PRS-NAME.
219500
219600     MOVE PRM-UPDATE-OPTION      TO G1-PRM-UPDATE-OPTION.
219700     MOVE PRM-CALC-DATE          TO G1-PRM-CALC-DATE.
219800
219900     PERFORM 3020-DO-BEN-PLAN-CODE
220000     THRU    3020-END
220100         UNTIL (END-OF-FILE)
220200         OR    (SRT-COMPANY      NOT = WS-BEN-COMPANY).
220300
220400 3010-END.
220500
220600******************************************************************
220700 3020-DO-BEN-PLAN-CODE.
220800******************************************************************
220900
221000     MOVE SRT-PLAN-TYPE          TO WS-BEN-PLAN-TYPE
221100                                    G2-BEN-PLAN-TYPE
221200                                    PG2-BEN-PLAN-TYPE
221300                                    DB-PLAN-TYPE.
221400
221500     MOVE SRT-PLAN-CODE          TO WS-BEN-PLAN-CODE
221600                                    G2-BEN-PLAN-CODE
221700                                    PG2-BEN-PLAN-CODE
221800                                    DB-PLAN-CODE.
221900
222000     MOVE SRT-PROC-LEVEL         TO WS-PROC-LEVEL
222100                                    PG2-PROC-LEVEL
222200                                    DB-PROCESS-LEVEL.
222300
222400     MOVE SRT-DEPARTMENT         TO WS-DEPARTMENT
222500                                    PG2-DEPARTMENT
222600                                    DB-DEPARTMENT.
222700
222800     PERFORM 840-FIND-PLNSET1.
222900     MOVE PLN-DESC               TO G2-PLN-DESC
223000                                    PG2-PLN-DESC.
223100
223200     MOVE GN1-BEN-COMPANY        TO RPT-GROUP-REQUEST.
223300     PERFORM 700-PRINT-RPT-GRP.
223400
223500     IF (PRM-REPORT-SEQ          = "1")
223600         MOVE GN2-BEN-PLAN-CODE  TO RPT-GROUP-REQUEST
223700         PERFORM 700-PRINT-RPT-GRP
223800     ELSE
223900         PERFORM 840-FIND-PRSSET1
224000         PERFORM 840-FIND-DPTSET1
224100         MOVE PRS-NAME           TO PG2-PROC-LEVEL-NAME
224200         INITIALIZE PG2-DEPARTMENT-NAME
224300         IF (DEPTCODE-FOUND)
224400             MOVE DPT-NAME       TO PG2-DEPARTMENT-NAME
224500         END-IF
224600
224700         MOVE GN2-PROC-LEVEL     TO RPT-GROUP-REQUEST
224800         PERFORM 700-PRINT-RPT-GRP.
224900
225000     PERFORM 3040-DO-BEN-EMPLOYEE
225100     THRU    3040-END
225200         UNTIL (END-OF-FILE)
225300         OR    (SRT-COMPANY      NOT = PRM-COMPANY)
225400         OR    (SRT-PLAN-TYPE    NOT = WS-BEN-PLAN-TYPE)
225500         OR    (SRT-PLAN-CODE    NOT = WS-BEN-PLAN-CODE)
225600         OR    (SRT-PROC-LEVEL   NOT = WS-PROC-LEVEL)
225700         OR    (SRT-DEPARTMENT   NOT = WS-DEPARTMENT).
225800
225900 3020-END.
226000
226100******************************************************************
226200 3040-DO-BEN-EMPLOYEE.
226300******************************************************************
226400
226500     MOVE SRT-EMPLOYEE           TO WS-BEN-EMPLOYEE
J18922                                    G3-EMPLOYEE-NUMBER
226700                                    DB-EMPLOYEE.
226800
227100     PERFORM 840-FIND-EMPSET1.
227200
227300     MOVE EMP-LAST-NAME          TO HRWS-LAST-NAME.
227400     MOVE EMP-FIRST-NAME         TO HRWS-FIRST-NAME.
227500     MOVE EMP-MIDDLE-INIT        TO HRWS-MIDDLE-INIT.
           MOVE EMP-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE.
           MOVE EMP-NAME-SUFFIX        TO HRWS-NAME-SUFFIX.
227600     PERFORM 750-HR-FORMAT-NAME.
227700
J18922     MOVE HRWS-FORMAT-NAME       TO G3-EMPLOYEE-NAME.
227900
228000     MOVE SRT-START-DATE         TO G3-BEN-START-DATE.
228100     MOVE SRT-COV-OPTION         TO G3-BEN-COV-OPTION.
228200
228300     INITIALIZE G3-COP-COV-DESC.
228400
228500     IF (PLN-COVERAGE-TYPE       = "1")
228600     OR (PLN-CONTRIB-TYPE        = "1" OR "2")
228700         MOVE SRT-COV-OPTION     TO DB-COVERAGE-OPT
228800         PERFORM 840-FIND-COPSET1
228900         MOVE COP-COV-DESC       TO G3-COP-COV-DESC.
229000
229100     MOVE SRT-MULTIPLE           TO G3-BEN-MULTIPLE.
229200     MOVE SRT-COVER-AMT          TO G3-BEN-COVER-AMT.
229300     MOVE SRT-PAY-RATE           TO G3-BEN-PAY-RATE.
229400     MOVE SRT-PCT-AMT-FLAG       TO G3-BEN-PCT-AMT-FLAG.
229500     MOVE SRT-CMP-FLX-CONT       TO G3-BEN-CMP-FLX-CONT.
229600     MOVE SRT-EMP-PRE-CONT       TO G3-BEN-EMP-PRE-CONT.
229700     MOVE SRT-EMP-AFT-CONT       TO G3-BEN-EMP-AFT-CONT.
229800     MOVE SRT-COMP-CONT          TO G3-BEN-COMP-CONT.
229900
230000     MOVE GN3-BEN-START-DATE     TO RPT-GROUP-REQUEST.
230100     PERFORM 700-PRINT-RPT-GRP.
230200
230300     IF (PRM-UPDATE-OPTION       = "U")
230400         PERFORM 3050-PRINT-DEDUCTIONS
230500         THRU    3050-END.
230600
230700 3040-NEXT-BN101BENAS.
230800     READ BN101BENAS-FILE        INTO SRT-BN101BENAS-REC
230900         AT END
231000             SET END-OF-FILE     TO TRUE.
231100
231200 3040-END.
231300
231400******************************************************************
231500 3050-PRINT-DEDUCTIONS.
231600******************************************************************
231700
231800     MOVE SRT-COMPANY                TO DB-COMPANY.
231900     MOVE SRT-PLAN-TYPE              TO DB-PLAN-TYPE.
232000     MOVE SRT-EMPLOYEE               TO DB-EMPLOYEE.
232100     MOVE SRT-START-DATE             TO DB-START-DATE.
232200     MOVE SRT-PLAN-CODE              TO DB-PLAN-CODE.
232300     PERFORM 840-FIND-BENSET1.
232400
232500     IF (BEN-FLEX-DED-SEQ            NOT = ZEROES)
232600         MOVE BEN-COMPANY            TO DB-COMPANY
232700         MOVE BEN-EMPLOYEE           TO DB-EMPLOYEE
232800         MOVE PLN-FLEX-DED-CODE      TO DB-DED-CODE
232900         MOVE BEN-FLEX-DED-SEQ       TO DB-SEQ-NBR
233000         PERFORM 840-FIND-EDMSET1
233100         MOVE PLN-FLEX-DED-CODE      TO G3-PLN-FLEX-DED-CODE
J67795*        MOVE EDM-NEXT-AMOUNT        TO G3-EDM-NEXT-AMOUNT
J67795         COMPUTE G3-EDM-NEXT-AMOUNT ROUNDED = 
J67795              EDM-NEXT-AMOUNT * 1
233300     ELSE
233400         INITIALIZE G3-EDM-NEXT-AMOUNT
233500                    G3-PLN-FLEX-DED-CODE.
233600
233700     IF (BEN-PRE-SEQ-NBR             NOT = ZEROES)
233800         MOVE BEN-COMPANY            TO DB-COMPANY
233900         MOVE BEN-EMPLOYEE           TO DB-EMPLOYEE
234000         IF (BEN-PCT-AMT-FLAG        = "A")
234100             MOVE PLN-PRE-DED-CODE-A TO DB-DED-CODE
234200         ELSE
234300             MOVE PLN-PRE-DED-CODE-P TO DB-DED-CODE
234400         END-IF
234500         MOVE BEN-PRE-SEQ-NBR        TO DB-SEQ-NBR
234600         PERFORM 840-FIND-EDMSET1
234700         MOVE DB-DED-CODE            TO G3-PLN-PRE-DED-CODE-A
J67795*        MOVE EDM-NEXT-AMOUNT        TO G3-1-EDM-NEXT-AMOUNT
J67795         COMPUTE G3-1-EDM-NEXT-AMOUNT ROUNDED = 
J67795              EDM-NEXT-AMOUNT * 1
234900     ELSE
235000         INITIALIZE G3-1-EDM-NEXT-AMOUNT
235100                    G3-PLN-PRE-DED-CODE-A.
235200
235300     IF (BEN-AFT-SEQ-NBR             NOT = ZEROES)
235400         MOVE BEN-COMPANY            TO DB-COMPANY
235500         MOVE BEN-EMPLOYEE           TO DB-EMPLOYEE
235600         IF (BEN-PCT-AMT-FLAG        = "A")
235700             MOVE PLN-AFT-DED-CODE-A TO DB-DED-CODE
235800         ELSE
235900             MOVE PLN-AFT-DED-CODE-P TO DB-DED-CODE
236000         END-IF
236100         MOVE BEN-AFT-SEQ-NBR        TO DB-SEQ-NBR
236200         PERFORM 840-FIND-EDMSET1
236300         MOVE DB-DED-CODE            TO G3-PLN-AFT-DED-CODE-A
J67795*        MOVE EDM-NEXT-AMOUNT        TO G3-2-EDM-NEXT-AMOUNT
J67795         COMPUTE G3-2-EDM-NEXT-AMOUNT ROUNDED = 
J67795              EDM-NEXT-AMOUNT * 1
236500     ELSE
236600         INITIALIZE G3-2-EDM-NEXT-AMOUNT
236700                    G3-PLN-AFT-DED-CODE-A.
236800
236900     IF (BEN-CMP-SEQ-NBR             NOT = ZEROES)
237000         MOVE BEN-COMPANY            TO DB-COMPANY
237100         MOVE BEN-EMPLOYEE           TO DB-EMPLOYEE
               IF (BEN-PCT-AMT-FLAG        = SPACES)
                   IF (PLN-CMP-DED-CODE-A NOT = SPACES)
                       MOVE PLN-CMP-DED-CODE-A TO DB-DED-CODE
                   ELSE
                       MOVE PLN-CMP-DED-CODE-P TO DB-DED-CODE
                   END-IF
               ELSE
237200         IF (BEN-PCT-AMT-FLAG        = "A")
237300             MOVE PLN-CMP-DED-CODE-A TO DB-DED-CODE
237400         ELSE
237500             MOVE PLN-CMP-DED-CODE-P TO DB-DED-CODE
237600         END-IF
               END-IF
237700         MOVE BEN-CMP-SEQ-NBR        TO DB-SEQ-NBR
237800         PERFORM 840-FIND-EDMSET1
237900         IF (EMDEDMASTR-NOTFOUND)
238000             IF (BEN-PCT-AMT-FLAG    = "A")
238100                 MOVE PLN-PRE-DED-MTCH-A TO DB-DED-CODE
238200             ELSE
238300                 MOVE PLN-PRE-DED-MTCH-P TO DB-DED-CODE
238400             END-IF
238500             PERFORM 840-FIND-EDMSET1
238600         END-IF
238700         MOVE DB-DED-CODE            TO G3-PLN-CMP-DED-CODE-A
J67795*        MOVE EDM-NEXT-AMOUNT        TO G3-3-EDM-NEXT-AMOUNT
J67795         COMPUTE G3-3-EDM-NEXT-AMOUNT ROUNDED = 
J67795              EDM-NEXT-AMOUNT * 1
238900     ELSE
239000         INITIALIZE G3-3-EDM-NEXT-AMOUNT
239100                    G3-PLN-CMP-DED-CODE-A.
239200
239300     IF (BEN-CMP-AFT-SEQ             NOT = ZEROES)
239400         MOVE BEN-COMPANY            TO DB-COMPANY
239500         MOVE BEN-EMPLOYEE           TO DB-EMPLOYEE
239600         IF (BEN-PCT-AMT-FLAG        = "A")
239700             MOVE PLN-AFT-DED-MTCH-P TO DB-DED-CODE
239800         ELSE
239900             MOVE PLN-AFT-DED-MTCH-A TO DB-DED-CODE
240000         END-IF
240100         MOVE BEN-CMP-AFT-SEQ        TO DB-SEQ-NBR
240200         PERFORM 840-FIND-EDMSET1
240300         MOVE DB-DED-CODE            TO G3-PLN-AFT-DED-MTCH-A
J67795*        MOVE EDM-NEXT-AMOUNT        TO G3-4-EDM-NEXT-AMOUNT
J67795         COMPUTE G3-4-EDM-NEXT-AMOUNT ROUNDED =
J67795              EDM-NEXT-AMOUNT * 1
240500     ELSE
240600         INITIALIZE G3-4-EDM-NEXT-AMOUNT
240700                    G3-PLN-AFT-DED-MTCH-A.
240800
240900     IF (BEN-FLEX-DED-SEQ            NOT = ZEROES)
241000     OR (BEN-CMP-AFT-SEQ             NOT = ZEROES)
241100     OR (BEN-CMP-SEQ-NBR             NOT = ZEROES)
241200     OR (BEN-AFT-SEQ-NBR             NOT = ZEROES)
241300     OR (BEN-PRE-SEQ-NBR             NOT = ZEROES)
241400         MOVE GN3-BEN-DEDUCTION      TO RPT-GROUP-REQUEST
241500         PERFORM 700-PRINT-RPT-GRP.
241600
241700     MOVE BEN-COMPANY                TO DB-COMPANY.
241800     MOVE BEN-EMPLOYEE               TO DB-EMPLOYEE.
241900     INITIALIZE DB-TIME-GROUP.
242000     MOVE BEN-STM-SEQ-NBR            TO DB-SEQ-NBR.
242100     PERFORM 840-FIND-STMSET2.
242200     IF (STANDTIME-FOUND)
242300         MOVE STM-RATE               TO G3-STM-RATE
242400         MOVE PLN-FLEX-PAY-CODE      TO G3-PLN-FLEX-PAY-CODE
242500
242600         MOVE GN3-BEN-STANDTIME      TO RPT-GROUP-REQUEST
242700         PERFORM 700-PRINT-RPT-GRP.
242800
242900 3050-END.
243000
243100******************************************************************
243200 3000-END.
243300******************************************************************
243400
243500******************************************************************
243600 4000-PRINT-ERROR-REPORT         SECTION 50.
243700******************************************************************
243800 4000-START.
243900
           OPEN OUTPUT ERRORS-FILE.

244000     INITIALIZE RPT-PAGE-COUNT (BN101-R2).
244100
244200     SET NO-END-OF-FILE          TO TRUE.
244300     READ BN101ERROR-FILE        INTO ERF-BN101ERROR-REC
244400         AT END
244500             SET END-OF-FILE     TO TRUE
244600             GO TO 4000-END.
244700
244800**** Processing BN101 - Printing error report
244900     MOVE 057                        TO CRT-MSG-NBR.
245000     PERFORM 780-DISPLAY-MSG.
245100
245200     PERFORM 4010-DO-BEN-COMPANY
245300     THRU    4010-END
245400         UNTIL (END-OF-FILE).
245500
           CLOSE ERRORS-FILE.

245600     GO TO 4000-END.
245700
245800******************************************************************
245900 4010-DO-BEN-COMPANY.
246000******************************************************************
246100
246200     MOVE PRM-COMPANY            TO E1-PRM-COMPANY
246300                                    WS-BEN-COMPANY.
246400
246500     MOVE ERF-COMPANY            TO DB-COMPANY.
246600     INITIALIZE DB-PROCESS-LEVEL.
246700     PERFORM 840-FIND-PRSSET1.
246800     MOVE PRS-NAME               TO E1-PRS-NAME.
246900
247000     MOVE PRM-UPDATE-OPTION      TO E1-PRM-UPDATE-OPTION.
247100
247200     PERFORM 4020-DO-BEN-PLAN-CODE
247300     THRU    4020-END
247400         UNTIL (END-OF-FILE)
247500         OR    (ERF-COMPANY      NOT = PRM-COMPANY).
247600
247700 4010-END.
247800
247900******************************************************************
248000 4020-DO-BEN-PLAN-CODE.
248100******************************************************************
248200
248300     MOVE ERF-PLAN-TYPE          TO WS-BEN-PLAN-TYPE
248400                                    E2-BEN-PLAN-TYPE
248500                                    PE2-BEN-PLAN-TYPE
248600                                    DB-PLAN-TYPE.
248700
248800     MOVE ERF-PLAN-CODE          TO WS-BEN-PLAN-CODE
248900                                    E2-BEN-PLAN-CODE
249000                                    PE2-BEN-PLAN-CODE
249100                                    DB-PLAN-CODE
249200                                    DB-FLEX-PLAN.
249300
249400     MOVE ERF-PROC-LEVEL         TO WS-PROC-LEVEL
249500                                    PE2-PROC-LEVEL
249600                                    DB-PROCESS-LEVEL.
249700
249800     MOVE ERF-DEPARTMENT         TO WS-DEPARTMENT
249900                                    PE2-DEPARTMENT
250000                                    DB-DEPARTMENT.
250100
250200     MOVE EN1-BEN-COMPANY        TO RPT-GROUP-REQUEST.
250300     PERFORM 700-PRINT-RPT-GRP.
250400
250500     INITIALIZE E2-PLN-DESC
250600                PE2-PLN-DESC.
250700
250800     IF (ERF-PLAN-TYPE           NOT = "AA")
250900         PERFORM 840-FIND-PLNSET1
251000         MOVE PLN-DESC           TO E2-PLN-DESC
251100                                    PE2-PLN-DESC
251200     ELSE
251300         MOVE "FL"               TO E2-BEN-PLAN-TYPE
251400                                    PE2-BEN-PLAN-TYPE
251500         IF (ERF-PLAN-CODE       NOT = SPACES)
251600             PERFORM 840-FIND-FLPSET1
251700             MOVE FLP-DESC       TO E2-PLN-DESC
251800                                    PE2-PLN-DESC.
251900
252000     IF (PRM-REPORT-SEQ          = "1")
252100         MOVE EN2-BEN-PLAN-CODE  TO RPT-GROUP-REQUEST
252200         PERFORM 700-PRINT-RPT-GRP
252300     ELSE
252400         PERFORM 840-FIND-PRSSET1
252500         PERFORM 840-FIND-DPTSET1
252600         MOVE PRS-NAME           TO PE2-PROC-LEVEL-NAME
252700         INITIALIZE PE2-DEPARTMENT-NAME
252800         IF (DEPTCODE-FOUND)
252900             MOVE DPT-NAME       TO PE2-DEPARTMENT-NAME
253000         END-IF
253100
253200         MOVE EN2-PROC-LEVEL     TO RPT-GROUP-REQUEST
253300         PERFORM 700-PRINT-RPT-GRP.
253400
253500     PERFORM 4040-DO-BEN-EMPLOYEE
253600     THRU    4040-END
253700         UNTIL (END-OF-FILE)
253800         OR    (ERF-COMPANY      NOT = PRM-COMPANY)
253900         OR    (ERF-PLAN-TYPE    NOT = WS-BEN-PLAN-TYPE)
254000         OR    (ERF-PLAN-CODE    NOT = WS-BEN-PLAN-CODE)
254100         OR    (ERF-PROC-LEVEL   NOT = WS-PROC-LEVEL)
254200         OR    (ERF-DEPARTMENT   NOT = WS-DEPARTMENT).
254300
254400
254500 4020-END.
254600
254700******************************************************************
254800 4040-DO-BEN-EMPLOYEE.
254900******************************************************************
255000
           IF (ERF-ERROR-NBR           = 127)
               GO TO 4040-NEXT-BN101ERROR.

255100     MOVE ERF-EMPLOYEE           TO WS-BEN-EMPLOYEE
J18922                                    E3-EMPLOYEE-NUMBER
255300                                    DB-EMPLOYEE.
255400     IF (EMPLOYEE-NOTFOUND)
255500     OR (EMP-EMPLOYEE            NOT = ERF-EMPLOYEE)
255600         PERFORM 840-FIND-EMPSET1
255700
255800         MOVE EMP-LAST-NAME      TO HRWS-LAST-NAME
255900         MOVE EMP-FIRST-NAME     TO HRWS-FIRST-NAME
256000         MOVE EMP-MIDDLE-INIT    TO HRWS-MIDDLE-INIT
               MOVE EMP-LAST-NAME-PRE  TO HRWS-LAST-NAME-PRE
               MOVE EMP-NAME-SUFFIX    TO HRWS-NAME-SUFFIX
256100         PERFORM 750-HR-FORMAT-NAME.
256200
J18922     MOVE HRWS-FORMAT-NAME       TO E3-EMPLOYEE-NAME.
256400
256500     MOVE ERF-ERROR-NBR          TO CRT-ERROR-NBR.
256600     MOVE ERF-ERROR-CAT          TO CRT-ERROR-CAT.
256700     MOVE ERF-ERR-VAR1           TO CRT-ERR-VAR1.
256800     MOVE ERF-ERR-VAR2           TO CRT-ERR-VAR2.
256900     MOVE ERF-ERR-VAR3           TO CRT-ERR-VAR3.
257000     PERFORM 790-GET-ERROR-MSG.
257100     MOVE CRT-ERROR-MSG          TO E3-ERROR-MESSAGE.
257200
257300     MOVE EN3-BEN-START-DATE     TO RPT-GROUP-REQUEST.
257400     PERFORM 700-PRINT-RPT-GRP.
257500
257600 4040-NEXT-BN101ERROR.
257700     READ BN101ERROR-FILE        INTO ERF-BN101ERROR-REC
257800         AT END
257900             SET END-OF-FILE     TO TRUE.
258000
258100 4040-END.
258200
258300******************************************************************
258400 4000-END.
258500******************************************************************
258600
258700******************************************************************
258800 5000-PRINT-ELIG-REPORT          SECTION 50.
258900******************************************************************
259000 5000-START.
259100
           OPEN OUTPUT ELIG-FILE.

259200     INITIALIZE RPT-PAGE-COUNT (BN101-R3).
259300
259400     SET NO-END-OF-FILE          TO TRUE.
259500     READ BN101ELIG-FILE         INTO ELG-BN101ELIG-REC
259600         AT END
259700             SET END-OF-FILE     TO TRUE
259800             GO TO 5000-END.
259900
260000**** Processing BN101 - Printing eligibility report
260100     MOVE 058                        TO CRT-MSG-NBR.
260200     PERFORM 780-DISPLAY-MSG.
260300
260400     PERFORM 5010-DO-ELG-COMPANY
260500     THRU    5010-END
260600         UNTIL (END-OF-FILE).
260700
           CLOSE ELIG-FILE.

260800     GO TO 5000-END.
260900
261000******************************************************************
261100 5010-DO-ELG-COMPANY.
261200******************************************************************
261300
261400     MOVE PRM-COMPANY            TO P1-PRM-COMPANY.
261500
261600     MOVE ELG-COMPANY            TO DB-COMPANY.
261700     INITIALIZE DB-PROCESS-LEVEL.
261800     PERFORM 840-FIND-PRSSET1.
261900     MOVE PRS-NAME               TO P1-PRS-NAME.
262000
262100     MOVE PRM-UPDATE-OPTION      TO P1-PRM-UPDATE-OPTION.
262200
262300     PERFORM 5020-DO-ELG-PLAN-CODE
262400     THRU    5020-END
262500         UNTIL (END-OF-FILE)
262600         OR    (ELG-COMPANY  NOT = PRM-COMPANY).
262700
262800 5010-END.
262900
263000******************************************************************
263100 5020-DO-ELG-PLAN-CODE.
263200******************************************************************
263300
263400     MOVE ELG-PLAN-TYPE          TO WS-BEN-PLAN-TYPE
263500                                    P2-PLN-PLAN-TYPE
263600                                    PP2-PLN-PLAN-TYPE
263700                                    DB-PLAN-TYPE.
263800
263900     MOVE ELG-PLAN-CODE          TO WS-BEN-PLAN-CODE
264000                                    P2-PLN-PLAN-CODE
264100                                    PP2-PLN-PLAN-CODE
264200                                    DB-PLAN-CODE.
264300
264400     MOVE ELG-PROC-LEVEL         TO WS-PROC-LEVEL
264500                                    PP2-PROC-LEVEL
264600                                    DB-PROCESS-LEVEL.
264700
264800     MOVE ELG-DEPARTMENT         TO WS-DEPARTMENT
264900                                    PP2-DEPARTMENT
265000                                    DB-DEPARTMENT.
265100
265200     MOVE PN1-PLN-COMPANY        TO RPT-GROUP-REQUEST.
265300     PERFORM 700-PRINT-RPT-GRP.
265400
265500     PERFORM 840-FIND-PLNSET1.
265600     MOVE PLN-DESC               TO P2-PLN-DESC
265700                                    PP2-PLN-DESC.
265800
265900     IF (PRM-REPORT-SEQ          = "1")
266000         MOVE PN2-PLN-PLAN-CODE  TO RPT-GROUP-REQUEST
266100         PERFORM 700-PRINT-RPT-GRP
266200     ELSE
266300         PERFORM 840-FIND-PRSSET1
266400         PERFORM 840-FIND-DPTSET1
266500         MOVE PRS-NAME           TO PP2-PROC-LEVEL-NAME
266600         INITIALIZE PP2-DEPARTMENT-NAME
266700         IF (DEPTCODE-FOUND)
266800             MOVE DPT-NAME       TO PP2-DEPARTMENT-NAME
266900         END-IF
267000
267100         MOVE PN2-PROC-LEVEL     TO RPT-GROUP-REQUEST
267200         PERFORM 700-PRINT-RPT-GRP.
267300
267400     PERFORM 5040-DO-ELG-EMPLOYEE
267500     THRU    5040-END
267600         UNTIL (END-OF-FILE)
267700         OR    (ELG-COMPANY      NOT = PRM-COMPANY)
267800         OR    (ELG-PLAN-TYPE    NOT = WS-BEN-PLAN-TYPE)
267900         OR    (ELG-PLAN-CODE    NOT = WS-BEN-PLAN-CODE)
268000         OR    (ELG-PROC-LEVEL   NOT = WS-PROC-LEVEL)
268100         OR    (ELG-DEPARTMENT   NOT = WS-DEPARTMENT).
268200
268300 5020-END.
268400
268500******************************************************************
268600 5040-DO-ELG-EMPLOYEE.
268700******************************************************************
268800
J18922     MOVE ELG-EMPLOYEE           TO P3-EMPLOYEE-NUMBER
269000                                    DB-EMPLOYEE.
269100     IF (EMPLOYEE-NOTFOUND)
269200     OR (EMP-EMPLOYEE            NOT = ELG-EMPLOYEE)
269300         PERFORM 840-FIND-EMPSET1
269400
269500         MOVE EMP-LAST-NAME      TO HRWS-LAST-NAME
269600         MOVE EMP-FIRST-NAME     TO HRWS-FIRST-NAME
269700         MOVE EMP-MIDDLE-INIT    TO HRWS-MIDDLE-INIT
               MOVE EMP-LAST-NAME-PRE  TO HRWS-LAST-NAME-PRE
               MOVE EMP-NAME-SUFFIX    TO HRWS-NAME-SUFFIX
269800         PERFORM 750-HR-FORMAT-NAME.
269900
J18922     MOVE HRWS-FORMAT-NAME       TO P3-EMPLOYEE-NAME.
270100
270200     MOVE ELG-ELIG-DATE          TO P3-ELIG-DATE.
270300
270400     MOVE PN3-PLN-ELIG-DATE      TO RPT-GROUP-REQUEST.
270500     PERFORM 700-PRINT-RPT-GRP.
270600
270700 5040-NEXT-BN101ELIG.
270800     READ BN101ELIG-FILE         INTO ELG-BN101ELIG-REC
270900         AT END
271000             SET END-OF-FILE     TO TRUE.
271100
271200 5040-END.
271300
271400******************************************************************
271500 5000-END.
271600******************************************************************
271700
271800******************************************************************
271900 8000-CREATE-BN101BENA           SECTION.
272000******************************************************************
272100 8000-START.
272200
272300     INITIALIZE IDX-COMPANY
272400                IDX-EMPLOYEE
272500                IDX-PLAN-TYPE
272600                IDX-PLAN-CODE
272700                IDX-START-DATE
272800                IDX-PROC-LEVEL
272900                IDX-DEPARTMENT
273000                IDX-LAST-NAME
273100                IDX-FIRST-NAME
273200                IDX-MIDDLE-INIT
273300                IDX-STOP-DATE
273400                IDX-COV-OPTION
273500                IDX-MULTIPLE
273600                IDX-COVER-AMT
273700                IDX-DEP-COVER-AMT
273800                IDX-PAY-RATE
273900                IDX-CMP-FLX-CONT
274000                IDX-EMP-PRE-CONT
274100                IDX-EMP-AFT-CONT
274200                IDX-COMP-CONT
274300                IDX-PCT-AMT-FLAG
274400                IDX-PRE-AFT-FLAG
274500                IDX-SMOKER
274600                IDX-COV-OVER-FLG
274700                IDX-ZERO-PREMIUM-SW
274800                IDX-NEG-PREMIUM-SW
274900                IDX-POS-PREMIUM-SW
275000                IDX-SPEND-ONLY
275100                IDX-FLEX-FLAG
275200                IDX-ELIG-UPD-DT
275300                IDX-ELIG-GROUP
275400                IDX-COV-UPD-DT
275500                IDX-COV-GROUP
275600                IDX-PREM-UPD-DT
275700                IDX-PREM-GROUP
275800                IDX-GL-UPD-DT
275900                IDX-GL-GROUP
276000                IDX-EFD-START-DATE
276100                IDX-EFD-GROUP-NAME
276200                IDX-COV-SAL-DATE
P38378                IDX-COV-AGE-DATE
276300                IDX-CNT-SAL-DATE
276400                IDX-CNT-AGE-DATE
276500                IDX-CNT-SERV-DATE
276600                IDX-DEFAULT.
276700
276800******************************************************************
276900 8000-END.
277000******************************************************************
277100
277200******************************************************************
277300 8200-STORE-BN101BENA            SECTION.
277400******************************************************************
277500 8200-START.
277600
277700     WRITE BN101BENA-REC
277800         INVALID KEY
277900             MOVE IDX-COMPANY        TO WS1-COMPANY
278000             MOVE IDX-PROC-LEVEL     TO WS1-PROC-LEVEL
278100             MOVE IDX-DEPARTMENT     TO WS1-DEPARTMENT
278200             MOVE IDX-EMPLOYEE       TO WS1-EMPLOYEE
278300             MOVE IDX-PLAN-TYPE      TO WS1-PLAN-TYPE
278400             MOVE IDX-PLAN-CODE      TO WS1-PLAN-CODE
278500             MOVE IDX-START-DATE     TO WS1-START-DATE
278600             MOVE 111                TO CRT-MSG-NBR
278700             PERFORM 780-PRINT-MSG
278800             MOVE WS-WORK-FILE       TO CRT-ERR-VAR1
278900             MOVE 113                TO CRT-MSG-NBR
279000             PERFORM 780-PRINT-MSG
279100             MOVE WS-BN101BENA-KEY   TO CRT-ERR-VAR1
279200             MOVE 114                TO CRT-MSG-NBR
279300             PERFORM 780-PRINT-MSG
279400     END-WRITE.
279500
279600******************************************************************
279700 8200-END.
279800******************************************************************
279900
280000******************************************************************
280100 8500-FIND-NLT-BN101BENA          SECTION.
280200******************************************************************
280300 8500-START.
280400
280500     SET BN101BENA-FOUND              TO TRUE.
280600
280700     START BN101BENA-FILE             KEY NOT < IDX-BN101BENA-KEY
280800         INVALID KEY
280900             SET BN101BENA-NOTFOUND   TO TRUE
281000     END-START.
281100
281200     IF (BN101BENA-FOUND)
281300         PERFORM 8600-FIND-NEXT-BN101BENA.
281400
281500******************************************************************
281600 8500-END.
281700******************************************************************
281800
281900******************************************************************
282000 8600-FIND-NEXT-BN101BENA         SECTION.
282100******************************************************************
282200 8600-START.
282300
282400     SET BN101BENA-FOUND              TO TRUE.
282500
282600     READ BN101BENA-FILE              NEXT RECORD
282700         AT END
282800             SET BN101BENA-NOTFOUND   TO TRUE
282900     END-READ.
283000
283100******************************************************************
283200 8600-END.
283300******************************************************************
283400
