******* PR14PD 27 <220360080>
000100******************************************************************
000200*                             PR14PD                             *
000300******************************************************************
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  345883 | J45883 | DEFINE DESCRIPTION ON MESSAGE MAINTENANCE   *
      *         |        | FOR CALC TYPE 1 TO 7 FOR PR14 FORM.         *
      * -------   ------   ------------------------------------------  *
      * 1366801 | J66801 | 2020 W-4 UPDATES: PHASE TWO                 *
      * -------   ------   ------------------------------------------  *
      * 1406129 | J06129 | REFINE EDIT FOR W4 MARITAL STATUS           *
     ******************************************************************
      *               M O D I F I C A T I O N   L O G:                 *
      ******************************************************************
      *  MG0304  03/04/04  RESTRICT ENTERING TAX WITHHOLDING FOR       *
      *                    MINISTERS ON DISABILITY                     *
      *          10/28/10  REAPPLIED ABOVE CUSTOMIZATIONS AFTER 9.0    *
      *                    APPS UPGRADE.                               *
      *          08/19/11  M. HUNTER - REAPPLIED CUSTOM MODS AFTER     *
      *                    9.0.1 APP UPGRADE                           *
000400******************************************************************
000500 PR14S1-TRANSACTION              SECTION 10.
000600******************************************************************
000700 PR14S1-START.
000800
000900     PERFORM 200-EDIT-TRAN
001000     THRU    200-END.
001100
001200     IF (PR14F1-PT-XMIT-NBR1   = 1)
001300         MOVE 9                TO PR14F1-PT-XMIT-NBR1
001400         GO TO PR14S1-TRANSACTION-END
001500     ELSE
001600     IF (ERROR-FOUND)
001700         GO TO PR14S1-TRANSACTION-END.
001800
001200     IF  (NO-ERROR-FOUND)
001300     AND (PR14F1-FC            = "R")
           AND (PR14F1-PT-FROM-PR134 = ZEROES)
001400         PERFORM 710-SCREEN-XFER
001500         THRU    710-END
001600         GO TO PR14S1-TRANSACTION-END.
001700
001200     IF (NO-ERROR-FOUND)
001300         PERFORM 400-PROCESS-TRAN
001400         THRU    400-END.
001500
001600     GO TO PR14S1-TRANSACTION-END.
001700
001800******************************************************************
001900 200-EDIT-TRAN.
002000******************************************************************
002100
           IF (PR14F1-FC                   = "R")
008400         MOVE PR14F1-EDM-COMPANY     TO DB-COMPANY
008500         INITIALIZE DB-PROCESS-LEVEL
008600         PERFORM 840-FIND-PRSSET1
008700         IF (PRSYSTEM-NOTFOUND)
      ************ Company does not exist
038300             MOVE 100                             TO CRT-ERROR-NBR
                   MOVE PR14F1-EDM-COMPANY-FN           TO CRT-FIELD-NBR
038400             GO TO 200-END
               END-IF
TLPROJ         MOVE PRS-TAX-FILTER         TO HREMP-PRS-TAX-FILTER
008400         MOVE PR14F1-EDM-COMPANY     TO DB-COMPANY
008500         MOVE PR14F1-EDM-EMPLOYEE    TO DB-EMPLOYEE
008600         PERFORM 840-FIND-EMPSET1
008700         IF (EMPLOYEE-NOTFOUND)
      ************ Employee does not exist
038300             MOVE 101                             TO CRT-ERROR-NBR
                   MOVE PR14F1-EDM-EMPLOYEE-FN          TO CRT-FIELD-NBR
038400             GO TO 200-END
               END-IF
TLPROJ         MOVE EMP-TAX-FILTER         TO HREMP-TAX-FILTER
009400         MOVE EMP-COMPANY            TO CRT-COMPANY
009500         MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL
009600         PERFORM 700-HR-EMP-SECURITY
009700         IF (HRWS-EMP-SECURED)
      ************ Not authorized to access employee
009900             MOVE 102                             TO CRT-ERROR-NBR
010000             MOVE PR14F1-EDM-EMPLOYEE-FN          TO CRT-FIELD-NBR
010100             GO TO 200-END
               END-IF
TLPROJ         MOVE EMP-COMPANY            TO DB-COMPANY
TLPROJ         MOVE EMP-PROCESS-LEVEL      TO DB-PROCESS-LEVEL
TLPROJ         PERFORM 840-FIND-PRSSET1
TLPROJ         IF  (PRSYSTEM-FOUND)
TLPROJ         AND (PRS-TAX-FILTER  NOT = ZERO)
TLPROJ             MOVE PRS-TAX-FILTER     TO HREMP-PRS-TAX-FILTER      
TLPROJ         END-IF
TLPROJ         MOVE EMP-COMPANY            TO DB-COMPANY
TLPROJ         MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE      
TLPROJ         PERFORM 840-FIND-PEMSET1
           ELSE
002200         PERFORM 201-MOVE-SCR-TO-WS
002300         THRU    201-END
002400
               SET PREDM-NO-OTD-FOR-ENDED TO TRUE
002500         PERFORM 2000-PREDM-EDIT-TRAN
002600
002700         IF (ERROR-FOUND)
002800             GO TO 200-END.
002900
           IF (PR14F1-FC                = "R")
               MOVE EMP-EMP-STATUS         TO DB-EMP-STATUS
               PERFORM 840-FIND-EMSSET1
               IF  ((EMP-TERM-DATE          NOT = ZEROES)
               OR   ((EMSTATUS-FOUND)
               AND   (EMS-PAY-STATUS (1 : 1) = "N")))
               AND (PR14F1-PT-XMIT-NBR1      = ZEROES)
      ************ Warning, Req Deds will be updated for term emp, Press OK
038200             MOVE 1                  TO PR14F1-PT-XMIT-NBR1
038300             MOVE 114                TO CRT-MSG-NBR
038400             GO TO 200-END
               END-IF
014300         MOVE PR14F1-EDM-COMPANY  TO PRRQC-COMPANY
014400         MOVE PR14F1-EDM-EMPLOYEE TO PRRQC-EMPLOYEE
014500         INITIALIZE PRRQC-DFT-MAR-STAT
014700                    PRRQC-DFT-EXEMPTS
               MOVE PR14F1-EFFECT-DATE  TO PRRQC-EFFECT-DATE
               MOVE PR14F1-END-DATE     TO PRRQC-END-DATE
               MOVE "N"                 TO PRRQC-UPDATE-OPTION
014800         PERFORM 500-REQ-DED-CREATION
               IF (ERROR-FOUND)
                   INITIALIZE PR14F1-PT-FROM-PR134
                   GO TO 200-END.

J46517     IF  (PR14F1-FC                = "C")
J46517     AND (PR14F1-EDM-DFLT-TCA-FLAG = 9)
J46517     AND (PR14F1-EDM-FORMULA-NUMBER  NOT = 
J46517                  PR14F1-EDM-BFORMULA-NUMBER)
J46517************ Cannot change formula on Federal locked-in     
J46517             MOVE 151                             TO CRT-ERROR-NBR
J46517             MOVE PR14F1-EDM-FORMULA-NUMBER-FN    TO CRT-FIELD-NBR
J46517             GO TO 200-END.
J46517    
MG0304
MG0304     MOVE EMP-COMPANY             TO WBHUF-COMPANY.
MG0304     MOVE EMP-EMPLOYEE            TO WBHUF-EMPLOYEE.
MG0304     MOVE "A-ROSTER CODE"         TO WBHUF-FIELD-NAME.
MG0304
MG0304     PERFORM 8000-GET-HR-USER-FIELD.
MG0304
MG0304     IF (PR14F1-FC = "A") AND 
MG0304        (PR14F1-EDM-DED-CODE = "CDSW") AND 
MG0304        (WBHUF-A-VALUE (4:1)   = "M")
MG0304         MOVE "WBMSG"                 TO CRT-ERROR-CAT
MG0304         MOVE 101                     TO CRT-ERROR-NBR
MG0304         MOVE PR14F1-EDM-EMPLOYEE-FN  TO CRT-FIELD-NBR
MG0304         GO TO 200-END
MG0304     END-IF.
MG0304
003000 200-END.
003100
003200******************************************************************
003300 201-MOVE-SCR-TO-WS.
003400******************************************************************
003500
003600     INITIALIZE PREDM-SCR-FIELDS.
003700
003800     IF  ((PR14F1-EDM-COMPANY     NOT = PR14F1-PT-EDM-COMPANY)
003900     AND  (PR14F1-PT-EDM-COMPANY  NOT = ZEROS))
004000     OR  ((PR14F1-EDM-EMPLOYEE    NOT = PR14F1-PT-EDM-EMPLOYEE)
004100     AND  (PR14F1-PT-EDM-EMPLOYEE NOT = ZEROS))
004200     OR  ((PR14F1-EDM-DED-CODE    NOT = PR14F1-PT-EDM-DED-CODE)
004300     AND  (PR14F1-PT-EDM-DED-CODE NOT = SPACES))
004400         MOVE ZEROS              TO PREDM-SEQ-NBR
004500     ELSE
004600         MOVE PR14F1-EDM-SEQ-NBR TO PREDM-SEQ-NBR.
004700
004800     MOVE PR14F1-FC                  TO PREDM-FC.
004900     MOVE PR14F1-FC-FN               TO PREDM-FC-FN.
005000     MOVE PR14F1-EDM-COMPANY         TO PREDM-COMPANY.
005100     MOVE PR14F1-EDM-COMPANY-FN      TO PREDM-COMPANY-FN.
005200     MOVE PR14F1-EDM-EMPLOYEE        TO PREDM-EMPLOYEE.
005300     MOVE PR14F1-EDM-EMPLOYEE-FN     TO PREDM-EMPLOYEE-FN.
005400     MOVE PR14F1-EDM-DED-CODE        TO PREDM-DED-CODE.
005500     MOVE PR14F1-EDM-DED-CODE-FN     TO PREDM-DED-CODE-FN.
005600     MOVE PR14F1-EDM-NEXT-AMOUNT     TO PREDM-NEXT-AMOUNT.
005700     MOVE PR14F1-EDM-NEXT-AMOUNT-FN  TO PREDM-NEXT-AMOUNT-FN.
005800     MOVE PR14F1-EDM-AMOUNT-2        TO PREDM-AMOUNT-2.
005900     MOVE PR14F1-EDM-AMOUNT-2-FN     TO PREDM-AMOUNT-2-FN.
P30095*     MOVE PR14F1-EDM-EXCLUDE-AMT     TO PREDM-AMOUNT-2.
P30095*     MOVE PR14F1-EDM-EXCLUDE-AMT-FN  TO PREDM-AMOUNT-2-FN.
P84870     MOVE PR14F1-EDM-EXCLUDE-AMT     TO PREDM-EXCLUDE-AMT.
P84870     MOVE PR14F1-EDM-EXCLUDE-AMT-FN  TO PREDM-EXCLUDE-AMT-FN.
006000     MOVE PR14F1-EDM-BALANCE-TYPE    TO PREDM-BALANCE-TYPE.
006100     MOVE PR14F1-EDM-BALANCE-TYPE-FN TO PREDM-BALANCE-TYPE-FN.
006200     MOVE PR14F1-EDM-BALANCE-AMT     TO PREDM-BALANCE-AMT.
006300     MOVE PR14F1-EDM-BALANCE-AMT-FN  TO PREDM-BALANCE-AMT-FN.
006400     MOVE PR14F1-EDM-DED-PRIORITY    TO PREDM-DED-PRIORITY.
006500     MOVE PR14F1-EDM-DED-PRIORITY-FN TO PREDM-DED-PRIORITY-FN.
P77923     MOVE PR14F1-EDM-FORMULA-NUMBER  TO PREDM-FORMULA-NUMBER.
P77923     MOVE PR14F1-EDM-FORMULA-NUMBER-FN TO PREDM-FORMULA-NUMBER-FN.
006600     MOVE PR14F1-EDM-ARREARS         TO PREDM-ARREARS.
006700     MOVE PR14F1-EDM-ARREARS-FN      TO PREDM-ARREARS-FN.
006800     MOVE PR14F1-EDM-DED-CYCLE1      TO PREDM-DED-CYCLE1.
006900     MOVE PR14F1-EDM-DED-CYCLE1-FN   TO PREDM-DED-CYCLE1-FN.
007000     MOVE PR14F1-EDM-DED-CYCLE2      TO PREDM-DED-CYCLE2.
007100     MOVE PR14F1-EDM-DED-CYCLE2-FN   TO PREDM-DED-CYCLE2-FN.
007200     MOVE PR14F1-EDM-DED-CYCLE3      TO PREDM-DED-CYCLE3.
007300     MOVE PR14F1-EDM-DED-CYCLE3-FN   TO PREDM-DED-CYCLE3-FN.
007400     MOVE PR14F1-EDM-DED-CYCLE4      TO PREDM-DED-CYCLE4.
007500     MOVE PR14F1-EDM-DED-CYCLE4-FN   TO PREDM-DED-CYCLE4-FN.
007600     MOVE PR14F1-EDM-DED-CYCLE5      TO PREDM-DED-CYCLE5.
007700     MOVE PR14F1-EDM-DED-CYCLE5-FN   TO PREDM-DED-CYCLE5-FN.
007800     MOVE PR14F1-EDM-DED-CYCLE6      TO PREDM-DED-CYCLE6.
007900     MOVE PR14F1-EDM-DED-CYCLE6-FN   TO PREDM-DED-CYCLE6-FN.
008000     MOVE PR14F1-EDM-DED-CYCLE7      TO PREDM-DED-CYCLE7.
008100     MOVE PR14F1-EDM-DED-CYCLE7-FN   TO PREDM-DED-CYCLE7-FN.
008200     MOVE PR14F1-EDM-DED-CYCLE8      TO PREDM-DED-CYCLE8.
008300     MOVE PR14F1-EDM-DED-CYCLE8-FN   TO PREDM-DED-CYCLE8-FN.
008400     MOVE PR14F1-EDM-DED-CYCLE9      TO PREDM-DED-CYCLE9.
008500     MOVE PR14F1-EDM-DED-CYCLE9-FN   TO PREDM-DED-CYCLE9-FN.
008600     MOVE PR14F1-EDM-EFFECT-DATE     TO PREDM-EFFECT-DATE.
008700     MOVE PR14F1-EDM-EFFECT-DATE-FN  TO PREDM-EFFECT-DATE-FN.
008800     MOVE PR14F1-EDM-END-DATE        TO PREDM-END-DATE.
008900     MOVE PR14F1-EDM-END-DATE-FN     TO PREDM-END-DATE-FN.
009000     MOVE PR14F1-EDM-MONTHLY-LIMIT    TO PREDM-MONTHLY-LIMIT.
009100     MOVE PR14F1-EDM-MONTHLY-LIMIT-FN TO PREDM-MONTHLY-LIMIT-FN.
009200     MOVE PR14F1-EDM-PAY-PRD-LIMIT    TO PREDM-PAY-PRD-LIMIT.
009300     MOVE PR14F1-EDM-PAY-PRD-LIMIT-FN TO PREDM-PAY-PRD-LIMIT-FN.
           MOVE PR14F1-EDM-CURRENCY-CODE    TO PREDM-CURRENCY-CODE.
           MOVE PR14F1-EDM-CURRENCY-CODE-FN TO PREDM-CURRENCY-CODE-FN.
           MOVE PR14F1-EDM-CURR-ND          TO PREDM-CURR-ND.
J86292     IF  (PR14F1-TM-BEN-FLAG = "Y")
J86292         MOVE "Y"                        TO PREDM-BN-DED
J86292         MOVE PR14F1-EDM-PCT-MATCHED     TO PREDM-PCT-MATCHED
J86292         MOVE PR14F1-EDM-PCT-MATCHED-FN  TO PREDM-PCT-MATCHED-FN.

009400
009500 201-END.
009600
009700******************************************************************
009800 400-PROCESS-TRAN.
009900******************************************************************
010000
010100     IF (PR14F1-FC = "A" OR "C" OR "D" OR "R")
010200         PERFORM 910-AUDIT-BEGIN
010300         IF (DMS-ABORTED)
010400             GO TO 400-END
010500         END-IF
010600
010700         PERFORM 410-UPDATE
010800         THRU    410-END
010900
011000         PERFORM 920-AUDIT-END
011100     ELSE
011200     IF (PR14F1-FC = "I" OR "N" OR "P")
011300         PERFORM 480-INQUIRE
011400         THRU    480-END.
011500
011600 400-END.
011700
011800******************************************************************
011900 410-UPDATE.
012000******************************************************************
012100
012200     IF (PR14F1-FC = "A" OR "C" OR "D")
012300         PERFORM 3000-PREDM-PROCESS-TRAN
012400         IF  (PR14F1-FC          = "A" OR "C")
012500         AND (PREDM-LINK-CREATED = 0)
012600             PERFORM 600-MOVE-TO-SCREEN
012700             THRU    600-END.
012800
012900     IF (PR14F1-FC = "A")
013000         IF (PREDM-LINK-CREATED = 1)
013100             MOVE "PREDM"          TO CRT-ERROR-CAT
013200             MOVE 134              TO CRT-MSG-NBR
013300         ELSE
013400             MOVE CRT-ADD-COMPLETE TO CRT-MESSAGE
013500     ELSE
013600     IF (PR14F1-FC = "C")
013700         MOVE CRT-CHG-COMPLETE    TO CRT-MESSAGE
               IF  (PREDM-OTD-FOR-ENDED)
                   MOVE 143             TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
               END-IF
013800     ELSE
013900     IF (PR14F1-FC = "D")
014000         MOVE CRT-RECS-DELETED    TO CRT-MESSAGE
014100     ELSE
014200     IF (PR14F1-FC = "R")
               INITIALIZE PR14F1-PT-XMIT-NBR1
014300         MOVE PR14F1-EDM-COMPANY  TO PRRQC-COMPANY
014400         MOVE PR14F1-EDM-EMPLOYEE TO PRRQC-EMPLOYEE
014500         INITIALIZE PRRQC-DFT-MAR-STAT
014700                    PRRQC-DFT-EXEMPTS
               MOVE PR14F1-EFFECT-DATE  TO PRRQC-EFFECT-DATE
               MOVE PR14F1-END-DATE     TO PRRQC-END-DATE
               INITIALIZE PRRQC-UPDATE-OPTION
014800         PERFORM 500-REQ-DED-CREATION
014900         IF (PRRQC-STD-DED-COUNT  = ZEROS)
015000             MOVE 123             TO CRT-MSG-NBR
015100         ELSE
                   MOVE PRRQC-I1        TO PR14WS-NUMBER-N
                   MOVE PR14WS-NUMBER-A TO CRT-ERR-VAR1
                   MOVE PRRQC-I2        TO PR14WS-NUMBER-N
                   MOVE PR14WS-NUMBER-A TO CRT-ERR-VAR2
                   MOVE PRRQC-I3        TO PR14WS-NUMBER-N
                   MOVE PR14WS-NUMBER-A TO CRT-ERR-VAR3
015200             MOVE 124             TO CRT-MSG-NBR
               END-IF
               INITIALIZE PR14F1-PT-FROM-PR134.
015300
015400 410-END.
015500
015600*****************************************************************
015700 480-INQUIRE.
015800******************************************************************
015900
016000     PERFORM 600-MOVE-TO-SCREEN
016100     THRU    600-END.
016200
016300     MOVE CRT-INQ-COMPLETE    TO CRT-MESSAGE.
016400
016500 480-END.
016600
016700******************************************************************
016800 600-MOVE-TO-SCREEN.
016900******************************************************************
017000
017100     MOVE EDM-COMPANY            TO PR14F1-EDM-COMPANY.
017200     MOVE EDM-EMPLOYEE           TO PR14F1-EDM-EMPLOYEE.
017300
017400     MOVE EMP-LAST-NAME          TO HRWS-LAST-NAME.
017500     MOVE EMP-FIRST-NAME         TO HRWS-FIRST-NAME.
017600     MOVE EMP-MIDDLE-INIT        TO HRWS-MIDDLE-INIT.
           MOVE EMP-NAME-SUFFIX        TO HRWS-NAME-SUFFIX.
           MOVE EMP-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE.
017700     PERFORM 750-HR-FORMAT-NAME.
017800     MOVE HRWS-FORMAT-NAME       TO PR14F1-EMP-NAME.
017900
018000     MOVE EDM-DED-CODE           TO PR14F1-EDM-DED-CODE.
018100
018200     MOVE EDM-SEQ-NBR            TO PR14F1-EDM-SEQ-NBR.
018300     MOVE EDM-NEXT-AMOUNT        TO PR14F1-EDM-NEXT-AMOUNT.
018400     MOVE EDM-AMOUNT-2           TO PR14F1-EDM-AMOUNT-2.
P30095     MOVE EDM-EXCLUDE-AMT        TO PR14F1-EDM-EXCLUDE-AMT.
018500     MOVE EDM-BALANCE-TYPE       TO PR14F1-EDM-BALANCE-TYPE.
018600     MOVE EDM-BALANCE-AMT        TO PR14F1-EDM-BALANCE-AMT.
018700     MOVE EDM-DED-PRIORITY       TO PR14F1-EDM-DED-PRIORITY.
018800     MOVE EDM-ARREARS            TO PR14F1-EDM-ARREARS.
018900     MOVE EDM-DED-CYCLE (1)      TO PR14F1-EDM-DED-CYCLE1.
019000     MOVE EDM-DED-CYCLE (2)      TO PR14F1-EDM-DED-CYCLE2.
019100     MOVE EDM-DED-CYCLE (3)      TO PR14F1-EDM-DED-CYCLE3.
019200     MOVE EDM-DED-CYCLE (4)      TO PR14F1-EDM-DED-CYCLE4.
019300     MOVE EDM-DED-CYCLE (5)      TO PR14F1-EDM-DED-CYCLE5.
019400     MOVE EDM-DED-CYCLE (6)      TO PR14F1-EDM-DED-CYCLE6.
019500     MOVE EDM-DED-CYCLE (7)      TO PR14F1-EDM-DED-CYCLE7.
019600     MOVE EDM-DED-CYCLE (8)      TO PR14F1-EDM-DED-CYCLE8.
019700     MOVE EDM-DED-CYCLE (9)      TO PR14F1-EDM-DED-CYCLE9.
019800     MOVE EDM-EFFECT-DATE        TO PR14F1-EDM-EFFECT-DATE.
019900     MOVE EDM-END-DATE           TO PR14F1-EDM-END-DATE.
020000     MOVE EDM-MONTHLY-LIMIT      TO PR14F1-EDM-MONTHLY-LIMIT.
020100     MOVE EDM-PAY-PRD-LIMIT      TO PR14F1-EDM-PAY-PRD-LIMIT.
P77923     MOVE EDM-FORMULA-NUMBER     TO PR14F1-EDM-FORMULA-NUMBER.
J46517     MOVE EDM-FORMULA-NUMBER     TO PR14F1-EDM-BFORMULA-NUMBER.
           MOVE EDM-CURRENCY-CODE      TO PR14F1-EDM-CURRENCY-CODE.
           MOVE EDM-CURR-ND            TO PR14F1-EDM-CURR-ND.
           MOVE EDM-OBJ-ID             TO PR14F1-EDM-OBJ-ID.
J46517     MOVE EDM-DFLT-TCA-FLAG      TO PR14F1-EDM-DFLT-TCA-FLAG.
020200
023800     MOVE PRS-NAME               TO PR14F1-PRS-NAME.
023900
024000     PERFORM 840-FIND-DDCSET1.
024100     MOVE DDC-DESCRIPTION        TO PR14F1-DDC-DESCRIPTION.
024200     MOVE DDC-CHECK-DESC         TO PR14F1-DDC-CHECK-DESC.
024300     MOVE DDC-CALC-TYPE          TO PR14F1-DDC-CALC-TYPE.
           MOVE DDC-COUNTRY-CODE       TO PR14F1-DDC-COUNTRY-CODE
                                          DB-COUNTRY-CODE.
           INITIALIZE PR14F1-INT-COUNTRY-DESC.
           IF (DB-COUNTRY-CODE NOT = SPACES)
               PERFORM 840-FIND-INTSET1
               IF (INSTCTRYCD-FOUND)
                   MOVE INT-COUNTRY-DESC   TO PR14F1-INT-COUNTRY-DESC.
024400
024500     IF (DDC-CALC-TYPE = "E")
024600         MOVE 128                TO CRT-MSG-NBR
024700         PERFORM 790-GET-MSG
024800         MOVE CRT-MESSAGE        TO PR14F1-DDCX-CALC-TYPE.
024900
025000     IF (DDC-CALC-TYPE = "H")
025100         IF (DDC-PAY-CLASS NOT = SPACES)
025200             MOVE DDC-PAY-CLASS  TO CRT-ERR-VAR1
025300             MOVE 132            TO CRT-MSG-NBR
025400         ELSE
025500             MOVE 129            TO CRT-MSG-NBR
025600         END-IF
025700         PERFORM 790-GET-MSG
025800         MOVE CRT-MESSAGE        TO PR14F1-DDCX-CALC-TYPE.
025900
026000     IF (DDC-CALC-TYPE = "N")
026100         MOVE 130                TO CRT-MSG-NBR
026200         PERFORM 790-GET-MSG
026300         MOVE CRT-MESSAGE        TO PR14F1-DDCX-CALC-TYPE.
026400
026500     IF (DDC-CALC-TYPE = "P")
026600         IF (DDC-PAY-CLASS NOT = SPACES)
026700             MOVE DDC-PAY-CLASS  TO CRT-ERR-VAR1
026800             MOVE 133            TO CRT-MSG-NBR
026900         ELSE
027000             MOVE 131            TO CRT-MSG-NBR
027100         END-IF
027200         PERFORM 790-GET-MSG
027300         MOVE CRT-MESSAGE        TO PR14F1-DDCX-CALC-TYPE.
027400
027500     IF (DDC-CALC-TYPE = "T")
027600         MOVE 134                TO CRT-MSG-NBR
027700         PERFORM 790-GET-MSG
027800         MOVE CRT-MESSAGE        TO PR14F1-DDCX-CALC-TYPE.
027900
028000     IF (DDC-CALC-TYPE = "A")
028100         MOVE 135                TO CRT-MSG-NBR
028200         PERFORM 790-GET-MSG
028300         MOVE CRT-MESSAGE        TO PR14F1-DDCX-CALC-TYPE.
028400
028500     IF (DDC-CALC-TYPE = "D")
028600         IF (DDC-PAY-CLASS NOT = SPACES)
028700             MOVE DDC-PAY-CLASS  TO CRT-ERR-VAR1
028800             MOVE 141            TO CRT-MSG-NBR
028900         ELSE
029000             MOVE 138            TO CRT-MSG-NBR
029100         END-IF
029200         PERFORM 790-GET-MSG
029300         MOVE CRT-MESSAGE        TO PR14F1-DDCX-CALC-TYPE.
029400
029500     IF (DDC-CALC-TYPE = "B")
029600         IF (DDC-PAY-CLASS NOT = SPACES)
029700             MOVE DDC-PAY-CLASS  TO CRT-ERR-VAR1
029800             MOVE 142            TO CRT-MSG-NBR
029900         ELSE
030000             MOVE 139            TO CRT-MSG-NBR
030100         END-IF
030200         PERFORM 790-GET-MSG
030300         MOVE CRT-MESSAGE        TO PR14F1-DDCX-CALC-TYPE.
030400
030500     IF (DDC-CALC-TYPE = "X")
030600         MOVE 140                TO CRT-MSG-NBR
030700         PERFORM 790-GET-MSG
030800         MOVE CRT-MESSAGE        TO PR14F1-DDCX-CALC-TYPE.
030900
J45883     IF (DDC-CALC-TYPE = "1")
J45883*--- PR14#152: FLAT AMOUNT + PERCENT OF GROSS PAY
J45883         MOVE 152                TO CRT-MSG-NBR
J45883         PERFORM 790-GET-MSG
J45883         MOVE CRT-MESSAGE        TO PR14F1-DDCX-CALC-TYPE
J45883     END-IF.
J45883
J45883     IF (DDC-CALC-TYPE = "2")
J45883*--- PR14#153: PERCENT OF DISP/NET WITH MIN FLAT AMOUNT
J45883         MOVE 153                TO CRT-MSG-NBR
J45883         PERFORM 790-GET-MSG
J45883         MOVE CRT-MESSAGE        TO PR14F1-DDCX-CALC-TYPE
J45883     END-IF.
J45883
J45883     IF (DDC-CALC-TYPE = "3")
J45883*--- PR14#154: PERCENT OF AVAILABLE WAGES
J45883         MOVE 154                TO CRT-MSG-NBR
J45883         PERFORM 790-GET-MSG
J45883         MOVE CRT-MESSAGE        TO PR14F1-DDCX-CALC-TYPE
J45883     END-IF.
J45883
J45883     IF (DDC-CALC-TYPE = "4")
J45883*--- PR14#155: UK WORKING FAMILY TAX CREADIT     
J45883         MOVE 155                TO CRT-MSG-NBR
J45883         PERFORM 790-GET-MSG
J45883         MOVE CRT-MESSAGE        TO PR14F1-DDCX-CALC-TYPE
J45883     END-IF.
J45883
J45883     IF (DDC-CALC-TYPE = "5")
J45883*--- PR14#156: UK AEO                            
J45883         MOVE 156                TO CRT-MSG-NBR
J45883         PERFORM 790-GET-MSG
J45883         MOVE CRT-MESSAGE        TO PR14F1-DDCX-CALC-TYPE
J45883     END-IF.
J45883
J45883     IF (DDC-CALC-TYPE = "6")
J45883*--- PR14#157: UK COMPS/COSRS PENSIONS            
J45883         MOVE 157                TO CRT-MSG-NBR
J45883         PERFORM 790-GET-MSG
J45883         MOVE CRT-MESSAGE        TO PR14F1-DDCX-CALC-TYPE
J45883     END-IF.
J45883
J45883     IF (DDC-CALC-TYPE = "7")
J45883*--- PR14#158: FLAT AMOUNT + PERCENT PAY > EXCLUDE AMOUNT
J45883         MOVE 158                TO CRT-MSG-NBR
J45883         PERFORM 790-GET-MSG
J45883         MOVE CRT-MESSAGE        TO PR14F1-DDCX-CALC-TYPE
J45883     END-IF.
J45883
           INITIALIZE PR14F1-PT-XMIT-NBR1.

031000 600-END.
031100
090900******************************************************************
091000 710-SCREEN-XFER.
091100******************************************************************
091200
092300     MOVE "I"                    TO CRT-PASS-FC.
091900     MOVE PR14F1-FC              TO CRT-DISPLAY-FC.
092400     MOVE CRT-MANUAL-CF          TO CRT-REQUEST.
092500     INITIALIZE CRT-MESSAGE.
092600     MOVE PR14F1-FC-FN           TO CRT-FIELD-NBR.
092900     MOVE "PR134"                TO CRT-SCREEN-CODE.
093100
101100 710-END.
101200
031200******************************************************************
031300 PR14S1-TRANSACTION-END.
031400******************************************************************
000300******************************************************************
000400******************************************************************
000500 PR14S2-TRANSACTION              SECTION 10.
000600******************************************************************
000700 PR14S2-START.
000800
000900     PERFORM 200-EDIT-TRAN
001000     THRU    200-END.
001100
001200     IF (NO-ERROR-FOUND)
001300         PERFORM 400-PROCESS-TRAN
001400         THRU    400-END.
001500
001600     GO TO PR14S2-TRANSACTION-END.
001700
001800******************************************************************
001900 200-EDIT-TRAN.
002000******************************************************************
002100
J06129     MOVE WS-TRUE                    TO PREDM-W4-MARITAL-SW.

           PERFORM 210-EDIT-ACCESS
           THRU    210-END.

           IF (ERROR-FOUND)
               GO TO 200-END.

           IF (PR14F2-FC = "C")
               PERFORM 230-EDIT-DATA
               THRU    230-END
               GO TO 200-END.

       200-END.

      ******************************************************************
       210-EDIT-ACCESS.
      ******************************************************************

008400     MOVE PR14F2-EDM-COMPANY             TO DB-COMPANY.
008500     INITIALIZE DB-PROCESS-LEVEL.
008600     PERFORM 840-FIND-PRSSET1.
008700     IF (PRSYSTEM-NOTFOUND)
      ************ Company does not exist
038300         MOVE 100                        TO CRT-ERROR-NBR
               MOVE PR14F2-EDM-COMPANY-FN      TO CRT-FIELD-NBR
038400         GO TO 210-END
           END-IF.

           IF (PR14F2-FC = "I")
               MOVE SPACES                TO PR14F2-EDM-TAX-EXEMPT-FLG
               MOVE ZEROES                TO PR14F2-EDM-DFLT-TCA-FLAG
                                             PR14F2-EDM-MARITAL-STATUS
                                             PR14F2-EDM-EXEMPTIONS
                                             PR14F2-EDM-EXEMPT-AMOUNT
                                             PR14F2-EDM-PERS-EXEMPTS
                                             PR14F2-EDM-DEPEND-EXEMPTS
                                             PR14F2-EDM-ADDL-EXEMPTS
                                             PR14F2-EDM-ADDL-EXEMP-AMT
                                             PR14F2-EDM-ADDL-TAX-CODE
                                             PR14F2-EDM-ADDL-RATE
                                             PR14F2-EDM-ADDL-AMOUNT
                                             PR14F2-EDM-FORMULA-NUMBER
J66801                                       PR14F2-RGP-FORM-YEAR
J66801                                       PR14F2-RGP-MULT-JOBS
J66801                                       PR14F2-RGP-DEPENDENTS
J66801                                       PR14F2-RGP-OTHER-AMOUNT
J66801                                       PR14F2-RGP-DEDUCTIONS
           END-IF.

           IF (PR14F2-FC = "N" OR "P")
               MOVE PR14F2-EDM-COMPANY         TO DB-COMPANY
               MOVE PR14F2-EDM-EMPLOYEE        TO DB-EMPLOYEE
               MOVE PR14F2-EDM-DED-CODE        TO DB-DED-CODE
               MOVE ZEROES                     TO DB-SEQ-NBR
               PERFORM 850-FIND-NLT-EDMSET1
               IF (PR14F2-FC = "N")
                   PERFORM 860-FIND-NEXT-EDMSET1
                   PERFORM
                       UNTIL (EMDEDMASTR-NOTFOUND)
                       OR    (EDM-COMPANY       NOT = DB-COMPANY)
                       OR    (EDM-DFLT-TCA-FLAG = 9)
                           PERFORM 860-FIND-NEXT-EDMSET1
                   END-PERFORM
               ELSE
                   PERFORM 870-FIND-PREV-EDMSET1
                   PERFORM
                       UNTIL (EMDEDMASTR-NOTFOUND)
                       OR    (EDM-COMPANY       NOT = DB-COMPANY)
                       OR    (EDM-DFLT-TCA-FLAG = 9)
                           PERFORM 870-FIND-PREV-EDMSET1
                   END-PERFORM
               END-IF.

           IF (PR14F2-FC = "N" OR "P")
               IF (EMDEDMASTR-NOTFOUND)
               OR (EDM-COMPANY   NOT = DB-COMPANY)
                   MOVE 12                     TO CRT-ERROR-NBR
                   MOVE PR14F2-FC-FN           TO CRT-FIELD-NBR
                   GO TO 210-END
               ELSE
                   MOVE EDM-EMPLOYEE      TO PR14F2-EDM-EMPLOYEE
                   MOVE SPACES            TO PR14F2-EDM-TAX-EXEMPT-FLG
                   MOVE ZEROES            TO PR14F2-EDM-DFLT-TCA-FLAG
                                             PR14F2-EDM-MARITAL-STATUS
                                             PR14F2-EDM-EXEMPTIONS
                                             PR14F2-EDM-EXEMPT-AMOUNT
                                             PR14F2-EDM-PERS-EXEMPTS
                                             PR14F2-EDM-DEPEND-EXEMPTS
                                             PR14F2-EDM-ADDL-EXEMPTS
                                             PR14F2-EDM-ADDL-EXEMP-AMT
                                             PR14F2-EDM-ADDL-TAX-CODE
                                             PR14F2-EDM-ADDL-RATE
                                             PR14F2-EDM-ADDL-AMOUNT
                                             PR14F2-EDM-FORMULA-NUMBER
J66801                                       PR14F2-RGP-FORM-YEAR
J66801                                       PR14F2-RGP-MULT-JOBS
J66801                                       PR14F2-RGP-DEPENDENTS
J66801                                       PR14F2-RGP-OTHER-AMOUNT
J66801                                       PR14F2-RGP-DEDUCTIONS
               END-IF.

           IF (EMPLOYEE-NOTFOUND)
           OR (EMP-EMPLOYEE NOT = PR14F2-EDM-COMPANY)
           OR (EMP-COMPANY  NOT = PR14F2-EDM-EMPLOYEE)
008500         MOVE PR14F2-EDM-EMPLOYEE        TO DB-EMPLOYEE
008600         PERFORM 840-FIND-EMPSET1
008700         IF (EMPLOYEE-NOTFOUND)
      ************ Employee does not exist
038300             MOVE 101                    TO CRT-ERROR-NBR
                   MOVE PR14F2-EDM-EMPLOYEE-FN TO CRT-FIELD-NBR
038400             GO TO 210-END
               ELSE
                   IF (EMP-WORK-COUNTRY NOT = HRWS-US-WORK-COUNTRY)
                       MOVE 145                TO CRT-ERROR-NBR
                       MOVE PR14F2-EDM-EMPLOYEE-FN
                                               TO CRT-FIELD-NBR
                       GO TO 210-END
                   END-IF
                   MOVE PR14F2-FC              TO CRT-PROC-LEV-SECURED
                   MOVE EMP-COMPANY            TO CRT-COMPANY
009500             MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL
009600             PERFORM 700-HR-EMP-SECURITY
009700             IF  (HRWS-EMP-SECURED)
                   AND (PR14F2-FC NOT = "N" AND "P")
      ************ Not authorized to access employee
009900                 MOVE 102                TO CRT-ERROR-NBR
010000                 MOVE PR14F2-EDM-EMPLOYEE-FN
                                               TO CRT-FIELD-NBR
010100                 GO TO 210-END
                   ELSE
                   IF  (HRWS-EMP-SECURED)
                   AND (PR14F2-FC = "N" OR "P")
      ************ Not authorized to access employee
                       IF (PR14F2-FC = "N")
                           PERFORM 860-FIND-NEXT-EDMSET1
                           PERFORM 212-GET-NEXT-EMPLOYEE
                           THRU    212-END
                               UNTIL (EMDEDMASTR-NOTFOUND)
                               OR    (EDM-COMPANY   NOT = DB-COMPANY)
                               OR    (HRWS-EMP-NOT-SECURED)
                               OR   ((HRWS-NEXT-COUNT > PRS-SEC-SEARCH)
                               AND   (PRS-SEC-SEARCH  > ZEROS))
                       ELSE
                            PERFORM 870-FIND-PREV-EDMSET1
                            PERFORM 212-GET-NEXT-EMPLOYEE
                            THRU    212-END
                               UNTIL (EMDEDMASTR-NOTFOUND)
                               OR    (EDM-COMPANY   NOT = DB-COMPANY)
                               OR    (HRWS-EMP-NOT-SECURED)
                               OR   ((HRWS-NEXT-COUNT > PRS-SEC-SEARCH)
                               AND   (PRS-SEC-SEARCH  > ZEROS))
                       END-IF
                   END-IF.

           IF  (PR14F2-FC = "N" OR "P")
           AND (HRWS-NEXT-COUNT > PRS-SEC-SEARCH)
           AND (PRS-SEC-SEARCH  > ZEROS)
               MOVE PRS-SEC-SEARCH              TO HRWS-SEARCH-NBR
               MOVE HRWS-SEARCH-VAR             TO CRT-ERR-VAR1
               MOVE 99                          TO CRT-ERROR-NBR
               MOVE PR14F2-FC-FN                TO CRT-FIELD-NBR
               GO TO 210-END.

           IF (PR14F2-FC = "N" OR "P")
               IF (EMDEDMASTR-NOTFOUND)
               OR (EDM-COMPANY   NOT = DB-COMPANY)
                   MOVE 12                          TO CRT-ERROR-NBR
                   MOVE PR14F2-FC-FN                TO CRT-FIELD-NBR
                   GO TO 210-END
               ELSE
                   MOVE EDM-EMPLOYEE           TO PR14F2-EDM-EMPLOYEE
                   MOVE EDM-DED-CODE           TO PR14F2-EDM-DED-CODE
               END-IF.

           MOVE PR14F2-EDM-DED-CODE            TO DB-DED-CODE.
           PERFORM 840-FIND-DDCSET1.
           IF (DEDCODE-NOTFOUND)
               MOVE 144                        TO CRT-ERROR-NBR
               MOVE PR14F2-EDM-DED-CODE-FN     TO CRT-FIELD-NBR
               GO TO 210-END.
        
P86426     IF ((DDC-TAX-AUTH-TYPE NOT = "FD") AND 
P86426         (DDC-TAX-AUTH-TYPE NOT = "ST"))
           OR (DDC-TAX-CATEGORY  NOT = 01)
           OR (DDC-COUNTRY-CODE  NOT = HRWS-US-WORK-COUNTRY)
               MOVE 146                        TO CRT-ERROR-NBR
               MOVE PR14F2-EDM-DED-CODE-FN     TO CRT-FIELD-NBR
               GO TO 210-END
           ELSE  
P58549         MOVE EMP-EMPLOYEE               TO DB-EMPLOYEE
P58549         PERFORM 840-FIND-PEMSET1
P58549         IF (PAEMPLOYEE-FOUND)
P58549             MOVE PEM-LOCAT-CODE         TO DB-LOCAT-CODE
P58549             MOVE PR14F2-EDM-DED-CODE    TO DB-DED-CODE
P58549             PERFORM 840-FIND-PXLSET1
P58549             IF ((PRTAXLOC-FOUND)
P58549             AND (PXL-RECORD-TYPE = "D"))
P58549                  MOVE 147               TO CRT-ERROR-NBR
P58549                  MOVE PR14F2-EDM-DED-CODE-FN
P58549                                         TO CRT-FIELD-NBR
P58549                  GO TO 210-END
P58549             END-IF
P58549         END-IF
           END-IF.

           IF (PR14F2-FC = "I")
               MOVE EDMSET1-DED-CODE TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-EDMSET5
               IF (EMDEDMASTR-NOTFOUND) 
                   MOVE 148                        TO CRT-ERROR-NBR
                   MOVE PR14F2-EDM-DED-CODE-FN     TO CRT-FIELD-NBR
                   GO TO 210-END
               END-IF
           END-IF.

003000 210-END.
003100
      ******************************************************************
       212-GET-NEXT-EMPLOYEE.
      ******************************************************************
           MOVE EDM-EMPLOYEE           TO PR14F2-EDM-EMPLOYEE.
           IF (EDM-EMPLOYEE = DB-EMPLOYEE)
               GO TO 212-NEXT.

           IF (EDM-DFLT-TCA-FLAG NOT = 9)
               GO TO 212-NEXT.

           ADD 1                       TO HRWS-NEXT-COUNT.
           MOVE EDM-EMPLOYEE           TO DB-EMPLOYEE.
           IF (EMPLOYEE-NOTFOUND)
           OR (EDM-COMPANY  NOT = DB-COMPANY)
           OR (EDM-EMPLOYEE NOT = DB-EMPLOYEE)
               PERFORM 840-FIND-EMPSET1.
           IF (EMP-WORK-COUNTRY NOT = HRWS-US-WORK-COUNTRY)
               GO TO 212-NEXT
           END-IF.
           MOVE EMP-COMPANY            TO CRT-COMPANY.
           MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL.
           PERFORM 700-HR-EMP-SECURITY.
           IF (HRWS-EMP-NOT-SECURED)
               GO TO 212-END
           END-IF.

       212-NEXT.
           IF (PR14F2-FC = "P")
               PERFORM 870-FIND-PREV-EDMSET1
           ELSE
               PERFORM 860-FIND-NEXT-EDMSET1.

       212-END.

      ******************************************************************
       230-EDIT-DATA.
      ******************************************************************

           PERFORM 520-MOVE-SCR-TO-WS
           THRU    520-END.

           PERFORM 2000-PREDM-EDIT-TRAN.
           IF (ERROR-FOUND)
               GO TO 230-END.

           PERFORM 530-MOVE-WS-TO-SCR
           THRU    530-END.

       230-END.

009700******************************************************************
009800 400-PROCESS-TRAN.
009900******************************************************************
010000
010100     IF (PR14F2-FC = "C")
010200         PERFORM 910-AUDIT-BEGIN
010300         IF (DMS-ABORTED)
010400             GO TO 400-END
010500         END-IF
010600
010700         PERFORM 410-UPDATE
010800         THRU    410-END
010900
011000         PERFORM 920-AUDIT-END
011100     ELSE
011200     IF (PR14F2-FC = "I" OR "N" OR "P")
011300         PERFORM 480-INQUIRE
011400         THRU    480-END.
011500
011600 400-END.
011700
011800******************************************************************
011900 410-UPDATE.
012000******************************************************************
012100
           PERFORM 520-MOVE-SCR-TO-WS
           THRU    520-END.

012300     PERFORM 3000-PREDM-PROCESS-TRAN.

012600     PERFORM 530-MOVE-WS-TO-SCR
012700     THRU    530-END.
012800
013700     MOVE CRT-CHG-COMPLETE    TO CRT-MESSAGE.
015300
015400 410-END.
015500
015600*****************************************************************
015700 480-INQUIRE.
015800******************************************************************
015900
016000     PERFORM 600-MOVE-TO-SCREEN
016100     THRU    600-END.
016200
016300     MOVE CRT-INQ-COMPLETE    TO CRT-MESSAGE.
016400
016500 480-END.
016600
      ******************************************************************
       520-MOVE-SCR-TO-WS.
      ******************************************************************

           INITIALIZE PREDM-SCR-FIELDS.

           MOVE EMP-WORK-COUNTRY             TO PREDM-COUNTRY-CODE.

           MOVE PR14F2-FC                    TO PREDM-FC.
           MOVE PR14F2-FC-FN                 TO PREDM-FC-FN.
           MOVE PR14F2-EDM-COMPANY           TO PREDM-COMPANY.
           MOVE PR14F2-EDM-COMPANY-FN        TO PREDM-COMPANY-FN.
           MOVE PR14F2-EDM-EMPLOYEE          TO PREDM-EMPLOYEE.
           MOVE PR14F2-EDM-EMPLOYEE-FN       TO PREDM-EMPLOYEE-FN.
           MOVE PR14F2-EDM-DED-CODE          TO PREDM-DED-CODE.
           MOVE PR14F2-EDM-DED-CODE-FN       TO PREDM-DED-CODE-FN.
           MOVE PR14F2-EDM-SEQ-NBR           TO PREDM-SEQ-NBR.
           MOVE PR14F2-EDM-DFLT-TCA-FLAG     TO PREDM-DFLT-TCA-FLAG.
           MOVE PR14F2-EDM-DFLT-TCA-FLAG-FN  TO PREDM-DFLT-TCA-FLAG-FN.
           MOVE PR14F2-EDM-MARITAL-STATUS    TO PREDM-MARITAL-STATUS.
           MOVE PR14F2-EDM-MARITAL-STATUS-FN TO PREDM-MARITAL-STATUS-FN.
           MOVE PR14F2-EDM-EXEMPTIONS        TO PREDM-EXEMPTIONS.
           MOVE PR14F2-EDM-EXEMPTIONS-FN     TO PREDM-EXEMPTIONS-FN.
J66801     MOVE PR14F2-RGP-FORM-YEAR         TO PREDM-FORM-YEAR.
J66801     MOVE PR14F2-RGP-FORM-YEAR-FN      TO PREDM-FORM-YEAR-FN.
J66801     MOVE PR14F2-RGP-MULT-JOBS         TO PREDM-MULT-JOBS.
J66801     MOVE PR14F2-RGP-MULT-JOBS-FN      TO PREDM-MULT-JOBS-FN.
J66801     MOVE PR14F2-RGP-DEPENDENTS        TO PREDM-DEPENDENTS.
J66801     MOVE PR14F2-RGP-DEPENDENTS-FN     TO PREDM-DEPENDENTS-FN.
J66801     MOVE PR14F2-RGP-OTHER-AMOUNT      TO PREDM-OTHER-AMOUNT.
J66801     MOVE PR14F2-RGP-OTHER-AMOUNT-FN   TO PREDM-OTHER-AMOUNT-FN.
J66801     MOVE PR14F2-RGP-DEDUCTIONS        TO PREDM-DEDUCTIONS.
J66801     MOVE PR14F2-RGP-DEDUCTIONS-FN     TO PREDM-DEDUCTIONS-FN.

           MOVE PR14F2-EDM-EXEMPT-AMOUNT     TO PREDM-EXEMPT-AMOUNT.
           MOVE PR14F2-EDM-TAX-EXEMPT-FLG    TO PREDM-TAX-EXEMPT-FLG.
           MOVE PR14F2-EDM-PERS-EXEMPTS      TO PREDM-PERS-EXEMPTS.
           MOVE PR14F2-EDM-DEPEND-EXEMPTS    TO PREDM-DEPEND-EXEMPTS.
           MOVE PR14F2-EDM-ADDL-EXEMPTS      TO PREDM-ADDL-EXEMPTS.
           MOVE PR14F2-EDM-ADDL-EXEMP-AMT    TO PREDM-ADDL-EXEMP-AMT.
           MOVE PR14F2-EDM-ADDL-TAX-CODE     TO PREDM-ADDL-TAX-CODE.
           MOVE PR14F2-EDM-ADDL-RATE         TO PREDM-ADDL-RATE.
           MOVE PR14F2-EDM-ADDL-AMOUNT       TO PREDM-ADDL-AMOUNT.
           MOVE PR14F2-EDM-FORMULA-NUMBER    TO PREDM-FORMULA-NUMBER.

       520-END.

      ******************************************************************
       530-MOVE-WS-TO-SCR.
      ******************************************************************

           MOVE PREDM-DFLT-TCA-FLAG   TO PR14F2-EDM-DFLT-TCA-FLAG.
           MOVE PREDM-MARITAL-STATUS  TO PR14F2-EDM-MARITAL-STATUS.
           MOVE PREDM-EXEMPTIONS      TO PR14F2-EDM-EXEMPTIONS.

           MOVE PREDM-EXEMPT-AMOUNT   TO PR14F2-EDM-EXEMPT-AMOUNT.
           MOVE PREDM-TAX-EXEMPT-FLG  TO PR14F2-EDM-TAX-EXEMPT-FLG.
           MOVE PREDM-PERS-EXEMPTS    TO PR14F2-EDM-PERS-EXEMPTS.
           MOVE PREDM-DEPEND-EXEMPTS  TO PR14F2-EDM-DEPEND-EXEMPTS.
           MOVE PREDM-ADDL-EXEMPTS    TO PR14F2-EDM-ADDL-EXEMPTS.
           MOVE PREDM-ADDL-EXEMP-AMT  TO PR14F2-EDM-ADDL-EXEMP-AMT.
           MOVE PREDM-ADDL-TAX-CODE   TO PR14F2-EDM-ADDL-TAX-CODE.
           MOVE PREDM-ADDL-RATE       TO PR14F2-EDM-ADDL-RATE.
           MOVE PREDM-ADDL-AMOUNT     TO PR14F2-EDM-ADDL-AMOUNT.
           MOVE PREDM-FORMULA-NUMBER  TO PR14F2-EDM-FORMULA-NUMBER.
           MOVE PREDM-DFLT-TCA-FLAG   TO PR14F2-EDM-DFLT-TCA-FLAG.
J66801     MOVE PREDM-FORM-YEAR       TO PR14F2-RGP-FORM-YEAR.
J66801     MOVE PREDM-MULT-JOBS       TO PR14F2-RGP-MULT-JOBS.
J66801     MOVE PREDM-DEPENDENTS      TO PR14F2-RGP-DEPENDENTS.
J66801     MOVE PREDM-OTHER-AMOUNT    TO PR14F2-RGP-OTHER-AMOUNT.
J66801     MOVE PREDM-DEDUCTIONS      TO PR14F2-RGP-DEDUCTIONS.
           
J66801     IF  (DDC-TAX-AUTH-TYPE = "FD" OR "ST")
J66801     AND (DDC-TAX-CATEGORY  = 01)
J66801     AND (DDC-COUNTRY-CODE  = "US")
J66801         MOVE EDM-COMPANY                TO DB-COMPANY
J66801         MOVE EDM-EMPLOYEE               TO DB-EMPLOYEE
J66801         MOVE "US"                       TO DB-COUNTRY-CODE
J66801         MOVE "E"                        TO DB-RECORD-TYPE
J66801         MOVE SPACES                     TO DB-REPORT-ENTITY
J66801         MOVE PR14F2-EDM-DED-CODE        TO DB-TAX-ID-CODE
J66801         MOVE ZEROS                      TO DB-PAYROLL-YEAR
J66801         MOVE "E6"                       TO DB-TOPIC
J66801         MOVE 6001                       TO DB-FLD-NBR
J66801         MOVE ZEROS                      TO DB-SEQ-NBR
J66801         PERFORM 850-FIND-NLT-RGPSET1
J66801         PERFORM
J66801           UNTIL (PRREGPARM-NOTFOUND)
J66801           OR    (RGP-COMPANY     NOT = EDM-COMPANY)
J66801           OR    (RGP-EMPLOYEE    NOT = EDM-EMPLOYEE)
J66801           OR    (RGP-TAX-ID-CODE NOT = PR14F2-EDM-DED-CODE)
J66801           OR    (RGP-TOPIC       NOT = "E6")
J66801           OR    (RGP-FLD-NBR > 6005)
J66801             IF  (RGP-FLD-NBR = 6001)
J66801                 MOVE RGP-S-VALUE    TO PR14F2-RGP-FORM-YEAR
J66801             END-IF
J66801             IF  (RGP-FLD-NBR = 6002)
J66801               IF  (PREDM-MULT-JOBS = ZEROES)
J66801                 MOVE RGP-S-VALUE    TO PR14F2-RGP-MULT-JOBS
J66801               ELSE
J66801                 MOVE PREDM-MULT-JOBS TO PR14F2-RGP-MULT-JOBS
J66801               END-IF
J66801             END-IF
J66801             IF  (RGP-FLD-NBR = 6003)
J66801               IF  (PREDM-DEPENDENTS = ZEROES)
J66801                 MOVE RGP-S-VALUE   TO PR14F2-RGP-DEPENDENTS
J66801               ELSE
J66801                 MOVE PREDM-DEPENDENTS TO PR14F2-RGP-DEPENDENTS
J66801               END-IF
J66801             END-IF
J66801             IF  (RGP-FLD-NBR = 6004)
J66801               IF  (PREDM-OTHER-AMOUNT = ZEROES)
J66801                 MOVE RGP-S-VALUE  TO PR14F2-RGP-OTHER-AMOUNT
J66801               ELSE
J66801                 MOVE PREDM-OTHER-AMOUNT 
J66801                                   TO PR14F2-RGP-OTHER-AMOUNT
J66801               END-IF
J66801             END-IF
J66801             IF  (RGP-FLD-NBR = 6005)
J66801               IF  (PREDM-DEDUCTIONS = ZEROES)
J66801                 MOVE RGP-S-VALUE   TO PR14F2-RGP-DEDUCTIONS
J66801               ELSE
J66801                 MOVE PREDM-DEDUCTIONS TO PR14F2-RGP-DEDUCTIONS
J66801               END-IF
J66801             END-IF
J66801             PERFORM 860-FIND-NEXT-RGPSET1
J66801         END-PERFORM
J66801     END-IF.
           
       530-END.

016700******************************************************************
016800 600-MOVE-TO-SCREEN.
016900******************************************************************
017000
017100     MOVE EDM-COMPANY            TO PR14F2-EDM-COMPANY.

023800     MOVE PRS-NAME               TO PR14F2-PRS-NAME.
023900
017200     MOVE EDM-EMPLOYEE           TO PR14F2-EDM-EMPLOYEE.
017300
017400     MOVE EMP-LAST-NAME          TO HRWS-LAST-NAME.
017500     MOVE EMP-FIRST-NAME         TO HRWS-FIRST-NAME.
017600     MOVE EMP-MIDDLE-INIT        TO HRWS-MIDDLE-INIT.
           MOVE EMP-NAME-SUFFIX        TO HRWS-NAME-SUFFIX.
           MOVE EMP-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE.
017700     PERFORM 750-HR-FORMAT-NAME.
017800     MOVE HRWS-FORMAT-NAME       TO PR14F2-EMP-NAME.
017900
018000     MOVE EDM-DED-CODE           TO PR14F2-EDM-DED-CODE.
024000     PERFORM 840-FIND-DDCSET1.
024100     MOVE DDC-DESCRIPTION        TO PR14F2-DDC-DESCRIPTION.
018100
018200     MOVE EDM-SEQ-NBR            TO PR14F2-EDM-SEQ-NBR.
030900
           MOVE EDM-MARITAL-STATUS     TO PR14F2-EDM-MARITAL-STATUS.
           MOVE EDM-EXEMPTIONS         TO PR14F2-EDM-EXEMPTIONS.

           MOVE EDM-EXEMPT-AMOUNT      TO PR14F2-EDM-EXEMPT-AMOUNT.
           MOVE EDM-TAX-EXEMPT-FLG     TO PR14F2-EDM-TAX-EXEMPT-FLG.
           MOVE EDM-PERS-EXEMPTS       TO PR14F2-EDM-PERS-EXEMPTS.
           MOVE EDM-DEPEND-EXEMPTS     TO PR14F2-EDM-DEPEND-EXEMPTS.
           MOVE EDM-ADDL-EXEMPTS       TO PR14F2-EDM-ADDL-EXEMPTS.
           MOVE EDM-ADDL-EXEMP-AMT     TO PR14F2-EDM-ADDL-EXEMP-AMT.
           MOVE EDM-ADDL-TAX-CODE      TO PR14F2-EDM-ADDL-TAX-CODE.
           MOVE EDM-ADDL-RATE          TO PR14F2-EDM-ADDL-RATE.
           MOVE EDM-ADDL-AMOUNT        TO PR14F2-EDM-ADDL-AMOUNT.
           MOVE EDM-FORMULA-NUMBER     TO PR14F2-EDM-FORMULA-NUMBER.
           MOVE EDM-DFLT-TCA-FLAG      TO PR14F2-EDM-DFLT-TCA-FLAG.

J66801     IF  (DDC-TAX-AUTH-TYPE = "FD" OR "ST")
J66801     AND (DDC-TAX-CATEGORY  = 01)
J66801     AND (DDC-COUNTRY-CODE  = "US")
J66801         MOVE EDM-COMPANY                TO DB-COMPANY
J66801         MOVE EDM-EMPLOYEE               TO DB-EMPLOYEE
J66801         MOVE "US"                       TO DB-COUNTRY-CODE
J66801         MOVE "E"                        TO DB-RECORD-TYPE
J66801         MOVE SPACES                     TO DB-REPORT-ENTITY
J66801         MOVE PR14F2-EDM-DED-CODE        TO DB-TAX-ID-CODE
J66801         MOVE ZEROS                      TO DB-PAYROLL-YEAR
J66801         MOVE "E6"                       TO DB-TOPIC
J66801         MOVE 6001                       TO DB-FLD-NBR
J66801         MOVE ZEROS                      TO DB-SEQ-NBR
J66801         PERFORM 850-FIND-NLT-RGPSET1
J66801         PERFORM
J66801           UNTIL (PRREGPARM-NOTFOUND)
J66801           OR    (RGP-COMPANY     NOT = EDM-COMPANY)
J66801           OR    (RGP-EMPLOYEE    NOT = EDM-EMPLOYEE)
J66801           OR    (RGP-TAX-ID-CODE NOT = PR14F2-EDM-DED-CODE)
J66801           OR    (RGP-TOPIC       NOT = "E6")
J66801           OR    (RGP-FLD-NBR > 6005)
J66801             IF  (RGP-FLD-NBR = 6001)
J66801                 MOVE RGP-S-VALUE    TO PR14F2-RGP-FORM-YEAR
J66801             END-IF
J66801             IF  (RGP-FLD-NBR = 6002)
J66801               IF  (PREDM-MULT-JOBS = ZEROES)
J66801                 MOVE RGP-S-VALUE    TO PR14F2-RGP-MULT-JOBS
J66801               ELSE
J66801                 MOVE PREDM-MULT-JOBS TO PR14F2-RGP-MULT-JOBS
J66801               END-IF
J66801             END-IF
J66801             IF  (RGP-FLD-NBR = 6003)
J66801               IF  (PREDM-DEPENDENTS = ZEROES)
J66801                 MOVE RGP-S-VALUE   TO PR14F2-RGP-DEPENDENTS
J66801               ELSE
J66801                 MOVE PREDM-DEPENDENTS TO PR14F2-RGP-DEPENDENTS
J66801               END-IF
J66801             END-IF
J66801             IF  (RGP-FLD-NBR = 6004)
J66801               IF  (PREDM-OTHER-AMOUNT = ZEROES)
J66801                 MOVE RGP-S-VALUE  TO PR14F2-RGP-OTHER-AMOUNT
J66801               ELSE
J66801                 MOVE PREDM-OTHER-AMOUNT 
J66801                                   TO PR14F2-RGP-OTHER-AMOUNT
J66801               END-IF
J66801             END-IF
J66801             IF  (RGP-FLD-NBR = 6005)
J66801               IF  (PREDM-DEDUCTIONS = ZEROES)
J66801                 MOVE RGP-S-VALUE   TO PR14F2-RGP-DEDUCTIONS
J66801               ELSE
J66801                 MOVE PREDM-DEDUCTIONS TO PR14F2-RGP-DEDUCTIONS
J66801               END-IF
J66801             END-IF
J66801             PERFORM 860-FIND-NEXT-RGPSET1
J66801         END-PERFORM
J66801     END-IF.

031000 600-END.
031100
031200******************************************************************
031300 PR14S2-TRANSACTION-END.
031400******************************************************************
