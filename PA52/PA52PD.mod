******* PA52PD 196 <3830413509>
      ******************************************************************
      *                            PA52PD                              *
      ******************************************************************
      ******************************************************************
      *                                                                *
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  208104 | J08104 | AUDITING OF PERSONNNEL ACTIONS              *
      *  ------   ------   ------------------------------------------  *
      *  304194 | J04194 | DECLARED PASSING OF VALUES TO FIELDS        *
      *         |        | UPDATE BENEFITS, UPDATE ABSENCE MGT AND     *
      *         |        | UPDATE REQUIRED DEDUCTIONS IN ADD FUNCTION  *
      *         |        | OF PA52.1 TO RESOLVE ADDINS VALUE DEFAULT   *
      *         |        | ISSUE WHEN FIELDS ARE FOUND BLANK.          *
      *  ------   ------   ------------------------------------------  *
      *  313588 | J13588 | Remove LP trigger checks                    *
      *  ------   ------   ------------------------------------------  *
      *  359149 | J59149 | MOVED VALUE FROM APL-PROCESS-LEVEL TO HREMP *
      *         |        | -PROCESS-LEVEL TO BE USED FOR FINDING POS   *
      *         |        | RULE EVEN IF PROCESS LEVEL IS NOT ENTERED   *
      *         |        | ON PA52.4                                   *
      *  ------   ------   ------------------------------------------  *
      *  359105 | J59105 | Use msgmnt for button descriptions.         *
      *  ------   ------   ------------------------------------------  *
      *  441791 | J41791 | Edit gives incorrect message.               *
      *  ------   ------   ------------------------------------------  *
      *  409245 | J09245 | LTM values from LT11 edit corrected.        *
      *  ------   ------   ------------------------------------------  *
      *  379563 | J79563 | HANDLED PA52F5-PCT-NEW-BASE-RATE-A COMPARE  *
      *         |        | DURING REVERSE.                             *
      *  ------   ------   ------------------------------------------  *
      *  428956 | J28956 | LTM *BLANK values Fill Defaults logic.      *
      *  ------   ------   ------------------------------------------  *
      *  383777 | J83777 | UPDATE APPROVAL STATUS FLAG BEFORE DOING    *
      *         |        | THE VALIDATION PROCESS FOR ACTION.          *
      *  ------   ------   ------------------------------------------  *
      *  571111 | J71111 | DO NOT UPDATE PA52F1-PCT-APPROVAL-FLAG      *
      *         |        | FROM PERSACTION TABLE WHEN VALUE IS "L".    *
      *         |        | - SAME WITH PA52F4 AND PA52F5               *
      *  ------   ------   ------------------------------------------  *
      *  545417 | J45417 | MOVED POSITION FOR PASSING EDIT MESSAGES    *
      *         |        | ADDING, CHANGING AND DELETING RECORDS       *
      *  ------   ------   ------------------------------------------  *
      *  646761 | J46761 | USED 4 DECIMAL PLACES FOR ANNUAL HOURS TO   *
      *         |        | FOLLOW THE CHANGES MADE IN HR11.            *
      *  ------   ------   ------------------------------------------  *
      *  531358 | J31358 | Added error message for scenario when user  *
      *         |        | attempts to remove END-DATE in PA52.5.      *
      *  ------   ------   ------------------------------------------- *
      *  718332 | J18332 | Populate audit trails/fields for PARTICIPNT *
      *  ------   ------   ------------------------------------------- *
      *  783708 | J83708 | Changes made to workflow trigger to create  *
      *         |        | Bod when immediate action = "Y"             *
      *  ------   ------   ------------------------------------------  *
      *  809538 | J09538 | Added initialize of entered screen values   *
      *         |        | after a successful ADD that is Immediate.   *
      * -------   ------   ------------------------------------------  *
      * 1385747 | J85747 | PERSONNEL ACTION UPDATES FOR 2020 W-4       *
      * -------   ------   ------------------------------------------  *
      * 1102321 | 102321 | Fixed inconsistent display of message for   *
      *         |        | multiple action entries.                    *
      * -------   ------   ------------------------------------------  *
      * 1141726 | 141726 | MOVED VALUES TO PCT-BASE-PAY-RATE AND PCT-  *
      *         |        | BASE-CURRENCY FOR PR36 DEFAULTING           *
      * -------   ------   ------------------------------------------  *
      * 1197195 | 197195 | Fixed rounding issue on PA52.2              *
      *         |        | Fixed round higher logic issue.             *
      *  ------   ------   ------------------------------------------  *
      * 1198255 | 198255 | Restricted "Position Level" field to allow  *
      *         |        | 01 - 98 values only.                        *
      *         |        | Corrected validation for PA52:169 error msg *
      *  ------   ------   ------------------------------------------  *
      * 1199885 | 199885 | CHANGE CALLED-FROM TO PA52.4                *
      * -------   ------   ------------------------------------------  *
      * 1312387 | 312387 | Logic that throws message PA52: 169 should  *
      *         |        | not be applied when MOVE LEVEL was done.    *
      * -------   ------   ------------------------------------------  *
      * 1313903 | 313903 | ADDED LOGIC THAT CHECKS IF THE BASE PAY RATE*
      *         |        | FIELD IS REQUIRED BEFORE PASSING THE VALUE  *
      *         |        | TO THE SCREEN. THIS APPLIES TO ADD FUNCTION.*
      * -------   ------   ------------------------------------------  *
      * 1736685 | 736685 | Add Gender Value of X                       *
      * -------   ------   ------------------------------------------  *
      ******************************************************************
AA0626*   N/A   | AA0626 | ADDED VARIABLE FOR STORING TERM-WORKUNIT    *
AA0626*         |        | TO FIX ISSUE WHERE TERM WORKUNIT IS NOT     *
AA0626*         |        | RELEASED WHEN TERM-DATE IS SUPPLIED         *
CRP001*  ------   ------   ------------------------------------------  *
CRP001* CPSMOD  | CRP001 | Added logic to create IPA work unit to      *
CRP001*         |        | recalc benefit information                  *
US3233*--------- -------- -------------------------------------------  *
US3233*   MLF   | US3233 | Added 1 line to initialize HRCRP-WS-DATE    *
US3233*         |        | Not initializing appears to corrupt data    *
US3233*         |        | submitted to subsequent workunits           *
      ******************************************************************
      *               M O D I F I C A T I O N   L O G:                 *
      ******************************************************************
      *  Modified by MARK GLISSEN - MG0301                             *
      ******************************************************************
      *  03/01/05  - MODIFIED PA52 TO PASS ADDITIONAL FIELDS TO PFLOW  *
      *  04/22/09  - ACS - M. HUNTER - REAPPLIED CUSTOM CODE AFTER CTP *
      *              58237 WAS APPLIED                                 *
      *  08/09/10  - ACS002 - ACS - M. HUNTER - ADDED CODE TO NOT      *
      *                                         PRODUCE A WORK UNIT IF *
      *                                         ACTION IS NOT IMMEDIATE*
      * 10/21/10   - ACS - M. HUNTER - REAPPLIED ALL ABOVE CUSTOM CODE *
      *                                AFTER 9.0 APP UPGRADE.          *
      * 08/19/11   - ACS - M. HUNTER - REAPPLIED ALL ABOVE CUSTOM CODE *
      *                                AFTER 9.0.1 APP UPGRADE.        *
      *                    NOTE:       MSGMNT MSG'S 900, 901 AND 902   *
      *                                MUST ALSO BE READDED.           *
      
      ******************************************************************
       PA52S1-TRANSACTION SECTION 30.
      ******************************************************************
       PA52S1-START.

CUSTOM     PERFORM 1000-OPEN-WORKFLOW-DB.
      
           MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY.
           MOVE PA52F1-PCT-ACTION-CODE TO DB-ACTION-CODE.
           PERFORM 840-FIND-PATSET1.
J31809     IF  ((PERSACTYPE-FOUND) AND (PAT-WORKFLOW-FLAG = "Y"))
J83708     OR  (PA52F1-IMMEDIATE-ACTION = "Y")
J31809         PERFORM 1000-OPEN-WORKFLOW-DB
J33429         IF  (PA52F1-PCT-ACTION-NBR-K NOT = ZEROES)
J33429             MOVE PA52F1-PCT-ACTION-NBR-K
J33429                                 TO PA52F1-PCT-ACTION-NBR
J33429         END-IF
J33429     END-IF.
J33429     MOVE ZEROES                 TO PA52F1-PCT-ACTION-NBR-K.
                       
J31809*    IF (PA52F1-FC = "A" OR "C")
J31809*        PERFORM 1000-OPEN-WORKFLOW-DB.

           MOVE "PA52.1"               TO PA52F1-CALLED-FROM.

           IF  (PA52F1-FC NOT = "A" AND "C")
               INITIALIZE PA52F1-XMIT-DEDDAT
                          PA52F1-XMIT-REQDED
                          PA52F1-XMIT-HREMP-BLOCK
                          PA52F1-XMIT-ACT-EXISTS
           END-IF.
           MOVE "PA52"                 TO PA52F1-PGM-NAME.
           IF  (PA52F1-FC NOT = "A")
           OR  (PA52F1-IMMEDIATE-ACTION NOT = "Y")
               INITIALIZE PA52F1-XMIT-IMMED.

P61169     INITIALIZE                     HREMPWS-GROUP-A.
P61169
J28456     IF (PA52F1-FC = "F" OR "L")
               PERFORM 650-DEFAULT
               THRU    650-END
               GO TO PA52S1-TRANSACTION-END.

           MOVE 411                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO PA52WS-SEC-MESSAGE.
      
           PERFORM 200-EDIT-TRAN
           THRU    200-END.

J41791     MOVE CRT-ERROR-CAT         TO PA52WS-ERROR-CAT-SAVE.
J40356     MOVE SPACES                TO PA52F1-LTM-EXISTS 
J40356                                   PA52F1-LTM-FLAG.
J40356     MOVE PA52F1-PCT-COMPANY    TO DB-COMPANY.
J40356     MOVE PA52F1-PCT-EMPLOYEE   TO DB-EMPLOYEE.
J40356     MOVE 1                     TO DB-STATUS.
J40356     MOVE LEMSET2-STATUS        TO WS-DB-BEG-RNG.
J40356     PERFORM 850-FIND-BEGRNG-LEMSET2.
J40356     IF (HREMPRCVR-FOUND)      
J59105         MOVE 547               TO CRT-MSG-NBR
J59105         PERFORM 790-GET-MSG
J50105         MOVE CRT-MESSAGE       TO PA52F1-LTM-EXISTS
J40356         MOVE "*"               TO PA52F1-LTM-FLAG
J40356     END-IF.
J41791     MOVE PA52WS-ERROR-CAT-SAVE TO CRT-ERROR-CAT.
         
           MOVE PA52F1-PCT-EFFECT-DATE TO PA52F1-CUC-EFFECT-DATE.

           IF  (NO-ERROR-FOUND)
               PERFORM 400-PROCESS-TRAN
               THRU    400-END
               INITIALIZE PA52F1-XMIT-DEDDAT
                          PA52F1-XMIT-REQDED
                          PA52F1-XMIT-HREMP-BLOCK
                          PA52F1-XMIT-IMMED
                          PA52F1-XMIT-ACT-EXISTS
           END-IF.
J09538
J09538     IF  (PA52F1-FC = "A")
J09538     AND (PA52F1-IMMEDIATE-ACTION = "Y")
J09538     AND (NO-ERROR-FOUND)
J09538         PERFORM
J09538             VARYING I1 FROM 1 BY 1
J09538             UNTIL  (I1 > 12)
J09538             INITIALIZE PA52F1-PCT-NEW-VALUE-1 (I1)
J09538                        PA52F1-PCT-NEW-VALUE-2 (I1)
J09538                        PA52F1-PCT-NEW-VALUE-3 (I1)
J09538         END-PERFORM
J09538     END-IF.
J38036 
J38036     IF  (PA52F1-PCT-APPROVAL-FLAG = "L")
J38036     AND (CRT-ERROR-NBR = 145)
J38036         INITIALIZE CRT-ERROR-NBR
J38036                    CRT-FIELD-NBR                                
J38036         MOVE 412                TO CRT-MSG-NBR
J38036         PERFORM 790-GET-MSG
J38036     END-IF.

           GO TO PA52S1-TRANSACTION-END.   
      
      ******************************************************************
       200-EDIT-TRAN.
      ******************************************************************
        
           MOVE "PA52"                   TO HRWS-CALLED-BY.

           IF  (PA52F1-PCT-PROCESS-TYPE    = "5")
           AND (PA52F1-PCT-MOVE-FROM-LEVEL = ZEROES)
               MOVE 151                    TO CRT-ERROR-NBR
               MOVE PA52F1-PCT-MOVE-FROM-LEVEL-FN
                                           TO CRT-FIELD-NBR
               GO TO 200-END.

           MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY.
           MOVE SPACES                 TO DB-PROCESS-LEVEL.
           PERFORM 840-FIND-PRSSET1.
           IF (PRSYSTEM-NOTFOUND)
               MOVE 100                                TO CRT-ERROR-NBR
               MOVE PA52F1-PCT-COMPANY-FN              TO CRT-FIELD-NBR
               GO TO 200-END
           ELSE
               MOVE PRS-NAME               TO PA52WS-PRS-NAME.
       
           IF  (PA52F1-FC = "V")
           AND (PA52F1-PCT-MOVE-FROM-LEVEL NOT = ZEROES)
               PERFORM 230-EDIT-SPECIAL-PROCESSING
               THRU    230-END.
                   
           IF (ERROR-FOUND)
               GO TO 200-END.

           PERFORM 210-EDIT-ACCESS
           THRU    210-END.
      
           IF (ERROR-FOUND)
               GO TO 200-END.

           IF   (PA52F1-FC  NOT = "D" AND "I" AND "N" AND "P" AND "V")
           OR  ((PA52F1-FC  = "V")
           AND  (PA52F1-PCT-MOVE-FROM-LEVEL = ZEROES))
               IF (PA52F1-FC = "R")
               OR (PA52F1-PCT-PROCESS-TYPE    NOT = SPACES)
               OR (PA52F1-PCT-NEW-EFFECT-DATE NOT = ZEROES)
               OR (PA52F1-PCT-HIST-CORR-FLAG  NOT = SPACES)
               OR (PA52F1-PCT-MERGE-ACTN-NBR  NOT = ZEROES)
                   PERFORM 230-EDIT-SPECIAL-PROCESSING
                   THRU    230-END.

           IF (ERROR-FOUND)
               GO TO 200-END.

           IF (PA52F1-FC = "A" OR "C")
               PERFORM 220-EDIT-DATA
               THRU    220-END
               GO TO 200-END.
      
           IF (PA52F1-FC = "V")
               PERFORM 700-MOVE-LEVEL
               THRU    700-END.

       200-END.
      
      ******************************************************************
       210-EDIT-ACCESS.
      ******************************************************************
      
           INITIALIZE                     HREMP-UPDPEP-DATE.

           INITIALIZE                     DB-COMPANY
004600                                    DB-PROCESS-LEVEL
004700                                    DB-DEPARTMENT
004800                                    DB-EFFECT-DATE.
004900     PERFORM 850-FIND-NLT-PPRSET1.
005000     IF (PAPOSRULE-FOUND)
005100         MOVE WS-TRUE            TO HREMP-PAUSER-FLAG-SW.
005200
005300     MOVE "E"                    TO PA52F1-PT-ACTION-TYPE.
005400
005800     IF   (PA52F1-FC             = "A")
P49898     AND ((PA52F1-PCT-COMPANY     NOT = PA52F1-ORIG-COMPANY)
P49898     OR  (PA52F1-PCT-ACTION-CODE  NOT = PA52F1-ORIG-ACTION-CODE)
P49898     OR  (PA52F1-PCT-EMPLOYEE     NOT = PA52F1-ORIG-EMPLOYEE)
P49898     OR  (PA52F1-PCT-EFFECT-DATE  NOT = PA52F1-ORIG-EFFECT-DATE))
006100         MOVE 10                                 TO CRT-ERROR-NBR
006200         MOVE PA52F1-FC-FN                       TO CRT-FIELD-NBR
006300         GO TO 210-END.
006400
005800     IF (PA52F1-FC             = "A" OR "C")
              IF (PA52F1-PCT-ACTION-CODE  NOT = PA52F1-ORIG-ACTION-CODE)
              OR (PA52F1-PCT-EMPLOYEE     NOT = PA52F1-ORIG-EMPLOYEE)
              OR ((PA52F1-PCT-EFFECT-DATE NOT = PA52F1-ORIG-EFFECT-DATE)
              AND (PA52F1-ORIG-EFFECT-DATE NOT = ZEROES))
006100           MOVE 132                              TO CRT-ERROR-NBR
006200           MOVE PA52F1-FC-FN                     TO CRT-FIELD-NBR
006300           GO TO 210-END.
                  
008100     IF (PA52F1-FC = "A" OR "C" OR "I" OR "R")
008200         MOVE PA52F1-PCT-COMPANY             TO DB-COMPANY
008300         MOVE PA52F1-PCT-ACTION-CODE         TO DB-ACTION-CODE
008400         PERFORM 840-FIND-PATSET1
008500         IF (PERSACTYPE-NOTFOUND)
008600             MOVE 110                        TO CRT-ERROR-NBR
008700             MOVE PA52F1-PCT-ACTION-CODE-FN  TO CRT-FIELD-NBR
008800             GO TO 210-END
               ELSE
               IF (PAT-ACTIVE-FLAG = "2")
                   MOVE 222                       TO CRT-ERROR-NBR
                   MOVE PAT-ACTION-CODE           TO CRT-ERR-VAR1
                   MOVE PA52F1-PCT-ACTION-CODE-FN TO CRT-FIELD-NBR
                   GO TO 210-END
               END-IF.
008900
              
009000     IF (PA52F1-FC = "A" OR "C" OR "I" OR "R")
009100         MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY
009200         MOVE PA52F1-PCT-EMPLOYEE    TO DB-EMPLOYEE
009300         PERFORM 840-FIND-PEMSET1
009400         PERFORM 840-FIND-EMPSET1
009500         IF (PAEMPLOYEE-NOTFOUND)
009600         OR (EMPLOYEE-NOTFOUND)
009700             MOVE 130                            TO CRT-ERROR-NBR
009800             MOVE PA52F1-PCT-EMPLOYEE-FN         TO CRT-FIELD-NBR
009900             GO TO 210-END
010000         ELSE
010100             MOVE EMP-COMPANY        TO CRT-COMPANY
010200             MOVE EMP-PROCESS-LEVEL  TO CRT-PROCESS-LEVEL
010300             PERFORM 700-HR-EMP-SECURITY
010400             IF (HRWS-EMP-SECURED)
010500                 MOVE 244                        TO CRT-ERROR-NBR
010600                 MOVE PA52F1-PCT-EMPLOYEE-FN     TO CRT-FIELD-NBR
010700                 GO TO 210-END.
010800
010900     MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY.
011000     MOVE "E"                    TO DB-ACTION-TYPE.
011100     MOVE PA52F1-PCT-EMPLOYEE    TO DB-EMPLOYEE.
011200     MOVE PA52F1-PCT-ACTION-CODE TO DB-ACTION-CODE.
011300     MOVE PA52F1-PCT-EFFECT-DATE TO DB-EFFECT-DATE.
011400     MOVE PA52F1-PCT-ACTION-NBR  TO DB-ACTION-NBR.
011500
011600     IF (PA52F1-FC       = "N")
011700         IF (DB-EFFECT-DATE = ZEROES)
011800             MOVE WS-HIGH-VALUES TO DB-ACTION-NBR
011900             PERFORM 850-FIND-NLT-PCTSET2
012000         ELSE
012100             PERFORM 850-FIND-NLT-PCTSET2
                   IF  (PCT-COMPANY     = DB-COMPANY)
                   AND (PCT-ACTION-TYPE = DB-ACTION-TYPE)
                   AND (PCT-EMPLOYEE    = DB-EMPLOYEE)
                   AND (PCT-ACTION-CODE = DB-ACTION-CODE)
                   AND (PCT-EFFECT-DATE = DB-EFFECT-DATE)
                   AND (PCT-ACTION-NBR  = DB-ACTION-NBR)
                       PERFORM 860-FIND-NEXT-PCTSET2
                   END-IF
012200         END-IF
012300     ELSE
012400     IF (PA52F1-FC = "P")
012500         PERFORM 850-FIND-NLT-PCTSET2
012600         PERFORM 870-FIND-PREV-PCTSET2.
012700
015100     IF  (PA52F1-FC       = "N" OR "P")
P60236      IF  (PERSACTION-FOUND)
P60236      AND (PCT-COMPANY     = DB-COMPANY)
P60236      AND (PCT-ACTION-TYPE = DB-ACTION-TYPE)
               IF (PCT-EMPLOYEE      NOT = EMP-EMPLOYEE)
               OR (CRT-PROCESS-LEVEL NOT = EMP-PROCESS-LEVEL)
                   MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE
015500             PERFORM 850-FIND-NLT-EMPSET1
016200             MOVE EMP-COMPANY            TO CRT-COMPANY
016300             MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL
016400             PERFORM 700-HR-EMP-SECURITY
016500             IF (HRWS-EMP-SECURED)
                       PERFORM 240-GET-NEXT-EMPLOYEE
                       THRU    240-END
                       UNTIL (EMPLOYEE-NOTFOUND)
P51669                 OR    (PCT-COMPANY     NOT = DB-COMPANY)
                       OR    (HRWS-EMP-NOT-SECURED)
                   END-IF
P60236         END-IF
P60236      END-IF
P60236     END-IF.
      
012800     IF (PA52F1-FC = "N" OR "P")
012900         IF (PERSACTION-NOTFOUND)
013000         OR (PCT-COMPANY     NOT = DB-COMPANY)
013100         OR (PCT-ACTION-TYPE NOT = DB-ACTION-TYPE)
013200             MOVE 12                     TO CRT-ERROR-NBR
013300             MOVE PA52F1-FC-FN           TO CRT-FIELD-NBR
013400             GO TO 210-END
013500         ELSE
                   PERFORM 840-FIND-PEMSET1
013600             MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE
013700                                            PA52F1-PCT-EMPLOYEE
013800             MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE
013900                                            PA52F1-PCT-ACTION-CODE
014000             MOVE PCT-EFFECT-DATE        TO DB-EFFECT-DATE
014100                                            PA52F1-PCT-EFFECT-DATE
014200             MOVE PCT-ACTION-NBR         TO DB-ACTION-NBR
014300                                            PA52F1-PCT-ACTION-NBR
014400             PERFORM 840-FIND-PATSET1
014500             IF (PERSACTYPE-NOTFOUND)
014600                 MOVE 110                        TO CRT-ERROR-NBR
014700                 MOVE PA52F1-PCT-ACTION-CODE-FN  TO CRT-FIELD-NBR
014900                 GO TO 210-END
                   END-IF.
015000
015100     IF (PA52F1-FC = "N" OR "P")
015200         MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY
015300         MOVE PA52F1-PCT-EMPLOYEE    TO DB-EMPLOYEE
015400         PERFORM 840-FIND-PEMSET1
015500         PERFORM 840-FIND-EMPSET1
015600         IF (PAEMPLOYEE-NOTFOUND)
015700         OR (EMPLOYEE-NOTFOUND)
015800             MOVE 130                            TO CRT-ERROR-NBR
015900             MOVE PA52F1-PCT-EMPLOYEE-FN         TO CRT-FIELD-NBR
016000             GO TO 210-END
016100         ELSE
016200             MOVE EMP-COMPANY        TO CRT-COMPANY
016300             MOVE EMP-PROCESS-LEVEL  TO CRT-PROCESS-LEVEL
016400             PERFORM 700-HR-EMP-SECURITY
016500             IF (HRWS-EMP-SECURED)
016600                 MOVE 244                        TO CRT-ERROR-NBR
016700                 MOVE PA52F1-PCT-EMPLOYEE-FN     TO CRT-FIELD-NBR
016800                 GO TO 210-END.
016900
           IF (PA52F1-FC = "I" OR "N" OR "P")
              MOVE PA52F1-PCT-EMPLOYEE    TO PA52F1-ORIG-EMPLOYEE
              MOVE PA52F1-PCT-ACTION-CODE TO PA52F1-ORIG-ACTION-CODE
              MOVE PA52F1-PCT-EFFECT-DATE TO PA52F1-ORIG-EFFECT-DATE.

017000     IF (PA52F1-FC = "N" OR "P")
               PERFORM 8400-AFTER-FIND-PCT
017100         GO TO 210-END.
017200
           PERFORM 840-FIND-PCTSET2.

           IF  (PA52F1-FC = "I")
           AND (PERSACTION-NOTFOUND)
               MOVE WS-HIGH-VALUES      TO DB-ACTION-NBR
               PERFORM 850-FIND-NLT-PCTSET2.

           IF (PA52F1-FC = "I")
               IF (PERSACTION-NOTFOUND)
               OR (PCT-COMPANY     NOT = DB-COMPANY)
               OR (PCT-ACTION-TYPE NOT = "E")
               OR (PCT-EMPLOYEE    NOT = DB-EMPLOYEE)
               OR (PCT-ACTION-CODE NOT = DB-ACTION-CODE)
               OR (PCT-EFFECT-DATE NOT = DB-EFFECT-DATE)
                   PERFORM
                       VARYING I1 FROM 1 BY 1
                       UNTIL  (I1 > 36)
                           MOVE SPACES TO PCT-NEW-VALUE (I1)
                   END-PERFORM
               END-IF.

017900     IF  (PA52F1-FC = "C" OR "D")
018000     AND (PERSACTION-NOTFOUND)
018500         MOVE 125                                TO CRT-ERROR-NBR
018600         MOVE PA52F1-PCT-ACTION-CODE-FN          TO CRT-FIELD-NBR
018700         GO TO 210-END.
018800
018900     IF  (PA52F1-FC               = "A")
019000     AND (PA52F1-PCT-EFFECT-DATE  = ZEROES)
019100     AND (PA52F1-IMMEDIATE-ACTION = "N")
019200         MOVE 102                                TO CRT-ERROR-NBR
019300         MOVE PA52F1-IMMEDIATE-ACTION-FN         TO CRT-FIELD-NBR
019400         GO TO 210-END.
019500
019600     IF  (PA52F1-FC              = "A")
019700     AND (PA52F1-PCT-EFFECT-DATE = ZEROES)
019800         MOVE WS-SYSTEM-DATE-YMD TO PA52F1-PCT-EFFECT-DATE
P68470                                    PA52F1-ORIG-EFFECT-DATE
019900         IF (PA52F1-IMMEDIATE-ACTION = SPACES)
020000             MOVE "Y"            TO PA52F1-IMMEDIATE-ACTION.
020100
020200     IF  (PA52F1-FC               = "A")
020300     AND (PA52F1-IMMEDIATE-ACTION = SPACES)
020400         MOVE "N"                TO PA52F1-IMMEDIATE-ACTION.
020500
020600     IF  (PA52F1-FC               = "A")
020700     AND (PA52F1-PCT-EFFECT-DATE  > WS-SYSTEM-DATE-YMD)
020800     AND (PA52F1-IMMEDIATE-ACTION = "Y")
020900         MOVE 101                                TO CRT-ERROR-NBR
021000         MOVE PA52F1-IMMEDIATE-ACTION-FN         TO CRT-FIELD-NBR
021100         GO TO 210-END.
021200
           PERFORM 8400-AFTER-FIND-PCT.

       210-END.   
                     
      ******************************************************************
       220-EDIT-DATA.
      ******************************************************************
      
           MOVE "PA52"                 TO CRT-ERROR-CAT.

           IF  (PA52F1-FC              = "A")
           AND (PA52F1-XMIT-ACT-EXISTS  = ZEROS)
               MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY
               MOVE PA52F1-PCT-ACTION-CODE TO DB-ACTION-CODE
               MOVE "E"                    TO DB-ACTION-TYPE
               MOVE PA52F1-PCT-EFFECT-DATE TO DB-EFFECT-DATE
               MOVE PA52F1-PCT-EMPLOYEE    TO DB-EMPLOYEE
               MOVE PCTSET3-EMPLOYEE       TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PCTSET3
               IF (PERSACTION-FOUND)
      ******** Warning, ADDING DUPLICATE RECORD.
               MOVE 1                      TO PA52F1-XMIT-ACT-EXISTS
               MOVE 261                    TO CRT-ERROR-NBR
               GO TO 220-END.

           IF  (PA52F1-FC               = "C")
           AND (PA52F1-IMMEDIATE-ACTION = "Y")
               MOVE 108                                TO CRT-ERROR-NBR
               MOVE PA52F1-IMMEDIATE-ACTION-FN         TO CRT-FIELD-NBR
               GO TO 220-END.
      
J71111     IF (PA52F1-PCT-APPROVAL-FLAG NOT = "L")
J83777         MOVE PCT-APPROVAL-FLAG      TO PA52F1-PCT-APPROVAL-FLAG
J71111     END-IF.
J83777
           IF  (PA52F1-FC                = "C")
           AND (PAT-WORKFLOW-FLAG        = "Y")
           AND (PA52F1-PCT-APPROVAL-FLAG = "Y")
      ******** Action has been approved thru workflow; Cannot change
022100         MOVE 191                                TO CRT-ERROR-NBR
022200         MOVE PA52F1-FC-FN                       TO CRT-FIELD-NBR
022300         GO TO 220-END.
022400
022500     IF  (PA52F1-PCT-ANT-END-DATE NOT = ZEROS)
022600     AND (PA52F1-PCT-ANT-END-DATE NOT > PA52F1-PCT-EFFECT-DATE)
022700         MOVE 233                                TO CRT-ERROR-NBR
022800         MOVE PA52F1-PCT-ANT-END-DATE-FN         TO CRT-FIELD-NBR
022900         GO TO 220-END.
023000
J13588*    MOVE PA52F1-PCT-COMPANY     TO EDCDWS-COMPANY.
J13588*    MOVE "LP"                   TO EDCDWS-SYSTEM.
J13588*    PERFORM 6000-IS-SYSTEM-TRIGGER-ENABLED.
J13588*    IF (NOT EDCDWS-TRIGGER-ENABLED)
J13588*        IF (PA52F1-PCT-UPD-ABS-MGMT = "Y")
J13588*            MOVE 234            TO CRT-ERROR-NBR
J13588*            MOVE PA52F1-PCT-UPD-ABS-MGMT-FN
J13588*                                TO CRT-FIELD-NBR
J13588*            GO TO 220-END
J13588*        END-IF
J13588*        IF (PA52F1-PCT-UPD-ABS-MGMT = SPACES)
J13588*            MOVE "N"            TO PA52F1-PCT-UPD-ABS-MGMT
J13588*        END-IF.

023100     IF (PA52F1-PCT-PARTICIPNT NOT = ZEROS)
023200     OR (PA52F1-PCT-OCCUR-TYPE NOT = SPACES)
               PERFORM 840-FIND-BNCSET1
               IF  (BNCOMPANY-NOTFOUND)
023300             MOVE 253                            TO CRT-ERROR-NBR
023400             IF (PA52F1-PCT-PARTICIPNT NOT = ZEROS)
023500                 MOVE PA52F1-PCT-PARTICIPNT-FN   TO CRT-FIELD-NBR
023600                 GO TO 220-END
023700             ELSE
023800                 MOVE PA52F1-PCT-OCCUR-TYPE-FN   TO CRT-FIELD-NBR
023900                 GO TO 220-END
024000         ELSE
024100             MOVE BNC-LAST-PART  TO PA52WS-LAST-PART.
024200
024300     MOVE ZEROS                  TO PA52WS-PARTICIPNT.
024400     IF  (PA52F1-PCT-OCCUR-TYPE NOT = SPACES)
024500     AND (BNC-AUTO-PART         = "Y")
024600     AND (PA52F1-FC             = "A")
024700         MOVE PA52WS-LAST-PART   TO DB-PARTICIPNT
024800                                    PA52WS-PARTICIPNT
024900         PERFORM 850-KFIND-NLT-PARSET1
025000         MOVE WS-FALSE           TO PA52WS-NUMBER-SW
025100         PERFORM 221-GET-PARTICIPNT-NUMBER
025200         THRU    221-END
025300             UNTIL (PA52WS-NUMBER-FOUND).
025400
025500     IF  (BNC-AUTO-PART         = "Y")
025600     AND (PA52F1-PCT-PARTICIPNT NOT = ZEROS)
025700         MOVE 254                                TO CRT-ERROR-NBR
025800         MOVE PA52F1-PCT-PARTICIPNT-FN           TO CRT-FIELD-NBR
025900         GO TO 220-END.
026000
026100     IF  (BNC-AUTO-PART         = "N")
026200     AND (PA52F1-PCT-OCCUR-TYPE NOT = SPACES)
026300     AND (PA52F1-PCT-PARTICIPNT = ZEROS)
026400         MOVE 255                                TO CRT-ERROR-NBR
026500         MOVE PA52F1-PCT-PARTICIPNT-FN           TO CRT-FIELD-NBR
026600         GO TO 220-END.
026700
026800     IF  (BNC-AUTO-PART         = "N")
026900     AND (PA52F1-PCT-PARTICIPNT NOT = ZEROS)
027000         MOVE PA52F1-PCT-PARTICIPNT TO DB-PARTICIPNT
027100         PERFORM 840-FIND-PARSET1
027200         IF (PARTICIPNT-FOUND)
027300             MOVE 258                            TO CRT-ERROR-NBR
027400             MOVE PA52F1-PCT-PARTICIPNT-FN       TO CRT-FIELD-NBR
027500             GO TO 220-END.
027600
027700     IF  (PA52F1-PCT-PARTICIPNT NOT = ZEROS)
027800     AND (PA52F1-PCT-OCCUR-TYPE = SPACES)
027900         MOVE 256                                TO CRT-ERROR-NBR
028000         MOVE PA52F1-PCT-OCCUR-TYPE-FN           TO CRT-FIELD-NBR
028100         GO TO 220-END.
028200
028300     IF (PA52F1-PCT-OCCUR-TYPE NOT = SPACES)
028400         MOVE PA52F1-PCT-OCCUR-TYPE  TO DB-OCCUR-TYPE
028500         PERFORM 840-FIND-OCCSET1
028600         IF (OCCURTYPE-NOTFOUND)
028700             MOVE 257                            TO CRT-ERROR-NBR
028800             MOVE PA52F1-PCT-OCCUR-TYPE-FN       TO CRT-FIELD-NBR
028900             GO TO 220-END
029000         ELSE
029100             MOVE OCC-MONTHS-EXT     TO PA52WS-OCC-MONTHS-EXT.
029200
110700     PERFORM 840-FIND-BNCSET1.
           IF  (BNCOMPANY-NOTFOUND)
           AND (PA52F1-PCT-UPDATE-BENEFIT  = "Y")
      ******** Cannot update benefit; Company is not setup in benefit system
028700         MOVE 180                            TO CRT-ERROR-NBR
028800         MOVE PA52F1-PCT-UPDATE-BENEFIT-FN   TO CRT-FIELD-NBR
028900         GO TO 220-END.

           IF (PA52F1-PCT-UPDATE-BENEFIT   NOT = "Y")
               IF (PA52F1-PCT-OCCUR-TYPE   NOT = SPACES)
      ************ Cannot enter occurence type; Update benefits is "N"
028700             MOVE 181                        TO CRT-ERROR-NBR
028800             MOVE PA52F1-PCT-OCCUR-TYPE-FN   TO CRT-FIELD-NBR
028900             GO TO 220-END
               END-IF

               IF (PA52F1-PCT-PARTICIPNT   NOT = ZEROES)
      ************ Cannot enter participant; Update benefits is "N"
028700             MOVE 182                        TO CRT-ERROR-NBR
028800             MOVE PA52F1-PCT-PARTICIPNT-FN   TO CRT-FIELD-NBR
028900             GO TO 220-END.

           IF (PA52F1-PCT-UPDATE-REQ-DED       = "Y")
               IF (PA52F1-PCT-EDM-EFFECT-DT    = ZEROES)
                   MOVE PA52F1-PCT-EFFECT-DATE TO
                                               PA52F1-PCT-EDM-EFFECT-DT
               END-IF
               IF (PA52F1-PCT-EDM-END-DATE     = ZEROES)
                   MOVE PA52F1-PCT-EDM-EFFECT-DT
                                               TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   SUBTRACT 1                  FROM WSDR-JULIAN-DAYS
                   PERFORM 900-JULIAN-TO-DATE
                   MOVE WSDR-FR-DATE           TO
                                               PA52F1-PCT-EDM-END-DATE.

           IF  ((PA52F1-FC                 = "A")
           OR  ((PA52F1-FC                 = "C")
           AND  ((PA52F1-PCT-EDM-EFFECT-DT NOT = PCT-EDM-EFFECT-DT)
           OR    (PA52F1-PCT-EDM-END-DATE  NOT = PCT-EDM-END-DATE)
           OR    (PA52F1-PCT-UPDATE-REQ-DED NOT = PCT-UPDATE-REQ-DED))))
           AND (PA52F1-PCT-EDM-EFFECT-DT   = PA52F1-PCT-EDM-END-DATE)
038100     AND (PA52F1-XMIT-DEDDAT        = ZEROS)
053263     AND (PA52F1-PCT-UPDATE-REQ-DED  = "Y")
      ******** Warning, Deductions dates equal, Press OK to continue
038200         MOVE 1                      TO PA52F1-XMIT-DEDDAT
038300         MOVE 183                    TO CRT-ERROR-NBR
038400         GO TO 220-END.

           IF  ((PA52F1-FC                 = "A")
           OR  ((PA52F1-FC                 = "C")
           AND  ((PA52F1-PCT-EDM-EFFECT-DT NOT = PCT-EDM-EFFECT-DT)
           OR    (PA52F1-PCT-EDM-END-DATE  NOT = PCT-EDM-END-DATE)
           OR    (PA52F1-PCT-UPDATE-REQ-DED NOT = PCT-UPDATE-REQ-DED))))
           AND (PA52F1-PCT-EDM-EFFECT-DT   < PA52F1-PCT-EDM-END-DATE)
038100     AND (PA52F1-XMIT-DEDDAT        = ZEROS)
053263     AND (PA52F1-PCT-UPDATE-REQ-DED  = "Y")
      ******** Warning, Deduction dates overlap, Press OK to continue
038200         MOVE 1                      TO PA52F1-XMIT-DEDDAT
038300         MOVE 184                    TO CRT-ERROR-NBR
038400         GO TO 220-END.

           IF  (PA52F1-IMMEDIATE-ACTION    = "Y")
           AND (PAT-WORKFLOW-FLAG          = "Y")
      ******** Immediate action not allowed; Workflow approval required
028700         MOVE 174                            TO CRT-ERROR-NBR
028800         MOVE PA52F1-IMMEDIATE-ACTION-FN     TO CRT-FIELD-NBR
038400         GO TO 220-END.

           IF  (PA52F1-FC NOT = "V")
           AND (PA52F1-PCT-MOVE-FROM-LEVEL NOT = ZEROES)
           AND (PA52WS-NO-MOVELEVELDONE)
               MOVE 158                        TO CRT-ERROR-NBR
               MOVE PA52F1-FC-FN               TO CRT-FIELD-NBR
               GO TO 220-END.

           IF (PAT-REQUIRE-REASON = 2)
               IF (PA52F1-PCT-REASON (1) = SPACES)
               OR (PA52F1-PCT-REASON (2) = SPACES)
                   MOVE 544                        TO CRT-ERROR-NBR
                   MOVE PAT-REQUIRE-REASON         TO CRT-ERR-VAR1
                   IF (PA52F1-PCT-REASON (2) = SPACES)
                       MOVE PA52F1-PCT-REASON-FN (2)   TO CRT-FIELD-NBR
                   END-IF
                   IF (PA52F1-PCT-REASON (1) = SPACES)
                       MOVE PA52F1-PCT-REASON-FN (1)   TO CRT-FIELD-NBR
                   END-IF
                   GO TO 220-END.

           IF (PAT-REQUIRE-REASON = 1)
               IF  (PA52F1-PCT-REASON (1) = SPACES)
               AND (PA52F1-PCT-REASON (2) = SPACES)
                   MOVE 544                        TO CRT-ERROR-NBR
                   MOVE PAT-REQUIRE-REASON         TO CRT-ERR-VAR1
                   IF (PA52F1-PCT-REASON (1) = SPACES)
                       MOVE PA52F1-PCT-REASON-FN (1)   TO CRT-FIELD-NBR
                   END-IF
                   GO TO 220-END.

           IF  (PA52F1-PCT-REASON (1) = PA52F1-PCT-REASON (2))
           AND (PA52F1-PCT-REASON (1) NOT = SPACES)
               MOVE 194                            TO CRT-ERROR-NBR
               MOVE PA52F1-PCT-REASON-FN (2)       TO CRT-FIELD-NBR
               GO TO 220-END.

029300     PERFORM 222-EDIT-REASONS
029400     THRU    222-END
029500         VARYING I1 FROM 1 BY 1
029600         UNTIL  (I1 > 2)
029700         OR     (ERROR-FOUND).
029800
029900     IF (ERROR-FOUND)
030000         GO TO 220-END.
030100
029900     IF (ERROR-FOUND)
030000         GO TO 220-END.
030100
           MOVE PA52F1-PCT-EDM-EFFECT-DT   TO PRPXL-PARM-EFF-DATE.
           MOVE PA52F1-PCT-EDM-END-DATE    TO PRPXL-PARM-END-DATE.

030200     INITIALIZE HREMP-SCR-FIELDS.
030300     INITIALIZE HRPEM-SCR-FIELDS.
030400     MOVE "C"                     TO HREMP-FC.
030500     MOVE PA52F1-PCT-COMPANY      TO HREMP-COMPANY.
030600     MOVE PA52F1-PCT-COMPANY-FN   TO HREMP-COMPANY-FN.
030700     MOVE PA52F1-PCT-EMPLOYEE     TO HREMP-EMPLOYEE.
030800     MOVE PA52F1-PCT-EMPLOYEE-FN  TO HREMP-EMPLOYEE-FN.
030900     MOVE PA52F1-IMMEDIATE-ACTION TO HREMP-IMM-ACTION.
031000     MOVE PA52F1-PCT-UPDATE-BENEFIT
031100                                  TO HREMP-UPDATE-BENEFIT.
           MOVE PA52F1-PCT-MOVE-FROM-LEVEL
                                        TO HREMP-POS-LEVEL-MOVE.
031200     MOVE "Y"                     TO HREMP-ACTION.
031300     MOVE "Y"                     TO HREMP-EDIT-VALUE-LIST.
031400     MOVE PA52F1-PCT-EFFECT-DATE  TO HREMP-EFFECT-DATE.
           MOVE PA52F1-XMIT-HREMP-BLOCK TO HREMP-XMIT-BLOCK.
           MOVE PAT-HIST-CORR-FLAG      TO HREMP-HIST-OVERRIDE-SW.
           IF  (PA52F1-PCT-HIST-CORR-FLAG NOT = SPACES)
               MOVE PA52F1-PCT-HIST-CORR-FLAG
                                        TO HREMP-HIST-OVERRIDE-SW.
           MOVE PA52F1-PCT-PROCESS-TYPE TO HREMP-PROCESS-TYPE-SW.
031700
           SET PA52WS-NOTHING-ENTERED   TO TRUE.
           SET PA52WS-FIELDS-EXIST      TO TRUE.
           INITIALIZE PA52WS-STEP-BLANKED-SW.

032000     PERFORM
032100         VARYING I1 FROM 1 BY 1
032200         UNTIL  (I1 > 12)
032300             MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
032400                                  TO PA52WS-PCT-NEW-VALUE (I1)
                   MOVE PA52F1-PAT-FLD-NBR-1 (I1)
                                               TO HREMP-FLD-NBR
                   MOVE "Y"                    TO HREMP-FORMAT-FIELD
                   MOVE PA52F1-PCT-COMPANY     TO HREMP-COMPANY
                   MOVE PA52F1-PCT-EMPLOYEE    TO HREMP-EMPLOYEE
                   MOVE SPACES                 TO HREMP-VALUE
                   PERFORM 5000-HREMP-RETRIEVE-VALUE
                   MOVE HREMP-VALUE     TO PA52WS-PCT-PRE-VALUE (I1)
                   MOVE PA52F1-PAT-FLD-NBR-1 (I1)
                                        TO PA52WS-PAT-FIELD-NBR (I1)
                   IF  (PA52F1-PAT-FLD-NBR-1 (I1) = HREMP-PAY-STEP-DN)
                   AND ((PA52F1-PCT-NEW-VALUE-1 (I1) = "*BLANK")
                   OR   (PA52F1-PCT-NEW-VALUE-1 (I1) = "*blank"))
                       MOVE WS-TRUE     TO PA52WS-STEP-BLANKED-SW
                   END-IF
032700     END-PERFORM.
           
032900     PERFORM
033000         VARYING I1 FROM 13 BY 1
033100         UNTIL  (I1 > 24)
033200             COMPUTE I8 = (I1 - 12)
033300             MOVE PA52F1-PCT-NEW-VALUE-2 (I8)
033400                                  TO PA52WS-PCT-NEW-VALUE (I1)
                   MOVE PA52F1-PAT-FLD-NBR-2 (I8)
                                               TO HREMP-FLD-NBR
                   MOVE "Y"                    TO HREMP-FORMAT-FIELD
                   MOVE PA52F1-PCT-COMPANY     TO HREMP-COMPANY
                   MOVE PA52F1-PCT-EMPLOYEE    TO HREMP-EMPLOYEE
                   MOVE SPACES                 TO HREMP-VALUE
                   PERFORM 5000-HREMP-RETRIEVE-VALUE
                   MOVE HREMP-VALUE     TO PA52WS-PCT-PRE-VALUE (I1)
                   MOVE PA52F1-PAT-FLD-NBR-2 (I8)
                                        TO PA52WS-PAT-FIELD-NBR (I1)
                   IF  (PA52F1-PAT-FLD-NBR-2 (I8) = HREMP-PAY-STEP-DN)
                   AND ((PA52F1-PCT-NEW-VALUE-2 (I8) = "*BLANK")
                   OR   (PA52F1-PCT-NEW-VALUE-2 (I8) = "*blank"))
                       MOVE WS-TRUE     TO PA52WS-STEP-BLANKED-SW
                   END-IF
033700     END-PERFORM.

033900     PERFORM
034000         VARYING I1 FROM 25 BY 1
034100         UNTIL  (I1 > 36)
034200             COMPUTE I8 = (I1 - 24)
034300             MOVE PA52F1-PCT-NEW-VALUE-3 (I8)
034400                                  TO PA52WS-PCT-NEW-VALUE (I1)
                   MOVE PA52F1-PAT-FLD-NBR-3 (I8)
                                               TO HREMP-FLD-NBR
                   MOVE "Y"                    TO HREMP-FORMAT-FIELD
                   MOVE PA52F1-PCT-COMPANY     TO HREMP-COMPANY
                   MOVE PA52F1-PCT-EMPLOYEE    TO HREMP-EMPLOYEE
                   MOVE SPACES                 TO HREMP-VALUE
                   PERFORM 5000-HREMP-RETRIEVE-VALUE
                   MOVE HREMP-VALUE     TO PA52WS-PCT-PRE-VALUE (I1)
                   MOVE PA52F1-PAT-FLD-NBR-3 (I8)
                                        TO PA52WS-PAT-FIELD-NBR (I1)
                   IF  (PA52F1-PAT-FLD-NBR-3 (I8) = HREMP-PAY-STEP-DN)
                   AND ((PA52F1-PCT-NEW-VALUE-3 (I8) = "*BLANK")
                   OR   (PA52F1-PCT-NEW-VALUE-3 (I8) = "*blank"))
                       MOVE WS-TRUE     TO PA52WS-STEP-BLANKED-SW
                   END-IF
034700     END-PERFORM.

052000     IF (PA52WS-PAT-FIELD-NBR (1) = ZEROS)
052200         SET PA52WS-NO-FIELDS-EXIST  TO TRUE
J52802* Check to see if any values were still entered
J52802         PERFORM VARYING I1 FROM 1 BY 1                  
J52802             UNTIL (I1 > 36)
J52802             OR    (ERROR-FOUND)
J52802             IF  (PA52WS-PCT-NEW-VALUE (I1) NOT = SPACES)
J52802                 MOVE 137        TO CRT-ERROR-NBR
J52802                 IF (I1 < 13)
J52802                     MOVE PA52F1-PCT-NEW-VALUE-1-FN (I1) 
J52802                                 TO CRT-FIELD-NBR
J52802                 ELSE
J52802                 IF (I1 > 24)
J52802                     COMPUTE I8 = (I1 - 24)
J52802                     MOVE PA52F1-PCT-NEW-VALUE-3-FN (I8) 
J52802                                 TO CRT-FIELD-NBR
J52802                 ELSE
J52802                 IF (I1 > 12)
J52802                     COMPUTE I8 = (I1 - 12)
J52802                     MOVE PA52F1-PCT-NEW-VALUE-2-FN (I8) 
J52802                                 TO CRT-FIELD-NBR
J52802                 END-IF
J52802                 END-IF
J52802                 END-IF
J52802             END-IF
J52802         END-PERFORM
052300         GO TO 220-END.
052400
           MOVE EMP-TERM-DATE          TO HREMP-TERM-DATE.

           INITIALIZE PA52WS-PAPEP-SW.

           INITIALIZE                     HRSEC-WORK-COUNTRY.
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36)
                   IF (PA52WS-PAT-FIELD-NBR (I1) =
                                   HREMP-WORK-COUNTRY-DN)
                       MOVE PA52WS-PCT-NEW-VALUE (I1)
                                             TO HRSEC-WORK-COUNTRY
                   ELSE
                       IF (PA52WS-PAT-FIELD-NBR (I1) =
                                   HREMP-PROCESS-LEVEL-DN)
                           MOVE PA52WS-PCT-NEW-VALUE (I1)
                                             TO HRSEC-PROCESS-LEVEL
                       END-IF
                   END-IF
           END-PERFORM.

           IF (HRSEC-WORK-COUNTRY = SPACES)
               MOVE EMP-WORK-COUNTRY         TO HRSEC-WORK-COUNTRY
           END-IF.

           IF (HRSEC-PROCESS-LEVEL = SPACES)
               MOVE EMP-PROCESS-LEVEL        TO HRSEC-PROCESS-LEVEL
           END-IF.

           MOVE PA52F1-PCT-COMPANY             TO DB-COMPANY.
           MOVE HRSEC-WORK-COUNTRY             TO DB-COUNTRY-CD-REQ.
           MOVE PASSET4-COUNTRY-CD-REQ         TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PASSET4.
           IF (PASCRTY-FOUND)
               SET PA52WS-COUNTRY-REQ          TO TRUE
           ELSE
               SET PA52WS-COMPANY-REQ          TO TRUE
           END-IF.
P48123    
P48123     MOVE EMP-CURRENCY-CODE              TO PAPCT-EMP-CURRENCY.
P48123     MOVE EMP-CURR-ND                    TO PAPCT-EMP-CURR-ND. 

P61169     INITIALIZE                          HREMP-PEP-FLD-CHG-SW.
P61169
034900     PERFORM 224-EDIT-NEW-VALUES
035000     THRU    224-END
035100         VARYING I1 FROM 1 BY 1
035200         UNTIL  (I1 > 36)
035300         OR     (ERROR-FOUND).
P60778
P60778     IF (PA52WS-PAPEP-NO-FLDS-FOUND)
P60778         SET NO-PEP-FLD-CHG              TO TRUE
P60778     END-IF.
035400
035500     IF (ERROR-FOUND)
035700         GO TO 220-END.
035800
035900     IF (PA52WS-NOTHING-ENTERED)
036000         MOVE 145                                TO CRT-ERROR-NBR
036100         MOVE PA52F1-PCT-NEW-VALUE-1-FN (1)      TO CRT-FIELD-NBR
036200         GO TO 220-END.
036300
      **** CHECK TO SEE IF TERM DATE IS USED ON PERSACTYPE RECORD ****
           SET TERM-OR-STATUS-NOTCHANGED   TO TRUE.
           SET TERM-NOTCHANGED             TO TRUE.
           INITIALIZE DB-EMP-STATUS.
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36)
               OR     (PAT-FLD-NBR (I1)    = ZEROES)
               OR     (PAT-FLD-NBR (I1)    = HREMP-TERM-DATE-DN)

               CONTINUE
           END-PERFORM.

           IF  (I1               <= 36)
           AND (PAT-FLD-NBR (I1) NOT = ZEROES)
      ******** CHECK TO SEE IF TERM DATE IS CHANGED ****
               IF (I1 >= 1) AND (I1 <= 12)
                   IF (PA52F1-PCT-NEW-VALUE-1 (I1) NOT = SPACES)
                       SET TERM-CHANGED            TO TRUE
                   END-IF
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   IF (PA52F1-PCT-NEW-VALUE-2 (I1 - 12) NOT = SPACES)
                       SET TERM-CHANGED                 TO TRUE
                   END-IF
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   IF (PA52F1-PCT-NEW-VALUE-3 (I1 - 24) NOT = SPACES)
                       SET TERM-CHANGED                 TO TRUE.

      **** CHECK TO SEE IF EMPLOYEE STATUS IS USED ON PERSACTYPE RECORD ****
           SET STATUS-NOTCHANGED           TO TRUE.
           INITIALIZE DB-EMP-STATUS.
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36)
               OR     (PAT-FLD-NBR (I1)    = ZEROES)
               OR     (PAT-FLD-NBR (I1)    = HREMP-EMP-STATUS-DN)

               CONTINUE
           END-PERFORM.

           IF  (I1               <= 36)
           AND (PAT-FLD-NBR (I1) NOT = ZEROES)
      ******** CHECK TO SEE IF EMPLOYEE STATUS IS CHANGED ****
               IF (I1 >= 1) AND (I1 <= 12)
                   IF (PA52F1-PCT-NEW-VALUE-1 (I1) NOT = SPACES)
                       MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                                   TO DB-EMP-STATUS
                       PERFORM 840-FIND-EMSSET1
                       IF  (EMSTATUS-FOUND)
                       AND (EMS-PAY-STATUS (1 : 1) = "N")
                           SET STATUS-CHANGED      TO TRUE
                       END-IF
                   END-IF
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   IF (PA52F1-PCT-NEW-VALUE-2 (I1 - 12) NOT = SPACES)
                       MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                                   TO DB-EMP-STATUS
                       PERFORM 840-FIND-EMSSET1
                       IF  (EMSTATUS-FOUND)
                       AND (EMS-PAY-STATUS (1 : 1) = "N")
                           SET STATUS-CHANGED      TO TRUE
                       END-IF
                   END-IF
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   IF (PA52F1-PCT-NEW-VALUE-3 (I1 - 24) NOT = SPACES)
                       MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                                   TO DB-EMP-STATUS
                       PERFORM 840-FIND-EMSSET1
                       IF  (EMSTATUS-FOUND)
                       AND (EMS-PAY-STATUS (1 : 1) = "N")
                           SET STATUS-CHANGED      TO TRUE.

           IF (TERM-CHANGED)
           OR (STATUS-CHANGED)
               SET TERM-OR-STATUS-CHANGED  TO TRUE.

           IF  (TERM-OR-STATUS-CHANGED)
           AND (PA52F1-PCT-UPDATE-REQ-DED  = "Y")
           AND (PA52F1-XMIT-REQDED         = ZEROES)
      ******** Warning, Req Deds will be updated for term emp, Press OK
038200         MOVE 1                      TO PA52F1-XMIT-REQDED
038300         MOVE 190                    TO CRT-ERROR-NBR
038400         GO TO 220-END.

036400     INITIALIZE                                HREMP-UPDPEP-DATE.
036500     MOVE PA52F1-PCT-COMPANY                TO PADT-COMPANY.
036600     MOVE PA52F1-PCT-EMPLOYEE               TO PADT-EMPLOYEE.
036700     MOVE EMP-TERM-DATE                     TO PADT-END-DATE.
036800     MOVE 1                                 TO PADT-POS-LEVEL.
036900     MOVE PA52F1-PCT-EFFECT-DATE            TO PADT-EFFECT-DATE.
037000     PERFORM 2300-PADT-DATE-CHECK.
037100     IF (ERROR-FOUND)
037200         GO TO 220-END
037300     ELSE
037400         INITIALIZE                       PA52WS-RETRO-POSITION
                                                PA52WS-POSITION
037500                                          PA52WS-RETRO-JOB-CODE
                                                PA52WS-JOB-CODE
037600                                          PA52WS-RETRO-PL
                                                PA52WS-PL
037700                                          PA52WS-RETRO-DEPARTMENT
                                                PA52WS-DEPARTMENT
                                                PA52WS-RETRO-SW.
037800     IF  (PADT-UPDPEP-DATE NOT = ZEROES)
038000         MOVE PADT-UPDPEP-DATE       TO HREMP-UPDPEP-DATE
038700         MOVE PA52F1-PCT-COMPANY        TO DB-COMPANY
038800         MOVE PA52F1-PCT-EMPLOYEE       TO DB-EMPLOYEE
038900         MOVE 1                         TO DB-POS-LEVEL
039000         MOVE PA52F1-PCT-EFFECT-DATE    TO DB-EFFECT-DATE
039100         PERFORM 850-FIND-NLT-PEPSET3
039200         IF  (PAEMPPOS-FOUND)
039300         AND (PEP-COMPANY   = DB-COMPANY)
039400         AND (PEP-EMPLOYEE  = DB-EMPLOYEE)
039500         AND (PEP-POS-LEVEL = DB-POS-LEVEL)
039600             MOVE PEP-POSITION         TO PA52WS-RETRO-POSITION
039700             MOVE PEP-JOB-CODE         TO PA52WS-RETRO-JOB-CODE
039800             MOVE PEP-PROCESS-LEVEL    TO PA52WS-RETRO-PL
039900             MOVE PEP-DEPARTMENT       TO PA52WS-RETRO-DEPARTMENT
                   SET PA52WS-RETRO          TO TRUE.
040000
038700     MOVE PA52F1-PCT-COMPANY        TO DB-COMPANY.
038800     MOVE PA52F1-PCT-EMPLOYEE       TO DB-EMPLOYEE.
038900     MOVE 1                         TO DB-POS-LEVEL.
P58592     MOVE PEPSET3-POS-LEVEL         TO WS-DB-BEG-RNG.
P58592     PERFORM 850-FIND-BEGRNG-PEPSET3.
039200     IF  (PAEMPPOS-FOUND)
039600         MOVE PEP-POSITION         TO PA52WS-POSITION
039700         MOVE PEP-JOB-CODE         TO PA52WS-JOB-CODE
039800         MOVE PEP-PROCESS-LEVEL    TO PA52WS-PL
039900         MOVE PEP-DEPARTMENT       TO PA52WS-DEPARTMENT.
040000
040100     MOVE "C"                      TO HREMP-FC.
           MOVE PA52F1-PCT-MOVE-FROM-LEVEL
                                         TO HREMP-POS-LEVEL-MOVE.
040200
040300     PERFORM 2000-HREMP-EDIT-TRAN.
           MOVE HREMP-CURRENCY-CODE    TO PAPCT-EMP-CURRENCY.
           MOVE HREMP-CURR-ND          TO PAPCT-EMP-CURR-ND.
           IF  ((HREMP-PAY-RATE      NOT = EMP-PAY-RATE)
            OR  (HREMP-CURRENCY-CODE NOT = EMP-CURRENCY-CODE))
           AND (HREMP-PAY-STEP = ZEROES)
               MOVE HREMP-CURRENCY-CODE   TO PA52F1-PCT-BASE-CURRENCY
               MOVE HREMP-CURR-ND         TO PA52F1-PCT-BASE-ND
               MOVE HREMP-PAY-RATE        TO PA52F1-PCT-BASE-PAY-RATE
           ELSE
               INITIALIZE PA52F1-PCT-BASE-CURRENCY
                          PA52F1-PCT-BASE-ND
                          PA52F1-PCT-BASE-PAY-RATE
           END-IF.

040400     MOVE HREMP-PAPOS-EFFECT-DATE   TO PA52WS-POS-EFFECT-DATE.
040500
040600     IF (HREMP-UPDPEP-DATE = PA52F1-PCT-EFFECT-DATE)
040700         INITIALIZE                 HREMP-UPDPEP-DATE.
040800
040900     PERFORM
041000       VARYING I1 FROM 1 BY 1
041100       UNTIL  (I1 > 12)
041200       OR     (PA52F1-PAT-ITEM-NAME-1 (I1) = SPACES)
041300        IF (PA52F1-PAT-FLD-NBR-1 (I1) =
041400                                 HREMP-ACCT-CATEGORY-DN)
              AND (PA52F1-PCT-NEW-VALUE-1 (I1) NOT = "*BLANK")
041500          IF (HREMP-ACCT-CATEGORY =
041600                                 PA52F1-PCT-PRE-VALUE-1 (I1))
041700            MOVE SPACES              TO
041800                                 PA52F1-PCT-NEW-VALUE-1 (I1)
041900          ELSE
042000            MOVE HREMP-ACCT-CATEGORY TO
042100                                 PA52F1-PCT-NEW-VALUE-1 (I1)
042200          END-IF
042300          MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
042400                                     TO PA52WS-PCT-NEW-VALUE (I1)
042500     END-PERFORM.
042600
042700     PERFORM
042800       VARYING I1 FROM 1 BY 1
042900       UNTIL  (I1 > 12)
043000       OR     (PA52F1-PAT-ITEM-NAME-2 (I1) = SPACES)
043100        IF (PA52F1-PAT-FLD-NBR-2 (I1) =
043200                                 HREMP-ACCT-CATEGORY-DN)
              AND (PA52F1-PCT-NEW-VALUE-2 (I1) NOT = "*BLANK")
043300          IF (HREMP-ACCT-CATEGORY =
043400                                 PA52F1-PCT-PRE-VALUE-2 (I1))
043500            MOVE SPACES              TO
043600                                 PA52F1-PCT-NEW-VALUE-2 (I1)
043700          ELSE
043800            MOVE HREMP-ACCT-CATEGORY TO
043900                                 PA52F1-PCT-NEW-VALUE-2 (I1)
044000          END-IF
044100          COMPUTE I8 = (I1 + 12)
044200          MOVE PA52F1-PCT-NEW-VALUE-2 (I1)
044300                                     TO PA52WS-PCT-NEW-VALUE (I8)
044400     END-PERFORM.
044500
044600     PERFORM
044700       VARYING I1 FROM 1 BY 1
044800       UNTIL  (I1 > 12)
044900       OR     (PA52F1-PAT-ITEM-NAME-3 (I1) = SPACES)
045000        IF (PA52F1-PAT-FLD-NBR-3 (I1) =
045100                                 HREMP-ACCT-CATEGORY-DN)
              AND (PA52F1-PCT-NEW-VALUE-3 (I1) NOT = "*BLANK")
045200          IF (HREMP-ACCT-CATEGORY =
045300                                 PA52F1-PCT-PRE-VALUE-3 (I1))
045400            MOVE SPACES              TO
045500                                 PA52F1-PCT-NEW-VALUE-3 (I1)
045600          ELSE
045700            MOVE HREMP-ACCT-CATEGORY TO
045800                                 PA52F1-PCT-NEW-VALUE-3 (I1)
045900          END-IF
046000          COMPUTE I8 = (I1 + 24)
046100          MOVE PA52F1-PCT-NEW-VALUE-3 (I1)
046200                                     TO PA52WS-PCT-NEW-VALUE (I8)
046300     END-PERFORM.
046400
           MOVE HREMP-XMIT-BLOCK       TO PA52F1-XMIT-HREMP-BLOCK.
046700
046800     IF (ERROR-FOUND)
046900         GO TO 220-END.

      * 225-EDIT is a 2nd pass of 224-EDIT for Currency fields
034900     PERFORM 225-EDIT-NEW-VALUES2
035000     THRU    225-END
035100         VARYING I1 FROM 1 BY 1
035200         UNTIL  (I1 > 36)
035300         OR     (ERROR-FOUND).
035400
035500     IF (ERROR-FOUND)
035700         GO TO 220-END.
047000
           IF  (PA52F1-IMMEDIATE-ACTION = "Y")
           AND (PA52F1-XMIT-IMMED       = ZEROS)
               MOVE 1                  TO PA52F1-XMIT-IMMED
               MOVE 404                TO CRT-ERROR-NBR
               GO TO 220-END.

022000     IF  (PA52F1-IMMEDIATE-ACTION    = "Y")
           AND (PA52F1-PCT-UPDATE-REQ-DED  = "Y")
467300         MOVE PA52F1-PCT-COMPANY       TO PRRQC-COMPANY
467400         MOVE PA52F1-PCT-EMPLOYEE      TO PRRQC-EMPLOYEE
467500         INITIALIZE PRRQC-DFT-MAR-STAT
467700                    PRRQC-DFT-EXEMPTS
               MOVE PA52F1-PCT-EDM-EFFECT-DT TO PRRQC-EFFECT-DATE
               MOVE PA52F1-PCT-EDM-END-DATE  TO PRRQC-END-DATE
               MOVE "N"                      TO PRRQC-UPDATE-OPTION
467800         PERFORM 500-REQ-DED-CREATION
               IF (ERROR-FOUND)
                   GO TO 220-END.
467900
047100 220-END.
047200******************************************************************
047300 221-GET-PARTICIPNT-NUMBER.
047400******************************************************************
047500
047600     IF (PA52WS-PARTICIPNT = ZEROS)
047700         ADD 1                   TO PA52WS-PARTICIPNT
047800     ELSE
047900         IF (PARTICIPNT-KNOTFOUND)
048000         OR (PAR-COMPANY NOT = DB-COMPANY)
048100             MOVE WS-TRUE        TO PA52WS-NUMBER-SW
048200         ELSE
048300             IF (PAR-PARTICIPNT NOT = PA52WS-PARTICIPNT)
048400                 MOVE WS-TRUE    TO PA52WS-NUMBER-SW
048500             ELSE
048600                 ADD 1           TO PA52WS-PARTICIPNT
048700                 PERFORM 860-KFIND-NEXT-PARSET1.
048800
048900 221-END.
049000******************************************************************
049100 222-EDIT-REASONS.
049200******************************************************************
049300
049400     IF (PA52F1-PCT-REASON (I1) = SPACES)
049500         GO TO 222-END.
049600
           SET PA52WS-NO-ACTION-REASON         TO TRUE.

           MOVE PA52F1-PCT-COMPANY             TO DB-COMPANY.
           MOVE PA52F1-PCT-ACTION-CODE         TO DB-ACTION-CODE.
049700     MOVE CRESET4-ACTION-CODE            TO WS-DB-BEG-RNG.
           PERFORM 850-KFIND-BEGRNG-CRESET4.
           IF (PAACTREAS-KFOUND)
               SET PA52WS-ACTION-REASON        TO TRUE
           END-IF.

050000     IF  (PA52F1-FC              = "A")
050100     OR ((PA52F1-FC              = "C")
050200     AND (PA52F1-PCT-REASON (I1) NOT = PCT-REASON (I1))
           AND (PA52F1-PCT-REASON (I1) NOT = SPACES))
               MOVE PA52F1-PCT-REASON (I1)     TO DB-ACT-REASON-CD
               IF (PA52WS-ACTION-REASON)
                   PERFORM 840-FIND-CRESET4
               ELSE
                   MOVE CRESET3-ACT-REASON-CD  TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-CRESET3
               END-IF
050400         IF (PAACTREAS-NOTFOUND)
                   IF (PA52WS-ACTION-REASON)
                       MOVE 142                TO CRT-ERROR-NBR
                   ELSE
050500                 MOVE 140                TO CRT-ERROR-NBR
                   END-IF
050600             MOVE PA52F1-PCT-REASON-FN (I1)      TO CRT-FIELD-NBR
050700             GO TO 222-END
050800         ELSE
050900             IF (CRE-ACTIVE-FLAG NOT = "1")
051000                 MOVE 141                        TO CRT-ERROR-NBR
051100                 MOVE PA52F1-PCT-REASON-FN (I1)  TO CRT-FIELD-NBR
051200                 GO TO 222-END.
051300
051400 222-END.
051500
051600******************************************************************
051700 224-EDIT-NEW-VALUES.
051800******************************************************************
051900
055500     IF  (PA52WS-PCT-NEW-VALUE (I1) NOT = SPACES)
055600     AND (PA52WS-PAT-FIELD-NBR (I1)     = ZEROS)
055700         MOVE 137                                TO CRT-ERROR-NBR
055800         IF (I1 < 13)
055900             MOVE PA52F1-PCT-NEW-VALUE-1-FN (I1) TO CRT-FIELD-NBR
056000         ELSE
056100         IF (I1 > 24)
056200             COMPUTE I8 = (I1 - 24)
056300             MOVE PA52F1-PCT-NEW-VALUE-3-FN (I8) TO CRT-FIELD-NBR
056400         ELSE
056500         IF (I1 > 12)
056600             COMPUTE I8 = (I1 - 12)
056700             MOVE PA52F1-PCT-NEW-VALUE-2-FN (I8) TO CRT-FIELD-NBR
056800         END-IF
056900         END-IF
057000         END-IF
057100         GO TO 224-END.

           IF  (PA52WS-PAT-FIELD-NBR (I1) = ZEROES)
           AND ((I1 NOT = 36)
           OR   (PRS-EEO4-OPTION NOT = "Y"))
               GO TO 224-END.

      * Needed to produce edit for functional group
           IF  (PA52WS-PAT-FIELD-NBR (I1) = HRPEM-FNCTN-GROUP-DN)
               IF (I1 < 13)
                   MOVE PA52F1-PCT-NEW-VALUE-1-FN (I1)
                                       TO PA52WS-FNCTN-FN
               ELSE
               IF (I1 > 24)
                   COMPUTE I8 = (I1 - 24)
                   MOVE PA52F1-PCT-NEW-VALUE-3-FN (I8)
                                       TO PA52WS-FNCTN-FN
               ELSE
               IF (I1 > 12)
                   COMPUTE I8 = (I1 - 12)
                   MOVE PA52F1-PCT-NEW-VALUE-2-FN (I8)
                                       TO PA52WS-FNCTN-FN
               END-IF
               END-IF
               END-IF
               IF (PA52WS-PCT-NEW-VALUE (I1) = SPACES)
                   IF (PA52WS-PCT-PRE-VALUE (I1) = SPACES)
                       MOVE 2            TO PA52WS-FNCTN-SW  
                   ELSE
                       MOVE 1            TO PA52WS-FNCTN-SW  
                   END-IF
               ELSE
               IF (PA52WS-PCT-NEW-VALUE (I1) = "*BLANK")
                   MOVE 2                TO PA52WS-FNCTN-SW  
               ELSE
                   MOVE 1                TO PA52WS-FNCTN-SW  
               END-IF
           ELSE
           IF (PA52WS-PAT-FIELD-NBR (I1) = HRPEM-EXCLUDE-FLAG-DN)
               IF (I1 < 13)
                   MOVE PA52F1-PCT-NEW-VALUE-1-FN (I1)
                                       TO PA52WS-EXCLUDE-FN
               ELSE
               IF (I1 > 24)
                   COMPUTE I8 = (I1 - 24)
                   MOVE PA52F1-PCT-NEW-VALUE-3-FN (I8)
                                       TO PA52WS-EXCLUDE-FN
               ELSE
               IF (I1 > 12)
                   COMPUTE I8 = (I1 - 12)
                   MOVE PA52F1-PCT-NEW-VALUE-2-FN (I8)
                                       TO PA52WS-EXCLUDE-FN
               END-IF
               END-IF
               END-IF
               IF (PA52WS-PCT-NEW-VALUE (I1) = SPACES)
                   IF (PA52WS-PCT-PRE-VALUE (I1) = SPACES)
                       MOVE 2            TO PA52WS-EXCLUDE-SW  
                   ELSE
                       MOVE 1            TO PA52WS-EXCLUDE-SW  
                   END-IF
               ELSE
               IF (PA52WS-PCT-NEW-VALUE (I1) = "*BLANK")
                   MOVE 2                TO PA52WS-EXCLUDE-SW  
               ELSE
                   MOVE 1                TO PA52WS-EXCLUDE-SW  
                END-IF
           END-IF 
           END-IF.

           IF  (I1 = 36)
               IF  (PA52WS-EMPTY-ON-SCR-F)
               AND (PA52WS-EMPTY-ON-SCR-E)
                  MOVE 270                TO CRT-ERROR-NBR
                  MOVE PA52WS-FNCTN-FN    TO CRT-FIELD-NBR
               ELSE
               IF  (PA52WS-EMPTY-ON-SCR-F)
               AND (PA52WS-NOT-ON-SCR-E)
                  IF (PEM-EXCLUDE-FLAG = SPACES)
                      MOVE 270                TO CRT-ERROR-NBR
                      MOVE PA52WS-FNCTN-FN    TO CRT-FIELD-NBR
                  END-IF
               ELSE
               IF  (PA52WS-EMPTY-ON-SCR-E)
               AND (PA52WS-NOT-ON-SCR-F)
P02857*            IF (PEM-FNCTN-GROUP = SPACES)
P02857             IF (PEM-FNCTN-GROUP = ZEROES)
                      MOVE 270                TO CRT-ERROR-NBR
                      MOVE PA52WS-EXCLUDE-FN  TO CRT-FIELD-NBR
                  END-IF
               END-IF 
               END-IF 
               END-IF
           END-IF.

           IF  (PA52WS-PAT-FIELD-NBR (I1) = ZEROES)
               GO TO 224-END.

           IF  (PA52WS-PCT-PRE-VALUE (I1) = PA52WS-SEC-MESSAGE)
           AND (PA52WS-PCT-NEW-VALUE (I1) NOT = SPACES)
               MOVE 193                TO CRT-ERROR-NBR
               IF (I1 < 13)
                   MOVE PA52F1-PCT-NEW-VALUE-1-FN (I1) TO CRT-FIELD-NBR
               ELSE
               IF (I1 > 24)
                   COMPUTE I8 = (I1 - 24)
                   MOVE PA52F1-PCT-NEW-VALUE-3-FN (I8) TO CRT-FIELD-NBR
               ELSE
               IF (I1 > 12)
                   COMPUTE I8 = (I1 - 12)
                   MOVE PA52F1-PCT-NEW-VALUE-2-FN (I8) TO CRT-FIELD-NBR
               END-IF
               END-IF
               END-IF
               GO TO 224-END.

           IF  (PA52WS-PAT-FIELD-NBR (I1) = HREMP-BASE-PAY-RATE-DN)
           AND (PA52WS-PCT-NEW-VALUE (I1) NOT = SPACES)
               MOVE 192                TO CRT-ERROR-NBR
               IF (I1 < 13)
                   MOVE PA52F1-PCT-NEW-VALUE-1-FN (I1) TO CRT-FIELD-NBR
               ELSE
               IF (I1 > 24)
                   COMPUTE I8 = (I1 - 24)
                   MOVE PA52F1-PCT-NEW-VALUE-3-FN (I8) TO CRT-FIELD-NBR
               ELSE
               IF (I1 > 12)
                   COMPUTE I8 = (I1 - 12)
                   MOVE PA52F1-PCT-NEW-VALUE-2-FN (I8) TO CRT-FIELD-NBR
               END-IF
               END-IF
               END-IF
               GO TO 224-END.

           MOVE PA52WS-PAT-FIELD-NBR (I1)    TO PAPCT-FLD-NBR
                                                DB-FLD-NBR.

           IF  (HRWS-PAD-TYPE  (DB-FLD-NBR) = SPACES)
               PERFORM 840-FIND-PADSET1
               MOVE PAD-DATA-TYPE      TO HRWS-PAD-TYPE (DB-FLD-NBR)
               MOVE PAD-DECIMALS       TO HRWS-PAD-DEC  (DB-FLD-NBR)
               MOVE PAD-CURRENCY-FLAG  TO HRWS-PAD-CURR (DB-FLD-NBR)
               MOVE PAD-SIZE           TO HRWS-PAD-SIZE (DB-FLD-NBR)
               MOVE PAD-CASE-FLAG      TO HRWS-PAD-CASE (DB-FLD-NBR)
           END-IF.

052500     IF (PA52WS-PCT-NEW-VALUE (I1) = SPACES)
               IF  (PA52WS-PAT-FIELD-NBR (I1) = HREMP-PAY-RATE-DN)
               AND (PA52WS-STEP-BLANKED)
                   NEXT SENTENCE
               END-IF
052600         IF (PA52WS-PCT-PRE-VALUE (I1) = SPACES)
                   IF  (PA52WS-PAT-FIELD-NBR (I1) > 1999)
                   AND (PA52WS-PAT-FIELD-NBR (I1) < 2100)
                       MOVE PA52WS-PAT-FIELD-NBR (I1)
                                                   TO PA52WS-USER-FLD
                       MOVE PA52WS-USER-FLD        TO DB-FIELD-KEY
                       IF (PA52WS-COUNTRY-REQ)
                           MOVE HRSEC-WORK-COUNTRY TO DB-COUNTRY-CD-REQ
                       ELSE
                           INITIALIZE                 DB-COUNTRY-CD-REQ
                                                      DB-PROCESS-LEVEL
                       END-IF
                       PERFORM 840-FIND-PASSET1
                       IF  (PASCRTY-FOUND)
                       AND (PAS-REQ-FLAG = "X")
                           MOVE 50                 TO CRT-ERROR-NBR
                           IF (I1 < 13)
                               MOVE PA52F1-PCT-NEW-VALUE-1-FN (I1)
                                                   TO CRT-FIELD-NBR
                           ELSE
                           IF (I1 > 24)
                               COMPUTE I8 = (I1 - 24)
                               MOVE PA52F1-PCT-NEW-VALUE-3-FN (I8)
                                                   TO CRT-FIELD-NBR
                           ELSE
                           IF (I1 > 12)
                               COMPUTE I8 = (I1 - 12)
                               MOVE PA52F1-PCT-NEW-VALUE-2-FN (I8)
                                                   TO CRT-FIELD-NBR
                           END-IF
                           END-IF
                           END-IF
                       END-IF
                   ELSE
052700                 MOVE PA52F1-PCT-COMPANY          TO DB-COMPANY
052800                 MOVE PA52WS-PAT-FIELD-NBR (I1)   TO DB-FLD-NBR
                       IF (PA52WS-COUNTRY-REQ)
                           MOVE HRSEC-WORK-COUNTRY     TO
                                                       DB-COUNTRY-CD-REQ
                       ELSE
                           INITIALIZE                  DB-COUNTRY-CD-REQ
                                                       DB-PROCESS-LEVEL
                       END-IF
052900                 PERFORM 840-FIND-PASSET1
053000                 IF  (PASCRTY-FOUND)
053100                 AND (PAS-REQ-FLAG = "X")
053200                     MOVE 50                      TO CRT-ERROR-NBR
053300                     IF (I1 < 13)
053400                         MOVE PA52F1-PCT-NEW-VALUE-1-FN (I1)
053500                                                  TO CRT-FIELD-NBR
053600                     ELSE
053700                     IF (I1 > 24)
053800                         COMPUTE I8 = (I1 - 24)
053900                         MOVE PA52F1-PCT-NEW-VALUE-3-FN (I8)
054000                                                  TO CRT-FIELD-NBR
054100                     ELSE
054200                     IF (I1 > 12)
054300                         COMPUTE I8 = (I1 - 12)
054400                         MOVE PA52F1-PCT-NEW-VALUE-2-FN (I8)
054500                                                  TO CRT-FIELD-NBR
054600                     END-IF
054700                     END-IF
054800                     END-IF
054900                 END-IF
                   END-IF
               END-IF
055100         GO TO 224-END.
055200
           SET PA52WS-SOMETHING-ENTERED TO TRUE.

           PERFORM
               VARYING I2 FROM 1 BY 1
               UNTIL  (I2 > PA52WS-5-FIELD-COUNT)
               IF  (PAPCT-FLD-NBR = PA52WS-PAT-FLD-NBR-5 (I2))
                   SET PA52WS-PAPEP-FLDS-FOUND TO TRUE
               END-IF
           END-PERFORM.
057200
P33133     IF  (PA52WS-PAT-FIELD-NBR (I1) >= 2000)
P33133         INITIALIZE PAPCT-SCR-FIELDS  
P33133         MOVE PA52WS-PAT-FIELD-NBR(I1) TO PAPCT-FLD-NBR 
P33133         MOVE PA52WS-PCT-NEW-VALUE(I1) TO PAPCT-NEW-VALUE 
P33133         MOVE PA52F1-PCT-COMPANY       TO PAPCT-COMPANY  
P33133         MOVE "Y"                      TO PAPCT-EDIT-DATA-ONLY
P33133         SET PAPCT-CURRENCY            TO TRUE
P33133         SET PAPCT-REFORMAT            TO TRUE
P33133         IF  (I1 < 13)
P33133             MOVE PA52F1-PCT-NEW-VALUE-1-FN(I1)
P33133                                       TO PAPCT-FIELD-FN
P33133         ELSE
P33133         IF  (I1 > 24)
P33133             COMPUTE I8 = (I1 - 24)
P33133             MOVE PA52F1-PCT-NEW-VALUE-3-FN(I8)
P33133                                       TO PAPCT-FIELD-FN
P33133         ELSE
P33133         IF  (I1 > 12)
P33133             COMPUTE I8 = (I1 - 12)
P33133             MOVE PA52F1-PCT-NEW-VALUE-2-FN(I8)
P33133                                       TO PAPCT-FIELD-FN
P33133         END-IF
P33133         END-IF
P33133         END-IF
P33133         PERFORM 5100-EDIT-USER-FIELDS    
P33133         MOVE PAPCT-NEW-VALUE          TO PA52WS-PCT-NEW-VALUE(I1)
P45820         IF  (I1 < 13)
P45820             MOVE PAPCT-NEW-VALUE TO  PA52F1-PCT-NEW-VALUE-1 (I1)
P45820         ELSE
P45820         IF  (I1 > 24)
P45820             COMPUTE I8 = (I1 - 24)
P45820             MOVE PAPCT-NEW-VALUE TO  PA52F1-PCT-NEW-VALUE-3 (I8)
P45820         ELSE
P45820         IF  (I1 > 12)
P45820             COMPUTE I8 = (I1 - 12)
P45820             MOVE PAPCT-NEW-VALUE TO  PA52F1-PCT-NEW-VALUE-2 (I8)
P45820         END-IF
P45820         END-IF
P45820         END-IF
P45820                                           
P33133         GO TO 224-END
P33133     END-IF.
P33133 
P33133*    IF (PA52WS-PAT-FIELD-NBR (I1) > 1999)
P33133*        GO TO 224-END.
061100
061200     MOVE PA52WS-PCT-NEW-VALUE (I1)  TO HRWS-UP-FIELD.
061500
           IF  (HRWS-UP-FIELD (1:1) = "*")
           OR  ((HRWS-PAD-TYPE (PAPCT-FLD-NBR) = "A")
            AND (HRWS-PAD-CASE (PAPCT-FLD-NBR) = "Y"))
064900             PERFORM 760-HR-UPPER-CASE
065000             MOVE HRWS-UP-FIELD     TO PA52WS-PCT-NEW-VALUE (I1)
065100             IF (I1 < 13)
065200                 MOVE HRWS-UP-FIELD TO PA52F1-PCT-NEW-VALUE-1 (I1)
065300             ELSE
065400             IF (I1 > 24)
065500                 COMPUTE I8 = (I1 - 24)
065600                 MOVE HRWS-UP-FIELD TO PA52F1-PCT-NEW-VALUE-3 (I8)
065700             ELSE
065800             IF (I1 > 12)
065900                 COMPUTE I8 = (I1 - 12)
066000                 MOVE HRWS-UP-FIELD
066000                                    TO PA52F1-PCT-NEW-VALUE-2 (I8)
                   END-IF.
066100
066200     IF (PA52WS-PCT-NEW-VALUE (I1) = PA52WS-PCT-PRE-VALUE (I1))
               IF  (PA52WS-PAT-FIELD-NBR (I1) = HREMP-PAY-RATE-DN)
               AND (PA52WS-STEP-BLANKED)
                   NEXT SENTENCE
               ELSE
P81486             IF  (PA52F1-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52WS-PCT-NEW-VALUE (I1)
P81486             ELSE
066300                  MOVE 146                       TO CRT-ERROR-NBR
P81486             END-IF
               END-IF
066400         IF (I1 < 13)
P81486             IF  (PA52F1-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F1-PCT-NEW-VALUE-1 (I1)
P81486             ELSE
066500                  MOVE PA52F1-PCT-NEW-VALUE-1-FN (I1)
066600                                                 TO CRT-FIELD-NBR
P81486             END-IF
066700         ELSE
066800         IF (I1 > 24)
066900             COMPUTE I8 = (I1 - 24)
P81486             IF  (PA52F1-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F1-PCT-NEW-VALUE-3 (I8)
P81486             ELSE
067000                  MOVE PA52F1-PCT-NEW-VALUE-3-FN (I8)
067100                                                 TO CRT-FIELD-NBR
P81486             END-IF
067200         ELSE
067300         IF (I1 > 12)
067400             COMPUTE I8 = (I1 - 12)
P81486             IF  (PA52F1-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F1-PCT-NEW-VALUE-2 (I8)
P81486             ELSE
067500                  MOVE PA52F1-PCT-NEW-VALUE-2-FN (I8)
067600                                                 TO CRT-FIELD-NBR
P81486             END-IF
067700         END-IF
067800         END-IF
P81486         END-IF
067900         GO TO 224-END.
068000
068100     IF  (PA52WS-PCT-NEW-VALUE (I1) = "*BLANK")
068200     AND (PA52WS-PCT-PRE-VALUE (I1) = SPACES)
P81486         IF  (PA52F1-PCT-APPROVAL-FLAG = "L")
P81486              MOVE SPACES           TO PA52WS-PCT-NEW-VALUE (I1)
P81486         ELSE
068300              MOVE 147                           TO CRT-ERROR-NBR
P81486         END-IF
068400         IF (I1 < 13)
P81486             IF  (PA52F1-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F1-PCT-NEW-VALUE-1 (I1)
P81486             ELSE
068500                  MOVE PA52F1-PCT-NEW-VALUE-1-FN (I1)
068600                                                 TO CRT-FIELD-NBR
P81486             END-IF
068700         ELSE
068800         IF (I1 > 24)
068900             COMPUTE I8 = (I1 - 24)
P81486             IF  (PA52F1-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F1-PCT-NEW-VALUE-3 (I8)
P81486             ELSE  
069000                  MOVE PA52F1-PCT-NEW-VALUE-3-FN (I8)
069100                                                 TO CRT-FIELD-NBR
P81486             END-IF
069200         ELSE
069300         IF (I1 > 12)
069400             COMPUTE I8 = (I1 - 12)
P81486             IF  (PA52F1-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F1-PCT-NEW-VALUE-2 (I8)
P81486             ELSE
069500                  MOVE PA52F1-PCT-NEW-VALUE-2-FN (I8)
069600                                                 TO CRT-FIELD-NBR
P81486             END-IF
069700         END-IF
069800         END-IF
069900         END-IF
070000         GO TO 224-END.
070100
070200     IF (PA52WS-PCT-NEW-VALUE (I1) NOT = "*BLANK")
070300         MOVE PA52WS-PCT-NEW-VALUE (I1)    TO PAPCT-NEW-VALUE
070400         MOVE PA52WS-PCT-NEW-VALUE (I1)    TO PAPCT-FIELD
070500         IF (I1 < 13)
070600             MOVE PA52F1-PCT-NEW-VALUE-1-FN (I1)
070700                                           TO PAPCT-FIELD-FN
070800         ELSE
070900         IF (I1 > 24)
071000             COMPUTE I8 = (I1 - 24)
071100             MOVE PA52F1-PCT-NEW-VALUE-3-FN (I8)
071200                                           TO PAPCT-FIELD-FN
071300         ELSE
071400         IF (I1 > 12)
071500             COMPUTE I8 = (I1 - 12)
071600             MOVE PA52F1-PCT-NEW-VALUE-2-FN (I8)
071700                                           TO PAPCT-FIELD-FN
071800         END-IF
071900         END-IF
072000         END-IF
072200         MOVE HRWS-PAD-TYPE (PAPCT-FLD-NBR)  TO PAPCT-TYPE
072300         MOVE HRWS-PAD-SIZE (PAPCT-FLD-NBR)  TO PAPCT-SIZE
               MOVE HRWS-PAD-DEC (PAPCT-FLD-NBR)   TO PAPCT-DECIMALS
               SET PAPCT-NO-CURRENCY TO TRUE
               SET PAPCT-NO-REFORMAT TO TRUE
072500         PERFORM 5400-EDIT-CORRECT-FORMAT
072600         IF (ERROR-FOUND)
072700             GO TO 224-END
072800         ELSE
072900             MOVE PAPCT-NEW-VALUE    TO PA52WS-PCT-NEW-VALUE (I1)
073000             IF (I1 < 13)
073100                 MOVE PAPCT-NEW-VALUE
073200                                 TO PA52F1-PCT-NEW-VALUE-1 (I1)
073300             ELSE
073400             IF (I1 > 24)
073500                 COMPUTE I8 = (I1 - 24)
073600                 MOVE PAPCT-NEW-VALUE
073700                                 TO PA52F1-PCT-NEW-VALUE-3 (I8)
073800             ELSE
073900             IF (I1 > 12)
074000                 COMPUTE I8 = (I1 - 12)
074100                 MOVE PAPCT-NEW-VALUE
074200                                 TO PA52F1-PCT-NEW-VALUE-2 (I8).

074400     INITIALIZE PAPCT-SCR-FIELDS.
074500     MOVE PA52F1-PCT-COMPANY           TO PAPCT-COMPANY.
074600     MOVE PA52F1-PCT-EMPLOYEE          TO PAPCT-EMPLOYEE.
074700     MOVE PA52WS-PAT-FIELD-NBR (I1)    TO PAPCT-FLD-NBR.
074800     MOVE PA52WS-PCT-NEW-VALUE (I1)    TO PAPCT-NEW-VALUE.
           MOVE PA52F1-PCT-EFFECT-DATE       TO PAPCT-EFFECT-DATE.
074900     IF (I1 < 13)
075000         MOVE PA52F1-PCT-NEW-VALUE-1-FN (I1)
075100                                       TO PAPCT-FIELD-FN
075200     ELSE
075300     IF (I1 > 24)
075400         COMPUTE I8 = (I1 - 24)
075500         MOVE PA52F1-PCT-NEW-VALUE-3-FN (I8)
075600                                       TO PAPCT-FIELD-FN
075700     ELSE
075800     IF (I1 > 12)
075900         COMPUTE I8 = (I1 - 12)
076000         MOVE PA52F1-PCT-NEW-VALUE-2-FN (I8)
076100                                       TO PAPCT-FIELD-FN.
076200     PERFORM 5000-PAPCT-MOVE-TO-HREMP.
076300
076400 224-END.
076500
051600******************************************************************
051700 225-EDIT-NEW-VALUES2.
051800******************************************************************
051900
           IF  (PA52WS-PAT-FIELD-NBR (I1) = ZEROES)
           OR  (PA52WS-PCT-NEW-VALUE (I1) = SPACES)
           OR  (PA52WS-PCT-NEW-VALUE (I1) = "*BLANK")
               GO TO 225-END.

           MOVE PA52WS-PAT-FIELD-NBR (I1)    TO PAPCT-FLD-NBR
                                                DB-FLD-NBR.

           IF  (HRWS-PAD-TYPE  (DB-FLD-NBR) = SPACES)
               PERFORM 840-FIND-PADSET1
               MOVE PAD-DATA-TYPE      TO HRWS-PAD-TYPE (DB-FLD-NBR)
               MOVE PAD-DECIMALS       TO HRWS-PAD-DEC  (DB-FLD-NBR)
               MOVE PAD-CURRENCY-FLAG  TO HRWS-PAD-CURR (DB-FLD-NBR)
               MOVE PAD-SIZE           TO HRWS-PAD-SIZE (DB-FLD-NBR)
               MOVE PAD-CASE-FLAG      TO HRWS-PAD-CASE (DB-FLD-NBR)
           END-IF.

           INITIALIZE PAPCT-SCR-FIELDS.
           MOVE PA52WS-PAT-FIELD-NBR (I1)    TO PAPCT-FLD-NBR.
           MOVE PA52WS-PCT-NEW-VALUE (I1)    TO PAPCT-NEW-VALUE.
           IF  (HRWS-PAD-CURR (PAPCT-FLD-NBR) = "Y")
               MOVE PAPCT-EMP-CURRENCY       TO PAPCT-CURRENCY-CODE
               MOVE PAPCT-EMP-CURR-ND        TO PAPCT-CURR-ND
           END-IF.

           IF (I1 < 13)
               MOVE PA52F1-PCT-NEW-VALUE-1-FN (I1) TO PAPCT-FIELD-FN
           ELSE
           IF (I1 > 24)
               COMPUTE I8 = (I1 - 24)
               MOVE PA52F1-PCT-NEW-VALUE-3-FN (I8) TO PAPCT-FIELD-FN
           ELSE
           IF (I1 > 12)
               COMPUTE I8 = (I1 - 12)
               MOVE PA52F1-PCT-NEW-VALUE-2-FN (I8) TO PAPCT-FIELD-FN
           END-IF.
           MOVE PA52F1-PCT-COMPANY           TO PAPCT-COMPANY.
           MOVE PA52F1-PCT-EMPLOYEE          TO PAPCT-EMPLOYEE.
           MOVE PA52F1-PCT-EFFECT-DATE       TO PAPCT-EFFECT-DATE.

           IF (PA52WS-PAT-FIELD-NBR (I1) < 2000)
               SET PAPCT-CURRENCY TO TRUE
               SET PAPCT-REFORMAT TO TRUE
               MOVE HRWS-PAD-TYPE (PAPCT-FLD-NBR)  TO PAPCT-TYPE
               MOVE HRWS-PAD-SIZE (PAPCT-FLD-NBR)  TO PAPCT-SIZE
               MOVE HRWS-PAD-DEC (PAPCT-FLD-NBR)   TO PAPCT-DECIMALS
               PERFORM 5400-EDIT-CORRECT-FORMAT
           ELSE
               PERFORM 5100-EDIT-USER-FIELDS.

           MOVE PAPCT-NEW-VALUE        TO PA52WS-PCT-NEW-VALUE (I1).
           IF (I1 < 13)
               MOVE PAPCT-NEW-VALUE  TO PA52F1-PCT-NEW-VALUE-1 (I1)
           ELSE
           IF (I1 > 24)
               COMPUTE I8 = (I1 - 24)
               MOVE PAPCT-NEW-VALUE  TO PA52F1-PCT-NEW-VALUE-3 (I8)
           ELSE
           IF (I1 > 12)
               COMPUTE I8 = (I1 - 12)
               MOVE PAPCT-NEW-VALUE  TO PA52F1-PCT-NEW-VALUE-2 (I8)
           END-IF.

           IF (PA52WS-PCT-NEW-VALUE (I1) = PA52WS-PCT-PRE-VALUE (I1))
               IF  (PA52WS-PAT-FIELD-NBR (I1) = HREMP-PAY-RATE-DN)
               AND (PA52WS-STEP-BLANKED)
                   NEXT SENTENCE
               ELSE
P81486             IF  (PA52F1-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52WS-PCT-NEW-VALUE (I1)
P81486             ELSE
                        MOVE 146                       TO CRT-ERROR-NBR
P81486             END-IF
               END-IF
               IF (I1 < 13)
P81486             IF  (PA52F1-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F1-PCT-NEW-VALUE-1 (I1)
P81486             ELSE
                        MOVE PA52F1-PCT-NEW-VALUE-1-FN (I1)
                                                       TO CRT-FIELD-NBR
P81486             END-IF
               ELSE
               IF (I1 > 24)
                   COMPUTE I8 = (I1 - 24)
P81486             IF  (PA52F1-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F1-PCT-NEW-VALUE-3 (I8)
P81486             ELSE
                        MOVE PA52F1-PCT-NEW-VALUE-3-FN (I8)
                                                       TO CRT-FIELD-NBR
P81486             END-IF
               ELSE
               IF (I1 > 12)
                   COMPUTE I8 = (I1 - 12)
P81486             IF  (PA52F1-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F1-PCT-NEW-VALUE-2 (I8)
P81486             ELSE
                        MOVE PA52F1-PCT-NEW-VALUE-2-FN (I8)
                                                       TO CRT-FIELD-NBR
P81486             END-IF
               END-IF
               END-IF
P81486         END-IF
               GO TO 225-END.

           IF  (PA52WS-PCT-NEW-VALUE (I1) = "*BLANK")
           AND (PA52WS-PCT-PRE-VALUE (I1) = SPACES)
P81486         IF  (PA52F1-PCT-APPROVAL-FLAG = "L")
P81486              MOVE SPACES           TO PA52WS-PCT-NEW-VALUE (I1)
P81486         ELSE
                    MOVE 147                           TO CRT-ERROR-NBR
P81486         END-IF
               IF (I1 < 13)
P81486             IF  (PA52F1-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F1-PCT-NEW-VALUE-1 (I1)
P81486             ELSE  
                        MOVE PA52F1-PCT-NEW-VALUE-1-FN (I1)
                                                       TO CRT-FIELD-NBR
P81486             END-IF
               ELSE
               IF (I1 > 24)
                   COMPUTE I8 = (I1 - 24)
P81486             IF  (PA52F1-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F1-PCT-NEW-VALUE-3 (I8)
P81486             ELSE
                        MOVE PA52F1-PCT-NEW-VALUE-3-FN (I8)
                                                       TO CRT-FIELD-NBR
P81486             END-IF
               ELSE
               IF (I1 > 12)
                   COMPUTE I8 = (I1 - 12)
P81486             IF  (PA52F1-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F1-PCT-NEW-VALUE-2 (I8)
P81486             ELSE
                        MOVE PA52F1-PCT-NEW-VALUE-2-FN (I8)
                                                       TO CRT-FIELD-NBR
P81486             END-IF
               END-IF
               END-IF
               END-IF
               GO TO 225-END.

076400 225-END.
076500
      ******************************************************************
       230-EDIT-SPECIAL-PROCESSING.
      ******************************************************************

           IF  (PA52F1-FC               = "R")
           AND (PA52F1-PCT-PROCESS-TYPE NOT = "4")
      **** Incorrect process-type for this special action ****
               MOVE 126                                TO CRT-ERROR-NBR
               MOVE PA52F1-PCT-PROCESS-TYPE-FN         TO CRT-FIELD-NBR
               GO TO 230-END.

           IF ((PA52F1-FC = "R")
           OR  (PA52F1-PCT-PROCESS-TYPE = "2" OR "3" OR "4" OR "5"))
           AND (PA52F1-PCT-HIST-CORR-FLAG = SPACES)
               MOVE PA52F1-PCT-COMPANY             TO DB-COMPANY
               MOVE PA52F1-PCT-ACTION-CODE         TO DB-ACTION-CODE
               PERFORM 840-FIND-PATSET1
               IF (PERSACTYPE-NOTFOUND)
                   MOVE 110                            TO CRT-ERROR-NBR
                   MOVE PA52F1-PCT-ACTION-CODE-FN      TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF
               IF (PAT-HIST-CORR-FLAG NOT = SPACES)
                   MOVE PAT-HIST-CORR-FLAG TO PA52F1-PCT-HIST-CORR-FLAG
               ELSE
               IF (PRS-HIST-CORR-FLAG NOT = SPACES)
                   MOVE PRS-HIST-CORR-FLAG TO PA52F1-PCT-HIST-CORR-FLAG
               ELSE
      **** History Correction method is required ****
      * This should never be blank as there is a default on HR00 that
      * sets the flag to 1.  This is also done on the upgrade.
                   MOVE 123                            TO CRT-ERROR-NBR
                   MOVE PA52F1-PCT-HIST-CORR-FLAG-FN   TO CRT-FIELD-NBR
                   GO TO 230-END.

           IF  (PA52F1-PCT-PROCESS-TYPE = "1")
           AND (PA52F1-FC               NOT = "C")
               MOVE 540                                TO CRT-ERROR-NBR
               MOVE PA52F1-PCT-PROCESS-TYPE-FN         TO CRT-FIELD-NBR
               GO TO 230-END.

           IF  (PA52F1-FC                 = "R")
           AND (PA52F1-PCT-MERGE-ACTN-NBR = ZEROES)
      **** Enter the Action number to be reversed ****
               MOVE 127                          TO CRT-ERROR-NBR
               MOVE PA52F1-PCT-MERGE-ACTN-NBR-FN TO CRT-FIELD-NBR
               GO TO 230-END.

           IF  (PA52F1-PCT-PROCESS-TYPE   = "2")
           AND (PA52F1-PCT-MERGE-ACTN-NBR NOT = ZEROES)
      **** Special Action Nbr not valid for this Process Type ****
               MOVE 528                          TO CRT-ERROR-NBR
               MOVE PA52F1-PCT-MERGE-ACTN-NBR-FN TO CRT-FIELD-NBR
               GO TO 230-END.

           IF  (PA52F1-PCT-PROCESS-TYPE   = "3")
           AND (PA52F1-PCT-MERGE-ACTN-NBR = ZEROES)
      **** Special Action Nbr is required for this Process Type ****
               MOVE 527                          TO CRT-ERROR-NBR
               MOVE PA52F1-PCT-MERGE-ACTN-NBR-FN TO CRT-FIELD-NBR
               GO TO 230-END.

           IF (PA52F1-PCT-PROCESS-TYPE = "3")
               MOVE PA52F1-PCT-EMPLOYEE              TO DB-EMPLOYEE
               MOVE PA52F1-PCT-EFFECT-DATE           TO DB-EFFECT-DATE
               MOVE PA52F1-PCT-ACTION-CODE           TO DB-ACTION-CODE
               MOVE PA52F1-PCT-MERGE-ACTN-NBR        TO DB-ACTION-NBR
               PERFORM 840-FIND-PAHSET1
               IF (PERSACTHST-FOUND)
                   MOVE PAH-OBJ-ID         TO PA52F1-MERGE-ACT-OBJ-ID
               ELSE
      **** Action to be combined doesn't exist in history ****
                   MOVE 529                            TO CRT-ERROR-NBR
                   MOVE PA52F1-PCT-MERGE-ACTN-NBR-FN   TO CRT-FIELD-NBR
                   GO TO 230-END.

           IF  (PA52F1-PCT-PROCESS-TYPE = "4")
               MOVE PA52F1-PCT-COMPANY         TO PAPCT-REV-COMPANY
               MOVE PA52F1-PCT-EMPLOYEE        TO PAPCT-REV-EMPLOYEE
               MOVE PA52F1-PCT-EFFECT-DATE     TO PAPCT-REV-EFFECT-DATE
               MOVE PA52F1-PCT-ACTION-CODE     TO PAPCT-REV-ACTION-CODE
               MOVE PA52F1-PCT-MERGE-ACTN-NBR  TO PAPCT-REV-ACTION-NBR
               MOVE 1                          TO PAPCT-REV-POS-LEVEL
               PERFORM 7000-PCT-EDIT-REVERSE
               IF  (ERROR-FOUND)
                   GO TO 230-END
               END-IF
               MOVE PAPCT-REV-ACT-OBJ-ID   TO HREMP-ACT-OBJ-ID-REV
                                              PA52F1-MERGE-ACT-OBJ-ID
           END-IF.

           IF  (PA52F1-PCT-NEW-EFFECT-DATE NOT = ZEROES)
           AND (PA52F1-PCT-PROCESS-TYPE    NOT = "1")
               MOVE 532                                TO CRT-ERROR-NBR
               MOVE PA52F1-PCT-NEW-EFFECT-DATE-FN      TO CRT-FIELD-NBR
               GO TO 230-END
           END-IF.

           IF  (PA52F1-PCT-PROCESS-TYPE   NOT = "1")
           AND (PA52F1-PCT-HIST-CORR-FLAG NOT = "1")
               MOVE PA52F1-PCT-COMPANY  TO DB-COMPANY
               MOVE PA52F1-PCT-EMPLOYEE TO DB-EMPLOYEE
               MOVE 1                   TO DB-POS-LEVEL
               MOVE PEASET1-POS-LEVEL   TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PEASET1
               IF (PAPEPAUDIT-FOUND)
      **** History Correction must = 1: pending correction exists ****
                   MOVE 530             TO CRT-ERROR-NBR
                   MOVE PA52F1-PCT-HIST-CORR-FLAG-FN
                                        TO CRT-FIELD-NBR
                   GO TO 230-END.

           IF   (PA52F1-PCT-PROCESS-TYPE    = SPACES)
           AND  (PA52F1-PCT-MOVE-FROM-LEVEL NOT = ZEROES)
      **** Special processing fields not valid when no Processing Type *
               IF  (PA52F1-PCT-NEW-EFFECT-DATE NOT = ZEROES)
                   MOVE 129                            TO CRT-ERROR-NBR
                   MOVE PA52F1-PCT-NEW-EFFECT-DATE-FN   TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF
               IF  (PA52F1-PCT-HIST-CORR-FLAG NOT = SPACES)
                   MOVE 129                            TO CRT-ERROR-NBR
                   MOVE PA52F1-PCT-HIST-CORR-FLAG-FN   TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF
               IF  (PA52F1-PCT-MERGE-ACTN-NBR NOT = ZEROES)
                   MOVE 129                            TO CRT-ERROR-NBR
                   MOVE PA52F1-PCT-PROCESS-TYPE-FN     TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF
           END-IF.

           IF (PA52F1-PCT-PROCESS-TYPE = "1")
               IF (PA52F1-FC = "A")
      **** Pending action doesn't exist; can't change effective date ***
                   MOVE 122                          TO CRT-ERROR-NBR
                   MOVE PA52F1-PCT-NEW-EFFECT-DATE-FN TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF
               IF  (PA52F1-PCT-NEW-EFFECT-DATE = ZEROES)
      **** New Effective Date is required for this Process Type ****
                   MOVE 526                          TO CRT-ERROR-NBR
                   MOVE PA52F1-PCT-NEW-EFFECT-DATE-FN TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF.

      **** If changing effective date, no other fields can be changed **
      **** Also not allowed to change any value on a reversal record  **
           IF  (PA52F1-FC         = "C")
           AND ((PCT-PROCESS-TYPE = "4")
            OR  (PA52F1-PCT-PROCESS-TYPE = "1"))
               IF  (PA52F1-PCT-REASON (1)     NOT = PCT-REASON (1))
               OR  (PA52F1-PCT-REASON (2)     NOT = PCT-REASON (2))
               OR  (PA52F1-PCT-ANT-END-DATE   NOT = PCT-ANT-END-DATE)
               OR  (PA52F1-PCT-UPDATE-BENEFIT NOT = PCT-UPDATE-BENEFIT)
               OR  (PA52F1-PCT-UPD-ABS-MGMT   NOT = PCT-UPD-ABS-MGMT)
               OR  (PA52F1-PCT-UPDATE-REQ-DED NOT = PCT-UPDATE-REQ-DED)
               OR  (PA52F1-PCT-EDM-END-DATE   NOT = PCT-EDM-END-DATE)
               OR  (PA52F1-PCT-EDM-EFFECT-DT  NOT = PCT-EDM-EFFECT-DT)
               OR  (PA52F1-PCT-OCCUR-TYPE     NOT = PCT-OCCUR-TYPE)
               OR  (PA52F1-PCT-PARTICIPNT     NOT = PCT-PARTICIPNT)
               OR  (PA52F1-PCT-MERGE-ACTN-NBR NOT = PCT-MERGE-ACTN-NBR)
               OR  (PA52F1-PCT-HIST-CORR-FLAG NOT = PCT-HIST-CORR-FLAG)
               OR  ((PCT-PROCESS-TYPE = "4")
                AND (PA52F1-PCT-PROCESS-TYPE   NOT = PCT-PROCESS-TYPE))
                   MOVE PA52F1-PCT-PROCESS-TYPE-FN     TO CRT-FIELD-NBR
                   IF  (PA52F1-PCT-REASON (1)     NOT = PCT-REASON (1))
                       MOVE PA52F1-PCT-REASON-FN (1)   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F1-PCT-REASON (2)     NOT = PCT-REASON (2))
                       MOVE PA52F1-PCT-REASON-FN (2)   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F1-PCT-ANT-END-DATE  NOT = PCT-ANT-END-DATE)
                       MOVE PA52F1-PCT-ANT-END-DATE-FN TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F1-PCT-UPDATE-BENEFIT
                                               NOT = PCT-UPDATE-BENEFIT)
                       MOVE PA52F1-PCT-UPDATE-BENEFIT-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F1-PCT-UPD-ABS-MGMT
                                               NOT = PCT-UPD-ABS-MGMT)
                       MOVE PA52F1-PCT-UPD-ABS-MGMT-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F1-PCT-UPDATE-REQ-DED
                                               NOT = PCT-UPDATE-REQ-DED)
                       MOVE PA52F1-PCT-UPDATE-REQ-DED-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F1-PCT-EDM-END-DATE  NOT = PCT-EDM-END-DATE)
                       MOVE PA52F1-PCT-EDM-END-DATE-FN TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F1-PCT-EDM-EFFECT-DT
                                               NOT = PCT-EDM-EFFECT-DT)
                       MOVE PA52F1-PCT-EDM-EFFECT-DT-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F1-PCT-OCCUR-TYPE     NOT = PCT-OCCUR-TYPE)
                       MOVE PA52F1-PCT-OCCUR-TYPE-FN   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F1-PCT-PARTICIPNT     NOT = PCT-PARTICIPNT)
                       MOVE PA52F1-PCT-PARTICIPNT-FN   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F1-PCT-MERGE-ACTN-NBR
                                               NOT = PCT-MERGE-ACTN-NBR)
                       MOVE PA52F1-PCT-MERGE-ACTN-NBR-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F1-PCT-HIST-CORR-FLAG
                                               NOT = PCT-HIST-CORR-FLAG)
                       MOVE PA52F1-PCT-HIST-CORR-FLAG-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF (PCT-PROCESS-TYPE = "4")
      **** Action is being reversed, values can not be changed ****
                       MOVE 537              TO CRT-ERROR-NBR
                   ELSE
                       MOVE 124              TO CRT-ERROR-NBR
                   END-IF
                   GO TO 230-END
               END-IF
           END-IF.

      **** If changing effective date, no other fields can be changed **
      **** Also not allowed to change any value on a reversal record  **
           IF  (PA52F1-FC         = "C")
           AND ((PCT-PROCESS-TYPE = "4")
            OR  (PA52F1-PCT-PROCESS-TYPE = "1"))
               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > 12)
                   COMPUTE I2 = I1 + 12
                   COMPUTE I3 = I1 + 24
                   IF (PA52F1-PCT-NEW-VALUE-1 (I1) NOT =
                                                     PCT-NEW-VALUE (I1))
                       MOVE PA52F1-PCT-NEW-VALUE-1-FN (I1)
                                                       TO CRT-FIELD-NBR
                       MOVE 124                        TO CRT-ERROR-NBR
                   ELSE
                   IF (PA52F1-PCT-NEW-VALUE-2 (I1) NOT =
                                                     PCT-NEW-VALUE (I2))
                       MOVE PA52F1-PCT-NEW-VALUE-2-FN (I1)
                                                       TO CRT-FIELD-NBR
                       MOVE 124                        TO CRT-ERROR-NBR
                   ELSE
                   IF (PA52F1-PCT-NEW-VALUE-3 (I1) NOT =
                                                     PCT-NEW-VALUE (I3))
                       MOVE PA52F1-PCT-NEW-VALUE-3-FN (I1)
                                                       TO CRT-FIELD-NBR
                       MOVE 124                        TO CRT-ERROR-NBR
                   END-IF
                   END-IF
                   END-IF
               END-PERFORM
               IF  (ERROR-FOUND)
                   IF  (PCT-PROCESS-TYPE = "4")
      **** Action is being reversed, values can not be changed ****
                       MOVE 537                        TO CRT-ERROR-NBR
                    END-IF
                    GO TO 230-END
               END-IF
           END-IF.

           IF  (PA52F1-FC               = "A")
           AND (PA52F1-PCT-PROCESS-TYPE = "4")
               IF (PA52F1-SVD-ACTN-NBR = ZEROES)
      **** Must perform Special Reverse Action before adding action ****
                   MOVE 542                TO CRT-ERROR-NBR
                   MOVE PA52F1-PCT-MERGE-ACTN-NBR-FN
                                           TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF                         
               IF (PA52F1-PCT-MERGE-ACTN-NBR NOT = PA52F1-SVD-ACTN-NBR)
      **** Action number changed; perform the reverse again ****
                   MOVE 538              TO CRT-ERROR-NBR
                   MOVE PA52F1-PCT-MERGE-ACTN-NBR-FN TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF.

           IF  (PA52F1-FC               = "A")
           AND (PA52F1-PCT-PROCESS-TYPE = "4")
               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > 12)
                   IF (PA52F1-PCT-NEW-VALUE-1 (I1) NOT =
                                          PA52F1-REV-SVD-VALUE-1 (I1))
      **** Value changed; perform Reverse again ****
                       MOVE 539                        TO CRT-ERROR-NBR
                       MOVE PA52F1-PCT-NEW-VALUE-1-FN (I1)
                                                       TO CRT-FIELD-NBR
                       GO TO 230-END
                   END-IF
               END-PERFORM
               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > 12)
                   IF (PA52F1-PCT-NEW-VALUE-2 (I1) NOT =
                                          PA52F1-REV-SVD-VALUE-2 (I1))
      **** Value changed; perform Reverse again ****
                       MOVE 539                        TO CRT-ERROR-NBR
                       MOVE PA52F1-PCT-NEW-VALUE-2-FN (I1)
                                                       TO CRT-FIELD-NBR
                       GO TO 230-END
                   END-IF
               END-PERFORM
               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > 12)
                   IF (PA52F1-PCT-NEW-VALUE-3 (I1) NOT =
                                          PA52F1-REV-SVD-VALUE-3 (I1))
      **** Value changed; perform Reverse again ****
                       MOVE 539                        TO CRT-ERROR-NBR
                       MOVE PA52F1-PCT-NEW-VALUE-3-FN (I1)
                                                       TO CRT-FIELD-NBR
                       GO TO 230-END
                   END-IF
               END-PERFORM

           IF (PA52F1-FC = "R")
               PERFORM 235-EDIT-REVERSAL
               THRU    235-END.

           IF  (PA52F1-PCT-MERGE-ACTN-NBR NOT = ZEROES)
           AND (PAH-POS-LEVEL             NOT = 1)
      **** Action number selected does not match position level ****
               MOVE 531                TO CRT-ERROR-NBR
               MOVE PA52F1-PCT-MERGE-ACTN-NBR-FN
                                       TO CRT-FIELD-NBR
               GO TO 230-END
           END-IF.

           IF  (PA52F1-PCT-PROCESS-TYPE     NOT = "5")
           AND (PA52F1-PCT-MOVE-FROM-LEVEL  NOT = ZEROES)
               MOVE 153                    TO CRT-ERROR-NBR
               MOVE PA52F1-PCT-PROCESS-TYPE-FN
                                           TO CRT-FIELD-NBR
               GO TO 230-END.

           IF (PA52F1-PCT-PROCESS-TYPE = "5")
               MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY
               MOVE PA52F1-PCT-EMPLOYEE    TO DB-EMPLOYEE
               MOVE PA52F1-PCT-MOVE-FROM-LEVEL
                                           TO DB-POS-LEVEL
               MOVE PEPSET3-POS-LEVEL      TO WS-DB-BEG-RNG 
               PERFORM 850-KFIND-BEGRNG-PEPSET3
               IF (PAEMPPOS-KNOTFOUND)
                   MOVE 154                TO CRT-ERROR-NBR
                   MOVE PA52F1-PCT-MOVE-FROM-LEVEL-FN
                                           TO CRT-FIELD-NBR
                   GO TO 230-END.

           IF (PA52F1-PCT-PROCESS-TYPE NOT = "5")
               GO TO 230-END.

           MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY.
           MOVE PA52F1-PT-ACTION-TYPE  TO DB-ACTION-TYPE.
           MOVE PA52F1-PCT-EMPLOYEE    TO DB-EMPLOYEE.
           MOVE PCTSET2-EMPLOYEE       TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PCTSET2.
           IF (PERSACTION-FOUND)
               PERFORM
                   UNTIL (PERSACTION-NOTFOUND)
                   OR   ((PCT-EFFECT-DATE > PA52F1-PCT-EFFECT-DATE)
                   AND   ((PCT-POS-LEVEL = 1)
                   OR     (PCT-POS-LEVEL = PA52F1-PCT-MOVE-FROM-LEVEL)))
                       PERFORM 860-FIND-NXTRNG-PCTSET2
               END-PERFORM
               IF (PERSACTION-FOUND)
                   MOVE 155                TO CRT-ERROR-NBR
                   MOVE PA52F1-PCT-MOVE-FROM-LEVEL-FN
                                           TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF.

           MOVE "L"                        TO DB-ACTION-TYPE.
           MOVE PCTSET2-EMPLOYEE           TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PCTSET2.
           IF (PERSACTION-FOUND)
               PERFORM
                   UNTIL (PERSACTION-NOTFOUND)
                   OR   ((PCT-EFFECT-DATE > PA52F1-PCT-EFFECT-DATE)
                   AND   ((PCT-POS-LEVEL = 1)
                   OR     (PCT-POS-LEVEL = PA52F1-PCT-MOVE-FROM-LEVEL)))
                       PERFORM 860-FIND-NXTRNG-PCTSET2
               END-PERFORM
               IF (PERSACTION-FOUND)
                   MOVE 155                TO CRT-ERROR-NBR
                   MOVE PA52F1-PCT-MOVE-FROM-LEVEL-FN
                                           TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF.
               
           MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY.
           MOVE "E"                    TO DB-ACTION-TYPE.
           MOVE PA52F1-PCT-EMPLOYEE    TO DB-EMPLOYEE.
           MOVE PA52F1-PCT-ACTION-CODE TO DB-ACTION-CODE.
           MOVE PA52F1-PCT-EFFECT-DATE TO DB-EFFECT-DATE.
           MOVE PA52F1-PCT-ACTION-NBR  TO DB-ACTION-NBR.

           PERFORM 840-FIND-PCTSET2.

       230-END.

      ******************************************************************
       235-EDIT-REVERSAL.
      ******************************************************************

           INITIALIZE PA52F1-SVD-ACTN-NBR.

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL (I1 > 12)

               INITIALIZE PA52F1-REV-SVD-VALUE-1 (I1)
                          PA52F1-PCT-NEW-VALUE-1 (I1)

               INITIALIZE PA52F1-REV-SVD-VALUE-2 (I1)
                          PA52F1-PCT-NEW-VALUE-2 (I1)

               INITIALIZE PA52F1-REV-SVD-VALUE-3 (I1)
                          PA52F1-PCT-NEW-VALUE-3 (I1)

           END-PERFORM.

       235-END.

      ******************************************************************
       240-GET-NEXT-EMPLOYEE.
      ******************************************************************
      
           IF (PA52F1-FC = "N")
               PERFORM 860-FIND-NEXT-PCTSET2
                   UNTIL (PERSACTION-NOTFOUND)
                   OR    (PCT-COMPANY  NOT = EMP-COMPANY)
                   OR    (PCT-EMPLOYEE NOT = EMP-EMPLOYEE)
           ELSE
               PERFORM 870-FIND-PREV-PCTSET2
                   UNTIL (PERSACTION-NOTFOUND)
                   OR    (PCT-COMPANY  NOT = EMP-COMPANY)
                   OR    (PCT-EMPLOYEE NOT = EMP-EMPLOYEE)
           END-IF.

           IF (PERSACTION-NOTFOUND)
           OR (PCT-COMPANY NOT = DB-COMPANY)
               GO TO 240-END.

           MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE.
           PERFORM 840-FIND-EMPSET1.
      
           IF (EMPLOYEE-FOUND)
               MOVE EMP-COMPANY        TO CRT-COMPANY
               MOVE EMP-PROCESS-LEVEL  TO CRT-PROCESS-LEVEL
               PERFORM 700-HR-EMP-SECURITY.
      
       240-END.
J40356******************************************************************
J40356 300-LOAD-LTM-VALUES. 
J40356******************************************************************
J40356
J40356     IF (PA52WS-LTM-SELECTED)
J40356     AND (PA52F1-FC = "I")
J40356         MOVE PA52WS-L-COMPANY        TO HREMP-L-COMPANY
J40356         MOVE PA52WS-L-EMPLOYEE       TO HREMP-L-EMPLOYEE
J40356         MOVE PA52WS-L-STATUS         TO HREMP-L-STATUS
J40356         MOVE PA52WS-L-DATE-STAMP     TO HREMP-L-DATE-STAMP
J40356         MOVE PA52WS-L-TIME-STAMP     TO HREMP-L-TIME-STAMP
J40356         MOVE PA52WS-L-ERROR-FLAG     TO HREMP-L-ERROR-FLAG
J40356         MOVE PA52WS-L-WORKASSIGNMENT TO HREMP-L-WORKASSIGNMENT
J40356         MOVE PA52WS-L-ACTION-CODE    TO HREMP-L-ACTION-CODE
J40356         PERFORM 5010-HREMP-FIND-LTM-RECORD 
J40356         IF (HREMPRCVR-FOUND) 
J40356             IF (PA52F1-PCT-EFFECT-DATE = ZEROES)
J40356                 MOVE PA52WS-L-EFFECT-DATE
J40356                                      TO PA52F1-PCT-EFFECT-DATE 
J40356             END-IF
J40356             MOVE SPACES              TO PA52WS-PCT-NEW-VALUE (I1)
J40356             MOVE PA52F1-PCT-COMPANY  TO HREMP-COMPANY
J40356             MOVE PA52F1-PCT-EMPLOYEE TO HREMP-EMPLOYEE
J40356             MOVE SPACES              TO HREMP-VALUE
J40356             MOVE SPACES              TO HRWS-VALUE
J40356             IF (PA52WS-PAT-FIELD-NBR (I1) NOT = ZEROES)
J40356                 MOVE PA52WS-PAT-FIELD-NBR (I1)
J40356                                         TO HREMP-FLD-NBR
J40356                                            PA52WS-FLD-NBR
J40356                 PERFORM 5020-HREMP-RETRIEVE-LTM-VALUE
J40356                 MOVE HREMP-VALUE TO PA52WS-PCT-NEW-VALUE (I1)
J40356                 
J40356             END-IF
J40356
J40356*    If LTM value is blank, *BLANKS to PA52 value.    
J40356             IF ((PA52WS-PCT-NEW-VALUE (I1) = SPACES)
J40356             AND (PA52WS-PAT-FIELD-NBR (I1) NOT = ZEROES)         
J40356             AND (PA52WS-PCT-PRE-VALUE (I1) NOT = SPACES) 
J30998             AND (HRWS-LTM-VALUE-FOUND))
J40356                 MOVE "*BLANK" TO PA52WS-PCT-NEW-VALUE (I1)
J28495                 MOVE 1        TO PA52WS-FILLED-BY-LTM (I1)
J40356             END-IF
J40356
J40356*    If LTM value is same as existing value, don't pass to PA52.
J40356             IF (PA52WS-PCT-NEW-VALUE (I1)                  
J40356                                   = PA52WS-PCT-PRE-VALUE (I1))
J40356                 MOVE SPACES TO PA52WS-PCT-NEW-VALUE (I1)
J40356             END-IF 
J40356
J40356             IF (I1 < 13)
J40356                 MOVE PA52WS-PCT-NEW-VALUE (I1)
J40356                                  TO PA52F1-PCT-NEW-VALUE-1 (I1)
J28495                 MOVE PA52WS-FILLED-BY-LTM (I1) 
J28495                                  TO PA52WS-FILLED-BY-LTM-1 (I1)
J40356             ELSE
J40356                 IF (I1 > 24)
J40356                     COMPUTE I8 = (I1 - 24)
J40356                     MOVE PA52WS-PCT-NEW-VALUE (I1)
J40356                                   TO PA52F1-PCT-NEW-VALUE-3 (I8)
J28495                     MOVE PA52WS-FILLED-BY-LTM (I1)
J28495                                  TO PA52WS-FILLED-BY-LTM-3 (I8)
J40356                 ELSE
J40356                     IF (I1 > 12)
J40356                         COMPUTE I8 = (I1 - 12)
J40356                         MOVE PA52WS-PCT-NEW-VALUE (I1)
J40356                                   TO PA52F1-PCT-NEW-VALUE-2 (I8)
J28495                         MOVE PA52WS-FILLED-BY-LTM (I1)
J28495                                  TO PA52WS-FILLED-BY-LTM-2 (I8)
J40356                     END-IF
J40356                 END-IF
J40356             END-IF
J40356         END-IF
J40356     END-IF.
J40356
J40356 300-END.
      ******************************************************************
       400-PROCESS-TRAN.
      ******************************************************************
      
           IF (PA52F1-FC = "A")
               PERFORM 410-ADD
               THRU    410-END
           ELSE
           IF (PA52F1-FC = "C") 
               PERFORM 420-CHANGE
               THRU    420-END
           ELSE
           IF (PA52F1-FC = "D")
               PERFORM 440-DELETE
               THRU    440-END
           ELSE
           IF (PA52F1-FC = "R")
               PERFORM 450-REVERSE
               THRU    450-END
           ELSE
           IF (PA52F1-FC = "I" OR "N" OR "P")
               PERFORM 480-INQUIRE
               THRU    480-END.
      
           IF ((PA52F1-FC = "A" OR "C")
           AND (PA52F1-IMMEDIATE-ACTION NOT = "Y"))
           OR  (PA52F1-FC = "D")   
               INITIALIZE HREMP-SCR-FIELDS 
                          HRPEM-SCR-FIELDS
               PERFORM 7000-HREMP-DEFAULT
               PERFORM 7000-HRPEM-DEFAULT   
J45417         IF ((PA52F1-FC = "A")
J45417         AND (NO-ERROR-FOUND))
J45417             MOVE CRT-ADD-COMPLETE    TO CRT-MESSAGE
J45417         END-IF
J45417         IF ((PA52F1-FC = "C")
J45417         AND (NO-ERROR-FOUND))
J45417             MOVE CRT-CHG-COMPLETE    TO CRT-MESSAGE
J45417         END-IF
J45417         IF ((PA52F1-FC = "D")
J45417         AND (NO-ERROR-FOUND))
J45417             MOVE CRT-RECS-DELETED    TO CRT-MESSAGE
J45417         END-IF
J45417     END-IF.
      
       400-END.
079600******************************************************************
079700 410-ADD.
079800******************************************************************
079900
081100     PERFORM 910-AUDIT-BEGIN.
081200     IF (DMS-ABORTED)
081300         GO TO 410-END.
081400
081500     MOVE PA52F1-PCT-COMPANY          TO PAACT-COMPANY.
081600     MOVE "E"                         TO PAACT-ACTION-TYPE.
081700     MOVE PA52F1-PCT-EMPLOYEE         TO PAACT-EMPLOYEE.
081800     MOVE PA52F1-PCT-ACTION-CODE      TO PAACT-ACTION-CODE.
081900     MOVE PA52F1-PCT-EFFECT-DATE      TO PAACT-EFFECT-DATE.
082000     MOVE "Y"                         TO PAACT-HISTORY.
082100     PERFORM 2300-PAACT-ACTION-NBR.
082200     MOVE PAACT-ACTION-NBR            TO PA52WS-ACTION-NBR.
082300     MOVE "L"                         TO PAACT-ACTION-TYPE.
082400     PERFORM 2300-PAACT-ACTION-NBR.
082500     IF (PAACT-ACTION-NBR > PA52WS-ACTION-NBR)
082600         MOVE PAACT-ACTION-NBR        TO PA52WS-ACTION-NBR
082700                                         PA52F1-PCT-ACTION-NBR.
082800
CUSTOM**** IF (PAT-WORKFLOW-FLAG       = "Y")
J83708**** OR (PA52F1-IMMEDIATE-ACTION = "Y")   
ANGIE**    IF (PAT-WORKFLOW-FLAG       = "Y")
ANGIE      IF  (PAT-WORKFLOW-FLAG       = "N")
ACS002     AND (PA52F1-IMMEDIATE-ACTION = "Y")
               PERFORM 430-CREATE-WORKFLOW-TRIGGER
               THRU    430-END.
CRP001
CRP001     IF (PA52F1-IMMEDIATE-ACTION NOT = "Y")
CRP001         GO TO 410-CONTINUE.
CRP001
CRP001     MOVE PA52F1-PCT-ACTION-CODE   TO HRCRP-ACTION-CODE.
CRP001     PERFORM 9000-CHECK-ACTION-CODE.
CRP001     IF (HRCRP-RECALC-ACTION = "N")
CRP001         GO TO 410-CONTINUE.         
      *--- 429- is a copy of 430- with the IPA calls removed
      *--- so these two should be kept in sync if CTPs update 430-
           PERFORM 429-CREATE-WORKFLOW-TRIGGER
           THRU    429-END.
CRP001
CRP001*--- Check to see if standard hours can be changed for action
CRP001     MOVE HREMP-STAND-HOURS-DN  TO PA52WS-FLD-NBR.
CRP001     PERFORM 432-FIND-FIELD-USAGE
CRP001     THRU    432-END.
CRP001
CRP001*--- Old Standard Hours
CRP001     MOVE EMP-STAND-HOURS        TO PA52WS-NUM-15.
CRP001     MOVE PA52WS-ALPHA-15        TO HRCRP-OLD-STD-HOURS.      
CRP001     MOVE SPACES                 TO HRCRP-NEW-STD-HOURS.      
CRP001
CRP001*--- New Standard Hours
CRP001     IF (FIELD-USED)
CRP001         IF (I1 >= 1) AND (I1 <= 12)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
CRP001                                 TO HRCRP-NEW-STD-HOURS    
CRP001         ELSE
CRP001         IF (I1 >= 13) AND (I1 <= 24)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
CRP001                                 TO HRCRP-NEW-STD-HOURS     
CRP001         ELSE
CRP001         IF (I1 >= 25) AND (I1 <= 36)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
CRP001                                 TO HRCRP-NEW-STD-HOURS.   
CRP001
CRP001     IF (HRCRP-NEW-STD-HOURS    = PA52WS-BLANK)
CRP001         INITIALIZE HRCRP-NEW-STD-HOURS     
CRP001     ELSE
CRP001     IF (HRCRP-NEW-STD-HOURS    = SPACES)
CRP001         MOVE HRCRP-OLD-STD-HOURS     
CRP001                                 TO HRCRP-NEW-STD-HOURS.    
CRP001
CRP001*--- Check to see if anniversary dt can be changed for action
CRP001     MOVE HREMP-ANNIVERS-DATE-DN  TO PA52WS-FLD-NBR.
CRP001     PERFORM 432-FIND-FIELD-USAGE
CRP001     THRU    432-END.
CRP001
CRP001*--- Old Aniversary Date 
CRP001     MOVE EMP-ANNIVERS-DATE      TO HRCRP-OLD-ANNV-DATE.      
CRP001     MOVE SPACES                 TO HRCRP-NEW-ANNV-DATE,
US3233                                    HRCRP-WS-DATE.   
      *     DISPLAY "PA52PD TRACE001:",
      *             "EMP:", EMP-EMPLOYEE,
      *             "OLD:", HRCRP-OLD-ANNV-DATE,
      *             "NEW:", HRCRP-NEW-ANNV-DATE,
      *             "FIELD-USED:", PA52WS-FIELD-USED-SW,
      *             "I1:", I1.
CRP001
CRP001*--- New Anniversary date 
CRP001     IF (FIELD-USED)
CRP001         IF (I1 >= 1) AND (I1 <= 12)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
CRP001                                 TO HRCRP-WS-DATE    
CRP001         ELSE
CRP001         IF (I1 >= 13) AND (I1 <= 24)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
CRP001                                 TO HRCRP-WS-DATE     
CRP001         ELSE
CRP001         IF (I1 >= 25) AND (I1 <= 36)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
CRP001                                 TO HRCRP-WS-DATE.   
      *     DISPLAY "PA52PD TRACE002:",
      *             "HRCRP-WS-DATE:", HRCRP-WS-DATE.
CRP001
CRP001     IF (HRCRP-WS-DATE    = PA52WS-BLANK)
CRP001         INITIALIZE HRCRP-NEW-ANNV-DATE     
CRP001     ELSE
CRP001     IF (HRCRP-WS-DATE          = SPACES)
CRP001         MOVE HRCRP-OLD-ANNV-DATE    
CRP001                                 TO HRCRP-NEW-ANNV-DATE
CRP001     ELSE
CRP001         MOVE HRCRP-WS-DATE(1:2) TO HRCRP-NEW-ANNV-DATE(5:2)   
CRP001         MOVE HRCRP-WS-DATE(4:2) TO HRCRP-NEW-ANNV-DATE(7:2)   
CRP001         MOVE HRCRP-WS-DATE(7:4) TO HRCRP-NEW-ANNV-DATE(1:4).   

      *     DISPLAY "PA52PD TRACE003:",
      *             "NEW:", HRCRP-NEW-ANNV-DATE.
CRP001
CRP001*--- Check to see if Position changed for action
CRP001     MOVE HREMP-POSITION-DN     TO PA52WS-FLD-NBR.
CRP001     PERFORM 432-FIND-FIELD-USAGE
CRP001     THRU    432-END.
CRP001
CRP001*--- Old Position        
CRP001     MOVE EMP-POSITION           TO HRCRP-OLD-POSITION.       
CRP001     MOVE SPACES                 TO HRCRP-NEW-POSITION.       
CRP001
CRP001*--- New Positon            
CRP001     IF (FIELD-USED)
CRP001         IF (I1 >= 1) AND (I1 <= 12)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
CRP001                                 TO HRCRP-NEW-POSITION     
CRP001         ELSE
CRP001         IF (I1 >= 13) AND (I1 <= 24)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
CRP001                                 TO HRCRP-NEW-POSITION      
CRP001         ELSE
CRP001         IF (I1 >= 25) AND (I1 <= 36)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
CRP001                                 TO HRCRP-NEW-POSITION.    
CRP001
CRP001     IF (HRCRP-NEW-POSITION     = PA52WS-BLANK)
CRP001         INITIALIZE HRCRP-NEW-POSITION      
CRP001     ELSE
CRP001     IF (HRCRP-NEW-POSITION     = SPACES)
CRP001         MOVE HRCRP-OLD-POSITION     
CRP001                                 TO HRCRP-NEW-POSITION.    
CRP001
CRP001*--- Capture Current Mail Group                 
CRP001     MOVE HRPEM-MAIL-GROUP-DN   TO PA52WS-FLD-NBR.
CRP001     PERFORM 432-FIND-FIELD-USAGE
CRP001     THRU    432-END.
CRP001
CRP001*--- Current Mail Group  
CRP001     MOVE PEM-MAIL-GROUP         TO HRCRP-MAIL-GROUP.         
CRP001
CRP001*--- New mail Group            
CRP001     IF (FIELD-USED)
CRP001         IF (I1 >= 1) AND (I1 <= 12)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
CRP001                                 TO HRCRP-MAIL-GROUP       
CRP001         ELSE
CRP001         IF (I1 >= 13) AND (I1 <= 24)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
CRP001                                 TO HRCRP-MAIL-GROUP        
CRP001         ELSE
CRP001         IF (I1 >= 25) AND (I1 <= 36)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
CRP001                                 TO HRCRP-MAIL-GROUP.      
CRP001
CRP001     IF (HRCRP-MAIL-GROUP       = PA52WS-BLANK)
CRP001         INITIALIZE HRCRP-MAIL-GROUP
CRP001     ELSE 
CRP001     IF (HRCRP-MAIL-GROUP = SPACES)
CRP001         MOVE PEM-MAIL-GROUP    TO HRCRP-MAIL-GROUP.       
CRP001              
CRP001*--- Check to see if Roster Code changed for action
CRP001     MOVE 2097     TO PA52WS-FLD-NBR.
CRP001     PERFORM 432-FIND-FIELD-USAGE
CRP001     THRU    432-END.
CRP001
CRP001*--- Retrieve "A-ROSTER"       user field number
CRP001     MOVE "A-ROSTER CODE"        TO DB-FIELD-NAME.
CRP001     PERFORM 840-FIND-HRUSET2.
CRP001     IF (HRUSERFLDS-FOUND)
CRP001         MOVE HRCRP-COMPANY      TO DB-COMPANY
CRP001         MOVE HRCRP-EMPLOYEE     TO DB-EMPLOYEE
CRP001         MOVE ZERO               TO DB-EMP-APP
CRP001         MOVE HRU-FIELD-KEY      TO DB-FIELD-KEY
CRP001         PERFORM 840-FIND-HEUSET1
CRP001         IF (HREMPUSF-NOTFOUND)
CRP001             MOVE SPACES         TO HEU-A-FIELD
CRP001         END-IF
CRP001     ELSE
CRP001         MOVE SPACES             TO HEU-A-FIELD.
CRP001
CRP001*--- New Roster Code - if found save in HEU-A-FIELD for later check
CRP001*--- of value in 4th position
CRP001     IF (FIELD-USED)
CRP001         IF (I1 >= 1) AND (I1 <= 12)
CRP001           IF (PA52F1-PCT-NEW-VALUE-1(I1) NOT = SPACES)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
CRP001                                 TO HEU-A-FIELD        
CRP001           END-IF    
CRP001         ELSE
CRP001         IF (I1 >= 13) AND (I1 <= 24)
CRP001           IF (PA52F1-PCT-NEW-VALUE-2(I1 - 12) NOT = SPACES)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
CRP001                                 TO HEU-A-FIELD             
CRP001           END-IF    
CRP001         ELSE
CRP001         IF (I1 >= 25) AND (I1 <= 36)
CRP001           IF (PA52F1-PCT-NEW-VALUE-3(I1 - 24) NOT = SPACES)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
CRP001                                 TO HEU-A-FIELD.           
CRP001
CRP001*--- An "M" in the 4th position of the A-ROSTER Code user
CRP001*--- means that it is a rostered person
CRP001     IF (HEU-A-FIELD(4:1) = "M")
CRP001         MOVE "Y"                TO HRCRP-ROSTERED
CRP001     ELSE
CRP001         MOVE "N"                TO HRCRP-ROSTERED.
CRP001
CRP001*--- Check to see if CRP Elibilibty changed for action
CRP001     MOVE 2042     TO PA52WS-FLD-NBR.
CRP001     PERFORM 432-FIND-FIELD-USAGE
CRP001     THRU    432-END.
CRP001
CRP001*--- Retrieve "CRP-ELIGIBLITY"       user field number
CRP001     MOVE "CRP-ELIGIBILITY"        TO DB-FIELD-NAME.
CRP001     PERFORM 840-FIND-HRUSET2.
CRP001     IF (HRUSERFLDS-FOUND)
CRP001         MOVE HRCRP-COMPANY      TO DB-COMPANY
CRP001         MOVE HRCRP-EMPLOYEE     TO DB-EMPLOYEE
CRP001         MOVE ZERO               TO DB-EMP-APP
CRP001         MOVE HRU-FIELD-KEY      TO DB-FIELD-KEY
CRP001         PERFORM 840-FIND-HEUSET1
CRP001         IF (HREMPUSF-NOTFOUND)
CRP001             MOVE SPACES         TO HEU-A-FIELD
CRP001         END-IF
CRP001     ELSE
CRP001         MOVE SPACES             TO HEU-A-FIELD.
CRP001
CRP001*--- New Eligiblity Code 
CRP001     IF (FIELD-USED)
CRP001         IF (I1 >= 1) AND (I1 <= 12)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
CRP001                                 TO HEU-A-FIELD            
CRP001         ELSE
CRP001         IF (I1 >= 13) AND (I1 <= 24)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
CRP001                                 TO HEU-A-FIELD             
CRP001         ELSE
CRP001         IF (I1 >= 25) AND (I1 <= 36)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
CRP001                                 TO HEU-A-FIELD.           
CRP001
CRP001     MOVE HEU-A-FIELD        TO HRCRP-GF-OPTION.
CRP001     MOVE PA52F1-PCT-EFFECT-DATE TO HRCRP-GF-EFFECT-DATE.
CRP001
CRP001*--- Check to see if CRP Elibilibty changed for action
CRP001     MOVE 2045     TO PA52WS-FLD-NBR.
CRP001     PERFORM 432-FIND-FIELD-USAGE
CRP001     THRU    432-END.
CRP001
CRP001*--- Retrieve "A-CRP FULL"       user field number
CRP001     MOVE "A-CRP FULL"        TO DB-FIELD-NAME.
CRP001     PERFORM 840-FIND-HRUSET2.
CRP001     IF (HRUSERFLDS-FOUND)
CRP001         MOVE HRCRP-COMPANY      TO DB-COMPANY
CRP001         MOVE HRCRP-EMPLOYEE     TO DB-EMPLOYEE
CRP001         MOVE ZERO               TO DB-EMP-APP
CRP001         MOVE HRU-FIELD-KEY      TO DB-FIELD-KEY
CRP001         PERFORM 840-FIND-HEUSET1
CRP001         IF (HREMPUSF-NOTFOUND)
CRP001             MOVE SPACES         TO HEU-A-FIELD
CRP001         END-IF
CRP001     ELSE
CRP001         MOVE SPACES             TO HEU-A-FIELD.
CRP001
CRP001*--- New CRP FULL Code - if found save in HEU-A-FIELD
CRP001     IF (FIELD-USED)
CRP001         IF (I1 >= 1) AND (I1 <= 12)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
CRP001                                 TO HEU-A-FIELD            
CRP001         ELSE
CRP001         IF (I1 >= 13) AND (I1 <= 24)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
CRP001                                 TO HEU-A-FIELD             
CRP001         ELSE
CRP001         IF (I1 >= 25) AND (I1 <= 36)
CRP001             MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
CRP001                                 TO HEU-A-FIELD.           
CRP001
CRP001     MOVE HEU-A-FIELD        TO HRCRP-A-CRP-FULL.   
CRP001
CRP001*--- Check process type for multiple actions
CRP001     IF (PA52F1-PCT-PROCESS-TYPE NOT = SPACES)
CRP001         MOVE "Y"                TO HRCRP-COMBINED-FL
CRP001     ELSE
CRP001         MOVE "N"                TO HRCRP-COMBINED-FL.
CRP001
CRP001     PERFORM 2000-CREATE-CRP-TRIGGER.
CRP001 410-CONTINUE.

082900     IF (PA52F1-IMMEDIATE-ACTION = "N")
083000         PERFORM 800-CREATE-PERSACTION
083100         MOVE PA52F1-PCT-COMPANY    TO DB-COMPANY
083200         MOVE PA52F1-PCT-EMPLOYEE   TO DB-EMPLOYEE
083300         PERFORM 840-MODIFY-EMPSET1
083400         MOVE PA52WS-ACTION-NBR     TO PCT-ACTION-NBR
J04194         IF  (PA52F1-PCT-UPDATE-BENEFIT = SPACES)
J04194             IF (PAT-DFT-UPD-BN = SPACES OR "Y")
J04194                 MOVE PA52F1-PCT-COMPANY    TO DB-COMPANY
J04194                 PERFORM 840-KFIND-BNCSET1
J04194                 IF (BNCOMPANY-KFOUND)
J04194                     MOVE "Y" TO PA52F1-PCT-UPDATE-BENEFIT
J04194                 ELSE
J04194                     MOVE "N" TO PA52F1-PCT-UPDATE-BENEFIT
J04194                 END-IF
J04194             ELSE
J04194                 MOVE PAT-DFT-UPD-BN TO PA52F1-PCT-UPDATE-BENEFIT
J04194             END-IF
J04194         END-IF
J04194         IF (PA52F1-PCT-UPD-ABS-MGMT = SPACES)
J04194             IF (PAT-DFT-UPD-LP = SPACES OR "Y")
J13588*                MOVE PA52F1-PCT-COMPANY     TO EDCDWS-COMPANY
J13588*                MOVE "LP"                   TO EDCDWS-SYSTEM
J13588*                PERFORM 6000-IS-SYSTEM-TRIGGER-ENABLED
J13588*                IF  (EDCDWS-TRIGGER-ENABLED)
J04194                     MOVE "Y" TO PA52F1-PCT-UPD-ABS-MGMT
J13588*                ELSE
J13588*                    MOVE "N" TO PA52F1-PCT-UPD-ABS-MGMT
J13588*                END-IF
J04194             ELSE
J04194                 MOVE PAT-DFT-UPD-LP    TO PA52F1-PCT-UPD-ABS-MGMT
J04194             END-IF
J04194         END-IF
J04194         IF (PA52F1-PCT-UPDATE-REQ-DED = SPACES)
J04194             MOVE PAT-DFT-UPD-RQ-DED  TO PA52F1-PCT-UPDATE-REQ-DED
J04194         END-IF
083500         PERFORM 500-MOVE-DATA
083600         THRU    500-END
               IF  (PCT-POSITION   NOT = SPACES)
               AND (PCT-POS-EFF-DT NOT = ZEROES)
                   MOVE PCT-POSITION        TO DB-POSITION
                   MOVE PCT-POS-EFF-DT      TO DB-EFFECT-DATE
                   MOVE PPCSET1-EFFECT-DATE TO WS-DB-BEG-RNG
                   PERFORM 850-KFIND-BEGRNG-PPCSET1
                   IF (PAPOSCHG-KFOUND)
                       MOVE PCT-EMPLOYEE    TO DB-EMPLOYEE
                       INITIALIZE              DB-REQUISITION
                       MOVE PPESET2-REQUISITION
                                            TO WS-DB-BEG-RNG
                       PERFORM 850-FIND-BEGRNG-PPESET2
                       IF (PAPOSERR-NOTFOUND)
                           PERFORM 800-CREATE-PAPOSERR
                           MOVE PPC-COMPANY TO PPE-COMPANY
                           MOVE PPC-EFFECT-DATE
                                            TO PPE-EFFECT-DATE
                           MOVE PPC-POSITION
                                            TO PPE-POSITION
                           MOVE "1  "       TO PPE-UPDATE-TYPE
                           MOVE PCT-EMPLOYEE
                                            TO PPE-EMPLOYEE
                           INITIALIZE          PPE-REQUISITION
                           MOVE 1           TO PPE-POS-LEVEL (1)
                           PERFORM 820-STORE-PAPOSERR
                       END-IF
                   END-IF
               END-IF
083700         PERFORM 8200-STORE-PERSACTION
083800         IF (PCT-EFFECT-DATE   < EMP-PEND-ACT-DATE)
083900         OR (EMP-PEND-ACT-DATE = ZEROS)
084000             MOVE PCT-EFFECT-DATE   TO EMP-PEND-ACT-DATE
084100         END-IF
084200         PERFORM 820-STORE-EMPLOYEE
084300     ELSE
               IF  (PA52F1-PCT-PROCESS-TYPE = "3")
                   MOVE PA52F1-MERGE-ACT-OBJ-ID    TO DB-OBJ-ID
                                                      PA52WS-OBJ-ID
                   PERFORM 840-MODIFY-PAHSET2
                   IF  (PA52F1-PCT-ANT-END-DATE NOT = ZEROES)
                       MOVE PA52F1-PCT-ANT-END-DATE TO PAH-ANT-END-DATE
                   END-IF
                   IF  (PA52F1-PCT-REASON (1) NOT = SPACES)
                       MOVE PA52F1-PCT-REASON (1)  TO PAH-REASON (1)
                   END-IF
                   IF  (PA52F1-PCT-REASON (2) NOT = SPACES)
                       MOVE PA52F1-PCT-REASON (2)  TO PAH-REASON (2)
                   END-IF
                   IF  (PA52F1-USER-ID NOT = SPACES)
                       MOVE PA52F1-USER-ID         TO PAH-USER-ID
                   ELSE
                       MOVE CRT-USER-NAME          TO PAH-USER-ID
                   END-IF
                   MOVE WS-SYSTEM-DATE-YMD         TO PAH-DATE-STAMP
J04194             IF (PA52F1-PCT-UPDATE-BENEFIT = SPACES)
J04194                 IF (PAT-DFT-UPD-BN = SPACES OR "Y")
J04194                     MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY
J04194                     PERFORM 840-KFIND-BNCSET1
J04194                     IF (BNCOMPANY-KFOUND)
J04194                         MOVE "Y" TO PA52F1-PCT-UPDATE-BENEFIT
J04194                     ELSE
J04194                         MOVE "N" TO PA52F1-PCT-UPDATE-BENEFIT
J04194                     END-IF
J04194                 ELSE
J04194                     MOVE PAT-DFT-UPD-BN 
J04194                                      TO PA52F1-PCT-UPDATE-BENEFIT
J04194                 END-IF
J04194             END-IF
J04194             IF (PA52F1-PCT-UPD-ABS-MGMT = SPACES)
J04194                 IF (PAT-DFT-UPD-LP = SPACES OR "Y")
J13588*                    MOVE PA52F1-PCT-COMPANY    TO EDCDWS-COMPANY
J13588*                    MOVE "LP"                  TO EDCDWS-SYSTEM
J13588*                    PERFORM 6000-IS-SYSTEM-TRIGGER-ENABLED
J13588*                    IF (EDCDWS-TRIGGER-ENABLED)
J04194                         MOVE "Y" TO PA52F1-PCT-UPD-ABS-MGMT
J13588*                    ELSE
J13588*                        MOVE "N" TO PA52F1-PCT-UPD-ABS-MGMT
J13588*                    END-IF
J04194                 ELSE
J04194                     MOVE PAT-DFT-UPD-LP  
J04194                                  TO PA52F1-PCT-UPD-ABS-MGMT
J04194                 END-IF
J04194             END-IF
J04194             IF (PA52F1-PCT-UPDATE-REQ-DED = SPACES)
J04194                 MOVE PAT-DFT-UPD-RQ-DED  
J04194                                      TO PA52F1-PCT-UPDATE-REQ-DED
J04194             END-IF
P80029             MOVE HHMMSS                     TO PAH-TIME-STAMP
J08104             IF (PA52F1-FC = "A")
J08104                 IF  (PA52F1-USER-ID NOT = SPACES)
J08104                     MOVE PA52F1-USER-ID     TO PAH-CREATE-USER
J08104                 ELSE 
J08104                     MOVE CRT-USER-NAME      TO PAH-CREATE-USER
J08104                 END-IF
J08104                 MOVE WS-SYSTEM-DATE-YMD     TO PAH-CREATE-DATE
J08104                 MOVE HHMMSS                 TO PAH-CREATE-TIME
J08104             END-IF
                   MOVE PA52F1-PCT-UPDATE-BENEFIT  TO PAH-UPDATE-BENEFIT
                   MOVE PA52F1-PCT-UPDATE-REQ-DED  TO PAH-UPDATE-REQ-DED
                   MOVE PA52F1-PCT-EDM-EFFECT-DT   TO PAH-EDM-EFFECT-DT
                   MOVE PA52F1-PCT-EDM-END-DATE    TO PAH-EDM-END-DATE
                   MOVE PA52F1-PCT-UPD-ABS-MGMT    TO PAH-UPD-ABS-MGMT
                   MOVE PA52F1-PCT-HIST-CORR-FLAG  TO PAH-HIST-CORR-FLAG
                   MOVE "1"                        TO PAH-ACTION-UPD
                   PERFORM 820-STORE-PERSACTHST
P78653             MOVE PAH-ACTION-NBR             TO PA52WS-ACTION-NBR
               ELSE
084400             MOVE "PAACT"                    TO IFOBIWS-OBJ-TYPE
084500             PERFORM 7000-ASSIGN-OBJ-ID-70
084600             MOVE IFOBIWS-OBJ-ID             TO PA52WS-OBJ-ID
084700             PERFORM 800-CREATE-PERSACTHST
084800             MOVE PA52F1-PCT-COMPANY         TO PAH-COMPANY
084900             MOVE "E"                        TO PAH-ACTION-TYPE
085000             MOVE PA52F1-PCT-ACTION-CODE     TO PAH-ACTION-CODE
085100             MOVE PA52WS-ACTION-NBR          TO PAH-ACTION-NBR
085200             MOVE PA52F1-PCT-EFFECT-DATE     TO PAH-EFFECT-DATE
085300             MOVE PA52F1-PCT-EMPLOYEE        TO PAH-EMPLOYEE
085400             MOVE PA52F1-PCT-ANT-END-DATE    TO PAH-ANT-END-DATE
085500             MOVE PA52F1-PCT-REASON (1)      TO PAH-REASON (1)
085600             MOVE PA52F1-PCT-REASON (2)      TO PAH-REASON (2)
                   IF  (PA52F1-USER-ID NOT = SPACES)
                       MOVE PA52F1-USER-ID         TO PAH-USER-ID
                   ELSE
                       MOVE CRT-USER-NAME          TO PAH-USER-ID
                   END-IF
085800             MOVE WS-SYSTEM-DATE-YMD         TO PAH-DATE-STAMP
P80029             MOVE HHMMSS                     TO PAH-TIME-STAMP
J08104             IF (PA52F1-FC = "A")
J08104                 IF  (PA52F1-USER-ID NOT = SPACES)
J08104                     MOVE PA52F1-USER-ID     TO PAH-CREATE-USER
J08104                 ELSE 
J08104                     MOVE CRT-USER-NAME      TO PAH-CREATE-USER
J08104                 END-IF
J08104                 MOVE WS-SYSTEM-DATE-YMD     TO PAH-CREATE-DATE
J08104                 MOVE HHMMSS                 TO PAH-CREATE-TIME
J08104             END-IF
085900             MOVE PA52WS-OBJ-ID              TO PAH-OBJ-ID
                   MOVE 1                          TO PAH-POS-LEVEL
J04194             IF (PA52F1-PCT-UPDATE-BENEFIT = SPACES)
J04194                 IF (PAT-DFT-UPD-BN = SPACES OR "Y")
J04194                     MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY
J04194                     PERFORM 840-KFIND-BNCSET1
J04194                     IF (BNCOMPANY-KFOUND)
J04194                         MOVE "Y" TO PA52F1-PCT-UPDATE-BENEFIT
J04194                     ELSE
J04194                         MOVE "N" TO PA52F1-PCT-UPDATE-BENEFIT
J04194                     END-IF
J04194                 ELSE
J04194                     MOVE PAT-DFT-UPD-BN 
J04194                                      TO PA52F1-PCT-UPDATE-BENEFIT
J04194                 END-IF
J04194             END-IF
J04194             IF (PA52F1-PCT-UPD-ABS-MGMT = SPACES)
J04194                 IF (PAT-DFT-UPD-LP = SPACES OR "Y")
J13588*                    MOVE PA52F1-PCT-COMPANY    TO EDCDWS-COMPANY
J13588*                    MOVE "LP"                  TO EDCDWS-SYSTEM
J13588*                    PERFORM 6000-IS-SYSTEM-TRIGGER-ENABLED
J13588*                    IF (EDCDWS-TRIGGER-ENABLED)
J04194                         MOVE "Y" TO PA52F1-PCT-UPD-ABS-MGMT
J13588*                    ELSE
J13588*                        MOVE "N" TO PA52F1-PCT-UPD-ABS-MGMT
J13588*                    END-IF
J04194                 ELSE
J04194                     MOVE PAT-DFT-UPD-LP  
J04194                                  TO PA52F1-PCT-UPD-ABS-MGMT
J04194                 END-IF
J04194             END-IF
J04194             IF (PA52F1-PCT-UPDATE-REQ-DED = SPACES)
J04194                 MOVE PAT-DFT-UPD-RQ-DED  
J04194                                      TO PA52F1-PCT-UPDATE-REQ-DED
J04194             END-IF
                   MOVE PA52F1-PCT-UPDATE-BENEFIT  TO PAH-UPDATE-BENEFIT
                   MOVE PA52F1-PCT-UPDATE-REQ-DED  TO PAH-UPDATE-REQ-DED
                   MOVE PA52F1-PCT-EDM-EFFECT-DT   TO PAH-EDM-EFFECT-DT
                   MOVE PA52F1-PCT-EDM-END-DATE    TO PAH-EDM-END-DATE
                   MOVE PA52F1-PCT-UPD-ABS-MGMT    TO PAH-UPD-ABS-MGMT
                   MOVE PA52F1-PCT-HIST-CORR-FLAG  TO PAH-HIST-CORR-FLAG
                   MOVE "1"                        TO PAH-ACTION-UPD
P80029             MOVE WS-SYSTEM-DATE-YMD         TO PAH-CREATE-DATE
P80029             MOVE HHMMSS                     TO PAH-CREATE-TIME
P80029             MOVE CRT-USER-NAME              TO PAH-CREATE-USER
090300             PERFORM 820-STORE-PERSACTHST
               END-IF
       
               IF (PA52WS-FIELDS-EXIST)
                   IF  (PA52F1-PCT-PROCESS-TYPE = "4")
                       MOVE PA52F1-MERGE-ACT-OBJ-ID
                                               TO HREMP-ACT-OBJ-ID-REV
                   END-IF
                   PERFORM 600-UPDATE-EMPLOYEE
090600             THRU    600-END
090700         END-IF
090800         PERFORM 610-CREATE-PARTICIPNT
090900         THRU    610-END.
091000
091300     PERFORM 920-AUDIT-END.
091400
      *---------------------------------+
      * Release Work Unit               |
      *---------------------------------+
           
CUSTOM**** IF (PAT-WORKFLOW-FLAG       = "Y")
J83708**** OR (PA52F1-IMMEDIATE-ACTION = "Y")   
ANGIE**    IF (PAT-WORKFLOW-FLAG       = "Y")
ANGIE      IF  (PAT-WORKFLOW-FLAG       = "N")
ACS002     AND (PA52F1-IMMEDIATE-ACTION = "Y")
               INITIALIZE CRT-MESSAGE
               INITIALIZE WFAPI-INPUT
               MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT
               INITIALIZE WFAPI-OUTPUT
AA0626*  PASS THE TERM-WORKUNIT CREATED IN 430-CREATE-WORKFLOW-TRIGGER
AA0626*  TO WFAPI-I-WORKUNIT IN ORDER TO RELEASE IT
AA0626         INITIALIZE WFAPI-I-WORKUNIT
AA0626         MOVE WS-WORKUNIT-TERM       TO WFAPI-I-WORKUNIT
               PERFORM 1000-WF-RELEASE-SETUP
           END-IF.

           MOVE EMP-LAST-NAME          TO HRWS-LAST-NAME.
           MOVE EMP-FIRST-NAME         TO HRWS-FIRST-NAME.
           MOVE EMP-MIDDLE-INIT        TO HRWS-MIDDLE-INIT.
           MOVE EMP-NAME-SUFFIX        TO HRWS-NAME-SUFFIX.
           MOVE EMP-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE.
           PERFORM 750-HR-FORMAT-NAME.
           MOVE HRWS-FORMAT-NAME       TO PA52F1-EMP-NAME.

           MOVE PRS-COMPANY            TO DB-COMPANY.
           MOVE SPACES                 TO DB-COUNTRY-CD-REQ
                                          DB-PROCESS-LEVEL.
      * 50 = Social Number
           MOVE HREMP-FICA-NBR-DN      TO DB-FLD-NBR.
           PERFORM 840-FIND-PASSET1.
           IF (PASCRTY-FOUND)
               MOVE PAS-SEC-LEVEL      TO HRWS-SEC-LEVEL
               PERFORM 730-HR-FIELD-SECURITY
               IF (HRWS-FLD-SECURED)
                   MOVE SPACES         TO PA52F1-EMP-FICA-NBR
               ELSE
                   MOVE EMP-FICA-NBR   TO PA52F1-EMP-FICA-NBR.

       
           MOVE 1 TO I8.
           PERFORM 484-MOVE-CURRENT-DATA
           THRU    484-END
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36).

J40356     IF ((PA52WS-L-LINE-FC = "X")
J40356     AND (PA52WS-LTM-SELECT-CLOSE))
J40356          MOVE PA52WS-L-COMPANY           TO DB-COMPANY
J40356          MOVE PA52WS-L-EMPLOYEE          TO DB-EMPLOYEE
J40356          MOVE PA52WS-L-STATUS            TO DB-STATUS
J40356          MOVE PA52WS-L-DATE-STAMP        TO DB-LTM-DATE-STAMP
J40356          MOVE PA52WS-L-TIME-STAMP        TO DB-LTM-TIME-STAMP
J40356          MOVE PA52WS-L-ERROR-FLAG        TO DB-ERROR-FLAG
J40356          MOVE PA52WS-L-WORKASSIGNMENT    TO DB-WORKASSIGNMENT
J40356          MOVE PA52WS-L-ACTION-CODE       TO DB-ACTION-CODE
J40356          PERFORM 910-AUDIT-BEGIN
J40356          PERFORM 840-MODIFY-LEMSET2
J40356          MOVE 9                          TO LEM-STATUS
J40356          PERFORM 820-STORE-HREMPRCVR
J40356          PERFORM 920-AUDIT-END
J40356*
J41791          MOVE CRT-ERROR-CAT              TO PA52WS-ERROR-CAT-SAVE
J40356          MOVE SPACES                     TO PA52F1-LTM-EXISTS
J40356                                             PA52F1-LTM-FLAG 
J40356          MOVE PA52F1-PCT-COMPANY         TO DB-COMPANY 
J40356          MOVE PA52F1-PCT-EMPLOYEE        TO DB-EMPLOYEE 
J40356          MOVE 1                          TO DB-STATUS 
J40356          MOVE LEMSET2-STATUS             TO WS-DB-BEG-RNG 
J40356          PERFORM 850-FIND-BEGRNG-LEMSET2 
J40356          IF (HREMPRCVR-FOUND)
J59105              MOVE 547                    TO CRT-MSG-NBR
J59105              PERFORM 790-GET-MSG
J59105              MOVE CRT-MESSAGE            TO PA52F1-LTM-EXISTS
J40356              MOVE "*"                    TO PA52F1-LTM-FLAG
J40356          END-IF 
J40356     END-IF.
J41791     MOVE PA52WS-ERROR-CAT-SAVE           TO CRT-ERROR-CAT.
J40356
J40356     SET PA52WS-LTM-SELECT-OPEN           TO TRUE.
         
           IF (PA52F1-IMMEDIATE-ACTION = "N")
               PERFORM 481-MOVE-TO-SCREEN
               THRU    481-END
               MOVE CRT-ADD-COMPLETE                TO CRT-MESSAGE
               GO TO 410-END.
       
           IF  (PA52F1-BYPASS-POPUPS = "Y")
               MOVE CRT-ADD-COMPLETE       TO CRT-MESSAGE
               GO TO 410-END.

           MOVE "PA561"                    TO CRT-SCREEN-CODE.
           MOVE SPACES                     TO PA52F1-PGM-NAME.
           MOVE SPACES                     TO CRT-PASS-FC.
           MOVE "A"                        TO CRT-DISPLAY-FC.
           MOVE "E"                        TO PA52F1-PT-ACTION-TYPE.
           MOVE PA52WS-ACTION-NBR          TO PA52F1-PCT-ACTION-NBR.
           IF (HRWS-GROUP-SIZE-ERROR)
               MOVE 260                    TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
           ELSE
           IF (PA52F1-PCT-UPDATE-REQ-DED = "Y")
               MOVE 406                    TO CRT-MSG-NBR
           ELSE
               MOVE 405                    TO CRT-MSG-NBR.
           MOVE CRT-MANUAL-CF              TO CRT-REQUEST.
      
       410-END.
      
094500******************************************************************
094600 420-CHANGE.
094700******************************************************************
094800
094900     PERFORM 910-AUDIT-BEGIN.
095000     IF (DMS-ABORTED)
095100         GO TO 420-END.
095200
           IF  (PA52F1-PCT-PROCESS-TYPE = "1")
               MOVE PA52F1-PCT-COMPANY         TO PAACT-COMPANY
               MOVE "E"                        TO PAACT-ACTION-TYPE
               MOVE PA52F1-PCT-EMPLOYEE        TO PAACT-EMPLOYEE
               MOVE PA52F1-PCT-ACTION-CODE     TO PAACT-ACTION-CODE
               MOVE PA52F1-PCT-NEW-EFFECT-DATE  TO PAACT-EFFECT-DATE
               MOVE "Y"                        TO PAACT-HISTORY
               PERFORM 2300-PAACT-ACTION-NBR
               MOVE PAACT-ACTION-NBR           TO PA52WS-ACTION-NBR
               MOVE "L"                        TO PAACT-ACTION-TYPE
               PERFORM 2300-PAACT-ACTION-NBR
               IF  (PAACT-ACTION-NBR > PA52WS-ACTION-NBR)
                   MOVE PAACT-ACTION-NBR       TO PA52WS-ACTION-NBR
               END-IF

095800         MOVE PA52F1-PCT-COMPANY         TO DB-COMPANY
095900         MOVE "E"                        TO DB-ACTION-TYPE
096000         MOVE PA52F1-PCT-EFFECT-DATE     TO DB-EFFECT-DATE
096100         MOVE PA52F1-PCT-ACTION-CODE     TO DB-ACTION-CODE
096200         MOVE PA52F1-PCT-EMPLOYEE        TO DB-EMPLOYEE
096300         MOVE PA52F1-PCT-ACTION-NBR      TO DB-ACTION-NBR
               PERFORM 840-MODIFY-PCTSET1
               PERFORM 830-DELETE-PERSACTION
               PERFORM 810-RECREATE-PERSACTION
               IF  (PA52F1-PCT-UPDATE-REQ-DED  = "Y")
               AND (PA52F1-PCT-EDM-EFFECT-DT   = PA52F1-PCT-EFFECT-DATE)
                   MOVE PA52F1-PCT-NEW-EFFECT-DATE
                                               TO PCT-EDM-EFFECT-DT
                                              PA52F1-PCT-EDM-EFFECT-DT
                   MOVE PCT-EDM-EFFECT-DT      TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   SUBTRACT 1                  FROM WSDR-JULIAN-DAYS
                   PERFORM 900-JULIAN-TO-DATE
                   MOVE WSDR-FR-DATE           TO
                                               PCT-EDM-END-DATE
                                               PA52F1-PCT-EDM-END-DATE
               END-IF
               MOVE PA52F1-PCT-NEW-EFFECT-DATE TO PCT-EFFECT-DATE
               MOVE PA52WS-ACTION-NBR          TO PCT-ACTION-NBR
               PERFORM 820-STORE-PERSACTION

               MOVE PA52F1-PCT-COMPANY         TO DB-COMPANY
               MOVE 0                          TO DB-EMP-APP
               MOVE "PA"                       TO DB-CMT-TYPE
               MOVE PA52F1-PCT-ACTION-CODE     TO DB-ACTION-CODE
               MOVE PA52F1-PCT-EFFECT-DATE     TO DB-DATE
               MOVE PA52F1-PCT-EMPLOYEE        TO DB-EMPLOYEE
               INITIALIZE                         DB-JOB-CODE
               MOVE PA52F1-PCT-ACTION-NBR      TO DB-LN-NBR
               MOVE PACSET2-LN-NBR             TO WS-DB-BEG-RNG
               PERFORM 850-MODIFY-BEGRNG-PACSET2
               PERFORM
                   UNTIL (PACOMMENTS-NOTFOUND)
                   PERFORM 830-DELETE-PACOMMENTS
                   PERFORM 810-RECREATE-PACOMMENTS
                   MOVE PA52F1-PCT-NEW-EFFECT-DATE  TO PAC-DATE
                   MOVE PA52WS-ACTION-NBR          TO PAC-LN-NBR
                   PERFORM 820-STORE-PACOMMENTS
                   PERFORM 860-MODIFY-NXTRNG-PACSET2
               END-PERFORM
               INITIALIZE PA52F1-PCT-NEW-EFFECT-DATE
                          PA52F1-PCT-PROCESS-TYPE
           ELSE
095800         MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY
095900         MOVE "E"                    TO DB-ACTION-TYPE
096000         MOVE PA52F1-PCT-EFFECT-DATE TO DB-EFFECT-DATE
096100         MOVE PA52F1-PCT-ACTION-CODE TO DB-ACTION-CODE
096200         MOVE PA52F1-PCT-EMPLOYEE    TO DB-EMPLOYEE
096300         MOVE PA52F1-PCT-ACTION-NBR  TO DB-ACTION-NBR
096500         PERFORM 840-MODIFY-PCTSET1
               PERFORM 8400-AFTER-FIND-PCT
096600         PERFORM 500-MOVE-DATA
096700         THRU    500-END
096800         PERFORM 8200-STORE-PERSACTION
           END-IF.
096900
097000     PERFORM 920-AUDIT-END.
097100
097200     MOVE EMP-LAST-NAME          TO HRWS-LAST-NAME.
097300     MOVE EMP-FIRST-NAME         TO HRWS-FIRST-NAME.
097400     MOVE EMP-MIDDLE-INIT        TO HRWS-MIDDLE-INIT.
           MOVE EMP-NAME-SUFFIX        TO HRWS-NAME-SUFFIX.
           MOVE EMP-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE.
097500     PERFORM 750-HR-FORMAT-NAME.
097600     MOVE HRWS-FORMAT-NAME       TO PA52F1-EMP-NAME.

           MOVE PRS-COMPANY            TO DB-COMPANY.
           MOVE SPACES                 TO DB-COUNTRY-CD-REQ
                                          DB-PROCESS-LEVEL.
      * 50 = Social Number
           MOVE HREMP-FICA-NBR-DN      TO DB-FLD-NBR.
           PERFORM 840-FIND-PASSET1.
           IF (PASCRTY-FOUND)
               MOVE PAS-SEC-LEVEL      TO HRWS-SEC-LEVEL
               PERFORM 730-HR-FIELD-SECURITY
               IF (HRWS-FLD-SECURED)
                   MOVE SPACES         TO PA52F1-EMP-FICA-NBR
               ELSE
                   MOVE EMP-FICA-NBR   TO PA52F1-EMP-FICA-NBR.

       
           MOVE 1 TO I8.
           PERFORM 484-MOVE-CURRENT-DATA
           THRU    484-END
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36).
      
           PERFORM 481-MOVE-TO-SCREEN
           THRU    481-END.

J40356     IF ((PA52WS-L-LINE-FC = "X")
J40356     AND (PA52WS-LTM-SELECT-CLOSE))
J40356          MOVE PA52WS-L-COMPANY           TO DB-COMPANY
J40356          MOVE PA52WS-L-EMPLOYEE          TO DB-EMPLOYEE
J40356          MOVE PA52WS-L-STATUS            TO DB-STATUS
J40356          MOVE PA52WS-L-DATE-STAMP        TO DB-LTM-DATE-STAMP
J40356          MOVE PA52WS-L-TIME-STAMP        TO DB-LTM-TIME-STAMP
J40356          MOVE PA52WS-L-ERROR-FLAG        TO DB-ERROR-FLAG
J40356          MOVE PA52WS-L-WORKASSIGNMENT    TO DB-WORKASSIGNMENT
J40356          MOVE PA52WS-L-ACTION-CODE       TO DB-ACTION-CODE
J40356          PERFORM 910-AUDIT-BEGIN 
J40356          PERFORM 840-MODIFY-LEMSET2
J40356          MOVE 9                          TO LEM-STATUS
J40356          PERFORM 820-STORE-HREMPRCVR
J40356          PERFORM 920-AUDIT-END   
J40356*
J41791          MOVE CRT-ERROR-CAT              TO PA52WS-ERROR-CAT-SAVE
J40356          MOVE SPACES                     TO PA52F1-LTM-EXISTS
J40356                                             PA52F1-LTM-FLAG
J40356          MOVE PA52F1-PCT-COMPANY         TO DB-COMPANY
J40356          MOVE PA52F1-PCT-EMPLOYEE        TO DB-EMPLOYEE
J40356          MOVE 1                          TO DB-STATUS
J40356          MOVE LEMSET2-STATUS             TO WS-DB-BEG-RNG
J40356          PERFORM 850-FIND-BEGRNG-LEMSET2
J40356          IF (HREMPRCVR-FOUND)
J59105              MOVE 547                    TO CRT-MSG-NBR
J59105              PERFORM 790-GET-MSG
J59105              MOVE CRT-MESSAGE            TO PA52F1-LTM-EXISTS
J40356              MOVE "*"                    TO PA52F1-LTM-FLAG
J40356          END-IF
J40356     END-IF.
J41791     MOVE PA52WS-ERROR-CAT-SAVE           TO CRT-ERROR-CAT.
J40356
J40356     SET PA52WS-LTM-SELECT-OPEN           TO TRUE.
098700     MOVE CRT-CHG-COMPLETE                TO CRT-MESSAGE.
098800
098900 420-END.
099000
099100******************************************************************
       429-CREATE-WORKFLOW-TRIGGER.
099100******************************************************************

      **** CHECK TO SEE IF PROCESS LEVEL CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-PROCESS-LEVEL-DN TO PA52WS-FLD-NBR.
           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.
           IF (FIELD-USED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO WFAPI-I-CRITERION-2
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO WFAPI-I-CRITERION-2
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO WFAPI-I-CRITERION-2.

           IF (WFAPI-I-CRITERION-2     = PA52WS-BLANK)
               INITIALIZE WFAPI-I-CRITERION-2
           ELSE
           IF (WFAPI-I-CRITERION-2     = SPACES)
               MOVE EMP-PROCESS-LEVEL  TO WFAPI-I-CRITERION-2.

      **** CHECK TO SEE IF DEPARTMENT CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-DEPARTMENT-DN    TO PA52WS-FLD-NBR.
           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.
           IF (FIELD-USED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO WFAPI-I-CRITERION-3
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO WFAPI-I-CRITERION-3
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO WFAPI-I-CRITERION-3.

           IF (WFAPI-I-CRITERION-3     = PA52WS-BLANK)
               INITIALIZE WFAPI-I-CRITERION-3
           ELSE
           IF (WFAPI-I-CRITERION-3     = SPACES)
               MOVE EMP-DEPARTMENT     TO WFAPI-I-CRITERION-3.

      **** COMPANY
CRP001     MOVE PA52F1-PCT-COMPANY      TO HRCRP-COMPANY.    

      **** CHECK TO SEE IF PROCESS LEVEL CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-PROCESS-LEVEL-DN TO PA52WS-FLD-NBR.
           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.

      **** OLD PROCESS LEVEL
CRP001     MOVE EMP-PROCESS-LEVEL       TO HRCRP-OLD-PROC-LEVEL.       

      **** NEW PROCESS LEVEL
           IF (FIELD-USED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO WFAPI-I-VARIABLE-VAL (3)
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO WFAPI-I-VARIABLE-VAL (3)
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO WFAPI-I-VARIABLE-VAL (3).

           IF (WFAPI-I-VARIABLE-VAL (3) = PA52WS-BLANK)
               INITIALIZE WFAPI-I-VARIABLE-VAL (3)
           ELSE
           IF (WFAPI-I-VARIABLE-VAL (3) = SPACES)
               MOVE EMP-PROCESS-LEVEL         
                                       TO WFAPI-I-VARIABLE-VAL (3).
CRP001     MOVE WFAPI-I-VARIABLE-VAL(3) TO HRCRP-NEW-PROC-LEVEL.       

      **** CHECK TO SEE IF DEPARTMENT CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-DEPARTMENT-DN    TO PA52WS-FLD-NBR.
           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.

      **** OLD DEPARTMENT
CRP001     MOVE EMP-DEPARTMENT          TO HRCRP-OLD-DEPARTMENT.       

      **** NEW DEPARTMENT
           IF (FIELD-USED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO WFAPI-I-VARIABLE-VAL (5)
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO WFAPI-I-VARIABLE-VAL (5)
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO WFAPI-I-VARIABLE-VAL (5).

           IF (WFAPI-I-VARIABLE-VAL (5) = PA52WS-BLANK)
               INITIALIZE WFAPI-I-VARIABLE-VAL (5)
           ELSE
           IF (WFAPI-I-VARIABLE-VAL (5) = SPACES)
               MOVE EMP-DEPARTMENT             
                                       TO WFAPI-I-VARIABLE-VAL (5).
CRP001     MOVE WFAPI-I-VARIABLE-VAL(5) TO HRCRP-NEW-DEPARTMENT.       

      **** CHECK TO SEE IF SUPERVISOR CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-SUPERVISOR-DN    TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.

      **** FIND OLD SUPERVISOR AND NEXT OLD SUPERVISOR ****
           INITIALIZE PA52WS-OLD-SUPER-EMP
                      PA52WS-NEXT-OLD-SUPER-EMP.
           IF (EMP-SUPERVISOR            NOT = SPACES)
               MOVE EMP-SUPERVISOR       TO DB-CODE
               PERFORM 840-FIND-HSUSET1
               IF (HSU-EMPLOYEE          = ZEROES)
                   PERFORM 439-FIND-SUPER-EMP
                   THRU    439-END
               END-IF
               IF  (HRSUPER-FOUND)
               AND (HSU-EMPLOYEE         NOT = ZEROES)
                   MOVE HSU-EMPLOYEE     TO PA52WS-OLD-SUPER-EMP
               END-IF
               IF (PA52WS-OLD-SUPER-EMP  NOT = ZEROES)
                   MOVE HSU-SUPER-RPTS-TO TO DB-CODE
                   PERFORM 840-FIND-HSUSET1
                   IF (HRSUPER-FOUND)
                       IF (HSU-EMPLOYEE      = ZEROES)
                           PERFORM 439-FIND-SUPER-EMP
                           THRU    439-END
                       END-IF
                       IF (HSU-EMPLOYEE     NOT = ZEROES)
                           MOVE HSU-EMPLOYEE 
                                          TO PA52WS-NEXT-OLD-SUPER-EMP.

      **** OLD SUPERVISOR EMPLOYEE
           MOVE PA52WS-OLD-SUPER-EMP   TO WFAPI-I-VARIABLE-VAL (6).
CRP001     MOVE WFAPI-I-VARIABLE-VAL(6) TO HRCRP-OLD-SUPER-EMP.        

      **** NEXT OLD SUPERVISOR EMPLOYEE
           MOVE 523                    TO CRT-MSG-NBR.
           MOVE PA52WS-NEXT-OLD-SUPER-EMP TO WFAPI-I-VARIABLE-VAL (7).
CRP001     MOVE WFAPI-I-VARIABLE-VAL(7) TO HRCRP-NXT-OLD-SUPER-EMP.    

      **** FIND NEW SUPERVISOR AND NEXT NEW SUPERVISOR ****
           INITIALIZE PA52WS-NEW-SUPER-EMP
                      PA52WS-NEXT-NEW-SUPER-EMP.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)      TO DB-CODE
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12) TO DB-CODE
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24) TO DB-CODE
               END-IF
               END-IF
               END-IF
               IF (DB-CODE NOT = SPACES AND "*BLANK")
054800             PERFORM 840-FIND-HSUSET1
                   IF (HRSUPER-FOUND)
                       IF (HSU-EMPLOYEE          = ZEROES)
                           PERFORM 439-FIND-SUPER-EMP
                           THRU    439-END
                       END-IF
                       IF (HSU-EMPLOYEE         NOT = ZEROES)
                           MOVE HSU-EMPLOYEE     TO PA52WS-NEW-SUPER-EMP
                       END-IF
                   END-IF
                   IF (PA52WS-NEW-SUPER-EMP  NOT = ZEROES)
                       MOVE HSU-SUPER-RPTS-TO TO DB-CODE
                       PERFORM 840-FIND-HSUSET1
                       IF (HRSUPER-FOUND)
                           IF (HSU-EMPLOYEE      = ZEROES)
                               PERFORM 439-FIND-SUPER-EMP
                               THRU    439-END
                           END-IF
                           IF (HSU-EMPLOYEE     NOT = ZEROES)
                               MOVE HSU-EMPLOYEE
                                           TO PA52WS-NEXT-NEW-SUPER-EMP.

           IF  (PA52WS-NEW-SUPER-EMP      = ZEROES)
           AND (DB-CODE               NOT = "*BLANK")
               MOVE PA52WS-OLD-SUPER-EMP TO PA52WS-NEW-SUPER-EMP.

           IF  (PA52WS-NEXT-NEW-SUPER-EMP = ZEROES)
           AND (DB-CODE               NOT = "*BLANK")
               MOVE PA52WS-NEXT-OLD-SUPER-EMP
                                         TO PA52WS-NEXT-NEW-SUPER-EMP.

      **** NEW SUPERVISOR EMPLOYEE
           MOVE PA52WS-NEW-SUPER-EMP   TO WFAPI-I-VARIABLE-VAL (8).
CRP001     MOVE WFAPI-I-VARIABLE-VAL(8) TO HRCRP-NEW-SUPER-EMP.    

      **** NEXT NEW SUPERVISOR EMPLOYEE
           MOVE PA52WS-NEXT-NEW-SUPER-EMP TO WFAPI-I-VARIABLE-VAL (9).
CRP001     MOVE WFAPI-I-VARIABLE-VAL(9) TO HRCRP-NXT-NEW-SUPER-EMP.    

           MOVE PA52F1-PCT-EMPLOYEE    TO DB-EMPLOYEE.
           PERFORM 840-FIND-EMPSET1.

      **** CHECK TO SEE IF ANY SALARY RELATED FIELDS ARE CHANGED ****
           INITIALIZE PA52WS-PAY-RATE
                      PA52WS-SALARY-CLASS
                      PA52WS-NBR-FTE
                      PA52WS-SCHEDULE
                      PA52WS-PAY-STEP
                      PA52WS-PAY-GRADE
                      PA52WS-ANNUAL-HOURS
MG0301*               PA52WS-JOB-CODE.
MG0301                PA52WS-JOB-CODE
MG0301                PA52WS-ZIP.

      **** CHECK ANNUAL-HOURS ****
           MOVE HREMP-ANNUAL-HOURS-DN  TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-ALPHA
               END-IF
               END-IF
               END-IF
               PERFORM 430A-CONVERT-NUMBER
               THRU    430A-END
               MOVE PA52WS-NUMBER      TO PA52WS-ANNUAL-HOURS.

      **** CHECK PAY RATE ****
           MOVE HREMP-PAY-RATE-DN      TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-ALPHA
               END-IF
               END-IF
               END-IF
               PERFORM 430A-CONVERT-NUMBER
               THRU    430A-END
               MOVE PA52WS-NUMBER      TO PA52WS-PAY-RATE.

      **** CHECK SALARY CLASS ****
           MOVE HREMP-SALARY-CLASS-DN  TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-SALARY-CLASS
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-SALARY-CLASS
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-SALARY-CLASS.

      **** CHECK NBR FTE ****
           MOVE HREMP-NBR-FTE-DN       TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-ALPHA
               END-IF
               END-IF
               END-IF
               PERFORM 430A-CONVERT-NUMBER
               THRU    430A-END
               MOVE PA52WS-NUMBER      TO PA52WS-NBR-FTE.

      **** CHECK SCHEDULE ****
           MOVE HREMP-SCHEDULE-DN      TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-SCHEDULE
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-SCHEDULE
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-SCHEDULE.

      **** CHECK PAY STEP ****
           MOVE HREMP-PAY-STEP-DN      TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-ALPHA
               END-IF
               END-IF
               END-IF
               PERFORM 430A-CONVERT-NUMBER
               THRU    430A-END
               MOVE PA52WS-NUMBER      TO PA52WS-PAY-STEP.

      **** CHECK PAY GRADE ****
           MOVE HREMP-PAY-GRADE-DN     TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-PAY-GRADE
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-PAY-GRADE
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-PAY-GRADE.

      **** CHECK JOB CODE
           MOVE HREMP-JOB-CODE-DN      TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-JOB-CODE
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-JOB-CODE
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-JOB-CODE.

MG0301**** CHECK ZIP CODE
MG0301     MOVE HREMP-ZIP-DN           TO PA52WS-FLD-NBR.
MG0301     PERFORM 431-CHECK-IF-CHANGED
MG0301     THRU    431-END.
MG0301     IF (FIELD-CHANGED)
MG0301         IF (I1 >= 1) AND (I1 <= 12)
MG0301             MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
MG0301                                 TO PA52WS-ZIP
MG0301         ELSE
MG0301         IF (I1 >= 13) AND (I1 <= 24)
MG0301             MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
MG0301                                 TO PA52WS-ZIP
MG0301         ELSE
MG0301         IF (I1 >= 25) AND (I1 <= 36)
MG0301             MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
MG0301                                 TO PA52WS-ZIP.
MG0301
           IF (PA52WS-PAY-RATE         NOT = ZEROES)
           OR (PA52WS-SALARY-CLASS     NOT = SPACES)
           OR (PA52WS-NBR-FTE          NOT = ZEROES)
           OR (PA52WS-SCHEDULE         NOT = SPACES)
           OR (PA52WS-PAY-STEP         NOT = ZEROES)
           OR (PA52WS-PAY-GRADE        NOT = SPACES)
           OR (PA52WS-JOB-CODE         NOT = SPACES)
           OR (PA52WS-ANNUAL-HOURS     NOT = ZEROES)
MG0301     OR (PA52WS-ZIP              NOT = SPACES)           
CRP001*--- Force new salary to always recalc so it is sent in WU variable
CRP001     OR (1 < 2)
               SET FIELD-CHANGED       TO TRUE
               IF (PA52WS-ANNUAL-HOURS = ZEROES)
                   MOVE EMP-ANNUAL-HOURS   TO PA52WS-ANNUAL-HOURS
               END-IF
               IF (PA52WS-PAY-RATE     = ZEROES)
                   MOVE EMP-PAY-RATE   TO PA52WS-PAY-RATE
               END-IF
               IF (PA52WS-SALARY-CLASS = SPACES)
                   MOVE EMP-SALARY-CLASS   TO PA52WS-SALARY-CLASS
               END-IF
               IF (PA52WS-NBR-FTE      = ZEROES)
                   MOVE EMP-NBR-FTE    TO PA52WS-NBR-FTE
               END-IF
               IF (PA52WS-SCHEDULE     = SPACES)
                   MOVE EMP-SCHEDULE   TO PA52WS-SCHEDULE
               END-IF
               IF (PA52WS-PAY-STEP     = ZEROES)
                   MOVE EMP-PAY-STEP   TO PA52WS-PAY-STEP
               END-IF
               IF (PA52WS-PAY-GRADE    = SPACES)
                   MOVE EMP-PAY-GRADE  TO PA52WS-PAY-GRADE
               END-IF
               IF (PA52WS-JOB-CODE     = SPACES)
ACS001*            MOVE EMP-JOB-CODE   TO PA52WS-JOB-CODE.
MG0301             MOVE EMP-JOB-CODE   TO PA52WS-JOB-CODE
MG0301         END-IF
MG0301         IF (PA52WS-ZIP          = SPACES)
MG0301             MOVE EMP-ZIP        TO PA52WS-ZIP.

      **** OLD SALARY
           PERFORM 433-CALC-OLD-SALARY
           THRU    433-END.
           MOVE PA52WS-OLD-SALARY      TO PA52WS-NUM-15.
           MOVE PA52WS-ALPHA-15        TO WFAPI-I-VARIABLE-VAL (10).
CRP001     MOVE WFAPI-I-VARIABLE-VAL(10) TO HRCRP-OLD-SALARY.  

           INITIALIZE WFAPI-OUTPUT.         
           INITIALIZE WFAPI-INPUT.         

      **** NEW SALARY
           PERFORM 436-CALC-NEW-SALARY
           THRU    436-END.
           MOVE PA52WS-NEW-SALARY      TO PA52WS-NUM-15.
           MOVE PA52WS-ALPHA-15        TO WFAPI-I-VARIABLE-VAL (1).
CRP001     MOVE WFAPI-I-VARIABLE-VAL(1) TO HRCRP-NEW-SALARY.           

      **** ACTION CODE
           MOVE PA52F1-PCT-ACTION-CODE TO WFAPI-I-VARIABLE-VAL (2).
CRP001     MOVE WFAPI-I-VARIABLE-VAL(2) TO HRCRP-ACTION-CODE.          

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 10)

               IF (WFAPI-I-VARIABLE-VAL (I1) = PA52WS-BLANK)
                   INITIALIZE WFAPI-I-VARIABLE-VAL (I1)
               END-IF
           END-PERFORM.

      **** REASON (1)
           MOVE PA52F1-PCT-REASON (1)  TO WFAPI-I-VARIABLE-VAL (3).
CRP001     MOVE WFAPI-I-VARIABLE-VAL(3) TO HRCRP-REASON-1.             

      **** REASON (2)
           MOVE PA52F1-PCT-REASON (2)  TO WFAPI-I-VARIABLE-VAL (4).
CRP001     MOVE WFAPI-I-VARIABLE-VAL(4) TO HRCRP-REASON-2.             

      **** CHECK TO SEE IF EMPLOYEE STATUS CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-EMP-STATUS-DN    TO PA52WS-FLD-NBR.
           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.

      **** OLD EMPLOYEE STATUS
           MOVE EMP-EMP-STATUS         TO WFAPI-I-VARIABLE-VAL (5).
CRP001     MOVE WFAPI-I-VARIABLE-VAL(5) TO HRCRP-OLD-EMP-STATUS.       

      **** NEW EMPLOYEE STATUS
           IF (FIELD-USED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO WFAPI-I-VARIABLE-VAL (6)
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO WFAPI-I-VARIABLE-VAL (6)
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO WFAPI-I-VARIABLE-VAL (6).

           IF (WFAPI-I-VARIABLE-VAL (6) = PA52WS-BLANK)
               INITIALIZE WFAPI-I-VARIABLE-VAL (6)
           ELSE
           IF (WFAPI-I-VARIABLE-VAL (6) = SPACES)
               MOVE WFAPI-I-VARIABLE-VAL (5)
                                       TO WFAPI-I-VARIABLE-VAL (6).
CRP001     MOVE WFAPI-I-VARIABLE-VAL(6) TO HRCRP-NEW-EMP-STATUS.       

      **** OVER BUDGET
           IF (HREMP-VAR-FTE           < ZEROES)
           OR (HREMP-VAR-HDCNT         < ZEROES)
               MOVE "Y"                TO WFAPI-I-VARIABLE-VAL (7)
           ELSE
               MOVE "N"                TO WFAPI-I-VARIABLE-VAL (7).

      **** PERCENT CHANGE
           COMPUTE PA52WS-PCT-CHANGE ROUNDED
                                       = (PA52WS-NEW-SALARY
                                       /  PA52WS-OLD-SALARY
                                       *  100)
                                       - 100.
           MOVE PA52WS-PCT-CHANGE      TO PA52WS-NUM-15.
           MOVE PA52WS-ALPHA-15        TO WFAPI-I-VARIABLE-VAL (8).

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 6)

               IF (WFAPI-I-VARIABLE-VAL (I1) = PA52WS-BLANK)
                   INITIALIZE WFAPI-I-VARIABLE-VAL (I1)
               END-IF
           END-PERFORM.

      **** ACTION TYPE
           MOVE PA52F1-PT-ACTION-TYPE  TO WFAPI-I-VARIABLE-VAL (9).

      **** EFFECT DATE
           MOVE PA52F1-PCT-EFFECT-DATE TO WFAPI-I-VARIABLE-VAL (10).
CRP001     MOVE WFAPI-I-VARIABLE-VAL(10) TO HRCRP-EFFECT-DATE.          

           INITIALIZE WFAPI-OUTPUT.         
           INITIALIZE WFAPI-INPUT.         

      **** EMPLOYEE
           MOVE PA52F1-PCT-EMPLOYEE    TO WFAPI-I-VARIABLE-VAL (1).
CRP001     MOVE PA52F1-PCT-EMPLOYEE    TO HRCRP-EMPLOYEE.             

      **** ACTION NBR
           MOVE PA52WS-ACTION-NBR      TO WFAPI-I-VARIABLE-VAL (2).

      **** MAIL-SUBJECT VARIABLE IS SET SO THAT PEOPLE CAN GET E-MAIL
025320     MOVE WFAPI-MAIL-SUBJECT     TO WFAPI-I-VARIABLE-NAME (3).

MG0301**** IMMEDIATE ACTION
MG0301     MOVE PA52F1-IMMEDIATE-ACTION TO WFAPI-I-VARIABLE-VAL  (4).
MG0301
MG0301**** OLD EMPLOYEE ZIP
MG0301     MOVE EMP-ZIP                TO WFAPI-I-VARIABLE-VAL (5).
MG0301
MG0301**** NEW EMPLOYEE ZIP
MG0301     MOVE PA52WS-ZIP             TO WFAPI-I-VARIABLE-VAL (6).
MG0301

       429-END.

099100******************************************************************
       430-CREATE-WORKFLOW-TRIGGER.
099100******************************************************************

      *---------------------------------+
      * Check Status                    |
      *---------------------------------+
      **** Check to see if workflow is enabled for a service
           INITIALIZE WFAPI-INPUT
                      WFAPI-OUTPUT.

      **** Check to see if Action Approval Service Definition is setup
      **** in WorkFlow System
           MOVE PA52F1-PCT-ACTION-CODE TO WFAPI-I-SERVICE.

           MOVE PA52F1-PCT-COMPANY     TO WFAPI-I-CRITERION-1.
      **** CHECK TO SEE IF PROCESS LEVEL CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-PROCESS-LEVEL-DN TO PA52WS-FLD-NBR.
           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.
           IF (FIELD-USED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO WFAPI-I-CRITERION-2
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO WFAPI-I-CRITERION-2
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO WFAPI-I-CRITERION-2.

           IF (WFAPI-I-CRITERION-2     = PA52WS-BLANK)
               INITIALIZE WFAPI-I-CRITERION-2
           ELSE
           IF (WFAPI-I-CRITERION-2     = SPACES)
               MOVE EMP-PROCESS-LEVEL  TO WFAPI-I-CRITERION-2.

      **** CHECK TO SEE IF DEPARTMENT CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-DEPARTMENT-DN    TO PA52WS-FLD-NBR.
           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.
           IF (FIELD-USED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO WFAPI-I-CRITERION-3
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO WFAPI-I-CRITERION-3
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO WFAPI-I-CRITERION-3.

           IF (WFAPI-I-CRITERION-3     = PA52WS-BLANK)
               INITIALIZE WFAPI-I-CRITERION-3
           ELSE
           IF (WFAPI-I-CRITERION-3     = SPACES)
               MOVE EMP-DEPARTMENT     TO WFAPI-I-CRITERION-3.

           PERFORM 1000-WF-SERVICE.
           IF (WFAPI-O-RETURN-CODE     NOT = ZEROES)
               GO TO 430-END.

      *---------------------------------+
      * Create Work Unit Header         |
      *---------------------------------+
      **** This creates data in WF20 and matches with fields in WF04
      **** Perform CREATE WORK Routine
           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-SERVICE        TO WFAPI-I-SERVICE.
           MOVE WFAPI-O-AGENT          TO WFAPI-I-AGENT.
           MOVE WFAPI-O-PROCEDURE      TO WFAPI-I-PROCEDURE.
           MOVE PA52WS-WORK-PRIORITY   TO WFAPI-I-WORK-PRIORITY.
           MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY.
           MOVE PA52F1-PCT-ACTION-CODE TO DB-ACTION-CODE.
           PERFORM 840-FIND-PATSET1.
           MOVE PAT-DESCRIPTION        TO WFAPI-I-WORK-TITLE.
           MOVE PA52WS-PCTSET1         TO WFAPI-I-OBJECT-NAME.
           MOVE PA52F1-PCT-COMPANY     TO WFAPI-I-KEY-VALUE (1).
           MOVE PA52F1-PT-ACTION-TYPE  TO WFAPI-I-KEY-VALUE (2).
           MOVE PA52F1-PCT-EFFECT-DATE TO WFAPI-I-KEY-VALUE (3).
           MOVE PA52F1-PCT-ACTION-CODE TO WFAPI-I-KEY-VALUE (4).
           MOVE PA52F1-PCT-EMPLOYEE    TO WFAPI-I-KEY-VALUE (5).
           MOVE PA52WS-ACTION-NBR      TO WFAPI-I-KEY-VALUE (6).

           MOVE 525                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-WORK-CATEGORY.

           MOVE PA52F1-PCT-COMPANY     TO PA52WS-COMPANY.
           MOVE EMP-SUPERVISOR         TO PA52WS-SUPERVISOR.
           MOVE PA52WS-COMP-CODE       TO WFAPI-I-WORK-CAT-VALUE.

           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-CREATE-SETUP.

      *---------------------------------+
      * Create Work Unit Variable       |
      *---------------------------------+
      **** Perform ADD VARIABLE Routine
           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.

      **** COMPANY
           MOVE 501                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (1).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (1).
           MOVE PA52F1-PCT-COMPANY     TO WFAPI-I-VARIABLE-VAL (1).

      **** CHECK TO SEE IF PROCESS LEVEL CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-PROCESS-LEVEL-DN TO PA52WS-FLD-NBR.
           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.

      **** OLD PROCESS LEVEL
           MOVE 502                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (2).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (2).
           MOVE EMP-PROCESS-LEVEL      TO WFAPI-I-VARIABLE-VAL (2).

      **** NEW PROCESS LEVEL
           MOVE 503                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (3).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (3).
           IF (FIELD-USED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO WFAPI-I-VARIABLE-VAL (3)
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO WFAPI-I-VARIABLE-VAL (3)
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO WFAPI-I-VARIABLE-VAL (3).

           IF (WFAPI-I-VARIABLE-VAL (3) = PA52WS-BLANK)
               INITIALIZE WFAPI-I-VARIABLE-VAL (3)
           ELSE
           IF (WFAPI-I-VARIABLE-VAL (3) = SPACES)
               MOVE WFAPI-I-VARIABLE-VAL (2)
                                       TO WFAPI-I-VARIABLE-VAL (3).

      **** CHECK TO SEE IF DEPARTMENT CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-DEPARTMENT-DN    TO PA52WS-FLD-NBR.
           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.

      **** OLD DEPARTMENT
           MOVE 504                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (4).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (4).
           MOVE EMP-DEPARTMENT         TO WFAPI-I-VARIABLE-VAL (4).

      **** NEW DEPARTMENT
           MOVE 505                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (5).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (5).
           IF (FIELD-USED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO WFAPI-I-VARIABLE-VAL (5)
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO WFAPI-I-VARIABLE-VAL (5)
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO WFAPI-I-VARIABLE-VAL (5).

           IF (WFAPI-I-VARIABLE-VAL (5) = PA52WS-BLANK)
               INITIALIZE WFAPI-I-VARIABLE-VAL (5)
           ELSE
           IF (WFAPI-I-VARIABLE-VAL (5) = SPACES)
               MOVE WFAPI-I-VARIABLE-VAL (4)
                                       TO WFAPI-I-VARIABLE-VAL (5).

      **** CHECK TO SEE IF SUPERVISOR CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-SUPERVISOR-DN    TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.

      **** FIND OLD SUPERVISOR AND NEXT OLD SUPERVISOR ****
           INITIALIZE PA52WS-OLD-SUPER-EMP
                      PA52WS-NEXT-OLD-SUPER-EMP.
           IF (EMP-SUPERVISOR            NOT = SPACES)
               MOVE EMP-SUPERVISOR       TO DB-CODE
               PERFORM 840-FIND-HSUSET1
               IF (HSU-EMPLOYEE          = ZEROES)
                   PERFORM 439-FIND-SUPER-EMP
                   THRU    439-END
               END-IF
               IF  (HRSUPER-FOUND)
               AND (HSU-EMPLOYEE         NOT = ZEROES)
                   MOVE HSU-EMPLOYEE     TO PA52WS-OLD-SUPER-EMP
               END-IF
               IF (PA52WS-OLD-SUPER-EMP  NOT = ZEROES)
                   MOVE HSU-SUPER-RPTS-TO TO DB-CODE
                   PERFORM 840-FIND-HSUSET1
                   IF (HRSUPER-FOUND)
                       IF (HSU-EMPLOYEE      = ZEROES)
                           PERFORM 439-FIND-SUPER-EMP
                           THRU    439-END
                       END-IF
                       IF (HSU-EMPLOYEE     NOT = ZEROES)
                           MOVE HSU-EMPLOYEE 
                                          TO PA52WS-NEXT-OLD-SUPER-EMP.

      **** OLD SUPERVISOR EMPLOYEE
           MOVE 506                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (6).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (6).
           MOVE PA52WS-OLD-SUPER-EMP   TO WFAPI-I-VARIABLE-VAL (6).

      **** NEXT OLD SUPERVISOR EMPLOYEE
           MOVE 523                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (7).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (7).
           MOVE PA52WS-NEXT-OLD-SUPER-EMP TO WFAPI-I-VARIABLE-VAL (7).

      **** FIND NEW SUPERVISOR AND NEXT NEW SUPERVISOR ****
           INITIALIZE PA52WS-NEW-SUPER-EMP
                      PA52WS-NEXT-NEW-SUPER-EMP.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)      TO DB-CODE
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12) TO DB-CODE
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24) TO DB-CODE
               END-IF
               END-IF
               END-IF
               IF (DB-CODE NOT = SPACES AND "*BLANK")
054800             PERFORM 840-FIND-HSUSET1
                   IF (HRSUPER-FOUND)
                       IF (HSU-EMPLOYEE          = ZEROES)
                           PERFORM 439-FIND-SUPER-EMP
                           THRU    439-END
                       END-IF
                       IF (HSU-EMPLOYEE         NOT = ZEROES)
                           MOVE HSU-EMPLOYEE     TO PA52WS-NEW-SUPER-EMP
                       END-IF
                   END-IF
                   IF (PA52WS-NEW-SUPER-EMP  NOT = ZEROES)
                       MOVE HSU-SUPER-RPTS-TO TO DB-CODE
                       PERFORM 840-FIND-HSUSET1
                       IF (HRSUPER-FOUND)
                           IF (HSU-EMPLOYEE      = ZEROES)
                               PERFORM 439-FIND-SUPER-EMP
                               THRU    439-END
                           END-IF
                           IF (HSU-EMPLOYEE     NOT = ZEROES)
                               MOVE HSU-EMPLOYEE
                                           TO PA52WS-NEXT-NEW-SUPER-EMP.

           IF  (PA52WS-NEW-SUPER-EMP      = ZEROES)
           AND (DB-CODE               NOT = "*BLANK")
               MOVE PA52WS-OLD-SUPER-EMP TO PA52WS-NEW-SUPER-EMP.

           IF  (PA52WS-NEXT-NEW-SUPER-EMP = ZEROES)
           AND (DB-CODE               NOT = "*BLANK")
               MOVE PA52WS-NEXT-OLD-SUPER-EMP
                                         TO PA52WS-NEXT-NEW-SUPER-EMP.

      **** NEW SUPERVISOR EMPLOYEE
           MOVE 507                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (8).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (8).
           MOVE PA52WS-NEW-SUPER-EMP   TO WFAPI-I-VARIABLE-VAL (8).

      **** NEXT NEW SUPERVISOR EMPLOYEE
           MOVE 524                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (9).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (9).
           MOVE PA52WS-NEXT-NEW-SUPER-EMP TO WFAPI-I-VARIABLE-VAL (9).

           MOVE PA52F1-PCT-EMPLOYEE    TO DB-EMPLOYEE.
           PERFORM 840-FIND-EMPSET1.

      **** CHECK TO SEE IF ANY SALARY RELATED FIELDS ARE CHANGED ****
           INITIALIZE PA52WS-PAY-RATE
                      PA52WS-SALARY-CLASS
                      PA52WS-NBR-FTE
                      PA52WS-SCHEDULE
                      PA52WS-PAY-STEP
                      PA52WS-PAY-GRADE
                      PA52WS-ANNUAL-HOURS
MG0301*               PA52WS-JOB-CODE.
MG0301                PA52WS-JOB-CODE
MG0301                PA52WS-ZIP.

      **** CHECK ANNUAL-HOURS ****
           MOVE HREMP-ANNUAL-HOURS-DN  TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-ALPHA
               END-IF
               END-IF
               END-IF
               PERFORM 430A-CONVERT-NUMBER
               THRU    430A-END
               MOVE PA52WS-NUMBER      TO PA52WS-ANNUAL-HOURS.

      **** CHECK PAY RATE ****
           MOVE HREMP-PAY-RATE-DN      TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-ALPHA
               END-IF
               END-IF
               END-IF
               PERFORM 430A-CONVERT-NUMBER
               THRU    430A-END
               MOVE PA52WS-NUMBER      TO PA52WS-PAY-RATE.

      **** CHECK SALARY CLASS ****
           MOVE HREMP-SALARY-CLASS-DN  TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-SALARY-CLASS
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-SALARY-CLASS
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-SALARY-CLASS.

      **** CHECK NBR FTE ****
           MOVE HREMP-NBR-FTE-DN       TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-ALPHA
               END-IF
               END-IF
               END-IF
               PERFORM 430A-CONVERT-NUMBER
               THRU    430A-END
               MOVE PA52WS-NUMBER      TO PA52WS-NBR-FTE.

      **** CHECK SCHEDULE ****
           MOVE HREMP-SCHEDULE-DN      TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-SCHEDULE
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-SCHEDULE
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-SCHEDULE.

      **** CHECK PAY STEP ****
           MOVE HREMP-PAY-STEP-DN      TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-ALPHA
               END-IF
               END-IF
               END-IF
               PERFORM 430A-CONVERT-NUMBER
               THRU    430A-END
               MOVE PA52WS-NUMBER      TO PA52WS-PAY-STEP.

      **** CHECK PAY GRADE ****
           MOVE HREMP-PAY-GRADE-DN     TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-PAY-GRADE
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-PAY-GRADE
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-PAY-GRADE.

      **** CHECK JOB CODE
           MOVE HREMP-JOB-CODE-DN      TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-JOB-CODE
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-JOB-CODE
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-JOB-CODE.

MG0301**** CHECK ZIP CODE
MG0301     MOVE HREMP-ZIP-DN           TO PA52WS-FLD-NBR.
MG0301     PERFORM 431-CHECK-IF-CHANGED
MG0301     THRU    431-END.
MG0301     IF (FIELD-CHANGED)
MG0301         IF (I1 >= 1) AND (I1 <= 12)
MG0301             MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
MG0301                                 TO PA52WS-ZIP
MG0301         ELSE
MG0301         IF (I1 >= 13) AND (I1 <= 24)
MG0301             MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
MG0301                                 TO PA52WS-ZIP
MG0301         ELSE
MG0301         IF (I1 >= 25) AND (I1 <= 36)
MG0301             MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
MG0301                                 TO PA52WS-ZIP.
MG0301
           IF (PA52WS-PAY-RATE         NOT = ZEROES)
           OR (PA52WS-SALARY-CLASS     NOT = SPACES)
           OR (PA52WS-NBR-FTE          NOT = ZEROES)
           OR (PA52WS-SCHEDULE         NOT = SPACES)
           OR (PA52WS-PAY-STEP         NOT = ZEROES)
           OR (PA52WS-PAY-GRADE        NOT = SPACES)
           OR (PA52WS-JOB-CODE         NOT = SPACES)
           OR (PA52WS-ANNUAL-HOURS     NOT = ZEROES)
MG0301     OR (PA52WS-ZIP              NOT = SPACES)           
               SET FIELD-CHANGED       TO TRUE
               IF (PA52WS-ANNUAL-HOURS = ZEROES)
                   MOVE EMP-ANNUAL-HOURS   TO PA52WS-ANNUAL-HOURS
               END-IF
               IF (PA52WS-PAY-RATE     = ZEROES)
                   MOVE EMP-PAY-RATE   TO PA52WS-PAY-RATE
               END-IF
               IF (PA52WS-SALARY-CLASS = SPACES)
                   MOVE EMP-SALARY-CLASS   TO PA52WS-SALARY-CLASS
               END-IF
               IF (PA52WS-NBR-FTE      = ZEROES)
                   MOVE EMP-NBR-FTE    TO PA52WS-NBR-FTE
               END-IF
               IF (PA52WS-SCHEDULE     = SPACES)
                   MOVE EMP-SCHEDULE   TO PA52WS-SCHEDULE
               END-IF
               IF (PA52WS-PAY-STEP     = ZEROES)
                   MOVE EMP-PAY-STEP   TO PA52WS-PAY-STEP
               END-IF
               IF (PA52WS-PAY-GRADE    = SPACES)
                   MOVE EMP-PAY-GRADE  TO PA52WS-PAY-GRADE
               END-IF
               IF (PA52WS-JOB-CODE     = SPACES)
ACS001*            MOVE EMP-JOB-CODE   TO PA52WS-JOB-CODE.
MG0301             MOVE EMP-JOB-CODE   TO PA52WS-JOB-CODE
MG0301         END-IF
MG0301         IF (PA52WS-ZIP          = SPACES)
MG0301             MOVE EMP-ZIP        TO PA52WS-ZIP.

      **** OLD SALARY
           PERFORM 433-CALC-OLD-SALARY
           THRU    433-END.
           MOVE 508                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (10).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (10).
           MOVE PA52WS-OLD-SALARY      TO PA52WS-NUM-15.
           MOVE PA52WS-ALPHA-15        TO WFAPI-I-VARIABLE-VAL (10).

           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-ADD-VAR-SETUP.

           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.

      **** NEW SALARY
           PERFORM 436-CALC-NEW-SALARY
           THRU    436-END.
           MOVE 509                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (1).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (1).
           MOVE PA52WS-NEW-SALARY      TO PA52WS-NUM-15.
           MOVE PA52WS-ALPHA-15        TO WFAPI-I-VARIABLE-VAL (1).

      **** ACTION CODE
           MOVE 510                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (2).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (2).
           MOVE PA52F1-PCT-ACTION-CODE TO WFAPI-I-VARIABLE-VAL (2).

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 10)

               IF (WFAPI-I-VARIABLE-VAL (I1) = PA52WS-BLANK)
                   INITIALIZE WFAPI-I-VARIABLE-VAL (I1)
               END-IF
           END-PERFORM.

      **** REASON (1)
           MOVE 511                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (3).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (3).
           MOVE PA52F1-PCT-REASON (1)  TO WFAPI-I-VARIABLE-VAL (3).

      **** REASON (2)
           MOVE 512                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (4).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (4).
           MOVE PA52F1-PCT-REASON (2)  TO WFAPI-I-VARIABLE-VAL (4).

      **** CHECK TO SEE IF EMPLOYEE STATUS CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-EMP-STATUS-DN    TO PA52WS-FLD-NBR.
           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.

      **** OLD EMPLOYEE STATUS
           MOVE 513                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (5).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (5).
           MOVE EMP-EMP-STATUS         TO WFAPI-I-VARIABLE-VAL (5).

      **** NEW EMPLOYEE STATUS
           MOVE 514                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (6).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (6).
           IF (FIELD-USED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1)
                                       TO WFAPI-I-VARIABLE-VAL (6)
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO WFAPI-I-VARIABLE-VAL (6)
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO WFAPI-I-VARIABLE-VAL (6).

           IF (WFAPI-I-VARIABLE-VAL (6) = PA52WS-BLANK)
               INITIALIZE WFAPI-I-VARIABLE-VAL (6)
           ELSE
           IF (WFAPI-I-VARIABLE-VAL (6) = SPACES)
               MOVE WFAPI-I-VARIABLE-VAL (5)
                                       TO WFAPI-I-VARIABLE-VAL (6).

      **** OVER BUDGET
           MOVE 515                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (7).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (7).
           IF (HREMP-VAR-FTE           < ZEROES)
           OR (HREMP-VAR-HDCNT         < ZEROES)
               MOVE "Y"                TO WFAPI-I-VARIABLE-VAL (7)
           ELSE
               MOVE "N"                TO WFAPI-I-VARIABLE-VAL (7).

      **** PERCENT CHANGE
           MOVE 516                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (8).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (8).
           COMPUTE PA52WS-PCT-CHANGE ROUNDED
                                       = (PA52WS-NEW-SALARY
                                       /  PA52WS-OLD-SALARY
                                       *  100)
                                       - 100.
           MOVE PA52WS-PCT-CHANGE      TO PA52WS-NUM-15.
           MOVE PA52WS-ALPHA-15        TO WFAPI-I-VARIABLE-VAL (8).

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 6)

               IF (WFAPI-I-VARIABLE-VAL (I1) = PA52WS-BLANK)
                   INITIALIZE WFAPI-I-VARIABLE-VAL (I1)
               END-IF
           END-PERFORM.

      **** ACTION TYPE
           MOVE 519                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (9).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (9).
           MOVE PA52F1-PT-ACTION-TYPE  TO WFAPI-I-VARIABLE-VAL (9).

      **** EFFECT DATE
           MOVE 520                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (10).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (10).
           MOVE PA52F1-PCT-EFFECT-DATE TO WFAPI-I-VARIABLE-VAL (10).

           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-ADD-VAR-SETUP.

           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.

      **** EMPLOYEE
           MOVE 521                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (1).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (1).
           MOVE PA52F1-PCT-EMPLOYEE    TO WFAPI-I-VARIABLE-VAL (1).

      **** ACTION NBR
           MOVE 522                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (2).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (2).
           MOVE PA52WS-ACTION-NBR      TO WFAPI-I-VARIABLE-VAL (2).

      **** MAIL-SUBJECT VARIABLE IS SET SO THAT PEOPLE CAN GET E-MAIL
025320     MOVE WFAPI-MAIL-SUBJECT     TO WFAPI-I-VARIABLE-NAME (3).
           MOVE 518                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-VAL  (3).
025430     MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (3).

MG0301**** IMMEDIATE ACTION
MG0301     MOVE 900                     TO CRT-MSG-NBR.
MG0301     PERFORM 790-GET-MSG.
MG0301     MOVE CRT-MESSAGE             TO WFAPI-I-VARIABLE-NAME (4).
MG0301     MOVE "S"                     TO WFAPI-I-VARIABLE-TYPE (4).
MG0301     MOVE PA52F1-IMMEDIATE-ACTION TO WFAPI-I-VARIABLE-VAL  (4).
MG0301
MG0301**** OLD EMPLOYEE ZIP
MG0301     MOVE 901                    TO CRT-MSG-NBR.
MG0301     PERFORM 790-GET-MSG.
MG0301     MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (5).
MG0301     MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (5).
MG0301     MOVE EMP-ZIP                TO WFAPI-I-VARIABLE-VAL (5).
MG0301
MG0301**** NEW EMPLOYEE ZIP
MG0301     MOVE 902                    TO CRT-MSG-NBR.
MG0301     PERFORM 790-GET-MSG.
MG0301     MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (6).
MG0301     MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (6).
MG0301     MOVE PA52WS-ZIP             TO WFAPI-I-VARIABLE-VAL (6).
MG0301

           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-ADD-VAR-SETUP.

      *---------------------------------+
      * Create Work Unit Message Header |
      *---------------------------------+
      **** Perform ADD MESSAGE HEADER Routine
           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.

           MOVE 517                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-MESSAGE-ID.
           MOVE CRT-USER-NAME          TO WFAPI-I-USER.

           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-ADD-MSGHDR-SETUP.

      *---------------------------------+
      * Create Work Unit Message Detail |
      *---------------------------------+
      **** Perform ADD MESSAGE DETAIL Routine
           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.

           MOVE 517                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-MESSAGE-ID.

           MOVE 518                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-MESSAGE.

           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-ADD-MSGDTL-SETUP.

      *---------------------------------+
      * Create Work Unit Folder         |
      *---------------------------------+
      **** This creates data in WF21 and matches with fields in WF02
      **** Perform ADD FOLDER Routine
           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.

           MOVE CRT-SCREEN-NAME        TO PA52WS-SCREEN-NAME.
           MOVE CRT-SCREEN-NUMBER      TO PA52WS-SCREEN-NUMBER.
           MOVE PA52WS-SCREEN-CODE     TO WFAPI-I-FORM.
           MOVE PA52WS-FORM            TO WFAPI-I-DOCUMENT-ID.
           MOVE PA52F1-PCT-COMPANY     TO WFAPI-I-KEY-VALUE (1).
           MOVE PA52F1-PCT-EMPLOYEE    TO WFAPI-I-KEY-VALUE (2).
           MOVE PA52F1-PCT-ACTION-CODE TO WFAPI-I-KEY-VALUE (3).
           MOVE PA52F1-PCT-EFFECT-DATE TO WFAPI-I-KEY-VALUE (4).
J33429     MOVE PA52WS-ACTION-NBR      TO WFAPI-I-KEY-VALUE (5).
           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-ADD-FOLDER-SETUP.

AA0626* PASS THE WORKUNIT CREATED FOR TERM TO WS-WORKUNIT-TERM
AA0626     INITIALIZE WS-WORKUNIT-TERM.
AA0626     MOVE WFAPI-O-WORKUNIT       TO WS-WORKUNIT-TERM.

      *---------------------------------+
      * Release Work Unit               |
      *---------------------------------+
       430-END.

      *---------------------------------+
      * Convert Alpha to Number         |
      *---------------------------------+
       430A-CONVERT-NUMBER.
           MOVE ZEROES                 TO PA52WS-NUMBER.
           IF (PA52WS-ALPHA = "*BLANK")
               GO TO 430A-END.

           INITIALIZE I2
                      I3.
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 15)
               IF  (PA52WS-ALPHA (I1:1) = ".")
                   MOVE I1             TO I2
               END-IF
               IF  (PA52WS-ALPHA (I1:1) = SPACE)
               AND (I3 = ZEROS)
                   COMPUTE I3 = I1 - 1
               END-IF
           END-PERFORM.
           IF  (I2 = ZEROES)
               COMPUTE I2 = I3 + 1
           END-IF.
           COMPUTE PA52WS-WHOLE-START = 1.
           COMPUTE PA52WS-WHOLE-END   = I2 - 1.
           COMPUTE PA52WS-FRACT-START = I2 + 1.
           COMPUTE PA52WS-FRACT-END   = I3.
           MOVE 10                     TO I2.
           PERFORM
               VARYING I1 FROM PA52WS-WHOLE-END BY NEGATIVE-ONE
               UNTIL  (I1 < PA52WS-WHOLE-START)
               MOVE PA52WS-ALPHA (I1:1)    TO PA52WS-NUMBER-A (I2:1)
               SUBTRACT 1                  FROM I2
           END-PERFORM.
           MOVE 11                     TO I2.
           PERFORM
               VARYING I1 FROM PA52WS-FRACT-START BY 1
               UNTIL  (I1 > PA52WS-FRACT-END)
               MOVE PA52WS-ALPHA (I1:1)    TO PA52WS-NUMBER-A (I2:1)
               ADD 1                   TO I2
           END-PERFORM.

       430A-END.

043300******************************************************************
       431-CHECK-IF-CHANGED.
043300******************************************************************

           SET FIELD-NOTCHANGED        TO TRUE.

           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.
           IF (FIELD-USED)
               IF (I1 >= 1) AND (I1 <= 12)
                   IF (PA52F1-PCT-NEW-VALUE-1 (I1)      NOT = SPACES)
                       SET FIELD-CHANGED TO TRUE
                   END-IF
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   IF (PA52F1-PCT-NEW-VALUE-2 (I1 - 12) NOT = SPACES)
                       SET FIELD-CHANGED TO TRUE
                   END-IF
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   IF (PA52F1-PCT-NEW-VALUE-3 (I1 - 24) NOT = SPACES)
                       SET FIELD-CHANGED TO TRUE.

       431-END.

099100******************************************************************
       432-FIND-FIELD-USAGE.
099100******************************************************************

           SET FIELD-NOTUSED               TO TRUE.
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36)
               OR     (PAT-FLD-NBR (I1)    = ZEROES)
               OR     (PAT-FLD-NBR (I1)    = PA52WS-FLD-NBR)

               CONTINUE
           END-PERFORM.

           IF  (I1                         <= 36)
           AND (PAT-FLD-NBR (I1)           NOT = ZEROES)
               SET FIELD-USED              TO TRUE.

       432-END.

043300******************************************************************
043400 433-CALC-OLD-SALARY.
043500******************************************************************
043600
           INITIALIZE PA52WS-OLD-SALARY.

043700     MOVE EMP-NBR-FTE                TO PA52WS-NBR-FTE-WF.
           MOVE EMP-ANNUAL-HOURS           TO PA52WS-ANNUAL-HOURS-WF.
043800
           IF  (PA52WS-ANNUAL-HOURS-WF = ZEROES)
043900         MOVE EMP-COMPANY            TO DB-COMPANY
044000         MOVE EMP-JOB-CODE           TO DB-JOB-CODE
044100         PERFORM 840-FIND-JBCSET1
044200         IF (JOBCODE-FOUND)
044300             MOVE JBC-ANNUAL-HOURS   TO PA52WS-ANNUAL-HOURS-WF
               END-IF
           END-IF.
044400
044500     IF (PA52WS-ANNUAL-HOURS-WF      = ZEROES)
044600         MOVE EMP-COMPANY            TO DB-COMPANY
044700         INITIALIZE DB-PROCESS-LEVEL
044800         PERFORM 840-FIND-PRSSET1
044900         MOVE PRS-ANNUAL-HOURS       TO PA52WS-ANNUAL-HOURS-WF
           END-IF.
045000
045100     IF (PA52WS-ANNUAL-HOURS-WF      = ZEROES)
045200         MOVE 2080                   TO PA52WS-ANNUAL-HOURS-WF.
045300
045400     IF (PA52WS-NBR-FTE-WF           = ZEROES)
045500         MOVE 1                      TO PA52WS-NBR-FTE-WF.
045600
045700     IF (EMP-PAY-STEP                NOT = ZEROES)
045800         PERFORM 434-CALC-FROM-STEP-GRADE
045900         THRU    434-END
046000     ELSE
046100         PERFORM 435-CALC-FROM-HR11
046200         THRU    435-END.
046300
046400 433-END.
046500
046600******************************************************************
046700 434-CALC-FROM-STEP-GRADE.
046800******************************************************************
046900
047000     MOVE EMP-COMPANY                TO DB-COMPANY.
047100     MOVE EMP-SCHEDULE               TO DB-SCHEDULE.
P70533     IF (EMP-PAY-STEP = ZEROES)
P70533         MOVE "G"                    TO DB-INDICATOR
P70533     ELSE
P70533         MOVE "S"                    TO DB-INDICATOR
P70533     END-IF.
047200     MOVE PA52F1-PCT-EFFECT-DATE     TO DB-EFFECT-DATE.
047300     PERFORM 850-FIND-NLT-SGHSET2.
047400
047500     MOVE EMP-COMPANY                TO DB-COMPANY.
047600     MOVE EMP-SCHEDULE               TO DB-SCHEDULE.
047700     MOVE EMP-PAY-GRADE              TO DB-PAY-GRADE.
047800     MOVE EMP-PAY-STEP               TO DB-PAY-STEP.
047900     MOVE SGH-EFFECT-DATE            TO DB-EFFECT-DATE.
048000     PERFORM 840-FIND-SGDSET3.
048100
048200     IF (EMP-SALARY-CLASS            = "H")
048300         COMPUTE PA52WS-OLD-SALARY ROUNDED
                                           = SGD-PAY-RATE
048400                                     * PA52WS-ANNUAL-HOURS-WF
048500                                     * PA52WS-NBR-FTE-WF
048600     ELSE
048700         MOVE SGD-PAY-RATE           TO PA52WS-OLD-SALARY.
048800
048900 434-END.
049000
049100******************************************************************
049200 435-CALC-FROM-HR11.
049300******************************************************************
049400
049500     IF (EMP-SALARY-CLASS            = "H")
049600         COMPUTE PA52WS-OLD-SALARY ROUNDED
                                           = EMP-PAY-RATE
049700                                     * PA52WS-ANNUAL-HOURS-WF
049800                                     * PA52WS-NBR-FTE-WF
049900     ELSE
050000         MOVE EMP-PAY-RATE           TO PA52WS-OLD-SALARY.
050100
050200 435-END.
050300
043300******************************************************************
043400 436-CALC-NEW-SALARY.
043500******************************************************************
043600
           INITIALIZE PA52WS-NEW-SALARY.

043700     MOVE PA52WS-NBR-FTE             TO PA52WS-NBR-FTE-WF.
           MOVE PA52WS-ANNUAL-HOURS        TO PA52WS-ANNUAL-HOURS-WF.
043800
           IF  (PA52WS-ANNUAL-HOURS-WF = ZEROES)
043900         MOVE EMP-COMPANY            TO DB-COMPANY
044000         MOVE PA52WS-JOB-CODE        TO DB-JOB-CODE
044100         PERFORM 840-FIND-JBCSET1
044200         IF  (JOBCODE-FOUND)
044300             MOVE JBC-ANNUAL-HOURS   TO PA52WS-ANNUAL-HOURS-WF
               END-IF
           END-IF.
044400
044500     IF (PA52WS-ANNUAL-HOURS-WF = ZEROES)
044600         MOVE EMP-COMPANY            TO DB-COMPANY
044700         INITIALIZE DB-PROCESS-LEVEL
044800         PERFORM 840-FIND-PRSSET1
044900         MOVE PRS-ANNUAL-HOURS       TO PA52WS-ANNUAL-HOURS-WF
           END-IF.
045000
045100     IF (PA52WS-ANNUAL-HOURS-WF = ZEROES)
045200         MOVE 2080                   TO PA52WS-ANNUAL-HOURS-WF.
045300
045400     IF (PA52WS-NBR-FTE-WF = ZEROES)
045500         MOVE 1                      TO PA52WS-NBR-FTE-WF.
045600
045700     IF (PA52WS-PAY-STEP             NOT = ZEROES)
045800         PERFORM 437-CALC-FROM-STEP-GRADE
045900         THRU    437-END
046000     ELSE
046100         PERFORM 438-CALC-FROM-HR11
046200         THRU    438-END.
046300
046400 436-END.
046500
046600******************************************************************
046700 437-CALC-FROM-STEP-GRADE.
046800******************************************************************
046900
047000     MOVE EMP-COMPANY                TO DB-COMPANY.
J33429     MOVE "S"                        TO DB-INDICATOR.
047100     MOVE PA52WS-SCHEDULE            TO DB-SCHEDULE.
047200     MOVE PA52F1-PCT-EFFECT-DATE     TO DB-EFFECT-DATE.
047300     PERFORM 850-FIND-NLT-SGHSET2.
047400
047500     MOVE EMP-COMPANY                TO DB-COMPANY.
047600     MOVE PA52WS-SCHEDULE            TO DB-SCHEDULE.
047700     MOVE PA52WS-PAY-GRADE           TO DB-PAY-GRADE.
047800     MOVE PA52WS-PAY-STEP            TO DB-PAY-STEP.
047900     MOVE SGH-EFFECT-DATE            TO DB-EFFECT-DATE.
048000     PERFORM 840-FIND-SGDSET3.
048100
048200     IF (PA52WS-SALARY-CLASS            = "H")
048300         COMPUTE PA52WS-NEW-SALARY ROUNDED
                                           = SGD-PAY-RATE
048400                                     * PA52WS-ANNUAL-HOURS-WF
048500                                     * PA52WS-NBR-FTE-WF
048600     ELSE
048700         MOVE SGD-PAY-RATE           TO PA52WS-NEW-SALARY.
048800
048900 437-END.
049000
049100******************************************************************
049200 438-CALC-FROM-HR11.
049300******************************************************************
049400
049500     IF (PA52WS-SALARY-CLASS         = "H")
049600         COMPUTE PA52WS-NEW-SALARY ROUNDED
                                           = PA52WS-PAY-RATE
049700                                     * PA52WS-ANNUAL-HOURS-WF
049800                                     * PA52WS-NBR-FTE-WF
049900     ELSE
050000         MOVE PA52WS-PAY-RATE        TO PA52WS-NEW-SALARY.
050100
050200 438-END.
050300
099100******************************************************************
       439-FIND-SUPER-EMP.
099100******************************************************************

           PERFORM
               VARYING I5 FROM 1 BY 1
               UNTIL  (I5               > PA52WS-NBR-OF-SEARCH)
               OR     (HSU-EMPLOYEE     NOT = ZEROES)
               OR     (HSU-SUPER-RPTS-TO    = SPACES)

               MOVE HSU-SUPER-RPTS-TO  TO DB-CODE
               PERFORM 840-FIND-HSUSET1

           END-PERFORM.

       439-END.

099100******************************************************************
099200 440-DELETE.
099300******************************************************************
099400
099500     PERFORM 910-AUDIT-BEGIN.
099600     IF (DMS-ABORTED)
099700         GO TO 440-END.
099800
099900     MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY.
100000     MOVE "E"                    TO DB-ACTION-TYPE.
100100     MOVE PA52F1-PCT-ACTION-CODE TO DB-ACTION-CODE.
100200     MOVE PA52F1-PCT-EFFECT-DATE TO DB-EFFECT-DATE.
100300     MOVE PA52F1-PCT-EMPLOYEE    TO DB-EMPLOYEE.
100400     MOVE PA52F1-PCT-ACTION-NBR  TO DB-ACTION-NBR.
100500
100600     PERFORM 840-MODIFY-PCTSET1.

100700     PERFORM 830-DELETE-PERSACTION.
100800
100900     MOVE ZEROES                 TO DB-EMP-APP.
101000     MOVE "PA"                   TO DB-CMT-TYPE.
101100     MOVE PA52F1-PCT-ACTION-CODE TO DB-ACTION-CODE.
101200     MOVE PA52F1-PCT-EFFECT-DATE TO DB-DATE.
101300     MOVE PA52F1-PCT-EMPLOYEE    TO DB-EMPLOYEE.
101400     MOVE SPACES                 TO DB-JOB-CODE.
101500     MOVE PA52F1-PCT-ACTION-NBR  TO DB-LN-NBR.
101600     MOVE PACSET2-LN-NBR         TO WS-DB-BEG-RNG.
101700     PERFORM 830-DELETERNG-PACSET2.
102321     SET PA52WS-NOMORE-CODES     TO TRUE.
101800
P63835     IF (PAT-WORKFLOW-FLAG = "Y")
J83708     OR (PA52F1-IMMEDIATE-ACTION = "Y")
P63835         INITIALIZE WFAPI-INPUT
P63835                    WFAPI-OUTPUT
P63835
P63835         MOVE "PCTSET1"              TO WFAPI-I-OBJECT-NAME
P63835         MOVE PA52F1-PCT-COMPANY     TO WFAPI-I-KEY-VALUE (1)
P63835         MOVE "E"                    TO WFAPI-I-KEY-VALUE (2)
P63835         MOVE PA52F1-PCT-EFFECT-DATE TO WFAPI-I-KEY-VALUE (3)
P63835         MOVE PA52F1-PCT-ACTION-CODE TO WFAPI-I-KEY-VALUE (4)
P63835         MOVE PA52F1-PCT-EMPLOYEE    TO WFAPI-I-KEY-VALUE (5)
P63835         MOVE PA52F1-PCT-ACTION-NBR  TO WFAPI-I-KEY-VALUE (6)
P63835         PERFORM 1000-WF-CANCEL-SETUP
P63835     END-IF.
           MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY.
           MOVE PA52F1-PCT-EMPLOYEE    TO DB-EMPLOYEE.
           PERFORM 4000-PAPCT-EMP-PEND-ACT-DATE.
103600
103700     MOVE CRT-RECS-DELETED       TO CRT-MESSAGE.
103800     PERFORM 920-AUDIT-END.
103900
104000 440-END.
104100
      ******************************************************************
       450-REVERSE.
      ******************************************************************

           MOVE PA52F1-PCT-MERGE-ACTN-NBR    TO PA52F1-SVD-ACTN-NBR.
           PERFORM 460-FILL-REV-FLDS
           THRU    460-END
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36).

           IF  (PAPCT-HISTERR-EXISTS)
               MOVE 541                TO CRT-MSG-NBR
           ELSE
               MOVE 534                TO CRT-MSG-NBR.

       450-END.

      ******************************************************************
       460-FILL-REV-FLDS.
      ******************************************************************

           IF (PAPCT-REV-NOTFOUND (I1))
               GO TO 460-END.

           MOVE PAPCT-REV-OLD-VALUE (I1)   TO HRWS-VALUE.
           IF (I1 <= 12)
               MOVE HRWS-VALUE         TO PA52F1-PCT-NEW-VALUE-1 (I1)
                                          PA52F1-REV-SVD-VALUE-1 (I1)
           ELSE
           IF (I1 <= 24)
               COMPUTE I2 = I1 - 12
               MOVE HRWS-VALUE         TO PA52F1-PCT-NEW-VALUE-2 (I2)
                                          PA52F1-REV-SVD-VALUE-2 (I2)
           ELSE
               COMPUTE I2 = I1 - 24
               MOVE HRWS-VALUE         TO PA52F1-PCT-NEW-VALUE-3 (I2)
                                          PA52F1-REV-SVD-VALUE-3 (I2)
           END-IF.

       460-END.

104200******************************************************************
104300 480-INQUIRE.
104400******************************************************************
104500
104600     INITIALIZE PA52F1-DETAIL-GROUP-1.
104700     INITIALIZE PA52F1-DETAIL-GROUP-2.
104800     INITIALIZE PA52F1-DETAIL-GROUP-3.
           INITIALIZE PA52F1-DETAIL-GROUP-1A.
           INITIALIZE PA52F1-DETAIL-GROUP-2A.
           INITIALIZE PA52F1-DETAIL-GROUP-3A.
           INITIALIZE PA52F1-PCT-ANT-END-DATE
                      PA52F1-PCT-EDM-END-DATE
                      PA52F1-PCT-EDM-EFFECT-DT
                      PA52F1-PCT-OCCUR-TYPE
                      PA52F1-OCC-DESC
                      PA52F1-PCT-PARTICIPNT
P86487                PA52F1-PCT-BASE-CURRENCY
P86487                PA52F1-PCT-BASE-ND
P86487                PA52F1-PCT-BASE-PAY-RATE
                      PA52F1-PCT-MOVE-FROM-LEVEL
                      PA52F1-PCT-REASON (1)
                      PA52F1-PCT-REASON (2).
           INITIALIZE                     PA52WS-FIRST-ACT-NBR
                                          PA52WS-LAST-ACT-NBR.
           INITIALIZE                     PA52F1-MORE-MSG.
105300
           INITIALIZE                     PA52F1-PCT-PROCESS-TYPE
                                          PA52F1-PCT-NEW-EFFECT-DATE
                                          PA52F1-PCT-HIST-CORR-FLAG
                                          PA52F1-PCT-MERGE-ACTN-NBR
                                          PA52F1-MERGE-ACT-OBJ-ID
                                          PA52F1-SVD-ACTN-NBR
                                          PA52F1-PENDING-MSG
                                          PA52F1-ON-HOLD-MSG.
           
105400     PERFORM
105500         VARYING I1 FROM 1 BY 1
105600         UNTIL  (I1 > 36)
105700
105900         MOVE PAT-FLD-NBR (I1)       TO PA52WS-PAT-FIELD-NBR (I1)
                                              DB-FLD-NBR
               INITIALIZE CRT-PHRASE-XLT
               IF (PAT-FLD-NBR (I1)        NOT = ZEROES)
                   PERFORM 840-FIND-PADSET1
                   MOVE PAD-ITEM-NAME      TO CRT-PHRASE
                   MOVE PA52WS-PHRASE-SIZE TO CRT-PHRASE-SIZE
                   MOVE "N"                TO CRT-PUT-DOTS
                   PERFORM 900-GET-PHRASE-XLT
               END-IF
106000         IF (I1 < 13)
                   MOVE "V"                TO PA52F1-SELECT-BTN-1 (I1)
                   MOVE CRT-PHRASE-XLT     TO
                                           PA52F1-PAT-ITEM-NAME-1 (I1)
106300             MOVE PAT-FLD-NBR (I1)   TO PA52F1-PAT-FLD-NBR-1 (I1)
106300                                        PA52F1-PAT-FLD-NBR-1A (I1)
                   IF (PAT-FLD-NBR (I1)    = 62)
                       MOVE "P"            TO PA52F1-SELECT-BTN-1A (I1)
                       INITIALIZE PA52F1-SELECT-BTN-1 (I1)
                   END-IF
106400         ELSE
106500         IF (I1 > 24)
106600             COMPUTE I8 = (I1 - 24)
                   MOVE CRT-PHRASE-XLT     TO
                                           PA52F1-PAT-ITEM-NAME-3 (I8)
106900             MOVE PAT-FLD-NBR (I1)   TO
107000                                     PA52F1-PAT-FLD-NBR-3 (I8)
107100         ELSE
107200         IF (I1 > 12)
107300             COMPUTE I8 = (I1 - 12)
                   MOVE "V"                TO PA52F1-SELECT-BTN-2 (I8)
                   MOVE CRT-PHRASE-XLT     TO
                                           PA52F1-PAT-ITEM-NAME-2 (I8)
107600             MOVE PAT-FLD-NBR (I1)   TO
107700                                     PA52F1-PAT-FLD-NBR-2 (I8)
106300                                     PA52F1-PAT-FLD-NBR-2A (I8)
                   IF (PAT-FLD-NBR (I1)    = 62)
                       MOVE "P"            TO PA52F1-SELECT-BTN-2A (I8)
                       INITIALIZE PA52F1-SELECT-BTN-2 (I8)
                   END-IF
107800         END-IF
107900         END-IF
108000         END-IF
108100     END-PERFORM.
108200
108300     MOVE EMP-LAST-NAME          TO HRWS-LAST-NAME.
108400     MOVE EMP-FIRST-NAME         TO HRWS-FIRST-NAME.
108500     MOVE EMP-MIDDLE-INIT        TO HRWS-MIDDLE-INIT.
           MOVE EMP-NAME-SUFFIX        TO HRWS-NAME-SUFFIX.
           MOVE EMP-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE.
108600     PERFORM 750-HR-FORMAT-NAME.
108700     MOVE HRWS-FORMAT-NAME       TO PA52F1-EMP-NAME.

           MOVE PRS-COMPANY               TO DB-COMPANY.
           MOVE SPACES                    TO DB-COUNTRY-CD-REQ
                                             DB-PROCESS-LEVEL.
      * 50 = Social Number
           MOVE HREMP-FICA-NBR-DN         TO DB-FLD-NBR.
           PERFORM 840-FIND-PASSET1.
           IF (PASCRTY-FOUND)
               MOVE PAS-SEC-LEVEL         TO HRWS-SEC-LEVEL
               PERFORM 730-HR-FIELD-SECURITY
               IF (HRWS-FLD-SECURED)
                   MOVE SPACES            TO PA52F1-EMP-FICA-NBR
               ELSE
                   MOVE EMP-FICA-NBR      TO PA52F1-EMP-FICA-NBR.
108900
109000     MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY.
109100     MOVE PA52F1-PCT-EMPLOYEE    TO DB-EMPLOYEE.
109200     MOVE 01                     TO DB-POS-LEVEL.
P58592     MOVE PEPSET3-POS-LEVEL      TO WS-DB-BEG-RNG.
P58592     PERFORM 850-FIND-BEGRNG-PEPSET3.
109500     IF  (PAEMPPOS-FOUND)
109900         MOVE PEP-EFFECT-DATE    TO PA52F1-PEP-EFFECT-DATE.
110000
           SET PA52WS-NO-MOVELEVELDONE TO TRUE.
102321     IF (PA52F1-PCT-EMPLOYEE      NOT = PA52WS-HOLD-EMPLOYEE)
102321     OR (PA52F1-PCT-ACTION-CODE   NOT = PA52WS-HOLD-ACTION-CODE)
102321         SET PA52WS-NOMORE-CODES     TO TRUE
102321     END-IF.
           MOVE PA52F1-PCT-COMPANY         TO DB-COMPANY.
           MOVE "E"                        TO DB-ACTION-TYPE.
           MOVE PA52F1-PCT-EMPLOYEE        TO DB-EMPLOYEE.
           MOVE PCTSET2-EMPLOYEE           TO WS-DB-BEG-RNG. 
           PERFORM 850-FIND-BEGRNG-PCTSET2.
           IF (PERSACTION-FOUND)
               IF  (PCT-ACTION-CODE       = PA52F1-PCT-ACTION-CODE)
               AND (PA52F1-PCT-ACTION-NBR = ZEROES)
102321         AND (PA52WS-NOMORE-CODES)
                   PERFORM 860-FIND-NXTRNG-PCTSET2
               ELSE
                   IF  (PCT-ACTION-CODE = PA52F1-PCT-ACTION-CODE)
                   AND (PCT-ACTION-NBR  = PA52F1-PCT-ACTION-NBR)
                   AND (PCT-EFFECT-DATE = PA52F1-PCT-EFFECT-DATE)
102321             AND (PA52WS-NOMORE-CODES)
                       PERFORM 860-FIND-NXTRNG-PCTSET2
                   END-IF
               END-IF
               IF  (PERSACTION-FOUND)
                   MOVE 116                TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
                   MOVE CRT-MESSAGE        TO PA52F1-PENDING-MSG
102321             INITIALIZE                 PA52WS-HOLD-EMPLOYEE
102321                                        PA52WS-HOLD-ACTION-CODE
102321             MOVE PCT-EMPLOYEE       TO PA52WS-HOLD-EMPLOYEE
102321             MOVE PCT-ACTION-CODE    TO PA52WS-HOLD-ACTION-CODE
102321             SET PA52WS-OTHER-CODES  TO TRUE
J60711*            IF (PCT-HOLD-FLAG = "Y")
J60711*                MOVE 175                TO CRT-MSG-NBR
J60711*                PERFORM 790-GET-MSG
J60711*                MOVE CRT-MESSAGE        TO PA52F1-ON-HOLD-MSG
J60711*            END-IF
               END-IF
           END-IF.

           IF (PA52F1-PENDING-MSG = SPACES)
               MOVE "L"                        TO DB-ACTION-TYPE
               MOVE PCTSET2-EMPLOYEE           TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PCTSET2
               IF  (PERSACTION-FOUND)
                   MOVE 116                TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
                   MOVE CRT-MESSAGE        TO PA52F1-PENDING-MSG
J60711*            IF (PCT-HOLD-FLAG = "Y")
J60711*                MOVE 175                TO CRT-MSG-NBR
J60711*                PERFORM 790-GET-MSG
J60711*                MOVE CRT-MESSAGE        TO PA52F1-ON-HOLD-MSG
J60711*            END-IF
               END-IF
           END-IF.

010900     MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY.
011000     MOVE "E"                    TO DB-ACTION-TYPE.
011100     MOVE PA52F1-PCT-EMPLOYEE    TO DB-EMPLOYEE.
011200     MOVE PA52F1-PCT-ACTION-CODE TO DB-ACTION-CODE.
011300     MOVE PA52F1-PCT-EFFECT-DATE TO DB-EFFECT-DATE.
           MOVE PA52F1-PCT-ACTION-NBR  TO DB-ACTION-NBR.
           PERFORM 840-FIND-PCTSET2.

           IF  (PA52F1-FC = "I")
           AND (PERSACTION-NOTFOUND)
               MOVE WS-HIGH-VALUES      TO DB-ACTION-NBR
               PERFORM 850-FIND-NLT-PCTSET2.

110100     MOVE 1 TO I8.
110200     PERFORM 484-MOVE-CURRENT-DATA
110300     THRU    484-END
110400         VARYING I1 FROM 1 BY 1
110500         UNTIL  (I1 > 36).


J28956     INITIALIZE   PA52WS-FILL-LTM
J28956                  PA52WS-FILL-LTM1
J28956                  PA52WS-FILL-LTM2
J28956                  PA52WS-FILL-LTM3.
J28956
J40356     MOVE 1 TO I8.
J40356     PERFORM 300-LOAD-LTM-VALUES
J40356     THRU    300-END 
J40356         VARYING I1 FROM 1 BY 1
J40356         UNTIL  (I1 > 36).
       
P49898     MOVE PA52F1-PCT-COMPANY     TO PA52F1-ORIG-COMPANY.
P49898     MOVE PA52F1-PCT-EMPLOYEE    TO PA52F1-ORIG-EMPLOYEE.
P49898     MOVE PA52F1-PCT-ACTION-CODE TO PA52F1-ORIG-ACTION-CODE.
P49898     MOVE PA52F1-PCT-EFFECT-DATE TO PA52F1-ORIG-EFFECT-DATE.
P49898
           IF (PERSACTION-NOTFOUND)
           OR (PCT-COMPANY     NOT = PA52F1-PCT-COMPANY)
           OR (PCT-ACTION-TYPE NOT = "E")
           OR (PCT-EMPLOYEE    NOT = PA52F1-PCT-EMPLOYEE)
           OR (PCT-ACTION-CODE NOT = PA52F1-PCT-ACTION-CODE)
           OR (PCT-EFFECT-DATE NOT = PA52F1-PCT-EFFECT-DATE)
               INITIALIZE                      PA52F1-PCT-ACTION-NBR
P81486                                         PA52F1-PCT-APPROVAL-FLAG
               IF  (PA52F1-PCT-REASON (1) = SPACES)
               AND (PA52F1-PCT-REASON (2) = SPACES)
                   IF (PAT-DFT-REASON (1) NOT = SPACES)
                       MOVE PAT-DFT-REASON (1) TO PA52F1-PCT-REASON (1)
                   END-IF
                   IF (PAT-DFT-REASON (2) NOT = SPACES)
                       MOVE PAT-DFT-REASON (2) TO PA52F1-PCT-REASON (2)
                   END-IF
               END-IF
               IF (PAT-DFT-UPD-BN = SPACES)
                   MOVE PA52F1-PCT-COMPANY         TO DB-COMPANY
                   PERFORM 840-KFIND-BNCSET1
                   IF (BNCOMPANY-KFOUND)
                      MOVE "Y" TO PA52F1-PCT-UPDATE-BENEFIT
                   ELSE
                      MOVE "N" TO PA52F1-PCT-UPDATE-BENEFIT
                   END-IF
               ELSE
                   MOVE PAT-DFT-UPD-BN TO PA52F1-PCT-UPDATE-BENEFIT
               END-IF
               IF (PAT-DFT-UPD-LP = SPACES)
J13588*            MOVE PA52F1-PCT-COMPANY         TO EDCDWS-COMPANY
J13588*            MOVE "LP"                   TO EDCDWS-SYSTEM
J13588*            PERFORM 6000-IS-SYSTEM-TRIGGER-ENABLED
J13588*            IF  (EDCDWS-TRIGGER-ENABLED)
                       MOVE "Y" TO PA52F1-PCT-UPD-ABS-MGMT
J13588*            ELSE
J13588*                MOVE "N" TO PA52F1-PCT-UPD-ABS-MGMT
J13588*            END-IF
               ELSE
                   MOVE PAT-DFT-UPD-LP TO  PA52F1-PCT-UPD-ABS-MGMT
               END-IF
               MOVE PAT-HIST-CORR-FLAG  TO PA52F1-PCT-HIST-CORR-FLAG
               MOVE PAT-DFT-UPD-RQ-DED  TO PA52F1-PCT-UPDATE-REQ-DED
J40356         IF (PA52WS-LTM-SELECTED)
J40356             MOVE 545                TO CRT-MSG-NBR
J40356             PERFORM 790-GET-MSG
J40356             SET PA52WS-LTM-NOT-SELECTED TO TRUE
J40356         ELSE
                   MOVE 407                TO CRT-MSG-NBR
J40356             SET PA52WS-LTM-SELECT-OPEN  TO TRUE
J40356         END-IF 
               GO TO 480-END  
J40356     END-IF.
      
           PERFORM 481-MOVE-TO-SCREEN
           THRU    481-END.

J40356     IF (PA52WS-LTM-SELECTED)
J40356         MOVE 545                    TO CRT-MSG-NBR  
J40356         PERFORM 790-GET-MSG 
J40356         SET PA52WS-LTM-NOT-SELECTED TO TRUE
J40356     ELSE
J40356         SET PA52WS-LTM-SELECT-OPEN  TO TRUE
J40356         MOVE CRT-INQ-COMPLETE       TO CRT-MESSAGE 
J40356     END-IF.
      
       480-END.
      
112800******************************************************************
112900 481-MOVE-TO-SCREEN.
113000******************************************************************
113100
113200     MOVE PCT-COMPANY            TO PA52F1-PCT-COMPANY.
113300     MOVE PCT-EMPLOYEE           TO PA52F1-PCT-EMPLOYEE.
113400     MOVE PCT-ACTION-CODE        TO PA52F1-PCT-ACTION-CODE.
113500     MOVE PCT-ACTION-NBR         TO PA52F1-PCT-ACTION-NBR.
113700     MOVE PCT-EFFECT-DATE        TO PA52F1-PCT-EFFECT-DATE.
           MOVE PCT-COMPANY            TO DB-COMPANY.
           MOVE "E"                    TO DB-ACTION-TYPE.
           MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE.
           MOVE PCT-EFFECT-DATE        TO DB-EFFECT-DATE.
           MOVE PCTSET2-EFFECT-DATE    TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PCTSET2.
           MOVE PCT-ACTION-NBR         TO PA52WS-LAST-ACT-NBR.
           PERFORM
               UNTIL (PERSACTION-NOTFOUND)
                   MOVE PCT-ACTION-NBR TO PA52WS-FIRST-ACT-NBR
                   PERFORM 860-FIND-NXTRNG-PCTSET2
           END-PERFORM.

           MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY.
           MOVE "E"                    TO DB-ACTION-TYPE.
           MOVE PA52F1-PCT-EMPLOYEE    TO DB-EMPLOYEE.
           MOVE PA52F1-PCT-ACTION-CODE TO DB-ACTION-CODE.
           MOVE PA52F1-PCT-EFFECT-DATE TO DB-EFFECT-DATE.
           MOVE PA52F1-PCT-ACTION-NBR  TO DB-ACTION-NBR.
           PERFORM 840-FIND-PCTSET2.
           PERFORM 8400-AFTER-FIND-PCT.

113800     MOVE PCT-ANT-END-DATE       TO PA52F1-PCT-ANT-END-DATE.

114100     MOVE PCT-PARTICIPNT         TO PA52F1-PCT-PARTICIPNT.
114200     MOVE PCT-OCCUR-TYPE         TO PA52F1-PCT-OCCUR-TYPE.
114300     MOVE PCT-EDM-EFFECT-DT      TO PA52F1-PCT-EDM-EFFECT-DT.
114300     MOVE PCT-EDM-END-DATE       TO PA52F1-PCT-EDM-END-DATE.
           MOVE PCT-APPROVAL-FLAG      TO PA52F1-PCT-APPROVAL-FLAG.
           MOVE PCT-PROCESS-TYPE       TO PA52F1-PCT-PROCESS-TYPE.
           MOVE PCT-HIST-CORR-FLAG     TO PA52F1-PCT-HIST-CORR-FLAG.
           MOVE PCT-MERGE-ACTN-NBR     TO PA52F1-PCT-MERGE-ACTN-NBR.
           MOVE PCT-POS-LEVEL-MOVE     TO PA52F1-PCT-MOVE-FROM-LEVEL.
113900     MOVE PCT-REASON (1)         TO PA52F1-PCT-REASON (1).
114000     MOVE PCT-REASON (2)         TO PA52F1-PCT-REASON (2).
           MOVE PCT-UPDATE-BENEFIT     TO PA52F1-PCT-UPDATE-BENEFIT.
           MOVE PCT-UPD-ABS-MGMT       TO PA52F1-PCT-UPD-ABS-MGMT.
           MOVE PCT-UPDATE-REQ-DED     TO PA52F1-PCT-UPDATE-REQ-DED.
114400
114500     IF (PCT-OCCUR-TYPE NOT = SPACES)
114600         MOVE PCT-OCCUR-TYPE     TO DB-OCCUR-TYPE
114700         PERFORM 840-FIND-OCCSET1
114800         IF (OCCURTYPE-FOUND)
114900             MOVE OCC-DESC       TO PA52F1-OCC-DESC
115000         ELSE
115100              MOVE SPACES        TO PA52F1-OCC-DESC
115200         END-IF
115300     ELSE
115400         MOVE SPACES             TO PA52F1-OCC-DESC.
115500
115600     MOVE PA52WS-PRS-NAME        TO PA52F1-PRS-NAME.
115700     MOVE PAT-DESCRIPTION        TO PA52F1-PAT-DESCRIPTION.
115800
           IF (PA52WS-FIRST-ACT-NBR NOT = PA52WS-LAST-ACT-NBR)
               MOVE 170                TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE        TO PA52F1-MORE-MSG.

116500     MOVE "N"                    TO PA52F1-IMMEDIATE-ACTION.
J60711
J60711     IF (PCT-HOLD-FLAG = "Y")
J60711         MOVE 175                TO CRT-MSG-NBR
J60711         PERFORM 790-GET-MSG
J60711         MOVE CRT-MESSAGE        TO PA52F1-ON-HOLD-MSG
J60711     ELSE
J60711         INITIALIZE                 PA52F1-ON-HOLD-MSG
J60711     END-IF.
116600
117100     PERFORM
117200         VARYING I1 FROM 1 BY 1
117300         UNTIL  (I1 > 36)
117400
117500             IF (PA52WS-PCT-PRE-VALUE (I1) = PA52WS-SEC-MESSAGE)
117600                 MOVE PA52WS-SEC-MESSAGE
                                           TO PA52WS-PCT-NEW-VALUE (I1)
117900                 IF (I1 < 13)
118000                     MOVE PA52WS-SEC-MESSAGE TO
118100                                      PA52F1-PCT-NEW-VALUE-1 (I1)
118200                 ELSE
118300                 IF (I1 > 24)
118400                     COMPUTE I8 = (I1 - 24)
118500                     MOVE PA52WS-SEC-MESSAGE TO
118600                                      PA52F1-PCT-NEW-VALUE-3 (I8)
118700                 ELSE
118800                 IF (I1 > 12)
118900                     COMPUTE I8 = (I1 - 12)
119000                     MOVE PA52WS-SEC-MESSAGE TO
119100                                      PA52F1-PCT-NEW-VALUE-2 (I8)
119200                 END-IF
119300                 END-IF
119400                 END-IF
119500             ELSE
J40356                 IF (PA52WS-LTM-NOT-SELECTED)
119600                     MOVE PCT-NEW-VALUE (I1) TO
119700                                      PA52WS-PCT-NEW-VALUE (I1)
119800                     IF (I1 < 13)
119900                         MOVE PCT-NEW-VALUE (I1)
120000                                   TO PA52F1-PCT-NEW-VALUE-1 (I1)
120100                     ELSE
120200                         IF (I1 > 24)
120300                             COMPUTE I8 = (I1 - 24)
120400                             MOVE PCT-NEW-VALUE (I1)
120500                                   TO PA52F1-PCT-NEW-VALUE-3 (I8)
120600                         ELSE
120700                             IF (I1 > 12)
120800                                COMPUTE I8 = (I1 - 12)
120900                                MOVE PCT-NEW-VALUE (I1)
121000                                    TO PA52F1-PCT-NEW-VALUE-2 (I8)
121100                              END-IF
121200                         END-IF
121300                     END-IF
J40356                 ELSE    
J40356                     IF (I1 < 13)
J40356                         MOVE PA52WS-PCT-NEW-VALUE (I1)
J40356                                   TO PA52F1-PCT-NEW-VALUE-1 (I1)
J40356                     ELSE
J40356                         IF (I1 > 24)
J40356                             COMPUTE I8 = (I1 - 24)
J40356                             MOVE PA52WS-PCT-NEW-VALUE (I1)
J40356                                   TO PA52F1-PCT-NEW-VALUE-3 (I8)
J40356                         ELSE
J40356                             IF (I1 > 12)
J40356                                COMPUTE I8 = (I1 - 12)  
J40356                                MOVE PA52WS-PCT-NEW-VALUE (I1)
J40356                                    TO PA52F1-PCT-NEW-VALUE-2 (I8)
J40356                              END-IF
J40356                         END-IF
J40356                     END-IF
J40356                 END-IF
121400             END-IF
121500     END-PERFORM.
121600
090800     MOVE "PAMSG"                TO CRT-ERROR-CAT.
090900     MOVE 100                    TO CRT-MSG-NBR.
091000     PERFORM 790-GET-MSG.
091100     MOVE CRT-MESSAGE            TO PA52F1-COMMENTS.
           INITIALIZE CRT-MESSAGE.

           INITIALIZE PA52F1-COMMENTS-FLAG.
121700     MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY.
121800     INITIALIZE DB-EMP-APP.
121900     MOVE "PA"                   TO DB-CMT-TYPE.
122200     MOVE PA52F1-PCT-ACTION-CODE TO DB-ACTION-CODE.
122000     MOVE PA52F1-PCT-EFFECT-DATE TO DB-DATE.
122000     MOVE PA52F1-PCT-EMPLOYEE    TO DB-EMPLOYEE.
122100     INITIALIZE DB-JOB-CODE.
122300     MOVE PA52F1-PCT-ACTION-NBR  TO DB-LN-NBR.
122400     MOVE PACSET2-LN-NBR         TO WS-DB-BEG-RNG.
122500     PERFORM 850-KFIND-BEGRNG-PACSET2.
           IF (PACOMMENTS-KFOUND)
               MOVE "*"                TO PA52F1-COMMENTS-FLAG.
122600
121700     MOVE PCT-COMPANY            TO DB-COMPANY.
121800     MOVE ZEROES                 TO DB-EMP-APP.
121900     MOVE "PA"                   TO DB-CMT-TYPE.
122000     MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE.
122100     MOVE SPACES                 TO DB-JOB-CODE.
122200     MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE.
122300     MOVE PCT-ACTION-NBR         TO DB-LN-NBR.
122400     MOVE PACSET1-LN-NBR         TO WS-DB-BEG-RNG.
122500     PERFORM 850-FIND-BEGRNG-PACSET1.
122600
122700 481-END.
122800
122900******************************************************************
123000 484-MOVE-CURRENT-DATA.
123100******************************************************************
123200
123300     IF (PA52F1-PAT-FLD-NBR-1 (1) = ZEROS)
123400         MOVE 36                        TO I1
123500         GO TO 484-END.
123600
123700     IF (I1 < 13)
               IF  (PA52F1-PAT-FLD-NBR-1 (I1) = ZEROES)
                   MOVE SPACES         TO PA52F1-PCT-PRE-VALUE-1 (I1)
                                          PA52WS-PCT-PRE-VALUE (I1)
                   GO TO 484-END
               END-IF
               IF  (PA52F1-PAT-FLD-NBR-1 (I1) > 2000)
               AND (PA52F1-PAT-FLD-NBR-1 (I1) < 2100)
                   MOVE PA52F1-PAT-FLD-NBR-1 (I1) TO PA52WS-USER-FLD
                   MOVE PA52WS-USER-FLD           TO DB-FIELD-KEY
                   PERFORM 840-FIND-HRUSET1
                   IF  (HRUSERFLDS-FOUND)
                       MOVE HRU-SEC-LEVEL         TO HRWS-SEC-LEVEL
                       PERFORM 730-HR-FIELD-SECURITY
                       IF (HRWS-FLD-SECURED)
                           MOVE 411               TO CRT-MSG-NBR
                           PERFORM 790-GET-MSG
                           MOVE CRT-MESSAGE
                                         TO PA52F1-PCT-PRE-VALUE-1 (I1)
                                            PA52WS-PCT-PRE-VALUE (I1)
                           GO TO 484-END
                       END-IF
                   END-IF
               ELSE
123800             MOVE PA52F1-PAT-FLD-NBR-1 (I1) TO DB-FLD-NBR
                   INITIALIZE                        DB-COUNTRY-CD-REQ
                                                     DB-PROCESS-LEVEL
123900             PERFORM 840-FIND-PASSET1
124000             IF (PASCRTY-FOUND)
124100                 MOVE PAS-SEC-LEVEL         TO HRWS-SEC-LEVEL
124200                 PERFORM 730-HR-FIELD-SECURITY
124300                 IF (HRWS-FLD-SECURED)
124400                     MOVE 411               TO CRT-MSG-NBR
124500                     PERFORM 790-GET-MSG
124600                     MOVE CRT-MESSAGE 
124600                                   TO PA52F1-PCT-PRE-VALUE-1 (I1)
                                            PA52WS-PCT-PRE-VALUE (I1)
125000                     GO TO 484-END
125100                 END-IF
125200             END-IF
               END-IF
125300         MOVE PA52F1-PAT-FLD-NBR-1 (I1) TO HREMP-FLD-NBR
125400         MOVE "Y"                       TO HREMP-FORMAT-FIELD
125500         MOVE PA52F1-PCT-COMPANY        TO HREMP-COMPANY
125600         MOVE PA52F1-PCT-EMPLOYEE       TO HREMP-EMPLOYEE
125700         MOVE SPACES                    TO HREMP-VALUE
125800         PERFORM 5000-HREMP-RETRIEVE-VALUE
125900         MOVE HREMP-VALUE        TO PA52F1-PCT-PRE-VALUE-1 (I1)
126200                                    PA52WS-PCT-PRE-VALUE (I1)
126300     ELSE
126400     IF (I1 > 24)
126500         COMPUTE I8 = (I1 - 24)
               IF  (PA52F1-PAT-FLD-NBR-3 (I8) = ZEROES)
                   MOVE SPACES         TO PA52F1-PCT-PRE-VALUE-3 (I8)
                                          PA52WS-PCT-PRE-VALUE (I1)
                   GO TO 484-END
               END-IF
               IF  (PA52F1-PAT-FLD-NBR-3 (I8) > 2000)
               AND (PA52F1-PAT-FLD-NBR-3 (I8) < 2100)
                   MOVE PA52F1-PAT-FLD-NBR-3 (I8) TO PA52WS-USER-FLD
                   MOVE PA52WS-USER-FLD           TO DB-FIELD-KEY
                   PERFORM 840-FIND-HRUSET1
                   IF  (HRUSERFLDS-FOUND)
                       MOVE HRU-SEC-LEVEL         TO HRWS-SEC-LEVEL
                       PERFORM 730-HR-FIELD-SECURITY
                       IF (HRWS-FLD-SECURED)
                           MOVE 411               TO CRT-MSG-NBR
                           PERFORM 790-GET-MSG
                           MOVE CRT-MESSAGE
                                         TO PA52F1-PCT-PRE-VALUE-3 (I8)
                                            PA52WS-PCT-PRE-VALUE (I1)
                           GO TO 484-END
                       END-IF
                   END-IF
               ELSE
126600             MOVE PA52F1-PAT-FLD-NBR-3 (I8) TO DB-FLD-NBR
                   INITIALIZE                        DB-COUNTRY-CD-REQ
                                                     DB-PROCESS-LEVEL
126700             PERFORM 840-FIND-PASSET1
126800             IF (PASCRTY-FOUND)
126900                 MOVE PAS-SEC-LEVEL         TO HRWS-SEC-LEVEL
127000                 PERFORM 730-HR-FIELD-SECURITY
127100                 IF (HRWS-FLD-SECURED)
127200                     MOVE 411               TO CRT-MSG-NBR
127300                     PERFORM 790-GET-MSG
127400                     MOVE CRT-MESSAGE 
127400                                   TO PA52F1-PCT-PRE-VALUE-3 (I8)
127700                                      PA52WS-PCT-PRE-VALUE (I1)
127800                     GO TO 484-END
127900                 END-IF
128000             END-IF
               END-IF
128100         MOVE PA52F1-PAT-FLD-NBR-3 (I8) TO HREMP-FLD-NBR
128200         MOVE "Y"                       TO HREMP-FORMAT-FIELD
128300         MOVE PA52F1-PCT-COMPANY        TO HREMP-COMPANY
128400         MOVE PA52F1-PCT-EMPLOYEE       TO HREMP-EMPLOYEE
128500         MOVE SPACES                    TO HREMP-VALUE
128600         PERFORM 5000-HREMP-RETRIEVE-VALUE
128700         MOVE HREMP-VALUE        TO PA52F1-PCT-PRE-VALUE-3 (I8)
129000                                    PA52WS-PCT-PRE-VALUE (I1)
129100     ELSE
129200     IF (I1 > 12)
129300         COMPUTE I8 = (I1 - 12)
               IF  (PA52F1-PAT-FLD-NBR-2 (I8) = ZEROES)
                   MOVE SPACES         TO PA52F1-PCT-PRE-VALUE-2 (I8)
                                          PA52WS-PCT-PRE-VALUE (I1)
                   GO TO 484-END
               END-IF
               IF  (PA52F1-PAT-FLD-NBR-2 (I8) > 2000)
               AND (PA52F1-PAT-FLD-NBR-2 (I8) < 2100)
                   MOVE PA52F1-PAT-FLD-NBR-2 (I8) TO PA52WS-USER-FLD
                   MOVE PA52WS-USER-FLD           TO DB-FIELD-KEY
                   PERFORM 840-FIND-HRUSET1
                   IF  (HRUSERFLDS-FOUND)
                       MOVE HRU-SEC-LEVEL         TO HRWS-SEC-LEVEL
                       PERFORM 730-HR-FIELD-SECURITY
                       IF (HRWS-FLD-SECURED)
                           MOVE 411               TO CRT-MSG-NBR
                           PERFORM 790-GET-MSG
                           MOVE CRT-MESSAGE
                                         TO PA52F1-PCT-PRE-VALUE-2 (I8)
                                            PA52WS-PCT-PRE-VALUE (I1)
                           GO TO 484-END
                       END-IF
                   END-IF
               ELSE
129400             MOVE PA52F1-PAT-FLD-NBR-2 (I8) TO DB-FLD-NBR
                   INITIALIZE                        DB-COUNTRY-CD-REQ
                                                     DB-PROCESS-LEVEL
129500             PERFORM 840-FIND-PASSET1
129600             IF (PASCRTY-FOUND)
129700                 MOVE PAS-SEC-LEVEL         TO HRWS-SEC-LEVEL
129800                 PERFORM 730-HR-FIELD-SECURITY
129900                 IF (HRWS-FLD-SECURED)
130000                     MOVE 411               TO CRT-MSG-NBR
130100                     PERFORM 790-GET-MSG
130200                     MOVE CRT-MESSAGE
130200                                   TO PA52F1-PCT-PRE-VALUE-2 (I8)
130500                                      PA52WS-PCT-PRE-VALUE (I1)
130600                     GO TO 484-END
130700                 END-IF
130800             END-IF
               END-IF
130900         MOVE PA52F1-PAT-FLD-NBR-2 (I8) TO HREMP-FLD-NBR
131000         MOVE "Y"                       TO HREMP-FORMAT-FIELD
131100         MOVE PA52F1-PCT-COMPANY        TO HREMP-COMPANY
131200         MOVE PA52F1-PCT-EMPLOYEE       TO HREMP-EMPLOYEE
131300         MOVE SPACES                    TO HREMP-VALUE
131400         PERFORM 5000-HREMP-RETRIEVE-VALUE
131500         MOVE HREMP-VALUE        TO PA52F1-PCT-PRE-VALUE-2 (I8)
131800                                    PA52WS-PCT-PRE-VALUE (I1).
131900
132000 484-END.
132100
132200******************************************************************
132300 500-MOVE-DATA.
132400******************************************************************
132500
132600     MOVE PA52F1-PCT-COMPANY              TO PCT-COMPANY.
132700     MOVE "E"                             TO PCT-ACTION-TYPE.
132800     MOVE PA52F1-PCT-ACTION-CODE          TO PCT-ACTION-CODE.
132900     MOVE PA52F1-PCT-EMPLOYEE             TO PCT-EMPLOYEE.
133000     MOVE PA52F1-PCT-EFFECT-DATE          TO PCT-EFFECT-DATE.
133100     MOVE PA52F1-PCT-ANT-END-DATE         TO PCT-ANT-END-DATE.
133200     MOVE PA52F1-PCT-REASON (1)           TO PCT-REASON (1).
133300     MOVE PA52F1-PCT-REASON (2)           TO PCT-REASON (2).
133400     MOVE PA52F1-PCT-PARTICIPNT           TO PCT-PARTICIPNT.
133500     MOVE PA52F1-PCT-OCCUR-TYPE           TO PCT-OCCUR-TYPE.
133600     MOVE PA52F1-PCT-UPDATE-BENEFIT       TO PCT-UPDATE-BENEFIT.
           MOVE PA52F1-PCT-UPD-ABS-MGMT         TO PCT-UPD-ABS-MGMT.
P86027     IF  (PA52F1-FC = "A")
               MOVE SPACES                      TO PCT-HOLD-FLAG.
133700     MOVE PA52WS-POS-EFFECT-DATE          TO PCT-POS-EFF-DT.
           IF  (PA52F1-USER-ID NOT = SPACES)
               MOVE PA52F1-USER-ID              TO PCT-USER-ID
           ELSE
               MOVE CRT-USER-NAME               TO PCT-USER-ID
           END-IF.
           MOVE PA52F1-PCT-UPDATE-REQ-DED       TO PCT-UPDATE-REQ-DED.
           MOVE PA52F1-PCT-EDM-EFFECT-DT        TO PCT-EDM-EFFECT-DT.
           MOVE PA52F1-PCT-EDM-END-DATE         TO PCT-EDM-END-DATE.
           MOVE PA52F1-PCT-HIST-CORR-FLAG       TO PCT-HIST-CORR-FLAG.
133900     MOVE 1                               TO PCT-POS-LEVEL.
           MOVE PA52F1-PCT-BASE-CURRENCY        TO PCT-BASE-CURRENCY.
           MOVE PA52F1-PCT-BASE-ND              TO PCT-BASE-ND.
           MOVE PA52F1-PCT-BASE-PAY-RATE        TO PCT-BASE-PAY-RATE.
           MOVE PA52F1-PCT-MERGE-ACTN-NBR       TO PCT-MERGE-ACTN-NBR.
           MOVE PA52F1-PCT-PROCESS-TYPE         TO PCT-PROCESS-TYPE.
           MOVE PA52F1-PCT-MOVE-FROM-LEVEL      TO PCT-POS-LEVEL-MOVE. 
J08104     IF  (PA52F1-FC = "A")
J08104         IF  (PA52F1-USER-ID NOT = SPACES)
J08104             MOVE PA52F1-USER-ID          TO PCT-CREATE-USER-ID
J08104         ELSE
J08104             MOVE CRT-USER-NAME           TO PCT-CREATE-USER-ID
J08104         END-IF
J08104         MOVE WS-SYSTEM-DATE-YMD          TO PCT-CREATE-DATE
J08104         MOVE HHMMSS                      TO PCT-CREATE-TIME
J08104     END-IF.
J08104     MOVE WS-SYSTEM-DATE-YMD              TO PCT-DATE-STAMP.
J08104     MOVE HHMMSS                          TO PCT-TIME-STAMP.
           IF (PA52F1-FC = "A")
P81486     AND (PA52F1-PCT-APPROVAL-FLAG NOT = "L")
               IF (PAT-WORKFLOW-FLAG           = "Y")
                   MOVE "N"                    TO PCT-APPROVAL-FLAG
                                               PA52F1-PCT-APPROVAL-FLAG
               ELSE
                   MOVE "Y"                    TO PCT-APPROVAL-FLAG
                                               PA52F1-PCT-APPROVAL-FLAG
               END-IF
           ELSE
               MOVE PA52F1-PCT-APPROVAL-FLAG   TO PCT-APPROVAL-FLAG
           END-IF.
134000
           PERFORM
               VARYING I8 FROM 1 BY 1
               UNTIL  (I8 > 12)

               COMPUTE I1 = (I8 +  0)
               MOVE PA52F1-PAT-FLD-NBR-1 (I8)   TO PCT-FLD-NBR (I1)
               MOVE PA52F1-PCT-NEW-VALUE-1 (I8) TO PCT-NEW-VALUE (I1)

               COMPUTE I1 = (I8 + 12)
               MOVE PA52F1-PAT-FLD-NBR-2 (I8)   TO PCT-FLD-NBR (I1)
               MOVE PA52F1-PCT-NEW-VALUE-2 (I8) TO PCT-NEW-VALUE (I1)

               COMPUTE I1 = (I8 + 24)
               MOVE PA52F1-PAT-FLD-NBR-3 (I8)   TO PCT-FLD-NBR (I1)
               MOVE PA52F1-PCT-NEW-VALUE-3 (I8) TO PCT-NEW-VALUE (I1)
           END-PERFORM.

           INITIALIZE PCT-SALARY-CLASS
                      PCT-PCT-PAY-INC
                      PCT-PCT-PAY-DEC
                      PCT-ROUND-OPT
                      PCT-FLAT-INC
                      PCT-FLAT-DEC
                      PCT-NEW-RATE
                      PCT-ROUND-EVEN
                      PCT-POSITION
                      PCT-PAY-POSITION
                      PCT-PROCESS-LEVEL
                      PCT-DEPARTMENT
                      PCT-JOB-CODE.

      * SEARCH FOR POSITION-DEFAULTABLE FIELDS INCLUDED IN ACTION
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36)
               IF  (PCT-NEW-VALUE (I1) NOT = SPACES)
                   IF  (PCT-FLD-NBR (I1) = HRPEP-END-DATE-DN)
                   OR  (PCT-FLD-NBR (I1) = HREMP-POSITION-DN)
                       GO TO 500-CONTINUE
                   END-IF
                   PERFORM
                       VARYING I8 FROM 1 BY 1
                       UNTIL  (I8 > PAPOSEMP-TABLE-SIZE)
                       IF  (PCT-FLD-NBR (I1) = PAPOSEMP-EMP-FLD (I8))
                           GO TO 500-CONTINUE
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.

      * IF CONTROL REACHES HERE, THIS PCT DOES NOT AFFECT PEP INFO
           GO TO 500-END.

       500-CONTINUE.

           IF  (PA52WS-RETRO)
               MOVE PA52WS-RETRO-POSITION       TO PCT-PAY-POSITION
               MOVE PA52WS-RETRO-JOB-CODE       TO PCT-JOB-CODE
               MOVE PA52WS-RETRO-PL             TO PCT-PROCESS-LEVEL
               MOVE PA52WS-RETRO-DEPARTMENT     TO PCT-DEPARTMENT
           ELSE
               MOVE PA52WS-POSITION             TO PCT-PAY-POSITION
               MOVE PA52WS-JOB-CODE             TO PCT-JOB-CODE
               MOVE PA52WS-PL                   TO PCT-PROCESS-LEVEL
               MOVE PA52WS-DEPARTMENT           TO PCT-DEPARTMENT
           END-IF.
           MOVE PA52WS-POSITION                 TO PCT-POSITION.

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36)

               IF (PCT-NEW-VALUE (I1) NOT = SPACES)
                   IF (PCT-FLD-NBR (I1) = HREMP-POSITION-DN)
                       IF  (PCT-NEW-VALUE (I1) = "*BLANK")
                           MOVE SPACES             TO PCT-POSITION
                                                      PCT-PAY-POSITION
                       ELSE
                           MOVE PCT-NEW-VALUE (I1) TO PCT-POSITION
                                                      PCT-PAY-POSITION
                       END-IF
                   END-IF
                   IF (PCT-FLD-NBR (I1) = HREMP-JOB-CODE-DN)
                       IF  (PCT-NEW-VALUE (I1) = "*BLANK")
                           MOVE SPACES             TO PCT-JOB-CODE
                       ELSE
                           MOVE PCT-NEW-VALUE (I1) TO PCT-JOB-CODE
                       END-IF
                   END-IF
                   IF (PCT-FLD-NBR (I1) = HREMP-PROCESS-LEVEL-DN)
                       IF  (PCT-NEW-VALUE (I1) = "*BLANK")
                           MOVE SPACES             TO PCT-PROCESS-LEVEL
                       ELSE
                           MOVE PCT-NEW-VALUE (I1) TO PCT-PROCESS-LEVEL
                       END-IF
                   END-IF
                   IF (PCT-FLD-NBR (I1) = HREMP-DEPARTMENT-DN)
                       IF  (PCT-NEW-VALUE (I1) = "*BLANK")
                           MOVE SPACES             TO PCT-DEPARTMENT
                       ELSE
                           MOVE PCT-NEW-VALUE (I1) TO PCT-DEPARTMENT
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

       500-END.
154000******************************************************************
154100 600-UPDATE-EMPLOYEE.
154200******************************************************************
154300
155100     MOVE PA52F1-PCT-EFFECT-DATE     TO HREMP-EFFECT-DATE.
155200     MOVE PA52F1-PCT-ACTION-CODE     TO HREMP-ACTION-CODE.
155300     MOVE PA52WS-ACTION-NBR          TO HREMP-ACTION-NBR.
155400     MOVE PA52F1-PCT-REASON (1)      TO HREMP-REASON1.
155500     MOVE PA52F1-PCT-REASON (2)      TO HREMP-REASON2.
           IF  (PA52F1-USER-ID NOT = SPACES)
               MOVE PA52F1-USER-ID         TO HREMP-USER-ID
           ELSE
               MOVE CRT-USER-NAME          TO HREMP-USER-ID
           END-IF.
155700     MOVE PA52F1-PCT-ANT-END-DATE    TO HREMP-ANT-END-DATE.
155800     MOVE PA52WS-OBJ-ID              TO HREMP-ACT-OBJ-ID.
155900     MOVE PA52F1-PCT-UPDATE-BENEFIT  TO HREMP-UPDATE-BENEFIT.
155900     MOVE PA52F1-PCT-UPD-ABS-MGMT    TO HREMP-UPDATE-ABSENCE-MGMT.
           MOVE "N"                        TO HRWS-ADD.
           SET HREMP-UPDATE-M3             TO TRUE.
156000     PERFORM 3000-HREMP-PROCESS-TRAN.
156100
156200     PERFORM 620-DO-USER-FIELDS
156300     THRU    620-END.
156400
           IF (PA52F1-PCT-UPDATE-REQ-DED     = "Y")
467300         MOVE PA52F1-PCT-COMPANY       TO PRRQC-COMPANY
467400         MOVE PA52F1-PCT-EMPLOYEE      TO PRRQC-EMPLOYEE
467500         INITIALIZE PRRQC-DFT-MAR-STAT
467700                    PRRQC-DFT-EXEMPTS
               MOVE PA52F1-PCT-EDM-EFFECT-DT TO PRRQC-EFFECT-DATE
               MOVE PA52F1-PCT-EDM-END-DATE  TO PRRQC-END-DATE
               INITIALIZE PRRQC-UPDATE-OPTION
467800         PERFORM 500-REQ-DED-CREATION.
467900
156500 600-END.
156600******************************************************************
156700 610-CREATE-PARTICIPNT.
156800******************************************************************
156900
157000     IF  (PA52F1-PCT-PARTICIPNT = ZEROS)
157100     AND (PA52WS-PARTICIPNT     = ZEROS)
157200         GO TO 610-END.
157300
157400     IF (PA52WS-PARTICIPNT NOT = ZEROS)
157500         MOVE PA52WS-PARTICIPNT  TO PA52WS-LAST-PART
157600     ELSE
157700         IF (PA52F1-PCT-PARTICIPNT > PA52WS-LAST-PART)
157800             MOVE PA52F1-PCT-PARTICIPNT TO PA52WS-LAST-PART.
157900
           IF  (PA52F1-PCT-EFFECT-DATE NOT = ZEROES)
               MOVE PA52F1-PCT-EFFECT-DATE TO PA52WS-DATE
           ELSE
158000         MOVE WS-SYSTEM-DATE-YMD     TO PA52WS-DATE.
158100
           MOVE PA52WS-MONTH               TO WS-MONTH.
158200     COMPUTE WS-MONTH            = WS-MONTH      
158300                                 + PA52WS-OCC-MONTHS-EXT.
158400
158500     PERFORM
158600         UNTIL (WS-MONTH < 13)
158700
158800         SUBTRACT 12             FROM WS-MONTH
158900         ADD 1                   TO PA52WS-YEAR
159000     END-PERFORM.
159100
           MOVE WS-MONTH               TO PA52WS-MONTH.

           MOVE PA52WS-DATE            TO WSDR-FR-DATE.
           PERFORM 900-IS-DATE-INVALID.
           IF (WSDR-DATE-ERROR-EXISTS)
               PERFORM 5000-SET-TO-EOM.

           PERFORM 900-DATE-TO-JULIAN.
           SUBTRACT 1                  FROM WSDR-JULIAN-DAYS.
           PERFORM 900-JULIAN-TO-DATE.
           MOVE WSDR-FR-DATE           TO PA52WS-DATE.
159500
160100     IF (PA52WS-PARTICIPNT NOT = ZEROS)
160200         MOVE PA52WS-PARTICIPNT  TO PA52F1-PCT-PARTICIPNT.
160300
160400     PERFORM 800-CREATE-PARTICIPNT.
160500     MOVE PA52F1-PCT-COMPANY     TO PAR-COMPANY.
160600     MOVE PA52F1-PCT-PARTICIPNT  TO PAR-PARTICIPNT.
160700     MOVE EMP-LAST-NAME          TO PAR-LAST-NAME.
160800     MOVE EMP-FIRST-NAME         TO PAR-FIRST-NAME.
160900     MOVE EMP-MIDDLE-INIT        TO PAR-MIDDLE-INIT.
161000     MOVE EMP-EMPLOYEE           TO PAR-EMPLOYEE.
161100     MOVE EMP-FICA-NBR           TO PAR-FICA-NBR.
161200     MOVE EMP-ADDR1              TO PAR-ADDR1.
161300     MOVE EMP-ADDR2              TO PAR-ADDR2.
161400     MOVE EMP-ADDR3              TO PAR-ADDR3.
161500     MOVE EMP-ADDR4              TO PAR-ADDR4.
161600     MOVE EMP-CITY               TO PAR-CITY.
161700     MOVE EMP-STATE              TO PAR-STATE.
161800     MOVE EMP-ZIP                TO PAR-ZIP.
161900     MOVE EMP-COUNTRY-CODE       TO PAR-COUNTRY-CODE.
162000     MOVE PEM-HM-PHONE-CNTRY     TO PAR-HM-PHONE-CNTRY.
162100     MOVE PEM-HM-PHONE-NBR       TO PAR-HM-PHONE-NBR.
162200     MOVE PEM-BIRTHDATE          TO PAR-BIRTHDATE.
162300     MOVE PEM-SEX                TO PAR-SEX.
162400     MOVE PEM-SMOKER             TO PAR-SMOKER.
162500     MOVE PA52F1-PCT-OCCUR-TYPE  TO PAR-OCCUR-TYPE.
162600     MOVE PA52F1-PCT-EFFECT-DATE TO PAR-OCCUR-DATE.
162700     MOVE PA52WS-DATE            TO PAR-TERM-DATE.
J67329     MOVE WS-SYSTEM-DATE-YMD     TO PAR-DATE-STAMP.
J67329     MOVE HHMMSS                 TO PAR-TIME-STAMP.
J67329     MOVE CRT-USER-NAME          TO PAR-USER-ID.
J67329     MOVE WS-SYSTEM-DATE-YMD     TO PAR-CREATE-DATE.
J67329     MOVE HHMMSS                 TO PAR-CREATE-TIME.
J67329     MOVE CRT-USER-NAME          TO PAR-CREATE-USER-ID.           
J18332     MOVE WS-SYSTEM-DATE-YMD     TO PAR-CREATE-DATE
J18332                                    PAR-DATE-STAMP.
J18332     MOVE HHMMSS                 TO PAR-CREATE-TIME
J18332                                    PAR-TIME-STAMP.
J18332     MOVE CRT-USER-NAME          TO PAR-CREATE-USER-ID
J18332                                    PAR-USER-ID.
162800     PERFORM 820-STORE-PARTICIPNT.
162900
163000     PERFORM 840-MODIFY-BNCSET1.
163100     MOVE PA52WS-LAST-PART       TO BNC-LAST-PART.
163200     PERFORM 820-STORE-BNCOMPANY.
163300
163400 610-END.
163500
163600******************************************************************
163700 620-DO-USER-FIELDS.
163800******************************************************************
163900
           INITIALIZE HRWS-FIELD-NBR-TABLE
                      I2.

164000     PERFORM
164100         VARYING I1 FROM 1 BY 1
164200         UNTIL  (I1 > 36)
164300
164400         IF  (PA52WS-PAT-FIELD-NBR (I1) NOT < 2000)
164500         AND (PA52WS-PAT-FIELD-NBR (I1) NOT > 2099)
164600         AND (PA52WS-PCT-NEW-VALUE (I1) NOT = SPACES)
164700             INITIALIZE PAPCT-SCR-FIELDS
164800             MOVE PA52WS-PAT-FIELD-NBR (I1) TO PAPCT-FLD-NBR
164900             MOVE PA52WS-PCT-NEW-VALUE (I1) TO PAPCT-NEW-VALUE
165000             MOVE PA52F1-PCT-COMPANY        TO PAPCT-COMPANY
165100             MOVE PA52F1-PCT-EMPLOYEE       TO PAPCT-EMPLOYEE
165200             MOVE PA52F1-PCT-EFFECT-DATE    TO PAPCT-EFFECT-DATE
165300             MOVE PA52F1-PCT-ACTION-CODE    TO PAPCT-ACTION-CODE
165400             MOVE PA52WS-ACTION-NBR         TO PAPCT-ACTION-NBR
165500             MOVE PA52F1-PCT-REASON (1)     TO PAPCT-REASON1
165600             MOVE PA52F1-PCT-REASON (2)     TO PAPCT-REASON2
                   IF  (PA52F1-USER-ID NOT = SPACES)
                       MOVE PA52F1-USER-ID        TO PAPCT-USER-ID
                   ELSE
                       MOVE CRT-USER-NAME         TO PAPCT-USER-ID
                   END-IF
165800             MOVE PA52F1-PCT-ANT-END-DATE   TO PAPCT-ANT-END-DATE
165900             PERFORM 5100-EDIT-USER-FIELDS
166000             PERFORM 3000-HRHEU-PROCESS-TRAN

                   MOVE PA52F1-PCT-COMPANY        TO DB-COMPANY
                   MOVE PA52WS-PAT-FIELD-NBR (I1) TO DB-FLD-NBR
                   INITIALIZE                        DB-COUNTRY-CD-REQ
                                                     DB-PROCESS-LEVEL
                   PERFORM 840-FIND-PASSET1
                   IF  (PASCRTY-FOUND)
                   AND (PAS-USED-BY-GROUP         = "X")
                       ADD 1                      TO I2
                       MOVE DB-FLD-NBR            TO HRWS-FIELD-NBR (I2)
                   END-IF
166100         END-IF
166200     END-PERFORM.
166300
           IF (I2                          NOT = ZEROES)
               MOVE WS-FALSE               TO HRWS-ALL-EMPLOYEES-SW
               MOVE PA52F1-PCT-COMPANY     TO HRWS-COMPANY
               MOVE PA52F1-PCT-EFFECT-DATE TO HRWS-EFFECT-DATE
               MOVE PA52WS-OBJ-ID          TO HRWS-ACT-OBJ-ID
               MOVE PA52F1-PCT-UPDATE-BENEFIT
                                           TO HRWS-UPDATE-BENEFIT
               MOVE PA52F1-PCT-UPD-ABS-MGMT
                                           TO HRWS-UPDATE-ABSENCE-MGMT
J63431         MOVE PA52F1-PCT-PROCESS-TYPE 
                                           TO HREMP-PROCESS-TYPE-SW                                           
               PERFORM 5000-REBUILD-PERSONNEL-GROUP.

166400 620-END.
468800******************************************************************
468900 650-DEFAULT.
469000******************************************************************
           MOVE 0                              TO PA52WS-DEFAULT-SW.
           MOVE 0                              TO I1.          
           PERFORM 
           UNTIL (PA52WS-DEFAULT-OK)
           OR    (I1 = 12)
              ADD 1                            TO I1
               IF  (PA52F1-PAT-FLD-NBR-1 (I1) = HREMP-POSITION-DN)
                    IF (PA52F1-PCT-NEW-VALUE-1 (I1)  NOT = SPACES)
                        MOVE WS-TRUE              TO PA52WS-DEFAULT-SW
                        MOVE PA52F1-PAT-FLD-NBR-1 (I1) 
                                                  TO PADFP-POSITION
                    ELSE  
469400                  MOVE 112                TO CRT-ERROR-NBR
469500                  MOVE PA52F1-FC-FN       TO CRT-FIELD-NBR
469600                  GO TO 650-END
                    END-IF
               ELSE     
               IF  (PA52F1-PAT-FLD-NBR-2 (I1)  = HREMP-POSITION-DN)
                    IF (PA52F1-PCT-NEW-VALUE-2 (I1) NOT = SPACES)
                        MOVE WS-TRUE              TO PA52WS-DEFAULT-SW
                        MOVE PA52F1-PAT-FLD-NBR-1 (I1) 
                                                  TO PADFP-POSITION
                    ELSE  
469400                  MOVE 112                TO CRT-ERROR-NBR
469500                  MOVE PA52F1-FC-FN       TO CRT-FIELD-NBR
469600                  GO TO 650-END
                    END-IF
               ELSE
               IF  (PA52F1-PAT-FLD-NBR-3 (I1)  = HREMP-POSITION-DN)
                    IF (PA52F1-PCT-NEW-VALUE-3 (I1) NOT = SPACES)
                        MOVE WS-TRUE              TO PA52WS-DEFAULT-SW
                        MOVE PA52F1-PAT-FLD-NBR-1 (I1) 
                                                  TO PADFP-POSITION
                    ELSE 
469400                  MOVE 112                TO CRT-ERROR-NBR
469500                  MOVE PA52F1-FC-FN       TO CRT-FIELD-NBR
469600                  GO TO 650-END
                    END-IF.
           IF  (PA52WS-DEFAULT-SW = 0)
469400          MOVE 112                TO CRT-ERROR-NBR
469500          MOVE PA52F1-FC-FN       TO CRT-FIELD-NBR
469600          GO TO 650-END.
           IF  (PA52F1-PCT-EFFECT-DATE = ZEROES)
                MOVE 106                TO CRT-ERROR-NBR
                MOVE PA52F1-PCT-EFFECT-DATE-FN TO CRT-FIELD-NBR
                GO TO 650-END.
 
           PERFORM 725-DEFAULT-ACTION
           THRU    725-END.

           IF (ERROR-FOUND)
               GO TO 650-END 
           ELSE
               MOVE "PA52"                TO CRT-ERROR-CAT.

476800     MOVE 109                       TO CRT-MSG-NBR.
476900     MOVE PA52F1-FC-FN              TO CRT-FIELD-NBR.
477000
       650-END.
477300******************************************************************
477400 700-MOVE-LEVEL.    
477500******************************************************************
477600
           IF (PA52F1-PCT-MOVE-FROM-LEVEL = ZEROES)
               MOVE 156                    TO CRT-ERROR-NBR
               MOVE PA52F1-PCT-MOVE-FROM-LEVEL-FN
                                           TO CRT-FIELD-NBR
               GO TO 700-END.

           PERFORM
               VARYING I9 FROM 1 BY 1
               UNTIL  (I9 > PAPOSPEP-TABLE-SIZE)
               OR     (ERROR-FOUND)
                   MOVE PAPOSPEP-EMP-FLD (I9)  TO DB-FLD-NBR
                   INITIALIZE                     DB-COUNTRY-CD-REQ
                                                  DB-PROCESS-LEVEL
                   PERFORM 840-FIND-PASSET1
                   IF (PASCRTY-FOUND)
                       MOVE PAS-SEC-LEVEL      TO HRWS-SEC-LEVEL
                       PERFORM 730-HR-FIELD-SECURITY
                       IF (HRWS-FLD-SECURED)
                           MOVE 157            TO CRT-ERROR-NBR
                           MOVE PA52F1-FC-FN   TO CRT-FIELD-NBR
                           INITIALIZE          PA52F1-PCT-PROCESS-TYPE
                                              PA52F1-PCT-MOVE-FROM-LEVEL
                           GO TO 700-END
                       END-IF
                   END-IF
           END-PERFORM.

           INITIALIZE                         PADFP-FIELDS.

           PERFORM 775-LOAD-PADFP-FIELDS
           THRU    775-END.

           PERFORM 2000-PADFPEP-EDIT-DEFAULTS.

           PERFORM 780-SAVE-PADFP-FIELDS
           THRU    780-END.

           SET PA52WS-MOVELEVELDONE        TO TRUE.

           MOVE 109                            TO CRT-MSG-NBR.
           MOVE PA52F1-FC-FN                   TO CRT-FIELD-NBR.

       700-END.

477300******************************************************************
477400 725-DEFAULT-ACTION.
477500******************************************************************
477600
           SET PA52WS-USE-COMPANY          TO TRUE.
           IF (PA52WS-POSITION NOT = SPACES)
               MOVE PA52F1-PCT-COMPANY     TO DB-COMPANY
               MOVE PA52WS-POSITION        TO DB-POSITION
               MOVE PA52F1-PCT-EFFECT-DATE TO DB-EFFECT-DATE
               PERFORM 850-FIND-NLT-POSSET2
               IF  (PAPOSITION-FOUND)
               AND (POS-COMPANY           = DB-COMPANY)
               AND (POS-POSITION          = DB-POSITION)
               AND (POS-PROCESS-LEVEL NOT = SPACES)
                   MOVE "PO"               TO DB-TOPIC
                   MOVE SPACES             TO DB-COUNTRY-CD-REQ
                   MOVE POS-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
                   MOVE PASSET2-PROCESS-LEVEL
                                           TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-PASSET2
                   IF (PASCRTY-FOUND)
                       SET PA52WS-USE-PROCESS-LEVEL
                                           TO TRUE.

478200     PERFORM 750-CHECK-SECURITY
478300     THRU    750-END.

478800     IF (ERROR-FOUND)
478900         GO TO 725-END.
479000
           INITIALIZE                         PADFP-FIELDS.

479400     PERFORM 775-LOAD-PADFP-FIELDS
479500     THRU    775-END.
          
           PERFORM 2000-EDIT-DEFAULTS.
           IF (ERROR-FOUND)
              GO TO 725-END.
          
           PERFORM 780-SAVE-PADFP-FIELDS
           THRU 780-END.
479600
       725-END.
480500******************************************************************
480600 750-CHECK-SECURITY.
480700******************************************************************

           MOVE "PO"                       TO DB-TOPIC.
           PERFORM
               VARYING I9 FROM 1 BY 1
               UNTIL  (I9 > PAPOSEMP-TABLE-SIZE)
               OR     (ERROR-FOUND)

               MOVE PAPOSEMP-POS-FLD (I9)  TO DB-FLD-NBR
               INITIALIZE                     DB-COUNTRY-CD-REQ
               IF (PA52WS-USE-PROCESS-LEVEL)
                   MOVE POS-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
               ELSE
                   MOVE SPACES             TO DB-PROCESS-LEVEL
               END-IF
               PERFORM 840-FIND-PASSET2
               IF  (PASCRTY-FOUND)
               AND (PAS-DEFAULT-FLG > 1)
                   MOVE PAPOSEMP-EMP-FLD (I9)  TO DB-FLD-NBR
                   INITIALIZE                     DB-COUNTRY-CD-REQ
                   MOVE SPACES                 TO DB-PROCESS-LEVEL
                   PERFORM 840-FIND-PASSET1
                   IF  (PASCRTY-FOUND)
                       MOVE PAS-SEC-LEVEL          TO HRWS-SEC-LEVEL
                       PERFORM 730-HR-FIELD-SECURITY
                       IF  (HRWS-FLD-SECURED)
                           MOVE 107                    TO CRT-ERROR-NBR
                           MOVE PA52F1-FC-FN           TO CRT-FIELD-NBR.

       750-END.
482300******************************************************************
482400 775-LOAD-PADFP-FIELDS.
482500******************************************************************
482600
482700     MOVE PA52F1-FC-FN               TO PADFP-FC-FN.
482800     MOVE WS-TRUE                    TO PADFP-LVL1-CHG-SW.
482900     MOVE PA52F1-PCT-COMPANY         TO PADFP-COMPANY.
           MOVE PA52F1-PCT-MOVE-FROM-LEVEL TO PADFP-POS-LEVEL.
           MOVE PA52F1-PCT-EMPLOYEE        TO PADFP-EMPLOYEE.
483000     MOVE PA52F1-PCT-EFFECT-DATE     TO PADFP-EFFECT-DATE.
           INITIALIZE                         PADFP-END-DATE.
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 12)

               MOVE PA52F1-PCT-PRE-VALUE-1 (I1)    TO PA52WS-VALUE
               MOVE PA52F1-PCT-NEW-VALUE-1-FN (I1) TO PA52WS-VALUE-FN
               MOVE PA52F1-PAT-FLD-NBR-1 (I1)      TO PA52WS-FLD-NBR
               PERFORM 776-LOAD-PADFP-FIELD
               THRU    776-END

               MOVE PA52F1-PCT-PRE-VALUE-2 (I1)    TO PA52WS-VALUE
               MOVE PA52F1-PCT-NEW-VALUE-2-FN (I1) TO PA52WS-VALUE-FN
               MOVE PA52F1-PAT-FLD-NBR-2 (I1)      TO PA52WS-FLD-NBR
               PERFORM 776-LOAD-PADFP-FIELD
               THRU    776-END

               MOVE PA52F1-PCT-PRE-VALUE-3 (I1)    TO PA52WS-VALUE
               MOVE PA52F1-PCT-NEW-VALUE-3-FN (I1) TO PA52WS-VALUE-FN
               MOVE PA52F1-PAT-FLD-NBR-3 (I1)      TO PA52WS-FLD-NBR
               PERFORM 776-LOAD-PADFP-FIELD
               THRU    776-END

           END-PERFORM.

491300 775-END.
      ******************************************************************
       776-LOAD-PADFP-FIELD.
      ******************************************************************

           IF (PA52F1-PCT-MOVE-FROM-LEVEL = ZEROES)
               IF (PA52WS-FLD-NBR         = HREMP-SALARY-CLASS-DN)
                   MOVE PA52WS-VALUE       TO PADFP-SALARY-CLASS
                   MOVE PA52WS-VALUE-FN    TO PADFP-SALARY-CLASS-FN
               END-IF
               IF (PA52WS-FLD-NBR         = HREMP-SEC-LVL-DN)
                   MOVE PA52WS-VALUE       TO PAPCT-FIELD
                   PERFORM 5200-CONVERT-NUMERIC-FIELD
                   MOVE PAPCT-NUMERIC      TO PADFP-SEC-LVL
                   MOVE PA52WS-VALUE-FN    TO PADFP-SEC-LVL-FN
               END-IF
               IF (PA52WS-FLD-NBR         = HREMP-SEC-LOCATION-DN)
                   MOVE PA52WS-VALUE       TO PADFP-SEC-LOCATION
                   MOVE PA52WS-VALUE-FN    TO PADFP-SEC-LOCATION-FN
               END-IF
               IF (PA52WS-FLD-NBR         = HREMP-EXEMPT-EMP-DN)
                   MOVE PA52WS-VALUE       TO PADFP-EXEMPT-EMP
                   MOVE PA52WS-VALUE-FN    TO PADFP-EXEMPT-EMP-FN
               END-IF
               IF (PA52WS-FLD-NBR         = HREMP-PAY-FREQUENCY-DN)
                   MOVE PA52WS-VALUE       TO PAPCT-FIELD
                   PERFORM 5200-CONVERT-NUMERIC-FIELD
                   MOVE PAPCT-NUMERIC      TO PADFP-PAY-FREQUENCY
                   MOVE PA52WS-VALUE-FN    TO PADFP-PAY-FREQUENCY-FN
               END-IF
               IF (PA52WS-FLD-NBR         = HREMP-OT-PLAN-CODE-DN)
                   MOVE PA52WS-VALUE       TO PADFP-OT-PLAN-CODE
                   MOVE PA52WS-VALUE-FN    TO PADFP-OT-PLAN-CODE-FN
               END-IF
           ELSE
               IF (PA52WS-FLD-NBR = HREMP-POSITION-DN)
                   MOVE PA52WS-VALUE       TO PADFP-POSITION
                   MOVE PA52WS-VALUE-FN    TO PADFP-POSITION-FN
               END-IF
           END-IF.
               
      *  You need to move the new value for position, all others
      *  need to use the old value
           IF (PA52F1-PCT-MOVE-FROM-LEVEL = ZEROES)
               IF ( PA52F1-PAT-FLD-NBR-1 (I1) = HREMP-POSITION-DN)
                   MOVE PA52F1-PCT-NEW-VALUE-1 (I1) TO PADFP-POSITION 
                   MOVE PA52WS-VALUE-FN    TO PADFP-POSITION-FN
               ELSE
               IF ( PA52F1-PAT-FLD-NBR-2 (I1) = HREMP-POSITION-DN)
                   MOVE PA52F1-PCT-NEW-VALUE-2 (I1) TO PADFP-POSITION 
                   MOVE PA52WS-VALUE-FN    TO PADFP-POSITION-FN
               ELSE
               IF ( PA52F1-PAT-FLD-NBR-3 (I1) = HREMP-POSITION-DN)
                   MOVE PA52F1-PCT-NEW-VALUE-3 (I1) TO PADFP-POSITION 
                   MOVE PA52WS-VALUE-FN    TO PADFP-POSITION-FN
               END-IF
               END-IF
               END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-JOB-CODE-DN)
               MOVE PA52WS-VALUE           TO PADFP-JOB-CODE
               MOVE PA52WS-VALUE-FN        TO PADFP-JOB-CODE-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PROCESS-LEVEL-DN)
               MOVE PA52WS-VALUE           TO PADFP-PROCESS-LEVEL
               MOVE PA52WS-VALUE-FN        TO PADFP-PROCESS-LEVEL-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-DEPARTMENT-DN)
               MOVE PA52WS-VALUE           TO PADFP-DEPARTMENT
               MOVE PA52WS-VALUE-FN        TO PADFP-DEPARTMENT-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-USER-LEVEL-DN)
               MOVE PA52WS-VALUE           TO PADFP-USER-LEVEL
               MOVE PA52WS-VALUE-FN        TO PADFP-USER-LEVEL-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-ANNUAL-HOURS-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC          TO PADFP-ANNUAL-HOURS
J46844         MOVE PAPCT-4-DECIMALS       TO PADFP-ANNUAL-HOURS             
               MOVE PA52WS-VALUE-FN        TO PADFP-ANNUAL-HOURS-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SCHEDULE-DN)
               MOVE PA52WS-VALUE           TO PADFP-SCHEDULE
               MOVE PA52WS-VALUE-FN        TO PADFP-SCHEDULE-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PAY-GRADE-DN)
               MOVE PA52WS-VALUE           TO PADFP-PAY-GRADE
               MOVE PA52WS-VALUE-FN        TO PADFP-PAY-GRADE-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PAY-STEP-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC          TO PADFP-PAY-STEP
               MOVE PA52WS-VALUE-FN        TO PADFP-PAY-STEP-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SUPERVISOR-DN)
               MOVE PA52WS-VALUE           TO PADFP-SUPERVISOR
               MOVE PA52WS-VALUE-FN        TO PADFP-SUPERVISOR-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SUPERVISOR-IND-DN)
               MOVE PA52WS-VALUE           TO PADFP-SUPERVISOR-IND
               MOVE PA52WS-VALUE-FN        TO PADFP-SUPERVISOR-IND-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SHIFT-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC          TO PADFP-SHIFT
               MOVE PA52WS-VALUE-FN        TO PADFP-SHIFT-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-WORK-SCHED-DN)
               MOVE PA52WS-VALUE           TO PADFP-WORK-SCHED
               MOVE PA52WS-VALUE-FN        TO PADFP-WORK-SCHED-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HRPEM-LOCAT-CODE-DN)
               MOVE PA52WS-VALUE           TO PADFP-LOCAT-CODE
               MOVE PA52WS-VALUE-FN        TO PADFP-LOCAT-CODE-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-NBR-FTE-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-6-DECIMALS       TO PADFP-FTE
               MOVE PA52WS-VALUE-FN        TO PADFP-FTE-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-HM-DIST-CO-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC          TO PADFP-EXP-COMPANY
               MOVE PA52WS-VALUE-FN        TO PADFP-EXP-COMPANY-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-HM-ACCT-UNIT-DN)
               MOVE PA52WS-VALUE           TO PADFP-EXP-ACCT-UNIT
               MOVE PA52WS-VALUE-FN        TO PADFP-EXP-ACCT-UNIT-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-HM-ACCOUNT-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC          TO PADFP-EXP-ACCOUNT
               MOVE PA52WS-VALUE-FN        TO PADFP-EXP-ACCOUNT-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-HM-SUB-ACCT-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC          TO PADFP-EXP-SUB-ACCT
               MOVE PA52WS-VALUE-FN        TO PADFP-EXP-SUB-ACCT-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PAY-RATE-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-4-DECIMALS       TO PADFP-PAY-RATE
               MOVE PA52WS-VALUE-FN        TO PADFP-PAY-RATE-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-ACTIVITY-DN)
               MOVE PA52WS-VALUE           TO PADFP-ACTIVITY
               MOVE PA52WS-VALUE-FN        TO PADFP-ACTIVITY-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-ACCT-CATEGORY-DN)
               MOVE PA52WS-VALUE           TO PADFP-ACCT-CATEGORY
               MOVE PA52WS-VALUE-FN        TO PADFP-ACCT-CATEGORY-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-OT-PLAN-CODE-DN)
               MOVE PA52WS-VALUE           TO PADFP-OT-PLAN-CODE
               MOVE PA52WS-VALUE-FN        TO PADFP-OT-PLAN-CODE-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-CURRENCY-CODE-DN)
               MOVE PA52WS-VALUE           TO PADFP-CURRENCY-CODE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-UNION-CODE-DN)
               MOVE PA52WS-VALUE           TO PADFP-UNION-CODE
               MOVE PA52WS-VALUE-FN        TO PADFP-UNION-CODE-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HRPEM-BARGAIN-UNIT-DN)
               MOVE PA52WS-VALUE           TO PADFP-BARGAIN-UNIT
               MOVE PA52WS-VALUE-FN        TO PADFP-BARGAIN-UNIT-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HRPEM-USER-AMOUNT-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
P52707         MOVE PAPCT-4-DECIMALS       TO PADFP-USER-AMOUNT
               MOVE PA52WS-VALUE-FN        TO PADFP-USER-AMOUNT-FN
           END-IF.

       776-END.

      ******************************************************************
       780-SAVE-PADFP-FIELDS.
      ******************************************************************

           MOVE PA52F1-FC-FN               TO PADFP-FC-FN.
           MOVE WS-TRUE                    TO PADFP-LVL1-CHG-SW.
           MOVE PA52F1-PCT-COMPANY         TO PADFP-COMPANY.
           MOVE PA52F1-PCT-EFFECT-DATE     TO PADFP-EFFECT-DATE.
           INITIALIZE                         PADFP-END-DATE.
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 12)

               MOVE PA52F1-PCT-NEW-VALUE-1 (I1)  TO PA52WS-VALUE
               MOVE PA52F1-PCT-PRE-VALUE-1 (I1)  TO PA52WS-PRE-VALUE
               MOVE PA52F1-PAT-FLD-NBR-1 (I1)    TO PA52WS-FLD-NBR
J28956         MOVE PA52WS-FILLED-BY-LTM-1 (I1)  TO PA52WS-LTM-FILLED-SW
               PERFORM 781-SAVE-PADFP-FIELD
               THRU    781-END
               MOVE PA52WS-VALUE       TO PA52F1-PCT-NEW-VALUE-1 (I1)

               MOVE PA52F1-PCT-NEW-VALUE-2 (I1)  TO PA52WS-VALUE
               MOVE PA52F1-PCT-PRE-VALUE-2 (I1)  TO PA52WS-PRE-VALUE
               MOVE PA52F1-PAT-FLD-NBR-2 (I1)    TO PA52WS-FLD-NBR
J28956         MOVE PA52WS-FILLED-BY-LTM-2 (I1)  TO PA52WS-LTM-FILLED-SW
               PERFORM 781-SAVE-PADFP-FIELD
               THRU    781-END                              
               MOVE PA52WS-VALUE       TO PA52F1-PCT-NEW-VALUE-2 (I1)

               MOVE PA52F1-PCT-NEW-VALUE-3 (I1)  TO PA52WS-VALUE
               MOVE PA52F1-PCT-PRE-VALUE-3 (I1)  TO PA52WS-PRE-VALUE
               MOVE PA52F1-PAT-FLD-NBR-3 (I1)    TO PA52WS-FLD-NBR
J28956         MOVE PA52WS-FILLED-BY-LTM-3 (I1)  TO PA52WS-LTM-FILLED-SW
               PERFORM 781-SAVE-PADFP-FIELD
               THRU    781-END
               MOVE PA52WS-VALUE       TO PA52F1-PCT-NEW-VALUE-3 (I1)
       
           END-PERFORM.

       780-END.
      ******************************************************************
       781-SAVE-PADFP-FIELD.
      ******************************************************************

           IF (PA52F1-PCT-MOVE-FROM-LEVEL = ZEROES)
               IF  (PA52WS-FLD-NBR = HREMP-SALARY-CLASS-DN)
                      IF  (PADFP-SALARY-CLASS  = SPACES)
                      AND (PA52WS-PRE-VALUE       NOT = SPACES)
                           MOVE "*BLANK"             TO PA52WS-VALUE 
                      ELSE
J28956                  IF   (PA52WS-VALUE = "*BLANK")
J28956                  AND  (PA52WS-LTM-FILLED)
J28956                  AND  (PADFP-SALARY-CLASS = PA52WS-PRE-VALUE)
J28956                         MOVE SPACES          TO PA52WS-VALUE
J28956                  ELSE
                          IF (PADFP-SALARY-CLASS NOT = PA52WS-PRE-VALUE)
                              MOVE PADFP-SALARY-CLASS TO PA52WS-VALUE
                          END-IF
J28956                  END-IF
                      END-IF
               END-IF

               IF  (PA52WS-FLD-NBR = HREMP-SEC-LVL-DN)
                   IF  (PADFP-SEC-LVL         = ZEROES)
                   AND (PA52WS-PRE-VALUE       NOT = SPACES)
                       MOVE "*BLANK"               TO PA52WS-VALUE 
                   ELSE
                        MOVE PADFP-SEC-LVL TO HRWS-DEC-FIELD
                        MOVE 0            TO HRWS-SIZE
                        MOVE 0            TO HRWS-NBR-DECIMALS
                        PERFORM 770-HR-FORMAT-DEC-FIELD
J28956                  IF   (PA52WS-VALUE = "*BLANK")
J28956                  AND  (PA52WS-LTM-FILLED)
J28956                  AND  (HRWS-VALUE = PA52WS-PRE-VALUE)
J28956                         MOVE SPACES          TO PA52WS-VALUE
J28956                  ELSE
                             IF  (PADFP-SEC-LVL      NOT = ZEROES)
                                  MOVE PADFP-SEC-LVL TO HRWS-DEC-FIELD
                                  MOVE 0            TO HRWS-SIZE
                                  MOVE 0            TO HRWS-NBR-DECIMALS
                                  PERFORM 770-HR-FORMAT-DEC-FIELD
                                  IF (HRWS-VALUE NOT = PA52WS-PRE-VALUE)
                                      MOVE HRWS-VALUE  TO PA52WS-VALUE
                                  END-IF
                              END-IF
J28956                  END-IF
                   END-IF
               END-IF

               IF  (PA52WS-FLD-NBR = HREMP-SEC-LOCATION-DN)
                   IF  (PADFP-SEC-LOCATION   = SPACES)
                   AND (PA52WS-PRE-VALUE              NOT = SPACES)
                       MOVE "*BLANK"                     TO PA52WS-VALUE
                   ELSE
J28956                IF   (PA52WS-VALUE = "*BLANK")
J28956                AND  (PA52WS-LTM-FILLED)
J28956                AND  (PADFP-SEC-LOCATION = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                     TO PA52WS-VALUE
J28956                ELSE
                         IF (PADFP-SEC-LOCATION  NOT = PA52WS-PRE-VALUE)
                             MOVE PADFP-SEC-LOCATION     TO PA52WS-VALUE
                         END-IF
J28956                END-IF
                   END-IF
               END-IF

               IF  (PA52WS-FLD-NBR = HREMP-EXEMPT-EMP-DN)
                   IF  (PADFP-EXEMPT-EMP     = SPACES)
                   AND (PA52WS-PRE-VALUE       NOT = SPACES)
                       MOVE "*BLANK"               TO PA52WS-VALUE 
                   ELSE
J28956                IF   (PA52WS-VALUE = "*BLANK")
J28956                AND  (PA52WS-LTM-FILLED)
J28956                AND  (PADFP-EXEMPT-EMP       = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES               TO PA52WS-VALUE
J28956                ELSE
                         IF (PADFP-EXEMPT-EMP  NOT = PA52WS-PRE-VALUE)
                             MOVE PADFP-EXEMPT-EMP TO PA52WS-VALUE
                         END-IF
J28956                END-IF
                   END-IF
               END-IF

               IF  (PA52WS-FLD-NBR = HREMP-PAY-FREQUENCY-DN)
                   IF  (PADFP-PAY-FREQUENCY = ZEROES)
                   AND (PA52WS-PRE-VALUE       NOT = SPACES)
                       MOVE "*BLANK"               TO PA52WS-VALUE 
                   ELSE
                        MOVE PADFP-PAY-FREQUENCY
                                        TO HRWS-DEC-FIELD
                        MOVE 0           TO HRWS-SIZE
                        MOVE 0           TO HRWS-NBR-DECIMALS
                        PERFORM 770-HR-FORMAT-DEC-FIELD
J28956                  IF   (PA52WS-VALUE = "*BLANK")
J28956                  AND  (PA52WS-LTM-FILLED)
J28956                  AND  (HRWS-VALUE = PA52WS-PRE-VALUE)
J28956                        MOVE SPACES          TO PA52WS-VALUE
J28956                  ELSE
                              IF  (PADFP-PAY-FREQUENCY NOT = ZEROES)
                                   MOVE PADFP-PAY-FREQUENCY  
                                                    TO HRWS-DEC-FIELD
                                   MOVE 0           TO HRWS-SIZE
                                   MOVE 0           TO HRWS-NBR-DECIMALS
                                   PERFORM 770-HR-FORMAT-DEC-FIELD
                                   IF (HRWS-VALUE(1:30) NOT = 
                                                       PA52WS-PRE-VALUE)
                                   MOVE HRWS-VALUE     TO PA52WS-VALUE
                              END-IF
J28956                  END-IF
                   END-IF
               END-IF

               IF  (PA52WS-FLD-NBR = HREMP-CURRENCY-CODE-DN)
                   IF  (PADFP-CURRENCY-CODE  = SPACES)
                   AND (PA52WS-PRE-VALUE          NOT = SPACES)
                       MOVE "*BLANK"                  TO PA52WS-VALUE 
                   ELSE
J28956                IF   (PA52WS-VALUE = "*BLANK")
J28956                AND  (PA52WS-LTM-FILLED)
J28956                AND  (PADFP-CURRENCY-CODE = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                  TO PA52WS-VALUE
J28956                ELSE
                         IF (PADFP-CURRENCY-CODE NOT = PA52WS-PRE-VALUE)
                             MOVE PADFP-CURRENCY-CODE TO PA52WS-VALUE
                         END-IF
                      END-IF
J28956             END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-POSITION-DN)
               IF (PA52F1-PCT-MOVE-FROM-LEVEL = ZEROES)
                   MOVE PADFP-POSITION         TO PA52WS-VALUE
               ELSE
                   IF  (PADFP-POSITION     = SPACES)
                   AND (PA52WS-PRE-VALUE       NOT = SPACES)
                       MOVE "*BLANK"           TO PA52WS-VALUE
                   ELSE
                   IF (PADFP-POSITION NOT = PA52WS-PRE-VALUE)
880100                MOVE PADFP-POSITION      TO PA52WS-VALUE
                   ELSE  
                       MOVE SPACES             TO PA52WS-VALUE
                   END-IF
                   END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-JOB-CODE-DN)
               IF  (PADFP-JOB-CODE     = SPACES)
               AND (PA52WS-PRE-VALUE         NOT = SPACES)
                   MOVE "*BLANK"                TO PA52WS-VALUE         
               ELSE
J28956                IF   (PA52WS-VALUE = "*BLANK")
J28956                AND  (PA52WS-LTM-FILLED)
J28956                AND  (PADFP-JOB-CODE = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES             TO PA52WS-VALUE
J28956                ELSE
                         IF (PADFP-JOB-CODE   NOT = PA52WS-PRE-VALUE)
880100                       MOVE PADFP-JOB-CODE TO PA52WS-VALUE
                         END-IF
J28956                END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PROCESS-LEVEL-DN)
               IF  (PADFP-PROCESS-LEVEL = SPACES)
               AND (PA52WS-PRE-VALUE               NOT = SPACES)
                   MOVE "*BLANK"                      TO PA52WS-VALUE 
               ELSE
J28956                IF   (PA52WS-VALUE = "*BLANK")
J28956                AND  (PA52WS-LTM-FILLED)
J28956                AND  (PADFP-PROCESS-LEVEL = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                  TO PA52WS-VALUE
J28956                ELSE
                         IF (PADFP-PROCESS-LEVEL NOT = PA52WS-PRE-VALUE)
880200                       MOVE PADFP-PROCESS-LEVEL TO PA52WS-VALUE
                         END-IF
J28956                END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-DEPARTMENT-DN)
               IF  (PADFP-DEPARTMENT    = SPACES)
               AND (PA52WS-PRE-VALUE             NOT = SPACES)
                   MOVE "*BLANK"                    TO PA52WS-VALUE 
               ELSE
J28956                IF   (PA52WS-VALUE = "*BLANK")
J28956                AND  (PA52WS-LTM-FILLED)
J28956                AND  (PADFP-DEPARTMENT = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                TO PA52WS-VALUE
J28956                ELSE
                         IF (PADFP-DEPARTMENT    NOT = PA52WS-PRE-VALUE)
880200                       MOVE PADFP-DEPARTMENT  TO PA52WS-VALUE 
J28956                END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-USER-LEVEL-DN)
               IF  (PADFP-USER-LEVEL    = SPACES)
               AND (PA52WS-PRE-VALUE             NOT = SPACES)
                   MOVE "*BLANK"                    TO PA52WS-VALUE 
               ELSE                             
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (PADFP-USER-LEVEL = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                TO PA52WS-VALUE
J28956             ELSE
                         IF (PADFP-USER-LEVEL    NOT = PA52WS-PRE-VALUE)
880200                       MOVE PADFP-USER-LEVEL  TO PA52WS-VALUE
                         END-IF
J28956             END-IF 
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-ANNUAL-HOURS-DN)
               IF  (PADFP-ANNUAL-HOURS  = ZEROES)
               AND (PA52WS-PRE-VALUE             NOT = SPACES)
                   MOVE "*BLANK"                    TO PA52WS-VALUE 
               ELSE
                   MOVE PADFP-ANNUAL-HOURS TO HRWS-DEC-FIELD
                   MOVE 0                  TO HRWS-SIZE
J46761             MOVE 4                  TO HRWS-NBR-DECIMALS
J46761*            MOVE 0                  TO HRWS-NBR-DECIMALS
                   PERFORM 770-HR-FORMAT-DEC-FIELD
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (HRWS-VALUE = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                TO PA52WS-VALUE
J28956             ELSE
                        IF (PADFP-ANNUAL-HOURS NOT = ZEROES)
                            MOVE PADFP-ANNUAL-HOURS TO HRWS-DEC-FIELD
                            MOVE 0                  TO HRWS-SIZE
J46761                      MOVE 4                  TO HRWS-NBR-DECIMALS
J46761*                     MOVE 0                  TO HRWS-NBR-DECIMALS
                            PERFORM 770-HR-FORMAT-DEC-FIELD
                            IF (HRWS-VALUE NOT = PA52WS-PRE-VALUE)    
                                MOVE HRWS-VALUE     TO PA52WS-VALUE 
                            END-IF
                        END-IF
J28956             END-IF
               END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SCHEDULE-DN)
               IF  (PADFP-SCHEDULE      = SPACES)
               AND (PA52WS-PRE-VALUE             NOT = SPACES)
                   MOVE "*BLANK"                    TO PA52WS-VALUE 
               ELSE
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (PADFP-SCHEDULE = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                TO PA52WS-VALUE
J28956             ELSE
                         IF (PADFP-SCHEDULE      NOT = PA52WS-PRE-VALUE)
                             MOVE PADFP-SCHEDULE    TO PA52WS-VALUE
                         END-IF
J28956             END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PAY-GRADE-DN)
               IF  (PADFP-PAY-GRADE     = SPACES)
               AND (PA52WS-PRE-VALUE           NOT = SPACES)
                   MOVE "*BLANK"                  TO PA52WS-VALUE 
               ELSE
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (PADFP-PAY-GRADE = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES               TO PA52WS-VALUE
J28956             ELSE
                        IF (PADFP-PAY-GRADE     NOT = PA52WS-PRE-VALUE)
                            MOVE PADFP-PAY-GRADE   TO PA52WS-VALUE
                        END-IF
J28956             END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PAY-STEP-DN)
               IF  (PADFP-PAY-STEP      = ZEROES)
               AND (PA52WS-PRE-VALUE       NOT = SPACES)
                   MOVE "*BLANK"               TO PA52WS-VALUE 
               ELSE
                   MOVE PADFP-PAY-STEP   TO HRWS-DEC-FIELD
                   MOVE 0                TO HRWS-SIZE
                   MOVE 0                TO HRWS-NBR-DECIMALS
                   PERFORM 770-HR-FORMAT-DEC-FIELD
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (HRWS-VALUE = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES          TO PA52WS-VALUE
J28956             ELSE
                         IF  (PADFP-PAY-STEP     NOT = ZEROES)
                              MOVE PADFP-PAY-STEP   TO HRWS-DEC-FIELD
                              MOVE 0                TO HRWS-SIZE
                              MOVE 0                TO HRWS-NBR-DECIMALS
                              PERFORM 770-HR-FORMAT-DEC-FIELD
                              IF (HRWS-VALUE NOT = PA52WS-PRE-VALUE)    
                                  MOVE HRWS-VALUE   TO PA52WS-VALUE
                              END-IF
                         END-IF
J28956             END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SUPERVISOR-DN)
               IF  (PADFP-SUPERVISOR    = SPACES)
               AND (PA52WS-PRE-VALUE             NOT = SPACES)
                   MOVE "*BLANK"                    TO PA52WS-VALUE 
               ELSE
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (PADFP-SUPERVISOR = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                TO PA52WS-VALUE
J28956             ELSE
                         IF (PADFP-SUPERVISOR    NOT = PA52WS-PRE-VALUE)
                             MOVE PADFP-SUPERVISOR  TO PA52WS-VALUE
                         END-IF
J28956             END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SUPERVISOR-IND-DN)
               IF  (PADFP-SUPERVISOR-IND = SPACES)
               AND (PA52WS-PRE-VALUE       NOT = SPACES)
                   MOVE "*BLANK"                    TO PA52WS-VALUE 
               ELSE
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (PADFP-SUPERVISOR-IND        = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                TO PA52WS-VALUE
J28956             ELSE
                        IF (PADFP-SUPERVISOR-IND NOT = PA52WS-PRE-VALUE)
                            MOVE PADFP-SUPERVISOR-IND  TO PA52WS-VALUE
                        END-IF
J28956             END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PAY-STEP-DN)
               IF  (PADFP-PAY-STEP      = ZEROES)
               AND (PA52WS-PRE-VALUE           NOT = SPACES)
                   MOVE "*BLANK"                  TO PA52WS-VALUE 
               ELSE
                   MOVE PADFP-PAY-STEP   TO HRWS-DEC-FIELD
                   MOVE 0                TO HRWS-SIZE
                   MOVE 0                TO HRWS-NBR-DECIMALS
                   PERFORM 770-HR-FORMAT-DEC-FIELD
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (HRWS-VALUE = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES              TO PA52WS-VALUE
J28956             ELSE
                        IF (PADFP-PAY-STEP     NOT = ZEROES)
                            MOVE PADFP-PAY-STEP   TO HRWS-DEC-FIELD
                            MOVE 0                TO HRWS-SIZE
                            MOVE 0                TO HRWS-NBR-DECIMALS
                            PERFORM 770-HR-FORMAT-DEC-FIELD
                            IF (HRWS-VALUE     NOT = PA52WS-PRE-VALUE)  
                                MOVE HRWS-VALUE   TO PA52WS-VALUE
                            END-IF
                        END-IF
J29856             END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SHIFT-DN)
               IF  (PADFP-SHIFT         = ZEROES)
               AND (PA52WS-PRE-VALUE             NOT = SPACES)
                   MOVE "*BLANK"                    TO PA52WS-VALUE 
               ELSE
                   MOVE PADFP-SHIFT        TO HRWS-DEC-FIELD
                   MOVE 0                  TO HRWS-SIZE
                   MOVE 0                  TO HRWS-NBR-DECIMALS
                   PERFORM 770-HR-FORMAT-DEC-FIELD
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (HRWS-VALUE = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                TO PA52WS-VALUE
J28956             ELSE
                        IF (PADFP-SHIFT          NOT = ZEROES)
                            MOVE PADFP-SHIFT        TO HRWS-DEC-FIELD
                            MOVE 0                  TO HRWS-SIZE
                            MOVE 0                  TO HRWS-NBR-DECIMALS
                            PERFORM 770-HR-FORMAT-DEC-FIELD
                            IF (HRWS-VALUE NOT = PA52WS-PRE-VALUE)    
                                MOVE HRWS-VALUE     TO PA52WS-VALUE
                            END-IF
                        END-IF
J28956             END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-WORK-SCHED-DN)
               IF  (PADFP-WORK-SCHED     = SPACES)
               AND (PA52WS-PRE-VALUE            NOT = SPACES)
                   MOVE "*BLANK"                   TO PA52WS-VALUE 
               ELSE
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (PADFP-WORK-SCHED = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES               TO PA52WS-VALUE
J28956             ELSE
                        IF (PADFP-WORK-SCHED    NOT = PA52WS-PRE-VALUE)
                            MOVE PADFP-WORK-SCHED  TO PA52WS-VALUE
                        END-IF
J28956             END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HRPEM-LOCAT-CODE-DN)
               IF  (PADFP-LOCAT-CODE     = SPACES)
               AND (PA52WS-PRE-VALUE             NOT = SPACES)
                   MOVE "*BLANK"                    TO PA52WS-VALUE 
               ELSE
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (PADFP-LOCAT-CODE = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                TO PA52WS-VALUE
J28956             ELSE
                        IF (PADFP-LOCAT-CODE     NOT = PA52WS-PRE-VALUE)
                            MOVE PADFP-LOCAT-CODE   TO PA52WS-VALUE
                        END-IF
J28956             END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-NBR-FTE-DN)
           AND (PA52F1-PCT-MOVE-FROM-LEVEL NOT = ZEROES)
               IF  (PADFP-FTE        = ZEROES)
               AND (PA52WS-PRE-VALUE NOT = SPACES)
                   MOVE "*BLANK"                    TO PA52WS-VALUE
               ELSE
                   MOVE PADFP-FTE          TO HRWS-DEC-FIELD
                   MOVE 0                  TO HRWS-SIZE
                   MOVE 6                  TO HRWS-NBR-DECIMALS
                   PERFORM 770-HR-FORMAT-DEC-FIELD
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (HRWS-VALUE = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                TO PA52WS-VALUE
J28956             ELSE
                        IF (PADFP-FTE NOT = ZEROES)
                            MOVE PADFP-FTE          TO HRWS-DEC-FIELD
                            MOVE 0                  TO HRWS-SIZE
                            MOVE 6                  TO HRWS-NBR-DECIMALS
                            PERFORM 770-HR-FORMAT-DEC-FIELD
                            IF (HRWS-VALUE NOT = PA52WS-PRE-VALUE)
                                MOVE HRWS-VALUE     TO PA52WS-VALUE
                            END-IF
                         END-IF 
J28956             END-IF
               END-IF
           END-IF.



           IF  (PA52WS-FLD-NBR = HREMP-HM-DIST-CO-DN)
               IF  (PADFP-EXP-COMPANY   = ZEROES)
               AND (PA52WS-PRE-VALUE             NOT = SPACES)
                   MOVE "*BLANK"                    TO PA52WS-VALUE 
               ELSE
                   MOVE PADFP-EXP-COMPANY  TO HRWS-DEC-FIELD
                   MOVE 0                  TO HRWS-SIZE
                   MOVE 0                  TO HRWS-NBR-DECIMALS
                   PERFORM 770-HR-FORMAT-DEC-FIELD
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (HRWS-VALUE = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                TO PA52WS-VALUE
J28956             ELSE
                         IF (PADFP-EXP-COMPANY  NOT = ZEROES)
                            MOVE PADFP-EXP-COMPANY  TO HRWS-DEC-FIELD
                            MOVE 0                  TO HRWS-SIZE
                            MOVE 0                  TO HRWS-NBR-DECIMALS
                            PERFORM 770-HR-FORMAT-DEC-FIELD
                            IF (HRWS-VALUE NOT = PA52WS-PRE-VALUE)    
                                MOVE HRWS-VALUE     TO PA52WS-VALUE
                            END-IF
                         END-IF
J28956             END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-HM-ACCT-UNIT-DN)
               IF  (PADFP-EXP-ACCT-UNIT  = SPACES)
               AND (PA52WS-PRE-VALUE              NOT = SPACES)
                   MOVE "*BLANK"                     TO PA52WS-VALUE 
               ELSE
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (PADFP-EXP-ACCT-UNIT = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                 TO PA52WS-VALUE
J28956             ELSE
                        IF (PADFP-EXP-ACCT-UNIT  NOT = PA52WS-PRE-VALUE)
                            MOVE PADFP-EXP-ACCT-UNIT TO PA52WS-VALUE
                        END-IF
J28956             END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-HM-ACCOUNT-DN)
               IF  (PADFP-EXP-ACCOUNT   = ZEROES)
               AND (PA52WS-PRE-VALUE       NOT = SPACES)
                   MOVE "*BLANK"                    TO PA52WS-VALUE 
               ELSE
                   MOVE PADFP-EXP-ACCOUNT  TO HRWS-DEC-FIELD
                   MOVE 0                  TO HRWS-SIZE
                   MOVE 0                  TO HRWS-NBR-DECIMALS
                   PERFORM 770-HR-FORMAT-DEC-FIELD
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (HRWS-VALUE = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                TO PA52WS-VALUE
J28956             ELSE
                        IF (PADFP-EXP-ACCOUNT    NOT = ZEROES)
                            MOVE PADFP-EXP-ACCOUNT  TO HRWS-DEC-FIELD
                            MOVE 0                  TO HRWS-SIZE
                            MOVE 0                  TO HRWS-NBR-DECIMALS
                            PERFORM 770-HR-FORMAT-DEC-FIELD
                            IF (HRWS-VALUE NOT = PA52WS-PRE-VALUE)    
                                MOVE HRWS-VALUE     TO PA52WS-VALUE
                            END-IF
                        END-IF
J28956             END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-HM-SUB-ACCT-DN)
               IF  (PADFP-EXP-SUB-ACCT  = ZEROES)
               AND (PA52WS-PRE-VALUE       NOT = SPACES)
                   MOVE "*BLANK"               TO PA52WS-VALUE 
               ELSE
                   MOVE PADFP-EXP-SUB-ACCT TO HRWS-DEC-FIELD
                   MOVE 0                  TO HRWS-SIZE
                   MOVE 0                  TO HRWS-NBR-DECIMALS
                   PERFORM 770-HR-FORMAT-DEC-FIELD
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (HRWS-VALUE = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                TO PA52WS-VALUE
J28956             ELSE
                        IF (PADFP-EXP-SUB-ACCT   NOT = ZEROES)
                            MOVE PADFP-EXP-SUB-ACCT TO HRWS-DEC-FIELD
                            MOVE 0                  TO HRWS-SIZE
                            MOVE 0                  TO HRWS-NBR-DECIMALS
                            PERFORM 770-HR-FORMAT-DEC-FIELD
                            IF (HRWS-VALUE NOT = PA52WS-PRE-VALUE)    
                                MOVE HRWS-VALUE     TO PA52WS-VALUE
                            END-IF
                        END-IF
J28956             END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PAY-RATE-DN)
               IF  (PADFP-PAY-RATE      = ZEROES)
               AND (PA52WS-PRE-VALUE       NOT = SPACES)
                   MOVE "*BLANK"                    TO PA52WS-VALUE 
               ELSE
                   MOVE PADFP-PAY-RATE     TO HRWS-DEC-FIELD
                   MOVE 0                  TO HRWS-SIZE
                   MOVE 4                  TO HRWS-NBR-DECIMALS
                   PERFORM 770-HR-FORMAT-DEC-FIELD
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (HRWS-VALUE = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                TO PA52WS-VALUE
J28956             ELSE
                        IF (PADFP-PAY-RATE       NOT = ZEROES)
                            MOVE PADFP-PAY-RATE     TO HRWS-DEC-FIELD
                            MOVE 0                  TO HRWS-SIZE
                            MOVE 4                  TO HRWS-NBR-DECIMALS
                            PERFORM 770-HR-FORMAT-DEC-FIELD
                            IF (HRWS-VALUE NOT = PA52WS-PRE-VALUE)    
                                MOVE HRWS-VALUE     TO PA52WS-VALUE
                            END-IF
                        END-IF
J28956             END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-ACTIVITY-DN)
               IF  (PADFP-ACTIVITY       = SPACES)
               AND (PA52WS-PRE-VALUE       NOT = SPACES)
                   MOVE "*BLANK"                  TO PA52WS-VALUE 
               ELSE
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (PADFP-ACTIVITY = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES              TO PA52WS-VALUE
J28956             ELSE
                        IF (PADFP-ACTIVITY     NOT = PA52WS-PRE-VALUE)
                            MOVE PADFP-ACTIVITY   TO PA52WS-VALUE
                        END-IF
J28956             END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-ACCT-CATEGORY-DN)
               IF  (PADFP-ACCT-CATEGORY  = SPACES)
               AND (PA52WS-PRE-VALUE              NOT = SPACES)
                   MOVE "*BLANK"                     TO PA52WS-VALUE 
               ELSE
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (PADFP-ACCT-CATEGORY = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                 TO PA52WS-VALUE
J28956             ELSE
                        IF (PADFP-ACCT-CATEGORY  NOT = PA52WS-PRE-VALUE)
                            MOVE PADFP-ACCT-CATEGORY TO PA52WS-VALUE
                        END-IF
J28956             END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-UNION-CODE-DN)
               IF  (PADFP-UNION-CODE     = SPACES)
               AND (PA52WS-PRE-VALUE          NOT = SPACES)
                   MOVE "*BLANK"                 TO PA52WS-VALUE 
               ELSE
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (PADFP-UNION-CODE = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES              TO PA52WS-VALUE
J28956             ELSE
                        IF (PADFP-UNION-CODE   NOT = PA52WS-PRE-VALUE)
                            MOVE PADFP-UNION-CODE TO PA52WS-VALUE
                        END-IF
J28956             END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HRPEM-BARGAIN-UNIT-DN)
               IF  (PADFP-BARGAIN-UNIT   = SPACES)
               AND (PA52WS-PRE-VALUE             NOT = SPACES)
                   MOVE "*BLANK"                    TO PA52WS-VALUE 
               ELSE
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (PADFP-BARGAIN-UNIT = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                TO PA52WS-VALUE
J28956             ELSE
                        IF (PADFP-BARGAIN-UNIT   NOT = PA52WS-PRE-VALUE)
                            MOVE PADFP-BARGAIN-UNIT TO PA52WS-VALUE
                        END-IF
J28956             END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HRPEM-USER-AMOUNT-DN)
               IF  (PADFP-USER-AMOUNT   = ZEROES)
               AND (PA52WS-PRE-VALUE       NOT = SPACES)
                   MOVE "*BLANK"                    TO PA52WS-VALUE 
               ELSE
                   MOVE PADFP-USER-AMOUNT TO HRWS-DEC-FIELD
                   MOVE 0                 TO HRWS-SIZE
                   MOVE 4                 TO HRWS-NBR-DECIMALS
                   PERFORM 770-HR-FORMAT-DEC-FIELD
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (HRWS-VALUE = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                TO PA52WS-VALUE
J28956             ELSE
                         IF (PADFP-USER-AMOUNT  NOT = ZEROES)
                             MOVE PADFP-USER-AMOUNT TO HRWS-DEC-FIELD
                             MOVE 0                 TO HRWS-SIZE
                             MOVE 4                 TO HRWS-NBR-DECIMALS
                             PERFORM 770-HR-FORMAT-DEC-FIELD
                             IF (HRWS-VALUE NOT = PA52WS-PRE-VALUE)    
                                 MOVE HRWS-VALUE    TO PA52WS-VALUE
                             END-IF
                         END-IF
J28956             END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-OT-PLAN-CODE-DN)
               IF  (PADFP-OT-PLAN-CODE   = SPACES)
               AND (PA52WS-PRE-VALUE       NOT = SPACES)
                   MOVE "*BLANK"                    TO PA52WS-VALUE 
               ELSE
J28956             IF   (PA52WS-VALUE = "*BLANK")
J28956             AND  (PA52WS-LTM-FILLED)
J28956             AND  (PADFP-OT-PLAN-CODE = PA52WS-PRE-VALUE)
J28956                   MOVE SPACES                TO PA52WS-VALUE
J28956             ELSE
                        IF (PADFP-OT-PLAN-CODE   NOT = PA52WS-PRE-VALUE)
                            MOVE PADFP-OT-PLAN-CODE TO PA52WS-VALUE
                        END-IF
J28956             END-IF
               END-IF
           END-IF.

       781-END.

166500******************************************************************
166600 PA52S1-TRANSACTION-END.
166700******************************************************************
166800******************************************************************
166900 PA52S2-TRANSACTION SECTION 31.
167000******************************************************************
167100 PA52S2-START.
167200
           IF (PA52F2-FC NOT = "A" AND "C")
              INITIALIZE PA52F2-XMIT-ACT-EXISTS
           END-IF.
P40376     MOVE "PA52"                 TO PA52F2-PGM-NAME.

167300     PERFORM 200-EDIT-TRAN
167400     THRU    200-END.
167500
167600     IF (NO-ERROR-FOUND)
               INITIALIZE PA52F2-XMIT-ACT-EXISTS
167700         PERFORM 400-PROCESS-TRAN
167800         THRU    400-END.
167900
168000     GO TO PA52S2-TRANSACTION-END.
168100
168200******************************************************************
168300 200-EDIT-TRAN.
168400******************************************************************
168500
168600     PERFORM 210-EDIT-ACCESS
168700     THRU    210-END.
168800
168900     IF (ERROR-FOUND)
169000         GO TO 200-END.
169100
169200     IF (PA52F2-FC = "A" OR "C")
169300         PERFORM 220-EDIT-DATA
169400         THRU    220-END
169500         GO TO 200-END.
169600
169700 200-END.
169800
169900******************************************************************
170000 210-EDIT-ACCESS.
170100******************************************************************
170200
           INITIALIZE                  HREMP-UPDPEP-DATE.

170300     MOVE "P"                    TO PA52F2-PT-ACTION-TYPE.
170400
170500     MOVE PA52F2-PCT-COMPANY     TO DB-COMPANY.
170600     MOVE SPACES                 TO DB-PROCESS-LEVEL.
170700     PERFORM 840-FIND-PRSSET1.
170800     IF (PRSYSTEM-NOTFOUND)
170900         MOVE 100                                TO CRT-ERROR-NBR
171000         MOVE PA52F2-PCT-COMPANY-FN              TO CRT-FIELD-NBR
171100         GO TO 210-END.
171200
171800     MOVE PRS-NAME               TO PA52WS-PRS-NAME.
172000
172100     IF (PA52F2-FC = "A" OR "C" OR "I")
172200         MOVE PA52F2-PCT-COMPANY     TO DB-COMPANY
172300         MOVE PA52F2-PCT-ACTION-CODE TO DB-ACTION-CODE
172400         PERFORM 840-FIND-PATSET1
172500         IF (PERSACTYPE-NOTFOUND)
172600             MOVE 110                            TO CRT-ERROR-NBR
172700             MOVE PA52F2-PCT-ACTION-CODE-FN      TO CRT-FIELD-NBR
172800             GO TO 210-END
               END-IF
               IF (PAT-ACTIVE-FLAG = "2")
                   MOVE 222                       TO CRT-ERROR-NBR
                   MOVE PAT-ACTION-CODE           TO CRT-ERR-VAR1
                   MOVE PA52F2-PCT-ACTION-CODE-FN TO CRT-FIELD-NBR
                   GO TO 210-END
               END-IF
               IF (PA52F2-FC = "A")
                   PERFORM
                       VARYING I1 FROM 1 BY 1
                       UNTIL  (I1 > 36)
                       OR     (PAT-FLD-NBR (I1) = HREMP-PAY-RATE-DN)
                           CONTINUE
                   END-PERFORM
                   IF (I1 > 36)
                       MOVE 148                        TO CRT-ERROR-NBR
                       MOVE PA52F2-PCT-ACTION-CODE-FN  TO CRT-FIELD-NBR
                       GO TO 210-END
                   END-IF
               END-IF.
172900
173000     MOVE HREMP-PAY-RATE-DN      TO DB-FLD-NBR.
           INITIALIZE                     DB-COUNTRY-CD-REQ
                                          DB-PROCESS-LEVEL.
173100     PERFORM 840-FIND-PASSET1.
173200     IF (PASCRTY-FOUND)
173300         MOVE PAS-SEC-LEVEL      TO HRWS-SEC-LEVEL
173400         PERFORM 730-HR-FIELD-SECURITY
173500         IF (HRWS-FLD-SECURED)
173600             MOVE 51                             TO CRT-ERROR-NBR
173700             MOVE PA52F2-FC-FN                   TO CRT-FIELD-NBR
173800             GO TO 210-END.
173900
174000     IF  (PA52F2-FC = "N" OR "P")
174100     AND (PA52F2-PT-ACTION-TYPE = SPACES)
174200         MOVE "P"                TO DB-ACTION-TYPE
174300     ELSE
174400     IF (PA52F2-FC = "A" OR "C" OR "I")
174500         MOVE PA52F2-PCT-EMPLOYEE
174600                                 TO DB-EMPLOYEE.
174700         IF (PA52F2-PCT-EMPLOYEE NOT = ZEROS)
174800             MOVE "E"            TO DB-ACTION-TYPE
174900         ELSE
175000             MOVE "P"            TO DB-ACTION-TYPE.
175100
175200     MOVE PA52F2-PCT-EFFECT-DATE TO DB-EFFECT-DATE.
           MOVE ZEROES                 TO DB-EMPLOYEE.
175300     MOVE PA52F2-PCT-ACTION-CODE TO DB-ACTION-CODE.
175400     MOVE PA52F2-PCT-ACTION-NBR  TO DB-ACTION-NBR.
175500
175600     IF (PA52F2-FC       = "N")
175700         IF (DB-EFFECT-DATE = ZEROES)
175800             MOVE WS-HIGH-VALUES TO DB-ACTION-NBR
175900             PERFORM 850-FIND-NLT-PCTSET2
176000         ELSE
                   PERFORM 850-FIND-NLT-PCTSET2
                   IF  (PCT-COMPANY     = DB-COMPANY)
                   AND (PCT-ACTION-TYPE = DB-ACTION-TYPE)
                   AND (PCT-EMPLOYEE    = DB-EMPLOYEE)
                   AND (PCT-ACTION-CODE = DB-ACTION-CODE)
                   AND (PCT-EFFECT-DATE = DB-EFFECT-DATE)
                   AND (PCT-ACTION-NBR  = DB-ACTION-NBR)
176100                 PERFORM 860-FIND-NEXT-PCTSET2
                   END-IF
176200         END-IF
176300     ELSE
176400     IF (PA52F2-FC = "P")
176500         PERFORM 850-FIND-NLT-PCTSET2
176600         PERFORM 870-FIND-PREV-PCTSET2.
176700
176800     IF (PA52F2-FC = "N" OR "P")
176900         IF (PERSACTION-NOTFOUND)
177000         OR (PCT-COMPANY      NOT = DB-COMPANY)
177100         OR (PCT-ACTION-TYPE  NOT = "P")
177200             MOVE 12                     TO CRT-ERROR-NBR
177300             MOVE PA52F2-FC-FN           TO CRT-FIELD-NBR
177400             GO TO 210-END
177500         ELSE
177600             MOVE PCT-ACTION-TYPE        TO DB-ACTION-TYPE
177700                                            PA52F2-PT-ACTION-TYPE
177800             MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE
177900                                            PA52F2-PCT-EMPLOYEE
178000             MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE
178100                                            PA52F2-PCT-ACTION-CODE
178200             MOVE PCT-EFFECT-DATE        TO DB-EFFECT-DATE
178300                                           PA52F2-PCT-EFFECT-DATE
178400             MOVE PCT-ACTION-NBR         TO DB-ACTION-NBR
178500                                            PA52F2-PCT-ACTION-NBR
178600             PERFORM 840-FIND-PATSET1
178700             IF (PERSACTYPE-NOTFOUND)
178800                 MOVE 110                TO CRT-ERROR-NBR
178900                 MOVE PA52F2-PCT-ACTION-CODE-FN
179000                                         TO CRT-FIELD-NBR
179100                 GO TO 210-END.
179200
179300     IF (PA52F2-FC = "N" OR "P")
               PERFORM 8400-AFTER-FIND-PCT
179400         GO TO 210-END.
179500
           PERFORM 840-FIND-PCTSET2.

           IF  (PA52F2-FC = "I")
           AND (PERSACTION-NOTFOUND)
               MOVE WS-HIGH-VALUES      TO DB-ACTION-NBR
               PERFORM 850-FIND-NLT-PCTSET2.

           IF  (PA52F2-FC = "I")
           AND ((PERSACTION-NOTFOUND)
            OR  (PCT-COMPANY     NOT = DB-COMPANY)
            OR  (PCT-ACTION-TYPE NOT = DB-ACTION-TYPE)
            OR  (PCT-EMPLOYEE    NOT = DB-EMPLOYEE)
            OR  (PCT-ACTION-CODE NOT = DB-ACTION-CODE)
            OR  (PCT-EFFECT-DATE NOT = DB-EFFECT-DATE))
               INITIALIZE PA52F2-PCT-REASON (1)
                          PA52F2-PCT-REASON (2)
               IF (PAT-DFT-REASON (1) NOT = SPACES)
                   MOVE PAT-DFT-REASON (1) TO PA52F2-PCT-REASON (1)
               END-IF
               IF (PAT-DFT-REASON (2) NOT = SPACES)
                   MOVE PAT-DFT-REASON (2) TO PA52F2-PCT-REASON (2)
               END-IF
               IF (PAT-DFT-UPD-BN = SPACES)
                   MOVE PA52F2-PCT-COMPANY      TO DB-COMPANY
                   PERFORM 840-KFIND-BNCSET1
                   IF (BNCOMPANY-KFOUND)
                      MOVE "Y" TO PA52F2-PCT-UPDATE-BENEFIT
                   ELSE
                      MOVE "N" TO PA52F2-PCT-UPDATE-BENEFIT
                   END-IF
               ELSE
                   MOVE PAT-DFT-UPD-BN TO PA52F2-PCT-UPDATE-BENEFIT
               END-IF
               IF (PAT-DFT-UPD-LP = SPACES)
J13588*            MOVE PA52F2-PCT-COMPANY     TO EDCDWS-COMPANY
J13588*            MOVE "LP"                   TO EDCDWS-SYSTEM
J13588*            PERFORM 6000-IS-SYSTEM-TRIGGER-ENABLED
J13588*            IF  (EDCDWS-TRIGGER-ENABLED)
                       MOVE "Y" TO PA52F2-PCT-UPD-ABS-MGMT
J13588*            ELSE
J13588*                MOVE "N"        TO PA52F2-PCT-UPD-ABS-MGMT
J13588*            END-IF
               ELSE
                   MOVE PAT-DFT-UPD-LP TO PA52F2-PCT-UPD-ABS-MGMT
               END-IF
               MOVE PAT-HIST-CORR-FLAG TO PA52F2-PCT-HIST-CORR-FLAG
               INITIALIZE                         PA52F2-PCT-ACTION-NBR
               MOVE 407                        TO CRT-ERROR-NBR
               GO TO 210-END.

           IF  (PA52F2-FC = "C" OR "D")
           AND (PERSACTION-NOTFOUND)
               MOVE 125                                TO CRT-ERROR-NBR
               MOVE PA52F2-PCT-ACTION-CODE-FN          TO CRT-FIELD-NBR
               GO TO 210-END.

           PERFORM 8400-AFTER-FIND-PCT.

181200 210-END.
181300
181400******************************************************************
181500 220-EDIT-DATA.
181600******************************************************************
181700
           IF  (PA52F2-FC              = "A")
           AND (PA52F2-XMIT-ACT-EXISTS  = ZEROS)
               MOVE PA52F2-PCT-COMPANY     TO DB-COMPANY
               MOVE PA52F2-PCT-ACTION-CODE TO DB-ACTION-CODE
               IF (PA52F2-PCT-EMPLOYEE NOT = ZEROS)
                  MOVE "E"                 TO DB-ACTION-TYPE
               ELSE
                  MOVE "P"                 TO DB-ACTION-TYPE
               END-IF
               MOVE PA52F2-PCT-EFFECT-DATE TO DB-EFFECT-DATE
               IF (PA52F2-PCT-EMPLOYEE NOT = ZEROS)
                  MOVE PA52F2-PCT-EMPLOYEE TO DB-EMPLOYEE
                  MOVE PCTSET3-EMPLOYEE    TO WS-DB-BEG-RNG
               ELSE
                  MOVE PCTSET3-EFFECT-DATE TO WS-DB-BEG-RNG
               END-IF
               PERFORM 850-FIND-BEGRNG-PCTSET3
               IF (PERSACTION-FOUND)
      ******** Warning, ADDING DUPLICATE RECORD.
               MOVE 1                      TO PA52F2-XMIT-ACT-EXISTS
               MOVE 261                    TO CRT-ERROR-NBR
               GO TO 220-END.

181800     IF  (PA52F2-PCT-EMPLOYEE = ZEROS)
               GO TO 220-CONTINUE.

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36)
               IF  (PAT-FLD-NBR (I1) = HREMP-PAY-RATE-DN)
                   GO TO 220-CONTINUE
               END-IF
           END-PERFORM.

182900     MOVE 204                                TO CRT-ERROR-NBR.
183000     MOVE PA52F2-PCT-ACTION-CODE-FN          TO CRT-FIELD-NBR.
183100     GO TO 220-END.
183200
       220-CONTINUE.

183300     IF  (PA52F2-PCT-ANT-END-DATE NOT = ZEROS)
183400     AND (PA52F2-PCT-ANT-END-DATE NOT > PA52F2-PCT-EFFECT-DATE)
183500         MOVE 233                                TO CRT-ERROR-NBR
183600         MOVE PA52F2-PCT-ANT-END-DATE-FN         TO CRT-FIELD-NBR
183700         GO TO 220-END.
183800
J13588*    MOVE PA52F2-PCT-COMPANY     TO EDCDWS-COMPANY.
J13588*    MOVE "LP"                   TO EDCDWS-SYSTEM.
J13588*    PERFORM 6000-IS-SYSTEM-TRIGGER-ENABLED.
J13588*    IF  (NOT EDCDWS-TRIGGER-ENABLED)
J13588*        IF (PA52F2-PCT-UPD-ABS-MGMT = "Y")
J13588*            MOVE 234            TO CRT-ERROR-NBR
J13588*            MOVE PA52F2-PCT-UPD-ABS-MGMT-FN
J13588*                                TO CRT-FIELD-NBR
J13588*            GO TO 220-END
J13588*        END-IF
J13588*        IF (PA52F2-PCT-UPD-ABS-MGMT = SPACES)
J13588*            MOVE "N"            TO PA52F2-PCT-UPD-ABS-MGMT
J13588*        END-IF.

           MOVE PA52F2-PCT-COMPANY                 TO DB-COMPANY.
110700     PERFORM 840-FIND-BNCSET1.
           IF  (BNCOMPANY-NOTFOUND)
           AND (PA52F2-PCT-UPDATE-BENEFIT  = "Y")
      ******** Cannot update benefit; Company is not setup in benefit system
028700         MOVE 180                            TO CRT-ERROR-NBR
028800         MOVE PA52F2-PCT-UPDATE-BENEFIT-FN   TO CRT-FIELD-NBR
028900         GO TO 220-END.

           IF (PAT-REQUIRE-REASON = 2)
               IF (PA52F2-PCT-REASON (1) = SPACES)
               OR (PA52F2-PCT-REASON (2) = SPACES)
                   MOVE 544                        TO CRT-ERROR-NBR
                   MOVE PAT-REQUIRE-REASON         TO CRT-ERR-VAR1
                   IF (PA52F2-PCT-REASON (2) = SPACES)
                       MOVE PA52F2-PCT-REASON-FN (2)   TO CRT-FIELD-NBR
                   END-IF
                   IF (PA52F2-PCT-REASON (1) = SPACES)
                       MOVE PA52F2-PCT-REASON-FN (1)   TO CRT-FIELD-NBR
                   END-IF
                   GO TO 220-END.

           IF (PAT-REQUIRE-REASON = 1)
               IF  (PA52F2-PCT-REASON (1) = SPACES)
               AND (PA52F2-PCT-REASON (2) = SPACES)
                   MOVE 544                        TO CRT-ERROR-NBR
                   MOVE PAT-REQUIRE-REASON         TO CRT-ERR-VAR1
                   IF (PA52F2-PCT-REASON (1) = SPACES)
                       MOVE PA52F2-PCT-REASON-FN (1)   TO CRT-FIELD-NBR
                   END-IF
                   GO TO 220-END.

           IF  (PA52F2-PCT-REASON (1) = PA52F2-PCT-REASON (2))
           AND (PA52F2-PCT-REASON (1) NOT = SPACES)
               MOVE 194                            TO CRT-ERROR-NBR
               MOVE PA52F2-PCT-REASON-FN (2)       TO CRT-FIELD-NBR
               GO TO 220-END.

029300     PERFORM 222-EDIT-REASONS
029400     THRU    222-END
029500         VARYING I1 FROM 1 BY 1
029600         UNTIL  (I1 > 2)
029700         OR     (ERROR-FOUND).
184400
184500     IF (ERROR-FOUND)
184600         GO TO 220-END.
184700
029900     IF (ERROR-FOUND)
030000         GO TO 220-END.
030100
184800     IF (PA52F2-PCT-PROCESS-LEVEL NOT = SPACES)
184900         MOVE PA52F2-PCT-PROCESS-LEVEL   TO DB-PROCESS-LEVEL
185000         PERFORM 840-FIND-PRSSET1
185100         IF (PRSYSTEM-NOTFOUND)
185200             MOVE 165                            TO CRT-ERROR-NBR
185300             MOVE PA52F2-PCT-PROCESS-LEVEL-FN    TO CRT-FIELD-NBR
185400             GO TO 220-END.
185500
185600     IF (PA52F2-PCT-DEPARTMENT NOT = SPACES)
185700         MOVE PA52F2-PCT-PROCESS-LEVEL   TO DB-PROCESS-LEVEL
185800         MOVE PA52F2-PCT-DEPARTMENT      TO DB-DEPARTMENT
185900         PERFORM 840-FIND-DPTSET1
186000         IF (DEPTCODE-NOTFOUND)
186100             MOVE 166                            TO CRT-ERROR-NBR
186200             MOVE PA52F2-PCT-DEPARTMENT-FN       TO CRT-FIELD-NBR
186300             GO TO 220-END.
186400
186500     IF (PA52F2-PCT-USER-LEVEL NOT = SPACES)
186600         MOVE "UL"                   TO DB-TYPE
186700         MOVE PA52F2-PCT-USER-LEVEL  TO DB-CODE
186800         PERFORM 840-FIND-PCOSET1
186900         IF (PCODES-NOTFOUND)
187000             MOVE 167                            TO CRT-ERROR-NBR
187100             MOVE PA52F2-PCT-USER-LEVEL-FN       TO CRT-FIELD-NBR
187200             GO TO 220-END.
187300
187400     IF (PA52F2-PCT-LOCAT-CODE NOT = SPACES)
187500         MOVE PA52F2-PCT-LOCAT-CODE  TO DB-CODE
187600         MOVE "LO"                   TO DB-TYPE
187700         PERFORM 840-FIND-PCOSET1
187800         IF (PCODES-NOTFOUND)
187900             MOVE 172                            TO CRT-ERROR-NBR
188000             MOVE PA52F2-PCT-LOCAT-CODE-FN       TO CRT-FIELD-NBR
188100             GO TO 220-END.
188200
188300     IF (PA52F2-PCT-SUPERVISOR NOT = SPACES)
188400         MOVE PA52F2-PCT-SUPERVISOR  TO DB-CODE
188500         PERFORM 840-FIND-HSUSET1
188600         IF (HRSUPER-NOTFOUND)
188700             MOVE 168                            TO CRT-ERROR-NBR
188800             MOVE PA52F2-PCT-SUPERVISOR-FN       TO CRT-FIELD-NBR
188900             GO TO 220-END.
189000
189100     IF (PA52F2-PCT-UNION-CODE NOT = SPACES)
189200         MOVE PA52F2-PCT-UNION-CODE  TO DB-CODE
189300         MOVE "UN"                   TO DB-TYPE
189400         PERFORM 840-FIND-PCOSET1
189500         IF (PCODES-NOTFOUND)
189600             MOVE 173                            TO CRT-ERROR-NBR
189700             MOVE PA52F2-PCT-UNION-CODE-FN       TO CRT-FIELD-NBR
189800             GO TO 220-END.
189900
190000     IF (PA52F2-PCT-PERS-GROUP NOT = SPACES)
190100         MOVE PA52F2-PCT-PERS-GROUP  TO DB-GROUP-NAME
190200         PERFORM 840-FIND-PRGSET1
190300         IF (PERSGROUP-NOTFOUND)
190400             MOVE 197                            TO CRT-ERROR-NBR
190500             MOVE PA52F2-PCT-PERS-GROUP-FN       TO CRT-FIELD-NBR
190600             GO TO 220-END
               ELSE
                   IF (PRG-UPDATE-FLAG = "Y")
                       MOVE 543                        TO CRT-ERROR-NBR
                       MOVE PA52F2-PCT-PERS-GROUP-FN   TO CRT-FIELD-NBR
                       GO TO 220-END
                   END-IF.
190700
190800     IF  (PA52F2-PCT-EMPLOYEE     = ZEROS)
190900     AND (PA52F2-PCT-SALARY-CLASS = SPACES)
191000         MOVE 50                                 TO CRT-ERROR-NBR
191100         MOVE PA52F2-PCT-SALARY-CLASS-FN         TO CRT-FIELD-NBR
191200         GO TO 220-END.
191300
191400     IF (PA52F2-PCT-EMPLOYEE NOT = ZEROS)
191500         IF (PA52F2-PCT-PROCESS-LEVEL NOT = SPACES)
191600         OR (PA52F2-PCT-DEPARTMENT    NOT = SPACES)
191700         OR (PA52F2-PCT-USER-LEVEL    NOT = SPACES)
191800         OR (PA52F2-PCT-LOCAT-CODE    NOT = SPACES)
191900         OR (PA52F2-PCT-SUPERVISOR    NOT = SPACES)
192000         OR (PA52F2-PCT-UNION-CODE    NOT = SPACES)
192100         OR (PA52F2-PCT-PERS-GROUP    NOT = SPACES)
192200             MOVE 203                            TO CRT-ERROR-NBR
192300             MOVE PA52F2-PCT-EMPLOYEE-FN         TO CRT-FIELD-NBR
192400             GO TO 220-END.
192500
192600     IF (PA52F2-PCT-EMPLOYEE NOT = ZEROS)
192700         MOVE PA52F2-PCT-EMPLOYEE    TO DB-EMPLOYEE
192800         PERFORM 840-FIND-EMPSET1
192900         IF (EMPLOYEE-NOTFOUND)
193000             MOVE 130                            TO CRT-ERROR-NBR
193100             MOVE PA52F2-PCT-EMPLOYEE-FN         TO CRT-FIELD-NBR
193200             GO TO 220-END
193300         ELSE
193400             MOVE EMP-COMPANY        TO CRT-COMPANY
193500             MOVE EMP-PROCESS-LEVEL  TO CRT-PROCESS-LEVEL
193600             PERFORM 700-HR-EMP-SECURITY
193700             IF (HRWS-EMP-SECURED)
193800                 MOVE 244                        TO CRT-ERROR-NBR
193900                 MOVE PA52F2-PCT-EMPLOYEE-FN     TO CRT-FIELD-NBR
194000                 GO TO 220-END
194100             ELSE
194200                 IF (EMP-PAY-STEP NOT = ZEROS)
194300                     MOVE 205                    TO CRT-ERROR-NBR
194400                     MOVE PA52F2-PCT-EMPLOYEE-FN TO CRT-FIELD-NBR
194500                     GO TO 220-END
194600                 ELSE
194700                     MOVE EMP-SALARY-CLASS
194800                                     TO PA52F2-PCT-SALARY-CLASS.
194900
           IF  (PA52F2-FC                  = "A")
           AND (PA52F2-PCT-NEW-EFFECT-DATE NOT = ZEROES)
               MOVE 122                                TO CRT-ERROR-NBR
               MOVE PA52F2-PCT-NEW-EFFECT-DATE-FN      TO CRT-FIELD-NBR
               GO TO 220-END.

           IF (PA52F2-PCT-HIST-CORR-FLAG = SPACES)
               IF (PAT-HIST-CORR-FLAG NOT = SPACES)
                   MOVE PAT-HIST-CORR-FLAG TO PA52F2-PCT-HIST-CORR-FLAG
               ELSE
                   MOVE 123                            TO CRT-ERROR-NBR
                   MOVE PA52F2-PCT-HIST-CORR-FLAG-FN   TO CRT-FIELD-NBR
                   GO TO 220-END
               END-IF
           END-IF.

           IF  (PA52F2-FC = "C")
           AND (PA52F2-PCT-NEW-EFFECT-DATE   NOT = ZEROES)
               IF (PA52F2-PCT-REASON (1)     NOT = PCT-REASON (1))
               OR (PA52F2-PCT-REASON (2)     NOT = PCT-REASON (2))
               OR (PA52F2-PCT-ANT-END-DATE   NOT = PCT-ANT-END-DATE)
               OR (PA52F2-PCT-UPDATE-BENEFIT NOT = PCT-UPDATE-BENEFIT)
               OR (PA52F2-PCT-UPD-ABS-MGMT   NOT = PCT-UPD-ABS-MGMT)
               OR (PA52F2-PCT-HIST-CORR-FLAG NOT = PCT-HIST-CORR-FLAG)
               OR (PA52F2-PCT-PROCESS-LEVEL  NOT = PCT-PROCESS-LEVEL)
               OR (PA52F2-PCT-DEPARTMENT     NOT = PCT-DEPARTMENT)
               OR (PA52F2-PCT-USER-LEVEL     NOT = PCT-USER-LEVEL)
               OR (PA52F2-PCT-LOCAT-CODE     NOT = PCT-LOCAT-CODE)
               OR (PA52F2-PCT-SUPERVISOR     NOT = PCT-SUPERVISOR)
               OR (PA52F2-PCT-UNION-CODE     NOT = PCT-UNION-CODE)
               OR (PA52F2-PCT-PERS-GROUP     NOT = PCT-PERS-GROUP)
               OR (PA52F2-PCT-SALARY-CLASS   NOT = PCT-SALARY-CLASS)
               OR (PA52F2-PCT-PCT-PAY-INC    NOT = PCT-PCT-PAY-INC)
               OR (PA52F2-PCT-PCT-PAY-DEC    NOT = PCT-PCT-PAY-DEC)
               OR (PA52F2-PCT-ROUND-OPT      NOT = PCT-ROUND-OPT)
               OR (PA52F2-PCT-ROUND-EVEN     NOT = PCT-ROUND-EVEN)
               OR (PA52F2-PCT-FLAT-INC       NOT = PCT-FLAT-INC)
               OR (PA52F2-PCT-FLAT-DEC       NOT = PCT-FLAT-DEC)
               OR (PA52F2-PCT-NEW-RATE       NOT = PCT-NEW-RATE)
                   MOVE PA52F2-PCT-NEW-EFFECT-DATE-FN  TO CRT-FIELD-NBR
                   IF  (PA52F2-PCT-REASON (1)  NOT = PCT-REASON (1))
                       MOVE PA52F2-PCT-REASON-FN (1)   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-REASON (2)  NOT = PCT-REASON (2))
                       MOVE PA52F2-PCT-REASON-FN (2)   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-ANT-END-DATE NOT = PCT-ANT-END-DATE)
                       MOVE PA52F2-PCT-ANT-END-DATE-FN TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-UPDATE-BENEFIT
                                               NOT = PCT-UPDATE-BENEFIT)
                       MOVE PA52F2-PCT-UPDATE-BENEFIT-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-UPD-ABS-MGMT
                                               NOT = PCT-UPD-ABS-MGMT)
                       MOVE PA52F2-PCT-UPD-ABS-MGMT-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-HIST-CORR-FLAG
                                               NOT = PCT-HIST-CORR-FLAG)
                       MOVE PA52F2-PCT-HIST-CORR-FLAG-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-PROCESS-LEVEL
                                               NOT = PCT-PROCESS-LEVEL)
                       MOVE PA52F2-PCT-PROCESS-LEVEL-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-DEPARTMENT  NOT = PCT-DEPARTMENT)
                       MOVE PA52F2-PCT-DEPARTMENT-FN   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-USER-LEVEL  NOT = PCT-USER-LEVEL)
                       MOVE PA52F2-PCT-USER-LEVEL-FN   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-LOCAT-CODE  NOT = PCT-LOCAT-CODE)
                       MOVE PA52F2-PCT-LOCAT-CODE-FN   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-SUPERVISOR  NOT = PCT-SUPERVISOR)
                       MOVE PA52F2-PCT-SUPERVISOR-FN   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-UNION-CODE  NOT = PCT-UNION-CODE)
                       MOVE PA52F2-PCT-UNION-CODE-FN   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-PERS-GROUP  NOT = PCT-PERS-GROUP)
                       MOVE PA52F2-PCT-PERS-GROUP-FN   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-SALARY-CLASS
                                               NOT = PCT-SALARY-CLASS)
                       MOVE PA52F2-PCT-SALARY-CLASS-FN TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-PCT-PAY-INC NOT = PCT-PCT-PAY-INC)
                       MOVE PA52F2-PCT-PCT-PAY-INC-FN  TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-PCT-PAY-DEC NOT = PCT-PCT-PAY-DEC)
                       MOVE PA52F2-PCT-PCT-PAY-DEC-FN  TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-ROUND-OPT   NOT = PCT-ROUND-OPT)
                       MOVE PA52F2-PCT-ROUND-OPT-FN    TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-ROUND-EVEN  NOT = PCT-ROUND-EVEN)
                       MOVE PA52F2-PCT-ROUND-EVEN-FN   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-FLAT-INC    NOT = PCT-FLAT-INC)
                       MOVE PA52F2-PCT-FLAT-INC-FN     TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-FLAT-DEC    NOT = PCT-FLAT-DEC)
                       MOVE PA52F2-PCT-FLAT-DEC-FN     TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F2-PCT-NEW-RATE    NOT = PCT-NEW-RATE)
                       MOVE PA52F2-PCT-NEW-RATE-FN     TO CRT-FIELD-NBR
                   END-IF
                   MOVE 124                            TO CRT-ERROR-NBR
                   GO TO 220-END.

195000     MOVE WS-FALSE               TO PA52WS-AMOUNT-ENTERED-SW.
195100
195200     IF (PA52F2-PCT-PCT-PAY-INC NOT = ZEROS)
195300         MOVE WS-TRUE            TO PA52WS-AMOUNT-ENTERED-SW.
195400
195500     IF (PA52F2-PCT-PCT-PAY-DEC NOT = ZEROS)
195600         IF (PA52WS-AMOUNT-ENTERED)
195700             MOVE 198                            TO CRT-ERROR-NBR
195800             MOVE PA52F2-PCT-PCT-PAY-DEC-FN      TO CRT-FIELD-NBR
195900             GO TO 220-END
196000         ELSE
196100             IF (PA52F2-PCT-PCT-PAY-DEC > 100)
196200                 MOVE 206                        TO CRT-ERROR-NBR
196300                 MOVE PA52F2-PCT-PCT-PAY-DEC-FN  TO CRT-FIELD-NBR
196400                 GO TO 220-END
196500             ELSE
196600                 MOVE WS-TRUE    TO PA52WS-AMOUNT-ENTERED-SW.
196700
196800     IF (PA52F2-PCT-FLAT-INC NOT = ZEROS)
196900         IF (PA52WS-AMOUNT-ENTERED)
197000             MOVE 198                            TO CRT-ERROR-NBR
197100             MOVE PA52F2-PCT-FLAT-INC-FN         TO CRT-FIELD-NBR
197200             GO TO 220-END
197300         ELSE
197400             MOVE WS-TRUE        TO PA52WS-AMOUNT-ENTERED-SW.
197500
197600     IF (PA52F2-PCT-FLAT-DEC NOT = ZEROS)
197700         IF (PA52WS-AMOUNT-ENTERED)
197800             MOVE 198                            TO CRT-ERROR-NBR
197900             MOVE PA52F2-PCT-FLAT-DEC-FN         TO CRT-FIELD-NBR
198000             GO TO 220-END
198100         ELSE
198200             MOVE WS-TRUE        TO PA52WS-AMOUNT-ENTERED-SW.
198300
198400     IF (PA52F2-PCT-NEW-RATE NOT = ZEROS)
198500         IF (PA52WS-AMOUNT-ENTERED)
198600             MOVE 198                            TO CRT-ERROR-NBR
198700             MOVE PA52F2-PCT-NEW-RATE-FN         TO CRT-FIELD-NBR
198800             GO TO 220-END
198900         ELSE
199000             MOVE WS-TRUE        TO PA52WS-AMOUNT-ENTERED-SW.
199100
199200     IF (PA52WS-NO-AMOUNT-ENTERED)
199300         MOVE 200                                TO CRT-ERROR-NBR
199400         MOVE PA52F2-FC-FN                       TO CRT-FIELD-NBR
199500         GO TO 220-END.
199600
199700     IF  (PA52F2-PCT-ROUND-OPT  = "N")
199800     AND (PA52F2-PCT-ROUND-EVEN NOT = ZEROS)
199900         MOVE 201                                TO CRT-ERROR-NBR
200000         MOVE PA52F2-PCT-ROUND-EVEN-FN           TO CRT-FIELD-NBR
200100         GO TO 220-END.
200200
200300     MOVE PA52F2-PCT-ROUND-EVEN  TO PA52WS-ROUND-EVEN.
200400     MOVE WS-FALSE               TO PA52WS-AMOUNT-ENTERED-SW.
200500
200600     IF  (PA52F2-PCT-SALARY-CLASS = "S")
200700     AND (PA52WS-ROUND-EVEN       NOT = ZEROS)
200800     AND (PA52WS-ROUND-EVEN       < 1)
200900         MOVE 202                                TO CRT-ERROR-NBR
201000         MOVE PA52F2-PCT-ROUND-EVEN-FN           TO CRT-FIELD-NBR
201100         GO TO 220-END.
201200
201300     PERFORM 224-EDIT-ROUND-TO-EVEN
201400     THRU    224-END.
201500
201600 220-END.
201700
201800******************************************************************
201900 222-EDIT-REASONS.
202000******************************************************************
202100
202200     IF (PA52F2-PCT-REASON (I1) = SPACES)
202300         GO TO 222-END.
202400
           SET PA52WS-NO-ACTION-REASON TO TRUE.

           MOVE PA52F2-PCT-COMPANY     TO DB-COMPANY.
           MOVE PA52F2-PCT-ACTION-CODE TO DB-ACTION-CODE.
049700     MOVE CRESET4-ACTION-CODE    TO WS-DB-BEG-RNG.
           PERFORM 850-KFIND-BEGRNG-CRESET4.
           IF (PAACTREAS-KFOUND)
               SET PA52WS-ACTION-REASON     TO TRUE
           END-IF.

050000     IF  (PA52F2-FC              = "A")
050100     OR ((PA52F2-FC              = "C")
050200     AND (PA52F2-PCT-REASON (I1) NOT = PCT-REASON (I1))
           AND (PA52F2-PCT-REASON (I1) NOT = SPACES))
               MOVE PA52F2-PCT-REASON (I1) TO DB-ACT-REASON-CD
               IF (PA52WS-ACTION-REASON)
                   PERFORM 840-FIND-CRESET4
               ELSE
                   MOVE CRESET3-ACT-REASON-CD    TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-CRESET3
               END-IF
050400         IF (PAACTREAS-NOTFOUND)
                   IF (PA52WS-ACTION-REASON)
                       MOVE 142                TO CRT-ERROR-NBR
                   ELSE
050500                 MOVE 140                        TO CRT-ERROR-NBR
                   END-IF
050600             MOVE PA52F2-PCT-REASON-FN (I1)      TO CRT-FIELD-NBR
050700             GO TO 222-END
050800         ELSE
050900             IF (CRE-ACTIVE-FLAG NOT = "1")
051000                 MOVE 141                        TO CRT-ERROR-NBR
051100                 MOVE PA52F2-PCT-REASON-FN (I1)  TO CRT-FIELD-NBR
051200                 GO TO 222-END.
051300
204200 222-END.
204300
204400*****************************************************************
204500 224-EDIT-ROUND-TO-EVEN.
204600*****************************************************************
204700
204800     IF (PA52WS-RND-THOUSAND NOT = ZEROS)
204900         MOVE WS-TRUE            TO PA52WS-AMOUNT-ENTERED-SW.
205000
205100     IF  (PA52WS-RND-THOUSAND NOT = ZEROS)
205200     AND (PA52WS-RND-THOUSAND NOT = 1 AND 2 AND 5)
205300         MOVE 210                                TO CRT-ERROR-NBR
205400         MOVE PA52F2-PCT-ROUND-EVEN-FN           TO CRT-FIELD-NBR
205500         GO TO 224-END.
205600
205700     IF (PA52WS-RND-HUNDRED NOT = ZEROS)
205800         IF (PA52WS-AMOUNT-ENTERED)
205900             MOVE 211                            TO CRT-ERROR-NBR
206000             MOVE PA52F2-PCT-ROUND-EVEN-FN       TO CRT-FIELD-NBR
206100             GO TO 224-END
206200         ELSE
206300             MOVE WS-TRUE        TO PA52WS-AMOUNT-ENTERED-SW.
206400
206500     IF  (PA52WS-RND-HUNDRED NOT = ZEROS)
206600     AND (PA52WS-RND-HUNDRED NOT = 1 AND 2 AND 5)
206700         MOVE 212                                TO CRT-ERROR-NBR
206800         MOVE PA52F2-PCT-ROUND-EVEN-FN           TO CRT-FIELD-NBR
206900         GO TO 224-END.
207000
207100     IF (PA52WS-RND-TEN NOT = ZEROS)
207200         IF (PA52WS-AMOUNT-ENTERED)
207300             MOVE 211                            TO CRT-ERROR-NBR
207400             MOVE PA52F2-PCT-ROUND-EVEN-FN       TO CRT-FIELD-NBR
207500             GO TO 224-END
207600         ELSE
207700             MOVE WS-TRUE        TO PA52WS-AMOUNT-ENTERED-SW.
207800
207900     IF  (PA52WS-RND-TEN NOT = ZEROS)
208000     AND (PA52WS-RND-TEN NOT = 1 AND 2 AND 5)
208100         MOVE 213                                TO CRT-ERROR-NBR
208200         MOVE PA52F2-PCT-ROUND-EVEN-FN           TO CRT-FIELD-NBR
208300         GO TO 224-END.
208400
208500     IF (PA52WS-RND-ONE NOT = ZEROS)
208600         IF (PA52WS-AMOUNT-ENTERED)
208700             MOVE 211                            TO CRT-ERROR-NBR
208800             MOVE PA52F2-PCT-ROUND-EVEN-FN       TO CRT-FIELD-NBR
208900             GO TO 224-END
209000         ELSE
209100             MOVE WS-TRUE        TO PA52WS-AMOUNT-ENTERED-SW.
209200
209300     IF  (PA52WS-RND-ONE NOT = ZEROS)
209400     AND (PA52WS-RND-ONE NOT = 1 AND 2 AND 5)
209500         MOVE 214                                TO CRT-ERROR-NBR
209600         MOVE PA52F2-PCT-ROUND-EVEN-FN           TO CRT-FIELD-NBR
209700         GO TO 224-END.
209800
209900     IF (PA52WS-RND-CENT-A NOT = ZEROS)
210000         IF (PA52WS-AMOUNT-ENTERED)
210100             MOVE 211                            TO CRT-ERROR-NBR
210200             MOVE PA52F2-PCT-ROUND-EVEN-FN       TO CRT-FIELD-NBR
210300             GO TO 224-END
210400         ELSE
210500             MOVE WS-TRUE        TO PA52WS-AMOUNT-ENTERED-SW.
210600
210700     IF  (PA52WS-RND-CENT-A NOT = ZEROS)
210800     AND (PA52WS-RND-CENT-A NOT = 1 AND 2 AND 5)
210900         MOVE 215                                TO CRT-ERROR-NBR
211000         MOVE PA52F2-PCT-ROUND-EVEN-FN           TO CRT-FIELD-NBR
211100         GO TO 224-END.
211200
211300     IF (PA52WS-RND-CENT-B NOT = ZEROS)
211400         IF (PA52WS-AMOUNT-ENTERED)
211500             MOVE 211                            TO CRT-ERROR-NBR
211600             MOVE PA52F2-PCT-ROUND-EVEN-FN       TO CRT-FIELD-NBR
211700             GO TO 224-END
211800         ELSE
211900             MOVE WS-TRUE        TO PA52WS-AMOUNT-ENTERED-SW.
212000
212100     IF  (PA52WS-RND-CENT-B NOT = ZEROS)
212200     AND (PA52WS-RND-CENT-B NOT = 1 AND 2 AND 5)
212300         MOVE 216                                TO CRT-ERROR-NBR
212400         MOVE PA52F2-PCT-ROUND-EVEN-FN           TO CRT-FIELD-NBR
212500         GO TO 224-END.
212600
212700     IF (PA52WS-RND-TENTH-CENT NOT = ZEROS)
212800         IF (PA52WS-AMOUNT-ENTERED)
212900             MOVE 211                            TO CRT-ERROR-NBR
213000             MOVE PA52F2-PCT-ROUND-EVEN-FN       TO CRT-FIELD-NBR
213100             GO TO 224-END
213200         ELSE
213300             MOVE WS-TRUE        TO PA52WS-AMOUNT-ENTERED-SW.
213400
213500     IF  (PA52WS-RND-TENTH-CENT   NOT = ZEROS)
213600     AND (PA52WS-RND-TENTH-CENT   NOT = 1 AND 2 AND 5)
213700         MOVE 217                                TO CRT-ERROR-NBR
213800         MOVE PA52F2-PCT-ROUND-EVEN-FN           TO CRT-FIELD-NBR
213900         GO TO 224-END.
214000
214100 224-END.
214200
214300******************************************************************
214400 400-PROCESS-TRAN.
214500******************************************************************
214600
214700     IF (PA52F2-FC = "A")
214800         PERFORM 410-ADD
214900         THRU    410-END
215000     ELSE
215100     IF (PA52F2-FC = "C")
215200         PERFORM 420-CHANGE
215300         THRU    420-END
215400     ELSE
215500     IF (PA52F2-FC = "D")
215600         PERFORM 440-DELETE
215700         THRU    440-END
215800     ELSE
215900     IF (PA52F2-FC = "I" OR "N" OR "P")
216000         PERFORM 480-INQUIRE
216100         THRU    480-END.
216200
216300     IF  (PA52F2-FC           = "A" OR "C" OR "D")
216400     AND (PA52F2-PCT-EMPLOYEE NOT = ZEROS)
216500         MOVE PA52F2-PCT-COMPANY  TO DB-COMPANY
216600         MOVE PA52F2-PCT-EMPLOYEE TO DB-EMPLOYEE
216700         PERFORM 840-FIND-PEMSET1
216800         INITIALIZE HREMP-SCR-FIELDS
216900                    HRPEM-SCR-FIELDS
217000         PERFORM 7000-HREMP-DEFAULT
217100         PERFORM 7000-HRPEM-DEFAULT 
J45417         IF ((PA52F2-FC = "A")
J45417         AND (NO-ERROR-FOUND))
J45417             MOVE CRT-ADD-COMPLETE    TO CRT-MESSAGE
J45417         END-IF
J45417         IF ((PA52F2-FC = "C")
J45417         AND (NO-ERROR-FOUND))
J45417             MOVE CRT-CHG-COMPLETE    TO CRT-MESSAGE
J45417         END-IF
J45417         IF ((PA52F2-FC = "D")
J45417         AND (NO-ERROR-FOUND))
J45417             MOVE CRT-RECS-DELETED    TO CRT-MESSAGE
J45417         END-IF
J45417     END-IF.
217300
217400 400-END.
217500
217600******************************************************************
217700 410-ADD.
217800******************************************************************
217900
218000     PERFORM 910-AUDIT-BEGIN.
218100     IF (DMS-ABORTED)
218200         GO TO 410-END.
218300
218400     MOVE PA52F2-PCT-COMPANY         TO PAACT-COMPANY.
218500     IF (PA52F2-PCT-EMPLOYEE NOT = ZEROS)
218600         MOVE "E"                    TO PAACT-ACTION-TYPE
218700     ELSE
218800         MOVE "P"                    TO PAACT-ACTION-TYPE.
218900     MOVE PA52F2-PCT-EMPLOYEE        TO PAACT-EMPLOYEE.
219000     MOVE PA52F2-PCT-ACTION-CODE     TO PAACT-ACTION-CODE.
219100     MOVE PA52F2-PCT-EFFECT-DATE     TO PAACT-EFFECT-DATE.
219200     MOVE "N"                        TO PAACT-HISTORY.
219300     PERFORM 2300-PAACT-ACTION-NBR.
219400
219500     PERFORM 800-CREATE-PERSACTION.
219600     MOVE PAACT-ACTION-NBR           TO PA52F2-PCT-ACTION-NBR
219700                                        PCT-ACTION-NBR.
219800     PERFORM 500-MOVE-DATA
219900     THRU    500-END.
220000     PERFORM 8200-STORE-PERSACTION.
220100
220200     IF (PCT-EMPLOYEE NOT = ZEROS)
220300         MOVE PCT-COMPANY            TO DB-COMPANY
220400         MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE
220500         PERFORM 840-MODIFY-EMPSET1
220600         IF (PCT-EFFECT-DATE   < EMP-PEND-ACT-DATE)
220700         OR (EMP-PEND-ACT-DATE = ZEROS)
220800             MOVE PCT-EFFECT-DATE    TO EMP-PEND-ACT-DATE
220900         END-IF
221000         PERFORM 820-STORE-EMPLOYEE.
221100
221200     PERFORM 920-AUDIT-END.
221300
221400     PERFORM 481-MOVE-TO-SCREEN
221500     THRU    481-END.
221600     MOVE CRT-ADD-COMPLETE       TO CRT-MESSAGE.
221700
P40376     MOVE SPACES                     TO PA52F2-PGM-NAME.
P40376     MOVE SPACES                     TO CRT-PASS-FC.
P40376     MOVE "A"                        TO CRT-DISPLAY-FC.
P40376     MOVE "P"                        TO PA52F2-PT-ACTION-TYPE.
P40376     MOVE PA52WS-ACTION-NBR          TO PA52F2-PCT-ACTION-NBR.
P40376
221800 410-END.
221900
222000******************************************************************
222100 420-CHANGE.
222200******************************************************************
222300
222400     PERFORM 910-AUDIT-BEGIN.
222500     IF (DMS-ABORTED)
222600         GO TO 420-END.

      * Only "P" actions are handled because once an "E" type action
      * is created, it can only be maintained on PA52.
223900
           IF (PA52F2-PCT-NEW-EFFECT-DATE NOT = ZEROES)
               MOVE PA52F2-PCT-COMPANY         TO PAACT-COMPANY
               MOVE "P"                        TO PAACT-ACTION-TYPE
               MOVE PA52F2-PCT-EMPLOYEE        TO PAACT-EMPLOYEE
               MOVE PA52F2-PCT-ACTION-CODE     TO PAACT-ACTION-CODE
               MOVE PA52F2-PCT-NEW-EFFECT-DATE TO PAACT-EFFECT-DATE
               MOVE "N"                        TO PAACT-HISTORY
               PERFORM 2300-PAACT-ACTION-NBR

222800         MOVE PA52F2-PCT-COMPANY         TO DB-COMPANY
223300         MOVE "P"                        TO DB-ACTION-TYPE
223500         MOVE PA52F2-PCT-ACTION-CODE     TO DB-ACTION-CODE
223600         MOVE PA52F2-PCT-EFFECT-DATE     TO DB-EFFECT-DATE
223700         MOVE PA52F2-PCT-EMPLOYEE        TO DB-EMPLOYEE
223800         MOVE PA52F2-PCT-ACTION-NBR      TO DB-ACTION-NBR
               PERFORM 840-MODIFY-PCTSET1
               PERFORM 830-DELETE-PERSACTION
               PERFORM 810-RECREATE-PERSACTION
               MOVE PA52F2-PCT-NEW-EFFECT-DATE TO PCT-EFFECT-DATE
               MOVE PAACT-ACTION-NBR           TO PCT-ACTION-NBR
               PERFORM 820-STORE-PERSACTION

               MOVE PA52F2-PCT-COMPANY         TO DB-COMPANY
               MOVE 0                          TO DB-EMP-APP
               MOVE "PA"                       TO DB-CMT-TYPE
               MOVE PA52F2-PCT-ACTION-CODE     TO DB-ACTION-CODE
               MOVE PA52F2-PCT-EFFECT-DATE     TO DB-DATE
               INITIALIZE                         DB-EMPLOYEE
                                                  DB-JOB-CODE
               MOVE PA52F2-PCT-ACTION-NBR      TO DB-LN-NBR
               MOVE PACSET2-LN-NBR             TO WS-DB-BEG-RNG
               PERFORM 850-MODIFY-BEGRNG-PACSET2
               PERFORM
                   UNTIL (PACOMMENTS-NOTFOUND)
                   PERFORM 830-DELETE-PACOMMENTS
                   PERFORM 810-RECREATE-PACOMMENTS
                   MOVE PA52F2-PCT-NEW-EFFECT-DATE TO PAC-DATE
                   MOVE PAACT-ACTION-NBR           TO PAC-LN-NBR
                   PERFORM 820-STORE-PACOMMENTS
                   PERFORM 860-MODIFY-NXTRNG-PACSET2
               END-PERFORM
               MOVE PAACT-ACTION-NBR           TO PA52F2-PCT-ACTION-NBR
               MOVE PA52F2-PCT-NEW-EFFECT-DATE TO PA52F2-PCT-EFFECT-DATE
               INITIALIZE PA52F2-PCT-NEW-EFFECT-DATE
           ELSE
222800         MOVE PA52F2-PCT-COMPANY     TO DB-COMPANY
223300         MOVE "P"                    TO DB-ACTION-TYPE
223500         MOVE PA52F2-PCT-ACTION-CODE TO DB-ACTION-CODE
223600         MOVE PA52F2-PCT-EFFECT-DATE TO DB-EFFECT-DATE
223700         MOVE PA52F2-PCT-EMPLOYEE    TO DB-EMPLOYEE
223800         MOVE PA52F2-PCT-ACTION-NBR  TO DB-ACTION-NBR
224000         PERFORM 840-MODIFY-PCTSET1
               PERFORM 8400-AFTER-FIND-PCT
224100         PERFORM 500-MOVE-DATA
224200         THRU    500-END
224300         PERFORM 8200-STORE-PERSACTION
           END-IF.
224400
224500     PERFORM 920-AUDIT-END.
224600     PERFORM 481-MOVE-TO-SCREEN
224700     THRU    481-END.
224800     MOVE CRT-CHG-COMPLETE       TO CRT-MESSAGE.
224900
225000 420-END.
225100
225200******************************************************************
225300 440-DELETE.
225400******************************************************************
225500
225600     PERFORM 910-AUDIT-BEGIN.
225700     IF (DMS-ABORTED)
225800         GO TO 440-END.
225900
226000     MOVE PA52F2-PCT-COMPANY     TO DB-COMPANY.
226100
226200     IF (PA52F2-PCT-EMPLOYEE NOT = ZEROS)
226300         MOVE "E"                TO DB-ACTION-TYPE
226400     ELSE
226500         MOVE "P"                TO DB-ACTION-TYPE.
226600
226700     MOVE PA52F2-PCT-ACTION-CODE TO DB-ACTION-CODE.
226800     MOVE PA52F2-PCT-EFFECT-DATE TO DB-EFFECT-DATE.
226900     MOVE PA52F2-PCT-EMPLOYEE    TO DB-EMPLOYEE.
227000     MOVE PA52F2-PCT-ACTION-NBR  TO DB-ACTION-NBR.
227100
227200     PERFORM 840-MODIFY-PCTSET1.
227300     PERFORM 830-DELETE-PERSACTION.
227400
227500     MOVE ZEROES                     TO DB-EMP-APP.
227600     MOVE "PA"                       TO DB-CMT-TYPE.
227700     MOVE ZEROS                      TO DB-EMPLOYEE.
227800     MOVE SPACES                     TO DB-JOB-CODE.
227900     MOVE PA52F2-PCT-ACTION-CODE     TO DB-ACTION-CODE.
228000     MOVE PA52F2-PCT-ACTION-NBR      TO DB-LN-NBR.
228100     MOVE PACSET1-LN-NBR             TO WS-DB-BEG-RNG.
228200     PERFORM 830-DELETERNG-PACSET1.
228300
228400     IF (PCT-EMPLOYEE NOT = ZEROS)
228600         MOVE PCT-COMPANY            TO DB-COMPANY
228800         MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE
               PERFORM 4000-PAPCT-EMP-PEND-ACT-DATE.
229700
229800     MOVE CRT-RECS-DELETED       TO CRT-MESSAGE.
229900     PERFORM 920-AUDIT-END.
230000
230100 440-END.
230200
230300******************************************************************
230400 480-INQUIRE.
230500******************************************************************
230600
           INITIALIZE PA52F2-PCT-REASON (1)
                      PA52F2-PCT-REASON (2).

230700     MOVE PA52WS-PRS-NAME        TO PA52F2-PRS-NAME.
231100
231200     PERFORM 481-MOVE-TO-SCREEN
231300     THRU    481-END.

           IF (PERSACTION-NOTFOUND)
               IF  (PA52F2-PCT-REASON (1) = SPACES)
               AND (PA52F2-PCT-REASON (2) = SPACES)
                   IF (PAT-DFT-REASON (1) NOT = SPACES)
                       MOVE PAT-DFT-REASON (1) TO PA52F2-PCT-REASON (1)
                   END-IF
                   IF (PAT-DFT-REASON (2) NOT = SPACES)
                       MOVE PAT-DFT-REASON (2) TO PA52F2-PCT-REASON (2)
                   END-IF
               END-IF
               IF (PAT-DFT-UPD-BN = SPACES)
                   MOVE PA52F2-PCT-COMPANY      TO DB-COMPANY
                   PERFORM 840-KFIND-BNCSET1
                   IF (BNCOMPANY-KFOUND)
                      MOVE "Y" TO PA52F2-PCT-UPDATE-BENEFIT
                   ELSE
                      MOVE "N" TO PA52F2-PCT-UPDATE-BENEFIT
                   END-IF
               ELSE
                   MOVE PAT-DFT-UPD-BN TO PA52F2-PCT-UPDATE-BENEFIT
               END-IF
               IF (PAT-DFT-UPD-LP = SPACES)
J13588*            MOVE PA52F2-PCT-COMPANY     TO EDCDWS-COMPANY
J13588*            MOVE "LP"                   TO EDCDWS-SYSTEM
J13588*            PERFORM 6000-IS-SYSTEM-TRIGGER-ENABLED
J13588*            IF  (EDCDWS-TRIGGER-ENABLED)
                       MOVE "Y" TO PA52F2-PCT-UPD-ABS-MGMT
J13588*            ELSE
J13588*                MOVE "N"        TO PA52F2-PCT-UPD-ABS-MGMT
J13588*            END-IF
               ELSE
                   MOVE PAT-DFT-UPD-LP TO PA52F2-PCT-UPD-ABS-MGMT
               END-IF
               MOVE PAT-HIST-CORR-FLAG TO PA52F2-PCT-HIST-CORR-FLAG.
     
231400     MOVE CRT-INQ-COMPLETE       TO CRT-MESSAGE.
231500
231600 480-END.
231700
231800******************************************************************
231900 481-MOVE-TO-SCREEN.
232000******************************************************************
           INITIALIZE                     PA52WS-FIRST-ACT-NBR
                                          PA52WS-LAST-ACT-NBR.
           INITIALIZE                     PA52F2-MORE-MSG.
           INITIALIZE                     PA52F2-PCT-NEW-EFFECT-DATE.
113200     MOVE PCT-COMPANY            TO PA52F2-PCT-COMPANY.
113300     MOVE PCT-EMPLOYEE           TO PA52F2-PCT-EMPLOYEE.
113400     MOVE PCT-ACTION-CODE        TO PA52F2-PCT-ACTION-CODE.
113500     MOVE PCT-ACTION-NBR         TO PA52F2-PCT-ACTION-NBR.
113700     MOVE PCT-EFFECT-DATE        TO PA52F2-PCT-EFFECT-DATE.
           MOVE PCT-COMPANY            TO DB-COMPANY.
226200     IF (PA52F2-PCT-EMPLOYEE NOT = ZEROS)
226300         MOVE "E"                TO DB-ACTION-TYPE
226400     ELSE
226500         MOVE "P"                TO DB-ACTION-TYPE.
           MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE.
           MOVE PCT-EFFECT-DATE        TO DB-EFFECT-DATE.
           MOVE WS-HIGH-VALUES         TO DB-ACTION-NBR.
           PERFORM 850-FIND-NLT-PCTSET2.
           MOVE PCT-ACTION-NBR         TO PA52WS-LAST-ACT-NBR.
           PERFORM
               UNTIL (PERSACTION-NOTFOUND)
               OR    (PCT-COMPANY     NOT = DB-COMPANY)
               OR    (PCT-ACTION-TYPE NOT = DB-ACTION-TYPE)
               OR    (PCT-EMPLOYEE    NOT = DB-EMPLOYEE)
               OR    (PCT-ACTION-CODE NOT = DB-ACTION-CODE)
               OR    (PCT-EFFECT-DATE NOT = DB-EFFECT-DATE)
                   MOVE PCT-ACTION-NBR TO PA52WS-FIRST-ACT-NBR
                   PERFORM 860-FIND-NEXT-PCTSET2
           END-PERFORM.

           MOVE PA52F2-PCT-COMPANY     TO DB-COMPANY.
226200     IF (PA52F2-PCT-EMPLOYEE NOT = ZEROS)
226300         MOVE "E"                TO DB-ACTION-TYPE
226400     ELSE
226500         MOVE "P"                TO DB-ACTION-TYPE.
           MOVE PA52F2-PCT-EMPLOYEE    TO DB-EMPLOYEE.
           MOVE PA52F2-PCT-ACTION-CODE TO DB-ACTION-CODE.
           MOVE PA52F2-PCT-EFFECT-DATE TO DB-EFFECT-DATE.
           MOVE PA52F2-PCT-ACTION-NBR  TO DB-ACTION-NBR.
           PERFORM 840-FIND-PCTSET2.
           PERFORM 8400-AFTER-FIND-PCT.

232300     MOVE PAT-DESCRIPTION        TO PA52F2-PAT-DESCRIPTION.
232700     MOVE PCT-ANT-END-DATE       TO PA52F2-PCT-ANT-END-DATE.
           IF (PA52WS-FIRST-ACT-NBR NOT = PA52WS-LAST-ACT-NBR)
               MOVE 170                TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE        TO PA52F2-MORE-MSG.

233000     MOVE PCT-UPDATE-BENEFIT     TO PA52F2-PCT-UPDATE-BENEFIT.
           MOVE PCT-UPD-ABS-MGMT       TO PA52F2-PCT-UPD-ABS-MGMT.
233100     MOVE PCT-PROCESS-LEVEL      TO PA52F2-PCT-PROCESS-LEVEL.
233200     MOVE PCT-DEPARTMENT         TO PA52F2-PCT-DEPARTMENT.
233300     MOVE PCT-USER-LEVEL         TO PA52F2-PCT-USER-LEVEL.
233400     MOVE PCT-SUPERVISOR         TO PA52F2-PCT-SUPERVISOR.
233500     MOVE PCT-LOCAT-CODE         TO PA52F2-PCT-LOCAT-CODE.
233600     MOVE PCT-UNION-CODE         TO PA52F2-PCT-UNION-CODE.
233700     MOVE PCT-PERS-GROUP         TO PA52F2-PCT-PERS-GROUP.
233800     MOVE PCT-SALARY-CLASS       TO PA52F2-PCT-SALARY-CLASS.
233900     MOVE PCT-EMPLOYEE           TO PA52F2-PCT-EMPLOYEE.
234000     MOVE PCT-PCT-PAY-INC        TO PA52F2-PCT-PCT-PAY-INC.
234100     MOVE PCT-ROUND-EVEN         TO PA52F2-PCT-ROUND-EVEN.
234200     MOVE PCT-PCT-PAY-DEC        TO PA52F2-PCT-PCT-PAY-DEC.
234300     MOVE PCT-ROUND-OPT          TO PA52F2-PCT-ROUND-OPT.
234400     MOVE PCT-FLAT-INC           TO PA52F2-PCT-FLAT-INC.
234500     MOVE PCT-FLAT-DEC           TO PA52F2-PCT-FLAT-DEC.
234600     MOVE PCT-NEW-RATE           TO PA52F2-PCT-NEW-RATE.
           MOVE PCT-HIST-CORR-FLAG     TO PA52F2-PCT-HIST-CORR-FLAG.
113900     MOVE PCT-REASON (1)         TO PA52F2-PCT-REASON (1).
114000     MOVE PCT-REASON (2)         TO PA52F2-PCT-REASON (2).
234700
234800     MOVE PCT-COMPANY            TO DB-COMPANY.
234900     MOVE ZEROES                 TO DB-EMP-APP.
235000     MOVE "PA"                   TO DB-CMT-TYPE.
235100     MOVE ZEROS                  TO DB-EMPLOYEE.
235200     MOVE SPACES                 TO DB-JOB-CODE.
235300     MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE.
235400     MOVE PCT-ACTION-NBR         TO DB-LN-NBR.
235500     MOVE PACSET1-LN-NBR         TO WS-DB-BEG-RNG.
235600     PERFORM 850-FIND-BEGRNG-PACSET1.
235700
235800     MOVE SPACES                 TO PA52F2-PRS1-NAME.
235900     IF (PCT-PROCESS-LEVEL NOT = SPACES)
236000         MOVE PCT-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
236100         PERFORM 840-FIND-PRSSET1
236200         IF (PRSYSTEM-FOUND)
236300             MOVE PRS-NAME       TO PA52F2-PRS1-NAME.
236400
236500     MOVE SPACES                 TO PA52F2-DPT-NAME.
236600     IF (PCT-DEPARTMENT NOT = SPACES)
236700         MOVE PCT-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
236800         MOVE PCT-DEPARTMENT     TO DB-DEPARTMENT
236900         PERFORM 840-FIND-DPTSET1
237000         IF (DEPTCODE-FOUND)
237100             MOVE DPT-NAME       TO PA52F2-DPT-NAME.
237200
237300     MOVE SPACES                  TO PA52F2-PCO-DESCRIPTION.
237400     IF (PCT-USER-LEVEL NOT = SPACES)
237500         MOVE "UL"                TO DB-TYPE
237600         MOVE PCT-USER-LEVEL      TO DB-CODE
237700         PERFORM 840-FIND-PCOSET1
237800         IF (PCODES-FOUND)
237900             MOVE PCO-DESCRIPTION TO PA52F2-PCO-DESCRIPTION.
238000
238100     MOVE SPACES                  TO PA52F2-PCO1-DESCRIPTION.
238200     IF (PCT-LOCAT-CODE NOT = SPACES)
238300         MOVE "LO"                TO DB-TYPE
238400         MOVE PCT-LOCAT-CODE      TO DB-CODE
238500         PERFORM 840-FIND-PCOSET1
238600         IF (PCODES-FOUND)
238700             MOVE PCO-DESCRIPTION TO PA52F2-PCO1-DESCRIPTION.
238800
238900     MOVE SPACES                  TO PA52F2-PCO2-DESCRIPTION.
239000     IF (PCT-SUPERVISOR NOT = SPACES)
239100         MOVE PCT-SUPERVISOR      TO DB-CODE
239200         PERFORM 840-FIND-HSUSET1
239300         IF (HRSUPER-FOUND)
239400             MOVE HSU-DESCRIPTION TO PA52F2-HSU-DESCRIPTION.
239500
239600     MOVE SPACES                  TO PA52F2-PCO2-DESCRIPTION.
239700     IF (PCT-UNION-CODE NOT = SPACES)
239800         MOVE "UN"                TO DB-TYPE
239900         MOVE PCT-UNION-CODE      TO DB-CODE
240000         PERFORM 840-FIND-PCOSET1
240100         IF (PCODES-FOUND)
240200             MOVE PCO-DESCRIPTION TO PA52F2-PCO2-DESCRIPTION.
240300
240400     MOVE SPACES                  TO PA52F2-PRG-DESCRIPTION.
240500     IF (PCT-PERS-GROUP NOT = SPACES)
240600         MOVE PCT-PERS-GROUP      TO DB-GROUP-NAME
240700         PERFORM 840-FIND-PRGSET1
240800         IF (PERSGROUP-FOUND)
240900             MOVE PRG-DESCRIPTION TO PA52F2-PRG-DESCRIPTION.
241000
241100     MOVE SPACES                 TO PA52F2-EMP-NAME.
241200     IF (PCT-EMPLOYEE NOT = ZEROS)
241300         MOVE PCT-EMPLOYEE       TO DB-EMPLOYEE
241400         PERFORM 840-FIND-EMPSET1
241500         MOVE EMP-LAST-NAME      TO HRWS-LAST-NAME
241600         MOVE EMP-FIRST-NAME     TO HRWS-FIRST-NAME
241700         MOVE EMP-MIDDLE-INIT    TO HRWS-MIDDLE-INIT
               MOVE EMP-NAME-SUFFIX    TO HRWS-NAME-SUFFIX
               MOVE EMP-LAST-NAME-PRE  TO HRWS-LAST-NAME-PRE
241800         PERFORM 750-HR-FORMAT-NAME
241900         MOVE HRWS-FORMAT-NAME   TO PA52F2-EMP-NAME.
242000
090800     MOVE "PAMSG"                TO CRT-ERROR-CAT.
090900     MOVE 100                    TO CRT-MSG-NBR.
091000     PERFORM 790-GET-MSG.
091100     MOVE CRT-MESSAGE            TO PA52F2-COMMENTS.
           INITIALIZE CRT-MESSAGE.

           INITIALIZE PA52F2-COMMENTS-FLAG.
121700     MOVE PA52F2-PCT-COMPANY     TO DB-COMPANY.
121800     INITIALIZE DB-EMP-APP.
121900     MOVE "PA"                   TO DB-CMT-TYPE.
122200     MOVE PA52F2-PCT-ACTION-CODE TO DB-ACTION-CODE.
122000     MOVE PA52F2-PCT-EFFECT-DATE TO DB-DATE.
122000     INITIALIZE DB-EMPLOYEE
122100                DB-JOB-CODE.
122300     MOVE PA52F2-PCT-ACTION-NBR  TO DB-LN-NBR.
122400     MOVE PACSET2-LN-NBR         TO WS-DB-BEG-RNG.
122500     PERFORM 850-KFIND-BEGRNG-PACSET2.
           IF (PACOMMENTS-KFOUND)
               MOVE "*"                TO PA52F2-COMMENTS-FLAG.
122600
243500 481-END.
243600
243700******************************************************************
243800 500-MOVE-DATA.
243900******************************************************************
244000
244100     MOVE PA52F2-PCT-COMPANY         TO PCT-COMPANY.
244200
244300     IF (PA52F2-PCT-EMPLOYEE NOT = ZEROS)
244400         MOVE "E"                    TO PCT-ACTION-TYPE
244500     ELSE
244600         MOVE "P"                    TO PCT-ACTION-TYPE.
244700
244800     MOVE PA52F2-PCT-ACTION-CODE     TO PCT-ACTION-CODE.
244900     MOVE PA52F2-PCT-EFFECT-DATE     TO PCT-EFFECT-DATE.
245000     MOVE PA52F2-PCT-ANT-END-DATE    TO PCT-ANT-END-DATE.
245100     MOVE PA52F2-PCT-REASON (1)      TO PCT-REASON (1).
245200     MOVE PA52F2-PCT-REASON (2)      TO PCT-REASON (2).
245300     MOVE PA52F2-PCT-UPDATE-BENEFIT  TO PCT-UPDATE-BENEFIT.
           MOVE PA52F2-PCT-UPD-ABS-MGMT    TO PCT-UPD-ABS-MGMT.
245400     MOVE PA52F2-PCT-PROCESS-LEVEL   TO PCT-PROCESS-LEVEL.
245500     MOVE PA52F2-PCT-DEPARTMENT      TO PCT-DEPARTMENT.
245600     MOVE PA52F2-PCT-USER-LEVEL      TO PCT-USER-LEVEL.
245700     MOVE PA52F2-PCT-SUPERVISOR      TO PCT-SUPERVISOR.
245800     MOVE PA52F2-PCT-LOCAT-CODE      TO PCT-LOCAT-CODE.
245900     MOVE PA52F2-PCT-UNION-CODE      TO PCT-UNION-CODE.
246000     MOVE PA52F2-PCT-PERS-GROUP      TO PCT-PERS-GROUP.
246100     MOVE PA52F2-PCT-SALARY-CLASS    TO PCT-SALARY-CLASS.
246200     MOVE PA52F2-PCT-EMPLOYEE        TO PCT-EMPLOYEE.
246300     MOVE PA52F2-PCT-PCT-PAY-INC     TO PCT-PCT-PAY-INC.
246400     MOVE PA52F2-PCT-PCT-PAY-DEC     TO PCT-PCT-PAY-DEC.
246500     MOVE PA52F2-PCT-ROUND-OPT       TO PCT-ROUND-OPT.
246600     MOVE PA52F2-PCT-FLAT-INC        TO PCT-FLAT-INC.
246700     MOVE PA52F2-PCT-FLAT-DEC        TO PCT-FLAT-DEC.
246800     MOVE PA52F2-PCT-NEW-RATE        TO PCT-NEW-RATE.
           MOVE PA52F2-PCT-HIST-CORR-FLAG  TO PCT-HIST-CORR-FLAG.

246900     MOVE 1                          TO PCT-POS-LEVEL.
           IF  (PA52F2-USER-ID NOT = SPACES)
               MOVE PA52F2-USER-ID         TO PCT-USER-ID
           ELSE
               MOVE CRT-USER-NAME          TO PCT-USER-ID
           END-IF.
           MOVE SPACES                     TO PCT-UPDATE-REQ-DED.
           MOVE ZEROES                     TO PCT-EDM-EFFECT-DT.
           MOVE ZEROES                     TO PCT-EDM-END-DATE.
J08104     IF  (PA52F2-FC = "A")
J08104         IF  (PA52F2-USER-ID NOT = SPACES)
J08104             MOVE PA52F2-USER-ID          TO PCT-CREATE-USER-ID
J08104         ELSE
J08104             MOVE CRT-USER-NAME           TO PCT-CREATE-USER-ID
J08104         END-IF
J08104         MOVE WS-SYSTEM-DATE-YMD          TO PCT-CREATE-DATE
J08104         MOVE HHMMSS                      TO PCT-CREATE-TIME
J08104     END-IF.
J08104     MOVE WS-SYSTEM-DATE-YMD              TO PCT-DATE-STAMP.
J08104     MOVE HHMMSS                          TO PCT-TIME-STAMP.
P86027     IF  (PA52F2-FC = "A")
               MOVE SPACES                 TO PCT-HOLD-FLAG.
247100
247200     IF  (PA52F2-PCT-ROUND-OPT       = "H" OR "R")
247300     AND (PA52F2-PCT-ROUND-EVEN      = ZEROES)
247400         MOVE .001                   TO PCT-ROUND-EVEN
247500     ELSE
247600         MOVE PA52F2-PCT-ROUND-EVEN  TO PCT-ROUND-EVEN.
247700
247800     IF (PA52F2-PCT-EMPLOYEE NOT = ZEROS)
247900         PERFORM 510-CALCULATE-NEW-RATE
248000         THRU    510-END
248100         MOVE "Y"                    TO HREMP-FORMAT-FIELD
248200         MOVE SPACES                 TO HRWS-VALUE
248300         MOVE ZEROS                  TO HRWS-DATE-FIELD
248400         MOVE ZEROS                  TO HRWS-DATE-8-FIELD
248500         MOVE PA52WS-NEW-PAY-RATE    TO HRWS-DEC-FIELD
248600         MOVE 4                      TO HRWS-NBR-DECIMALS
               MOVE HREMP-PAY-RATE-DN      TO HREMP-FLD-NBR
248700         PERFORM 5000-HREMP-FORMAT-FIELD
248800         PERFORM
248900             VARYING I1 FROM 1 BY 1
249000             UNTIL  (I1 > 36)
249100
249300             MOVE PAT-FLD-NBR (I1)       TO PCT-FLD-NBR (I1)
249200             IF (PAT-FLD-NBR (I1) = HREMP-PAY-RATE-DN)
249400                 MOVE HREMP-VALUE        TO PCT-NEW-VALUE (I1)
249500             END-IF
249600         END-PERFORM
               MOVE PCT-EMPLOYEE       TO DB-EMPLOYEE
               PERFORM 840-FIND-EMPSET1
               MOVE PA52WS-NEW-PAY-RATE    TO PCT-BASE-PAY-RATE
               MOVE EMP-CURRENCY-CODE      TO PCT-BASE-CURRENCY
               MOVE EMP-CURR-ND            TO PCT-BASE-ND
           END-IF.
249700
           MOVE "Y"                        TO PCT-APPROVAL-FLAG.

249800 500-END.
249900
250000*****************************************************************
250100 510-CALCULATE-NEW-RATE.
250200*****************************************************************
250300
250400     MOVE ZEROS                  TO PA52WS-AMOUNT.
250500
250600     IF (PCT-PCT-PAY-INC NOT = ZEROS)
250700         COMPUTE PA52WS-PERCENT      = (PCT-PCT-PAY-INC / 100)
197195*        COMPUTE PA52WS-AMOUNT       = (EMP-PAY-RATE
197195*                                    *  PA52WS-PERCENT)
197195*        COMPUTE PA52WS-NEW-PAY-RATE = (EMP-PAY-RATE
197195*                                    +  PA52WS-AMOUNT)
197195         IF  (PA52WS-ROUND-EVEN-A (8:1) = 1)
197195         AND (PCT-ROUND-OPT          = "R")
197195             COMPUTE PA52WS-NEW-PAY-RATE ROUNDED
197195                                     =  EMP-PAY-RATE
197195                                     * (1 + PA52WS-PERCENT)
197195         ELSE
197195             COMPUTE PA52WS-NEW-PAY-RATE
197195                                     =  EMP-PAY-RATE
197195                                     * (1 + PA52WS-PERCENT)
197195         END-IF
197195
251200         PERFORM 512-ROUNDING
251300         THRU    512-END.
251400
251500     IF (PCT-PCT-PAY-DEC NOT = ZEROS)
251600         COMPUTE PA52WS-PERCENT      = (PCT-PCT-PAY-DEC / 100)
197195*        COMPUTE PA52WS-AMOUNT       = (EMP-PAY-RATE
197195*                                    *  PA52WS-PERCENT)
197195*        COMPUTE PA52WS-NEW-PAY-RATE = (EMP-PAY-RATE
197195*                                    -  PA52WS-AMOUNT)
197195         IF  (PA52WS-ROUND-EVEN-A (8:1) = 1)
197195         AND (PCT-ROUND-OPT          = "R")
197195             COMPUTE PA52WS-NEW-PAY-RATE ROUNDED
197195                                     =  EMP-PAY-RATE
197195                                     * (1 - PA52WS-PERCENT)
197195         ELSE
197195             COMPUTE PA52WS-NEW-PAY-RATE
197195                                     =  EMP-PAY-RATE
197195                                     * (1 - PA52WS-PERCENT)
197195         END-IF
197195
252100         PERFORM 512-ROUNDING
252200         THRU    512-END.
252300
252400     IF (PCT-FLAT-INC NOT = ZEROS)
252500         COMPUTE PA52WS-NEW-PAY-RATE = (EMP-PAY-RATE
252600                                     +  PCT-FLAT-INC).
252700
252800     IF (PCT-FLAT-DEC NOT = ZEROS)
252900         COMPUTE PA52WS-NEW-PAY-RATE = (EMP-PAY-RATE
253000                                     -  PCT-FLAT-DEC).
253100
253200     IF (PCT-NEW-RATE NOT = ZEROS)
253300         MOVE PCT-NEW-RATE       TO PA52WS-NEW-PAY-RATE.
253400
253500 510-END.
253600
253700*****************************************************************
253800 512-ROUNDING.
253900*****************************************************************
254000*
254100***
254200*** IF ROUNDING OPTION IS:
254300***    SPACES - THEN ROUNDED TO NEAREST TENTH CENT FOR HOURLY
254400***             NEAREST DOLLAR FOR SALARY EMPLOYEE.
254500***    R      - THEN NORMAL ROUNDING DEPENDING WHERE THEY SET
254600***             SET THE ROUNDING TO OCCUR.
254700***    H      - THE AMOUNT WILL BE ROUNDED TO THE NEXT HIGHER AMT
254800***             INDICATED BY THE ROUND TO EVEN FIELD.
254900***    N      - NO ROUNDING WILL TAKE PLACE THE PAY RATE WILL BE
255000***             TRUNCATED AFTER THE CENTS FIELDS.
255100***
255200*
255300*
255400     IF (PCT-ROUND-OPT         = SPACES)
255500         IF (EMP-SALARY-CLASS  = "H")
255600             IF (PA52WS-TENTH-CENT NOT < 5)
255700                 ADD .01         TO PA52WS-NEW-PAY-RATE
255800                 MOVE ZEROS      TO PA52WS-TENTH-CENT
255900                 MOVE ZEROS      TO PA52WS-HUNDREDTH-CENT
256000             ELSE
256100                 MOVE ZEROS      TO PA52WS-TENTH-CENT
256200                 MOVE ZEROS      TO PA52WS-HUNDREDTH-CENT
256300             END-IF
256400         ELSE
256500             IF (PA52WS-CENT-A NOT < 5)
256600                 ADD 1           TO PA52WS-NEW-PAY-RATE
256700                 MOVE ZEROS      TO PA52WS-CENT-A
256800                                    PA52WS-CENT-B
256900                                    PA52WS-TENTH-CENT
257000                                    PA52WS-HUNDREDTH-CENT
257100             ELSE
257200                 MOVE ZEROS      TO PA52WS-CENT-A
257300                                    PA52WS-CENT-B
257400                                    PA52WS-TENTH-CENT
257500                                    PA52WS-HUNDREDTH-CENT.
257600
257700     IF (PCT-ROUND-OPT = SPACES)
257800         GO TO 512-END.
257900
258000     IF (PCT-ROUND-EVEN = ZEROS)
258100         GO TO 512-END
258200     ELSE
258300         MOVE PCT-ROUND-EVEN     TO PA52WS-ROUND-EVEN.
258400
258500     IF (PCT-ROUND-OPT = "R")
258600         PERFORM 514-ROUNDING-R-OPTION
258700         THRU    514-END.
258800
258900     IF (PCT-ROUND-OPT = "H")
259000         PERFORM 516-ROUNDING-H-OPTION
259100         THRU    516-END.
259200
259300     IF (PCT-ROUND-OPT = "N")
259400         PERFORM 518-ROUNDING-N-OPTION
259500         THRU    518-END.
259600
259700 512-END.
259800
259900*****************************************************************
260000 514-ROUNDING-R-OPTION.
260100*****************************************************************
260200***
260300*** THIS SECTION WILL DO THE ROUNDING WHEN ROUNDING OPTION IS AN
260400*** 'R'.
260500***
260600*** THIS SPECIAL CONDITION IS HERE IN CASE THEY TRY TO ROUND A
260700*** SALARIED PERSON TO A FIELD LESS THAN ONES.
260800***
260900*
261000     IF (PA52WS-RND-CENT-A         NOT = ZEROS)
261100     OR (PA52WS-RND-CENT-B         NOT = ZEROS)
261200     OR (PA52WS-RND-TENTH-CENT     NOT = ZEROS)
261300     OR (PA52WS-RND-HUNDREDTH-CENT NOT = ZEROS)
261400         IF (EMP-SALARY-CLASS  = "S")
261500             MOVE PA52WS-NEW-PAY-RATE    TO PA52WS-NODECIMAL-AMT
261600             MOVE PA52WS-NODECIMAL-AMT   TO PA52WS-NEW-PAY-RATE
261700             GO TO 514-END.
261800
261900     COMPUTE PA52WS-NODECIMAL-AMT    ROUNDED
262000                                 = (PA52WS-NEW-PAY-RATE
262100                                 /  PA52WS-ROUND-EVEN).
262200     COMPUTE PA52WS-DECIMAL-AMT      = (PA52WS-NODECIMAL-AMT
262300                                 *  PA52WS-ROUND-EVEN).
262400
262500     MOVE PA52WS-DECIMAL-AMT         TO PA52WS-NEW-PAY-RATE.
262600
262700 514-END.
262800
262900*****************************************************************
263000 516-ROUNDING-H-OPTION.
263100*****************************************************************
263200***
263300*** THIS SECTION WILL DO THE ROUNDING WHEN ROUNDING OPTION IS AN
263400*** 'H'.
263500***
263600
197195     IF (PA52WS-ROUND-EVEN         > PA52WS-NEW-PAY-RATE)
197195         GO TO 516-END
197195     ELSE
197195     IF (PA52WS-ROUND-EVEN-A (8:1) = 1)
197195         ADD PA52WS-ROUND-EVEN      TO PA52WS-NEW-PAY-RATE
197195     ELSE
197195     IF (PA52WS-ROUND-EVEN-A (7:1) = 1)
197195         ADD PA52WS-ROUND-EVEN      TO PA52WS-NEW-PAY-RATE
197195         MOVE ZEROES TO PA52WS-NEW-PAY-RATE (13:1)
197195     ELSE
197195     IF (PA52WS-ROUND-EVEN-A (6:1) = 1)
197195         ADD PA52WS-ROUND-EVEN      TO PA52WS-NEW-PAY-RATE
197195         MOVE ZEROES TO PA52WS-NEW-PAY-RATE (12:2)
197195     ELSE
197195     IF (PA52WS-ROUND-EVEN-A (5:1) = 1)
197195         ADD PA52WS-ROUND-EVEN      TO PA52WS-NEW-PAY-RATE
197195         MOVE ZEROES TO PA52WS-NEW-PAY-RATE (11:3)
197195     ELSE
197195     IF (PA52WS-ROUND-EVEN-A (4:1) = 1)
197195         ADD PA52WS-ROUND-EVEN      TO PA52WS-NEW-PAY-RATE
197195         MOVE ZEROES TO PA52WS-NEW-PAY-RATE (10:4)
197195     ELSE
197195     IF (PA52WS-ROUND-EVEN-A (3:1) = 1)
197195         ADD PA52WS-ROUND-EVEN      TO PA52WS-NEW-PAY-RATE
197195         MOVE ZEROES TO PA52WS-NEW-PAY-RATE (09:5)
197195     ELSE
197195     IF (PA52WS-ROUND-EVEN-A (2:1) = 1)
197195         ADD PA52WS-ROUND-EVEN      TO PA52WS-NEW-PAY-RATE
197195         MOVE ZEROES TO PA52WS-NEW-PAY-RATE (08:6)
197195     ELSE
197195     IF (PA52WS-ROUND-EVEN-A (1:1) = 1)
197195         ADD PA52WS-ROUND-EVEN      TO PA52WS-NEW-PAY-RATE
197195         MOVE ZEROES TO PA52WS-NEW-PAY-RATE (07:7)
197195     END-IF
197195     END-IF
197195     END-IF
197195     END-IF
197195     END-IF
197195     END-IF
197195     END-IF
197195     END-IF
197195     END-IF.
197195
263700*     COMPUTE PA52WS-AMT1         = (PA52WS-NEW-PAY-RATE * 1000).
263800*
263900*     COMPUTE PA52WS-NODECIMAL-AMT    = (PA52WS-NEW-PAY-RATE
264000*                                 /  PCT-ROUND-EVEN).
264100*
264200*     COMPUTE PA52WS-AMT4             = (PA52WS-NODECIMAL-AMT
264300*                                 *  PCT-ROUND-EVEN).
264400*
264500*     COMPUTE PA52WS-AMT2             = (PA52WS-AMT4 * 1000).
264600*
264700*     COMPUTE PA52WS-AMT3         = (PA52WS-AMT1 - PA52WS-AMT2).
264800*
264900*     IF (PA52WS-AMT3 NOT = ZEROS)
265000*         COMPUTE PA52WS-NEW-PAY-RATE = (PA52WS-AMT4
265100*                                     +  PCT-ROUND-EVEN).
265200
265300 516-END.
265400
265500*****************************************************************
265600 518-ROUNDING-N-OPTION.
265700*****************************************************************
265800***
265900*** THIS SECTION WILL DO THE TRUNCATING WHEN ROUNDING OPTION IS
266000*** AN 'N'.
266100***
266200
266300     COMPUTE PA52WS-NODECIMAL-AMT    = (PA52WS-NEW-PAY-RATE
266400                                 /  PCT-ROUND-EVEN).
266500
266600     COMPUTE PA52WS-NEW-PAY-RATE     = (PA52WS-NODECIMAL-AMT
266700                                 *  PCT-ROUND-EVEN).
266800
266900 518-END.
267000
267100******************************************************************
267200 PA52S2-TRANSACTION-END.
267300******************************************************************
267400******************************************************************
267500 PA52S3-TRANSACTION SECTION 32.
267600******************************************************************
267700 PA52S3-START.
267800
           MOVE "PA52.3"               TO PA52F3-CALLED-FROM.
           IF  (PA52F3-FC NOT = "A" AND "C")
               INITIALIZE PA52F3-XMIT-CURR
               INITIALIZE PA52F3-XMIT-ACT-EXISTS
           END-IF.
P40376     MOVE "PA52"                 TO PA52F3-PGM-NAME.

267900     PERFORM 200-EDIT-TRAN
268000     THRU    200-END.
268100
268200     IF (NO-ERROR-FOUND)
               INITIALIZE PA52F3-XMIT-CURR
                          PA52F3-XMIT-ACT-EXISTS
268300         PERFORM 400-PROCESS-TRAN
268400         THRU    400-END.
268500
268600     GO TO PA52S3-TRANSACTION-END.
268700
268800******************************************************************
268900 200-EDIT-TRAN.
269000******************************************************************
269100
269200     PERFORM 210-EDIT-ACCESS
269300     THRU    210-END.
269400
269500     IF (ERROR-FOUND)
269600         GO TO 200-END.
269700
269800     IF (PA52F3-FC = "A" OR "C")
269900         PERFORM 220-EDIT-DATA
270000         THRU    220-END
270100         GO TO 200-END.
270200
270300 200-END.
270400
270500******************************************************************
270600 210-EDIT-ACCESS.
270700******************************************************************
270800
           INITIALIZE                  HREMP-UPDPEP-DATE.

270900     MOVE "M"                    TO PA52F3-PT-ACTION-TYPE.
271000
271100     IF  (PA52F3-FC              = "A")
271200     AND ((PA52F3-PCT-COMPANY    NOT = PA52F3-PT-PCT-COMPANY)
271300     OR  (PA52F3-PCT-ACTION-CODE NOT = PA52F3-PT-PCT-ACTION-CODE))
271400         MOVE 10                                 TO CRT-ERROR-NBR
271500         MOVE PA52F3-FC-FN                       TO CRT-FIELD-NBR
271600         GO TO 210-END.
271700
271800     MOVE PA52F3-PCT-COMPANY     TO DB-COMPANY.
271900     MOVE SPACES                 TO DB-PROCESS-LEVEL.
272000     PERFORM 840-FIND-PRSSET1.
272100     IF (PRSYSTEM-NOTFOUND)
272200         MOVE 100                                TO CRT-ERROR-NBR
272300         MOVE PA52F3-PCT-COMPANY-FN              TO CRT-FIELD-NBR
272400         GO TO 210-END.
272500
273100     MOVE PRS-NAME               TO PA52WS-PRS-NAME.
273300
273400     IF (PA52F3-FC = "A" OR "C" OR "I")
273500         MOVE PA52F3-PCT-COMPANY     TO DB-COMPANY
273600         MOVE PA52F3-PCT-ACTION-CODE TO DB-ACTION-CODE
273700         PERFORM 840-FIND-PATSET1
273800         IF (PERSACTYPE-NOTFOUND)
273900             MOVE 110                            TO CRT-ERROR-NBR
274000             MOVE PA52F3-PCT-ACTION-CODE-FN      TO CRT-FIELD-NBR
274100             GO TO 210-END
               ELSE
                   IF (PAT-ACTIVE-FLAG = "2")
                       MOVE 222                       TO CRT-ERROR-NBR
                       MOVE PAT-ACTION-CODE           TO CRT-ERR-VAR1
                       MOVE PA52F3-PCT-ACTION-CODE-FN TO CRT-FIELD-NBR 
                       GO TO 210-END
                   END-IF.
274200
274300     MOVE "M"                    TO DB-ACTION-TYPE.
274400     MOVE PA52F3-PCT-EFFECT-DATE TO DB-EFFECT-DATE.
274500     MOVE PA52F3-PCT-ACTION-CODE TO DB-ACTION-CODE.
274600     INITIALIZE                     DB-EMPLOYEE.
274700     MOVE PA52F3-PCT-ACTION-NBR  TO DB-ACTION-NBR.
274800
274900     IF (PA52F3-FC = "N")
275000         IF (DB-EFFECT-DATE = ZEROES)
275100             MOVE WS-HIGH-VALUES TO DB-ACTION-NBR
275200             PERFORM 850-FIND-NLT-PCTSET2
275300         ELSE
                   PERFORM 850-FIND-NLT-PCTSET2
                   IF  (PCT-COMPANY     = DB-COMPANY)
                   AND (PCT-ACTION-TYPE = DB-ACTION-TYPE)
                   AND (PCT-EMPLOYEE    = DB-EMPLOYEE)
                   AND (PCT-ACTION-CODE = DB-ACTION-CODE)
                   AND (PCT-EFFECT-DATE = DB-EFFECT-DATE)
                   AND (PCT-ACTION-NBR  = DB-ACTION-NBR)
176100                 PERFORM 860-FIND-NEXT-PCTSET2
                   END-IF
275500         END-IF
275600     ELSE
275700     IF (PA52F3-FC = "P")
275800         PERFORM 850-FIND-NLT-PCTSET2
275900         PERFORM 870-FIND-PREV-PCTSET2.
276000
276100     IF (PA52F3-FC = "N" OR "P")
276200         IF (PERSACTION-NOTFOUND)
276300         OR (PCT-COMPANY     NOT = DB-COMPANY)
276400         OR (PCT-ACTION-TYPE NOT = DB-ACTION-TYPE)
276500             MOVE 12                     TO CRT-ERROR-NBR
276600             MOVE PA52F3-FC-FN           TO CRT-FIELD-NBR
276700             GO TO 210-END
276800         ELSE
276900             MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE
277000                                            PA52F3-PCT-ACTION-CODE
277100             MOVE PCT-EFFECT-DATE        TO DB-EFFECT-DATE
277200                                           PA52F3-PCT-EFFECT-DATE
277300             MOVE PCT-ACTION-NBR         TO DB-ACTION-NBR
277400                                            PA52F3-PCT-ACTION-NBR
277500             PERFORM 840-FIND-PATSET1
277600             IF (PERSACTYPE-NOTFOUND)
277700                 MOVE 110                TO CRT-ERROR-NBR
277800                 MOVE PA52F3-PCT-ACTION-CODE-FN
277900                                         TO CRT-FIELD-NBR
278000                 GO TO 210-END
                   END-IF.
278100
278200     IF (PA52F3-FC = "N" OR "P")
               PERFORM 8400-AFTER-FIND-PCT
278300         GO TO 210-END.
278400
           PERFORM 840-FIND-PCTSET2.

           IF  (PA52F3-FC = "I")
           AND (PERSACTION-NOTFOUND)
               MOVE WS-HIGH-VALUES      TO DB-ACTION-NBR
               PERFORM 850-FIND-NLT-PCTSET2.

           IF  (PA52F3-FC = "C" OR "D")
           AND (PERSACTION-NOTFOUND)
               MOVE 125                                TO CRT-ERROR-NBR
               MOVE PA52F3-PCT-ACTION-CODE-FN          TO CRT-FIELD-NBR
               GO TO 210-END.

           PERFORM 8400-AFTER-FIND-PCT.

280100 210-END.
280200
280300******************************************************************
280400 220-EDIT-DATA.
280500******************************************************************
280600
           SET PA52WS-NOTHING-ENTERED  TO TRUE.
           SET PA52WS-FIELDS-EXIST     TO TRUE.
280900

           IF  (PA52F3-FC              = "A")
           AND (PA52F3-XMIT-ACT-EXISTS  = ZEROS)
               MOVE PA52F3-PCT-COMPANY     TO DB-COMPANY
               MOVE PA52F3-PCT-ACTION-CODE TO DB-ACTION-CODE
               MOVE "M"                    TO DB-ACTION-TYPE
               MOVE PA52F3-PCT-EFFECT-DATE TO DB-EFFECT-DATE
               MOVE PCTSET3-EFFECT-DATE    TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PCTSET3
               IF (PERSACTION-FOUND)
      ******** Warning, ADDING DUPLICATE RECORD.
               MOVE 1                      TO PA52F3-XMIT-ACT-EXISTS
               MOVE 261                    TO CRT-ERROR-NBR
               GO TO 220-END.

281000     IF  (PA52F3-PCT-ANT-END-DATE NOT = ZEROS)
281100     AND (PA52F3-PCT-ANT-END-DATE NOT > PA52F3-PCT-EFFECT-DATE)
281200         MOVE 233                                TO CRT-ERROR-NBR
281300         MOVE PA52F3-PCT-ANT-END-DATE-FN         TO CRT-FIELD-NBR
281400         GO TO 220-END.
281500
J13588*    MOVE PA52F3-PCT-COMPANY     TO EDCDWS-COMPANY.
J13588*    MOVE "LP"                   TO EDCDWS-SYSTEM.
J13588*    PERFORM 6000-IS-SYSTEM-TRIGGER-ENABLED.
J13588*    IF  (NOT EDCDWS-TRIGGER-ENABLED)
J13588*        IF (PA52F3-PCT-UPD-ABS-MGMT = "Y")
J13588*            MOVE 234            TO CRT-ERROR-NBR
J13588*            MOVE PA52F3-PCT-UPD-ABS-MGMT-FN
J13588*                                TO CRT-FIELD-NBR
J13588*            GO TO 220-END
J13588*        END-IF
J13588*        IF (PA52F3-PCT-UPD-ABS-MGMT = SPACES)
J13588*            MOVE "N"            TO PA52F3-PCT-UPD-ABS-MGMT
J13588*        END-IF.

110700     PERFORM 840-FIND-BNCSET1.
           IF  (BNCOMPANY-NOTFOUND)
           AND (PA52F3-PCT-UPDATE-BENEFIT  = "Y")
      ******** Cannot update benefit; Company is not setup in benefit system
028700         MOVE 180                                TO CRT-ERROR-NBR
028800         MOVE PA52F3-PCT-UPDATE-BENEFIT-FN       TO CRT-FIELD-NBR
028900         GO TO 220-END.

           IF (PAT-REQUIRE-REASON = 2)
               IF (PA52F3-PCT-REASON (1) = SPACES)
               OR (PA52F3-PCT-REASON (2) = SPACES)
                   MOVE 544                        TO CRT-ERROR-NBR
                   MOVE PAT-REQUIRE-REASON         TO CRT-ERR-VAR1
                   IF (PA52F3-PCT-REASON (2) = SPACES)
                       MOVE PA52F3-PCT-REASON-FN (2)   TO CRT-FIELD-NBR
                   END-IF
                   IF (PA52F3-PCT-REASON (1) = SPACES)
                       MOVE PA52F3-PCT-REASON-FN (1)   TO CRT-FIELD-NBR
                   END-IF
                   GO TO 220-END.

           IF (PAT-REQUIRE-REASON = 1)
               IF  (PA52F3-PCT-REASON (1) = SPACES)
               AND (PA52F3-PCT-REASON (2) = SPACES)
                   MOVE 544                        TO CRT-ERROR-NBR
                   MOVE PAT-REQUIRE-REASON         TO CRT-ERR-VAR1
                   IF (PA52F3-PCT-REASON (1) = SPACES)
                       MOVE PA52F3-PCT-REASON-FN (1)   TO CRT-FIELD-NBR
                   END-IF
                   GO TO 220-END.

           IF  (PA52F3-PCT-REASON (1) = PA52F3-PCT-REASON (2))
           AND (PA52F3-PCT-REASON (1) NOT = SPACES)
               MOVE 194                            TO CRT-ERROR-NBR
               MOVE PA52F3-PCT-REASON-FN (2)       TO CRT-FIELD-NBR
               GO TO 220-END.

029300     PERFORM 222-EDIT-REASONS
029400     THRU    222-END
029500         VARYING I1 FROM 1 BY 1
029600         UNTIL  (I1 > 2)
029700         OR     (ERROR-FOUND).
282100
282200     IF (ERROR-FOUND)
282300         GO TO 220-END.
282400
029900     IF (ERROR-FOUND)
030000         GO TO 220-END.
030100
282500     IF (PA52F3-PCT-PROCESS-LEVEL NOT = SPACES)
282600         MOVE PA52F3-PCT-PROCESS-LEVEL   TO DB-PROCESS-LEVEL
282700         PERFORM 840-FIND-PRSSET1
282800         IF (PRSYSTEM-NOTFOUND)
282900             MOVE 165                            TO CRT-ERROR-NBR
283000             MOVE PA52F3-PCT-PROCESS-LEVEL-FN    TO CRT-FIELD-NBR
283100             GO TO 220-END.
283200
283300     IF (PA52F3-PCT-DEPARTMENT NOT = SPACES)
283400         MOVE PA52F3-PCT-PROCESS-LEVEL   TO DB-PROCESS-LEVEL
283500         MOVE PA52F3-PCT-DEPARTMENT      TO DB-DEPARTMENT
283600         PERFORM 840-FIND-DPTSET1
283700         IF (DEPTCODE-NOTFOUND)
283800             MOVE 166                            TO CRT-ERROR-NBR
283900             MOVE PA52F3-PCT-DEPARTMENT-FN       TO CRT-FIELD-NBR
284000             GO TO 220-END.
284100
284200     IF (PA52F3-PCT-USER-LEVEL NOT = SPACES)
284300         MOVE "UL"                   TO DB-TYPE
284400         MOVE PA52F3-PCT-USER-LEVEL  TO DB-CODE
284500         PERFORM 840-FIND-PCOSET1
284600         IF (PCODES-NOTFOUND)
284700             MOVE 167                            TO CRT-ERROR-NBR
284800             MOVE PA52F3-PCT-USER-LEVEL-FN       TO CRT-FIELD-NBR
284900             GO TO 220-END.
285000
285100     IF (PA52F3-PCT-LOCAT-CODE NOT = SPACES)
285200         MOVE PA52F3-PCT-LOCAT-CODE  TO DB-CODE
285300         MOVE "LO"                   TO DB-TYPE
285400         PERFORM 840-FIND-PCOSET1
285500         IF (PCODES-NOTFOUND)
285600             MOVE 172                            TO CRT-ERROR-NBR
285700             MOVE PA52F3-PCT-LOCAT-CODE-FN       TO CRT-FIELD-NBR
285800             GO TO 220-END.
285900
286000     IF (PA52F3-PCT-SUPERVISOR NOT = SPACES)
286100         MOVE PA52F3-PCT-SUPERVISOR  TO DB-CODE
286200         PERFORM 840-FIND-HSUSET1
286300         IF (HRSUPER-NOTFOUND)
286400             MOVE 168                            TO CRT-ERROR-NBR
286500             MOVE PA52F3-PCT-SUPERVISOR-FN       TO CRT-FIELD-NBR
286600             GO TO 220-END.
286700
286800     IF (PA52F3-PCT-PERS-GROUP NOT = SPACES)
286900         MOVE PA52F3-PCT-PERS-GROUP  TO DB-GROUP-NAME
287000         PERFORM 840-FIND-PRGSET1
287100         IF (PERSGROUP-NOTFOUND)
287200             MOVE 197                            TO CRT-ERROR-NBR
287300             MOVE PA52F3-PCT-PERS-GROUP-FN       TO CRT-FIELD-NBR
287400             GO TO 220-END
               ELSE
                   IF (PRG-UPDATE-FLAG = "Y")
                       MOVE 543                        TO CRT-ERROR-NBR
                       MOVE PA52F3-PCT-PERS-GROUP-FN   TO CRT-FIELD-NBR
                       GO TO 220-END
                   END-IF.
287500
           IF  (PA52F3-FC                  = "A")
           AND (PA52F3-PCT-NEW-EFFECT-DATE NOT = ZEROES)
               MOVE 122                                TO CRT-ERROR-NBR
               MOVE PA52F3-PCT-NEW-EFFECT-DATE-FN      TO CRT-FIELD-NBR
               GO TO 220-END.

           IF (PA52F3-PCT-HIST-CORR-FLAG = SPACES)
               IF (PAT-HIST-CORR-FLAG NOT = SPACES)
                   MOVE PAT-HIST-CORR-FLAG TO PA52F3-PCT-HIST-CORR-FLAG
               ELSE
                   MOVE 123                            TO CRT-ERROR-NBR
                   MOVE PA52F3-PCT-HIST-CORR-FLAG-FN   TO CRT-FIELD-NBR
                   GO TO 220-END
               END-IF
           END-IF.

           IF  (PA52F3-FC = "C")
           AND (PA52F3-PCT-NEW-EFFECT-DATE   NOT = ZEROES)
               IF (PA52F3-PCT-REASON (1)     NOT = PCT-REASON (1))
               OR (PA52F3-PCT-REASON (2)     NOT = PCT-REASON (2))
               OR (PA52F3-PCT-UPDATE-BENEFIT NOT = PCT-UPDATE-BENEFIT)
               OR (PA52F3-PCT-UPD-ABS-MGMT   NOT = PCT-UPD-ABS-MGMT)
               OR (PA52F3-PCT-ANT-END-DATE   NOT = PCT-ANT-END-DATE)
               OR (PA52F3-PCT-HIST-CORR-FLAG NOT = PCT-HIST-CORR-FLAG)
               OR (PA52F3-PCT-PERS-GROUP     NOT = PCT-PERS-GROUP)
               OR (PA52F3-PCT-PROCESS-LEVEL  NOT = PCT-PROCESS-LEVEL)
               OR (PA52F3-PCT-DEPARTMENT     NOT = PCT-DEPARTMENT)
               OR (PA52F3-PCT-USER-LEVEL     NOT = PCT-USER-LEVEL)
               OR (PA52F3-PCT-SUPERVISOR     NOT = PCT-SUPERVISOR)
               OR (PA52F3-PCT-LOCAT-CODE     NOT = PCT-LOCAT-CODE)
               OR (PA52F3-PCT-HIST-CORR-FLAG NOT = PCT-HIST-CORR-FLAG)
                   MOVE PA52F3-PCT-NEW-EFFECT-DATE-FN  TO CRT-FIELD-NBR
                   IF  (PA52F3-PCT-REASON (1)  NOT = PCT-REASON (1))
                       MOVE PA52F3-PCT-REASON-FN (1)   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F3-PCT-REASON (2)  NOT = PCT-REASON (2))
                       MOVE PA52F3-PCT-REASON-FN (2)   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F3-PCT-ANT-END-DATE NOT = PCT-ANT-END-DATE)
                       MOVE PA52F3-PCT-ANT-END-DATE-FN TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F3-PCT-UPDATE-BENEFIT
                                               NOT = PCT-UPDATE-BENEFIT)
                       MOVE PA52F3-PCT-UPDATE-BENEFIT-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F3-PCT-UPD-ABS-MGMT
                                               NOT = PCT-UPD-ABS-MGMT)
                       MOVE PA52F3-PCT-UPD-ABS-MGMT-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F3-PCT-HIST-CORR-FLAG
                                               NOT = PCT-HIST-CORR-FLAG)
                       MOVE PA52F3-PCT-HIST-CORR-FLAG-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F3-PCT-PROCESS-LEVEL
                                               NOT = PCT-PROCESS-LEVEL)
                       MOVE PA52F3-PCT-PROCESS-LEVEL-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F3-PCT-DEPARTMENT  NOT = PCT-DEPARTMENT)
                       MOVE PA52F3-PCT-DEPARTMENT-FN   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F3-PCT-USER-LEVEL  NOT = PCT-USER-LEVEL)
                       MOVE PA52F3-PCT-USER-LEVEL-FN   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F3-PCT-LOCAT-CODE  NOT = PCT-LOCAT-CODE)
                       MOVE PA52F3-PCT-LOCAT-CODE-FN   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F3-PCT-SUPERVISOR  NOT = PCT-SUPERVISOR)
                       MOVE PA52F3-PCT-SUPERVISOR-FN   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F3-PCT-PERS-GROUP  NOT = PCT-PERS-GROUP)
                       MOVE PA52F3-PCT-PERS-GROUP-FN   TO CRT-FIELD-NBR
                   END-IF
                   MOVE 124                            TO CRT-ERROR-NBR
                   GO TO 220-END.

           IF  (PA52F3-FC = "C")
           AND (PA52F3-PCT-NEW-EFFECT-DATE   NOT = ZEROES)
               MOVE 1 TO I2
               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > 12)
                       IF (PA52F3-PCT-NEW-VALUE-1 (I1) NOT =
                                                     PCT-NEW-VALUE (I2))
                           MOVE 124                    TO CRT-ERROR-NBR
                           MOVE PA52F3-PCT-NEW-VALUE-1-FN (I1)
                                                       TO CRT-FIELD-NBR
                           GO TO 220-END
                       END-IF
                       ADD 1 TO I2
               END-PERFORM

               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > 12)
                       IF (PA52F3-PCT-NEW-VALUE-2 (I1) NOT =
                                                     PCT-NEW-VALUE (I2))
                           MOVE 124                    TO CRT-ERROR-NBR
                           MOVE PA52F3-PCT-NEW-VALUE-2-FN (I1)
                                                       TO CRT-FIELD-NBR
                           GO TO 220-END
                       END-IF
                       ADD 1 TO I2
               END-PERFORM

               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > 12)
                       IF (PA52F3-PCT-NEW-VALUE-3 (I1) NOT =
                                                     PCT-NEW-VALUE (I2))
                           MOVE 124                    TO CRT-ERROR-NBR
                           MOVE PA52F3-PCT-NEW-VALUE-3-FN (I1)
                                                       TO CRT-FIELD-NBR
                           GO TO 220-END
                       END-IF
                       ADD 1 TO I2
               END-PERFORM
           END-IF.

287600     INITIALIZE HREMP-SCR-FIELDS.
287700     INITIALIZE HRPEM-SCR-FIELDS.
287800     MOVE "A"                    TO HREMP-FC.
287900     MOVE PA52F3-PCT-COMPANY     TO HREMP-COMPANY.
288000     MOVE PA52F3-PCT-COMPANY-FN  TO HREMP-COMPANY-FN.
288100     MOVE PA52F3-PCT-UPDATE-BENEFIT
288200                                 TO HREMP-UPDATE-BENEFIT.
288100     MOVE PA52F3-PCT-UPD-ABS-MGMT
288200                                 TO HREMP-UPDATE-ABSENCE-MGMT.
288300     MOVE "Y"                    TO HREMP-ACTION.
288400     MOVE "Y"                    TO HREMP-EDIT-VALUE-LIST.
288500     MOVE "Y"                    TO HREMP-EDIT-DATA-ONLY.
288600
           IF  (PA52F3-PAT-FLD-NBR-1 (1) = ZEROES)
               SET PA52WS-NO-FIELDS-EXIST TO TRUE
               GO TO 220-END.

           INITIALIZE PA52WS-CURRENCY-CHANGE-SW
                      PA52WS-PL-CHANGE-SW
                      PA52WS-PAY-RATE-CHANGE-SW
                      PA52WS-NEW-PL
                      PA52WS-NEW-CURRENCY-A
                      PA52WS-NEW-CURRENCY-FN
                      PA52WS-STEP-BLANKED-SW.

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 12)
               OR     (PA52WS-STEP-BLANKED)

               IF   (PA52F3-PAT-FLD-NBR-1 (I1)   = HREMP-PAY-STEP-DN)
               AND ((PA52F3-PCT-NEW-VALUE-1 (I1) = "*BLANK")
               OR   (PA52F3-PCT-NEW-VALUE-1 (I1) = "*blank"))
                   MOVE WS-TRUE TO PA52WS-STEP-BLANKED-SW.

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 12)
               OR     (PA52WS-STEP-BLANKED)

               IF   (PA52F3-PAT-FLD-NBR-2 (I1)   = HREMP-PAY-STEP-DN)
               AND ((PA52F3-PCT-NEW-VALUE-2 (I1) = "*BLANK")
               OR   (PA52F3-PCT-NEW-VALUE-2 (I1) = "*blank"))
                   MOVE WS-TRUE TO PA52WS-STEP-BLANKED-SW.

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 12)
               OR     (PA52WS-STEP-BLANKED)

               IF   (PA52F3-PAT-FLD-NBR-3 (I1)   = HREMP-PAY-STEP-DN)
               AND ((PA52F3-PCT-NEW-VALUE-3 (I1) = "*BLANK")
               OR   (PA52F3-PCT-NEW-VALUE-3 (I1) = "*blank"))
                   MOVE WS-TRUE TO PA52WS-STEP-BLANKED-SW.

P48123    
P48123     MOVE PRS-CURRENCY-CODE              TO PAPCT-EMP-CURRENCY.
P48123     MOVE PRS-CURR-ND                    TO PAPCT-EMP-CURR-ND. 
292000
292100     PERFORM 224-EDIT-NEW-VALUES-1
292200     THRU    224-END1
292300         VARYING I1 FROM 1 BY 1
292400         UNTIL  (I1 > 12)
292500         OR     (ERROR-FOUND).
292600
292700     IF (ERROR-FOUND)
292800         GO TO 220-END.
292900
293000     PERFORM 224-EDIT-NEW-VALUES-2
293100     THRU    224-END2
293200         VARYING I1 FROM 1 BY 1
293300         UNTIL  (I1 > 12)
293400         OR     (ERROR-FOUND).
293500
293600     IF (ERROR-FOUND)
293700         GO TO 220-END.
293800
293900     PERFORM 224-EDIT-NEW-VALUES-3
294000     THRU    224-END3
294100         VARYING I1 FROM 1 BY 1
294200         UNTIL  (I1 > 12)
294300         OR     (ERROR-FOUND).
294400
294500     IF (ERROR-FOUND)
294600         GO TO 220-END.
294700
           IF (PA52WS-NOTHING-ENTERED)
               MOVE 145                                TO CRT-ERROR-NBR
               MOVE PA52F3-PCT-NEW-VALUE-1-FN (1)      TO CRT-FIELD-NBR
               GO TO 220-END.

           IF  (PA52WS-CURRENCY-CHANGED)
           AND (PA52WS-PL-CHANGED)
           AND (PA52WS-NEW-CURRENCY-A NOT = "*BLANK")
               MOVE PA52F3-PCT-COMPANY     TO DB-COMPANY
               IF  (PA52WS-NEW-PL = "*BLANK")
                   INITIALIZE                 DB-PROCESS-LEVEL
               ELSE
                   MOVE PA52WS-NEW-PL      TO DB-PROCESS-LEVEL
               END-IF
               PERFORM 840-FIND-PRSSET1
               IF  (PRS-CURRENCY-CODE NOT = PA52WS-NEW-CURRENCY-A)
                   IF  (PA52WS-NEW-PL = "*BLANK")
                       MOVE 163                        TO CRT-ERROR-NBR
                   ELSE
                       MOVE 164                        TO CRT-ERROR-NBR
                   END-IF
               MOVE PA52WS-NEW-CURRENCY-FN             TO CRT-FIELD-NBR
               GO TO 220-END.

           IF  (PA52WS-CURRENCY-CHANGED)
           AND (PA52WS-PAY-RATE-NOT-CHANGED)
           AND (PA52F3-XMIT-CURR = ZEROES)
               MOVE 1                  TO PA52F3-XMIT-CURR
               MOVE 162                                TO CRT-ERROR-NBR
               MOVE PA52WS-NEW-CURRENCY-FN             TO CRT-FIELD-NBR
               GO TO 220-END.

           MOVE WS-FALSE               TO HREMP-PAUSER-FLAG-SW.
           MOVE PA52F3-PCT-EFFECT-DATE TO HREMP-EFFECT-DATE.
294900     PERFORM 2000-HREMP-EDIT-TRAN.
295000     IF (ERROR-FOUND)
295100         GO TO 220-END.
295200
295300     IF (HREMP-UPDPEP-DATE = PA52F3-PCT-EFFECT-DATE)
295400         INITIALIZE                 HREMP-UPDPEP-DATE.
295500
295600     PERFORM
295700       VARYING I9 FROM 1 BY 1
295800       UNTIL  (I9 > 12)
295900       OR     (PA52F3-PAT-ITEM-NAME-1 (I9) = SPACES)
296000        IF (PA52F3-PAT-FLD-NBR-1 (I9) = HREMP-ACCT-CATEGORY-DN)
              AND (PA52F3-PCT-NEW-VALUE-1 (I9) NOT = "*BLANK")
296100           MOVE HREMP-ACCT-CATEGORY TO PA52F3-PCT-NEW-VALUE-1 (I9)
296200        END-IF
296300     END-PERFORM.
296400
296500     PERFORM
296600       VARYING I9 FROM 1 BY 1
296700       UNTIL  (I9 > 12)
296800       OR     (PA52F3-PAT-ITEM-NAME-2 (I9) = SPACES)
296900        IF (PA52F3-PAT-FLD-NBR-2 (I9) = HREMP-ACCT-CATEGORY-DN)
              AND (PA52F3-PCT-NEW-VALUE-2 (I9) NOT = "*BLANK")
297000           MOVE HREMP-ACCT-CATEGORY TO PA52F3-PCT-NEW-VALUE-2 (I9)
297100        END-IF
297200     END-PERFORM.
297300
297400     PERFORM
297500       VARYING I9 FROM 1 BY 1
297600       UNTIL  (I9 > 12)
297700       OR     (PA52F3-PAT-ITEM-NAME-3 (I9) = SPACES)
297800        IF (PA52F3-PAT-FLD-NBR-3 (I9) = HREMP-ACCT-CATEGORY-DN)
              AND (PA52F3-PCT-NEW-VALUE-3 (I9) NOT = "*BLANK")
297900           MOVE HREMP-ACCT-CATEGORY TO PA52F3-PCT-NEW-VALUE-3 (I9)
298000        END-IF
298100     END-PERFORM.
298200
298300     MOVE ZEROS                         TO IFAUWS-ACCT-UNIT-FLAG.
298400
           INITIALIZE PA52WS-DIST-CO-SW.
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL (I1 > 12)
               OR    (ERROR-FOUND)
               OR    (PA52WS-DIST-CO)
                   IF  (PA52F3-PAT-FLD-NBR-1 (I1) =
                                            HREMP-HM-DIST-CO-DN)
P95789*            AND (HREMP-HM-DIST-CO            NOT = SPACES)
P95789             AND (HREMP-HM-DIST-CO            NOT = ZEROES)
                   AND (PA52F3-PCT-NEW-VALUE-1 (I1) NOT = SPACES
                                                  AND "*BLANK")
                       MOVE HREMP-HM-DIST-CO  TO IFAUWS-COMPANY
                       MOVE WS-TRUE           TO PA52WS-DIST-CO-SW
                   END-IF
           END-PERFORM.

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL (I1 > 12)
               OR    (ERROR-FOUND)
               OR    (PA52WS-DIST-CO)
                   IF  (PA52F3-PAT-FLD-NBR-2 (I1) =
                                            HREMP-HM-DIST-CO-DN)
P95789*            AND (HREMP-HM-DIST-CO            NOT = SPACES)
P95789             AND (HREMP-HM-DIST-CO            NOT = ZEROES)
                   AND (PA52F3-PCT-NEW-VALUE-2 (I1) NOT = SPACES
                                                  AND "*BLANK")
                       MOVE HREMP-HM-DIST-CO  TO IFAUWS-COMPANY
                       MOVE WS-TRUE           TO PA52WS-DIST-CO-SW
                   END-IF
           END-PERFORM.

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL (I1 > 12)
               OR    (ERROR-FOUND)
               OR    (PA52WS-DIST-CO)
                   IF  (PA52F3-PAT-FLD-NBR-3 (I1) =
                                            HREMP-HM-DIST-CO-DN)
P95789*            AND (HREMP-HM-DIST-CO            NOT = SPACES)
P95789             AND (HREMP-HM-DIST-CO            NOT = ZEROES)
                   AND (PA52F3-PCT-NEW-VALUE-3 (I1) NOT = SPACES
                                                  AND "*BLANK")
                       MOVE HREMP-HM-DIST-CO  TO IFAUWS-COMPANY
                       MOVE WS-TRUE           TO PA52WS-DIST-CO-SW
                   END-IF
           END-PERFORM.

298500     PERFORM
298600         VARYING I1 FROM 1 BY 1
298700         UNTIL (I1 > 12)
298800         OR    (ERROR-FOUND)
               OR    (IFAUWS-ACCT-UNIT-FLAG = 2)
298900             IF  (PA52F3-PAT-FLD-NBR-1 (I1) =
299000                                      HREMP-HM-ACCT-UNIT-DN)
299100             AND (HREMP-HM-ACCOUNT = ZEROS)
299200             AND (PA52F3-PCT-NEW-VALUE-1 (I1) NOT = SPACES
299300                                            AND "*BLANK")
299400                 MOVE PA52F3-PCT-NEW-VALUE-1 (I1)
299500                                        TO IFAUWS-ACCT-UNIT
299600                 MOVE 2                 TO IFAUWS-ACCT-UNIT-FLAG
299700             END-IF
299800     END-PERFORM.
299900
300000     PERFORM
300100         VARYING I1 FROM 1 BY 1
300200         UNTIL (I1 > 12)
300300         OR    (ERROR-FOUND)
               OR    (IFAUWS-ACCT-UNIT-FLAG = 2)
300400             IF  (PA52F3-PAT-FLD-NBR-2 (I1) =
300500                                     HREMP-HM-ACCT-UNIT-DN)
300600             AND (HREMP-HM-ACCOUNT = ZEROS)
300700             AND (PA52F3-PCT-NEW-VALUE-2 (I1) NOT = SPACES
300800                                            AND "*BLANK")
300900                 MOVE PA52F3-PCT-NEW-VALUE-2 (I1)
301000                                        TO IFAUWS-ACCT-UNIT
301100                 MOVE 2                 TO IFAUWS-ACCT-UNIT-FLAG
301200             END-IF
301300     END-PERFORM.
301400
301500     PERFORM
301600         VARYING I1 FROM 1 BY 1
301700         UNTIL (I1 > 12)
301800         OR    (ERROR-FOUND)
               OR    (IFAUWS-ACCT-UNIT-FLAG = 2)
301900             IF  (PA52F3-PAT-FLD-NBR-3 (I1) =
302000                                    HREMP-HM-ACCT-UNIT-DN)
302100             AND (HREMP-HM-ACCOUNT = ZEROS)
302200             AND (PA52F3-PCT-NEW-VALUE-3 (I1) NOT = SPACES
302300                                            AND "*BLANK")
302400                 MOVE PA52F3-PCT-NEW-VALUE-3 (I1)
302500                                        TO IFAUWS-ACCT-UNIT
302600                 MOVE 2                 TO IFAUWS-ACCT-UNIT-FLAG
302700             END-IF
302800     END-PERFORM.
302900
303000     IF  (IFAUWS-ACCT-UNIT-FLAG = 2)
           AND (PA52WS-DIST-CO)
303200         PERFORM 645-EDIT-ACCT-UNIT-60.
303300
303400 220-END.
303500
303600******************************************************************
303700 222-EDIT-REASONS.
303800******************************************************************
303900
304000     IF (PA52F3-PCT-REASON (I1) = SPACES)
304100         GO TO 222-END.
304200
           SET PA52WS-NO-ACTION-REASON TO TRUE.

           MOVE PA52F3-PCT-COMPANY     TO DB-COMPANY.
           MOVE PA52F3-PCT-ACTION-CODE TO DB-ACTION-CODE.
049700     MOVE CRESET4-ACTION-CODE    TO WS-DB-BEG-RNG.
           PERFORM 850-KFIND-BEGRNG-CRESET4.
           IF (PAACTREAS-KFOUND)
               SET PA52WS-ACTION-REASON     TO TRUE
           END-IF.

050000     IF  (PA52F3-FC              = "A")
050100     OR ((PA52F3-FC              = "C")
050200     AND (PA52F3-PCT-REASON (I1) NOT = PCT-REASON (I1))
           AND (PA52F3-PCT-REASON (I1) NOT = SPACES))
               MOVE PA52F3-PCT-REASON (I1) TO DB-ACT-REASON-CD
               IF (PA52WS-ACTION-REASON)
                   PERFORM 840-FIND-CRESET4
               ELSE
                   MOVE CRESET3-ACT-REASON-CD    TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-CRESET3
               END-IF
050400         IF (PAACTREAS-NOTFOUND)
                   IF (PA52WS-ACTION-REASON)
                       MOVE 142                TO CRT-ERROR-NBR
                   ELSE
050500                 MOVE 140                        TO CRT-ERROR-NBR
                   END-IF
050600             MOVE PA52F3-PCT-REASON-FN (I1)      TO CRT-FIELD-NBR
050700             GO TO 222-END
050800         ELSE
050900             IF (CRE-ACTIVE-FLAG NOT = "1")
051000                 MOVE 141                        TO CRT-ERROR-NBR
051100                 MOVE PA52F3-PCT-REASON-FN (I1)  TO CRT-FIELD-NBR
051200                 GO TO 222-END.
051300
306000 222-END.
306100
306200******************************************************************
306300 224-EDIT-NEW-VALUES-1.
306400******************************************************************
306500
306600     IF (PA52F3-PCT-NEW-VALUE-1 (I1) = SPACES)
               IF  (PA52F3-PAT-FLD-NBR-1 (I1) = HREMP-PAY-RATE-DN)
               AND (PA52WS-STEP-BLANKED)
                   INITIALIZE HREMP-EDIT-DATA-ONLY
                   NEXT SENTENCE
               END-IF
306700         GO TO 224-END1.
306800
           SET PA52WS-SOMETHING-ENTERED TO TRUE.

306900     MOVE PA52F3-PAT-FLD-NBR-1 (I1)  TO PA52WS-PAT-FLD-NBRS3.
307000     MOVE PA52F3-PCT-NEW-VALUE-1 (I1)
307100                                     TO PA52WS-PCT-NEW-VALUES3.
307200     MOVE PA52F3-PCT-NEW-VALUE-1-FN (I1)
307300                                     TO PA52WS-PCT-NEW-VALUE-FNS3.
307400     PERFORM 224-EDIT-NEW-VALUES
307500     THRU    224-END.
307600
307700     IF (ERROR-FOUND)
307800         MOVE PA52F3-PCT-NEW-VALUE-1-FN (I1)  TO CRT-FIELD-NBR
307900         GO TO 224-END1.
308000
308100     MOVE PA52WS-PCT-NEW-VALUES3 TO PA52F3-PCT-NEW-VALUE-1 (I1).
308200
308300 224-END1.
308400
308500******************************************************************
308600 224-EDIT-NEW-VALUES-2.
308700******************************************************************
308800
308900     IF (PA52F3-PCT-NEW-VALUE-2 (I1) = SPACES)
               IF  (PA52F3-PAT-FLD-NBR-2 (I1) = HREMP-PAY-RATE-DN)
               AND (PA52WS-STEP-BLANKED)
                   INITIALIZE HREMP-EDIT-DATA-ONLY
                   NEXT SENTENCE
               END-IF
309000         GO TO 224-END2.
309100
           SET PA52WS-SOMETHING-ENTERED TO TRUE.

309200     MOVE PA52F3-PAT-FLD-NBR-2 (I1)  TO PA52WS-PAT-FLD-NBRS3.
309300     MOVE PA52F3-PCT-NEW-VALUE-2 (I1)
309400                                     TO PA52WS-PCT-NEW-VALUES3.
309500     MOVE PA52F3-PCT-NEW-VALUE-2-FN (I1)
309600                                     TO PA52WS-PCT-NEW-VALUE-FNS3.
309700     PERFORM 224-EDIT-NEW-VALUES
309800     THRU    224-END.
309900
310000     IF (ERROR-FOUND)
310100         MOVE PA52F3-PCT-NEW-VALUE-2-FN (I1)  TO CRT-FIELD-NBR
310200         GO TO 224-END2.
310300
310400     MOVE PA52WS-PCT-NEW-VALUES3 TO PA52F3-PCT-NEW-VALUE-2 (I1).
310500
310600 224-END2.
310700
310800******************************************************************
310900 224-EDIT-NEW-VALUES-3.
311000******************************************************************
311100
311200     IF (PA52F3-PCT-NEW-VALUE-3 (I1) = SPACES)
               IF  (PA52F3-PAT-FLD-NBR-3 (I1) = HREMP-PAY-RATE-DN)
               AND (PA52WS-STEP-BLANKED)
                   INITIALIZE HREMP-EDIT-DATA-ONLY
                   NEXT SENTENCE
               END-IF
311300         GO TO 224-END3.
311400
           SET PA52WS-SOMETHING-ENTERED TO TRUE.

311500     MOVE PA52F3-PAT-FLD-NBR-3 (I1)  TO PA52WS-PAT-FLD-NBRS3.
311600     MOVE PA52F3-PCT-NEW-VALUE-3 (I1)
311700                                     TO PA52WS-PCT-NEW-VALUES3.
311800     MOVE PA52F3-PCT-NEW-VALUE-3-FN (I1)
311900                                     TO PA52WS-PCT-NEW-VALUE-FNS3.
312000     PERFORM 224-EDIT-NEW-VALUES
312100     THRU    224-END.
312200
312300     IF (ERROR-FOUND)
312400         MOVE PA52F3-PCT-NEW-VALUE-3-FN (I1)  TO CRT-FIELD-NBR
312500         GO TO 224-END3.
312600
312700     MOVE PA52WS-PCT-NEW-VALUES3 TO PA52F3-PCT-NEW-VALUE-3 (I1).
312800
312900 224-END3.
313000
313100******************************************************************
313200 224-EDIT-NEW-VALUES.
313300******************************************************************
313400
313500     IF  (PA52WS-PCT-NEW-VALUES3  NOT = SPACES)
313600     AND (PA52WS-PAT-FLD-NBRS3    = ZEROS)
313700         MOVE 137                                TO CRT-ERROR-NBR
313800         MOVE PA52WS-PCT-NEW-VALUE-FNS3          TO CRT-FIELD-NBR
313900         GO TO 224-END.
314000
314100     IF (PA52WS-PAT-FLD-NBRS3      >= 2000)
314200         INITIALIZE PAPCT-SCR-FIELDS
314300         MOVE PA52WS-PAT-FLD-NBRS3         TO PAPCT-FLD-NBR
314400         MOVE PA52WS-PCT-NEW-VALUES3       TO PAPCT-NEW-VALUE
314500         MOVE PA52WS-PCT-NEW-VALUE-FNS3    TO PAPCT-FIELD-FN
314600         MOVE PA52F3-PCT-COMPANY           TO PAPCT-COMPANY
314700         MOVE "Y"                          TO PAPCT-EDIT-DATA-ONLY
               SET PAPCT-CURRENCY TO TRUE
               SET PAPCT-REFORMAT TO TRUE
314800         PERFORM 5100-EDIT-USER-FIELDS
314900         MOVE PAPCT-NEW-VALUE        TO PA52WS-PCT-NEW-VALUES3
315000         GO TO 224-END.
315100
315200     MOVE PA52WS-PCT-NEW-VALUES3       TO HRWS-UP-FIELD.
315300     MOVE PA52WS-PAT-FLD-NBRS3         TO DB-FLD-NBR.

           IF  (HRWS-PAD-TYPE  (DB-FLD-NBR) = SPACES)
               PERFORM 840-FIND-PADSET1
               MOVE PAD-DATA-TYPE      TO HRWS-PAD-TYPE (DB-FLD-NBR)
               MOVE PAD-DECIMALS       TO HRWS-PAD-DEC  (DB-FLD-NBR)
               MOVE PAD-CURRENCY-FLAG  TO HRWS-PAD-CURR (DB-FLD-NBR)
               MOVE PAD-SIZE           TO HRWS-PAD-SIZE (DB-FLD-NBR)
               MOVE PAD-CASE-FLAG      TO HRWS-PAD-CASE (DB-FLD-NBR)
           END-IF.

318800     IF  (HRWS-UP-FIELD (1:1) = "*")
315600     OR  ((HRWS-PAD-TYPE (DB-FLD-NBR) = "A")
            AND (HRWS-PAD-CASE (DB-FLD-NBR) = "Y"))
318900         PERFORM 760-HR-UPPER-CASE
319000         MOVE HRWS-UP-FIELD      TO PA52WS-PCT-NEW-VALUES3.
319100
           IF  (PA52WS-PAT-FLD-NBRS3 = HREMP-CURRENCY-CODE-DN)
           AND (PA52WS-PCT-NEW-VALUES3 NOT = SPACES)
               MOVE PA52WS-PCT-NEW-VALUE-FNS3
                                           TO PA52WS-NEW-CURRENCY-FN
               MOVE PA52WS-PCT-NEW-VALUES3 TO PA52WS-NEW-CURRENCY-A
               SET PA52WS-CURRENCY-CHANGED TO TRUE.

           IF  (PA52WS-PAT-FLD-NBRS3 = HREMP-PROCESS-LEVEL-DN)
           AND (PA52WS-PCT-NEW-VALUES3 NOT = SPACES)
               MOVE PA52WS-PCT-NEW-VALUES3 TO PA52WS-NEW-PL
               SET PA52WS-PL-CHANGED TO TRUE.

           IF  (PA52WS-PAT-FLD-NBRS3 = HREMP-PAY-RATE-DN)
           AND (PA52WS-PCT-NEW-VALUES3 NOT = SPACES)
               SET PA52WS-PAY-RATE-CHANGED TO TRUE.

319200     IF (PA52WS-PCT-NEW-VALUES3      = "*BLANK")
               MOVE PA52F3-PCT-COMPANY             TO DB-COMPANY
               MOVE " A"                           TO DB-COUNTRY-CD-REQ
               MOVE HIGH-VALUES                    TO
                                                   DBRNG-COUNTRY-CD-REQ
               MOVE PASSET4-COUNTRY-CD-REQ         TO WS-DB-SUB-RNG
               PERFORM 850-FIND-SUBRNG-PASSET4
               IF (PASCRTY-NOTFOUND)
                   MOVE PA52WS-PAT-FLD-NBRS3       TO DB-FLD-NBR
                   INITIALIZE                         DB-COUNTRY-CD-REQ
                                                      DB-PROCESS-LEVEL
                   PERFORM 840-FIND-PASSET3
                   IF  (PASCRTY-FOUND)
                   AND (PAS-REQ-FLAG = "X")
                       MOVE 259                    TO CRT-ERROR-NBR
                       MOVE PA52WS-PCT-NEW-VALUE-FNS3
                                                   TO CRT-FIELD-NBR
                   END-IF
               END-IF
           ELSE
319300         MOVE PA52WS-PCT-NEW-VALUES3         TO PAPCT-NEW-VALUE
319500         MOVE PA52WS-PCT-NEW-VALUE-FNS3      TO PAPCT-FIELD-FN
319600         MOVE PA52WS-PAT-FLD-NBRS3           TO PAPCT-FLD-NBR
319700         MOVE HRWS-PAD-TYPE (DB-FLD-NBR)     TO PAPCT-TYPE
319800         MOVE HRWS-PAD-SIZE (DB-FLD-NBR)     TO PAPCT-SIZE
319900         MOVE HRWS-PAD-DEC  (DB-FLD-NBR)     TO PAPCT-DECIMALS
               SET PAPCT-NO-CURRENCY TO TRUE
               SET PAPCT-REFORMAT TO TRUE
320000         PERFORM 5400-EDIT-CORRECT-FORMAT
320100         IF (ERROR-FOUND)
320200             GO TO 224-END
320300         ELSE
320400             MOVE PAPCT-NEW-VALUE     TO PA52WS-PCT-NEW-VALUES3.
320500
320600     INITIALIZE PAPCT-SCR-FIELDS.
320700     MOVE PA52F3-PCT-COMPANY           TO PAPCT-COMPANY.
320800     MOVE PA52WS-PAT-FLD-NBRS3         TO PAPCT-FLD-NBR.
320900     MOVE PA52WS-PCT-NEW-VALUES3       TO PAPCT-NEW-VALUE.
321000     MOVE PA52WS-PCT-NEW-VALUE-FNS3    TO PAPCT-FIELD-FN.
321100     MOVE "Y"                          TO PAPCT-EDIT-DATA-ONLY.
321200     PERFORM 5000-PAPCT-MOVE-TO-HREMP.
321300     IF  (HREMP-HM-ACCOUNT = ZEROS)
321400     AND ((HREMP-HM-ACCT-UNIT NOT = SPACES)
321500     OR   (HREMP-HM-SUB-ACCT  NOT = ZEROS))
321600         MOVE SPACES TO HREMP-HM-ACCT-UNIT
321700         MOVE ZEROS TO HREMP-HM-SUB-ACCT.
321800
321900 224-END.
322000
322100******************************************************************
322200 400-PROCESS-TRAN.
322300******************************************************************
322400
322500     IF (PA52F3-FC = "A")
322600         PERFORM 410-ADD
322700         THRU    410-END
322800     ELSE
322900     IF (PA52F3-FC = "C")
323000         PERFORM 420-CHANGE
323100         THRU    420-END
323200     ELSE
323300     IF (PA52F3-FC = "D")
323400         PERFORM 440-DELETE
323500         THRU    440-END
323600     ELSE
323700     IF (PA52F3-FC = "I" OR "N" OR "P")
323800         PERFORM 480-INQUIRE
323900         THRU    480-END.
324000
324100 400-END.
324200
324300******************************************************************
324400 410-ADD.
324500******************************************************************
324600
324700     PERFORM 910-AUDIT-BEGIN.
324800     IF (DMS-ABORTED)
324900         GO TO 410-END.
325000
325100     MOVE PA52F3-PCT-COMPANY     TO PAACT-COMPANY.
325200     MOVE "M"                    TO PAACT-ACTION-TYPE.
325300     INITIALIZE                     PAACT-EMPLOYEE.
325400     MOVE PA52F3-PCT-ACTION-CODE TO PAACT-ACTION-CODE.
325500     MOVE PA52F3-PCT-EFFECT-DATE TO PAACT-EFFECT-DATE.
325600     MOVE "N"                    TO PAACT-HISTORY.
325700     PERFORM 2300-PAACT-ACTION-NBR.
325800     PERFORM 800-CREATE-PERSACTION.
325900     MOVE PAACT-ACTION-NBR       TO PCT-ACTION-NBR
326000                                    PA52F3-PCT-ACTION-NBR.
326100
326200     PERFORM 500-MOVE-DATA
326300     THRU    500-END.
326400
326500     PERFORM 8200-STORE-PERSACTION.
326600     PERFORM 920-AUDIT-END.
326700
326800     PERFORM 481-MOVE-TO-SCREEN
326900     THRU    481-END.
327000
327100     MOVE CRT-ADD-COMPLETE       TO CRT-MESSAGE.
327200
P40376     MOVE SPACES                     TO PA52F3-PGM-NAME.
P40376     MOVE SPACES                     TO CRT-PASS-FC.
P40376     MOVE "A"                        TO CRT-DISPLAY-FC.
P40376     MOVE "M"                        TO PA52F3-PT-ACTION-TYPE.
P40376     MOVE PA52WS-ACTION-NBR          TO PA52F3-PCT-ACTION-NBR.
P40376
327300 410-END.
327400
327500******************************************************************
327600 420-CHANGE.
327700******************************************************************
327800
327900     PERFORM 910-AUDIT-BEGIN.
328000     IF (DMS-ABORTED)
328100         GO TO 420-END.
328200
           IF (PA52F3-PCT-NEW-EFFECT-DATE NOT = ZEROES)
               MOVE PA52F3-PCT-COMPANY         TO PAACT-COMPANY
               MOVE "M"                        TO PAACT-ACTION-TYPE
               INITIALIZE                         PAACT-EMPLOYEE
               MOVE PA52F3-PCT-ACTION-CODE     TO PAACT-ACTION-CODE
               MOVE PA52F3-PCT-NEW-EFFECT-DATE TO PAACT-EFFECT-DATE
               MOVE "N"                        TO PAACT-HISTORY
               PERFORM 2300-PAACT-ACTION-NBR

328300         MOVE PA52F3-PCT-COMPANY         TO DB-COMPANY
328400         MOVE "M"                        TO DB-ACTION-TYPE
328500         MOVE PA52F3-PCT-ACTION-CODE     TO DB-ACTION-CODE
328600         MOVE PA52F3-PCT-EFFECT-DATE     TO DB-EFFECT-DATE
328700         MOVE ZEROS                      TO DB-EMPLOYEE
328800         MOVE PA52F3-PCT-ACTION-NBR      TO DB-ACTION-NBR
               PERFORM 840-MODIFY-PCTSET1
               PERFORM 830-DELETE-PERSACTION
               PERFORM 810-RECREATE-PERSACTION
               MOVE PA52F3-PCT-NEW-EFFECT-DATE TO PCT-EFFECT-DATE
               MOVE PAACT-ACTION-NBR           TO PCT-ACTION-NBR
               PERFORM 820-STORE-PERSACTION

               MOVE PA52F3-PCT-COMPANY         TO DB-COMPANY
               INITIALIZE                         DB-EMP-APP
               MOVE "PA"                       TO DB-CMT-TYPE
               MOVE PA52F3-PCT-ACTION-CODE     TO DB-ACTION-CODE
               MOVE PA52F3-PCT-EFFECT-DATE     TO DB-DATE
               INITIALIZE                         DB-EMPLOYEE
                                                  DB-JOB-CODE
               MOVE PA52F3-PCT-ACTION-NBR      TO DB-LN-NBR
               MOVE PACSET2-LN-NBR             TO WS-DB-BEG-RNG
               PERFORM 850-MODIFY-BEGRNG-PACSET2
               PERFORM
                   UNTIL (PACOMMENTS-NOTFOUND)
                   PERFORM 830-DELETE-PACOMMENTS
                   PERFORM 810-RECREATE-PACOMMENTS
                   MOVE PA52F3-PCT-NEW-EFFECT-DATE TO PAC-DATE
                   MOVE PAACT-ACTION-NBR           TO PAC-LN-NBR
                   PERFORM 820-STORE-PACOMMENTS
                   PERFORM 860-MODIFY-NXTRNG-PACSET2
               END-PERFORM
               MOVE PA52F3-PCT-NEW-EFFECT-DATE TO PA52F3-PCT-EFFECT-DATE
               MOVE PAACT-ACTION-NBR           TO PA52F3-PCT-ACTION-NBR
               INITIALIZE PA52F3-PCT-NEW-EFFECT-DATE
           ELSE
328300         MOVE PA52F3-PCT-COMPANY     TO DB-COMPANY
328400         MOVE "M"                    TO DB-ACTION-TYPE
328500         MOVE PA52F3-PCT-ACTION-CODE TO DB-ACTION-CODE
328600         MOVE PA52F3-PCT-EFFECT-DATE TO DB-EFFECT-DATE
328700         MOVE ZEROS                  TO DB-EMPLOYEE
328800         MOVE PA52F3-PCT-ACTION-NBR  TO DB-ACTION-NBR
224000         PERFORM 840-MODIFY-PCTSET1
               PERFORM 8400-AFTER-FIND-PCT
224100         PERFORM 500-MOVE-DATA
224200         THRU    500-END
224300         PERFORM 8200-STORE-PERSACTION
           END-IF.
224400
224500     PERFORM 920-AUDIT-END.
329800
329900     PERFORM 481-MOVE-TO-SCREEN
330000     THRU    481-END.
330100     MOVE CRT-CHG-COMPLETE       TO CRT-MESSAGE.
330200
330300 420-END.
330400
330500******************************************************************
330600 440-DELETE.
330700******************************************************************
330800
330900     PERFORM 910-AUDIT-BEGIN.
331000     IF (DMS-ABORTED)
331100         GO TO 440-END.
331200
331300     MOVE PA52F3-PCT-COMPANY     TO DB-COMPANY.
331400     MOVE "M"                    TO DB-ACTION-TYPE.
331500     MOVE PA52F3-PCT-ACTION-CODE TO DB-ACTION-CODE.
331600     MOVE PA52F3-PCT-EFFECT-DATE TO DB-EFFECT-DATE.
331700     MOVE ZEROS                  TO DB-EMPLOYEE.
331800     MOVE PA52F3-PCT-ACTION-NBR  TO DB-ACTION-NBR.
331900
332000     PERFORM 840-MODIFY-PCTSET1.
332100     PERFORM 830-DELETE-PERSACTION.
332200
332300     MOVE ZEROES                 TO DB-EMP-APP.
332400     MOVE "PA"                   TO DB-CMT-TYPE.
332500     MOVE PA52F3-PCT-ACTION-CODE TO DB-ACTION-CODE.
332600     MOVE PA52F3-PCT-EFFECT-DATE TO DB-DATE.
332700     MOVE ZEROS                  TO DB-EMPLOYEE.
332800     MOVE SPACES                 TO DB-JOB-CODE.
332900     MOVE PA52F3-PCT-ACTION-NBR  TO DB-LN-NBR.
333000     MOVE PACSET2-LN-NBR         TO WS-DB-BEG-RNG.
333100     PERFORM 830-DELETERNG-PACSET2.
333200
333300     MOVE CRT-RECS-DELETED       TO CRT-MESSAGE.
333400     PERFORM 920-AUDIT-END.
333500
333600 440-END.
333700
333800******************************************************************
333900 480-INQUIRE.
334000******************************************************************
334100
334200     INITIALIZE PA52F3-DETAIL-GROUP-1.
334300     INITIALIZE PA52F3-DETAIL-GROUP-2.
334400     INITIALIZE PA52F3-DETAIL-GROUP-3.
           INITIALIZE PA52F3-DETAIL-GROUP-1A.
           INITIALIZE PA52F3-DETAIL-GROUP-2A.
           INITIALIZE PA52F3-DETAIL-GROUP-3A.
           INITIALIZE PA52F3-MORE-MSG.
           INITIALIZE PA52F3-PCT-REASON (1).
           INITIALIZE PA52F3-PCT-REASON (2).
334500
334600     MOVE PA52WS-PRS-NAME        TO PA52F3-PRS-NAME.
335000
335100     PERFORM
335200         VARYING I1 FROM 1 BY 1
335300         UNTIL  (I1 > 12)
335400
               INITIALIZE CRT-PHRASE-XLT
               IF (PAT-FLD-NBR (I1)    NOT = ZEROES)
                   MOVE PAT-FLD-NBR (I1)   TO DB-FLD-NBR
                   PERFORM 840-FIND-PADSET1
                   MOVE PAD-ITEM-NAME      TO CRT-PHRASE
                   MOVE PA52WS-PHRASE-SIZE TO CRT-PHRASE-SIZE
                   MOVE "N"                TO CRT-PUT-DOTS
                   PERFORM 900-GET-PHRASE-XLT
               END-IF
335500         MOVE CRT-PHRASE-XLT     TO PA52F3-PAT-ITEM-NAME-1 (I1)
337400         MOVE PAT-FLD-NBR (I1)      TO PA52F3-PAT-FLD-NBR-1 (I1)
106300                                       PA52F3-PAT-FLD-NBR-1A (I1)
               MOVE "V"                   TO PA52F3-SELECT-BTN-1 (I1)
337500         ADD 1                      TO I8
337600     END-PERFORM.
337700
337800     PERFORM
337900         VARYING I1 FROM 13 BY 1
338000         UNTIL  (I1 > 24)
338100
               INITIALIZE CRT-PHRASE-XLT
               IF (PAT-FLD-NBR (I1)    NOT = ZEROES)
                   MOVE PAT-FLD-NBR (I1)   TO DB-FLD-NBR
                   PERFORM 840-FIND-PADSET1
                   MOVE PAD-ITEM-NAME      TO CRT-PHRASE
                   MOVE PA52WS-PHRASE-SIZE TO CRT-PHRASE-SIZE
                   MOVE "N"                TO CRT-PUT-DOTS
                   PERFORM 900-GET-PHRASE-XLT
               END-IF
338200         COMPUTE I8 = (I1 - 12)
335500         MOVE CRT-PHRASE-XLT     TO PA52F3-PAT-ITEM-NAME-2 (I8)
340200         MOVE PAT-FLD-NBR (I1)      TO PA52F3-PAT-FLD-NBR-2 (I8)
106300                                       PA52F3-PAT-FLD-NBR-2A (I8)
               MOVE "V"                   TO PA52F3-SELECT-BTN-2 (I8)
340300     END-PERFORM.
340400
340500     PERFORM
340600         VARYING I1 FROM 25 BY 1
340700         UNTIL  (I1 > 36)
340800
               INITIALIZE CRT-PHRASE-XLT
               IF (PAT-FLD-NBR (I1)    NOT = ZEROES)
                   MOVE PAT-FLD-NBR (I1)   TO DB-FLD-NBR
                   PERFORM 840-FIND-PADSET1
                   MOVE PAD-ITEM-NAME      TO CRT-PHRASE
                   MOVE PA52WS-PHRASE-SIZE TO CRT-PHRASE-SIZE
                   MOVE "N"                TO CRT-PUT-DOTS
                   PERFORM 900-GET-PHRASE-XLT
               END-IF
340900         COMPUTE I8 = (I1 - 24)
335500         MOVE CRT-PHRASE-XLT     TO PA52F3-PAT-ITEM-NAME-3 (I8)
342900         MOVE PAT-FLD-NBR (I1)      TO PA52F3-PAT-FLD-NBR-3 (I8)
343000     END-PERFORM.
343100
           IF (PERSACTION-NOTFOUND)
           OR (PCT-COMPANY     NOT = PA52F3-PCT-COMPANY)
           OR (PCT-ACTION-TYPE NOT = "M")
           OR (PCT-EMPLOYEE    NOT = ZEROES)
           OR (PCT-ACTION-CODE NOT = PA52F3-PCT-ACTION-CODE)
           OR (PCT-EFFECT-DATE NOT = PA52F3-PCT-EFFECT-DATE)
               INITIALIZE                         PA52F3-PCT-ACTION-NBR
               IF (PAT-DFT-UPD-BN = SPACES)
                   MOVE PA52F3-PCT-COMPANY      TO DB-COMPANY
                   PERFORM 840-KFIND-BNCSET1
                   IF (BNCOMPANY-KFOUND)
                      MOVE "Y" TO PA52F3-PCT-UPDATE-BENEFIT
                   ELSE
                      MOVE "N" TO PA52F3-PCT-UPDATE-BENEFIT
                   END-IF
               ELSE
                   MOVE PAT-DFT-UPD-BN TO PA52F3-PCT-UPDATE-BENEFIT
               END-IF
               IF (PAT-DFT-UPD-LP = SPACES)
J13588*            MOVE PA52F3-PCT-COMPANY     TO EDCDWS-COMPANY
J13588*            MOVE "LP"                   TO EDCDWS-SYSTEM
J13588*            PERFORM 6000-IS-SYSTEM-TRIGGER-ENABLED
J13588*            IF  (EDCDWS-TRIGGER-ENABLED)
                       MOVE "Y" TO PA52F3-PCT-UPD-ABS-MGMT
J13588*            ELSE
J13588*                MOVE "N"    TO PA52F3-PCT-UPD-ABS-MGMT
J13588*            END-IF
               ELSE
                   MOVE PAT-DFT-UPD-LP TO PA52F3-PCT-UPD-ABS-MGMT
               END-IF
               IF  (PA52F3-PCT-REASON (1) = SPACES)
               AND (PA52F3-PCT-REASON (2) = SPACES)
                   IF (PAT-DFT-REASON (1) NOT = SPACES)
                       MOVE PAT-DFT-REASON (1) TO PA52F3-PCT-REASON (1)
                   END-IF
                   IF (PAT-DFT-REASON (2) NOT = SPACES)
                       MOVE PAT-DFT-REASON (2) TO PA52F3-PCT-REASON (2)
                   END-IF
               END-IF
               MOVE PAT-HIST-CORR-FLAG  TO PA52F3-PCT-HIST-CORR-FLAG
343600         MOVE 407                        TO CRT-MSG-NBR
343700         GO TO 480-END.
343800
343900     PERFORM 481-MOVE-TO-SCREEN
344000     THRU    481-END.
344100
344200     MOVE CRT-INQ-COMPLETE       TO CRT-MESSAGE.
344300
344400 480-END.
344500
344600******************************************************************
344700 481-MOVE-TO-SCREEN.
344800******************************************************************
344900
           INITIALIZE                     PA52WS-FIRST-ACT-NBR
                                          PA52WS-LAST-ACT-NBR.
           INITIALIZE                     PA52F3-PCT-NEW-EFFECT-DATE.
113200     MOVE PCT-COMPANY            TO PA52F3-PCT-COMPANY.
113400     MOVE PCT-ACTION-CODE        TO PA52F3-PCT-ACTION-CODE.
113500     MOVE PCT-ACTION-NBR         TO PA52F3-PCT-ACTION-NBR.
113700     MOVE PCT-EFFECT-DATE        TO PA52F3-PCT-EFFECT-DATE.
           MOVE PCT-COMPANY            TO DB-COMPANY.
           MOVE "M"                    TO DB-ACTION-TYPE.
           INITIALIZE                     DB-EMPLOYEE.
           MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE.
           MOVE PCT-EFFECT-DATE        TO DB-EFFECT-DATE.
           MOVE WS-HIGH-VALUES         TO DB-ACTION-NBR.
           PERFORM 850-FIND-NLT-PCTSET2.
           MOVE PCT-ACTION-NBR         TO PA52WS-LAST-ACT-NBR.
           PERFORM
               UNTIL (PERSACTION-NOTFOUND)
               OR    (PCT-COMPANY     NOT = DB-COMPANY)
               OR    (PCT-ACTION-TYPE NOT = DB-ACTION-TYPE)
               OR    (PCT-ACTION-CODE NOT = DB-ACTION-CODE)
               OR    (PCT-EFFECT-DATE NOT = DB-EFFECT-DATE)
                   MOVE PCT-ACTION-NBR TO PA52WS-FIRST-ACT-NBR
                   PERFORM 860-FIND-NEXT-PCTSET2
           END-PERFORM.

           MOVE PA52F3-PCT-COMPANY     TO DB-COMPANY.
           MOVE "M"                    TO DB-ACTION-TYPE.
           INITIALIZE                     DB-EMPLOYEE.
           MOVE PA52F3-PCT-ACTION-CODE TO DB-ACTION-CODE.
           MOVE PA52F3-PCT-EFFECT-DATE TO DB-EFFECT-DATE.
           MOVE PA52F3-PCT-ACTION-NBR  TO DB-ACTION-NBR.
           PERFORM 840-FIND-PCTSET2.
           PERFORM 8400-AFTER-FIND-PCT.

345200     MOVE PAT-DESCRIPTION        TO PA52F3-PAT-DESCRIPTION.
345600     MOVE PCT-ANT-END-DATE       TO PA52F3-PCT-ANT-END-DATE.
           IF (PA52WS-FIRST-ACT-NBR NOT = PA52WS-LAST-ACT-NBR)
               MOVE 170                TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE        TO PA52F3-MORE-MSG.

345900     MOVE PCT-PROCESS-LEVEL      TO PA52F3-PCT-PROCESS-LEVEL.
346000     MOVE PCT-DEPARTMENT         TO PA52F3-PCT-DEPARTMENT.
346100     MOVE PCT-USER-LEVEL         TO PA52F3-PCT-USER-LEVEL.
346200     MOVE PCT-SUPERVISOR         TO PA52F3-PCT-SUPERVISOR.
346300     MOVE PCT-LOCAT-CODE         TO PA52F3-PCT-LOCAT-CODE.
346400     MOVE PCT-PERS-GROUP         TO PA52F3-PCT-PERS-GROUP.
346500     MOVE PCT-UPDATE-BENEFIT     TO PA52F3-PCT-UPDATE-BENEFIT.
           MOVE PCT-UPD-ABS-MGMT       TO PA52F3-PCT-UPD-ABS-MGMT.
           MOVE PCT-HIST-CORR-FLAG     TO PA52F3-PCT-HIST-CORR-FLAG.
113900     MOVE PCT-REASON (1)         TO PA52F3-PCT-REASON (1).
114000     MOVE PCT-REASON (2)         TO PA52F3-PCT-REASON (2).
346600
346700     PERFORM
346800         VARYING I1 FROM 1 BY 1
346900         UNTIL  (I1 > 12)
347000
347100         MOVE PCT-NEW-VALUE (I1) TO PA52F3-PCT-NEW-VALUE-1 (I1)
347200     END-PERFORM.
347300
347400     PERFORM
347500         VARYING I1 FROM 13 BY 1
347600         UNTIL  (I1 > 24)
347700
347800         COMPUTE I2 = (I1 - 12)
347900         MOVE PCT-NEW-VALUE (I1) TO PA52F3-PCT-NEW-VALUE-2 (I2)
348000     END-PERFORM.
348100
348200     PERFORM
348300         VARYING I1 FROM 25 BY 1
348400         UNTIL  (I1 > 36)
348500
348600         COMPUTE I2 = (I1 - 24)
348700         MOVE PCT-NEW-VALUE (I1) TO PA52F3-PCT-NEW-VALUE-3 (I2)
348800     END-PERFORM.
348900
349000     MOVE PCT-COMPANY            TO DB-COMPANY.
349100     MOVE ZEROES                 TO DB-EMP-APP.
349200     MOVE "PA"                   TO DB-CMT-TYPE.
349300     MOVE ZEROS                  TO DB-EMPLOYEE.
349400     MOVE SPACES                 TO DB-JOB-CODE.
349500     MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE.
349600     MOVE PCT-ACTION-NBR         TO DB-LN-NBR.
349700     MOVE PACSET1-LN-NBR         TO WS-DB-BEG-RNG.
349800     PERFORM 850-FIND-BEGRNG-PACSET1.
349900
350700     MOVE SPACES                 TO PA52F3-PRS1-NAME.
350800     IF (PCT-PROCESS-LEVEL NOT = SPACES)
350900         MOVE PCT-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
351000         PERFORM 840-FIND-PRSSET1
351100         IF (PRSYSTEM-FOUND)
351200             MOVE PRS-NAME       TO PA52F3-PRS1-NAME.
351300
351400     MOVE SPACES                 TO PA52F3-DPT-NAME.
351500     IF (PCT-DEPARTMENT NOT = SPACES)
351600         MOVE PCT-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
351700         MOVE PCT-DEPARTMENT     TO DB-DEPARTMENT
351800         PERFORM 840-FIND-DPTSET1
351900         IF (DEPTCODE-FOUND)
352000             MOVE DPT-NAME       TO PA52F3-DPT-NAME.
352100
352200     MOVE SPACES                  TO PA52F3-PCO-DESCRIPTION.
352300     IF (PCT-USER-LEVEL NOT = SPACES)
352400         MOVE "UL"                TO DB-TYPE
352500         MOVE PCT-USER-LEVEL      TO DB-CODE
352600         PERFORM 840-FIND-PCOSET1
352700         IF (PCODES-FOUND)
352800             MOVE PCO-DESCRIPTION TO PA52F3-PCO-DESCRIPTION.
352900
353000     MOVE SPACES                  TO PA52F3-PCO1-DESCRIPTION.
353100     IF (PCT-LOCAT-CODE NOT = SPACES)
353200         MOVE "LO"                TO DB-TYPE
353300         MOVE PCT-LOCAT-CODE      TO DB-CODE
353400         PERFORM 840-FIND-PCOSET1
353500         IF (PCODES-FOUND)
353600             MOVE PCO-DESCRIPTION TO PA52F3-PCO1-DESCRIPTION.
353700
353800     MOVE SPACES                  TO PA52F3-HSU-DESCRIPTION.
353900     IF (PCT-SUPERVISOR NOT = SPACES)
354000         MOVE PCT-SUPERVISOR      TO DB-CODE
354100         PERFORM 840-FIND-HSUSET1
354200         IF (HRSUPER-FOUND)
354300             MOVE HSU-DESCRIPTION TO PA52F3-HSU-DESCRIPTION.
354400
354500     MOVE SPACES                  TO PA52F3-PRG-DESCRIPTION.
354600     IF (PCT-PERS-GROUP NOT = SPACES)
354700         MOVE PCT-PERS-GROUP      TO DB-GROUP-NAME
354800         PERFORM 840-FIND-PRGSET1
354900         IF (PERSGROUP-FOUND)
355000             MOVE PRG-DESCRIPTION TO PA52F3-PRG-DESCRIPTION.
355100
090800     MOVE "PAMSG"                TO CRT-ERROR-CAT.
090900     MOVE 100                    TO CRT-MSG-NBR.
091000     PERFORM 790-GET-MSG.
091100     MOVE CRT-MESSAGE            TO PA52F3-COMMENTS.
           INITIALIZE CRT-MESSAGE.

           INITIALIZE PA52F3-COMMENTS-FLAG.
121700     MOVE PA52F3-PCT-COMPANY     TO DB-COMPANY.
121800     INITIALIZE DB-EMP-APP.
121900     MOVE "PA"                   TO DB-CMT-TYPE.
122200     MOVE PA52F3-PCT-ACTION-CODE TO DB-ACTION-CODE.
122000     MOVE PA52F3-PCT-EFFECT-DATE TO DB-DATE.
122000     INITIALIZE DB-EMPLOYEE
122100                DB-JOB-CODE.
122300     MOVE PA52F3-PCT-ACTION-NBR  TO DB-LN-NBR.
122400     MOVE PACSET2-LN-NBR         TO WS-DB-BEG-RNG.
122500     PERFORM 850-KFIND-BEGRNG-PACSET2.
           IF (PACOMMENTS-KFOUND)
               MOVE "*"                TO PA52F3-COMMENTS-FLAG.
122600
355200 481-END.
355300
355400******************************************************************
355500 500-MOVE-DATA.
355600******************************************************************
355700
355800     MOVE PA52F3-PCT-COMPANY         TO PCT-COMPANY.
355900     MOVE "M"                        TO PCT-ACTION-TYPE.
356000     MOVE PA52F3-PCT-ACTION-CODE     TO PCT-ACTION-CODE.
356100     MOVE PA52F3-PCT-ACTION-NBR      TO PCT-ACTION-NBR.
356200     MOVE PA52F3-PCT-EFFECT-DATE     TO PCT-EFFECT-DATE.
356300     MOVE ZEROS                      TO PCT-EMPLOYEE.
356400     MOVE PA52F3-PCT-ANT-END-DATE    TO PCT-ANT-END-DATE.
356500     MOVE PA52F3-PCT-REASON (1)      TO PCT-REASON (1).
356600     MOVE PA52F3-PCT-REASON (2)      TO PCT-REASON (2).
356700     MOVE PA52F3-PCT-PROCESS-LEVEL   TO PCT-PROCESS-LEVEL.
356800     MOVE PA52F3-PCT-DEPARTMENT      TO PCT-DEPARTMENT.
356900     MOVE PA52F3-PCT-USER-LEVEL      TO PCT-USER-LEVEL.
357000     MOVE PA52F3-PCT-SUPERVISOR      TO PCT-SUPERVISOR.
357100     MOVE PA52F3-PCT-LOCAT-CODE      TO PCT-LOCAT-CODE.
357200     MOVE PA52F3-PCT-PERS-GROUP      TO PCT-PERS-GROUP.
357300     MOVE PA52F3-PCT-UPDATE-BENEFIT  TO PCT-UPDATE-BENEFIT.
           MOVE PA52F3-PCT-UPD-ABS-MGMT    TO PCT-UPD-ABS-MGMT.
           MOVE PA52F3-PCT-HIST-CORR-FLAG  TO PCT-HIST-CORR-FLAG.
357400     MOVE 1                          TO PCT-POS-LEVEL.
           IF  (PA52F3-USER-ID NOT = SPACES)
               MOVE PA52F3-USER-ID         TO PCT-USER-ID
           ELSE
               MOVE CRT-USER-NAME          TO PCT-USER-ID
           END-IF.
           MOVE SPACES                     TO PCT-UPDATE-REQ-DED.
           MOVE ZEROES                     TO PCT-EDM-EFFECT-DT.
           MOVE ZEROES                     TO PCT-EDM-END-DATE.
J08104     IF  (PA52F3-FC = "A")
J08104         IF  (PA52F3-USER-ID NOT = SPACES)
J08104             MOVE PA52F3-USER-ID          TO PCT-CREATE-USER-ID
J08104         ELSE
J08104             MOVE CRT-USER-NAME           TO PCT-CREATE-USER-ID
J08104         END-IF
J08104         MOVE WS-SYSTEM-DATE-YMD          TO PCT-CREATE-DATE
J08104         MOVE HHMMSS                      TO PCT-CREATE-TIME
J08104     END-IF.
J08104     MOVE WS-SYSTEM-DATE-YMD              TO PCT-DATE-STAMP.
J08104     MOVE HHMMSS                          TO PCT-TIME-STAMP.
357600
357700     PERFORM
357800         VARYING I1 FROM 1 BY 1
357900         UNTIL  (I1 > 12)
358000
358100         MOVE PA52F3-PAT-FLD-NBR-1 (I1)   TO PCT-FLD-NBR (I1)
358200         MOVE PA52F3-PCT-NEW-VALUE-1 (I1) TO PCT-NEW-VALUE (I1)
358300     END-PERFORM.
358400
358500     PERFORM
358600         VARYING I1 FROM 13 BY 1
358700         UNTIL  (I1 > 24)
358800         COMPUTE I2 = (I1 - 12)
358900         MOVE PA52F3-PAT-FLD-NBR-2 (I2)   TO PCT-FLD-NBR (I1)
359000         MOVE PA52F3-PCT-NEW-VALUE-2 (I2) TO PCT-NEW-VALUE (I1)
359100     END-PERFORM.
359200
359300     PERFORM
359400         VARYING I1 FROM 25 BY 1
359500         UNTIL  (I1 > 36)
359600
359700         COMPUTE I2 = (I1 - 24)
359800         MOVE PA52F3-PAT-FLD-NBR-3 (I2)   TO PCT-FLD-NBR (I1)
359900         MOVE PA52F3-PCT-NEW-VALUE-3 (I2) TO PCT-NEW-VALUE (I1)
360000     END-PERFORM.
360100
           MOVE "Y"                        TO PCT-APPROVAL-FLAG.
P86027     IF  (PA52F3-FC = "A")
               MOVE SPACES                 TO PCT-HOLD-FLAG.

360200 500-END.
360300
360400******************************************************************
360500 PA52S3-TRANSACTION-END.
360600******************************************************************
360700******************************************************************
360800 PA52S4-TRANSACTION SECTION 33.
360900******************************************************************
361000 PA52S4-START.
361100
           MOVE PA52F4-PCT-COMPANY     TO DB-COMPANY.
           MOVE PA52F4-PCT-ACTION-CODE TO DB-ACTION-CODE.
           PERFORM 840-FIND-PATSET1.
J31809     IF  (PERSACTYPE-FOUND)
J31809     AND (PAT-WORKFLOW-FLAG = "Y")
J31809         PERFORM 1000-OPEN-WORKFLOW-DB
J31809     END-IF.

J85747     IF  (PA52F4-FC = "I" OR "N" OR "P")
J85747         MOVE ZEROES                 TO PA52F4-FORM-YEAR
J85747     END-IF.
J85747     IF  (PA52F4-FORM-YEAR           = ZEROES)
J85747         IF  (PA52F4-PCT-EFFECT-DATE NOT = ZEROES)
J85747             MOVE PA52F4-PCT-EFFECT-DATE TO PA52WS-DATE
J85747             IF  (PA52WS-YEAR        NOT < 2020)
J85747                 MOVE PA52WS-YEAR    TO PA52F4-FORM-YEAR
J85747             END-IF
J85747         END-IF
J85747     END-IF.
J85747
J31809*    IF (PA52F4-FC = "A" OR "C")
J31809*        PERFORM 1000-OPEN-WORKFLOW-DB.

199885*    MOVE "PA52.1"               TO PA52F4-CALLED-FROM.
199885     MOVE "PA52.4"               TO PA52F4-CALLED-FROM.

           IF  (PA52F4-FC NOT = "A" AND "C")
               INITIALIZE PA52F4-XMIT-DEDDAT
                          PA52F4-XMIT-REQDED
                          PA52F4-XMIT-HREMP-BLOCK
           END-IF.
P42132  
P42132     MOVE "PA52"                 TO PA52F4-PGM-NAME.
P42132
P62517     INITIALIZE                     HREMP-PEP-FLD-CHG-SW
P63798                                    HREMP-PROCESS-TYPE-SW
P63798                                    HREMP-HIST-CORR-SW.
P62517
           IF  (PA52F4-FC NOT = "A")
           OR  (PA52F4-IMMEDIATE-ACTION NOT = "Y")
               INITIALIZE PA52F4-XMIT-IMMED.

361200     IF (PA52F4-FC = "F")
               IF (PA52F4-PCT-COMPANY     NOT = PA52F4-PT-PCT-COMPANY)
               OR (PA52F4-PCT-ACTION-CODE NOT =
                                             PA52F4-PT-PCT-ACTION-CODE)
                   MOVE 52                             TO CRT-ERROR-NBR
                   MOVE PA52F4-FC-FN                   TO CRT-FIELD-NBR
                   GO TO PA52S4-TRANSACTION-END 
               END-IF
361300         PERFORM 650-DEFAULT
361400         THRU    650-END
361500         GO TO PA52S4-TRANSACTION-END.
361600
116700     MOVE 411                    TO CRT-MSG-NBR.
116800     PERFORM 790-GET-MSG.
116900     MOVE CRT-MESSAGE            TO PA52WS-SEC-MESSAGE.
117000
362200     PERFORM 200-EDIT-TRAN
362300     THRU    200-END.
362400
           MOVE PA52F4-PCT-EFFECT-DATE TO PA52F4-CUC-EFFECT-DATE.

362500     IF (NO-ERROR-FOUND)
362600         PERFORM 400-PROCESS-TRAN
362700         THRU    400-END
               INITIALIZE PA52F4-XMIT-DEDDAT
                          PA52F4-XMIT-REQDED
                          PA52F4-XMIT-HREMP-BLOCK
                          PA52F4-XMIT-IMMED
           END-IF.
J38036
J38036     IF  (PA52F4-PCT-APPROVAL-FLAG = "L")
J38036     AND (CRT-ERROR-NBR = 145)
J38036         INITIALIZE CRT-ERROR-NBR
J38036                    CRT-FIELD-NBR
J38036         MOVE 412                TO CRT-MSG-NBR
J38036         PERFORM 790-GET-MSG
J38036     END-IF.

362900     GO TO PA52S4-TRANSACTION-END.
363000
363100******************************************************************
363200 200-EDIT-TRAN.
363300******************************************************************
363400
363500     PERFORM 210-EDIT-ACCESS
363600     THRU    210-END.
363700
363800     IF (ERROR-FOUND)
363900         GO TO 200-END.
364000
364100     IF (PA52F4-FC = "A" OR "C")
364200         PERFORM 220-EDIT-DATA
364300         THRU    220-END
364400         GO TO 200-END.
364500
364600 200-END.
364700
364800******************************************************************
364900 210-EDIT-ACCESS.
365000******************************************************************
365100
           INITIALIZE                  HREMP-UPDPEP-DATE.

004500     INITIALIZE                     DB-COMPANY
004600                                    DB-PROCESS-LEVEL
004700                                    DB-DEPARTMENT
004800                                    DB-EFFECT-DATE.
004900     PERFORM 850-FIND-NLT-PPRSET1.
005000     IF (PAPOSRULE-FOUND)
005100         MOVE WS-TRUE            TO HREMP-PAUSER-FLAG-SW.

365200     MOVE "A"                    TO PA52F4-PT-ACTION-TYPE.
365300
005800     IF   (PA52F4-FC             = "A")
005900     AND ((PA52F4-PCT-COMPANY     NOT = PA52F4-ORIG-COMPANY)
P49898     OR   (PA52F4-PCT-APPLICANT   NOT = PA52F4-ORIG-APPLICANT)
P49898     OR   (PA52F4-PCT-ACTION-CODE NOT = PA52F4-ORIG-ACTION-CODE)
P49898     OR   (PA52F4-PJR-REQUISITION NOT = PA52F4-ORIG-REQUISITION)
P49898     OR   (PA52F4-PCT-EFFECT-DATE NOT = PA52F4-ORIG-EFFECT-DATE))
006100         MOVE 10                                 TO CRT-ERROR-NBR
006200         MOVE PA52F4-FC-FN                       TO CRT-FIELD-NBR
006300         GO TO 210-END.

365700     MOVE PA52F4-PCT-COMPANY     TO DB-COMPANY.
365800     MOVE SPACES                 TO DB-PROCESS-LEVEL.
365900     PERFORM 840-FIND-PRSSET1.
366000     IF (PRSYSTEM-NOTFOUND)
366100         MOVE 100                                TO CRT-ERROR-NBR
366200         MOVE PA52F4-PCT-COMPANY-FN              TO CRT-FIELD-NBR
366300         GO TO 210-END
           ELSE
368100         MOVE PRS-NAME               TO PA52WS-PRS-NAME.

J85747     IF  (PA52F4-FC = "A" OR "C" OR "I")
J85747     AND (PA52F4-FORM-YEAR NOT = ZEROES)
J85747         IF  (PA52F4-FORM-YEAR < 2020)
J85747             MOVE 429                    TO CRT-ERROR-NBR
J85747             MOVE PA52F4-FORM-YEAR-FN    TO CRT-FIELD-NBR
J85747             GO TO 210-END
J85747         END-IF
J85747     END-IF.

008100     IF (PA52F4-FC = "A" OR "C" OR "I")
008200         MOVE PA52F4-PCT-COMPANY             TO DB-COMPANY
008300         MOVE PA52F4-PCT-ACTION-CODE         TO DB-ACTION-CODE
008400         PERFORM 840-FIND-PATSET1
008500         IF (PERSACTYPE-NOTFOUND)
008600             MOVE 110                            TO CRT-ERROR-NBR
008700             MOVE PA52F4-PCT-ACTION-CODE-FN      TO CRT-FIELD-NBR
008800             GO TO 210-END
               ELSE
               IF (PAT-ACTIVE-FLAG = "2")
                   MOVE 222                       TO CRT-ERROR-NBR
                   MOVE PAT-ACTION-CODE           TO CRT-ERR-VAR1
                   MOVE PA52F4-PCT-ACTION-CODE-FN TO CRT-FIELD-NBR
                   GO TO 210-END
               END-IF.

           IF (PA52F4-FC = "A" OR "C" OR "I")
               MOVE PA52F4-PCT-COMPANY     TO DB-COMPANY
               MOVE PA52F4-PCT-APPLICANT   TO DB-APPLICANT
               PERFORM 840-FIND-APLSET1
               IF (APPLICANT-NOTFOUND)
                   MOVE 220                            TO CRT-ERROR-NBR
                   MOVE PA52F4-PCT-APPLICANT-FN        TO CRT-FIELD-NBR
                   GO TO 210-END
               END-IF

               IF (PA52F4-PJR-REQUISITION NOT = ZEROES)
                   MOVE PA52F4-PCT-COMPANY         TO DB-COMPANY
                   MOVE PA52F4-PJR-REQUISITION     TO DB-REQUISITION
                   PERFORM 840-FIND-PJRSET1
                   IF (PAJOBREQ-NOTFOUND)
                       MOVE 221                        TO CRT-ERROR-NBR
                       MOVE PA52F4-PJR-REQUISITION-FN  TO CRT-FIELD-NBR
                       GO TO 210-END
                   END-IF
               END-IF
           END-IF.
366400
368400     MOVE PA52F4-PCT-COMPANY      TO DB-COMPANY.
368500     MOVE "A"                     TO DB-ACTION-TYPE.
368600     MOVE PA52F4-PCT-APPLICANT    TO DB-EMPLOYEE.
368800     MOVE PA52F4-PCT-ACTION-CODE  TO DB-ACTION-CODE.
368700     MOVE PA52F4-PCT-EFFECT-DATE  TO DB-EFFECT-DATE.
368900     MOVE PA52F4-PCT-ACTION-NBR   TO DB-ACTION-NBR.
369000
           IF (PA52F4-FC = "N")
               IF (DB-EFFECT-DATE = ZEROES)
                   MOVE WS-HIGH-VALUES TO DB-ACTION-NBR
                   PERFORM 850-FIND-NLT-PCTSET2
               ELSE
                   PERFORM 850-FIND-NLT-PCTSET2
                   IF  (PCT-COMPANY     = DB-COMPANY)
                   AND (PCT-ACTION-TYPE = DB-ACTION-TYPE)
                   AND (PCT-EMPLOYEE    = DB-EMPLOYEE)
                   AND (PCT-ACTION-CODE = DB-ACTION-CODE)
                   AND (PCT-EFFECT-DATE = DB-EFFECT-DATE)
                   AND (PCT-ACTION-NBR  = DB-ACTION-NBR)
                       PERFORM 860-FIND-NEXT-PCTSET2
                   END-IF
               END-IF
369800     ELSE
369900     IF (PA52F4-FC = "P")
370000         PERFORM 850-FIND-NLT-PCTSET2
370100         PERFORM 870-FIND-PREV-PCTSET2.
370200
370300     IF (PA52F4-FC = "N" OR "P")
370400         IF (PERSACTION-NOTFOUND)
370500         OR (PCT-COMPANY     NOT = DB-COMPANY)
370600         OR (PCT-ACTION-TYPE NOT = DB-ACTION-TYPE)
370700             MOVE 12                             TO CRT-ERROR-NBR
370800             MOVE PA52F4-FC-FN                   TO CRT-FIELD-NBR
370900             GO TO 210-END
371000         ELSE
371100             MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE
371200                                            PA52F4-PCT-APPLICANT
                   MOVE PCT-REQUISITION        TO PA52F4-PJR-REQUISITION
371300             MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE
371400                                            PA52F4-PCT-ACTION-CODE
371500             MOVE PCT-EFFECT-DATE        TO DB-EFFECT-DATE
371600                                           PA52F4-PCT-EFFECT-DATE
371700             MOVE PCT-ACTION-NBR         TO DB-ACTION-NBR
371800                                            PA52F4-PCT-ACTION-NBR
371900             PERFORM 840-FIND-PATSET1
372000             IF (PERSACTYPE-NOTFOUND)
                       MOVE 110                        TO CRT-ERROR-NBR
                       MOVE PA52F4-PCT-ACTION-CODE-FN  TO CRT-FIELD-NBR
                       GO TO 210-END
                   END-IF.

           IF (PA52F4-FC = "N" OR "P")
               MOVE PA52F4-PCT-COMPANY     TO DB-COMPANY
               MOVE PA52F4-PCT-APPLICANT   TO DB-APPLICANT
               PERFORM 840-FIND-APLSET1
               IF (APPLICANT-NOTFOUND)
                   MOVE 220                            TO CRT-ERROR-NBR
                   MOVE PA52F4-PCT-APPLICANT-FN        TO CRT-FIELD-NBR
                   GO TO 210-END
               END-IF

               IF (PA52F4-PJR-REQUISITION NOT = ZEROES)
                   MOVE PA52F4-PCT-COMPANY         TO DB-COMPANY
                   MOVE PA52F4-PJR-REQUISITION     TO DB-REQUISITION
                   PERFORM 840-FIND-PJRSET1
                   IF (PAJOBREQ-NOTFOUND)
                       MOVE 221                        TO CRT-ERROR-NBR
                       MOVE PA52F4-PJR-REQUISITION-FN  TO CRT-FIELD-NBR
                       GO TO 210-END
                   END-IF
               END-IF
           END-IF.
366400
374800     IF (PA52F4-FC = "N" OR "P")
               PERFORM 8400-AFTER-FIND-PCT
374900         GO TO 210-END.
375000
           PERFORM 840-FIND-PCTSET2.

           IF  (PA52F4-FC = "I")
           AND (PERSACTION-NOTFOUND)
               IF  (PA52F4-PCT-ACTION-CODE = SPACES)
                   MOVE PCTSET2-EMPLOYEE       TO WS-DB-BEG-RNG
               ELSE
               IF  (PA52F4-PCT-EFFECT-DATE = ZEROES)
                   MOVE PCTSET2-ACTION-CODE    TO WS-DB-BEG-RNG
               ELSE
                   MOVE PCTSET2-EFFECT-DATE    TO WS-DB-BEG-RNG
               END-IF
               END-IF
               PERFORM 850-FIND-BEGRNG-PCTSET2
               IF  (PERSACTION-FOUND)
                   MOVE PCT-ACTION-CODE    TO PA52F4-PCT-ACTION-CODE
                   MOVE PCT-EFFECT-DATE    TO PA52F4-PCT-EFFECT-DATE
                   MOVE PCT-ACTION-NBR     TO PA52F4-PCT-ACTION-NBR
               ELSE
                   INITIALIZE PA52F4-PCT-ACTION-NBR
               END-IF
           END-IF.

           IF  (PA52F4-FC = "C" OR "D")
           AND (PERSACTION-NOTFOUND)
               MOVE 125                                TO CRT-ERROR-NBR
               MOVE PA52F4-PCT-ACTION-CODE-FN          TO CRT-FIELD-NBR
               GO TO 210-END.

           IF  (PA52F4-FC = "A")
               MOVE PA52F4-PCT-COMPANY      TO DB-COMPANY
               MOVE "A"                     TO DB-ACTION-TYPE
               MOVE PA52F4-PCT-APPLICANT    TO DB-EMPLOYEE
               MOVE PA52F4-PCT-ACTION-CODE  TO DB-ACTION-CODE
               MOVE PCTSET2-ACTION-CODE     TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PCTSET2
               IF  (PERSACTION-FOUND)
                   MOVE 219                            TO CRT-ERROR-NBR
                   MOVE PA52F4-PCT-ACTION-CODE         TO CRT-ERR-VAR1
                   GO TO 210-END
               END-IF
           END-IF.

           IF  (PA52F4-FC = "I" OR "A")
               MOVE PA52F4-PCT-COMPANY      TO DB-COMPANY
               MOVE "A"                     TO DB-ACTION-TYPE
               MOVE PA52F4-PCT-APPLICANT    TO DB-EMPLOYEE
               MOVE PCTSET2-EMPLOYEE        TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PCTSET2
               IF  (PERSACTION-FOUND)
               AND (PCT-EFFECT-DATE NOT = PA52F4-PCT-EFFECT-DATE)
                   IF  (PA52F4-FC = "I")
                   AND (PA52F4-PCT-EFFECT-DATE = ZEROES)
                       MOVE PCT-EFFECT-DATE    TO PA52F4-PCT-EFFECT-DATE
                   ELSE
                       MOVE PCT-EFFECT-DATE    TO HRWS-DATE-FIELD
                       INITIALIZE                 HRWS-DATE-8-FIELD
                       PERFORM 781-HR-FORMAT-DATE-FIELD
                       MOVE HRWS-VALUE                 TO CRT-ERR-VAR1
                       MOVE 219                        TO CRT-ERROR-NBR
                       MOVE PA52F4-PCT-APPLICANT-FN    TO CRT-FIELD-NBR
                       GO TO 210-END
                   END-IF
               END-IF
           END-IF.

           IF  (PA52F4-FC = "D")
               IF  (PA52F4-NBR-ACTIONS > 1)
               AND (PA52F4-PCT-ACTION-CODE = PA52F4-FIRST-ACTION-CODE)
                   MOVE HRWS-VALUE                     TO CRT-ERR-VAR1
                   MOVE 414                            TO CRT-ERROR-NBR
                   MOVE PA52F4-PCT-ACTION-CODE-FN      TO CRT-FIELD-NBR
                   GO TO 210-END
               END-IF
           END-IF.

379000     IF  (PA52F4-FC NOT = "A")
               GO TO 210-GET-PCT.

           IF  (PA52F4-NBR-ACTIONS >= PA52WS-MAX-HIRE-ACTIONS)
               MOVE 422                                TO CRT-ERROR-NBR
               MOVE PA52WS-MAX-HIRE-ACTIONS            TO CRT-ERR-VAR1
               MOVE PA52F4-FC-FN                       TO CRT-FIELD-NBR
               GO TO 210-END
           END-IF.

           IF  (PA52F4-NBR-ACTIONS > 0)
           AND (PA52F4-IMMEDIATE-ACTION = "Y")
               MOVE 423                                TO CRT-ERROR-NBR
               MOVE PA52F4-IMMEDIATE-ACTION-FN         TO CRT-FIELD-NBR
               GO TO 210-END
           END-IF.

379100     IF  (PA52F4-PCT-EFFECT-DATE  = ZEROES)
379200     AND (PA52F4-IMMEDIATE-ACTION = "N")
379300         MOVE 402                                TO CRT-ERROR-NBR
379400         MOVE PA52F4-IMMEDIATE-ACTION-FN         TO CRT-FIELD-NBR
379500         GO TO 210-END.
379600
379800     IF  (PA52F4-PCT-EFFECT-DATE = ZEROES)
379900         MOVE WS-SYSTEM-DATE-YMD TO PA52F4-PCT-EFFECT-DATE
P68470                                    PA52F4-ORIG-EFFECT-DATE
380000         IF (PA52F4-IMMEDIATE-ACTION = SPACES)
380100             MOVE "Y"            TO PA52F4-IMMEDIATE-ACTION.
380200
380400     IF  (PA52F4-IMMEDIATE-ACTION = SPACES)
380500         MOVE "N"                TO PA52F4-IMMEDIATE-ACTION.
380600
       210-GET-PCT.
368400     MOVE PA52F4-PCT-COMPANY      TO DB-COMPANY.
368500     MOVE "A"                     TO DB-ACTION-TYPE.
368600     MOVE PA52F4-PCT-APPLICANT    TO DB-EMPLOYEE.
368800     MOVE PA52F4-PCT-ACTION-CODE  TO DB-ACTION-CODE.
368700     MOVE PA52F4-PCT-EFFECT-DATE  TO DB-EFFECT-DATE.
368900     MOVE PA52F4-PCT-ACTION-NBR   TO DB-ACTION-NBR.
           PERFORM 840-FIND-PCTSET2.
           PERFORM 8400-AFTER-FIND-PCT.

381400 210-END.
381500
381600******************************************************************
381700 220-EDIT-DATA.
381800******************************************************************
381900
382000     MOVE "PA52 "                             TO CRT-ERROR-CAT.

           IF  (PRS-AUTO-EMPLOYEE NOT = "Y")
               IF  (PA52F4-ASSIGN-EMPLOYEE-NBR = "Y")
                   MOVE 410                            TO CRT-ERROR-NBR
                   MOVE PA52F4-ASSIGN-EMPLOYEE-NBR-FN  TO CRT-FIELD-NBR
                   GO TO 220-END
               END-IF
401900         IF  (PA52F4-PCT-EMPLOYEE = ZEROES)
402100             MOVE 111                            TO CRT-ERROR-NBR
402200             MOVE PA52F4-PCT-EMPLOYEE-FN         TO CRT-FIELD-NBR
402300             GO TO 220-END
               END-IF
           END-IF.

           IF  (PA52F4-FIRST-ACTION-CODE = SPACES)
           OR  (PA52F4-PCT-ACTION-CODE   = PA52F4-FIRST-ACTION-CODE)
               IF  (PRS-AUTO-EMPLOYEE = "Y")
               AND (PA52F4-PCT-EMPLOYEE NOT = PA52F4-FIRST-EMPLOYEE)
               AND (PA52F4-PCT-EMPLOYEE NOT = ZEROES)
               AND (PA52F4-PREASSIGNED-DONE NOT = "Y")
                   MOVE 419                            TO CRT-ERROR-NBR
                   MOVE PA52F4-PCT-EMPLOYEE-FN         TO CRT-FIELD-NBR
                   GO TO 220-END
               END-IF
               IF  (PA52F4-ASSIGN-EMPLOYEE-NBR = "Y")
               AND (PA52F4-PCT-EMPLOYEE NOT = ZEROES)
                   MOVE 421                            TO CRT-ERROR-NBR
                   MOVE PA52F4-ASSIGN-EMPLOYEE-NBR-FN  TO CRT-FIELD-NBR
                   GO TO 220-END
               END-IF
           ELSE
               IF  (PA52F4-PCT-ANT-END-DATE NOT = ZEROES)
                   MOVE 420                            TO CRT-ERROR-NBR
                   MOVE PA52F4-PCT-ANT-END-DATE-FN     TO CRT-FIELD-NBR
                   MOVE PA52F4-FIRST-ACTION-CODE       TO CRT-ERR-VAR1
                   GO TO 220-END
               END-IF
               IF  (PA52F4-PCT-UPDATE-BENEFIT NOT = SPACES)
                   MOVE 420                            TO CRT-ERROR-NBR
                   MOVE PA52F4-PCT-UPDATE-BENEFIT-FN   TO CRT-FIELD-NBR
                   MOVE PA52F4-FIRST-ACTION-CODE       TO CRT-ERR-VAR1
                   GO TO 220-END
               END-IF
               IF  (PA52F4-PCT-UPD-ABS-MGMT NOT = SPACES)
                   MOVE 420                            TO CRT-ERROR-NBR
                   MOVE PA52F4-PCT-UPD-ABS-MGMT-FN     TO CRT-FIELD-NBR
                   MOVE PA52F4-FIRST-ACTION-CODE       TO CRT-ERR-VAR1
                   GO TO 220-END
               END-IF
               IF  (PA52F4-PCT-UPDATE-REQ-DED NOT = SPACES)
                   MOVE 420                            TO CRT-ERROR-NBR
                   MOVE PA52F4-PCT-UPDATE-REQ-DED-FN   TO CRT-FIELD-NBR
                   MOVE PA52F4-FIRST-ACTION-CODE       TO CRT-ERR-VAR1
                   GO TO 220-END
               END-IF
               IF  (PA52F4-PCT-EDM-EFFECT-DT NOT = ZEROES)
                   MOVE 420                            TO CRT-ERROR-NBR
                   MOVE PA52F4-PCT-EDM-EFFECT-DT-FN    TO CRT-FIELD-NBR
                   MOVE PA52F4-FIRST-ACTION-CODE       TO CRT-ERR-VAR1
                   GO TO 220-END
               END-IF
               IF  (PA52F4-ASSIGN-EMPLOYEE-NBR NOT = SPACES)
                   MOVE 420                            TO CRT-ERROR-NBR
                   MOVE PA52F4-ASSIGN-EMPLOYEE-NBR-FN  TO CRT-FIELD-NBR
                   MOVE PA52F4-FIRST-ACTION-CODE       TO CRT-ERR-VAR1
                   GO TO 220-END
               END-IF
               IF  (PA52F4-PCT-REASON (1) NOT = SPACES)
                   MOVE 420                            TO CRT-ERROR-NBR
                   MOVE PA52F4-PCT-REASON-FN (1)       TO CRT-FIELD-NBR
                   MOVE PA52F4-FIRST-ACTION-CODE       TO CRT-ERR-VAR1
                   GO TO 220-END
               END-IF
               IF  (PA52F4-PCT-REASON (2) NOT = SPACES)
                   MOVE 420                            TO CRT-ERROR-NBR
                   MOVE PA52F4-PCT-REASON-FN (2)       TO CRT-FIELD-NBR
                   MOVE PA52F4-FIRST-ACTION-CODE       TO CRT-ERR-VAR1
                   GO TO 220-END
               END-IF
               IF  (PA52F4-PCT-EMPLOYEE NOT = PA52F4-FIRST-EMPLOYEE)
                   MOVE 418                            TO CRT-ERROR-NBR
                   MOVE PA52F4-PCT-EMPLOYEE-FN         TO CRT-FIELD-NBR
                   MOVE PA52F4-FIRST-ACTION-CODE       TO CRT-ERR-VAR1
                   GO TO 220-END
               END-IF
               IF  (PA52F4-ASSIGN-EMPLOYEE-NBR = "Y")
                   MOVE 417                            TO CRT-ERROR-NBR
                   MOVE PA52F4-ASSIGN-EMPLOYEE-NBR-FN  TO CRT-FIELD-NBR
                   MOVE PA52F4-FIRST-ACTION-CODE       TO CRT-ERR-VAR1
                   GO TO 220-END
               END-IF
           END-IF.

402400
382200     IF  (PA52F4-PJR-REQUISITION NOT = ZEROES)
382300     AND (PJR-NBR-APPROVALS      > ZEROES)
382400         INITIALIZE                             PA52WS-APPROVALS
382500         PERFORM
382600             VARYING I1 FROM 1 BY 1
382700             UNTIL  (I1 > 10)
382800                 IF (PJR-APPROVAL-EMP (I1) NOT = ZEROES)
382900                     ADD 1 TO PA52WS-APPROVALS
383000                 END-IF
383100         END-PERFORM
383200         IF (PA52WS-APPROVALS < PJR-NBR-APPROVALS)
383300             COMPUTE PA52WS-DIFF =
383400                 (PJR-NBR-APPROVALS - PA52WS-APPROVALS)
383500             MOVE 104                        TO CRT-ERROR-NBR
383600             MOVE PA52WS-DIFF                TO PA52WS-DIFF-ERR
383700             MOVE PA52WS-DIFF-ERR            TO CRT-ERR-VAR1
383800             MOVE PA52F4-FC-FN               TO CRT-FIELD-NBR
383900             GO TO 220-END.
384000
393800     IF  (PA52F4-FC               = "C")
393900     AND (PA52F4-IMMEDIATE-ACTION = "Y")
394000         MOVE 108                                TO CRT-ERROR-NBR
394100         MOVE PA52F4-IMMEDIATE-ACTION-FN         TO CRT-FIELD-NBR
394200         GO TO 220-END.
394300
J71111     IF (PA52F4-PCT-APPROVAL-FLAG NOT = "L")
J83777         MOVE PCT-APPROVAL-FLAG      TO PA52F4-PCT-APPROVAL-FLAG
J71111     END-IF.
J83777
           IF  (PA52F4-FC                = "C")
           AND (PAT-WORKFLOW-FLAG        = "Y")
           AND (PA52F4-PCT-APPROVAL-FLAG = "Y")
      ******** Action has been approved thru workflow; Cannot change
022100         MOVE 191                                TO CRT-ERROR-NBR
022200         MOVE PA52F4-FC-FN                       TO CRT-FIELD-NBR
022300         GO TO 220-END.
022400
           IF  (PA52F4-IMMEDIATE-ACTION    = "Y")
           AND (PAT-WORKFLOW-FLAG          = "Y")
      ******** Immediate action not allowed; Workflow approval required
028700         MOVE 174                            TO CRT-ERROR-NBR
028800         MOVE PA52F4-IMMEDIATE-ACTION-FN     TO CRT-FIELD-NBR
038400         GO TO 220-END.

394400     IF (APL-DATE-HIRED NOT = ZEROS)
394500         MOVE 218                                TO CRT-ERROR-NBR
394600         MOVE PA52F4-PCT-APPLICANT-FN            TO CRT-FIELD-NBR
394700         GO TO 220-END.
394800
J13588*    MOVE PA52F4-PCT-COMPANY     TO EDCDWS-COMPANY.
J13588*    MOVE "LP"                   TO EDCDWS-SYSTEM.
J13588*    PERFORM 6000-IS-SYSTEM-TRIGGER-ENABLED.
J13588*    IF  (NOT EDCDWS-TRIGGER-ENABLED)
J13588*        IF (PA52F4-PCT-UPD-ABS-MGMT = "Y")
J13588*            MOVE 234            TO CRT-ERROR-NBR
J13588*            MOVE PA52F4-PCT-UPD-ABS-MGMT-FN
J13588*                                TO CRT-FIELD-NBR
J13588*            GO TO 220-END
J13588*        END-IF
J13588*        IF (PA52F4-PCT-UPD-ABS-MGMT = SPACES)
J13588*            MOVE "N"            TO PA52F4-PCT-UPD-ABS-MGMT
J13588*        END-IF.

394900     IF  (PA52F4-PCT-ANT-END-DATE NOT = ZEROS)
395000     AND (PA52F4-PCT-ANT-END-DATE NOT > PA52F4-PCT-EFFECT-DATE)
395100         MOVE 233                                TO CRT-ERROR-NBR
395200         MOVE PA52F4-PCT-ANT-END-DATE-FN         TO CRT-FIELD-NBR
395300         GO TO 220-END.
395400
110700     PERFORM 840-FIND-BNCSET1.
           IF  (BNCOMPANY-NOTFOUND)
           AND (PA52F4-PCT-UPDATE-BENEFIT  = "Y")
      ******** Cannot update benefit; Company is not setup in benefit system
028700         MOVE 180                                TO CRT-ERROR-NBR
028800         MOVE PA52F4-PCT-UPDATE-BENEFIT-FN       TO CRT-FIELD-NBR
028900         GO TO 220-END.

           IF  (PA52F4-PCT-UPDATE-REQ-DED  = "Y")
           AND (PA52F4-PCT-EDM-EFFECT-DT   = ZEROES)
               MOVE PA52F4-PCT-EFFECT-DATE TO PA52F4-PCT-EDM-EFFECT-DT.

           IF  (PA52F4-FIRST-ACTION-CODE = SPACES)
           OR  (PA52F4-PCT-ACTION-CODE   = PA52F4-FIRST-ACTION-CODE)
               IF (PAT-REQUIRE-REASON = 2)
                   IF (PA52F4-PCT-REASON (1) = SPACES)
                   OR (PA52F4-PCT-REASON (2) = SPACES)
                       MOVE 544                        TO CRT-ERROR-NBR
                       MOVE PAT-REQUIRE-REASON         TO CRT-ERR-VAR1
                       IF (PA52F4-PCT-REASON (2) = SPACES)
                           MOVE PA52F4-PCT-REASON-FN (2) TO
                                                         CRT-FIELD-NBR
                       END-IF
                       IF (PA52F4-PCT-REASON (1) = SPACES)
                           MOVE PA52F4-PCT-REASON-FN (1) TO
                                                         CRT-FIELD-NBR
                       END-IF
                       GO TO 220-END
                   END-IF
               END-IF
               IF (PAT-REQUIRE-REASON = 1)
                   IF  (PA52F4-PCT-REASON (1) = SPACES)
                   AND (PA52F4-PCT-REASON (2) = SPACES)
                       MOVE 544                        TO CRT-ERROR-NBR
                       MOVE PAT-REQUIRE-REASON         TO CRT-ERR-VAR1
                       IF (PA52F4-PCT-REASON (1) = SPACES)
                           MOVE PA52F4-PCT-REASON-FN (1) TO
                                                         CRT-FIELD-NBR
                       END-IF
                       GO TO 220-END.

           IF  (PA52F4-PCT-REASON (1) = PA52F4-PCT-REASON (2))
           AND (PA52F4-PCT-REASON (1) NOT = SPACES)
               MOVE 194                            TO CRT-ERROR-NBR
               MOVE PA52F4-PCT-REASON-FN (2)       TO CRT-FIELD-NBR
               GO TO 220-END.

029300     PERFORM 222-EDIT-REASONS
029400     THRU    222-END
029500         VARYING I1 FROM 1 BY 1
029600         UNTIL  (I1 > 2)
029700         OR     (ERROR-FOUND).
396000
396100     IF (ERROR-FOUND)
396200         GO TO 220-END.
396300
029900     IF (ERROR-FOUND)
030000         GO TO 220-END.
030100
           IF  (PA52F4-FC                  = "A")
           AND (PA52F4-PCT-NEW-EFFECT-DATE NOT = ZEROES)
               MOVE 122                                TO CRT-ERROR-NBR
               MOVE PA52F4-PCT-NEW-EFFECT-DATE-FN      TO CRT-FIELD-NBR
               GO TO 220-END.

           IF  (PA52F4-FC = "C")
           AND (PA52F4-PCT-NEW-EFFECT-DATE   NOT = ZEROES)
               IF (PA52F4-PCT-REASON (1)     NOT = PCT-REASON (1))
               OR (PA52F4-PCT-REASON (2)     NOT = PCT-REASON (2))
               OR (PA52F4-PCT-ANT-END-DATE   NOT = PCT-ANT-END-DATE)
               OR (PA52F4-PCT-UPDATE-BENEFIT NOT = PCT-UPDATE-BENEFIT)
               OR (PA52F4-PCT-UPD-ABS-MGMT   NOT = PCT-UPD-ABS-MGMT)
               OR (PA52F4-PCT-UPDATE-REQ-DED NOT = PCT-UPDATE-REQ-DED)
               OR (PA52F4-PCT-EDM-EFFECT-DT  NOT = PCT-EDM-EFFECT-DT)
               OR (PA52F4-PCT-APPLICANT      NOT = PCT-EMPLOYEE)
                   MOVE PA52F4-PCT-NEW-EFFECT-DATE-FN  TO CRT-FIELD-NBR
                   IF  (PA52F4-PCT-REASON (1)     NOT = PCT-REASON (1))
                       MOVE PA52F4-PCT-REASON-FN (1)   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F4-PCT-REASON (2)     NOT = PCT-REASON (2))
                       MOVE PA52F4-PCT-REASON-FN (2)   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F4-PCT-ANT-END-DATE  NOT = PCT-ANT-END-DATE)
                       MOVE PA52F4-PCT-ANT-END-DATE-FN TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F4-PCT-UPDATE-BENEFIT
                                               NOT = PCT-UPDATE-BENEFIT)
                       MOVE PA52F4-PCT-UPDATE-BENEFIT-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F4-PCT-UPD-ABS-MGMT
                                               NOT = PCT-UPD-ABS-MGMT)
                       MOVE PA52F4-PCT-UPD-ABS-MGMT-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F4-PCT-UPDATE-REQ-DED
                                               NOT = PCT-UPDATE-REQ-DED)
                       MOVE PA52F4-PCT-UPDATE-REQ-DED-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F4-PCT-EDM-EFFECT-DT
                                               NOT = PCT-EDM-EFFECT-DT)
                       MOVE PA52F4-PCT-EDM-EFFECT-DT-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F4-PCT-APPLICANT   NOT = PCT-EMPLOYEE)
                       MOVE PA52F4-PCT-APPLICANT-FN    TO CRT-FIELD-NBR
                   END-IF
                   MOVE 124                       TO CRT-ERROR-NBR
                   MOVE PA52F4-PCT-NEW-EFFECT-DATE-FN
                                                  TO CRT-FIELD-NBR
                   GO TO 220-END.

           IF  (PA52F4-FC = "C")
           AND (PA52F4-PCT-NEW-EFFECT-DATE   NOT = ZEROES)
               MOVE 1 TO I2
               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > 12)
                       IF (PA52F4-PCT-NEW-VALUE-1 (I1) NOT =
                                                     PCT-NEW-VALUE (I2))
                           MOVE 124                    TO CRT-ERROR-NBR
                           MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1)
                                                       TO CRT-FIELD-NBR
                           GO TO 220-END
                       END-IF
                       ADD 1 TO I2
               END-PERFORM

               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > 12)
                       IF (PA52F4-PCT-NEW-VALUE-2 (I1) NOT =
                                                     PCT-NEW-VALUE (I2))
                           MOVE 124                    TO CRT-ERROR-NBR
                           MOVE PA52F4-PCT-NEW-VALUE-2-FN (I1)
                                                       TO CRT-FIELD-NBR
                           GO TO 220-END
                       END-IF
                       ADD 1 TO I2
               END-PERFORM

               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > 12)
                       IF (PA52F4-PCT-NEW-VALUE-3 (I1) NOT =
                                                     PCT-NEW-VALUE (I2))
                           MOVE 124                    TO CRT-ERROR-NBR
                           MOVE PA52F4-PCT-NEW-VALUE-3-FN (I1)
                                                       TO CRT-FIELD-NBR
                           GO TO 220-END
                       END-IF
                       ADD 1 TO I2
               END-PERFORM
           END-IF.

           MOVE PA52F4-PCT-EDM-EFFECT-DT   TO PRPXL-PARM-EFF-DATE.

396400     INITIALIZE HREMP-SCR-FIELDS.
396500     INITIALIZE HRPEM-SCR-FIELDS.
396600
396700     MOVE "A"                    TO HREMP-FC.
396800     MOVE PA52F4-PCT-COMPANY     TO HREMP-COMPANY.
396900     MOVE PA52F4-PCT-COMPANY-FN  TO HREMP-COMPANY-FN.
           MOVE PA52F4-PCT-EMPLOYEE    TO HREMP-EMPLOYEE.
           MOVE PA52F4-PCT-EMPLOYEE-FN TO HREMP-EMPLOYEE-FN.
397000     MOVE PA52F4-IMMEDIATE-ACTION
397100                                 TO HREMP-IMM-ACTION.
397200     MOVE PA52F4-PCT-EFFECT-DATE TO HREMP-DATE-HIRED
                                          HREMP-EFFECT-DATE.
397300     MOVE WS-SYSTEM-DATE-YMD     TO HREMP-CREATION-DATE.
397400     MOVE "Y"                    TO HREMP-ACTION.
397500     MOVE "Y"                    TO HREMP-EDIT-VALUE-LIST.
           SET HREMP-EMP-PRE-ASSIGN TO TRUE.

           MOVE PA52F4-XMIT-HREMP-BLOCK
                                       TO HREMP-XMIT-BLOCK.
397900
      * This code to move APL to HREMP should match PA100 1410-MOVE-APL
398000     MOVE APL-LAST-NAME          TO HREMP-LAST-NAME.
398100     MOVE APL-FIRST-NAME         TO HREMP-FIRST-NAME.
398200     MOVE APL-MIDDLE-NAME        TO HREMP-MIDDLE-NAME.
J18344     MOVE APL-MIDDLE-INIT        TO HREMP-MIDDLE-INIT.
398300     MOVE APL-NICK-NAME          TO HREMP-NICK-NAME.
398400     MOVE APL-ADDR1              TO HREMP-ADDR1.
398500     MOVE APL-ADDR2              TO HREMP-ADDR2.
398600     MOVE APL-ADDR3              TO HREMP-ADDR3.
398700     MOVE APL-ADDR4              TO HREMP-ADDR4.
398800     MOVE APL-CITY               TO HREMP-CITY.
398900     MOVE APL-STATE              TO HREMP-STATE.
P62465     MOVE APL-COUNTY             TO HREMP-COUNTY.
399000     MOVE APL-ZIP                TO HREMP-ZIP.
J87867     MOVE APL-EMAIL-ADDRESS      TO HREMP-EMAIL-PERSONAL.
399100     MOVE APL-COUNTRY-CODE       TO HREMP-COUNTRY-CODE.
           MOVE APL-BIRTHDATE          TO HRPEM-BIRTHDATE.
           MOVE APL-EMP-STATUS         TO HREMP-EMP-STATUS.
           MOVE APL-NBR-FTE            TO HREMP-NBR-FTE.
           MOVE APL-WORK-SCHED         TO HREMP-WORK-SCHED.

J09245     MOVE PAAPLWT-LEM-POSITION       TO HREMP-POSITION.
J09245     MOVE PAAPLWT-LEM-HM-DIST-CO     TO HREMP-HM-DIST-CO.
J09245     MOVE PAAPLWT-LEM-HM-ACCT-UNIT   TO HREMP-HM-ACCT-UNIT.
J09245     MOVE PAAPLWT-LEM-HM-ACCOUNT     TO HREMP-HM-ACCOUNT.
J09245     MOVE PAAPLWT-LEM-HM-SUB-ACCT    TO HREMP-HM-SUB-ACCT.
J09245     MOVE PAAPLWT-LEM-JOB-CODE       TO HREMP-JOB-CODE.
J09245     MOVE PAAPLWT-LEM-UNION-CODE     TO HREMP-UNION-CODE.
J09245     MOVE PAAPLWT-LEM-SUPERVISOR     TO HREMP-SUPERVISOR.
J09245     MOVE PAAPLWT-LEM-PAY-FREQUENCY  TO HREMP-PAY-FREQUENCY.
J09245     MOVE PAAPLWT-LEM-SALARY-CLASS   TO HREMP-SALARY-CLASS.
J09245     MOVE PAAPLWT-LEM-EXEMPT-EMP     TO HREMP-EXEMPT-EMP.
J09245     MOVE PAAPLWT-LEM-PAY-RATE       TO HREMP-PAY-RATE.
J09245     MOVE PAAPLWT-LEM-PAY-STEP       TO HREMP-PAY-STEP.
J09245     MOVE PAAPLWT-LEM-PAY-GRADE      TO HREMP-PAY-GRADE.
J09245     MOVE PAAPLWT-LEM-SCHEDULE       TO HREMP-SCHEDULE.
J09245     MOVE PAAPLWT-LEM-OT-PLAN-CODE   TO HREMP-OT-PLAN-CODE.
J09245     MOVE PAAPLWT-LEM-SUPERVISOR-IND TO HREMP-SUPERVISOR-IND.
J09245     MOVE PAAPLWT-LEM-ANNUAL-HOURS   TO HREMP-ANNUAL-HOURS.
J09245     MOVE PAAPLWT-LEM-LAB-DIST-FLAG  TO HREMP-LAB-DIST-FLAG.
J09245     MOVE PAAPLWT-LEM-ENCUMBER-FLAG  TO HREMP-ENCUMBER-FLAG.
J09245     MOVE PAAPLWT-LEM-EFFORT-FLAG    TO HREMP-EFFORT-FLAG.
J09245     MOVE PAAPLWT-LEM-RPT-CURR       TO HREMP-RPT-CURR.
J09245     MOVE PAAPLWT-LEM-PRMCERT-ID     TO HREMP-PRMCERT-ID.
J09245     MOVE PAAPLWT-LEM-SNDCERT-ID     TO HREMP-SNDCERT-ID.
J09245     MOVE PAAPLWT-LEM-FRNG-RATE      TO HREMP-FRNG-RATE.
J09245     MOVE PAAPLWT-LEM-FRNG-ACCT-CAT  TO HREMP-FRNG-ACCT-CAT.
J09245     MOVE PAAPLWT-LEM-FRNG-ACCOUNT   TO HREMP-FRNG-ACCOUNT.
J09245     MOVE PAAPLWT-LEM-FRNG-SUB-ACCT  TO HREMP-FRNG-SUB-ACCT.
J09245     MOVE PAAPLWT-LEM-REMOTE         TO HREMP-REMOTE.
J09245     MOVE PAAPLWT-LEM-LOCAT-CODE     TO HRPEM-LOCAT-CODE.
J09245     MOVE PAAPLWT-LEM-BARGAIN-UNIT   TO HRPEM-BARGAIN-UNIT.
J04749     MOVE PAAPLWT-LEM-LAST-DAY-PAID  TO HREMP-LAST-DAY-PAID.
J04749     MOVE PAAPLWT-LEM-TERM-DATE      TO HREMP-TERM-DATE.
J04749     MOVE PAAPLWT-LEM-ACTIVITY       TO HREMP-ACTIVITY.
J04749     MOVE PAAPLWT-LEM-AUTO-TIME-REC  TO HREMP-AUTO-TIME-REC.
J04749     MOVE PAAPLWT-LEM-AUTO-DEPOSIT   TO HREMP-AUTO-DEPOSIT.
J04749     MOVE PAAPLWT-LEM-DECEASED       TO HREMP-DECEASED.
J04749     MOVE PAAPLWT-LEM-MAIDEN-FST-NM  TO HRPEM-MAIDEN-FST-NM.
J04749     MOVE PAAPLWT-LEM-MAIDEN-LST-NM  TO HRPEM-MAIDEN-LST-NM.
J04749     MOVE PAAPLWT-LEM-MAIDEN-MI      TO HRPEM-MAIDEN-MI.
J04749     MOVE PAAPLWT-LEM-BIRTH-CITY     TO HRPEM-BIRTH-CITY.
J04749     MOVE PAAPLWT-LEM-BIRTH-COUNTRY  TO HRPEM-BIRTH-CNTRY-CD.
J04749     MOVE PAAPLWT-LEM-TAX-PROVINCE   TO HREMP-TAX-PROVINCE.
J04749     MOVE PAAPLWT-LEM-WORK-STATE     TO HREMP-WORK-STATE.
J04749     MOVE PAAPLWT-LEM-WORK-CITY      TO HREMP-WORK-CITY.
J04749     MOVE PAAPLWT-LEM-WORK-COUNTY    TO HREMP-WORK-COUNTY.
J04749     MOVE PAAPLWT-LEM-WORK-COUNTRY   TO HREMP-WORK-COUNTRY.
J04749     MOVE PAAPLWT-LEM-WC-PROVINCE    TO HREMP-WC-PROVINCE.
J04749     MOVE PAAPLWT-LEM-MB-NBR         TO HRPEM-MB-NBR.
J04749     MOVE PAAPLWT-LEM-ABORIGINAL     TO HRPEM-ABORIGINAL.
J04749     MOVE PAAPLWT-LEM-HIGH-COMP      TO HRPEM-HIGH-COMP.
J04749     MOVE PAAPLWT-LEM-SUPP-ADDR1     TO HRPEM-SUPP-ADDR1.
J04749     MOVE PAAPLWT-LEM-SUPP-ADDR2     TO HRPEM-SUPP-ADDR2.
J04749     MOVE PAAPLWT-LEM-SUPP-ADDR3     TO HRPEM-SUPP-ADDR3.
J04749     MOVE PAAPLWT-LEM-SUPP-ADDR4     TO HRPEM-SUPP-ADDR4.
J04749     MOVE PAAPLWT-LEM-SUPP-CITY      TO HRPEM-SUPP-CITY.
J04749     MOVE PAAPLWT-LEM-SUPP-STATE     TO HRPEM-SUPP-STATE.
J04749     MOVE PAAPLWT-LEM-SUPP-ZIP       TO HRPEM-SUPP-ZIP.
J04749     MOVE PAAPLWT-LEM-SUPP-COUNTY    TO HRPEM-SUPP-COUNTY.
J04749     MOVE PAAPLWT-LEM-SUPP-CNTRY-CD  TO HRPEM-SUPP-CNTRY-CD.
J04749     MOVE PAAPLWT-LEM-SUPP-PHONE-CNT TO HRPEM-SUPP-PHONE-CNT.
J04749     MOVE PAAPLWT-LEM-SUPP-PHONE-NBR TO HRPEM-SUPP-PHONE-NBR.
J04749     MOVE PAAPLWT-LEM-FNCTN-GROUP    TO HRPEM-FNCTN-GROUP.
J04749     MOVE PAAPLWT-LEM-EXCLUDE-FLAG   TO HRPEM-EXCLUDE-FLAG.


      * HREMPPD will default WorkCountry and CurrencyCode
           INITIALIZE                     HREMP-WORK-COUNTRY 
J42080                                    HRSEC-WORK-COUNTRY
J42080                                    PA52WS-COUNTRY-SW.
           MOVE 1                      TO HREMP-WORK-COUNTRY-FN.
           INITIALIZE                     HREMP-CURRENCY-CODE.
399500     MOVE APL-VETERAN            TO HRPEM-VETERAN.
399600     MOVE APL-DRAFT-STATUS       TO HRPEM-DRAFT-STATUS.
399700     MOVE APL-FINAL-RANK         TO HRPEM-FINAL-RANK.
399800     MOVE APL-CUR-STATUS         TO HRPEM-CUR-STATUS.
399900     MOVE APL-LAST-PHYSICAL      TO HRPEM-LAST-PHYSICAL.
400000     MOVE APL-HM-PHONE-CNTRY     TO HRPEM-HM-PHONE-CNTRY.
400100     MOVE APL-HM-PHONE-NBR       TO HRPEM-HM-PHONE-NBR.
400200     MOVE APL-HIRE-SOURCE        TO HRPEM-HIRE-SOURCE.
           MOVE APL-NAME-PREFIX        TO HREMP-NAME-PREFIX.
           MOVE APL-NAME-SUFFIX        TO HREMP-NAME-SUFFIX.
           MOVE APL-LANGUAGE-CODE      TO HRPEM-LANGUAGE-CODE.
           MOVE APL-DISABILITY         TO HRPEM-DISABILITY.
736685*    IF (APL-SEX = "X")
736685*        MOVE SPACES             TO HRPEM-SEX
736685*    ELSE
400600         MOVE APL-SEX            TO HRPEM-SEX.
401000     MOVE APL-EEO-CLASS          TO HRPEM-EEO-CLASS.
           MOVE APL-CONSENT            TO HRPEM-CONSENT.
           MOVE APL-RELIGION           TO HRPEM-RELIGION.
           MOVE APL-LAST-NAME-PRE      TO HREMP-LAST-NAME-PRE.
401100     IF (APL-HANDICAP-ID = "X")
401200         MOVE SPACES             TO HRPEM-HANDICAP-ID
401300     ELSE
401400         MOVE APL-HANDICAP-ID    TO HRPEM-HANDICAP-ID.

401700     MOVE APL-FICA-NBR           TO HREMP-FICA-NBR.
           MOVE APL-FORMER-LST-NM      TO HRPEM-FORMER-LST-NM.
           MOVE APL-FORMER-FST-NM      TO HRPEM-FORMER-FST-NM.
           MOVE APL-FORMER-MI          TO HRPEM-FORMER-MI.

J59149     MOVE APL-PROCESS-LEVEL      TO HREMP-PROCESS-LEVEL.
           IF  (PA52F4-PROCESS-LEVEL NOT = SPACES)
               MOVE PA52F4-PROCESS-LEVEL   TO HREMP-PROCESS-LEVEL.
           IF  (PA52F4-DEPARTMENT    NOT = SPACES)
               MOVE PA52F4-DEPARTMENT      TO HREMP-DEPARTMENT.
           IF  (PA52F4-JOB-CODE      NOT = SPACES)
P70173         MOVE PA52F4-JOB-CODE        TO HREMP-JOB-CODE
P70173         MOVE PA52F4-TIPPED          TO HREMP-TIPPED.
           IF  (PA52F4-EMP-STATUS    NOT = SPACES)
               MOVE PA52F4-EMP-STATUS      TO HREMP-EMP-STATUS.
           IF  (PA52F4-PCT-POSITION  NOT = SPACES)
               MOVE PA52F4-PCT-POSITION    TO HREMP-POSITION.
           IF  (PA52F4-SOC-NBR       NOT = SPACES)
               IF  (PA52F4-SOC-NBR = "*BLANK")
                   MOVE SPACES             TO HREMP-FICA-NBR
               ELSE
                   MOVE PA52F4-SOC-NBR     TO HREMP-FICA-NBR
               END-IF.
           IF  (PA52F4-ETHNICITY     NOT = SPACES)
               IF  (PA52F4-ETHNICITY = "*BLANK")
                   MOVE SPACES             TO HRPEM-EEO-CLASS
               ELSE
                   MOVE PA52F4-ETHNICITY   TO HRPEM-EEO-CLASS
               END-IF.
P47308     IF  (PA52F4-TAX-PROVINCE  NOT = SPACES)
P47308         MOVE PA52F4-TAX-PROVINCE    TO HREMP-TAX-PROVINCE
P47308     END-IF.
P56067     IF  (PA52F4-HM-DIST-CO    NOT = ZEROES)
P56067         MOVE PA52F4-HM-DIST-CO      TO HREMP-HM-DIST-CO
P56067     END-IF.
P56067     IF  (PA52F4-HM-ACCT-UNIT  NOT = SPACES)
P56067         MOVE PA52F4-HM-ACCT-UNIT    TO HREMP-HM-ACCT-UNIT
P56067     END-IF.
P51135     IF  (PA52F4-HM-ACCOUNT    NOT = ZEROES)
P51135         MOVE PA52F4-HM-ACCOUNT      TO HREMP-HM-ACCOUNT
P51135     END-IF.
P74764     IF  (PA52F4-HM-SUB-ACCT   NOT = ZEROES)
P74764         MOVE PA52F4-HM-SUB-ACCT     TO HREMP-HM-SUB-ACCT
P74764     END-IF.
P51135     IF  (PA52F4-ACTIVITY      NOT = SPACES)
P51135         MOVE PA52F4-ACTIVITY        TO HREMP-ACTIVITY
P51135     END-IF.
P51135     IF  (PA52F4-ACCT-CATEGORY NOT = SPACES)
P51135         MOVE PA52F4-ACCT-CATEGORY   TO HREMP-ACCT-CATEGORY
P51135     END-IF.

P48650     IF (PA52F4-FC = "A" )
P51532        INITIALIZE                      PA52WS-SCHEDULE
P51532                                        PA52WS-PAY-GRADE 
P51532                                        PA52WS-PAY-STEP
P48650        PERFORM
P48650          VARYING I8 FROM 1 BY 1
P48650          UNTIL (I8 > 12)
P48650          IF (PA52F4-PAT-FLD-NBR-1 (I8)    = HREMP-PAY-GRADE-DN)
P48650             MOVE PA52F4-PCT-NEW-VALUE-1 (I8)  TO PA52WS-PAY-GRADE
P48650          ELSE
P48650          IF (PA52F4-PAT-FLD-NBR-1 (I8)       = HREMP-PAY-STEP-DN)
P48650             IF (PA52F4-PCT-NEW-VALUE-1 (I8) NOT = SPACES)   
P48650                MOVE PA52F4-PCT-NEW-VALUE-1 (I8) TO PA52WS-ALPHA
P48650                PERFORM 430A-CONVERT-NUMBER
P48650                THRU    430A-END         
P48650                MOVE PA52WS-NUMBER             TO PA52WS-PAY-STEP
P48650             ELSE
P48650               MOVE ZEROES                     TO PA52WS-PAY-STEP
P48650             END-IF
P48650          ELSE
P48650          IF (PA52F4-PAT-FLD-NBR-1 (I8)   = HREMP-SCHEDULE-DN)
P48650             MOVE PA52F4-PCT-NEW-VALUE-1 (I8)  TO PA52WS-SCHEDULE
P48650          END-IF
P48650
P48650          IF (PA52F4-PAT-FLD-NBR-2 (I8)   = HREMP-PAY-GRADE-DN)
P48650             MOVE PA52F4-PCT-NEW-VALUE-2 (I8)  TO PA52WS-PAY-GRADE
P48650          ELSE
P48650          IF (PA52F4-PAT-FLD-NBR-2 (I8)       = HREMP-PAY-STEP-DN)
P48650             IF (PA52F4-PCT-NEW-VALUE-2 (I8) NOT = SPACES)
P48650                MOVE PA52F4-PCT-NEW-VALUE-2 (I8) TO PA52WS-ALPHA
P48650                PERFORM 430A-CONVERT-NUMBER
P48650                THRU    430A-END
P48650                MOVE PA52WS-NUMBER              TO PA52WS-PAY-STEP
P48650             ELSE
P48650                MOVE ZEROES                     TO PA52WS-PAY-STEP
P48650             END-IF
P48650          ELSE
P48650          IF (PA52F4-PAT-FLD-NBR-2 (I8)   = HREMP-SCHEDULE-DN)
P48650             MOVE PA52F4-PCT-NEW-VALUE-2 (I8)  TO PA52WS-SCHEDULE
P48650          END-IF
P48650
P48650          IF (PA52F4-PAT-FLD-NBR-3 (I8)   = HREMP-PAY-GRADE-DN)
P48650             MOVE PA52F4-PCT-NEW-VALUE-3 (I8)  TO PA52WS-PAY-GRADE
P48650          ELSE
P48650          IF (PA52F4-PAT-FLD-NBR-3 (I8)       = HREMP-PAY-STEP-DN)
P48650            IF (PA52F4-PCT-NEW-VALUE-3 (I8) NOT = SPACES)
P48650               MOVE PA52F4-PCT-NEW-VALUE-3 (I8) TO PA52WS-ALPHA
P48650               PERFORM 430A-CONVERT-NUMBER
P48650               THRU    430A-END
P48650               MOVE PA52WS-NUMBER               TO PA52WS-PAY-STEP
P48650            ELSE
P48650               MOVE ZEROES                      TO PA52WS-PAY-STEP
P48650            END-IF
P48650          ELSE
P48650          IF (PA52F4-PAT-FLD-NBR-3 (I8)   = HREMP-SCHEDULE-DN)
P48650             MOVE PA52F4-PCT-NEW-VALUE-3 (I8)  TO PA52WS-SCHEDULE
P48650          END-IF
P48650        END-PERFORM
P48650
P48650        IF  (PA52WS-SCHEDULE   NOT = SPACES)
P48650        AND (PA52WS-PAY-GRADE  NOT = SPACES)
P48650        AND (PA52WS-PAY-STEP   NOT = ZEROES)
P48650            MOVE PA52WS-SCHEDULE          TO DB-SCHEDULE
P48650            MOVE PA52WS-PAY-GRADE         TO DB-PAY-GRADE
P48650            MOVE PA52WS-PAY-STEP          TO DB-PAY-STEP
P48650            MOVE PA52F4-PCT-EFFECT-DATE   TO DB-EFFECT-DATE
P48650            PERFORM 850-FIND-NLT-SGDSET3
P48650            IF (PRSAGDTL-FOUND)
P48650            AND (SGD-COMPANY   = DB-COMPANY)
P48650            AND (SGD-SCHEDULE  = DB-SCHEDULE)
P48650            AND (SGD-PAY-GRADE = DB-PAY-GRADE)
P48650            AND (SGD-PAY-STEP  = DB-PAY-STEP)
P48650                MOVE SGD-PAY-RATE         TO PA52WS-PAY-RATE
P48650                MOVE PA52WS-PAY-RATE      TO HRWS-DEC-FIELD
P48650                MOVE 0                    TO HRWS-SIZE
P48650                MOVE 4                    TO HRWS-NBR-DECIMALS
P48650                PERFORM 770-HR-FORMAT-DEC-FIELD
P48650                MOVE HRWS-VALUE           TO PA52WS-VALUE
P48650            END-IF
P48650            PERFORM
P48650              VARYING I1 FROM 1 BY 1
P48650              UNTIL (I1 > 12)
P48650              IF (PA52F4-PAT-FLD-NBR-1 (I1)   = HREMP-PAY-RATE-DN)
P48650                 MOVE PA52WS-VALUE  TO PA52F4-PCT-NEW-VALUE-1 (I1)
P48650              ELSE
P48650              IF (PA52F4-PAT-FLD-NBR-2 (I1)   = HREMP-PAY-RATE-DN)
P48650                 MOVE PA52WS-VALUE  TO PA52F4-PCT-NEW-VALUE-2 (I1)
P48650              ELSE
P48650              IF (PA52F4-PAT-FLD-NBR-3 (I1)   = HREMP-PAY-RATE-DN)
P48650                 MOVE PA52WS-VALUE  TO PA52F4-PCT-NEW-VALUE-3 (I1)
P48650              END-IF
P48650            END-PERFORM
P48650        END-IF
P48650     END-IF.
P48650
032000     PERFORM
032100         VARYING I1 FROM 1 BY 1
032200         UNTIL  (I1 > 12)
                   IF  (PA52F4-PAT-FLD-NBR-1 (I1)   = 150)
                   AND (PA52F4-PCT-PRE-VALUE-1 (I1) NOT = SPACES)
                   AND (PA52F4-PCT-NEW-VALUE-1 (I1) = SPACES)
                       MOVE APL-WK-PHONE-CNTRY TO HRPEM-WK-PHONE-CNTRY
                   END-IF
                   IF  (PA52F4-PAT-FLD-NBR-1 (I1)   = 11)
                   AND (PA52F4-PCT-PRE-VALUE-1 (I1) NOT = SPACES)
                   AND (PA52F4-PCT-NEW-VALUE-1 (I1) = SPACES)
                       MOVE APL-WK-PHONE-NBR TO HRPEM-WK-PHONE-NBR
                   END-IF
                   IF  (PA52F4-PAT-FLD-NBR-1 (I1)   = 12)
                   AND (PA52F4-PCT-PRE-VALUE-1 (I1) NOT = SPACES)
                   AND (PA52F4-PCT-NEW-VALUE-1 (I1) = SPACES)
                       MOVE APL-WK-PHONE-EXT TO HRPEM-WK-PHONE-EXT
                   END-IF
032300             MOVE PA52F4-PCT-NEW-VALUE-1 (I1)
032400                                  TO PA52WS-PCT-NEW-VALUE (I1)
032500             MOVE PA52F4-PCT-PRE-VALUE-1 (I1)
032600                                  TO PA52WS-PCT-PRE-VALUE (I1)
                   MOVE PA52F4-PAT-FLD-NBR-1 (I1)
                                        TO PA52WS-PAT-FIELD-NBR (I1)
032700     END-PERFORM.
032800
032900     PERFORM
033000         VARYING I1 FROM 13 BY 1
033100         UNTIL  (I1 > 24)
033200             COMPUTE I8 = (I1 - 12)
                   IF  (PA52F4-PAT-FLD-NBR-2 (I8)   = 150)
                   AND (PA52F4-PCT-PRE-VALUE-2 (I8) NOT = SPACES)
                   AND (PA52F4-PCT-NEW-VALUE-2 (I8) = SPACES)
                       MOVE APL-WK-PHONE-CNTRY TO HRPEM-WK-PHONE-CNTRY
                   END-IF
                   IF  (PA52F4-PAT-FLD-NBR-2 (I8)   = 11)
                   AND (PA52F4-PCT-PRE-VALUE-2 (I8) NOT = SPACES)
                   AND (PA52F4-PCT-NEW-VALUE-2 (I8) = SPACES)
                       MOVE APL-WK-PHONE-NBR TO HRPEM-WK-PHONE-NBR
                   END-IF
                   IF  (PA52F4-PAT-FLD-NBR-2 (I8)   = 12)
                   AND (PA52F4-PCT-PRE-VALUE-2 (I8) NOT = SPACES)
                   AND (PA52F4-PCT-NEW-VALUE-2 (I8) = SPACES)
                       MOVE APL-WK-PHONE-EXT TO HRPEM-WK-PHONE-EXT
                   END-IF
033300             MOVE PA52F4-PCT-NEW-VALUE-2 (I8)
033400                                  TO PA52WS-PCT-NEW-VALUE (I1)
033500             MOVE PA52F4-PCT-PRE-VALUE-2 (I8)
033600                                  TO PA52WS-PCT-PRE-VALUE (I1)
                   MOVE PA52F4-PAT-FLD-NBR-2 (I8)
                                        TO PA52WS-PAT-FIELD-NBR (I1)
033700     END-PERFORM.
033800
033900     PERFORM
034000         VARYING I1 FROM 25 BY 1
034100         UNTIL  (I1 > 36)
034200             COMPUTE I8 = (I1 - 24)
                   IF  (PA52F4-PAT-FLD-NBR-3 (I8)   = 150)
                   AND (PA52F4-PCT-PRE-VALUE-3 (I8) NOT = SPACES)
                   AND (PA52F4-PCT-NEW-VALUE-3 (I8) = SPACES)
                       MOVE APL-WK-PHONE-CNTRY TO HRPEM-WK-PHONE-CNTRY
                   END-IF
                   IF  (PA52F4-PAT-FLD-NBR-3 (I8)   = 11)
                   AND (PA52F4-PCT-PRE-VALUE-3 (I8) NOT = SPACES)
                   AND (PA52F4-PCT-NEW-VALUE-3 (I8) = SPACES)
                       MOVE APL-WK-PHONE-NBR TO HRPEM-WK-PHONE-NBR
                   END-IF
                   IF  (PA52F4-PAT-FLD-NBR-3 (I8)   = 12)
                   AND (PA52F4-PCT-PRE-VALUE-3 (I8) NOT = SPACES)
                   AND (PA52F4-PCT-NEW-VALUE-3 (I8) = SPACES)
                       MOVE APL-WK-PHONE-EXT TO HRPEM-WK-PHONE-EXT
                   END-IF
034300             MOVE PA52F4-PCT-NEW-VALUE-3 (I8)
034400                                  TO PA52WS-PCT-NEW-VALUE (I1)
034500             MOVE PA52F4-PCT-PRE-VALUE-3 (I8)
034600                                  TO PA52WS-PCT-PRE-VALUE (I1)
                   MOVE PA52F4-PAT-FLD-NBR-3 (I8)
                                        TO PA52WS-PAT-FIELD-NBR (I1)
034700     END-PERFORM.
034800
402500     PERFORM 224-EDIT-NEW-VALUES
402600     THRU    224-END
402700         VARYING I1 FROM 1 BY 1
402800         UNTIL  (I1 > 36)
402900         OR     (ERROR-FOUND).
403000
403100     IF (ERROR-FOUND)
403200         GO TO 220-END.
403300
           IF (PA52WS-NOTHING-ENTERED)
               MOVE 145                                TO CRT-ERROR-NBR
               MOVE PA52F4-PCT-NEW-VALUE-1-FN (1)      TO CRT-FIELD-NBR
               GO TO 220-END.

           INITIALIZE PA52WS-EMP-STATUS
                      PA52WS-TERM-FN.

           SET NO-PL-ENTERED TO TRUE.
           SET NO-ST-ENTERED TO TRUE.
           SET HREMP-EDIT-FTE          TO TRUE.
           IF  (PA52F4-IMMEDIATE-ACTION = "N")
               IF ((PA52F4-FIRST-ACTION-CODE NOT = SPACES)
               OR  (PA52F4-PCT-ACTION-CODE   NOT =
                                          PA52F4-FIRST-ACTION-CODE))
                   SET HREMP-NOEDIT-FTE        TO TRUE.

           MOVE PA52F4-FC-FN           TO PA52WS-PL-FN.
           MOVE PA52F4-FC-FN           TO PA52WS-ST-FN.
           INITIALIZE PA52WS-POSITION.
           MOVE PA52F4-PROCESS-LEVEL   TO PA52WS-PL.
           MOVE PA52F4-DEPARTMENT      TO PA52WS-DEPARTMENT.
           MOVE PA52F4-EMP-STATUS      TO PA52WS-EMP-STATUS.
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 12)
               IF  (PA52F4-PAT-FLD-NBR-1 (I1) = HREMP-EMP-STATUS-DN)
                   MOVE PA52F4-PCT-NEW-VALUE-1 (I1) TO PA52WS-EMP-STATUS
                   MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1) TO PA52WS-ST-FN
                   IF  (PA52F4-PCT-NEW-VALUE-1 (I1) NOT = SPACES)
                       SET ST-ENTERED TO TRUE
                   END-IF
               END-IF
               IF  (PA52F4-PAT-FLD-NBR-2 (I1) = HREMP-EMP-STATUS-DN)
                   MOVE PA52F4-PCT-NEW-VALUE-2 (I1) TO PA52WS-EMP-STATUS
                   MOVE PA52F4-PCT-NEW-VALUE-2-FN (I1) TO PA52WS-ST-FN
                   IF  (PA52F4-PCT-NEW-VALUE-2 (I1) NOT = SPACES)
                       SET ST-ENTERED TO TRUE
                   END-IF
               END-IF
               IF  (PA52F4-PAT-FLD-NBR-3 (I1) = HREMP-EMP-STATUS-DN)
                   MOVE PA52F4-PCT-NEW-VALUE-3 (I1) TO PA52WS-EMP-STATUS
                   MOVE PA52F4-PCT-NEW-VALUE-3-FN (I1) TO PA52WS-ST-FN
                   IF  (PA52F4-PCT-NEW-VALUE-3 (I1) NOT = SPACES)
                       SET ST-ENTERED TO TRUE
                   END-IF
               END-IF
               IF  (PA52F4-PAT-FLD-NBR-1 (I1) = HREMP-TERM-DATE-DN)
                   MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1) TO PA52WS-TERM-FN
               END-IF
               IF  (PA52F4-PAT-FLD-NBR-2 (I1) = HREMP-TERM-DATE-DN)
                   MOVE PA52F4-PCT-NEW-VALUE-2-FN (I1) TO PA52WS-TERM-FN
               END-IF
               IF  (PA52F4-PAT-FLD-NBR-3 (I1) = HREMP-TERM-DATE-DN)
                   MOVE PA52F4-PCT-NEW-VALUE-3-FN (I1) TO PA52WS-TERM-FN
               END-IF
               IF  (PA52F4-IMMEDIATE-ACTION = "N")
                   IF ((PA52F4-FIRST-ACTION-CODE NOT = SPACES)
                   OR  (PA52F4-PCT-ACTION-CODE   NOT =
                                              PA52F4-FIRST-ACTION-CODE))
                       IF  (PA52F4-PAT-FLD-NBR-1 (I1) =
                                                      HREMP-NBR-FTE-DN)
                           SET HREMP-EDIT-FTE          TO TRUE
                       END-IF
                       IF  (PA52F4-PAT-FLD-NBR-2 (I1) =
                                                      HREMP-NBR-FTE-DN)
                           SET HREMP-EDIT-FTE          TO TRUE
                       END-IF
                       IF  (PA52F4-PAT-FLD-NBR-3 (I1) =
                                                      HREMP-NBR-FTE-DN)
                           SET HREMP-EDIT-FTE          TO TRUE
                       END-IF
                   END-IF
               END-IF
               IF  (PA52F4-PAT-FLD-NBR-1 (I1) = HREMP-PROCESS-LEVEL-DN)
                   MOVE PA52F4-PCT-NEW-VALUE-1 (I1)    TO PA52WS-PL
                   MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1) TO PA52WS-PL-FN
                   IF  (PA52F4-PCT-NEW-VALUE-1 (I1) NOT = SPACES)
J34836             OR  (PA52F4-PCT-PRE-VALUE-1 (I1) NOT = SPACES)
                       SET PL-ENTERED TO TRUE
                   END-IF
               END-IF
               IF  (PA52F4-PAT-FLD-NBR-2 (I1) = HREMP-PROCESS-LEVEL-DN)
                   MOVE PA52F4-PCT-NEW-VALUE-2 (I1)    TO PA52WS-PL
                   MOVE PA52F4-PCT-NEW-VALUE-2-FN (I1) TO PA52WS-PL-FN
                   IF  (PA52F4-PCT-NEW-VALUE-2 (I1) NOT = SPACES)
J34836             OR  (PA52F4-PCT-PRE-VALUE-2 (I1) NOT = SPACES)
                       SET PL-ENTERED TO TRUE
                   END-IF
               END-IF
               IF  (PA52F4-PAT-FLD-NBR-3 (I1) = HREMP-PROCESS-LEVEL-DN)
                   MOVE PA52F4-PCT-NEW-VALUE-3 (I1)    TO PA52WS-PL
                   MOVE PA52F4-PCT-NEW-VALUE-3-FN (I1) TO PA52WS-PL-FN
                   IF  (PA52F4-PCT-NEW-VALUE-3 (I1) NOT = SPACES)
J34836             OR  (PA52F4-PCT-PRE-VALUE-3 (I1) NOT = SPACES)
                       SET PL-ENTERED TO TRUE
                   END-IF
               END-IF
               IF  (PA52F4-PAT-FLD-NBR-1 (I1) = HREMP-DEPARTMENT-DN)
                   MOVE PA52F4-PCT-NEW-VALUE-1 (I1)
                                               TO PA52WS-DEPARTMENT
               END-IF
               IF  (PA52F4-PAT-FLD-NBR-2 (I1) = HREMP-DEPARTMENT-DN)
                   MOVE PA52F4-PCT-NEW-VALUE-2 (I1)
                                               TO PA52WS-DEPARTMENT
               END-IF
               IF  (PA52F4-PAT-FLD-NBR-3 (I1) = HREMP-DEPARTMENT-DN)
                   MOVE PA52F4-PCT-NEW-VALUE-3 (I1)
                                               TO PA52WS-DEPARTMENT
               END-IF
               IF  (PA52F4-PAT-FLD-NBR-1 (I1) = HREMP-POSITION-DN)
                   MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1)
                                               TO PA52WS-POS-FN
               END-IF
               IF  (PA52F4-PAT-FLD-NBR-2 (I1) = HREMP-POSITION-DN)
                   MOVE PA52F4-PCT-NEW-VALUE-2-FN (I1)
                                               TO PA52WS-POS-FN
               END-IF
               IF  (PA52F4-PAT-FLD-NBR-3 (I1) = HREMP-POSITION-DN)
                   MOVE PA52F4-PCT-NEW-VALUE-3-FN (I1)
                                               TO PA52WS-POS-FN
               END-IF
               IF  (PA52F4-PCT-POSITION NOT = SPACES)
                   IF  (PA52F4-PAT-FLD-NBR-1 (I1) = HREMP-POSITION-DN)
                   AND (PA52F4-PCT-NEW-VALUE-1 (I1) NOT = SPACES)
                   AND (PA52F4-PCT-NEW-VALUE-1 (I1) NOT =
                                                    PA52F4-PCT-POSITION)
                       MOVE 424                        TO CRT-ERROR-NBR
                       MOVE PA52F4-PCT-POSITION        TO CRT-ERR-VAR1
                       MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1)
                                                       TO CRT-FIELD-NBR
                       GO TO 220-END
                   END-IF
                   IF  (PA52F4-PAT-FLD-NBR-2 (I1) = HREMP-POSITION-DN)
                   AND (PA52F4-PCT-NEW-VALUE-2 (I1) NOT = SPACES)
                   AND (PA52F4-PCT-NEW-VALUE-2 (I1) NOT =
                                                    PA52F4-PCT-POSITION)
                       MOVE 424                        TO CRT-ERROR-NBR
                       MOVE PA52F4-PCT-POSITION        TO CRT-ERR-VAR1
                       MOVE PA52F4-PCT-NEW-VALUE-2-FN (I1)
                                                       TO CRT-FIELD-NBR
                       GO TO 220-END
                   END-IF
                   IF  (PA52F4-PAT-FLD-NBR-3 (I1) = HREMP-POSITION-DN)
                   AND (PA52F4-PCT-NEW-VALUE-3 (I1) NOT = SPACES)
                   AND (PA52F4-PCT-NEW-VALUE-3 (I1) NOT =
                                                    PA52F4-PCT-POSITION)
                       MOVE 424                        TO CRT-ERROR-NBR
                       MOVE PA52F4-PCT-POSITION        TO CRT-ERR-VAR1
                       MOVE PA52F4-PCT-NEW-VALUE-3-FN (I1)
                                                       TO CRT-FIELD-NBR
                       GO TO 220-END
                   END-IF
               ELSE
                   IF  (PA52F4-PAT-FLD-NBR-1 (I1) = HREMP-POSITION-DN)
                       MOVE PA52F4-PCT-NEW-VALUE-1 (I1)
                                                   TO PA52WS-POSITION
                   END-IF
                   IF  (PA52F4-PAT-FLD-NBR-2 (I1) = HREMP-POSITION-DN)
                       MOVE PA52F4-PCT-NEW-VALUE-2 (I1)
                                                   TO PA52WS-POSITION
                   END-IF
                   IF  (PA52F4-PAT-FLD-NBR-3 (I1) = HREMP-POSITION-DN)
                       MOVE PA52F4-PCT-NEW-VALUE-3 (I1)
                                                   TO PA52WS-POSITION
                   END-IF
               END-IF
           END-PERFORM.
           IF  (PA52F4-IMMEDIATE-ACTION = "N")
               IF  (PA52F4-FIRST-ACTION-CODE = SPACES)
               OR  (PA52F4-PCT-ACTION-CODE = PA52F4-FIRST-ACTION-CODE)
                   IF  (NO-PL-ENTERED)
                       MOVE 415                        TO CRT-ERROR-NBR
                       MOVE PA52WS-PL-FN               TO CRT-FIELD-NBR
                       GO TO 220-END
                   END-IF
                   IF  (NO-ST-ENTERED)
                   AND (APL-EMP-STATUS = SPACES)
                       MOVE 426                        TO CRT-ERROR-NBR
                       MOVE PA52WS-ST-FN               TO CRT-FIELD-NBR
                       GO TO 220-END
                   END-IF
               ELSE
                   IF  (PL-ENTERED)
                       MOVE 416                        TO CRT-ERROR-NBR
                       MOVE PA52WS-PL-FN               TO CRT-FIELD-NBR
                       GO TO 220-END
                   END-IF
                   IF  (ST-ENTERED)
                       MOVE 427                        TO CRT-ERROR-NBR
                       MOVE PA52WS-ST-FN               TO CRT-FIELD-NBR
                       GO TO 220-END
                   END-IF
               END-IF
           END-IF.

           INITIALIZE HREMP-COUNT.
           IF  (PA52WS-EMP-STATUS NOT = SPACES)
               MOVE PA52F4-PCT-COMPANY     TO DB-COMPANY
               MOVE PA52WS-EMP-STATUS      TO DB-EMP-STATUS
               PERFORM 840-FIND-EMSSET1
               IF (EMSTATUS-FOUND)
                   MOVE EMS-COUNT          TO HREMP-COUNT
               END-IF
           END-IF.
403900
           SET HREMP-HIRE-ACTION TO TRUE.
           MOVE HREMP-WORK-COUNTRY             TO HRSEC-WORK-COUNTRY.
           MOVE HREMP-PROCESS-LEVEL            TO HRSEC-PROCESS-LEVEL.
           IF  (PA52F4-IMMEDIATE-ACTION = "N")
               SET PRPXL-UPDATES-BYPASS TO TRUE
           ELSE
               SET PRPXL-UPDATES-ALLOWED TO TRUE.

           IF  (PA52F4-ASSIGN-EMPLOYEE-NBR = "Y")
           AND (PA52F4-IMMEDIATE-ACTION NOT = "Y")
           AND (PA52F4-PREASSIGNED-DONE NOT = "Y")
               MOVE PA52F4-PCT-COMPANY     TO DB-COMPANY
               MOVE SPACES                 TO DB-PROCESS-LEVEL
               PERFORM 840-FIND-PRSSET1

               MOVE PRS-COMPANY            TO DB-COMPANY
               MOVE PRS-LAST-EMPLOYEE      TO DB-EMPLOYEE
               ADD 1                       TO DB-EMPLOYEE
               PERFORM 840-KFIND-EMPSET1
               PERFORM
                   UNTIL (EMPLOYEE-KNOTFOUND)
                   ADD 1                   TO DB-EMPLOYEE
                   PERFORM 840-KFIND-EMPSET1
               END-PERFORM
               IF  (DB-EMPLOYEE > PRS-LAST-EMPLOYEE)
                   PERFORM 910-AUDIT-BEGIN
                   PERFORM 840-MODIFY-PRSSET1
                   MOVE DB-EMPLOYEE            TO PRS-LAST-EMPLOYEE
                   PERFORM 820-STORE-PRSYSTEM
                   PERFORM 925-AUDIT-END
               END-IF
               MOVE DB-EMPLOYEE        TO PA52F4-PCT-EMPLOYEE
                                          HREMP-EMPLOYEE
               MOVE "Y"                TO PA52F4-EMP-AUTO-ASSIGNED
                                          PA52F4-PREASSIGNED-DONE
               INITIALIZE                 PA52F4-ASSIGN-EMPLOYEE-NBR
           END-IF.

           IF  (PRS-AUTO-EMPLOYEE = "Y")
           AND (PA52F4-ASSIGN-EMPLOYEE-NBR = "N")
               MOVE WS-TRUE            TO HREMP-AUTO-Y-SW.

           IF  (PA52F4-ASSIGN-EMPLOYEE-NBR = "Y")
           AND (PA52F4-IMMEDIATE-ACTION = "Y")
               INITIALIZE                 PA52F4-ASSIGN-EMPLOYEE-NBR
           END-IF.

404100     PERFORM 2000-HREMP-EDIT-TRAN.
           INITIALIZE HREMP-HIRE-ACTION-SW.

           IF  (CRT-ERROR-NBR = 101)
           AND (CRT-ERROR-CAT = "PAPPR")
               IF  (PA52WS-POS-FN NOT = ZEROES)
                   MOVE PA52WS-POS-FN                  TO CRT-FIELD-NBR
               ELSE
                   MOVE PA52F4-FC-FN                   TO CRT-FIELD-NBR
               END-IF
           END-IF.

           MOVE HREMP-CURRENCY-CODE    TO PAPCT-EMP-CURRENCY.
           MOVE HREMP-CURR-ND          TO PAPCT-EMP-CURR-ND.

           IF  (PAPCT-EMP-CURRENCY = SPACES)
               MOVE PA52F4-PCT-COMPANY     TO DB-COMPANY
               MOVE HREMP-PROCESS-LEVEL    TO DB-PROCESS-LEVEL
               PERFORM 840-FIND-PRSSET1
               IF  (PRSYSTEM-FOUND)
                   MOVE PRS-CURRENCY-CODE  TO PAPCT-EMP-CURRENCY
                   MOVE PRS-CURR-ND        TO PAPCT-EMP-CURR-ND
               END-IF
           END-IF.

404200     MOVE HREMP-PAPOS-EFFECT-DATE    TO PA52WS-POS-EFFECT-DATE.
404400
404500     IF (HREMP-UPDPEP-DATE = PA52F4-PCT-EFFECT-DATE)
404600         INITIALIZE                 HREMP-UPDPEP-DATE.
404700
           MOVE HREMP-XMIT-BLOCK       TO PA52F4-XMIT-HREMP-BLOCK.
405100
405200     IF (ERROR-FOUND)
               IF (PA52F4-EMP-AUTO-ASSIGNED = "Y")
                   PERFORM 221-RESET-EMPLOYEE
                   THRU    221-END
               END-IF
405300         GO TO 220-END.
405400
           IF (HRSEC-USE-COUNTRY)
               SET PA52WS-COUNTRY-REQ          TO TRUE
           ELSE
               SET PA52WS-COMPANY-REQ          TO TRUE
           END-IF.

      * 225-EDIT is a 2nd pass of 224-EDIT for Currency fields
402500     PERFORM 225-EDIT-NEW-VALUES2
402600     THRU    225-END
402700         VARYING I1 FROM 1 BY 1
402800         UNTIL  (I1 > 36)
402900         OR     (ERROR-FOUND).
403000
403100     IF (ERROR-FOUND)
               IF (PA52F4-EMP-AUTO-ASSIGNED = "Y")
                   PERFORM 221-RESET-EMPLOYEE
                   THRU    221-END
               END-IF
403200         GO TO 220-END.
403300
           IF  (HREMP-TERM-DATE NOT = ZEROES)
           AND (HREMP-TERM-DATE < HREMP-DATE-HIRED)
               MOVE HREMP-DATE-HIRED   TO HRWS-DATE-FIELD
               INITIALIZE                 HRWS-DATE-8-FIELD
               PERFORM 781-HR-FORMAT-DATE-FIELD
               MOVE HRWS-VALUE                         TO CRT-ERR-VAR1
               MOVE 425                                TO CRT-ERROR-NBR
               MOVE PA52WS-TERM-FN                     TO CRT-FIELD-NBR
               IF (PA52F4-EMP-AUTO-ASSIGNED = "Y")
                   PERFORM 221-RESET-EMPLOYEE
                   THRU    221-END
               END-IF
               GO TO 220-END
           END-IF.

           IF  (PA52F4-IMMEDIATE-ACTION = "Y")
           AND (PA52F4-XMIT-IMMED       = ZEROS)
               MOVE 1                  TO PA52F4-XMIT-IMMED
               IF  (PA52F4-PCT-EFFECT-DATE > WS-SYSTEM-DATE-YMD)
                   MOVE 401                            TO CRT-ERROR-NBR
               ELSE
                   MOVE 404                            TO CRT-ERROR-NBR
               END-IF
               MOVE PA52F4-IMMEDIATE-ACTION-FN         TO CRT-FIELD-NBR
               GO TO 220-END
           END-IF.

405800 220-END.
405900
406000******************************************************************
406100 221-RESET-EMPLOYEE.
406200******************************************************************
406300
           IF (PA52F4-FC NOT = "A")
               GO TO 221-END.

           MOVE PA52F4-PCT-COMPANY     TO DB-COMPANY.
           MOVE SPACES                 TO DB-PROCESS-LEVEL.
           PERFORM 840-FIND-PRSSET1.
           IF  (PRS-LAST-EMPLOYEE = PA52F4-PCT-EMPLOYEE)
           AND (PRS-LAST-EMPLOYEE > 1)
               PERFORM 910-AUDIT-BEGIN
               PERFORM 840-MODIFY-PRSSET1
               SUBTRACT 1 FROM PRS-LAST-EMPLOYEE
               PERFORM 820-STORE-PRSYSTEM
               PERFORM 925-AUDIT-END
           END-IF.

           MOVE ZEROES                 TO PA52F4-PCT-EMPLOYEE.
           MOVE "Y"                    TO PA52F4-ASSIGN-EMPLOYEE-NBR.
P49680     INITIALIZE                     PA52F4-PREASSIGNED-DONE. 

       221-END.

406000******************************************************************
406100 222-EDIT-REASONS.
406200******************************************************************
406300
406400     IF (PA52F4-PCT-REASON (I1) = SPACES)
406500         GO TO 222-END.
406600
           SET PA52WS-NO-ACTION-REASON TO TRUE.

           MOVE PA52F4-PCT-COMPANY     TO DB-COMPANY.
           MOVE PA52F4-PCT-ACTION-CODE TO DB-ACTION-CODE.
049700     MOVE CRESET4-ACTION-CODE    TO WS-DB-BEG-RNG.
           PERFORM 850-KFIND-BEGRNG-CRESET4.
           IF (PAACTREAS-KFOUND)
               SET PA52WS-ACTION-REASON     TO TRUE
           END-IF.

050000     IF  (PA52F4-FC              = "A")
050100     OR ((PA52F4-FC              = "C")
050200     AND (PA52F4-PCT-REASON (I1) NOT = PCT-REASON (I1))
           AND (PA52F4-PCT-REASON (I1) NOT = SPACES))
               MOVE PA52F4-PCT-REASON (I1) TO DB-ACT-REASON-CD
               IF (PA52WS-ACTION-REASON)
                   PERFORM 840-FIND-CRESET4
               ELSE
                   MOVE CRESET3-ACT-REASON-CD    TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-CRESET3
               END-IF
050400         IF (PAACTREAS-NOTFOUND)
                   IF (PA52WS-ACTION-REASON)
                       MOVE 142                TO CRT-ERROR-NBR
                   ELSE
050500                 MOVE 140                        TO CRT-ERROR-NBR
                   END-IF
050600             MOVE PA52F4-PCT-REASON-FN (I1)      TO CRT-FIELD-NBR
050700             GO TO 222-END
050800         ELSE
050900             IF (CRE-ACTIVE-FLAG NOT = "1")
051000                 MOVE 141                        TO CRT-ERROR-NBR
051100                 MOVE PA52F4-PCT-REASON-FN (I1)  TO CRT-FIELD-NBR
051200                 GO TO 222-END.
051300
408400 222-END.
408500
051600******************************************************************
051700 224-EDIT-NEW-VALUES.
051800******************************************************************
051900
055500     IF  (PA52WS-PCT-NEW-VALUE (I1) NOT = SPACES)
055600     AND (PA52WS-PAT-FIELD-NBR (I1)     = ZEROS)
055700         MOVE 137                                TO CRT-ERROR-NBR
055800         IF (I1 < 13)
055900             MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1) TO CRT-FIELD-NBR
056000         ELSE
056100         IF (I1 > 24)
056200             COMPUTE I8 = (I1 - 24)
056300             MOVE PA52F4-PCT-NEW-VALUE-3-FN (I8) TO CRT-FIELD-NBR
056400         ELSE
056500         IF (I1 > 12)
056600             COMPUTE I8 = (I1 - 12)
056700             MOVE PA52F4-PCT-NEW-VALUE-2-FN (I8) TO CRT-FIELD-NBR
056800         END-IF
056900         END-IF
057000         END-IF
057100         GO TO 224-END.

           IF  (PA52WS-PAT-FIELD-NBR (I1) = ZEROES)
               GO TO 224-END.

           IF  (PA52WS-PCT-PRE-VALUE (I1) = PA52WS-SEC-MESSAGE)
           AND (PA52WS-PCT-NEW-VALUE (I1) NOT = SPACES)
               MOVE 193                TO CRT-ERROR-NBR
               IF (I1 < 13)
                   MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1) TO CRT-FIELD-NBR
               ELSE
               IF (I1 > 24)
                   COMPUTE I8 = (I1 - 24)
                   MOVE PA52F4-PCT-NEW-VALUE-3-FN (I8) TO CRT-FIELD-NBR
               ELSE
               IF (I1 > 12)
                   COMPUTE I8 = (I1 - 12)
                   MOVE PA52F4-PCT-NEW-VALUE-2-FN (I8) TO CRT-FIELD-NBR
               END-IF
               END-IF
               END-IF
               GO TO 224-END.

           IF  ((PA52WS-PAT-FIELD-NBR (I1) = HREMP-BASE-PAY-RATE-DN)
           OR   (PA52WS-PAT-FIELD-NBR (I1) = HREMP-BASE-CURRENCY-DN))
           AND (PA52WS-PCT-NEW-VALUE (I1) NOT = SPACES)
               MOVE 192                TO CRT-ERROR-NBR
               IF (I1 < 13)
                   MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1) TO CRT-FIELD-NBR
               ELSE
               IF (I1 > 24)
                   COMPUTE I8 = (I1 - 24)
                   MOVE PA52F4-PCT-NEW-VALUE-3-FN (I8) TO CRT-FIELD-NBR
               ELSE
               IF (I1 > 12)
                   COMPUTE I8 = (I1 - 12)
                   MOVE PA52F4-PCT-NEW-VALUE-2-FN (I8) TO CRT-FIELD-NBR
               END-IF
               END-IF
               END-IF
               GO TO 224-END.

           MOVE PA52WS-PAT-FIELD-NBR (I1)    TO PAPCT-FLD-NBR
                                                DB-FLD-NBR.

           IF  (HRWS-PAD-TYPE  (DB-FLD-NBR) = SPACES)
               PERFORM 840-FIND-PADSET1
               MOVE PAD-DATA-TYPE      TO HRWS-PAD-TYPE (DB-FLD-NBR)
               MOVE PAD-DECIMALS       TO HRWS-PAD-DEC  (DB-FLD-NBR)
               MOVE PAD-CURRENCY-FLAG  TO HRWS-PAD-CURR (DB-FLD-NBR)
               MOVE PAD-SIZE           TO HRWS-PAD-SIZE (DB-FLD-NBR)
               MOVE PAD-CASE-FLAG      TO HRWS-PAD-CASE (DB-FLD-NBR)
           END-IF.

052500     IF (PA52WS-PCT-NEW-VALUE (I1) = SPACES)
052600         IF (PA52WS-PCT-PRE-VALUE (I1) = SPACES)
                   IF  (PA52WS-PAT-FIELD-NBR (I1) > 1999)
                   AND (PA52WS-PAT-FIELD-NBR (I1) < 2100)
                       MOVE PA52WS-PAT-FIELD-NBR (I1)
                                                    TO PA52WS-USER-FLD
                       MOVE PA52WS-USER-FLD         TO DB-FIELD-KEY
                       IF (PA52WS-COUNTRY-REQ)
                           MOVE HRSEC-WORK-COUNTRY TO DB-COUNTRY-CD-REQ
                       ELSE
                           INITIALIZE                  DB-COUNTRY-CD-REQ
                                                       DB-PROCESS-LEVEL
                       END-IF
                       PERFORM 840-FIND-PASSET1
                       IF  (PASCRTY-FOUND)
                       AND (PAS-REQ-FLAG = "X")
                           MOVE 50                 TO CRT-ERROR-NBR
                           IF (I1 < 13)
                               MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1)
                                                   TO CRT-FIELD-NBR
                           ELSE
                           IF (I1 > 24)
                               COMPUTE I8 = (I1 - 24)
                               MOVE PA52F4-PCT-NEW-VALUE-3-FN (I8)
                                                   TO CRT-FIELD-NBR
                           ELSE
                           IF (I1 > 12)
                               COMPUTE I8 = (I1 - 12)
                               MOVE PA52F4-PCT-NEW-VALUE-2-FN (I8)
                                                   TO CRT-FIELD-NBR
                           END-IF
                           END-IF
                           END-IF
                       END-IF
                   ELSE
052700                 MOVE PA52F4-PCT-COMPANY          TO DB-COMPANY
052800                 MOVE PA52WS-PAT-FIELD-NBR (I1)   TO DB-FLD-NBR
                       IF (PA52WS-COUNTRY-REQ)
                           MOVE HRSEC-WORK-COUNTRY     TO
                                                       DB-COUNTRY-CD-REQ
                       ELSE
                           INITIALIZE                  DB-COUNTRY-CD-REQ
                                                       DB-PROCESS-LEVEL
                       END-IF
052900                 PERFORM 840-FIND-PASSET1
053000                 IF  (PASCRTY-FOUND)
053100                 AND (PAS-REQ-FLAG = "X")
053200                     MOVE 50                      TO CRT-ERROR-NBR
053300                     IF (I1 < 13)
053400                         MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1)
053500                                                  TO CRT-FIELD-NBR
053600                     ELSE
053700                     IF (I1 > 24)
053800                         COMPUTE I8 = (I1 - 24)
053900                         MOVE PA52F4-PCT-NEW-VALUE-3-FN (I8)
054000                                                  TO CRT-FIELD-NBR
054100                     ELSE
054200                     IF (I1 > 12)
054300                         COMPUTE I8 = (I1 - 12)
054400                         MOVE PA52F4-PCT-NEW-VALUE-2-FN (I8)
054500                                                  TO CRT-FIELD-NBR
054600                     END-IF
054700                     END-IF
054800                     END-IF
054900                 END-IF
                   END-IF
055000         END-IF
055100         GO TO 224-END.
055200
           SET PA52WS-SOMETHING-ENTERED TO TRUE.
057200
057300     IF (PA52WS-PAT-FIELD-NBR (I1) >= 2000)
061000         GO TO 224-END.
061100
061200     MOVE PA52WS-PCT-NEW-VALUE (I1)  TO HRWS-UP-FIELD.
061500
           IF  (HRWS-UP-FIELD (1:1) = "*")
           OR  ((HRWS-PAD-TYPE (PAPCT-FLD-NBR) = "A")
            AND (HRWS-PAD-CASE (PAPCT-FLD-NBR) = "Y"))
064900             PERFORM 760-HR-UPPER-CASE
065000             MOVE HRWS-UP-FIELD     TO PA52WS-PCT-NEW-VALUE (I1)
065100             IF (I1 < 13)
065200                 MOVE HRWS-UP-FIELD TO PA52F4-PCT-NEW-VALUE-1 (I1)
065300             ELSE
065400             IF (I1 > 24)
065500                 COMPUTE I8 = (I1 - 24)
065600                 MOVE HRWS-UP-FIELD TO PA52F4-PCT-NEW-VALUE-3 (I8)
065700             ELSE
065800             IF (I1 > 12)
065900                 COMPUTE I8 = (I1 - 12)
066000                 MOVE HRWS-UP-FIELD
066000                                    TO PA52F4-PCT-NEW-VALUE-2 (I8)
                   END-IF.

J53204**** Fields which will be validated using APPLICANT file values
J53204**** are with length greater than 20. Other previous values or 
J53204**** PA52WS-PCT-PRE-VALUE (I1) were already obtained at prior 
J53204**** part of the program.                                        
J53204     MOVE PA52WS-PAT-FIELD-NBR (I1)  TO DB-FLD-NBR.
J53204     PERFORM 840-FIND-PADSET1.
J53204     IF  (PADICT-FOUND)  
J53204     AND (PAD-SIZE > 20)
J29752         MOVE PA52WS-PAT-FIELD-NBR (I1) TO HREMP-FLD-NBR 
J29752         MOVE "Y"                       TO HREMP-FORMAT-FIELD 
J29752         MOVE PA52F4-PCT-COMPANY        TO HREMP-COMPANY 
J29752         MOVE SPACES                    TO HREMP-VALUE 
J29752         PERFORM 5000-PAAPL-RETRIEVE-VALUE 
J29752         MOVE HREMP-VALUE               TO  
J53204                                        PA52WS-PCT-PRE-VALUE (I1)
J53204     END-IF.
066100
066200     IF (PA52WS-PCT-NEW-VALUE (I1) = PA52WS-PCT-PRE-VALUE (I1))
P81486         IF  (PA52F4-PCT-APPROVAL-FLAG = "L")
P81486              MOVE SPACES           TO PA52WS-PCT-NEW-VALUE (I1)
P81486         ELSE
066300              MOVE 146                           TO CRT-ERROR-NBR
P81486         END-IF
066400         IF (I1 < 13)
P81486             IF  (PA52F4-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F4-PCT-NEW-VALUE-1 (I1)
P81486             ELSE
066500                  MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1)
066600                                                 TO CRT-FIELD-NBR
P81486             END-IF
066700         ELSE
066800         IF (I1 > 24)
066900             COMPUTE I8 = (I1 - 24)
P81486             IF  (PA52F4-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F4-PCT-NEW-VALUE-3 (I8)
P81486             ELSE
067000                  MOVE PA52F4-PCT-NEW-VALUE-3-FN (I8)
067100                                                 TO CRT-FIELD-NBR
P81486             END-IF
067200         ELSE
067300         IF (I1 > 12)
067400             COMPUTE I8 = (I1 - 12)
P81486             IF  (PA52F4-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F4-PCT-NEW-VALUE-2 (I8)
P81486             ELSE
067500                 MOVE PA52F4-PCT-NEW-VALUE-2-FN (I8)
067600                                                 TO CRT-FIELD-NBR
P81486             END-IF
067700         END-IF
067800         END-IF
P81486         END-IF
067900         GO TO 224-END.
068000
068100     IF  (PA52WS-PCT-NEW-VALUE (I1) = "*BLANK")
068200     AND (PA52WS-PCT-PRE-VALUE (I1) = SPACES)
P81486         IF  (PA52F4-PCT-APPROVAL-FLAG = "L")
P81486              MOVE SPACES           TO PA52WS-PCT-NEW-VALUE (I1)
P81486         ELSE
068300              MOVE 147                           TO CRT-ERROR-NBR
P81486         END-IF
068400         IF (I1 < 13)
P81486             IF  (PA52F4-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F4-PCT-NEW-VALUE-1 (I1)
P81486             ELSE
068500                  MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1)
068600                                                 TO CRT-FIELD-NBR 
P81486             END-IF
068700         ELSE
068800         IF (I1 > 24)
068900             COMPUTE I8 = (I1 - 24)
P81486             IF  (PA52F4-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F4-PCT-NEW-VALUE-3 (I8)
P81486             ELSE
069000                  MOVE PA52F4-PCT-NEW-VALUE-3-FN (I8)
069100                                                 TO CRT-FIELD-NBR
P81486             END-IF
069200         ELSE
069300         IF (I1 > 12)
069400             COMPUTE I8 = (I1 - 12)
P81486             IF  (PA52F4-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F4-PCT-NEW-VALUE-3 (I8)
P81486             ELSE
069500                  MOVE PA52F4-PCT-NEW-VALUE-2-FN (I8)
069600                                                 TO CRT-FIELD-NBR
P81486             END-IF                                               
069700         END-IF
069800         END-IF
069900         END-IF
070000         GO TO 224-END.
070100
070200     IF  (PA52WS-PCT-NEW-VALUE (I1) NOT = "*BLANK")
070300         MOVE PA52WS-PCT-NEW-VALUE (I1)    TO PAPCT-NEW-VALUE
070400         MOVE PA52WS-PCT-NEW-VALUE (I1)    TO PAPCT-FIELD
070500         IF (I1 < 13)
070600             MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1)
070700                                           TO PAPCT-FIELD-FN
070800         ELSE
070900         IF (I1 > 24)
071000             COMPUTE I8 = (I1 - 24)
071100             MOVE PA52F4-PCT-NEW-VALUE-3-FN (I8)
071200                                           TO PAPCT-FIELD-FN
071300         ELSE
071400         IF (I1 > 12)
071500             COMPUTE I8 = (I1 - 12)
071600             MOVE PA52F4-PCT-NEW-VALUE-2-FN (I8)
071700                                           TO PAPCT-FIELD-FN
071800         END-IF
071900         END-IF
072000         END-IF
072200         MOVE HRWS-PAD-TYPE (PAPCT-FLD-NBR)  TO PAPCT-TYPE
072300         MOVE HRWS-PAD-SIZE (PAPCT-FLD-NBR)  TO PAPCT-SIZE
               MOVE HRWS-PAD-DEC (PAPCT-FLD-NBR)   TO PAPCT-DECIMALS
               SET PAPCT-NO-CURRENCY TO TRUE
               SET PAPCT-NO-REFORMAT TO TRUE
072500         PERFORM 5400-EDIT-CORRECT-FORMAT
072600         IF (ERROR-FOUND)
072700             GO TO 224-END
072800         ELSE
072900             MOVE PAPCT-NEW-VALUE    TO PA52WS-PCT-NEW-VALUE (I1)
073000             IF (I1 < 13)
073100                 MOVE PAPCT-NEW-VALUE
073200                                 TO PA52F4-PCT-NEW-VALUE-1 (I1)
073300             ELSE
073400             IF (I1 > 24)
073500                 COMPUTE I8 = (I1 - 24)
073600                 MOVE PAPCT-NEW-VALUE
073700                                 TO PA52F4-PCT-NEW-VALUE-3 (I8)
073800             ELSE
073900             IF (I1 > 12)
074000                 COMPUTE I8 = (I1 - 12)
074100                 MOVE PAPCT-NEW-VALUE
074200                                 TO PA52F4-PCT-NEW-VALUE-2 (I8).

074400     INITIALIZE PAPCT-SCR-FIELDS.
074500     MOVE PA52F4-PCT-COMPANY           TO PAPCT-COMPANY.
074600     MOVE PA52F4-PCT-APPLICANT         TO PAPCT-APPLICANT.
074600     MOVE PA52F4-PCT-EMPLOYEE          TO PAPCT-EMPLOYEE.
074700     MOVE PA52WS-PAT-FIELD-NBR (I1)    TO PAPCT-FLD-NBR.
074800     MOVE PA52WS-PCT-NEW-VALUE (I1)    TO PAPCT-NEW-VALUE.
           MOVE PA52F4-PCT-EFFECT-DATE       TO PAPCT-EFFECT-DATE.
074900     IF (I1 < 13)
075000         MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1)
075100                                       TO PAPCT-FIELD-FN
075200     ELSE
075300     IF (I1 > 24)
075400         COMPUTE I8 = (I1 - 24)
075500         MOVE PA52F4-PCT-NEW-VALUE-3-FN (I8)
075600                                       TO PAPCT-FIELD-FN
075700     ELSE
075800     IF (I1 > 12)
075900         COMPUTE I8 = (I1 - 12)
076000         MOVE PA52F4-PCT-NEW-VALUE-2-FN (I8)
076100                                       TO PAPCT-FIELD-FN.
076200     PERFORM 5000-PAPCT-MOVE-TO-HREMP.
076300
076400 224-END.
076500
051600******************************************************************
051700 225-EDIT-NEW-VALUES2.
051800******************************************************************
051900
052500     IF  ((PA52WS-PCT-NEW-VALUE (I1) = SPACES)
            AND (PA52WS-PCT-PRE-VALUE (I1) = SPACES))
           OR  (PA52WS-PCT-NEW-VALUE (I1) = "*BLANK")
               IF  (PA52WS-PAT-FIELD-NBR (I1) = HREMP-PAY-RATE-DN)
               AND (HREMP-PAY-STEP NOT = ZEROES)
                   GO TO 225-END
               END-IF
                   IF  (PA52WS-PAT-FIELD-NBR (I1) > 1999)
                   AND (PA52WS-PAT-FIELD-NBR (I1) < 2100)
                       MOVE PA52WS-PAT-FIELD-NBR (I1)
                                                   TO DB-FLD-NBR
                       IF (PA52WS-COUNTRY-REQ)
                           MOVE HRSEC-WORK-COUNTRY TO DB-COUNTRY-CD-REQ
                       ELSE
                           INITIALIZE                 DB-COUNTRY-CD-REQ
                                                      DB-PROCESS-LEVEL
                       END-IF
                       PERFORM 840-FIND-PASSET1
                       IF  (PASCRTY-FOUND)
                       AND (PAS-REQ-FLAG = "X")
                           MOVE 50                 TO CRT-ERROR-NBR
                           IF (I1 < 13)
                               MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1)
                                                   TO CRT-FIELD-NBR
                           ELSE
                           IF (I1 > 24)
                               COMPUTE I8 = (I1 - 24)
                               MOVE PA52F4-PCT-NEW-VALUE-3-FN (I8)
                                                   TO CRT-FIELD-NBR
                           ELSE
                           IF (I1 > 12)
                               COMPUTE I8 = (I1 - 12)
                               MOVE PA52F4-PCT-NEW-VALUE-2-FN (I8)
                                                   TO CRT-FIELD-NBR
                           END-IF
                           END-IF
                           END-IF
                           GO TO 225-END
                       END-IF
                   ELSE
052700                 MOVE PA52F4-PCT-COMPANY          TO DB-COMPANY
052800                 MOVE PA52WS-PAT-FIELD-NBR (I1)   TO DB-FLD-NBR
                       IF (PA52WS-COUNTRY-REQ)
                           MOVE HRSEC-WORK-COUNTRY      TO
                                                       DB-COUNTRY-CD-REQ
                       ELSE
                           INITIALIZE                  DB-COUNTRY-CD-REQ
                                                       DB-PROCESS-LEVEL
                       END-IF
052900                 PERFORM 840-FIND-PASSET1
053000                 IF  (PASCRTY-FOUND)
053100                 AND (PAS-REQ-FLAG = "X")
053200                     MOVE 50                      TO CRT-ERROR-NBR
053300                     IF (I1 < 13)
053400                         MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1)
053500                                                  TO CRT-FIELD-NBR
053600                     ELSE
053700                     IF (I1 > 24)
                               COMPUTE I8 = (I1 - 24)
053900                         MOVE PA52F4-PCT-NEW-VALUE-3-FN (I8)
054000                                                  TO CRT-FIELD-NBR
054100                     ELSE
054200                     IF (I1 > 12)
                               COMPUTE I8 = (I1 - 12)
054400                         MOVE PA52F4-PCT-NEW-VALUE-2-FN (I8)
054500                                                  TO CRT-FIELD-NBR
054600                     END-IF
054700                     END-IF
054800                     END-IF
054900                 END-IF
                       GO TO 225-END
                   END-IF
055000         END-IF.
055200
           IF  (PA52WS-PAT-FIELD-NBR (I1) = ZEROES)
           OR  (PA52WS-PCT-NEW-VALUE (I1) = SPACES)
           OR  (PA52WS-PCT-NEW-VALUE (I1) = "*BLANK")
               GO TO 225-END.

           MOVE PA52WS-PAT-FIELD-NBR (I1)    TO PAPCT-FLD-NBR
                                                DB-FLD-NBR.

           IF  (HRWS-PAD-TYPE  (DB-FLD-NBR) = SPACES)
               PERFORM 840-FIND-PADSET1
               MOVE PAD-DATA-TYPE      TO HRWS-PAD-TYPE (DB-FLD-NBR)
               MOVE PAD-DECIMALS       TO HRWS-PAD-DEC  (DB-FLD-NBR)
               MOVE PAD-CURRENCY-FLAG  TO HRWS-PAD-CURR (DB-FLD-NBR)
               MOVE PAD-SIZE           TO HRWS-PAD-SIZE (DB-FLD-NBR)
               MOVE PAD-CASE-FLAG      TO HRWS-PAD-CASE (DB-FLD-NBR)
           END-IF.

           INITIALIZE PAPCT-SCR-FIELDS.
           MOVE PA52WS-PAT-FIELD-NBR (I1)    TO PAPCT-FLD-NBR.
           MOVE PA52WS-PCT-NEW-VALUE (I1)    TO PAPCT-NEW-VALUE.
           IF  (HRWS-PAD-CURR (PAPCT-FLD-NBR) = "Y")
               MOVE PAPCT-EMP-CURRENCY       TO PAPCT-CURRENCY-CODE
               MOVE PAPCT-EMP-CURR-ND        TO PAPCT-CURR-ND
           END-IF.

           IF (I1 < 13)
               MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1) TO PAPCT-FIELD-FN
           ELSE
           IF (I1 > 24)
               COMPUTE I8 = (I1 - 24)
               MOVE PA52F4-PCT-NEW-VALUE-3-FN (I8) TO PAPCT-FIELD-FN
           ELSE
           IF (I1 > 12)
               COMPUTE I8 = (I1 - 12)
               MOVE PA52F4-PCT-NEW-VALUE-2-FN (I8) TO PAPCT-FIELD-FN
           END-IF.

           MOVE PA52F4-PCT-COMPANY           TO PAPCT-COMPANY.
           MOVE PA52F4-PCT-APPLICANT         TO PAPCT-EMPLOYEE.
           MOVE 1                            TO PAPCT-EMP-APP.
           MOVE PA52F4-PCT-EFFECT-DATE       TO PAPCT-EFFECT-DATE.

           IF (PA52WS-PAT-FIELD-NBR (I1) < 2000)
               SET PAPCT-CURRENCY TO TRUE
               SET PAPCT-REFORMAT TO TRUE
               MOVE HRWS-PAD-TYPE (PAPCT-FLD-NBR)  TO PAPCT-TYPE
               MOVE HRWS-PAD-SIZE (PAPCT-FLD-NBR)  TO PAPCT-SIZE
               MOVE HRWS-PAD-DEC (PAPCT-FLD-NBR)   TO PAPCT-DECIMALS
               PERFORM 5400-EDIT-CORRECT-FORMAT
           ELSE
               PERFORM 5100-EDIT-USER-FIELDS.

           MOVE PAPCT-NEW-VALUE        TO PA52WS-PCT-NEW-VALUE (I1).
           IF (I1 < 13)
               MOVE PAPCT-NEW-VALUE    TO PA52F4-PCT-NEW-VALUE-1 (I1)
           ELSE
           IF (I1 > 24)
               COMPUTE I8 = (I1 - 24)
               MOVE PAPCT-NEW-VALUE    TO PA52F4-PCT-NEW-VALUE-3 (I8)
           ELSE
           IF (I1 > 12)
               COMPUTE I8 = (I1 - 12)
               MOVE PAPCT-NEW-VALUE    TO PA52F4-PCT-NEW-VALUE-2 (I8)
           END-IF.
J53204**** Fields which will be validated using APPLICANT file values
J53204**** are with length greater than 20. Other previous values or
J53204**** PA52WS-PCT-PRE-VALUE (I1) were already obtained at prior
J53204**** part of the program.
J53204     MOVE PA52WS-PAT-FIELD-NBR (I1)  TO DB-FLD-NBR.
J53204     PERFORM 840-FIND-PADSET1.
J53204     IF  (PADICT-FOUND)  
J53204     AND (PAD-SIZE > 20)
J29752         MOVE PA52WS-PAT-FIELD-NBR (I1) TO HREMP-FLD-NBR 
J29752         MOVE "Y"                       TO HREMP-FORMAT-FIELD 
J29752         MOVE PA52F4-PCT-COMPANY        TO HREMP-COMPANY 
J29752         MOVE SPACES                    TO HREMP-VALUE 
J29752         PERFORM 5000-PAAPL-RETRIEVE-VALUE 
J29752         MOVE HREMP-VALUE               TO                    
J53204                                        PA52WS-PCT-PRE-VALUE (I1)
J53204     END-IF. 

           IF (PA52WS-PCT-NEW-VALUE (I1) = PA52WS-PCT-PRE-VALUE (I1))
P81486         IF  (PA52F4-PCT-APPROVAL-FLAG = "L")
P81486              MOVE SPACES        TO PA52WS-PCT-NEW-VALUE (I1)
P81486         ELSE
                    MOVE 146                           TO CRT-ERROR-NBR
P81486         END-IF
               IF (I1 < 13)
P81486             IF  (PA52F4-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F4-PCT-NEW-VALUE-1 (I1)
P81486             ELSE
                        MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1)
                                                       TO CRT-FIELD-NBR
P81486             END-IF
               ELSE
               IF (I1 > 24)
                   COMPUTE I8 = (I1 - 24)
P81486             IF  (PA52F4-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F4-PCT-NEW-VALUE-3 (I8)
P81486             ELSE
                        MOVE PA52F4-PCT-NEW-VALUE-3-FN (I8)
                                                       TO CRT-FIELD-NBR
P81486             END-IF
               ELSE
               IF (I1 > 12)
                   COMPUTE I8 = (I1 - 12)
P81486             IF  (PA52F4-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F4-PCT-NEW-VALUE-2 (I8)
P81486             ELSE
                        MOVE PA52F4-PCT-NEW-VALUE-2-FN (I8)
                                                       TO CRT-FIELD-NBR
P81486             END-IF
               END-IF
               END-IF
P81486         END-IF
               GO TO 225-END.

           IF  (PA52WS-PCT-NEW-VALUE (I1) = "*BLANK")
           AND (PA52WS-PCT-PRE-VALUE (I1) = SPACES)
P81486         IF  (PA52F4-PCT-APPROVAL-FLAG = "L")
P81486              MOVE SPACES           TO PA52WS-PCT-NEW-VALUE (I1)
P81486         ELSE
                    MOVE 147                           TO CRT-ERROR-NBR
P81486         END-IF
               IF (I1 < 13)
P81486             IF  (PA52F4-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F4-PCT-NEW-VALUE-1 (I1)
P81486             ELSE
                        MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1)
                                                       TO CRT-FIELD-NBR
P81486             END-IF
               ELSE
               IF (I1 > 24)
                   COMPUTE I8 = (I1 - 24)
P81486             IF  (PA52F4-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F4-PCT-NEW-VALUE-3 (I8)
P81486             ELSE
                        MOVE PA52F4-PCT-NEW-VALUE-3-FN (I8)
                                                       TO CRT-FIELD-NBR
P81486             END-IF
               ELSE
               IF (I1 > 12)
                   COMPUTE I8 = (I1 - 12)
P81486             IF  (PA52F4-PCT-APPROVAL-FLAG = "L")
P81486                  MOVE SPACES       TO PA52F4-PCT-NEW-VALUE-2 (I8)
P81486             ELSE
                        MOVE PA52F4-PCT-NEW-VALUE-2-FN (I8)
                                                       TO CRT-FIELD-NBR
P81486             END-IF
               END-IF
               END-IF
               END-IF
               GO TO 225-END.

076400 225-END.
076500
J02128******************************************************************
J02128 233-LTM-CHECK.
J02128******************************************************************
J02128
J02128     MOVE SPACES            TO PA52WS-LTM-FLAG.  
J02128
J02128     IF (APL-L-HIRE-DATE    NOT = ZEROES)
J02128     OR (APL-L-ADJ-HIRE-DT  NOT = ZEROES)
J02128     OR (APL-L-ANNIVR-DATE  NOT = ZEROES)
J02128     OR (APL-L-SENIOR-DATE  NOT = ZEROES)
J02128     OR (APL-L-TRUE-MAR-ST  NOT = SPACES)
J02128     OR (APL-L-FIRST-DAY-WK NOT = ZEROES)
J02128     OR (APL-L-WRK-SCH-HIRE NOT = SPACES)
J02128     OR (APL-L-SHIFT-HIRED  NOT = ZEROES)
J02128     OR (APL-L-ORG-UNIT     NOT = ZEROES)
J02128     OR (APL-PROCESS-LEVEL  NOT = SPACES)
J02128     OR (APL-DEPARTMENT     NOT = SPACES)
J02128         MOVE "*"           TO PA52WS-LTM-FLAG
J02128     END-IF.
J02128
J02128 233-END.
423100******************************************************************
423200 400-PROCESS-TRAN.
423300******************************************************************
423400
423500     IF (PA52F4-FC = "A")
423600         PERFORM 410-ADD
423700         THRU    410-END
423800     ELSE
423900     IF (PA52F4-FC = "C")
424000         PERFORM 420-CHANGE
424100         THRU    420-END
424200     ELSE
424300     IF (PA52F4-FC = "D")
424400         PERFORM 440-DELETE
424500         THRU    440-END
424600     ELSE
424700     IF (PA52F4-FC = "I" OR "N" OR "P")
424800         PERFORM 480-INQUIRE
424900         THRU    480-END.
425000
425100 400-END.
425200******************************************************************
425300 410-ADD.
425400******************************************************************
425500
426600     PERFORM 910-AUDIT-BEGIN.
426700     IF (DMS-ABORTED)
426800         GO TO 410-END.
426900
           MOVE PA52F4-LAST-ACTION     TO PA52WS-ACTION-NBR.
           ADD 1                       TO PA52WS-ACTION-NBR.
427100
           IF (PAT-WORKFLOW-FLAG       = "Y")
               PERFORM 430-CREATE-WORKFLOW-TRIGGER
               THRU    430-END.

427200     IF (PA52F4-IMMEDIATE-ACTION = "N")
427300         PERFORM 800-CREATE-PERSACTION
427400         MOVE PA52WS-ACTION-NBR  TO PCT-ACTION-NBR
427500                                    PA52F4-PCT-ACTION-NBR
427600         PERFORM 500-MOVE-DATA
427700         THRU    500-END
427800         PERFORM 8200-STORE-PERSACTION
427900         MOVE ZEROS              TO PA52F4-PAH-OBJ-ID
428000     ELSE
428100         MOVE "PAACT"            TO IFOBIWS-OBJ-TYPE
428200         PERFORM 7000-ASSIGN-OBJ-ID-70
428300         MOVE IFOBIWS-OBJ-ID     TO PA52WS-OBJ-ID
428400                                    PA52F4-PAH-OBJ-ID
428500         PERFORM 600-CREATE-EMPLOYEE
428600         THRU    600-END

428700         PERFORM 800-CREATE-PERSACTHST
428800         MOVE PA52F4-PCT-COMPANY         TO PAH-COMPANY
428900         MOVE "A"                        TO PAH-ACTION-TYPE
429000         MOVE PA52F4-PCT-ACTION-CODE     TO PAH-ACTION-CODE
429100         MOVE PA52WS-ACTION-NBR          TO PAH-ACTION-NBR
429200         MOVE PA52F4-PCT-EFFECT-DATE     TO PAH-EFFECT-DATE
429300         MOVE HREMP-EMPLOYEE             TO PAH-EMPLOYEE
429400         MOVE PA52F4-PCT-ANT-END-DATE    TO PAH-ANT-END-DATE
429500         MOVE PA52F4-PCT-REASON (1)      TO PAH-REASON (1)
429600         MOVE PA52F4-PCT-REASON (2)      TO PAH-REASON (2)
               IF  (PA52F4-USER-ID NOT = SPACES)
                   MOVE PA52F4-USER-ID         TO PAH-USER-ID
               ELSE
                   MOVE CRT-USER-NAME          TO PAH-USER-ID
               END-IF
429800         MOVE WS-SYSTEM-DATE-YMD         TO PAH-DATE-STAMP
P80029         MOVE HHMMSS                     TO PAH-TIME-STAMP
J08104             IF (PA52F4-FC = "A")
J08104                 IF  (PA52F4-USER-ID NOT = SPACES)
J08104                     MOVE PA52F4-USER-ID TO PAH-CREATE-USER
J08104                 ELSE 
J08104                     MOVE CRT-USER-NAME  TO PAH-CREATE-USER
J08104                 END-IF
J08104                 MOVE WS-SYSTEM-DATE-YMD TO PAH-CREATE-DATE
J08104                 MOVE HHMMSS             TO PAH-CREATE-TIME
J08104             END-IF
429900         MOVE PA52WS-OBJ-ID              TO PAH-OBJ-ID
               MOVE 1                          TO PAH-POS-LEVEL
               MOVE PA52F4-PCT-UPDATE-BENEFIT  TO PAH-UPDATE-BENEFIT
               MOVE PA52F4-PCT-UPDATE-REQ-DED  TO PAH-UPDATE-REQ-DED
               MOVE PA52F4-PCT-EDM-EFFECT-DT   TO PAH-EDM-EFFECT-DT
               MOVE ZEROES                     TO PAH-EDM-END-DATE
               MOVE PA52F4-PCT-UPD-ABS-MGMT    TO PAH-UPD-ABS-MGMT
               MOVE "1"                        TO PAH-ACTION-UPD
P80029         MOVE WS-SYSTEM-DATE-YMD         TO PAH-CREATE-DATE
P80029         MOVE HHMMSS                     TO PAH-CREATE-TIME
P80029         MOVE CRT-USER-NAME              TO PAH-CREATE-USER
431500         PERFORM 820-STORE-PERSACTHST

               IF  (PA52F4-PCT-UPDATE-BENEFIT = "Y")
                   MOVE PA52F4-PCT-COMPANY         TO DB-COMPANY
                   MOVE HREMP-EMPLOYEE             TO DB-EMPLOYEE
                   MOVE PA52F4-PCT-EFFECT-DATE     TO DB-EFFECT-DATE
                   MOVE ZEROES                     TO DB-SEQ-NBR
                   PERFORM 840-FIND-BNHSET1
                   PERFORM
                       UNTIL (BNCHANGE-NOTFOUND)
                       ADD 1                       TO DB-SEQ-NBR
                       PERFORM 840-FIND-BNHSET1
                   END-PERFORM
                   PERFORM 800-CREATE-BNCHANGE
                   MOVE DB-COMPANY                 TO BNH-COMPANY
                   MOVE DB-EMPLOYEE                TO BNH-EMPLOYEE
                   MOVE DB-EFFECT-DATE             TO BNH-EFFECT-DATE
                   MOVE DB-SEQ-NBR                 TO BNH-SEQ-NBR
                   MOVE PA52WS-OBJ-ID              TO BNH-ACT-OBJ-ID
                   MOVE "7"                        TO BNH-CHANGE-TYPE
                   MOVE WS-SYSTEM-DATE-YMD         TO BNH-DATE-STAMP
J67329                                                BNH-CREATE-TIME
J67329             MOVE HHMMSS                     TO BNH-TIME-STAMP
J67329                                                BNH-CREATE-TIME                   
                   MOVE "N"                        TO BNH-HOLD-FLAG
J67329             IF (PA52F4-USER-ID NOT = SPACES)
J67329                 MOVE PA52F4-USER-ID         TO BNH-CREATE-USER-ID
J67329                                                BNH-USER-ID   
J67329             ELSE
J67329                 MOVE CRT-USER-NAME          TO BNH-CREATE-USER-ID
J67329                                                BNH-USER-ID     
J67329             END-IF                   
                   PERFORM 820-STORE-BNCHANGE
               END-IF

           END-IF.
431600
431700     MOVE HREMP-EMPLOYEE         TO PA52F4-PCT-EMPLOYEE.
432300
432400     PERFORM 920-AUDIT-END.
432500
      *---------------------------------+
      * Release Work Unit               |
      *---------------------------------+
           
           IF (PAT-WORKFLOW-FLAG       = "Y")
               INITIALIZE CRT-MESSAGE
               INITIALIZE WFAPI-INPUT
               MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT
               INITIALIZE WFAPI-OUTPUT
               PERFORM 1000-WF-RELEASE-SETUP
           END-IF.

           MOVE PA52F4-PCT-EMPLOYEE    TO PA52F4-FIRST-EMPLOYEE.

432800     IF (PA52F4-IMMEDIATE-ACTION = "N")
432900         PERFORM 481-MOVE-TO-SCREEN
433000         THRU    481-END
433100         MOVE CRT-ADD-COMPLETE   TO CRT-MESSAGE
               ADD 1 TO                   PA52F4-NBR-ACTIONS
433200         GO TO 410-END.
433300
           IF  (PA52F4-BYPASS-POPUPS = "Y")
               MOVE CRT-ADD-COMPLETE   TO CRT-MESSAGE
               GO TO 410-END.


433600     MOVE "PA561"                TO CRT-SCREEN-CODE.
P42132     MOVE SPACES                 TO PA52F4-PGM-NAME.
433700     MOVE SPACES                 TO CRT-PASS-FC.
433800     MOVE "A"                    TO CRT-DISPLAY-FC.
433900     MOVE "A"                    TO PA52F4-PT-ACTION-TYPE.
434000     MOVE PA52WS-ACTION-NBR      TO PA52F4-PCT-ACTION-NBR.
           IF (HRWS-GROUP-SIZE-ERROR)
               MOVE 409                TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
           ELSE
434200         MOVE 408                TO CRT-MSG-NBR
           END-IF.

434300     MOVE CRT-MANUAL-CF          TO CRT-REQUEST.
434400
434500 410-END.
434600
434700******************************************************************
434800 420-CHANGE.
434900******************************************************************
435000
435100     PERFORM 910-AUDIT-BEGIN.
435200     IF (DMS-ABORTED)
435300         GO TO 420-END.
435400
           IF  (PA52F4-PCT-EMPLOYEE NOT = PA52F4-FIRST-EMPLOYEE)
           OR  (PA52WS-POSITION     NOT = PA52F4-ORIG-POSITION)
               MOVE PA52F4-PCT-COMPANY         TO DB-COMPANY
               MOVE "A"                        TO DB-ACTION-TYPE
               MOVE PA52F4-PCT-APPLICANT       TO DB-EMPLOYEE
               MOVE PCTSET2-EMPLOYEE           TO WS-DB-BEG-RNG
               PERFORM 850-MODIFY-BEGRNG-PCTSET2
               PERFORM
                   UNTIL (PERSACTION-NOTFOUND)
                   MOVE PA52F4-PCT-EMPLOYEE    TO PCT-PARTICIPNT
                   MOVE PA52WS-POSITION        TO PCT-POSITION
                   PERFORM 8200-STORE-PERSACTION
                   PERFORM 860-MODIFY-NXTRNG-PCTSET2
               END-PERFORM
               MOVE PA52F4-PCT-EMPLOYEE    TO PA52F4-FIRST-EMPLOYEE
               MOVE PA52WS-POSITION        TO PA52F4-ORIG-POSITION
           END-IF.

           IF (PA52F4-PCT-NEW-EFFECT-DATE NOT = ZEROES)
               MOVE PA52F4-PCT-COMPANY         TO DB-COMPANY
               MOVE "A"                        TO DB-ACTION-TYPE
               MOVE PA52F4-PCT-APPLICANT       TO DB-EMPLOYEE
               MOVE PCTSET2-EMPLOYEE           TO WS-DB-BEG-RNG
               PERFORM 850-MODIFY-BEGRNG-PCTSET2
               PERFORM 425-UPDATE-OTHER-ACTIONS
               THRU    425-END
                   UNTIL (PERSACTION-NOTFOUND)

               MOVE PA52F4-PCT-NEW-EFFECT-DATE TO PA52F4-PCT-EFFECT-DATE
               INITIALIZE PA52F4-PCT-NEW-EFFECT-DATE
               MOVE PA52F4-PCT-COMPANY         TO DB-COMPANY
               MOVE "A"                        TO DB-ACTION-TYPE
               MOVE PA52F4-PCT-APPLICANT       TO DB-EMPLOYEE
               MOVE PA52F4-PCT-ACTION-CODE     TO DB-ACTION-CODE
               MOVE PA52F4-PCT-EFFECT-DATE     TO DB-EFFECT-DATE
               MOVE PA52F4-PCT-ACTION-NBR      TO DB-ACTION-NBR
               PERFORM 840-FIND-PCTSET1
           ELSE
435900         MOVE PA52F4-PCT-COMPANY         TO DB-COMPANY
436000         MOVE "A"                        TO DB-ACTION-TYPE
436100         MOVE PA52F4-PCT-APPLICANT       TO DB-EMPLOYEE
436200         MOVE PA52F4-PCT-ACTION-CODE     TO DB-ACTION-CODE
436300         MOVE PA52F4-PCT-EFFECT-DATE     TO DB-EFFECT-DATE
436400         MOVE PA52F4-PCT-ACTION-NBR      TO DB-ACTION-NBR
224000         PERFORM 840-MODIFY-PCTSET1
               PERFORM 8400-AFTER-FIND-PCT
224100         PERFORM 500-MOVE-DATA
224200         THRU    500-END
224300         PERFORM 8200-STORE-PERSACTION
           END-IF.
437000
437100     PERFORM 920-AUDIT-END.
P30119     PERFORM 8400-AFTER-FIND-PCT.
437200     PERFORM 481-MOVE-TO-SCREEN
437300     THRU    481-END.
437400     MOVE CRT-CHG-COMPLETE               TO CRT-MESSAGE.
437500
437600 420-END.
437700
099100******************************************************************
       425-UPDATE-OTHER-ACTIONS.
099100******************************************************************

           PERFORM 830-DELETE-PERSACTION.
           PERFORM 810-RECREATE-PERSACTION.
           MOVE PA52F4-PCT-NEW-EFFECT-DATE     TO PCT-EFFECT-DATE.
           PERFORM 820-STORE-PERSACTION.

           MOVE PCT-COMPANY                    TO DB-COMPANY.
           MOVE 1                              TO DB-EMP-APP.
           MOVE "PA"                           TO DB-CMT-TYPE.
           MOVE PCT-ACTION-CODE                TO DB-ACTION-CODE.
           MOVE PA52F4-PCT-APPLICANT           TO DB-EMPLOYEE.
           INITIALIZE                             DB-JOB-CODE.
           MOVE PCT-ACTION-NBR                 TO DB-LN-NBR.
           MOVE PACSET2-LN-NBR                 TO WS-DB-BEG-RNG.
           PERFORM 850-MODIFY-BEGRNG-PACSET2.
           PERFORM
               UNTIL (PACOMMENTS-NOTFOUND)
               PERFORM 830-DELETE-PACOMMENTS
               PERFORM 810-RECREATE-PACOMMENTS
               MOVE PA52F4-PCT-NEW-EFFECT-DATE TO PAC-DATE
               PERFORM 820-STORE-PACOMMENTS
               PERFORM 860-MODIFY-NXTRNG-PACSET1
           END-PERFORM.

       425-NEXT-PERSACTION.
          PERFORM 860-MODIFY-NXTRNG-PCTSET2.

       425-END.

099100******************************************************************
       430-CREATE-WORKFLOW-TRIGGER.
099100******************************************************************

      *---------------------------------+
      * Check Status                    |
      *---------------------------------+
      **** Check to see if workflow is enabled for a service
           INITIALIZE WFAPI-INPUT
                      WFAPI-OUTPUT.

      **** Check to see if Action Approval Service Definition is setup ****
      **** in WorkFlow System                                          ****
           MOVE PA52F4-PCT-ACTION-CODE TO WFAPI-I-SERVICE.

           MOVE PA52F4-PCT-COMPANY     TO WFAPI-I-CRITERION-1.
      **** CHECK TO SEE IF PROCESS LEVEL CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-PROCESS-LEVEL-DN TO PA52WS-FLD-NBR.
           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.
           IF (FIELD-USED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F4-PCT-NEW-VALUE-1 (I1)
                                       TO WFAPI-I-CRITERION-2
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F4-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO WFAPI-I-CRITERION-2
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F4-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO WFAPI-I-CRITERION-2.

           IF (WFAPI-I-CRITERION-2     = PA52WS-BLANK)
               INITIALIZE WFAPI-I-CRITERION-2.

      **** CHECK TO SEE IF DEPARTMENT CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-DEPARTMENT-DN    TO PA52WS-FLD-NBR.
           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.
           IF (FIELD-USED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F4-PCT-NEW-VALUE-1 (I1)
                                       TO WFAPI-I-CRITERION-3
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F4-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO WFAPI-I-CRITERION-3
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F4-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO WFAPI-I-CRITERION-3.

           IF (WFAPI-I-CRITERION-3     = PA52WS-BLANK)
               INITIALIZE WFAPI-I-CRITERION-3.

           PERFORM 1000-WF-SERVICE.
           IF (WFAPI-O-RETURN-CODE     NOT = ZEROES)
               GO TO 430-END.

      *---------------------------------+
      * Create Work Unit Header         |
      *---------------------------------+
      **** This creates data in WF20 and matches with fields in WF04
      **** Perform CREATE WORK Routine
           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-SERVICE        TO WFAPI-I-SERVICE.
           MOVE WFAPI-O-AGENT          TO WFAPI-I-AGENT.
           MOVE WFAPI-O-PROCEDURE      TO WFAPI-I-PROCEDURE.
           MOVE PA52WS-WORK-PRIORITY   TO WFAPI-I-WORK-PRIORITY.
           MOVE PA52F4-PCT-COMPANY     TO DB-COMPANY.
           MOVE PA52F4-PCT-ACTION-CODE TO DB-ACTION-CODE.
           PERFORM 840-FIND-PATSET1.
           MOVE PAT-DESCRIPTION        TO WFAPI-I-WORK-TITLE.
           MOVE PA52WS-PCTSET1         TO WFAPI-I-OBJECT-NAME.
           MOVE PA52F4-PCT-COMPANY     TO WFAPI-I-KEY-VALUE (1).
           MOVE PA52F4-PT-ACTION-TYPE  TO WFAPI-I-KEY-VALUE (2).
           MOVE PA52F4-PCT-EFFECT-DATE TO WFAPI-I-KEY-VALUE (3).
           MOVE PA52F4-PCT-ACTION-CODE TO WFAPI-I-KEY-VALUE (4).
           MOVE PA52F4-PCT-APPLICANT   TO WFAPI-I-KEY-VALUE (5).
           MOVE PA52WS-ACTION-NBR      TO WFAPI-I-KEY-VALUE (6).

           MOVE 525                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-WORK-CATEGORY.

           MOVE PA52F4-PCT-COMPANY     TO PA52WS-COMPANY.
           MOVE EMP-SUPERVISOR         TO PA52WS-SUPERVISOR.
           MOVE PA52WS-COMP-CODE       TO WFAPI-I-WORK-CAT-VALUE.

           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-CREATE-SETUP.

      *---------------------------------+
      * Create Work Unit Variable       |
      *---------------------------------+
      **** Perform ADD VARIABLE Routine
           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.

      **** COMPANY
           MOVE 501                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (1).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (1).
           MOVE PA52F4-PCT-COMPANY     TO WFAPI-I-VARIABLE-VAL (1).

      **** CHECK TO SEE IF PROCESS LEVEL CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-PROCESS-LEVEL-DN TO PA52WS-FLD-NBR.
           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.

      **** OLD PROCESS LEVEL
           MOVE 502                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (2).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (2).
           MOVE SPACES                 TO WFAPI-I-VARIABLE-VAL (2).

      **** NEW PROCESS LEVEL
           MOVE 503                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (3).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (3).
           IF (FIELD-USED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F4-PCT-NEW-VALUE-1 (I1)
                                       TO WFAPI-I-VARIABLE-VAL (3)
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F4-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO WFAPI-I-VARIABLE-VAL (3)
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F4-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO WFAPI-I-VARIABLE-VAL (3).

           IF (WFAPI-I-VARIABLE-VAL (3) = PA52WS-BLANK)
               INITIALIZE WFAPI-I-VARIABLE-VAL (3)
           ELSE
           IF (WFAPI-I-VARIABLE-VAL (3) = SPACES)
               MOVE WFAPI-I-VARIABLE-VAL (2)
                                       TO WFAPI-I-VARIABLE-VAL (3).

      **** CHECK TO SEE IF DEPARTMENT CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-DEPARTMENT-DN    TO PA52WS-FLD-NBR.
           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.

      **** OLD DEPARTMENT
           MOVE 504                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (4).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (4).
           MOVE SPACES                 TO WFAPI-I-VARIABLE-VAL (4).

      **** NEW DEPARTMENT
           MOVE 505                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (5).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (5).
           IF (FIELD-USED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F4-PCT-NEW-VALUE-1 (I1)
                                       TO WFAPI-I-VARIABLE-VAL (5)
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F4-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO WFAPI-I-VARIABLE-VAL (5)
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F4-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO WFAPI-I-VARIABLE-VAL (5).

           IF (WFAPI-I-VARIABLE-VAL (5) = PA52WS-BLANK)
               INITIALIZE WFAPI-I-VARIABLE-VAL (5)
           ELSE
           IF (WFAPI-I-VARIABLE-VAL (5) = SPACES)
               MOVE WFAPI-I-VARIABLE-VAL (4)
                                       TO WFAPI-I-VARIABLE-VAL (5).

      **** CHECK TO SEE IF SUPERVISOR CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-SUPERVISOR-DN    TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.

      **** SET OLD SUPERVISOR AND NEXT OLD SUPERVISOR ****
           INITIALIZE PA52WS-OLD-SUPER-EMP
                      PA52WS-NEXT-OLD-SUPER-EMP.

      **** OLD SUPERVISOR EMPLOYEE
           MOVE 506                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (6).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (6).
           MOVE PA52WS-OLD-SUPER-EMP   TO WFAPI-I-VARIABLE-VAL (6).

      **** NEXT OLD SUPERVISOR EMPLOYEE
           MOVE 523                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (7).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (7).
           MOVE PA52WS-NEXT-OLD-SUPER-EMP TO WFAPI-I-VARIABLE-VAL (7).

      **** FIND NEW SUPERVISOR AND NEXT NEW SUPERVISOR ****
           INITIALIZE PA52WS-NEW-SUPER-EMP
                      PA52WS-NEXT-NEW-SUPER-EMP.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F4-PCT-NEW-VALUE-1 (I1)      TO DB-CODE
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F4-PCT-NEW-VALUE-2 (I1 - 12) TO DB-CODE
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F4-PCT-NEW-VALUE-3 (I1 - 24) TO DB-CODE
               END-IF
               END-IF
               END-IF
               IF (DB-CODE NOT = SPACES AND "*BLANK")
054800             PERFORM 840-FIND-HSUSET1
                   IF (HRSUPER-FOUND)
                       IF (HSU-EMPLOYEE         = ZEROES)
                           PERFORM 439-FIND-SUPER-EMP
                           THRU    439-END
                       END-IF
                       IF (HSU-EMPLOYEE         NOT = ZEROES)
                           MOVE HSU-EMPLOYEE     TO PA52WS-NEW-SUPER-EMP
                       END-IF
                   END-IF
                   IF (PA52WS-NEW-SUPER-EMP  NOT = ZEROES)
                       MOVE HSU-SUPER-RPTS-TO TO DB-CODE
                       PERFORM 840-FIND-HSUSET1
                       IF (HRSUPER-FOUND)
                           IF (HSU-EMPLOYEE      = ZEROES)
                               PERFORM 439-FIND-SUPER-EMP
                               THRU    439-END
                           END-IF
                           IF (HSU-EMPLOYEE  NOT = ZEROES)
                               MOVE HSU-EMPLOYEE
                                           TO PA52WS-NEXT-NEW-SUPER-EMP.

           IF  (PA52WS-NEW-SUPER-EMP      = ZEROES)
           AND (DB-CODE               NOT = "*BLANK")
               MOVE PA52WS-OLD-SUPER-EMP TO PA52WS-NEW-SUPER-EMP.

           IF  (PA52WS-NEXT-NEW-SUPER-EMP = ZEROES)
           AND (DB-CODE               NOT = "*BLANK")
               MOVE PA52WS-NEXT-OLD-SUPER-EMP
                                         TO PA52WS-NEXT-NEW-SUPER-EMP.

      **** NEW SUPERVISOR EMPLOYEE
           MOVE 507                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (8).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (8).
           MOVE PA52WS-NEW-SUPER-EMP   TO WFAPI-I-VARIABLE-VAL (8).

      **** NEXT NEW SUPERVISOR EMPLOYEE
           MOVE 524                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (9).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (9).
           MOVE PA52WS-NEXT-NEW-SUPER-EMP TO WFAPI-I-VARIABLE-VAL (9).

           MOVE PA52F4-PCT-EMPLOYEE    TO DB-EMPLOYEE.
           PERFORM 840-FIND-EMPSET1.

      **** CHECK TO SEE IF ANY SALARY RELATED FIELDS ARE CHANGED ****
           INITIALIZE PA52WS-PAY-RATE
                      PA52WS-SALARY-CLASS
                      PA52WS-NBR-FTE
                      PA52WS-SCHEDULE
                      PA52WS-PAY-STEP
                      PA52WS-PAY-GRADE
                      PA52WS-ANNUAL-HOURS
                      PA52WS-JOB-CODE.

      **** CHECK ANNUAL-HOURS ****
           MOVE HREMP-ANNUAL-HOURS-DN  TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F4-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F4-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F4-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-ALPHA
               END-IF
               END-IF
               END-IF
               PERFORM 430A-CONVERT-NUMBER
               THRU    430A-END
               MOVE PA52WS-NUMBER      TO PA52WS-ANNUAL-HOURS.

      **** CHECK PAY RATE ****
           MOVE HREMP-PAY-RATE-DN      TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F4-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F4-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F4-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-ALPHA
               END-IF
               END-IF
               END-IF
               PERFORM 430A-CONVERT-NUMBER
               THRU    430A-END
               MOVE PA52WS-NUMBER      TO PA52WS-PAY-RATE.

      **** CHECK SALARY CLASS ****
           MOVE HREMP-SALARY-CLASS-DN  TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F4-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-SALARY-CLASS
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F4-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-SALARY-CLASS
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F4-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-SALARY-CLASS.

      **** CHECK NBR FTE ****
           MOVE HREMP-NBR-FTE-DN       TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F4-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F4-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F4-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-ALPHA
               END-IF
               END-IF
               END-IF
               PERFORM 430A-CONVERT-NUMBER
               THRU    430A-END
               MOVE PA52WS-NUMBER      TO PA52WS-NBR-FTE.

      **** CHECK SCHEDULE ****
           MOVE HREMP-SCHEDULE-DN      TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F4-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-SCHEDULE
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F4-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-SCHEDULE
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F4-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-SCHEDULE.

      **** CHECK PAY STEP ****
           MOVE HREMP-PAY-STEP-DN      TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F4-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F4-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-ALPHA
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F4-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-ALPHA
               END-IF
               END-IF
               END-IF
               PERFORM 430A-CONVERT-NUMBER
               THRU    430A-END
               MOVE PA52WS-NUMBER      TO PA52WS-PAY-STEP.

      **** CHECK PAY GRADE ****
           MOVE HREMP-PAY-GRADE-DN     TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F4-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-PAY-GRADE
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F4-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-PAY-GRADE
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F4-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-PAY-GRADE.

      **** CHECK JOB CODE
           MOVE HREMP-JOB-CODE-DN      TO PA52WS-FLD-NBR.
           PERFORM 431-CHECK-IF-CHANGED
           THRU    431-END.
           IF (FIELD-CHANGED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F4-PCT-NEW-VALUE-1 (I1)
                                       TO PA52WS-JOB-CODE
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F4-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO PA52WS-JOB-CODE
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F4-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO PA52WS-JOB-CODE.

           IF (PA52WS-PAY-RATE         NOT = ZEROES)
           OR (PA52WS-SALARY-CLASS     NOT = SPACES)
           OR (PA52WS-NBR-FTE          NOT = ZEROES)
           OR (PA52WS-SCHEDULE         NOT = SPACES)
           OR (PA52WS-PAY-STEP         NOT = ZEROES)
           OR (PA52WS-PAY-GRADE        NOT = SPACES)
           OR (PA52WS-JOB-CODE         NOT = SPACES)
           OR (PA52WS-ANNUAL-HOURS     NOT = ZEROES)
               SET FIELD-CHANGED       TO TRUE.

      **** OLD SALARY
           MOVE 508                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (10).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (10).
           MOVE ZEROES                 TO PA52WS-NUM-15.
           MOVE PA52WS-ALPHA-15        TO WFAPI-I-VARIABLE-VAL (10).

           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-ADD-VAR-SETUP.

           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.

      **** NEW SALARY
           PERFORM 436-CALC-NEW-SALARY
           THRU    436-END.
           MOVE 509                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (1).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (1).
           MOVE PA52WS-NEW-SALARY      TO PA52WS-NUM-15.
           MOVE PA52WS-ALPHA-15        TO WFAPI-I-VARIABLE-VAL (1).

      **** ACTION CODE
           MOVE 510                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (2).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (2).
           MOVE PA52F4-PCT-ACTION-CODE TO WFAPI-I-VARIABLE-VAL (2).

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 10)

               IF (WFAPI-I-VARIABLE-VAL (I1) = PA52WS-BLANK)
                   INITIALIZE WFAPI-I-VARIABLE-VAL (I1)
               END-IF
           END-PERFORM.

      **** REASON (1)
           MOVE 511                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (3).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (3).
           MOVE PA52F4-PCT-REASON (1)  TO WFAPI-I-VARIABLE-VAL (3).

      **** REASON (2)
           MOVE 512                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (4).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (4).
           MOVE PA52F4-PCT-REASON (2)  TO WFAPI-I-VARIABLE-VAL (4).

      **** CHECK TO SEE IF EMPLOYEE STATUS CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-EMP-STATUS-DN    TO PA52WS-FLD-NBR.
           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.

      **** OLD EMPLOYEE STATUS
           MOVE 513                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (5).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (5).
           MOVE SPACES                 TO WFAPI-I-VARIABLE-VAL (5).

      **** NEW EMPLOYEE STATUS
           MOVE 514                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (6).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (6).
           IF (FIELD-USED)
               IF (I1 >= 1) AND (I1 <= 12)
                   MOVE PA52F4-PCT-NEW-VALUE-1 (I1)
                                       TO WFAPI-I-VARIABLE-VAL (6)
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   MOVE PA52F4-PCT-NEW-VALUE-2 (I1 - 12)
                                       TO WFAPI-I-VARIABLE-VAL (6)
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   MOVE PA52F4-PCT-NEW-VALUE-3 (I1 - 24)
                                       TO WFAPI-I-VARIABLE-VAL (6).

           IF (WFAPI-I-VARIABLE-VAL (6) = PA52WS-BLANK)
               INITIALIZE WFAPI-I-VARIABLE-VAL (6)
           ELSE
           IF (WFAPI-I-VARIABLE-VAL (6) = SPACES)
               MOVE WFAPI-I-VARIABLE-VAL (5)
                                       TO WFAPI-I-VARIABLE-VAL (6).

      **** OVER BUDGET
           MOVE 515                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (7).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (7).
           IF (HREMP-VAR-FTE           < ZEROES)
           OR (HREMP-VAR-HDCNT         < ZEROES)
               MOVE "Y"                TO WFAPI-I-VARIABLE-VAL (7)
           ELSE
               MOVE "N"                TO WFAPI-I-VARIABLE-VAL (7).

      **** PERCENT CHANGE
           MOVE 516                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (8).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (8).
           MOVE ZEROES                 TO PA52WS-NUM-15.
           MOVE PA52WS-ALPHA-15        TO WFAPI-I-VARIABLE-VAL (8).

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 6)

               IF (WFAPI-I-VARIABLE-VAL (I1) = PA52WS-BLANK)
                   INITIALIZE WFAPI-I-VARIABLE-VAL (I1)
               END-IF
           END-PERFORM.

      **** ACTION TYPE
           MOVE 519                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (9).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (9).
           MOVE PA52F4-PT-ACTION-TYPE  TO WFAPI-I-VARIABLE-VAL (9).

      **** EFFECT DATE
           MOVE 520                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (10).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (10).
           MOVE PA52F4-PCT-EFFECT-DATE TO WFAPI-I-VARIABLE-VAL (10).

           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-ADD-VAR-SETUP.

           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.

      **** APPLICANT
           MOVE 533                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (1).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (1).
           MOVE PA52F4-PCT-APPLICANT   TO WFAPI-I-VARIABLE-VAL (1).

      **** ACTION NBR
           MOVE 522                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (2).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (2).
           MOVE PA52WS-ACTION-NBR      TO WFAPI-I-VARIABLE-VAL (2).

      **** MAIL-SUBJECT VARIABLE IS SET SO THAT PEOPLE CAN GET E-MAIL
025320     MOVE WFAPI-MAIL-SUBJECT     TO WFAPI-I-VARIABLE-NAME (3).
           MOVE 518                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-VAL  (3).
025430     MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (3).
025440
           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-ADD-VAR-SETUP.

      *---------------------------------+
      * Create Work Unit Message Header |
      *---------------------------------+
      **** Perform ADD MESSAGE HEADER Routine
           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.

           MOVE 517                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-MESSAGE-ID.
           MOVE CRT-USER-NAME          TO WFAPI-I-USER.

           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-ADD-MSGHDR-SETUP.

      *---------------------------------+
      * Create Work Unit Message Detail |
      *---------------------------------+
      **** Perform ADD MESSAGE DETAIL Routine
           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.

           MOVE 517                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-MESSAGE-ID.

           MOVE 518                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-MESSAGE.

           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-ADD-MSGDTL-SETUP.

      *---------------------------------+
      * Create Work Unit Folder         |
      *---------------------------------+
      **** This creates data in WF21 and matches with fields in WF02
      **** Perform ADD FOLDER Routine
           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.

           MOVE CRT-SCREEN-NAME        TO PA52WS-SCREEN-NAME.
           MOVE CRT-SCREEN-NUMBER      TO PA52WS-SCREEN-NUMBER.
           MOVE PA52WS-SCREEN-CODE     TO WFAPI-I-FORM.
           MOVE PA52WS-FORM            TO WFAPI-I-DOCUMENT-ID.
           MOVE PA52F4-PCT-COMPANY     TO WFAPI-I-KEY-VALUE (1).
           MOVE PA52F4-PCT-APPLICANT   TO WFAPI-I-KEY-VALUE (2).
           MOVE PA52F4-PCT-ACTION-CODE TO WFAPI-I-KEY-VALUE (3).
           MOVE PA52F4-PCT-EFFECT-DATE TO WFAPI-I-KEY-VALUE (4).
           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-ADD-FOLDER-SETUP.

       430-END.


      *---------------------------------+
      * Convert Alpha to Number         |
      *---------------------------------+
       430A-CONVERT-NUMBER.
           MOVE ZEROES                 TO PA52WS-NUMBER.
           IF (PA52WS-ALPHA = "*BLANK")
               GO TO 430A-END.

           INITIALIZE I2
                      I3.
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 15)
               IF  (PA52WS-ALPHA (I1:1) = ".")
                   MOVE I1             TO I2
               END-IF
               IF  (PA52WS-ALPHA (I1:1) = SPACE)
               AND (I3 = ZEROS)
                   COMPUTE I3 = I1 - 1
               END-IF
           END-PERFORM.
           IF  (I2 = ZEROES)
               COMPUTE I2 = I3 + 1
           END-IF.
           COMPUTE PA52WS-WHOLE-START = 1.
           COMPUTE PA52WS-WHOLE-END   = I2 - 1.
           COMPUTE PA52WS-FRACT-START = I2 + 1.
           COMPUTE PA52WS-FRACT-END   = I3.
           MOVE 10                     TO I2.
           PERFORM
               VARYING I1 FROM PA52WS-WHOLE-END BY NEGATIVE-ONE
               UNTIL  (I1 < PA52WS-WHOLE-START)
               MOVE PA52WS-ALPHA (I1:1)    TO PA52WS-NUMBER-A (I2:1)
               SUBTRACT 1                  FROM I2
           END-PERFORM.
           MOVE 11                     TO I2.
           PERFORM
               VARYING I1 FROM PA52WS-FRACT-START BY 1
               UNTIL  (I1 > PA52WS-FRACT-END)
               MOVE PA52WS-ALPHA (I1:1)    TO PA52WS-NUMBER-A (I2:1)
               ADD 1                   TO I2
           END-PERFORM.

       430A-END.

043300******************************************************************
       431-CHECK-IF-CHANGED.
043300******************************************************************

           SET FIELD-NOTCHANGED        TO TRUE.

           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.
           IF (FIELD-USED)
               IF (I1 >= 1) AND (I1 <= 12)
                   IF (PA52F4-PCT-NEW-VALUE-1 (I1)      NOT = SPACES)
                       SET FIELD-CHANGED TO TRUE
                   END-IF
               ELSE
               IF (I1 >= 13) AND (I1 <= 24)
                   IF (PA52F4-PCT-NEW-VALUE-2 (I1 - 12) NOT = SPACES)
                       SET FIELD-CHANGED TO TRUE
                   END-IF
               ELSE
               IF (I1 >= 25) AND (I1 <= 36)
                   IF (PA52F4-PCT-NEW-VALUE-3 (I1 - 24) NOT = SPACES)
                       SET FIELD-CHANGED TO TRUE.

       431-END.

099100******************************************************************
       432-FIND-FIELD-USAGE.
099100******************************************************************

           SET FIELD-NOTUSED               TO TRUE.
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36)
               OR     (PAT-FLD-NBR (I1)    = ZEROES)
               OR     (PAT-FLD-NBR (I1)    = PA52WS-FLD-NBR)

               CONTINUE
           END-PERFORM.

           IF  (I1                         <= 36)
           AND (PAT-FLD-NBR (I1)           NOT = ZEROES)
               SET FIELD-USED              TO TRUE.

       432-END.

043300******************************************************************
043400 436-CALC-NEW-SALARY.
043500******************************************************************
043600
           INITIALIZE PA52WS-NEW-SALARY.

043700     MOVE PA52WS-NBR-FTE             TO PA52WS-NBR-FTE-WF.
           MOVE PA52WS-ANNUAL-HOURS        TO PA52WS-ANNUAL-HOURS-WF.
043800
           IF  (PA52WS-ANNUAL-HOURS-WF = ZEROES)
043900*         MOVE EMP-COMPANY            TO DB-COMPANY
043900         MOVE PA52F4-PCT-COMPANY     TO DB-COMPANY
044000         MOVE PA52WS-JOB-CODE        TO DB-JOB-CODE
044100         PERFORM 840-FIND-JBCSET1
044200         IF  (JOBCODE-FOUND)
044300             MOVE JBC-ANNUAL-HOURS   TO PA52WS-ANNUAL-HOURS-WF
               END-IF
           END-IF.
044400
044500     IF (PA52WS-ANNUAL-HOURS-WF = ZEROES)
044600         MOVE PA52F4-PCT-COMPANY     TO DB-COMPANY
044700         INITIALIZE DB-PROCESS-LEVEL
044800         PERFORM 840-FIND-PRSSET1
044900         MOVE PRS-ANNUAL-HOURS       TO PA52WS-ANNUAL-HOURS-WF
           END-IF.
045000
045100     IF (PA52WS-ANNUAL-HOURS-WF = ZEROES)
045200         MOVE 2080                   TO PA52WS-ANNUAL-HOURS-WF.
045300
045400     IF (PA52WS-NBR-FTE-WF = ZEROES)
045500         MOVE 1                      TO PA52WS-NBR-FTE-WF.
045600
045700     IF (PA52WS-PAY-STEP             NOT = ZEROES)
045800         PERFORM 437-CALC-FROM-STEP-GRADE
045900         THRU    437-END
046000     ELSE
046100         PERFORM 438-CALC-FROM-HR11
046200         THRU    438-END.
046300
046400 436-END.
046500
046600******************************************************************
046700 437-CALC-FROM-STEP-GRADE.
046800******************************************************************
046900
047000     MOVE PA52F4-PCT-COMPANY         TO DB-COMPANY.
J33429     MOVE "S"                        TO DB-INDICATOR.
047100     MOVE PA52WS-SCHEDULE            TO DB-SCHEDULE.
047200     MOVE PA52F4-PCT-EFFECT-DATE     TO DB-EFFECT-DATE.
047300     PERFORM 850-FIND-NLT-SGHSET2.
047400
047500     MOVE PA52F4-PCT-COMPANY         TO DB-COMPANY.
047600     MOVE PA52WS-SCHEDULE            TO DB-SCHEDULE.
047700     MOVE PA52WS-PAY-GRADE           TO DB-PAY-GRADE.
047800     MOVE PA52WS-PAY-STEP            TO DB-PAY-STEP.
047900     MOVE SGH-EFFECT-DATE            TO DB-EFFECT-DATE.
048000     PERFORM 840-FIND-SGDSET3.
048100
048200     IF (HREMP-SALARY-CLASS          = "H")
048300         COMPUTE PA52WS-NEW-SALARY ROUNDED
                                           = SGD-PAY-RATE
048400                                     * PA52WS-ANNUAL-HOURS-WF
048500                                     * PA52WS-NBR-FTE-WF
048600     ELSE
048700         MOVE SGD-PAY-RATE           TO PA52WS-NEW-SALARY.
048800
048900 437-END.
049000
049100******************************************************************
049200 438-CALC-FROM-HR11.
049300******************************************************************
049400
049500     IF (HREMP-SALARY-CLASS          = "H")
049600         COMPUTE PA52WS-NEW-SALARY ROUNDED
                                           = PA52WS-PAY-RATE
049700                                     * PA52WS-ANNUAL-HOURS-WF
049800                                     * PA52WS-NBR-FTE-WF
049900     ELSE
050000         MOVE PA52WS-PAY-RATE        TO PA52WS-NEW-SALARY.
050100
050200 438-END.
050300
099100******************************************************************
       439-FIND-SUPER-EMP.
099100******************************************************************

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1               > PA52WS-NBR-OF-SEARCH)
               OR     (HSU-EMPLOYEE     NOT = ZEROES)
               OR     (HSU-SUPER-RPTS-TO    = SPACES)

               MOVE HSU-SUPER-RPTS-TO  TO DB-CODE
               PERFORM 840-FIND-HSUSET1

           END-PERFORM.

       439-END.

437800******************************************************************
437900 440-DELETE.
438000******************************************************************
438100
438200     PERFORM 910-AUDIT-BEGIN.
438300     IF (DMS-ABORTED)
438400         GO TO 440-END.
438500
438600     MOVE PA52F4-PCT-COMPANY     TO DB-COMPANY.
438700     MOVE "A"                    TO DB-ACTION-TYPE.
438800     MOVE PA52F4-PCT-ACTION-CODE TO DB-ACTION-CODE.
438900     MOVE PA52F4-PCT-EFFECT-DATE TO DB-EFFECT-DATE.
439000     MOVE PA52F4-PCT-APPLICANT   TO DB-EMPLOYEE.
439100     MOVE PA52F4-PCT-ACTION-NBR  TO DB-ACTION-NBR.
439200
439300     PERFORM 840-MODIFY-PCTSET1.
439400     PERFORM 830-DELETE-PERSACTION.
439500
439600     MOVE 1                      TO DB-EMP-APP.
439700     MOVE "PA"                   TO DB-CMT-TYPE.
439800     MOVE PA52F4-PCT-ACTION-CODE TO DB-ACTION-CODE.
439900     MOVE PA52F4-PCT-EFFECT-DATE TO DB-DATE.
440000     MOVE PA52F4-PCT-APPLICANT   TO DB-EMPLOYEE.
440100     MOVE SPACES                 TO DB-JOB-CODE.
440200     MOVE PA52F4-PCT-ACTION-NBR  TO DB-LN-NBR.
440300     MOVE PACSET2-LN-NBR         TO WS-DB-BEG-RNG.
440400     PERFORM 830-DELETERNG-PACSET2.
440500
P63835     IF (PAT-WORKFLOW-FLAG = "Y")
P63835         INITIALIZE WFAPI-INPUT
P63835                    WFAPI-OUTPUT
P63835
P63835         MOVE "PCTSET1"              TO WFAPI-I-OBJECT-NAME
P63835         MOVE PA52F4-PCT-COMPANY     TO WFAPI-I-KEY-VALUE (1)
P63835         MOVE "A"                    TO WFAPI-I-KEY-VALUE (2)
P63835         MOVE PA52F4-PCT-EFFECT-DATE TO WFAPI-I-KEY-VALUE (3)
P63835         MOVE PA52F4-PCT-ACTION-CODE TO WFAPI-I-KEY-VALUE (4)
J31809*        MOVE PA52F4-PCT-EMPLOYEE    TO WFAPI-I-KEY-VALUE (5)
J31809         MOVE PA52F4-PCT-APPLICANT   TO WFAPI-I-KEY-VALUE (5)
P63835         MOVE PA52F4-PCT-ACTION-NBR  TO WFAPI-I-KEY-VALUE (6)
P63835         PERFORM 1000-WF-CANCEL-SETUP
P63835     END-IF.
           IF  (PA52F4-PCT-ACTION-NBR = 1)
               INITIALIZE PA52F4-LAST-ACTION.

           IF (PA52F4-IMMEDIATE-ACTION = "N")
               SUBTRACT 1 FROM            PA52F4-NBR-ACTIONS.

440600     MOVE CRT-RECS-DELETED       TO CRT-MESSAGE.
440700     PERFORM 920-AUDIT-END.
440800
440900 440-END.
441000
441100******************************************************************
441200 480-INQUIRE.
441300******************************************************************
441400
           INITIALIZE CRT-FIELD-NBR.
104600     INITIALIZE PA52F4-DETAIL-GROUP-1.
104700     INITIALIZE PA52F4-DETAIL-GROUP-2.
104800     INITIALIZE PA52F4-DETAIL-GROUP-3.
           INITIALIZE PA52F4-DETAIL-GROUP-1A.
           INITIALIZE PA52F4-DETAIL-GROUP-2A.
           INITIALIZE PA52F4-DETAIL-GROUP-3A.
           INITIALIZE PA52F4-ASSIGN-EMPLOYEE-NBR
                      PA52F4-PCT-EMPLOYEE
                      PA52F4-EMP-AUTO-ASSIGNED
                      PA52F4-PCT-POSITION
                      PA52F4-POS-DESCRIPTION
                      PA52F4-PREASSIGNED-DONE
                      PA52F4-ON-HOLD-MSG
                      PA52F4-PCT-REASON (1)
                      PA52F4-PCT-REASON (2).

           INITIALIZE                     PA52WS-FIRST-ACT-NBR
                                          PA52WS-LAST-ACT-NBR.
444700
447200     MOVE APL-LAST-NAME          TO HRWS-LAST-NAME.
447300     MOVE APL-FIRST-NAME         TO HRWS-FIRST-NAME.
447400     MOVE APL-MIDDLE-INIT        TO HRWS-MIDDLE-INIT.
           MOVE APL-NAME-SUFFIX        TO HRWS-NAME-SUFFIX.
           MOVE APL-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE.
447500     PERFORM 750-HR-FORMAT-NAME.
447600     MOVE HRWS-FORMAT-NAME       TO PA52F4-APL-NAME.
447700     MOVE APL-FICA-NBR           TO PA52F4-APL-FICA-NBR.
448000     IF (PA52F4-PJR-REQUISITION NOT = ZEROES)
448100         MOVE PA52F4-PCT-COMPANY TO DB-COMPANY
448200         MOVE PA52F4-PJR-REQUISITION
448300                                 TO DB-REQUISITION
448400         PERFORM 840-FIND-PJRSET1
448500         IF (PAJOBREQ-FOUND)
448600             MOVE PJR-DESCRIPTION
448700                                 TO PA52F4-PJR-DESCRIPTION.

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36)

               MOVE PAT-FLD-NBR (I1)       TO PA52WS-PAT-FIELD-NBR (I1)
                                              DB-FLD-NBR
               INITIALIZE CRT-PHRASE-XLT
               IF (PAT-FLD-NBR (I1)        NOT = ZEROES)
                   PERFORM 840-FIND-PADSET1
                   MOVE PAD-ITEM-NAME      TO CRT-PHRASE
                   MOVE PA52WS-PHRASE-SIZE TO CRT-PHRASE-SIZE
                   MOVE "N"                TO CRT-PUT-DOTS
                   PERFORM 900-GET-PHRASE-XLT
               END-IF
               IF (I1 < 13)
                   MOVE "V"                TO PA52F4-SELECT-BTN-1 (I1)
                   MOVE CRT-PHRASE-XLT     TO
                                           PA52F4-PAT-ITEM-NAME-1 (I1)
                   MOVE PAT-FLD-NBR (I1)   TO PA52F4-PAT-FLD-NBR-1 (I1)
                                              PA52F4-PAT-FLD-NBR-1A (I1)
               ELSE
               IF (I1 > 24)
                   COMPUTE I8 = (I1 - 24)
                   MOVE CRT-PHRASE-XLT     TO
                                           PA52F4-PAT-ITEM-NAME-3 (I8)
                   MOVE PAT-FLD-NBR (I1)   TO
                                           PA52F4-PAT-FLD-NBR-3 (I8)
               ELSE
               IF (I1 > 12)
                   COMPUTE I8 = (I1 - 12)
                   MOVE "V"                TO PA52F4-SELECT-BTN-2 (I8)
                   MOVE CRT-PHRASE-XLT     TO
                                           PA52F4-PAT-ITEM-NAME-2 (I8)
                   MOVE PAT-FLD-NBR (I1)   TO
                                           PA52F4-PAT-FLD-NBR-2 (I8)
                                           PA52F4-PAT-FLD-NBR-2A (I8)
               END-IF
               END-IF
               END-IF
           END-PERFORM.

J73281     MOVE PA52F4-PCT-COMPANY         TO PAAPL-COMPANY.
J73281     MOVE ZEROES                     TO PAAPL-EMPLOYEE.
J73281     MOVE PA52F4-PCT-APPLICANT       TO PAAPL-APPLICANT.
J73281     PERFORM 7010-GET-LEM-DATA.

110200     PERFORM 484-MOVE-CURRENT-DATA
110300     THRU    484-END
110400         VARYING I1 FROM 1 BY 1
110500         UNTIL  (I1 > 36).

           INITIALIZE PA52F4-NBR-ACTIONS
                      PA52F4-LAST-ACTION
                      PA52F4-FIRST-EMPLOYEE
                      PA52F4-FIRST-ACTION-CODE.
           INITIALIZE PA52F4-PROCESS-LEVEL
                      PA52F4-DEPARTMENT
                      PA52F4-JOB-CODE
P70173                PA52F4-TIPPED
                      PA52F4-SOC-NBR
                      PA52F4-ETHNICITY
P47308                PA52F4-TAX-PROVINCE.
P56067     INITIALIZE PA52F4-HM-DIST-CO
P56067                PA52F4-HM-ACCT-UNIT.
P51135     INITIALIZE PA52F4-HM-ACCOUNT
P74764                PA52F4-HM-SUB-ACCT
P51135                PA52F4-ACTIVITY
P51135                PA52F4-ACCT-CATEGORY.
           INITIALIZE CRT-MSG-NBR.
           INITIALIZE PA52F4-MORE-MSG.
           MOVE PA52F4-PCT-COMPANY     TO DB-COMPANY.
           MOVE "A"                    TO DB-ACTION-TYPE.
           MOVE PA52F4-PCT-APPLICANT   TO DB-EMPLOYEE.
           MOVE PCTSET2-EMPLOYEE       TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PCTSET2.
           PERFORM
               UNTIL (PERSACTION-NOTFOUND)
               ADD 1                   TO PA52F4-NBR-ACTIONS
               IF  (PCT-ACTION-NBR > PA52F4-LAST-ACTION)
                   MOVE PCT-ACTION-NBR TO PA52F4-LAST-ACTION
               END-IF
J60711*        IF  (PCT-HOLD-FLAG      = "Y")
J60711*        AND (PA52F4-ON-HOLD-MSG = SPACES)
J60711*            MOVE 175                TO CRT-MSG-NBR
J60711*            PERFORM 790-GET-MSG
J60711*            MOVE CRT-MESSAGE        TO PA52F4-ON-HOLD-MSG
J60711*        END-IF
               IF  (PCT-ACTION-NBR = 1)
                   MOVE "N"                TO PA52F4-IMMEDIATE-ACTION
                   MOVE PCT-PARTICIPNT     TO PA52F4-PCT-EMPLOYEE
                                              PA52F4-FIRST-EMPLOYEE
                   MOVE PCT-ACTION-CODE    TO PA52F4-FIRST-ACTION-CODE
                   MOVE PCT-HOLD-FLAG      TO PA52F4-FIRST-HOLD-FLAG

                   IF  (PCT-ACTION-CODE NOT = PA52F4-PCT-ACTION-CODE)
                       MOVE "1"            TO PA52F4-EMP-AUTO-ASSIGNED
                   ELSE
                       IF  (PRS-AUTO-EMPLOYEE = "Y")
                       AND (PA52F4-PCT-EMPLOYEE NOT = ZEROES)
                           MOVE "Y"        TO PA52F4-EMP-AUTO-ASSIGNED
                       END-IF
                   END-IF
               END-IF
               IF  (PCT-ACTION-CODE NOT = PA52F4-PCT-ACTION-CODE)
                   PERFORM
                       VARYING I1 FROM 1 BY 1
                       UNTIL  (I1 > 36)
                       IF  (PCT-FLD-NBR (I1) = HREMP-POSITION-DN)
                       AND (PCT-NEW-VALUE (I1) NOT = SPACES)
                           MOVE PCT-NEW-VALUE (I1)
                                               TO PA52F4-PCT-POSITION
                                                  DB-POSITION
                           MOVE POSSET2-POSITION   TO WS-DB-BEG-RNG
                           PERFORM 850-FIND-BEGRNG-POSSET2
                           IF  (PAPOSITION-FOUND)
                               MOVE POS-DESCRIPTION
                                               TO PA52F4-POS-DESCRIPTION
                           END-IF
                       END-IF
                       IF  (PCT-FLD-NBR (I1) = HREMP-PROCESS-LEVEL-DN)
                       AND (PCT-NEW-VALUE (I1) NOT = SPACES)
                           MOVE PCT-NEW-VALUE (I1)
                                               TO PA52F4-PROCESS-LEVEL
                       END-IF
                       IF  (PCT-FLD-NBR (I1) = HREMP-DEPARTMENT-DN)
                       AND (PCT-NEW-VALUE (I1) NOT = SPACES)
                           MOVE PCT-NEW-VALUE (I1) TO PA52F4-DEPARTMENT
                       END-IF
                       IF  (PCT-FLD-NBR (I1) = HREMP-JOB-CODE-DN)
                       AND (PCT-NEW-VALUE (I1) NOT = SPACES)
                           MOVE PCT-NEW-VALUE (I1) TO PA52F4-JOB-CODE
                       END-IF
P70173                 IF  (PCT-FLD-NBR (I1) = HREMP-TIPPED-DN)
P70173                 AND (PCT-NEW-VALUE (I1) NOT = SPACES)
P70173                     MOVE PCT-NEW-VALUE (I1) TO PA52F4-TIPPED
P70173                 END-IF
                       IF  (PCT-FLD-NBR (I1) = HREMP-EMP-STATUS-DN)
                       AND (PCT-NEW-VALUE (I1) NOT = SPACES)
                           MOVE PCT-NEW-VALUE (I1) TO PA52F4-EMP-STATUS
                       END-IF
                       IF  (PCT-FLD-NBR (I1) = HREMP-FICA-NBR-DN)
                       AND (PCT-NEW-VALUE (I1) NOT = SPACES)
                           MOVE PCT-NEW-VALUE (I1) TO PA52F4-SOC-NBR
                       END-IF
                       IF  (PCT-FLD-NBR (I1) = HRPEM-EEO-CLASS-DN)
                       AND (PCT-NEW-VALUE (I1) NOT = SPACES)
                           MOVE PCT-NEW-VALUE (I1) TO PA52F4-ETHNICITY
                       END-IF
P47308                 IF  (PCT-FLD-NBR (I1) = HREMP-TAX-PROVINCE-DN)
P47308                 AND (PCT-NEW-VALUE (I1) NOT = SPACES)
P47308                     MOVE PCT-NEW-VALUE (I1) TO 
P47308                                             PA52F4-TAX-PROVINCE
P47308                 END-IF
P56067                 IF  (PCT-FLD-NBR (I1) = HREMP-HM-DIST-CO-DN)
P56067                 AND (PCT-NEW-VALUE (I1) NOT = SPACES)
P56067                     MOVE PCT-NEW-VALUE (I1) TO PAPCT-FIELD
P56067                     PERFORM 5200-CONVERT-NUMERIC-FIELD
P56067                     MOVE PAPCT-NUMERIC      TO PA52F4-HM-DIST-CO
P56067                 END-IF
P56067                 IF  (PCT-FLD-NBR (I1) = HREMP-HM-ACCT-UNIT-DN)
P56067                 AND (PCT-NEW-VALUE (I1) NOT = SPACES)
P56067                     MOVE PCT-NEW-VALUE (I1) TO 
P56067                                             PA52F4-HM-ACCT-UNIT
P56067                 END-IF
P51135                 IF  (PCT-FLD-NBR (I1) = HREMP-HM-ACCOUNT-DN)
P51135                 AND (PCT-NEW-VALUE (I1) NOT = SPACES)
P51135                     MOVE PCT-NEW-VALUE (I1) TO PAPCT-FIELD
P51135                     PERFORM 5200-CONVERT-NUMERIC-FIELD
P51135                     MOVE PAPCT-NUMERIC      TO PA52F4-HM-ACCOUNT
P51135                 END-IF
P74764                 IF  (PCT-FLD-NBR (I1) = HREMP-HM-SUB-ACCT-DN)
P74764                 AND (PCT-NEW-VALUE (I1) NOT = SPACES)
P74764                     MOVE PCT-NEW-VALUE (I1) TO PAPCT-FIELD
P74764                     PERFORM 5200-CONVERT-NUMERIC-FIELD
P74764                     MOVE PAPCT-NUMERIC      TO PA52F4-HM-SUB-ACCT
P74764                 END-IF
P51135                 IF  (PCT-FLD-NBR (I1) = HREMP-ACTIVITY-DN)
P51135                 AND (PCT-NEW-VALUE (I1) NOT = SPACES)
P51135                     MOVE PCT-NEW-VALUE (I1) TO 
P51135                                             PA52F4-ACTIVITY
P51135                 END-IF
P51135                 IF  (PCT-FLD-NBR (I1) = HREMP-ACCT-CATEGORY-DN)
P51135                 AND (PCT-NEW-VALUE (I1) NOT = SPACES)
P51135                     MOVE PCT-NEW-VALUE (I1) TO 
P51135                                             PA52F4-ACCT-CATEGORY
P51135                 END-IF
                   END-PERFORM
               ELSE
                   MOVE PCT-POSITION       TO PA52F4-ORIG-POSITION
               END-IF

               IF  (PCT-ACTION-CODE NOT = PA52F4-PCT-ACTION-CODE)
                   MOVE 171                TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
                   MOVE CRT-MESSAGE        TO PA52F4-MORE-MSG
                   INITIALIZE CRT-MESSAGE
               END-IF

               PERFORM 860-FIND-NXTRNG-PCTSET2
           END-PERFORM.

P49898     MOVE PA52F4-PCT-COMPANY     TO PA52F4-ORIG-COMPANY.
P49898     MOVE PA52F4-PCT-APPLICANT   TO PA52F4-ORIG-APPLICANT.
P49898     MOVE PA52F4-PCT-ACTION-CODE TO PA52F4-ORIG-ACTION-CODE.
P49898     MOVE PA52F4-PJR-REQUISITION TO PA52F4-ORIG-REQUISITION.
P49898     MOVE PA52F4-PCT-EFFECT-DATE TO PA52F4-ORIG-EFFECT-DATE.
P49898
           MOVE PA52F4-PCT-COMPANY      TO DB-COMPANY.
           MOVE "A"                     TO DB-ACTION-TYPE.
           MOVE PA52F4-PCT-APPLICANT    TO DB-EMPLOYEE.
           MOVE PA52F4-PCT-ACTION-CODE  TO DB-ACTION-CODE.
           MOVE PA52F4-PCT-EFFECT-DATE  TO DB-EFFECT-DATE.
           MOVE PA52F4-PCT-ACTION-NBR   TO DB-ACTION-NBR.
           PERFORM 840-FIND-PCTSET2.
           IF (PERSACTION-FOUND)
               MOVE PCT-UPDATE-BENEFIT   TO PA52F4-PCT-UPDATE-BENEFIT
               MOVE PCT-UPD-ABS-MGMT     TO PA52F4-PCT-UPD-ABS-MGMT
               MOVE PCT-UPDATE-REQ-DED   TO PA52F4-PCT-UPDATE-REQ-DED
               MOVE PCT-REASON (1)       TO PA52F4-PCT-REASON (1)
               MOVE PCT-REASON (2)       TO PA52F4-PCT-REASON (2)
           ELSE
               IF  (PA52F4-FIRST-ACTION-CODE = SPACES)
               OR  (PA52F4-PCT-ACTION-CODE   = PA52F4-FIRST-ACTION-CODE)
                   IF (PAT-DFT-REASON (1) NOT = SPACES)
                       MOVE PAT-DFT-REASON (1) TO PA52F4-PCT-REASON (1)
                   END-IF
                   IF (PAT-DFT-REASON (2) NOT = SPACES)
                       MOVE PAT-DFT-REASON (2) TO PA52F4-PCT-REASON (2)
                   END-IF
                   IF (PAT-DFT-UPD-BN = SPACES)
                       MOVE PA52F4-PCT-COMPANY      TO DB-COMPANY
                       PERFORM 840-KFIND-BNCSET1
                       IF (BNCOMPANY-KFOUND)
                           MOVE "Y" TO PA52F4-PCT-UPDATE-BENEFIT
                       ELSE
                           MOVE "N" TO PA52F4-PCT-UPDATE-BENEFIT
                       END-IF
                   ELSE
                       MOVE PAT-DFT-UPD-BN TO
                                               PA52F4-PCT-UPDATE-BENEFIT
                   END-IF
                   IF (PAT-DFT-UPD-LP = SPACES)
J13588*                MOVE PA52F4-PCT-COMPANY     TO EDCDWS-COMPANY
J13588*                MOVE "LP"                   TO EDCDWS-SYSTEM
J13588*                PERFORM 6000-IS-SYSTEM-TRIGGER-ENABLED
J13588*                IF  (EDCDWS-TRIGGER-ENABLED)
                           MOVE "Y" TO PA52F4-PCT-UPD-ABS-MGMT
J13588*                ELSE
J13588*                    MOVE "N"       TO PA52F4-PCT-UPD-ABS-MGMT
J13588*                END-IF
                   ELSE
                       MOVE PAT-DFT-UPD-LP TO
                                       PA52F4-PCT-UPD-ABS-MGMT
                   END-IF
                   MOVE PAT-DFT-UPD-RQ-DED TO PA52F4-PCT-UPDATE-REQ-DED
               END-IF.

           IF  (PERSACTION-FOUND)
               PERFORM 8400-AFTER-FIND-PCT
               PERFORM 481-MOVE-TO-SCREEN
               THRU    481-END
               MOVE CRT-INQ-COMPLETE    TO CRT-MESSAGE
           ELSE
               IF (PA52F4-FIRST-ACTION-CODE = SPACES)
               OR (PA52F4-PCT-ACTION-CODE   = PA52F4-FIRST-ACTION-CODE)
                   CONTINUE
               ELSE
                   INITIALIZE PA52F4-PCT-UPDATE-BENEFIT
                              PA52F4-PCT-UPD-ABS-MGMT
                              PA52F4-PCT-UPDATE-REQ-DED
                              PA52F4-PCT-EDM-EFFECT-DT
                              PA52F4-PCT-REASON (1)
                              PA52F4-PCT-REASON (2)
               END-IF
P81486         INITIALIZE     PA52F4-PCT-APPROVAL-FLAG
               MOVE 407                 TO CRT-MSG-NBR
           END-IF.
445900
446000 480-END.
446100
446200******************************************************************
446300 481-MOVE-TO-SCREEN.
446400******************************************************************
446500
446600     MOVE PCT-COMPANY            TO PA52F4-PCT-COMPANY.
           MOVE PCT-ACTION-CODE        TO PA52F4-PCT-ACTION-CODE.
446900     MOVE PCT-ACTION-NBR         TO PA52F4-PCT-ACTION-NBR.
447100     MOVE PCT-EMPLOYEE           TO PA52F4-PCT-APPLICANT.
447800     MOVE PCT-REQUISITION        TO PA52F4-PJR-REQUISITION.
447900     INITIALIZE                     PA52F4-PJR-DESCRIPTION.
           INITIALIZE                     PA52F4-PCT-NEW-EFFECT-DATE
P49680                                    PA52F4-PREASSIGNED-DONE.
448800
448900     MOVE PCT-EFFECT-DATE        TO PA52F4-PCT-EFFECT-DATE.
449000     MOVE PCT-ANT-END-DATE       TO PA52F4-PCT-ANT-END-DATE.
449300     MOVE "N"                    TO PA52F4-IMMEDIATE-ACTION.
           MOVE PCT-UPDATE-BENEFIT     TO PA52F4-PCT-UPDATE-BENEFIT.
           MOVE PCT-UPD-ABS-MGMT       TO PA52F4-PCT-UPD-ABS-MGMT.
449300     MOVE PCT-UPDATE-REQ-DED     TO PA52F4-PCT-UPDATE-REQ-DED.
449300     MOVE PCT-EDM-EFFECT-DT      TO PA52F4-PCT-EDM-EFFECT-DT.
449400
446700     MOVE PA52WS-PRS-NAME        TO PA52F4-PRS-NAME.
446800     MOVE PAT-DESCRIPTION        TO PA52F4-PAT-DESCRIPTION.
           MOVE PCT-APPROVAL-FLAG      TO PA52F4-PCT-APPROVAL-FLAG.
J60711
J60711     IF (PA52F4-FIRST-HOLD-FLAG = "Y")
J60711         MOVE 175                TO CRT-MSG-NBR
J60711         PERFORM 790-GET-MSG
J60711         MOVE CRT-MESSAGE        TO PA52F4-ON-HOLD-MSG
J60711     ELSE
J60711         INITIALIZE                 PA52F4-ON-HOLD-MSG
J60711     END-IF.

117100     PERFORM
117200         VARYING I1 FROM 1 BY 1
117300         UNTIL  (I1 > 36)
117400
117500             IF (PA52WS-PCT-PRE-VALUE (I1) = PA52WS-SEC-MESSAGE)
117800                 MOVE PA52WS-SEC-MESSAGE
                                           TO PA52WS-PCT-NEW-VALUE (I1)
117900                 IF (I1 < 13)
118000                     MOVE PA52WS-SEC-MESSAGE TO
118100                                      PA52F4-PCT-NEW-VALUE-1 (I1)
118200                 ELSE
118300                 IF (I1 > 24)
118400                     COMPUTE I8 = (I1 - 24)
118500                     MOVE PA52WS-SEC-MESSAGE TO
118600                                      PA52F4-PCT-NEW-VALUE-3 (I8)
118700                 ELSE
118800                 IF (I1 > 12)
118900                     COMPUTE I8 = (I1 - 12)
119000                     MOVE PA52WS-SEC-MESSAGE TO
119100                                      PA52F4-PCT-NEW-VALUE-2 (I8)
119200                 END-IF
119300                 END-IF
119400                 END-IF
119500             ELSE
119600                 MOVE PCT-NEW-VALUE (I1) TO
119700                                      PA52WS-PCT-NEW-VALUE (I1)
119800                 IF (I1 < 13)
119900                     MOVE PCT-NEW-VALUE (I1)
120000                                   TO PA52F4-PCT-NEW-VALUE-1 (I1)
120100                 ELSE
120200                 IF (I1 > 24)
120300                     COMPUTE I8 = (I1 - 24)
120400                     MOVE PCT-NEW-VALUE (I1)
120500                                   TO PA52F4-PCT-NEW-VALUE-3 (I8)
120600                 ELSE
120700                 IF (I1 > 12)
120800                     COMPUTE I8 = (I1 - 12)
120900                     MOVE PCT-NEW-VALUE (I1)
121000                                    TO PA52F4-PCT-NEW-VALUE-2 (I8)
121100                 END-IF
121200                 END-IF
121300             END-IF
121400         END-IF
121500     END-PERFORM.

090800     MOVE "PAMSG"                TO CRT-ERROR-CAT.
090900     MOVE 100                    TO CRT-MSG-NBR.
091000     PERFORM 790-GET-MSG.
091100     MOVE CRT-MESSAGE            TO PA52F4-COMMENTS.
           INITIALIZE CRT-MESSAGE.

           INITIALIZE PA52F4-COMMENTS-FLAG.
121700     MOVE PA52F4-PCT-COMPANY     TO DB-COMPANY.
121800     MOVE 1                      TO DB-EMP-APP.
121900     MOVE "PA"                   TO DB-CMT-TYPE.
122200     MOVE PA52F4-PCT-ACTION-CODE TO DB-ACTION-CODE.
122000     MOVE PA52F4-PCT-EFFECT-DATE TO DB-DATE.
122000     MOVE PA52F4-PCT-APPLICANT   TO DB-EMPLOYEE.
122100     INITIALIZE DB-JOB-CODE.
122300     MOVE PA52F4-PCT-ACTION-NBR  TO DB-LN-NBR.
122400     MOVE PACSET2-LN-NBR         TO WS-DB-BEG-RNG.
122500     PERFORM 850-KFIND-BEGRNG-PACSET2.
           IF (PACOMMENTS-KFOUND)
               MOVE "*"                TO PA52F4-COMMENTS-FLAG.
122600
453800     MOVE PCT-COMPANY            TO DB-COMPANY.
453900     MOVE 1                      TO DB-EMP-APP.
454000     MOVE "PA"                   TO DB-CMT-TYPE.
454100     MOVE ZEROS                  TO DB-EMPLOYEE.
454200     MOVE SPACES                 TO DB-JOB-CODE.
454300     MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE.
454400     MOVE PCT-ACTION-NBR         TO DB-LN-NBR.
454500     MOVE PACSET1-LN-NBR         TO WS-DB-BEG-RNG.
454600     PERFORM 850-FIND-BEGRNG-PACSET1.
454700
454800 481-END.
454900
122900******************************************************************
123000 484-MOVE-CURRENT-DATA.
123100******************************************************************
123200
123300     IF (PA52F4-PAT-FLD-NBR-1 (1) = ZEROS)
123400         MOVE 36                        TO I1
123500         GO TO 484-END.
123600
123700     IF (I1 < 13)
               IF  (PA52F4-PAT-FLD-NBR-1 (I1) = HREMP-PROCESS-LEVEL-DN)
J02128             IF (APPLICANT-FOUND)
J02128                 PERFORM 233-LTM-CHECK
J02128                 THRU    233-END 
J02128                 IF (PA52WS-LTM-FLAG = SPACES)
J02128                     GO TO 484-END
J02128                 END-IF
J02128             ELSE
J02128                 GO TO 484-END
J02128             END-IF 
               END-IF
               IF  (PA52F4-PAT-FLD-NBR-1 (I1) = ZEROES)
                   MOVE SPACES         TO PA52F4-PCT-PRE-VALUE-1 (I1)
                                          PA52WS-PCT-PRE-VALUE (I1)
                   GO TO 484-END
               END-IF
123800         MOVE PA52F4-PAT-FLD-NBR-1 (I1) TO DB-FLD-NBR
               INITIALIZE                        DB-COUNTRY-CD-REQ
                                                 DB-PROCESS-LEVEL
123900         PERFORM 840-FIND-PASSET1
124000         IF (PASCRTY-FOUND)
124100             MOVE PAS-SEC-LEVEL         TO HRWS-SEC-LEVEL
124200             PERFORM 730-HR-FIELD-SECURITY
124300             IF (HRWS-FLD-SECURED)
124400                 MOVE 411               TO CRT-MSG-NBR
124500                 PERFORM 790-GET-MSG
124600                 MOVE CRT-MESSAGE  TO PA52F4-PCT-PRE-VALUE-1 (I1)
                                            PA52WS-PCT-PRE-VALUE (I1)
125000                 GO TO 484-END
125100             END-IF
125200         END-IF
125300         MOVE PA52F4-PAT-FLD-NBR-1 (I1) TO HREMP-FLD-NBR
125400         MOVE "Y"                       TO HREMP-FORMAT-FIELD
125500         MOVE PA52F4-PCT-COMPANY        TO HREMP-COMPANY
125700         MOVE SPACES                    TO HREMP-VALUE
125800         PERFORM 5000-PAAPL-RETRIEVE-VALUE
125900         MOVE HREMP-VALUE        TO PA52F4-PCT-PRE-VALUE-1 (I1)
126200                                    PA52WS-PCT-PRE-VALUE (I1)
126300     ELSE
126400     IF (I1 > 24)
126500         COMPUTE I8 = (I1 - 24)
               IF  (PA52F4-PAT-FLD-NBR-3 (I8) = HREMP-PROCESS-LEVEL-DN)
J04749             IF (APPLICANT-FOUND)
J04749                 PERFORM 233-LTM-CHECK
J04749                 THRU    233-END 
J04749                 IF (PA52WS-LTM-FLAG = SPACES)
J04749                     GO TO 484-END
J04749                 END-IF
J04749             ELSE
J04749                 GO TO 484-END
J04749             END-IF 
               END-IF
               IF  (PA52F4-PAT-FLD-NBR-3 (I8) = ZEROES)
                   MOVE SPACES         TO PA52F4-PCT-PRE-VALUE-3 (I8)
                                          PA52WS-PCT-PRE-VALUE (I1)
                   GO TO 484-END
               END-IF
126600         MOVE PA52F4-PAT-FLD-NBR-3 (I8) TO DB-FLD-NBR
               INITIALIZE                        DB-COUNTRY-CD-REQ
                                                 DB-PROCESS-LEVEL
126700         PERFORM 840-FIND-PASSET1
126800         IF (PASCRTY-FOUND)
126900             MOVE PAS-SEC-LEVEL         TO HRWS-SEC-LEVEL
127000             PERFORM 730-HR-FIELD-SECURITY
127100             IF (HRWS-FLD-SECURED)
127200                 MOVE 411               TO CRT-MSG-NBR
127300                 PERFORM 790-GET-MSG
127400                 MOVE CRT-MESSAGE  TO PA52F4-PCT-PRE-VALUE-3 (I8)
127700                                      PA52WS-PCT-PRE-VALUE (I1)
127800                 GO TO 484-END
127900             END-IF
128000         END-IF
J07960*        IF (PA52F4-PAT-FLD-NBR-3 (I8) = HREMP-NBR-FTE-DN)
J07960*           MOVE SPACES TO PA52F4-PCT-PRE-VALUE-3 (I8)
J07960*           MOVE SPACES TO PA52WS-PCT-PRE-VALUE (I1)
J07960*           GO TO 484-END
J07960*        END-IF
128100         MOVE PA52F4-PAT-FLD-NBR-3 (I8) TO HREMP-FLD-NBR
128200         MOVE "Y"                       TO HREMP-FORMAT-FIELD
128300         MOVE PA52F4-PCT-COMPANY        TO HREMP-COMPANY
128400         MOVE PA52F4-PCT-EMPLOYEE       TO HREMP-EMPLOYEE
128500         MOVE SPACES                    TO HREMP-VALUE
128600         PERFORM 5000-PAAPL-RETRIEVE-VALUE
128700         MOVE HREMP-VALUE        TO PA52F4-PCT-PRE-VALUE-3 (I8)
129000                                    PA52WS-PCT-PRE-VALUE (I1)
129100     ELSE
129200     IF (I1 > 12)
129300         COMPUTE I8 = (I1 - 12)
               IF  (PA52F4-PAT-FLD-NBR-2 (I8) = HREMP-PROCESS-LEVEL-DN)
J04749             IF (APPLICANT-FOUND)
J04749                 PERFORM 233-LTM-CHECK
J04749                 THRU    233-END 
J04749                 IF (PA52WS-LTM-FLAG = SPACES)
J04749                     GO TO 484-END
J04749                 END-IF
J04749             ELSE
J04749                 GO TO 484-END
J04749             END-IF 
               END-IF
               IF  (PA52F4-PAT-FLD-NBR-2 (I8) = ZEROES)
                   MOVE SPACES         TO PA52F4-PCT-PRE-VALUE-2 (I8)
                                          PA52WS-PCT-PRE-VALUE (I1)
                   GO TO 484-END
               END-IF
129400         MOVE PA52F4-PAT-FLD-NBR-2 (I8) TO DB-FLD-NBR
               INITIALIZE                        DB-COUNTRY-CD-REQ
                                                 DB-PROCESS-LEVEL
129500         PERFORM 840-FIND-PASSET1
129600         IF (PASCRTY-FOUND)
129700             MOVE PAS-SEC-LEVEL         TO HRWS-SEC-LEVEL
129800             PERFORM 730-HR-FIELD-SECURITY
129900             IF (HRWS-FLD-SECURED)
130000                 MOVE 411               TO CRT-MSG-NBR
130100                 PERFORM 790-GET-MSG
130200                 MOVE CRT-MESSAGE  TO PA52F4-PCT-PRE-VALUE-2 (I8)
130500                                      PA52WS-PCT-PRE-VALUE (I1)
130600                 GO TO 484-END
130700             END-IF
130800         END-IF
J07960*        IF (PA52F4-PAT-FLD-NBR-2 (I8) = HREMP-NBR-FTE-DN)
J07960*           MOVE SPACES TO PA52F4-PCT-PRE-VALUE-2 (I8)
J07960*           MOVE SPACES TO PA52WS-PCT-PRE-VALUE (I1)
J07960*           GO TO 484-END
J07960*        END-IF
130900         MOVE PA52F4-PAT-FLD-NBR-2 (I8) TO HREMP-FLD-NBR
131000         MOVE "Y"                       TO HREMP-FORMAT-FIELD
131100         MOVE PA52F4-PCT-COMPANY        TO HREMP-COMPANY
131200         MOVE PA52F4-PCT-EMPLOYEE       TO HREMP-EMPLOYEE
131300         MOVE SPACES                    TO HREMP-VALUE
131400         PERFORM 5000-PAAPL-RETRIEVE-VALUE
131500         MOVE HREMP-VALUE        TO PA52F4-PCT-PRE-VALUE-2 (I8)
131800                                    PA52WS-PCT-PRE-VALUE (I1).
131900
132000 484-END.
132100
455000******************************************************************
455100 500-MOVE-DATA.
455200******************************************************************
455300
455400     MOVE PA52F4-PCT-COMPANY         TO PCT-COMPANY.
455500     MOVE "A"                        TO PCT-ACTION-TYPE.
455600     MOVE PA52F4-PCT-ACTION-CODE     TO PCT-ACTION-CODE.
           MOVE PA52F4-PCT-EMPLOYEE        TO PCT-PARTICIPNT.
455700     MOVE PA52F4-PCT-APPLICANT       TO PCT-EMPLOYEE.
455800     MOVE PA52F4-PJR-REQUISITION     TO PCT-REQUISITION.
455900     MOVE PA52F4-PCT-EFFECT-DATE     TO PCT-EFFECT-DATE.
456000     MOVE PA52F4-PCT-ANT-END-DATE    TO PCT-ANT-END-DATE.
456100     MOVE PA52F4-PCT-REASON (1)      TO PCT-REASON (1).
456200     MOVE PA52F4-PCT-REASON (2)      TO PCT-REASON (2).
           MOVE PA52F4-PCT-UPDATE-BENEFIT  TO PCT-UPDATE-BENEFIT.
           MOVE PA52F4-PCT-UPD-ABS-MGMT    TO PCT-UPD-ABS-MGMT.
456400     MOVE PA52WS-POS-EFFECT-DATE     TO PCT-POS-EFF-DT.
456500     MOVE 1                          TO PCT-POS-LEVEL.
J59149     MOVE HREMP-PROCESS-LEVEL        TO PCT-PROCESS-LEVEL.
           IF  (PA52F4-USER-ID NOT = SPACES)
               MOVE PA52F4-USER-ID         TO PCT-USER-ID
           ELSE
               MOVE CRT-USER-NAME          TO PCT-USER-ID
           END-IF.
449300     MOVE PA52F4-PCT-UPDATE-REQ-DED  TO PCT-UPDATE-REQ-DED.
449300     MOVE PA52F4-PCT-EDM-EFFECT-DT   TO PCT-EDM-EFFECT-DT.
J08104     IF  (PA52F4-FC = "A")
J08104         IF  (PA52F4-USER-ID NOT = SPACES)
J08104             MOVE PA52F4-USER-ID          TO PCT-CREATE-USER-ID
J08104         ELSE
J08104             MOVE CRT-USER-NAME           TO PCT-CREATE-USER-ID
J08104         END-IF
J08104         MOVE WS-SYSTEM-DATE-YMD          TO PCT-CREATE-DATE
J08104         MOVE HHMMSS                      TO PCT-CREATE-TIME
J08104     END-IF.
J08104     MOVE WS-SYSTEM-DATE-YMD              TO PCT-DATE-STAMP.
J08104     MOVE HHMMSS                          TO PCT-TIME-STAMP.
456700
           IF  (PA52F4-PCT-POSITION NOT = SPACES)
               MOVE PA52F4-PCT-POSITION    TO PCT-POSITION
           ELSE
               MOVE PA52WS-POSITION        TO PCT-POSITION.

           IF (PA52F4-FC = "A")
P81486     AND (PA52F4-PCT-APPROVAL-FLAG NOT = "L")
               IF (PAT-WORKFLOW-FLAG           = "Y")
                   MOVE "N"                    TO PCT-APPROVAL-FLAG
                                               PA52F4-PCT-APPROVAL-FLAG
               ELSE
                   MOVE "Y"                    TO PCT-APPROVAL-FLAG
                                               PA52F4-PCT-APPROVAL-FLAG
               END-IF
           ELSE
               MOVE PA52F4-PCT-APPROVAL-FLAG   TO PCT-APPROVAL-FLAG
           END-IF.
           MOVE ZEROES                         TO PCT-EDM-END-DATE.
           MOVE SPACES                         TO PCT-HIST-CORR-FLAG.
P86027     IF  (PA52F4-FC      = "A")
P86027     OR  (PCT-ACTION-NBR = 1)
               MOVE PA52F4-FIRST-HOLD-FLAG     TO PCT-HOLD-FLAG.

456800     PERFORM
456900         VARYING I1 FROM 1 BY 1
457000         UNTIL  (I1 > 36)
               IF  (I1 >=  1) AND (I1 <= 12)
                   COMPUTE I2 = I1
                   MOVE PA52F4-PAT-FLD-NBR-1 (I2)
                                               TO PCT-FLD-NBR (I1)
                   MOVE PA52F4-PCT-NEW-VALUE-1 (I2)
                                               TO PCT-NEW-VALUE (I1)
               END-IF
               IF  (I1 >= 13) AND (I1 <= 24)
                   COMPUTE I2 = I1 - 12
                   MOVE PA52F4-PAT-FLD-NBR-2 (I2)
                                               TO PCT-FLD-NBR (I1)
                   MOVE PA52F4-PCT-NEW-VALUE-2 (I2)
                                               TO PCT-NEW-VALUE (I1)
               END-IF
               IF  (I1 >= 25) AND (I1 <= 36)
                   COMPUTE I2 = I1 - 24
                   MOVE PA52F4-PAT-FLD-NBR-3 (I2)
                                               TO PCT-FLD-NBR (I1)
                   MOVE PA52F4-PCT-NEW-VALUE-3 (I2)
                                               TO PCT-NEW-VALUE (I1)
               END-IF
           END-PERFORM.

459000 500-END.
459100
459200******************************************************************
459300 600-CREATE-EMPLOYEE.
459400******************************************************************
459500
           PERFORM 3000-HREMP-NUMBER.
462200
462300     MOVE APL-COMPANY             TO PAPCT-COMPANY.
462400     MOVE APL-APPLICANT           TO PAPCT-APPLICANT.
462500     MOVE APL-WK-RST-CODE1        TO PAPCT-WK-RST-CODE1.
462600     MOVE APL-WK-RST-CODE2        TO PAPCT-WK-RST-CODE2.
462700     MOVE APL-WK-RST-CODE3        TO PAPCT-WK-RST-CODE3.
462800     MOVE APL-WK-RST-CODE4        TO PAPCT-WK-RST-CODE4.
462900     MOVE PA52F4-PCT-EFFECT-DATE  TO HREMP-EFFECT-DATE.
463000     MOVE PA52F4-PCT-ACTION-CODE  TO HREMP-ACTION-CODE.
463100     MOVE PA52WS-ACTION-NBR       TO HREMP-ACTION-NBR.
463200     MOVE PA52F4-PCT-REASON (1)   TO HREMP-REASON1.
463300     MOVE PA52F4-PCT-REASON (2)   TO HREMP-REASON2.

J06067     IF (PA52WS-LTM-FLAG = "*") 
J06067         MOVE APL-L-JOB-APP       TO HREMP-L-JOB-APP 
J06067         MOVE APL-L-JOB-REQ       TO HREMP-L-JOB-REQ 
J06067         MOVE APL-L-CANDIDATE-ID  TO HREMP-L-CANDIDATE-ID  
J59149
J59149*********BY THIS TIME HREMP-PROCESS-LEVEL SHOULD ALREADY HAVE THE 
J59149*********CORRECT VALUE EITHER FROM PA31 OR FROM PA52.4 SO THERE IS
J59149*********NO NEED TO MOVE VALUE COMING FROM APPLICANT TABLE.
J59149 
J59149*        IF  (APL-PROCESS-LEVEL  NOT = SPACES)
J59149*            MOVE APL-PROCESS-LEVEL TO HREMP-PROCESS-LEVEL
J59149*        END-IF
J06067         IF (APL-DEPARTMENT NOT = SPACES)
J73281         AND (HREMP-DEPARTMENT = SPACES)
J06067             MOVE APL-DEPARTMENT    TO HREMP-DEPARTMENT
J06067         END-IF
J06067     END-IF.

           IF  (PA52F4-USER-ID NOT = SPACES)
               MOVE PA52F4-USER-ID      TO HREMP-USER-ID
           ELSE
               MOVE CRT-USER-NAME       TO HREMP-USER-ID
           END-IF.
463500     MOVE PA52F4-PCT-ANT-END-DATE TO HREMP-ANT-END-DATE.
463600     MOVE PA52WS-OBJ-ID           TO HREMP-ACT-OBJ-ID.
463800
456800     PERFORM
456900         VARYING I1 FROM 1 BY 1
457000         UNTIL  (I1 > 12)
               MOVE PA52F4-PAT-FLD-NBR-1 (I1)  TO I2
               IF  (I2 NOT = ZEROES)
               AND (I2 < 2000)
                   MOVE "X"                    TO HREMP-LOG-FLAG (I2)
               END-IF
               MOVE PA52F4-PAT-FLD-NBR-2 (I1)  TO I2
               IF  (I2 NOT = ZEROES)
               AND (I2 < 2000)
                   MOVE "X"                    TO HREMP-LOG-FLAG (I2)
               END-IF
               MOVE PA52F4-PAT-FLD-NBR-3 (I1)  TO I2
               IF  (I2 NOT = ZEROES)
               AND (I2 < 2000)
                   MOVE "X"                    TO HREMP-LOG-FLAG (I2)
               END-IF
           END-PERFORM.
P58299
P58299* THIS CODE IS FOR LOGGING THE PA31 DEFAULT FIELDS
P58299     PERFORM
P58299        VARYING I1 FROM 1 BY 1
P58299        UNTIL  (I1 > 34)
P58299        MOVE   PA52WS-PAT-FLD-NBR-4 (I1) TO I2
P58299        MOVE   "X"                       TO HREMP-LOG-FLAG (I2) 
P58299        END-PERFORM.
P58299
           IF  (HREMP-LOG-FLAG (HREMP-PAY-RATE-DN) = "X")
           OR  (HREMP-LOG-FLAG (HREMP-NBR-FTE-DN) = "X")
           OR  (HREMP-LOG-FLAG (HREMP-ANNUAL-HOURS-DN) = "X")
           OR  (HREMP-LOG-FLAG (HREMP-JOB-CODE-DN) = "X")
           OR  (HREMP-LOG-FLAG (HREMP-SALARY-CLASS-DN) = "X")
               MOVE "X"  TO HREMP-LOG-FLAG (HREMP-PRO-RATE-A-SAL-DN).

           MOVE HREMP-LOG-FLAG (HREMP-NBR-FTE-DN)
                         TO HREMP-LOG-FLAG (HREMP-FTE-TOTAL-DN).
           MOVE HREMP-LOG-FLAG (HREMP-PRO-RATE-A-SAL-DN)
                         TO HREMP-LOG-FLAG (HREMP-PRO-RATE-TOTAL-DN).

           MOVE PA52F4-PCT-UPD-ABS-MGMT    TO HREMP-UPDATE-ABSENCE-MGMT.

P85139     PERFORM 620-DO-USER-FIELDS
P85139     THRU    620-END.
           MOVE "Y"                 TO HRWS-ADD.
           SET HREMP-HIRE-ACTION TO TRUE.
           SET HREMP-UPDATE-M3             TO TRUE.
467100     PERFORM 3000-HREMP-PROCESS-TRAN.
           INITIALIZE HREMP-HIRE-ACTION-SW.
467200
P85139*    PERFORM 620-DO-USER-FIELDS
P85139*    THRU    620-END.

           IF (PA52F4-PCT-UPDATE-REQ-DED     = "Y")
467300         MOVE HREMP-COMPANY            TO PRRQC-COMPANY
467400         MOVE HREMP-EMPLOYEE           TO PRRQC-EMPLOYEE
467500         INITIALIZE PRRQC-DFT-MAR-STAT
467700                    PRRQC-DFT-EXEMPTS
               MOVE PA52F4-PCT-EDM-EFFECT-DT TO PRRQC-EFFECT-DATE
               INITIALIZE PRRQC-END-DATE
                          PRRQC-UPDATE-OPTION
J85747         MOVE PA52F4-FORM-YEAR         TO PRRQC-FORM-YEAR
J85747                                          PREDM-FORM-YEAR
467800         PERFORM 500-REQ-DED-CREATION.
467900
468000     PERFORM 5000-HREMP-CREATE-ETM.
468100
P62980     MOVE PA52F4-PCT-COMPANY     TO DB-COMPANY.
468200     MOVE PA52F4-PCT-APPLICANT   TO DB-APPLICANT.
468300     PERFORM 840-MODIFY-APLSET1.
           MOVE HREMP-EMPLOYEE         TO APL-EMPLOYEE.
468400     MOVE HREMP-DATE-HIRED       TO APL-DATE-HIRED.
468500     PERFORM 820-STORE-APPLICANT.
468600
468700 600-END.

      ******************************************************************
       620-DO-USER-FIELDS.
      ******************************************************************

           INITIALIZE HRWS-FIELD-NBR-TABLE
                      I2.

164000     PERFORM
164100         VARYING I1 FROM 1 BY 1
164200         UNTIL  (I1 > 36)
164300
164400         IF  (PA52WS-PAT-FIELD-NBR (I1) NOT < 2000)
164500         AND (PA52WS-PAT-FIELD-NBR (I1) NOT > 2099)
164600         AND (PA52WS-PCT-NEW-VALUE (I1) NOT = SPACES)
164700             INITIALIZE PAPCT-SCR-FIELDS
164800             MOVE PA52WS-PAT-FIELD-NBR (I1) TO PAPCT-FLD-NBR
164900             MOVE PA52WS-PCT-NEW-VALUE (I1) TO PAPCT-NEW-VALUE
165000             MOVE PA52F4-PCT-COMPANY        TO PAPCT-COMPANY
165100             MOVE HREMP-EMPLOYEE            TO PAPCT-EMPLOYEE
165200             MOVE PA52F4-PCT-EFFECT-DATE    TO PAPCT-EFFECT-DATE
165300             MOVE PA52F4-PCT-ACTION-CODE    TO PAPCT-ACTION-CODE
165400             MOVE PA52WS-ACTION-NBR         TO PAPCT-ACTION-NBR
165500             MOVE PA52F4-PCT-REASON (1)     TO PAPCT-REASON1
165600             MOVE PA52F4-PCT-REASON (2)     TO PAPCT-REASON2
P85139             MOVE PA52F4-PCT-APPLICANT      TO PAPCT-APPLICANT
                   IF  (PA52F4-USER-ID NOT = SPACES)
                       MOVE PA52F4-USER-ID        TO PAPCT-USER-ID
                   ELSE
                       MOVE CRT-USER-NAME         TO PAPCT-USER-ID
                   END-IF
165800             MOVE PA52F4-PCT-ANT-END-DATE   TO PAPCT-ANT-END-DATE
165900             PERFORM 5100-EDIT-USER-FIELDS
166000             PERFORM 3000-HRHEU-PROCESS-TRAN

                   MOVE PA52F4-PCT-COMPANY        TO DB-COMPANY
                   INITIALIZE                        DB-COUNTRY-CD-REQ
                                                     DB-PROCESS-LEVEL
                   MOVE PA52WS-PAT-FIELD-NBR (I1) TO DB-FLD-NBR
                   PERFORM 840-FIND-PASSET1
                   IF  (PASCRTY-FOUND)
                   AND (PAS-USED-BY-GROUP         = "X")
                       ADD 1                      TO I2
                       MOVE DB-FLD-NBR            TO HRWS-FIELD-NBR (I2)
                   END-IF
166100         END-IF
166200     END-PERFORM.
166300
           IF (I2                          NOT = ZEROES)
               MOVE WS-FALSE               TO HRWS-ALL-EMPLOYEES-SW
               MOVE PA52F4-PCT-COMPANY     TO HRWS-COMPANY
               MOVE PA52F4-PCT-EFFECT-DATE TO HRWS-EFFECT-DATE
               MOVE PA52WS-OBJ-ID          TO HRWS-ACT-OBJ-ID
               MOVE PA52F4-PCT-UPDATE-BENEFIT
                                           TO HRWS-UPDATE-BENEFIT
               MOVE PA52F4-PCT-UPD-ABS-MGMT
                                           TO HRWS-UPDATE-ABSENCE-MGMT
               PERFORM 5000-REBUILD-PERSONNEL-GROUP.

166400 620-END.
468800******************************************************************
468900 650-DEFAULT.
469000******************************************************************
469100
           MOVE PA52F4-PCT-POSITION    TO PA52WS-POSITION.
           INITIALIZE PA52WS-POS-IDX.
           IF  (PA52WS-POSITION = SPACES)
               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > 12)
                   IF  (PA52F4-PAT-FLD-NBR-1 (I1) = HREMP-POSITION-DN)
                       COMPUTE PA52WS-POS-IDX = I1
J73281                 MOVE PAAPLWS-LEM-POSITION   TO HRWS-UP-FIELD
J73281                 IF (PA52F4-PCT-NEW-VALUE-1 (I1) NOT = SPACES)
                           MOVE PA52F4-PCT-NEW-VALUE-1 (I1)
                                                   TO HRWS-UP-FIELD
J73281                 END-IF
                       PERFORM 760-HR-UPPER-CASE
                       MOVE HRWS-UP-FIELD  TO PA52WS-POSITION
                                 PA52F4-PCT-NEW-VALUE-1 (I1)
                   END-IF
                   IF  (PA52F4-PAT-FLD-NBR-2 (I1) = HREMP-POSITION-DN)
                       COMPUTE PA52WS-POS-IDX = I1 + 12
J73281                 MOVE PAAPLWS-LEM-POSITION   TO HRWS-UP-FIELD
J73281                 IF (PA52F4-PCT-NEW-VALUE-2 (I1) NOT = SPACES)
                           MOVE PA52F4-PCT-NEW-VALUE-2 (I1)
                                                   TO HRWS-UP-FIELD
J73281                 END-IF
                       PERFORM 760-HR-UPPER-CASE
                       MOVE HRWS-UP-FIELD  TO PA52WS-POSITION
                                 PA52F4-PCT-NEW-VALUE-2 (I1)
                   END-IF
                   IF  (PA52F4-PAT-FLD-NBR-3 (I1) = HREMP-POSITION-DN)
                       COMPUTE PA52WS-POS-IDX = I1 + 24
J73281                 MOVE PAAPLWS-LEM-POSITION   TO HRWS-UP-FIELD
J73281                 IF (PA52F4-PCT-NEW-VALUE-3 (I1) NOT = SPACES)
                           MOVE PA52F4-PCT-NEW-VALUE-3 (I1)
                                                   TO HRWS-UP-FIELD
J73281                 END-IF
                       PERFORM 760-HR-UPPER-CASE
                       MOVE HRWS-UP-FIELD  TO PA52WS-POSITION
                                PA52F4-PCT-NEW-VALUE-3 (I1)
                   END-IF
               END-PERFORM
           END-IF.
469200     IF  (PA52WS-POSITION           = SPACES)
469300     AND (PA52F4-PJR-REQUISITION    = ZEROES)
469400         MOVE 105                        TO CRT-ERROR-NBR
469500         MOVE PA52F4-FC-FN               TO CRT-FIELD-NBR
469600         GO TO 650-END.
469700
469800     IF (PA52F4-PCT-EFFECT-DATE = ZEROES)
469900         MOVE 106                        TO CRT-ERROR-NBR
470000         MOVE PA52F4-PCT-EFFECT-DATE-FN  TO CRT-FIELD-NBR
470100         GO TO 650-END.
470200
470300     MOVE PA52F4-PCT-COMPANY             TO DB-COMPANY.
470400     MOVE PA52F4-PCT-APPLICANT           TO DB-APPLICANT.
470500     PERFORM 840-FIND-APLSET1.
470600     IF (APPLICANT-NOTFOUND)
470700         MOVE 220                        TO CRT-ERROR-NBR
470800         MOVE PA52F4-PCT-APPLICANT-FN    TO CRT-FIELD-NBR
470900         GO TO 650-END.
471000
471100     IF (PA52F4-PJR-REQUISITION NOT = ZEROES)
471200         MOVE PA52F4-PCT-COMPANY         TO DB-COMPANY
471300         MOVE PA52F4-PJR-REQUISITION     TO DB-REQUISITION
471400         PERFORM 840-FIND-PJRSET1
471500         IF (PAJOBREQ-NOTFOUND)
471600             MOVE 221                    TO CRT-ERROR-NBR
471700             MOVE PA52F4-PJR-REQUISITION-FN
471800                                         TO CRT-FIELD-NBR
471900             GO TO 650-END.
472000
472100     IF  (PA52WS-POSITION           = SPACES)
472200     AND (PA52F4-PJR-REQUISITION    NOT = ZEROES)
472300     AND (PJR-POSITION              NOT = SPACES)
472400         MOVE PJR-POSITION           TO PA52WS-POSITION
               IF  (PA52WS-POS-IDX >=  1) AND (PA52WS-POS-IDX <= 12)
                   MOVE PA52WS-POSITION
                       TO PA52F4-PCT-NEW-VALUE-1 (PA52WS-POS-IDX)
               END-IF
               IF  (PA52WS-POS-IDX >= 13) AND (PA52WS-POS-IDX <= 24)
                   MOVE PA52WS-POSITION
                       TO PA52F4-PCT-NEW-VALUE-2 (PA52WS-POS-IDX - 12)
               END-IF
               IF  (PA52WS-POS-IDX >= 25) AND (PA52WS-POS-IDX <= 36)
                   MOVE PA52WS-POSITION
                       TO PA52F4-PCT-NEW-VALUE-3 (PA52WS-POS-IDX - 24)
               END-IF
           END-IF.
472500
472600     IF (PA52WS-POSITION NOT = SPACES)
472700         MOVE PA52F4-PCT-COMPANY         TO PAPOS-COMPANY
472800         MOVE PA52WS-POSITION            TO PAPOS-POSITION
472900         MOVE PA52F4-PCT-EFFECT-DATE     TO PAPOS-EFFECT-DATE
473100         IF (PA52F4-PCT-EFFECT-DATE < WS-SYSTEM-DATE-YMD)
473200             MOVE WS-SYSTEM-DATE-YMD     TO PAPOS-END-DATE
473300         ELSE
473400             MOVE PA52F4-PCT-EFFECT-DATE TO PAPOS-END-DATE
473600         END-IF
474000         MOVE 1                          TO ERROR-FLAG-SW
474100         PERFORM 2000-EDIT-POSITION-DATES
474200         IF (ERROR-FOUND)
474300             GO TO 650-END
474400         END-IF
474500         MOVE "PA52"                             TO CRT-ERROR-CAT
474600         IF (PAPOS-STATUS > 2)
474700             MOVE 103                            TO CRT-ERROR-NBR
                   MOVE PA52F4-FC-FN                   TO CRT-FIELD-NBR
                   IF  (PA52WS-POS-IDX >=  1) AND (PA52WS-POS-IDX <= 12)
                       MOVE PA52F4-PCT-NEW-VALUE-1-FN (PA52WS-POS-IDX)
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52WS-POS-IDX >= 13) AND (PA52WS-POS-IDX <= 24)
                    MOVE PA52F4-PCT-NEW-VALUE-2-FN (PA52WS-POS-IDX - 12)
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52WS-POS-IDX >= 25) AND (PA52WS-POS-IDX <= 36)
                    MOVE PA52F4-PCT-NEW-VALUE-3-FN (PA52WS-POS-IDX - 24)
                                                       TO CRT-FIELD-NBR
                   END-IF
475000             GO TO 650-END.
475100
475200     IF  (PA52WS-POSITION NOT = SPACES)
475300         PERFORM 725-DEFAULT-ACTION
475400         THRU    725-END.
475500
475600     IF (ERROR-FOUND)
475700         GO TO 650-END.

475900     MOVE "PA52"                     TO CRT-ERROR-CAT.
476000
476100     IF (PA52F4-PJR-REQUISITION NOT = ZEROES)
476200         PERFORM 850-DEFAULT-REQ
476300         THRU    850-END.
476400
476800     MOVE 109                            TO CRT-MSG-NBR.
476900     MOVE PA52F4-FC-FN                   TO CRT-FIELD-NBR.
477000
477100 650-END.
477200
477300******************************************************************
477400 725-DEFAULT-ACTION.
477500******************************************************************
477600
           SET PA52WS-USE-COMPANY          TO TRUE.
           IF (PA52WS-POSITION NOT = SPACES)
               MOVE PA52F4-PCT-COMPANY     TO DB-COMPANY
               MOVE PA52WS-POSITION        TO DB-POSITION
               MOVE PA52F4-PCT-EFFECT-DATE TO DB-EFFECT-DATE
               PERFORM 850-FIND-NLT-POSSET2
               IF  (PAPOSITION-FOUND)
               AND (POS-COMPANY           = DB-COMPANY)
               AND (POS-POSITION          = DB-POSITION)
               AND (POS-PROCESS-LEVEL NOT = SPACES)
                   MOVE "PO"               TO DB-TOPIC
                   MOVE SPACES             TO DB-COUNTRY-CD-REQ
                   MOVE POS-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
                   MOVE PASSET2-PROCESS-LEVEL
                                           TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-PASSET2
                   IF (PASCRTY-FOUND)
                       SET PA52WS-USE-PROCESS-LEVEL
                                           TO TRUE.

478200     PERFORM 750-CHECK-SECURITY
478300     THRU    750-END.

478800     IF (ERROR-FOUND)
478900         GO TO 725-END.
479000
           INITIALIZE                         PADFP-FIELDS.

479400     PERFORM 775-LOAD-PADFP-FIELDS
479500     THRU    775-END.
479600
479700     PERFORM 2000-EDIT-DEFAULTS.
479800     IF (ERROR-FOUND)
479900         GO TO 725-END.
480000
480100     PERFORM 780-SAVE-PADFP-FIELDS
480200     THRU    780-END.
480300
480400 725-END.
480500******************************************************************
480600 750-CHECK-SECURITY.
480700******************************************************************

           MOVE "PO"                       TO DB-TOPIC.
           PERFORM
               VARYING I9 FROM 1 BY 1
               UNTIL  (I9 > PAPOSEMP-TABLE-SIZE)
               OR     (ERROR-FOUND)

               MOVE PAPOSEMP-POS-FLD (I9)  TO DB-FLD-NBR
               INITIALIZE                     DB-COUNTRY-CD-REQ
               IF (PA52WS-USE-PROCESS-LEVEL)
                   MOVE POS-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
               ELSE
                   MOVE SPACES             TO DB-PROCESS-LEVEL
               END-IF
               PERFORM 840-FIND-PASSET2
               IF  (PASCRTY-FOUND)
               AND (PAS-DEFAULT-FLG > 1)
                   MOVE PAPOSEMP-EMP-FLD (I9)  TO DB-FLD-NBR
                   INITIALIZE                     DB-COUNTRY-CD-REQ
                   MOVE SPACES                 TO DB-PROCESS-LEVEL
                   PERFORM 840-FIND-PASSET1
                   IF  (PASCRTY-FOUND)
                       MOVE PAS-SEC-LEVEL          TO HRWS-SEC-LEVEL
                       PERFORM 730-HR-FIELD-SECURITY
                       IF  (HRWS-FLD-SECURED)
                           MOVE 107                    TO CRT-ERROR-NBR
                           MOVE PA52F4-FC-FN           TO CRT-FIELD-NBR.

       750-END.
482300******************************************************************
482400 775-LOAD-PADFP-FIELDS.
482500******************************************************************
482600
482700     MOVE PA52F4-FC-FN               TO PADFP-FC-FN.
482800     MOVE WS-TRUE                    TO PADFP-LVL1-CHG-SW.
482900     MOVE PA52F4-PCT-COMPANY         TO PADFP-COMPANY.
           MOVE PA52F4-PCT-POSITION        TO PADFP-POSITION.
483000     MOVE PA52F4-PCT-EFFECT-DATE     TO PADFP-EFFECT-DATE.
           INITIALIZE                         PADFP-END-DATE.
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 12)

               MOVE PA52F4-PCT-NEW-VALUE-1 (I1)    TO PA52WS-VALUE
               MOVE PA52F4-PCT-NEW-VALUE-1-FN (I1) TO PA52WS-VALUE-FN
               MOVE PA52F4-PAT-FLD-NBR-1 (I1)      TO PA52WS-FLD-NBR
               PERFORM 776-LOAD-PADFP-FIELD
               THRU    776-END

               MOVE PA52F4-PCT-NEW-VALUE-2 (I1)    TO PA52WS-VALUE
               MOVE PA52F4-PCT-NEW-VALUE-2-FN (I1) TO PA52WS-VALUE-FN
               MOVE PA52F4-PAT-FLD-NBR-2 (I1)      TO PA52WS-FLD-NBR
               PERFORM 776-LOAD-PADFP-FIELD
               THRU    776-END

               MOVE PA52F4-PCT-NEW-VALUE-3 (I1)    TO PA52WS-VALUE
               MOVE PA52F4-PCT-NEW-VALUE-3-FN (I1) TO PA52WS-VALUE-FN
               MOVE PA52F4-PAT-FLD-NBR-3 (I1)      TO PA52WS-FLD-NBR
               PERFORM 776-LOAD-PADFP-FIELD
               THRU    776-END

           END-PERFORM.

491300 775-END.
      ******************************************************************
       776-LOAD-PADFP-FIELD.
      ******************************************************************

           IF  (PA52WS-FLD-NBR = HREMP-POSITION-DN)
           AND (PA52WS-VALUE NOT = SPACES)
               MOVE PA52WS-VALUE           TO PADFP-POSITION
               MOVE PA52WS-VALUE-FN        TO PADFP-POSITION-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-JOB-CODE-DN)
               MOVE PA52WS-VALUE           TO PADFP-JOB-CODE
               MOVE PA52WS-VALUE-FN        TO PADFP-JOB-CODE-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PROCESS-LEVEL-DN)
               MOVE PA52WS-VALUE           TO PADFP-PROCESS-LEVEL
               MOVE PA52WS-VALUE-FN        TO PADFP-PROCESS-LEVEL-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-DEPARTMENT-DN)
               MOVE PA52WS-VALUE           TO PADFP-DEPARTMENT
               MOVE PA52WS-VALUE-FN        TO PADFP-DEPARTMENT-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-USER-LEVEL-DN)
               MOVE PA52WS-VALUE           TO PADFP-USER-LEVEL
               MOVE PA52WS-VALUE-FN        TO PADFP-USER-LEVEL-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-ANNUAL-HOURS-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC          TO PADFP-ANNUAL-HOURS
J46844         MOVE PAPCT-4-DECIMALS       TO PADFP-ANNUAL-HOURS
               MOVE PA52WS-VALUE-FN        TO PADFP-ANNUAL-HOURS-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SALARY-CLASS-DN)
               MOVE PA52WS-VALUE           TO PADFP-SALARY-CLASS
               MOVE PA52WS-VALUE-FN        TO PADFP-SALARY-CLASS-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SCHEDULE-DN)
               MOVE PA52WS-VALUE           TO PADFP-SCHEDULE
               MOVE PA52WS-VALUE-FN        TO PADFP-SCHEDULE-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PAY-GRADE-DN)
               MOVE PA52WS-VALUE           TO PADFP-PAY-GRADE
               MOVE PA52WS-VALUE-FN        TO PADFP-PAY-GRADE-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PAY-STEP-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC          TO PADFP-PAY-STEP
               MOVE PA52WS-VALUE-FN        TO PADFP-PAY-STEP-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SUPERVISOR-DN)
               MOVE PA52WS-VALUE           TO PADFP-SUPERVISOR
               MOVE PA52WS-VALUE-FN        TO PADFP-SUPERVISOR-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SUPERVISOR-IND-DN)
               MOVE PA52WS-VALUE           TO PADFP-SUPERVISOR-IND
               MOVE PA52WS-VALUE-FN        TO PADFP-SUPERVISOR-IND-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SEC-LVL-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC          TO PADFP-SEC-LVL
               MOVE PA52WS-VALUE-FN        TO PADFP-SEC-LVL-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SEC-LOCATION-DN)
               MOVE PA52WS-VALUE           TO PADFP-SEC-LOCATION
               MOVE PA52WS-VALUE-FN        TO PADFP-SEC-LOCATION-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SHIFT-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC          TO PADFP-SHIFT
               MOVE PA52WS-VALUE-FN        TO PADFP-SHIFT-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-WORK-SCHED-DN)
               MOVE PA52WS-VALUE           TO PADFP-WORK-SCHED
               MOVE PA52WS-VALUE-FN        TO PADFP-WORK-SCHED-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HRPEM-LOCAT-CODE-DN)
               MOVE PA52WS-VALUE           TO PADFP-LOCAT-CODE
               MOVE PA52WS-VALUE-FN        TO PADFP-LOCAT-CODE-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-EXEMPT-EMP-DN)
               MOVE PA52WS-VALUE           TO PADFP-EXEMPT-EMP
               MOVE PA52WS-VALUE-FN        TO PADFP-EXEMPT-EMP-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-NBR-FTE-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-6-DECIMALS       TO PADFP-FTE
               MOVE PA52WS-VALUE-FN        TO PADFP-FTE-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-HM-DIST-CO-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC          TO PADFP-EXP-COMPANY
               MOVE PA52WS-VALUE-FN        TO PADFP-EXP-COMPANY-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-HM-ACCT-UNIT-DN)
               MOVE PA52WS-VALUE           TO PADFP-EXP-ACCT-UNIT
               MOVE PA52WS-VALUE-FN        TO PADFP-EXP-ACCT-UNIT-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-HM-ACCOUNT-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC          TO PADFP-EXP-ACCOUNT
               MOVE PA52WS-VALUE-FN        TO PADFP-EXP-ACCOUNT-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-HM-SUB-ACCT-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC          TO PADFP-EXP-SUB-ACCT
               MOVE PA52WS-VALUE-FN        TO PADFP-EXP-SUB-ACCT-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PAY-RATE-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-4-DECIMALS       TO PADFP-PAY-RATE
               MOVE PA52WS-VALUE-FN        TO PADFP-PAY-RATE-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PAY-FREQUENCY-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC          TO PADFP-PAY-FREQUENCY
               MOVE PA52WS-VALUE-FN        TO PADFP-PAY-FREQUENCY-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-ACTIVITY-DN)
               MOVE PA52WS-VALUE           TO PADFP-ACTIVITY
               MOVE PA52WS-VALUE-FN        TO PADFP-ACTIVITY-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-ACCT-CATEGORY-DN)
               MOVE PA52WS-VALUE           TO PADFP-ACCT-CATEGORY
               MOVE PA52WS-VALUE-FN        TO PADFP-ACCT-CATEGORY-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-OT-PLAN-CODE-DN)
               MOVE PA52WS-VALUE           TO PADFP-OT-PLAN-CODE
               MOVE PA52WS-VALUE-FN        TO PADFP-OT-PLAN-CODE-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-CURRENCY-CODE-DN)
               MOVE PA52WS-VALUE           TO PADFP-CURRENCY-CODE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-UNION-CODE-DN)
               MOVE PA52WS-VALUE           TO PADFP-UNION-CODE
               MOVE PA52WS-VALUE-FN        TO PADFP-UNION-CODE-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HRPEM-BARGAIN-UNIT-DN)
               MOVE PA52WS-VALUE           TO PADFP-BARGAIN-UNIT
               MOVE PA52WS-VALUE-FN        TO PADFP-BARGAIN-UNIT-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HRPEM-ASSIGN-DATE-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5300-CONVERT-DATE
               MOVE PAPCT-DATE-FORMAT      TO PADFP-ASSIGN-DATE
               MOVE PA52WS-VALUE-FN        TO PADFP-ASSIGN-DATE-FN
           END-IF.

           IF  (PA52WS-FLD-NBR = HRPEM-USER-AMOUNT-DN)
               MOVE PA52WS-VALUE           TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
P52707         MOVE PAPCT-4-DECIMALS       TO PADFP-USER-AMOUNT
               MOVE PA52WS-VALUE-FN        TO PADFP-USER-AMOUNT-FN
           END-IF.

       776-END.
      ******************************************************************
       780-SAVE-PADFP-FIELDS.
      ******************************************************************

           MOVE PA52F4-FC-FN               TO PADFP-FC-FN.
           MOVE WS-TRUE                    TO PADFP-LVL1-CHG-SW.
           MOVE PA52F4-PCT-COMPANY         TO PADFP-COMPANY.
           MOVE PA52F4-PCT-EFFECT-DATE     TO PADFP-EFFECT-DATE.
           INITIALIZE                         PADFP-END-DATE.
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 12)

               MOVE PA52F4-PCT-NEW-VALUE-1 (I1)    TO PA52WS-VALUE
               MOVE PA52F4-PAT-FLD-NBR-1 (I1)      TO PA52WS-FLD-NBR
               PERFORM 781-SAVE-PADFP-FIELD
               THRU    781-END
J34836         IF (PA52WS-VALUE NOT = PA52F4-PCT-PRE-VALUE-1 (I1))
               MOVE PA52WS-VALUE       TO PA52F4-PCT-NEW-VALUE-1 (I1)
J34836         END-IF

               MOVE PA52F4-PCT-NEW-VALUE-2 (I1)    TO PA52WS-VALUE
               MOVE PA52F4-PAT-FLD-NBR-2 (I1)      TO PA52WS-FLD-NBR
               PERFORM 781-SAVE-PADFP-FIELD
               THRU    781-END
J34836         IF (PA52WS-VALUE NOT = PA52F4-PCT-PRE-VALUE-2 (I1))
               MOVE PA52WS-VALUE       TO PA52F4-PCT-NEW-VALUE-2 (I1)
J34836         END-IF

               MOVE PA52F4-PCT-NEW-VALUE-3 (I1)    TO PA52WS-VALUE
               MOVE PA52F4-PAT-FLD-NBR-3 (I1)      TO PA52WS-FLD-NBR
               PERFORM 781-SAVE-PADFP-FIELD
               THRU    781-END
J34836         IF (PA52WS-VALUE NOT = PA52F4-PCT-PRE-VALUE-3 (I1))
               MOVE PA52WS-VALUE       TO PA52F4-PCT-NEW-VALUE-3 (I1)
J34836         END-IF

           END-PERFORM.

       780-END.
      ******************************************************************
       781-SAVE-PADFP-FIELD.
      ******************************************************************

           IF  (PA52WS-FLD-NBR = HREMP-POSITION-DN)
               MOVE PADFP-POSITION         TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-JOB-CODE-DN)
               MOVE PADFP-JOB-CODE         TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PROCESS-LEVEL-DN)
               MOVE PADFP-PROCESS-LEVEL    TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-DEPARTMENT-DN)
               MOVE PADFP-DEPARTMENT       TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-USER-LEVEL-DN)
               MOVE PADFP-USER-LEVEL       TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-ANNUAL-HOURS-DN)
           AND (PADFP-ANNUAL-HOURS NOT = ZEROES)
               MOVE PADFP-ANNUAL-HOURS     TO HRWS-DEC-FIELD
               MOVE 0                      TO HRWS-SIZE
J46844         MOVE 4                      TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               MOVE HRWS-VALUE             TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SALARY-CLASS-DN)
               MOVE PADFP-SALARY-CLASS     TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SCHEDULE-DN)
               MOVE PADFP-SCHEDULE         TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PAY-GRADE-DN)
               MOVE PADFP-PAY-GRADE        TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PAY-STEP-DN)
           AND (PADFP-PAY-STEP NOT = ZEROES)
               MOVE PADFP-PAY-STEP         TO HRWS-DEC-FIELD
               MOVE 0                      TO HRWS-SIZE
               MOVE 0                      TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               MOVE HRWS-VALUE             TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SUPERVISOR-DN)
               MOVE PADFP-SUPERVISOR       TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SUPERVISOR-IND-DN)
               MOVE PADFP-SUPERVISOR-IND   TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SEC-LVL-DN)
           AND (PADFP-SEC-LVL NOT = ZEROES)
               MOVE PADFP-SEC-LVL          TO HRWS-DEC-FIELD
               MOVE 0                      TO HRWS-SIZE
               MOVE 0                      TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               MOVE HRWS-VALUE             TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SEC-LOCATION-DN)
               MOVE PADFP-SEC-LOCATION     TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SHIFT-DN)
           AND (PADFP-SHIFT NOT = ZEROES)
               MOVE PADFP-SHIFT            TO HRWS-DEC-FIELD
               MOVE 0                      TO HRWS-SIZE
               MOVE 0                      TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               MOVE HRWS-VALUE             TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-WORK-SCHED-DN)
               MOVE PADFP-WORK-SCHED       TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HRPEM-LOCAT-CODE-DN)
               MOVE PADFP-LOCAT-CODE       TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-EXEMPT-EMP-DN)
               MOVE PADFP-EXEMPT-EMP       TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-NBR-FTE-DN)
           AND (PADFP-FTE NOT = ZEROES)
               MOVE PADFP-FTE              TO HRWS-DEC-FIELD
               MOVE 0                      TO HRWS-SIZE
               MOVE 6                      TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               MOVE HRWS-VALUE             TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-HM-DIST-CO-DN)
           AND (PADFP-EXP-COMPANY NOT = ZEROES)
               MOVE PADFP-EXP-COMPANY      TO HRWS-DEC-FIELD
               MOVE 0                      TO HRWS-SIZE
               MOVE 0                      TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               MOVE HRWS-VALUE             TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-HM-ACCT-UNIT-DN)
               MOVE PADFP-EXP-ACCT-UNIT    TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-HM-ACCOUNT-DN)
           AND (PADFP-EXP-ACCOUNT NOT = ZEROES)
               MOVE PADFP-EXP-ACCOUNT      TO HRWS-DEC-FIELD
               MOVE 0                      TO HRWS-SIZE
               MOVE 0                      TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               MOVE HRWS-VALUE             TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-HM-SUB-ACCT-DN)
           AND (PADFP-EXP-SUB-ACCT NOT = ZEROES)
               MOVE PADFP-EXP-SUB-ACCT     TO HRWS-DEC-FIELD
               MOVE 0                      TO HRWS-SIZE
               MOVE 0                      TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               MOVE HRWS-VALUE             TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PAY-RATE-DN)
           AND (PADFP-PAY-RATE NOT = ZEROES)
               MOVE PADFP-PAY-RATE         TO HRWS-DEC-FIELD
               MOVE 0                      TO HRWS-SIZE
               MOVE 4                      TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               MOVE HRWS-VALUE             TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PAY-FREQUENCY-DN)
           AND (PADFP-PAY-FREQUENCY NOT = ZEROES)
               MOVE PADFP-PAY-FREQUENCY    TO HRWS-DEC-FIELD
               MOVE 0                      TO HRWS-SIZE
               MOVE 0                      TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               MOVE HRWS-VALUE             TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-ACTIVITY-DN)
               MOVE PADFP-ACTIVITY         TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-ACCT-CATEGORY-DN)
               MOVE PADFP-ACCT-CATEGORY    TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-OT-PLAN-CODE-DN)
               MOVE PADFP-OT-PLAN-CODE     TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-CURRENCY-CODE-DN)
               MOVE PADFP-CURRENCY-CODE    TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-UNION-CODE-DN)
               MOVE PADFP-UNION-CODE       TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HRPEM-BARGAIN-UNIT-DN)
               MOVE PADFP-BARGAIN-UNIT     TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR    = HRPEM-ASSIGN-DATE-DN)
           AND (PADFP-ASSIGN-DATE NOT = ZEROES)
               IF  (PADFP-ASSIGN-DATE   = ZEROES)
               AND (PA52WS-PRE-VALUE       NOT = SPACES)
                   MOVE "*BLANK"               TO PA52WS-VALUE 
               ELSE
               IF  (PADFP-ASSIGN-DATE  NOT = ZEROES)
                   MOVE PADFP-ASSIGN-DATE      TO HRWS-DATE-8-FIELD
                   INITIALIZE                     HRWS-DATE-FIELD
                   PERFORM 781-HR-FORMAT-DATE-FIELD
                   IF (HRWS-VALUE NOT = PA52WS-PRE-VALUE)    
                       MOVE HRWS-VALUE             TO PA52WS-VALUE
                   END-IF
               END-IF
               END-IF
           END-IF.

           IF  (PA52WS-FLD-NBR    = HRPEM-USER-AMOUNT-DN)
           AND (PADFP-USER-AMOUNT NOT = ZEROES)
               IF  (PADFP-USER-AMOUNT   = ZEROES)
               AND (PA52WS-PRE-VALUE       NOT = SPACES)
                   MOVE "*BLANK"               TO PA52WS-VALUE 
               ELSE
               IF  (PADFP-USER-AMOUNT  NOT = ZEROES)
                   MOVE PADFP-USER-AMOUNT      TO HRWS-DEC-FIELD
                   MOVE 0                      TO HRWS-SIZE
                   MOVE 4                      TO HRWS-NBR-DECIMALS
                   PERFORM 770-HR-FORMAT-DEC-FIELD
                   IF (HRWS-VALUE NOT = PA52WS-PRE-VALUE)    
                       MOVE HRWS-VALUE             TO PA52WS-VALUE
                   END-IF
               END-IF
               END-IF
           END-IF.

       781-END.
      ******************************************************************
       850-DEFAULT-REQ.
      ******************************************************************

           MOVE PA52F4-PCT-COMPANY           TO DB-COMPANY.
           MOVE PA52F4-PJR-REQUISITION       TO DB-REQUISITION.
           MOVE 1                            TO DB-EMP-APP.
           MOVE PA52F4-PCT-APPLICANT         TO DB-APPLICANT.
           MOVE PRFSET1-APPLICANT            TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PRFSET1.
           MOVE WS-FALSE                     TO PA52WS-ACCEPT-SW.
           PERFORM
               UNTIL  (PAREQOFFER-NOTFOUND)
               OR     (PA52WS-ACCEPT-FOUND)
                   IF (PRF-OFFER-TYPE   = "A")
                       MOVE WS-TRUE          TO PA52WS-ACCEPT-SW
                   ELSE
                       PERFORM 860-FIND-NXTRNG-PRFSET1
                   END-IF
           END-PERFORM.

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 12)

               MOVE PA52F4-PCT-NEW-VALUE-1 (I1)    TO PA52WS-VALUE
               MOVE PA52F4-PAT-FLD-NBR-1 (I1)      TO PA52WS-FLD-NBR
               PERFORM 851-DEFAULT-REQ-FIELD
               THRU    851-END
               MOVE PA52WS-VALUE       TO PA52F4-PCT-NEW-VALUE-1 (I1)

               MOVE PA52F4-PCT-NEW-VALUE-2 (I1)    TO PA52WS-VALUE
               MOVE PA52F4-PAT-FLD-NBR-2 (I1)      TO PA52WS-FLD-NBR
               PERFORM 851-DEFAULT-REQ-FIELD
               THRU    851-END
               MOVE PA52WS-VALUE       TO PA52F4-PCT-NEW-VALUE-2 (I1)

               MOVE PA52F4-PCT-NEW-VALUE-3 (I1)    TO PA52WS-VALUE
               MOVE PA52F4-PAT-FLD-NBR-3 (I1)      TO PA52WS-FLD-NBR
               PERFORM 851-DEFAULT-REQ-FIELD
               THRU    851-END
               MOVE PA52WS-VALUE       TO PA52F4-PCT-NEW-VALUE-3 (I1)

           END-PERFORM.

       850-END.
514500******************************************************************
514600 851-DEFAULT-REQ-FIELD.
514700******************************************************************
514800
           IF  (PA52WS-VALUE NOT = SPACES)
               GO TO 851-END.

           IF  (PA52WS-POSITION = SPACES)
               GO TO 851-DEFAULT.

           IF  (PA52WS-FLD-NBR = HREMP-EMP-STATUS-DN)
               GO TO 851-DEFAULT.

           PERFORM
               VARYING I2 FROM 1 BY 1
               UNTIL  (I2 > PAPOSEMP-TABLE-SIZE)

               IF  (PA52WS-FLD-NBR = PAPOSEMP-EMP-FLD (I2))
                   MOVE PA52F4-PCT-COMPANY     TO DB-COMPANY
                   MOVE "PO"                   TO DB-TOPIC
                   MOVE PAPOSEMP-POS-FLD (I2)  TO DB-FLD-NBR
                   INITIALIZE                     DB-COUNTRY-CD-REQ
                   IF (PA52WS-USE-PROCESS-LEVEL)
                       MOVE POS-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
                   ELSE
                       MOVE SPACES             TO DB-PROCESS-LEVEL
                   END-IF
                   PERFORM 840-FIND-PASSET2
                   IF  (PASCRTY-NOTFOUND)
                       GO TO 851-END
                   END-IF
                   IF  (PAS-DEFAULT-FLG = 1)
                   OR  ((PA52WS-FLD-NBR = HREMP-PAY-RATE-DN)
                    AND (PAS-DEFAULT-FLG = 1 OR 2))
                       GO TO 851-DEFAULT
                   ELSE
                       GO TO 851-END
                   END-IF
               END-IF
           END-PERFORM.

       851-DEFAULT.
           PERFORM 852-DEFAULT-REQ-FIELD
           THRU    852-END.

       851-END.
      ******************************************************************
       852-DEFAULT-REQ-FIELD.
      ******************************************************************

           IF  (PA52WS-FLD-NBR = HREMP-JOB-CODE-DN)
               MOVE PJR-JOB-CODE       TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PROCESS-LEVEL-DN)
               MOVE PJR-PROCESS-LEVEL  TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-DEPARTMENT-DN)
               MOVE PJR-DEPARTMENT     TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-USER-LEVEL-DN)
               MOVE PJR-USER-LEVEL     TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HRPEM-LOCAT-CODE-DN)
               MOVE PJR-LOCAT-CODE     TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SUPERVISOR-DN)
               MOVE PJR-SUPERVISOR     TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SALARY-CLASS-DN)
               MOVE PJR-SALARY-CLASS   TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-EXEMPT-EMP-DN)
               MOVE PJR-EXEMPT-EMP     TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SCHEDULE-DN)
               MOVE PJR-SCHEDULE       TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PAY-GRADE-DN)
               MOVE PJR-PAY-GRADE      TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PAY-STEP-DN)
           AND (PJR-PAY-STEP NOT = ZEROES)
               MOVE PJR-PAY-STEP       TO HRWS-DEC-FIELD
               MOVE 0                  TO HRWS-SIZE
               MOVE 0                  TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               MOVE HRWS-VALUE         TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-WORK-SCHED-DN)
               MOVE PJR-WORK-SCHED     TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-SHIFT-DN)
           AND (PJR-SHIFT NOT = ZEROES)
               MOVE PJR-SHIFT          TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-EMP-STATUS-DN)
               MOVE PJR-APP-STATUS     TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-UNION-CODE-DN)
               MOVE PJR-UNION-CODE     TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HRPEM-BARGAIN-UNIT-DN)
               MOVE PJR-BARGAIN-UNIT   TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-NBR-FTE-DN)
           AND (PA52WS-ACCEPT-FOUND)
           AND (PRF-FTE NOT = ZEROES)
               MOVE PRF-FTE            TO HRWS-DEC-FIELD
               MOVE 0                  TO HRWS-SIZE
               MOVE 6                  TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               MOVE HRWS-VALUE         TO PA52WS-VALUE
           END-IF.

           IF  (PA52WS-FLD-NBR = HREMP-PAY-RATE-DN)
           AND (PA52WS-ACCEPT-FOUND)
           AND (PRF-PAY-RATE NOT = ZEROES)
               MOVE PRF-PAY-RATE       TO HRWS-DEC-FIELD
               MOVE 0                  TO HRWS-SIZE
               MOVE 4                  TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               MOVE HRWS-VALUE         TO PA52WS-VALUE
           END-IF.

       852-END.
535400******************************************************************
535500 PA52S4-TRANSACTION-END.
535600******************************************************************
535700******************************************************************
535800 PA52S5-TRANSACTION SECTION 30.
535900******************************************************************
536000 PA52S5-START.
536100
           MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY.
           MOVE PA52F5-PCT-ACTION-CODE TO DB-ACTION-CODE.
           PERFORM 840-FIND-PATSET1.
J31809     IF  (PERSACTYPE-FOUND)
J31809     AND (PAT-WORKFLOW-FLAG = "Y")
J31809         PERFORM 1000-OPEN-WORKFLOW-DB
J33429         IF  (PA52F5-PCT-ACTION-NBR-K NOT = ZEROES)
J33429             MOVE PA52F5-PCT-ACTION-NBR-K
J33429                                 TO PA52F5-PCT-ACTION-NBR
J33429         END-IF
J33429     END-IF.
J33429     MOVE ZEROES                 TO PA52F5-PCT-ACTION-NBR-K.
J31809*    IF (PA52F5-FC = "A" OR "C")
J31809*        PERFORM 1000-OPEN-WORKFLOW-DB.

536200     INITIALIZE                           CRT-MSG-NBR.
536300
           IF  (PA52F5-FC NOT = "A" AND "C" AND "R")
               INITIALIZE PA52F5-XMIT-DEDDAT
                          PA52F5-XMIT-REQDED
                          PA52F5-XMIT-HREMP-BLOCK
                          PA52F5-XMIT-PAPEP-BLOCK
                          PA52F5-XMIT-ACT-EXISTS
           END-IF.
           IF  (PA52F5-FC NOT = "A")
           OR  (PA52F5-IMMEDIATE-ACTION NOT = "Y")
               INITIALIZE PA52F5-XMIT-IMMED.
P40376     MOVE "PA52"                 TO PA52F5-PGM-NAME.

536400     IF (PA52F5-FC = "F")
536500         PERFORM 210-EDIT-ACCESS
536600         THRU    210-END
536700         IF (ERROR-FOUND)
536800             GO TO PA52S5-TRANSACTION-END
536900         ELSE
537000             PERFORM 650-DEFAULT
537100             THRU    650-END
537200             GO TO PA52S5-TRANSACTION-END.
537300
           MOVE 411                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO PA52WS-SEC-MESSAGE.

537900     PERFORM 200-EDIT-TRAN
538000     THRU    200-END.

J41791     MOVE CRT-ERROR-CAT        TO PA52WS-ERROR-CAT-SAVE.
J40356     MOVE SPACES               TO PA52F5-LTM-EXISTS
J40356                                  PA52F5-LTM-FLAG.
J40356     MOVE PA52F5-PCT-COMPANY   TO DB-COMPANY.
J40356     MOVE PA52F5-PCT-EMPLOYEE  TO DB-EMPLOYEE.
J40356     MOVE 1                    TO DB-STATUS.
J40356     MOVE LEMSET2-STATUS       TO WS-DB-BEG-RNG.
J40356     PERFORM 850-FIND-BEGRNG-LEMSET2.
J40356     IF (HREMPRCVR-FOUND)
J59105         MOVE 547              TO CRT-MSG-NBR
J59105         PERFORM 790-GET-MSG
J59105         MOVE CRT-MESSAGE      TO PA52F5-LTM-EXISTS
J40356         MOVE "*"              TO PA52F5-LTM-FLAG
J40356     END-IF.
J41791     MOVE PA52WS-ERROR-CAT-SAVE TO CRT-ERROR-CAT.
538100
           IF  (NO-ERROR-FOUND)
               PERFORM 400-PROCESS-TRAN
               THRU    400-END
               INITIALIZE PA52F5-XMIT-DEDDAT
                          PA52F5-XMIT-REQDED
                          PA52F5-XMIT-HREMP-BLOCK
                          PA52F5-XMIT-PAPEP-BLOCK
                          PA52F5-XMIT-IMMED
                          PA52F5-XMIT-ACT-EXISTS
           END-IF.
J38036
J38036     IF  (PA52F5-PCT-APPROVAL-FLAG = "L")
J38036     AND (CRT-ERROR-NBR = 145)
J38036         INITIALIZE CRT-ERROR-NBR
J38036                    CRT-FIELD-NBR
J38036         MOVE 412                TO CRT-MSG-NBR
J38036         PERFORM 790-GET-MSG
J38036     END-IF.

539200     GO TO PA52S5-TRANSACTION-END.
539300
539400******************************************************************
539500 200-EDIT-TRAN.
539600******************************************************************
539700
           IF (PA52F5-PCT-PROCESS-TYPE    = "5")
               IF (PA52F5-PCT-MOVE-FROM-LEVEL = ZEROES)
                   MOVE 151                TO CRT-ERROR-NBR
                   MOVE PA52F5-PCT-MOVE-FROM-LEVEL-FN
                                           TO CRT-FIELD-NBR
                   GO TO 200-END
               END-IF
               IF (PA52F5-PCT-MOVE-FROM-LEVEL = PA52F5-POS-LEVEL)
                   MOVE 152                TO CRT-ERROR-NBR
                   MOVE PA52F5-PCT-MOVE-FROM-LEVEL-FN
                                           TO CRT-FIELD-NBR
                   GO TO 200-END
               END-IF.
198255     IF (PA52F5-POS-LEVEL > 98)
198255*--- PA52#428: Accepts 01 - 98 values only
198255         MOVE 428                    TO CRT-ERROR-NBR
198255         MOVE PA52F5-POS-LEVEL-FN    TO CRT-FIELD-NBR
198255         GO TO 200-END
198255     END-IF.

542800     MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY.
542900     MOVE SPACES                 TO DB-PROCESS-LEVEL.
543000     PERFORM 840-FIND-PRSSET1.
543100     IF (PRSYSTEM-NOTFOUND)
543200         MOVE 100                                TO CRT-ERROR-NBR
543300         MOVE PA52F5-PCT-COMPANY-FN              TO CRT-FIELD-NBR
543400         GO TO 200-END
543500     ELSE
543600         MOVE PRS-NAME                TO PA52WS-PRS-NAME.
543800
           IF  (PA52F5-FC = "V")
           AND (PA52F5-PCT-MOVE-FROM-LEVEL NOT = ZEROES)
               PERFORM 230-EDIT-SPECIAL-PROCESSING
               THRU    230-END.
                   
           IF (ERROR-FOUND)
              GO TO 200-END.

539800     PERFORM 210-EDIT-ACCESS
539900     THRU    210-END.
540000
540100     IF (ERROR-FOUND)
540200        GO TO 200-END.

           PERFORM 201-BUILD-TABLE
           THRU    201-END.

           IF   (PA52F5-FC   NOT = "D" AND "I" AND "N" AND "P" AND "V")
           OR  ((PA52F5-FC   = "V")
           AND  (PA52F5-PCT-MOVE-FROM-LEVEL = ZEROES))
               IF (PA52F5-FC = "R")
               OR (PA52F5-PCT-PROCESS-TYPE   NOT = SPACES)
               OR (PA52F5-PCT-NEW-EFFECT-DATE NOT = ZEROES)
               OR (PA52F5-PCT-HIST-CORR-FLAG NOT = SPACES)
               OR (PA52F5-PCT-MERGE-ACTN-NBR NOT = ZEROES)
                   PERFORM 230-EDIT-SPECIAL-PROCESSING
                   THRU    230-END.

540100     IF (ERROR-FOUND)
540200         GO TO 200-END.
540300
540400     IF (PA52F5-FC = "A" OR "C")
540500         PERFORM 220-EDIT-DATA
540600         THRU    220-END
540700         GO TO 200-END.
540800
           IF (PA52F5-FC = "V")
               PERFORM 700-MOVE-LEVEL
               THRU    700-END.

540900 200-END.
541000
541100******************************************************************
541200 201-BUILD-TABLE.
541300******************************************************************

581400     MOVE PA52F5-PCT-NEW-END-DATE   TO PA52WS-PCT-NEW-VALUE5 (1).
P51466     MOVE PA52F5-PCT-NEW-DATE-ASSIGN
                                          TO PA52WS-PCT-NEW-VALUE5 (2).
582200     MOVE PA52F5-PCT-NEW-POSITION   TO PA52WS-PCT-NEW-VALUE5 (3).
583000     MOVE PA52F5-PCT-NEW-JOBCODE    TO PA52WS-PCT-NEW-VALUE5 (4).
583800     MOVE PA52F5-PCT-NEW-PL         TO PA52WS-PCT-NEW-VALUE5 (5).
584500     MOVE PA52F5-PCT-NEW-DEPARTMENT TO PA52WS-PCT-NEW-VALUE5 (6).
585300     MOVE PA52F5-PCT-NEW-USER-LEVEL TO PA52WS-PCT-NEW-VALUE5 (7).
586100     MOVE PA52F5-PCT-NEW-SUPERVISOR TO PA52WS-PCT-NEW-VALUE5 (8).
586900     MOVE PA52F5-PCT-NEW-IND-SUPER  TO PA52WS-PCT-NEW-VALUE5 (9).
587700     MOVE PA52F5-PCT-NEW-LOCATION   TO PA52WS-PCT-NEW-VALUE5 (10).
588500     MOVE PA52F5-PCT-NEW-FTE        TO PA52WS-PCT-NEW-VALUE5 (11).
589200     MOVE PA52F5-PCT-NEW-ANNUAL-HOURS
589300                                    TO PA52WS-PCT-NEW-VALUE5 (12).
590000     MOVE PA52F5-PCT-NEW-SALARY-CLASS
590100                                    TO PA52WS-PCT-NEW-VALUE5 (13).
590800     MOVE PA52F5-PCT-NEW-PAY-FREQ   TO PA52WS-PCT-NEW-VALUE5 (14).
591600     MOVE PA52F5-PCT-NEW-PAY-RATE   TO PA52WS-PCT-NEW-VALUE5 (15).
           MOVE PA52F5-PCT-NEW-CURRENCY   TO PA52WS-PCT-NEW-VALUE5 (16).
592400     MOVE PA52F5-PCT-NEW-EXEMPT     TO PA52WS-PCT-NEW-VALUE5 (17).
593100     MOVE PA52F5-PCT-NEW-PAY-PLAN   TO PA52WS-PCT-NEW-VALUE5 (18).
593900     MOVE PA52F5-PCT-NEW-UNION      TO PA52WS-PCT-NEW-VALUE5 (19).
           MOVE PA52F5-PCT-NEW-BARGAIN-UNIT
                                          TO PA52WS-PCT-NEW-VALUE5 (20).
594600     MOVE PA52F5-PCT-NEW-SCHEDULE   TO PA52WS-PCT-NEW-VALUE5 (21).
595400     MOVE PA52F5-PCT-NEW-GRADE      TO PA52WS-PCT-NEW-VALUE5 (22).
596100     MOVE PA52F5-PCT-NEW-STEP       TO PA52WS-PCT-NEW-VALUE5 (23).
597600     MOVE PA52F5-PCT-NEW-SHIFT      TO PA52WS-PCT-NEW-VALUE5 (24).
596800     MOVE PA52F5-PCT-NEW-WORK-SCHD  TO PA52WS-PCT-NEW-VALUE5 (25).
598300     MOVE PA52F5-PCT-NEW-SECURITY-LVL
598400                                    TO PA52WS-PCT-NEW-VALUE5 (26).
599100     MOVE PA52F5-PCT-NEW-SECURITY-LOC
599200                                    TO PA52WS-PCT-NEW-VALUE5 (27).
599900     MOVE PA52F5-PCT-NEW-EXP-COMPANY
600000                                    TO PA52WS-PCT-NEW-VALUE5 (28).
600700     MOVE PA52F5-PCT-NEW-ACCT-UNIT  TO PA52WS-PCT-NEW-VALUE5 (29).
601500     MOVE PA52F5-PCT-NEW-ACCOUNT    TO PA52WS-PCT-NEW-VALUE5 (30).
602300     MOVE PA52F5-PCT-NEW-SUBACCOUNT TO PA52WS-PCT-NEW-VALUE5 (31).
603100     MOVE PA52F5-PCT-NEW-ACTIVITY   TO PA52WS-PCT-NEW-VALUE5 (32).
603900     MOVE PA52F5-PCT-NEW-CATEGORY   TO PA52WS-PCT-NEW-VALUE5 (33).
           MOVE PA52F5-PCT-NEW-BASE-RATE-A
                                          TO PA52WS-PCT-NEW-VALUE5 (34).
           MOVE PA52F5-PCT-NEW-USER-AMOUNT
                                         TO PA52WS-PCT-NEW-VALUE5 (36).
           INITIALIZE                        PA52WS-PCT-NEW-VALUE5 (37).

           MOVE "*"                       TO PA52WS-PCT-STAR (1).
P51466     MOVE PA52F5-DATE-ASSIGN-STAR   TO PA52WS-PCT-STAR (2).
582200     MOVE PA52F5-POSITION-STAR      TO PA52WS-PCT-STAR (3).
583000     MOVE PA52F5-JOBCODE-STAR       TO PA52WS-PCT-STAR (4).
583800     MOVE PA52F5-PL-STAR            TO PA52WS-PCT-STAR (5).
584500     MOVE PA52F5-DEPARTMENT-STAR    TO PA52WS-PCT-STAR (6).
585300     MOVE PA52F5-USER-LEVEL-STAR    TO PA52WS-PCT-STAR (7).
586100     MOVE PA52F5-SUPERVISOR-STAR    TO PA52WS-PCT-STAR (8).
586900     MOVE PA52F5-IND-SUPER-STAR     TO PA52WS-PCT-STAR (9).
587700     MOVE PA52F5-LOCATION-STAR      TO PA52WS-PCT-STAR (10).
588500     MOVE PA52F5-FTE-STAR           TO PA52WS-PCT-STAR (11).
589200     MOVE PA52F5-ANNUAL-HOURS-STAR  TO PA52WS-PCT-STAR (12).
590000     MOVE PA52F5-SALARY-CLASS-STAR  TO PA52WS-PCT-STAR (13).
590800     MOVE PA52F5-PAY-FREQ-STAR      TO PA52WS-PCT-STAR (14).
591600     MOVE PA52F5-PAY-RATE-STAR      TO PA52WS-PCT-STAR (15).
           MOVE PA52F5-CURRENCY-STAR      TO PA52WS-PCT-STAR (16).
592400     MOVE PA52F5-EXEMPT-STAR        TO PA52WS-PCT-STAR (17).
593100     MOVE PA52F5-PAY-PLAN-STAR      TO PA52WS-PCT-STAR (18).
593900     MOVE PA52F5-UNION-STAR         TO PA52WS-PCT-STAR (19).
           MOVE PA52F5-BARGAIN-UNIT-STAR  TO PA52WS-PCT-STAR (20).
594600     MOVE PA52F5-SCHEDULE-STAR      TO PA52WS-PCT-STAR (21).
595400     MOVE PA52F5-GRADE-STAR         TO PA52WS-PCT-STAR (22).
596100     MOVE PA52F5-STEP-STAR          TO PA52WS-PCT-STAR (23).
597600     MOVE PA52F5-SHIFT-STAR         TO PA52WS-PCT-STAR (24).
596800     MOVE PA52F5-WORK-SCHD-STAR     TO PA52WS-PCT-STAR (25).
598300     MOVE PA52F5-SECURITY-LVL-STAR  TO PA52WS-PCT-STAR (26).
599100     MOVE PA52F5-SECURITY-LOC-STAR  TO PA52WS-PCT-STAR (27).
599900     MOVE PA52F5-COMPANY-STAR       TO PA52WS-PCT-STAR (28).
600700     MOVE PA52F5-ACCT-UNIT-STAR     TO PA52WS-PCT-STAR (29).
601500     MOVE PA52F5-ACCOUNT-STAR       TO PA52WS-PCT-STAR (30).
602300     MOVE PA52F5-SUBACCOUNT-STAR    TO PA52WS-PCT-STAR (31).
603100     MOVE PA52F5-ACTIVITY-STAR      TO PA52WS-PCT-STAR (32).
603900     MOVE PA52F5-CATEGORY-STAR      TO PA52WS-PCT-STAR (33).
           MOVE PA52F5-BASE-RATE-STAR     TO PA52WS-PCT-STAR (34).
           MOVE "*"                       TO PA52WS-PCT-STAR (35).
           MOVE PA52F5-USER-AMOUNT-STAR   TO PA52WS-PCT-STAR (36).

581400     MOVE PA52F5-PCT-END-DATE       TO PA52WS-PCT-PRE-VALUE (1).
P51466     MOVE PA52F5-PCT-DATE-ASSIGN    TO PA52WS-PCT-PRE-VALUE (2).
582200     MOVE PA52F5-PCT-POSITION       TO PA52WS-PCT-PRE-VALUE (3).
583000     MOVE PA52F5-PCT-JOBCODE        TO PA52WS-PCT-PRE-VALUE (4).
583800     MOVE PA52F5-PCT-PL             TO PA52WS-PCT-PRE-VALUE (5).
584500     MOVE PA52F5-PCT-DEPARTMENT     TO PA52WS-PCT-PRE-VALUE (6).
585300     MOVE PA52F5-PCT-USER-LEVEL     TO PA52WS-PCT-PRE-VALUE (7).
586100     MOVE PA52F5-PCT-SUPERVISOR     TO PA52WS-PCT-PRE-VALUE (8).
586900     MOVE PA52F5-PCT-IND-SUPER      TO PA52WS-PCT-PRE-VALUE (9).
587700     MOVE PA52F5-PCT-LOCATION       TO PA52WS-PCT-PRE-VALUE (10).
588500     MOVE PA52F5-PCT-FTE            TO PA52WS-PCT-PRE-VALUE (11).
589200     MOVE PA52F5-PCT-ANNUAL-HOURS   TO PA52WS-PCT-PRE-VALUE (12).
590000     MOVE PA52F5-PCT-SALARY-CLASS   TO PA52WS-PCT-PRE-VALUE (13).
590800     MOVE PA52F5-PCT-PAY-FREQ       TO PA52WS-PCT-PRE-VALUE (14).
591600     MOVE PA52F5-PCT-PAY-RATE       TO PA52WS-PCT-PRE-VALUE (15).
           MOVE PA52F5-PCT-CURRENCY-CODE  TO PA52WS-PCT-PRE-VALUE (16).
592400     MOVE PA52F5-PCT-EXEMPT         TO PA52WS-PCT-PRE-VALUE (17).
593100     MOVE PA52F5-PCT-PAY-PLAN       TO PA52WS-PCT-PRE-VALUE (18).
593900     MOVE PA52F5-PCT-UNION          TO PA52WS-PCT-PRE-VALUE (19).
           MOVE PA52F5-PCT-BARGAIN-UNIT   TO PA52WS-PCT-PRE-VALUE (20).
594600     MOVE PA52F5-PCT-SCHEDULE       TO PA52WS-PCT-PRE-VALUE (21).
595400     MOVE PA52F5-PCT-GRADE          TO PA52WS-PCT-PRE-VALUE (22).
596100     MOVE PA52F5-PCT-STEP           TO PA52WS-PCT-PRE-VALUE (23).
597600     MOVE PA52F5-PCT-SHIFT          TO PA52WS-PCT-PRE-VALUE (24).
596800     MOVE PA52F5-PCT-WORK-SCHD      TO PA52WS-PCT-PRE-VALUE (25).
598300     MOVE PA52F5-PCT-SECURITY-LVL   TO PA52WS-PCT-PRE-VALUE (26).
599100     MOVE PA52F5-PCT-SECURITY-LOC   TO PA52WS-PCT-PRE-VALUE (27).
599900     MOVE PA52F5-PCT-EXP-COMPANY    TO PA52WS-PCT-PRE-VALUE (28).
600700     MOVE PA52F5-PCT-ACCT-UNIT      TO PA52WS-PCT-PRE-VALUE (29).
601500     MOVE PA52F5-PCT-ACCOUNT        TO PA52WS-PCT-PRE-VALUE (30).
602300     MOVE PA52F5-PCT-SUBACCOUNT     TO PA52WS-PCT-PRE-VALUE (31).
603100     MOVE PA52F5-PCT-ACTIVITY       TO PA52WS-PCT-PRE-VALUE (32).
603900     MOVE PA52F5-PCT-CATEGORY       TO PA52WS-PCT-PRE-VALUE (33).
           MOVE PA52F5-PCT-BASE-PAY-RATE  TO PA52WS-PCT-PRE-VALUE (34).
           MOVE PA52F5-PCT-USER-AMOUNT    TO PA52WS-PCT-PRE-VALUE (36).

605600     MOVE PA52F5-PCT-NEW-END-DATE-FN
605700                                 TO PA52WS-PCT-NEW-VALUE5-FN (1).
P51466     MOVE PA52F5-PCT-NEW-DATE-ASSIGN-FN
                                       TO PA52WS-PCT-NEW-VALUE5-FN (2).
605800     MOVE PA52F5-PCT-NEW-POSITION-FN
605900                                 TO PA52WS-PCT-NEW-VALUE5-FN (3).
606000     MOVE PA52F5-PCT-NEW-JOBCODE-FN
606100                                 TO PA52WS-PCT-NEW-VALUE5-FN (4).
606200     MOVE PA52F5-PCT-NEW-PL-FN   TO PA52WS-PCT-NEW-VALUE5-FN (5).
606400     MOVE PA52F5-PCT-NEW-DEPARTMENT-FN
606600                                 TO PA52WS-PCT-NEW-VALUE5-FN (6).
606700     MOVE PA52F5-PCT-NEW-USER-LEVEL-FN
606800                                 TO PA52WS-PCT-NEW-VALUE5-FN (7).
606900     MOVE PA52F5-PCT-NEW-SUPERVISOR-FN
607000                                 TO PA52WS-PCT-NEW-VALUE5-FN (8).
607100     MOVE PA52F5-PCT-NEW-IND-SUPER-FN
607200                                 TO PA52WS-PCT-NEW-VALUE5-FN (9).
607300     MOVE PA52F5-PCT-NEW-LOCATION-FN
607400                                 TO PA52WS-PCT-NEW-VALUE5-FN (10).
607500     MOVE PA52F5-PCT-NEW-FTE-FN  TO PA52WS-PCT-NEW-VALUE5-FN (11).
607700     MOVE PA52F5-PCT-NEW-ANNUAL-HOURS-FN
607800                                 TO PA52WS-PCT-NEW-VALUE5-FN (12).
607900     MOVE PA52F5-PCT-NEW-SALARY-CLASS-FN
608000                                 TO PA52WS-PCT-NEW-VALUE5-FN (13).
608100     MOVE PA52F5-PCT-NEW-PAY-FREQ-FN
608200                                 TO PA52WS-PCT-NEW-VALUE5-FN (14).
608300     MOVE PA52F5-PCT-NEW-PAY-RATE-FN
608400                                 TO PA52WS-PCT-NEW-VALUE5-FN (15).
           MOVE PA52F5-PCT-NEW-CURRENCY-FN
612100                                 TO PA52WS-PCT-NEW-VALUE5-FN (16).
608500     MOVE PA52F5-PCT-NEW-EXEMPT-FN
608600                                 TO PA52WS-PCT-NEW-VALUE5-FN (17).
608700     MOVE PA52F5-PCT-NEW-PAY-PLAN-FN
608800                                 TO PA52WS-PCT-NEW-VALUE5-FN (18).
608900     MOVE PA52F5-PCT-NEW-UNION-FN
609000                                 TO PA52WS-PCT-NEW-VALUE5-FN (19).
           MOVE PA52F5-PCT-BARGAIN-UNIT-FN
                                       TO PA52WS-PCT-NEW-VALUE5-FN (20).
609100     MOVE PA52F5-PCT-NEW-SCHEDULE-FN
609200                                 TO PA52WS-PCT-NEW-VALUE5-FN (21).
609300     MOVE PA52F5-PCT-NEW-GRADE-FN
609400                                 TO PA52WS-PCT-NEW-VALUE5-FN (22).
609500     MOVE PA52F5-PCT-NEW-STEP-FN TO PA52WS-PCT-NEW-VALUE5-FN (23).
609900     MOVE PA52F5-PCT-NEW-SHIFT-FN
610000                                 TO PA52WS-PCT-NEW-VALUE5-FN (24).
609700     MOVE PA52F5-PCT-NEW-WORK-SCHD-FN
609800                                 TO PA52WS-PCT-NEW-VALUE5-FN (25).
610100     MOVE PA52F5-PCT-NEW-SECURITY-LVL-FN
610200                                 TO PA52WS-PCT-NEW-VALUE5-FN (26).
610300     MOVE PA52F5-PCT-NEW-SECURITY-LOC-FN
610400                                 TO PA52WS-PCT-NEW-VALUE5-FN (27).
610500     MOVE PA52F5-PCT-NEW-EXP-COMPANY-FN
610600                                 TO PA52WS-PCT-NEW-VALUE5-FN (28).
610700     MOVE PA52F5-PCT-NEW-ACCT-UNIT-FN
610800                                 TO PA52WS-PCT-NEW-VALUE5-FN (29).
610900     MOVE PA52F5-PCT-NEW-ACCOUNT-FN
611000                                 TO PA52WS-PCT-NEW-VALUE5-FN (30).
611100     MOVE PA52F5-PCT-NEW-SUBACCOUNT-FN
611200                                 TO PA52WS-PCT-NEW-VALUE5-FN (31).
611300     MOVE PA52F5-PCT-NEW-ACTIVITY-FN
611400                                 TO PA52WS-PCT-NEW-VALUE5-FN (32).
611500     MOVE PA52F5-PCT-NEW-CATEGORY-FN
611600                                 TO PA52WS-PCT-NEW-VALUE5-FN (33).
           MOVE PA52F5-PCT-ENTERED-BASE-FN
                                       TO PA52WS-PCT-NEW-VALUE5-FN (34).
           MOVE PA52F5-PCT-USER-AMOUNT-FN
                                       TO PA52WS-PCT-NEW-VALUE5-FN (36).

       201-END.
541100******************************************************************
541200 210-EDIT-ACCESS.
541300******************************************************************
541400
           INITIALIZE                  HREMP-UPDPEP-DATE.
004500     INITIALIZE                     DB-COMPANY
004600                                    DB-PROCESS-LEVEL
004700                                    DB-DEPARTMENT
004800                                    DB-EFFECT-DATE.
004900     PERFORM 850-FIND-NLT-PPRSET1.
005000     IF (PAPOSRULE-FOUND)
005100         MOVE WS-TRUE            TO HREMP-PAUSER-FLAG-SW.

541500     MOVE "L"                    TO PA52F5-PT-ACTION-TYPE.
541600
542000     IF   (PA52F5-FC              = "A")
P49898     AND ((PA52F5-PCT-COMPANY     NOT = PA52F5-ORIG-COMPANY)
P49898     OR   (PA52F5-PCT-ACTION-CODE NOT =
P49898                                       PA52F5-ORIG-ACTION-CODE)
P49898     OR  (PA52F5-PCT-EMPLOYEE    NOT = PA52F5-ORIG-EMPLOYEE)
P49898     OR  (PA52F5-POS-LEVEL       NOT = PA52F5-ORIG-POS-LEVEL)
P49898     OR  (PA52F5-PCT-EFFECT-DATE NOT = PA52F5-ORIG-EFFECT-DATE))
542400         MOVE 10                                 TO CRT-ERROR-NBR
542500         MOVE PA52F5-FC-FN                       TO CRT-FIELD-NBR
542600         GO TO 210-END.
542700
542000     IF   (PA52F5-FC              = "F" OR "V")
542100     AND ((PA52F5-PCT-COMPANY     NOT = PA52F5-PT-PCT-COMPANY)
542200     OR   (PA52F5-PCT-ACTION-CODE NOT =
542300                                       PA52F5-PT-PCT-ACTION-CODE))
542400         MOVE 52                                 TO CRT-ERROR-NBR
542500         MOVE PA52F5-FC-FN                       TO CRT-FIELD-NBR
542600         GO TO 210-END.
542700
544400     IF (PA52F5-FC = "A" OR "C" OR "I" OR "R")
544500         MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY
544600         MOVE PA52F5-PCT-ACTION-CODE TO DB-ACTION-CODE
544700         PERFORM 840-FIND-PATSET1
544800         IF (PERSACTYPE-NOTFOUND)
544900             MOVE 110                            TO CRT-ERROR-NBR
545000             MOVE PA52F5-PCT-ACTION-CODE-FN      TO CRT-FIELD-NBR
545100             GO TO 210-END
               END-IF
               IF (PAT-ACTIVE-FLAG = "2")
                   MOVE 222                       TO CRT-ERROR-NBR
                   MOVE PAT-ACTION-CODE           TO CRT-ERR-VAR1
                   MOVE PA52F5-PCT-ACTION-CODE-FN TO CRT-FIELD-NBR
                   GO TO 210-END
               END-IF
550400         MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY
550500         MOVE PA52F5-PCT-EMPLOYEE    TO DB-EMPLOYEE
550600         PERFORM 840-FIND-PEMSET1
550700         PERFORM 840-FIND-EMPSET1
550800         IF (PAEMPLOYEE-NOTFOUND)
550900         OR (EMPLOYEE-NOTFOUND)
551000             MOVE 130                            TO CRT-ERROR-NBR
551100             MOVE PA52F5-PCT-EMPLOYEE-FN         TO CRT-FIELD-NBR
551200             GO TO 210-END
551300         ELSE
551400             MOVE EMP-SALARY-CLASS   TO PA52WS-SALARY-CLASS
551500             MOVE EMP-NBR-FTE        TO PA52WS-FTE
551600             MOVE EMP-TIPPED         TO PA52WS-TIPPED
                   MOVE PA52F5-POS-LEVEL   TO DB-POS-LEVEL
P58592             MOVE PEPSET3-POS-LEVEL  TO WS-DB-BEG-RNG
P58592             PERFORM 850-FIND-BEGRNG-PEPSET3
                   IF  (PAEMPPOS-FOUND)
551700                 MOVE EMP-COMPANY        TO CRT-COMPANY
551800                 MOVE PEP-PROCESS-LEVEL  TO CRT-PROCESS-LEVEL
551900                 PERFORM 700-HR-EMP-SECURITY
552000                 IF (HRWS-EMP-SECURED)
552100                     MOVE 244                    TO CRT-ERROR-NBR
552200                     MOVE PA52F5-PCT-EMPLOYEE-FN TO CRT-FIELD-NBR
552300                     GO TO 210-END
                       END-IF
                   END-IF.
P53742
P47809     IF (PA52F5-FC = "A")
P47809        MOVE PA52F5-PCT-COMPANY             TO DB-COMPANY
P47809        MOVE PA52F5-PCT-EMPLOYEE            TO DB-EMPLOYEE
P47809        MOVE PA52F5-POS-LEVEL               TO DB-POS-LEVEL
P58592        MOVE PEPSET3-POS-LEVEL              TO WS-DB-BEG-RNG
P58592        PERFORM 850-FIND-BEGRNG-PEPSET3
P47809        IF  (PAEMPPOS-FOUND)
P47809            IF (PEP-END-DATE = ZEROES)
P47809               INITIALIZE                     PA52F5-PCT-END-DATE
P47809            ELSE
P47809               MOVE PEP-END-DATE            TO HRWS-DATE-8-FIELD
P47809               PERFORM 781-HR-FORMAT-DATE-FIELD
P47809               MOVE HRWS-VALUE              TO PA52F5-PCT-END-DATE
P47809            END-IF
P47809            PERFORM 484-MOVE-CURRENT-DATA
P47809            THRU    484-END
P47809            VARYING I1 FROM 2 BY 1
P47809            UNTIL  (I1 > 37)
P47809        END-IF
P47809     END-IF.
552400
552500     MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY.
552600     MOVE "L"                    TO DB-ACTION-TYPE.
552700     MOVE PA52F5-PCT-EMPLOYEE    TO DB-EMPLOYEE.
552800     MOVE PA52F5-PCT-ACTION-CODE TO DB-ACTION-CODE.
552900     MOVE PA52F5-PCT-EFFECT-DATE TO DB-EFFECT-DATE.
553000     MOVE PA52F5-PCT-ACTION-NBR  TO DB-ACTION-NBR.
553100
553200     IF (PA52F5-FC = "N")
553300         IF (DB-EFFECT-DATE = ZEROES)
553400             MOVE WS-HIGH-VALUES TO DB-ACTION-NBR
553500             PERFORM 850-FIND-NLT-PCTSET2
553600         ELSE
                   PERFORM 850-FIND-NLT-PCTSET2
                   IF  (PCT-COMPANY     = DB-COMPANY)
                   AND (PCT-ACTION-TYPE = DB-ACTION-TYPE)
                   AND (PCT-EMPLOYEE    = DB-EMPLOYEE)
                   AND (PCT-ACTION-CODE = DB-ACTION-CODE)
                   AND (PCT-EFFECT-DATE = DB-EFFECT-DATE)
                   AND (PCT-ACTION-NBR  = DB-ACTION-NBR)
176100                 PERFORM 860-FIND-NEXT-PCTSET2
                   END-IF
553800         END-IF
553900     ELSE
554000     IF (PA52F5-FC = "P")
554100         PERFORM 850-FIND-NLT-PCTSET2
554200         PERFORM 870-FIND-PREV-PCTSET2.
554300
015100     IF  (PA52F5-FC       = "N" OR "P")
           AND (PCT-COMPANY     = DB-COMPANY)
           AND (PCT-ACTION-TYPE = DB-ACTION-TYPE)
               IF (PCT-EMPLOYEE      NOT = EMP-EMPLOYEE)
               OR (CRT-PROCESS-LEVEL NOT = EMP-PROCESS-LEVEL)
                   MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE
015500             PERFORM 850-FIND-NLT-EMPSET1
016200             MOVE EMP-COMPANY            TO CRT-COMPANY
016300             MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL
016400             PERFORM 700-HR-EMP-SECURITY
016500             IF (HRWS-EMP-SECURED)
                       PERFORM 240-GET-NEXT-EMPLOYEE
                       THRU    240-END
                       UNTIL (EMPLOYEE-NOTFOUND)
P51669                 OR    (PCT-COMPANY     NOT = DB-COMPANY)
                       OR    (HRWS-EMP-NOT-SECURED)
                   END-IF
               END-IF.
      
554400     IF (PA52F5-FC = "N" OR "P")
554500         IF (PERSACTION-NOTFOUND)
554600         OR (PCT-COMPANY     NOT = DB-COMPANY)
554700         OR (PCT-ACTION-TYPE NOT = DB-ACTION-TYPE)
554800             MOVE 12                     TO CRT-ERROR-NBR
554900             MOVE PA52F5-FC-FN           TO CRT-FIELD-NBR
555000             GO TO 210-END
555100         ELSE
                   PERFORM 840-FIND-PEMSET1
555200             MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE
555300                                            PA52F5-PCT-EMPLOYEE
555400             MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE
555500                                            PA52F5-PCT-ACTION-CODE
555600             MOVE PCT-EFFECT-DATE        TO DB-EFFECT-DATE
555700                                           PA52F5-PCT-EFFECT-DATE
555800             MOVE PCT-ACTION-NBR         TO DB-ACTION-NBR
555900                                            PA52F5-PCT-ACTION-NBR
556000             MOVE PCT-POS-LEVEL          TO PA52F5-POS-LEVEL
556100             PERFORM 840-FIND-PATSET1
556200             IF (PERSACTYPE-NOTFOUND)
556300                 MOVE 110                TO CRT-ERROR-NBR
556400                 MOVE PA52F5-PCT-ACTION-CODE-FN
556500                                         TO CRT-FIELD-NBR

556600                 GO TO 210-END.
556700
556800     IF (PA52F5-FC = "N" OR "P")
556900         MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY
557000         MOVE PA52F5-PCT-EMPLOYEE    TO DB-EMPLOYEE
557100         PERFORM 840-FIND-PEMSET1
557200         PERFORM 840-FIND-EMPSET1
557300         IF (PAEMPLOYEE-NOTFOUND)
557400         OR (EMPLOYEE-NOTFOUND)
557500             MOVE 130                            TO CRT-ERROR-NBR
557600             MOVE PA52F5-PCT-EMPLOYEE-FN         TO CRT-FIELD-NBR
557700             GO TO 210-END
557800         ELSE
                   MOVE PA52F5-POS-LEVEL   TO DB-POS-LEVEL
P58592             MOVE PEPSET3-POS-LEVEL  TO WS-DB-BEG-RNG
P58592             PERFORM 850-FIND-BEGRNG-PEPSET3
                   IF  (PAEMPPOS-FOUND)
557900                 MOVE EMP-COMPANY        TO CRT-COMPANY
558000                 MOVE PEP-PROCESS-LEVEL  TO CRT-PROCESS-LEVEL
558100                 PERFORM 700-HR-EMP-SECURITY
558200                 IF (HRWS-EMP-SECURED)
558300                     MOVE 244                    TO CRT-ERROR-NBR
558400                     MOVE PA52F5-PCT-EMPLOYEE-FN TO CRT-FIELD-NBR
558500                     GO TO 210-END
                       END-IF
                   END-IF
               END-IF
               PERFORM 8400-AFTER-FIND-PCT
558800         GO TO 210-END.
558900
           PERFORM 840-FIND-PCTSET2.

           IF  (PA52F5-FC = "I")
           AND ((PERSACTION-NOTFOUND)
            OR  (PCT-POS-LEVEL   NOT = PA52F5-POS-LEVEL))
               MOVE PCTSET2-EFFECT-DATE TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PCTSET2
               PERFORM 860-FIND-NXTRNG-PCTSET2
                   UNTIL (PERSACTION-NOTFOUND)
                   OR    (PCT-POS-LEVEL = PA52F5-POS-LEVEL)
           END-IF.

           IF  (PA52F5-FC = "C" OR "D")
           AND (PERSACTION-NOTFOUND)
               MOVE 125                                TO CRT-ERROR-NBR
               MOVE PA52F5-PCT-ACTION-CODE-FN          TO CRT-FIELD-NBR
               GO TO 210-END.

562700     IF  (PA52F5-FC               = "A")
562800     AND (PA52F5-PCT-EFFECT-DATE  = ZEROES)
562900         IF (PA52F5-IMMEDIATE-ACTION = "N")
563000             MOVE 102                            TO CRT-ERROR-NBR
563100             MOVE PA52F5-IMMEDIATE-ACTION-FN     TO CRT-FIELD-NBR
563200             GO TO 210-END
563300         END-IF
563600         MOVE WS-SYSTEM-DATE-YMD TO PA52F5-PCT-EFFECT-DATE
P68470                                    PA52F5-ORIG-EFFECT-DATE
563700         IF (PA52F5-IMMEDIATE-ACTION = SPACES)
563800             MOVE "Y"            TO PA52F5-IMMEDIATE-ACTION.
563900
564000     IF (PA52F5-FC               = "A")
564100         IF (PA52F5-IMMEDIATE-ACTION = SPACES)
564200             MOVE "N"            TO PA52F5-IMMEDIATE-ACTION
564300         END-IF
564500         IF  (PA52F5-PCT-EFFECT-DATE  > WS-SYSTEM-DATE-YMD)
564600         AND (PA52F5-IMMEDIATE-ACTION = "Y")
564700             MOVE 101                            TO CRT-ERROR-NBR
564800             MOVE PA52F5-IMMEDIATE-ACTION-FN     TO CRT-FIELD-NBR
564900             GO TO 210-END.
565000
           PERFORM 8400-AFTER-FIND-PCT.

           IF  (PA52F5-FC = "F" OR "C" OR "A" OR "R")
545300     AND (PA52F5-PCT-NEW-POSITION  NOT = SPACES)
545400     AND (PA52F5-PCT-NEW-POSITION  NOT = "*BLANK")
545500         MOVE PA52F5-PCT-COMPANY         TO PAPOS-COMPANY
545600         MOVE PA52F5-PCT-NEW-POSITION    TO PAPOS-POSITION
545700         MOVE PA52F5-PCT-EFFECT-DATE     TO PAPOS-EFFECT-DATE
549100         MOVE 1                          TO ERROR-FLAG-SW
549200         PERFORM 2000-EDIT-POSITION-DATES
549300         IF (ERROR-FOUND)
                   IF  (CRT-ERROR-NBR = 103)
                       MOVE PA52F5-PCT-NEW-END-DATE-FN TO CRT-FIELD-NBR
                   ELSE
                       MOVE PA52F5-PCT-NEW-POSITION-FN TO CRT-FIELD-NBR
                   END-IF
549400             GO TO 210-END
549500         ELSE

549600             MOVE "PA52"                 TO CRT-ERROR-CAT
549700             IF (PAPOS-STATUS > 2)
549800                 MOVE 103                TO CRT-ERROR-NBR
549900                 MOVE PA52F5-PCT-NEW-POSITION-FN
550000                                         TO CRT-FIELD-NBR
550100                 GO TO 210-END.
550200
565100 210-END.
565200
565300******************************************************************
565400 220-EDIT-DATA.
565500******************************************************************
565600
565700     MOVE "PA52 "                             TO CRT-ERROR-CAT.
565800
           IF  (PA52F5-FC              = "A")
           AND (PA52F5-XMIT-ACT-EXISTS  = ZEROS)
               MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY
               MOVE PA52F5-PCT-ACTION-CODE TO DB-ACTION-CODE
               MOVE "L"                    TO DB-ACTION-TYPE
               MOVE PA52F5-PCT-EFFECT-DATE TO DB-EFFECT-DATE
               MOVE PA52F5-PCT-EMPLOYEE    TO DB-EMPLOYEE
               MOVE PCTSET3-EMPLOYEE       TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PCTSET3
               MOVE WS-FALSE               TO PA52WS-PCT-FOUND-SW
               PERFORM
                 UNTIL (PERSACTION-NOTFOUND)
                 OR    (PA52WS-PCT-FOUND)
                       IF (PA52F5-POS-LEVEL = PCT-POS-LEVEL)
                          MOVE WS-TRUE TO PA52WS-PCT-FOUND-SW
                       ELSE
                          PERFORM 860-FIND-NXTRNG-PCTSET3
                       END-IF
               END-PERFORM
               IF (PA52WS-PCT-FOUND)
      ******** Warning, ADDING DUPLICATE RECORD.
                  MOVE 1                   TO PA52F5-XMIT-ACT-EXISTS
                  MOVE 261                 TO CRT-ERROR-NBR
                  GO TO 220-END
               END-IF
           END-IF.

           IF  (PA52F5-FC       = "C" OR "D")
           AND (PCT-POS-LEVEL   NOT = PA52F5-POS-LEVEL)
               MOVE 150                            TO CRT-ERROR-NBR
               MOVE PA52F5-POS-LEVEL-FN            TO CRT-FIELD-NBR
               GO TO 220-END.

565900     IF  (PA52F5-FC               = "C")
566000     AND (PA52F5-IMMEDIATE-ACTION = "Y")
566100         MOVE 108                                TO CRT-ERROR-NBR
566200         MOVE PA52F5-IMMEDIATE-ACTION-FN         TO CRT-FIELD-NBR
566300         GO TO 220-END.
566400
           IF  (PA52F5-POS-LEVEL          = 1)
           AND (PA52F5-PCT-ENTERED-BASE NOT = SPACES)
               MOVE 192                TO CRT-ERROR-NBR
               IF (I1 < 13)
                   MOVE PA52F5-PCT-ENTERED-BASE-FN     TO CRT-FIELD-NBR
               ELSE
               IF (I1 > 24)
                   COMPUTE I8 = (I1 - 24)
                   MOVE PA52F5-PCT-ENTERED-BASE-FN     TO CRT-FIELD-NBR
               ELSE
               IF (I1 > 12)
                   COMPUTE I8 = (I1 - 12)
                   MOVE PA52F5-PCT-ENTERED-BASE-FN     TO CRT-FIELD-NBR
               END-IF
               END-IF
               END-IF
               GO TO 220-END.

J71111     IF (PA52F5-PCT-APPROVAL-FLAG NOT = "L")
J83777         MOVE PCT-APPROVAL-FLAG      TO PA52F5-PCT-APPROVAL-FLAG
J71111     END-IF.
J83777
           IF  (PA52F5-FC                = "C")
           AND (PAT-WORKFLOW-FLAG        = "Y")
           AND (PA52F5-PCT-APPROVAL-FLAG = "Y")
      ******** Action has been approved thru workflow; Cannot change
022100         MOVE 191                                TO CRT-ERROR-NBR
022200         MOVE PA52F5-FC-FN                       TO CRT-FIELD-NBR
022300         GO TO 220-END.
022400
J13588*    MOVE PA52F5-PCT-COMPANY     TO EDCDWS-COMPANY.
J13588*    MOVE "LP"                   TO EDCDWS-SYSTEM.
J13588*    PERFORM 6000-IS-SYSTEM-TRIGGER-ENABLED.
J13588*    IF  (NOT EDCDWS-TRIGGER-ENABLED)
J13588*        IF (PA52F5-PCT-UPD-ABS-MGMT = "Y")
J13588*            MOVE 234            TO CRT-ERROR-NBR
J13588*            MOVE PA52F5-PCT-UPD-ABS-MGMT-FN
J13588*                                TO CRT-FIELD-NBR
J13588*            GO TO 220-END
J13588*        END-IF
J13588*        IF (PA52F5-PCT-UPD-ABS-MGMT = SPACES)
J13588*            MOVE "N"            TO PA52F5-PCT-UPD-ABS-MGMT
J13588*        END-IF.

566500     IF  (PA52F5-PCT-ANT-END-DATE NOT = ZEROS)
566600     AND (PA52F5-PCT-ANT-END-DATE NOT > PA52F5-PCT-EFFECT-DATE)
566700         MOVE 233                                TO CRT-ERROR-NBR
566800         MOVE PA52F5-PCT-ANT-END-DATE-FN         TO CRT-FIELD-NBR
566900         GO TO 220-END.
567000
110700     PERFORM 840-FIND-BNCSET1.
           IF  (BNCOMPANY-NOTFOUND)
           AND (PA52F5-PCT-UPDATE-BENEFIT  = "Y")
      ******** Cannot update benefit; Company is not setup in benefit system
028700         MOVE 180                            TO CRT-ERROR-NBR
028800         MOVE PA52F5-PCT-UPDATE-BENEFIT-FN   TO CRT-FIELD-NBR
028900         GO TO 220-END.

           IF (PA52F5-PCT-UPDATE-BENEFIT   NOT = "Y")
               IF (PA52F5-PCT-OCCUR-TYPE   NOT = SPACES)
      ************ Cannot enter occurence type; Update benefits is "N"
028700             MOVE 181                        TO CRT-ERROR-NBR
028800             MOVE PA52F5-PCT-OCCUR-TYPE-FN   TO CRT-FIELD-NBR
028900             GO TO 220-END
               END-IF

               IF (PA52F5-PCT-PARTICIPNT   NOT = ZEROES)
      ************ Cannot enter participant; Update benefits is "N"
028700             MOVE 182                        TO CRT-ERROR-NBR
028800             MOVE PA52F5-PCT-PARTICIPNT-FN   TO CRT-FIELD-NBR
028900             GO TO 220-END.

           IF (PA52F5-POS-LEVEL            NOT = 1)
               IF (PA52F5-PCT-UPDATE-REQ-DED   = "Y")
      ************ Update Required deduction = Yes only valid for pos level = 1
028700             MOVE 185                            TO CRT-ERROR-NBR
028800             MOVE PA52F5-PCT-UPDATE-REQ-DED-FN   TO CRT-FIELD-NBR
028900             GO TO 220-END
               END-IF

               IF (PA52F5-PCT-OCCUR-TYPE   NOT = SPACES)
      ************ Occurrence type valid only for pos level = 1
028700             MOVE 186                        TO CRT-ERROR-NBR
028800             MOVE PA52F5-PCT-OCCUR-TYPE-FN   TO CRT-FIELD-NBR
028900             GO TO 220-END
               END-IF

               IF (PA52F5-PCT-PARTICIPNT   NOT = ZEROES)
      ************ Participant valid only for pos level 1
028700             MOVE 187                        TO CRT-ERROR-NBR
028800             MOVE PA52F5-PCT-PARTICIPNT-FN   TO CRT-FIELD-NBR
028900             GO TO 220-END.

           IF (PA52F5-PCT-UPDATE-REQ-DED       = "Y")
               IF (PA52F5-PCT-EDM-EFFECT-DT    = ZEROES)
                   MOVE PA52F5-PCT-EFFECT-DATE TO
                                               PA52F5-PCT-EDM-EFFECT-DT
               END-IF
               IF (PA52F5-PCT-EDM-END-DATE     = ZEROES)
                   MOVE PA52F5-PCT-EDM-EFFECT-DT
                                               TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   SUBTRACT 1                  FROM WSDR-JULIAN-DAYS
                   PERFORM 900-JULIAN-TO-DATE
                   MOVE WSDR-FR-DATE           TO
                                               PA52F5-PCT-EDM-END-DATE.

           IF  ((PA52F5-FC                 = "A")
           OR  ((PA52F5-FC                 = "C")
           AND  ((PA52F5-PCT-EDM-EFFECT-DT NOT = PCT-EDM-EFFECT-DT)
           OR    (PA52F5-PCT-EDM-END-DATE  NOT = PCT-EDM-END-DATE)
           OR    (PA52F5-PCT-UPDATE-REQ-DED NOT = PCT-UPDATE-REQ-DED))))
               IF  (PA52F5-PCT-EDM-EFFECT-DT  = PA52F5-PCT-EDM-END-DATE)
038100         AND (PA52F5-XMIT-DEDDAT        = ZEROS)
               AND (PA52F5-POS-LEVEL          = 1)
053263         AND (PA52F5-PCT-UPDATE-REQ-DED = "Y")
      ******** Warning, Deductions dates equal, Press OK to continue
038200             MOVE 1                  TO PA52F5-XMIT-DEDDAT
038300             MOVE 183                TO CRT-ERROR-NBR
038400             GO TO 220-END
               END-IF
               IF  (PA52F5-PCT-EDM-EFFECT-DT  < PA52F5-PCT-EDM-END-DATE)
038100         AND (PA52F5-XMIT-DEDDAT        = ZEROS)
               AND (PA52F5-POS-LEVEL          = 1)
053263         AND (PA52F5-PCT-UPDATE-REQ-DED = "Y")
      ******** Warning, Deduction dates overlap, Press OK to continue
038200             MOVE 1                  TO PA52F5-XMIT-DEDDAT
038300             MOVE 184                TO CRT-ERROR-NBR
038400             GO TO 220-END.

           IF  (PA52F5-IMMEDIATE-ACTION    = "Y")
           AND (PAT-WORKFLOW-FLAG          = "Y")
      ******** Immediate action not allowed; Workflow approval required
038300         MOVE 174                            TO CRT-ERROR-NBR
               MOVE PA52F5-IMMEDIATE-ACTION-FN     TO CRT-FIELD-NBR
038400         GO TO 220-END.

022000     IF  (PA52F5-IMMEDIATE-ACTION    = "Y")
           AND (PA52F5-PCT-UPDATE-REQ-DED  = "Y")
467300         MOVE PA52F5-PCT-COMPANY       TO PRRQC-COMPANY
467400         MOVE PA52F5-PCT-EMPLOYEE      TO PRRQC-EMPLOYEE
467500         INITIALIZE PRRQC-DFT-MAR-STAT
467700                    PRRQC-DFT-EXEMPTS
               MOVE PA52F5-PCT-EDM-EFFECT-DT TO PRRQC-EFFECT-DATE
               MOVE PA52F5-PCT-EDM-END-DATE  TO PRRQC-END-DATE
               MOVE "N"                      TO PRRQC-UPDATE-OPTION
467800         PERFORM 500-REQ-DED-CREATION
               IF (ERROR-FOUND)
                   GO TO 220-END.
467900
567100     IF (PA52F5-PCT-PARTICIPNT NOT = ZEROS)
567200     OR (PA52F5-PCT-OCCUR-TYPE NOT = SPACES)
567400         IF (BNCOMPANY-NOTFOUND)
567500             MOVE 253                            TO CRT-ERROR-NBR
567600             IF (PA52F5-PCT-PARTICIPNT NOT = ZEROS)
567700                 MOVE PA52F5-PCT-PARTICIPNT-FN   TO CRT-FIELD-NBR
567800                 GO TO 220-END
567900             ELSE
568000                 MOVE PA52F5-PCT-OCCUR-TYPE-FN   TO CRT-FIELD-NBR
568100                 GO TO 220-END
568200         ELSE
568300             MOVE BNC-LAST-PART  TO PA52WS-LAST-PART.
568400
568500     MOVE ZEROS                  TO PA52WS-PARTICIPNT.
568600     IF  (PA52F5-PCT-OCCUR-TYPE NOT = SPACES)
568700     AND (BNC-AUTO-PART         = "Y")
568800     AND (PA52F5-FC             = "A")
568900         MOVE PA52WS-LAST-PART   TO DB-PARTICIPNT
569000                                    PA52WS-PARTICIPNT
569100         PERFORM 850-KFIND-NLT-PARSET1
569200         MOVE WS-FALSE           TO PA52WS-NUMBER-SW
569300         PERFORM 221-GET-PARTICIPNT-NUMBER
569400         THRU    221-END
569500             UNTIL (PA52WS-NUMBER-FOUND).
569600
569700     IF  (BNC-AUTO-PART         = "Y")
569800     AND (PA52F5-PCT-PARTICIPNT NOT = ZEROS)
569900         MOVE 254                                TO CRT-ERROR-NBR
570000         MOVE PA52F5-PCT-PARTICIPNT-FN           TO CRT-FIELD-NBR
570100         GO TO 220-END.
570200
570300     IF  (BNC-AUTO-PART         = "N")
570400     AND (PA52F5-PCT-OCCUR-TYPE NOT = SPACES)
570500     AND (PA52F5-PCT-PARTICIPNT = ZEROS)
570600         MOVE 255                                TO CRT-ERROR-NBR
570700         MOVE PA52F5-PCT-PARTICIPNT-FN           TO CRT-FIELD-NBR
570800         GO TO 220-END.
570900
571000     IF  (BNC-AUTO-PART         = "N")
571100     AND (PA52F5-PCT-PARTICIPNT NOT = ZEROS)
571200         MOVE PA52F5-PCT-PARTICIPNT TO DB-PARTICIPNT
571300         PERFORM 840-FIND-PARSET1
571400         IF (PARTICIPNT-FOUND)
571500             MOVE 258                            TO CRT-ERROR-NBR
571600             MOVE PA52F5-PCT-PARTICIPNT-FN       TO CRT-FIELD-NBR
571700             GO TO 220-END.
571800
571900     IF  (PA52F5-PCT-PARTICIPNT NOT = ZEROS)
572000     AND (PA52F5-PCT-OCCUR-TYPE = SPACES)
572100         MOVE 256                                TO CRT-ERROR-NBR
572200         MOVE PA52F5-PCT-OCCUR-TYPE-FN           TO CRT-FIELD-NBR
572300         GO TO 220-END.
572400
572500     IF (PA52F5-PCT-OCCUR-TYPE NOT = SPACES)
572600         MOVE PA52F5-PCT-OCCUR-TYPE  TO DB-OCCUR-TYPE
572700         PERFORM 840-FIND-OCCSET1
572800         IF (OCCURTYPE-NOTFOUND)
572900             MOVE 257                            TO CRT-ERROR-NBR
573000             MOVE PA52F5-PCT-OCCUR-TYPE-FN       TO CRT-FIELD-NBR
573100             GO TO 220-END
573200         ELSE
573300             MOVE OCC-MONTHS-EXT     TO PA52WS-OCC-MONTHS-EXT.
573400
312387     IF   (PA52F5-PCT-MOVE-FROM-LEVEL     = ZEROES)
312387     AND ((PA52F5-PCT-NEW-POSITION    NOT = SPACES)
574600     OR   (PA52F5-PCT-NEW-JOBCODE     NOT = SPACES)
574700     OR   (PA52F5-PCT-NEW-PL          NOT = SPACES)
574800     OR   (PA52F5-PCT-NEW-DEPARTMENT  NOT = SPACES)
574900     OR   (PA52F5-PCT-NEW-POSITION    = "*BLANK")
575000     OR   (PA52F5-PCT-NEW-JOBCODE     = "*BLANK")
575100     OR   (PA52F5-PCT-NEW-PL          = "*BLANK")
575200     OR   (PA52F5-PCT-NEW-DEPARTMENT  = "*BLANK"))
575300         MOVE PA52F5-PCT-COMPANY         TO DB-COMPANY
575400         MOVE PA52F5-PCT-EMPLOYEE        TO DB-EMPLOYEE
575500         MOVE SPACES                     TO DB-POSITION
575600         IF (PA52F5-PCT-NEW-POSITION = SPACES)
575700             MOVE PA52F5-PCT-POSITION    TO DB-POSITION
575800         ELSE
575900         IF (PA52F5-PCT-NEW-POSITION NOT = "*BLANK")
576000             MOVE PA52F5-PCT-NEW-POSITION
576100                                         TO DB-POSITION
576200         END-IF
576300         END-IF
576400         MOVE SPACES                     TO DB-JOB-CODE
576500         IF (PA52F5-PCT-NEW-JOBCODE = SPACES)
576600             MOVE PA52F5-PCT-JOBCODE     TO DB-JOB-CODE
576700         ELSE
576800         IF (PA52F5-PCT-NEW-JOBCODE NOT = "*BLANK")
576900             MOVE PA52F5-PCT-NEW-JOBCODE TO DB-JOB-CODE
577000         END-IF
577100         END-IF
577200         MOVE SPACES                     TO DB-PROCESS-LEVEL
577300         IF (PA52F5-PCT-NEW-PL = SPACES)
577400             MOVE PA52F5-PCT-PL          TO DB-PROCESS-LEVEL
577500         ELSE
577600         IF (PA52F5-PCT-NEW-PL NOT = "*BLANK")
577700             MOVE PA52F5-PCT-NEW-PL      TO DB-PROCESS-LEVEL
577800         END-IF
577900         END-IF
578000         MOVE SPACES                     TO DB-DEPARTMENT
578100         IF (PA52F5-PCT-NEW-DEPARTMENT = SPACES)
578200             MOVE PA52F5-PCT-DEPARTMENT   TO DB-DEPARTMENT
578300         ELSE
578400         IF (PA52F5-PCT-NEW-DEPARTMENT NOT = "*BLANK")
578500             MOVE PA52F5-PCT-NEW-DEPARTMENT
578600                                          TO DB-DEPARTMENT
578700         END-IF
578800         END-IF
578900         MOVE PEPSET5-DEPARTMENT         TO WS-DB-BEG-RNG
579000         PERFORM 850-FIND-BEGRNG-PEPSET5
579100         IF (PAEMPPOS-FOUND)
579200             IF (PA52F5-PCT-NEW-END-DATE = "*BLANK")
579300             OR (PA52F5-PCT-NEW-END-DATE = SPACES)
579400                 INITIALIZE                 PAPEP-END-DATE
579500             ELSE
579600                 MOVE PA52F5-PCT-NEW-END-DATE
579700                                         TO PAPCT-FIELD
579800                 PERFORM 5300-CONVERT-DATE
579900                 MOVE PAPCT-DATE-FORMAT  TO PAPEP-END-DATE
580000             END-IF
580100             PERFORM 223-CHECK-FOR-DUPS
580200             THRU    223-END
580300                 UNTIL (PAEMPPOS-NOTFOUND)
580400                 OR    (ERROR-FOUND)
580500             IF (ERROR-FOUND)
580600                 GO TO 220-END.
580700
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > PA52WS-5-FIELD-COUNT)
               IF  (PA52WS-PCT-NEW-VALUE5 (I1) = "*BLANK")
               AND (PA52WS-PCT-PRE-VALUE  (I1) = SPACES)
P81486             IF  (PA52F5-PCT-APPROVAL-FLAG = "L")             
P81486                  CONTINUE                                    
P81486             ELSE
                        MOVE 147               TO CRT-ERROR-NBR
                        MOVE PA52WS-PCT-NEW-VALUE5-FN (I1)  
                                               TO CRT-FIELD-NBR 
                        GO TO 220-END
P81486             END-IF
               END-IF
           END-PERFORM.

           IF  (PA52F5-FC NOT = "V")
           AND (PA52F5-PCT-MOVE-FROM-LEVEL NOT = ZEROES)
           AND (PA52WS-NO-MOVELEVELDONE)
               MOVE 158                        TO CRT-ERROR-NBR
               MOVE PA52F5-FC-FN               TO CRT-FIELD-NBR
               GO TO 220-END.

           IF (PAT-REQUIRE-REASON = 2)
               IF (PA52F5-PCT-REASON (1) = SPACES)
               OR (PA52F5-PCT-REASON (2) = SPACES)
                   MOVE 544                        TO CRT-ERROR-NBR
                   MOVE PAT-REQUIRE-REASON         TO CRT-ERR-VAR1
                   IF (PA52F5-PCT-REASON (2) = SPACES)
                       MOVE PA52F5-PCT-REASON-FN (2)   TO CRT-FIELD-NBR
                   END-IF
                   IF (PA52F5-PCT-REASON (1) = SPACES)
                       MOVE PA52F5-PCT-REASON-FN (1)   TO CRT-FIELD-NBR
                   END-IF
                   GO TO 220-END.

           IF (PAT-REQUIRE-REASON = 1)
               IF  (PA52F5-PCT-REASON (1) = SPACES)
               AND (PA52F5-PCT-REASON (2) = SPACES)
                   MOVE 544                        TO CRT-ERROR-NBR
                   MOVE PAT-REQUIRE-REASON         TO CRT-ERR-VAR1
                   IF (PA52F5-PCT-REASON (1) = SPACES)
                       MOVE PA52F5-PCT-REASON-FN (1)   TO CRT-FIELD-NBR
                   END-IF
                   GO TO 220-END.

           IF  (PA52F5-PCT-REASON (1) = PA52F5-PCT-REASON (2))
           AND (PA52F5-PCT-REASON (1) NOT = SPACES)
               MOVE 194                            TO CRT-ERROR-NBR
               MOVE PA52F5-PCT-REASON-FN (2)       TO CRT-FIELD-NBR
               GO TO 220-END.

029300     PERFORM 222-EDIT-REASONS
029400     THRU    222-END
029500         VARYING I1 FROM 1 BY 1
029600         UNTIL  (I1 > 2)
029700         OR     (ERROR-FOUND).
612700
612800     IF (ERROR-FOUND)
612900         GO TO 220-END.
613000
029900     IF (ERROR-FOUND)
030000         GO TO 220-END.
030100
           MOVE PA52F5-PCT-EDM-EFFECT-DT   TO PRPXL-PARM-EFF-DATE.
           MOVE PA52F5-PCT-EDM-END-DATE    TO PRPXL-PARM-END-DATE.

613100     INITIALIZE HREMP-SCR-FIELDS.
613200     INITIALIZE HRPEM-SCR-FIELDS.
613300
613400     MOVE "C"                    TO HREMP-FC.
613500     MOVE PA52F5-PCT-COMPANY     TO HREMP-COMPANY.
613600     MOVE PA52F5-PCT-COMPANY-FN  TO HREMP-COMPANY-FN.
613700     MOVE PA52F5-PCT-EMPLOYEE    TO HREMP-EMPLOYEE.
613800     MOVE PA52F5-PCT-EMPLOYEE-FN TO HREMP-EMPLOYEE-FN.
613900     MOVE PA52F5-IMMEDIATE-ACTION
614000                                 TO HREMP-IMM-ACTION.
614100     MOVE PA52F5-PCT-UPDATE-BENEFIT
614200                                 TO HREMP-UPDATE-BENEFIT.
614100     MOVE PA52F5-PCT-UPD-ABS-MGMT
614200                                 TO HREMP-UPDATE-ABSENCE-MGMT.
614300     MOVE "Y"                    TO HREMP-ACTION.
614400     MOVE "Y"                    TO HREMP-EDIT-VALUE-LIST.
614500     MOVE PA52F5-PCT-EFFECT-DATE TO HREMP-EFFECT-DATE.
614600     MOVE EMP-EMP-STATUS         TO HREMP-EMP-STATUS.
           MOVE PAT-HIST-CORR-FLAG     TO HREMP-HIST-OVERRIDE-SW.
           IF  (PA52F5-PCT-HIST-CORR-FLAG NOT = SPACES)
               MOVE PA52F5-PCT-HIST-CORR-FLAG
                                       TO HREMP-HIST-OVERRIDE-SW.
           MOVE PA52F5-PCT-PROCESS-TYPE TO HREMP-PROCESS-TYPE-SW.
614700
614800     SET PA52WS-NOTHING-ENTERED TO TRUE.
614900
           MOVE EMP-TERM-DATE          TO HREMP-TERM-DATE.

615000     PERFORM 224-EDIT-NEW-VALUES
615100     THRU    224-END
615200         VARYING I1 FROM 1 BY 1
615300         UNTIL  (I1 > PA52WS-5-FIELD-COUNT)
615400         OR     (ERROR-FOUND).
615500
615600     IF (ERROR-FOUND)
615700         GO TO 220-END.
615800
615900     IF (PA52WS-NOTHING-ENTERED)
616000         MOVE 145                                TO CRT-ERROR-NBR
616100         MOVE PA52F5-PCT-NEW-END-DATE-FN         TO CRT-FIELD-NBR
616200         GO TO 220-END.
616300
616400     MOVE PA52F5-POS-LEVEL       TO HREMP-POS-LEVEL.
616500
616600     IF  (PA52F5-PCT-NEW-POSITION = SPACES)
616700     AND (PA52F5-PCT-POSITION     NOT = SPACES)
616800         MOVE PA52F5-PCT-POSITION
616900                                 TO HREMP-POSITION.
617000
617100     IF  (PA52F5-PCT-NEW-PL = SPACES)
617200     AND (PA52F5-PCT-PL     NOT = SPACES)
617300         MOVE PA52F5-PCT-PL      TO HREMP-PROCESS-LEVEL.
617400
617500     IF  (PA52F5-PCT-NEW-DEPARTMENT = SPACES)
617600     AND (PA52F5-PCT-DEPARTMENT     NOT = SPACES)
617700         MOVE PA52F5-PCT-DEPARTMENT
617800                                 TO HREMP-DEPARTMENT.
617900
618000     IF  (PA52F5-PCT-NEW-JOBCODE   = SPACES)
618100     AND (PA52F5-PCT-JOBCODE       NOT = SPACES)
618200         MOVE PA52F5-PCT-JOBCODE TO HREMP-JOB-CODE.
618300
618400     IF  (PA52F5-POS-LEVEL = 01)
618500     AND (PA52WS-HREMP-FLDS-FOUND)
622300         INITIALIZE                     PA52WS-RETRO-POSITION
                                              PA52WS-POSITION
622400                                        PA52WS-RETRO-JOB-CODE
                                              PA52WS-JOB-CODE
622500                                        PA52WS-RETRO-PL
                                              PA52WS-PL
622600                                        PA52WS-RETRO-DEPARTMENT
                                              PA52WS-DEPARTMENT
                                              PA52WS-RETRO-SW
622700         IF  (PADT-UPDPEP-DATE NOT = ZEROES)
622900             MOVE PADT-UPDPEP-DATE   TO HREMP-UPDPEP-DATE
623600             MOVE PA52F5-PCT-COMPANY        TO DB-COMPANY
623700             MOVE PA52F5-PCT-EMPLOYEE       TO DB-EMPLOYEE
623800             MOVE PA52F5-POS-LEVEL          TO DB-POS-LEVEL
623900             MOVE PA52F5-PCT-EFFECT-DATE    TO DB-EFFECT-DATE
624000             PERFORM 850-FIND-NLT-PEPSET3
624100             IF  (PAEMPPOS-FOUND)
624200             AND (PEP-COMPANY   = DB-COMPANY)
624300             AND (PEP-EMPLOYEE  = DB-EMPLOYEE)
624400             AND (PEP-POS-LEVEL = DB-POS-LEVEL)
624500                 MOVE PEP-POSITION      TO PA52WS-RETRO-POSITION
624600                 MOVE PEP-JOB-CODE      TO PA52WS-RETRO-JOB-CODE
624700                 MOVE PEP-PROCESS-LEVEL TO PA52WS-RETRO-PL
624800                 MOVE PEP-DEPARTMENT    TO PA52WS-RETRO-DEPARTMENT
                       SET PA52WS-RETRO       TO TRUE
624900             END-IF
625000         END-IF
               MOVE PA52F5-PCT-COMPANY        TO DB-COMPANY
               MOVE PA52F5-PCT-EMPLOYEE       TO DB-EMPLOYEE
               MOVE PA52F5-POS-LEVEL          TO DB-POS-LEVEL
P58592         MOVE PEPSET3-POS-LEVEL         TO WS-DB-BEG-RNG
P58592         PERFORM 850-FIND-BEGRNG-PEPSET3
               IF  (PAEMPPOS-FOUND)
                   MOVE PEP-POSITION      TO PA52WS-POSITION
                   MOVE PEP-JOB-CODE      TO PA52WS-JOB-CODE
                   MOVE PEP-PROCESS-LEVEL TO PA52WS-PL
                   MOVE PEP-DEPARTMENT    TO PA52WS-DEPARTMENT
               END-IF
               MOVE PA52F5-XMIT-HREMP-BLOCK   TO HREMP-XMIT-BLOCK
               MOVE PA52F5-PCT-MOVE-FROM-LEVEL
                                              TO HREMP-POS-LEVEL-MOVE
625900         PERFORM 2000-HREMP-EDIT-TRAN
               MOVE HREMP-XMIT-BLOCK          TO PA52F5-XMIT-HREMP-BLOCK
626000         MOVE HREMP-PAPOS-EFFECT-DATE   TO PA52WS-POS-EFFECT-DATE
626100         IF  (HREMP-HM-ACCOUNT   = ZEROS)
626200         AND ((HREMP-HM-ACCT-UNIT NOT = SPACES)
626300          OR  (HREMP-HM-SUB-ACCT  NOT = ZEROS))
626400             MOVE SPACES TO HREMP-HM-ACCT-UNIT
626500             MOVE ZEROS  TO HREMP-HM-SUB-ACCT
626600         END-IF
626700         IF (HREMP-UPDPEP-DATE = PA52F5-PCT-EFFECT-DATE)
626800             INITIALIZE                 HREMP-UPDPEP-DATE
626900         END-IF
627300         IF (ERROR-FOUND)
627400             GO TO 220-END
               END-IF
           END-IF.
627500
627600     IF   (PA52F5-POS-LEVEL      > 01)
627700     OR  ((PA52F5-POS-LEVEL      = 01)
627800     AND  (PA52WS-HREMP-NO-FLDS-FOUND))
P49123     OR  ((PA52F5-POS-LEVEL      = 01)
P49123     AND  (PA52F5-PCT-NEW-PAY-RATE  NOT = SPACES))
627900         INITIALIZE                     PAPEP-UPDPEP-DATE
628000         MOVE WS-TRUE                TO PAPEP-FROM-ACTION-SW
628100         MOVE PA52F5-PCT-COMPANY     TO PADT-COMPANY
628200         MOVE PA52F5-PCT-EMPLOYEE    TO PADT-EMPLOYEE
628300         MOVE EMP-TERM-DATE          TO PADT-END-DATE
628400         MOVE PA52F5-POS-LEVEL       TO PADT-POS-LEVEL
628500         MOVE PA52F5-PCT-EFFECT-DATE TO PADT-EFFECT-DATE
628600         PERFORM 2300-PADT-DATE-CHECK
628700         IF (ERROR-FOUND)
628800             GO TO 220-END
628900         END-IF
629000         INITIALIZE                      PA52WS-RETRO-POSITION
                                               PA52WS-POSITION
629100                                         PA52WS-RETRO-JOB-CODE
                                               PA52WS-JOB-CODE
629200                                         PA52WS-RETRO-PL
                                               PA52WS-PL
629300                                         PA52WS-RETRO-DEPARTMENT
                                               PA52WS-DEPARTMENT
                                               PA52WS-RETRO-SW
629400         IF (PADT-UPDPEP-DATE NOT = ZEROES)
629500             MOVE PADT-UPDPEP-DATE    TO PAPEP-UPDPEP-DATE
630100             MOVE PA52F5-PCT-COMPANY        TO DB-COMPANY
630200             MOVE PA52F5-PCT-EMPLOYEE       TO DB-EMPLOYEE
630300             MOVE PA52F5-POS-LEVEL          TO DB-POS-LEVEL
630400             MOVE PA52F5-PCT-EFFECT-DATE    TO DB-EFFECT-DATE
630500             PERFORM 850-FIND-NLT-PEPSET3
630600             IF  (PAEMPPOS-FOUND)
630700             AND (PEP-COMPANY   = DB-COMPANY)
630800             AND (PEP-EMPLOYEE  = DB-EMPLOYEE)
630900             AND (PEP-POS-LEVEL = DB-POS-LEVEL)
631000                 MOVE PEP-POSITION      TO PA52WS-RETRO-POSITION
631100                 MOVE PEP-JOB-CODE      TO PA52WS-RETRO-JOB-CODE
631200                 MOVE PEP-PROCESS-LEVEL TO PA52WS-RETRO-PL
631300                 MOVE PEP-DEPARTMENT    TO PA52WS-RETRO-DEPARTMENT
                       SET PA52WS-RETRO       TO TRUE
631400             END-IF
631500         END-IF
               MOVE PA52F5-PCT-COMPANY        TO DB-COMPANY
               MOVE PA52F5-PCT-EMPLOYEE       TO DB-EMPLOYEE
               MOVE PA52F5-POS-LEVEL          TO DB-POS-LEVEL
P58591         MOVE PEPSET3-POS-LEVEL         TO WS-DB-BEG-RNG
P58591         PERFORM 850-FIND-BEGRNG-PEPSET3
               IF  (PAEMPPOS-FOUND)
                   MOVE PEP-POSITION      TO PA52WS-POSITION
                   MOVE PEP-JOB-CODE      TO PA52WS-JOB-CODE
                   MOVE PEP-PROCESS-LEVEL TO PA52WS-PL
                   MOVE PEP-DEPARTMENT    TO PA52WS-DEPARTMENT
               END-IF
               INITIALIZE PAPEP-SCR-FIELDS
631600         PERFORM 620-MOVE-SCR-TO-WS
631700         THRU    620-END
               MOVE PA52F5-XMIT-PAPEP-BLOCK    TO PAPEP-XMIT-BLOCK
               MOVE WS-TRUE                    TO PAPEP-DATE-SW
               MOVE PA52F5-PCT-MOVE-FROM-LEVEL TO PAPEP-POS-LEVEL-MOVE
632000         PERFORM 2000-PAPEP-EDIT-TRAN
632100         MOVE PAPEP-PAPOS-EFFECT-DATE
632200                                     TO PA52WS-POS-EFFECT-DATE
632300         MOVE PAPEP-XMIT-BLOCK       TO PA52F5-XMIT-PAPEP-BLOCK
632700         IF (PAPEP-UPDPEP-DATE = PA52F5-PCT-EFFECT-DATE)
632800             INITIALIZE                 PAPEP-UPDPEP-DATE
632900         END-IF
               IF  ((PAPEP-BASE-CURRENCY NOT =
                                          PA52F5-PCT-BASE-CURRENCY)
                OR  (PAPEP-BASE-PAY-RATE NOT =
                                          PA52F5-PEP-BASE-PAY-RATE))
               AND (PAPEP-PAY-STEP = ZEROES)
                   IF  (PAPEP-BASE-CURRENCY NOT =
                                              PA52F5-PCT-BASE-CURRENCY)
                       MOVE PAPEP-BASE-CURRENCY
                                           TO PA52F5-PCT-NEW-BASE-CURR
                       MOVE PAPEP-BASE-ND  TO PA52F5-PCT-NEW-BASE-ND
                   END-IF
                   IF (PAPEP-BASE-PAY-RATE   = ZEROES)
                   OR (PA52F5-BASE-RATE-STAR = SPACES)
                       MOVE SPACES         TO PA52F5-PCT-NEW-BASE-RATE-A
                   ELSE
                       MOVE PAPEP-BASE-PAY-RATE
                                           TO PA52F5-PCT-NEW-BASE-RATE
                                                 HRWS-DEC-FIELD
                       MOVE 0              TO HRWS-SIZE
                       MOVE 4              TO HRWS-NBR-DECIMALS
                       PERFORM 770-HR-FORMAT-DEC-FIELD
                       MOVE HRWS-VALUE     TO PA52F5-PCT-NEW-BASE-RATE-A
                   END-IF
               ELSE
J79563             INITIALIZE WS-REVERSE-RATE-A-34
J79563             SET        WS-REVERSE-RATE-NOTINIT 
J79563                                     TO TRUE
J79563             IF (PA52F5-XMIT-IMMED   =  ZEROS)
J79563                 MOVE PA52F5-PCT-NEW-BASE-RATE-A 
J79563                                     TO WS-REVERSE-RATE-A-34
J79563                 SET    WS-REVERSE-RATE-INIT 
J79563                                     TO TRUE   
J79563             END-IF
                   INITIALIZE PA52F5-PCT-NEW-BASE-CURR
                              PA52F5-PCT-NEW-BASE-ND
                              PA52F5-PCT-NEW-BASE-RATE
                              PA52F5-PCT-NEW-BASE-RATE-A
               END-IF
633000         IF (ERROR-FOUND)
633100             GO TO 220-END
               END-IF
           END-IF.
633200
633300     MOVE "PA52"                   TO CRT-ERROR-CAT.
633400
694500     IF  (PA52F5-IMMEDIATE-ACTION = "Y")
           AND (PA52F5-XMIT-IMMED       = ZEROS)
               MOVE 1                  TO PA52F5-XMIT-IMMED
               MOVE 404                TO CRT-ERROR-NBR
               GO TO 220-END.
695000
633500 220-END.
633600
633700******************************************************************
633800 221-GET-PARTICIPNT-NUMBER.
633900******************************************************************
634000
634100     IF (PA52WS-PARTICIPNT = ZEROS)
634200         ADD 1                   TO PA52WS-PARTICIPNT
634300     ELSE
634400         IF (PARTICIPNT-KNOTFOUND)
634500         OR (PAR-COMPANY NOT = DB-COMPANY)
634600             MOVE WS-TRUE        TO PA52WS-NUMBER-SW
634700         ELSE
634800             IF (PAR-PARTICIPNT NOT = PA52WS-PARTICIPNT)
634900                 MOVE WS-TRUE    TO PA52WS-NUMBER-SW
635000             ELSE
635100                 ADD 1           TO PA52WS-PARTICIPNT
635200                 PERFORM 860-KFIND-NEXT-PARSET1.
635300
635400 221-END.
635500
635600******************************************************************
635700 222-EDIT-REASONS.
635800******************************************************************
635900
636000     IF (PA52F5-PCT-REASON (I1) = SPACES)
636100         GO TO 222-END.
636200
           SET PA52WS-NO-ACTION-REASON TO TRUE.

           MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY.
           MOVE PA52F5-PCT-ACTION-CODE TO DB-ACTION-CODE.
049700     MOVE CRESET4-ACTION-CODE    TO WS-DB-BEG-RNG.
           PERFORM 850-KFIND-BEGRNG-CRESET4.
           IF (PAACTREAS-KFOUND)
               SET PA52WS-ACTION-REASON     TO TRUE
           END-IF.

050000     IF  (PA52F5-FC              = "A")
050100     OR ((PA52F5-FC              = "C")
050200     AND (PA52F5-PCT-REASON (I1) NOT = PCT-REASON (I1))
           AND (PA52F5-PCT-REASON (I1) NOT = SPACES))
               MOVE PA52F5-PCT-REASON (I1) TO DB-ACT-REASON-CD
               IF (PA52WS-ACTION-REASON)
                   PERFORM 840-FIND-CRESET4
               ELSE
                   MOVE CRESET3-ACT-REASON-CD    TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-CRESET3
               END-IF
050400         IF (PAACTREAS-NOTFOUND)
                   IF (PA52WS-ACTION-REASON)
                       MOVE 142                TO CRT-ERROR-NBR
                   ELSE
050500                 MOVE 140                        TO CRT-ERROR-NBR
                   END-IF
050600             MOVE PA52F5-PCT-REASON-FN (I1)      TO CRT-FIELD-NBR
050700             GO TO 222-END
050800         ELSE
050900             IF (CRE-ACTIVE-FLAG NOT = "1")
051000                 MOVE 141                        TO CRT-ERROR-NBR
051100                 MOVE PA52F5-PCT-REASON-FN (I1)  TO CRT-FIELD-NBR
051200                 GO TO 222-END.
051300
638000 222-END.
638100
638200******************************************************************
638300 223-CHECK-FOR-DUPS.
638400******************************************************************
638500
638600     IF  (PEP-END-DATE > ZEROES)
638700     AND (PEP-END-DATE < PA52F5-PCT-EFFECT-DATE)
638800         GO TO 223-FIND-NEXT-PEPSET5.
638900
639000     IF  (PAPEP-END-DATE > ZEROES)
639100     AND (PAPEP-END-DATE < PEP-EFFECT-DATE)
639200         GO TO 223-FIND-NEXT-PEPSET5.
639300
639400*    IF  (PAPEP-END-DATE    = ZEROES)
639500*    AND (PEP-POS-LEVEL     NOT = PA52F5-POS-LEVEL)
639600*        IF  (PEP-PROCESS-LEVEL = PA52F5-PCT-NEW-PL)
639700*        AND (PEP-DEPARTMENT    = PA52F5-PCT-NEW-DEPARTMENT)
639800*        AND (PEP-JOB-CODE      = PA52F5-PCT-NEW-JOBCODE)
639900*        AND (PEP-POSITION      = PA52F5-PCT-POSITION)
640000*            MOVE 169                TO CRT-ERROR-NBR
      *            MOVE 0                  TO HRWS-SIZE
      *            MOVE 0                  TO HRWS-NBR-DECIMALS
      *            MOVE PEP-POS-LEVEL      TO HRWS-DEC-FIELD
      *            PERFORM 770-HR-FORMAT-DEC-FIELD
      *            MOVE HRWS-VALUE         TO CRT-ERR-VAR1
640300*            MOVE PA52F5-FC-FN       TO CRT-FIELD-NBR
640400*            GO TO 223-END
640500*         ELSE
640600*             GO TO 223-FIND-NEXT-PEPSET5
640700*         END-IF
640800*    ELSE

198255     IF  (PAPEP-END-DATE           = ZEROES)
198255         IF (PEP-POS-LEVEL     NOT = PA52F5-POS-LEVEL)
198255             MOVE 169                TO CRT-ERROR-NBR
198255             MOVE 0                  TO HRWS-SIZE
198255             MOVE 0                  TO HRWS-NBR-DECIMALS
198255             MOVE PEP-POS-LEVEL      TO HRWS-DEC-FIELD
198255             PERFORM 770-HR-FORMAT-DEC-FIELD
198255             MOVE HRWS-VALUE         TO CRT-ERR-VAR1
198255             MOVE PA52F5-FC-FN       TO CRT-FIELD-NBR
198255             GO TO 223-END
198255         ELSE
198255             GO TO 223-FIND-NEXT-PEPSET5
198255         END-IF
198255     ELSE
640900     IF  (PAPEP-END-DATE   > ZEROES)
641000     AND (PEP-POS-LEVEL    NOT = PA52F5-POS-LEVEL)
641100         IF (PEP-END-DATE = ZEROES)
641200             IF  (PA52F5-PCT-EFFECT-DATE < PEP-EFFECT-DATE)
641300             AND (PAPEP-END-DATE         < PEP-EFFECT-DATE)
641400                 GO TO 223-FIND-NEXT-PEPSET5
641500             ELSE
641600                 MOVE 169                TO CRT-ERROR-NBR
                       MOVE 0                  TO HRWS-SIZE
                       MOVE 0                  TO HRWS-NBR-DECIMALS
                       MOVE PEP-POS-LEVEL      TO HRWS-DEC-FIELD
                       PERFORM 770-HR-FORMAT-DEC-FIELD
                       MOVE HRWS-VALUE         TO CRT-ERR-VAR1
641900                 MOVE PAPEP-FC-FN        TO CRT-FIELD-NBR
642000                 GO TO 223-END
642100             END-IF
642200         ELSE
642300         IF  (PEP-EFFECT-DATE < PA52F5-PCT-EFFECT-DATE)
642400         AND (PEP-END-DATE    < PA52F5-PCT-EFFECT-DATE)
642500             GO TO 223-FIND-NEXT-PEPSET5
642600         ELSE
642700             MOVE 169                TO CRT-ERROR-NBR
                   MOVE 0                  TO HRWS-SIZE
                   MOVE 0                  TO HRWS-NBR-DECIMALS
                   MOVE PEP-POS-LEVEL      TO HRWS-DEC-FIELD
                   PERFORM 770-HR-FORMAT-DEC-FIELD
                   MOVE HRWS-VALUE         TO CRT-ERR-VAR1
643000             MOVE PAPEP-FC-FN        TO CRT-FIELD-NBR
643100             GO TO 223-END
198255         END-IF
198255         END-IF
198255     END-IF
198255     END-IF.
643200
643300 223-FIND-NEXT-PEPSET5.
643400     PERFORM 860-FIND-NXTRNG-PEPSET5.
643500
643600 223-END.
643700
643800******************************************************************
643900 224-EDIT-NEW-VALUES.
644000******************************************************************
644100
644200     IF (PA52WS-PCT-NEW-VALUE5 (I1)  = SPACES)
      *pay rate
               IF  (I1 = 15)
      *pay step
               AND (PA52WS-PCT-NEW-VALUE5 (23) = "*BLANK")
                   NEXT SENTENCE
               END-IF
644300         GO TO 224-END.
644400
644600     SET PA52WS-SOMETHING-ENTERED TO TRUE.
645400
      * Note that field 1 is not being checked because it is the end
      * date which can not be secured.
      *

           IF  (PA52WS-PCT-PRE-VALUE (I1) = PA52WS-SEC-MESSAGE)
           AND (PA52WS-PCT-NEW-VALUE5 (I1) NOT = SPACES)
               MOVE 193                                TO CRT-ERROR-NBR
               MOVE PA52WS-PCT-NEW-VALUE5-FN (I1)      TO CRT-FIELD-NBR
               GO TO 224-END
           END-IF.
689900
      * salary class, pay frequency, exempt, pay plan, security level,  
      * and security location
645500     IF  (PA52F5-POS-LEVEL           NOT = 01)
645600     AND (PA52WS-PCT-NEW-VALUE5 (I1) NOT = SPACES)
           AND (I1 = 13 OR 14 OR 17 OR 18 OR 26 OR 27)
645800         MOVE 114                                TO CRT-ERROR-NBR
645900         MOVE PA52WS-PCT-NEW-VALUE5-FN (I1)      TO CRT-FIELD-NBR
646000         GO TO 224-END
           END-IF.
653900
654000     IF  (PA52F5-POS-LEVEL           = 01)
654100     AND (PA52WS-PCT-NEW-VALUE5 (I1) NOT = SPACES)
654200     AND (PA52WS-HREMP-NO-FLDS-FOUND)
654400     AND (I1 = 13 OR 14 OR 17 OR 18 OR 26 OR 27)
655800         MOVE WS-TRUE                      TO PA52WS-HREMP-SW.
655900
P43929     IF  (PA52WS-PCT-STAR (I1) NOT = "*")
P43929         MOVE 121                                TO CRT-ERROR-NBR
P43929         MOVE PA52WS-PCT-NEW-VALUE5-FN (I1)      TO CRT-FIELD-NBR
P43929         GO TO 224-END
P43929     END-IF.
P43929
656000     MOVE PA52WS-PAT-FLD-NBR-5 (I1) TO DB-FLD-NBR.

           IF  (HRWS-PAD-TYPE  (DB-FLD-NBR) = SPACES)
               PERFORM 840-FIND-PADSET1
               MOVE PAD-DATA-TYPE      TO HRWS-PAD-TYPE (DB-FLD-NBR)
               MOVE PAD-DECIMALS       TO HRWS-PAD-DEC  (DB-FLD-NBR)
               MOVE PAD-CURRENCY-FLAG  TO HRWS-PAD-CURR (DB-FLD-NBR)
               MOVE PAD-SIZE           TO HRWS-PAD-SIZE (DB-FLD-NBR)
               MOVE PAD-CASE-FLAG      TO HRWS-PAD-CASE (DB-FLD-NBR)
           END-IF.
656200
656300     INITIALIZE                                  PA52WS-VALUE.
657400     IF  (PA52WS-PCT-NEW-VALUE5 (I1)  NOT = SPACES)
657500     AND (PA52WS-PCT-NEW-VALUE5 (I1)  NOT = "*BLANK")
657600         MOVE PA52WS-PCT-NEW-VALUE5 (I1)    TO PAPCT-NEW-VALUE
657800         MOVE PA52WS-PCT-NEW-VALUE5-FN (I1) TO PAPCT-FIELD-FN
657900         MOVE PA52WS-PAT-FLD-NBR-5 (I1)     TO PAPCT-FLD-NBR
658000         MOVE HRWS-PAD-TYPE (DB-FLD-NBR)    TO PAPCT-TYPE
658100         MOVE HRWS-PAD-SIZE (DB-FLD-NBR)    TO PAPCT-SIZE
658200         MOVE HRWS-PAD-DEC  (DB-FLD-NBR)    TO PAPCT-DECIMALS
               SET PAPCT-NO-CURRENCY TO TRUE
               SET PAPCT-REFORMAT TO TRUE
658300         PERFORM 5400-EDIT-CORRECT-FORMAT
658400         IF (ERROR-FOUND)
                   IF  (I1 = 1)
                       MOVE PA52F5-PCT-NEW-END-DATE-FN
                                                  TO CRT-FIELD-NBR
                   END-IF
                   IF  (I1 = 2)
P51466                 MOVE PA52F5-PCT-NEW-DATE-ASSIGN-FN 
                                                  TO CRT-FIELD-NBR
                   END-IF
                   IF  (I1 = 11)
                       MOVE PA52F5-PCT-NEW-FTE-FN
                                                  TO CRT-FIELD-NBR
                   END-IF
                   IF  (I1 = 12)
                       MOVE PA52F5-PCT-NEW-ANNUAL-HOURS-FN
                                                  TO CRT-FIELD-NBR
                   END-IF
                   IF  (I1 = 14)
                       MOVE PA52F5-PCT-NEW-PAY-FREQ-FN
                                                  TO CRT-FIELD-NBR
                   END-IF
                   IF (I1 = 15)
                       MOVE PA52F5-PCT-NEW-PAY-RATE-FN
                                                  TO CRT-FIELD-NBR
                   END-IF
                   IF (I1 = 23)
                       MOVE PA52F5-PCT-NEW-STEP-FN
                                                  TO CRT-FIELD-NBR
                   END-IF
                   IF (I1 = 24)
                       MOVE PA52F5-PCT-NEW-SHIFT-FN
                                                  TO CRT-FIELD-NBR
                   END-IF
                   IF (I1 = 26)
                       MOVE PA52F5-PCT-NEW-SECURITY-LVL-FN
                                                  TO CRT-FIELD-NBR
                   END-IF
                   IF (I1 = 28)
                       MOVE PA52F5-PCT-NEW-EXP-COMPANY-FN
                                                  TO CRT-FIELD-NBR
                   END-IF
                   IF  (I1 = 30)
                       MOVE PA52F5-PCT-NEW-ACCOUNT-FN
                                                  TO CRT-FIELD-NBR
                   END-IF
                   IF  (I1 = 31)
                       MOVE PA52F5-PCT-NEW-SUBACCOUNT-FN
                                                  TO CRT-FIELD-NBR
                   END-IF
                   IF (I1 = 34)
                       MOVE PA52F5-PCT-ENTERED-BASE-FN
                                                  TO CRT-FIELD-NBR
                   END-IF
                   IF (I1 = 36)
                       MOVE PA52F5-PCT-NEW-USER-AMOUNT-FN
                                                  TO CRT-FIELD-NBR
                   END-IF
658500             GO TO 224-END
658600         ELSE
658700             MOVE PAPCT-NEW-VALUE   TO PA52WS-PCT-NEW-VALUE5 (I1)
658800                                       PA52WS-VALUE
658900         END-IF
659000      ELSE
659100          MOVE PA52WS-PCT-NEW-VALUE5 (I1)    TO PA52WS-VALUE.

P81486     IF  (PA52WS-PCT-NEW-VALUE5 (I1) NOT = "*BLANK")
P81486     AND (PA52WS-PCT-NEW-VALUE5 (I1) NOT = SPACES)
P81486     AND (PA52WS-PCT-NEW-VALUE5 (I1) = PA52WS-PCT-PRE-VALUE (I1))
P81486         IF  (I1 = 14)
P81486         AND (PA52WS-PCT-NEW-VALUE5 (20) = "*BLANK")
P81486             CONTINUE        
P81486         ELSE
P81486             IF  (PA52F5-PCT-APPROVAL-FLAG = "L")             
P81486                  MOVE SPACES    TO PA52WS-VALUE
P81486             END-IF                         
P81486         END-IF
P81486     END-IF.
P81486
P81486     IF  (PA52WS-PCT-NEW-VALUE5 (I1) = "*BLANK")
P81486     AND (PA52WS-PCT-PRE-VALUE  (I1) = SPACES)
P81486     AND (PA52F5-PCT-APPROVAL-FLAG = "L")                        
P81486         MOVE SPACES             TO PA52WS-VALUE
P81486                                    PA52WS-PCT-NEW-VALUE5 (I1) 
P81486     END-IF.               
P81486
           IF  (I1 = 1)
               MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-END-DATE.
           IF  (I1 = 2)
P51466         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-DATE-ASSIGN.
659800     IF  (I1 = 3)
               MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-POSITION.
660700     IF  (I1 = 4)
661300         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-JOBCODE.
661600     IF  (I1 = 5)
662200         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-PL.
662500     IF  (I1 = 6)
663200         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-DEPARTMENT.
663500     IF  (I1 = 7)
664200         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-USER-LEVEL.
664500     IF  (I1 = 8)
665200         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-SUPERVISOR.
665500     IF  (I1 = 9)
666100         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-IND-SUPER.
666400     IF  (I1 = 10)
667000         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-LOCATION.
667300     IF  (I1 = 11)
667900         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-FTE.
668200     IF  (I1 = 12)
669000         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-ANNUAL-HOURS.
669400     IF  (I1 = 13)
670200         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-SALARY-CLASS.
670600     IF  (I1 = 14)
671300         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-PAY-FREQ.
671600     IF (I1 = 15)
672300         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-PAY-RATE.
672600     IF  (I1 = 16)
               MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-CURRENCY.
672600     IF  (I1 = 17)
673300         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-EXEMPT.
673600     IF  (I1 = 18)
674300         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-PAY-PLAN.
674600     IF (I1 = 19)
675200         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-UNION.
           IF (I1 = 20)
               MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-BARGAIN-UNIT.
675500     IF (I1 = 21)
676100         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-SCHEDULE.
676400     IF (I1 = 22)
677000         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-GRADE.
677300     IF (I1 = 23)
677900         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-STEP.
679100     IF (I1 = 24)
679700         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-SHIFT.
678200     IF (I1 = 25)
678800         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-WORK-SCHD.
680000     IF (I1 = 26)
680600         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-SECURITY-LVL.
681000     IF (I1 = 27)
681600         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-SECURITY-LOC.
682000     IF (I1 = 28)
682700         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-EXP-COMPANY.
683100     IF (I1 = 29)
683800         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-ACCT-UNIT.
684100     IF  (I1 = 30)
684800         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-ACCOUNT.
685100     IF  (I1 = 31)
685800         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-SUBACCOUNT.
686100     IF  (I1 = 32)
686800         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-ACTIVITY.
687100     IF  (I1 = 33)
687800         MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-CATEGORY.
           IF (I1 = 34)
               MOVE PA52WS-VALUE       TO PA52F5-PCT-ENTERED-BASE.
           IF (I1 = 36)
               MOVE PA52WS-VALUE       TO PA52F5-PCT-NEW-USER-AMOUNT.
689900
           IF  (PA52WS-PCT-NEW-VALUE5 (I1) NOT = "*BLANK")
           AND (PA52WS-PCT-NEW-VALUE5 (I1) NOT = SPACES)
           AND (PA52WS-PCT-NEW-VALUE5 (I1) = PA52WS-PCT-PRE-VALUE (I1))
               IF  (I1 = 15)
               AND (PA52WS-PCT-NEW-VALUE5 (23) = "*BLANK")
                   NEXT SENTENCE
               ELSE
P81486             IF  (PA52F5-PCT-APPROVAL-FLAG = "L")             
P81486                  MOVE SPACES    TO PA52WS-PCT-NEW-VALUE5 (I1)
P81486                  GO TO 224-END
P81486             ELSE 
                        MOVE 146                       TO CRT-ERROR-NBR
P81486             END-IF                         
               END-IF
               MOVE PA52WS-PCT-NEW-VALUE5-FN (I1)      TO CRT-FIELD-NBR
               GO TO 224-END
           END-IF.

P60027     SET PEP-FLD-CHG                    TO TRUE.
690000     INITIALIZE PAPCT-SCR-FIELDS.
690100     MOVE PA52F5-PCT-COMPANY            TO PAPCT-COMPANY.
690200     MOVE PA52F5-PCT-EMPLOYEE           TO PAPCT-EMPLOYEE.
690300     MOVE PA52WS-PCT-NEW-VALUE5-FN (I1) TO PAPCT-FIELD-FN.
690400     MOVE PA52WS-PAT-FLD-NBR-5 (I1)     TO PAPCT-FLD-NBR.
690500     MOVE PA52WS-VALUE                  TO PAPCT-NEW-VALUE.
           MOVE PA52F5-PCT-EFFECT-DATE        TO PAPCT-EFFECT-DATE.
690600
690700     PERFORM 5000-PAPCT-MOVE-TO-HREMP.
690800
690900 224-END.
691000
      ******************************************************************
       230-EDIT-SPECIAL-PROCESSING.
      ******************************************************************

           IF  (PA52F5-FC = "R")
           AND (PA52F5-PCT-PROCESS-TYPE NOT = "4")
      **** Incorrect process-type for this special action ****
               MOVE 126                          TO CRT-ERROR-NBR
               MOVE PA52F5-PCT-PROCESS-TYPE-FN   TO CRT-FIELD-NBR
               GO TO 230-END.

           IF ((PA52F5-FC = "R")
           OR  (PA52F5-PCT-PROCESS-TYPE = "2" OR "3" OR "4" OR "5"))
           AND (PA52F5-PCT-HIST-CORR-FLAG = SPACES)
               MOVE PA52F5-PCT-COMPANY             TO DB-COMPANY
               MOVE PA52F5-PCT-ACTION-CODE         TO DB-ACTION-CODE
               PERFORM 840-FIND-PATSET1
               IF (PERSACTYPE-NOTFOUND)
                   MOVE 110                        TO CRT-ERROR-NBR
                   MOVE PA52F5-PCT-ACTION-CODE-FN  TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF
               IF (PAT-HIST-CORR-FLAG NOT = SPACES)
                   MOVE PAT-HIST-CORR-FLAG TO PA52F5-PCT-HIST-CORR-FLAG
               ELSE
               IF (PRS-HIST-CORR-FLAG NOT = SPACES)
                   MOVE PRS-HIST-CORR-FLAG TO PA52F5-PCT-HIST-CORR-FLAG
               ELSE
      **** History Correction method is required ****
                   MOVE 123                          TO CRT-ERROR-NBR
                   MOVE PA52F5-PCT-HIST-CORR-FLAG-FN TO CRT-FIELD-NBR
                   GO TO 230-END.

           IF  (PA52F5-PCT-PROCESS-TYPE = "1")
           AND (PA52F5-FC NOT = "C")
               MOVE 540                                TO CRT-ERROR-NBR
               MOVE PA52F5-PCT-PROCESS-TYPE-FN         TO CRT-FIELD-NBR
               GO TO 230-END.

           IF  (PA52F5-FC = "R")
           AND (PA52F5-PCT-MERGE-ACTN-NBR = ZEROES)
      **** Enter the Action number to be reversed ****
               MOVE 127                          TO CRT-ERROR-NBR
               MOVE PA52F5-PCT-MERGE-ACTN-NBR-FN TO CRT-FIELD-NBR
               GO TO 230-END.

           IF  (PA52F5-PCT-PROCESS-TYPE = "2")
           AND (PA52F5-PCT-MERGE-ACTN-NBR NOT = ZEROES)
      **** Special Action Nbr not valid for this Process Type ****
               MOVE 528                          TO CRT-ERROR-NBR
               MOVE PA52F5-PCT-MERGE-ACTN-NBR-FN TO CRT-FIELD-NBR
               GO TO 230-END.

           IF  (PA52F5-PCT-PROCESS-TYPE = "3")
           AND (PA52F5-PCT-MERGE-ACTN-NBR = ZEROES)
      **** Special Action Nbr is required for this Process Type ****
               MOVE 527                          TO CRT-ERROR-NBR
               MOVE PA52F5-PCT-MERGE-ACTN-NBR-FN TO CRT-FIELD-NBR
               GO TO 230-END.

           IF (PA52F5-PCT-PROCESS-TYPE = "3")
               MOVE PA52F5-PCT-EMPLOYEE              TO DB-EMPLOYEE
               MOVE PA52F5-PCT-EFFECT-DATE           TO DB-EFFECT-DATE
               MOVE PA52F5-PCT-ACTION-CODE           TO DB-ACTION-CODE
               MOVE PA52F5-PCT-MERGE-ACTN-NBR        TO DB-ACTION-NBR
               PERFORM 840-FIND-PAHSET1
               IF (PERSACTHST-FOUND)
                   MOVE PAH-OBJ-ID         TO PA52F5-MERGE-ACT-OBJ-ID
               ELSE
      **** Action to be combined doesn't exist in history ****
                   MOVE 529                            TO CRT-ERROR-NBR
                   MOVE PA52F5-PCT-MERGE-ACTN-NBR-FN   TO CRT-FIELD-NBR
                   GO TO 230-END.

           IF  (PA52F5-PCT-PROCESS-TYPE = "4")
J31358         IF  (PA52F5-FC                      = "R")
J31358         AND (PA52F5-PCT-END-DATE        NOT = SPACES)
J31358**** Reverse action on position stop date is not allowed
J31358             MOVE "PA52"                 TO CRT-ERROR-CAT
J31358             MOVE 271                    TO CRT-ERROR-NBR
J31358             MOVE PA52F5-PCT-END-DATE-FN   
J31358                                         TO CRT-FIELD-NBR
J31358             GO TO 230-END
J31358         END-IF
               MOVE PA52F5-PCT-COMPANY         TO PAPCT-REV-COMPANY
               MOVE PA52F5-PCT-EMPLOYEE        TO PAPCT-REV-EMPLOYEE
               MOVE PA52F5-PCT-EFFECT-DATE     TO PAPCT-REV-EFFECT-DATE
               MOVE PA52F5-PCT-ACTION-CODE     TO PAPCT-REV-ACTION-CODE
               MOVE PA52F5-PCT-MERGE-ACTN-NBR  TO PAPCT-REV-ACTION-NBR
               MOVE PA52F5-POS-LEVEL           TO PAPCT-REV-POS-LEVEL
               PERFORM 7000-PCT-EDIT-REVERSE
               IF  (ERROR-FOUND)
                   GO TO 230-END
               END-IF
               MOVE PAPCT-REV-ACT-OBJ-ID   TO HREMP-ACT-OBJ-ID-REV
                                              PA52F5-MERGE-ACT-OBJ-ID
           END-IF.

           IF  (PA52F5-PCT-NEW-EFFECT-DATE NOT = ZEROES)
           AND (PA52F5-PCT-PROCESS-TYPE NOT = "1")
               MOVE 532                                TO CRT-ERROR-NBR
               MOVE PA52F5-PCT-NEW-EFFECT-DATE-FN      TO CRT-FIELD-NBR
               GO TO 230-END
           END-IF.

           IF  (PA52F5-PCT-PROCESS-TYPE   NOT = "1")
           AND (PA52F5-PCT-HIST-CORR-FLAG NOT = "1")
               MOVE PA52F5-PCT-COMPANY  TO DB-COMPANY
               MOVE PA52F5-PCT-EMPLOYEE TO DB-EMPLOYEE
               MOVE PA52F5-POS-LEVEL    TO DB-POS-LEVEL
               MOVE PEASET1-POS-LEVEL   TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PEASET1
               IF (PAPEPAUDIT-FOUND)
      **** History Correction must = 1: pending correction exists ****
                   MOVE 530             TO CRT-ERROR-NBR
                   MOVE PA52F5-PCT-HIST-CORR-FLAG-FN
                                        TO CRT-FIELD-NBR
                   GO TO 230-END.

           IF   (PA52F5-PCT-PROCESS-TYPE    = SPACES)
           AND  (PA52F5-PCT-MOVE-FROM-LEVEL NOT = ZEROES)
      **** Special processing fields not valid when no Processing Type ****
               IF  (PA52F5-PCT-NEW-EFFECT-DATE NOT = ZEROES)
                   MOVE 129                            TO CRT-ERROR-NBR
                   MOVE PA52F5-PCT-NEW-EFFECT-DATE-FN  TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF
               IF  (PA52F5-PCT-HIST-CORR-FLAG NOT = SPACES)
                   MOVE 129                            TO CRT-ERROR-NBR
                   MOVE PA52F5-PCT-HIST-CORR-FLAG-FN   TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF
               IF  (PA52F5-PCT-MERGE-ACTN-NBR NOT = ZEROES)
                   MOVE 129                            TO CRT-ERROR-NBR
                   MOVE PA52F5-PCT-PROCESS-TYPE-FN     TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF
           END-IF.

           IF (PA52F5-PCT-PROCESS-TYPE = "1")
               IF (PA52F5-FC = "A")
      **** Pending action doesn't exist; can't change effective date ****
                   MOVE 122                            TO CRT-ERROR-NBR
                   MOVE PA52F5-PCT-NEW-EFFECT-DATE-FN  TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF
               IF  (PA52F5-PCT-NEW-EFFECT-DATE = ZEROES)
               AND (PA52F5-FC NOT = "D")
      **** New Effective Date is required for this Process Type ****
                   MOVE 526                            TO CRT-ERROR-NBR
                   MOVE PA52F5-PCT-NEW-EFFECT-DATE-FN  TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF.

      **** If changing effective date, no other fields can be changed ****
      **** Also not allowed to change any value on a reversal record  ****
           IF  (PA52F5-FC = "C")
           AND ((PCT-PROCESS-TYPE = "4")
            OR  (PA52F5-PCT-PROCESS-TYPE = "1"))
               IF  (PA52F5-PCT-REASON (1)     NOT = PCT-REASON (1))
               OR  (PA52F5-PCT-REASON (2)     NOT = PCT-REASON (2))
               OR  (PA52F5-PCT-ANT-END-DATE   NOT = PCT-ANT-END-DATE)
               OR  (PA52F5-PCT-UPDATE-BENEFIT NOT = PCT-UPDATE-BENEFIT)
               OR  (PA52F5-PCT-UPD-ABS-MGMT   NOT = PCT-UPD-ABS-MGMT)
               OR  (PA52F5-PCT-UPDATE-REQ-DED NOT = PCT-UPDATE-REQ-DED)
               OR  (PA52F5-PCT-EDM-END-DATE   NOT = PCT-EDM-END-DATE)
               OR  (PA52F5-PCT-EDM-EFFECT-DT  NOT = PCT-EDM-EFFECT-DT)
               OR  (PA52F5-PCT-OCCUR-TYPE     NOT = PCT-OCCUR-TYPE)
               OR  (PA52F5-PCT-PARTICIPNT     NOT = PCT-PARTICIPNT)
               OR  (PA52F5-PCT-MERGE-ACTN-NBR NOT = PCT-MERGE-ACTN-NBR)
               OR  (PA52F5-PCT-HIST-CORR-FLAG NOT = PCT-HIST-CORR-FLAG)
               OR  ((PCT-PROCESS-TYPE = "4")
                AND (PA52F5-PCT-PROCESS-TYPE   NOT = PCT-PROCESS-TYPE))
                   MOVE PA52F5-PCT-PROCESS-TYPE-FN     TO CRT-FIELD-NBR
                   IF  (PA52F5-PCT-REASON (1)     NOT = PCT-REASON (1))
                       MOVE PA52F5-PCT-REASON-FN (1)   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F5-PCT-REASON (2)     NOT = PCT-REASON (2))
                       MOVE PA52F5-PCT-REASON-FN (2)   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F5-PCT-ANT-END-DATE  NOT = PCT-ANT-END-DATE)
                       MOVE PA52F5-PCT-ANT-END-DATE-FN TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F5-PCT-UPDATE-BENEFIT
                                               NOT = PCT-UPDATE-BENEFIT)
                       MOVE PA52F5-PCT-UPDATE-BENEFIT-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F5-PCT-UPD-ABS-MGMT
                                               NOT = PCT-UPD-ABS-MGMT)
                       MOVE PA52F5-PCT-UPD-ABS-MGMT-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F5-PCT-UPDATE-REQ-DED
                                               NOT = PCT-UPDATE-REQ-DED)
                       MOVE PA52F5-PCT-UPDATE-REQ-DED-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F5-PCT-EDM-END-DATE  NOT = PCT-EDM-END-DATE)
                       MOVE PA52F5-PCT-EDM-END-DATE-FN TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F5-PCT-EDM-EFFECT-DT
                                               NOT = PCT-EDM-EFFECT-DT)
                       MOVE PA52F5-PCT-EDM-EFFECT-DT-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F5-PCT-OCCUR-TYPE     NOT = PCT-OCCUR-TYPE)
                       MOVE PA52F5-PCT-OCCUR-TYPE-FN   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F5-PCT-PARTICIPNT     NOT = PCT-PARTICIPNT)
                       MOVE PA52F5-PCT-PARTICIPNT-FN   TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F5-PCT-MERGE-ACTN-NBR
                                               NOT = PCT-MERGE-ACTN-NBR)
                       MOVE PA52F5-PCT-MERGE-ACTN-NBR-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF  (PA52F5-PCT-HIST-CORR-FLAG
                                               NOT = PCT-HIST-CORR-FLAG)
                       MOVE PA52F5-PCT-HIST-CORR-FLAG-FN
                                                       TO CRT-FIELD-NBR
                   END-IF
                   IF (PCT-PROCESS-TYPE = "4")
      **** Action is being reversed, values can not be changed ****
                       MOVE 537                        TO CRT-ERROR-NBR
                   ELSE
                       MOVE 124                        TO CRT-ERROR-NBR
                   END-IF
                   GO TO 230-END
               END-IF
           END-IF.

      **** If changing effective date, no other fields can be changed ****
      **** Also not allowed to change any value on a reversal record  ****
           IF  (PA52F5-FC = "C")
           AND ((PCT-PROCESS-TYPE = "4")
            OR  (PA52F5-PCT-PROCESS-TYPE = "1"))
               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > PA52WS-5-FIELD-COUNT)
                   IF  (PA52WS-PCT-NEW-VALUE5 (I1)
                                               NOT = PCT-NEW-VALUE (I1))
                       MOVE PA52WS-PCT-NEW-VALUE5-FN (I1)
                                                       TO CRT-FIELD-NBR
                       MOVE 124                        TO CRT-ERROR-NBR
                   END-IF
               END-PERFORM
               IF  (ERROR-FOUND)
                   IF (PCT-PROCESS-TYPE = "4")
      **** Action is being reversed, values can not be changed ****
                       MOVE 537                        TO CRT-ERROR-NBR
                   END-IF
                   GO TO 230-END
               END-IF
           END-IF.

           IF  (PA52F5-FC = "A")
           AND (PA52F5-PCT-PROCESS-TYPE = "4")
               IF (PA52F5-SVD-ACTN-NBR = ZEROES)
      **** Must perform Special Reverse Action before adding action ****
                   MOVE 542                TO CRT-ERROR-NBR
                   MOVE PA52F5-PCT-MERGE-ACTN-NBR-FN
                                           TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF                         
               IF  (PA52F5-PCT-MERGE-ACTN-NBR NOT = PA52F5-SVD-ACTN-NBR)
      **** Action number changed; perform the reverse again ****
                   MOVE 538                            TO CRT-ERROR-NBR
                   MOVE PA52F5-PCT-MERGE-ACTN-NBR-FN   TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF
               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > PA52WS-5-FIELD-COUNT)
J79563             IF  (WS-REVERSE-RATE-INIT)
J79563             AND (I1 = 34)
J79563                 IF  (WS-REVERSE-RATE-A-34       
J79563                                      NOT = PA52F5-REV-VALUE (I1))
J79563**** Value changed; perform Reverse again ****
J79563                     MOVE 539                    TO CRT-ERROR-NBR
J79563                     MOVE PA52WS-PCT-NEW-VALUE5-FN (I1)
J79563                                                 TO CRT-FIELD-NBR
J79563                     INITIALIZE WS-REVERSE-RATE-A-34
J79563                     SET WS-REVERSE-RATE-NOTINIT TO TRUE
J79563                     GO TO 230-END
J79563                 END-IF
J79563                 INITIALIZE WS-REVERSE-RATE-A-34
J79563                 SET WS-REVERSE-RATE-NOTINIT     TO TRUE
J79563             ELSE
                       IF  (PA52WS-PCT-NEW-VALUE5 (I1)
                                            NOT = PA52F5-REV-VALUE (I1))
      **** Value changed; perform Reverse again ****
                           MOVE 539                    TO CRT-ERROR-NBR
                           MOVE PA52WS-PCT-NEW-VALUE5-FN (I1)
                                                       TO CRT-FIELD-NBR
                           GO TO 230-END
                       END-IF
J79563             END-IF
               END-PERFORM
           END-IF.

           IF (PA52F5-FC = "R")
               PERFORM 235-EDIT-REVERSAL
               THRU    235-END.

           IF  (PA52F5-PCT-MERGE-ACTN-NBR NOT = ZEROES)
           AND (PAH-POS-LEVEL             NOT = PA52F5-POS-LEVEL)
      **** Action number selected does not match position level ****
               MOVE 531                TO CRT-ERROR-NBR
               MOVE PA52F5-PCT-MERGE-ACTN-NBR-FN
                                       TO CRT-FIELD-NBR
               GO TO 230-END
           END-IF.

           IF  (PA52F5-PCT-PROCESS-TYPE     NOT = "5")
           AND (PA52F5-PCT-MOVE-FROM-LEVEL  NOT = ZEROES)
               MOVE 153                    TO CRT-ERROR-NBR
               MOVE PA52F5-PCT-PROCESS-TYPE-FN
                                           TO CRT-FIELD-NBR
               GO TO 230-END.

           IF (PA52F5-PCT-PROCESS-TYPE = "5")
               MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY
               MOVE PA52F5-PCT-EMPLOYEE    TO DB-EMPLOYEE
               MOVE PA52F5-PCT-MOVE-FROM-LEVEL
                                           TO DB-POS-LEVEL
               MOVE PEPSET3-POS-LEVEL      TO WS-DB-BEG-RNG 
               PERFORM 850-KFIND-BEGRNG-PEPSET3
               IF (PAEMPPOS-KNOTFOUND)
                   MOVE 154                TO CRT-ERROR-NBR
                   MOVE PA52F5-PCT-MOVE-FROM-LEVEL-FN
                                           TO CRT-FIELD-NBR
                   GO TO 230-END.

           IF (PA52F5-PCT-PROCESS-TYPE NOT = "5")
               GO TO 230-END.

           MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY.
           MOVE PA52F5-PT-ACTION-TYPE  TO DB-ACTION-TYPE.
           MOVE PA52F5-PCT-EMPLOYEE    TO DB-EMPLOYEE.
           MOVE PCTSET2-EMPLOYEE       TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PCTSET2.
           IF (PERSACTION-FOUND)
               PERFORM
                   UNTIL (PERSACTION-NOTFOUND)
                   OR   ((PCT-EFFECT-DATE > PA52F5-PCT-EFFECT-DATE)
                   AND   ((PCT-POS-LEVEL = 1)
                   OR     (PCT-POS-LEVEL = PA52F5-PCT-MOVE-FROM-LEVEL)))
                       PERFORM 860-FIND-NXTRNG-PCTSET2
               END-PERFORM
               IF (PERSACTION-FOUND)
                   MOVE 155                TO CRT-ERROR-NBR
                   MOVE PA52F5-PCT-MOVE-FROM-LEVEL-FN
                                           TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF.

           MOVE "E"                        TO DB-ACTION-TYPE.
           MOVE PCTSET2-EMPLOYEE           TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PCTSET2.
           IF (PERSACTION-FOUND)
               PERFORM
                   UNTIL (PERSACTION-NOTFOUND)
                   OR   ((PCT-EFFECT-DATE > PA52F5-PCT-EFFECT-DATE)
                   AND   ((PCT-POS-LEVEL = 1)
                   OR     (PCT-POS-LEVEL = PA52F5-PCT-MOVE-FROM-LEVEL)))
                       PERFORM 860-FIND-NXTRNG-PCTSET2
               END-PERFORM
               IF (PERSACTION-FOUND)
                   MOVE 155                TO CRT-ERROR-NBR
                   MOVE PA52F5-PCT-MOVE-FROM-LEVEL-FN
                                           TO CRT-FIELD-NBR
                   GO TO 230-END
               END-IF.
               
           MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY.
           MOVE "L"                    TO DB-ACTION-TYPE.
           MOVE PA52F5-PCT-EMPLOYEE    TO DB-EMPLOYEE.
           MOVE PA52F5-PCT-ACTION-CODE TO DB-ACTION-CODE.
           MOVE PA52F5-PCT-EFFECT-DATE TO DB-EFFECT-DATE.
           MOVE PA52F5-PCT-ACTION-NBR  TO DB-ACTION-NBR.

           PERFORM 840-FIND-PCTSET2.

       230-END.

      ******************************************************************
       235-EDIT-REVERSAL.
      ******************************************************************

           INITIALIZE PA52F5-REV-VALUE-GRP.

           INITIALIZE                    PA52F5-PCT-NEW-END-DATE
P51466                                   PA52F5-PCT-NEW-DATE-ASSIGN
                                         PA52F5-PCT-NEW-POSITION
                                         PA52F5-PCT-NEW-JOBCODE
                                         PA52F5-PCT-NEW-PL
                                         PA52F5-PCT-NEW-DEPARTMENT
                                         PA52F5-PCT-NEW-USER-LEVEL
                                         PA52F5-PCT-NEW-SUPERVISOR
                                         PA52F5-PCT-NEW-IND-SUPER
                                         PA52F5-PCT-NEW-LOCATION
                                         PA52F5-PCT-NEW-FTE
                                         PA52F5-PCT-NEW-ANNUAL-HOURS
                                         PA52F5-PCT-NEW-SALARY-CLASS
                                         PA52F5-PCT-NEW-PAY-FREQ
                                         PA52F5-PCT-NEW-PAY-RATE
                                         PA52F5-PCT-NEW-EXEMPT
                                         PA52F5-PCT-NEW-PAY-PLAN
                                         PA52F5-PCT-NEW-UNION
                                         PA52F5-PCT-NEW-BARGAIN-UNIT
                                         PA52F5-PCT-NEW-SCHEDULE
                                         PA52F5-PCT-NEW-GRADE
                                         PA52F5-PCT-NEW-STEP
                                         PA52F5-PCT-NEW-SHIFT
                                         PA52F5-PCT-NEW-WORK-SCHD
                                         PA52F5-PCT-NEW-SECURITY-LVL
                                         PA52F5-PCT-NEW-SECURITY-LOC
                                         PA52F5-PCT-NEW-EXP-COMPANY
                                         PA52F5-PCT-NEW-ACCT-UNIT
                                         PA52F5-PCT-NEW-ACCOUNT
                                         PA52F5-PCT-NEW-SUBACCOUNT
                                         PA52F5-PCT-NEW-ACTIVITY
                                         PA52F5-PCT-NEW-CATEGORY
                                         PA52F5-PCT-NEW-CURRENCY
                                         PA52F5-PCT-ENTERED-BASE
                                         PA52F5-PCT-NEW-BASE-RATE
                                         PA52F5-PCT-NEW-BASE-RATE-A
                                         PA52F5-PCT-NEW-BASE-CURR
                                         PA52F5-PCT-NEW-USER-AMOUNT.

       235-END.

      ******************************************************************
       240-GET-NEXT-EMPLOYEE.
      ******************************************************************
      
           IF (PA52F5-FC = "N")
               PERFORM 860-FIND-NEXT-PCTSET2
                   UNTIL (PERSACTION-NOTFOUND)
                   OR    (PCT-COMPANY  NOT = EMP-COMPANY)
                   OR    (PCT-EMPLOYEE NOT = EMP-EMPLOYEE)
           ELSE
               PERFORM 870-FIND-PREV-PCTSET2
                   UNTIL (PERSACTION-NOTFOUND)
                   OR    (PCT-COMPANY  NOT = EMP-COMPANY)
                   OR    (PCT-EMPLOYEE NOT = EMP-EMPLOYEE)
           END-IF.

           IF (PERSACTION-NOTFOUND)
           OR (PCT-COMPANY NOT = DB-COMPANY)
               GO TO 240-END.

           MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE.
           PERFORM 840-FIND-EMPSET1.
      
           IF (EMPLOYEE-FOUND)
               MOVE EMP-COMPANY        TO CRT-COMPANY
               MOVE EMP-PROCESS-LEVEL  TO CRT-PROCESS-LEVEL
               PERFORM 700-HR-EMP-SECURITY.
      
       240-END.
J40356******************************************************************
J40356 300-LOAD-LTM-VALUES.
J40356******************************************************************
J40356
J40356     IF ((PA52WS-PAT-FLD-NBR-5 (I1) = ZEROES)
J40356     OR  (PA52WS-PCT-STAR (I1) NOT = "*"))
J40356         GO TO 300-END.
J40356
J40356     IF (PA52WS-LTM-SELECTED)
J40356     AND (PA52F5-FC = "I")
J40356         MOVE PA52WS-L-COMPANY           TO HREMP-L-COMPANY
J40356         MOVE PA52WS-L-EMPLOYEE          TO HREMP-L-EMPLOYEE
J40356         MOVE PA52WS-L-STATUS            TO HREMP-L-STATUS
J40356         MOVE PA52WS-L-DATE-STAMP        TO HREMP-L-DATE-STAMP
J40356         MOVE PA52WS-L-TIME-STAMP        TO HREMP-L-TIME-STAMP
J40356         MOVE PA52WS-L-ERROR-FLAG        TO HREMP-L-ERROR-FLAG
J40356         MOVE PA52WS-L-WORKASSIGNMENT    TO HREMP-L-WORKASSIGNMENT
J40356         MOVE PA52WS-L-ACTION-CODE       TO HREMP-L-ACTION-CODE
J40356         PERFORM 5010-HREMP-FIND-LTM-RECORD
J40356         IF (HREMPRCVR-FOUND)
J40356             MOVE PA52F5-PCT-COMPANY     TO HREMP-COMPANY
J40356             MOVE PA52F5-PCT-EMPLOYEE    TO HREMP-EMPLOYEE
J40356             MOVE SPACES                 TO HREMP-VALUE
J40356             MOVE SPACES                 TO HRWS-VALUE
J40356
J40356             MOVE PA52WS-PAT-FLD-NBR-5 (I1) TO DB-FLD-NBR 
J40356                                               HREMP-FLD-NBR
J40356             INITIALIZE                        DB-COUNTRY-CD-REQ
J40356                                               DB-PROCESS-LEVEL 
J40356             PERFORM 840-FIND-PASSET1 
J40356             SET PA52WS-NOT-SECURED         TO TRUE 
J40356             IF (PASCRTY-FOUND)
J40356                 MOVE PAS-SEC-LEVEL         TO HRWS-SEC-LEVEL
J40356                 PERFORM 730-HR-FIELD-SECURITY
J40356                 IF (HRWS-FLD-SECURED)
J40356                     MOVE 411               TO CRT-MSG-NBR
J40356                     PERFORM 790-GET-MSG
J40356                     MOVE CRT-MESSAGE       TO HREMP-VALUE
J40356                                               PA52WS-SEC-MESSAGE 
J40356                     INITIALIZE                HREMP-FLD-NBR
J40356                     SET PA52WS-SECURED     TO TRUE 
J40356                     GO TO 300-END
J40356                 END-IF
J40356
J40356                 IF (PA52WS-NOT-SECURED)
J40356                      MOVE PA52WS-PAT-FLD-NBR-5 (I1)
J40356                                            TO HREMP-FLD-NBR
J40356                      MOVE "Y"              TO HREMP-FORMAT-FIELD
J40356                      MOVE PA52F5-PCT-COMPANY
J40356                                            TO HREMP-COMPANY
J40356                      MOVE PA52F5-PCT-EMPLOYEE 
J40356                                            TO HREMP-EMPLOYEE
J40356                      MOVE SPACES           TO HREMP-VALUE
J40356                 END-IF
J40356             END-IF 
J40356
J40356* salary class, pay frequency, exempt, pay plan, security level,
J40356* and security location
J40356             IF (I1 = 13 OR 14 OR 17 OR 18 OR 26 OR 27)
J40356                 PERFORM 5020-HREMP-RETRIEVE-LTM-VALUE
J40356             ELSE
J40356                 PERFORM 5020-PAPEP-RETRIEVE-LTM-VALUE 
J40356             END-IF 
J40356
J40356*    If LTM value is blank, *BLANKS to PA52 value.
J40356             IF ((HREMP-VALUE = SPACES)
J40356             AND (PA52WS-PCT-PRE-VALUE (I1) NOT = SPACES)
J30998             AND (HRWS-LTM-VALUE-FOUND))
J40356                 MOVE "*BLANK" TO HREMP-VALUE                  
J40356             END-IF
J40356
J40356*    If LTM value is same as existing value, don't pass to PA52.5
J40356             IF (HREMP-VALUE       = PA52WS-PCT-PRE-VALUE (I1))
J40356                 MOVE SPACES TO HREMP-VALUE                  
J40356             END-IF
J40356  
J40356             IF  (I1 = 1)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-END-DATE
J40356             ELSE
J40356             IF  (I1 = 2)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-DATE-ASSIGN
J40356             ELSE
J40356             IF  (I1 = 3)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-POSITION
J40356             ELSE
J40356             IF  (I1 = 4)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-JOBCODE
J40356             ELSE
J40356             IF  (I1 = 5)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-PL
J40356             ELSE
J40356             IF  (I1 = 6)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-DEPARTMENT
J40356             ELSE
J40356             IF  (I1 = 7)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-USER-LEVEL
J40356             ELSE
J40356             IF  (I1 = 8)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-SUPERVISOR
J40356             ELSE
J40356             IF  (I1 = 9)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-IND-SUPER
J40356             ELSE
J40356             IF  (I1 = 10)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-LOCATION
J40356             ELSE
J40356             IF  (I1 = 11)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-FTE
J40356             ELSE
J40356             IF  (I1 = 12)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-ANNUAL-HOURS 
J40356             ELSE
J40356             IF  (I1 = 13)   
J40356             AND (PA52F5-POS-LEVEL = 01)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-SALARY-CLASS
J40356             ELSE
J40356             IF  (I1 = 14)
J40356             AND (PA52F5-POS-LEVEL = 01)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-PAY-FREQ
J40356             ELSE
J40356             IF  (I1 = 15)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-PAY-RATE
J40356             ELSE
J40356             IF  (I1 = 16)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-CURRENCY
J40356             ELSE
J40356             IF  (I1 = 17)
J40356             AND (PA52F5-POS-LEVEL = 01)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-EXEMPT
J40356             ELSE
J40356             IF  (I1 = 18)
J40356             AND (PA52F5-POS-LEVEL = 01)  
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-PAY-PLAN
J40356             ELSE
J40356             IF  (I1 = 19)  
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-UNION
J40356             ELSE
J40356             IF  (I1 = 20)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-BARGAIN-UNIT
J40356             ELSE
J40356             IF  (I1 = 21)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-SCHEDULE
J40356             ELSE
J40356             IF  (I1 = 22)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-GRADE
J40356             ELSE
J40356             IF  (I1 = 23)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-STEP
J40356             ELSE
J40356             IF  (I1 = 24)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-SHIFT
J40356             ELSE
J40356             IF  (I1 = 25) 
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-WORK-SCHD
J40356             ELSE
J40356             IF  (I1 = 26)
J40356             AND (PA52F5-POS-LEVEL = 01)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-SECURITY-LVL
J40356             ELSE
J40356             IF  (I1 = 27)
J40356             AND (PA52F5-POS-LEVEL = 01)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-SECURITY-LOC
J40356             ELSE
J40356             IF  (I1 = 28)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-EXP-COMPANY
J40356             ELSE
J40356             IF  (I1 = 29)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-ACCT-UNIT
J40356             ELSE
J40356             IF  (I1 = 30)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-ACCOUNT
J40356             ELSE
J40356             IF  (I1 = 31)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-SUBACCOUNT
J40356             ELSE
J40356             IF  (I1 = 32)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-ACTIVITY
J40356             ELSE
J40356             IF  (I1 = 33)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-CATEGORY
J40356             ELSE
J40356             IF  (I1 = 34)
J40356                MOVE PEP-BASE-PAY-RATE TO PA52F5-PEP-BASE-PAY-RATE
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-BASE-RATE-A
J40356             ELSE
J40356             IF  (I1 = 35) 
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-BASE-CURR
J40356             ELSE
J40356             IF  (I1 = 36)
J40356                  MOVE HREMP-VALUE TO PA52F5-PCT-NEW-USER-AMOUNT
J40356             END-IF 
J40356         END-IF
J40356     END-IF.
J40356
J40356 300-END.
691100******************************************************************
691200 400-PROCESS-TRAN.
691300******************************************************************
691400
691500     IF (PA52F5-FC = "A")
691600         PERFORM 410-ADD
691700         THRU    410-END
691800     ELSE
691900     IF (PA52F5-FC = "C")
692000         PERFORM 420-CHANGE
692100         THRU    420-END
692200     ELSE
692300     IF (PA52F5-FC = "D")
692400         PERFORM 440-DELETE
692500         THRU    440-END
692600     ELSE
           IF (PA52F5-FC = "R")
               PERFORM 450-REVERSE
               THRU    450-END
           ELSE
692700     IF (PA52F5-FC = "I" OR "N" OR "P")
692800         PERFORM 480-INQUIRE
692900         THRU    480-END.
693000
693100     IF ((PA52F5-FC = "A" OR "C")
693200     AND (PA52F5-IMMEDIATE-ACTION NOT = "Y"))
693300     OR  (PA52F5-FC = "D")
693400             INITIALIZE HREMP-SCR-FIELDS
693500                        HRPEM-SCR-FIELDS
693600             PERFORM 7000-HREMP-DEFAULT
693700             PERFORM 7000-HRPEM-DEFAULT 
J45417         IF ((PA52F5-FC = "A")
J45417         AND (NO-ERROR-FOUND))
J45417             MOVE CRT-ADD-COMPLETE    TO CRT-MESSAGE
J45417         END-IF
J45417         IF ((PA52F5-FC = "C")
J45417         AND (NO-ERROR-FOUND))
J45417             MOVE CRT-CHG-COMPLETE    TO CRT-MESSAGE
J45417         END-IF
J45417         IF ((PA52F5-FC = "D")
J45417         AND (NO-ERROR-FOUND))
J45417             MOVE CRT-RECS-DELETED    TO CRT-MESSAGE
J45417         END-IF
J45417     END-IF.
693900
694000 400-END.
694100******************************************************************
694200 410-ADD.
694300******************************************************************
694400
           MOVE SPACES                 TO PA52F5-PT-PCT-ACTION-CODE.
695500
695600     PERFORM 910-AUDIT-BEGIN.
695700     IF (DMS-ABORTED)
695800         GO TO 410-END.
695900
696000     MOVE PA52F5-PCT-COMPANY          TO PAACT-COMPANY.
696100     MOVE "L"                         TO PAACT-ACTION-TYPE.
696200     MOVE PA52F5-PCT-EMPLOYEE         TO PAACT-EMPLOYEE.
696300     MOVE PA52F5-PCT-ACTION-CODE      TO PAACT-ACTION-CODE.
696400     MOVE PA52F5-PCT-EFFECT-DATE      TO PAACT-EFFECT-DATE.
696500     MOVE "Y"                         TO PAACT-HISTORY.
696600     PERFORM 2300-PAACT-ACTION-NBR.
696700     MOVE PAACT-ACTION-NBR            TO PA52WS-ACTION-NBR.
696800     MOVE "E"                         TO PAACT-ACTION-TYPE.
696900     PERFORM 2300-PAACT-ACTION-NBR.
697000     IF (PAACT-ACTION-NBR > PA52WS-ACTION-NBR)
697100         MOVE PAACT-ACTION-NBR        TO PA52WS-ACTION-NBR
697200                                         PA52F5-PCT-ACTION-NBR.

697300
           IF (PAT-WORKFLOW-FLAG       = "Y")
               PERFORM 430-CREATE-WORKFLOW-TRIGGER
               THRU    430-END.

697400     IF (PA52F5-IMMEDIATE-ACTION = "Y")
697500         IF   (PA52F5-POS-LEVEL      > 01)
697600         OR  ((PA52F5-POS-LEVEL      = 01)
697700         AND  (PA52WS-HREMP-NO-FLDS-FOUND))
697800             MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY
697900             MOVE PA52F5-PCT-EMPLOYEE    TO DB-EMPLOYEE
698000             MOVE PA52F5-POS-LEVEL       TO DB-POS-LEVEL
P58592             MOVE PEPSET3-POS-LEVEL      TO WS-DB-BEG-RNG
P58592             PERFORM 850-FIND-BEGRNG-PEPSET3.
698900
699000     IF (PA52F5-IMMEDIATE-ACTION = "N")
699100         PERFORM 800-CREATE-PERSACTION
699200         MOVE PA52WS-ACTION-NBR          TO PCT-ACTION-NBR
699300                                            PA52F5-PCT-ACTION-NBR
699400         PERFORM 500-MOVE-DATA
699500         THRU    500-END
               IF  (PCT-POSITION NOT = SPACES)
               AND (PCT-POS-EFF-DT NOT = ZEROES)
                   MOVE PCT-POSITION        TO DB-POSITION
                   MOVE PCT-POS-EFF-DT      TO DB-EFFECT-DATE
                   MOVE PPCSET1-EFFECT-DATE TO WS-DB-BEG-RNG
                   PERFORM 850-KFIND-BEGRNG-PPCSET1
                   IF (PAPOSCHG-KFOUND)
                       MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE
                       INITIALIZE                     DB-REQUISITION
                       MOVE PPESET2-REQUISITION    TO WS-DB-BEG-RNG
                       PERFORM 850-FIND-BEGRNG-PPESET2
                       IF (PAPOSERR-NOTFOUND)
                           PERFORM 800-CREATE-PAPOSERR
                           MOVE PPC-COMPANY TO PPE-COMPANY
                           MOVE PPC-EFFECT-DATE    TO PPE-EFFECT-DATE
                           MOVE PPC-POSITION       TO PPE-POSITION
                           MOVE "1  "              TO PPE-UPDATE-TYPE
                           MOVE PCT-EMPLOYEE       TO PPE-EMPLOYEE
                           INITIALIZE                 PPE-REQUISITION
                           MOVE PCT-POS-LEVEL      TO PPE-POS-LEVEL (1)
                           PERFORM 820-STORE-PAPOSERR
                       END-IF
                   END-IF
               END-IF
699600         PERFORM 8200-STORE-PERSACTION
699700         MOVE PCT-COMPANY                TO DB-COMPANY
699800         MOVE PCT-EMPLOYEE               TO DB-EMPLOYEE
699900         PERFORM 840-MODIFY-EMPSET1
700000         IF (PCT-EFFECT-DATE   < EMP-PEND-ACT-DATE)
700100         OR (EMP-PEND-ACT-DATE = ZEROS)
700200             MOVE PCT-EFFECT-DATE        TO EMP-PEND-ACT-DATE
700300         END-IF
700400         PERFORM 820-STORE-EMPLOYEE
700500     ELSE
               IF  (PA52F5-PCT-PROCESS-TYPE = "3")
                   MOVE PA52F5-MERGE-ACT-OBJ-ID    TO DB-OBJ-ID
                                                      PA52WS-OBJ-ID
                                                      PA52F5-PAH-OBJ-ID
                   PERFORM 840-MODIFY-PAHSET2
                   IF  (PA52F5-PCT-ANT-END-DATE NOT = ZEROES)
                       MOVE PA52F5-PCT-ANT-END-DATE TO PAH-ANT-END-DATE
                   END-IF
                   IF  (PA52F5-PCT-REASON (1) NOT = SPACES)
                       MOVE PA52F5-PCT-REASON (1)  TO PAH-REASON (1)
                   END-IF
                   IF  (PA52F5-PCT-REASON (2) NOT = SPACES)
                       MOVE PA52F5-PCT-REASON (2)  TO PAH-REASON (2)
                   END-IF
                   IF  (PA52F5-USER-ID NOT = SPACES)
                       MOVE PA52F5-USER-ID         TO PAH-USER-ID
                   ELSE
                       MOVE CRT-USER-NAME          TO PAH-USER-ID
                   END-IF

                   MOVE WS-SYSTEM-DATE-YMD         TO PAH-DATE-STAMP
P80029             MOVE HHMMSS                     TO PAH-TIME-STAMP
J08104             IF (PA52F5-FC = "A")
J08104                 IF  (PA52F5-USER-ID NOT = SPACES)
J08104                     MOVE PA52F5-USER-ID     TO PAH-CREATE-USER
J08104                 ELSE 
J08104                     MOVE CRT-USER-NAME      TO PAH-CREATE-USER
J08104                 END-IF
J08104                 MOVE WS-SYSTEM-DATE-YMD     TO PAH-CREATE-DATE
J08104                 MOVE HHMMSS                 TO PAH-CREATE-TIME
J08104             END-IF
                   MOVE PA52F5-PCT-UPDATE-BENEFIT  TO PAH-UPDATE-BENEFIT
                   MOVE PA52F5-PCT-UPDATE-REQ-DED  TO PAH-UPDATE-REQ-DED
                   MOVE PA52F5-PCT-EDM-EFFECT-DT   TO PAH-EDM-EFFECT-DT
                   MOVE PA52F5-PCT-EDM-END-DATE    TO PAH-EDM-END-DATE
                   MOVE PA52F5-PCT-UPD-ABS-MGMT    TO PAH-UPD-ABS-MGMT
                   MOVE PA52F5-PCT-HIST-CORR-FLAG  TO PAH-HIST-CORR-FLAG
                   MOVE "1"                        TO PAH-ACTION-UPD
                   PERFORM 820-STORE-PERSACTHST
P78653             MOVE PAH-ACTION-NBR             TO PA52WS-ACTION-NBR
               ELSE
084400             MOVE "PAACT"                    TO IFOBIWS-OBJ-TYPE
084500             PERFORM 7000-ASSIGN-OBJ-ID-70
084600             MOVE IFOBIWS-OBJ-ID             TO PA52WS-OBJ-ID
700800             MOVE PA52WS-OBJ-ID              TO PA52F5-PAH-OBJ-ID
084700             PERFORM 800-CREATE-PERSACTHST
084800             MOVE PA52F5-PCT-COMPANY         TO PAH-COMPANY
084900             MOVE "L"                        TO PAH-ACTION-TYPE
085000             MOVE PA52F5-PCT-ACTION-CODE     TO PAH-ACTION-CODE
085100             MOVE PA52WS-ACTION-NBR          TO PAH-ACTION-NBR
085200             MOVE PA52F5-PCT-EFFECT-DATE     TO PAH-EFFECT-DATE
085300             MOVE PA52F5-PCT-EMPLOYEE        TO PAH-EMPLOYEE
085400             MOVE PA52F5-PCT-ANT-END-DATE    TO PAH-ANT-END-DATE
085500             MOVE PA52F5-PCT-REASON (1)      TO PAH-REASON (1)
085600             MOVE PA52F5-PCT-REASON (2)      TO PAH-REASON (2)
                   IF  (PA52F5-USER-ID NOT = SPACES)
                       MOVE PA52F5-USER-ID         TO PAH-USER-ID
                   ELSE
                       MOVE CRT-USER-NAME          TO PAH-USER-ID
                   END-IF
085800             MOVE WS-SYSTEM-DATE-YMD         TO PAH-DATE-STAMP
P80029             MOVE HHMMSS                     TO PAH-TIME-STAMP
J08104             IF (PA52F5-FC = "A")
J08104                 IF  (PA52F5-USER-ID NOT = SPACES)
J08104                     MOVE PA52F5-USER-ID     TO PAH-CREATE-USER
J08104                 ELSE 
J08104                     MOVE CRT-USER-NAME      TO PAH-CREATE-USER
J08104                 END-IF
J08104                 MOVE WS-SYSTEM-DATE-YMD     TO PAH-CREATE-DATE
J08104                 MOVE HHMMSS                 TO PAH-CREATE-TIME
J08104             END-IF
085900             MOVE PA52WS-OBJ-ID              TO PAH-OBJ-ID
                   MOVE PA52F5-POS-LEVEL           TO PAH-POS-LEVEL
                   MOVE PA52F5-PCT-UPDATE-BENEFIT  TO PAH-UPDATE-BENEFIT
                   MOVE PA52F5-PCT-UPDATE-REQ-DED  TO PAH-UPDATE-REQ-DED
                   MOVE PA52F5-PCT-EDM-EFFECT-DT   TO PAH-EDM-EFFECT-DT
                   MOVE PA52F5-PCT-EDM-END-DATE    TO PAH-EDM-END-DATE
                   MOVE PA52F5-PCT-UPD-ABS-MGMT    TO PAH-UPD-ABS-MGMT
                   MOVE PA52F5-PCT-HIST-CORR-FLAG  TO PAH-HIST-CORR-FLAG
                   MOVE "1"                        TO PAH-ACTION-UPD
P80029             MOVE WS-SYSTEM-DATE-YMD         TO PAH-CREATE-DATE
P80029             MOVE HHMMSS                     TO PAH-CREATE-TIME
P80029             MOVE CRT-USER-NAME              TO PAH-CREATE-USER
090300             PERFORM 820-STORE-PERSACTHST
               END-IF

702700         MOVE PA52F5-PCT-COMPANY     TO PADT-COMPANY
702800         MOVE PA52F5-PCT-EMPLOYEE    TO PADT-EMPLOYEE
702900         MOVE EMP-TERM-DATE          TO PADT-END-DATE
703000         MOVE PA52F5-POS-LEVEL       TO PADT-POS-LEVEL
703100         MOVE PA52F5-PCT-EFFECT-DATE TO PADT-EFFECT-DATE
703200         PERFORM 2300-PADT-DATE-CHECK
703300         IF (NO-ERROR-FOUND)
703400             MOVE PADT-UPDPEP-DATE   TO PAPEP-UPDPEP-DATE
703500                                        HREMP-UPDPEP-DATE
703600         END-IF
               IF  (PA52F5-PCT-PROCESS-TYPE = "4")
                   MOVE PA52F5-MERGE-ACT-OBJ-ID
                                           TO HREMP-ACT-OBJ-ID-REV
               END-IF
825600         MOVE PA52WS-OBJ-ID              TO HREMP-ACT-OBJ-ID
703700         IF   (PA52F5-POS-LEVEL      > 01)
703800         OR  ((PA52F5-POS-LEVEL      = 01)
703900         AND  (PA52WS-HREMP-NO-FLDS-FOUND))
704200             MOVE WS-TRUE                TO PAPEP-FROM-ACTION-SW
                   MOVE "N"                    TO HRWS-ADD
                   IF (PA52F5-POS-LEVEL = 01)
                       SET HREMP-UPDATE-M3         TO TRUE
                   END-IF
704400             PERFORM 3000-PAPEP-PROCESS-TRAN
                   IF  (PA52F5-POS-LEVEL   = 01)
                   AND (PA52F5-PCT-UPDATE-REQ-DED  = "Y")
                       MOVE PA52F5-PCT-COMPANY     TO PRRQC-COMPANY
                       MOVE PA52F5-PCT-EMPLOYEE    TO PRRQC-EMPLOYEE
                       INITIALIZE PRRQC-DFT-MAR-STAT
                                  PRRQC-DFT-EXEMPTS
                       MOVE PA52F5-PCT-EDM-EFFECT-DT
                                                   TO PRRQC-EFFECT-DATE
                       MOVE PA52F5-PCT-EDM-END-DATE
                                                   TO PRRQC-END-DATE
                       INITIALIZE PRRQC-UPDATE-OPTION
                       PERFORM 500-REQ-DED-CREATION
                   END-IF
704500         ELSE
704600             PERFORM 600-UPDATE-EMPLOYEE
704700             THRU    600-END
704800             PERFORM 610-CREATE-PARTICIPNT
704900             THRU    610-END.
705000
705300     PERFORM 920-AUDIT-END.
705400
      *---------------------------------+
      * Release Work Unit               |
      *---------------------------------+

           IF (PAT-WORKFLOW-FLAG       = "Y")
               INITIALIZE CRT-MESSAGE
               INITIALIZE WFAPI-INPUT
               MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT
               INITIALIZE WFAPI-OUTPUT
               PERFORM 1000-WF-RELEASE-SETUP
050900     END-IF.

705500     MOVE EMP-LAST-NAME                  TO HRWS-LAST-NAME.
705600     MOVE EMP-FIRST-NAME                 TO HRWS-FIRST-NAME.
705700     MOVE EMP-MIDDLE-INIT                TO HRWS-MIDDLE-INIT.
           MOVE EMP-NAME-SUFFIX                TO HRWS-NAME-SUFFIX.
           MOVE EMP-LAST-NAME-PRE              TO HRWS-LAST-NAME-PRE.
705800     PERFORM 750-HR-FORMAT-NAME.
705900     MOVE HRWS-FORMAT-NAME               TO PA52F5-EMP-NAME.

           MOVE PRS-COMPANY               TO DB-COMPANY.
           MOVE SPACES                    TO DB-COUNTRY-CD-REQ
                                             DB-PROCESS-LEVEL.
      * 50 = Social Number
           MOVE HREMP-FICA-NBR-DN         TO DB-FLD-NBR.
           PERFORM 840-FIND-PASSET1.
           IF (PASCRTY-FOUND)
               MOVE PAS-SEC-LEVEL         TO HRWS-SEC-LEVEL
               PERFORM 730-HR-FIELD-SECURITY
               IF (HRWS-FLD-SECURED)
                   MOVE SPACES            TO PA52F5-EMP-FICA-NBR
               ELSE
                   MOVE EMP-FICA-NBR      TO PA52F5-EMP-FICA-NBR.

706200     MOVE PA52F5-PCT-COMPANY             TO DB-COMPANY.
706300     MOVE PA52F5-PCT-EMPLOYEE            TO DB-EMPLOYEE.
706400     MOVE PA52F5-POS-LEVEL               TO DB-POS-LEVEL.
P58592     MOVE PEPSET3-POS-LEVEL              TO WS-DB-BEG-RNG.
P58592     PERFORM 850-FIND-BEGRNG-PEPSET3.
706700     IF  (PAEMPPOS-FOUND)
707100         IF (PEP-END-DATE = ZEROES)
707200             INITIALIZE                     PA52F5-PCT-END-DATE
707300         ELSE
707400             MOVE PEP-END-DATE           TO HRWS-DATE-8-FIELD
707500             PERFORM 781-HR-FORMAT-DATE-FIELD
707600             MOVE HRWS-VALUE             TO PA52F5-PCT-END-DATE
707700         END-IF
707800         PERFORM 484-MOVE-CURRENT-DATA
707900         THRU    484-END
708000             VARYING I1 FROM 2 BY 1
708100             UNTIL  (I1 > 37).
708200
708300     IF (PA52F5-IMMEDIATE-ACTION = "N")
708400         PERFORM 481-MOVE-TO-SCREEN
708500         THRU    481-END

J40356         IF ((PA52WS-L-LINE-FC = "X")
J40356         AND (PA52WS-LTM-SELECT-CLOSE))
J40356              MOVE PA52WS-L-COMPANY           TO DB-COMPANY
J40356              MOVE PA52WS-L-EMPLOYEE          TO DB-EMPLOYEE
J40356              MOVE PA52WS-L-STATUS            TO DB-STATUS
J40356              MOVE PA52WS-L-DATE-STAMP        TO DB-LTM-DATE-STAMP
J40356              MOVE PA52WS-L-TIME-STAMP        TO DB-LTM-TIME-STAMP
J40356              MOVE PA52WS-L-ERROR-FLAG        TO DB-ERROR-FLAG
J40356              MOVE PA52WS-L-WORKASSIGNMENT    TO DB-WORKASSIGNMENT
J40356              MOVE PA52WS-L-ACTION-CODE       TO DB-ACTION-CODE
J40356              PERFORM 910-AUDIT-BEGIN
J40356              PERFORM 840-MODIFY-LEMSET2
J40356              MOVE 9                          TO LEM-STATUS
J40356              PERFORM 820-STORE-HREMPRCVR
J40356              PERFORM 920-AUDIT-END
J40356         END-IF
J40356
J40356         SET PA52WS-LTM-SELECT-OPEN           TO TRUE
093100         MOVE CRT-ADD-COMPLETE                TO CRT-MESSAGE
093200         GO TO 410-END.
708800
708900     MOVE "PA561"                        TO CRT-SCREEN-CODE.
709000     MOVE SPACES                         TO CRT-PASS-FC.
P40376     MOVE SPACES                         TO PA52F5-PGM-NAME.  
709100     MOVE "A"                            TO CRT-DISPLAY-FC.
709200     MOVE "L"                            TO PA52F5-PT-ACTION-TYPE.
709300     MOVE PA52WS-ACTION-NBR              TO PA52F5-PCT-ACTION-NBR.
           IF (HRWS-GROUP-SIZE-ERROR)
               MOVE 260                        TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
           ELSE
           IF (PA52F5-PCT-UPDATE-REQ-DED = "Y")
               MOVE 406                        TO CRT-MSG-NBR
           ELSE
709500         MOVE 405                        TO CRT-MSG-NBR.

709600     MOVE CRT-MANUAL-CF                  TO CRT-REQUEST.
709700
709800 410-END.
709900
710000******************************************************************
710100 420-CHANGE.
710200******************************************************************
710300
710400     PERFORM 910-AUDIT-BEGIN.
710500     IF (DMS-ABORTED)
710600         GO TO 420-END.
710700
           IF  (PA52F5-PCT-PROCESS-TYPE = "1")
               MOVE PA52F5-PCT-COMPANY         TO PAACT-COMPANY
               MOVE "E"                        TO PAACT-ACTION-TYPE
               MOVE PA52F5-PCT-EMPLOYEE        TO PAACT-EMPLOYEE
               MOVE PA52F5-PCT-ACTION-CODE     TO PAACT-ACTION-CODE
               MOVE PA52F5-PCT-NEW-EFFECT-DATE  TO PAACT-EFFECT-DATE
               MOVE "Y"                        TO PAACT-HISTORY
               PERFORM 2300-PAACT-ACTION-NBR
               MOVE PAACT-ACTION-NBR           TO PA52WS-ACTION-NBR
               MOVE "L"                        TO PAACT-ACTION-TYPE
               PERFORM 2300-PAACT-ACTION-NBR
               IF  (PAACT-ACTION-NBR > PA52WS-ACTION-NBR)
                   MOVE PAACT-ACTION-NBR       TO PA52WS-ACTION-NBR
               END-IF

095800         MOVE PA52F5-PCT-COMPANY         TO DB-COMPANY
095900         MOVE "L"                        TO DB-ACTION-TYPE
096000         MOVE PA52F5-PCT-EFFECT-DATE     TO DB-EFFECT-DATE
096100         MOVE PA52F5-PCT-ACTION-CODE     TO DB-ACTION-CODE
096200         MOVE PA52F5-PCT-EMPLOYEE        TO DB-EMPLOYEE
096300         MOVE PA52F5-PCT-ACTION-NBR      TO DB-ACTION-NBR
               PERFORM 840-MODIFY-PCTSET1
               PERFORM 830-DELETE-PERSACTION
               PERFORM 810-RECREATE-PERSACTION
               IF  (PA52F5-PCT-UPDATE-REQ-DED  = "Y")
               AND (PA52F5-PCT-EDM-EFFECT-DT   = PA52F5-PCT-EFFECT-DATE)
                   MOVE PA52F5-PCT-NEW-EFFECT-DATE
                                               TO PCT-EDM-EFFECT-DT
                                              PA52F5-PCT-EDM-EFFECT-DT
                   MOVE PCT-EDM-EFFECT-DT      TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   SUBTRACT 1                  FROM WSDR-JULIAN-DAYS
                   PERFORM 900-JULIAN-TO-DATE
                   MOVE WSDR-FR-DATE           TO
                                               PCT-EDM-END-DATE
                                               PA52F5-PCT-EDM-END-DATE
               END-IF
               MOVE PA52F5-PCT-NEW-EFFECT-DATE TO PCT-EFFECT-DATE
               MOVE PA52WS-ACTION-NBR          TO PCT-ACTION-NBR
               PERFORM 820-STORE-PERSACTION

               MOVE PA52F5-PCT-COMPANY         TO DB-COMPANY
               MOVE 0                          TO DB-EMP-APP
               MOVE "PA"                       TO DB-CMT-TYPE
               MOVE PA52F5-PCT-ACTION-CODE     TO DB-ACTION-CODE
               MOVE PA52F5-PCT-EFFECT-DATE     TO DB-DATE
               MOVE PA52F5-PCT-EMPLOYEE        TO DB-EMPLOYEE
               INITIALIZE                         DB-JOB-CODE
               MOVE PA52F5-PCT-ACTION-NBR      TO DB-LN-NBR
               MOVE PACSET2-LN-NBR             TO WS-DB-BEG-RNG
               PERFORM 850-MODIFY-BEGRNG-PACSET2
               PERFORM
                   UNTIL (PACOMMENTS-NOTFOUND)
                   PERFORM 830-DELETE-PACOMMENTS
                   PERFORM 810-RECREATE-PACOMMENTS
                   MOVE PA52F5-PCT-NEW-EFFECT-DATE  TO PAC-DATE
                   MOVE PA52WS-ACTION-NBR          TO PAC-LN-NBR
                   PERFORM 820-STORE-PACOMMENTS
                   PERFORM 860-MODIFY-NXTRNG-PACSET2
               END-PERFORM
               INITIALIZE PA52F5-PCT-NEW-EFFECT-DATE
                          PA52F5-PCT-PROCESS-TYPE
           ELSE
095800         MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY
095900         MOVE "L"                    TO DB-ACTION-TYPE
096000         MOVE PA52F5-PCT-EFFECT-DATE TO DB-EFFECT-DATE
096100         MOVE PA52F5-PCT-ACTION-CODE TO DB-ACTION-CODE
096200         MOVE PA52F5-PCT-EMPLOYEE    TO DB-EMPLOYEE
096300         MOVE PA52F5-PCT-ACTION-NBR  TO DB-ACTION-NBR
096500         PERFORM 840-MODIFY-PCTSET1
               PERFORM 8400-AFTER-FIND-PCT
096600         PERFORM 500-MOVE-DATA
096700         THRU    500-END
096800         PERFORM 8200-STORE-PERSACTION
           END-IF.
712400
712500     PERFORM 920-AUDIT-END.
712600
712700     MOVE EMP-LAST-NAME          TO HRWS-LAST-NAME.
712800     MOVE EMP-FIRST-NAME         TO HRWS-FIRST-NAME.
712900     MOVE EMP-MIDDLE-INIT        TO HRWS-MIDDLE-INIT.
           MOVE EMP-NAME-SUFFIX        TO HRWS-NAME-SUFFIX.
           MOVE EMP-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE.
713000     PERFORM 750-HR-FORMAT-NAME.
713100     MOVE HRWS-FORMAT-NAME       TO PA52F5-EMP-NAME.

           MOVE PRS-COMPANY               TO DB-COMPANY.
           MOVE SPACES                    TO DB-COUNTRY-CD-REQ
                                             DB-PROCESS-LEVEL.
      * 50 = Social Number
           MOVE HREMP-FICA-NBR-DN         TO DB-FLD-NBR.
           PERFORM 840-FIND-PASSET1.
           IF (PASCRTY-FOUND)
               MOVE PAS-SEC-LEVEL         TO HRWS-SEC-LEVEL
               PERFORM 730-HR-FIELD-SECURITY
               IF (HRWS-FLD-SECURED)
                   MOVE SPACES            TO PA52F5-EMP-FICA-NBR
               ELSE
                   MOVE EMP-FICA-NBR      TO PA52F5-EMP-FICA-NBR.

713300
713400     MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY.
713500     MOVE PA52F5-PCT-EMPLOYEE    TO DB-EMPLOYEE.
713600     MOVE PA52F5-POS-LEVEL       TO DB-POS-LEVEL.
P58592     MOVE PEPSET3-POS-LEVEL      TO WS-DB-BEG-RNG.
P58592     PERFORM 850-FIND-BEGRNG-PEPSET3.
713900     IF  (PAEMPPOS-FOUND)
714300         IF (PEP-END-DATE = ZEROES)
714400             INITIALIZE                     PA52F5-PCT-END-DATE
714500         ELSE
714600             MOVE PEP-END-DATE           TO HRWS-DATE-8-FIELD
714700             PERFORM 781-HR-FORMAT-DATE-FIELD
714800             MOVE HRWS-VALUE             TO PA52F5-PCT-END-DATE
714900         END-IF
715000         PERFORM 484-MOVE-CURRENT-DATA
715100         THRU    484-END
715200             VARYING I1 FROM 2 BY 1
715300             UNTIL  (I1 > 37) 
715400
715500     PERFORM 481-MOVE-TO-SCREEN
715600     THRU    481-END.
715700*    MOVE CRT-CHG-COMPLETE                       TO CRT-MESSAGE.
J40356
J40356     IF ((PA52WS-L-LINE-FC = "X")
J40356     AND (PA52WS-LTM-SELECT-CLOSE))
J40356          MOVE PA52WS-L-COMPANY           TO DB-COMPANY
J40356          MOVE PA52WS-L-EMPLOYEE          TO DB-EMPLOYEE
J40356          MOVE PA52WS-L-STATUS            TO DB-STATUS
J40356          MOVE PA52WS-L-DATE-STAMP        TO DB-LTM-DATE-STAMP
J40356          MOVE PA52WS-L-TIME-STAMP        TO DB-LTM-TIME-STAMP
J40356          MOVE PA52WS-L-ERROR-FLAG        TO DB-ERROR-FLAG
J40356          MOVE PA52WS-L-WORKASSIGNMENT    TO DB-WORKASSIGNMENT
J40356          MOVE PA52WS-L-ACTION-CODE       TO DB-ACTION-CODE
J40356          PERFORM 910-AUDIT-BEGIN
J40356          PERFORM 840-MODIFY-LEMSET2
J40356          MOVE 9                          TO LEM-STATUS
J40356          PERFORM 820-STORE-HREMPRCVR
J40356          PERFORM 920-AUDIT-END
J40356     END-IF.
J40356
J40356     SET PA52WS-LTM-SELECT-OPEN           TO TRUE.
098700     MOVE CRT-CHG-COMPLETE                TO CRT-MESSAGE.
715800
715900 420-END.
716000
099100******************************************************************
       430-CREATE-WORKFLOW-TRIGGER.
099100******************************************************************

      *---------------------------------+
      * Check Status                    |
      *---------------------------------+
      **** Check to see if workflow is enabled for a service
           INITIALIZE WFAPI-INPUT
                      WFAPI-OUTPUT.

           MOVE PA52F5-PCT-ACTION-CODE TO WFAPI-I-SERVICE.

           MOVE PA52F5-PCT-COMPANY     TO WFAPI-I-CRITERION-1.
P56312     MOVE HREMP-PROCESS-LEVEL-DN TO PA52WS-FLD-NBR.
P56312     PERFORM 432-FIND-FIELD-USAGE
P56312     THRU    432-END.
P56312     IF (FIELD-USED)
P56312         MOVE PA52F5-PCT-NEW-PL  TO WFAPI-I-CRITERION-2
P56312     END-IF.
P56312     IF (WFAPI-I-CRITERION-2     = PA52WS-BLANK)
P56312         INITIALIZE WFAPI-I-CRITERION-2
P56312     ELSE
P56312         IF (WFAPI-I-CRITERION-2     = SPACES)
P56312             MOVE EMP-PROCESS-LEVEL TO WFAPI-I-CRITERION-2
P56312         END-IF
P56312     END-IF.
P56312
P56312     MOVE HREMP-DEPARTMENT-DN    TO PA52WS-FLD-NBR.
P56312     PERFORM 432-FIND-FIELD-USAGE
P56312     THRU    432-END.
P56312     IF (FIELD-USED)
P56312         MOVE PA52F5-PCT-NEW-DEPARTMENT  TO WFAPI-I-CRITERION-3
P56312     END-IF.
P56312     IF (WFAPI-I-CRITERION-3     = PA52WS-BLANK)
P56312         INITIALIZE WFAPI-I-CRITERION-3
P56312     ELSE
P56312         IF (WFAPI-I-CRITERION-3     = SPACES)
P56312             MOVE EMP-DEPARTMENT TO WFAPI-I-CRITERION-3
P56312         END-IF
P56312     END-IF.
           PERFORM 1000-WF-SERVICE.
           IF (WFAPI-O-RETURN-CODE     NOT = ZEROES)
               GO TO 430-END.

      *---------------------------------+
      * Create Work Unit Header         |
      *---------------------------------+
      **** This creates data in WF20 and matches with fields in WF04
      **** Perform CREATE WORK Routine
           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-SERVICE        TO WFAPI-I-SERVICE.
           MOVE WFAPI-O-AGENT          TO WFAPI-I-AGENT.
           MOVE WFAPI-O-PROCEDURE      TO WFAPI-I-PROCEDURE.
           MOVE PA52WS-WORK-PRIORITY   TO WFAPI-I-WORK-PRIORITY.
           MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY.
           MOVE PA52F5-PCT-ACTION-CODE TO DB-ACTION-CODE.
           PERFORM 840-FIND-PATSET1.
           MOVE PAT-DESCRIPTION        TO WFAPI-I-WORK-TITLE.
           MOVE PA52WS-PCTSET1         TO WFAPI-I-OBJECT-NAME.
           MOVE PA52F5-PCT-COMPANY     TO WFAPI-I-KEY-VALUE (1).
           MOVE PA52F5-PT-ACTION-TYPE  TO WFAPI-I-KEY-VALUE (2).
           MOVE PA52F5-PCT-EFFECT-DATE TO WFAPI-I-KEY-VALUE (3).
           MOVE PA52F5-PCT-ACTION-CODE TO WFAPI-I-KEY-VALUE (4).
           MOVE PA52F5-PCT-EMPLOYEE    TO WFAPI-I-KEY-VALUE (5).
           MOVE PA52WS-ACTION-NBR      TO WFAPI-I-KEY-VALUE (6).
           INITIALIZE WFAPI-I-WORK-CATEGORY
                      WFAPI-I-WORK-CAT-VALUE
                      WFAPI-OUTPUT.
           PERFORM 1000-WF-CREATE-SETUP.

      *---------------------------------+
      * Create Work Unit Variable       |
      *---------------------------------+
      **** Perform ADD VARIABLE Routine
           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.

      **** COMPANY
           MOVE 501                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (1).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (1).
           MOVE PA52F5-PCT-COMPANY     TO WFAPI-I-VARIABLE-VAL (1).

      **** CHECK TO SEE IF PROCESS LEVEL CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-PROCESS-LEVEL-DN TO PA52WS-FLD-NBR.
           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.

      **** OLD PROCESS LEVEL
           MOVE 502                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (2).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (2).
           IF (I1                      > 36)
           OR (PAT-FLD-NBR (I1)        = ZEROES)
               INITIALIZE WFAPI-I-VARIABLE-VAL (2)
           ELSE
               MOVE PA52F5-PCT-PL      TO WFAPI-I-VARIABLE-VAL (2).

      **** NEW PROCESS LEVEL
           MOVE 503                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (3).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (3).
           IF (I1                      > 36)
           OR (PAT-FLD-NBR (I1)        = ZEROES)
               INITIALIZE WFAPI-I-VARIABLE-VAL (3)
           ELSE
               MOVE PA52F5-PCT-NEW-PL  TO WFAPI-I-VARIABLE-VAL (3).

           IF (WFAPI-I-VARIABLE-VAL (3) = PA52WS-BLANK)
               INITIALIZE WFAPI-I-VARIABLE-VAL (3)
           ELSE
           IF (WFAPI-I-VARIABLE-VAL (3) = SPACES)
               MOVE WFAPI-I-VARIABLE-VAL (2)
                                       TO WFAPI-I-VARIABLE-VAL (3).

      **** CHECK TO SEE IF DEPARTMENT CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-DEPARTMENT-DN    TO PA52WS-FLD-NBR.
           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.

      **** OLD DEPARTMENT
           MOVE 504                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (4).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (4).
           IF (I1                      > 36)
           OR (PAT-FLD-NBR (I1)        = ZEROES)
               INITIALIZE WFAPI-I-VARIABLE-VAL (4)
           ELSE
               MOVE PA52F5-PCT-DEPARTMENT
                                       TO WFAPI-I-VARIABLE-VAL (4).

      **** NEW DEPARTMENT
           MOVE 505                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (5).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (5).
           IF (I1                      > 36)
           OR (PAT-FLD-NBR (I1)        = ZEROES)
               INITIALIZE WFAPI-I-VARIABLE-VAL (5)
           ELSE
               MOVE PA52F5-PCT-NEW-DEPARTMENT
                                       TO WFAPI-I-VARIABLE-VAL (5).

           IF (WFAPI-I-VARIABLE-VAL (5) = PA52WS-BLANK)
               INITIALIZE WFAPI-I-VARIABLE-VAL (5)
           ELSE
           IF (WFAPI-I-VARIABLE-VAL (5) = SPACES)
               MOVE WFAPI-I-VARIABLE-VAL (4)
                                       TO WFAPI-I-VARIABLE-VAL (5).

      **** FIND OLD SUPERVISOR AND NEXT OLD SUPERVISOR ****
           INITIALIZE PA52WS-OLD-SUPER-EMP
                      PA52WS-NEXT-OLD-SUPER-EMP.
           IF (EMP-SUPERVISOR            NOT = SPACES)
               MOVE EMP-SUPERVISOR       TO DB-CODE
               PERFORM 840-FIND-HSUSET1
               IF (HSU-EMPLOYEE          = ZEROES)
                   PERFORM 439-FIND-SUPER-EMP
                   THRU    439-END
               END-IF
               IF  (HRSUPER-FOUND)
               AND (HSU-EMPLOYEE         NOT = ZEROES)
                   MOVE HSU-EMPLOYEE     TO PA52WS-OLD-SUPER-EMP
               END-IF
               IF (PA52WS-OLD-SUPER-EMP  NOT = ZEROES)
                   MOVE HSU-SUPER-RPTS-TO TO DB-CODE
                   PERFORM 840-FIND-HSUSET1
                   IF (HRSUPER-FOUND)
                       IF (HSU-EMPLOYEE      = ZEROES)
                           PERFORM 439-FIND-SUPER-EMP
                           THRU    439-END
                       END-IF
                       IF (HSU-EMPLOYEE     NOT = ZEROES)
                           MOVE HSU-EMPLOYEE 
                                          TO PA52WS-NEXT-OLD-SUPER-EMP.

      **** OLD SUPERVISOR EMPLOYEE
           MOVE 506                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (6).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (6).
           MOVE PA52WS-OLD-SUPER-EMP   TO WFAPI-I-VARIABLE-VAL (6).

      **** NEXT OLD SUPERVISOR EMPLOYEE
           MOVE 523                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (7).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (7).
           MOVE PA52WS-NEXT-OLD-SUPER-EMP TO WFAPI-I-VARIABLE-VAL (7).

      **** FIND NEW SUPERVISOR AND NEXT NEW SUPERVISOR ****
           INITIALIZE PA52WS-NEW-SUPER-EMP
                      PA52WS-NEXT-NEW-SUPER-EMP.
           IF (PA52F5-PCT-NEW-SUPERVISOR NOT = SPACES)
               MOVE PA52F5-PCT-NEW-SUPERVISOR  TO DB-CODE
               IF (DB-CODE NOT = SPACES AND "*BLANK")
054800             PERFORM 840-FIND-HSUSET1
                   IF (HRSUPER-FOUND)
                       IF (HSU-EMPLOYEE          = ZEROES)
                           PERFORM 439-FIND-SUPER-EMP
                           THRU    439-END
                       END-IF
                       IF (HSU-EMPLOYEE         NOT = ZEROES)
                           MOVE HSU-EMPLOYEE     TO PA52WS-NEW-SUPER-EMP
                       END-IF
                   END-IF
                   IF (PA52WS-NEW-SUPER-EMP  NOT = ZEROES)
                       MOVE HSU-SUPER-RPTS-TO TO DB-CODE
                       PERFORM 840-FIND-HSUSET1
                       IF (HRSUPER-FOUND)
                           IF (HSU-EMPLOYEE      = ZEROES)
                               PERFORM 439-FIND-SUPER-EMP
                               THRU    439-END
                           END-IF
                           IF (HSU-EMPLOYEE     NOT = ZEROES)
                               MOVE HSU-EMPLOYEE
                                           TO PA52WS-NEXT-NEW-SUPER-EMP.

           IF  (PA52WS-NEW-SUPER-EMP      = ZEROES)
           AND (DB-CODE               NOT = "*BLANK")
               MOVE PA52WS-OLD-SUPER-EMP TO PA52WS-NEW-SUPER-EMP.

           IF  (PA52WS-NEXT-NEW-SUPER-EMP = ZEROES)
           AND (DB-CODE               NOT = "*BLANK")
               MOVE PA52WS-NEXT-OLD-SUPER-EMP
                                         TO PA52WS-NEXT-NEW-SUPER-EMP.

      **** NEW SUPERVISOR EMPLOYEE
           MOVE 507                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (8).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (8).
           MOVE PA52WS-NEW-SUPER-EMP   TO WFAPI-I-VARIABLE-VAL (8).

      **** NEXT NEW SUPERVISOR EMPLOYEE
           MOVE 524                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (9).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (9).
           MOVE PA52WS-NEXT-NEW-SUPER-EMP TO WFAPI-I-VARIABLE-VAL (9).

      **** CHECK TO SEE IF ANY SALARY RELATED FIELDS ARE CHANGED ****
           INITIALIZE PA52WS-PAY-RATE
                      PA52WS-SALARY-CLASS
                      PA52WS-NBR-FTE
                      PA52WS-SCHEDULE
                      PA52WS-PAY-STEP
                      PA52WS-PAY-GRADE
                      PA52WS-ANNUAL-HOURS
                      PA52WS-JOB-CODE.

      **** CHECK ANNUAL HOURS ****
           IF (PA52F5-PCT-NEW-ANNUAL-HOURS NOT = SPACES)
               MOVE PA52F5-PCT-NEW-ANNUAL-HOURS
                                            TO PAPCTIO-NEW-VALUE
               MOVE "N"                     TO PAPCTIO-DATA-TYPE
               MOVE 4                       TO PAPCTIO-DATA-DECIMALS
               MOVE "N"                     TO PAPCTIO-DATA-CURR
               PERFORM 9100-FORMAT-PAPCT-TO-STORAGE
               MOVE PAPCTIO-NEW-NUM-S       TO PA52WS-ANNUAL-HOURS.

      **** CHECK PAY RATE ****
           IF (PA52F5-PCT-NEW-PAY-RATE NOT = SPACES)
               MOVE PA52F5-PCT-NEW-PAY-RATE TO PAPCTIO-NEW-VALUE
               MOVE "N"                     TO PAPCTIO-DATA-TYPE
               MOVE 4                       TO PAPCTIO-DATA-DECIMALS
               MOVE "N"                     TO PAPCTIO-DATA-CURR
               PERFORM 9100-FORMAT-PAPCT-TO-STORAGE
               MOVE PAPCTIO-NEW-NUM-S       TO PA52WS-PAY-RATE.

      **** CHECK SALARY CLASS ****
           IF (PA52F5-PCT-NEW-SALARY-CLASS NOT = SPACES)
               MOVE PA52F5-PCT-NEW-SALARY-CLASS TO PA52WS-SALARY-CLASS.

      **** CHECK NBR FTE ****
           IF (PA52F5-PCT-NEW-FTE      NOT = SPACES)
               MOVE PA52F5-PCT-NEW-FTE TO PAPCTIO-NEW-VALUE
               MOVE "N"                TO PAPCTIO-DATA-TYPE
               MOVE 6                  TO PAPCTIO-DATA-DECIMALS
               MOVE "N"                TO PAPCTIO-DATA-CURR
               PERFORM 9100-FORMAT-PAPCT-TO-STORAGE
               MOVE PAPCTIO-NEW-NUM-S  TO PA52WS-NBR-FTE.

      **** CHECK SCHEDULE ****
           IF (PA52F5-PCT-NEW-SCHEDULE NOT = SPACES)
               MOVE PA52F5-PCT-NEW-SCHEDULE TO PA52WS-SCHEDULE.

      **** CHECK PAY STEP ****
           IF (PA52F5-PCT-NEW-STEP      NOT = SPACES)
               MOVE PA52F5-PCT-NEW-STEP TO PAPCTIO-NEW-VALUE
               MOVE "N"                 TO PAPCTIO-DATA-TYPE
               MOVE 0                   TO PAPCTIO-DATA-DECIMALS
               MOVE "N"                 TO PAPCTIO-DATA-CURR
               PERFORM 9100-FORMAT-PAPCT-TO-STORAGE
               MOVE PAPCTIO-NEW-NUM-S   TO PA52WS-PAY-STEP.

      **** CHECK PAY GRADE ****
           IF (PA52F5-PCT-NEW-GRADE      NOT = SPACES)
               MOVE PA52F5-PCT-NEW-GRADE TO PA52WS-PAY-GRADE.

      **** CHECK JOB CODE
           IF (PA52F5-PCT-NEW-JOBCODE      NOT = SPACES)
               MOVE PA52F5-PCT-NEW-JOBCODE TO PA52WS-JOB-CODE.

           SET FIELD-NOTCHANGED        TO TRUE.
           IF (PA52WS-PAY-RATE         NOT = ZEROES)
           OR (PA52WS-SALARY-CLASS     NOT = SPACES)
           OR (PA52WS-NBR-FTE          NOT = ZEROES)
           OR (PA52WS-SCHEDULE         NOT = SPACES)
           OR (PA52WS-PAY-STEP         NOT = ZEROES)
           OR (PA52WS-PAY-GRADE        NOT = SPACES)
           OR (PA52WS-JOB-CODE         NOT = SPACES)
           OR (PA52WS-ANNUAL-HOURS     NOT = ZEROES)
               SET FIELD-CHANGED       TO TRUE
               IF (PA52WS-ANNUAL-HOURS = ZEROES)
                   MOVE EMP-ANNUAL-HOURS   TO PA52WS-ANNUAL-HOURS
               END-IF
               IF (PA52WS-PAY-RATE     = ZEROES)
                   MOVE EMP-PAY-RATE   TO PA52WS-PAY-RATE
               END-IF
               IF (PA52WS-SALARY-CLASS = SPACES)
                   MOVE EMP-SALARY-CLASS   TO PA52WS-SALARY-CLASS
               END-IF
               IF (PA52WS-NBR-FTE      = ZEROES)
                   MOVE EMP-NBR-FTE    TO PA52WS-NBR-FTE
               END-IF
               IF (PA52WS-SCHEDULE     = SPACES)
                   MOVE EMP-SCHEDULE   TO PA52WS-SCHEDULE
               END-IF
               IF (PA52WS-PAY-STEP     = ZEROES)
                   MOVE EMP-PAY-STEP   TO PA52WS-PAY-STEP
               END-IF
               IF (PA52WS-PAY-GRADE    = SPACES)
                   MOVE EMP-PAY-GRADE  TO PA52WS-PAY-GRADE
               END-IF
               IF (PA52WS-JOB-CODE     = SPACES)
                   MOVE EMP-JOB-CODE   TO PA52WS-JOB-CODE.

      **** OLD SALARY
           MOVE 508                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (10).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (10).
           IF (FIELD-NOTCHANGED)
               INITIALIZE WFAPI-I-VARIABLE-VAL (8)
           ELSE
               PERFORM 433-CALC-OLD-SALARY
               THRU    433-END
               MOVE PA52WS-OLD-SALARY    TO PA52WS-NUM-15
               MOVE WS-DECIMAL-SEPARATOR TO PA52WS-INTL-DECIMAL
               MOVE PA52WS-ALPHA-15      TO WFAPI-I-VARIABLE-VAL (10).

           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-ADD-VAR-SETUP.

           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.

      **** NEW SALARY
           MOVE 509                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (1).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (1).
           IF (FIELD-NOTCHANGED)
               INITIALIZE WFAPI-I-VARIABLE-VAL (1)
           ELSE
               PERFORM 436-CALC-NEW-SALARY
               THRU    436-END
               MOVE PA52WS-NEW-SALARY    TO PA52WS-NUM-15
               MOVE WS-DECIMAL-SEPARATOR TO PA52WS-INTL-DECIMAL
               MOVE PA52WS-ALPHA-15      TO WFAPI-I-VARIABLE-VAL (1).

      **** ACTION
           MOVE 510                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (2).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (2).
           MOVE PA52F5-PCT-ACTION-CODE TO WFAPI-I-VARIABLE-VAL (2).

      **** REASON (1)
           MOVE 511                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (3).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (3).
           MOVE PA52F5-PCT-REASON (1)  TO WFAPI-I-VARIABLE-VAL (3).

      **** REASON (2)
           MOVE 512                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (4).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (4).
           MOVE PA52F5-PCT-REASON (2)  TO WFAPI-I-VARIABLE-VAL (4).

      **** CHECK TO SEE IF EMPLOYEE STATUS CAN BE CHANGED FOR ACTION ****
           MOVE HREMP-EMP-STATUS-DN    TO PA52WS-FLD-NBR.
           PERFORM 432-FIND-FIELD-USAGE
           THRU    432-END.

      **** OLD EMPLOYEE STATUS
           MOVE 513                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (5).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (5).
           INITIALIZE WFAPI-I-VARIABLE-VAL (5).

      **** NEW EMPLOYEE STATUS
           MOVE 514                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (6).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (6).
           INITIALIZE WFAPI-I-VARIABLE-VAL (6).

           IF (WFAPI-I-VARIABLE-VAL (6) = PA52WS-BLANK)
               INITIALIZE WFAPI-I-VARIABLE-VAL (6)
           ELSE
           IF (WFAPI-I-VARIABLE-VAL (6) = SPACES)
               MOVE WFAPI-I-VARIABLE-VAL (5)
                                       TO WFAPI-I-VARIABLE-VAL (6).

      **** OVER BUDGET
           MOVE 515                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (7).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (7).
           IF (HREMP-RATE-WARN-FLAG    = 1)
           OR (PAPEP-RATE-WARN-FLAG    = 1)
               MOVE "Y"                TO WFAPI-I-VARIABLE-VAL (7)
           ELSE
               MOVE "N"                TO WFAPI-I-VARIABLE-VAL (7).

      **** PERCENT CHANGE
           MOVE 516                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (8).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (8).
           COMPUTE PA52WS-PCT-CHANGE ROUNDED
                                       = (PA52WS-NEW-SALARY
                                       /  PA52WS-OLD-SALARY
                                       *  100)
                                       - 100.
           MOVE PA52WS-PCT-CHANGE      TO PA52WS-NUM-15.
           MOVE WS-DECIMAL-SEPARATOR   TO PA52WS-INTL-DECIMAL.
           MOVE PA52WS-ALPHA-15        TO WFAPI-I-VARIABLE-VAL (8).

025320     MOVE WFAPI-MAIL-SUBJECT     TO WFAPI-I-VARIABLE-NAME (9).
           MOVE 518                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-VAL  (9).
025430     MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (9).
025440
      **** ACTION TYPE
           MOVE 519                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (10).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (10).
           MOVE PA52F5-PT-ACTION-TYPE  TO WFAPI-I-VARIABLE-VAL (10).

           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-ADD-VAR-SETUP.

           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.

      **** EFFECT DATE
           MOVE 520                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (1).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (1).
           MOVE PA52F5-PCT-EFFECT-DATE TO WFAPI-I-VARIABLE-VAL (1).

      **** EMPLOYEE
           MOVE 521                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (2).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (2).
           MOVE PA52F5-PCT-EMPLOYEE    TO WFAPI-I-VARIABLE-VAL (2).

      **** ACTION NBR
           MOVE 522                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (3).
           MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (3).
           MOVE PA52WS-ACTION-NBR      TO WFAPI-I-VARIABLE-VAL (3).

           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-ADD-VAR-SETUP.

      *---------------------------------+
      * Create Work Unit Message Header |
      *---------------------------------+
      **** Perform ADD MESSAGE HEADER Routine
           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.

           MOVE 517                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-MESSAGE-ID.
           MOVE CRT-USER-NAME          TO WFAPI-I-USER.

           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-ADD-MSGHDR-SETUP.

      *---------------------------------+
      * Create Work Unit Message Detail |
      *---------------------------------+
      **** Perform ADD MESSAGE DETAIL Routine
           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.

           MOVE 517                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-MESSAGE-ID.

           MOVE 518                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO WFAPI-I-MESSAGE.

           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-ADD-MSGDTL-SETUP.

      *---------------------------------+
      * Create Work Unit Folder         |
      *---------------------------------+
      **** This creates data in WF21 and matches with fields in WF02
      **** Perform ADD FOLDER Routine
           INITIALIZE WFAPI-INPUT.

           MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.

           MOVE CRT-SCREEN-NAME        TO PA52WS-SCREEN-NAME.
           MOVE CRT-SCREEN-NUMBER      TO PA52WS-SCREEN-NUMBER.
           MOVE PA52WS-SCREEN-CODE     TO WFAPI-I-FORM.
           MOVE PA52WS-FORM            TO WFAPI-I-DOCUMENT-ID.
           MOVE PA52F5-PCT-COMPANY     TO WFAPI-I-KEY-VALUE (1).
           MOVE PA52F5-PCT-EMPLOYEE    TO WFAPI-I-KEY-VALUE (2).
           MOVE PA52F5-PCT-ACTION-CODE TO WFAPI-I-KEY-VALUE (3).
           MOVE PA52F5-POS-LEVEL       TO WFAPI-I-KEY-VALUE (4).
           MOVE PA52F5-PCT-EFFECT-DATE TO WFAPI-I-KEY-VALUE (5).
J33429     MOVE PA52WS-ACTION-NBR      TO WFAPI-I-KEY-VALUE (6).
           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-ADD-FOLDER-SETUP.

      *---------------------------------+
      * Release Work Unit               |
      *---------------------------------+

       430-END.

099100******************************************************************
       432-FIND-FIELD-USAGE.
099100******************************************************************

           SET FIELD-NOTUSED               TO TRUE.
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36)
               OR     (PAT-FLD-NBR (I1)    = ZEROES)
               OR     (PAT-FLD-NBR (I1)    = PA52WS-FLD-NBR)

               CONTINUE
           END-PERFORM.

           IF  (I1                         <= 36)
           AND (PAT-FLD-NBR (I1)           NOT = ZEROES)
               SET FIELD-USED              TO TRUE.

       432-END.

043300******************************************************************
043400 433-CALC-OLD-SALARY.
043500******************************************************************
043600
           INITIALIZE PA52WS-OLD-SALARY.

043700     MOVE EMP-NBR-FTE                TO PA52WS-NBR-FTE-WF.
           MOVE EMP-ANNUAL-HOURS           TO PA52WS-ANNUAL-HOURS-WF.
043800
           IF  (PA52WS-ANNUAL-HOURS-WF = ZEROES)
043900         MOVE EMP-COMPANY            TO DB-COMPANY
044000         MOVE EMP-JOB-CODE           TO DB-JOB-CODE
044100         PERFORM 840-FIND-JBCSET1
044200         IF  (JOBCODE-FOUND)
044300             MOVE JBC-ANNUAL-HOURS   TO PA52WS-ANNUAL-HOURS-WF
               END-IF
           END-IF.
044400
044500     IF (PA52WS-ANNUAL-HOURS-WF = ZEROES)
044600         MOVE EMP-COMPANY            TO DB-COMPANY
044700         INITIALIZE DB-PROCESS-LEVEL
044800         PERFORM 840-FIND-PRSSET1
044900         MOVE PRS-ANNUAL-HOURS       TO PA52WS-ANNUAL-HOURS-WF
           END-IF.
045000
045100     IF (PA52WS-ANNUAL-HOURS-WF = ZEROES)
045200         MOVE 2080                   TO PA52WS-ANNUAL-HOURS-WF.
045300
045400     IF (PA52WS-NBR-FTE-WF = ZEROES)
045500         MOVE 1                      TO PA52WS-NBR-FTE-WF.
045600
045700     IF (EMP-PAY-STEP                NOT = ZEROES)
045800         PERFORM 434-CALC-FROM-STEP-GRADE
045900         THRU    434-END
046000     ELSE
046100         PERFORM 435-CALC-FROM-HR11
046200         THRU    435-END.
046300
046400 433-END.
046500
046600******************************************************************
046700 434-CALC-FROM-STEP-GRADE.
046800******************************************************************
046900
047000     MOVE EMP-COMPANY                TO DB-COMPANY.
J33429     MOVE "S"                        TO DB-INDICATOR. 
047100     MOVE EMP-SCHEDULE               TO DB-SCHEDULE.
047200     MOVE PA52F5-PCT-EFFECT-DATE     TO DB-EFFECT-DATE.
047300     PERFORM 850-FIND-NLT-SGHSET2.
047400
047500     MOVE EMP-COMPANY                TO DB-COMPANY.
047600     MOVE EMP-SCHEDULE               TO DB-SCHEDULE.
047700     MOVE EMP-PAY-GRADE              TO DB-PAY-GRADE.
047800     MOVE EMP-PAY-STEP               TO DB-PAY-STEP.
047900     MOVE SGH-EFFECT-DATE            TO DB-EFFECT-DATE.
048000     PERFORM 840-FIND-SGDSET3.
048100
048200     IF (EMP-SALARY-CLASS            = "H")
048300         COMPUTE PA52WS-OLD-SALARY ROUNDED
                                           = SGD-PAY-RATE
048400                                     * PA52WS-ANNUAL-HOURS-WF
048500                                     * PA52WS-NBR-FTE-WF
048600     ELSE
048700         MOVE SGD-PAY-RATE           TO PA52WS-OLD-SALARY.
048800
048900 434-END.
049000
049100******************************************************************
049200 435-CALC-FROM-HR11.
049300******************************************************************
049400
049500     IF (EMP-SALARY-CLASS            = "H")
049600         COMPUTE PA52WS-OLD-SALARY ROUNDED
                                           = EMP-PAY-RATE
049700                                     * PA52WS-ANNUAL-HOURS-WF
049800                                     * PA52WS-NBR-FTE-WF
049900     ELSE
050000         MOVE EMP-PAY-RATE           TO PA52WS-OLD-SALARY.
050100
050200 435-END.
050300
043300******************************************************************
043400 436-CALC-NEW-SALARY.
043500******************************************************************
043600
           INITIALIZE PA52WS-NEW-SALARY.

043700     MOVE PA52WS-NBR-FTE             TO PA52WS-NBR-FTE.
           MOVE PA52WS-ANNUAL-HOURS        TO PA52WS-ANNUAL-HOURS-WF.
043800
           IF  (PA52WS-ANNUAL-HOURS-WF = ZEROES)
043900         MOVE EMP-COMPANY            TO DB-COMPANY
044000         MOVE PA52WS-JOB-CODE        TO DB-JOB-CODE
044100         PERFORM 840-FIND-JBCSET1
044200         IF  (JOBCODE-FOUND)
044300             MOVE JBC-ANNUAL-HOURS   TO PA52WS-ANNUAL-HOURS-WF
               END-IF
           END-IF.
044400
044500     IF (PA52WS-ANNUAL-HOURS-WF = ZEROES)
044600         MOVE EMP-COMPANY            TO DB-COMPANY
044700         INITIALIZE DB-PROCESS-LEVEL
044800         PERFORM 840-FIND-PRSSET1
044900         MOVE PRS-ANNUAL-HOURS       TO PA52WS-ANNUAL-HOURS-WF
           END-IF.
045000
045100     IF (PA52WS-ANNUAL-HOURS-WF = ZEROES)
045200         MOVE 2080                   TO PA52WS-ANNUAL-HOURS-WF.
045300
045400     IF (PA52WS-NBR-FTE-WF = ZEROES)
045500         MOVE 1                      TO PA52WS-NBR-FTE-WF.
045600
045700     IF (PA52WS-PAY-STEP             NOT = ZEROES)
045800         PERFORM 437-CALC-FROM-STEP-GRADE
045900         THRU    437-END
046000     ELSE
046100         PERFORM 438-CALC-FROM-HR11
046200         THRU    438-END.
046300
046400 436-END.
046500
046600******************************************************************
046700 437-CALC-FROM-STEP-GRADE.
046800******************************************************************
046900
047000     MOVE EMP-COMPANY                TO DB-COMPANY.
J33429     MOVE "S"                        TO DB-INDICATOR.
047100     MOVE PA52WS-SCHEDULE            TO DB-SCHEDULE.
047200     MOVE PA52F5-PCT-EFFECT-DATE     TO DB-EFFECT-DATE.
047300     PERFORM 850-FIND-NLT-SGHSET2.
047400
047500     MOVE EMP-COMPANY                TO DB-COMPANY.
047600     MOVE PA52WS-SCHEDULE            TO DB-SCHEDULE.
047700     MOVE PA52WS-PAY-GRADE           TO DB-PAY-GRADE.
047800     MOVE PA52WS-PAY-STEP            TO DB-PAY-STEP.
047900     MOVE SGH-EFFECT-DATE            TO DB-EFFECT-DATE.
048000     PERFORM 840-FIND-SGDSET3.
048100
048200     IF (PA52WS-SALARY-CLASS         = "H")
048300         COMPUTE PA52WS-NEW-SALARY ROUNDED
                                           = SGD-PAY-RATE
048400                                     * PA52WS-ANNUAL-HOURS-WF
048500                                     * PA52WS-NBR-FTE-WF
048600     ELSE
048700         MOVE SGD-PAY-RATE           TO PA52WS-NEW-SALARY.
048800
048900 437-END.
049000
049100******************************************************************
049200 438-CALC-FROM-HR11.
049300******************************************************************
049400
049500     IF (PA52WS-SALARY-CLASS         = "H")
049600         COMPUTE PA52WS-NEW-SALARY ROUNDED
                                           = PA52WS-PAY-RATE
049700                                     * PA52WS-ANNUAL-HOURS-WF
049800                                     * PA52WS-NBR-FTE-WF
049900     ELSE
050000         MOVE PA52WS-PAY-RATE        TO PA52WS-NEW-SALARY.
050100
050200 438-END.
050300
099100******************************************************************
       439-FIND-SUPER-EMP.
099100******************************************************************

           PERFORM
               VARYING I5 FROM 1 BY 1
               UNTIL  (I5               > PA52WS-NBR-OF-SEARCH)
               OR     (HSU-EMPLOYEE     NOT = ZEROES)
               OR     (HSU-SUPER-RPTS-TO    = SPACES)

               MOVE HSU-SUPER-RPTS-TO  TO DB-CODE
               PERFORM 840-FIND-HSUSET1

           END-PERFORM.

       439-END.

716100******************************************************************
716200 440-DELETE.
716300******************************************************************
716400
716500     PERFORM 910-AUDIT-BEGIN.
716600     IF (DMS-ABORTED)
716700         GO TO 440-END.
716800
716900     MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY.
717000     MOVE "L"                    TO DB-ACTION-TYPE.
717100     MOVE PA52F5-PCT-ACTION-CODE TO DB-ACTION-CODE.
717200     MOVE PA52F5-PCT-EFFECT-DATE TO DB-EFFECT-DATE.
717300     MOVE PA52F5-PCT-EMPLOYEE    TO DB-EMPLOYEE.
717400     MOVE PA52F5-PCT-ACTION-NBR  TO DB-ACTION-NBR.
717500
717600     PERFORM 840-MODIFY-PCTSET1.

717700     PERFORM 830-DELETE-PERSACTION.
717800
717900     MOVE ZEROES                 TO DB-EMP-APP.
718000     MOVE "PA"                   TO DB-CMT-TYPE.
718100     MOVE PA52F5-PCT-ACTION-CODE TO DB-ACTION-CODE.
718200     MOVE PA52F5-PCT-EFFECT-DATE TO DB-DATE.
718300     MOVE PA52F5-PCT-EMPLOYEE    TO DB-EMPLOYEE.
718400     MOVE SPACES                 TO DB-JOB-CODE.
718500     MOVE PA52F5-PCT-ACTION-NBR  TO DB-LN-NBR.
718600     MOVE PACSET2-LN-NBR         TO WS-DB-BEG-RNG.
718700     PERFORM 830-DELETERNG-PACSET2.
718800
102321     SET PA52WS-NOMORE-CODES     TO TRUE.
P63835     IF (PAT-WORKFLOW-FLAG = "Y")
P63835         INITIALIZE WFAPI-INPUT
P63835                    WFAPI-OUTPUT
P63835
P63835         MOVE "PCTSET1"              TO WFAPI-I-OBJECT-NAME
P63835         MOVE PA52F5-PCT-COMPANY     TO WFAPI-I-KEY-VALUE (1)
P63835         MOVE "L"                    TO WFAPI-I-KEY-VALUE (2)
P63835         MOVE PA52F5-PCT-EFFECT-DATE TO WFAPI-I-KEY-VALUE (3)
P63835         MOVE PA52F5-PCT-ACTION-CODE TO WFAPI-I-KEY-VALUE (4)
P63835         MOVE PA52F5-PCT-EMPLOYEE    TO WFAPI-I-KEY-VALUE (5)
P63835         MOVE PA52F5-PCT-ACTION-NBR  TO WFAPI-I-KEY-VALUE (6)
P63835         PERFORM 1000-WF-CANCEL-SETUP
P63835     END-IF.
719000     MOVE PCT-COMPANY            TO DB-COMPANY.
719200     MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE.
           PERFORM 4000-PAPCT-EMP-PEND-ACT-DATE.
720200
720300     MOVE CRT-RECS-DELETED       TO CRT-MESSAGE.
720400     PERFORM 920-AUDIT-END.
720500
720600 440-END.
720700
      ******************************************************************
       450-REVERSE.
      ******************************************************************

           MOVE PA52F5-PCT-MERGE-ACTN-NBR    TO PA52F5-SVD-ACTN-NBR.
           PERFORM 460-FILL-REV-FLDS
           THRU    460-END
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36).

           IF  (PAPCT-HISTERR-EXISTS)
               MOVE 541                TO CRT-MSG-NBR
           ELSE
               MOVE 534                TO CRT-MSG-NBR.

       450-END.

      ******************************************************************
       460-FILL-REV-FLDS.
      ******************************************************************

           IF (PAPCT-REV-NOTFOUND (I1))
               GO TO 460-END.

           MOVE PAPCT-REV-OLD-VALUE (I1)   TO HRWS-VALUE.

           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (1))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-END-DATE
                                          PA52F5-REV-VALUE (1)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (2))
P51466         MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-DATE-ASSIGN
                                          PA52F5-REV-VALUE (2)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (3))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-POSITION
                                          PA52F5-REV-VALUE (3)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (4))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-JOBCODE
                                          PA52F5-REV-VALUE (4)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (5))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-PL
                                          PA52F5-REV-VALUE (5)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (6))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-DEPARTMENT
                                          PA52F5-REV-VALUE (6)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (7))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-USER-LEVEL
                                          PA52F5-REV-VALUE (7)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (8))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-SUPERVISOR
                                          PA52F5-REV-VALUE (8)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (9))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-IND-SUPER
                                          PA52F5-REV-VALUE (9)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (10))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-LOCATION
                                          PA52F5-REV-VALUE (10)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (11))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-FTE
                                          PA52F5-REV-VALUE (11)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (12))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-ANNUAL-HOURS
                                          PA52F5-REV-VALUE (12)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (13))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-SALARY-CLASS
                                          PA52F5-REV-VALUE (13)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (14))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-PAY-FREQ
                                          PA52F5-REV-VALUE (14)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (15))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-PAY-RATE
                                          PA52F5-REV-VALUE (15)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (16))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-CURRENCY
                                          PA52F5-REV-VALUE (16)
           ELSE
           IF  (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (17))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-EXEMPT
                                          PA52F5-REV-VALUE (17)
           ELSE
           IF  (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (18))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-PAY-PLAN
                                          PA52F5-REV-VALUE (18)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (19))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-UNION
                                          PA52F5-REV-VALUE (19)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (20))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-BARGAIN-UNIT
                                          PA52F5-REV-VALUE (20)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (21))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-SCHEDULE
                                          PA52F5-REV-VALUE (21)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (22))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-GRADE
                                          PA52F5-REV-VALUE (22)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (23))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-STEP
                                          PA52F5-REV-VALUE (23)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (24))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-SHIFT
                                          PA52F5-REV-VALUE (24)
P60002     ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (25))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-WORK-SCHD
                                          PA52F5-REV-VALUE (25)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (26))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-SECURITY-LVL
                                          PA52F5-REV-VALUE (26)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (27))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-SECURITY-LOC
                                          PA52F5-REV-VALUE (27)
          ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (28))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-EXP-COMPANY
                                          PA52F5-REV-VALUE (28)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (29))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-ACCT-UNIT
                                          PA52F5-REV-VALUE (29)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (30))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-ACCOUNT
                                          PA52F5-REV-VALUE (30)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (31))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-SUBACCOUNT
                                          PA52F5-REV-VALUE (31)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (32))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-ACTIVITY
                                          PA52F5-REV-VALUE (32)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (33))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-CATEGORY
                                          PA52F5-REV-VALUE (33)
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (34))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-BASE-RATE-A
                                          PA52F5-REV-VALUE (34)
                                          PA52F5-PCT-ENTERED-BASE
           ELSE
           IF (PAPCT-REV-FLD-NBR (I1) = PA52WS-PAT-FLD-NBR-5 (36))
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-USER-AMOUNT
                                          PA52F5-REV-VALUE (36)
           END-IF.

       460-END.

720800******************************************************************
720900 480-INQUIRE.
721000******************************************************************
721100
           INITIALIZE PA52F5-PCT-ANT-END-DATE
                      PA52F5-PCT-EDM-END-DATE
                      PA52F5-PCT-EDM-EFFECT-DT
                      PA52F5-PCT-OCCUR-TYPE
                      PA52F5-OCC-DESC
                      PA52F5-PCT-PARTICIPNT
                      PA52F5-PCT-MOVE-FROM-LEVEL
                      PA52F5-PCT-REASON (1)
                      PA52F5-PCT-REASON (2).
           INITIALIZE                    PA52F5-PCT-PROCESS-TYPE
                                         PA52F5-PCT-NEW-EFFECT-DATE
                                         PA52F5-PCT-HIST-CORR-FLAG
                                         PA52F5-MERGE-ACT-OBJ-ID
                                         PA52F5-REV-VALUE-GRP
                                         PA52F5-PCT-MERGE-ACTN-NBR.

           INITIALIZE                    PA52F5-REV-VALUE-GRP.

721600     INITIALIZE                    PA52F5-PCT-NEW-END-DATE
P51466                                   PA52F5-PCT-NEW-DATE-ASSIGN
721700                                   PA52F5-PCT-NEW-POSITION
721800                                   PA52F5-PCT-NEW-JOBCODE
721900                                   PA52F5-PCT-NEW-PL
722000                                   PA52F5-PCT-NEW-DEPARTMENT
722100                                   PA52F5-PCT-NEW-USER-LEVEL
722200                                   PA52F5-PCT-NEW-SUPERVISOR
722300                                   PA52F5-PCT-NEW-IND-SUPER
722400                                   PA52F5-PCT-NEW-LOCATION
722500                                   PA52F5-PCT-NEW-FTE
722600                                   PA52F5-PCT-NEW-ANNUAL-HOURS
722700                                   PA52F5-PCT-NEW-SALARY-CLASS
722800                                   PA52F5-PCT-NEW-PAY-FREQ
722900                                   PA52F5-PCT-NEW-PAY-RATE
723000                                   PA52F5-PCT-NEW-EXEMPT
723100                                   PA52F5-PCT-NEW-PAY-PLAN
723200                                   PA52F5-PCT-NEW-UNION
                                         PA52F5-PCT-NEW-BARGAIN-UNIT
723300                                   PA52F5-PCT-NEW-SCHEDULE
723400                                   PA52F5-PCT-NEW-GRADE
723500                                   PA52F5-PCT-NEW-STEP
723700                                   PA52F5-PCT-NEW-SHIFT
723600                                   PA52F5-PCT-NEW-WORK-SCHD
723800                                   PA52F5-PCT-NEW-SECURITY-LVL
723900                                   PA52F5-PCT-NEW-SECURITY-LOC
724000                                   PA52F5-PCT-NEW-EXP-COMPANY
724100                                   PA52F5-PCT-NEW-ACCT-UNIT
724200                                   PA52F5-PCT-NEW-ACCOUNT
724300                                   PA52F5-PCT-NEW-SUBACCOUNT
724400                                   PA52F5-PCT-NEW-ACTIVITY
724500                                   PA52F5-PCT-NEW-CATEGORY
                                         PA52F5-PCT-NEW-CURRENCY
                                         PA52F5-PCT-ENTERED-BASE
                                         PA52F5-PCT-NEW-BASE-RATE
                                         PA52F5-PCT-NEW-BASE-RATE-A
                                         PA52F5-PCT-NEW-BASE-CURR
                                         PA52F5-PCT-NEW-USER-AMOUNT.
724800
724900     INITIALIZE                    PA52F5-PCT-END-DATE
P51466                                   PA52F5-PCT-DATE-ASSIGN
725100                                   PA52F5-PEP-EFFECT-DATE
725200                                   PA52F5-PEP-END-DATE
                                         PA52F5-POS-EFF-DT
725300                                   PA52F5-PCT-POSITION
725400                                   PA52F5-PCT-JOBCODE
725500                                   PA52F5-PCT-PL
725600                                   PA52F5-PCT-DEPARTMENT
725700                                   PA52F5-PCT-USER-LEVEL
725800                                   PA52F5-PCT-SUPERVISOR
725900                                   PA52F5-PCT-IND-SUPER
726000                                   PA52F5-PCT-LOCATION
726100                                   PA52F5-PCT-FTE
726200                                   PA52F5-PCT-ANNUAL-HOURS
726300                                   PA52F5-PCT-SALARY-CLASS
726400                                   PA52F5-PCT-PAY-FREQ
726500                                   PA52F5-PCT-PAY-RATE
726600                                   PA52F5-PCT-EXEMPT
726700                                   PA52F5-PCT-PAY-PLAN
726800                                   PA52F5-PCT-UNION
                                         PA52F5-PCT-BARGAIN-UNIT
726900                                   PA52F5-PCT-SCHEDULE
727000                                   PA52F5-PCT-GRADE
727100                                   PA52F5-PCT-STEP
727300                                   PA52F5-PCT-SHIFT
727200                                   PA52F5-PCT-WORK-SCHD
727400                                   PA52F5-PCT-SECURITY-LVL
727500                                   PA52F5-PCT-SECURITY-LOC
727600                                   PA52F5-PCT-EXP-COMPANY
727700                                   PA52F5-PCT-ACCT-UNIT
727800                                   PA52F5-PCT-ACCOUNT
727900                                   PA52F5-PCT-SUBACCOUNT
728000                                   PA52F5-PCT-ACTIVITY
728100                                   PA52F5-PCT-CATEGORY
                                         PA52F5-PCT-CURRENCY-CODE
                                         PA52F5-PCT-BASE-PAY-RATE
                                         PA52F5-PCT-BASE-CURRENCY
                                         PA52F5-PCT-USER-AMOUNT.
728400
728500     INITIALIZE                     PA52F5-POSITION-DESC
728600                                    PA52F5-JOBCODE-DESC
728700                                    PA52F5-PL-DESC
728800                                    PA52F5-DEPARTMENT-DESC
728900                                    PA52F5-USER-LEVEL-DESC
729000                                    PA52F5-SUPERVISOR-DESC
729100                                    PA52F5-IND-SUPER-DESC
729200                                    PA52F5-LOCATION-DESC
729300                                    PA52F5-PAY-PLAN-DESC
                                          PA52F5-OT-EFFECT-DATE
729400                                    PA52F5-UNION-DESC
729500                                    PA52F5-SCHEDULE-DESC
729600                                    PA52F5-WORK-SCHD-DESC
729700                                    PA52F5-ACCOUNT-DESC
729800                                    PA52F5-ACTIVITY-DESC
                                          PA52F5-CURRENCY-FORMS-EXP
                                          PA52F5-BARGAIN-UNIT-DESC.
730000
730100     INITIALIZE                     PA52F5-POSITION-STAR
P51466                                    PA52F5-DATE-ASSIGN-STAR
730200                                    PA52F5-JOBCODE-STAR
730300                                    PA52F5-PL-STAR
730400                                    PA52F5-DEPARTMENT-STAR
730500                                    PA52F5-USER-LEVEL-STAR
730600                                    PA52F5-SUPERVISOR-STAR
730700                                    PA52F5-IND-SUPER-STAR
730800                                    PA52F5-LOCATION-STAR
730900                                    PA52F5-FTE-STAR
731000                                    PA52F5-ANNUAL-HOURS-STAR
731100                                    PA52F5-SALARY-CLASS-STAR
731200                                    PA52F5-PAY-FREQ-STAR
731300                                    PA52F5-PAY-RATE-STAR
731400                                    PA52F5-EXEMPT-STAR
731500                                    PA52F5-PAY-PLAN-STAR
731600                                    PA52F5-UNION-STAR
                                          PA52F5-BARGAIN-UNIT-STAR
731700                                    PA52F5-SCHEDULE-STAR
731800                                    PA52F5-GRADE-STAR
731900                                    PA52F5-STEP-STAR
732000                                    PA52F5-WORK-SCHD-STAR
732100                                    PA52F5-SHIFT-STAR
732200                                    PA52F5-SECURITY-LVL-STAR
732300                                    PA52F5-SECURITY-LOC-STAR
732400                                    PA52F5-COMPANY-STAR
732500                                    PA52F5-ACCT-UNIT-STAR
732600                                    PA52F5-ACCOUNT-STAR
732700                                    PA52F5-SUBACCOUNT-STAR
732800                                    PA52F5-ACTIVITY-STAR
732900                                    PA52F5-CATEGORY-STAR
                                          PA52F5-CURRENCY-STAR
                                          PA52F5-BASE-RATE-STAR
                                          PA52F5-USER-AMOUNT-STAR.
           INITIALIZE PA52F5-MORE-MSG
                      PA52F5-PENDING-MSG
                      PA52F5-ON-HOLD-MSG.
733200
735000     MOVE WS-FALSE               TO PAPEP-HISTORICAL-SW.
735100     MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY.
735200     MOVE PA52F5-PCT-EMPLOYEE    TO DB-EMPLOYEE.
735300     MOVE PA52F5-POS-LEVEL       TO DB-POS-LEVEL.
P58592     MOVE PEPSET3-POS-LEVEL      TO WS-DB-BEG-RNG.
P58592     PERFORM 850-FIND-BEGRNG-PEPSET3.
735600     IF  (PAEMPPOS-FOUND)
736000         MOVE PEP-EFFECT-DATE            TO PA52F5-PEP-EFFECT-DATE
736100         MOVE PEP-END-DATE               TO PA52F5-PEP-END-DATE
               MOVE PEP-POS-EFF-DT             TO PA52F5-POS-EFF-DT
736200         IF (PEP-END-DATE = ZEROES)
736300             INITIALIZE                     PA52F5-PCT-END-DATE
736400         ELSE
736500             MOVE PEP-END-DATE           TO HRWS-DATE-8-FIELD
736600             PERFORM 781-HR-FORMAT-DATE-FIELD
736700             MOVE HRWS-VALUE             TO PA52F5-PCT-END-DATE
736800         END-IF
736900         PERFORM 484-MOVE-CURRENT-DATA
737000         THRU    484-END
737100             VARYING I1 FROM 2 BY 1
737200             UNTIL  (I1 > 37).
J40356
J40356         PERFORM 300-LOAD-LTM-VALUES
J40356         THRU    300-END
J40356             VARYING I1 FROM 1 BY 1
J40356             UNTIL  (I1 > 36).
J40356
737400     PERFORM 490-MOVE-STARS
737500     THRU    490-END.
737600
740600     MOVE EMP-LAST-NAME          TO HRWS-LAST-NAME.
740700     MOVE EMP-FIRST-NAME         TO HRWS-FIRST-NAME.
740800     MOVE EMP-MIDDLE-INIT        TO HRWS-MIDDLE-INIT.
           MOVE EMP-NAME-SUFFIX        TO HRWS-NAME-SUFFIX.
           MOVE EMP-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE.
740900     PERFORM 750-HR-FORMAT-NAME.
741000     MOVE HRWS-FORMAT-NAME       TO PA52F5-EMP-NAME.

           MOVE PRS-COMPANY               TO DB-COMPANY.
           MOVE SPACES                    TO DB-COUNTRY-CD-REQ
                                             DB-PROCESS-LEVEL.
      * 50 = Social Number
           MOVE HREMP-FICA-NBR-DN         TO DB-FLD-NBR.
           PERFORM 840-FIND-PASSET1.
           IF (PASCRTY-FOUND)
               MOVE PAS-SEC-LEVEL         TO HRWS-SEC-LEVEL
               PERFORM 730-HR-FIELD-SECURITY
               IF (HRWS-FLD-SECURED)
                   MOVE SPACES            TO PA52F5-EMP-FICA-NBR
               ELSE
                   MOVE EMP-FICA-NBR      TO PA52F5-EMP-FICA-NBR.

           SET PA52WS-NO-MOVELEVELDONE TO TRUE.
102321     IF (PA52F5-PCT-EMPLOYEE      NOT = PA52WS-HOLD-EMPLOYEE)
102321     OR (PA52F5-PCT-ACTION-CODE   NOT = PA52WS-HOLD-ACTION-CODE)
102321         SET PA52WS-NOMORE-CODES     TO TRUE
102321     END-IF.
           MOVE PA52F5-PCT-COMPANY         TO DB-COMPANY.
           MOVE "L"                        TO DB-ACTION-TYPE.
           MOVE PA52F5-PCT-EMPLOYEE        TO DB-EMPLOYEE.
           MOVE PCTSET2-EMPLOYEE           TO WS-DB-BEG-RNG. 
           PERFORM 850-FIND-BEGRNG-PCTSET2.
           IF (PERSACTION-FOUND)
               IF  (PCT-ACTION-CODE       = PA52F5-PCT-ACTION-CODE)
               AND (PA52F5-PCT-ACTION-NBR = ZEROES)
102321         AND (PA52WS-NOMORE-CODES)
                   PERFORM 860-FIND-NXTRNG-PCTSET2
               ELSE
                   IF  (PCT-ACTION-CODE = PA52F5-PCT-ACTION-CODE)
                   AND (PCT-ACTION-NBR  = PA52F5-PCT-ACTION-NBR)
                   AND (PCT-EFFECT-DATE = PA52F5-PCT-EFFECT-DATE)
102321             AND (PA52WS-NOMORE-CODES)
                       PERFORM 860-FIND-NXTRNG-PCTSET2
                   END-IF
               END-IF
               IF  (PERSACTION-FOUND)
                   MOVE 116                TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
                   MOVE CRT-MESSAGE        TO PA52F5-PENDING-MSG
102321             INITIALIZE                 PA52WS-HOLD-EMPLOYEE
102321                                        PA52WS-HOLD-ACTION-CODE
102321             MOVE PCT-EMPLOYEE       TO PA52WS-HOLD-EMPLOYEE
102321             MOVE PCT-ACTION-CODE    TO PA52WS-HOLD-ACTION-CODE
102321             SET PA52WS-OTHER-CODES  TO TRUE
J60711*            IF (PCT-HOLD-FLAG = "Y")
J60711*                MOVE 175                TO CRT-MSG-NBR
J60711*                PERFORM 790-GET-MSG
J60711*                MOVE CRT-MESSAGE        TO PA52F5-ON-HOLD-MSG
J60711*            END-IF
               END-IF
           END-IF.

           IF (PA52F5-PENDING-MSG = SPACES)
               MOVE "E"                        TO DB-ACTION-TYPE
               MOVE PCTSET2-EMPLOYEE           TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PCTSET2
               IF  (PERSACTION-FOUND)
                   MOVE 116                TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
                   MOVE CRT-MESSAGE        TO PA52F5-PENDING-MSG
J60711*            IF (PCT-HOLD-FLAG = "Y")
J60711*                MOVE 175                TO CRT-MSG-NBR
J60711*                PERFORM 790-GET-MSG
J60711*                MOVE CRT-MESSAGE        TO PA52F5-ON-HOLD-MSG
J60711*            END-IF
               END-IF
           END-IF.

010900     MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY.
011000     MOVE "L"                    TO DB-ACTION-TYPE.
011100     MOVE PA52F5-PCT-EMPLOYEE    TO DB-EMPLOYEE.
011200     MOVE PA52F5-PCT-ACTION-CODE TO DB-ACTION-CODE.
011300     MOVE PA52F5-PCT-EFFECT-DATE TO DB-EFFECT-DATE.
           MOVE PA52F5-PCT-ACTION-NBR  TO DB-ACTION-NBR.
           PERFORM 840-FIND-PCTSET2.

           IF  (PA52F5-FC = "I")
           AND ((PERSACTION-NOTFOUND)
            OR  (PCT-POS-LEVEL   NOT = PA52F5-POS-LEVEL))
               MOVE PCTSET2-EFFECT-DATE TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PCTSET2
               PERFORM 860-FIND-NXTRNG-PCTSET2
                   UNTIL (PERSACTION-NOTFOUND)
                   OR    (PCT-POS-LEVEL = PA52F5-POS-LEVEL)
           END-IF.

P49898     MOVE PA52F5-PCT-COMPANY         TO PA52F5-ORIG-COMPANY.
P49898     MOVE PA52F5-PCT-EMPLOYEE        TO PA52F5-ORIG-EMPLOYEE.
P49898     MOVE PA52F5-PCT-ACTION-CODE     TO PA52F5-ORIG-ACTION-CODE.
P49898     MOVE PA52F5-POS-LEVEL           TO PA52F5-ORIG-POS-LEVEL.
P49898     MOVE PA52F5-PCT-EFFECT-DATE     TO PA52F5-ORIG-EFFECT-DATE.

737700     IF (PERSACTION-NOTFOUND)
737800     OR (PCT-COMPANY     NOT = PA52F5-PCT-COMPANY)
737900     OR (PCT-ACTION-TYPE NOT = "L")
738000     OR (PCT-EMPLOYEE    NOT = PA52F5-PCT-EMPLOYEE)
738100     OR (PCT-ACTION-CODE NOT = PA52F5-PCT-ACTION-CODE)
           OR (PCT-EFFECT-DATE NOT = PA52F5-PCT-EFFECT-DATE)
738200     OR (PCT-POS-LEVEL   NOT = PA52F5-POS-LEVEL)
               INITIALIZE                     PA52F5-PCT-ACTION-NBR
P81486                                        PA52F5-PCT-APPROVAL-FLAG
               IF  (PA52F5-PCT-REASON (1) = SPACES)
               AND (PA52F5-PCT-REASON (2) = SPACES)
                   IF (PAT-DFT-REASON (1) NOT = SPACES)
                       MOVE PAT-DFT-REASON (1) TO PA52F5-PCT-REASON (1)
                   END-IF
                   IF (PAT-DFT-REASON (2) NOT = SPACES)
                       MOVE PAT-DFT-REASON (2) TO PA52F5-PCT-REASON (2)
                   END-IF
               END-IF
               IF (PAT-DFT-UPD-BN = SPACES)
                   MOVE PA52F5-PCT-COMPANY      TO DB-COMPANY
                   PERFORM 840-KFIND-BNCSET1
                   IF (BNCOMPANY-KFOUND)
                      MOVE "Y" TO PA52F5-PCT-UPDATE-BENEFIT
                   ELSE
                      MOVE "N" TO PA52F5-PCT-UPDATE-BENEFIT
                   END-IF
               ELSE
                   MOVE PAT-DFT-UPD-BN TO PA52F5-PCT-UPDATE-BENEFIT
               END-IF
               IF (PAT-DFT-UPD-LP = SPACES)
J13588*            MOVE PA52F5-PCT-COMPANY     TO EDCDWS-COMPANY
J13588*            MOVE "LP"                   TO EDCDWS-SYSTEM
J13588*            PERFORM 6000-IS-SYSTEM-TRIGGER-ENABLED
J13588*            IF  (EDCDWS-TRIGGER-ENABLED)
                       MOVE "Y" TO PA52F5-PCT-UPD-ABS-MGMT
J13588*            ELSE
J13588*                MOVE "N"        TO PA52F5-PCT-UPD-ABS-MGMT
J13588*            END-IF
               ELSE
                   MOVE PAT-DFT-UPD-LP TO PA52F5-PCT-UPD-ABS-MGMT
               END-IF
               IF (PA52F5-POS-LEVEL = 1)
                   MOVE PAT-DFT-UPD-RQ-DED TO PA52F5-PCT-UPDATE-REQ-DED
               ELSE
                   MOVE "N" TO PA52F5-PCT-UPDATE-REQ-DED
               END-IF
               MOVE PAT-HIST-CORR-FLAG  TO PA52F5-PCT-HIST-CORR-FLAG
738300         MOVE 407                    TO CRT-MSG-NBR
J40356         IF (PA52WS-LTM-SELECTED)
J40356             MOVE 545                TO CRT-MSG-NBR
J40356             PERFORM 790-GET-MSG
J40356             SET PA52WS-LTM-NOT-SELECTED TO TRUE
J40356         ELSE
111900             MOVE 407                TO CRT-MSG-NBR
J40356             SET PA52WS-LTM-SELECT-OPEN  TO TRUE
J40356         END-IF

112000         GO TO 480-END.
        
       480-CONTINUE.

738600     PERFORM 481-MOVE-TO-SCREEN
738700     THRU    481-END.
738800
J40356*    MOVE CRT-INQ-COMPLETE           TO CRT-MESSAGE.
J40356
J40356     IF (PA52WS-LTM-SELECTED)
J40356         MOVE 545                    TO CRT-MSG-NBR
J40356         PERFORM 790-GET-MSG
J40356         SET PA52WS-LTM-NOT-SELECTED TO TRUE
J40356     ELSE
J40356         SET PA52WS-LTM-SELECT-OPEN  TO TRUE
J40356         MOVE CRT-INQ-COMPLETE       TO CRT-MESSAGE
J40356     END-IF.
739000
739100 480-END.
739200
739300******************************************************************
739400 481-MOVE-TO-SCREEN.
739500******************************************************************
739600
           INITIALIZE                     PA52WS-FIRST-ACT-NBR
                                          PA52WS-LAST-ACT-NBR.
113200     MOVE PCT-COMPANY            TO PA52F5-PCT-COMPANY.
113300     MOVE PCT-EMPLOYEE           TO PA52F5-PCT-EMPLOYEE.
113400     MOVE PCT-ACTION-CODE        TO PA52F5-PCT-ACTION-CODE.
113500     MOVE PCT-ACTION-NBR         TO PA52F5-PCT-ACTION-NBR.
113700     MOVE PCT-EFFECT-DATE        TO PA52F5-PCT-EFFECT-DATE.
           MOVE PCT-COMPANY            TO DB-COMPANY.
           MOVE "L"                    TO DB-ACTION-TYPE.
           MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE.
           MOVE PCT-EFFECT-DATE        TO DB-EFFECT-DATE.
           MOVE WS-HIGH-VALUES         TO DB-ACTION-NBR.
           MOVE ZEROES                 TO PA52WS-LAST-ACT-NBR
                                          PA52WS-FIRST-ACT-NBR.
           PERFORM 850-FIND-NLT-PCTSET2.
           IF  (PCT-COMPANY     = DB-COMPANY)
           AND (PCT-ACTION-TYPE = DB-ACTION-TYPE)
           AND (PCT-EMPLOYEE    = DB-EMPLOYEE)
           AND (PCT-ACTION-CODE = DB-ACTION-CODE)
           AND (PCT-EFFECT-DATE = DB-EFFECT-DATE)
               MOVE PCT-ACTION-NBR     TO PA52WS-LAST-ACT-NBR
           END-IF.
           PERFORM
               UNTIL (PERSACTION-NOTFOUND)
               OR    (PCT-COMPANY     NOT = DB-COMPANY)
               OR    (PCT-ACTION-TYPE NOT = DB-ACTION-TYPE)
               OR    (PCT-EMPLOYEE    NOT = DB-EMPLOYEE)
               OR    (PCT-ACTION-CODE NOT = DB-ACTION-CODE)
               OR    (PCT-EFFECT-DATE NOT = DB-EFFECT-DATE)
                   MOVE PCT-ACTION-NBR TO PA52WS-FIRST-ACT-NBR
                   PERFORM 860-FIND-NEXT-PCTSET2
           END-PERFORM.

           MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY.
           MOVE "L"                    TO DB-ACTION-TYPE.
           MOVE PA52F5-PCT-EMPLOYEE    TO DB-EMPLOYEE.
           MOVE PA52F5-PCT-ACTION-CODE TO DB-ACTION-CODE.
           MOVE PA52F5-PCT-EFFECT-DATE TO DB-EFFECT-DATE.
           MOVE PA52F5-PCT-ACTION-NBR  TO DB-ACTION-NBR.
           PERFORM 840-FIND-PCTSET2.
           PERFORM 8400-AFTER-FIND-PCT.

739800     MOVE PA52WS-PRS-NAME        TO PA52F5-PRS-NAME.
739900     MOVE PAT-DESCRIPTION        TO PA52F5-PAT-DESCRIPTION.
740100     MOVE PCT-ACTION-TYPE        TO PA52F5-PT-ACTION-TYPE.
740200     MOVE PCT-EMPLOYEE           TO PA52F5-PCT-EMPLOYEE.
740500     MOVE PCT-PARTICIPNT         TO PA52F5-PCT-PARTICIPNT.
741300     MOVE PCT-ANT-END-DATE       TO PA52F5-PCT-ANT-END-DATE.
           IF (PA52WS-FIRST-ACT-NBR NOT = PA52WS-LAST-ACT-NBR)
               MOVE 170                TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE        TO PA52F5-MORE-MSG.

741600     MOVE PCT-ACTION-NBR         TO PA52F5-PCT-ACTION-NBR.
741700     MOVE "N"                    TO PA52F5-IMMEDIATE-ACTION.
741800     MOVE PCT-POS-LEVEL          TO PA52F5-POS-LEVEL.
741900     MOVE PCT-OCCUR-TYPE         TO PA52F5-PCT-OCCUR-TYPE.
742000     MOVE PCT-UPDATE-BENEFIT     TO PA52F5-PCT-UPDATE-BENEFIT.
           MOVE PCT-UPD-ABS-MGMT       TO PA52F5-PCT-UPD-ABS-MGMT.
114300     MOVE PCT-UPDATE-REQ-DED     TO PA52F5-PCT-UPDATE-REQ-DED.
114300     MOVE PCT-EDM-EFFECT-DT      TO PA52F5-PCT-EDM-EFFECT-DT.
           MOVE PCT-APPROVAL-FLAG      TO PA52F5-PCT-APPROVAL-FLAG.
114300     MOVE PCT-EDM-END-DATE       TO PA52F5-PCT-EDM-END-DATE.
           MOVE PCT-PROCESS-TYPE       TO PA52F5-PCT-PROCESS-TYPE.
           MOVE PCT-HIST-CORR-FLAG     TO PA52F5-PCT-HIST-CORR-FLAG.
           MOVE PCT-MERGE-ACTN-NBR     TO PA52F5-PCT-MERGE-ACTN-NBR.
           MOVE PCT-POS-LEVEL-MOVE     TO PA52F5-PCT-MOVE-FROM-LEVEL.
742100
J40356     IF (PA52WS-LTM-NOT-SELECTED)
742200         MOVE PCT-NEW-VALUE (1)   TO PA52F5-PCT-NEW-END-DATE 
P51466         MOVE PCT-NEW-VALUE (2)   TO PA52F5-PCT-NEW-DATE-ASSIGN 
742300         MOVE PCT-NEW-VALUE (3)   TO PA52F5-PCT-NEW-POSITION 
742400         MOVE PCT-NEW-VALUE (4)   TO PA52F5-PCT-NEW-JOBCODE 
742500         MOVE PCT-NEW-VALUE (5)   TO PA52F5-PCT-NEW-PL 
742600         MOVE PCT-NEW-VALUE (6)   TO PA52F5-PCT-NEW-DEPARTMENT 
742700         MOVE PCT-NEW-VALUE (7)   TO PA52F5-PCT-NEW-USER-LEVEL 
742800         MOVE PCT-NEW-VALUE (8)   TO PA52F5-PCT-NEW-SUPERVISOR 
742900         MOVE PCT-NEW-VALUE (9)   TO PA52F5-PCT-NEW-IND-SUPER 
743000         MOVE PCT-NEW-VALUE (10)  TO PA52F5-PCT-NEW-LOCATION 
743100         MOVE PCT-NEW-VALUE (11)  TO PA52F5-PCT-NEW-FTE 
743200         MOVE PCT-NEW-VALUE (12)  TO PA52F5-PCT-NEW-ANNUAL-HOURS 
743300         MOVE PCT-NEW-VALUE (13)  TO PA52F5-PCT-NEW-SALARY-CLASS 
743400         MOVE PCT-NEW-VALUE (14)  TO PA52F5-PCT-NEW-PAY-FREQ 
743500         MOVE PCT-NEW-VALUE (15)  TO PA52F5-PCT-NEW-PAY-RATE 
               MOVE PCT-NEW-VALUE (16)  TO PA52F5-PCT-NEW-CURRENCY 
743600         MOVE PCT-NEW-VALUE (17)  TO PA52F5-PCT-NEW-EXEMPT 
743700         MOVE PCT-NEW-VALUE (18)  TO PA52F5-PCT-NEW-PAY-PLAN 
743800         MOVE PCT-NEW-VALUE (19)  TO PA52F5-PCT-NEW-UNION 
               MOVE PCT-NEW-VALUE (20)  TO PA52F5-PCT-NEW-BARGAIN-UNIT 
743900         MOVE PCT-NEW-VALUE (21)  TO PA52F5-PCT-NEW-SCHEDULE 
744000         MOVE PCT-NEW-VALUE (22)  TO PA52F5-PCT-NEW-GRADE 
744100         MOVE PCT-NEW-VALUE (23)  TO PA52F5-PCT-NEW-STEP 
744300         MOVE PCT-NEW-VALUE (24)  TO PA52F5-PCT-NEW-SHIFT 
744200         MOVE PCT-NEW-VALUE (25)  TO PA52F5-PCT-NEW-WORK-SCHD 
744400         MOVE PCT-NEW-VALUE (26)  TO PA52F5-PCT-NEW-SECURITY-LVL 
744500         MOVE PCT-NEW-VALUE (27)  TO PA52F5-PCT-NEW-SECURITY-LOC 
744600         MOVE PCT-NEW-VALUE (28)  TO PA52F5-PCT-NEW-EXP-COMPANY 
744700         MOVE PCT-NEW-VALUE (29)  TO PA52F5-PCT-NEW-ACCT-UNIT 
744800         MOVE PCT-NEW-VALUE (30)  TO PA52F5-PCT-NEW-ACCOUNT 
744900         MOVE PCT-NEW-VALUE (31)  TO PA52F5-PCT-NEW-SUBACCOUNT 
745000         MOVE PCT-NEW-VALUE (32)  TO PA52F5-PCT-NEW-ACTIVITY 
745100         MOVE PCT-NEW-VALUE (33)  TO PA52F5-PCT-NEW-CATEGORY 
               MOVE PCT-NEW-VALUE (34)  TO PA52F5-PCT-ENTERED-BASE 
               MOVE PCT-NEW-VALUE (36)  TO PA52F5-PCT-NEW-USER-AMOUNT 
J40356     END-IF.
           MOVE PCT-UPDATE-BENEFIT     TO PA52F5-PCT-UPDATE-BENEFIT.
           MOVE PCT-UPD-ABS-MGMT       TO PA52F5-PCT-UPD-ABS-MGMT.
           MOVE PCT-UPDATE-REQ-DED     TO PA52F5-PCT-UPDATE-REQ-DED.
           MOVE PCT-REASON (1)         TO PA52F5-PCT-REASON (1).
           MOVE PCT-REASON (2)         TO PA52F5-PCT-REASON (2).
J60711
J60711     IF (PCT-HOLD-FLAG = "Y")
J60711         MOVE 175                TO CRT-MSG-NBR
J60711         PERFORM 790-GET-MSG
J60711         MOVE CRT-MESSAGE        TO PA52F5-ON-HOLD-MSG
J60711     ELSE
J60711         INITIALIZE                 PA52F5-ON-HOLD-MSG
J60711     END-IF.

           IF (PA52F5-PCT-NEW-POSITION NOT = SPACES)
               IF (PAPOS-POSITION NOT = PA52F5-PCT-NEW-POSITION)
                   MOVE PA52F5-PCT-COMPANY
                                       TO DB-COMPANY
                   MOVE PA52F5-PCT-NEW-POSITION
                                       TO DB-POSITION
                   MOVE PA52F5-PCT-EFFECT-DATE
                                       TO DB-EFFECT-DATE
                   PERFORM 850-FIND-NLT-POSSET2
                   IF  (PAPOSITION-FOUND)
                   AND (POS-COMPANY  = DB-COMPANY)
                   AND (POS-POSITION = DB-POSITION)
                       MOVE POS-DESCRIPTION
                                       TO PA52F5-POSITION-DESC
                   END-IF
               ELSE
                   MOVE PAPOS-DESCRIPTION
                                       TO PA52F5-POSITION-DESC.

           MOVE PCT-BASE-CURRENCY      TO PA52F5-PCT-NEW-BASE-CURR.
           MOVE PCT-BASE-ND            TO PA52F5-PCT-NEW-BASE-ND.
           MOVE PCT-BASE-PAY-RATE      TO PA52F5-PCT-NEW-BASE-RATE
                                          HRWS-DEC-FIELD.
           IF (PCT-BASE-PAY-RATE = ZEROES)
               MOVE SPACES             TO PA52F5-PCT-NEW-BASE-RATE-A
           ELSE
               MOVE 0                  TO HRWS-SIZE
               MOVE 4                  TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-BASE-RATE-A
           END-IF.
745400
745500     MOVE PA52F5-PCT-COMPANY      TO DB-COMPANY.
745600
746500     PERFORM 825-GET-DESCRIPTIONS
746600     THRU    825-END.
746700
090800     MOVE "PAMSG"                TO CRT-ERROR-CAT.
090900     MOVE 100                    TO CRT-MSG-NBR.
091000     PERFORM 790-GET-MSG.
091100     MOVE CRT-MESSAGE            TO PA52F5-COMMENTS.
           INITIALIZE CRT-MESSAGE.

           INITIALIZE PA52F5-COMMENTS-FLAG.
121700     MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY.
121800     INITIALIZE DB-EMP-APP.
121900     MOVE "PA"                   TO DB-CMT-TYPE.
122200     MOVE PA52F5-PCT-ACTION-CODE TO DB-ACTION-CODE.
122000     MOVE PA52F5-PCT-EFFECT-DATE TO DB-DATE.
122000     MOVE PA52F5-PCT-EMPLOYEE    TO DB-EMPLOYEE.
122100     INITIALIZE DB-JOB-CODE.
122300     MOVE PA52F5-PCT-ACTION-NBR  TO DB-LN-NBR.
122400     MOVE PACSET2-LN-NBR         TO WS-DB-BEG-RNG.
122500     PERFORM 850-KFIND-BEGRNG-PACSET2.
           IF (PACOMMENTS-KFOUND)
               MOVE "*"                TO PA52F5-COMMENTS-FLAG.
122600
746800     MOVE PCT-COMPANY            TO DB-COMPANY.
746900     MOVE ZEROES                 TO DB-EMP-APP.
747000     MOVE "PA"                   TO DB-CMT-TYPE.
747100     MOVE ZEROS                  TO DB-EMPLOYEE.
747200     MOVE SPACES                 TO DB-JOB-CODE.
747300     MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE.
747400     MOVE PCT-ACTION-NBR         TO DB-LN-NBR.
747500     MOVE PACSET1-LN-NBR         TO WS-DB-BEG-RNG.
747600     PERFORM 850-FIND-BEGRNG-PACSET1.
747700
747800 481-END.
747900
748000******************************************************************
748100 484-MOVE-CURRENT-DATA.
748200******************************************************************
748300
           IF  (PA52WS-PAT-FLD-NBR-5 (I1) = ZEROES)
               GO TO 484-END.

748400     MOVE PA52WS-PAT-FLD-NBR-5 (I1) TO DB-FLD-NBR.
           INITIALIZE                        DB-COUNTRY-CD-REQ
                                             DB-PROCESS-LEVEL.
748500     PERFORM 840-FIND-PASSET1.
           SET PA52WS-NOT-SECURED         TO TRUE.
748600     IF (PASCRTY-FOUND)
748700         MOVE PAS-SEC-LEVEL         TO HRWS-SEC-LEVEL
748800         PERFORM 730-HR-FIELD-SECURITY
748900         IF (HRWS-FLD-SECURED)
749000             MOVE 411               TO CRT-MSG-NBR
749100             PERFORM 790-GET-MSG
                   MOVE CRT-MESSAGE       TO HREMP-VALUE
                                             PA52WS-SEC-MESSAGE
                   SET PA52WS-SECURED     TO TRUE.
749300
           IF (PA52WS-NOT-SECURED)
749400         MOVE PA52WS-PAT-FLD-NBR-5 (I1)
749400                                    TO HREMP-FLD-NBR
749500         MOVE "Y"                   TO HREMP-FORMAT-FIELD
749600         MOVE PA52F5-PCT-COMPANY    TO HREMP-COMPANY
749700         MOVE PA52F5-PCT-EMPLOYEE   TO HREMP-EMPLOYEE
749800         MOVE SPACES                TO HREMP-VALUE
749900
      * salary class, pay frequency, exempt, pay plan, security level,  
      * and security location
750000         IF (I1 = 13 OR 14 OR 17 OR 18 OR 26 OR 27)
750200             PERFORM 5000-HREMP-RETRIEVE-VALUE
750300         ELSE
750400             PERFORM 5000-PAPEP-RETRIEVE-VALUE 
               END-IF
           END-IF.
750500
750600     IF  (I1 = 1)
750700         MOVE HREMP-VALUE           TO PA52F5-PCT-END-DATE
750800     ELSE
750900     IF  (I1 = 2)
P51466         MOVE HREMP-VALUE           TO PA52F5-PCT-DATE-ASSIGN
751100     ELSE
750900     IF  (I1 = 3)
751000         MOVE HREMP-VALUE           TO PA52F5-PCT-POSITION
751100     ELSE
751200     IF  (I1 = 4)
751300         MOVE HREMP-VALUE           TO PA52F5-PCT-JOBCODE
751400     ELSE
751500     IF  (I1 = 5)
751600         MOVE HREMP-VALUE           TO PA52F5-PCT-PL
751700     ELSE
751800     IF  (I1 = 6)
751900         MOVE HREMP-VALUE           TO PA52F5-PCT-DEPARTMENT
752000     ELSE
752100     IF  (I1 = 7)
752200         MOVE HREMP-VALUE           TO PA52F5-PCT-USER-LEVEL
752300     ELSE
752400     IF  (I1 = 8)
752500         MOVE HREMP-VALUE           TO PA52F5-PCT-SUPERVISOR
752600     ELSE
752700     IF  (I1 = 9)
752800         MOVE HREMP-VALUE           TO PA52F5-PCT-IND-SUPER
752900     ELSE
753000     IF  (I1 = 10)
753100         MOVE HREMP-VALUE           TO PA52F5-PCT-LOCATION
753200     ELSE
753300     IF  (I1 = 11)
753400         MOVE HREMP-VALUE           TO PA52F5-PCT-FTE
753500     ELSE
753600     IF  (I1 = 12)
753800         MOVE HREMP-VALUE           TO PA52F5-PCT-ANNUAL-HOURS
753900     ELSE
754000     IF  (I1 = 13)
754100     AND (PA52F5-POS-LEVEL = 01)
754200         MOVE HREMP-VALUE           TO PA52F5-PCT-SALARY-CLASS
754300     ELSE
754400     IF  (I1 = 14)
754500     AND (PA52F5-POS-LEVEL = 01)
754600         MOVE HREMP-VALUE           TO PA52F5-PCT-PAY-FREQ
754700     ELSE
754800     IF  (I1 = 15)
754900         MOVE HREMP-VALUE           TO PA52F5-PCT-PAY-RATE
755000     ELSE
           IF  (I1 = 16)
               MOVE HREMP-VALUE           TO PA52F5-PCT-CURRENCY-CODE
           ELSE
755100     IF  (I1 = 17)
755200     AND (PA52F5-POS-LEVEL = 01)
755300         MOVE HREMP-VALUE           TO PA52F5-PCT-EXEMPT
755400     ELSE
755500     IF  (I1 = 18)
755600     AND (PA52F5-POS-LEVEL = 01)
755700         MOVE HREMP-VALUE           TO PA52F5-PCT-PAY-PLAN
755800     ELSE
755900     IF  (I1 = 19)
756000         MOVE HREMP-VALUE           TO PA52F5-PCT-UNION
756100     ELSE
755900     IF  (I1 = 20)
756000         MOVE HREMP-VALUE           TO PA52F5-PCT-BARGAIN-UNIT
756100     ELSE
756200     IF  (I1 = 21)
756300         MOVE HREMP-VALUE           TO PA52F5-PCT-SCHEDULE
756400     ELSE
756500     IF  (I1 = 22)
756600         MOVE HREMP-VALUE           TO PA52F5-PCT-GRADE
756700     ELSE
756800     IF  (I1 = 23)
756900         MOVE HREMP-VALUE           TO PA52F5-PCT-STEP
757000     ELSE
757400     IF  (I1 = 24)
757500         MOVE HREMP-VALUE           TO PA52F5-PCT-SHIFT
757600     ELSE
757100     IF  (I1 = 25)
757200         MOVE HREMP-VALUE           TO PA52F5-PCT-WORK-SCHD
757300     ELSE
757700     IF  (I1 = 26)
757800     AND (PA52F5-POS-LEVEL = 01)
757900         MOVE HREMP-VALUE           TO PA52F5-PCT-SECURITY-LVL
758000     ELSE
758100     IF  (I1 = 27)
758200     AND (PA52F5-POS-LEVEL = 01)
758300         MOVE HREMP-VALUE           TO PA52F5-PCT-SECURITY-LOC
758400     ELSE
758500     IF  (I1 = 28)
758700         MOVE HREMP-VALUE           TO PA52F5-PCT-EXP-COMPANY
758800     ELSE
758900     IF  (I1 = 29)
759100         MOVE HREMP-VALUE           TO PA52F5-PCT-ACCT-UNIT
759200     ELSE
759300     IF  (I1 = 30)
759500         MOVE HREMP-VALUE           TO PA52F5-PCT-ACCOUNT
759600     ELSE
759700     IF  (I1 = 31)
759900         MOVE HREMP-VALUE           TO PA52F5-PCT-SUBACCOUNT
760000     ELSE
760100     IF  (I1 = 32)
760300         MOVE HREMP-VALUE           TO PA52F5-PCT-ACTIVITY
760400     ELSE
760500     IF  (I1 = 33)
760700         MOVE HREMP-VALUE           TO PA52F5-PCT-CATEGORY
760800     ELSE
           IF  (I1 = 34)
               MOVE PEP-BASE-PAY-RATE     TO PA52F5-PEP-BASE-PAY-RATE
               MOVE HREMP-VALUE           TO PA52F5-PCT-BASE-PAY-RATE
           ELSE
           IF  (I1 = 35)
               MOVE HREMP-VALUE           TO PA52F5-PCT-BASE-CURRENCY
           ELSE
           IF  (I1 = 36)
               MOVE HREMP-VALUE           TO PA52F5-PCT-USER-AMOUNT
           END-IF.
761600
761700 484-END.
761800
761900******************************************************************
762000 490-MOVE-STARS.
762100******************************************************************
762200
762300     MOVE PA52WS-STAR                TO PA52F5-END-DATE-STAR.
762400     MOVE HREMP-POSITION-DN          TO PA52WS-FLD-NBR.
762500     PERFORM 495-FLD-NBR-CHECK
762600     THRU    495-END.
762700     IF (I1              < 37)
762800         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
762900            MOVE PA52WS-STAR         TO PA52F5-POSITION-STAR.
           MOVE HRPEM-ASSIGN-DATE-DN       TO PA52WS-FLD-NBR.
           PERFORM 495-FLD-NBR-CHECK
           THRU    495-END.
           IF (I1              < 37)
               IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
P51466             MOVE PA52WS-STAR         TO PA52F5-DATE-ASSIGN-STAR.
763000     MOVE HREMP-JOB-CODE-DN          TO PA52WS-FLD-NBR.
763100     PERFORM 495-FLD-NBR-CHECK
763200     THRU    495-END.
763300     IF (I1              < 37)
763400         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
763500            MOVE PA52WS-STAR         TO PA52F5-JOBCODE-STAR.
763600     MOVE HREMP-PROCESS-LEVEL-DN     TO PA52WS-FLD-NBR.
763700     PERFORM 495-FLD-NBR-CHECK
763800     THRU    495-END.
763900     IF (I1              < 37)
764000         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
764100            MOVE PA52WS-STAR         TO PA52F5-PL-STAR.
764200     MOVE HREMP-DEPARTMENT-DN        TO PA52WS-FLD-NBR.
764300     PERFORM 495-FLD-NBR-CHECK
764400     THRU    495-END.
764500     IF (I1              < 37)
764600         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
764700            MOVE PA52WS-STAR         TO PA52F5-DEPARTMENT-STAR.
764800     MOVE HREMP-USER-LEVEL-DN        TO PA52WS-FLD-NBR.
764900     PERFORM 495-FLD-NBR-CHECK
765000     THRU    495-END.
765100     IF (I1              < 37)
765200         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
765300            MOVE PA52WS-STAR         TO PA52F5-USER-LEVEL-STAR.
765400     MOVE HREMP-SUPERVISOR-DN        TO PA52WS-FLD-NBR.
765500     PERFORM 495-FLD-NBR-CHECK
765600     THRU    495-END.
765700     IF (I1              < 37)
765800         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
765900            MOVE PA52WS-STAR         TO PA52F5-SUPERVISOR-STAR.
766000     MOVE HREMP-SUPERVISOR-IND-DN    TO PA52WS-FLD-NBR.
766100     PERFORM 495-FLD-NBR-CHECK
766200     THRU    495-END.
766300     IF (I1              < 37)
766400         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
766500            MOVE PA52WS-STAR         TO PA52F5-IND-SUPER-STAR.
766600     MOVE HRPEM-LOCAT-CODE-DN        TO PA52WS-FLD-NBR.
766700     PERFORM 495-FLD-NBR-CHECK
766800     THRU    495-END.
766900     IF (I1              < 37)
767000         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
767100            MOVE PA52WS-STAR         TO PA52F5-LOCATION-STAR.
767200     MOVE HREMP-NBR-FTE-DN           TO PA52WS-FLD-NBR.
767300     PERFORM 495-FLD-NBR-CHECK
767400     THRU    495-END.
767500     IF (I1              < 37)
767600         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
767700            MOVE PA52WS-STAR         TO PA52F5-FTE-STAR.
767800     MOVE HREMP-ANNUAL-HOURS-DN      TO PA52WS-FLD-NBR.
767900     PERFORM 495-FLD-NBR-CHECK
768000     THRU    495-END.
768100     IF (I1              < 37)
768200         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
768400            MOVE PA52WS-STAR         TO PA52F5-ANNUAL-HOURS-STAR.
768500     MOVE HREMP-SALARY-CLASS-DN      TO PA52WS-FLD-NBR.
768600     PERFORM 495-FLD-NBR-CHECK
768700     THRU    495-END.
768800     IF (I1              < 37)
768900         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
769000         AND (PA52F5-POS-LEVEL = 1)
769100            MOVE PA52WS-STAR         TO PA52F5-SALARY-CLASS-STAR.
769200     MOVE HREMP-PAY-FREQUENCY-DN     TO PA52WS-FLD-NBR.
769300     PERFORM 495-FLD-NBR-CHECK
769400     THRU    495-END.
769500     IF (I1              < 37)
769600         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
769700         AND (PA52F5-POS-LEVEL = 1)
769800            MOVE PA52WS-STAR         TO PA52F5-PAY-FREQ-STAR.
769900     MOVE HREMP-PAY-RATE-DN          TO PA52WS-FLD-NBR.
770000     PERFORM 495-FLD-NBR-CHECK
770100     THRU    495-END.
770200     IF (I1              < 37)
770300         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
770400            MOVE PA52WS-STAR         TO PA52F5-PAY-RATE-STAR.
770500     MOVE HREMP-EXEMPT-EMP-DN        TO PA52WS-FLD-NBR.
770600     PERFORM 495-FLD-NBR-CHECK
770700     THRU    495-END.
770800     IF (I1              < 37)
770900         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
771000         AND (PA52F5-POS-LEVEL = 1)
771100            MOVE PA52WS-STAR         TO PA52F5-EXEMPT-STAR.
771200     MOVE HREMP-OT-PLAN-CODE-DN      TO PA52WS-FLD-NBR.
771300     PERFORM 495-FLD-NBR-CHECK
771400     THRU    495-END.
771500     IF (I1              < 37)
771600         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
771700         AND (PA52F5-POS-LEVEL = 1)
771800            MOVE PA52WS-STAR         TO PA52F5-PAY-PLAN-STAR.
771900     MOVE HREMP-UNION-CODE-DN        TO PA52WS-FLD-NBR.
772000     PERFORM 495-FLD-NBR-CHECK
772100     THRU    495-END.
772200     IF (I1              < 37)
772300         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
772400            MOVE PA52WS-STAR         TO PA52F5-UNION-STAR.
           MOVE HRPEM-BARGAIN-UNIT-DN      TO PA52WS-FLD-NBR.
           PERFORM 495-FLD-NBR-CHECK
           THRU    495-END.
           IF (I1              < 37)
               IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
                  MOVE PA52WS-STAR         TO PA52F5-BARGAIN-UNIT-STAR.
772500     MOVE HREMP-SCHEDULE-DN          TO PA52WS-FLD-NBR.
772600     PERFORM 495-FLD-NBR-CHECK
772700     THRU    495-END.
772800     IF (I1              < 37)
772900         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
773000            MOVE PA52WS-STAR         TO PA52F5-SCHEDULE-STAR.
773100     MOVE HREMP-PAY-GRADE-DN         TO PA52WS-FLD-NBR.
773200     PERFORM 495-FLD-NBR-CHECK
773300     THRU    495-END.
773400     IF (I1              < 37)
773500         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
773600            MOVE PA52WS-STAR         TO PA52F5-GRADE-STAR.
773700     MOVE HREMP-PAY-STEP-DN          TO PA52WS-FLD-NBR.
773800     PERFORM 495-FLD-NBR-CHECK
773900     THRU    495-END.
774000     IF (I1              < 37)
774100         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
774200            MOVE PA52WS-STAR         TO PA52F5-STEP-STAR.
774300     MOVE HREMP-WORK-SCHED-DN        TO PA52WS-FLD-NBR.
774400     PERFORM 495-FLD-NBR-CHECK
774500     THRU    495-END.
774600     IF (I1              < 37)
774700         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
774800            MOVE PA52WS-STAR         TO PA52F5-WORK-SCHD-STAR.
774900     MOVE HREMP-SHIFT-DN             TO PA52WS-FLD-NBR.
775000     PERFORM 495-FLD-NBR-CHECK
775100     THRU    495-END.
775200     IF (I1              < 37)
775300         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
775400            MOVE PA52WS-STAR         TO PA52F5-SHIFT-STAR.
775500     MOVE HREMP-SEC-LVL-DN           TO PA52WS-FLD-NBR.
775600     PERFORM 495-FLD-NBR-CHECK
775700     THRU    495-END.
775800     IF (I1              < 37)
775900         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
776000         AND (PA52F5-POS-LEVEL = 1)
776100            MOVE PA52WS-STAR         TO PA52F5-SECURITY-LVL-STAR.
776200     MOVE HREMP-SEC-LOCATION-DN      TO PA52WS-FLD-NBR.
776300     PERFORM 495-FLD-NBR-CHECK
776400     THRU    495-END.
776500     IF (I1              < 37)
776600         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
P43929         AND (PA52F5-POS-LEVEL = 1)
776800            MOVE PA52WS-STAR         TO PA52F5-SECURITY-LOC-STAR.
776900     MOVE HREMP-HM-DIST-CO-DN        TO PA52WS-FLD-NBR.
777000     PERFORM 495-FLD-NBR-CHECK
777100     THRU    495-END.
777200     IF (I1              < 37)
777300         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
777500            MOVE PA52WS-STAR         TO PA52F5-COMPANY-STAR.
777600     MOVE HREMP-HM-ACCT-UNIT-DN      TO PA52WS-FLD-NBR.
777700     PERFORM 495-FLD-NBR-CHECK
777800     THRU    495-END.
777900     IF (I1              < 37)
778000         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
778200            MOVE PA52WS-STAR         TO PA52F5-ACCT-UNIT-STAR.
778300     MOVE HREMP-HM-ACCOUNT-DN        TO PA52WS-FLD-NBR.
778400     PERFORM 495-FLD-NBR-CHECK
778500     THRU    495-END.
778600     IF (I1              < 37)
778700         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
778900            MOVE PA52WS-STAR         TO PA52F5-ACCOUNT-STAR.
779000     MOVE HREMP-HM-SUB-ACCT-DN       TO PA52WS-FLD-NBR.
779100     PERFORM 495-FLD-NBR-CHECK
779200     THRU    495-END.
779300     IF (I1              < 37)
779400         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
779600            MOVE PA52WS-STAR        TO PA52F5-SUBACCOUNT-STAR.
779700     MOVE HREMP-ACTIVITY-DN         TO PA52WS-FLD-NBR.
779800     PERFORM 495-FLD-NBR-CHECK
779900     THRU    495-END.
780000     IF (I1              < 37)
780100         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
780300            MOVE PA52WS-STAR         TO PA52F5-ACTIVITY-STAR.
780400     MOVE HREMP-ACCT-CATEGORY-DN     TO PA52WS-FLD-NBR.
780500     PERFORM 495-FLD-NBR-CHECK
780600     THRU    495-END.
780700     IF (I1              < 37)
780800         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
781000            MOVE PA52WS-STAR         TO PA52F5-CATEGORY-STAR.
781100     MOVE HREMP-EMP-STATUS-DN        TO PA52WS-FLD-NBR.
781200     PERFORM 495-FLD-NBR-CHECK
781300     THRU    495-END.
781800     MOVE HREMP-TERM-DATE-DN         TO PA52WS-FLD-NBR.
781900     PERFORM 495-FLD-NBR-CHECK
782000     THRU    495-END.
781800     MOVE HREMP-CURRENCY-CODE-DN     TO PA52WS-FLD-NBR.
781900     PERFORM 495-FLD-NBR-CHECK
782000     THRU    495-END.
782100     IF (I1              < 37)
782200         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
782400            MOVE PA52WS-STAR         TO PA52F5-CURRENCY-STAR.
781800     MOVE HREMP-BASE-PAY-RATE-DN     TO PA52WS-FLD-NBR.
781900     PERFORM 495-FLD-NBR-CHECK
782000     THRU    495-END.
782100     IF (I1              < 37)
782200         IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
782400            MOVE PA52WS-STAR         TO PA52F5-BASE-RATE-STAR.
           MOVE HRPEM-USER-AMOUNT-DN      TO PA52WS-FLD-NBR.
           PERFORM 495-FLD-NBR-CHECK
           THRU    495-END.
           IF (I1              < 37)
               IF  (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
                  MOVE PA52WS-STAR         TO PA52F5-USER-AMOUNT-STAR.
782500
782600 490-END.
782700
782800******************************************************************
782900 495-FLD-NBR-CHECK.
783000******************************************************************
783100
783200    MOVE 1 TO I1.
783300    PERFORM
783400        UNTIL  (I1               > 36)
783500        OR     (PAT-FLD-NBR (I1) = PA52WS-FLD-NBR)
783600            ADD 1 TO I1.
783700
783800 495-END.
783900
784000******************************************************************
784100 500-MOVE-DATA.
784200******************************************************************
784300
784400     MOVE PA52F5-PCT-COMPANY         TO PCT-COMPANY.
784500     MOVE "L"                        TO PCT-ACTION-TYPE.
784600     MOVE PA52F5-PCT-ACTION-CODE     TO PCT-ACTION-CODE.
784700     MOVE PA52F5-PCT-EMPLOYEE        TO PCT-EMPLOYEE.
784800     MOVE PA52F5-PCT-EFFECT-DATE     TO PCT-EFFECT-DATE.
784900     MOVE PA52F5-PCT-ANT-END-DATE    TO PCT-ANT-END-DATE.
785000     MOVE PA52F5-PCT-REASON (1)      TO PCT-REASON (1).
785100     MOVE PA52F5-PCT-REASON (2)      TO PCT-REASON (2).
J18332     MOVE PA52F5-PCT-PARTICIPNT      TO PCT-PARTICIPNT.
J18332     MOVE PA52F5-PCT-OCCUR-TYPE      TO PCT-OCCUR-TYPE.
785200     MOVE PAPOS-POS-EFFECT-DATE      TO PCT-POS-EFF-DT.
785300     MOVE PA52F5-POS-LEVEL           TO PCT-POS-LEVEL.
785400     MOVE PA52WS-POS-EFFECT-DATE     TO PCT-POS-EFF-DT.

788600     MOVE PA52F5-PCT-UPDATE-BENEFIT  TO PCT-UPDATE-BENEFIT.
           MOVE PA52F5-PCT-UPD-ABS-MGMT    TO PCT-UPD-ABS-MGMT.
           IF  (PA52F5-USER-ID NOT = SPACES)
               MOVE PA52F5-USER-ID         TO PCT-USER-ID
           ELSE
               MOVE CRT-USER-NAME          TO PCT-USER-ID
           END-IF.
114300     MOVE PA52F5-PCT-UPDATE-REQ-DED  TO PCT-UPDATE-REQ-DED.
114300     MOVE PA52F5-PCT-EDM-EFFECT-DT   TO PCT-EDM-EFFECT-DT.
114300     MOVE PA52F5-PCT-EDM-END-DATE    TO PCT-EDM-END-DATE.
J08104     IF  (PA52F5-FC = "A")
J08104         IF  (PA52F5-USER-ID NOT = SPACES)
J08104             MOVE PA52F5-USER-ID          TO PCT-CREATE-USER-ID
J08104         ELSE
J08104             MOVE CRT-USER-NAME           TO PCT-CREATE-USER-ID
J08104         END-IF
J08104         MOVE WS-SYSTEM-DATE-YMD          TO PCT-CREATE-DATE
J08104         MOVE HHMMSS                      TO PCT-CREATE-TIME
J08104     END-IF.
J08104     MOVE WS-SYSTEM-DATE-YMD              TO PCT-DATE-STAMP.
J08104     MOVE HHMMSS                          TO PCT-TIME-STAMP.
788800
           IF (PA52F5-FC = "A")
P81486     AND (PA52F5-PCT-APPROVAL-FLAG NOT = "L")
               IF (PAT-WORKFLOW-FLAG           = "Y")
                   MOVE "N"                    TO PCT-APPROVAL-FLAG
                                               PA52F5-PCT-APPROVAL-FLAG
               ELSE
                   MOVE "Y"                    TO PCT-APPROVAL-FLAG
                                               PA52F5-PCT-APPROVAL-FLAG
               END-IF
           ELSE
               MOVE PA52F5-PCT-APPROVAL-FLAG   TO PCT-APPROVAL-FLAG
           END-IF.
           MOVE PA52F5-PCT-HIST-CORR-FLAG      TO PCT-HIST-CORR-FLAG.
P86027     IF  (PA52F5-FC = "A")
               MOVE SPACES                     TO PCT-HOLD-FLAG.

788900     MOVE PA52F5-PCT-NEW-END-DATE    TO PCT-NEW-VALUE (1).
789000     MOVE PA52WS-PAT-FLD-NBR-5 (1)   TO PCT-FLD-NBR (1).
P51466     MOVE PA52F5-PCT-NEW-DATE-ASSIGN TO PCT-NEW-VALUE (2).
           MOVE PA52WS-PAT-FLD-NBR-5 (2)   TO PCT-FLD-NBR (2).
789100     MOVE PA52F5-PCT-NEW-POSITION    TO PCT-NEW-VALUE (3).
789200     MOVE PA52WS-PAT-FLD-NBR-5 (3)   TO PCT-FLD-NBR (3).
789300     MOVE PA52F5-PCT-NEW-JOBCODE     TO PCT-NEW-VALUE (4).
789400     MOVE PA52WS-PAT-FLD-NBR-5 (4)   TO PCT-FLD-NBR (4).
789500     MOVE PA52F5-PCT-NEW-PL          TO PCT-NEW-VALUE (5).
789600     MOVE PA52WS-PAT-FLD-NBR-5 (5)   TO PCT-FLD-NBR (5).
789700     MOVE PA52F5-PCT-NEW-DEPARTMENT  TO PCT-NEW-VALUE (6).
789800     MOVE PA52WS-PAT-FLD-NBR-5 (6)   TO PCT-FLD-NBR (6).
789900     MOVE PA52F5-PCT-NEW-USER-LEVEL  TO PCT-NEW-VALUE (7).
790000     MOVE PA52WS-PAT-FLD-NBR-5 (7)   TO PCT-FLD-NBR (7).
790100     MOVE PA52F5-PCT-NEW-SUPERVISOR  TO PCT-NEW-VALUE (8).
790200     MOVE PA52WS-PAT-FLD-NBR-5 (8)   TO PCT-FLD-NBR (8).
790300     MOVE PA52F5-PCT-NEW-IND-SUPER   TO PCT-NEW-VALUE (9).
790400     MOVE PA52WS-PAT-FLD-NBR-5 (9)   TO PCT-FLD-NBR (9).
790500     MOVE PA52F5-PCT-NEW-LOCATION    TO PCT-NEW-VALUE (10).
790600     MOVE PA52WS-PAT-FLD-NBR-5 (10)  TO PCT-FLD-NBR (10).
790700     MOVE PA52F5-PCT-NEW-FTE         TO PCT-NEW-VALUE (11).
790800     MOVE PA52WS-PAT-FLD-NBR-5 (11)  TO PCT-FLD-NBR (11).
790900     MOVE PA52F5-PCT-NEW-ANNUAL-HOURS
791000                                     TO PCT-NEW-VALUE (12).
791100     MOVE PA52WS-PAT-FLD-NBR-5 (12)  TO PCT-FLD-NBR (12).
791200     MOVE PA52F5-PCT-NEW-SALARY-CLASS
791300                                     TO PCT-NEW-VALUE (13).
791400     MOVE PA52WS-PAT-FLD-NBR-5 (13)  TO PCT-FLD-NBR (13).
791500     MOVE PA52F5-PCT-NEW-PAY-FREQ    TO PCT-NEW-VALUE (14).
791600     MOVE PA52WS-PAT-FLD-NBR-5 (14)  TO PCT-FLD-NBR (14).
791700     MOVE PA52F5-PCT-NEW-PAY-RATE    TO PCT-NEW-VALUE (15).
791800     MOVE PA52WS-PAT-FLD-NBR-5 (15)  TO PCT-FLD-NBR (15).
           MOVE PA52F5-PCT-NEW-CURRENCY    TO PCT-NEW-VALUE (16).
           MOVE PA52WS-PAT-FLD-NBR-5 (16)  TO PCT-FLD-NBR (16).
791900     MOVE PA52F5-PCT-NEW-EXEMPT      TO PCT-NEW-VALUE (17).
792000     MOVE PA52WS-PAT-FLD-NBR-5 (17)  TO PCT-FLD-NBR (17).
792100     MOVE PA52F5-PCT-NEW-PAY-PLAN    TO PCT-NEW-VALUE (18).
792200     MOVE PA52WS-PAT-FLD-NBR-5 (18)  TO PCT-FLD-NBR (18).
792300     MOVE PA52F5-PCT-NEW-UNION       TO PCT-NEW-VALUE (19).
792400     MOVE PA52WS-PAT-FLD-NBR-5 (19)  TO PCT-FLD-NBR (19).
           MOVE PA52F5-PCT-NEW-BARGAIN-UNIT
                                           TO PCT-NEW-VALUE (20).
           MOVE PA52WS-PAT-FLD-NBR-5 (20)  TO PCT-FLD-NBR (20).
792500     MOVE PA52F5-PCT-NEW-SCHEDULE    TO PCT-NEW-VALUE (21).
792600     MOVE PA52WS-PAT-FLD-NBR-5 (21)  TO PCT-FLD-NBR (21).
792700     MOVE PA52F5-PCT-NEW-GRADE       TO PCT-NEW-VALUE (22).
792800     MOVE PA52WS-PAT-FLD-NBR-5 (22)  TO PCT-FLD-NBR (22).
792900     MOVE PA52F5-PCT-NEW-STEP        TO PCT-NEW-VALUE (23).
793000     MOVE PA52WS-PAT-FLD-NBR-5 (23)  TO PCT-FLD-NBR (23).
793300     MOVE PA52F5-PCT-NEW-SHIFT       TO PCT-NEW-VALUE (24).
793400     MOVE PA52WS-PAT-FLD-NBR-5 (24)  TO PCT-FLD-NBR (24).
793100     MOVE PA52F5-PCT-NEW-WORK-SCHD   TO PCT-NEW-VALUE (25).
793200     MOVE PA52WS-PAT-FLD-NBR-5 (25)  TO PCT-FLD-NBR (25).
793500     MOVE PA52F5-PCT-NEW-SECURITY-LVL
793600                                     TO PCT-NEW-VALUE (26).
793700     MOVE PA52WS-PAT-FLD-NBR-5 (26)  TO PCT-FLD-NBR (26).
793800     MOVE PA52F5-PCT-NEW-SECURITY-LOC
793900                                     TO PCT-NEW-VALUE (27).
794000     MOVE PA52WS-PAT-FLD-NBR-5 (27)  TO PCT-FLD-NBR (27).
794100     MOVE PA52F5-PCT-NEW-EXP-COMPANY TO PCT-NEW-VALUE (28).
794200     MOVE PA52WS-PAT-FLD-NBR-5 (28)  TO PCT-FLD-NBR (28).
794300     MOVE PA52F5-PCT-NEW-ACCT-UNIT   TO PCT-NEW-VALUE (29).
794400     MOVE PA52WS-PAT-FLD-NBR-5 (29)  TO PCT-FLD-NBR (29).
794500     MOVE PA52F5-PCT-NEW-ACCOUNT     TO PCT-NEW-VALUE (30).
794600     MOVE PA52WS-PAT-FLD-NBR-5 (30)  TO PCT-FLD-NBR (30).
794700     MOVE PA52F5-PCT-NEW-SUBACCOUNT  TO PCT-NEW-VALUE (31).
794800     MOVE PA52WS-PAT-FLD-NBR-5 (31)  TO PCT-FLD-NBR (31).
794900     MOVE PA52F5-PCT-NEW-ACTIVITY    TO PCT-NEW-VALUE (32).
795000     MOVE PA52WS-PAT-FLD-NBR-5 (32)  TO PCT-FLD-NBR (32).
795100     MOVE PA52F5-PCT-NEW-CATEGORY    TO PCT-NEW-VALUE (33).
795200     MOVE PA52WS-PAT-FLD-NBR-5 (33)  TO PCT-FLD-NBR (33).
           MOVE PA52F5-PCT-ENTERED-BASE    TO PCT-NEW-VALUE (34).
           MOVE PA52WS-PAT-FLD-NBR-5 (34)  TO PCT-FLD-NBR (34).
      * Field value for base currency (35) is not included in action
           MOVE PA52WS-PAT-FLD-NBR-5 (35)  TO PCT-FLD-NBR (35).
           MOVE PA52F5-PCT-NEW-USER-AMOUNT TO PCT-NEW-VALUE (36).
           MOVE PA52WS-PAT-FLD-NBR-5 (36)  TO PCT-FLD-NBR (36).
795700
141726*    MOVE PA52F5-PCT-NEW-BASE-CURR   TO PCT-BASE-CURRENCY.
141726     MOVE PAPEP-BASE-CURRENCY        TO PCT-BASE-CURRENCY.
           MOVE PA52F5-PCT-NEW-BASE-ND     TO PCT-BASE-ND.
141726*    MOVE PA52F5-PCT-NEW-BASE-RATE   TO PCT-BASE-PAY-RATE.
313903*    MOVE PAPEP-BASE-PAY-RATE        TO PCT-BASE-PAY-RATE.
313903     IF (PA52F5-BASE-RATE-STAR = "*")
313903         MOVE PAPEP-BASE-PAY-RATE    TO PCT-BASE-PAY-RATE
313903     END-IF.
           MOVE PA52F5-PCT-MERGE-ACTN-NBR  TO PCT-MERGE-ACTN-NBR.
           MOVE PA52F5-PCT-HIST-CORR-FLAG  TO PCT-HIST-CORR-FLAG.
           MOVE PA52F5-PCT-PROCESS-TYPE    TO PCT-PROCESS-TYPE.
           MOVE PA52F5-PCT-MOVE-FROM-LEVEL TO PCT-POS-LEVEL-MOVE.

           INITIALIZE PCT-POSITION
                      PCT-PAY-POSITION
                      PCT-PROCESS-LEVEL
                      PCT-DEPARTMENT
                      PCT-JOB-CODE.

      * SEARCH FOR POSITION-DEFAULTABLE FIELDS INCLUDED IN ACTION
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36)
               IF  (PCT-NEW-VALUE (I1) NOT = SPACES)
                   IF  (PCT-FLD-NBR (I1) = HRPEP-END-DATE-DN)
                   OR  (PCT-FLD-NBR (I1) = HREMP-POSITION-DN)
                       GO TO 500-CONTINUE
                   END-IF
                   PERFORM
                       VARYING I8 FROM 1 BY 1
                       UNTIL  (I8 > PAPOSEMP-TABLE-SIZE)
                       IF  (PCT-FLD-NBR (I1) = PAPOSEMP-EMP-FLD (I8))
                           GO TO 500-CONTINUE
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.

      * IF CONTROL REACHES HERE, THIS PCT DOES NOT AFFECT PEP INFO
           GO TO 500-END.

       500-CONTINUE.

           IF  (PA52WS-RETRO)
               MOVE PA52WS-RETRO-POSITION       TO PCT-PAY-POSITION
               MOVE PA52WS-RETRO-JOB-CODE       TO PCT-JOB-CODE
               MOVE PA52WS-RETRO-PL             TO PCT-PROCESS-LEVEL
               MOVE PA52WS-RETRO-DEPARTMENT     TO PCT-DEPARTMENT
           ELSE
               MOVE PA52WS-POSITION             TO PCT-PAY-POSITION
               MOVE PA52WS-JOB-CODE             TO PCT-JOB-CODE
               MOVE PA52WS-PL                   TO PCT-PROCESS-LEVEL
               MOVE PA52WS-DEPARTMENT           TO PCT-DEPARTMENT
           END-IF.
           MOVE PA52WS-POSITION                 TO PCT-POSITION.

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36)

               IF (PCT-NEW-VALUE (I1) NOT = SPACES)
                   IF (PCT-FLD-NBR (I1) = HREMP-POSITION-DN)
                       IF  (PCT-NEW-VALUE (I1) = "*BLANK")
                           MOVE SPACES             TO PCT-POSITION
                                                      PCT-PAY-POSITION
                       ELSE
                           MOVE PCT-NEW-VALUE (I1) TO PCT-POSITION
                                                      PCT-PAY-POSITION
                       END-IF
                   END-IF
                   IF (PCT-FLD-NBR (I1) = HREMP-JOB-CODE-DN)
                       IF  (PCT-NEW-VALUE (I1) = "*BLANK")
                           MOVE SPACES             TO PCT-JOB-CODE
                       ELSE
                           MOVE PCT-NEW-VALUE (I1) TO PCT-JOB-CODE
                       END-IF
                   END-IF
                   IF (PCT-FLD-NBR (I1) = HREMP-PROCESS-LEVEL-DN)
                       IF  (PCT-NEW-VALUE (I1) = "*BLANK")
                           MOVE SPACES             TO PCT-PROCESS-LEVEL
                       ELSE
                           MOVE PCT-NEW-VALUE (I1) TO PCT-PROCESS-LEVEL
                       END-IF
                   END-IF
                   IF (PCT-FLD-NBR (I1) = HREMP-DEPARTMENT-DN)
                       IF  (PCT-NEW-VALUE (I1) = "*BLANK")
                           MOVE SPACES             TO PCT-DEPARTMENT
                       ELSE
                           MOVE PCT-NEW-VALUE (I1) TO PCT-DEPARTMENT
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

795800 500-END.
795900
823700******************************************************************
823800 600-UPDATE-EMPLOYEE.
823900******************************************************************
824000
824900     MOVE PA52F5-PCT-EFFECT-DATE     TO HREMP-EFFECT-DATE.
825000     MOVE PA52F5-PCT-ACTION-CODE     TO HREMP-ACTION-CODE.
825100     MOVE PA52WS-ACTION-NBR          TO HREMP-ACTION-NBR.
825200     MOVE PA52F5-PCT-REASON (1)      TO HREMP-REASON1.
825300     MOVE PA52F5-PCT-REASON (2)      TO HREMP-REASON2.
           IF  (PA52F5-USER-ID NOT = SPACES)
               MOVE PA52F5-USER-ID         TO HREMP-USER-ID
           ELSE
               MOVE CRT-USER-NAME          TO HREMP-USER-ID
           END-IF.
825500     MOVE PA52F5-PCT-ANT-END-DATE    TO HREMP-ANT-END-DATE.
825700     MOVE PA52F5-PCT-UPDATE-BENEFIT  TO HREMP-UPDATE-BENEFIT.
           MOVE PA52F5-PCT-UPD-ABS-MGMT    TO HREMP-UPDATE-ABSENCE-MGMT.
           MOVE "N"                        TO HRWS-ADD.
           SET HREMP-UPDATE-M3             TO TRUE.
825800     PERFORM 3000-HREMP-PROCESS-TRAN.
825900
           IF (PA52F5-PCT-UPDATE-REQ-DED     = "Y")
467300         MOVE PA52F5-PCT-COMPANY       TO PRRQC-COMPANY
467400         MOVE PA52F5-PCT-EMPLOYEE      TO PRRQC-EMPLOYEE
467500         INITIALIZE PRRQC-DFT-MAR-STAT
467700                    PRRQC-DFT-EXEMPTS
               MOVE PA52F5-PCT-EDM-EFFECT-DT TO PRRQC-EFFECT-DATE
               MOVE PA52F5-PCT-EDM-END-DATE  TO PRRQC-END-DATE
               INITIALIZE PRRQC-UPDATE-OPTION
467800         PERFORM 500-REQ-DED-CREATION.
467900
826000 600-END.
826100******************************************************************
826200 610-CREATE-PARTICIPNT.
826300******************************************************************
826400
826500     IF  (PA52F5-PCT-PARTICIPNT = ZEROS)
826600     AND (PA52WS-PARTICIPNT     = ZEROS)
826700         GO TO 610-END.
826800
826900     IF (PA52WS-PARTICIPNT NOT = ZEROS)
827000         MOVE PA52WS-PARTICIPNT  TO PA52WS-LAST-PART
827100     ELSE
827200         IF (PA52F5-PCT-PARTICIPNT > PA52WS-LAST-PART)
827300             MOVE PA52F5-PCT-PARTICIPNT TO PA52WS-LAST-PART.
827400
           IF (PA52F5-PCT-EFFECT-DATE NOT = ZEROES)
               MOVE PA52F5-PCT-EFFECT-DATE TO PA52WS-DATE
           ELSE
158000         MOVE WS-SYSTEM-DATE-YMD     TO PA52WS-DATE.
158100
           MOVE PA52WS-MONTH               TO WS-MONTH.
158200     COMPUTE WS-MONTH            = WS-MONTH      
158300                                 + PA52WS-OCC-MONTHS-EXT.
158400
828000     PERFORM
828100         UNTIL (WS-MONTH < 13)
828200
828300         SUBTRACT 12             FROM WS-MONTH
828400         ADD 1                   TO PA52WS-YEAR
828500     END-PERFORM.
828600
           MOVE WS-MONTH               TO PA52WS-MONTH.

           MOVE PA52WS-DATE            TO WSDR-FR-DATE.
           PERFORM 900-IS-DATE-INVALID.
           IF (WSDR-DATE-ERROR-EXISTS)
               PERFORM 5000-SET-TO-EOM.

           PERFORM 900-DATE-TO-JULIAN.
           SUBTRACT 1                  FROM WSDR-JULIAN-DAYS.
           PERFORM 900-JULIAN-TO-DATE.
           MOVE WSDR-FR-DATE           TO PA52WS-DATE.
159500
829600     IF (PA52WS-PARTICIPNT NOT = ZEROS)
829700         MOVE PA52WS-PARTICIPNT  TO PA52F5-PCT-PARTICIPNT.
829800
829900     PERFORM 800-CREATE-PARTICIPNT.
830000     MOVE PA52F5-PCT-COMPANY     TO PAR-COMPANY.
830100     MOVE PA52F5-PCT-PARTICIPNT  TO PAR-PARTICIPNT.
830200     MOVE EMP-LAST-NAME          TO PAR-LAST-NAME.
830300     MOVE EMP-FIRST-NAME         TO PAR-FIRST-NAME.
830400     MOVE EMP-MIDDLE-INIT        TO PAR-MIDDLE-INIT.
830500     MOVE EMP-EMPLOYEE           TO PAR-EMPLOYEE.
830600     MOVE EMP-FICA-NBR           TO PAR-FICA-NBR.
830700     MOVE EMP-ADDR1              TO PAR-ADDR1.
830800     MOVE EMP-ADDR2              TO PAR-ADDR2.
830900     MOVE EMP-ADDR3              TO PAR-ADDR3.
831000     MOVE EMP-ADDR4              TO PAR-ADDR4.
831100     MOVE EMP-CITY               TO PAR-CITY.
831200     MOVE EMP-STATE              TO PAR-STATE.
831300     MOVE EMP-ZIP                TO PAR-ZIP.
831400     MOVE EMP-COUNTRY-CODE       TO PAR-COUNTRY-CODE.
831500     MOVE PEM-HM-PHONE-CNTRY     TO PAR-HM-PHONE-CNTRY.
831600     MOVE PEM-HM-PHONE-NBR       TO PAR-HM-PHONE-NBR.
831700     MOVE PEM-BIRTHDATE          TO PAR-BIRTHDATE.
831800     MOVE PEM-SEX                TO PAR-SEX.
831900     MOVE PEM-SMOKER             TO PAR-SMOKER.
832000     MOVE PA52F5-PCT-OCCUR-TYPE  TO PAR-OCCUR-TYPE.
832100     MOVE PA52F5-PCT-EFFECT-DATE TO PAR-OCCUR-DATE.
832200     MOVE PA52WS-DATE            TO PAR-TERM-DATE.
J18332     MOVE WS-SYSTEM-DATE-YMD     TO PAR-CREATE-DATE
J18332                                    PAR-DATE-STAMP.
J18332     MOVE HHMMSS                 TO PAR-CREATE-TIME
J18332                                    PAR-TIME-STAMP.
J18332     MOVE CRT-USER-NAME          TO PAR-CREATE-USER-ID
J18332                                    PAR-USER-ID.
832300     PERFORM 820-STORE-PARTICIPNT.
832400
832500     PERFORM 840-MODIFY-BNCSET1.
832600     MOVE PA52WS-LAST-PART       TO BNC-LAST-PART.
832700     PERFORM 820-STORE-BNCOMPANY.
832800
832900 610-END.
833000
833100******************************************************************
833200 620-MOVE-SCR-TO-WS.
833300******************************************************************

833500     MOVE PA52F5-PCT-COMPANY            TO PAPEP-COMPANY.
833600     MOVE PA52F5-PCT-COMPANY-FN         TO PAPEP-COMPANY-FN.
833700     MOVE PA52F5-PCT-EMPLOYEE           TO PAPEP-EMPLOYEE.
833800     MOVE PA52F5-PCT-EMPLOYEE-FN        TO PAPEP-EMPLOYEE-FN.
833900     MOVE PA52F5-IMMEDIATE-ACTION       TO PAPEP-IMM-ACTION.
834000     IF (PA52F5-PCT-EFFECT-DATE = PADT-PEP-DATE)
834100         MOVE "C"                       TO PAPEP-FC
834200     ELSE
834300     IF (PADT-UPDPEP-DATE NOT = ZEROES)
834400         MOVE "C"                       TO PAPEP-FC
834500     ELSE
834600         MOVE "A"                       TO PAPEP-FC.

834700     MOVE PA52F5-FC-FN                  TO PAPEP-FC-FN.
834800     MOVE PA52F5-POS-LEVEL              TO PAPEP-POS-LEVEL.
834900     MOVE PA52F5-POS-LEVEL-FN           TO PAPEP-POS-LEVEL-FN.
835000     MOVE PA52F5-PCT-EFFECT-DATE        TO PAPEP-EFFECT-DATE.
835100     MOVE PA52F5-PCT-EFFECT-DATE-FN     TO PAPEP-EFFECT-DATE-FN.
835300     MOVE PA52F5-PEP-END-DATE           TO PAPEP-PEP-END-DATE.
835400     MOVE "Y"                           TO PAPEP-ACTION.
           MOVE PA52F5-PCT-ACTION-CODE        TO PAPEP-ACTION-CODE.
835500     IF (PA52F5-PCT-NEW-END-DATE = SPACES)
835600         IF  (PA52F5-PCT-END-DATE NOT = SPACES)
               AND (PA52F5-PCT-END-DATE NOT = PA52WS-SEC-MESSAGE)
835700             MOVE PA52F5-PCT-END-DATE   TO PAPCT-FIELD
835800             PERFORM 5300-CONVERT-DATE
835900             MOVE PAPCT-DATE-FORMAT     TO PAPEP-END-DATE
836000         END-IF
836100     ELSE
836200     IF (PA52F5-PCT-NEW-END-DATE NOT = "*BLANK")
836300         MOVE PA52F5-PCT-NEW-END-DATE   TO PAPCT-FIELD
836400         PERFORM 5300-CONVERT-DATE
836500         MOVE PAPCT-DATE-FORMAT         TO PAPEP-END-DATE.
836600     MOVE PA52F5-PCT-NEW-END-DATE-FN    TO PAPEP-END-DATE-FN.

P51466     IF (PA52F5-PCT-NEW-DATE-ASSIGN = SPACES)
P51466         IF  (PA52F5-PCT-DATE-ASSIGN NOT = SPACES)
P51466         AND (PA52F5-PCT-DATE-ASSIGN NOT = PA52WS-SEC-MESSAGE)
P51466             MOVE PA52F5-PCT-DATE-ASSIGN TO PAPCT-FIELD
                   PERFORM 5300-CONVERT-DATE
                   MOVE PAPCT-DATE-FORMAT      TO PAPEP-ASSIGN-DATE
               END-IF
           ELSE
P51466     IF (PA52F5-PCT-NEW-DATE-ASSIGN NOT = "*BLANK")
P51466         MOVE PA52F5-PCT-NEW-DATE-ASSIGN TO PAPCT-FIELD
               PERFORM 5300-CONVERT-DATE
               MOVE PAPCT-DATE-FORMAT  TO PAPEP-ASSIGN-DATE.
P51466     MOVE PA52F5-PCT-NEW-DATE-ASSIGN-FN  TO PAPEP-ASSIGN-DATE-FN.
836700     IF (PA52F5-PCT-NEW-POSITION = SPACES)
836800         IF  (PA52F5-PCT-POSITION NOT = SPACES)
               AND (PA52F5-PCT-POSITION NOT = PA52WS-SEC-MESSAGE)
836900             MOVE PA52F5-PCT-POSITION   TO PAPEP-POSITION
837000         END-IF
837100     ELSE
837200     IF (PA52F5-PCT-NEW-POSITION NOT = "*BLANK")
837300         MOVE PA52F5-PCT-NEW-POSITION   TO PAPEP-POSITION.
837400     MOVE PA52F5-PCT-NEW-POSITION-FN    TO PAPEP-POSITION-FN.

837500     IF (PA52F5-PCT-NEW-JOBCODE = SPACES)
837600         IF  (PA52F5-PCT-JOBCODE NOT = SPACES)
               AND (PA52F5-PCT-JOBCODE NOT = PA52WS-SEC-MESSAGE)
837700             MOVE PA52F5-PCT-JOBCODE    TO PAPEP-JOB-CODE
837800         END-IF
837900     ELSE
838000     IF (PA52F5-PCT-NEW-JOBCODE NOT = "*BLANK")
838100         MOVE PA52F5-PCT-NEW-JOBCODE    TO PAPEP-JOB-CODE.
838200     MOVE PA52F5-PCT-NEW-JOBCODE-FN     TO PAPEP-JOB-CODE-FN.

838300     IF (PA52F5-PCT-NEW-PL = SPACES)
838400         IF  (PA52F5-PCT-PL NOT = SPACES)
               AND (PA52F5-PCT-PL NOT = PA52WS-SEC-MESSAGE)
838500             MOVE PA52F5-PCT-PL         TO PAPEP-PROCESS-LEVEL
838600         END-IF
838700     ELSE
838800     IF (PA52F5-PCT-NEW-PL NOT = "*BLANK")
838900         MOVE PA52F5-PCT-NEW-PL      TO PAPEP-PROCESS-LEVEL.
839000     MOVE PA52F5-PCT-NEW-PL-FN   TO PAPEP-PROCESS-LEVEL-FN.

839200     IF (PA52F5-PCT-NEW-DEPARTMENT = SPACES)
               IF  (PA52F5-PCT-DEPARTMENT NOT = SPACES)
               AND (PA52F5-PCT-DEPARTMENT NOT = PA52WS-SEC-MESSAGE)
839300             MOVE PA52F5-PCT-DEPARTMENT     TO PAPEP-DEPARTMENT
               END-IF
839400     ELSE
839500     IF (PA52F5-PCT-NEW-DEPARTMENT NOT = "*BLANK")
839600         MOVE PA52F5-PCT-NEW-DEPARTMENT TO PAPEP-DEPARTMENT.
839700     MOVE PA52F5-PCT-NEW-DEPARTMENT-FN  TO PAPEP-DEPARTMENT-FN.

839800     IF  (PA52F5-PCT-NEW-USER-LEVEL = SPACES)
               IF  (PA52F5-PCT-USER-LEVEL NOT = SPACES)
               AND (PA52F5-PCT-USER-LEVEL NOT = PA52WS-SEC-MESSAGE)
839900             MOVE PA52F5-PCT-USER-LEVEL     TO PAPEP-USER-LEVEL
               END-IF
840000     ELSE
840100     IF (PA52F5-PCT-NEW-USER-LEVEL     NOT = "*BLANK")
840200         MOVE PA52F5-PCT-NEW-USER-LEVEL TO PAPEP-USER-LEVEL.
840300     MOVE PA52F5-PCT-NEW-USER-LEVEL-FN  TO PAPEP-USER-LEVEL-FN.

840900     IF  (PA52F5-POS-LEVEL           = 01)
           AND (PA52WS-HREMP-FLDS-FOUND)
841000     AND (PA52F5-PCT-NEW-PAY-RATE = SPACES)
841100         MOVE EMP-PAY-RATE              TO HREMP-PAY-RATE
841200     ELSE
841300     IF  ((PA52F5-POS-LEVEL           > 01)
            OR  ((PA52F5-POS-LEVEL           = 01)
             AND (PA52WS-HREMP-NO-FLDS-FOUND)))
841400     AND (PA52F5-PCT-NEW-PAY-RATE = SPACES)
841500     AND (PA52F5-PCT-PAY-RATE     NOT = SPACES)
           AND (PA52F5-PCT-PAY-RATE     NOT = PA52WS-SEC-MESSAGE)
841600         MOVE PA52F5-PCT-PAY-RATE       TO PAPCT-FIELD
841700         PERFORM 5200-CONVERT-NUMERIC-FIELD
841800         MOVE PAPCT-4-DECIMALS          TO PAPEP-PAY-RATE
841900     ELSE
842000     IF (PA52F5-PCT-NEW-PAY-RATE     NOT = "*BLANK")
842100         MOVE PA52F5-PCT-NEW-PAY-RATE   TO PAPCT-FIELD
842200         PERFORM 5200-CONVERT-NUMERIC-FIELD
842300         MOVE PAPCT-4-DECIMALS          TO PAPEP-PAY-RATE.
842400     MOVE PA52F5-PCT-NEW-PAY-RATE-FN    TO PAPEP-PAY-RATE-FN.

           IF  (PA52F5-PCT-NEW-CURRENCY         = SPACES)
               IF  (PA52F5-PCT-CURRENCY-CODE NOT = SPACES)
               AND (PA52F5-PCT-CURRENCY-CODE NOT = PA52WS-SEC-MESSAGE)
                   MOVE PA52F5-PCT-CURRENCY-CODE  TO PAPEP-CURRENCY-CODE
               END-IF
           ELSE
           IF  (PA52F5-PCT-NEW-CURRENCY     NOT = "*BLANK")
               MOVE PA52F5-PCT-NEW-CURRENCY    TO PAPEP-CURRENCY-CODE.
           MOVE PA52F5-PCT-NEW-CURRENCY-FN    TO PAPEP-CURRENCY-CODE-FN.

842500     IF  (PA52F5-PCT-NEW-SUPERVISOR = SPACES)
               IF  (PA52F5-PCT-SUPERVISOR NOT = SPACES)
               AND (PA52F5-PCT-SUPERVISOR NOT = PA52WS-SEC-MESSAGE)
842600             MOVE PA52F5-PCT-SUPERVISOR     TO PAPEP-SUPERVISOR
               END-IF
842700     ELSE
842800     IF (PA52F5-PCT-NEW-SUPERVISOR     NOT = "*BLANK")
842900         MOVE PA52F5-PCT-NEW-SUPERVISOR TO PAPEP-SUPERVISOR.
843000     MOVE PA52F5-PCT-NEW-SUPERVISOR-FN  TO PAPEP-SUPERVISOR-FN.

843100     IF  (PA52F5-PCT-NEW-IND-SUPER = SPACES)
               IF  (PA52F5-PCT-IND-SUPER NOT = SPACES)
               AND (PA52F5-PCT-IND-SUPER NOT = PA52WS-SEC-MESSAGE)
843200             MOVE PA52F5-PCT-IND-SUPER     TO PAPEP-SUPERVISOR-IND
               END-IF
843300     ELSE
843400     IF (PA52F5-PCT-NEW-IND-SUPER     NOT = "*BLANK")
843500         MOVE PA52F5-PCT-NEW-IND-SUPER  TO PAPEP-SUPERVISOR-IND.
843600     MOVE PA52F5-PCT-NEW-IND-SUPER-FN   TO
843700                                        PAPEP-SUPERVISOR-IND-FN.

843800     IF  (PA52F5-PCT-NEW-SCHEDULE = SPACES)
               IF  (PA52F5-PCT-SCHEDULE NOT = SPACES)
               AND (PA52F5-PCT-SCHEDULE NOT = PA52WS-SEC-MESSAGE)
843900             MOVE PA52F5-PCT-SCHEDULE       TO PAPEP-SCHEDULE
               END-IF
844000     ELSE
844100     IF (PA52F5-PCT-NEW-SCHEDULE     NOT = "*BLANK")
844200         MOVE PA52F5-PCT-NEW-SCHEDULE   TO PAPEP-SCHEDULE.
844300     MOVE PA52F5-PCT-NEW-SCHEDULE-FN    TO PAPEP-SCHEDULE-FN.

844400     IF  (PA52F5-PCT-NEW-GRADE = SPACES)
               IF  (PA52F5-PCT-GRADE NOT = SPACES)
               AND (PA52F5-PCT-GRADE NOT = PA52WS-SEC-MESSAGE)
844500             MOVE PA52F5-PCT-GRADE          TO PAPEP-PAY-GRADE
               END-IF
844600     ELSE
844700     IF (PA52F5-PCT-NEW-GRADE     NOT = "*BLANK")
844800         MOVE PA52F5-PCT-NEW-GRADE      TO PAPEP-PAY-GRADE.
844900     MOVE PA52F5-PCT-NEW-GRADE-FN       TO PAPEP-PAY-GRADE-FN.

845000     IF (PA52F5-PCT-NEW-STEP = SPACES)
845100         IF  (PA52F5-PCT-STEP     NOT = SPACES)
               AND (PA52F5-PCT-STEP     NOT = PA52WS-SEC-MESSAGE)
845200             MOVE PA52F5-PCT-STEP       TO PAPCT-FIELD
845300             PERFORM 5200-CONVERT-NUMERIC-FIELD
845400             MOVE PAPCT-NUMERIC         TO PAPEP-PAY-STEP
845500         END-IF
845600     ELSE
845700     IF (PA52F5-PCT-NEW-STEP     NOT = "*BLANK")
845800         MOVE PA52F5-PCT-NEW-STEP       TO PAPCT-FIELD
845900         PERFORM 5200-CONVERT-NUMERIC-FIELD
846000         MOVE PAPCT-NUMERIC             TO PAPEP-PAY-STEP.
846100     MOVE PA52F5-PCT-NEW-STEP-FN        TO PAPEP-PAY-STEP-FN.

846200     IF  (PA52F5-PCT-NEW-LOCATION = SPACES)
               IF  (PA52F5-PCT-LOCATION NOT = SPACES)
               AND (PA52F5-PCT-LOCATION NOT = PA52WS-SEC-MESSAGE)
846300             MOVE PA52F5-PCT-LOCATION       TO PAPEP-LOCAT-CODE
               END-IF
846400     ELSE
846500     IF  (PA52F5-PCT-NEW-LOCATION     NOT = "*BLANK")
846600         MOVE PA52F5-PCT-NEW-LOCATION   TO PAPEP-LOCAT-CODE.
846700     MOVE PA52F5-PCT-NEW-LOCATION-FN    TO PAPEP-LOCAT-CODE-FN.

846800     IF  (PA52F5-PCT-NEW-WORK-SCHD = SPACES)
               IF  (PA52F5-PCT-WORK-SCHD NOT = SPACES)
               AND (PA52F5-PCT-WORK-SCHD NOT = PA52WS-SEC-MESSAGE)
846900             MOVE PA52F5-PCT-WORK-SCHD      TO PAPEP-WORK-SCHED
               END-IF
847000     ELSE
847100     IF (PA52F5-PCT-NEW-WORK-SCHD     NOT = "*BLANK")
847200         MOVE PA52F5-PCT-NEW-WORK-SCHD  TO PAPEP-WORK-SCHED.
847300     MOVE PA52F5-PCT-NEW-WORK-SCHD-FN   TO PAPEP-WORK-SCHED-FN.

847700     IF (PA52F5-PCT-NEW-SHIFT = SPACES)
               IF  (PA52F5-PCT-SHIFT NOT = SPACES)
               AND (PA52F5-PCT-SHIFT NOT = PA52WS-SEC-MESSAGE)
847800             MOVE PA52F5-PCT-SHIFT      TO PAPCT-FIELD
847900             PERFORM 5200-CONVERT-NUMERIC-FIELD
848000             MOVE PAPCT-NUMERIC         TO PAPEP-SHIFT
               END-IF
848100     ELSE
848200     IF (PA52F5-PCT-NEW-SHIFT     NOT = "*BLANK")
848300         MOVE PA52F5-PCT-NEW-SHIFT      TO PAPCT-FIELD
848400         PERFORM 5200-CONVERT-NUMERIC-FIELD
848500         MOVE PAPCT-NUMERIC             TO PAPEP-SHIFT.
848600     MOVE PA52F5-PCT-NEW-SHIFT-FN       TO PAPEP-SHIFT-FN.

848700     IF  (PA52F5-PCT-NEW-UNION = SPACES)
               IF  (PA52F5-PCT-UNION NOT = SPACES)
               AND (PA52F5-PCT-UNION NOT = PA52WS-SEC-MESSAGE)
848800             MOVE PA52F5-PCT-UNION          TO PAPEP-UNION-CODE
               END-IF
848900     ELSE
849000     IF (PA52F5-PCT-NEW-UNION     NOT = "*BLANK")
849100         MOVE PA52F5-PCT-NEW-UNION      TO PAPEP-UNION-CODE.
849200     MOVE PA52F5-PCT-NEW-UNION-FN       TO PAPEP-UNION-CODE-FN.

           IF (PA52F5-PCT-NEW-BARGAIN-UNIT  = SPACES)
               IF  (PA52F5-PCT-BARGAIN-UNIT NOT = SPACES)
               AND (PA52F5-PCT-BARGAIN-UNIT NOT = PA52WS-SEC-MESSAGE)
                   MOVE PA52F5-PCT-BARGAIN-UNIT TO PAPEP-BARGAIN-UNIT
               END-IF
           ELSE
           IF (PA52F5-PCT-NEW-BARGAIN-UNIT NOT = "*BLANK")
               MOVE PA52F5-PCT-NEW-BARGAIN-UNIT TO PAPEP-BARGAIN-UNIT.
           MOVE PA52F5-PCT-NEW-BARGAIN-UNIT-FN  TO
                                                PAPEP-BARGAIN-UNIT-FN.

           IF  (PA52F5-PCT-NEW-ANNUAL-HOURS     = SPACES)
               IF  (PA52F5-PCT-ANNUAL-HOURS NOT = SPACES)
               AND (PA52F5-PCT-ANNUAL-HOURS NOT = PA52WS-SEC-MESSAGE)
                   MOVE PA52F5-PCT-ANNUAL-HOURS    TO PAPCT-FIELD
                   PERFORM 5200-CONVERT-NUMERIC-FIELD
J46844*             MOVE PAPCT-NUMERIC              TO PAPEP-ANNUAL-HOURS
J46844             MOVE PAPCT-4-DECIMALS           TO PAPEP-ANNUAL-HOURS
               END-IF
           ELSE
           IF  (PA52F5-PCT-NEW-ANNUAL-HOURS NOT = "*BLANK")
               MOVE PA52F5-PCT-NEW-ANNUAL-HOURS TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
J46844*         MOVE PAPCT-NUMERIC              TO PAPEP-ANNUAL-HOURS
J46844         MOVE PAPCT-4-DECIMALS           TO PAPEP-ANNUAL-HOURS
           END-IF.
           MOVE PA52F5-PCT-NEW-ANNUAL-HOURS-FN TO PAPEP-ANNUAL-HOURS-FN.

           IF  (PA52F5-PCT-NEW-EXP-COMPANY      = SPACES)
               IF  (PA52F5-PCT-EXP-COMPANY NOT = SPACES)
               AND (PA52F5-PCT-EXP-COMPANY NOT = PA52WS-SEC-MESSAGE)
                   MOVE PA52F5-PCT-EXP-COMPANY     TO PAPCT-FIELD
                   PERFORM 5200-CONVERT-NUMERIC-FIELD
                   MOVE PAPCT-NUMERIC              TO PAPEP-EXP-DIST-CO
               END-IF
           ELSE
           IF  (PA52F5-PCT-NEW-EXP-COMPANY  NOT = "*BLANK")
               MOVE PA52F5-PCT-NEW-EXP-COMPANY TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC              TO PAPEP-EXP-DIST-CO.
           MOVE PA52F5-PCT-NEW-EXP-COMPANY-FN  TO PAPEP-EXP-DIST-CO-FN.

           IF  (PA52F5-PCT-NEW-ACCT-UNIT        = SPACES)
               IF  (PA52F5-PCT-ACCT-UNIT NOT = SPACES)
               AND (PA52F5-PCT-ACCT-UNIT NOT = PA52WS-SEC-MESSAGE)
                   MOVE PA52F5-PCT-ACCT-UNIT      TO PAPEP-EXP-ACCT-UNIT
               END-IF
           ELSE
           IF  (PA52F5-PCT-NEW-ACCT-UNIT    NOT = "*BLANK")
               MOVE PA52F5-PCT-NEW-ACCT-UNIT   TO PAPEP-EXP-ACCT-UNIT.
           MOVE PA52F5-PCT-NEW-ACCT-UNIT-FN   TO PAPEP-EXP-ACCT-UNIT-FN.

           IF  (PA52F5-PCT-NEW-ACCOUNT          = SPACES)
               IF  (PA52F5-PCT-ACCOUNT NOT = SPACES)
               AND (PA52F5-PCT-ACCOUNT NOT = PA52WS-SEC-MESSAGE)
                   MOVE PA52F5-PCT-ACCOUNT         TO PAPCT-FIELD
                   PERFORM 5200-CONVERT-NUMERIC-FIELD
                   MOVE PAPCT-NUMERIC              TO PAPEP-EXP-ACCOUNT
               END-IF
           ELSE
           IF  (PA52F5-PCT-NEW-ACCOUNT      NOT = "*BLANK")
               MOVE PA52F5-PCT-NEW-ACCOUNT     TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC              TO PAPEP-EXP-ACCOUNT.
           MOVE PA52F5-PCT-NEW-ACCOUNT-FN      TO PAPEP-EXP-ACCOUNT-FN.

           IF  (PA52F5-PCT-NEW-SUBACCOUNT       = SPACES)
               IF  (PA52F5-PCT-SUBACCOUNT NOT = SPACES)
               AND (PA52F5-PCT-SUBACCOUNT NOT = PA52WS-SEC-MESSAGE)
                   MOVE PA52F5-PCT-SUBACCOUNT      TO PAPCT-FIELD
                   PERFORM 5200-CONVERT-NUMERIC-FIELD
                   MOVE PAPCT-NUMERIC              TO PAPEP-EXP-SUB-ACCT
               END-IF
           ELSE
           IF  (PA52F5-PCT-NEW-SUBACCOUNT   NOT = "*BLANK")
               MOVE PA52F5-PCT-NEW-SUBACCOUNT  TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC              TO PAPEP-EXP-SUB-ACCT.
           MOVE PA52F5-PCT-NEW-SUBACCOUNT-FN   TO PAPEP-EXP-SUB-ACCT-FN.

           IF  (PA52F5-PCT-NEW-ACTIVITY         = SPACES)
               IF  (PA52F5-PCT-ACTIVITY NOT = SPACES)
               AND (PA52F5-PCT-ACTIVITY NOT = PA52WS-SEC-MESSAGE)
                   MOVE PA52F5-PCT-ACTIVITY        TO PAPEP-ACTIVITY
               END-IF
           ELSE
           IF  (PA52F5-PCT-NEW-ACTIVITY     NOT = "*BLANK")
               MOVE PA52F5-PCT-NEW-ACTIVITY    TO PAPEP-ACTIVITY.
           MOVE PA52F5-PCT-NEW-ACTIVITY-FN     TO PAPEP-ACTIVITY-FN.

           IF  (PA52F5-PCT-NEW-CATEGORY         = SPACES)
               IF  (PA52F5-PCT-CATEGORY NOT = SPACES)
               AND (PA52F5-PCT-CATEGORY NOT = PA52WS-SEC-MESSAGE)
                   MOVE PA52F5-PCT-CATEGORY       TO PAPEP-ACCT-CATEGORY
               END-IF
           ELSE
           IF  (PA52F5-PCT-NEW-CATEGORY     NOT = "*BLANK")
               MOVE PA52F5-PCT-NEW-CATEGORY    TO PAPEP-ACCT-CATEGORY.
               MOVE PA52F5-PCT-NEW-CATEGORY-FN TO
                                               PAPEP-ACCT-CATEGORY-FN.
           
           INITIALIZE PAPEP-BASE-OVERRIDE-SW.
           IF  (PA52F5-POS-LEVEL           > 01)
           OR  ((PA52F5-POS-LEVEL           = 01)
            AND (PA52WS-HREMP-NO-FLDS-FOUND))
               IF  (PAPEP-FC = "C")
               AND (PA52F5-PCT-BASE-CURRENCY NOT = PA52WS-SEC-MESSAGE)
                   MOVE PA52F5-PCT-BASE-CURRENCY
                                               TO PAPEP-BASE-CURRENCY
                   MOVE PA52F5-PCT-NEW-BASE-CURR-FN
                                               TO PAPEP-BASE-CURRENCY-FN
               END-IF
               IF  ((PA52F5-PCT-ENTERED-BASE = SPACES)
               OR   (PA52F5-PCT-ENTERED-BASE = PA52WS-SEC-MESSAGE))
               AND ((PA52F5-PCT-BASE-PAY-RATE = SPACES)
               OR   (PA52F5-PCT-BASE-PAY-RATE = PA52WS-SEC-MESSAGE))
                   INITIALIZE PAPEP-BASE-PAY-RATE
               ELSE
                   IF  (PA52F5-PCT-ENTERED-BASE  = SPACES)
                   OR  (PA52F5-PCT-ENTERED-BASE = PA52WS-SEC-MESSAGE)
                       IF (PA52F5-PCT-BASE-PAY-RATE NOT =
                                                    PA52WS-SEC-MESSAGE)
                           MOVE PA52F5-PCT-BASE-PAY-RATE  TO PAPCT-FIELD
                           PERFORM 5200-CONVERT-NUMERIC-FIELD
                           MOVE PAPCT-4-DECIMALS  TO PAPEP-BASE-PAY-RATE
                       END-IF
                   ELSE
                       SET PAPEP-BASE-OVERRIDE TO TRUE
                       IF  (PA52F5-PCT-ENTERED-BASE = "*BLANK")
                           MOVE ZEROES            TO PAPEP-BASE-PAY-RATE
                       ELSE
                           MOVE PA52F5-PCT-ENTERED-BASE   TO PAPCT-FIELD
                           PERFORM 5200-CONVERT-NUMERIC-FIELD
                           MOVE PAPCT-4-DECIMALS  TO PAPEP-BASE-PAY-RATE
                       END-IF
                   END-IF
               END-IF
               MOVE PA52F5-PCT-ENTERED-BASE-FN
                                               TO PAPEP-BASE-PAY-RATE-FN
           END-IF.

           IF  (PA52F5-USER-ID NOT = SPACES)
               MOVE PA52F5-USER-ID         TO PAPEP-USER-ID
           ELSE
               MOVE CRT-USER-NAME          TO PAPEP-USER-ID
           END-IF.

849300     INITIALIZE                         PAPEPWS-SUM-FTE.
849400
849500     MOVE PA52F5-PCT-COMPANY           TO DB-COMPANY.
849600     MOVE PA52F5-PCT-EMPLOYEE          TO DB-EMPLOYEE.
849700     MOVE PEPSET2-EMPLOYEE             TO WS-DB-BEG-RNG.
849800     PERFORM 850-FIND-BEGRNG-PEPSET2.
849900     INITIALIZE                           PAPEPWS-SUM-FTE.
850000     PERFORM 640-CHECK-ADD
850100     THRU    640-END
850200         UNTIL (PAEMPPOS-NOTFOUND)
850300         OR    (PEP-COMPANY  NOT = DB-COMPANY)
850400         OR    (PEP-EMPLOYEE NOT = DB-EMPLOYEE).
850500
850600     IF  (PA52F5-PCT-NEW-FTE = SPACES)
850700     AND (PA52F5-PCT-FTE     NOT = SPACES)
           AND (PA52F5-PCT-FTE     NOT = PA52WS-SEC-MESSAGE)
850800         MOVE PA52F5-PCT-FTE       TO PAPCT-FIELD
850900         PERFORM 5200-CONVERT-NUMERIC-FIELD
851000         MOVE PAPCT-6-DECIMALS          TO PAPEP-NBR-FTE
851100         IF (PA52F5-PCT-NEW-END-DATE = SPACES)
851200         OR (PA52F5-PCT-NEW-END-DATE = "*BLANK")
851300             COMPUTE PAPEPWS-SUM-FTE =
851400                        (PAPEPWS-SUM-FTE + PAPCT-6-DECIMALS)
851500         END-IF
851600     ELSE
851700     IF  (PA52F5-PCT-NEW-FTE     NOT = "*BLANK")
           AND (PA52F5-PCT-FTE     NOT = PA52WS-SEC-MESSAGE)
851800         MOVE PA52F5-PCT-NEW-FTE   TO PAPCT-FIELD
851900         PERFORM 5200-CONVERT-NUMERIC-FIELD
852000         MOVE PAPCT-6-DECIMALS          TO PAPEP-NBR-FTE
852100         IF (PA52F5-PCT-NEW-END-DATE = SPACES)
852200         OR (PA52F5-PCT-NEW-END-DATE = "*BLANK")
852300             COMPUTE PAPEPWS-SUM-FTE =
852400                           (PAPEPWS-SUM-FTE + PAPCT-6-DECIMALS).
852500     MOVE PA52F5-PCT-NEW-FTE-FN         TO PAPEP-NBR-FTE-FN.
852600
850600     IF  (PA52F5-PCT-NEW-USER-AMOUNT = SPACES)
850700     AND (PA52F5-PCT-USER-AMOUNT    NOT = SPACES)
           AND (PA52F5-PCT-USER-AMOUNT    NOT = PA52WS-SEC-MESSAGE)
850800         MOVE PA52F5-PCT-USER-AMOUNT    TO PAPCT-FIELD
850900         PERFORM 5200-CONVERT-NUMERIC-FIELD
851000         MOVE PAPCT-4-DECIMALS          TO PAPEP-USER-AMOUNT
           ELSE
           IF (PA52F5-PCT-NEW-USER-AMOUNT NOT = "*BLANK")
               MOVE PA52F5-PCT-NEW-USER-AMOUNT TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
P84591         MOVE PAPCT-4-DECIMALS          TO PAPEP-USER-AMOUNT.
P84591*        MOVE PAPCT-NUMERIC             TO PAPEP-USER-AMOUNT.
852500     MOVE PA52F5-PCT-NEW-USER-AMOUNT-FN TO PAPEP-USER-AMOUNT-FN.
852600
853000     IF (EMP-EMP-STATUS NOT = SPACES)
853100         IF   (PA52F5-POS-LEVEL      > 01)
853200         OR  ((PA52F5-POS-LEVEL      = 01)
853300         AND  (PA52WS-HREMP-NO-FLDS-FOUND))
853400             MOVE PA52F5-PCT-COMPANY       TO DB-COMPANY
853500             MOVE EMP-EMP-STATUS           TO DB-EMP-STATUS
853600             PERFORM 840-FIND-EMSSET1
853700             IF (EMSTATUS-FOUND)
853800                 MOVE EMS-COUNT            TO PAPEP-BCOUNT
                                                    PAPEP-COUNT.
853900
240900     IF  (PAPEP-FC                    = "A")
241000     AND (PAPEP-EFFECT-DATE       NOT = ZEROES)
               MOVE PAPEP-COMPANY          TO DB-COMPANY
               MOVE PAPEP-EMPLOYEE         TO DB-EMPLOYEE
               MOVE PAPEP-POS-LEVEL        TO DB-POS-LEVEL
P58592         MOVE PEPSET3-POS-LEVEL      TO WS-DB-BEG-RNG
P58592         PERFORM 850-FIND-BEGRNG-PEPSET3
241100         IF   (PAEMPPOS-FOUND)
241200         AND ((PAPEP-JOB-CODE           = PEP-JOB-CODE)
               AND  (PAPEP-ASSIGN-DATE        = PEP-DATE-ASSIGN)
241300         AND  (PAPEP-POSITION           = PEP-POSITION)
241400         AND  (PAPEP-PROCESS-LEVEL      = PEP-PROCESS-LEVEL)
241500         AND  (PAPEP-DEPARTMENT         = PEP-DEPARTMENT)
241600         AND  (PAPEP-USER-LEVEL         = PEP-USER-LEVEL)
241700         AND  (PAPEP-SCHEDULE           = PEP-SCHEDULE)
241800         AND  (PAPEP-PAY-GRADE          = PEP-PAY-GRADE)
241900         AND  (PAPEP-PAY-STEP           = PEP-PAY-STEP)
242000         AND  (PAPEP-SUPERVISOR         = PEP-SUPERVISOR)
242100         AND  (PAPEP-SUPERVISOR-IND     = PEP-SUPERVISOR-IND)
242200         AND  (PAPEP-SHIFT              = PEP-SHIFT)
242300         AND  (PAPEP-LOCAT-CODE         = PEP-LOCAT-CODE)
242400         AND  (PAPEP-PAY-RATE           = PEP-PAY-RATE)
242500         AND  (PAPEP-UNION-CODE         = PEP-UNION-CODE)
               AND  (PAPEP-BARGAIN-UNIT       = PEP-BARGAIN-UNIT)
242600         AND  (PAPEP-NBR-FTE            = PEP-FTE)
242700         AND  (PAPEP-WORK-SCHED         = PEP-WORK-SCHED)
               AND  (PAPEP-ANNUAL-HOURS       = PEP-ANNUAL-HOURS)
               AND  (PAPEP-EXP-DIST-CO        = PEP-EXP-COMPANY)
               AND  (PAPEP-EXP-ACCT-UNIT      = PEP-EXP-ACCT-UNIT)
               AND  (PAPEP-EXP-ACCOUNT        = PEP-EXP-ACCOUNT)
               AND  (PAPEP-EXP-SUB-ACCT       = PEP-EXP-SUB-ACCT)
               AND  (PAPEP-ACTIVITY           = PEP-ACTIVITY)
               AND  (PAPEP-ACCT-CATEGORY      = PEP-ACCT-CATEGORY)
               AND  (PAPEP-USER-AMOUNT        = PEP-USER-AMOUNT)
               AND  ((PAPEP-CURRENCY-CODE     = PEP-CURRENCY-CODE)
               OR   (PAPEP-CURRENCY-CODE      = SPACES)
               OR   (PEP-CURRENCY-CODE        = SPACES))
               AND  ((PAPEP-BASE-CURRENCY     = PEP-BASE-CURRENCY)
               OR   (PAPEP-BASE-CURRENCY      = SPACES )
               OR   (PEP-BASE-CURRENCY        = SPACES ))
               AND  ((PAPEP-BASE-PAY-RATE     = PEP-BASE-PAY-RATE)
               OR   (PAPEP-BASE-PAY-RATE      = ZEROES )))
242800         AND  (PAPEP-END-DATE       NOT = PEP-END-DATE)
                   IF  (PEP-END-DATE      NOT = ZEROES)
                   AND (PAPEP-END-DATE    = ZEROES)
                   AND (PAPEP-EFFECT-DATE > PEP-END-DATE)
                       CONTINUE
                   ELSE 
                       MOVE "C"            TO PAPEP-FC.

855800 620-END.
855900
856000******************************************************************
856100 640-CHECK-ADD.
856200******************************************************************
856300
856400     IF (PA52F5-POS-LEVEL      NOT = PEP-POS-LEVEL)
856500         ADD PEP-FTE           TO PAPEPWS-SUM-FTE.
856600
856700     PERFORM 860-FIND-NXTRNG-PEPSET2.
856800
856900 640-END.
857000
857100******************************************************************
857200 650-DEFAULT.
857300******************************************************************
857400
857500     IF (PA52F5-PCT-NEW-POSITION       = SPACES)
857600         MOVE 112                        TO CRT-ERROR-NBR
857700         MOVE PA52F5-PCT-POSITION-FN     TO CRT-FIELD-NBR
857800         GO TO 650-END.
857900
858000     IF (PA52F5-PCT-EFFECT-DATE = ZEROES)
858100         MOVE 106                        TO CRT-ERROR-NBR
858200         MOVE PA52F5-PCT-EFFECT-DATE-FN  TO CRT-FIELD-NBR
858300         GO TO 650-END.
858400
858500     PERFORM 725-DEFAULT-ACTION
858600     THRU    725-END.
858700
858800     IF (ERROR-FOUND)
858900         GO TO 650-END
859000     ELSE
859100         MOVE "PA52"                     TO CRT-ERROR-CAT.
859200
859300     PERFORM 825-GET-DESCRIPTIONS
859400     THRU    825-END.
859500
859600     MOVE 109                            TO CRT-MSG-NBR.
859700     MOVE PA52F5-FC-FN                   TO CRT-FIELD-NBR.
859800
859900 650-END.
860000
477300******************************************************************
477400 700-MOVE-LEVEL.    
477500******************************************************************
477600
           IF (PA52F5-PCT-MOVE-FROM-LEVEL = ZEROES)
               MOVE 156                    TO CRT-ERROR-NBR
               MOVE PA52F5-PCT-MOVE-FROM-LEVEL-FN
                                           TO CRT-FIELD-NBR
               GO TO 700-END.

           PERFORM
               VARYING I9 FROM 1 BY 1
               UNTIL  (I9 > PAPOSPEP-TABLE-SIZE)
               OR     (ERROR-FOUND)
                   MOVE PAPOSPEP-EMP-FLD (I9)  TO DB-FLD-NBR
                   INITIALIZE                     DB-COUNTRY-CD-REQ
                                                  DB-PROCESS-LEVEL
                   PERFORM 840-FIND-PASSET1
                   IF (PASCRTY-FOUND)
                       MOVE PAS-SEC-LEVEL      TO HRWS-SEC-LEVEL
                       PERFORM 730-HR-FIELD-SECURITY
                       IF (HRWS-FLD-SECURED)
                           MOVE 157             TO CRT-ERROR-NBR
                           MOVE PA52F5-FC-FN    TO CRT-FIELD-NBR
                           INITIALIZE          PA52F5-PCT-PROCESS-TYPE
                                              PA52F5-PCT-MOVE-FROM-LEVEL
                           GO TO 700-END
                       END-IF
                   END-IF
           END-PERFORM.

           INITIALIZE                         PADFP-FIELDS.

           PERFORM 775-LOAD-PADFP-FIELDS
           THRU    775-END.

           PERFORM 2000-PADFPEP-EDIT-DEFAULTS.

           PERFORM 800-MOVE-WS-TO-SCR
           THRU    800-END.

           PERFORM 825-GET-DESCRIPTIONS
           THRU    825-END.

           SET PA52WS-MOVELEVELDONE        TO TRUE.

           MOVE 109                        TO CRT-MSG-NBR.
           MOVE PA52F5-FC-FN               TO CRT-FIELD-NBR.

       700-END.

860100******************************************************************
860200 725-DEFAULT-ACTION.
860300******************************************************************
860400
           SET PA52WS-USE-COMPANY          TO TRUE.
           IF (PA52WS-POSITION NOT = SPACES)
               MOVE PA52F5-PCT-COMPANY     TO DB-COMPANY
               MOVE PA52WS-POSITION        TO DB-POSITION
               MOVE PA52F5-PCT-EFFECT-DATE TO DB-EFFECT-DATE
               PERFORM 850-FIND-NLT-POSSET2
               IF  (PAPOSITION-FOUND)
               AND (POS-COMPANY           = DB-COMPANY)
               AND (POS-POSITION          = DB-POSITION)
               AND (POS-PROCESS-LEVEL NOT = SPACES)
                   MOVE "PO"               TO DB-TOPIC
                   MOVE SPACES             TO DB-COUNTRY-CD-REQ
                   MOVE POS-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
                   MOVE PASSET2-PROCESS-LEVEL
                                           TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-PASSET2
                   IF (PASCRTY-FOUND)
                       SET PA52WS-USE-PROCESS-LEVEL
                                           TO TRUE.

861000     PERFORM 750-CHECK-SECURITY
861100     THRU    750-END.
861600
861700     IF (ERROR-FOUND)
861800         GO TO 725-END.
861900
           INITIALIZE                         PADFP-FIELDS.

862300     PERFORM 775-LOAD-PADFP-FIELDS
862400     THRU    775-END.
862500
862600     PERFORM 2000-EDIT-DEFAULTS.
862700     IF (ERROR-FOUND)
862800         GO TO 725-END.
862900
480100     PERFORM 800-MOVE-WS-TO-SCR   
480200     THRU    800-END.

863300 725-END.
863400******************************************************************
863500 750-CHECK-SECURITY.
863600******************************************************************

           MOVE "PO"                       TO DB-TOPIC.
           PERFORM
               VARYING I9 FROM 1 BY 1
               UNTIL  (I9 > PAPOSEMP-TABLE-SIZE)
               OR     (ERROR-FOUND)

               MOVE PAPOSEMP-POS-FLD (I9)  TO DB-FLD-NBR
               INITIALIZE                     DB-COUNTRY-CD-REQ
               IF (PA52WS-USE-PROCESS-LEVEL)
                   MOVE POS-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
               ELSE
                   MOVE SPACES             TO DB-PROCESS-LEVEL
               END-IF
               PERFORM 840-FIND-PASSET2
               IF  (PASCRTY-FOUND)
               AND (PAS-DEFAULT-FLG > 1)
                   MOVE PAPOSEMP-EMP-FLD (I9)  TO DB-FLD-NBR
                   INITIALIZE                     DB-COUNTRY-CD-REQ
                   MOVE SPACES                 TO DB-PROCESS-LEVEL
                   PERFORM 840-FIND-PASSET1
                   IF  (PASCRTY-FOUND)
                       MOVE PAS-SEC-LEVEL          TO HRWS-SEC-LEVEL
                       PERFORM 730-HR-FIELD-SECURITY
                       IF  (HRWS-FLD-SECURED)
                           MOVE 107                    TO CRT-ERROR-NBR
                           MOVE PA52F5-FC-FN           TO CRT-FIELD-NBR.

       750-END.
865200******************************************************************
865300 775-LOAD-PADFP-FIELDS.
865400******************************************************************
865500
865600     MOVE PA52F5-FC-FN               TO PADFP-FC-FN.
865700     IF (PA52F5-POS-LEVEL = 1)
865800         MOVE WS-TRUE                TO PADFP-LVL1-CHG-SW
865900     ELSE
866000         MOVE WS-FALSE               TO PADFP-LVL1-CHG-SW.
866100     MOVE PA52F5-PCT-COMPANY         TO PADFP-COMPANY.
           MOVE PA52F5-PCT-MOVE-FROM-LEVEL TO PADFP-POS-LEVEL.
           MOVE PA52F5-PCT-EMPLOYEE        TO PADFP-EMPLOYEE.
866200     MOVE PA52F5-PCT-EFFECT-DATE     TO PADFP-EFFECT-DATE.
           IF  (PA52F5-PCT-NEW-POSITION    NOT = PA52F5-PCT-POSITION)
           AND (PA52F5-PCT-MOVE-FROM-LEVEL = ZEROES)
               MOVE PA52F5-PCT-NEW-POSITION
                                           TO PADFP-POSITION
           ELSE
           IF  (PA52F5-PCT-NEW-POSITION    = PA52F5-PCT-POSITION)
           AND (PA52F5-PCT-EFFECT-DATE     < PA52F5-PEP-EFFECT-DATE)
           AND (PA52F5-POS-EFF-DT      NOT = ZEROES)
               IF (PA52F5-PCT-MOVE-FROM-LEVEL = ZEROES)
866300             MOVE PA52F5-PCT-NEW-POSITION
866300                                     TO PADFP-POSITION
               ELSE
                   MOVE PA52F5-PCT-POSITION
                                           TO PADFP-POSITION
               END-IF
           ELSE
               MOVE PA52F5-PCT-NEW-POSITION TO PADFP-POSITION
           END-IF.
           MOVE PA52F5-PCT-NEW-POSITION-FN TO PADFP-POSITION-FN.

           IF (PA52F5-FC = "V")
P51466         MOVE PA52F5-PCT-NEW-DATE-ASSIGN
                                           TO PAPCT-FIELD
P61248         IF (PAPCT-FIELD             = "*BLANK")      
P61248             MOVE ZEROS              TO PAPCT-DATE-FORMAT  
P61248         ELSE
P61248             PERFORM 5300-CONVERT-DATE
P61248         END-IF
               MOVE PAPCT-DATE-FORMAT      TO PADFP-ASSIGN-DATE
P51466         MOVE PA52F5-PCT-NEW-DATE-ASSIGN-FN
                                           TO PADFP-ASSIGN-DATE-FN
           END-IF.

866500     MOVE PA52F5-PCT-JOBCODE         TO PADFP-JOB-CODE.
866600     MOVE PA52F5-PCT-NEW-JOBCODE-FN  TO PADFP-JOB-CODE-FN.
866700     MOVE PA52F5-PCT-PL              TO PADFP-PROCESS-LEVEL.
866800     MOVE PA52F5-PCT-NEW-PL-FN       TO PADFP-PROCESS-LEVEL-FN.
           MOVE PA52F5-PCT-CURRENCY-CODE   TO PADFP-CURRENCY-CODE.
866900     MOVE PA52F5-PCT-DEPARTMENT      TO PADFP-DEPARTMENT.
867000     MOVE PA52F5-PCT-NEW-DEPARTMENT-FN
867100                                     TO PADFP-DEPARTMENT-FN.
867200     MOVE PA52F5-PCT-USER-LEVEL      TO PADFP-USER-LEVEL.
867300     MOVE PA52F5-PCT-NEW-USER-LEVEL-FN
867400                                     TO PADFP-USER-LEVEL-FN.
867500     MOVE PA52F5-PCT-SCHEDULE        TO PADFP-SCHEDULE.
867600     MOVE PA52F5-PCT-NEW-SCHEDULE-FN TO PADFP-SCHEDULE-FN.
867700     MOVE PA52F5-PCT-GRADE           TO PADFP-PAY-GRADE.
867800     MOVE PA52F5-PCT-NEW-GRADE-FN    TO PADFP-PAY-GRADE-FN.
867900     MOVE PA52F5-PCT-STEP            TO PAPCT-FIELD.
868000     PERFORM 5200-CONVERT-NUMERIC-FIELD.
868100     MOVE PAPCT-NUMERIC              TO PADFP-PAY-STEP.
868200     MOVE PA52F5-PCT-NEW-STEP-FN     TO PADFP-PAY-STEP-FN.
           MOVE PA52F5-PCT-UNION           TO PADFP-UNION-CODE.
           MOVE PA52F5-PCT-NEW-UNION-FN    TO PADFP-UNION-CODE-FN.
           MOVE PA52F5-PCT-BARGAIN-UNIT    TO PADFP-BARGAIN-UNIT.
           MOVE PA52F5-PCT-BARGAIN-UNIT-FN TO PADFP-BARGAIN-UNIT-FN.
868300     MOVE PA52F5-PCT-SUPERVISOR      TO PADFP-SUPERVISOR.
868400     MOVE PA52F5-PCT-NEW-SUPERVISOR-FN
868500                                     TO PADFP-SUPERVISOR-FN.
868600     MOVE PA52F5-PCT-IND-SUPER       TO PADFP-SUPERVISOR-IND.
868700     MOVE PA52F5-PCT-NEW-IND-SUPER-FN
868800                                     TO PADFP-SUPERVISOR-IND-FN.
868900     MOVE PA52F5-PCT-SHIFT           TO PAPCT-FIELD.
869000     PERFORM 5200-CONVERT-NUMERIC-FIELD.
869100     MOVE PAPCT-NUMERIC              TO PADFP-SHIFT.
869200     MOVE PA52F5-PCT-NEW-SHIFT-FN    TO PADFP-SHIFT-FN.
869300     MOVE PA52F5-PCT-WORK-SCHD       TO PADFP-WORK-SCHED.
869400     MOVE PA52F5-PCT-NEW-WORK-SCHD-FN
869500                                     TO PADFP-WORK-SCHED-FN.
869600     MOVE PA52F5-PCT-LOCATION        TO PADFP-LOCAT-CODE.
869700     MOVE PA52F5-PCT-NEW-LOCATION-FN TO PADFP-LOCAT-CODE-FN.
869800     MOVE PA52F5-PCT-FTE             TO PAPCT-FIELD.
869900     PERFORM 5200-CONVERT-NUMERIC-FIELD.
870000     MOVE PAPCT-6-DECIMALS           TO PADFP-FTE.
870100     MOVE PA52F5-PCT-NEW-FTE-FN      TO PADFP-FTE-FN.
870200     MOVE PA52F5-PCT-PAY-RATE        TO PAPCT-FIELD.
870300     PERFORM 5200-CONVERT-NUMERIC-FIELD.
870400     MOVE PAPCT-4-DECIMALS           TO PADFP-PAY-RATE.
870500     MOVE PA52F5-PCT-NEW-PAY-RATE-FN TO PADFP-PAY-RATE-FN.
           MOVE PA52F5-PCT-ANNUAL-HOURS    TO PAPCT-FIELD.
           PERFORM 5200-CONVERT-NUMERIC-FIELD.
           MOVE PAPCT-NUMERIC              TO PADFP-ANNUAL-HOURS.
J46844     MOVE PAPCT-4-DECIMALS           TO PADFP-ANNUAL-HOURS.           
           MOVE PA52F5-PCT-NEW-ANNUAL-HOURS-FN
                                           TO PADFP-ANNUAL-HOURS-FN.
           MOVE PA52F5-PCT-EXP-COMPANY     TO PAPCT-FIELD.
           PERFORM 5200-CONVERT-NUMERIC-FIELD.
           MOVE PAPCT-NUMERIC              TO PADFP-EXP-COMPANY.
           MOVE PA52F5-PCT-NEW-EXP-COMPANY-FN
                                           TO PADFP-EXP-COMPANY-FN.
           MOVE PA52F5-PCT-ACCT-UNIT       TO PADFP-EXP-ACCT-UNIT.
           MOVE PA52F5-PCT-NEW-ACCT-UNIT-FN
                                           TO PADFP-EXP-ACCT-UNIT-FN.
           MOVE PA52F5-PCT-ACCOUNT         TO PAPCT-FIELD.
           PERFORM 5200-CONVERT-NUMERIC-FIELD.
           MOVE PAPCT-NUMERIC              TO PADFP-EXP-ACCOUNT.
           MOVE PA52F5-PCT-NEW-ACCOUNT-FN  TO PADFP-EXP-ACCOUNT-FN.
           MOVE PA52F5-PCT-SUBACCOUNT      TO PAPCT-FIELD.
           PERFORM 5200-CONVERT-NUMERIC-FIELD.
           MOVE PAPCT-NUMERIC              TO PADFP-EXP-SUB-ACCT.
           MOVE PA52F5-PCT-NEW-SUBACCOUNT-FN
                                           TO PADFP-EXP-SUB-ACCT-FN.
           MOVE PA52F5-PCT-ACTIVITY        TO PADFP-ACTIVITY.
           MOVE PA52F5-PCT-NEW-ACTIVITY-FN TO PADFP-ACTIVITY-FN.
           MOVE PA52F5-PCT-CATEGORY        TO PADFP-ACCT-CATEGORY.
           MOVE PA52F5-PCT-NEW-CATEGORY-FN TO PADFP-ACCT-CATEGORY-FN.
           MOVE PA52F5-PCT-USER-AMOUNT     TO PAPCT-FIELD.
           PERFORM 5200-CONVERT-NUMERIC-FIELD.
           MOVE PAPCT-4-DECIMALS           TO PADFP-USER-AMOUNT.
           MOVE PA52F5-PCT-USER-AMOUNT-FN  TO PADFP-USER-AMOUNT-FN.

870600     IF (PA52F5-POS-LEVEL       = 01)
870700         MOVE PA52F5-PCT-PAY-FREQ        TO PAPCT-FIELD
870800         PERFORM 5200-CONVERT-NUMERIC-FIELD
870900         MOVE PAPCT-NUMERIC              TO PADFP-PAY-FREQUENCY
871000         MOVE PA52F5-PCT-NEW-PAY-FREQ-FN TO PADFP-PAY-FREQUENCY-FN
871700         MOVE PA52F5-PCT-PAY-PLAN        TO PADFP-OT-PLAN-CODE
871800         MOVE PA52F5-PCT-NEW-PAY-PLAN-FN TO PADFP-OT-PLAN-CODE-FN
872300         MOVE PA52F5-PCT-SECURITY-LVL    TO PAPCT-FIELD
872500         PERFORM 5200-CONVERT-NUMERIC-FIELD
872600         MOVE PAPCT-NUMERIC              TO PADFP-SEC-LVL
872700         MOVE PA52F5-PCT-NEW-SECURITY-LVL-FN
872800                                         TO PADFP-SEC-LVL-FN
872900         MOVE PA52F5-PCT-SECURITY-LOC    TO PADFP-SEC-LOCATION
873100         MOVE PA52F5-PCT-NEW-SECURITY-LOC-FN
873200                                         TO PADFP-SEC-LOCATION-FN
873300         MOVE PA52F5-PCT-SALARY-CLASS    TO PADFP-SALARY-CLASS
873500         MOVE PA52F5-PCT-NEW-SALARY-CLASS-FN
873600                                         TO PADFP-SALARY-CLASS-FN
873700         MOVE PA52F5-PCT-EXEMPT          TO PADFP-EXEMPT-EMP
873800         MOVE PA52F5-PCT-NEW-EXEMPT-FN   TO PADFP-EXEMPT-EMP-FN
           ELSE
876200         IF  (PA52F5-PCT-NEW-END-DATE = "*BLANK")
876300         OR ((PA52F5-PCT-NEW-END-DATE = SPACES)
               AND (PA52F5-PCT-END-DATE     = SPACES))
                   INITIALIZE                   PADFP-END-DATE
               ELSE
               IF  (PA52F5-PCT-NEW-END-DATE = SPACES)
               AND (PA52F5-PCT-END-DATE     NOT = SPACES)
                   MOVE PA52F5-PCT-END-DATE TO PAPCT-FIELD
877400             PERFORM 5300-CONVERT-DATE
877500             MOVE PAPCT-DATE-FORMAT   TO PADFP-END-DATE
               ELSE
                   MOVE PA52F5-PCT-NEW-END-DATE
                                            TO PAPCT-FIELD
877400             PERFORM 5300-CONVERT-DATE
877500             MOVE PAPCT-DATE-FORMAT  TO PADFP-END-DATE.
879200
879300 775-END.
879400******************************************************************
879500 800-MOVE-WS-TO-SCR.
879600******************************************************************
879700
           IF (PA52F5-FC = "V")
               IF  (PADFP-ASSIGN-DATE      = ZEROES)
P51466         AND (PA52F5-PCT-DATE-ASSIGN NOT = SPACES)
P51466             MOVE "*BLANK"       TO PA52F5-PCT-NEW-DATE-ASSIGN
               ELSE
               IF (PADFP-ASSIGN-DATE NOT = ZEROES)
                   MOVE PADFP-ASSIGN-DATE
                                       TO HRWS-DATE-8-FIELD
                   INITIALIZE             HRWS-DATE-FIELD
                   PERFORM 781-HR-FORMAT-DATE-FIELD
P51466             IF (HRWS-VALUE NOT = PA52F5-PCT-DATE-ASSIGN)
P51466                 MOVE HRWS-VALUE TO PA52F5-PCT-NEW-DATE-ASSIGN
                   ELSE
P51466                 MOVE SPACES     TO PA52F5-PCT-NEW-DATE-ASSIGN
                   END-IF
               END-IF.

           IF  (PADFP-UNION-CODE = SPACES)
           AND (PA52F5-PCT-UNION NOT = SPACES)
               MOVE "*BLANK"           TO PA52F5-PCT-NEW-UNION
           ELSE
           IF (PADFP-UNION-CODE   NOT = PA52F5-PCT-UNION)
               MOVE PADFP-UNION-CODE   TO PA52F5-PCT-NEW-UNION
           ELSE
               MOVE SPACES             TO PA52F5-PCT-NEW-UNION.

           IF  (PADFP-BARGAIN-UNIT      = SPACES)
           AND (PA52F5-PCT-BARGAIN-UNIT NOT = SPACES)
               MOVE "*BLANK"           TO PA52F5-PCT-NEW-BARGAIN-UNIT
           ELSE
           IF (PADFP-BARGAIN-UNIT NOT = PA52F5-PCT-BARGAIN-UNIT)
880100         MOVE PADFP-BARGAIN-UNIT TO PA52F5-PCT-NEW-BARGAIN-UNIT
           ELSE
               MOVE SPACES             TO PA52F5-PCT-NEW-BARGAIN-UNIT.

           IF (PA52F5-PCT-MOVE-FROM-LEVEL NOT = ZEROES)
               IF  (PADFP-POSITION     = SPACES)
               AND (PA52F5-PCT-POSITION NOT = SPACES)
                   MOVE "*BLANK"           TO PA52F5-PCT-NEW-POSITION
               ELSE
               IF (PADFP-POSITION NOT = PA52F5-PCT-POSITION)
880100             MOVE PADFP-POSITION     TO PA52F5-PCT-NEW-POSITION
               ELSE
                   MOVE SPACES             TO PA52F5-PCT-NEW-POSITION.

           IF  (PADFP-JOB-CODE     = SPACES)
           AND (PA52F5-PCT-JOBCODE NOT = SPACES)
               MOVE "*BLANK"               TO PA52F5-PCT-NEW-JOBCODE
           ELSE
           IF (PADFP-JOB-CODE NOT = PA52F5-PCT-JOBCODE)
880100         MOVE PADFP-JOB-CODE     TO PA52F5-PCT-NEW-JOBCODE
           ELSE
               MOVE SPACES             TO PA52F5-PCT-NEW-JOBCODE.

           IF  (PADFP-PROCESS-LEVEL = SPACES)
           AND (PA52F5-PCT-PL       NOT = SPACES)
               MOVE "*BLANK"               TO PA52F5-PCT-NEW-PL
           ELSE
           IF (PADFP-PROCESS-LEVEL NOT = PA52F5-PCT-PL)
880200         MOVE PADFP-PROCESS-LEVEL    TO PA52F5-PCT-NEW-PL
           ELSE
               MOVE SPACES                 TO PA52F5-PCT-NEW-PL.

           IF  (PADFP-CURRENCY-CODE NOT = SPACES)
               IF (PADFP-CURRENCY-CODE NOT = PA52F5-PCT-CURRENCY-CODE)
                   MOVE PADFP-CURRENCY-CODE
                                           TO PA52F5-PCT-NEW-CURRENCY
               ELSE
                   MOVE SPACES             TO PA52F5-PCT-NEW-CURRENCY.

           IF  (PADFP-DEPARTMENT      = SPACES)
           AND (PA52F5-PCT-DEPARTMENT NOT = SPACES)
               MOVE "*BLANK"               TO PA52F5-PCT-NEW-DEPARTMENT
           ELSE
           IF (PADFP-DEPARTMENT NOT = PA52F5-PCT-DEPARTMENT)
880300         MOVE PADFP-DEPARTMENT       TO PA52F5-PCT-NEW-DEPARTMENT
           ELSE
               MOVE SPACES                 TO PA52F5-PCT-NEW-DEPARTMENT.

           IF  (PADFP-USER-LEVEL      = SPACES)
           AND (PA52F5-PCT-USER-LEVEL NOT = SPACES)
               MOVE "*BLANK"               TO PA52F5-PCT-NEW-USER-LEVEL
           ELSE
           IF (PADFP-USER-LEVEL NOT = PA52F5-PCT-USER-LEVEL)
880400         MOVE PADFP-USER-LEVEL       TO PA52F5-PCT-NEW-USER-LEVEL
           ELSE
               MOVE SPACES                 TO PA52F5-PCT-NEW-USER-LEVEL.

           IF  (PADFP-SCHEDULE      = SPACES)
           AND (PA52F5-PCT-SCHEDULE NOT = SPACES)
               MOVE "*BLANK"               TO PA52F5-PCT-NEW-SCHEDULE
           ELSE
           IF (PADFP-SCHEDULE NOT = PA52F5-PCT-SCHEDULE)
880500         MOVE PADFP-SCHEDULE         TO PA52F5-PCT-NEW-SCHEDULE
           ELSE
               MOVE SPACES                 TO PA52F5-PCT-NEW-SCHEDULE.

           IF  (PADFP-PAY-STEP  = ZEROES)
           AND (PA52F5-PCT-STEP NOT = SPACES)
               MOVE "*BLANK"               TO PA52F5-PCT-NEW-STEP
           ELSE
           IF (PADFP-PAY-STEP NOT = ZEROES)
880600         MOVE PADFP-PAY-STEP         TO HRWS-DEC-FIELD
880700         MOVE 0                      TO HRWS-SIZE
880800         MOVE 0                      TO HRWS-NBR-DECIMALS
880900         PERFORM 770-HR-FORMAT-DEC-FIELD
               IF (HRWS-VALUE NOT = PA52F5-PCT-STEP)
881000             MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-STEP
               ELSE
                   MOVE SPACES             TO PA52F5-PCT-NEW-STEP.

           IF  (PADFP-PAY-GRADE  = SPACES)
           AND (PA52F5-PCT-GRADE NOT = SPACES)
               MOVE "*BLANK"               TO PA52F5-PCT-NEW-GRADE
           ELSE
           IF (PADFP-PAY-GRADE NOT = PA52F5-PCT-GRADE)
881100         MOVE PADFP-PAY-GRADE        TO PA52F5-PCT-NEW-GRADE
           ELSE
               MOVE SPACES                 TO PA52F5-PCT-NEW-GRADE.

           IF  (PADFP-SUPERVISOR      = SPACES)
           AND (PA52F5-PCT-SUPERVISOR NOT = SPACES)
               MOVE "*BLANK"               TO PA52F5-PCT-NEW-SUPERVISOR
           ELSE
           IF (PADFP-SUPERVISOR NOT = PA52F5-PCT-SUPERVISOR)
881200         MOVE PADFP-SUPERVISOR       TO PA52F5-PCT-NEW-SUPERVISOR
           ELSE
               MOVE SPACES                 TO PA52F5-PCT-NEW-SUPERVISOR.

           IF  (PADFP-SUPERVISOR-IND = SPACES)
           AND (PA52F5-PCT-IND-SUPER NOT = SPACES)
               MOVE "*BLANK"               TO PA52F5-PCT-NEW-IND-SUPER
           ELSE
           IF (PADFP-SUPERVISOR-IND NOT = PA52F5-PCT-IND-SUPER)
881300         MOVE PADFP-SUPERVISOR-IND   TO PA52F5-PCT-NEW-IND-SUPER
           ELSE
               MOVE SPACES                 TO PA52F5-PCT-NEW-IND-SUPER.

           IF  (PADFP-SHIFT      = ZEROES)
           AND (PA52F5-PCT-SHIFT NOT = SPACES)
               MOVE "*BLANK"               TO PA52F5-PCT-NEW-SHIFT
           ELSE
           IF (PADFP-SHIFT NOT = ZEROES)
881400         MOVE PADFP-SHIFT            TO HRWS-DEC-FIELD
881500         MOVE 0                      TO HRWS-SIZE
881600         MOVE 0                      TO HRWS-NBR-DECIMALS
881700         PERFORM 770-HR-FORMAT-DEC-FIELD
               IF (HRWS-VALUE NOT = PA52F5-PCT-SHIFT)
881800             MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-SHIFT
               ELSE
                   MOVE SPACES             TO PA52F5-PCT-NEW-SHIFT.

           IF  (PADFP-LOCAT-CODE    = SPACES)
           AND (PA52F5-PCT-LOCATION NOT = SPACES)
               MOVE "*BLANK"               TO PA52F5-PCT-NEW-LOCATION
           ELSE
           IF (PADFP-LOCAT-CODE NOT = PA52F5-PCT-LOCATION)
881900         MOVE PADFP-LOCAT-CODE       TO PA52F5-PCT-NEW-LOCATION
           ELSE
               MOVE SPACES                 TO PA52F5-PCT-NEW-LOCATION.

      * FTE is not a field that is defaulted from the position
           IF  (PADFP-FTE      = ZEROES)
           AND (PA52F5-PCT-FTE NOT = SPACES)
           AND (PA52F5-PCT-MOVE-FROM-LEVEL NOT = ZEROES)
                   MOVE "*BLANK"           TO PA52F5-PCT-NEW-FTE
           ELSE
           IF (PADFP-FTE NOT = ZEROES)
882000         MOVE PADFP-FTE              TO HRWS-DEC-FIELD
882100         MOVE 0                      TO HRWS-SIZE
882200         MOVE 6                      TO HRWS-NBR-DECIMALS
882300         PERFORM 770-HR-FORMAT-DEC-FIELD
               IF (HRWS-VALUE     NOT = PA52F5-PCT-FTE)
882400             MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-FTE
               ELSE
                   MOVE SPACES             TO PA52F5-PCT-NEW-FTE.

           IF  (PADFP-PAY-RATE      = ZEROES)
           AND (PA52F5-PCT-PAY-RATE NOT = SPACES)
               MOVE "*BLANK"               TO PA52F5-PCT-NEW-PAY-RATE
           ELSE
           IF (PADFP-PAY-RATE NOT = ZEROES)
882500         MOVE PADFP-PAY-RATE         TO HRWS-DEC-FIELD
882600         MOVE 0                      TO HRWS-SIZE
882700         MOVE 4                      TO HRWS-NBR-DECIMALS
882800         PERFORM 770-HR-FORMAT-DEC-FIELD
               IF (HRWS-VALUE NOT = PA52F5-PCT-PAY-RATE)
882900             MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-PAY-RATE
               ELSE
                   MOVE SPACES             TO PA52F5-PCT-NEW-PAY-RATE.

           IF  (PADFP-WORK-SCHED     = SPACES)
           AND (PA52F5-PCT-WORK-SCHD NOT = SPACES)
               MOVE "*BLANK"               TO PA52F5-PCT-NEW-WORK-SCHD
           ELSE
           IF (PADFP-WORK-SCHED NOT = PA52F5-PCT-WORK-SCHD)
883000         MOVE PADFP-WORK-SCHED       TO PA52F5-PCT-NEW-WORK-SCHD
           ELSE
               MOVE SPACES                 TO PA52F5-PCT-NEW-WORK-SCHD.
883100
           IF  (PADFP-ANNUAL-HOURS      = ZEROES)
           AND (PA52F5-PCT-ANNUAL-HOURS NOT = SPACES)
               MOVE "*BLANK"           TO
                                       PA52F5-PCT-NEW-ANNUAL-HOURS
           ELSE
           IF (PADFP-ANNUAL-HOURS NOT = ZEROES)
               MOVE PADFP-ANNUAL-HOURS TO HRWS-DEC-FIELD
               MOVE 0                  TO HRWS-SIZE
J46844         MOVE 4                  TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               IF (HRWS-VALUE NOT = PA52F5-PCT-ANNUAL-HOURS)
                   MOVE HRWS-VALUE     TO
                                       PA52F5-PCT-NEW-ANNUAL-HOURS
               ELSE
                   MOVE SPACES         TO PA52F5-PCT-NEW-ANNUAL-HOURS
               END-IF
           END-IF
           END-IF.

           IF  (PADFP-EXP-COMPANY      = ZEROES)
           AND (PA52F5-PCT-EXP-COMPANY NOT = SPACES)
               MOVE "*BLANK"           TO
                                       PA52F5-PCT-NEW-EXP-COMPANY
           ELSE
           IF (PADFP-EXP-COMPANY NOT = ZEROES)
               MOVE PADFP-EXP-COMPANY  TO HRWS-DEC-FIELD
               MOVE 0                  TO HRWS-SIZE
               MOVE 0                  TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               IF (HRWS-VALUE NOT = PA52F5-PCT-EXP-COMPANY)
                   MOVE HRWS-VALUE     TO PA52F5-PCT-NEW-EXP-COMPANY
               ELSE
                   MOVE SPACES         TO PA52F5-PCT-NEW-EXP-COMPANY
               END-IF
           END-IF
           END-IF.

           IF  (PADFP-EXP-ACCT-UNIT  = SPACES)
           AND (PA52F5-PCT-ACCT-UNIT NOT = SPACES)
               MOVE "*BLANK"           TO
                                       PA52F5-PCT-NEW-ACCT-UNIT
           ELSE
           IF (PADFP-EXP-ACCT-UNIT NOT = PA52F5-PCT-ACCT-UNIT)
               MOVE PADFP-EXP-ACCT-UNIT
                                       TO PA52F5-PCT-NEW-ACCT-UNIT
           ELSE
               MOVE SPACES             TO PA52F5-PCT-NEW-ACCT-UNIT
           END-IF
           END-IF.

           IF  (PADFP-EXP-ACCOUNT  = ZEROES)
           AND (PA52F5-PCT-ACCOUNT NOT = SPACES)
               MOVE "*BLANK"           TO
                                       PA52F5-PCT-NEW-ACCOUNT
           ELSE
           IF (PADFP-EXP-ACCOUNT NOT = ZEROES)
               MOVE PADFP-EXP-ACCOUNT  TO HRWS-DEC-FIELD
               MOVE 0                  TO HRWS-SIZE
               MOVE 0                  TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               IF (HRWS-VALUE NOT = PA52F5-PCT-ACCOUNT)
                   MOVE HRWS-VALUE     TO PA52F5-PCT-NEW-ACCOUNT
               ELSE
                   MOVE SPACES         TO PA52F5-PCT-NEW-ACCOUNT
               END-IF
           END-IF
           END-IF.

           IF  (PADFP-EXP-SUB-ACCT    = ZEROES)
           AND (PA52F5-PCT-SUBACCOUNT NOT = SPACES)
               MOVE "*BLANK"           TO
                                       PA52F5-PCT-NEW-SUBACCOUNT
           ELSE
           IF (PADFP-EXP-SUB-ACCT NOT = ZEROES)
               MOVE PADFP-EXP-SUB-ACCT TO HRWS-DEC-FIELD
               MOVE 0                  TO HRWS-SIZE
               MOVE 0                  TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               IF (HRWS-VALUE NOT = PA52F5-PCT-SUBACCOUNT)
                   MOVE HRWS-VALUE     TO PA52F5-PCT-NEW-SUBACCOUNT
               ELSE
                   MOVE SPACES         TO PA52F5-PCT-NEW-SUBACCOUNT
               END-IF
           END-IF
           END-IF.

           IF  (PADFP-ACTIVITY      = SPACES)
           AND (PA52F5-PCT-ACTIVITY NOT = SPACES)
               MOVE "*BLANK"           TO
                                       PA52F5-PCT-NEW-ACTIVITY
           ELSE
           IF (PADFP-ACTIVITY NOT = PA52F5-PCT-ACTIVITY)
               MOVE PADFP-ACTIVITY     TO PA52F5-PCT-NEW-ACTIVITY
           ELSE
               MOVE SPACES             TO PA52F5-PCT-NEW-ACTIVITY
           END-IF
           END-IF.

           IF  (PADFP-ACCT-CATEGORY = SPACES)
           AND (PA52F5-PCT-CATEGORY NOT = SPACES)
               MOVE "*BLANK"           TO
                                       PA52F5-PCT-NEW-CATEGORY
           ELSE
           IF (PADFP-ACCT-CATEGORY NOT = PA52F5-PCT-CATEGORY)
               MOVE PADFP-ACCT-CATEGORY
                                       TO PA52F5-PCT-NEW-CATEGORY
           ELSE
               MOVE SPACES             TO PA52F5-PCT-NEW-CATEGORY.

883200     IF (PA52F5-POS-LEVEL = 01)
               IF  (PADFP-OT-PLAN-CODE = SPACES)
               AND (PA52F5-PCT-PAY-PLAN NOT = SPACES)
                   MOVE "*BLANK"           TO
                                           PA52F5-PCT-NEW-PAY-PLAN
               ELSE
               IF (PADFP-OT-PLAN-CODE NOT = PA52F5-PCT-PAY-PLAN)
883900             MOVE PADFP-OT-PLAN-CODE TO PA52F5-PCT-NEW-PAY-PLAN
               ELSE
                   MOVE SPACES             TO PA52F5-PCT-NEW-PAY-PLAN
               END-IF
               END-IF
               IF  (PADFP-PAY-FREQUENCY = ZEROES)
               AND (PA52F5-PCT-PAY-FREQ NOT = SPACES)
                   MOVE "*BLANK"           TO
                                           PA52F5-PCT-NEW-PAY-FREQ
               ELSE
               IF (PADFP-PAY-FREQUENCY NOT = ZEROES)
884000             MOVE PADFP-PAY-FREQUENCY
884000                                     TO HRWS-DEC-FIELD
884100             MOVE 0                  TO HRWS-SIZE
884200             MOVE 0                  TO HRWS-NBR-DECIMALS
884300             PERFORM 770-HR-FORMAT-DEC-FIELD
                   IF (HRWS-VALUE NOT = PA52F5-PCT-PAY-FREQ)
884400                 MOVE HRWS-VALUE     TO PA52F5-PCT-NEW-PAY-FREQ
                   ELSE
                       MOVE SPACES         TO PA52F5-PCT-NEW-PAY-FREQ
                   END-IF
               END-IF
               END-IF
               IF  (PADFP-SEC-LVL           = ZEROES)
               AND (PA52F5-PCT-SECURITY-LVL NOT = SPACES)
                   MOVE "*BLANK"           TO
                                           PA52F5-PCT-NEW-SECURITY-LVL
               ELSE
               IF (PADFP-SEC-LVL NOT = ZEROES)
884500             MOVE PADFP-SEC-LVL      TO HRWS-DEC-FIELD
884600             MOVE 0                  TO HRWS-SIZE
884700             MOVE 0                  TO HRWS-NBR-DECIMALS
884800             PERFORM 770-HR-FORMAT-DEC-FIELD
                   IF (HRWS-VALUE NOT = PA52F5-PCT-SECURITY-LVL)
884900                 MOVE HRWS-VALUE     TO
885000                                     PA52F5-PCT-NEW-SECURITY-LVL
                   ELSE
                       MOVE SPACES         TO
                                           PA52F5-PCT-NEW-SECURITY-LVL
                   END-IF
               END-IF
               END-IF
               IF  (PADFP-SEC-LOCATION      = SPACES)
               AND (PA52F5-PCT-SECURITY-LOC NOT = SPACES)
                   MOVE "*BLANK"           TO
                                           PA52F5-PCT-NEW-SECURITY-LOC
               ELSE
               IF (PADFP-SEC-LOCATION NOT = PA52F5-PCT-SECURITY-LOC)
885100             MOVE PADFP-SEC-LOCATION TO
885200                                     PA52F5-PCT-NEW-SECURITY-LOC
               ELSE
                   MOVE SPACES             TO
                                           PA52F5-PCT-NEW-SECURITY-LOC
               END-IF
               END-IF
               IF  (PADFP-SALARY-CLASS      = SPACES)
               AND (PA52F5-PCT-SALARY-CLASS NOT = SPACES)
                   MOVE "*BLANK"           TO
                                           PA52F5-PCT-NEW-SALARY-CLASS
               ELSE
               IF (PADFP-SALARY-CLASS NOT = PA52F5-PCT-SALARY-CLASS)
885300             MOVE PADFP-SALARY-CLASS TO
885400                                     PA52F5-PCT-NEW-SALARY-CLASS
               ELSE
                   MOVE SPACES             TO
                                           PA52F5-PCT-NEW-SALARY-CLASS
               END-IF
               END-IF
               IF  (PADFP-EXEMPT-EMP  = SPACES)
               AND (PA52F5-PCT-EXEMPT NOT = SPACES)
                   MOVE "*BLANK"           TO
                                           PA52F5-PCT-NEW-EXEMPT
               ELSE
               IF (PADFP-EXEMPT-EMP NOT = PA52F5-PCT-EXEMPT)
885500             MOVE PADFP-EXEMPT-EMP   TO PA52F5-PCT-NEW-EXEMPT
               ELSE
                   MOVE SPACES             TO PA52F5-PCT-NEW-EXEMPT
               END-IF
               END-IF.
887400
           IF  (PADFP-USER-AMOUNT  = ZEROES)
           AND (PA52F5-PCT-USER-AMOUNT NOT = SPACES)
               MOVE "*BLANK"               TO PA52F5-PCT-NEW-USER-AMOUNT
           ELSE
           IF (PADFP-USER-AMOUNT NOT = ZEROES)
               MOVE PADFP-USER-AMOUNT      TO HRWS-DEC-FIELD
               MOVE 0                      TO HRWS-SIZE
               MOVE 4                      TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               IF (HRWS-VALUE NOT = PA52F5-PCT-USER-AMOUNT)
                   MOVE HRWS-VALUE         TO PA52F5-PCT-NEW-USER-AMOUNT
               ELSE
                   MOVE SPACES             TO
                                           PA52F5-PCT-NEW-USER-AMOUNT.

887500 800-END.
887600
887700******************************************************************
887800 825-GET-DESCRIPTIONS.
887900******************************************************************
888000
888100     IF  (PA52F5-PCT-NEW-POSITION NOT = SPACES)
           AND (PA52F5-PCT-NEW-POSITION NOT = "*BLANK")
               MOVE PA52F5-PCT-COMPANY      TO DB-COMPANY
               MOVE PA52F5-PCT-NEW-POSITION TO DB-POSITION
P60833         MOVE PA52F5-PCT-EFFECT-DATE  TO DB-EFFECT-DATE
               PERFORM 850-FIND-NLT-POSSET2
               IF  (PAPOSITION-FOUND)
               AND (POS-COMPANY  = DB-COMPANY)
               AND (POS-POSITION = DB-POSITION)
888200             MOVE POS-DESCRIPTION     TO PA52F5-POSITION-DESC.
888700                                      
888100     IF  (PA52F5-PCT-NEW-JOBCODE NOT = SPACES)
           AND (PA52F5-PCT-NEW-JOBCODE NOT = "*BLANK")
888200         MOVE PA52F5-PCT-COMPANY      TO DB-COMPANY
888300         MOVE PA52F5-PCT-NEW-JOBCODE  TO DB-JOB-CODE
888400         PERFORM 840-FIND-JBCSET1
888500         IF (JOBCODE-FOUND)
888600             MOVE JBC-DESCRIPTION     TO PA52F5-JOBCODE-DESC.
888700
888800     IF  (PA52F5-PCT-NEW-PL        NOT = SPACES)
888800     AND (PA52F5-PCT-NEW-PL        NOT = "*BLANK")
888900         MOVE PA52F5-PCT-NEW-PL       TO DB-PROCESS-LEVEL
889000         PERFORM 840-FIND-PRSSET1
889100         IF (PRSYSTEM-FOUND)
889200             MOVE PRS-NAME            TO PA52F5-PL-DESC.
889300
889400     IF  (PA52F5-PCT-NEW-DEPARTMENT NOT = SPACES)
889400     AND (PA52F5-PCT-NEW-DEPARTMENT NOT = "*BLANK")
889500         MOVE PA52F5-PCT-COMPANY      TO DB-COMPANY
               IF (PA52F5-PCT-NEW-PL = SPACES)
               OR (PA52F5-PCT-NEW-PL = "*BLANK")
                   MOVE PA52F5-PCT-PL       TO DB-PROCESS-LEVEL
               ELSE
889600             MOVE PA52F5-PCT-NEW-PL   TO DB-PROCESS-LEVEL
               END-IF
889700         MOVE PA52F5-PCT-NEW-DEPARTMENT
889800                                      TO DB-DEPARTMENT
889900         PERFORM 840-FIND-DPTSET1
890000         IF (DEPTCODE-FOUND)
890100             MOVE DPT-NAME            TO
890200                                      PA52F5-DEPARTMENT-DESC
P51834         ELSE 
P51834             INITIALIZE               DEPTCODE
P51834         END-IF 
P51834     END-IF.
890300
           IF (PA52F5-PCT-NEW-DEPARTMENT = SPACES)
           OR (PA52F5-PCT-NEW-DEPARTMENT = "*BLANK")
               IF (PA52F5-PCT-DEPARTMENT NOT = SPACES)
                   MOVE PA52F5-PCT-COMPANY  TO DB-COMPANY
                   IF (PA52F5-PCT-NEW-PL = SPACES)
                   OR (PA52F5-PCT-NEW-PL = "*BLANK")
                       MOVE PA52F5-PCT-PL   TO DB-PROCESS-LEVEL
                   ELSE
                       MOVE PA52F5-PCT-NEW-PL
                                            TO DB-PROCESS-LEVEL
                   END-IF
                   MOVE PA52F5-PCT-DEPARTMENT
                                            TO DB-DEPARTMENT
                   PERFORM 840-FIND-DPTSET1
               ELSE
                   INITIALIZE         DPT-DEP-DIST-CO
                                      DPT-DEP-ACCT-UNIT
                                      DPT-DEP-ACCOUNT
                                      DPT-DEP-SUB-ACCT
               END-IF.

890400     IF  (PA52F5-PCT-NEW-LOCATION NOT = SPACES)
890400     AND (PA52F5-PCT-NEW-LOCATION NOT = "*BLANK")
890500         MOVE "UL"                    TO DB-TYPE
890600         MOVE PA52F5-PCT-NEW-LOCATION TO DB-CODE
890700         PERFORM 840-FIND-PCOSET1
890800         IF (PCODES-FOUND)
890900             MOVE PCO-DESCRIPTION     TO PA52F5-LOCATION-DESC.
891000
891100     IF  (PA52F5-PCT-NEW-SUPERVISOR NOT = SPACES)
891100     AND (PA52F5-PCT-NEW-SUPERVISOR NOT = "*BLANK")
891200         MOVE PA52F5-PCT-COMPANY         TO DB-COMPANY
891300         MOVE PA52F5-PCT-NEW-SUPERVISOR  TO DB-CODE
891400         PERFORM 840-FIND-HSUSET1
891500         IF  (HRSUPER-FOUND)
891600         AND (HSU-EMPLOYEE NOT = ZEROS)
891700             MOVE HSU-EMPLOYEE        TO DB-EMPLOYEE
891800             MOVE ZEROS               TO DB-FLD-NBR
891900             PERFORM 850-FIND-NLT-PTFSET1
892000             MOVE PTF-LAST-NAME       TO HRWS-LAST-NAME
892100             MOVE PTF-FIRST-NAME      TO HRWS-FIRST-NAME
892200             MOVE PTF-MIDDLE-INIT     TO HRWS-MIDDLE-INIT
892300             PERFORM 750-HR-FORMAT-NAME
892400             MOVE HRWS-FORMAT-NAME    TO
892500                                      PA52F5-SUPERVISOR-DESC.
892600
892700     IF  (PA52F5-PCT-NEW-IND-SUPER NOT = SPACES)
892700     AND (PA52F5-PCT-NEW-IND-SUPER NOT = "*BLANK")
892800         MOVE PA52F5-PCT-COMPANY             TO DB-COMPANY
892900         MOVE PA52F5-PCT-NEW-IND-SUPER       TO DB-CODE
893000         PERFORM 840-FIND-HSUSET1
893100         IF  (HRSUPER-FOUND)
893200         AND (HSU-EMPLOYEE NOT = ZEROS)
893300             MOVE HSU-EMPLOYEE        TO DB-EMPLOYEE
893400             MOVE ZEROS               TO DB-FLD-NBR
893500             PERFORM 850-FIND-NLT-PTFSET1
893600             MOVE PTF-LAST-NAME       TO HRWS-LAST-NAME
893700             MOVE PTF-FIRST-NAME      TO HRWS-FIRST-NAME
893800             MOVE PTF-MIDDLE-INIT     TO HRWS-MIDDLE-INIT
893900             PERFORM 750-HR-FORMAT-NAME
894000             MOVE HRWS-FORMAT-NAME    TO
894100                                      PA52F5-IND-SUPER-DESC.
894200
894300     IF  (PA52F5-PCT-NEW-LOCATION NOT = SPACES)
894300     AND (PA52F5-PCT-NEW-LOCATION NOT = "*BLANK")
894400         MOVE "LO"                    TO DB-TYPE
894500         MOVE PA52F5-PCT-NEW-LOCATION TO DB-CODE
894600         PERFORM 840-FIND-PCOSET1
894700             IF (PCODES-FOUND)
894800                 MOVE PCO-DESCRIPTION TO PA52F5-LOCATION-DESC.
894900
895000     IF  (PA52F5-PCT-NEW-WORK-SCHD NOT = SPACES)
895000     AND (PA52F5-PCT-NEW-WORK-SCHD NOT = "*BLANK")
895100         MOVE "WS"                        TO DB-TYPE
895200         MOVE PA52F5-PCT-NEW-WORK-SCHD    TO DB-CODE
895300         PERFORM 840-FIND-PCOSET1
895400         IF (PCODES-FOUND)
895500             MOVE PCO-DESCRIPTION      TO PA52F5-WORK-SCHD-DESC.
895600
895700     IF  (PA52F5-PCT-NEW-SCHEDULE  = SPACES)
895700     AND (PA52F5-PCT-NEW-SCHEDULE  = "*BLANK")
895800     AND (PA52F5-PCT-NEW-STEP      = SPACES)
895800     AND (PA52F5-PCT-NEW-STEP      = "*BLANK")
895900         MOVE PA52F5-PCT-COMPANY       TO DB-COMPANY
896000         MOVE "G"                      TO DB-INDICATOR
896100         MOVE PA52F5-PCT-NEW-SCHEDULE  TO DB-SCHEDULE
896200         MOVE PA52F5-PCT-EFFECT-DATE   TO DB-EFFECT-DATE
896300         PERFORM 850-FIND-NLT-SGHSET2
896400         IF  (PRSAGHEAD-FOUND)
896500         AND (SGH-COMPANY   = DB-COMPANY)
896600         AND (SGH-INDICATOR = DB-INDICATOR)
896700         AND (SGH-SCHEDULE  = DB-SCHEDULE)
896800             MOVE SGH-DESCRIPTION      TO
896900                                       PA52F5-SCHEDULE-DESC.
897000
897100     IF  (PA52F5-PCT-NEW-SCHEDULE NOT = SPACES)
897100     AND (PA52F5-PCT-NEW-SCHEDULE NOT = "*BLANK")
897200     AND (PA52F5-PCT-NEW-STEP     NOT = SPACES)
897200     AND (PA52F5-PCT-NEW-STEP     NOT = "*BLANK")
897300         MOVE PA52F5-PCT-COMPANY       TO DB-COMPANY
897400         MOVE "S"                      TO DB-INDICATOR
897500         MOVE PA52F5-PCT-NEW-SCHEDULE  TO DB-SCHEDULE
897600         MOVE PA52F5-PCT-EFFECT-DATE   TO DB-EFFECT-DATE
897700         PERFORM 850-FIND-NLT-SGHSET2
897800         IF  (PRSAGHEAD-FOUND)
897900         AND (SGH-COMPANY   = DB-COMPANY)
898000         AND (SGH-INDICATOR = DB-INDICATOR)
898100         AND (SGH-SCHEDULE  = DB-SCHEDULE)
898200             MOVE SGH-DESCRIPTION      TO
898300                                       PA52F5-SCHEDULE-DESC.
898400
898500     IF  (PA52F5-PCT-NEW-USER-LEVEL NOT = SPACES)
898500     AND (PA52F5-PCT-NEW-USER-LEVEL NOT = "*BLANK")
898600         MOVE "UL"                        TO DB-TYPE
898700         MOVE PA52F5-PCT-NEW-USER-LEVEL   TO DB-CODE
898800         PERFORM 840-FIND-PCOSET1
898900         IF (PCODES-FOUND)
899000             MOVE PCO-DESCRIPTION   TO PA52F5-USER-LEVEL-DESC.
899100
899200     IF  (PA52F5-PCT-NEW-PAY-PLAN NOT = SPACES)
899200     AND (PA52F5-PCT-NEW-PAY-PLAN NOT = "*BLANK")
899300         MOVE PA52F5-PCT-COMPANY       TO DB-COMPANY
899400         MOVE PA52F5-PCT-NEW-PAY-PLAN  TO DB-PLAN-CODE
899500         MOVE PA52F5-PCT-EFFECT-DATE   TO DB-EFFECT-DATE
899600         PERFORM 850-FIND-NLT-PROSET2
899700         IF  (PROVERTIME-FOUND)
               AND (PRO-COMPANY   = DB-COMPANY)
               AND (PRO-PLAN-CODE = DB-PLAN-CODE)
899800             MOVE PRO-DESCRIPTION      TO
899900                                       PA52F5-PAY-PLAN-DESC
                   MOVE PRO-EFFECT-DATE      TO PA52F5-OT-EFFECT-DATE.
900000
           IF (PA52F5-PCT-NEW-EXP-COMPANY = SPACES)
               MOVE PA52F5-PCT-EXP-COMPANY     TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC              TO IFACWS-COMPANY
                                                  ACACWS-COMPANY
           ELSE
           IF (PA52F5-PCT-NEW-EXP-COMPANY NOT = "*BLANK")
               MOVE PA52F5-PCT-NEW-EXP-COMPANY TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC              TO IFACWS-COMPANY
                                                  ACACWS-COMPANY
           ELSE
               MOVE DPT-DEP-DIST-CO            TO IFACWS-COMPANY
                                                  ACACWS-COMPANY.

           IF (PA52F5-PCT-NEW-ACCT-UNIT   = SPACES)
               MOVE PA52F5-PCT-ACCT-UNIT       TO IFACWS-ACCT-UNIT
           ELSE
           IF (PA52F5-PCT-NEW-ACCT-UNIT   NOT = "*BLANK")
               MOVE PA52F5-PCT-NEW-ACCT-UNIT   TO IFACWS-ACCT-UNIT
           ELSE
               MOVE DPT-DEP-ACCT-UNIT          TO IFACWS-ACCT-UNIT.

           IF (PA52F5-PCT-NEW-ACCOUNT     = SPACES)
               MOVE PA52F5-PCT-ACCOUNT         TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC              TO IFACWS-ACCOUNT
           ELSE
           IF (PA52F5-PCT-NEW-ACCOUNT     NOT = "*BLANK")
               MOVE PA52F5-PCT-NEW-ACCOUNT     TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC              TO IFACWS-ACCOUNT
           ELSE
               MOVE DPT-DEP-ACCOUNT            TO IFACWS-ACCOUNT.

           IF (PA52F5-PCT-NEW-SUBACCOUNT  = SPACES)
               MOVE PA52F5-PCT-SUBACCOUNT      TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC              TO IFACWS-SUB-ACCOUNT
           ELSE
           IF (PA52F5-PCT-NEW-SUBACCOUNT  NOT = "*BLANK")
               MOVE PA52F5-PCT-NEW-SUBACCOUNT  TO PAPCT-FIELD
               PERFORM 5200-CONVERT-NUMERIC-FIELD
               MOVE PAPCT-NUMERIC              TO IFACWS-SUB-ACCOUNT
           ELSE
               MOVE DPT-DEP-SUB-ACCT           TO IFACWS-SUB-ACCOUNT.

901000     IF ( (PA52F5-PCT-NEW-ACCT-UNIT   NOT = SPACES)
901100      AND (PA52F5-PCT-NEW-ACCT-UNIT   NOT = "*BLANK"))
901200     OR ( (PA52F5-PCT-NEW-ACCOUNT     NOT = SPACES)
901300      AND (PA52F5-PCT-NEW-ACCOUNT     NOT = "*BLANK"))
901400     OR ( (PA52F5-PCT-NEW-SUBACCOUNT  NOT = SPACES)
901500      AND (PA52F5-PCT-NEW-SUBACCOUNT  NOT = "*BLANK"))
           OR ( (PA52F5-PCT-NEW-EXP-COMPANY NOT = SPACES)
            AND (PA52F5-PCT-NEW-EXP-COMPANY NOT = "*BLANK"))
902300         MOVE 4                        TO IFACWS-EDIT-TYPE
902500         PERFORM 635-EDIT-GLMASTER-60
902600         MOVE IFACWS-GLM-DESCRIPTION   TO PA52F5-ACCOUNT-DESC
902700     ELSE
902800         MOVE SPACES                   TO PA52F5-ACCOUNT-DESC.
902900
903000     IF  (PA52F5-PCT-NEW-ACTIVITY NOT = SPACES)
903000     AND (PA52F5-PCT-NEW-ACTIVITY NOT = "*BLANK")
903100         MOVE PA52F5-PCT-NEW-ACTIVITY  TO ACACWS-ACTIVITY
903200         MOVE 2                        TO ACACWS-EDIT-CODE
P68498         MOVE "N"                      TO ACACWS-CURRENCY-OR
P68498                                          ACACWS-RESOURCE-OR
P68498                                          ACACWS-BUDGET-OR
P68498                                          ACACWS-GL-OR
903300         PERFORM 640-EDIT-ACTIVITY-70
903400         MOVE ACACWS-ACV-DESCRIPTION   TO
903500                                        PA52F5-ACTIVITY-DESC.
903600
           IF  (PA52F5-PCT-NEW-CURRENCY NOT = SPACES)
               MOVE PA52F5-PCT-NEW-CURRENCY  TO DB-CURRENCY-CODE
               PERFORM 840-FIND-CUCSET1
               IF  (CUCODES-FOUND)
                   MOVE CUC-FORMS-EXP    TO PA52F5-CURRENCY-FORMS-EXP
               END-IF.

           IF (PA52F5-PCT-NEW-UNION NOT = SPACES)
               MOVE "UN"                      TO DB-TYPE
               MOVE PA52F5-PCT-NEW-UNION      TO DB-CODE
               PERFORM 840-FIND-PCOSET1
               IF (PCODES-FOUND)
                   MOVE PCO-DESCRIPTION TO PA52F5-UNION-DESC.

           IF  (PA52F5-PCT-NEW-BARGAIN-UNIT NOT = SPACES)
           AND (PA52F5-PCT-NEW-BARGAIN-UNIT NOT = "*BLANK")
               MOVE "BU"                        TO DB-TYPE
               MOVE PA52F5-PCT-NEW-BARGAIN-UNIT TO DB-CODE
               PERFORM 840-FIND-PCOSET1
               IF (PCODES-FOUND)
                   MOVE PCO-DESCRIPTION     TO PA52F5-BARGAIN-UNIT-DESC.
        
      
       825-END. 
       
      ******************************************************************
       PA52S5-TRANSACTION-END.
J40356******************************************************************
Begin  PA52S6-TRANSACTION SECTION 30.
      ******************************************************************
       PA52S6-START.

           IF (PA52F6-FC = SPACES)
               GO TO PA52S6-TRANSACTION-END.

           IF (PA52F6-FC = "I" OR "+" OR "-")
               PERFORM 210-EDIT-ACCESS
               THRU    210-END.

           IF (PA52F6-FC = "S" OR "X")
               PERFORM 220-EDIT-SELECT 
               THRU    220-END 
               IF ((PA52WS-NO-SELECT-ERROR)
               AND (PA52WS-LTM-SELECTED))
                  IF (PA52F6-FC = "X")
                      SET PA52WS-LTM-SELECT-CLOSE TO TRUE
                  ELSE
                      SET PA52WS-LTM-SELECT-OPEN  TO TRUE
                  END-IF
                  MOVE "I"                        TO CRT-PASS-FC
                                                     CRT-FC
                                                     CRT-DISPLAY-FC
                  MOVE CRT-EXEC-CALLER            TO CRT-REQUEST
                  MOVE "PA521"                    TO CRT-SCREEN-CODE 
               END-IF
           ELSE
               INITIALIZE PA52WS-LEM-FIELDS
               SET PA52WS-LTM-NOT-SELECTED        TO TRUE
           END-IF.

           GO TO PA52S6-TRANSACTION-END.
      
      ******************************************************************
       210-EDIT-ACCESS.   
      ******************************************************************

           IF (PA52F6-FC NOT = "I" AND "+" AND "-")
               GO TO 210-END.

           INITIALIZE PA52WS-LEM-FIELDS.

           IF (PA52F6-FC = "I")
           OR ((HREMPRCVR-NOTFOUND)
           AND (PA52F6-FC = "+"))
               MOVE PA52F6-COMPANY             TO DB-COMPANY
               MOVE PA52F6-EMPLOYEE            TO DB-EMPLOYEE
               MOVE SPACES                     TO DB-ERROR-FLAG
                                                  DB-ACTION-CODE
               MOVE ZEROES                     TO DB-LTM-DATE-STAMP 
                                                  DB-LTM-TIME-STAMP
                                                  DB-STATUS
                                                  DB-WORKASSIGNMENT

               PERFORM 850-FIND-NLT-LEMSET2
           END-IF. 
      
           MOVE PA52F6-COMPANY              TO DB-COMPANY.
           MOVE SPACES                      TO DB-PROCESS-LEVEL.
           PERFORM 840-FIND-PRSSET1.
           IF (PRSYSTEM-NOTFOUND)
               MOVE 100                     TO CRT-ERROR-NBR
               MOVE PA52F6-COMPANY-FN       TO CRT-FIELD-NBR
               GO TO 210-END.
      
           MOVE PA52F6-COMPANY              TO DB-COMPANY.
           MOVE PA52F6-EMPLOYEE             TO DB-EMPLOYEE.
           PERFORM 840-FIND-EMPSET1.
           IF (EMPLOYEE-NOTFOUND)
               MOVE 104                     TO CRT-ERROR-NBR
               MOVE PA52F6-EMPLOYEE-FN      TO CRT-FIELD-NBR
               GO TO 210-END.
       
           MOVE ZEROS                       TO HRWS-NEXT-COUNT.
           MOVE EMP-COMPANY                 TO CRT-COMPANY.
           MOVE EMP-PROCESS-LEVEL           TO CRT-PROCESS-LEVEL.  
           PERFORM 700-HR-EMP-SECURITY.  
           IF (HRWS-EMP-SECURED)
                   MOVE 121                 TO CRT-ERROR-NBR
                   MOVE PA52F6-EMPLOYEE-FN  TO CRT-FIELD-NBR
                   GO TO 210-END.

            PERFORM 480-INQUIRE 
            THRU    480-END.
      
       210-END.

      ******************************************************************
       220-EDIT-SELECT.
      ******************************************************************

           MOVE 1                  TO I1.
           INITIALIZE PA52WS-LEM-FIELDS.
           SET PA52WS-NO-SELECT-ERROR      TO TRUE.
           SET PA52WS-LTM-NOT-SELECTED     TO TRUE.
           PERFORM
              UNTIL (I1 > 11)
                     IF (PA52F6-LINE-FC (I1) NOT = SPACES)
                         IF (PA52WS-LTM-SELECTED)         
                             SET PA52WS-SELECT-ERROR    TO TRUE
                             MOVE 546                   TO CRT-ERROR-NBR
                             MOVE PA52F6-LINE-FC-FN(I1) TO CRT-FIELD-NBR
                             GO TO 220-END
                         ELSE
                             SET PA52WS-LTM-SELECTED    TO TRUE
                             MOVE PA52F6-COMPANY    TO PA52WS-L-COMPANY
                             MOVE PA52F6-EMPLOYEE   TO PA52WS-L-EMPLOYEE
                             MOVE PA52F6-STATUS(I1) TO PA52WS-L-STATUS
                             MOVE PA52F6-LTM-DATE-STAMP (I1)        
                                              TO PA52WS-L-DATE-STAMP
                             MOVE PA52F6-LTM-TIME-STAMP (I1)
                                              TO PA52WS-L-TIME-STAMP
                             MOVE PA52F6-ERROR-FLAG (I1)
                                              TO PA52WS-L-ERROR-FLAG
                             MOVE PA52F6-WORKASSIGNMENT (I1)
                                              TO PA52WS-L-WORKASSIGNMENT
                             MOVE PA52F6-ACTION-CODE (I1)
                                              TO PA52WS-L-ACTION-CODE
                             MOVE PA52F6-ACTION-REASON (I1)
                                              TO PA52WS-L-ACTION-REASON
                             MOVE PA52F6-EFFECT-DATE (I1) 
                                              TO PA52WS-L-EFFECT-DATE
                             MOVE PA52F6-LINE-FC (I1)                   
                                              TO PA52WS-L-LINE-FC
                         END-IF                        
                         ADD 1           TO I1
                     ELSE
                         ADD 1           TO I1
                     END-IF                        
           END-PERFORM.

       220-END.

      ******************************************************************
       480-INQUIRE.
      ******************************************************************
      
           IF (PA52F6-FC = "I")
               MOVE "+"                    TO PA52F6-FC.

           INITIALIZE PA52WS-LEM-FIELDS.

           MOVE PRS-NAME                   TO PA52F6-PRS-NAME.

           MOVE PA52F6-COMPANY             TO DB-COMPANY.
           MOVE PA52F6-EMPLOYEE            TO DB-EMPLOYEE.
           PERFORM 840-FIND-EMPSET1.

           MOVE EMP-LAST-NAME              TO HRWS-LAST-NAME.
           MOVE EMP-FIRST-NAME             TO HRWS-FIRST-NAME.
           MOVE EMP-MIDDLE-INIT            TO HRWS-MIDDLE-INIT.
           MOVE EMP-NAME-SUFFIX            TO HRWS-NAME-SUFFIX.
           MOVE EMP-LAST-NAME-PRE          TO HRWS-LAST-NAME-PRE.
           PERFORM 750-HR-FORMAT-NAME.
           MOVE HRWS-FORMAT-NAME           TO PA52F6-EMP-FULL-NAME.

           IF (PA52F6-FC = "-")
               MOVE 11                     TO I1
               MOVE PA52F6-FC                 TO PA52WS-LST-FC
               INITIALIZE PA52F6-DETAIL-GROUP
               MOVE PA52WS-FST-STATUS          TO DB-STATUS 
               MOVE PA52WS-FST-LTM-DATE-STAMP  TO DB-LTM-DATE-STAMP 
               MOVE PA52WS-FST-LTM-TIME-STAMP  TO DB-LTM-TIME-STAMP 
               MOVE PA52WS-FST-ERROR-FLAG      TO DB-ERROR-FLAG 
               MOVE PA52WS-FST-WORKASSIGNMENT  TO DB-WORKASSIGNMENT
               MOVE PA52WS-FST-ACTION-CODE     TO DB-ACTION-CODE
               PERFORM 850-FIND-NLT-LEMSET2
               PERFORM 870-FIND-PREV-LEMSET2
               PERFORM 610-MOVE-DTL-TO-SCREEN
               THRU    610-END
                   UNTIL  (I1           < 1)
                   OR     (HREMPRCVR-NOTFOUND)
                   OR     (LEM-COMPANY  NOT = PA52F6-COMPANY)
                   OR     (LEM-EMPLOYEE NOT = PA52F6-EMPLOYEE)
                   OR     (LEM-STATUS NOT = 1)

               IF (I1 NOT < 1)
                   COMPUTE I9              = I1 + 1
                   MOVE 1                  TO I1
                   PERFORM
                       UNTIL (I9 > 11)
                           MOVE PA52F6-DETAIL-LINE (I9)
                                           TO PA52F6-DETAIL-LINE (I1)
                           INITIALIZE         PA52F6-DETAIL-LINE (I9)
                           ADD 1           TO I1
                                              I9
                   END-PERFORM
                   MOVE "+"                       TO PA52F6-FC
                   MOVE PA52F6-COMPANY            TO DB-COMPANY
                   MOVE PA52F6-EMPLOYEE           TO DB-EMPLOYEE

                   IF (HREMPRCVR-FOUND)
                     MOVE PA52WS-LST-STATUS         TO DB-STATUS
                     MOVE PA52WS-LST-LTM-DATE-STAMP TO DB-LTM-DATE-STAMP
                     MOVE PA52WS-LST-LTM-TIME-STAMP TO DB-LTM-TIME-STAMP
                     MOVE PA52WS-LST-ERROR-FLAG     TO DB-ERROR-FLAG
                     MOVE PA52WS-LST-WORKASSIGNMENT TO DB-WORKASSIGNMENT
                     MOVE PA52WS-LST-ACTION-CODE    TO DB-ACTION-CODE
                     PERFORM 850-FIND-NLT-LEMSET2
                     PERFORM 860-FIND-NEXT-LEMSET2
                   ELSE
                     MOVE PA52WS-FST-STATUS         TO DB-STATUS
                     MOVE PA52WS-FST-LTM-DATE-STAMP TO DB-LTM-DATE-STAMP
                     MOVE PA52WS-FST-LTM-TIME-STAMP TO DB-LTM-TIME-STAMP
                     MOVE PA52WS-FST-ERROR-FLAG     TO DB-ERROR-FLAG
                     MOVE PA52WS-FST-WORKASSIGNMENT TO DB-WORKASSIGNMENT
                     MOVE PA52WS-FST-ACTION-CODE    TO DB-ACTION-CODE
                     PERFORM 850-FIND-NLT-LEMSET2
                   END-IF

                   PERFORM 610-MOVE-DTL-TO-SCREEN
                   THRU    610-END
                       UNTIL  (I1 > 11)
                       OR     (HREMPRCVR-NOTFOUND)
                       OR     (LEM-COMPANY  NOT = PA52F6-COMPANY)
                       OR     (LEM-EMPLOYEE NOT = PA52F6-EMPLOYEE)
                       OR     (LEM-STATUS NOT = 1)
      
               END-IF
      
               IF (HREMPRCVR-NOTFOUND)
               OR (LEM-COMPANY  NOT = PA52F6-COMPANY)
               OR (LEM-EMPLOYEE NOT = PA52F6-EMPLOYEE)
               OR (LEM-STATUS NOT = 1)
                   SET HREMPRCVR-NOTFOUND         TO TRUE 
                   MOVE CRT-INQ-COMPLETE          TO CRT-MESSAGE
               ELSE
                   MOVE CRT-MORE-RECS             TO CRT-MESSAGE
               END-IF                        
           ELSE
               MOVE 1                             TO I1
               INITIALIZE PA52F6-DETAIL-GROUP  
               MOVE PA52F6-COMPANY                TO DB-COMPANY
               MOVE PA52F6-EMPLOYEE               TO DB-EMPLOYEE
               IF (PA52WS-LST-FC NOT = "+")
                   PERFORM 850-FIND-NLT-LEMSET2
               END-IF
               PERFORM 610-MOVE-DTL-TO-SCREEN
               THRU    610-END
                   UNTIL  (I1           > 11)
                   OR     (HREMPRCVR-NOTFOUND)
                   OR     (LEM-COMPANY  NOT = PA52F6-COMPANY)
                   OR     (LEM-EMPLOYEE NOT = PA52F6-EMPLOYEE)
                   OR     (LEM-STATUS NOT = 1)
               MOVE PA52F6-FC                     TO PA52WS-LST-FC 
               IF (HREMPRCVR-NOTFOUND)   
               OR (LEM-COMPANY  NOT = PA52F6-COMPANY)
               OR (LEM-EMPLOYEE NOT = PA52F6-EMPLOYEE)
               OR (LEM-STATUS NOT = 1)
                   MOVE CRT-INQ-COMPLETE          TO CRT-MESSAGE
                   SET HREMPRCVR-NOTFOUND         TO TRUE
               ELSE
                   MOVE CRT-MORE-RECS             TO CRT-MESSAGE    
               END-IF 
           END-IF.

           MOVE SPACES TO PA52F6-FC.

       480-END.
       
      ******************************************************************
       610-MOVE-DTL-TO-SCREEN.
      ******************************************************************
      
           MOVE LEM-COMPANY               TO PA52F6-COMPANY.
           MOVE LEM-EMPLOYEE              TO PA52F6-EMPLOYEE. 
           MOVE LEM-STATUS                TO PA52F6-STATUS (I1).  
           MOVE LEM-LTM-DATE-STAMP        TO PA52F6-LTM-DATE-STAMP (I1).
           MOVE LEM-LTM-TIME-STAMP        TO PA52F6-LTM-TIME-STAMP (I1).
           MOVE LEM-ERROR-FLAG            TO PA52F6-ERROR-FLAG (I1).
           MOVE LEM-WORKASSIGNMENT        TO PA52F6-WORKASSIGNMENT(I1).
           MOVE LEM-ACTION-CODE           TO PA52F6-ACTION-CODE (I1).
           MOVE LEM-ACTION-REASON         TO PA52F6-ACTION-REASON (I1).
           MOVE LEM-EFFECT-DATE           TO PA52F6-EFFECT-DATE (I1).

           IF (I1 = 1)
            MOVE PA52F6-COMPANY            TO PA52WS-FST-COMPANY 
            MOVE PA52F6-EMPLOYEE           TO PA52WS-FST-EMPLOYEE 
            MOVE PA52F6-STATUS (I1)        TO PA52WS-FST-STATUS 
            MOVE PA52F6-LTM-DATE-STAMP(I1) TO PA52WS-FST-LTM-DATE-STAMP
            MOVE PA52F6-LTM-TIME-STAMP(I1) TO PA52WS-FST-LTM-TIME-STAMP
            MOVE PA52F6-ERROR-FLAG (I1)    TO PA52WS-FST-ERROR-FLAG 
            MOVE PA52F6-WORKASSIGNMENT(I1) TO PA52WS-FST-WORKASSIGNMENT 
            MOVE PA52F6-ACTION-CODE (I1)   TO PA52WS-FST-ACTION-CODE
            MOVE PA52F6-ACTION-REASON(I1)  TO PA52WS-FST-ACTION-REASON 
           END-IF.
           IF (I1 = 11)
            MOVE PA52F6-COMPANY            TO PA52WS-LST-COMPANY
            MOVE PA52F6-EMPLOYEE           TO PA52WS-LST-EMPLOYEE
            MOVE PA52F6-STATUS (I1)        TO PA52WS-LST-STATUS
            MOVE PA52F6-LTM-DATE-STAMP(I1) TO PA52WS-LST-LTM-DATE-STAMP
            MOVE PA52F6-LTM-TIME-STAMP(I1) TO PA52WS-LST-LTM-TIME-STAMP
            MOVE PA52F6-ERROR-FLAG (I1)    TO PA52WS-LST-ERROR-FLAG
            MOVE PA52F6-WORKASSIGNMENT(I1) TO PA52WS-LST-WORKASSIGNMENT 
            MOVE PA52F6-ACTION-CODE (I1)   TO PA52WS-LST-ACTION-CODE
            MOVE PA52F6-ACTION-REASON(I1)  TO PA52WS-LST-ACTION-REASON
           END-IF.
      
           IF (PA52F6-FC = "-")
               SUBTRACT 1             FROM I1
           ELSE
               ADD 1                  TO I1.
      
       610-NEXT.
           IF (PA52F6-FC = "-")
               PERFORM 870-FIND-PREV-LEMSET2
           ELSE
               PERFORM 860-FIND-NEXT-LEMSET2.
      
       610-END.
      ******************************************************************
End    PA52S6-TRANSACTION-END.
J40356******************************************************************
