******* PA100PD 97.1.1.1.1.1.44 <2089204881>
      ******************************************************************
      *                            PA100PD                             *
      ******************************************************************
      *                                                                *
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  314716 | J14716 | INCLUDE MOVING OF DECIMAL LENGTH TO FIELD   *
      *         |        | HRWS-NBR-DECIMALS TO BE USED FOR COMPUTA-   *
      *         |        | TION AND PROPER ASSIGNMENT OF EMPLOYEE      *
      *         |        | GROUPS.                                     *
      *  ------   ------   ------------------------------------------  *
      *  208104   J08104   AUDITING OF PERSONNNEL ACTIONS              *
      *  ------   ------   ------------------------------------------  *
      *  263431 | J63431 | PASSING PROCESS-TYPE-SW TO PA100FLDS        *
      *  ------   ------   ------------------------------------------  *
      *  359149 | J59149 | MOVED VALUE FROM APL-PROCESS-LEVEL TO HREMP *
      *         |        | -PROCESS-LEVEL TO BE USED FOR FINDING POS   *
      *         |        | RULE EVEN IF PROCESS LEVEL IS NOT ENTERED   *
      *         |        | ON PA52.4                                   *
      *  ------   ------   ------------------------------------------  *
      *  284223 | J84223 | FIXED MOVE OF PAY FREQUENCY VALUE FROM AN   *
      *         |        | ALPHANUMERIC VARIABLE TO A NUMERIC VARIABLE *
      *  ------   ------   ------------------------------------------  *
      *  327541 | J27541 | PA100 ERROR EMPLOYEES INCLUDED IN THE       *
      *         |        | PA100.RPT OUTPUT REPORT.                    *
      *  ------   ------   ------------------------------------------  *
      *  401796 | J01796 | EDIT PROGRAM TO RECOGNIZE DIFFERENCE OF PAY *
      *         |        | RATE VALUE COMING FROM A STEP AND GRADE AND *
      *         |        | WHEN ENTERED BY A USER                      *
      *  ------   ------   ------------------------------------------  *
      *  421883 | J21883 | FIX RESTART LOGIC - PA100 NOT RESTARTING    *
      *         |        | PROPERLY WHEN RUNNING FOR OPTION 4.         *
      *  ------   ------   ------------------------------------------  *
      *  460556 | J60556 | ALLOW SAME FIELD VALUE IF FIELD IS SETUP    *
      *         |        | AS DEFAULTING.                              *
      *  ------   ------   ------------------------------------------  *
      *  619114 | J19114 | MOVE EMAIL COMING FROM APL-EMAIL-ADDRESS    *
      *         |        | TO HREMP-EMAIL-PERSONAL                     *
      *  ------   ------   ------------------------------------------  *
      *  573509 | J73509 | NOT UPDATING EMPLOYEES IN REGARDS TO        *
      *         |        | EMPLOYEE GROUPS ON RECOVERY.                *
      *  ------   ------   ------------------------------------------  *
      *  665420 | J65420 | PA100 New hire action does not update by    *
      *         |        | individual Process Level.                   *
      *  ------   ------   ------------------------------------------  *
      *  664902 | J64902 | - REARRANGED PRINTING OF EMPLOYEE DETAILS.  *
      *         |        | - VALIDATED DEF-LOAD-SW FOR SAME-VALUE ERROR*
      *  ------   ------   ------------------------------------------  *
      *  718332 | J18332 | Populate audit trails/fields for PARTICIPNT *
      *  ------   ------   ------------------------------------------  *
      *  719449 | J19449 | Find existing PERSACTHST record before      *
      *         |        | creating a new record                       *
      *  ------   ------   ------------------------------------------- *
      *  783708 | J83708 | Changes made allow workflow to trigger in   *
      *         |        | order to create BOD                         *
CRP001*  ------   ------   ------------------------------------------  *
CRP001* CPSMOD  | CRP001 | Added logic to create IPA work unit to      *
CRP001*         |        | recalc benefit information                  *      
      *  ------   ------   ------------------------------------------  *
      *  921860 | J21860 | ADDED CALL TO 900-GET-USER-DISPLAY-NAME     *
      *         |        | WHEN PUTTING CREATE-ID AND USER-ID ON THE   *
      *         |        | REPORTS.                                    *
      *  ------ | ------ | ------------------------------------------- *
      *  913033 | J13033 | ADDED CALL TO CLOSE THE ERRORS, WARNINGS,   *
      *         |        | AND HISTAUDIT FILES.                        *
      *  ------ | ------ | ------------------------------------------- *
      *  948710 | J48710 | FOR WINDOWS USERS, ALLOW ENTRY OF THE       *
      *         |        | COMMON ID TO BE TRANSLATED TO NTID          *
      *  ------ | ------ | ------------------------------------------- *
      * 1078854 | 078854 | Fix for PA100 creating BN35 records         *
      *         |        | differently that PA52 Immediate Y actions   *
      *  ------ | ------ | ------------------------------------------- *
      * 1101470 | 101470 | Personnel Action History shows incorrect    *
      *         |        | reason code and "updated user" for a PA02   *
      *         |        | change that was captured by PA100           *
      ******************************************************************
MG0808*****************************************************************
MG0808* Modified by MARK GLISSEN - MG0808                             *
MG0808*****************************************************************
MG0808* 08/08/05  - MODIFIED PA100 TO PROCESS PA52 ACTIONS THAT ARE   *
MG0808*             PENDING ADDRESS CHANGES.                          *
ACS001* 04/22/09  - ACS - M. HUNTER                                   *
ACS001*             REAPPLIED ABOVE CUSTOMIZATIONS AFTER CTP 58237    *
ACS001*             WAS APPLIED.                                      *
ACS002* 05/09/09  - ACS - M. HUNTER                                   *
ACS002*             REMOVED THE PERFORM 1000-OPEN-WORKFLOW-DB         *
ACS002*             STATEMENT FROM THE PAPFADCHPD AND ADDED IT TO THE *
ACS002*             100-START PARAGRAPH BECAUSE IT MUST BE DONE BEFORE*
ACS002*             A 910-AUDIT-BEGIN STATEMENT.                      *
ACS003* 05/21/10    ACS - M. HUNTER                                   *
ACS003*             MODIFIED TO CREATE A TRIGGER FOR PENDING ACTIONS  *
ACS003*             THAT USE THE PPLBPCUPD AND BPCUPD PROCESSES.      *
ACS004* 10/20/10    ACS - M. HUNTER                                   *
ACS004*             REAPPLIED CUSTOMIZATIONS AFTER 9.0 APP UPGRADE    *
ACS005* 08/19/11    ACS - M. HUNTER                                   *
ACS005*             REAPPLIED CUSTOMIZATIONS AFTER 9.0.1 APP UPGRADE  *
SDB001* 10/2012    ADDED NEW ACTION FOR 'SPMISSTERM' TO PARAGRAPH     *
SDB001*            2100-PF-PAPFBPCUP 
SDB002* 05/2014    FP 5900
SDB002*            THE PERSONAL ACTION SHOULD BE "NH CORRECT" RATHER 
SDB002*            THAN "NHCORRECT"  
      
000200*****************************************************************
000100 050-EDIT-PARAMETERS             SECTION.
000200*****************************************************************
000300 050-START.
000400*
           MOVE "Y"                       TO HREMP-BATCH-PROGRAM.

000500     MOVE PRM-COMPANY               TO DB-COMPANY.
000600     INITIALIZE                        DB-PROCESS-LEVEL
000700                                       DB-DEPARTMENT
000800                                       DB-EFFECT-DATE.
000900     PERFORM 850-FIND-NLT-PPRSET1.
001000     IF (PAPOSRULE-FOUND)
001100         SET PA-USER TO TRUE.
001200
001300     PERFORM 840-FIND-PRSSET1.
001400     IF (PRSYSTEM-NOTFOUND)
001500         MOVE PRM-COMPANY           TO CRT-ERR-VAR1
001600         MOVE 100                   TO CRT-ERROR-NBR
001700         MOVE WS-TRUE               TO WS-PARAMETER-ERROR
001800         PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-EDIT-PARAMETERS-END
002000     ELSE
002100         MOVE PRS-NAME              TO WS-COMPANY-NAME
               MOVE PRS-ACTION-CODE       TO WS-ACTION-CODE
               MOVE PRS-REASON            TO WS-REASON.
002300
J48710     IF (PRM-USER-ID NOT = SPACES)
J48710         MOVE PRM-USER-ID            TO WS-USER-NAME
J48710         PERFORM 900-EDIT-USER-NAME
J48710         IF (ERROR-FOUND)
J48710             MOVE PRM-USER-ID        TO WS-USER-DBUIDKEY
J48710             PERFORM 900-GET-USER-DISPLAY-NAME
J48710             PERFORM 900-GET-USER-DBUIDKEY
J48710             MOVE WS-USER-DBUIDKEY   TO WS-USER-NAME
J48710             INITIALIZE CRT-ERROR-NBR
J48710                        CRT-ERROR-CAT
J48710             PERFORM 900-EDIT-USER-NAME
J48710             MOVE WS-USER-NAME TO PRM-USER-ID
J48710          END-IF
J48710     END-IF.
           SET WS-SPEC-ACT-NOT-FOUND TO TRUE.
002500     PERFORM
002600         VARYING I2 FROM 1 BY 1
002700         UNTIL  (I2 > 6)
002800             IF (PRM-EMP-ACTION (I2) NOT = SPACES)
                       ADD 1                    TO PA100WS-ACTION-CNT
002900                 IF (PRM-RUN-OPTION = 1 OR 5 OR 6)
003000                     MOVE PRM-EMP-ACTION (I2)
003100                                          TO CRT-ERR-VAR1
003200                     MOVE 120             TO CRT-ERROR-NBR
003300                     MOVE WS-TRUE         TO WS-PARAMETER-ERROR
003400                     PERFORM 780-PRINT-ERROR-MSG
                           GO TO 050-EDIT-PARAMETERS-END
003600                 ELSE
003700                     MOVE PRM-EMP-ACTION (I2) TO DB-ACTION-CODE
003800                     PERFORM 840-FIND-PATSET1
003900                     IF (PERSACTYPE-NOTFOUND)
004000                         MOVE PRM-EMP-ACTION (I2)
004100                                          TO CRT-ERR-VAR1
004200                         MOVE 109         TO CRT-ERROR-NBR
004300                         MOVE WS-TRUE     TO WS-PARAMETER-ERROR
004400                         PERFORM 780-PRINT-ERROR-MSG
                               GO TO 050-EDIT-PARAMETERS-END
004600                     END-IF
                           IF (PAT-ACTIVE-FLAG = "2")   
                               MOVE PRM-EMP-ACTION (I2)
                                                TO CRT-ERR-VAR1
004200                         MOVE 112         TO CRT-ERROR-NBR
004300                         MOVE WS-TRUE     TO WS-PARAMETER-ERROR
004400                         PERFORM 780-PRINT-ERROR-MSG
                               GO TO 050-EDIT-PARAMETERS-END
                           ELSE
004700                         SET WS-SPEC-ACT-FOUND TO TRUE
                           END-IF
                       END-IF
                   END-IF
           END-PERFORM.
004800
004900     IF  (PRM-RUN-OPTION      = 1 OR 5 OR 6)
           AND ((PRM-EMP-PROC-GRP   NOT = SPACES)
           OR   (PRM-PROCESS-LEVEL  NOT = SPACES)
           OR   (PRM-GROUP-NAME     NOT = SPACES)
           OR   (PRM-EMPLOYEE       NOT = ZEROES)
           OR   (PRM-EMP-ACTION-NBR NOT = ZEROES))
006800          MOVE WS-TRUE           TO WS-PARAMETER-ERROR
006700          MOVE 119               TO CRT-ERROR-NBR
006900          PERFORM 780-PRINT-ERROR-MSG
                GO TO 050-EDIT-PARAMETERS-END.
006300
004900     IF  (PRM-RUN-OPTION      = 2 OR 3 OR 5 OR 6)
           AND ((PRM-APL-PROC-GRP   NOT = SPACES)
           OR   (PRM-APL-PROC-LEVEL NOT = SPACES)
           OR   (PRM-APPLICANT      NOT = ZEROES))
006800          MOVE WS-TRUE           TO WS-PARAMETER-ERROR
006700          MOVE 118               TO CRT-ERROR-NBR
006900          PERFORM 780-PRINT-ERROR-MSG
                GO TO 050-EDIT-PARAMETERS-END.
006300
004900     IF  (PRM-RUN-OPTION      = 4)
           AND ((PRM-APPLICANT      NOT = ZEROES)
           OR   (PRM-EMPLOYEE       NOT = ZEROES)
           OR   (PRM-EMP-ACTION-NBR NOT = ZEROES))
006800          MOVE WS-TRUE           TO WS-PARAMETER-ERROR
006700          MOVE 128               TO CRT-ERROR-NBR
006900          PERFORM 780-PRINT-ERROR-MSG
                GO TO 050-EDIT-PARAMETERS-END.
006300
004900     IF (PRM-RUN-OPTION < 5)
P69186     OR (PRM-RUN-OPTION = 7)
               IF (PRM-MASS-ACTION-CODE NOT = SPACES)
               OR (PRM-MASS-ACTION-NBR  NOT = ZEROES)
               OR (PRM-ERROR-OPTION     NOT = ZEROES)
006800             MOVE WS-TRUE           TO WS-PARAMETER-ERROR
006700             MOVE 129               TO CRT-ERROR-NBR
006900             PERFORM 780-PRINT-ERROR-MSG
                   GO TO 050-EDIT-PARAMETERS-END
               END-IF
               IF (PRM-APL-PROC-GRP       NOT = SPACES)
                   IF (PRM-APL-PROC-LEVEL NOT = SPACES)
006800                 MOVE WS-TRUE           TO WS-PARAMETER-ERROR
006700                 MOVE 130               TO CRT-ERROR-NBR
006900                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
                   IF (PRM-APPLICANT NOT = ZEROES)
006800                 MOVE WS-TRUE           TO WS-PARAMETER-ERROR
006700                 MOVE 131               TO CRT-ERROR-NBR
006900                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
                   MOVE PRM-COMPANY        TO DB-COMPANY
                   MOVE PRM-APL-PROC-GRP   TO DB-PROC-GROUP
                   MOVE PRPSET1-PROC-GROUP TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-PRPSET1
                   IF (PRPROCGRP-NOTFOUND)
004900                 MOVE 136              TO CRT-ERROR-NBR
000270                 MOVE PRM-APL-PROC-GRP TO CRT-ERR-VAR1
000290                 MOVE WS-TRUE          TO WS-PARAMETER-ERROR
000300                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
               END-IF
               IF (PRM-APL-PROC-LEVEL NOT = SPACES)
                   IF (PRM-APPLICANT  NOT = ZEROES)
006800                 MOVE WS-TRUE           TO WS-PARAMETER-ERROR
006700                 MOVE 137               TO CRT-ERROR-NBR
006900                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
                   IF (PRM-EMP-ACTION-NBR NOT = ZEROES)
006800                 MOVE WS-TRUE           TO WS-PARAMETER-ERROR
006700                 MOVE 162               TO CRT-ERROR-NBR
006900                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
007200             MOVE PRM-APL-PROC-LEVEL    TO DB-PROCESS-LEVEL
007300             PERFORM 840-FIND-PRSSET1
007400             IF (PRSYSTEM-NOTFOUND)
007500                 MOVE PRM-APL-PROC-LEVEL TO CRT-ERR-VAR1
007600                 MOVE 138                TO CRT-ERROR-NBR
007700                 MOVE WS-TRUE            TO WS-PARAMETER-ERROR
007800                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
               END-IF
               IF (PRM-APPLICANT NOT = ZEROES)
                   MOVE PRM-COMPANY   TO DB-COMPANY
                   MOVE PRM-APPLICANT TO DB-APPLICANT
                   PERFORM 840-FIND-APLSET1
                   IF (APPLICANT-NOTFOUND)
007500                 MOVE PRM-APPLICANT      TO CRT-ERR-VAR1
007600                 MOVE 139                TO CRT-ERROR-NBR
007700                 MOVE WS-TRUE            TO WS-PARAMETER-ERROR
007800                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
               END-IF
               IF (PRM-EMP-PROC-GRP NOT = SPACES)
                   IF (PRM-PROCESS-LEVEL NOT = SPACES)
006800                 MOVE WS-TRUE           TO WS-PARAMETER-ERROR
006700                 MOVE 140               TO CRT-ERROR-NBR
006900                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
                   IF (PRM-GROUP-NAME NOT = SPACES)
006800                 MOVE WS-TRUE           TO WS-PARAMETER-ERROR
006700                 MOVE 141               TO CRT-ERROR-NBR
006900                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
                   IF (PRM-EMPLOYEE NOT = ZEROES)
006800                 MOVE WS-TRUE           TO WS-PARAMETER-ERROR
006700                 MOVE 142               TO CRT-ERROR-NBR
006900                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
                   IF (PRM-EMP-ACTION-NBR NOT = ZEROES)
006800                 MOVE WS-TRUE           TO WS-PARAMETER-ERROR
006700                 MOVE 160               TO CRT-ERROR-NBR
006900                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
                   MOVE PRM-COMPANY        TO DB-COMPANY
                   MOVE PRM-EMP-PROC-GRP   TO DB-PROC-GROUP
                   MOVE PRPSET1-PROC-GROUP TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-PRPSET1
                   IF (PRPROCGRP-NOTFOUND)
004900                 MOVE 143              TO CRT-ERROR-NBR
000270                 MOVE PRM-EMP-PROC-GRP TO CRT-ERR-VAR1
000290                 MOVE WS-TRUE          TO WS-PARAMETER-ERROR
000300                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
               END-IF
               IF (PRM-PROCESS-LEVEL NOT = SPACES)
                   IF (PRM-GROUP-NAME NOT = SPACES)
006800                 MOVE WS-TRUE           TO WS-PARAMETER-ERROR
006700                 MOVE 144               TO CRT-ERROR-NBR
006900                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
                   IF (PRM-EMPLOYEE NOT = ZEROES)
006800                 MOVE WS-TRUE           TO WS-PARAMETER-ERROR
006700                 MOVE 145               TO CRT-ERROR-NBR
006900                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
                   IF (PRM-EMP-ACTION-NBR NOT = ZEROES)
006800                 MOVE WS-TRUE           TO WS-PARAMETER-ERROR
006700                 MOVE 163               TO CRT-ERROR-NBR
006900                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
               END-IF
               IF (PRM-GROUP-NAME NOT = SPACES)
                   IF (PRM-EMPLOYEE NOT = ZEROES)
006800                 MOVE WS-TRUE           TO WS-PARAMETER-ERROR
006700                 MOVE 146               TO CRT-ERROR-NBR
006900                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
                   IF (PRM-EMP-ACTION-NBR NOT = ZEROES)
006800                 MOVE WS-TRUE           TO WS-PARAMETER-ERROR
006700                 MOVE 164               TO CRT-ERROR-NBR
006900                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
                   MOVE PRM-COMPANY        TO DB-COMPANY
                   MOVE PRM-GROUP-NAME     TO DB-GROUP-NAME
                   PERFORM 840-FIND-PRGSET1
                   IF (PERSGROUP-NOTFOUND)
000270                 MOVE PRM-GROUP-NAME TO CRT-ERR-VAR1
000290                 MOVE WS-TRUE        TO WS-PARAMETER-ERROR
004900                 MOVE 147            TO CRT-ERROR-NBR
000300                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
               END-IF
               IF (PRM-EMPLOYEE NOT = ZEROES)
                   MOVE PRM-COMPANY        TO DB-COMPANY
                   MOVE PRM-EMPLOYEE       TO DB-EMPLOYEE
                   PERFORM 840-FIND-EMPSET1
                   IF (EMPLOYEE-NOTFOUND)
000290                 MOVE WS-TRUE        TO WS-PARAMETER-ERROR
004900                 MOVE 156            TO CRT-ERROR-NBR
000270                 MOVE PRM-EMPLOYEE   TO CRT-ERR-VAR1
000300                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
               END-IF
               IF (PRM-EMP-ACTION-NBR NOT = ZEROES)
                   IF (PRM-EMPLOYEE   = ZEROES)
000290                 MOVE WS-TRUE        TO WS-PARAMETER-ERROR
004900                 MOVE 157            TO CRT-ERROR-NBR
000300                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   END-IF
                   IF (PA100WS-ACTION-CNT  NOT = 1)
000290                 MOVE WS-TRUE        TO WS-PARAMETER-ERROR
004900                 MOVE 158            TO CRT-ERROR-NBR
000300                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END.
006300
006400     IF (PRM-PROCESS-LEVEL  NOT = SPACES)
006500         IF (PRM-RUN-OPTION = 1 OR 5 OR 6)
006600             MOVE PRM-PROCESS-LEVEL TO CRT-ERR-VAR1
006700             MOVE 134               TO CRT-ERROR-NBR
006800             MOVE WS-TRUE           TO WS-PARAMETER-ERROR
006900             PERFORM 780-PRINT-ERROR-MSG
                   GO TO 050-EDIT-PARAMETERS-END
007100         ELSE
007200             MOVE PRM-PROCESS-LEVEL     TO DB-PROCESS-LEVEL
007300             PERFORM 840-FIND-PRSSET1
007400             IF (PRSYSTEM-NOTFOUND)
007500                 MOVE PRM-PROCESS-LEVEL TO CRT-ERR-VAR1
007600                 MOVE 124               TO CRT-ERROR-NBR
007700                 MOVE WS-TRUE           TO WS-PARAMETER-ERROR
007800                 PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END.
008200*
008300     IF  ((PRM-EFFECT-DATE     NOT = ZEROES)
008400     AND  (PRM-CURRENT         = "Y"))
008500     OR  ((PRM-EFFECT-DATE     = ZEROES)
008600     AND  (PRM-CURRENT         = "N"))
008700         MOVE PRM-EFFECT-DATE   TO CRT-ERR-VAR1
008800         MOVE 152               TO CRT-ERROR-NBR
008900         MOVE WS-TRUE           TO WS-PARAMETER-ERROR
009000         PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-EDIT-PARAMETERS-END.
009200*
009300     IF  (PRM-EFFECT-DATE = ZEROES)
009400     AND (PRM-CURRENT     = "Y")
009500         MOVE WS-SYSTEM-DATE-YMD  TO WS-PRM-EFFECT-DATE
009600     ELSE
009700         MOVE PRM-EFFECT-DATE     TO WS-PRM-EFFECT-DATE.
009800*
P69186     IF  (PRM-RUN-OPTION    = 7)
P69186     AND (PRM-UPDATE-OPTION = "Y")
P69186         MOVE 340               TO CRT-ERROR-NBR
P69186         MOVE PRM-RUN-OPTION    TO CRT-ERR-VAR1
P69186         MOVE WS-TRUE           TO WS-PARAMETER-ERROR
P69186         PERFORM 780-PRINT-ERROR-MSG
P69186         GO TO 050-EDIT-PARAMETERS-END.
P69186
P69186     IF  (PRM-RUN-OPTION    = 5 OR 6)
011400     AND (PRM-ERROR-OPTION  = ZEROES)
               MOVE PRM-RUN-OPTION    TO CRT-ERR-VAR1
011600         MOVE 126               TO CRT-ERROR-NBR
011700         MOVE WS-TRUE           TO WS-PARAMETER-ERROR
011800         PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-EDIT-PARAMETERS-END.

004900     IF (PRM-RUN-OPTION > 4)
               IF (PRM-MASS-ACTION-CODE NOT = SPACES)
                   MOVE PRM-COMPANY          TO DB-COMPANY
                   MOVE PRM-MASS-ACTION-CODE TO DB-ACTION-CODE
                   PERFORM 840-FIND-PATSET1
                   IF (PERSACTYPE-NOTFOUND)
000290                 MOVE WS-TRUE                TO WS-PARAMETER-ERROR
004900                 MOVE 159                    TO CRT-ERROR-NBR
000270                 MOVE PRM-MASS-ACTION-CODE   TO CRT-ERR-VAR1
000300                 PERFORM 780-PRINT-ERROR-MSG
004600             ELSE
                       IF (PAT-ACTIVE-FLAG = "2")   
                           MOVE PRM-MASS-ACTION-CODE   
                                                   TO CRT-ERR-VAR1
004200                     MOVE 166                TO CRT-ERROR-NBR
004300                     MOVE WS-TRUE            TO WS-PARAMETER-ERROR
004400                     PERFORM 780-PRINT-ERROR-MSG
                       END-IF.
012000
020400     SET WS-ACT-NOTFOUND TO TRUE.
020500     SET WS-PL-NOTFOUND TO TRUE.
020600
P69186     IF  (PRM-RUN-OPTION = 5 OR 6)
               IF (PRM-RUN-OPTION = 5)
                   MOVE "M"            TO DB-ACTION-TYPE
               END-IF
               IF (PRM-RUN-OPTION = 6)
                   MOVE "P"            TO DB-ACTION-TYPE
               END-IF
               MOVE WS-PRM-EFFECT-DATE TO DB-EFFECT-DATE
               IF  (PRM-MASS-ACTION-CODE = SPACES)
                   MOVE PCTSET1-EFFECT-DATE    TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-PCTSET1
                   IF  (PERSACTION-NOTFOUND)
                       MOVE 132                        TO CRT-ERROR-NBR
                       PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   ELSE
                   IF  (PRM-USER-ID        NOT = SPACES)
                   AND (PCT-USER-ID        NOT = PRM-USER-ID)
                       MOVE 165            TO CRT-ERROR-NBR
                       PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   ELSE
                       SET WS-ACT-FOUND TO TRUE
                       MOVE PCT-ACTION-CODE        TO WS-SV-ACTION-CODE
                       MOVE PCT-ACTION-NBR         TO WS-SV-ACTION-NBR
                       MOVE PCT-EFFECT-DATE        TO WS-SV-EFFECT-DATE
                       PERFORM 860-FIND-NXTRNG-PCTSET1
                       IF  (PERSACTION-FOUND)
                           MOVE 108                    TO CRT-ERROR-NBR
                           PERFORM 780-PRINT-ERROR-MSG
                           GO TO 050-EDIT-PARAMETERS-END
                       END-IF
                   END-IF
                   END-IF
               ELSE
               IF  (PRM-MASS-ACTION-NBR = ZEROES)
                   MOVE PRM-MASS-ACTION-CODE   TO DB-ACTION-CODE
                   MOVE PCTSET1-ACTION-CODE    TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-PCTSET1
                   IF  (PERSACTION-NOTFOUND)
                       MOVE 132                        TO CRT-ERROR-NBR
                       PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   ELSE
                   IF  (PRM-USER-ID        NOT = SPACES)
                   AND (PCT-USER-ID        NOT = PRM-USER-ID)
                       MOVE 165            TO CRT-ERROR-NBR
                       PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   ELSE
                       SET WS-ACT-FOUND TO TRUE
                       MOVE PCT-ACTION-CODE        TO WS-SV-ACTION-CODE
                       MOVE PCT-ACTION-NBR         TO WS-SV-ACTION-NBR
                       MOVE PCT-EFFECT-DATE        TO WS-SV-EFFECT-DATE
                       PERFORM 860-FIND-NXTRNG-PCTSET1
                       IF  (PERSACTION-FOUND)
                           MOVE 123                    TO CRT-ERROR-NBR
                           PERFORM 780-PRINT-ERROR-MSG
                           GO TO 050-EDIT-PARAMETERS-END
                       END-IF
                   END-IF
                   END-IF
               ELSE
                   MOVE PRM-MASS-ACTION-CODE   TO DB-ACTION-CODE
                   INITIALIZE                     DB-EMPLOYEE
                   MOVE PRM-MASS-ACTION-NBR    TO DB-ACTION-NBR
                   PERFORM 840-FIND-PCTSET1
                   IF  (PERSACTION-NOTFOUND)
                       MOVE 132                        TO CRT-ERROR-NBR
                       PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   ELSE
                   IF  (PRM-USER-ID        NOT = SPACES)
                   AND (PCT-USER-ID        NOT = PRM-USER-ID)
                       MOVE 165            TO CRT-ERROR-NBR
                       PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-EDIT-PARAMETERS-END
                   ELSE
                       SET WS-ACT-FOUND TO TRUE
                       MOVE PCT-ACTION-CODE        TO WS-SV-ACTION-CODE
                       MOVE PCT-ACTION-NBR         TO WS-SV-ACTION-NBR
                       MOVE PCT-EFFECT-DATE        TO WS-SV-EFFECT-DATE
                   END-IF
                   END-IF
               END-IF
               END-IF
           END-IF.
020000
020100     IF (NOT PARAMETERS-OK)
020200         GO TO 050-EDIT-PARAMETERS-END.
020300
020700     IF (PRM-RUN-OPTION = 1)
020800         PERFORM 051-EDIT-APPL-ACTION
020900         THRU    051-END
021000     ELSE
021100     IF (PRM-RUN-OPTION = 2)
021200         PERFORM 052-EDIT-EMPL-ACTION
021300         THRU    052-END
021400     ELSE
021500     IF (PRM-RUN-OPTION = 3)
021600         PERFORM 056-EDIT-POS-ACTION
021700         THRU    056-END
021800     ELSE
021900     IF (PRM-RUN-OPTION = 4)
022000         PERFORM 053-EDIT-APP-EMP-POS-ACTION
022100         THRU    053-END
022200         IF (WS-AEP-ACT-NOTFOUND)
                   SET WS-ACT-NOTFOUND TO TRUE
022400         ELSE
                   SET WS-ACT-FOUND TO TRUE
022600         END-IF
022700     END-IF.
023500
023600     SET NO-RECORD-FOUND TO TRUE.
023700     GO TO 050-EDIT-PARAMETERS-END.
023800
023900*****************************************************************
024000 051-EDIT-APPL-ACTION.
024100*****************************************************************
024200
           MOVE "A"                    TO DB-ACTION-TYPE.
           MOVE ZEROES                 TO DB-EFFECT-DATE.
           MOVE WS-PRM-EFFECT-DATE     TO DBRNG-EFFECT-DATE.
           MOVE PCTSET1-EFFECT-DATE    TO WS-DB-SUB-RNG.
           PERFORM 850-FIND-SUBRNG-PCTSET1.
024600
024700     IF (PERSACTION-NOTFOUND)
               SET WS-ACT-NOTFOUND TO TRUE
025100         GO TO 051-END
025200     ELSE
               SET WS-ACT-FOUND TO TRUE
           END-IF.
025400
025500     IF  (PRM-APL-PROC-LEVEL = SPACES)
           AND (PRM-APL-PROC-GRP   = SPACES)
           AND (PRM-USER-ID        = SPACES)
025600         SET WS-PL-FOUND TO TRUE
025700         GO TO 051-END
025800     ELSE
025900         SET WS-PL-NOTFOUND TO TRUE
026000         PERFORM 080-CHECK-PROCESS-LEVEL
026100         THRU    080-END
026200             UNTIL (PERSACTION-NOTFOUND)
026500             OR    (WS-PL-FOUND)
026600         IF (WS-PL-NOTFOUND)
                   SET WS-ACT-NOTFOUND TO TRUE
026800             GO TO 051-END.
026900
027000 051-END.
027100
027200*****************************************************************
027300 052-EDIT-EMPL-ACTION.
027400*****************************************************************
027500
           MOVE "E"                    TO DB-ACTION-TYPE.
           MOVE ZEROES                 TO DB-EFFECT-DATE.
           MOVE WS-PRM-EFFECT-DATE     TO DBRNG-EFFECT-DATE.
           MOVE PCTSET1-EFFECT-DATE    TO WS-DB-SUB-RNG.
           PERFORM 850-FIND-SUBRNG-PCTSET1.
027900
028000     IF (PERSACTION-NOTFOUND)
               SET WS-ACT-NOTFOUND TO TRUE
028400         GO TO 052-END.
028500
028600     IF (WS-SPEC-ACT-FOUND)
028700         PERFORM 060-EDIT-ACTIONS
028800         THRU    060-END
028900             UNTIL  (PERSACTION-NOTFOUND)
029100             OR     (WS-ACT-FOUND)
029200         IF (WS-ACT-NOTFOUND)
                   SET WS-ACT-NOTFOUND TO TRUE
029400             GO TO 052-END
029500         END-IF
029600     ELSE
               SET WS-ACT-FOUND TO TRUE
           END-IF.
029800
029900     IF  (PRM-PROCESS-LEVEL = SPACES)
029900     AND (PRM-EMP-PROC-GRP  = SPACES)
029900     AND (PRM-GROUP-NAME    = SPACES)
029900     AND (PRM-EMPLOYEE      = ZEROES)
           AND (PRM-USER-ID       = SPACES)
               SET WS-PL-FOUND TO TRUE
030300         GO TO 052-END
030400     ELSE
               SET WS-PL-NOTFOUND TO TRUE
030600         PERFORM 080-CHECK-PROCESS-LEVEL
030700         THRU    080-END
030800             UNTIL (PERSACTION-NOTFOUND)
031100             OR    (WS-PL-FOUND)
031200         IF (WS-PL-NOTFOUND)
                   SET WS-ACT-NOTFOUND TO TRUE
031400             GO TO 052-END.
031500
031600 052-END.
031700
031800*****************************************************************
031900 053-EDIT-APP-EMP-POS-ACTION.
032000*****************************************************************
032100
           SET WS-AEP-ACT-NOTFOUND TO TRUE.
032300     PERFORM 051-EDIT-APPL-ACTION
032400     THRU    051-END.
032500     IF  (WS-ACT-FOUND)
032600     AND (WS-PL-FOUND)
               SET WS-AEP-ACT-FOUND TO TRUE
032800         GO TO 053-END.
032900
           SET WS-ACT-NOTFOUND TO TRUE.
           SET WS-PL-NOTFOUND TO TRUE.
033200     PERFORM 052-EDIT-EMPL-ACTION
033300     THRU    052-END.
033400     IF  (WS-ACT-FOUND)
033500     AND (WS-PL-FOUND)
               SET WS-AEP-ACT-FOUND TO TRUE
033700         GO TO 053-END.
033800
           SET WS-ACT-NOTFOUND TO TRUE.
           SET WS-PL-NOTFOUND TO TRUE.
034100     PERFORM 056-EDIT-POS-ACTION
034200     THRU    056-END.
034300     IF  (WS-ACT-FOUND)
034400     AND (WS-PL-FOUND)
               SET WS-AEP-ACT-FOUND TO TRUE
034600         GO TO 053-END.
034700
034800 053-END.
034900
049500*****************************************************************
049600 056-EDIT-POS-ACTION.
049700*****************************************************************
049800
049900     MOVE "L"                    TO DB-ACTION-TYPE.
           MOVE ZEROES                 TO DB-EFFECT-DATE.
           MOVE WS-PRM-EFFECT-DATE     TO DBRNG-EFFECT-DATE.
           MOVE PCTSET1-EFFECT-DATE    TO WS-DB-SUB-RNG.
           PERFORM 850-FIND-SUBRNG-PCTSET1.
050200
050300     IF (PERSACTION-NOTFOUND)
               SET WS-ACT-NOTFOUND TO TRUE
050700         GO TO 056-END.
050800
050900     IF (WS-SPEC-ACT-FOUND)
051000         PERFORM 060-EDIT-ACTIONS
051100         THRU    060-END
051200             UNTIL  (PERSACTION-NOTFOUND)
051300             OR     (PCT-ACTION-TYPE NOT = DB-ACTION-TYPE)
051400             OR     (WS-ACT-FOUND)
051500         IF (WS-ACT-NOTFOUND)
                   SET WS-ACT-NOTFOUND TO TRUE
051700             GO TO 056-END
051800         END-IF
051900     ELSE
               SET WS-ACT-FOUND TO TRUE
           END-IF.
052100
052200     IF  (PRM-PROCESS-LEVEL = SPACES)
029900     AND (PRM-EMP-PROC-GRP  = SPACES)
029900     AND (PRM-GROUP-NAME    = SPACES)
029900     AND (PRM-EMPLOYEE      = ZEROES)
           AND (PRM-USER-ID       = SPACES)
               SET WS-PL-FOUND TO TRUE
052600         GO TO 056-END
052700     ELSE
               SET WS-PL-NOTFOUND TO TRUE
052900         PERFORM 080-CHECK-PROCESS-LEVEL
053000         THRU    080-END
053100             UNTIL (PERSACTION-NOTFOUND)
053200             OR    (PCT-ACTION-TYPE NOT = DB-ACTION-TYPE)
053300             OR    (PCT-EFFECT-DATE > WS-PRM-EFFECT-DATE)
053400             OR    (WS-PL-FOUND)
053500         IF (WS-PL-NOTFOUND)
                   SET WS-ACT-NOTFOUND TO TRUE
053700             GO TO 056-END.
053800
053900 056-END.
054000
054100*****************************************************************
054200 060-EDIT-ACTIONS.
054300*****************************************************************
054400
054500     IF (PCT-ACTION-CODE = PRM-EMP-ACTION (1))
054600     OR (PCT-ACTION-CODE = PRM-EMP-ACTION (2))
054700     OR (PCT-ACTION-CODE = PRM-EMP-ACTION (3))
054800     OR (PCT-ACTION-CODE = PRM-EMP-ACTION (4))
054900     OR (PCT-ACTION-CODE = PRM-EMP-ACTION (5))
055000     OR (PCT-ACTION-CODE = PRM-EMP-ACTION (6))
055100         IF  (PCT-ACTION-TYPE = DB-ACTION-TYPE)
055200         AND (PCT-EFFECT-DATE NOT > WS-PRM-EFFECT-DATE)
                   SET WS-ACT-FOUND TO TRUE
055400         ELSE
055500             PERFORM 860-FIND-NXTRNG-PCTSET1
055600         END-IF
055700     ELSE
055800         PERFORM 860-FIND-NXTRNG-PCTSET1.
055900
056000 060-END.
056100
056200*****************************************************************
056300 080-CHECK-PROCESS-LEVEL.
056400*****************************************************************
056500
056600     IF (PCT-ACTION-TYPE = "A")
               IF  (PCT-ACTION-NBR NOT = 1)
                   PERFORM 860-FIND-NXTRNG-PCTSET1
                   GO TO 080-END
               END-IF
               IF  (PRM-APPLICANT NOT = ZEROES)
               AND (PRM-APPLICANT NOT = PCT-EMPLOYEE)
                   PERFORM 860-FIND-NXTRNG-PCTSET1
                   GO TO 080-END
               END-IF
               IF  (PRM-USER-ID NOT = SPACES)
               AND (PCT-USER-ID NOT = PRM-USER-ID)
                   PERFORM 860-FIND-NXTRNG-PCTSET1
                   GO TO 080-END
               END-IF
               IF  (PRM-APL-PROC-LEVEL = SPACES)
               AND (PRM-APL-PROC-GRP   = SPACES)
               AND (PCT-USER-ID        = PRM-USER-ID)
                   SET WS-PL-FOUND TO TRUE
                   GO TO 080-END
               END-IF
               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > 36)
                   OR     (WS-PL-FOUND)
                   IF (PCT-FLD-NBR (I1)   = HREMP-PROCESS-LEVEL-DN)
                       IF  (PRM-APL-PROC-LEVEL NOT = SPACES)
J65420                 AND ((PCT-NEW-VALUE (I1) = PRM-APL-PROC-LEVEL)
J65420                 OR  ((PCT-PROCESS-LEVEL  = PRM-APL-PROC-LEVEL)
J65420                 AND (PCT-NEW-VALUE (I1) = SPACES)))
                           SET WS-PL-FOUND TO TRUE
                       ELSE
                       IF (PRM-APL-PROC-GRP NOT = SPACES)
                           MOVE PRM-COMPANY        TO DB-COMPANY
                           MOVE PRM-APL-PROC-GRP   TO DB-PROC-GROUP
                           MOVE PCT-NEW-VALUE (I1) TO DB-PROCESS-LEVEL
                           PERFORM 840-FIND-PRPSET1
                           IF (PRPROCGRP-FOUND)
                               SET WS-PL-FOUND TO TRUE
                           END-IF
                       END-IF
                       END-IF
                   END-IF
               END-PERFORM
056700         IF (WS-PL-NOTFOUND)
057100             PERFORM 860-FIND-NXTRNG-PCTSET1
               END-IF
057200         GO TO 080-END.
057300
057400     IF (PCT-ACTION-TYPE = "E" OR "L")
               IF (PRM-USER-ID NOT = SPACES)
                   IF (PRM-USER-ID = PCT-USER-ID)
                       SET WS-PL-FOUND TO TRUE
                   ELSE
                       PERFORM 860-FIND-NXTRNG-PCTSET1
                       GO TO 080-END
                   END-IF
               END-IF
               IF (PRM-EMPLOYEE NOT = ZEROES)
                   IF (PCT-EMPLOYEE = PRM-EMPLOYEE)
                       IF (PRM-EMP-ACTION-NBR = ZEROES)
                           SET WS-PL-FOUND TO TRUE
                       ELSE
                       IF (PRM-EMP-ACTION-NBR = PCT-ACTION-NBR)
                           SET WS-PL-FOUND TO TRUE
                       END-IF
                       END-IF
                   END-IF
               END-IF

               IF (PRM-GROUP-NAME NOT = SPACES)
                   MOVE PRM-COMPANY    TO DB-COMPANY
                   MOVE PCT-EMPLOYEE   TO DB-EMPLOYEE
                   MOVE PRM-GROUP-NAME TO DB-GROUP-NAME
                   PERFORM 840-FIND-PGESET1
                   IF (PGEMPLOYEE-FOUND)
                       SET WS-PL-FOUND TO TRUE
                   END-IF
               END-IF

               IF (PRM-PROCESS-LEVEL NOT = SPACES)
                   MOVE PRM-COMPANY       TO DB-COMPANY
                   MOVE PRM-PROCESS-LEVEL TO DB-PROCESS-LEVEL
                   MOVE PCT-EMPLOYEE      TO DB-EMPLOYEE
                   PERFORM 840-FIND-EMPSET7
                   IF (EMPLOYEE-FOUND)
                       SET WS-PL-FOUND TO TRUE
                   END-IF
               END-IF

               IF (PRM-EMP-PROC-GRP NOT = SPACES)
057500             MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE
057600             PERFORM 840-FIND-EMPSET1
057700             IF (EMPLOYEE-FOUND)
                       MOVE PRM-COMPANY        TO DB-COMPANY
                       MOVE PRM-EMP-PROC-GRP   TO DB-PROC-GROUP
                       MOVE EMP-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
                       PERFORM 840-FIND-PRPSET1
                       IF (PRPROCGRP-FOUND)
                           SET WS-PL-FOUND TO TRUE
                       END-IF
                   END-IF
               END-IF
056700         IF (WS-PL-NOTFOUND)
057100             PERFORM 860-FIND-NXTRNG-PCTSET1
               END-IF
057200         GO TO 080-END.
058200
058300 080-END.
058400
058500*****************************************************************
058600 050-EDIT-PARAMETERS-END.
058700*****************************************************************
058800*****************************************************************
058900 100-PROGRAM-CONTROL             SECTION.
059000*****************************************************************
059100 100-START.
059200
ACS002     PERFORM 1000-OPEN-WORKFLOW-DB.
ACS002
P83383     SET PREDM-OBJ-ID-OPTIMIZE   TO TRUE.
P63874     SET PAPEP-OBJ-ID-OPTIMIZE   TO TRUE.
           INITIALIZE WS-USED-BY-GROUP-FLDS.
059300     MOVE 110                TO CRT-MSG-NBR.
059400     PERFORM 780-DISPLAY-MSG.
059500
           MOVE HREMP-USER-LEVEL-DN    TO DB-FLD-NBR.
           PERFORM 840-FIND-PADSET1.
           MOVE PAD-ITEM-NAME          TO CRT-PHRASE.
           MOVE WS-PHRASE-SIZE         TO CRT-PHRASE-SIZE.
           MOVE "N"                    TO CRT-PUT-DOTS.
           PERFORM 900-GET-PHRASE-XLT.
           MOVE CRT-PHRASE-XLT         TO WS-LEVEL-NAME.

           MOVE HREMP-PROCESS-LEVEL-DN TO DB-FLD-NBR.
           PERFORM 840-FIND-PADSET1.
           MOVE PAD-ITEM-NAME          TO CRT-PHRASE.
           MOVE WS-PHRASE-SIZE         TO CRT-PHRASE-SIZE.
           MOVE "N"                    TO CRT-PUT-DOTS.
           PERFORM 900-GET-PHRASE-XLT.
           MOVE CRT-PHRASE-XLT         TO WS-PL-LABEL.

           MOVE HREMP-DEPARTMENT-DN    TO DB-FLD-NBR.
           PERFORM 840-FIND-PADSET1.
           MOVE PAD-ITEM-NAME          TO CRT-PHRASE.
           MOVE WS-PHRASE-SIZE         TO CRT-PHRASE-SIZE.
           MOVE "N"                    TO CRT-PUT-DOTS.
           PERFORM 900-GET-PHRASE-XLT.
           MOVE CRT-PHRASE-XLT         TO WS-DEPT-LABEL.

           OPEN OUTPUT ERRORS-FILE.
           SET NO-ERRORS-PRINTED TO TRUE.
           MOVE WS-COMPANY-NAME        TO REG5-PRS-NAME.
           MOVE PRM-COMPANY            TO REG5-PCT-COMPANY.
           MOVE PRM-UPDATE-OPTION      TO REG5-UPDATE-OPTION.
           MOVE REGN5-PCT-COMPANY      TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

           OPEN OUTPUT WARNINGS-FILE.
           SET NO-WARNINGS-PRINTED TO TRUE.
           MOVE WS-COMPANY-NAME        TO RWG5-PRS-NAME.
           MOVE PRM-COMPANY            TO RWG5-PCT-COMPANY.
           MOVE PRM-UPDATE-OPTION      TO RWG5-UPDATE-OPTION.
           MOVE RWGN5-PCT-COMPANY      TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.
060700
060800     OPEN OUTPUT HISTAUDIT-FILE.
           SET NO-HISTAUDITS-PRINTED TO TRUE.
060900     MOVE WS-COMPANY-NAME        TO R4G6-PRS-NAME.
061000     MOVE PRM-COMPANY            TO R4G6-PCT-COMPANY.
061600     MOVE PRM-UPDATE-OPTION      TO R4G6-UPDATE-OPTION.
061700     MOVE R4GN6-HISTORY-AUDIT    TO RPT-GROUP-REQUEST.
061800     PERFORM 700-PRINT-RPT-GRP.

J83708     IF  (PRM-UPDATE-OPTION = "Y")
J83708         PERFORM 1000-OPEN-WORKFLOW-DB.
061900
J21883* LOAD RESTART DATA - CHECK FOR PHASE 4.
J21883     PERFORM 840-FIND-CKPSET1.
J21883     IF  (CKPOINT-FOUND)
J21883         MOVE CKP-RESTART-INFO           TO WS-RESTART-INFO
J73509         DISPLAY "APP-WORK-NAME "  WS-RESTART-AW-NAME
J73509         DISPLAY "PA100FLDS-FILE " WS-RESTART-WRK-NAME 
J21883     END-IF.
J21883     IF  (WS-RESTART-PHASE-A NOT NUMERIC)
J21883         MOVE ZEROES                     TO WS-RESTART-PHASE
J21883     END-IF.
J21883     IF  (WS-RESTART-PHASE = 4)
J21883         IF (WS-RESTART-WRK-NAME NOT = SPACES)
J21883             MOVE WS-RESTART-WRK-NAME TO WS-PA100FLDS-NAME
J21883         END-IF
J21883         GO TO 100-PHASE-4
J21883     END-IF.
062000     IF  (WS-ACT-NOTFOUND)
P69186     AND (PRM-RUN-OPTION NOT = 7)
               SET ERRORS-PRINTED TO TRUE
               MOVE 324                        TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE                TO REG5A-NODATA-MSG
062400         MOVE REGN5A-PCT-ACTION-ERROR    TO RPT-GROUP-REQUEST
062500         PERFORM 700-PRINT-RPT-GRP
062600         GO TO 100-CLEANUP.

           MOVE REGN5B-PCT-ACTION-ERROR    TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.
           MOVE RWGN5B-PCT-ACTION-ERROR    TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

           IF (PRM-RUN-OPTION              = 1 OR 2 OR 3 OR 4)
               SET NO-REQDEDS-PRINTED TO TRUE
060800         OPEN OUTPUT PRREQDED-FILE
               OPEN OUTPUT TAXLOCATOR-FILE
               MOVE PRM-UPDATE-OPTION      TO R7G1-UPDATE-OPTION
               MOVE WS-COMPANY-NAME        TO R7G1-PRS-NAME
               MOVE PRM-COMPANY            TO R7G1-PCT-COMPANY
           END-IF.

P58564     MOVE 332                        TO CRT-MSG-NBR.
P58564     PERFORM 790-GET-MSG.
P58564     MOVE CRT-MESSAGE                TO WS-HREMP-MSG-332.
P58564     MOVE 329                        TO CRT-MSG-NBR.
P58564     PERFORM 790-GET-MSG.
P65316     MOVE CRT-MESSAGE                TO WS-HREMP-MSG-329.
P58564     MOVE 111                        TO CRT-MSG-NBR.
P58564     PERFORM 790-GET-MSG.
P65316     MOVE CRT-MESSAGE                TO WS-HREMP-MSG-111.
P58564     MOVE SPACES                     TO CRT-MESSAGE.
P58564     MOVE ZEROES                     TO CRT-MSG-NBR.
P58564
063200     IF (PRM-RUN-OPTION = 1)
063500         PERFORM 1000-PROCESS-APPL-ACTION
063600     ELSE
063700     IF (PRM-RUN-OPTION = 2)
063800         MOVE "E"                    TO DB-ACTION-TYPE
063900         MOVE PCTSET1-ACTION-TYPE    TO WS-DB-BEG-RNG
064000         PERFORM 2000-PROCESS-EMPL-ACTION
064100     ELSE
064200     IF (PRM-RUN-OPTION = 3)
064300         MOVE "L"                    TO DB-ACTION-TYPE
064400         MOVE PCTSET1-ACTION-TYPE    TO WS-DB-BEG-RNG
064500         PERFORM 1800-PROCESS-POS-ACTION
064600     ELSE
064700     IF (PRM-RUN-OPTION = 4)
078854*********for latest persaction flds value
078854         PERFORM 900-BUILD-TMP-FILE-NAME
078854         MOVE WS-TMP-FILE             TO PA100WS-LATEST-FILENAME
078854         OPEN OUTPUT PA100LAT-FILE
078854         CLOSE       PA100LAT-FILE
078854         OPEN I-O    PA100LAT-FILE
078854***
065000         PERFORM 1000-PROCESS-APPL-ACTION
               INITIALIZE WS-SV-EMPLOYEE-PRT
065100         MOVE "E"                    TO DB-ACTION-TYPE
065200         MOVE PCTSET1-ACTION-TYPE    TO WS-DB-BEG-RNG
065300         PERFORM 2000-PROCESS-EMPL-ACTION
               INITIALIZE WS-SV-EMPLOYEE-PRT
065400         MOVE "L"                    TO DB-ACTION-TYPE
065500         MOVE PCTSET1-ACTION-TYPE    TO WS-DB-BEG-RNG
065600         PERFORM 1800-PROCESS-POS-ACTION
065700     ELSE
065800     IF (PRM-RUN-OPTION = 5)
065900         MOVE "M"                    TO DB-ACTION-TYPE
066000         MOVE PCTSET1-ACTION-TYPE    TO WS-DB-BEG-RNG
P58564         MOVE ZEROES                 TO WS-MAX-OPS-COUNT
066100         PERFORM 3000-PROCESS-MASS-CHANGE
066200     ELSE
066300     IF (PRM-RUN-OPTION = 6)
066400         MOVE "P"                    TO DB-ACTION-TYPE
066500         MOVE PCTSET1-ACTION-TYPE    TO WS-DB-BEG-RNG
               SET PRPXL-UPDATES-BYPASS TO TRUE
P58564         MOVE ZEROES                 TO WS-MAX-OPS-COUNT
P69186         PERFORM 4000-PROCESS-PAY-CHANGE
P69186     ELSE
P69186     IF (PRM-RUN-OPTION = 7)
               MOVE WS-COMPANY-NAME            TO RHLD1-PRS-NAME
               MOVE PRM-COMPANY                TO RHLD1-PCT-COMPANY
               MOVE RHLD1-HELD-COMPANY         TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
P69186         MOVE PCTSET1-COMPANY            TO WS-DB-BEG-RNG
P69186         MOVE SPACES                     TO FILTER-STRING
P69186         MOVE "(PCT-HOLD-FLAG = ?)"      TO FILTER-STRING
P69186         PERFORM 890-CREATE-FILTER
P69186         MOVE "Y"                        TO ALPHANUM-FILTER-VALUE
P69186         PERFORM 890-SET-ALPHANUM-FILTER-VALUE
P69186
P69186         PERFORM 850-FILTER-BEGRNG-PCTSET1
P69186         IF (PERSACTION-NOTFOUND)
P69186             MOVE 341                    TO CRT-MSG-NBR
P69186             PERFORM 790-GET-MSG
P69186             MOVE CRT-MESSAGE            TO RHLD4-NO-HOLD-MSG
                   MOVE RHLD4-MESSAGE          TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
P69186             GO TO 100-END
P69186         ELSE
P69186             PERFORM 7000-PROCESS-ON-HOLD
P69186                 UNTIL (PERSACTION-NOTFOUND).
P69186
           IF (PRM-RUN-OPTION              = 1 OR 2 OR 3 OR 4)
               CLOSE PRREQDED-FILE
               CLOSE TAXLOCATOR-FILE
           END-IF.        
          
J21883 100-PHASE-4.
030400     IF  (PRM-RUN-OPTION         = 2 OR 3 OR 4 OR 5 OR 6)
           AND (PRM-UPDATE-OPTION      = "Y")
               PERFORM 910-AUDIT-BEGIN
               PERFORM 840-MODIFY-CKPSET1
               INITIALIZE WS-RESTART-RECORD
P69000                    WS-RESTART-EMPLOYEE
715100         MOVE 4                  TO WS-RESTART-PHASE
               MOVE WS-RESTART-INFO    TO CKP-RESTART-INFO
               PERFORM 820-STORE-CKPOINT
               PERFORM 900-SAVE-PRINT-FILES
               PERFORM 925-AUDIT-END

               PERFORM 910-AUDIT-BEGIN

J73509         SORT PA100SORT-FILE   
J73509             ASCENDING KEY   DSF1-COMPANY
J73509                             DSF1-EMPLOYEE     
J73509                             DSF1-EFFECT-DATE
J73509                             DSF1-ACTION-CODE
J73509                             DSF1-ACTION-NBR
J73509                             DSF1-FLD-NBR    
J73509                       USING PA100FLDS-FILE
J73509                      GIVING PA100FLDS-FILE SAVE
               OPEN INPUT PA100FLDS-FILE

               PERFORM 5500-UPDATE-GROUPS
               
               CLOSE PA100FLDS-FILE

               PERFORM 925-AUDIT-END

201940         MOVE WS-PA100FLDS-NAME  TO WS-TMP-FILE
201950         PERFORM 901-REMOVE-TMP-FILE.
 
J13033     CLOSE ERRORS-FILE.
J13033
J13033     CLOSE WARNINGS-FILE.
J13033
J13033     CLOSE HISTAUDIT-FILE.

       100-CLEANUP.      

066800 100-END.
068400*****************************************************************
068500 1000-PROCESS-APPL-ACTION        SECTION.
068600*****************************************************************
068700 1000-START.
068800
           SET HRSEC-NO-FLD-SECURITY TO TRUE.

068900     MOVE WS-COMPANY-NAME        TO R1G1-PRS-NAME
068900                                    R6G1-PRS-NAME.
069000     MOVE PRM-COMPANY            TO R1G1-PCT-COMPANY
068900                                    R6G1-PCT-COMPANY.
069100
P58564     MOVE WS-HREMP-MSG-111       TO R1G1-TITLE.
069500
           MOVE PRM-UPDATE-OPTION      TO R1G1-UPDATE-OPTION
                                          R6G1-UPDATE-OPTION.

           PERFORM 1500-CHECK-FOR-RESTART.

           IF (WS-RESTART-PHASE-A NOT NUMERIC)
               MOVE ZEROES             TO WS-RESTART-PHASE.

J21883* IF REPORT OPTION 4 AND APPLICANT ALREADY PROCESSED, SKIP APPLICANT.
J21883     IF  (WS-RESTART-WRK-NAME NOT = SPACES)
J21883     AND (PRM-RUN-OPTION = 4)
J21883         GO TO 1000-END
J21883     END-IF.

           IF  (WS-RESTART-PHASE > 1)
               GO TO 1000-PHASE-2.

           OPEN I-O APP-WORK-FILE.
           MOVE "A"                    TO DB-ACTION-TYPE.
           MOVE ZEROES                 TO DB-EFFECT-DATE.
           MOVE WS-PRM-EFFECT-DATE     TO DBRNG-EFFECT-DATE.
           MOVE PCTSET1-EFFECT-DATE    TO WS-DB-SUB-RNG.
           PERFORM 850-FIND-SUBRNG-PCTSET1.
           PERFORM
               UNTIL (PERSACTION-NOTFOUND)
               IF  (PCT-ACTION-NBR = 1)

                   IF   (PRM-USER-ID  = SPACES)
                   OR  ((PRM-USER-ID  NOT = SPACES)
                   AND  (PRM-USER-ID  = PCT-USER-ID))
                       IF  (PRM-APPLICANT    NOT = ZEROES)
                       AND (PRM-APPLICANT    = PCT-EMPLOYEE)
                           SET RECORD-FOUND TO TRUE
P64381                 ELSE
P64381                     SET NO-RECORD-FOUND TO TRUE
                       END-IF

                       IF  (PRM-APL-PROC-LEVEL = SPACES)
                       AND (PRM-APL-PROC-GRP   = SPACES)
                       AND (PRM-APPLICANT      = ZEROES)
                           SET RECORD-FOUND TO TRUE
                       ELSE
                       IF  (PRM-APPLICANT    = ZEROES)
                
                           SET NO-RECORD-FOUND TO TRUE
                           PERFORM
                               VARYING I1 FROM 1 BY 1
                               UNTIL  (I1 > 36)
                               OR     (RECORD-FOUND)
                               IF (PCT-FLD-NBR (I1) =
                                                HREMP-PROCESS-LEVEL-DN)
                                   IF  (PRM-APL-PROC-LEVEL NOT = SPACES)
                                   AND (PCT-NEW-VALUE (I1)
                                                   = PRM-APL-PROC-LEVEL)
                                       SET RECORD-FOUND TO TRUE
J65420                             ELSE
J65420                             IF  (PRM-APL-PROC-LEVEL NOT = SPACES)
J65420                             AND (PCT-PROCESS-LEVEL
J65420                                             = PRM-APL-PROC-LEVEL)
J65420                             AND (PCT-NEW-VALUE (I1)
J65420                                             = SPACES)
J65420                                 SET RECORD-FOUND TO TRUE  
                                   ELSE
                                   IF (PRM-APL-PROC-GRP NOT = SPACES)
                                       MOVE PRM-COMPANY  TO DB-COMPANY
                                       MOVE PRM-APL-PROC-GRP
                                                     TO DB-PROC-GROUP
                                       MOVE PCT-NEW-VALUE (I1)
                                                     TO DB-PROCESS-LEVEL
                                       PERFORM 840-FIND-PRPSET1
                                       IF (PRPROCGRP-FOUND)
                                           SET RECORD-FOUND TO TRUE
                                       END-IF
                                   END-IF
                                   END-IF
                               END-IF
                           END-PERFORM
                       END-IF
                       END-IF
                       IF  (RECORD-FOUND)
                           MOVE PCT-EMPLOYEE    TO AW-APPLICANT
                           MOVE PCT-REQUISITION TO AW-REQUISITION
                           WRITE APP-WORK-REC
                       END-IF
                   END-IF
               END-IF
               PERFORM 860-FIND-NXTRNG-PCTSET1
           END-PERFORM.
           CLOSE APP-WORK-FILE.

           IF  ((PRM-UPDATE-OPTION   = "Y")
J00148     AND  (DIFF-VALUE)
J00148     AND  (PRM-RUN-OPTION  NOT = "1"))
J00148     OR  ((PRM-UPDATE-OPTION   = "Y")
J00148     AND  (PRM-RUN-OPTION      = "1"))
               MOVE 2                  TO WS-RESTART-PHASE
               PERFORM 910-AUDIT-BEGIN
               PERFORM 840-MODIFY-CKPSET1
               MOVE WS-RESTART-INFO    TO CKP-RESTART-INFO
               PERFORM 820-STORE-CKPOINT
               PERFORM 900-SAVE-PRINT-FILES
               PERFORM 925-AUDIT-END
           END-IF.

       1000-PHASE-2.
           IF (WS-RESTART-PHASE-A NOT NUMERIC)
               MOVE ZEROES             TO WS-RESTART-PHASE.

           IF  (WS-RESTART-PHASE > 2)
               GO TO 1000-END.

           INITIALIZE HRSEC-WORK-COUNTRY.

           OPEN INPUT APP-WORK-FILE.
           READ APP-WORK-FILE NEXT RECORD
               AT END
                   IF (PRM-RUN-OPTION = 1)
                       SET ERRORS-PRINTED TO TRUE
                       MOVE 324                    TO CRT-MSG-NBR
                       PERFORM 790-GET-MSG
                       MOVE CRT-MESSAGE            TO REG5A-NODATA-MSG
                       MOVE REGN5A-PCT-ACTION-ERROR
                                                   TO RPT-GROUP-REQUEST
                       PERFORM 700-PRINT-RPT-GRP
                   END-IF
                   GO TO 1000-PHASE-2-CONTINUE.

072400     IF ((PRM-UPDATE-OPTION   = "Y")
J00148     AND (DIFF-VALUE)
J00148     AND (PRM-RUN-OPTION  NOT = "1"))
J00148     OR ((PRM-UPDATE-OPTION   = "Y")
J00148     AND (PRM-RUN-OPTION      = "1")) 
072500         PERFORM 910-AUDIT-BEGIN.

           SET RECORD-FOUND TO TRUE.
071400
071500     IF  (RECORD-FOUND)
071800         MOVE R1GN1-PCT-COMPANY      TO RPT-GROUP-REQUEST
071900         PERFORM 700-PRINT-RPT-GRP
071800         MOVE R6GN1-PCT-COMPANY      TO RPT-GROUP-REQUEST
071900         PERFORM 700-PRINT-RPT-GRP
               MOVE R7GN1-PCT-COMPANY      TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
072000         PERFORM 1400-PROCESS-APPLICANT
072100             UNTIL (NO-RECORD-FOUND).
072300
072400     IF ((PRM-UPDATE-OPTION   = "Y")
J00148     AND (DIFF-VALUE)
J00148     AND (PRM-RUN-OPTION  NOT = "1"))
J00148     OR ((PRM-UPDATE-OPTION   = "Y")
J00148     AND (PRM-RUN-OPTION      = "1"))
               PERFORM 900-SAVE-PRINT-FILES
072500         PERFORM 925-AUDIT-END.

       1000-PHASE-2-CONTINUE.

           CLOSE APP-WORK-FILE.

           MOVE WS-APP-WORK-NAME  TO WS-TMP-FILE.
           PERFORM 901-REMOVE-TMP-FILE.

           IF ((PRM-UPDATE-OPTION  = "Y")
J00148     AND (DIFF-VALUE)
J00148     AND (PRM-RUN-OPTION NOT = "1"))
J00148     OR ((PRM-UPDATE-OPTION  = "Y")
J00148     AND (PRM-RUN-OPTION     = "1"))
               PERFORM 910-AUDIT-BEGIN
               PERFORM 840-MODIFY-CKPSET1
               INITIALIZE WS-RESTART-RECORD
P69000                    WS-RESTART-EMPLOYEE
               MOVE 3                  TO WS-RESTART-PHASE
               MOVE WS-RESTART-INFO    TO CKP-RESTART-INFO
               PERFORM 820-STORE-CKPOINT
               PERFORM 900-SAVE-PRINT-FILES
               PERFORM 925-AUDIT-END
           END-IF.           
           
072700 1000-END.
072800*****************************************************************
072900 1400-PROCESS-APPLICANT          SECTION.
073000*****************************************************************
073100 1400-START.
073200
           IF (WS-RESTART-RECORD-A NOT NUMERIC)
               MOVE ZEROES             TO WS-RESTART-RECORD
           END-IF.
           IF  (PRM-UPDATE-OPTION = "Y")
           AND (AW-APPLICANT <= WS-RESTART-RECORD)
               GO TO 1400-NEXT.            
            
           MOVE PRM-COMPANY               TO DB-COMPANY.
           MOVE AW-APPLICANT              TO DB-APPLICANT.
           PERFORM 840-FIND-APLSET1.

           INITIALIZE HREMP-SCR-FIELDS
                      HRPEM-SCR-FIELDS.

J73281     MOVE PRM-COMPANY               TO PAAPL-COMPANY.
J73281     MOVE ZEROES                    TO PAAPL-EMPLOYEE.
J73281     MOVE AW-APPLICANT              TO PAAPL-APPLICANT.
J73281     PERFORM 7010-GET-LEM-DATA.

           PERFORM 1410-MOVE-APL-TO-WS.

           SET NO-ACTION-USERFIELDS TO TRUE.
           SET NOT-EDITING-USERFIELDS TO TRUE.
           SET SINGLE-ACTION TO TRUE.
           MOVE "A"                    TO DB-ACTION-TYPE.
           MOVE AW-APPLICANT           TO DB-EMPLOYEE.
           MOVE PCTSET2-EMPLOYEE       TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PCTSET2.
           SET NO-WORKFLOW-ERROR       TO TRUE.
           PERFORM 1420-MOVE-PCT-TO-WS
               UNTIL (PERSACTION-NOTFOUND).

           IF (WORKFLOW-ERROR)
               GO TO 1400-WF-NEXT.

P69186     IF (WS-HOLD-FLAG = "Y")
               SET ERRORS-PRINTED          TO TRUE
               MOVE "A"                    TO REG6-APP
               MOVE AW-APPLICANT           TO REG6-EMPLOYEE
               MOVE HREMP-ACTION-CODE      TO REG6-PCT-ACTION-CODE
               MOVE HREMP-ACTION-NBR       TO REG6-PCT-ACTION-NBR
               MOVE "A"                    TO REG6-ACTION-TYPE
               MOVE 1                      TO REG6-PCT-POS-LEVEL
               MOVE HREMP-EFFECT-DATE      TO REG6-PCT-EFFECT-DATE
               MOVE HREMP-EMPLOYEE         TO REG6-EMPLOYEE
               MOVE HREMP-LAST-NAME        TO HRWS-LAST-NAME
               MOVE HREMP-FIRST-NAME       TO HRWS-FIRST-NAME
               MOVE HREMP-MIDDLE-INIT      TO HRWS-MIDDLE-INIT
               MOVE HREMP-LAST-NAME-PRE    TO HRWS-LAST-NAME-PRE
               MOVE HREMP-NAME-SUFFIX      TO HRWS-NAME-SUFFIX
               PERFORM 750-HR-FORMAT-NAME
               MOVE HRWS-FORMAT-NAME       TO REG6-EMP-NAME
               MOVE 167                    TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE            TO REG6-ERROR-DESC
               MOVE ERROR-LINE             TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
P69186         GO TO 1400-NEXT.
P69186
           INITIALIZE                     PADT-UPDPEP-DATE
                                          PADT-PEP-DATE.
075600
075800     MOVE ZEROS                  TO CRT-ERROR-NBR.
075900
076000     MOVE HREMP-COMPANY          TO DB-COMPANY.
076100     MOVE HREMP-EMP-STATUS       TO DB-EMP-STATUS.
076200     PERFORM 840-FIND-EMSSET1.
076300     IF (EMSTATUS-FOUND)
076400         MOVE EMS-COUNT          TO HREMP-COUNT.
076500
076600     INITIALIZE                     HREMP-UPDPEP-DATE.
076800     MOVE 1                      TO HREMP-POS-LEVEL.
           SET HREMP-EMP-PRE-ASSIGN TO TRUE.
076900     PERFORM 2000-HREMP-EDIT-TRAN.
           MOVE HREMP-CURRENCY-CODE    TO PAPCT-EMP-CURRENCY.
           MOVE HREMP-CURR-ND          TO PAPCT-EMP-CURR-ND.
077000
077100     IF (HREMP-UPDPEP-DATE = HREMP-EFFECT-DATE)
077200         INITIALIZE                 HREMP-UPDPEP-DATE.
077300
           INITIALIZE WS-USER-FIELD-VALUE-TABLE.
           IF  (ACTION-USERFIELDS)
               SET EDITING-USERFIELDS TO TRUE
               MOVE "A"                    TO DB-ACTION-TYPE
               MOVE AW-APPLICANT           TO DB-EMPLOYEE
               MOVE PCTSET2-EMPLOYEE       TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PCTSET2
               PERFORM 1420-MOVE-PCT-TO-WS
                   UNTIL (PERSACTION-NOTFOUND)
           END-IF.

           IF (PRM-CHECK-ALL-REQ = "Y")
P58564         IF (WS-HREMP-MSG-50 = SPACES) 
P58564             MOVE "HREMP"            TO CRT-ERROR-CAT
P58564             MOVE 50                 TO CRT-MSG-NBR
P58564             PERFORM 790-GET-MSG
P58564             MOVE CRT-MESSAGE        TO WS-HREMP-MSG-50
P58564             MOVE SPACES             TO CRT-MESSAGE
P58564             MOVE ZEROES             TO CRT-MSG-NBR
P58564         END-IF
               PERFORM 1430-CHECK-REQ-FIELDS
           END-IF.

           IF  (PRM-UPDATE-OPTION = "Y")
           AND (HREMP-NO-ERROR-FOUND)
               PERFORM 3000-HREMP-NUMBER.

       1400-WF-NEXT.
           MOVE HREMP-EMPLOYEE         TO R1G2A-PCT-EMPLOYEE
                                          R6G2A-PCT-EMPLOYEE
                                          R7G2A-PCT-EMPLOYEE.
           MOVE HREMP-LAST-NAME        TO HRWS-LAST-NAME.
           MOVE HREMP-FIRST-NAME       TO HRWS-FIRST-NAME.
           MOVE HREMP-MIDDLE-INIT      TO HRWS-MIDDLE-INIT.
           MOVE HREMP-LAST-NAME-PRE    TO HRWS-LAST-NAME-PRE.
           MOVE HREMP-NAME-SUFFIX      TO HRWS-NAME-SUFFIX.
           PERFORM 750-HR-FORMAT-NAME.
           MOVE HRWS-FORMAT-NAME       TO R1G2A-EMP-NAME
                                          R1G2B-EMP-NAME
                                          R6G2A-EMP-NAME
                                          R7G2A-EMP-NAME.

           MOVE APL-APPLICANT          TO R1G2A-PCT-APPLICANT
                                          R1G2B-PCT-APPLICANT
                                          R6G2A-PCT-APPLICANT
                                          R7G2A-PCT-APPLICANT.
           MOVE APL-LAST-NAME          TO HRWS-LAST-NAME.
           MOVE APL-FIRST-NAME         TO HRWS-FIRST-NAME.
           MOVE APL-MIDDLE-INIT        TO HRWS-MIDDLE-INIT.
           MOVE APL-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE.
           MOVE APL-NAME-SUFFIX        TO HRWS-NAME-SUFFIX.
           PERFORM 750-HR-FORMAT-NAME.
           MOVE HRWS-FORMAT-NAME       TO R1G2A-APL-NAME
                                          R1G2B-APL-NAME
                                          R6G2A-APL-NAME
                                          R7G2A-APL-NAME.

           MOVE AW-REQUISITION         TO R1G2A-PCT-REQUISITION
                                          R1G2B-PCT-REQUISITION
                                          R6G2A-PCT-REQUISITION
                                          R7G2A-PCT-REQUISITION.
           IF (AW-REQUISITION = ZEROES)
               MOVE SPACES             TO R1G2A-PJR-DESCRIPTION
                                           R1G2B-PJR-DESCRIPTION
                                           R6G2A-PJR-DESCRIPTION
                                           R7G2A-PJR-DESCRIPTION
           ELSE
               MOVE APL-COMPANY       TO DB-COMPANY
               MOVE AW-REQUISITION    TO DB-REQUISITION
               PERFORM 840-FIND-PJRSET1
               IF (PAJOBREQ-FOUND)
                   MOVE PJR-DESCRIPTION TO R1G2A-PJR-DESCRIPTION
                                           R1G2B-PJR-DESCRIPTION
                                           R6G2A-PJR-DESCRIPTION
                                           R7G2A-PJR-DESCRIPTION
               END-IF.

           MOVE "A"                    TO WS-ERR-ACTION-TYPE.
P73870     MOVE "PA100"                TO CRT-ERROR-CAT.
           IF  (SINGLE-ACTION)
               MOVE HREMP-ACTION-CODE      TO WS-ERR-ACTION-CODE
               MOVE HREMP-ACTION-NBR       TO WS-ERR-ACTION-NBR
           ELSE
               MOVE 336                    TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE            TO WS-ERR-ACTION-CODE
               INITIALIZE CRT-MESSAGE
               MOVE 0                      TO WS-ERR-ACTION-NBR
           END-IF.
           MOVE 1                      TO WS-ERR-POS-LEVEL.
           MOVE HREMP-EFFECT-DATE      TO WS-ERR-EFFECT-DATE.
           PERFORM 6000-PRINT-MESSAGES.

           IF  (HREMP-ERROR-FOUND)
               GO TO 1400-NEXT.

           IF  (HREMP-EMPLOYEE NOT = ZEROES)
               MOVE R1GN2A-PCT-APPLICANT   TO RPT-GROUP-REQUEST
           ELSE
               MOVE R1GN2B-PCT-APPLICANT   TO RPT-GROUP-REQUEST
           END-IF.
           PERFORM 700-PRINT-RPT-GRP.
           MOVE R6GN2A-PCT-APPLICANT   TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.
           MOVE R7GN2A-PCT-APPLICANT   TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

           MOVE "PA100"                TO CRT-ERROR-CAT.

           MOVE "A"                    TO DB-ACTION-TYPE.
           MOVE AW-APPLICANT           TO DB-EMPLOYEE.
           MOVE HREMP-ACTION-CODE      TO DB-ACTION-CODE.
           MOVE PCTSET2-ACTION-CODE    TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PCTSET2.
           PERFORM 1450-PRINT-ACTION-INFO.

           MOVE "A"                    TO DB-ACTION-TYPE.
           MOVE AW-APPLICANT           TO DB-EMPLOYEE.
           MOVE PCTSET2-EMPLOYEE       TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PCTSET2.
           PERFORM
               UNTIL (PERSACTION-NOTFOUND)
               IF  (PCT-ACTION-CODE NOT = HREMP-ACTION-CODE)
                   PERFORM 1450-PRINT-ACTION-INFO
               END-IF
               PERFORM 860-FIND-NXTRNG-PCTSET2
           END-PERFORM.

           IF  (TAX-LOCATOR)
               PERFORM 2670-PRINT-PXL.

084800     IF ((PRM-UPDATE-OPTION NOT = "Y")
J00148     OR  (SAME-VALUE)
J00148     AND (PRM-RUN-OPTION    NOT = "1"))
J00148     OR ((PRM-UPDATE-OPTION NOT = "Y")
J00148     AND (PRM-RUN-OPTION        = "1"))
084900         GO TO 1400-NEXT.
085000
085600     MOVE APL-COMPANY             TO PAPCT-COMPANY.
085700     MOVE APL-APPLICANT           TO PAPCT-APPLICANT.
085800     MOVE APL-WK-RST-CODE1        TO PAPCT-WK-RST-CODE1.
085900     MOVE APL-WK-RST-CODE2        TO PAPCT-WK-RST-CODE2.
086000     MOVE APL-WK-RST-CODE3        TO PAPCT-WK-RST-CODE3.
086100     MOVE APL-WK-RST-CODE4        TO PAPCT-WK-RST-CODE4.
087000
087300     PERFORM 840-MODIFY-APLSET1.
           MOVE HREMP-EMPLOYEE          TO APL-EMPLOYEE.
087400     MOVE HREMP-EFFECT-DATE       TO APL-DATE-HIRED.
087500     PERFORM 820-STORE-APPLICANT.

P63874     PERFORM 7500-GET-ACTION-OBJ-ID.
089300     MOVE IFOBIWS-OBJ-ID         TO WS-OBJ-ID.
089400
089500     PERFORM 800-CREATE-PERSACTHST.
089600     MOVE HREMP-COMPANY          TO PAH-COMPANY.
089700     MOVE "A"                    TO PAH-ACTION-TYPE.
090000     MOVE HREMP-ACTION-CODE      TO PAH-ACTION-CODE.
090100     MOVE HREMP-ACTION-NBR       TO PAH-ACTION-NBR.
090200     MOVE HREMP-EFFECT-DATE      TO PAH-EFFECT-DATE.
090300     MOVE HREMP-EMPLOYEE         TO PAH-EMPLOYEE.
090400     MOVE HREMP-ANT-END-DATE     TO PAH-ANT-END-DATE.
090500     MOVE HREMP-REASON1          TO PAH-REASON (1).
090600     MOVE HREMP-REASON2          TO PAH-REASON (2).
090700     MOVE HREMP-USER-ID          TO PAH-USER-ID.
090800     MOVE WS-SYSTEM-DATE-YMD     TO PAH-DATE-STAMP.
P80029     MOVE HHMMSS                 TO PAH-TIME-STAMP.
090900     MOVE WS-OBJ-ID              TO PAH-OBJ-ID.
           MOVE 1                      TO PAH-POS-LEVEL.
           MOVE WS-UPDATE-BENEFIT      TO PAH-UPDATE-BENEFIT.
           MOVE WS-UPD-ABS-MGMT        TO PAH-UPD-ABS-MGMT.
           MOVE WS-HIST-CORR-FLAG      TO PAH-HIST-CORR-FLAG.
           MOVE WS-UPDATE-REQ-DED      TO PAH-UPDATE-REQ-DED.
           MOVE WS-EDM-EFFECT-DT       TO PAH-EDM-EFFECT-DT.
           MOVE WS-EDM-END-DATE        TO PAH-EDM-END-DATE.
           MOVE "2"                    TO PAH-ACTION-UPD.
J08104     MOVE WS-CREATE-USER-ID      TO PAH-CREATE-USER.
J08104     MOVE WS-CREATE-DATE         TO PAH-CREATE-DATE.
J08104     MOVE WS-CREATE-TIME         TO PAH-CREATE-TIME.
092700     PERFORM 820-STORE-PERSACTHST.
094600
095500     MOVE ZEROS                  TO CRT-ERROR-NBR.
095600     INITIALIZE                     HREMP-UPDPEP-DATE.
095900     MOVE WS-OBJ-ID              TO HREMP-ACT-OBJ-ID.

           PERFORM 1405-SET-LOG
           THRU    1405-END.
P85139     INITIALIZE HRWS-FIELD-NBR-TABLE
P85139                I2.
P85139     MOVE HREMP-COMPANY          TO DB-COMPANY.
P85139     MOVE "A"                    TO DB-ACTION-TYPE.
P85139     MOVE AW-APPLICANT           TO DB-EMPLOYEE.
P85139     MOVE HREMP-ACTION-CODE      TO DB-ACTION-CODE.
P85139     MOVE PCTSET2-ACTION-CODE    TO WS-DB-BEG-RNG.
P85139     PERFORM 850-MODIFY-BEGRNG-PCTSET2.
P85139
P85139     IF (WS-HREMP-MSG-335 = SPACES)
P85139         MOVE 335                TO CRT-MSG-NBR
P85139         PERFORM 790-GET-MSG
P85139         MOVE CRT-MESSAGE        TO WS-HREMP-MSG-335
P85139         MOVE SPACES             TO CRT-MESSAGE
P85139         MOVE ZEROES             TO CRT-MSG-NBR
P85139     END-IF.
P85139
P85139     PERFORM 1460-UPDATE-ACTION-INFO.

P85139     MOVE "A"                    TO DB-ACTION-TYPE.
P85139     MOVE AW-APPLICANT           TO DB-EMPLOYEE.
P85139     MOVE PCTSET2-EMPLOYEE       TO WS-DB-BEG-RNG.
P85139     PERFORM 850-MODIFY-BEGRNG-PCTSET2.
P85139     PERFORM
P85139         UNTIL (PERSACTION-NOTFOUND)
P85139         IF  (PCT-ACTION-CODE NOT = HREMP-ACTION-CODE)
P85139             PERFORM 1460-UPDATE-ACTION-INFO
P85139         END-IF
P85139         PERFORM 860-MODIFY-NXTRNG-PCTSET2
P85139     END-PERFORM.

           MOVE "Y"                TO HRWS-ADD.
           SET HREMP-HIRE-ACTION   TO TRUE.
096000     PERFORM 3000-HREMP-PROCESS-TRAN.
           MOVE ZEROES             TO HREMP-HIRE-ACTION-SW.

           IF (HRWS-GROUP-SIZE-ERROR)
               SET WARNINGS-PRINTED    TO TRUE
               MOVE HREMP-EMPLOYEE     TO RWG6-EMPLOYEE
               MOVE HREMP-LAST-NAME    TO HRWS-LAST-NAME
               MOVE HREMP-FIRST-NAME   TO HRWS-FIRST-NAME
               MOVE HREMP-MIDDLE-INIT  TO HRWS-MIDDLE-INIT
               MOVE HREMP-LAST-NAME-PRE
                                       TO HRWS-LAST-NAME-PRE
               MOVE HREMP-NAME-SUFFIX  TO HRWS-NAME-SUFFIX
               PERFORM 750-HR-FORMAT-NAME
               MOVE HRWS-FORMAT-NAME   TO RWG6-EMP-NAME
               MOVE 337                TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE        TO RWG6-WARNING-DESC
               MOVE HREMP-ACTION-CODE  TO RWG6-PCT-ACTION-CODE
               MOVE HREMP-ACTION-NBR   TO RWG6-PCT-ACTION-NBR
               MOVE 1                  TO RWG6-PCT-POS-LEVEL
               MOVE HREMP-EFFECT-DATE  TO RWG6-PCT-EFFECT-DATE
               MOVE "A"                TO RWG6-ACTION-TYPE
               MOVE WARNING-LINE       TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP.

P85139*    INITIALIZE HRWS-FIELD-NBR-TABLE
P85139*               I2.
P59574*    MOVE HREMP-COMPANY          TO DB-COMPANY.
P85139*    MOVE "A"                    TO DB-ACTION-TYPE.
P85139*    MOVE AW-APPLICANT           TO DB-EMPLOYEE.
P85139*    MOVE HREMP-ACTION-CODE      TO DB-ACTION-CODE.
P85139*    MOVE PCTSET2-ACTION-CODE    TO WS-DB-BEG-RNG.
P85139*    PERFORM 850-MODIFY-BEGRNG-PCTSET2.
P85139*
P58564*    IF (WS-HREMP-MSG-335 = SPACES)
P58564*        MOVE 335                TO CRT-MSG-NBR
P58564*        PERFORM 790-GET-MSG
P58564*        MOVE CRT-MESSAGE        TO WS-HREMP-MSG-335
P58564*        MOVE SPACES             TO CRT-MESSAGE
P58564*        MOVE ZEROES             TO CRT-MSG-NBR
P58564*    END-IF.
P58564*
P85139*    PERFORM 1460-UPDATE-ACTION-INFO.
P85139*
P85139*    MOVE "A"                    TO DB-ACTION-TYPE.
P85139*    MOVE AW-APPLICANT           TO DB-EMPLOYEE.
P85139*    MOVE PCTSET2-EMPLOYEE       TO WS-DB-BEG-RNG.
P85139*    PERFORM 850-MODIFY-BEGRNG-PCTSET2.
P85139*    PERFORM
P85139*        UNTIL (PERSACTION-NOTFOUND)
P85139*        IF  (PCT-ACTION-CODE NOT = HREMP-ACTION-CODE)
P85139*            PERFORM 1460-UPDATE-ACTION-INFO
P85139*        END-IF
P85139*        PERFORM 860-MODIFY-NXTRNG-PCTSET2
P85139*    END-PERFORM.

           IF (I2 NOT = ZEROES)
               MOVE WS-FALSE           TO HRWS-ALL-EMPLOYEES-SW
               MOVE HREMP-COMPANY      TO HRWS-COMPANY
               MOVE HREMP-EFFECT-DATE  TO HRWS-EFFECT-DATE
               MOVE HREMP-ACT-OBJ-ID   TO HRWS-ACT-OBJ-ID
               MOVE "N"                TO HRWS-UPDATE-BENEFIT
P69485         MOVE WS-UPD-ABS-MGMT    TO HRWS-UPDATE-ABSENCE-MGMT
               PERFORM 5000-REBUILD-PERSONNEL-GROUP
           END-IF.

P50750     PERFORM 840-MODIFY-PATSET1.
P50750     IF  (PCT-ACTION-CODE NOT = HREMP-ACTION-CODE)
P50750          MOVE PA100WS-CMT-SEQ-NBR       TO PAT-LAST-CMT-SEQ
P50750     END-IF.
           PERFORM 820-STORE-PERSACTYPE.
           IF  (WS-UPDATE-BENEFIT = "Y")
               MOVE HREMP-COMPANY              TO DB-COMPANY
               MOVE HREMP-EMPLOYEE             TO DB-EMPLOYEE
               MOVE HREMP-EFFECT-DATE          TO DB-EFFECT-DATE
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
               MOVE WS-OBJ-ID                  TO BNH-ACT-OBJ-ID
               MOVE "7"                        TO BNH-CHANGE-TYPE
               MOVE WS-SYSTEM-DATE-YMD         TO BNH-DATE-STAMP
J67329                                            BNH-CREATE-DATE
J67329         MOVE HHMMSS                     TO BNH-TIME-STAMP
J67329                                            BNH-CREATE-TIME               
               MOVE "N"                        TO BNH-HOLD-FLAG
J67329         MOVE CRT-USER-NAME              TO BNH-CREATE-USER-ID
J67329                                            BNH-USER-ID                  
               PERFORM 820-STORE-BNCHANGE
           END-IF.
096100
096200     IF (HREMP-UPDPEP-DATE = HREMP-EFFECT-DATE)
096300         INITIALIZE                 HREMP-UPDPEP-DATE.
096400
           IF (WS-UPDATE-REQ-DED      = "Y")
096500         MOVE HREMP-COMPANY      TO PRRQC-COMPANY
096600         MOVE HREMP-EMPLOYEE     TO PRRQC-EMPLOYEE
096700         INITIALIZE PRRQC-DFT-MAR-STAT
096900                    PRRQC-DFT-EXEMPTS
               MOVE WS-EDM-EFFECT-DT   TO PRRQC-EFFECT-DATE
               MOVE ZEROES             TO PRRQC-END-DATE
               MOVE PRM-UPDATE-OPTION  TO PRRQC-UPDATE-OPTION
097000         PERFORM 500-REQ-DED-CREATION
               IF (ERROR-FOUND)
                   SET ACTION-ERROR TO TRUE
141800             MOVE HREMP-EMPLOYEE       TO REG6-EMPLOYEE
142300             MOVE HRWS-FORMAT-NAME     TO REG6-EMP-NAME
142600             MOVE CRT-ERROR-NBR        TO CRT-MSG-NBR
142700             PERFORM 790-GET-MSG
142800             MOVE CRT-MESSAGE          TO REG6-ERROR-DESC
142900             MOVE HREMP-ACTION-CODE    TO REG6-PCT-ACTION-CODE
                   MOVE HREMP-ACTION-NBR     TO REG6-PCT-ACTION-NBR
143000             MOVE 1                    TO REG6-PCT-POS-LEVEL
143100             MOVE HREMP-EFFECT-DATE    TO REG6-PCT-EFFECT-DATE
143900             MOVE "A"                  TO REG6-ACTION-TYPE
144000             MOVE ERROR-LINE           TO RPT-GROUP-REQUEST
144100             PERFORM 700-PRINT-RPT-GRP
                   SET ERRORS-PRINTED TO TRUE
               ELSE
                   PERFORM 2660-PRINT-REQ-DED.
097100
097200     PERFORM 5000-HREMP-CREATE-ETM.

       1400-NEXT.
           IF (WS-RESTART-RECORD-A NOT NUMERIC)
               MOVE ZEROES             TO WS-RESTART-RECORD
           END-IF.
           IF  (PRM-UPDATE-OPTION = "Y")
           AND (AW-APPLICANT > WS-RESTART-RECORD)
               PERFORM 840-MODIFY-CKPSET1
               MOVE CKP-RESTART-INFO   TO WS-RESTART-INFO
               MOVE AW-APPLICANT       TO WS-RESTART-RECORD
               MOVE WS-RESTART-INFO    TO CKP-RESTART-INFO
               PERFORM 820-STORE-CKPOINT
               PERFORM 900-SAVE-PRINT-FILES
               PERFORM 925-AUDIT-END
               PERFORM 910-AUDIT-BEGIN
           END-IF.

           READ APP-WORK-FILE NEXT RECORD
               AT END
                   SET NO-RECORD-FOUND TO TRUE.

           GO TO 1400-END.

      ******************************************************************
       1405-SET-LOG.
      ******************************************************************

           MOVE "A"                    TO DB-ACTION-TYPE.
           MOVE AW-APPLICANT           TO DB-EMPLOYEE.
           MOVE PCTSET2-EMPLOYEE       TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PCTSET2.
           PERFORM
               UNTIL (PERSACTION-NOTFOUND)
               PERFORM 1407-SET-LOG-FLAGS
               THRU    1407-END
               PERFORM 860-FIND-NXTRNG-PCTSET2
           END-PERFORM.
P58299
P58299*  THIS CODE IS FOR LOGING PA31 DEFAULT FIELDS FOR HIRE ACTION
P58299     PERFORM 
P58299        VARYING I1 FROM 1 BY 1
P58299        UNTIL  (I1 > 34)
P58299        MOVE PA100WS-HIRE-FLD-NBR(I1) TO I3
P58299        MOVE "X"                      TO HREMP-LOG-FLAG (I3) 
P58299     END-PERFORM.

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
           MOVE HREMP-LOG-FLAG (HRPEM-LOCAT-CODE-DN)
                         TO HREMP-LOG-FLAG (HREMP-WORK-STATE-DN).
           IF (HREMP-LOG-FLAG (HREMP-STATE-DN)      = "X")
           OR (HREMP-LOG-FLAG (HRPEM-SUPP-STATE-DN) = "X")
               MOVE "X"   TO HREMP-LOG-FLAG (HREMP-TAX-STATE-DN).  

       1405-END.

      ******************************************************************
       1407-SET-LOG-FLAGS.
      ******************************************************************

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36)
               OR     (PCT-FLD-NBR (I1) = ZEROES)

                   IF (PCT-FLD-NBR (I1) < 2000)
                       MOVE PCT-FLD-NBR (I1) TO WS-FLD-NBR
                       MOVE "X" TO HREMP-LOG-FLAG (WS-FLD-NBR)
                   END-IF
           END-PERFORM.

       1407-END.

098200 1400-END.
098300
098400*****************************************************************
098500 1410-MOVE-APL-TO-WS             SECTION.
098600*****************************************************************
098700 1410-START.
098800
           MOVE "A"                    TO HREMP-FC.
           MOVE APL-COMPANY            TO HREMP-COMPANY.
           MOVE 1                      TO HREMP-COMPANY-FN.
           MOVE "Y"                    TO HREMP-ACTION.
           MOVE 1                      TO HREMP-POS-LEVEL.

      * This code to move APL to HREMP should match PA52.4 220-EDIT-DATA
J59149     MOVE APL-PROCESS-LEVEL      TO HREMP-PROCESS-LEVEL.
102200     MOVE APL-LAST-NAME          TO HREMP-LAST-NAME.
102300     MOVE APL-FIRST-NAME         TO HREMP-FIRST-NAME.
102400     MOVE APL-MIDDLE-NAME        TO HREMP-MIDDLE-NAME.
J18344     MOVE APL-MIDDLE-INIT        TO HREMP-MIDDLE-INIT.
102500     MOVE APL-NICK-NAME          TO HREMP-NICK-NAME.
102600     MOVE APL-ADDR1              TO HREMP-ADDR1.
102700     MOVE APL-ADDR2              TO HREMP-ADDR2.
           MOVE APL-ADDR3              TO HREMP-ADDR3.
           MOVE APL-ADDR4              TO HREMP-ADDR4.
102800     MOVE APL-CITY               TO HREMP-CITY.
102900     MOVE APL-STATE              TO HREMP-STATE.
103000     MOVE APL-ZIP                TO HREMP-ZIP.
P62465     MOVE APL-COUNTY             TO HREMP-COUNTY.
J19114     MOVE APL-EMAIL-ADDRESS      TO HREMP-EMAIL-PERSONAL.
103100     MOVE APL-COUNTRY-CODE       TO HREMP-COUNTRY-CODE.
           MOVE APL-BIRTHDATE          TO HRPEM-BIRTHDATE.
           MOVE APL-EMP-STATUS         TO HREMP-EMP-STATUS.
           MOVE APL-NBR-FTE            TO HREMP-NBR-FTE.
           MOVE APL-WORK-SCHED         TO HREMP-WORK-SCHED.

J73281     MOVE PAAPLWS-LEM-POSITION       TO HREMP-POSITION.
J73281     MOVE PAAPLWS-LEM-HM-DIST-CO     TO HREMP-HM-DIST-CO.
J73281     MOVE PAAPLWS-LEM-HM-ACCT-UNIT   TO HREMP-HM-ACCT-UNIT.
J73281     MOVE PAAPLWS-LEM-HM-ACCOUNT     TO HREMP-HM-ACCOUNT.
J73281     MOVE PAAPLWS-LEM-HM-SUB-ACCT    TO HREMP-HM-SUB-ACCT.
J73281     MOVE PAAPLWS-LEM-JOB-CODE       TO HREMP-JOB-CODE.
J73281     MOVE PAAPLWS-LEM-UNION-CODE     TO HREMP-UNION-CODE.
J73281     MOVE PAAPLWS-LEM-SUPERVISOR     TO HREMP-SUPERVISOR.
J73281     MOVE PAAPLWS-LEM-PAY-FREQUENCY  TO HREMP-PAY-FREQUENCY.
J73281     MOVE PAAPLWS-LEM-SALARY-CLASS   TO HREMP-SALARY-CLASS.
J73281     MOVE PAAPLWS-LEM-EXEMPT-EMP     TO HREMP-EXEMPT-EMP.
J73281     MOVE PAAPLWS-LEM-PAY-RATE       TO HREMP-PAY-RATE.
J73281     MOVE PAAPLWS-LEM-PAY-STEP       TO HREMP-PAY-STEP.
J73281     MOVE PAAPLWS-LEM-PAY-GRADE      TO HREMP-PAY-GRADE.
J73281     MOVE PAAPLWS-LEM-SCHEDULE       TO HREMP-SCHEDULE.
J73281     MOVE PAAPLWS-LEM-OT-PLAN-CODE   TO HREMP-OT-PLAN-CODE.
J73281     MOVE PAAPLWS-LEM-SUPERVISOR-IND TO HREMP-SUPERVISOR-IND.
J73281     MOVE PAAPLWS-LEM-ANNUAL-HOURS   TO HREMP-ANNUAL-HOURS.
J73281     MOVE PAAPLWS-LEM-LAB-DIST-FLAG  TO HREMP-LAB-DIST-FLAG.
J73281     MOVE PAAPLWS-LEM-ENCUMBER-FLAG  TO HREMP-ENCUMBER-FLAG.
J73281     MOVE PAAPLWS-LEM-EFFORT-FLAG    TO HREMP-EFFORT-FLAG.
J73281     MOVE PAAPLWS-LEM-RPT-CURR       TO HREMP-RPT-CURR.
J73281     MOVE PAAPLWS-LEM-PRMCERT-ID     TO HREMP-PRMCERT-ID.
J73281     MOVE PAAPLWS-LEM-SNDCERT-ID     TO HREMP-SNDCERT-ID.
J73281     MOVE PAAPLWS-LEM-FRNG-RATE      TO HREMP-FRNG-RATE.
J73281     MOVE PAAPLWS-LEM-FRNG-ACCT-CAT  TO HREMP-FRNG-ACCT-CAT.
J73281     MOVE PAAPLWS-LEM-FRNG-ACCOUNT   TO HREMP-FRNG-ACCOUNT.
J73281     MOVE PAAPLWS-LEM-FRNG-SUB-ACCT  TO HREMP-FRNG-SUB-ACCT.
J73281     MOVE PAAPLWS-LEM-REMOTE         TO HREMP-REMOTE.
J73281     MOVE PAAPLWS-LEM-LOCAT-CODE     TO HRPEM-LOCAT-CODE.
J73281     MOVE PAAPLWS-LEM-BARGAIN-UNIT   TO HRPEM-BARGAIN-UNIT.

J73281     IF (PAAPLWS-LTM-FLAG = "*")
J73281         MOVE APL-L-JOB-APP       TO HREMP-L-JOB-APP
J73281         MOVE APL-L-JOB-REQ       TO HREMP-L-JOB-REQ
J73281         MOVE APL-L-CANDIDATE-ID  TO HREMP-L-CANDIDATE-ID
J73281         IF (APL-PROCESS-LEVEL NOT = SPACES)
J73281         AND (HREMP-PROCESS-LEVEL = SPACES) 
J73281             MOVE APL-PROCESS-LEVEL TO HREMP-PROCESS-LEVEL
J73281         END-IF
J73281         IF (APL-DEPARTMENT NOT = SPACES)
J73281         AND (HREMP-DEPARTMENT = SPACES)
J73281             MOVE APL-DEPARTMENT  TO HREMP-DEPARTMENT
J73281         END-IF
J73281     END-IF.

      * HREMPPD will default WorkCountry and CurrencyCode
           INITIALIZE                     HREMP-WORK-COUNTRY.
           MOVE 1                      TO HREMP-WORK-COUNTRY-FN.
           INITIALIZE                     HREMP-CURRENCY-CODE.
104100     MOVE APL-VETERAN            TO HRPEM-VETERAN.
103200     MOVE APL-DRAFT-STATUS       TO HRPEM-DRAFT-STATUS.
103300     MOVE APL-FINAL-RANK         TO HRPEM-FINAL-RANK.
           MOVE APL-CUR-STATUS         TO HRPEM-CUR-STATUS.
103400     MOVE APL-LAST-PHYSICAL      TO HRPEM-LAST-PHYSICAL.
103500     MOVE APL-HM-PHONE-CNTRY     TO HRPEM-HM-PHONE-CNTRY.
103600     MOVE APL-HM-PHONE-NBR       TO HRPEM-HM-PHONE-NBR.
103700     MOVE APL-HIRE-SOURCE        TO HRPEM-HIRE-SOURCE.
           MOVE APL-NAME-PREFIX        TO HREMP-NAME-PREFIX.
           MOVE APL-NAME-SUFFIX        TO HREMP-NAME-SUFFIX.
           MOVE APL-LANGUAGE-CODE      TO HRPEM-LANGUAGE-CODE.
           MOVE APL-DISABILITY         TO HRPEM-DISABILITY.
104200     IF (APL-SEX = "X")
104300         MOVE SPACES             TO HRPEM-SEX
104400     ELSE
104500         MOVE APL-SEX            TO HRPEM-SEX.
104900     MOVE APL-EEO-CLASS          TO HRPEM-EEO-CLASS.
           MOVE APL-CONSENT            TO HRPEM-CONSENT.
           MOVE APL-RELIGION           TO HRPEM-RELIGION.
           MOVE APL-LAST-NAME-PRE      TO HREMP-LAST-NAME-PRE.
105000     IF (APL-HANDICAP-ID = "X")
105100         MOVE SPACES             TO HRPEM-HANDICAP-ID
105200     ELSE
105300         MOVE APL-HANDICAP-ID    TO HRPEM-HANDICAP-ID.
           MOVE APL-FICA-NBR           TO HREMP-FICA-NBR.
           MOVE APL-FORMER-LST-NM      TO HRPEM-FORMER-LST-NM.
           MOVE APL-FORMER-FST-NM      TO HRPEM-FORMER-FST-NM.
           MOVE APL-FORMER-MI          TO HRPEM-FORMER-MI.

           MOVE "A"                    TO DB-ACTION-TYPE.
           MOVE AW-APPLICANT           TO DB-EMPLOYEE.
           MOVE PCTSET2-EMPLOYEE       TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PCTSET2.
           PERFORM
               UNTIL (PERSACTION-NOTFOUND)
                   PERFORM 1415-WORK-PHONE
                       VARYING I1 FROM 1 BY 1
                       UNTIL  (I1 > 36)
                       OR     (PCT-FLD-NBR (I1) = ZEROES)
                   PERFORM 860-FIND-NXTRNG-PCTSET2
           END-PERFORM.

107400 1410-END.

      *****************************************************************
       1415-WORK-PHONE                 SECTION.
      *****************************************************************
       1415-START.
         
           IF  (PCT-FLD-NBR (I1)   = 150)
           AND (PCT-NEW-VALUE (I1) = SPACES)
           AND (APL-WK-PHONE-CNTRY NOT = SPACES)
               MOVE APL-WK-PHONE-CNTRY TO HRPEM-WK-PHONE-CNTRY
           END-IF.
           IF  (PCT-FLD-NBR (I1)   = 11)
           AND (PCT-NEW-VALUE (I1) = SPACES)
           AND (APL-WK-PHONE-NBR   NOT = SPACES)
               MOVE APL-WK-PHONE-NBR   TO HRPEM-WK-PHONE-NBR
           END-IF.
           IF  (PCT-FLD-NBR (I1)   = 12)
           AND (PCT-NEW-VALUE (I1) = SPACES)
           AND (APL-WK-PHONE-EXT   NOT = SPACES)
               MOVE APL-WK-PHONE-EXT   TO HRPEM-WK-PHONE-EXT
           END-IF.

       1415-END.

      *****************************************************************
       1420-MOVE-PCT-TO-WS             SECTION.
      *****************************************************************
       1420-START.

           IF  (EDITING-USERFIELDS)
               GO TO 1420-CONTINUE.

           IF  (PCT-ACTION-NBR = 1)
               MOVE PCT-EDM-EFFECT-DT      TO PRPXL-PARM-EFF-DATE

               MOVE PCT-PARTICIPNT         TO HREMP-EMPLOYEE
               MOVE PCT-EFFECT-DATE        TO HREMP-DATE-HIRED
                                              HREMP-EFFECT-DATE
               MOVE WS-SYSTEM-DATE-YMD     TO HREMP-CREATION-DATE
               MOVE PCT-ACTION-CODE        TO HREMP-ACTION-CODE
               MOVE PCT-ACTION-NBR         TO HREMP-ACTION-NBR
               MOVE PCT-REASON (1)         TO HREMP-REASON1
               MOVE PCT-REASON (2)         TO HREMP-REASON2
101470*        MOVE PCT-USER-ID            TO HREMP-USER-ID
101470         MOVE CRT-USER-NAME          TO HREMP-USER-ID
               MOVE PCT-ANT-END-DATE       TO HREMP-ANT-END-DATE
               MOVE PCT-HOLD-FLAG          TO WS-HOLD-FLAG
               MOVE PCT-HIST-CORR-FLAG     TO WS-HIST-CORR-FLAG
               MOVE PCT-UPDATE-BENEFIT     TO WS-UPDATE-BENEFIT
               MOVE PCT-UPD-ABS-MGMT       TO WS-UPD-ABS-MGMT
               MOVE PCT-UPDATE-REQ-DED     TO WS-UPDATE-REQ-DED
               MOVE PCT-EDM-EFFECT-DT      TO WS-EDM-EFFECT-DT
               MOVE PCT-EDM-END-DATE       TO WS-EDM-END-DATE
J08104         MOVE PCT-CREATE-USER-ID     TO WS-CREATE-USER-ID
J08104         MOVE PCT-CREATE-DATE        TO WS-CREATE-DATE
J08104         MOVE PCT-CREATE-TIME        TO WS-CREATE-TIME

           ELSE
               SET MULTIPLE-ACTIONS TO TRUE
           END-IF.

           MOVE PCT-ACTION-CODE            TO DB-ACTION-CODE.
           PERFORM 840-FIND-PATSET1.
           IF  (PAT-WORKFLOW-FLAG = "Y")
           AND (PCT-APPROVAL-FLAG = "N")
               MOVE 154                    TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE 1                      TO HREMP-I2
               MOVE CRT-MESSAGE            TO HREMP-MESSAGE (HREMP-I2)
               SET HREMP-ERROR-FOUND TO TRUE
               SET WORKFLOW-ERROR          TO TRUE
               MOVE ZERO                   TO HREMP-FIELD-NBR (HREMP-I2)
           ELSE
               IF (PAT-ACTIVE-FLAG = "2")   
                   MOVE PCT-ACTION-CODE    TO CRT-ERR-VAR1
004200             MOVE 112                TO CRT-ERROR-NBR
004400             PERFORM 790-GET-MSG         
                   MOVE 1                  TO HREMP-I2
                   SET HREMP-ERROR-FOUND   TO TRUE
                   MOVE ZERO               TO HREMP-FIELD-NBR (HREMP-I2)
               END-IF
           END-IF.

       1420-CONTINUE.

           PERFORM 1421-MOVE-PCT-FLD
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36).

           PERFORM 860-FIND-NXTRNG-PCTSET2.

       1420-END.

      *****************************************************************
       1421-MOVE-PCT-FLD               SECTION.
      *****************************************************************
       1421-START.

           IF  (PCT-NEW-VALUE (I1) = SPACES)
               GO TO 1421-END.

           IF  (EDITING-USERFIELDS)
           AND (PCT-FLD-NBR (I1) < 2000)
               GO TO 1421-END.

           IF  (NOT-EDITING-USERFIELDS)
           AND (PCT-FLD-NBR (I1) > 1999)
               SET ACTION-USERFIELDS TO TRUE
               GO TO 1421-END.

           IF  (PCT-FLD-NBR (I1) >= 2000)
               MOVE PCT-FLD-NBR (I1)       TO WS-FLD-NBR
               COMPUTE I2 = WS-FIELD-KEY-N + 1
               MOVE "X"                    TO WS-USER-VAL (I2)
           END-IF.

           INITIALIZE PAPCT-SCR-FIELDS.
           MOVE PCT-COMPANY            TO PAPCT-COMPANY.
           MOVE APL-APPLICANT          TO PAPCT-APPLICANT.
           MOVE 1                      TO PAPCT-EMP-APP.
           MOVE PCT-EMPLOYEE           TO PAPCT-EMPLOYEE.
           MOVE PCT-FLD-NBR (I1)       TO PAPCT-FLD-NBR.
           MOVE PCT-NEW-VALUE (I1)     TO PAPCT-NEW-VALUE.
           MOVE 1                      TO PAPCT-FIELD-FN.
           MOVE PCT-EFFECT-DATE        TO PAPCT-EFFECT-DATE.
           MOVE PCT-ACTION-CODE        TO PAPCT-ACTION-CODE.
           MOVE PCT-ACTION-NBR         TO PAPCT-ACTION-NBR.
           MOVE PCT-REASON (1)         TO PAPCT-REASON1.
           MOVE PCT-REASON (2)         TO PAPCT-REASON2.
101470*    MOVE PCT-USER-ID            TO PAPCT-USER-ID.
101470     MOVE CRT-USER-NAME          TO PAPCT-USER-ID.
           MOVE PCT-ANT-END-DATE       TO PAPCT-ANT-END-DATE.
           INITIALIZE                     PAPCT-EDIT-DATA-ONLY.

           PERFORM 5000-PAPCT-MOVE-TO-HREMP.

       1421-END.

      *****************************************************************
       1430-CHECK-REQ-FIELDS           SECTION.
      *****************************************************************
       1430-START.

           MOVE SPACES                 TO HREMP-TOPIC.
           MOVE HREMP-WORK-COUNTRY     TO HRSEC-WORK-COUNTRY.
           PERFORM 6000-LOAD-HREMP-SECURITY.

           PERFORM 5000-HREMP-FILL-FN.

           PERFORM 6200-EMP-REQ-EDITS.

           IF  (WS-REQ-USER-COUNTRY-CODE NOT = HRSEC-WORK-COUNTRY)
               INITIALIZE WS-REQ-USER-FIELDS-TABLE
               MOVE HRSEC-WORK-COUNTRY     TO WS-REQ-USER-COUNTRY-CODE
               IF  (HRSEC-USE-COUNTRY)
                   MOVE WS-REQ-USER-COUNTRY-CODE   TO DB-COUNTRY-CD-REQ
               ELSE
                   INITIALIZE                         DB-COUNTRY-CD-REQ
               END-IF
               MOVE 2000                           TO DB-FLD-NBR
               MOVE 2099                           TO DBRNG-FLD-NBR
               MOVE PASSET1-FLD-NBR                TO WS-DB-SUB-RNG
               PERFORM 850-FIND-SUBRNG-PASSET1
               PERFORM
                   UNTIL (PASCRTY-NOTFOUND)
                   MOVE PAS-FLD-NBR        TO WS-FLD-NBR
                   COMPUTE I1 = WS-FIELD-KEY-N + 1
                   MOVE PAS-REQ-FLAG       TO WS-REQ-USER (I1)
                   PERFORM 860-FIND-NXTRNG-PASSET1
               END-PERFORM
           END-IF.

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 100)
               OR     (HREMP-I2 > HREMP-MESSAGE-MAX)
               IF  (WS-REQ-USER (I1) = "X")
               AND (WS-USER-VAL (I1) = SPACES)
                   MOVE PRM-COMPANY    TO DB-COMPANY
                   MOVE 1              TO DB-EMP-APP
                   MOVE APL-APPLICANT  TO DB-EMPLOYEE
                   COMPUTE WS-FIELD-KEY-N = I1 - 1
                   MOVE WS-FIELD-KEY   TO DB-FIELD-KEY
                   PERFORM 840-FIND-HEUSET1
                   IF  (HREMPUSF-FOUND)
                       MOVE "X"        TO WS-USER-VAL (I1)
               END-IF
               IF  (WS-REQ-USER (I1) = "X")
               AND (WS-USER-VAL (I1) = SPACES)
                   MOVE 2000           TO WS-FLD-NBR
                   COMPUTE WS-FIELD-KEY-N = I1 - 1
                   MOVE WS-FLD-NBR     TO DB-FLD-NBR

                   PERFORM 840-FIND-PADSET1
                   MOVE PAD-ITEM-NAME      TO CRT-PHRASE
                   MOVE HREMP-PHRASE-SIZE  TO CRT-PHRASE-SIZE
                   MOVE "N"                TO CRT-PUT-DOTS
                   PERFORM 900-GET-PHRASE-XLT
                   MOVE CRT-PHRASE-XLT     TO HREMP-FIELD-NAME

P58564             MOVE WS-HREMP-MSG-50    TO HREMP-ERR-MSG

                   ADD 1                   TO HREMP-I2
                   MOVE HREMP-ERROR-MESSAGE
                                           TO HREMP-MESSAGE (HREMP-I2)
                   MOVE DB-FLD-NBR         TO HREMP-FIELD-NBR (HREMP-I2)
                   SET HREMP-ERROR-FOUND TO TRUE
               END-IF
           END-PERFORM.

       1430-END.

      *****************************************************************
       1450-PRINT-ACTION-INFO          SECTION.
      *****************************************************************
       1450-START.

           PERFORM 1700-PRINT-ACTION.

082400     PERFORM 1750-PRINT-ACTION-DETAIL
082500         VARYING I1 FROM 1 BY 1
082600         UNTIL  (I1 > 36).

082800     MOVE 1                      TO DB-EMP-APP.
082900     MOVE "PA"                   TO DB-CMT-TYPE.
083000     MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE.
083100     MOVE PCT-EFFECT-DATE        TO DB-DATE.
124240     IF (HREMP-EMPLOYEE          NOT = ZEROES)
124240         MOVE HREMP-EMPLOYEE     TO DB-EMPLOYEE
124240     ELSE
083200         MOVE PCT-EMPLOYEE       TO DB-EMPLOYEE.
083300     MOVE SPACES                 TO DB-JOB-CODE.
083400     MOVE PCT-ACTION-NBR         TO DB-LN-NBR.
083500     MOVE PACSET2-LN-NBR         TO WS-DB-BEG-RNG.
083600     PERFORM 850-FIND-BEGRNG-PACSET2.
083700     PERFORM 860-FIND-NXTRNG-PACSET2
083800         UNTIL (PACOMMENTS-NOTFOUND)
083900         OR    (PAC-PRINT-CODE  NOT = "N").
084000
084100     IF (PACOMMENTS-FOUND)
084200         MOVE GN6H-PAC-SEQ-NBR   TO RPT-GROUP-REQUEST
084300         PERFORM 700-PRINT-RPT-GRP.
084400
084500     PERFORM 1755-PRINT-COMMENTS
084600         UNTIL (PACOMMENTS-NOTFOUND).
084700
       1450-END.

      *****************************************************************
       1460-UPDATE-ACTION-INFO         SECTION.
      *****************************************************************
       1460-START.

           PERFORM 1470-DO-USER-FIELDS.

087700     MOVE 1                      TO DB-EMP-APP.
087800     MOVE "PA"                   TO DB-CMT-TYPE.
087900     MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE.
088000     MOVE PCT-EFFECT-DATE        TO DB-DATE.
088100     MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE.
088200     MOVE SPACES                 TO DB-JOB-CODE.
088300     MOVE PCT-ACTION-NBR         TO DB-LN-NBR.
088400     MOVE PACSET2-LN-NBR         TO WS-DB-BEG-RNG.

           IF  (PCT-ACTION-CODE NOT = HREMP-ACTION-CODE)
               PERFORM 850-FIND-BEGRNG-PACSET2
               IF  (PACOMMENTS-FOUND)
                   PERFORM 800-CREATE-PACOMMENTS
                   MOVE HREMP-COMPANY      TO PAC-COMPANY
                   MOVE 0                  TO PAC-EMP-APP
                   MOVE "PA"               TO PAC-CMT-TYPE
                   MOVE HREMP-EMPLOYEE     TO PAC-EMPLOYEE
                   MOVE SPACES             TO PAC-JOB-CODE
                   MOVE HREMP-ACTION-CODE  TO PAC-ACTION-CODE
                   MOVE HREMP-ACTION-NBR   TO PAC-LN-NBR
                   ADD 1                   TO PAT-LAST-CMT-SEQ
                   MOVE PAT-LAST-CMT-SEQ   TO PAC-SEQ-NBR
P50750             MOVE PAT-LAST-CMT-SEQ   TO PA100WS-CMT-SEQ-NBR
P58564             MOVE WS-HREMP-MSG-335   TO PAC-CMT-TEXT
                   INITIALIZE CRT-MESSAGE
                   MOVE HREMP-EFFECT-DATE  TO PAC-DATE
                   MOVE "Y"                TO PAC-PRINT-CODE
                   PERFORM 820-STORE-PACOMMENTS
               END-IF
           END-IF.

088500     PERFORM 850-MODIFY-BEGRNG-PACSET2.
088600     PERFORM 1614-ACTION-COMMENTS
088700         UNTIL (PACOMMENTS-NOTFOUND).
088800
088900     PERFORM 830-DELETE-PERSACTION.

       1460-END.

      ******************************************************************
       1470-DO-USER-FIELDS             SECTION.
      ******************************************************************
       1470-START.

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36)

               IF  (PCT-FLD-NBR (I1)   NOT < 2000)
               AND (PCT-FLD-NBR (I1)   NOT > 2099)
               AND (PCT-NEW-VALUE (I1) NOT = SPACES)
                   INITIALIZE PAPCT-SCR-FIELDS
                   MOVE PCT-FLD-NBR (I1)   TO PAPCT-FLD-NBR
                   MOVE PCT-NEW-VALUE (I1) TO PAPCT-NEW-VALUE
                   MOVE HREMP-COMPANY      TO PAPCT-COMPANY
                   MOVE HREMP-EMPLOYEE     TO PAPCT-EMPLOYEE
                   MOVE HREMP-EFFECT-DATE  TO PAPCT-EFFECT-DATE
                   MOVE HREMP-ACTION-CODE  TO PAPCT-ACTION-CODE
                   MOVE HREMP-ACTION-NBR   TO PAPCT-ACTION-NBR
                   MOVE HREMP-REASON1      TO PAPCT-REASON1
                   MOVE HREMP-REASON2      TO PAPCT-REASON2
                   MOVE HREMP-USER-ID      TO PAPCT-USER-ID
                   MOVE HREMP-ANT-END-DATE TO PAPCT-ANT-END-DATE
P85139             MOVE APL-APPLICANT      TO PAPCT-APPLICANT
                   PERFORM 5100-EDIT-USER-FIELDS
                   PERFORM 3000-HRHEU-PROCESS-TRAN

                   MOVE PAPCT-FLD-NBR      TO DB-FLD-NBR
                   INITIALIZE                 DB-COUNTRY-CD-REQ
                                              DB-PROCESS-LEVEL
                   PERFORM 840-FIND-PASSET1
                   IF  (PASCRTY-FOUND)
                   AND (PAS-USED-BY-GROUP = "X")
                       ADD 1               TO I2
                       MOVE DB-FLD-NBR     TO HRWS-FIELD-NBR (I2)
                   END-IF

               END-IF
           END-PERFORM.

       1470-END.

      *****************************************************************
       1500-CHECK-FOR-RESTART          SECTION.
      *****************************************************************
       1500-START.

           MOVE ZEROS                  TO WS-RECORD-COUNT.
           SET NOT-RESTARTING TO TRUE.

           PERFORM 840-FIND-CKPSET1.

           IF (CKPOINT-FOUND)
P69000         MOVE CKP-RESTART-INFO        TO WS-RESTART-INFO

J21883* IF REPORT OPTION 4 AND APPLICANT ALREADY PROCESSED, SKIP APPLICANT.
J21883         IF  (WS-RESTART-WRK-NAME NOT = SPACES)
J21883         AND (PRM-RUN-OPTION = 4)
J21883             GO TO 1500-END
J21883         END-IF

               IF  (WS-RESTART-AW-NAME NOT = SPACES)
               AND (PRM-UPDATE-OPTION  = "Y")
                   MOVE WS-RESTART-AW-NAME  TO WS-APP-WORK-NAME
                   SET RESTARTING TO TRUE
               ELSE
                   INITIALIZE WS-RESTART-INFO
                   MOVE 1                  TO WS-RESTART-PHASE
                   PERFORM 900-BUILD-TMP-FILE-NAME
                   MOVE WS-TMP-FILE        TO WS-APP-WORK-NAME
                   OPEN OUTPUT APP-WORK-FILE
                   CLOSE APP-WORK-FILE
                   IF  (PRM-UPDATE-OPTION = "Y")
                       PERFORM 910-AUDIT-BEGIN
                       PERFORM 840-MODIFY-CKPSET1
                       MOVE WS-APP-WORK-NAME   TO WS-RESTART-AW-NAME
                       MOVE WS-RESTART-INFO    TO CKP-RESTART-INFO
                       PERFORM 820-STORE-CKPOINT
                       PERFORM 900-SAVE-PRINT-FILES
                       PERFORM 925-AUDIT-END
                   END-IF
               END-IF.

       1500-END.

107600******************************************************************
107700 1614-ACTION-COMMENTS            SECTION.
107800******************************************************************
107900 1614-START.
108000
108100     PERFORM 830-DELETE-PACOMMENTS.
108200     PERFORM 810-RECREATE-PACOMMENTS.
108300
           IF  (PAC-ACTION-CODE NOT = HREMP-ACTION-CODE)
               MOVE HREMP-ACTION-CODE  TO PAC-ACTION-CODE
               MOVE HREMP-ACTION-NBR   TO PAC-LN-NBR
               ADD 1                   TO PAT-LAST-CMT-SEQ
               MOVE PAT-LAST-CMT-SEQ   TO PAC-SEQ-NBR
P50750         MOVE PAT-LAST-CMT-SEQ   TO PA100WS-CMT-SEQ-NBR
           END-IF.

           MOVE 0                      TO PAC-EMP-APP.
           MOVE HREMP-EMPLOYEE         TO PAC-EMPLOYEE.

108600     PERFORM 820-STORE-PACOMMENTS.
108700     PERFORM 860-MODIFY-NXTRNG-PACSET2.
108800
108900 1614-END.
109000
109100*****************************************************************
109200 1700-PRINT-ACTION               SECTION.
109300*****************************************************************
109400 1700-START.
109500
109600     MOVE PCT-ACTION-CODE        TO R1G3-PCT-ACTION-CODE
109700                                    DB-ACTION-CODE.
109800     PERFORM 840-FIND-PATSET1.
109900     IF (PERSACTYPE-FOUND)
110000         MOVE PAT-DESCRIPTION    TO R1G3-ACTION-DESC
110100     ELSE
110200         MOVE SPACES             TO R1G3-ACTION-DESC.
110300
110400     MOVE PCT-ACTION-NBR         TO R1G3-PCT-ACTION-NBR.
P51871*    MOVE PCT-USER-ID            TO R1G3-PCT-USER-ID.
101470*    MOVE PCT-USER-ID            TO WS-USER-DBUIDKEY.
101470     MOVE CRT-USER-NAME          TO WS-USER-DBUIDKEY.
J21860     PERFORM 900-GET-USER-DISPLAY-NAME.
J21860     PERFORM 5300-NAME-CHECK.
J21860     MOVE WS-CHECK-NAME-OUTPUT   TO R1G3-PCT-USER-ID.
J08104*    MOVE PCT-CREATE-USER-ID     TO R1G3-PCT-CREATE-USER-ID.
J21860     MOVE PCT-CREATE-USER-ID     TO WS-USER-DBUIDKEY.
J21860     PERFORM 900-GET-USER-DISPLAY-NAME.
J21860     PERFORM 5300-NAME-CHECK.
J21860     MOVE WS-CHECK-NAME-OUTPUT   TO R1G3-PCT-CREATE-USER-ID.

           IF  (PCT-ACTION-CODE NOT = HREMP-ACTION-CODE)
               GO TO 1700-PRINT-LINE.

110500     MOVE 1                      TO R1G3-PCT-POS-LEVEL.
110600     MOVE PCT-UPDATE-BENEFIT     TO R1G3-PCT-UPDATE-BENEFIT.
           MOVE PCT-UPD-ABS-MGMT       TO R1G3-PCT-UPD-ABS-MGMT.
110600     MOVE PCT-UPDATE-REQ-DED     TO R1G3-PCT-UPDATE-REQ-DED.
110700     MOVE PCT-EFFECT-DATE        TO R1G3-PCT-EFFECT-DATE.
110800     MOVE PCT-ANT-END-DATE       TO R1G3-PCT-ANT-END-DATE.
110900
111000     IF (PCT-REASON (1) NOT = SPACES)
111100         MOVE PCT-REASON (1)     TO R1G3-PCT-REASON-1
112000     ELSE
112100         MOVE SPACES             TO R1G3-PCT-REASON-1.
112300
112400     IF (PCT-REASON (2) NOT = SPACES)
112500         MOVE PCT-REASON (2)     TO R1G3-PCT-REASON-2
113300     ELSE
113400         MOVE SPACES             TO R1G3-PCT-REASON-2.
113600
           IF  (PCT-UPDATE-REQ-DED = "Y")
           AND (PCT-ACTION-CODE    = HREMP-ACTION-CODE)
               MOVE PCT-ACTION-CODE        TO R6G3-PCT-ACTION-CODE
               IF (PERSACTYPE-FOUND)
                   MOVE PAT-DESCRIPTION    TO R6G3-ACTION-DESC
               ELSE
                   MOVE SPACES             TO R6G3-ACTION-DESC
               END-IF
               MOVE PCT-ACTION-NBR         TO R6G3-PCT-ACTION-NBR
               MOVE PCT-EFFECT-DATE        TO R6G3-PCT-EFFECT-DATE
P51871*        MOVE PCT-USER-ID            TO R6G3-PCT-USER-ID
101470*        MOVE PCT-USER-ID            TO WS-USER-DBUIDKEY
101470         MOVE CRT-USER-NAME          TO WS-USER-DBUIDKEY
J21860         PERFORM 900-GET-USER-DISPLAY-NAME
J21860         PERFORM 5300-NAME-CHECK
J21860         MOVE WS-CHECK-NAME-OUTPUT   TO R6G3-PCT-USER-ID
J08104*        MOVE PCT-CREATE-USER-ID     TO R6G3-PCT-CREATE-USER-ID
J21860         MOVE PCT-CREATE-USER-ID     TO WS-USER-DBUIDKEY
J21860         PERFORM 900-GET-USER-DISPLAY-NAME
J21860         PERFORM 5300-NAME-CHECK
J21860         MOVE WS-CHECK-NAME-OUTPUT   TO R6G3-PCT-CREATE-USER-ID
               MOVE R6GN3-PCT-ACTION-CODE  TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
           END-IF.

       1700-PRINT-LINE.

113700     MOVE R1GN3-PCT-ACTION-CODE   TO RPT-GROUP-REQUEST.
113800     PERFORM 700-PRINT-RPT-GRP.

114000 1700-END.
114100*****************************************************************
114200 1750-PRINT-ACTION-DETAIL        SECTION.
114300*****************************************************************
114400 1750-START.
114500
114600     IF (PCT-FLD-NBR (I1) = ZEROS)
114700         MOVE 36                 TO I1
114800         GO TO 1750-END.
114900
115000     MOVE PCT-FLD-NBR (I1)       TO DB-FLD-NBR.
115100     PERFORM 840-FIND-PADSET1.
115200
           MOVE PAD-ITEM-NAME          TO CRT-PHRASE.
           MOVE WS-PHRASE-SIZE         TO CRT-PHRASE-SIZE.
           MOVE "N"                    TO CRT-PUT-DOTS.
           PERFORM 900-GET-PHRASE-XLT.
           MOVE CRT-PHRASE-XLT         TO R1G4-FIELD-DESC.
           MOVE PCT-FLD-NBR (I1)       TO HREMP-FLD-NBR.
           MOVE "Y"                    TO HREMP-FORMAT-FIELD.
           MOVE PCT-COMPANY            TO HREMP-COMPANY.
           MOVE SPACES                 TO HREMP-VALUE.
           PERFORM 5000-PAAPL-RETRIEVE-VALUE.
           MOVE HREMP-VALUE            TO R1G4-PCT-PRE-VALUE.

           MOVE PAD-DATA-TYPE          TO PAPCTIO-DATA-TYPE.
           MOVE "N"                    TO PAPCTIO-DATA-CURR.
           MOVE PAD-DECIMALS           TO PAPCTIO-DATA-DECIMALS.
           MOVE PCT-NEW-VALUE (I1)     TO PAPCTIO-NEW-VALUE.
           PERFORM 9200-FORMAT-PAPCT-TO-DISPLAY.
           MOVE PAPCTIO-NEW-VALUE      TO R1G4-PCT-NEW-VALUE.

116600     MOVE R1GN4D-PCT-ACTION-DETAIL TO RPT-GROUP-REQUEST.
116800     PERFORM 700-PRINT-RPT-GRP.
116900
117000 1750-END.
117100******************************************************************
117200 1755-PRINT-COMMENTS             SECTION.
117300******************************************************************
117400 1755-START.
117500
117600     IF (PAC-PRINT-CODE = "N")
117700         GO TO 1755-FIND-NEXT.
117800
117900     MOVE PAC-CMT-TEXT           TO G6-PAC-CMT-TEXT.
118000
118100     MOVE GN6D-PAC-SEQ-NBR       TO RPT-GROUP-REQUEST.
118200     PERFORM 700-PRINT-RPT-GRP.
118300
118400 1755-FIND-NEXT.
118500     PERFORM 860-FIND-NXTRNG-PACSET2.
118600
118700 1755-END.
118800
118900*****************************************************************
119000 1800-PROCESS-POS-ACTION         SECTION.
119100*****************************************************************
119200 1800-START.
119300
119400     MOVE WS-COMPANY-NAME        TO R1G1-PRS-NAME
119400                                    R6G1-PRS-NAME.
119500     MOVE PRM-COMPANY            TO R1G1-PCT-COMPANY
119400                                    R6G1-PCT-COMPANY.
P58564     MOVE WS-HREMP-MSG-332       TO R1G1-TITLE.

120400     MOVE PRM-UPDATE-OPTION      TO R1G1-UPDATE-OPTION
120400                                    R6G1-UPDATE-OPTION.

120900     IF (PRM-UPDATE-OPTION = "Y")
               PERFORM 2100-CHECK-RESTART
               IF (WS-RESTART-PHASE-A NOT NUMERIC)
                   MOVE ZEROES             TO WS-RESTART-PHASE
               END-IF
               IF (WS-RESTART-PHASE    = 4)
                   GO TO 1800-END
               END-IF
J73509         OPEN EXTEND PA100FLDS-FILE
121000         PERFORM 910-AUDIT-BEGIN
121100         PERFORM 850-MODIFY-BEGRNG-PCTSET1
121200     ELSE
121300         PERFORM 850-FIND-BEGRNG-PCTSET1.
121400     MOVE ZEROS                  TO WS-LAST-PART.
121500
121600     IF  (PERSACTION-FOUND)
121700     AND (PCT-ACTION-TYPE = DB-ACTION-TYPE)
121800     AND (PCT-EFFECT-DATE <= WS-PRM-EFFECT-DATE)
121900         MOVE R1GN1-PCT-COMPANY      TO RPT-GROUP-REQUEST
122000         PERFORM 700-PRINT-RPT-GRP
121900         MOVE R6GN1-PCT-COMPANY      TO RPT-GROUP-REQUEST
122000         PERFORM 700-PRINT-RPT-GRP
               MOVE R7GN1-PCT-COMPANY      TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
122100         MOVE PCT-COMPANY            TO PA100WS-SAVE-COMPANY
122200         MOVE "L"                    TO PA100WS-SAVE-ACTION-TYPE
122300         MOVE PCT-EFFECT-DATE        TO PA100WS-SAVE-EFFECT-DATE
122400         MOVE PCT-ACTION-CODE        TO PA100WS-SAVE-ACTION-CODE
122500         MOVE PCT-EMPLOYEE           TO PA100WS-SAVE-EMPLOYEE
122600         MOVE PCT-ACTION-NBR         TO PA100WS-SAVE-ACTION-NBR
122700         PERFORM 2400-PROCESS-EMPLOYEE
122800             UNTIL (PERSACTION-NOTFOUND)
122900             OR    (PCT-ACTION-TYPE    NOT = DB-ACTION-TYPE)
123000             OR    (PCT-EFFECT-DATE    > WS-PRM-EFFECT-DATE).
123100
123200     IF (PRM-UPDATE-OPTION = "Y")
               PERFORM 900-SAVE-PRINT-FILES
123300         PERFORM 925-AUDIT-END

J73509         CLOSE PA100FLDS-FILE SAVE.
123400
123500 1800-END.
123600
123700*****************************************************************
123800 2000-PROCESS-EMPL-ACTION        SECTION.
123900*****************************************************************
124000 2000-START.
124100
124200     MOVE WS-COMPANY-NAME        TO R1G1-PRS-NAME
124200                                    R6G1-PRS-NAME.
124300     MOVE PRM-COMPANY            TO R1G1-PCT-COMPANY
124200                                    R6G1-PCT-COMPANY.
P58564     MOVE WS-HREMP-MSG-329       TO R1G1-TITLE.
           MOVE PRM-UPDATE-OPTION      TO R1G1-UPDATE-OPTION
                                          R6G1-UPDATE-OPTION.

126000     IF (PRM-UPDATE-OPTION = "Y")
               PERFORM 2100-CHECK-RESTART
               IF (WS-RESTART-PHASE-A NOT NUMERIC)
                   MOVE ZEROES             TO WS-RESTART-PHASE
               END-IF
               IF (WS-RESTART-PHASE    = 4)
                   GO TO 2000-END
               END-IF
J73509         OPEN EXTEND PA100FLDS-FILE
126100         PERFORM 910-AUDIT-BEGIN
126200         PERFORM 850-MODIFY-BEGRNG-PCTSET1
126300     ELSE
126400         PERFORM 850-FIND-BEGRNG-PCTSET1.
126500
126600     MOVE ZEROS                  TO WS-LAST-PART.
126700     IF  (PERSACTION-FOUND)
126800     AND (PCT-ACTION-TYPE = DB-ACTION-TYPE)
126900     AND (PCT-EFFECT-DATE <= WS-PRM-EFFECT-DATE)
127000         MOVE R1GN1-PCT-COMPANY      TO RPT-GROUP-REQUEST
127100         PERFORM 700-PRINT-RPT-GRP
127000         MOVE R6GN1-PCT-COMPANY      TO RPT-GROUP-REQUEST
127100         PERFORM 700-PRINT-RPT-GRP
               MOVE R7GN1-PCT-COMPANY      TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
127200         MOVE PCT-COMPANY            TO PA100WS-SAVE-COMPANY
127300         MOVE "E"                    TO PA100WS-SAVE-ACTION-TYPE
127400         MOVE PCT-EFFECT-DATE        TO PA100WS-SAVE-EFFECT-DATE
127500         MOVE PCT-ACTION-CODE        TO PA100WS-SAVE-ACTION-CODE
127600         MOVE PCT-EMPLOYEE           TO PA100WS-SAVE-EMPLOYEE
127700         MOVE PCT-ACTION-NBR         TO PA100WS-SAVE-ACTION-NBR
127800         PERFORM 2400-PROCESS-EMPLOYEE
127900             UNTIL (PERSACTION-NOTFOUND)
128000             OR    (PCT-ACTION-TYPE    NOT = DB-ACTION-TYPE)
128100             OR    (PCT-EFFECT-DATE    > WS-PRM-EFFECT-DATE).
128200
128300     IF (PRM-UPDATE-OPTION = "Y")
               PERFORM 900-SAVE-PRINT-FILES
128400         PERFORM 925-AUDIT-END
J73509         CLOSE PA100FLDS-FILE SAVE.
128500
128600 2000-END.
128700
156800*****************************************************************
156900 2100-CHECK-RESTART              SECTION.
157000*****************************************************************
157100 2100-START.
157200
714100     PERFORM 840-FIND-CKPSET1.
714300     IF  (CKPOINT-FOUND)
P96000         MOVE CKP-RESTART-INFO    TO WS-RESTART-INFO

P96000         IF (WS-RESTART-WRK-NAME NOT = SPACES)
                   MOVE WS-RESTART-WRK-NAME TO WS-PA100FLDS-NAME.

           IF (WS-PA100FLDS-NAME            = SPACES)
               INITIALIZE                  WS-RESTART-INFO
               PERFORM 900-BUILD-TMP-FILE-NAME
               MOVE WS-TMP-FILE         TO WS-PA100FLDS-NAME
                                           WS-RESTART-WRK-NAME
J73509         OPEN EXTEND PA100FLDS-FILE
J73509         CLOSE PA100FLDS-FILE SAVE

159100         PERFORM 910-AUDIT-BEGIN

709900         PERFORM 840-MODIFY-CKPSET1
               MOVE WS-PA100FLDS-NAME   TO WS-RESTART-WRK-NAME
710100         MOVE WS-RESTART-INFO     TO CKP-RESTART-INFO
710200         PERFORM 820-STORE-CKPOINT

               PERFORM 900-SAVE-PRINT-FILES
159100         PERFORM 925-AUDIT-END.

160600 2100-END.
160700
128800*****************************************************************
128900 2400-PROCESS-EMPLOYEE           SECTION.
129000*****************************************************************
129100 2400-START.
129200
           IF  (PRM-USER-ID NOT = SPACES)
           AND (PRM-USER-ID NOT = PCT-USER-ID)
               GO TO 2400-NEXT.

129300     IF  (WS-SPEC-ACT-FOUND)
129400     AND (PCT-ACTION-CODE NOT = PRM-EMP-ACTION (1))
129500     AND (PCT-ACTION-CODE NOT = PRM-EMP-ACTION (2))
129600     AND (PCT-ACTION-CODE NOT = PRM-EMP-ACTION (3))
129700     AND (PCT-ACTION-CODE NOT = PRM-EMP-ACTION (4))
129800     AND (PCT-ACTION-CODE NOT = PRM-EMP-ACTION (5))
129900     AND (PCT-ACTION-CODE NOT = PRM-EMP-ACTION (6))
130000         GO TO 2400-NEXT.

           IF (PRM-EMPLOYEE NOT = ZEROES)
               IF (PCT-EMPLOYEE NOT = PRM-EMPLOYEE)
                   GO TO 2400-NEXT
               ELSE
               IF  (PRM-EMP-ACTION-NBR NOT = ZEROES)
               AND (PRM-EMP-ACTION-NBR NOT = PCT-ACTION-NBR)
                   GO TO 2400-NEXT.

           IF (PRM-GROUP-NAME NOT = SPACES)
               MOVE PRM-COMPANY    TO DB-COMPANY
               MOVE PCT-EMPLOYEE   TO DB-EMPLOYEE
               MOVE PRM-GROUP-NAME TO DB-GROUP-NAME
               PERFORM 840-FIND-PGESET1
               IF (PGEMPLOYEE-NOTFOUND)
                   GO TO 2400-NEXT.

           SET ACTION-NOERROR TO TRUE.
           INITIALIZE HREMP-I2
                      HREMP-ERROR-SW.
134500     INITIALIZE HREMP-SCR-FIELDS
134600                HRPEM-SCR-FIELDS
                      PAPEP-SCR-FIELDS.

P63059     INITIALIZE HREMPWS-NEW-PAY-RATE
P63059                HREMPWS-OLD-PAY-RATE.
           INITIALIZE HREMP-HIST-OVERRIDE-SW.
           IF  (PCT-HIST-CORR-FLAG NOT = SPACES)
               MOVE PCT-HIST-CORR-FLAG     TO HREMP-HIST-OVERRIDE-SW
           ELSE
               MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE
               PERFORM 840-FIND-PATSET1
               IF  (PERSACTYPE-FOUND)
                   MOVE PAT-HIST-CORR-FLAG TO HREMP-HIST-OVERRIDE-SW
               END-IF
           END-IF.
           MOVE PCT-PROCESS-TYPE       TO HREMP-PROCESS-TYPE-SW.

           INITIALIZE HREMP-ACT-OBJ-ID-REV.

           MOVE PCT-COMPANY            TO DB-COMPANY.
           MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE.
           PERFORM 840-FIND-EMPSET1.
           MOVE EMP-LAST-NAME          TO HRWS-LAST-NAME.
           MOVE EMP-FIRST-NAME         TO HRWS-FIRST-NAME.
           MOVE EMP-MIDDLE-INIT        TO HRWS-MIDDLE-INIT.
           MOVE EMP-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE.
           MOVE EMP-NAME-SUFFIX        TO HRWS-NAME-SUFFIX.
           PERFORM 750-HR-FORMAT-NAME.

P55901     IF  (PRM-PROCESS-LEVEL NOT = SPACES)
P55901     AND (EMP-PROCESS-LEVEL NOT = PRM-PROCESS-LEVEL)
P55901         GO TO 2400-NEXT.
P55901
P64245     IF (PRM-EMP-PROC-GRP NOT = SPACES)
P64245         MOVE PCT-COMPANY        TO DB-COMPANY
P64245         MOVE PRM-EMP-PROC-GRP   TO DB-PROC-GROUP
P64245         MOVE EMP-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
P64245         PERFORM 840-FIND-PRPSET1
P64245         IF (PRPROCGRP-NOTFOUND)
P64245             GO TO 2400-NEXT.
P64245
P69186     IF (PCT-HOLD-FLAG = "Y")
               SET ERRORS-PRINTED          TO TRUE
               MOVE SPACES                 TO REG6-APP
               MOVE PCT-EMPLOYEE           TO REG6-EMPLOYEE
               MOVE PCT-ACTION-CODE        TO REG6-PCT-ACTION-CODE
               MOVE PCT-ACTION-NBR         TO REG6-PCT-ACTION-NBR
               MOVE PCT-ACTION-TYPE        TO REG6-ACTION-TYPE
               MOVE PCT-POS-LEVEL          TO REG6-PCT-POS-LEVEL
               MOVE PCT-EFFECT-DATE        TO REG6-PCT-EFFECT-DATE
               MOVE HRWS-FORMAT-NAME       TO REG6-EMP-NAME
               MOVE 167                    TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE            TO REG6-ERROR-DESC
               MOVE ERROR-LINE             TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
P69186         GO TO 2400-NEXT.
P69186
           IF  (HREMP-REVERSE-ACTION)
               MOVE PCT-COMPANY        TO PAPCT-REV-COMPANY
               MOVE PCT-EMPLOYEE       TO PAPCT-REV-EMPLOYEE
               MOVE PCT-EFFECT-DATE    TO PAPCT-REV-EFFECT-DATE
               MOVE PCT-ACTION-CODE    TO PAPCT-REV-ACTION-CODE
               MOVE PCT-MERGE-ACTN-NBR TO PAPCT-REV-ACTION-NBR
               MOVE PCT-POS-LEVEL      TO PAPCT-REV-POS-LEVEL
               PERFORM 7000-PCT-EDIT-REVERSE
               IF  (HREMP-ERROR-FOUND)
                   MOVE PCT-EMPLOYEE       TO HREMP-EMPLOYEE
                   MOVE PCT-ACTION-CODE    TO WS-ERR-ACTION-CODE
                   MOVE PCT-ACTION-NBR     TO WS-ERR-ACTION-NBR
                   MOVE PCT-ACTION-TYPE    TO WS-ERR-ACTION-TYPE
                   MOVE PCT-POS-LEVEL      TO WS-ERR-POS-LEVEL
                   MOVE PCT-EFFECT-DATE    TO WS-ERR-EFFECT-DATE
                   PERFORM 6000-PRINT-MESSAGES
                   GO TO 2400-NEXT
               END-IF
           END-IF.
137700
134700     MOVE "I"                    TO HREMP-FC
134800                                    PAPEP-FC.
134900     MOVE PCT-COMPANY            TO HREMP-COMPANY
135000                                    PAPEP-COMPANY.
135100     MOVE PCT-EMPLOYEE           TO HREMP-EMPLOYEE
135200                                    PAPEP-EMPLOYEE.
135300     MOVE ZEROS                  TO CRT-ERROR-NBR.
135400     INITIALIZE                     PAPEP-UPDPEP-DATE
                                          HREMP-UPDPEP-DATE.
135500     SET PAPEP-FROM-ACTION TO TRUE.
135600     MOVE PCT-COMPANY            TO PADT-COMPANY.
135700     MOVE PCT-EMPLOYEE           TO PADT-EMPLOYEE.
135800     MOVE EMP-TERM-DATE          TO PADT-END-DATE.
135900     MOVE PCT-POS-LEVEL          TO PADT-POS-LEVEL.
136000     MOVE PCT-EFFECT-DATE        TO PADT-EFFECT-DATE.
136100     PERFORM 2300-PADT-DATE-CHECK.
136200
136400     MOVE ZEROS                  TO CRT-ERROR-NBR.
136500     MOVE PADT-UPDPEP-DATE       TO HREMP-UPDPEP-DATE.
136600     MOVE 1                      TO HREMP-POS-LEVEL.
136700     PERFORM 2000-HREMP-EDIT-TRAN.
           MOVE HREMP-CURRENCY-CODE    TO PAPCT-EMP-CURRENCY.
           MOVE HREMP-CURR-ND          TO PAPCT-EMP-CURR-ND.
136800     IF (HREMP-UPDPEP-DATE = PCT-EFFECT-DATE)
136900       INITIALIZE             HREMP-UPDPEP-DATE.
137000     IF (ERROR-FOUND)
137100         GO TO 2400-NEXT.

137200     MOVE "PA100"                TO CRT-ERROR-CAT.
133700
           IF  (HREMP-COMBINE-ACTION)
               MOVE PCT-EMPLOYEE               TO DB-EMPLOYEE
               MOVE PCT-EFFECT-DATE            TO DB-EFFECT-DATE
               MOVE PCT-ACTION-CODE            TO DB-ACTION-CODE
               MOVE PCT-MERGE-ACTN-NBR         TO DB-ACTION-NBR
               PERFORM 840-FIND-PAHSET1
               IF  (PERSACTHST-NOTFOUND)
141800             MOVE PCT-EMPLOYEE           TO REG6-EMPLOYEE
142300             MOVE HRWS-FORMAT-NAME       TO REG6-EMP-NAME
                   MOVE 338                    TO CRT-MSG-NBR
142700             PERFORM 790-GET-MSG
142800             MOVE CRT-MESSAGE            TO REG6-ERROR-DESC
142900             MOVE PCT-ACTION-CODE        TO REG6-PCT-ACTION-CODE
                   MOVE PCT-ACTION-NBR         TO REG6-PCT-ACTION-NBR
143000             MOVE PCT-POS-LEVEL          TO REG6-PCT-POS-LEVEL
143100             MOVE PCT-EFFECT-DATE        TO REG6-PCT-EFFECT-DATE
143900             MOVE PCT-ACTION-TYPE        TO REG6-ACTION-TYPE
144000             MOVE ERROR-LINE             TO RPT-GROUP-REQUEST
144100             PERFORM 700-PRINT-RPT-GRP
                   SET ERRORS-PRINTED TO TRUE
                   SET ACTION-ERROR TO TRUE
                   GO TO 2400-NEXT
               END-IF
           END-IF.

           SET ACTION-NOERROR TO TRUE.
134200     MOVE ZEROS                  TO WS-PARTICIPNT.
134300     MOVE ZEROS                  TO WS-PCT-PARTICIPNT.
134400
           SET PA100WS-REGULAR         TO TRUE.
           INITIALIZE PA100WS-FLD-COUNT.
           INITIALIZE PA100WS-EFFECT-DATE.
           INITIALIZE PA100WS-DEFAULTS.
J64902     SET PA100WS-DEF-NOT-LOADED  TO TRUE.

           IF  (PCT-ACTION-TYPE   = "E" OR "L")
           AND (PCT-POSITION      NOT = SPACES)
               MOVE PCT-COMPANY        TO DB-COMPANY
               MOVE PCT-EMPLOYEE       TO DB-EMPLOYEE
               INITIALIZE                 DB-REQUISITION
               MOVE PCT-POSITION       TO DB-POSITION
               INITIALIZE                 DB-EFFECT-DATE
                                          DB-UPDATE-TYPE
               PERFORM 850-FIND-NLT-PPESET1
               IF (PAPOSERR-FOUND)
                   PERFORM
                       UNTIL ((PAPOSERR-NOTFOUND)
                       OR     (PPE-COMPANY     NOT = DB-COMPANY)
                       OR     (PPE-EMPLOYEE    NOT = DB-EMPLOYEE))
                       OR    ((PPE-COMPANY     = DB-COMPANY)
                       AND    (PPE-EMPLOYEE    = DB-EMPLOYEE)
                       AND    (PPE-REQUISITION = DB-REQUISITION)
                       AND    (PPE-POSITION    = DB-POSITION)
                       AND    (PPE-UPDATE-TYPE = "1  ")
                       AND    (PPE-EFFECT-DATE >= PCT-POS-EFF-DT))
                           PERFORM 860-FIND-NEXT-PPESET1
                   END-PERFORM
                   IF  (PAPOSERR-FOUND)
                   AND (PPE-COMPANY     = DB-COMPANY)
                   AND (PPE-EMPLOYEE    = DB-EMPLOYEE)
                   AND (PPE-REQUISITION = DB-REQUISITION)
                   AND (PPE-POSITION    = DB-POSITION)
                       SET PA100WS-NOUPDATE    TO TRUE
J64902                 SET PA100WS-DEF-LOADED  TO TRUE
                       MOVE PPE-EFFECT-DATE    TO PA100WS-EFFECT-DATE
                       PERFORM 2700-LOAD-DEFAULTS
                       IF (PA100WS-DEFAULT)
                         PERFORM
                            VARYING I1 FROM 1 BY 1
                            UNTIL  (I1 > 36)
                            OR    ((PCT-FLD-NBR (I1) =
                                                   HREMP-POSITION-DN)
                            AND    (PCT-NEW-VALUE (I1) NOT = SPACES))
                                CONTINUE
                       END-PERFORM
                       IF (I1 > 36)
                           SET PA100WS-ADD TO TRUE
                       ELSE
                           SET PA100WS-CHANGE TO TRUE
                       END-IF
                       PERFORM
                           VARYING I1 FROM 1 BY 1
                           UNTIL  (I1 > 98)
                           OR     (PPE-POS-LEVEL (I1) = PCT-POS-LEVEL)
                               CONTINUE
                       END-PERFORM
                       IF (I1 > 98)
                           SET PA100WS-NO-POS-LVL TO TRUE
                       ELSE
                           SET PA100WS-POS-LVL    TO TRUE
                       END-IF
                       IF (PA100WS-CHANGE)
                           PERFORM 2800-PROCESS-CHANGES
                           SET PA100WS-REGULAR TO TRUE
                       ELSE
                           PERFORM 2900-PROCESS-ADDS
                           IF (ACTION-ERROR)
                               GO TO 2400-NEXT
                           END-IF
                           MOVE GBLNK-BLANK   TO RPT-GROUP-REQUEST
                           PERFORM 700-PRINT-RPT-GRP
                           SET PA100WS-REGULAR TO TRUE
                       END-IF
                       IF  (PRM-UPDATE-OPTION = "Y")
                       AND (PA100WS-POS-LVL)
                       AND (PA100WS-UPDATE)
                           MOVE PPE-COMPANY TO DB-COMPANY
                           MOVE PPE-EMPLOYEE TO DB-EMPLOYEE
                           INITIALIZE           DB-REQUISITION
                           MOVE PPE-POSITION  TO DB-POSITION
                           MOVE PPE-EFFECT-DATE TO DB-EFFECT-DATE
                           MOVE "1  "           TO DB-UPDATE-TYPE
                           PERFORM 840-MODIFY-PPESET1
                           IF (PAPOSERR-FOUND)
                             PERFORM
                               VARYING I1 FROM 1 BY 1
                               UNTIL  (I1 > 98)
                               OR     (PPE-POS-LEVEL (I1) =
                                                      PCT-POS-LEVEL)
                                 CONTINUE
                             END-PERFORM
                             IF (I1 NOT > 98)
                                 INITIALIZE PPE-POS-LEVEL (I1)
                             END-IF
                             PERFORM 820-STORE-PAPOSERR.

137800     IF  (PCT-OCCUR-TYPE NOT = SPACES)
137900     AND (WS-LAST-PART   = ZEROS)
138000         PERFORM 840-FIND-BNCSET1
138100         IF (BNCOMPANY-NOTFOUND)
138200             IF (PCT-EMPLOYEE NOT = WS-SV-EMPLOYEE)
138300                 MOVE EMP-EMPLOYEE       TO WS-SV-EMPLOYEE
138400                 MOVE EMP-EMPLOYEE       TO REG6-EMPLOYEE
138500                 MOVE EMP-LAST-NAME      TO HRWS-LAST-NAME
138600                 MOVE EMP-FIRST-NAME     TO HRWS-FIRST-NAME
138700                 MOVE EMP-MIDDLE-INIT    TO HRWS-MIDDLE-INIT
                       MOVE EMP-LAST-NAME-PRE  TO HRWS-LAST-NAME-PRE
                       MOVE EMP-NAME-SUFFIX    TO HRWS-NAME-SUFFIX
138800                 PERFORM 750-HR-FORMAT-NAME
138900                 MOVE HRWS-FORMAT-NAME   TO REG6-EMP-NAME
139000             END-IF
139100             MOVE 148                    TO CRT-MSG-NBR
139200             PERFORM 790-GET-MSG
139300             MOVE CRT-MESSAGE            TO REG6-ERROR-DESC
139400             MOVE PCT-ACTION-CODE        TO REG6-PCT-ACTION-CODE
                   MOVE PCT-ACTION-NBR         TO REG6-PCT-ACTION-NBR
139500             MOVE PCT-POS-LEVEL          TO REG6-PCT-POS-LEVEL
139600             MOVE PCT-EFFECT-DATE        TO REG6-PCT-EFFECT-DATE
140400             MOVE PCT-ACTION-TYPE        TO REG6-ACTION-TYPE
140500             MOVE ERROR-LINE             TO RPT-GROUP-REQUEST
140600             PERFORM 700-PRINT-RPT-GRP
                   SET ERRORS-PRINTED TO TRUE
140700             GO TO 2400-NEXT
140800         ELSE
140900             MOVE BNC-LAST-PART  TO WS-LAST-PART.
141000
141100     IF (PCT-OCCUR-TYPE NOT = SPACES)
141200         MOVE PCT-OCCUR-TYPE     TO DB-OCCUR-TYPE
141300         PERFORM 840-FIND-OCCSET1
141400         IF (OCCURTYPE-NOTFOUND)
                   SET ACTION-ERROR TO TRUE
141600             IF (PCT-EMPLOYEE NOT = WS-SV-EMPLOYEE)
141700                 MOVE EMP-EMPLOYEE       TO WS-SV-EMPLOYEE
141800                 MOVE EMP-EMPLOYEE       TO REG6-EMPLOYEE
141900                 MOVE EMP-LAST-NAME      TO HRWS-LAST-NAME
142000                 MOVE EMP-FIRST-NAME     TO HRWS-FIRST-NAME
142100                 MOVE EMP-MIDDLE-INIT    TO HRWS-MIDDLE-INIT
                       MOVE EMP-LAST-NAME-PRE  TO HRWS-LAST-NAME-PRE
                       MOVE EMP-NAME-SUFFIX    TO HRWS-NAME-SUFFIX
142200                 PERFORM 750-HR-FORMAT-NAME
142300                 MOVE HRWS-FORMAT-NAME   TO REG6-EMP-NAME
142400             END-IF
142500             MOVE PCT-OCCUR-TYPE         TO CRT-ERR-VAR1
142600             MOVE 149                    TO CRT-MSG-NBR
142700             PERFORM 790-GET-MSG
142800             MOVE CRT-MESSAGE             TO REG6-ERROR-DESC
142900             MOVE PCT-ACTION-CODE         TO REG6-PCT-ACTION-CODE
                   MOVE PCT-ACTION-NBR          TO REG6-PCT-ACTION-NBR
143000             MOVE PCT-POS-LEVEL           TO REG6-PCT-POS-LEVEL
143100             MOVE PCT-EFFECT-DATE         TO REG6-PCT-EFFECT-DATE
143900             MOVE PCT-ACTION-TYPE         TO REG6-ACTION-TYPE
144000             MOVE ERROR-LINE              TO RPT-GROUP-REQUEST
144100             PERFORM 700-PRINT-RPT-GRP
                   SET ERRORS-PRINTED TO TRUE
144200         ELSE
144300             MOVE OCC-MONTHS-EXT          TO WS-OCC-MONTHS-EXT.
144400
144500     IF (PCT-OCCUR-TYPE    NOT = SPACES)
144600         IF (BNC-AUTO-PART = "Y")
144700             MOVE WS-LAST-PART   TO DB-PARTICIPNT
144800                                    WS-PARTICIPNT
144900             PERFORM 850-KFIND-NLT-PARSET1
                   SET WS-NUMBER-NOT-FOUND TO TRUE
145100             PERFORM 2420-GET-PARTICIPNT-NUMBER
145200                 UNTIL (WS-NUMBER-FOUND)
145300         ELSE
145400         IF (PCT-PARTICIPNT NOT = ZEROS)
145500             MOVE PCT-PARTICIPNT TO WS-MSG-PARTICIPNT
145600                                    WS-PARTICIPNT
145700                                    DB-PARTICIPNT
145800             PERFORM 840-KFIND-PARSET1
145900             IF (PARTICIPNT-KFOUND)
                       SET ACTION-ERROR TO TRUE
146100                 IF (PCT-EMPLOYEE NOT = WS-SV-EMPLOYEE)
146200                     MOVE EMP-EMPLOYEE       TO WS-SV-EMPLOYEE
146300                     MOVE EMP-EMPLOYEE       TO REG6-EMPLOYEE
146400                     MOVE EMP-LAST-NAME      TO HRWS-LAST-NAME
146500                     MOVE EMP-FIRST-NAME     TO HRWS-FIRST-NAME
146600                     MOVE EMP-MIDDLE-INIT    TO HRWS-MIDDLE-INIT
                           MOVE EMP-LAST-NAME-PRE  TO HRWS-LAST-NAME-PRE
                           MOVE EMP-NAME-SUFFIX    TO HRWS-NAME-SUFFIX
146700                     PERFORM 750-HR-FORMAT-NAME
146800                     MOVE HRWS-FORMAT-NAME   TO REG6-EMP-NAME
146900                 END-IF
147000                 MOVE WS-MSG-PARTICIPNT TO CRT-ERR-VAR1
147100                 MOVE 150               TO CRT-MSG-NBR
147200                 PERFORM 790-GET-MSG
147300                 MOVE CRT-MESSAGE       TO REG6-ERROR-DESC
147400                 MOVE PCT-ACTION-CODE   TO REG6-PCT-ACTION-CODE
                       MOVE PCT-ACTION-NBR    TO REG6-PCT-ACTION-NBR
147500                 MOVE PCT-POS-LEVEL     TO REG6-PCT-POS-LEVEL
147600                 MOVE PCT-EFFECT-DATE   TO REG6-PCT-EFFECT-DATE
148400                 MOVE PCT-ACTION-TYPE   TO REG6-ACTION-TYPE
148500                 MOVE ERROR-LINE        TO RPT-GROUP-REQUEST
148600                 PERFORM 700-PRINT-RPT-GRP
                       SET ERRORS-PRINTED TO TRUE
148700             ELSE
148800                 NEXT SENTENCE
148900         ELSE
                   SET ACTION-ERROR TO TRUE
149100             IF (PCT-EMPLOYEE NOT = WS-SV-EMPLOYEE)
149200                 MOVE EMP-EMPLOYEE       TO WS-SV-EMPLOYEE
149300                 MOVE EMP-EMPLOYEE       TO REG6-EMPLOYEE
149400                 MOVE EMP-LAST-NAME      TO HRWS-LAST-NAME
149500                 MOVE EMP-FIRST-NAME     TO HRWS-FIRST-NAME
149600                 MOVE EMP-MIDDLE-INIT    TO HRWS-MIDDLE-INIT
                       MOVE EMP-LAST-NAME-PRE  TO HRWS-LAST-NAME-PRE
                       MOVE EMP-NAME-SUFFIX    TO HRWS-NAME-SUFFIX
149700                 PERFORM 750-HR-FORMAT-NAME
149800                 MOVE HRWS-FORMAT-NAME   TO REG6-EMP-NAME
149900             END-IF
150000             MOVE 151                    TO CRT-MSG-NBR
150100             PERFORM 790-GET-MSG
150200             MOVE CRT-MESSAGE            TO REG6-ERROR-DESC
150300             MOVE PCT-ACTION-CODE        TO REG6-PCT-ACTION-CODE
                   MOVE PCT-ACTION-NBR         TO REG6-PCT-ACTION-NBR
150400             MOVE PCT-POS-LEVEL          TO REG6-PCT-POS-LEVEL
150500             MOVE PCT-EFFECT-DATE        TO REG6-PCT-EFFECT-DATE
151300             MOVE PCT-ACTION-TYPE        TO REG6-ACTION-TYPE
151400             MOVE ERROR-LINE             TO RPT-GROUP-REQUEST
151500             PERFORM 700-PRINT-RPT-GRP
                   SET ERRORS-PRINTED TO TRUE
               END-IF.
151600
           IF  (PCT-ACTION-TYPE            = "E" OR "L")
           AND (PCT-APPROVAL-FLAG          = "N")
               SET ACTION-ERROR TO TRUE
141600         IF (PCT-EMPLOYEE            NOT = WS-SV-EMPLOYEE)
141700             MOVE EMP-EMPLOYEE       TO WS-SV-EMPLOYEE
141800             MOVE EMP-EMPLOYEE       TO REG6-EMPLOYEE
141900             MOVE EMP-LAST-NAME      TO HRWS-LAST-NAME
142000             MOVE EMP-FIRST-NAME     TO HRWS-FIRST-NAME
142100             MOVE EMP-MIDDLE-INIT    TO HRWS-MIDDLE-INIT
                   MOVE EMP-LAST-NAME-PRE  TO HRWS-LAST-NAME-PRE
                   MOVE EMP-NAME-SUFFIX    TO HRWS-NAME-SUFFIX
142200             PERFORM 750-HR-FORMAT-NAME
142300             MOVE HRWS-FORMAT-NAME   TO REG6-EMP-NAME
142400         END-IF
142600         MOVE 154                    TO CRT-MSG-NBR
142700         PERFORM 790-GET-MSG
142800         MOVE CRT-MESSAGE            TO REG6-ERROR-DESC
142900         MOVE PCT-ACTION-CODE        TO REG6-PCT-ACTION-CODE
               MOVE PCT-ACTION-NBR         TO REG6-PCT-ACTION-NBR
143000         MOVE PCT-POS-LEVEL          TO REG6-PCT-POS-LEVEL
143100         MOVE PCT-EFFECT-DATE        TO REG6-PCT-EFFECT-DATE
143900         MOVE PCT-ACTION-TYPE        TO REG6-ACTION-TYPE
144000         MOVE ERROR-LINE             TO RPT-GROUP-REQUEST
144100         PERFORM 700-PRINT-RPT-GRP
               SET ERRORS-PRINTED TO TRUE
               GO TO 2400-NEXT
           END-IF.

151700     IF (PCT-POS-LEVEL > 01)
151800         INITIALIZE                  PAPEP-SCR-FIELDS
151900         MOVE PCT-COMPANY         TO DB-COMPANY
152000         MOVE PCT-EMPLOYEE        TO DB-EMPLOYEE
152100         MOVE PCT-POS-LEVEL       TO DB-POS-LEVEL
P58564         MOVE PEPSET3-POS-LEVEL   TO WS-DB-BEG-RNG
152200         MOVE WS-HIGH-VALUES      TO DB-EFFECT-DATE
P58564         PERFORM 850-FIND-BEGRNG-PEPSET3
152400         IF  (PAEMPPOS-FOUND)
152800             MOVE PEP-EFFECT-DATE   TO PA100WS-PEP-EFFECT-DATE
152900         ELSE
153000             INITIALIZE                PAEMPPOS
                                             PA100WS-PEP-EFFECT-DATE
153100         END-IF
153200         MOVE PCT-COMPANY            TO PADT-COMPANY
153300         MOVE PCT-EMPLOYEE           TO PADT-EMPLOYEE
153400         MOVE EMP-TERM-DATE          TO PADT-END-DATE
153500         MOVE PCT-POS-LEVEL          TO PADT-POS-LEVEL
153600         MOVE PCT-EFFECT-DATE        TO PADT-EFFECT-DATE
153700         PERFORM 2300-PADT-DATE-CHECK
153800         MOVE ZEROS                  TO CRT-ERROR-NBR
153900         IF (ERROR-FOUND)
154000             INITIALIZE                 PAPEP-UPDPEP-DATE
154100         ELSE
154200             MOVE PADT-UPDPEP-DATE   TO PAPEP-UPDPEP-DATE
154300         END-IF
154400         IF (PADT-UPDPEP-DATE        NOT = ZEROES)
154500         OR (PA100WS-PEP-EFFECT-DATE = PCT-EFFECT-DATE)
154600             MOVE "C"                      TO PAPEP-FC
154700         ELSE
154800             MOVE "A"                      TO PAPEP-FC.
154900
155000     INITIALIZE                            PAPEPWS-SUM-FTE
155100                                           HREMPWS-SUM-FTE.
155200     MOVE PCT-COMPANY                  TO DB-COMPANY.
155300     MOVE PCT-EMPLOYEE                 TO DB-EMPLOYEE.
155400     MOVE PEPSET2-EMPLOYEE             TO WS-DB-BEG-RNG.
155500     PERFORM 850-FIND-BEGRNG-PEPSET2.
155600     INITIALIZE                           PAPEPWS-SUM-FTE.
155700     IF (PAEMPPOS-FOUND)
155800         PERFORM 2405-CHECK-ADD
155900             UNTIL (PAEMPPOS-NOTFOUND)
156000             OR    (PEP-COMPANY  NOT = DB-COMPANY)
156100             OR    (PEP-EMPLOYEE NOT = DB-EMPLOYEE).
156200
471200     IF (PRM-UPDATE-OPTION       = "Y")
               IF  (HREMP-COMBINE-ACTION)
                   MOVE PAH-OBJ-ID         TO WS-OBJ-ID
               ELSE
P63874             PERFORM 7500-GET-ACTION-OBJ-ID
                   MOVE IFOBIWS-OBJ-ID     TO WS-OBJ-ID
               END-IF
           END-IF.

156300     IF (PCT-POS-LEVEL > 01)
156500         MOVE PCT-COMPANY            TO PAPEP-COMPANY
156600         MOVE 1                      TO PAPEP-COMPANY-FN
156700         MOVE PCT-EMPLOYEE           TO PAPEP-EMPLOYEE
156800         MOVE 1                      TO PAPEP-EMPLOYEE-FN
156900         MOVE PCT-EFFECT-DATE        TO PAPEP-EFFECT-DATE
157000         MOVE PCT-POS-LEVEL          TO PAPEP-POS-LEVEL
157100         MOVE PCT-ACTION-CODE        TO PAPEP-ACTION-CODE
157200         MOVE PCT-ACTION-NBR         TO PAPEP-ACTION-NBR
157300         MOVE PCT-REASON (1)         TO PAPEP-REASON1
157400         MOVE PCT-REASON (2)         TO PAPEP-REASON2
101470*        MOVE PCT-USER-ID            TO PAPEP-USER-ID
101470         MOVE CRT-USER-NAME          TO PAPEP-USER-ID 
157600         MOVE PCT-ANT-END-DATE       TO PAPEP-ANT-END-DATE
               MOVE PCT-POS-LEVEL-MOVE     TO PAPEP-POS-LEVEL-MOVE
               MOVE "Y"                    TO PAPEP-ACTION
157700         MOVE PCT-UPDATE-BENEFIT     TO HREMP-UPDATE-BENEFIT
               MOVE PCT-UPD-ABS-MGMT       TO HREMP-UPDATE-ABSENCE-MGMT
159600         MOVE "N"                    TO HREMP-UPDATE-EMP-GROUPS
157800     ELSE
               MOVE PCT-EDM-EFFECT-DT      TO PRPXL-PARM-EFF-DATE
               MOVE PCT-EDM-END-DATE       TO PRPXL-PARM-END-DATE
157900         INITIALIZE HREMP-SCR-FIELDS
158000                    HRPEM-SCR-FIELDS
158200         MOVE "C"                    TO HREMP-FC
158300         MOVE PCT-COMPANY            TO HREMP-COMPANY
158400         MOVE 1                      TO HREMP-COMPANY-FN
158500         MOVE PCT-EMPLOYEE           TO HREMP-EMPLOYEE
158600         MOVE 1                      TO HREMP-EMPLOYEE-FN
158700         MOVE PCT-POS-LEVEL          TO HREMP-POS-LEVEL
158800         MOVE PCT-EFFECT-DATE        TO HREMP-EFFECT-DATE
158900         MOVE PCT-ACTION-CODE        TO HREMP-ACTION-CODE
159000         MOVE PCT-ACTION-NBR         TO HREMP-ACTION-NBR
159100         MOVE PCT-REASON (1)         TO HREMP-REASON1
159200         MOVE PCT-REASON (2)         TO HREMP-REASON2
101470*        MOVE PCT-USER-ID            TO HREMP-USER-ID
101470         MOVE CRT-USER-NAME          TO HREMP-USER-ID
159400         MOVE PCT-ANT-END-DATE       TO HREMP-ANT-END-DATE
159500         MOVE PCT-UPDATE-BENEFIT     TO HREMP-UPDATE-BENEFIT
               MOVE PCT-UPD-ABS-MGMT       TO HREMP-UPDATE-ABSENCE-MGMT
159600         MOVE "Y"                    TO HREMP-ACTION
159600         MOVE "N"                    TO HREMP-UPDATE-EMP-GROUPS
               MOVE PCT-POS-LEVEL-MOVE     TO HREMP-POS-LEVEL-MOVE.
159700
           IF  (HREMP-REVERSE-ACTION)
               MOVE PAPCT-REV-ACT-OBJ-ID   TO HREMP-ACT-OBJ-ID-REV
           END-IF.

           MOVE PCT-ACTION-CODE        TO WS-ERR-ACTION-CODE.
           MOVE PCT-ACTION-NBR         TO WS-ERR-ACTION-NBR.
           MOVE PCT-ACTION-TYPE        TO WS-ERR-ACTION-TYPE.
           MOVE PCT-POS-LEVEL          TO WS-ERR-POS-LEVEL.
           MOVE PCT-EFFECT-DATE        TO WS-ERR-EFFECT-DATE.
           PERFORM 6000-PRINT-MESSAGES.

           INITIALIZE HREMP-I2.
           SET NOT-EDITING-USERFIELDS TO TRUE.
P60778     SET NO-PEP-FLD-CHG         TO TRUE.
159800     PERFORM 2410-MOVE-SCR-TO-WS
159900         VARYING I1 FROM 1 BY 1
160000         UNTIL  (I1 > 36).

           MOVE PCT-ACTION-CODE        TO WS-ERR-ACTION-CODE.
           MOVE PCT-ACTION-NBR         TO WS-ERR-ACTION-NBR.
           MOVE PCT-ACTION-TYPE        TO WS-ERR-ACTION-TYPE.
           MOVE PCT-POS-LEVEL          TO WS-ERR-POS-LEVEL.
           MOVE PCT-EFFECT-DATE        TO WS-ERR-EFFECT-DATE.
           PERFORM 6000-PRINT-MESSAGES.

           IF  (HREMP-ERROR-FOUND)
               GO TO 2400-NEXT.
160100
160200     IF (PCT-POS-LEVEL > 1)
160300         MOVE PCT-COMPANY        TO DB-COMPANY
160400         MOVE EMP-EMP-STATUS     TO DB-EMP-STATUS
160500         PERFORM 840-FIND-EMSSET1
160600         IF (EMSTATUS-FOUND)
160700             MOVE EMS-COUNT      TO PAPEP-COUNT.
160800
           IF  (PCT-POS-LEVEL > 1)
           AND (PCT-BASE-CURRENCY NOT = SPACES)
               MOVE PCT-BASE-CURRENCY  TO PAPEP-BASE-CURRENCY
               MOVE PCT-BASE-ND        TO PAPEP-BASE-ND
               MOVE PCT-BASE-PAY-RATE  TO PAPEP-BASE-PAY-RATE
               MOVE 1                  TO PAPEP-BASE-CURRENCY-FN
                                          PAPEP-BASE-PAY-RATE-FN
               SET PAPEP-BASE-OVERRIDE TO TRUE
           END-IF.

160900     INITIALIZE                     PAPEP-UPDPEP-DATE.
           SET PAPEP-FROM-ACTION TO TRUE.
161100     MOVE PCT-COMPANY            TO PADT-COMPANY.
161200     MOVE PCT-EMPLOYEE           TO PADT-EMPLOYEE.
161300     MOVE EMP-TERM-DATE          TO PADT-END-DATE.
161400     MOVE PCT-POS-LEVEL          TO PADT-POS-LEVEL.
161500     MOVE PCT-EFFECT-DATE        TO PADT-EFFECT-DATE.
161600     PERFORM 2300-PADT-DATE-CHECK.
161700
161900     MOVE ZEROS                  TO CRT-ERROR-NBR.
162000     IF (PCT-FLD-NBR (1) NOT = ZEROS)
162100         IF (PCT-POS-LEVEL > 01)
162200             IF  (PAEMPPOS-FOUND)
162300             AND (PEP-COMPANY       = DB-COMPANY)
162400             AND (PEP-EMPLOYEE      = DB-EMPLOYEE)
162500             AND (PEP-POS-LEVEL     = DB-POS-LEVEL)
162600                 PERFORM 7000-PAPEP-DEFAULT
162700             END-IF
162800             MOVE PADT-UPDPEP-DATE
162900                                  TO PAPEP-UPDPEP-DATE
163000             MOVE PCT-POS-LEVEL   TO PAPEP-POS-LEVEL
163100             COMPUTE PAPEPWS-SUM-FTE =
163200               (PAPEPWS-SUM-FTE + PAPEP-NBR-FTE)
163300             PERFORM 2000-PAPEP-EDIT-TRAN
                   IF  (NOT HREMP-ERROR-FOUND)
                   AND (NOT PAPEP-NEW-ADD)
                   AND (PAPEP-PAY-RATE   NOT = PEP-PAY-RATE)
                       MOVE 1              TO PAPEP-PAY-RATE-FN
                   END-IF
163400             IF (PAPEP-UPDPEP-DATE = PCT-EFFECT-DATE)
163500                 INITIALIZE         PAPEP-UPDPEP-DATE
163600             END-IF
                   MOVE EMP-LAST-NAME      TO HRWS-LAST-NAME
                   MOVE EMP-FIRST-NAME     TO HRWS-FIRST-NAME
                   MOVE EMP-MIDDLE-INIT    TO HRWS-MIDDLE-INIT
                   MOVE EMP-LAST-NAME-PRE  TO HRWS-LAST-NAME-PRE
                   MOVE EMP-NAME-SUFFIX    TO HRWS-NAME-SUFFIX
                   PERFORM 750-HR-FORMAT-NAME
163700         ELSE
163800             MOVE PADT-UPDPEP-DATE
163900                                  TO HREMP-UPDPEP-DATE
164000             MOVE 1               TO HREMP-POS-LEVEL
164100             COMPUTE HREMPWS-SUM-FTE =
164200               (HREMPWS-SUM-FTE + HREMP-NBR-FTE)
164300             PERFORM 2000-HREMP-EDIT-TRAN
                   MOVE HREMP-LAST-NAME        TO HRWS-LAST-NAME
                   MOVE HREMP-FIRST-NAME       TO HRWS-FIRST-NAME
                   MOVE HREMP-MIDDLE-INIT      TO HRWS-MIDDLE-INIT
                   MOVE HREMP-LAST-NAME-PRE    TO HRWS-LAST-NAME-PRE
                   MOVE HREMP-NAME-SUFFIX      TO HRWS-NAME-SUFFIX
                   PERFORM 750-HR-FORMAT-NAME
                   MOVE HREMP-CURRENCY-CODE    TO PAPCT-EMP-CURRENCY
                   MOVE HREMP-CURR-ND          TO PAPCT-EMP-CURR-ND
164400             IF (HREMP-UPDPEP-DATE = PCT-EFFECT-DATE)
164500                 INITIALIZE         HREMP-UPDPEP-DATE.
164600
           SET EDITING-USERFIELDS TO TRUE.
           PERFORM 2410-MOVE-SCR-TO-WS
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36).

           MOVE PCT-ACTION-CODE        TO WS-ERR-ACTION-CODE.
           MOVE PCT-ACTION-NBR         TO WS-ERR-ACTION-NBR.
           MOVE PCT-ACTION-TYPE        TO WS-ERR-ACTION-TYPE.
           MOVE PCT-POS-LEVEL          TO WS-ERR-POS-LEVEL.
           MOVE PCT-EFFECT-DATE        TO WS-ERR-EFFECT-DATE.
           PERFORM 6000-PRINT-MESSAGES.
           IF  (HREMP-ERROR-FOUND)
               GO TO 2400-NEXT.

J27541     SET DIFF-VALUE              TO TRUE.
J27541
J27541     PERFORM 2645-CHECK-ACTION-DETAIL
J27541         VARYING I1 FROM 1 BY 1
J27541         UNTIL  (I1 > 36)
J27541         OR     (SAME-VALUE).

168700     IF (PCT-EMPLOYEE NOT = WS-SV-EMPLOYEE-PRT)
168800         MOVE EMP-EMPLOYEE                 TO WS-SV-EMPLOYEE-PRT
168900         MOVE EMP-EMPLOYEE                 TO R1G2-PCT-EMPLOYEE
168900                                              R6G2-PCT-EMPLOYEE
                                                    R7G2-PCT-EMPLOYEE
169000         MOVE EMP-LAST-NAME                TO HRWS-LAST-NAME
169100         MOVE EMP-FIRST-NAME               TO HRWS-FIRST-NAME
169200         MOVE EMP-MIDDLE-INIT              TO HRWS-MIDDLE-INIT
               MOVE EMP-LAST-NAME-PRE            TO HRWS-LAST-NAME-PRE
               MOVE EMP-NAME-SUFFIX              TO HRWS-NAME-SUFFIX
169300         PERFORM 750-HR-FORMAT-NAME
169400         MOVE HRWS-FORMAT-NAME             TO R1G2-EMP-NAME
169400                                              R6G2-EMP-NAME
                                                    R7G2-EMP-NAME
J27541         IF (DIFF-VALUE)
J27541             MOVE R1GN2-PCT-EMPLOYEE       TO RPT-GROUP-REQUEST
J27541             PERFORM 700-PRINT-RPT-GRP
169500             MOVE R6GN2-PCT-EMPLOYEE       TO RPT-GROUP-REQUEST
169600             PERFORM 700-PRINT-RPT-GRP
169500             MOVE R7GN2-PCT-EMPLOYEE       TO RPT-GROUP-REQUEST
169600             PERFORM 700-PRINT-RPT-GRP
J27541         END-IF
J27541     END-IF.
169700

J27541*J00148  SET DIFF-VALUE                        TO TRUE.
J27541*J00148  PERFORM 2645-CHECK-ACTION-DETAIL
J27541*J00148      VARYING I1 FROM 1 BY 1
J27541*J00148      UNTIL  (I1 > 36)
J27541*J00148      OR     (SAME-VALUE).
J27541*J00148
J00148     IF (DIFF-VALUE)
               PERFORM 2960-PRINT-ACTION
J64902     END-IF.
169900
170000     MOVE "PA100"                          TO CRT-ERROR-CAT.
170100
170200*** RETURN NEW ACCT-CATEGORY VALUES IN CASE EDIT HAS DEFAULTED ***
170300*** SO REPORT REFLECTS TRUE CHANGE                             ***
170400
170500     PERFORM
170600       VARYING I9 FROM 1 BY 1
170700       UNTIL  (I9 > 36)
170800       OR     (PCT-FLD-NBR (I9) = ZEROES)
170900          IF  (PCT-FLD-NBR (I9) = HREMP-ACCT-CATEGORY-DN)
171000          AND (PCT-POS-LEVEL    = 01)
P82120          AND (HREMP-FLD-CHG (HREMP-ACCT-CATEGORY-DN))
P82120          AND (HREMP-ACCT-CATEGORY NOT = SPACES)   
171100              MOVE HREMP-ACCT-CATEGORY TO PCT-NEW-VALUE (I9)
171200          END-IF
171300     END-PERFORM.
171400
171500     IF (PCT-POS-LEVEL > 01)
171600         MOVE PCT-COMPANY                  TO DB-COMPANY
171700         MOVE PCT-EMPLOYEE                 TO DB-EMPLOYEE
171800         MOVE PCT-POS-LEVEL                TO DB-POS-LEVEL
P58564         MOVE PEPSET3-POS-LEVEL            TO WS-DB-BEG-RNG 
P58564         PERFORM 850-FIND-BEGRNG-PEPSET3.
172100
172200     PERFORM 2650-PRINT-ACTION-DETAIL
172300         VARYING I1 FROM 1 BY 1
172400         UNTIL  (I1 > 36) 
J00148         OR (SAME-VALUE).
172500
172600     IF (HREMP-IS-RETRO)
172700     OR (PCT-PROCESS-TYPE NOT = SPACES)
172800         MOVE PCT-EMPLOYEE             TO R4G7-PCT-EMPLOYEE
172900         MOVE HRWS-FORMAT-NAME         TO R4G7-EMP-NAME
173200         MOVE PCT-EFFECT-DATE          TO R4G7-PCT-EFFECT-DATE
173000         MOVE PCT-ACTION-CODE          TO R4G7-PCT-ACTION-CODE
               MOVE PCT-ACTION-NBR           TO R4G7-PCT-ACTION-NBR
173100         MOVE PCT-POS-LEVEL            TO R4G7-PCT-POS-LEVEL
               IF  (PCT-PROCESS-TYPE = "3")
                   MOVE PCT-MERGE-ACTN-NBR   TO R4G7-COMBINE-ACTION-NBR
               ELSE
                   INITIALIZE                   R4G7-COMBINE-ACTION-NBR
               END-IF
               IF  (PCT-PROCESS-TYPE = "4")
                   MOVE PCT-MERGE-ACTN-NBR   TO R4G7-REVERSE-ACTION-NBR
               ELSE
                   INITIALIZE                   R4G7-REVERSE-ACTION-NBR
               END-IF
               MOVE PCT-HIST-CORR-FLAG       TO R4G7-PCT-HIST-CORR-FLAG
               INITIALIZE                       R4G7-RUN-PA113
               SET RETRO-PCT-NOT-PRINTED TO TRUE
               IF  (HREMP-IS-RETRO)
               OR  (PCT-PROCESS-TYPE NOT = SPACES)
                   IF  (HREMP-RETRO-PENDING)
                       MOVE "Y"                  TO R4G7-RUN-PA113
                   END-IF
                   PERFORM
                       VARYING I1 FROM 1 BY 1
                       UNTIL  (I1 > 36)
                       IF  (PCT-FLD-NBR (I1) NOT = ZEROES)
                           MOVE PCT-FLD-NBR (I1)     TO DB-FLD-NBR
                           IF  (HREMP-FLD-RETRO (DB-FLD-NBR))
                               PERFORM 840-FIND-PADSET1
                               INITIALIZE              R4G7-RETRO-FIELD
                                                       R4G8-RETRO-FIELD
                               IF  (PADICT-FOUND)
                                   MOVE PAD-ITEM-NAME
                                                   TO CRT-PHRASE
                                   MOVE WS-PHRASE-SIZE
                                                   TO CRT-PHRASE-SIZE
                                   MOVE "N"        TO CRT-PUT-DOTS
                                   PERFORM 900-GET-PHRASE-XLT
                                   MOVE CRT-PHRASE-XLT
                                                   TO R4G7-RETRO-FIELD
                                                      R4G8-RETRO-FIELD
                               END-IF
                               IF  (RETRO-PCT-NOT-PRINTED)
                                   MOVE R4GN7-PCT-ACTION
                                                   TO RPT-GROUP-REQUEST
                                   SET RETRO-PCT-PRINTED TO TRUE
                               ELSE
                                   MOVE R4GN8-PCT-FIELD
                                                   TO RPT-GROUP-REQUEST
                               END-IF
                               PERFORM 700-PRINT-RPT-GRP
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
               IF  (RETRO-PCT-NOT-PRINTED)
173300             MOVE R4GN7-PCT-ACTION     TO RPT-GROUP-REQUEST
173400             PERFORM 700-PRINT-RPT-GRP
               END-IF
               SET HISTAUDITS-PRINTED TO TRUE
           END-IF.
173500
173600     MOVE ZEROES                           TO DB-EMP-APP.
173700     MOVE "PA"                             TO DB-CMT-TYPE.
173800     MOVE PCT-ACTION-CODE                  TO DB-ACTION-CODE.
173900     MOVE PCT-EFFECT-DATE                  TO DB-DATE.
174000     MOVE PCT-EMPLOYEE                     TO DB-EMPLOYEE.
174100     MOVE SPACES                           TO DB-JOB-CODE.
174200     MOVE PCT-ACTION-NBR                   TO DB-LN-NBR.
174300     MOVE PACSET2-LN-NBR                   TO WS-DB-BEG-RNG.
174400     PERFORM 850-FIND-BEGRNG-PACSET2.
174500     PERFORM 860-FIND-NXTRNG-PACSET2
174600         UNTIL (PACOMMENTS-NOTFOUND)
174700         OR    (PAC-PRINT-CODE  NOT = "N").
174800
174900     IF (PACOMMENTS-FOUND)
175000         MOVE GN6H-PAC-SEQ-NBR   TO RPT-GROUP-REQUEST
175100         PERFORM 700-PRINT-RPT-GRP.
175200
175300     PERFORM 1755-PRINT-COMMENTS
175400         UNTIL (PACOMMENTS-NOTFOUND).
175500
175600     IF (WS-PARTICIPNT NOT = ZEROS)
175700         MOVE WS-PARTICIPNT      TO WS-LAST-PART
175800     ELSE
175900     IF (PCT-PARTICIPNT > WS-LAST-PART)
176000         MOVE PCT-PARTICIPNT     TO WS-LAST-PART.
176100
           IF  (TAX-LOCATOR)
           AND (PCT-POS-LEVEL = 01)
               PERFORM 2670-PRINT-PXL.

           IF  (PCT-UPDATE-REQ-DED     = "Y")
           AND (PRM-UPDATE-OPTION      = "N")
096500         MOVE HREMP-COMPANY      TO PRRQC-COMPANY
096600         MOVE HREMP-EMPLOYEE     TO PRRQC-EMPLOYEE
096700         INITIALIZE PRRQC-DFT-MAR-STAT
096900                    PRRQC-DFT-EXEMPTS
               MOVE PCT-EDM-EFFECT-DT  TO PRRQC-EFFECT-DATE
               MOVE PCT-EDM-END-DATE   TO PRRQC-END-DATE
               MOVE PRM-UPDATE-OPTION  TO PRRQC-UPDATE-OPTION
097000         PERFORM 500-REQ-DED-CREATION
               IF  (ERROR-FOUND)
                   SET ACTION-NOERROR TO TRUE
141600             IF  (PCT-EMPLOYEE          NOT = WS-SV-EMPLOYEE)
141700                 MOVE EMP-EMPLOYEE     TO WS-SV-EMPLOYEE
141800                 MOVE EMP-EMPLOYEE     TO REG6-EMPLOYEE
141900                 MOVE EMP-LAST-NAME    TO HRWS-LAST-NAME
142000                 MOVE EMP-FIRST-NAME   TO HRWS-FIRST-NAME
142100                 MOVE EMP-MIDDLE-INIT  TO HRWS-MIDDLE-INIT
                       MOVE EMP-LAST-NAME-PRE
                                             TO HRWS-LAST-NAME-PRE
                       MOVE EMP-NAME-SUFFIX  TO HRWS-NAME-SUFFIX
142200                 PERFORM 750-HR-FORMAT-NAME
142300                 MOVE HRWS-FORMAT-NAME TO REG6-EMP-NAME
142400             END-IF
142600             MOVE CRT-ERROR-NBR        TO CRT-MSG-NBR
142700             PERFORM 790-GET-MSG
142800             MOVE CRT-MESSAGE          TO REG6-ERROR-DESC
142900             MOVE PCT-ACTION-CODE      TO REG6-PCT-ACTION-CODE
                   MOVE PCT-ACTION-NBR       TO REG6-PCT-ACTION-NBR
143000             MOVE PCT-POS-LEVEL        TO REG6-PCT-POS-LEVEL
143100             MOVE PCT-EFFECT-DATE      TO REG6-PCT-EFFECT-DATE
143900             MOVE PCT-ACTION-TYPE      TO REG6-ACTION-TYPE
144000             MOVE ERROR-LINE           TO RPT-GROUP-REQUEST
144100             PERFORM 700-PRINT-RPT-GRP
                   SET ERRORS-PRINTED TO TRUE
               ELSE
                   PERFORM 2660-PRINT-REQ-DED.

176200     IF  ((PRM-UPDATE-OPTION NOT = "Y")
J00148     OR   (SAME-VALUE)
J00148     AND  (PRM-RUN-OPTION    NOT = "1"))
J00148     OR  ((PRM-UPDATE-OPTION NOT = "Y")
J00148     AND  (PRM-RUN-OPTION        = "1"))
176300         GO TO 2400-NEXT.
176400
176500     IF (WS-PARTICIPNT NOT = ZEROS)
176600         MOVE WS-PARTICIPNT      TO WS-PCT-PARTICIPNT.
176700
176800     IF (WS-PCT-PARTICIPNT NOT = ZEROS)
176900         PERFORM 2460-CALC-TERM-DATE.
177000
177100     IF (WS-PCT-PARTICIPNT NOT = ZEROS)
177200         PERFORM 800-CREATE-PARTICIPNT
177300         MOVE PCT-COMPANY        TO PAR-COMPANY
177400         MOVE WS-PCT-PARTICIPNT  TO PAR-PARTICIPNT
177500         MOVE EMP-LAST-NAME      TO PAR-LAST-NAME
177600         MOVE EMP-FIRST-NAME     TO PAR-FIRST-NAME
177700         MOVE EMP-MIDDLE-INIT    TO PAR-MIDDLE-INIT
177800         MOVE EMP-EMPLOYEE       TO PAR-EMPLOYEE
177900         MOVE EMP-FICA-NBR       TO PAR-FICA-NBR
178000         MOVE EMP-ADDR1          TO PAR-ADDR1
178100         MOVE EMP-ADDR2          TO PAR-ADDR2
178200         MOVE EMP-CITY           TO PAR-CITY
178300         MOVE EMP-STATE          TO PAR-STATE
178400         MOVE EMP-ZIP            TO PAR-ZIP
178500         MOVE EMP-COUNTRY-CODE   TO PAR-COUNTRY-CODE
178600         MOVE PEM-HM-PHONE-CNTRY TO PAR-HM-PHONE-CNTRY
178700         MOVE PEM-HM-PHONE-NBR   TO PAR-HM-PHONE-NBR
178800         MOVE PEM-BIRTHDATE      TO PAR-BIRTHDATE
178900         MOVE PEM-SEX            TO PAR-SEX
179000         MOVE PEM-SMOKER         TO PAR-SMOKER
179100         MOVE PCT-OCCUR-TYPE     TO PAR-OCCUR-TYPE
179200         MOVE PCT-EFFECT-DATE    TO PAR-OCCUR-DATE
179300         MOVE WS-TERM-DATE       TO PAR-TERM-DATE
J67329         MOVE WS-SYSTEM-DATE-YMD TO PAR-DATE-STAMP
J67329                                    PAR-CREATE-DATE
J67329         MOVE HHMMSS             TO PAR-TIME-STAMP
J67329                                    PAR-CREATE-TIME
J67329         MOVE CRT-USER-NAME      TO PAR-USER-ID
J67329                                    PAR-CREATE-USER-ID
J18332         MOVE WS-SYSTEM-DATE-YMD TO PAR-CREATE-DATE
J18332                                    PAR-DATE-STAMP
J18332         MOVE HHMMSS             TO PAR-CREATE-TIME
J18332                                    PAR-TIME-STAMP
J18332         MOVE CRT-USER-NAME      TO PAR-CREATE-USER-ID
J18332                                    PAR-USER-ID
179400         PERFORM 820-STORE-PARTICIPNT.
179500
179600     IF (PCT-PARTICIPNT NOT = ZEROS)
179700         PERFORM 840-MODIFY-BNCSET1
179800         MOVE WS-LAST-PART       TO BNC-LAST-PART
179900         PERFORM 820-STORE-BNCOMPANY.
180000
MG0808*****************************************************************
MG0808*   PROCESS PENDING ADDRESSCHG PA52 ACTIONS
MG0808*****************************************************************
MG0808   
MG0808     IF (PCT-ACTION-CODE = "ADDRESSCHG") AND
MG0808        (PRM-UPDATE-OPTION = "Y")
MG0808         PERFORM 2000-PF-ADDRESS-CHANGE
MG0808     END-IF.
MG0808
ACS003     IF (PCT-ACTION-CODE = "TRANSFER" OR "DIS-APPROV"   
ACS003         OR "DIS-PRTW" OR "DIS-RETIRE" OR "DIS-RTW" OR "DIS-TERM"
ACS003         OR "DIS-WAIVER" OR "DISABILITY" OR "ER-WITHDRW" OR "ITP" 
ACS003         OR "JOBSALCHG" OR "LEAVE" OR "REHIRE" OR "RETIRE"
ACS003         OR "TERM" OR "UPDPOS" OR "SPMISSTERM" OR "OFF-BOARD" 
               OR "TERMSTATUS")
ACS003         AND (PRM-UPDATE-OPTION = "Y")
ACS003            PERFORM 2100-PF-PAPFBPCUP
ACS003     END-IF.
ACS003
ACS003     IF (PCT-ACTION-CODE = "NH CORRECT" OR "NHCORRECTN")   
ACS003         AND (PRM-UPDATE-OPTION = "Y")
ACS003            PERFORM 2100-PF-PAPFBPCUP
ACS003     END-IF.
ACS003
MG0808*****************************************************************

CRP001     MOVE PCT-ACTION-CODE        TO HRCRP-ACTION-CODE.
CRP001     PERFORM 9000-CHECK-ACTION-CODE.
CRP001     IF (HRCRP-RECALC-ACTION = "N")
CRP001         GO TO 2400-SKIP-CRP-RECALC.
CRP001     MOVE PCT-COMPANY             TO HRCRP-COMPANY.
CRP001     MOVE PCT-EMPLOYEE            TO HRCRP-EMPLOYEE.
CRP001     MOVE PCT-EFFECT-DATE         TO HRCRP-EFFECT-DATE.
CRP001     PERFORM 8000-SETUP-WS-FOR-PA100.
CRP001     PERFORM 2000-CREATE-CRP-TRIGGER.
CRP001
CRP001 2400-SKIP-CRP-RECALC.
180100     PERFORM 830-DELETE-PERSACTION.
180500
           IF  (PCT-PROCESS-TYPE = "3")
               MOVE WS-OBJ-ID              TO DB-OBJ-ID
               PERFORM 840-MODIFY-PAHSET2
               IF  (PCT-ANT-END-DATE NOT = ZEROES)
                   MOVE PCT-ANT-END-DATE   TO PAH-ANT-END-DATE
               END-IF
               IF  (PCT-REASON (1) NOT = SPACES)
                   MOVE PCT-REASON (1)     TO PAH-REASON (1)
               END-IF
               IF  (PCT-REASON (2) NOT = SPACES)
                   MOVE PCT-REASON (2)     TO PAH-REASON (2)
               END-IF
101470*        IF  (PCT-USER-ID NOT = SPACES)
101470*            MOVE PCT-USER-ID        TO PAH-USER-ID
101470*        ELSE
101470*            MOVE CRT-USER-NAME      TO PAH-USER-ID
101470*        END-IF
101470         MOVE CRT-USER-NAME          TO PAH-USER-ID
               MOVE PCT-UPDATE-BENEFIT     TO PAH-UPDATE-BENEFIT
               MOVE PCT-UPD-ABS-MGMT       TO PAH-UPD-ABS-MGMT
               MOVE PCT-HIST-CORR-FLAG     TO PAH-HIST-CORR-FLAG
               IF (PCT-ACTION-TYPE = "A" OR "E" OR "L")
                   MOVE PCT-UPDATE-REQ-DED TO PAH-UPDATE-REQ-DED
                   MOVE PCT-EDM-EFFECT-DT  TO PAH-EDM-EFFECT-DT
                   MOVE PCT-EDM-END-DATE   TO PAH-EDM-END-DATE
                   MOVE "2"                TO PAH-ACTION-UPD
               END-IF
               IF (PCT-ACTION-TYPE = "M" OR "P")
                   MOVE "3"                TO PAH-ACTION-UPD
               END-IF
           ELSE
J19449         MOVE PCT-COMPANY            TO DB-COMPANY
J19449         MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE
J19449         MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE
J19449         MOVE PCT-EFFECT-DATE        TO DB-EFFECT-DATE
J19449         MOVE PAHSET4-EFFECT-DATE    TO WS-DB-BEG-RNG
J19449         PERFORM 850-KFIND-BEGRNG-PAHSET4
J19449         IF (PERSACTHST-KFOUND)
J19449             MOVE PAH-ACTION-NBR     TO PA100WS-ACTION-NBR
J19449             ADD 1                   TO PA100WS-ACTION-NBR
J19449         ELSE
J19449             MOVE PCT-ACTION-NBR     TO PA100WS-ACTION-NBR
J19449         END-IF
180600         PERFORM 800-CREATE-PERSACTHST
180700         MOVE PCT-COMPANY            TO PAH-COMPANY
180800         MOVE PCT-ACTION-TYPE        TO PAH-ACTION-TYPE
181100         MOVE PCT-ACTION-CODE        TO PAH-ACTION-CODE
J19449*        MOVE PCT-ACTION-NBR         TO PAH-ACTION-NBR
J19449         MOVE PA100WS-ACTION-NBR     TO PAH-ACTION-NBR
181300         MOVE PCT-EFFECT-DATE        TO PAH-EFFECT-DATE
181400         MOVE PCT-EMPLOYEE           TO PAH-EMPLOYEE
181500         MOVE PCT-ANT-END-DATE       TO PAH-ANT-END-DATE
181600         MOVE PCT-REASON (1)         TO PAH-REASON (1)
181700         MOVE PCT-REASON (2)         TO PAH-REASON (2)
               MOVE PCT-UPDATE-BENEFIT     TO PAH-UPDATE-BENEFIT
               MOVE PCT-UPD-ABS-MGMT       TO PAH-UPD-ABS-MGMT
               MOVE PCT-HIST-CORR-FLAG     TO PAH-HIST-CORR-FLAG
               IF (PCT-ACTION-TYPE = "A" OR "E" OR "L")
                   MOVE PCT-UPDATE-REQ-DED TO PAH-UPDATE-REQ-DED
                   MOVE PCT-EDM-EFFECT-DT  TO PAH-EDM-EFFECT-DT
                   MOVE PCT-EDM-END-DATE   TO PAH-EDM-END-DATE
                   MOVE "2"                TO PAH-ACTION-UPD
               END-IF
               IF (PCT-ACTION-TYPE = "M" OR "P")
                   MOVE "3"                TO PAH-ACTION-UPD
               END-IF
101470*        MOVE PCT-USER-ID            TO PAH-USER-ID
101470         MOVE CRT-USER-NAME          TO PAH-USER-ID
               MOVE PCT-POS-LEVEL          TO PAH-POS-LEVEL
           END-IF.

181900     MOVE WS-SYSTEM-DATE-YMD     TO PAH-DATE-STAMP.
P80029     MOVE HHMMSS                 TO PAH-TIME-STAMP.
J08104     IF  (PCT-CREATE-USER-ID NOT = SPACES)
J08104         MOVE PCT-CREATE-USER-ID TO PAH-CREATE-USER
J08104     ELSE
J08104         MOVE CRT-USER-NAME      TO PAH-CREATE-USER
J08104     END-IF.
J08104     MOVE PCT-CREATE-DATE        TO PAH-CREATE-DATE.
J08104     MOVE PCT-CREATE-TIME        TO PAH-CREATE-TIME.
182000     MOVE WS-OBJ-ID              TO PAH-OBJ-ID.
182100
183800     PERFORM 820-STORE-PERSACTHST.
183900
P57519     PERFORM
P57519         VARYING I4 FROM 1 BY 1 
P57519         UNTIL  (I4 > 1999)
P57519         MOVE   "X"                  TO HREMP-LOG-FLAG (I4)
P57519     END-PERFORM.
184600
           IF  (PAPEP-FC                    = "A")
           AND (PAPEP-EFFECT-DATE       NOT = ZEROES)
               MOVE PAPEP-COMPANY          TO DB-COMPANY
               MOVE PAPEP-EMPLOYEE         TO DB-EMPLOYEE
               MOVE PAPEP-POS-LEVEL        TO DB-POS-LEVEL
P58564         MOVE PEPSET3-POS-LEVEL      TO WS-DB-BEG-RNG
P58564         PERFORM 850-FIND-BEGRNG-PEPSET3
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
               AND  (PAPEP-USER-AMOUNT        = PEP-USER-AMOUNT)
               AND  (PAPEP-ACTIVITY           = PEP-ACTIVITY)
               AND  (PAPEP-ACCT-CATEGORY      = PEP-ACCT-CATEGORY)
               AND  ((PAPEP-CURRENCY-CODE     = PEP-CURRENCY-CODE)
               OR   (PAPEP-CURRENCY-CODE      = SPACES)
               OR   (PEP-CURRENCY-CODE        = SPACES))
               AND  ((PAPEP-BASE-CURRENCY     = PEP-BASE-CURRENCY)
               OR   (PAPEP-BASE-CURRENCY      = SPACES )
               OR   (PEP-BASE-CURRENCY        = SPACES ))
               AND  ((PAPEP-BASE-PAY-RATE     = PEP-BASE-PAY-RATE)
               OR   (PAPEP-BASE-PAY-RATE      = ZEROES )))
242800         AND  (PAPEP-END-DATE       NOT = PEP-END-DATE)
               AND  (PAPEP-BARGAIN-UNIT       = PEP-BARGAIN-UNIT)
                   IF  (PEP-END-DATE      NOT = ZEROES)
                   AND (PAPEP-END-DATE        = ZEROES)
                   AND (PAPEP-EFFECT-DATE     > PEP-END-DATE)
                       CONTINUE
                   ELSE
                       MOVE "C"            TO PAPEP-FC.

184700     IF (PCT-FLD-NBR (1) NOT = ZEROS)
184800         INITIALIZE                 PAPEP-UPDPEP-DATE
               SET PAPEP-FROM-ACTION TO TRUE
185000         MOVE PCT-COMPANY        TO PADT-COMPANY
185100         MOVE PCT-EMPLOYEE       TO PADT-EMPLOYEE
185200         MOVE EMP-TERM-DATE      TO PADT-END-DATE
185300         MOVE PCT-POS-LEVEL      TO PADT-POS-LEVEL
185400         MOVE PCT-EFFECT-DATE    TO PADT-EFFECT-DATE
185500         PERFORM 2300-PADT-DATE-CHECK
185600         MOVE ZEROS              TO CRT-ERROR-NBR
185700         IF  (PCT-POS-LEVEL > 01)
185800             MOVE PADT-UPDPEP-DATE
185900                                 TO PAPEP-UPDPEP-DATE
186000             MOVE WS-OBJ-ID      TO HREMP-ACT-OBJ-ID
                   MOVE "N"            TO HRWS-ADD
186500             PERFORM 3000-PAPEP-PROCESS-TRAN
186600             IF (PAPEP-UPDPEP-DATE = PCT-EFFECT-DATE)
186700                 INITIALIZE             PAPEP-UPDPEP-DATE
186800             END-IF
186900         ELSE
187000             MOVE PADT-UPDPEP-DATE
187100                                 TO HREMP-UPDPEP-DATE
187200             MOVE WS-OBJ-ID      TO HREMP-ACT-OBJ-ID
                   MOVE "N"            TO HRWS-ADD
J83708             SET HREMP-UPDATE-M3 TO TRUE                  
187300             PERFORM 3000-HREMP-PROCESS-TRAN
187400             IF (HREMP-UPDPEP-DATE = PCT-EFFECT-DATE)
187500                 INITIALIZE             PAPEP-UPDPEP-DATE
187600             END-IF
                   IF (PCT-UPDATE-REQ-DED     = "Y")
096500                 MOVE HREMP-COMPANY     TO PRRQC-COMPANY
096600                 MOVE HREMP-EMPLOYEE    TO PRRQC-EMPLOYEE
096700                 INITIALIZE PRRQC-DFT-MAR-STAT
096900                            PRRQC-DFT-EXEMPTS
                       MOVE PCT-EDM-EFFECT-DT TO PRRQC-EFFECT-DATE
                       MOVE PCT-EDM-END-DATE  TO PRRQC-END-DATE
                       MOVE PRM-UPDATE-OPTION TO PRRQC-UPDATE-OPTION
097000                 PERFORM 500-REQ-DED-CREATION
                       IF  (ERROR-FOUND)
                           SET ACTION-NOERROR TO TRUE
141600                     IF (PCT-EMPLOYEE   NOT = WS-SV-EMPLOYEE)
141700                         MOVE EMP-EMPLOYEE      TO WS-SV-EMPLOYEE
141800                         MOVE EMP-EMPLOYEE      TO REG6-EMPLOYEE
141900                         MOVE EMP-LAST-NAME     TO HRWS-LAST-NAME
142000                         MOVE EMP-FIRST-NAME    TO HRWS-FIRST-NAME
142100                         MOVE EMP-MIDDLE-INIT   TO
142100                                                HRWS-MIDDLE-INIT
                               MOVE EMP-LAST-NAME-PRE TO
                                                      HRWS-LAST-NAME-PRE
                               MOVE EMP-NAME-SUFFIX TO HRWS-NAME-SUFFIX
142200                         PERFORM 750-HR-FORMAT-NAME
142300                         MOVE HRWS-FORMAT-NAME TO REG6-EMP-NAME
142400                     END-IF
142600                     MOVE CRT-ERROR-NBR   TO CRT-MSG-NBR
142700                     PERFORM 790-GET-MSG
142800                     MOVE CRT-MESSAGE     TO REG6-ERROR-DESC
142900                     MOVE PCT-ACTION-CODE TO REG6-PCT-ACTION-CODE
                           MOVE PCT-ACTION-NBR  TO REG6-PCT-ACTION-NBR
143000                     MOVE PCT-POS-LEVEL   TO REG6-PCT-POS-LEVEL
143100                     MOVE PCT-EFFECT-DATE TO REG6-PCT-EFFECT-DATE
143900                     MOVE PCT-ACTION-TYPE TO REG6-ACTION-TYPE
144000                     MOVE ERROR-LINE      TO RPT-GROUP-REQUEST
144100                     PERFORM 700-PRINT-RPT-GRP
                           SET ERRORS-PRINTED TO TRUE
                           INITIALIZE              CRT-ERROR-NBR
                       ELSE
                           PERFORM 2660-PRINT-REQ-DED
                       END-IF
                   END-IF
187700         END-IF
187800         IF (PRM-RUN-OPTION NOT = 3)
187900             PERFORM 5100-DO-USER-FIELDS.

           SET NO-END-DATE-CHG         TO TRUE.

           IF (PRM-UPDATE-OPTION = "Y")
               IF (PCT-POS-LEVEL = 1)
                   PERFORM 2415-MOVE-FOR-GRP-UPD
                       VARYING I1 FROM 1 BY 1
                       UNTIL  (I1 > 36)
               ELSE
                   PERFORM 2416-LEV297-GRP-UPD
                       VARYING I1 FROM 1 BY 1
                       UNTIL  (I1 > 36)
               END-IF
P80185         PERFORM 2417-HREMP-GRP-UPD.

           IF  (PRM-UPDATE-OPTION = "Y")
               MOVE PCT-COMPANY        TO DB-COMPANY
               MOVE PCT-EMPLOYEE       TO DB-EMPLOYEE
               PERFORM 4000-PAPCT-EMP-PEND-ACT-DATE.
189400
189500     IF (PRM-UPDATE-OPTION = "Y")
189600         INITIALIZE HREMP-SCR-FIELDS
189700                    HRPEM-SCR-FIELDS
                          PAPEP-SCR-FIELDS
189800         PERFORM 7000-HREMP-DEFAULT
189900         PERFORM 7000-HRPEM-DEFAULT.
190100
190200 2400-NEXT.
190300
190400     MOVE PA100WS-SAVE-ACTION-TYPE TO DB-ACTION-TYPE.
190500     MOVE PA100WS-SAVE-EFFECT-DATE TO DB-EFFECT-DATE.
190600     MOVE PA100WS-SAVE-ACTION-CODE TO DB-ACTION-CODE.
190700     MOVE PA100WS-SAVE-EMPLOYEE    TO DB-EMPLOYEE.
190800     MOVE PA100WS-SAVE-ACTION-NBR  TO DB-ACTION-NBR.
190900     IF (PRM-UPDATE-OPTION = "Y")
               PERFORM 900-SAVE-PRINT-FILES
191000         PERFORM 925-AUDIT-END
      *--- RELEASE WORKUNIT     
J83708         INITIALIZE CRT-MESSAGE
J83708         INITIALIZE WFAPI-INPUT
J83708         MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT
J83708         INITIALIZE WFAPI-OUTPUT
J83708         PERFORM 1000-WF-RELEASE-SETUP
      *---      
191100         PERFORM 910-AUDIT-BEGIN
J73509         CLOSE   PA100FLDS-FILE SAVE
J73509         OPEN EXTEND PA100FLDS-FILE
191300         PERFORM 860-MODIFY-NXTRNG-PCTSET1
191700     ELSE
191900         PERFORM 860-FIND-NXTRNG-PCTSET1.
192200
192300     IF  (PERSACTION-FOUND)
192400     AND (PCT-ACTION-TYPE    = DB-ACTION-TYPE)
192500     AND (PCT-EFFECT-DATE    <= WS-PRM-EFFECT-DATE)
P60778         INITIALIZE                 HREMPWS-NEW-PAY-RATE
P60778                                    HREMPWS-OLD-PAY-RATE
192600         MOVE PCT-EFFECT-DATE    TO PA100WS-SAVE-EFFECT-DATE
192700         MOVE PCT-ACTION-CODE    TO PA100WS-SAVE-ACTION-CODE
192800         MOVE PCT-EMPLOYEE       TO PA100WS-SAVE-EMPLOYEE
192900         MOVE PCT-ACTION-NBR     TO PA100WS-SAVE-ACTION-NBR.
193000
193100******************************************************************
193200 2405-CHECK-ADD                  SECTION.
193300******************************************************************
193400
193500     IF (PCT-POS-LEVEL      NOT = PEP-POS-LEVEL)
193600         ADD PEP-FTE           TO PAPEPWS-SUM-FTE
193700                                  HREMPWS-SUM-FTE.
193800
193900     PERFORM 860-FIND-NXTRNG-PEPSET2.
194000
194100*****************************************************************
194200 2410-MOVE-SCR-TO-WS             SECTION.
194300*****************************************************************
194400 2410-START.
194500
194600     IF  (PCT-NEW-VALUE (I1) = SPACES)
194700         GO TO 2410-END.

           IF  ((EDITING-USERFIELDS)
            AND (PCT-FLD-NBR (I1) < 2000))
           OR  ((NOT-EDITING-USERFIELDS)
            AND (PCT-FLD-NBR (I1) > 1999))
                GO TO 2410-END.
194800
194900     INITIALIZE PAPCT-SCR-FIELDS.
195000     MOVE PCT-COMPANY            TO PAPCT-COMPANY.
195100     MOVE PCT-EMPLOYEE           TO PAPCT-EMPLOYEE.
195200     MOVE PCT-FLD-NBR (I1)       TO PAPCT-FLD-NBR.
195300     MOVE PCT-NEW-VALUE (I1)     TO PAPCT-NEW-VALUE.
195400     MOVE 1                      TO PAPCT-FIELD-FN.
           MOVE PCT-EFFECT-DATE        TO PAPCT-EFFECT-DATE.
           MOVE PCT-ACTION-CODE        TO PAPCT-ACTION-CODE.
           MOVE PCT-ACTION-NBR         TO PAPCT-ACTION-NBR.
           MOVE PCT-REASON (1)         TO PAPCT-REASON1.
           MOVE PCT-REASON (2)         TO PAPCT-REASON2.
101470*    MOVE PCT-USER-ID            TO PAPCT-USER-ID.
101470     MOVE CRT-USER-NAME          TO PAPCT-USER-ID.
           MOVE PCT-ANT-END-DATE       TO PAPCT-ANT-END-DATE.
           INITIALIZE                     PAPCT-EDIT-DATA-ONLY.

195500     IF  (PCT-POS-LEVEL > 01)
195600         PERFORM 5050-PAPCT-MOVE-TO-PAPEP
195700     ELSE
195800         PERFORM 5000-PAPCT-MOVE-TO-HREMP.
195900
           IF (ERROR-FOUND)
               MOVE CRT-ERROR-NBR        TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               ADD 1                     TO HREMP-I2
               MOVE CRT-MESSAGE          TO HREMP-MESSAGE (HREMP-I2)
               MOVE ZEROS                TO CRT-ERROR-NBR
               SET HREMP-ERROR-FOUND TO TRUE
               GO TO 2410-END.

P58444     IF (NO-PEP-FLD-CHG)
P58444         PERFORM
P58444            VARYING I2 FROM 1 BY 1
P60778            UNTIL  (I2 > 37)
P58444            IF  (PAPCT-FLD-NBR = PA100WS-PAT-FLD-NBR (I2))
P58444                 SET PEP-FLD-CHG   TO TRUE
P58444            END-IF
P58444         END-PERFORM
P58444     END-IF.
P58444
198000 2410-END.
198100
194100*****************************************************************
194200 2415-MOVE-FOR-GRP-UPD           SECTION.
194300*****************************************************************
194400 2415-START.
194500
194600     IF (PCT-NEW-VALUE (I1) = SPACES)
194700         GO TO 2415-END.
194800
           IF   (PCT-POS-LEVEL    = 1)
           AND ((PCT-FLD-NBR (I1) = HREMP-STATE-DN)
           OR   (PCT-FLD-NBR (I1) = HRPEM-SUPP-STATE-DN))
               IF (EMPLOYEE-NOTFOUND)
               OR (EMP-COMPANY  NOT = DB-COMPANY)
               OR (EMP-EMPLOYEE NOT = DB-EMPLOYEE)
                   MOVE PCT-COMPANY      TO DB-COMPANY
                   MOVE PCT-EMPLOYEE     TO DB-EMPLOYEE
                   PERFORM 840-FIND-EMPSET1
               END-IF
               IF  (EMPLOYEE-FOUND)
               AND (EMP-WORK-COUNTRY = "US")
                   IF (PRS-COMPANY       NOT = PCT-COMPANY)
                   OR (PRS-PROCESS-LEVEL NOT = HREMP-PROCESS-LEVEL)
                       MOVE PCT-COMPANY         TO DB-COMPANY
                       MOVE HREMP-PROCESS-LEVEL TO DB-PROCESS-LEVEL
                       PERFORM 840-FIND-PRSSET1
                   END-IF
                   IF  (PCT-FLD-NBR (I1) = HREMP-STATE-DN)
                   AND (PRS-EMP-TAX-ADDR = 2)
                       MOVE PCT-COMPANY    TO WS-WRK-COMPANY
                       MOVE PCT-EMPLOYEE   TO WS-WRK-EMPLOYEE
                       MOVE PCT-EFFECT-DATE
                                           TO WS-WRK-EFFECT-DATE
                       MOVE PCT-ACTION-CODE
                                           TO WS-WRK-ACTION-CODE
                       MOVE PCT-ACTION-NBR TO WS-WRK-ACTION-NBR
                       MOVE WS-OBJ-ID      TO WS-WRK-ACT-OBJ-ID
                       MOVE PCT-UPDATE-BENEFIT
                                           TO WS-WRK-UPDATE-BENEFIT
                       MOVE HREMP-TAX-STATE-DN
                                           TO WS-WRK-FLD-NBR
                       MOVE PCT-NEW-VALUE (I1)
                                           TO WS-WRK-NEW-VALUE
J63431                 MOVE PCT-PROCESS-TYPE   
                                           TO WS-WRK-PROCESS-TYPE-SW                                           
                       PERFORM 5400-ADD-GRP-FLD
                   END-IF
                   IF  (PCT-FLD-NBR (I1) = HRPEM-SUPP-STATE-DN)
                   AND (PRS-EMP-TAX-ADDR = 1)
                       MOVE PCT-COMPANY    TO WS-WRK-COMPANY
                       MOVE PCT-EMPLOYEE   TO WS-WRK-EMPLOYEE
                       MOVE PCT-EFFECT-DATE
                                           TO WS-WRK-EFFECT-DATE
                       MOVE PCT-ACTION-CODE
                                           TO WS-WRK-ACTION-CODE
                       MOVE PCT-ACTION-NBR TO WS-WRK-ACTION-NBR
                       MOVE WS-OBJ-ID      TO WS-WRK-ACT-OBJ-ID
                       MOVE PCT-UPDATE-BENEFIT
                                           TO WS-WRK-UPDATE-BENEFIT
                       MOVE HREMP-TAX-STATE-DN
                                           TO WS-WRK-FLD-NBR
                       MOVE PCT-NEW-VALUE (I1)
                                           TO WS-WRK-NEW-VALUE
J63431                 MOVE PCT-PROCESS-TYPE   
                                           TO WS-WRK-PROCESS-TYPE-SW                                           
                       PERFORM 5400-ADD-GRP-FLD
                   END-IF.

           IF  (PCT-POS-LEVEL    = 1)
           AND (PCT-FLD-NBR (I1) = HRPEM-LOCAT-CODE-DN)
               MOVE "LO"               TO DB-TYPE
               MOVE PCT-NEW-VALUE (I1) TO DB-CODE
               PERFORM 840-FIND-PDDSET1
               IF (PCODESDTL-FOUND)
                   MOVE PCT-COMPANY    TO WS-WRK-COMPANY
                   MOVE PCT-EMPLOYEE   TO WS-WRK-EMPLOYEE
                   MOVE PCT-EFFECT-DATE
                                       TO WS-WRK-EFFECT-DATE
                   MOVE PCT-ACTION-CODE
                                       TO WS-WRK-ACTION-CODE
                   MOVE PCT-ACTION-NBR TO WS-WRK-ACTION-NBR
                   MOVE WS-OBJ-ID      TO WS-WRK-ACT-OBJ-ID
                   MOVE PCT-UPDATE-BENEFIT
                                       TO WS-WRK-UPDATE-BENEFIT
                   MOVE HREMP-WORK-STATE-DN
                                       TO WS-WRK-FLD-NBR
                   MOVE PDD-STATE      TO WS-WRK-NEW-VALUE
J63431             MOVE PCT-PROCESS-TYPE   
                                       TO WS-WRK-PROCESS-TYPE-SW                   
                   PERFORM 5400-ADD-GRP-FLD
               END-IF.

           IF  (PCT-POS-LEVEL = 1)
               MOVE PCT-COMPANY        TO WS-WRK-COMPANY
               MOVE PCT-EMPLOYEE       TO WS-WRK-EMPLOYEE
               MOVE PCT-EFFECT-DATE    TO WS-WRK-EFFECT-DATE
               MOVE PCT-ACTION-CODE    TO WS-WRK-ACTION-CODE
               MOVE PCT-ACTION-NBR     TO WS-WRK-ACTION-NBR
               MOVE WS-OBJ-ID          TO WS-WRK-ACT-OBJ-ID
               MOVE PCT-UPDATE-BENEFIT TO WS-WRK-UPDATE-BENEFIT
P84788         MOVE PCT-UPD-ABS-MGMT   TO WS-WRK-UPD-ABS-MGMT
               MOVE PCT-FLD-NBR (I1)   TO WS-WRK-FLD-NBR
               MOVE PCT-NEW-VALUE (I1) TO WS-WRK-NEW-VALUE
J63431         MOVE PCT-PROCESS-TYPE   TO WS-WRK-PROCESS-TYPE-SW
               PERFORM 5400-ADD-GRP-FLD
               GO TO 2415-END.

198000 2415-END.
198100
194100*****************************************************************
194200 2416-LEV297-GRP-UPD             SECTION.
194300*****************************************************************
194400 2416-START.
194500
           IF  (PCT-FLD-NBR (I1) = HRPEP-END-DATE-DN)
           AND (PCT-NEW-VALUE (I1) NOT = SPACES)
               SET END-DATE-CHG        TO TRUE.

           IF (PCT-FLD-NBR (I1) = HRPEM-ASSIGN-DATE-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-ASSIGN-DATE-DN   TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-POSITION-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-POSITION-DN      TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-POSITION-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-POSITION-DN      TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-JOB-CODE-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-JOB-CODE-DN      TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-PROCESS-LEVEL-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-PROCESS-LEVEL-DN TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-DEPARTMENT-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-DEPARTMENT-DN    TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-USER-LEVEL-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-USER-LEVEL-DN    TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HRPEM-LOCAT-CODE-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-LOCAT-CODE-DN    TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-UNION-CODE-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-UNION-CODE-DN    TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HRPEM-BARGAIN-UNIT-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-BARGAIN-UNIT-DN  TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-WORK-SCHED-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-WORK-SCHED-DN    TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-SUPERVISOR-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-SUPERVISOR-DN    TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-SUPERVISOR-IND-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-SUPERVISOR-IND-DN
                                               TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-NBR-FTE-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-NBR-FTE-DN       TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-ANNUAL-HOURS-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-ANNUAL-HOURS-DN  TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-PAY-RATE-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-PAY-RATE-DN      TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-BASE-PAY-RATE-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-PBASE-PAY-RATE-DN
                                               TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-PRO-RATE-A-SAL-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-PRO-RATE-A-SAL-DN
                                               TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-CURRENCY-CODE-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-PCURRENCY-CODE-DN TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-BASE-CURRENCY-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-PBASE-CURRENCY-DN
                                               TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-SCHEDULE-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-SCHEDULE-DN      TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-PAY-GRADE-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-PAY-GRADE-DN     TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-PAY-STEP-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-PAY-STEP-DN      TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-SHIFT-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-SHIFT-DN         TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-HM-DIST-CO-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-HM-DIST-CO-DN    TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-HM-ACCT-UNIT-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-HM-ACCT-UNIT-DN  TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-HM-ACCOUNT-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-HM-ACCOUNT-DN    TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-HM-SUB-ACCT-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-HM-SUB-ACCT-DN   TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HRPEM-USER-AMOUNT-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-USER-AMOUNT-DN   TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-ACTIVITY-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-ACTIVITY-DN      TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

           IF (PCT-FLD-NBR (I1) = HREMP-ACCT-CATEGORY-DN)
               IF (END-DATE-CHG)
               OR (PCT-NEW-VALUE (I1) NOT = SPACES)
                   MOVE HRPEP-ACCT-CATEGORY-DN TO WS-WRK-FLD-NBR
                   PERFORM 2418-ADD-TO-WORK-FILE
               END-IF.

       2416-END.
198100
P80185*****************************************************************
P80185 2417-HREMP-GRP-UPD              SECTION.
P80185*****************************************************************
P80185 2417-START.
P80185
P80185     IF (HREMP-FLD-CHG (HREMP-FTE-TOTAL-DN))
P80185         MOVE PCT-COMPANY                TO WS-WRK-COMPANY
P80185         MOVE PCT-EMPLOYEE               TO WS-WRK-EMPLOYEE
P80185         MOVE PCT-EFFECT-DATE            TO WS-WRK-EFFECT-DATE
P80185         MOVE PCT-ACTION-CODE            TO WS-WRK-ACTION-CODE
P80185         MOVE PCT-ACTION-NBR             TO WS-WRK-ACTION-NBR
P80185         MOVE WS-OBJ-ID                  TO WS-WRK-ACT-OBJ-ID
P80185         MOVE PCT-UPDATE-BENEFIT         TO WS-WRK-UPDATE-BENEFIT
P80185         MOVE HREMP-FTE-TOTAL-DN         TO WS-WRK-FLD-NBR
P80185
P80185         INITIALIZE HRWS-VALUE
P80185         MOVE HREMP-FTE-TOTAL            TO HRWS-DEC-FIELD
J14716         MOVE 6                          TO HRWS-NBR-DECIMALS
P80185         PERFORM 770-HR-FORMAT-DEC-FIELD
P80185         MOVE HRWS-VALUE                 TO WS-WRK-NEW-VALUE
P80185         PERFORM 5400-ADD-GRP-FLD.
P80185
P80185     IF (HREMP-FLD-CHG (HREMP-PRO-RATE-TOTAL-DN))
P80185         MOVE PCT-COMPANY                TO WS-WRK-COMPANY
P80185         MOVE PCT-EMPLOYEE               TO WS-WRK-EMPLOYEE
P80185         MOVE PCT-EFFECT-DATE            TO WS-WRK-EFFECT-DATE
P80185         MOVE PCT-ACTION-CODE            TO WS-WRK-ACTION-CODE
P80185         MOVE PCT-ACTION-NBR             TO WS-WRK-ACTION-NBR
P80185         MOVE WS-OBJ-ID                  TO WS-WRK-ACT-OBJ-ID
P80185         MOVE PCT-UPDATE-BENEFIT         TO WS-WRK-UPDATE-BENEFIT
P80185         MOVE HREMP-PRO-RATE-TOTAL-DN    TO WS-WRK-FLD-NBR
P80185
P80185         INITIALIZE HRWS-VALUE
P80185         MOVE HREMP-PRO-RATE-TOTAL       TO HRWS-DEC-FIELD
P80185         PERFORM 770-HR-FORMAT-DEC-FIELD
P80185         MOVE HRWS-VALUE                 TO WS-WRK-NEW-VALUE
P80185         PERFORM 5400-ADD-GRP-FLD.
P80185
P83230     IF (HREMP-FLD-CHG (HRPEM-SUPP-ZIP-DN))
P83230         MOVE PCT-COMPANY                TO WS-WRK-COMPANY
P83230         MOVE PCT-EMPLOYEE               TO WS-WRK-EMPLOYEE
P83230         MOVE PCT-EFFECT-DATE            TO WS-WRK-EFFECT-DATE
P83230         MOVE PCT-ACTION-CODE            TO WS-WRK-ACTION-CODE
P83230         MOVE PCT-ACTION-NBR             TO WS-WRK-ACTION-NBR
P83230         MOVE WS-OBJ-ID                  TO WS-WRK-ACT-OBJ-ID
P83230         MOVE PCT-UPDATE-BENEFIT         TO WS-WRK-UPDATE-BENEFIT
P83230         MOVE HRPEM-SUPP-ZIP-DN          TO WS-WRK-FLD-NBR
P83230         MOVE HRPEM-SUPP-ZIP             TO WS-WRK-NEW-VALUE
P83230         PERFORM 5400-ADD-GRP-FLD.
P83230
P83230     IF (HREMP-FLD-CHG (HRPEM-SUPP-CITY-DN))
P83230         MOVE PCT-COMPANY                TO WS-WRK-COMPANY
P83230         MOVE PCT-EMPLOYEE               TO WS-WRK-EMPLOYEE
P83230         MOVE PCT-EFFECT-DATE            TO WS-WRK-EFFECT-DATE
P83230         MOVE PCT-ACTION-CODE            TO WS-WRK-ACTION-CODE
P83230         MOVE PCT-ACTION-NBR             TO WS-WRK-ACTION-NBR
P83230         MOVE WS-OBJ-ID                  TO WS-WRK-ACT-OBJ-ID
P83230         MOVE PCT-UPDATE-BENEFIT         TO WS-WRK-UPDATE-BENEFIT
P83230         MOVE HRPEM-SUPP-CITY-DN         TO WS-WRK-FLD-NBR
P83230         MOVE HRPEM-SUPP-CITY            TO WS-WRK-NEW-VALUE
P83230         PERFORM 5400-ADD-GRP-FLD.
P83230
P83230     IF (HREMP-FLD-CHG (HRPEM-SUPP-STATE-DN))
P83230         MOVE PCT-COMPANY                TO WS-WRK-COMPANY
P83230         MOVE PCT-EMPLOYEE               TO WS-WRK-EMPLOYEE
P83230         MOVE PCT-EFFECT-DATE            TO WS-WRK-EFFECT-DATE
P83230         MOVE PCT-ACTION-CODE            TO WS-WRK-ACTION-CODE
P83230         MOVE PCT-ACTION-NBR             TO WS-WRK-ACTION-NBR
P83230         MOVE WS-OBJ-ID                  TO WS-WRK-ACT-OBJ-ID
P83230         MOVE PCT-UPDATE-BENEFIT         TO WS-WRK-UPDATE-BENEFIT
P83230         MOVE HRPEM-SUPP-STATE-DN        TO WS-WRK-FLD-NBR
P83230         MOVE HRPEM-SUPP-STATE           TO WS-WRK-NEW-VALUE
P83230         PERFORM 5400-ADD-GRP-FLD.
P83230
P83230     IF (HREMP-FLD-CHG (HRPEM-SUPP-CNTRY-CD-DN))
P83230         MOVE PCT-COMPANY                TO WS-WRK-COMPANY
P83230         MOVE PCT-EMPLOYEE               TO WS-WRK-EMPLOYEE
P83230         MOVE PCT-EFFECT-DATE            TO WS-WRK-EFFECT-DATE
P83230         MOVE PCT-ACTION-CODE            TO WS-WRK-ACTION-CODE
P83230         MOVE PCT-ACTION-NBR             TO WS-WRK-ACTION-NBR
P83230         MOVE WS-OBJ-ID                  TO WS-WRK-ACT-OBJ-ID
P83230         MOVE PCT-UPDATE-BENEFIT         TO WS-WRK-UPDATE-BENEFIT
P83230         MOVE HRPEM-SUPP-CNTRY-CD-DN     TO WS-WRK-FLD-NBR
P83230         MOVE HRPEM-SUPP-CNTRY-CD        TO WS-WRK-NEW-VALUE
P83230         PERFORM 5400-ADD-GRP-FLD.
P83230
P83230     IF (HREMP-FLD-CHG (HRPEM-SUPP-COUNTY-DN))
P83230         MOVE PCT-COMPANY                TO WS-WRK-COMPANY
P83230         MOVE PCT-EMPLOYEE               TO WS-WRK-EMPLOYEE
P83230         MOVE PCT-EFFECT-DATE            TO WS-WRK-EFFECT-DATE
P83230         MOVE PCT-ACTION-CODE            TO WS-WRK-ACTION-CODE
P83230         MOVE PCT-ACTION-NBR             TO WS-WRK-ACTION-NBR
P83230         MOVE WS-OBJ-ID                  TO WS-WRK-ACT-OBJ-ID
P83230         MOVE PCT-UPDATE-BENEFIT         TO WS-WRK-UPDATE-BENEFIT
P83230         MOVE HRPEM-SUPP-COUNTY-DN       TO WS-WRK-FLD-NBR
P83230         MOVE HRPEM-SUPP-COUNTY          TO WS-WRK-NEW-VALUE
P83230         PERFORM 5400-ADD-GRP-FLD.
P83230
P85516     IF (HREMP-FLD-CHG (HREMP-TAX-CITY-DN))
P85516         MOVE PCT-COMPANY                TO WS-WRK-COMPANY
P85516         MOVE PCT-EMPLOYEE               TO WS-WRK-EMPLOYEE
P85516         MOVE PCT-EFFECT-DATE            TO WS-WRK-EFFECT-DATE
P85516         MOVE PCT-ACTION-CODE            TO WS-WRK-ACTION-CODE
P85516         MOVE PCT-ACTION-NBR             TO WS-WRK-ACTION-NBR
P85516         MOVE WS-OBJ-ID                  TO WS-WRK-ACT-OBJ-ID
P85516         MOVE PCT-UPDATE-BENEFIT         TO WS-WRK-UPDATE-BENEFIT
P85516         MOVE HREMP-TAX-CITY-DN          TO WS-WRK-FLD-NBR
P85516         MOVE HREMP-TAX-CITY             TO WS-WRK-NEW-VALUE
P85516         PERFORM 5400-ADD-GRP-FLD.
P85516
P85516     IF (HREMP-FLD-CHG (HREMP-TAX-COUNTY-DN))
P85516         MOVE PCT-COMPANY                TO WS-WRK-COMPANY
P85516         MOVE PCT-EMPLOYEE               TO WS-WRK-EMPLOYEE
P85516         MOVE PCT-EFFECT-DATE            TO WS-WRK-EFFECT-DATE
P85516         MOVE PCT-ACTION-CODE            TO WS-WRK-ACTION-CODE
P85516         MOVE PCT-ACTION-NBR             TO WS-WRK-ACTION-NBR
P85516         MOVE WS-OBJ-ID                  TO WS-WRK-ACT-OBJ-ID
P85516         MOVE PCT-UPDATE-BENEFIT         TO WS-WRK-UPDATE-BENEFIT
P85516         MOVE HREMP-TAX-COUNTY-DN        TO WS-WRK-FLD-NBR
P85516         MOVE HREMP-TAX-COUNTY           TO WS-WRK-NEW-VALUE
P85516         PERFORM 5400-ADD-GRP-FLD.
P85516
P85516     IF (HREMP-FLD-CHG (HREMP-TAX-PROVINCE-DN))
P85516         MOVE PCT-COMPANY                TO WS-WRK-COMPANY
P85516         MOVE PCT-EMPLOYEE               TO WS-WRK-EMPLOYEE
P85516         MOVE PCT-EFFECT-DATE            TO WS-WRK-EFFECT-DATE
P85516         MOVE PCT-ACTION-CODE            TO WS-WRK-ACTION-CODE
P85516         MOVE PCT-ACTION-NBR             TO WS-WRK-ACTION-NBR
P85516         MOVE WS-OBJ-ID                  TO WS-WRK-ACT-OBJ-ID
P85516         MOVE PCT-UPDATE-BENEFIT         TO WS-WRK-UPDATE-BENEFIT
P85516         MOVE HREMP-TAX-PROVINCE-DN      TO WS-WRK-FLD-NBR
P85516         MOVE HREMP-TAX-PROVINCE         TO WS-WRK-NEW-VALUE
P85516         PERFORM 5400-ADD-GRP-FLD.
P85516
P85516     IF (HREMP-FLD-CHG (HREMP-TAX-SCHOOL-DN))
P85516         MOVE PCT-COMPANY                TO WS-WRK-COMPANY
P85516         MOVE PCT-EMPLOYEE               TO WS-WRK-EMPLOYEE
P85516         MOVE PCT-EFFECT-DATE            TO WS-WRK-EFFECT-DATE
P85516         MOVE PCT-ACTION-CODE            TO WS-WRK-ACTION-CODE
P85516         MOVE PCT-ACTION-NBR             TO WS-WRK-ACTION-NBR
P85516         MOVE WS-OBJ-ID                  TO WS-WRK-ACT-OBJ-ID
P85516         MOVE PCT-UPDATE-BENEFIT         TO WS-WRK-UPDATE-BENEFIT
P85516         MOVE HREMP-TAX-SCHOOL-DN        TO WS-WRK-FLD-NBR
P85516         MOVE HREMP-TAX-SCHOOL           TO WS-WRK-NEW-VALUE
P85516         PERFORM 5400-ADD-GRP-FLD.
P85516
P85516     IF (HREMP-FLD-CHG (HREMP-TAX-STATE-DN))
P85516         MOVE PCT-COMPANY                TO WS-WRK-COMPANY
P85516         MOVE PCT-EMPLOYEE               TO WS-WRK-EMPLOYEE
P85516         MOVE PCT-EFFECT-DATE            TO WS-WRK-EFFECT-DATE
P85516         MOVE PCT-ACTION-CODE            TO WS-WRK-ACTION-CODE
P85516         MOVE PCT-ACTION-NBR             TO WS-WRK-ACTION-NBR
P85516         MOVE WS-OBJ-ID                  TO WS-WRK-ACT-OBJ-ID
P85516         MOVE PCT-UPDATE-BENEFIT         TO WS-WRK-UPDATE-BENEFIT
P85516         MOVE HREMP-TAX-STATE-DN         TO WS-WRK-FLD-NBR
P85516         MOVE HREMP-TAX-STATE            TO WS-WRK-NEW-VALUE
P85516         PERFORM 5400-ADD-GRP-FLD.
P85516
P80185 2417-END.
P80185
198200*****************************************************************
198300 2418-ADD-TO-WORK-FILE           SECTION.   
198400*****************************************************************
           MOVE PCT-COMPANY            TO WS-WRK-COMPANY. 
           MOVE PCT-EMPLOYEE           TO WS-WRK-EMPLOYEE. 
           MOVE PCT-EFFECT-DATE        TO WS-WRK-EFFECT-DATE. 
           MOVE PCT-ACTION-CODE        TO WS-WRK-ACTION-CODE.
           MOVE PCT-ACTION-NBR         TO WS-WRK-ACTION-NBR.
           MOVE WS-OBJ-ID              TO WS-WRK-ACT-OBJ-ID.
           MOVE PCT-UPDATE-BENEFIT     TO WS-WRK-UPDATE-BENEFIT.
           MOVE PCT-UPD-ABS-MGMT       TO WS-WRK-UPD-ABS-MGMT.
           MOVE PCT-NEW-VALUE (I1)     TO WS-WRK-NEW-VALUE.
           MOVE "PEP"                  TO WS-WRK-DB-FILE.
           PERFORM 5400-ADD-GRP-FLD.

198000 2418-END.

198200*****************************************************************
198300 2420-GET-PARTICIPNT-NUMBER      SECTION.
198400*****************************************************************
198500 2420-START.
198600
198700     IF (WS-PARTICIPNT = ZEROS)
198800         ADD 1                   TO WS-PARTICIPNT
198900     ELSE
199000     IF (PARTICIPNT-KNOTFOUND)
199100     OR (PAR-COMPANY NOT = DB-COMPANY)
               SET WS-NUMBER-FOUND TO TRUE
199300     ELSE
199400     IF (PAR-PARTICIPNT NOT = WS-PARTICIPNT)
               SET WS-NUMBER-FOUND TO TRUE
199600     ELSE
199700         ADD 1                   TO WS-PARTICIPNT
199800         PERFORM 860-KFIND-NEXT-PARSET1.
199900
200000 2420-END.
200100
200200*****************************************************************
200300 2460-CALC-TERM-DATE             SECTION.
200400*****************************************************************
200500 2460-START.
200600
200700     MOVE PCT-EFFECT-DATE        TO WS-TERM-DATE.
200800
200900     MOVE WS-TERM-MNTH           TO WS-TERM-MONTH.
201000
201100     COMPUTE WS-TERM-MONTH       = WS-TERM-MONTH
201200                                 + WS-OCC-MONTHS-EXT.
201300
201400     PERFORM
201500         UNTIL (WS-TERM-MONTH < 13)
201600
201700         SUBTRACT 12             FROM WS-TERM-MONTH
201800         ADD 1                   TO WS-TERM-YEAR
201900     END-PERFORM.
202000
202100     IF (WS-TERM-DAY > WSDT-MONTH-DAYS (WS-TERM-MONTH))
202200         MOVE WSDT-MONTH-DAYS (WS-TERM-MONTH)
202300                                 TO WS-TERM-DAY.
202400
203000     MOVE WS-TERM-MONTH          TO WS-TERM-MNTH.
203100
203200     MOVE WS-TERM-DATE           TO WSDR-FR-DATE.
203300     PERFORM 900-DATE-TO-JULIAN.
203400     SUBTRACT 1                  FROM WSDR-JULIAN-DAYS.
203500     PERFORM 900-JULIAN-TO-DATE.
203600     MOVE WSDR-FR-DATE           TO WS-TERM-DATE.
203700
203800 2460-END.
203900
J00148*****************************************************************
J00148 2645-CHECK-ACTION-DETAIL        SECTION.
J00148*****************************************************************
J00148 2645-START.
J00148
J00148     IF (PCT-FLD-NBR (I1) = ZEROS)
J00148         MOVE 36                     TO I1
J00148         GO TO 2645-END.
J00148
J00148     MOVE PCT-FLD-NBR (I1)           TO DB-FLD-NBR.
J00148     PERFORM 840-FIND-PADSET1.
J00148
J00148     MOVE PCT-FLD-NBR (I1)           TO HREMP-FLD-NBR.
J00148     MOVE "Y"                        TO HREMP-FORMAT-FIELD.
J00148     MOVE SPACES                     TO HREMP-VALUE.
J00148
J00148     IF (PCT-POS-LEVEL > 01)
J00148         IF  (PAEMPPOS-FOUND)
J00148         AND (PEP-COMPANY   = DB-COMPANY)
J00148         AND (PEP-EMPLOYEE  = DB-EMPLOYEE)
J00148         AND (PEP-POS-LEVEL = DB-POS-LEVEL)
J00148             PERFORM 5000-PAPEP-RETRIEVE-VALUE
J00148             MOVE HREMP-VALUE        TO WS-PCT-PRE-VALUE
J00148         ELSE
J00148             INITIALIZE                 WS-PCT-PRE-VALUE
J00148                                        HREMP-VALUE
J00148         END-IF
J00148     ELSE
J00148         PERFORM 5000-HREMP-RETRIEVE-VALUE
J00148         MOVE HREMP-VALUE            TO WS-PCT-PRE-VALUE
J00148     END-IF.
J00148
J00148     IF  (PADICT-FOUND)
J00148         MOVE PAD-DATA-TYPE          TO PAPCTIO-DATA-TYPE
J00148         MOVE "N"                    TO PAPCTIO-DATA-CURR
J00148         MOVE PAD-DECIMALS           TO PAPCTIO-DATA-DECIMALS
J00148         MOVE PCT-NEW-VALUE (I1)     TO PAPCTIO-NEW-VALUE
J00148         PERFORM 9200-FORMAT-PAPCT-TO-DISPLAY
J00148         MOVE PAPCTIO-NEW-VALUE      TO WS-PCT-NEW-VALUE
J00148     ELSE
J00148         MOVE PCT-NEW-VALUE (I1)     TO WS-PCT-NEW-VALUE
J00148     END-IF.
J00148
J00148     IF ((WS-PCT-PRE-VALUE           = WS-PCT-NEW-VALUE)
J00148     AND (WS-PCT-NEW-VALUE       NOT = SPACES))
J64902*J60556  AND (PA100WS-NO-DEFAULT)
J64902         IF (PA100WS-DEF-LOADED)
J64902             IF (PA100WS-NO-DEFAULT)
J64902                 SET SAME-VALUE      TO TRUE
J64902             END-IF
J64902         ELSE
J00148             SET SAME-VALUE          TO TRUE
J64902         END-IF 
J00148     END-IF.
J00148
J01796     IF  (PCT-FLD-NBR (I1)           = HREMP-PAY-RATE-DN)
J01796         MOVE EMP-PAY-RATE           TO PAPCTIO-NEW-VALUE
J01796         PERFORM 9200-FORMAT-PAPCT-TO-DISPLAY
J01796         MOVE PAPCTIO-NEW-VALUE      TO WS-PCT-NEW-VALUE
J01796         IF  (WS-PCT-PRE-VALUE       = WS-PCT-NEW-VALUE)
J01796         AND (WS-PCT-NEW-VALUE   NOT = SPACES)
J01796             SET SAME-VALUE          TO TRUE
J01796         ELSE
J01796             SET DIFF-VALUE          TO TRUE
J01796         END-IF
J01796     END-IF.
J01796
J00148     IF (SAME-VALUE)
J00148         SET ERRORS-PRINTED TO TRUE
J00148         MOVE HREMP-EMPLOYEE         TO REG6-EMPLOYEE
J00148         MOVE HRWS-FORMAT-NAME       TO REG6-EMP-NAME
J00148         MOVE 342                    TO CRT-ERROR-NBR
J00148         MOVE PAD-ITEM-NAME          TO CRT-ERR-VAR1
J00148         MOVE CRT-ERROR-NBR          TO CRT-MSG-NBR
J00148         PERFORM 790-GET-MSG
J00148         INITIALIZE CRT-ERROR-NBR 
J00148         MOVE CRT-MESSAGE            TO REG6-ERROR-DESC
J00148         MOVE PCT-ACTION-CODE        TO REG6-PCT-ACTION-CODE
J00148         MOVE PCT-ACTION-NBR         TO REG6-PCT-ACTION-NBR
J00148         MOVE PCT-POS-LEVEL          TO REG6-PCT-POS-LEVEL
J00148         MOVE PCT-ACTION-TYPE        TO REG6-ACTION-TYPE
J00148         MOVE PCT-EFFECT-DATE        TO REG6-PCT-EFFECT-DATE
J00148         MOVE ERROR-LINE             TO RPT-GROUP-REQUEST
J00148         PERFORM 700-PRINT-RPT-GRP
J00148     END-IF.
J00148
J00148 2645-END.   
209500*****************************************************************
209600 2650-PRINT-ACTION-DETAIL        SECTION.
209700*****************************************************************
209800 2650-START.
209900
210000     IF (PCT-FLD-NBR (I1) = ZEROS)
210100         MOVE 36                     TO I1
210200         GO TO 2650-END.
210300
210400     MOVE PCT-FLD-NBR (I1)           TO DB-FLD-NBR.
210500     PERFORM 840-FIND-PADSET1.
210600
211000     INITIALIZE                     R1G4-FIELD-DESC.
211100     IF  (PADICT-FOUND)
               MOVE PAD-ITEM-NAME          TO CRT-PHRASE
               MOVE WS-PHRASE-SIZE         TO CRT-PHRASE-SIZE
               MOVE "N"                    TO CRT-PUT-DOTS
               PERFORM 900-GET-PHRASE-XLT
               MOVE CRT-PHRASE-XLT         TO R1G4-FIELD-DESC.
211300
211400     MOVE PCT-FLD-NBR (I1)           TO HREMP-FLD-NBR.
211500     MOVE "Y"                        TO HREMP-FORMAT-FIELD.
211600     MOVE SPACES                     TO HREMP-VALUE.
211700
211800     IF (PCT-POS-LEVEL > 01)
211900         IF  (PAEMPPOS-FOUND)
212000         AND (PEP-COMPANY   = DB-COMPANY)
212100         AND (PEP-EMPLOYEE  = DB-EMPLOYEE)
212200         AND (PEP-POS-LEVEL = DB-POS-LEVEL)
212300             PERFORM 5000-PAPEP-RETRIEVE-VALUE
212400             MOVE HREMP-VALUE        TO R1G4-PCT-PRE-VALUE
212500         ELSE
212600             INITIALIZE                 R1G4-PCT-PRE-VALUE
212700                                        HREMP-VALUE
212800         END-IF
212900     ELSE
213000         PERFORM 5000-HREMP-RETRIEVE-VALUE
213100         MOVE HREMP-VALUE            TO R1G4-PCT-PRE-VALUE.
213200
INTL       IF  (PADICT-FOUND)
INTL           MOVE PAD-DATA-TYPE          TO PAPCTIO-DATA-TYPE
               MOVE "N"                    TO PAPCTIO-DATA-CURR
INTL           MOVE PAD-DECIMALS           TO PAPCTIO-DATA-DECIMALS
INTL           MOVE PCT-NEW-VALUE (I1)     TO PAPCTIO-NEW-VALUE
INTL           PERFORM 9200-FORMAT-PAPCT-TO-DISPLAY
INTL           MOVE PAPCTIO-NEW-VALUE      TO R1G4-PCT-NEW-VALUE
INTL       ELSE
213300         MOVE PCT-NEW-VALUE (I1)     TO R1G4-PCT-NEW-VALUE.
213400
213500     MOVE R1GN4D-PCT-ACTION-DETAIL   TO RPT-GROUP-REQUEST.
213600
213700     PERFORM 700-PRINT-RPT-GRP.
213800
213900 2650-END.
      ******************************************************************
000400 2660-PRINT-REQ-DED              SECTION 50.
000600******************************************************************
000700 2660-START.
000800
           IF (PRRQC-I1                    NOT = ZEROES)
           OR (PRRQC-I2                    NOT = ZEROES)
               MOVE PRRQC-EFFECT-DATE      TO R6G4-PCT-EDM-EFFECT-DT
213500         MOVE R6GN4-REQ-DED-ADD-CHG  TO RPT-GROUP-REQUEST
213700         PERFORM 700-PRINT-RPT-GRP.

           IF (PRRQC-I1                          NOT = ZEROES)
               PERFORM
                   VARYING WS-I1 FROM 1 BY 1
                   UNTIL  (WS-I1 > PRRQC-I1)

                   MOVE PRRQC-ADD-REQ    (WS-I1) TO R6G6-REQ-CODE
                   MOVE PRRQC-ADD-DED    (WS-I1) TO R6G6-DED-CODE
                   MOVE "N"                      TO R6G6-MAINTENANCE
                   MOVE PRRQC-ADD-RES    (WS-I1) TO R6G6-RES-CODE
                   MOVE PRRQC-ADD-MAR-ST (WS-I1) TO R6G6-MARITAL-STATUS
                   MOVE PRRQC-ADD-EXEM   (WS-I1) TO R6G6-EXEMPTIONS

213500             MOVE R6GN6-REQ-DED-DETAIL     TO RPT-GROUP-REQUEST
213700             PERFORM 700-PRINT-RPT-GRP
                   SET REQDEDS-PRINTED TO TRUE

                   IF (PRRQC-ADD-PRT-DT (WS-I1)  = 1)
                       PERFORM 2662-PRINT-ADD-FD-DTL
                       THRU    2662-END
                   END-IF

               END-PERFORM.

           IF (PRRQC-I2                          NOT = ZEROES)
               PERFORM
                   VARYING WS-I1 FROM 1 BY 1
                   UNTIL  (WS-I1 > PRRQC-I2)

                   MOVE PRRQC-CHG-REQ    (WS-I1) TO R6G6-REQ-CODE
                   MOVE PRRQC-CHG-DED    (WS-I1) TO R6G6-DED-CODE
                   MOVE "C"                      TO R6G6-MAINTENANCE
                   MOVE PRRQC-CHG-RES    (WS-I1) TO R6G6-RES-CODE
                   MOVE PRRQC-CHG-MAR-ST (WS-I1) TO R6G6-MARITAL-STATUS
                   MOVE PRRQC-CHG-EXEM   (WS-I1) TO R6G6-EXEMPTIONS

213500             MOVE R6GN6-REQ-DED-DETAIL     TO RPT-GROUP-REQUEST
213700             PERFORM 700-PRINT-RPT-GRP
                   SET REQDEDS-PRINTED TO TRUE

                   IF (PRRQC-CHG-PRT-DT (WS-I1)  = 1)
                       PERFORM 2664-PRINT-CHG-FD-DTL
                       THRU    2664-END
                   END-IF

               END-PERFORM.

           INITIALIZE R6G6-MAINTENANCE
                      R6G6-RES-CODE
                      R6G6-MARITAL-STATUS
                      R6G6-EXEMPTIONS.

           IF (PRRQC-I3                          NOT = ZEROES)
               MOVE PRRQC-END-DATE              TO R6G5-PCT-EDM-END-DATE
213500         MOVE R6GN5-REQ-DED-END            TO RPT-GROUP-REQUEST
213700         PERFORM 700-PRINT-RPT-GRP

               PERFORM
                   VARYING WS-I1 FROM 1 BY 1
                   UNTIL  (WS-I1 > PRRQC-I3)

                   MOVE PRRQC-END-REQ (WS-I1)    TO R6G6-REQ-CODE
                   MOVE PRRQC-END-DED (WS-I1)    TO R6G6-DED-CODE

213500             MOVE R6GN6-REQ-DED-DETAIL     TO RPT-GROUP-REQUEST
213700             PERFORM 700-PRINT-RPT-GRP
                   SET REQDEDS-PRINTED TO TRUE
               END-PERFORM.

           GO TO 2660-END.

      ******************************************************************
       2662-PRINT-ADD-FD-DTL.
      ******************************************************************

213500     MOVE R6GN7-REQ-DED-FD-HEAD      TO RPT-GROUP-REQUEST.
213700     PERFORM 700-PRINT-RPT-GRP.
           SET REQDEDS-PRINTED TO TRUE.

           MOVE PRRQC-ADD-EA (WS-I1)       TO R6G8-EXEMPT-AMOUNT.
           MOVE PRRQC-ADD-PE (WS-I1)       TO R6G8-PERS-EXEMPTS.
           MOVE PRRQC-ADD-DE (WS-I1)       TO R6G8-DEPEND-EXEMPTS.
           MOVE PRRQC-ADD-AE (WS-I1)       TO R6G8-ADDL-EXEMPTS.
           MOVE PRRQC-ADD-AEA (WS-I1)      TO R6G8-ADDL-EXEMP-AMT.
           MOVE PRRQC-ADD-ATC (WS-I1)      TO R6G8-ADDL-TAX-CODE.
           MOVE PRRQC-ADD-AR (WS-I1)       TO R6G8-ADDL-RATE.
           MOVE PRRQC-ADD-AA (WS-I1)       TO R6G8-ADDL-AMOUNT.
           MOVE PRRQC-ADD-CC (WS-I1)       TO R6G8-CERT-CODE.
           MOVE PRRQC-ADD-FN (WS-I1)       TO R6G8-FORMULA-NUMBER.

213500     MOVE R6GN8-REQ-DED-FD-DET       TO RPT-GROUP-REQUEST.
213700     PERFORM 700-PRINT-RPT-GRP.
           SET REQDEDS-PRINTED TO TRUE.

       2662-END.

      ******************************************************************
       2664-PRINT-CHG-FD-DTL.
      ******************************************************************

213500     MOVE R6GN7-REQ-DED-FD-HEAD      TO RPT-GROUP-REQUEST.
213700     PERFORM 700-PRINT-RPT-GRP.
           SET REQDEDS-PRINTED TO TRUE.

           MOVE PRRQC-CHG-EA (WS-I1)       TO R6G8-EXEMPT-AMOUNT.
           MOVE PRRQC-CHG-PE (WS-I1)       TO R6G8-PERS-EXEMPTS.
           MOVE PRRQC-CHG-DE (WS-I1)       TO R6G8-DEPEND-EXEMPTS.
           MOVE PRRQC-CHG-AE (WS-I1)       TO R6G8-ADDL-EXEMPTS.
           MOVE PRRQC-CHG-AEA (WS-I1)      TO R6G8-ADDL-EXEMP-AMT.
           MOVE PRRQC-CHG-ATC (WS-I1)      TO R6G8-ADDL-TAX-CODE.
           MOVE PRRQC-CHG-AR (WS-I1)       TO R6G8-ADDL-RATE.
           MOVE PRRQC-CHG-AA (WS-I1)       TO R6G8-ADDL-AMOUNT.
           MOVE PRRQC-CHG-CC (WS-I1)       TO R6G8-CERT-CODE.
           MOVE PRRQC-CHG-FN (WS-I1)       TO R6G8-FORMULA-NUMBER.

213500     MOVE R6GN8-REQ-DED-FD-DET       TO RPT-GROUP-REQUEST.
213700     PERFORM 700-PRINT-RPT-GRP.
           SET REQDEDS-PRINTED TO TRUE.

       2664-END.

213900 2660-END.

      ******************************************************************
       2670-PRINT-PXL                  SECTION 50.
      ******************************************************************

           IF  (PXL-NO-UPDATES)
               GO TO 2670-END.

           INITIALIZE WS-LAST-LOCATION.
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > PRPXL-TAX-CNT)
               IF  (NOT PXL-NOUPDATE (I1))
                   PERFORM 2673-CLEAR-LINE
                   THRU    2673-END
                   PERFORM 2674-TAX-INFO
                   THRU    2674-END
                   PERFORM
                       VARYING I2 FROM 1 BY 1
                       UNTIL  (I2 > PRPXL-EDM-CNT)
                       OR     (PRPXL-EDM-DED-CODE (I2) =
                                               PRPXL-TAX-DEDUCTION (I1))
                       CONTINUE
                   END-PERFORM
                   IF  (I2 <= PRPXL-EDM-CNT)
                       PERFORM 2675-EDM-INFO
                       THRU    2675-END
                   END-IF
                   MOVE R7GN3-PXL-DETAIL       TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
               END-IF
           END-PERFORM.

           PERFORM
               VARYING I2 FROM 1 BY 1
               UNTIL  (I2 > PRPXL-EDM-CNT)
               IF  (NOT EDM-NOUPDATE (I2))
               AND (NOT EDM-PRINTED (I2))
                   PERFORM 2673-CLEAR-LINE
                   THRU    2673-END
                   PERFORM 2675-EDM-INFO
                   THRU    2675-END
                   MOVE R7GN3-PXL-DETAIL       TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
               END-IF
           END-PERFORM.

           GO TO 2670-END.

      *****************************************************************
       2673-CLEAR-LINE.

           INITIALIZE     R7G3-HOME-LOCATION
                          R7G3-TR-LOCATION
                          R7G3-SOURCE
                          R7G3-DED-CODE
                          R7G3-LOC-MAINT
                          R7G3-DED-MAINT
                          R7G3-EFFECT-DATE
                          R7G3-END-DATE
                          R7G3-RES-CODE.

       2673-END.

      *****************************************************************
       2674-TAX-INFO.

           IF  (PRPXL-TAX-LOCATION (I1) NOT = WS-LAST-LOCATION)
               IF  (PRIMARY-LOCATION (I1))
                   MOVE PRPXL-TAX-LOCATION (I1)    TO R7G3-HOME-LOCATION
               ELSE
                   MOVE PRPXL-TAX-LOCATION (I1)    TO R7G3-TR-LOCATION
               END-IF
           END-IF.
           MOVE PRPXL-TAX-LOCATION (I1)    TO WS-LAST-LOCATION.
           IF  (PXL-SYSTEM-REC (I1))
               MOVE "S"                            TO R7G3-SOURCE.
           IF  (PXL-USER-REC (I1))
               MOVE "U"                            TO R7G3-SOURCE.
           IF  (PXL-DISABLED (I1))
               MOVE "D"                            TO R7G3-SOURCE.
           IF  (PXL-M-ADD (I1))
               MOVE "A"                            TO R7G3-LOC-MAINT.
           IF  (PXL-M-DELETE (I1))
               MOVE "D"                            TO R7G3-LOC-MAINT.
           IF  (PXL-M-INACT (I1))
               MOVE "I"                            TO R7G3-LOC-MAINT.
           IF  (PXL-M-REACT (I1))
               MOVE "R"                            TO R7G3-LOC-MAINT.
           IF  (PXL-M-CHANGE (I1))
               MOVE "C"                            TO R7G3-LOC-MAINT.
           MOVE PRPXL-TAX-DEDUCTION (I1)           TO R7G3-DED-CODE.

       2674-END.

      *****************************************************************
       2675-EDM-INFO.

           MOVE PRPXL-EDM-DED-CODE (I2)    TO R7G3-DED-CODE.
           IF  (EDM-M-ADD (I2))
               MOVE "A"                    TO R7G3-DED-MAINT.
           IF  (EDM-M-END (I2))
               MOVE "E"                    TO R7G3-DED-MAINT.
           IF  (EDM-M-REACT (I2))
               MOVE "R"                    TO R7G3-DED-MAINT.
           IF  (EDM-M-CHANGE (I2))
               MOVE "C"                    TO R7G3-DED-MAINT.
           IF  (EDM-M-ADD (I2))
           OR  (EDM-M-REACT (I2))
               MOVE PRPXL-EFFECT-DATE      TO R7G3-EFFECT-DATE.
           IF  (EDM-M-END (I2))
               MOVE PRPXL-END-DATE         TO R7G3-END-DATE.
           MOVE PRPXL-EDM-RES-CODE (I2)    TO R7G3-RES-CODE.
           SET EDM-PRINTED (I2) TO TRUE.

       2675-END.

      ******************************************************************
       2670-END.

      ******************************************************************
000400 2700-LOAD-DEFAULTS              SECTION 50.
000600******************************************************************
000700 2700-START.
000800
           SET PA100WS-USE-COMPANY         TO TRUE.
101470     MOVE SPACES                     TO PA100WS-CHG-REASON.
           IF (PCT-POSITION NOT = SPACES)
               MOVE PCT-COMPANY            TO DB-COMPANY
               MOVE PCT-POSITION           TO DB-POSITION
               MOVE PCT-EFFECT-DATE        TO DB-EFFECT-DATE
               PERFORM 850-FIND-NLT-POSSET2
               IF  (PAPOSITION-FOUND)
               AND (POS-COMPANY           = DB-COMPANY)
               AND (POS-POSITION          = DB-POSITION)
               AND (POS-PROCESS-LEVEL NOT = SPACES)
101470             MOVE POS-CHG-REASON     TO PA100WS-CHG-REASON
                   MOVE "PO"               TO DB-TOPIC
                   MOVE SPACES             TO DB-COUNTRY-CD-REQ
                   MOVE POS-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
                   MOVE PASSET2-PROCESS-LEVEL
                                           TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-PASSET2
                   IF (PASCRTY-FOUND)
                       SET PA100WS-USE-PROCESS-LEVEL
                                           TO TRUE
101470             END-IF
101470         END-IF
101470     END-IF.

           MOVE PCT-COMPANY                TO DB-COMPANY.
           MOVE "PO"                       TO DB-TOPIC.
           INITIALIZE                         DB-COUNTRY-CD-REQ.
           IF (PA100WS-USE-PROCESS-LEVEL)
               MOVE POS-PROCESS-LEVEL      TO DB-PROCESS-LEVEL
               MOVE PASSET2-PROCESS-LEVEL  TO WS-DB-BEG-RNG
           ELSE
               MOVE PASSET2-COUNTRY-CD-REQ TO WS-DB-BEG-RNG
           END-IF.
           PERFORM 850-FIND-BEGRNG-PASSET2.
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (PASCRTY-NOTFOUND)
               OR     (PAS-TOPIC NOT = DB-TOPIC)
               OR     (I1 > PA100WS-COUNTER)
                   MOVE PAS-FLD-NBR         TO WS-FIELD-NBR (I1)
                   MOVE PAS-DEFAULT-FLG     TO WS-DEFAULT-FLG (I1)
                   IF  (PAS-DEFAULT-FLG = 2 OR 3)
                   AND (PA100WS-NO-DEFAULT)
                       SET PA100WS-DEFAULT TO TRUE
                   END-IF
                   PERFORM 860-FIND-NXTRNG-PASSET2
           END-PERFORM.

011200     MOVE PCT-COMPANY                  TO DB-COMPANY.
011600     MOVE PCT-POSITION                 TO DB-POSITION.
           MOVE PPE-EFFECT-DATE              TO DB-EFFECT-DATE.
011700     MOVE PPCSET1-EFFECT-DATE          TO WS-DB-BEG-RNG.

           PERFORM 850-FIND-BEGRNG-PPCSET1.
012000
           IF (PA100WS-NO-DEFAULT)
               GO TO 2700-END
           ELSE
               SET PA100WS-NO-DEFAULT TO TRUE.

           MOVE 1 TO I1.
           PERFORM 2750-LOAD-PAPOSCHG-DATA
           THRU    2750-END
               UNTIL (PAPOSCHG-NOTFOUND).

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 30)
               OR     (PA100WS-DEFAULT-FLG (I1) > 1)
                   CONTINUE
           END-PERFORM.

           IF (I1 NOT > 30)
               SET PA100WS-DEFAULT TO TRUE.

           GO TO 2700-END.

018300******************************************************************
018400 2750-LOAD-PAPOSCHG-DATA.
018500******************************************************************
018600
           IF (PPC-UPDATE-TYPE = "2  ")
               GO TO 2750-FIND-NEXT-PAPOSCHG.

           MOVE 1                   TO I2.
           PERFORM
              UNTIL  (I2 > 27)
              OR     (PAPOSEMP-POS-FLD (I2) = PPC-FLD-NBR)
                 ADD 1              TO I2
           END-PERFORM.

           IF  (I2 NOT > 27)
           AND (PAPOSEMP-POS-FLD (I2) = PPC-FLD-NBR)
               MOVE PAPOSEMP-EMP-FLD (I2)   TO PA100WS-FIELD-NBR2 (I1).

           MOVE PPC-FLD-NBR             TO PA100WS-FIELD-NBR (I1).
           INITIALIZE                      PA100WS-DEFAULT-FLG (I1).
           PERFORM
               VARYING I2 FROM 1 BY 1
               UNTIL  (I2 > PA100WS-COUNTER)
               OR     (WS-FIELD-NBR (I2) = PPC-FLD-NBR)
                   CONTINUE
           END-PERFORM.
           IF (I2 > PA100WS-COUNTER)
               GO TO 2750-FIND-NEXT-PAPOSCHG
           ELSE
               IF (WS-DEFAULT-FLG (I2) < 2)
                   GO TO 2750-FIND-NEXT-PAPOSCHG
               ELSE
                   MOVE WS-DEFAULT-FLG (I2) TO PA100WS-DEFAULT-FLG (I1).

           IF (PPC-N-VALUE-PRIOR NOT = ZEROES)
           OR (PPC-N-VALUE       NOT = ZEROES)
               INITIALIZE                  PAPCTIO-NEW-VALUE
               MOVE PPC-N-VALUE         TO PAPCTIO-NEW-NUM
               MOVE PAPCTIO-NEW-VALUE   TO PA100WS-VALUE (I1)
               INITIALIZE                  PAPCTIO-NEW-VALUE
               MOVE PPC-N-VALUE-PRIOR   TO PAPCTIO-NEW-NUM
               MOVE PAPCTIO-NEW-VALUE   TO PA100WS-VALUE-PRIOR (I1).

           IF (PPC-A-VALUE-PRIOR NOT = SPACES)
           OR (PPC-A-VALUE       NOT = SPACES)
               MOVE PPC-A-VALUE             TO PA100WS-VALUE (I1)
               MOVE PPC-A-VALUE-PRIOR       TO PA100WS-VALUE-PRIOR (I1).

           ADD 1 TO I1.

           IF (PPC-FLD-NBR = PAPOS-EXP-COMPANY-DN)
               MOVE PPC-N-VALUE-PRIOR       TO PA100WS-EXP-COMPANY.
           IF (PPC-FLD-NBR = PAPOS-EXP-ACCOUNT-DN)
               MOVE PPC-N-VALUE-PRIOR       TO PA100WS-EXP-ACCOUNT.
           IF (PPC-FLD-NBR = PAPOS-EXP-SUB-ACCT-DN)
               MOVE PPC-N-VALUE-PRIOR       TO PA100WS-EXP-SUB-ACCT.
           IF (PPC-FLD-NBR = PAPOS-PAY-RATE-DN)
               MOVE PPC-N-VALUE-PRIOR       TO PA100WS-PAY-RATE.
           IF (PPC-FLD-NBR = PAPOS-PAY-FREQUENCY-DN)
               MOVE PPC-N-VALUE-PRIOR       TO PA100WS-PAY-FREQUENCY.
           IF (PPC-FLD-NBR = PAPOS-PAY-STEP-DN)
               MOVE PPC-N-VALUE-PRIOR       TO PA100WS-PAY-STEP.
           IF (PPC-FLD-NBR = PAPOS-SHIFT-DN)
               MOVE PPC-N-VALUE-PRIOR       TO PA100WS-SHIFT.
           IF (PPC-FLD-NBR = PAPOS-SEC-LVL-DN)
               MOVE PPC-N-VALUE-PRIOR       TO PA100WS-SEC-LVL.
           IF (PPC-FLD-NBR = PAPOS-ANNUAL-HOURS-DN)
               MOVE PPC-N-VALUE-PRIOR       TO PA100WS-ANNUAL-HOURS.

       2750-FIND-NEXT-PAPOSCHG.
           PERFORM 860-FIND-NXTRNG-PPCSET1.

       2750-END.

024200******************************************************************
024300 2700-END.
024400******************************************************************
024200******************************************************************
024300 2800-PROCESS-CHANGES            SECTION 60.
024400******************************************************************

           IF (PAEMPPOS-NOTFOUND)
           OR (PEP-COMPANY   NOT = PCT-COMPANY)
           OR (PEP-EMPLOYEE  NOT = PCT-EMPLOYEE)
           OR (PEP-POS-LEVEL NOT = PCT-POS-LEVEL)
               MOVE PCT-COMPANY         TO DB-COMPANY
               MOVE PCT-EMPLOYEE        TO DB-EMPLOYEE
               MOVE PCT-POS-LEVEL       TO DB-POS-LEVEL
P58564         MOVE PEPSET3-POS-LEVEL   TO WS-DB-BEG-RNG
P58564         PERFORM 850-FIND-BEGRNG-PEPSET3.

           IF (PCT-EFFECT-DATE <= PPE-EFFECT-DATE)
               SET PA100WS-FROM-PCT         TO TRUE
           ELSE
               SET PA100WS-FROM-EMP         TO TRUE.

           MOVE 1 TO I4.
           PERFORM
               VARYING I2 FROM 1 BY 1
               UNTIL  (I2 > 30)
               OR     (PA100WS-FIELD-NBR (I2) = ZEROES)
                   PERFORM 2825-CHECK-FIELD
                   THRU    2825-END.

           INITIALIZE                      PA100WS-FLDS-NEEDED
                                           PA100WS-FLDS-AVAILABLE.

           IF (PCT-ACTION-TYPE = "E")
               INITIALIZE          PA100WS-FLDS-NEEDED
               PERFORM
                   VARYING I2 FROM 1 BY 1
                   UNTIL  (I2 > 30)
                       IF (PA100WS-MOVE (I2) = "A")
                           ADD 1 TO PA100WS-FLDS-NEEDED
                       END-IF
               END-PERFORM
               INITIALIZE PA100WS-FLDS-AVAILABLE
               PERFORM
                   VARYING I2 FROM 1 BY 1
                   UNTIL  (I2 > 36)
                       IF (PCT-FLD-NBR (I2) = ZEROES)
                           COMPUTE PA100WS-FLDS-AVAILABLE =
                                        (PA100WS-FLDS-AVAILABLE + 1)
                       END-IF
               END-PERFORM
               IF (PA100WS-FLDS-NEEDED > PA100WS-FLDS-AVAILABLE)
                   GO TO 2800-END.

               SET PA100WS-UPDATE TO TRUE.
               PERFORM 2875-MOVE-FIELDS
               THRU    2875-END
                   VARYING I2 FROM 1 BY 1
                   UNTIL  (I2 > 30)
                   OR     (PA100WS-FIELD (I2) = ZEROES).

           GO TO 2800-END.

024200******************************************************************
024300 2825-CHECK-FIELD.
024400******************************************************************

           PERFORM
               VARYING I3 FROM 1 BY 1
               UNTIL  (I3 > 36)
               OR     (PCT-FLD-NBR (I3) = PA100WS-FIELD-NBR2 (I2))
                   CONTINUE
           END-PERFORM.
           IF (PA100WS-DEFAULT-FLG (I2) < 2)
               GO TO 2825-END.

           IF (I3 NOT > 36)
               IF  (PA100WS-DEFAULT-FLG (I2) = 3)
               AND (PA100WS-POS-LVL)
                   MOVE PA100WS-FIELD-NBR2 (I2)
                                           TO PA100WS-FIELD (I4)
                   MOVE PCT-NEW-VALUE (I3) TO PA100WS-FIELD-VALUE (I4)
                   MOVE PA100WS-VALUE (I2) TO PA100WS-NEW-VALUE (I4)
                   MOVE "C"                TO PA100WS-MOVE (I4)
                   ADD 1 TO I4
                   GO TO 2825-END
               ELSE
               IF (PA100WS-DEFAULT-FLG (I2) = 2)
                   IF  (PA100WS-FROM-PCT)
                       MOVE PCT-NEW-VALUE (I3) TO PA100WS-OLD-VALUE
                   ELSE
                       PERFORM 2850-GET-FIELD-VALUE
                       THRU    2850-END
                   END-IF
                   IF (PA100WS-OLD-VALUE = PA100WS-VALUE-PRIOR (I2))
                       MOVE PA100WS-FIELD-NBR2 (I2)
                                           TO PA100WS-FIELD (I4)
                       MOVE PA100WS-OLD-VALUE
                                           TO PA100WS-FIELD-VALUE (I4)
                       MOVE PA100WS-VALUE (I2)
                                           TO PA100WS-NEW-VALUE (I4)
                       MOVE "C"            TO PA100WS-MOVE (I4)
                       ADD 1 TO I4
                       GO TO 2825-END
                   ELSE
                       GO TO 2825-END.

      * SINCE FIELD IS NOT ON ACTION, SHOULD THE VALUE BE ADDED?
           IF (I3 > 36)
               IF (PA100WS-DEFAULT-FLG (I2) = 3)
                   PERFORM 2850-GET-FIELD-VALUE
                   THRU    2850-END
                   MOVE PA100WS-FIELD-NBR2 (I2)
                                           TO PA100WS-FIELD (I4)
                   MOVE PA100WS-OLD-VALUE  TO PA100WS-FIELD-VALUE (I4)
                   MOVE PA100WS-VALUE (I2) TO PA100WS-NEW-VALUE (I4)
                   MOVE "A"                TO PA100WS-MOVE (I4)
                   ADD 1 TO I4
                   GO TO 2825-END
               ELSE
               IF (PA100WS-DEFAULT-FLG (I2) = 2)
                   PERFORM 2850-GET-FIELD-VALUE
                   THRU    2850-END
                   IF (PA100WS-OLD-VALUE = PA100WS-VALUE-PRIOR (I2))
                       MOVE PA100WS-FIELD-NBR2 (I2)
                                           TO PA100WS-FIELD (I4)
                       MOVE PA100WS-OLD-VALUE
                                           TO PA100WS-FIELD-VALUE (I4)
                       MOVE PA100WS-VALUE (I2)
                                           TO PA100WS-NEW-VALUE (I4)
                       MOVE "A"            TO PA100WS-MOVE (I4)
                       ADD 1 TO I4.

       2825-END.

024200******************************************************************
024300 2850-GET-FIELD-VALUE.
024400******************************************************************
024500
           INITIALIZE PAPCTIO-NEW-VALUE.

           IF (PA100WS-FIELD-NBR2 (I2) = HREMP-JOB-CODE-DN)
               MOVE PEP-JOB-CODE           TO PA100WS-OLD-VALUE
           ELSE
           IF (PA100WS-FIELD-NBR2 (I2) = HREMP-PROCESS-LEVEL-DN)
               MOVE PEP-PROCESS-LEVEL      TO PA100WS-OLD-VALUE
           ELSE
           IF (PA100WS-FIELD-NBR2 (I2) = HREMP-DEPARTMENT-DN)
               MOVE PEP-DEPARTMENT         TO PA100WS-OLD-VALUE
           ELSE
           IF (PA100WS-FIELD-NBR2 (I2) = HREMP-USER-LEVEL-DN)
               MOVE PEP-USER-LEVEL         TO PA100WS-OLD-VALUE
           ELSE
           IF  (PA100WS-FIELD-NBR2 (I2) = HREMP-HM-DIST-CO-DN)
               MOVE PEP-EXP-COMPANY        TO PAPCTIO-NEW-NUM
           ELSE
           IF  (PA100WS-FIELD-NBR2 (I2) = HREMP-HM-ACCT-UNIT-DN)
               MOVE PEP-EXP-ACCT-UNIT      TO PA100WS-OLD-VALUE
           ELSE
           IF  (PA100WS-FIELD-NBR2 (I2) = HREMP-HM-ACCOUNT-DN)
               MOVE PEP-EXP-ACCOUNT        TO PAPCTIO-NEW-NUM
           ELSE
           IF  (PA100WS-FIELD-NBR2 (I2) = HREMP-HM-SUB-ACCT-DN)
               MOVE PEP-EXP-SUB-ACCT       TO PAPCTIO-NEW-NUM
           ELSE
           IF (PA100WS-FIELD-NBR2 (I2) = HREMP-PAY-RATE-DN)
               MOVE PEP-PAY-RATE           TO PAPCTIO-NEW-NUM
           ELSE
           IF  (PA100WS-FIELD-NBR2 (I2) = HREMP-SALARY-CLASS-DN)
           AND (PEP-POS-LEVEL          = 1)
               MOVE EMP-SALARY-CLASS       TO PA100WS-OLD-VALUE
           ELSE
           IF  (PA100WS-FIELD-NBR2 (I2) = HREMP-PAY-FREQUENCY-DN)
           AND (PEP-POS-LEVEL          = 1)
               MOVE EMP-PAY-FREQUENCY      TO PAPCTIO-NEW-NUM
           ELSE
           IF (PA100WS-FIELD-NBR2 (I2) = HREMP-SCHEDULE-DN)
               MOVE PEP-SCHEDULE           TO PA100WS-OLD-VALUE
           ELSE
           IF (PA100WS-FIELD-NBR2 (I2) = HREMP-PAY-GRADE-DN)
               MOVE PEP-PAY-GRADE          TO PA100WS-OLD-VALUE
           ELSE
           IF (PA100WS-FIELD-NBR2 (I2) = HREMP-PAY-STEP-DN)
               MOVE PEP-PAY-STEP           TO PAPCTIO-NEW-NUM
           ELSE
           IF (PA100WS-FIELD-NBR2 (I2) = HREMP-SUPERVISOR-DN)
               MOVE PEP-SUPERVISOR         TO PA100WS-OLD-VALUE
           ELSE
           IF (PA100WS-FIELD-NBR2 (I2) = HREMP-SHIFT-DN)
               MOVE PEP-SHIFT              TO PAPCTIO-NEW-NUM
           ELSE
           IF (PA100WS-FIELD-NBR2 (I2) = HRPEM-LOCAT-CODE-DN)
               MOVE PEP-LOCAT-CODE         TO PA100WS-OLD-VALUE
           ELSE
           IF (PA100WS-FIELD-NBR2 (I2) = HREMP-ACTIVITY-DN)
               MOVE PEP-ACTIVITY           TO PA100WS-OLD-VALUE
           ELSE
           IF (PA100WS-FIELD-NBR2 (I2) = HREMP-ACCT-CATEGORY-DN)
               MOVE PEP-ACCT-CATEGORY      TO PA100WS-OLD-VALUE
           ELSE
           IF  (PA100WS-FIELD-NBR2 (I2) = HREMP-OT-PLAN-CODE-DN)
           AND (PEP-POS-LEVEL          = 1)
               MOVE EMP-OT-PLAN-CODE       TO PA100WS-OLD-VALUE
           ELSE
           IF (PA100WS-FIELD-NBR2 (I2) = HREMP-SUPERVISOR-IND-DN)
               MOVE PEP-SUPERVISOR-IND     TO PA100WS-OLD-VALUE
           ELSE
           IF (PA100WS-FIELD-NBR2 (I2) = HREMP-WORK-SCHED-DN)
              MOVE PEP-WORK-SCHED          TO PA100WS-OLD-VALUE
           ELSE
           IF  (PA100WS-FIELD-NBR2 (I2) = HREMP-EXEMPT-EMP-DN)
           AND (PEP-POS-LEVEL          = 1)
               MOVE EMP-EXEMPT-EMP         TO PA100WS-OLD-VALUE
           ELSE
           IF  (PA100WS-FIELD-NBR2 (I2) = HREMP-SEC-LVL-DN)
           AND (PEP-POS-LEVEL          = 1)
               MOVE EMP-SEC-LVL            TO PAPCTIO-NEW-NUM
           ELSE
           IF  (PA100WS-FIELD-NBR2 (I2) = HREMP-SEC-LOCATION-DN)
           AND (PEP-POS-LEVEL          = 1)
               MOVE EMP-SEC-LOCATION       TO PA100WS-OLD-VALUE
           ELSE
           IF (PA100WS-FIELD-NBR2 (I2) = HREMP-ANNUAL-HOURS-DN)
               MOVE PEP-ANNUAL-HOURS       TO PAPCTIO-NEW-NUM
           END-IF.

           IF  (PAPCTIO-NEW-VALUE NOT = SPACES)
               MOVE PAPCTIO-NEW-VALUE      TO PA100WS-OLD-VALUE.

       2850-END.

024200******************************************************************
024300 2875-MOVE-FIELDS.
024400******************************************************************
024500
           IF (PA100WS-MOVE (I2) = "C")
             PERFORM
               VARYING I3 FROM 1 BY 1
               UNTIL  (I3 > 36)
               OR     (PCT-FLD-NBR (I3) = PA100WS-FIELD (I2))
                   CONTINUE
             END-PERFORM
             IF (I3 NOT > 36)
                   MOVE PA100WS-NEW-VALUE (I2) TO PCT-NEW-VALUE (I3)
             END-IF
           ELSE
           IF (PA100WS-MOVE (I2) = "A")
             PERFORM
               VARYING I3 FROM 1 BY 1
               UNTIL  (I3 > 36)
               OR     (PCT-FLD-NBR (I3) = ZEROES)
                   CONTINUE
             END-PERFORM
             IF (I3 NOT > 36)
                   MOVE PA100WS-FIELD (I2)     TO PCT-FLD-NBR (I3)
                   MOVE PA100WS-NEW-VALUE (I2) TO PCT-NEW-VALUE (I3).

       2875-END.

024200******************************************************************
024300 2800-END.
024400******************************************************************
024200******************************************************************
024300 2900-PROCESS-ADDS               SECTION 60.
024400******************************************************************
024500
       2900-START.

151700     IF (PCT-POS-LEVEL > 01)
151800         INITIALIZE                  PAPEP-SCR-FIELDS
151900         MOVE PCT-COMPANY         TO DB-COMPANY
152000         MOVE PCT-EMPLOYEE        TO DB-EMPLOYEE
152100         MOVE PCT-POS-LEVEL       TO DB-POS-LEVEL
P58564         MOVE PEPSET3-POS-LEVEL   TO WS-DB-BEG-RNG
P58564         PERFORM 850-FIND-BEGRNG-PEPSET3
152400         IF  (PAEMPPOS-FOUND)
152800             MOVE PEP-EFFECT-DATE   TO PA100WS-PEP-EFFECT-DATE
                   PERFORM 7000-PAPEP-DEFAULT
152900         ELSE
153000             INITIALIZE                PAPEP-SCR-FIELDS
                                             PA100WS-PEP-EFFECT-DATE
153100         END-IF
153200         MOVE PCT-COMPANY            TO PADT-COMPANY
153300         MOVE PCT-EMPLOYEE           TO PADT-EMPLOYEE
153400         MOVE EMP-TERM-DATE          TO PADT-END-DATE
153500         MOVE PCT-POS-LEVEL          TO PADT-POS-LEVEL
153600         MOVE PA100WS-EFFECT-DATE    TO PADT-EFFECT-DATE
153700         PERFORM 2300-PADT-DATE-CHECK
153800         MOVE ZEROS                  TO CRT-ERROR-NBR
153900         IF (ERROR-FOUND)
154000             INITIALIZE                 PAPEP-UPDPEP-DATE
154100         ELSE
154200             MOVE PADT-UPDPEP-DATE   TO PAPEP-UPDPEP-DATE
154300         END-IF
               IF (PADT-UPDPEP-DATE        NOT = ZEROES)
               OR (PA100WS-PEP-EFFECT-DATE = PA100WS-EFFECT-DATE)
                   MOVE "C"                      TO PAPEP-FC
               ELSE
                   MOVE "A"                      TO PAPEP-FC
           END-IF.
154900
151700     IF (PCT-POS-LEVEL = 01)
               PERFORM 840-FIND-PEMSET1
151800         INITIALIZE                  HREMP-SCR-FIELDS
                                           HRPEM-SCR-FIELDS
               PERFORM 7000-HREMP-DEFAULT
               PERFORM 7000-HRPEM-DEFAULT
153200         MOVE PCT-COMPANY            TO PADT-COMPANY
153300         MOVE PCT-EMPLOYEE           TO PADT-EMPLOYEE
153400         MOVE EMP-TERM-DATE          TO PADT-END-DATE
153500         MOVE PCT-POS-LEVEL          TO PADT-POS-LEVEL
153600         MOVE PA100WS-EFFECT-DATE    TO PADT-EFFECT-DATE
153700         PERFORM 2300-PADT-DATE-CHECK
153800         MOVE ZEROS                  TO CRT-ERROR-NBR
153900         IF (ERROR-FOUND)
154000             INITIALIZE                 HREMP-UPDPEP-DATE
154100         ELSE
154200             MOVE PADT-UPDPEP-DATE   TO HREMP-UPDPEP-DATE
154300         END-IF
154600         MOVE "C"                    TO HREMP-FC
           END-IF.
154900
           IF (PCT-EFFECT-DATE <= PPE-EFFECT-DATE)
               SET PA100WS-FROM-PCT         TO TRUE
           ELSE
               SET PA100WS-FROM-EMP         TO TRUE.

           MOVE 1 TO I3.
           PERFORM
               VARYING I2 FROM 1 BY 1
               UNTIL  (I2 > 30)
               OR     (PA100WS-FIELD-NBR (I2) = ZEROES)
                   IF  (PA100WS-DEFAULT-FLG (I2) = 3)
                   AND (PA100WS-POS-LVL)
                       PERFORM 2910-GET-VALUE
                       THRU    2910-END
                       IF (PA100WS-OLD-VALUE NOT = PA100WS-VALUE (I2))
                           MOVE PA100WS-FIELD-NBR2 (I2)
                                           TO PA100WS-FIELD (I3)
                           MOVE PA100WS-OLD-VALUE
                                           TO PA100WS-FIELD-VALUE (I3)
                           MOVE PA100WS-VALUE (I2)
                                           TO PA100WS-NEW-VALUE (I3)
                           ADD 1 TO I3
                       END-IF
                   END-IF
                   IF  (PA100WS-DEFAULT-FLG (I2) = 2)
                       PERFORM 2910-GET-VALUE
                       THRU    2910-END
                       IF (PA100WS-OLD-VALUE = PA100WS-VALUE-PRIOR (I2))
                           MOVE PA100WS-FIELD-NBR2 (I2)
                                           TO PA100WS-FIELD (I3)
                           MOVE PA100WS-OLD-VALUE
                                           TO PA100WS-FIELD-VALUE (I3)
                           MOVE PA100WS-VALUE (I2)
                                           TO PA100WS-NEW-VALUE (I3)
                           ADD 1 TO I3
                   END-IF
           END-PERFORM.

           SET PA100WS-UPDATE TO TRUE.

           IF (PA100WS-FIELD (1) NOT = ZEROES)
               PERFORM 2920-PROCESS-ACTION.

           GO TO 2900-END.

024200******************************************************************
024300 2910-GET-VALUE.
024400******************************************************************
024500
           INITIALIZE PAPCTIO-NEW-VALUE.

           IF (PA100WS-FROM-PCT)
               PERFORM
                   VARYING I4 FROM 1 BY 1
                   UNTIL  (I4 > 36)
                   OR     (PCT-FLD-NBR (I4) = PA100WS-FIELD-NBR2 (I2))
                       CONTINUE
               END-PERFORM
               IF  (I4                 NOT > 36)
               AND (PCT-NEW-VALUE (I4) NOT = SPACES)
                   MOVE PCT-NEW-VALUE (I4) TO PA100WS-OLD-VALUE
                   GO TO 2910-END
               END-IF
           END-IF.

           IF (PA100WS-FIELD-NBR (I2) = PAPOS-JOB-CODE-DN)
               IF (PCT-POS-LEVEL = 1)
                   MOVE HREMP-JOB-CODE       TO PA100WS-OLD-VALUE
               ELSE
                   MOVE PAPEP-JOB-CODE       TO PA100WS-OLD-VALUE
               END-IF
           ELSE
           IF (PA100WS-FIELD-NBR (I2) = PAPOS-PROCESS-LEVEL-DN)
               IF (PCT-POS-LEVEL = 1)
                   MOVE HREMP-PROCESS-LEVEL  TO PA100WS-OLD-VALUE
               ELSE
                   MOVE PAPEP-PROCESS-LEVEL  TO PA100WS-OLD-VALUE
               END-IF
           ELSE
           IF (PA100WS-FIELD-NBR (I2) = PAPOS-DEPARTMENT-DN)
               IF (PCT-POS-LEVEL = 1)
                   MOVE HREMP-DEPARTMENT     TO PA100WS-OLD-VALUE
               ELSE
                   MOVE PAPEP-DEPARTMENT     TO PA100WS-OLD-VALUE
               END-IF
           ELSE
           IF (PA100WS-FIELD-NBR (I2) = PAPOS-USER-LEVEL-DN)
               IF (PCT-POS-LEVEL = 1)
                   MOVE HREMP-USER-LEVEL     TO PA100WS-OLD-VALUE
               ELSE
                   MOVE PAPEP-USER-LEVEL     TO PA100WS-OLD-VALUE
               END-IF
           ELSE
           IF  (PA100WS-FIELD-NBR (I2) = PAPOS-EXP-COMPANY-DN)
               IF  (PCT-POS-LEVEL = 1)
                   MOVE HREMP-HM-DIST-CO     TO PAPCTIO-NEW-NUM
               ELSE
                   MOVE PAPEP-EXP-DIST-CO    TO PAPCTIO-NEW-NUM
               END-IF
           ELSE
           IF  (PA100WS-FIELD-NBR (I2) = PAPOS-EXP-ACCT-UNIT-DN)
               IF  (PCT-POS-LEVEL = 1)
                   MOVE HREMP-HM-ACCT-UNIT   TO PA100WS-OLD-VALUE
               ELSE
                   MOVE PAPEP-EXP-ACCT-UNIT  TO PA100WS-OLD-VALUE
               END-IF
           ELSE
           IF  (PA100WS-FIELD-NBR (I2) = PAPOS-EXP-ACCOUNT-DN)
               IF  (PCT-POS-LEVEL = 1)
                   MOVE HREMP-HM-ACCOUNT     TO PAPCTIO-NEW-NUM
               ELSE
                   MOVE PAPEP-EXP-ACCOUNT    TO PAPCTIO-NEW-NUM
               END-IF
           ELSE
           IF  (PA100WS-FIELD-NBR (I2) = PAPOS-EXP-SUB-ACCT-DN)
               IF  (PCT-POS-LEVEL = 1)
                   MOVE HREMP-HM-SUB-ACCT    TO PAPCTIO-NEW-NUM
               ELSE
                   MOVE PAPEP-EXP-SUB-ACCT   TO PAPCTIO-NEW-NUM
               END-IF
           ELSE
           IF (PA100WS-FIELD-NBR (I2) = PAPOS-PAY-RATE-DN)
               IF (PCT-POS-LEVEL = 1)
                   MOVE HREMP-PAY-RATE       TO PAPCTIO-NEW-NUM
               ELSE
                   MOVE PAPEP-PAY-RATE       TO PAPCTIO-NEW-NUM
               END-IF
           ELSE
           IF  (PA100WS-FIELD-NBR (I2) = PAPOS-SALARY-CLASS-DN)
           AND (PCT-POS-LEVEL = 1)
               MOVE HREMP-SALARY-CLASS       TO PA100WS-OLD-VALUE
           ELSE
           IF  (PA100WS-FIELD-NBR (I2) = PAPOS-PAY-FREQUENCY-DN)
           AND (PCT-POS-LEVEL = 1)
J84223*        MOVE HREMP-PAY-FREQUENCY      TO PA100WS-OLD-VALUE
J84223         MOVE HREMP-PAY-FREQUENCY      TO PAPCTIO-NEW-NUM
           ELSE
           IF (PA100WS-FIELD-NBR (I2) = PAPOS-SCHEDULE-DN)
               IF (PCT-POS-LEVEL = 1)
                   MOVE HREMP-SCHEDULE       TO PA100WS-OLD-VALUE
               ELSE
                   MOVE PAPEP-SCHEDULE       TO PA100WS-OLD-VALUE
               END-IF
           ELSE
           IF  (PA100WS-FIELD-NBR (I2) = PAPOS-PAY-GRADE-DN)
               IF (PCT-POS-LEVEL = 1)
                   MOVE HREMP-PAY-GRADE      TO PA100WS-OLD-VALUE
               ELSE
                   MOVE PAPEP-PAY-GRADE      TO PA100WS-OLD-VALUE
               END-IF
           ELSE
           IF (PA100WS-FIELD-NBR (I2) = PAPOS-PAY-STEP-DN)
               IF (PCT-POS-LEVEL = 1)
                   MOVE HREMP-PAY-STEP       TO PAPCTIO-NEW-NUM
               ELSE
                   MOVE PAPEP-PAY-STEP       TO PAPCTIO-NEW-NUM
               END-IF
           ELSE
           IF  (PA100WS-FIELD-NBR (I2) = PAPOS-SUPERVISOR-DN)
               IF (PCT-POS-LEVEL = 1)
                   MOVE HREMP-SUPERVISOR     TO PA100WS-OLD-VALUE
               ELSE
                   MOVE PAPEP-SUPERVISOR     TO PA100WS-OLD-VALUE
               END-IF
           ELSE
           IF (PA100WS-FIELD-NBR (I2) = PAPOS-SHIFT-DN)
               IF (PCT-POS-LEVEL = 1)
                   MOVE HREMP-SHIFT          TO PAPCTIO-NEW-NUM
               ELSE
                   MOVE PAPEP-SHIFT          TO PAPCTIO-NEW-NUM
               END-IF
           ELSE
           IF  (PA100WS-FIELD-NBR (I2) = PAPOS-LOCAT-CODE-DN)
               IF (PCT-POS-LEVEL = 1)
                   MOVE HRPEM-LOCAT-CODE     TO PA100WS-OLD-VALUE
               ELSE
                   MOVE PAPEP-LOCAT-CODE     TO PA100WS-OLD-VALUE
               END-IF
           ELSE
           IF  (PA100WS-FIELD-NBR (I2) = PAPOS-ACTIVITY-DN)
               IF  (PCT-POS-LEVEL  = 1)
                   MOVE HREMP-ACTIVITY       TO PA100WS-OLD-VALUE
               ELSE
                   MOVE PAPEP-ACTIVITY       TO PA100WS-OLD-VALUE
               END-IF
           ELSE
           IF  (PA100WS-FIELD-NBR (I2) = PAPOS-ACCT-CATEGORY-DN)
               IF  (PCT-POS-LEVEL  = 1)
                   MOVE HREMP-ACCT-CATEGORY  TO PA100WS-OLD-VALUE
               ELSE
                   MOVE PAPEP-ACCT-CATEGORY  TO PA100WS-OLD-VALUE
               END-IF
           ELSE
           IF  (PA100WS-FIELD-NBR (I2) = PAPOS-OT-PLAN-CODE-DN)
           AND (PCT-POS-LEVEL  = 1)
               MOVE HREMP-OT-PLAN-CODE      TO PA100WS-OLD-VALUE
           ELSE
           IF (PA100WS-FIELD-NBR (I2) = PAPOS-SUPERVISOR-IND-DN)
               IF (PCT-POS-LEVEL = 1)
                   MOVE HREMP-SUPERVISOR-IND TO PA100WS-OLD-VALUE
               ELSE
                   MOVE PAPEP-SUPERVISOR-IND TO PA100WS-OLD-VALUE
               END-IF
           ELSE
           IF  (PA100WS-FIELD-NBR (I2) = PAPOS-WORK-SCHED-DN)
               IF (PCT-POS-LEVEL = 1)
                   MOVE HREMP-WORK-SCHED     TO PA100WS-OLD-VALUE
               ELSE
                   MOVE PAPEP-WORK-SCHED     TO PA100WS-OLD-VALUE
               END-IF
           ELSE
           IF  (PA100WS-FIELD-NBR (I2) = PAPOS-EXEMPT-EMP-DN)
           AND (PCT-POS-LEVEL = 1)
               MOVE HREMP-EXEMPT-EMP         TO PA100WS-OLD-VALUE
           ELSE
           IF  (PA100WS-FIELD-NBR (I2) = PAPOS-SEC-LVL-DN)
           AND (PCT-POS-LEVEL = 1)
               MOVE HREMP-SEC-LVL            TO PAPCTIO-NEW-NUM
           ELSE
           IF  (PA100WS-FIELD-NBR (I2) = PAPOS-SEC-LOCATION-DN)
           AND (PCT-POS-LEVEL = 1)
               MOVE HREMP-SEC-LOCATION       TO PA100WS-OLD-VALUE
           ELSE
           IF  (PA100WS-FIELD-NBR (I2) = PAPOS-ANNUAL-HOURS-DN)
               IF  (PCT-POS-LEVEL = 1)
                   MOVE HREMP-ANNUAL-HOURS   TO PAPCTIO-NEW-NUM
               ELSE
                   MOVE PAPEP-ANNUAL-HOURS   TO PAPCTIO-NEW-NUM
               END-IF
           END-IF.

           IF  (PAPCTIO-NEW-VALUE NOT = SPACES)
               MOVE PAPCTIO-NEW-VALUE      TO PA100WS-OLD-VALUE.

       2910-END.

024200******************************************************************
024300 2900-END.
024400******************************************************************
024200******************************************************************
024300 2920-PROCESS-ACTION             SECTION 75.
024400******************************************************************
024500
           SET ACTION-NOERROR TO TRUE.
134400
155000     INITIALIZE                            PAPEPWS-SUM-FTE
155100                                           HREMPWS-SUM-FTE.
155200     MOVE PCT-COMPANY                  TO DB-COMPANY.
155300     MOVE PCT-EMPLOYEE                 TO DB-EMPLOYEE.
155400     MOVE PEPSET2-EMPLOYEE             TO WS-DB-BEG-RNG.
155500     PERFORM 850-FIND-BEGRNG-PEPSET2.
155600     INITIALIZE                           PAPEPWS-SUM-FTE.
155700     IF (PAEMPPOS-FOUND)
155800         PERFORM 2930-CHECK-ADD
155900             UNTIL (PAEMPPOS-NOTFOUND)
156000             OR    (PEP-COMPANY  NOT = DB-COMPANY)
156100             OR    (PEP-EMPLOYEE NOT = DB-EMPLOYEE).
156200
           MOVE PRM-COMPANY                TO PAACT-COMPANY.
           MOVE "E"                        TO PAACT-ACTION-TYPE.
           MOVE PCT-EMPLOYEE               TO PAACT-EMPLOYEE.
           MOVE WS-ACTION-CODE             TO PAACT-ACTION-CODE.
           MOVE PA100WS-EFFECT-DATE        TO PAACT-EFFECT-DATE.
           MOVE "Y"                        TO PAACT-HISTORY.
           PERFORM 2300-PAACT-ACTION-NBR.
           MOVE PAACT-ACTION-NBR           TO PA100WS-ACTION-NBR.
           MOVE "L"                        TO PAACT-ACTION-TYPE.
           PERFORM 2300-PAACT-ACTION-NBR.
           IF (PAACT-ACTION-NBR > PA100WS-ACTION-NBR)
               MOVE PAACT-ACTION-NBR       TO PA100WS-ACTION-NBR.

           IF  (PERSACTION-NOTFOUND)
           OR ((PCT-COMPANY     NOT = PRM-COMPANY)
           OR  (PCT-EMPLOYEE    NOT = PA100WS-SAVE-EMPLOYEE)
           OR  (PCT-ACTION-TYPE NOT = PA100WS-SAVE-ACTION-TYPE)
           OR  (PCT-ACTION-CODE NOT = PA100WS-SAVE-ACTION-CODE)
           OR  (PCT-ACTION-NBR  NOT = PA100WS-SAVE-ACTION-NBR)
           OR  (PCT-EFFECT-DATE NOT = PA100WS-SAVE-EFFECT-DATE))
               MOVE PRM-COMPANY              TO DB-COMPANY
               MOVE PA100WS-SAVE-ACTION-TYPE TO DB-ACTION-TYPE
               MOVE PA100WS-SAVE-EFFECT-DATE TO DB-EFFECT-DATE
               MOVE PA100WS-SAVE-ACTION-CODE TO DB-ACTION-CODE
               MOVE PA100WS-SAVE-EMPLOYEE    TO DB-EMPLOYEE
               MOVE PA100WS-SAVE-ACTION-NBR  TO DB-ACTION-NBR
               PERFORM 840-FIND-PCTSET3.

156300     IF (PCT-POS-LEVEL > 01)
156500         MOVE PCT-COMPANY            TO PAPEP-COMPANY
156600         MOVE 1                      TO PAPEP-COMPANY-FN
156700         MOVE PCT-EMPLOYEE           TO PAPEP-EMPLOYEE
156800         MOVE 1                      TO PAPEP-EMPLOYEE-FN
156900         MOVE PA100WS-EFFECT-DATE    TO PAPEP-EFFECT-DATE
157000         MOVE PCT-POS-LEVEL          TO PAPEP-POS-LEVEL
157100         MOVE PCT-ACTION-CODE        TO PAPEP-ACTION-CODE
157200         MOVE PA100WS-ACTION-NBR     TO PAPEP-ACTION-NBR
157300         MOVE PCT-REASON (1)         TO PAPEP-REASON1
157400         MOVE PCT-REASON (2)         TO PAPEP-REASON2
101470*        MOVE PCT-USER-ID            TO PAPEP-USER-ID
101470         MOVE CRT-USER-NAME          TO PAPEP-USER-ID
157600         MOVE PCT-ANT-END-DATE       TO PAPEP-ANT-END-DATE
               MOVE "Y"                    TO PAPEP-ACTION
157700         MOVE PCT-UPDATE-BENEFIT     TO HREMP-UPDATE-BENEFIT
               MOVE PCT-UPD-ABS-MGMT       TO HREMP-UPDATE-ABSENCE-MGMT
157800     ELSE
               MOVE PCT-EDM-EFFECT-DT      TO PRPXL-PARM-EFF-DATE
               MOVE PCT-EDM-END-DATE       TO PRPXL-PARM-END-DATE
158200         MOVE "C"                    TO HREMP-FC
158300         MOVE PCT-COMPANY            TO HREMP-COMPANY
158400         MOVE 1                      TO HREMP-COMPANY-FN
158500         MOVE PCT-EMPLOYEE           TO HREMP-EMPLOYEE
158600         MOVE 1                      TO HREMP-EMPLOYEE-FN
158700         MOVE PCT-POS-LEVEL          TO HREMP-POS-LEVEL
158800         MOVE PA100WS-EFFECT-DATE    TO HREMP-EFFECT-DATE
158900         MOVE WS-ACTION-CODE         TO HREMP-ACTION-CODE
159000         MOVE PA100WS-ACTION-NBR     TO HREMP-ACTION-NBR
159100         MOVE PCT-REASON (1)         TO HREMP-REASON1
159200         MOVE PCT-REASON (2)         TO HREMP-REASON2
101470*        MOVE PCT-USER-ID            TO HREMP-USER-ID
101470         MOVE CRT-USER-NAME          TO HREMP-USER-ID
159400         MOVE PCT-ANT-END-DATE       TO HREMP-ANT-END-DATE
159500         MOVE PCT-UPDATE-BENEFIT     TO HREMP-UPDATE-BENEFIT
               MOVE PCT-UPD-ABS-MGMT       TO HREMP-UPDATE-ABSENCE-MGMT
159600         MOVE "Y"                    TO HREMP-ACTION.
159700
159800     PERFORM 2940-PROCESS-PAPCT
159900         VARYING I1 FROM 1 BY 1
160000         UNTIL  (I1 > 30)
               OR (PA100WS-FIELD (I1) = ZEROES).
160100
160200     IF (PCT-POS-LEVEL > 1)
160300         MOVE PCT-COMPANY        TO DB-COMPANY
160400         MOVE EMP-EMP-STATUS     TO DB-EMP-STATUS
160500         PERFORM 840-FIND-EMSSET1
160600         IF (EMSTATUS-FOUND)
160700             MOVE EMS-COUNT      TO PAPEP-COUNT.
160800
           IF  (PCT-POS-LEVEL > 1)
           AND (PCT-BASE-CURRENCY NOT = SPACES)
               MOVE PCT-BASE-CURRENCY  TO PAPEP-BASE-CURRENCY
               MOVE PCT-BASE-ND        TO PAPEP-BASE-ND
               MOVE PCT-BASE-PAY-RATE  TO PAPEP-BASE-PAY-RATE
               MOVE 1                  TO PAPEP-BASE-CURRENCY-FN
                                          PAPEP-BASE-PAY-RATE-FN
               SET PAPEP-BASE-OVERRIDE TO TRUE
           END-IF.

160900     INITIALIZE                     PAPEP-UPDPEP-DATE.
           SET PAPEP-FROM-ACTION TO TRUE.
161100     MOVE PCT-COMPANY            TO PADT-COMPANY.
161200     MOVE PCT-EMPLOYEE           TO PADT-EMPLOYEE.
161300     MOVE EMP-TERM-DATE          TO PADT-END-DATE.
161400     MOVE PCT-POS-LEVEL          TO PADT-POS-LEVEL.
161500     MOVE PA100WS-EFFECT-DATE    TO PADT-EFFECT-DATE.
161600     PERFORM 2300-PADT-DATE-CHECK.
161700
161900     MOVE ZEROS                  TO CRT-ERROR-NBR.
162000     IF (PA100WS-FIELD (1) NOT = ZEROS)
162100         IF (PCT-POS-LEVEL > 01)
162800             MOVE PADT-UPDPEP-DATE   TO PAPEP-UPDPEP-DATE
163000             MOVE PCT-POS-LEVEL      TO PAPEP-POS-LEVEL
163100             COMPUTE PAPEPWS-SUM-FTE =
163200               (PAPEPWS-SUM-FTE + PAPEP-NBR-FTE)
163300             PERFORM 2000-PAPEP-EDIT-TRAN
                   MOVE PAPEP-EMPLOYEE         TO HREMP-EMPLOYEE
                   MOVE EMP-LAST-NAME          TO HRWS-LAST-NAME
                   MOVE EMP-FIRST-NAME         TO HRWS-FIRST-NAME
                   MOVE EMP-MIDDLE-INIT        TO HRWS-MIDDLE-INIT
                   MOVE EMP-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE
                   MOVE EMP-NAME-SUFFIX        TO HRWS-NAME-SUFFIX
                   PERFORM 750-HR-FORMAT-NAME
163400             IF (PAPEP-UPDPEP-DATE = PCT-EFFECT-DATE)
163500                 INITIALIZE             PAPEP-UPDPEP-DATE
163600             END-IF
163700         ELSE
163800             MOVE PADT-UPDPEP-DATE   TO HREMP-UPDPEP-DATE
164000             MOVE 1                  TO HREMP-POS-LEVEL
164100             COMPUTE HREMPWS-SUM-FTE =
164200               (HREMPWS-SUM-FTE + HREMP-NBR-FTE)
164300             PERFORM 2000-HREMP-EDIT-TRAN
                   MOVE HREMP-LAST-NAME        TO HRWS-LAST-NAME
                   MOVE HREMP-FIRST-NAME       TO HRWS-FIRST-NAME
                   MOVE HREMP-MIDDLE-INIT      TO HRWS-MIDDLE-INIT
                   MOVE HREMP-LAST-NAME-PRE    TO HRWS-LAST-NAME-PRE
                   MOVE HREMP-NAME-SUFFIX      TO HRWS-NAME-SUFFIX
                   PERFORM 750-HR-FORMAT-NAME
                   MOVE HREMP-CURRENCY-CODE    TO PAPCT-EMP-CURRENCY
                   MOVE HREMP-CURR-ND          TO PAPCT-EMP-CURR-ND
164400             IF (HREMP-UPDPEP-DATE = PCT-EFFECT-DATE)
164500                 INITIALIZE             HREMP-UPDPEP-DATE.
164600
           ADD 1                       TO HREMP-I2.
           MOVE 155                    TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO HREMP-MESSAGE (HREMP-I2).

           MOVE WS-ACTION-CODE         TO WS-ERR-ACTION-CODE.
           MOVE 0                      TO WS-ERR-ACTION-NBR.
           MOVE PCT-ACTION-TYPE        TO WS-ERR-ACTION-TYPE.
           MOVE PCT-POS-LEVEL          TO WS-ERR-POS-LEVEL.
           MOVE PA100WS-EFFECT-DATE    TO WS-ERR-EFFECT-DATE.
           PERFORM 6000-PRINT-MESSAGES.

           IF  (HREMP-ERROR-FOUND)
               SET ACTION-ERROR TO TRUE
               GO TO 2920-END.

168700     IF (PCT-EMPLOYEE NOT = WS-SV-EMPLOYEE-PRT)
168800         MOVE EMP-EMPLOYEE                 TO WS-SV-EMPLOYEE-PRT
168900         MOVE EMP-EMPLOYEE                 TO R1G2-PCT-EMPLOYEE
168900                                              R6G2-PCT-EMPLOYEE
                                                    R7G2-PCT-EMPLOYEE
169000         MOVE EMP-LAST-NAME                TO HRWS-LAST-NAME
169100         MOVE EMP-FIRST-NAME               TO HRWS-FIRST-NAME
169200         MOVE EMP-MIDDLE-INIT              TO HRWS-MIDDLE-INIT
               MOVE EMP-LAST-NAME-PRE            TO HRWS-LAST-NAME-PRE
               MOVE EMP-NAME-SUFFIX              TO HRWS-NAME-SUFFIX
169300         PERFORM 750-HR-FORMAT-NAME
169400         MOVE HRWS-FORMAT-NAME             TO R1G2-EMP-NAME
169400                                              R6G2-EMP-NAME
                                                    R7G2-EMP-NAME
169500         MOVE R1GN2-PCT-EMPLOYEE           TO RPT-GROUP-REQUEST
169600         PERFORM 700-PRINT-RPT-GRP
169500         MOVE R6GN2-PCT-EMPLOYEE           TO RPT-GROUP-REQUEST
169600         PERFORM 700-PRINT-RPT-GRP
169500         MOVE R7GN2-PCT-EMPLOYEE           TO RPT-GROUP-REQUEST
169600         PERFORM 700-PRINT-RPT-GRP.
169700
170000     MOVE "PA100"                          TO CRT-ERROR-CAT.

169800     PERFORM 2960-PRINT-ACTION.

170200*** RETURN NEW ACCT-CATEGORY VALUES IN CASE EDIT HAS DEFAULTED ***
170300*** SO REPORT REFLECTS TRUE CHANGE                             ***
170400
170500     PERFORM
170600         VARYING I9 FROM 1 BY 1
170700         UNTIL  (I9 > 30)
170800         OR     (PA100WS-FIELD (I9) = ZEROES)
170900         IF  (PA100WS-FIELD (I9) = HREMP-ACCT-CATEGORY-DN)
171000         AND (PCT-POS-LEVEL    = 01)
P82120         AND (HREMP-FLD-CHG (HREMP-ACCT-CATEGORY-DN))
P82120         AND (HREMP-ACCT-CATEGORY NOT = SPACES)
171100             MOVE HREMP-ACCT-CATEGORY    TO PA100WS-NEW-VALUE (I9)
171200         END-IF
171300     END-PERFORM.
171400
171500     IF  (PCT-POS-LEVEL > 01)
171600         MOVE PCT-COMPANY                  TO DB-COMPANY
171700         MOVE PCT-EMPLOYEE                 TO DB-EMPLOYEE
171800         MOVE PCT-POS-LEVEL                TO DB-POS-LEVEL
P58564         MOVE PEPSET3-POS-LEVEL            TO WS-DB-BEG-RNG
P58564         PERFORM 850-FIND-BEGRNG-PEPSET3.
172100
172200     PERFORM 2950-PRINT-ACTION-DETAIL
172300         VARYING I1 FROM 1 BY 1
172400         UNTIL  (I1 > 30)
               OR     (PA100WS-FIELD (I1) = ZEROS).
172500
           IF  (PAPEP-FC                    = "A")
           AND (PAPEP-EFFECT-DATE       NOT = ZEROES)
               MOVE PAPEP-COMPANY          TO DB-COMPANY
               MOVE PAPEP-EMPLOYEE         TO DB-EMPLOYEE
               MOVE PAPEP-POS-LEVEL        TO DB-POS-LEVEL
P58564         MOVE PEPSET3-POS-LEVEL      TO WS-DB-BEG-RNG
P58564         PERFORM 850-FIND-BEGRNG-PEPSET3
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
242600         AND  (PAPEP-NBR-FTE            = PEP-FTE)
242700         AND  (PAPEP-WORK-SCHED         = PEP-WORK-SCHED)
               AND  (PAPEP-ANNUAL-HOURS       = PEP-ANNUAL-HOURS)
               AND  (PAPEP-EXP-DIST-CO        = PEP-EXP-COMPANY)
               AND  (PAPEP-EXP-ACCT-UNIT      = PEP-EXP-ACCT-UNIT)
               AND  (PAPEP-EXP-ACCOUNT        = PEP-EXP-ACCOUNT)
               AND  (PAPEP-EXP-SUB-ACCT       = PEP-EXP-SUB-ACCT)
               AND  (PAPEP-ACTIVITY           = PEP-ACTIVITY)
               AND  (PAPEP-ACCT-CATEGORY      = PEP-ACCT-CATEGORY)
               AND  ((PAPEP-CURRENCY-CODE     = PEP-CURRENCY-CODE)
               OR   (PAPEP-CURRENCY-CODE      = SPACES)
               OR   (PEP-CURRENCY-CODE        = SPACES))
               AND  ((PAPEP-BASE-CURRENCY     = PEP-BASE-CURRENCY)
               OR   (PAPEP-BASE-CURRENCY      = SPACES )
               OR   (PEP-BASE-CURRENCY        = SPACES ))
               AND  ((PAPEP-BASE-PAY-RATE     = PEP-BASE-PAY-RATE)
               OR   (PAPEP-BASE-PAY-RATE      = ZEROES )))
242800         AND  (PAPEP-END-DATE       NOT = PEP-END-DATE)
               AND  (PAPEP-BARGAIN-UNIT       = PEP-BARGAIN-UNIT)
               AND  (PAPEP-USER-AMOUNT        = PEP-USER-AMOUNT)
                   IF  (PEP-END-DATE      NOT = ZEROES)
                   AND (PAPEP-END-DATE        = ZEROES)
                   AND (PAPEP-EFFECT-DATE     > PEP-END-DATE)
                       CONTINUE
                   ELSE
                       MOVE "C"            TO PAPEP-FC.

176200     IF  (PRM-UPDATE-OPTION = "Y")
P63874         PERFORM 7500-GET-ACTION-OBJ-ID
180400         MOVE IFOBIWS-OBJ-ID         TO WS-OBJ-ID
180600         PERFORM 800-CREATE-PERSACTHST
180700         MOVE PCT-COMPANY            TO PAH-COMPANY
180700         MOVE PCT-ACTION-TYPE        TO PAH-ACTION-TYPE
181100         MOVE WS-ACTION-CODE         TO PAH-ACTION-CODE
181200         MOVE PA100WS-ACTION-NBR     TO PAH-ACTION-NBR
181300         MOVE PA100WS-EFFECT-DATE    TO PAH-EFFECT-DATE
181400         MOVE PCT-EMPLOYEE           TO PAH-EMPLOYEE
181500         MOVE PCT-ANT-END-DATE       TO PAH-ANT-END-DATE
101470         IF  (PA100WS-CHG-REASON NOT = SPACES)
101470              MOVE PA100WS-CHG-REASON 
101470                                     TO PAH-REASON (1)
101470         ELSE
101470              MOVE WS-REASON         TO PAH-REASON (1)
101470         END-IF 
101470*        MOVE PCT-REASON (1)         TO PAH-REASON (1)
101470*        MOVE PCT-REASON (2)         TO PAH-REASON (2)
101470*        MOVE PCT-USER-ID            TO PAH-USER-ID
101470         MOVE CRT-USER-NAME          TO PAH-USER-ID 
181900         MOVE WS-SYSTEM-DATE-YMD     TO PAH-DATE-STAMP
P80029         MOVE HHMMSS                 TO PAH-TIME-STAMP
182000         MOVE WS-OBJ-ID              TO PAH-OBJ-ID
               MOVE PCT-POS-LEVEL          TO PAH-POS-LEVEL
               MOVE PCT-UPDATE-BENEFIT     TO PAH-UPDATE-BENEFIT
               MOVE PCT-UPD-ABS-MGMT       TO PAH-UPD-ABS-MGMT
               MOVE PCT-HIST-CORR-FLAG     TO PAH-HIST-CORR-FLAG
               IF (PCT-ACTION-TYPE = "A" OR "E" OR "L")
                   MOVE PCT-UPDATE-REQ-DED TO PAH-UPDATE-REQ-DED
                   MOVE PCT-EDM-EFFECT-DT  TO PAH-EDM-EFFECT-DT
                   MOVE PCT-EDM-END-DATE   TO PAH-EDM-END-DATE
                   MOVE "2"                TO PAH-ACTION-UPD
               END-IF
               IF (PCT-ACTION-TYPE = "M" OR "P")
                   MOVE "3"                TO PAH-ACTION-UPD
               END-IF
J08104         IF  (PCT-CREATE-USER-ID NOT = SPACES)
J08104             MOVE PCT-CREATE-USER-ID TO PAH-CREATE-USER
J08104         ELSE
J08104             MOVE CRT-USER-NAME      TO PAH-CREATE-USER
J08104         END-IF
J08104         MOVE PCT-CREATE-DATE        TO PAH-CREATE-DATE
J08104         MOVE PCT-CREATE-TIME        TO PAH-CREATE-TIME
183800         PERFORM 820-STORE-PERSACTHST
184000         PERFORM
184100             VARYING I4 FROM 1 BY 1
184200             UNTIL  (I4 > 1999)
184400                 MOVE "X"                TO HREMP-LOG-FLAG (I4)
184500         END-PERFORM
184700         IF (PA100WS-FIELD (1) NOT = ZEROS)
184800             INITIALIZE                  PAPEP-UPDPEP-DATE
                   SET PAPEP-FROM-ACTION TO TRUE
185000             MOVE PCT-COMPANY         TO PADT-COMPANY
185100             MOVE PCT-EMPLOYEE        TO PADT-EMPLOYEE
185200             MOVE EMP-TERM-DATE       TO PADT-END-DATE
185300             MOVE PCT-POS-LEVEL       TO PADT-POS-LEVEL
185400             MOVE PA100WS-EFFECT-DATE TO PADT-EFFECT-DATE
185500             PERFORM 2300-PADT-DATE-CHECK
185600             MOVE ZEROS               TO CRT-ERROR-NBR
185700             IF  (PCT-POS-LEVEL > 01)
185800                 MOVE PADT-UPDPEP-DATE
185900                                 TO PAPEP-UPDPEP-DATE
186000                 MOVE WS-OBJ-ID      TO HREMP-ACT-OBJ-ID
                       MOVE "N"            TO HRWS-ADD
186500                 PERFORM 3000-PAPEP-PROCESS-TRAN
186600                 IF (PAPEP-UPDPEP-DATE = PCT-EFFECT-DATE)
186700                     INITIALIZE             PAPEP-UPDPEP-DATE
186800                 END-IF
186900             ELSE
187000                 MOVE PADT-UPDPEP-DATE
187100                                 TO HREMP-UPDPEP-DATE
187200                 MOVE WS-OBJ-ID      TO HREMP-ACT-OBJ-ID
                       MOVE "N"            TO HRWS-ADD
187300                 PERFORM 3000-HREMP-PROCESS-TRAN
187400                 IF (HREMP-UPDPEP-DATE = PCT-EFFECT-DATE)
187500                     INITIALIZE             HREMP-UPDPEP-DATE
                       END-IF
                   END-IF
189600             INITIALIZE                     HREMP-SCR-FIELDS
189700                                            HRPEM-SCR-FIELDS
                                                  PAPEP-SCR-FIELDS
189800             PERFORM 7000-HREMP-DEFAULT
189900             PERFORM 7000-HRPEM-DEFAULT
190400             MOVE PA100WS-SAVE-ACTION-TYPE TO DB-ACTION-TYPE
190500             MOVE PA100WS-SAVE-EFFECT-DATE TO DB-EFFECT-DATE
190600             MOVE PA100WS-SAVE-ACTION-CODE TO DB-ACTION-CODE
190700             MOVE PA100WS-SAVE-EMPLOYEE    TO DB-EMPLOYEE
190800             MOVE PA100WS-SAVE-ACTION-NBR  TO DB-ACTION-NBR
191300             PERFORM 840-MODIFY-PCTSET3.
190100
       2920-END.

193100******************************************************************
193200 2930-CHECK-ADD                  SECTION 50.
193300******************************************************************
193400
193500     IF (PCT-POS-LEVEL      NOT = PEP-POS-LEVEL)
193600         ADD PEP-FTE           TO PAPEPWS-SUM-FTE
193700                                  HREMPWS-SUM-FTE.
193800
193900     PERFORM 860-FIND-NXTRNG-PEPSET2.
194000
194100*****************************************************************
194200 2940-PROCESS-PAPCT              SECTION 60.
194300*****************************************************************
194500
194900     INITIALIZE PAPCT-SCR-FIELDS.
195000     MOVE PCT-COMPANY            TO PAPCT-COMPANY.
195100     MOVE PCT-EMPLOYEE           TO PAPCT-EMPLOYEE.
195200     MOVE PA100WS-FIELD (I1)     TO PAPCT-FLD-NBR.
195300     MOVE PA100WS-NEW-VALUE (I1) TO PAPCT-NEW-VALUE.
195400     MOVE 1                      TO PAPCT-FIELD-FN.
195500     IF  (PCT-POS-LEVEL > 01)
195600         PERFORM 5050-PAPCT-MOVE-TO-PAPEP
195700     ELSE
195800         PERFORM 5000-PAPCT-MOVE-TO-HREMP.
195900
209500*****************************************************************
209600 2950-PRINT-ACTION-DETAIL        SECTION 60.
209700*****************************************************************
209900
210400     MOVE PA100WS-FIELD (I1)           TO DB-FLD-NBR.
210500     PERFORM 840-FIND-PADSET1.
210600
211000     INITIALIZE                     R1G4-FIELD-DESC.
211100     IF (PADICT-FOUND)
               MOVE PAD-ITEM-NAME          TO CRT-PHRASE
               MOVE WS-PHRASE-SIZE         TO CRT-PHRASE-SIZE
               MOVE "N"                    TO CRT-PUT-DOTS
               PERFORM 900-GET-PHRASE-XLT
               MOVE CRT-PHRASE-XLT         TO R1G4-FIELD-DESC.
211300
211400     MOVE PA100WS-FIELD (I1)     TO HREMP-FLD-NBR.
211500     MOVE "Y"                        TO HREMP-FORMAT-FIELD.
211600     MOVE SPACES                     TO HREMP-VALUE.
211700
INTL       IF  (PADICT-FOUND)
INTL           MOVE PAD-DATA-TYPE          TO PAPCTIO-DATA-TYPE
               MOVE "N"                    TO PAPCTIO-DATA-CURR
INTL           MOVE PAD-DECIMALS           TO PAPCTIO-DATA-DECIMALS
INTL           MOVE PA100WS-FIELD-VALUE (I1)
                                           TO PAPCTIO-NEW-VALUE
INTL           PERFORM 9200-FORMAT-PAPCT-TO-DISPLAY
INTL           MOVE PAPCTIO-NEW-VALUE      TO R1G4-PCT-PRE-VALUE
INTL           MOVE PA100WS-NEW-VALUE (I1) TO PAPCTIO-NEW-VALUE
INTL           PERFORM 9200-FORMAT-PAPCT-TO-DISPLAY
INTL           MOVE PAPCTIO-NEW-VALUE      TO R1G4-PCT-NEW-VALUE
INTL       ELSE
               MOVE PA100WS-FIELD-VALUE (I1)
                                           TO R1G4-PCT-PRE-VALUE
213300         MOVE PA100WS-NEW-VALUE (I1) TO R1G4-PCT-NEW-VALUE.
213400
213500     MOVE R1GN4D-PCT-ACTION-DETAIL   TO RPT-GROUP-REQUEST.
213600
213700     PERFORM 700-PRINT-RPT-GRP.
213800
204000*****************************************************************
204100 2960-PRINT-ACTION               SECTION.
204200*****************************************************************
204300 2960-START.
204400
           IF (PA100WS-ADD)
               MOVE WS-ACTION-CODE         TO R1G3-PCT-ACTION-CODE
                                              R6G3-PCT-ACTION-CODE
                                              DB-ACTION-CODE
           ELSE
204500         MOVE PCT-ACTION-CODE        TO R1G3-PCT-ACTION-CODE
                                              R6G3-PCT-ACTION-CODE
204600                                        DB-ACTION-CODE.
204700     PERFORM 840-FIND-PATSET1.
204800     IF (PERSACTYPE-FOUND)
204900         MOVE PAT-DESCRIPTION        TO R1G3-ACTION-DESC
204900                                        R6G3-ACTION-DESC
205000     ELSE
205100         MOVE SPACES                 TO R1G3-ACTION-DESC
204900                                        R6G3-ACTION-DESC.
205200
           IF (PA100WS-ADD)
               MOVE PA100WS-ACTION-NBR     TO R1G3-PCT-ACTION-NBR
                                              R6G3-PCT-ACTION-NBR
               MOVE PA100WS-EFFECT-DATE    TO R1G3-PCT-EFFECT-DATE
                                              R6G3-PCT-EFFECT-DATE
               MOVE "SP"                   TO R1G3-SPECIAL
               MOVE SPACES                 TO R1G3-PCT-USER-ID
J08104                                        R1G3-PCT-CREATE-USER-ID
           ELSE
205300         MOVE PCT-ACTION-NBR         TO R1G3-PCT-ACTION-NBR
205300                                        R6G3-PCT-ACTION-NBR
205400         MOVE PCT-EFFECT-DATE        TO R1G3-PCT-EFFECT-DATE
205400                                        R6G3-PCT-EFFECT-DATE
               MOVE SPACES                 TO R1G3-SPECIAL
      *        MOVE PCT-USER-ID            TO R1G3-PCT-USER-ID
101470*        MOVE PCT-USER-ID            TO WS-USER-DBUIDKEY
101470         MOVE CRT-USER-NAME          TO WS-USER-DBUIDKEY
J21860         PERFORM 900-GET-USER-DISPLAY-NAME
J21860         PERFORM 5300-NAME-CHECK
J21860         MOVE WS-CHECK-NAME-OUTPUT   TO R1G3-PCT-USER-ID
J08104*        MOVE PCT-CREATE-USER-ID     TO R1G3-PCT-CREATE-USER-ID
J21860         MOVE PCT-CREATE-USER-ID     TO WS-USER-DBUIDKEY
J21860         PERFORM 900-GET-USER-DISPLAY-NAME
J21860         PERFORM 5300-NAME-CHECK
J21860         MOVE WS-CHECK-NAME-OUTPUT   TO R1G3-PCT-CREATE-USER-ID
J08104     END-IF.
205500     MOVE PCT-POS-LEVEL              TO R1G3-PCT-POS-LEVEL.
205600     MOVE PCT-ANT-END-DATE           TO R1G3-PCT-ANT-END-DATE.
205700     MOVE PCT-UPDATE-BENEFIT         TO R1G3-PCT-UPDATE-BENEFIT.
           MOVE PCT-UPD-ABS-MGMT           TO R1G3-PCT-UPD-ABS-MGMT.
205700     MOVE PCT-UPDATE-REQ-DED         TO R1G3-PCT-UPDATE-REQ-DED.
206800     MOVE SPACES                     TO R1G3-PCT-REASON-1.
           IF (PA100WS-ADD)
               IF (WS-REASON NOT = SPACES)
                   MOVE WS-REASON          TO R1G3-PCT-REASON-1
               END-IF
           ELSE
205800     IF (PCT-REASON (1) NOT = SPACES)
205900         MOVE PCT-REASON (1)         TO R1G3-PCT-REASON-1.
207000
208100     MOVE SPACES                     TO R1G3-PCT-REASON-2.

           IF  (PA100WS-REGULAR)
207100     AND (PCT-REASON (2) NOT = SPACES)
207200         MOVE PCT-REASON (2)         TO R1G3-PCT-REASON-2.
208300
208400     MOVE PCT-PARTICIPNT             TO R1G3-PARTICIPNT.
208500     MOVE PCT-OCCUR-TYPE             TO R1G3-OCCUR-TYPE.
208600
208700     MOVE R1GN3-PCT-ACTION-CODE      TO RPT-GROUP-REQUEST.
208800     PERFORM 700-PRINT-RPT-GRP.
           IF (PCT-UPDATE-REQ-DED           = "Y")
113700         MOVE R6GN3-PCT-ACTION-CODE   TO RPT-GROUP-REQUEST
113800         PERFORM 700-PRINT-RPT-GRP.
208900     IF  (PCT-PARTICIPNT NOT = ZEROS)
209000     OR  (PCT-OCCUR-TYPE NOT = SPACES)
209100         MOVE R1GN3-PCT-PARTICIPNT   TO RPT-GROUP-REQUEST
209200         PERFORM 700-PRINT-RPT-GRP.

           INITIALIZE R1G3-SPECIAL.
209300
214000*****************************************************************
214100 3000-PROCESS-MASS-CHANGE        SECTION.
214200*****************************************************************
214300 3000-START.
214400
           MOVE ZEROES                 TO WS-PROCESS-LEVEL-COUNT.
427000     IF (PRM-UPDATE-OPTION       = "Y")
               PERFORM 3100-CHECK-RESTART
               IF (WS-RESTART-PHASE-A NOT NUMERIC)
                   MOVE ZEROES             TO WS-RESTART-PHASE
               END-IF
               IF (WS-RESTART-PHASE    = 3)
                   GO TO 3000-END
               END-IF
J73509         OPEN EXTEND PA100FLDS-FILE.

           SET NO-EMPLOYEE-PRINTED TO TRUE.
214600
214700     MOVE "M"                    TO DB-ACTION-TYPE.
214800     MOVE WS-SV-ACTION-CODE      TO DB-ACTION-CODE.
214900     MOVE WS-SV-ACTION-NBR       TO DB-ACTION-NBR.
215000     MOVE WS-SV-EFFECT-DATE      TO DB-EFFECT-DATE.
215100     MOVE ZEROS                  TO DB-EMPLOYEE.
215200
215300     PERFORM 840-FIND-PCTSET1.
215400     IF (PERSACTION-NOTFOUND)
215500         GO TO 3000-END.
215600
           INITIALIZE HREMP-HIST-OVERRIDE-SW.
           IF  (PCT-HIST-CORR-FLAG NOT = SPACES)
               MOVE PCT-HIST-CORR-FLAG     TO HREMP-HIST-OVERRIDE-SW
           ELSE
               MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE
               PERFORM 840-FIND-PATSET1
               IF  (PERSACTYPE-FOUND)
                   MOVE PAT-HIST-CORR-FLAG TO HREMP-HIST-OVERRIDE-SW
               END-IF
           END-IF.

           SET PRPXL-UPDATES-BYPASS TO TRUE.
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36)
               IF  (PCT-FLD-NBR (I1) = HRPEM-LOCAT-CODE-DN)
               AND (PCT-NEW-VALUE (I1) NOT = SPACES)
                   SET PRPXL-UPDATES-ALLOWED TO TRUE
               END-IF
           END-PERFORM.

215700     MOVE PERSACTION             TO WSPCT-PERSACTION.
215800     MOVE ZEROS                  TO DB-SEQ-NBR.
215900     PERFORM 3550-PRINT-MASS-ACTION.
216000
           IF (WSPCT-PERS-GROUP        NOT = SPACES)
               MOVE WSPCT-PERS-GROUP         TO DB-GROUP-NAME
               PERFORM 840-FIND-PRGSET1
               IF (PRG-UPDATE-FLAG = "Y")
                   MOVE ZEROES                   TO REG6-EMPLOYEE
                   MOVE SPACES                   TO REG6-EMP-NAME
                   MOVE 334                      TO CRT-MSG-NBR
                   MOVE WSPCT-PERS-GROUP         TO CRT-ERR-VAR1
                   PERFORM 790-GET-MSG
219100             MOVE CRT-MESSAGE              TO REG6-ERROR-DESC
219300             MOVE WSPCT-ACTION-CODE        TO REG6-PCT-ACTION-CODE
                   MOVE WSPCT-ACTION-NBR         TO REG6-PCT-ACTION-NBR
219400             MOVE WSPCT-POS-LEVEL          TO REG6-PCT-POS-LEVEL
219500             MOVE WSPCT-EFFECT-DATE        TO REG6-PCT-EFFECT-DATE
                   MOVE WSPCT-ACTION-TYPE        TO REG6-ACTION-TYPE
219200             MOVE ERROR-LINE               TO RPT-GROUP-REQUEST
220400             PERFORM 700-PRINT-RPT-GRP
                   SET ERRORS-PRINTED TO TRUE
                   IF (PRM-UPDATE-OPTION       = "Y")
J73509                 CLOSE PA100FLDS-FILE SAVE
                   END-IF
P96000             PERFORM 900-SAVE-PRINT-FILES
220500             GO TO 3000-END.

           INITIALIZE WSSEL-PARAMS.
           MOVE WSPCT-COMPANY          TO WSSEL-COMPANY.
           MOVE WSPCT-PERS-GROUP       TO WSSEL-EMPLOYEE-GROUP.
           MOVE WSPCT-PROCESS-LEVEL    TO WSSEL-PROCESS-LEVEL (1).
           SET WSSEL-SECURITY          TO TRUE.
           PERFORM 8500-FIND-FIRST-EMPLOYEE.
           
           IF (PRM-UPDATE-OPTION = "Y")
               MOVE ZEROES             TO PA100WS-SEQ-COUNTER
                                          PA100WS-LN-NBR
               MOVE WSPCT-COMPANY      TO DB-COMPANY
               MOVE ZEROES             TO DB-EMP-APP
               MOVE "PA"               TO DB-CMT-TYPE
               MOVE WSPCT-ACTION-CODE  TO DB-ACTION-CODE
               MOVE WSPCT-EFFECT-DATE  TO DB-DATE
               MOVE ZEROES             TO DB-EMPLOYEE
               MOVE SPACES             TO DB-JOB-CODE
               MOVE PCT-ACTION-NBR     TO DB-LN-NBR
                                          PA100WS-LN-NBR
               MOVE PACSET2-LN-NBR     TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PACSET2
               IF (PACOMMENTS-FOUND)
                   PERFORM
                       UNTIL (PACOMMENTS-NOTFOUND)
                           COMPUTE PA100WS-SEQ-COUNTER =
                               (PA100WS-SEQ-COUNTER + 1)
                           PERFORM 860-FIND-NXTRNG-PACSET2
                   END-PERFORM
                   PERFORM 910-AUDIT-BEGIN
                   MOVE WSPCT-COMPANY     TO DB-COMPANY
                   MOVE WSPCT-ACTION-CODE TO DB-ACTION-CODE
                   PERFORM 840-MODIFY-PATSET1
                   MOVE PAT-LAST-CMT-SEQ  TO PA100WS-CMT-SEQ-NBR
                   COMPUTE PAT-LAST-CMT-SEQ =
                       (PAT-LAST-CMT-SEQ + PA100WS-SEQ-COUNTER)
                   PERFORM 820-STORE-PERSACTYPE
                   PERFORM 925-AUDIT-END
               END-IF.

216600     PERFORM 3400-PROCESS-EMPLOYEE
216700         UNTIL (EMPLOYEE-NOTFOUND).
216800
218800     IF (NO-EMPLOYEE-PRINTED)
               MOVE ZEROES                   TO REG6-EMPLOYEE
               MOVE SPACES                   TO REG6-EMP-NAME
218900         MOVE 323                      TO CRT-MSG-NBR
219000         PERFORM 790-GET-MSG
219100         MOVE CRT-MESSAGE              TO REG6-ERROR-DESC
219300         MOVE WSPCT-ACTION-CODE        TO REG6-PCT-ACTION-CODE
               MOVE WSPCT-ACTION-NBR         TO REG6-PCT-ACTION-NBR
219400         MOVE WSPCT-POS-LEVEL          TO REG6-PCT-POS-LEVEL
219500         MOVE WSPCT-EFFECT-DATE        TO REG6-PCT-EFFECT-DATE
               MOVE WSPCT-ACTION-TYPE        TO REG6-ACTION-TYPE
219200         MOVE ERROR-LINE               TO RPT-GROUP-REQUEST
220400         PERFORM 700-PRINT-RPT-GRP
               SET ERRORS-PRINTED TO TRUE
               IF (PRM-UPDATE-OPTION       = "Y")
J73509             CLOSE PA100FLDS-FILE SAVE
               END-IF
P96000         PERFORM 900-SAVE-PRINT-FILES
220500         GO TO 3000-END
           ELSE
               MOVE WS-PROCESS-LEVEL-COUNT TO R2GN4-PROCESS-LEVEL-COUNT
               MOVE R2GN4-MASS-TOTAL       TO RPT-GROUP-REQUEST.
               PERFORM 700-PRINT-RPT-GRP.
220600
220700     IF (PRM-UPDATE-OPTION NOT = "Y")
220800         GO TO 3000-END.
220900
P61255     IF (WS-MAX-OPS-COUNT = ZEROES)
221000         PERFORM 910-AUDIT-BEGIN.
221100
221200     MOVE WS-SV-ACTION-CODE      TO DB-ACTION-CODE.
221300     MOVE WS-SV-ACTION-NBR       TO DB-ACTION-NBR.
221400     MOVE WS-SV-EFFECT-DATE      TO DB-EFFECT-DATE.
221500     MOVE "M"                    TO DB-ACTION-TYPE.
221600     MOVE ZEROS                  TO DB-EMPLOYEE.
221700     PERFORM 840-MODIFY-PCTSET1.
221800     MOVE WSPCT-COMPANY          TO DB-COMPANY.
221900     MOVE ZEROES                 TO DB-EMP-APP.
222000     MOVE "PA"                   TO DB-CMT-TYPE.
222100     MOVE WSPCT-ACTION-CODE      TO DB-ACTION-CODE.
222200     MOVE WSPCT-EFFECT-DATE      TO DB-DATE.
222300     MOVE ZEROS                  TO DB-EMPLOYEE.
222400     MOVE SPACES                 TO DB-JOB-CODE.
222500     MOVE WSPCT-ACTION-NBR       TO DB-LN-NBR.
222600     MOVE PACSET2-LN-NBR         TO WS-DB-BEG-RNG.
222700     PERFORM 830-DELETERNG-PACSET2.
222800
222900     PERFORM 830-DELETE-PERSACTION.
P96000     PERFORM 900-SAVE-PRINT-FILES.
223000     PERFORM 925-AUDIT-END.
223100
427000     IF (PRM-UPDATE-OPTION       = "Y")
706100         CLOSE PA100FLDS-FILE SAVE.

223200 3000-END.
223300
156800*****************************************************************
156900 3100-CHECK-RESTART              SECTION.
157000*****************************************************************
157100 3100-START.
157200
714100     PERFORM 840-FIND-CKPSET1.
714300     IF (CKPOINT-FOUND)
P96000         MOVE CKP-RESTART-INFO    TO WS-RESTART-INFO

               MOVE WS-RESTART-WRK-NAME TO WS-PA100FLDS-NAME
               IF (WS-RESTART-EMPLOYEE-A NOT NUMERIC)
                   MOVE ZEROES             TO WS-RESTART-EMPLOYEE
               END-IF
P96000         IF (WS-RESTART-WRK-NAME NOT = SPACES)
P69000             IF (WS-RESTART-EMPLOYEE = ZEROES)
P96000                 SET WS-REST-REC-FOUND TO TRUE
P96000             ELSE
P69000                 SET WS-REST-REC-NOTFOUND TO TRUE
P69000             END-IF.

           IF (WS-PA100FLDS-NAME            = SPACES)
               INITIALIZE                  WS-RESTART-INFO
159100         PERFORM 910-AUDIT-BEGIN
               PERFORM 900-BUILD-TMP-FILE-NAME
               MOVE WS-TMP-FILE         TO WS-PA100FLDS-NAME
                                           WS-RESTART-WRK-NAME
706100         OPEN OUTPUT PA100FLDS-FILE
706100         CLOSE PA100FLDS-FILE

709900         PERFORM 840-MODIFY-CKPSET1
               MOVE WS-PA100FLDS-NAME   TO WS-RESTART-WRK-NAME
               MOVE HREMP-EMPLOYEE      TO WS-RESTART-EMPLOYEE
710100         MOVE WS-RESTART-INFO     TO CKP-RESTART-INFO
710200         PERFORM 820-STORE-CKPOINT
P58564         MOVE ZEROES              TO WS-MAX-OPS-COUNT
               PERFORM 900-SAVE-PRINT-FILES
159100         PERFORM 925-AUDIT-END.

160600 3100-END.
160700
223400*****************************************************************
223500 3400-PROCESS-EMPLOYEE           SECTION.
223600*****************************************************************
223700 3400-START.
223800
227600     IF  (WSPCT-DEPARTMENT NOT = SPACES)
227700     AND (WSPCT-DEPARTMENT NOT = EMP-DEPARTMENT)
227800         GO TO 3400-NEXT.
227900
228000     IF  (WSPCT-USER-LEVEL NOT = SPACES)
228100     AND (WSPCT-USER-LEVEL NOT = EMP-USER-LEVEL)
228200         GO TO 3400-NEXT.
228300
228800     IF  (WSPCT-SUPERVISOR NOT = SPACES)
228900     AND (WSPCT-SUPERVISOR NOT = EMP-SUPERVISOR)
229000         GO TO 3400-NEXT.

           IF (WS-RESTART-EMPLOYEE-A NOT NUMERIC)
               MOVE ZEROES             TO WS-RESTART-EMPLOYEE
           END-IF.
P69000     IF  (WS-RESTART-EMPLOYEE   NOT = ZEROES)            
P69000     AND (WS-REST-REC-NOTFOUND)
P69000         IF (EMP-EMPLOYEE <= WS-RESTART-EMPLOYEE)
P69000             GO TO 3400-NEXT
P69000         ELSE
P69000             SET WS-REST-REC-FOUND TO TRUE
P69000         END-IF.
P69000
           MOVE EMP-COMPANY            TO DB-COMPANY.
           MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
           PERFORM 840-FIND-PEMSET1.

228400     IF  (WSPCT-LOCAT-CODE NOT = SPACES)
228500     AND (WSPCT-LOCAT-CODE NOT = PEM-LOCAT-CODE)
228600         GO TO 3400-NEXT.

           MOVE EMP-LAST-NAME          TO HRWS-LAST-NAME.
           MOVE EMP-FIRST-NAME         TO HRWS-FIRST-NAME.
           MOVE EMP-MIDDLE-INIT        TO HRWS-MIDDLE-INIT.
           MOVE EMP-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE.
           MOVE EMP-NAME-SUFFIX        TO HRWS-NAME-SUFFIX.
           PERFORM 750-HR-FORMAT-NAME.

223900     INITIALIZE HREMP-SCR-FIELDS.
224000     INITIALIZE HRPEM-SCR-FIELDS.

224100     MOVE WSPCT-COMPANY          TO HREMP-COMPANY.
224200     MOVE 1                      TO HREMP-COMPANY-FN.
224300     MOVE EMP-EMPLOYEE           TO HREMP-EMPLOYEE.
224600     MOVE "I"                    TO HREMP-FC.
225300
225400     MOVE 1                      TO HREMP-EMPLOYEE-FN.
225500     MOVE ZEROS                  TO CRT-ERROR-NBR.
225600     INITIALIZE                     PAPEP-UPDPEP-DATE.
           SET PAPEP-FROM-ACTION TO TRUE.
225800
225900     MOVE 1                      TO HREMP-POS-LEVEL.
226000     PERFORM 2000-HREMP-EDIT-TRAN.
           MOVE HREMP-CURRENCY-CODE    TO PAPCT-EMP-CURRENCY.
           MOVE HREMP-CURR-ND          TO PAPCT-EMP-CURR-ND.
226100
226500     IF (ERROR-FOUND)
226600         GO TO 3400-NEXT.
226900
227000     MOVE "PA100"                TO CRT-ERROR-CAT.
227100
229100
           SET EMPLOYEE-PRINTED TO TRUE.

471200     IF (PRM-UPDATE-OPTION         = "Y")
               MOVE "PAACT"              TO IFOBIWS-OBJ-TYPE
P58564         IF (PA100WS-OBJ-ID-LEFT = ZEROES)
P58564             MOVE WS-MAX-OPS-IN-TRAN
P58564                                   TO IFOBIWS-NBR-OBJECTS
P58564                                      PA100WS-OBJ-ID-LEFT
P58564             PERFORM 7000-ASSIGN-OBJ-ID-70
P58564             MOVE IFOBIWS-OBJ-ID   TO PA100WS-MEM-OBJ-ID
P58564             SUBTRACT WS-MAX-OPS-IN-TRAN 
P58564                               FROM PA100WS-MEM-OBJ-ID
P58564         END-IF
P58564         ADD 1                     TO PA100WS-MEM-OBJ-ID
P58564         MOVE PA100WS-MEM-OBJ-ID   TO IFOBIWS-OBJ-ID
P58564         SUBTRACT 1            FROM PA100WS-OBJ-ID-LEFT
P58564         MOVE IFOBIWS-OBJ-ID       TO WS-OBJ-ID

229200     INITIALIZE HREMP-SCR-FIELDS.
229300     INITIALIZE HRPEM-SCR-FIELDS.
229500     MOVE "C"                    TO HREMP-FC.
229600     MOVE WSPCT-COMPANY          TO HREMP-COMPANY.
229700     MOVE 1                      TO HREMP-COMPANY-FN.
229800     MOVE EMP-EMPLOYEE           TO HREMP-EMPLOYEE.
229900     MOVE 1                      TO HREMP-EMPLOYEE-FN.
230000     MOVE WSPCT-EFFECT-DATE      TO HREMP-EFFECT-DATE.
230100     MOVE WSPCT-ACTION-CODE      TO HREMP-ACTION-CODE.
230200     MOVE WSPCT-ACTION-NBR       TO HREMP-ACTION-NBR.
230300     MOVE WSPCT-REASON (1)       TO HREMP-REASON1.
230400     MOVE WSPCT-REASON (2)       TO HREMP-REASON2.
230500     MOVE WSPCT-USER-ID          TO HREMP-USER-ID.
230600     MOVE WSPCT-ANT-END-DATE     TO HREMP-ANT-END-DATE.
230700     MOVE WSPCT-UPDATE-BENEFIT   TO HREMP-UPDATE-BENEFIT.
           MOVE WSPCT-UPD-ABS-MGMT     TO HREMP-UPDATE-ABSENCE-MGMT.
230800     MOVE "Y"                    TO HREMP-ACTION.
230800     MOVE "N"                    TO HREMP-UPDATE-EMP-GROUPS.
231000     MOVE ZEROS                  TO CRT-ERROR-NBR.

           SET ACTION-NOERROR TO TRUE.
           INITIALIZE HREMP-I2
                      HREMP-ERROR-SW.
           SET NOT-EDITING-USERFIELDS TO TRUE.
           INITIALIZE HREMPWS-MASS.
           SET HREMP-MASS-ACTION TO TRUE.
P60778     SET NO-PEP-FLD-CHG    TO TRUE.
231100     PERFORM 3410-MOVE-SCR-TO-WS
231200         VARYING I1 FROM 1 BY 1
231300         UNTIL  (I1 > 36).

           IF  (HREMP-NO-ERROR-FOUND)
               INITIALIZE                     PAPEP-UPDPEP-DATE
               SET PAPEP-FROM-ACTION TO TRUE
               MOVE WSPCT-COMPANY          TO PADT-COMPANY
               MOVE HREMP-EMPLOYEE         TO PADT-EMPLOYEE
               MOVE HREMP-TERM-DATE        TO PADT-END-DATE
               MOVE WSPCT-POS-LEVEL        TO PADT-POS-LEVEL
               MOVE WSPCT-EFFECT-DATE      TO PADT-EFFECT-DATE
               PERFORM 2300-PADT-DATE-CHECK
               MOVE ZEROS                  TO CRT-ERROR-NBR

               IF (WSPCT-FLD-NBR (1) NOT = ZEROS)
                   MOVE PADT-UPDPEP-DATE   TO HREMP-UPDPEP-DATE
                   MOVE 1                  TO HREMP-POS-LEVEL
                   PERFORM 2000-HREMP-EDIT-TRAN
                   MOVE HREMP-LAST-NAME        TO HRWS-LAST-NAME
                   MOVE HREMP-FIRST-NAME       TO HRWS-FIRST-NAME
                   MOVE HREMP-MIDDLE-INIT      TO HRWS-MIDDLE-INIT
                   MOVE HREMP-LAST-NAME-PRE    TO HRWS-LAST-NAME-PRE
                   MOVE HREMP-NAME-SUFFIX      TO HRWS-NAME-SUFFIX
                   PERFORM 750-HR-FORMAT-NAME
                   MOVE HREMP-CURRENCY-CODE    TO PAPCT-EMP-CURRENCY
                   MOVE HREMP-CURR-ND          TO PAPCT-EMP-CURR-ND

                   SET EDITING-USERFIELDS TO TRUE
                   PERFORM 3410-MOVE-SCR-TO-WS
                       VARYING I1 FROM 1 BY 1
                       UNTIL  (I1 > 36)
               END-IF

               IF (HREMP-UPDPEP-DATE = WSPCT-EFFECT-DATE)
                   INITIALIZE                 HREMP-UPDPEP-DATE
               END-IF
           END-IF.

P74237     IF  (HREMP-ERROR-FOUND)
P74237     AND (PRM-ERROR-OPTION  = 2)
P74237     AND (PRM-UPDATE-OPTION = "Y")
P74237     AND (WS-MAX-OPS-COUNT = ZEROES)
P74237          PERFORM 910-AUDIT-BEGIN.
P74237
           IF  (HREMP-ERROR-FOUND)
           AND (PRM-ERROR-OPTION  = 1)
           AND (PRM-UPDATE-OPTION = "Y")
P69000         IF (WS-MAX-OPS-COUNT = ZEROES)
P69000             PERFORM 910-AUDIT-BEGIN
P69000         END-IF
               PERFORM 5200-CREATE-PERSACTION
               MOVE PCT-ACTION-CODE        TO WS-ERR-ACTION-CODE
               MOVE PCT-ACTION-NBR         TO WS-ERR-ACTION-NBR
               MOVE PCT-ACTION-TYPE        TO WS-ERR-ACTION-TYPE
               MOVE 1                      TO WS-ERR-POS-LEVEL
               MOVE PCT-EFFECT-DATE        TO WS-ERR-EFFECT-DATE
           ELSE
               MOVE WSPCT-ACTION-CODE      TO WS-ERR-ACTION-CODE
               MOVE WSPCT-ACTION-NBR       TO WS-ERR-ACTION-NBR
               MOVE WSPCT-ACTION-TYPE      TO WS-ERR-ACTION-TYPE
               MOVE 1                      TO WS-ERR-POS-LEVEL
               MOVE WSPCT-EFFECT-DATE      TO WS-ERR-EFFECT-DATE
           END-IF.

           PERFORM 6000-PRINT-MESSAGES.

           IF  (HREMP-ERROR-FOUND)
P69900     AND (PRM-UPDATE-OPTION = "Y")
P69000         ADD 1 TO WS-MAX-OPS-COUNT
P69000         IF (WS-MAX-OPS-COUNT > WS-MAX-OPS-IN-TRAN)
P69000             PERFORM 840-MODIFY-CKPSET1
                   MOVE CKP-RESTART-INFO   TO WS-RESTART-INFO
P69000             MOVE HREMP-EMPLOYEE     TO WS-RESTART-EMPLOYEE
P69000             MOVE WS-RESTART-INFO    TO CKP-RESTART-INFO
P69000             PERFORM 820-STORE-CKPOINT
P69000             MOVE ZEROES             TO WS-MAX-OPS-COUNT
P69000             PERFORM 900-SAVE-PRINT-FILES
P69000             PERFORM 925-AUDIT-END
P69000         END-IF
               GO TO 3400-NEXT.

           ADD 1                         TO WS-PROCESS-LEVEL-COUNT.
           MOVE EMP-EMPLOYEE             TO R2G4-EMPLOYEE.
           MOVE HRWS-FORMAT-NAME         TO R2G4-EMP-NAME.
           MOVE R2GN4D-MASS-CHG-EMPLOYEE TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

           IF  (PRM-UPDATE-OPTION NOT = "Y")
               IF  (HREMP-IS-RETRO)
               OR  (PCT-PROCESS-TYPE NOT = SPACES)
                   PERFORM 3405-PRINT-RETRO.
236800
236900     MOVE "PA100"                TO CRT-ERROR-CAT.
237000
237100     IF (PRM-UPDATE-OPTION         NOT = "Y")
237200         GO TO 3400-NEXT.
237300
P58564     IF (WS-MAX-OPS-COUNT = ZEROES)
237400          PERFORM 910-AUDIT-BEGIN.
237500
237600     MOVE WSPCT-COMPANY          TO PAACT-COMPANY.
237700     MOVE "E"                    TO PAACT-ACTION-TYPE.
237800     MOVE HREMP-EMPLOYEE         TO PAACT-EMPLOYEE.
237900     MOVE WSPCT-ACTION-CODE      TO PAACT-ACTION-CODE.
238000     MOVE WSPCT-EFFECT-DATE      TO PAACT-EFFECT-DATE.
238100     MOVE "Y"                    TO PAACT-HISTORY.
238200     PERFORM 2300-PAACT-ACTION-NBR.
238300     MOVE PAACT-ACTION-NBR       TO PA100WS-ACTION-NBR.
238400     MOVE "L"                    TO PAACT-ACTION-TYPE.
238500     PERFORM 2300-PAACT-ACTION-NBR.
238600     IF (PAACT-ACTION-NBR > PA100WS-ACTION-NBR)
238700         MOVE PAACT-ACTION-NBR   TO PA100WS-ACTION-NBR.
238800
           IF  (HREMP-IS-RETRO)
           OR  (PCT-PROCESS-TYPE NOT = SPACES)
               PERFORM 3405-PRINT-RETRO.

238900     MOVE "M"                    TO DB-ACTION-TYPE.
239000     MOVE WS-SV-EFFECT-DATE      TO DB-EFFECT-DATE.
239100     MOVE WS-SV-ACTION-CODE      TO DB-ACTION-CODE.
239200     MOVE ZEROES                 TO DB-EMPLOYEE.
239300     MOVE WS-SV-ACTION-NBR       TO DB-ACTION-NBR.
239400     PERFORM 840-FIND-PCTSET1.
239500
           MOVE PA100WS-CMT-SEQ-NBR    TO PA100WS-SEQ-COUNTER.
239600     MOVE WSPCT-COMPANY          TO DB-COMPANY.
239700     MOVE ZEROES                 TO DB-EMP-APP.
239800     MOVE "PA"                   TO DB-CMT-TYPE.
239900     MOVE WSPCT-ACTION-CODE      TO DB-ACTION-CODE.
240000     MOVE WSPCT-EFFECT-DATE      TO DB-DATE.
240100     MOVE ZEROS                  TO DB-EMPLOYEE.
240200     MOVE SPACES                 TO DB-JOB-CODE.
240300     MOVE PA100WS-LN-NBR         TO DB-LN-NBR.
P58564     PERFORM 840-FIND-PATSET1.
240500     MOVE PACSET2-LN-NBR         TO WS-DB-BEG-RNG.
240600     PERFORM 850-FIND-BEGRNG-PACSET2.
240700     PERFORM
240800         UNTIL (PACOMMENTS-NOTFOUND)
240900
241000         PERFORM 810-RECREATE-PACOMMENTS
241100         MOVE EMP-EMPLOYEE       TO PAC-EMPLOYEE
241200         ADD 1                   TO PA100WS-SEQ-COUNTER
241300         MOVE PA100WS-SEQ-COUNTER
241300                                 TO PAC-SEQ-NBR
               MOVE PA100WS-ACTION-NBR TO PAC-LN-NBR
241400         PERFORM 820-STORE-PACOMMENTS
241500         PERFORM 860-FIND-NXTRNG-PACSET2
241600     END-PERFORM.
241700
242000     PERFORM
242100         VARYING I4 FROM 1 BY 1
242200         UNTIL  (I4 > 1999)
242300
242400         MOVE "X"                TO HREMP-LOG-FLAG (I4)
242500     END-PERFORM.
242600
243100     PERFORM 800-CREATE-PERSACTHST.
243200     MOVE WSPCT-COMPANY            TO PAH-COMPANY.
243300     MOVE WSPCT-ACTION-TYPE        TO PAH-ACTION-TYPE.
243600     MOVE WSPCT-ACTION-CODE        TO PAH-ACTION-CODE.
243700     MOVE PA100WS-ACTION-NBR       TO PAH-ACTION-NBR.
243800     MOVE WSPCT-EFFECT-DATE        TO PAH-EFFECT-DATE.
243900     MOVE HREMP-EMPLOYEE           TO PAH-EMPLOYEE.
244000     MOVE WSPCT-ANT-END-DATE       TO PAH-ANT-END-DATE.
244100     MOVE WSPCT-REASON (1)         TO PAH-REASON (1).
244200     MOVE WSPCT-REASON (2)         TO PAH-REASON (2).
244300     MOVE WSPCT-USER-ID            TO PAH-USER-ID.
244400     MOVE WS-SYSTEM-DATE-YMD       TO PAH-DATE-STAMP.
P80029     MOVE HHMMSS                   TO PAH-TIME-STAMP.
244500     MOVE WS-OBJ-ID                TO PAH-OBJ-ID.
           MOVE WSPCT-POS-LEVEL          TO PAH-POS-LEVEL.
           MOVE WSPCT-UPDATE-BENEFIT     TO PAH-UPDATE-BENEFIT.
           MOVE WSPCT-UPD-ABS-MGMT       TO PAH-UPD-ABS-MGMT.
           MOVE WSPCT-HIST-CORR-FLAG     TO PAH-HIST-CORR-FLAG.
           IF (WSPCT-ACTION-TYPE = "A" OR "E" OR "L")
               MOVE WSPCT-UPDATE-REQ-DED TO PAH-UPDATE-REQ-DED
               MOVE WSPCT-EDM-EFFECT-DT  TO PAH-EDM-EFFECT-DT
               MOVE WSPCT-EDM-END-DATE   TO PAH-EDM-END-DATE
               MOVE "2"                  TO PAH-ACTION-UPD
           END-IF.
           IF (WSPCT-ACTION-TYPE = "M" OR "P")
               MOVE SPACES             TO PAH-UPDATE-REQ-DED
               MOVE ZEROES             TO PAH-EDM-EFFECT-DT
                                          PAH-EDM-END-DATE
               MOVE "3"                TO PAH-ACTION-UPD
           END-IF.
J08104     IF  (WSPCT-CREATE-USER-ID NOT = SPACES)
J08104         MOVE WSPCT-CREATE-USER-ID TO PAH-CREATE-USER
J08104     ELSE
J08104         MOVE CRT-USER-NAME      TO PAH-CREATE-USER
J08104     END-IF.
J08104     MOVE WSPCT-CREATE-DATE      TO PAH-CREATE-DATE.
J08104     MOVE WSPCT-CREATE-TIME      TO PAH-CREATE-TIME.
246300
246400     PERFORM 820-STORE-PERSACTHST.
246500
CRP001     MOVE WSPCT-ACTION-CODE        TO HRCRP-ACTION-CODE.
CRP001     PERFORM 9000-CHECK-ACTION-CODE.
CRP001     IF (HRCRP-RECALC-ACTION = "N")
CRP001         GO TO 3400-SKIP-CRP-RECALC.
CRP001     MOVE WSPCT-COMPANY             TO HRCRP-COMPANY.
CRP001     MOVE WSPCT-EMPLOYEE            TO HRCRP-EMPLOYEE.
CRP001     PERFORM 8000-SETUP-WS-FOR-PA100.
CRP001     PERFORM 2000-CREATE-CRP-TRIGGER.
CRP001
CRP001 3400-SKIP-CRP-RECALC.
246600     MOVE WS-OBJ-ID              TO HREMP-ACT-OBJ-ID.
246700
246800     IF (WSPCT-FLD-NBR (1) NOT = ZEROS)
246900         INITIALIZE                   PAPEP-UPDPEP-DATE
               SET PAPEP-FROM-ACTION TO TRUE
247100         MOVE WSPCT-COMPANY        TO PADT-COMPANY
247200         MOVE HREMP-EMPLOYEE       TO PADT-EMPLOYEE
247300         MOVE HREMP-TERM-DATE      TO PADT-END-DATE
247400         MOVE 1                    TO PADT-POS-LEVEL
247500         MOVE WSPCT-EFFECT-DATE    TO PADT-EFFECT-DATE
247600         PERFORM 2300-PADT-DATE-CHECK
247700         MOVE ZEROS                TO CRT-ERROR-NBR
247800         MOVE PADT-UPDPEP-DATE     TO HREMP-UPDPEP-DATE
247900         INITIALIZE                   PAPEP-UPDPEP-DATE
               MOVE "N"                  TO HRWS-ADD
248000         PERFORM 3000-HREMP-PROCESS-TRAN
248100         IF (HREMP-UPDPEP-DATE = WSPCT-EFFECT-DATE)
248200             INITIALIZE             HREMP-UPDPEP-DATE
248300         END-IF
248400         PERFORM 5000-DO-WSPCT-USER-FIELDS.
248500
           PERFORM 3415-MOVE-FOR-GRP-UPD
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36).

P58564     ADD 1 TO WS-MAX-OPS-COUNT.
P58564     IF (WS-MAX-OPS-COUNT > WS-MAX-OPS-IN-TRAN)
P69000         PERFORM 840-MODIFY-CKPSET1
               MOVE CKP-RESTART-INFO       TO WS-RESTART-INFO
P69000         MOVE HREMP-EMPLOYEE         TO WS-RESTART-EMPLOYEE
P69000         MOVE WS-RESTART-INFO        TO CKP-RESTART-INFO
P69000         PERFORM 820-STORE-CKPOINT
PERF           MOVE ZEROES                 TO WS-MAX-OPS-COUNT
               PERFORM 900-SAVE-PRINT-FILES
248600         PERFORM 925-AUDIT-END.
248700
249500 3400-NEXT.
249600
           PERFORM 8600-FIND-NEXT-EMPLOYEE.
249900
250000 3400-END.
250100
250200*****************************************************************
250300 3405-PRINT-RETRO                SECTION.
250400*****************************************************************
250500 3405-START.

           MOVE EMP-EMPLOYEE           TO R4G7-PCT-EMPLOYEE.
           MOVE HRWS-FORMAT-NAME       TO R4G7-EMP-NAME.
           MOVE WSPCT-EFFECT-DATE      TO R4G7-PCT-EFFECT-DATE.
           MOVE WSPCT-ACTION-CODE      TO R4G7-PCT-ACTION-CODE.
           IF  (PRM-UPDATE-OPTION NOT = "Y")
               INITIALIZE                 R4G7-PCT-ACTION-NBR
           ELSE
               MOVE PA100WS-ACTION-NBR TO R4G7-PCT-ACTION-NBR
           END-IF.
           MOVE 1                      TO R4G7-PCT-POS-LEVEL.
           INITIALIZE                     R4G7-COMBINE-ACTION-NBR.
           INITIALIZE                     R4G7-REVERSE-ACTION-NBR.
           MOVE WSPCT-HIST-CORR-FLAG   TO R4G7-PCT-HIST-CORR-FLAG.
           INITIALIZE                     R4G7-RUN-PA113.
           SET RETRO-PCT-NOT-PRINTED TO TRUE.
           IF  (HREMP-RETRO-PENDING)
               MOVE "Y"                TO R4G7-RUN-PA113
           END-IF.
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 36)
               IF  (PCT-FLD-NBR (I1) NOT = ZEROES)
                   MOVE PCT-FLD-NBR (I1)   TO DB-FLD-NBR
                   IF  (HREMP-FLD-RETRO (DB-FLD-NBR))
                       PERFORM 840-FIND-PADSET1
                       INITIALIZE                     R4G7-RETRO-FIELD
                                                      R4G8-RETRO-FIELD
                       IF  (PADICT-FOUND)
                           MOVE PAD-ITEM-NAME      TO CRT-PHRASE
                           MOVE WS-PHRASE-SIZE     TO CRT-PHRASE-SIZE
                           MOVE "N"                TO CRT-PUT-DOTS
                           PERFORM 900-GET-PHRASE-XLT
                           MOVE CRT-PHRASE-XLT     TO R4G7-RETRO-FIELD
                                                      R4G8-RETRO-FIELD
                       END-IF
                       IF  (RETRO-PCT-NOT-PRINTED)
                           MOVE R4GN7-PCT-ACTION   TO RPT-GROUP-REQUEST
                           SET RETRO-PCT-PRINTED TO TRUE
                       ELSE
                           MOVE R4GN8-PCT-FIELD    TO RPT-GROUP-REQUEST
                       END-IF
                       PERFORM 700-PRINT-RPT-GRP
                   END-IF
               END-IF
           END-PERFORM.
           IF  (RETRO-PCT-NOT-PRINTED)
               MOVE R4GN7-PCT-ACTION     TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
           END-IF.
           SET HISTAUDITS-PRINTED TO TRUE.

       3405-END.

250200*****************************************************************
250300 3410-MOVE-SCR-TO-WS             SECTION.
250400*****************************************************************
250500 3410-START.
250600
250700     IF (WSPCT-NEW-VALUE (I1) = SPACES)
250800         GO TO 3410-END.
250900
           IF  ((EDITING-USERFIELDS)
            AND (PCT-FLD-NBR (I1) < 2000))
           OR  ((NOT-EDITING-USERFIELDS)
            AND (PCT-FLD-NBR (I1) > 1999))
                GO TO 3410-END.

           MOVE WSPCT-FLD-NBR (I1)       TO HREMPWS-MASS-FLD-NBR (I1).
251000     INITIALIZE PAPCT-SCR-FIELDS.
251100     MOVE WSPCT-COMPANY            TO PAPCT-COMPANY.
251200     MOVE HREMP-EMPLOYEE           TO PAPCT-EMPLOYEE.
251300     MOVE WSPCT-FLD-NBR (I1)       TO PAPCT-FLD-NBR.
251400     MOVE WSPCT-NEW-VALUE (I1)     TO PAPCT-NEW-VALUE.
251500     MOVE 1                        TO PAPCT-FIELD-FN.
           MOVE WSPCT-EFFECT-DATE        TO PAPCT-EFFECT-DATE.
           MOVE WSPCT-ACTION-CODE        TO PAPCT-ACTION-CODE.
           MOVE WSPCT-ACTION-NBR         TO PAPCT-ACTION-NBR.
           MOVE WSPCT-REASON (1)         TO PAPCT-REASON1.
           MOVE WSPCT-REASON (2)         TO PAPCT-REASON2.
           MOVE WSPCT-USER-ID            TO PAPCT-USER-ID.
           MOVE WSPCT-ANT-END-DATE       TO PAPCT-ANT-END-DATE.
           INITIALIZE                       PAPCT-EDIT-DATA-ONLY.

251600     PERFORM 5000-PAPCT-MOVE-TO-HREMP.
251700
251800     IF (ERROR-FOUND)
251900         MOVE CRT-ERROR-NBR        TO CRT-MSG-NBR
252000         PERFORM 790-GET-MSG
252100         ADD 1                     TO HREMP-I2
252200         MOVE CRT-MESSAGE          TO HREMP-MESSAGE (HREMP-I2)
252300         MOVE ZEROS                TO CRT-ERROR-NBR
               SET HREMP-ERROR-FOUND TO TRUE
               GO TO 3410-END.
P58444
P58444     IF (NO-PEP-FLD-CHG)
P58444         PERFORM
P58444            VARYING I2 FROM 1 BY 1
P60778            UNTIL  (I2 > 37)
P58444            IF  (PAPCT-FLD-NBR = PA100WS-PAT-FLD-NBR (I2))
P58444                 SET PEP-FLD-CHG   TO TRUE
P58444            END-IF
P58444         END-PERFORM
P58444     END-IF.
252400
252500 3410-END.
252600
      *****************************************************************
       3415-MOVE-FOR-GRP-UPD           SECTION.
      *****************************************************************
       3415-START.

           IF (WSPCT-NEW-VALUE (I1) = SPACES)
               GO TO 3415-END.

           IF   (WSPCT-POS-LEVEL    = 1)
           AND ((WSPCT-FLD-NBR (I1) = HREMP-STATE-DN)
           OR   (WSPCT-FLD-NBR (I1) = HRPEM-SUPP-STATE-DN))
               IF (EMP-WORK-COUNTRY = "US")
                   IF (PRS-COMPANY       NOT = PCT-COMPANY)
                   OR (PRS-PROCESS-LEVEL NOT = HREMP-PROCESS-LEVEL)
                       MOVE WSPCT-COMPANY         TO DB-COMPANY
                       MOVE HREMP-PROCESS-LEVEL TO DB-PROCESS-LEVEL
                       PERFORM 840-FIND-PRSSET1
                   END-IF
                   IF  (WSPCT-FLD-NBR (I1) = HREMP-STATE-DN)
                   AND (PRS-EMP-TAX-ADDR = 2)
                       MOVE WSPCT-COMPANY    TO WS-WRK-COMPANY
                       MOVE EMP-EMPLOYEE     TO WS-WRK-EMPLOYEE
                       MOVE WSPCT-EFFECT-DATE
                                           TO WS-WRK-EFFECT-DATE
                       MOVE WSPCT-ACTION-CODE
                                           TO WS-WRK-ACTION-CODE
                       MOVE WSPCT-ACTION-NBR TO WS-WRK-ACTION-NBR
                       MOVE WS-OBJ-ID      TO WS-WRK-ACT-OBJ-ID
                       MOVE WSPCT-UPDATE-BENEFIT
                                           TO WS-WRK-UPDATE-BENEFIT
                       MOVE HREMP-TAX-STATE-DN
                                           TO WS-WRK-FLD-NBR
                       MOVE WSPCT-NEW-VALUE (I1)
                                           TO WS-WRK-NEW-VALUE
                       PERFORM 5400-ADD-GRP-FLD
                   END-IF
                   IF  (WSPCT-FLD-NBR (I1) = HRPEM-SUPP-STATE-DN)
                   AND (PRS-EMP-TAX-ADDR = 1)
                       MOVE WSPCT-COMPANY    TO WS-WRK-COMPANY
                       MOVE EMP-EMPLOYEE     TO WS-WRK-EMPLOYEE
                       MOVE WSPCT-EFFECT-DATE
                                           TO WS-WRK-EFFECT-DATE
                       MOVE WSPCT-ACTION-CODE
                                           TO WS-WRK-ACTION-CODE
                       MOVE WSPCT-ACTION-NBR TO WS-WRK-ACTION-NBR
                       MOVE WS-OBJ-ID      TO WS-WRK-ACT-OBJ-ID
                       MOVE WSPCT-UPDATE-BENEFIT
                                           TO WS-WRK-UPDATE-BENEFIT
                       MOVE HREMP-TAX-STATE-DN
                                           TO WS-WRK-FLD-NBR
                       MOVE WSPCT-NEW-VALUE (I1)
                                           TO WS-WRK-NEW-VALUE
                       PERFORM 5400-ADD-GRP-FLD
                   END-IF.

           IF  (WSPCT-POS-LEVEL    = 1)
           AND (WSPCT-FLD-NBR (I1) = HRPEM-LOCAT-CODE-DN)
               MOVE "LO"               TO DB-TYPE
               MOVE WSPCT-NEW-VALUE (I1) TO DB-CODE
               PERFORM 840-FIND-PDDSET1
               IF (PCODESDTL-FOUND)
                   MOVE WSPCT-COMPANY    TO WS-WRK-COMPANY
                   MOVE EMP-EMPLOYEE     TO WS-WRK-EMPLOYEE
                   MOVE WSPCT-EFFECT-DATE
                                       TO WS-WRK-EFFECT-DATE
                   MOVE WSPCT-ACTION-CODE
                                       TO WS-WRK-ACTION-CODE
                   MOVE WSPCT-ACTION-NBR TO WS-WRK-ACTION-NBR
                   MOVE WS-OBJ-ID      TO WS-WRK-ACT-OBJ-ID
                   MOVE WSPCT-UPDATE-BENEFIT
                                       TO WS-WRK-UPDATE-BENEFIT
                   MOVE HREMP-WORK-STATE-DN
                                       TO WS-WRK-FLD-NBR
                   MOVE PDD-STATE      TO WS-WRK-NEW-VALUE
                   PERFORM 5400-ADD-GRP-FLD
               END-IF.

           MOVE WSPCT-COMPANY            TO WS-WRK-COMPANY.
           MOVE EMP-EMPLOYEE             TO WS-WRK-EMPLOYEE.
           MOVE WSPCT-EFFECT-DATE        TO WS-WRK-EFFECT-DATE.
           MOVE WSPCT-ACTION-CODE        TO WS-WRK-ACTION-CODE.
           MOVE WSPCT-ACTION-NBR         TO WS-WRK-ACTION-NBR.
           MOVE WSPCT-FLD-NBR (I1)       TO WS-WRK-FLD-NBR.
           MOVE WSPCT-NEW-VALUE (I1)     TO WS-WRK-NEW-VALUE.
           MOVE WS-OBJ-ID                TO WS-WRK-ACT-OBJ-ID.
           MOVE WSPCT-UPDATE-BENEFIT     TO WS-WRK-UPDATE-BENEFIT.
           MOVE WSPCT-UPD-ABS-MGMT       TO WS-WRK-UPD-ABS-MGMT.
           PERFORM 5400-ADD-GRP-FLD.

       3415-END.

252700*****************************************************************
252800 3550-PRINT-MASS-ACTION          SECTION.
252900*****************************************************************
253000 3550-START.
253100
253200     MOVE SPACES                 TO DB-PROCESS-LEVEL.
253300     PERFORM 840-FIND-PRSSET1.
253400
253500     MOVE WSPCT-COMPANY          TO R2G1-PCT-COMPANY.
253600     MOVE PRS-NAME               TO R2G1-PRS-NAME.
254400     MOVE PRM-UPDATE-OPTION      TO R2G1-UPDATE-OPTION.
254500
254600     MOVE R2GN1-MASS-CHG-COMPANY TO RPT-GROUP-REQUEST.
254700     PERFORM 700-PRINT-RPT-GRP.
254800
254900     MOVE WSPCT-UPDATE-BENEFIT     TO R2G2-PCT-UPDATE-BENEFIT.
           MOVE WSPCT-UPD-ABS-MGMT       TO R2G2-PCT-UPD-ABS-MGMT.
255000     MOVE WSPCT-POS-LEVEL          TO R2G2-PCT-POS-LEVEL.
255100     MOVE WSPCT-PROCESS-LEVEL      TO R2G2-PCT-PROC-LEV.
255200     MOVE WSPCT-DEPARTMENT         TO R2G2-PCT-DEPARTMENT.
255400     MOVE WSPCT-USER-LEVEL         TO R2G2-PCT-USER-LEVEL.
255500     MOVE WSPCT-LOCAT-CODE         TO R2G2-PCT-LOCAT-CODE.
255600     MOVE WSPCT-SUPERVISOR         TO R2G2-PCT-SUPERVISOR.
255700     MOVE WSPCT-UNION-CODE         TO R2G2-PCT-UNION-CODE.
255800     MOVE WSPCT-PERS-GROUP         TO R2G2-PCT-PERS-GROUP.
      *    MOVE WSPCT-USER-ID            TO R2G2-PCT-USER-ID.
J21860     MOVE WSPCT-USER-ID            TO WS-USER-DBUIDKEY.
J21860     PERFORM 900-GET-USER-DISPLAY-NAME.
J21860     PERFORM 5300-NAME-CHECK.
J21860     MOVE WS-CHECK-NAME-OUTPUT     TO R2G2-PCT-USER-ID.
J08104*    MOVE WSPCT-CREATE-USER-ID     TO R2G2-PCT-CREATE-USER-ID.
J21860     MOVE WSPCT-CREATE-USER-ID     TO WS-USER-DBUIDKEY.
J21860     PERFORM 900-GET-USER-DISPLAY-NAME.
J21860     PERFORM 5300-NAME-CHECK.
J21860     MOVE WS-CHECK-NAME-OUTPUT     TO R2G2-PCT-CREATE-USER-ID.
255900     MOVE WSPCT-ACTION-CODE        TO R2G2-PCT-ACTION-CODE
256000                                    DB-ACTION-CODE.
256100     PERFORM 840-FIND-PATSET1.
256200     MOVE PAT-DESCRIPTION          TO R2G2-ACTION-DESC.
256300     MOVE WSPCT-EFFECT-DATE        TO R2G2-PCT-EFFECT-DATE.
256400     MOVE WSPCT-ANT-END-DATE       TO R2G2-PCT-ANT-END-DATE.
256500
256600     IF (WSPCT-REASON (1) NOT = SPACES)
256700         MOVE WSPCT-REASON (1)     TO R2G2-PCT-REASON-1
257200     ELSE
257300         MOVE SPACES             TO R2G2-PCT-REASON-1.
257500
257600     IF (WSPCT-REASON (2) NOT = SPACES)
257700         MOVE WSPCT-REASON (2)     TO R2G2-PCT-REASON-2
258200     ELSE
258300         MOVE SPACES             TO R2G2-PCT-REASON-2.
258500
258600     MOVE R2GN2-MASS-ACTION      TO RPT-GROUP-REQUEST.
258700     PERFORM 700-PRINT-RPT-GRP.
258800
           IF (PA100WS-1ST-PAGE)
               MOVE R2GN2F-MASS-ACTION      TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
258900         PERFORM 3560-PRINT-ACTION-DETAIL
259000             VARYING I1 FROM 1 BY 1
259100             UNTIL  (I1 > 36)
               SET PA100WS-NOT-1ST-PAGE  TO TRUE.
259200
260100     MOVE ZEROES                   TO DB-EMP-APP.
260200     MOVE "PA"                     TO DB-CMT-TYPE.
260300     MOVE WSPCT-ACTION-CODE        TO DB-ACTION-CODE.
260400     MOVE WSPCT-EFFECT-DATE        TO DB-DATE.
260500     MOVE ZEROS                    TO DB-EMPLOYEE.
260600     MOVE SPACES                   TO DB-JOB-CODE.
260700     MOVE WSPCT-ACTION-NBR         TO DB-LN-NBR.
260800     MOVE PACSET2-LN-NBR           TO WS-DB-BEG-RNG.
260900     PERFORM 850-FIND-BEGRNG-PACSET2.
261000     PERFORM 860-FIND-NXTRNG-PACSET2
261100         UNTIL (PACOMMENTS-NOTFOUND)
261200         OR    (PAC-PRINT-CODE  NOT = "N").
261300
261400     IF (PACOMMENTS-FOUND)
261500         MOVE GN6H-PAC-SEQ-NBR   TO RPT-GROUP-REQUEST
261600         PERFORM 700-PRINT-RPT-GRP.
261700
261800     PERFORM 1755-PRINT-COMMENTS
261900         UNTIL (PACOMMENTS-NOTFOUND).
262000
262100 3550-END.
262200*****************************************************************
262300 3560-PRINT-ACTION-DETAIL        SECTION.
262400*****************************************************************
262500 3560-START.
262600
262700     IF (WSPCT-FLD-NBR (I1) = ZEROS)
262800         MOVE 36                     TO I1
262900         GO TO 3560-END.
263000
263400     MOVE WSPCT-FLD-NBR (I1)     TO DB-FLD-NBR.
263500     PERFORM 840-FIND-PADSET1.
263600     IF (PADICT-FOUND)
               MOVE PAD-ITEM-NAME      TO CRT-PHRASE
               MOVE WS-PHRASE-SIZE     TO CRT-PHRASE-SIZE
               MOVE "N"                TO CRT-PUT-DOTS
               PERFORM 900-GET-PHRASE-XLT
               MOVE CRT-PHRASE-XLT     TO R2G3-FIELD-DESC
263800     ELSE
263900         MOVE SPACES             TO R2G3-FIELD-DESC.
264000
INTL       IF  (PADICT-FOUND)
INTL           MOVE PAD-DATA-TYPE          TO PAPCTIO-DATA-TYPE
               MOVE "N"                    TO PAPCTIO-DATA-CURR
INTL           MOVE PAD-DECIMALS           TO PAPCTIO-DATA-DECIMALS
INTL           MOVE WSPCT-NEW-VALUE (I1)   TO PAPCTIO-NEW-VALUE
INTL           PERFORM 9200-FORMAT-PAPCT-TO-DISPLAY
INTL           MOVE PAPCTIO-NEW-VALUE      TO R2G3-PCT-NEW-VALUE
INTL       ELSE
264100         MOVE WSPCT-NEW-VALUE (I1)   TO R2G3-PCT-NEW-VALUE.
264200
264300     MOVE R2GN3D-MASS-ACTION-DETAIL  TO RPT-GROUP-REQUEST.
264400     PERFORM 700-PRINT-RPT-GRP.
264500
264600 3560-END.
266500*****************************************************************
266600 4000-PROCESS-PAY-CHANGE         SECTION.
266700*****************************************************************
266800 4000-START.
266900
           INITIALIZE HREMP-HIST-OVERRIDE-SW.
           IF  (PCT-HIST-CORR-FLAG NOT = SPACES)
               MOVE PCT-HIST-CORR-FLAG     TO HREMP-HIST-OVERRIDE-SW
           ELSE
               MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE
               PERFORM 840-FIND-PATSET1
               IF  (PERSACTYPE-FOUND)
                   MOVE PAT-HIST-CORR-FLAG TO HREMP-HIST-OVERRIDE-SW
               END-IF
           END-IF.

267000     PERFORM 4210-CHECK-FOR-RESTART.
267100
267200     IF (RESTARTING)
               IF (WS-RESTART-PHASE-A NOT NUMERIC)
                   MOVE ZEROES             TO WS-RESTART-PHASE
               END-IF
267300         IF (WS-RESTART-PHASE = 2)
267400             OPEN INPUT WORK-FILE
267500             PERFORM 4600-PRINT-REPORT
267600             CLOSE WORK-FILE PURGE
267700             MOVE WS-WORKFILE-NAME   TO WS-TMP-FILE
267800             PERFORM 901-REMOVE-TMP-FILE
267900             GO TO 4000-END
               ELSE
705500         IF (WS-RESTART-PHASE = 3)
705900             GO TO 4000-END.
268000
268100     PERFORM 900-BUILD-TMP-FILE-NAME.
268200     MOVE WS-TMP-FILE            TO WS-WORKFILE-NAME.
268300     OPEN OUTPUT WORK-FILE.
268400     PERFORM 4200-BUILD-SORT-FILE.
268500     CLOSE WORK-FILE SAVE.
268600
268700     MOVE ZEROES                 TO CRT-ERROR-NBR.
268800
268900     PERFORM 4100-PROCESS-SORT.
269000
269100     IF (RECORD-FOUND)
269200         OPEN INPUT WORK-FILE
               IF (PRM-UPDATE-OPTION   = "Y")
J73509             OPEN EXTEND PA100FLDS-FILE
               END-IF
269300         PERFORM 4600-PRINT-REPORT
269400         CLOSE WORK-FILE PURGE
427000         IF (PRM-UPDATE-OPTION   = "Y")
J73509             CLOSE PA100FLDS-FILE SAVE
               END-IF
269500         MOVE WS-WORKFILE-NAME   TO WS-TMP-FILE
269600         PERFORM 901-REMOVE-TMP-FILE
269700     ELSE
               SET ERRORS-PRINTED TO TRUE
269800         MOVE 324                     TO CRT-MSG-NBR
269900         PERFORM 790-GET-MSG
270000         MOVE CRT-MESSAGE             TO REG6-ERROR-DESC
270100         MOVE PCT-ACTION-CODE         TO REG6-PCT-ACTION-CODE
               MOVE PCT-ACTION-NBR          TO REG6-PCT-ACTION-NBR
270200         MOVE 1                       TO REG6-PCT-POS-LEVEL
270300         MOVE PCT-EFFECT-DATE         TO REG6-PCT-EFFECT-DATE
               MOVE PCT-ACTION-TYPE         TO REG6-ACTION-TYPE
271500         MOVE ERROR-LINE              TO RPT-GROUP-REQUEST
271600         PERFORM 700-PRINT-RPT-GRP
               SET ERRORS-PRINTED TO TRUE
271700         GO TO 4000-END.
271800
           INITIALIZE WS-RECORD-COUNT.

271900 4000-END.
272000
272100*****************************************************************
272200 4100-PROCESS-SORT               SECTION.
272300*****************************************************************
272400 4100-START.
272500
           IF (WS-RESTART-PHASE-A NOT NUMERIC)
               MOVE ZEROES             TO WS-RESTART-PHASE
           END-IF.
272600     IF  (RESTARTING)
272700     AND (WS-RESTART-PHASE = 2)
272800         GO TO 4100-END.
272900
273000     IF (RECORD-FOUND)
273100         SORT SORT-FILE
273200             ASCENDING KEY       DSF-ACTION-CODE
273300                                 DSF-ACTION-NBR
273400                                 DSF-PROCESS-LEVEL
273500                                 DSF-DEPARTMENT
273600                                 DSF-USER-LEVEL
273700                                 DSF-EMPLOYEE
273800             USING               WORK-FILE
273900             GIVING              WORK-FILE.
274000
P58564     IF (PRM-UPDATE-OPTION = "Y")
274100         PERFORM 910-AUDIT-BEGIN
274200         PERFORM 840-MODIFY-CKPSET1
               MOVE CKP-RESTART-INFO       TO WS-RESTART-INFO
274300         MOVE 2                      TO WS-RESTART-PHASE
274400         MOVE WS-WORKFILE-NAME       TO WS-RESTART-WF-NAME
               MOVE WS-PA100FLDS-NAME      TO WS-RESTART-WRK-NAME
               MOVE ZEROES                 TO WS-RESTART-EMPLOYEE
274500         MOVE WS-RESTART-INFO        TO CKP-RESTART-INFO
274600         PERFORM 820-STORE-CKPOINT
P58564         MOVE ZEROES                 TO WS-MAX-OPS-COUNT
274800         PERFORM 900-SAVE-PRINT-FILES
274700         PERFORM 925-AUDIT-END.
274900
275000 4100-END.
275100
275200*****************************************************************
275300 4200-BUILD-SORT-FILE            SECTION.
275400*****************************************************************
275500 4200-START.
275600
275700     MOVE "P"                    TO DB-ACTION-TYPE.
275800     MOVE ZEROES                 TO DB-EMPLOYEE.
275900     MOVE WS-SV-ACTION-CODE      TO DB-ACTION-CODE.
276000     MOVE WS-SV-EFFECT-DATE      TO DB-EFFECT-DATE.
276100     MOVE WS-SV-ACTION-NBR       TO DB-ACTION-NBR.
276200
276300     PERFORM 840-FIND-PCTSET1.
276400
276500     IF (PERSACTION-NOTFOUND)
276600         GO TO 4200-END.
276700
276800     IF (PCT-PERS-GROUP NOT = SPACES)
276900         MOVE PCT-PERS-GROUP     TO DB-GROUP-NAME
277000         PERFORM 840-FIND-PRGSET1
277100         IF (PRG-UPDATE-FLAG = "Y")
                   MOVE ZEROES                   TO REG6-EMPLOYEE
                   MOVE SPACES                   TO REG6-EMP-NAME
                   MOVE 334                      TO CRT-MSG-NBR
                   MOVE PCT-PERS-GROUP           TO CRT-ERR-VAR1
                   PERFORM 790-GET-MSG
219100             MOVE CRT-MESSAGE              TO REG6-ERROR-DESC
219300             MOVE PCT-ACTION-CODE          TO REG6-PCT-ACTION-CODE
                   MOVE PCT-ACTION-NBR           TO REG6-PCT-ACTION-NBR
219400             MOVE PCT-POS-LEVEL            TO REG6-PCT-POS-LEVEL
219500             MOVE PCT-EFFECT-DATE          TO REG6-PCT-EFFECT-DATE
                   MOVE PCT-ACTION-TYPE          TO REG6-ACTION-TYPE
219200             MOVE ERROR-LINE               TO RPT-GROUP-REQUEST
220400             PERFORM 700-PRINT-RPT-GRP
                   SET ERRORS-PRINTED TO TRUE
220500             GO TO 4200-END.
277600
           IF  (PCT-EMPLOYEE NOT = ZEROES)
               MOVE PCT-COMPANY        TO DB-COMPANY
               MOVE PCT-EMPLOYEE       TO DB-EMPLOYEE
               PERFORM 840-FIND-EMPSET1
               IF  (EMPLOYEE-FOUND)
                   PERFORM 4300-PROCESS-EMPLOYEE
               END-IF
               GO TO 4200-END.

           INITIALIZE WSSEL-PARAMS.
           MOVE PCT-COMPANY            TO WSSEL-COMPANY.
           MOVE PCT-PERS-GROUP         TO WSSEL-EMPLOYEE-GROUP.
           MOVE PCT-PROCESS-LEVEL      TO WSSEL-PROCESS-LEVEL (1).
           SET WSSEL-SECURITY          TO TRUE.
           PERFORM 8500-FIND-FIRST-EMPLOYEE.

           SET ACTION-NOERROR TO TRUE.
           PERFORM 4300-PROCESS-EMPLOYEE
               UNTIL (EMPLOYEE-KNOTFOUND).
278900
279000 4200-END.
279100
279200*****************************************************************
279300 4210-CHECK-FOR-RESTART          SECTION.
279400*****************************************************************
279500 4210-START.
279600
279700     MOVE ZEROS                  TO WS-RECORD-COUNT.
           SET NOT-RESTARTING TO TRUE.
           IF  (PRM-UPDATE-OPTION NOT = "Y")
               GO TO 4210-END.

279900     PERFORM 840-FIND-CKPSET1.
280000
280100     IF (CKPOINT-FOUND)
P96000         MOVE CKP-RESTART-INFO        TO WS-RESTART-INFO

280200         IF (WS-RESTART-WRK-NAME NOT = SPACES)
280400             MOVE WS-RESTART-WF-NAME  TO WS-WORKFILE-NAME
                   MOVE WS-RESTART-WRK-NAME TO WS-PA100FLDS-NAME
                   IF (WS-RESTART-EMPLOYEE-A NOT NUMERIC)
                       MOVE ZEROES             TO WS-RESTART-EMPLOYEE
                   END-IF
P69000             IF (WS-RESTART-EMPLOYEE = ZEROES)
P96000                 SET WS-REST-REC-FOUND TO TRUE
                       MOVE 1            TO WS-RESTART-PHASE
P96000             ELSE
P69000                 SET WS-REST-REC-NOTFOUND TO TRUE
P69000             END-IF
                   SET RESTARTING TO TRUE
280600         ELSE
280700             PERFORM 910-AUDIT-BEGIN
280800             PERFORM 840-MODIFY-CKPSET1
280900             INITIALIZE WS-RESTART-INFO
281000             MOVE 1                  TO WS-RESTART-PHASE
                   IF (PRM-UPDATE-OPTION    = "Y")
                       PERFORM 900-BUILD-TMP-FILE-NAME
                       MOVE WS-TMP-FILE     TO WS-PA100FLDS-NAME
706100                 OPEN OUTPUT PA100FLDS-FILE
                       CLOSE PA100FLDS-FILE
                   END-IF
281100             MOVE WS-RESTART-INFO    TO CKP-RESTART-INFO
281200             PERFORM 820-STORE-CKPOINT
P58564             MOVE ZEROES             TO WS-MAX-OPS-COUNT
                   PERFORM 900-SAVE-PRINT-FILES
281300             PERFORM 925-AUDIT-END
               END-IF.
281400
281500 4210-END.
281600
281700*****************************************************************
281800 4300-PROCESS-EMPLOYEE           SECTION.
281900*****************************************************************
282000 4300-START.
282100
286200     IF  (PCT-DEPARTMENT NOT = SPACES)
286300     AND (PCT-DEPARTMENT NOT = EMP-DEPARTMENT)
286400         GO TO 4300-NEXT.
286500
286600     IF  (PCT-USER-LEVEL NOT = SPACES)
286700     AND (PCT-USER-LEVEL NOT = EMP-USER-LEVEL)
286800         GO TO 4300-NEXT.
286900
287400     IF  (PCT-SUPERVISOR NOT = SPACES)
287500     AND (PCT-SUPERVISOR NOT = EMP-SUPERVISOR)
287600         GO TO 4300-NEXT.
287700
287800     IF  (PCT-UNION-CODE NOT = SPACES)
287900     AND (PCT-UNION-CODE NOT = EMP-UNION-CODE)
288000         GO TO 4300-NEXT.
288100
288200     IF (EMP-PAY-STEP NOT = ZEROES)
288300         GO TO 4300-NEXT.
288400
288500     IF (PCT-SALARY-CLASS NOT = EMP-SALARY-CLASS)
288600         GO TO 4300-NEXT.
288700
           IF (WS-RESTART-EMPLOYEE-A NOT NUMERIC)
               MOVE ZEROES             TO WS-RESTART-EMPLOYEE
           END-IF.

P69000     IF  (WS-RESTART-EMPLOYEE   NOT = ZEROES)            
P69000     AND (WS-REST-REC-NOTFOUND)
P69000         IF (EMP-EMPLOYEE <= WS-RESTART-EMPLOYEE)
P69000             GO TO 4300-NEXT
P69000         ELSE
P69000             SET WS-REST-REC-FOUND TO TRUE
P69000         END-IF.
P69000
           MOVE EMP-COMPANY            TO DB-COMPANY.
           MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
           PERFORM 840-FIND-PEMSET1.

287000     IF  (PCT-LOCAT-CODE NOT = SPACES)
287100     AND (PCT-LOCAT-CODE NOT = PEM-LOCAT-CODE)
287200         GO TO 4300-NEXT.
287300
           MOVE EMP-LAST-NAME          TO HRWS-LAST-NAME.
           MOVE EMP-FIRST-NAME         TO HRWS-FIRST-NAME.
           MOVE EMP-MIDDLE-INIT        TO HRWS-MIDDLE-INIT.
           MOVE EMP-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE.
           MOVE EMP-NAME-SUFFIX        TO HRWS-NAME-SUFFIX.
           PERFORM 750-HR-FORMAT-NAME.

282200     INITIALIZE HREMP-SCR-FIELDS.
282300     INITIALIZE HRPEM-SCR-FIELDS.
282400     MOVE PCT-COMPANY            TO HREMP-COMPANY.
282500     MOVE 1                      TO HREMP-COMPANY-FN.
282600     MOVE 1                      TO HREMP-EMPLOYEE-FN.
           MOVE EMP-EMPLOYEE           TO HREMP-EMPLOYEE.
           MOVE "I"                    TO HREMP-FC.
284100
284200     MOVE 1                      TO HREMP-POS-LEVEL.
284300     PERFORM 2000-HREMP-EDIT-TRAN.
           MOVE HREMP-CURRENCY-CODE    TO PAPCT-EMP-CURRENCY.
           MOVE HREMP-CURR-ND          TO PAPCT-EMP-CURR-ND.
284400
284500     IF (HREMP-UPDPEP-DATE = PCT-EFFECT-DATE)
284600         INITIALIZE                 HREMP-UPDPEP-DATE.
284700
285100     IF (ERROR-FOUND)
285200         GO TO 4300-END.
285500
285600     MOVE "PA100"                TO CRT-ERROR-CAT.
285700
           SET RECORD-FOUND TO TRUE.
288900
289000     PERFORM 4400-WRITE-RECORD.
289100
289200 4300-NEXT.
289300
           IF  (PCT-EMPLOYEE = ZEROES)
               PERFORM 8600-FIND-NEXT-EMPLOYEE.
289900
290000 4300-END.
290100
290200*****************************************************************
290300 4400-WRITE-RECORD               SECTION.
290400*****************************************************************
290500 4400-START.
290600
290700     PERFORM 4500-CALCULATE-NEW-RATE.
          
290800     IF (WS-NEW-PAY-RATE < ZEROES)
               MOVE EMP-EMPLOYEE             TO REG6-EMPLOYEE
               MOVE HRWS-FORMAT-NAME         TO REG6-EMP-NAME
               MOVE 330                      TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE              TO REG6-ERROR-DESC
               MOVE PCT-ACTION-CODE          TO REG6-PCT-ACTION-CODE
               MOVE PCT-ACTION-NBR           TO REG6-PCT-ACTION-NBR
               MOVE PCT-POS-LEVEL            TO REG6-PCT-POS-LEVEL
               MOVE PCT-EFFECT-DATE          TO REG6-PCT-EFFECT-DATE
               MOVE PCT-ACTION-TYPE          TO REG6-ACTION-TYPE
               MOVE ERROR-LINE               TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
               SET ERRORS-PRINTED TO TRUE
           ELSE
290900         MOVE EMP-EMPLOYEE           TO WF-EMPLOYEE
291000         MOVE EMP-PROCESS-LEVEL      TO WF-PROCESS-LEVEL
291100         MOVE EMP-DEPARTMENT         TO WF-DEPARTMENT
291200         MOVE EMP-USER-LEVEL         TO WF-USER-LEVEL
291300         MOVE EMP-LAST-NAME          TO WF-LAST-NAME
291400         MOVE EMP-FIRST-NAME         TO WF-FIRST-NAME
291500         MOVE EMP-MIDDLE-INIT        TO WF-MIDDLE-INIT
291600         MOVE EMP-JOB-CODE           TO WF-JOB-CODE
291700         MOVE EMP-UNION-CODE         TO WF-UNION-CODE
291800         MOVE EMP-NBR-FTE            TO WF-NBR-FTE
291900         MOVE EMP-SALARY-CLASS       TO WF-SALARY-CLASS
292000         MOVE EMP-ANNUAL-HOURS       TO WF-ANNUAL-HOURS
292100         MOVE EMP-PAY-RATE           TO WF-OLD-PAY-RATE
292200         MOVE WS-NEW-PAY-RATE        TO WF-NEW-PAY-RATE
292300         MOVE PCT-ACTION-CODE        TO WF-ACTION-CODE
292400
292500         WRITE WORK-REC              FROM WF-WORK-REC.
292600
292700 4400-END.
292800
292900*****************************************************************
293000 4500-CALCULATE-NEW-RATE         SECTION.
293100*****************************************************************
293200 4500-START.
293300
293400     MOVE ZEROS                  TO WS-AMOUNT.
293500
293600     IF (PCT-PCT-PAY-INC NOT = ZEROS)
293700         COMPUTE WS-PERCENT      = (PCT-PCT-PAY-INC / 100)
293800         COMPUTE WS-AMOUNT       = (EMP-PAY-RATE * WS-PERCENT)
293900         COMPUTE WS-NEW-PAY-RATE = (EMP-PAY-RATE + WS-AMOUNT)
294000         PERFORM 4510-ROUNDING.
294100
294200     IF (PCT-PCT-PAY-DEC NOT = ZEROS)
294300         COMPUTE WS-PERCENT      = (PCT-PCT-PAY-DEC / 100)
294400         COMPUTE WS-AMOUNT       = (EMP-PAY-RATE * WS-PERCENT)
294500         COMPUTE WS-NEW-PAY-RATE = (EMP-PAY-RATE - WS-AMOUNT)
294600         PERFORM 4510-ROUNDING.
294700
294800     IF (PCT-FLAT-INC NOT = ZEROS)
294900         COMPUTE WS-NEW-PAY-RATE = (EMP-PAY-RATE + PCT-FLAT-INC).
295000
295100     IF (PCT-FLAT-DEC NOT = ZEROS)
295200         COMPUTE WS-NEW-PAY-RATE = (EMP-PAY-RATE - PCT-FLAT-DEC).
295300
295400     IF (PCT-NEW-RATE NOT = ZEROS)
295500         MOVE PCT-NEW-RATE       TO WS-NEW-PAY-RATE.
295600
295700 4500-END.
295800
295900*****************************************************************
296000 4510-ROUNDING                   SECTION.
296100*****************************************************************
296200 4510-START.
296300*
296400***
296500*** IF ROUNDING OPTION IS:
296600***    SPACES - THEN ROUNDED TO NEAREST TENTH CENT FOR HOURLY
296700***             NEAREST DOLLAR FOR SALARY EMPLOYEE.
296800***    R      - THEN NORMAL ROUNDING DEPENDING WHERE THEY SET
296900***             SET THE ROUNDING TO OCCUR.
297000***    H      - THE AMOUNT WILL BE ROUNDED TO THE NEXT HIGHER AMT
297100***             INDICATED BY THE ROUND TO EVEN FIELD.
297200***    N      - NO ROUNDING WILL TAKE PLACE THE PAY RATE WILL BE
297300***             TRUNCATED AFTER THE CENTS FIELDS.
297400***
297500*
297600*
297700     IF (PCT-ROUND-OPT         = SPACES)
297800         IF (EMP-SALARY-CLASS  = "H")
297900             IF (WS-TENTH-CENT NOT < 5)
298000                 ADD .01         TO WS-NEW-PAY-RATE
298100                 MOVE ZEROS      TO WS-TENTH-CENT
298200                 MOVE ZEROS      TO WS-HUNDREDTH-CENT
298300             ELSE
298400                 MOVE ZEROS      TO WS-TENTH-CENT
298500                 MOVE ZEROS      TO WS-HUNDREDTH-CENT
298600             END-IF
298700         ELSE
298800             IF (WS-CENT-A NOT < 5)
298900                 ADD 1           TO WS-NEW-PAY-RATE
299000                 MOVE ZEROS      TO WS-CENT-A
299100                                    WS-CENT-B
299200                                    WS-TENTH-CENT
299300                                    WS-HUNDREDTH-CENT
299400             ELSE
299500                 MOVE ZEROS      TO WS-CENT-A
299600                                    WS-CENT-B
299700                                    WS-TENTH-CENT
299800                                    WS-HUNDREDTH-CENT.
299900
300000     IF (PCT-ROUND-OPT = SPACES)
300100         GO TO 4510-END.
300200
300300     IF (PCT-ROUND-EVEN = ZEROS)
300400         GO TO 4510-END
300500     ELSE
300600         MOVE PCT-ROUND-EVEN     TO WS-ROUND-EVEN.
300700
300800     IF (PCT-ROUND-OPT = "R")
300900         PERFORM 4520-ROUNDING-R-OPTION.
301000
301100     IF (PCT-ROUND-OPT = "H")
301200         PERFORM 4530-ROUNDING-H-OPTION.
301300
301400     IF (PCT-ROUND-OPT = "N")
301500         PERFORM 4540-ROUNDING-N-OPTION.
301600
301700 4510-END.
301800
301900*****************************************************************
302000 4520-ROUNDING-R-OPTION          SECTION.
302100*****************************************************************
302200 4520-START.
302300***
302400*** THIS SECTION WILL DO THE ROUNDING WHEN ROUNDING OPTION IS AN
302500*** 'R'.
302600***
302700*** THIS SPECIAL CONDITION IS HERE IN CASE THEY TRY TO ROUND A
302800*** SALARIED PERSON TO A FIELD LESS THAN ONES.
302900***
303000*
303100     IF (WS-RND-CENT-A         NOT = ZEROS)
303200     OR (WS-RND-CENT-B         NOT = ZEROS)
303300     OR (WS-RND-TENTH-CENT     NOT = ZEROS)
303400     OR (WS-RND-HUNDREDTH-CENT NOT = ZEROS)
303500         IF (EMP-SALARY-CLASS  = "S")
303600             COMPUTE WS-NODECIMAL-AMT ROUNDED = WS-NEW-PAY-RATE
303700             COMPUTE WS-NEW-PAY-RATE          = WS-NODECIMAL-AMT
303800             GO TO 4520-END.
303900
304000     COMPUTE WS-NODECIMAL-AMT    ROUNDED
304100                                 = (WS-NEW-PAY-RATE
304200                                 /  WS-ROUND-EVEN).
304300     COMPUTE WS-DECIMAL-AMT      = (WS-NODECIMAL-AMT
304400                                 *  WS-ROUND-EVEN).
304500
304600     MOVE WS-DECIMAL-AMT         TO WS-NEW-PAY-RATE.
304700
304800 4520-END.
304900
305000*****************************************************************
305100 4530-ROUNDING-H-OPTION          SECTION.
305200*****************************************************************
305300 4530-START.
305400***
305500*** THIS SECTION WILL DO THE ROUNDING WHEN ROUNDING OPTION IS AN
305600*** 'R'.
305700***
305800
305900     COMPUTE WS-AMT1             = (WS-NEW-PAY-RATE * 10000).
306000
306100     COMPUTE WS-NODECIMAL-AMT    = (WS-NEW-PAY-RATE
306200                                 /  PCT-ROUND-EVEN).
306300
306400     COMPUTE WS-AMT4             = (WS-NODECIMAL-AMT
306500                                 *  PCT-ROUND-EVEN).
306600
306700     COMPUTE WS-AMT2             = (WS-AMT4 * 10000).
306800
306900     COMPUTE WS-AMT3             = (WS-AMT1 - WS-AMT2).
307000
307100     IF (WS-AMT3 NOT = ZEROS)
307200         COMPUTE WS-NEW-PAY-RATE = (WS-AMT4 + PCT-ROUND-EVEN).
307300
307400 4530-END.
307500
307600*****************************************************************
307700 4540-ROUNDING-N-OPTION          SECTION.
307800*****************************************************************
307900 4540-START.
308000***
308100*** THIS SECTION WILL DO THE TRUNCATING WHEN ROUNDING OPTION IS
308200*** AN 'N'.
308300***
308400
308500     COMPUTE WS-NODECIMAL-AMT    = (WS-NEW-PAY-RATE
308600                                 /  PCT-ROUND-EVEN).
308700
308800     COMPUTE WS-NEW-PAY-RATE     = (WS-NODECIMAL-AMT
308900                                 *  PCT-ROUND-EVEN).
309000
309100 4540-END.
309200*****************************************************************
309300 4600-PRINT-REPORT               SECTION.
309400*****************************************************************
309500 4600-START.
309600
309700     MOVE PRM-COMPANY            TO DB-COMPANY.
309800
309900     READ WORK-FILE              INTO WF-WORK-REC
310000         AT END
310100             GO TO 4600-END.
310200
310300     PERFORM 4855-BANNER-PAGE.
310400     MOVE ZEROS                  TO WS-COMPANY-COUNT
310500                                    WS-CP-CURRENT-GROSS
310600                                    WS-CP-PROJECTED-GROSS.
           IF (PRM-UPDATE-OPTION = "Y")
               MOVE ZEROES             TO PA100WS-SEQ-COUNTER
                                          PA100WS-LN-NBR
               MOVE ZEROES             TO DB-EMP-APP
               MOVE "PA"               TO DB-CMT-TYPE
               MOVE WS-SV-ACTION-CODE  TO DB-ACTION-CODE
               MOVE WS-SV-EFFECT-DATE  TO DB-DATE
               MOVE ZEROES             TO DB-EMPLOYEE
               MOVE SPACES             TO DB-JOB-CODE
               MOVE WS-SV-ACTION-NBR   TO DB-LN-NBR
                                          PA100WS-LN-NBR
               MOVE PACSET2-LN-NBR     TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PACSET2
               IF (PACOMMENTS-FOUND)
                   PERFORM
                       UNTIL (PACOMMENTS-NOTFOUND)
                           COMPUTE PA100WS-SEQ-COUNTER =
                               (PA100WS-SEQ-COUNTER + 1)
                           PERFORM 860-FIND-NXTRNG-PACSET2
                   END-PERFORM
                   PERFORM 910-AUDIT-BEGIN
                   MOVE PRM-COMPANY       TO DB-COMPANY
                   MOVE WS-SV-ACTION-CODE TO DB-ACTION-CODE
                   PERFORM 840-MODIFY-PATSET1
                   MOVE PAT-LAST-CMT-SEQ  TO PA100WS-CMT-SEQ-NBR
                   COMPUTE PAT-LAST-CMT-SEQ =
                       (PAT-LAST-CMT-SEQ + PA100WS-SEQ-COUNTER)
                   PERFORM 820-STORE-PERSACTYPE
                   PERFORM 925-AUDIT-END
               END-IF.

P58564     IF (PRM-UPDATE-OPTION = "Y")
P58564         PERFORM 910-AUDIT-BEGIN.
P58564
310700     PERFORM 4610-PROCESS-COMPANY
310800         UNTIL (END-OF-FILE).
310900
311000     PERFORM 4830-PRINT-COMPANY-TOTALS.
311100     IF (PRM-UPDATE-OPTION  = "Y")
P58564         PERFORM 840-MODIFY-CKPSET1
P69000         MOVE CKP-RESTART-INFO   TO WS-RESTART-INFO
P58564         MOVE WS-RECORD-COUNT    TO WS-RESTART-RECORD
P58564         MOVE WS-RESTART-INFO    TO CKP-RESTART-INFO
P58564         PERFORM 820-STORE-CKPOINT
P58564         MOVE ZEROES             TO WS-MAX-OPS-COUNT  
P58564         PERFORM 900-SAVE-PRINT-FILES
311300         MOVE PRM-COMPANY            TO DB-COMPANY
311400         MOVE "P"                    TO DB-ACTION-TYPE
311500         MOVE WS-SV-EFFECT-DATE      TO DB-EFFECT-DATE
311600         MOVE WS-SV-ACTION-CODE      TO DB-ACTION-CODE
               INITIALIZE                     DB-EMPLOYEE
311700         MOVE WS-SV-ACTION-NBR       TO DB-ACTION-NBR
311800         MOVE PCTSET1-ACTION-NBR     TO WS-DB-BEG-RNG
311900         PERFORM 850-MODIFY-BEGRNG-PCTSET1
312000         PERFORM
312100             UNTIL (PERSACTION-NOTFOUND)
312200
312300             MOVE 0                      TO DB-EMP-APP
312400             MOVE "PA"                   TO DB-CMT-TYPE
312500             MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE
312600             MOVE PCT-EFFECT-DATE        TO DB-DATE
312700             MOVE ZEROS                  TO DB-EMPLOYEE
312800             MOVE SPACES                 TO DB-JOB-CODE
312900             MOVE PCT-ACTION-NBR         TO DB-LN-NBR
313000             MOVE PACSET2-LN-NBR         TO WS-DB-BEG-RNG
313100             PERFORM 830-DELETERNG-PACSET2
313200             PERFORM 830-DELETE-PERSACTION
313300             PERFORM 860-MODIFY-NXTRNG-PCTSET1
313400         END-PERFORM
313500         PERFORM 925-AUDIT-END.
313600
313700 4600-END.
313800*****************************************************************
313900 4610-PROCESS-COMPANY            SECTION.
314000*****************************************************************
314100 4610-START.
314200
314300     MOVE ZEROS                  TO WS-PROCESS-LEVEL-COUNT
314400                                    WS-PL-CURRENT-GROSS
314500                                    WS-PL-PROJECTED-GROSS.
314600     MOVE WF-PROCESS-LEVEL       TO PA100-SV-PROCESS-LEVEL.
314700
314800     PERFORM 4620-PROCESS-PROCESS-LEVEL
314900         UNTIL (END-OF-FILE)
315000         OR    (WF-PROCESS-LEVEL NOT = PA100-SV-PROCESS-LEVEL).
315100
315200     IF (PA100-SV-PROCESS-LEVEL    NOT = WF-PROCESS-LEVEL)
315300     OR (END-OF-FILE)
315400         PERFORM 4820-PRINT-PROC-LEVEL-TOTALS.
315500
315600 4610-END.
315700*****************************************************************
315800 4620-PROCESS-PROCESS-LEVEL      SECTION.
315900*****************************************************************
316000 4620-START.
316100
316200     MOVE ZEROS                  TO WS-DEPARTMENT-COUNT
316300                                    WS-DP-CURRENT-GROSS
316400                                    WS-DP-PROJECTED-GROSS.
316500     MOVE WF-DEPARTMENT          TO WS-SV-DEPARTMENT.
316600
316700     PERFORM 4630-PROCESS-DEPARTMENT
316800         UNTIL (END-OF-FILE)
316900         OR    (WF-PROCESS-LEVEL NOT = PA100-SV-PROCESS-LEVEL)
317000         OR    (WF-DEPARTMENT    NOT = WS-SV-DEPARTMENT).
317100
317200     IF (WS-SV-DEPARTMENT    NOT = WF-DEPARTMENT)
317300     OR (PA100-SV-PROCESS-LEVEL NOT = WF-PROCESS-LEVEL)
317400     OR (END-OF-FILE)
317500         PERFORM 4810-PRINT-DEPARTMENT-TOTALS.
317600
317700 4620-END.
317800*****************************************************************
317900 4630-PROCESS-DEPARTMENT         SECTION.
318000*****************************************************************
318100 4630-START.
318200
318300     MOVE ZEROS                  TO WS-USER-LEVEL-COUNT
318400                                    WS-UL-CURRENT-GROSS
318500                                    WS-UL-PROJECTED-GROSS.
318600     MOVE WF-USER-LEVEL          TO WS-SV-USER-LEVEL.
318700     PERFORM 4850-PAGE-HEADING.
318800     PERFORM 4840-PROCESS-LEVEL-HEADING.
318900
           SET TOTAL-HEADING-NOT-PRINTED TO TRUE.
319100
319200     PERFORM 4640-PROCESS-EMPLOYEE
319300         UNTIL (END-OF-FILE)
319400         OR    (WF-PROCESS-LEVEL NOT = PA100-SV-PROCESS-LEVEL)
319500         OR    (WF-DEPARTMENT    NOT = WS-SV-DEPARTMENT)
319600         OR    (WF-USER-LEVEL    NOT = WS-SV-USER-LEVEL).
319700
319800     IF (WS-SV-USER-LEVEL    NOT = WF-USER-LEVEL)
319900     OR (WS-SV-DEPARTMENT    NOT = WF-DEPARTMENT)
320000     OR (PA100-SV-PROCESS-LEVEL NOT = WF-PROCESS-LEVEL)
320100     OR (END-OF-FILE)
320200         PERFORM 4800-PRINT-USER-LEVEL-TOTALS.
320300
320400 4630-END.
320500*****************************************************************
320600 4640-PROCESS-EMPLOYEE           SECTION.
320700*****************************************************************
320800 4640-START.
320900
321000     MOVE PRM-COMPANY            TO DB-COMPANY.
321100     MOVE WF-EMPLOYEE            TO R3G4-EMPLOYEE
321200                                    DB-EMPLOYEE.
321300     MOVE ZEROS                  TO DB-SEQ-NBR.
321400
321500     MOVE WF-LAST-NAME           TO HRWS-LAST-NAME.
321600     MOVE WF-FIRST-NAME          TO HRWS-FIRST-NAME.
321700     MOVE WF-MIDDLE-INIT         TO HRWS-MIDDLE-INIT.
321800     PERFORM 750-HR-FORMAT-NAME.
321900     MOVE HRWS-FORMAT-NAME       TO R3G4-EMP-NAME.
322000     MOVE WF-JOB-CODE            TO R3G4-JOB-CODE.
322100     MOVE WF-UNION-CODE          TO R3G4-UNION-CODE.
322200     MOVE WF-OLD-PAY-RATE        TO R3G4-OLD-PAY-RATE.
322300     MOVE WF-NEW-PAY-RATE        TO R3G4-NEW-PAY-RATE.
322400
322500     MOVE WF-ANNUAL-HOURS        TO WS-ANNUAL-HOURS.
322600
322700     IF  (WF-SALARY-CLASS  = "H")
322800     AND (WF-JOB-CODE      NOT = SPACES)
322900     AND (WS-ANNUAL-HOURS     = ZEROES)
323000         MOVE WF-JOB-CODE          TO DB-JOB-CODE
323100         PERFORM 840-FIND-JBCSET1
323200         IF  (JOBCODE-FOUND)
323300         AND (JBC-ANNUAL-HOURS NOT = ZEROS)
323400             MOVE JBC-ANNUAL-HOURS TO WS-ANNUAL-HOURS.
323500
323600     IF  (WF-SALARY-CLASS     = "H")
323700     AND (WS-ANNUAL-HOURS     = ZEROES)
323800         IF (PRS-ANNUAL-HOURS = ZEROS)
323900             MOVE 2080             TO WS-ANNUAL-HOURS
324000         ELSE
324100             MOVE PRS-ANNUAL-HOURS TO WS-ANNUAL-HOURS.
324200
324300     IF (WF-SALARY-CLASS = "H")
324400         COMPUTE WS-NEW-SALARY   = (WF-NBR-FTE *
324500                                    WS-ANNUAL-HOURS) *
324600                                    WF-NEW-PAY-RATE
324700         COMPUTE WS-OLD-SALARY   = (WF-NBR-FTE *
324800                                    WS-ANNUAL-HOURS) *
324900                                    WF-OLD-PAY-RATE.
325000
           IF (WS-RESTART-RECORD-A NOT NUMERIC)
               MOVE ZEROES             TO WS-RESTART-RECORD
           END-IF.
325100     IF  (RESTARTING)
325200     AND (WS-RECORD-COUNT NOT > WS-RESTART-RECORD)
325300         ADD 1               TO WS-RECORD-COUNT
               GO TO 4640-NEXT.

325500     INITIALIZE HREMP-SCR-FIELDS.
325600     INITIALIZE HRPEM-SCR-FIELDS.
325700     MOVE "C"                    TO HREMP-FC.
325800     MOVE PCT-COMPANY            TO HREMP-COMPANY.
325900     MOVE 1                      TO HREMP-COMPANY-FN.
326000     MOVE WF-EMPLOYEE            TO HREMP-EMPLOYEE.
326100     MOVE 1                      TO HREMP-EMPLOYEE-FN.
326200     MOVE WF-NEW-PAY-RATE        TO HREMP-PAY-RATE.
326300     MOVE 1                      TO HREMP-PAY-RATE-FN.
326400     MOVE PCT-EFFECT-DATE        TO HREMP-EFFECT-DATE.
326500     MOVE PCT-ACTION-CODE        TO HREMP-ACTION-CODE.
326600     MOVE PCT-ACTION-NBR         TO HREMP-ACTION-NBR.
326700     MOVE PCT-REASON (1)         TO HREMP-REASON1.
326800     MOVE PCT-REASON (2)         TO HREMP-REASON2.
101470*    MOVE PCT-USER-ID            TO HREMP-USER-ID.
101470     MOVE CRT-USER-NAME          TO HREMP-USER-ID.
327000     MOVE PCT-ANT-END-DATE       TO HREMP-ANT-END-DATE.
327100     MOVE PCT-UPDATE-BENEFIT     TO HREMP-UPDATE-BENEFIT.
           MOVE PCT-UPD-ABS-MGMT       TO HREMP-UPDATE-ABSENCE-MGMT.
327200     MOVE "Y"                    TO HREMP-ACTION.
327300     MOVE "N"                    TO HREMP-UPDATE-EMP-GROUPS.
           INITIALIZE HREMPWS-MASS.
           SET HREMP-MASS-ACTION TO TRUE.
           MOVE HREMP-PAY-RATE-DN      TO HREMPWS-MASS-FLD-NBR (1).
327400     MOVE ZEROS                  TO CRT-ERROR-NBR.
327500     INITIALIZE                     PAPEP-UPDPEP-DATE.
           SET PAPEP-FROM-ACTION TO TRUE.
327700     MOVE PCT-COMPANY            TO PADT-COMPANY.
327800     MOVE HREMP-EMPLOYEE         TO PADT-EMPLOYEE.
327900     MOVE HREMP-TERM-DATE        TO PADT-END-DATE .
328000     MOVE 1                      TO PADT-POS-LEVEL.
328100     MOVE PCT-EFFECT-DATE        TO PADT-EFFECT-DATE.
328200     PERFORM 2300-PADT-DATE-CHECK.
328300     MOVE ZEROS                  TO CRT-ERROR-NBR.
328400     MOVE PADT-UPDPEP-DATE       TO HREMP-UPDPEP-DATE.
328500     MOVE 1                      TO HREMP-POS-LEVEL.
328600     PERFORM 2000-HREMP-EDIT-TRAN.
           MOVE HREMP-LAST-NAME        TO HRWS-LAST-NAME.
           MOVE HREMP-FIRST-NAME       TO HRWS-FIRST-NAME.
           MOVE HREMP-MIDDLE-INIT      TO HRWS-MIDDLE-INIT.
           MOVE HREMP-LAST-NAME-PRE    TO HRWS-LAST-NAME-PRE.
           MOVE HREMP-NAME-SUFFIX      TO HRWS-NAME-SUFFIX.
           PERFORM 750-HR-FORMAT-NAME.
           MOVE HREMP-CURRENCY-CODE    TO PAPCT-EMP-CURRENCY.
           MOVE HREMP-CURR-ND          TO PAPCT-EMP-CURR-ND.

           IF  (HREMP-ERROR-FOUND)
           AND (PRM-ERROR-OPTION  = 1)
           AND (PRM-UPDATE-OPTION = "Y")
               PERFORM 5200-CREATE-PERSACTION
               MOVE PCT-ACTION-CODE        TO WS-ERR-ACTION-CODE
               MOVE PCT-ACTION-NBR         TO WS-ERR-ACTION-NBR
               MOVE PCT-ACTION-TYPE        TO WS-ERR-ACTION-TYPE
               MOVE 1                      TO WS-ERR-POS-LEVEL
               MOVE PCT-EFFECT-DATE        TO WS-ERR-EFFECT-DATE
           ELSE
               MOVE PCT-ACTION-CODE        TO WS-ERR-ACTION-CODE
               MOVE PCT-ACTION-NBR         TO WS-ERR-ACTION-NBR
               MOVE PCT-ACTION-TYPE        TO WS-ERR-ACTION-TYPE
               MOVE 1                      TO WS-ERR-POS-LEVEL
               MOVE PCT-EFFECT-DATE        TO WS-ERR-EFFECT-DATE
           END-IF.
           PERFORM 6000-PRINT-MESSAGES.

           IF  (HREMP-ERROR-FOUND)
P69900     AND (PRM-UPDATE-OPTION = "Y")
P69000         ADD 1 TO WS-MAX-OPS-COUNT
P69000         IF (WS-MAX-OPS-COUNT > WS-MAX-OPS-IN-TRAN)
P69000             PERFORM 840-MODIFY-CKPSET1
                   MOVE CKP-RESTART-INFO   TO WS-RESTART-INFO
P69000             MOVE HREMP-EMPLOYEE     TO WS-RESTART-EMPLOYEE
P69000             MOVE WS-RESTART-INFO    TO CKP-RESTART-INFO
P69000             PERFORM 820-STORE-CKPOINT
P69000             MOVE ZEROES             TO WS-MAX-OPS-COUNT
P69000             PERFORM 900-SAVE-PRINT-FILES
P69000             PERFORM 925-AUDIT-END
376900             PERFORM 910-AUDIT-BEGIN
               END-IF
               GO TO 4640-NEXT
           END-IF.

328700     IF (HREMP-UPDPEP-DATE = PCT-EFFECT-DATE)
328800         INITIALIZE              HREMP-UPDPEP-DATE
328900     END-IF.

329200     IF (WF-SALARY-CLASS NOT = "H")
329300         ADD WF-OLD-PAY-RATE     TO WS-UL-CURRENT-GROSS
329400                                    WS-DP-CURRENT-GROSS
329500                                    WS-PL-CURRENT-GROSS
329600                                    WS-CP-CURRENT-GROSS
329700         ADD WF-NEW-PAY-RATE     TO WS-UL-PROJECTED-GROSS
329800                                    WS-DP-PROJECTED-GROSS
329900                                    WS-PL-PROJECTED-GROSS
330000                                    WS-CP-PROJECTED-GROSS
330100     ELSE
330200         ADD WS-OLD-SALARY       TO WS-UL-CURRENT-GROSS
330300                                    WS-DP-CURRENT-GROSS
330400                                    WS-PL-CURRENT-GROSS
330500                                    WS-CP-CURRENT-GROSS
330600         ADD WS-NEW-SALARY       TO WS-UL-PROJECTED-GROSS
330700                                    WS-DP-PROJECTED-GROSS
330800                                    WS-PL-PROJECTED-GROSS
330900                                    WS-CP-PROJECTED-GROSS
331000     END-IF.
331100     ADD 1                       TO WS-USER-LEVEL-COUNT
331200                                    WS-DEPARTMENT-COUNT
331300                                    WS-PROCESS-LEVEL-COUNT
331400                                    WS-COMPANY-COUNT.
331500     MOVE R3GN4-MASS-PAY-EMPLOYEE    TO RPT-GROUP-REQUEST.
331600     PERFORM 700-PRINT-RPT-GRP.
331700     MOVE "PA100"                TO CRT-ERROR-CAT.

           IF  (PRM-UPDATE-OPTION NOT = "Y")
               IF  (HREMP-IS-RETRO)
               OR  (PCT-PROCESS-TYPE NOT = SPACES)
                   PERFORM 4645-PRINT-RETRO.

           IF  (PRM-UPDATE-OPTION = "Y")
331900         MOVE "PAACT"                TO IFOBIWS-OBJ-TYPE
P58564         IF (PA100WS-OBJ-ID-LEFT = ZEROES)
P58564             MOVE WS-MAX-OPS-IN-TRAN TO IFOBIWS-NBR-OBJECTS
P58564                                        PA100WS-OBJ-ID-LEFT
P58564             PERFORM 7000-ASSIGN-OBJ-ID-70
P84384             PERFORM 925-AUDIT-END
P84384             PERFORM 910-AUDIT-BEGIN
P58564             MOVE IFOBIWS-OBJ-ID     TO PA100WS-MEM-OBJ-ID
P58564             SUBTRACT WS-MAX-OPS-IN-TRAN
P58564                                  FROM PA100WS-MEM-OBJ-ID
P58564         END-IF
P58564         ADD 1                       TO PA100WS-MEM-OBJ-ID
P58564         MOVE PA100WS-MEM-OBJ-ID     TO IFOBIWS-OBJ-ID
P58564         SUBTRACT 1              FROM PA100WS-OBJ-ID-LEFT
P58564         MOVE IFOBIWS-OBJ-ID         TO WS-OBJ-ID
332200         MOVE PCT-COMPANY            TO PAACT-COMPANY
332300         MOVE "E"                    TO PAACT-ACTION-TYPE
332400         MOVE HREMP-EMPLOYEE         TO PAACT-EMPLOYEE
332500         MOVE PCT-ACTION-CODE        TO PAACT-ACTION-CODE
332600         MOVE PCT-EFFECT-DATE        TO PAACT-EFFECT-DATE
332700         MOVE "Y"                    TO PAACT-HISTORY
332800         PERFORM 2300-PAACT-ACTION-NBR
332900         MOVE PAACT-ACTION-NBR       TO PA100WS-ACTION-NBR
333000         MOVE "L"                    TO PAACT-ACTION-TYPE
333100         PERFORM 2300-PAACT-ACTION-NBR
333200         IF (PAACT-ACTION-NBR > PA100WS-ACTION-NBR)
333300             MOVE PAACT-ACTION-NBR
333400                                     TO PA100WS-ACTION-NBR
333500         END-IF
               IF  (HREMP-IS-RETRO)
               OR  (PCT-PROCESS-TYPE NOT = SPACES)
                   PERFORM 4645-PRINT-RETRO
               END-IF

333600         MOVE "P"                    TO DB-ACTION-TYPE
333700         MOVE WS-SV-EFFECT-DATE      TO DB-EFFECT-DATE
333800         MOVE WS-SV-ACTION-CODE      TO DB-ACTION-CODE
333900         MOVE ZEROES                 TO DB-EMPLOYEE
334000         MOVE WS-SV-ACTION-NBR       TO DB-ACTION-NBR
334100         PERFORM 840-FIND-PCTSET1
334200         PERFORM 800-CREATE-PERSACTHST
334300         MOVE PCT-COMPANY        TO PAH-COMPANY
334400         MOVE PCT-ACTION-TYPE    TO PAH-ACTION-TYPE
334800         MOVE PCT-ACTION-CODE    TO PAH-ACTION-CODE
334900         MOVE PA100WS-ACTION-NBR TO PAH-ACTION-NBR
335000         MOVE PCT-EFFECT-DATE    TO PAH-EFFECT-DATE
335100         MOVE HREMP-EMPLOYEE     TO PAH-EMPLOYEE
335200         MOVE PCT-ANT-END-DATE   TO PAH-ANT-END-DATE
335300         MOVE PCT-REASON (1)     TO PAH-REASON (1)
335400         MOVE PCT-REASON (2)     TO PAH-REASON (2)
101470*        MOVE PCT-USER-ID        TO PAH-USER-ID
101470         MOVE CRT-USER-NAME      TO PAH-USER-ID
335600         MOVE WS-SYSTEM-DATE-YMD TO PAH-DATE-STAMP
P80029         MOVE HHMMSS             TO PAH-TIME-STAMP
335700         MOVE WS-OBJ-ID          TO PAH-OBJ-ID
               MOVE PCT-POS-LEVEL      TO PAH-POS-LEVEL
               MOVE PCT-UPDATE-BENEFIT TO PAH-UPDATE-BENEFIT
               MOVE PCT-UPD-ABS-MGMT   TO PAH-UPD-ABS-MGMT
               MOVE PCT-HIST-CORR-FLAG TO PAH-HIST-CORR-FLAG
               IF (WSPCT-ACTION-TYPE = "A" OR "E" OR "L")
                   MOVE PCT-UPDATE-REQ-DED
                                       TO PAH-UPDATE-REQ-DED
                   MOVE PCT-EDM-EFFECT-DT
                                       TO PAH-EDM-EFFECT-DT
                   MOVE PCT-EDM-END-DATE
                                       TO PAH-EDM-END-DATE
                   MOVE "2"            TO PAH-ACTION-UPD
               END-IF
               IF (PCT-ACTION-TYPE = "M" OR "P")
                   MOVE "3"            TO PAH-ACTION-UPD
               END-IF
J08104         IF  (PCT-CREATE-USER-ID NOT = SPACES)
J08104             MOVE PCT-CREATE-USER-ID TO PAH-CREATE-USER
J08104         ELSE
J08104             MOVE CRT-USER-NAME  TO PAH-CREATE-USER
J08104         END-IF
J08104         MOVE PCT-CREATE-DATE    TO PAH-CREATE-DATE
J08104         MOVE PCT-CREATE-TIME    TO PAH-CREATE-TIME
335800         PERFORM 820-STORE-PERSACTHST
336800         MOVE WS-OBJ-ID          TO HREMP-ACT-OBJ-ID
336900         INITIALIZE                 PAPEP-UPDPEP-DATE
               SET PAPEP-FROM-ACTION TO TRUE
337100         MOVE PCT-COMPANY        TO PADT-COMPANY
337200         MOVE HREMP-EMPLOYEE     TO PADT-EMPLOYEE
337300         MOVE HREMP-TERM-DATE    TO PADT-END-DATE
337400         MOVE 1                  TO PADT-POS-LEVEL
337500         MOVE PCT-EFFECT-DATE    TO PADT-EFFECT-DATE
337600         PERFORM 2300-PADT-DATE-CHECK
337700         MOVE ZEROS              TO CRT-ERROR-NBR
337800         MOVE PADT-UPDPEP-DATE   TO HREMP-UPDPEP-DATE
               MOVE "N"                TO HRWS-ADD
337900         PERFORM 3000-HREMP-PROCESS-TRAN

               MOVE PA100WS-CMT-SEQ-NBR
                                       TO PA100WS-SEQ-COUNTER
               MOVE PCT-COMPANY        TO WS-WRK-COMPANY
               MOVE EMP-EMPLOYEE       TO WS-WRK-EMPLOYEE
               MOVE PCT-EFFECT-DATE    TO WS-WRK-EFFECT-DATE
               MOVE PCT-ACTION-CODE    TO WS-WRK-ACTION-CODE
               MOVE PCT-ACTION-NBR     TO WS-WRK-ACTION-NBR
               MOVE HREMP-PAY-RATE-DN  TO WS-WRK-FLD-NBR
               MOVE HREMP-PAY-RATE     TO WS-PAY-RATE
               MOVE WS-PAY-RATE-R      TO WS-PAY-RATE-NV
               MOVE WS-PAY-NEW-VALUE   TO WS-WRK-NEW-VALUE
               MOVE WS-OBJ-ID          TO WS-WRK-ACT-OBJ-ID
               MOVE PCT-UPDATE-BENEFIT TO WS-WRK-UPDATE-BENEFIT
               MOVE PCT-UPD-ABS-MGMT   TO WS-WRK-UPD-ABS-MGMT
               PERFORM 5400-ADD-GRP-FLD

338000         IF (HREMP-UPDPEP-DATE = PCT-EFFECT-DATE)
338100             INITIALIZE             HREMP-UPDPEP-DATE
338200         END-IF
338300         MOVE PCT-ACTION-CODE    TO DB-ACTION-CODE
P58564         PERFORM 840-FIND-PATSET1
338500         MOVE PCT-COMPANY        TO DB-COMPANY
338600         MOVE 0                  TO DB-EMP-APP
338700         MOVE "PA"               TO DB-CMT-TYPE
338800         MOVE PCT-ACTION-CODE    TO DB-ACTION-CODE
338900         MOVE PCT-EFFECT-DATE    TO DB-DATE
339000         MOVE ZEROS              TO DB-EMPLOYEE
339100         MOVE SPACES             TO DB-JOB-CODE
339200         MOVE PCT-ACTION-NBR     TO DB-LN-NBR
339300         MOVE PACSET2-LN-NBR     TO WS-DB-BEG-RNG
339400         PERFORM 850-FIND-BEGRNG-PACSET2
339500         PERFORM
339600             UNTIL (PACOMMENTS-NOTFOUND)
339700
339800             PERFORM 810-RECREATE-PACOMMENTS
339900             MOVE WF-EMPLOYEE      TO PAC-EMPLOYEE
340000             ADD 1                 TO PA100WS-SEQ-COUNTER
340100             MOVE PA100WS-SEQ-COUNTER
340100                                   TO PAC-SEQ-NBR
                   MOVE PA100WS-ACTION-NBR
                                         TO PAC-LN-NBR
340200             PERFORM 820-STORE-PACOMMENTS
340300             PERFORM 860-FIND-NXTRNG-PACSET2
340400         END-PERFORM
340600         ADD 1                   TO WS-RECORD-COUNT
340700         PERFORM 4900-UPDATE-EXP-RESTART-INFO.
340900
       4640-NEXT.
343000
343100     READ WORK-FILE              INTO WF-WORK-REC
343200         AT END
343300              SET END-OF-FILE TO TRUE.
343400
343500 4640-END.
343600*****************************************************************
343700 4645-PRINT-RETRO                SECTION.
343800*****************************************************************
343900 4645-START.

           MOVE EMP-EMPLOYEE           TO R4G7-PCT-EMPLOYEE.
           MOVE HRWS-FORMAT-NAME       TO R4G7-EMP-NAME.
           MOVE PCT-EFFECT-DATE        TO R4G7-PCT-EFFECT-DATE.
           MOVE PCT-ACTION-CODE        TO R4G7-PCT-ACTION-CODE.
           IF  (PRM-UPDATE-OPTION NOT = "Y")
               INITIALIZE                 R4G7-PCT-ACTION-NBR
           ELSE
               MOVE PA100WS-ACTION-NBR TO R4G7-PCT-ACTION-NBR
           END-IF.
           MOVE 1                      TO R4G7-PCT-POS-LEVEL.
           INITIALIZE                     R4G7-COMBINE-ACTION-NBR.
           INITIALIZE                     R4G7-REVERSE-ACTION-NBR.
           MOVE PCT-HIST-CORR-FLAG     TO R4G7-PCT-HIST-CORR-FLAG.
           INITIALIZE                     R4G7-RUN-PA113.
           SET RETRO-PCT-NOT-PRINTED TO TRUE.
           IF  (HREMP-RETRO-PENDING)
               MOVE "Y"                TO R4G7-RUN-PA113
           END-IF.
           MOVE HREMP-PAY-RATE-DN      TO DB-FLD-NBR.
           IF  (HREMP-FLD-RETRO (DB-FLD-NBR))
               INITIALIZE                     R4G7-RETRO-FIELD
                                              R4G8-RETRO-FIELD
               PERFORM 840-FIND-PADSET1
               IF  (PADICT-FOUND)
                   MOVE PAD-ITEM-NAME      TO CRT-PHRASE
                   MOVE WS-PHRASE-SIZE     TO CRT-PHRASE-SIZE
                   MOVE "N"                TO CRT-PUT-DOTS
                   PERFORM 900-GET-PHRASE-XLT
                   MOVE CRT-PHRASE-XLT     TO R4G7-RETRO-FIELD
                                              R4G8-RETRO-FIELD
               END-IF
               IF  (RETRO-PCT-NOT-PRINTED)
                   MOVE R4GN7-PCT-ACTION   TO RPT-GROUP-REQUEST
                   SET RETRO-PCT-PRINTED TO TRUE
               ELSE
                   MOVE R4GN8-PCT-FIELD    TO RPT-GROUP-REQUEST
               END-IF
               PERFORM 700-PRINT-RPT-GRP
           END-IF.
           IF  (RETRO-PCT-NOT-PRINTED)
               MOVE R4GN7-PCT-ACTION     TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
           END-IF.
           SET HISTAUDITS-PRINTED TO TRUE.

       4645-END.

343600*****************************************************************
343700 4800-PRINT-USER-LEVEL-TOTALS    SECTION.
343800*****************************************************************
343900 4800-START.
344000
344100     IF  (TOTAL-HEADING-NOT-PRINTED)
344200         MOVE R3GN5H-MASS-PAY-TOTAL  TO RPT-GROUP-REQUEST
344300         PERFORM 700-PRINT-RPT-GRP
               SET TOTAL-HEADING-PRINTED TO TRUE
           END-IF.
344500
344700     MOVE WS-LEVEL-NAME          TO R3G5-LITERAL.
344600     MOVE WS-USER-LEVEL-DESC     TO R3G5-DESCRIPTION.
344800     MOVE WS-SV-USER-LEVEL       TO R3G5-LEVEL.
344900     MOVE WS-USER-LEVEL-COUNT    TO R3G5-COUNT.
345000     MOVE WS-UL-CURRENT-GROSS    TO R3G5-CURRENT-GROSS.
345100     MOVE WS-UL-PROJECTED-GROSS  TO R3G5-PROJECTED-GROSS.
345200     MOVE R3GN5D-MASS-PAY-TOTAL  TO RPT-GROUP-REQUEST.
345300     PERFORM 700-PRINT-RPT-GRP.
345400
345500 4800-END.
345600*****************************************************************
345700 4810-PRINT-DEPARTMENT-TOTALS    SECTION.
345800*****************************************************************
345900 4810-START.
346000
346100     IF  (TOTAL-HEADING-NOT-PRINTED)
346200         MOVE R3GN5H-MASS-PAY-TOTAL  TO RPT-GROUP-REQUEST
346300         PERFORM 700-PRINT-RPT-GRP
               SET TOTAL-HEADING-PRINTED TO TRUE
           END-IF.
346500
346800     MOVE WS-DEPT-LABEL          TO R3G5-LITERAL.
346900     MOVE WS-DEPART-DESC         TO R3G5-DESCRIPTION.
347000     MOVE WS-SV-DEPARTMENT       TO R3G5-LEVEL.
347100     MOVE WS-DEPARTMENT-COUNT    TO R3G5-COUNT.
347200     MOVE WS-DP-CURRENT-GROSS    TO R3G5-CURRENT-GROSS.
347300     MOVE WS-DP-PROJECTED-GROSS  TO R3G5-PROJECTED-GROSS.
347400     MOVE R3GN5D-MASS-PAY-TOTAL  TO RPT-GROUP-REQUEST.
347500     PERFORM 700-PRINT-RPT-GRP.
347600
347700 4810-END.
347800*****************************************************************
347900 4820-PRINT-PROC-LEVEL-TOTALS    SECTION.
348000*****************************************************************
348100 4820-START.
348200
348300     IF  (TOTAL-HEADING-NOT-PRINTED)
348400         MOVE R3GN5H-MASS-PAY-TOTAL  TO RPT-GROUP-REQUEST
348500         PERFORM 700-PRINT-RPT-GRP
               SET TOTAL-HEADING-PRINTED TO TRUE
           END-IF.
348700
349000     MOVE WS-PL-LABEL            TO R3G5-LITERAL.
349100     MOVE WS-PROC-LEV-DESC       TO R3G5-DESCRIPTION.
349200     MOVE PA100-SV-PROCESS-LEVEL TO R3G5-LEVEL.
349300     MOVE WS-PROCESS-LEVEL-COUNT TO R3G5-COUNT.
349400     MOVE WS-PL-CURRENT-GROSS    TO R3G5-CURRENT-GROSS.
349500     MOVE WS-PL-PROJECTED-GROSS  TO R3G5-PROJECTED-GROSS.
349600     MOVE R3GN5D-MASS-PAY-TOTAL  TO RPT-GROUP-REQUEST.
349700     PERFORM 700-PRINT-RPT-GRP.
349800
349900 4820-END.
350000*****************************************************************
350100 4830-PRINT-COMPANY-TOTALS       SECTION.
350200*****************************************************************
350300 4830-START.
350400
350500     IF  (TOTAL-HEADING-NOT-PRINTED)
350600         MOVE R3GN5H-MASS-PAY-TOTAL  TO RPT-GROUP-REQUEST
350700         PERFORM 700-PRINT-RPT-GRP
               SET TOTAL-HEADING-PRINTED TO TRUE
           END-IF.
350900
351000     MOVE WS-COMPANY-COUNT           TO R3G5C-COUNT.
351100     MOVE WS-CP-CURRENT-GROSS        TO R3G5C-CURRENT-GROSS.
351200     MOVE WS-CP-PROJECTED-GROSS      TO R3G5C-PROJECTED-GROSS.
351300     MOVE R3GN5D-MASS-PAY-TOTAL-C    TO RPT-GROUP-REQUEST.
351400     PERFORM 700-PRINT-RPT-GRP.
351500
351600 4830-END.
351700*****************************************************************
351800 4840-PROCESS-LEVEL-HEADING      SECTION.
351900*****************************************************************
352000 4840-START.
352100
352200     MOVE WF-PROCESS-LEVEL       TO DB-PROCESS-LEVEL.
352300     PERFORM 840-FIND-PRSSET1.
352400     MOVE PRS-NAME               TO R3G3-PROCESS-LEVEL-DESC
352500                                    WS-PROC-LEV-DESC.
352600
352700     MOVE WF-DEPARTMENT          TO DB-DEPARTMENT.
352800     PERFORM 840-FIND-DPTSET1.
352900     IF (DEPTCODE-FOUND)
353000         MOVE DPT-NAME           TO R3G3-DEPARTMENT-DESC
353100                                    WS-DEPART-DESC
353200     ELSE
353300         MOVE SPACES             TO R3G3-DEPARTMENT-DESC
353400                                    WS-DEPART-DESC.
353500
353600     MOVE WF-PROCESS-LEVEL       TO R3G3-PROCESS-LEVEL.
353700     MOVE WF-DEPARTMENT          TO R3G3-DEPARTMENT.
353800     MOVE WF-USER-LEVEL          TO R3G3-USER-LEVEL
353900                                    DB-CODE.
354000
354200     MOVE "UL"                   TO DB-TYPE.
354300     PERFORM 840-FIND-PCOSET1.
354400     IF (PCODES-FOUND)
354500         MOVE PCO-DESCRIPTION    TO R3G3-USER-LEVEL-DESC
354600                                    WS-USER-LEVEL-DESC
354700     ELSE
354800         MOVE SPACES             TO R3G3-USER-LEVEL-DESC
354900                                    WS-USER-LEVEL-DESC.
355000
355100     MOVE R3GN3-MASS-PAY-PROC-LEV TO RPT-GROUP-REQUEST.
355200     PERFORM 700-PRINT-RPT-GRP.
355300
355400 4840-END.
355500*****************************************************************
355600 4850-PAGE-HEADING               SECTION.
355700*****************************************************************
355800 4850-START.
355900
356000     MOVE PRM-COMPANY            TO R3G1-PCT-COMPANY.
356100     MOVE WS-COMPANY-NAME        TO R3G1-PRS-NAME.
356200
357000     MOVE PRM-UPDATE-OPTION      TO R3G1-UPDATE-OPTION.
357100
357200     MOVE R3GN1-MASS-PAY-COMPANY TO RPT-GROUP-REQUEST.
357300     PERFORM 700-PRINT-RPT-GRP.
357400
357500 4850-END.
357600*****************************************************************
357700 4855-BANNER-PAGE                SECTION.
357800*****************************************************************
357900 4855-START.
358000
358100     MOVE PRM-COMPANY            TO R3G1-PCT-COMPANY.
358200     MOVE WS-COMPANY-NAME        TO R3G1-PRS-NAME.
358300
357000     MOVE PRM-UPDATE-OPTION      TO R3G1-UPDATE-OPTION.
359200
359300     MOVE R3GN1-MASS-PAY-COMPANY TO RPT-GROUP-REQUEST.
359400     PERFORM 700-PRINT-RPT-GRP.
359500
359600     MOVE PRM-COMPANY            TO DB-COMPANY.
359700     MOVE "P"                    TO DB-ACTION-TYPE.
359800     MOVE WS-SV-EFFECT-DATE      TO DB-EFFECT-DATE.
359900     MOVE WS-SV-ACTION-CODE      TO DB-ACTION-CODE.
           INITIALIZE                     DB-EMPLOYEE.
           MOVE WS-SV-ACTION-NBR       TO DB-ACTION-NBR.
           MOVE PCTSET1-ACTION-NBR     TO WS-DB-BEG-RNG.
360100     PERFORM 850-FIND-BEGRNG-PCTSET1.
360200
360300     MOVE PCT-UPDATE-BENEFIT     TO R3G2-PCT-UPDATE-BENEFIT.
           MOVE PCT-UPD-ABS-MGMT       TO R3G2-PCT-UPD-ABS-MGMT.
360400     MOVE 1                      TO R3G2-PCT-POS-LEVEL.
360500     MOVE PCT-PROCESS-LEVEL      TO R3G2-PCT-PROC-LEV.
360600     MOVE PCT-DEPARTMENT         TO R3G2-PCT-DEPARTMENT.
360800     MOVE PCT-USER-LEVEL         TO R3G2-PCT-USER-LEVEL.
360900     MOVE PCT-LOCAT-CODE         TO R3G2-PCT-LOCAT-CODE.
361000     MOVE PCT-SUPERVISOR         TO R3G2-PCT-SUPERVISOR.
361100     MOVE PCT-UNION-CODE         TO R3G2-PCT-UNION-CODE.
361200     MOVE PCT-PERS-GROUP         TO R3G2-PCT-PERS-GROUP.
      *    MOVE PCT-USER-ID            TO R3G2-PCT-USER-ID.
101470*    MOVE PCT-USER-ID            TO WS-USER-DBUIDKEY.
101470     MOVE CRT-USER-NAME          TO WS-USER-DBUIDKEY.
J21860     PERFORM 900-GET-USER-DISPLAY-NAME.
J21860     PERFORM 5300-NAME-CHECK.
J21860     MOVE WS-CHECK-NAME-OUTPUT   TO R3G2-PCT-USER-ID.
J08104*    MOVE PCT-CREATE-USER-ID     TO R2G2-PCT-CREATE-USER-ID.
J21860     MOVE PCT-CREATE-USER-ID     TO WS-USER-DBUIDKEY.
J21860     PERFORM 900-GET-USER-DISPLAY-NAME.
J21860     PERFORM 5300-NAME-CHECK.
J21860     MOVE WS-CHECK-NAME-OUTPUT   TO R3G2-PCT-CREATE-USER-ID.
361300     MOVE PCT-ACTION-CODE        TO R3G2-PCT-ACTION-CODE
361400                                    DB-ACTION-CODE.
361500     PERFORM 840-FIND-PATSET1.
361600     IF (PERSACTYPE-FOUND)
361700         MOVE PAT-DESCRIPTION    TO R3G2-ACTION-DESC
361800     ELSE
361900         MOVE SPACES             TO R3G2-ACTION-DESC.
362000
362100     MOVE PCT-EFFECT-DATE        TO R3G2-PCT-EFFECT-DATE.
362200     MOVE PCT-ANT-END-DATE       TO R3G2-PCT-ANT-END-DATE.
362300
362400     IF (PCT-REASON (1) NOT = SPACES)
362500         MOVE PCT-REASON (1)     TO R3G2-PCT-REASON-1
363000     ELSE
363100         MOVE SPACES             TO R3G2-PCT-REASON-1.
363300
363400     IF (PCT-REASON (2) NOT = SPACES)
363500         MOVE PCT-REASON (2)     TO R3G2-PCT-REASON-2
364000     ELSE
364100         MOVE SPACES             TO R3G2-PCT-REASON-2.
364300
364400     MOVE PCT-SALARY-CLASS       TO R3G2-PCT-SAL-CLASS.
364500     MOVE PCT-PCT-PAY-INC        TO R3G2-PCT-PCT-PAY-INC.
364600     MOVE PCT-PCT-PAY-DEC        TO R3G2-PCT-PCT-PAY-DEC.
364700     MOVE PCT-ROUND-EVEN         TO R3G2-PCT-ROUND-EVEN.
364800     MOVE PCT-ROUND-OPT          TO R3G2-PCT-ROUND-OPT.
364900     MOVE PCT-FLAT-INC           TO R3G2-PCT-FLAT-INC.
365000     MOVE PCT-FLAT-DEC           TO R3G2-PCT-FLAT-DEC.
365100     MOVE PCT-NEW-RATE           TO R3G2-PCT-NEW-RATE.
365200
365300     MOVE R3GN2-MASS-PAY-ACTION  TO RPT-GROUP-REQUEST.
365400     PERFORM 700-PRINT-RPT-GRP.
365500
365600     MOVE 0                      TO DB-EMP-APP.
365700     MOVE "PA"                   TO DB-CMT-TYPE.
365800     MOVE PCT-ACTION-CODE        TO DB-ACTION-CODE.
365900     MOVE PCT-EFFECT-DATE        TO DB-DATE.
366000     MOVE ZEROS                  TO DB-EMPLOYEE.
366100     MOVE SPACES                 TO DB-JOB-CODE.
366200     MOVE PCT-ACTION-NBR         TO DB-LN-NBR.
366300     MOVE PACSET2-LN-NBR         TO WS-DB-BEG-RNG.
366400     PERFORM 850-FIND-BEGRNG-PACSET2.
366500     PERFORM 860-FIND-NXTRNG-PACSET2
366600         UNTIL (PACOMMENTS-NOTFOUND)
366700         OR    (PAC-PRINT-CODE  NOT = "N").
366800
366900     IF (PACOMMENTS-FOUND)
367000         MOVE GN6H-PAC-SEQ-NBR   TO RPT-GROUP-REQUEST
367100         PERFORM 700-PRINT-RPT-GRP.
367200
367300     PERFORM 1755-PRINT-COMMENTS
367400         UNTIL (PACOMMENTS-NOTFOUND).
367500
367600 4855-END.
367700
367800*****************************************************************
367900 4900-UPDATE-EXP-RESTART-INFO    SECTION.
368000*****************************************************************
368100 4900-START.
368200
P58564     ADD 1 TO WS-MAX-OPS-COUNT.
P58564     IF (WS-MAX-OPS-COUNT  > WS-MAX-OPS-IN-TRAN)
368500         PERFORM 840-MODIFY-CKPSET1
               MOVE CKP-RESTART-INFO       TO WS-RESTART-INFO
368600         MOVE WS-RECORD-COUNT        TO WS-RESTART-RECORD
P69000         MOVE EMP-EMPLOYEE           TO WS-RESTART-EMPLOYEE
368700         MOVE WS-RESTART-INFO        TO CKP-RESTART-INFO
368800         PERFORM 820-STORE-CKPOINT
               PERFORM 900-SAVE-PRINT-FILES
368900         PERFORM 925-AUDIT-END
369000         PERFORM 910-AUDIT-BEGIN
P58564     END-IF.
369100
369200 4900-END.
369300******************************************************************
369400 5000-DO-WSPCT-USER-FIELDS       SECTION.
369500******************************************************************
369600 5000-START.
369700
369800     PERFORM
369900         VARYING I1 FROM 1 BY 1
370000         UNTIL  (I1 > 36)
370100
370200         IF  (WSPCT-FLD-NBR (I1)   >= 2000)
370300         AND (WSPCT-FLD-NBR (I1)   <= 2099)
370400         AND (WSPCT-NEW-VALUE (I1) NOT = SPACES)
370500             INITIALIZE PAPCT-SCR-FIELDS
370600             MOVE WSPCT-FLD-NBR (I1)   TO PAPCT-FLD-NBR
370700             MOVE WSPCT-NEW-VALUE (I1) TO PAPCT-NEW-VALUE
370800             MOVE WSPCT-COMPANY        TO PAPCT-COMPANY
370900             MOVE HREMP-EMPLOYEE       TO PAPCT-EMPLOYEE
371000             MOVE WSPCT-EFFECT-DATE    TO PAPCT-EFFECT-DATE
371100             MOVE WSPCT-ACTION-CODE    TO PAPCT-ACTION-CODE
371200             MOVE WSPCT-ACTION-NBR     TO PAPCT-ACTION-NBR
371300             MOVE WSPCT-REASON (1)     TO PAPCT-REASON1
371400             MOVE WSPCT-REASON (2)     TO PAPCT-REASON2
371500             MOVE WSPCT-USER-ID        TO PAPCT-USER-ID
371600             MOVE WSPCT-ANT-END-DATE   TO PAPCT-ANT-END-DATE
371700             PERFORM 5100-EDIT-USER-FIELDS
371800             PERFORM 3000-HRHEU-PROCESS-TRAN
372400         END-IF
372500     END-PERFORM.
372600
372700 5000-END.
372800
372900******************************************************************
373000 5100-DO-USER-FIELDS             SECTION.
373100******************************************************************
373200 5100-START.
373300
373400     PERFORM
373500         VARYING I1 FROM 1 BY 1
373600         UNTIL  (I1 > 36)
373700
373800         IF  (PCT-FLD-NBR (I1)   NOT < 2000)
373900         AND (PCT-FLD-NBR (I1)   NOT > 2099)
374000         AND (PCT-NEW-VALUE (I1) NOT = SPACES)
374100             INITIALIZE PAPCT-SCR-FIELDS
374200             MOVE PCT-FLD-NBR (I1)   TO PAPCT-FLD-NBR
374300             MOVE PCT-NEW-VALUE (I1) TO PAPCT-NEW-VALUE
374400             MOVE PCT-COMPANY        TO PAPCT-COMPANY
374500             MOVE HREMP-EMPLOYEE     TO PAPCT-EMPLOYEE
374600             MOVE PCT-EFFECT-DATE    TO PAPCT-EFFECT-DATE
374700             MOVE PCT-ACTION-CODE    TO PAPCT-ACTION-CODE
374800             MOVE PCT-ACTION-NBR     TO PAPCT-ACTION-NBR
374900             MOVE PCT-REASON (1)     TO PAPCT-REASON1
375000             MOVE PCT-REASON (2)     TO PAPCT-REASON2
101470*            MOVE PCT-USER-ID        TO PAPCT-USER-ID
101470             MOVE CRT-USER-NAME      TO PAPCT-USER-ID
375200             MOVE PCT-ANT-END-DATE   TO PAPCT-ANT-END-DATE
375300             PERFORM 5100-EDIT-USER-FIELDS
375400             PERFORM 3000-HRHEU-PROCESS-TRAN
376000         END-IF
376100     END-PERFORM.
376200
376300 5100-END.
376400
376500*****************************************************************
376600 5200-CREATE-PERSACTION          SECTION.
376700*****************************************************************
376800
377000     MOVE HREMP-COMPANY               TO PAACT-COMPANY.
377100     MOVE "E"                         TO PAACT-ACTION-TYPE.
377200     MOVE HREMP-EMPLOYEE              TO PAACT-EMPLOYEE.
377300     MOVE PCT-ACTION-CODE             TO PAACT-ACTION-CODE.
377400     MOVE PCT-EFFECT-DATE             TO PAACT-EFFECT-DATE.
377500     MOVE "Y"                         TO PAACT-HISTORY.
377600     PERFORM 2300-PAACT-ACTION-NBR.
377700     MOVE PAACT-ACTION-NBR            TO PA100WS-ACTION-NBR.
377800     MOVE "L"                         TO PAACT-ACTION-TYPE.
377900     PERFORM 2300-PAACT-ACTION-NBR.
378000     IF (PAACT-ACTION-NBR > PA100WS-ACTION-NBR)
378100         MOVE PAACT-ACTION-NBR        TO PA100WS-ACTION-NBR.
378200
378300     IF (PRM-RUN-OPTION = 5)
378400         MOVE "M"                     TO DB-ACTION-TYPE
378500     ELSE
378600         MOVE "P"                     TO DB-ACTION-TYPE.
378700     MOVE WS-SV-EFFECT-DATE           TO DB-EFFECT-DATE.
378800     MOVE WS-SV-ACTION-CODE           TO DB-ACTION-CODE.
378900     MOVE ZEROES                      TO DB-EMPLOYEE.
379000     MOVE WS-SV-ACTION-NBR            TO DB-ACTION-NBR.
379100     PERFORM 840-FIND-PCTSET1.
379200
379300     MOVE PERSACTION                  TO WSPCT-PERSACTION.
379400
           MOVE PA100WS-CMT-SEQ-NBR
                                   TO PA100WS-SEQ-COUNTER.
379500     MOVE PCT-COMPANY        TO DB-COMPANY.
379600     MOVE 0                  TO DB-EMP-APP.
379700     MOVE "PA"               TO DB-CMT-TYPE.
379800     MOVE PCT-ACTION-CODE    TO DB-ACTION-CODE.
379900     MOVE PCT-EFFECT-DATE    TO DB-DATE.
380000     MOVE ZEROS              TO DB-EMPLOYEE .
380100     MOVE SPACES             TO DB-JOB-CODE.
380200     MOVE PA100WS-LN-NBR     TO DB-LN-NBR.
380300     MOVE PACSET2-LN-NBR     TO WS-DB-BEG-RNG.
380400     PERFORM 850-FIND-BEGRNG-PACSET2.
380500     PERFORM
380600        UNTIL (PACOMMENTS-NOTFOUND)
380700
380800            PERFORM 810-RECREATE-PACOMMENTS
380900            MOVE HREMP-EMPLOYEE   TO PAC-EMPLOYEE
                  MOVE PA100WS-ACTION-NBR
                                        TO PAC-LN-NBR
381000            ADD 1                 TO PA100WS-SEQ-COUNTER
381100            MOVE PA100WS-SEQ-COUNTER
381100                                  TO PAC-SEQ-NBR
381200            PERFORM 820-STORE-PACOMMENTS
381300            PERFORM 860-FIND-NXTRNG-PACSET2
381400     END-PERFORM.
381500
381600     PERFORM 800-CREATE-PERSACTION.
381700     IF (PRM-RUN-OPTION = 5)
381800         MOVE "E"                     TO PCT-ACTION-TYPE
381900     ELSE
382000         MOVE "L"                     TO PCT-ACTION-TYPE.
382100     MOVE 1                           TO PCT-POS-LEVEL.
382200     MOVE WSPCT-COMPANY               TO PCT-COMPANY.
382300     MOVE HREMP-EMPLOYEE              TO PCT-EMPLOYEE.
382400     MOVE WSPCT-ACTION-CODE           TO PCT-ACTION-CODE.
382500     MOVE WSPCT-EFFECT-DATE           TO PCT-EFFECT-DATE.
382600     MOVE WSPCT-ANT-END-DATE          TO PCT-ANT-END-DATE.
382700     MOVE WSPCT-REASON (1)            TO PCT-REASON (1).
382800     MOVE WSPCT-REASON (2)            TO PCT-REASON (2).
382900     MOVE WSPCT-USER-ID               TO PCT-USER-ID.
J08104     MOVE WSPCT-CREATE-USER-ID        TO PCT-CREATE-USER-ID.
J08104     MOVE WSPCT-CREATE-DATE           TO PCT-CREATE-DATE.
J08104     MOVE WSPCT-CREATE-TIME           TO PCT-CREATE-TIME.
J08104     MOVE WSPCT-TIME-STAMP            TO PCT-TIME-STAMP.
J08104     MOVE WSPCT-DATE-STAMP            TO PCT-DATE-STAMP.
383000     MOVE WSPCT-PARTICIPNT            TO PCT-PARTICIPNT.
383100     MOVE WSPCT-OCCUR-TYPE            TO PCT-OCCUR-TYPE.
383200     MOVE WSPCT-UPDATE-BENEFIT        TO PCT-UPDATE-BENEFIT.
           MOVE WSPCT-UPD-ABS-MGMT          TO PCT-UPD-ABS-MGMT.
383300     MOVE WSPCT-POS-EFF-DT            TO PCT-POS-EFF-DT.
383400     MOVE WSPCT-POSITION              TO PCT-POSITION.
383500     MOVE WSPCT-REQUISITION           TO PCT-REQUISITION.
383600     MOVE WSPCT-JOB-CODE              TO PCT-JOB-CODE.
383700     MOVE PA100WS-ACTION-NBR          TO PCT-ACTION-NBR.
383800     IF (WSPCT-ACTION-TYPE = "P")
383900         MOVE HREMP-PAY-RATE-DN       TO PCT-FLD-NBR (14)
INTL           INITIALIZE                      PAPCTIO-NEW-VALUE
               MOVE WSPCT-PCT-PAY-INC       TO PCT-PCT-PAY-INC
               MOVE WSPCT-PCT-PAY-DEC       TO PCT-PCT-PAY-DEC
               MOVE WSPCT-FLAT-INC          TO PCT-FLAT-INC
               MOVE WSPCT-FLAT-DEC          TO PCT-FLAT-DEC
               MOVE WSPCT-NEW-RATE          TO PCT-NEW-RATE
               MOVE WSPCT-ROUND-OPT         TO PCT-ROUND-OPT
               MOVE WSPCT-ROUND-EVEN        TO PCT-ROUND-EVEN
               PERFORM 4500-CALCULATE-NEW-RATE
               INITIALIZE                      PCT-PCT-PAY-INC
                                               PCT-PCT-PAY-DEC
                                               PCT-FLAT-INC
                                               PCT-FLAT-DEC
                                               PCT-NEW-RATE
                                               PCT-ROUND-OPT
                                               PCT-ROUND-EVEN
INTL           MOVE WS-NEW-PAY-RATE         TO PAPCTIO-NEW-NUM
INTL           MOVE PAPCTIO-NEW-VALUE       TO PCT-NEW-VALUE (14)
384500     ELSE
384600     IF (WSPCT-ACTION-TYPE = "M")
384700         PERFORM
384800             VARYING I1 FROM 1 BY 1
384900             UNTIL  (I1 > 32)
385000                 MOVE WSPCT-FLD-NBR (I1)   TO PCT-FLD-NBR (I1)
385100                 MOVE WSPCT-NEW-VALUE (I1) TO PCT-NEW-VALUE (I1)
385200         END-PERFORM.
385300
           PERFORM 8200-BEFORE-STORE-PERSACTION.
385400     PERFORM 820-STORE-PERSACTION.
385600
385700*****************************************************************
385800 5200-END.
385900*****************************************************************
J21860******************************************************************
J21860 5300-NAME-CHECK                 SECTION.
J21860******************************************************************
J21860 5300-START.
J21860
J21860     MOVE WS-USER-DISPLAY-NAME   TO WS-CHECK-NAME-INPUT.
J21860     PERFORM
J21860         VARYING I1 FROM 1 BY 1
J21860         UNTIL  (I1 > 30)
J21860         OR     ( WS-CHECK-NAME-IN (I1)= "\")
J21860             CONTINUE
J21860     END-PERFORM.
J21860
J21860     IF (I1 > 30)
J21860         MOVE WS-CHECK-NAME-INPUT TO WS-CHECK-NAME-OUTPUT
J21860     ELSE
J21860         ADD 1 TO I1
J21860         MOVE 1 TO I2
J21860         PERFORM
J21860             VARYING I1 FROM I1 BY 1
J21860             UNTIL  (I1 > 30)
J21860                 MOVE WS-CHECK-NAME-IN (I1)
J21860                                 TO WS-CHECK-NAME-OUT (I2)
J21860                 ADD 1 TO I2
J21860         END-PERFORM
J21860     END-IF.
J21860
J21860******************************************************************
J21860 5300-END.
J21860******************************************************************
      ******************************************************************
       5400-ADD-GRP-FLD                SECTION.
      ******************************************************************
       5400-START.

           IF  (WS-USED-BY-GROUP (WS-WRK-FLD-NBR) = SPACES)
               MOVE PCT-COMPANY        TO DB-COMPANY
               MOVE WS-WRK-FLD-NBR     TO DB-FLD-NBR
               INITIALIZE                 DB-COUNTRY-CD-REQ
                                          DB-PROCESS-LEVEL
               PERFORM 840-FIND-PASSET1
               IF  (PASCRTY-FOUND)
               AND (PAS-USED-BY-GROUP = "X")
                   MOVE "X"        TO WS-USED-BY-GROUP (WS-WRK-FLD-NBR)
               ELSE
                   MOVE "-"        TO WS-USED-BY-GROUP (WS-WRK-FLD-NBR)
               END-IF.

           IF  (WS-USED-BY-GROUP (WS-WRK-FLD-NBR) NOT = "X")
               GO TO 5400-END.

J73509*    MOVE WS-WRK-KEY             TO WRK-PA100FLDS-KEY.
J73509*    PERFORM 8400-FIND-PA100FLDS.
J73509*    IF  (PA100FLDS-FOUND)
J73509*        GO TO 5400-END.

           PERFORM 8000-CREATE-PA100FLDS.
J73509     MOVE WS-WRK-REC             TO WRK-PA100FLDS-REC.
           PERFORM 8200-STORE-PA100FLDS.
078854     
078854     IF (PRM-RUN-OPTION = 4)     
078854         INITIALIZE  PA100WS-LATEST-SW
078854         PERFORM 8000-CREATE-PA100LAT
078854*********KEY
078854         MOVE WS-WRK-COMPANY         TO LAT-COMPANY
078854         MOVE WS-WRK-EMPLOYEE        TO LAT-EMPLOYEE
078854         MOVE WS-WRK-FLD-NBR         TO LAT-FLD-NBR
078854*********
078854         PERFORM 8600-FIND-PA100LAT
078854         MOVE WS-WRK-EFFECT-DATE     TO LAT-EFFECT-DATE
078854         MOVE WS-WRK-ACTION-CODE     TO LAT-ACTION-CODE
078854         MOVE WS-WRK-ACTION-NBR      TO LAT-ACTION-NBR
078854         MOVE WS-WRK-NEW-VALUE       TO LAT-NEW-VALUE
078854         MOVE WS-WRK-ACT-OBJ-ID      TO LAT-ACT-OBJ-ID
078854         MOVE WS-WRK-UPDATE-BENEFIT  TO LAT-UPDATE-BENEFIT
078854         MOVE WS-WRK-UPD-ABS-MGMT    TO LAT-UPD-ABS-MGMT
078854         MOVE WS-WRK-DB-FILE         TO LAT-DB-FILE
078854         MOVE WS-WRK-PROCESS-TYPE-SW TO LAT-PROCESS-TYPE-SW
078854
078854         PERFORM 8200-STORE-PA100LAT
078854     END-IF.

       5400-END.
701800******************************************************************
       5500-UPDATE-GROUPS              SECTION.
701800******************************************************************
       5500-START.

J73509*    INITIALIZE WRK-PA100FLDS-KEY
           INITIALIZE WS-UPDATE-COUNT
                      CRT-ERROR-NBR
                      CRT-ERROR-CAT
                      CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3
                      CRT-ERR-VAR4
                      CRT-ERR-VAR5.

J73509*    PERFORM 8500-FIND-NLT-PA100FLDS.
J73509     PERFORM 8600-FIND-NEXT-PA100FLDS.

J73509*    IF (WS-RECORD-COUNT        NOT = ZEROES)
J73509*        PERFORM 5510-READ-PA100FLDS
J73509*        THRU    5510-END
J73509*            WS-RECORD-COUNT    TIMES.

           MOVE "Y"                TO HREMP-ACTION.

           PERFORM 5520-SEL-ACTION-NBR
           THRU    5520-END
               UNTIL (PA100FLDS-NOTFOUND).

078854     IF (PRM-RUN-OPTION = 4)
078854         CLOSE PA100LAT-FILE
078854     END-IF. 
078854
           GO TO 5500-END.

014030******************************************************************
J73509*5510-READ-PA100FLDS.
014030******************************************************************
J73509*
J73509*    PERFORM 8600-FIND-NEXT-PA100FLDS.
J73509*
J73509*5510-END.

014030******************************************************************
       5520-SEL-ACTION-NBR.
014030******************************************************************

           MOVE WRK-COMPANY            TO WS-SAVE-COMPANY.
           MOVE WRK-EMPLOYEE           TO WS-SAVE-EMPLOYEE.
           MOVE WRK-EFFECT-DATE        TO WS-SAVE-EFFECT-DATE.
           MOVE WRK-ACTION-CODE        TO WS-SAVE-ACTION-CODE.
           MOVE WRK-ACTION-NBR         TO WS-SAVE-ACTION-NBR.

           MOVE WRK-COMPANY            TO DB-COMPANY.
           MOVE WRK-EMPLOYEE           TO DB-EMPLOYEE.
           PERFORM 840-FIND-EMPSET1.
           PERFORM 840-FIND-PEMSET1.

           INITIALIZE HRWS-NBR-GROUPS.

           MOVE WS-FALSE               TO HRWS-ALL-EMPLOYEES-SW.
           MOVE WRK-COMPANY            TO HRWS-COMPANY.
           MOVE WRK-EFFECT-DATE        TO HRWS-EFFECT-DATE.
           MOVE WRK-ACT-OBJ-ID         TO HRWS-ACT-OBJ-ID.
           MOVE WRK-UPDATE-BENEFIT     TO HRWS-UPDATE-BENEFIT.
           MOVE WRK-UPD-ABS-MGMT       TO HRWS-UPDATE-ABSENCE-MGMT.
           MOVE "Y"                    TO HRWS-COMP-FROM-TABLE-SW.
J63431     MOVE WRK-PROCESS-TYPE-SW    TO HREMP-PROCESS-TYPE-SW.           

           INITIALIZE HRWS-FIELD-NBR-TABLE
                      I1.

           PERFORM 5530-SEL-FLD-NBR
           THRU    5530-END
               UNTIL (PA100FLDS-NOTFOUND)
               OR    (WRK-COMPANY      NOT = WS-SAVE-COMPANY)
               OR    (WRK-EMPLOYEE     NOT = WS-SAVE-EMPLOYEE)
               OR    (WRK-EFFECT-DATE  NOT = WS-SAVE-EFFECT-DATE)
               OR    (WRK-ACTION-CODE  NOT = WS-SAVE-ACTION-CODE)
               OR    (WRK-ACTION-NBR   NOT = WS-SAVE-ACTION-NBR).

           PERFORM 5000-REBUILD-PERSONNEL-GROUP.

           IF (HRWS-GROUP-SIZE-ERROR)
               SET WARNINGS-PRINTED    TO TRUE
               MOVE EMP-EMPLOYEE       TO RWG6-EMPLOYEE
               MOVE EMP-LAST-NAME      TO HRWS-LAST-NAME
               MOVE EMP-FIRST-NAME     TO HRWS-FIRST-NAME
               MOVE EMP-MIDDLE-INIT    TO HRWS-MIDDLE-INIT
               MOVE EMP-LAST-NAME-PRE  TO HRWS-LAST-NAME-PRE
               MOVE EMP-NAME-SUFFIX    TO HRWS-NAME-SUFFIX
               PERFORM 750-HR-FORMAT-NAME
               MOVE HRWS-FORMAT-NAME   TO RWG6-EMP-NAME
               MOVE 337                TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE        TO RWG6-WARNING-DESC
               MOVE WRK-ACTION-CODE    TO RWG6-PCT-ACTION-CODE
               MOVE WRK-ACTION-NBR     TO RWG6-PCT-ACTION-NBR
               INITIALIZE                 RWG6-PCT-POS-LEVEL
               MOVE WRK-EFFECT-DATE    TO RWG6-PCT-EFFECT-DATE
               IF (PRM-RUN-OPTION = 2)
                   MOVE "E"            TO RWG6-ACTION-TYPE
               END-IF
               IF (PRM-RUN-OPTION = 3)
                   MOVE "L"            TO RWG6-ACTION-TYPE
               END-IF
               IF (PRM-RUN-OPTION = 4)
                   INITIALIZE             RWG6-ACTION-TYPE
               END-IF
               IF (PRM-RUN-OPTION = 5)
                   MOVE "M"            TO RWG6-ACTION-TYPE
               END-IF
               IF (PRM-RUN-OPTION = 7)
                   MOVE "P"            TO RWG6-ACTION-TYPE
               END-IF
               MOVE WARNING-LINE       TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP.

           IF (PRM-RUN-OPTION          = 5 OR 6)
               SET SKIP-GROUP-SELECT   TO TRUE.

           COMPUTE HRWS-NBR-GROUPS     = HRWS-NBR-GROUPS
                                       * 3.

           ADD HRWS-NBR-GROUPS         TO WS-UPDATE-COUNT.

           IF (WS-UPDATE-COUNT         > WS-MAX-OPS-IN-TRAN)
               PERFORM 900-SAVE-PRINT-FILES
               PERFORM 840-MODIFY-CKPSET1
P69000         MOVE CKP-RESTART-INFO   TO WS-RESTART-INFO
               MOVE WS-RECORD-COUNT    TO WS-RESTART-RECORD
P69000         MOVE EMP-EMPLOYEE       TO WS-RESTART-EMPLOYEE
               MOVE WS-RESTART-INFO    TO CKP-RESTART-INFO
               PERFORM 820-STORE-CKPOINT
P69000         PERFORM 925-AUDIT-END
P69000         PERFORM 910-AUDIT-BEGIN
               MOVE 1                  TO WS-UPDATE-COUNT.

       5520-END.

014030******************************************************************
       5530-SEL-FLD-NBR.
014030******************************************************************

           ADD 1                       TO WS-RECORD-COUNT
                                          I1.

078854     IF (PRM-RUN-OPTION = 4)
078854         INITIALIZE  PA100WS-LATEST-SW
078854                     PA100LAT-REC
078854         MOVE WRK-COMPANY            TO LAT-COMPANY
078854         MOVE WRK-EMPLOYEE           TO LAT-EMPLOYEE
078854         MOVE WRK-FLD-NBR            TO LAT-FLD-NBR   
078854         PERFORM 8600-FIND-PA100LAT 
078854         IF (PA100LAT-FOUND)
078854             MOVE LAT-FLD-NBR        TO HRWS-FIELD-NBR (I1)
078854             MOVE LAT-NEW-VALUE      TO HRWS-FIELD-VALUE (I1)
078854             MOVE LAT-DB-FILE        TO HRWS-DATABASE-FILE (I1)
078854         END-IF
078854     ELSE
               MOVE WRK-FLD-NBR            TO HRWS-FIELD-NBR (I1)
               MOVE WRK-NEW-VALUE          TO HRWS-FIELD-VALUE (I1)
               MOVE WRK-DB-FILE            TO HRWS-DATABASE-FILE (I1)
078854     END-IF.

       5530-NEXT-PA100FLDS.
           PERFORM 8600-FIND-NEXT-PA100FLDS.

       5530-END.

014030******************************************************************
       5500-END.
014030******************************************************************

      ******************************************************************
       6000-PRINT-MESSAGES             SECTION.
      ******************************************************************
       6000-START.

           IF  (HREMP-I2 = ZEROES)
               GO TO 6000-END.

           IF  (HREMP-ERROR-FOUND)
               IF  (WS-ERR-ACTION-TYPE = "A")
                   MOVE APL-APPLICANT      TO REG6-EMPLOYEE
                   MOVE "A"                TO REG6-APP
               ELSE
                   MOVE HREMP-EMPLOYEE     TO REG6-EMPLOYEE
               END-IF
               MOVE HRWS-FORMAT-NAME       TO REG6-EMP-NAME
               MOVE WS-ERR-ACTION-CODE     TO REG6-PCT-ACTION-CODE
               MOVE WS-ERR-ACTION-NBR      TO REG6-PCT-ACTION-NBR
               MOVE WS-ERR-ACTION-TYPE     TO REG6-ACTION-TYPE
               MOVE WS-ERR-POS-LEVEL       TO REG6-PCT-POS-LEVEL
               MOVE WS-ERR-EFFECT-DATE     TO REG6-PCT-EFFECT-DATE
               PERFORM
                   VARYING I3 FROM 1 BY 1
                   UNTIL  (I3 > HREMP-I2)
                   MOVE HREMP-MESSAGE (I3)     TO REG6-ERROR-DESC
                   MOVE ERROR-LINE             TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
                   INITIALIZE REG6-ACTION-TYPE
                              REG6-APP
               END-PERFORM
               SET ERRORS-PRINTED TO TRUE
           ELSE
               IF  (WS-ERR-ACTION-TYPE = "A")
               AND (PRM-UPDATE-OPTION = "N")
                   MOVE APL-APPLICANT      TO RWG6-EMPLOYEE
                   MOVE "A"                TO RWG6-APP
               ELSE
                   MOVE HREMP-EMPLOYEE     TO RWG6-EMPLOYEE
               END-IF
               MOVE HRWS-FORMAT-NAME       TO RWG6-EMP-NAME
               MOVE WS-ERR-ACTION-CODE     TO RWG6-PCT-ACTION-CODE
               MOVE WS-ERR-ACTION-NBR      TO RWG6-PCT-ACTION-NBR
               MOVE WS-ERR-ACTION-TYPE     TO RWG6-ACTION-TYPE
               MOVE WS-ERR-POS-LEVEL       TO RWG6-PCT-POS-LEVEL
               MOVE WS-ERR-EFFECT-DATE     TO RWG6-PCT-EFFECT-DATE
               PERFORM
                   VARYING I3 FROM 1 BY 1
                   UNTIL  (I3 > HREMP-I2)
                   MOVE HREMP-MESSAGE (I3)     TO RWG6-WARNING-DESC
                   MOVE WARNING-LINE           TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
                   INITIALIZE RWG6-ACTION-TYPE
                              RWG6-APP
               END-PERFORM
               SET WARNINGS-PRINTED TO TRUE
           END-IF.

      ******************************************************************
       6000-END.
      ******************************************************************
701800******************************************************************
       7000-PROCESS-ON-HOLD            SECTION.
701800******************************************************************
       7000-START.

121800     IF (PCT-EFFECT-DATE > WS-PRM-EFFECT-DATE)
               GO TO 7000-NEXT.

           IF  (PRM-USER-ID NOT = SPACES)
           AND (PRM-USER-ID NOT = PCT-USER-ID)
               GO TO 7000-NEXT.

129300     IF  (WS-SPEC-ACT-FOUND)
           AND (PCT-ACTION-TYPE = "E" OR "L")
129400     AND (PCT-ACTION-CODE NOT = PRM-EMP-ACTION (1))
129500     AND (PCT-ACTION-CODE NOT = PRM-EMP-ACTION (2))
129600     AND (PCT-ACTION-CODE NOT = PRM-EMP-ACTION (3))
129700     AND (PCT-ACTION-CODE NOT = PRM-EMP-ACTION (4))
129800     AND (PCT-ACTION-CODE NOT = PRM-EMP-ACTION (5))
129900     AND (PCT-ACTION-CODE NOT = PRM-EMP-ACTION (6))
130000         GO TO 7000-NEXT.

           IF  (PCT-ACTION-TYPE = "E" OR "L")
           AND (PRM-EMPLOYEE NOT = ZEROES)
           AND (PCT-EMPLOYEE      NOT = PRM-EMPLOYEE)
               GO TO 7000-NEXT.

           IF  (PCT-ACTION-TYPE  = "E" OR "L")
           AND (PRM-GROUP-NAME   NOT = SPACES)
               MOVE PRM-COMPANY         TO DB-COMPANY
               MOVE PCT-EMPLOYEE        TO DB-EMPLOYEE
               MOVE PRM-GROUP-NAME      TO DB-GROUP-NAME
               PERFORM 840-FIND-PGESET1
               IF (PGEMPLOYEE-NOTFOUND)
                   GO TO 7000-NEXT.

           IF (PCT-ACTION-TYPE     = "E" OR "L")
               IF (PRM-EMP-PROC-GRP  NOT = SPACES)
               OR (PRM-PROCESS-LEVEL NOT = SPACES)
                   MOVE PCT-COMPANY            TO DB-COMPANY
                   MOVE PCT-EMPLOYEE           TO DB-EMPLOYEE
                   PERFORM 840-FIND-EMPSET1
                   IF  (PRM-PROCESS-LEVEL NOT = SPACES)
                   AND (PRM-PROCESS-LEVEL NOT = EMP-PROCESS-LEVEL)
                       GO TO 7000-NEXT
                   END-IF
                   IF (PRM-EMP-PROC-GRP NOT = SPACES)
                       MOVE PRM-COMPANY        TO DB-COMPANY
                       MOVE PRM-EMP-PROC-GRP   TO DB-PROC-GROUP
                       MOVE EMP-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
                       PERFORM 840-FIND-PRPSET1
                       IF (PRPROCGRP-NOTFOUND)
                           GO TO 7000-NEXT
                       END-IF
                   END-IF.

           IF  (PCT-ACTION-TYPE = "A")
           AND (PRM-APPLICANT   NOT = ZEROES)
           AND (PCT-EMPLOYEE    NOT = PRM-APPLICANT)
               GO TO 7000-NEXT.

           IF (PCT-ACTION-TYPE = "A")
               IF (PCT-ACTION-NBR > 1)
                   GO TO 7000-NEXT
               END-IF
               IF (PRM-APL-PROC-GRP   NOT = SPACES)
               OR (PRM-APL-PROC-LEVEL NOT = SPACES)
                   SET NO-RECORD-FOUND     TO TRUE
                   MOVE SPACES TO WS-PROCESS-LEVEL
                   IF  (PCT-ACTION-NBR = 1)
                       PERFORM
                           VARYING I1 FROM 1 BY 1
                           UNTIL  (I1 > 36)
                           OR     (RECORD-FOUND)
                               IF (PCT-FLD-NBR (I1) =
                                                HREMP-PROCESS-LEVEL-DN)
                                   MOVE PCT-NEW-VALUE (I1)
                                                TO WS-PROCESS-LEVEL
                                   SET RECORD-FOUND TO TRUE
                               END-IF
                       END-PERFORM
                       IF (WS-PROCESS-LEVEL NOT = 
                                            PRM-APL-PROC-LEVEL)
                           GO TO 7000-NEXT
                       END-IF
                       IF (PRM-APL-PROC-GRP NOT = SPACES)
                           MOVE PRM-COMPANY  TO DB-COMPANY
                           MOVE PRM-APL-PROC-GRP
                                             TO DB-PROC-GROUP
                           MOVE PCT-NEW-VALUE (I1)
                                             TO DB-PROCESS-LEVEL
                           PERFORM 840-FIND-PRPSET1
                           IF (PRPROCGRP-FOUND)
                               SET RECORD-FOUND TO TRUE
                           END-IF
                       END-IF
                   END-IF
                   IF (NO-RECORD-FOUND)
                       GO TO 7000-NEXT.

           MOVE PCT-EMPLOYEE           TO RHLD2-PCT-EMPLOYEE.
           IF (PCT-ACTION-TYPE = "A")
               IF (APPLICANT-NOTFOUND)
               OR (APL-COMPANY   NOT = PCT-COMPANY)
               OR (APL-APPLICANT NOT = PCT-EMPLOYEE)
                   MOVE PCT-COMPANY    TO DB-COMPANY
                   MOVE PCT-EMPLOYEE   TO DB-APPLICANT
                   PERFORM 840-FIND-APLSET1
               END-IF
               MOVE APL-LAST-NAME      TO HRWS-LAST-NAME
               MOVE APL-FIRST-NAME     TO HRWS-FIRST-NAME
               MOVE APL-MIDDLE-INIT    TO HRWS-MIDDLE-INIT
               MOVE APL-LAST-NAME-PRE  TO HRWS-LAST-NAME-PRE
               MOVE APL-NAME-SUFFIX    TO HRWS-NAME-SUFFIX
           ELSE
               IF (EMPLOYEE-NOTFOUND)
               OR (EMP-COMPANY  NOT = PCT-COMPANY)
               OR (EMP-EMPLOYEE NOT = PCT-EMPLOYEE)
                   MOVE PCT-COMPANY    TO DB-COMPANY
                   MOVE PCT-EMPLOYEE   TO DB-EMPLOYEE 
                   PERFORM 840-FIND-EMPSET1
               END-IF
               MOVE EMP-LAST-NAME      TO HRWS-LAST-NAME
               MOVE EMP-FIRST-NAME     TO HRWS-FIRST-NAME
               MOVE EMP-MIDDLE-INIT    TO HRWS-MIDDLE-INIT
               MOVE EMP-LAST-NAME-PRE  TO HRWS-LAST-NAME-PRE
               MOVE EMP-NAME-SUFFIX    TO HRWS-NAME-SUFFIX
           END-IF.
           PERFORM 750-HR-FORMAT-NAME.
           MOVE HRWS-FORMAT-NAME       TO RHLD2-EMP-NAME.

           MOVE PCT-ACTION-CODE        TO RHLD2-PCT-ACTION-CODE.
           MOVE PCT-ACTION-NBR         TO RHLD2-PCT-ACTION-NBR.
           MOVE PCT-EFFECT-DATE        TO RHLD2-PCT-EFFECT-DATE.
           MOVE PCT-ANT-END-DATE       TO RHLD2-PCT-ANT-END-DATE.
           MOVE PCT-REASON (1)         TO RHLD2-PCT-REASON-1.
           MOVE PCT-REASON (2)         TO RHLD2-PCT-REASON-2.
           MOVE PCT-POS-LEVEL          TO RHLD2-PCT-POS-LEVEL.
           MOVE PCT-UPDATE-BENEFIT     TO RHLD2-PCT-UPDATE-BENEFIT.
           MOVE PCT-UPD-ABS-MGMT       TO RHLD2-PCT-UPD-ABS-MGMT.
           MOVE PCT-UPDATE-REQ-DED     TO RHLD2-PCT-UPDATE-REQ-DED.
      *    MOVE PCT-USER-ID            TO RHLD2-PCT-USER-ID.
101470*    MOVE PCT-USER-ID            TO WS-USER-DBUIDKEY.
101470     MOVE CRT-USER-NAME          TO WS-USER-DBUIDKEY.
J21860     PERFORM 900-GET-USER-DISPLAY-NAME.
J21860     PERFORM 5300-NAME-CHECK.
J21860     MOVE WS-CHECK-NAME-OUTPUT   TO RHLD2-PCT-USER-ID.          
           MOVE PCT-ACTION-TYPE        TO RHLD2-PCT-ACTION-TYPE.
           MOVE PAT-DESCRIPTION        TO RHLD2-ACTION-DESC.
           MOVE RHLD2-PCT-HELD-ACTIONS TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

258900     PERFORM 7200-PRINT-ACTION-DETAIL
259000         VARYING I1 FROM 1 BY 1
259100         UNTIL  (I1 > 36).

190200 7000-NEXT.
190300
191900     PERFORM 860-FIND-NXTRNG-PCTSET1.
193000
      ******************************************************************
       7000-END.
      ******************************************************************
262200*****************************************************************
262300 7200-PRINT-ACTION-DETAIL        SECTION.
262400*****************************************************************
262500 7200-START.
262600
262700     IF (PCT-FLD-NBR (I1) = ZEROS)
262800         MOVE 36                     TO I1
262900         GO TO 7200-END.
263000
263400     MOVE PCT-FLD-NBR (I1)       TO DB-FLD-NBR.
263500     PERFORM 840-FIND-PADSET1.
263600     IF (PADICT-FOUND)
               MOVE PAD-ITEM-NAME      TO CRT-PHRASE
               MOVE WS-PHRASE-SIZE     TO CRT-PHRASE-SIZE
               MOVE "N"                TO CRT-PUT-DOTS
               PERFORM 900-GET-PHRASE-XLT
               MOVE CRT-PHRASE-XLT     TO RHLD3-FIELD-DESC
263800     ELSE
263900         MOVE SPACES             TO RHLD3-FIELD-DESC.
264000
INTL       IF  (PADICT-FOUND)
INTL           MOVE PAD-DATA-TYPE          TO PAPCTIO-DATA-TYPE
               MOVE "N"                    TO PAPCTIO-DATA-CURR
INTL           MOVE PAD-DECIMALS           TO PAPCTIO-DATA-DECIMALS
INTL           MOVE PCT-NEW-VALUE (I1)     TO PAPCTIO-NEW-VALUE
INTL           PERFORM 9200-FORMAT-PAPCT-TO-DISPLAY
INTL           MOVE PAPCTIO-NEW-VALUE      TO RHLD3-PCT-NEW-VALUE
INTL       ELSE
264100         MOVE PCT-NEW-VALUE (I1)     TO RHLD3-PCT-NEW-VALUE.
264200
264300     MOVE RHLD3-PCT-ACTION-DETAIL    TO RPT-GROUP-REQUEST.
264400     PERFORM 700-PRINT-RPT-GRP.
264500
264600 7200-END.
P63874 
P63874******************************************************************
P63874 7500-GET-ACTION-OBJ-ID          SECTION.
P63874******************************************************************
P63874 7500-START.
P63874
P63874     MOVE "PAACT"                       TO   IFOBIWS-OBJ-TYPE.
P63874     IF  (PAPEP-OBJ-ID-OPTIMIZE)
P63874         IF (PAPEP-OBJ-ID-LEFT = ZEROES)
P63874            MOVE WS-MAX-OPS-IN-TRAN     TO   IFOBIWS-NBR-OBJECTS
P63874                                             PAPEP-OBJ-ID-LEFT
P63874            PERFORM 7000-ASSIGN-OBJ-ID-70
P84384            PERFORM 925-AUDIT-END
P84384            PERFORM 910-AUDIT-BEGIN
P63874            MOVE IFOBIWS-OBJ-ID         TO   PAPEP-MEM-OBJ-ID
P63874            SUBTRACT WS-MAX-OPS-IN-TRAN FROM PAPEP-MEM-OBJ-ID
P63874         END-IF
P63874         ADD 1                          TO   PAPEP-MEM-OBJ-ID
P63874         MOVE PAPEP-MEM-OBJ-ID          TO   IFOBIWS-OBJ-ID
P63874         SUBTRACT 1                     FROM PAPEP-OBJ-ID-LEFT
P63874     ELSE
P63874         PERFORM 7000-ASSIGN-OBJ-ID-70
P84384         PERFORM 925-AUDIT-END
P84384         PERFORM 910-AUDIT-BEGIN
P63874     END-IF.
P63874
P63874******************************************************************
P63874 7500-END.
P63874******************************************************************
P63874
014030******************************************************************
014040 8000-CREATE-PA100FLDS           SECTION.
014050******************************************************************
014060 8000-START.
014070
J73509     INITIALIZE WRK-PA100FLDS-REC.
014300
014310******************************************************************
014320 8000-END.
014330******************************************************************
014340
014350******************************************************************
014360 8200-STORE-PA100FLDS            SECTION.
014370******************************************************************
014380 8200-START.
014390
J73509     WRITE PA100FLDS-REC FROM WRK-PA100FLDS-REC.
014540
014550******************************************************************
014560 8200-END.
014570******************************************************************
014580
014590******************************************************************
J73509*8400-FIND-PA100FLDS             SECTION.
014610******************************************************************
J73509*8400-START.
J73509*
J73509*    SET PA100FLDS-FOUND              TO TRUE.
J73509*
J73509*    READ PA100FLDS-FILE              KEY WRK-PA100FLDS-KEY
J73509*        INVALID KEY
J73509*            SET PA100FLDS-NOTFOUND   TO TRUE
J73509*    END-READ.
J73509*
014710******************************************************************
J73509*8400-END.
014730******************************************************************
014740
014750******************************************************************
J73509*8500-FIND-NLT-PA100FLDS         SECTION.
014770******************************************************************
J73509*8500-START.
J73509*
J73509*    SET PA100FLDS-FOUND              TO TRUE.
J73509*
J73509*    START PA100FLDS-FILE             KEY NOT < WRK-PA100FLDS-KEY
J73509*        INVALID KEY
J73509*            SET PA100FLDS-NOTFOUND   TO TRUE
J73509*    END-START.
J73509*
J73509*    IF (PA100FLDS-FOUND)
J73509*        PERFORM 8600-FIND-NEXT-PA100FLDS.
J73509*
014900******************************************************************
J73509*8500-END.
014920******************************************************************
014930
014940******************************************************************
014950 8600-FIND-NEXT-PA100FLDS        SECTION.
014960******************************************************************
014970 8600-START.
014980
014990     SET PA100FLDS-FOUND              TO TRUE.
015000
J73509*    READ PA100FLDS-FILE              NEXT RECORD
J73509*        AT END
J73509*            SET PA100FLDS-NOTFOUND   TO TRUE
J73509*    END-READ.
J73509     READ PA100FLDS-FILE              INTO WRK-PA100FLDS-REC 
J73509         AT END
J73509             SET PA100FLDS-NOTFOUND   TO TRUE
J73509     END-READ.
015050
015060******************************************************************
015070 8600-END.
015080******************************************************************
015090
078854******************************************************************
078854 8000-CREATE-PA100LAT           SECTION.
078854******************************************************************
078854 8000-START.
078854
078854     INITIALIZE PA100LAT-REC.
078854
078854******************************************************************
078854 8000-END.
078854******************************************************************
078854
078854******************************************************************
078854 8200-STORE-PA100LAT            SECTION.
078854******************************************************************
078854 8200-START.
078854
078854     IF (PA100LAT-NOTFOUND)
078854         WRITE PA100LAT-REC     FROM PA100LAT-REC
078854     ELSE
078854         REWRITE PA100LAT-REC   FROM PA100LAT-REC
078854     END-IF.
078854
078854******************************************************************
078854 8200-END.
078854******************************************************************
078854
078854*****************************************************************
078854 8600-FIND-PA100LAT            SECTION.
078854******************************************************************
078854 8600-START.
078854
078854     SET PA100LAT-FOUND               TO TRUE.
078854
078854     READ PA100LAT-FILE               KEY LAT-PA100-REC-KEY
078854         INVALID KEY
078854             SET PA100LAT-NOTFOUND    TO TRUE.
078854
078854******************************************************************
078854 8600-END.
078854******************************************************************
