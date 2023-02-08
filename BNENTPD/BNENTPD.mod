******* BNENTPD 123.1.64 <4159075680>
      ******************************************************************
      *                            BNENTPD                             *
      ******************************************************************
      *                                                                *
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  401777 | J01777 | INCLUDE BN100 WHEN VALIDATING FOR BENEFIT/  *
      *         |        | PLAN ELIGIBILITY DONE ON BN31 AND BN531 TO  *
      *         |        | PREVENT ENROLLMENT OF BENEFITS WITH FUTURE  *
      *         |        | ELIGIBILITY DATES.                          *
      *  ------   ------   ------------------------------------------  *
      *  449766 | J49766 | ADDED 860-FIND-NXTRNG-PDHSET1 WHEN CHECKING *
      *         |        | DEDUCTION AMOUNT FROM PREMDEDHST.           *
      *  ------   ------   ------------------------------------------  *
      *  557057 | J57057 | ADDED CONDITION FOR BNENTPD #123 ERROR.     *
      *         |        | ADDED DELETE EMPLEXREM WHEN VALUE FOR       *
      *         |        | EFR-CREDITS-AVAIL IS ZERO ALREADY.          *
      *  ------   ------   ------------------------------------------  *
      *  594418 | J94418 | UPDATED USER-ID OF PREVIOUS BENEFIT RECORD  *
      *         |        | BEING STOPPED WHEN ADDING NEW BENEFIT       *
      *  ------   ------   ------------------------------------------  *
      *  716908 | J16908 | Remove the Delete EMPFLEXREM from JT-449766 *
      *  ------   ------   ------------------------------------------  *
      *  226055 | J26055 | ADDED 3000-PREDM-PROCESS-TRAN TO SAVE A     *
      *         |        | DEDUCTION STOP DATE OVERRIDE.               *
      *  ------   ------   ------------------------------------------  *
      *  224483 | J24483 | ADDED CONDITION AGAINST STATUS 9 IN PAYMASTR*
      *         |        | TO NO ALLOW BENEFIT CHANGES                 *
      *  ------   ------   ------------------------------------------  *
      *  664828 | J64828 | Added the contribution logic from 2400-EDIT-*
      *         |        | DTL-DATA to 2800-EDIT-STOP-DTL
      *  ------   ------   ------------------------------------------- *
      *  717193 | J17193 | ACA REGULATORY REPORTING                    *
      *  ------   ------   ------------------------------------------- *
      *  827636 | J27636 | Applied fix to enable mid year stop of BN   *
      *         |        | plan having a different coverage option     *
      *  ------   ------   ------------------------------------------- *
      *  841123 | J41123 | BN100 using Entry rules insted of add rules *
      *  ------   ------  -------------------------------------------- *
      *  864465 | J64465 | Doubling Add Rules wait period date         *
      *  ------   ------  -------------------------------------------- *
      *  867617 | J67617 | Fixed the issue of error while storing in   *
      *         |        | table EMDEDMASTR                            *
      *  ------   ------  -------------------------------------------- *
      *  861613 | J61617 | Wellnes Credit Assistance for Centura       *
      *  ------   ------  -------------------------------------------- *
      *  890269 | J90269 | POPULATED BNWS-COVER-OPT FOR BNEMDED.       *
      *  ------  -------   ------------------------------------------  *
      *  890068 | J90068 | FIXED FOR NOT CREATING A RECORD IN BN53 FOR *
      *         |        | DEPENDENT WHEN CREATE TRANSACTION NO IS     *
      *         |        | SELECTED WHILE DOING CHANGES IN BN32        *
      *  ------  -------   ------------------------------------------  *
      *  906128 | J06128 | FIXED FOR STOREDBREC ERROR IN EMDEDMASTR    *
      *  ------   ------  -------------------------------------------- *
      *  901388 | J01388 | FIXED FOR ILLEGAL CHARACTER ERROR           *
      *  ------   ------  -------------------------------------------- *
      *  912251 | J12251 | FIXED ILLEGAL CHARACTER ERROR FOR PLAN      *
      *         |        | TYPE = RS AND DC.                           *
      * -------   ------  -------------------------------------------- *
      * 1030162 | 030162 | INITIALIZED BNEDWS-ELIGIBILITY-DATE AFTER   *
      *         |        | USED IN COMPUTING ELIG DATE FOR ACA PLANS   *
      * -------   ------  -------------------------------------------- *
      * 1031132 | 031132 | INITIALIZED BNEDWS-FROM-MAGIC-SW AFTER USED *
      *         |        | IN COMPUTING ELIG DATE FOR ACA PLANS        *
      * -------   ------  -------------------------------------------- *
      * 1000884 | J00884 | BYPASS ERROR 158 IF BEN-PCT-AMT-FLG IS NOT  *
      *         |        | EQUAL TO "A" OR "P"                         *
      * -------   ------  -------------------------------------------- *
      * 1029712 | 029712 | MOVE BNCTWS-IN-SMOKER-SW TO BNCTWS-SMOKER-SW*
      *         |        | TO STORE SMOKER FLAG ON ENROLLMENT          *
      * -------   ------  -------------------------------------------- *
      * 1033536 | 033536 | INCLUDE BN32 WHEN VALIDATING FOR BENEFIT    *
      *         |        | PLAN ELIGIBILITY DONE ON BN31,BN531, & BN100*
      * -------   ------  -------------------------------------------- *
      * 1048246 | 048246 | ALWAYS USE SYSTEM DATE AND SYSTEM TIME FOR  *
      *         |        | ALL BENEFIT TABLE AUDIT TRAILS              *
      * -------   ------  -------------------------------------------- *
      * 1087429 | 087429 | USE DATES FROM CSV FILE IF PROGRAM CALLING  *
      *         |        | IS BN531 FOR BENEFIT TABLE AUDIT TRAILS     *
      * -------   ------  -------------------------------------------- *
      * 1117816 | 117816 | ADDED A VALIDATION FOR THE INITIALIZATION   *
      *         |        | OF BNEDWS-MAGIC-SW FOR NON FLEX AND         *
      *         |        | UNCOMMENTED IT TO READ THE BNWAIT AND GET   *
      *         |        | THE CORRECT DATA.                           *
      * -------   ------- -------------------------------------------- *
      *  864465 | J64465 | Doubling Add Rules wait period date         *
      *  ------   ------  -------------------------------------------- *
      *  857497 | J57497 | Enhance to populate Entry-Date with the BN35*
      *         |        | effective date instead of plan Entry Rule Dt*
      *  ------   ------  -------------------------------------------- *
000100******************************************************************
000100*Use Range operators wherever possible                           *
000100******************************************************************
      ******************************************************************
      *                                                                *
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  313588 | J13588 | Remove LP trigger checks                    *
      *  ------   ------   ------------------------------------------  *
      *  307540 | J07540 | DOMESTIC PARTNER                            *
      *  ------   ------   ------------------------------------------  *
      *  267795 | J67795 | ALLOW 4 DECIMALS FOR BN PENSION PERCENT PLAN*
      *  ------   ------   ------------------------------------------  *
      *  866130 | J66130 | FIX FOR SHOWING ERROR MESSAGE " Only two    *
      *         |        | decimal places allowed                      *
      *  ------   ------   ------------------------------------------  *
      *  871747 | J71747 | FIX FOR STORE MORE THAN 2 DECIMAL PLACES IN *
      *         |        | ENDEDMASTR                                  *
      *  ------   ------   ------------------------------------------  *
      *  907169 | J07169 | FIXED ERROR MESSAGE FOR CONTRIB TYPE "5","3"*
      *  ------   ------   ------------------------------------------  *
      * 1164175 | 164175 | Direct YTD values to the right BEN- record  *
      * -------   ------  -------------------------------------------- *
      * 1248335 | 248335 | REMOVED CHANGES MADE UNDER JT-1164175       *
      * -------   ------  -------------------------------------------- *
      ******************************************************************
      ******************************************************************
      *               M O D I F I C A T I O N   L O G:                 *
      ******************************************************************
      *  Modified by Analysts International,MN                         *
AI0095******************************************************************
AI0095*  AI0095  03/24/03  RETROACTIVE BENEFIT CHANGE MODIFICATION     *
AI0095******************************************************************
AI0010*
AI0010*  AI0010  03/24/03  MODIFY THE BENEFIT PLAN ELIGIBILITY COMMON  *
AI0010*                    MODULE TO UTILIZE THE EFFECTIVE DATE AND    *
AI0010*                    END DATE FIELDS IN DETERMINATION OF THE     *
AI0010*                    EMPLOYEES ELIGIBILITY FOR THE BENEFIT PLAN. *
AI0030*  AI0030  07/24/03  ADD LOGIC FOR DL AND HL BENEFICIARIES       *
      ******************************************************************
      *  Modified by CPS                       *
AI0095******************************************************************
      *  SDB 4/16/2019   SDB416   ADDED STATUS CHECK TO DETERMINE
      *                           NONPARTIC-CD 
000300*=================================================================
      *  SDB 10/29/2018  ADD BEN-PLAN-COCE(3:1) = "F" OR "R"
      *                  AND EMP-EMP-STATUS = "TO"
000300*=================================================================
      *  SDB 10/8/2018    ADDED LOGIC TO SEE IF THE PLAN IS ON THE 
      *                   IF10 (LUPTBLDTL) BEFORE WRITING THE TRIGGER   
000300*=================================================================
      * 9/2018  SDB  MODIFIED LOGIC SO THAT PLN-CONTRIB-TYPE IS NOT 
      *         CHECKED FOR > ZEROES AND NOT="0"
      *         LOGIC ADDED TO SKIP PLANS THAT START WITH "Y"
000300*=================================================================
      * 6/2018 FP 4823  Add updates for status RD same as RE
      *       SEARCH SR4823
      *=================================================================
      * 8/2015 FP 8141  Add updates for status RN same as RE
      *       SEARCH SDB815
      *================================================================*
      * 1/2014 FP 5155 SDB114 - ADDED LOGIC TO TREAT PLAN-TYPE = "DC"  *
      *        THE SAME AS CONTRIBUTION-TYPE = 0 FOR ADDING, CHANGING  *
      *        OR DELETING A TRIGGER                                   *
      *================================================================*
ACS002******************************************************************
ACS002* 03/23/2011 MHUNTER   MODIFIED TO WRITE RECORD TO ZN325WK1 FILE
ACS002*                      WHEN HRDEPBEN RECORD IS DELETED.  FILE IS 
ACS002*                      USED FOR REPORTING IN THE ZN325 PROGRAM.
000300******************************************************************
SDB308*      WO 8930. ADD LOGIC FOR NEW PLAN CODES AND CRP-VEST        *
*******     AND SRA-VEST CODES AND DEFAULTING               *
      *================================================================*
000100******************************************************************
MG0629*  06/29/07  - DURING ER WITHDRAWAL PROCESS, BN531 ENROLLS EE's  *
MG0629*              INTO PNP PLAN BUT DOES NOT POPULATE CRP-VEST,     *
MG0629*              SRA-VEST and NONPARTIC-CO FIELDS CORRECTLY        *
000100******************************************************************
GW0308*  SEE WO 5794. IF PREVIOUS SPAN = PNP,DEFAULT NON-PARTIC TO ZERO*
*******     SEE WO 4422 ABOVE. SEE PARA 4220-UPDATE                    *
      *================================================================*
GW1206*  SEE WO 4422. CODED 12/2006 BUT NOT MOVED TO PROD UNTIL 7/2007.*
      *   ASSIGN PRIOR NON-PARTIC CODE IF NONE EXISTS RATHER           *
      *   THAN DEFAULTING TO 0                                         *
      *================================================================*
      ******************************************************************
GW    * 2/2005  WBP TRIGGER RECS AREN'T GETTING WRITTEN ON REHIRES     *
      *  SEE 4420-GET-LAST-BILLED-DATE.                                *
      *  EG WE FIND THE LAST PBH/ZB4(PBK)RECS FOR AN EE IS  PRIOR TO THE   *
      *   NEW BEN DATE SO WE DONT WRITE A WBP REC. BUT IT IS THE ER    *
      *   WHO IS GETTING BILLED AND THEIR MOST RECENT BILL DATE        *
      *   MAY BE MORE RECENT THAN THE BEN-START-DATE SO WE SHOULD HAVE *
      *   RETRO TRIGGER. SEE 4400-RETRO-BILL-TRIGGER.                  *
      ******************************************************************
000100*Use Range operators wherever possible                           *
000100******************************************************************
GW    *  1/2004 WHEN CREATING/CHANGING A WBP REC, IF THERE IS A        *
GW    *  STOP DATE ON THE CORRESPONDING BEN REC, THE START-DATE IN     *
GW    *  THE WBP REC SHOULD BE THE BEN-STOP-DATE + 1, NOT THE BEN-     *
GW    *  START-DATE BECAUSE THIS IS THE DATE WB120 NEEDS TO PERFORM    *
GW    *  A RECALC.  THIS CHANGE IS ALSO GOING INTO BNPTBPD FOR         *
GW    *  PARTBEN RECS.                                                 *
      *================================================================*
      *  WBP MOD 8/2003                                                *
      *  UPDATE WBPBENTAG FOR UPDATES TO BENEFIT                       *
      *  THE TBLS HAVE NO FIELDS IN COMMON EXCEPT FOR THE KEYS, SO WE  *
      *  ONLY INTERESTED IN ADDS AND DELETES.                          *
      *  ON ADD, USE SAME DEFAULTS AS USED ON ZN31.2                   *
      *================================================================*
AI0030*  AI0030  07/24/03  ADD LOGIC FOR DL AND HL BENEFICIARIES       *
AI0010*  AI0010  03/24/03  MODIFY THE BENEFIT PLAN ELIGIBILITY COMMON  *
AI0010*                    MODULE TO UTILIZE THE EFFECTIVE DATE AND    *
AI0010*                    END DATE FIELDS IN DETERMINATION OF THE     *
AI0010*                    EMPLOYEES ELIGIBILITY FOR THE BENEFIT PLAN. *
AI0095*  AI0095  03/24/03  RETROACTIVE BENEFIT CHANGE MODIFICATION     *
      *================================================================*
000100* COVER-OPT Used as Coverage Option                              *
000100*                   Contribution Option                          *
000100*                   Cycles remaining  (Limits)                   *
000100*                   Nbr of Hours      (if VA)                    *
000100*                   Nbr of Bonds      (if SB)                    *
000100* COVER-AMT Used as Coverage Amount   (Flat Amt/ Supplimental)   *
000100*                   Coverage Amount   (Cov/Sal Contrib, type 4)  *
000100*                   Pay Period Amount (Limits)                   *
000100* PAY-RATE  Used as Annual Salary     (In Cover Mult/ Pct of Sal)*
000100*                   Annual Salary     (In Contr Cov/Sal)         *
000100*                   Ann Amt/Tot Cont  (In Contr Limits)          *
000100******************************************************************
000100******************************************************************
000200 2000-BNBEN-EDIT-TRAN            SECTION 41.
000300******************************************************************
000400 2000-START.  
000500
           INITIALIZE BNBEN-PASS-MSG-NBR
                      BNBEN-PASS-ERROR-CAT
                      CRT-ERROR-NBR
                      CRT-ERROR-CAT
                      CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3
                      CRT-ERR-VAR4
                      CRT-ERR-VAR5
                      BNBEN-SV-EMP
                      BNBEN-SV-PL-TP
                      BNBEN-SV-PL-CD.

           MOVE CRT-PROGRAM-CODE           TO BNBEN-HIPAA-DEFAULT-SW.

           IF (CRT-PROGRAM-CODE            = "BN531")
               MOVE "Y"                    TO BNREWS-USE-PGE-TBL
                                              BNREWS-USE-BWT-TBL
                                              BNREWS-USE-CVR-TBL
                                              BNREWS-USE-PRE-TBL.

           PERFORM 2100-EDIT-ACCESS
           THRU    2100-END.

           IF (ERROR-FOUND)
               GO TO 2000-BNBEN-SET-CAT.

           MOVE 1                          TO BNBEN-NEW-ADD-AT.
           MOVE BNBEN-NBR-LINES            TO BNBEN-LAST-DETAIL
                                              BNBEN-ORIG-NBR-LINES.
           SET ITS-NOT-DONE                TO TRUE.
           SET NOT-DTL-OVERFLOW            TO TRUE.

           PERFORM 2150-DO-FUTURE-ADDS
           THRU    2150-END
               UNTIL (ITS-DONE)
               OR    (ERROR-FOUND).

           IF (USE-NAVIGATE)
               PERFORM 2200-SET-NAVIGATE-TABLE
               THRU    2200-END

               PERFORM
                   VARYING BNBEN-I2 FROM 1 BY 1
                   UNTIL  (BNBEN-I2              > BNBEN-NBR-LINES)
                   OR     (BNBEN-NBR (BNBEN-I2)  = ZEROES)
                   OR     (ERROR-FOUND)

                   MOVE BNBEN-NBR (BNBEN-I2)     TO I1
                   PERFORM 2300-EDIT-DTL-TRAN
                   THRU    2300-END
               END-PERFORM
           ELSE
               PERFORM 2300-EDIT-DTL-TRAN
               THRU    2300-END
035600             VARYING I1 FROM 1 BY 1
035700             UNTIL  (I1 > BNBEN-NBR-LINES)
035800             OR     (ERROR-FOUND).
035900 
           PERFORM 3100-EDIT-SCREEN-BEN
           THRU    3100-END
035600         VARYING I1 FROM 1 BY 1
035700         UNTIL  (I1 > BNBEN-NBR-LINES)
035800         OR     (ERROR-FOUND).
035900
000800 2000-BNBEN-SET-CAT.
000810     IF  (ERROR-FOUND)
000820     AND (CRT-ERROR-CAT          = SPACES)
000830         MOVE "BNBEN"            TO CRT-ERROR-CAT.
000840
           GO TO 2000-END.

036200******************************************************************
       2100-EDIT-ACCESS.
036200******************************************************************

           MOVE BNBEN-COMPANY              TO DB-COMPANY.
           INITIALIZE DB-PROCESS-LEVEL.
           PERFORM 840-FIND-PRSSET1.
004200     IF (PRSYSTEM-NOTFOUND)
      ******** Company does not exist
004300         MOVE 152                    TO CRT-ERROR-NBR
004400         MOVE BNBEN-COMPANY-FN       TO CRT-FIELD-NBR
004500         GO TO 2100-END.
004600
186400     MOVE BNBEN-COMPANY              TO DB-COMPANY.
186500     PERFORM 840-FIND-BNCSET1.
004800     IF (BNCOMPANY-NOTFOUND)
      ******** Company is not setup in benefit system
004900         MOVE 153                    TO CRT-ERROR-NBR
005000         MOVE BNBEN-COMPANY-FN       TO CRT-FIELD-NBR
005100         GO TO 2100-END.
005200
039200     IF (BNBEN-ENROLLMENT-DATE       = ZEROES)
039300         MOVE WS-SYSTEM-DATE-YMD     TO BNBEN-ENROLLMENT-DATE.
039500
           INITIALIZE BNFRWS-EFR-TABLE
                      BNFRWS-EFR-TABLE-1
                      BNFRWS-TOTAL-AVAIL
                      BNBEN-C5-MAX-TABLE.

       2100-END.

153200******************************************************************
       2150-DO-FUTURE-ADDS.
153200******************************************************************

           PERFORM
               VARYING I1 FROM BNBEN-NEW-ADD-AT BY 1
               UNTIL  (I1                  > BNBEN-NBR-LINES)
               OR     (ERROR-FOUND)
               OR     (DTL-OVERFLOW)

               IF (BNBEN-LINE-FC (I1)      = "A")
                   PERFORM 2152-EDIT-FUTURE-ADDS
                   THRU    2152-END
               END-IF

           END-PERFORM.

           IF (ERROR-FOUND)
               GO TO 2150-END.

           IF (BNBEN-LAST-DETAIL           = BNBEN-NBR-LINES)
               SET ITS-DONE                TO TRUE
           ELSE
               MOVE BNBEN-LAST-DETAIL      TO BNBEN-NBR-LINES.

       2150-END.

153200******************************************************************
       2152-EDIT-FUTURE-ADDS.
153200******************************************************************

           MOVE BNBEN-COMPANY              TO DB-COMPANY.
           MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
           PERFORM 840-FIND-EMPSET1.
           IF (EMPLOYEE-NOTFOUND)
      ******** Employee does not exist
               MOVE 154                    TO CRT-ERROR-NBR
               MOVE BNBEN-EMPLOYEE-FN (I1) TO CRT-FIELD-NBR
               GO TO 2152-END.

           PERFORM 840-FIND-PEMSET1.
           IF (PAEMPLOYEE-NOTFOUND)
      ******** Paemployee does not exist
               MOVE 209                    TO CRT-ERROR-NBR
               MOVE BNBEN-EMPLOYEE-FN (I1) TO CRT-FIELD-NBR
               GO TO 2152-END.
 
           MOVE EMP-COMPANY                TO CRT-COMPANY.
           MOVE EMP-PROCESS-LEVEL          TO CRT-PROCESS-LEVEL.
           PERFORM 700-HR-EMP-SECURITY.
           IF (HRWS-EMP-SECURED)
      ******** Not authorised to access employee information
               MOVE 156                    TO CRT-ERROR-NBR
               MOVE BNBEN-EMPLOYEE-FN (I1) TO CRT-FIELD-NBR
               GO TO 2152-END.

           MOVE BNBEN-COMPANY              TO DB-COMPANY.
           MOVE BNBEN-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
           MOVE BNBEN-PLAN-CODE (I1)       TO DB-PLAN-CODE.
           PERFORM 840-FIND-PLNSET1.
           IF (PLAN-NOTFOUND)
               GO TO 2152-END.

           IF (USE-PGE-TBL)
           OR (USE-BWT-TBL)
           OR (USE-CVR-TBL)
           OR (USE-PRE-TBL)
               PERFORM 2153-CREATE-TABLES
               THRU    2153-END.

           IF (PLN-FLEX-PLAN               NOT = SPACES)
               GO TO 2152-END.

           INITIALIZE BNBEN-ADD-START-DATE.

J64465     IF (BNBEN-SKIP-ELIGIBILITY (I1) NOT = "Y")
               PERFORM 2154-CALC-START-DATE
               THRU    2154-END
J64465     END-IF.

           IF (NO-ERROR-FOUND)
               IF (PLN-COVERAGE-TYPE          = "2")
                   PERFORM 2156-CALL-BNRE-FOR-CVR
                   THRU    2156-END
               END-IF
               IF (PLN-CONTRIB-TYPE           NOT = "0" AND "X")
                   PERFORM 2158-CALL-BNRE-FOR-PRE
                   THRU    2158-END
               END-IF
J73344         IF (BNBEN-PRE-STOP-DATE (I1)      NOT = ZEROES)
J73344             MOVE BNBEN-PRE-STOP-DATE (I1) 
J73344                                      TO BNBEN-STOP-DATE (I1)   
J73344         END-IF
J73344         IF (BNBEN-PRE-DED-START-DATE (1)  NOT = ZEROES)
J73344             MOVE BNBEN-PRE-DED-START-DATE (1)
J73344                                      TO BNBEN-DED-START-DATE (1)
J73344         END-IF
J73344         IF (BNBEN-PRE-DED-STOP-DATE (1)   NOT = ZEROES)
J73344             MOVE BNBEN-PRE-DED-STOP-DATE (1)
J73344                                      TO BNBEN-DED-STOP-DATE (1)
J73344         END-IF
               IF   (BNBEN-ADD-START-DATE     NOT = ZEROES)
               AND ((BNBEN-ADD-START-DATE     > BNBEN-STOP-DATE (I1))
               AND  (BNBEN-STOP-DATE (I1)     NOT = ZEROES))
J40590         OR   (BNBEN-ADD-START-DATE     NOT = ZEROES)
J64398         AND ((BNBEN-ADD-START-DATE  <= BNBEN-DED-START-DATE (1))
J40590         AND  (BNBEN-DED-START-DATE (1) NOT = ZEROES))
J40590         OR   (BNBEN-ADD-START-DATE     NOT = ZEROES)
J64398         AND ((BNBEN-ADD-START-DATE  <= BNBEN-DED-STOP-DATE (1))
J40590         AND  (BNBEN-DED-STOP-DATE (1)  NOT = ZEROES))
                   GO TO 2152-END
               END-IF
               IF (BNBEN-ADD-START-DATE       NOT = ZEROES)
                   MOVE BNBEN-COMPANY         TO DB-COMPANY
                   MOVE BNBEN-EMPLOYEE (I1)   TO DB-EMPLOYEE
                   MOVE BNBEN-PLAN-TYPE (I1)  TO DB-PLAN-TYPE
                   MOVE BNBEN-PLAN-CODE (I1)  TO DB-PLAN-CODE
                   MOVE BNBEN-START-DATE (I1) TO DB-START-DATE
                   PERFORM 840-FIND-BENSET1
                   IF (BENEFIT-NOTFOUND)
                       IF (BNBEN-NBR-LINES    = BNBEN-LAST-DETAIL)
                           COMPUTE BNBEN-NEW-ADD-AT
                                              = BNBEN-LAST-DETAIL
                                              + 1 
                       END-IF
                       ADD 1                  TO BNBEN-LAST-DETAIL
                       IF (BNBEN-LAST-DETAIL  > BNBEN-MAX-DTL-LINES)
                           SET DTL-OVERFLOW   TO TRUE
                           MOVE BNBEN-MAX-DTL-LINES TO BNBEN-LAST-DETAIL
                                                       BNBEN-NBR-LINES
                           GO TO 2152-END
                       END-IF
                       PERFORM 2160-CREATE-NEW-ADD
                       THRU    2160-END.

       2152-END.

153200******************************************************************
       2153-CREATE-TABLES.
153200******************************************************************

           IF  (BNBEN-EMPLOYEE (I1)        NOT = BNBEN-SV-EE)
           AND (USE-PGE-TBL)
               INITIALIZE BNREWS-PGE-TBL
                          BNBEN-I3

               MOVE BNBEN-EMPLOYEE (I1)    TO BNBEN-SV-EE

               MOVE BNBEN-COMPANY          TO DB-COMPANY
               MOVE BNBEN-EMPLOYEE (I1)    TO DB-EMPLOYEE
               MOVE PGESET2-EMPLOYEE       TO WS-DB-BEG-RNG
               PERFORM 850-KFIND-BEGRNG-PGESET2
               PERFORM
                   UNTIL (PGEMPLOYEE-KNOTFOUND)
                   OR    (BNREWS-USE-PGE-TBL = SPACES)

                   ADD 1                   TO BNBEN-I3
                   IF (BNBEN-I3            <= BNREWS-MAX-TBL)
                       MOVE PGE-GROUP-NAME TO BNREWS-PGE-GRP (BNBEN-I3)
                   ELSE
      *--------------- This means that we do not have enough space in WS Table
      *--------------- so we need to process the conventional way
                       INITIALIZE BNREWS-USE-PGE-TBL
                   END-IF
                   PERFORM 860-KFIND-NXTRNG-PGESET2
               END-PERFORM.

           IF (BNBEN-PLAN-TYPE (I1)        NOT = BNBEN-SV-PLT)
           OR (BNBEN-PLAN-CODE (I1)        NOT = BNBEN-SV-PLC)
               MOVE BNBEN-PLAN-TYPE (I1)   TO BNBEN-SV-PLT
               MOVE BNBEN-PLAN-CODE (I1)   TO BNBEN-SV-PLC
           ELSE
               GO TO 2153-END.

           IF (USE-BWT-TBL)
               INITIALIZE BNREWS-BWT-TBL
                          BNBEN-I3

               MOVE BNBEN-COMPANY          TO DB-COMPANY
               INITIALIZE DB-RULE-TYPE
               MOVE BNBEN-PLAN-TYPE (I1)   TO DB-PLAN-TYPE
               MOVE BNBEN-PLAN-CODE (I1)   TO DB-PLAN-CODE
               MOVE BWTSET2-PLAN-CODE      TO WS-DB-BEG-RNG
               PERFORM 850-KFIND-BEGRNG-BWTSET2
               PERFORM
                   UNTIL (BNWAIT-KNOTFOUND)
                   OR    (BNREWS-USE-BWT-TBL = SPACES)

                   ADD 1                   TO BNBEN-I3
                   IF (BNBEN-I3            <= BNREWS-MAX-TBL)
                       MOVE BWT-START-DATE TO BNREWS-BWT-STDT (BNBEN-I3)
                       MOVE BWT-GROUP-NAME TO BNREWS-BWT-GRP (BNBEN-I3)
                   ELSE
      *--------------- This means that we do not have enough space in WS Table
      *--------------- so we need to process the conventional way
                       INITIALIZE BNREWS-USE-BWT-TBL
                   END-IF
                   PERFORM 860-KFIND-NXTRNG-BWTSET2
               END-PERFORM
               IF (USE-BWT-TBL)
                   MOVE BNBEN-I3           TO BNREWS-LAST-BWT.

           IF  (USE-CVR-TBL)
           AND (PLN-COVERAGE-TYPE          = "2")
               INITIALIZE BNREWS-CVR-TBL
                          BNBEN-I3

               MOVE BNBEN-COMPANY          TO DB-COMPANY
               MOVE BNBEN-PLAN-TYPE (I1)   TO DB-PLAN-TYPE
               MOVE BNBEN-PLAN-CODE (I1)   TO DB-PLAN-CODE
               MOVE "E"                    TO DB-COVER-TYPE
               MOVE CVRSET2-COVER-TYPE     TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-CVRSET2
               PERFORM
                   UNTIL (BNCOVERAGE-NOTFOUND)
                   OR    (BNREWS-USE-CVR-TBL = SPACES)

                   ADD 1                   TO BNBEN-I3
                   IF (BNBEN-I3            <= BNREWS-MAX-TBL)
                       MOVE CVR-START-DATE TO BNREWS-CVR-STDT (BNBEN-I3)
                       MOVE CVR-GROUP-NAME TO BNREWS-CVR-GRP (BNBEN-I3)
                       MOVE CVR-STATUS     TO BNREWS-CVR-STS (BNBEN-I3)
                   ELSE
      *--------------- This means that we do not have enough space in WS Table
      *--------------- so we need to process the conventional way
                       INITIALIZE BNREWS-USE-CVR-TBL
                   END-IF
                   PERFORM 860-FIND-NXTRNG-CVRSET2
               END-PERFORM
               IF (USE-CVR-TBL)
                   MOVE BNBEN-I3           TO BNREWS-LAST-CVR.

           IF  (USE-PRE-TBL)
           AND (PLN-CONTRIB-TYPE           NOT = "0" AND "X")
               INITIALIZE BNREWS-PRE-TBL
                          BNBEN-I3

               MOVE BNBEN-COMPANY          TO DB-COMPANY
               MOVE BNBEN-PLAN-TYPE (I1)   TO DB-PLAN-TYPE
               MOVE BNBEN-PLAN-CODE (I1)   TO DB-PLAN-CODE
               MOVE "E"                    TO DB-COVER-TYPE
               MOVE PRESET3-COVER-TYPE     TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PRESET3
               PERFORM
                   UNTIL (PREMIUM-NOTFOUND)
                   OR    (BNREWS-USE-PRE-TBL = SPACES)

                   ADD 1                   TO BNBEN-I3
                   IF (BNBEN-I3            <= BNREWS-MAX-TBL)
                       MOVE PRE-START-DATE TO BNREWS-PRE-STDT (BNBEN-I3)
                       MOVE PRE-GROUP-NAME TO BNREWS-PRE-GRP (BNBEN-I3)
                       MOVE PRE-STATUS     TO BNREWS-PRE-STS (BNBEN-I3)
                   ELSE
      *--------------- This means that we do not have enough space in WS Table
      *--------------- so we need to process the conventional way
                       INITIALIZE BNREWS-USE-PRE-TBL
                   END-IF
                   PERFORM 860-FIND-NXTRNG-PRESET3
               END-PERFORM
               IF (USE-PRE-TBL)
                   MOVE BNBEN-I3           TO BNREWS-LAST-PRE.

       2153-END.

153200******************************************************************
       2154-CALC-START-DATE.
153200******************************************************************

           MOVE BNBEN-COMPANY              TO BNEDWS-COMPANY.
           MOVE BNBEN-PLAN-TYPE (I1)       TO BNEDWS-PLAN-TYPE.
           MOVE BNBEN-PLAN-CODE (I1)       TO BNEDWS-PLAN-CODE.
           MOVE BNBEN-EMPLOYEE (I1)        TO BNEDWS-EMPLOYEE.
           IF (BNBEN-START-DATE (I1)       = ZEROES)
               MOVE BNBEN-ENROLLMENT-DATE  TO BNEDWS-AS-OF-DATE
               INITIALIZE BNEDWS-START-DATE
           ELSE
               MOVE BNBEN-START-DATE (I1)  TO BNEDWS-AS-OF-DATE 
                                              BNEDWS-START-DATE.
117816     IF (BNBEN-PROC-NON-FLEX)
J41123         INITIALIZE BNEDWS-FROM-MAGIC-SW 
117816     END-IF.
           PERFORM 5000-ELIGIBILITY-DATE-CALC-70.
           IF (ERROR-FOUND)
041300         MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
               GO TO 2154-END.

039200     IF (BNBEN-START-DATE (I1)       NOT = ZEROES)
               IF (RE-ENROLLMENT)
      ******** RESET ENTRY POINTS FILLED BY BNED ROUTINE TO RE-ENROLLMENT
      ******** ENTRY POINTS
                   MOVE BWT-SUB-ENT-POINT      TO BNEDWS-INIT-ENT-POINT
                   PERFORM
                       VARYING I2 FROM 1 BY 1
                       UNTIL   (I2                  > 12)
                       OR      (BWT-SUB-POINTS (I2) = ZEROES)

                       MOVE BWT-SUB-POINTS (I2)
                                           TO BNEDWS-ENTRY-MMDD (I2)
                       MOVE I2             TO BNEDWS-LAST-EP-INDEX
                   END-PERFORM
               END-IF
               PERFORM 2310-EDIT-START-DATE
               THRU    2310-END
               IF (ERROR-FOUND)
041300             MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
                   GO TO 2154-END.

039200     IF  (BNBEN-START-DATE (I1)       = ZEROES)
P84430     OR  ((BNBEN-START-DATE (I1)      < BNEDWS-ELIGIBILITY-DATE)
P84430     AND (BNEDWS-ELIGIBILITY-DATE     > PLN-START-DATE) 
P84430     AND (CRT-PROGRAM-CODE            NOT = "BN31")
P84430     AND (CRT-PROGRAM-CODE            NOT = "BN531")
033536     AND (CRT-PROGRAM-CODE            NOT = "BN32")
J01777     AND (CRT-PROGRAM-CODE            NOT = "BN100"))
039300         MOVE BNEDWS-ELIGIBILITY-DATE
039300                                     TO BNBEN-START-DATE (I1).

       2154-END.

153200******************************************************************
       2156-CALL-BNRE-FOR-CVR.
153200******************************************************************

030700     MOVE BNBEN-COMPANY          TO BNREWS-COMPANY.
030800     MOVE BNBEN-PLAN-TYPE (I1)   TO BNREWS-PLAN-TYPE.
030900     MOVE BNBEN-PLAN-CODE (I1)   TO BNREWS-PLAN-CODE.
031000     MOVE "E"                    TO BNREWS-COVER-TYPE.
031100     MOVE BNBEN-EMPLOYEE (I1)    TO BNREWS-EMPLOYEE.
031200     MOVE BNBEN-START-DATE (I1)  TO BNREWS-AS-OF-DATE.
031300     MOVE "CVR"                  TO BNREWS-FILE-PREFIX.
031400     PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70.
031500     IF (ERROR-FOUND)
031600         GO TO 2156-END.
031700
           MOVE CVR-START-DATE         TO BNBEN-COV-UPD-DT (I1).
           MOVE CVR-GROUP-NAME         TO BNBEN-COV-GROUP  (I1).

           PERFORM 5000-FIND-NEXT-BNCOVERAGE.

           MOVE BNREWS-ADD-START-DATE  TO BNBEN-ADD-START-DATE.

       2156-END.

153200******************************************************************
       2158-CALL-BNRE-FOR-PRE.
153200******************************************************************

030700     MOVE BNBEN-COMPANY          TO BNREWS-COMPANY.
030800     MOVE BNBEN-PLAN-TYPE (I1)   TO BNREWS-PLAN-TYPE.
030900     MOVE BNBEN-PLAN-CODE (I1)   TO BNREWS-PLAN-CODE.
031000     MOVE "E"                    TO BNREWS-COVER-TYPE.
031100     MOVE BNBEN-EMPLOYEE (I1)    TO BNREWS-EMPLOYEE.
031200     MOVE BNBEN-START-DATE (I1)  TO BNREWS-AS-OF-DATE.
031300     MOVE "PRE"                  TO BNREWS-FILE-PREFIX.
031400     PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70.
031500     IF (ERROR-FOUND)
031600         GO TO 2158-END.
031700
           PERFORM 5000-FIND-NEXT-PREMIUM.

           IF  (BNBEN-ADD-START-DATE   = ZEROES)
           OR  ((BNREWS-ADD-START-DATE < BNBEN-ADD-START-DATE)
           AND  (BNREWS-ADD-START-DATE NOT = ZEROES))
               MOVE BNREWS-ADD-START-DATE TO BNBEN-ADD-START-DATE.

       2158-END.

153200******************************************************************
       2160-CREATE-NEW-ADD.
153200******************************************************************

           MOVE BNBEN-LAST-DETAIL    TO I9.

           MOVE BNBEN-ADD-START-DATE TO WSDR-FR-DATE.
           PERFORM 900-DATE-TO-JULIAN.
           SUBTRACT 1                FROM WSDR-JULIAN-DAYS.
           PERFORM 900-JULIAN-TO-DATE.
           MOVE WSDR-FR-DATE         TO BNBEN-STOP-DATE (I1).

           INITIALIZE BNBEN-BENEFIT-DETAIL (I9).
      
           PERFORM 2162-MOVE-TO-BNBEN
           THRU    2162-END.

       2160-END.

006960******************************************************************
       2162-MOVE-TO-BNBEN.
006960******************************************************************

           MOVE "A"                        TO BNBEN-LINE-FC (I9).

           MOVE BNBEN-EMPLOYEE (I1)        TO BNBEN-EMPLOYEE (I9).

           MOVE BNBEN-PLAN-TYPE (I1)       TO BNBEN-PLAN-TYPE (I9).
           MOVE BNBEN-PLAN-CODE (I1)       TO BNBEN-PLAN-CODE (I9).

           MOVE BNBEN-ADD-START-DATE       TO BNBEN-START-DATE (I9).

           MOVE BNBEN-COVER-OPT (I1)       TO BNBEN-COVER-OPT (I9).
           MOVE BNBEN-COVER-OPT (I1)       TO BNBEN-COVER-OPT (I9).
           MOVE BNBEN-COVER-AMT (I1)       TO BNBEN-COVER-AMT (I9).
           IF (PLN-COVERAGE-TYPE           = "2")
               MOVE "E"                    TO DB-COVER-TYPE
               MOVE BNBEN-COV-UPD-DT (I1)  TO DB-START-DATE
               MOVE BNBEN-COV-GROUP  (I1)  TO DB-GROUP-NAME
               PERFORM 840-FIND-CVRSET1
               IF (CVR-CALC-TYPE           NOT = "S")
                   INITIALIZE BNBEN-COVER-AMT (I9).

           MOVE BNBEN-PAY-RATE (I1)        TO BNBEN-PAY-RATE (I9).

           MOVE BNBEN-MULT-SALARY (I1)     TO BNBEN-MULT-SALARY (I9).

           MOVE BNBEN-EMP-PRE-CONT (I1)    TO BNBEN-EMP-PRE-CONT (I9).
           MOVE BNBEN-EMP-AFT-CONT (I1)    TO BNBEN-EMP-AFT-CONT (I9).
           MOVE BNBEN-CMP-FLX-CONT (I1)    TO BNBEN-CMP-FLX-CONT (I9).
           MOVE BNBEN-COMP-CONT (I1)       TO BNBEN-COMP-CONT (I9).

           IF (PLN-CONTRIB-TYPE            NOT = "5" AND "6" AND "7")
               INITIALIZE BNBEN-PAY-RATE (I9)
                          BNBEN-EMP-PRE-CONT (I9)
                          BNBEN-EMP-AFT-CONT (I9)
                          BNBEN-CMP-FLX-CONT (I9)
                          BNBEN-COMP-CONT (I9).

           MOVE BNBEN-PCT-AMT-FLAG (I1)    TO BNBEN-PCT-AMT-FLAG (I9).
           MOVE BNBEN-SMOKER-FLAG (I1)     TO BNBEN-SMOKER-FLAG (I9).
J34418     MOVE BNBEN-PEND-EVIDENCE (I1)   TO BNBEN-PEND-EVIDENCE (I9).
           MOVE BNBEN-PRE-AFT-FLAG (I1)    TO BNBEN-PRE-AFT-FLAG (I9).

           MOVE "N"                        TO BNBEN-CREATE-TRANS (I9).
           INITIALIZE BNBEN-REASON (I9)
                      BNBEN-MEMBER-ID (I9).

J73344     IF  (BNBEN-PRE-STOP-DATE (I1)      NOT = ZEROES)
J73344         MOVE BNBEN-PRE-STOP-DATE (I1)
J73344                                     TO BNBEN-STOP-DATE (I9)
J73344     END-IF.
J73344     IF  (BNBEN-STOP-DATE (I9)              = ZEROES)
J73344     AND (BNBEN-PRE-STOP-DATE (1)       NOT = ZEROES)
J73344         MOVE BNBEN-PRE-STOP-DATE (1)   
J73344                                     TO BNBEN-STOP-DATE (I9)
J73344     END-IF.

       2162-END.

153200******************************************************************
       2200-SET-NAVIGATE-TABLE.
153200******************************************************************

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > BNBEN-NBR-LINES)

               INITIALIZE BNBEN-NAV-TABLE (I1)
           END-PERFORM.

           MOVE BNBEN-COMPANY          TO DB-COMPANY.
           INITIALIZE DB-PROCESS-LEVEL.
           MOVE BNBEN-EMPLOYEE (1)     TO DB-EMPLOYEE.
           PERFORM 840-FIND-NAVSET1.
           IF (NAVIGATE-FOUND)
               GO TO 2200-CONTINUE.  

           MOVE BNBEN-COMPANY          TO DB-COMPANY.
           MOVE EMP-PROCESS-LEVEL      TO DB-PROCESS-LEVEL.
P62300*    INITIALIZE DB-EMPLOYEE.
P62300     MOVE NAVSET1-PROCESS-LEVEL  TO WS-DB-BEG-RNG.
P62300*    PERFORM 850-FIND-NLT-NAVSET1.
P62300     PERFORM 850-FIND-BEGRNG-NAVSET1.
           IF  (NAVIGATE-FOUND)
P62300*    AND (NAV-COMPANY            = BNBEN-COMPANY)
P62300*    AND (NAV-PROCESS-LEVEL      = EMP-PROCESS-LEVEL)
               GO TO 2200-CONTINUE.

           MOVE BNBEN-COMPANY          TO DB-COMPANY.
P62300*    INITIALIZE DB-PROCESS-LEVEL
P62300*               DB-EMPLOYEE.
P62300     MOVE NAVSET1-COMPANY        TO WS-DB-BEG-RNG.
P62300*    PERFORM 850-FIND-NLT-NAVSET1.
P62300     PERFORM 850-FIND-BEGRNG-NAVSET1.
           IF  (NAVIGATE-FOUND)
P62300*    AND (NAV-COMPANY            = BNBEN-COMPANY)
           AND (NAV-PROCESS-LEVEL      = SPACES)
           AND (NAV-EMPLOYEE           = ZEROES)
               GO TO 2200-CONTINUE.

      *
      **** IF NAVIGATE RECORD NOT FOUND PROCESS IN ORDER ENTERED
      *
           INITIALIZE I3.

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > BNBEN-NBR-LINES)

               IF (BNBEN-LINE-FC (I1)  NOT = SPACES)
                   ADD 1               TO I3
                   MOVE I1             TO BNBEN-NBR (I3)
               END-IF
           END-PERFORM.

           GO TO 2200-END.

       2200-CONTINUE.

           INITIALIZE I3.

           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 11)

               PERFORM
                   VARYING I2 FROM 1 BY 1
                   UNTIL  (I2 > BNBEN-NBR-LINES)

                   IF  (BNBEN-LINE-FC (I2)   NOT = SPACES)
                   AND (BNBEN-PLAN-TYPE (I2) = NAV-PLAN-TYPE (I1))
                       ADD 1               TO I3
                       MOVE I2             TO BNBEN-NBR (I3)
                   END-IF
               END-PERFORM
           END-PERFORM.

       2200-END.

036200******************************************************************
036300 2300-EDIT-DTL-TRAN.
036400******************************************************************
036500
036600     IF (BNBEN-LINE-FC (I1)          = SPACES)
036700         GO TO 2300-END.
036800
005500     MOVE BNBEN-COMPANY              TO DB-COMPANY.
005600     MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
188900     PERFORM 840-FIND-EMPSET1.
006200     IF (EMPLOYEE-NOTFOUND)
      ******** Employee does not exist
006300         MOVE 154                    TO CRT-ERROR-NBR
006400         MOVE BNBEN-EMPLOYEE-FN (I1) TO CRT-FIELD-NBR
006500         GO TO 2300-END.

115100     PERFORM 840-FIND-PEMSET1.
006200     IF (PAEMPLOYEE-NOTFOUND)
      ******** Paemployee does not exist
006300         MOVE 209                    TO CRT-ERROR-NBR
006400         MOVE BNBEN-EMPLOYEE-FN (I1) TO CRT-FIELD-NBR
006500         GO TO 2300-END.

           MOVE EMP-COMPANY                TO CRT-COMPANY.
           MOVE EMP-PROCESS-LEVEL          TO CRT-PROCESS-LEVEL.
006900     PERFORM 700-HR-EMP-SECURITY.
007000     IF (HRWS-EMP-SECURED)
      ******** Not authorised to access employee information
007100         MOVE 156                    TO CRT-ERROR-NBR
007200         MOVE BNBEN-EMPLOYEE-FN (I1) TO CRT-FIELD-NBR
007300         GO TO 2300-END.
007400
040200     MOVE BNBEN-COMPANY              TO DB-COMPANY.
040300     MOVE BNBEN-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
040600     MOVE BNBEN-PLAN-CODE (I1)       TO DB-PLAN-CODE.
040800     PERFORM 840-FIND-PLNSET1.
041100     IF (PLAN-NOTFOUND)
      ******** Plan does not exist
041200         MOVE 103                      TO CRT-ERROR-NBR
041300         MOVE BNBEN-PLAN-CODE-FN (I1)  TO CRT-FIELD-NBR
041400         GO TO 2300-END.

           IF (USE-PGE-TBL)
           OR (USE-BWT-TBL)
           OR (USE-CVR-TBL)
           OR (USE-PRE-TBL)
               PERFORM 2153-CREATE-TABLES
               THRU    2153-END.

           IF (PLN-FLEX-PLAN               NOT = SPACES)
               MOVE PLN-COMPANY            TO DB-COMPANY
               MOVE PLN-FLEX-PLAN          TO DB-FLEX-PLAN
               PERFORM 840-FIND-FLPSET1
               IF (FLEXPLAN-NOTFOUND)
      ********   Flex Plan does not exist
                   MOVE 122                     TO CRT-ERROR-NBR
                   MOVE BNBEN-PLAN-CODE-FN (I1) TO CRT-FIELD-NBR
                   GO TO 2300-END.

           IF  (BNBEN-LINE-FC (I1)         NOT = "A")
           AND (BNBEN-PT-START-DATE (I1)   NOT = BNBEN-START-DATE (I1))
      ******** Cannot change start date
               MOVE 210                    TO CRT-ERROR-NBR
               MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
               GO TO 2300-END.

           IF  (BNBEN-LINE-FC (I1)         = "A")
           AND (CRT-PROGRAM-CODE           NOT = "BN105")
           AND (CRT-PROGRAM-CODE           NOT = "BN102")
           AND (BNBEN-SKIP-ELIGIBILITY (I1) NOT = "Y")
               PERFORM 2305-EDIT-PLAN-ELIGIBILITY
               THRU    2305-END
               IF (ERROR-FOUND)
                   GO TO 2300-END.

040200     MOVE BNBEN-COMPANY              TO DB-COMPANY.
040400     MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
040500     MOVE BNBEN-START-DATE (I1)      TO DB-START-DATE.
040300     MOVE BNBEN-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
040600     MOVE BNBEN-PLAN-CODE (I1)       TO DB-PLAN-CODE.
040900     PERFORM 840-FIND-BENSET1.
041600     IF  (BENEFIT-NOTFOUND)
041700     AND (BNBEN-LINE-FC (I1)         = "C" OR "D" OR "S")
      ******** Benefit does not exist
041800         MOVE 109                    TO CRT-ERROR-NBR
041900         MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
042000         GO TO 2300-END.

042200     IF  (BENEFIT-FOUND)
042300     AND (BNBEN-LINE-FC (I1)         = "A")
      ******** Benefit already exist
042400         MOVE 110                    TO CRT-ERROR-NBR
042500         MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
042600         GO TO 2300-END.
042700
           IF  (BENEFIT-FOUND)
P12051     AND (CRT-PROGRAM-CODE    NOT = "BN531")
               MOVE BEN-YTD-CONT           TO BNBEN-YTD-CONT (I1)
P77247         MOVE BEN-EMP-YTD-CONT       TO BNBEN-EMP-YTD-CONT (I1).

           IF (PLN-FLEX-PLAN               = SPACES)
               MOVE "N"                    TO BNBEN-FLEX-FLAG (I1)
                                              BNBEN-SPEND-ONLY (I1)
           ELSE
               MOVE BNBEN-COMPANY          TO DB-COMPANY
               MOVE PLN-FLEX-PLAN          TO DB-FLEX-PLAN
               PERFORM 840-FIND-FLPSET1
041100         IF (FLEXPLAN-NOTFOUND)
      ************ Flex plan does not exist
041200             MOVE 122                    TO CRT-ERROR-NBR
041300             MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
041400             GO TO 2300-END
               END-IF
               MOVE FLP-SPEND-ONLY         TO BNBEN-SPEND-ONLY (I1)
               IF (BNBEN-SPEND-ONLY (I1)   = "Y")
                   MOVE "N"                TO BNBEN-FLEX-FLAG (I1)
               ELSE
                   MOVE "Y"                TO BNBEN-FLEX-FLAG (I1).

           IF (BNBEN-SPEND-ONLY (I1)       = "Y")
               PERFORM 2330-GET-SPEND-ONLY-START-STOP
               THRU    2330-END.

           IF (BNBEN-FLEX-FLAG (I1)        = "Y")
               PERFORM 2320-SET-EFR-TABLE
               THRU    2320-END.

           IF (ERROR-FOUND)
               GO TO 2300-END.

           MOVE WS-FALSE               TO BNBEN-TRIGGER-ENABLED-SW (I1).
           IF (PLN-PLAN-NAME           NOT = SPACES)
J13588*        MOVE BNBEN-COMPANY      TO EDCDWS-COMPANY
J13588*        MOVE "LP"               TO EDCDWS-SYSTEM
J13588*        PERFORM 6000-IS-SYSTEM-TRIGGER-ENABLED
J13588*        IF (EDCDWS-TRIGGER-ENABLED)
                   MOVE WS-TRUE        TO BNBEN-TRIGGER-ENABLED-SW (I1).

VILAS      IF (BNBEN-LINE-FC (I1)          = "A" OR "C")
042900         PERFORM 2400-EDIT-DTL-DATA
043000         THRU    2400-END.
043100
P86476     IF (ERROR-FOUND)
P86476         GO TO 2300-END.
P86476
P67321     IF  (BNBEN-LINE-FC (I1)         = "S")
P67321         MOVE BEN-PREM-UPD-DT        TO BNBEN-PREM-UPD-DT (I1)
P67321         MOVE BEN-PREM-GROUP         TO BNBEN-PREM-GROUP (I1)
P67321         PERFORM 5500-GET-COMP-CONT-SW
P71353         MOVE BNBEN-START-DATE (I1)  TO DB-START-DATE.

043200     IF (BNBEN-LINE-FC (I1)          = "D")
043300         PERFORM 2600-EDIT-DTL-DELETE
043400         THRU    2600-END.
043500
043200     IF  (BNBEN-LINE-FC (I1)     = "S")
P67732         IF  (BNBEN-MULT-SALARY (I1) = ZEROES)
P67732         AND (PLN-COVERAGE-TYPE      = "2")
P67732             IF (BENEFIT-NOTFOUND)
P67732                 MOVE BNBEN-COMPANY          TO DB-COMPANY
P67732                 MOVE BNBEN-EMPLOYEE (I1)    TO DB-EMPLOYEE
P67732                 MOVE BNBEN-START-DATE (I1)  TO DB-START-DATE
P67732                 MOVE BNBEN-PLAN-TYPE (I1)   TO DB-PLAN-TYPE
P67732                 MOVE BNBEN-PLAN-CODE (I1)   TO DB-PLAN-CODE
P67732                 PERFORM 840-FIND-BENSET1
P67732             END-IF
P67732             MOVE "E"                        TO DB-COVER-TYPE
P67732             MOVE BEN-COV-GROUP              TO DB-GROUP-NAME
P67732             MOVE CVRSET4-GROUP-NAME         TO WS-DB-BEG-RNG
P67732             PERFORM 850-FIND-BEGRNG-CVRSET4
P67732             IF  (BNCOVERAGE-FOUND)
P67732             AND (CVR-CALC-TYPE = "N")
P67732                 MOVE BEN-MULTIPLE       TO BNBEN-MULT-SALARY (I1)
P67732             END-IF
P67732         END-IF
043300         PERFORM 2800-EDIT-DTL-STOP
043400         THRU    2800-END.
043500
043600 2300-END.     

153200******************************************************************
       2305-EDIT-PLAN-ELIGIBILITY.
153200******************************************************************

      *
      **** CALL PLAN ELIGIBILITY SECTION WHICH VALIDATES ONLY
      **** GROUP & ZIP CODE
      *
           MOVE BNBEN-COMPANY              TO BNPEWS-COMPANY.
           MOVE BNBEN-PLAN-TYPE (I1)       TO BNPEWS-PLAN-TYPE.
           MOVE BNBEN-PLAN-CODE (I1)       TO BNPEWS-PLAN-CODE.
           MOVE BNBEN-EMPLOYEE (I1)        TO BNPEWS-EMPLOYEE.
           MOVE "E"                        TO BNPEWS-COVER-TYPE.
AI0010     MOVE BNBEN-ENROLLMENT-DATE      TO BNPEWS-START-DATE.           
           IF (BNBEN-SKIP-GROUP-N-ZIP      NOT = "Y")
               PERFORM 5000-EDIT-GROUP-N-ZIP-70
               IF (ERROR-FOUND)
041300             MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
                   GO TO 2305-END.

      *
      **** CALCULATE ELIGIBILITY DATE
      *
           MOVE BNBEN-COMPANY              TO BNEDWS-COMPANY.
           MOVE BNBEN-PLAN-TYPE (I1)       TO BNEDWS-PLAN-TYPE.
           MOVE BNBEN-PLAN-CODE (I1)       TO BNEDWS-PLAN-CODE.
           MOVE BNBEN-EMPLOYEE (I1)        TO BNEDWS-EMPLOYEE.
           IF (BNBEN-START-DATE (I1)       = ZEROES)
               MOVE BNBEN-ENROLLMENT-DATE  TO BNEDWS-AS-OF-DATE
               INITIALIZE BNEDWS-START-DATE
           ELSE
               MOVE BNBEN-START-DATE (I1)  TO BNEDWS-AS-OF-DATE 
                                              BNEDWS-START-DATE.
117816     IF (BNBEN-PROC-NON-FLEX)
J41123         INITIALIZE BNEDWS-FROM-MAGIC-SW 
117816     END-IF.
           PERFORM 5000-ELIGIBILITY-DATE-CALC-70.
           IF (ERROR-FOUND)
041300         MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
               GO TO 2305-END.

039200     IF  (BNBEN-START-DATE (I1)      NOT = ZEROES)
           AND (I1                         <= BNBEN-ORIG-NBR-LINES)
               PERFORM 2310-EDIT-START-DATE
               THRU    2310-END
               IF (ERROR-FOUND)
041300             MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
                   GO TO 2305-END.

039200     IF (BNBEN-START-DATE (I1)       = ZEROES)
039300         MOVE BNEDWS-ELIGIBILITY-DATE
039300                                     TO BNBEN-START-DATE (I1).

      *
      **** CALL PLAN ELIGIBILITY SECTION WHICH VALIDATES START DATE
      *
           MOVE BNBEN-START-DATE (I1)      TO BNPEWS-START-DATE.
           PERFORM 5000-EDIT-PLAN-START-DATE-70.
           IF (ERROR-FOUND)
041300         MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
               GO TO 2305-END.

           MOVE BNEDWS-BWT-START-DATE      TO BNBEN-ELIG-UPD-DT (I1).
           MOVE BNEDWS-BWT-GROUP-NAME      TO BNBEN-ELIG-GROUP (I1).
J17193     MOVE BNEDWS-FROM-DATE           TO BNBEN-FROM-DATE (I1).
J17193     MOVE BNEDWS-ENROLLMENT-SW       TO BNBEN-ENROLLMENT-SW (I1).

       2305-END.

153200******************************************************************
       2310-EDIT-START-DATE.
153200******************************************************************

           IF (BNEDWS-INIT-ENT-POINT   = "1" OR "2")
               PERFORM 2312-USE-PLAN-ENTRY-POINTS
               THRU    2312-END
           ELSE
           IF (BNEDWS-INIT-ENT-POINT   = "3" OR "4")
               PERFORM 2314-USE-PAY-PERIOD-DATES
               THRU    2314-END
           ELSE
           IF (BNEDWS-INIT-ENT-POINT   = "5" OR "6")
               PERFORM 2316-USE-WORK-PERIOD-DATES
               THRU    2316-END.

       2310-END.

153200******************************************************************
       2312-USE-PLAN-ENTRY-POINTS.
153200******************************************************************

           MOVE BNBEN-START-DATE (I1)      TO BNBEN-START-DATE-CALC.

           PERFORM
               VARYING BNBEN-I1 FROM 1 BY 1
               UNTIL  (BNBEN-I1            > BNEDWS-LAST-EP-INDEX)
               OR     (BNEDWS-ENTRY-MMDD (BNBEN-I1)
                                           = BNBEN-START-DATE-MMDD)

               CONTINUE
           END-PERFORM.

           IF (BNBEN-I1                    > BNEDWS-LAST-EP-INDEX)
      ******** Start date {0} must match entry point
037600         MOVE 199                    TO CRT-ERROR-NBR
P38260         MOVE BNBEN-START-DATE (I1)  TO HRWS-DATE-FIELD
P38260         INITIALIZE HRWS-DATE-8-FIELD
P38260         PERFORM 781-HR-FORMAT-DATE-FIELD
P38260         MOVE HRWS-VALUE             TO CRT-ERR-VAR1
037700         MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
037800         GO TO 2312-END.

       2312-END.

153200******************************************************************
       2314-USE-PAY-PERIOD-DATES.
153200******************************************************************

           PERFORM 2318-EDIT-PAY-PLAN
           THRU    2318-END.

           MOVE BNBEN-COMPANY              TO DB-COMPANY.
           MOVE PRO-PLAN-CODE              TO DB-PLAN-CODE.
           MOVE PRO-EFFECT-DATE            TO DB-EFFECT-DATE.
           MOVE PRYSET1-EFFECT-DATE        TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PRYSET1.

           PERFORM
               UNTIL (PROTPAYPRD-NOTFOUND)
               OR    (PRY-PAY-START-DATE     = BNBEN-START-DATE (I1))

               PERFORM 860-FIND-NXTRNG-PRYSET1
           END-PERFORM.

           IF (PROTPAYPRD-NOTFOUND)
      ******** Start date must match pay period start date
037600         MOVE 203                    TO CRT-ERROR-NBR
037700         MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
037800         GO TO 2314-END.

       2314-END.

153200******************************************************************
       2316-USE-WORK-PERIOD-DATES.
153200******************************************************************

           PERFORM 2318-EDIT-PAY-PLAN
           THRU    2318-END.

           MOVE BNBEN-COMPANY              TO DB-COMPANY.
           MOVE PRO-PLAN-CODE              TO DB-PLAN-CODE.
           MOVE PRO-EFFECT-DATE            TO DB-EFFECT-DATE.
           MOVE PRWSET1-EFFECT-DATE        TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PRWSET1.

           PERFORM
               UNTIL (PROTWRKPRD-NOTFOUND)
               OR    (PRW-WRK-START-DATE     = BNBEN-START-DATE (I1))

               PERFORM 860-FIND-NXTRNG-PRWSET1
           END-PERFORM.

           IF (PROTWRKPRD-NOTFOUND)
      ******** Start date must match work period start date
037600         MOVE 204                    TO CRT-ERROR-NBR
037700         MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
037800         GO TO 2316-END.

       2316-END.

153200******************************************************************
       2318-EDIT-PAY-PLAN.
153200******************************************************************

FAK        IF (EMP-OT-PLAN-CODE        = SPACES)
               MOVE BNBEN-COMPANY          TO DB-COMPANY 
009821         MOVE EMP-PROCESS-LEVEL      TO DB-PROCESS-LEVEL
009822         PERFORM 840-FIND-PRSSET1
               IF (PRS-OT-PLAN-CODE (EMP-PAY-FREQUENCY) NOT = SPACES)
009823             MOVE PRS-OT-PLAN-CODE (EMP-PAY-FREQUENCY)
009824                                     TO DB-PLAN-CODE
009825         ELSE                             
009821             INITIALIZE DB-PROCESS-LEVEL
009822             PERFORM 840-FIND-PRSSET1
                   IF (PRS-OT-PLAN-CODE (EMP-PAY-FREQUENCY) = SPACES)
000500                 MOVE 212                  TO CRT-ERROR-NBR
000490                 MOVE BNBEN-EMPLOYEE (I1)  TO CRT-ERR-VAR1
000510                 GO TO 2318-END
000510             ELSE
                       MOVE PRS-OT-PLAN-CODE (EMP-PAY-FREQUENCY) 
                                           TO DB-PLAN-CODE
           ELSE                                      
               MOVE EMP-OT-PLAN-CODE       TO DB-PLAN-CODE.

           MOVE BNBEN-COMPANY            TO DB-COMPANY.
           MOVE PROSET2-PLAN-CODE        TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PROSET2.

           PERFORM
               UNTIL (PROVERTIME-NOTFOUND)
P72978*         OR    (PRO-EFFECT-DATE    <= BNEDWS-WAIT-PERIOD-DATE)
J30221         OR    (PRO-EFFECT-DATE    <= BNBEN-START-DATE (I1))
               PERFORM 860-FIND-NXTRNG-PROSET2
           END-PERFORM.

       2318-END.

030400******************************************************************
       2320-SET-EFR-TABLE.
030400******************************************************************

           PERFORM 2322-FIND-EMPFLEXDOL
           THRU    2322-END.

           PERFORM 2324-FIND-EFR-TABLE
           THRU    2324-END.

      *
      **** 2324-FIND-EFR-TABLE WILL CHECK IF WE HAVE ALREADY CREATED
      **** EFR TABLE FOR EMPLOYEE AND EMPFLEXDOL, IF NOT WE WILL CREATE
      **** IT, THIS SETS THE VALUE OF I6 WHICH IS POINTING TO THE RIGHT EFD
      *
           IF (ERROR-FOUND)
           OR (BNFRWS-EFD-EMPLOYEE (I6)    = BNBEN-EMPLOYEE (I1))
               GO TO 2320-END.

           INITIALIZE I7.

           MOVE BNBEN-EMPLOYEE (I1)        TO BNFRWS-EFD-EMPLOYEE (I6).
           MOVE BNBEN-EFD-START-DT (I1)    TO BNFRWS-EFD-START-DT (I6).

           MOVE BNBEN-COMPANY              TO DB-COMPANY.
           MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
           MOVE BNBEN-EFD-START-DT (I1)    TO DB-PLAN-START-DT.
           MOVE EFRSET1-PLAN-START-DT      TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-EFRSET1.
           PERFORM
               UNTIL (EMPFLEXREM-NOTFOUND)
               OR    (I7               > BNFRWS-MAX-PERIODS)

               ADD 1                   TO I7

               PERFORM 795-EDIT-AVAIL-DOLLARS

               MOVE EFR-START-DATE     TO BNFRWS-START-DATE    (I6 I7)
               MOVE EFR-STOP-DATE      TO BNFRWS-STOP-DATE     (I6 I7)
               MOVE EFR-CREDITS-AVAIL  TO BNFRWS-CREDITS-AVAIL (I6 I7)
               MOVE EFR-PRE-TAX-AVAIL  TO BNFRWS-PRE-TAX-AVAIL (I6 I7)

               PERFORM 860-FIND-NXTRNG-EFRSET1
           END-PERFORM.
           
           IF (I7                      > BNFRWS-MAX-PERIODS)
      ******** More than 24 flex periods exist; increase table size
               MOVE 200                TO CRT-ERROR-NBR
               MOVE BNBEN-FC-FN        TO CRT-FIELD-NBR
               GO TO 2320-END.
035300
       2320-END.

153200******************************************************************
       2322-FIND-EMPFLEXDOL.
153200******************************************************************

           IF (BNBEN-LINE-FC (I1)          = "A")
015800         MOVE BNBEN-COMPANY          TO DB-COMPANY
015800         MOVE BNBEN-EMPLOYEE (I1)    TO DB-EMPLOYEE
015800         MOVE BNBEN-START-DATE (I1)  TO DB-START-DATE
015900         PERFORM 850-FIND-NLT-EFDSET2
016100         IF  (EMPFLEXDOL-FOUND)
016200         AND (EFD-COMPANY            = DB-COMPANY)
016300         AND (EFD-EMPLOYEE           = DB-EMPLOYEE)
016300         AND (EFD-STOP-DATE          >= DB-START-DATE)
016500             MOVE EFD-START-DATE     TO BNBEN-EFD-START-DT (I1)
016600             MOVE EFD-STOP-DATE      TO BNBEN-EFD-STOP-DT (I1)
                   MOVE EFD-FLD-GROUP-NAME TO BNBEN-EFD-GROUP-NM (I1)
                   MOVE EFD-EMP-FLEX-AMT   TO BNBEN-EFD-PT-AMT (I1)
               ELSE
                   INITIALIZE BNBEN-EFD-START-DT (I1)
                              BNBEN-EFD-STOP-DT (I1)
                              BNBEN-EFD-GROUP-NM (I1)
                              BNBEN-EFD-PT-AMT (I1)
               END-IF
           ELSE
015800         MOVE BEN-COMPANY            TO DB-COMPANY
015800         MOVE BEN-EMPLOYEE           TO DB-EMPLOYEE
015800         MOVE BEN-EFD-START-DATE     TO DB-START-DATE
015900         PERFORM 840-FIND-EFDSET1
016100         IF (EMPFLEXDOL-FOUND)
016500             MOVE EFD-START-DATE     TO BNBEN-EFD-START-DT (I1)
016600             MOVE EFD-STOP-DATE      TO BNBEN-EFD-STOP-DT (I1)
                   MOVE EFD-FLD-GROUP-NAME TO BNBEN-EFD-GROUP-NM (I1)
                   MOVE EFD-EMP-FLEX-AMT   TO BNBEN-EFD-PT-AMT (I1)
               ELSE
                   INITIALIZE BNBEN-EFD-START-DT (I1)
                              BNBEN-EFD-STOP-DT (I1)
                              BNBEN-EFD-GROUP-NM (I1)
                              BNBEN-EFD-PT-AMT (I1).

           IF (EFD-FLEX-PLAN               NOT = PLN-FLEX-PLAN)
      ******** Flex dollar record does not exist for flex plan \
               MOVE 220                    TO CRT-ERROR-NBR
               MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
               MOVE PLN-FLEX-PLAN          TO CRT-ERR-VAR1
               GO TO 2322-END.

       2322-END.

153200******************************************************************
       2324-FIND-EFR-TABLE.
153200******************************************************************

           PERFORM
               VARYING I6 FROM 1 BY 1
               UNTIL  (I6                  > 24)
               OR     ((BNFRWS-EFD-EMPLOYEE (I6)
                                           = BNBEN-EMPLOYEE (I1))
               AND     (BNFRWS-EFD-START-DT (I6)
                                           = BNBEN-EFD-START-DT (I1)))
               OR     (BNFRWS-EFD-EMPLOYEE (I6)
                                           = ZEROES)

               CONTINUE
           END-PERFORM.

           IF (I6                      > BNFRWS-MAX-EFDS)
      ******** More than 24 EFDs exist for the detail scr; Increase table size
               MOVE 126                TO CRT-ERROR-NBR
               MOVE BNBEN-FC-FN        TO CRT-FIELD-NBR
               GO TO 2324-END.

       2324-END.

153200******************************************************************
153300 2330-GET-SPEND-ONLY-START-STOP.
153400******************************************************************

           MOVE BNBEN-START-DATE (I1)      TO BNBEN-START-DATE-CALC.
           IF (BNBEN-START-DATE-MMDD       < FLP-ENTRY-MMDD)
               MOVE FLP-ENTRY-MMDD         TO BNBEN-START-DATE-MMDD
               SUBTRACT 1                  FROM BNBEN-START-YYYY
               MOVE BNBEN-START-DATE-CALC  TO BNBEN-EFD-START-DT (I1)
176400         ADD 1                       TO BNBEN-START-YYYY
176500         MOVE BNBEN-START-DATE-CALC  TO WSDR-FR-DATE
176600         PERFORM 900-DATE-TO-JULIAN
176700         COMPUTE WSDR-JULIAN-DAYS    = WSDR-JULIAN-DAYS
                                           - 1
176800         PERFORM 900-JULIAN-TO-DATE    
176900         MOVE WSDR-FR-DATE           TO BNBEN-EFD-STOP-DT (I1)
           ELSE
           IF (BNBEN-START-DATE-MMDD       > FLP-ENTRY-MMDD)
               MOVE FLP-ENTRY-MMDD         TO BNBEN-START-DATE-MMDD
               MOVE BNBEN-START-DATE-CALC  TO BNBEN-EFD-START-DT (I1)
176400         ADD 1                       TO BNBEN-START-YYYY
176500         MOVE BNBEN-START-DATE-CALC  TO WSDR-FR-DATE
176600         PERFORM 900-DATE-TO-JULIAN
176700         COMPUTE WSDR-JULIAN-DAYS    = WSDR-JULIAN-DAYS
                                           - 1
176800         PERFORM 900-JULIAN-TO-DATE    
176900         MOVE WSDR-FR-DATE           TO BNBEN-EFD-STOP-DT (I1)
           ELSE
           IF (BNBEN-START-DATE-MMDD       = FLP-ENTRY-MMDD)
               MOVE BNBEN-START-DATE-CALC  TO BNBEN-EFD-START-DT (I1)
176400         ADD 1                       TO BNBEN-START-YYYY
176500         MOVE BNBEN-START-DATE-CALC  TO WSDR-FR-DATE
176600         PERFORM 900-DATE-TO-JULIAN
176700         COMPUTE WSDR-JULIAN-DAYS    = WSDR-JULIAN-DAYS
                                           - 1
176800         PERFORM 900-JULIAN-TO-DATE
176900         MOVE WSDR-FR-DATE           TO BNBEN-EFD-STOP-DT (I1).

084200     IF (BNBEN-STOP-DATE (I1)        = ZEROES)
084300         MOVE BNBEN-EFD-STOP-DT (I1) TO BNBEN-STOP-DATE (I1)
084400     ELSE
084500     IF (BNBEN-STOP-DATE (I1)        > BNBEN-EFD-STOP-DT (I1))
      ******** Stop date cannot be > flex dollar stop date
084600         MOVE 107                        TO CRT-ERROR-NBR
084700         MOVE BNBEN-STOP-DATE-FN (I1)    TO CRT-FIELD-NBR
084800         GO TO 2330-END.
084900
       2330-END.

153200******************************************************************
042900 2400-EDIT-DTL-DATA.
153200******************************************************************

037400     IF  (BNBEN-FLEX-FLAG (I1)       = "Y")
037500     AND (BNBEN-EFD-START-DT (I1)    = ZEROES)
      ******** No flex dollars exist on enrollment date
037600         MOVE 128                    TO CRT-ERROR-NBR
037700         MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
037800         GO TO 2400-END.

044200     IF  (BNBEN-START-DATE (I1)      > BNBEN-STOP-DATE (I1))
044300     AND (BNBEN-STOP-DATE (I1)       NOT = ZEROES)
      ******** Start date cannot be greater than stop date
044400         MOVE 111                        TO CRT-ERROR-NBR
044500         MOVE BNBEN-STOP-DATE-FN (I1)    TO CRT-FIELD-NBR
044600         GO TO 2400-END.
044700
044800     IF  (BNBEN-START-DATE (I1)      < PLN-START-DATE)
044900     OR ((BNBEN-START-DATE (I1)      > PLN-STOP-DATE)
045000     AND (PLN-STOP-DATE              NOT = ZEROES))
      ******** Start date must be within start and stop date
045100         MOVE 114                        TO CRT-ERROR-NBR
045200         MOVE BNBEN-START-DATE-FN (I1)   TO CRT-FIELD-NBR
045300         GO TO 2400-END.
045400
           IF  (BNBEN-LINE-FC  (I1)        = "C")
           AND (BNBEN-STOP-DATE (I1)       NOT = BEN-STOP-DATE)
      ******** Cannot change stop date; Use "S"top function code
               MOVE 160                    TO CRT-ERROR-NBR
               MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
               GO TO 2400-END.

           IF (BNBEN-LINE-FC (I1)          = "C")
               PERFORM 2420-EDIT-YTD
               THRU    2420-END
               IF (ERROR-FOUND)
                   GO TO 2400-END.

           IF (BNBEN-LINE-FC (I1)          = "A")
047400         IF (BNBEN-START-DATE (I1)   < BNEDWS-ELIGIBILITY-DATE)
067900             MOVE BNEDWS-ELIGIBILITY-DATE    TO BNWS-DATE-FORMAT1
                                                      HRWS-DATE-FIELD
                   INITIALIZE                         HRWS-DATE-8-FIELD
047600             PERFORM 781-HR-FORMAT-DATE-FIELD
                   MOVE HRWS-VALUE                 TO CRT-ERR-VAR1
      ************ Employee not eligible to enroll until
069900             MOVE 100                        TO CRT-ERROR-NBR
                   MOVE BNBEN-START-DATE-FN (I1)   TO CRT-FIELD-NBR
047900             GO TO 2400-END.
048000
050900     IF (BNBEN-FLEX-FLAG (I1)        = "Y")
051000         PERFORM 2810-EDIT-FLEX-1
051100         THRU    2810-END
051300         IF (ERROR-FOUND)
051400             GO TO 2400-END.
051500
P62300*    MOVE BNBEN-START-DATE (I1)      TO DB-START-DATE.
P62300     MOVE BENSET3-PLAN-CODE          TO WS-DB-BEG-RNG.
P62300     MOVE "(BEN-START-DATE <= ?)"    TO FILTER-STRING.
P62300     PERFORM 890-CREATE-FILTER.
P62300     MOVE BNBEN-START-DATE (I1)      TO DATETIME-FILTER-VALUE.
P62300     PERFORM 890-SET-DATETIME-FILTER-VALUE.

051600     IF (BNBEN-LINE-FC (I1)          = "A")
P62300*         PERFORM 850-FIND-NLT-BENSET3
P62300         PERFORM 850-FILTER-BEGRNG-BENSET3
051800        IF  (BENEFIT-FOUND)
P62300*        AND (BEN-COMPANY            = DB-COMPANY)
P62300*        AND (BEN-EMPLOYEE           = DB-EMPLOYEE)
P62300*        AND (BEN-PLAN-TYPE          = DB-PLAN-TYPE)
P62300*        AND (BEN-PLAN-CODE          = DB-PLAN-CODE)
052300             PERFORM 2440-EDIT-EXISTING-PREV-BEN
052400             THRU    2440-END
052600             IF (ERROR-FOUND)
052700                 GO TO 2400-END
                   END-IF
                   INITIALIZE BNBEN-EDM-TABLE
                   IF (BEN-STOP-DATE       = ZEROES)
                   OR (BEN-STOP-DATE       > BNBEN-START-DATE (I1))
                       PERFORM 2444-CREATE-EDM-TABLE-STOP
                       THRU    2444-END.
052800
P62300     MOVE BENSET4-PLAN-CODE          TO WS-DB-BEG-RNG.
P62300     MOVE "(BEN-START-DATE >= ?)"    TO FILTER-STRING.
P62300     PERFORM 890-CREATE-FILTER.
P62300     MOVE BNBEN-START-DATE (I1)      TO DATETIME-FILTER-VALUE.
P62300     PERFORM 890-SET-DATETIME-FILTER-VALUE.
052900     IF (BNBEN-LINE-FC (I1)         = "A")
P62300*         PERFORM 850-FIND-NLT-BENSET4
P62300         PERFORM 850-FILTER-BEGRNG-BENSET4
053600         IF  (BENEFIT-FOUND)
P62300*         AND (BEN-COMPANY            = DB-COMPANY)
P62300*         AND (BEN-EMPLOYEE           = DB-EMPLOYEE)
P62300*         AND (BEN-PLAN-TYPE          = DB-PLAN-TYPE)
P62300*         AND (BEN-PLAN-CODE          = DB-PLAN-CODE)
054100             MOVE WS-FALSE           TO BNBEN-STOP-DFLT-SW
054200             PERFORM 2450-EDIT-EXISTING-POST-BEN
054300             THRU    2450-END
054400                 UNTIL  (ERROR-FOUND)
054500                 OR     (BNBEN-STOP-DEFAULTED)
054600                 OR     (BENEFIT-NOTFOUND)
P26300*                OR     (BEN-COMPANY          NOT = DB-COMPANY)
P26300*                OR     (BEN-EMPLOYEE         NOT = DB-EMPLOYEE)
P26300*                OR     (BEN-PLAN-TYPE        NOT = DB-PLAN-TYPE)
P26300*                OR     (BEN-PLAN-CODE        NOT = DB-PLAN-CODE)
055100                 OR    ((BNBEN-STOP-DATE (I1) NOT = ZEROES)
055200                 AND    (BNBEN-STOP-DATE (I1) < BEN-START-DATE))
055500             IF (ERROR-FOUND)
055600                 GO TO 2400-END.
059500*
059600**** NOTE: THE PROGRAM MAY NO LONGER BE ON THE BENEFIT RECORD FROM
059700**** THE SCREEN AT THIS POINT, BECAUSE 2440 AND 2450 MAY HAVE YOU
059800**** POINTING AT ANOTHER BENEFIT RECORD.
059900*
           IF (DB-START-DATE               NOT = BNBEN-START-DATE (I1))
               MOVE BNBEN-START-DATE (I1)  TO DB-START-DATE.
           PERFORM 840-FIND-BENSET1.

           IF  (PLN-COVERAGE-TYPE          = "0")
           AND (PLN-CONTRIB-TYPE           = "0")
               PERFORM 2430-EDIT-ALL-ZERO
               THRU    2430-END
               IF (ERROR-FOUND)
                   GO TO 2400-END.

           IF (BNBEN-MULT-SALARY (I1)      NOT = ZEROES)
               IF (PLN-PLAN-TYPE           = "VA")
               OR (PLN-CONTRIB-TYPE        = "X")
P60050*        OR (PLN-COVERAGE-TYPE       = "2")
               OR (PLN-COVERAGE-TYPE       = "X")
                   CONTINUE
P60050         ELSE
P60050         IF  (PLN-COVERAGE-TYPE      = "2")
P60050             MOVE BNBEN-COMPANY      TO DB-COMPANY
P60050             MOVE BNBEN-PLAN-TYPE (I1)
P60050                                     TO DB-PLAN-TYPE
P60050             MOVE BNBEN-PLAN-CODE (I1)
P60050                                     TO DB-PLAN-CODE
P60050             MOVE "E"                TO DB-COVER-TYPE
P60050             IF  (BNBEN-LINE-FC (I1) = "C")
P60050                 MOVE BEN-COV-UPD-DT TO DB-START-DATE
P60050                 MOVE BEN-COV-GROUP  TO DB-GROUP-NAME
P60050             ELSE
P60050                 MOVE BNBEN-COV-UPD-DT (I1)
P60050                                     TO DB-START-DATE
P60050                 MOVE BNBEN-COV-GROUP  (I1)
P60050                                     TO DB-GROUP-NAME
P60050             END-IF
P60050             PERFORM 840-FIND-CVRSET1
P60050             IF  (CVR-CALC-TYPE      = "N")
P85709             AND (CRT-PROGRAM-CODE NOT = "BN102")
P79642             AND (CRT-PROGRAM-CODE NOT = "BN105")
P60050                 MOVE 312            TO CRT-ERROR-NBR
P60050                 MOVE BNBEN-MULT-SALARY-FN (I1)
P60050                                     TO CRT-FIELD-NBR
P60050                 GO TO 2400-END
P60050             END-IF
               ELSE
      ************ Multiple invalid for this benefit
044400             MOVE 250                        TO CRT-ERROR-NBR
044500             MOVE BNBEN-MULT-SALARY-FN (I1)  TO CRT-FIELD-NBR
P60050             GO TO 2400-END
P60050         END-IF
P60050         END-IF
P60050     END-IF.

           IF  (BNBEN-LINE-FC (I1)         = "C")
P48954     AND (PLN-CONTRIB-TYPE           = "5" OR "6" OR "7")
           AND (PLN-FLEX-PLAN              = SPACES)
           AND (BEN-CYCLES-REMAIN          NOT = BNBEN-COVER-OPT (I1))
      ******** Cannot override cycles for a non-flex plan
               MOVE 159                    TO CRT-ERROR-NBR
               MOVE BNBEN-COVER-OPT-FN(I1) TO CRT-FIELD-NBR
               GO TO 2400-END.

P42111     IF  (PLN-COVERAGE-TYPE          = "0")
P42111     AND (PLN-CONTRIB-TYPE           = "3")
P42111         IF (BNBEN-COVER-OPT (I1)    NOT = ZEROES)
P42111************ Coverage Option invalid for flat amount
P42111             MOVE 231                       TO CRT-ERROR-NBR
P42111             MOVE BNBEN-COVER-OPT-FN (I1)   TO CRT-FIELD-NBR
P42111             GO TO 2400-END
P42111         END-IF
P42111
P42111         IF (BNBEN-COVER-AMT (I1)    NOT = ZEROES)
P42111************ Pay period amount invalid for flat amount
P42111             MOVE 232                       TO CRT-ERROR-NBR
P42111             MOVE BNBEN-COVER-AMT-FN (I1)   TO CRT-FIELD-NBR
P42111             GO TO 2400-END
P42111         END-IF
P42111
P42111         IF (BNBEN-PAY-RATE (I1)     NOT = ZEROES)
P42111************ Salary invalid for flat amount
P42111             MOVE 233                       TO CRT-ERROR-NBR
P42111             MOVE BNBEN-PAY-RATE-FN (I1)    TO CRT-FIELD-NBR
P42111             GO TO 2400-END.
P42111
      *
      **** CALCULATE COVERAGE AMOUNT
      *
           MOVE BNBEN-COMPANY              TO BNCVWS-COMPANY.
           MOVE BNBEN-PLAN-TYPE (I1)       TO BNCVWS-PLAN-TYPE.
           MOVE BNBEN-PLAN-CODE (I1)       TO BNCVWS-PLAN-CODE.
           MOVE "E"                        TO BNCVWS-COVER-TYPE.
           MOVE BNBEN-EMPLOYEE (I1)        TO BNCVWS-EMPLOYEE.
           MOVE BNBEN-START-DATE (I1)      TO BNCVWS-AS-OF-DATE
                                              BNCVWS-START-DATE.
           MOVE BNBEN-STOP-DATE (I1)       TO BNCVWS-STOP-DATE.
           MOVE BNBEN-COVER-OPT (I1)       TO BNCVWS-IN-COVER-OPT.
           MOVE BNBEN-COVER-AMT (I1)       TO BNCVWS-IN-COVER-AMT.
           MOVE BNBEN-MULT-SALARY (I1)     TO BNCVWS-MULT-OF-SALARY.
           MOVE BNBEN-PAY-RATE (I1)        TO BNCVWS-IN-ANNUAL-SALARY.
           MOVE BNBEN-LINE-FC (I1)         TO BNCVWS-FC.
           IF (BNBEN-LINE-FC (I1)          = "C")
               MOVE BEN-COV-UPD-DT         TO BNCVWS-COV-UPD-DT
               MOVE BEN-COV-GROUP          TO BNCVWS-COV-GROUP.

FAK        IF (PLN-COVERAGE-TYPE           NOT = "0")
               PERFORM 5000-DO-COVERAGE-70
061500         IF (ERROR-FOUND)
                   MOVE BNCVWS-FIELD-NBR   TO BNBEN-FIELD-NBR
                   PERFORM 2410-SET-ERROR-CURSOR
                   THRU    2410-END
061700             GO TO 2400-END
               ELSE
                   MOVE BNCVWS-COVER-OPT   TO BNBEN-COVER-OPT (I1)
                   MOVE BNCVWS-COVER-AMT   TO BNBEN-COVER-AMT (I1)
                   MOVE BNCVWS-DEPEND-AMT  TO BNBEN-DEP-COVER-AMT (I1)
                   MOVE BNCVWS-MULT-OF-SALARY
                                           TO BNBEN-MULT-SALARY (I1)
                   MOVE BNCVWS-CVR-START-DATE
                                           TO BNBEN-COV-UPD-DT (I1)
                   MOVE BNCVWS-CVR-GROUP-NAME
                                           TO BNBEN-COV-GROUP (I1)
                   MOVE BNCVWS-OVERRIDE-SW TO BNBEN-COV-OVER-FLG (I1)
                   MOVE BNCVWS-SAL-CALC-DATE
                                           TO BNBEN-COV-SAL-DATE (I1)
                   MOVE BNCVWS-AGE-CALC-DATE
                                           TO BNBEN-COV-AGE-DATE (I1)
                   IF  (PLN-COVERAGE-TYPE = "2")
                   AND (PLN-CONTRIB-TYPE  = "0")
                   AND (BNCOVERAGE-FOUND)
P60050             AND (CVR-CALC-TYPE = "M" OR "N")
                       MOVE BNCVWS-ANNUAL-SALARY
                                           TO BNBEN-PAY-RATE (I1)
P55140             END-IF
P55140         END-IF
P55140     ELSE
P55140         INITIALIZE BNCVWS-COVER-OPT
P55140                    BNCVWS-COV-DEPENDENTS
P55140                    BNCVWS-COVER-AMT
P55140                    BNCVWS-ANNUAL-SALARY
P55140                    BNCVWS-DEPEND-AMT
P55140                    BNCVWS-CVR-START-DATE
P55140                    BNCVWS-CVR-GROUP-NAME
P55140                    BNCVWS-SAL-CALC-DATE
P55140                    BNCVWS-AGE-CALC-DATE
P55140                    BNCVWS-FIELD-NBR
P55140     END-IF.

      *
      **** CALCULATE CONTRIBUTION AMOUNT
      *
           MOVE BNBEN-COMPANY              TO BNCTWS-COMPANY.
           MOVE BNBEN-PLAN-TYPE (I1)       TO BNCTWS-PLAN-TYPE.
           MOVE BNBEN-PLAN-CODE (I1)       TO BNCTWS-PLAN-CODE.
           MOVE "E"                        TO BNCTWS-COVER-TYPE.
           MOVE BNBEN-EMPLOYEE (I1)        TO BNCTWS-EMPLOYEE.
           MOVE BNBEN-START-DATE (I1)      TO BNCTWS-AS-OF-DATE.
           MOVE BNBEN-EMP-PRE-CONT (I1)    TO BNCTWS-IN-EMP-PT-CONT.
           MOVE BNBEN-EMP-AFT-CONT (I1)    TO BNCTWS-IN-EMP-AT-CONT.
           MOVE BNBEN-COVER-OPT (I1)       TO BNCTWS-CONTRIB-OPT.
           MOVE BNBEN-COVER-AMT (I1)       TO BNCTWS-COVER-AMT.
           IF (PLN-CONTRIB-TYPE            = "5" OR "6" OR "7")
               MOVE BNBEN-COVER-AMT (I1)   TO BNCTWS-IN-PAY-PER-AMT
               IF (BNBEN-LINE-FC (I1)      = "C")
P83236             IF  (BNBEN-PLAN-TYPE (I1) = "RS") 
P83236             AND (BNBEN-FLEX-FLAG (I1) = "Y")
P83236                 MOVE BEN-YTD-CONT   TO BNCTWS-YTD-CONT
P83236             ELSE
P83236                 MOVE BEN-EMP-YTD-CONT
P83236                                     TO BNCTWS-YTD-CONT
P83236             END-IF
P63803*             MOVE BEN-EMP-YTD-CONT   TO BNCTWS-YTD-CONT.
P83236         END-IF    
P83236     END-IF.
           MOVE BNBEN-PAY-RATE (I1)        TO BNCTWS-ANNUAL-SALARY.
           MOVE BNBEN-PAY-RATE (I1)        TO BNCTWS-IN-ANNUAL-AMT.
           MOVE BNBEN-PCT-AMT-FLAG (I1)    TO BNCTWS-IN-PCT-AMT-FLAG.
           MOVE BNBEN-PRE-AFT-FLAG (I1)    TO BNCTWS-IN-PRE-AFT-FLAG.
           MOVE BNBEN-SMOKER-FLAG (I1)     TO BNCTWS-IN-SMOKER-SW.
P59213*****This fix was introduced for SEA issue.
P59213     IF  (PLN-CONTRIB-TYPE     = "5" OR "6" OR "7")
P59213     AND (PLN-COVERAGE-TYPE    = "0")
P59213     AND (BNBEN-PLAN-TYPE (I1) = "VA")
P59213     AND (CRT-PROGRAM-CODE     = "BN31")
P59213         INITIALIZE BNCTWS-CYC-REMAIN
P59213     ELSE
           MOVE BNBEN-COVER-OPT (I1)       TO BNCTWS-CYC-REMAIN.
           MOVE BNBEN-START-DATE (I1)      TO BNCTWS-START-DATE.
           MOVE BNBEN-STOP-DATE (I1)       TO BNCTWS-STOP-DATE.
           MOVE BNBEN-LINE-FC (I1)         TO BNCTWS-FC.
           IF (BNBEN-LINE-FC (I1)          = "C")
               MOVE BEN-PREM-UPD-DT        TO BNCTWS-PREM-UPD-DT
               MOVE BEN-PREM-GROUP         TO BNCTWS-PREM-GROUP.

           IF (BNBEN-PLAN-TYPE (I1)        = "VA")
               MOVE BNBEN-MULT-SALARY (I1) TO BNCTWS-NBR-OF-HOURS.

FAK        IF (PLN-CONTRIB-TYPE            NOT = "0")
               PERFORM 5000-DO-CONTRIBUTION-70 
               IF (ERROR-FOUND)
                   MOVE BNCTWS-FIELD-NBR   TO BNBEN-FIELD-NBR
                   PERFORM 2410-SET-ERROR-CURSOR
                   THRU    2410-END
                   GO TO 2400-END 
               ELSE
                   MOVE WS-FALSE           TO BNBEN-MID-YEAR-SW (I1)
P63031             IF (PREMIUM-FOUND)
                       MOVE PRE-START-DATE TO BNWS-PRE-START-DT-CALC
                                              BNBEN-SVD-PRE-DATE (I1)
P63031             END-IF
                   IF (PLN-CONTRIB-TYPE = "6" OR "7")
                       PERFORM 2415-SET-MID-YEAR-SWITCH
                       THRU    2415-END
                   END-IF
J66130
J66130             IF  (BNBEN-COVER-AMT (I1) = ZEROES)
J66130                 SET BNBEN-COVER-AMT-Y    TO TRUE
J66130             END-IF
J66130
J66130             IF  (BNBEN-PAY-RATE (I1) = ZEROES)
J66130                 SET BNBEN-PAY-RATE-Y     TO TRUE
J66130             END-IF
J66130
J66130             IF  (BNBEN-EMP-PRE-CONT (I1) = ZEROES)
J66130                 SET BNBEN-EMP-PRE-CONT-Y TO TRUE
J66130             END-IF
J66130
J66130             IF  (BNBEN-EMP-AFT-CONT (I1) = ZEROES)
J66130                 SET BNBEN-EMP-AFT-CONT-Y TO TRUE
J66130             END-IF
J66130
J71747*J66130      IF  (BNBEN-COMP-CONT (I1) = ZEROES)
J71747*J66130          SET BNBEN-COMP-CONT-Y    TO TRUE
J71747*J66130      END-IF
J66130
                   MOVE BNCTWS-PCT-AMT-FLAG
                                           TO BNBEN-PCT-AMT-FLAG (I1)
                   MOVE BNCTWS-PRE-AFT-FLAG
                                           TO BNBEN-PRE-AFT-FLAG (I1)
                   MOVE BNCTWS-EMP-PT-CONT TO BNBEN-EMP-PRE-CONT (I1)
                   MOVE BNCTWS-EMP-AT-CONT TO BNBEN-EMP-AFT-CONT (I1)
                   MOVE BNCTWS-COMP-CONT   TO BNBEN-COMP-CONT (I1)
                   MOVE BNCTWS-PRE-START-DATE
                                           TO BNBEN-PREM-UPD-DT (I1)
                   MOVE BNCTWS-PRE-GROUP-NAME
                                           TO BNBEN-PREM-GROUP (I1)
                   MOVE BNCTWS-PRE-MATCH-OPTION
                                           TO
                                           BNBEN-PRE-MATCH-OPTION (I1)
                   MOVE BNCTWS-PRE-MATCH-CALC
                                           TO BNBEN-PRE-MATCH-CALC (I1)
                   IF (PLN-CONTRIB-TYPE    = "5" OR "6" OR "7")
                       MOVE BNCTWS-PAY-PER-AMT
                                           TO BNBEN-COVER-AMT (I1)
                   END-IF
                   MOVE BNCTWS-ANNUAL-AMT  TO BNBEN-PAY-RATE (I1)
                   IF (BNBEN-PAY-RATE (I1) = ZEROES)
                       MOVE BNCVWS-ANNUAL-SALARY       
                                           TO BNBEN-PAY-RATE (I1)
                   END-IF
                   MOVE BNCTWS-SAL-CALC-DATE
                                           TO BNBEN-CNT-SAL-DATE (I1)
                   MOVE BNCTWS-AGE-CALC-DATE
                                           TO BNBEN-CNT-AGE-DATE (I1)
                   MOVE BNCTWS-SERV-CALC-DATE
                                           TO BNBEN-CNT-SERV-DATE (I1)
029712             IF  (BNCTWS-IN-SMOKER-SW NOT = SPACES)
029712             AND (BNCTWS-SMOKER-SW = SPACES)
029712                 MOVE BNCTWS-IN-SMOKER-SW TO BNCTWS-SMOKER-SW
029712             END-IF
                   MOVE BNCTWS-SMOKER-SW   TO BNBEN-SMOKER-FLAG (I1)
                   MOVE BNCTWS-COMP-CONT-SW TO BNBEN-COMP-CONT-SW (I1)
                   IF (PLN-CONTRIB-TYPE    = "6")
                       MOVE BNCTWS-PCT-MATCHED
                                           TO BNBEN-PCT-MATCHED (I1)
                   END-IF
                   IF (PLN-CONTRIB-TYPE    = "6" OR "7")
                       MOVE BNCTWS-MAX-MATCH  
                                           TO BNBEN-MAX-MATCH (I1)
                   END-IF
                   IF  (BNBEN-COVER-OPT (I1) = ZEROES)
                   AND (BNBEN-PLAN-TYPE (I1) NOT = "VA")
                       MOVE BNCTWS-CYC-REMAIN TO BNBEN-COVER-OPT (I1)
                   END-IF

P58009             IF  (PRE-CONT-TAX-STS            = "B")             
P58009             AND (BNBEN-LINE-FC(I1)           = "C")
P58009                 IF (BNBEN-PRE-AFT-FLAG(I1) 
P58009                                       NOT    = BEN-PRE-AFT-FLAG)
P58009                 OR  ((BNBEN-PRE-AFT-FLAG(I1) = "B")
P58009                 AND (BEN-PRE-AFT-FLAG       = "B"))   
P58009                     IF  (BEN-PRE-AFT-FLAG    = "P")
P58009                     OR  ((BEN-PRE-AFT-FLAG   = "B")
P58009                     AND (((BNBEN-EMP-PRE-CONT(I1)       
P58009                                       NOT    = BEN-EMP-PRE-CONT)
P58009                     AND (BNBEN-EMP-AFT-CONT(I1)
P58009                                        NOT   = BEN-EMP-AFT-CONT))
P58009                     AND ((BNBEN-EMP-AFT-CONT (I1)  
P58009                                              = ZEROS)
P58009                     OR  (BNBEN-EMP-PRE-CONT  (I1)  
P58009                                              = ZEROS)
P58009                     OR  (BEN-EMP-PRE-CONT    = ZEROS)
P58009                     OR  (BEN-EMP-AFT-CONT    = ZEROS))))

P58009                         IF(BEN-PCT-AMT-FLAG  = "A")   
P58009                            MOVE PLN-PRE-DED-CODE-A 
P58009                                              TO DB-DED-CODE
P58009                         ELSE
P58009                            MOVE PLN-PRE-DED-CODE-P 
P58009                                              TO DB-DED-CODE   
P58009                         END-IF 
P58009                         MOVE BEN-PRE-SEQ-NBR TO DB-SEQ-NBR 
P58009                         PERFORM 840-FIND-EDMSET1
P58009                         MOVE PDHSET1-SEQ-NBR TO WS-DB-BEG-RNG    
P58009                         PERFORM 850-KFIND-BEGRNG-PDHSET1
P58009                         IF(PREMDEDHST-KFOUND)   
P58009                           MOVE 215           TO CRT-ERROR-NBR    
P58009                           MOVE BNBEN-LINE-FC-FN(I1) 
P58009                                              TO CRT-FIELD-NBR
P58009                           GO TO 2400-END
P58009                         END-IF                    
J24483                         INITIALIZE FILTER-STRING
J24483                       MOVE "(PYM-STATUS != '9')" TO FILTER-STRING
J24483                         PERFORM 890-CREATE-FILTER
J24483          
J24483                         MOVE BNBEN-COMPANY       TO DB-COMPANY
J24483                         MOVE BNBEN-EMPLOYEE (I1) TO DB-EMPLOYEE
J24483                         MOVE PYMSET1-EMPLOYEE    TO WS-DB-BEG-RNG
J24483                         PERFORM 850-FILTER-BEGRNG-PYMSET1
J24483                         IF  (PAYMASTR-FOUND)
J24483                              MOVE 213     TO CRT-ERROR-NBR
J24483                              MOVE BNBEN-LINE-FC-FN (I1) 
J24483                                           TO CRT-FIELD-NBR
J24483                              GO TO 2400-END
J24483                         END-IF
P58009                     END-IF

P58009                     IF  (BEN-PRE-AFT-FLAG    = "A")
P58009                     OR  ((BEN-PRE-AFT-FLAG   = "B")
P58009                     AND (((BNBEN-EMP-PRE-CONT(I1)       
P58009                                          NOT = BEN-EMP-PRE-CONT)
P58009                     AND (BNBEN-EMP-AFT-CONT(I1)
P58009                                          NOT = BEN-EMP-AFT-CONT))
P58009                     AND ((BNBEN-EMP-AFT-CONT(I1)   
P58009                                              = ZEROS)
P58009                     OR  (BNBEN-EMP-PRE-CONT(I1) 
P58009                                              = ZEROS)
P58009                     OR  (BEN-EMP-PRE-CONT    = ZEROS)   
P58009                     OR  (BEN-EMP-AFT-CONT    = ZEROS))))   

P58009                         IF(BEN-PCT-AMT-FLAG  = "A")   
P58009                            MOVE PLN-AFT-DED-CODE-A 
P58009                                              TO DB-DED-CODE
P58009                         ELSE
P58009                            MOVE PLN-AFT-DED-CODE-P 
P58009                                              TO DB-DED-CODE   
P58009                         END-IF 
P58009                         MOVE BEN-AFT-SEQ-NBR TO DB-SEQ-NBR
P58009                         PERFORM 840-FIND-EDMSET1
P58009                         MOVE PDHSET1-SEQ-NBR TO WS-DB-BEG-RNG    
P58009                         PERFORM 850-KFIND-BEGRNG-PDHSET1
P58009                         IF(PREMDEDHST-KFOUND)   
P58009                            MOVE 215          TO CRT-ERROR-NBR    
P58009                            MOVE BNBEN-LINE-FC-FN(I1) 
P58009                                              TO CRT-FIELD-NBR
P58009                            GO TO 2400-END
P58009                         END-IF
J24483                         INITIALIZE FILTER-STRING
J24483                       MOVE "(PYM-STATUS != '9')" TO FILTER-STRING
J24483                         PERFORM 890-CREATE-FILTER
J24483          
J24483                         MOVE BNBEN-COMPANY       TO DB-COMPANY
J24483                         MOVE BNBEN-EMPLOYEE (I1) TO DB-EMPLOYEE
J24483                         MOVE PYMSET1-EMPLOYEE    TO WS-DB-BEG-RNG
J24483                         PERFORM 850-FILTER-BEGRNG-PYMSET1
J24483                         IF  (PAYMASTR-FOUND)
J24483                              MOVE 213     TO CRT-ERROR-NBR
J24483                              MOVE BNBEN-LINE-FC-FN (I1) 
J24483                                           TO CRT-FIELD-NBR
J24483                              GO TO 2400-END
J24483                         END-IF
P58009                     END-IF 
P58009                 END-IF
P58009             END-IF   
                   IF  (PLN-CONTRIB-TYPE     = "5")
                   AND (PLN-PLAN-TYPE        = "RS")
                   AND (PLN-FLEX-PLAN        NOT = SPACES)
                   AND (BNBEN-COVER-OPT (I1) NOT = BNWS-TOT-CYC)
                       MOVE "Y"              TO BNBEN-COV-OVER-FLG (I1).

J67795* EDIT DECIMALS 3 AND 4
J67795*** EDIT COVER AMT
J07169*
J67795     MOVE BNBEN-COVER-AMT (I1)       TO BNBEN-TEMP-NBR.
J67795     IF (BNBEN-DEC-3-4 NOT = ZEROES)
J07169*        IF (PLN-CONTRIB-TYPE NOT = "3" AND "5")
J66130         IF (BNBEN-COVER-AMT-Y)
J66130             IF ((BNBEN-PCT-AMT-FLAG (I1) = "A")
J66130             AND (PRE-ROUND-METH = "N" OR SPACES))
J66130                 MOVE ZEROES         TO BNBEN-DEC-3-4
J66130                 MOVE BNBEN-TEMP-NBR TO BNBEN-COVER-AMT (I1)  
J66130             END-IF
J66130         ELSE
J07169             IF (BNBEN-PCT-AMT-FLAG (I1) = "A")
J07169*---BNBEN#234: Only two decimal places allowed
J67795                 MOVE 234            TO CRT-ERROR-NBR
J67795                 MOVE BNBEN-COVER-AMT-FN (I1) 
J07169                                     TO CRT-FIELD-NBR
J67795                 GO TO 2400-END
J07169             END-IF
J66130         END-IF
J07169*        ELSE
J07169*            IF ((BNBEN-PCT-AMT-FLAG (I1)= "A")
J07169*            AND (PLN-CONTRIB-TYPE = "5" OR "3"))
J07169*                MOVE 234                     TO CRT-ERROR-NBR
J07169*                MOVE BNBEN-COVER-AMT-FN (I1) TO CRT-FIELD-NBR
J07169*                GO TO 2400-END
J07169*            END-IF
J07169*        END-IF
J67795     END-IF.
J07169*
J67795*** EDIT PAY RATE
J07169*
J67795     MOVE BNBEN-PAY-RATE (I1)        TO BNBEN-TEMP-NBR.
J67795     IF (BNBEN-DEC-3-4 NOT = ZEROES)
J07169*        IF (PLN-CONTRIB-TYPE NOT = "3" AND "5")
J66130         IF (BNBEN-PAY-RATE-Y)
J66130             IF ((BNBEN-PCT-AMT-FLAG (I1) = "A")
J66130             AND (PRE-ROUND-METH = "N" OR SPACES))
J66130                 MOVE ZEROES         TO BNBEN-DEC-3-4
J66130                 MOVE BNBEN-TEMP-NBR TO BNBEN-PAY-RATE (I1)  
J66130             END-IF
J66130         ELSE
J07169             IF (BNBEN-PCT-AMT-FLAG (I1) = "A")
J07169*---BNBEN#234: Only two decimal places allowed
J67795                 MOVE 234            TO CRT-ERROR-NBR
J67795                 MOVE BNBEN-PAY-RATE-FN (I1) 
J07169                                     TO CRT-FIELD-NBR
J67795                 GO TO 2400-END
J07169             END-IF
J66130         END-IF
J07169*        ELSE
J07169*            IF ((BNBEN-PCT-AMT-FLAG (I1)= "A")
J07169*            AND (PLN-CONTRIB-TYPE = "5" OR "3"))
J07169*                MOVE 234                    TO CRT-ERROR-NBR
J07169*                MOVE BNBEN-PAY-RATE-FN (I1) TO CRT-FIELD-NBR
J07169*                GO TO 2400-END
J07169*            END-IF
J07169*        END-IF
J67795     END-IF.
J07169*
J67795*** EDIT EMP PRE CONT
J07169*
J67795     MOVE BNBEN-EMP-PRE-CONT (I1)    TO BNBEN-TEMP-NBR.
J67795     IF (BNBEN-DEC-3-4 NOT = ZEROES)
J07169*        IF (PLN-CONTRIB-TYPE NOT = "3" AND "5")
J66130         IF (BNBEN-EMP-PRE-CONT-Y)
J66130             IF ((BNBEN-PCT-AMT-FLAG (I1) = "A")
J66130             AND (PRE-ROUND-METH = "N" OR SPACES))
J66130                 MOVE ZEROES         TO BNBEN-DEC-3-4
J66130                 MOVE BNBEN-TEMP-NBR TO BNBEN-EMP-PRE-CONT (I1)
J66130             END-IF
J66130         ELSE
J07169             IF (BNBEN-PCT-AMT-FLAG (I1) = "A")
J07169*---BNBEN#234: Only two decimal places allowed
J67795                 MOVE 234            TO CRT-ERROR-NBR
J67795                 MOVE BNBEN-EMP-PRE-CONT-FN (I1) 
J07169                                     TO CRT-FIELD-NBR
J67795                 GO TO 2400-END
J07169             END-IF
J66130         END-IF
J07169*        ELSE
J07169*            IF ((BNBEN-PCT-AMT-FLAG (I1)= "A")
J07169*            AND (PLN-CONTRIB-TYPE = "5" OR "3"))
J07169*                MOVE 234                        TO CRT-ERROR-NBR
J07169*                MOVE BNBEN-EMP-PRE-CONT-FN (I1) TO CRT-FIELD-NBR
J07169*                GO TO 2400-END
J07169*            END-IF
J07169*        END-IF
J67795     END-IF.
J07169*
J67795*** EDIT EMP AFT CONT
J07169*
J67795     MOVE BNBEN-EMP-AFT-CONT (I1)    TO BNBEN-TEMP-NBR.
J67795     IF (BNBEN-DEC-3-4 NOT = ZEROES)
J07169*        IF (PLN-CONTRIB-TYPE NOT = "3" AND "5")
J66130         IF (BNBEN-EMP-AFT-CONT-Y)
J66130             IF ((BNBEN-PCT-AMT-FLAG (I1) = "A")
J66130             AND (PRE-ROUND-METH = "N" OR SPACES))
J66130                 MOVE ZEROES         TO BNBEN-DEC-3-4
J66130                 MOVE BNBEN-TEMP-NBR TO BNBEN-EMP-AFT-CONT (I1)
J66130             END-IF
J66130         ELSE
J07169             IF (BNBEN-PCT-AMT-FLAG (I1) = "A")
J07169*---BNBEN#234: Only two decimal places allowed
J67795                 MOVE 234            TO CRT-ERROR-NBR
J67795                 MOVE BNBEN-EMP-AFT-CONT-FN (I1) 
J07169                                     TO CRT-FIELD-NBR
J67795                 GO TO 2400-END
J07169             END-IF
J66130         END-IF
J07169*        ELSE
J07169*            IF ((BNBEN-PCT-AMT-FLAG (I1)= "A")
J07169*            AND (PLN-CONTRIB-TYPE = "5" OR "3"))
J07169*                MOVE 234                        TO CRT-ERROR-NBR
J07169*                MOVE BNBEN-EMP-AFT-CONT-FN (I1) TO CRT-FIELD-NBR
J07169*                GO TO 2400-END
J07169*            END-IF
J07169*        END-IF
J67795     END-IF.
J07169*
J67795*** EDIT COMP CONT
J07169*
J67795     MOVE BNBEN-COMP-CONT (I1)       TO BNBEN-TEMP-NBR.
J67795     IF (BNBEN-DEC-3-4 NOT = ZEROES)
J07169*        IF (PLN-CONTRIB-TYPE NOT = "3" AND "5")
J71747*J66130      IF (BNBEN-COMP-CONT-Y)
J66130                 IF ((BNBEN-PCT-AMT-FLAG (I1) = "A")
J66130                 AND (PRE-ROUND-METH = "N" OR SPACES))
J66130                     MOVE ZEROES         TO BNBEN-DEC-3-4
J66130                     MOVE BNBEN-TEMP-NBR TO BNBEN-COMP-CONT (I1)
J66130                 END-IF
J71747*J66130      ELSE
J71747*J67795          MOVE 234                     TO CRT-ERROR-NBR
J71747*J67795          MOVE BNBEN-LINE-FC-FN (I1)   TO CRT-FIELD-NBR
J71747*J67795          GO TO 2400-END
J71747*J67795      END-IF
J07169*        ELSE
J07169*            IF ((BNBEN-PCT-AMT-FLAG (I1)= "A")
J07169*            AND (PLN-CONTRIB-TYPE = "5" OR "3"))
J07169*                MOVE 234                     TO CRT-ERROR-NBR
J07169*                MOVE BNBEN-LINE-FC-FN (I1)   TO CRT-FIELD-NBR
J07169*                GO TO 2400-END
J07169*            END-IF
J07169*        END-IF
J67795     END-IF.
J66130
J66130     INITIALIZE BNBEN-COVER-AMT-SW
J66130                BNBEN-PAY-RATE-SW
J66130                BNBEN-EMP-PRE-CONT-SW
J66130                BNBEN-EMP-AFT-CONT-SW
J66130                BNBEN-COMP-CONT-SWT.

           IF (ZERO-PREMIUM)
               MOVE WS-TRUE            TO BNBEN-ZERO-PREMIUM-SW (I1)
P54229         MOVE WS-FALSE           TO BNBEN-NEG-PREMIUM-SW (I1)
P54229         MOVE WS-FALSE           TO BNBEN-POS-PREMIUM-SW (I1)
           ELSE
           IF (NEGATIVE-PREMIUM)
               MOVE WS-TRUE            TO BNBEN-NEG-PREMIUM-SW (I1)
P54229         MOVE WS-FALSE           TO BNBEN-ZERO-PREMIUM-SW (I1)
P54229         MOVE WS-FALSE           TO BNBEN-POS-PREMIUM-SW (I1)
           ELSE
P54229         MOVE WS-FALSE           TO BNBEN-ZERO-PREMIUM-SW (I1)
P54229         MOVE WS-FALSE           TO BNBEN-NEG-PREMIUM-SW (I1)
               MOVE WS-TRUE            TO BNBEN-POS-PREMIUM-SW (I1).

           IF (BNBEN-COMP-CONT-SW (I1) = SPACES)
               PERFORM 5500-GET-COMP-CONT-SW
               MOVE BNWS-COMP-CONT-SW  TO BNBEN-COMP-CONT-SW (I1).

           IF (BNBEN-FLEX-FLAG (I1)    = "Y")
               PERFORM 2820-EDIT-EMPFLEXREM
               THRU    2820-END
               IF (ERROR-FOUND)
                   GO TO 2400-END.

           IF  (PLN-CONTRIB-TYPE       = "5")
           AND (PLN-FLEX-PLAN          NOT = SPACES)
               PERFORM 2480-EDIT-ANN-LIMITS
               THRU    2480-END.

           IF (BNBEN-PLAN-TYPE (I1)    = "HL" OR "DN" OR "EL")
           OR (BNBEN-PLAN-TYPE (I1)    = "DL")
               PERFORM 2700-EDIT-DEPENDENTS
               THRU    2700-END
               IF (ERROR-FOUND)
                   GO TO 2400-END.

           MOVE BNBEN-COMPANY          TO BNREWS-COMPANY.
           MOVE BNBEN-PLAN-TYPE (I1)   TO BNREWS-PLAN-TYPE.
           MOVE BNBEN-PLAN-CODE (I1)   TO BNREWS-PLAN-CODE.
           MOVE BNBEN-START-DATE (I1)  TO BNREWS-AS-OF-DATE.
           MOVE "BNA"                  TO BNREWS-FILE-PREFIX.
           PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70.
           IF (ERROR-FOUND)
               GO TO 2400-END.

           IF (BNREWS-BNACCOUNTS-SW    = "Y")
               MOVE BNA-START-DATE     TO BNBEN-BNA-START-DATE (I1)
               MOVE BNA-GROUP-NAME     TO BNBEN-BNA-GROUP-NAME (I1)

040200         MOVE BNBEN-COMPANY              TO DB-COMPANY 
040300         MOVE BNBEN-PLAN-TYPE (I1)       TO DB-PLAN-TYPE 
040600         MOVE BNBEN-PLAN-CODE (I1)       TO DB-PLAN-CODE 
               MOVE BNBEN-BNA-START-DATE (I1)  TO DB-START-DATE
               MOVE BNBEN-BNA-GROUP-NAME (I1)  TO DB-GROUP-NAME
               INITIALIZE DB-DED-CODE
               PERFORM 850-FIND-NLT-BNASET1
               IF  (BNACCOUNTS-FOUND)
               AND (BNA-COMPANY    = DB-COMPANY)
               AND (BNA-PLAN-TYPE  = DB-PLAN-TYPE)
               AND (BNA-PLAN-CODE  = DB-PLAN-CODE)
               AND (BNA-START-DATE = DB-START-DATE)
               AND (BNA-GROUP-NAME = DB-GROUP-NAME)
                    PERFORM 2900-EDIT-BNACCOUNTS
                    THRU    2900-END
                        UNTIL (ERROR-FOUND)
                        OR    (BNACCOUNTS-NOTFOUND)
                        OR    (BNA-COMPANY    NOT = DB-COMPANY)
                        OR    (BNA-PLAN-TYPE  NOT = DB-PLAN-TYPE)
                        OR    (BNA-PLAN-CODE  NOT = DB-PLAN-CODE)
                        OR    (BNA-START-DATE NOT = DB-START-DATE)
                        OR    (BNA-GROUP-NAME NOT = DB-GROUP-NAME)
                        IF (ERROR-FOUND)
                            GO TO 2400-END.
                              
           IF (BNBEN-LINE-FC (I1)      = "C")
               PERFORM 3000-SET-CHANGE-PREM-SW
               THRU    3000-END.

           IF  (BNBEN-LINE-FC (I1)     NOT = "A")
           AND (BNBEN-ELIG-UPD-DT (I1) = ZEROES)
           AND (BNBEN-ELIG-GROUP  (I1) = SPACES)
               MOVE BEN-ELIG-UPD-DT    TO BNBEN-ELIG-UPD-DT (I1)
               MOVE BEN-ELIG-GROUP     TO BNBEN-ELIG-GROUP  (I1).

      **** ADDED BENEFITS SW IS USED TO DISPLAY A BUTTON FROM BN31 ****

           IF (I1                    <= BNBEN-ORIG-NBR-LINES)
               MOVE "Y"              TO BNBEN-ADDED-BENEFITS-SW (I1).

           PERFORM 2850-EDIT-HIPAA
           THRU    2850-END.

           IF (BNBEN-TRIGGER-ENABLED-SW (I1) = WS-TRUE)
               PERFORM 2490-EDIT-LP
               THRU    2490-END
               IF (ERROR-FOUND)
                   GO TO 2400-END.

           PERFORM 2500-EDIT-DED-OVERRIDE
           THRU    2500-END.
           IF (ERROR-FOUND)
               GO TO 2400-END.

           IF  (I1                       > BNBEN-ORIG-NBR-LINES)
           AND ((BNBEN-EMPLOYEE (I1)     NOT = BNBEN-SV-EMP)
           OR   (BNBEN-PLAN-TYPE (I1)    NOT = BNBEN-SV-PL-TP)
           OR   (BNBEN-PLAN-CODE (I1)    NOT = BNBEN-SV-PL-CD))
               PERFORM 2550-EDIT-DED-OVERRIDE
               THRU    2550-END
               IF (ERROR-FOUND)
                   GO TO 2400-END.

043000 2400-END.
           IF  (I1                   > BNBEN-ORIG-NBR-LINES)
           AND (ERROR-FOUND)
               PERFORM 2405-SKIP-REST-PLAN-CODES
               THRU    2405-END.

063100******************************************************************
       2405-SKIP-REST-PLAN-CODES.
063100******************************************************************

           INITIALIZE CRT-ERROR-NBR
                      CRT-ERROR-CAT
                      CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3
                      CRT-ERR-VAR4
                      CRT-ERR-VAR5.

           MOVE "Y"                    TO BNBEN-FUTURE-ERROR-SW (I1).

           MOVE BNBEN-EMPLOYEE (I1)    TO BNBEN-SAVE-EMPLOYEE.
           MOVE BNBEN-PLAN-TYPE (I1)   TO BNBEN-SAVE-PLAN-TYPE.
           MOVE BNBEN-PLAN-CODE (I1)   TO BNBEN-SAVE-PLAN-CODE.

           IF (USE-NAVIGATE)
               SET ITS-NOT-DONE        TO TRUE

               PERFORM
                   VARYING BNBEN-I2 FROM BNBEN-I2 BY 1
                   UNTIL  (BNBEN-I2              > BNBEN-NBR-LINES)
                   OR     (BNBEN-NBR (BNBEN-I2)  = ZEROES)
                   OR     (ITS-DONE)

                   MOVE BNBEN-NBR (BNBEN-I2)     TO I1

                   MOVE "Y"                 TO
                                            BNBEN-FUTURE-ERROR-SW (I1)

                   IF (BNBEN-EMPLOYEE (I1)  NOT = BNBEN-SAVE-EMPLOYEE)
                   OR (BNBEN-PLAN-TYPE (I1) NOT = BNBEN-SAVE-PLAN-TYPE)
                   OR (BNBEN-PLAN-CODE (I1) NOT = BNBEN-SAVE-PLAN-CODE)
                       SET ITS-DONE         TO TRUE
                   END-IF
               END-PERFORM
           ELSE
               PERFORM
                   VARYING I1 FROM I1 BY 1
                   UNTIL  (I1 > BNBEN-NBR-LINES)
                   OR     (BNBEN-EMPLOYEE (I1)  NOT =
                                                BNBEN-SAVE-EMPLOYEE)
                   OR     (BNBEN-PLAN-TYPE (I1) NOT =
                                                BNBEN-SAVE-PLAN-TYPE)
                   OR     (BNBEN-PLAN-CODE (I1) NOT =
                                                BNBEN-SAVE-PLAN-CODE)

                   MOVE "Y"                 TO
                                            BNBEN-FUTURE-ERROR-SW (I1)
               END-PERFORM.

       2405-END.

063100******************************************************************
063200 2410-SET-ERROR-CURSOR.
063300******************************************************************

           IF (BNBEN-FIELD-NBR                 = 1)
061600         MOVE BNBEN-COVER-OPT-FN (I1)    TO CRT-FIELD-NBR
           ELSE
           IF (BNBEN-FIELD-NBR                 = 2)
061600         MOVE BNBEN-MULT-SALARY-FN (I1)  TO CRT-FIELD-NBR
           ELSE
           IF (BNBEN-FIELD-NBR                 = 3)
061600         MOVE BNBEN-COVER-AMT-FN (I1)    TO CRT-FIELD-NBR
           ELSE
           IF (BNBEN-FIELD-NBR                 = 4)
061600         MOVE BNBEN-PAY-RATE-FN (I1)     TO CRT-FIELD-NBR
           ELSE
           IF (BNBEN-FIELD-NBR                 = 5)
061600         MOVE BNBEN-START-DATE-FN (I1)   TO CRT-FIELD-NBR
           ELSE
           IF (BNBEN-FIELD-NBR                 = 6)
061600         MOVE BNBEN-STOP-DATE-FN (I1)    TO CRT-FIELD-NBR
           ELSE
           IF (BNBEN-FIELD-NBR                 = 7)
061600         MOVE BNBEN-PCT-AMT-FLAG-FN (I1) TO CRT-FIELD-NBR
           ELSE
           IF (BNBEN-FIELD-NBR                 = 8)
061600         MOVE BNBEN-PRE-AFT-FLAG-FN (I1) TO CRT-FIELD-NBR
           ELSE
           IF (BNBEN-FIELD-NBR                 = 9)
061600         MOVE BNBEN-EMP-PRE-CONT-FN (I1) TO CRT-FIELD-NBR
           ELSE
           IF (BNBEN-FIELD-NBR                 = 10)
061600         MOVE BNBEN-EMP-AFT-CONT-FN (I1) TO CRT-FIELD-NBR
           ELSE
           IF (BNBEN-FIELD-NBR                 = 11)
061600         MOVE BNBEN-SMOKER-FLAG-FN (I1)  TO CRT-FIELD-NBR
           ELSE
061600         MOVE BNBEN-LINE-FC-FN (I1)      TO CRT-FIELD-NBR.

043000 2410-END.

      ******************************************************************
       2415-SET-MID-YEAR-SWITCH.
      ******************************************************************

      * The following switch is added for use in BNEMDED when 
      * calculating the maximum match amount for a Company match 
      * deduction, when the enrollment is added mid-year.
           MOVE BNBEN-START-DATE (I1)      TO BNBEN-START-DATE-CALC
                                              BNWS-DATE.
           IF (BNWS-PRE-START-CALC-MMDD NOT = BNBEN-START-DATE-MMDD)
      * Calculate the plan-year start date.
               IF (BNBEN-LINE-FC (I1) = "A")
                   MOVE BNWS-PRE-START-CALC-MMDD  
                                           TO BNWS-PLN-YR-START-MMDD
                   IF (BNWS-PRE-START-CALC-MMDD > BNWS-DATE-MMDD)
                       COMPUTE BNWS-PLN-YR-START-YYYY = BNWS-YYYY - 1
                   ELSE
                       MOVE BNWS-YYYY      TO BNWS-PLN-YR-START-YYYY
                   END-IF
      * Determine whether this is initial enrollment for employee
      * If it is initial enrollment, the new calc routine in BNEMDED
      * does not need to be performed, so the switch will not be set.
                   MOVE BNCTWS-COMPANY     TO DB-COMPANY     
                   MOVE BNCTWS-EMPLOYEE    TO DB-EMPLOYEE     
                   MOVE BNCTWS-PLAN-TYPE   TO DB-PLAN-TYPE     
                   MOVE BNCTWS-PLAN-CODE   TO DB-PLAN-CODE
P26300*            MOVE BNCTWS-START-DATE  TO DB-START-DATE
P26300             MOVE BENSET3-PLAN-CODE  TO WS-DB-BEG-RNG
P65424             MOVE "(BEN-START-DATE <= ?)"
P26300                                     TO FILTER-STRING
P26300             PERFORM 890-CREATE-FILTER
P26300             MOVE BNCTWS-START-DATE  TO DATETIME-FILTER-VALUE
P26300             PERFORM 890-SET-DATETIME-FILTER-VALUE
P67829             PERFORM 850-FILTER-BEGRNG-BENSET3
P26300*            PERFORM 850-FIND-NLT-BENSET3
P26300             IF  (BENEFIT-FOUND)
P26300*            AND (BEN-COMPANY        = DB-COMPANY)
P26300*            AND (BEN-EMPLOYEE       = DB-EMPLOYEE)
P26300*            AND (BEN-PLAN-TYPE      = DB-PLAN-TYPE)
P26300*            AND (BEN-PLAN-CODE      = DB-PLAN-CODE))
                   AND ((BEN-STOP-DATE     = ZEROES)
                   OR  ((BEN-STOP-DATE NOT = ZEROES)
                   AND  (BEN-STOP-DATE NOT < BNWS-PLN-YR-START-DT)))
                       MOVE WS-TRUE        TO BNBEN-MID-YEAR-SW (I1)
                   END-IF.

       2415-END.

063100******************************************************************
063200 2420-EDIT-YTD.
063300******************************************************************

           IF  ((BEN-YTD-CONT              = ZEROES)
           AND  (BEN-PLAN-TYPE             = "RS" OR "SB" OR "SP"))
           AND ((BNBEN-SPEND-ONLY (I1)     NOT = "Y")
           AND  (PLN-FLEX-PLAN             NOT = SPACES))
           AND (PLN-CONTRIB-TYPE           = "5")
      ******** Cannot change benefit with no year to date; delete and add
044400         MOVE 149                    TO CRT-ERROR-NBR
044500         MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
044600         GO TO 2420-END.

      *--- Type '5' added for PT130113 where it was leaving two deductions
      *--- open for a Benefit
           IF  (BEN-YTD-CONT             = ZEROES)
           AND (PLN-CONTRIB-TYPE         = "5" OR "6" OR "7")
           AND ((BEN-PRE-AFT-FLAG        NOT = BNBEN-PRE-AFT-FLAG (I1))
           OR   (BEN-PCT-AMT-FLAG        NOT = BNBEN-PCT-AMT-FLAG (I1)))
      ******** Cannot change benefit with no year to date; delete and add
044400         MOVE 149                  TO CRT-ERROR-NBR
044500         MOVE BNBEN-LINE-FC-FN(I1) TO CRT-FIELD-NBR
044600         GO TO 2420-END.

           IF  (BEN-YTD-CONT               NOT = ZEROES)
           AND (PLN-FLEX-PLAN              = SPACES)
           AND (PLN-CONTRIB-TYPE           = "5")
      ******** Cannot change benefit with year to date; stop and re-add
044400         MOVE 215                    TO CRT-ERROR-NBR
044500         MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
044600         GO TO 2420-END.

      *--- When YTD present and type '6' or '7' and trying to change
      *--- either of PctAmt or PreAft flag give an error, Added with
      *--- fix for PT130113
           IF  (BEN-YTD-CONT             NOT = ZEROES)
           AND (PLN-CONTRIB-TYPE         = "6" OR "7")
           AND ((BEN-PRE-AFT-FLAG        NOT = BNBEN-PRE-AFT-FLAG (I1))
           OR   (BEN-PCT-AMT-FLAG        NOT = BNBEN-PCT-AMT-FLAG (I1)))
      ******** Cannot change benefit with year to date; stop and re-add
044400         MOVE 215                    TO CRT-ERROR-NBR
044500         MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
044600         GO TO 2420-END.

           IF  (BEN-YTD-CONT               NOT = ZEROES)
           AND (BNBEN-YTD-XMIT             = ZEROES)
084800     AND (((PLN-CONTRIB-TYPE         = "5" OR "6" OR "7")
084900     AND   ((BEN-CYCLES-REMAIN       NOT = BNBEN-COVER-OPT (I1))
085900     OR     (BEN-BOND-DED-AMT        NOT = BNBEN-COVER-AMT (I1))
086400     OR     (BEN-ANNUAL-AMT          NOT = BNBEN-PAY-RATE (I1))))
084800     OR   ((PLN-CONTRIB-TYPE         = "1" OR "2" OR "3" OR "4")
085100     AND   ((BEN-COV-OPTION          NOT = BNBEN-COVER-OPT (I1))
086100     OR     (BEN-COVER-AMT           NOT = BNBEN-COVER-AMT (I1))
086600     OR     (BEN-PAY-RATE            NOT = BNBEN-PAY-RATE (I1)))))
               IF (CRT-PROGRAM-CODE        = "BN32")
      ************ Warning:Change will affect YTD amount; OK to continue
                   MOVE 310                    TO CRT-ERROR-NBR
                   MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
                   MOVE 1                      TO BNBEN-YTD-XMIT
               ELSE
               IF (CRT-PROGRAM-CODE        = "BN531")
      ************ Warning:Record updated; Change will affect YTD amount
                   MOVE 311                    TO CRT-ERROR-NBR
               END-IF
               GO TO 2420-END.

062900 2420-END.

070700******************************************************************
070800 2430-EDIT-ALL-ZERO.
070900******************************************************************
071000
           IF (BNBEN-COVER-OPT (I1)           NOT = ZEROES)
      ******** Cannot enter for zero coverage and contribution plan
               MOVE 219                       TO CRT-ERROR-NBR
061600         MOVE BNBEN-COVER-OPT-FN (I1)   TO CRT-FIELD-NBR
               GO TO 2430-END.

           IF (BNBEN-MULT-SALARY (I1)         NOT = ZEROES)
      ******** Cannot enter for zero coverage and contribution plan
               MOVE 219                       TO CRT-ERROR-NBR
061600         MOVE BNBEN-MULT-SALARY-FN (I1) TO CRT-FIELD-NBR
               GO TO 2430-END.

           IF (BNBEN-COVER-AMT (I1)           NOT = ZEROES)
      ******** Cannot enter for zero coverage and contribution plan
               MOVE 219                       TO CRT-ERROR-NBR
061600         MOVE BNBEN-COVER-AMT-FN (I1)   TO CRT-FIELD-NBR
               GO TO 2430-END.

           IF (BNBEN-PAY-RATE (I1)            NOT = ZEROES)
      ******** Cannot enter for zero coverage and contribution plan
               MOVE 219                       TO CRT-ERROR-NBR
061600         MOVE BNBEN-PAY-RATE-FN (I1)    TO CRT-FIELD-NBR
               GO TO 2430-END.

           IF (BNBEN-EMP-PRE-CONT (I1)         NOT = ZEROES)
      ******** Cannot enter for zero coverage and contribution plan
               MOVE 219                        TO CRT-ERROR-NBR
061600         MOVE BNBEN-EMP-PRE-CONT-FN (I1) TO CRT-FIELD-NBR
               GO TO 2430-END.

           IF (BNBEN-EMP-AFT-CONT (I1)         NOT = ZEROES)
      ******** Cannot enter for zero coverage and contribution plan
               MOVE 219                        TO CRT-ERROR-NBR
061600         MOVE BNBEN-EMP-AFT-CONT-FN (I1) TO CRT-FIELD-NBR
               GO TO 2430-END.

           IF (BNBEN-PRE-AFT-FLAG (I1)         NOT = SPACES)
      ******** Cannot enter for zero coverage and contribution plan
               MOVE 219                        TO CRT-ERROR-NBR
061600         MOVE BNBEN-PRE-AFT-FLAG-FN (I1) TO CRT-FIELD-NBR
               GO TO 2430-END.

           IF (BNBEN-PCT-AMT-FLAG (I1)         NOT = SPACES)
      ******** Cannot enter for zero coverage and contribution plan
               MOVE 219                        TO CRT-ERROR-NBR
061600         MOVE BNBEN-PCT-AMT-FLAG-FN (I1) TO CRT-FIELD-NBR
               GO TO 2430-END.

           IF (BNBEN-SMOKER-FLAG (I1)          NOT = SPACES)
      ******** Cannot enter for zero coverage and contribution plan
               MOVE 219                        TO CRT-ERROR-NBR
061600         MOVE BNBEN-SMOKER-FLAG-FN (I1)  TO CRT-FIELD-NBR
               GO TO 2430-END.

062900 2430-END.

070700******************************************************************
070800 2440-EDIT-EXISTING-PREV-BEN.
070900******************************************************************
071000
071100     MOVE WS-FALSE                   TO BNBEN-DEL-BEN-SW
071200                                        BNBEN-CHG-BEN-SW.
071300
071400     PERFORM 2442-EDIT-CHANGING-BEN
071500     THRU    2442-END
071600         VARYING I2 FROM 1 BY 1
071700         UNTIL  (I2 = I1)
071800         OR     (BNBEN-DELETING-BEN)
071900         OR     (BNBEN-CHANGING-BEN).
072000
072100     IF  (BNBEN-NOT-CHANGING-BEN)
072200     AND (BEN-STOP-DATE              NOT = ZEROES)
072300     AND (BEN-STOP-DATE              >= BNBEN-START-DATE (I1))
      ******** Already enrolled for specified time period
072400         MOVE 124                        TO CRT-ERROR-NBR
072500         MOVE BNBEN-STOP-DATE-FN (I1)    TO CRT-FIELD-NBR
072600         GO TO 2440-END.
072700
P51666*--- Below is the scenario where, there is prior dated Benefit which
P51666*--- e.g. started on 10/01/05 and have deduction override date of
P51666*--- 10/16/05 and now new Benefit is being added on 10/05/05, we need
P51666*--- to error out on Overlap message
P51666     IF  (BNBEN-NOT-CHANGING-BEN)
P51666     AND (BEN-DED-START-DATE         NOT = ZEROES)
P51666         IF  (BNBEN-START-DATE (I1)      <= BEN-DED-START-DATE)
P85709         OR ((BNBEN-DED-START-DATE (I1)  <= BEN-DED-START-DATE) 
P85709         AND (BNBEN-DED-START-DATE (I1) NOT = ZEROES))
P51666*------- Deduction dates overlap; Check override dates
P51666             MOVE 303                          TO CRT-ERROR-NBR
P51666             MOVE BNBEN-DED-START-DATE-FN (I1) TO CRT-FIELD-NBR.
P51666
072800 2440-END.
072900
073000******************************************************************
073100 2442-EDIT-CHANGING-BEN.
073200******************************************************************
073300
073400     IF  (BNBEN-LINE-FC (I2)         = "D")
073500     AND (BNBEN-PLAN-CODE (I2)       = BEN-PLAN-CODE)
073600     AND (BNBEN-START-DATE (I2)      = BEN-START-DATE)
073700         MOVE WS-TRUE                TO BNBEN-DEL-BEN-SW
073800         GO TO 2442-END.
073900
074000     IF  (BNBEN-LINE-FC (I2)         = "S")
074100     AND (BNBEN-PLAN-CODE (I2)       = BEN-PLAN-CODE)
074200     AND (BNBEN-START-DATE (I2)      = BEN-START-DATE)
074300         IF  (BNBEN-STOP-DATE (I2)   NOT = ZEROES)
074500         AND (BNBEN-STOP-DATE (I2)   >= BNBEN-START-DATE (I1))
      ************ Already enrolled for specified time period
074800             MOVE 124                        TO CRT-ERROR-NBR
072500             MOVE BNBEN-STOP-DATE-FN (I1)    TO CRT-FIELD-NBR
074700             MOVE WS-TRUE                    TO BNBEN-CHG-BEN-SW
075000             GO TO 2442-END
075100         ELSE
075200             MOVE WS-TRUE                    TO BNBEN-CHG-BEN-SW
075300             GO TO 2442-END.
075400
075500 2442-END.

153200******************************************************************
       2444-CREATE-EDM-TABLE-STOP.
153200******************************************************************

      *--- This will create EdmCode table with Start and Stop dates that
      *--- will be automatically stopped because of benefit added in future.
      *--- To achieve this we will use existing logic in BNEMDED where
      *--- we know which deductions will be stopped

P85709     IF  (BEN-DED-STOP-DATE          NOT = ZEROES)
P85709     AND (PLN-COVERAGE-TYPE          NOT = "0")
P85709     AND (PLN-FLEX-PLAN              = SPACES)
               MOVE BNBEN-START-DATE (I1)  TO WSDR-FR-DATE
               PERFORM 900-DATE-TO-JULIAN
               SUBTRACT 1                  FROM WSDR-JULIAN-DAYS
               PERFORM 900-JULIAN-TO-DATE
               MOVE WSDR-FR-DATE           TO BNWS-SCR-STOP-DATE
           ELSE
               MOVE BEN-DED-STOP-DATE      TO BNWS-SCR-STOP-DATE.

           INITIALIZE BNBEN-EDM-TABLE
                      I3.

      *--- Indicates it won't do any updates in BNEMDED
           MOVE "E"                        TO BNWS-DEDEDIT-TYPE.

           PERFORM 600-STOP-EXISTING-EMDEDREC.

      *--- This is so that we don't have to do it in Update Section
           INITIALIZE BNWS-DEDEDIT-TYPE.

       2444-END.

075700******************************************************************
075800 2450-EDIT-EXISTING-POST-BEN.
075900******************************************************************
076000
076100     IF (BEN-START-DATE              = BNBEN-START-DATE (I1))
076200         GO TO 2450-FIND-NEXT-BENSET4.
076300
076400     MOVE WS-FALSE                   TO BNBEN-DEL-BEN-SW.
076500
076600     PERFORM 2452-EDIT-DELETING-POST-BEN
076700     THRU    2452-END
076800         VARYING I2 FROM 1 BY 1
076900         UNTIL  (BNBEN-DELETING-BEN)
077000         OR     (I2 > BNBEN-NBR-LINES).
077100
077200     IF (BNBEN-DELETING-BEN)
077300         GO TO 2450-FIND-NEXT-BENSET4.
077400
077500     IF (BNBEN-NOT-DELETING-BEN)
077600         IF (BNBEN-STOP-DATE (I1)    = ZEROES)
077700             MOVE WS-TRUE            TO BNBEN-STOP-DFLT-SW
077800             MOVE BEN-START-DATE     TO WSDR-FR-DATE
077900             PERFORM 900-DATE-TO-JULIAN
078000             SUBTRACT 1              FROM WSDR-JULIAN-DAYS
078100             PERFORM 900-JULIAN-TO-DATE   
078200             MOVE WSDR-FR-DATE       TO BNBEN-STOP-DATE (I1)
078300         ELSE
078400         IF (BNBEN-STOP-DATE (I1)    >= BEN-START-DATE)
      ************ Already enrolled for specified time period
078500             MOVE 124                    TO CRT-ERROR-NBR
078600             MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
078700             GO TO 2450-END.
078800
078900 2450-FIND-NEXT-BENSET4.
P64135     IF (BNBEN-LINE-FC (I1)         = "A")
P64135         PERFORM 860-FIND-NXTRNG-BENSET4
P64135     ELSE
P64135         PERFORM 860-FIND-NEXT-BENSET4.
079200
079300 2450-END.

079500******************************************************************
079600 2452-EDIT-DELETING-POST-BEN.
079700******************************************************************
079800
080100     IF  (BNBEN-LINE-FC (I2)         = "D")
079900     AND (BNBEN-PLAN-CODE (I2)       = BEN-PLAN-CODE)
080000     AND (BNBEN-START-DATE (I2)      = BEN-START-DATE)
080200         MOVE WS-TRUE                TO BNBEN-DEL-BEN-SW.
080300
080400 2452-END.

067500******************************************************************
       2480-EDIT-ANN-LIMITS.
067500******************************************************************

           INITIALIZE BNBEN-C5-ANNUAL-AMT.

           MOVE BNBEN-COMPANY              TO DB-COMPANY.
           MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE
                                              BNBEN-SAVE-EMPLOYEE.
           MOVE BNBEN-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
           MOVE BNBEN-PLAN-CODE (I1)       TO DB-PLAN-CODE
TEDS                                          BNBEN-SAVE-PLAN-CODE.
P26300*    MOVE BNBEN-EFD-START-DT (I1)    TO DB-START-DATE.
P26300     MOVE BENSET4-PLAN-CODE          TO WS-DB-BEG-RNG.
P26300     MOVE "(BEN-START-DATE >= ?)"    TO FILTER-STRING.
P26300     PERFORM 890-CREATE-FILTER.
P26300     MOVE BNBEN-EFD-START-DT (I1)    TO DATETIME-FILTER-VALUE.
P26300     PERFORM 890-SET-DATETIME-FILTER-VALUE.
P26300*    PERFORM 850-FIND-NLT-BENSET4.
P26300     PERFORM 850-FILTER-BEGRNG-BENSET4.
           PERFORM 2482-ADD-BEN-ANNUAL-AMT
           THRU    2482-END
               UNTIL (BENEFIT-NOTFOUND)
P26300*        OR    (BEN-COMPANY          NOT = DB-COMPANY)
P26300*        OR    (BEN-EMPLOYEE         NOT = DB-EMPLOYEE)
P26300*        OR    (BEN-PLAN-TYPE        NOT = DB-PLAN-TYPE)
P26300*        OR    (BEN-PLAN-CODE        NOT = DB-PLAN-CODE)
               OR    (BEN-START-DATE       > BNBEN-EFD-STOP-DT (I1)).

           PERFORM 2484-ADD-FROM-DETAIL-LINE
           THRU    2484-END
J97795         VARYING BNBEN-I1 FROM I1 BY 1
               UNTIL  (BNBEN-I1 > I1).

040200     MOVE BNBEN-COMPANY              TO DB-COMPANY.
           MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
040300     MOVE BNBEN-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
040600     MOVE BNBEN-PLAN-CODE (I1)       TO DB-PLAN-CODE.
040500     MOVE BNBEN-START-DATE (I1)      TO DB-START-DATE.
040900     PERFORM 840-FIND-BENSET1.

051200     IF  (PRE-ANN-AMT-MAX            NOT = ZEROES)
051100     AND (BNBEN-C5-ANNUAL-AMT        > BNCTWS-ANN-AMT-MAX)
      ******** Total of benefits for plan year exceeds plan maximum
051300         MOVE 184                    TO CRT-ERROR-NBR
085700         MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
051500         GO TO 2480-END.

           IF  (PRE-ANN-AMT-MIN            NOT = ZEROES)
           AND (BNBEN-C5-ANNUAL-AMT        < BNCTWS-ANN-AMT-MIN)
      ******** Contribution amount less than annual minimum amount
J25635*        MOVE 196                    TO BNCTWS-ERROR-NBR
J25635         MOVE 196                    TO CRT-ERROR-NBR 
085700         MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
000510         GO TO 2480-END.

       2480-END.

058800******************************************************************
058900 2482-ADD-BEN-ANNUAL-AMT.
059000******************************************************************

           IF (BEN-START-DATE              < BNBEN-EFD-START-DT (I1))
           OR (BEN-START-DATE              > BNBEN-EFD-STOP-DT (I1))
               GO TO 2482-GET-NEXT-BENEFIT.

           ADD BEN-ANNUAL-AMT              TO BNBEN-C5-ANNUAL-AMT.

       2482-GET-NEXT-BENEFIT.
P26300*    PERFORM 860-FIND-NEXT-BENSET4.
P26300     PERFORM 860-FIND-NXTRNG-BENSET4.

       2482-END.

067500******************************************************************
       2484-ADD-FROM-DETAIL-LINE.
067500******************************************************************

           IF (BNBEN-LINE-FC (BNBEN-I1)         NOT = "A" AND "C")
           OR (BNBEN-EMPLOYEE (BNBEN-I1) NOT = BNBEN-SAVE-EMPLOYEE)
               GO TO 2484-END.

           IF (BNBEN-PLAN-TYPE (BNBEN-I1)       NOT = "RS")
               GO TO 2484-END.

040200     MOVE BNBEN-COMPANY                   TO DB-COMPANY.
           MOVE BNBEN-EMPLOYEE (BNBEN-I1)       TO DB-EMPLOYEE.
040300     MOVE BNBEN-PLAN-TYPE (BNBEN-I1)      TO DB-PLAN-TYPE.
040600     MOVE BNBEN-PLAN-CODE (BNBEN-I1)      TO DB-PLAN-CODE.
040500     MOVE BNBEN-START-DATE (BNBEN-I1)     TO DB-START-DATE.
           PERFORM 840-FIND-PLNSET1.

           IF  (PLN-CONTRIB-TYPE                = "5")
           AND (PLN-FLEX-PLAN                   NOT = SPACES)
TEDS       AND (PLN-PLAN-CODE                   = BNBEN-SAVE-PLAN-CODE)
               CONTINUE
           ELSE
               GO TO 2484-END.

           IF (BNBEN-LINE-FC (BNBEN-I1)         = "A")
               ADD BNBEN-PAY-RATE (BNBEN-I1)    TO BNBEN-C5-ANNUAL-AMT.

           IF (BNBEN-LINE-FC (BNBEN-I1)         = "C")
040900         PERFORM 840-FIND-BENSET1
               SUBTRACT BEN-ANNUAL-AMT          FROM BNBEN-C5-ANNUAL-AMT
               ADD  BNBEN-PAY-RATE (BNBEN-I1)   TO BNBEN-C5-ANNUAL-AMT.

       2484-END.

153200******************************************************************
       2490-EDIT-LP.
153200******************************************************************

           IF  (CRT-PROGRAM-CODE           = "BN531")
           AND (BNBEN-BTE-EMP-GRP (I1)     NOT = SPACES)
               PERFORM 2492-EDIT-LP-FOR-BN531
               THRU    2492-END
               GO TO 2490-END.

           STRING "((TEM-MASTR-ENTRY <= ?) "
                  "AND ((TEM-MSTR-END = ?) "
                  "OR   (TEM-MSTR-END >= ?)))"
                  DELIMITED BY SIZE INTO FILTER-STRING.
           PERFORM 890-CREATE-FILTER.

           MOVE BNBEN-START-DATE (I1)      TO DATETIME-FILTER-VALUE.
           PERFORM 890-SET-DATETIME-FILTER-VALUE.

           INITIALIZE NUMERIC-FILTER-VALUE.
           PERFORM 890-SET-NUMERIC-FILTER-VALUE.

           MOVE BNBEN-STOP-DATE (I1)       TO DATETIME-FILTER-VALUE.
           PERFORM 890-SET-DATETIME-FILTER-VALUE.

           MOVE BNBEN-COMPANY              TO DB-COMPANY.
           MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
           MOVE PLN-PLAN-NAME              TO DB-PLAN.
           MOVE TEMSET1-PLAN               TO WS-DB-BEG-RNG.
           PERFORM 850-FILTER-BEGRNG-TEMSET1.
           PERFORM
               UNTIL (TAEEMASTER-NOTFOUND)

               MOVE TEM-COMPANY            TO DB-COMPANY
               MOVE PLN-PLAN-CODE          TO DB-PLAN-CODE
               MOVE TEM-PLAN               TO DB-TA-PLAN
               MOVE TEM-EMPLOYEE-GROUP     TO DB-EMPLOYEE-GROUP
               PERFORM 840-FIND-BTGSET1
               IF  (BNTAGROUP-FOUND)
               AND (BTG-EMP-GRP-STATUS     = 1)
                   NEXT SENTENCE
               END-IF

               PERFORM 860-FIND-NXTRNG-TEMSET1
           END-PERFORM.

           IF (TAEEMASTER-NOTFOUND)
      ******** Employee not eligible to buy or sell hours
000500         MOVE 251                    TO CRT-ERROR-NBR
085700         MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
000510         GO TO 2490-END.

       2490-END.

153200******************************************************************
       2492-EDIT-LP-FOR-BN531.
153200******************************************************************

           MOVE BNBEN-COMPANY              TO DB-COMPANY.
           MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
           MOVE PLN-PLAN-NAME              TO DB-PLAN.
           MOVE BNBEN-BTE-EMP-GRP (I1)     TO DB-EMPLOYEE-GROUP.
           MOVE BNBEN-BTE-POSITION (I1)    TO DB-POSITION.
           PERFORM 840-FIND-TEMSET1.
           IF (TAEEMASTER-NOTFOUND)
      ******** Employee not eligible to buy or sell hours
000500         MOVE 251                    TO CRT-ERROR-NBR
085700         MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
               GO TO 2492-END.

           MOVE PLN-PLAN-NAME              TO DB-TA-PLAN.
           PERFORM 840-FIND-BTGSET1.
           IF  (BNTAGROUP-FOUND)
           AND (BTG-EMP-GRP-STATUS         = 1)
               CONTINUE
           ELSE
      ******** Employee not eligible to buy or sell hours
000500         MOVE 251                    TO CRT-ERROR-NBR
085700         MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
               GO TO 2492-END.

           IF (BNBEN-LINE-FC (I1)           = "C")
               MOVE BNBEN-COMPANY           TO DB-COMPANY
               MOVE BNBEN-PLAN-CODE (I1)    TO DB-PLAN-CODE
               MOVE BNBEN-EMPLOYEE (I1)     TO DB-EMPLOYEE
               MOVE BNBEN-START-DATE (I1)   TO DB-START-DATE
               MOVE BNBEN-BTE-EMP-GRP (I1)  TO DB-EMPLOYEE-GROUP
               MOVE BNBEN-BTE-POSITION (I1) TO DB-POSITION
               PERFORM 840-FIND-BTESET1
               IF (BNTAEMPBAL-FOUND)
                   MOVE BTE-PERCENT         TO BNBEN-BTE-PERCENT

                   MOVE BTESET3-START-DATE  TO WS-DB-BEG-RNG

                   PERFORM 880-INIT-DBAG-BTESET3

                   SET DBSUM-BTESET3-PERCENT TO TRUE

                   PERFORM 880-CALC-DBAG-BTESET3

                   MOVE DBAG-SUM-BTESET3-PERCENT
                                             TO BNBEN-TOTAL-PCT-ON-BTE

                   COMPUTE BNBEN-TOTAL-PCT-ON-BTE
                                             = BNBEN-TOTAL-PCT-ON-BTE
                                             + BNBEN-BTE-ALLOC-PCT (I1)
                                             - BNBEN-BTE-PERCENT
                   IF (BNBEN-TOTAL-PCT-ON-BTE     > 100)
      **************** Total allocations cannot exceed 100 percent
000500                 MOVE 252                   TO CRT-ERROR-NBR
085700                 MOVE BNBEN-LINE-FC-FN (I1) TO CRT-FIELD-NBR
000510                 GO TO 2492-END.

       2492-END.

153200******************************************************************
       2500-EDIT-DED-OVERRIDE.
153200******************************************************************

           MOVE BNBEN-DED-START-DATE (I1)  TO BNBEN-DED-START.
           IF (BNBEN-DED-START             = ZEROES)
               MOVE BNBEN-START-DATE (I1)  TO BNBEN-DED-START.

           MOVE BNBEN-DED-STOP-DATE (I1)   TO BNBEN-DED-STOP.
           IF  (BNBEN-LINE-FC (I1)         = "S" OR "C")
           AND (BNBEN-STOP-DATE  (I1)      NOT = ZEROES)
           AND (BNBEN-DED-STOP-DATE (I1)   = ZEROES)
J24315     AND (PLN-COVERAGE-TYPE          NOT = "0")
J24315     AND (PLN-FLEX-PLAN              = SPACES)
               MOVE BNBEN-STOP-DATE (I1)   TO BNBEN-DED-STOP.

           IF  (BNBEN-DED-START            > BNBEN-DED-STOP)
           AND (BNBEN-DED-STOP             NOT = ZEROES)
      ******** Deduction start date cannot be > override stop date
               MOVE 309                    TO CRT-ERROR-NBR
               MOVE BNBEN-DED-START-DATE-FN (I1)
                                           TO CRT-FIELD-NBR
               GO TO 2500-END.

           IF (PLN-FLEX-PLAN              NOT = SPACES)
P85709     OR (PLN-COVERAGE-TYPE          = "0")
              IF  (BNBEN-DED-START-DATE (I1) NOT = ZEROES)
              OR  (BNBEN-DED-STOP-DATE (I1)  NOT = ZEROES)
      ******* Cannot enter deduction start or stop date for plan
                   MOVE 301                    TO CRT-ERROR-NBR
                   IF (BNBEN-DED-START         NOT = ZEROES)
                       MOVE BNBEN-DED-START-DATE-FN (I1)
                                           TO CRT-FIELD-NBR
                   ELSE
                   IF (BNBEN-DED-STOP          NOT = ZEROES)
                       MOVE BNBEN-DED-STOP-DATE-FN (I1)
                                           TO CRT-FIELD-NBR
                   END-IF
                   END-IF
                   GO TO 2500-END
               END-IF
           END-IF.

           IF  (BNBEN-STOP-DATE (I1)       = ZEROES)
           AND (BNBEN-DED-STOP             NOT = ZEROES)
               IF (BNBEN-LINE-FC (I1)      NOT = "S")
      ******** Cannot enter deduction stop date; Benefit stop date is blank
                   MOVE 302                TO CRT-ERROR-NBR
               ELSE
      ******** Cannot remove benefit stop date; Deduction stop date exists
                   MOVE 305                TO CRT-ERROR-NBR
               END-IF
               MOVE BNBEN-DED-STOP-DATE-FN (I1)
                                           TO CRT-FIELD-NBR
               GO TO 2500-END.

           IF  (CRT-PROGRAM-CODE           = "BN32")
           AND (BNBEN-LINE-FC (I1)         = "S")
           AND (BNBEN-DED-OVER-XMIT        = ZEROES)
           AND (BNBEN-STOP-DATE (I1)       NOT = ZEROES)
           AND (BNBEN-DED-STOP-DATE (I1)   NOT = ZEROES)
      ******** Warning:Deduction stop date exist; Press OK to continue
               MOVE 306                    TO CRT-ERROR-NBR
               MOVE BNBEN-STOP-DATE-FN(I1) TO CRT-FIELD-NBR
               MOVE 1                      TO BNBEN-DED-OVER-XMIT
               GO TO 2500-END.

           IF  (BNBEN-DED-START            NOT = ZEROES)
           AND (BNBEN-DED-START            > BNBEN-STOP-DATE (I1))
           AND (BNBEN-STOP-DATE (I1)       NOT = ZEROES)
      ******** Cannot override; Deduction start date > benefit stop date
               MOVE 307                    TO CRT-ERROR-NBR
               MOVE BNBEN-DED-START-DATE-FN (I1)
                                           TO CRT-FIELD-NBR
               GO TO 2500-END.

           IF  (BNBEN-DED-STOP             NOT = ZEROES)
           AND (BNBEN-DED-STOP             < BNBEN-START-DATE (I1))
      ******** Cannot override; Deduction stop date < benefit start date
               MOVE 308                    TO CRT-ERROR-NBR
               MOVE BNBEN-DED-STOP-DATE-FN (I1)
                                           TO CRT-FIELD-NBR
               GO TO 2500-END.

           IF (BNBEN-LINE-FC (I1)          = "A" OR "C" OR "S")
J01388         INITIALIZE BNWS-COMP-MATCH
J01388                    BNWS-COMP-CONT
               PERFORM 5600-SET-BNEMDED-VARS

               INITIALIZE BNBEN-DED-TABLE
                          I3

               MOVE "E"                    TO BNWS-DEDEDIT-TYPE

               IF (BNBEN-LINE-FC (I1)      = "A")
                   PERFORM 620-ADD-EMDEDREC
               ELSE
                   PERFORM 600-STOP-EXISTING-EMDEDREC
               END-IF

               INITIALIZE BNWS-DEDEDIT-TYPE.

           PERFORM
               VARYING I3 FROM 1 BY 1
               UNTIL  (BNBEN-DED-CODE (I3) = SPACES)

               MOVE BNBEN-COMPANY          TO DB-COMPANY
               MOVE BNBEN-EMPLOYEE (I1)    TO DB-EMPLOYEE
               MOVE BNBEN-DED-CODE (I3)    TO DB-DED-CODE
               MOVE EDMSET1-DED-CODE       TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-EDMSET1
P85709         IF   (EMDEDMASTR-NOTFOUND)
P85709         AND ((CRT-PROGRAM-CODE      = "BN531")
P85709         OR   (CRT-PROGRAM-CODE      = "BN31"))
P85709             IF (CRT-PROGRAM-CODE      = "BN531")
P85709                 MOVE 2               TO BEN-COUNT
P85709             ELSE
P86476                 IF (I1 < 12)
P85709                     MOVE 12 TO BEN-COUNT
P86476                 ELSE
P86476                     COMPUTE BEN-COUNT = I1 + 1
P86476                 END-IF
P85709             END-IF
P85709             PERFORM 2501-CHECK
P85709             THRU    2501-END
P85709                 UNTIL  (BEN-COUNT > 30)
P85709                 OR     (ERROR-FOUND)
P85709                 OR    ((BNBEN-START-DATE (BEN-COUNT) = ZEROES)
P85709                 AND    (BNBEN-STOP-DATE (BEN-COUNT)  = ZEROES))
P85709             GO TO 2500-END
P85709         END-IF
               PERFORM
                   UNTIL (EMDEDMASTR-NOTFOUND)
                   OR    (ERROR-FOUND)

                   PERFORM 2502-COMPARE-DATES
                   THRU    2502-END

                   PERFORM 860-FIND-NXTRNG-EDMSET1
               END-PERFORM
           END-PERFORM.

       2500-END.

P85709******************************************************************
P85709 2501-CHECK.         
P85700******************************************************************
P85709
P86691     IF (BNBEN-DED-START-DATE (I1) >
P85709                                BNBEN-START-DATE (BEN-COUNT))
P86691     OR (BNBEN-DED-STOP-DATE (I1)  >=
P85709                                BNBEN-START-DATE (BEN-COUNT))
J32750         IF  (BNBEN-PLAN-TYPE (I1) = BNBEN-PLAN-TYPE (BEN-COUNT))
J32750         AND (BNBEN-PLAN-CODE (I1) = BNBEN-PLAN-CODE (BEN-COUNT))
P85709******** Cannot override; Deduction stop date < benefit start date
P85709*------- Deduction dates overlap; Check override dates
P85709             MOVE 303                TO CRT-ERROR-NBR
P85709             MOVE BNBEN-DED-STOP-DATE-FN (I1)
P85709                                     TO CRT-FIELD-NBR
P85709             GO TO 2501-END
J32750         END-IF  
J32750     END-IF.  
P85709
P85709     ADD 1 TO BEN-COUNT.
P85709
P85709 2501-END.
P85709
153200******************************************************************
       2502-COMPARE-DATES.
153200******************************************************************

J32750     COMPUTE BNBEN-I4 = (I1 + 1).
J32750     COMPUTE BNBEN-I5 = (I1 - 1).
J32750     
P86476     IF  (CRT-PROGRAM-CODE       = "BN31" OR "BN32" OR "BN531")
P86476     AND (BNBEN-LINE-FC (I1)     = "A")
P86476         MOVE EDM-EFFECT-DATE        TO BNBEN-EFFECT-DATE
P86476         MOVE EDM-END-DATE           TO BNBEN-END-DATE
P86476     END-IF.
P86476
           IF (BNBEN-LINE-FC (I1)          = "A")
               PERFORM
                   VARYING I4 FROM 1 BY 1
                   UNTIL  (BNBEN-EDM-CODE (I4)  = SPACES)
                   OR     ((BNBEN-EDM-CODE (I4) = EDM-DED-CODE)
                   AND     (BNBEN-EDM-SEQ (I4)  = EDM-SEQ-NBR))

                   CONTINUE
               END-PERFORM
               IF  (I4                     <= 25)
               AND (BNBEN-EDM-CODE (I4)    NOT = SPACES)
                   MOVE BNBEN-EDM-END (I4) TO BNBEN-END-DATE
               END-IF
           ELSE
           IF  (BNBEN-LINE-FC (I1)         = "C" OR "S")
           AND (BNBEN-DED-SEQ (I3)         = EDM-SEQ-NBR)
P86476     AND (BNBEN-DED-STOP-DATE (I1)   = EDM-END-DATE)
      *------- Can't compare dates on the same deduction that we are trying
      *------- to change
               GO TO 2502-END.

P86476     IF  (CRT-PROGRAM-CODE       = "BN31" OR "BN32" OR "BN531")
P86476     AND (BNBEN-LINE-FC (I1)     = "A")
P86476         IF (BNBEN-DED-START-DATE (I1) > BNBEN-END-DATE)
P86476             GO TO 2502-END
P86476         END-IF
P86476         IF   ((BNBEN-DED-START-DATE (I1) >= BNBEN-EFFECT-DATE)
P86476         AND   (BNBEN-DED-START-DATE (I1) <= BNBEN-END-DATE)
P32750         AND   (BNBEN-DED-START-DATE (I1) NOT = ZEROES))
P86476
P86476         OR  ((BNBEN-DED-STOP-DATE (I1)   >= BNBEN-EFFECT-DATE)
P86476         AND  (BNBEN-DED-STOP-DATE (I1)   <= BNBEN-END-DATE)
P32750         AND  (BNBEN-DED-STOP-DATE (I1)   NOT = ZEROES))
P86476
P86476         OR  ((BNBEN-DED-START-DATE (I1)  <= BNBEN-EFFECT-DATE)
P86476         AND  (BNBEN-DED-STOP-DATE (I1)   >= BNBEN-END-DATE)
P32750         AND  (BNBEN-DED-START-DATE (I1)  NOT = ZEROES)
P32750         AND  (BNBEN-DED-STOP-DATE (I1)   NOT = ZEROES))
P86476
P86476******** Deduction dates overlap; Check override dates
P86476             MOVE 303                TO CRT-ERROR-NBR
P86476             MOVE BNBEN-DED-START-DATE-FN (I1)
P86476                                     TO CRT-FIELD-NBR
P86476             GO TO 2502-END
P86476         END-IF
P86476     END-IF.
P86476
P86476     IF (I1 = 1)
P86476         IF  (BNBEN-DED-STOP-DATE (I1) NOT = EDM-END-DATE)
P86476         AND (BNBEN-DED-START-DATE (I1 + 1) NOT = ZEROES)
P86476         AND (BNBEN-DED-STOP-DATE (I1) >=
P86476                                  BNBEN-DED-START-DATE (BNBEN-I4))
J32750         AND (BNBEN-PLAN-TYPE (I1) = BNBEN-PLAN-TYPE (BNBEN-I4))
J32750         AND (BNBEN-PLAN-CODE (I1) = BNBEN-PLAN-CODE (BNBEN-I4))
P86476********* Deduction dates overlap; Check override dates
P86476             MOVE 303                          TO CRT-ERROR-NBR
P86476             MOVE BNBEN-DED-STOP-DATE-FN (I1)  TO CRT-FIELD-NBR
P86476         END-IF
P86476         GO TO 2502-END
P86476     ELSE
P86476         IF  (((BNBEN-DED-START-DATE (I1) >= 
P86476                                BNBEN-DED-START-DATE (BNBEN-I5))
P86476         AND   (BNBEN-DED-START-DATE (I1) <= 
P86476                                BNBEN-DED-STOP-DATE  (BNBEN-I5))
J24315         AND   (BNBEN-DED-START-DATE (I1) NOT = ZEROES)
J32750         AND   (BNBEN-PLAN-TYPE (I1) =
J32750                                     BNBEN-PLAN-TYPE (BNBEN-I5))
J32750         AND   (BNBEN-PLAN-CODE (I1) =
J32750                                     BNBEN-PLAN-CODE (BNBEN-I5))
J24315         AND   (BNBEN-DED-START-DATE (BNBEN-I5) NOT = ZEROES))
P86476         OR  ((BNBEN-DED-STOP-DATE (I1)   >=
P86476                                BNBEN-DED-START-DATE (BNBEN-I5))
P86476         AND  (BNBEN-DED-STOP-DATE (I1)   <=
P86476                                BNBEN-DED-STOP-DATE  (BNBEN-I5))
J24315         AND  (BNBEN-DED-START-DATE (I1) NOT = ZEROES)
J32750         AND  (BNBEN-PLAN-TYPE (I1) = BNBEN-PLAN-TYPE (BNBEN-I5))
J32750         AND  (BNBEN-PLAN-CODE (I1) = BNBEN-PLAN-CODE (BNBEN-I5))
J24315         AND  (BNBEN-DED-START-DATE (BNBEN-I5) NOT = ZEROES))
P86476         OR  ((BNBEN-DED-START-DATE (I1)  <= 
P86476                                BNBEN-DED-START-DATE (BNBEN-I5))
P86476         AND  (BNBEN-DED-STOP-DATE (I1)   >= 
P86476                                BNBEN-DED-STOP-DATE (BNBEN-I5))
J24315         AND  (BNBEN-DED-START-DATE (I1) NOT = ZEROES)
J32750         AND  (BNBEN-PLAN-TYPE (I1) = BNBEN-PLAN-TYPE (BNBEN-I5))
J32750         AND  (BNBEN-PLAN-CODE (I1) = BNBEN-PLAN-CODE (BNBEN-I5))
J24315         AND  (BNBEN-DED-START-DATE (BNBEN-I5) NOT = ZEROES)))

      ******** Deduction dates overlap; Check override dates
               MOVE 303                          TO CRT-ERROR-NBR
               MOVE BNBEN-DED-START-DATE-FN (I1) TO CRT-FIELD-NBR.

       2502-END.

153200******************************************************************
       2550-EDIT-DED-OVERRIDE.
153200******************************************************************

      *--- This is to check ded override dates for benefits that are
      *--- automatically added for future dated cov/prem records
      *--- There may be more than one benefits that can be added, but
      *--- need to check only the first such one because users cannot
      *--- enter override dates for auto added benefits

           MOVE BNBEN-EMPLOYEE (I1)        TO BNBEN-SV-EMP.
           MOVE BNBEN-PLAN-TYPE (I1)       TO BNBEN-SV-PL-TP.
           MOVE BNBEN-PLAN-CODE (I1)       TO BNBEN-SV-PL-CD.

           PERFORM
               VARYING I5 FROM 1 BY 1
               UNTIL  (I5                    > BNBEN-ORIG-NBR-LINES)
               OR     ((BNBEN-EMPLOYEE (I5)  = BNBEN-SV-EMP)
               AND     (BNBEN-PLAN-TYPE (I5) = BNBEN-SV-PL-TP)
               AND     (BNBEN-PLAN-CODE (I5) = BNBEN-SV-PL-CD))

               CONTINUE
           END-PERFORM.
           IF (I5                          > BNBEN-ORIG-NBR-LINES)
               GO TO 2550-END.

      *--- If there are no override dates on benefit entered on screen
      *--- no need to check override dates
           IF  ((BNBEN-DED-START-DATE (I5)  = ZEROES)
           AND  (BNBEN-DED-STOP-DATE (I5)   = ZEROES))
P79232     OR   (BNBEN-DED-START-DATE (I5) >= BNBEN-START-DATE (I5))
               GO TO 2550-END.

      *--- Here we will have to process already existing EDMs + EDMs that
      *--- will be added for benefit entered on screen + EDMs for benefit that
      *--- will be auto added.

      *--- Create a table BNBEN-EDM-TABLE for benefit entered on screen using
      *--- 620-ADD-EMDEDREC
      *--- Create a table BNBEN-DED-TABLE for benefit that will be auto-added
      *--- using 620-ADD-EMDEDREC

      *--- Create BNBEN-DED-TABLE
           PERFORM 5600-SET-BNEMDED-VARS.

           INITIALIZE BNBEN-DED-TABLE
                          I3.

           MOVE "E"                    TO BNWS-DEDEDIT-TYPE.

           PERFORM 620-ADD-EMDEDREC.

           MOVE I1                     TO BNBEN-SV-I1.

           MOVE I5                     TO I1.

      *--- Create BNBEN-EDM-TABLE
           PERFORM 5600-SET-BNEMDED-VARS.

           INITIALIZE BNBEN-DED-TABLE
                      I3.

           MOVE "A"                    TO BNWS-DEDEDIT-TYPE.

           PERFORM 620-ADD-EMDEDREC.

           MOVE BNBEN-SV-I1            TO I1.

       2550-END.

153200******************************************************************
043300 2600-EDIT-DTL-DELETE.
153200******************************************************************

           IF  (BNBEN-STOP-DATE (I1)       NOT = BEN-STOP-DATE)
J57057     AND (BNBEN-PLAN-TYPE (I1)           = "FL")
      ******** Cannot change stop date with delete function code
               MOVE 123                    TO CRT-ERROR-NBR
               MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
               GO TO 2600-END
J57057     END-IF.

           PERFORM 2602-EDIT-YTD
           THRU    2602-END.
           IF (ERROR-FOUND)
065500         MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
065600         GO TO 2600-END.
065700
           COMPUTE BNBEN-EMP-CONT          = BEN-CMP-FLX-CONT
                                           + BEN-EMP-PRE-CONT
                                           + BEN-EMP-AFT-CONT.
           MOVE BEN-COMP-CONT              TO BNBEN-CMP-CONT.
           IF  (BNBEN-CMP-CONT             = ZEROES)
           AND (BNBEN-EMP-CONT             = ZEROES)
               MOVE WS-TRUE                TO BNBEN-ZERO-PREMIUM-SW (I1)
P54229         MOVE WS-FALSE               TO BNBEN-NEG-PREMIUM-SW (I1)
P54229         MOVE WS-FALSE               TO BNBEN-POS-PREMIUM-SW (I1)
           ELSE
           IF  (BNBEN-EMP-CONT             < ZEROES)
           AND (BNBEN-CMP-CONT             NOT < ZEROES)
               MOVE WS-TRUE                TO BNBEN-NEG-PREMIUM-SW (I1)
P54229         MOVE WS-FALSE               TO BNBEN-ZERO-PREMIUM-SW (I1)
P54229         MOVE WS-FALSE               TO BNBEN-POS-PREMIUM-SW (I1)
           ELSE
P54229         MOVE WS-FALSE               TO BNBEN-ZERO-PREMIUM-SW (I1)
P54229         MOVE WS-FALSE               TO BNBEN-NEG-PREMIUM-SW (I1)
               MOVE WS-TRUE                TO BNBEN-POS-PREMIUM-SW (I1).

065800     IF (BNBEN-FLEX-FLAG (I1)        = "Y")
               PERFORM 2610-EDIT-FLEX
               THRU    2610-END
               IF (ERROR-FOUND)
                   GO TO 2600-END.

           IF (BNBEN-PLAN-TYPE (I1)        = "DI" OR "EL" OR "DC")
      *    OR (BNBEN-PLAN-TYPE (I1)        = "DB")
AI0030     OR (BNBEN-PLAN-TYPE (I1)        = "DB" OR "DL" OR "HL")         
               PERFORM 2620-EDIT-BENEFICRY
               THRU    2620-END
               IF (ERROR-FOUND)
                   GO TO 2600-END.

P58843     IF  (BNBEN-PLAN-TYPE (I1)        = "DC" OR "DB")
P58843     AND (NOT-ZERO-YTD-CONT)       
P58843          PERFORM 2630-EDIT-COMPHIST
P58843          THRU    2630-END
P58843          IF (ERROR-FOUND)
P58843              GO TO 2600-END.

P58843     IF  (BNBEN-PLAN-TYPE (I1)        = "DC" OR "DB")
P58843     AND (NOT-ZERO-YTD-CONT)
P58843          PERFORM 2640-EDIT-CODA
P58843          THRU    2640-END
P58843          IF (ERROR-FOUND)
P58843              GO TO 2600-END.

           IF (BNBEN-PLAN-TYPE (I1)        = "RS")
               PERFORM 2680-EDIT-RESTRANS
               THRU    2680-END
               IF (ERROR-FOUND)
                   GO TO 2600-END.

           IF  ((BNBEN-PLAN-TYPE (I1)      = "HL" OR "DN" OR "EL")
           OR   (BNBEN-PLAN-TYPE (I1)      = "DL"))
           AND (CRT-PROGRAM-CODE           NOT = "BN100")
               PERFORM 2700-EDIT-DEPENDENTS
               THRU    2700-END
               IF (ERROR-FOUND)
                   GO TO 2600-END.

           PERFORM 2850-EDIT-HIPAA
           THRU    2850-END.

           IF (BNBEN-TRIGGER-ENABLED-SW (I1) = WS-TRUE)
               PERFORM 2710-EDIT-LP-DELETE
               THRU    2710-END
               IF (ERROR-FOUND)
                   GO TO 2600-END.

P42026     MOVE BNBEN-COMPANY              TO DB-COMPANY.
P42026     MOVE BNBEN-EMPLOYEE   (I1)      TO DB-EMPLOYEE.
P42026     MOVE BNBEN-PLAN-TYPE  (I1)      TO DB-PLAN-TYPE. 
P42026     MOVE BNBEN-PLAN-CODE  (I1)      TO DB-PLAN-CODE.
P42026     MOVE BNBEN-START-DATE (I1)      TO DB-START-DATE.
P42026     PERFORM 840-FIND-BENSET1.
P42026
043400 2600-END.

069300******************************************************************
       2602-EDIT-YTD.
069300******************************************************************
P58843     INITIALIZE BEN-YTD-CONT-SW.      
P58843     MOVE BEN-START-DATE             TO BNBEN-START-DATE-CALC.
           IF (BEN-FLEX-DED-SEQ            NOT = ZEROES)
               MOVE PLN-FLEX-DED-CODE      TO DB-DED-CODE
               MOVE BEN-FLEX-DED-SEQ       TO DB-SEQ-NBR
               PERFORM 840-FIND-EDMSET1
               MOVE PDHSET1-SEQ-NBR        TO WS-DB-BEG-RNG
P58843         PERFORM 850-FIND-BEGRNG-PDHSET1
J49766         IF  (PREMDEDHST-FOUND)
J49766         AND (PDH-DED-AMT (13)       = ZEROS)
J49766         AND (PDH-YEAR               < BNBEN-START-YYYY)
J49766             PERFORM
J49766                 UNTIL (PDH-DED-AMT (13) NOT = ZEROS)  
J49766                 OR    (PREMDEDHST-NOTFOUND)
J49766
J49766                 PERFORM 860-FIND-NXTRNG-PDHSET1
J49766
J49766             END-PERFORM
J49766         END-IF
P58843         IF  (PREMDEDHST-FOUND)
P58843         AND (PDH-YEAR                   >= BNBEN-START-YYYY)
P58843              IF (PDH-DED-AMT (13)   NOT  = ZEROS)
      ************ Cannot delete; deduction has been taken
P58843                  MOVE 104                TO CRT-ERROR-NBR
P58843                  GO TO 2602-END
P58843              ELSE
P58843              IF (BEN-PLAN-TYPE      = "DB" OR "DC") 
P58843                  SET ZERO-YTD-CONT  TO TRUE
P58843              END-IF
P58843              END-IF  
               END-IF

               PERFORM 2604-EDIT-ONETMDED
               THRU    2604-END
               IF (ERROR-FOUND)
                   GO TO 2602-END
               END-IF
P58843         IF (PREMDEDHST-NOTFOUND) 
P58843         OR ((PREMDEDHST-FOUND)
P58843         AND (PDH-DED-AMT (13)      NOT  = ZEROS))
P58843              PERFORM 2606-EDIT-PAYDEDUCTN
P58843              THRU    2606-END
P58843              IF (ERROR-FOUND)
P58843                  GO TO 2602-END
P58843              END-IF
P58843         END-IF.

           IF (BEN-PRE-SEQ-NBR             NOT = ZEROES)
               IF (BEN-PCT-AMT-FLAG        = "P")
                   MOVE PLN-PRE-DED-CODE-P TO DB-DED-CODE
               ELSE
                   MOVE PLN-PRE-DED-CODE-A TO DB-DED-CODE
               END-IF
               MOVE BEN-PRE-SEQ-NBR        TO DB-SEQ-NBR
               PERFORM 840-FIND-EDMSET1
               MOVE PDHSET1-SEQ-NBR        TO WS-DB-BEG-RNG
P58843         PERFORM 850-FIND-BEGRNG-PDHSET1
J49766         IF  (PREMDEDHST-FOUND)
J49766         AND (PDH-DED-AMT (13)       = ZEROS)
J49766         AND (PDH-YEAR               < BNBEN-START-YYYY)
J49766             PERFORM
J49766                 UNTIL (PDH-DED-AMT (13) NOT = ZEROS)  
J49766                 OR    (PREMDEDHST-NOTFOUND)
J49766
J49766                 PERFORM 860-FIND-NXTRNG-PDHSET1
J49766
J49766             END-PERFORM
J49766         END-IF
P58843         IF  (PREMDEDHST-FOUND)
P58843         AND (PDH-YEAR                   >= BNBEN-START-YYYY)   
P58843              IF (PDH-DED-AMT(13)    NOT  = ZEROS)
      ************ Cannot delete; deduction has been taken
P58843                  MOVE 104                TO CRT-ERROR-NBR
P58843                  GO TO 2602-END
P58843              ELSE
P58843              IF (BEN-PLAN-TYPE      = "DB" OR "DC") 
P58843                  SET ZERO-YTD-CONT  TO TRUE
P58843              END-IF
P58843              END-IF  
               END-IF
               PERFORM 2604-EDIT-ONETMDED
               THRU    2604-END
               IF (ERROR-FOUND)
                   GO TO 2602-END
               END-IF
P58843         IF (PREMDEDHST-NOTFOUND) 
P58843         OR ((PREMDEDHST-FOUND)
P58843         AND (PDH-DED-AMT (13)        NOT = ZEROS))
P58843              PERFORM 2606-EDIT-PAYDEDUCTN
P58843              THRU    2606-END
P58843              IF (ERROR-FOUND)
P58843                  GO TO 2602-END
P58843              END-IF
P58843         END-IF
P58843     END-IF.

           IF (BEN-AFT-SEQ-NBR             NOT = ZEROES)
               IF (BEN-PCT-AMT-FLAG        = "P")
                   MOVE PLN-AFT-DED-CODE-P TO DB-DED-CODE
               ELSE
                   MOVE PLN-AFT-DED-CODE-A TO DB-DED-CODE
               END-IF
               MOVE BEN-AFT-SEQ-NBR        TO DB-SEQ-NBR
               PERFORM 840-FIND-EDMSET1
               MOVE PDHSET1-SEQ-NBR        TO WS-DB-BEG-RNG
P58843         PERFORM 850-FIND-BEGRNG-PDHSET1
J49766         IF  (PREMDEDHST-FOUND)
J49766         AND (PDH-DED-AMT (13)       = ZEROS)
J49766         AND (PDH-YEAR               < BNBEN-START-YYYY)
J49766             PERFORM
J49766                 UNTIL (PDH-DED-AMT (13) NOT = ZEROS)  
J49766                 OR    (PREMDEDHST-NOTFOUND)
J49766
J49766                 PERFORM 860-FIND-NXTRNG-PDHSET1
J49766
J49766             END-PERFORM
J49766         END-IF
P58843         IF  (PREMDEDHST-FOUND)
P58843         AND (PDH-YEAR                   >= BNBEN-START-YYYY)   
P58843              IF (PDH-DED-AMT(13)    NOT  = ZEROS)
      ************ Cannot delete; deduction has been taken
P58843                  MOVE 104           TO CRT-ERROR-NBR
P58843                  GO TO 2602-END
P58843              ELSE   
P58843              IF (BEN-PLAN-TYPE      = "DB" OR "DC")
P58843                  SET ZERO-YTD-CONT  TO TRUE   
P58843              END-IF
P58843              END-IF
               END-IF
               PERFORM 2604-EDIT-ONETMDED
               THRU    2604-END
               IF (ERROR-FOUND)
                   GO TO 2602-END
               END-IF
P58843         IF (PREMDEDHST-NOTFOUND) 
P58843         OR ((PREMDEDHST-FOUND)
P58843         AND (PDH-DED-AMT (13)       NOT = ZEROS))
P58843              PERFORM 2606-EDIT-PAYDEDUCTN
P58843              THRU    2606-END
P58843              IF (ERROR-FOUND)
P58843                  GO TO 2602-END
P58843              END-IF
P58843         END-IF
P58843     END-IF.

           IF (BEN-CMP-SEQ-NBR             NOT = ZEROES)
               IF (BEN-PCT-AMT-FLAG        = "P")
                   MOVE PLN-CMP-DED-CODE-P TO DB-DED-CODE
               ELSE
P58993         IF (BEN-PCT-AMT-FLAG        = "A") 
                   MOVE PLN-CMP-DED-CODE-A TO DB-DED-CODE
P58993         ELSE
P58993         IF (PLN-CMP-DED-CODE-A  NOT = SPACES)
P58993             MOVE PLN-CMP-DED-CODE-A TO DB-DED-CODE
P58993         ELSE
P58993         IF (PLN-CMP-DED-CODE-P  NOT = SPACES)
P58993             MOVE PLN-CMP-DED-CODE-P TO DB-DED-CODE
P58993         END-IF
P58993         END-IF
P58993         END-IF
P58993         END-IF
               MOVE BEN-CMP-SEQ-NBR        TO DB-SEQ-NBR
               PERFORM 840-FIND-EDMSET1
               IF (EMDEDMASTR-NOTFOUND)
                   IF (BEN-PCT-AMT-FLAG    = "P")
                       MOVE PLN-PRE-DED-MTCH-P TO DB-DED-CODE
                   ELSE
                       MOVE PLN-PRE-DED-MTCH-A TO DB-DED-CODE
                   END-IF
                   PERFORM 840-FIND-EDMSET1
               END-IF
               MOVE PDHSET1-SEQ-NBR        TO WS-DB-BEG-RNG
P58843         PERFORM 850-FIND-BEGRNG-PDHSET1
J49766         IF  (PREMDEDHST-FOUND)
J49766         AND (PDH-DED-AMT (13)       = ZEROS)
J49766         AND (PDH-YEAR               < BNBEN-START-YYYY)
J49766             PERFORM
J49766                 UNTIL (PDH-DED-AMT (13) NOT = ZEROS)  
J49766                 OR    (PREMDEDHST-NOTFOUND)
J49766
J49766                 PERFORM 860-FIND-NXTRNG-PDHSET1
J49766
J49766             END-PERFORM
J49766         END-IF
P58843         IF  (PREMDEDHST-FOUND)
P58843         AND (PDH-YEAR                  >= BNBEN-START-YYYY)      
P58843             IF (PDH-DED-AMT (13)    NOT  = ZEROS)
      ************ Cannot delete; deduction has been taken
P58843                 MOVE 104            TO CRT-ERROR-NBR
P58843                 GO TO 2602-END
P58843             ELSE 
P58843             IF (BEN-PLAN-TYPE       = "DB" OR "DC")
P58843                 SET ZERO-YTD-CONT   TO TRUE
P58843             END-IF
P58843             END-IF
P58843         END-IF
               PERFORM 2604-EDIT-ONETMDED
               THRU    2604-END
               IF (ERROR-FOUND)
                   GO TO 2602-END
               END-IF
P58843         IF (PREMDEDHST-NOTFOUND) 
P58843         OR ((PREMDEDHST-FOUND)
P58843         AND (PDH-DED-AMT (13)       NOT = ZEROS))
P58843              PERFORM 2606-EDIT-PAYDEDUCTN
P58843              THRU    2606-END
P58843              IF (ERROR-FOUND)
P58843                  GO TO 2602-END
P58843              END-IF
P58843         END-IF
P58843     END-IF.    

           IF (BEN-CMP-AFT-SEQ             NOT = ZEROES)
               IF (BEN-PCT-AMT-FLAG        = "P")
                   MOVE PLN-AFT-DED-MTCH-P TO DB-DED-CODE
               ELSE
                   MOVE PLN-AFT-DED-MTCH-A TO DB-DED-CODE
               END-IF
               MOVE BEN-CMP-AFT-SEQ        TO DB-SEQ-NBR
               PERFORM 840-FIND-EDMSET1
               MOVE PDHSET1-SEQ-NBR        TO WS-DB-BEG-RNG
P58843         PERFORM 850-FIND-BEGRNG-PDHSET1
J49766         IF  (PREMDEDHST-FOUND)
J49766         AND (PDH-DED-AMT (13)       = ZEROS)
J49766         AND (PDH-YEAR               < BNBEN-START-YYYY)
J49766             PERFORM
J49766                 UNTIL (PDH-DED-AMT (13) NOT = ZEROS)  
J49766                 OR    (PREMDEDHST-NOTFOUND)
J49766
J49766                 PERFORM 860-FIND-NXTRNG-PDHSET1
J49766
J49766             END-PERFORM
J49766         END-IF
P58843         IF  (PREMDEDHST-FOUND)
P58843         AND (PDH-YEAR                  >= BNBEN-START-YYYY)
P58843              IF (PDH-DED-AMT(13)   NOT  = ZEROS)
      ************ Cannot delete; deduction has been taken
P58843                  MOVE 104                TO CRT-ERROR-NBR
P58843                  GO TO 2602-END
P58843              ELSE 
P58843              IF (BEN-PLAN-TYPE      = "DB" OR "DC")
P58843                  SET ZERO-YTD-CONT  TO TRUE
P58843              END-IF
P58843              END-IF  
P58843         END-IF
               PERFORM 2604-EDIT-ONETMDED
               THRU    2604-END
               IF (ERROR-FOUND)
                   GO TO 2602-END
               END-IF
P58843         IF (PREMDEDHST-NOTFOUND) 
P58843         OR ((PREMDEDHST-FOUND)
P58843         AND (PDH-DED-AMT (13)       NOT = ZEROS))
P58843              PERFORM 2606-EDIT-PAYDEDUCTN
P58843              THRU    2606-END
P58843              IF (ERROR-FOUND)
P58843                 GO TO 2602-END
P58843              END-IF
P58843         END-IF
P58843     END-IF.

       2602-END.

069300******************************************************************
       2604-EDIT-ONETMDED.
069300******************************************************************

           IF (EMDEDMASTR-FOUND)
               MOVE EDM-COMPANY            TO DB-COMPANY
               MOVE EDM-EMPLOYEE           TO DB-EMPLOYEE
               MOVE EDM-DED-CODE           TO DB-DED-CODE
               MOVE EDM-SEQ-NBR            TO DB-EDM-SEQ-NBR
               MOVE OTDSET1-EDM-SEQ-NBR    TO WS-DB-BEG-RNG
               PERFORM 850-KFIND-BEGRNG-OTDSET1
               IF (ONETMDED-KFOUND)
                   IF (BNBEN-LINE-FC (I1)          = "D")
      ************ Cannot delete; one time deductions exist
                       MOVE 217            TO CRT-ERROR-NBR
                   ELSE
                       IF (BEN-PLAN-TYPE = "RS" OR "SB" OR "SP"
                                        OR "DC" OR "DB")
      ************ One time deds exist; Delete one time ded
                           MOVE 222        TO CRT-ERROR-NBR
                       ELSE
      ************ WARNING One time deds exist
                           MOVE "BNBEN"    TO BNBEN-PASS-ERROR-CAT
                           MOVE 223        TO BNBEN-PASS-MSG-NBR
                       END-IF
                   END-IF
                   MOVE PREDM-FC-FN        TO CRT-FIELD-NBR
                   GO TO 2604-END.

       2604-END.

069300******************************************************************
       2606-EDIT-PAYDEDUCTN.
069300******************************************************************

           IF (EMDEDMASTR-FOUND)
               CALL "USEEARLYBINDFILTERS"  USING WS-TRUE
               INITIALIZE FILTER-STRING
               MOVE EDM-COMPANY            TO DB-COMPANY
               MOVE EDM-EMPLOYEE           TO DB-EMPLOYEE
               MOVE PYDSET1-EMPLOYEE       TO WS-DB-BEG-RNG
               STRING "((PYD-DED-CODE = ?) AND "
                      "(PYD-EDM-SEQ-NBR = ?))"
                      DELIMITED BY SIZE INTO FILTER-STRING
               MOVE EDM-DED-CODE           TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
               MOVE EDM-SEQ-NBR            TO NUMERIC-FILTER-VALUE
               PERFORM 890-SET-NUMERIC-FILTER-VALUE
               PERFORM 890-CREATE-FILTER
               PERFORM 850-FILTER-BEGRNG-PYDSET1
               IF (PAYDEDUCTN-FOUND)
      ************ Cannot delete; payments exist
                   MOVE 218                TO CRT-ERROR-NBR
                   MOVE PREDM-FC-FN        TO CRT-FIELD-NBR
                   GO TO 2606-END.
016400
       2606-END.

069300******************************************************************
       2610-EDIT-FLEX.
069300******************************************************************

           PERFORM 
               VARYING I7 FROM 1 BY 1
               UNTIL  (I7 > BNFRWS-MAX-PERIODS)
               OR    ((BNBEN-START-DATE (I1) 
                                           >= BNFRWS-START-DATE (I6 I7))
               AND    (BNBEN-STOP-DATE (I1) 
                                           <= BNFRWS-STOP-DATE (I6 I7)))
               OR    ((BNBEN-START-DATE (I1)
                                           >= BNFRWS-START-DATE (I6 I7))
               AND    (BNBEN-START-DATE (I1)
                                           <= BNFRWS-STOP-DATE (I6 I7))
               AND    (BNBEN-STOP-DATE (I1)
                                           >= BNFRWS-STOP-DATE (I1 I1)))
               OR     (BNFRWS-START-DATE (I6 I7)
                                           = ZEROES)

               CONTINUE
           END-PERFORM.

           PERFORM 
               UNTIL (BNFRWS-START-DATE (I6 I7) = ZEROES)
               OR    (I7                        > BNFRWS-MAX-PERIODS)
               OR    (ERROR-FOUND)

               IF (BNBEN-STOP-DATE (I1) >= BNFRWS-STOP-DATE (I6 I7))
066600             COMPUTE BNFRWS-CREDITS-AVAIL (I6 I7)
                      = BNFRWS-CREDITS-AVAIL (I6 I7)
                      + BEN-CMP-FLX-CONT
066800             COMPUTE BNFRWS-PRE-TAX-AVAIL (I6 I7)
                      = BNFRWS-PRE-TAX-AVAIL (I6 I7)
                      + BEN-EMP-PRE-CONT
067000             COMPUTE BNFRWS-TOTAL-AVAIL 
                      = BNFRWS-CREDITS-AVAIL (I6 I7)
067100                + BNFRWS-PRE-TAX-AVAIL (I6 I7)
               END-IF
06720
               IF  (BNFRWS-CREDITS-AVAIL (I6 I7)  < ZEROES)
               AND (PLN-FLEX-PAY-CODE             = SPACES)
    ************** Cannot delete; dollars have been spend
                   MOVE 178                    TO CRT-ERROR-NBR
                   MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
               END-IF

               ADD 1                   TO I7
           END-PERFORM.

       2610-END.

069300******************************************************************
       2620-EDIT-BENEFICRY.
069300******************************************************************

217200     MOVE BNBEN-COMPANY              TO DB-COMPANY.
217300     MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
217400     MOVE BNBEN-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
217500     MOVE BNBEN-PLAN-CODE (I1)       TO DB-PLAN-CODE.
217600     INITIALIZE DB-SEQ-NBR.
217700     PERFORM 850-FIND-NLT-BNFSET1.

065500     IF  (BENEFICRY-FOUND)
065600     AND (BNF-COMPANY                = DB-COMPANY)
065700     AND (BNF-EMPLOYEE               = DB-EMPLOYEE)
065800     AND (BNF-PLAN-TYPE              = DB-PLAN-TYPE)
065900     AND (BNF-PLAN-CODE              = DB-PLAN-CODE)
P26300*        INITIALIZE DB-START-DATE
P26300         MOVE BENSET4-PLAN-CODE      TO WS-DB-BEG-RNG
066100         MOVE WS-FALSE               TO BNBEN-BENEFS-SW
P26300*        PERFORM 850-FIND-NLT-BENSET4
P26300         PERFORM 850-FIND-BEGRNG-BENSET4
066300         PERFORM 2690-EDIT-SCR-DELETE
066400         THRU    2690-END
066500             UNTIL (BNBEN-BENEFS-OK)
066600             OR    (BENEFIT-NOTFOUND)
P26300*            OR    (BEN-COMPANY      NOT = DB-COMPANY)
P26300*            OR    (BEN-EMPLOYEE     NOT = DB-EMPLOYEE)
P26300*            OR    (BEN-PLAN-TYPE    NOT = DB-PLAN-TYPE)
P26300*            OR    (BEN-PLAN-CODE    NOT = DB-PLAN-CODE)
067100         IF (BNBEN-BENEFS-NOT-OK)
      ************ Cannot delete; beneficiaries exist
067200             MOVE 101                    TO CRT-ERROR-NBR
067300             MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
067400             GO TO 2620-END.

       2620-END.

153200******************************************************************
       2630-EDIT-COMPHIST.
153200******************************************************************

208000     MOVE BNBEN-COMPANY              TO DB-COMPANY.
208100     MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
208200     MOVE BNBEN-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
208300     MOVE BNBEN-PLAN-CODE (I1)       TO DB-PLAN-CODE.
208400     INITIALIZE DB-PLAN-YEAR.
208500     PERFORM 850-FIND-NLT-CPHSET1.
071600
071700     IF  (COMPHIST-FOUND)
071800     AND (CPH-COMPANY                = DB-COMPANY)
071900     AND (CPH-EMPLOYEE               = DB-EMPLOYEE)
072000     AND (CPH-PLAN-TYPE              = DB-PLAN-TYPE)
072100     AND (CPH-PLAN-CODE              = DB-PLAN-CODE)
P26300*        INITIALIZE DB-START-DATE
P26300         MOVE BENSET4-PLAN-CODE      TO WS-DB-BEG-RNG
072300         MOVE WS-FALSE               TO BNBEN-BENEFS-SW
P26300*        PERFORM 850-FIND-NLT-BENSET4
P26300         PERFORM 850-FIND-BEGRNG-BENSET4
072500         PERFORM 2690-EDIT-SCR-DELETE
072600         THRU    2690-END
072700             UNTIL (BNBEN-BENEFS-OK)
072800             OR    (BENEFIT-NOTFOUND)
P26300*            OR    (BEN-COMPANY      NOT = DB-COMPANY)
P26300*            OR    (BEN-EMPLOYEE     NOT = DB-EMPLOYEE)
P26300*            OR    (BEN-PLAN-TYPE    NOT = DB-PLAN-TYPE)
P26300*            OR    (BEN-PLAN-CODE    NOT = DB-PLAN-CODE)
073300         IF (BNBEN-BENEFS-NOT-OK)
      ************ Cannot delete; company history records exist
073400             MOVE 198                    TO CRT-ERROR-NBR
073500             MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
073600             GO TO 2630-END.
073700
       2630-END.

153200******************************************************************
       2640-EDIT-CODA.
153200******************************************************************

207000     MOVE BNBEN-COMPANY              TO DB-COMPANY.
207100     MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
207200     MOVE BNBEN-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
207200     MOVE BNBEN-PLAN-CODE (I1)       TO DB-PLAN-CODE.
207300     INITIALIZE DB-PLAN-YEAR.
L10113     MOVE SPACES                     TO DB-PROCESS-LEVEL
L10113                                        DB-BUS-NBR-GRP
L10113                                        DB-QC-ENT-NBR-GRP.
207400     PERFORM 850-FIND-NLT-CODSET1.
069400
069500     IF  (CODA-FOUND)
069600     AND (COD-COMPANY                = DB-COMPANY)
069700     AND (COD-EMPLOYEE               = DB-EMPLOYEE)
069800     AND (COD-PLAN-TYPE              = DB-PLAN-TYPE)
069800     AND (COD-PLAN-CODE              = DB-PLAN-CODE)
P26300*        INITIALIZE DB-START-DATE
P26300         MOVE BENSET4-PLAN-CODE      TO WS-DB-BEG-RNG
066100         MOVE WS-FALSE               TO BNBEN-BENEFS-SW
P26300*        PERFORM 850-FIND-NLT-BENSET4
P26300         PERFORM 850-FIND-BEGRNG-BENSET4
070200         PERFORM 2690-EDIT-SCR-DELETE
070300         THRU    2690-END
070400             UNTIL (BNBEN-BENEFS-OK)
070500             OR    (BENEFIT-NOTFOUND)
P26300*            OR    (BEN-COMPANY      NOT = DB-COMPANY)
P26300*            OR    (BEN-EMPLOYEE     NOT = DB-EMPLOYEE)
P26300*            OR    (BEN-PLAN-TYPE    NOT = DB-PLAN-TYPE)
P26300*            OR    (BEN-PLAN-CODE    NOT = DB-PLAN-CODE)
071000         IF (BNBEN-BENEFS-NOT-OK)
      ************ Cannot delete; CODA exist
071100             MOVE 197                    TO CRT-ERROR-NBR
071200             MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
071300             GO TO 2640-END.
071400
       2640-END.

153200******************************************************************
       2680-EDIT-RESTRANS.
153200******************************************************************

141700     MOVE BNBEN-COMPANY              TO DB-COMPANY.
141800     MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
141900     MOVE BNBEN-PLAN-CODE  (I1)      TO DB-PLAN-CODE.
142000     MOVE BNBEN-START-DATE (I1)      TO DB-START-DATE.
142100     INITIALIZE DB-DATE
142200                DB-CHECK-NBR.
142300     PERFORM 850-FIND-NLT-RTRSET2.

062600     IF  (RESTRANS-FOUND)
062700     AND (RTR-COMPANY                = DB-COMPANY)
062800     AND (RTR-EMPLOYEE               = DB-EMPLOYEE)
062900     AND (RTR-PLAN-CODE              = DB-PLAN-CODE)
063000     AND (RTR-START-DATE             = DB-START-DATE)
      ******** Cannot delete; spending account transactions exist
063100         MOVE 139                    TO CRT-ERROR-NBR
063200         MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
063300         GO TO 2680-END.

       2680-END.

069300******************************************************************
069400 2690-EDIT-SCR-DELETE.
069500******************************************************************
069600
069700     MOVE WS-FALSE                   TO BNBEN-DEL-BEN-SW.
069800
069900     PERFORM
070100         VARYING I2 FROM 1 BY 1
P78537         UNTIL  (I2 > BNBEN-NBR-LINES)
070300         OR     (BNBEN-DELETING-BEN)
070400
071600         IF  (BNBEN-PLAN-TYPE (I2)   = BEN-PLAN-TYPE)
071600         AND (BNBEN-PLAN-CODE (I2)   = BEN-PLAN-CODE)
071700         AND (BNBEN-START-DATE (I2)  = BEN-START-DATE)
071800         AND (BNBEN-LINE-FC (I2)     = "D")
071900             MOVE WS-TRUE            TO BNBEN-DEL-BEN-SW
               END-IF
           END-PERFORM.

070500     IF (BNBEN-NOT-DELETING-BEN)
070600         MOVE WS-TRUE                TO BNBEN-BENEFS-SW
070700     ELSE
P26300*        PERFORM 860-FIND-NEXT-BENSET4.
P26300         PERFORM 860-FIND-NXTRNG-BENSET4.
070900
071000 2690-END.
071100
153200******************************************************************
043300 2700-EDIT-DEPENDENTS.
153200******************************************************************

           IF  (BNBEN-LINE-FC (I1)         = "C")
           AND (BNBEN-COVER-OPT (I1)       NOT = BEN-COV-OPTION)
               MOVE BNBEN-COVER-OPT (I1)   TO DB-COVERAGE-OPT
               PERFORM 840-FIND-COPSET1.

141700     MOVE BNBEN-COMPANY              TO DB-COMPANY.
           MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
           MOVE BNBEN-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
           MOVE BNBEN-PLAN-CODE  (I1)      TO DB-PLAN-CODE.
           MOVE BNBEN-START-DATE (I1)      TO DB-EMP-START.
           INITIALIZE DB-DEPENDENT
                      DB-START-DATE.
           PERFORM 850-FIND-NLT-HDBSET3.
           PERFORM 860-FIND-NEXT-HDBSET3
               UNTIL (HRDEPBEN-NOTFOUND)
               OR    (HDB-COMPANY          NOT = DB-COMPANY)
               OR    (HDB-EMPLOYEE         NOT = DB-EMPLOYEE)
               OR    (HDB-PLAN-TYPE        NOT = DB-PLAN-TYPE)
               OR    (HDB-PLAN-CODE        NOT = DB-PLAN-CODE)
               OR    (HDB-EMP-START        NOT = DB-EMP-START)
J10377         OR    ((HDB-PARTICIPNT = ZEROS) 
               AND    ((HDB-START-DATE      NOT < BNBEN-START-DATE (I1))
               AND  (((HDB-START-DATE      NOT > BNBEN-STOP-DATE (I1)) 
               AND    (BNBEN-STOP-DATE (I1) NOT = ZEROES)) 
               OR     (BNBEN-STOP-DATE (I1) = ZEROES)))).

           IF (BNBEN-LINE-FC (I1)          = "C")
               IF  ((HRDEPBEN-FOUND)
               AND  (HDB-COMPANY           = DB-COMPANY)
               AND  (HDB-EMPLOYEE          = DB-EMPLOYEE)
               AND  (HDB-PLAN-TYPE         = DB-PLAN-TYPE)
               AND  (HDB-PLAN-CODE         = DB-PLAN-CODE)
               AND  (HDB-EMP-START         = DB-EMP-START)
               AND  (HDB-START-DATE        >= BNBEN-START-DATE (I1))
               AND (((HDB-START-DATE       <= BNBEN-STOP-DATE  (I1))
               AND   (BNBEN-STOP-DATE (I1) NOT = ZEROES))
               OR    (BNBEN-STOP-DATE (I1) = ZEROES))
               AND ((BNCOVOPT-FOUND)
P83232         AND   (COP-COMPANY          = DB-COMPANY)
P83232         AND   (COP-PLAN-TYPE        = DB-PLAN-TYPE)
P83232         AND   (COP-PLAN-CODE        = DB-PLAN-CODE)
J07540* NOT (S)POUSE, (D)EPENDENTS, (B)OTH SPOUSE AND DEPS, (P)ARTNER, 
J07540*     SPOUSE (O)R PARTNER, PA(R)TNER DEPS, (C) PARTNER AND DEPS, 
J07540*     SPOUSE OR PARTNER (A)ND DEPS.
J07540         AND   (COP-COV-DEPENDENTS    NOT = "S" AND "D" AND "B" 
J07540                AND "P" AND "O" AND "R" AND "C" AND "A")))
      ************ Cannot select single coverage; dependents exist
063100             MOVE 202                      TO CRT-ERROR-NBR
063200             MOVE BNBEN-COVER-OPT-FN (I1)  TO CRT-FIELD-NBR
063300             GO TO 2700-END.

           IF (BNBEN-LINE-FC (I1)          = "D")
               IF   (HRDEPBEN-FOUND)
               AND  (HDB-COMPANY           = DB-COMPANY)
               AND  (HDB-EMPLOYEE          = DB-EMPLOYEE)
               AND  (HDB-PLAN-TYPE         = DB-PLAN-TYPE)
               AND  (HDB-PLAN-CODE         = DB-PLAN-CODE)
               AND  (HDB-EMP-START         = DB-EMP-START)
               AND  (HDB-START-DATE        >= BNBEN-START-DATE (I1))
               AND (((HDB-START-DATE       <= BNBEN-STOP-DATE  (I1))
               AND   (BNBEN-STOP-DATE (I1) NOT = ZEROES))
               OR    (BNBEN-STOP-DATE (I1) = ZEROES))
      ************ Cannot delete dependent benefits exist
063100             MOVE 201                      TO CRT-ERROR-NBR
063200             MOVE BNBEN-LINE-FC-FN (I1)    TO CRT-FIELD-NBR
063300             GO TO 2700-END.

071000 2700-END.
071100
153200******************************************************************
       2710-EDIT-LP-DELETE.
153200******************************************************************

           IF (BNBEN-LP-WARN-I1            NOT = ZEROES)
               GO TO 2710-END.

           MOVE "(BTE-UPDATED-HOURS > ?)"  TO FILTER-STRING.
           PERFORM 890-CREATE-FILTER.
           MOVE ZEROES                     TO NUMERIC-FILTER-VALUE.
           PERFORM 890-SET-NUMERIC-FILTER-VALUE.

           MOVE BNBEN-COMPANY              TO DB-COMPANY.
           MOVE BNBEN-PLAN-CODE (I1)       TO DB-PLAN-CODE.
           MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
           MOVE BNBEN-START-DATE  (I1)     TO DB-START-DATE.
           MOVE BTESET3-START-DATE         TO WS-DB-BEG-RNG.
           PERFORM 850-FILTER-BEGRNG-BTESET3.
           IF (BNTAEMPBAL-FOUND)
               MOVE I1                     TO BNBEN-LP-WARN-I1
063300         GO TO 2710-END.

       2710-END.

153200******************************************************************
043300 2800-EDIT-DTL-STOP.
153200******************************************************************

J27636* Enable error message 216 which was commented out in JT-664828
J64828      IF (BNBEN-STOP-DATE (I1)        = BEN-STOP-DATE)
J64828******** Must change stop date with stop function code
J64828          MOVE 216                        TO CRT-ERROR-NBR
J64828          MOVE BNBEN-STOP-DATE-FN (I1)    TO CRT-FIELD-NBR
J64828          GO TO 2800-END.

044200     IF  (BNBEN-START-DATE (I1)      > BNBEN-STOP-DATE (I1))
044300     AND (BNBEN-STOP-DATE (I1)       NOT = ZEROES)
      ******** Start date cannot be greater than stop date
044400         MOVE 111                        TO CRT-ERROR-NBR
044500         MOVE BNBEN-STOP-DATE-FN (I1)    TO CRT-FIELD-NBR
044600         GO TO 2800-END.
044700
           IF  (BEN-CURRENCY-CODE          NOT = EMP-CURRENCY-CODE)
           AND ((BNBEN-STOP-DATE (I1)      > BEN-STOP-DATE)
           OR   (BNBEN-STOP-DATE (I1)      = ZEROES))
      ******** Benefit currency does not match emp currency; cannot change
032600         MOVE 221                        TO CRT-ERROR-NBR
032700         MOVE BNBEN-STOP-DATE-FN (I1)    TO CRT-FIELD-NBR
032800         GO TO 2800-END.
044700
           IF (BNBEN-MULT-SALARY (I1)      NOT = BEN-MULTIPLE)
           OR (BNBEN-PCT-AMT-FLAG (I1)     NOT = BEN-PCT-AMT-FLAG)
           OR (BNBEN-START-DATE (I1)       NOT = BEN-START-DATE)
           OR (BNBEN-SMOKER-FLAG (I1)      NOT = BEN-SMOKER)
           OR (BNBEN-EMP-PRE-CONT (I1)     NOT = BEN-EMP-PRE-CONT
                                               + BEN-CMP-FLX-CONT)
           OR (BNBEN-EMP-AFT-CONT (I1)     NOT = BEN-EMP-AFT-CONT)
J00884         IF ((BNBEN-PCT-AMT-FLAG (I1)      NOT = BEN-PCT-AMT-FLAG)
J00884         AND (BEN-PCT-AMT-FLAG             NOT = "A" AND "P"))
J00884             CONTINUE
J00884         ELSE
      ******** Cannot change with Stop function code
                   MOVE 158                    TO CRT-ERROR-NBR
                   MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
                   GO TO 2800-END
J00884         END-IF    
J00884     END-IF.

           IF (PLN-PLAN-TYPE               = "VA")
               IF (BEN-NBR-HOURS           NOT = BNBEN-MULT-SALARY (I1))
                   MOVE 158                TO CRT-ERROR-NBR
               END-IF
           ELSE
           IF (PLN-CONTRIB-TYPE            = "5" OR "6" OR "7")
               IF (BEN-CYCLES-REMAIN       NOT = BNBEN-COVER-OPT (I1))
                   MOVE 158                TO CRT-ERROR-NBR
               END-IF
           ELSE
184000         IF (BEN-COV-OPTION          NOT = BNBEN-COVER-OPT (I1))
                   MOVE 158                TO CRT-ERROR-NBR.

           IF (PLN-CONTRIB-TYPE            = "5")
               IF (BEN-BOND-DED-AMT        NOT = BNBEN-COVER-AMT (I1))
                   MOVE 158                TO CRT-ERROR-NBR
               END-IF
           ELSE
           IF (PLN-CONTRIB-TYPE            = "6" OR "7")
      *    IF (PLN-CONTRIB-TYPE            = "5" OR "6" OR "7")
               IF (BEN-BOND-DED-AMT        NOT = BNBEN-COVER-AMT (I1))
               OR (BEN-ANNUAL-AMT          NOT = BNBEN-PAY-RATE (I1))
                   MOVE 158                TO CRT-ERROR-NBR
               END-IF
           ELSE
               IF (BEN-COVER-AMT           NOT = BNBEN-COVER-AMT (I1))
               OR (BEN-PAY-RATE            NOT = BNBEN-PAY-RATE (I1))
                   MOVE 158                TO CRT-ERROR-NBR.

           IF (ERROR-FOUND)
               MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
               GO TO 2800-END.

ROBX       PERFORM 2805-EDIT-STOP-DATE-OTD
           THRU    2805-END.
           IF (ERROR-FOUND)
               GO TO 2800-END.

           MOVE WS-FALSE                   TO BNCTWS-PRE-SIGN-SW.
           COMPUTE BNBEN-EMP-CONT          = BEN-CMP-FLX-CONT
                                           + BEN-EMP-PRE-CONT
                                           + BEN-EMP-AFT-CONT.
           MOVE BEN-COMP-CONT              TO BNBEN-CMP-CONT.
           IF  (BNBEN-CMP-CONT             = ZEROES)
           AND (BNBEN-EMP-CONT             = ZEROES)
               SET ZERO-PREMIUM            TO TRUE
           ELSE
           IF  (BNBEN-EMP-CONT             < ZEROES)
           AND (BNBEN-CMP-CONT             NOT < ZEROES)
               SET NEGATIVE-PREMIUM        TO TRUE
           ELSE
               SET POSITIVE-PREMIUM        TO TRUE.

           IF (ZERO-PREMIUM)
               MOVE WS-TRUE            TO BNBEN-ZERO-PREMIUM-SW (I1)
               MOVE WS-FALSE           TO BNBEN-NEG-PREMIUM-SW (I1)
               MOVE WS-FALSE           TO BNBEN-POS-PREMIUM-SW (I1)
           ELSE
           IF (NEGATIVE-PREMIUM)
               MOVE WS-TRUE            TO BNBEN-NEG-PREMIUM-SW (I1)
               MOVE WS-FALSE           TO BNBEN-ZERO-PREMIUM-SW (I1)
               MOVE WS-FALSE           TO BNBEN-POS-PREMIUM-SW (I1)
           ELSE
               MOVE WS-TRUE            TO BNBEN-POS-PREMIUM-SW (I1)
               MOVE WS-FALSE           TO BNBEN-NEG-PREMIUM-SW (I1)
               MOVE WS-FALSE           TO BNBEN-ZERO-PREMIUM-SW (I1).

           IF (BNBEN-STOP-DATE (I1)        NOT = BEN-STOP-DATE)
               IF (BNBEN-FLEX-FLAG (I1)    = "Y")
                   PERFORM 2810-EDIT-FLEX-1
                   THRU    2810-END
                   IF (ERROR-FOUND)
                       GO TO 2800-END
                   END-IF

                   PERFORM 2820-EDIT-EMPFLEXREM
                   THRU    2820-END
                   IF (ERROR-FOUND)
                       GO TO 2800-END.

053100     IF  (BNBEN-STOP-DATE (I1)       > BEN-STOP-DATE) 
053300     OR ((BNBEN-STOP-DATE (I1)       NOT = BEN-STOP-DATE)
053400     AND (BNBEN-STOP-DATE (I1)       = ZEROES))
P26300         MOVE BENSET4-PLAN-CODE      TO WS-DB-BEG-RNG
P64135         PERFORM 850-FIND-NLT-BENSET4
P64135*         PERFORM 850-FIND-BEGRNG-BENSET4
053600         IF  (BENEFIT-FOUND)
P64135         AND (BEN-COMPANY            = DB-COMPANY)
P64135         AND (BEN-EMPLOYEE           = DB-EMPLOYEE)
P64135         AND (BEN-PLAN-TYPE          = DB-PLAN-TYPE)
P64135         AND (BEN-PLAN-CODE          = DB-PLAN-CODE)
054100             MOVE WS-FALSE           TO BNBEN-STOP-DFLT-SW
054200             PERFORM 2450-EDIT-EXISTING-POST-BEN
054300             THRU    2450-END
054400                 UNTIL  (ERROR-FOUND)
054500                 OR     (BNBEN-STOP-DEFAULTED)
054600                 OR     (BENEFIT-NOTFOUND)
P64135                 OR     (BEN-COMPANY          NOT = DB-COMPANY)
P64135                 OR     (BEN-EMPLOYEE         NOT = DB-EMPLOYEE)
P64135                 OR     (BEN-PLAN-TYPE        NOT = DB-PLAN-TYPE)
P64135                 OR     (BEN-PLAN-CODE        NOT = DB-PLAN-CODE)
055100                 OR    ((BNBEN-STOP-DATE (I1) NOT = ZEROES)
055200                 AND    (BNBEN-STOP-DATE (I1) < BEN-START-DATE))
055500             IF (ERROR-FOUND)
055600                 GO TO 2800-END.
059500*
059600**** NOTE: THE PROGRAM MAY NO LONGER BE ON THE BENEFIT RECORD FROM
059700**** THE SCREEN AT THIS POINT, BECAUSE 2440 AND 2450 MAY HAVE YOU
059800**** POINTING AT ANOTHER BENEFIT RECORD.
059900*
           IF (DB-START-DATE               NOT = BNBEN-START-DATE (I1))
               MOVE BNBEN-START-DATE (I1)  TO DB-START-DATE.
           PERFORM 840-FIND-BENSET1.

           PERFORM 2850-EDIT-HIPAA
           THRU    2850-END.

           PERFORM 2500-EDIT-DED-OVERRIDE
           THRU    2500-END.
           IF (ERROR-FOUND)
               GO TO 2800-END.

072100 2800-END.

      ******************************************************************
       2805-EDIT-STOP-DATE-OTD.
      ******************************************************************

           IF (BEN-FLEX-DED-SEQ            NOT = ZEROES)
               MOVE PLN-FLEX-DED-CODE      TO DB-DED-CODE
               MOVE BEN-FLEX-DED-SEQ       TO DB-SEQ-NBR
               PERFORM 840-FIND-EDMSET1
               PERFORM 2604-EDIT-ONETMDED
               THRU    2604-END
               IF (ERROR-FOUND)
                   GO TO 2805-END.

           IF (BEN-PRE-SEQ-NBR             NOT = ZEROES)
               IF (BEN-PCT-AMT-FLAG        = "P")
                   MOVE PLN-PRE-DED-CODE-P TO DB-DED-CODE
               ELSE
                   MOVE PLN-PRE-DED-CODE-A TO DB-DED-CODE
               END-IF
               MOVE BEN-PRE-SEQ-NBR        TO DB-SEQ-NBR
               PERFORM 840-FIND-EDMSET1
               PERFORM 2604-EDIT-ONETMDED
               THRU    2604-END
               IF (ERROR-FOUND)
                   GO TO 2805-END.

           IF (BEN-AFT-SEQ-NBR             NOT = ZEROES)
               IF (BEN-PCT-AMT-FLAG        = "P")
                   MOVE PLN-AFT-DED-CODE-P TO DB-DED-CODE
               ELSE
                   MOVE PLN-AFT-DED-CODE-A TO DB-DED-CODE
               END-IF
               MOVE BEN-AFT-SEQ-NBR        TO DB-SEQ-NBR
               PERFORM 840-FIND-EDMSET1
               PERFORM 2604-EDIT-ONETMDED
               THRU    2604-END
               IF (ERROR-FOUND)
                   GO TO 2805-END.

           IF (BEN-CMP-SEQ-NBR             NOT = ZEROES)
               IF (BEN-PCT-AMT-FLAG        = "P")
                   MOVE PLN-CMP-DED-CODE-P TO DB-DED-CODE
               ELSE
                   MOVE PLN-CMP-DED-CODE-A TO DB-DED-CODE
               END-IF
               MOVE BEN-CMP-SEQ-NBR        TO DB-SEQ-NBR
               PERFORM 840-FIND-EDMSET1
               IF (EMDEDMASTR-NOTFOUND)
                   IF (BEN-PCT-AMT-FLAG    = "P")
                       MOVE PLN-PRE-DED-MTCH-P TO DB-DED-CODE
                   ELSE
                       MOVE PLN-PRE-DED-MTCH-A TO DB-DED-CODE
                   END-IF
                   PERFORM 840-FIND-EDMSET1
               END-IF
               PERFORM 2604-EDIT-ONETMDED
               THRU    2604-END
               IF (ERROR-FOUND)
                   GO TO 2805-END.

           IF (BEN-CMP-AFT-SEQ             NOT = ZEROES)
               IF (BEN-PCT-AMT-FLAG        = "P")
                   MOVE PLN-AFT-DED-MTCH-P TO DB-DED-CODE
               ELSE
                   MOVE PLN-AFT-DED-MTCH-A TO DB-DED-CODE
               END-IF
               MOVE BEN-CMP-AFT-SEQ        TO DB-SEQ-NBR
               PERFORM 840-FIND-EDMSET1
               PERFORM 2604-EDIT-ONETMDED
               THRU    2604-END
               IF (ERROR-FOUND)
                   GO TO 2805-END.

       2805-END.

080600******************************************************************
080700 2810-EDIT-FLEX-1.
080800******************************************************************
080900
083100     IF (BNBEN-START-DATE (I1)       > BNBEN-EFD-STOP-DT (I1))
083200     OR (BNBEN-START-DATE (I1)       < BNBEN-EFD-START-DT (I1))
083300         IF (BNBEN-LINE-FC (I1)      = "A")
      ************ Start date must be within employee flex dollar dates
083400             MOVE 112                        TO CRT-ERROR-NBR
083500             MOVE BNBEN-START-DATE-FN (I1)   TO CRT-FIELD-NBR
083600             GO TO 2810-END
083700         ELSE
      ************ Cannot update; benefit not in enrollment flex year
083800             MOVE 121                        TO CRT-ERROR-NBR
083900             MOVE BNBEN-LINE-FC-FN (I1)      TO CRT-FIELD-NBR
084000             GO TO 2810-END.

084200     IF (BNBEN-STOP-DATE (I1)        = ZEROES)
084300         MOVE BNBEN-EFD-STOP-DT (I1) TO BNBEN-STOP-DATE (I1)
084400     ELSE
084500     IF (BNBEN-STOP-DATE (I1)        > BNBEN-EFD-STOP-DT (I1))
      ******** Stop date cannot be > flex dollar stop date
084600         MOVE 107                        TO CRT-ERROR-NBR
084700         MOVE BNBEN-STOP-DATE-FN (I1)    TO CRT-FIELD-NBR
084800         GO TO 2810-END.
084900
           PERFORM
               VARYING I7 FROM 1 BY 1
               UNTIL  (I7                  > BNFRWS-MAX-PERIODS)
               OR     (BNFRWS-START-DATE (I6 I7) = ZEROES)
               OR    ((BNBEN-START-DATE (I1)
                                           >= BNFRWS-START-DATE (I6 I7))
               AND    (BNBEN-STOP-DATE (I1)
                                           <= BNFRWS-STOP-DATE (I6 I7)))
               OR    ((BNBEN-START-DATE (I1)
                                           >= BNFRWS-START-DATE (I6 I7))
               AND    (BNBEN-START-DATE (I1)
                                           <= BNFRWS-STOP-DATE (I6 I7))
               AND    (BNBEN-STOP-DATE (I1)
                                           >= BNFRWS-STOP-DATE (I6 I7)))

               CONTINUE
           END-PERFORM.

083100     IF (BNBEN-START-DATE (I1)       > BNFRWS-STOP-DATE (I6 I7))
083200     OR (BNBEN-START-DATE (I1)       < BNFRWS-START-DATE (I6 I7))
083300         IF (BNBEN-LINE-FC (I1)      = "A")
                   MOVE BNFRWS-START-DATE (I6 I7)  TO BNWS-DATE-FORMAT1
                                                      HRWS-DATE-FIELD
                   INITIALIZE                         HRWS-DATE-8-FIELD
                   PERFORM 781-HR-FORMAT-DATE-FIELD
                   MOVE HRWS-VALUE                 TO CRT-ERR-VAR1
                   MOVE BNFRWS-STOP-DATE (I6 I7)   TO BNWS-DATE-FORMAT1
                                                      HRWS-DATE-FIELD
                   PERFORM 781-HR-FORMAT-DATE-FIELD
                   MOVE HRWS-VALUE                 TO CRT-ERR-VAR2
      ************ Start date must be within flex period
083400             MOVE 162                        TO CRT-ERROR-NBR
083500             MOVE BNBEN-START-DATE-FN (I1)   TO CRT-FIELD-NBR
083600             GO TO 2810-END
083700         ELSE
      ************ Cannot update; benefit not within flex period
083800             MOVE 171                        TO CRT-ERROR-NBR
083900             MOVE BNBEN-LINE-FC-FN (I1)      TO CRT-FIELD-NBR
084000             GO TO 2810-END.
084100            
086900 2810-END.

      ******************************************************************
       2820-EDIT-EMPFLEXREM.
      ******************************************************************

084500     IF (BNBEN-STOP-DATE (I1)        > BNFRWS-STOP-DATE (I6 I7))
               IF  (NEGATIVE-PREMIUM)
               AND (PLN-FLEX-PAY-CODE      NOT = SPACES)
                   NEXT SENTENCE
               ELSE
                   MOVE BNFRWS-STOP-DATE (I6 I7)   TO BNWS-DATE-FORMAT1
                                                      HRWS-DATE-FIELD
                   INITIALIZE                         HRWS-DATE-8-FIELD
                   PERFORM 781-HR-FORMAT-DATE-FIELD
                   MOVE HRWS-VALUE                 TO CRT-ERR-VAR1
      ************ Stop date cannot be > flex period end date
084600             MOVE 163                        TO CRT-ERROR-NBR
084700             MOVE BNBEN-STOP-DATE-FN (I1)    TO CRT-FIELD-NBR
084800             GO TO 2820-END.
086800
           IF  (BNBEN-LINE-FC (I1)         = "C")
           AND (PLN-FLEX-PAY-CODE          NOT = SPACES)
               IF  (BEN-CMP-FLX-CONT       >= ZEROES)
               AND (BNBEN-EMP-PRE-CONT(I1) < ZEROES)
      ************ Change from positive or zero to negative is not allowed
084600             MOVE 147                        TO CRT-ERROR-NBR
104900             MOVE BNBEN-EMP-PRE-CONT-FN (I1) TO CRT-FIELD-NBR
084800             GO TO 2820-END
               END-IF
               IF  (BEN-CMP-FLX-CONT       < ZEROES)
               AND (BNBEN-EMP-PRE-CONT(I1) >= ZEROES)
      ************ Change from negative to positive or zero is not allowed
084600             MOVE 151                        TO CRT-ERROR-NBR
104900             MOVE BNBEN-EMP-PRE-CONT-FN (I1) TO CRT-FIELD-NBR
084800             GO TO 2820-END.

      *
      **** CREATE A NEW TABLE OCCURENCE IF A NEW FLEX PERIOD IS BEING 
      **** CREATED.  THIS ROUTINE WILL ALSO RESET I7 IF NECESSARY.
      *
           IF (BNBEN-START-DATE (I1)   NOT = BNFRWS-START-DATE (I6 I7))
           OR (BNBEN-STOP-DATE (I1)    NOT = BNFRWS-STOP-DATE  (I6 I7))
               PERFORM 2822-UPDATE-EFR-TABLE
               THRU    2822-END.

102100     IF (BNBEN-LINE-FC (I1)          = "C")
102100     OR (BNBEN-LINE-FC (I1)          = "S")
               IF  ((BEN-CMP-FLX-CONT      < ZEROES)
               AND  (PLN-FLEX-PAY-CODE     NOT = SPACES))
               OR  ((BNBEN-LINE-FC (I1)    = "S")
               AND  (BNBEN-START-DATE (I1) = BNFRWS-START-DATE (I6 I7))
               AND  (BNBEN-STOP-DATE (I1)  = BNFRWS-STOP-DATE  (I6 I7)))
                   NEXT SENTENCE
               ELSE
102200             COMPUTE BNFRWS-CREDITS-AVAIL (I6 I7) 
                     = BNFRWS-CREDITS-AVAIL (I6 I7) + BEN-CMP-FLX-CONT
102400             COMPUTE BNFRWS-PRE-TAX-AVAIL (I6 I7)
                     = BNFRWS-PRE-TAX-AVAIL (I6 I7) + BEN-EMP-PRE-CONT
102600             COMPUTE BNFRWS-TOTAL-AVAIL
                     = BNFRWS-CREDITS-AVAIL (I6 I7)
                     + BNFRWS-PRE-TAX-AVAIL (I6 I7).
102800
           IF  (BNBEN-LINE-FC (I1)         = "S")
           AND (BNBEN-START-DATE (I1)      = BNFRWS-START-DATE (I6 I7))
           AND (BNBEN-STOP-DATE (I1)       = BNFRWS-STOP-DATE  (I6 I7))
               NEXT SENTENCE
           ELSE
           IF (POSITIVE-PREMIUM)
               IF (PLN-FLEX-DED-CODE   NOT = SPACES)
102900             IF (BNBEN-EMP-PRE-CONT (I1)  
                                       <= BNFRWS-CREDITS-AVAIL (I6 I7))
103000                 MOVE BNBEN-EMP-PRE-CONT (I1)
103100                                 TO BNBEN-CMP-FLX-CONT (I1)
103300                 COMPUTE BNFRWS-CREDITS-AVAIL (I6 I7)
                                       = BNFRWS-CREDITS-AVAIL (I6 I7)
                                       - BNBEN-EMP-PRE-CONT (I1)
103500                 COMPUTE BNFRWS-TOTAL-AVAIL 
                                       = BNFRWS-CREDITS-AVAIL (I6 I7)
                                       + BNFRWS-PRE-TAX-AVAIL (I6 I7)
103700             ELSE
103800                 MOVE BNFRWS-CREDITS-AVAIL (I6 I7)
                                       TO BNBEN-CMP-FLX-CONT (I1)
104200                 COMPUTE BNFRWS-PRE-TAX-AVAIL (I6 I7)
                                       = BNFRWS-PRE-TAX-AVAIL (I6 I7)
104300                                 - (BNBEN-EMP-PRE-CONT (I1)
                                       -  BNFRWS-CREDITS-AVAIL (I6 I7))
104100                 INITIALIZE BNFRWS-CREDITS-AVAIL (I6 I7) 
104400                 COMPUTE BNFRWS-TOTAL-AVAIL
                                       = BNFRWS-CREDITS-AVAIL (I6 I7)
104500                                 + BNFRWS-PRE-TAX-AVAIL (I6 I7)
                       IF  ((PLN-COUNTRY-CODE   = HRWS-CA-WORK-COUNTRY)
                       AND  (PLN-PLAN-TYPE      = "DB" OR "DC" OR "VA")
                       AND  (BNFRWS-PRE-TAX-AVAIL (I6 I7)
                                                > ZEROES)
105900                 AND  (PLN-PRE-DED-CODE-A = SPACES))
                       OR  ((PLN-COUNTRY-CODE   NOT =
                                                HRWS-CA-WORK-COUNTRY)
105900                 AND  (PLN-PRE-DED-CODE-A = SPACES))
      ******************** Spending employee pre-tax dollars;assign pre-tax ded
106000                     MOVE 146                    TO CRT-ERROR-NBR
106100                     MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
106200                     GO TO 2820-END
                       END-IF
                   END-IF
               ELSE
                   IF (BNBEN-EMP-PRE-CONT (I1)  
                                       <= BNFRWS-PRE-TAX-AVAIL (I6 I7))
                       INITIALIZE BNBEN-CMP-FLX-CONT (I1)
                       COMPUTE BNFRWS-PRE-TAX-AVAIL (I6 I7)
                                       = BNFRWS-PRE-TAX-AVAIL (I6 I7)
                                       - BNBEN-EMP-PRE-CONT (I1)
                       MOVE BNFRWS-PRE-TAX-AVAIL (I6 I7)
                                       TO BNFRWS-TOTAL-AVAIL
                   ELSE
                       INITIALIZE BNBEN-CMP-FLX-CONT (I1)
                       COMPUTE BNFRWS-TOTAL-AVAIL  
                                       = BNFRWS-PRE-TAX-AVAIL (I6 I7)
                                       - BNBEN-EMP-PRE-CONT (I1).
104600
           IF  (BNBEN-LINE-FC (I1)         = "S")
           AND (BNBEN-START-DATE (I1)      = BNFRWS-START-DATE (I6 I7))
           AND (BNBEN-STOP-DATE (I1)       = BNFRWS-STOP-DATE  (I6 I7))
               NEXT SENTENCE
           ELSE
           IF  (BNBEN-LINE-FC (I1)         = "C")
           AND (NEGATIVE-PREMIUM)
           AND (PLN-FLEX-PAY-CODE          = SPACES)
103300         COMPUTE BNFRWS-CREDITS-AVAIL (I6 I7)
                                       = BNFRWS-CREDITS-AVAIL (I6 I7)
                                       + (BNBEN-EMP-PRE-CONT (I1)
                                       *  NEGATIVE-ONE).

104700     IF  (BNFRWS-TOTAL-AVAIL         < ZEROES)
           AND (POSITIVE-PREMIUM)
               MOVE BNFRWS-TOTAL-AVAIL         TO HRWS-DEC-FIELD
               MOVE 0                          TO HRWS-SIZE
               MOVE 2                          TO HRWS-NBR-DECIMALS
               PERFORM 770-HR-FORMAT-DEC-FIELD
               MOVE HRWS-VALUE                 TO CRT-ERR-VAR1
      ******** Amount exceeds available dollars
104800         MOVE 113                        TO CRT-ERROR-NBR
104900         MOVE BNBEN-EMP-PRE-CONT-FN (I1) TO CRT-FIELD-NBR
105000         GO TO 2820-END.
105100
           IF (((BNBEN-START-DATE (I1)  NOT = BNFRWS-START-DATE (I6 I7))
           OR  (BNBEN-STOP-DATE (I1)   NOT = BNFRWS-STOP-DATE  (I6 I7)))
           AND (BNFRWS-CREDITS-AVAIL (I6 I7)  < ZEROES)
           AND (PLN-FLEX-PAY-CODE             = SPACES))
104700     OR  ((BNFRWS-CREDITS-AVAIL (I6 I7) < ZEROES)
           AND  ((NEGATIVE-PREMIUM)
           OR    (ZERO-PREMIUM)))
      ********* Cannot change dollars have been spend
                MOVE 180                   TO CRT-ERROR-NBR
                MOVE BNBEN-LINE-FC-FN (I1) TO CRT-FIELD-NBR
                GO TO 2820-END.

           IF  (BNBEN-LINE-FC (I1)         = "C" OR "S")
           AND (BNBEN-START-DATE (I1)      = BNFRWS-START-DATE (I6 I7))
           AND (BNBEN-STOP-DATE  (I1)      = BNFRWS-STOP-DATE  (I6 I7))
           AND (NEGATIVE-PREMIUM)
           AND (PLN-FLEX-PAY-CODE          = SPACES)
               MOVE I7                     TO I9
               ADD  1                      TO I9
               PERFORM 
                   VARYING I8 FROM I9 BY 1
                   UNTIL   (BNFRWS-START-DATE (I6 I8)    = ZEROES)
                   OR      (BNFRWS-CREDITS-AVAIL (I6 I8) < ZEROES)

                   IF (BNFRWS-STOP-DATE (I6 I8) <= BEN-STOP-DATE)
102200                 COMPUTE BNFRWS-CREDITS-AVAIL (I6 I8) 
                      = BNFRWS-CREDITS-AVAIL (I6 I8) + BEN-CMP-FLX-CONT
                   END-IF
                   IF (BNFRWS-CREDITS-AVAIL (I6 I8) < ZEROES)
      **************** Cannot change dollars have been spend
                       MOVE 180                    TO CRT-ERROR-NBR
                       MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
                   END-IF
               END-PERFORM
               IF (ERROR-FOUND)
                   GO TO 2820-END.
       
106400 2820-END.

087100******************************************************************
087200 2822-UPDATE-EFR-TABLE.
087300******************************************************************

           MOVE WS-FALSE                   TO BNWS-STOP-STM-SW
                                              BNWS-MID-PERIOD-SW.

           IF  (NEGATIVE-PREMIUM)
           AND (PLN-FLEX-PAY-CODE          NOT = SPACES)
               GO TO 2822-END.

           IF (BNBEN-LINE-FC (I1)          = "A")
               IF  (BNBEN-START-DATE (I1)  > BNFRWS-START-DATE (I6 I7))
               AND (BNBEN-STOP-DATE (I1)   < BNFRWS-STOP-DATE (I6 I7))
                   MOVE BNFRWS-STOP-DATE  (I6 I7)
                                           TO BNWS-SAVE-STOP-DATE
                   MOVE BNBEN-START-DATE (I1)
                                           TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   SUBTRACT 1              FROM WSDR-JULIAN-DAYS
                   PERFORM 900-JULIAN-TO-DATE   
                   MOVE WSDR-FR-DATE       TO BNFRWS-STOP-DATE (I6 I7)
                   MOVE I7                 TO I8
                   PERFORM 
                       VARYING I7 FROM 1 BY 1
                       UNTIL  (BNFRWS-START-DATE (I6 I7) = ZEROES)
    
                       CONTINUE
                   END-PERFORM
                   MOVE BNBEN-STOP-DATE (I1) TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   ADD 1                   TO WSDR-JULIAN-DAYS
                   PERFORM 900-JULIAN-TO-DATE   
                   MOVE WSDR-FR-DATE       TO BNFRWS-START-DATE (I6 I7)
                   MOVE BNWS-SAVE-STOP-DATE
                                           TO BNFRWS-STOP-DATE (I6 I7)
                   MOVE BNFRWS-CREDITS-AVAIL (I6 I8)
                                           TO
                                           BNFRWS-CREDITS-AVAIL (I6 I7)
                   MOVE BNFRWS-PRE-TAX-AVAIL (I6 I8)
                                           TO
                                           BNFRWS-PRE-TAX-AVAIL (I6 I7)
                   ADD 1                   TO I7
                   MOVE BNBEN-START-DATE (I1)
                                           TO BNFRWS-START-DATE (I6 I7)
                   MOVE BNBEN-STOP-DATE  (I1) 
                                           TO BNFRWS-STOP-DATE (I6 I7)
                   MOVE BNFRWS-CREDITS-AVAIL (I6 I8)
                                           TO
                                           BNFRWS-CREDITS-AVAIL (I6 I7)
                   MOVE BNFRWS-PRE-TAX-AVAIL (I6 I8)
                                           TO
                                           BNFRWS-PRE-TAX-AVAIL (I6 I7)
               ELSE  
               IF (BNBEN-START-DATE (I1)   > BNFRWS-START-DATE (I6 I7))
                   MOVE BNFRWS-STOP-DATE (I6 I7)
                                              TO BNWS-SAVE-STOP-DATE
                   MOVE BNBEN-START-DATE (I1) TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   SUBTRACT 1              FROM WSDR-JULIAN-DAYS
                   PERFORM 900-JULIAN-TO-DATE    
                   MOVE WSDR-FR-DATE       TO BNFRWS-STOP-DATE (I6 I7)
                   MOVE I7                 TO I8
                   PERFORM 
                       VARYING I7 FROM 1 BY 1
                       UNTIL  (BNFRWS-START-DATE (I6 I7) = ZEROES)
    
                       CONTINUE
                   END-PERFORM
                   MOVE BNBEN-START-DATE (I1)
                                           TO BNFRWS-START-DATE (I6 I7)
                   MOVE BNWS-SAVE-STOP-DATE 
                                           TO BNFRWS-STOP-DATE (I6 I7)
                   MOVE BNFRWS-CREDITS-AVAIL (I6 I8)
                                           TO
                                           BNFRWS-CREDITS-AVAIL (I6 I7)
                   MOVE BNFRWS-PRE-TAX-AVAIL (I6 I8)
                                           TO
                                           BNFRWS-PRE-TAX-AVAIL (I6 I7)
               END-IF
               IF (BNBEN-STOP-DATE (I1)      < BNFRWS-STOP-DATE (I6 I7))
                   MOVE BNFRWS-STOP-DATE (I6 I7)
                                             TO BNWS-SAVE-STOP-DATE
                   MOVE BNBEN-STOP-DATE (I1) TO BNFRWS-STOP-DATE (I6 I7)
                   MOVE I7                   TO I8
                   PERFORM 
                       VARYING I7 FROM 1 BY 1
                       UNTIL  (BNFRWS-START-DATE (I6 I7) = ZEROES)
    
                       CONTINUE
                   END-PERFORM
                   MOVE BNBEN-STOP-DATE (I1) TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   ADD 1                   TO WSDR-JULIAN-DAYS
                   PERFORM 900-JULIAN-TO-DATE    
                   MOVE WSDR-FR-DATE       TO BNFRWS-START-DATE (I6 I7)
                   MOVE BNWS-SAVE-STOP-DATE
                                           TO BNFRWS-STOP-DATE (I6 I7)
                   MOVE BNFRWS-CREDITS-AVAIL (I6 I8)
                                           TO
                                           BNFRWS-CREDITS-AVAIL (I6 I7)
                   MOVE BNFRWS-PRE-TAX-AVAIL (I6 I8)
                                           TO
                                           BNFRWS-PRE-TAX-AVAIL (I6 I7).

           IF (BNBEN-LINE-FC (I1)          = "S")
               IF  (BEN-STOP-DATE          NOT = BNBEN-STOP-DATE (I1))
               AND (BNBEN-STOP-DATE (I1)   NOT =
                                           BNFRWS-STOP-DATE (I6 I7))
                   MOVE BNFRWS-STOP-DATE (I6 I7)
                                             TO BNWS-SAVE-STOP-DATE
                   MOVE BNBEN-STOP-DATE (I1) TO BNFRWS-STOP-DATE (I6 I7)
                   MOVE I7                   TO I8
                   PERFORM 
                       VARYING I7 FROM 1 BY 1
                       UNTIL  (BNFRWS-START-DATE (I6 I7) = ZEROES)
    
                       CONTINUE
                   END-PERFORM
                   MOVE BNBEN-STOP-DATE (I1) TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   ADD 1                   TO WSDR-JULIAN-DAYS
                   PERFORM 900-JULIAN-TO-DATE   
                   MOVE WSDR-FR-DATE       TO BNFRWS-START-DATE (I6 I7)
                   MOVE BNWS-SAVE-STOP-DATE
                                           TO BNFRWS-STOP-DATE (I6 I7)
                   MOVE BNFRWS-CREDITS-AVAIL (I6 I8)
                                           TO
                                           BNFRWS-CREDITS-AVAIL (I6 I7)
                   MOVE BNFRWS-PRE-TAX-AVAIL (I6 I8)
                                           TO
                                           BNFRWS-PRE-TAX-AVAIL (I6 I7).

       2822-END.

067500******************************************************************
       2850-EDIT-HIPAA.
067500******************************************************************

           IF  (BNBEN-CREATE-TRANS (I1)    = SPACES)
           OR  ((BNBEN-CREATE-TRANS (I1)   = "Y" OR "P")
           AND  (BNBEN-HIPAA-DEFAULT))
               MOVE PLN-CREATE-TRANS       TO BNBEN-CREATE-TRANS (I1).

           IF  (BNBEN-CREATE-TRANS (I1)    = "Y" OR "P")
           AND (BNBEN-HIPAA-DEFAULT)
               MOVE BNBEN-HIPAA-REASON     TO BNBEN-REASON (I1).

           IF  (PLN-CREATE-TRANS           = SPACES OR "N")
           AND (BNBEN-CREATE-TRANS (I1)    = "Y" OR "P")
      ******** Plan not defined for HIPAA transactions
               MOVE 230                                 TO CRT-ERROR-NBR
               MOVE BNBEN-CREATE-TRANS-FN (I1)          TO CRT-FIELD-NBR
               GO TO 2850-END.

           IF (BNBEN-CREATE-TRANS (I1)     = "N")
               IF (BNBEN-REASON (I1)       NOT = SPACES)
      ************ Do not enter reason if create transaction = "N"
                   MOVE 224                             TO CRT-ERROR-NBR
                   MOVE BNBEN-REASON-FN (I1)            TO CRT-FIELD-NBR
                   GO TO 2850-END
               END-IF
               IF (BNBEN-MEMBER-ID (I1)    NOT = ZEROES)
      ************ Do not specify member id if create transaction = "N"
                   MOVE 225                             TO CRT-ERROR-NBR
                   MOVE BNBEN-MEMBER-ID-FN (I1)         TO CRT-FIELD-NBR
                   GO TO 2850-END.

           IF (BNBEN-CREATE-TRANS (I1)     = "Y" OR "P")
               IF (BNBEN-REASON (I1)       = SPACES)
      ************ Must enter reason if create transaction = "Y"
                   MOVE 226                             TO CRT-ERROR-NBR
                   MOVE BNBEN-REASON-FN (I1)            TO CRT-FIELD-NBR
                   GO TO 2850-END
               END-IF
               IF (BNBEN-MEMBER-ID (I1)    = ZEROES)
                   MOVE PLN-MEMBER-ID      TO BNBEN-MEMBER-ID (I1).

           IF (BNBEN-REASON (I1)           NOT = SPACES)
               MOVE BNBEN-BT               TO DB-TYPE
               MOVE BNBEN-REASON (I1)      TO DB-CODE
               PERFORM 840-FIND-PCOSET1
               IF (PCODES-NOTFOUND)
      ************ Reason code does not exist
                   MOVE 228                             TO CRT-ERROR-NBR
                   MOVE BNBEN-REASON-FN (I1)            TO CRT-FIELD-NBR
                   GO TO 2850-END.

           IF  (BNBEN-MEMBER-ID (I1)       NOT = ZEROES)
           AND (BNBEN-MEMBER-ID (I1)       NOT = 1 AND 2)
      ******** Invalid member id
               MOVE 229                                 TO CRT-ERROR-NBR
               MOVE BNBEN-MEMBER-ID-FN (I1)             TO CRT-FIELD-NBR
               GO TO 2850-END.

       2850-END.

000100******************************************************************
       2900-EDIT-BNACCOUNTS.    
000100******************************************************************

           IF (DEDCODE-NOTFOUND)
               MOVE BNA-COMPANY        TO DB-COMPANY
               MOVE BNA-DED-CODE       TO DB-DED-CODE
               PERFORM 840-FIND-DDCSET1.

           IF (BNA-EXP-ACCT-UNIT NOT = SPACES)
018000     OR (BNA-EXP-ACCOUNT   NOT = ZEROS)
           OR (BNA-EXP-SUB-ACCT  NOT = ZEROS)
               IF (BNA-EXP-DIST-CO       NOT = ZEROES)
018100             MOVE BNA-EXP-DIST-CO  TO IFACWS-COMPANY
               ELSE
                   IF (DDC-EXP-DIST-CO NOT = ZEROES)
                       MOVE DDC-EXP-DIST-CO TO IFACWS-COMPANY
                   ELSE
                       MOVE EMP-HM-DIST-CO  TO IFACWS-COMPANY
                   END-IF
               END-IF
018200         MOVE BNA-EXP-ACCT-UNIT    TO IFACWS-ACCT-UNIT
018300         MOVE BNA-EXP-ACCOUNT      TO IFACWS-ACCOUNT
018400         MOVE BNA-EXP-SUB-ACCT     TO IFACWS-SUB-ACCOUNT
               MOVE 1                    TO IFACWS-EDIT-TYPE
018500         PERFORM 635-EDIT-GLMASTER-60
018600         IF (ERROR-FOUND)
041900             MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
018900             GO TO 2900-END.
      
           IF (BNA-ACR-ACCT-UNIT NOT = SPACES)
018000     OR (BNA-ACR-ACCOUNT   NOT = ZEROS)
           OR (BNA-ACR-SUB-ACCT  NOT = ZEROS)
               IF (BNA-ACR-DIST-CO       NOT = ZEROES)
018100             MOVE BNA-ACR-DIST-CO  TO IFACWS-COMPANY
               ELSE
                   IF (DDC-ACR-DIST-CO NOT = ZEROES)
                       MOVE DDC-ACR-DIST-CO TO IFACWS-COMPANY
                   ELSE
                       MOVE PRS-ACR-DIST-CO TO IFACWS-COMPANY
                   END-IF
               END-IF
018200         MOVE BNA-ACR-ACCT-UNIT    TO IFACWS-ACCT-UNIT
018300         MOVE BNA-ACR-ACCOUNT      TO IFACWS-ACCOUNT
018400         MOVE BNA-ACR-SUB-ACCT     TO IFACWS-SUB-ACCOUNT
               MOVE 1                    TO IFACWS-EDIT-TYPE
018500         PERFORM 635-EDIT-GLMASTER-60
018600         IF (ERROR-FOUND)
041900             MOVE BNBEN-LINE-FC-FN (I1)  TO CRT-FIELD-NBR
018900             GO TO 2900-END.

079100     PERFORM 860-FIND-NEXT-BNASET1.

       2900-END.

000100******************************************************************
       3000-SET-CHANGE-PREM-SW.
000100******************************************************************

           COMPUTE BNBEN-PREM-EMP-CONT = BEN-CMP-FLX-CONT
                                       + BEN-EMP-PRE-CONT
                                       + BEN-EMP-AFT-CONT.

           COMPUTE BNBEN-EMP-PREMIUM   = BNBEN-CMP-FLX-CONT (I1)
                                       + BNBEN-EMP-PRE-CONT (I1)
                                       + BNBEN-EMP-AFT-CONT (I1).

           IF (BNBEN-EMP-PREMIUM       NOT = BNBEN-PREM-EMP-CONT)
               MOVE WS-TRUE            TO BNBEN-CHANGE-PREMIUM-SW (I1).

           IF (BNBEN-COMP-CONT (I1)    NOT = BEN-COMP-CONT)
               MOVE WS-TRUE            TO BNBEN-COMP-CHANGED-SW (I1).

           IF ((BNBEN-COMP-CONT (I1)   NOT = BEN-COMP-MATCH)
           OR  (BNBEN-MAX-MATCH (I1)   NOT = BEN-COMP-CONT)
           OR  (BNBEN-PCT-MATCHED (I1) NOT = BEN-MTCH-UP-TO))
           AND (PLN-CONTRIB-TYPE           = "6" OR "7")
               MOVE WS-TRUE            TO BNBEN-CMP-CONT-CHG-SW (I1).

       3000-END.

063100******************************************************************
063200 3100-EDIT-SCREEN-BEN.
063300******************************************************************

036600     IF (BNBEN-LINE-FC (I1) NOT = "A")
036700         GO TO 3100-END.

           COMPUTE I2 = I1 + 1.

           PERFORM 3120-EDIT-NEXT-SCR-BEN
           THRU    3120-END
035600         VARYING I2 FROM I2 BY 1
035700         UNTIL  (I2 > BNBEN-NBR-LINES)
035800         OR     (ERROR-FOUND).

062900 3100-END.

063100******************************************************************
063200 3120-EDIT-NEXT-SCR-BEN.
063300******************************************************************

036600     IF (BNBEN-LINE-FC (I2) NOT = "A")
036700         GO TO 3120-END.

           IF  (BNBEN-PLAN-TYPE  (I1) = BNBEN-PLAN-TYPE (I2))
           AND (BNBEN-EMPLOYEE   (I1) = BNBEN-EMPLOYEE  (I2))
           AND (BNBEN-START-DATE (I1) = BNBEN-START-DATE (I2))
           AND (BNBEN-PLAN-CODE  (I1) = BNBEN-PLAN-CODE (I2))
      ******** DUPLICATE BENEFIT RECORD      
042400         MOVE 211                    TO CRT-ERROR-NBR
042500         MOVE BNBEN-LINE-FC-FN (I2)  TO CRT-FIELD-NBR
042600         GO TO 3120-END.

062900 3120-END.

000100******************************************************************
025000 2000-END.
000100******************************************************************

113000******************************************************************
113100 4000-BNBEN-PROCESS-TRAN         SECTION.
113200******************************************************************
       4000-START.
113300 
           MOVE CRT-PROGRAM-CODE       TO BNBEN-HIPAA-DEFAULT-SW.

114400     IF (BNBEN-FC                = "A" OR "C" OR "S" OR "D")
114500         PERFORM 4200-CHANGE
114600         THRU    4200-END.
115200
115700     GO TO 4000-END. 

115900******************************************************************
116000 4200-CHANGE.
116100******************************************************************
116200
           INITIALIZE I5.

      *    MOVE "Y"                    TO PREDM-EMP-MODIFIED
      *                                   PREDM-LOG-EMP-MOD.
      *CHGD I2 TO I3
           IF (USE-NAVIGATE)
               PERFORM
                   VARYING I3 FROM 1 BY 1
                   UNTIL  (I3              > BNBEN-NBR-LINES)
                   OR     (BNBEN-NBR (I3)  = ZEROES)

                   MOVE BNBEN-NBR (I3)     TO I1
                   PERFORM 4220-PROCESS-DETAIL
                   THRU    4220-END
               END-PERFORM
           ELSE
117100         PERFORM 4220-PROCESS-DETAIL
117200         THRU    4220-END
117300             VARYING I1 FROM 1 BY 1
117400             UNTIL  (I1 > BNBEN-NBR-LINES).
035900 
120500 4200-END.

120700******************************************************************
120800 4220-PROCESS-DETAIL.
120900******************************************************************
121000
121100     IF (BNBEN-LINE-FC (I1)          = SPACES)
121200         GO TO 4220-END.
121600
           IF (BNBEN-FUTURE-ERROR-SW (I1)  = "Y")
               GO TO 4220-END.

J17305     MOVE WS-FALSE                   TO BNBEN-CHG-BEN-SW.

FAK        MOVE BNBEN-COMPANY              TO DB-COMPANY.
125100     MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
125200     MOVE BNBEN-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
125300     MOVE BNBEN-PLAN-CODE (I1)       TO DB-PLAN-CODE.
P26300*    MOVE BNBEN-START-DATE (I1)      TO DB-START-DATE.
P26300     MOVE BENSET3-PLAN-CODE          TO WS-DB-BEG-RNG.
P26300     MOVE "(BEN-START-DATE <= ?)"    TO FILTER-STRING.
P26300     PERFORM 890-CREATE-FILTER.
P26300     MOVE BNBEN-START-DATE (I1)      TO DATETIME-FILTER-VALUE.
P26300     PERFORM 890-SET-DATETIME-FILTER-VALUE.
P53613     PERFORM 840-FIND-PLNSET1.
125500*
125600     IF (BNBEN-LINE-FC (I1) = "A")
P26300*        PERFORM 850-MODIFY-NLT-BENSET3
P26300         PERFORM 850-MODFILTER-BEGRNG-BENSET3
125800         IF  (BENEFIT-FOUND)
P26300*        AND (BEN-COMPANY   = DB-COMPANY)
P26300*        AND (BEN-EMPLOYEE  = DB-EMPLOYEE)
P26300*        AND (BEN-PLAN-TYPE = DB-PLAN-TYPE)
P26300*        AND (BEN-PLAN-CODE = DB-PLAN-CODE)
126300         AND (BEN-STOP-DATE = ZEROES)
126400             PERFORM 4260-STOP-EXISTING-BENEFIT
126500             THRU    4260-END
FAK                MOVE BNBEN-PLAN-CODE (I1) TO DB-PLAN-CODE.

005500     MOVE BNBEN-COMPANY              TO DB-COMPANY.
005600     MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
188900     PERFORM 840-FIND-EMPSET1.

125000     MOVE BNBEN-COMPANY              TO DB-COMPANY.
125200     MOVE BNBEN-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
121800     MOVE BNBEN-PLAN-CODE (I1)       TO DB-PLAN-CODE.
127400     PERFORM 840-FIND-PLNSET1.

248335*    MOVE BNBEN-COMPANY              TO DB-COMPANY.
248335*    MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
248335*    MOVE PLN-PLAN-TYPE              TO DB-PLAN-TYPE.
248335*    MOVE PLN-PLAN-CODE              TO DB-PLAN-CODE.
248335*    MOVE BENSET3-PLAN-CODE          TO WS-DB-BEG-RNG.
248335*    PERFORM 850-FIND-BEGRNG-BENSET3.
248335*    INITIALIZE BNWS-YTD-CONT
248335*               BNWS-EMP-YTD-AMT.
248335*    IF (BENEFIT-FOUND)
248335*        MOVE BEN-YTD-CONT           TO BNWS-YTD-CONT
248335*                                       BNBEN-YTD-CONT (I1)
248335*        MOVE BEN-EMP-YTD-CONT       TO BNWS-EMP-YTD-AMT
248335*                                       BNBEN-EMP-YTD-CONT (I1)
248335*    END-IF.

127100     IF (BNBEN-LINE-FC (I1)          = "A")
127200         PERFORM 800-CREATE-BENEFIT
           END-IF.

126800     IF (BNBEN-LINE-FC (I1)          = "C" OR "D" OR "S")
125000         MOVE BNBEN-COMPANY          TO DB-COMPANY
125100         MOVE BNBEN-EMPLOYEE (I1)    TO DB-EMPLOYEE
125200         MOVE BNBEN-PLAN-TYPE (I1)   TO DB-PLAN-TYPE
125300         MOVE BNBEN-PLAN-CODE (I1)   TO DB-PLAN-CODE
125400         MOVE BNBEN-START-DATE (I1)  TO DB-START-DATE
126900         PERFORM 840-MODIFY-BENSET1
           END-IF.
GW             IF  (BENEFIT-FOUND)
GW             AND (BNBEN-LINE-FC (I1) = "S")        
GW                 MOVE BEN-STOP-DATE      TO WS-BNBEN-STOP-DATE.

132000     PERFORM 4222-SET-BNWS-VARIABLES
132100     THRU    4222-END.

128600     IF (BNBEN-FLEX-FLAG (I1)        = "Y")
               IF  ((BNWS-POSITIVE-PREMIUM)
               AND  (BNBEN-EMP-PRE-CONT (I1) NOT = ZEROES))
               OR   (BNWS-NEGATIVE-PREMIUM)
               OR   (BNWS-CHANGED-PREMIUM)
               OR  ((BNBEN-LINE-FC  (I1)        = "D")
               AND ((BEN-CMP-FLX-CONT           NOT = ZEROES)
               OR   (BEN-EMP-PRE-CONT           NOT = ZEROES)))
                   MOVE BNBEN-COMPANY           TO DB-COMPANY
                   MOVE PLN-FLEX-PLAN           TO DB-FLEX-PLAN
                   PERFORM 840-FIND-FLPSET1
                   MOVE BNBEN-EFD-START-DT (I1) TO DB-START-DATE
                   MOVE BNBEN-EFD-GROUP-NM (I1) TO DB-GROUP-NAME
                   PERFORM 840-FIND-EFDSET1
                   MOVE EFD-FLD-START-DATE      TO DB-START-DATE
                   MOVE EFD-FLD-GROUP-NAME      TO DB-GROUP-NAME
                   PERFORM 840-FIND-FLDSET1
                   MOVE BNBEN-COMPANY           TO DB-COMPANY
                   MOVE BNBEN-EMPLOYEE (I1)     TO DB-EMPLOYEE
                   MOVE BNBEN-EFD-START-DT (I1) TO DB-PLAN-START-DT
129000             MOVE BNBEN-START-DATE (I1)   TO DB-START-DATE
129100             PERFORM 850-MODIFY-NLT-EFRSET2
                   PERFORM 785-GET-AVAIL-DOLLARS
                   MOVE BNBEN-START-DATE (I1)   TO BNWS-SCR-START-DATE
                   MOVE BNBEN-STOP-DATE (I1)    TO BNWS-SCR-STOP-DATE
                   MOVE BNBEN-PLAN-TYPE (I1)    TO BNFRWS-PLAN-TYPE
                   MOVE PLN-CONTRIB-TYPE        TO BNFRWS-CONTRIB-TYPE
129200             PERFORM 680-UPDATE-REMAINING-DOLLARS
129400             PERFORM 820-STORE-EMPFLEXREM
J16908*J57057             IF (EFR-CREDITS-AVAIL = ZEROES)
J16908*J57057                 MOVE BNBEN-COMPANY           TO DB-COMPANY
J16908*J57057                 MOVE BNBEN-EMPLOYEE (I1)     TO DB-EMPLOYEE
J16908*J57057                 MOVE BNBEN-EFD-START-DT (I1) TO DB-PLAN-START-DT
J16908*J57057                 MOVE EFR-START-DATE          TO DB-START-DATE
J16908*J57057                 PERFORM 840-MODIFY-EFRSET2
J16908*J57057                 IF (EMPFLEXREM-FOUND)
J16908*J57057                    PERFORM 830-DELETE-EMPFLEXREM
J16908*J57057                 END-IF 
J16908*J57057             END-IF
J57057         END-IF
J57057     END-IF.

           IF  (BNWS-POSITIVE-PREMIUM)
           OR  (BNWS-NEGATIVE-PREMIUM)
           OR  (BNWS-CHANGED-PREMIUM)
           OR  (BNWS-COMP-CHANGED)
           OR  (BNBEN-LINE-FC (I1)           = "D" OR "S")
           OR  ((BNBEN-LINE-FC (I1)          = "D")
           AND  (BEN-CMP-FLX-CONT            NOT = ZEROES))
J61613     OR  (CRT-PROGRAM-CODE = "BN45" OR "BN145")
FAK            PERFORM 5600-SET-BNEMDED-VARS
132300         IF  (BNBEN-LINE-FC (I1)       = "S")
132600         AND (BNBEN-STOP-DATE (I1)     NOT = BEN-STOP-DATE)
               AND (BNBEN-SPEND-ONLY (I1)    NOT = "Y")
                   IF  (BNBEN-DED-STOP-DATE (I1) NOT = ZEROES)
P85709             AND (PLN-COVERAGE-TYPE        NOT = "0")
P85709             AND (PLN-FLEX-PLAN            = SPACES)
P85709                 MOVE BNBEN-DED-STOP-DATE (I1)
P85709                                           TO BNWS-SCR-STOP-DATE
P85709             ELSE
P85709                 MOVE BNBEN-STOP-DATE (I1)
P85709                                           TO BNWS-SCR-STOP-DATE
                   END-IF
                   IF (BNBEN-PLAN-TYPE (I1)  = "RS")
                       PERFORM 640-CHANGE-EMDEDREC
                   ELSE
132800                 PERFORM 600-STOP-EXISTING-EMDEDREC
                   END-IF
                   INITIALIZE CRT-ERROR-NBR
133000         ELSE
               IF (BNWS-NEGATIVE-PREMIUM)
                   IF (BNBEN-LINE-FC (I1) = "A")
                       IF (BNWS-COMP-CONT     NOT = ZEROES)
                           MOVE BNWS-CMP-FLX-CONT   
                                              TO BNWS-SAV-CMP-FLX-CONT
                           MOVE BNWS-AFT-TAX-AMT TO BNWS-SAV-AFT-TAX-AMT
                           MOVE BNWS-PRE-TAX-AMT TO BNWS-SAV-PRE-TAX-AMT
                           INITIALIZE BNWS-CMP-FLX-CONT
                                      BNWS-AFT-TAX-AMT
                                      BNWS-PRE-TAX-AMT
                       ELSE  
                           NEXT SENTENCE
                       END-IF
                   END-IF
                   IF (BNBEN-LINE-FC (I1) = "C")
                       IF (BNWS-CHANGED-PREMIUM)
                       OR (BNWS-COMP-CHANGED)
                           MOVE BNWS-CMP-FLX-CONT   
                                              TO BNWS-SAV-CMP-FLX-CONT
                           MOVE BNWS-AFT-TAX-AMT TO BNWS-SAV-AFT-TAX-AMT
                           MOVE BNWS-PRE-TAX-AMT TO BNWS-SAV-PRE-TAX-AMT
                           INITIALIZE BNWS-CMP-FLX-CONT
                                      BNWS-AFT-TAX-AMT
                                      BNWS-PRE-TAX-AMT
                       ELSE
                           NEXT SENTENCE
                       END-IF
                   END-IF
               END-IF
J31958         INITIALIZE BNWS-DEDEDIT-TYPE
133100         IF (BNBEN-LINE-FC (I1)      = "D")
133200             PERFORM 610-DELETE-EMDEDREC
133300         ELSE
133400         IF (BNBEN-LINE-FC (I1)      = "A")
133500             PERFORM 620-ADD-EMDEDREC
133600         ELSE
133400         IF (BNBEN-LINE-FC (I1)      = "C" OR "S")
133700             PERFORM 640-CHANGE-EMDEDREC.
133800
           IF    (BNWS-NEGATIVE-PREMIUM)
           AND (((BNBEN-LINE-FC (I1) = "A")
           AND   (BNWS-COMP-CONT     NOT = ZEROES))
           OR   ((BNBEN-LINE-FC (I1) = "C")
           AND   (BNWS-COMP-CHANGED)
           OR    (BNWS-CHANGED-PREMIUM)))
               MOVE BNWS-SAV-AFT-TAX-AMT   TO BNWS-AFT-TAX-AMT
               MOVE BNWS-SAV-PRE-TAX-AMT   TO BNWS-PRE-TAX-AMT
               MOVE BNWS-SAV-CMP-FLX-CONT  TO BNWS-CMP-FLX-CONT.

       4220-UPDATE.

133900     IF (BNBEN-LINE-FC (I1)            = "A" OR "C" OR "S")
               IF  (PLN-PLAN-NAME                  NOT = SPACES)
               AND (BNBEN-TRIGGER-ENABLED-SW (I1) = WS-TRUE)
               AND ((BNBEN-LINE-FC (I1)           = "S")
               OR   ((BNBEN-LINE-FC (I1)          = "C")
               AND   (BNBEN-MULT-SALARY (I1)      NOT = BEN-NBR-HOURS)))
                   IF (CRT-PROGRAM-CODE           NOT = "BN531")
                       PERFORM 4244-CHANGE-BNTAEMPBAL-RECORD
                       THRU    4244-END
                   ELSE
                   IF (BNBEN-BTE-EMP-GRP (I1)     NOT = SPACES)
                       PERFORM 4246-CHANGE-BTE-FOR-BN531
                       THRU    4246-END
                   END-IF
                   END-IF
               END-IF
P86484         IF  (PLN-COVERAGE-TYPE        NOT = "0")
P86484         AND (PLN-FLEX-PLAN           = SPACES)
P86484             MOVE BNWS-SCR-START-DATE TO BNBEN-DED-START-DATE (I1)
P86484         END-IF
133900         IF (BNBEN-LINE-FC (I1)        = "A" OR "C")
134000             PERFORM 5100-MOVE-DETAIL-DATA
134100             THRU    5100-END
               ELSE
133900         IF (BNBEN-LINE-FC (I1)        = "S")
J17305             IF  (BNBEN-STOP-DATE (I1) NOT = BEN-STOP-DATE)
J17305                 MOVE WS-TRUE          TO BNBEN-CHG-BEN-SW
J17305             END-IF
                   MOVE BNBEN-STOP-DATE (I1) TO BEN-STOP-DATE
P83180             IF  (BNBEN-DED-STOP-DATE (I1) = ZEROES)
P83180             AND (PLN-COVERAGE-TYPE        NOT = "0")
J51835             AND (PLN-FLEX-PLAN            = SPACES)
J17305                 IF (BNBEN-STOP-DATE (I1) NOT = BEN-DED-STOP-DATE)
J17305                     MOVE WS-TRUE      TO BNBEN-CHG-BEN-SW
J17305                 END-IF
P83180                 MOVE BNBEN-STOP-DATE (I1)
P83180                                       TO BEN-DED-STOP-DATE
P83180             ELSE
P86484                 IF  (PLN-COVERAGE-TYPE = "0")
P86484                 AND (PLN-FLEX-PLAN     = SPACES)
J17305                     IF  (BEN-DED-START-DATE   NOT = ZEROES)
J17305                         MOVE WS-TRUE          TO BNBEN-CHG-BEN-SW
J17305                     END-IF
P86484                     MOVE ZEROES       TO BEN-DED-START-DATE
P86484                 ELSE
J17305                     IF  (BNBEN-DED-STOP-DATE (I1) NOT =
J17305                                          BEN-DED-STOP-DATE)
J17305                         MOVE WS-TRUE  TO BNBEN-CHG-BEN-SW
J17305                     END-IF
P86484                     MOVE BNBEN-DED-STOP-DATE (I1)
P86484                                       TO BEN-DED-STOP-DATE
P86484                 END-IF
P83180             END-IF
P83180             IF  (PLN-COVERAGE-TYPE        NOT = "0")
J51835             AND (PLN-FLEX-PLAN                = SPACES)
J17305                 IF  (BNBEN-DED-START-DATE (I1) NOT = 
J17305                                              BEN-DED-START-DATE)
J17305                     MOVE WS-TRUE          TO BNBEN-CHG-BEN-SW
J17305                 END-IF
P56534                 MOVE BNBEN-DED-START-DATE(I1)
P56534                                       TO BEN-DED-START-DATE
P85709             END-IF



J67329             IF (BNBEN-LINE-FC (I1) = "A" OR "C" OR "S")
J17305             AND (BNBEN-CHANGING-BEN)
087429*                MOVE WS-SYSTEM-DATE-YMD   TO BEN-UPD-DATE

087429                 IF  (CRT-PROGRAM-CODE     = "BN531")
087429                 AND (BNBEN-UPD-DATE (I1)  NOT = ZEROES)
087429                     MOVE BNBEN-TIME-STAMP (I1) TO BEN-TIME-STAMP
087429                     MOVE BNBEN-UPD-DATE (I1)   TO BEN-UPD-DATE
087429                 ELSE
                           MOVE HHMMSS                TO BEN-TIME-STAMP
087429                     MOVE WS-SYSTEM-DATE-YMD    TO BEN-UPD-DATE
087429                 END-IF

                       IF (CRT-PROGRAM-CODE      = "BN100")
                       OR (CRT-PROGRAM-CODE      = "BN101")
                       OR (CRT-PROGRAM-CODE      = "BN102")
                       OR (CRT-PROGRAM-CODE      = "BN103")
                       OR (CRT-PROGRAM-CODE      = "BN104")
                       OR (CRT-PROGRAM-CODE      = "BN105")
                           MOVE CRT-PROGRAM-CODE TO BEN-USER-ID
                       ELSE
                           IF (BNBEN-USER-ID (I1)    = SPACES)
                               MOVE CRT-USER-NAME    
                                             TO BEN-USER-ID
                           ELSE
                               MOVE BNBEN-USER-ID (I1)
                                             TO BEN-USER-ID
                           END-IF
                       END-IF
J67329             END-IF
               END-IF
               END-IF
               IF  ((BNBEN-LINE-FC (I1)            = "A")
               AND  (PLN-PLAN-NAME                 NOT = SPACES)
               AND  (BNBEN-TRIGGER-ENABLED-SW (I1) = WS-TRUE))
               OR  ((CRT-PROGRAM-CODE              = "BN531")
               AND  (BNBEN-LINE-FC (I1)            = "C")
               AND  (BNBEN-BTE-EMP-GRP (I1)        NOT = SPACES)
               AND  (BNTAEMPBAL-NOTFOUND))
                   MOVE "LPBEN"                    TO IFOBIWS-OBJ-TYPE
                   PERFORM 7000-ASSIGN-OBJ-ID-70
                   MOVE IFOBIWS-OBJ-ID             TO BEN-TA-OBJ-ID

                   PERFORM 4240-CREATE-BNTAEMPBAL-RECORD
                   THRU    4240-END
               END-IF
                                                  
J67329         IF (BNBEN-LINE-FC (I1) = "A")
087429*            MOVE WS-SYSTEM-DATE-YMD         TO BEN-CREATION-DATE

087429             IF  (CRT-PROGRAM-CODE              = "BN531")
087429             AND (BNBEN-CREATION-DATE (I1)  NOT = ZEROES)
087429                 MOVE BNBEN-TIME-STAMP (I1)  TO BEN-CREATE-TIME
087429                 MOVE BNBEN-CREATION-DATE (I1) 
087429                                             TO BEN-CREATION-DATE
087429             ELSE
J67329                 MOVE HHMMSS                 TO BEN-CREATE-TIME
087429                 MOVE WS-SYSTEM-DATE-YMD     TO BEN-CREATION-DATE
087429             END-IF

J67329             IF (CRT-PROGRAM-CODE      = "BN100")
J67329             OR (CRT-PROGRAM-CODE      = "BN101")
J67329             OR (CRT-PROGRAM-CODE      = "BN102")
J67329             OR (CRT-PROGRAM-CODE      = "BN103")
J67329             OR (CRT-PROGRAM-CODE      = "BN104")
J67329             OR (CRT-PROGRAM-CODE      = "BN105")
J67329                 MOVE CRT-PROGRAM-CODE       TO BEN-CREATE-USER-ID
J67329             ELSE
J67329                 IF (BNBEN-USER-ID (I1)    = SPACES)
J67329                     MOVE CRT-USER-NAME      TO BEN-CREATE-USER-ID
J67329                 ELSE
J67329                     MOVE BNBEN-USER-ID (I1) TO BEN-CREATE-USER-ID
J67329                 END-IF
J67329             END-IF
J67329         END-IF

J67329         IF (BNBEN-LINE-FC (I1) = "A" OR "C" OR "S")
J17305         AND (BNBEN-CHANGING-BEN)
087429*            MOVE WS-SYSTEM-DATE-YMD         TO BEN-UPD-DATE

087429             IF  (CRT-PROGRAM-CODE     = "BN531")
087429             AND (BNBEN-UPD-DATE (I1)  NOT = ZEROES)
087429                 MOVE BNBEN-TIME-STAMP (I1)  TO BEN-TIME-STAMP
087429                 MOVE BNBEN-UPD-DATE (I1)    TO BEN-UPD-DATE    
087429             ELSE
J67329                 MOVE HHMMSS                 TO BEN-TIME-STAMP
087429                 MOVE WS-SYSTEM-DATE-YMD     TO BEN-UPD-DATE
087429             END-IF

J67329             IF (CRT-PROGRAM-CODE      = "BN100")
J67329             OR (CRT-PROGRAM-CODE      = "BN101")
J67329             OR (CRT-PROGRAM-CODE      = "BN102")
J67329             OR (CRT-PROGRAM-CODE      = "BN103")
J67329             OR (CRT-PROGRAM-CODE      = "BN104")
J67329             OR (CRT-PROGRAM-CODE      = "BN105")
J67329                 MOVE CRT-PROGRAM-CODE       TO BEN-USER-ID
J67329             ELSE
J67329                 IF (BNBEN-USER-ID (I1)    = SPACES)
J67329                     MOVE CRT-USER-NAME      TO BEN-USER-ID
J67329                 ELSE
J67329                     MOVE BNBEN-USER-ID (I1) TO BEN-USER-ID
J67329                 END-IF
J67329             END-IF
J67329         END-IF
               PERFORM 820-STORE-BENEFIT
********===========================================================
      *   CPS MODS START
********===========================================================
***********  WBP MOD 8/2003 TO UPDATE WBPBENTAG ************************
               IF  (BEN-PLAN-CODE >= "PAAA")
               AND (BEN-PLAN-CODE <= "PZZZ")

12/06****GW** GET PRIOR NONPARTIC-CD SEE WO4422 ************************

                  MOVE "0"               TO WS-NONPARTIC-CD
                  MOVE BEN-COMPANY       TO DB-COMPANY
                  MOVE BEN-EMPLOYEE      TO DB-EMPLOYEE
                  INITIALIZE                DB-START-DATE
                                            DB-PLAN-TYPE
                                            DB-PLAN-CODE
                  MOVE WENSET2-EMPLOYEE  TO WS-DB-BEG-RNG
                  PERFORM 850-FIND-BEGRNG-WENSET2

                  PERFORM
                  UNTIL (WBPBENTAG-NOTFOUND)
******** GW 3/2008 WO 5794 BEGIN *************************************
                     IF (WEN-PLAN-CODE = "PNP")
                          MOVE "0"               TO WS-NONPARTIC-CD
                     ELSE
                          MOVE WEN-NONPARTIC-CD  TO WS-NONPARTIC-CD
                      END-IF
******** GW 3/2008 WO 5794 END  **************************************
                     PERFORM 860-FIND-NXTRNG-WENSET2
                  END-PERFORM
12/06********** END **************************************************
                  MOVE BEN-COMPANY       TO DB-COMPANY
                  MOVE BEN-PLAN-TYPE     TO DB-PLAN-TYPE
                  MOVE BEN-EMPLOYEE      TO DB-EMPLOYEE
                  MOVE BEN-START-DATE    TO DB-START-DATE
                  MOVE BEN-PLAN-CODE     TO DB-PLAN-CODE
                  PERFORM 840-MODIFY-WENSET1
                  IF (WBPBENTAG-NOTFOUND)
                     PERFORM 800-CREATE-WBPBENTAG
                     MOVE BEN-COMPANY       TO WEN-COMPANY
                     MOVE BEN-PLAN-TYPE     TO WEN-PLAN-TYPE
                     MOVE BEN-EMPLOYEE      TO WEN-EMPLOYEE
                     MOVE BEN-START-DATE    TO WEN-START-DATE
                     MOVE BEN-PLAN-CODE     TO WEN-PLAN-CODE
12/06***** GW        MOVE "0"               TO WEN-NONPARTIC-CD
SDB411               IF  (WS-NONPARTIC-CD NOT = SPACES)
12/06                   MOVE WS-NONPARTIC-CD   TO WEN-NONPARTIC-CD
SDB411               END-IF 
SDB411*              MOVE "C"               TO WEN-CRP-VEST
SDB411               MOVE "X"               TO WEN-CRP-VEST
SDB411               IF (BEN-PLAN-CODE(2:1) = "A")
SDB411                   MOVE "V" TO WEN-CRP-VEST
SDB411               END-IF
SDB411               IF (BEN-PLAN-CODE(3:1) = "S")
SDB411               OR (BEN-PLAN-CODE(3:1) = "M")  
SD1028               OR (BEN-PLAN-CODE(3:1) = "F")
                     OR (BEN-PLAN-CODE(3:1) = "R")                   
SDB411                  MOVE "C" TO WEN-CRP-VEST
SDB411               END-IF

SDB411               IF (BEN-PLAN-CODE = "PNP")
SDB411                   MOVE " " TO WEN-CRP-VEST
SDB411               END-IF
SDB411
SDB411*              MOVE "C"               TO WEN-SRA-VEST
SDB411               MOVE "X"               TO WEN-SRA-VEST
SDB411               IF (BEN-PLAN-CODE NOT = "PNP")                   
SDB411                    MOVE "C" TO WEN-SRA-VEST
SDB411               END-IF
SDB411               IF (BEN-PLAN-CODE(3:1) = "E")
SDB411                  MOVE "V" TO WEN-SRA-VEST
SDB411               END-IF
SDB411               IF (BEN-PLAN-CODE  = "PNP")                   
SDB411                    MOVE "X" TO WEN-SRA-VEST
SDB411               END-IF
SDB411               IF (BEN-PLAN-CODE = "PNP")
SDB411                  IF   (BEN-EMPLOYEE = EMP-EMPLOYEE)
SDB411                  AND ((EMP-EMP-STATUS = "TW")
SD1028                  OR   (EMP-EMP-STATUS = "TO")  
SDB411                  OR   (EMP-EMP-STATUS = "TV")                    
SDB411                  OR   (EMP-EMP-STATUS = "RE")
SDB815                  OR   (EMP-EMP-STATUS = "RN")
SR4823                  OR   (EMP-EMP-STATUS = "RD"))
SDB411                      MOVE "I"        TO WEN-SRA-VEST                            
SDB411                  END-IF
SDB411               END-IF
SDB411
SDB411               IF (BEN-PLAN-CODE = "PNP")
SDB411                  IF  (BEN-EMPLOYEE = EMP-EMPLOYEE)
SDB411                  AND (EMP-EMP-STATUS = "TW")
SDB411                      MOVE 1          TO WEN-NONPARTIC-CD
SDB411                  ELSE
SDB411                    IF  (BEN-EMPLOYEE = EMP-EMPLOYEE)
SDB411                    AND (EMP-PROCESS-LEVEL(1:1) = "0")
                          AND (EMP-EMP-STATUS        = "AW")
SDB411                        MOVE 19         TO WEN-NONPARTIC-CD
                          ELSE       
SDB411                      IF  (BEN-EMPLOYEE = EMP-EMPLOYEE)
SDB411                      AND (EMP-PROCESS-LEVEL(1:1) = "0")
SDB416                      AND ((EMP-EMP-STATUS(1:1) = "A")
                             OR  (EMP-EMP-STATUS(1:1) = "I") 
                             OR  (EMP-EMP-STATUS(1:1) = "D")
                             OR  (EMP-EMP-STATUS(1:1) = "L"))
SDB411                         MOVE 6          TO WEN-NONPARTIC-CD
SDB411                      ELSE
SDB416                        IF (EMP-EMP-STATUS(1:1) = "T")
                              OR (EMP-EMP-STATUS(1:1) = "R")
SDB411                          MOVE 2          TO WEN-NONPARTIC-CD 
                              END-IF  
SDB411                      END-IF
SDB411                    END-IF
                        END-IF
SDB411               END-IF

                     MOVE CRT-USER-NAME     TO WEN-USER-ID
                     MOVE HHMMSS            TO WEN-TIME-STAMP
                     MOVE WS-SYSTEM-DATE-YMD   TO WEN-DATE-STAMP
                     PERFORM 4221-GET-WEN-FIELDS
                        THRU 4221-END

MG0629               IF (WEN-EMP-STATUS = "TW")
MG0629                   MOVE "1"               TO WEN-NONPARTIC-CD
MG0629                   MOVE " "               TO WEN-CRP-VEST
MG0629                   MOVE "I"               TO WEN-SRA-VEST
MG0629               END-IF
MG0629
                     PERFORM 820-STORE-WBPBENTAG.
***********  END WBP ***************************************************
AI0095******************************************************************
AI0095****   ANALYSTS INTERNATIONAL   RDAHL    3/24/03             *****
AI0095****   ENHANCEMENT FOR MOD 9.5 RETROACTIVE BILLING           *****
AI0095******************************************************************
RAY        PERFORM 4400-RETRO-BILL-TRIGGER.
RAY   *----------------------------------------------------------------
*******        END-IF.
********===========================================================
      *   CPS MODS END
********===========================================================

134600     IF  (BNBEN-LINE-FC (I1)        = "A")
GW7/07*** the below line has been deleted in production *****************
GW7/07*    AND (BNBEN-PLAN-TYPE (I1)      = "DC")
           AND (CRT-PROGRAM-CODE          = "BN31" OR "BN32" OR "BN531")
               PERFORM 4280-CREATE-EMPINVEST
               THRU    4280-END.

VAN        IF  (BNBEN-LINE-FC (I1)         = "S")
           AND (BNBEN-PLAN-TYPE (I1)       NOT = "RS")
J17305         IF  (BNBEN-STOP-DATE (I1) NOT = BEN-STOP-DATE)
J17305             MOVE WS-TRUE          TO BNBEN-CHG-BEN-SW
J17305         END-IF
               MOVE BNBEN-STOP-DATE (I1)   TO BEN-STOP-DATE
               IF (BNBEN-PLAN-TYPE (I1)    = "HL" OR "DN" OR "EL")
               OR (BNBEN-PLAN-TYPE (I1)    = "DL" OR "VA")
                   PERFORM 5110-STOP-HRDEPBEN
                   THRU    5110-END
               END-IF
               IF (BNBEN-PLAN-TYPE (I1)    = "DC")
                   PERFORM 4300-CHANGE-EMPINVEST
                   THRU    4300-END.
183300
           IF  (BNBEN-CREATE-TRANS (I1)    = SPACES)
           OR  ((BNBEN-CREATE-TRANS (I1)   = "Y" OR "P")
           AND  (BNBEN-HIPAA-DEFAULT))
               PERFORM 5300-DEFAULT-HIPAA-FIELDS
               THRU    5300-END.

           IF (BNBEN-CREATE-TRANS (I1)       = "Y")
               PERFORM 5200-CREATE-BNTRANS
               THRU    5200-END.
                   
P66372     IF (BNBEN-CREATE-TRANS (I1)       = "P")
P66372         PERFORM 5400-CREATE-WORKFLOW-TRIGGER
P66372         THRU    5400-END.
P66372
134600     IF (BNBEN-LINE-FC (I1)          = "D")
               IF (BNBEN-PLAN-TYPE (I1)    = "DC")
                   PERFORM 4226-DELETE-STANDTIME
                   THRU    4226-END

                   PERFORM 4228-DELETE-EMPINVEST
                   THRU    4228-END
               END-IF

               IF (CRT-PROGRAM-CODE        = "BN100")
                   PERFORM 4230-DELETE-HRDEPBEN
                   THRU    4230-END
               END-IF

134700         PERFORM 830-DELETE-BENEFIT
ACS002         SET BENEFIT-DELETED     TO TRUE 
***********  WBP MOD 8/2003 TO UPDATE WBPBENTAG ************************

               MOVE BEN-COMPANY       TO DB-COMPANY
               MOVE BEN-PLAN-TYPE     TO DB-PLAN-TYPE
               MOVE BEN-EMPLOYEE      TO DB-EMPLOYEE
               MOVE BEN-START-DATE    TO DB-START-DATE
               MOVE BEN-PLAN-CODE     TO DB-PLAN-CODE
               PERFORM 840-MODIFY-WENSET1
               IF (WBPBENTAG-FOUND)
                  PERFORM 830-DELETE-WBPBENTAG
               END-IF
***********  END WBP ***************************************************

               PERFORM 4224-DELETE-BNCOMMENTS
               THRU    4224-END

               IF  (PLN-PLAN-NAME                  NOT = SPACES)
               AND (BNBEN-TRIGGER-ENABLED-SW (I1)  = WS-TRUE)
                   MOVE BNBEN-COMPANY              TO DB-COMPANY
                   MOVE BNBEN-PLAN-CODE (I1)       TO DB-PLAN-CODE
                   MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE
                   MOVE BNBEN-START-DATE  (I1)     TO DB-START-DATE
                   MOVE BTESET3-START-DATE         TO WS-DB-BEG-RNG
                   PERFORM 830-DELETERNG-BTESET3
               END-IF

J17193* DELETE BENEFIT OFFER
J17193         IF  (BNBEN-LINE-FC (I1)   = "D")
J17193         AND (BNBEN-PLAN-TYPE (I1) = "HL")
J17193             MOVE I1                      TO BNACAWS-SV-I1
J17193             MOVE BNBEN-COMPANY           TO BNACAWS-COMPANY 
J17193             MOVE BNBEN-EMPLOYEE (I1)     TO BNACAWS-EMPLOYEE
J17193             MOVE BNBEN-START-DATE (I1)   TO BNACAWS-NEW-DATE
J17193             MOVE BNBEN-LINE-FC (I1)      TO BNACAWS-BN-LINE-FC
J17193             MOVE BNBEN-PLAN-TYPE (I1)    TO BNACAWS-BN-PLAN-TYPE
J17193             MOVE BNBEN-PLAN-CODE (I1)    TO BNACAWS-BN-PLAN-CODE
J17193             PERFORM 4100-DLT-ACA-OFFER
J17193             MOVE BNACAWS-SV-I1           TO I1
J17193             MOVE BNBEN-COMPANY           TO DB-COMPANY 
J17193             MOVE BNBEN-PLAN-TYPE (I1)    TO DB-PLAN-TYPE
J17193             MOVE BNBEN-PLAN-CODE (I1)    TO DB-PLAN-CODE
J17193             PERFORM 840-FIND-PLNSET1
J17193         END-IF

               INITIALIZE BNBEN-BENEFIT-DETAIL (I1).

J17193* CREATE BENEFIT OFFER
J17193     IF  (BNBEN-LINE-FC (I1)   = "A" OR "C")
J17193     AND (BNBEN-PLAN-TYPE (I1) = "HL")
J17193     AND (BNBEN-UPDATE-ACA NOT = "N")
J17193         MOVE I1                      TO BNACAWS-SV-I1
J17193         MOVE BNBEN-COMPANY           TO BNACAWS-COMPANY 
J17193         MOVE BNBEN-EMPLOYEE (I1)     TO BNACAWS-EMPLOYEE
J17193         MOVE BNBEN-START-DATE (I1)   TO BNACAWS-NEW-DATE
J17193         MOVE BNBEN-LINE-FC (I1)      TO BNACAWS-BN-LINE-FC
J17193         MOVE BNBEN-PLAN-TYPE (I1)    TO BNACAWS-BN-PLAN-TYPE
J17193         MOVE BNBEN-PLAN-CODE (I1)    TO BNACAWS-BN-PLAN-CODE
J17193         IF (BNBEN-ENROLLMENT-SW (I1) = ZEROES)
J17193             MOVE "Y"                 TO BNBEN-ACA-INIT-ENROLL
J17193         END-IF
J17193         IF (CRT-PROGRAM-CODE = "BN531")
J17193             MOVE SPACES              TO BNREWS-USE-PGE-TBL
J17193                                         BNREWS-USE-BWT-TBL
J17193                                         BNREWS-USE-CVR-TBL
J17193                                         BNREWS-USE-PRE-TBL
J17193         END-IF
J17193         IF (BNBEN-WEB-UPDATE = "Y")
J17193             MOVE BNBEN-RULE-TYPE     TO BNACAWS-RULE-TYPE
J17193             MOVE BNBEN-FAMILY-STATUS TO BNACAWS-FAMILY-STATUS
J17193             MOVE BNBEN-NEW-DATE      TO BNACAWS-SV-NEW-DATE
J17193             MOVE BNBEN-START-DATE (I1) 
J17193                                      TO BNACAWS-SV-START-DATE
J17193             PERFORM 2100-CREATE-EMSS-OFFER
J17193         ELSE
J57497             MOVE BNBEN-NEW-DATE      TO BNACAWS-SV-NEW-DATE
J17193             PERFORM 1100-CRT-ACA-OFFER
J17193         END-IF
J17193         MOVE SPACES                  TO BNBEN-ACA-INIT-ENROLL
030162         MOVE ZEROES                  TO BNEDWS-ELIGIBILITY-DATE
031132         MOVE SPACES                  TO BNEDWS-FROM-MAGIC-SW
J17193         IF (CRT-PROGRAM-CODE = "BN531")
J17193             MOVE "Y"                 TO BNREWS-USE-PGE-TBL
J17193                                         BNREWS-USE-BWT-TBL
J17193                                         BNREWS-USE-CVR-TBL
J17193                                         BNREWS-USE-PRE-TBL
J17193         END-IF
J17193         MOVE BNACAWS-SV-I1           TO I1
J17193         MOVE BNBEN-COMPANY           TO DB-COMPANY 
J17193         MOVE BNBEN-PLAN-TYPE (I1)    TO DB-PLAN-TYPE
J17193         MOVE BNBEN-PLAN-CODE (I1)    TO DB-PLAN-CODE
J17193         PERFORM 840-FIND-PLNSET1
J49035         MOVE BNBEN-COMPANY           TO DB-COMPANY
J49035         MOVE BNBEN-EMPLOYEE (I1)     TO DB-EMPLOYEE
J49035         MOVE BNBEN-PLAN-TYPE (I1)    TO DB-PLAN-TYPE
J49035         MOVE BNBEN-PLAN-CODE (I1)    TO DB-PLAN-CODE
J49035         MOVE BNBEN-START-DATE (I1)   TO DB-START-DATE
J49035         PERFORM 840-FIND-BENSET1
J17193     END-IF.
J17193
135100
135200 4220-END.

137700******************************************************************
       4221-GET-WEN-FIELDS.
137700******************************************************************
***********  WBP MOD 8/2003 TO UPDATE WBPBENTAG ************************

           MOVE "A-ROSTER CODE"           TO DB-FIELD-NAME.
           PERFORM 840-FIND-HRUSET2.
           IF (HRUSERFLDS-FOUND)
              MOVE BEN-COMPANY            TO DB-COMPANY
              INITIALIZE DB-EMP-APP
              MOVE BEN-EMPLOYEE           TO DB-EMPLOYEE
              MOVE HRU-FIELD-KEY          TO DB-FIELD-KEY
              PERFORM 840-FIND-HEUSET1
              IF (HREMPUSF-FOUND)
                 MOVE HEU-A-FIELD  TO WEN-LCMS-ROSTER 
              ELSE
                 MOVE SPACES      TO WEN-LCMS-ROSTER.

           MOVE "A-SELF EMPL SS PART"  TO DB-FIELD-NAME.
           PERFORM 840-FIND-HRUSET2.
           IF (HRUSERFLDS-FOUND)
              MOVE BEN-COMPANY            TO DB-COMPANY
              INITIALIZE DB-EMP-APP
              MOVE BEN-EMPLOYEE           TO DB-EMPLOYEE
              MOVE HRU-FIELD-KEY          TO DB-FIELD-KEY
              PERFORM 840-FIND-HEUSET1
              IF (HREMPUSF-FOUND)
                 MOVE HEU-A-FIELD  TO WEN-SS-PARTIC
              ELSE
                 MOVE SPACES       TO WEN-SS-PARTIC.

           MOVE BEN-COMPANY        TO DB-COMPANY.
           MOVE BEN-EMPLOYEE       TO DB-EMPLOYEE.
           PERFORM 840-FIND-EMPSET1.
           IF (EMPLOYEE-FOUND)
              MOVE EMP-EMP-STATUS    TO WEN-EMP-STATUS 
              MOVE EMP-PROCESS-LEVEL TO WEN-PROCESS-LEVEL
           ELSE
              MOVE SPACES    TO WEN-EMP-STATUS 
              MOVE SPACES    TO WEN-PROCESS-LEVEL.  
                                     
       4221-END.

***********  WBP END 
137700******************************************************************
137800 4222-SET-BNWS-VARIABLES.
137900******************************************************************
138000
138100     MOVE BNBEN-COMPANY              TO BNWS-COMPANY.
138200     MOVE BNBEN-EMPLOYEE (I1)        TO BNWS-EMPLOYEE.
P85709     IF  (PLN-COVERAGE-TYPE              NOT = "0")
P85709     AND (PLN-FLEX-PLAN                  = SPACES)
P85709         IF (BNBEN-DED-STOP-DATE (I1)   NOT = ZEROES)
P85709             IF (BNBEN-START-DATE (I1 + 1) <
P85709                                         BNBEN-DED-STOP-DATE (I1))
P85709                 MOVE BNBEN-STOP-DATE (I1)
P85709                                     TO BNWS-SCR-STOP-DATE
O85709             ELSE
P85709                 MOVE BNBEN-DED-STOP-DATE (I1)
P85709                                     TO BNWS-SCR-STOP-DATE
P85709             END-IF
P85709         ELSE
P85709             MOVE BNBEN-STOP-DATE (I1)
P85709                                     TO BNWS-SCR-STOP-DATE
P85709         END-IF
P85709         IF  (BNBEN-LINE-FC (I1) = "S")
P85709         AND (CRT-PROGRAM-CODE   NOT = "BN32")
P85709         AND (CRT-PROGRAM-CODE   NOT = "BN103")
P85709             IF (BNWS-SCR-STOP-DATE < BNBEN-DED-STOP-DATE (I1))
P85709                 MOVE BNWS-SCR-STOP-DATE TO
P85709                                         BNBEN-DED-STOP-DATE (I1)
P85709             END-IF
P85709         END-IF
P85709     END-IF.
P85709
138400     MOVE BNBEN-START-DATE (I1)      TO BNWS-START-DATE
                                              BNWS-SCR-START-DATE.
           MOVE BNBEN-ZERO-PREMIUM-SW (I1) TO BNWS-ZERO-PREMIUM-SW.
           MOVE BNBEN-NEG-PREMIUM-SW (I1)  TO BNWS-NEGATIVE-PREMIUM-SW.
           MOVE BNBEN-POS-PREMIUM-SW (I1)  TO BNWS-POSITIVE-PREMIUM-SW.
           MOVE BNBEN-CHANGE-PREMIUM-SW (I1)
                                           TO BNWS-CHANGE-PREMIUM-SW.
           MOVE BNBEN-COMP-CHANGED-SW (I1) TO BNWS-COMP-CHANGED-SW.
           MOVE BNBEN-CMP-CONT-CHG-SW (I1) TO BNWS-CMP-CONT-CHG-SW.

139500     MOVE BNBEN-EMP-AFT-CONT (I1)    TO BNWS-AFT-TAX-AMT.
139700     MOVE BNBEN-EMP-PRE-CONT (I1)    TO BNWS-PRE-TAX-AMT.

           IF (BNWS-ZERO-PREMIUM)
           OR (BNWS-POSITIVE-PREMIUM)
               MOVE BNBEN-EMP-PRE-CONT (I1)
                                           TO BNWS-EMP-PRE-CONT
           ELSE
               MOVE ZEROES                 TO BNWS-EMP-PRE-CONT.

           MOVE BNBEN-EMP-AFT-CONT (I1)    TO BNWS-EMP-AFT-CONT.
           MOVE BNBEN-CMP-FLX-CONT (I1)    TO BNWS-CMP-FLX-CONT.
           MOVE BNBEN-COMP-CONT (I1)       TO BNWS-COMP-CONT.
           MOVE BNBEN-PCT-MATCHED (I1)     TO BNWS-CND-PCT-MATCHED.
           MOVE BNBEN-MAX-MATCH   (I1)     TO BNWS-MAX-MATCH.      

           MOVE BNBEN-PCT-AMT-FLAG (I1)    TO BNWS-PCT-AMT-FLAG.

139400     IF (BNBEN-FLEX-FLAG (I1)        = "N")
139900         INITIALIZE BNWS-CMP-FLX-CONT.
          
           MOVE BNBEN-COVER-AMT (I1)       TO BNWS-PAY-PER-AMT.
           MOVE BNBEN-PAY-RATE (I1)        TO BNWS-TOT-CONT.
140200
           MOVE BNBEN-COVER-OPT (I1)       TO BNWS-TOTAL-CYC.
           MOVE BNBEN-SPEND-ONLY (I1)      TO BNWS-SPEND-ONLY.
           MOVE BNBEN-FLEX-FLAG (I1)       TO BNWS-FLEX-FLAG.
J46581     IF (BNBEN-PRE-MATCH-OPTION (I1) = SPACES) 
J46581         PERFORM 5500-GET-COMP-CONT-SW
J46581         MOVE BNWS-COMP-CONT-SW     TO BNBEN-PRE-MATCH-OPTION (I1)
J46581     END-IF.
           MOVE BNBEN-PRE-MATCH-OPTION (I1)
                                           TO BNWS-COMP-MATCH.
           MOVE BNBEN-PRE-MATCH-CALC (I1)  TO BNWS-PRE-MATCH-CALC.

           MOVE BNBEN-BNA-START-DATE (I1)  TO BNWS-BNA-START-DATE.
           MOVE BNBEN-BNA-GROUP-NAME (I1)  TO BNWS-BNA-GROUP-NAME.

           INITIALIZE BNWS-CMP-DEC-BAL
                      BNWS-EMP-P-DEC-BAL
                      BNWS-EMP-A-DEC-BAL
138500                BNWS-YOS
138600                BNWS-TOT-BOND-DIFF
138700                BNWS-CONTR-UPD-DT.
139300
FAK        IF (BNBEN-FLEX-FLAG (I1)        = "Y")
               IF (BNBEN-LINE-FC (I1)      = "S")
                   MOVE "C"                TO BNWS-LINE-FC
               ELSE
               MOVE BNBEN-LINE-FC (I1)     TO BNWS-LINE-FC.

           IF (PLN-CONTRIB-TYPE            NOT = "0")
J90269         INITIALIZE BNWS-COVER-OPT
J90269         MOVE BNBEN-COVER-OPT (I1)   TO BNWS-COVER-OPT
130600         PERFORM 430-MOVE-DEDFREQTBL-TO-WS
J90269     END-IF.

           MOVE WS-FALSE                   TO BNWS-BN39-SW
                                              BNWS-BN39-PRE-SW.

           IF  (PLN-CONTRIB-TYPE          = "5")
           AND (PLN-FLEX-PLAN             NOT = SPACES)
           AND (PLN-PLAN-TYPE             NOT = "VA")
               MOVE WS-TRUE               TO BNWS-BN39-SW.

           MOVE BNBEN-LINE-FC (I1)        TO BNWS-MAX-LINE-FC.
           MOVE BNBEN-MID-YEAR-SW (I1)    TO BNWS-MID-YEAR-SW.
           MOVE BNBEN-SVD-PRE-DATE (I1)   TO BNWS-PRE-START-DT-CALC.
           MOVE WS-FALSE                  TO BNWS-MAX-CALC-SW.

140300 4222-END.

182400******************************************************************
       4224-DELETE-BNCOMMENTS.
182400******************************************************************

118100     MOVE BNBEN-COMPANY              TO DB-COMPANY.
118200     MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
118300     MOVE BNBEN-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
P26300*    INITIALIZE DB-PLAN-CODE
P26300*               DB-START-DATE.
P26300     MOVE BENSET4-PLAN-TYPE          TO WS-DB-BEG-RNG.
P26300*    PERFORM 850-FIND-NLT-BENSET4.
P26300     PERFORM 850-FIND-BEGRNG-BENSET4.
118700     IF (BENEFIT-NOTFOUND)
P26300*    OR (BEN-COMPANY                 NOT = DB-COMPANY)
P26300*    OR (BEN-EMPLOYEE                NOT = DB-EMPLOYEE)
P26300*    OR (BEN-PLAN-TYPE               NOT = DB-PLAN-TYPE)
               MOVE BNBEN-COMPANY          TO DB-COMPANY
               MOVE "EM"                   TO DB-CMT-TYPE
               MOVE BNBEN-EMPLOYEE (I1)    TO DB-EMPLOYEE
               INITIALIZE DB-PARTICIPNT
               MOVE BNBEN-PLAN-TYPE (I1)   TO DB-PLAN-TYPE
               INITIALIZE DB-PLAN-CODE
                          DB-GROUP-NAME
                          DB-START-DATE
                          DB-SEQ-NBR
               PERFORM 850-MODIFY-NLT-BCMSET1
119200         PERFORM 
119400             UNTIL (BNCOMMENTS-NOTFOUND)
                   OR    (BCM-COMPANY      NOT = DB-COMPANY)
                   OR    (BCM-CMT-TYPE     NOT = DB-CMT-TYPE)
                   OR    (BCM-EMPLOYEE     NOT = DB-EMPLOYEE)
                   OR    (BCM-PARTICIPNT   NOT = DB-PARTICIPNT)
                   OR    (BCM-PLAN-TYPE    NOT = DB-PLAN-TYPE)

140900             PERFORM 830-DELETE-BNCOMMENTS
192800             PERFORM 860-MODIFY-NEXT-BCMSET1
               END-PERFORM.
119500
       4224-END.

182400******************************************************************
       4226-DELETE-STANDTIME.
182400******************************************************************

           IF  (BENEFIT-FOUND)
           AND (BEN-STM-SEQ-NBR            NOT = ZEROES)
               MOVE BNBEN-COMPANY          TO DB-COMPANY
               MOVE BNBEN-EMPLOYEE (I1)    TO DB-EMPLOYEE
               INITIALIZE DB-TIME-GROUP
               MOVE BEN-STM-SEQ-NBR        TO DB-SEQ-NBR
               PERFORM 840-MODIFY-STMSET2
               IF (STANDTIME-FOUND)
                   PERFORM 830-DELETE-STANDTIME.

       4226-END.

182400******************************************************************
       4228-DELETE-EMPINVEST.
182400******************************************************************

           MOVE BNBEN-COMPANY              TO DB-COMPANY.
           MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
           MOVE BNBEN-PLAN-CODE (I1)       TO DB-PLAN-CODE.
           MOVE BNBEN-START-DATE (I1)      TO DB-START-DATE.
159700     INITIALIZE DB-SEQ-NBR
159800                DB-DIST-ST-DATE.
159900     PERFORM 850-MODIFY-NLT-EMISET1.
160000     PERFORM 
160200         UNTIL (EMPINVEST-NOTFOUND)
160300         OR    (EMI-COMPANY          NOT = DB-COMPANY)
160400         OR    (EMI-EMPLOYEE         NOT = DB-EMPLOYEE)
160500         OR    (EMI-PLAN-CODE        NOT = DB-PLAN-CODE)
160600         OR    (EMI-START-DATE       NOT = DB-START-DATE)

165700         PERFORM 830-DELETE-EMPINVEST
165800         PERFORM 860-MODIFY-NEXT-EMISET1
           END-PERFORM.

       4228-END.

      ******************************************************************
       4230-DELETE-HRDEPBEN.
      ******************************************************************

           MOVE BNBEN-COMPANY              TO DB-COMPANY.
           MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
           MOVE BNBEN-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
           MOVE BNBEN-PLAN-CODE (I1)       TO DB-PLAN-CODE.
           MOVE BNBEN-START-DATE (I1)      TO DB-EMP-START.
           MOVE HDBSET3-EMP-START          TO WS-DB-BEG-RNG.
           PERFORM 850-MODIFY-BEGRNG-HDBSET3.
           PERFORM
               UNTIL (HRDEPBEN-NOTFOUND)

               PERFORM 830-DELETE-HRDEPBEN
ACS002         SET HRDEPBEN-DELETED        TO TRUE

               PERFORM 860-MODIFY-NXTRNG-HDBSET3
           END-PERFORM.

       4230-END.

135600******************************************************************
       4240-CREATE-BNTAEMPBAL-RECORD.
135600******************************************************************

           PERFORM 800-CREATE-BNTAEMPBAL.

           PERFORM 4242-MOVE-TO-BTE
           THRU    4242-END.

           PERFORM 820-STORE-BNTAEMPBAL.

       4240-END.

135600******************************************************************
       4242-MOVE-TO-BTE.
135600******************************************************************

           MOVE BEN-COMPANY                TO BTE-COMPANY.
           MOVE BEN-PLAN-CODE              TO BTE-PLAN-CODE.
           MOVE BEN-EMPLOYEE               TO BTE-EMPLOYEE.
           MOVE TEM-PLAN                   TO BTE-TA-PLAN.
           MOVE TEM-EMPLOYEE-GROUP         TO BTE-EMPLOYEE-GROUP.
           MOVE TEM-POSITION               TO BTE-POSITION.
           MOVE BEN-START-DATE             TO BTE-START-DATE.
           MOVE BEN-STOP-DATE              TO BTE-STOP-DATE.
           MOVE BEN-TA-OBJ-ID              TO BTE-TA-OBJ-ID.
           MOVE BEN-NBR-HOURS              TO BTE-TOTAL-HOURS.
           MOVE 100                        TO BTE-PERCENT.
           INITIALIZE BTE-STATUS
                      BTE-UPDATED-HOURS
                      BTE-UPD-THRU-DATE.
           MOVE WS-SYSTEM-DATE-YMD         TO BTE-DATE-STAMP.
           MOVE HHMMSS                     TO BTE-TIME-STAMP.
           MOVE CRT-USER-NAME              TO BTE-USER-ID.

       4242-END.

135600******************************************************************
       4244-CHANGE-BNTAEMPBAL-RECORD.
135600******************************************************************

           MOVE BNBEN-COMPANY              TO DB-COMPANY.
           MOVE BNBEN-PLAN-CODE (I1)       TO DB-PLAN-CODE.
           MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
           MOVE BNBEN-START-DATE  (I1)     TO DB-START-DATE.
           MOVE BTESET3-START-DATE         TO WS-DB-BEG-RNG.
           PERFORM 850-MODIFY-BEGRNG-BTESET3.
           PERFORM
               UNTIL (BNTAEMPBAL-NOTFOUND)

               IF (BNBEN-LINE-FC (I1)      = "C")
                   MOVE BNBEN-MULT-SALARY (I1) TO BTE-TOTAL-HOURS
               ELSE
               IF (BNBEN-LINE-FC (I1)      = "S")
                   MOVE BNBEN-STOP-DATE (I1)   TO BTE-STOP-DATE
               END-IF
               END-IF

               MOVE 2                     TO BTE-STATUS
               MOVE WS-SYSTEM-DATE-YMD    TO BTE-DATE-STAMP
               MOVE HHMMSS                TO BTE-TIME-STAMP
               MOVE CRT-USER-NAME         TO BTE-USER-ID
               PERFORM 820-STORE-BNTAEMPBAL

               PERFORM 860-MODIFY-NXTRNG-BTESET3
           END-PERFORM.

       4244-END.

135600******************************************************************
       4246-CHANGE-BTE-FOR-BN531.
135600******************************************************************

           MOVE BNBEN-COMPANY             TO DB-COMPANY.
           MOVE BNBEN-PLAN-CODE (I1)      TO DB-PLAN-CODE.
           MOVE BNBEN-EMPLOYEE (I1)       TO DB-EMPLOYEE.
           MOVE BNBEN-START-DATE (I1)     TO DB-START-DATE.
           MOVE BNBEN-BTE-EMP-GRP (I1)    TO DB-EMPLOYEE-GROUP.
           MOVE BNBEN-BTE-POSITION (I1)   TO DB-POSITION.
           PERFORM 840-MODIFY-BTESET1.
           IF (BNTAEMPBAL-FOUND)
               MOVE BNBEN-MULT-SALARY(I1)   TO BTE-TOTAL-HOURS
               MOVE BNBEN-BTE-ALLOC-PCT(I1) TO BTE-PERCENT

               MOVE 2                     TO BTE-STATUS
               MOVE WS-SYSTEM-DATE-YMD    TO BTE-DATE-STAMP
               MOVE HHMMSS                TO BTE-TIME-STAMP
               MOVE CRT-USER-NAME         TO BTE-USER-ID

               PERFORM 820-STORE-BNTAEMPBAL.

       4246-END.

FAK   ******************************************************************
135500 4260-STOP-EXISTING-BENEFIT.
135600******************************************************************
135700
135800     MOVE BNBEN-START-DATE (I1)      TO WSDR-FR-DATE.
135900     PERFORM 900-DATE-TO-JULIAN.
136000     SUBTRACT 1                      FROM WSDR-JULIAN-DAYS.
136100     PERFORM 900-JULIAN-TO-DATE.    
J17305     IF  (WSDR-FR-DATE            NOT = BEN-STOP-DATE)
J17305         MOVE WS-TRUE                TO BNBEN-CHG-BEN-SW
J17305     END-IF.
136200     MOVE WSDR-FR-DATE               TO BEN-STOP-DATE.
P86489     IF (PLN-FLEX-PLAN              NOT = SPACES)
P86489     OR (PLN-COVERAGE-TYPE          = "0")
J17305         IF  (BEN-DED-STOP-DATE   NOT = ZEROES)
J17305             MOVE WS-TRUE            TO BNBEN-CHG-BEN-SW
J17305         END-IF
P86489         MOVE ZEROES                 TO BEN-DED-STOP-DATE
P86489     ELSE
J17305         IF  (WSDR-FR-DATE        NOT = BEN-DED-STOP-DATE)
J17305             MOVE WS-TRUE            TO BNBEN-CHG-BEN-SW
J17305         END-IF
P86489         MOVE WSDR-FR-DATE           TO BEN-DED-STOP-DATE
P86489     END-IF.
136300
J17305     IF (BNBEN-CHANGING-BEN)
               MOVE WS-SYSTEM-DATE-YMD         TO BEN-UPD-DATE 
P44822         MOVE HHMMSS                     TO BEN-TIME-STAMP 
J67329         MOVE CRT-USER-NAME              TO BEN-USER-ID 
J17305     END-IF.
J94418     IF (CRT-PROGRAM-CODE      = "BN100")
J94418     OR (CRT-PROGRAM-CODE      = "BN101")
J94418     OR (CRT-PROGRAM-CODE      = "BN102")
J94418     OR (CRT-PROGRAM-CODE      = "BN103")
J94418     OR (CRT-PROGRAM-CODE      = "BN104")
J94418     OR (CRT-PROGRAM-CODE      = "BN105")
J94418         MOVE CRT-PROGRAM-CODE       TO BEN-USER-ID
J94418     ELSE
J94418     IF (BNBEN-USER-ID (I1)    = SPACES)
J94418         MOVE CRT-USER-NAME          TO BEN-USER-ID
J94418     ELSE
J94418         MOVE BNBEN-USER-ID (I1)     TO BEN-USER-ID
J94418     END-IF.
J94418
136400     PERFORM 820-STORE-BENEFIT.
AI0095******************************************************************
AI0095****   ANALYSTS INTERNATIONAL   RDAHL    3/24/03             *****
AI0095****   ENHANCEMENT FOR MOD 9.5 RETROACTIVE BILLING           *****
AI0095******************************************************************
RAY   *--- THE RETRO ROUTINE WILL BE PERFORMED FOR BOTH THE STOP OLD
RAY   *--- AND THE ADD NEW. THE STOP OLD WILL CREATE A TRIGGER, THE
RAY   *--- ADD WILL NOT BECAUSE THE DATE WILL BE THE SAME
RAY
RAY        MOVE "S"                        TO BNBEN-LINE-FC (I1).
RAY        PERFORM 4400-RETRO-BILL-TRIGGER.
RAY        MOVE "A"                        TO BNBEN-LINE-FC (I1).
RAY   *----------------------------------------------------------------
      *
           IF (BNBEN-PLAN-TYPE (I1) = "HL" OR "DN" OR "EL")
           OR (BNBEN-PLAN-TYPE (I1) = "DL")
               PERFORM 4270-STOP-EXISTING-HRDEPBEN
               THRU    4270-END.
      *
           IF (BNBEN-PLAN-TYPE (I1) = "DC")
               PERFORM 4300-CHANGE-EMPINVEST
               THRU    4300-END.
      *
           IF (BNWS-POSITIVE-PREMIUM)
           OR (BNWS-NEGATIVE-PREMIUM)
128600         IF (BNBEN-FLEX-FLAG (I1)    = "Y")
      *            MOVE BNWS-EFD-PLAN-START-DT TO DB-PLAN-START-DT
                   MOVE BNWS-EFD-START-DATE    TO DB-START-DATE
                   MOVE BNBEN-EFD-START-DT (I1)   TO DB-START-DATE
                   MOVE BNBEN-EFD-GROUP-NM (I1)   TO DB-GROUP-NAME
                   PERFORM 840-FIND-EFDSET1
                   MOVE EFD-FLD-START-DATE     TO DB-START-DATE
                   MOVE EFD-FLD-GROUP-NAME     TO DB-GROUP-NAME
                   PERFORM 840-FIND-FLDSET1
                   MOVE BEN-START-DATE         TO DB-START-DATE
                   PERFORM 850-MODIFY-NLT-EFRSET2
                   PERFORM 4265-STOP-EXISTING-EFR 
                   THRU    4265-END.
136500*
136600**** THIS ROUTINE WILL ONLY BE PERFORMED FOR NON-FLEX BENEFITS,
136700**** BECAUSE FLEXIBLE BENEFITS ALWAYS HAVE STOP DATES.  THUS,
136800**** 600-STOP-EXISTING-EMDEDREC IS PERFORMED TO STOP THE EMDEDMASTR.
136900*
137000     MOVE BEN-PLAN-CODE              TO DB-PLAN-CODE.
137100     PERFORM 840-FIND-PLNSET1.
137200*
137250     MOVE BEN-STOP-DATE              TO BNWS-SCR-STOP-DATE.
137300     PERFORM 600-STOP-EXISTING-EMDEDREC.
137400*
P68420     IF  (BNBEN-CREATE-TRANS (I1)       = "Y")
P68420     AND (CRT-PROGRAM-CODE              = "BN32")
P68420         MOVE BNBEN-COMPANY              TO BNSNWS-COMPANY
P68420         MOVE BNBEN-PLAN-TYPE (I1)       TO BNSNWS-PLAN-TYPE
P68420         MOVE BNBEN-PLAN-CODE (I1)       TO BNSNWS-PLAN-CODE
P68420         MOVE BNBEN-EMPLOYEE (I1)        TO BNSNWS-EMPLOYEE
P68420         INITIALIZE BNSNWS-PARTICIPNT
P68420                    BNSNWS-DEPENDENT
P68420         MOVE BEN-START-DATE             TO BNSNWS-START-DATE
P68420         MOVE BEN-STOP-DATE              TO BNSNWS-EFFECT-DATE
P68420         PERFORM 5000-GET-BNT-TRAN-SEQ-NBR
P68420         MOVE BNSNWS-TRAN-SEQ-NBR        TO BNBEN-TRAN-SEQ-NBR
P68420       
P68420         PERFORM 800-CREATE-BNTRANS
P68420         MOVE BNBEN-COMPANY              TO BNT-COMPANY
P68420         MOVE BNBEN-PLAN-TYPE (I1)       TO BNT-PLAN-TYPE
P68420         MOVE BNBEN-PLAN-CODE (I1)       TO BNT-PLAN-CODE
P68420         MOVE BNBEN-EMPLOYEE (I1)        TO BNT-EMPLOYEE
P68420
P68420         INITIALIZE BNT-PARTICIPNT
P68420                    BNT-DEPENDENT
P68420       
P68420         MOVE BEN-START-DATE             TO BNT-START-DATE
P68420         MOVE BNBEN-TRAN-SEQ-NBR         TO BNT-TRAN-SEQ-NBR
P68420       
P68420         INITIALIZE BNT-EMP-START
P68420                    BNT-EVENT-CODE
P68420
P68420         MOVE "E"                        TO BNT-COVER-TYPE
P68420         MOVE 1                          TO BNT-TRAN-STATUS
P68420         MOVE "S"                        TO BNT-TRAN-ACTION
P68420         MOVE BNBEN-REASON (I1)          TO BNT-TRAN-REASON
P68420         MOVE BNBEN-MEMBER-ID (I1)       TO BNT-MEMBER-ID
P68420         MOVE BEN-STOP-DATE              TO BNT-EFFECT-DATE
P68420         MOVE BEN-STOP-DATE              TO BNT-STOP-DATE
P68420       
P68420         INITIALIZE BNT-HIPAA-FILE-NBR
P68420                    BNT-FILE-DATE
P68420       
P68420         MOVE WS-SYSTEM-DATE-YMD         TO BNT-DATE-STAMP
P68420         MOVE HHMMSS                     TO BNT-TIME-STAMP
P68420
P68420         IF (BNBEN-USER-ID (I1)          = SPACES)
P68420             MOVE CRT-USER-NAME          TO BNT-USER-ID
P68420         ELSE
P68420             MOVE BNBEN-USER-ID (I1)     TO BNT-USER-ID
P68420         END-IF
P68420       
P68420         PERFORM 820-STORE-BNTRANS.
P68420
137500 4260-END.

      ******************************************************************
       4265-STOP-EXISTING-EFR.
      ******************************************************************

           MOVE EFR-STOP-DATE              TO BNWS-SAVE-STOP-DATE.

135800     MOVE BNBEN-START-DATE (I1)      TO WSDR-FR-DATE.
135900     PERFORM 900-DATE-TO-JULIAN.
136000     SUBTRACT 1                      FROM WSDR-JULIAN-DAYS.
136100     PERFORM 900-JULIAN-TO-DATE.    
136200     MOVE WSDR-FR-DATE               TO EFR-STOP-DATE.

           PERFORM 820-STORE-EMPFLEXREM.
           MOVE "C"                        TO BNWS-LINE-FC.
           PERFORM 690-UPDATE-STANDTIME.
           MOVE BNBEN-LINE-FC (I1)         TO BNWS-LINE-FC.
           PERFORM 810-RECREATE-EMPFLEXREM.
           MOVE BNBEN-START-DATE (I1)      TO EFR-START-DATE.
           MOVE BNWS-SAVE-STOP-DATE        TO EFR-STOP-DATE.

       4265-END.
FAK
      ******************************************************************
       4270-STOP-EXISTING-HRDEPBEN.
      ******************************************************************

           MOVE BNBEN-COMPANY          TO HRHDB-COMPANY.
           MOVE BNBEN-EMPLOYEE (I1)    TO HRHDB-EMPLOYEE.
           MOVE BNBEN-PLAN-TYPE (I1)   TO HRHDB-PLAN-TYPE.
           MOVE BNBEN-PLAN-CODE (I1)   TO HRHDB-PLAN-CODE.
           MOVE BEN-START-DATE         TO HRHDB-BEN-START-DATE
                                          HRHDB-EMP-START.
           MOVE BEN-STOP-DATE          TO HRHDB-BEN-STOP-DATE.
           INITIALIZE HRHDB-DEPENDENT
                      HRHDB-START-DATE.
           MOVE "S"                    TO HRHDB-FC.
           MOVE "E"                    TO HRHDB-COVER-TYPE.
           MOVE BNBEN-BATCH-FC (I1)    TO HRHDBWS-BATCH-FC.
P68420     MOVE BNBEN-REASON (I1)      TO HRHDB-HIPAA-REASON.
J90068     MOVE BNBEN-CREATE-TRANS (I1)    
J90068                                 TO HRHDB-CREATE-TRANS.

           PERFORM 3000-HRHDB-PROCESS-TRAN.

           IF  (CRT-PROGRAM-CODE = "BN105" OR "BN102" OR "BN100")
           AND (HRHDB-SV-SP-END-DT (1)  NOT = ZEROES)
               INITIALIZE BNBEN-SV-SP-TBL (I1)
               MOVE HRHDB-SV-SP-TBL    TO BNBEN-SV-SP-TBL (I1)
               INITIALIZE HRHDB-SV-SP-TBL
               INITIALIZE I4.
    
       4270-END.
    
182400******************************************************************
       4280-CREATE-EMPINVEST.
182400******************************************************************

           MOVE BEN-COMPANY            TO DB-COMPANY.
           MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
           INITIALIZE DB-SEQ-NBR.
           PERFORM 850-KFIND-NLT-BIVSET2.
           IF  (BNINVEST-KFOUND)
           AND (BIV-COMPANY            = DB-COMPANY)
           AND (BIV-PLAN-CODE          = DB-PLAN-CODE)
               PERFORM 800-CREATE-EMPINVEST

               PERFORM 4282-MOVE-TO-EMPINVEST
               THRU    4282-END

               PERFORM 820-STORE-EMPINVEST.

       4280-END.

011700******************************************************************
       4282-MOVE-TO-EMPINVEST.
011700******************************************************************

           MOVE BEN-COMPANY            TO EMI-COMPANY.
           MOVE BEN-EMPLOYEE           TO EMI-EMPLOYEE.
           MOVE BEN-PLAN-CODE          TO EMI-PLAN-CODE.
           MOVE BEN-START-DATE         TO EMI-START-DATE.

           MOVE BIV-SEQ-NBR            TO EMI-SEQ-NBR.

           MOVE BEN-START-DATE         TO EMI-DIST-ST-DATE.
           MOVE BEN-STOP-DATE          TO EMI-STOP-DATE.

           MOVE 100                    TO EMI-DIST-PCT.

       4282-END.

182400******************************************************************
       4300-CHANGE-EMPINVEST.
182400******************************************************************

      **** IF COMING HERE WHEN LINE-FC = "A" MEANING WE ARE STOPPING
      **** ALL EXISTING BENEFITS, WE NEED TO USE BEN-START AND STOP-DATE
      **** BECAUSE THAT IS THE BENEFIT THAT IS EFFECTING EMPINVEST RECORDS

           MOVE BNBEN-COMPANY              TO DB-COMPANY.
           MOVE BNBEN-EMPLOYEE (I1)        TO DB-EMPLOYEE.
           MOVE BNBEN-PLAN-CODE (I1)       TO DB-PLAN-CODE.
           IF (BNBEN-LINE-FC (I1)          = "A")
               MOVE BEN-START-DATE         TO DB-START-DATE
           ELSE
               MOVE BNBEN-START-DATE (I1)  TO DB-START-DATE.
161100     INITIALIZE DB-SEQ-NBR
161200                DB-DIST-ST-DATE.
161300     PERFORM 850-MODIFY-NLT-EMISET1.
161400     PERFORM
161600         UNTIL (EMPINVEST-NOTFOUND)
161700         OR    (EMI-COMPANY          NOT = DB-COMPANY)
161800         OR    (EMI-EMPLOYEE         NOT = DB-EMPLOYEE)
161900         OR    (EMI-PLAN-CODE        NOT = DB-PLAN-CODE)
162000         OR    (EMI-START-DATE       NOT = DB-START-DATE)

               IF (BNBEN-LINE-FC (I1)      = "A")
166600             IF (EMI-STOP-DATE       = ZEROES)
166700             OR (EMI-STOP-DATE       > BEN-STOP-DATE)
166800                 MOVE BEN-STOP-DATE  TO EMI-STOP-DATE
166900                 PERFORM 820-STORE-EMPINVEST
                   END-IF
               ELSE
166600         IF (EMI-STOP-DATE           = ZEROES)
166700         OR (EMI-STOP-DATE           > BNBEN-STOP-DATE (I1))
166800             MOVE BNBEN-STOP-DATE (I1)
                                           TO EMI-STOP-DATE
                   MOVE WS-SYSTEM-DATE-YMD TO EMI-UPD-DATE
                   MOVE HHMMSS             TO EMI-TIME-STAMP
                   MOVE CRT-USER-NAME      TO EMI-USER-ID

166900             PERFORM 820-STORE-EMPINVEST
               END-IF
               END-IF
167100         PERFORM 860-MODIFY-NEXT-EMISET1
           END-PERFORM.

       4300-END.
      ******************************************************************
       4400-RETRO-BILL-TRIGGER.
      ******************************************************************
AI0095******************************************************************
AI0095****   ANALYSTS INTERNATIONAL   RDAHL    3/24/03             *****
AI0095****   ENHANCEMENT FOR MOD 9.5 RETROACTIVE BILLING           *****
RAY   ****   TRIGGER REWRITE - HERWECK 5/5/04                      *****
AI0095******************************************************************
           MOVE BEN-COMPANY                TO DB-COMPANY.
           MOVE BEN-PLAN-TYPE              TO DB-PLAN-TYPE.
           MOVE BEN-PLAN-CODE              TO DB-PLAN-CODE.
           PERFORM 840-FIND-PLNSET1.

AI0095     IF  (BNBEN-LINE-FC (I1) = "A" OR "C" OR "S")
SDB114     AND (PLN-PLAN-TYPE = "DC")
SDB114            NEXT SENTENCE
SDB114     ELSE   
AI0095     IF (BNBEN-LINE-FC (I1) = "A" OR "C" OR "S")
RAY********AND (PLN-CONTRIB-TYPE > ZEROES)
SDB
********AND (PLN-PLAN-CODE(1:1) NOT = "Y")
AI0095         MOVE BEN-COMPANY            TO DB-COMPANY
AI0095         MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE
AI0095         MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE
AI0095         MOVE BEN-EMPLOYEE           TO DB-EMPLOYEE
RAY            MOVE ZEROES                 TO DB-PARTICIPNT
GW             MOVE BEN-START-DATE         TO DB-START-DATE
RAY            PERFORM 840-FIND-WBPSET1
RAY            IF (WBPBENEFIT-NOTFOUND)
RAY                MOVE ZEROES             TO WS-BNBEN-START-DATE
RAY                MOVE ZEROES             TO WS-BNBEN-STOP-DATE
RAY            ELSE
RAY                MOVE WBP-RETRO-START-DT TO WS-BNBEN-START-DATE
RAY            END-IF
RAY            PERFORM 4420-GET-LAST-BILLED-DATE
AI0095         IF  (BNBEN-PROCESSED-DATE = ZERO)
AI0095         OR  (BEN-START-DATE <= BNBEN-PROCESSED-DATE)
RAY                PERFORM 4460-WBP-RETRO-START-DATE
RAY                PERFORM 4490-WRITE-RETRO-BILL-TRIGGER
RAY                THRU    4490-END
RAY            END-IF
RAY        END-IF. 
RAY
RAY        IF  (BNBEN-LINE-FC (I1) = "D")
SDB114     AND (PLN-PLAN-TYPE = "DC")
SDB114          NEXT SENTENCE
           ELSE
RAY        IF  (BNBEN-LINE-FC (I1) = "D")
RAY********AND (PLN-CONTRIB-TYPE > ZEROES)
SDB******* AND (PLN-PLAN-CODE(1:1) NOT = "Y")
RAY            PERFORM 4420-GET-LAST-BILLED-DATE
RAY            MOVE BEN-COMPANY            TO DB-COMPANY
RAY            MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE
RAY            MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE
RAY            MOVE BEN-EMPLOYEE           TO DB-EMPLOYEE
RAY            MOVE ZEROES                 TO DB-PARTICIPNT
RAY            MOVE BEN-START-DATE         TO DB-START-DATE
RAY            PERFORM 840-FIND-WBPSET1
RAY            IF  (BNBEN-PROCESSED-DATE = ZERO)
RAY            OR  (BEN-START-DATE <= BNBEN-PROCESSED-DATE)
RAY                MOVE BEN-START-DATE     TO WS-BNBEN-START-DATE
RAY                PERFORM 4490-WRITE-RETRO-BILL-TRIGGER
RAY                THRU    4490-END
RAY            END-IF
RAY        END-IF.
RAY
RAY   ******************************************************************
RAY    4420-GET-LAST-BILLED-DATE.
RAY   ******************************************************************
RAY
******** GW 2/2005 WE ARE NOT LOOKING FOR THE EE'S LAST BILLING DATE.
********  WE ARE LOOKING FOR ER'S LAST BILLING DATE
*********  WBP TRIGGER RECS AREN'T GETTING WRITTEN ON REHIRES
AI0095     MOVE ZERO                       TO BNBEN-PROCESSED-DATE.
AI0095*GW  MOVE BEN-COMPANY                TO DB-COMPANY.
AI0095*GW  MOVE BEN-EMPLOYEE               TO DB-EMPLOYEE.
RAY   *GW  MOVE BEN-PLAN-TYPE              TO DB-PLAN-TYPE.
RAY   *GW  MOVE BEN-PLAN-CODE              TO DB-PLAN-CODE.
AI0095*GW  MOVE PBHSETW1-PLAN-CODE          TO WS-DB-BEG-RNG.
AI0095*GW  PERFORM 850-FIND-BEGRNG-PBHSETW1.
AI0095*GW  PERFORM
AI0095*GW  UNTIL (PBHSTDTL-NOTFOUND)
AI0095*GW      MOVE PBH-COMPANY            TO DB-COMPANY
AI0095*GW      MOVE PBH-PROCESS-LEVEL      TO DB-PROCESS-LEVEL
AI0095*GW      MOVE PBH-INVOICE            TO DB-INVOICE
AI0095*GW      PERFORM 840-FIND-PBKSET1
AI0095*GW      IF  (PBHSTHDR-FOUND)
AI0095*GW      AND (PBK-PER-END-DATE > BNBEN-PROCESSED-DATE)
AI0095*GW          MOVE PBK-PER-END-DATE   TO BNBEN-PROCESSED-DATE
AI0095*GW      END-IF
AI0095*GW      PERFORM 860-FIND-NXTRNG-PBHSETW1
AI0095*GW  END-PERFORM.

GW2/05*NO  MOVE BEN-START-DATE             TO BNBEN-PROCESSED-DATE.
GW2/05     MOVE EMP-COMPANY                TO DB-COMPANY.
GW2/05     MOVE EMP-PROCESS-LEVEL          TO DB-PROCESS-LEVEL.
GW2/05     MOVE BEN-START-DATE             TO DB-INVOICE-DATE.
GW2/05     MOVE SPACES                     TO DB-INVOICE.
GW2/05     PERFORM 850-FIND-NLT-ZB4SET5.
GW2/05     PERFORM
GW2/05      UNTIL (PBHSTHDR-NOTFOUND)
GW2/05         OR (ZB4-PROCESS-LEVEL NOT = EMP-PROCESS-LEVEL)
GW2/05          IF (ZB4-PER-END-DATE > BNBEN-PROCESSED-DATE)
GW2/05             MOVE ZB4-PER-END-DATE   TO BNBEN-PROCESSED-DATE
GW2/05          END-IF
GW2/05          PERFORM 860-FIND-NEXT-ZB4SET5.

RAY 
RAY   *    MOVE EMP-COMPANY                TO DB-COMPANY.
RAY   *    MOVE EMP-PROCESS-LEVEL          TO DB-PROCESS-LEVEL.
RAY   *    MOVE BEN-PLAN-TYPE              TO DB-PLAN-TYPE.
RAY   *    MOVE BEN-PLAN-CODE              TO DB-PLAN-CODE.
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
RAY        MOVE BEN-START-DATE         TO WS-BNBEN-START-DATE.
RAY   *    -------------------------------------------------------------
RAY   *    | IF THIS IS A NEW STOP DATE, CHANGE RETRO START DATE       |
RAY   *    | TO 1 DAY AFTER THE NEW STOP DATE.                         |
RAY   *    | NO NEED TO RECALC MONTHS THAT WERE BILLED CORRECTLY.      |
RAY   *    -------------------------------------------------------------
RAY        IF (BNBEN-LINE-FC (I1) NOT = "A")
GW         AND ((WS-BNBEN-STOP-DATE  = ZERO)
GW         OR  (WS-BNBEN-STOP-DATE  > BEN-STOP-DATE))
RAY        AND (BEN-STOP-DATE > ZERO)
GW             MOVE BEN-STOP-DATE      TO WSDR-FR-DATE
GW             PERFORM 900-DATE-TO-JULIAN
GW             ADD 1 TO WSDR-JULIAN-DAYS
GW             PERFORM 900-JULIAN-TO-DATE
RAY   *---     DON'T REPLACE AN EARLIER RETRO START DATE
RAY            IF (WSDR-FR-DATE < WS-BNBEN-START-DATE)
RAY            OR (WS-BNBEN-STOP-DATE = ZERO)
GW                 MOVE WSDR-FR-DATE   TO WS-BNBEN-START-DATE.
RAY
RAY   ******************************************************************
RAY    4490-WRITE-RETRO-BILL-TRIGGER.
RAY   ******************************************************************
RAY
RAY   *--- SKIP WRITE IF THE RETRO START DATE ALREADY EXISTS FOR THE 
RAY   *    SAME PLAN TYPE/PLAN CODE WITH A DIFFERENT START DATE
RAY        MOVE BEN-COMPANY            TO DB-COMPANY.
RAY        MOVE ZEROES                 TO DB-STATUS.
RAY        MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE.
RAY        MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
RAY        MOVE BEN-EMPLOYEE           TO DB-EMPLOYEE.
RAY        INITIALIZE                     DB-PARTICIPNT.
RAY        INITIALIZE                     DB-START-DATE.
RAY        MOVE WBPSET4-EMPLOYEE       TO WS-DB-BEG-RNG.
RAY        PERFORM 850-FIND-BEGRNG-WBPSET4.
RAY        PERFORM 
RAY        UNTIL (WBPBENEFIT-NOTFOUND)
RAY            IF (WBP-RETRO-START-DT <= WS-BNBEN-START-DATE)
RAY                GO TO 4490-END
RAY            END-IF
RAY            PERFORM 860-FIND-NXTRNG-WBPSET4
RAY        END-PERFORM.
RAY
RAY   *--- SKIP WRITE IF FUTURE DATED RETRO
RAY   *    IF (PBBNPLAN-FOUND)
RAY   *    AND (WS-BNBEN-START-DATE > PBL-LAST-BILLED-DT)
RAY   *        GO TO 4490-END.
RAY
RAY        IF  ((BEN-STOP-DATE        > ZERO)
RAY        AND  (BNBEN-PROCESSED-DATE > ZERO)
RAY        AND  (BEN-STOP-DATE        > BNBEN-PROCESSED-DATE))
RAY        OR  ((BNBEN-PROCESSED-DATE > ZEROES)
RAY        AND (WS-BNBEN-START-DATE   >= BNBEN-PROCESSED-DATE))
RAY            GO TO 4490-END.
RAY
           STRING  BEN-PLAN-TYPE       DELIMITED BY SIZE
                   BEN-PLAN-CODE       DELIMITED BY SIZE
                                       INTO DB-DTL-KEY.
           MOVE "TRIGGER"             TO DB-LVL-1-KEY.
           PERFORM 840-FIND-IF1SET1.
           IF (LUPTBLDTL-NOTFOUND)
             GO TO 4490-END
           END-IF.  

RAY        MOVE BEN-COMPANY            TO DB-COMPANY.
RAY        MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE.
RAY        MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
RAY        MOVE BEN-EMPLOYEE           TO DB-EMPLOYEE.
RAY        INITIALIZE                     DB-PARTICIPNT.
GW****** USERS HAVE BEEN GETTING DUP RECORD FAILURES *************  
GW******   DUE TO DELETING AN EARLIER DATE THEN TRYING TO WRITE **
GW******   DUP REC WITH BEN-START-DATE ***************************
RAY        INITIALIZE                     DB-START-DATE.
RAY        MOVE WBPSET1-EMPLOYEE       TO WS-DB-BEG-RNG.
RAY        PERFORM 850-MODIFY-BEGRNG-WBPSET1.

GW         PERFORM
GW         UNTIL (WBPBENEFIT-NOTFOUND)
RAY            PERFORM 830-DELETE-WBPBENEFIT
               PERFORM 860-MODIFY-NXTRNG-WBPSET1 
           END-PERFORM.

RAY        PERFORM 800-CREATE-WBPBENEFIT.
RAY        MOVE BEN-COMPANY            TO WBP-COMPANY.
RAY        MOVE BEN-PLAN-TYPE          TO WBP-PLAN-TYPE.
RAY        MOVE BEN-PLAN-CODE          TO WBP-PLAN-CODE.
RAY        MOVE BEN-EMPLOYEE           TO WBP-EMPLOYEE.
RAY        MOVE ZEROES                 TO WBP-PARTICIPNT.
RAY        MOVE BEN-START-DATE         TO WBP-START-DATE.

RAY        PERFORM 4495-WBPBENEFIT-COMMON-FLDS.
RAY        MOVE WS-BNBEN-START-DATE    TO WBP-RETRO-START-DT.
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
RAY    4495-END.       

182400******************************************************************
182500 5100-MOVE-DETAIL-DATA.
182600******************************************************************
182700
J17305     IF   (BNBEN-PLAN-TYPE (I1)   NOT = BEN-PLAN-TYPE)
J17305     OR   (BNBEN-EMPLOYEE (I1)    NOT = BEN-EMPLOYEE)
J17305     OR   (BNBEN-START-DATE (I1)  NOT = BEN-START-DATE)
J17305     OR   (BNBEN-PLAN-CODE (I1)   NOT = BEN-PLAN-CODE)
J17305     OR   (BNBEN-STOP-DATE  (I1)  NOT = BEN-STOP-DATE)
J17305          MOVE WS-TRUE               TO BNBEN-CHG-BEN-SW
J17305     END-IF.

183400     MOVE BNBEN-COMPANY              TO BEN-COMPANY.

183500     MOVE BNBEN-PLAN-TYPE (I1)       TO BEN-PLAN-TYPE.
183600     MOVE BNBEN-EMPLOYEE (I1)        TO BEN-EMPLOYEE.
183700     MOVE BNBEN-START-DATE (I1)      TO BEN-START-DATE.
183800     MOVE BNBEN-PLAN-CODE (I1)       TO BEN-PLAN-CODE.

           MOVE PLN-FLEX-PLAN              TO BEN-FLEX-PLAN.

183900     MOVE BNBEN-STOP-DATE (I1)       TO BEN-STOP-DATE.

           IF (PLN-CONTRIB-TYPE            = "6" OR "7")
J17305         IF (BNBEN-MAX-MATCH (I1)   NOT = BEN-COMP-CONT)
J17305         OR (BNBEN-PCT-MATCHED (I1) NOT = BEN-MTCH-UP-TO)
J17305         OR (BNBEN-COMP-CONT (I1)   NOT = BEN-COMP-MATCH)
J17305             MOVE WS-TRUE            TO BNBEN-CHG-BEN-SW
J17305         END-IF
184400         MOVE BNBEN-MAX-MATCH (I1)   TO BEN-COMP-CONT
184400         MOVE BNBEN-PCT-MATCHED (I1) TO BEN-MTCH-UP-TO
184400         MOVE BNBEN-COMP-CONT (I1)   TO BEN-COMP-MATCH
           ELSE
J17305         IF (BNBEN-COMP-CONT (I1)   NOT = BEN-COMP-CONT)
J17305             MOVE WS-TRUE            TO BNBEN-CHG-BEN-SW
J17305         END-IF 
184400         MOVE BNBEN-COMP-CONT (I1)   TO BEN-COMP-CONT.

           IF (BNBEN-FLEX-FLAG (I1)        = "Y")
J17305         IF (BNWS-CMP-FLX-CONT    NOT = BEN-CMP-FLX-CONT)
J17305         OR (BNWS-EMP-PRE-CONT    NOT = BEN-EMP-PRE-CONT)
J17305             MOVE WS-TRUE            TO BNBEN-CHG-BEN-SW
J17305         END-IF
185100         MOVE BNWS-CMP-FLX-CONT      TO BEN-CMP-FLX-CONT
185200         MOVE BNWS-EMP-PRE-CONT      TO BEN-EMP-PRE-CONT
           ELSE
J17305         IF (BNBEN-CMP-FLX-CONT (I1) NOT = BEN-CMP-FLX-CONT)
J17305         OR (BNBEN-EMP-PRE-CONT (I1) NOT = BEN-EMP-PRE-CONT)
J17305             MOVE WS-TRUE               TO BNBEN-CHG-BEN-SW
J17305         END-IF
185100         MOVE BNBEN-CMP-FLX-CONT (I1)
185100                                     TO BEN-CMP-FLX-CONT
185200         MOVE BNBEN-EMP-PRE-CONT (I1)
185200                                     TO BEN-EMP-PRE-CONT.
J17305     IF (BNBEN-EMP-AFT-CONT (I1)  NOT = BEN-EMP-AFT-CONT)
J17305     OR (BNBEN-PREM-UPD-DT (I1)   NOT = BEN-PREM-UPD-DT)
J17305     OR (BNBEN-ELIG-UPD-DT (I1)   NOT = BEN-ELIG-UPD-DT)
J17305     OR (BNBEN-COV-UPD-DT (I1)    NOT = BEN-COV-UPD-DT)
J17305     OR (BNBEN-BNA-START-DATE (I1) NOT = BEN-GL-UPD-DT)
J17305         MOVE WS-TRUE            TO BNBEN-CHG-BEN-SW
J17305     END-IF.
184300     MOVE BNBEN-EMP-AFT-CONT (I1)    TO BEN-EMP-AFT-CONT.

184100     MOVE BNBEN-PREM-UPD-DT (I1)     TO BEN-PREM-UPD-DT.
184100     MOVE BNBEN-ELIG-UPD-DT (I1)     TO BEN-ELIG-UPD-DT.
184100     MOVE BNBEN-COV-UPD-DT (I1)      TO BEN-COV-UPD-DT.
184100     MOVE BNBEN-BNA-START-DATE (I1)  TO BEN-GL-UPD-DT.

           IF (PLN-CONTRIB-TYPE            = "5" OR "6" OR "7")
J17305         IF (BNBEN-COVER-AMT (I1) NOT = BEN-BOND-DED-AMT)
J17305             MOVE WS-TRUE            TO BNBEN-CHG-BEN-SW
J17305         END-IF
               MOVE BNBEN-COVER-AMT (I1)   TO BEN-BOND-DED-AMT
           ELSE
J17305         IF (BNBEN-COVER-AMT (I1) NOT = BEN-COVER-AMT)
J17305             MOVE WS-TRUE            TO BNBEN-CHG-BEN-SW
J17305         END-IF
               MOVE BNBEN-COVER-AMT (I1)   TO BEN-COVER-AMT.

J17305     IF (BNBEN-DEP-COVER-AMT (I1) NOT = BEN-DEP-COVER-AMT)
J17305     OR (BNBEN-PCT-AMT-FLAG (I1)  NOT = BEN-PCT-AMT-FLAG)
J17305     OR (BNBEN-PRE-AFT-FLAG (I1)  NOT = BEN-PRE-AFT-FLAG)
J17305         MOVE WS-TRUE                TO BNBEN-CHG-BEN-SW
J17305     END-IF.
           MOVE BNBEN-DEP-COVER-AMT (I1)   TO BEN-DEP-COVER-AMT.

           MOVE BNBEN-PCT-AMT-FLAG (I1)    TO BEN-PCT-AMT-FLAG.
           MOVE BNBEN-PRE-AFT-FLAG (I1)    TO BEN-PRE-AFT-FLAG.

           IF (PLN-PLAN-TYPE               = "VA")
J17305         IF (BNBEN-MULT-SALARY (I1) NOT = BEN-NBR-HOURS)
J17305             MOVE WS-TRUE            TO BNBEN-CHG-BEN-SW
J17305         END-IF
               MOVE BNBEN-MULT-SALARY (I1) TO BEN-NBR-HOURS
           ELSE
           IF (PLN-CONTRIB-TYPE            = "5" OR "6" OR "7")
J17305         IF (BNBEN-COVER-OPT (I1) NOT = BEN-CYCLES-REMAIN)
J17305             MOVE WS-TRUE            TO BNBEN-CHG-BEN-SW
J17305         END-IF
               MOVE BNBEN-COVER-OPT (I1)   TO BEN-CYCLES-REMAIN
           ELSE
J17305         IF (BNBEN-COVER-OPT (I1) NOT = BEN-COV-OPTION)
J17305             MOVE WS-TRUE            TO BNBEN-CHG-BEN-SW
J17305         END-IF
184000         MOVE BNBEN-COVER-OPT (I1)   TO BEN-COV-OPTION.

           IF (PLN-CONTRIB-TYPE            = "5" OR "6" OR "7")
J17305         IF (BNBEN-PAY-RATE  (I1) NOT = BEN-ANNUAL-AMT)
J17305             MOVE WS-TRUE            TO BNBEN-CHG-BEN-SW
J17305         END-IF
               MOVE BNBEN-PAY-RATE (I1)    TO BEN-ANNUAL-AMT
           ELSE
J17305         IF (BNBEN-PAY-RATE (I1)  NOT = BEN-PAY-RATE)
J17305             MOVE WS-TRUE            TO BNBEN-CHG-BEN-SW
J17305         END-IF
206600         MOVE BNBEN-PAY-RATE (I1)    TO BEN-PAY-RATE.

J17305     IF (BNBEN-YTD-CONT (I1)      NOT = BEN-YTD-CONT)
J17305     OR (BNBEN-EMP-YTD-CONT (I1)  NOT = BEN-EMP-YTD-CONT)
J17305         MOVE WS-TRUE                TO BNBEN-CHG-BEN-SW
J17305     END-IF.
           MOVE BNBEN-YTD-CONT (I1)        TO BEN-YTD-CONT.
P77247     MOVE BNBEN-EMP-YTD-CONT (I1)    TO BEN-EMP-YTD-CONT.

      *
      **** THE FOLLOWING SEQUENCE NUMBERS ARE DETERMINED AND FILLED BY
      **** THE PREDM ROUTINE: CMP-SEQ-NBR, CMP-AFT-SEQ, PRE-SEQ-NBR,
      **** AFT-SEQ-NBR, FLEX-DED-SEQ.
      *                                    TO BEN-CMP-SEQ-NBR.
      *                                    TO BEN-CMP-AFT-NBR.
      *                                    TO BEN-PRE-SEQ-NBR.
      *                                    TO BEN-AFT-SEQ-NBR.
      *                                    TO BEN-FLEX-DED-SEQ.
           MOVE BNBEN-STM-SEQ-NBR (I1)     TO BEN-STM-SEQ-NBR.
      *    MOVE                            TO BEN-AFT-LMT-SEQ.
      *    MOVE                            TO BEN-AFT-LMT-MT-SEQ.

J17305     IF (BNBEN-COV-OVER-FLG (I1)  NOT = BEN-COV-OVER-FLG)
J17305     OR (BNBEN-YTD-FLEX-CONT (I1) NOT = BEN-YTD-FLEX-CONT)
J17305     OR (BNBEN-ELIG-GROUP (I1)    NOT = BEN-ELIG-GROUP)
J17305     OR (BNBEN-COV-GROUP (I1)     NOT = BEN-COV-GROUP)
J17305     OR (BNBEN-PREM-GROUP (I1)    NOT = BEN-PREM-GROUP)
J17305     OR (BNBEN-BNA-GROUP-NAME (I1) NOT = BEN-GL-GROUP)
J17305     OR (BNBEN-MULT-SALARY (I1)   NOT = BEN-MULTIPLE)
J17305     OR (BNBEN-SMOKER-FLAG (I1)   NOT = BEN-SMOKER)
J17305     OR (BNBEN-PEND-EVIDENCE (I1) NOT = BEN-PEND-EVIDENCE)
J17305         MOVE WS-TRUE                TO BNBEN-CHG-BEN-SW
J17305     END-IF.
185700*    MOVE BNBEN-TA-UPD-FLAG (I1)     TO BEN-TA-UPD-FLAG.

208700     MOVE BNBEN-COV-OVER-FLG (I1)    TO BEN-COV-OVER-FLG.

      *    MOVE BNBEN-TAKEN-FLAG (I1)      TO BEN-TAKEN-FLAG.
206800
           MOVE BNBEN-YTD-FLEX-CONT (I1)   TO BEN-YTD-FLEX-CONT.

184100     MOVE BNBEN-ELIG-GROUP (I1)      TO BEN-ELIG-GROUP.
184100     MOVE BNBEN-COV-GROUP (I1)       TO BEN-COV-GROUP.
184100     MOVE BNBEN-PREM-GROUP (I1)      TO BEN-PREM-GROUP.
           MOVE BNBEN-BNA-GROUP-NAME (I1)  TO BEN-GL-GROUP.

           MOVE BNBEN-MULT-SALARY (I1)     TO BEN-MULTIPLE.

           MOVE BNBEN-SMOKER-FLAG (I1)     TO BEN-SMOKER.

           MOVE BNBEN-PEND-EVIDENCE (I1)   TO BEN-PEND-EVIDENCE.

J67329     IF (BNBEN-LINE-FC (I1) = "A")

087429         IF  (CRT-PROGRAM-CODE           = "BN531")
087429         AND (BNBEN-CREATION-DATE (I1)   NOT = ZEROES)
087429             MOVE BNBEN-UPD-DATE (I1)    TO BEN-CREATION-DATE
087429             MOVE BNBEN-TIME-STAMP (I1)  TO BEN-CREATE-TIME
087429         ELSE
J67329             MOVE WS-SYSTEM-DATE-YMD     TO BEN-CREATION-DATE
J67329             MOVE HHMMSS                 TO BEN-CREATE-TIME
087429         END-IF

087429*        IF  (CRT-PROGRAM-CODE           = "BN531")
087429*        AND (BNBEN-UPD-DATE (I1)        NOT = ZEROES)
087429*            MOVE BNBEN-TIME-STAMP (I1)  TO BEN-CREATE-TIME
087429*        ELSE
087429*            MOVE HHMMSS                 TO BEN-CREATE-TIME
087429*        END-IF

J67329         IF (CRT-PROGRAM-CODE            = "BN100")
J67329         OR (CRT-PROGRAM-CODE            = "BN101")
J67329         OR (CRT-PROGRAM-CODE            = "BN102")
J67329         OR (CRT-PROGRAM-CODE            = "BN103")
J67329         OR (CRT-PROGRAM-CODE            = "BN104")
J67329         OR (CRT-PROGRAM-CODE            = "BN105")
J67329             MOVE CRT-PROGRAM-CODE       TO BEN-CREATE-USER-ID
J67329         ELSE
J67329             IF (BNBEN-USER-ID (I1)          = SPACES)
J67329                 MOVE CRT-USER-NAME      TO BEN-CREATE-USER-ID
J67329             ELSE
J67329                 MOVE BNBEN-USER-ID (I1) TO BEN-CREATE-USER-ID
J67329             END-IF
J67329         END-IF
J67329     END-IF.

J67329     IF (BNBEN-LINE-FC (I1) = "A" OR "C")
J17305     AND (BNBEN-CHANGING-BEN)

087429         IF  (CRT-PROGRAM-CODE           = "BN531")
087429         AND (BNBEN-UPD-DATE (I1)        NOT = ZEROES)
087429             MOVE BNBEN-UPD-DATE (I1)    TO BEN-UPD-DATE
087429             MOVE BNBEN-TIME-STAMP (I1)  TO BEN-TIME-STAMP
087429         ELSE
                   MOVE WS-SYSTEM-DATE-YMD     TO BEN-UPD-DATE
087429             MOVE HHMMSS                 TO BEN-TIME-STAMP
087429         END-IF

087429*        IF  (CRT-PROGRAM-CODE           = "BN531")
087429*        AND (BNBEN-UPD-DATE (I1)        NOT = ZEROES)
087429*            MOVE BNBEN-TIME-STAMP (I1)  TO BEN-TIME-STAMP
087429*        ELSE
087429*            MOVE HHMMSS                 TO BEN-TIME-STAMP
087429*        END-IF

               IF (CRT-PROGRAM-CODE            = "BN100")
               OR (CRT-PROGRAM-CODE            = "BN101")
               OR (CRT-PROGRAM-CODE            = "BN102")
               OR (CRT-PROGRAM-CODE            = "BN103")
               OR (CRT-PROGRAM-CODE            = "BN104")
               OR (CRT-PROGRAM-CODE            = "BN105")
                   MOVE CRT-PROGRAM-CODE       TO BEN-USER-ID
               ELSE
                   IF (BNBEN-USER-ID (I1)          = SPACES)
                       MOVE CRT-USER-NAME      TO BEN-USER-ID
                   ELSE
                       MOVE BNBEN-USER-ID (I1) TO BEN-USER-ID
                   END-IF
               END-IF
J67329     END-IF.

           IF (BNBEN-FLEX-FLAG (I1)        = "Y")
               MOVE BNBEN-EFD-START-DT (I1)    TO BEN-EFD-START-DATE.

           MOVE BNBEN-COV-SAL-DATE (I1)    TO BEN-COV-SAL-DATE.
           MOVE BNBEN-COV-AGE-DATE (I1)    TO BEN-COV-AGE-DATE.
           MOVE BNBEN-CNT-SAL-DATE (I1)    TO BEN-CNT-SAL-DATE.
           MOVE BNBEN-CNT-AGE-DATE (I1)    TO BEN-CNT-AGE-DATE.
           MOVE BNBEN-CNT-SERV-DATE (I1)   TO BEN-CNT-SERV-DATE.

           MOVE PLN-PLAN-OPTION            TO BEN-PLAN-OPTION.

           MOVE EMP-CURRENCY-CODE          TO BEN-CURRENCY-CODE.
           MOVE EMP-CURR-ND                TO BEN-CURR-ND.

P85709     IF (PLN-FLEX-PLAN            NOT = SPACES)
P85709     OR (PLN-COVERAGE-TYPE        = "0")
P85709         MOVE ZEROES                 TO BEN-DED-START-DATE
P85709                                        BEN-DED-STOP-DATE
P85709         GO TO 5100-END.  
P85709
           MOVE BNBEN-DED-START-DATE (I1)  TO BEN-DED-START-DATE.

P86484     IF  (PLN-FLEX-PLAN     = SPACES)
P86484     AND (PLN-COVERAGE-TYPE = "0")
P86484         MOVE ZEROES                TO BEN-DED-STOP-DATE
P86484     ELSE
P85709     IF  ((CRT-PROGRAM-CODE         = "BN100")
P85709     OR   (CRT-PROGRAM-CODE         = "BN105"))
P85709     AND (BNBEN-DED-STOP-DATE (I1) NOT = BNBEN-DED-OVSTOP (I1))
P85709     AND (BNBEN-DED-OVSTOP (I1)    NOT = ZEROES)
P85709         MOVE BNBEN-DED-OVSTOP (I1)  TO BEN-DED-STOP-DATE
J26055                                        PREDM-END-DATE
J26055         MOVE "C" TO PREDM-FC
J06128*
J06128         IF (BEN-EMP-PRE-CONT NOT = ZEROES)
J06128             IF (BEN-PCT-AMT-FLAG = "P")
J06128                 MOVE PLN-PRE-DED-CODE-P 
J06128                                     TO PREDM-DED-CODE
J06128             ELSE
J06128                 MOVE PLN-PRE-DED-CODE-A 
J06128                                     TO PREDM-DED-CODE
J06128             END-IF
J06128         END-IF
J06128
J06128         IF (BEN-EMP-AFT-CONT NOT = ZEROES)
J67617             IF (BEN-PCT-AMT-FLAG = "P")
J67617                 MOVE PLN-AFT-DED-CODE-P 
J06128                                     TO PREDM-DED-CODE
J67617             ELSE
J67617                 MOVE PLN-AFT-DED-CODE-A 
J06128                                     TO PREDM-DED-CODE
J67617             END-IF
J06128         END-IF
J67617 
J67617         MOVE BEN-COMPANY    TO DB-COMPANY
J67617         MOVE BEN-EMPLOYEE   TO DB-EMPLOYEE
J67617         MOVE PREDM-DED-CODE TO DB-DED-CODE
J67617         MOVE WS-HIGH-VALUES TO DB-SEQ-NBR
J67617         PERFORM 850-FIND-NLT-EDMSET2
J67617         IF (EMDEDMASTR-FOUND)
J06128             MOVE EDM-SEQ-NBR  TO  PREDM-SEQ-NBR
J67617             PERFORM 870-FIND-PREV-EDMSET2
J06128             IF  (EMDEDMASTR-FOUND) 
J06128             AND (DB-COMPANY = EDM-COMPANY)
J06128             AND (DB-EMPLOYEE = EDM-EMPLOYEE)
J06128             AND (DB-DED-CODE = EDM-DED-CODE)
J67617                 MOVE EDM-SEQ-NBR  TO  PREDM-SEQ-NBR
J06128             END-IF
J67617         END-IF
J67617 
J26055         PERFORM 3000-PREDM-PROCESS-TRAN
P85709     ELSE
P85709         IF (BNBEN-DED-STOP-DATE (I1) = ZEROES)
P85709             MOVE BNWS-SCR-STOP-DATE TO BEN-DED-STOP-DATE
P85709         ELSE
                   MOVE BNBEN-DED-STOP-DATE (I1)
P85709                                     TO BEN-DED-STOP-DATE
P85709         END-IF
P85709     END-IF.

185800 5100-END.

193800******************************************************************
193900 5110-STOP-HRDEPBEN.
194000******************************************************************

           MOVE BNBEN-COMPANY              TO HRHDB-COMPANY.
           MOVE 1                          TO HRHDB-COMPANY-FN.
           MOVE BNBEN-EMPLOYEE (I1)        TO HRHDB-EMPLOYEE.
           MOVE 1                          TO HRHDB-EMPLOYEE-FN.
           INITIALIZE HRHDB-DEPENDENT.
           MOVE 1                          TO HRHDB-DEPENDENT-FN.
           MOVE BNBEN-PLAN-TYPE (I1)       TO HRHDB-PLAN-TYPE.
           MOVE 1                          TO HRHDB-PLAN-TYPE-FN.
           MOVE BNBEN-PLAN-CODE (I1)       TO HRHDB-PLAN-CODE.
           MOVE 1                          TO HRHDB-PLAN-CODE-FN.
           MOVE BNBEN-START-DATE (I1)      TO HRHDB-BEN-START-DATE
                                              HRHDB-EMP-START.
           MOVE 1                          TO HRHDB-BEN-START-DATE-FN. 
           INITIALIZE HRHDB-START-DATE.
           MOVE 1                          TO HRHDB-START-DATE-FN.
           MOVE "S"                        TO HRHDB-FC.
           MOVE 1                          TO HRHDB-FC-FN.
           MOVE BNBEN-STOP-DATE (I1)       TO HRHDB-BEN-STOP-DATE.
           MOVE 1                          TO HRHDB-BEN-STOP-DATE-FN.
           MOVE "E"                        TO HRHDB-COVER-TYPE.

           MOVE BNBEN-CREATE-TRANS (I1)    TO HRHDB-CREATE-TRANS.
           MOVE BNBEN-REASON (I1)          TO HRHDB-REASON.
           MOVE BNBEN-MEMBER-ID (I1)       TO HRHDB-MEMBER-ID.

           MOVE BNBEN-BATCH-FC (I1)        TO HRHDBWS-BATCH-FC.

           PERFORM 3000-HRHDB-PROCESS-TRAN.

           IF  (CRT-PROGRAM-CODE = "BN105" OR "BN102" OR "BN100")
           AND (HRHDB-SV-SP-END-DT (1)  NOT = ZEROES)
               INITIALIZE BNBEN-SV-SP-TBL (I1)
               MOVE HRHDB-SV-SP-TBL    TO BNBEN-SV-SP-TBL (I1)
               INITIALIZE HRHDB-SV-SP-TBL
               INITIALIZE I4.

       5110-END.

194000******************************************************************
       5200-CREATE-BNTRANS.
194000******************************************************************

027800     MOVE BNBEN-COMPANY              TO BNSNWS-COMPANY.
027800     MOVE BNBEN-PLAN-TYPE (I1)       TO BNSNWS-PLAN-TYPE.
027800     MOVE BNBEN-PLAN-CODE (I1)       TO BNSNWS-PLAN-CODE.
027800     MOVE BNBEN-EMPLOYEE (I1)        TO BNSNWS-EMPLOYEE.
027800     INITIALIZE BNSNWS-PARTICIPNT
027800                BNSNWS-DEPENDENT.
027800     MOVE BNBEN-START-DATE (I1)      TO BNSNWS-START-DATE.
           IF (BNBEN-LINE-FC (I1)          = "A" OR "C" OR "D")
               MOVE BNBEN-START-DATE (I1)  TO BNSNWS-EFFECT-DATE
           ELSE
           IF (BNBEN-LINE-FC (I1)          = "S")
               MOVE BNBEN-STOP-DATE (I1)   TO BNSNWS-EFFECT-DATE.
           PERFORM 5000-GET-BNT-TRAN-SEQ-NBR.
           MOVE BNSNWS-TRAN-SEQ-NBR        TO BNBEN-TRAN-SEQ-NBR.

           PERFORM 800-CREATE-BNTRANS.

           MOVE BNBEN-COMPANY              TO BNT-COMPANY.
           MOVE BNBEN-PLAN-TYPE (I1)       TO BNT-PLAN-TYPE.
           MOVE BNBEN-PLAN-CODE (I1)       TO BNT-PLAN-CODE.
           MOVE BNBEN-EMPLOYEE (I1)        TO BNT-EMPLOYEE.

           INITIALIZE BNT-PARTICIPNT
                      BNT-DEPENDENT.

           MOVE BNBEN-START-DATE (I1)      TO BNT-START-DATE.
           MOVE BNBEN-TRAN-SEQ-NBR         TO BNT-TRAN-SEQ-NBR.

           INITIALIZE BNT-EMP-START
                      BNT-EVENT-CODE.

           MOVE "E"                        TO BNT-COVER-TYPE.
           MOVE 1                          TO BNT-TRAN-STATUS.
           MOVE BNBEN-LINE-FC (I1)         TO BNT-TRAN-ACTION.
           MOVE BNBEN-REASON (I1)          TO BNT-TRAN-REASON.
           MOVE BNBEN-MEMBER-ID (I1)       TO BNT-MEMBER-ID.

           IF (BNBEN-LINE-FC (I1)          = "A" OR "C" OR "D")
               MOVE BNBEN-START-DATE (I1)  TO BNT-EFFECT-DATE
           ELSE
           IF (BNBEN-LINE-FC (I1)          = "S")
               MOVE BNBEN-STOP-DATE (I1)   TO BNT-EFFECT-DATE.

           MOVE BNBEN-STOP-DATE (I1)       TO BNT-STOP-DATE.

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
           IF (BNBEN-USER-ID (I1)          = SPACES)
               MOVE CRT-USER-NAME          TO BNT-USER-ID
           ELSE
               MOVE BNBEN-USER-ID (I1)     TO BNT-USER-ID.

           PERFORM 820-STORE-BNTRANS.

       5200-END.

194000******************************************************************
       5300-DEFAULT-HIPAA-FIELDS.
194000******************************************************************

           MOVE PLN-CREATE-TRANS           TO BNBEN-CREATE-TRANS (I1).

           IF (BNBEN-CREATE-TRANS (I1)     = "Y" OR "P")
               MOVE BNBEN-HIPAA-REASON     TO BNBEN-REASON (I1)
               MOVE PLN-MEMBER-ID          TO BNBEN-MEMBER-ID (I1).

       5300-END.

P66372******************************************************************
P66372 5400-CREATE-WORKFLOW-TRIGGER.
P66372******************************************************************
P66372
P66372*---------------------------------+
P66372* Check Status                    |
P66372*---------------------------------+
P66372
P66372**** Check to see if workflow is enabled for a service
P66372
P66372     INITIALIZE WFAPI-INPUT
P66372                WFAPI-OUTPUT.
P66372
P66372**** Check to see if S3.HR Event Service Definition is setup in
P66372**** WorkFlow System
P66372
P66372     MOVE "BNBEN"                TO CRT-ERROR-CAT.
P66372     MOVE 313                    TO CRT-MSG-NBR.
P66372     PERFORM 790-GET-MSG.
P66372     MOVE CRT-MESSAGE            TO WFAPI-I-SERVICE.
P66372     INITIALIZE                     CRT-MESSAGE.
P66372
P66372     PERFORM 1000-WF-SERVICE.
P66372     IF (WFAPI-O-RETURN-CODE     NOT = ZEROES)
P66372         GO TO 5400-END.
P66372
P66372**** If workflow is enabled, create a Work Unit Header message to
P66372**** indicate the specific Event that has occurred in benefits
P66372**** by the HR Company and Employee ID.  Use the Object Definition
P66372**** called BENSET1 defined with the key fields and move BNBEN-COMPANY,
P66372**** BNBEN-PLAN-TYPE, BNBEN-EMPLOYEE, BNBEN-START-DATE, and
P66372**** BNBEN-PLAN-CODE to the WFAPI-I-KEY-VALUE's:
P66372
P66372*---------------------------------+
P66372* Create Work Unit Header         |
P66372*---------------------------------+
P66372**** This creates data in Work Unit Header and matches the fields in
P66372**** the Object Definition Perform CREATE WORK Routine.
P66372
P66372     INITIALIZE WFAPI-INPUT.
P66372
P66372     MOVE BNBEN-BENSET1          TO WFAPI-I-OBJECT-NAME.
P66372     MOVE WFAPI-O-SERVICE        TO WFAPI-I-SERVICE.
P66372     MOVE WFAPI-O-AGENT          TO WFAPI-I-AGENT.
P66372     MOVE WFAPI-O-PROCEDURE      TO WFAPI-I-PROCEDURE.
P66372
P66372     MOVE "BNBEN"                TO CRT-ERROR-CAT.
P66372     MOVE 314                    TO CRT-MSG-NBR.
P66372     PERFORM 790-GET-MSG.
P66372     MOVE CRT-MESSAGE            TO WFAPI-I-WORK-TITLE.
P66372     MOVE BNBEN-COMPANY          TO WFAPI-I-KEY-VALUE (1).
P66372     MOVE BNBEN-PLAN-TYPE (I1)   TO WFAPI-I-KEY-VALUE (2).
P66372     MOVE BNBEN-EMPLOYEE (I1)    TO WFAPI-I-KEY-VALUE (3).
P66372     MOVE BNBEN-START-DATE (I1)  TO WFAPI-I-KEY-VALUE (4).
P66372     MOVE BNBEN-PLAN-CODE (I1)   TO WFAPI-I-KEY-VALUE (5).
P66372
P66372     INITIALIZE WFAPI-OUTPUT.
P66372     PERFORM 1000-WF-CREATE-SETUP.
P66372
P66372**** Pass the variables defined for the service S3.HR Event to the
P66372**** work unit by performing the ADD VARIABLE routine:
P66372
P66372*---------------------------------+
P66372* Create Work Unit Variable       |
P66372*---------------------------------+
P66372**** Perform ADD VARIABLE Routine
P66372
P66372     INITIALIZE WFAPI-INPUT.
P66372
P66372     MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.
P66372
P66372**** COMPANY
P66372     MOVE "BNBEN"                TO CRT-ERROR-CAT.
P66372     MOVE 315                    TO CRT-MSG-NBR.
P66372     PERFORM 790-GET-MSG.
P66372     MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (1).
P66372     MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (1).
P66372     MOVE BNBEN-COMPANY          TO WFAPI-I-VARIABLE-VAL (1).
P66372
P66372**** EMPLOYEE
P66372     MOVE "BNBEN"                TO CRT-ERROR-CAT.
P66372     MOVE 316                    TO CRT-MSG-NBR.
P66372     PERFORM 790-GET-MSG.
P66372     MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (2).
P66372     MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (2).
P66372     MOVE BNBEN-EMPLOYEE (I1)    TO WFAPI-I-VARIABLE-VAL (2).
P66372
P66372**** PLAN-CODE
P66372     MOVE "BNBEN"                TO CRT-ERROR-CAT.
P66372     MOVE 317                    TO CRT-MSG-NBR.
P66372     PERFORM 790-GET-MSG.
P66372     MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (4).
P66372     MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (4).
P66372     MOVE BNBEN-PLAN-CODE (I1)   TO WFAPI-I-VARIABLE-VAL (4).
P66372
P66372**** PLAN-TYPE
P66372     MOVE "BNBEN"                TO CRT-ERROR-CAT.
P66372     MOVE 318                    TO CRT-MSG-NBR.
P66372     PERFORM 790-GET-MSG.
P66372     MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (5).
P66372     MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (5).
P66372     MOVE BNBEN-PLAN-TYPE (I1)   TO WFAPI-I-VARIABLE-VAL (5).
P66372
P66372**** START-DATE
P66372     MOVE "BNBEN"                TO CRT-ERROR-CAT.
P66372     MOVE 319                    TO CRT-MSG-NBR.
P66372     PERFORM 790-GET-MSG.
P66372     MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (6).
P66372     MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (6).
P66372     MOVE BNBEN-START-DATE (I1)  TO WFAPI-I-VARIABLE-VAL (6).
P66372
P66372**** FUNCTION CODE
P66372     MOVE "BNBEN"                TO CRT-ERROR-CAT.
P66372     MOVE 320                    TO CRT-MSG-NBR.
P66372     PERFORM 790-GET-MSG.
P66372     MOVE CRT-MESSAGE            TO WFAPI-I-VARIABLE-NAME (3).
P66372     MOVE "S"                    TO WFAPI-I-VARIABLE-TYPE (3).
P66372     MOVE BNBEN-LINE-FC (I1)     TO WFAPI-I-VARIABLE-VAL (3) .
P66372
P66372     INITIALIZE WFAPI-OUTPUT.
P66372
P66372     PERFORM 1000-WF-ADD-VAR-SETUP.
P66372
P66372     INITIALIZE WFAPI-INPUT.
P66372
P66372     MOVE WFAPI-O-WORKUNIT       TO WFAPI-I-WORKUNIT.
P66372
P66372 5400-END.
P66372
194000******************************************************************
       4000-END.
194000******************************************************************

194000******************************************************************
       5500-GET-COMP-CONT-SW               SECTION.
194000******************************************************************
       5500-START.

           INITIALIZE BNWS-COMP-CONT-SW.

           MOVE BNBEN-COMPANY              TO DB-COMPANY.
           MOVE BNBEN-PLAN-TYPE (I1)       TO DB-PLAN-TYPE.
           MOVE BNBEN-PLAN-CODE (I1)       TO DB-PLAN-CODE.
           MOVE "E"                        TO DB-COVER-TYPE.
           MOVE BNBEN-PREM-UPD-DT(I1)      TO DB-START-DATE.
           MOVE BNBEN-PREM-GROUP (I1)      TO DB-GROUP-NAME.
           PERFORM 840-FIND-PRESET1.
           IF (PREMIUM-FOUND)
               IF  (PLN-CONTRIB-TYPE       = "5")
               AND (PLN-PLAN-TYPE          NOT = "VA")
                   IF (PRE-COMP-AMT        NOT = ZEROES)
                       MOVE "A"            TO BNWS-COMP-CONT-SW
                   ELSE
                   IF (PRE-COMP-PCT        NOT = ZEROES)
                       MOVE "P"            TO BNWS-COMP-CONT-SW
                   END-IF
                   END-IF
               ELSE
               IF (PLN-CONTRIB-TYPE        = "4")
                   MOVE "A"                TO BNWS-COMP-CONT-SW
               ELSE
               IF (PLN-CONTRIB-TYPE        = "1" OR "2" OR "3")
                   IF  (PLN-CONTRIB-TYPE   = "3")
                   AND (PRE-CALC-TYPE      = "4")
                       MOVE PRE-RATE-TABLE   TO DB-TABLE-CODE
                       MOVE PRE-START-DATE   TO DB-START-DATE
                       PERFORM 840-FIND-RTHSET1
                       MOVE RTH-PCT-RATE-FLG TO
                                             BNWS-COMP-CONT-SW
                   ELSE
                   IF (PRE-CALC-TYPE       = "2")
                       MOVE "P"            TO BNWS-COMP-CONT-SW
                   ELSE
                       MOVE "A"            TO BNWS-COMP-CONT-SW
J46581             END-IF
J46581         ELSE
J46581         IF (PLN-CONTRIB-TYPE   = "6" OR "7")
J04191             IF (PLN-PLAN-TYPE  = "RS" OR "DC")
J12251*                MOVE PRE-MATCH-OPTION TO BNWS-COMP-CONT-SW
J12251                 MOVE PRE-EMP-CONT-TYPE TO BNWS-COMP-CONT-SW
J46581             END-IF
J46581         END-IF
J46581     END-IF.

194000******************************************************************
       5500-END.
194000******************************************************************

194000******************************************************************
       5600-SET-BNEMDED-VARS               SECTION.
194000******************************************************************
       5600-START.

138100     MOVE BNBEN-COMPANY              TO BNWS-COMPANY.
138200     MOVE BNBEN-EMPLOYEE  (I1)       TO BNWS-EMPLOYEE.

P85709     IF  (CRT-PROGRAM-CODE     = "BN103")
P85709     AND (PLN-COVERAGE-TYPE    NOT = "0")
P85709     AND (PLN-FLEX-PLAN        = SPACES)
P85709     AND (BNBEN-STOP-DATE (I1) NOT = BNBEN-DED-STOP-DATE (I1))
P85709         MOVE BNBEN-STOP-DATE (I1) TO BNBEN-DED-STOP-DATE (I1)
P85709     END-IF.
P85709
J24315     IF   (BNBEN-DED-START-DATE (I1)    = BNWS-SCR-START-DATE)
P85709     OR  ((BNBEN-DED-START-DATE (I1)    NOT = BNWS-SCR-START-DATE)
P85709     AND  (BNBEN-DED-START-DATE (I1)    NOT = ZEROES))
               MOVE BNBEN-DED-START-DATE (I1) TO BNWS-SCR-START-DATE
           ELSE
138400         MOVE BNBEN-START-DATE (I1)     TO BNWS-SCR-START-DATE
P85709         IF  (PLN-COVERAGE-TYPE    NOT = "0")
P85709         AND (PLN-FLEX-PLAN        = SPACES)
P86476             MOVE BNBEN-START-DATE (I1) TO
P86476                                        BNBEN-DED-START-DATE (I1)
P86476         END-IF
P85709     END-IF.

P86476*     IF  (I1 > 1)
P86476*     AND (BNBEN-DED-STOP-DATE (I1) > BNBEN-DED-START-DATE (I1))
P86476*     AND (PLN-COVERAGE-TYPE       NOT = "0")
P86476*     AND (PLN-FLEX-PLAN           = SPACES)
P86476*         MOVE BNBEN-DED-STOP-DATE (I1) TO BNBEN-DED-STOP-DATE (I1)
P86476*         MOVE BNBEN-DED-START-DATE (I1) TO WSDR-FR-DATE
P86476*         PERFORM 900-DATE-TO-JULIAN
P86476*         SUBTRACT 1                FROM WSDR-JULIAN-DAYS
P86476*         PERFORM 900-JULIAN-TO-DATE
P86476*         MOVE WSDR-FR-DATE         TO BNBEN-DED-STOP-DATE (I1)
P86476*     END-IF.
P86476*
           IF (CRT-PROGRAM-CODE = "BN31" OR "BN32" OR "BN531" OR
                                  "BN102")
P85709         IF  (BNBEN-STOP-DATE (I1) NOT = BNBEN-DED-STOP-DATE (I1))
J24315         AND (BNBEN-DED-STOP-DATE (I1)
J24315                                   NOT = ZEROES)
P85709             MOVE BNBEN-DED-STOP-DATE (I1)  TO BNWS-SCR-STOP-DATE
P85709         ELSE
138400             MOVE BNBEN-STOP-DATE (I1)      TO BNWS-SCR-STOP-DATE
J24315         END-IF
J24315     END-IF.

           IF (CRT-PROGRAM-CODE NOT = "BN31" AND "BN32" AND "BN102"
J24315                                       AND "BN531")
P85709         IF  (BNBEN-STOP-DATE (I1) NOT = BNBEN-DED-STOP-DATE (I1))
P85709         AND (BNBEN-DED-STOP-DATE (I1) < BNBEN-STOP-DATE (I1))
J24315         AND (BNBEN-DED-STOP-DATE (I1) NOT = ZEROES)
P85709             MOVE BNBEN-DED-STOP-DATE (I1)
J24315                                       TO BNWS-SCR-STOP-DATE
P85709         ELSE
138400             MOVE BNBEN-STOP-DATE (I1) TO BNWS-SCR-STOP-DATE
                                                BNBEN-DED-STOP-DATE (I1)
               END-IF
J24315     END-IF.

P86476     IF (PLN-COVERAGE-TYPE        = "0")
P86476     OR (PLN-FLEX-PLAN            NOT = SPACES)
J50193*        MOVE ZEROES                TO BNWS-SCR-START-DATE
J50193         MOVE ZEROES                TO BNWS-SCR-STOP-DATE
                                             BNBEN-DED-START-DATE (I1)
                                             BNBEN-DED-STOP-DATE (I1)
P85709     END-IF.

P85709*     IF  (BNBEN-DED-STOP-DATE (I1)   NOT = BNWS-SCR-STOP-DATE)
P85709*     AND (BNBEN-DED-STOP-DATE (I1)   NOT = ZEROES)
P85709*         MOVE BNBEN-DED-STOP-DATE (I1)  TO BNWS-SCR-STOP-DATE
P85709*     ELSE
P86476*         IF (BNBEN-STOP-DATE (I1) NOT = ZEROES)
138400*             MOVE BNBEN-STOP-DATE (I1)  TO BNWS-SCR-STOP-DATE
P86476*             IF  (PLN-COVERAGE-TYPE        NOT = "0")
P86476*             AND (PLN-FLEX-PLAN            = SPACES)
P86476*             AND (BNBEN-DED-STOP-DATE (I1) = ZEROES)
P86476*                 MOVE BNWS-SCR-STOP-DATE
P86476*                                       TO BNBEN-DED-STOP-DATE (I1)
P86476*             END-IF
P86476*         END-IF
P85709*     END-IF.

138500     INITIALIZE BNWS-YOS
138600                BNWS-TOT-BOND-DIFF
138700                BNWS-CONTR-UPD-DT.
138800     MOVE BNBEN-PCT-AMT-FLAG (I1)    TO BNWS-PCT-AMT-FLAG.
P62556
P62556     IF (BNBEN-COMP-CONT-SW (I1) = SPACES)
P62556         PERFORM 5500-GET-COMP-CONT-SW
P62556         MOVE BNWS-COMP-CONT-SW      TO BNBEN-COMP-CONT-SW (I1)
P62556     ELSE
VAN        MOVE BNBEN-COMP-CONT-SW (I1)    TO BNWS-COMP-CONT-SW.
138900*
139000**** BNWS-COMP-CONT AND BNWS-EMP-AFT-CONT ARE CALCULATED IN 505.
139100**** BNWS-EMP-PRE-CONT AND BNWS-CMP-FLX-CONT ARE CALCULATED IN
139200**** 780-CALC-DOLLARS-SPENT FOR FLEXIBLE BENEFITS.
139300*
139400     IF (BNBEN-FLEX-FLAG (I1)     = "Y")
139500         MOVE BNWS-EMP-AFT-CONT      TO BNWS-AFT-TAX-AMT 
               IF (BNBEN-PLAN-TYPE (I1) = "DC")
139700             COMPUTE BNWS-PRE-TAX-AMT = BNWS-EMP-PRE-CONT 
                                            + BNWS-CMP-FLX-CONT 
               ELSE
139700             MOVE BNWS-EMP-PRE-CONT  TO BNWS-PRE-TAX-AMT
               END-IF
139800     ELSE
139900         INITIALIZE BNWS-CMP-FLX-CONT
140000         MOVE BNWS-EMP-AFT-CONT      TO BNWS-AFT-TAX-AMT
140100         MOVE BNBEN-EMP-PRE-CONT (I1)
140100                                     TO BNWS-PRE-TAX-AMT 
           END-IF.
140200*
           IF  (PLN-CONTRIB-TYPE          = "5")
           AND (PLN-FLEX-PLAN             NOT = SPACES)
VAN        AND (PLN-PLAN-TYPE             NOT = "VA")
               INITIALIZE BNWS-EMP-A-DEC-BAL
               IF (PLN-PLAN-TYPE                = "RS")
                   MOVE WS-TRUE                 TO BNWS-BN39-SW
                   IF (BNBEN-EMP-PRE-CONT (I1)  NOT = ZEROES)
                       MOVE WS-TRUE             TO BNWS-BN39-PRE-SW
                   END-IF
               END-IF
               IF (BNBEN-LINE-FC (I1)     = "A")
                   COMPUTE BNWS-EMP-A-DEC-BAL = (BNBEN-PAY-RATE (I1) -
                                                (BNWS-CMP-DEC-BAL +
                                                 BNWS-EMP-P-DEC-BAL)).

P52255     IF (BNBEN-SPEND-ONLY (I1)       = "Y")
VAN   *     IF (BNBEN-SPEND-ONLY (I1)       = "Y")
           IF  (PLN-CONTRIB-TYPE          = "5")
           AND (PLN-FLEX-PLAN             NOT = SPACES)
           AND (PLN-PLAN-TYPE             NOT = "VA")
               INITIALIZE BNWS-COMPUTE-DIFF
P56420         IF (BNWS-TOTAL-CYC NOT = ZEROS)
P56420            COMPUTE BNWS-COMPUTE-DIFF ROUNDED
                                          = (BNWS-EMP-PRE-CONT /
                                             BNWS-TOTAL-CYC)
                                          + .004
P56420         END-IF
               COMPUTE BNWS-NEXT-PRE-AMT ROUNDED = BNWS-COMPUTE-DIFF
               MOVE BNWS-EMP-PRE-CONT      TO BNWS-EMP-P-DEC-BAL
               INITIALIZE BNWS-COMPUTE-DIFF
P56420         IF (BNWS-TOTAL-CYC NOT = ZEROS) 
P56420            COMPUTE BNWS-COMPUTE-DIFF ROUNDED
                                          = (BNWS-EMP-AFT-CONT /
                                             BNWS-TOTAL-CYC)
                                          + .004
P56420
               COMPUTE BNWS-NEXT-AFT-AMT ROUNDED = BNWS-COMPUTE-DIFF
               MOVE BNWS-EMP-AFT-CONT      TO BNWS-EMP-A-DEC-BAL.

194000******************************************************************
       5600-END.
194000******************************************************************

194000******************************************************************
       6000-CALC-HDB-STOP-DATE             SECTION.
194000******************************************************************
       6000-START.

           PERFORM
               VARYING I4 FROM 1 BY 1
               UNTIL  (I4 > 25)
               OR    ((BNBEN-SV-SP-SPOUSE (1 I4) = HDB-DEPENDENT)
               AND    (BNBEN-SV-SP-PLN-TP (1 I4) = HDB-PLAN-TYPE)
               AND    (BNBEN-SV-SP-PLN-CD (1 I4) = HDB-PLAN-CODE))

               CONTINUE
           END-PERFORM.

           IF   (BNBEN-FUNCTION-CODE       = "CA")
           AND  (BNBEN-LINE-FC (1)         = "A")
           AND  (I4                        NOT > 25)
           AND  (BNBEN-SV-SP-END-DT (1 I4) NOT = ZEROES)
           AND ((BNBEN-SV-SP-END-DT (1 I4) < BEN-STOP-DATE)
           OR   (BEN-STOP-DATE             = ZEROES))
               MOVE BNBEN-SV-SP-END-DT (1 I4) TO HDB-STOP-DATE
               GO TO 6000-END
           ELSE
               MOVE BEN-STOP-DATE          TO HDB-STOP-DATE
               IF (BEN-STOP-DATE           NOT = ZEROES)
J07540         OR (EMD-DEP-TYPE            = "S" OR "P")
                   GO TO 6000-END.

           INITIALIZE HRDEP-CALC-DEP-N-STUD.
           PERFORM 5200-DEP-AGE-DATE-CALC.

           IF (HRDEP-DEP-END-DATE          < BEN-START-DATE)
               INITIALIZE HDB-START-DATE
                          HDB-STOP-DATE
           ELSE
           IF (HRDEP-DEP-END-DATE          >= BEN-START-DATE)
               IF (HRDEP-DEP-END-DATE      < BEN-STOP-DATE)
                   MOVE HRDEP-DEP-END-DATE TO HDB-STOP-DATE
               ELSE
               IF (BEN-STOP-DATE           NOT = ZEROES)
                   MOVE BEN-STOP-DATE      TO HDB-STOP-DATE
               ELSE
                   MOVE HRDEP-DEP-END-DATE TO HDB-STOP-DATE.

           IF  (EMD-DEP-TYPE               = "D")
           AND (EMD-DISABLED               = "Y")
               MOVE BEN-STOP-DATE          TO HDB-STOP-DATE.

194000******************************************************************
       6000-END.
194000******************************************************************

