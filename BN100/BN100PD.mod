******* BN100PD 122.1.25.1.32 <3126621023>
000100******************************************************************
000200*                             BN100PD                            *
000300******************************************************************
      ******************************************************************
      *               M O D I F I C A T I O N   L O G:                 *
      ******************************************************************
      *  Modified by Analysts International,MN               DEVX      *
      ******************************************************************
      *  AI0010  03/24/03  MODIFY THE BENEFIT PLAN ELIGIBILITY COMMON  *
      *                    MODULE TO UTILIZE THE EFFECTIVE DATE AND    *
      *                    END DATE FIELDS IN DETERMINATION OF THE     *
      *                    EMPLOYEES ELIGIBILITY FOR THE BENEFIT PLAN. *
      ******************************************************************
      ******************************************************************
      *  SDB     09/24/04  ADDED LINE TO MATCH CUSTOM PRE-MSP 8        *
000100******************************************************************
      *  GBW WRITE TO FILE TAIPAUDIT IF CHANGE TO TAIP, TAMI OR TAMF   *
      *    TO BE PICKED UP BY REPORT ZN320 - 3/2009                    *
ACS002******************************************************************
ACS002* M. HUNTER - ACS - REAPPLIED ABOVE CUSTOMIZATIONS AFTER 9.0 APP *
ACS002*                   UPGRADE.                                     *
ACS003******************************************************************
ACS003* M. HUNTER - ACS                                                *
ACS003* ACS003  04/08/11   MODIFIED TO WRITE RECORD TO THE ZN325WK1    *
ACS003*                    FILE WHEN A RECORD WITH A PLAN CODE ESMS OR *
ACS003*                    ESMX IS DELETED FROM THE BENEFIT, PARTBEN OR*
ACS003*                    HRDEPBEN TABLES.                            *
ACS003*                    PDLIB MEMBERS BNENTPD AND HRHDBPD WERE ALSO *
ACS003*                    MODIFIED.                                   *
      ******************************************************************
      * M. HUNTER - ACS - REAPPLIED ABOVE CUSTOMIZATIONS AFTER 9.0.1   *
      *  8/23/11          APP UPGRADE.                                 *
      ******************************************************************
      * M. HUNTER - ACS - MODIFIED TO RESOLOVE ERROR 48 WHEN RUN FIRST *
      *  11/22/11         TIME.                                        *
      ******************************************************************
      * M. HUNTER - ACS - MODIFIED TO ONLY WRITE ZN325WK1 RECORD WHEN  *
      * ACS004 11/30/11   DELETING PLAN CODE ESMS.                     *
      ******************************************************************
      * ACS005 12/02/11    MODIFIED TO POPULATE AND INCLUDE WK1-COV-   *
      * M. HUNTER - ACS    OPTION ON ZN325WK1 FILE.                    *
ACS002******************************************************************
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  363718 | J63718 | VALIDATE PREVIOUSLY PRINTED RECORD ON REPORT*
      *         |        | TO PREVENT DUPLICATE PRINTING RECORD OF THE *
      *         |        | SAME TRANSACTION TYPE AND PLAN.             *
      *  ------   ------   ------------------------------------------  *
      *  331822 | J31822 | INCLUDED BNFF ERROR IN WORKFILE TO BE ADDED *
      *         |        | TO THE REPORT.                              *
      *  ------   ------   ------------------------------------------  *
      *  267795 | J67795 | ALLOW 4 DECIMALS FOR BN PENSION PERCENT PLAN*
      *  ------   ------   ------------------------------------------  *
      *  517582 | J17582 | ADDED CODE TO ENABLE LBI BURST REPORTING.   *
      *  ------   ------   ------------------------------------------  *
      *  573358 | J73358 | DO NOT ALLOW DELETION OF BN35 RECORDS THAT  *
      *         |        | WERE NOT PROCESSED BY BN100.                *
      *  ------   ------   ------------------------------------------  *
      *  521197 | J21197 | ADDED PLAN-TYPE AS CONDITION WHEN CHECKING  *
      *         |        | DUPLICATE ROWS IN 1418-EDIT-BY-OPTION.      *
      *  ------   ------   ------------------------------------------  *
      *  614556 | J14556 | ADDED ADDITIONAL VALIDATION BEFORE EXECUTE  *
      *         |        | OF JT331822 CHANGES                         *
      *  ------   ------   ------------------------------------------  *
      *  626718 | J26718 | ADDED ADDITIONAL VALIDATION WITH JT573358   *
      *  ------   ------   ------------------------------------------  *
      *  632757 | J32757 | REMOVED CHANGE MADE FOR JT331822 & 614556   *
      *  ------   ------   ------------------------------------------  *
      *  735176 | J35176 | ADDED CHECKING IF BENEFIT IS FOUND BEFORE   *
      *         |        | USING BEN- FIELDS FOR CALCULATION.          *
      *  ------   ------   ------------------------------------------  *
      *  759685 | J59685 | ADDED INITIALIZE OF CRT-ERROR-NBR IN 3400-  *
      *  ------   ------   ------------------------------------------  *
      *  743365 | J43365 | ADDED SECURITY CHECK FOR COMPANYWIDE PROC   *
      *  ------   ------   ------------------------------------------  *
      *  666766 | J66766 | ADDED A CHANGE IN DED-CODE AS TRIGGER FOR   *
      *         |        | PRINTING OF G4- IN REPORT IN 5810-          *
      *  ------   ------   ------------------------------------------  *
      *  717193 | J17193 | ADDED LOGIC TO PASS DATA OF PRM-UPDATE-ACA  *
      *         |        | AND WEB-UPDATE TO THE BNBEN WS FIELDS.      *
      *  ------   ------   ------------------------------------------- *
      *  864465 | J64465 | Doubling Add Rules wait period date         *
      *  ------   ------   ------------------------------------------  *
      *  890269 | J90269 | ADDED INITIALIZE OF FIELD PRIOR TO BNEMDED  *
      *  ------   ------   ------------------------------------------  *
      *  916623 | J16623 | ADDED INITIALIZATION AND MOVE OF VARIABLES  *
      *         |        | BNEDWS-FROM-MAGIC-SW/BNBEN-SKIP-ELIGIBILITY *
      * -------   ------   ------------------------------------------- *
      * 1014073 | 014073 | REMOVED SETTING BNEDWS-FROM-MAGIC-SW ON     *
      *         |        | PARAGRAPH 7400-CALC-ADD-DATE FOR ADD RULES  *
      * -------   ------   ------------------------------------------- *
      * 1117816 | 117816 | UNCOMMENTED BNEDWS-FROM-MAGIC-SW AND        *
      *         |        | COMMENTED BNEDWS-SKIP-ELIGIBILITY WHICH     *
      *         |        | CAUSES THE UNEXPECTED START DATE            *
      * -------   ------   ------------------------------------------- *
      * 1155990 | 155990 | FIXED ISSUE ON BN100 NOT POPULATING FIELD   *
      *         |        | ELIG-GROUP ON BENEFIT TABLE                 *
      * -------   ------   ------------------------------------------- *
      * 1170393 | 170393 | FIXED ISSUE ON BN100 REPORT DUPLICATE LINES *
      * -------   ------   ------------------------------------------- *
      * 1149940 | 149940 | VALIDATED PLAN START-DATE WHEN PRINTING     *
      *         |        | ENTRIES IN THE REPORT.                      *
      *  857497 | J57497 | Enhance to populate Entry-Date with the BN35*
      *         |        | effective date instead of plan Entry Rule Dt*
      *  ------   ------   ------------------------------------------- *
      ******************************************************************
000100*If error while creating EFR table, Error all BENs               *
000100*Check restart in Proc Bena                                      *
000100*Proc 5,6,7 type for salary change                               *
000100* If PctAmtFlag = "A" we use salary for calculations             *
000100*    Recreate Benefit with old values                            *
000100******************************************************************
000100*REC TYPES     : "C"  - Change records         \                 *
000100*                "S"  - Stop records            \                *
000100*                "CS" - Change Stop              \  Will create  *
000100*                "D"  - (Future) Delete             EFDC or BENC *
000100*                "ET" - Error while Terminating  /  record       *
000100*                "EC" - Error while Changing    /                *
000100*                "ED" - Error while Deleting   /                 *
000100*                "SO" - Sal Override Error    /                  *
000100*                "A"  - Add                     \                *
000100*                "CA" - Change Add               \  Will create  *
000100*                "SA" - Split Add (*)               EFDA or BENA *
000100*                "ES" - Split Error (*)          /  record       *
000100*                "EA" - Error while Adding      /                *
000100*In each of the above Rec Type one BN100WORK record is created   *
000100*with the same Rec Type.                                         *
000100*(*) Record is created only in BENA file                         *
000100******************************************************************
000100*Program creates following Work Files                            *
000100*BN100WORK - Creates record for each BNG, BNH, EFD, BEN and PLN  *
      *            it processed. Contains keys of these records.       *
      *            Records are created in the sequence they are        *
      *            processed. Used for Report and Purging.             *
000100*BN100BENC - Contains S/CS/D/ET/EC type records. Used for Stop,  *
000100*            Change Stop and Delete BENEFIT records.             *
000100*BN100BENA - Contains A/CA/EA type records. Used for Add, Change *
000100*            Add BENEFIT records.                                *
000100*BN100EFDC - Contains S/CS/D/ET/EC type records. Used for Stop,  *
000100*            Change Stop and Delete EMPFLEXDOL records.          *
000100*BN100EFDA - Contains A/CA/EA type records. Used for Add, Change *
000100*            Add EMPFLEXDOL records.                             *
000100*ET/EC/EA/ES will be used with errors                            *
000100******************************************************************
000100*PROCESSING SEQ: Delete Group         (BNGRPCHG)                 *
000100*(For Effect Dt) Zip Code change      (BNCHANGE)                 *
000100*                Salary Change        (BNCHANGE)                 *
000100*                Pay Frequency Change (BNCHANGE)                 *
000100*                Birthdate Change     (BNCHANGE)                 *
000100*                Service Date Change  (BNCHANGE)                 *
000100*                Smoker Flag Change   (BNCHANGE)                 *
000100*                Add Group            (BNGRPCHG)                 *
000100******************************************************************
000100*UPDATE SEQ    : Stop EmpFlexDol      (BN100EFDC)                *
000100*                Add EmpFlexDol       (BN100EFDA)                *
000100*                Stop/Change-Add Ben  (BN100BENC & A)            *
000100*                Add New Benefits     (BN100BENA)                *
000100*                Purge Log Records    (BN100WORK)                *
000100******************************************************************
000100*PHASE CREATE  : Creation of BN100WORK, BN100EFDC, BN100EFDA,    *
      *                            BN100BENC, BN100BENA files          *
000100*PHASE UPDATE  : Process BN100EFDC file. This will Stop or Delete*
000100*(One)           EMPFLEXDOL records.                             *
000100*PHASE UPDATE  : Process BN100EFDA file. This will Add EMPFLEXDOL*
000100*(Two)           records.                                        *
000100*PHASE UPDATE  : Process BN100BENC & A files. This will Stop and *
000100*(Three)         Add BENEFIT records.                            *
000100*PHASE UPDATE  : Process BN100BENA file. This will process Add   *
000100*(Four)          only records.                                   *
000100*PHASE REPORT  : Program will print report in sequence records   *
000100*                created in BN100WORK file, meaning print BNG &  *
000100*                BNH records for the Effect date then print EFD  *
000100*                records from BN100EFDC OR A, then print BEN     *
000100*                records from BN100BENC OR A. Program will find  *
      *                records in C or A files using Function Code,    *
      *                if S/CS/D/ET or EC find a record in C file else *
      *                in A file                                       *
000100*PHASE PURGE   : Process BN100WORK file to purge BNG and BNH     *
000100*                records.                                        *
000100******************************************************************
      *                                                                *
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  307540 | J07540 | DOMESTIC PARTNER                            *
      ******************************************************************

000100******************************************************************
000200 050-EDIT-PARAMETERS             SECTION 10.
000300******************************************************************
000400 050-START.
000500
           MOVE "E"                    TO DB-COVER-TYPE.

           MOVE PRM-COMPANY            TO DB-COMPANY.
           INITIALIZE DB-PROCESS-LEVEL.
           PERFORM 840-FIND-PRSSET1.
004200     IF (PRSYSTEM-NOTFOUND)
      ******** Company does not exist
004300         MOVE 100                TO CRT-ERROR-NBR
000270         MOVE PRM-COMPANY        TO CRT-ERR-VAR1
000290         MOVE WS-TRUE            TO WS-PARAMETER-ERROR
000300         PERFORM 780-PRINT-ERROR-MSG
004500         GO TO 050-END.
004600
186400     MOVE PRM-COMPANY            TO DB-COMPANY.
186500     PERFORM 840-FIND-BNCSET1.
004800     IF (BNCOMPANY-NOTFOUND)
      ******** Company is not setup in benefit system
004900         MOVE 101                TO CRT-ERROR-NBR
000270         MOVE PRM-COMPANY        TO CRT-ERR-VAR1
000290         MOVE WS-TRUE            TO WS-PARAMETER-ERROR
000300         PERFORM 780-PRINT-ERROR-MSG
005100         GO TO 050-END.
005200
003500     IF (PRM-EMPLOYEE-SEQ        = SPACES)
003600         MOVE PRS-EMPLOYEE-SEQ   TO PRM-EMPLOYEE-SEQ.
003700
           IF  (PRM-USE-CURR-DATE      = "Y")
           AND (PRM-THRU-DATE          NOT = ZEROES)
      ******** Thru date must be blank when using current date
004900         MOVE 167                TO CRT-ERROR-NBR
000290         MOVE WS-TRUE            TO WS-PARAMETER-ERROR
000300         PERFORM 780-PRINT-ERROR-MSG
005100         GO TO 050-END.
005200
           IF  (PRM-USE-CURR-DATE      NOT = "Y")
           AND (PRM-THRU-DATE          = ZEROES)
      ******** Thru date is required when not using current date
004900         MOVE 168                TO CRT-ERROR-NBR
000290         MOVE WS-TRUE            TO WS-PARAMETER-ERROR
000300         PERFORM 780-PRINT-ERROR-MSG
005100         GO TO 050-END.
005200
           IF (PRM-USE-CURR-DATE       = "Y")
               MOVE WS-SYSTEM-DATE-YMD TO PRM-THRU-DATE.

           IF  (PRM-PROC-LEVEL         NOT = SPACES)
           AND (PRM-PROC-GROUP         NOT = SPACES)
      ******** Cannot enter process level and processing group
004300         MOVE 143                TO CRT-ERROR-NBR
               MOVE PRM-PROC-LEVEL     TO CRT-ERR-VAR1
000290         MOVE WS-TRUE            TO WS-PARAMETER-ERROR
000300         PERFORM 780-PRINT-ERROR-MSG.
004600
           IF  (PRM-PROC-LEVEL         NOT = SPACES)
           AND (PRM-GROUP-NAME         NOT = SPACES)
      ******** Cannot enter process level and employee group
004300         MOVE 144                TO CRT-ERROR-NBR
               MOVE PRM-PROC-LEVEL     TO CRT-ERR-VAR1
000290         MOVE WS-TRUE            TO WS-PARAMETER-ERROR
000300         PERFORM 780-PRINT-ERROR-MSG.
004600
           IF  (PRM-PROC-GROUP         NOT = SPACES)
           AND (PRM-GROUP-NAME         NOT = SPACES)
      ******** Cannot enter processing group and employee group
004300         MOVE 145                TO CRT-ERROR-NBR
               MOVE PRM-PROC-GROUP     TO CRT-ERR-VAR1
000290         MOVE WS-TRUE            TO WS-PARAMETER-ERROR
000300         PERFORM 780-PRINT-ERROR-MSG.
004600
           IF (PRM-PROC-LEVEL          NOT = SPACES)
               MOVE PRM-COMPANY        TO DB-COMPANY
               MOVE PRM-PROC-LEVEL     TO DB-PROCESS-LEVEL
               PERFORM 840-FIND-PRSSET1
004200         IF (PRSYSTEM-NOTFOUND)
      ************ Process level \ does not exist
004300             MOVE 148            TO CRT-ERROR-NBR
000270             MOVE PRM-PROC-LEVEL TO CRT-ERR-VAR1
000290             MOVE WS-TRUE        TO WS-PARAMETER-ERROR
000300             PERFORM 780-PRINT-ERROR-MSG
               ELSE
      ************ Process level is inactive
                   IF (PRS-ACTIVE-FLAG = "I")
                       MOVE 170            TO CRT-ERROR-NBR
                       MOVE PRM-PROC-LEVEL TO CRT-ERR-VAR1
                       MOVE WS-TRUE        TO WS-PARAMETER-ERROR
                       PERFORM 780-PRINT-ERROR-MSG
                   END-IF.
004600
           IF (PRM-PROC-GROUP          NOT = SPACES)
               MOVE PRM-COMPANY        TO DB-COMPANY
               MOVE PRM-PROC-GROUP     TO DB-PROC-GROUP
               MOVE PRPSET1-PROC-GROUP TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PRPSET1
               IF (PRPROCGRP-NOTFOUND)
      ************ Processing group \ does not exist
004900             MOVE 146            TO CRT-ERROR-NBR
000270             MOVE PRM-PROC-GROUP TO CRT-ERR-VAR1
000290             MOVE WS-TRUE        TO WS-PARAMETER-ERROR
000300             PERFORM 780-PRINT-ERROR-MSG.

           IF (PRM-GROUP-NAME          NOT = SPACES)
               MOVE PRM-COMPANY        TO DB-COMPANY
               MOVE PRM-GROUP-NAME     TO DB-GROUP-NAME
               PERFORM 840-FIND-PRGSET1
               IF (PERSGROUP-NOTFOUND)
      ************ Employee group \ does not exist
004900             MOVE 147            TO CRT-ERROR-NBR
000270             MOVE PRM-GROUP-NAME TO CRT-ERR-VAR1
000290             MOVE WS-TRUE        TO WS-PARAMETER-ERROR
000300             PERFORM 780-PRINT-ERROR-MSG.

000600 050-END.
000700
000800******************************************************************
000900 100-PROGRAM-CONTROL             SECTION 10.
001000******************************************************************
001100 100-START.
001200
      **** Processing BN100 - Employee benefit change
001400     MOVE 050                        TO CRT-MSG-NBR.
001410     PERFORM 780-DISPLAY-MSG.
PERF******
PERF***GET MSGS AND STORE IN WORKING STORAGE FOR PERFORMANCE IMPROVEMENT
PERF       MOVE 114                    TO CRT-MSG-NBR.
PERF       MOVE "BN100"                TO CRT-ERROR-CAT.
PERF       PERFORM 790-GET-MSG.
PERF       MOVE CRT-MESSAGE            TO WS-BN100-MSG-114.
PERF
PERF       MOVE 115                    TO CRT-MSG-NBR.
PERF       MOVE "BN100"                TO CRT-ERROR-CAT.
PERF       PERFORM 790-GET-MSG.
PERF       MOVE CRT-MESSAGE            TO WS-BN100-MSG-115.
PERF
PERF       MOVE 130                    TO CRT-MSG-NBR.
PERF       MOVE "BN100"                TO CRT-ERROR-CAT.
PERF       PERFORM 790-GET-MSG.
PERF       MOVE CRT-MESSAGE            TO WS-BN100-MSG-130.
PERF
PERF       MOVE 190                    TO CRT-MSG-NBR.
PERF       MOVE "BNMSG"                TO CRT-ERROR-CAT.
PERF       PERFORM 790-GET-MSG.
PERF       MOVE CRT-MESSAGE            TO WS-BNMSG-MSG-190.
PERF
PERF       MOVE 191                    TO CRT-MSG-NBR.
PERF       MOVE "BNMSG"                TO CRT-ERROR-CAT.
PERF       PERFORM 790-GET-MSG.
PERF       MOVE CRT-MESSAGE            TO WS-BNMSG-MSG-191.
PERF
PERF       MOVE 192                    TO CRT-MSG-NBR.
PERF       MOVE "BNMSG"                TO CRT-ERROR-CAT.
PERF       PERFORM 790-GET-MSG.
PERF       MOVE CRT-MESSAGE            TO WS-BNMSG-MSG-192.
PERF
PERF       MOVE 193                    TO CRT-MSG-NBR.
PERF       MOVE "BNMSG"                TO CRT-ERROR-CAT.
PERF       PERFORM 790-GET-MSG.
PERF       MOVE CRT-MESSAGE            TO WS-BNMSG-MSG-193.
PERF
PERF       MOVE 194                    TO CRT-MSG-NBR.
PERF       MOVE "BNMSG"                TO CRT-ERROR-CAT.
PERF       PERFORM 790-GET-MSG.
PERF       MOVE CRT-MESSAGE            TO WS-BNMSG-MSG-194.
PERF
PERF       MOVE 195                    TO CRT-MSG-NBR.
PERF       MOVE "BNMSG"                TO CRT-ERROR-CAT.
PERF       PERFORM 790-GET-MSG.
PERF       MOVE CRT-MESSAGE            TO WS-BNMSG-MSG-195.
PERF
PERF       MOVE 196                    TO CRT-MSG-NBR.
PERF       MOVE "BNMSG"                TO CRT-ERROR-CAT.
PERF       PERFORM 790-GET-MSG.
PERF       MOVE CRT-MESSAGE            TO WS-BNMSG-MSG-196.
PERF
PERF       MOVE 198                    TO CRT-MSG-NBR.
PERF       MOVE "BNMSG"                TO CRT-ERROR-CAT.
PERF       PERFORM 790-GET-MSG.
PERF       MOVE CRT-MESSAGE            TO WS-BNMSG-MSG-198.
PERF
PERF       MOVE 203                    TO CRT-MSG-NBR.
PERF       MOVE "BNMSG"                TO CRT-ERROR-CAT.
PERF       PERFORM 790-GET-MSG.
PERF       MOVE CRT-MESSAGE            TO WS-BNMSG-MSG-203.
PERF
PERF       MOVE 107                    TO CRT-MSG-NBR.
PERF       MOVE "BNCOM"                TO CRT-ERROR-CAT.
PERF       PERFORM 790-GET-MSG.
PERF       MOVE CRT-MESSAGE            TO WS-BNCOM-MSG-107.
PERF
PERF       MOVE 108                    TO CRT-MSG-NBR.
PERF       MOVE "BNCOM"                TO CRT-ERROR-CAT.
PERF       PERFORM 790-GET-MSG.
PERF       MOVE CRT-MESSAGE            TO WS-BNCOM-MSG-108.
001420
001430     PERFORM 840-FIND-CKPSET1.
001440     IF  (CKPOINT-FOUND)
001450     AND (CKP-RESTART-INFO           NOT = SPACES)
001460         MOVE CKP-RESTART-INFO       TO WS-RESTART-DATA
001470         MOVE WS-REST-WORK-FILE      TO WS-WORK-FILE
P28952         MOVE WS-REST-WORKS-FILE     TO WS-WORKS-FILE
P28952         MOVE WS-REST-WORKI-FILE     TO WS-WORKI-FILE
001470         MOVE WS-REST-BEN-CHANGE-FILE   TO WS-BEN-CHANGE-FILE
001470         MOVE WS-REST-EFD-CHANGE-FILE   TO WS-EFD-CHANGE-FILE
001470         MOVE WS-REST-BEN-ADD-FILE   TO WS-BEN-ADD-FILE
001470         MOVE WS-REST-EFD-ADD-FILE   TO WS-EFD-ADD-FILE
001510         MOVE WS-RESTART-COUNT       TO WS-RECORD-COUNT
001510         MOVE WS-REST-EMP-COUNT      TO WS-EMP-REC-COUNT
001510         MOVE WS-REST-FILE-NAME      TO WS-FILE-NAME
001510         MOVE WS-REST-NAV-POINTER    TO WS-NAV-POINTER
001510         MOVE WS-REST-EFDA-DATA-SW   TO WS-EFDA-DATA-SW
001510         MOVE WS-REST-EFDC-DATA-SW   TO WS-EFDC-DATA-SW
001510         MOVE WS-REST-BENA-DATA-SW   TO WS-BENA-DATA-SW
001510         MOVE WS-REST-BENC-DATA-SW   TO WS-BENC-DATA-SW
001520         MOVE WS-TRUE                TO PROGRAM-RESTARTING
001530         IF (CREATE-WORK)
001540             GO TO 100-CREATE-WORK
001550         ELSE
001630         IF (PROCESS-BN100EFDC)
001640             GO TO 100-PROCESS-BN100EFDC
               ELSE
001630         IF (PROCESS-BN100EFDA)
001640             GO TO 100-PROCESS-BN100EFDA
               ELSE
001630         IF (PROCESS-BN100BENC)
001640             GO TO 100-PROCESS-BN100BENC
001690         ELSE
001630         IF (PROCESS-BN100BENA)
001640             GO TO 100-PROCESS-BN100BENA
001690         ELSE
001560         IF (PRINT-REPORT)
001570             GO TO 100-PRINT-REPORT
001580         ELSE
001620         IF (PURGE-BNG-BNH)
001640             GO TO 100-PURGE-BNG-N-BNH
001700         ELSE
001710             GO TO 100-END.
001720
001730     PERFORM 910-AUDIT-BEGIN.
001740
001750     PERFORM 840-MODIFY-CKPSET1.
001760     SET CREATE-WORK             TO TRUE.
001770     MOVE WS-RESTART-DATA        TO CKP-RESTART-INFO.
001780     PERFORM 820-STORE-CKPOINT.
001790
001800     PERFORM 925-AUDIT-END.
001810
001820******************************************************************
001830* CREATE BN100WORK, BN100EFDC, BN100BENC, BN100EFDA, BN100BENA   *
001840******************************************************************
001850 100-CREATE-WORK.
001860******************************************************************
001870
001880     IF (WS-WORK-FILE            = SPACES)
001890         PERFORM 900-BUILD-TMP-FILE-NAME
001900         MOVE WS-TMP-FILE        TO WS-WORK-FILE
                                          WS-REST-WORK-FILE
               OPEN OUTPUT BN100WORK-FILE
               CLOSE BN100WORK-FILE.
001910
001880     IF (WS-BEN-CHANGE-FILE      = SPACES)
001890         PERFORM 900-BUILD-TMP-FILE-NAME
001900         MOVE WS-TMP-FILE        TO WS-BEN-CHANGE-FILE
001900                                    WS-REST-BEN-CHANGE-FILE
               OPEN OUTPUT BN100BENC-FILE
               CLOSE BN100BENC-FILE.
001910
001880     IF (WS-EFD-CHANGE-FILE      = SPACES)
001890         PERFORM 900-BUILD-TMP-FILE-NAME
001900         MOVE WS-TMP-FILE        TO WS-EFD-CHANGE-FILE
001900                                    WS-REST-EFD-CHANGE-FILE
               OPEN OUTPUT BN100EFDC-FILE
               CLOSE BN100EFDC-FILE.

001880     IF (WS-BEN-ADD-FILE         = SPACES)
001890         PERFORM 900-BUILD-TMP-FILE-NAME
001900         MOVE WS-TMP-FILE        TO WS-BEN-ADD-FILE
001900                                    WS-REST-BEN-ADD-FILE
               OPEN OUTPUT BN100BENA-FILE
               CLOSE BN100BENA-FILE.
001910
001880     IF (WS-EFD-ADD-FILE         = SPACES)
001890         PERFORM 900-BUILD-TMP-FILE-NAME
001900         MOVE WS-TMP-FILE        TO WS-EFD-ADD-FILE
001900                                    WS-REST-EFD-ADD-FILE
               OPEN OUTPUT BN100EFDA-FILE
               CLOSE BN100EFDA-FILE.
001910
001920     OPEN I-O BN100WORK-FILE.
001930
001920     OPEN I-O BN100BENC-FILE.
001930
001920     OPEN I-O BN100EFDC-FILE.
001930
001920     OPEN I-O BN100BENA-FILE.
001930
001920     OPEN I-O BN100EFDA-FILE.
001930
001940     PERFORM 1000-CREATE-WORK.
001950
001960     CLOSE BN100WORK-FILE SAVE.
001970
001960     CLOSE BN100BENC-FILE SAVE.
001970
001960     CLOSE BN100EFDC-FILE SAVE.
001970
001960     CLOSE BN100BENA-FILE SAVE.
001970
001960     CLOSE BN100EFDA-FILE SAVE.
001970
001980     PERFORM 910-AUDIT-BEGIN.
001990
002000     PERFORM 840-MODIFY-CKPSET1.
P60882     INITIALIZE BNEDWS-ELIGIBILITY-DATE.
P60882
           IF (PRM-UPDATE-OPTION           = "U")
               IF (EFDC-DATA)
002010             SET PROCESS-BN100EFDC   TO TRUE
               ELSE
               IF (EFDA-DATA)
002010             SET PROCESS-BN100EFDA   TO TRUE
               ELSE
               IF (BENC-DATA)
002010             SET PROCESS-BN100BENC   TO TRUE
               ELSE
               IF (BENA-DATA)
002010             SET PROCESS-BN100BENA   TO TRUE
               ELSE
002010             SET PRINT-REPORT        TO TRUE
               END-IF
               END-IF
               END-IF
               END-IF
           ELSE
002010         SET PRINT-REPORT            TO TRUE.
           MOVE WS-EFDA-DATA-SW            TO WS-REST-EFDA-DATA-SW.
           MOVE WS-EFDC-DATA-SW            TO WS-REST-EFDC-DATA-SW.
           MOVE WS-BENA-DATA-SW            TO WS-REST-BENA-DATA-SW.
           MOVE WS-BENC-DATA-SW            TO WS-REST-BENC-DATA-SW.
002070     MOVE WS-RESTART-DATA            TO CKP-RESTART-INFO.
002080     PERFORM 820-STORE-CKPOINT.
002090
002100     PERFORM 925-AUDIT-END.
002110
           IF (PROCESS-BN100EFDC)
               GO TO 100-PROCESS-BN100EFDC
           ELSE
           IF (PROCESS-BN100EFDA)
               GO TO 100-PROCESS-BN100EFDA
           ELSE
           IF (PROCESS-BN100BENC)
               GO TO 100-PROCESS-BN100BENC
           ELSE
           IF (PROCESS-BN100BENA)
               GO TO 100-PROCESS-BN100BENA
           ELSE
           IF (PRINT-REPORT)
               GO TO 100-PRINT-REPORT.

003300******************************************************************
003310* STOP/DELETE EMPFLEXDOL USING BN100EFDC                         *
003320******************************************************************
003330 100-PROCESS-BN100EFDC.
003340******************************************************************
003350
002870     OPEN INPUT BN100EFDC-FILE.
002880
003360     PERFORM 910-AUDIT-BEGIN.
003370
003400     PERFORM 2000-PROCESS-BN100EFDC.
003410
003030     PERFORM 840-MODIFY-CKPSET1.
           INITIALIZE WS-RESTART-COUNT
                      WS-RECORD-COUNT.
           IF (PRM-UPDATE-OPTION           = "U")
               IF (EFDA-DATA)
002010             SET PROCESS-BN100EFDA   TO TRUE
               ELSE
               IF (BENC-DATA)
002010             SET PROCESS-BN100BENC   TO TRUE
               ELSE
               IF (BENA-DATA)
002010             SET PROCESS-BN100BENA   TO TRUE
               ELSE
002010             SET PRINT-REPORT        TO TRUE
               END-IF
               END-IF
               END-IF
           ELSE
002010         SET PRINT-REPORT            TO TRUE.
003100     MOVE WS-RESTART-DATA            TO CKP-RESTART-INFO.
003110     PERFORM 820-STORE-CKPOINT.
003120
003130     PERFORM 925-AUDIT-END.
003140
002990     CLOSE BN100EFDC-FILE SAVE.
003000
           IF (PROCESS-BN100EFDA)
               GO TO 100-PROCESS-BN100EFDA
           ELSE
           IF (PROCESS-BN100BENC)
               GO TO 100-PROCESS-BN100BENC
           ELSE
           IF (PROCESS-BN100BENA)
               GO TO 100-PROCESS-BN100BENA
           ELSE
           IF (PRINT-REPORT)
               GO TO 100-PRINT-REPORT.

003300******************************************************************
003310* ADD EMPFLEXDOL USING BN100EFDA                                 *
003320******************************************************************
003330 100-PROCESS-BN100EFDA.
003340******************************************************************
003350
002870     OPEN INPUT BN100EFDA-FILE.
002880
003360     PERFORM 910-AUDIT-BEGIN.
003370
003400     PERFORM 3000-PROCESS-BN100EFDA.
003410
003030     PERFORM 840-MODIFY-CKPSET1.
           INITIALIZE WS-RESTART-COUNT
                      WS-RECORD-COUNT.
           IF (PRM-UPDATE-OPTION           = "U")
               IF (BENC-DATA)
002010             SET PROCESS-BN100BENC   TO TRUE
               ELSE
               IF (BENA-DATA)
002010             SET PROCESS-BN100BENA   TO TRUE
               ELSE
002010             SET PRINT-REPORT        TO TRUE
               END-IF
               END-IF
           ELSE
002010         SET PRINT-REPORT            TO TRUE.
003100     MOVE WS-RESTART-DATA            TO CKP-RESTART-INFO.
003110     PERFORM 820-STORE-CKPOINT.
003120
003130     PERFORM 925-AUDIT-END.
003140
002990     CLOSE BN100EFDA-FILE SAVE.
003000
           IF (PROCESS-BN100BENC)
               GO TO 100-PROCESS-BN100BENC
           ELSE
           IF (PROCESS-BN100BENA)
               GO TO 100-PROCESS-BN100BENA
           ELSE
           IF (PRINT-REPORT)
               GO TO 100-PRINT-REPORT.

003300******************************************************************
003310* STOP/DELETE/ADD BENEFIT                                        *
003320******************************************************************
003330 100-PROCESS-BN100BENC.
003340******************************************************************
ACS004
ACS004     IF (PRM-UPDATE-OPTION           = "U")
ACS004        OPEN EXTEND ZN325WK1-FILE. 
002870     OPEN I-O BN100BENC-FILE.

002870     OPEN I-O BN100BENA-FILE.
002880
002870     OPEN I-O BN100WORK-FILE.
002880
003360     PERFORM 910-AUDIT-BEGIN.
003370
           PERFORM 4000-PROCESS-BN100BENC.

003030     PERFORM 840-MODIFY-CKPSET1.
           INITIALIZE WS-RESTART-COUNT
                      WS-RECORD-COUNT.
           IF (PRM-UPDATE-OPTION           = "U")
               IF (BENA-DATA)
002010             SET PROCESS-BN100BENA   TO TRUE
               ELSE
002010             SET PRINT-REPORT        TO TRUE
               END-IF
           ELSE
002010         SET PRINT-REPORT            TO TRUE.
003100     MOVE WS-RESTART-DATA            TO CKP-RESTART-INFO.
003110     PERFORM 820-STORE-CKPOINT.
003120
003130     PERFORM 925-AUDIT-END.
003140
002990     CLOSE BN100BENC-FILE SAVE.
003000
002990     CLOSE BN100BENA-FILE SAVE.
003000
002990     CLOSE BN100WORK-FILE SAVE.
003000
ACS004     IF (PRM-UPDATE-OPTION           = "U")
ACS004        CLOSE ZN325WK1-FILE SAVE.
           IF (PROCESS-BN100BENA)
               GO TO 100-PROCESS-BN100BENA
           ELSE
           IF (PRINT-REPORT)
               GO TO 100-PRINT-REPORT.

003300******************************************************************
003310* ADD BENEFIT                                                    *
003320******************************************************************
003330 100-PROCESS-BN100BENA.
003340******************************************************************
003350
      **** Processing BN100 - Updating benefits (Add only)
001400     MOVE 060                    TO CRT-MSG-NBR.
001410     PERFORM 780-DISPLAY-MSG.
001420
002870     OPEN I-O BN100BENA-FILE.
002880
002870     OPEN I-O BN100WORK-FILE.
002880
003360     PERFORM 910-AUDIT-BEGIN.
003370
003400     PERFORM 4500-PROCESS-BN100BENA.
003410
003030     PERFORM 840-MODIFY-CKPSET1.
           INITIALIZE WS-RESTART-COUNT
                      WS-RECORD-COUNT.
002010     SET PRINT-REPORT            TO TRUE.
003100     MOVE WS-RESTART-DATA        TO CKP-RESTART-INFO.
003110     PERFORM 820-STORE-CKPOINT.
003120
003130     PERFORM 925-AUDIT-END.
003140
002990     CLOSE BN100BENA-FILE SAVE.
003000
002990     CLOSE BN100WORK-FILE SAVE.
003000
002710******************************************************************
002740* PRINT REPORT USING BN100WORK                                   *
002750******************************************************************
002760 100-PRINT-REPORT.
002770******************************************************************
002780
J17582     PERFORM 1000-OPEN-WORKFLOW-DB.
J17582     PERFORM 600-BEGIN-BROADCAST.
J17582     MOVE "Y"                    TO PRBRD-DO-BASE-BROADCAST.
J17582     MOVE PRM-PRINT-FILE         TO PRBRD-PRINT-FILE.
J17582
001880     IF (WS-WORKS-FILE           = SPACES)
001890         PERFORM 900-BUILD-TMP-FILE-NAME
001900         MOVE WS-TMP-FILE        TO WS-WORKS-FILE
P28952                                    WS-REST-WORKS-FILE.
001910
P28952     IF (WS-WORKI-FILE           = SPACES)
P28952         PERFORM 900-BUILD-TMP-FILE-NAME
P28952         MOVE WS-TMP-FILE        TO WS-WORKI-FILE
P28952                                    WS-REST-WORKI-FILE.
P28952
P28952     PERFORM 910-AUDIT-BEGIN.
P28952     PERFORM 840-MODIFY-CKPSET1.
P28952     MOVE WS-RESTART-DATA        TO CKP-RESTART-INFO.
P28952     PERFORM 820-STORE-CKPOINT.
P28952     PERFORM 925-AUDIT-END.
P28952
           IF (PRM-EMPLOYEE-SEQ       = "N")
001290         SORT BN100SORT-FILE
001300             ASCENDING KEY       DSF-COMPANY
001330                                 DSF-PROC-LEVEL
001330                                 DSF-DEPARTMENT
001330                                 DSF-EMPLOYEE
001360                                 DSF-EFFECT-DATE
001360                                 DSF-REC-TYPE
001360                                 DSF-SEQ-NBR
001400                           USING BN100WORK-FILE
001410                          GIVING BN100WORKI-FILE SAVE
           ELSE
001290         SORT BN100SORT-FILE
001300             ASCENDING KEY       DSF-COMPANY
001330                                 DSF-PROC-LEVEL
001330                                 DSF-DEPARTMENT
001330                                 DSF-LAST-NAME
001330                                 DSF-FIRST-NAME
001330                                 DSF-MIDDLE-INIT
P67650                                 DSF-EMPLOYEE
001360                                 DSF-EFFECT-DATE
001360                                 DSF-REC-TYPE
001360                                 DSF-SEQ-NBR
001400                           USING BN100WORK-FILE
001410                          GIVING BN100WORKI-FILE SAVE.

P28952*--- Create BN100WORKS-FILE using BN100WORKI-FILE
P28952
P28952     OPEN INPUT BN100WORKI-FILE.
P28952
P28952     OPEN OUTPUT BN100WORKS-FILE.
P28952
P28952     SET NO-END-OF-FILE          TO TRUE.
P28952     READ BN100WORKI-FILE        INTO SRI-BN100WORKI-REC
P28952         AT END
P28952             SET END-OF-FILE     TO TRUE.
P28952     PERFORM
P28952         UNTIL (END-OF-FILE)
P28952
P28952         MOVE SRI-BN100WORKI-REC TO SRT-BN100WORKS-REC
P28952
P28952         WRITE BN100WORKS-REC    FROM SRT-BN100WORKS-REC
P28952
P28952         READ BN100WORKI-FILE    INTO SRI-BN100WORKI-REC
P28952             AT END
P28952                 SET END-OF-FILE TO TRUE
P28952         END-READ
P28952     END-PERFORM.
P28952
P28952     CLOSE BN100WORKI-FILE.
P28952
P28952     CLOSE BN100WORKS-FILE.
P28952
P28952*--- End Create BN100WORKS-FILE
011890
002870     OPEN INPUT BN100WORKS-FILE.
002880
002870     OPEN INPUT BN100BENC-FILE.
002880
002870     OPEN INPUT BN100EFDC-FILE.
002880
002870     OPEN INPUT BN100BENA-FILE.
002880
002870     OPEN INPUT BN100EFDA-FILE.
002880
002400     OPEN OUTPUT ERROR-FILE.
002500
           OPEN OUTPUT ELIG-FILE.
GW309      IF (PRM-UPDATE-OPTION           = "U")
GW309         PERFORM 800-OPENAPPENDCSV-TAIPAUDIT.

002930     PERFORM 5000-PRINT-REPORT.
002940
002990     CLOSE BN100WORKS-FILE SAVE.
003000
002990     CLOSE BN100BENC-FILE SAVE.
003000
002990     CLOSE BN100EFDC-FILE SAVE.
003000
002990     CLOSE BN100BENA-FILE SAVE.
003000
002990     CLOSE BN100EFDA-FILE SAVE.
003000
003200     CLOSE ERROR-FILE.
003300
           CLOSE ELIG-FILE.
GW0309     IF (PRM-UPDATE-OPTION           = "U")
GW0309        PERFORM 800-CLOSECSV-TAIPAUDIT.

003010     PERFORM 910-AUDIT-BEGIN.
003020
003030     PERFORM 840-MODIFY-CKPSET1.
003040     SET PURGE-BNG-BNH           TO TRUE.
003100     MOVE WS-RESTART-DATA        TO CKP-RESTART-INFO.
003110     PERFORM 820-STORE-CKPOINT.
003120
003130     PERFORM 925-AUDIT-END.
003140
003150     IF (PRM-UPDATE-OPTION       = "R")
003160         MOVE WS-WORK-FILE       TO WS-TMP-FILE
003170         PERFORM 901-REMOVE-TMP-FILE
003180
003160         MOVE WS-WORKS-FILE      TO WS-TMP-FILE
003170         PERFORM 901-REMOVE-TMP-FILE
003180
P28952         MOVE WS-WORKI-FILE      TO WS-TMP-FILE
P28952         PERFORM 901-REMOVE-TMP-FILE
P28952
003160         MOVE WS-BEN-CHANGE-FILE TO WS-TMP-FILE
003170         PERFORM 901-REMOVE-TMP-FILE
003180
003160         MOVE WS-EFD-CHANGE-FILE TO WS-TMP-FILE
003170         PERFORM 901-REMOVE-TMP-FILE

003160         MOVE WS-BEN-ADD-FILE    TO WS-TMP-FILE
003170         PERFORM 901-REMOVE-TMP-FILE
003180
003160         MOVE WS-EFD-ADD-FILE    TO WS-TMP-FILE
003170         PERFORM 901-REMOVE-TMP-FILE
003180
J17582         IF (PRBRD-BROADCAST-ENABLED)
J17582             MOVE PRM-PRINT-FILE TO PRBRD-PRINT-FILE
J17582             PERFORM 605-END-BROADCAST
J17582             PERFORM 620-RELEASE-BROADCAST
J17582         END-IF
003280         GO TO 100-END.
J17582
J17582     IF (PRBRD-BROADCAST-ENABLED)
J17582         MOVE PRM-PRINT-FILE TO PRBRD-PRINT-FILE
J17582         PERFORM 605-END-BROADCAST
J17582         PERFORM 620-RELEASE-BROADCAST
J17582     END-IF.
003290
003300******************************************************************
003310* PURGE BNGRPCHG AND BNCHANGE RECORDS USING BN100WORK            *
003320******************************************************************
003330 100-PURGE-BNG-N-BNH.
003340******************************************************************
003350
002870     OPEN INPUT BN100WORK-FILE.
002880
003360     PERFORM 910-AUDIT-BEGIN.
003370
003400     PERFORM 6000-PURGE-BNG-N-BNH.
003410
003130     PERFORM 925-AUDIT-END.
003140
002990     CLOSE BN100WORK-FILE SAVE.
003000
003740     MOVE WS-WORK-FILE           TO WS-TMP-FILE.
003750     PERFORM 901-REMOVE-TMP-FILE.
003760
003740     MOVE WS-BEN-CHANGE-FILE     TO WS-TMP-FILE.
003750     PERFORM 901-REMOVE-TMP-FILE.
003760
003740     MOVE WS-EFD-CHANGE-FILE     TO WS-TMP-FILE.
003750     PERFORM 901-REMOVE-TMP-FILE.

003740     MOVE WS-BEN-ADD-FILE        TO WS-TMP-FILE.
003750     PERFORM 901-REMOVE-TMP-FILE.
003760
003740     MOVE WS-EFD-ADD-FILE        TO WS-TMP-FILE.
003750     PERFORM 901-REMOVE-TMP-FILE.
003760
003900******************************************************************
003860 100-END.
003900******************************************************************
003870
003880******************************************************************
003890 1000-CREATE-WORK                SECTION 50.
003900******************************************************************
003910 1000-START.
003920
      **** Processing BN100 - Creating workfiles
001400     MOVE 055                        TO CRT-MSG-NBR.
001410     PERFORM 780-DISPLAY-MSG.
001420
           SET SELECT-EMPLOYEE             TO TRUE.

           MOVE PRM-COMPANY                TO DB-COMPANY.
           IF (PRM-PROC-LEVEL              NOT = SPACES)
               MOVE PRM-PROC-LEVEL         TO DB-PROCESS-LEVEL
               MOVE EMPSET7-PROCESS-LEVEL  TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-EMPSET7
               PERFORM 1050-SEL-EMPLOYEE
               THRU    1050-END
                   UNTIL (EMPLOYEE-NOTFOUND)
           ELSE
           IF (PRM-PROC-GROUP              NOT = SPACES)
               MOVE PRM-PROC-GROUP         TO DB-PROC-GROUP
               MOVE PRPSET1-PROC-GROUP     TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-PRPSET1
               PERFORM
                   UNTIL (PRPROCGRP-NOTFOUND)

                   MOVE PRP-PROCESS-LEVEL  TO DB-PROCESS-LEVEL
                   MOVE EMPSET7-PROCESS-LEVEL TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-EMPSET7
                   PERFORM 1050-SEL-EMPLOYEE
                   THRU    1050-END
                       UNTIL (EMPLOYEE-NOTFOUND)

                   PERFORM 860-FIND-NXTRNG-PRPSET1
               END-PERFORM
           ELSE
           IF (PRM-GROUP-NAME              NOT = SPACES)
               MOVE PRM-COMPANY            TO WS-PGE-COMPANY
               MOVE PRM-GROUP-NAME         TO DB-GROUP-NAME
                                              WS-PGE-GROUP-NAME
               INITIALIZE                     DB-EMPLOYEE
               PERFORM 850-FIND-NLT-PGESET1
               PERFORM
                   UNTIL (PGEMPLOYEE-NOTFOUND)
                   OR    (PGE-COMPANY      NOT = DB-COMPANY)
                   OR    (PGE-GROUP-NAME   NOT = DB-GROUP-NAME)

                   MOVE PGE-EMPLOYEE       TO DB-EMPLOYEE
                                              WS-PGE-EMPLOYEE
                   PERFORM 840-FIND-EMPSET1
                   PERFORM 840-FIND-PEMSET1
                   PERFORM 1050-SEL-EMPLOYEE
                   THRU    1050-END

      *  PGE-EMPLOYEE may have been changed in 1050, find original
                   MOVE WS-PGE-COMPANY     TO DB-COMPANY
                   MOVE WS-PGE-GROUP-NAME  TO DB-GROUP-NAME
                   MOVE WS-PGE-EMPLOYEE    TO DB-EMPLOYEE
                   PERFORM 850-FIND-NLT-PGESET1
                   PERFORM 860-FIND-NEXT-PGESET1
               END-PERFORM
           ELSE
               SET SELECT-COMPANY          TO TRUE

005590         MOVE WS-FALSE               TO WS-END-OF-FILE-SW

               MOVE PRM-COMPANY            TO DB-COMPANY
               MOVE BNGSET2-COMPANY        TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-BNGSET2
               INITIALIZE DB-EMPLOYEE
                          DB-EFFECT-DATE
                          DB-SEQ-NBR
               PERFORM 850-FIND-NLT-BNHSET2

               IF (BNGRPCHG-FOUND)
               OR ((BNCHANGE-FOUND)
               AND (BNH-COMPANY            = DB-COMPANY))
                   PERFORM 1900-SET-PROC-BNG-BNH-SW
                   THRU    1900-END

                   PERFORM 1100-SEL-COMPANY
                   THRU    1100-END
                       UNTIL (END-OF-FILE).

004170     GO TO 1000-END.
004180
006960******************************************************************
       1050-SEL-EMPLOYEE.
006960******************************************************************

           MOVE EMP-COMPANY            TO CRT-COMPANY.
           MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL.
006900     PERFORM 700-HR-EMP-SECURITY.
007000     IF (HRWS-EMP-SECURED)
               GO TO 1050-NEXT-EMPLOYEE.

005590     MOVE WS-FALSE               TO WS-END-OF-FILE-SW.

           MOVE PRM-COMPANY            TO DB-COMPANY.
           MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE BNGSET2-EMPLOYEE       TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-BNGSET2.
           INITIALIZE DB-EFFECT-DATE
                      DB-SEQ-NBR.
           PERFORM 850-FIND-NLT-BNHSET2.

           IF  (BNGRPCHG-NOTFOUND)
           AND ((BNCHANGE-NOTFOUND)
           OR   (BNH-COMPANY           NOT = DB-COMPANY)
           OR   (BNH-EMPLOYEE          NOT = DB-EMPLOYEE))
                GO TO 1050-NEXT-EMPLOYEE.

           PERFORM 1900-SET-PROC-BNG-BNH-SW
           THRU    1900-END.

           PERFORM 1100-SEL-COMPANY
           THRU    1100-END
               UNTIL (END-OF-FILE).

       1050-NEXT-EMPLOYEE.
           IF (PRM-PROC-LEVEL          NOT = SPACES)
           OR (PRM-PROC-GROUP          NOT = SPACES)
               PERFORM 860-FIND-NXTRNG-EMPSET7.

       1050-END.

006960******************************************************************
       1100-SEL-COMPANY.
006960******************************************************************

           INITIALIZE WS-PREV-CHANGE-DATE.

           IF (PROC-BNG)
               PERFORM 1200-SEL-BNGRPCHG
               THRU    1200-END
                   UNTIL (BNGRPCHG-NOTFOUND)
                   OR    (BNG-EMPLOYEE NOT = WS-EMPLOYEE)
                   OR    (PROC-BNH)
           ELSE
               PERFORM 1500-SEL-BNH-EMPLOYEE
               THRU    1500-END
                   UNTIL (BNCHANGE-NOTFOUND)
                   OR    (BNH-COMPANY  NOT = WS-COMPANY)
                   OR    (BNH-EMPLOYEE NOT = WS-EMPLOYEE)
                   OR    (PROC-BNG).

       1100-END.

006960******************************************************************
       1200-SEL-BNGRPCHG.
006960******************************************************************

           MOVE BNG-EFFECT-DATE            TO WS-EFFECT-DATE.

           IF (BNG-ADD-DELETE              = "D")
               PERFORM 1300-DELETE-GROUP
               THRU    1300-END
                   UNTIL (BNGRPCHG-NOTFOUND)
                   OR    (BNG-EMPLOYEE     NOT = WS-EMPLOYEE)
                   OR    (BNG-EFFECT-DATE  NOT = WS-EFFECT-DATE)
                   OR    (BNG-ADD-DELETE   NOT = "D")
                   OR    (PROC-BNH)
           ELSE
               PERFORM 1400-ADD-GROUP
               THRU    1400-END
                   UNTIL (BNGRPCHG-NOTFOUND)
                   OR    (BNG-EMPLOYEE     NOT = WS-EMPLOYEE)
                   OR    (BNG-EFFECT-DATE  NOT = WS-EFFECT-DATE)
                   OR    (BNG-ADD-DELETE   NOT = "A")
                   OR    (PROC-BNH).

       1200-END.

006960******************************************************************
       1300-DELETE-GROUP.
006960******************************************************************

J43365     IF (SELECT-COMPANY)
J43365          MOVE EMP-COMPANY            TO CRT-COMPANY
J43365          MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL
J43365          PERFORM 700-HR-EMP-SECURITY
J43365          IF (HRWS-EMP-SECURED)
J43365              GO TO 1300-NEXT-BNGRPCHG
J43365          END-IF
J43365     END-IF.
J43365
           SET DEL-GROUP-CHANGES       TO TRUE.

           IF (BNG-EFFECT-DATE         > PRM-THRU-DATE)
               GO TO 1300-NEXT-BNGRPCHG.

           SET PROC-OFF                TO TRUE.

P67618     IF (EMPLOYEE-NOTFOUND)
P67618         MOVE BNG-COMPANY        TO DB-COMPANY
P67618         MOVE BNG-EMPLOYEE       TO DB-EMPLOYEE
P67618         PERFORM 840-FIND-EMPSET1.
P67618
           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           MOVE BNG-COMPANY            TO DB-COMPANY.
           MOVE BNG-EMPLOYEE           TO DB-EMPLOYEE.
PERF       MOVE EFDSET5-EMPLOYEE       TO WS-DB-BEG-RNG.
PERF       MOVE "(EFD-STOP-DATE >= ?)" TO FILTER-STRING.
PERF       PERFORM 890-CREATE-FILTER.
PERF       MOVE BNG-EFFECT-DATE        TO DATETIME-FILTER-VALUE.
PERF       PERFORM 890-SET-DATETIME-FILTER-VALUE.
PERF       PERFORM 850-FILTER-BEGRNG-EFDSET5.
           PERFORM 1310-SEL-EFD-EMPLOYEE
           THRU    1310-END
PERF           UNTIL (EMPFLEXDOL-NOTFOUND)  
P74673         OR    (EFD-COMPANY      NOT = DB-COMPANY)
P74673         OR    (EFD-EMPLOYEE     NOT = DB-EMPLOYEE).
P74673* ADDED THESE LOOP CONDITIONS TO PREVENT INFINITE LOOPS

           MOVE BNG-COMPANY            TO DB-COMPANY.
           MOVE BNG-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE BENSET7-EMPLOYEE       TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-BENSET7.
           PERFORM 1340-SEL-REMAINING-BEN
           THRU    1340-END
               UNTIL (BENEFIT-NOTFOUND).

       1300-NEXT-BNGRPCHG.
           PERFORM 860-FIND-NXTRNG-BNGSET2.

           PERFORM 1900-SET-PROC-BNG-BNH-SW
           THRU    1900-END.

       1300-END.

006960******************************************************************
       1310-SEL-EFD-EMPLOYEE.
006960******************************************************************

           INITIALIZE WS-TERM-DATE
                      WS-CHANGE-DATE.

           MOVE EFD-FLEX-PLAN          TO WS-FLEX-PLAN.

           PERFORM 1312-SEL-EFD-FLEX-PLAN
           THRU    1312-END
               UNTIL (EMPFLEXDOL-NOTFOUND)
               OR    (EFD-COMPANY      NOT = DB-COMPANY)
               OR    (EFD-EMPLOYEE     NOT = DB-EMPLOYEE)
               OR    (EFD-FLEX-PLAN    NOT = WS-FLEX-PLAN).

       1310-END.

006960******************************************************************
       1312-SEL-EFD-FLEX-PLAN.
006960******************************************************************

PERF  *    IF (EFD-STOP-DATE           < BNG-EFFECT-DATE)
PERF  *        GO TO 1312-NEXT-EMPFLEXDOL.

           MOVE EFD-COMPANY            TO DB-COMPANY.
           MOVE EFD-FLEX-PLAN          TO DB-FLEX-PLAN.
           PERFORM 840-FIND-FLPSET1.
           IF (FLP-GROUP-NAME          = BNG-GROUP-NAME)
           OR (EFD-FLD-GROUP-NAME      = BNG-GROUP-NAME)
               GO TO 1312-PROCESS-EMPFLEXDOL.

           GO TO 1312-NEXT-EMPFLEXDOL.

       1312-PROCESS-EMPFLEXDOL.
           SET NO-ERROR-IN-EFD         TO TRUE.

           MOVE EFD-COMPANY            TO WF4-COMPANY.
           MOVE EFD-EMPLOYEE           TO WF4-EMPLOYEE.
           MOVE EFD-START-DATE         TO WF4-START-DATE.
           PERFORM 8400-FIND-BN100EFDC.
           IF (BN100EFDC-FOUND)
               IF (WF4-FUNCTION-CODE   = "D")
               OR (EFD-FLD-GROUP-NAME  = BNG-GROUP-NAME)
                   GO TO 1312-NEXT-EMPFLEXDOL
               END-IF
               MOVE "3"                TO WF-REC-TYPE
               MOVE WF4-WF-EFFECT-DATE TO WF-EFFECT-DATE
               MOVE WF4-WF-SEQ-NBR     TO WF-SEQ-NBR
               PERFORM 8300-DELETE-BN100WORK
               PERFORM 8300-DELETE-BN100EFDC

               MOVE EFD-COMPANY        TO WF3-COMPANY
               MOVE EFD-EMPLOYEE       TO WF3-EMPLOYEE

               MOVE WF4-STOP-DATE      TO WSDR-FR-DATE
               PERFORM 900-DATE-TO-JULIAN
               ADD 1                   TO WSDR-JULIAN-DAYS
               PERFORM 900-JULIAN-TO-DATE
               MOVE WSDR-FR-DATE       TO WF3-START-DATE
               PERFORM 8400-FIND-BN100EFDA
               IF (BN100EFDA-FOUND)
                   MOVE "3"                TO WF-REC-TYPE
                   MOVE WF3-WF-EFFECT-DATE TO WF-EFFECT-DATE
                   MOVE WF3-WF-SEQ-NBR     TO WF-SEQ-NBR
                   PERFORM 8300-DELETE-BN100WORK
                   PERFORM 8300-DELETE-BN100EFDA.

           SET NO-ERROR-FOR-CA         TO TRUE.

           SET PROC-EFD                TO TRUE.

           IF (FLP-GROUP-NAME          = BNG-GROUP-NAME)
      ******** FIND TERMINATION RULES FOR FLEX PLAN AND ACTION CODE
               SET TERM-RULES          TO TRUE
               PERFORM 7000-FIND-BWT-FOR-ACTION
               IF (ERROR-FOUND)
                   SET GWG-RULES-NOTFOUND  TO TRUE
                   MOVE CRT-ERROR-NBR      TO WS-ERROR-NBR
                   MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
                   MOVE CRT-ERR-VAR1       TO WS-ERR-VAR1
                   MOVE CRT-ERR-VAR2       TO WS-ERR-VAR2
                   MOVE CRT-ERR-VAR3       TO WS-ERR-VAR3
                   MOVE CRT-ERR-VAR4       TO WS-ERR-VAR4
                   MOVE CRT-ERR-VAR5       TO WS-ERR-VAR5

                   IF (CRT-ERROR-NBR       = 116 OR 117 OR 118)
                       INITIALIZE CRT-ERROR-NBR
                                  CRT-ERROR-CAT
                                  CRT-ERR-VAR1
                                  CRT-ERR-VAR2
                                  CRT-ERR-VAR3
                                  CRT-ERR-VAR4
                                  CRT-ERR-VAR5

                       PERFORM 1342-FIND-RULES-FOR-GWG
                       THRU    1342-END

                       IF (GWG-RULES-FOUND)
                           MOVE WS-TERM-DATE   TO WS-FLEX-TERM-DATE

                           NEXT SENTENCE
                       END-IF
                   END-IF
                   IF (GWG-RULES-NOTFOUND)
                       MOVE WS-ERROR-NBR   TO CRT-ERROR-NBR
                       MOVE WS-ERROR-CAT   TO CRT-ERROR-CAT
                       MOVE WS-ERR-VAR1    TO CRT-ERR-VAR1
                       MOVE WS-ERR-VAR2    TO CRT-ERR-VAR2
                       MOVE WS-ERR-VAR3    TO CRT-ERR-VAR3
                       MOVE WS-ERR-VAR4    TO CRT-ERR-VAR4
                       MOVE WS-ERR-VAR5    TO CRT-ERR-VAR5

                       PERFORM 1820-CREATE-ETEC-EFDC
                       THRU    1820-END
                   END-IF
                   SET ERROR-IN-EFD    TO TRUE
                   MOVE 164            TO CRT-ERROR-NBR
P15764             MOVE "BN100"        TO CRT-ERROR-CAT
                   INITIALIZE CRT-ERR-VAR1
                              CRT-ERR-VAR2
                              CRT-ERR-VAR3
                              CRT-ERR-VAR4
                              CRT-ERR-VAR5
                   GO TO 1312-CONTINUE
               END-IF
               MOVE WS-TERM-DATE       TO WS-FLEX-TERM-DATE
           ELSE
           IF (EFD-FLD-GROUP-NAME      = BNG-GROUP-NAME)
      ******** FIND CHANGE RULES FOR FLEX PLAN AND ACTION CODE
               SET CHANGE-RULES        TO TRUE
               PERFORM 7000-FIND-BWT-FOR-ACTION
               IF (ERROR-FOUND)
                   PERFORM 1820-CREATE-ETEC-EFDC
                   THRU    1820-END
                   SET ERROR-IN-EFD    TO TRUE
                   MOVE 164            TO CRT-ERROR-NBR
P15764             MOVE "BN100"        TO CRT-ERROR-CAT
                   INITIALIZE CRT-ERR-VAR1
                              CRT-ERR-VAR2
                              CRT-ERR-VAR3
                              CRT-ERR-VAR4
                              CRT-ERR-VAR5
                   GO TO 1312-CONTINUE.

           IF (TERM-RULES)
               IF (EFD-STOP-DATE       <= WS-TERM-DATE)
                   GO TO 1312-NEXT-EMPFLEXDOL
               END-IF
               IF (WS-TERM-DATE        < BNG-EFFECT-DATE)
                   PERFORM 1314-REPROCESS-EFDS
                   THRU    1314-END.

           IF (CHANGE-RULES)
               IF (EFD-STOP-DATE       < WS-CHANGE-DATE)
                   GO TO 1312-NEXT-EMPFLEXDOL
               END-IF
               IF (WS-CHANGE-DATE      < BNG-EFFECT-DATE)
                   PERFORM 1314-REPROCESS-EFDS
                   THRU    1314-END.

      *
      **** IF TERM DATE IS > EFD START DATE, DELETE EFD AND ASSOCIATED
      **** BENEFITS
      *
           IF (TERM-RULES)
               IF (EFD-START-DATE      >= WS-TERM-DATE)
      ************ GIVE MESSAGE TO MAKE CHANGES MANUALLY
                   IF (EFD-START-DATE  = WS-TERM-DATE)
                       MOVE 161        TO CRT-ERROR-NBR
                   ELSE
                       MOVE 163        TO CRT-ERROR-NBR
                   END-IF
P15764             MOVE "BN100"        TO CRT-ERROR-CAT
                   PERFORM 1820-CREATE-ETEC-EFDC
                   THRU    1820-END
                   SET ERROR-IN-EFD    TO TRUE
                   GO TO 1312-CONTINUE
               ELSE
                   PERFORM 1892-FIND-FUTURE-EFR
                   THRU    1892-END
                   IF (ERROR-IN-EFD)
                       GO TO 1312-CONTINUE
                   END-IF
      ************ STOP/ TERMINATE EFD AND ASSOCIATED BENS
                   PERFORM 1842-CREATE-S-EFDC
                   THRU    1842-END
               END-IF
           ELSE
           IF (CHANGE-RULES)
               IF (EFD-START-DATE      >= WS-CHANGE-DATE)
      ************ GIVE MESSAGE TO MAKE CHANGES MANUALLY
                   IF (EFD-START-DATE  = WS-CHANGE-DATE)
                       MOVE 160        TO CRT-ERROR-NBR
                   ELSE
                       MOVE 162        TO CRT-ERROR-NBR
                   END-IF
P15764             MOVE "BN100"        TO CRT-ERROR-CAT
                   PERFORM 1820-CREATE-ETEC-EFDC
                   THRU    1820-END
                   SET ERROR-IN-EFD    TO TRUE
                   GO TO 1312-CONTINUE
               ELSE
                   PERFORM 1892-FIND-FUTURE-EFR
                   THRU    1892-END
                   IF (ERROR-IN-EFD)
                       GO TO 1312-CONTINUE
                   END-IF
      ************ STOP AND RE-ADD EFD AND ASSOCIATED BENS
                   PERFORM 1846-CREATE-CS-EFDC
                   THRU    1846-END
               END-IF

               PERFORM 1313-FIND-FLD-FOR-CA
               THRU    1313-END

               IF (ERROR-FOUND)
                   SET ERROR-FOR-CA    TO TRUE
                   PERFORM 1825-CREATE-EA-EFDA
                   THRU    1825-END
P77775             SET NO-ERROR-FOR-CA TO TRUE
               ELSE
                   PERFORM 1848-CREATE-CA-EFDA
                   THRU    1848-END.

       1312-CONTINUE.
           MOVE EFD-COMPANY            TO DB-COMPANY.
           MOVE EFD-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE EFD-START-DATE         TO DB-EFD-START-DATE.
           MOVE BENSET7-EFD-START-DATE TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-BENSET7.

           PERFORM 1320-SEL-FLEX-BENEFIT
           THRU    1320-END
               UNTIL (BENEFIT-NOTFOUND).

           INITIALIZE CRT-ERROR-NBR
                      CRT-ERROR-CAT
                      CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3.

       1312-NEXT-EMPFLEXDOL.
PERF       PERFORM 860-FIND-NXTRNG-EFDSET5.

       1312-END.

006960******************************************************************
       1313-FIND-FLD-FOR-CA.
006960******************************************************************

      *
      **** FIND FLEX DOLLAR RECORD FOR NEW EFD
      *
           MOVE BNG-COMPANY            TO BNREWS-COMPANY.
           MOVE "FL"                   TO BNREWS-PLAN-TYPE.
           MOVE FLP-FLEX-PLAN          TO BNREWS-PLAN-CODE.
           MOVE BNG-EMPLOYEE           TO BNREWS-EMPLOYEE.
           MOVE EFD-START-DATE         TO BNREWS-AS-OF-DATE.
           IF (WS-CHANGE-DATE          > EFD-START-DATE)
               MOVE WS-CHANGE-DATE     TO BNREWS-AS-OF-DATE.
           MOVE "FLD"                  TO BNREWS-FILE-PREFIX.
           PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70.

       1313-END.

006960******************************************************************
       1314-REPROCESS-EFDS.
006960******************************************************************

           MOVE EFD-COMPANY            TO WS-SAVE-COMPANY.
           MOVE EFD-EMPLOYEE           TO WS-SAVE-EMPLOYEE.
           MOVE EFD-FLEX-PLAN          TO WS-SAVE-FLEX-PLAN.
           MOVE EFD-START-DATE         TO WS-SAVE-START-DATE.

           MOVE EFD-COMPANY            TO DB-COMPANY.
           MOVE EFD-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE EFD-FLEX-PLAN          TO DB-FLEX-PLAN.
PERF       MOVE EFDSET4-FLEX-PLAN      TO WS-DB-BEG-RNG.
PERF       IF (TERM-RULES)
PERF           MOVE "(EFD-STOP-DATE > ?)"
PERF                                   TO FILTER-STRING
PERF           PERFORM 890-CREATE-FILTER
PERF           MOVE WS-TERM-DATE       TO DATETIME-FILTER-VALUE
PERF       ELSE
PERF       IF (CHANGE-RULES)
PERF           MOVE "(EFD-STOP-DATE >= ?)"
PERF                                   TO FILTER-STRING
PERF           PERFORM 890-CREATE-FILTER
PERF          MOVE WS-CHANGE-DATE      TO DATETIME-FILTER-VALUE
PERF       END-IF
PERF       END-IF.
PERF       PERFORM 890-SET-DATETIME-FILTER-VALUE.
PERF       PERFORM 850-FILTER-BEGRNG-EFDSET4.
           PERFORM 1316-SEL-EFD-FLEX-PLAN
           THRU    1316-END
               UNTIL (EMPFLEXDOL-NOTFOUND)
               OR    (EFD-START-DATE   = WS-SAVE-START-DATE).

           MOVE WS-SAVE-COMPANY        TO DB-COMPANY.
           MOVE WS-SAVE-EMPLOYEE       TO DB-EMPLOYEE.
           MOVE WS-SAVE-FLEX-PLAN      TO DB-FLEX-PLAN.
           MOVE WS-SAVE-START-DATE     TO DB-START-DATE.
PERF  **   PERFORM 850-FIND-NLT-EFDSET4.

       1314-END.

006960******************************************************************
       1316-SEL-EFD-FLEX-PLAN.
006960******************************************************************

PERF  *    IF  (TERM-RULES)
PERF  *    AND (EFD-STOP-DATE          <= WS-TERM-DATE)
PERF  *        GO TO 1316-NEXT-EMPFLEXDOL.

PERF  *    IF  (CHANGE-RULES)
PERF  *    AND (EFD-STOP-DATE          < WS-CHANGE-DATE)
PERF  *        GO TO 1316-NEXT-EMPFLEXDOL.

           MOVE EFD-COMPANY            TO WF4-COMPANY.
           MOVE EFD-EMPLOYEE           TO WF4-EMPLOYEE.
           MOVE EFD-START-DATE         TO WF4-START-DATE.
           PERFORM 8400-FIND-BN100EFDC.
           IF (BN100EFDC-FOUND)
               GO TO 1316-NEXT-EMPFLEXDOL.

           SET PROC-EFD                TO TRUE.

           SET NO-ERROR-IN-EFD         TO TRUE.

           IF (TERM-RULES)
               IF (EFD-START-DATE      >= WS-TERM-DATE)
      ************ GIVE MESSAGE TO MAKE CHANGES MANUALLY
                   IF (EFD-START-DATE  = WS-TERM-DATE)
                       MOVE 161        TO CRT-ERROR-NBR
                   ELSE
                       MOVE 163        TO CRT-ERROR-NBR
                   END-IF
P15764             MOVE "BN100"        TO CRT-ERROR-CAT
                   PERFORM 1820-CREATE-ETEC-EFDC
                   THRU    1820-END
                   SET ERROR-IN-EFD    TO TRUE
                   GO TO 1316-CONTINUE
               ELSE
                   PERFORM 1892-FIND-FUTURE-EFR
                   THRU    1892-END
                   IF (ERROR-IN-EFD)
                       GO TO 1316-CONTINUE
                   END-IF
      ************ STOP/ TERMINATE EFD AND ASSOCIATED BENS
                   PERFORM 1842-CREATE-S-EFDC
                   THRU    1842-END
               END-IF
           ELSE
           IF (CHANGE-RULES)
               IF (EFD-START-DATE      = WS-CHANGE-DATE)
      ************ GIVE MESSAGE TO MAKE CHANGES MANUALLY
                   IF (EFD-START-DATE  = WS-CHANGE-DATE)
                       MOVE 160        TO CRT-ERROR-NBR
                   ELSE
                       MOVE 162        TO CRT-ERROR-NBR
                   END-IF
P15764             MOVE "BN100"        TO CRT-ERROR-CAT
                   PERFORM 1820-CREATE-ETEC-EFDC
                   THRU    1820-END
                   SET ERROR-IN-EFD    TO TRUE
                   GO TO 1316-CONTINUE
               ELSE
                   PERFORM 1892-FIND-FUTURE-EFR
                   THRU    1892-END
                   IF (ERROR-IN-EFD)
                       GO TO 1316-CONTINUE
                   END-IF
      ************ STOP AND RE-ADD EFD AND ASSOCIATED BENS
                   PERFORM 1846-CREATE-CS-EFDC
                   THRU    1846-END
               END-IF

               PERFORM 1313-FIND-FLD-FOR-CA
               THRU    1313-END

               IF (ERROR-FOUND)
                   SET ERROR-FOR-CA    TO TRUE
                   PERFORM 1825-CREATE-EA-EFDA
                   THRU    1825-END
P77775             SET NO-ERROR-FOR-CA TO TRUE
               ELSE
                   PERFORM 1848-CREATE-CA-EFDA
                   THRU    1848-END.

       1316-CONTINUE.
           MOVE EFD-COMPANY            TO DB-COMPANY.
           MOVE EFD-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE EFD-START-DATE         TO DB-EFD-START-DATE.
           MOVE BENSET7-EFD-START-DATE TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-BENSET7.

           PERFORM 1320-SEL-FLEX-BENEFIT
           THRU    1320-END
               UNTIL (BENEFIT-NOTFOUND).

           INITIALIZE CRT-ERROR-NBR
                      CRT-ERROR-CAT
                      CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3.

       1316-NEXT-EMPFLEXDOL.
PERF       PERFORM 860-FIND-NXTRNG-EFDSET4.

       1316-END.

006960******************************************************************
       1320-SEL-FLEX-BENEFIT.
006960******************************************************************

           IF (BEN-FLEX-PLAN           NOT = EFD-FLEX-PLAN)
               GO TO 1320-NEXT-BENEFIT.

           MOVE BEN-COMPANY            TO WF2-COMPANY.
           MOVE BEN-EMPLOYEE           TO WF2-EMPLOYEE.
           MOVE BEN-PLAN-TYPE          TO WF2-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO WF2-PLAN-CODE.
           MOVE BEN-START-DATE         TO WF2-START-DATE.
           PERFORM 8400-FIND-BN100BENC.
           IF (BN100BENC-FOUND)
               IF (WF2-FUNCTION-CODE   = "D")
                   GO TO 1320-NEXT-BENEFIT
               END-IF
               MOVE "3"                TO WF-REC-TYPE
               MOVE WF2-WF-EFFECT-DATE TO WF-EFFECT-DATE
               MOVE WF2-WF-SEQ-NBR     TO WF-SEQ-NBR
               PERFORM 8300-DELETE-BN100WORK
               PERFORM 8300-DELETE-BN100BENC

               MOVE BEN-COMPANY        TO WF1-COMPANY
               MOVE BEN-EMPLOYEE       TO WF1-EMPLOYEE
               MOVE BEN-PLAN-TYPE      TO WF1-PLAN-TYPE
               MOVE BEN-PLAN-CODE      TO WF1-PLAN-CODE

               MOVE WF2-STOP-DATE      TO WSDR-FR-DATE
               PERFORM 900-DATE-TO-JULIAN
               ADD 1                   TO WSDR-JULIAN-DAYS
               PERFORM 900-JULIAN-TO-DATE
               MOVE WSDR-FR-DATE       TO WF1-START-DATE
               PERFORM 8400-FIND-BN100BENA
               IF (BN100BENA-FOUND)
                   MOVE "3"                TO WF-REC-TYPE
                   MOVE WF1-WF-EFFECT-DATE TO WF-EFFECT-DATE
                   MOVE WF1-WF-SEQ-NBR     TO WF-SEQ-NBR
                   PERFORM 8300-DELETE-BN100WORK
                   PERFORM 8300-DELETE-BN100BENA.

           SET PROC-BEN                TO TRUE.

           IF  (ERROR-IN-EFD)
           AND (NO-ERROR-FOR-CA)
      ******** IF ERROR IN EFD, LIST ALL BENEFITS WITH ERROR
               PERFORM 1830-CREATE-ETEC-BENC
               THRU    1830-END
               GO TO 1320-NEXT-BENEFIT.

           MOVE BEN-COMPANY            TO DB-COMPANY.
           MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
           PERFORM 840-FIND-PLNSET1.

           IF (TERM-RULES)
               PERFORM 7000-FIND-BWT-FOR-ACTION
               IF (ERROR-FOUND)
                   MOVE WS-FLEX-TERM-DATE  TO WS-TERM-DATE
                   INITIALIZE CRT-ERROR-NBR
                              CRT-ERROR-CAT
                              CRT-ERR-VAR1
                              CRT-ERR-VAR2
                              CRT-ERR-VAR3
               END-IF
               IF (WS-TERM-DATE            > WS-FLEX-TERM-DATE)
                   MOVE WS-FLEX-TERM-DATE  TO WS-TERM-DATE.

           IF  (TERM-RULES)
           AND (BEN-STOP-DATE          <= WS-TERM-DATE)
               GO TO 1320-NEXT-BENEFIT.

           IF  (CHANGE-RULES)
           AND (BEN-STOP-DATE          < WS-CHANGE-DATE)
               GO TO 1320-NEXT-BENEFIT.

           IF (TERM-RULES)
               IF (WF4-FUNCTION-CODE   = "D")
                   PERFORM 1868-CHECK-YTD-N-HIPAA
                   THRU    1868-END

                   IF (ERROR-FOUND)
                       PERFORM 1832-CREATE-ED-BENC
                       THRU    1832-END
                       INITIALIZE CRT-ERROR-NBR
                                  CRT-ERROR-CAT
                                  CRT-ERR-VAR1
                                  CRT-ERR-VAR2
                                  CRT-ERR-VAR3
                   ELSE
                       PERFORM 1854-CREATE-D-BENC
                       THRU    1854-END
                   END-IF
               ELSE
                   PERFORM 1705-STOP-BENEFIT
                   THRU    1705-END
               END-IF
           ELSE
           IF (BEN-START-DATE          = WS-CHANGE-DATE)
               PERFORM 1652-CALL-BNBEN
               THRU    1652-END
           ELSE
               SET NO-RECOMPUTE-BEN        TO TRUE
               IF (PLN-CONTRIB-TYPE = "1" OR "2" OR "3" OR "4")
                   SET RECOMPUTE-BEN       TO TRUE
               END-IF
               PERFORM 1710-STOP-READD-BENEFIT
               THRU    1710-END.

       1320-NEXT-BENEFIT.
           PERFORM 860-FIND-NXTRNG-BENSET7.

       1320-END.

006960******************************************************************
       1340-SEL-REMAINING-BEN.
006960******************************************************************

           IF  (BEN-STOP-DATE          < BNG-EFFECT-DATE)
           AND (BEN-STOP-DATE          NOT = ZEROES)
               GO TO 1340-NEXT-BENEFIT.

           MOVE BEN-COMPANY            TO DB-COMPANY.
           MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
           PERFORM 840-FIND-PLNSET1.

           INITIALIZE WS-FLEX-FLAG.
           IF (PLN-FLEX-PLAN           NOT = SPACES)
               MOVE PLN-FLEX-PLAN      TO DB-FLEX-PLAN
               PERFORM 840-FIND-FLPSET1
               IF (FLP-SPEND-ONLY      = "N")
                   MOVE "Y"            TO WS-FLEX-FLAG.

           MOVE BEN-COMPANY            TO WF2-COMPANY.
           MOVE BEN-EMPLOYEE           TO WF2-EMPLOYEE.
           MOVE BEN-PLAN-TYPE          TO WF2-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO WF2-PLAN-CODE.
           MOVE BEN-START-DATE         TO WF2-START-DATE.
           PERFORM 8400-FIND-BN100BENC.
           IF  (BN100BENC-FOUND)
           AND (PLN-GROUP-NAME         = BNG-GROUP-NAME)
               IF  (WS-FLEX-FLAG       = "Y")
               AND (WF2-FUNCTION-CODE  = "ET" OR "EC")
                   GO TO 1340-NEXT-BENEFIT
               ELSE
P47575*----------- If the previous record is created thru flex and with
P47575*----------- no errors we need to skip finding term rules based on
P47575*----------- group defined on BN15 for that plan
P47575             IF  (WF2-FLEX-PLAN  NOT = SPACES)
P47575             AND (WF2-FUNCTION-CODE  = "S")
P47575                 GO TO 1340-NEXT-BENEFIT
P47575             END-IF
                   MOVE BEN-COMPANY        TO WF1-COMPANY
                   MOVE BEN-EMPLOYEE       TO WF1-EMPLOYEE
                   MOVE BEN-PLAN-TYPE      TO WF1-PLAN-TYPE
                   MOVE BEN-PLAN-CODE      TO WF1-PLAN-CODE
                   MOVE WF2-STOP-DATE      TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   ADD 1                   TO WSDR-JULIAN-DAYS
                   PERFORM 900-JULIAN-TO-DATE
                   MOVE WSDR-FR-DATE       TO WF1-START-DATE
                   PERFORM 8400-FIND-BN100BENA
                   IF (BN100BENA-FOUND)
                       MOVE "3"                TO WF-REC-TYPE
                       MOVE WF1-WF-EFFECT-DATE TO WF-EFFECT-DATE
                       MOVE WF1-WF-SEQ-NBR     TO WF-SEQ-NBR
                       PERFORM 8300-DELETE-BN100WORK
                       PERFORM 8300-DELETE-BN100BENA
                   END-IF
                   MOVE "3"                TO WF-REC-TYPE
                   MOVE WF2-WF-EFFECT-DATE TO WF-EFFECT-DATE
                   MOVE WF2-WF-SEQ-NBR     TO WF-SEQ-NBR
                   PERFORM 8400-FIND-BN100WORK
                   PERFORM 8300-DELETE-BN100WORK
                   PERFORM 8300-DELETE-BN100BENC
               END-IF
           ELSE
           IF (BN100BENC-FOUND)
               GO TO 1340-NEXT-BENEFIT.

           IF (PLN-GROUP-NAME          = BNG-GROUP-NAME)
               GO TO 1340-TERM-BENEFIT.

           IF (BEN-ELIG-GROUP          = BNG-GROUP-NAME)
           OR (BEN-COV-GROUP           = BNG-GROUP-NAME)
           OR (BEN-PREM-GROUP          = BNG-GROUP-NAME)
           OR (BEN-GL-GROUP            = BNG-GROUP-NAME)
P79673         IF (WS-RULE-TYPE-SW NOT = "T") 
                   GO TO 1340-CHANGE-BENEFIT.

           GO TO 1340-NEXT-BENEFIT.

       1340-TERM-BENEFIT.
           SET PROC-BEN                TO TRUE.

      *
      **** FIND TERM RULES FOR PLAN AND ACTION CODE
      *
           SET TERM-RULES              TO TRUE.
           PERFORM 7000-FIND-BWT-FOR-ACTION.
           IF (ERROR-FOUND)
               SET GWG-RULES-NOTFOUND  TO TRUE
               MOVE CRT-ERROR-NBR      TO WS-ERROR-NBR
               MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
               MOVE CRT-ERR-VAR1       TO WS-ERR-VAR1
               MOVE CRT-ERR-VAR2       TO WS-ERR-VAR2
               MOVE CRT-ERR-VAR3       TO WS-ERR-VAR3
               MOVE CRT-ERR-VAR4       TO WS-ERR-VAR4
               MOVE CRT-ERR-VAR5       TO WS-ERR-VAR5

               IF (CRT-ERROR-NBR       = 116 OR 117 OR 118)
                   INITIALIZE CRT-ERROR-NBR
                              CRT-ERROR-CAT
                              CRT-ERR-VAR1
                              CRT-ERR-VAR2
                              CRT-ERR-VAR3
                              CRT-ERR-VAR4
                              CRT-ERR-VAR5

                   PERFORM 1342-FIND-RULES-FOR-GWG
                   THRU    1342-END

                   IF (GWG-RULES-FOUND)
                       NEXT SENTENCE
                   END-IF
               END-IF
               IF (GWG-RULES-NOTFOUND)
                   MOVE WS-ERROR-NBR   TO CRT-ERROR-NBR
                   MOVE WS-ERROR-CAT   TO CRT-ERROR-CAT
                   MOVE WS-ERR-VAR1    TO CRT-ERR-VAR1
                   MOVE WS-ERR-VAR2    TO CRT-ERR-VAR2
                   MOVE WS-ERR-VAR3    TO CRT-ERR-VAR3
                   MOVE WS-ERR-VAR4    TO CRT-ERR-VAR4
                   MOVE WS-ERR-VAR5    TO CRT-ERR-VAR5

                   PERFORM 1830-CREATE-ETEC-BENC
                   THRU    1830-END
               END-IF
               GO TO 1340-CONTINUE.

           IF  (BN100BENC-FOUND)
           AND (WF2-STOP-DATE          <= WS-TERM-DATE)
           AND (WF2-STOP-DATE          NOT = ZEROES)
               MOVE "S"                TO WF-FUNCTION-CODE
                                          WF2-FUNCTION-CODE
               INITIALIZE WF-ERROR-NBR
                          WF-WARNING-NBR
                          WF-ERROR-CAT
                          WF-ERR-VAR1
               
               SET BENC-DATA           TO TRUE

               PERFORM 8200-STORE-BN100WORK
               PERFORM 8200-STORE-BN100BENC
               GO TO 1340-NEXT-BENEFIT.

           IF  (BEN-STOP-DATE          <= WS-TERM-DATE)
           AND (BEN-STOP-DATE          NOT = ZEROES)
               GO TO 1340-NEXT-BENEFIT.

           IF (BEN-START-DATE          > WS-TERM-DATE)
               PERFORM 1868-CHECK-YTD-N-HIPAA
               THRU    1868-END

               IF (ERROR-FOUND)
                   PERFORM 1832-CREATE-ED-BENC
                   THRU    1832-END
               ELSE
                   PERFORM 1854-CREATE-D-BENC
                   THRU    1854-END
               END-IF
           ELSE
               PERFORM 1705-STOP-BENEFIT
               THRU    1705-END.

           GO TO 1340-CONTINUE.

       1340-CHANGE-BENEFIT.
           SET PROC-BEN                TO TRUE.

      *
      **** FIND CHANGE RULES FOR PLAN AND ACTION CODE
      *
           SET CHANGE-RULES            TO TRUE.
           PERFORM 7000-FIND-BWT-FOR-ACTION.
           IF (ERROR-FOUND)
               PERFORM 1830-CREATE-ETEC-BENC
               THRU    1830-END
               GO TO 1340-CONTINUE.

           IF  (BEN-STOP-DATE          < WS-CHANGE-DATE)
           AND (BEN-STOP-DATE          NOT = ZEROES)
               GO TO 1340-NEXT-BENEFIT.

           IF (BEN-START-DATE          >= WS-CHANGE-DATE)
               PERFORM 1652-CALL-BNBEN
               THRU    1652-END
           ELSE
               SET NO-RECOMPUTE-BEN        TO TRUE
               IF (PLN-CONTRIB-TYPE = "1" OR "2" OR "3" OR "4")
                   SET RECOMPUTE-BEN       TO TRUE
               END-IF
               PERFORM 1710-STOP-READD-BENEFIT
               THRU    1710-END.

       1340-CONTINUE.
           INITIALIZE CRT-ERROR-NBR
                      CRT-ERROR-CAT
                      CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3 
J42548                WS-RULE-TYPE-SW.

       1340-NEXT-BENEFIT.
           PERFORM 860-FIND-NXTRNG-BENSET7.

       1340-END.

006960******************************************************************
       1342-FIND-RULES-FOR-GWG.
006960******************************************************************

      **** IF TERM RULES NOT FOUND FOR GROUP TRY TO FIND RULE FOR ****
      **** GROUP WITHIN GROUP                                     ****

           MOVE BNG-EFFECT-DATE        TO WS-SV-EFFECT-DATE.
           MOVE BNG-SEQ-NBR            TO WS-SV-SEQ-NBR.

           MOVE "D"                    TO WS-ADD-DELETE.

           MOVE WS-HIGH-VALUES         TO WS-SV-TERM-DATE.

           MOVE PRM-COMPANY            TO DB-COMPANY.
           MOVE WS-GWG-FLD-NBR         TO DB-FLD-NBR.
           MOVE BNG-GROUP-NAME         TO DB-GROUP-NAME.
           MOVE PGSSET4-GROUP-NAME     TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PGSSET4.
           PERFORM
               UNTIL (PGSELECT-NOTFOUND)

P48592         MOVE EMP-EMPLOYEE       TO DB-EMPLOYEE
               MOVE WS-SV-EFFECT-DATE  TO DB-EFFECT-DATE
               MOVE WS-ADD-DELETE      TO DB-ADD-DELETE
               MOVE PGS-BEGIN-VALUE    TO DB-GROUP-NAME
               PERFORM 840-FIND-BNGSET4
               IF (BNGRPCHG-FOUND)
                   PERFORM 7000-FIND-BWT-FOR-ACTION
                   IF  (NO-ERROR-FOUND)
                   AND (WS-TERM-DATE   < WS-SV-TERM-DATE)
                       MOVE WS-TERM-DATE TO WS-SV-TERM-DATE
                   END-IF
               END-IF

               PERFORM 860-FIND-NXTRNG-PGSSET4
           END-PERFORM.

           IF (WS-SV-TERM-DATE         NOT = 99999999)
               SET GWG-RULES-FOUND     TO TRUE
               MOVE WS-SV-TERM-DATE    TO WS-TERM-DATE.

           MOVE EMP-COMPANY            TO DB-COMPANY.
           MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE WS-SV-EFFECT-DATE      TO DB-EFFECT-DATE.
           MOVE WS-SV-SEQ-NBR          TO DB-SEQ-NBR.
           PERFORM 840-FIND-BNGSET1.

       1342-END.

006960******************************************************************
       1400-ADD-GROUP.
006960******************************************************************

J43365     IF (SELECT-COMPANY)
J43365          MOVE EMP-COMPANY            TO CRT-COMPANY
J43365          MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL
J43365          PERFORM 700-HR-EMP-SECURITY
J43365          IF (HRWS-EMP-SECURED)
J43365              GO TO 1400-NEXT-BNGRPCHG
J43365          END-IF
J43365     END-IF.
J43365
           IF (WS-EFFECT-DATE          > PRM-THRU-DATE)
               GO TO 1400-NEXT-BNGRPCHG.

           INITIALIZE WS-ADD-DATE.

           IF (PROC-BNH)
               SET NEW-HIRE-CHANGES    TO TRUE
           ELSE
               SET ADD-GROUP-CHANGES   TO TRUE.

           SET PROC-OFF                TO TRUE.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           IF (NEW-HIRE-CHANGES)
               GO TO 1400-PROCESS-NEW.

           MOVE EMP-COMPANY            TO DB-COMPANY.
           MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE EFDSET4-EMPLOYEE       TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-EFDSET4.
           PERFORM 1402-SEL-EFD-EMPLOYEE
           THRU    1402-END
               UNTIL (EMPFLEXDOL-NOTFOUND).

           MOVE EMP-COMPANY            TO DB-COMPANY.
           MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE BENSET7-EMPLOYEE       TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-BENSET7.
           PERFORM 1408-SEL-BENEFIT
           THRU    1408-END
               UNTIL (BENEFIT-NOTFOUND).

       1400-PROCESS-NEW.
           SET PROC-FLEX               TO TRUE.

           PERFORM 1410-SEL-EMPFLEXDOL
           THRU    1410-END.

           SET PROC-NON-FLEX           TO TRUE.

           MOVE EMP-COMPANY            TO DB-COMPANY.
           INITIALIZE DB-FLEX-PLAN.
           MOVE PLNSET6-FLEX-PLAN      TO WS-DB-BEG-RNG.
PERF       IF  (ADD-GROUP-CHANGES)
PERF           STRING
P74069         "((PLN-DEFAULT = ?) OR (PLN-DEFAULT = ?) OR "
J79757         "(PLN-DEFAULT = ?) OR "
P74069         "(PLN-DEFAULT = ?)) AND ((PLN-GROUP-NAME = ?) OR "
P74069         "(PLN-GROUP-NAME = ?))"
P74069*        "(PLN-DEFAULT = ?) OR (PLN-DEFAULT = ?) OR "
P74069*        "(PLN-DEFAULT = ?) AND ((PLN-GROUP-NAME = ?) OR "
P74069*        "(PLN-GROUP-NAME = ?))"
PERF           DELIMITED BY SIZE INTO FILTER-STRING
PERF           PERFORM 890-CREATE-FILTER
PERF           MOVE "T"                TO ALPHANUM-FILTER-VALUE
PERF           PERFORM 890-SET-ALPHANUM-FILTER-VALUE
PERF           MOVE "O"                TO ALPHANUM-FILTER-VALUE
PERF           PERFORM 890-SET-ALPHANUM-FILTER-VALUE
PERF           MOVE "A"                TO ALPHANUM-FILTER-VALUE
J79757         PERFORM 890-SET-ALPHANUM-FILTER-VALUE
J79757         MOVE "N"                TO ALPHANUM-FILTER-VALUE
PERF           PERFORM 890-SET-ALPHANUM-FILTER-VALUE
PERF           MOVE SPACES             TO ALPHANUM-FILTER-VALUE
PERF           PERFORM 890-SET-ALPHANUM-FILTER-VALUE
PERF           MOVE BNG-GROUP-NAME     TO ALPHANUM-FILTER-VALUE
PERF           PERFORM 890-SET-ALPHANUM-FILTER-VALUE
PERF           PERFORM 850-FILTER-BEGRNG-PLNSET6
PERF       ELSE
           PERFORM 850-FIND-BEGRNG-PLNSET6.

           PERFORM 1414-SEL-PLAN
           THRU    1414-END
               UNTIL (PLAN-NOTFOUND).

       1400-NEXT-BNGRPCHG.
           IF (PROC-BNH)
               PERFORM 860-FIND-NEXT-BNHSET2
           ELSE
               PERFORM 860-FIND-NXTRNG-BNGSET2.

           PERFORM 1900-SET-PROC-BNG-BNH-SW
           THRU    1900-END.

       1400-END.

006960******************************************************************
       1402-SEL-EFD-EMPLOYEE.
006960******************************************************************

           MOVE EFD-FLEX-PLAN          TO WS-FLEX-PLAN.

           PERFORM 1404-SEL-EFD-FLEX-PLAN
           THRU    1404-END
               UNTIL (EMPFLEXDOL-NOTFOUND)
               OR    (EFD-FLEX-PLAN    NOT = WS-FLEX-PLAN).

       1402-END.

006960******************************************************************
       1404-SEL-EFD-FLEX-PLAN.
006960******************************************************************

           IF (EFD-STOP-DATE           < WS-EFFECT-DATE)
           OR (EFD-FLD-GROUP-NAME      NOT = SPACES)
               GO TO 1404-NEXT-EMPFLEXDOL.

           MOVE EFD-COMPANY            TO WF4-COMPANY.
           MOVE EFD-EMPLOYEE           TO WF4-EMPLOYEE.
           MOVE EFD-START-DATE         TO WF4-START-DATE.
           PERFORM 8400-FIND-BN100EFDC.
           IF (BN100EFDC-FOUND)
               GO TO 1404-NEXT-EMPFLEXDOL.

           SET NO-ERROR-IN-EFD         TO TRUE.

           MOVE EFD-COMPANY            TO BNREWS-COMPANY.
           MOVE "FL"                   TO BNREWS-PLAN-TYPE.
           MOVE EFD-FLEX-PLAN          TO BNREWS-PLAN-CODE.
           MOVE EFD-EMPLOYEE           TO BNREWS-EMPLOYEE.
           MOVE BNG-EFFECT-DATE        TO BNREWS-AS-OF-DATE.
           MOVE "FLD"                  TO BNREWS-FILE-PREFIX.
           PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70.
           IF (ERROR-FOUND)
           OR (FLD-GROUP-NAME          = EFD-FLD-GROUP-NAME)
               GO TO 1404-CONTINUE
           ELSE
               MOVE FLD-START-DATE     TO WS-FLD-START-DATE
               MOVE FLD-GROUP-NAME     TO WS-FLD-GROUP-NAME.

       1404-PROCESS-EMPFLEXDOL.
           SET PROC-EFD                TO TRUE.

      *
      **** FIND CHANGE RULES FOR FLEX PLAN AND ACTION CODE
      *
           SET CHANGE-RULES            TO TRUE.
           PERFORM 7000-FIND-BWT-FOR-ACTION.
           IF (ERROR-FOUND)
               PERFORM 1820-CREATE-ETEC-EFDC
               THRU    1820-END
               SET ERROR-IN-EFD        TO TRUE
               MOVE 164                TO CRT-ERROR-NBR
P15764         MOVE "BN100"            TO CRT-ERROR-CAT
               INITIALIZE CRT-ERR-VAR1
                          CRT-ERR-VAR2
                          CRT-ERR-VAR3
                          CRT-ERR-VAR4
                          CRT-ERR-VAR5
               GO TO 1404-PROCESS-BENEFIT.

           IF (EFD-STOP-DATE           < WS-CHANGE-DATE)
               GO TO 1404-NEXT-EMPFLEXDOL.

           IF (EFD-START-DATE          >= WS-CHANGE-DATE)
      ******** GIVE MESSAGE TO MAKE CHANGES MANUALLY
               IF (EFD-START-DATE      = WS-CHANGE-DATE)
                   MOVE 160            TO CRT-ERROR-NBR
               ELSE
                   MOVE 162            TO CRT-ERROR-NBR
               END-IF
P15764         MOVE "BN100"            TO CRT-ERROR-CAT
               PERFORM 1820-CREATE-ETEC-EFDC
               THRU    1820-END
               SET ERROR-IN-EFD        TO TRUE
               GO TO 1404-PROCESS-BENEFIT
           ELSE
               PERFORM 1892-FIND-FUTURE-EFR
               THRU    1892-END
               IF (ERROR-IN-EFD)
                   GO TO 1404-PROCESS-BENEFIT
               END-IF
               PERFORM 1846-CREATE-CS-EFDC
               THRU    1846-END

               PERFORM 1848-CREATE-CA-EFDA
               THRU    1848-END.

       1404-PROCESS-BENEFIT.
           MOVE EFD-COMPANY            TO DB-COMPANY.
           MOVE EFD-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE EFD-START-DATE         TO DB-EFD-START-DATE.
           MOVE BENSET7-EFD-START-DATE TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-BENSET7.

           PERFORM 1406-SEL-FLEX-BENEFIT
           THRU    1406-END
               UNTIL (BENEFIT-NOTFOUND).

       1404-CONTINUE.
           INITIALIZE CRT-ERROR-NBR
                      CRT-ERROR-CAT
                      CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3.

       1404-NEXT-EMPFLEXDOL.
           PERFORM 860-FIND-NXTRNG-EFDSET4.

       1404-END.

006960******************************************************************
       1406-SEL-FLEX-BENEFIT.
006960******************************************************************

           IF (BEN-FLEX-PLAN           NOT = EFD-FLEX-PLAN)
               GO TO 1406-NEXT-BENEFIT.

           MOVE BEN-COMPANY            TO WF2-COMPANY.
           MOVE BEN-EMPLOYEE           TO WF2-EMPLOYEE.
           MOVE BEN-PLAN-TYPE          TO WF2-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO WF2-PLAN-CODE.
           MOVE BEN-START-DATE         TO WF2-START-DATE.
           PERFORM 8400-FIND-BN100BENC.
           IF (BN100BENC-FOUND)
               GO TO 1406-NEXT-BENEFIT.

           SET PROC-BEN                TO TRUE.

           IF (ERROR-IN-EFD)
      ******** IF ERROR IN EFD, LIST ALL BENEFITS WITH ERROR
               PERFORM 1830-CREATE-ETEC-BENC
               THRU    1830-END
               GO TO 1406-NEXT-BENEFIT.

           IF (BEN-STOP-DATE           < WS-CHANGE-DATE)
               GO TO 1406-NEXT-BENEFIT.

           MOVE BEN-COMPANY            TO DB-COMPANY.
           MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
           PERFORM 840-FIND-PLNSET1.

           MOVE BEN-COMPANY            TO DB-COMPANY.
           MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
           MOVE "E"                    TO DB-COVER-TYPE.
           MOVE BEN-COV-UPD-DT         TO DB-START-DATE.
           MOVE BEN-COV-GROUP          TO DB-GROUP-NAME.
           PERFORM 840-FIND-CVRSET1.

      *
      **** PRE-CALC-TYPE "3" IS PERCENT OF SALARY
      **** PRE-TABLE-TYPE "2" IS SALARY RATE TABLE
      **** PRE-CONTRIB-TYPE "4" IS CONTRIBUTION BASED ON COVERAGE
      **** OR SALARY
      *
           MOVE BEN-COMPANY            TO DB-COMPANY.
           MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
           MOVE BEN-PREM-UPD-DT        TO DB-START-DATE.
           MOVE BEN-PREM-GROUP         TO DB-GROUP-NAME.
           PERFORM 840-FIND-PRESET1.

           SET NO-RECOMPUTE-BEN        TO TRUE.

           IF  ((((PRE-CALC-TYPE       = "1")
           OR    (PRE-CALC-TYPE        = "2")
           OR    (PRE-CALC-TYPE        = "3")
           OR    (PRE-TABLE-TYPE       = "2")
           OR    ((PRE-CONTRIB-TYPE    = "4")
           AND    (PRE-CONTRIB-BASIS   = "S"))))
P60050     OR  (CVR-CALC-TYPE          = "M" OR "P" OR "N"))
           AND (BEN-COV-OVER-FLG       NOT = "Y")
               SET RECOMPUTE-BEN       TO TRUE.

           IF (BEN-START-DATE          = WS-CHANGE-DATE)
               PERFORM 1652-CALL-BNBEN
               THRU    1652-END
           ELSE
               IF (PLN-CONTRIB-TYPE = "1" OR "2" OR "3" OR "4")
                   SET RECOMPUTE-BEN       TO TRUE
               END-IF
               PERFORM 1710-STOP-READD-BENEFIT
               THRU    1710-END.

       1406-NEXT-BENEFIT.
           PERFORM 860-FIND-NXTRNG-BENSET7.

       1406-END.

006960******************************************************************
       1408-SEL-BENEFIT.
006960******************************************************************

           IF  (BEN-STOP-DATE          < BNG-EFFECT-DATE)
           AND (BEN-STOP-DATE          NOT = ZEROES)
               GO TO 1408-NEXT-BENEFIT.

           MOVE BEN-COMPANY            TO DB-COMPANY.
           MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
           PERFORM 840-FIND-PLNSET1.

           MOVE BEN-COMPANY            TO WF2-COMPANY.
           MOVE BEN-EMPLOYEE           TO WF2-EMPLOYEE.
           MOVE BEN-PLAN-TYPE          TO WF2-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO WF2-PLAN-CODE.
           MOVE BEN-START-DATE         TO WF2-START-DATE.
           PERFORM 8400-FIND-BN100BENC.
           IF (BN100BENC-FOUND)
               GO TO 1408-NEXT-BENEFIT.

           MOVE BEN-COMPANY            TO BNREWS-COMPANY.
           MOVE BEN-PLAN-TYPE          TO BNREWS-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO BNREWS-PLAN-CODE.
           MOVE "E"                    TO BNREWS-COVER-TYPE.
           MOVE BEN-EMPLOYEE           TO BNREWS-EMPLOYEE.
           MOVE WS-EFFECT-DATE         TO BNREWS-AS-OF-DATE.
           INITIALIZE BNREWS-RULE-TYPE.
           INITIALIZE BNWS-SCR-START-DATE
                      BNWS-SCR-STOP-DATE
                      BNWS-START-DATE
                      BNWS-STOP-DATE.
       1408-ELIGIBILITY-GROUP.
           IF (BEN-ELIG-GROUP          = SPACES)
               MOVE "BWT"              TO BNREWS-FILE-PREFIX
               PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70
               IF (ERROR-FOUND)
                   GO TO 1408-CONTINUE
               ELSE
               IF (BWT-GROUP-NAME      = BEN-ELIG-GROUP)
                   GO TO 1408-COVERAGE
               ELSE
               IF  (BWT-GROUP-NAME     = BNG-GROUP-NAME)
               AND (BWT-GROUP-NAME     NOT = BEN-ELIG-GROUP)
                   GO TO 1408-PROCESS-BENEFIT.

       1408-COVERAGE.
      *    IF (BEN-COV-GROUP           = SPACES)
           IF (PLN-COVERAGE-TYPE       NOT = "0")
               IF (PLN-COVERAGE-TYPE   = "1")
                   MOVE "BCD"          TO BNREWS-FILE-PREFIX
                   MOVE "Y"            TO BNREWS-FIND-ST-GN
               ELSE
                   MOVE "CVR"          TO BNREWS-FILE-PREFIX
               END-IF
               PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70
               IF (ERROR-FOUND)
                   GO TO 1408-CONTINUE
               ELSE
               IF  ((PLN-COVERAGE-TYPE = "1")
               AND  (BCD-GROUP-NAME    = BEN-COV-GROUP))
               OR  ((PLN-COVERAGE-TYPE = "2")
               AND  (CVR-GROUP-NAME    = BEN-COV-GROUP))
                   GO TO 1408-CONTRIBUTION
               ELSE
               IF  (((PLN-COVERAGE-TYPE = "1")
               AND   (BCD-GROUP-NAME    = BNG-GROUP-NAME))
               OR   ((PLN-COVERAGE-TYPE = "2")
               AND   (CVR-GROUP-NAME    = BNG-GROUP-NAME)))
               AND (((PLN-COVERAGE-TYPE = "1")
               AND   (BCD-GROUP-NAME    NOT = BEN-COV-GROUP))
               OR   ((PLN-COVERAGE-TYPE = "2")
               AND   (CVR-GROUP-NAME    NOT = BEN-COV-GROUP)))
                   GO TO 1408-PROCESS-BENEFIT.

       1408-CONTRIBUTION.
      *    IF (BEN-PREM-GROUP          = SPACES)
               MOVE "PRE"              TO BNREWS-FILE-PREFIX.
               PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70.
               IF (ERROR-FOUND)
                   GO TO 1408-CONTINUE
               ELSE
               IF (PRE-GROUP-NAME      = BEN-PREM-GROUP)
                   GO TO 1408-GL-OVERRIDE
               ELSE
               IF  (PRE-GROUP-NAME     = BNG-GROUP-NAME)
               AND (PRE-GROUP-NAME     NOT = BEN-PREM-GROUP)
                   GO TO 1408-PROCESS-BENEFIT.

       1408-GL-OVERRIDE.
      *    IF (BEN-GL-GROUP            = SPACES)
               MOVE "BNA"              TO BNREWS-FILE-PREFIX.
               PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70.
               IF (ERROR-FOUND)
               OR (BNREWS-BNACCOUNTS-SW NOT = "Y")
                   GO TO 1408-CONTINUE
               ELSE
               IF (BNA-GROUP-NAME      = BEN-GL-GROUP)
                   GO TO 1408-NEXT-BENEFIT
               ELSE
               IF  (BNA-GROUP-NAME     = BNG-GROUP-NAME)
               AND (BNA-GROUP-NAME     NOT = BEN-GL-GROUP)
                   GO TO 1408-PROCESS-BENEFIT.

           GO TO 1408-NEXT-BENEFIT.

       1408-PROCESS-BENEFIT.
           SET PROC-BEN                TO TRUE.

      *
      **** FIND CHANGE RULES FOR PLAN AND ACTION CODE
      *
           SET CHANGE-RULES            TO TRUE.
           PERFORM 7000-FIND-BWT-FOR-ACTION.
           IF (ERROR-FOUND)
               PERFORM 1830-CREATE-ETEC-BENC
               THRU    1830-END
               GO TO 1408-CONTINUE.

           MOVE BEN-COMPANY            TO WF1-COMPANY.
           MOVE BEN-EMPLOYEE           TO WF1-EMPLOYEE.
           MOVE BEN-PLAN-TYPE          TO WF1-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO WF1-PLAN-CODE.
           MOVE WS-CHANGE-DATE         TO WF1-START-DATE.
           PERFORM 8400-FIND-BN100BENA.
           IF (BN100BENA-FOUND)
               GO TO 1408-CONTINUE.

           INITIALIZE WS-STOP-DATE.

           IF (BN100BENC-FOUND)
               IF (WS-CHANGE-DATE      < WF2-STOP-DATE)
                   MOVE WF2-STOP-DATE  TO WS-STOP-DATE

                   MOVE WS-CHANGE-DATE TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   SUBTRACT 1          FROM WSDR-JULIAN-DAYS
                   PERFORM 900-JULIAN-TO-DATE
                   MOVE WSDR-FR-DATE   TO WF2-STOP-DATE
                   PERFORM 8200-REWRITE-BN100BENC

                   SET CREATE-ONLY-BENA TO TRUE
               ELSE
               IF (WF2-STOP-DATE       = ZEROES)
                   GO TO 1408-CONTINUE
               ELSE
                   MOVE WF2-STOP-DATE  TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   ADD 1               TO WSDR-JULIAN-DAYS
                   PERFORM 900-JULIAN-TO-DATE
                   MOVE WSDR-FR-DATE   TO WF1-START-DATE
                   PERFORM 8400-FIND-BN100BENA

                   MOVE WS-CHANGE-DATE TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   SUBTRACT 1          FROM WSDR-JULIAN-DAYS
                   PERFORM 900-JULIAN-TO-DATE
                   MOVE WSDR-FR-DATE   TO WF1-STOP-DATE

                   PERFORM 8200-REWRITE-BN100BENA

                   SET CREATE-ONLY-BENA TO TRUE.

           IF  (BEN-STOP-DATE          < WS-CHANGE-DATE)
           AND (BEN-STOP-DATE          NOT = ZEROES)
               GO TO 1408-NEXT-BENEFIT.

           MOVE BEN-COMPANY            TO DB-COMPANY.
           MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
           MOVE "E"                    TO DB-COVER-TYPE.
           MOVE BEN-COV-UPD-DT         TO DB-START-DATE.
           MOVE BEN-COV-GROUP          TO DB-GROUP-NAME.
           PERFORM 840-FIND-CVRSET1.

      *
      **** PRE-CALC-TYPE "3" IS PERCENT OF SALARY
      **** PRE-TABLE-TYPE "2" IS SALARY RATE TABLE
      **** PRE-CONTRIB-TYPE "4" IS CONTRIBUTION BASED ON COVERAGE
      **** OR SALARY
      *
           MOVE BEN-COMPANY            TO DB-COMPANY.
           MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
           MOVE BEN-PREM-UPD-DT        TO DB-START-DATE.
           MOVE BEN-PREM-GROUP         TO DB-GROUP-NAME.
           PERFORM 840-FIND-PRESET1.

           SET NO-RECOMPUTE-BEN        TO TRUE.

           IF  ((((PRE-CALC-TYPE       = "1")
           OR    (PRE-CALC-TYPE        = "2")
           OR    (PRE-CALC-TYPE        = "3")
           OR    (PRE-TABLE-TYPE       = "2")
           OR    ((PRE-CONTRIB-TYPE    = "4")
           AND    (PRE-CONTRIB-BASIS   = "S"))))
P60050     OR  (CVR-CALC-TYPE          = "M" OR "P" OR "N"))
           AND (BEN-COV-OVER-FLG       NOT = "Y")
               SET RECOMPUTE-BEN       TO TRUE.

           IF (BEN-START-DATE          >= WS-CHANGE-DATE)
               PERFORM 1652-CALL-BNBEN
               THRU    1652-END
           ELSE
               IF (PLN-CONTRIB-TYPE = "1" OR "2" OR "3" OR "4")
                   SET RECOMPUTE-BEN       TO TRUE
               END-IF
               PERFORM 1710-STOP-READD-BENEFIT
               THRU    1710-END.

       1408-CONTINUE.
           INITIALIZE CRT-ERROR-NBR
                      CRT-ERROR-CAT
                      CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3.

           SET NO-CREATE-ONLY-BENA     TO TRUE.

       1408-NEXT-BENEFIT.
           PERFORM 860-FIND-NXTRNG-BENSET7.

       1408-END.

006960******************************************************************
       1410-SEL-EMPFLEXDOL.
006960******************************************************************

           SET PROC-EFD                TO TRUE.

      *
      **** FIND FLEX PLAN USING EMP GROUP LOGIC
      *
           MOVE EMP-COMPANY            TO BNFFWS-COMPANY.
           MOVE EMP-EMPLOYEE           TO BNFFWS-EMPLOYEE.
           PERFORM 5000-FIND-FLEXPLAN-70.
           IF  (ERROR-FOUND)
               GO TO 1410-CONTINUE
J31822     END-IF.

           IF  (ADD-GROUP-CHANGES)
           AND (FLP-GROUP-NAME         NOT = SPACES)
           AND (FLP-GROUP-NAME         NOT = BNG-GROUP-NAME)
               GO TO 1410-END.

           IF (NEW-HIRE-CHANGES)
027900         MOVE EMP-COMPANY        TO BNEDWS-COMPANY
028000         MOVE "FL"               TO BNEDWS-PLAN-TYPE
028100         MOVE FLP-FLEX-PLAN      TO BNEDWS-PLAN-CODE
028200         MOVE EMP-EMPLOYEE       TO BNEDWS-EMPLOYEE
028300         MOVE WS-EFFECT-DATE     TO BNEDWS-AS-OF-DATE
P71977         INITIALIZE BNEDWS-FROM-MAGIC-SW    
028400         PERFORM 5000-ELIGIBILITY-DATE-CALC-70
085400         IF (ERROR-FOUND)
                   PERFORM 1825-CREATE-EA-EFDA
                   THRU    1825-END
                   GO TO 1410-CONTINUE
               ELSE
086000             MOVE BNEDWS-ELIGIBILITY-DATE
                                       TO WS-ADD-DATE
                   SET ADD-RULES       TO TRUE
                   GO TO 1410-PROCEED.

028500         IF (ERROR-FOUND)
028600             GO TO 1410-CONTINUE.
028700
      *
      **** FIND ADD RULES FOR FLEX PLAN
      *
           SET ADD-RULES               TO TRUE.
           MOVE FLP-FLEX-PLAN          TO EFD-FLEX-PLAN.
           PERFORM 7000-FIND-BWT-FOR-ACTION.
           IF (ERROR-FOUND)
               PERFORM 1825-CREATE-EA-EFDA
               THRU    1825-END
               GO TO 1410-CONTINUE.

       1410-PROCEED.
           MOVE EMP-COMPANY            TO WF3-COMPANY.
           MOVE EMP-EMPLOYEE           TO WF3-EMPLOYEE.
           MOVE WS-ADD-DATE            TO WF3-START-DATE.
           PERFORM 8400-FIND-BN100EFDA.
           IF (BN100EFDA-FOUND)
               GO TO 1410-END.

      *
      **** FIND FLEX DOLLAR RECORD FOR NEW EFD
      *
           MOVE EMP-COMPANY            TO BNREWS-COMPANY.
           MOVE "FL"                   TO BNREWS-PLAN-TYPE.
           MOVE FLP-FLEX-PLAN          TO BNREWS-PLAN-CODE.
           MOVE EMP-EMPLOYEE           TO BNREWS-EMPLOYEE.
           MOVE WS-ADD-DATE            TO BNREWS-AS-OF-DATE.
           MOVE "FLD"                  TO BNREWS-FILE-PREFIX.
           PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70.
           IF (ERROR-FOUND)
               PERFORM 1825-CREATE-EA-EFDA
               THRU    1825-END
               GO TO 1410-CONTINUE.

      *
      **** NOW WE WILL TRY TO FIND OUT STOP DATE OF NEW EFD RECORD
      *
           MOVE EMP-COMPANY            TO DB-COMPANY.
           MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE WS-ADD-DATE            TO DB-START-DATE.
           PERFORM 850-FIND-NLT-EFDSET1.
           IF  (EMPFLEXDOL-FOUND)
           AND (EFD-COMPANY            = DB-COMPANY)
           AND (EFD-EMPLOYEE           = DB-EMPLOYEE)
               IF  (EMPFLEXDOL-FOUND)
               AND (EFD-COMPANY        = EMP-COMPANY)
               AND (EFD-EMPLOYEE       = EMP-EMPLOYEE)
               AND (EFD-START-DATE     > FLD-STOP-DATE)
                   MOVE FLD-STOP-DATE  TO WS-STOP-DATE
               ELSE
                   MOVE EFD-START-DATE TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   SUBTRACT 1          FROM WSDR-JULIAN-DAYS
                   PERFORM 900-JULIAN-TO-DATE
                   MOVE WSDR-FR-DATE   TO WS-STOP-DATE
               END-IF
           ELSE
               MOVE FLD-STOP-DATE      TO WS-STOP-DATE.

           SET NO-DUP-EFD              TO TRUE.

      *
      **** CHECK IF WE HAVE EMPLOYEE FLEX DOLLAR RECORD FOR WS-ADD-DATE
      *
           MOVE EMP-COMPANY            TO DB-COMPANY.
           MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE WS-ADD-DATE            TO DB-START-DATE.
           PERFORM 850-FIND-NLT-EFDSET2.
           IF  (EMPFLEXDOL-FOUND)
           AND (EFD-COMPANY            = DB-COMPANY)
           AND (EFD-EMPLOYEE           = DB-EMPLOYEE)
               PERFORM 1411-CHECK-DUP-EMPFLEXDOL
               THRU    1411-END.

           SET NO-DUP-WF3              TO TRUE.

           MOVE EMP-COMPANY            TO WF3-COMPANY.
           MOVE EMP-EMPLOYEE           TO WF3-EMPLOYEE.
           INITIALIZE WF3-START-DATE.
           PERFORM 8500-FIND-NLT-BN100EFDA.
           PERFORM 1412-CHECK-DUP-BN100EFDA
           THRU    1412-END
               UNTIL (BN100EFDA-NOTFOUND)
               OR    (WF3-COMPANY      NOT = EMP-COMPANY)
               OR    (WF3-EMPLOYEE     NOT = EMP-EMPLOYEE)
               OR    (DUP-WF3).

           IF (DUP-EFD)
           OR (DUP-WF3)
               GO TO 1410-CONTINUE.

       1410-PROCESS-EMPFLEXDOL.
           MOVE FLD-FLEX-PLAN          TO EFD-FLEX-PLAN.

           PERFORM 1850-CREATE-A-EFDA
           THRU    1850-END.

       1410-PROCESS-PLAN.
           MOVE EMP-COMPANY            TO DB-COMPANY.
           MOVE FLP-FLEX-PLAN          TO DB-FLEX-PLAN.
           MOVE PLNSET6-FLEX-PLAN      TO WS-DB-BEG-RNG.
PERF       IF  (ADD-GROUP-CHANGES)
PERF           STRING
P74069         "((PLN-DEFAULT = ?) OR (PLN-DEFAULT = ?) OR "
P74069         "(PLN-DEFAULT = ?)) AND ((PLN-GROUP-NAME = ?) OR "
P74069         "(PLN-GROUP-NAME = ?))"
P74069*        "(PLN-DEFAULT = ?) OR (PLN-DEFAULT = ?) OR "
P74069*        "(PLN-DEFAULT = ?) AND ((PLN-GROUP-NAME = ?) OR "
P74069*        "(PLN-GROUP-NAME = ?))"
PERF           DELIMITED BY SIZE INTO FILTER-STRING
PERF           PERFORM 890-CREATE-FILTER
PERF           MOVE "T"                TO ALPHANUM-FILTER-VALUE
PERF           PERFORM 890-SET-ALPHANUM-FILTER-VALUE
PERF           MOVE "O"                TO ALPHANUM-FILTER-VALUE
PERF           PERFORM 890-SET-ALPHANUM-FILTER-VALUE
PERF           MOVE "A"                TO ALPHANUM-FILTER-VALUE
PERF           PERFORM 890-SET-ALPHANUM-FILTER-VALUE
PERF           MOVE SPACES             TO ALPHANUM-FILTER-VALUE
PERF           PERFORM 890-SET-ALPHANUM-FILTER-VALUE
PERF           MOVE BNG-GROUP-NAME     TO ALPHANUM-FILTER-VALUE
PERF           PERFORM 890-SET-ALPHANUM-FILTER-VALUE
PERF           PERFORM 850-FILTER-BEGRNG-PLNSET6
PERF       ELSE
           PERFORM 850-FIND-BEGRNG-PLNSET6.

           PERFORM 1414-SEL-PLAN
           THRU    1414-END
               UNTIL (PLAN-NOTFOUND).

       1410-CONTINUE.
           INITIALIZE CRT-ERROR-NBR
                      CRT-ERROR-CAT
                      CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3.

       1410-END.

006960******************************************************************
       1411-CHECK-DUP-EMPFLEXDOL.
006960******************************************************************

           MOVE EFD-COMPANY                TO WF4-COMPANY.
           MOVE EFD-EMPLOYEE               TO WF4-EMPLOYEE.
           MOVE EFD-START-DATE             TO WF4-START-DATE.
           PERFORM 8400-FIND-BN100EFDC.
           IF (BN100EFDC-FOUND)
               IF (WF4-FUNCTION-CODE       = "S"  OR "CS")
J29761             IF  ((WF4-START-DATE    < WS-ADD-DATE)
J29761             AND  (WF4-STOP-DATE     > WS-ADD-DATE))
J29761             OR  ((WF4-START-DATE    < WS-STOP-DATE)
J29761             AND  (WF4-STOP-DATE     > WS-STOP-DATE))
J29761             OR  ((WF4-START-DATE    > WS-ADD-DATE)
J29761             AND  (WF4-STOP-DATE     < WS-STOP-DATE))
                       SET DUP-EFD         TO TRUE
J29761                 GO TO 1411-END 
J29761             END-IF
J29761         END-IF
J29761     ELSE
               IF  ((EFD-START-DATE        <= WS-ADD-DATE)
               AND  (EFD-STOP-DATE         >= WS-ADD-DATE))
               OR  ((EFD-START-DATE        <= WS-STOP-DATE)
               AND  (EFD-STOP-DATE         >= WS-STOP-DATE))
               OR  ((EFD-START-DATE        >= WS-ADD-DATE)
               AND  (EFD-STOP-DATE         <= WS-STOP-DATE))
                   SET DUP-EFD             TO TRUE  
J29761     END-IF.

       1411-END.

006960******************************************************************
       1412-CHECK-DUP-BN100EFDA.
006960******************************************************************

           IF (WF3-FUNCTION-CODE       = "EA")
               GO TO 1412-NEXT-BN100EFDA.

           IF  ((WF3-START-DATE        <= WS-ADD-DATE)
           AND  (WF3-STOP-DATE         >= WS-ADD-DATE))
           OR  ((WF3-START-DATE        <= WS-STOP-DATE)
           AND  (WF3-STOP-DATE         >= WS-STOP-DATE))
           OR  ((WF3-START-DATE        >= WS-ADD-DATE)
           AND  (WF3-STOP-DATE         <= WS-STOP-DATE))
               SET DUP-WF3             TO TRUE
               GO TO 1412-END.

       1412-NEXT-BN100EFDA.
           PERFORM 8600-FIND-NEXT-BN100EFDA.

       1412-END.

006960******************************************************************
       1414-SEL-PLAN.
006960******************************************************************

PERF  *    IF  (ADD-GROUP-CHANGES)
PERF  *    AND (PLN-DEFAULT            NOT = "T" AND "O" AND "A")
PERF  *        GO TO 1414-NEXT-PLAN.

PERF  *    IF  (ADD-GROUP-CHANGES)
PERF  *    AND (PLN-GROUP-NAME         NOT = SPACES)
PERF  *    AND (PLN-GROUP-NAME         NOT = BNG-GROUP-NAME)
PERF  *        GO TO 1414-NEXT-PLAN.

      *
      **** CALL PLAN ELIGIBILITY SECTION WHICH VALIDATES ONLY
      **** GROUP & ZIP CODE
      *
           MOVE EMP-COMPANY            TO BNPEWS-COMPANY.
           MOVE PLN-PLAN-TYPE          TO BNPEWS-PLAN-TYPE.
           MOVE PLN-PLAN-CODE          TO BNPEWS-PLAN-CODE.
           MOVE WS-EMPLOYEE            TO BNPEWS-EMPLOYEE.
AI0010     MOVE WS-EFFECT-DATE         TO BNPEWS-START-DATE.           
           MOVE "E"                    TO BNPEWS-COVER-TYPE.
           PERFORM 5000-EDIT-GROUP-N-ZIP-70.
           IF (ERROR-FOUND)
               GO TO 1414-CONTINUE.

           SET PROC-PLN                TO TRUE.

           IF (NEW-HIRE-CHANGES)
084600******** CALCULATE ELIGIBILITY DATE AND SET START DATE
084800         MOVE EMP-COMPANY        TO BNEDWS-COMPANY
084900         MOVE PLN-PLAN-TYPE      TO BNEDWS-PLAN-TYPE
085000         MOVE PLN-PLAN-CODE      TO BNEDWS-PLAN-CODE
085100         MOVE EMP-EMPLOYEE       TO BNEDWS-EMPLOYEE
085200         MOVE WS-EFFECT-DATE     TO BNEDWS-AS-OF-DATE
P71977         INITIALIZE BNEDWS-FROM-MAGIC-SW
085300         PERFORM 5000-ELIGIBILITY-DATE-CALC-70
085400         IF (ERROR-FOUND)
                   PERFORM 1835-CREATE-EA-BENA
                   THRU    1835-END
                   GO TO 1414-CONTINUE
               ELSE
086000             MOVE BNEDWS-ELIGIBILITY-DATE
                                       TO WS-ADD-DATE
                   GO TO 1414-PROCEED.

           IF (PROC-NON-FLEX)
               SET ADD-RULES           TO TRUE
               PERFORM 7000-FIND-BWT-FOR-ACTION
               IF (ERROR-FOUND)
                   INITIALIZE CRT-ERROR-NBR
                              CRT-ERROR-CAT
                              CRT-ERR-VAR1
                              CRT-ERR-VAR2
                              CRT-ERR-VAR3
                              CRT-ERR-VAR4
                              CRT-ERR-VAR5
                   MOVE EMP-COMPANY    TO BNEDWS-COMPANY
                   MOVE PLN-PLAN-TYPE  TO BNEDWS-PLAN-TYPE
                   MOVE PLN-PLAN-CODE  TO BNEDWS-PLAN-CODE
                   MOVE WS-EMPLOYEE    TO BNEDWS-EMPLOYEE
                   MOVE WS-EFFECT-DATE TO BNEDWS-AS-OF-DATE
                   INITIALIZE BNEDWS-FROM-MAGIC-SW
                   PERFORM 5000-ELIGIBILITY-DATE-CALC-70
                   IF (NO-ERROR-FOUND)
P50065                 MOVE WS-EFFECT-DATE
P50065                                 TO WS-ADD-DATE
                       PERFORM 1420-EDIT-BY-CODE
                       THRU    1420-END

                       IF (DUP-BEN)
                           GO TO 1414-CONTINUE
                       END-IF

J79757              IF (BNREWS-ERR-VAR4 NOT = 118)
J79757              OR (PLN-DEFAULT NOT = "N")
                       MOVE 159        TO CRT-ERROR-NBR
                       MOVE "BN100"    TO CRT-ERROR-CAT
                       INITIALIZE CRT-ERR-VAR1
                                  CRT-ERR-VAR2
                                  CRT-ERR-VAR3
                                  CRT-ERR-VAR4
                                  CRT-ERR-VAR5
                       PERFORM 1835-CREATE-EA-BENA
                       THRU    1835-END
                   END-IF
J79757             END-IF
                   GO TO 1414-CONTINUE
               END-IF
           ELSE
           IF (ERROR-FOUND)
               PERFORM 1835-CREATE-EA-BENA
               THRU    1835-END
               GO TO 1414-CONTINUE.

           IF (PLN-START-DATE          > WS-ADD-DATE)
               GO TO 1414-NEXT-PLAN.

           IF  (PLN-STOP-DATE          <= WS-ADD-DATE)
           AND (PLN-STOP-DATE          NOT = ZEROES)
               GO TO 1414-NEXT-PLAN.

       1414-PROCEED.
           IF  (PLN-STOP-DATE           NOT = ZEROES)
           AND (PLN-DEFAULT             = "N")
           AND (BNEDWS-ELIGIBILITY-DATE > PLN-STOP-DATE)
               GO TO 1414-NEXT-PLAN.

           IF (PLN-FLEX-PLAN           NOT = SPACES)
               MOVE PLN-COMPANY        TO DB-COMPANY
               MOVE PLN-FLEX-PLAN      TO DB-FLEX-PLAN
               PERFORM 840-FIND-FLPSET1.

           IF (PLN-DEFAULT             = "T")
               PERFORM 1416-EDIT-BY-TYPE
               THRU    1416-END
               IF (DUP-BEN)
                   GO TO 1414-NEXT-PLAN
               END-IF
           ELSE
           IF (PLN-DEFAULT             = "O")
               PERFORM 1418-EDIT-BY-OPTION
               THRU    1418-END
               IF (DUP-BEN)
                   GO TO 1414-NEXT-PLAN.

           PERFORM 1420-EDIT-BY-CODE
           THRU    1420-END.

           IF (DUP-BEN)
               GO TO 1414-NEXT-PLAN.

       1414-PROCESS-PLAN.
           IF  ((NEW-HIRE-CHANGES)
J79757     OR   (ADD-GROUP-CHANGES))
           AND (PLN-DEFAULT            = "N")
      *For new warning report
               SET ELIG-ERROR          TO TRUE
               INITIALIZE CRT-ERR-VAR1
                          CRT-ERR-VAR2
                          CRT-ERR-VAR3
                          CRT-ERR-VAR4
                          CRT-ERR-VAR5
               PERFORM 1835-CREATE-EA-BENA
               THRU    1835-END
               SET NO-ELIG-ERROR       TO TRUE
           ELSE
               PERFORM 1426-CALL-BNBEN
               THRU    1426-END
               IF (ERROR-FOUND)
                   PERFORM 1835-CREATE-EA-BENA
                   THRU    1835-END
                   GO TO 1414-CONTINUE
               ELSE
                   MOVE WS-ADD-DATE     TO WS-SAVE-ADD-DATE

                   PERFORM 1862-CREATE-A-BENA
                   THRU    1862-END
                       VARYING I1 FROM 1 BY 1
                       UNTIL  (I1 > BNBEN-NBR-LINES).

           MOVE WS-SAVE-ADD-DATE       TO WS-ADD-DATE.

       1414-CONTINUE.
           INITIALIZE CRT-ERROR-NBR
                      CRT-ERROR-CAT
                      CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3.

       1414-NEXT-PLAN.
           PERFORM 860-FIND-NXTRNG-PLNSET6.

       1414-END.

006960******************************************************************
       1416-EDIT-BY-TYPE.
006960******************************************************************

           SET NO-DUP-BEN              TO TRUE.

           MOVE EMP-COMPANY            TO DB-COMPANY.
           MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE PLN-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE BENSET4-PLAN-TYPE      TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-BENSET4.
           PERFORM
               UNTIL (BENEFIT-NOTFOUND)
               OR    (DUP-BEN)

P77134*        MOVE BEN-COMPANY        TO WF1-COMPANY
P77134*        MOVE BEN-EMPLOYEE       TO WF1-EMPLOYEE
P77134*        MOVE BEN-PLAN-TYPE      TO WF1-PLAN-TYPE
P77134*        MOVE BEN-PLAN-CODE      TO WF1-PLAN-CODE
P77134*        MOVE BEN-START-DATE     TO WF1-START-DATE

P77134         MOVE BEN-COMPANY        TO WF2-COMPANY
P77134         MOVE BEN-EMPLOYEE       TO WF2-EMPLOYEE
P77134         MOVE BEN-PLAN-TYPE      TO WF2-PLAN-TYPE
P77134         MOVE BEN-PLAN-CODE      TO WF2-PLAN-CODE
P77134         MOVE BEN-START-DATE     TO WF2-START-DATE
               PERFORM 8400-FIND-BN100BENC
               IF  (BN100BENC-FOUND)
               AND (WF2-FUNCTION-CODE  NOT = "ET" AND "EC")
                   IF (WF2-STOP-DATE   >= WS-ADD-DATE)
                       SET DUP-BEN     TO TRUE
                   END-IF
               ELSE
                   PERFORM 1422-CHECK-FOR-DUP-BEN
                   THRU    1422-END
               END-IF

               IF (NO-DUP-BEN)
                   PERFORM 860-FIND-NXTRNG-BENSET4
               END-IF
           END-PERFORM.

           IF (DUP-BEN)
               GO TO 1416-END.

           MOVE EMP-COMPANY            TO WF1-COMPANY.
           MOVE EMP-EMPLOYEE           TO WF1-EMPLOYEE.
           MOVE PLN-PLAN-TYPE          TO WF1-PLAN-TYPE.
           INITIALIZE WF1-PLAN-CODE
                      WF1-START-DATE.
           PERFORM 8500-FIND-NLT-BN100BENA.
           PERFORM
               UNTIL (BN100BENA-NOTFOUND)
               OR    (WF1-COMPANY      NOT = EMP-COMPANY)
               OR    (WF1-EMPLOYEE     NOT = EMP-EMPLOYEE)
               OR    (WF1-PLAN-TYPE    NOT = PLN-PLAN-TYPE)
               OR    (DUP-BEN)

               IF (WF1-FUNCTION-CODE   NOT = "EA" AND "ES")
                   PERFORM 1424-CHECK-FOR-DUP-WF1
                   THRU    1424-END
               END-IF

               IF (NO-DUP-BEN)
                   PERFORM 8600-FIND-NEXT-BN100BENA
               END-IF
           END-PERFORM.

       1416-END.

006960******************************************************************
       1418-EDIT-BY-OPTION.
006960******************************************************************

           SET NO-DUP-BEN              TO TRUE.

           MOVE EMP-COMPANY            TO DB-COMPANY.
           MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE PLN-PLAN-OPTION        TO DB-PLAN-OPTION.
           MOVE BENSET8-PLAN-OPTION    TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-BENSET8.
           PERFORM
               UNTIL (BENEFIT-NOTFOUND)
               OR    (DUP-BEN)

P77134*        MOVE BEN-COMPANY        TO WF1-COMPANY
P77134*        MOVE BEN-EMPLOYEE       TO WF1-EMPLOYEE
P77134*        MOVE BEN-PLAN-TYPE      TO WF1-PLAN-TYPE
P77134*        MOVE BEN-PLAN-CODE      TO WF1-PLAN-CODE
P77134*        MOVE BEN-START-DATE     TO WF1-START-DATE

P77134         MOVE BEN-COMPANY        TO WF2-COMPANY
P77134         MOVE BEN-EMPLOYEE       TO WF2-EMPLOYEE
P77134         MOVE BEN-PLAN-TYPE      TO WF2-PLAN-TYPE
P77134         MOVE BEN-PLAN-CODE      TO WF2-PLAN-CODE
P77134         MOVE BEN-START-DATE     TO WF2-START-DATE
               PERFORM 8400-FIND-BN100BENC
               IF  (BN100BENC-FOUND)
               AND (WF2-FUNCTION-CODE  NOT = "ET" AND "EC")
                   IF (WF2-STOP-DATE   >= WS-ADD-DATE)
                       SET DUP-BEN     TO TRUE
                   END-IF
               ELSE
                   PERFORM 1422-CHECK-FOR-DUP-BEN
                   THRU    1422-END
               END-IF

               IF (NO-DUP-BEN)
                   PERFORM 860-FIND-NXTRNG-BENSET8
               END-IF
           END-PERFORM.

           IF (DUP-BEN)
               GO TO 1418-CONTINUE.

           MOVE PLN-PLAN-TYPE          TO WS-SAVE-PLAN-TYPE.
           MOVE PLN-PLAN-CODE          TO WS-SAVE-PLAN-CODE.
           MOVE PLN-PLAN-OPTION        TO WS-SAVE-PLAN-OPTION.

           MOVE EMP-COMPANY            TO WF1-COMPANY.
           MOVE EMP-EMPLOYEE           TO WF1-EMPLOYEE.
           INITIALIZE WF1-PLAN-TYPE
                      WF1-PLAN-CODE
                      WF1-START-DATE.
           PERFORM 8500-FIND-NLT-BN100BENA.
           PERFORM
               UNTIL (BN100BENA-NOTFOUND)
               OR    (WF1-COMPANY      NOT = EMP-COMPANY)
               OR    (WF1-EMPLOYEE     NOT = EMP-EMPLOYEE)
               OR    (DUP-BEN)

               MOVE WF1-PLAN-TYPE      TO DB-PLAN-TYPE
               MOVE WF1-PLAN-CODE      TO DB-PLAN-CODE
               PERFORM 840-FIND-PLNSET1

               IF  (PLN-PLAN-OPTION    = WS-SAVE-PLAN-OPTION)
J21197         AND (PLN-PLAN-TYPE      = WS-SAVE-PLAN-TYPE)
               AND (WF1-FUNCTION-CODE  NOT = "EA" AND "ES")
                   PERFORM 1424-CHECK-FOR-DUP-WF1
                   THRU    1424-END
               END-IF

               IF (NO-DUP-BEN)
                   PERFORM 8600-FIND-NEXT-BN100BENA
               END-IF
           END-PERFORM.

       1418-CONTINUE.
           MOVE EMP-COMPANY            TO DB-COMPANY.
           MOVE WS-SAVE-PLAN-TYPE      TO DB-PLAN-TYPE.
           MOVE WS-SAVE-PLAN-CODE      TO DB-PLAN-CODE.
           PERFORM 840-FIND-PLNSET1.

       1418-END.

006960******************************************************************
       1420-EDIT-BY-CODE.
006960******************************************************************

           SET NO-DUP-BEN              TO TRUE.

           MOVE EMP-COMPANY            TO DB-COMPANY.
           MOVE EMP-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE PLN-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE PLN-PLAN-CODE          TO DB-PLAN-CODE.
           MOVE BENSET4-PLAN-CODE      TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-BENSET4.
           PERFORM
               UNTIL (BENEFIT-NOTFOUND)
               OR    (DUP-BEN)

P77134*        MOVE BEN-COMPANY        TO WF1-COMPANY
P77134*        MOVE BEN-EMPLOYEE       TO WF1-EMPLOYEE
P77134*        MOVE BEN-PLAN-TYPE      TO WF1-PLAN-TYPE
P77134*        MOVE BEN-PLAN-CODE      TO WF1-PLAN-CODE
P77134*        MOVE BEN-START-DATE     TO WF1-START-DATE

P77134         MOVE BEN-COMPANY        TO WF2-COMPANY
P77134         MOVE BEN-EMPLOYEE       TO WF2-EMPLOYEE
P77134         MOVE BEN-PLAN-TYPE      TO WF2-PLAN-TYPE
P77134         MOVE BEN-PLAN-CODE      TO WF2-PLAN-CODE
P77134         MOVE BEN-START-DATE     TO WF2-START-DATE
               PERFORM 8400-FIND-BN100BENC
               IF  (BN100BENC-FOUND)
               AND (WF2-FUNCTION-CODE  NOT = "ET" AND "EC")
                   IF (WF2-STOP-DATE   >= WS-ADD-DATE)
                       SET DUP-BEN     TO TRUE
                   END-IF
               ELSE
                   PERFORM 1422-CHECK-FOR-DUP-BEN
                   THRU    1422-END
               END-IF

               IF (NO-DUP-BEN)
                   PERFORM 860-FIND-NXTRNG-BENSET4
               END-IF
           END-PERFORM.

           IF (DUP-BEN)
               GO TO 1420-END.

           MOVE EMP-COMPANY            TO WF1-COMPANY.
           MOVE EMP-EMPLOYEE           TO WF1-EMPLOYEE.
           MOVE PLN-PLAN-TYPE          TO WF1-PLAN-TYPE.
           MOVE PLN-PLAN-CODE          TO WF1-PLAN-CODE.
           INITIALIZE WF1-START-DATE.
           PERFORM 8500-FIND-NLT-BN100BENA.
           PERFORM
               UNTIL (BN100BENA-NOTFOUND)
               OR    (WF1-COMPANY      NOT = EMP-COMPANY)
               OR    (WF1-EMPLOYEE     NOT = EMP-EMPLOYEE)
               OR    (WF1-PLAN-TYPE    NOT = PLN-PLAN-TYPE)
               OR    (WF1-PLAN-CODE    NOT = PLN-PLAN-CODE)
               OR    (DUP-BEN)

               IF (WF1-FUNCTION-CODE   NOT = "EA" AND "ES")
                   PERFORM 1424-CHECK-FOR-DUP-WF1
                   THRU    1424-END
               END-IF

               IF (NO-DUP-BEN)
                   PERFORM 8600-FIND-NEXT-BN100BENA
               END-IF
           END-PERFORM.

       1420-END.

006960******************************************************************
       1422-CHECK-FOR-DUP-BEN.
006960******************************************************************

           IF (PROC-FLEX)
               IF  ((BEN-START-DATE    <= WS-ADD-DATE)
               AND  (BEN-STOP-DATE     >= WS-ADD-DATE))
               OR  ((BEN-START-DATE    <= WS-STOP-DATE)
               AND  (BEN-STOP-DATE     >= WS-STOP-DATE))
               OR  ((BEN-START-DATE    >= WS-ADD-DATE)
               AND  (BEN-STOP-DATE     <= WS-STOP-DATE))
                   SET DUP-BEN         TO TRUE
               END-IF
           ELSE
           IF (BEN-STOP-DATE           NOT = ZEROES)
               IF  (BEN-START-DATE     <= WS-ADD-DATE)
               AND (BEN-STOP-DATE      >= WS-ADD-DATE)
                   SET DUP-BEN         TO TRUE
               END-IF
           ELSE
           IF (BEN-START-DATE          <= WS-ADD-DATE)
               SET DUP-BEN             TO TRUE.

       1422-END.

006960******************************************************************
       1424-CHECK-FOR-DUP-WF1.
006960******************************************************************

           IF (PROC-FLEX)
           OR (WF1-STOP-DATE           NOT = ZEROES)
               IF  ((WF1-START-DATE    <= WS-ADD-DATE)
               AND  (WF1-STOP-DATE     >= WS-ADD-DATE))
               OR  ((WF1-START-DATE    <= WS-STOP-DATE)
               AND  (WF1-STOP-DATE     >= WS-STOP-DATE))
               OR  ((WF1-START-DATE    >= WS-ADD-DATE)
               AND  (WF1-STOP-DATE     <= WS-STOP-DATE))
                   SET DUP-BEN         TO TRUE
               END-IF
           ELSE
           IF (WF1-START-DATE          <= WS-ADD-DATE)
               SET DUP-BEN             TO TRUE.

       1424-END.

006960******************************************************************
       1426-CALL-BNBEN.
006960******************************************************************

           PERFORM 1428-MOVE-TO-BNBEN
           THRU    1428-END.

           PERFORM 2000-BNBEN-EDIT-TRAN.

       1426-END.

006960******************************************************************
       1428-MOVE-TO-BNBEN.
006960******************************************************************

002000     INITIALIZE BNBEN-DETAIL-GROUP
117816                BNBEN-PROC-FLEX-SW 
002000                BNBEN-USE-NAVIGATE-SW.
J31573*               BNBEN-HIPAA-REASON
J31573*               HRHDB-HIPAA-REASON.

           MOVE PRM-COMPANY                TO BNBEN-COMPANY.
           MOVE EMP-EMPLOYEE               TO BNBEN-EMPLOYEE (1).
           MOVE PLN-PLAN-TYPE              TO BNBEN-PLAN-TYPE (1).
           MOVE PLN-PLAN-CODE              TO BNBEN-PLAN-CODE (1).
           MOVE WS-ADD-DATE                TO BNBEN-START-DATE (1).
J17193     MOVE PRM-UPDATE-ACA             TO BNBEN-UPDATE-ACA.
J17193     MOVE SPACES                     TO BNBEN-WEB-UPDATE.
           INITIALIZE BNBEN-STOP-DATE (1).
117816*    MOVE "Y"                      TO BNBEN-SKIP-ELIGIBILITY (1).

           MOVE PRM-CREATE-TRANS           TO BNBEN-CREATE-TRANS (1).

           MOVE "C"                        TO BNBEN-FC.
           MOVE "A"                        TO BNBEN-LINE-FC (1).
           MOVE 1                          TO BNBEN-NBR-LINES.
117816     MOVE WS-PROC-FLEX-SW            TO BNBEN-PROC-FLEX-SW.
J31573
J31573     IF (NEW-HIRE-CHANGES)
P72733         MOVE "BT"                   TO DB-TYPE
P72733         MOVE "(PCO-HIPAA-REASON = ?)"   
P72733                                     TO FILTER-STRING
P72733         PERFORM 890-CREATE-FILTER
P72733         MOVE 28                     TO NUMERIC-FILTER-VALUE
P72733         PERFORM 890-SET-NUMERIC-FILTER-VALUE
P72733         MOVE PCOSET1-TYPE           TO WS-DB-BEG-RNG
P72733         PERFORM 850-FILTER-BEGRNG-PCOSET1
P72733         IF (PCODES-FOUND)
P72733             MOVE PCO-CODE           TO BNBEN-HIPAA-REASON
P72733                                        HRHDB-HIPAA-REASON
P72733         END-IF
J31573     END-IF.

       1428-END.

006960******************************************************************
       1500-SEL-BNH-EMPLOYEE.
006960******************************************************************

           MOVE BNH-EFFECT-DATE        TO WS-EFFECT-DATE.

           PERFORM 1600-SEL-BNCHANGE
           THRU    1600-END
               UNTIL (BNCHANGE-NOTFOUND)
               OR    (BNH-COMPANY      NOT = WS-COMPANY)
               OR    (BNH-EMPLOYEE     NOT = WS-EMPLOYEE)
               OR    (BNH-EFFECT-DATE  NOT = WS-EFFECT-DATE)
               OR    (PROC-BNG).

       1500-END.

006960******************************************************************
       1600-SEL-BNCHANGE.
006960******************************************************************

           IF (BNH-CHANGE-TYPE             = "1")
               PERFORM 1620-ZIP-CODE-CHANGES
               THRU    1620-END
                   UNTIL (BNCHANGE-NOTFOUND)
                   OR    (BNH-COMPANY      NOT = WS-COMPANY)
                   OR    (BNH-EMPLOYEE     NOT = WS-EMPLOYEE)
                   OR    (BNH-EFFECT-DATE  NOT = WS-EFFECT-DATE)
                   OR    (BNH-CHANGE-TYPE  NOT = "1")
                   OR    (PROC-BNG).

           IF (BNH-CHANGE-TYPE             = "2")
               PERFORM 1630-SALARY-CHANGES
               THRU    1630-END
                   UNTIL (BNCHANGE-NOTFOUND)
                   OR    (BNH-COMPANY      NOT = WS-COMPANY)
                   OR    (BNH-EMPLOYEE     NOT = WS-EMPLOYEE)
                   OR    (BNH-EFFECT-DATE  NOT = WS-EFFECT-DATE)
                   OR    (BNH-CHANGE-TYPE  NOT = "2")
                   OR    (PROC-BNG).

           IF (BNH-CHANGE-TYPE             = "3")
               INITIALIZE I1
               PERFORM 1660-PAY-FREQ-CHANGES
               THRU    1660-END
                   UNTIL (BNCHANGE-NOTFOUND)
                   OR    (BNH-COMPANY      NOT = WS-COMPANY)
                   OR    (BNH-EMPLOYEE     NOT = WS-EMPLOYEE)
                   OR    (BNH-EFFECT-DATE  NOT = WS-EFFECT-DATE)
                   OR    (BNH-CHANGE-TYPE  NOT = "3")
                   OR    (PROC-BNG).

           IF (BNH-CHANGE-TYPE             = "4")
               PERFORM 1690-BIRTHDATE-CHANGES
               THRU    1690-END
                   UNTIL (BNCHANGE-NOTFOUND)
                   OR    (BNH-COMPANY      NOT = WS-COMPANY)
                   OR    (BNH-EMPLOYEE     NOT = WS-EMPLOYEE)
                   OR    (BNH-EFFECT-DATE  NOT = WS-EFFECT-DATE)
                   OR    (BNH-CHANGE-TYPE  NOT = "4")
                   OR    (PROC-BNG).

           IF (BNH-CHANGE-TYPE             = "5")
               PERFORM 1695-SERVICE-DATE-CHANGES
               THRU    1695-END
                   UNTIL (BNCHANGE-NOTFOUND)
                   OR    (BNH-COMPANY      NOT = WS-COMPANY)
                   OR    (BNH-EMPLOYEE     NOT = WS-EMPLOYEE)
                   OR    (BNH-EFFECT-DATE  NOT = WS-EFFECT-DATE)
                   OR    (BNH-CHANGE-TYPE  NOT = "5")
                   OR    (PROC-BNG).

           IF (BNH-CHANGE-TYPE             = "6")
               PERFORM 1700-SMOKER-FLAG-CHANGES
               THRU    1700-END
                   UNTIL (BNCHANGE-NOTFOUND)
                   OR    (BNH-COMPANY      NOT = WS-COMPANY)
                   OR    (BNH-EMPLOYEE     NOT = WS-EMPLOYEE)
                   OR    (BNH-EFFECT-DATE  NOT = WS-EFFECT-DATE)
                   OR    (BNH-CHANGE-TYPE  NOT = "6")
                   OR    (PROC-BNG).

           IF (BNH-CHANGE-TYPE             = "7")
               PERFORM 1400-ADD-GROUP
               THRU    1400-END
                   UNTIL (BNCHANGE-NOTFOUND)
                   OR    (BNH-COMPANY      NOT = WS-COMPANY)
                   OR    (BNH-EMPLOYEE     NOT = WS-EMPLOYEE)
                   OR    (BNH-EFFECT-DATE  NOT = WS-EFFECT-DATE)
                   OR    (BNH-CHANGE-TYPE  NOT = "7")
                   OR    (PROC-BNG).

       1600-END.

006960******************************************************************
       1620-ZIP-CODE-CHANGES.
006960******************************************************************

J43365     IF (SELECT-COMPANY)
J43365          MOVE EMP-COMPANY            TO CRT-COMPANY
J43365          MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL
J43365          PERFORM 700-HR-EMP-SECURITY
J43365          IF (HRWS-EMP-SECURED)
J43365              GO TO 1620-NEXT-BNCHANGE
J43365          END-IF
J43365     END-IF.
J43365
           SET ZIP-CODE-CHANGES        TO TRUE.

           IF (BNH-EFFECT-DATE         > PRM-THRU-DATE)
               GO TO 1620-NEXT-BNCHANGE.

           SET PROC-OFF                TO TRUE.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           MOVE BNH-COMPANY            TO DB-COMPANY.
           MOVE BNH-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE BENSET7-EMPLOYEE       TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-BENSET7.
           PERFORM 1622-SEL-BENEFIT
           THRU    1622-END
               UNTIL (BENEFIT-NOTFOUND).

       1620-NEXT-BNCHANGE.
           PERFORM 860-FIND-NEXT-BNHSET2.

           PERFORM 1900-SET-PROC-BNG-BNH-SW
           THRU    1900-END.

       1620-END.

006960******************************************************************
       1622-SEL-BENEFIT.
006960******************************************************************

           IF  (BEN-STOP-DATE          < BNH-EFFECT-DATE)
           AND (BEN-STOP-DATE          NOT = ZEROES)
               GO TO 1622-NEXT-BENEFIT.

           MOVE BEN-COMPANY            TO DB-COMPANY.
           MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
           PERFORM 840-FIND-PLNSET1.
           IF (PLN-POST-CODE-TBL       = SPACES)
               GO TO 1622-NEXT-BENEFIT.

           MOVE BEN-COMPANY            TO WF2-COMPANY.
           MOVE BEN-EMPLOYEE           TO WF2-EMPLOYEE.
           MOVE BEN-PLAN-TYPE          TO WF2-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO WF2-PLAN-CODE.
           MOVE BEN-START-DATE         TO WF2-START-DATE.
           PERFORM 8400-FIND-BN100BENC.
           IF  (BN100BENC-FOUND)
           AND (WF2-FUNCTION-CODE      NOT = "CS" AND "S")
               GO TO 1622-NEXT-BENEFIT.

P47575*--- If the previous record is created thru flex and with no erros
P47575*--- we need to skip finding term rules based on zip code
P47575     IF  (BN100BENC-FOUND)
P47575     AND (WF2-FLEX-PLAN          NOT = SPACES)
P47575     AND (WF2-FUNCTION-CODE      = "S")
P47575         GO TO 1622-NEXT-BENEFIT.

           MOVE PLN-POST-CODE-TBL      TO DB-POST-CODE-TBL.
           MOVE BNH-NEW-VALUE (1 : 5)  TO DB-POSTAL-CODE.
           PERFORM 850-FIND-NLT-BPCSET1.
           IF  (BNPOSTCODE-FOUND)
           AND (BPC-POST-CODE-TBL       = DB-POST-CODE-TBL)
           AND (BPC-POSTAL-CODE (1 : 5) = DB-POSTAL-CODE)
               GO TO 1622-NEXT-BENEFIT.

           SET PROC-BEN                TO TRUE.

      *
      **** FIND TERMINATION RULES FOR PLAN CODE AND ACTION CODE
      *
           SET TERM-RULES              TO TRUE.
           PERFORM 7000-FIND-BWT-FOR-ACTION.
           IF (ERROR-FOUND)
               PERFORM 1830-CREATE-ETEC-BENC
               THRU    1830-END
               GO TO 1622-CONTINUE.

           IF  (BN100BENC-FOUND)
           AND (WF2-FUNCTION-CODE      = "CS" OR "S")
               PERFORM 1624-STOP-CS-BENEFIT
               THRU    1624-END

               GO TO 1622-NEXT-BENEFIT.

           IF  (BEN-STOP-DATE          <= WS-TERM-DATE)
           AND (BEN-STOP-DATE          NOT = ZEROES)
               GO TO 1622-NEXT-BENEFIT.

       1622-PROCESS-BENEFIT.
      *
      **** IF TERM DATE IS > EFD START DATE, DELETE EFD AND ASSOCIATED
      **** BENEFITS
      *
           IF (BEN-START-DATE          > WS-TERM-DATE)
               PERFORM 1868-CHECK-YTD-N-HIPAA
               THRU    1868-END

               IF (ERROR-FOUND)
                   PERFORM 1832-CREATE-ED-BENC
                   THRU    1832-END
               ELSE
                   PERFORM 1854-CREATE-D-BENC
                   THRU    1854-END
               END-IF
           ELSE
               PERFORM 1705-STOP-BENEFIT
               THRU    1705-END.

       1622-CONTINUE.
           INITIALIZE CRT-ERROR-NBR
                      CRT-ERROR-CAT
                      CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3.

       1622-NEXT-BENEFIT.
           PERFORM 860-FIND-NXTRNG-BENSET7.

       1622-END.

006960******************************************************************
       1624-STOP-CS-BENEFIT.
006960******************************************************************

           IF (WF2-FUNCTION-CODE       = "CS")
               IF (WF2-STOP-DATE       NOT = WS-TERM-DATE)
                   MOVE WS-TERM-DATE   TO WF2-STOP-DATE
                   MOVE "S"            TO WF2-FUNCTION-CODE

                   PERFORM 8200-REWRITE-BN100BENC
               END-IF
      *
      ******** FIND NEXT IS TO FIND "CA" RECORD AND DELETE IT SINCE
      ******** EMPLOYEE WON'T BE ELIGIBLE FOR THIS PLAN
      *
               PERFORM 8600-FIND-NEXT-BN100BENC

               MOVE BNH-COMPANY        TO WF-COMPANY
               MOVE BNH-EMPLOYEE       TO WF-EMPLOYEE
               MOVE "3"                TO WF-REC-TYPE
               MOVE WF2-WF-EFFECT-DATE TO WF-EFFECT-DATE
               MOVE WF2-WF-SEQ-NBR     TO WF-SEQ-NBR
               PERFORM 8400-FIND-BN100WORK
               IF (BN100WORK-FOUND)
                   PERFORM 8300-DELETE-BN100WORK
               END-IF

               PERFORM 8300-DELETE-BN100BENC
           ELSE
           IF  (WF2-FUNCTION-CODE      = "S")
           AND (WF2-STOP-DATE          > WS-TERM-DATE)
                MOVE WS-TERM-DATE      TO WF2-STOP-DATE

                PERFORM 8200-REWRITE-BN100BENC.

       1624-END.

006960******************************************************************
       1630-SALARY-CHANGES.
006960******************************************************************

J43365     IF (SELECT-COMPANY)
J43365          MOVE EMP-COMPANY            TO CRT-COMPANY
J43365          MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL
J43365          PERFORM 700-HR-EMP-SECURITY
J43365          IF (HRWS-EMP-SECURED)
J43365              GO TO 1630-NEXT-BNCHANGE
J43365          END-IF
J43365     END-IF.
J43365
           SET SALARY-CHANGES          TO TRUE.

           IF (BNH-EFFECT-DATE         > PRM-THRU-DATE)
               GO TO 1630-NEXT-BNCHANGE.

           SET PROC-OFF                TO TRUE.

           PERFORM 1632-PRINT-SALARY-CHANGES
           THRU    1632-END.

           MOVE BNH-COMPANY            TO DB-COMPANY.
           MOVE BNH-EMPLOYEE           TO DB-EMPLOYEE.
PERF       MOVE EFDSET5-EMPLOYEE       TO WS-DB-BEG-RNG.
PERF       MOVE "(EFD-STOP-DATE >= ?)" TO FILTER-STRING.
PERF       PERFORM 890-CREATE-FILTER.
PERF       MOVE BNH-EFFECT-DATE        TO DATETIME-FILTER-VALUE.
PERF       PERFORM 890-SET-DATETIME-FILTER-VALUE.
PERF       PERFORM 850-FILTER-BEGRNG-EFDSET5.
           PERFORM 1640-SEL-EFD-EMPLOYEE
           THRU    1640-END
PERF           UNTIL (EMPFLEXDOL-NOTFOUND).

           MOVE BNH-COMPANY            TO DB-COMPANY.
           MOVE BNH-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE BENSET7-EMPLOYEE       TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-BENSET7.
           PERFORM 1650-SEL-REMAINING-BEN
           THRU    1650-END
               UNTIL (BENEFIT-NOTFOUND).

       1630-NEXT-BNCHANGE.
           IF (BNH-EFFECT-DATE         > PRM-THRU-DATE)
J43365     OR ((SELECT-COMPANY)
J43365     AND (HRWS-EMP-SECURED))
               PERFORM 860-FIND-NEXT-BNHSET2
           ELSE
               MOVE WS-SAV-COMPANY     TO DB-COMPANY
               MOVE WS-SV-EMPLOYEE     TO DB-EMPLOYEE
               MOVE WS-SV-EFFECT-DATE  TO DB-EFFECT-DATE
               MOVE WS-SV-CHANGE-TYPE  TO DB-CHANGE-TYPE
               MOVE WS-SV-SEQ-NBR      TO DB-SEQ-NBR
               PERFORM 850-FIND-NLT-BNHSET2.

           PERFORM 1900-SET-PROC-BNG-BNH-SW
           THRU    1900-END.

       1630-END.

006960******************************************************************
       1632-PRINT-SALARY-CHANGES.
006960******************************************************************

      *
      **** PRINT ALL SALALRY CHANGE RECORDS AS OF THE EFFECTIVE DATE
      **** USE LAST RECORD FOR ACTUAL PROCESSING
      **** 'SV' VARIABLES USED TO RESTORE 'NEXT' BNCHANGE RECORD
      **** 'SAVE' VARIABLES USED TO RESTORE 'LAST' SALARY CHANGE RECORD
      *
           MOVE BNH-COMPANY            TO WS-SAV-COMPANY.
           MOVE BNH-EMPLOYEE           TO WS-SV-EMPLOYEE.
           MOVE BNH-EFFECT-DATE        TO WS-SV-EFFECT-DATE.
           MOVE BNH-CHANGE-TYPE        TO WS-SV-CHANGE-TYPE.
           MOVE WS-HIGH-VALUES         TO WS-SV-SEQ-NBR.

           PERFORM
               UNTIL (BNCHANGE-NOTFOUND)
               OR    (BNH-COMPANY      NOT = WS-COMPANY)
               OR    (BNH-EMPLOYEE     NOT = WS-EMPLOYEE)
               OR    (BNH-EFFECT-DATE  NOT = WS-EFFECT-DATE)
               OR    (BNH-CHANGE-TYPE  NOT = "2")

               PERFORM 1800-CREATE-BN100WORK
               THRU    1800-END

               MOVE BNH-COMPANY        TO WS-SAVE-COMPANY
               MOVE BNH-EMPLOYEE       TO WS-SAVE-EMPLOYEE
               MOVE BNH-EFFECT-DATE    TO WS-SAVE-EFFECT-DATE
               MOVE BNH-CHANGE-TYPE    TO WS-SAVE-CHANGE-TYPE
               MOVE BNH-SEQ-NBR        TO WS-SAVE-SEQ-NBR

               PERFORM 860-FIND-NEXT-BNHSET2
           END-PERFORM.

           MOVE WS-SAVE-COMPANY        TO DB-COMPANY.
           MOVE WS-SAVE-EMPLOYEE       TO DB-EMPLOYEE.
           MOVE WS-SAVE-EFFECT-DATE    TO DB-EFFECT-DATE.
           MOVE WS-SAVE-CHANGE-TYPE    TO DB-CHANGE-TYPE.
           MOVE WS-SAVE-SEQ-NBR        TO DB-SEQ-NBR.
           PERFORM 850-FIND-NLT-BNHSET2.

       1632-END.

006960******************************************************************
       1640-SEL-EFD-EMPLOYEE.
006960******************************************************************

           MOVE EFD-FLEX-PLAN          TO WS-FLEX-PLAN.

           PERFORM 1642-SEL-EFD-FLEX-PLAN
           THRU    1642-END
               UNTIL (EMPFLEXDOL-NOTFOUND)
               OR    (EFD-COMPANY      NOT = DB-COMPANY)
               OR    (EFD-EMPLOYEE     NOT = DB-EMPLOYEE)
               OR    (EFD-FLEX-PLAN    NOT = WS-FLEX-PLAN).

       1640-END.

006960******************************************************************
       1642-SEL-EFD-FLEX-PLAN.
006960******************************************************************

PERF  *    IF (EFD-STOP-DATE           < BNH-EFFECT-DATE)
PERF  *        GO TO 1642-NEXT-EMPFLEXDOL.

           MOVE EFD-COMPANY            TO WF4-COMPANY.
           MOVE EFD-EMPLOYEE           TO WF4-EMPLOYEE.
           MOVE EFD-START-DATE         TO WF4-START-DATE.
           PERFORM 8400-FIND-BN100EFDC.
           IF (BN100EFDC-FOUND)
               GO TO 1642-NEXT-EMPFLEXDOL.

           MOVE EFD-COMPANY            TO DB-COMPANY.
           MOVE EFD-FLEX-PLAN          TO DB-FLEX-PLAN.
           MOVE EFD-FLD-START-DATE     TO DB-START-DATE.
           MOVE EFD-FLD-GROUP-NAME     TO DB-GROUP-NAME.
           PERFORM 840-FIND-FLDSET1.

           IF (FLD-CMP-BASE-PCT        NOT = ZEROES)
               GO TO 1642-PROCESS-EMPFLEXDOL.

           IF (FLD-EMP-BASE-PCT        NOT = ZEROES)
               GO TO 1642-PROCESS-EMPFLEXDOL.

           IF  (FLD-AGE-RATE-TBL       NOT = SPACES)
           AND (FLD-AGE-CREDIT         NOT = ZEROES)
               GO TO 1642-PROCESS-EMPFLEXDOL.

           IF  (FLD-SERV-RATE-TBL      NOT = SPACES)
           AND (FLD-SERV-CREDIT        NOT = ZEROES)
               GO TO 1642-PROCESS-EMPFLEXDOL.

           GO TO 1642-NEXT-EMPFLEXDOL.

       1642-PROCESS-EMPFLEXDOL.
           SET PROC-EFD                TO TRUE.

           SET NO-ERROR-IN-EFD         TO TRUE.

      *
      **** FIND CHANGE RULES FOR FLEX PLAN AND ACTION CODE
      *
           SET CHANGE-RULES            TO TRUE.
           PERFORM 7000-FIND-BWT-FOR-ACTION.
           IF (ERROR-FOUND)
               PERFORM 1820-CREATE-ETEC-EFDC
               THRU    1820-END
               SET ERROR-IN-EFD        TO TRUE
               MOVE 164                TO CRT-ERROR-NBR
P15764         MOVE "BN100"            TO CRT-ERROR-CAT
               INITIALIZE CRT-ERR-VAR1
                          CRT-ERR-VAR2
                          CRT-ERR-VAR3 CRT-ERR-VAR4
                          CRT-ERR-VAR5
               GO TO 1642-CONTINUE.

           MOVE EFD-COMPANY            TO BNREWS-COMPANY.
           MOVE "FL"                   TO BNREWS-PLAN-TYPE.
           MOVE EFD-FLEX-PLAN          TO BNREWS-PLAN-CODE.
           MOVE EFD-EMPLOYEE           TO BNREWS-EMPLOYEE.
           MOVE EFD-START-DATE         TO BNREWS-AS-OF-DATE.
           IF (WS-CHANGE-DATE          > EFD-START-DATE)
               MOVE WS-CHANGE-DATE     TO BNREWS-AS-OF-DATE.
           MOVE "FLD"                  TO BNREWS-FILE-PREFIX.
           PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70.
           IF (ERROR-FOUND)
               PERFORM 1820-CREATE-ETEC-EFDC
               THRU    1820-END
               SET ERROR-IN-EFD        TO TRUE
               MOVE 164                TO CRT-ERROR-NBR
P15764         MOVE "BN100"            TO CRT-ERROR-CAT
               INITIALIZE CRT-ERR-VAR1
                          CRT-ERR-VAR2
                          CRT-ERR-VAR3
                          CRT-ERR-VAR4
                          CRT-ERR-VAR5
               GO TO 1642-CONTINUE.

           IF (WS-CHANGE-DATE          < BNH-EFFECT-DATE)
               PERFORM 1644-REPROCESS-EFDS
               THRU    1644-END.

           IF (EFD-STOP-DATE           < WS-CHANGE-DATE)
               GO TO 1642-NEXT-EMPFLEXDOL.

           IF (EFD-SAL-CALC-DATE       = WS-CHANGE-DATE)
               MOVE EFD-COMPANY        TO BNASWS-COMPANY
               MOVE EFD-EMPLOYEE       TO BNASWS-EMPLOYEE
               MOVE "O"                TO BNASWS-DATE-TYPE
               MOVE FLD-SALARY-TYPE    TO BNASWS-SALARY-TYPE
               MOVE EFD-SAL-CALC-DATE  TO BNASWS-START-DATE
               PERFORM 5000-DO-ANNUAL-SALARY-71
               IF (ERROR-FOUND)
               OR (EFD-SALARY          = BNASWS-ANNUAL-SALARY)
                   INITIALIZE CRT-ERROR-NBR
                              CRT-ERROR-CAT
                              CRT-ERR-VAR1
                              CRT-ERR-VAR2
                              CRT-ERR-VAR3
                   GO TO 1642-NEXT-EMPFLEXDOL.

           IF (EFD-START-DATE          >= WS-CHANGE-DATE)
      ******** GIVE MESSAGE TO MAKE CHANGES MANUALLY
               IF (EFD-START-DATE      = WS-CHANGE-DATE)
                   MOVE 160            TO CRT-ERROR-NBR
               ELSE
                   MOVE 162            TO CRT-ERROR-NBR
               END-IF
P15764         MOVE "BN100"            TO CRT-ERROR-CAT
               PERFORM 1820-CREATE-ETEC-EFDC
               THRU    1820-END
               SET ERROR-IN-EFD        TO TRUE
               GO TO 1642-CONTINUE
           ELSE
               PERFORM 1892-FIND-FUTURE-EFR
               THRU    1892-END
               IF (ERROR-IN-EFD)
                   GO TO 1642-CONTINUE
               END-IF
               PERFORM 1846-CREATE-CS-EFDC
               THRU    1846-END

               PERFORM 1848-CREATE-CA-EFDA
               THRU    1848-END.

       1642-CONTINUE.
           MOVE EFD-COMPANY            TO DB-COMPANY.
           MOVE EFD-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE EFD-START-DATE         TO DB-EFD-START-DATE.
           MOVE BENSET7-EFD-START-DATE TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-BENSET7.

           PERFORM 1648-SEL-FLEX-BENEFIT
           THRU    1648-END
               UNTIL (BENEFIT-NOTFOUND).

           INITIALIZE CRT-ERROR-NBR
                      CRT-ERROR-CAT
                      CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3.

       1642-NEXT-EMPFLEXDOL.
PERF       PERFORM 860-FIND-NXTRNG-EFDSET5.

       1642-END.

006960******************************************************************
       1644-REPROCESS-EFDS.
006960******************************************************************

           MOVE EFD-COMPANY            TO WS-SAVE-COMPANY.
           MOVE EFD-EMPLOYEE           TO WS-SAVE-EMPLOYEE.
           MOVE EFD-FLEX-PLAN          TO WS-SAVE-FLEX-PLAN.
           MOVE EFD-START-DATE         TO WS-SAVE-START-DATE.

           MOVE EFD-COMPANY            TO DB-COMPANY.
           MOVE EFD-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE EFD-FLEX-PLAN          TO DB-FLEX-PLAN.
PERF       MOVE EFDSET4-FLEX-PLAN      TO WS-DB-BEG-RNG.
PERF       MOVE "(EFD-STOP-DATE >= ?)" TO FILTER-STRING.
PERF       PERFORM 890-CREATE-FILTER.
PERF       MOVE WS-CHANGE-DATE         TO DATETIME-FILTER-VALUE.
PERF       PERFORM 890-SET-DATETIME-FILTER-VALUE.
PERF       PERFORM 850-FILTER-BEGRNG-EFDSET4.
           PERFORM 1646-SEL-EFD-FLEX-PLAN
           THRU    1646-END
               UNTIL (EMPFLEXDOL-NOTFOUND)
               OR    (EFD-START-DATE   = WS-SAVE-START-DATE).

           MOVE WS-SAVE-COMPANY        TO DB-COMPANY.
           MOVE WS-SAVE-EMPLOYEE       TO DB-EMPLOYEE.
           MOVE WS-SAVE-FLEX-PLAN      TO DB-FLEX-PLAN.
           MOVE WS-SAVE-START-DATE     TO DB-START-DATE.
PERF  **   PERFORM 850-FIND-NLT-EFDSET4.

       1644-END.

006960******************************************************************
       1646-SEL-EFD-FLEX-PLAN.
006960******************************************************************

           INITIALIZE BNWS-PROC-DATE.

PERF  *    IF (EFD-STOP-DATE           < WS-CHANGE-DATE)
PERF  *        GO TO 1646-NEXT-EMPFLEXDOL.

           MOVE EFD-COMPANY            TO WF4-COMPANY.
           MOVE EFD-EMPLOYEE           TO WF4-EMPLOYEE.
           MOVE EFD-START-DATE         TO WF4-START-DATE.
           PERFORM 8400-FIND-BN100EFDC.
           IF (BN100EFDC-FOUND)
               GO TO 1646-NEXT-EMPFLEXDOL.

           MOVE EFD-COMPANY            TO DB-COMPANY.
           MOVE EFD-FLEX-PLAN          TO DB-FLEX-PLAN.
           MOVE EFD-FLD-START-DATE     TO DB-START-DATE.
           MOVE EFD-FLD-GROUP-NAME     TO DB-GROUP-NAME.
           PERFORM 840-FIND-FLDSET1.

           IF (FLD-CMP-BASE-PCT        NOT = ZEROES)
               GO TO 1646-PROCESS-EMPFLEXDOL.

           IF (FLD-EMP-BASE-PCT        NOT = ZEROES)
               GO TO 1646-PROCESS-EMPFLEXDOL.

           IF  (FLD-AGE-RATE-TBL       NOT = SPACES)
           AND (FLD-AGE-CREDIT         NOT = ZEROES)
               GO TO 1646-PROCESS-EMPFLEXDOL.

           IF  (FLD-SERV-RATE-TBL      NOT = SPACES)
           AND (FLD-SERV-CREDIT        NOT = ZEROES)
               GO TO 1646-PROCESS-EMPFLEXDOL.

           GO TO 1646-NEXT-EMPFLEXDOL.

       1646-PROCESS-EMPFLEXDOL.
           SET PROC-EFD                TO TRUE.

           SET NO-ERROR-IN-EFD         TO TRUE.

           MOVE BEN-START-DATE         TO BNWS-PROC-DATE.

           IF (EFD-SAL-CALC-DATE       = WS-CHANGE-DATE)
               MOVE EFD-COMPANY        TO BNASWS-COMPANY
               MOVE EFD-EMPLOYEE       TO BNASWS-EMPLOYEE
               MOVE "O"                TO BNASWS-DATE-TYPE
               MOVE FLD-SALARY-TYPE    TO BNASWS-SALARY-TYPE
               MOVE EFD-SAL-CALC-DATE  TO BNASWS-START-DATE
               PERFORM 5000-DO-ANNUAL-SALARY-71
               IF (ERROR-FOUND)
               OR (EFD-SALARY          = BNASWS-ANNUAL-SALARY)
                   INITIALIZE CRT-ERROR-NBR
                              CRT-ERROR-CAT
                              CRT-ERR-VAR1
                              CRT-ERR-VAR2
                              CRT-ERR-VAR3
                   GO TO 1646-NEXT-EMPFLEXDOL.

           IF (EFD-START-DATE          >= WS-CHANGE-DATE)
      ******** GIVE MESSAGE TO MAKE CHANGES MANUALLY
               IF (EFD-START-DATE      = WS-CHANGE-DATE)
                   MOVE 160            TO CRT-ERROR-NBR
               ELSE
                   MOVE 162            TO CRT-ERROR-NBR
               END-IF
P15764         MOVE "BN100"            TO CRT-ERROR-CAT
               PERFORM 1820-CREATE-ETEC-EFDC
               THRU    1820-END
               SET ERROR-IN-EFD        TO TRUE
               GO TO 1646-CONTINUE
           ELSE
               PERFORM 1892-FIND-FUTURE-EFR
               THRU    1892-END
               IF (ERROR-IN-EFD)
                   GO TO 1646-CONTINUE
               END-IF
               PERFORM 1846-CREATE-CS-EFDC
               THRU    1846-END

               PERFORM 1848-CREATE-CA-EFDA
               THRU    1848-END.

       1646-CONTINUE.
           MOVE EFD-COMPANY            TO DB-COMPANY.
           MOVE EFD-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE EFD-START-DATE         TO DB-EFD-START-DATE.
           MOVE BENSET7-EFD-START-DATE TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-BENSET7.

           PERFORM 1648-SEL-FLEX-BENEFIT
           THRU    1648-END
               UNTIL (BENEFIT-NOTFOUND).

           INITIALIZE CRT-ERROR-NBR
                      CRT-ERROR-CAT
                      CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3
                      BNWS-PROC-DATE.

       1646-NEXT-EMPFLEXDOL.
PERF       PERFORM 860-FIND-NXTRNG-EFDSET4.

       1646-END.

006960******************************************************************
       1648-SEL-FLEX-BENEFIT.
006960******************************************************************

           IF (BEN-FLEX-PLAN           NOT = EFD-FLEX-PLAN)
               GO TO 1648-NEXT-BENEFIT.

           MOVE BEN-COMPANY            TO WF2-COMPANY.
           MOVE BEN-EMPLOYEE           TO WF2-EMPLOYEE.
           MOVE BEN-PLAN-TYPE          TO WF2-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO WF2-PLAN-CODE.
           MOVE BEN-START-DATE         TO WF2-START-DATE.
           PERFORM 8400-FIND-BN100BENC.
           IF (BN100BENC-FOUND)
               GO TO 1648-NEXT-BENEFIT.

           SET PROC-BEN                TO TRUE.

           IF (ERROR-IN-EFD)
      ******** IF ERROR IN EFD, LIST ALL BENEFITS WITH ERROR
               PERFORM 1830-CREATE-ETEC-BENC
               THRU    1830-END
               GO TO 1648-NEXT-BENEFIT.

           IF (BEN-STOP-DATE           < WS-CHANGE-DATE)
               GO TO 1648-NEXT-BENEFIT.

           MOVE BEN-COMPANY            TO DB-COMPANY.
           MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
           PERFORM 840-FIND-PLNSET1.

           IF (BEN-START-DATE          = WS-CHANGE-DATE)
               PERFORM 1652-CALL-BNBEN
               THRU    1652-END
           ELSE
               SET NO-RECOMPUTE-BEN        TO TRUE
               IF (PLN-CONTRIB-TYPE = "1" OR "2" OR "3" OR "4")
               OR ((PLN-CONTRIB-TYPE = "5")
               AND (PLN-PLAN-TYPE    = "VA"))
                   SET RECOMPUTE-BEN       TO TRUE
               END-IF
               PERFORM 1710-STOP-READD-BENEFIT
               THRU    1710-END.

       1648-NEXT-BENEFIT.
           PERFORM 860-FIND-NXTRNG-BENSET7.

       1648-END.

006960******************************************************************
       1650-SEL-REMAINING-BEN.
006960******************************************************************

           INITIALIZE BNWS-PROC-DATE.

           IF  (BEN-STOP-DATE          < BNH-EFFECT-DATE)
           AND (BEN-STOP-DATE          NOT = ZEROES)
               GO TO 1650-NEXT-BENEFIT.

           MOVE BEN-COMPANY            TO WF2-COMPANY.
           MOVE BEN-EMPLOYEE           TO WF2-EMPLOYEE.
           MOVE BEN-PLAN-TYPE          TO WF2-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO WF2-PLAN-CODE.
           MOVE BEN-START-DATE         TO WF2-START-DATE.
           PERFORM 8400-FIND-BN100BENC.
           IF (BN100BENC-FOUND)
               IF (WF2-FUNCTION-CODE   NOT = "C" AND "CC" AND "CS")
               OR (BEN-FLEX-PLAN       NOT = SPACES)
                   GO TO 1650-NEXT-BENEFIT.

           SET NOT-SKIP-CHANGE-STOP    TO TRUE.
           IF  (BN100BENC-FOUND)
           AND (WF2-FUNCTION-CODE      = "CS")
           AND (BEN-FLEX-PLAN          = SPACES)
               SET SKIP-CHANGE-STOP    TO TRUE.

           SET PROC-BEN                TO TRUE.

      *
      **** IF COVERAGE IS PCT OR MULTIPLY OF SALARY, STOP AND RE-ADD
      **** BENEFIT
      *
           MOVE BEN-COMPANY            TO DB-COMPANY.
           MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
           PERFORM 840-FIND-PLNSET1.

           MOVE BEN-COMPANY            TO DB-COMPANY.
           MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
           MOVE "E"                    TO DB-COVER-TYPE.
           MOVE BEN-COV-UPD-DT         TO DB-START-DATE.
           MOVE BEN-COV-GROUP          TO DB-GROUP-NAME.
           PERFORM 840-FIND-CVRSET1.

      *
      **** PRE-CALC-TYPE "3" IS PERCENT OF SALARY
      **** PRE-TABLE-TYPE "2" IS SALARY RATE TABLE
      **** PRE-CONTRIB-TYPE "4" IS CONTRIBUTION BASED ON COVERAGE
      **** OR SALARY
      *
           MOVE BEN-COMPANY            TO DB-COMPANY.
           MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
           MOVE BEN-PREM-UPD-DT        TO DB-START-DATE.
           MOVE BEN-PREM-GROUP         TO DB-GROUP-NAME.
           PERFORM 840-FIND-PRESET1.

           IF (PREMIUM-FOUND)
               IF (PRE-RATE-TABLE          NOT = SPACES)
                   MOVE PRE-RATE-TABLE     TO DB-TABLE-CODE
                   MOVE PRE-START-DATE     TO DB-START-DATE
                   PERFORM 840-FIND-RTHSET1
                   MOVE RTH-TABLE-TYPE     TO PRE-TABLE-TYPE
               ELSE
               IF  (PLN-CONTRIB-TYPE       = "2")
               AND (PRE-TABLE-TYPE         = "2")
                   MOVE BEN-COMPANY        TO DB-COMPANY
                   MOVE BEN-PLAN-TYPE      TO DB-PLAN-TYPE
                   MOVE BEN-PLAN-CODE      TO DB-PLAN-CODE
                   MOVE "E"                TO DB-COVER-TYPE
                   MOVE PRE-START-DATE     TO DB-START-DATE
                   MOVE PRE-GROUP-NAME     TO DB-GROUP-NAME
                   MOVE BEN-COV-OPTION     TO DB-COVERAGE-OPT
                   PERFORM 840-FIND-PCVSET1
                   MOVE PCV-RATE-TABLE     TO DB-TABLE-CODE
                   MOVE PCV-START-DATE     TO DB-START-DATE
                   PERFORM 840-FIND-RTHSET1.

      *** Following inserted for PT# 61945, regarding salary change
      *** for contribution type 7 plan with amount emp contribution
      *** and percent company match contribution.

           INITIALIZE WS-CONT-7-SAL-CHG-SW.

           IF  (PRE-CONTRIB-TYPE = "7")
           AND (BEN-PCT-AMT-FLAG = "A")
           AND ((PLN-PRE-DED-MTCH-P NOT = SPACES)
           OR   (PLN-AFT-DED-MTCH-P NOT = SPACES))
               MOVE WS-TRUE TO WS-CONT-7-SAL-CHG-SW.
       
           IF  (((PRE-CALC-TYPE        = "3")
           OR    (PRE-TABLE-TYPE       = "2")
           OR    ((PRE-CONTRIB-TYPE    = "4")
           AND    (PRE-CONTRIB-BASIS   = "S"))))
P60050     OR  (CVR-CALC-TYPE          = "M" OR "P" OR "N")
P49316     OR  ((CVR-CALC-TYPE         = "S")
P49316     AND ((CVR-MIN-COVER         > ZEROES)
P49316     OR  (CVR-MAX-COVER          > ZEROES)
P49316     OR  (CVR-MIN-MULTIPLE       > ZEROES)
P49316     OR  (CVR-MAX-MULTIPLE       > ZEROES)))
           OR  (CONT-7-SAL-CHG)
               CONTINUE
           ELSE
               GO TO 1650-NEXT-BENEFIT.

      *
      **** FIND CHANGE RULES FOR PLAN AND ACTION CODE
      *
           SET CHANGE-RULES            TO TRUE.
           PERFORM 7000-FIND-BWT-FOR-ACTION.
           IF (ERROR-FOUND)
               PERFORM 1830-CREATE-ETEC-BENC
               THRU    1830-END
               GO TO 1650-CONTINUE.

           IF  (BEN-STOP-DATE          < WS-CHANGE-DATE)
           AND (BEN-STOP-DATE          NOT = ZEROES)
               GO TO 1650-NEXT-BENEFIT.

P60050     IF (CVR-CALC-TYPE           = "M" OR "P" OR "N")
               MOVE BEN-COMPANY        TO BNASWS-COMPANY
               MOVE BEN-EMPLOYEE       TO BNASWS-EMPLOYEE
               MOVE CVR-SALARY-TYPE    TO BNASWS-SALARY-TYPE
               MOVE CVR-SAL-FIRST-MO   TO BNASWS-DATE-TYPE
               MOVE WS-CHANGE-DATE     TO BNASWS-START-DATE
               INITIALIZE BNASWS-STOP-DATE
               MOVE CVR-SALARY-DATE    TO BNASWS-AS-OF-MMDD
               MOVE CVR-SALARY-YEAR    TO BNASWS-AS-OF-YYYY
               PERFORM 5000-DO-ANNUAL-SALARY-71
               IF (ERROR-FOUND)
               OR (BEN-PAY-RATE        = BNASWS-ANNUAL-SALARY)
                   GO TO 1650-CONTINUE
               ELSE
                   GO TO 1650-PROCESS-BENEFIT.
P49316 
P49316     IF (CVR-CALC-TYPE = "S")
P49316         MOVE BEN-COMPANY        TO BNCVWS-COMPANY
P49316         MOVE CVR-PLAN-TYPE      TO BNCVWS-PLAN-TYPE
P49316         MOVE CVR-PLAN-CODE      TO BNCVWS-PLAN-CODE
P49316         MOVE CVR-COVER-TYPE     TO BNCVWS-COVER-TYPE
P49316         MOVE BEN-EMPLOYEE       TO BNCVWS-EMPLOYEE
P49316         MOVE BEN-START-DATE     TO BNCVWS-START-DATE
P49316                                    BNCVWS-AS-OF-DATE
P49316         MOVE BEN-STOP-DATE      TO BNCVWS-STOP-DATE
P49316         MOVE BEN-COV-OPTION     TO BNCVWS-IN-COVER-OPT
P49316         MOVE BEN-COVER-AMT      TO BNCVWS-IN-COVER-AMT
P49316         MOVE BEN-MULTIPLE       TO BNCVWS-MULT-OF-SALARY
P49316         MOVE BEN-PAY-RATE       TO BNCVWS-IN-ANNUAL-SALARY
P49316         MOVE BEN-COV-UPD-DT     TO BNCVWS-COV-UPD-DT
P49316         MOVE BEN-COV-GROUP      TO BNCVWS-COV-GROUP
P49316         INITIALIZE                 BNCVWS-FC
P49316                                    BNCVWS-CALL-FROM-BS
P49316         IF  (PLN-CONTRIB-TYPE        = "5" OR "6" OR "7")
P49316             MOVE BEN-CYCLES-REMAIN
P49316                                 TO BNCVWS-IN-COVER-OPT
P49316             MOVE BEN-BOND-DED-AMT
P49316                                 TO BNCVWS-IN-COVER-AMT
P49316             MOVE BEN-ANNUAL-AMT TO BNCVWS-IN-ANNUAL-SALARY
P49316         END-IF
P49316         IF  (BEN-PLAN-TYPE = "VA")
P49316             MOVE BEN-NBR-HOURS  TO BNCVWS-MULT-OF-SALARY
P49316         END-IF
P49316         PERFORM 5000-DO-COVERAGE-70
P49316         IF  (ERROR-FOUND)
P49316             PERFORM 1830-CREATE-ETEC-BENC
P49316             THRU    1830-END
P49316             GO TO 1650-CONTINUE
P49316         ELSE
P49316         IF (BEN-COVER-AMT = BNCVWS-COVER-AMT)
P49316             GO TO 1650-CONTINUE
P49316         ELSE
P49316            GO TO 1650-PROCESS-BENEFIT
P49316         END-IF
P49316         END-IF
P49316     END-IF.

           IF  (((PRE-CALC-TYPE        = "3")
           OR    (PRE-TABLE-TYPE       = "2")
           OR    ((PRE-CONTRIB-TYPE    = "4")
           AND    (PRE-CONTRIB-BASIS   = "S"))))
           OR  (CONT-7-SAL-CHG)
               MOVE BEN-COMPANY        TO BNASWS-COMPANY
               MOVE BEN-EMPLOYEE       TO BNASWS-EMPLOYEE
               MOVE PRE-SALARY-TYPE    TO BNASWS-SALARY-TYPE
               MOVE PRE-SAL-FIRST-MO   TO BNASWS-DATE-TYPE
               MOVE WS-CHANGE-DATE     TO BNASWS-START-DATE
               INITIALIZE BNASWS-STOP-DATE
               MOVE PRE-SALARY-DATE    TO BNASWS-AS-OF-MMDD
               MOVE PRE-SALARY-YEAR    TO BNASWS-AS-OF-YYYY
               PERFORM 5000-DO-ANNUAL-SALARY-71
               IF (ERROR-FOUND)
               OR (BEN-PAY-RATE        = BNASWS-ANNUAL-SALARY)
                   GO TO 1650-CONTINUE
               ELSE
               IF  (PRE-CALC-TYPE      = "3")
               OR  ((PRE-CONTRIB-TYPE  = "4")
               AND  (PRE-CONTRIB-BASIS = "S"))
                   GO TO 1650-PROCESS-BENEFIT
               ELSE
      ******** If salary table is used we want to see if rates are different
      ******** because of salary change
               IF (PRE-TABLE-TYPE      = "2")
                   MOVE BNASWS-ANNUAL-SALARY TO WS-AGE-YR-SAL
                   PERFORM 1656-FIND-CURR-RATETBLDTL
                   THRU    1656-END
                   IF (ERROR-FOUND)
                       PERFORM 1830-CREATE-ETEC-BENC
                       THRU    1830-END
                       GO TO 1650-CONTINUE
                   END-IF
                   MOVE WS-RTD-AGE-YR-SAL    TO WS-CURR-AGE-YR-SAL

                   MOVE BEN-START-DATE       TO BNWS-PROC-DATE
                   MOVE BEN-COMPANY          TO BNASWS-COMPANY
                   MOVE BEN-EMPLOYEE         TO BNASWS-EMPLOYEE
                   MOVE "O"                  TO BNASWS-DATE-TYPE
                   MOVE PRE-SALARY-TYPE      TO BNASWS-SALARY-TYPE
P64496             MOVE PRE-START-DATE       TO BNASWS-START-DATE
                   PERFORM 5000-DO-ANNUAL-SALARY-71
                   INITIALIZE BNWS-PROC-DATE
                   IF (ERROR-FOUND)
                       PERFORM 1830-CREATE-ETEC-BENC
                       THRU    1830-END
                       GO TO 1650-CONTINUE
                   END-IF
                   MOVE BNASWS-ANNUAL-SALARY TO WS-AGE-YR-SAL
                   PERFORM 1656-FIND-CURR-RATETBLDTL
                   THRU    1656-END
                   IF (ERROR-FOUND)
                       PERFORM 1830-CREATE-ETEC-BENC
                       THRU    1830-END
                       GO TO 1650-CONTINUE
                   END-IF
                   IF (WS-RTD-AGE-YR-SAL     = WS-CURR-AGE-YR-SAL)
                       GO TO 1650-CONTINUE
                   ELSE
                       GO TO 1650-PROCESS-BENEFIT.

           GO TO 1650-NEXT-BENEFIT.

       1650-PROCESS-BENEFIT.
           IF (BN100BENC-FOUND)
               PERFORM 1651-DO-MULT-CHANGES
               THRU    1651-END.

           IF  (BN100BENC-FOUND)
           AND (WF2-FUNCTION-CODE      = "CC")
               MOVE WS-CHANGE-DATE     TO BNWS-PROC-DATE
           ELSE
               MOVE BEN-START-DATE     TO BNWS-PROC-DATE.

           IF (BEN-COV-OVER-FLG        = "Y")
               MOVE 133                TO CRT-ERROR-NBR
P56073         MOVE "BN100"            TO CRT-ERROR-CAT
               PERFORM 1830-CREATE-ETEC-BENC
               THRU    1830-END
               GO TO 1650-CONTINUE.

           IF (BEN-START-DATE          >= WS-CHANGE-DATE)
               PERFORM 1652-CALL-BNBEN
               THRU    1652-END
           ELSE
               SET NO-RECOMPUTE-BEN        TO TRUE
               IF  (PLN-COVERAGE-TYPE      = "2")
P60050         AND (CVR-CALC-TYPE          = "M" OR "P" OR "N")
               AND (BEN-COV-OVER-FLG       NOT = "Y")
                   SET RECOMPUTE-BEN       TO TRUE
               END-IF
               IF (PLN-CONTRIB-TYPE = "1" OR "2" OR "3" OR "4")
                   SET RECOMPUTE-BEN       TO TRUE
               END-IF
               PERFORM 1710-STOP-READD-BENEFIT
               THRU    1710-END.

           SET NOT-SKIP-CHANGE-STOP        TO TRUE.
           MOVE WS-CHANGE-DATE             TO WS-PREV-CHANGE-DATE.

       1650-CONTINUE.
           INITIALIZE CRT-ERROR-NBR
                      CRT-ERROR-CAT
                      CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3
                      BNWS-PROC-DATE.

       1650-NEXT-BENEFIT.
           PERFORM 860-FIND-NXTRNG-BENSET7.

       1650-END.

006960******************************************************************
       1651-DO-MULT-CHANGES.
006960******************************************************************

           IF (WF2-FUNCTION-CODE           = "C")
               IF  (WF2-START-DATE         < WS-CHANGE-DATE)
               AND ((WF2-STOP-DATE         = ZEROES)
               OR   (WF2-STOP-DATE         > WS-CHANGE-DATE))
                   MOVE "CC"               TO WF2-FUNCTION-CODE

                   MOVE WS-CHANGE-DATE     TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   SUBTRACT 1              FROM WSDR-JULIAN-DAYS
                   PERFORM 900-JULIAN-TO-DATE
                   MOVE WSDR-FR-DATE       TO WF2-STOP-DATE
                   PERFORM 8200-REWRITE-BN100BENC
               ELSE
                   MOVE "3"                TO WF-REC-TYPE
                   MOVE WF2-WF-EFFECT-DATE TO WF-EFFECT-DATE
                   MOVE WF2-WF-SEQ-NBR     TO WF-SEQ-NBR
                   PERFORM 8300-DELETE-BN100WORK
                   PERFORM 8300-DELETE-BN100BENC
               END-IF
           ELSE
           IF (WF2-FUNCTION-CODE           = "CC")
               MOVE BEN-COMPANY            TO WF1-COMPANY
               MOVE BEN-EMPLOYEE           TO WF1-EMPLOYEE
               MOVE BEN-PLAN-TYPE          TO WF1-PLAN-TYPE
               MOVE BEN-PLAN-CODE          TO WF1-PLAN-CODE

               MOVE WF2-STOP-DATE          TO WSDR-FR-DATE
               PERFORM 900-DATE-TO-JULIAN
               ADD 1                       TO WSDR-JULIAN-DAYS
               PERFORM 900-JULIAN-TO-DATE
               MOVE WSDR-FR-DATE           TO WF1-START-DATE

               PERFORM 8400-FIND-BN100BENA
               IF (BN100BENA-FOUND)
                   MOVE WS-CHANGE-DATE     TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   SUBTRACT 1              FROM WSDR-JULIAN-DAYS
                   PERFORM 900-JULIAN-TO-DATE
                   IF (WSDR-FR-DATE        >= WF1-START-DATE)
                       MOVE WSDR-FR-DATE   TO WF1-STOP-DATE

                       PERFORM 8200-REWRITE-BN100BENA
                   END-IF
               END-IF
           ELSE
               MOVE BEN-COMPANY            TO WF1-COMPANY
               MOVE BEN-EMPLOYEE           TO WF1-EMPLOYEE
               MOVE BEN-PLAN-TYPE          TO WF1-PLAN-TYPE
               MOVE BEN-PLAN-CODE          TO WF1-PLAN-CODE
               MOVE WS-PREV-CHANGE-DATE    TO WF1-START-DATE
               PERFORM 8400-FIND-BN100BENA
               IF (BN100BENA-FOUND)
                   MOVE WS-CHANGE-DATE     TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   SUBTRACT 1              FROM WSDR-JULIAN-DAYS
                   PERFORM 900-JULIAN-TO-DATE
                   IF (WSDR-FR-DATE        >= WF1-START-DATE)
                       MOVE WSDR-FR-DATE   TO WF1-STOP-DATE

                       PERFORM 8200-REWRITE-BN100BENA.

       1651-END.

006960******************************************************************
       1652-CALL-BNBEN.
006960******************************************************************

           MOVE BEN-COMPANY            TO WS-SAVE-COMPANY.
           MOVE BEN-PLAN-TYPE          TO WS-SAVE-PLAN-TYPE.
           MOVE BEN-EMPLOYEE           TO WS-SAVE-EMPLOYEE.
           MOVE BEN-PLAN-CODE          TO WS-SAVE-PLAN-CODE.
           MOVE BEN-START-DATE         TO WS-SAVE-START-DATE.

           PERFORM 1654-MOVE-TO-BNBEN
           THRU    1654-END.

           PERFORM 2000-BNBEN-EDIT-TRAN.

           MOVE WS-SAVE-COMPANY        TO DB-COMPANY.
           MOVE WS-SAVE-PLAN-TYPE      TO DB-PLAN-TYPE.
           MOVE WS-SAVE-EMPLOYEE       TO DB-EMPLOYEE.
           MOVE WS-SAVE-PLAN-CODE      TO DB-PLAN-CODE.
           MOVE WS-SAVE-START-DATE     TO DB-START-DATE.
           PERFORM 840-FIND-BENSET1.

           IF (ERROR-FOUND)
               PERFORM 1830-CREATE-ETEC-BENC
               THRU    1830-END

               IF (WS-FLEX-FLAG        = "Y")
                   MOVE BEN-PLAN-TYPE  TO WS-SV-PLAN-TYPE
                   MOVE BEN-PLAN-CODE  TO WS-SV-PLAN-CODE
                   MOVE BEN-START-DATE TO WS-SV-START-DATE
                   PERFORM 1718-PUT-OTHERS-IN-ERROR
                   THRU    1718-END
                   MOVE EMP-COMPANY    TO DB-COMPANY
                   MOVE EMP-EMPLOYEE   TO DB-EMPLOYEE
                   MOVE EFD-START-DATE TO DB-EFD-START-DATE
                   MOVE BENSET7-EFD-START-DATE TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-BENSET7
                   PERFORM 860-FIND-NXTRNG-BENSET7
                       UNTIL (BENEFIT-NOTFOUND)
                       OR    ((BEN-PLAN-TYPE  = WS-SV-PLAN-TYPE)
                       AND    (BEN-PLAN-CODE  = WS-SV-PLAN-CODE)
                       AND    (BEN-START-DATE = WS-SV-START-DATE))
               END-IF
           ELSE
               PERFORM 1870-CREATE-C-BENC
               THRU    1870-END.

       1652-END.

006960******************************************************************
       1654-MOVE-TO-BNBEN.
006960******************************************************************

002000     INITIALIZE BNBEN-DETAIL-GROUP
117816                BNBEN-PROC-FLEX-SW
002000                BNBEN-USE-NAVIGATE-SW.

           INITIALIZE WS-FLEX-FLAG.
           IF (PLN-FLEX-PLAN           NOT = SPACES)
               MOVE PLN-FLEX-PLAN      TO DB-FLEX-PLAN
               PERFORM 840-FIND-FLPSET1
               IF (FLP-SPEND-ONLY      = "N")
                   MOVE "Y"            TO WS-FLEX-FLAG.

           MOVE BEN-COMPANY            TO BNBEN-COMPANY.
           MOVE BEN-EMPLOYEE           TO BNBEN-EMPLOYEE (1).
           MOVE BEN-PLAN-TYPE          TO BNBEN-PLAN-TYPE (1).
           MOVE BEN-PLAN-CODE          TO BNBEN-PLAN-CODE (1).

           IF (PLN-COVERAGE-TYPE       = "2")
               MOVE BEN-COMPANY        TO DB-COMPANY
               MOVE BEN-PLAN-TYPE      TO DB-PLAN-TYPE
               MOVE BEN-PLAN-CODE      TO DB-PLAN-CODE
               MOVE "E"                TO DB-COVER-TYPE
               MOVE BEN-COV-UPD-DT     TO DB-START-DATE
               MOVE BEN-COV-GROUP      TO DB-GROUP-NAME
               PERFORM 840-FIND-CVRSET1.

           IF (BEN-START-DATE          >= WS-CHANGE-DATE)
               MOVE BEN-START-DATE     TO BNBEN-START-DATE (1)
                                          BNBEN-PT-START-DATE (1)
           ELSE
               MOVE WS-CHANGE-DATE     TO BNBEN-START-DATE (1).

184000     MOVE BEN-STOP-DATE          TO BNBEN-STOP-DATE (1).

184000     MOVE BEN-COV-OPTION         TO BNBEN-COVER-OPT (1).

           IF (PLN-CONTRIB-TYPE        = "5" OR "6" OR "7")
               MOVE BEN-CYCLES-REMAIN  TO BNBEN-COVER-OPT (1).

           MOVE BEN-COVER-AMT          TO BNBEN-COVER-AMT (1).

           IF  (PLN-COVERAGE-TYPE      = "2")
P60050     AND (CVR-CALC-TYPE          = "M" OR "P" OR "F" OR "N")
               INITIALIZE BNBEN-COVER-AMT (1).

           IF (PLN-CONTRIB-TYPE        = "5" OR "6" OR "7")
               MOVE BEN-BOND-DED-AMT   TO BNBEN-COVER-AMT (1)
               MOVE BEN-ANNUAL-AMT     TO BNBEN-PAY-RATE (1)
           ELSE
               MOVE BEN-PAY-RATE       TO BNBEN-PAY-RATE (1).

           COMPUTE BNBEN-EMP-PRE-CONT (1) = BEN-EMP-PRE-CONT
                                          + BEN-CMP-FLX-CONT.
           MOVE BEN-EMP-AFT-CONT       TO BNBEN-EMP-AFT-CONT (1).
           MOVE BEN-CMP-FLX-CONT       TO BNBEN-CMP-FLX-CONT (1).
           MOVE BEN-COMP-CONT          TO BNBEN-COMP-CONT (1).

           IF (PLN-CONTRIB-TYPE        NOT = "6" AND "7")
               INITIALIZE BNBEN-PAY-RATE     (1)
                          BNBEN-EMP-PRE-CONT (1)
                          BNBEN-EMP-AFT-CONT (1)
                          BNBEN-CMP-FLX-CONT (1)
                          BNBEN-COMP-CONT    (1).

           MOVE BEN-PCT-AMT-FLAG       TO BNBEN-PCT-AMT-FLAG (1).
           MOVE BEN-SMOKER             TO BNBEN-SMOKER-FLAG (1).

           IF (BEN-PLAN-TYPE           = "VA")
               MOVE BEN-NBR-HOURS      TO BNBEN-MULT-SALARY (1)
P60050     ELSE
P64594     IF (CVR-CALC-TYPE           = "N" )
P60050         INITIALIZE                 BNBEN-MULT-SALARY (1)
           ELSE
P60050         MOVE BEN-MULTIPLE       TO BNBEN-MULT-SALARY (1)
P60050     END-IF
P60050     END-IF.

           MOVE BEN-PRE-AFT-FLAG       TO BNBEN-PRE-AFT-FLAG (1).

           MOVE "C"                    TO BNBEN-FC.

           IF (BEN-START-DATE          >= WS-CHANGE-DATE)
               MOVE "C"                TO BNBEN-LINE-FC (1)
P51666         MOVE BEN-DED-START-DATE TO BNBEN-DED-START-DATE (1)
P51666         MOVE BEN-DED-STOP-DATE  TO BNBEN-DED-STOP-DATE (1)
           ELSE
               MOVE "A"                TO BNBEN-LINE-FC (1).

           MOVE 1                      TO BNBEN-NBR-LINES.

           MOVE "Y"                    TO BNBEN-SKIP-ELIGIBILITY (1).

           MOVE PRM-CREATE-TRANS       TO BNBEN-CREATE-TRANS (1).

117816     MOVE WS-PROC-FLEX-SW        TO BNBEN-PROC-FLEX-SW.

       1654-END.

194800******************************************************************
194900 1656-FIND-CURR-RATETBLDTL.
195000******************************************************************
195100
195200     MOVE RTH-COMPANY            TO DB-COMPANY.
195300     MOVE RTH-TABLE-CODE         TO DB-TABLE-CODE.
195400     MOVE RTH-START-DATE         TO DB-START-DATE.
195500     MOVE BEN-SMOKER             TO DB-SMOKER.
195600     MOVE RTDSET2-SMOKER         TO WS-DB-BEG-RNG.
195700     PERFORM 850-FIND-BEGRNG-RTDSET2.
195800     PERFORM
195900         UNTIL (RATETBLDTL-NOTFOUND)
196000         OR    (RTD-BEG-AGE-YR-SAL  <= WS-AGE-YR-SAL)
196100
196400         PERFORM 860-FIND-NXTRNG-RTDSET2
196500     END-PERFORM.
196600
196700     IF (RATETBLDTL-NOTFOUND)
196800         IF (BEN-SMOKER          = "Y" OR "N")
196900             INITIALIZE DB-SMOKER 
197000             MOVE RTDSET2-SMOKER TO WS-DB-BEG-RNG
197100             PERFORM 850-FIND-BEGRNG-RTDSET2
197200             PERFORM
197300                 UNTIL (RATETBLDTL-NOTFOUND)
197400                 OR    (RTD-BEG-AGE-YR-SAL <= WS-AGE-YR-SAL)
197500
197800                 PERFORM 860-FIND-NXTRNG-RTDSET2
197900             END-PERFORM
198000         ELSE
198100             MOVE "N"            TO DB-SMOKER 
198200             MOVE RTDSET2-SMOKER TO WS-DB-BEG-RNG
198300             PERFORM 850-FIND-BEGRNG-RTDSET2
198400             PERFORM
198500                 UNTIL (RATETBLDTL-NOTFOUND)
198600                 OR    (RTD-BEG-AGE-YR-SAL <= WS-AGE-YR-SAL)
198700
199000                 PERFORM 860-FIND-NXTRNG-RTDSET2
199100             END-PERFORM
199200             IF (RATETBLDTL-NOTFOUND)
199300                 MOVE "Y"            TO DB-SMOKER 
199400                 MOVE RTDSET2-SMOKER TO WS-DB-BEG-RNG
199500                 PERFORM 850-FIND-BEGRNG-RTDSET2
199600                 PERFORM
199700                     UNTIL (RATETBLDTL-NOTFOUND)
199800                     OR    (RTD-BEG-AGE-YR-SAL 
199900                                     <= WS-AGE-YR-SAL)
200000
200300                     PERFORM 860-FIND-NXTRNG-RTDSET2
200400                 END-PERFORM.
200500
200600     IF (RATETBLDTL-FOUND)
196200         MOVE RTD-BEG-AGE-YR-SAL     TO WS-RTD-AGE-YR-SAL
           ELSE
      ******** Salary rate table does not exist
200700         MOVE 169                    TO CRT-ERROR-NBR
               MOVE DB-TABLE-CODE          TO CRT-ERR-VAR1
               MOVE WS-START-DATE          TO HRWS-DATE-FIELD
               INITIALIZE HRWS-DATE-8-FIELD
               PERFORM 781-HR-FORMAT-DATE-FIELD
               MOVE HRWS-VALUE             TO CRT-ERR-VAR2
200800         GO TO 1656-END.
200900
201000 1656-END.
201100
006960******************************************************************
       1660-PAY-FREQ-CHANGES.
006960******************************************************************

J43365     IF (SELECT-COMPANY)
J43365          MOVE EMP-COMPANY            TO CRT-COMPANY
J43365          MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL
J43365          PERFORM 700-HR-EMP-SECURITY
J43365          IF (HRWS-EMP-SECURED)
J43365              GO TO 1660-NEXT-BNCHANGE
J43365          END-IF
J43365     END-IF.
J43365
           SET PAY-FREQ-CHANGES        TO TRUE.

           IF (BNH-EFFECT-DATE         > PRM-THRU-DATE)
               GO TO 1660-NEXT-BNCHANGE.

           PERFORM 1662-CHECK-MULTIPLES
           THRU    1662-END.

           SET PROC-OFF                TO TRUE.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           MOVE BNH-COMPANY            TO DB-COMPANY.
           MOVE BNH-EMPLOYEE           TO DB-EMPLOYEE.
PERF       MOVE EFDSET5-EMPLOYEE       TO WS-DB-BEG-RNG.
PERF       MOVE "(EFD-STOP-DATE >= ?)" TO FILTER-STRING.
PERF       PERFORM 890-CREATE-FILTER.
PERF       MOVE BNH-EFFECT-DATE        TO DATETIME-FILTER-VALUE.
PERF       PERFORM 890-SET-DATETIME-FILTER-VALUE.
PERF       PERFORM 850-FILTER-BEGRNG-EFDSET5.
           PERFORM 1670-SEL-EFD-EMPLOYEE
           THRU    1670-END
PERF           UNTIL (EMPFLEXDOL-NOTFOUND).

           MOVE BNH-COMPANY            TO DB-COMPANY.
           MOVE BNH-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE BENSET7-EMPLOYEE       TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-BENSET7.
           PERFORM 1680-SEL-REMAINING-BEN
           THRU    1680-END
               UNTIL (BENEFIT-NOTFOUND).

       1660-NEXT-BNCHANGE.
           IF (BNH-EFFECT-DATE         > PRM-THRU-DATE)
J43365     OR ((SELECT-COMPANY)
J43365     AND (HRWS-EMP-SECURED))
               PERFORM 860-FIND-NEXT-BNHSET2
           ELSE
               MOVE WS-SAV-COMPANY      TO DB-COMPANY
               MOVE WS-SV-EMPLOYEE     TO DB-EMPLOYEE
               MOVE WS-SV-EFFECT-DATE  TO DB-EFFECT-DATE
               MOVE WS-SV-CHANGE-TYPE  TO DB-CHANGE-TYPE
               MOVE WS-SV-SEQ-NBR      TO DB-SEQ-NBR
               PERFORM 850-FIND-NLT-BNHSET2.

           PERFORM 1900-SET-PROC-BNG-BNH-SW
           THRU    1900-END.

       1660-END.

006960******************************************************************
       1662-CHECK-MULTIPLES.
006960******************************************************************

      *
      **** CHECK TO SEE IF WE HAVE MORE THAN ONE PAY-FREQ CHANGES FOR
      **** SAME EFFECTIVE-DATE, AND USE OLD VALUE OF FIRST PAY FREQ 
      **** CHANGE RECORD WITH NEW VALUE OF LAST PAY FREQ CHANGE RECORD
      **** FOR COMPARISON
      *
           MOVE BNH-COMPANY            TO WS-SAV-COMPANY.
           MOVE BNH-EMPLOYEE           TO WS-SV-EMPLOYEE.
           MOVE BNH-EFFECT-DATE        TO WS-SV-EFFECT-DATE.
           MOVE BNH-CHANGE-TYPE        TO WS-SV-CHANGE-TYPE.
           MOVE WS-HIGH-VALUES         TO WS-SV-SEQ-NBR.

           MOVE BNH-OLD-VALUE          TO WS-OLD-VALUE.
           MOVE BNH-NEW-VALUE          TO WS-NEW-VALUE.

           PERFORM
               UNTIL (BNCHANGE-NOTFOUND)
               OR    (BNH-COMPANY      NOT = WS-COMPANY)
               OR    (BNH-EMPLOYEE     NOT = WS-EMPLOYEE)
               OR    (BNH-EFFECT-DATE  NOT = WS-EFFECT-DATE)
               OR    (BNH-CHANGE-TYPE  NOT = "3")

               MOVE BNH-COMPANY        TO WS-SAVE-COMPANY
               MOVE BNH-EMPLOYEE       TO WS-SAVE-EMPLOYEE
               MOVE BNH-EFFECT-DATE    TO WS-SAVE-EFFECT-DATE
               MOVE BNH-CHANGE-TYPE    TO WS-SAVE-CHANGE-TYPE
               MOVE BNH-SEQ-NBR        TO WS-SAVE-SEQ-NBR

               MOVE BNH-NEW-VALUE      TO WS-NEW-VALUE

               PERFORM 860-FIND-NEXT-BNHSET2
           END-PERFORM.

           MOVE WS-SAVE-COMPANY        TO DB-COMPANY.
           MOVE WS-SAVE-EMPLOYEE       TO DB-EMPLOYEE.
           MOVE WS-SAVE-EFFECT-DATE    TO DB-EFFECT-DATE.
           MOVE WS-SAVE-CHANGE-TYPE    TO DB-CHANGE-TYPE.
           MOVE WS-SAVE-SEQ-NBR        TO DB-SEQ-NBR.
           PERFORM 850-FIND-NLT-BNHSET2.

       1662-END.

006960******************************************************************
       1670-SEL-EFD-EMPLOYEE.
006960******************************************************************

           MOVE EFD-FLEX-PLAN          TO WS-FLEX-PLAN.

           MOVE EFD-COMPANY            TO DB-COMPANY.
           MOVE EFD-FLEX-PLAN          TO DB-FLEX-PLAN.
           PERFORM 840-FIND-FLPSET1.

           MOVE FLP-COMPANY            TO WS-COMPANY.
           MOVE FLP-DED-TABLE          TO WS-DED-TABLE.

           PERFORM 1682-CHECK-PAY-FREQ-CHG
           THRU    1682-END.

           IF (PAY-FREQ-CHG)
               PERFORM 1672-SEL-EFD-FLEX-PLAN
               THRU    1672-END
                   UNTIL (EMPFLEXDOL-NOTFOUND)
                   OR    (EFD-COMPANY      NOT = WS-COMPANY)
                   OR    (EFD-EMPLOYEE     NOT = WS-EMPLOYEE)
                   OR    (EFD-FLEX-PLAN    NOT = WS-FLEX-PLAN)
           ELSE
               PERFORM
                   UNTIL (EMPFLEXDOL-NOTFOUND)
                   OR    (EFD-COMPANY      NOT = WS-COMPANY)
                   OR    (EFD-EMPLOYEE     NOT = WS-EMPLOYEE)
                   OR    (EFD-FLEX-PLAN    NOT = WS-FLEX-PLAN)

PERF               PERFORM 860-FIND-NXTRNG-EFDSET5
               END-PERFORM.

       1670-END.

006960******************************************************************
       1672-SEL-EFD-FLEX-PLAN.
006960******************************************************************

PERF  *    IF (EFD-STOP-DATE           < BNH-EFFECT-DATE)
PERF  *        GO TO 1672-NEXT-EMPFLEXDOL.

           MOVE EFD-COMPANY            TO WF4-COMPANY.
           MOVE EFD-EMPLOYEE           TO WF4-EMPLOYEE.
           MOVE EFD-START-DATE         TO WF4-START-DATE.
           PERFORM 8400-FIND-BN100EFDC.
           IF (BN100EFDC-FOUND)
               GO TO 1672-NEXT-EMPFLEXDOL.

           SET PROC-EFD                TO TRUE.

           SET NO-ERROR-IN-EFD         TO TRUE.

      *
      **** FIND CHANGE RULES FOR FLEX PLAN AND ACTION CODE
      *
           SET CHANGE-RULES            TO TRUE.
           PERFORM 7000-FIND-BWT-FOR-ACTION.
           IF (ERROR-FOUND)
               PERFORM 1820-CREATE-ETEC-EFDC
               THRU    1820-END
               SET ERROR-IN-EFD        TO TRUE
               MOVE 164                TO CRT-ERROR-NBR
P15764         MOVE "BN100"            TO CRT-ERROR-CAT
               INITIALIZE CRT-ERR-VAR1
                          CRT-ERR-VAR2
                          CRT-ERR-VAR3
                          CRT-ERR-VAR4
                          CRT-ERR-VAR5
               GO TO 1672-CONTINUE.

           MOVE EFD-COMPANY            TO BNREWS-COMPANY.
           MOVE "FL"                   TO BNREWS-PLAN-TYPE.
           MOVE EFD-FLEX-PLAN          TO BNREWS-PLAN-CODE.
           MOVE EFD-EMPLOYEE           TO BNREWS-EMPLOYEE.
           MOVE EFD-START-DATE         TO BNREWS-AS-OF-DATE.
           IF (WS-CHANGE-DATE          > EFD-START-DATE)
               MOVE WS-CHANGE-DATE     TO BNREWS-AS-OF-DATE.
           MOVE "FLD"                  TO BNREWS-FILE-PREFIX.
           PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70.
           IF (ERROR-FOUND)
               PERFORM 1820-CREATE-ETEC-EFDC
               THRU    1820-END
               SET ERROR-IN-EFD        TO TRUE
               MOVE 164                TO CRT-ERROR-NBR
P15764         MOVE "BN100"            TO CRT-ERROR-CAT
               INITIALIZE CRT-ERR-VAR1
                          CRT-ERR-VAR2
                          CRT-ERR-VAR3
                          CRT-ERR-VAR4
                          CRT-ERR-VAR5
               GO TO 1672-CONTINUE.

           IF (WS-CHANGE-DATE          < BNH-EFFECT-DATE)
               PERFORM 1674-REPROCESS-EFDS
               THRU    1674-END.

           IF (EFD-STOP-DATE           < WS-CHANGE-DATE)
               GO TO 1672-NEXT-EMPFLEXDOL.

           IF (EFD-START-DATE          >= WS-CHANGE-DATE)
      ******** GIVE MESSAGE TO MAKE CHANGES MANUALLY
               IF (EFD-START-DATE      = WS-CHANGE-DATE)
                   MOVE 160            TO CRT-ERROR-NBR
               ELSE
                   MOVE 162            TO CRT-ERROR-NBR
               END-IF
P15764         MOVE "BN100"            TO CRT-ERROR-CAT
               PERFORM 1820-CREATE-ETEC-EFDC
               THRU    1820-END
               SET ERROR-IN-EFD        TO TRUE
               GO TO 1672-CONTINUE
           ELSE
               PERFORM 1892-FIND-FUTURE-EFR
               THRU    1892-END
               IF (ERROR-IN-EFD)
                   GO TO 1672-CONTINUE
               END-IF
               PERFORM 1846-CREATE-CS-EFDC
               THRU    1846-END

               PERFORM 1848-CREATE-CA-EFDA
               THRU    1848-END.

       1672-CONTINUE.
           MOVE EFD-COMPANY            TO DB-COMPANY.
           MOVE EFD-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE EFD-START-DATE         TO DB-EFD-START-DATE.
           MOVE BENSET7-EFD-START-DATE TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-BENSET7.

           PERFORM 1678-SEL-FLEX-BENEFIT
           THRU    1678-END
               UNTIL (BENEFIT-NOTFOUND).

           INITIALIZE CRT-ERROR-NBR
                      CRT-ERROR-CAT
                      CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3.

       1672-NEXT-EMPFLEXDOL.
PERF       PERFORM 860-FIND-NXTRNG-EFDSET5.

       1672-END.

006960******************************************************************
       1674-REPROCESS-EFDS.
006960******************************************************************

           MOVE EFD-COMPANY            TO WS-SAVE-COMPANY.
           MOVE EFD-EMPLOYEE           TO WS-SAVE-EMPLOYEE.
           MOVE EFD-FLEX-PLAN          TO WS-SAVE-FLEX-PLAN.
           MOVE EFD-START-DATE         TO WS-SAVE-START-DATE.

           MOVE EFD-COMPANY            TO DB-COMPANY.
           MOVE EFD-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE EFD-FLEX-PLAN          TO DB-FLEX-PLAN.
PERF       MOVE EFDSET4-FLEX-PLAN      TO WS-DB-BEG-RNG.
PERF       MOVE "(EFD-STOP-DATE >= ?)" TO FILTER-STRING.
PERF       PERFORM 890-CREATE-FILTER.
PERF       MOVE WS-CHANGE-DATE         TO DATETIME-FILTER-VALUE.
PERF       PERFORM 890-SET-DATETIME-FILTER-VALUE.
PERF       PERFORM 850-FILTER-BEGRNG-EFDSET4.
           PERFORM 1676-SEL-EFD-FLEX-PLAN
           THRU    1676-END
               UNTIL (EMPFLEXDOL-NOTFOUND)
               OR    (EFD-START-DATE   = WS-SAVE-START-DATE).

           MOVE WS-SAVE-COMPANY        TO DB-COMPANY.
           MOVE WS-SAVE-EMPLOYEE       TO DB-EMPLOYEE.
           MOVE WS-SAVE-FLEX-PLAN      TO DB-FLEX-PLAN.
           MOVE WS-SAVE-START-DATE     TO DB-START-DATE.
PERF  **   PERFORM 850-FIND-NLT-EFDSET4.

       1674-END.

006960******************************************************************
       1676-SEL-EFD-FLEX-PLAN.
006960******************************************************************

PERF  *    IF (EFD-STOP-DATE           < WS-CHANGE-DATE)
PERF  *        GO TO 1676-NEXT-EMPFLEXDOL.

           MOVE EFD-COMPANY            TO WF4-COMPANY.
           MOVE EFD-EMPLOYEE           TO WF4-EMPLOYEE.
           MOVE EFD-START-DATE         TO WF4-START-DATE.
           PERFORM 8400-FIND-BN100EFDC.
           IF (BN100EFDC-FOUND)
               GO TO 1676-NEXT-EMPFLEXDOL.

           SET PROC-EFD                TO TRUE.

           SET NO-ERROR-IN-EFD         TO TRUE.

           IF (EFD-START-DATE          >= WS-CHANGE-DATE)
      ******** GIVE MESSAGE TO MAKE CHANGES MANUALLY
               IF (EFD-START-DATE      = WS-CHANGE-DATE)
                   MOVE 160            TO CRT-ERROR-NBR
               ELSE
                   MOVE 162            TO CRT-ERROR-NBR
               END-IF
P15764         MOVE "BN100"            TO CRT-ERROR-CAT
               PERFORM 1820-CREATE-ETEC-EFDC
               THRU    1820-END
               SET ERROR-IN-EFD        TO TRUE
               GO TO 1676-CONTINUE
           ELSE
               PERFORM 1892-FIND-FUTURE-EFR
               THRU    1892-END
               IF (ERROR-IN-EFD)
                   GO TO 1676-CONTINUE
               END-IF
               PERFORM 1846-CREATE-CS-EFDC
               THRU    1846-END

               PERFORM 1848-CREATE-CA-EFDA
               THRU    1848-END.

       1676-CONTINUE.
           MOVE EFD-COMPANY            TO DB-COMPANY.
           MOVE EFD-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE EFD-START-DATE         TO DB-EFD-START-DATE.
           MOVE BENSET7-EFD-START-DATE TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-BENSET7.

           PERFORM 1678-SEL-FLEX-BENEFIT
           THRU    1678-END
               UNTIL (BENEFIT-NOTFOUND).

           INITIALIZE CRT-ERROR-NBR
                      CRT-ERROR-CAT
                      CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3.

       1676-NEXT-EMPFLEXDOL.
PERF       PERFORM 860-FIND-NXTRNG-EFDSET4.

       1676-END.

006960******************************************************************
       1678-SEL-FLEX-BENEFIT.
006960******************************************************************

           IF (BEN-FLEX-PLAN           NOT = EFD-FLEX-PLAN)
               GO TO 1678-NEXT-BENEFIT.

           MOVE BEN-COMPANY            TO WF2-COMPANY.
           MOVE BEN-EMPLOYEE           TO WF2-EMPLOYEE.
           MOVE BEN-PLAN-TYPE          TO WF2-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO WF2-PLAN-CODE.
           MOVE BEN-START-DATE         TO WF2-START-DATE.
           PERFORM 8400-FIND-BN100BENC.
           IF (BN100BENC-FOUND)
               GO TO 1678-NEXT-BENEFIT.

           SET PROC-BEN                TO TRUE.

           IF (ERROR-IN-EFD)
      ******** IF ERROR IN EFD, LIST ALL BENEFITS WITH ERROR
               PERFORM 1830-CREATE-ETEC-BENC
               THRU    1830-END
               GO TO 1678-NEXT-BENEFIT.

           IF (BEN-STOP-DATE           < WS-CHANGE-DATE)
               GO TO 1678-NEXT-BENEFIT.

           MOVE BEN-COMPANY            TO DB-COMPANY.
           MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
           PERFORM 840-FIND-PLNSET1.

           IF (BEN-START-DATE          = WS-CHANGE-DATE)
               PERFORM 1652-CALL-BNBEN
               THRU    1652-END
           ELSE
               SET NO-RECOMPUTE-BEN        TO TRUE
               IF (PLN-CONTRIB-TYPE = "1" OR "2" OR "3" OR "4")
                   SET RECOMPUTE-BEN       TO TRUE
               END-IF
               PERFORM 1710-STOP-READD-BENEFIT
               THRU    1710-END.

       1678-NEXT-BENEFIT.
           PERFORM 860-FIND-NXTRNG-BENSET7.

       1678-END.

006960******************************************************************
       1680-SEL-REMAINING-BEN.
006960******************************************************************

           IF  (BEN-STOP-DATE          < BNH-EFFECT-DATE)
           AND (BEN-STOP-DATE          NOT = ZEROES)
               GO TO 1680-NEXT-BENEFIT.

           MOVE BEN-COMPANY            TO DB-COMPANY.
           MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
           PERFORM 840-FIND-PLNSET1.

           IF (PLN-CONTRIB-TYPE        = "0")
               GO TO 1680-NEXT-BENEFIT.

           MOVE BEN-COMPANY            TO WF2-COMPANY.
           MOVE BEN-EMPLOYEE           TO WF2-EMPLOYEE.
           MOVE BEN-PLAN-TYPE          TO WF2-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO WF2-PLAN-CODE.
           MOVE BEN-START-DATE         TO WF2-START-DATE.
           PERFORM 8400-FIND-BN100BENC.
           IF (BN100BENC-FOUND)
               GO TO 1680-NEXT-BENEFIT.

           SET PROC-BEN                TO TRUE.

      *
      **** FIND CHANGE RULES FOR PLAN AND ACTION CODE
      *
           SET CHANGE-RULES            TO TRUE.
           PERFORM 7000-FIND-BWT-FOR-ACTION.
           IF (ERROR-FOUND)
               PERFORM 1830-CREATE-ETEC-BENC
               THRU    1830-END
               GO TO 1680-CONTINUE.

           IF  (BEN-STOP-DATE          < WS-CHANGE-DATE)
           AND (BEN-STOP-DATE          NOT = ZEROES)
               GO TO 1680-NEXT-BENEFIT.

           MOVE PLN-COMPANY            TO WS-COMPANY.
           MOVE PLN-DED-TABLE          TO WS-DED-TABLE.

           PERFORM 1682-CHECK-PAY-FREQ-CHG
           THRU    1682-END.

           IF (NO-PAY-FREQ-CHG)
               GO TO 1680-CONTINUE.

       1680-PROCESS-BENEFIT..
           INITIALIZE WS-FLEX-FLAG.
           IF (PLN-FLEX-PLAN           NOT = SPACES)
               MOVE PLN-FLEX-PLAN      TO DB-FLEX-PLAN
               PERFORM 840-FIND-FLPSET1
               IF (FLP-SPEND-ONLY      = "N")
                   MOVE "Y"            TO WS-FLEX-FLAG.

           IF (BEN-START-DATE          >= WS-CHANGE-DATE)
               PERFORM 1652-CALL-BNBEN
               THRU    1652-END
           ELSE
           IF (WS-FLEX-FLAG            = "Y")
               SET NO-RECOMPUTE-BEN        TO TRUE
               IF (PLN-CONTRIB-TYPE = "1" OR "2" OR "3" OR "4")
                   SET RECOMPUTE-BEN       TO TRUE
               END-IF
               PERFORM 1710-STOP-READD-BENEFIT
               THRU    1710-END
           ELSE
               PERFORM 1686-CALL-BNBEN
               THRU    1686-END
               IF (ERROR-FOUND)
                   PERFORM 1830-CREATE-ETEC-BENC
                   THRU    1830-END

                   IF (WS-FLEX-FLAG        = "Y")
                       MOVE BEN-PLAN-TYPE  TO WS-SV-PLAN-TYPE
                       MOVE BEN-PLAN-CODE  TO WS-SV-PLAN-CODE
                       MOVE BEN-START-DATE TO WS-SV-START-DATE
                       PERFORM 1718-PUT-OTHERS-IN-ERROR
                       THRU    1718-END
                       MOVE EMP-COMPANY    TO DB-COMPANY
                       MOVE EMP-EMPLOYEE   TO DB-EMPLOYEE
                       MOVE EFD-START-DATE TO DB-EFD-START-DATE
                       MOVE BENSET7-EFD-START-DATE TO WS-DB-BEG-RNG
                       PERFORM 850-FIND-BEGRNG-BENSET7
                       PERFORM 860-FIND-NXTRNG-BENSET7
                           UNTIL (BENEFIT-NOTFOUND)
                           OR    ((BEN-PLAN-TYPE  = WS-SV-PLAN-TYPE)
                           AND    (BEN-PLAN-CODE  = WS-SV-PLAN-CODE)
                           AND    (BEN-START-DATE = WS-SV-START-DATE))
                   END-IF
               ELSE
                   PERFORM 1856-CREATE-CS-BENC
                   THRU    1856-END

                   IF (PLN-CONTRIB-TYPE = "5")
                       PERFORM 1858-CREATE-CA-BENA
                       THRU    1858-END
                           VARYING I1 FROM 1 BY 1
                           UNTIL  (I1 > 2)
                   ELSE
                       PERFORM 1858-CREATE-CA-BENA
                       THRU    1858-END
                           VARYING I1 FROM 1 BY 1
                           UNTIL  (I1 > BNBEN-NBR-LINES).

       1680-CONTINUE.
           INITIALIZE CRT-ERROR-NBR
                      CRT-ERROR-CAT
                      CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3.

       1680-NEXT-BENEFIT.
           PERFORM 860-FIND-NXTRNG-BENSET7.

       1680-END.

006960******************************************************************
       1682-CHECK-PAY-FREQ-CHG.
006960******************************************************************

           SET NO-PAY-FREQ-CHG         TO TRUE.

           MOVE WS-COMPANY             TO DB-COMPANY.
           MOVE WS-DED-TABLE           TO DB-TABLE-CODE.
           PERFORM 840-FIND-DFTSET1.

           PERFORM 1684-MOVE-OLD-N-NEW
           THRU    1684-END.

           IF (WS-OLD-DIVI             NOT = WS-NEW-DIVI)
               SET PAY-FREQ-CHG        TO TRUE.

           PERFORM
               VARYING I1              FROM 1 BY 1
               UNTIL  (I1              > 9)
               OR     (PAY-FREQ-CHG)

               IF (WS-OLD-CYCLE (I1)   NOT = WS-NEW-CYCLE (I1))
                   SET PAY-FREQ-CHG    TO TRUE
               END-IF
           END-PERFORM.

       1682-END.

006960******************************************************************
       1684-MOVE-OLD-N-NEW.
006960******************************************************************

           IF (WS-OLD-VALUE            = "1")
               MOVE DFT-WKLY-DIVI      TO WS-OLD-DIVI
               PERFORM
                   VARYING I1          FROM 1 BY 1
                   UNTIL  (I1          > 9)

                   MOVE DFT-WKLY-CYCLE (I1)
                                       TO WS-OLD-CYCLE (I1)
               END-PERFORM
           ELSE
           IF (WS-OLD-VALUE            = "2")
               MOVE DFT-BIWK-DIVI      TO WS-OLD-DIVI
               PERFORM
                   VARYING I1          FROM 1 BY 1
                   UNTIL  (I1          > 9)

                   MOVE DFT-BIWK-CYCLE (I1)
                                       TO WS-OLD-CYCLE (I1)
               END-PERFORM
           ELSE
           IF (WS-OLD-VALUE            = "3")
               MOVE DFT-SEMI-DIVI      TO WS-OLD-DIVI
               PERFORM
                   VARYING I1          FROM 1 BY 1
                   UNTIL  (I1          > 9)

                   MOVE DFT-SEMI-CYCLE (I1)
                                       TO WS-OLD-CYCLE (I1)
               END-PERFORM
           ELSE
           IF (WS-OLD-VALUE            = "4")
               MOVE DFT-MNTH-DIVI      TO WS-OLD-DIVI
               PERFORM
                   VARYING I1          FROM 1 BY 1
                   UNTIL  (I1          > 9)

                   MOVE DFT-MNTH-CYCLE (I1)
                                       TO WS-OLD-CYCLE (I1)
               END-PERFORM
               
      *** Add Pay Frequency 5  GSN 17/12/2001 **************************
           
           ELSE         
           IF (WS-OLD-VALUE            = "5")
               MOVE DFT-FRWK-DIVI      TO WS-OLD-DIVI
               PERFORM
                   VARYING I1          FROM 1 BY 1
                   UNTIL  (I1          > 9)

                   MOVE DFT-FRWK-CYCLE (I1)
                                       TO WS-OLD-CYCLE (I1)
               END-PERFORM.         
               

           IF (WS-NEW-VALUE            = "1")
               MOVE DFT-WKLY-DIVI      TO WS-NEW-DIVI
               PERFORM
                   VARYING I1          FROM 1 BY 1
                   UNTIL  (I1          > 9)

                   MOVE DFT-WKLY-CYCLE (I1)
                                       TO WS-NEW-CYCLE (I1)
               END-PERFORM
           ELSE
           IF (WS-NEW-VALUE            = "2")
               MOVE DFT-BIWK-DIVI      TO WS-NEW-DIVI
               PERFORM
                   VARYING I1          FROM 1 BY 1
                   UNTIL  (I1          > 9)

                   MOVE DFT-BIWK-CYCLE (I1)
                                       TO WS-NEW-CYCLE (I1)
               END-PERFORM
           ELSE
           IF (WS-NEW-VALUE            = "3")
               MOVE DFT-SEMI-DIVI      TO WS-NEW-DIVI
               PERFORM
                   VARYING I1          FROM 1 BY 1
                   UNTIL  (I1          > 9)

                   MOVE DFT-SEMI-CYCLE (I1)
                                       TO WS-NEW-CYCLE (I1)
               END-PERFORM
           ELSE
           IF (WS-NEW-VALUE            = "4")
               MOVE DFT-MNTH-DIVI      TO WS-NEW-DIVI
               PERFORM
                   VARYING I1          FROM 1 BY 1
                   UNTIL  (I1          > 9)

                   MOVE DFT-MNTH-CYCLE (I1)
                                       TO WS-NEW-CYCLE (I1)
               END-PERFORM
               
      *** Add Pay Frequency 5  GSN 17/12/2001 **************************
           
           ELSE         
           IF (WS-OLD-VALUE            = "5")
               MOVE DFT-FRWK-DIVI      TO WS-OLD-DIVI
               PERFORM
                   VARYING I1          FROM 1 BY 1
                   UNTIL  (I1          > 9)

                   MOVE DFT-FRWK-CYCLE (I1)
                                       TO WS-OLD-CYCLE (I1)
               END-PERFORM.         

       1684-END.

006960******************************************************************
       1686-CALL-BNBEN.
006960******************************************************************

           IF  (PLN-CONTRIB-TYPE       = "5")
           AND (WS-FLEX-FLAG           = "Y")
               MOVE 158                TO CRT-ERROR-NBR
               GO TO 1686-END.

           MOVE BEN-COMPANY            TO WS-SAVE-COMPANY.
           MOVE BEN-PLAN-TYPE          TO WS-SAVE-PLAN-TYPE.
           MOVE BEN-EMPLOYEE           TO WS-SAVE-EMPLOYEE.
           MOVE BEN-PLAN-CODE          TO WS-SAVE-PLAN-CODE.
           MOVE BEN-START-DATE         TO WS-SAVE-START-DATE.

           PERFORM 1688-MOVE-TO-BNBEN
           THRU    1688-END.
           IF (ERROR-FOUND)
               GO TO 1686-END.

           PERFORM 2000-BNBEN-EDIT-TRAN.

           MOVE WS-SAVE-COMPANY        TO DB-COMPANY.
           MOVE WS-SAVE-PLAN-TYPE      TO DB-PLAN-TYPE.
           MOVE WS-SAVE-EMPLOYEE       TO DB-EMPLOYEE.
           MOVE WS-SAVE-PLAN-CODE      TO DB-PLAN-CODE.
           MOVE WS-SAVE-START-DATE     TO DB-START-DATE.
           PERFORM 840-FIND-BENSET1.

       1686-END.

006960******************************************************************
       1688-MOVE-TO-BNBEN.
006960******************************************************************

002000     INITIALIZE BNBEN-DETAIL-GROUP
117816                BNBEN-PROC-FLEX-SW
002000                BNBEN-USE-NAVIGATE-SW.

           INITIALIZE WS-FLEX-FLAG.
           IF (PLN-FLEX-PLAN           NOT = SPACES)
               MOVE PLN-FLEX-PLAN      TO DB-FLEX-PLAN
               PERFORM 840-FIND-FLPSET1
               IF (FLP-SPEND-ONLY      = "N")
                   MOVE "Y"            TO WS-FLEX-FLAG.

           MOVE BEN-COMPANY            TO BNBEN-COMPANY.
           MOVE BEN-EMPLOYEE           TO BNBEN-EMPLOYEE (1).
           MOVE BEN-PLAN-TYPE          TO BNBEN-PLAN-TYPE (1).
           MOVE BEN-PLAN-CODE          TO BNBEN-PLAN-CODE (1).

           MOVE BEN-START-DATE         TO BNBEN-START-DATE (1)
                                          BNBEN-PT-START-DATE (1).

           MOVE WS-CHANGE-DATE         TO WSDR-FR-DATE.
           PERFORM 900-DATE-TO-JULIAN.
           SUBTRACT 1                  FROM WSDR-JULIAN-DAYS.
           PERFORM 900-JULIAN-TO-DATE.
           MOVE WSDR-FR-DATE           TO BNBEN-STOP-DATE (1).

           MOVE BEN-COMPANY            TO DB-COMPANY.
           MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
           PERFORM 840-FIND-PLNSET1.

           MOVE BEN-PREM-UPD-DT        TO DB-START-DATE.
           MOVE BEN-PREM-GROUP         TO DB-GROUP-NAME.
           PERFORM 840-FIND-PRESET1.

           IF (PLN-COVERAGE-TYPE       = "2")
               MOVE BEN-COMPANY        TO DB-COMPANY
               MOVE BEN-PLAN-TYPE      TO DB-PLAN-TYPE
               MOVE BEN-PLAN-CODE      TO DB-PLAN-CODE
               MOVE "E"                TO DB-COVER-TYPE
               MOVE BEN-COV-UPD-DT     TO DB-START-DATE
               MOVE BEN-COV-GROUP      TO DB-GROUP-NAME
               PERFORM 840-FIND-CVRSET1.

184000     MOVE BEN-COV-OPTION         TO BNBEN-COVER-OPT (1).

           IF (PLN-CONTRIB-TYPE        = "5" OR "6" OR "7")
               MOVE BEN-CYCLES-REMAIN  TO BNBEN-COVER-OPT (1).

           MOVE BEN-COVER-AMT          TO BNBEN-COVER-AMT (1).

           IF (PLN-CONTRIB-TYPE        = "5" OR "6" OR "7")
               MOVE BEN-BOND-DED-AMT   TO BNBEN-COVER-AMT (1).

           IF (PLN-CONTRIB-TYPE        = "5" OR "6" OR "7")
               MOVE BEN-ANNUAL-AMT     TO BNBEN-PAY-RATE (1)
           ELSE
               MOVE BEN-PAY-RATE       TO BNBEN-PAY-RATE (1).

           COMPUTE BNBEN-EMP-PRE-CONT (1) = BEN-EMP-PRE-CONT
                                          + BEN-CMP-FLX-CONT.
           MOVE BEN-EMP-AFT-CONT       TO BNBEN-EMP-AFT-CONT (1).
           MOVE BEN-CMP-FLX-CONT       TO BNBEN-CMP-FLX-CONT (1).
           MOVE BEN-COMP-CONT          TO BNBEN-COMP-CONT    (1).

           MOVE BEN-PCT-AMT-FLAG       TO BNBEN-PCT-AMT-FLAG (1).
           MOVE BEN-SMOKER             TO BNBEN-SMOKER-FLAG (1).

           IF (PLN-PLAN-TYPE           = "VA")
               MOVE BEN-NBR-HOURS      TO BNBEN-MULT-SALARY (1)
P60050     ELSE
P60050     IF (CVR-CALC-TYPE           = "N")
P60050         INITIALIZE                 BNBEN-MULT-SALARY (1)
           ELSE
P60050         MOVE BEN-MULTIPLE       TO BNBEN-MULT-SALARY (1)
P60050     END-IF
P60050     END-IF.

           MOVE BEN-PRE-AFT-FLAG       TO BNBEN-PRE-AFT-FLAG (1).

           MOVE PRM-CREATE-TRANS       TO BNBEN-CREATE-TRANS (1).

P51666     MOVE BEN-DED-START-DATE     TO BNBEN-DED-START-DATE (1).
P51666     MOVE BEN-DED-STOP-DATE      TO BNBEN-DED-STOP-DATE (1).
P51666
           MOVE "S"                    TO BNBEN-LINE-FC (1).

           PERFORM 1689-MOVE-FOR-ADD
           THRU    1689-END.

           MOVE "C"                    TO BNBEN-FC.
           IF (BNBEN-PAY-FRQ-ADD-ERR (2) = "Y")
               MOVE 1                  TO BNBEN-NBR-LINES
           ELSE
               MOVE 2                  TO BNBEN-NBR-LINES.

117816     MOVE WS-PROC-FLEX-SW        TO BNBEN-PROC-FLEX-SW.

       1688-END.

006960******************************************************************
       1689-MOVE-FOR-ADD.
006960******************************************************************

           MOVE BNBEN-EMPLOYEE (1)     TO BNBEN-EMPLOYEE (2).
           MOVE BNBEN-PLAN-TYPE (1)    TO BNBEN-PLAN-TYPE (2).
           MOVE BNBEN-PLAN-CODE (1)    TO BNBEN-PLAN-CODE (2).

           MOVE WS-CHANGE-DATE         TO BNBEN-START-DATE (2)
                                          BNBEN-PT-START-DATE (2).

           MOVE BEN-STOP-DATE          TO BNBEN-STOP-DATE (2).

184000     MOVE BNBEN-COVER-OPT (1)    TO BNBEN-COVER-OPT (2).
           MOVE BNBEN-COVER-AMT (1)    TO BNBEN-COVER-AMT (2).
           MOVE BNBEN-PAY-RATE (1)     TO BNBEN-PAY-RATE (2).

P54229     MOVE BNBEN-EMP-PRE-CONT (1) TO BNBEN-EMP-PRE-CONT (2).
           MOVE BNBEN-EMP-AFT-CONT (1) TO BNBEN-EMP-AFT-CONT (2).
           MOVE BNBEN-CMP-FLX-CONT (1) TO BNBEN-CMP-FLX-CONT (2).
           MOVE BNBEN-COMP-CONT (1)    TO BNBEN-COMP-CONT (2).

           MOVE BNBEN-PCT-AMT-FLAG (1) TO BNBEN-PCT-AMT-FLAG (2).
           MOVE BNBEN-SMOKER-FLAG (1)  TO BNBEN-SMOKER-FLAG (2).
           MOVE BNBEN-MULT-SALARY (1)  TO BNBEN-MULT-SALARY (2).
           MOVE BNBEN-PRE-AFT-FLAG (1) TO BNBEN-PRE-AFT-FLAG (2).
           IF  (PAY-FREQ-CHANGES)
           AND (PLN-CONTRIB-TYPE = "5")
               MOVE "Y"                TO BNBEN-PAY-FRQ-ADD-ERR (2).

           MOVE "A"                    TO BNBEN-LINE-FC (2).

           MOVE "Y"                    TO BNBEN-SKIP-ELIGIBILITY (2).

           MOVE PRM-CREATE-TRANS       TO BNBEN-CREATE-TRANS (2).

           IF  (PLN-CONTRIB-TYPE            NOT = "0")
P63031     AND (PLN-CONTRIB-TYPE            NOT = "X ")
019000         MOVE BNBEN-COMPANY          TO BNREWS-COMPANY
019100         MOVE BNBEN-PLAN-TYPE (2)    TO BNREWS-PLAN-TYPE
019200         MOVE BNBEN-PLAN-CODE (2)    TO BNREWS-PLAN-CODE
019300         MOVE "E"                    TO BNREWS-COVER-TYPE
019400         MOVE BNBEN-EMPLOYEE (2)     TO BNREWS-EMPLOYEE
019500         MOVE BNBEN-START-DATE (2)   TO BNREWS-AS-OF-DATE
019600         MOVE "PRE"                  TO BNREWS-FILE-PREFIX
019700         PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70
019800         IF (ERROR-FOUND)
019900             GO TO 1689-END.
020000
           IF (PRE-CONT-TAX-STS        = "N")
               INITIALIZE BNBEN-PCT-AMT-FLAG (2)
                          BNBEN-PRE-AFT-FLAG (2).

       1689-END.

006960******************************************************************
       1690-BIRTHDATE-CHANGES.
006960******************************************************************

J43365     IF (SELECT-COMPANY)
J43365          MOVE EMP-COMPANY            TO CRT-COMPANY
J43365          MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL
J43365          PERFORM 700-HR-EMP-SECURITY
J43365          IF (HRWS-EMP-SECURED)
J43365              GO TO 1690-NEXT-BNCHANGE
J43365          END-IF
J43365     END-IF.
J43365
           IF (BNH-EFFECT-DATE         > PRM-THRU-DATE)
               GO TO 1690-NEXT-BNCHANGE.

           SET PROC-OFF                TO TRUE.

           MOVE 114                    TO CRT-ERROR-NBR.
P77393     MOVE "BN100"                TO CRT-ERROR-CAT.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           INITIALIZE CRT-ERROR-NBR.

       1690-NEXT-BNCHANGE.
           PERFORM 860-FIND-NEXT-BNHSET2.

           PERFORM 1900-SET-PROC-BNG-BNH-SW
           THRU    1900-END.

       1690-END.

006960******************************************************************
       1695-SERVICE-DATE-CHANGES.
006960******************************************************************

J43365     IF (SELECT-COMPANY)
J43365          MOVE EMP-COMPANY            TO CRT-COMPANY
J43365          MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL
J43365          PERFORM 700-HR-EMP-SECURITY
J43365          IF (HRWS-EMP-SECURED)
J43365              GO TO 1695-NEXT-BNCHANGE
J43365          END-IF
J43365     END-IF.
J43365
           IF (BNH-EFFECT-DATE         > PRM-THRU-DATE)
               GO TO 1695-NEXT-BNCHANGE.

           SET PROC-OFF                TO TRUE.

           MOVE 115                    TO CRT-ERROR-NBR.
P77393     MOVE "BN100"                TO CRT-ERROR-CAT.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           INITIALIZE CRT-ERROR-NBR.

       1695-NEXT-BNCHANGE.
           PERFORM 860-FIND-NEXT-BNHSET2.

           PERFORM 1900-SET-PROC-BNG-BNH-SW
           THRU    1900-END.

       1695-END.

006960******************************************************************
       1700-SMOKER-FLAG-CHANGES.
006960******************************************************************

J43365     IF (SELECT-COMPANY)
J43365          MOVE EMP-COMPANY            TO CRT-COMPANY
J43365          MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL
J43365          PERFORM 700-HR-EMP-SECURITY
J43365          IF (HRWS-EMP-SECURED)
J43365              GO TO 1700-NEXT-BNCHANGE
J43365          END-IF
J43365     END-IF.
J43365
           IF (BNH-EFFECT-DATE         > PRM-THRU-DATE)
               GO TO 1700-NEXT-BNCHANGE.

           SET PROC-OFF                TO TRUE.

           MOVE 130                    TO CRT-ERROR-NBR.
P77393     MOVE "BN100"                TO CRT-ERROR-CAT.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           INITIALIZE CRT-ERROR-NBR.

       1700-NEXT-BNCHANGE.
           PERFORM 860-FIND-NEXT-BNHSET2.

           PERFORM 1900-SET-PROC-BNG-BNH-SW
           THRU    1900-END.

       1700-END.

006960******************************************************************
       1705-STOP-BENEFIT.
006960******************************************************************

      *
      **** ERROR 163 FROM BNENT IS BEN-STOP-DATE IS > EFR-STOP-DATE
      **** FOR THIS WE HAVE TO STOP A BENEFIT ON EFR-STOP-DATE AND ADD
      **** A NEW ONE IN BETWEEN
      *
002000     INITIALIZE BNBEN-DETAIL-GROUP
002000                BNBEN-USE-NAVIGATE-SW.

           MOVE 1                      TO WS-I3.

           PERFORM 6200-CALL-BNENTPD.
           IF (ERROR-FOUND)
               IF (CRT-ERROR-NBR       NOT = 163)
                   PERFORM 1830-CREATE-ETEC-BENC
                   THRU    1830-END

                   IF (WS-FLEX-FLAG    = "Y")
                       MOVE BEN-PLAN-TYPE    TO WS-SV-PLAN-TYPE
                       MOVE BEN-PLAN-CODE    TO WS-SV-PLAN-CODE
                       MOVE BEN-START-DATE   TO WS-SV-START-DATE
                       PERFORM 1718-PUT-OTHERS-IN-ERROR
                       THRU    1718-END
                       MOVE EMP-COMPANY      TO DB-COMPANY
                       MOVE EMP-EMPLOYEE     TO DB-EMPLOYEE
                       MOVE EFD-START-DATE   TO DB-EFD-START-DATE
                       MOVE BENSET7-EFD-START-DATE TO WS-DB-BEG-RNG
                       PERFORM 850-FIND-BEGRNG-BENSET7
                       PERFORM 860-FIND-NXTRNG-BENSET7
                           UNTIL (BENEFIT-NOTFOUND)
                           OR    ((BEN-PLAN-TYPE  = WS-SV-PLAN-TYPE)
                           AND    (BEN-PLAN-CODE  = WS-SV-PLAN-CODE)
                           AND    (BEN-START-DATE = WS-SV-START-DATE))
                   END-IF

               ELSE
                   PERFORM 1712-CREATE-S-BENC
                   THRU    1712-END

                   SET NO-QUIT-LOOP    TO TRUE
                   PERFORM
                       UNTIL (QUIT-LOOP)

                       PERFORM 1714-CHECK-163
                       THRU    1714-END

                   END-PERFORM
               END-IF
           ELSE
               PERFORM 1852-CREATE-S-BENC
               THRU    1852-END.

       1705-END.

006960******************************************************************
       1710-STOP-READD-BENEFIT.
006960******************************************************************

      *
      **** ERROR 163 FROM BNENT IS BEN-STOP-DATE IS > EFR-STOP-DATE
      **** FOR THIS WE HAVE TO STOP A BENEFIT ON EFR-STOP-DATE AND ADD
      **** A NEW ONE IN BETWEEN THESE TWO STOP DATES
      *
002000     INITIALIZE BNBEN-DETAIL-GROUP
002000                BNBEN-USE-NAVIGATE-SW.

           MOVE 1                      TO WS-I3.

J64465     MOVE "Y"                    TO BNEDWS-FROM-MAGIC-SW.

           PERFORM 6200-CALL-BNENTPD.
           IF (ERROR-FOUND)
               IF (CRT-ERROR-NBR       NOT = 163)
                   PERFORM 1830-CREATE-ETEC-BENC
                   THRU    1830-END

                   IF (WS-FLEX-FLAG    = "Y")
                       MOVE BEN-PLAN-TYPE    TO WS-SV-PLAN-TYPE
                       MOVE BEN-PLAN-CODE    TO WS-SV-PLAN-CODE
                       MOVE BEN-START-DATE   TO WS-SV-START-DATE
                       PERFORM 1718-PUT-OTHERS-IN-ERROR
                       THRU    1718-END
                       MOVE EMP-COMPANY      TO DB-COMPANY
                       MOVE EMP-EMPLOYEE     TO DB-EMPLOYEE
                       MOVE EFD-START-DATE   TO DB-EFD-START-DATE
                       MOVE BENSET7-EFD-START-DATE TO WS-DB-BEG-RNG
                       PERFORM 850-FIND-BEGRNG-BENSET7
                       PERFORM 860-FIND-NXTRNG-BENSET7
                           UNTIL (BENEFIT-NOTFOUND)
                           OR    ((BEN-PLAN-TYPE  = WS-SV-PLAN-TYPE)
                           AND    (BEN-PLAN-CODE  = WS-SV-PLAN-CODE)
                           AND    (BEN-START-DATE = WS-SV-START-DATE))
                   END-IF

                   GO TO 1710-END
               ELSE
                   PERFORM 1712-CREATE-S-BENC
                   THRU    1712-END

                   SET NO-QUIT-LOOP    TO TRUE
                   PERFORM
                       UNTIL (QUIT-LOOP)

                       PERFORM 1714-CHECK-163
                       THRU    1714-END

                   END-PERFORM
                   IF (ERROR-FOUND)
                       GO TO 1710-END
                   END-IF
               END-IF
           ELSE
           IF (NOT-SKIP-CHANGE-STOP)
               PERFORM 1856-CREATE-CS-BENC
               THRU    1856-END.

           MOVE BNBEN-COMPANY          TO DB-COMPANY.
           MOVE BNBEN-PLAN-TYPE (1)    TO DB-PLAN-TYPE.
           MOVE BNBEN-EMPLOYEE (1)     TO DB-EMPLOYEE.
           MOVE BNBEN-PLAN-CODE (1)    TO DB-PLAN-CODE.
           MOVE BNBEN-START-DATE (1)   TO DB-START-DATE.
           PERFORM 840-FIND-BENSET1.

           IF (ERROR-FOR-CA)
               PERFORM 1835-CREATE-EA-BENA
               THRU    1835-END
           ELSE
               SET MOVE-2ND-BNBEN-LINE TO TRUE

               MOVE WS-ADD-DATE        TO WS-SAVE-ADD-DATE

               PERFORM 1858-CREATE-CA-BENA
               THRU    1858-END
                   VARYING I1 FROM WS-I3 BY 1
                   UNTIL  (I1 > BNBEN-NBR-LINES)

               MOVE WS-SAVE-ADD-DATE   TO WS-ADD-DATE

               SET MOVE-1ST-BNBEN-LINE TO TRUE.

       1710-END.

006960******************************************************************
       1712-CREATE-S-BENC.
006960******************************************************************

           PERFORM 8000-CREATE-BN100BENC.

           MOVE BEN-COMPANY            TO WF2-COMPANY.
           MOVE BEN-EMPLOYEE           TO WF2-EMPLOYEE.
           MOVE BEN-PLAN-TYPE          TO WF2-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO WF2-PLAN-CODE.
           MOVE BEN-START-DATE         TO WF2-START-DATE.

           MOVE "S"                    TO WF2-FUNCTION-CODE.

      *
      **** BNWS-DATE-FORMAT1 IS SET IN BNENTPD
      *
           MOVE BNWS-DATE-FORMAT1      TO WF2-STOP-DATE.

           MOVE BEN-FLEX-PLAN          TO WF2-FLEX-PLAN.

           MOVE WS-EFFECT-DATE         TO WF2-WF-EFFECT-DATE.

           COMPUTE WF2-WF-SEQ-NBR      = WS-SEQ-NBR
                                       + 1.

           MOVE BNBEN-HIPAA-REASON     TO WF2-HIPAA-REASON.

           PERFORM 8200-STORE-BN100BENC.

           INITIALIZE CRT-ERROR-NBR
                      CRT-ERROR-CAT
                      CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3
                      CRT-ERR-VAR4
                      CRT-ERR-VAR5.

P67618     IF (EMPLOYEE-NOTFOUND)
P67618         MOVE BEN-COMPANY        TO DB-COMPANY
P67618         MOVE BEN-EMPLOYEE       TO DB-EMPLOYEE
P67618         PERFORM 840-FIND-EMPSET1.
P67618
           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

       1712-END.

006960******************************************************************
       1714-CHECK-163.
006960******************************************************************

           MOVE BNWS-DATE-FORMAT1          TO BNBEN-STOP-DATE (WS-I3).

           ADD 1                           TO WS-I3.

           PERFORM 1716-MOVE-TO-BNBEN
           THRU    1716-END.

           MOVE "C"                        TO BNBEN-FC.
           MOVE "A"                        TO BNBEN-LINE-FC (WS-I3).

           MOVE BNWS-DATE-FORMAT1          TO WSDR-FR-DATE.
           PERFORM 900-DATE-TO-JULIAN.
           ADD 1                           TO WSDR-JULIAN-DAYS.
           PERFORM 900-JULIAN-TO-DATE.
           MOVE WSDR-FR-DATE               TO BNBEN-START-DATE (WS-I3)
                                           BNBEN-PT-START-DATE (WS-I3).

           IF (TERM-RULES)
               MOVE WS-TERM-DATE           TO BNBEN-STOP-DATE (WS-I3)
           ELSE
           IF (CHANGE-RULES)
               MOVE WS-CHANGE-DATE         TO WSDR-FR-DATE
               PERFORM 900-DATE-TO-JULIAN
               SUBTRACT 1                  FROM WSDR-JULIAN-DAYS
               PERFORM 900-JULIAN-TO-DATE
               MOVE WSDR-FR-DATE           TO BNBEN-STOP-DATE (WS-I3).

           MOVE WS-I3                      TO BNBEN-NBR-LINES.

           MOVE "Y"                        TO BNBEN-SKIP-GROUP-N-ZIP.

           PERFORM 2000-BNBEN-EDIT-TRAN.

           IF (NO-ERROR-FOUND)
               PERFORM 1860-CREATE-SA-BENA
               THRU    1860-END

               SET QUIT-LOOP               TO TRUE
           ELSE
           IF (ERROR-FOUND)
               IF (CRT-ERROR-NBR           NOT = 163)
                   PERFORM 1864-CREATE-EA-BENA
                   THRU    1864-END

                   SET QUIT-LOOP           TO TRUE
               ELSE
                   MOVE BNWS-DATE-FORMAT1  TO BNBEN-STOP-DATE (WS-I3)

                   INITIALIZE CRT-ERROR-NBR
                              CRT-ERROR-CAT
                              CRT-ERR-VAR1
                              CRT-ERR-VAR2
                              CRT-ERR-VAR3
                              CRT-ERR-VAR4
                              CRT-ERR-VAR5

                   PERFORM 1860-CREATE-SA-BENA
                   THRU    1860-END.

       1714-END.

006960******************************************************************
       1716-MOVE-TO-BNBEN.
006960******************************************************************

183600     MOVE BNBEN-EMPLOYEE (WS-I3 - 1)  TO BNBEN-EMPLOYEE (WS-I3).
183800     MOVE BNBEN-PLAN-TYPE (WS-I3 - 1) TO BNBEN-PLAN-TYPE (WS-I3).
183800     MOVE BNBEN-PLAN-CODE (WS-I3 - 1) TO BNBEN-PLAN-CODE (WS-I3).


184000     MOVE BNBEN-COVER-OPT (WS-I3 - 1) TO BNBEN-COVER-OPT (WS-I3).

           MOVE BNBEN-COVER-AMT (WS-I3 - 1) TO BNBEN-COVER-AMT (WS-I3).

           MOVE BNBEN-MULT-SALARY (WS-I3 - 1)
                                            TO
                                            BNBEN-MULT-SALARY (WS-I3).

           MOVE BNBEN-PAY-RATE (WS-I3 - 1)  TO BNBEN-PAY-RATE (WS-I3).

           IF (BNBEN-ZERO-PREMIUM-SW (WS-I3 - 1) = 1)
           OR (BNBEN-POS-PREMIUM-SW  (WS-I3 - 1) = 1)
184300         MOVE BNBEN-EMP-PRE-CONT (WS-I3 - 1)
                                            TO
                                            BNBEN-EMP-PRE-CONT (WS-I3).
184300     MOVE BNBEN-EMP-AFT-CONT (WS-I3 - 1)
                                            TO
                                            BNBEN-EMP-AFT-CONT (WS-I3).
           MOVE BNBEN-CMP-FLX-CONT (WS-I3 - 1)
                                            TO
                                            BNBEN-CMP-FLX-CONT (WS-I3).
           MOVE BNBEN-COMP-CONT (WS-I3 - 1) TO BNBEN-COMP-CONT (WS-I3).

           MOVE BNBEN-PCT-AMT-FLAG (WS-I3 - 1)
                                            TO
                                            BNBEN-PCT-AMT-FLAG (WS-I3).
           MOVE BNBEN-PRE-AFT-FLAG (WS-I3 - 1)
                                            TO
                                            BNBEN-PRE-AFT-FLAG (WS-I3).
184300     MOVE BNBEN-SMOKER-FLAG (WS-I3 - 1)
                                            TO
                                            BNBEN-SMOKER-FLAG (WS-I3).

           MOVE "Y"                         TO
                                         BNBEN-SKIP-ELIGIBILITY (WS-I3).

       1716-END.

006960******************************************************************
       1718-PUT-OTHERS-IN-ERROR.
006960******************************************************************

      **** THIS ROUTINE WILL PUT EFDS AND BENS IN ERROR BECAUSE OF ****
      **** ERROR IN ONE OF THE FLEX BENEFIT CANNOT PROCESS EFD AND ****
      **** OTHER FLEX BENEFITS IN PLAN                             ****

           MOVE BEN-PLAN-TYPE          TO WS-SAVE-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO WS-SAVE-PLAN-CODE.
           MOVE BEN-START-DATE         TO WS-SAVE-START-DATE.

           SET ERROR-IN-EFD            TO TRUE.

           MOVE 165                    TO CRT-ERROR-NBR.
P15764     MOVE "BN100"                TO CRT-ERROR-CAT.
           INITIALIZE CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3
                      CRT-ERR-VAR4
                      CRT-ERR-VAR3.

           MOVE BEN-COMPANY            TO WF4-COMPANY.
           MOVE BEN-EMPLOYEE           TO WF4-EMPLOYEE.
           MOVE BEN-EFD-START-DATE     TO WF4-START-DATE.
           PERFORM 8400-FIND-BN100EFDC.
           IF (BN100EFDC-FOUND)
      ******** DELETE EFDA AND ASSOCIATED WORK RECORD
               MOVE WF4-STOP-DATE      TO WSDR-FR-DATE
               PERFORM 900-DATE-TO-JULIAN
               ADD 1                   TO WSDR-JULIAN-DAYS
               PERFORM 900-JULIAN-TO-DATE
               MOVE WSDR-FR-DATE       TO WF3-START-DATE
               PERFORM 8400-FIND-BN100EFDA
               IF (BN100EFDA-FOUND)
                   MOVE WF4-COMPANY        TO WF-COMPANY
                   MOVE WF4-EMPLOYEE       TO WF-EMPLOYEE
                   MOVE "3"                TO WF-REC-TYPE
                   MOVE WF3-WF-EFFECT-DATE TO WF-EFFECT-DATE
                   MOVE WF3-WF-SEQ-NBR     TO WF-SEQ-NBR
                   PERFORM 8300-DELETE-BN100WORK
                   PERFORM 8300-DELETE-BN100EFDA
               END-IF

               IF (TERM-RULES)
                   MOVE "ET"           TO WF4-FUNCTION-CODE
               ELSE
               IF (CHANGE-RULES)
                   MOVE "EC"           TO WF4-FUNCTION-CODE
               END-IF
               END-IF

               MOVE WF4-COMPANY        TO WF-COMPANY
               MOVE WF4-EMPLOYEE       TO WF-EMPLOYEE
               MOVE "3"                TO WF-REC-TYPE
               MOVE WF4-WF-EFFECT-DATE TO WF-EFFECT-DATE
               MOVE WF4-WF-SEQ-NBR     TO WF-SEQ-NBR
               PERFORM 8400-FIND-BN100WORK

               MOVE WF4-FUNCTION-CODE  TO WF-FUNCTION-CODE

               MOVE CRT-ERROR-NBR      TO WF-ERROR-NBR
J91207         MOVE CRT-ERROR-CAT      TO WF-ERROR-CAT
P09149         MOVE WS-TRUE            TO WS-CO-ERR-SW

               PERFORM 8200-REWRITE-BN100EFDC
               PERFORM 8200-REWRITE-BN100WORK.

           MOVE BEN-COMPANY            TO DB-COMPANY.
           MOVE BEN-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE BEN-EFD-START-DATE     TO DB-EFD-START-DATE.
           INITIALIZE DB-PLAN-TYPE
                      DB-PLAN-CODE
                      DB-START-DATE.
           PERFORM 850-KFIND-NLT-BENSET7.
           PERFORM
               UNTIL (BENEFIT-KNOTFOUND)
               OR    (BEN-COMPANY          NOT = DB-COMPANY)
               OR    (BEN-EMPLOYEE         NOT = DB-EMPLOYEE)
               OR    (BEN-EFD-START-DATE   NOT = DB-EFD-START-DATE)

               IF (BEN-PLAN-TYPE       NOT = WS-SAVE-PLAN-TYPE)
               OR (BEN-PLAN-CODE       NOT = WS-SAVE-PLAN-CODE)
               OR (BEN-START-DATE      NOT = WS-SAVE-START-DATE)
                   MOVE BEN-COMPANY    TO WF2-COMPANY
                   MOVE BEN-EMPLOYEE   TO WF2-EMPLOYEE
                   MOVE BEN-PLAN-TYPE  TO WF2-PLAN-TYPE
                   MOVE BEN-PLAN-CODE  TO WF2-PLAN-CODE
                   MOVE BEN-START-DATE TO WF2-START-DATE
                   PERFORM 8400-FIND-BN100BENC
                   IF (BN100BENC-FOUND)
                       PERFORM 1720-CLEANUP-BENEFITS
                       THRU    1720-END
                   END-IF
               END-IF

               PERFORM 860-KFIND-NEXT-BENSET7
           END-PERFORM.

       1718-END.

006960******************************************************************
       1720-CLEANUP-BENEFITS.
006960******************************************************************

      **** DELETE BENA AND ASSOCIATED WORK RECORD
           MOVE WF2-COMPANY            TO WF1-COMPANY.
           MOVE WF2-EMPLOYEE           TO WF1-EMPLOYEE.
           MOVE WF2-PLAN-TYPE          TO WF1-PLAN-TYPE.
           MOVE WF2-PLAN-CODE          TO WF1-PLAN-CODE.
           MOVE WF2-STOP-DATE          TO WSDR-FR-DATE.
           PERFORM 900-DATE-TO-JULIAN.
           ADD 1                       TO WSDR-JULIAN-DAYS.
           PERFORM 900-JULIAN-TO-DATE.
           MOVE WSDR-FR-DATE           TO WF1-START-DATE.
           PERFORM 8400-FIND-BN100BENA.
           IF (BN100BENA-FOUND)
               MOVE WF1-COMPANY        TO WF-COMPANY
               MOVE WF1-EMPLOYEE       TO WF-EMPLOYEE
               MOVE "3"                TO WF-REC-TYPE
               MOVE WF1-WF-EFFECT-DATE TO WF-EFFECT-DATE
               MOVE WF1-WF-SEQ-NBR     TO WF-SEQ-NBR
               PERFORM 8300-DELETE-BN100WORK
               PERFORM 8300-DELETE-BN100BENA.

           IF (TERM-RULES)
               MOVE "ET"               TO WF2-FUNCTION-CODE
           ELSE
           IF (CHANGE-RULES)
               MOVE "EC"               TO WF2-FUNCTION-CODE.

           MOVE WF1-COMPANY            TO WF-COMPANY.
           MOVE WF1-EMPLOYEE           TO WF-EMPLOYEE.
           MOVE "3"                    TO WF-REC-TYPE.
           MOVE WF2-WF-EFFECT-DATE     TO WF-EFFECT-DATE.
           MOVE WF2-WF-SEQ-NBR         TO WF-SEQ-NBR.
           PERFORM 8400-FIND-BN100WORK.

           MOVE WF2-FUNCTION-CODE      TO WF-FUNCTION-CODE.

           IF (BN100EFDC-FOUND)
           OR (WF-ERROR-NBR            = ZEROES)
P09149         MOVE WS-TRUE            TO WS-CO-ERR-SW
               MOVE CRT-ERROR-NBR      TO WF-ERROR-NBR
               MOVE CRT-ERROR-CAT      TO WF-ERROR-CAT
               MOVE CRT-ERR-VAR1       TO WF-ERR-VAR1.

           PERFORM 8200-REWRITE-BN100BENC.
           PERFORM 8200-REWRITE-BN100WORK.

       1720-END.

006960******************************************************************
       1800-CREATE-BN100WORK.
006960******************************************************************

           PERFORM 8000-CREATE-BN100WORK.

           PERFORM 1810-MOVE-TO-BN100WORK
           THRU    1810-END.

           PERFORM 8200-STORE-BN100WORK.

       1800-END.

006960******************************************************************
       1810-MOVE-TO-BN100WORK.
006960******************************************************************

P67618     IF (EMPLOYEE-NOTFOUND)
P67618         MOVE BNH-COMPANY        TO DB-COMPANY
P67618         MOVE BNH-EMPLOYEE       TO DB-EMPLOYEE
P67618         PERFORM 840-FIND-EMPSET1.
P67618
           MOVE EMP-COMPANY            TO WF-COMPANY.
           MOVE EMP-EMPLOYEE           TO WF-EMPLOYEE.

           ADD 1                       TO WS-SEQ-NBR.

           IF (PRM-REPORT-SEQ          = "2")
               MOVE EMP-PROCESS-LEVEL  TO WF-PROC-LEVEL
               MOVE EMP-DEPARTMENT     TO WF-DEPARTMENT.

           MOVE EMP-LAST-NAME          TO WF-LAST-NAME.
           MOVE EMP-FIRST-NAME         TO WF-FIRST-NAME.
           MOVE EMP-MIDDLE-INIT        TO WF-MIDDLE-INIT.

           IF (PROC-BNG)
               MOVE BNG-EFFECT-DATE    TO WF-EFFECT-DATE

               MOVE WS-SEQ-NBR         TO WF-SEQ-NBR

               MOVE "1"                TO WF-REC-TYPE

               MOVE BNG-SEQ-NBR        TO WF-BNG-BNH-SEQ-NBR
               MOVE BNG-ACT-OBJ-ID     TO WF-ACT-OBJ-ID

               MOVE "BNG"              TO WF-FILE-PREFIX

               INITIALIZE WF-PLAN-TYPE
                          WF-PLAN-CODE
                          WF-START-DATE
               IF (ELIG-ERROR)
                   MOVE 166            TO WF-WARNING-NBR
               ELSE
                   IF (ERROR-FOUND)
P09149                 MOVE WS-TRUE        TO WS-CO-ERR-SW
                       MOVE CRT-ERROR-NBR  TO WF-ERROR-NBR
                       MOVE CRT-ERROR-CAT  TO WF-ERROR-CAT
                       MOVE CRT-ERR-VAR1   TO WF-ERR-VAR1
                   END-IF
               END-IF
           ELSE
               MOVE BNH-EFFECT-DATE    TO WF-EFFECT-DATE

               MOVE WS-SEQ-NBR         TO WF-SEQ-NBR

               MOVE "2"                TO WF-REC-TYPE

               MOVE BNH-SEQ-NBR        TO WF-BNG-BNH-SEQ-NBR
               MOVE BNH-ACT-OBJ-ID     TO WF-ACT-OBJ-ID

               MOVE "BNH"              TO WF-FILE-PREFIX

               INITIALIZE WF-PLAN-TYPE
                          WF-PLAN-CODE
                          WF-START-DATE

               IF  ((BNH-CHANGE-TYPE    = "3" OR "4" OR "5" OR "6")
               AND (ERROR-FOUND))
P72365*        OR  (BNH-CHANGE-TYPE     = "2")
P09149             MOVE WS-TRUE        TO WS-CO-ERR-SW
                   MOVE CRT-ERROR-NBR  TO WF-ERROR-NBR
                   MOVE CRT-ERROR-CAT  TO WF-ERROR-CAT
                   MOVE CRT-ERR-VAR1   TO WF-ERR-VAR1.

           IF (PROC-BEN)
           OR (PROC-EFD)
           OR (PROC-PLN)
               MOVE "3"                    TO WF-REC-TYPE
               IF (PROC-BEN)
                   MOVE "BEN"              TO WF-FILE-PREFIX

                   IF (ELIG-ERROR)
                       MOVE 166            TO WF-WARNING-NBR
                   ELSE
                       IF (CRT-ERROR-NBR       NOT = ZEROES)
P09149                     MOVE WS-TRUE        TO WS-CO-ERR-SW
                           MOVE CRT-ERROR-NBR  TO WF-ERROR-NBR
                           MOVE CRT-ERROR-CAT  TO WF-ERROR-CAT
                           MOVE CRT-ERR-VAR1   TO WF-ERR-VAR1
                       END-IF
                   END-IF

                   IF ((TERM-RULES)
                   OR  (CHANGE-RULES))
                   AND (NO-PROC-163-ERR)
                       MOVE WF2-FUNCTION-CODE
                                           TO WF-FUNCTION-CODE
                       MOVE WF2-PLAN-TYPE  TO WF-PLAN-TYPE
                       MOVE WF2-PLAN-CODE  TO WF-PLAN-CODE
                       MOVE WF2-START-DATE TO WF-START-DATE
                   ELSE
                       MOVE WF1-FUNCTION-CODE
                                           TO WF-FUNCTION-CODE
                       MOVE WF1-PLAN-TYPE  TO WF-PLAN-TYPE
                       MOVE WF1-PLAN-CODE  TO WF-PLAN-CODE
                       MOVE WF1-START-DATE TO WF-START-DATE
                   END-IF
               ELSE
               IF (PROC-EFD)
                   MOVE "EFD"              TO WF-FILE-PREFIX

                   IF (ELIG-ERROR)
                       MOVE 166            TO WF-WARNING-NBR
                   ELSE
                       IF (CRT-ERROR-NBR       NOT = ZEROES)
P09149                     MOVE WS-TRUE        TO WS-CO-ERR-SW
                           MOVE CRT-ERROR-NBR  TO WF-ERROR-NBR
                           MOVE CRT-ERROR-CAT  TO WF-ERROR-CAT
                           MOVE CRT-ERR-VAR1   TO WF-ERR-VAR1
                       END-IF
                   END-IF

                   MOVE "FL"               TO WF-PLAN-TYPE
P77134             MOVE FLP-FLEX-PLAN      TO WF-PLAN-CODE
P77134*            MOVE EFD-FLEX-PLAN      TO WF-PLAN-CODE

                   IF (TERM-RULES)
                   OR (CHANGE-RULES)
                       MOVE WF4-FUNCTION-CODE
                                           TO WF-FUNCTION-CODE
                       MOVE WF4-START-DATE TO WF-START-DATE
PERF                   MOVE EFD-FLEX-PLAN  TO WF-PLAN-CODE
                   ELSE
                       MOVE WF3-FUNCTION-CODE
                                           TO WF-FUNCTION-CODE
                       MOVE WF3-START-DATE TO WF-START-DATE
PERF                   MOVE WF3-FLEX-PLAN  TO WF-PLAN-CODE
                   END-IF
               ELSE
               IF (PROC-PLN)
                   MOVE "PLN"              TO WF-FILE-PREFIX

                   MOVE PLN-PLAN-TYPE      TO WF-PLAN-TYPE
                   MOVE PLN-PLAN-CODE      TO WF-PLAN-CODE

                   MOVE WF1-FUNCTION-CODE  TO WF-FUNCTION-CODE

                   MOVE WS-ADD-DATE        TO WF-START-DATE

                   IF (ELIG-ERROR)
                       MOVE 166            TO WF-WARNING-NBR
                       MOVE ZEROES         TO WF-ERROR-NBR
                       MOVE SPACES         TO WF-ERROR-CAT 
                   ELSE
                       MOVE CRT-ERROR-NBR  TO WF-ERROR-NBR
P09149                 MOVE WS-TRUE        TO WS-CO-ERR-SW
                       MOVE CRT-ERROR-CAT  TO WF-ERROR-CAT
                       MOVE CRT-ERR-VAR1   TO WF-ERR-VAR1
                   END-IF.

J57497     MOVE WS-NEW-DATE                TO WF-NEW-DATE.

       1810-END.

006960******************************************************************
       1820-CREATE-ETEC-EFDC.
006960******************************************************************

           PERFORM 8000-CREATE-BN100EFDC.

           MOVE EFD-COMPANY            TO WF4-COMPANY.
           MOVE EFD-EMPLOYEE           TO WF4-EMPLOYEE.
           MOVE EFD-START-DATE         TO WF4-START-DATE.

           IF (TERM-RULES)
               MOVE "ET"               TO WF4-FUNCTION-CODE
           ELSE
               MOVE "EC"               TO WF4-FUNCTION-CODE.

           MOVE WS-EFFECT-DATE         TO WF4-WF-EFFECT-DATE.

           COMPUTE WF4-WF-SEQ-NBR      = WS-SEQ-NBR
                                       + 1.

           PERFORM 8200-STORE-BN100EFDC.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

       1820-END.

006960******************************************************************
       1825-CREATE-EA-EFDA.
006960******************************************************************

           PERFORM 8000-CREATE-BN100EFDA.

           MOVE PRM-COMPANY            TO WF3-COMPANY.
           MOVE WS-EMPLOYEE            TO WF3-EMPLOYEE.
P53194
P53194     IF  (CHANGE-RULES)
P53194     AND (EMPFLEXDOL-NOTFOUND)
P53194     AND (NO-ERROR-FOR-CA)
P53194         SET ADD-RULES           TO TRUE
P53194     END-IF.
P53194
           IF (ERROR-FOR-CA)
      ******** WSDR-FR-DATE IS ALREADY FILLED WITH STOP DATE OF EFDC
               PERFORM 900-DATE-TO-JULIAN
               ADD 1                   TO WSDR-JULIAN-DAYS
               PERFORM 900-JULIAN-TO-DATE
               MOVE WSDR-FR-DATE       TO WF3-START-DATE
           ELSE
           IF (CHANGE-RULES)
               MOVE EFD-START-DATE     TO WF3-START-DATE
           ELSE
           IF (ADD-RULES)
               MOVE WS-ADD-DATE        TO WF3-START-DATE.

           PERFORM 8400-FIND-BN100EFDA.
           IF (BN100EFDA-FOUND)
               GO TO 1825-END.

           MOVE "EA"                   TO WF3-FUNCTION-CODE.

           MOVE FLP-FLEX-PLAN          TO WF3-FLEX-PLAN.

           MOVE WS-EFFECT-DATE         TO WF3-WF-EFFECT-DATE.

           COMPUTE WF3-WF-SEQ-NBR      = WS-SEQ-NBR
                                       + 1.

           PERFORM 8200-STORE-BN100EFDA.

           SET ADD-RULES               TO TRUE.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           SET CHANGE-RULES            TO TRUE.

       1825-END.

006960******************************************************************
       1830-CREATE-ETEC-BENC.
006960******************************************************************

           PERFORM 8000-CREATE-BN100BENC.

           MOVE BEN-COMPANY            TO WF2-COMPANY.
           MOVE BEN-EMPLOYEE           TO WF2-EMPLOYEE.
           MOVE BEN-PLAN-TYPE          TO WF2-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO WF2-PLAN-CODE.
           MOVE BEN-START-DATE         TO WF2-START-DATE.

           IF (TERM-RULES)
               MOVE "ET"               TO WF2-FUNCTION-CODE
           ELSE
           IF (CRT-ERROR-NBR           = 133)
      ******** ERROR 133 IS SALARY OVERRIDE ERROR
               MOVE "SO"               TO WF2-FUNCTION-CODE
           ELSE
               MOVE "EC"               TO WF2-FUNCTION-CODE.

           MOVE WS-EFFECT-DATE         TO WF2-WF-EFFECT-DATE.

           COMPUTE WF2-WF-SEQ-NBR      = WS-SEQ-NBR
                                       + 1.

           PERFORM 8200-STORE-BN100BENC.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

       1830-END.

006960******************************************************************
       1832-CREATE-ED-BENC.
006960******************************************************************

           PERFORM 8000-CREATE-BN100BENC.

           MOVE BEN-COMPANY            TO WF2-COMPANY.
           MOVE BEN-EMPLOYEE           TO WF2-EMPLOYEE.
           MOVE BEN-PLAN-TYPE          TO WF2-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO WF2-PLAN-CODE.
           MOVE BEN-START-DATE         TO WF2-START-DATE.

           MOVE "ED"                   TO WF2-FUNCTION-CODE.

           MOVE WS-EFFECT-DATE         TO WF2-WF-EFFECT-DATE.

           COMPUTE WF2-WF-SEQ-NBR      = WS-SEQ-NBR
                                       + 1.

           PERFORM 8200-STORE-BN100BENC.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

       1832-END.

006960******************************************************************
       1835-CREATE-EA-BENA.
006960******************************************************************

           PERFORM 8000-CREATE-BN100BENA.

           IF (PROC-PLN)
               MOVE PLN-COMPANY        TO WF1-COMPANY
               MOVE WS-EMPLOYEE        TO WF1-EMPLOYEE
               MOVE PLN-PLAN-TYPE      TO WF1-PLAN-TYPE
               MOVE PLN-PLAN-CODE      TO WF1-PLAN-CODE
               MOVE WS-ADD-DATE        TO WF1-START-DATE
           ELSE
               MOVE BEN-COMPANY        TO WF1-COMPANY
               MOVE BEN-EMPLOYEE       TO WF1-EMPLOYEE
               MOVE BEN-PLAN-TYPE      TO WF1-PLAN-TYPE
               MOVE BEN-PLAN-CODE      TO WF1-PLAN-CODE
               IF (ERROR-FOR-CA)
                   MOVE WSDR-FR-DATE   TO WF1-START-DATE
               ELSE
                   MOVE BEN-START-DATE TO WF1-START-DATE.

           PERFORM 8400-FIND-BN100BENA.
           IF (BN100BENA-FOUND)
               GO TO 1835-END.

           MOVE "EA"                   TO WF1-FUNCTION-CODE.

           MOVE WS-EFFECT-DATE         TO WF1-WF-EFFECT-DATE.

           COMPUTE WF1-WF-SEQ-NBR      = WS-SEQ-NBR
                                       + 1.

J57497     MOVE WS-NEW-DATE            TO WF1-NEW-DATE.

           PERFORM 8200-STORE-BN100BENA.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

       1835-END.

006960******************************************************************
       1842-CREATE-S-EFDC.
006960******************************************************************

           PERFORM 8000-CREATE-BN100EFDC.

           MOVE EFD-COMPANY            TO WF4-COMPANY.
           MOVE EFD-EMPLOYEE           TO WF4-EMPLOYEE.
           MOVE EFD-START-DATE         TO WF4-START-DATE.

           MOVE "S"                    TO WF4-FUNCTION-CODE.

           MOVE WS-TERM-DATE           TO WF4-STOP-DATE.

           MOVE WS-EFFECT-DATE         TO WF4-WF-EFFECT-DATE.

           COMPUTE WF4-WF-SEQ-NBR      = WS-SEQ-NBR
                                       + 1.

           PERFORM 8200-STORE-BN100EFDC.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           SET EFDC-DATA               TO TRUE.

       1842-END.

006960******************************************************************
       1846-CREATE-CS-EFDC.
006960******************************************************************

           PERFORM 8000-CREATE-BN100EFDC.

           MOVE EFD-COMPANY            TO WF4-COMPANY.
           MOVE EFD-EMPLOYEE           TO WF4-EMPLOYEE.
           MOVE EFD-START-DATE         TO WF4-START-DATE.

           MOVE "CS"                   TO WF4-FUNCTION-CODE.

           MOVE WS-CHANGE-DATE         TO WSDR-FR-DATE.
           PERFORM 900-DATE-TO-JULIAN.
           SUBTRACT 1                  FROM WSDR-JULIAN-DAYS.
           PERFORM 900-JULIAN-TO-DATE.
           MOVE WSDR-FR-DATE           TO WF4-STOP-DATE.

           MOVE WS-EFFECT-DATE         TO WF4-WF-EFFECT-DATE.

           COMPUTE WF4-WF-SEQ-NBR      = WS-SEQ-NBR
                                       + 1.

           PERFORM 8200-STORE-BN100EFDC.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           SET EFDC-DATA               TO TRUE.

       1846-END.

006960******************************************************************
       1848-CREATE-CA-EFDA.
006960******************************************************************

           PERFORM 8000-CREATE-BN100EFDA.

           MOVE EFD-COMPANY            TO WF3-COMPANY.
           MOVE EFD-EMPLOYEE           TO WF3-EMPLOYEE.

           MOVE "CA"                   TO WF3-FUNCTION-CODE.

           MOVE WS-CHANGE-DATE         TO WF3-START-DATE.

           MOVE EFD-STOP-DATE          TO WF3-STOP-DATE.
           MOVE EFD-FLEX-PLAN          TO WF3-FLEX-PLAN.

           IF (WS-FLD-START-DATE       NOT = ZEROES)
           OR (WS-FLD-GROUP-NAME       NOT = SPACES)
               MOVE WS-FLD-START-DATE  TO WF3-FLD-START-DATE
               MOVE WS-FLD-GROUP-NAME  TO WF3-FLD-GROUP-NAME
               INITIALIZE WS-FLD-START-DATE
                          WS-FLD-GROUP-NAME
           ELSE
               MOVE EFD-FLD-GROUP-NAME TO WF3-FLD-GROUP-NAME
               MOVE EFD-FLD-START-DATE TO WF3-FLD-START-DATE.

           IF (BNWS-PROC-DATE          NOT = ZEROES)
               MOVE "Y"                TO WF3-RETRO-CHANGE.

           MOVE WS-EFFECT-DATE         TO WF3-WF-EFFECT-DATE.
           MOVE WS-CREDITS-SPENT       TO WF3-CREDITS-SPENT.
           INITIALIZE WS-CREDITS-SPENT.

           COMPUTE WF3-WF-SEQ-NBR      = WS-SEQ-NBR
                                       + 1.

           PERFORM 8200-STORE-BN100EFDA.

           SET ADD-RULES               TO TRUE.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           SET CHANGE-RULES            TO TRUE.

           SET EFDA-DATA               TO TRUE.

       1848-END.

006960******************************************************************
       1850-CREATE-A-EFDA.
006960******************************************************************

           PERFORM 8000-CREATE-BN100EFDA.

           MOVE EMP-COMPANY            TO WF3-COMPANY.
           MOVE EMP-EMPLOYEE           TO WF3-EMPLOYEE.

           IF (CHANGE-RULES)
               MOVE WS-CHANGE-DATE     TO WF3-START-DATE
           ELSE
               MOVE WS-ADD-DATE        TO WF3-START-DATE.

           MOVE WS-STOP-DATE           TO WF3-STOP-DATE.

           MOVE FLP-FLEX-PLAN          TO WF3-FLEX-PLAN.
           MOVE FLD-GROUP-NAME         TO WF3-FLD-GROUP-NAME.
           MOVE FLD-START-DATE         TO WF3-FLD-START-DATE.

           MOVE "A"                    TO WF3-FUNCTION-CODE.

           MOVE WS-EFFECT-DATE         TO WF3-WF-EFFECT-DATE.

           COMPUTE WF3-WF-SEQ-NBR      = WS-SEQ-NBR
                                       + 1.

           PERFORM 8200-STORE-BN100EFDA.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           SET EFDA-DATA               TO TRUE.

       1850-END.

006960******************************************************************
       1852-CREATE-S-BENC.
006960******************************************************************

           PERFORM 8000-CREATE-BN100BENC.

           MOVE BEN-COMPANY            TO WF2-COMPANY.
           MOVE BEN-EMPLOYEE           TO WF2-EMPLOYEE.
           MOVE BEN-PLAN-TYPE          TO WF2-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO WF2-PLAN-CODE.
           MOVE BEN-START-DATE         TO WF2-START-DATE.

           MOVE "S"                    TO WF2-FUNCTION-CODE.

           MOVE WS-TERM-DATE           TO WF2-STOP-DATE.

           MOVE BWT-COBRA-OCC          TO WF2-COBRA-OCC.
           MOVE BWT-COBRA-TYPE         TO WF2-COBRA-TYPE.

           MOVE BEN-FLEX-PLAN          TO WF2-FLEX-PLAN.

           MOVE WS-EFFECT-DATE         TO WF2-WF-EFFECT-DATE.

           COMPUTE WF2-WF-SEQ-NBR      = WS-SEQ-NBR
                                       + 1.

           MOVE BNBEN-HIPAA-REASON     TO WF2-HIPAA-REASON.

           PERFORM 8200-STORE-BN100BENC.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           SET BENC-DATA               TO TRUE.

       1852-END.

006960******************************************************************
       1854-CREATE-D-BENC.
006960******************************************************************

           PERFORM 8000-CREATE-BN100BENC.

           MOVE BEN-COMPANY            TO WF2-COMPANY.
           MOVE BEN-EMPLOYEE           TO WF2-EMPLOYEE.
           MOVE BEN-PLAN-TYPE          TO WF2-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO WF2-PLAN-CODE.
           MOVE BEN-START-DATE         TO WF2-START-DATE.
           MOVE BEN-STOP-DATE          TO WF2-STOP-DATE.

           MOVE "D"                    TO WF2-FUNCTION-CODE.

           MOVE WS-EFFECT-DATE         TO WF2-WF-EFFECT-DATE.

           COMPUTE WF2-WF-SEQ-NBR      = WS-SEQ-NBR
                                       + 1.

           MOVE BNBEN-HIPAA-REASON     TO WF2-HIPAA-REASON.

           PERFORM 8200-STORE-BN100BENC.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           SET BENC-DATA               TO TRUE.

       1854-END.

006960******************************************************************
       1856-CREATE-CS-BENC.
006960******************************************************************

           IF (CREATE-ONLY-BENA)
               GO TO 1856-END.

           PERFORM 8000-CREATE-BN100BENC.

           MOVE BEN-COMPANY            TO WF2-COMPANY.
           MOVE BEN-EMPLOYEE           TO WF2-EMPLOYEE.
           MOVE BEN-PLAN-TYPE          TO WF2-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO WF2-PLAN-CODE.
           MOVE BEN-START-DATE         TO WF2-START-DATE.

           MOVE "CS"                   TO WF2-FUNCTION-CODE.

           MOVE WS-CHANGE-DATE         TO WSDR-FR-DATE.
           PERFORM 900-DATE-TO-JULIAN.
           SUBTRACT 1                  FROM WSDR-JULIAN-DAYS.
           PERFORM 900-JULIAN-TO-DATE.
           MOVE WSDR-FR-DATE           TO WF2-STOP-DATE.

           MOVE BEN-FLEX-PLAN          TO WF2-FLEX-PLAN.

           MOVE WS-EFFECT-DATE         TO WF2-WF-EFFECT-DATE.

           COMPUTE WF2-WF-SEQ-NBR      = WS-SEQ-NBR
                                       + 1.

           MOVE BNBEN-HIPAA-REASON     TO WF2-HIPAA-REASON.

           PERFORM 8200-STORE-BN100BENC.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           SET BENC-DATA               TO TRUE.

       1856-END.

006960******************************************************************
       1858-CREATE-CA-BENA.
006960******************************************************************

           IF (BNBEN-LINE-FC (I1)          NOT = "A")
               GO TO 1858-END.

           IF  (BNBEN-NBR-LINES            > BNBEN-ORIG-NBR-LINES)
           AND (BNBEN-FUTURE-ERROR-SW (I1) = "Y")
               GO TO 1858-END.

           PERFORM 8000-CREATE-BN100BENA.

           MOVE BEN-COMPANY            TO WF1-COMPANY.
           MOVE BEN-EMPLOYEE           TO WF1-EMPLOYEE.
           MOVE BEN-PLAN-TYPE          TO WF1-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO WF1-PLAN-CODE.
           MOVE BEN-START-DATE         TO WF1-OLD-START-DATE.
P85174     MOVE BEN-DED-STOP-DATE      TO WF1-OLD-DED-STOP-DATE.

           MOVE "CA"                   TO WF1-FUNCTION-CODE.

           IF (I1                      > BNBEN-ORIG-NBR-LINES)
               MOVE BNBEN-START-DATE (I1)  TO WF1-START-DATE
           ELSE
               MOVE WS-CHANGE-DATE         TO WF1-START-DATE.

           IF  (CREATE-ONLY-BENA)
           AND (WS-STOP-DATE           NOT = ZEROES)
               MOVE WS-STOP-DATE       TO WF1-STOP-DATE
           ELSE
           IF  (MOVE-2ND-BNBEN-LINE)
           AND (I1                         = BNBEN-ORIG-NBR-LINES)
               MOVE BEN-STOP-DATE          TO WF1-STOP-DATE
           ELSE
               MOVE BNBEN-STOP-DATE (I1)   TO WF1-STOP-DATE.

           IF (NOT ADD-GROUP-CHANGES)
               MOVE BNH-CHANGE-TYPE    TO WF1-CHANGE-TYPE.

           MOVE WS-EFFECT-DATE         TO WF1-WF-EFFECT-DATE.

           COMPUTE WF1-WF-SEQ-NBR      = WS-SEQ-NBR
                                       + 1.

           IF (CHANGE-RULES)
J67795*        MOVE BNBEN-COVER-AMT    (I1) TO WF1-COVER-AMT
J67795         COMPUTE WF1-COVER-AMT ROUNDED = 
J67795              BNBEN-COVER-AMT (I1) * 1
J67795*        MOVE BNBEN-EMP-PRE-CONT (I1) TO WF1-EMP-PRE-CONT
J67795         COMPUTE WF1-EMP-PRE-CONT ROUNDED = 
J67795              BNBEN-EMP-PRE-CONT (I1) * 1
J67795*        MOVE BNBEN-EMP-AFT-CONT (I1) TO WF1-EMP-AFT-CONT
J67795         COMPUTE WF1-EMP-AFT-CONT ROUNDED = 
J67795              BNBEN-EMP-AFT-CONT (I1) * 1
J67795*        MOVE BNBEN-CMP-FLX-CONT (I1) TO WF1-CMP-FLX-CONT
J67795         COMPUTE WF1-CMP-FLX-CONT ROUNDED = 
J67795              BNBEN-CMP-FLX-CONT (I1) * 1 
J67795*        MOVE BNBEN-COMP-CONT    (I1) TO WF1-COMP-CONT.
J67795         COMPUTE WF1-COMP-CONT ROUNDED = 
J67795              BNBEN-COMP-CONT (I1) * 1

           IF (BNBEN-PAY-FRQ-ADD-ERR (2) = "Y")
               MOVE "EA"               TO WF1-FUNCTION-CODE
               MOVE ZEROES             TO WF1-COVER-AMT
               MOVE 158                TO CRT-ERROR-NBR.
       
           IF (BNWS-PROC-DATE          NOT = ZEROES)
               MOVE "Y"                TO WF1-RETRO-CHANGE.

           MOVE BNBEN-HIPAA-REASON     TO WF1-HIPAA-REASON.
155990     MOVE BNBEN-ELIG-GROUP (I1)  TO WF1-ELIG-GROUP.

J57497     MOVE WS-NEW-DATE            TO WF1-NEW-DATE.

           PERFORM 8200-STORE-BN100BENA.

           MOVE WF1-START-DATE         TO WS-ADD-DATE.

           SET ADD-RULES               TO TRUE.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           SET CHANGE-RULES            TO TRUE.

           SET BENC-DATA               TO TRUE.

       1858-END.

006960******************************************************************
       1860-CREATE-SA-BENA.
006960******************************************************************

           PERFORM 8000-CREATE-BN100BENA.

           MOVE BNBEN-COMPANY              TO WF1-COMPANY.
           MOVE BNBEN-EMPLOYEE (WS-I3)     TO WF1-EMPLOYEE.
           MOVE BNBEN-PLAN-TYPE (WS-I3)    TO WF1-PLAN-TYPE.
           MOVE BNBEN-PLAN-CODE (WS-I3)    TO WF1-PLAN-CODE.
           MOVE BNBEN-START-DATE (WS-I3)   TO WF1-START-DATE.
           MOVE BNBEN-STOP-DATE (WS-I3)    TO WF1-STOP-DATE.

           MOVE BNBEN-START-DATE (1)       TO WF1-OLD-START-DATE.

           MOVE "SA"                       TO WF1-FUNCTION-CODE.

           MOVE WS-EFFECT-DATE             TO WF1-WF-EFFECT-DATE.

           COMPUTE WF1-WF-SEQ-NBR          = WS-SEQ-NBR
                                           + 1.

J67795*    MOVE BNBEN-COVER-AMT    (WS-I3) TO WF1-COVER-AMT.
J67795     COMPUTE WF1-COVER-AMT ROUNDED = 
J67795          BNBEN-COVER-AMT (WS-I3) * 1.
           IF (BNBEN-ZERO-PREMIUM-SW (WS-I3) = 1)
           OR (BNBEN-POS-PREMIUM-SW  (WS-I3) = 1)
J67795*        MOVE BNBEN-EMP-PRE-CONT (WS-I3)
J67795*                                    TO WF1-EMP-PRE-CONT.
J67795         COMPUTE WF1-EMP-PRE-CONT ROUNDED = 
J67795                 BNBEN-EMP-PRE-CONT (WS-I3) * 1.
J67795*    MOVE BNBEN-EMP-AFT-CONT (WS-I3) TO WF1-EMP-AFT-CONT.
J67795     COMPUTE WF1-EMP-AFT-CONT ROUNDED = 
J67795          BNBEN-EMP-AFT-CONT (WS-I3) * 1.
J67795*    MOVE BNBEN-CMP-FLX-CONT (WS-I3) TO WF1-CMP-FLX-CONT.
J67795     COMPUTE WF1-CMP-FLX-CONT ROUNDED = 
J67795          BNBEN-CMP-FLX-CONT (WS-I3) * 1.
J67795*    MOVE BNBEN-COMP-CONT    (WS-I3) TO WF1-COMP-CONT.
J67795     COMPUTE WF1-COMP-CONT ROUNDED = 
J67795          BNBEN-COMP-CONT (WS-I3) * 1.

155990     MOVE BNBEN-ELIG-GROUP (I1)      TO WF1-ELIG-GROUP.
155990
J57497     MOVE WS-NEW-DATE                TO WF1-NEW-DATE.

           PERFORM 8200-STORE-BN100BENA.

           SET PROC-163-ERR                TO TRUE.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           SET NO-PROC-163-ERR             TO TRUE.

           SET BENC-DATA                   TO TRUE.

       1860-END.

006960******************************************************************
       1862-CREATE-A-BENA.
006960******************************************************************

           IF  (BNBEN-NBR-LINES            > BNBEN-ORIG-NBR-LINES)
           AND (BNBEN-FUTURE-ERROR-SW (I1) = "Y")
               GO TO 1862-END.

           PERFORM 8000-CREATE-BN100BENA.

           MOVE EMP-COMPANY            TO WF1-COMPANY.
           MOVE EMP-EMPLOYEE           TO WF1-EMPLOYEE.
           MOVE PLN-PLAN-TYPE          TO WF1-PLAN-TYPE.
           MOVE PLN-PLAN-CODE          TO WF1-PLAN-CODE.

           IF (I1                      > BNBEN-ORIG-NBR-LINES)
               MOVE BNBEN-START-DATE (I1)  TO WF1-START-DATE
           ELSE
               MOVE WS-ADD-DATE            TO WF1-START-DATE.

           MOVE BNBEN-STOP-DATE (I1)   TO WF1-STOP-DATE.

J67795*    MOVE BNBEN-COVER-AMT (I1)   TO WF1-COVER-AMT.
J67795     COMPUTE WF1-COVER-AMT ROUNDED =
J67795          BNBEN-COVER-AMT (I1) * 1.
J67795*    MOVE BNBEN-CMP-FLX-CONT(I1) TO WF1-CMP-FLX-CONT.
J67795     COMPUTE WF1-CMP-FLX-CONT ROUNDED =
J67795          BNBEN-CMP-FLX-CONT (I1) * 1.
J67795*    MOVE BNBEN-EMP-PRE-CONT(I1) TO WF1-EMP-PRE-CONT.
J67795     COMPUTE WF1-EMP-PRE-CONT ROUNDED =
J67795          BNBEN-EMP-PRE-CONT (I1) * 1.
J67795*    MOVE BNBEN-EMP-AFT-CONT(I1) TO WF1-EMP-AFT-CONT.
J67795     COMPUTE WF1-EMP-AFT-CONT ROUNDED =
J67795          BNBEN-EMP-AFT-CONT (I1) * 1.
J67795*    MOVE BNBEN-COMP-CONT (I1)   TO WF1-COMP-CONT.
J67795     COMPUTE WF1-COMP-CONT ROUNDED =
J67795          BNBEN-COMP-CONT (I1) * 1.

           MOVE "A"                    TO WF1-FUNCTION-CODE.

           MOVE WS-EFFECT-DATE         TO WF1-WF-EFFECT-DATE.

           COMPUTE WF1-WF-SEQ-NBR      = WS-SEQ-NBR
                                       + 1.

           MOVE BNBEN-HIPAA-REASON     TO WF1-HIPAA-REASON.
155990     MOVE BNBEN-ELIG-GROUP (I1)  TO WF1-ELIG-GROUP.

J57497     MOVE WS-NEW-DATE            TO WF1-NEW-DATE.

           PERFORM 8200-STORE-BN100BENA.

           MOVE WF1-START-DATE         TO WS-ADD-DATE.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           SET BENA-DATA               TO TRUE.

       1862-END.

006960******************************************************************
       1864-CREATE-EA-BENA.
006960******************************************************************

           PERFORM 8000-CREATE-BN100BENA.

           MOVE BNBEN-COMPANY              TO WF1-COMPANY.
           MOVE BNBEN-EMPLOYEE (WS-I3)     TO WF1-EMPLOYEE.
           MOVE BNBEN-PLAN-TYPE (WS-I3)    TO WF1-PLAN-TYPE.
           MOVE BNBEN-PLAN-CODE (WS-I3)    TO WF1-PLAN-CODE.
           MOVE BNBEN-START-DATE (WS-I3)   TO WF1-START-DATE.
           MOVE BNBEN-STOP-DATE (WS-I3)    TO WF1-STOP-DATE.

           MOVE "EA"                       TO WF1-FUNCTION-CODE.

           MOVE WS-EFFECT-DATE             TO WF1-WF-EFFECT-DATE.

           COMPUTE WF1-WF-SEQ-NBR          = WS-SEQ-NBR
                                           + 1.

J57497     MOVE WS-NEW-DATE                TO WF1-NEW-DATE.

           PERFORM 8200-STORE-BN100BENA.

           SET PROC-163-ERR                TO TRUE.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           SET NO-PROC-163-ERR             TO TRUE.

       1864-END.

006960******************************************************************
       1868-CHECK-YTD-N-HIPAA.
006960******************************************************************

           MOVE BEN-COMPANY                TO DB-COMPANY.
           MOVE BEN-PLAN-TYPE              TO DB-PLAN-TYPE.
           MOVE BEN-PLAN-CODE              TO DB-PLAN-CODE.
           PERFORM 840-FIND-PLNSET1.

           MOVE BEN-EMPLOYEE               TO DB-EMPLOYEE.

           IF (BEN-FLEX-DED-SEQ            NOT = ZEROES)
               MOVE PLN-FLEX-DED-CODE      TO DB-DED-CODE
               MOVE BEN-FLEX-DED-SEQ       TO DB-SEQ-NBR
               PERFORM 840-FIND-EDMSET1
               MOVE PDHSET1-SEQ-NBR        TO WS-DB-BEG-RNG
               PERFORM 850-KFIND-BEGRNG-PDHSET1
               IF (PREMDEDHST-KFOUND)
                   MOVE 134                TO CRT-ERROR-NBR
P78374             MOVE "BN100"            TO CRT-ERROR-CAT
                   GO TO 1868-END.

           IF (BEN-PRE-SEQ-NBR             NOT = ZEROES)
               IF (BEN-PCT-AMT-FLAG        = "A")
                   MOVE PLN-PRE-DED-CODE-A TO DB-DED-CODE
               ELSE
                   MOVE PLN-PRE-DED-CODE-P TO DB-DED-CODE
               END-IF
               MOVE BEN-PRE-SEQ-NBR        TO DB-SEQ-NBR
               PERFORM 840-FIND-EDMSET1
               MOVE PDHSET1-SEQ-NBR        TO WS-DB-BEG-RNG
               PERFORM 850-KFIND-BEGRNG-PDHSET1
               IF (PREMDEDHST-KFOUND)
                   MOVE 134                TO CRT-ERROR-NBR
P78374             MOVE "BN100"            TO CRT-ERROR-CAT
                   GO TO 1868-END.

           IF (BEN-AFT-SEQ-NBR             NOT = ZEROES)
               IF (BEN-PCT-AMT-FLAG        = "A")
                   MOVE PLN-AFT-DED-CODE-A TO DB-DED-CODE
               ELSE
                   MOVE PLN-AFT-DED-CODE-P TO DB-DED-CODE
               END-IF
               MOVE BEN-AFT-SEQ-NBR        TO DB-SEQ-NBR
               PERFORM 840-FIND-EDMSET1
               MOVE PDHSET1-SEQ-NBR        TO WS-DB-BEG-RNG
               PERFORM 850-KFIND-BEGRNG-PDHSET1
               IF (PREMDEDHST-KFOUND)
                   MOVE 134                TO CRT-ERROR-NBR
P78374             MOVE "BN100"            TO CRT-ERROR-CAT
                   GO TO 1868-END.

           IF (BEN-CMP-SEQ-NBR             NOT = ZEROES)
               IF (BEN-PCT-AMT-FLAG        = SPACES)
                   IF (PLN-CMP-DED-CODE-A NOT = SPACES)
                       MOVE PLN-CMP-DED-CODE-A TO DB-DED-CODE
                   ELSE
                       MOVE PLN-CMP-DED-CODE-P TO DB-DED-CODE
                   END-IF
               ELSE
               IF (BEN-PCT-AMT-FLAG        = "A")
                   MOVE PLN-CMP-DED-CODE-A TO DB-DED-CODE
               ELSE
                   MOVE PLN-CMP-DED-CODE-P TO DB-DED-CODE
               END-IF
               END-IF
               MOVE BEN-CMP-SEQ-NBR        TO DB-SEQ-NBR
               PERFORM 840-FIND-EDMSET1
               IF (EMDEDMASTR-NOTFOUND)
                   IF (BEN-PCT-AMT-FLAG    = "A")
                       MOVE PLN-PRE-DED-MTCH-A TO DB-DED-CODE
                   ELSE
                       MOVE PLN-PRE-DED-MTCH-P TO DB-DED-CODE
                   END-IF
                   PERFORM 840-FIND-EDMSET1
               END-IF
               MOVE PDHSET1-SEQ-NBR        TO WS-DB-BEG-RNG
               PERFORM 850-KFIND-BEGRNG-PDHSET1
               IF (PREMDEDHST-KFOUND)
                   MOVE 134                TO CRT-ERROR-NBR
P78374             MOVE "BN100"            TO CRT-ERROR-CAT
                   GO TO 1868-END.

           IF (BEN-CMP-AFT-SEQ             NOT = ZEROES)
               IF (BEN-PCT-AMT-FLAG        = "A")
                   MOVE PLN-AFT-DED-MTCH-A TO DB-DED-CODE
               ELSE
                   MOVE PLN-AFT-DED-MTCH-P TO DB-DED-CODE
               END-IF
               MOVE BEN-CMP-AFT-SEQ        TO DB-SEQ-NBR
               PERFORM 840-FIND-EDMSET1
               MOVE PDHSET1-SEQ-NBR        TO WS-DB-BEG-RNG
               PERFORM 850-KFIND-BEGRNG-PDHSET1
               IF (PREMDEDHST-KFOUND)
                   MOVE 134                TO CRT-ERROR-NBR
P78374             MOVE "BN100"            TO CRT-ERROR-CAT
                   GO TO 1868-END.

           IF  ((PRM-CREATE-TRANS      = "Y")
           OR   ((PRM-CREATE-TRANS     = SPACES)
           AND   (PLN-CREATE-TRANS     = "Y")))
           AND (BNBEN-HIPAA-REASON     = SPACES)
               MOVE "BNBEN"            TO CRT-ERROR-CAT
               MOVE 226                TO CRT-ERROR-NBR
               GO TO 1868-END.

       1868-END.

006960******************************************************************
       1870-CREATE-C-BENC.
006960******************************************************************

           PERFORM 8000-CREATE-BN100BENC.

           MOVE BEN-COMPANY            TO WF2-COMPANY.
           MOVE BEN-EMPLOYEE           TO WF2-EMPLOYEE.
           MOVE BEN-PLAN-TYPE          TO WF2-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO WF2-PLAN-CODE.
           MOVE BEN-START-DATE         TO WF2-START-DATE.
           MOVE BEN-STOP-DATE          TO WF2-STOP-DATE.

           MOVE "C"                    TO WF2-FUNCTION-CODE.

           IF (NOT ADD-GROUP-CHANGES)
               MOVE BNH-CHANGE-TYPE    TO WF2-CHANGE-TYPE.

           MOVE WS-EFFECT-DATE         TO WF2-WF-EFFECT-DATE.

J67795*    MOVE BNBEN-COVER-AMT    (1) TO WF2-COVER-AMT.
J67795     COMPUTE WF2-COVER-AMT ROUNDED =
J67795          BNBEN-COVER-AMT (1) * 1.
J67795*    MOVE BNBEN-EMP-PRE-CONT (1) TO WF2-EMP-PRE-CONT.
J67795     COMPUTE WF2-EMP-PRE-CONT ROUNDED =
J67795          BNBEN-EMP-PRE-CONT (1) * 1.
J67795*    MOVE BNBEN-EMP-AFT-CONT (1) TO WF2-EMP-AFT-CONT.
J67795     COMPUTE WF2-EMP-AFT-CONT ROUNDED = 
J67795          BNBEN-EMP-AFT-CONT (1) * 1.
J67795*    MOVE BNBEN-CMP-FLX-CONT (1) TO WF2-CMP-FLX-CONT.
J67795     COMPUTE WF2-CMP-FLX-CONT ROUNDED =
J67795          BNBEN-CMP-FLX-CONT (1) * 1.
J67795*    MOVE BNBEN-COMP-CONT    (1) TO WF2-COMP-CONT.
J67795     COMPUTE WF2-COMP-CONT ROUNDED =
J67795          BNBEN-COMP-CONT (1) * 1.

           COMPUTE WF2-WF-SEQ-NBR      = WS-SEQ-NBR
                                       + 1.

           IF (BNWS-PROC-DATE          NOT = ZEROES)
               MOVE "Y"                TO WF2-RETRO-CHANGE.

           MOVE BNBEN-HIPAA-REASON     TO WF2-HIPAA-REASON.

           PERFORM 8200-STORE-BN100BENC.

           PERFORM 1800-CREATE-BN100WORK
           THRU    1800-END.

           SET BENC-DATA               TO TRUE.

       1870-END.

006960******************************************************************
       1892-FIND-FUTURE-EFR.
006960******************************************************************

           IF (TERM-RULES)
               MOVE WS-TERM-DATE       TO WS-SV-START-DATE
           ELSE
           IF (CHANGE-RULES)
               MOVE WS-CHANGE-DATE     TO WS-SV-START-DATE.

           MOVE EFD-COMPANY            TO DB-COMPANY.
           MOVE EFD-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE EFD-START-DATE         TO DB-PLAN-START-DT.
           MOVE EFRSET1-PLAN-START-DT  TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-EFRSET1.
           PERFORM 860-FIND-NXTRNG-EFRSET1
               UNTIL (EMPFLEXREM-NOTFOUND)
               OR    (EFR-START-DATE   >= WS-SV-START-DATE).

           IF (EMPFLEXREM-FOUND)
      ******** This change must be done online
               SET ERROR-IN-EFD        TO TRUE
               MOVE 158                TO CRT-ERROR-NBR
P15764         MOVE "BN100"            TO CRT-ERROR-CAT
P82924*        INITIALIZE CRT-ERROR-CAT
P82924         INITIALIZE CRT-ERR-VAR1
                          CRT-ERR-VAR2
                          CRT-ERR-VAR3
                          CRT-ERR-VAR4
                          CRT-ERR-VAR5
               PERFORM 1820-CREATE-ETEC-EFDC
               THRU    1820-END.

       1892-END.

006960******************************************************************
       1900-SET-PROC-BNG-BNH-SW.
006960******************************************************************

P80056     SET NOT-SKIP-CHANGE-STOP TO TRUE.
P80056
J57497     MOVE ZEROES                 TO WS-NEW-DATE.

           IF  (BNGRPCHG-NOTFOUND)
           AND (((SELECT-COMPANY)
           AND   ((BNCHANGE-NOTFOUND)
           OR     (BNH-COMPANY         NOT = DB-COMPANY)))
           OR   ((SELECT-EMPLOYEE)
           AND   ((BNCHANGE-NOTFOUND)
           OR     (BNH-COMPANY         NOT = DB-COMPANY)
           OR     (BNH-EMPLOYEE        NOT = DB-EMPLOYEE))))
               SET END-OF-FILE         TO TRUE
               GO TO 1900-END.

           IF  (BNGRPCHG-NOTFOUND)
           AND (((SELECT-COMPANY)
           AND   (BNCHANGE-FOUND)
           AND   (BNH-COMPANY          = DB-COMPANY))
           OR   ((SELECT-EMPLOYEE)
           AND   (BNCHANGE-FOUND)
           AND   (BNH-COMPANY          = DB-COMPANY)
           AND   (BNH-EMPLOYEE         = DB-EMPLOYEE)))
               SET PROC-BNH            TO TRUE
               GO TO 1900-CONTINUE.

           IF  (((SELECT-COMPANY)
           AND   ((BNCHANGE-NOTFOUND)
           OR     (BNH-COMPANY         NOT = DB-COMPANY)))
           OR   ((SELECT-EMPLOYEE)
           AND   ((BNCHANGE-NOTFOUND)
           OR     (BNH-COMPANY         NOT = DB-COMPANY)
           OR     (BNH-EMPLOYEE        NOT = DB-EMPLOYEE))))
           AND (BNGRPCHG-FOUND)
               SET PROC-BNG            TO TRUE
               GO TO 1900-CONTINUE.

           IF (BNG-EMPLOYEE            = BNH-EMPLOYEE)
               IF (BNG-EFFECT-DATE     = BNH-EFFECT-DATE)
                   IF (BNG-ADD-DELETE  = "D")
                       SET PROC-BNG    TO TRUE
                   ELSE
                       SET PROC-BNH    TO TRUE
                   END-IF
               ELSE
               IF (BNG-EFFECT-DATE     < BNH-EFFECT-DATE)
                   SET PROC-BNG        TO TRUE
               ELSE
                   SET PROC-BNH        TO TRUE
               END-IF
               END-IF
           ELSE
           IF (BNG-EMPLOYEE            < BNH-EMPLOYEE)
               SET PROC-BNG            TO TRUE
           ELSE
               SET PROC-BNH            TO TRUE.

       1900-CONTINUE.
           IF (PROC-BNG)
J57497         MOVE BNG-EFFECT-DATE    TO WS-NEW-DATE
               MOVE BNG-COMPANY        TO WS-COMPANY
               MOVE BNG-EMPLOYEE       TO WS-PROC-EMPLOYEE
           ELSE
J57497         MOVE BNH-EFFECT-DATE    TO WS-NEW-DATE
               MOVE BNH-COMPANY        TO WS-COMPANY
               MOVE BNH-EMPLOYEE       TO WS-PROC-EMPLOYEE.

           IF (WS-EMPLOYEE             NOT = WS-PROC-EMPLOYEE)
               MOVE PRM-COMPANY        TO DB-COMPANY
               MOVE WS-PROC-EMPLOYEE   TO DB-EMPLOYEE
               PERFORM 840-FIND-EMPSET1
               PERFORM 840-FIND-PEMSET1
               INITIALIZE WS-SEQ-NBR
P74673         SET PGEMPLOYEE-KNOTFOUND   TO TRUE   
               MOVE WS-PROC-EMPLOYEE   TO WS-EMPLOYEE.

       1900-END.

006960******************************************************************
005520 1000-END.
006960******************************************************************

006960******************************************************************
006970 2000-PROCESS-BN100EFDC          SECTION 50.
006980******************************************************************
006990 2000-START.
007000
      **** Processing BN100 - Updating employee flex dollars (Change)
001400     MOVE 056                    TO CRT-MSG-NBR.
001410     PERFORM 780-DISPLAY-MSG.
001420
007130     INITIALIZE WF4-BN100EFDC-KEY.
007140     PERFORM 8500-FIND-NLT-BN100EFDC.

           IF (WS-RECORD-COUNT         > 1)
               PERFORM 2050-READ-BN100EFDC
               THRU    2050-END
                   WS-RECORD-COUNT TIMES.

011900     PERFORM 2100-UPD-WF4-COMPANY
011910     THRU    2100-END
011920         UNTIL (BN100EFDC-NOTFOUND).
011930
007380     GO TO 2000-END.
007390
011700******************************************************************
       2050-READ-BN100EFDC.
011700******************************************************************

           PERFORM 8600-FIND-NEXT-BN100EFDC.

       2050-END.

011700******************************************************************
011900 2100-UPD-WF4-COMPANY.
011700******************************************************************

           MOVE WF4-COMPANY            TO WS-COMPANY.

011900     PERFORM 2200-UPD-WF4-EMPLOYEE
011910     THRU    2200-END
011920         UNTIL (BN100EFDC-NOTFOUND)
               OR    (WF4-COMPANY      NOT = WS-COMPANY).

011910 2100-END.

011700******************************************************************
011900 2200-UPD-WF4-EMPLOYEE.
011700******************************************************************

           MOVE WF4-EMPLOYEE           TO WS-EMPLOYEE.

011900     PERFORM 2300-UPD-WF4-START-DATE
011910     THRU    2300-END
011920         UNTIL (BN100EFDC-NOTFOUND)
               OR    (WF4-COMPANY      NOT = WS-COMPANY)
               OR    (WF4-EMPLOYEE     NOT = WS-EMPLOYEE).

011910 2200-END.

011700******************************************************************
011900 2300-UPD-WF4-START-DATE.
011700******************************************************************

           ADD 1                       TO WS-RECORD-COUNT.

           IF (WF4-FUNCTION-CODE       = "S" OR "CS")
               PERFORM 2400-STOP-EFD
               THRU    2400-END
           ELSE
           IF (WF4-FUNCTION-CODE       = "D")
               PERFORM 2500-DELETE-EFD
               THRU    2500-END.

           IF (WS-UPDATE-COUNT         > WS-MAX-OPS-IN-TRAN)
               INITIALIZE WS-UPDATE-COUNT
               PERFORM 840-MODIFY-CKPSET1
               MOVE WS-RECORD-COUNT    TO WS-RESTART-COUNT
               MOVE WS-RESTART-DATA    TO CKP-RESTART-INFO
               PERFORM 820-STORE-CKPOINT
               PERFORM 925-AUDIT-END
               PERFORM 910-AUDIT-BEGIN.

       2300-NEXT-BN100EFDC.
           PERFORM 8600-FIND-NEXT-BN100EFDC.

011910 2300-END.

011700******************************************************************
       2400-STOP-EFD.
011700******************************************************************

           MOVE WF4-COMPANY            TO DB-COMPANY.
           MOVE WF4-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE WF4-START-DATE         TO DB-START-DATE.
           PERFORM 840-MODIFY-EFDSET1.

           MOVE EFD-FLEX-PLAN          TO DB-FLEX-PLAN.
           PERFORM 840-FIND-FLPSET1.

           MOVE EFD-FLD-START-DATE     TO DB-START-DATE.
           MOVE EFD-FLD-GROUP-NAME     TO DB-GROUP-NAME.
           PERFORM 840-FIND-FLDSET1.

           MOVE WF4-COMPANY            TO DB-COMPANY.
           MOVE WF4-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE WF4-START-DATE         TO DB-PLAN-START-DT.
           MOVE EFRSET1-PLAN-START-DT  TO WS-DB-BEG-RNG.
           PERFORM 850-MODIFY-BEGRNG-EFRSET1.

           PERFORM
               UNTIL (EMPFLEXREM-NOTFOUND)

               PERFORM 2410-STOP-EFR-STM
               THRU    2410-END

               PERFORM 860-MODIFY-NXTRNG-EFRSET1
           END-PERFORM.

           MOVE WF4-STOP-DATE          TO EFD-STOP-DATE.

           MOVE WS-SYSTEM-DATE-YMD     TO EFD-UPD-DATE.
           MOVE HHMMSS                 TO EFD-TIME-STAMP.
           MOVE CRT-PROGRAM-CODE       TO EFD-USER-ID.

078400     PERFORM 820-STORE-EMPFLEXDOL.

       2400-END.

011700******************************************************************
       2410-STOP-EFR-STM.
011700******************************************************************

           IF  (EFR-START-DATE             >= EFD-START-DATE)
           AND (EFR-START-DATE             < EFD-STOP-DATE)
           AND (EFR-STOP-DATE              < WF4-STOP-DATE)
               CONTINUE
           ELSE
           IF  (EFR-START-DATE             >= EFD-START-DATE)
           AND (EFR-START-DATE             <= EFD-STOP-DATE)
           AND (EFR-STOP-DATE              > WF4-STOP-DATE)
           AND (EFR-START-DATE             <= WF4-STOP-DATE)
               MOVE WF4-STOP-DATE          TO EFR-STOP-DATE

               MOVE WS-SYSTEM-DATE-YMD     TO EFR-UPD-DATE
               MOVE HHMMSS                 TO EFR-TIME-STAMP
               MOVE CRT-PROGRAM-CODE       TO EFR-USER-ID

               IF (FLD-ADD-TO-GRS-PCT      > ZEROES)
                   INITIALIZE DB-TIME-GROUP
                   MOVE FLP-FLEX-PAY-CODE  TO DB-PAY-CODE
                   MOVE EFR-STM-SEQ-NBR    TO DB-SEQ-NBR
                   PERFORM 840-MODIFY-STMSET1
                   IF (STANDTIME-FOUND)
                       MOVE WF4-STOP-DATE      TO STM-END-DATE
P54339                 MOVE WS-SYSTEM-DATE-YMD TO STM-DATE-STAMP
P54339                 MOVE HHMMSS             TO STM-TIME-STAMP
P54339                 MOVE CRT-USER-NAME      TO STM-USER-ID
                       PERFORM 820-STORE-STANDTIME
                   END-IF
               END-IF

               PERFORM 820-STORE-EMPFLEXREM
               ADD 2                       TO WS-UPDATE-COUNT
           ELSE
           IF  (EFR-START-DATE             > WF4-STOP-DATE)
           AND (EFR-START-DATE             > EFD-START-DATE)
               INITIALIZE DB-TIME-GROUP
               MOVE FLP-FLEX-PAY-CODE      TO DB-PAY-CODE
               MOVE EFR-STM-SEQ-NBR        TO DB-SEQ-NBR
               PERFORM 840-MODIFY-STMSET1
               IF (STANDTIME-FOUND)
                   PERFORM 830-DELETE-STANDTIME
                   ADD 1                   TO WS-UPDATE-COUNT
               END-IF
               PERFORM 830-DELETE-EMPFLEXREM
               ADD 1                       TO WS-UPDATE-COUNT.

       2410-END.

011700******************************************************************
       2500-DELETE-EFD.
011700******************************************************************

           MOVE WF4-COMPANY            TO DB-COMPANY.
           MOVE WF4-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE WF4-START-DATE         TO DB-START-DATE.
           PERFORM 840-MODIFY-EFDSET1.

           MOVE EFD-FLEX-PLAN          TO DB-FLEX-PLAN.
           PERFORM 840-FIND-FLPSET1.

           MOVE EFD-FLD-START-DATE     TO DB-START-DATE.
           MOVE EFD-FLD-GROUP-NAME     TO DB-GROUP-NAME.
           PERFORM 840-FIND-FLDSET1.

           MOVE WF4-COMPANY            TO DB-COMPANY.
           MOVE WF4-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE WF4-START-DATE         TO DB-PLAN-START-DT.
           MOVE EFRSET1-PLAN-START-DT  TO WS-DB-BEG-RNG.
           PERFORM 850-MODIFY-BEGRNG-EFRSET1.

           IF (FLD-ADD-TO-GRS-PCT      > ZEROES)
               INITIALIZE DB-TIME-GROUP
               MOVE FLP-FLEX-PAY-CODE  TO DB-PAY-CODE
               MOVE EFR-STM-SEQ-NBR    TO DB-SEQ-NBR
               PERFORM 840-MODIFY-STMSET1
               IF (STANDTIME-FOUND)
                   PERFORM 830-DELETE-STANDTIME
                   ADD 1               TO WS-UPDATE-COUNT.

           PERFORM 830-DELETE-EMPFLEXREM.

           PERFORM 830-DELETE-EMPFLEXDOL.

           ADD 2                       TO WS-UPDATE-COUNT.

       2500-END.

011700******************************************************************
011710 2000-END.
011720******************************************************************
011730
006960******************************************************************
006970 3000-PROCESS-BN100EFDA          SECTION 50.
006980******************************************************************
006990 3000-START.
007000
      **** Processing BN100 - Updating employee flex dollars (Add)
001400     MOVE 061                    TO CRT-MSG-NBR.
001410     PERFORM 780-DISPLAY-MSG.
001420
007130     INITIALIZE WF3-BN100EFDA-KEY.
007140     PERFORM 8500-FIND-NLT-BN100EFDA.

           IF (WS-RECORD-COUNT         > 1)
               PERFORM 3050-READ-BN100EFDA
               THRU    3050-END
                   WS-RECORD-COUNT TIMES.

011900     PERFORM 3100-UPD-WF3-COMPANY
011910     THRU    3100-END
011920         UNTIL (BN100EFDA-NOTFOUND).
011930
007380     GO TO 3000-END.
007390
011700******************************************************************
       3050-READ-BN100EFDA.
011700******************************************************************

           PERFORM 8600-FIND-NEXT-BN100EFDA.

       3050-END.

011700******************************************************************
011900 3100-UPD-WF3-COMPANY.
011700******************************************************************

           MOVE WF3-COMPANY            TO WS-COMPANY.

011900     PERFORM 3200-UPD-WF3-EMPLOYEE
011910     THRU    3200-END
011920         UNTIL (BN100EFDA-NOTFOUND)
               OR    (WF3-COMPANY      NOT = WS-COMPANY).

011910 3100-END.

011700******************************************************************
011900 3200-UPD-WF3-EMPLOYEE.
011700******************************************************************

           MOVE WF3-EMPLOYEE           TO WS-EMPLOYEE.

011900     PERFORM 3300-UPD-WF3-START-DATE
011910     THRU    3300-END
011920         UNTIL (BN100EFDA-NOTFOUND)
               OR    (WF3-COMPANY      NOT = WS-COMPANY)
               OR    (WF3-EMPLOYEE     NOT = WS-EMPLOYEE).

011910 3200-END.

011700******************************************************************
011900 3300-UPD-WF3-START-DATE.
011700******************************************************************

           ADD 1                       TO WS-RECORD-COUNT.

           IF (WF3-FUNCTION-CODE       NOT = "A" AND "CA")
               GO TO 3300-NEXT-BN100EFDA.

           PERFORM 3400-ADD-EFD
           THRU    3400-END.

           IF (WS-UPDATE-COUNT         > WS-MAX-OPS-IN-TRAN)
               INITIALIZE WS-UPDATE-COUNT
               PERFORM 840-MODIFY-CKPSET1
               MOVE WS-RECORD-COUNT    TO WS-RESTART-COUNT
               MOVE WS-RESTART-DATA    TO CKP-RESTART-INFO
               PERFORM 820-STORE-CKPOINT
               PERFORM 925-AUDIT-END
               PERFORM 910-AUDIT-BEGIN.

       3300-NEXT-BN100EFDA.
           PERFORM 8600-FIND-NEXT-BN100EFDA.

011910 3300-END.

011700******************************************************************
       3400-ADD-EFD.
011700******************************************************************

J59685     SET NO-ERROR-FOUND              TO TRUE.
J59685
           MOVE WF3-EMPLOYEE               TO DB-EMPLOYEE.
           PERFORM 840-FIND-EMPSET1.
           PERFORM 840-FIND-PEMSET1.

           MOVE WF3-COMPANY                TO DB-COMPANY.
           MOVE WF3-FLEX-PLAN              TO DB-FLEX-PLAN.
           PERFORM 840-FIND-FLPSET1.

           MOVE WF3-FLD-GROUP-NAME         TO DB-GROUP-NAME.
           MOVE WF3-FLD-START-DATE         TO DB-START-DATE.
           PERFORM 840-FIND-FLDSET1.

           MOVE WF3-COMPANY                TO DB-COMPANY.
           MOVE WF3-START-DATE             TO DB-START-DATE.
           MOVE WF3-EMPLOYEE               TO DB-EMPLOYEE.
           PERFORM 840-FIND-EFDSET1.
           IF (EMPFLEXDOL-FOUND)
               MOVE WF3-COMPANY            TO DB-COMPANY
               MOVE WF3-START-DATE         TO DB-START-DATE
               MOVE WF3-EMPLOYEE           TO DB-EMPLOYEE
               PERFORM 840-MODIFY-EFDSET1
           ELSE
               PERFORM 800-CREATE-EMPFLEXDOL
               MOVE WS-SYSTEM-DATE-YMD     TO EFD-CREATION-DATE
J67329         MOVE HHMMSS                 TO EFD-CREATE-TIME
J67329         MOVE CRT-PROGRAM-CODE       TO EFD-CREATE-USER-ID               
               MOVE EMP-CURRENCY-CODE      TO EFD-CURRENCY-CODE
               MOVE EMP-CURR-ND            TO EFD-CURR-ND.

           MOVE WF3-COMPANY                TO DB-COMPANY.
           MOVE WF3-START-DATE             TO DB-START-DATE
                                              DB-PLAN-START-DT.
           MOVE WF3-EMPLOYEE               TO DB-EMPLOYEE.
           PERFORM 840-FIND-EFRSET1.
           IF (EMPFLEXREM-FOUND)
               MOVE WF3-COMPANY            TO DB-COMPANY
               MOVE WF3-START-DATE         TO DB-START-DATE
                                              DB-PLAN-START-DT
               MOVE WF3-EMPLOYEE           TO DB-EMPLOYEE
               PERFORM 840-MODIFY-EFRSET1     
           ELSE
               PERFORM 800-CREATE-EMPFLEXREM
               MOVE WS-SYSTEM-DATE-YMD     TO EFR-CREATION-DATE
J67329         MOVE HHMMSS                 TO EFR-CREATE-TIME
J67329         MOVE CRT-PROGRAM-CODE       TO EFR-CREATE-USER-ID               
               MOVE EMP-CURRENCY-CODE      TO EFR-CURRENCY-CODE
               MOVE EMP-CURR-ND            TO EFR-CURR-ND.

           IF (WF3-RETRO-CHANGE            = "Y")
               MOVE WF3-FLD-START-DATE     TO BNWS-PROC-DATE.

075100     MOVE WF3-COMPANY                TO EFD-COMPANY.
075200     MOVE WF3-START-DATE             TO EFD-START-DATE.
075600     MOVE WF3-EMPLOYEE               TO EFD-EMPLOYEE.
075500     MOVE WF3-STOP-DATE              TO EFD-STOP-DATE.

           INITIALIZE EFD-COMP-SPENT
                      EFD-EMP-SPENT.

075300     MOVE WF3-FLEX-PLAN              TO EFD-FLEX-PLAN.

           PERFORM 3420-COMP-SAL-FTE-ANN-HR
           THRU    3420-END.

075900     MOVE WS-SALARY                  TO EFD-SALARY.
075700     MOVE WS-ANNUAL-HOURS            TO EFD-ANNUAL-HOURS.
075800     MOVE WS-NBR-FTE                 TO EFD-NBR-FTE.

           MOVE WF3-COMPANY                TO BNFDWS-COMPANY.
           MOVE WF3-FLEX-PLAN              TO BNFDWS-FLEX-PLAN.
           MOVE WF3-EMPLOYEE               TO BNFDWS-EMPLOYEE.
           MOVE WF3-START-DATE             TO BNFDWS-AS-OF-DATE
                                              BNFDWS-START-DATE.
           MOVE WF3-STOP-DATE              TO BNFDWS-STOP-DATE.
           MOVE WS-SALARY                  TO BNFDWS-IN-ANNUAL-SALARY.
           PERFORM 5000-DO-FLEX-DOLLAR-70.

066650     MOVE BNFDWS-BASE-DOLLARS        TO EFD-COMP-FLX-AMT.
076200     MOVE BNFDWS-SERV-CREDITS        TO EFD-SERV-CREDIT.
076300     MOVE BNFDWS-AGE-CREDITS         TO EFD-AGE-CREDIT.
076400     MOVE BNFDWS-DEP-CREDITS         TO EFD-DEP-CREDIT.
076400     MOVE BNFDWS-LIFE-CREDITS        TO EFD-LIFE-CREDIT.

           INITIALIZE EFD-VAC-CREDIT.

066600     MOVE BNFDWS-PT-DOLLARS          TO EFD-EMP-FLEX-AMT.
           MOVE BNFDWS-FLEX-CREDITS        TO EFD-TOT-COMP-AMT.

           INITIALIZE EFD-UPDATE-FLAG.

           MOVE WF3-FLD-GROUP-NAME         TO EFD-FLD-GROUP-NAME.
           MOVE WF3-FLD-START-DATE         TO EFD-FLD-START-DATE.

           MOVE BNASWS-CALC-DATE           TO EFD-SAL-CALC-DATE.
           MOVE BNFDWS-AGE-CALC-DATE       TO EFD-AGE-CALC-DATE.
           MOVE BNFDWS-SERV-CALC-DATE      TO EFD-SERV-CALC-DATE.

           MOVE WS-SYSTEM-DATE-YMD         TO EFD-UPD-DATE.
           MOVE HHMMSS                     TO EFD-TIME-STAMP.
           MOVE CRT-PROGRAM-CODE           TO EFD-USER-ID.

021500     MOVE FLP-DED-TABLE              TO DB-TABLE-CODE.
021600     PERFORM 840-FIND-DFTSET1.

           INITIALIZE PLN-DED-TABLE.

076700     MOVE "Y"                        TO BNWS-FLEX-FLAG.
J90269     INITIALIZE BNWS-COVER-OPT.
076800     PERFORM 430-MOVE-DEDFREQTBL-TO-WS.
076900
077000     MOVE EFD-COMPANY                TO BNWS-COMPANY.
077100     MOVE EFD-EMPLOYEE               TO BNWS-EMPLOYEE.
077600
           PERFORM 3440-MOVE-EFR-DATA
           THRU    3440-END.
078300
           PERFORM 820-STORE-EMPFLEXREM.

078400     PERFORM 820-STORE-EMPFLEXDOL.
078500
           INITIALIZE BNWS-PROC-DATE.

           ADD 2                           TO WS-UPDATE-COUNT.

       3400-END.

096300******************************************************************
096400 3420-COMP-SAL-FTE-ANN-HR.
096500******************************************************************
096600
           MOVE WF3-COMPANY                TO BNASWS-COMPANY.
           MOVE WF3-EMPLOYEE               TO BNASWS-EMPLOYEE.
           MOVE FLD-SALARY-TYPE            TO BNASWS-SALARY-TYPE.
           MOVE FLD-SAL-FIRST-MO           TO BNASWS-DATE-TYPE.
           MOVE WF3-START-DATE             TO BNASWS-START-DATE.
           MOVE WF3-STOP-DATE              TO BNASWS-STOP-DATE.
           MOVE FLD-SALARY-DATE            TO BNASWS-AS-OF-MMDD.
           MOVE FLD-SALARY-YEAR            TO BNASWS-AS-OF-YYYY.
           PERFORM 5000-DO-ANNUAL-SALARY-71.

           MOVE "Y"                        TO BNMRWS-ROUND-ONLY-SW.
           MOVE BNASWS-ANNUAL-SALARY       TO BNMRWS-AMOUNT.
           INITIALIZE BNMRWS-BEF-AFT-FLAG.
           MOVE FLD-SAL-ROUND-METH         TO BNMRWS-ROUND-METH.
           MOVE FLD-SAL-ROUND-TO           TO BNMRWS-ROUND-TO.
           INITIALIZE BNMRWS-MULT-OF-SALARY
                      BNMRWS-SALARY-PCT.
           PERFORM 5000-DO-MULTIPLY-AND-ROUND-70.
           MOVE BNMRWS-AMT-ROUNDED         TO WS-SALARY.

           IF (FLD-SALARY-DATE             NOT = ZEROES)
               MOVE FLD-SALARY-DATE        TO WS-FLD-MMDD
               IF (FLD-SALARY-YEAR         NOT = ZEROES)
                   MOVE FLD-SALARY-YEAR    TO WS-FLD-YEAR
               ELSE
                   MOVE WF3-START-DATE     TO WS-SYS-DATE
                   MOVE WS-SYS-YEAR        TO WS-FLD-YEAR
                   IF (WS-FLD-MMDD         > WS-SYS-MMDD)
                       SUBTRACT 1          FROM WS-FLD-YEAR.

041700     IF (FLD-SALARY-DATE             < WS-SYSTEM-DATE-YMD)
               MOVE EMP-COMPANY            TO HRHRHWS-COMPANY
               MOVE EMP-EMPLOYEE           TO HRHRHWS-EMPLOYEE
               INITIALIZE HRHRHWS-OBJ-ID
041800         MOVE 19                     TO HRHRHWS-FLD-NBR
               IF (FLD-SALARY-DATE         NOT = ZEROES)
041900             MOVE WS-FLD-DATE        TO HRHRHWS-DATE
               ELSE
                   MOVE WF3-START-DATE     TO HRHRHWS-DATE
               END-IF
               INITIALIZE HRHRHWS-FIRST-VALUE
               PERFORM 5000-GET-HRHISTORY-VALUE-70
               IF (HRHRHWS-REC-FOUND       = "Y")
                   MOVE HRHRHWS-A-VALUE    TO DB-JOB-CODE
               END-IF
               MOVE 475                    TO HRHRHWS-FLD-NBR
               INITIALIZE HRHRHWS-FIRST-VALUE
               PERFORM 5000-GET-HRHISTORY-VALUE-70
               IF (HRHRHWS-REC-FOUND       = "Y")
                   MOVE HRHRHWS-N-VALUE    TO WS-NBR-FTE.
      *
      **** GET ANNUAL-HOURS FROM LOGFILE
      *
           INITIALIZE WS-ANNUAL-HOURS.
           MOVE EMP-COMPANY                TO HRHRHWS-COMPANY.
           MOVE EMP-EMPLOYEE               TO HRHRHWS-EMPLOYEE.
           MOVE ZEROS                      TO HRHRHWS-OBJ-ID.
           MOVE BNBEN-HRH-ANNUAL-HOURS-DN  TO HRHRHWS-FLD-NBR.
           MOVE BNCTWS-START-DATE          TO HRHRHWS-DATE.
           INITIALIZE HRHRHWS-FIRST-VALUE.
           PERFORM 5000-GET-HRHISTORY-VALUE-70.
           IF (HRHRHWS-REC-FOUND           = "Y")
               MOVE HRHRHWS-N-VALUE        TO WS-ANNUAL-HOURS.

           IF (WS-ANNUAL-HOURS             = ZEROES)
               MOVE EMP-ANNUAL-HOURS       TO WS-ANNUAL-HOURS.

           IF (WS-ANNUAL-HOURS             = ZEROES)
           AND (DB-JOB-CODE            NOT = SPACES)
               MOVE EMP-COMPANY            TO DB-COMPANY
               PERFORM 840-FIND-JBCSET1
               IF (JOBCODE-FOUND)
                   MOVE JBC-ANNUAL-HOURS   TO WS-ANNUAL-HOURS
               ELSE
                   GO TO 3420-PRS-HOURS
               END-IF
041700         IF (FLD-SALARY-DATE         < WS-SYSTEM-DATE-YMD)
                   MOVE EMP-COMPANY        TO HRHRHWS-COMPANY
                   INITIALIZE HRHRHWS-EMPLOYEE
                   MOVE JBC-OBJ-ID         TO HRHRHWS-OBJ-ID
                   MOVE BNBEN-JBC-ANNUAL-HOURS-DN 
                                           TO HRHRHWS-FLD-NBR
                   IF (FLD-SALARY-DATE     NOT = ZEROES)
041900                 MOVE WS-FLD-DATE        TO HRHRHWS-DATE
                   ELSE
                       MOVE WF3-START-DATE     TO HRHRHWS-DATE
                   END-IF
                   INITIALIZE HRHRHWS-FIRST-VALUE
                   PERFORM 5000-GET-HRHISTORY-VALUE-70
                   IF (HRHRHWS-REC-FOUND       = "Y")
                       MOVE HRHRHWS-N-VALUE    TO WS-ANNUAL-HOURS.

       3420-PRS-HOURS.   
           IF (WS-ANNUAL-HOURS             = ZEROES)
               MOVE EMP-COMPANY            TO DB-COMPANY
               INITIALIZE DB-PROCESS-LEVEL
               PERFORM 840-FIND-PRSSET1
               MOVE PRS-ANNUAL-HOURS       TO WS-ANNUAL-HOURS.

           IF (WS-ANNUAL-HOURS             = ZEROES)
               MOVE 2080                   TO WS-ANNUAL-HOURS.

           IF (WS-NBR-FTE                  = ZEROES)
               MOVE 1                      TO WS-NBR-FTE.

       3420-END.

096300******************************************************************
096400 3440-MOVE-EFR-DATA.
096500******************************************************************
096600
098600     MOVE WF3-COMPANY                TO EFR-COMPANY.
098800     MOVE WF3-EMPLOYEE               TO EFR-EMPLOYEE.
           MOVE WF3-START-DATE             TO EFR-PLAN-START-DT
099000                                        EFR-START-DATE.
099100     MOVE WF3-STOP-DATE              TO EFR-STOP-DATE.

           MOVE BNFDWS-FLEX-CREDITS        TO EFR-CREDITS-AVAIL.

           MOVE WF3-CREDITS-SPENT          TO EFR-CREDITS-SPENT.

066600     MOVE BNFDWS-PT-DOLLARS          TO EFR-PRE-TAX-AVAIL.

           MOVE WS-SYSTEM-DATE-YMD         TO EFR-UPD-DATE.
           MOVE HHMMSS                     TO EFR-TIME-STAMP.
           MOVE CRT-PROGRAM-CODE           TO EFR-USER-ID.

           IF (FLD-ADD-TO-GRS-PCT          > ZEROES)
102100         PERFORM 3442-UPDATE-STANDTIME
102200         THRU    3442-END
               MOVE BNSTM-SAVE-SEQ-NBR     TO EFR-STM-SEQ-NBR. 
101600
       3440-END.

104700******************************************************************
104800 3442-UPDATE-STANDTIME.
104900******************************************************************
105000
           MOVE "A"                    TO BNSTM-FC.

           MOVE EFR-COMPANY            TO BNSTM-COMPANY.
           MOVE EFR-EMPLOYEE           TO BNSTM-EMPLOYEE.

           INITIALIZE BNSTM-PLN-PAY-CODE.

           MOVE FLP-FLEX-PAY-CODE      TO BNSTM-FLP-PAY-CODE.

           MOVE EFR-CREDITS-AVAIL      TO BNSTM-AMOUNT.

           MOVE BNWS-TOT-CYC           TO BNSTM-DIVISOR.

           MOVE FLD-ADD-TO-GRS-PCT     TO BNSTM-ADD-TO-GRS-PCT.

108400     MOVE EFR-START-DATE         TO BNSTM-EFFECT-DATE.
108500     MOVE EFR-STOP-DATE          TO BNSTM-END-DATE.
           MOVE EFR-STM-SEQ-NBR        TO BNSTM-SEQ-NBR.

           PERFORM 700-UPDATE-STANDARD-TIME.
109300
109400 3442-END.
109500
011700******************************************************************
011710 3000-END.
011720******************************************************************
011730
006960******************************************************************
006970 4000-PROCESS-BN100BENC          SECTION 50.
006980******************************************************************
006990 4000-START.
007000
      **** Processing BN100 - Updating benefits (Change)
001400     MOVE 057                    TO CRT-MSG-NBR.
001410     PERFORM 780-DISPLAY-MSG.
001420
           INITIALIZE WF2-BN100BENC-KEY.
007140     PERFORM 8500-FIND-NLT-BN100BENC.

           IF (WS-RECORD-COUNT         > 1)
               IF (WS-FILE-NAME        = SPACES)
      ************ IF WE BLOW UP IN ADD PHASE WE NEED TO READ EXACT NO OF 
      ************ RECORDS IN BN100BENC
                   COMPUTE WS-I1       = WS-RECORD-COUNT
                                       - 1
                   PERFORM 4050-READ-BN100BENC
                   THRU    4050-END
                       WS-I1 TIMES
               ELSE
                   PERFORM 4050-READ-BN100BENC
                   THRU    4050-END
                       WS-RECORD-COUNT TIMES.

011900     PERFORM 4100-UPD-WF2-COMPANY
011910     THRU    4100-END
011920         UNTIL (BN100BENC-NOTFOUND).
011930
007380     GO TO 4000-END.
007390
011700******************************************************************
       4050-READ-BN100BENC.
011700******************************************************************

           PERFORM 8600-FIND-NEXT-BN100BENC.

       4050-END.

011700******************************************************************
011900 4100-UPD-WF2-COMPANY.
011700******************************************************************

           MOVE WF2-COMPANY            TO WS-COMPANY.

011900     PERFORM 4200-UPD-WF2-EMPLOYEE
011910     THRU    4200-END
011920         UNTIL (BN100BENC-NOTFOUND)
               OR    (WF2-COMPANY      NOT = WS-COMPANY).

011910 4100-END.

011700******************************************************************
011900 4200-UPD-WF2-EMPLOYEE.
011700******************************************************************

           MOVE WF2-EMPLOYEE           TO WS-EMPLOYEE.

           INITIALIZE WS-EFR-TABLE
                      WS-EFR-TABLE-1
                      WS-TOTAL-AVAIL
                      I5
                      WS-FLEX-STOP-TABLE.

           IF (WS-FILE-NAME            = SPACES)
               PERFORM 4210-SAVE-BN100BENC-KEY
               THRU    4210-END

               PERFORM 4220-SET-FLEX-STOP-TABLE
               THRU    4220-END
                   UNTIL (BN100BENC-NOTFOUND)
                   OR    (WF2-COMPANY  NOT = WS-COMPANY)
                   OR    (WF2-EMPLOYEE NOT = WS-EMPLOYEE)

               IF (I1                  > ZEROES)
                   PERFORM 4230-SORT-TABLE-BY-STOP
                   THRU    4230-END

                   PERFORM 4240-STOP-FLEX-BENEFITS
                   THRU    4240-END
               END-IF

               PERFORM 4250-RESET-BN100BENC-REC
               THRU    4250-END

011900         PERFORM 4300-UPD-WF2-PLAN-TYPE
011910         THRU    4300-END
011920             UNTIL (BN100BENC-NOTFOUND)
                   OR    (WF2-COMPANY  NOT = WS-COMPANY)
                   OR    (WF2-EMPLOYEE NOT = WS-EMPLOYEE)

               PERFORM 840-MODIFY-CKPSET1
               MOVE WS-RECORD-COUNT    TO WS-RESTART-COUNT
               MOVE WS-BENA-FILE-NAME  TO WS-REST-FILE-NAME
               INITIALIZE WS-REST-EMP-COUNT
               MOVE WS-RESTART-DATA    TO CKP-RESTART-INFO
               PERFORM 820-STORE-CKPOINT
               PERFORM 925-AUDIT-END
               PERFORM 910-AUDIT-BEGIN.

           MOVE WF2-COMPANY            TO WS-SAV-COMPANY.
           MOVE WF2-EMPLOYEE           TO WS-SV-EMPLOYEE.
           MOVE WF2-PLAN-TYPE          TO WS-SV-PLAN-TYPE.
           MOVE WF2-PLAN-CODE          TO WS-SV-PLAN-CODE.
           MOVE WF2-START-DATE         TO WS-SV-START-DATE.
           MOVE WS-BN100BENC-SW        TO WS-SV-BENC-FOUND-SW.

           PERFORM 4500-PROCESS-BN100BENA.

           MOVE WS-SAV-COMPANY          TO WF2-COMPANY.
           MOVE WS-SV-EMPLOYEE         TO WF2-EMPLOYEE.
           MOVE WS-SV-PLAN-TYPE        TO WF2-PLAN-TYPE.
           MOVE WS-SV-PLAN-CODE        TO WF2-PLAN-CODE.
           MOVE WS-SV-START-DATE       TO WF2-START-DATE.
           PERFORM 8400-FIND-BN100BENC.
           MOVE WS-SV-BENC-FOUND-SW    TO WS-BN100BENC-SW.

011910 4200-END.

011700******************************************************************
       4210-SAVE-BN100BENC-KEY.
011700******************************************************************

           MOVE WF2-COMPANY            TO WS-SAVE-COMPANY.
           MOVE WF2-EMPLOYEE           TO WS-SAVE-EMPLOYEE.
           MOVE WF2-PLAN-TYPE          TO WS-SAVE-PLAN-TYPE.
           MOVE WF2-PLAN-CODE          TO WS-SAVE-PLAN-CODE.
           MOVE WF2-START-DATE         TO WS-SAVE-START-DATE.

           INITIALIZE I1.

       4210-END.

011700******************************************************************
       4220-SET-FLEX-STOP-TABLE.
011700******************************************************************

           IF (WF2-FLEX-PLAN           = SPACES)
               GO TO 4220-NEXT-BN100BENC.

           IF (WF2-FUNCTION-CODE       NOT = "S" AND "CS" AND "CC")
               GO TO 4220-NEXT-BN100BENC.

           MOVE WF2-COMPANY            TO DB-COMPANY.
           MOVE WF2-FLEX-PLAN          TO DB-FLEX-PLAN.
           PERFORM 840-FIND-FLPSET1.
           IF (FLP-SPEND-ONLY          = "Y")
               GO TO 4220-NEXT-BN100BENC.

           ADD 1                       TO I1.

           MOVE WF2-COMPANY            TO WS-FST-COMPANY (I1).
           MOVE WF2-EMPLOYEE           TO WS-FST-EMPLOYEE (I1).
           MOVE WF2-PLAN-TYPE          TO WS-FST-PLAN-TYPE (I1).
           MOVE WF2-PLAN-CODE          TO WS-FST-PLAN-CODE (I1).
           MOVE WF2-START-DATE         TO WS-FST-START-DATE (I1).
           MOVE WF2-STOP-DATE          TO WS-FST-STOP-DATE (I1).

       4220-NEXT-BN100BENC.
           PERFORM 8600-FIND-NEXT-BN100BENC.

       4220-END.

011700******************************************************************
       4230-SORT-TABLE-BY-STOP.
011700******************************************************************

           PERFORM
               VARYING I8 FROM 1 BY 1
               UNTIL  (I8 > 100)
               OR     (I8 > I1)

               PERFORM
                   VARYING I9 FROM I8 BY 1
                   UNTIL  (I9 > 100)
                   OR     (I9 > I1)

                   IF (WS-FST-STOP-DATE (I9) > WS-FST-STOP-DATE (I8))
                       PERFORM 4232-SWAP-TABLE-DATA
                       THRU    4232-END
                   END-IF
               END-PERFORM
           END-PERFORM.

       4230-END.

011700******************************************************************
       4232-SWAP-TABLE-DATA.
011700******************************************************************

           MOVE WS-FST-COMPANY (I8)    TO WS-SWAP-COMPANY.
           MOVE WS-FST-EMPLOYEE (I8)   TO WS-SWAP-EMPLOYEE.
           MOVE WS-FST-PLAN-TYPE (I8)  TO WS-SWAP-PLAN-TYPE.
           MOVE WS-FST-PLAN-CODE (I8)  TO WS-SWAP-PLAN-CODE.
           MOVE WS-FST-START-DATE (I8) TO WS-SWAP-START-DATE.
           MOVE WS-FST-STOP-DATE (I8)  TO WS-SWAP-STOP-DATE.

           MOVE WS-FST-COMPANY (I9)    TO WS-FST-COMPANY (I8).
           MOVE WS-FST-EMPLOYEE (I9)   TO WS-FST-EMPLOYEE (I8).
           MOVE WS-FST-PLAN-TYPE (I9)  TO WS-FST-PLAN-TYPE (I8).
           MOVE WS-FST-PLAN-CODE (I9)  TO WS-FST-PLAN-CODE (I8).
           MOVE WS-FST-START-DATE (I9) TO WS-FST-START-DATE (I8).
           MOVE WS-FST-STOP-DATE (I9)  TO WS-FST-STOP-DATE (I8).

           MOVE WS-SWAP-COMPANY        TO WS-FST-COMPANY (I9).
           MOVE WS-SWAP-EMPLOYEE       TO WS-FST-EMPLOYEE (I9).
           MOVE WS-SWAP-PLAN-TYPE      TO WS-FST-PLAN-TYPE (I9).
           MOVE WS-SWAP-PLAN-CODE      TO WS-FST-PLAN-CODE (I9).
           MOVE WS-SWAP-START-DATE     TO WS-FST-START-DATE (I9).
           MOVE WS-SWAP-STOP-DATE      TO WS-FST-STOP-DATE (I9).

       4232-END.

011700******************************************************************
       4240-STOP-FLEX-BENEFITS.
011700******************************************************************

           PERFORM
               VARYING WS-I2 FROM 1 BY 1
               UNTIL  (WS-I2 > 100)
               OR     (WS-FST-COMPANY (WS-I2) = ZEROES)

               MOVE WS-FST-COMPANY (WS-I2)    TO WF2-COMPANY
               MOVE WS-FST-EMPLOYEE (WS-I2)   TO WF2-EMPLOYEE
               MOVE WS-FST-PLAN-TYPE (WS-I2)  TO WF2-PLAN-TYPE
               MOVE WS-FST-PLAN-CODE (WS-I2)  TO WF2-PLAN-CODE
               MOVE WS-FST-START-DATE (WS-I2) TO WF2-START-DATE
               PERFORM 8400-FIND-BN100BENC

               PERFORM 4300-UPD-WF2-PLAN-TYPE
               THRU    4300-END

           END-PERFORM.

       4240-END.

011700******************************************************************
       4250-RESET-BN100BENC-REC.
011700******************************************************************

           MOVE WS-SAVE-COMPANY        TO WF2-COMPANY.
           MOVE WS-SAVE-EMPLOYEE       TO WF2-EMPLOYEE.
           MOVE WS-SAVE-PLAN-TYPE      TO WF2-PLAN-TYPE.
           MOVE WS-SAVE-PLAN-CODE      TO WF2-PLAN-CODE.
           MOVE WS-SAVE-START-DATE     TO WF2-START-DATE.
           PERFORM 8400-FIND-BN100BENC.

       4250-END.

011700******************************************************************
011900 4300-UPD-WF2-PLAN-TYPE.
011700******************************************************************

           ADD 1                       TO WS-RECORD-COUNT.

           IF  (WF2-FUNCTION-CODE      NOT = "S" AND "CS" AND "D")
           AND (WF2-FUNCTION-CODE      NOT = "C" AND "CC")
               GO TO 4300-NEXT-BN100BENC.

           MOVE WF2-COMPANY            TO DB-COMPANY.
           MOVE WF2-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE WF2-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE WF2-PLAN-CODE          TO DB-PLAN-CODE.
           MOVE WF2-START-DATE         TO DB-START-DATE.

           PERFORM 840-FIND-PLNSET1.

           PERFORM 840-FIND-BENSET1.

           IF (PLN-FLEX-PLAN           NOT = SPACES)
               MOVE PLN-FLEX-PLAN      TO DB-FLEX-PLAN
               PERFORM 840-FIND-FLPSET1
               IF  (FLP-SPEND-ONLY     = "N")
               AND (WF2-FUNCTION-CODE  = "S" OR "CS" OR "CC")
               AND (BEN-STOP-DATE      = WF2-STOP-DATE)
                   SUBTRACT 1          FROM WS-RECORD-COUNT
                   GO TO 4300-NEXT-BN100BENC.

           MOVE BEN-EFD-START-DATE     TO WS-START-DATE.
           MOVE "C"                    TO WS-FUNCTION-CODE.

           PERFORM 4400-MOVE-TO-BNBEN
           THRU    4400-END.

           PERFORM 2000-BNBEN-EDIT-TRAN.
           IF (ERROR-FOUND)
               PERFORM 4310-REWRITE-EC-BENC
               THRU    4310-END
               INITIALIZE CRT-ERROR-NBR
                          CRT-ERROR-CAT
                          CRT-ERR-VAR1
                          CRT-ERR-VAR2
                          CRT-ERR-VAR3
                          CRT-ERR-VAR4
                          CRT-ERR-VAR5
               GO TO 4300-NEXT-BN100BENC
           ELSE
ACS003*        PERFORM 4000-BNBEN-PROCESS-TRAN.
ACS003         SET HRDEPBEN-NOT-DELETED    TO TRUE 
ACS003         SET BENEFIT-NOT-DELETED     TO TRUE
ACS003         PERFORM 4000-BNBEN-PROCESS-TRAN 
ACS003         IF (PRM-UPDATE-OPTION       = "U")      
ACS003             IF ((HRDEPBEN-DELETED) OR (BENEFIT-DELETED)) 
ACS003                 SET A-WF2-PLAN-TYPE     TO TRUE
ACS003                 PERFORM 8900-WRITE-ZN325WK1-REC. 
ACS003

           IF (WF2-FUNCTION-CODE       = "CC")
               MOVE WF2-STOP-DATE      TO BNBEN-STOP-DATE (1)
               MOVE "S"                TO BNBEN-LINE-FC (1)

               PERFORM 2000-BNBEN-EDIT-TRAN
               IF (ERROR-FOUND)
                   PERFORM 4310-REWRITE-EC-BENC
                   THRU    4310-END
                   INITIALIZE CRT-ERROR-NBR
                              CRT-ERROR-CAT
                              CRT-ERR-VAR1
                              CRT-ERR-VAR2
                              CRT-ERR-VAR3
                              CRT-ERR-VAR4
                              CRT-ERR-VAR5
                   GO TO 4300-NEXT-BN100BENC
               ELSE
ACS003*            PERFORM 4000-BNBEN-PROCESS-TRAN.
ACS003             SET HRDEPBEN-NOT-DELETED    TO TRUE 
ACS003             SET BENEFIT-NOT-DELETED     TO TRUE
ACS003             PERFORM 4000-BNBEN-PROCESS-TRAN 
ACS003             IF (PRM-UPDATE-OPTION       = "U")      
ACS003                 IF ((HRDEPBEN-DELETED) OR (BENEFIT-DELETED)) 
ACS003                     SET A-WF2-PLAN-TYPE     TO TRUE
ACS003                     PERFORM 8900-WRITE-ZN325WK1-REC. 
ACS003

           ADD 5                       TO WS-UPDATE-COUNT.

           IF  (WF2-COBRA-OCC          = "Y")
           AND (WF2-PLAN-TYPE          = "HL" OR "DN" OR "EL" OR "RS")
               PERFORM 4420-CREATE-PARTICIPNT
               THRU    4420-END.

           IF (WS-UPDATE-COUNT         > WS-MAX-OPS-IN-TRAN)
               INITIALIZE WS-UPDATE-COUNT
               PERFORM 840-MODIFY-CKPSET1
               MOVE WS-RECORD-COUNT    TO WS-RESTART-COUNT
               INITIALIZE WS-REST-FILE-NAME
                          WS-REST-EMP-COUNT
               MOVE WS-RESTART-DATA    TO CKP-RESTART-INFO
               PERFORM 820-STORE-CKPOINT
               PERFORM 925-AUDIT-END
               PERFORM 910-AUDIT-BEGIN.

       4300-NEXT-BN100BENC.
           PERFORM 8600-FIND-NEXT-BN100BENC.

       4300-END.

011700******************************************************************
       4310-REWRITE-EC-BENC.
011700******************************************************************

      **** IF "CS" RECORD IS IN ERROR, WE NEED TO FIND OUT "CA" RECORD ****
      **** BY ADDING ONE TO STOP DATE OF "CS" RECORD AND CHANGE ITS    ****
      **** FUNCTION CODE TO "EA"                                       ****

           IF (WF2-FUNCTION-CODE       NOT = "CS")
               GO TO 4310-CONTINUE.

           MOVE WF2-COMPANY            TO WF1-COMPANY.
           MOVE WF2-EMPLOYEE           TO WF1-EMPLOYEE.
           MOVE WF2-PLAN-TYPE          TO WF1-PLAN-TYPE.
           MOVE WF2-PLAN-CODE          TO WF1-PLAN-CODE.

           MOVE WF2-STOP-DATE          TO WSDR-FR-DATE.
           PERFORM 900-DATE-TO-JULIAN.
           ADD 1                       TO WSDR-JULIAN-DAYS.
           PERFORM 900-JULIAN-TO-DATE.
           MOVE WSDR-FR-DATE           TO WF1-START-DATE.

           PERFORM 8400-FIND-BN100BENA.
           IF (BN100BENA-NOTFOUND)
           OR (WF1-FUNCTION-CODE       = "EA")
               GO TO 4310-CONTINUE.

           MOVE "EA"                   TO WF1-FUNCTION-CODE.

           PERFORM 8200-REWRITE-BN100BENA.

           MOVE WF1-COMPANY            TO WF-COMPANY.
           MOVE WF1-EMPLOYEE           TO WF-EMPLOYEE.
           MOVE WF1-WF-EFFECT-DATE     TO WF-EFFECT-DATE.
           MOVE "3"                    TO WF-REC-TYPE.
           MOVE WF1-WF-SEQ-NBR         TO WF-SEQ-NBR.
           PERFORM 8400-FIND-BN100WORK.

           MOVE "EA"                   TO WF-FUNCTION-CODE.
           MOVE CRT-ERROR-NBR          TO WF-ERROR-NBR.
P09149     MOVE WS-TRUE                TO WS-CO-ERR-SW.
           MOVE CRT-ERROR-CAT          TO WF-ERROR-CAT.
           MOVE CRT-ERR-VAR1           TO WF-ERR-VAR1.

           PERFORM 8200-REWRITE-BN100WORK.

       4310-CONTINUE.
           IF (WF2-FUNCTION-CODE       = "S" OR "CS")
               MOVE "ET"               TO WF2-FUNCTION-CODE
           ELSE
           IF (WF2-FUNCTION-CODE       = "C" OR "CC")
               MOVE "EC"               TO WF2-FUNCTION-CODE
           ELSE
               MOVE "ED"               TO WF2-FUNCTION-CODE.

           PERFORM 8200-REWRITE-BN100BENC.

           MOVE WF2-COMPANY            TO WF-COMPANY.
           MOVE WF2-EMPLOYEE           TO WF-EMPLOYEE.
           MOVE WF2-WF-EFFECT-DATE     TO WF-EFFECT-DATE.
           MOVE "3"                    TO WF-REC-TYPE.
           MOVE WF2-WF-SEQ-NBR         TO WF-SEQ-NBR.
           PERFORM 8400-FIND-BN100WORK.

           MOVE WF2-FUNCTION-CODE      TO WF-FUNCTION-CODE.
           MOVE CRT-ERROR-NBR          TO WF-ERROR-NBR.
P09149     MOVE WS-TRUE                TO WS-CO-ERR-SW.
           MOVE CRT-ERROR-CAT          TO WF-ERROR-CAT.
           MOVE CRT-ERR-VAR1           TO WF-ERR-VAR1.

           PERFORM 8200-REWRITE-BN100WORK.

       4310-END.

011700******************************************************************
       4400-MOVE-TO-BNBEN.
011700******************************************************************

           IF (BNBEN-SV-SP-END-DT (1,1) NOT = ZEROES)
               INITIALIZE BN100WS-SV-SP-TBL
               MOVE BNBEN-SV-SP-TBL (1)  TO BN100WS-SV-SP-TBL.

002000     INITIALIZE BNBEN-DETAIL-GROUP
002000                BNBEN-USE-NAVIGATE-SW.

           IF (BN100WS-SV-SP-END-DT (1)  NOT = ZEROES)
               MOVE BN100WS-SV-SP-TBL  TO BNBEN-SV-SP-TBL (1)
               INITIALIZE BN100WS-SV-SP-TBL
               INITIALIZE I4.

           MOVE BEN-PREM-UPD-DT        TO DB-START-DATE.
           MOVE BEN-PREM-GROUP         TO DB-GROUP-NAME.
           PERFORM 840-FIND-PRESET1.

           IF (PLN-COVERAGE-TYPE       = "2")
               MOVE BEN-COMPANY        TO DB-COMPANY
               MOVE BEN-PLAN-TYPE      TO DB-PLAN-TYPE
               MOVE BEN-PLAN-CODE      TO DB-PLAN-CODE
               MOVE "E"                TO DB-COVER-TYPE
               MOVE BEN-COV-UPD-DT     TO DB-START-DATE
               MOVE BEN-COV-GROUP      TO DB-GROUP-NAME
               PERFORM 840-FIND-CVRSET1.

           MOVE "C"                    TO BNBEN-FC.

183600     MOVE BEN-COMPANY            TO BNBEN-COMPANY.
183600     MOVE BEN-EMPLOYEE           TO BNBEN-EMPLOYEE (1).
183800     MOVE BEN-PLAN-TYPE          TO BNBEN-PLAN-TYPE (1).
183800     MOVE BEN-PLAN-CODE          TO BNBEN-PLAN-CODE (1).
183700     MOVE BEN-START-DATE         TO BNBEN-START-DATE (1)
183700                                    BNBEN-PT-START-DATE (1).

J64465     MOVE "Y"                    TO BNBEN-SKIP-ELIGIBILITY (1).

           IF (WF2-RETRO-CHANGE        = "Y")
183700         MOVE BEN-START-DATE     TO BNWS-PROC-DATE.

           IF (BEN-EMP-PRE-CONT        = ZEROES)
               MOVE WS-TRUE            TO BNBEN-ZERO-PREMIUM-SW (1)
P54229         MOVE WS-FALSE           TO BNBEN-NEG-PREMIUM-SW (1)
P54229         MOVE WS-FALSE           TO BNBEN-POS-PREMIUM-SW (1)
           ELSE
           IF (BEN-EMP-PRE-CONT        < ZEROES)
               MOVE WS-TRUE            TO BNBEN-NEG-PREMIUM-SW (1)
P54229         MOVE WS-FALSE           TO BNBEN-ZERO-PREMIUM-SW (1)
P54229         MOVE WS-FALSE           TO BNBEN-POS-PREMIUM-SW (1)
           ELSE
P54229         MOVE WS-FALSE           TO BNBEN-ZERO-PREMIUM-SW (1)
P54229         MOVE WS-FALSE           TO BNBEN-NEG-PREMIUM-SW (1)
               MOVE WS-TRUE            TO BNBEN-POS-PREMIUM-SW (1).

184000     MOVE BEN-COV-OPTION         TO BNBEN-COVER-OPT (1).

           IF (PLN-CONTRIB-TYPE        = "5" OR "6" OR "7")
               MOVE BEN-CYCLES-REMAIN  TO BNBEN-COVER-OPT (1).

           IF  (WF2-FUNCTION-CODE      = "C" OR "CC")
           AND (PLN-COVERAGE-TYPE      = "2")
P60050     AND (CVR-CALC-TYPE          = "M" OR "P" OR "F" OR "N")
               INITIALIZE BNBEN-COVER-AMT (1)
           ELSE
           IF (PLN-CONTRIB-TYPE        = "5" OR "6" OR "7")
               MOVE BEN-BOND-DED-AMT   TO BNBEN-COVER-AMT (1)
           ELSE
               MOVE BEN-COVER-AMT      TO BNBEN-COVER-AMT (1).

           IF (PLN-CONTRIB-TYPE        = "5" OR "6" OR "7")
               MOVE BEN-ANNUAL-AMT     TO BNBEN-PAY-RATE (1)
           ELSE
               MOVE BEN-PAY-RATE       TO BNBEN-PAY-RATE (1).

           COMPUTE BNBEN-EMP-PRE-CONT (1) = BEN-EMP-PRE-CONT
                                          + BEN-CMP-FLX-CONT.
           MOVE BEN-EMP-AFT-CONT       TO BNBEN-EMP-AFT-CONT (1).
           MOVE BEN-CMP-FLX-CONT       TO BNBEN-CMP-FLX-CONT (1).
           MOVE BEN-COMP-CONT          TO BNBEN-COMP-CONT (1).

           IF  (WF2-FUNCTION-CODE      = "C" OR "CC")
           AND (PLN-CONTRIB-TYPE       NOT = "6" AND "7")
               INITIALIZE BNBEN-PAY-RATE     (1)
                          BNBEN-EMP-PRE-CONT (1)
                          BNBEN-EMP-AFT-CONT (1)
                          BNBEN-CMP-FLX-CONT (1)
                          BNBEN-COMP-CONT    (1).

           MOVE BEN-PCT-AMT-FLAG       TO BNBEN-PCT-AMT-FLAG (1).
           MOVE BEN-SMOKER             TO BNBEN-SMOKER-FLAG (1).
           MOVE BEN-PEND-EVIDENCE      TO BNBEN-PEND-EVIDENCE (1).

           IF (PLN-PLAN-TYPE           = "VA")
               MOVE BEN-NBR-HOURS      TO BNBEN-MULT-SALARY (1)
           ELSE
P60050     IF  (WF2-FUNCTION-CODE       = "C" OR "CC")
P60050     AND (PLN-COVERAGE-TYPE       = "2")
P73722     AND (CVR-CALC-TYPE           = "N")
P60050*    AND (CVR-CALC-TYPE           = "M" OR "N")
P60050          INITIALIZE                BNBEN-MULT-SALARY (1)
P60050     ELSE
P60050         MOVE BEN-MULTIPLE       TO BNBEN-MULT-SALARY (1)
P60050     END-IF.

           MOVE BEN-PRE-AFT-FLAG       TO BNBEN-PRE-AFT-FLAG (1).

           IF  (PLN-FLEX-PLAN          NOT = SPACES)
           AND (FLP-SPEND-ONLY         = "N")
               MOVE "Y"                TO BNBEN-FLEX-FLAG (1)
           ELSE
               MOVE "N"                TO BNBEN-FLEX-FLAG (1).

           MOVE FLP-SPEND-ONLY         TO BNBEN-SPEND-ONLY (1).

           MOVE BEN-GL-UPD-DT          TO BNBEN-BNA-START-DATE (1).
           MOVE BEN-GL-GROUP           TO BNBEN-BNA-GROUP-NAME (1).

           MOVE BEN-PREM-UPD-DT        TO BNBEN-PREM-UPD-DT (1).
           MOVE BEN-PREM-GROUP         TO BNBEN-PREM-GROUP (1).

           MOVE PRE-MATCH-OPTION       TO BNBEN-PRE-MATCH-OPTION (1).

P51666     MOVE BEN-DED-START-DATE     TO BNBEN-DED-START-DATE (1).
P51666     MOVE BEN-DED-STOP-DATE      TO BNBEN-DED-STOP-DATE (1).
P51666
           MOVE 1                      TO BNBEN-NBR-LINES.

           IF (WF2-FUNCTION-CODE       = "D")
               MOVE "D"                TO BNBEN-LINE-FC (1)
               MOVE BEN-STOP-DATE      TO BNBEN-STOP-DATE (1)
           ELSE
           IF (WF2-FUNCTION-CODE       = "C" OR "CC")
               MOVE "C"                TO BNBEN-LINE-FC (1)
               MOVE BEN-STOP-DATE      TO BNBEN-STOP-DATE (1)
           ELSE
               MOVE WF2-STOP-DATE      TO BNBEN-STOP-DATE (1)
P85709         IF (WF2-STOP-DATE < BNBEN-DED-STOP-DATE (1))
P85709             MOVE WF2-STOP-DATE  TO BNBEN-DED-STOP-DATE (1)
P85709         END-IF
               MOVE "S"                TO BNBEN-LINE-FC (1).

           IF  (WF2-CHANGE-TYPE        = "3")
           AND (WF2-FUNCTION-CODE      = "C" OR "CC")
               SET BNA-PRE-TAX         TO TRUE
               SET BNA-PRE-MTCH        TO TRUE
               SET BNA-AFT-TAX         TO TRUE
               SET BNA-AFT-MTCH        TO TRUE
               SET BNA-AFT-LMT         TO TRUE
               SET BNA-AFT-LMT-MTCH    TO TRUE
               SET BNA-COMP-CONT       TO TRUE
               SET BNA-FLEX-DED        TO TRUE.

      **** DO NOT CREATE HIPAA TRANS FOR STOPPED BENEFITS WHEN IT IS
      **** STOPPED AND ADDED
           IF (WF2-FUNCTION-CODE       = "CS")
               MOVE "N"                TO BNBEN-CREATE-TRANS (1)
           ELSE
           IF (PRM-CREATE-TRANS        NOT = SPACES)
               MOVE PRM-CREATE-TRANS   TO BNBEN-CREATE-TRANS (1)
           ELSE
               MOVE PLN-CREATE-TRANS   TO BNBEN-CREATE-TRANS (1).

           IF (BNBEN-CREATE-TRANS (1)  = "Y")
P72961         MOVE WF2-HIPAA-REASON   TO BNBEN-HIPAA-REASON
P72961                                    HRHDB-HIPAA-REASON.

           MOVE WF2-FUNCTION-CODE      TO BNBEN-BATCH-FC (1).

       4400-END.

006960******************************************************************
      *4410-REWRITE-ET-BENC.
006960******************************************************************

      *    MOVE "ET"                   TO WF2-FUNCTION-CODE.

      *    PERFORM 8200-REWRITE-BN100BENC.

      *    MOVE WF2-COMPANY            TO WF-COMPANY.
      *    MOVE WF2-EMPLOYEE           TO WF-EMPLOYEE.
      *    MOVE WF2-WF-EFFECT-DATE     TO WF-EFFECT-DATE.
      *    MOVE "3"                    TO WF-REC-TYPE.
      *    MOVE WF2-WF-SEQ-NBR         TO WF-SEQ-NBR.
      *    PERFORM 8400-FIND-BN100WORK.

      *    MOVE "ET"                   TO WF-FUNCTION-CODE.
      *    MOVE CRT-ERROR-NBR          TO WF-ERROR-NBR.
      *    MOVE CRT-ERROR-CAT          TO WF-ERROR-CAT.
      *    MOVE CRT-ERR-VAR1           TO WF-ERR-VAR1.

      *    PERFORM 8200-REWRITE-BN100WORK.

      *4410-END.

011700******************************************************************
       4420-CREATE-PARTICIPNT.
011700******************************************************************

           MOVE WF2-STOP-DATE          TO WSDR-FR-DATE.
           PERFORM 900-DATE-TO-JULIAN.
           ADD 1                       TO WSDR-JULIAN-DAYS.
           PERFORM 900-JULIAN-TO-DATE.
           MOVE WSDR-FR-DATE           TO WS-PAR-OCC-DATE.

           SET CREATE-PAR              TO TRUE.

           MOVE WF2-COMPANY            TO DB-COMPANY.
           MOVE WF2-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE PARSET3-EMPLOYEE       TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PARSET3.
           PERFORM
               UNTIL (PARTICIPNT-NOTFOUND)
               OR    (NO-CREATE-PAR)

               IF  (PAR-OCCUR-DATE     <= WS-PAR-OCC-DATE)
               AND (PAR-TERM-DATE      >= WS-PAR-OCC-DATE)
               AND (PAR-DEPENDENT      = ZEROES)
                   SET NO-CREATE-PAR   TO TRUE
               END-IF

               PERFORM 860-FIND-NXTRNG-PARSET3
           END-PERFORM.

           IF (CREATE-PAR)
               PERFORM 4422-GET-PARTICIPNT-NBR
               THRU    4422-END

               PERFORM 800-CREATE-PARTICIPNT

               PERFORM 4424-MOVE-TO-PARTICIPNT
               THRU    4424-END

               PERFORM 820-STORE-PARTICIPNT.

       4420-END.

011700******************************************************************
       4422-GET-PARTICIPNT-NBR.
011700******************************************************************

168000     MOVE BNC-LAST-PART          TO DB-PARTICIPNT
168100                                    WS-PARTICIPNT.
168200     PERFORM 850-KFIND-NLT-PARSET1.
168300     MOVE WS-FALSE               TO WS-NUMBER-SW.
168400     PERFORM 4423-GET-PARTICIPNT-NBR
           THRU    4423-END
168500         UNTIL (WS-NUMBER-FOUND).

           PERFORM 840-MODIFY-BNCSET1.

           MOVE WS-PARTICIPNT          TO BNC-LAST-PART.

           PERFORM 820-STORE-BNCOMPANY.

       4422-END.

011700******************************************************************
168400 4423-GET-PARTICIPNT-NBR.
011700******************************************************************

224300     IF (WS-PARTICIPNT           = ZEROES)
224400         ADD 1                   TO WS-PARTICIPNT
224500     ELSE
224600     IF (PARTICIPNT-KNOTFOUND)
224700     OR (PAR-COMPANY             NOT = DB-COMPANY)
224800         MOVE WS-TRUE            TO WS-NUMBER-SW
224900     ELSE
225000     IF (PAR-PARTICIPNT          NOT = WS-PARTICIPNT)
225100         MOVE WS-TRUE            TO WS-NUMBER-SW
225200     ELSE
225300         ADD 1                   TO WS-PARTICIPNT
225400         PERFORM 860-KFIND-NEXT-PARSET1.
225500
       4423-END.

011700******************************************************************
       4424-MOVE-TO-PARTICIPNT.
011700******************************************************************

217700     MOVE EMP-COMPANY            TO PAR-COMPANY.
217800     MOVE WS-PARTICIPNT          TO PAR-PARTICIPNT.
217900     MOVE EMP-LAST-NAME          TO PAR-LAST-NAME.
218000     MOVE EMP-FIRST-NAME         TO PAR-FIRST-NAME.
218100     MOVE EMP-MIDDLE-INIT        TO PAR-MIDDLE-INIT.
218200     MOVE EMP-EMPLOYEE           TO PAR-EMPLOYEE.
           MOVE EMP-FICA-NBR           TO PAR-FICA-NBR.
218300     MOVE EMP-ADDR1              TO PAR-ADDR1.
218400     MOVE EMP-ADDR2              TO PAR-ADDR2.
218500     MOVE EMP-CITY               TO PAR-CITY.
218600     MOVE EMP-STATE              TO PAR-STATE.
218700     MOVE EMP-ZIP                TO PAR-ZIP.
218800     MOVE EMP-COUNTRY-CODE       TO PAR-COUNTRY-CODE.
218900     MOVE PEM-HM-PHONE-CNTRY     TO PAR-HM-PHONE-CNTRY.
219000     MOVE PEM-HM-PHONE-NBR       TO PAR-HM-PHONE-NBR.
219200     MOVE PEM-BIRTHDATE          TO PAR-BIRTHDATE.
219300     MOVE PEM-SEX                TO PAR-SEX.
219400     MOVE PEM-SMOKER             TO PAR-SMOKER.
219500     MOVE WF2-COBRA-TYPE         TO PAR-OCCUR-TYPE.

           MOVE WF2-STOP-DATE          TO WSDR-FR-DATE.
           PERFORM 900-DATE-TO-JULIAN.
           ADD 1                       TO WSDR-JULIAN-DAYS.
           PERFORM 900-JULIAN-TO-DATE.
           MOVE WSDR-FR-DATE           TO PAR-OCCUR-DATE.

           PERFORM 4426-CALC-TERM-DATE
           THRU    4426-END.

J85300     MOVE WS-SYSTEM-DATE-YMD     TO PAR-CREATE-DATE.
J85300     MOVE HHMMSS                 TO PAR-CREATE-TIME.
J85300     MOVE CRT-USER-NAME          TO PAR-CREATE-USER-ID.
J85300     MOVE WS-SYSTEM-DATE-YMD     TO PAR-DATE-STAMP.
J85300     MOVE HHMMSS                 TO PAR-TIME-STAMP.
J85300     MOVE CRT-USER-NAME          TO PAR-USER-ID.
J85300
       4424-END.

011700******************************************************************
       4426-CALC-TERM-DATE.
011700******************************************************************
227700
166100     MOVE PAR-COMPANY            TO DB-COMPANY.
166100     MOVE PAR-OCCUR-TYPE         TO DB-OCCUR-TYPE.
166200     PERFORM 840-FIND-OCCSET1.

227600     MOVE PAR-OCCUR-DATE         TO HRDTWS-FROM-DATE.
           MOVE "M"                    TO HRDTWS-ADD-WHAT.
167600     MOVE OCC-MONTHS-EXT         TO HRDTWS-NUMBER.
           PERFORM 5000-DO-ADD-TO-DATE-70.
           IF (ERROR-FOUND)
               GO TO 4426-END.

219600     MOVE HRDTWS-TARGET-DATE     TO PAR-TERM-DATE.

           MOVE HRDTWS-TARGET-DATE     TO WSDR-FR-DATE.
           PERFORM 900-DATE-TO-JULIAN.
           SUBTRACT 1                  FROM WSDR-JULIAN-DAYS.
           PERFORM 900-JULIAN-TO-DATE.
           MOVE WSDR-FR-DATE           TO PAR-TERM-DATE.

       4426-END.

011700******************************************************************
011710 4000-END.
011720******************************************************************
011730
006960******************************************************************
006970 4500-PROCESS-BN100BENA          SECTION 50.
006980******************************************************************
006990 4500-START.
007000
J16623     INITIALIZE BNEDWS-FROM-MAGIC-SW.
J16623 
           IF (PROCESS-BN100BENC)
               MOVE WS-COMPANY         TO WF1-COMPANY
               MOVE WS-EMPLOYEE        TO WF1-EMPLOYEE
               INITIALIZE WF1-PLAN-TYPE
                          WF1-PLAN-CODE
                          WF1-START-DATE
           ELSE
               INITIALIZE WF1-COMPANY
                          WF1-EMPLOYEE
                          WF1-PLAN-TYPE
                          WF1-PLAN-CODE
                          WF1-START-DATE.
007140     PERFORM 8500-FIND-NLT-BN100BENA.

           IF  (PROCESS-BN100BENC)
           AND ((BN100BENA-NOTFOUND)
           OR   (WF1-COMPANY           NOT = WS-COMPANY)
           OR   (WF1-EMPLOYEE          NOT = WS-EMPLOYEE))
               INITIALIZE WS-FILE-NAME
                          WS-EMP-REC-COUNT
               PERFORM 840-MODIFY-CKPSET1
               MOVE WS-RECORD-COUNT        TO WS-RESTART-COUNT
               INITIALIZE WS-REST-FILE-NAME
                          WS-REST-EMP-COUNT
                          WS-REST-NAV-POINTER
               MOVE WS-RESTART-DATA        TO CKP-RESTART-INFO
               PERFORM 820-STORE-CKPOINT
               GO TO 4500-END.

           IF (WS-EMP-REC-COUNT        NOT = ZEROES)
               PERFORM 4550-READ-BN100BENA
               THRU    4550-END
                   WS-EMP-REC-COUNT TIMES.

           IF (PROCESS-BN100BENC)
               PERFORM 4700-UPD-WF1-EMPLOYEE
               THRU    4700-END
           ELSE
011900         PERFORM 4700-UPD-WF1-EMPLOYEE
011910         THRU    4700-END
011920             UNTIL (BN100BENA-NOTFOUND).
011930
           GO TO 4500-END.

011700******************************************************************
       4550-READ-BN100BENA.
011700******************************************************************

           PERFORM 8600-FIND-NEXT-BN100BENA.

       4550-END.

011700******************************************************************
       4700-UPD-WF1-EMPLOYEE.
011700******************************************************************

           IF (PROCESS-BN100BENC)
               CONTINUE
           ELSE
      ******** IF WE ARE COMING AFTER PROCESSING STOPS FOR THE EMPLOYEE
      ******** WS-COMPANY AND EMPLOYEE ARE SET IN 4000-PROCESS-BN100BENC
               MOVE WF1-COMPANY        TO WS-COMPANY
               MOVE WF1-EMPLOYEE       TO WS-EMPLOYEE.

           MOVE WF1-COMPANY            TO DB-COMPANY.
           MOVE WF1-EMPLOYEE           TO DB-EMPLOYEE.

           PERFORM 840-FIND-EMPSET1.
           PERFORM 840-FIND-PEMSET1.

           PERFORM 4710-FIND-NAVIGATE
           THRU    4710-END.

           IF (USE-NAV-SEQ)
011900         PERFORM 4800-UPD-WF1-PLAN-TYPE-NAV
011910         THRU    4800-END

               PERFORM 4720-FIND-NEXT-EMP-IN-BENA
               THRU    4720-END
           ELSE
011900         PERFORM 4900-UPD-WF1-PLAN-TYPE
011910         THRU    4900-END
011920             UNTIL (BN100BENA-NOTFOUND)
                   OR    (WF1-COMPANY  NOT = WS-COMPANY)
                   OR    (WF1-EMPLOYEE NOT = WS-EMPLOYEE).

           INITIALIZE WS-FILE-NAME
                      WS-EMP-REC-COUNT.

           PERFORM 840-MODIFY-CKPSET1.
           MOVE WS-RECORD-COUNT        TO WS-RESTART-COUNT.
           INITIALIZE WS-REST-FILE-NAME
                      WS-REST-EMP-COUNT
                      WS-REST-NAV-POINTER.
           MOVE WS-RESTART-DATA        TO CKP-RESTART-INFO.
           PERFORM 820-STORE-CKPOINT.
           PERFORM 925-AUDIT-END.
           PERFORM 910-AUDIT-BEGIN.

011910 4700-END.

011700******************************************************************
       4710-FIND-NAVIGATE.
011700******************************************************************

           SET NO-NAV-SEQ              TO TRUE.

           MOVE WF1-COMPANY            TO DB-COMPANY.
           INITIALIZE DB-PROCESS-LEVEL.
           MOVE WF1-EMPLOYEE           TO DB-EMPLOYEE.
           PERFORM 840-FIND-NAVSET1.
           IF (NAVIGATE-FOUND)
               SET USE-NAV-SEQ         TO TRUE
               GO TO 4710-END.

           MOVE WF1-COMPANY            TO DB-COMPANY.
           MOVE EMP-PROCESS-LEVEL      TO DB-PROCESS-LEVEL.
PERF       MOVE NAVSET1-PROCESS-LEVEL  TO WS-DB-BEG-RNG.
PERF       PERFORM 850-FIND-BEGRNG-NAVSET1.
           IF  (NAVIGATE-FOUND)
               SET USE-NAV-SEQ         TO TRUE
               GO TO 4710-END.

           MOVE WF1-COMPANY            TO DB-COMPANY.
PERF       MOVE NAVSET1-COMPANY        TO WS-DB-BEG-RNG.
PERF       PERFORM 850-FIND-BEGRNG-NAVSET1.
           IF  (NAVIGATE-FOUND)
               SET USE-NAV-SEQ         TO TRUE
               GO TO 4710-END.

       4710-END.

011700******************************************************************
       4720-FIND-NEXT-EMP-IN-BENA.
011700******************************************************************

           MOVE WS-COMPANY             TO WF1-COMPANY.
           MOVE WS-EMPLOYEE            TO WF1-EMPLOYEE.
           MOVE HIGH-VALUES            TO WF1-PLAN-TYPE
                                          WF1-PLAN-CODE.
           MOVE WS-HIGH-VALUES         TO WF1-START-DATE.
007140     PERFORM 8500-FIND-NLT-BN100BENA.

       4720-END.

011700******************************************************************
011900 4800-UPD-WF1-PLAN-TYPE-NAV.
011700******************************************************************

           IF (WS-NAV-POINTER              = ZEROES)
               MOVE 1                      TO WS-NAV-POINTER.

           PERFORM
               VARYING WS-I1               FROM WS-NAV-POINTER BY 1
               UNTIL  (WS-I1               > 11)

               MOVE WS-EMPLOYEE            TO WF1-EMPLOYEE
               MOVE NAV-PLAN-TYPE (WS-I1)  TO WF1-PLAN-TYPE
               INITIALIZE WF1-PLAN-CODE
                          WF1-START-DATE
               PERFORM 8500-FIND-NLT-BN100BENA

011900         PERFORM 4900-UPD-WF1-PLAN-TYPE
011910         THRU    4900-END
011920             UNTIL (BN100BENA-NOTFOUND)
                   OR    (WF1-COMPANY      NOT = WS-COMPANY)
                   OR    (WF1-EMPLOYEE     NOT = WS-EMPLOYEE)
                   OR    (WF1-PLAN-TYPE    NOT = NAV-PLAN-TYPE (WS-I1))
           END-PERFORM.

011910 4800-END.

011700******************************************************************
011900 4900-UPD-WF1-PLAN-TYPE.
011700******************************************************************

           ADD 1                       TO WS-EMP-REC-COUNT.

           IF (PROCESS-BN100BENC)
               IF (WF1-FUNCTION-CODE   NOT = "CA" AND "SA")
                   GO TO 4900-NEXT-BN100BENA
               END-IF
           ELSE
           IF (WF1-FUNCTION-CODE       NOT = "A")
               GO TO 4900-NEXT-BN100BENA.

           MOVE WF1-START-DATE         TO WS-START-DATE.
           MOVE "A"                    TO WS-FUNCTION-CODE.

           IF (WF1-RETRO-CHANGE        = "Y")
               MOVE WF1-START-DATE     TO BNWS-PROC-DATE.

           PERFORM 4910-CALL-EDIT-BNBEN
           THRU    4910-END.
           IF (ERROR-FOUND)
               PERFORM 4920-REWRITE-EA-BENA
               THRU    4920-END.

           IF (NO-ERROR-FOUND)
ACS003         SET HRDEPBEN-NOT-DELETED    TO TRUE 
ACS003         SET BENEFIT-NOT-DELETED     TO TRUE
               PERFORM 4000-BNBEN-PROCESS-TRAN
ACS003         IF ((PRM-UPDATE-OPTION       = "U")      
ACS003         AND ((HRDEPBEN-DELETED) OR (BENEFIT-DELETED))) 
ACS003             SET A-WF1-PLAN-TYPE     TO TRUE
ACS003             PERFORM 8900-WRITE-ZN325WK1-REC 
ACS003         END-IF
               ADD 5                   TO WS-UPDATE-COUNT
           ELSE
               INITIALIZE CRT-ERROR-NBR
                          CRT-ERROR-CAT
                          CRT-ERR-VAR1
                          CRT-ERR-VAR2
                          CRT-ERR-VAR3
                          CRT-ERR-VAR4
                          CRT-ERR-VAR5
               GO TO 4900-NEXT-BN100BENA.

           IF (PLN-PLAN-TYPE           = "DC")
               PERFORM 4930-CREATE-EMPINVEST
               THRU    4930-END.

           IF  (PLN-COVERAGE-TYPE      NOT = "0")
           AND ((PLN-PLAN-TYPE         = "HL")
           AND  (BNC-DEP-HEALTH        = "Y"))
           OR  ((PLN-PLAN-TYPE         = "DN")
           AND  (BNC-DEP-DENTAL        = "Y"))
           OR  ((PLN-PLAN-TYPE         = "DL")
           AND  (BNC-DEP-DEP-LIFE      = "Y"))
               PERFORM 4940-CREATE-HRDEPBEN
               THRU    4940-END.

           INITIALIZE BNWS-PROC-DATE.

           IF (WS-UPDATE-COUNT         > WS-MAX-OPS-IN-TRAN)
               INITIALIZE WS-UPDATE-COUNT
               PERFORM 840-MODIFY-CKPSET1
               MOVE WS-BENA-FILE-NAME  TO WS-REST-FILE-NAME
               MOVE WS-EMP-REC-COUNT   TO WS-REST-EMP-COUNT
               MOVE WS-I1              TO WS-REST-NAV-POINTER
               MOVE WS-RESTART-DATA    TO CKP-RESTART-INFO
               PERFORM 820-STORE-CKPOINT
               PERFORM 925-AUDIT-END
               PERFORM 910-AUDIT-BEGIN.

       4900-NEXT-BN100BENA.
           PERFORM 8600-FIND-NEXT-BN100BENA.

011910 4900-END.

011700******************************************************************
       4910-CALL-EDIT-BNBEN.
011700******************************************************************

           IF (BNBEN-SV-SP-END-DT (1,1) NOT = ZEROES)
               INITIALIZE BN100WS-SV-SP-TBL
               MOVE BNBEN-SV-SP-TBL (1)  TO BN100WS-SV-SP-TBL.

002000     INITIALIZE BNBEN-DETAIL-GROUP
002000                BNBEN-USE-NAVIGATE-SW.

           IF (BN100WS-SV-SP-END-DT (1)  NOT = ZEROES)
               MOVE BN100WS-SV-SP-TBL  TO BNBEN-SV-SP-TBL (1)
               INITIALIZE BN100WS-SV-SP-TBL
               INITIALIZE I4.

           MOVE "A"                    TO BNBEN-LINE-FC (1).

           MOVE WF1-COMPANY            TO BNBEN-COMPANY.
           MOVE WF1-EMPLOYEE           TO BNBEN-EMPLOYEE (1).
           MOVE WF1-PLAN-TYPE          TO BNBEN-PLAN-TYPE (1).
           MOVE WF1-PLAN-CODE          TO BNBEN-PLAN-CODE (1).
           MOVE WF1-START-DATE         TO BNBEN-START-DATE (1).
184000     MOVE WF1-STOP-DATE          TO BNBEN-STOP-DATE (1).
P85709     MOVE WF1-OLD-DED-STOP-DATE  TO BNBEN-DED-OVSTOP (1).
P85174     MOVE WF1-OLD-DED-STOP-DATE  TO BNBEN-DED-STOP-DATE (1).
J16623     MOVE "Y"                    TO BNEDWS-FROM-MAGIC-SW.
J16623     MOVE "Y"                    TO BNBEN-SKIP-ELIGIBILITY (1).
155990     MOVE WF1-ELIG-GROUP         TO BNBEN-ELIG-GROUP (1).

J08297*    IF (PRM-CREATE-TRANS        NOT = SPACES)
J08297*        MOVE PRM-CREATE-TRANS   TO BNBEN-CREATE-TRANS (1)
J08297*    ELSE
J08297*        MOVE PLN-CREATE-TRANS   TO BNBEN-CREATE-TRANS (1).

J08297*    IF (BNBEN-CREATE-TRANS (1)  = "Y")
P72961*        MOVE WF1-HIPAA-REASON   TO BNBEN-HIPAA-REASON
P72961*                                   HRHDB-HIPAA-REASON.

J57497     MOVE WF1-NEW-DATE           TO BNBEN-NEW-DATE.

           IF (WF1-FUNCTION-CODE       = "CA" OR "SA")
               PERFORM 4912-MOVE-TO-BNBEN
               THRU    4912-END
               IF (ERROR-FOUND)
                   GO TO 4910-END.

           PERFORM 2000-BNBEN-EDIT-TRAN.

       4910-END.

006960******************************************************************
       4912-MOVE-TO-BNBEN.
006960******************************************************************

           MOVE WF1-COMPANY            TO DB-COMPANY.
           MOVE WF1-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE WF1-PLAN-CODE          TO DB-PLAN-CODE.
           PERFORM 840-FIND-PLNSET1.

           IF (PLN-FLEX-PLAN           NOT = SPACES)
               MOVE PLN-FLEX-PLAN      TO DB-FLEX-PLAN
               PERFORM 840-FIND-FLPSET1.

           MOVE WF1-COMPANY            TO DB-COMPANY.
           MOVE WF1-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE WF1-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE WF1-PLAN-CODE          TO DB-PLAN-CODE.
           MOVE WF1-OLD-START-DATE     TO DB-START-DATE.
           PERFORM 840-FIND-BENSET1.

           MOVE BEN-PREM-UPD-DT        TO DB-START-DATE.
           MOVE BEN-PREM-GROUP         TO DB-GROUP-NAME.
           PERFORM 840-FIND-PRESET1.

           IF (PLN-COVERAGE-TYPE       = "2")
               MOVE BEN-COMPANY        TO DB-COMPANY
               MOVE BEN-PLAN-TYPE      TO DB-PLAN-TYPE
               MOVE BEN-PLAN-CODE      TO DB-PLAN-CODE
               MOVE "E"                TO DB-COVER-TYPE
               MOVE BEN-COV-UPD-DT     TO DB-START-DATE
               MOVE BEN-COV-GROUP      TO DB-GROUP-NAME
               PERFORM 840-FIND-CVRSET1.

J08297     IF (PRM-CREATE-TRANS        NOT = SPACES)
J08297         MOVE PRM-CREATE-TRANS   TO BNBEN-CREATE-TRANS (1)
J08297     ELSE
J08297         MOVE PLN-CREATE-TRANS   TO BNBEN-CREATE-TRANS (1).

J08297     IF (BNBEN-CREATE-TRANS (1)  = "Y")
P72961         MOVE WF1-HIPAA-REASON   TO BNBEN-HIPAA-REASON
P72961                                    HRHDB-HIPAA-REASON.

184000     MOVE BEN-COV-OPTION         TO BNBEN-COVER-OPT (1).

           IF (PLN-CONTRIB-TYPE        = "5" OR "6" OR "7")
               MOVE BEN-CYCLES-REMAIN  TO BNBEN-COVER-OPT (1).

           IF (PLN-CONTRIB-TYPE        = "5" OR "6" OR "7")
               MOVE BEN-BOND-DED-AMT   TO BNBEN-COVER-AMT (1)
           ELSE
           IF  (WF1-CHANGE-TYPE        = "2")
           AND (PLN-COVERAGE-TYPE      = "2")
P60050     AND (CVR-CALC-TYPE          = "M" OR "P" OR "N")
      ******** IF SALARY CHANGE INITIALIZE TO RECALC COVERAGE
               INITIALIZE BNBEN-COVER-AMT (1)
           ELSE
           IF  (PLN-COVERAGE-TYPE      = "2")
P60050     AND (CVR-CALC-TYPE          = "F" OR "P" OR "M" OR "N")
      ******** IF FLAT AMOUNT COVERAGE, INITIALIZE TO RECALC COVERAGE
               INITIALIZE BNBEN-COVER-AMT (1)
           ELSE
               MOVE BEN-COVER-AMT      TO BNBEN-COVER-AMT (1).

           IF (PLN-CONTRIB-TYPE        = "5" OR "6" OR "7")
               MOVE BEN-ANNUAL-AMT     TO BNBEN-PAY-RATE (1)
           ELSE
           IF (WF1-CHANGE-TYPE         = "2")
               INITIALIZE BNBEN-PAY-RATE (1)
           ELSE
               MOVE BEN-PAY-RATE       TO BNBEN-PAY-RATE (1).

           IF (WF1-CHANGE-TYPE         = "2")
      ******** IF SALARY CHANGE INITIALIZE TO RECALC CONTRIBUTION
               INITIALIZE BNBEN-EMP-PRE-CONT (1)
                          BNBEN-EMP-AFT-CONT (1)
                          BNBEN-CMP-FLX-CONT (1)
                          BNBEN-COMP-CONT (1)
           ELSE
               COMPUTE BNBEN-EMP-PRE-CONT (1) = BEN-EMP-PRE-CONT
                                              + BEN-CMP-FLX-CONT
               MOVE BEN-EMP-AFT-CONT   TO BNBEN-EMP-AFT-CONT (1)
               MOVE BEN-CMP-FLX-CONT   TO BNBEN-CMP-FLX-CONT (1)
               MOVE BEN-COMP-CONT      TO BNBEN-COMP-CONT (1).

           IF  (WF1-FUNCTION-CODE      = "CA")
P49244     AND ((WF1-CHANGE-TYPE       NOT = "3")
P49244     OR  (ADD-GROUP-CHANGES))
           AND ((PLN-CONTRIB-TYPE      NOT = "5" AND "6" AND "7")
           OR   (PLN-PLAN-TYPE         = "VA"))
               INITIALIZE BNBEN-PAY-RATE     (1)
                          BNBEN-EMP-PRE-CONT (1)
                          BNBEN-EMP-AFT-CONT (1)
                          BNBEN-CMP-FLX-CONT (1)
                          BNBEN-COMP-CONT    (1).

P78868     IF  (PLN-CONTRIB-TYPE = "5" OR "6" OR "7")
P78868     AND (BEN-PCT-AMT-FLAG = "P")
P78868         COMPUTE BNBEN-EMP-PRE-CONT (1) = BEN-EMP-PRE-CONT
P78868                                        + BEN-CMP-FLX-CONT
P78868         MOVE BEN-EMP-AFT-CONT   TO BNBEN-EMP-AFT-CONT (1).

           MOVE BEN-PCT-AMT-FLAG       TO BNBEN-PCT-AMT-FLAG (1).
           MOVE BEN-SMOKER             TO BNBEN-SMOKER-FLAG (1).
           MOVE BEN-PEND-EVIDENCE      TO BNBEN-PEND-EVIDENCE (1).

           IF (PLN-PLAN-TYPE           = "VA")
               MOVE BEN-NBR-HOURS      TO BNBEN-MULT-SALARY (1)
P60050     ELSE
P64594     IF (CVR-CALC-TYPE           = "N" )
P60050         INITIALIZE                 BNBEN-MULT-SALARY (1)
           ELSE
P60050         MOVE BEN-MULTIPLE       TO BNBEN-MULT-SALARY (1)
P60050     END-IF
P60050     END-IF.

           MOVE BEN-PRE-AFT-FLAG       TO BNBEN-PRE-AFT-FLAG (1).

           MOVE BEN-TA-UPD-FLAG        TO BNBEN-TA-UPD-FLAG (1).

           IF (WF1-FUNCTION-CODE       = "SA")
               MOVE "Y"                TO BNBEN-SKIP-GROUP-N-ZIP.

           MOVE "C"                    TO BNBEN-FC.
           MOVE 1                      TO BNBEN-NBR-LINES.

           MOVE "Y"                    TO BNBEN-SKIP-ELIGIBILITY (1).

           MOVE BEN-ELIG-UPD-DT        TO BNBEN-ELIG-UPD-DT (1).
           MOVE BEN-ELIG-GROUP         TO BNBEN-ELIG-GROUP  (1).

           IF  (PLN-CONTRIB-TYPE            NOT = "0")
P63031     AND (PLN-CONTRIB-TYPE            NOT = "X ")
019000         MOVE BNBEN-COMPANY          TO BNREWS-COMPANY
019100         MOVE BNBEN-PLAN-TYPE (1)    TO BNREWS-PLAN-TYPE
019200         MOVE BNBEN-PLAN-CODE (1)    TO BNREWS-PLAN-CODE
019300         MOVE "E"                    TO BNREWS-COVER-TYPE
019400         MOVE BNBEN-EMPLOYEE (1)     TO BNREWS-EMPLOYEE
019500         MOVE BNBEN-START-DATE (1)   TO BNREWS-AS-OF-DATE
019600         MOVE "PRE"                  TO BNREWS-FILE-PREFIX
019700         PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70
019800         IF (ERROR-FOUND)
019900             GO TO 4912-END.
020000
           IF (PRE-CONT-TAX-STS        = "N")
               INITIALIZE BNBEN-PCT-AMT-FLAG (1)
                          BNBEN-PRE-AFT-FLAG (1).

J64465     MOVE "Y"                    TO BNEDWS-FROM-MAGIC-SW.
J64465     MOVE "Y"                    TO BNBEN-SKIP-ELIGIBILITY (1).
       4912-END.

006960******************************************************************
       4920-REWRITE-EA-BENA.
006960******************************************************************

           MOVE "EA"                   TO WF1-FUNCTION-CODE.

           PERFORM 8200-REWRITE-BN100BENA.

           MOVE WF1-COMPANY            TO WF-COMPANY.
           MOVE WF1-EMPLOYEE           TO WF-EMPLOYEE.
           MOVE WF1-WF-EFFECT-DATE     TO WF-EFFECT-DATE.
           MOVE "3"                    TO WF-REC-TYPE.
           MOVE WF1-WF-SEQ-NBR         TO WF-SEQ-NBR.
           PERFORM 8400-FIND-BN100WORK.

           MOVE "EA"                   TO WF-FUNCTION-CODE.
           MOVE CRT-ERROR-NBR          TO WF-ERROR-NBR.
P09149     MOVE WS-TRUE                TO WS-CO-ERR-SW.
           MOVE CRT-ERROR-CAT          TO WF-ERROR-CAT.
           MOVE CRT-ERR-VAR1           TO WF-ERR-VAR1.

           PERFORM 8200-REWRITE-BN100WORK.

       4920-END.

011700******************************************************************
       4930-CREATE-EMPINVEST.
011700******************************************************************

           IF (WF1-FUNCTION-CODE           = "CA" OR "SA")
               PERFORM 4932-CREATE-EMI-FOR-CASA
               THRU    4932-END
           ELSE
           IF (PRM-UPDATE-INVEST           = "Y")
               PERFORM 4934-CREATE-EMI-FOR-A
               THRU    4934-END.

       4930-END.

011700******************************************************************
       4932-CREATE-EMI-FOR-CASA.
011700******************************************************************

           MOVE WF1-COMPANY                TO DB-COMPANY.
           MOVE WF1-EMPLOYEE               TO DB-EMPLOYEE.
           MOVE WF1-PLAN-CODE              TO DB-PLAN-CODE.
           MOVE WF1-OLD-START-DATE         TO DB-START-DATE.
           INITIALIZE DB-SEQ-NBR
                      DB-DIST-ST-DATE.
           PERFORM 850-FIND-NLT-EMISET1.
           PERFORM
               UNTIL (EMPINVEST-NOTFOUND)
               OR    (EMI-COMPANY          NOT = WF1-COMPANY)
               OR    (EMI-EMPLOYEE         NOT = WF1-EMPLOYEE)
               OR    (EMI-PLAN-CODE        NOT = WF1-PLAN-CODE)
               OR    (EMI-START-DATE       NOT = WF1-OLD-START-DATE)

               MOVE EMI-SEQ-NBR            TO WS-SAVE-SEQ-NBR
               MOVE EMI-DIST-ST-DATE       TO WS-SAVE-DIST-ST-DATE

               PERFORM 810-RECREATE-EMPINVEST

               MOVE WF1-START-DATE         TO EMI-START-DATE
               MOVE WF1-STOP-DATE          TO EMI-STOP-DATE

               PERFORM 820-STORE-EMPINVEST
               
               MOVE WS-SAVE-SEQ-NBR        TO DB-SEQ-NBR
               MOVE WS-SAVE-DIST-ST-DATE   TO DB-DIST-ST-DATE
               PERFORM 840-FIND-EMISET1

               PERFORM 860-FIND-NEXT-EMISET1
           END-PERFORM.

       4932-END.

011700******************************************************************
       4934-CREATE-EMI-FOR-A.
011700******************************************************************

           MOVE WF1-COMPANY            TO DB-COMPANY.
           MOVE WF1-PLAN-CODE          TO DB-PLAN-CODE.
           INITIALIZE DB-SEQ-NBR.
           PERFORM 850-FIND-NLT-BIVSET2.
           IF  (BNINVEST-FOUND)
           AND (BIV-COMPANY            = DB-COMPANY)
           AND (BIV-PLAN-CODE          = DB-PLAN-CODE)
               PERFORM 800-CREATE-EMPINVEST

               PERFORM 4936-MOVE-TO-EMPINVEST
               THRU    4936-END

               PERFORM 820-STORE-EMPINVEST.

       4934-END.

011700******************************************************************
       4936-MOVE-TO-EMPINVEST.
011700******************************************************************

           MOVE WF1-COMPANY            TO EMI-COMPANY.
           MOVE WF1-EMPLOYEE           TO EMI-EMPLOYEE.
           MOVE WF1-PLAN-CODE          TO EMI-PLAN-CODE.
           MOVE WF1-START-DATE         TO EMI-START-DATE.

           MOVE BIV-SEQ-NBR            TO EMI-SEQ-NBR.

           MOVE WF1-START-DATE         TO EMI-DIST-ST-DATE.
           MOVE WF1-STOP-DATE          TO EMI-STOP-DATE.

           MOVE 100                    TO EMI-DIST-PCT.

       4936-END.

011700******************************************************************
       4940-CREATE-HRDEPBEN.
011700******************************************************************

           MOVE EMDSET1-EMPLOYEE           TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-EMDSET1.
           IF (EMDEPEND-FOUND)
               MOVE "E"                    TO HRDEP-COVER-TYPE
               INITIALIZE HRDEP-CALC-AGE-DATE
               PERFORM 5110-FIND-DEP-N-STUD-AGE

J07540* (S)POUSE, (D)EPENDENTS, (B)OTH SPOUSE AND DEPS, (P)ARTNER, 
J07540* SPOUSE (O)R PARTNER, PA(R)TNER DEPS, (C) PARTNER AND DEPS, 
J07540* SPOUSE OR PARTNER (A)ND DEPS.
J07540         IF (HRDEP-COV-DEPS          = "S" OR "D" OR "B" OR
J07540             "P" OR "O" OR "R" OR "C" OR "A")
                   IF (WF1-FUNCTION-CODE   = "CA" OR "SA")
                       PERFORM 4942-CREATE-HDB-FOR-CASA
                       THRU    4942-END
                   ELSE
                   IF (PRM-UPDATE-DEPEND   = "Y")
                       PERFORM 4944-CREATE-HDB-FOR-A
                       THRU    4944-END.

       4940-END.

011700******************************************************************
       4942-CREATE-HDB-FOR-CASA.
011700******************************************************************

           INITIALIZE WS-DEP-TBL.

           MOVE WF1-COMPANY            TO WF2-COMPANY
                                          DB-COMPANY.
           MOVE WF1-EMPLOYEE           TO WF2-EMPLOYEE
                                          DB-EMPLOYEE.
           MOVE WF1-PLAN-TYPE          TO WF2-PLAN-TYPE
                                          DB-PLAN-TYPE.
           MOVE WF1-PLAN-CODE          TO WF2-PLAN-CODE
                                          DB-PLAN-CODE.
           MOVE WF1-OLD-START-DATE     TO WF2-START-DATE.
           PERFORM 8400-FIND-BN100BENC.
           MOVE WF1-START-DATE         TO DB-START-DATE.
           PERFORM 840-FIND-BENSET1.

           MOVE WF1-COMPANY            TO DB-COMPANY.
           MOVE WF1-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE WF1-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE WF1-PLAN-CODE          TO DB-PLAN-CODE.
           MOVE WF1-OLD-START-DATE     TO DB-EMP-START.
           INITIALIZE DB-DEPENDENT
                      DB-START-DATE.
           PERFORM 850-FIND-NLT-HDBSET3.
           PERFORM
               UNTIL (HRDEPBEN-NOTFOUND)
               OR    (HDB-COMPANY      NOT = WF1-COMPANY)
               OR    (HDB-EMPLOYEE     NOT = WF1-EMPLOYEE)
               OR    (HDB-PLAN-TYPE    NOT = WF1-PLAN-TYPE)
               OR    (HDB-PLAN-CODE    NOT = WF1-PLAN-CODE)
               OR    (HDB-EMP-START    NOT = WF1-OLD-START-DATE)

J10377         IF (HDB-PARTICIPNT = ZEROS)        
                   MOVE HDB-COMPANY        TO DB-COMPANY
                   MOVE HDB-EMPLOYEE       TO DB-EMPLOYEE
                   MOVE HDB-DEPENDENT      TO DB-SEQ-NBR
                   PERFORM 840-FIND-EMDSET1

                   IF  (EMD-ACTIVE-FLAG    = "A")
                   AND (HDB-STOP-DATE      = WF2-STOP-DATE)
                   AND (HDB-USER-ID        = "BN100")
                       MOVE HDB-DEPENDENT      TO WS-SAVE-DEPENDENT
                       MOVE HDB-START-DATE     TO WS-SAVE-START-DATE

                       INITIALIZE WS-HDB-STOP-DATE

                       MOVE HDB-COMPANY        TO DB-COMPANY
                       MOVE HDB-EMPLOYEE       TO DB-EMPLOYEE
                       MOVE HDB-DEPENDENT      TO DB-SEQ-NBR
                       PERFORM 840-FIND-EMDSET1

                       MOVE WF1-FUNCTION-CODE  TO BNBEN-FUNCTION-CODE
                       PERFORM 6000-CALC-HDB-STOP-DATE

                       IF (HDB-START-DATE      NOT = ZEROES)
J07540                 OR (EMD-DEP-TYPE            = "S" OR "P")
J59067                 OR (EMD-DISABLED            = "Y")
                           MOVE HDB-STOP-DATE  TO WS-HDB-STOP-DATE

                           PERFORM 4943-ADD-TO-DEP-TBL
                           THRU    4943-END
    
320200                     PERFORM 810-RECREATE-HRDEPBEN

320400                     MOVE WF1-START-DATE     TO HDB-EMP-START
320500                                                HDB-START-DATE

                           MOVE WS-HDB-STOP-DATE   TO HDB-STOP-DATE
                           MOVE WS-SYSTEM-DATE-YMD TO HDB-CREATION-DATE
                                                  HDB-UPD-DATE
                           MOVE "BN100"            TO HDB-USER-ID
J67329                                                HDB-CREATE-USER-ID
                           MOVE HHMMSS             TO HDB-TIME-STAMP
J67329                                                HDB-CREATE-TIME                       
321000                     PERFORM 820-STORE-HRDEPBEN

                           IF (BNBEN-CREATE-TRANS (1) = "Y")
                               PERFORM 4960-CREATE-BNTRANS
                               THRU    4960-END
                           END-IF

                       END-IF

321200                 MOVE WS-SAVE-DEPENDENT  TO DB-DEPENDENT
321300                 MOVE WS-SAVE-START-DATE TO DB-START-DATE
321400                 PERFORM 850-FIND-NLT-HDBSET3
                   END-IF

J10377         END-IF
               PERFORM 860-FIND-NEXT-HDBSET3
           END-PERFORM.

           INITIALIZE I4.

           IF (WF1-FUNCTION-CODE   = "CA")
               PERFORM 4952-CHG-EMP-START-ON-HDB
               THRU    4952-END.

       4942-END.

011700******************************************************************
       4943-ADD-TO-DEP-TBL.
011700******************************************************************

           PERFORM
               VARYING WS-I4 FROM 1 BY 1
               UNTIL  (WS-I4              > WS-DEP-MAX)
               OR     (WS-DEP-NBR (WS-I4) = ZEROES)
               OR     (HDB-DEPENDENT      = WS-DEP-NBR (WS-I4))

               CONTINUE
           END-PERFORM.

           IF (WS-I4                   > WS-DEP-MAX)
           OR (HDB-DEPENDENT           = WS-DEP-NBR (WS-I4))
               GO TO 4943-END.

           MOVE HDB-DEPENDENT          TO WS-DEP-NBR (WS-I4).
           MOVE WS-HDB-STOP-DATE       TO WS-DEP-ST-DT (WS-I4).

       4943-END.

011700******************************************************************
       4944-CREATE-HDB-FOR-A.
011700******************************************************************

           MOVE WF1-COMPANY            TO DB-COMPANY.
           MOVE WF1-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE EMDSET1-EMPLOYEE       TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-EMDSET1.
           PERFORM
               UNTIL (EMDEPEND-NOTFOUND)

               IF (EMD-ACTIVE-FLAG     = "A")
                   PERFORM 4946-CREATE-HRDEPBEN
                   THRU    4946-END
               END-IF

               PERFORM 860-FIND-NXTRNG-EMDSET1
           END-PERFORM.

       4944-END.

004200******************************************************************
       4946-CREATE-HRDEPBEN.
004200******************************************************************

J07540* (E)MPLOYEE, (N)ONE, (S)POUSE, (P)ARTNER, SPOUSE (O)R PARTNER
J07540     IF  (EMD-DEP-TYPE               = "D")
J07540     AND (HRDEP-COV-DEPS             = "E" OR "N" OR "S" OR
J07540          "P" OR "O")
J07540         GO TO 4946-END.        
J07540* (E)MPLOYEE, (N)ONE, (D)EPENDENTS, PA(R)TNER DEPS
J07540     IF  (EMD-DEP-TYPE               = "S" OR "P")
J07540     AND (HRDEP-COV-DEPS             = "E" OR "N" OR "D" OR "R")
J07540         GO TO 4946-END.         
J07540* (P)ARTNER, (C)PARTNER AND DEPS
J07540     IF  (EMD-DEP-TYPE               = "S")
J07540     AND (HRDEP-COV-DEPS             = "P" OR "C")
J07540         GO TO 4946-END.
J07540* (S)POUSE, (B)OTH SPOUSE AND DEPS
J07540     IF  (EMD-DEP-TYPE               = "P")
J07540     AND (HRDEP-COV-DEPS             = "S" OR "B")
J07540         GO TO 4946-END.

           IF  (EMD-DEP-TYPE               = "D")
           AND (EMD-DISABLED               = "N")
               INITIALIZE HRDEP-CALC-DEP-N-STUD
               PERFORM 5200-DEP-AGE-DATE-CALC
               IF (HRDEP-DEP-END-DATE      < WF1-START-DATE)
                   GO TO 4946-END.

           PERFORM 800-CREATE-HRDEPBEN.

           PERFORM 4948-MOVE-TO-HRDEPBEN
           THRU    4948-END.

           PERFORM 820-STORE-HRDEPBEN.

           IF (BNBEN-CREATE-TRANS (1)  = "Y")
               PERFORM 4960-CREATE-BNTRANS
               THRU    4960-END.

       4946-END.

004200******************************************************************
       4948-MOVE-TO-HRDEPBEN.
004200******************************************************************

           MOVE EMD-COMPANY            TO HDB-COMPANY.
           MOVE EMD-EMPLOYEE           TO HDB-EMPLOYEE.
           MOVE EMD-SEQ-NBR            TO HDB-DEPENDENT.

           MOVE WF1-PLAN-TYPE          TO HDB-PLAN-TYPE.
           MOVE WF1-PLAN-CODE          TO HDB-PLAN-CODE.
           MOVE WF1-START-DATE         TO HDB-EMP-START
                                          HDB-START-DATE.

           MOVE HRDEP-DEP-END-DATE     TO HDB-STOP-DATE.

           MOVE WS-SYSTEM-DATE-YMD     TO HDB-CREATION-DATE
                                          HDB-UPD-DATE.
           MOVE HHMMSS                 TO HDB-TIME-STAMP
J67329                                    HDB-CREATE-USER-ID.
           MOVE CRT-USER-NAME          TO HDB-USER-ID
J67329                                    HDB-CREATE-USER-ID.

       4948-END.

011700******************************************************************
       4952-CHG-EMP-START-ON-HDB.
011700******************************************************************

      **** If benefit is stopped and added. We want to move HRDEPBEN
      **** from stopped benefit to added benefit by changing EmpStart
      **** field. This is done on HRDEPBENs that are in future from the
      **** StopDate of Benefit

           MOVE WF1-COMPANY            TO DB-COMPANY.
           MOVE WF1-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE WF1-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE WF1-PLAN-CODE          TO DB-PLAN-CODE.
           MOVE WF1-OLD-START-DATE     TO DB-EMP-START.
           INITIALIZE DB-DEPENDENT
                      DB-START-DATE.
           PERFORM 850-MODIFY-NLT-HDBSET3.
           PERFORM
               UNTIL (HRDEPBEN-NOTFOUND)
               OR    (HDB-COMPANY      NOT = WF1-COMPANY)
               OR    (HDB-EMPLOYEE     NOT = WF1-EMPLOYEE)
               OR    (HDB-PLAN-TYPE    NOT = WF1-PLAN-TYPE)
               OR    (HDB-PLAN-CODE    NOT = WF1-PLAN-CODE)
               OR    (HDB-EMP-START    NOT = WF1-OLD-START-DATE)


J10377         IF (HDB-PARTICIPNT = ZEROS)
                   MOVE WF1-OLD-START-DATE     TO DB-START-DATE
                   PERFORM 840-FIND-BENSET1
    
                   IF (HDB-START-DATE          > BEN-STOP-DATE)
                       MOVE HDB-START-DATE     TO WS-SAVE-START-DATE

                       PERFORM 830-DELETE-HRDEPBEN

                       PERFORM 810-RECREATE-HRDEPBEN

                       MOVE WF1-START-DATE     TO HDB-EMP-START
    
                       MOVE WS-SYSTEM-DATE-YMD TO HDB-CREATION-DATE
                                                  HDB-UPD-DATE
                       MOVE "BN100"            TO HDB-USER-ID
J67329                                            HDB-CREATE-USER-ID
                       MOVE HHMMSS             TO HDB-TIME-STAMP
J67329                                            HDB-CREATE-TIME

                       PERFORM 820-STORE-HRDEPBEN
    
                       MOVE WF1-OLD-START-DATE TO DB-EMP-START
                       MOVE WS-SAVE-DEPENDENT  TO DB-DEPENDENT
                       MOVE WS-SAVE-START-DATE TO DB-START-DATE
                       PERFORM 850-MODIFY-NLT-HDBSET3
                   ELSE
                       PERFORM 860-MODIFY-NEXT-HDBSET3
                   END-IF
J10377         ELSE 
J10377             PERFORM 860-MODIFY-NEXT-HDBSET3
J10377         END-IF
           END-PERFORM.

       4952-END.

011700******************************************************************
       4960-CREATE-BNTRANS.
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

           MOVE WF1-HIPAA-REASON           TO BNT-TRAN-REASON.

           MOVE PLN-MEMBER-ID              TO BNT-MEMBER-ID.

           MOVE HDB-START-DATE             TO BNT-EFFECT-DATE.

           INITIALIZE BNT-HIPAA-FILE-NBR
                      BNT-FILE-DATE.

           MOVE WS-SYSTEM-DATE-YMD         TO BNT-DATE-STAMP.
           MOVE HHMMSS                     TO BNT-TIME-STAMP.

           MOVE CRT-PROGRAM-CODE           TO BNT-USER-ID.

           PERFORM 820-STORE-BNTRANS.

       4960-END.

011700******************************************************************
011710 4500-END.
011720******************************************************************
011730
006960******************************************************************
006970 5000-PRINT-REPORT               SECTION 50.
006980******************************************************************
006990 5000-START.
007000
      **** Processing BN100 - Printing report
001400     MOVE 058                    TO CRT-MSG-NBR.
001410     PERFORM 780-DISPLAY-MSG.
001420
003390     INITIALIZE RPT-PAGE-COUNT (BN100-R1)
                      RPT-PAGE-COUNT (BN100-R2).
003400
003560     SET NO-END-OF-FILE          TO TRUE.
005600     READ BN100WORKS-FILE        INTO SRT-BN100WORKS-REC
003580         AT END
      ************ No data to print in Employee benefit change
003560             SET END-OF-FILE     TO TRUE
007170             MOVE 051            TO CRT-MSG-NBR
007180             PERFORM 780-PRINT-MSG
007190             GO TO 5000-END.
011890
P09149     OPEN I-O BN100WORK-FILE.
ACS004
ACS004     IF (PRM-UPDATE-OPTION           = "U")
ACS004        OPEN EXTEND ZN325WK1-FILE. 
P09149  
011900     PERFORM 5100-DO-BNG-COMPANY
011910     THRU    5100-END
011920         UNTIL (END-OF-FILE).
011930
P09149     CLOSE BN100WORK-FILE.
ACS004
ACS004     IF (PRM-UPDATE-OPTION           = "U")
ACS004        CLOSE ZN325WK1-FILE SAVE.
P09149  
007380     GO TO 5000-END.
007390
011700******************************************************************
011900 5100-DO-BNG-COMPANY.
011700******************************************************************

           MOVE SRT-COMPANY            TO G1-BNG-COMPANY
                                          E1-BNG-COMPANY
                                          W1-BNG-COMPANY
                                          DB-COMPANY
                                          WS-COMPANY.
           INITIALIZE DB-PROCESS-LEVEL.
           PERFORM 840-FIND-PRSSET1.

           INITIALIZE WS-PROC-LEVEL
                      WS-DEPARTMENT.

           MOVE PRS-NAME               TO G1-PRS-NAME
                                          E1-PRS-NAME
                                          W1-PRS-NAME.
003790     MOVE PRM-UPDATE-OPTION      TO G1-PRM-UPDATE-OPTION
                                          E1-PRM-UPDATE-OPTION
                                          W1-PRM-UPDATE-OPTION.
003850
J17582     IF (PRBRD-BROADCAST-ENABLED)
J17582         MOVE 500                    TO PRBRD-ELEMENT-TYPE
J17582         MOVE PRM-COMPANY            TO PRBRD-ELEMENT-VALUE
J17582         PERFORM 610-BEG-ELEMENT-TAG
J17582         MOVE WFCHWS-TAG-STRING      TO BTL1-TAG-LINE
J17582         MOVE BROADCAST-TAG-LINE-1   TO RPT-GROUP-REQUEST
J17582         PERFORM 700-PRINT-RPT-GRP
J17582     END-IF.
J17582
           MOVE GN1-BNG-COMPANY        TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.
004340
      *--- Error report header not printed when there are error records
      *--- in work files. Since this is @ LEVEL A with DELAY won't get
      *--- printed when there are no details printed in error report
      *--- This was related to PT125796
P09149*    IF (COMPANY-HAS-ERROR)
P09149         MOVE EN1-BNG-COMPANY        TO RPT-GROUP-REQUEST.
P09149         PERFORM 700-PRINT-RPT-GRP.
004340
      *    IF (COMPANY-HAS-WARNING)
               MOVE WN1-BNG-COMPANY        TO RPT-GROUP-REQUEST.
               PERFORM 700-PRINT-RPT-GRP.
      
           IF (PRM-REPORT-SEQ          = "2")
               PERFORM 5125-DO-PROC-LEVEL
               THRU    5125-END
                   UNTIL (END-OF-FILE)
                   OR    (SRT-COMPANY  NOT = WS-COMPANY)
           ELSE
               PERFORM 5150-DO-BNG-EMPLOYEE
               THRU    5150-END
                   UNTIL (END-OF-FILE)
                   OR    (SRT-COMPANY  NOT = WS-COMPANY).
J17582
J17582     IF (PRBRD-BROADCAST-ENABLED)
J17582         MOVE 500                    TO PRBRD-ELEMENT-TYPE
J17582         MOVE PRM-COMPANY            TO PRBRD-ELEMENT-VALUE
J17582         PERFORM 615-END-ELEMENT-TAG
J17582         MOVE WFCHWS-TAG-STRING      TO BTL1-TAG-LINE
J17582         MOVE BROADCAST-TAG-LINE-1   TO RPT-GROUP-REQUEST
J17582         PERFORM 700-PRINT-RPT-GRP
J17582     END-IF.

011910 5100-END.

008300******************************************************************
008400 5125-DO-PROC-LEVEL.
008500******************************************************************
008600
008700     MOVE SRT-PROC-LEVEL         TO WS-PROC-LEVEL
008900                                    G2-PROC-LEVEL
008900                                    E2-PROC-LEVEL
008900                                    W2-PROC-LEVEL
008900                                    DB-PROCESS-LEVEL.
008700     MOVE SRT-DEPARTMENT         TO WS-DEPARTMENT
008900                                    G2-DEPARTMENT
008900                                    E2-DEPARTMENT
008900                                    W2-DEPARTMENT
008900                                    DB-DEPARTMENT.
           PERFORM 840-FIND-PRSSET1.
           PERFORM 840-FIND-DPTSET1.
           MOVE PRS-NAME               TO G2-PROC-LEVEL-NAME
                                          G2-PROC-LEVEL-NAME.
           INITIALIZE G2-DEPARTMENT-NAME
                      E2-DEPARTMENT-NAME
                      W2-DEPARTMENT-NAME.
           IF (DEPTCODE-FOUND)
               MOVE DPT-NAME           TO G2-DEPARTMENT-NAME
                                          G2-DEPARTMENT-NAME.

J17582     IF  (PRBRD-BROADCAST-ENABLED)
J17582          MOVE 501                    TO PRBRD-ELEMENT-TYPE
J17582          MOVE WS-PROC-LEVEL          TO PRBRD-ELEMENT-VALUE
J17582          PERFORM 610-BEG-ELEMENT-TAG
J17582          MOVE WFCHWS-TAG-STRING      TO BTL1-TAG-LINE
J17582          MOVE BROADCAST-TAG-LINE-1   TO RPT-GROUP-REQUEST
J17582          PERFORM 700-PRINT-RPT-GRP
J17582     END-IF.
J17582
           MOVE GN2-PROC-LEVEL         TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

P09149     MOVE WS-FALSE               TO WS-PL-ERR-SW
                                          WS-PL-WARN-SW.

P09149     MOVE SRT-COMPANY            TO WF-COMPANY.
P09149     INITIALIZE WF-EMPLOYEE.
P09149     INITIALIZE WF-EFFECT-DATE.
P09149     INITIALIZE WF-REC-TYPE.
P09149     INITIALIZE WF-SEQ-NBR.
P09149     PERFORM 8500-FIND-NLT-BN100WORK.
P09149     PERFORM 
P09149         UNTIL  (BN100WORK-NOTFOUND)
P09149            OR  (WF-COMPANY       NOT = SRT-COMPANY)
P09149            OR ((PL-HAS-ERROR)
                  AND (PL-HAS-WARN))
P09149                IF  (WF-PROC-LEVEL    = SRT-PROC-LEVEL)
P09149                AND (WF-DEPARTMENT    = SRT-DEPARTMENT)
                          IF  (WF-WARNING-NBR NOT = ZEROES)
                              MOVE WS-TRUE TO WS-PL-WARN-SW
                          ELSE
P09149                        IF  (WF-ERROR-NBR NOT = ZEROES)
P09149                        AND (WF-ERROR-CAT NOT = SPACES)
                                  MOVE WS-TRUE TO WS-PL-ERR-SW
P09149                        END-IF
                          END-IF
P09149                END-IF
P09149                PERFORM 8600-FIND-NEXT-BN100WORK
P09149     END-PERFORM.

P09149     IF (PL-HAS-ERROR)
P09149         MOVE EN2-PROC-LEVEL         TO RPT-GROUP-REQUEST
P09149         PERFORM 700-PRINT-RPT-GRP.
009000
           IF (PL-HAS-WARN)
               MOVE WN2-PROC-LEVEL         TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP.
      
009100     PERFORM 5150-DO-BNG-EMPLOYEE
009200     THRU    5150-END
009300         UNTIL (END-OF-FILE)
009400         OR    (SRT-COMPANY      NOT = PRM-COMPANY)
009400         OR    (SRT-PROC-LEVEL   NOT = WS-PROC-LEVEL)
009400         OR    (SRT-DEPARTMENT   NOT = WS-DEPARTMENT).
J17582
J17582     IF  (PRBRD-BROADCAST-ENABLED)
J17582          MOVE 501                    TO PRBRD-ELEMENT-TYPE
J17582          MOVE WS-PROC-LEVEL          TO PRBRD-ELEMENT-VALUE
J17582          PERFORM 615-END-ELEMENT-TAG
J17582          MOVE WFCHWS-TAG-STRING      TO BTL1-TAG-LINE
J17582          MOVE BROADCAST-TAG-LINE-1   TO RPT-GROUP-REQUEST
J17582          PERFORM 700-PRINT-RPT-GRP
J17582     END-IF.
009600
009700 5125-END.
009800
011700******************************************************************
011900 5150-DO-BNG-EMPLOYEE.
011700******************************************************************

           MOVE SRT-EMPLOYEE           TO WS-EMPLOYEE
                                          G2-BNG-EMPLOYEE
                                          E2-BNG-EMPLOYEE
                                          W2-BNG-EMPLOYEE
                                          DB-EMPLOYEE.
           PERFORM 840-FIND-EMPSET1.
           PERFORM 840-FIND-PEMSET1.

           MOVE EMP-LAST-NAME          TO HRWS-LAST-NAME.
           MOVE EMP-FIRST-NAME         TO HRWS-FIRST-NAME.
           MOVE EMP-MIDDLE-NAME        TO HRWS-MIDDLE-NAME.
           MOVE EMP-LAST-NAME-PRE      TO HRWS-LAST-NAME-PRE.
           MOVE EMP-NAME-SUFFIX        TO HRWS-NAME-SUFFIX.
           PERFORM 750-HR-FORMAT-NAME.
           MOVE HRWS-FORMAT-NAME       TO G2-EMP-NAME
                                          E2-EMP-NAME
                                          W2-EMP-NAME.

J17582     IF  (PRBRD-BROADCAST-ENABLED)
J17582          MOVE 503                    TO PRBRD-ELEMENT-TYPE
J17582          MOVE WS-EMPLOYEE            TO PRBRD-ELEMENT-VALUE
J17582          PERFORM 610-BEG-ELEMENT-TAG
J17582          MOVE WFCHWS-TAG-STRING      TO BTL1-TAG-LINE
J17582          MOVE BROADCAST-TAG-LINE-1   TO RPT-GROUP-REQUEST
J17582          PERFORM 700-PRINT-RPT-GRP
J17582     END-IF.
J17582
P44626     IF (PRM-PRINT-LOG           = "N")
P44626         MOVE GN2-BNG-EMPLOYEE   TO RPT-GROUP-REQUEST
P44626         PERFORM 700-PRINT-RPT-GRP.

P09149     MOVE WS-FALSE               TO WS-EMP-ERR-SW
                                          WS-EMP-WARN-SW.
P09149     MOVE SRT-COMPANY            TO WF-COMPANY.
P09149     MOVE SRT-EMPLOYEE           TO WF-EMPLOYEE.
P09149     INITIALIZE WF-EFFECT-DATE.
P09149     INITIALIZE WF-REC-TYPE.
P09149     INITIALIZE WF-SEQ-NBR.
P09149     PERFORM 8500-FIND-NLT-BN100WORK.
P09149     PERFORM 
P09149         UNTIL  (BN100WORK-NOTFOUND)
P09149            OR  (WF-COMPANY       NOT = SRT-COMPANY)
P09149            OR  (WF-EMPLOYEE      NOT = SRT-EMPLOYEE)
P09149            OR ((EMP-HAS-ERROR)
                  AND (EMP-HAS-WARN))
                      IF  (WF-WARNING-NBR NOT = ZEROES)
                          MOVE WS-TRUE TO WS-EMP-WARN-SW
                      ELSE
P09149                    IF  (WF-ERROR-NBR NOT = ZEROES)
P09149                    AND (WF-ERROR-CAT NOT = SPACES)
P09149                        MOVE WS-TRUE TO WS-EMP-ERR-SW
                          END-IF
                      END-IF
P09149                PERFORM 8600-FIND-NEXT-BN100WORK
P09149     END-PERFORM.

          IF (EMP-HAS-WARN)
              MOVE WN2-BNG-EMPLOYEE       TO RPT-GROUP-REQUEST
              PERFORM 700-PRINT-RPT-GRP.
                     
P09149    IF (EMP-HAS-ERROR)
P09149        MOVE EN2-BNG-EMPLOYEE       TO RPT-GROUP-REQUEST
P09149        PERFORM 700-PRINT-RPT-GRP.
004340
P43669     MOVE WS-FALSE                  TO WS-G3-ARRAY-FULL-SW.
           PERFORM 5200-DO-BNG-EFFECT-DATE
           THRU    5200-END
               UNTIL (END-OF-FILE)
               OR    (SRT-COMPANY      NOT = WS-COMPANY)
009400         OR    (SRT-PROC-LEVEL   NOT = WS-PROC-LEVEL)
009400         OR    (SRT-DEPARTMENT   NOT = WS-DEPARTMENT)
P43669         OR    (SRT-EMPLOYEE     NOT = WS-EMPLOYEE)
P43669         OR    (WS-G3-ARRAY-FULL).
P43669
P43669     IF  (WS-G3-ARRAY-FULL)
P43669         MOVE 171                        TO CRT-MSG-NBR
P43669         PERFORM 780-PRINT-MSG
P43669         PERFORM
P43669             UNTIL (END-OF-FILE)
P43669             OR    (SRT-COMPANY      NOT = WS-COMPANY)
P43669             OR    (SRT-PROC-LEVEL   NOT = WS-PROC-LEVEL)
P43669             OR    (SRT-DEPARTMENT   NOT = WS-DEPARTMENT)
P43669             OR    (SRT-EMPLOYEE     NOT = WS-EMPLOYEE)
P43669             READ BN100WORKS-FILE      INTO SRT-BN100WORKS-REC
P43669               AT END
P43669              SET END-OF-FILE            TO TRUE
P43669         END-PERFORM
P43669     END-IF.
J17582
J17582     IF  (PRBRD-BROADCAST-ENABLED)
J17582          MOVE 503                    TO PRBRD-ELEMENT-TYPE
J17582          MOVE WS-EMPLOYEE            TO PRBRD-ELEMENT-VALUE
J17582          PERFORM 615-END-ELEMENT-TAG
J17582          MOVE WFCHWS-TAG-STRING      TO BTL1-TAG-LINE
J17582          MOVE BROADCAST-TAG-LINE-1   TO RPT-GROUP-REQUEST
J17582          PERFORM 700-PRINT-RPT-GRP
J17582     END-IF.

011910 5150-END.

011700******************************************************************
       5200-DO-BNG-EFFECT-DATE.
011700******************************************************************

           MOVE SRT-EFFECT-DATE        TO WS-EFFECT-DATE.

           INITIALIZE WS-SAVE-PLAN-TYPE
                      WS-SAVE-PLAN-CODE
                      WS-SAVE-START-DATE.

P09149     MOVE WS-FALSE               TO WS-EFF-ERR-SW 
                                          WS-EFF-WARN-SW.
P09149     MOVE SRT-COMPANY            TO WF-COMPANY.
P09149     MOVE SRT-EMPLOYEE           TO WF-EMPLOYEE.
P09149     MOVE SRT-EFFECT-DATE        TO WF-EFFECT-DATE.
P09149     INITIALIZE WF-REC-TYPE
P09149                WF-SEQ-NBR.
P09149     PERFORM 8500-FIND-NLT-BN100WORK.
P09149     PERFORM 
P09149         UNTIL (BN100WORK-NOTFOUND)
P09149            OR (WF-COMPANY       NOT = SRT-COMPANY)
P09149            OR (WF-EMPLOYEE      NOT = SRT-EMPLOYEE)
P09149            OR (WF-EFFECT-DATE   NOT = SRT-EFFECT-DATE)
P09149            OR ((EFF-HAS-ERROR)
                  AND (EFF-HAS-WARN))
                      IF  (WF-WARNING-NBR NOT = ZEROES)
                          MOVE WS-TRUE TO WS-EFF-WARN-SW
                      ELSE
P09149                    IF  (WF-ERROR-NBR NOT = ZEROES)
P09149                    AND (WF-ERROR-CAT NOT = SPACES)
P09149                        MOVE WS-TRUE TO WS-EFF-ERR-SW
P09149                    END-IF
P09149                END-IF
P09149                PERFORM 8600-FIND-NEXT-BN100WORK
P09149     END-PERFORM.

           PERFORM 5440-INITIALIZE-WS-G3
           THRU    5440-END
               VARYING WS-I6 FROM 1 BY 1
P43669         UNTIL  (WS-I6 > 99).
           MOVE 1 TO WS-I6.
                
           PERFORM 5300-DO-BNG-REC-TYPE
           THRU    5300-END
               UNTIL (END-OF-FILE)
               OR    (SRT-COMPANY      NOT = WS-COMPANY)
009400         OR    (SRT-PROC-LEVEL   NOT = WS-PROC-LEVEL)
009400         OR    (SRT-DEPARTMENT   NOT = WS-DEPARTMENT)
               OR    (SRT-EMPLOYEE     NOT = WS-EMPLOYEE)
P43669         OR    (SRT-EFFECT-DATE  NOT = WS-EFFECT-DATE)
P43669         OR    (WS-G3-ARRAY-FULL).

       5200-END.

011700******************************************************************
       5300-DO-BNG-REC-TYPE.
011700******************************************************************

           SET NO-PRINT-PF-MSG             TO TRUE.
           SET NO-PRINT-BD-MSG             TO TRUE.
           SET NO-PRINT-SD-MSG             TO TRUE.
           SET NO-PRINT-SF-MSG             TO TRUE.

           IF (PRM-PRINT-LOG               = "Y")
               PERFORM 5400-DO-BNG-N-BNH
               THRU    5400-END
                   UNTIL (END-OF-FILE)
                   OR    (SRT-COMPANY      NOT = WS-COMPANY)
                   OR    (SRT-PROC-LEVEL   NOT = WS-PROC-LEVEL)
                   OR    (SRT-DEPARTMENT   NOT = WS-DEPARTMENT)
                   OR    (SRT-EMPLOYEE     NOT = WS-EMPLOYEE)
                   OR    (SRT-EFFECT-DATE  NOT = WS-EFFECT-DATE)
                   OR    (SRT-REC-TYPE     = "3")
P43669             OR    (WS-G3-ARRAY-FULL)
           ELSE
               PERFORM
                   UNTIL (END-OF-FILE)
                   OR    (SRT-COMPANY      NOT = WS-COMPANY)
                   OR    (SRT-PROC-LEVEL   NOT = WS-PROC-LEVEL)
                   OR    (SRT-DEPARTMENT   NOT = WS-DEPARTMENT)
                   OR    (SRT-EMPLOYEE     NOT = WS-EMPLOYEE)
                   OR    (SRT-EFFECT-DATE  NOT = WS-EFFECT-DATE)
                   OR    (SRT-REC-TYPE     = "3")

                   IF (SRT-FILE-PREFIX     = "BNH")
                       PERFORM 5350-SET-PF-BD-SD-SF-SWITCH
                       THRU    5350-END
                   END-IF
                
005600             READ BN100WORKS-FILE    INTO SRT-BN100WORKS-REC
003580                 AT END
003560                     SET END-OF-FILE TO TRUE
                   END-READ
               END-PERFORM.

           IF  (SRT-COMPANY                = WS-COMPANY)
           AND (SRT-PROC-LEVEL             = WS-PROC-LEVEL)
           AND (SRT-DEPARTMENT             = WS-DEPARTMENT)
           AND (SRT-EMPLOYEE               = WS-EMPLOYEE)
           AND (SRT-EFFECT-DATE            = WS-EFFECT-DATE)
           AND (SRT-REC-TYPE               = "3")

               IF (EFF-HAS-WARN)
                   MOVE WN4-DETAIL-HEADING     TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
               END-IF
P09149         IF (EFF-HAS-ERROR)
P09149             MOVE EN4-DETAIL-HEADING     TO RPT-GROUP-REQUEST
P09149             PERFORM 700-PRINT-RPT-GRP
               END-IF
           END-IF.

           PERFORM 5500-DO-BEN-EFD-PLN
           THRU    5500-END
               UNTIL (END-OF-FILE)
               OR    (SRT-COMPANY          NOT = WS-COMPANY)
               OR    (SRT-PROC-LEVEL       NOT = WS-PROC-LEVEL)
               OR    (SRT-DEPARTMENT       NOT = WS-DEPARTMENT)
               OR    (SRT-EMPLOYEE         NOT = WS-EMPLOYEE)
               OR    (SRT-EFFECT-DATE      NOT = WS-EFFECT-DATE)
               OR    (SRT-REC-TYPE         NOT = "3").

004320     MOVE GN6-BLANK-LINE             TO RPT-GROUP-REQUEST.
004330     PERFORM 700-PRINT-RPT-GRP.

P09149     IF (EFF-HAS-ERROR)
P09149         MOVE EN6-BLANK-LINE             TO RPT-GROUP-REQUEST
P09149         PERFORM 700-PRINT-RPT-GRP.

           IF (EFF-HAS-WARN)
               MOVE WN6-BLANK-LINE             TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP.

           PERFORM 5375-PRINT-PF-BD-SD-SF-MSG
           THRU    5375-END.

       5300-END.

011700******************************************************************
       5350-SET-PF-BD-SD-SF-SWITCH.
011700******************************************************************

           MOVE SRT-COMPANY            TO DB-COMPANY.
           MOVE SRT-EMPLOYEE           TO DB-EMPLOYEE.
           MOVE SRT-EFFECT-DATE        TO DB-EFFECT-DATE.
           MOVE SRT-BNG-BNH-SEQ-NBR    TO DB-SEQ-NBR.
           PERFORM 840-FIND-BNHSET1.
           IF  (BNH-CHANGE-TYPE        = "3")
           AND (SRT-ERROR-NBR          NOT = ZEROES)
               SET PRINT-PF-MSG        TO TRUE
           ELSE
           IF (BNH-CHANGE-TYPE         = "4")
               SET PRINT-BD-MSG        TO TRUE
           ELSE
           IF (BNH-CHANGE-TYPE         = "5")
               SET PRINT-SD-MSG        TO TRUE
           ELSE
           IF (BNH-CHANGE-TYPE         = "6")
               SET PRINT-SF-MSG        TO TRUE.

       5350-END.

011700******************************************************************
       5375-PRINT-PF-BD-SD-SF-MSG.
011700******************************************************************

           IF (PRINT-PF-MSG)
003820         MOVE SRT-ERROR-NBR      TO CRT-MSG-NBR
003830         PERFORM 790-GET-MSG
003790         MOVE CRT-MESSAGE        TO G5-MESSAGE

004320         MOVE GN5-MESSAGE        TO RPT-GROUP-REQUEST
004330         PERFORM 700-PRINT-RPT-GRP.

           IF (PRINT-BD-MSG)
PERF           MOVE WS-BN100-MSG-114   TO G5-MESSAGE

004320         MOVE GN5-MESSAGE        TO RPT-GROUP-REQUEST
004330         PERFORM 700-PRINT-RPT-GRP.

           IF (PRINT-SD-MSG)
PERF           MOVE WS-BN100-MSG-115   TO G5-MESSAGE

004320         MOVE GN5-MESSAGE        TO RPT-GROUP-REQUEST
004330         PERFORM 700-PRINT-RPT-GRP.

           IF (PRINT-SF-MSG)
PERF           MOVE WS-BN100-MSG-130   TO G5-MESSAGE

004320         MOVE GN5-MESSAGE        TO RPT-GROUP-REQUEST
004330         PERFORM 700-PRINT-RPT-GRP.

           IF (PRINT-BD-MSG)
           OR (PRINT-SD-MSG)
           OR (PRINT-SF-MSG)
004320         MOVE GN6-BLANK-LINE     TO RPT-GROUP-REQUEST
004330         PERFORM 700-PRINT-RPT-GRP.

       5375-END.

011700******************************************************************
       5400-DO-BNG-N-BNH.
011700******************************************************************

PERF       MOVE WS-BNMSG-MSG-190       TO WS-G3-EFFECT-MESSAGE (WS-I6)
                                          E3-EFFECT-MESSAGE
                                          W3-EFFECT-MESSAGE.

PERF       MOVE WS-BNMSG-MSG-191       TO WS-G3-ACTION (WS-I6)
                                          E3-ACTION
                                          W3-ACTION.

           MOVE SRT-EFFECT-DATE        TO WS-G3-EFFECT-DATE (WS-I6)
                                          E3-EFFECT-DATE
                                          W3-EFFECT-DATE
                                          DB-EFFECT-DATE.

           IF (SRT-ACT-OBJ-ID          NOT = ZEROES)
               MOVE SRT-ACT-OBJ-ID     TO DB-OBJ-ID
               PERFORM 840-FIND-PAHSET2

               IF (PERSACTHST-FOUND)
                   MOVE PAH-ACTION-CODE   
                                        TO WS-G3-PAH-ACTION-CODE (WS-I6)
                                           E3-PAH-ACTION-CODE
                                           W3-PAH-ACTION-CODE
               ELSE
                   INITIALIZE WS-G3-PAH-ACTION-CODE (WS-I6)
                              E3-PAH-ACTION-CODE
                              W3-PAH-ACTION-CODE.

           IF (SRT-REC-TYPE                = "1")
               INITIALIZE WS-GROUP-CNT
                          WS-ADD-DELETE
               PERFORM 5410-MOVE-GROUP
               THRU    5410-END
                   UNTIL (END-OF-FILE)
                   OR    (SRT-COMPANY      NOT = WS-COMPANY)
                   OR    (SRT-PROC-LEVEL   NOT = WS-PROC-LEVEL)
                   OR    (SRT-DEPARTMENT   NOT = WS-DEPARTMENT)
                   OR    (SRT-EMPLOYEE     NOT = WS-EMPLOYEE)
                   OR    (SRT-EFFECT-DATE  NOT = WS-EFFECT-DATE)
                   OR    (SRT-REC-TYPE     NOT = "1")
P43669             OR    (WS-G3-ARRAY-FULL)

               IF (WS-GROUP-CNT                NOT = ZEROES)
004340
                   IF (EFF-HAS-WARN)
                       MOVE WN3-BNG-EFFECT-DATE TO RPT-GROUP-REQUEST
                       PERFORM 700-PRINT-RPT-GRP
                   END-IF
P09149             IF (EFF-HAS-ERROR)
P09149                 MOVE EN3-BNG-EFFECT-DATE TO RPT-GROUP-REQUEST
P09149                 PERFORM 700-PRINT-RPT-GRP
P09149             END-IF.
004340
           IF (SRT-REC-TYPE                = "2")
               INITIALIZE WS-GROUP-CNT
                          WS-CHANGE-TYPE
               PERFORM 5420-MOVE-CHANGE
               THRU    5420-END
                   UNTIL (END-OF-FILE)
                   OR    (SRT-COMPANY      NOT = WS-COMPANY)
                   OR    (SRT-PROC-LEVEL   NOT = WS-PROC-LEVEL)
                   OR    (SRT-DEPARTMENT   NOT = WS-DEPARTMENT)
                   OR    (SRT-EMPLOYEE     NOT = WS-EMPLOYEE)
                   OR    (SRT-EFFECT-DATE  NOT = WS-EFFECT-DATE)
                   OR    (SRT-REC-TYPE     NOT = "2")
P43669             OR    (WS-G3-ARRAY-FULL)

               IF (WS-GROUP-CNT                NOT = ZEROES)
                   IF (EFF-HAS-WARN)
                       MOVE WN3-BNG-EFFECT-DATE TO RPT-GROUP-REQUEST
                       PERFORM 700-PRINT-RPT-GRP
                   END-IF
004340
P09149             IF (EFF-HAS-ERROR)
P09149                 MOVE EN3-BNG-EFFECT-DATE TO RPT-GROUP-REQUEST
P09149                 PERFORM 700-PRINT-RPT-GRP
P09149             END-IF.

P44035     PERFORM 5900-HEADING
P44035     THRU    5900-END.
P44035
           PERFORM 5430-INITIALIZE-E3W3
           THRU    5430-END.
004340
       5400-END.

011700******************************************************************
       5410-MOVE-GROUP.
011700******************************************************************

           MOVE SRT-BNG-BNH-SEQ-NBR    TO DB-SEQ-NBR.
           PERFORM 840-FIND-BNGSET1.

           IF  (WS-ADD-DELETE          NOT = SPACES)
           AND (WS-ADD-DELETE          NOT = BNG-ADD-DELETE)
               INITIALIZE WS-GROUP-CNT

               IF (EFF-HAS-WARN)
                   MOVE WN3-BNG-EFFECT-DATE TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
               END-IF
004340
P09149         IF (EFF-HAS-ERROR)
P09149             MOVE EN3-BNG-EFFECT-DATE TO RPT-GROUP-REQUEST
P09149             PERFORM 700-PRINT-RPT-GRP
P09149         END-IF

               PERFORM 5430-INITIALIZE-E3W3
               THRU    5430-END.
      
           IF  (WS-G3-ADD-DEL-CHANGE (WS-I6) NOT = BNG-ADD-DELETE)
           AND (WS-G3-ADD-DEL-CHANGE (WS-I6) NOT = SPACES)
               ADD 1 TO WS-I6
           END-IF.

P43669     IF  (WS-I6 > 99)
P43669         MOVE WS-TRUE             TO WS-G3-ARRAY-FULL-SW
P43669         GO TO 5410-END
P43669     END-IF.
P43669
           MOVE BNG-ADD-DELETE         TO WS-G3-ADD-DEL-CHANGE (WS-I6)
                                          E3-ADD-DEL-CHANGE
                                          W3-ADD-DEL-CHANGE
                                          WS-ADD-DELETE.

           ADD 1                       TO WS-GROUP-CNT.

           IF (WS-GROUP-CNT            = 1)
               MOVE BNG-GROUP-NAME     TO WS-G3-GRP-OR-CHANGE-1 (WS-I6)
                                          E3-GRP-OR-CHANGE-1
                                          W3-GRP-OR-CHANGE-1
           ELSE
           IF (WS-GROUP-CNT            = 2)
               MOVE BNG-GROUP-NAME     TO WS-G3-GRP-OR-CHANGE-2 (WS-I6)
                                          E3-GRP-OR-CHANGE-2
                                          W3-GRP-OR-CHANGE-2
           ELSE
           IF (WS-GROUP-CNT            = 3)
               MOVE BNG-GROUP-NAME     TO WS-G3-GRP-OR-CHANGE-3 (WS-I6)
                                          E3-GRP-OR-CHANGE-3
                                          W3-GRP-OR-CHANGE-3
           ELSE
           IF (WS-GROUP-CNT            = 4)
               MOVE BNG-GROUP-NAME     TO WS-G3-GRP-OR-CHANGE-4 (WS-I6)
                                          E3-GRP-OR-CHANGE-4
                                          W3-GRP-OR-CHANGE-4
           ELSE
           IF (WS-GROUP-CNT            = 5)
               MOVE BNG-GROUP-NAME     TO WS-G3-GRP-OR-CHANGE-5 (WS-I6)
                                          E3-GRP-OR-CHANGE-5
                                          W3-GRP-OR-CHANGE-5
           ELSE
           IF (WS-GROUP-CNT            = 6)
               MOVE BNG-GROUP-NAME     TO WS-G3-GRP-OR-CHANGE-6 (WS-I6)
                                          E3-GRP-OR-CHANGE-6
                                          W3-GRP-OR-CHANGE-6
               INITIALIZE WS-GROUP-CNT

               IF (EFF-HAS-WARN)
                   MOVE WN3-BNG-EFFECT-DATE TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
               END-IF
004340
P09149         IF (EFF-HAS-ERROR)
P09149             MOVE EN3-BNG-EFFECT-DATE TO RPT-GROUP-REQUEST
P09149             PERFORM 700-PRINT-RPT-GRP
P09149         END-IF

               PERFORM 5430-INITIALIZE-E3W3
               THRU    5430-END

               ADD 1 TO WS-I6.


       5410-NEXT-BN100WORKS.
005600     READ BN100WORKS-FILE        INTO SRT-BN100WORKS-REC
003580         AT END
003560             SET END-OF-FILE     TO TRUE.
006000

       5410-END.

011700******************************************************************
       5420-MOVE-CHANGE.
011700******************************************************************

           MOVE SRT-BNG-BNH-SEQ-NBR    TO DB-SEQ-NBR.
           PERFORM 840-FIND-BNHSET1.
      
           IF  (WS-G3-ADD-DEL-CHANGE (WS-I6) NOT = BNH-CHANGE-TYPE)
           AND (WS-G3-ADD-DEL-CHANGE (WS-I6) NOT = SPACES)
               ADD 1 TO WS-I6
           END-IF.

P43669     IF  (WS-I6 > 99)
P43669         MOVE WS-TRUE             TO WS-G3-ARRAY-FULL-SW
P43669         GO TO 5420-END
P43669     END-IF.
P43669
          MOVE "C"                     TO WS-G3-ADD-DEL-CHANGE (WS-I6)
                                          E3-ADD-DEL-CHANGE
                                          W3-ADD-DEL-CHANGE.

           IF (WS-CHANGE-TYPE              NOT = BNH-CHANGE-TYPE)
               MOVE BNH-CHANGE-TYPE        TO WS-CHANGE-TYPE
           ELSE
               IF (EFF-HAS-WARN)
                   MOVE WN3-BNG-EFFECT-DATE    TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
               END-IF

P09149         IF (EFF-HAS-ERROR)
P09149             MOVE EN3-BNG-EFFECT-DATE    TO RPT-GROUP-REQUEST
P09149             PERFORM 700-PRINT-RPT-GRP
P09149         END-IF

               PERFORM 5430-INITIALIZE-E3W3
               THRU    5430-END

               MOVE "C"                  TO WS-G3-ADD-DEL-CHANGE (WS-I6)
                                            E3-ADD-DEL-CHANGE
                                            W3-ADD-DEL-CHANGE
               INITIALIZE WS-GROUP-CNT

PERF           MOVE WS-BNMSG-MSG-191       TO WS-G3-ACTION (WS-I6)

               IF (SRT-ACT-OBJ-ID          NOT = ZEROES)
                   MOVE SRT-ACT-OBJ-ID     TO DB-OBJ-ID
                   PERFORM 840-FIND-PAHSET2

                   IF (PERSACTHST-FOUND)
                      MOVE PAH-ACTION-CODE
                                        TO WS-G3-PAH-ACTION-CODE (WS-I6)
                                           E3-PAH-ACTION-CODE
                                           W3-PAH-ACTION-CODE
                   ELSE
                       INITIALIZE WS-G3-PAH-ACTION-CODE (WS-I6)
                                  E3-PAH-ACTION-CODE 
                                  W3-PAH-ACTION-CODE.

           PERFORM 5422-GET-MESSAGE
           THRU    5422-END.

           IF (WS-GROUP-CNT            = 1)
               MOVE CRT-MESSAGE        TO WS-G3-GRP-OR-CHANGE-1 (WS-I6)
                                          E3-GRP-OR-CHANGE-1
                                          W3-GRP-OR-CHANGE-1
           ELSE
           IF (WS-GROUP-CNT            = 2)
               MOVE CRT-MESSAGE        TO WS-G3-GRP-OR-CHANGE-2 (WS-I6)
                                          E3-GRP-OR-CHANGE-2
                                          W3-GRP-OR-CHANGE-2
           ELSE
           IF (WS-GROUP-CNT            = 3)
               MOVE CRT-MESSAGE        TO WS-G3-GRP-OR-CHANGE-3 (WS-I6)
                                          E3-GRP-OR-CHANGE-3
                                          W3-GRP-OR-CHANGE-3
           ELSE
           IF (WS-GROUP-CNT            = 4)
               MOVE CRT-MESSAGE        TO WS-G3-GRP-OR-CHANGE-4 (WS-I6)
                                          E3-GRP-OR-CHANGE-4
                                          W3-GRP-OR-CHANGE-4
           ELSE
           IF (WS-GROUP-CNT            = 5)
               MOVE CRT-MESSAGE        TO WS-G3-GRP-OR-CHANGE-5 (WS-I6)
                                          E3-GRP-OR-CHANGE-5
                                          W3-GRP-OR-CHANGE-5
           ELSE
           IF (WS-GROUP-CNT            = 6)
               MOVE CRT-MESSAGE        TO WS-G3-GRP-OR-CHANGE-6 (WS-I6)
                                          E3-GRP-OR-CHANGE-6
                                          W3-GRP-OR-CHANGE-6
               INITIALIZE WS-GROUP-CNT

               IF (EFF-HAS-WARN)
                   MOVE WN3-BNG-EFFECT-DATE TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
               END-IF

P09149         IF (EFF-HAS-ERROR)
P09149             MOVE EN3-BNG-EFFECT-DATE TO RPT-GROUP-REQUEST
P09149             PERFORM 700-PRINT-RPT-GRP
P09149         END-IF

               PERFORM 5430-INITIALIZE-E3W3
               THRU    5430-END

               ADD 1 TO WS-I6.

004340
       5420-NEXT-BN100WORKS.
005600     READ BN100WORKS-FILE        INTO SRT-BN100WORKS-REC
003580         AT END
003560             SET END-OF-FILE     TO TRUE.

       5420-END.

011700******************************************************************
       5422-GET-MESSAGE.
011700******************************************************************

           IF (BNH-CHANGE-TYPE         = "1")
PERF           MOVE WS-BNMSG-MSG-192   TO CRT-MESSAGE
           ELSE
           IF (BNH-CHANGE-TYPE         = "2")
PERF           MOVE WS-BNMSG-MSG-193   TO CRT-MESSAGE
           ELSE
           IF (BNH-CHANGE-TYPE         = "3")
PERF           MOVE WS-BNMSG-MSG-194   TO CRT-MESSAGE
               IF (SRT-ERROR-NBR       NOT = ZEROES)
                   SET PRINT-PF-MSG    TO TRUE
               END-IF
           ELSE
           IF (BNH-CHANGE-TYPE         = "4")
PERF           MOVE WS-BNMSG-MSG-195   TO CRT-MESSAGE
003820         SET PRINT-BD-MSG        TO TRUE
           ELSE
           IF (BNH-CHANGE-TYPE         = "5")
PERF           MOVE WS-BNMSG-MSG-196   TO CRT-MESSAGE
003820         SET PRINT-SD-MSG        TO TRUE
           ELSE
           IF (BNH-CHANGE-TYPE         = "6")
PERF           MOVE WS-BNMSG-MSG-198   TO CRT-MESSAGE
003820         SET PRINT-SF-MSG        TO TRUE
           ELSE
           IF (BNH-CHANGE-TYPE         = "7")
PERF           MOVE WS-BNMSG-MSG-203   TO CRT-MESSAGE.

           ADD 1                       TO WS-GROUP-CNT.

       5422-END.

011700******************************************************************
       5430-INITIALIZE-E3W3.
011700******************************************************************

           INITIALIZE E3-EFFECT-MESSAGE
                      E3-ACTION
                      E3-EFFECT-DATE
                      E3-PAH-ACTION-CODE
                      E3-ADD-DEL-CHANGE
                      E3-GRP-OR-CHANGE-1
                      E3-GRP-OR-CHANGE-2
                      E3-GRP-OR-CHANGE-3
                      E3-GRP-OR-CHANGE-4
                      E3-GRP-OR-CHANGE-5
                      E3-GRP-OR-CHANGE-6.

           INITIALIZE W3-EFFECT-MESSAGE
                      W3-ACTION
                      W3-EFFECT-DATE
                      W3-PAH-ACTION-CODE
                      W3-ADD-DEL-CHANGE
                      W3-GRP-OR-CHANGE-1
                      W3-GRP-OR-CHANGE-2
                      W3-GRP-OR-CHANGE-3
                      W3-GRP-OR-CHANGE-4
                      W3-GRP-OR-CHANGE-5
                      W3-GRP-OR-CHANGE-6.

       5430-END.

011700******************************************************************
       5440-INITIALIZE-WS-G3.
011700******************************************************************

PERF       MOVE ZEROES                 TO WS-G3-EFFECT-DATE     (WS-I6).
PERF       MOVE SPACES                 TO WS-G3-EFFECT-MESSAGE  (WS-I6)
PERF                                      WS-G3-ACTION          (WS-I6)
PERF                                      WS-G3-PAH-ACTION-CODE (WS-I6)
PERF                                      WS-G3-ADD-DEL-CHANGE  (WS-I6)
PERF                                      WS-G3-GRP-OR-CHANGE-1 (WS-I6)
PERF                                      WS-G3-GRP-OR-CHANGE-2 (WS-I6)
PERF                                      WS-G3-GRP-OR-CHANGE-3 (WS-I6)
PERF                                      WS-G3-GRP-OR-CHANGE-4 (WS-I6)
PERF                                      WS-G3-GRP-OR-CHANGE-5 (WS-I6)
PERF                                      WS-G3-GRP-OR-CHANGE-6 (WS-I6).

           IF (WS-I6 = 1)
PERF          MOVE ZEROES              TO G3-EFFECT-DATE
PERF          MOVE SPACES              TO G3-EFFECT-MESSAGE
PERF                                      G3-ACTION
PERF                                      G3-PAH-ACTION-CODE
PERF                                      G3-ADD-DEL-CHANGE
PERF                                      G3-GRP-OR-CHANGE-1
PERF                                      G3-GRP-OR-CHANGE-2
PERF                                      G3-GRP-OR-CHANGE-3
PERF                                      G3-GRP-OR-CHANGE-4
PERF                                      G3-GRP-OR-CHANGE-5.
PERF

       5440-END.

011700******************************************************************
       5500-DO-BEN-EFD-PLN.
011700******************************************************************

           INITIALIZE G4-PROC-MESSAGE
                      G4-PLAN-TYPE
                      G4-PLAN-CODE
                      G4-PLN-DESC
                      G4-START-DATE
                      G4-STOP-DATE
                      G4-BEN-COVER-AMT
                      G4-BEN-CONT-TYPE
                      G4-BEN-CONT-AMT
                      G4-PLN-DED-CODE
                      G4-EDM-NEXT-AMOUNT
                      G4-ERROR-MESSAGE.

           IF (SRT-FILE-PREFIX         = "EFD")
               PERFORM 5600-DO-EMPFLEXDOL
               THRU    5600-END
           ELSE
           IF (SRT-FILE-PREFIX         = "BEN")
               PERFORM 5700-DO-BENEFIT
               THRU    5700-END
           ELSE
           IF (SRT-FILE-PREFIX         = "PLN")
               PERFORM 5800-DO-PLAN
               THRU    5800-END.

       5500-END.

011700******************************************************************
       5600-DO-EMPFLEXDOL.
011700******************************************************************

           MOVE SRT-COMPANY            TO DB-COMPANY
                                          WF3-COMPANY
                                          WF4-COMPANY.
           MOVE SRT-EMPLOYEE           TO DB-EMPLOYEE
                                          WF3-EMPLOYEE
                                          WF4-EMPLOYEE.
           MOVE SRT-START-DATE         TO DB-START-DATE
                                          WF3-START-DATE
                                          WF4-START-DATE.
           MOVE SRT-PLAN-CODE          TO DB-FLEX-PLAN.

           PERFORM 840-FIND-FLPSET1.

           INITIALIZE G4-PROC-MESSAGE
                      G4-PLAN-TYPE
                      G4-PLAN-CODE
                      G4-PLN-DESC.

           IF (SRT-FUNCTION-CODE       NOT = WS-SAVE-FUNCTION-CODE)
           OR (SRT-PLAN-TYPE           NOT = WS-SAVE-PLAN-TYPE)
           OR (SRT-PLAN-CODE           NOT = WS-SAVE-PLAN-CODE)
               MOVE SRT-FUNCTION-CODE  TO G4-PROC-MESSAGE
                                          WS-SAVE-FUNCTION-CODE
               MOVE SRT-PLAN-TYPE      TO G4-PLAN-TYPE
                                          WS-SAVE-PLAN-TYPE
               MOVE SRT-PLAN-CODE      TO G4-PLAN-CODE
                                          WS-SAVE-PLAN-CODE

               MOVE FLP-DESC           TO G4-PLN-DESC.

           IF (SRT-FUNCTION-CODE       = "S"  OR "CS" OR "D")
           OR (SRT-FUNCTION-CODE       = "ET" OR "EC" OR "ED")
               PERFORM 840-FIND-EFDSET1
               PERFORM 8400-FIND-BN100EFDC

               MOVE WF4-FUNCTION-CODE  TO SRT-FUNCTION-CODE

               MOVE WF4-START-DATE     TO G4-START-DATE
               IF (SRT-FUNCTION-CODE   = "ET" OR "EC" OR "ED")
                   MOVE EFD-STOP-DATE  TO G4-STOP-DATE
               ELSE
                   MOVE WF4-STOP-DATE  TO G4-STOP-DATE
               END-IF
           ELSE
           IF (SRT-FUNCTION-CODE       = "A" OR "CA" OR "EA")
               PERFORM 8400-FIND-BN100EFDA

               MOVE WF3-FUNCTION-CODE  TO SRT-FUNCTION-CODE

               MOVE WF3-START-DATE     TO G4-START-DATE
               MOVE WF3-STOP-DATE      TO G4-STOP-DATE.

           IF (SRT-ERROR-NBR           NOT = ZEROES)
003820         MOVE SRT-ERROR-NBR      TO CRT-MSG-NBR
003760         MOVE SRT-ERROR-CAT      TO CRT-ERROR-CAT
003760         MOVE SRT-ERR-VAR1       TO CRT-ERR-VAR1
003830         PERFORM 790-GET-MSG
003790         MOVE CRT-MESSAGE        TO G4-ERROR-MESSAGE
                                          E4-ERROR-MESSAGE.

GW0309     IF (PRM-UPDATE-OPTION       = "U")      
GW0309        IF (G4-PLAN-CODE = "TAIP" OR "TAMI" OR "TAMF")
GW0309           PERFORM 8700-MOVE-TO-TAIP-AUDIT 
ACS003        END-IF
ACS004*       IF (G4-PLAN-CODE = "ESMS" OR "ESMX")
ACS004        IF (G4-PLAN-CODE = "ESMS")
ACS003            PERFORM 8900-WRITE-ZN325WK1-REC
ACS003        END-IF
ACS003     END-IF.
           
       5600-CONTINUE.
           IF (SRT-WARNING-NBR         NOT = ZEROES)
               PERFORM 5825-PRINT-WARNING
               THRU    5825-END
           ELSE
P44035         PERFORM 5910-DETAIL-HEADING
P44035         THRU    5910-END
J63718
J63718         IF (SRT-FUNCTION-CODE    NOT = WS-PREV-FUNCTION-CODE)
J63718         OR (SRT-PLAN-TYPE        NOT = WS-PREV-PLAN-TYPE)
J63718         OR (SRT-PLAN-CODE        NOT = WS-PREV-PLAN-CODE) 
J63718         OR (SRT-EMPLOYEE         NOT = WS-PREV-EMPLOYEE)
                   MOVE GN4-DETAIL-DATA        TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
J63718
J63718             MOVE G4-PROC-MESSAGE        TO WS-PREV-FUNCTION-CODE
J63718             MOVE G4-PLAN-TYPE           TO WS-PREV-PLAN-TYPE
J63718             MOVE G4-PLAN-CODE           TO WS-PREV-PLAN-CODE
J63718             MOVE SRT-EMPLOYEE           TO WS-PREV-EMPLOYEE
J63718         END-IF
J63718         
               IF (SRT-ERROR-NBR           NOT = ZEROES)
                   PERFORM 5820-PRINT-ERROR
                   THRU    5820-END
               END-IF
J63718     END-IF.

       5600-NEXT-BN100WORKS.
005600     READ BN100WORKS-FILE        INTO SRT-BN100WORKS-REC
003580         AT END
003560             SET END-OF-FILE     TO TRUE.

       5600-END.

011700******************************************************************
       5700-DO-BENEFIT.
011700******************************************************************

           MOVE SRT-COMPANY            TO DB-COMPANY
                                          WF1-COMPANY
                                          WF2-COMPANY.
           MOVE SRT-EMPLOYEE           TO DB-EMPLOYEE
                                          WF1-EMPLOYEE
                                          WF2-EMPLOYEE.
           MOVE SRT-PLAN-TYPE          TO DB-PLAN-TYPE
                                          WF1-PLAN-TYPE
                                          WF2-PLAN-TYPE.
           MOVE SRT-PLAN-CODE          TO DB-PLAN-CODE
                                          WF1-PLAN-CODE
                                          WF2-PLAN-CODE.
           MOVE SRT-START-DATE         TO DB-START-DATE
                                          WF1-START-DATE
                                          WF2-START-DATE.
           PERFORM 840-FIND-PLNSET1.

           INITIALIZE G4-PROC-MESSAGE
                      G4-PLAN-TYPE
                      G4-PLAN-CODE
                      G4-PLN-DESC.

           IF (SRT-FUNCTION-CODE       NOT = WS-SAVE-FUNCTION-CODE)
           OR (SRT-PLAN-TYPE           NOT = WS-SAVE-PLAN-TYPE)
           OR (SRT-PLAN-CODE           NOT = WS-SAVE-PLAN-CODE)
               MOVE SRT-FUNCTION-CODE  TO G4-PROC-MESSAGE
                                          WS-SAVE-FUNCTION-CODE
               MOVE SRT-PLAN-TYPE      TO G4-PLAN-TYPE
                                          WS-SAVE-PLAN-TYPE
               MOVE SRT-PLAN-CODE      TO G4-PLAN-CODE
                                          WS-SAVE-PLAN-CODE

               MOVE PLN-DESC           TO G4-PLN-DESC.

           IF (SRT-FUNCTION-CODE       = "S"  OR "CS" OR "D")
           OR (SRT-FUNCTION-CODE       = "ET" OR "EC" OR "ES" OR "ED")
           OR (SRT-FUNCTION-CODE       = "SO" OR "C" OR "CC")
               PERFORM 840-FIND-BENSET1
               PERFORM 8400-FIND-BN100BENC

               MOVE WF2-FUNCTION-CODE  TO SRT-FUNCTION-CODE

               MOVE WF2-START-DATE     TO G4-START-DATE
               IF (SRT-FUNCTION-CODE   = "ET" OR "EC" OR "ES" OR "ED")
               OR (SRT-FUNCTION-CODE   = "SO")
                   MOVE BEN-STOP-DATE  TO G4-STOP-DATE
               ELSE
                   MOVE WF2-STOP-DATE  TO G4-STOP-DATE
               END-IF

               IF  (PRM-UPDATE-OPTION  = "U")
               AND (SRT-FUNCTION-CODE  = "D")
                   CONTINUE
               ELSE
               IF (SRT-FUNCTION-CODE   = "C" OR "CC")
                   MOVE WF2-COVER-AMT  TO G4-BEN-COVER-AMT
               ELSE
J67795*            MOVE BEN-COVER-AMT  TO G4-BEN-COVER-AMT
J67795             COMPUTE G4-BEN-COVER-AMT ROUNDED = 
J67795                  BEN-COVER-AMT * 1
               END-IF
               END-IF
           ELSE
           IF (SRT-FUNCTION-CODE       = "CA" OR "SA" OR "EA")
               PERFORM 840-FIND-BENSET1
               PERFORM 8400-FIND-BN100BENA

               MOVE WF1-FUNCTION-CODE  TO SRT-FUNCTION-CODE

               IF (BENEFIT-FOUND)
J67795*            MOVE BEN-COVER-AMT  TO G4-BEN-COVER-AMT
J67795             COMPUTE G4-BEN-COVER-AMT ROUNDED = 
J67795                  BEN-COVER-AMT * 1
               ELSE
                   MOVE WF1-COVER-AMT  TO G4-BEN-COVER-AMT
               END-IF
               MOVE WF1-START-DATE     TO G4-START-DATE
               MOVE WF1-STOP-DATE      TO G4-STOP-DATE.
               
GW0309     IF (PRM-UPDATE-OPTION       = "U")      
GW0309        IF (G4-PLAN-CODE = "TAIP" OR "TAMI" OR "TAMF")
GW0309           PERFORM 8700-MOVE-TO-TAIP-AUDIT.

           IF (SRT-ERROR-NBR           NOT = ZEROES)
003820         MOVE SRT-ERROR-NBR      TO CRT-MSG-NBR
003760         MOVE SRT-ERROR-CAT      TO CRT-ERROR-CAT
003760         MOVE SRT-ERR-VAR1       TO CRT-ERR-VAR1
003830         PERFORM 790-GET-MSG
003790         MOVE CRT-MESSAGE        TO G4-ERROR-MESSAGE
                                          E4-ERROR-MESSAGE
               IF  (SRT-ERROR-CAT      = "BNBEN")
               AND (SRT-ERROR-NBR      = 156)
                   INITIALIZE G4-BEN-COVER-AMT
P44035             PERFORM 5910-DETAIL-HEADING
P44035             THRU    5910-END

J63718             IF (SRT-FUNCTION-CODE    NOT = WS-PREV-FUNCTION-CODE)
J63718             OR (SRT-PLAN-TYPE        NOT = WS-PREV-PLAN-TYPE)
J63718             OR (SRT-PLAN-CODE        NOT = WS-PREV-PLAN-CODE)
J63718             OR (SRT-EMPLOYEE         NOT = WS-PREV-EMPLOYEE)
                       MOVE GN4-DETAIL-DATA    TO RPT-GROUP-REQUEST
                       PERFORM 700-PRINT-RPT-GRP
J63718
J63718                 MOVE G4-PROC-MESSAGE    TO WS-PREV-FUNCTION-CODE
J63718                 MOVE G4-PLAN-TYPE       TO WS-PREV-PLAN-TYPE
J63718                 MOVE G4-PLAN-CODE       TO WS-PREV-PLAN-CODE
J63718                 MOVE SRT-EMPLOYEE       TO WS-PREV-EMPLOYEE
J63718
                       PERFORM 5820-PRINT-ERROR
                       THRU    5820-END
                       GO TO 5700-NEXT-BN100WORK
J63718             END-IF
J63718         END-IF
J63718     END-IF.

           IF  (SRT-FUNCTION-CODE      = "CA" OR "SA")
           AND (PRM-UPDATE-OPTION      = "U")
               MOVE WF1-START-DATE     TO DB-START-DATE
               PERFORM 840-FIND-BENSET1.

           IF  (PRM-UPDATE-OPTION      = "U")
           AND (SRT-FUNCTION-CODE      = "D")
P44035         PERFORM 5910-DETAIL-HEADING
P44035         THRU    5910-END

J63718         IF (SRT-FUNCTION-CODE    NOT = WS-PREV-FUNCTION-CODE)
J63718         OR (SRT-PLAN-TYPE        NOT = WS-PREV-PLAN-TYPE)
J63718         OR (SRT-PLAN-CODE        NOT = WS-PREV-PLAN-CODE)
J63718         OR (SRT-EMPLOYEE         NOT = WS-PREV-EMPLOYEE)
                   MOVE GN4-DETAIL-DATA    TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
J63718
J63718             MOVE G4-PROC-MESSAGE    TO WS-PREV-FUNCTION-CODE
J63718             MOVE G4-PLAN-TYPE       TO WS-PREV-PLAN-TYPE
J63718             MOVE G4-PLAN-CODE       TO WS-PREV-PLAN-CODE
J63718             MOVE SRT-EMPLOYEE       TO WS-PREV-EMPLOYEE
J63718         END-IF
J63718
               GO TO 5700-NEXT-BN100WORK
           ELSE
               PERFORM 5810-PRINT-CONT-N-DED
               THRU    5810-END
J63718     END-IF.

           IF (DETAIL-NOT-PRINTED)
      
               IF (SRT-WARNING-NBR         NOT = ZEROES)
                   PERFORM 5825-PRINT-WARNING
                   THRU    5825-END
               ELSE
P44035             PERFORM 5910-DETAIL-HEADING
P44035             THRU    5910-END
J6318
J63718             IF (SRT-FUNCTION-CODE    NOT = WS-PREV-FUNCTION-CODE)
J63718             OR (SRT-PLAN-TYPE        NOT = WS-PREV-PLAN-TYPE)
J63718             OR (SRT-PLAN-CODE        NOT = WS-PREV-PLAN-CODE)
J63718             OR (SRT-EMPLOYEE         NOT = WS-PREV-EMPLOYEE)
004320                 MOVE GN4-DETAIL-DATA    TO RPT-GROUP-REQUEST
004330                 PERFORM 700-PRINT-RPT-GRP
J63718
J63718                 MOVE G4-PROC-MESSAGE    TO WS-PREV-FUNCTION-CODE
J63718                 MOVE G4-PLAN-TYPE       TO WS-PREV-PLAN-TYPE
J63718                 MOVE G4-PLAN-CODE       TO WS-PREV-PLAN-CODE
J63718                 MOVE SRT-EMPLOYEE       TO WS-PREV-EMPLOYEE
J63718             END-IF
J63718
                   IF (SRT-ERROR-NBR       NOT = ZEROES)
                       PERFORM 5820-PRINT-ERROR
                       THRU    5820-END
                   END-IF
J63718         END-IF
J36716
J63718     END-IF.

       5700-NEXT-BN100WORK.
005600     READ BN100WORKS-FILE        INTO SRT-BN100WORKS-REC
003580         AT END
003560             SET END-OF-FILE     TO TRUE.

       5700-END.

011700******************************************************************
       5800-DO-PLAN.
011700******************************************************************

           MOVE SRT-COMPANY            TO DB-COMPANY
                                          WF1-COMPANY.
           MOVE SRT-EMPLOYEE           TO DB-EMPLOYEE
                                          WF1-EMPLOYEE.
           MOVE SRT-PLAN-TYPE          TO DB-PLAN-TYPE
                                          WF1-PLAN-TYPE.
           MOVE SRT-PLAN-CODE          TO DB-PLAN-CODE
                                          WF1-PLAN-CODE.
           MOVE SRT-START-DATE         TO DB-START-DATE
                                          WF1-START-DATE.
           PERFORM 840-FIND-PLNSET1.

           INITIALIZE G4-PROC-MESSAGE
                      G4-PLAN-TYPE
                      G4-PLAN-CODE
                      G4-PLN-DESC.

           IF (SRT-FUNCTION-CODE       NOT = WS-SAVE-FUNCTION-CODE)
           OR (SRT-PLAN-TYPE           NOT = WS-SAVE-PLAN-TYPE)
           OR (SRT-PLAN-CODE           NOT = WS-SAVE-PLAN-CODE)
               MOVE SRT-FUNCTION-CODE  TO G4-PROC-MESSAGE
                                          WS-SAVE-FUNCTION-CODE
               MOVE SRT-PLAN-TYPE      TO G4-PLAN-TYPE
                                          WS-SAVE-PLAN-TYPE
               MOVE SRT-PLAN-CODE      TO G4-PLAN-CODE
                                          WS-SAVE-PLAN-CODE

               MOVE PLN-DESC           TO G4-PLN-DESC.

GW0309     IF (PRM-UPDATE-OPTION       = "U")      
GW0309        IF (G4-PLAN-CODE = "TAIP" OR "TAMI" OR "TAMF")
GW0309           PERFORM 8700-MOVE-TO-TAIP-AUDIT.

           IF (SRT-FUNCTION-CODE       = "A")
               PERFORM 8400-FIND-BN100BENA

               MOVE WF1-FUNCTION-CODE  TO SRT-FUNCTION-CODE

               IF (SRT-FUNCTION-CODE   = "A")
                   MOVE SRT-START-DATE TO G4-START-DATE
                   MOVE WF1-STOP-DATE  TO G4-STOP-DATE
                   MOVE WF1-COVER-AMT  TO G4-BEN-COVER-AMT
                   IF (PRM-UPDATE-OPTION   = "R")
                       PERFORM 5810-PRINT-CONT-N-DED
                       THRU    5810-END.

           IF (SRT-WARNING-NBR         NOT = ZEROES)
               PERFORM 5825-PRINT-WARNING
               THRU    5825-END
           ELSE
           IF (SRT-ERROR-NBR           NOT = ZEROES)
               SET DETAIL-NOT-PRINTED  TO TRUE
               MOVE SRT-START-DATE     TO G4-START-DATE
003820         MOVE SRT-ERROR-NBR      TO CRT-MSG-NBR
003760         MOVE SRT-ERROR-CAT      TO CRT-ERROR-CAT
003760         MOVE SRT-ERR-VAR1       TO CRT-ERR-VAR1
003830         PERFORM 790-GET-MSG
003790         MOVE CRT-MESSAGE        TO G4-ERROR-MESSAGE
                                          E4-ERROR-MESSAGE.

           IF  (PRM-UPDATE-OPTION      = "U")
           AND (SRT-FUNCTION-CODE      = "A")
               MOVE SRT-EMPLOYEE       TO DB-EMPLOYEE
               MOVE SRT-START-DATE     TO DB-START-DATE
               PERFORM 840-FIND-BENSET1
J67795*        MOVE BEN-COVER-AMT      TO G4-BEN-COVER-AMT
J67795         COMPUTE G4-BEN-COVER-AMT ROUNDED = 
J67795              BEN-COVER-AMT * 1
               PERFORM 5810-PRINT-CONT-N-DED
               THRU    5810-END.

           IF  (DETAIL-NOT-PRINTED)
P70073     AND (SRT-WARNING-NBR        = ZEROES)              
P44035         PERFORM 5910-DETAIL-HEADING
P44035         THRU    5910-END
J63718
J63718         IF (SRT-FUNCTION-CODE    NOT = WS-PREV-FUNCTION-CODE)
J63718         OR (SRT-PLAN-TYPE        NOT = WS-PREV-PLAN-TYPE)
J63718         OR (SRT-PLAN-CODE        NOT = WS-PREV-PLAN-CODE)
J63718         OR (SRT-EMPLOYEE         NOT = WS-PREV-EMPLOYEE)
                   MOVE GN4-DETAIL-DATA    TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
J63718
J63718             MOVE G4-PROC-MESSAGE    TO WS-PREV-FUNCTION-CODE
J63718             MOVE G4-PLAN-TYPE       TO WS-PREV-PLAN-TYPE
J63718             MOVE G4-PLAN-CODE       TO WS-PREV-PLAN-CODE
J63718             MOVE SRT-EMPLOYEE       TO WS-PREV-EMPLOYEE
J63718         END-IF
J63718
               IF (SRT-ERROR-NBR       NOT = ZEROES)
                   PERFORM 5820-PRINT-ERROR
                   THRU    5820-END
               END-IF
J63718     END-IF.

       5800-NEXT-BN100WORK.
005600     READ BN100WORKS-FILE        INTO SRT-BN100WORKS-REC
003580         AT END
003560             SET END-OF-FILE     TO TRUE.

       5800-END.

011700******************************************************************
       5810-PRINT-CONT-N-DED.
011700******************************************************************

           INITIALIZE G4-BEN-CONT-TYPE
                      G4-BEN-CONT-AMT.

           SET DETAIL-NOT-PRINTED          TO TRUE.

           IF (SRT-FUNCTION-CODE           = "C" OR "CC")
               MOVE WF2-CMP-FLX-CONT       TO G4-BEN-CONT-AMT
           ELSE
           IF (SRT-FUNCTION-CODE          = "A" OR "CA" OR "SA" OR "EA")
               MOVE WF1-CMP-FLX-CONT       TO G4-BEN-CONT-AMT
           ELSE
           IF (BENEFIT-FOUND)
J67795*        MOVE BEN-CMP-FLX-CONT       TO G4-BEN-CONT-AMT.
J67795         COMPUTE G4-BEN-CONT-AMT ROUNDED = 
J67795              BEN-CMP-FLX-CONT * 1.

170393     SET WS-SAME-DED-CODE            TO TRUE.
           IF (G4-BEN-CONT-AMT             NOT = ZEROES)
               MOVE "FLEX"                 TO G4-BEN-CONT-TYPE
               INITIALIZE G4-PLN-DED-CODE
                          G4-EDM-NEXT-AMOUNT

               IF (SRT-ERROR-NBR           NOT = ZEROES)
                   PERFORM 5820-PRINT-ERROR
                   THRU    5820-END
               END-IF

               IF  ((SRT-FUNCTION-CODE         = "S"  OR "CS" OR "D")
               OR   (SRT-FUNCTION-CODE         = "ET" OR "EC" OR "ES")
               OR   (SRT-FUNCTION-CODE         = "SO" OR "ED")
               OR   (PRM-UPDATE-OPTION         = "U"))
               AND (BENEFIT-FOUND)
                   IF (BEN-STM-SEQ-NBR         NOT = ZEROES)
                       MOVE BEN-COMPANY        TO DB-COMPANY
                       MOVE BEN-EMPLOYEE       TO DB-EMPLOYEE
                       INITIALIZE DB-TIME-GROUP
                       MOVE BEN-STM-SEQ-NBR    TO DB-SEQ-NBR
                       PERFORM 840-FIND-STMSET2
                       IF (STANDTIME-FOUND)
J66766                     IF (STM-PAY-CODE    = WS-PREV-PLN-DED-CODE)
J66766                         SET WS-SAME-DED-CODE TO TRUE
J66766                     ELSE
J66766                         SET WS-DIFF-DED-CODE TO TRUE
J66766                     END-IF
J66766                     MOVE SPACES         TO WS-PREV-PLN-DED-CODE
                           MOVE STM-PAY-CODE   TO G4-PLN-DED-CODE
J66766                                            WS-PREV-PLN-DED-CODE
                           MOVE STM-RATE       TO G4-EDM-NEXT-AMOUNT
                       END-IF
                   ELSE
                   IF (BEN-FLEX-DED-SEQ        NOT = ZEROES)
                       MOVE BEN-COMPANY        TO DB-COMPANY
                       MOVE BEN-EMPLOYEE       TO DB-EMPLOYEE
                       MOVE PLN-FLEX-DED-CODE  TO DB-DED-CODE
                       MOVE BEN-FLEX-DED-SEQ   TO DB-SEQ-NBR
                       PERFORM 840-FIND-EDMSET1
                       IF (EMDEDMASTR-FOUND)
J66766                     IF (PLN-FLEX-DED-CODE
J66766                                         = WS-PREV-PLN-DED-CODE)
J66766                         SET WS-SAME-DED-CODE TO TRUE
J66766                     ELSE
J66766                         SET WS-DIFF-DED-CODE TO TRUE
J66766                     END-IF
J66766                     MOVE SPACES          TO WS-PREV-PLN-DED-CODE
                           MOVE PLN-FLEX-DED-CODE  TO G4-PLN-DED-CODE
J66766                                             WS-PREV-PLN-DED-CODE
J67795*                    MOVE EDM-NEXT-AMOUNT    TO G4-EDM-NEXT-AMOUNT
J67795                     COMPUTE G4-EDM-NEXT-AMOUNT ROUNDED = 
J67795                          EDM-NEXT-AMOUNT * 1
                       END-IF
                   END-IF
                   END-IF
               END-IF

               IF (EMDEDMASTR-FOUND)
               OR (PRM-UPDATE-OPTION               = "R")
               OR ((EMDEDMASTR-NOTFOUND)
               AND (G4-BEN-CONT-AMT                < ZEROES))
               OR (SRT-FUNCTION-CODE               = "EA")
P44035             PERFORM 5910-DETAIL-HEADING
P44035             THRU    5910-END
J63718
J63718             IF (SRT-FUNCTION-CODE    NOT = WS-PREV-FUNCTION-CODE)
J63718             OR (SRT-PLAN-TYPE        NOT = WS-PREV-PLAN-TYPE)
J63718             OR (SRT-PLAN-CODE        NOT = WS-PREV-PLAN-CODE)
J63718             OR (SRT-EMPLOYEE         NOT = WS-PREV-EMPLOYEE)
149940             OR (SRT-START-DATE       NOT = WS-PREV-START-DATE)
J66766             OR (WS-DIFF-DED-CODE)
                       MOVE GN4-DETAIL-DATA    TO RPT-GROUP-REQUEST
                       PERFORM 700-PRINT-RPT-GRP
J63718
J63718                 MOVE G4-PROC-MESSAGE    TO WS-PREV-FUNCTION-CODE
J63718                 MOVE G4-PLAN-TYPE       TO WS-PREV-PLAN-TYPE
J63718                 MOVE G4-PLAN-CODE       TO WS-PREV-PLAN-CODE
J63718                 MOVE SRT-EMPLOYEE       TO WS-PREV-EMPLOYEE
149940                 MOVE SRT-START-DATE     TO WS-PREV-START-DATE
J63718             END-IF
J63718
                   PERFORM 5812-INIT-DETAIL-BEN
                   THRU    5812-END
J63718         END-IF
J63718     END-IF.

           INITIALIZE G4-BEN-CONT-TYPE
                      G4-BEN-CONT-AMT.

           IF (SRT-FUNCTION-CODE           = "C" OR "CC")
               MOVE WF2-EMP-PRE-CONT       TO G4-BEN-CONT-AMT
           ELSE
           IF (SRT-FUNCTION-CODE          = "A" OR "CA" OR "SA" OR "EA")
               MOVE WF1-EMP-PRE-CONT       TO G4-BEN-CONT-AMT
           ELSE
           IF (BENEFIT-FOUND)
J67795*        MOVE BEN-EMP-PRE-CONT       TO G4-BEN-CONT-AMT.
J67795         COMPUTE G4-BEN-CONT-AMT ROUNDED = 
J67795              BEN-EMP-PRE-CONT * 1.

170393     SET WS-SAME-DED-CODE            TO TRUE.
           IF (G4-BEN-CONT-AMT             NOT = ZEROES)
               MOVE "PT"                   TO G4-BEN-CONT-TYPE
               INITIALIZE G4-PLN-DED-CODE
                          G4-EDM-NEXT-AMOUNT

               IF (SRT-ERROR-NBR           NOT = ZEROES)
                   PERFORM 5820-PRINT-ERROR
                   THRU    5820-END
               END-IF

               IF  ((SRT-FUNCTION-CODE         = "S"  OR "CS" OR "D")
               OR   (SRT-FUNCTION-CODE         = "ET" OR "EC" OR "ES")
               OR   (SRT-FUNCTION-CODE         = "SO" OR "ED")
               OR   (PRM-UPDATE-OPTION         = "U"))
               AND (BENEFIT-FOUND)
                   MOVE BEN-COMPANY            TO DB-COMPANY
                   MOVE BEN-EMPLOYEE           TO DB-EMPLOYEE
                   IF (BEN-PCT-AMT-FLAG        = "A")
                       MOVE PLN-PRE-DED-CODE-A TO DB-DED-CODE
                   ELSE
                       MOVE PLN-PRE-DED-CODE-P TO DB-DED-CODE
                   END-IF
                   MOVE BEN-PRE-SEQ-NBR        TO DB-SEQ-NBR
                   PERFORM 840-FIND-EDMSET1
                   IF (EMDEDMASTR-FOUND)
J66766                 INITIALIZE WS-COMP-EDM-DED-CODE
J66766                 MOVE DB-DED-CODE        TO WS-COMP-EDM-DED-CODE
J66766                 IF (WS-COMP-EDM-DED-CODE = WS-PREV-PLN-DED-CODE) 
J66766                     SET WS-SAME-DED-CODE TO TRUE
J66766                 ELSE
J66766                     SET WS-DIFF-DED-CODE TO TRUE
J66766                 END-IF
J66766                 MOVE SPACES             TO WS-PREV-PLN-DED-CODE
                       MOVE DB-DED-CODE        TO G4-PLN-DED-CODE
J66766                                            WS-PREV-PLN-DED-CODE
J67795*                MOVE EDM-NEXT-AMOUNT    TO G4-EDM-NEXT-AMOUNT
J67795                 COMPUTE G4-EDM-NEXT-AMOUNT ROUNDED = 
J67795                      EDM-NEXT-AMOUNT * 1
                   END-IF
               END-IF

               IF (EMDEDMASTR-FOUND)
               OR (PRM-UPDATE-OPTION           = "R")
P54229         OR ((EMDEDMASTR-NOTFOUND)
P54229         AND (G4-BEN-CONT-AMT                < ZEROES))
               OR (SRT-FUNCTION-CODE           = "EA")
P44035             PERFORM 5910-DETAIL-HEADING
P44035             THRU    5910-END
J63718
J63718             IF (SRT-FUNCTION-CODE    NOT = WS-PREV-FUNCTION-CODE)
J63718             OR (SRT-PLAN-TYPE        NOT = WS-PREV-PLAN-TYPE)
J63718             OR (SRT-PLAN-CODE        NOT = WS-PREV-PLAN-CODE)
J63718             OR (SRT-EMPLOYEE         NOT = WS-PREV-EMPLOYEE)
149940             OR (SRT-START-DATE       NOT = WS-PREV-START-DATE)
J66766             OR (WS-DIFF-DED-CODE)
                       MOVE GN4-DETAIL-DATA    TO RPT-GROUP-REQUEST
                       PERFORM 700-PRINT-RPT-GRP
J63718
J63718                 MOVE G4-PROC-MESSAGE    TO WS-PREV-FUNCTION-CODE
J63718                 MOVE G4-PLAN-TYPE       TO WS-PREV-PLAN-TYPE
J63718                 MOVE G4-PLAN-CODE       TO WS-PREV-PLAN-CODE
J63718                 MOVE SRT-EMPLOYEE       TO WS-PREV-EMPLOYEE
149940                 MOVE SRT-START-DATE     TO WS-PREV-START-DATE
J63718             END-IF
J63718
                   PERFORM 5812-INIT-DETAIL-BEN
                   THRU    5812-END
J63718         END-IF
J63718     END-IF.

           INITIALIZE G4-BEN-CONT-TYPE
                      G4-BEN-CONT-AMT.

           IF (SRT-FUNCTION-CODE           = "C" OR "CC")
               MOVE WF2-EMP-AFT-CONT       TO G4-BEN-CONT-AMT
           ELSE
           IF (SRT-FUNCTION-CODE          = "A" OR "CA" OR "SA" OR "EA")
               MOVE WF1-EMP-AFT-CONT       TO G4-BEN-CONT-AMT
           ELSE
J35176     IF (BENEFIT-FOUND)
J35176*J67795  MOVE BEN-EMP-AFT-CONT       TO G4-BEN-CONT-AMT.
J67795         COMPUTE G4-BEN-CONT-AMT ROUNDED = 
J67795              BEN-EMP-AFT-CONT * 1.

170393     SET WS-SAME-DED-CODE            TO TRUE.
           IF (G4-BEN-CONT-AMT             NOT = ZEROES)
               MOVE "AT"                   TO G4-BEN-CONT-TYPE
               INITIALIZE G4-PLN-DED-CODE
                          G4-EDM-NEXT-AMOUNT

               IF (SRT-ERROR-NBR           NOT = ZEROES)
                   PERFORM 5820-PRINT-ERROR
                   THRU    5820-END
               END-IF

               IF  ((SRT-FUNCTION-CODE         = "S"  OR "CS" OR "D")
               OR   (SRT-FUNCTION-CODE         = "ET" OR "EC" OR "ES")
               OR   (SRT-FUNCTION-CODE         = "SO" OR "ED")
               OR   (PRM-UPDATE-OPTION         = "U"))
               AND (BENEFIT-FOUND)
                   MOVE BEN-COMPANY            TO DB-COMPANY
                   MOVE BEN-EMPLOYEE           TO DB-EMPLOYEE
                   IF (BEN-PCT-AMT-FLAG        = "A")
                       MOVE PLN-AFT-DED-CODE-A TO DB-DED-CODE
                   ELSE
                       MOVE PLN-AFT-DED-CODE-P TO DB-DED-CODE
                   END-IF
                   MOVE BEN-AFT-SEQ-NBR        TO DB-SEQ-NBR
                   PERFORM 840-FIND-EDMSET1
                   IF (EMDEDMASTR-FOUND)
J66766                 INITIALIZE WS-COMP-EDM-DED-CODE
J66766                 MOVE DB-DED-CODE        TO WS-COMP-EDM-DED-CODE
J66766                 IF (WS-COMP-EDM-DED-CODE = WS-PREV-PLN-DED-CODE) 
J66766                     SET WS-SAME-DED-CODE TO TRUE
J66766                 ELSE
J66766                     SET WS-DIFF-DED-CODE TO TRUE
J66766                 END-IF
J66766                 MOVE SPACES             TO WS-PREV-PLN-DED-CODE
                       MOVE DB-DED-CODE        TO G4-PLN-DED-CODE
J66766                                            WS-PREV-PLN-DED-CODE
J67795*                MOVE EDM-NEXT-AMOUNT    TO G4-EDM-NEXT-AMOUNT
J67795                 COMPUTE G4-EDM-NEXT-AMOUNT ROUNDED = 
J67795                      EDM-NEXT-AMOUNT * 1
                   END-IF
               END-IF

               IF (EMDEDMASTR-FOUND)
               OR (PRM-UPDATE-OPTION           = "R")
P54229         OR ((EMDEDMASTR-NOTFOUND)
P54229         AND (G4-BEN-CONT-AMT                < ZEROES))
               OR (SRT-FUNCTION-CODE           = "EA")
P44035             PERFORM 5910-DETAIL-HEADING
P44035             THRU    5910-END
J63718
J63718             IF (SRT-FUNCTION-CODE    NOT = WS-PREV-FUNCTION-CODE)
J63718             OR (SRT-PLAN-TYPE        NOT = WS-PREV-PLAN-TYPE)
J63718             OR (SRT-PLAN-CODE        NOT = WS-PREV-PLAN-CODE)
J63718             OR (SRT-EMPLOYEE         NOT = WS-PREV-EMPLOYEE)
149940             OR (SRT-START-DATE       NOT = WS-PREV-START-DATE)
J66766             OR (WS-DIFF-DED-CODE)
                       MOVE GN4-DETAIL-DATA    TO RPT-GROUP-REQUEST
                       PERFORM 700-PRINT-RPT-GRP
J63718
J63718                 MOVE G4-PROC-MESSAGE    TO WS-PREV-FUNCTION-CODE
J63718                 MOVE G4-PLAN-TYPE       TO WS-PREV-PLAN-TYPE
J63718                 MOVE G4-PLAN-CODE       TO WS-PREV-PLAN-CODE
J63718                 MOVE SRT-EMPLOYEE       TO WS-PREV-EMPLOYEE
149940                 MOVE SRT-START-DATE     TO WS-PREV-START-DATE
J63718             END-IF
J63718
                   PERFORM 5812-INIT-DETAIL-BEN
                   THRU    5812-END
J63718         END-IF
J63718     END-IF.

           INITIALIZE G4-BEN-CONT-TYPE
                      G4-BEN-CONT-AMT.

           IF (SRT-FUNCTION-CODE           = "C" OR "CC")
               MOVE WF2-COMP-CONT          TO G4-BEN-CONT-AMT
           ELSE
           IF (SRT-FUNCTION-CODE          = "A" OR "CA" OR "SA" OR "EA")
               MOVE WF1-COMP-CONT          TO G4-BEN-CONT-AMT
           ELSE
J35176     IF (BENEFIT-FOUND)
J67795*        MOVE BEN-COMP-CONT          TO G4-BEN-CONT-AMT.
J67795         COMPUTE G4-BEN-CONT-AMT ROUNDED = 
J67795              BEN-COMP-CONT * 1.

170393     SET WS-SAME-DED-CODE            TO TRUE.
           IF (G4-BEN-CONT-AMT             NOT = ZEROES)
               MOVE "CO"                   TO G4-BEN-CONT-TYPE
               INITIALIZE G4-PLN-DED-CODE
                          G4-EDM-NEXT-AMOUNT

               IF (SRT-ERROR-NBR           NOT = ZEROES)
                   PERFORM 5820-PRINT-ERROR
                   THRU    5820-END
               END-IF

               IF  ((SRT-FUNCTION-CODE         = "S"  OR "CS" OR "D")
               OR   (SRT-FUNCTION-CODE         = "ET" OR "EC" OR "ES")
               OR   (SRT-FUNCTION-CODE         = "SO" OR "ED")
               OR   (PRM-UPDATE-OPTION         = "U"))
               AND (BENEFIT-FOUND)
                   MOVE BEN-PREM-UPD-DT        TO DB-START-DATE
                   MOVE BEN-PREM-GROUP         TO DB-GROUP-NAME
                   PERFORM 840-FIND-PRESET1
                   MOVE BEN-COMPANY            TO DB-COMPANY
                   MOVE BEN-EMPLOYEE           TO DB-EMPLOYEE
                   IF (BEN-PCT-AMT-FLAG        = SPACES)
                       IF (PLN-CMP-DED-CODE-A NOT = SPACES)
                           MOVE PLN-CMP-DED-CODE-A TO DB-DED-CODE
                       ELSE
                           MOVE PLN-CMP-DED-CODE-P TO DB-DED-CODE
                       END-IF
                   ELSE
                   IF (BEN-PCT-AMT-FLAG        = "A")
                   OR (PRE-EMP-CONT-TYPE       = "A")
                       MOVE PLN-CMP-DED-CODE-A TO DB-DED-CODE
                   ELSE
                       MOVE PLN-CMP-DED-CODE-P TO DB-DED-CODE
                   END-IF
                   END-IF
                   MOVE BEN-CMP-SEQ-NBR        TO DB-SEQ-NBR
                   PERFORM 840-FIND-EDMSET1
                   IF (EMDEDMASTR-NOTFOUND)
                       IF (BEN-PCT-AMT-FLAG    = "A")
                       OR (PRE-EMP-CONT-TYPE   = "A")
                           MOVE PLN-PRE-DED-MTCH-A TO DB-DED-CODE
                       ELSE
                           MOVE PLN-PRE-DED-MTCH-P TO DB-DED-CODE
                       END-IF
                       PERFORM 840-FIND-EDMSET1
                   END-IF
                   IF (EMDEDMASTR-FOUND)
J66766                 INITIALIZE WS-COMP-EDM-DED-CODE
J66766                 MOVE DB-DED-CODE        TO WS-COMP-EDM-DED-CODE
J66766                 IF (WS-COMP-EDM-DED-CODE = WS-PREV-PLN-DED-CODE) 
J66766                     SET WS-SAME-DED-CODE TO TRUE
J66766                 ELSE
J66766                     SET WS-DIFF-DED-CODE TO TRUE
J66766                 END-IF
J66766                 MOVE SPACES             TO WS-PREV-PLN-DED-CODE
                       MOVE DB-DED-CODE        TO G4-PLN-DED-CODE
J66766                                            WS-PREV-PLN-DED-CODE
J67795*                MOVE EDM-NEXT-AMOUNT    TO G4-EDM-NEXT-AMOUNT
J67795                 COMPUTE G4-EDM-NEXT-AMOUNT ROUNDED = 
J67795                      EDM-NEXT-AMOUNT * 1
                   END-IF
               END-IF

               IF (EMDEDMASTR-FOUND)
               OR (PRM-UPDATE-OPTION           = "R")
P54229         OR ((EMDEDMASTR-NOTFOUND)
P54229         AND (G4-BEN-CONT-AMT                < ZEROES))
               OR (SRT-FUNCTION-CODE           = "EA")
P44035             PERFORM 5910-DETAIL-HEADING
P44035             THRU    5910-END
J63718
J63718             IF (SRT-FUNCTION-CODE    NOT = WS-PREV-FUNCTION-CODE)
J63718             OR (SRT-PLAN-TYPE        NOT = WS-PREV-PLAN-TYPE)
J63718             OR (SRT-PLAN-CODE        NOT = WS-PREV-PLAN-CODE)
J63718             OR (SRT-EMPLOYEE         NOT = WS-PREV-EMPLOYEE)
149940             OR (SRT-START-DATE       NOT = WS-PREV-START-DATE)
J66766             OR (WS-DIFF-DED-CODE)
                       MOVE GN4-DETAIL-DATA    TO RPT-GROUP-REQUEST
                       PERFORM 700-PRINT-RPT-GRP
J63718
J63718                 MOVE G4-PROC-MESSAGE    TO WS-PREV-FUNCTION-CODE
J63718                 MOVE G4-PLAN-TYPE       TO WS-PREV-PLAN-TYPE
J63718                 MOVE G4-PLAN-CODE       TO WS-PREV-PLAN-CODE
J63718                 MOVE SRT-EMPLOYEE       TO WS-PREV-EMPLOYEE
149940                 MOVE SRT-START-DATE     TO WS-PREV-START-DATE
J63718             END-IF  
J63718
                   PERFORM 5812-INIT-DETAIL-BEN
                   THRU    5812-END
J63718         END-IF
J63718     END-IF.

170393     SET WS-SAME-DED-CODE            TO TRUE.
           IF  ((SRT-FUNCTION-CODE             = "S"  OR "CS" OR "D")
           OR   (SRT-FUNCTION-CODE             = "ET" OR "EC" OR "ES")
           OR   (SRT-FUNCTION-CODE             = "SO" OR "ED")
           OR   (PRM-UPDATE-OPTION             = "U"))
           AND (BENEFIT-FOUND)
               IF (BEN-CMP-AFT-SEQ             NOT = ZEROES)
                   MOVE BEN-COMPANY            TO DB-COMPANY
                   MOVE BEN-EMPLOYEE           TO DB-EMPLOYEE
                   IF (BEN-PCT-AMT-FLAG        = "A")
                       MOVE PLN-AFT-DED-MTCH-A TO DB-DED-CODE
                   ELSE
                       MOVE PLN-AFT-DED-MTCH-P TO DB-DED-CODE
                   END-IF
                   MOVE BEN-CMP-AFT-SEQ        TO DB-SEQ-NBR
                   PERFORM 840-FIND-EDMSET1
                   IF (EMDEDMASTR-FOUND)
J66766                 INITIALIZE WS-COMP-EDM-DED-CODE
J66766                 MOVE DB-DED-CODE        TO WS-COMP-EDM-DED-CODE
J66766                 IF (WS-COMP-EDM-DED-CODE = WS-PREV-PLN-DED-CODE) 
J66766                     SET WS-SAME-DED-CODE TO TRUE
J66766                 ELSE
J66766                     SET WS-DIFF-DED-CODE TO TRUE
J66766                 END-IF
J66766                 MOVE SPACES             TO WS-PREV-PLN-DED-CODE
                       MOVE DB-DED-CODE        TO G4-PLN-DED-CODE
J66766                                            WS-PREV-PLN-DED-CODE
J67795*                MOVE EDM-NEXT-AMOUNT    TO G4-EDM-NEXT-AMOUNT
J67795                 COMPUTE G4-EDM-NEXT-AMOUNT ROUNDED = 
J67795                      EDM-NEXT-AMOUNT * 1
P47112                 PERFORM 5910-DETAIL-HEADING
P47112                 THRU    5910-END
J63718
J63718               IF (SRT-FUNCTION-CODE  NOT = WS-PREV-FUNCTION-CODE)
J63718               OR (SRT-PLAN-TYPE      NOT = WS-PREV-PLAN-TYPE)    
J63718               OR (SRT-PLAN-CODE      NOT = WS-PREV-PLAN-CODE)    
J63718               OR (SRT-EMPLOYEE       NOT = WS-PREV-EMPLOYEE)
149940               OR (SRT-START-DATE     NOT = WS-PREV-START-DATE)
J66766               OR (WS-DIFF-DED-CODE)
P47112                   MOVE GN4-DETAIL-DATA   TO RPT-GROUP-REQUEST
P47112                   PERFORM 700-PRINT-RPT-GRP
J63718
J63718                   MOVE G4-PROC-MESSAGE   TO WS-PREV-FUNCTION-CODE
J63718                   MOVE G4-PLAN-TYPE      TO WS-PREV-PLAN-TYPE
J63718                   MOVE G4-PLAN-CODE      TO WS-PREV-PLAN-CODE
J63718                   MOVE SRT-EMPLOYEE      TO WS-PREV-EMPLOYEE
149940                   MOVE SRT-START-DATE    TO WS-PREV-START-DATE
J63718               END-IF
J63718
                     PERFORM 5812-INIT-DETAIL-BEN
                     THRU    5812-END
J63718             END-IF
J63718         END-IF
J63718     END-IF.

           IF (DETAIL-NOT-PRINTED)
P70073         IF (SRT-WARNING-NBR         NOT = ZEROES)               
P70073             PERFORM 5825-PRINT-WARNING                           
P70073             THRU    5825-END                                     
P70073         ELSE
P47112             PERFORM 5910-DETAIL-HEADING
P47112             THRU    5910-END
J63718
J63718             IF (SRT-FUNCTION-CODE    NOT = WS-PREV-FUNCTION-CODE)
J63718             OR (SRT-PLAN-TYPE        NOT = WS-PREV-PLAN-TYPE)
J63718             OR (SRT-PLAN-CODE        NOT = WS-PREV-PLAN-CODE)
J63718             OR (SRT-EMPLOYEE         NOT = WS-PREV-EMPLOYEE)
P47112                 MOVE GN4-DETAIL-DATA    TO RPT-GROUP-REQUEST
P47112                 PERFORM 700-PRINT-RPT-GRP
P47112
J63718
J63718                 MOVE G4-PROC-MESSAGE    TO WS-PREV-FUNCTION-CODE
J63718                 MOVE G4-PLAN-TYPE       TO WS-PREV-PLAN-TYPE
J63718                 MOVE G4-PLAN-CODE       TO WS-PREV-PLAN-CODE
J63718                 MOVE SRT-EMPLOYEE       TO WS-PREV-EMPLOYEE
J63718             END-IF
J63718
                   IF (SRT-ERROR-NBR               NOT = ZEROES)
P70375*                PERFORM 5910-DETAIL-HEADING
P70375*                THRU    5910-END
P70375*                MOVE GN4-DETAIL-DATA        TO RPT-GROUP-REQUEST
P70375*                PERFORM 700-PRINT-RPT-GRP
                       PERFORM 5820-PRINT-ERROR
                       THRU    5820-END
                   END-IF
P70073         END-IF

P70073*        IF (SRT-WARNING-NBR         NOT = ZEROES)
P70073*            PERFORM 5825-PRINT-WARNING
P70073*            THRU    5825-END
P70073*         END-IF

               PERFORM 5812-INIT-DETAIL-BEN
               THRU    5812-END
J63718     END-IF.

       5810-END.

011700******************************************************************
       5820-PRINT-ERROR.
011700******************************************************************


           MOVE G4-PROC-MESSAGE        TO E4-PROC-MESSAGE.
           MOVE G4-PLAN-TYPE           TO E4-PLAN-TYPE.
           MOVE G4-PLAN-CODE           TO E4-PLAN-CODE.
           MOVE G4-PLN-DESC            TO E4-PLN-DESC.
           MOVE G4-START-DATE          TO E4-START-DATE.
           MOVE G4-STOP-DATE           TO E4-STOP-DATE.
           MOVE G4-BEN-COVER-AMT       TO E4-BEN-COVER-AMT.
           MOVE G4-BEN-CONT-TYPE       TO E4-BEN-CONT-TYPE.
           MOVE G4-BEN-CONT-AMT        TO E4-BEN-CONT-AMT.

004320     MOVE EN4-DETAIL-DATA        TO RPT-GROUP-REQUEST.
004330     PERFORM 700-PRINT-RPT-GRP.

       5820-END.

011700******************************************************************
       5825-PRINT-WARNING.
011700******************************************************************


           MOVE G4-PLAN-TYPE           TO W4-PLAN-TYPE.
           MOVE G4-PLAN-CODE           TO W4-PLAN-CODE.
           MOVE G4-PLN-DESC            TO W4-PLN-DESC.
           MOVE G4-START-DATE          TO W4-START-DATE.
           MOVE G4-STOP-DATE           TO W4-STOP-DATE.
           MOVE G4-BEN-COVER-AMT       TO W4-BEN-COVER-AMT.
           MOVE G4-BEN-CONT-TYPE       TO W4-BEN-CONT-TYPE.
           MOVE G4-BEN-CONT-AMT        TO W4-BEN-CONT-AMT.
           MOVE SRT-WARNING-NBR        TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE            TO W4-WARNING-MESSAGE.

           MOVE WN4-DETAIL-DATA        TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.
 
       5825-END.

011700******************************************************************
       5812-INIT-DETAIL-BEN.
011700******************************************************************

           INITIALIZE G4-PROC-MESSAGE
                      G4-PLAN-TYPE
                      G4-PLAN-CODE
                      G4-PLN-DESC
                      G4-START-DATE
                      G4-STOP-DATE
                      G4-BEN-COVER-AMT.

           INITIALIZE E4-PROC-MESSAGE
                      E4-PLAN-TYPE
                      E4-PLAN-CODE
                      E4-PLN-DESC
                      E4-START-DATE
                      E4-STOP-DATE
                      E4-BEN-COVER-AMT.

           INITIALIZE W4-PLAN-TYPE
                      W4-PLAN-CODE
                      W4-PLN-DESC
                      W4-START-DATE
                      W4-STOP-DATE
                      W4-BEN-COVER-AMT.

       5812-END.

      ******************************************************************
       5900-HEADING.
      ******************************************************************


P44035     IF (WS-EMP-HEADING NOT = WS-EMPLOYEE)
               MOVE GN2-BNG-EMPLOYEE             TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
P55444     END-IF.
           PERFORM
           VARYING WS-I6 FROM 1 BY 1
P43669     UNTIL  (WS-I6 > 99)
P43669     OR    ((WS-G3-GRP-OR-CHANGE-1 (WS-I6) = SPACES)
P43669     AND    (WS-I6 NOT = 1))
              MOVE WS-G3-EFFECT-MESSAGE  (WS-I6) TO G3-EFFECT-MESSAGE
              MOVE WS-G3-EFFECT-DATE     (WS-I6) TO G3-EFFECT-DATE
              MOVE WS-G3-ACTION (WS-I6)          TO G3-ACTION
              MOVE WS-G3-PAH-ACTION-CODE (WS-I6) TO G3-PAH-ACTION-CODE
              MOVE WS-G3-ADD-DEL-CHANGE  (WS-I6) TO G3-ADD-DEL-CHANGE
              MOVE WS-G3-GRP-OR-CHANGE-1 (WS-I6) TO G3-GRP-OR-CHANGE-1
              MOVE WS-G3-GRP-OR-CHANGE-2 (WS-I6) TO G3-GRP-OR-CHANGE-2
              MOVE WS-G3-GRP-OR-CHANGE-3 (WS-I6) TO G3-GRP-OR-CHANGE-3
              MOVE WS-G3-GRP-OR-CHANGE-4 (WS-I6) TO G3-GRP-OR-CHANGE-4
              MOVE WS-G3-GRP-OR-CHANGE-5 (WS-I6) TO G3-GRP-OR-CHANGE-5
              MOVE WS-G3-GRP-OR-CHANGE-6 (WS-I6) TO G3-GRP-OR-CHANGE-6
              MOVE GN3-BNG-EFFECT-DATE           TO RPT-GROUP-REQUEST
              PERFORM 700-PRINT-RPT-GRP
           END-PERFORM.
           PERFORM 5440-INITIALIZE-WS-G3
           THRU    5440-END
           VARYING WS-I6 FROM 1 BY 1
P43669     UNTIL  (WS-I6 > 99)
PERF          OR ((WS-G3-GRP-OR-CHANGE-1 (WS-I6) = SPACES)
PERF         AND  (WS-I6 NOT = 1)).
P44035     MOVE WS-EMPLOYEE                      TO WS-EMP-HEADING.

       5900-END.

P44035******************************************************************
P44035 5910-DETAIL-HEADING.
P44035******************************************************************
P44035
P44035     IF  (SRT-FILE-PREFIX        = "EFD" OR "BEN" OR "PLN")
P44035     AND (WS-EMP-DET-HEADING NOT = WS-EMPLOYEE)
P44035         MOVE GN4-DETAIL-HEADING TO RPT-GROUP-REQUEST
P44035         PERFORM 700-PRINT-RPT-GRP
P44035         MOVE WS-EMPLOYEE        TO WS-EMP-DET-HEADING
P44035     END-IF.
P44035
P44626     SET DETAIL-PRINTED          TO TRUE.

P44035 5910-END.
P44035
011700******************************************************************
011710 5000-END.
011720******************************************************************
011730
006960******************************************************************
006970 6000-PURGE-BNG-N-BNH            SECTION 50.
006980******************************************************************
006990 6000-START.
007000
      **** Processing BN100 - Purging benefit audit records
001400     MOVE 059                    TO CRT-MSG-NBR.
001410     PERFORM 780-DISPLAY-MSG.
001420
007130     INITIALIZE WF-BN100WORK-KEY.
007140     PERFORM 8500-FIND-NLT-BN100WORK.

           IF (WS-RECORD-COUNT         > 1)
               PERFORM 6050-READ-BN100WORK
               THRU    6050-END
                   WS-RECORD-COUNT TIMES.

011900     PERFORM 6100-PRG-WF-COMPANY
011910     THRU    6100-END
011920         UNTIL (BN100WORK-NOTFOUND).
011930
007380     GO TO 6000-END.
007390
011700******************************************************************
       6050-READ-BN100WORK.
011700******************************************************************

           PERFORM 8600-FIND-NEXT-BN100WORK.

       6050-END.

011700******************************************************************
011900 6100-PRG-WF-COMPANY.
011700******************************************************************

           ADD 1                       TO WS-RECORD-COUNT.

           IF  (WF-REC-TYPE             = 3)
           OR  (WF-FUNCTION-CODE        = "ET" OR "EC" OR "ES" OR "ED")
           OR  (WF-FUNCTION-CODE        = "EA" OR "SO")
J73358     OR ((WF-FUNCTION-CODE        = SPACES)
J26718     AND (WF-FILE-PREFIX      NOT = "BNH" AND "BNG"))
               GO TO 6100-NEXT-BN100WORK.

           MOVE WF-COMPANY             TO DB-COMPANY.
           MOVE WF-EMPLOYEE            TO DB-EMPLOYEE.
           MOVE WF-EFFECT-DATE         TO DB-EFFECT-DATE.
           MOVE WF-BNG-BNH-SEQ-NBR     TO DB-SEQ-NBR.

           IF (WF-REC-TYPE             = 1)
PERF           MOVE BNGSET1-SEQ-NBR    TO WS-DB-BEG-RNG
PERF           PERFORM 830-DELETERNG-BNGSET1
           ELSE
PERF           MOVE BNHSET1-SEQ-NBR    TO WS-DB-BEG-RNG
PERF           PERFORM 830-DELETERNG-BNHSET1
PERF       END-IF.

           ADD 1                       TO WS-UPDATE-COUNT.

           IF (WS-UPDATE-COUNT         > WS-MAX-OPS-IN-TRAN)
               INITIALIZE WS-UPDATE-COUNT
               PERFORM 840-MODIFY-CKPSET1
               MOVE WS-RECORD-COUNT    TO WS-RESTART-COUNT
               MOVE WS-RESTART-DATA    TO CKP-RESTART-INFO
               PERFORM 820-STORE-CKPOINT
               PERFORM 925-AUDIT-END
               PERFORM 910-AUDIT-BEGIN.

       6100-NEXT-BN100WORK.
           PERFORM 8600-FIND-NEXT-BN100WORK.

011910 6100-END.

011700******************************************************************
011710 6000-END.
011720******************************************************************
011730
014750******************************************************************
014760 6200-CALL-BNENTPD               SECTION.
014770******************************************************************
014780 6200-START.

           INITIALIZE WS-FLEX-FLAG.
           IF (PLN-FLEX-PLAN           NOT = SPACES)
               MOVE PLN-FLEX-PLAN      TO DB-FLEX-PLAN
               PERFORM 840-FIND-FLPSET1
               IF (FLP-SPEND-ONLY      = "N")
                   MOVE "Y"            TO WS-FLEX-FLAG.

           IF  (PLN-CONTRIB-TYPE       = "5")
           AND (WS-FLEX-FLAG           = "Y")
           AND (PAY-FREQ-CHANGES)
               MOVE 158                TO CRT-ERROR-NBR
               GO TO 6200-END.

           IF  (SALARY-CHANGES)
           AND (BEN-COV-OVER-FLG       = "Y")
               MOVE 133                TO CRT-ERROR-NBR
               GO TO 6200-END.

           PERFORM 6210-MOVE-TO-BNBEN
           THRU    6210-END.
           IF (ERROR-FOUND)
               GO TO 6200-END.

           PERFORM 2000-BNBEN-EDIT-TRAN.

           IF (CHANGE-RULES)
               MOVE BNBEN-COMPANY        TO DB-COMPANY
               MOVE BNBEN-PLAN-TYPE (1)  TO DB-PLAN-TYPE
               MOVE BNBEN-EMPLOYEE (1)   TO DB-EMPLOYEE
               MOVE BNBEN-PLAN-CODE (1)  TO DB-PLAN-CODE
               MOVE BNBEN-START-DATE (1) TO DB-START-DATE
               PERFORM 840-FIND-BENSET1.

           GO TO 6200-END.

014750******************************************************************
014760 6210-MOVE-TO-BNBEN.
014770******************************************************************

           MOVE BEN-COMPANY            TO DB-COMPANY.
           MOVE BEN-PLAN-TYPE          TO DB-PLAN-TYPE.
           MOVE BEN-PLAN-CODE          TO DB-PLAN-CODE.
           PERFORM 840-FIND-PLNSET1.

           MOVE "C"                    TO BNBEN-FC.


183600     MOVE BEN-COMPANY            TO BNBEN-COMPANY.
183600     MOVE BEN-EMPLOYEE           TO BNBEN-EMPLOYEE (1).
183800     MOVE BEN-PLAN-TYPE          TO BNBEN-PLAN-TYPE (1).
183800     MOVE BEN-PLAN-CODE          TO BNBEN-PLAN-CODE (1).

           IF (ADD-RULES)
183900         MOVE WS-ADD-DATE        TO BNBEN-START-DATE (1)
           ELSE
183700         MOVE BEN-START-DATE     TO BNBEN-START-DATE (1)
183700                                    BNBEN-PT-START-DATE (1).

           IF (TERM-RULES)
183900         MOVE WS-TERM-DATE       TO BNBEN-STOP-DATE (1)
           ELSE
           IF (CHANGE-RULES)
               MOVE WS-CHANGE-DATE     TO WSDR-FR-DATE
               PERFORM 900-DATE-TO-JULIAN
               SUBTRACT 1              FROM WSDR-JULIAN-DAYS
               PERFORM 900-JULIAN-TO-DATE
               MOVE WSDR-FR-DATE       TO BNBEN-STOP-DATE (1).

184000     MOVE BEN-COV-OPTION         TO BNBEN-COVER-OPT (1).

           IF (PLN-CONTRIB-TYPE        = "5" OR "6" OR "7")
               MOVE BEN-CYCLES-REMAIN  TO BNBEN-COVER-OPT (1).

           IF (PLN-CONTRIB-TYPE        = "5" OR "6" OR "7")
               MOVE BEN-BOND-DED-AMT   TO BNBEN-COVER-AMT (1)
           ELSE
               MOVE BEN-COVER-AMT      TO BNBEN-COVER-AMT (1).

           IF (PLN-CONTRIB-TYPE        = "5" OR "6" OR "7")
               MOVE BEN-ANNUAL-AMT     TO BNBEN-PAY-RATE (1)
           ELSE
               MOVE BEN-PAY-RATE       TO BNBEN-PAY-RATE (1).

           IF (PLN-PLAN-TYPE           = "VA")
               MOVE BEN-NBR-HOURS      TO BNBEN-MULT-SALARY (1)
P67732     ELSE
P67732     IF (CVR-CALC-TYPE           = "N" )
P67732         INITIALIZE                 BNBEN-MULT-SALARY (1)
           ELSE
               MOVE BEN-MULTIPLE       TO BNBEN-MULT-SALARY (1).

           MOVE BEN-PCT-AMT-FLAG       TO BNBEN-PCT-AMT-FLAG (1).
           MOVE BEN-PRE-AFT-FLAG       TO BNBEN-PRE-AFT-FLAG (1).
           COMPUTE BNBEN-EMP-PRE-CONT (1) = BEN-EMP-PRE-CONT
                                          + BEN-CMP-FLX-CONT.
184300     MOVE BEN-EMP-AFT-CONT       TO BNBEN-EMP-AFT-CONT (1).
           MOVE BEN-CMP-FLX-CONT       TO BNBEN-CMP-FLX-CONT (1).
           MOVE BEN-COMP-CONT          TO BNBEN-COMP-CONT (1).

184300     MOVE BEN-SMOKER             TO BNBEN-SMOKER-FLAG (1).

           MOVE PRM-CREATE-TRANS       TO BNBEN-CREATE-TRANS (1).

P51666     MOVE BEN-DED-START-DATE     TO BNBEN-DED-START-DATE (1).
P51666     MOVE BEN-DED-STOP-DATE      TO BNBEN-DED-STOP-DATE (1).
P51666
           MOVE "S"                    TO BNBEN-LINE-FC (1).

           IF (TERM-RULES)
               MOVE 1                  TO BNBEN-NBR-LINES
           ELSE
               MOVE 2                  TO BNBEN-NBR-LINES

               PERFORM 6212-MOVE-FOR-ADD
               THRU    6212-END.

011710 6210-END.
011730
006960******************************************************************
       6212-MOVE-FOR-ADD.
006960******************************************************************

           MOVE BEN-PREM-UPD-DT        TO DB-START-DATE.
           MOVE BEN-PREM-GROUP         TO DB-GROUP-NAME.
           PERFORM 840-FIND-PRESET1.

           IF (PLN-COVERAGE-TYPE       = "2")
               MOVE BEN-COV-UPD-DT     TO DB-START-DATE
               MOVE BEN-COV-GROUP      TO DB-GROUP-NAME
               PERFORM 840-FIND-CVRSET1.

           MOVE BNBEN-EMPLOYEE (1)     TO BNBEN-EMPLOYEE (2).
           MOVE BNBEN-PLAN-TYPE (1)    TO BNBEN-PLAN-TYPE (2).
           MOVE BNBEN-PLAN-CODE (1)    TO BNBEN-PLAN-CODE (2).

           MOVE WS-CHANGE-DATE         TO BNBEN-START-DATE (2)
                                          BNBEN-PT-START-DATE (2).

           MOVE BEN-STOP-DATE          TO BNBEN-STOP-DATE (2).

184000     MOVE BNBEN-COVER-OPT (1)    TO BNBEN-COVER-OPT (2).

           MOVE BNBEN-COVER-AMT (1)    TO BNBEN-COVER-AMT (2).
           MOVE BNBEN-PAY-RATE (1)     TO BNBEN-PAY-RATE (2).

           MOVE BNBEN-EMP-PRE-CONT (1) TO BNBEN-EMP-PRE-CONT (2).
           MOVE BNBEN-EMP-AFT-CONT (1) TO BNBEN-EMP-AFT-CONT (2).
           MOVE BNBEN-CMP-FLX-CONT (1) TO BNBEN-CMP-FLX-CONT (2).
           MOVE BNBEN-COMP-CONT (1)    TO BNBEN-COMP-CONT (2).

           MOVE BNBEN-PCT-AMT-FLAG (1) TO BNBEN-PCT-AMT-FLAG (2).
           MOVE BNBEN-SMOKER-FLAG (1)  TO BNBEN-SMOKER-FLAG (2).
           MOVE BNBEN-MULT-SALARY (1)  TO BNBEN-MULT-SALARY (2).
           MOVE BNBEN-PRE-AFT-FLAG (1) TO BNBEN-PRE-AFT-FLAG (2).

           MOVE "A"                    TO BNBEN-LINE-FC (2).

           IF  (BNCHANGE-FOUND)
           AND (BNH-COMPANY            = BNBEN-COMPANY)
           AND (BNH-EMPLOYEE           = BNBEN-EMPLOYEE (2))
           AND (BNH-CHANGE-TYPE        = "2")
           AND (PLN-COVERAGE-TYPE      = "2")
P60050     AND (CVR-CALC-TYPE          = "M" OR "P" OR "N")
      ******** IF SALARY CHANGE INITIALIZE TO RECALC COVERAGE/ CONTRIBUTION
               INITIALIZE BNBEN-COVER-AMT (2).

           IF  (BNGRPCHG-FOUND)
           AND (BNG-COMPANY            = BNBEN-COMPANY)
           AND (BNG-EMPLOYEE           = BNBEN-EMPLOYEE (2))
           AND (PLN-COVERAGE-TYPE      = "2")
P60050     AND (CVR-CALC-TYPE          = "F" OR "P" OR "M" OR "N")
      ******** IF FLAT COVERAGE AND CHANGE IS TRIGGERED DUE TO DELETE/ADD
      ******** OF GROUP INITIALIZE COVER-AMT
               INITIALIZE BNBEN-COVER-AMT (2).

P60050     IF  (BNGRPCHG-FOUND)
P60050     AND (BNG-COMPANY            = BNBEN-COMPANY)
P60050     AND (BNG-EMPLOYEE           = BNBEN-EMPLOYEE (2))
P60050     AND (PLN-COVERAGE-TYPE      = "2")
P60050*    AND (CVR-CALC-TYPE          = "N" OR "M")
P67661     AND (CVR-CALC-TYPE          = "N")
P60050******* IF FIXED MULTIPLE COVERAGE AND CHANGE IS TRIGGERED DUE TO 
P60050******* DELETE/ADD OF GROUP INITIALIZE MULT-SALARY
P60050          INITIALIZE    BNBEN-MULT-SALARY (2).
P60050
           IF (RECOMPUTE-BEN)
      ******** IF SALARY CHANGE INITIALIZE TO RECALC COVERAGE/ CONTRIBUTION
               INITIALIZE BNBEN-PAY-RATE (2)
                          BNBEN-EMP-PRE-CONT (2)
                          BNBEN-EMP-AFT-CONT (2)
                          BNBEN-CMP-FLX-CONT (2)
                          BNBEN-COMP-CONT (2).

           MOVE "Y"                    TO BNBEN-SKIP-ELIGIBILITY (2).

           MOVE PRM-CREATE-TRANS       TO BNBEN-CREATE-TRANS (2).

           IF  (PLN-CONTRIB-TYPE            NOT = "0")
P63031     AND (PLN-CONTRIB-TYPE            NOT = "X ")
019000         MOVE BNBEN-COMPANY          TO BNREWS-COMPANY
019100         MOVE BNBEN-PLAN-TYPE (2)    TO BNREWS-PLAN-TYPE
019200         MOVE BNBEN-PLAN-CODE (2)    TO BNREWS-PLAN-CODE
019300         MOVE "E"                    TO BNREWS-COVER-TYPE
019400         MOVE BNBEN-EMPLOYEE (2)     TO BNREWS-EMPLOYEE
019500         MOVE BNBEN-START-DATE (2)   TO BNREWS-AS-OF-DATE
019600         MOVE "PRE"                  TO BNREWS-FILE-PREFIX
019700         PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70
019800         IF (ERROR-FOUND)
019900             GO TO 6212-END.
020000
           IF (PRE-CONT-TAX-STS        = "N")
               INITIALIZE BNBEN-PCT-AMT-FLAG (2)
                          BNBEN-PRE-AFT-FLAG (2).

       6212-END.

011700******************************************************************
011710 6200-END.
011720******************************************************************
011730
014750******************************************************************
014760 7000-FIND-BWT-FOR-ACTION        SECTION.
014770******************************************************************
014780 7000-START.

           MOVE PRM-COMPANY            TO BNREWS-COMPANY.
           MOVE WS-RULE-TYPE-SW        TO BNREWS-RULE-TYPE.
           MOVE WS-EMPLOYEE            TO BNREWS-EMPLOYEE.

           IF (PROC-EFD)
               MOVE "FL"               TO BNREWS-PLAN-TYPE
               MOVE EFD-FLEX-PLAN      TO BNREWS-PLAN-CODE
           ELSE
           IF (PROC-BEN)
               MOVE BEN-PLAN-TYPE      TO BNREWS-PLAN-TYPE
               MOVE BEN-PLAN-CODE      TO BNREWS-PLAN-CODE
           ELSE
           IF (PROC-PLN)
               MOVE PLN-PLAN-TYPE      TO BNREWS-PLAN-TYPE
               MOVE PLN-PLAN-CODE      TO BNREWS-PLAN-CODE.

           IF (PROC-BNG)
               MOVE BNG-EFFECT-DATE    TO BNREWS-AS-OF-DATE
               MOVE BNG-GROUP-NAME     TO BNREWS-DA-GROUP-NM
               MOVE BNG-ACT-OBJ-ID     TO DB-OBJ-ID
           ELSE
               MOVE BNH-EFFECT-DATE    TO BNREWS-AS-OF-DATE
               INITIALIZE BNREWS-DA-GROUP-NM
               MOVE BNH-ACT-OBJ-ID     TO DB-OBJ-ID.

           IF  ((PROC-BEN)
           OR   (PROC-PLN))
           AND (BNREWS-AS-OF-DATE      < PLN-START-DATE)
      ******** Action is prior to plan start date
               MOVE 139                TO CRT-ERROR-NBR
J91207         MOVE "BN100"            TO CRT-ERROR-CAT
               GO TO 7000-END.

           INITIALIZE BNREWS-ACTION-CODE.

           PERFORM 840-FIND-PAHSET2.
           IF (PERSACTHST-FOUND)
               MOVE PAH-ACTION-CODE    TO BNREWS-ACTION-CODE.

           MOVE "BWT"                  TO BNREWS-FILE-PREFIX.

           PERFORM 5000-FIND-RECS-BY-EMP-GROUP-70.

           IF (ERROR-FOUND)
               GO TO 7000-END.

           MOVE BWT-TRAN-REASON        TO BNBEN-HIPAA-REASON
                                          HRHDB-HIPAA-REASON.

           IF (TERM-RULES)
               PERFORM 7100-CALC-TERM-DATE
           ELSE
           IF (CHANGE-RULES)
               PERFORM 7300-CALC-CHANGE-DATE
           ELSE
           IF (ADD-RULES)
               PERFORM 7400-CALC-ADD-DATE.

P47448*--- If coming from 1342-FIND-BWT-BY-EMP-GROUP ROUTINE need to
P47448*--- set WS- error variables which are used for printing error
P47448
P47448     IF (ERROR-FOUND)
P47448         MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
P47448         MOVE CRT-ERROR-NBR      TO WS-ERROR-NBR
P47448         MOVE CRT-ERR-VAR1       TO WS-ERR-VAR1
P47448         MOVE CRT-ERR-VAR2       TO WS-ERR-VAR2
P47448         MOVE CRT-ERR-VAR3       TO WS-ERR-VAR3
P47448         MOVE CRT-ERR-VAR4       TO WS-ERR-VAR4
P47448         MOVE CRT-ERR-VAR5       TO WS-ERR-VAR5.

           GO TO 7000-END.

014750******************************************************************
014780 7000-END.
014750******************************************************************

014750******************************************************************
014760 7100-CALC-TERM-DATE             SECTION.
014770******************************************************************
014780 7100-START.
014790
           IF (BWT-TERM-STOP-DATE      = "TM")
               PERFORM 7110-CALC-FROM-TERM-DATE
               THRU    7110-END
           ELSE
           IF (BWT-TERM-STOP-DATE      = "PA")
               PERFORM 7120-CALC-FROM-PERS-ACT
               THRU    7120-END
           ELSE
           IF (BWT-TERM-STOP-DATE      = "WP")
               PERFORM 7130-CALC-FROM-WORK-PERIOD
               THRU    7130-END
           ELSE
           IF (BWT-TERM-STOP-DATE      = "PP")
               PERFORM 7140-CALC-FROM-PAY-PERIOD
               THRU    7140-END
           ELSE
           IF (BWT-TERM-STOP-DATE      = "ME")
               PERFORM 7150-CALC-FROM-END-OF-MNTH
               THRU    7150-END
           ELSE
           IF (BWT-TERM-STOP-DATE      = "X")
               INITIALIZE WS-TERM-DATE
      *------- BNUXWS-TERM-DATE may be set in BNUX70PD routine
               PERFORM 5000-DO-UX-TERM-DATE-70
               MOVE BNUXWS-TERM-DATE   TO WS-TERM-DATE.

           IF (BWT-TERM-MOS            NOT = ZEROES)
               IF (BWT-TERM-STOP-DATE  = "ME")
                   NEXT SENTENCE
               END-IF
               PERFORM 7160-ADD-MONTHS
               THRU    7160-END.

           IF (BWT-TERM-DAYS           NOT = ZEROES)
               PERFORM 7170-ADD-DAYS
               THRU    7170-END.

           GO TO 7100-END.

011700******************************************************************
       7110-CALC-FROM-TERM-DATE.
011700******************************************************************

           IF (EMP-TERM-DATE           = ZEROES)
               MOVE 131                TO CRT-ERROR-NBR
P77393         MOVE "BN100"            TO CRT-ERROR-CAT
           ELSE
               MOVE EMP-TERM-DATE      TO WS-TERM-DATE.

       7110-END.

011700******************************************************************
       7120-CALC-FROM-PERS-ACT.
011700******************************************************************

           IF (PROC-BNG)
               MOVE BNG-EFFECT-DATE    TO WS-TERM-DATE
           ELSE
               MOVE BNH-EFFECT-DATE    TO WS-TERM-DATE.

       7120-END.

011700******************************************************************
       7130-CALC-FROM-WORK-PERIOD.
011700******************************************************************

           PERFORM 7200-EDIT-PAY-PLAN.
           IF (ERROR-FOUND)
               GO TO 7130-END.

           MOVE PRM-COMPANY            TO DB-COMPANY.
           MOVE WS-SAVE-PAY-PLAN       TO DB-PLAN-CODE.
           MOVE PRO-EFFECT-DATE        TO DB-EFFECT-DATE.
           MOVE PRWSET1-EFFECT-DATE    TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PRWSET1.

           PERFORM
               UNTIL (PROTWRKPRD-NOTFOUND)
               OR    (PRW-WRK-END-DATE >= WS-EFFECT-DATE)

               PERFORM 860-FIND-NXTRNG-PRWSET1
           END-PERFORM.

           IF (PROTWRKPRD-NOTFOUND)
000500         MOVE 112                TO CRT-ERROR-NBR
               MOVE EMP-EMPLOYEE       TO WS-BWZ-EMP-NBR
000490         MOVE WS-BWZ-EMP-NBR     TO CRT-ERR-VAR1
000510         GO TO 7130-END.

           MOVE PRW-WRK-END-DATE       TO WS-TERM-DATE.

       7130-END.

011700******************************************************************
       7140-CALC-FROM-PAY-PERIOD.
011700******************************************************************

           PERFORM 7200-EDIT-PAY-PLAN.
           IF (ERROR-FOUND)
               GO TO 7140-END.

           MOVE PRM-COMPANY            TO DB-COMPANY.
           MOVE WS-SAVE-PAY-PLAN       TO DB-PLAN-CODE.
           MOVE PRO-EFFECT-DATE        TO DB-EFFECT-DATE.
           MOVE PRYSET1-EFFECT-DATE    TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PRYSET1.

           PERFORM
               UNTIL (PROTPAYPRD-NOTFOUND)
               OR    (PRY-PAY-END-DATE >= WS-EFFECT-DATE)

               PERFORM 860-FIND-NXTRNG-PRYSET1
           END-PERFORM.

           IF (PROTPAYPRD-NOTFOUND)
000500         MOVE 109                TO CRT-ERROR-NBR
               MOVE EMP-EMPLOYEE       TO WS-BWZ-EMP-NBR
000490         MOVE WS-BWZ-EMP-NBR     TO CRT-ERR-VAR1
000510         GO TO 7140-END.

           MOVE PRY-PAY-END-DATE     TO WS-TERM-DATE.

       7140-END.

011700******************************************************************
       7150-CALC-FROM-END-OF-MNTH.
011700******************************************************************

           IF (PROC-BNG)
               MOVE BNG-EFFECT-DATE    TO WS-DATE
           ELSE
               MOVE BNH-EFFECT-DATE    TO WS-DATE.

           IF (BWT-TERM-MOS NOT = ZEROES)
               MOVE WS-DATE            TO HRDTWS-FROM-DATE
               MOVE "M"                TO HRDTWS-ADD-WHAT
               MOVE BWT-TERM-MOS       TO HRDTWS-NUMBER
               PERFORM 5000-DO-ADD-TO-DATE-70
               IF (NO-ERROR-FOUND)
                   MOVE HRDTWS-TARGET-DATE TO WS-DATE.

           DIVIDE WS-YEAR              BY 4
                                       GIVING WS-DUMMY
                                       REMAINDER WS-REMAINDER.

           IF (WS-REMAINDER            NOT = ZEROES)
PERF           MOVE WS-BNCOM-MSG-107   TO CRT-MESSAGE
           ELSE
PERF           MOVE WS-BNCOM-MSG-108   TO CRT-MESSAGE.

003790     MOVE CRT-MESSAGE            TO WS-MNTHS-TBL.

           MOVE WS-MNTHS-TBL-RED (WS-MONTH)
                                       TO WS-DAY.

           MOVE WS-DATE                TO WS-TERM-DATE.

       7150-END.

011700******************************************************************
       7160-ADD-MONTHS.
011700******************************************************************

            MOVE WS-TERM-DATE           TO HRDTWS-FROM-DATE.
            MOVE "M"                    TO HRDTWS-ADD-WHAT.
            MOVE BWT-TERM-MOS           TO HRDTWS-NUMBER.
            PERFORM 5000-DO-ADD-TO-DATE-70.
            IF (NO-ERROR-FOUND)
                MOVE HRDTWS-TARGET-DATE TO WS-TERM-DATE.

       7160-END.

011700******************************************************************
       7170-ADD-DAYS.
011700******************************************************************

            MOVE WS-TERM-DATE           TO HRDTWS-FROM-DATE.
            MOVE "D"                    TO HRDTWS-ADD-WHAT.
            MOVE BWT-TERM-DAYS          TO HRDTWS-NUMBER.
            PERFORM 5000-DO-ADD-TO-DATE-70.
            IF (NO-ERROR-FOUND)
                MOVE HRDTWS-TARGET-DATE TO WS-TERM-DATE.

       7170-END.

011700******************************************************************
011710 7100-END.
011720******************************************************************
014890
000410******************************************************************
       7200-EDIT-PAY-PLAN              SECTION.
000410******************************************************************
       7200-START.

FAK        IF (EMP-OT-PLAN-CODE            = SPACES)
               MOVE PRM-COMPANY            TO DB-COMPANY 
009821         MOVE EMP-PROCESS-LEVEL      TO DB-PROCESS-LEVEL
009822         PERFORM 840-FIND-PRSSET1
               IF (PRS-OT-PLAN-CODE (EMP-PAY-FREQUENCY) NOT = SPACES)
009823             MOVE PRS-OT-PLAN-CODE (EMP-PAY-FREQUENCY)
009824                                     TO DB-PLAN-CODE
                                              WS-SAVE-PAY-PLAN
009825         ELSE                             
009821             INITIALIZE DB-PROCESS-LEVEL
009822             PERFORM 840-FIND-PRSSET1
                   IF (PRS-OT-PLAN-CODE (EMP-PAY-FREQUENCY) = SPACES)
      **************** Pay plan does not exist for employee \
000500                 MOVE 132            TO CRT-ERROR-NBR
                       MOVE EMP-EMPLOYEE   TO WS-BWZ-EMP-NBR
000490                 MOVE WS-BWZ-EMP-NBR TO CRT-ERR-VAR1
000510                 GO TO 7200-END
000510             ELSE
                       MOVE PRS-OT-PLAN-CODE (EMP-PAY-FREQUENCY) 
                                           TO DB-PLAN-CODE
                                              WS-SAVE-PAY-PLAN
                   END-IF
               END-IF
           ELSE                                      
               MOVE EMP-OT-PLAN-CODE       TO DB-PLAN-CODE
                                              WS-SAVE-PAY-PLAN.

           MOVE PRM-COMPANY                TO DB-COMPANY.
           MOVE PROSET2-PLAN-CODE          TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PROSET2.
           PERFORM
               UNTIL (PROVERTIME-NOTFOUND)
               OR    (PRO-EFFECT-DATE      <= WS-EFFECT-DATE)

               PERFORM 860-FIND-NXTRNG-PROSET2
           END-PERFORM.

           GO TO 7200-END.

014900******************************************************************
       7200-END.
014900******************************************************************

014750******************************************************************
014760 7300-CALC-CHANGE-DATE           SECTION.
014770******************************************************************
014780 7300-START.
014790
           IF (BWT-CHANGE-EFFECT       = "PA")
               PERFORM 7310-CALC-FROM-PERS-ACT
               THRU    7310-END
           ELSE
           IF (BWT-CHANGE-EFFECT       = "NM")
               PERFORM 7320-CALC-FROM-1ST-OF-NXT-MNTH
               THRU    7320-END
           ELSE
           IF (BWT-CHANGE-EFFECT       = "PM")
               PERFORM 7330-CALC-FROM-1ST-OF-MNTH
               THRU    7330-END
           ELSE
           IF (BWT-CHANGE-EFFECT       = "DC")
               PERFORM 7340-CALC-FROM-ACTION-DATE
               THRU    7340-END
           ELSE
           IF (BWT-CHANGE-EFFECT       = "PP" OR "NP" OR "PW" OR "NW")
               IF (PROC-BNG)
                   MOVE BNG-COMPANY    TO BNEDWS-COMPANY
                   MOVE BNG-EMPLOYEE   TO BNEDWS-EMPLOYEE
                   MOVE BNG-EFFECT-DATE TO BNEDWS-WAIT-PERIOD-DATE
               ELSE
                   MOVE BNH-COMPANY    TO BNEDWS-COMPANY
                   MOVE BNH-EMPLOYEE   TO BNEDWS-EMPLOYEE
                   MOVE BNH-EFFECT-DATE TO BNEDWS-WAIT-PERIOD-DATE
               END-IF
               IF (BWT-CHANGE-EFFECT   = "PP")
                   MOVE "3"            TO BNEDWS-INIT-ENT-POINT
               ELSE
               IF (BWT-CHANGE-EFFECT   = "NP")
                   MOVE "4"            TO BNEDWS-INIT-ENT-POINT
               ELSE
               IF (BWT-CHANGE-EFFECT   = "PW")
                   MOVE "5"            TO BNEDWS-INIT-ENT-POINT
               ELSE
               IF (BWT-CHANGE-EFFECT   = "NW")
                   MOVE "6"            TO BNEDWS-INIT-ENT-POINT
               END-IF
               END-IF
               END-IF
               END-IF
               PERFORM 7000-CALC-FROM-ENTRY-POINTS-70
               IF (ERROR-FOUND)
                   GO TO 7300-END
               END-IF
               MOVE BNEDWS-ELIGIBILITY-DATE TO WS-CHANGE-DATE.

           MOVE WS-CHANGE-DATE         TO BNEDWS-ELIGIBILITY-DATE.

           GO TO 7300-END.

011700******************************************************************
       7310-CALC-FROM-PERS-ACT.
011700******************************************************************

           IF (PROC-BNG)
               MOVE BNG-EFFECT-DATE    TO WS-CHANGE-DATE
           ELSE
               MOVE BNH-EFFECT-DATE    TO WS-CHANGE-DATE.

       7310-END.

011700******************************************************************
       7320-CALC-FROM-1ST-OF-NXT-MNTH.
011700******************************************************************

           IF (PROC-BNG)
               MOVE BNG-EFFECT-DATE    TO WS-DATE
           ELSE
               MOVE BNH-EFFECT-DATE    TO WS-DATE.

           MOVE 1                      TO WS-DAY.

           ADD 1                       TO WS-MONTH.

           IF (WS-MONTH                > 12)
               MOVE 1                  TO WS-MONTH
               ADD 1                   TO WS-YEAR.

           MOVE WS-DATE                TO WS-CHANGE-DATE.

       7320-END.

011700******************************************************************
       7330-CALC-FROM-1ST-OF-MNTH.
011700******************************************************************

           IF (PROC-BNG)
               MOVE BNG-EFFECT-DATE    TO WS-DATE
           ELSE
               MOVE BNH-EFFECT-DATE    TO WS-DATE.

           MOVE 1                      TO WS-DAY.

           MOVE WS-DATE                TO WS-CHANGE-DATE.

       7330-END.

011700******************************************************************
       7340-CALC-FROM-ACTION-DATE.
011700******************************************************************

           IF (PROC-BNG)
               MOVE BNG-DATE-STAMP     TO WS-CHANGE-DATE
           ELSE
               MOVE BNH-DATE-STAMP     TO WS-CHANGE-DATE.

       7340-END.

011700******************************************************************
011710 7300-END.
011720******************************************************************
014890
014750******************************************************************
014760 7400-CALC-ADD-DATE              SECTION.
014770******************************************************************
014780 7400-START.
014790
           MOVE PRM-COMPANY            TO BNEDWS-COMPANY.
           MOVE WS-EMPLOYEE            TO BNEDWS-EMPLOYEE.

           IF (PROC-EFD)
               MOVE "FL"               TO BNEDWS-PLAN-TYPE
           ELSE
           IF (PROC-BEN)
               MOVE BEN-PLAN-TYPE      TO BNEDWS-PLAN-TYPE
               MOVE BEN-PLAN-CODE      TO BNEDWS-PLAN-CODE
           ELSE
           IF (PROC-PLN)
               MOVE PLN-PLAN-TYPE      TO BNEDWS-PLAN-TYPE
               MOVE PLN-PLAN-CODE      TO BNEDWS-PLAN-CODE.

           IF (PROC-BNG)
               MOVE BNG-EFFECT-DATE    TO BNEDWS-AS-OF-DATE
           ELSE
               MOVE BNH-EFFECT-DATE    TO BNEDWS-AS-OF-DATE.

117816     MOVE "Y"                    TO BNEDWS-FROM-MAGIC-SW.

           PERFORM 5000-ELIGIBILITY-DATE-CALC-70.
           IF (NO-ERROR-FOUND)
               MOVE BNEDWS-ELIGIBILITY-DATE  TO WS-ADD-DATE.

           MOVE WS-ADD-DATE            TO BNEDWS-ELIGIBILITY-DATE.

           GO TO 7400-END.

011700******************************************************************
011710 7400-END.
011720******************************************************************
014890
014030******************************************************************
014040 8000-CREATE-BN100WORK           SECTION.
014050******************************************************************
014060 8000-START.
014070
014080     INITIALIZE BN100WORK-REC.
014300
014310******************************************************************
014320 8000-END.
014330******************************************************************
014340
014350******************************************************************
014360 8200-STORE-BN100WORK            SECTION.
014370******************************************************************
014380 8200-START.
014390
014400     WRITE BN100WORK-REC
014410         INVALID KEY
                   MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
014420*            MOVE WF-COMPANY         TO WS3-COMPANY
014430*            MOVE WF-EMPLOYEE        TO WS3-EMPLOYEE
014440*            MOVE WF-EFFECT-DATE     TO WS3-EFFECT-DATE
014440*            MOVE WF-REC-TYPE        TO WS3-REC-TYPE
014450*            MOVE WF-SEQ-NBR         TO WS3-SEQ-NBR
      *            MOVE "BN100"            TO CRT-ERROR-CAT
014480*            MOVE 103                TO CRT-MSG-NBR
014490*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-WORK-FILE       TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 106                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BN100WORK-KEY   TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 107                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
                   MOVE WS-ERROR-CAT       TO CRT-ERROR-CAT
014530     END-WRITE.
014540
014550******************************************************************
014560 8200-END.
014570******************************************************************
014580
014350******************************************************************
014360 8200-REWRITE-BN100WORK          SECTION.
014370******************************************************************
014380 8200-START.
014390
014400     REWRITE BN100WORK-REC
014410         INVALID KEY
                   MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
014420*            MOVE WF-COMPANY         TO WS3-COMPANY
014430*            MOVE WF-EMPLOYEE        TO WS3-EMPLOYEE
014440*            MOVE WF-EFFECT-DATE     TO WS3-EFFECT-DATE
014440*            MOVE WF-REC-TYPE        TO WS3-REC-TYPE
014450*            MOVE WF-SEQ-NBR         TO WS3-SEQ-NBR
      *            MOVE "BN100"            TO CRT-ERROR-CAT
014480*            MOVE 129                TO CRT-MSG-NBR
014490*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-WORK-FILE       TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 106                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BN100WORK-KEY   TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 107                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
                   MOVE WS-ERROR-CAT       TO CRT-ERROR-CAT
014530     END-REWRITE.
014540
014550******************************************************************
014560 8200-END.
014570******************************************************************
014580
014350******************************************************************
014360 8300-DELETE-BN100WORK           SECTION.
014370******************************************************************
014380 8300-START.
014390
014400     DELETE BN100WORK-FILE RECORD
014410         INVALID KEY
                   MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
014420*            MOVE WF-COMPANY         TO WS3-COMPANY
014430*            MOVE WF-EMPLOYEE        TO WS3-EMPLOYEE
014440*            MOVE WF-EFFECT-DATE     TO WS3-EFFECT-DATE
014440*            MOVE WF-REC-TYPE        TO WS3-REC-TYPE
014450*            MOVE WF-SEQ-NBR         TO WS3-SEQ-NBR
      *            MOVE "BN100"            TO CRT-ERROR-CAT
014480*            MOVE 123                TO CRT-MSG-NBR
014490*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-WORK-FILE       TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 106                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BN100WORK-KEY   TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 107                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
                   MOVE WS-ERROR-CAT       TO CRT-ERROR-CAT
014530     END-DELETE.
014540
014550******************************************************************
014560 8300-END.
014570******************************************************************
014580
014590******************************************************************
014600 8400-FIND-BN100WORK             SECTION.
014610******************************************************************
014620 8400-START.
014630
014640     SET BN100WORK-FOUND             TO TRUE.
014650
014660     READ BN100WORK-FILE             KEY WF-BN100WORK-KEY
014670         INVALID KEY
014680             SET BN100WORK-NOTFOUND  TO TRUE
014690     END-READ.
014700
014710******************************************************************
014720 8400-END.
014730******************************************************************
014740
014750******************************************************************
014760 8500-FIND-NLT-BN100WORK         SECTION.
014770******************************************************************
014780 8500-START.
014790
014800     SET BN100WORK-FOUND             TO TRUE.
014810
014820     START BN100WORK-FILE            KEY NOT < WF-BN100WORK-KEY
014830         INVALID KEY
014840             SET BN100WORK-NOTFOUND  TO TRUE
014850     END-START.
014860
014870     IF (BN100WORK-FOUND)
014880         PERFORM 8600-FIND-NEXT-BN100WORK.
014890
014900******************************************************************
014910 8500-END.
014920******************************************************************
014930
014940******************************************************************
014950 8600-FIND-NEXT-BN100WORK        SECTION.
014960******************************************************************
014970 8600-START.
014980
014990     SET BN100WORK-FOUND             TO TRUE.
015000
015010     READ BN100WORK-FILE             NEXT RECORD
015020         AT END
015030             SET BN100WORK-NOTFOUND  TO TRUE
015040     END-READ.
015050
015060******************************************************************
015070 8600-END.
015080******************************************************************
015090
014030******************************************************************
014040 8000-CREATE-BN100BENC           SECTION.
014050******************************************************************
014060 8000-START.
014070
014080     INITIALIZE BN100BENC-REC.
014300
014310******************************************************************
014320 8000-END.
014330******************************************************************
014340
014350******************************************************************
014360 8200-STORE-BN100BENC            SECTION.
014370******************************************************************
014380 8200-START.
014390
014400     WRITE BN100BENC-REC
014410         INVALID KEY
                   MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
014420*            MOVE WF2-COMPANY        TO WS1-COMPANY
014430*            MOVE WF2-EMPLOYEE       TO WS1-EMPLOYEE
014430*            MOVE WF2-PLAN-TYPE      TO WS1-PLAN-TYPE
014430*            MOVE WF2-PLAN-CODE      TO WS1-PLAN-CODE
014440*            MOVE WF2-START-DATE     TO WS1-START-DATE
      *            MOVE "BN100"            TO CRT-ERROR-CAT
014480*            MOVE 104                TO CRT-MSG-NBR
014490*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BEN-CHANGE-FILE TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 106                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BN100BEN-KEY    TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 107                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
                   MOVE WS-ERROR-CAT       TO CRT-ERROR-CAT
014530     END-WRITE.
014540
014550******************************************************************
014560 8200-END.
014570******************************************************************
014580
014350******************************************************************
014360 8200-REWRITE-BN100BENC          SECTION.
014370******************************************************************
014380 8200-START.
014390
014400     REWRITE BN100BENC-REC
014410         INVALID KEY
                   MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
014420*            MOVE WF2-COMPANY        TO WS1-COMPANY
014430*            MOVE WF2-EMPLOYEE       TO WS1-EMPLOYEE
014430*            MOVE WF2-PLAN-TYPE      TO WS1-PLAN-TYPE
014430*            MOVE WF2-PLAN-CODE      TO WS1-PLAN-CODE
014440*            MOVE WF2-START-DATE     TO WS1-START-DATE
      *            MOVE "BN100"            TO CRT-ERROR-CAT
014480*            MOVE 121                TO CRT-MSG-NBR
014490*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BEN-CHANGE-FILE TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 106                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BN100BEN-KEY    TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 107                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
                   MOVE WS-ERROR-CAT       TO CRT-ERROR-CAT
014530     END-REWRITE.
014540
014550******************************************************************
014560 8200-END.
014570******************************************************************
014580
014350******************************************************************
014360 8300-DELETE-BN100BENC           SECTION.
014370******************************************************************
014380 8300-START.
014390
014400     DELETE BN100BENC-FILE RECORD
014410         INVALID KEY
                   MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
014420*            MOVE WF2-COMPANY        TO WS1-COMPANY
014430*            MOVE WF2-EMPLOYEE       TO WS1-EMPLOYEE
014430*            MOVE WF2-PLAN-TYPE      TO WS1-PLAN-TYPE
014430*            MOVE WF2-PLAN-CODE      TO WS1-PLAN-CODE
014440*            MOVE WF2-START-DATE     TO WS1-START-DATE
      *            MOVE "BN100"            TO CRT-ERROR-CAT
014480*            MOVE 122                TO CRT-MSG-NBR
014490*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BEN-CHANGE-FILE TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 106                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BN100BEN-KEY    TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 107                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
                   MOVE WS-ERROR-CAT       TO CRT-ERROR-CAT
014530     END-DELETE.
014540
014550******************************************************************
014560 8300-END.
014570******************************************************************
014580
014590******************************************************************
014600 8400-FIND-BN100BENC             SECTION.
014610******************************************************************
014620 8400-START.
014630
014640     SET BN100BENC-FOUND             TO TRUE.
014650
014660     READ BN100BENC-FILE             KEY WF2-BN100BENC-KEY
014670         INVALID KEY
014680             SET BN100BENC-NOTFOUND  TO TRUE
014690     END-READ.
014700
014710******************************************************************
014720 8400-END.
014730******************************************************************
014740
014750******************************************************************
014760 8500-FIND-NLT-BN100BENC         SECTION.
014770******************************************************************
014780 8500-START.
014790
014800     SET BN100BENC-FOUND             TO TRUE.
014810
014820     START BN100BENC-FILE            KEY NOT < WF2-BN100BENC-KEY
014830         INVALID KEY
014840             SET BN100BENC-NOTFOUND  TO TRUE
014850     END-START.
014860
014870     IF (BN100BENC-FOUND)
014880         PERFORM 8600-FIND-NEXT-BN100BENC.
014890
014900******************************************************************
014910 8500-END.
014920******************************************************************
014930
014940******************************************************************
014950 8600-FIND-NEXT-BN100BENC        SECTION.
014960******************************************************************
014970 8600-START.
014980
014990     SET BN100BENC-FOUND             TO TRUE.
015000
015010     READ BN100BENC-FILE             NEXT RECORD
015020         AT END
015030             SET BN100BENC-NOTFOUND  TO TRUE
015040     END-READ.
015050
015060******************************************************************
015070 8600-END.
015080******************************************************************
015090
014030******************************************************************
014040 8000-CREATE-BN100BENA           SECTION.
014050******************************************************************
014060 8000-START.
014070
014080     INITIALIZE BN100BENA-REC.
014300
014310******************************************************************
014320 8000-END.
014330******************************************************************
014340
014350******************************************************************
014360 8200-STORE-BN100BENA            SECTION.
014370******************************************************************
014380 8200-START.
014390
014400     WRITE BN100BENA-REC
014410         INVALID KEY
                   MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
014420*            MOVE WF1-COMPANY        TO WS1-COMPANY
014430*            MOVE WF1-EMPLOYEE       TO WS1-EMPLOYEE
014430*            MOVE WF1-PLAN-TYPE      TO WS1-PLAN-TYPE
014430*            MOVE WF1-PLAN-CODE      TO WS1-PLAN-CODE
014440*            MOVE WF1-START-DATE     TO WS1-START-DATE
      *            MOVE "BN100"            TO CRT-ERROR-CAT
014480*            MOVE 149                TO CRT-MSG-NBR
014490*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BEN-ADD-FILE    TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 106                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BN100BEN-KEY    TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 107                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
                   MOVE WS-ERROR-CAT       TO CRT-ERROR-CAT
014530     END-WRITE.
014540
014550******************************************************************
014560 8200-END.
014570******************************************************************
014580
014350******************************************************************
014360 8200-REWRITE-BN100BENA          SECTION.
014370******************************************************************
014380 8200-START.
014390
014400     REWRITE BN100BENA-REC
014410         INVALID KEY
                   MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
014420*            MOVE WF1-COMPANY        TO WS1-COMPANY
014430*            MOVE WF1-EMPLOYEE       TO WS1-EMPLOYEE
014430*            MOVE WF1-PLAN-TYPE      TO WS1-PLAN-TYPE
014430*            MOVE WF1-PLAN-CODE      TO WS1-PLAN-CODE
014440*            MOVE WF1-START-DATE     TO WS1-START-DATE
      *            MOVE "BN100"            TO CRT-ERROR-CAT
014480*            MOVE 128                TO CRT-MSG-NBR
014490*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BEN-ADD-FILE    TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 106                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BN100BEN-KEY    TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 107                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
                   MOVE WS-ERROR-CAT       TO CRT-ERROR-CAT
014530     END-REWRITE.
014540
014550******************************************************************
014560 8200-END.
014570******************************************************************
014580
014350******************************************************************
014360 8300-DELETE-BN100BENA           SECTION.
014370******************************************************************
014380 8300-START.
014390
014400     DELETE BN100BENA-FILE RECORD
014410         INVALID KEY
                   MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
014420*            MOVE WF1-COMPANY        TO WS1-COMPANY
014430*            MOVE WF1-EMPLOYEE       TO WS1-EMPLOYEE
014430*            MOVE WF1-PLAN-TYPE      TO WS1-PLAN-TYPE
014430*            MOVE WF1-PLAN-CODE      TO WS1-PLAN-CODE
014440*            MOVE WF1-START-DATE     TO WS1-START-DATE
      *            MOVE "BN100"            TO CRT-ERROR-CAT
014480*            MOVE 151                TO CRT-MSG-NBR
014490*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BEN-ADD-FILE    TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 106                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BN100BEN-KEY    TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 107                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
                   MOVE WS-ERROR-CAT       TO CRT-ERROR-CAT
014530     END-DELETE.
014540
014550******************************************************************
014560 8300-END.
014570******************************************************************
014580
014590******************************************************************
014600 8400-FIND-BN100BENA              SECTION.
014610******************************************************************
014620 8400-START.
014630
014640     SET BN100BENA-FOUND              TO TRUE.
014650
014660     READ BN100BENA-FILE              KEY WF1-BN100BENA-KEY
014670         INVALID KEY
014680             SET BN100BENA-NOTFOUND   TO TRUE
014690     END-READ.
014700
014710******************************************************************
014720 8400-END.
014730******************************************************************
014740
014750******************************************************************
014760 8500-FIND-NLT-BN100BENA          SECTION.
014770******************************************************************
014780 8500-START.
014790
014800     SET BN100BENA-FOUND              TO TRUE.
014810
014820     START BN100BENA-FILE             KEY NOT < WF1-BN100BENA-KEY
014830         INVALID KEY
014840             SET BN100BENA-NOTFOUND   TO TRUE
014850     END-START.
014860
014870     IF (BN100BENA-FOUND)
014880         PERFORM 8600-FIND-NEXT-BN100BENA.
014890
014900******************************************************************
014910 8500-END.
014920******************************************************************
014930
014940******************************************************************
014950 8600-FIND-NEXT-BN100BENA         SECTION.
014960******************************************************************
014970 8600-START.
014980
014990     SET BN100BENA-FOUND              TO TRUE.
015000
015010     READ BN100BENA-FILE              NEXT RECORD
015020         AT END
015030             SET BN100BENA-NOTFOUND   TO TRUE
015040     END-READ.
015050
015060******************************************************************
015070 8600-END.
015080******************************************************************
015090
014030******************************************************************
014040 8000-CREATE-BN100EFDC           SECTION.
014050******************************************************************
014060 8000-START.
014070
014080     INITIALIZE BN100EFDC-REC.
014300
014310******************************************************************
014320 8000-END.
014330******************************************************************
014340
014350******************************************************************
014360 8200-STORE-BN100EFDC            SECTION.
014370******************************************************************
014380 8200-START.
014390
014400     WRITE BN100EFDC-REC
014410         INVALID KEY
                   MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
014420*            MOVE WF4-COMPANY        TO WS2-COMPANY
014430*            MOVE WF4-EMPLOYEE       TO WS2-EMPLOYEE
014440*            MOVE WF4-START-DATE     TO WS2-START-DATE
      *            MOVE "BN100"            TO CRT-ERROR-CAT
014480*            MOVE 105                TO CRT-MSG-NBR
014490*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-EFD-CHANGE-FILE TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 106                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BN100EFD-KEY    TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 107                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
                   MOVE WS-ERROR-CAT       TO CRT-ERROR-CAT
014530     END-WRITE.
014540
014550******************************************************************
014560 8200-END.
014570******************************************************************
014580
014350******************************************************************
014360 8200-REWRITE-BN100EFDC          SECTION.
014370******************************************************************
014380 8200-START.
014390
014400     REWRITE BN100EFDC-REC
014410         INVALID KEY
                   MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
014420*            MOVE WF4-COMPANY        TO WS2-COMPANY
014430*            MOVE WF4-EMPLOYEE       TO WS2-EMPLOYEE
014440*            MOVE WF4-START-DATE     TO WS2-START-DATE
      *            MOVE "BN100"            TO CRT-ERROR-CAT
014480*            MOVE 105                TO CRT-MSG-NBR
014490*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-EFD-CHANGE-FILE TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 106                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BN100EFD-KEY    TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 107                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
                   MOVE WS-ERROR-CAT       TO CRT-ERROR-CAT
014530     END-REWRITE.
014540
014550******************************************************************
014560 8200-END.
014570******************************************************************
014580
014350******************************************************************
014360 8300-DELETE-BN100EFDC           SECTION.
014370******************************************************************
014380 8300-START.
014390
014400     DELETE BN100EFDC-FILE RECORD
014410         INVALID KEY
                   MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
014420*            MOVE WF4-COMPANY        TO WS2-COMPANY
014430*            MOVE WF4-EMPLOYEE       TO WS2-EMPLOYEE
014440*            MOVE WF4-START-DATE     TO WS2-START-DATE
      *            MOVE "BN100"            TO CRT-ERROR-CAT
014480*            MOVE 152                TO CRT-MSG-NBR
014490*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-EFD-CHANGE-FILE TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 106                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BN100EFD-KEY    TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 107                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
                   MOVE WS-ERROR-CAT       TO CRT-ERROR-CAT
014530     END-DELETE.
014540
014550******************************************************************
014560 8300-END.
014570******************************************************************
014580
014590******************************************************************
014600 8400-FIND-BN100EFDC             SECTION.
014610******************************************************************
014620 8400-START.
014630
014640     SET BN100EFDC-FOUND              TO TRUE.
014650
014660     READ BN100EFDC-FILE              KEY WF4-BN100EFDC-KEY
014670         INVALID KEY
014680             SET BN100EFDC-NOTFOUND   TO TRUE
014690     END-READ.
014700
014710******************************************************************
014720 8400-END.
014730******************************************************************
014740
014750******************************************************************
014760 8500-FIND-NLT-BN100EFDC          SECTION.
014770******************************************************************
014780 8500-START.
014790
014800     SET BN100EFDC-FOUND              TO TRUE.
014810
014820     START BN100EFDC-FILE             KEY NOT < WF4-BN100EFDC-KEY
014830         INVALID KEY
014840             SET BN100EFDC-NOTFOUND   TO TRUE
014850     END-START.
014860
014870     IF (BN100EFDC-FOUND)
014880         PERFORM 8600-FIND-NEXT-BN100EFDC.
014890
014900******************************************************************
014910 8500-END.
014920******************************************************************
014930
014940******************************************************************
014950 8600-FIND-NEXT-BN100EFDC         SECTION.
014960******************************************************************
014970 8600-START.
014980
014990     SET BN100EFDC-FOUND              TO TRUE.
015000
015010     READ BN100EFDC-FILE              NEXT RECORD
015020         AT END
015030             SET BN100EFDC-NOTFOUND   TO TRUE
015040     END-READ.
015050
015060******************************************************************
015070 8600-END.
015080******************************************************************
015090
014030******************************************************************
014040 8000-CREATE-BN100EFDA            SECTION.
014050******************************************************************
014060 8000-START.
014070
014080     INITIALIZE BN100EFDA-REC.
014300
014310******************************************************************
014320 8000-END.
014330******************************************************************
014340
014350******************************************************************
014360 8200-STORE-BN100EFDA             SECTION.
014370******************************************************************
014380 8200-START.
014390
014400     WRITE BN100EFDA-REC
014410         INVALID KEY
                   MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
014420*            MOVE WF3-COMPANY        TO WS2-COMPANY
014430*            MOVE WF3-EMPLOYEE       TO WS2-EMPLOYEE
014440*            MOVE WF3-START-DATE     TO WS2-START-DATE
      *            MOVE "BN100"            TO CRT-ERROR-CAT
014480*            MOVE 150                TO CRT-MSG-NBR
014490*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-EFD-ADD-FILE    TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 106                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BN100EFD-KEY    TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 107                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
                   MOVE WS-ERROR-CAT       TO CRT-ERROR-CAT
014530     END-WRITE.
014540
014550******************************************************************
014560 8200-END.
014570******************************************************************
014580
014350******************************************************************
014360 8300-DELETE-BN100EFDA           SECTION.
014370******************************************************************
014380 8300-START.
014390
014400     DELETE BN100EFDA-FILE RECORD
014410         INVALID KEY
                   MOVE CRT-ERROR-CAT      TO WS-ERROR-CAT
014420*            MOVE WF3-COMPANY        TO WS2-COMPANY
014430*            MOVE WF3-EMPLOYEE       TO WS2-EMPLOYEE
014440*            MOVE WF3-START-DATE     TO WS2-START-DATE
      *            MOVE "BN100"            TO CRT-ERROR-CAT
014480*            MOVE 153                TO CRT-MSG-NBR
014490*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-EFD-CHANGE-FILE TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 106                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
014500*            MOVE WS-BN100EFD-KEY    TO WS-ERR-VARS
014500*            MOVE WS-ERR-VAR1        TO CRT-ERR-VAR1
014500*            MOVE WS-ERR-VAR2        TO CRT-ERR-VAR2
014500*            MOVE WS-ERR-VAR3        TO CRT-ERR-VAR3
014500*            MOVE WS-ERR-VAR4        TO CRT-ERR-VAR4
014500*            MOVE WS-ERR-VAR5        TO CRT-ERR-VAR5
014510*            MOVE 107                TO CRT-MSG-NBR
014520*            PERFORM 780-PRINT-MSG
                   MOVE WS-ERROR-CAT       TO CRT-ERROR-CAT
014530     END-DELETE.
014540
014550******************************************************************
014560 8300-END.
014570******************************************************************
014580
014590******************************************************************
014600 8400-FIND-BN100EFDA              SECTION.
014610******************************************************************
014620 8400-START.
014630
014640     SET BN100EFDA-FOUND              TO TRUE.
014650
014660     READ BN100EFDA-FILE              KEY WF3-BN100EFDA-KEY
014670         INVALID KEY
014680             SET BN100EFDA-NOTFOUND   TO TRUE
014690     END-READ.
014700
014710******************************************************************
014720 8400-END.
014730******************************************************************
014740
014750******************************************************************
014760 8500-FIND-NLT-BN100EFDA          SECTION.
014770******************************************************************
014780 8500-START.
014790
014800     SET BN100EFDA-FOUND              TO TRUE.
014810
014820     START BN100EFDA-FILE             KEY NOT < WF3-BN100EFDA-KEY
014830         INVALID KEY
014840             SET BN100EFDA-NOTFOUND   TO TRUE
014850     END-START.
014860
014870     IF (BN100EFDA-FOUND)
014880         PERFORM 8600-FIND-NEXT-BN100EFDA.
014890
014900******************************************************************
014910 8500-END.
014920******************************************************************
014930
014940******************************************************************
014950 8600-FIND-NEXT-BN100EFDA         SECTION.
014960******************************************************************
014970 8600-START.
014980
014990     SET BN100EFDA-FOUND              TO TRUE.
015000
015010     READ BN100EFDA-FILE              NEXT RECORD
015020         AT END
015030             SET BN100EFDA-NOTFOUND   TO TRUE
015040     END-READ.
015050
015060******************************************************************
015070 8600-END.
015080******************************************************************
GW0309 8700-MOVE-TO-TAIP-AUDIT          SECTION.
GW0309******************************************************************
GW0309
GW0309     MOVE SRT-EMPLOYEE           TO TAP-EMPLOYEE.
GW0309     MOVE SRT-PLAN-CODE          TO TAP-PLAN-CODE.
GW0309
GW0309     MOVE SRT-COMPANY            TO DB-COMPANY.
GW0309     INITIALIZE DB-EMP-APP.
GW0309     MOVE SRT-EMPLOYEE           TO DB-EMPLOYEE.
GW0309     MOVE "57"                   TO DB-FIELD-KEY.
GW0309     PERFORM 840-FIND-HEUSET1.
GW0309     IF (HREMPUSF-FOUND)
GW0309         MOVE HEU-A-FIELD        TO TAP-PROCESS-LEVEL
GW0309     ELSE
GW0309         PERFORM 840-FIND-EMPSET1
GW0309         IF (EMPLOYEE-FOUND)
GW0309            MOVE EMP-PROCESS-LEVEL TO TAP-PROCESS-LEVEL
GW0309         ELSE
GW0309            MOVE SPACES            TO TAP-PROCESS-LEVEL.
GW0309     
GW0309     MOVE G4-STOP-DATE           TO TAP-STOP-DATE-NEW.
GW0309     IF (SRT-FILE-PREFIX         = "BEN")
GW0309        MOVE BEN-STOP-DATE       TO TAP-STOP-DATE-OLD
GW0309     ELSE
GW0309        DISPLAY "SRT FILE PRX " SRT-FILE-PREFIX
GW0309        MOVE 99999999            TO TAP-STOP-DATE-OLD.
GW0309
GW0309     MOVE SRT-FUNCTION-CODE      TO TAP-ACTION.
GW0309     MOVE CRT-PROGRAM-CODE       TO TAP-USER-ID.
GW0309     MOVE WS-SYSTEM-DATE-YMD     TO TAP-DATE-STAMP.
GW0309     MOVE HHMMSS                 TO TAP-TIME-STAMP.
GW0309     MOVE "N"                    TO TAP-PROCESSED-FLAG.
GW0309     PERFORM 800-WRITECSV-TAIPAUDIT.
GW0309
GW0309 8700-END.
ACS003******************************************************************
ACS003 8900-WRITE-ZN325WK1-REC          SECTION.
ACS003******************************************************************
ACS003
ACS003     IF (A-WF1-PLAN-TYPE)
ACS003         NEXT SENTENCE
ACS003     ELSE 
ACS003         GO TO 8900-WK2-PLAN-TYPE.
ACS003
ACS004*    IF (WF1-PLAN-CODE = "ESMS" OR "ESMX")
ACS004     IF (WF1-PLAN-CODE = "ESMS")
ACS003         NEXT SENTENCE
ACS003     ELSE
ACS003         GO TO 8900-CONTINUE.
ACS003
ACS003     MOVE WF1-EMPLOYEE               TO WK1-EMPLOYEE.
ACS003     MOVE WF1-COMPANY                TO DB-COMPANY
ACS003                                        WK1-COMPANY.
ACS003     INITIALIZE DB-EMP-APP.
ACS003     MOVE WF1-EMPLOYEE               TO DB-EMPLOYEE.
ACS003     MOVE "57"                       TO DB-FIELD-KEY.
ACS003     PERFORM 840-FIND-HEUSET1.
ACS003     IF (HREMPUSF-FOUND)
ACS003         MOVE HEU-A-FIELD            TO WK1-PROCESS-LEVEL
ACS003     ELSE
ACS003         MOVE WF1-COMPANY            TO DB-COMPANY
ACS003         MOVE WF1-EMPLOYEE           TO DB-EMPLOYEE
ACS003         PERFORM 840-FIND-EMPSET1
ACS003         IF (EMPLOYEE-FOUND)
ACS003             MOVE EMP-PROCESS-LEVEL  TO WK1-PROCESS-LEVEL
ACS003         ELSE
ACS003             MOVE SPACES             TO WK1-PROCESS-LEVEL
ACS003         END-IF
ACS003     END-IF.
ACS003
ACS003     MOVE WF1-START-DATE             TO WK1-START-DATE.
ACS003     MOVE WF1-STOP-DATE              TO WK1-STOP-DATE.
ACS003     MOVE WF1-PLAN-CODE              TO WK1-PLAN-CODE.
ACS003*    MOVE CRT-USER-NAME              TO WK1-USER-ID.
ACS003     MOVE WS-SYSTEM-DATE-YMD         TO WK1-DATE-DELETED.      
ACS005
ACS005     MOVE WF1-COMPANY                TO DB-COMPANY.
ACS005     MOVE WF1-PLAN-TYPE              TO DB-PLAN-TYPE.
ACS005     MOVE WF1-EMPLOYEE               TO DB-EMPLOYEE.
ACS005     MOVE WF1-PLAN-CODE              TO DB-PLAN-CODE.
ACS005     MOVE WF1-START-DATE             TO DB-START-DATE.
ACS005     PERFORM 840-FIND-BENSET1.
ACS005     IF (BENEFIT-NOTFOUND)
ACS005         MOVE SPACES                 TO WK1-COV-OPTION
ACS005     ELSE
ACS005         MOVE BEN-COV-OPTION         TO WK1-COV-OPTION.
ACS003
ACS003     GO TO 8900-WRITE-RECORD.
ACS003
ACS003 8900-WK2-PLAN-TYPE.
ACS003
ACS004*    IF (WF2-PLAN-CODE = "ESMS" OR "ESMX")
ACS004     IF (WF2-PLAN-CODE = "ESMS")
ACS003         NEXT SENTENCE
ACS003     ELSE
ACS003         GO TO 8900-CONTINUE.
ACS003
ACS003     MOVE WF2-EMPLOYEE               TO WK1-EMPLOYEE.
ACS003     MOVE WF2-COMPANY                TO DB-COMPANY
ACS003                                        WK1-COMPANY.
ACS003     INITIALIZE DB-EMP-APP.
ACS003     MOVE WF2-EMPLOYEE               TO DB-EMPLOYEE.
ACS003     MOVE "57"                       TO DB-FIELD-KEY.
ACS003     PERFORM 840-FIND-HEUSET1.
ACS003     IF (HREMPUSF-FOUND)
ACS003         MOVE HEU-A-FIELD            TO WK1-PROCESS-LEVEL
ACS003     ELSE
ACS003         MOVE WF2-COMPANY            TO DB-COMPANY
ACS003         MOVE WF2-EMPLOYEE           TO DB-EMPLOYEE
ACS003         PERFORM 840-FIND-EMPSET1
ACS003         IF (EMPLOYEE-FOUND)
ACS003             MOVE EMP-PROCESS-LEVEL  TO WK1-PROCESS-LEVEL
ACS003         ELSE
ACS003             MOVE SPACES             TO WK1-PROCESS-LEVEL
ACS003         END-IF
ACS003     END-IF.
ACS003
ACS003     MOVE WF2-START-DATE             TO WK1-START-DATE.
ACS003     MOVE WF2-STOP-DATE              TO WK1-STOP-DATE.
ACS003     MOVE WF2-PLAN-CODE              TO WK1-PLAN-CODE.
ACS003*    MOVE CRT-USER-NAME              TO WK1-USER-ID.
ACS003     MOVE WS-SYSTEM-DATE-YMD         TO WK1-DATE-DELETED.      
ACS005
ACS005     MOVE WF2-COMPANY                TO DB-COMPANY.
ACS005     MOVE WF2-PLAN-TYPE              TO DB-PLAN-TYPE.
ACS005     MOVE WF2-EMPLOYEE               TO DB-EMPLOYEE.
ACS005     MOVE WF2-PLAN-CODE              TO DB-PLAN-CODE.
ACS005     MOVE WF2-START-DATE             TO DB-START-DATE.
ACS005     PERFORM 840-FIND-BENSET1.
ACS005     IF (BENEFIT-NOTFOUND)
ACS005         MOVE SPACES                 TO WK1-COV-OPTION
ACS005     ELSE
ACS005         MOVE BEN-COV-OPTION         TO WK1-COV-OPTION.
ACS003
ACS003 8900-WRITE-RECORD.
ACS003
ACS003     WRITE ZN325WK1-REC FROM WK1-ZN325WK1-REC. 
ACS003
ACS003 8900-CONTINUE.
ACS003
ACS003     SET NOT-WF1-PLAN-TYPE           TO TRUE. 
ACS003     SET NOT-WF2-PLAN-TYPE           TO TRUE. 
ACS003
ACS003 8900-END.
015090
