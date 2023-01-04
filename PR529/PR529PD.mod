******* PR529PD 8 <281575771>
      *** MODIFICATION LOG *********************************************
      ******************************************************************
      *  GW1003 7/2003 ADDED CODE TO OPEN PENPAYCSV FOR OUTPUT WHEN    *
      *                DONE                                            *
      ******************************************************************
      *  GW1104 11/2004 MOD WE ARE GOING TO SET UP A TAG ALONG         *
      *                 RECORD IN WBPPRPENPY FOR EACH RECORD IN        *
      *                 PENPAYCSV. TWO ADDN FIELDS WILL BE APPENDED TO *
      *                 INPUT FILE AND POPULATED IN TYPE 1 RECS.       *
      * ACS     8/2011  REAPPLIED ABOVE MODS AFTER 9.0.1 APP UPGRADE   *
000100******************************************************************
000200* PR529 - PENSION PAYMENT INTERFACE (CSV) WILL ONLY FUNCTION     *
000300* PROPERLY IF THE CSV INPUT FILE IS PRE-SORTED IN THE FOLLOWING  *
000400* ORDER:                                                         *
000700*          CSV-COMPANY                                           *
      *          CSV-EMPLOYEE
000500*          CSV-PENSION-ID                                        *
000600*          CSV-RECORD-TYPE                                       *
000800*                                                                *
000900* UNPREDICTABLE RESULTS WILL OCCUR OTHERWISE.                    *
001000******************************************************************
001100 050-EDIT-PARAMETERS             SECTION.
001200******************************************************************
001300 050-START.
001400
001500     MOVE PRM-COMPANY                TO DB-COMPANY.
001600     INITIALIZE DB-PROCESS-LEVEL.
001800     PERFORM 840-FIND-PRSSET1. 
002000     IF (PRSYSTEM-NOTFOUND)
      ******** Company does not exist
002100         MOVE PRM-COMPANY            TO CRT-ERR-VAR1
002200         MOVE 100                    TO CRT-ERROR-NBR
002300         MOVE WS-TRUE                TO WS-PARAMETER-ERROR
002500         PERFORM 780-PRINT-ERROR-MSG
002600     ELSE
002700         MOVE PRS-COMPANY            TO PH-COMPANY
002700                                        EH-COMPANY
002800         MOVE PRS-NAME               TO PH-NAME
002800                                        EH-NAME.
002900
007700 050-END.
007800     EXIT.

007900*****************************************************************
008000 100-PROGRAM-CONTROL             SECTION.
008100*****************************************************************
008200 100-START.
008300******************************************************************
008400*** WS-MAX-OPS-IN-TRAN controls how many PAYMENTS will be      ***
008500*** PROCESSED PER TRANSACTION                                  ***
009100***                                                            ***
009200*** The RECOMMENDED VALUE is 50. Raising the value too high    ***
009300*** may cause rollback segment size problems in SQL databases. ***
009400***                                                            ***
009500*** Change the value of WS-MAX-OPS-IN-TRAN through             ***
009600*** "Job Definition" (jobdef) "Logical Transaction Size".      ***
009700*** The default for WS-MAX-OPS-IN-TRAN is 50.                  ***
009800******************************************************************
009900
      **** Processing PR529 - Pension Payroll Conversion
010000     MOVE 052                        TO CRT-MSG-NBR.
010100     PERFORM 780-DISPLAY-MSG.
010200
010300     PERFORM 840-FIND-CKPSET1.
010400     IF (CKP-RESTART-INFO            NOT = SPACES)
010500         MOVE WS-TRUE                TO WS-RESTART-SW
010600         MOVE CKP-RESTART-INFO       TO WS-RESTART-INFO
      ******** Program restarting
010700         MOVE 055                    TO CRT-MSG-NBR
010800         PERFORM 780-DISPLAY-MSG
010900     ELSE
011000         MOVE WS-FALSE               TO WS-RESTART-SW
011100         INITIALIZE WS-RESTART-INFO.
011200
           IF  (WS-RS-COMPANY              = WS-HIGH-VALUES)
           AND (WS-RS-EMPLOYEE             = WS-HIGH-VALUES)
           AND (WS-RS-NBR-RECS             = WS-HIGH-VALUES)
               GO TO 100-PRINT-ERRORS.

012300     PERFORM 900-BUILD-TMP-FILE-NAME.
012400     MOVE WS-TMP-FILE                TO WS-ERRORFILE-NAME.
012500     OPEN OUTPUT ERROR-FILE.
012600
011600     IF  (WS-NOT-RESTARTING)
           AND (PRM-RERUN                  = "Y")
           AND (PRM-UPDATE-OPTION          = "U")
011300         PERFORM 800-OPENINPUTCSV-PENPAYCSV
011500
               PERFORM 150-CLEANUP-PRPENPAY

               PERFORM 800-CLOSECSV-PENPAYCSV.

011300     PERFORM 800-OPENINPUTCSV-PENPAYCSV.

011600     IF (WS-RESTARTING)
011700         PERFORM 800-OPENAPPENDCSV-PENOUTCSV
011800     ELSE
011900         PERFORM 800-OPENOUTPUTCSV-PENOUTCSV.

           PERFORM 9900-HR-PRINT-BLANK.
           MOVE CSVINFO-FILENAME           TO HR-PRT-LINE.
           PERFORM 9900-HR-PRINT-CSVIN.
           PERFORM 9900-HR-PRINT-BLANK.
           MOVE PNOINFO-FILENAME           TO HR-PRT-LINE.
           PERFORM 9900-HR-PRINT-CSVOUT.
           PERFORM 9900-HR-PRINT-BLANK.
012200
           IF (WS-RESTARTING)
               PERFORM 800-READCSV-PENPAYCSV
                   WS-RS-NBR-RECS TIMES

               MOVE WS-RS-NBR-RECS         TO WS-NBR-RECS
               MOVE WS-RS-TOT-EMP          TO WS-TE-TOT-EMPLOYEES
               MOVE WS-RS-TOT-ERRORS       TO WS-TOT-ERRORS
           ELSE
013100         PERFORM 800-READCSV-PENPAYCSV
013300         IF (PENPAYCSV-NOTFOUND)
      ************ No data to print in Pension Payroll Conversion Report
013400             MOVE 053                TO CRT-MSG-NBR
                   IF (PRM-BYPASS-REPORT   = "N")
013600                 PERFORM 780-PRINT-MSG
                   END-IF
014400             PERFORM 800-CLOSECSV-PENPAYCSV
014500             PERFORM 800-CLOSECSV-PENOUTCSV
                   CLOSE ERROR-FILE
014800             GO TO 100-PRINT-ERRORS
               END-IF
013800
012900         MOVE 1                          TO WS-NBR-RECS.
016200
      **** Processing pension payment csv
010700     MOVE 056                        TO CRT-MSG-NBR.
010800     PERFORM 780-DISPLAY-MSG.

           SET NO-DATA-FOUND               TO TRUE.

016300     PERFORM 910-AUDIT-BEGIN.
016400
013900     PERFORM 200-PROCESS-INTERFACE
014000         UNTIL (PENPAYCSV-NOTFOUND)
               OR    (QUIT-PROG).
014100
           IF (NO-DATA-FOUND)
      ******** No data to print in Pension Payroll Conversion Report
013400         MOVE 053                    TO CRT-MSG-NBR
               IF (PRM-BYPASS-REPORT       = "N")
013600             PERFORM 780-PRINT-MSG.

           MOVE WS-HIGH-VALUES             TO WS-RS-COMPANY
                                              WS-RS-EMPLOYEE
                                              WS-RS-NBR-RECS.
028800     PERFORM 400-UPDATE-CKPOINT.

018300     PERFORM 925-AUDIT-END.
018400
019300     IF (WS-TE-TOT-EMPLOYEES         > ZEROES)
067000         MOVE WS-TE-TOT-EMPLOYEES    TO TE-TOT-EMPLOYEES
067200
               IF (PRM-BYPASS-REPORT       = "N")
067300             MOVE TOTAL-EMPLOYEES    TO RPT-GROUP-REQUEST
067400             PERFORM 700-PRINT-RPT-GRP.
067500
014200     CLOSE ERROR-FILE SAVE.
014300
014400     PERFORM 800-CLOSECSV-PENPAYCSV.
014500     PERFORM 800-CLOSECSV-PENOUTCSV.
GW1003     IF (PRM-UPDATE-OPTION          = "U")
GW            PERFORM 800-OPENOUTPUTCSV-PENPAYCSV
GW            PERFORM 800-CLOSECSV-PENPAYCSV.
014600
GW1104     DISPLAY "WBPPRPENPY RECS ADDED " WS-TAG-ADD-CTR.
GW1104     DISPLAY "WBPPRPENPY RECS CHANGED " WS-TAG-CHG-CTR.
GW1104     DISPLAY "WBPPRPENPY RECS SKIPPED " WS-TAG-SKIP-CTR.
GW1104
014700 100-PRINT-ERRORS.
068400     OPEN INPUT ERROR-FILE.

           IF (PRM-BYPASS-REPORT           = "N")
014800         PERFORM 500-PRINT-ERRORS.
014900
071200     CLOSE ERROR-FILE.

071300     MOVE WS-ERRORFILE-NAME          TO WS-TMP-FILE.
071400     PERFORM 901-REMOVE-TMP-FILE.
071500
015000 100-END.
015100     EXIT.

020600*****************************************************************
020700 150-CLEANUP-PRPENPAY            SECTION.
020800*****************************************************************
020900 150-START.
021000
           INITIALIZE WS-NBR-OF-UPDS.

029200     PERFORM 910-AUDIT-BEGIN.

           PERFORM 800-READCSV-PENPAYCSV.
           PERFORM 152-SEL-COMPANY
               UNTIL (PENPAYCSV-NOTFOUND).

029100     PERFORM 925-AUDIT-END.

015000 150-END.
015100     EXIT.

020600*****************************************************************
020700 152-SEL-COMPANY                 SECTION.
020800*****************************************************************
020900 152-START.
021000
           IF (CSV-COMPANY                 NOT = PRM-COMPANY)
           OR (CSV-RECORD-TYPE             NOT = 1)
               GO TO 152-READ-NEXT-PENPAYCSV.

           MOVE CSV-COMPANY                TO DB-COMPANY.
           MOVE CSV-EMPLOYEE               TO DB-EMPLOYEE.
           MOVE CSV-EFFECT-DATE            TO DB-EFFECT-DATE.
           MOVE PNPSET3-EFFECT-DATE        TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-PNPSET3.
           PERFORM
               UNTIL (PRPENPAY-NOTFOUND)

007930         INITIALIZE PRPNP-SCR-FIELDS
007930                    PRPNP-SRC-TABLE-GROUP
007930                    PRPNP-TIME-TABLE-GROUP

               MOVE "D"                    TO PRPNP-FC

               MOVE PNP-COMPANY            TO PRPNP-COMPANY
               MOVE PNP-EMPLOYEE           TO PRPNP-EMPLOYEE
               MOVE PNP-EFFECT-DATE        TO PRPNP-EFFECT-DATE
               MOVE PNP-PENS-SEQ-NBR       TO PRPNP-PENS-SEQ-NBR
               PERFORM 2000-PRPNP-EDIT-TRAN
               IF (NO-ERROR-FOUND)
                   MOVE PNP-PENS-DIST-TYPE TO PRPNP-PENS-DIST-TYPE-SW
                   PERFORM 3000-PRPNP-PROCESS-TRAN
GW1104             PERFORM 600-PROCESS-TAG

                   ADD 10                  TO WS-NBR-OF-UPDS
                   IF (WS-NBR-OF-UPDS      > WS-MAX-OPS-IN-TRAN)
                       INITIALIZE WS-NBR-OF-UPDS
                       PERFORM 925-AUDIT-END
                       PERFORM 910-AUDIT-BEGIN
                   END-IF
               END-IF

               PERFORM 860-FIND-NXTRNG-PNPSET3
           END-PERFORM.

015000 152-READ-NEXT-PENPAYCSV.
           PERFORM 800-READCSV-PENPAYCSV.

015000 152-END.
015100     EXIT.

020600*****************************************************************
020700 200-PROCESS-INTERFACE           SECTION.
020800*****************************************************************
020900 200-START.
021000
           IF (CSV-COMPANY                 NOT = PRM-COMPANY)
               PERFORM 800-READCSV-PENPAYCSV

               ADD 1                       TO WS-NBR-RECS

               GO TO 200-END.

           SET DATA-FOUND                  TO TRUE.

           MOVE CSV-COMPANY                TO WS-COMPANY
                                              DB-COMPANY.

024600     PERFORM 210-SEL-EMPLOYEE
024700         UNTIL (PENPAYCSV-NOTFOUND)
               OR    (CSV-COMPANY          NOT = WS-COMPANY)
               OR    (QUIT-PROG).
025300
030800 200-END.
030900     EXIT.

038000******************************************************************
038100 210-SEL-EMPLOYEE                SECTION.
038200******************************************************************
038300 210-START.
038400
028800     PERFORM 400-UPDATE-CKPOINT.

           MOVE CSV-EMPLOYEE               TO WS-EMPLOYEE
                                              DB-EMPLOYEE
                                              PH-EMPLOYEE.
           PERFORM 840-FIND-EMPSET1.
           MOVE EMP-LAST-NAME              TO HRWS-LAST-NAME.
           MOVE EMP-FIRST-NAME             TO HRWS-FIRST-NAME.
           MOVE EMP-MIDDLE-NAME            TO HRWS-MIDDLE-NAME.
           MOVE EMP-LAST-NAME-PRE          TO HRWS-LAST-NAME-PRE.
           MOVE EMP-NAME-SUFFIX            TO HRWS-NAME-SUFFIX.
           PERFORM 750-HR-FORMAT-NAME.
           MOVE HRWS-FORMAT-NAME           TO PH-EMP-NAME.

           SET NO-PAYMENT-PROCESSED        TO TRUE.

           PERFORM 220-SEL-PENSION-ID
024700         UNTIL (PENPAYCSV-NOTFOUND)
               OR    (CSV-COMPANY          NOT = WS-COMPANY)
               OR    (CSV-EMPLOYEE         NOT = WS-EMPLOYEE)
               OR    (QUIT-PROG).

029600     IF  (PAYMENT-PROCESSED)
           AND (NO-QUIT-PROG)
029700         ADD 1                       TO WS-TE-TOT-EMPLOYEES.
029800
030800 210-END.
030900     EXIT.

038000******************************************************************
038100 220-SEL-PENSION-ID              SECTION.
038200******************************************************************
038300 220-START.
038400
           MOVE CSV-PENSION-ID             TO WS-PENSION-ID.

007930     INITIALIZE PRPNP-SCR-FIELDS
007930                PRPNP-SRC-TABLE-GROUP
007930                PRPNP-TIME-TABLE-GROUP
                      I1
                      I2
                      I3
                      I4
                      CRT-ERROR-NBR
                      CRT-ERROR-CAT
                      CRT-ERR-VAR1
                      CRT-ERR-VAR2
                      CRT-ERR-VAR3
                      CRT-ERR-VAR4
                      CRT-ERR-VAR5.
007940
           IF  (PRM-BYPASS-REPORT           = "N")
           AND (EMPLOYEE-FOUND)
069100         MOVE PAGE-HEADING           TO RPT-GROUP-REQUEST
069200         PERFORM 700-PRINT-RPT-GRP.
069300
           SET NO-ERR-RECS-WRITTEN         TO TRUE.

           PERFORM 230-SEL-RECORD-TYPE
024700         UNTIL (PENPAYCSV-NOTFOUND)
               OR    (CSV-COMPANY          NOT = WS-COMPANY)
               OR    (CSV-EMPLOYEE         NOT = WS-EMPLOYEE)
               OR    (CSV-PENSION-ID       NOT = WS-PENSION-ID)
               OR    (QUIT-PROG).

           IF (ERR-RECS-WRITTEN)
           OR (QUIT-PROG)
               GO TO 220-END.

      **** I1 REPRESENTS NO OF TYPE 1 RECORDS ****

           IF  (I1                         = ZEROES)
           AND (NO-ERR-RECS-WRITTEN)
      ******** Record type 1 does not exist for pension id
               MOVE 107                    TO CRT-MSG-NBR
               MOVE WS-IFC-PENSION-ID      TO CRT-ERR-VAR1
               PERFORM 320-WRITE-ERROR
               GO TO 220-END.

           MOVE I1                         TO WS-I1.
           MOVE I2                         TO WS-I2.
           MOVE I3                         TO WS-I3.
           MOVE I4                         TO WS-I4.

043400     PERFORM 2000-PRPNP-EDIT-TRAN.

           MOVE WS-I1                      TO I1.
           MOVE WS-I2                      TO I2.
           MOVE WS-I3                      TO I3.
           MOVE WS-I4                      TO I4.

045100     IF (ERROR-FOUND)
               PERFORM 320-WRITE-ERROR
               GO TO 220-END
           ELSE
045500     IF (PRM-UPDATE-OPTION           = "U")
045800         PERFORM 3000-PRPNP-PROCESS-TRAN
P68632         IF  (CRT-ERROR-NBR      NOT = ZEROES)
P68632             MOVE WS-IFC-COMPANY             TO ERR-COMPANY
P68632             MOVE WS-IFC-EMPLOYEE            TO ERR-EMPLOYEE
P68632             MOVE WS-IFC-PENSION-ID          TO ERR-PENSION-ID
P68632             MOVE WS-IFC-RECORD-TYPE         TO ERR-RECORD-TYPE
P68632             MOVE CRT-ERROR-NBR              TO CRT-MSG-NBR
P68632             PERFORM 790-GET-MSG
P68632             MOVE CRT-MESSAGE                TO ERR-MESSAGE
P68632             WRITE ERROR-REC                 FROM ERR-ERROR-REC
P68632         END-IF    
GW1104         PERFORM 600-PROCESS-TAG
P68632     END-IF.

           IF (PRM-BYPASS-REPORT           = "N")
048700         PERFORM 240-PRINT-PAYMENT-RECORD
           END-IF.
048800
           SET PAYMENT-PROCESSED           TO TRUE.
045900
030800 220-END.
030900     EXIT.

038000******************************************************************
038100 230-SEL-RECORD-TYPE             SECTION.
038200******************************************************************
038300 230-START.
038400
           PERFORM 300-VALIDATE-PENPAYCSV.
           IF (ERR-RECS-WRITTEN)
               IF (QUIT-PROG)
                   GO TO 230-END
               ELSE
                   PERFORM 350-WRITEPNO-FROMCSV
                   GO TO 230-NEXT-PENPAYCSV.

           IF (WS-IFC-RECORD-TYPE          = "1")
               PERFORM 232-MOVE-TYPE-1
           ELSE
           IF (WS-IFC-RECORD-TYPE          = "2")
               PERFORM 234-MOVE-TYPE-2
           ELSE
           IF (WS-IFC-RECORD-TYPE          = "3")
               PERFORM 236-MOVE-TYPE-3
           ELSE
           IF (WS-IFC-RECORD-TYPE          = "4")
               PERFORM 238-MOVE-TYPE-4.

030800 230-NEXT-PENPAYCSV.
           PERFORM 800-READCSV-PENPAYCSV.

           ADD 1                           TO WS-NBR-RECS.

030800 230-END.
030900     EXIT.

038000******************************************************************
038100 232-MOVE-TYPE-1                 SECTION.
038200******************************************************************
038300 232-START.
038400
007980     MOVE "A"                        TO PRPNP-FC.

008010     MOVE WS-IFC-COMPANY             TO PRPNP-COMPANY.
008030     MOVE WS-IFC-EMPLOYEE            TO PRPNP-EMPLOYEE.
008050     MOVE WS-IFC-EFFECT-DATE         TO PRPNP-EFFECT-DATE.
008070     MOVE WS-IFC-END-DATE            TO PRPNP-END-DATE.

008090     INITIALIZE PRPNP-PENS-SEQ-NBR.
008110
008310     MOVE WS-IFC-PENS-DIST-TYPE      TO PRPNP-PENS-DIST-TYPE
008310                                        PRPNP-PENS-DIST-TYPE-SW.
008340     MOVE WS-IFC-PENS-DIST-CODE      TO PRPNP-PENS-DIST-CODE.
008370     MOVE WS-IFC-PENS-DST-CODE2      TO PRPNP-PENS-DST-CODE2.
008400     MOVE WS-IFC-TAX-CATEGORY        TO PRPNP-TAX-CATEGORY.

008120     IF (WS-IFC-PROCESS-LEVEL        = SPACES)
008130         MOVE EMP-PROCESS-LEVEL      TO PRPNP-PROCESS-LEVEL
008140     ELSE
008150         MOVE WS-IFC-PROCESS-LEVEL   TO PRPNP-PROCESS-LEVEL.
008170
008180     IF (WS-IFC-DEPARTMENT           = SPACES)
008190         MOVE EMP-DEPARTMENT         TO PRPNP-DEPARTMENT
008200     ELSE
008210         MOVE WS-IFC-DEPARTMENT      TO PRPNP-DEPARTMENT.
008230
008420     MOVE WS-IFC-ADDL-FED-TAX        TO PRPNP-ADDL-FED-TAX.
008240     MOVE WS-IFC-TAX-FREQ-OVERRIDE   TO PRPNP-TAX-FREQ-OVER.
008260     MOVE WS-IFC-PROCESS-GRP         TO PRPNP-PROCESS-GRP.
008440     MOVE WS-IFC-PERC-TOT-DIST       TO PRPNP-PERC-TOT-DIST.
008470     MOVE WS-IFC-PAYABLE-TO          TO PRPNP-PAYABLE-TO.
008490     MOVE WS-IFC-FBO-NAME            TO PRPNP-FBO-NAME.
008510     MOVE WS-IFC-ADDR1               TO PRPNP-ADDR1.
008530     MOVE WS-IFC-ADDR2               TO PRPNP-ADDR2.
008550     MOVE WS-IFC-ADDR3               TO PRPNP-ADDR3.
008570     MOVE WS-IFC-ADDR4               TO PRPNP-ADDR4.
008590     MOVE WS-IFC-CITY                TO PRPNP-CITY.
008610     MOVE WS-IFC-STATE               TO PRPNP-STATE.
008630     MOVE WS-IFC-POSTAL-CODE         TO PRPNP-POSTAL-CODE.
008660     MOVE WS-IFC-COUNTRY-CODE        TO PRPNP-COUNTRY-CODE.
008670     MOVE WS-IFC-ROLLOVER-ACCT       TO PRPNP-ROLLOVER-ACCT.
008700     MOVE WS-IFC-MISCELLANEOUS       TO PRPNP-MISCELLANEOUS.
008730     MOVE WS-IFC-MISCELLANEOUS2      TO PRPNP-MISCELLANEOUS2.
008760     MOVE WS-IFC-EBANK-ID            TO PRPNP-EAD-EBANK-ID.
008780     MOVE WS-IFC-BANK-NAME           TO PRPNP-EAD-DESCRIPTION.
008800     MOVE WS-IFC-EBNK-ACCT-NBR       TO PRPNP-EAD-EBNK-ACCT-NBR.
008830     MOVE WS-IFC-ACCOUNT-TYPE        TO PRPNP-EAD-ACCOUNT-TYPE.
008850     MOVE WS-IFC-PAYMENT-DESCRIPTION TO PRPNP-EAD-CHECK-DESC.
008870     MOVE WS-IFC-WIRE-IND            TO PRPNP-WIRE-IND.
008890     MOVE WS-IFC-CA-EFT              TO PRPNP-CA-EFT.
P68632     MOVE WS-IFC-ROTH-YEAR           TO PRPNP-ROTH-YEAR.
J93849     MOVE WS-IFC-IAT-ACH             TO PRPNP-PNP-IAT.
008910
030800 232-END.
030900     EXIT.

038000******************************************************************
038100 234-MOVE-TYPE-2                 SECTION.
038200******************************************************************
038300 234-START.
038400
           IF (I2                          > PRPNP-SRC-LINES)
               GO TO 234-END.

007420     MOVE WS-IFC-FUNDING-SOURCE      TO
                                           PRPNP-STB-FUNDING-SOURCE(I2).
007440     MOVE WS-IFC-FUND-AMOUNT         TO
                                           PRPNP-STB-FUND-AMOUNT (I2).

030800 234-END.
030900     EXIT.

038000******************************************************************
038100 236-MOVE-TYPE-3                 SECTION.
038200******************************************************************
038300 236-START.
038400
           IF (I3                          > PRPNP-TIME-LINES)
               GO TO 236-END.

           MOVE "A"                        TO PRPNP-TTB-LINE-FC (I3).

009290     MOVE WS-IFC-PAY-CODE            TO PRPNP-TTB-PAY-CODE (I3).
009350     MOVE CSV-AMOUNT-RATE            TO PRPNP-TTB-RATE (I3).

030800 236-END.
030900     EXIT.

038000******************************************************************
038100 238-MOVE-TYPE-4                 SECTION.
038200******************************************************************
038300 238-START.
038400
009290     MOVE WS-IFC-EBANK-ID            TO PRPNP-EAD-EBANK-ID.
009290     MOVE WS-IFC-BANK-NAME           TO PRPNP-EAD-DESCRIPTION.
009290     MOVE WS-IFC-EBNK-ACCT-NBR       TO PRPNP-EAD-EBNK-ACCT-NBR.
009290     MOVE WS-IFC-ACCOUNT-TYPE        TO PRPNP-EAD-ACCOUNT-TYPE.
009290     MOVE WS-IFC-PAYMENT-DESCRIPTION TO PRPNP-EAD-CHECK-DESC.

030800 238-END.
030900     EXIT.

054500******************************************************************
054600 240-PRINT-PAYMENT-RECORD        SECTION.
054700******************************************************************
054800 240-START.
054900
008050     MOVE WS-IFC-PENSION-ID          TO PL-PENSION-ID.

008050     MOVE PRPNP-EFFECT-DATE          TO PL-EFFECT-DATE.
008070     MOVE PRPNP-END-DATE             TO PL-END-DATE.
008310     MOVE PRPNP-PENS-DIST-TYPE       TO PL-PENS-DIST-TYPE.
008340     MOVE PRPNP-PENS-DIST-CODE       TO PL-PENS-DIST-CODE.
008370     MOVE PRPNP-PENS-DST-CODE2       TO PL-PENS-DIST-CODE2.
008400     MOVE PRPNP-TAX-CATEGORY         TO PL-TAX-CATEGORY.
008310     MOVE PRPNP-PROCESS-LEVEL        TO PL-PROCESS-LEVEL.
008310     MOVE PRPNP-DEPARTMENT           TO PL-DEPARTMENT.
008310     MOVE PRPNP-ADDL-FED-TAX         TO PL-ADDL-FED-TAX.
008240     MOVE PRPNP-TAX-FREQ-OVER        TO PL-TAX-FREQ-OVER.
008260     MOVE PRPNP-PROCESS-GRP          TO PL-PROCESS-GRP.
008440     MOVE PRPNP-PERC-TOT-DIST        TO PL-PERC-TOT-DIST.
P68632     MOVE PRPNP-ROTH-YEAR            TO PL-ROTH-YEAR.
J93849     MOVE PRPNP-PNP-IAT              TO PL-IAT-ACH.

057600     MOVE PID-REC-TYPE-1             TO RPT-GROUP-REQUEST.
057700     PERFORM 700-PRINT-RPT-GRP.
058200
           PERFORM
               VARYING I1 FROM 1 BY 1
               UNTIL  (PRPNP-STB-FUNDING-SOURCE (I1)
                                           = SPACES)
               AND    (PRPNP-TTB-PAY-CODE (I1)
                                           = SPACES)

               INITIALIZE PL-FUNDING-SOURCE
                          PL-FUND-AMOUNT
                          PL-PAY-CODE
                          PL-AMOUNT-RATE

               IF (I1                      <= PRPNP-SRC-LINES)
007420             MOVE PRPNP-STB-FUNDING-SOURCE (I1)
                                           TO PL-FUNDING-SOURCE
007420             MOVE PRPNP-STB-FUND-AMOUNT (I1)
                                           TO PL-FUND-AMOUNT
               END-IF

               IF (I1                      <= PRPNP-TIME-LINES)
007420             MOVE PRPNP-TTB-PAY-CODE (I1)
                                           TO PL-PAY-CODE
007420             MOVE PRPNP-TTB-RATE (I1)
                                           TO PL-AMOUNT-RATE
               END-IF

057600         MOVE PID-REC-TYPE-2-3       TO RPT-GROUP-REQUEST
057700         PERFORM 700-PRINT-RPT-GRP
           END-PERFORM.

           MOVE PRPNP-PAYABLE-TO           TO PL-PAYABLE-TO.
           MOVE PRPNP-FBO-NAME             TO PL-FBO-NAME.
           MOVE PRPNP-ADDR1                TO PL-ADDR1.
           MOVE PRPNP-ADDR2                TO PL-ADDR2.
           MOVE PRPNP-ADDR3                TO PL-ADDR3.
           MOVE PRPNP-ADDR4                TO PL-ADDR4.
           MOVE PRPNP-CITY                 TO PL-CITY.
           MOVE PRPNP-STATE                TO PL-STATE.
           MOVE PRPNP-POSTAL-CODE          TO PL-POSTAL-CODE.
           MOVE PRPNP-COUNTRY-CODE         TO PL-COUNTRY-CODE.
           MOVE PRPNP-ROLLOVER-ACCT        TO PL-ROLLOVER-ACCT.
           MOVE PRPNP-MISCELLANEOUS        TO PL-MISCELLANEOUS.
           MOVE PRPNP-MISCELLANEOUS2       TO PL-MISCELLANEOUS2.

009290     MOVE PRPNP-EAD-EBANK-ID         TO PL-EBANK-ID.
009290     MOVE PRPNP-EAD-DESCRIPTION      TO PL-BANK-NAME.
009290     MOVE PRPNP-EAD-EBNK-ACCT-NBR    TO PL-EBNK-ACCT-NBR.
009290     MOVE PRPNP-EAD-ACCOUNT-TYPE     TO PL-ACCOUNT-TYPE.
009290     MOVE PRPNP-EAD-CHECK-DESC       TO PL-PAYMENT-DESCRIPTION.
           MOVE PRPNP-WIRE-IND             TO PL-WIRE-IND.
           MOVE PRPNP-CA-EFT               TO PL-CA-EFT.

004210     IF (PRPNP-WARNING-FLAG          = WS-TRUE)
      ******** Add complete - Employee not setup for auto time record
004220         MOVE 110                    TO CRT-MSG-NBR
               INITIALIZE CRT-ERROR-CAT
004310     ELSE
           IF (CRT-MSG-NBR                 NOT = ZEROES)
               INITIALIZE CRT-MESSAGE.

           INITIALIZE PL-WARNING.
004210     IF (PRPNP-WARNING-FLAG          = WS-TRUE)
           OR (CRT-MSG-NBR                 NOT = ZEROES)
004300         PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE            TO PL-WARNING.

057600     MOVE PID-REC-TYPE-4             TO RPT-GROUP-REQUEST.
057700     PERFORM 700-PRINT-RPT-GRP.

060100 240-END.
060200     EXIT.

038000******************************************************************
038100 300-VALIDATE-PENPAYCSV          SECTION.
038200******************************************************************
038300 300-START.
038400
           PERFORM 310-MOVE-TO-WS.

      **** EDIT RECORD TYPE
           IF (WS-IFC-RECORD-TYPE          NOT = "1" AND "2" AND "3" AND
                                                 "4")
      ******** Record-type is not valid
               MOVE 101                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR
               GO TO 300-END.

           IF (WS-IFC-RECORD-TYPE          = "1")
               ADD 1                       TO I1
               IF (I1                      = 2)
      ************ Multiple type 1 records exist for a pension
                   MOVE 105                TO CRT-MSG-NBR
                   MOVE WS-IFC-PENSION-ID  TO CRT-ERR-VAR1
                   PERFORM 320-WRITE-ERROR
                   GO TO 300-END
               END-IF
           ELSE
           IF (WS-IFC-RECORD-TYPE          = "2")
               ADD 1                       TO I2
           ELSE
           IF (WS-IFC-RECORD-TYPE          = "3")
               ADD 1                       TO I3
           ELSE
           IF (WS-IFC-RECORD-TYPE          = "4")
               ADD 1                       TO I4
               IF (I4                      = 2)
      ************ Multiple type 4 records exist for a pension
                   MOVE 106                TO CRT-MSG-NBR
                   MOVE WS-IFC-PENSION-ID  TO CRT-ERR-VAR1
                   PERFORM 320-WRITE-ERROR
                   GO TO 300-END.

      **** EDIT PENSION ID
           IF (WS-IFC-PENSION-ID-A         NOT NUMERIC)
      ******** pension-id
               MOVE 211                    TO CRT-MSG-NBR
               MOVE CRT-PROGRAM-CODE       TO CRT-ERROR-CAT
               PERFORM 790-GET-MSG
      ******** Non numeric data in field
               MOVE CRT-MESSAGE            TO CRT-ERR-VAR1
               MOVE 200                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR
               GO TO 300-END
           ELSE
           IF (WS-IFC-PENSION-ID           = ZEROES)
      ******** Pension-id is required
               MOVE 303                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR
               GO TO 300-END.

       300-CSV-SIGNED-FIELD-EDIT.
      ***** CSV Change - environment edit added for signed fields
           IF  (CSVINFO-ERROR     = "Y")
           AND (CSVINFO-ERROR-NBR = 203)
               MOVE 222                    TO CRT-ERROR-NBR
               MOVE CSVINFO-FIELD-NBR      TO CRT-ERR-VAR1
               MOVE CSVINFO-RECORD-COUNT   TO CRT-ERR-VAR2
               INSPECT CRT-ERR-VAR1 REPLACING LEADING "0" BY " "
               INSPECT CRT-ERR-VAR2 REPLACING LEADING "0" BY " "
               MOVE CRT-PROGRAM-CODE       TO CRT-ERROR-CAT
               PERFORM 320-WRITE-ERROR
               GO TO 300-END.

           IF (WS-IFC-RECORD-TYPE          = "1")
               CONTINUE
           ELSE
           IF (WS-IFC-RECORD-TYPE          = "2")
               GO TO 300-FUNDING-SOURCE
           ELSE
           IF (WS-IFC-RECORD-TYPE          = "3")
               GO TO 300-PAY-CODE
           ELSE
           IF (WS-IFC-RECORD-TYPE          = "4")
               GO TO 300-EBANK-ID.

       300-COMPANY.
           IF (WS-IFC-COMPANY-A            NOT NUMERIC)
      ******** company
               MOVE 210                    TO CRT-MSG-NBR
               MOVE CRT-PROGRAM-CODE       TO CRT-ERROR-CAT
               PERFORM 790-GET-MSG
      ******** Non numeric data in field
               MOVE CRT-MESSAGE            TO CRT-ERR-VAR1
               MOVE 200                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR
           ELSE
           IF (WS-IFC-COMPANY              = ZEROES)
      ******** Company is required
               MOVE 300                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR
           ELSE
           IF (WS-IFC-COMPANY              NOT = PRM-COMPANY)
      ******** Company does not match parameter company
               MOVE 102                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR.

       300-EMPLOYEE.
           IF (WS-IFC-EMPLOYEE-A           NOT NUMERIC)
      ******** employee
               MOVE 211                    TO CRT-MSG-NBR
               MOVE CRT-PROGRAM-CODE       TO CRT-ERROR-CAT
               PERFORM 790-GET-MSG
      ******** Non numeric data in field
               MOVE CRT-MESSAGE            TO CRT-ERR-VAR1
               MOVE 200                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR
           ELSE
           IF (WS-IFC-EMPLOYEE             = ZEROES)
      ******** Employee is required
               MOVE 301                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR.

       300-EFFECT-DATE.
053500     IF (WS-IFC-EFFECT-DATE-A        NOT NUMERIC)
      ******** effective-date
               MOVE 212                    TO CRT-MSG-NBR
               MOVE CRT-PROGRAM-CODE       TO CRT-ERROR-CAT
               PERFORM 790-GET-MSG
      ******** Non numeric data in field
               MOVE CRT-MESSAGE            TO CRT-ERR-VAR1
               MOVE 200                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR
054100     ELSE
049600     IF (WS-IFC-EFFECT-DATE          = ZEROES)
049700******** Effective-date is required
049800         MOVE 302                    TO CRT-MSG-NBR
049900         PERFORM 320-WRITE-ERROR
050100     ELSE
050300         MOVE WS-IFC-EFFECT-DATE     TO WSDR-FR-DATE
050400         PERFORM 900-IS-DATE-INVALID
050500         IF (WSDR-DATE-ERROR-EXISTS)
050600************ Effective-date \ is invalid
050700             MOVE 103                TO CRT-MSG-NBR
050800             MOVE WS-IFC-EFFECT-DATE TO CRT-ERR-VAR1
049900             PERFORM 320-WRITE-ERROR.

       300-END-DATE.
053500     IF (WS-IFC-END-DATE-A           NOT NUMERIC)
      ******** end-date
               MOVE 214                    TO CRT-MSG-NBR
               MOVE CRT-PROGRAM-CODE       TO CRT-ERROR-CAT
               PERFORM 790-GET-MSG
      ******** Non numeric data in field
               MOVE CRT-MESSAGE            TO CRT-ERR-VAR1
               MOVE 200                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR
054100     ELSE
049600     IF (WS-IFC-END-DATE             = ZEROES)
049700******** End-date is required
049800         MOVE 304                    TO CRT-MSG-NBR
049900         PERFORM 320-WRITE-ERROR
050100     ELSE
050300         MOVE WS-IFC-END-DATE        TO WSDR-FR-DATE
050400         PERFORM 900-IS-DATE-INVALID
050500         IF (WSDR-DATE-ERROR-EXISTS)
050600************ End-date \ is invalid
050700             MOVE 104                TO CRT-MSG-NBR
050800             MOVE WS-IFC-END-DATE    TO CRT-ERR-VAR1
049900             PERFORM 320-WRITE-ERROR.

       300-PENS-DIST-TYPE.
           IF (WS-IFC-PENS-DIST-TYPE-A     NOT NUMERIC)
      ******** pens-dist-type
               MOVE 211                    TO CRT-MSG-NBR
               MOVE CRT-PROGRAM-CODE       TO CRT-ERROR-CAT
               PERFORM 790-GET-MSG
      ******** Non numeric data in field
               MOVE CRT-MESSAGE            TO CRT-ERR-VAR1
               MOVE 200                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR
           ELSE
           IF (WS-IFC-PENS-DIST-TYPE       = ZEROES)
      ******** Pens-dist-type is required
               MOVE 305                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR.

       300-TAX-CATEGORY.
           IF (WS-IFC-TAX-CATEGORY-A       NOT NUMERIC)
      ******** tax-category
               MOVE 216                    TO CRT-MSG-NBR
               MOVE CRT-PROGRAM-CODE       TO CRT-ERROR-CAT
               PERFORM 790-GET-MSG
      ******** Non numeric data in field
               MOVE CRT-MESSAGE            TO CRT-ERR-VAR1
               MOVE 200                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR.

       300-ADDL-FED-TAX.
      ***** CSV Change - non numeric edit was not working for signed fields
      *    IF (WS-IFC-ADDL-FED-TAX-A       NOT NUMERIC)
      ******** addl-fed-tax
      *        MOVE 217                    TO CRT-MSG-NBR
      *        MOVE CRT-PROGRAM-CODE       TO CRT-ERROR-CAT
      *        PERFORM 790-GET-MSG
      ******** Non numeric data in field
      *        MOVE CRT-MESSAGE            TO CRT-ERR-VAR1
      *        MOVE 200                    TO CRT-MSG-NBR
      *        PERFORM 320-WRITE-ERROR
      *    ELSE
           IF (CSV-ADDL-FED-TAX            < ZEROES)
      ******** Additional federal tax amount cannot be negative
               MOVE 108                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR.

       300-PERC-TOT-DIST.
           IF (WS-IFC-PERC-TOT-DIST-A      NOT NUMERIC)
      ******** perc-tot-dist
               MOVE 218                    TO CRT-MSG-NBR
               MOVE CRT-PROGRAM-CODE       TO CRT-ERROR-CAT
               PERFORM 790-GET-MSG
      ******** Non numeric data in field
               MOVE CRT-MESSAGE            TO CRT-ERR-VAR1
               MOVE 200                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR.

       300-WIRE-IND.
           IF (WS-IFC-WIRE-IND-A           NOT NUMERIC)
      ******** wire-ind
               MOVE 219                    TO CRT-MSG-NBR
               MOVE CRT-PROGRAM-CODE       TO CRT-ERROR-CAT
               PERFORM 790-GET-MSG
      ******** Non numeric data in field
               MOVE CRT-MESSAGE            TO CRT-ERR-VAR1
               MOVE 200                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR.

GW1104 300-BEN-START-DATE.
GW1104     IF (WS-IFC-BEN-START-DATE-A NOT NUMERIC)
GW1104         MOVE 317                    TO CRT-MSG-NBR
GW1104         MOVE CRT-PROGRAM-CODE       TO CRT-ERROR-CAT
GW1104         PERFORM 790-GET-MSG
GW1104******** Non numeric data in field
GW1104         MOVE CRT-MESSAGE            TO CRT-ERR-VAR1
GW1104         MOVE 200                    TO CRT-MSG-NBR
GW1104         PERFORM 320-WRITE-ERROR
GW1104     ELSE
GW1104     IF (WS-IFC-BEN-START-DATE             = ZEROES)
GW1104******** BEN-START-DATE is required FOR TAG ALONG
GW1104         MOVE 318                    TO CRT-MSG-NBR
GW1104         PERFORM 320-WRITE-ERROR
GW1104     ELSE
GW1104         MOVE WS-IFC-BEN-START-DATE        TO WSDR-FR-DATE
GW1104         PERFORM 900-IS-DATE-INVALID
GW1104         IF (WSDR-DATE-ERROR-EXISTS)
GW1104************ BEN-START-DATE \ is invalid
GW1104             MOVE 319                      TO CRT-MSG-NBR
GW1104             MOVE WS-IFC-BEN-START-DATE    TO CRT-ERR-VAR1
GW1104             PERFORM 320-WRITE-ERROR.
GW1104
GW1104 300-BEN-STOP-RSN.
GW1104     IF (WS-IFC-BEN-STOP-RSN         = SPACES)
GW1104******** BEN-STOP-RSN is required FOR TAG ALONG
GW1104         MOVE 320                    TO CRT-MSG-NBR
GW1104         PERFORM 320-WRITE-ERROR.
GW1104
P68632 300-ROTH-YEAR.
P68632     IF (WS-IFC-ROTH-YEAR-A          NOT NUMERIC)
P68632******** roth initial contr year
P68632         MOVE 223                    TO CRT-MSG-NBR
P68632         MOVE CRT-PROGRAM-CODE       TO CRT-ERROR-CAT
P68632         PERFORM 790-GET-MSG
P68632******** Non numeric data in field
P68632         MOVE CRT-MESSAGE            TO CRT-ERR-VAR1
P68632         MOVE 200                    TO CRT-MSG-NBR
P68632         PERFORM 320-WRITE-ERROR.
J93849
J93849 300-IAT-ACH.
J93849     IF (WS-IFC-IAT-ACH = SPACES)
J93849         MOVE "N"                    TO WS-IFC-IAT-ACH.
J93849
J93849     IF (WS-IFC-IAT-ACH NOT = "N" AND "Y")
J93849******** IA-ACH must be "Y" or "N".
J93849         MOVE 317                    TO CRT-MSG-NBR
J93849         PERFORM 320-WRITE-ERROR.

           GO TO 300-END.

       300-FUNDING-SOURCE.
           IF (WS-IFC-FUNDING-SOURCE       = SPACES)
      ******** Funding-Source is required
               MOVE 307                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR.

       300-FUND-AMOUNT.
      ***** CSV Change - non numeric edit was not working for signed fields
      *    IF (WS-IFC-FUND-AMOUNT-A        NOT NUMERIC)
      ******** fund-amount
      *        MOVE 220                    TO CRT-MSG-NBR
      *        MOVE CRT-PROGRAM-CODE       TO CRT-ERROR-CAT
      *        PERFORM 790-GET-MSG
      ******** Non numeric data in field
      *        MOVE CRT-MESSAGE            TO CRT-ERR-VAR1
      *        MOVE 200                    TO CRT-MSG-NBR
      *        PERFORM 320-WRITE-ERROR
      *    ELSE
           IF (WS-IFC-FUND-AMOUNT          = ZEROES)
      ******** Fund-Amount is required
               MOVE 314                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR
           ELSE
           IF (CSV-FUND-AMOUNT             < ZEROES)
      ******** Fund-Amount cannot be negative
               MOVE 109                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR.

           GO TO 300-END.

       300-PAY-CODE.
           IF (WS-IFC-PAY-CODE             = SPACES)
      ******** Pay-code is required
               MOVE 308                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR.

      
       300-AMOUNT-RATE.
      ***** CSV Change - non numeric edit was not working for signed fields
      *     IF (WS-IFC-AMOUNT-RATE-A        NOT NUMERIC)
      ******** amount-rate
      *         MOVE 221                    TO CRT-MSG-NBR
      *         MOVE CRT-PROGRAM-CODE       TO CRT-ERROR-CAT
      *         PERFORM 790-GET-MSG
      ******** Non numeric data in field
      *         MOVE CRT-MESSAGE            TO CRT-ERR-VAR1
      *         MOVE 200                    TO CRT-MSG-NBR
      *         PERFORM 320-WRITE-ERROR
      *     ELSE
           IF (WS-IFC-AMOUNT-RATE          = ZEROES)
      ******** Amount-Rate is required
               MOVE 315                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR.

           GO TO 300-END.

       300-EBANK-ID.
           IF (WS-IFC-EBANK-ID-A           NOT NUMERIC)
      ******** ebank-id
               MOVE 218                    TO CRT-MSG-NBR
               MOVE CRT-PROGRAM-CODE       TO CRT-ERROR-CAT
               PERFORM 790-GET-MSG
      ******** Non numeric data in field
               MOVE CRT-MESSAGE            TO CRT-ERR-VAR1
               MOVE 200                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR
           ELSE
           IF (WS-IFC-EBANK-ID             = ZEROES)
      ******** Ebank-id is required
               MOVE 309                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR.

       300-BANK-NAME.
           IF (WS-IFC-BANK-NAME            = SPACES)
      ******** Bank-name is required
               MOVE 310                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR.

       300-EBNK-ACCT-NBR.
           IF (WS-IFC-EBNK-ACCT-NBR        = SPACES)
      ******** Ebnk-acct-nbr is required
               MOVE 311                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR.

       300-ACCOUNT-TYPE.
           IF (WS-IFC-ACCOUNT-TYPE         = SPACES)
      ******** Account-type is required
               MOVE 312                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR.

       300-PAYMENT-DESCRIPTION.
           IF (WS-IFC-PAYMENT-DESCRIPTION  = SPACES)
      ******** Payment-description is required
               MOVE 313                    TO CRT-MSG-NBR
               PERFORM 320-WRITE-ERROR.

030800 300-END.
030900     EXIT.

050400******************************************************************
050500 310-MOVE-TO-WS                 SECTION.
050600******************************************************************
050700 310-START.
050800
           MOVE CSV-COMPANY                TO WS-IFC-COMPANY.
           MOVE CSV-EMPLOYEE               TO WS-IFC-EMPLOYEE.
           MOVE CSV-PENSION-ID             TO WS-IFC-PENSION-ID.
           MOVE CSV-RECORD-TYPE            TO WS-IFC-RECORD-TYPE.
           MOVE CSV-EFFECT-DATE            TO WS-IFC-EFFECT-DATE.
           MOVE CSV-END-DATE               TO WS-IFC-END-DATE.
           MOVE CSV-PENS-DIST-TYPE         TO WS-IFC-PENS-DIST-TYPE.
           MOVE CSV-PENS-DIST-CODE         TO WS-IFC-PENS-DIST-CODE.
           MOVE CSV-PENS-DST-CODE2         TO WS-IFC-PENS-DST-CODE2.
           MOVE CSV-TAX-CATEGORY           TO WS-IFC-TAX-CATEGORY.
           MOVE CSV-PROCESS-LEVEL          TO WS-IFC-PROCESS-LEVEL.
           MOVE CSV-DEPARTMENT             TO WS-IFC-DEPARTMENT.
           MOVE CSV-ADDL-FED-TAX           TO WS-IFC-ADDL-FED-TAX.
           MOVE CSV-TAX-FREQ-OVERRIDE      TO WS-IFC-TAX-FREQ-OVERRIDE.
           MOVE CSV-PROCESS-GRP            TO WS-IFC-PROCESS-GRP.
           MOVE CSV-PERC-TOT-DIST          TO WS-IFC-PERC-TOT-DIST.
           MOVE CSV-PAYABLE-TO             TO WS-IFC-PAYABLE-TO.
           MOVE CSV-FBO-NAME               TO WS-IFC-FBO-NAME.
           MOVE CSV-ADDR1                  TO WS-IFC-ADDR1.
           MOVE CSV-ADDR2                  TO WS-IFC-ADDR2.
           MOVE CSV-ADDR3                  TO WS-IFC-ADDR3.
           MOVE CSV-ADDR4                  TO WS-IFC-ADDR4.
           MOVE CSV-CITY                   TO WS-IFC-CITY.
           MOVE CSV-STATE                  TO WS-IFC-STATE.
           MOVE CSV-POSTAL-CODE            TO WS-IFC-POSTAL-CODE.
           MOVE CSV-COUNTRY-CODE           TO WS-IFC-COUNTRY-CODE.
           MOVE CSV-ROLLOVER-ACCT          TO WS-IFC-ROLLOVER-ACCT.
           MOVE CSV-MISCELLANEOUS          TO WS-IFC-MISCELLANEOUS.
           MOVE CSV-MISCELLANEOUS2         TO WS-IFC-MISCELLANEOUS2.
           MOVE CSV-WIRE-IND               TO WS-IFC-WIRE-IND.
           MOVE CSV-CA-EFT                 TO WS-IFC-CA-EFT.
           MOVE CSV-FUNDING-SOURCE         TO WS-IFC-FUNDING-SOURCE.
           MOVE CSV-FUND-AMOUNT            TO WS-IFC-FUND-AMOUNT.
           MOVE CSV-PAY-CODE               TO WS-IFC-PAY-CODE.
           MOVE CSV-AMOUNT-RATE            TO WS-IFC-AMOUNT-RATE.
           MOVE CSV-EBANK-ID               TO WS-IFC-EBANK-ID.
           MOVE CSV-BANK-NAME              TO WS-IFC-BANK-NAME.
           MOVE CSV-EBNK-ACCT-NBR          TO WS-IFC-EBNK-ACCT-NBR.
           MOVE CSV-ACCOUNT-TYPE           TO WS-IFC-ACCOUNT-TYPE.
           MOVE CSV-PAYMENT-DESCRIPTION   TO WS-IFC-PAYMENT-DESCRIPTION.
GW1104     IF (WS-IFC-RECORD-TYPE = "1")
GW1104        MOVE CSV-BEN-START-DATE         TO WS-IFC-BEN-START-DATE
GW1104        MOVE CSV-BEN-STOP-RSN           TO WS-IFC-BEN-STOP-RSN.
GW1104
P68632     MOVE CSV-ROTH-INITIAL-CONTR-YEAR TO WS-IFC-ROTH-YEAR.
J93849     MOVE CSV-IAT-ACH                TO WS-IFC-IAT-ACH.

054300 310-END.
           EXIT.
054400
050400******************************************************************
050500 320-WRITE-ERROR                SECTION.
050600******************************************************************
050700 320-START.
050800
           ADD 1                           TO WS-TOT-ERRORS.
           IF  (WS-TOT-ERRORS              > PRM-ERROR-LIMIT)
           AND (PRM-ERROR-LIMIT            NOT = ZEROES)
               SET QUIT-PROG               TO TRUE
               GO TO 320-END.

           MOVE WS-IFC-COMPANY             TO ERR-COMPANY.
027000     MOVE WS-IFC-EMPLOYEE            TO ERR-EMPLOYEE.
           MOVE WS-IFC-PENSION-ID          TO ERR-PENSION-ID.
           MOVE WS-IFC-RECORD-TYPE         TO ERR-RECORD-TYPE.

      **** ERROR-FOUND SWITCH IS NOT SET IF WE FOUND AN ERROR WITH CSV ****
      **** FILE, IN THAT SITUATION WE NEED TO SET CRT-ERROR-CAT        ****
      **** ERROR-FOUND SWITCH IS SET IF ERROR IS FOUND IN PRPNP PDLIB  ****
      **** WHEN WE DO NOT NEED TO SET CRT-ERROR-CAT                    ****

           IF (NO-ERROR-FOUND)
               MOVE CRT-PROGRAM-CODE       TO CRT-ERROR-CAT
           ELSE
               MOVE CRT-ERROR-NBR          TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE                TO ERR-MESSAGE.

027200     WRITE ERROR-REC                 FROM ERR-ERROR-REC.
054000
           IF  (PRM-ERROR-OPTION           = "Y")
           AND (NO-ERR-RECS-WRITTEN)
               PERFORM 330-WRITE-ALL-PIDS-TO-PNO.

054100     SET ERR-RECS-WRITTEN            TO TRUE.
054200
054300 320-END.
           EXIT.
054400
050400******************************************************************
050500 330-WRITE-ALL-PIDS-TO-PNO      SECTION.
050600******************************************************************
050700 330-START.
050800
      **** WRITE ALL RECORDS FOR PENSION ID INTO PENOUTCSV FILE  ****

           IF (WS-IFC-RECORD-TYPE          NOT = "1" AND "2" AND "3" AND
                                                 "4")
               CONTINUE
           ELSE
           IF (WS-IFC-RECORD-TYPE          = "1")
               IF (I1                      = 1)
                   PERFORM 332-WRITEPNO-FROMWS-TYPE-1
               ELSE
                   PERFORM 334-WRITEPNO-FROMPNP-TYPE-1
                   PERFORM 332-WRITEPNO-FROMWS-TYPE-1
               END-IF
           ELSE
           IF (WS-IFC-RECORD-TYPE          = "2")
               IF (I1                      = 1)
                   PERFORM 334-WRITEPNO-FROMPNP-TYPE-1
               END-IF
               PERFORM 336-WRITEPNO-FROMPNP-TYPE-2
           ELSE
           IF (WS-IFC-RECORD-TYPE          = "3")
               IF (I1                      = 1)
                   PERFORM 334-WRITEPNO-FROMPNP-TYPE-1
               END-IF
               PERFORM 336-WRITEPNO-FROMPNP-TYPE-2
               PERFORM 338-WRITEPNO-FROMPNP-TYPE-3
           ELSE
           IF (WS-IFC-RECORD-TYPE          = "4")
               IF (I1                      = 1)
                   PERFORM 334-WRITEPNO-FROMPNP-TYPE-1
               END-IF
               PERFORM 336-WRITEPNO-FROMPNP-TYPE-2
               PERFORM 338-WRITEPNO-FROMPNP-TYPE-3
               IF (I4                      = 1)
                   PERFORM 340-WRITEPNO-FROMWS-TYPE-4
               ELSE
                   PERFORM 342-WRITEPNO-FROMPNP-TYPE-4
                   PERFORM 340-WRITEPNO-FROMWS-TYPE-4.

054300 330-END.
           EXIT.
054400
050400******************************************************************
050500 332-WRITEPNO-FROMWS-TYPE-1      SECTION.
050600******************************************************************
050700 332-START.
050800
           INITIALIZE PNO-PENOUTCSV-REC.

           MOVE WS-IFC-COMPANY             TO PNO-COMPANY.
           MOVE WS-IFC-EMPLOYEE            TO PNO-EMPLOYEE.
           MOVE WS-IFC-PENSION-ID          TO PNO-PENSION-ID.
           MOVE WS-IFC-RECORD-TYPE         TO PNO-RECORD-TYPE.

           MOVE WS-IFC-EFFECT-DATE         TO PNO-EFFECT-DATE.
           MOVE WS-IFC-END-DATE            TO PNO-END-DATE.
           MOVE WS-IFC-PENS-DIST-TYPE      TO PNO-PENS-DIST-TYPE.
           MOVE WS-IFC-PENS-DIST-CODE      TO PNO-PENS-DIST-CODE.
           MOVE WS-IFC-PENS-DST-CODE2      TO PNO-PENS-DST-CODE2.
           MOVE WS-IFC-TAX-CATEGORY        TO PNO-TAX-CATEGORY.
           MOVE WS-IFC-PROCESS-LEVEL       TO PNO-PROCESS-LEVEL.
           MOVE WS-IFC-DEPARTMENT          TO PNO-DEPARTMENT.
           MOVE WS-IFC-ADDL-FED-TAX        TO PNO-ADDL-FED-TAX.
           MOVE WS-IFC-TAX-FREQ-OVERRIDE   TO PNO-TAX-FREQ-OVERRIDE.
           MOVE WS-IFC-PROCESS-GRP         TO PNO-PROCESS-GRP.
           MOVE WS-IFC-PERC-TOT-DIST       TO PNO-PERC-TOT-DIST.
           MOVE WS-IFC-PAYABLE-TO          TO PNO-PAYABLE-TO.
           MOVE WS-IFC-FBO-NAME            TO PNO-FBO-NAME.
           MOVE WS-IFC-ADDR1               TO PNO-ADDR1.
           MOVE WS-IFC-ADDR2               TO PNO-ADDR2.
           MOVE WS-IFC-ADDR3               TO PNO-ADDR3.
           MOVE WS-IFC-ADDR4               TO PNO-ADDR4.
           MOVE WS-IFC-CITY                TO PNO-CITY.
           MOVE WS-IFC-STATE               TO PNO-STATE.
           MOVE WS-IFC-POSTAL-CODE         TO PNO-POSTAL-CODE.
           MOVE WS-IFC-COUNTRY-CODE        TO PNO-COUNTRY-CODE.
           MOVE WS-IFC-ROLLOVER-ACCT       TO PNO-ROLLOVER-ACCT.
           MOVE WS-IFC-MISCELLANEOUS       TO PNO-MISCELLANEOUS.
           MOVE WS-IFC-MISCELLANEOUS2      TO PNO-MISCELLANEOUS2.
           MOVE WS-IFC-WIRE-IND            TO PNO-WIRE-IND.
           MOVE WS-IFC-CA-EFT              TO PNO-CA-EFT.
P68632     MOVE WS-IFC-ROTH-YEAR       TO PNO-ROTH-INITIAL-CONTR-YEAR.
J93849     MOVE WS-IFC-IAT-ACH             TO PNO-IAT-ACH.

           PERFORM 800-WRITECSV-PENOUTCSV.

054300 332-END.
           EXIT.
054400
050400******************************************************************
050500 334-WRITEPNO-FROMPNP-TYPE-1     SECTION.
050600******************************************************************
050700 334-START.
050800
           INITIALIZE PNO-PENOUTCSV-REC.

           MOVE WS-IFC-COMPANY             TO PNO-COMPANY.
           MOVE WS-IFC-EMPLOYEE            TO PNO-EMPLOYEE.
           MOVE WS-IFC-PENSION-ID          TO PNO-PENSION-ID.
           MOVE "1"                        TO PNO-RECORD-TYPE.

           MOVE PRPNP-EFFECT-DATE          TO PNO-EFFECT-DATE.
           MOVE PRPNP-END-DATE             TO PNO-END-DATE.
           MOVE PRPNP-PENS-DIST-TYPE       TO PNO-PENS-DIST-TYPE.
           MOVE PRPNP-PENS-DIST-CODE       TO PNO-PENS-DIST-CODE.
           MOVE PRPNP-PENS-DST-CODE2       TO PNO-PENS-DST-CODE2.
           MOVE PRPNP-TAX-CATEGORY         TO PNO-TAX-CATEGORY.
           MOVE PRPNP-PROCESS-LEVEL        TO PNO-PROCESS-LEVEL.
           MOVE PRPNP-DEPARTMENT           TO PNO-DEPARTMENT.
           MOVE PRPNP-ADDL-FED-TAX         TO PNO-ADDL-FED-TAX.
           MOVE PRPNP-TAX-FREQ-OVER        TO PNO-TAX-FREQ-OVERRIDE.
           MOVE PRPNP-PROCESS-GRP          TO PNO-PROCESS-GRP.
           MOVE PRPNP-PERC-TOT-DIST        TO PNO-PERC-TOT-DIST.
           MOVE PRPNP-PAYABLE-TO           TO PNO-PAYABLE-TO.
           MOVE PRPNP-FBO-NAME             TO PNO-FBO-NAME.
           MOVE PRPNP-ADDR1                TO PNO-ADDR1.
           MOVE PRPNP-ADDR2                TO PNO-ADDR2.
           MOVE PRPNP-ADDR3                TO PNO-ADDR3.
           MOVE PRPNP-ADDR4                TO PNO-ADDR4.
           MOVE PRPNP-CITY                 TO PNO-CITY.
           MOVE PRPNP-STATE                TO PNO-STATE.
           MOVE PRPNP-POSTAL-CODE          TO PNO-POSTAL-CODE.
           MOVE PRPNP-COUNTRY-CODE         TO PNO-COUNTRY-CODE.
           MOVE PRPNP-ROLLOVER-ACCT        TO PNO-ROLLOVER-ACCT.
           MOVE PRPNP-MISCELLANEOUS        TO PNO-MISCELLANEOUS.
           MOVE PRPNP-MISCELLANEOUS2       TO PNO-MISCELLANEOUS2.
           MOVE PRPNP-WIRE-IND             TO PNO-WIRE-IND.
           MOVE PRPNP-CA-EFT               TO PNO-CA-EFT.
GW1104     MOVE WS-IFC-BEN-START-DATE      TO PNO-BEN-START-DATE.
GW1104     MOVE WS-IFC-BEN-STOP-RSN        TO PNO-BEN-STOP-RSN.
P68632     MOVE PRPNP-ROTH-YEAR        TO PNO-ROTH-INITIAL-CONTR-YEAR.
J93849     MOVE PRPNP-PNP-IAT              TO PNO-IAT-ACH.

           PERFORM 800-WRITECSV-PENOUTCSV.

054300 334-END.
           EXIT.
054400
050400******************************************************************
050500 336-WRITEPNO-FROMPNP-TYPE-2     SECTION.
050600******************************************************************
050700 336-START.
050800
           PERFORM
               VARYING I5 FROM 1 BY 1
               UNTIL  (I5 > I2)

               INITIALIZE PNO-PENOUTCSV-REC

               MOVE WS-IFC-COMPANY         TO PNO-COMPANY
               MOVE WS-IFC-EMPLOYEE        TO PNO-EMPLOYEE
               MOVE WS-IFC-PENSION-ID      TO PNO-PENSION-ID
               MOVE "2"                    TO PNO-RECORD-TYPE

               IF  (WS-IFC-RECORD-TYPE     = "2")
               AND (I5                     = I2)
                   MOVE WS-IFC-FUNDING-SOURCE
                                           TO PNO-FUNDING-SOURCE
                   MOVE WS-IFC-FUND-AMOUNT TO PNO-FUND-AMOUNT
               ELSE
                   MOVE PRPNP-STB-FUNDING-SOURCE (I5)
                                           TO PNO-FUNDING-SOURCE
                   MOVE PRPNP-STB-FUND-AMOUNT (I5)
                                           TO PNO-FUND-AMOUNT
               END-IF

               PERFORM 800-WRITECSV-PENOUTCSV
           END-PERFORM.

054300 336-END.
           EXIT.
054400
050400******************************************************************
050500 338-WRITEPNO-FROMPNP-TYPE-3     SECTION.
050600******************************************************************
050700 338-START.
050800
           PERFORM
               VARYING I5 FROM 1 BY 1
               UNTIL  (I5 > I3)

               INITIALIZE PNO-PENOUTCSV-REC

               MOVE WS-IFC-COMPANY         TO PNO-COMPANY
               MOVE WS-IFC-EMPLOYEE        TO PNO-EMPLOYEE
               MOVE WS-IFC-PENSION-ID      TO PNO-PENSION-ID
               MOVE "3"                    TO PNO-RECORD-TYPE

               IF  (WS-IFC-RECORD-TYPE     = "3")
               AND (I5                     = I3)
                   MOVE WS-IFC-PAY-CODE    TO PNO-PAY-CODE
                   MOVE WS-IFC-AMOUNT-RATE TO PNO-AMOUNT-RATE
               ELSE
                   MOVE PRPNP-TTB-PAY-CODE (I5)
                                           TO PNO-PAY-CODE
                   MOVE PRPNP-TTB-RATE(I5) TO PNO-AMOUNT-RATE
               END-IF
               PERFORM 800-WRITECSV-PENOUTCSV
           END-PERFORM.

054300 338-END.
           EXIT.
054400
050400******************************************************************
050500 340-WRITEPNO-FROMWS-TYPE-4      SECTION.
050600******************************************************************
050700 340-START.
050800
           INITIALIZE PNO-PENOUTCSV-REC.

           MOVE WS-IFC-COMPANY             TO PNO-COMPANY.
           MOVE WS-IFC-EMPLOYEE            TO PNO-EMPLOYEE.
           MOVE WS-IFC-PENSION-ID          TO PNO-PENSION-ID.
           MOVE WS-IFC-RECORD-TYPE         TO PNO-RECORD-TYPE.

009290     MOVE WS-IFC-EBANK-ID            TO PNO-EBANK-ID.
009290     MOVE WS-IFC-BANK-NAME           TO PNO-BANK-NAME.
009290     MOVE WS-IFC-EBNK-ACCT-NBR       TO PNO-EBNK-ACCT-NBR.
009290     MOVE WS-IFC-ACCOUNT-TYPE        TO PNO-ACCOUNT-TYPE.
009290     MOVE WS-IFC-PAYMENT-DESCRIPTION TO PNO-PAYMENT-DESCRIPTION.

           PERFORM 800-WRITECSV-PENOUTCSV.

054300 340-END.
           EXIT.
054400
050400******************************************************************
050500 342-WRITEPNO-FROMPNP-TYPE-4     SECTION.
050600******************************************************************
050700 342-START.
050800
           INITIALIZE PNO-PENOUTCSV-REC.

           MOVE WS-IFC-COMPANY             TO PNO-COMPANY.
           MOVE WS-IFC-EMPLOYEE            TO PNO-EMPLOYEE.
           MOVE WS-IFC-PENSION-ID          TO PNO-PENSION-ID.
           MOVE "4"                        TO PNO-RECORD-TYPE.

009290     MOVE PRPNP-EAD-EBANK-ID         TO PNO-EBANK-ID.
009290     MOVE PRPNP-EAD-DESCRIPTION      TO PNO-BANK-NAME.
009290     MOVE PRPNP-EAD-EBNK-ACCT-NBR    TO PNO-EBNK-ACCT-NBR.
009290     MOVE PRPNP-EAD-ACCOUNT-TYPE     TO PNO-ACCOUNT-TYPE.
009290     MOVE PRPNP-EAD-CHECK-DESC       TO PNO-PAYMENT-DESCRIPTION.
GW1104     MOVE WS-IFC-BEN-START-DATE      TO PNO-BEN-START-DATE.
GW1104     MOVE WS-IFC-BEN-STOP-RSN        TO PNO-BEN-STOP-RSN.

           PERFORM 800-WRITECSV-PENOUTCSV.

054300 342-END.
           EXIT.
054400
050400******************************************************************
050500 350-WRITEPNO-FROMCSV           SECTION.
050600******************************************************************
050700 350-START.
050800
           INITIALIZE PNO-PENOUTCSV-REC.

           MOVE CSV-COMPANY                TO PNO-COMPANY.
           MOVE CSV-EMPLOYEE               TO PNO-EMPLOYEE.
           MOVE CSV-PENSION-ID             TO PNO-PENSION-ID.
           MOVE CSV-RECORD-TYPE            TO PNO-RECORD-TYPE.

           IF (CSV-RECORD-TYPE             = "1")
           OR (CSV-RECORD-TYPE             NOT = "1" AND "2" AND "3" AND
                                                 "4")
               MOVE CSV-EFFECT-DATE        TO PNO-EFFECT-DATE
               MOVE CSV-END-DATE           TO PNO-END-DATE
               MOVE CSV-PENS-DIST-TYPE     TO PNO-PENS-DIST-TYPE
               MOVE CSV-PENS-DIST-CODE     TO PNO-PENS-DIST-CODE
               MOVE CSV-PENS-DST-CODE2     TO PNO-PENS-DST-CODE2
               MOVE CSV-TAX-CATEGORY       TO PNO-TAX-CATEGORY
               MOVE CSV-PROCESS-LEVEL      TO PNO-PROCESS-LEVEL
               MOVE CSV-DEPARTMENT         TO PNO-DEPARTMENT
               MOVE CSV-ADDL-FED-TAX       TO PNO-ADDL-FED-TAX
               MOVE CSV-TAX-FREQ-OVERRIDE  TO PNO-TAX-FREQ-OVERRIDE
               MOVE CSV-PROCESS-GRP        TO PNO-PROCESS-GRP
               MOVE CSV-PERC-TOT-DIST      TO PNO-PERC-TOT-DIST
               MOVE CSV-PAYABLE-TO         TO PNO-PAYABLE-TO
               MOVE CSV-FBO-NAME           TO PNO-FBO-NAME
               MOVE CSV-ADDR1              TO PNO-ADDR1
               MOVE CSV-ADDR2              TO PNO-ADDR2
               MOVE CSV-ADDR3              TO PNO-ADDR3
               MOVE CSV-ADDR4              TO PNO-ADDR4
               MOVE CSV-CITY               TO PNO-CITY
               MOVE CSV-STATE              TO PNO-STATE
               MOVE CSV-POSTAL-CODE        TO PNO-POSTAL-CODE
               MOVE CSV-COUNTRY-CODE       TO PNO-COUNTRY-CODE
               MOVE CSV-ROLLOVER-ACCT      TO PNO-ROLLOVER-ACCT
               MOVE CSV-MISCELLANEOUS      TO PNO-MISCELLANEOUS
               MOVE CSV-MISCELLANEOUS2     TO PNO-MISCELLANEOUS2
               MOVE CSV-WIRE-IND           TO PNO-WIRE-IND
               MOVE CSV-CA-EFT             TO PNO-CA-EFT
P68632         MOVE CSV-ROTH-INITIAL-CONTR-YEAR
P68632                                 TO PNO-ROTH-INITIAL-CONTR-YEAR.

           IF (CSV-RECORD-TYPE             = "2")
           OR (CSV-RECORD-TYPE             NOT = "1" AND "2" AND "3" AND
                                                 "4")
               MOVE CSV-FUNDING-SOURCE     TO PNO-FUNDING-SOURCE
               MOVE CSV-FUND-AMOUNT        TO PNO-FUND-AMOUNT.

           IF (CSV-RECORD-TYPE             = "3")
           OR (CSV-RECORD-TYPE             NOT = "1" AND "2" AND "3" AND
                                                 "4")
               MOVE CSV-PAY-CODE           TO PNO-PAY-CODE
               MOVE CSV-AMOUNT-RATE        TO PNO-AMOUNT-RATE.

           IF (CSV-RECORD-TYPE             = "3")
           OR (CSV-RECORD-TYPE             NOT = "1" AND "2" AND "3" AND
                                                 "4")
               MOVE CSV-EBANK-ID           TO PNO-EBANK-ID
               MOVE CSV-BANK-NAME          TO PNO-BANK-NAME
               MOVE CSV-EBNK-ACCT-NBR      TO PNO-EBNK-ACCT-NBR
               MOVE CSV-ACCOUNT-TYPE       TO PNO-ACCOUNT-TYPE
               MOVE CSV-PAYMENT-DESCRIPTION
                                           TO PNO-PAYMENT-DESCRIPTION.

J93849     MOVE CSV-IAT-ACH                TO PNO-IAT-ACH.

           PERFORM 800-WRITECSV-PENOUTCSV.

054300 350-END.
           EXIT.
054400
085600******************************************************************
085700 400-UPDATE-CKPOINT              SECTION.
085800******************************************************************
085900 400-START.
086000
086100     PERFORM 840-MODIFY-CKPSET1.

086200     MOVE WS-COMPANY                 TO WS-RS-COMPANY.
086500     MOVE WS-EMPLOYEE                TO WS-RS-EMPLOYEE.
086500     MOVE WS-NBR-RECS                TO WS-RS-NBR-RECS.
086500     MOVE WS-TOT-ERRORS              TO WS-RS-TOT-ERRORS.
087000
087100     MOVE WS-RESTART-INFO            TO CKP-RESTART-INFO.
087200     PERFORM 820-STORE-CKPOINT.
087500
029100     PERFORM 925-AUDIT-END.
029200     PERFORM 910-AUDIT-BEGIN.
029300
029400     PERFORM 900-SAVE-PRINT-FILES.
029500
087600 400-END.
           EXIT.

067800*****************************************************************
067900 500-PRINT-ERRORS                SECTION.
068000*****************************************************************
068100 500-START.
068200
003390     INITIALIZE RPT-PAGE-COUNT (PR529-R2).

069100     MOVE ERROR-HEADING              TO RPT-GROUP-REQUEST.
069200     PERFORM 700-PRINT-RPT-GRP.
069300
068300     MOVE WS-FALSE                   TO WS-END-OF-FILE-SW.

068500     READ ERROR-FILE                 INTO ERR-ERROR-REC
068600         AT END
068700             MOVE WS-TRUE            TO WS-END-OF-FILE-SW.
068800
069400     IF (END-OF-FILE)
      ******** No data to print in Pension Payroll Conversion Error Report
069500         MOVE 054                    TO CRT-MSG-NBR 
069700         PERFORM 780-PRINT-MSG
               GO TO 500-END.

069900     PERFORM
070000         UNTIL (END-OF-FILE)

072400         MOVE ERR-EMPLOYEE           TO ERL-EMPLOYEE
072400         MOVE ERR-PENSION-ID         TO ERL-PENSION-ID
072500         MOVE ERR-MESSAGE            TO ERL-MESSAGE

072600         MOVE ERROR-LINE             TO RPT-GROUP-REQUEST
072700         PERFORM 700-PRINT-RPT-GRP
072800
072900         READ ERROR-FILE             INTO ERR-ERROR-REC
073000             AT END
073100                 MOVE WS-TRUE        TO WS-END-OF-FILE-SW
               END-READ
           END-PERFORM.

071600 500-END.
071700     EXIT.
GW1104*****************************************************************
GW1104 600-PROCESS-TAG                 SECTION.
GW1104*****************************************************************
GW1104
GW1104** GW 11/2004 MOD TO SET UP  TAG ALONG *************************
GW1104
GW1104 600-START.
GW1104
GW1104     MOVE PNP-COMPANY            TO DB-COMPANY.
GW1104     MOVE PNP-EMPLOYEE           TO DB-EMPLOYEE.
GW1104     MOVE PNP-EFFECT-DATE        TO DB-EFFECT-DATE.
GW1104     MOVE PNP-PENS-SEQ-NBR       TO DB-PENS-SEQ-NBR.
GW1104     PERFORM 840-FIND-PNPSET3.
GW1104     IF (PRPENPAY-FOUND)
GW1104        MOVE PNP-COMPANY            TO DB-COMPANY
GW1104        MOVE PNP-EMPLOYEE           TO DB-EMPLOYEE
GW1104        MOVE PNP-EFFECT-DATE        TO DB-EFFECT-DATE
GW1104        MOVE PNP-PENS-SEQ-NBR       TO DB-PENS-SEQ-NBR
GW1104        PERFORM 840-MODIFY-WPNSET1
GW1104        IF (WBPPRPENPY-FOUND)
GW1104            MOVE WS-IFC-BEN-START-DATE   TO WPN-BEN-START-DATE
GW1104            MOVE WS-IFC-BEN-STOP-RSN     TO WPN-BEN-STOP-RSN
GW1104            PERFORM 820-STORE-WBPPRPENPY
GW1104            ADD 1                        TO WS-TAG-CHG-CTR
GW1104            DISPLAY "TAG FILE CHANGED " WPN-EMPLOYEE
GW1104        ELSE
GW1104            PERFORM 800-CREATE-WBPPRPENPY
GW1104            MOVE PNP-COMPANY            TO WPN-COMPANY
GW1104            MOVE PNP-EMPLOYEE           TO WPN-EMPLOYEE
GW1104            MOVE PNP-EFFECT-DATE        TO WPN-EFFECT-DATE
GW1104            MOVE PNP-PENS-SEQ-NBR       TO WPN-PENS-SEQ-NBR
GW1104
GW1104            MOVE WS-IFC-BEN-START-DATE     TO WPN-BEN-START-DATE
GW1104            MOVE WS-IFC-BEN-STOP-RSN       TO WPN-BEN-STOP-RSN
GW1104
GW1104            PERFORM 820-STORE-WBPPRPENPY
GW1104            ADD 1                       TO WS-TAG-ADD-CTR
GW1104     ELSE
GW1104        ADD 1                        TO WS-TAG-SKIP-CTR
GW1104        DISPLAY "PNP REC SKIPPED " WS-IFC-EMPLOYEE.
GW1104
GW1104 600-END.
GW1104     EXIT.
